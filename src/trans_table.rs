use std::{
    cmp::{max, min},
    collections::HashMap,
    hash::Hash, str::FromStr,
};

use rand::seq::SliceRandom;
use regex::Regex;

use crate::{CardMap, CardSet, Deal, PlayOfCards, PlayState, Player};

#[derive(Clone, Copy, PartialEq, Eq)]
pub struct AB {
    /// guaranteed to make <= `a`
    pub a: u8,
    /// impossible to make > `b`
    pub b: u8,
}
impl AB {
    pub(crate) fn intersect(self, other: AB) -> AB {
        debug_assert!(self.a <= self.b);
        debug_assert!(other.a <= other.b);
        let result = AB {
            a: max(self.a, other.a),
            b: min(self.b, other.b),
        };
        debug_assert!(
            result.a <= result.b,
            "{self:?} intersect {other:?} = {result:?}"
        );
        result
    }

    fn intersects(self, other: AB) -> bool {
        max(self.a, other.a) <= min(self.b, other.b)
    }
    
    #[must_use]
    pub fn contains(&self, x: u8) -> bool {
        x >= self.a && x <= self.b
    }
}

impl std::fmt::Debug for AB {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "({}, {})", self.a, self.b)
    }
}

pub trait TransTable {
    fn get<PoC: PlayOfCards>(&self, state: &PlayState<PoC>) -> Option<(CardSet, AB)>;
    fn update<PoC: PlayOfCards>(&mut self, state: &PlayState<PoC>, rel_mask: CardSet, ab: AB);
}

pub struct Empty {}
impl TransTable for Empty {
    fn get<PoC: PlayOfCards>(&self, _state: &PlayState<PoC>) -> Option<(CardSet, AB)> {
        None
    }

    fn update<PoC: PlayOfCards>(&mut self, _state: &PlayState<PoC>, _rel_mask: CardSet, _ab: AB) {}
}

#[derive(Eq, Hash, PartialEq, PartialOrd, Ord, Clone)]
struct Distr {
    distr: u64,
}
impl Distr {
    fn get(&self, p: Player, suit: usize) -> u8 {
        let distr = (self.distr >> (suit * 16)) & 0xffff;
        let p = p.index();
        let distr = (distr >> ((3 - p) * 4)) & 0xf;
        debug_assert!(distr <= 13);
        distr as u8
    }
}

impl From<&Deal> for Distr {
    fn from(deal: &Deal) -> Self {
        let c = (deal & CardSet::C).count();
        let d = (deal & CardSet::D).count();
        let h = (deal & CardSet::H).count();
        let s = (deal & CardSet::S).count();

        let c = u64::from(c[3]) | u64::from(c[2]) << 4 | u64::from(c[1]) << 8 | u64::from(c[0]) << 12;
        let d = u64::from(d[3]) | u64::from(d[2]) << 4 | u64::from(d[1]) << 8 | u64::from(d[0]) << 12;
        let h = u64::from(h[3]) | u64::from(h[2]) << 4 | u64::from(h[1]) << 8 | u64::from(h[0]) << 12;
        let s = u64::from(s[3]) | u64::from(s[2]) << 4 | u64::from(s[1]) << 8 | u64::from(s[0]) << 12;

        let distr = (c) | (d) << 16 | (h) << 32 | (s) << 48;    
        Distr { distr }
    }
}

impl FromStr for Distr {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let re = Regex::new(r"(.*)\.(.*)\.(.*)\.(.*)").unwrap();

        let players:Vec<_> = s.split(' ').collect();
        if players.len() != 4 {
            return Err(format!("bad input: {s}"));
        }

        let mut c = 0u64;
        let mut d = 0u64;
        let mut h = 0u64;
        let mut s = 0u64;

        for (i, _) in Player::ALL.iter().enumerate() {
            let Some(captures) = re.captures(players[i]) else {
                return Err(format!("bad input: {}", players[i]));
            };
            let (_, [s1, h1, d1, c1]) = captures.extract();
            c |= (c1.len() as u64) << ((3 - i)*4);
            d |= (d1.len() as u64) << ((3 - i)*4);
            h |= (h1.len() as u64) << ((3 - i)*4);
            s |= (s1.len() as u64) << ((3 - i)*4);
        }

        let distr = (c) | (d) << 16 | (h) << 32 | (s) << 48;    
        Ok(Distr { distr })
    }
}

impl std::fmt::Debug for Distr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:016x}", self.distr)
    }
}

#[derive(Eq, Hash, PartialEq, PartialOrd, Ord, Clone)]
struct Key {
    distr: Distr,
    next: Player,
}

impl<PoC: PlayOfCards> From<&PlayState<PoC>> for Key {
    fn from(state: &PlayState<PoC>) -> Self {
        let distr = Distr::from(&state.deal);
        let next = state.next;
        Key { distr, next }
    }
}

impl std::fmt::Debug for Key {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}{:?}", self.next, self.distr)
    }
}

#[derive(Debug, Clone)]
struct Entry {
    mask: Deal,
    ab: AB,
}

#[derive(Debug, Clone)]
pub struct Pattern {
    key: Key,
    entry: Entry,
}
impl Pattern {
    #[must_use]
    pub fn gen_random_deal(&self) -> Deal {
        let mut rng = rand::thread_rng();
        let mut deal = self.entry.mask.clone();
        let fixed = deal.present();
        let rest = CardSet::ALL52.without(fixed);

        for (i, suit) in CardSet::SUITS.iter().enumerate() {
            let cards = rest & *suit;
            let mut cards: Vec<_> = cards.iter().map(|item| item.card()).collect();
            cards.shuffle(&mut rng);
            for p in &Player::ALL {
                let count = self.key.distr.get(*p, i);
                let fixed = (deal.get(*p) & *suit).size();
                debug_assert!(count >= fixed);
                let count = count - fixed;
                for _ in 0..count {
                    let card = cards.pop().unwrap();
                    deal.add(*p, card);
                }
            }
        }

        let mut card_map = CardMap::default();
        deal.promote_all(&mut card_map)
    }
    
    #[must_use]
    pub fn next(&self) -> Player {
        self.key.next
    }
    
    #[must_use]
    pub fn ab(&self) -> AB {
        self.entry.ab
    }
}

impl FromStr for Pattern {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let re = Regex::new(r"(.) \{ (.*) \} *=? *\((\d+), (\d+)\)").unwrap();
        let Some(captures) = re.captures(s) else {
            return Err(format!("bad input: {s}"));
        };
        let (_, [next, deal, a, b]) = captures.extract();
        let next = Player::from_str(next)?;
        let a = u8::from_str(a).map_err(|e| format!("parse error: {e:?}"))?;
        let b = u8::from_str(b).map_err(|e| format!("parse error: {e:?}"))?;
        let mask = deal.replace('x', "");
        let mask = Deal::try_from_pbn(&mask)?;
        let distr = Distr::from_str(deal)?;
        Ok(Pattern { key: Key { distr, next }, entry: Entry{ mask, ab: AB { a, b }} })
    }
}

pub struct UnsyncTable {
    cache: HashMap<Key, Vec<Entry>>,
}

impl UnsyncTable {
    pub(crate) fn new(size: usize) -> Self {
        Self {
            cache: HashMap::with_capacity(size),
        }
    }
}

impl TransTable for UnsyncTable {
    fn get<PoC: PlayOfCards>(&self, state: &PlayState<PoC>) -> Option<(CardSet, AB)> {
        let entries = self.cache.get(&Key::from(state))?;
        let deal = &state.deal;
        let mut result: Option<(CardSet, AB)> = None;
        for entry in entries {
            if (&entry.mask & deal) == entry.mask {
                if let Some(prev_result) = result {
                    debug_assert!(
                        entry.ab.intersects(prev_result.1),
                        "inconsistent entries:\n    {:?} {} = {:?}\n  vs\n    {:?} {} = {:?}\n  deal = {deal:?}", 
                        state.next,
                        deal.format_bpn_masked(entry.mask.present()),
                        entry.ab, 
                        state.next,
                        deal.format_bpn_masked(prev_result.0),
                        prev_result.1,
                    );
                    result = Some((
                        entry.mask.present() | prev_result.0,
                        entry.ab.intersect(prev_result.1),
                    ));
                    // if entry.ab.a > prev_result.1.a {
                    //     result = Some((entry.deal_mask.present(), entry.ab))
                    // }
                } else {
                    result = Some((entry.mask.present(), entry.ab));
                }
            }
        }
        result
    }

    fn update<PoC: PlayOfCards>(&mut self, state: &PlayState<PoC>, rel_mask: CardSet, ab: AB) {
        debug_assert!(ab.b <= 13);
        let key = Key::from(state);

        let deal = &state.deal;
        let deal = deal & rel_mask;
        debug_assert_eq!(deal.present(), rel_mask);
        let new_entry = Entry {
            mask: deal,
            ab,
        };
        let Some(entries) = self.cache.get_mut(&key) else {
            let entries = vec![new_entry];
            self.cache.insert(key, entries);
            return;
        };
        for entry in entries.iter_mut() {
            if entry.mask == new_entry.mask {
                let ab = entry.ab.intersect(new_entry.ab);
                debug_assert_ne!(ab, entry.ab, "suboptimal table insert");
                entry.ab = ab;
            }
        }
        entries.push(new_entry);
    }
}
