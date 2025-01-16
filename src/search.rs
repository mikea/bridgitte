use std::{cmp::max, path::PathBuf};

use clap::ValueEnum;

use crate::{
    observers::{Dot, Empty, Logger, Observer, UnsyncStats},
    trans_table::{TransTable, UnsyncTable, AB},
    Card, CardMap, CardSet, Deal, Pair, PlayOfCards, PlayState, PlayedCardsNT, PlayedCardsT,
    Player, PlayerMap, SetItem, Suit, Trick,
};

pub trait Search {
    fn run_unbounded(&mut self) -> u8;
    fn optimal_line(&mut self, target: u8) -> Vec<(Player, [Card; 4])>;
}

trait ABLattice {
    fn default_possible() -> bool;
    fn search_cutoff(target: u8, max: u8) -> AB;
    fn pick(r1: AB, r2: AB) -> AB;
}

struct Max {}
impl ABLattice for Max {
    fn default_possible() -> bool {
        false
    }

    fn search_cutoff(target: u8, max: u8) -> AB {
        AB { a: target, b: max }
    }

    fn pick(r1: AB, r2: AB) -> AB {
        if r1.a > r2.a || (r1.a == r2.a && r1.b < r2.b) {
            r1
        } else {
            r2
        }
    }
}

struct Min {}
impl ABLattice for Min {
    fn default_possible() -> bool {
        true
    }

    fn search_cutoff(target: u8, _max: u8) -> AB {
        debug_assert!(target > 0);
        // we can deny target tricks, so at most target -1 tricks are possible
        AB {
            a: 0,
            b: target - 1,
        }
    }

    fn pick(r1: AB, r2: AB) -> AB {
        if r1.a < r2.a || (r1.a == r2.a && r1.b < r2.b) {
            r1
        } else {
            r2
        }
    }
}

#[derive(Debug, Clone)]
pub struct InitialPosition {
    pub deal: Deal,
    pub trumps: Option<Suit>,
    pub declarer: Player,
    pub next: Option<Player>,
}

impl InitialPosition {
    fn search_impl(
        self: &InitialPosition,
        table: impl TransTable + 'static,
        observer: impl Observer + 'static,
    ) -> Box<dyn Search> {
        if let Some(_trumps) = self.trumps {
            Box::new(SearchImpl::<PlayedCardsT, _, _>::new(self, table, observer))
        } else {
            Box::new(SearchImpl::<PlayedCardsNT, _, _>::new(
                self, table, observer,
            ))
        }
    }

    #[must_use]
    pub fn search(self: &InitialPosition, config: &Config) -> Box<dyn Search> {
        let table = UnsyncTable::new(1_000_000);
        let Some(tracing) = &config.tracing else {
            return self.search_impl(table, Empty::default());
        };

        match tracing.typ {
            TracingType::Stats => self.search_impl(table, UnsyncStats::default()),
            TracingType::Log => self.search_impl(table, Logger::default()),
            TracingType::Dot => self.search_impl(table, Dot::new(&tracing.output)),
        }
    }
}

/// zero-window search with transposition table
struct SearchImpl<PoC, TT, O>
where
    PoC: PlayOfCards,
    TT: TransTable,
    O: Observer,
{
    init: InitialPosition,
    state: PlayState<PoC>,
    table: TT,
    observer: O,
    cards: CardMap,
    players: PlayerMap,
}

#[derive(Debug)]
pub(crate) struct SearchResult {
    pub(crate) possible: bool,
    pub(crate) rel_mask: CardSet,
    pub(crate) ab: AB,
}

impl<PoC: PlayOfCards, TT: TransTable, O: Observer> SearchImpl<PoC, TT, O> {
    fn new(init: &InitialPosition, table: TT, observer: O) -> Self {
        let mut cards: CardMap = CardMap::default();
        let mut players = PlayerMap::default();

        let mut deal = init
            .deal
            .rotate_to_west(init.declarer.next(), Some(&mut players));
        let next = init
            .next
            .map_or(Player::W, |p| players.map_from_absolute(p));

        // trumps are always spades
        if let Some(trumps) = init.trumps {
            deal = deal.swap_suits(Suit::S, trumps, &mut cards);
        }

        let deal = deal.promote_all(&mut cards);

        Self {
            init: init.clone(),
            state: PlayState::<PoC>::new(&deal, next),
            table,
            observer,
            cards,
            players,
        }
    }

    /// returns true if _max_ pair can take `target` tricks.
    fn visit(&mut self, target: u8) -> SearchResult {
        debug_assert!(target > 0);
        if self.state.deal.is_empty() {
            return SearchResult {
                possible: false,
                rel_mask: CardSet::default(),
                ab: AB { a: 0, b: 0 },
            };
        }

        self.observer.node_enter(target, &self.state);
        let result = match Pair::from(self.state.next) {
            Pair::NS => self.visit_impl::<Max>(target),
            Pair::WE => self.visit_impl::<Min>(target),
        };
        self.observer.node_exit(target, &self.state, &result);
        debug_assert_eq!(
            result.possible,
            target <= result.ab.a,
            "inconsistent result: {target} vs {result:?}"
        );
        result
    }

    fn visit_impl<L: ABLattice>(&mut self, target: u8) -> SearchResult {
        let max_tricks = self.state.deal.max_tricks();

        // start of the round
        if self.state.play.is_empty() {
            if let Some((rel_mask, ab)) = self.table.get(&self.state) {
                self.observer.tt_hit(&self.state, rel_mask, ab);
                if target <= ab.a {
                    self.observer.tt_lower_cutoff();
                    return SearchResult {
                        possible: true,
                        rel_mask,
                        ab,
                    };
                }
                if target > ab.b {
                    self.observer.tt_upper_cutoff();
                    return SearchResult {
                        possible: false,
                        rel_mask,
                        ab,
                    };
                }
            }

            // declarare can't take more tricks than available
            if target > max_tricks {
                self.observer.max_tricks_cutoff();
                return SearchResult {
                    possible: false,
                    rel_mask: CardSet::default(),
                    ab: AB {
                        a: 0,
                        b: max_tricks,
                    },
                };
            }

            // check quick tricks cut-off
            let (quick_tricks, rel_mask) = self.state.quick_tricks();
            match Pair::from(self.state.next) {
                Pair::NS => {
                    if target <= quick_tricks {
                        // max side can quickly take `target` tricks
                        self.observer.quick_tricks_cutoff();
                        return SearchResult {
                            possible: true,
                            rel_mask,
                            ab: AB {
                                a: quick_tricks,
                                b: max_tricks,
                            },
                        };
                    }
                }
                Pair::WE => {
                    if target > (max_tricks - quick_tricks) {
                        self.observer.quick_tricks_cutoff();
                        return SearchResult {
                            possible: false,
                            rel_mask,
                            ab: AB {
                                a: 0,
                                b: max_tricks - quick_tricks,
                            },
                        };
                    }
                }
            }
        }

        debug_assert!(target > 0);
        let plays = self.state.plays();
        debug_assert!(!plays.is_empty());

        let mut possible = L::default_possible();
        let mut r = None;

        for c in seach_plays_iter(plays) {
            let result = self.visit_child(&c, target);
            if result.possible != possible {
                // counter-example
                self.observer.search_cutoff();
                possible = !possible;
                r = Some((L::search_cutoff(target, max_tricks), result.rel_mask));
                break;
            }
            r = match r {
                None => Some((result.ab, result.rel_mask)),
                Some(r1) => {
                    let p = L::pick(r1.0, result.ab);
                    Some((p, r1.1 | result.rel_mask))
                }
            };
        }

        let (ab, rel_mask) = r.unwrap();

        if self.state.play.is_empty() {
            self.observer.update_table(&self.state, rel_mask, ab);
            self.table.update(&self.state, rel_mask, ab);
        }
        SearchResult {
            possible,
            rel_mask,
            ab,
        }
    }

    fn visit_child(&mut self, c: &SetItem, target: u8) -> SearchResult {
        self.observer.child_enter(c.card());
        let (trick, undo) = self.state.apply_mut(c);
        let target_inc = trick.as_ref().map(Trick::target_inc).unwrap_or_default();
        let target = target - target_inc;
        let mut result = if target > 0 {
            self.visit(target)
        } else {
            SearchResult {
                possible: true,
                rel_mask: CardSet::default(),
                ab: AB {
                    a: 0,
                    b: self.state.deal.max_tricks(),
                },
            }
        };
        self.state.undo(undo);
        if let Some(trick) = trick {
            result.rel_mask = PoC::rel_mask(&trick, result.rel_mask);
            result.ab.a += target_inc;
            result.ab.b += target_inc;
        }
        self.observer.child_exit(c.card());
        result
    }

    fn optimal_line_impl(&mut self, mut target: u8) -> Vec<(Player, [Card; 4])> {
        let initial_deal = self.state.deal.clone();

        let mut undos = vec![];
        let mut cards = vec![];

        'next_play: while !self.state.deal.is_empty() {
            let plays = self.state.plays();
            debug_assert!(!plays.is_empty());

            for c in seach_plays_iter(plays) {
                let child = self.visit_child(&c, target);
                let pick = match Pair::from(self.state.next) {
                    Pair::WE => {
                        // min side shouldn't be able to prevent optimal play
                        assert!(child.possible);
                        // pick the play that prevents too many tricks
                        !self.visit_child(&c, target + 1).possible
                    }
                    Pair::NS => child.possible,
                };

                if pick {
                    cards.push((
                        self.players.map_to_absolute(self.state.next),
                        self.cards.to_absolute(c.card()),
                    ));
                    let (trick, undo) = self.state.apply_mut(&c);
                    if let Some(trick) = trick {
                        target -= trick.target_inc();
                        self.cards.promote(trick.cards);
                    }
                    undos.push(undo);
                    continue 'next_play;
                }
            }

            panic!("should never happen, is target optimal? {:?}", self.state);
        }

        let plays: Vec<_> = cards
            .chunks_exact(4)
            .map(|chunk| {
                let array: [(Player, Card); 4] = chunk.try_into().expect("Chunk size should be 4");
                array
            })
            .collect();

        undos.reverse();
        for undo in undos {
            self.state.undo(undo);
        }
        assert_eq!(initial_deal, self.state.deal);

        plays
            .iter()
            .map(|chunk| {
                let player = chunk[0].0;
                let mut cards: [Option<Card>; 4] = [None; 4];

                for pc in chunk {
                    cards[pc.0.index()] = Some(pc.1);
                }

                (
                    player,
                    [
                        cards[0].expect("bad play"),
                        cards[1].expect("bad play"),
                        cards[2].expect("bad play"),
                        cards[3].expect("bad play"),
                    ],
                )
            })
            .collect()
    }
}

impl<PoC: PlayOfCards, TT: TransTable, ST: Observer> Search for SearchImpl<PoC, TT, ST> {
    fn run_unbounded(&mut self) -> u8 {
        assert_eq!(0, self.state.deal.size() % 4);

        self.observer.search_started();

        let mut upper = self.state.deal.max_tricks();
        let mut lower = 0u8;
        let mut guess = upper;

        assert!(guess >= lower && guess <= upper);

        while lower < upper {
            let target = max(guess, lower + 1);
            self.observer.guess_iter(target);
            let result = self.visit(target);
            self.observer.guess_iter_done(target);
            if result.possible {
                // we were able to achieve `target` tricks, this is our new lower bound then
                lower = target;
                guess = lower;
            } else {
                // we were not able to achieve `target` tricks, the best we can hope for is -1
                upper = target - 1;
                guess = upper;
            }
        }
        self.observer.search_finished(&self.init);

        guess
    }

    fn optimal_line(&mut self, target: u8) -> Vec<(Player, [Card; 4])> {
        self.optimal_line_impl(target)
    }
}

#[derive(Default)]
pub struct Config {
    pub tracing: Option<TracingConfig>,
}

#[derive(Debug)]
pub struct TracingConfig {
    pub typ: TracingType,
    pub output: PathBuf,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, ValueEnum)]
pub enum TracingType {
    Stats,
    Log,
    Dot,
}

#[must_use]
pub(crate) fn search(init: &InitialPosition, config: &Config) -> u8 {
    init.search(config).run_unbounded()
}

struct PlayIterator<I: Iterator<Item = SetItem>> {
    iter: I,
    next: Option<SetItem>,
}

impl<I: Iterator<Item = SetItem>> PlayIterator<I> {
    fn new(mut iter: I) -> Self {
        let next = iter.next();
        Self { iter, next }
    }
}

impl<I: Iterator<Item = SetItem>> Iterator for PlayIterator<I> {
    type Item = SetItem;

    fn next(&mut self) -> Option<Self::Item> {
        // play the lowest card of the sequence.
        // it is important to play low to avoid rel_mask adjusting later for equal ranks.
        loop {
            self.next.as_ref()?;
            let mut next = self.iter.next();
            std::mem::swap(&mut self.next, &mut next);
            let next = next.unwrap();
            match &self.next {
                Some(follow) => {
                    let in_seq = next.suit == follow.suit && next.card_set == follow.card_set << 1;
                    if !in_seq {
                        return Some(next);
                    }
                }
                None => return Some(next),
            }
        }

        // loop {
        //     let item = self.iter.next()?;
        //     if let Some(prev) = &self.prev {
        //         let in_sequence = prev.suit == item.suit && prev.card_set == item.card_set << 1;

        //         if in_sequence {
        //             self.prev = Some(item);
        //             continue;
        //         }
        //     }

        //     self.prev = Some(item);
        //     return self.prev.clone();
        // }
    }
}

fn seach_plays_iter(set: CardSet) -> impl Iterator<Item = SetItem> {
    PlayIterator::new(set.rev_iter())
}

#[cfg(test)]
mod tests {
    use std::collections::HashSet;

    use crate::{
        observers::{Empty, Logger},
        search::{seach_plays_iter, search, Config, InitialPosition},
        trans_table::{TransTable, UnsyncTable},
        ByPlayer, CardSet, Deal, PlayOfCards, PlayState, Player, Suit,
    };

    use super::AB;

    #[test]
    fn play_iterator() {
        let hand = CardSet::try_from_bpn(".63.AKQ987.A9732").unwrap();
        let rev: Vec<_> = seach_plays_iter(hand)
            .map(|c| c.card().symbol().to_string())
            .collect();
        assert_eq!("🂶🂳🃍🃇🃑🃙🃗🃒", rev.join(""));
    }

    struct TestTable {
        verified_entries: HashSet<String>,
        table: UnsyncTable,
        seen: HashSet<String>,
    }

    impl TestTable {
        fn new(verified_entries: &[&str]) -> Self {
            Self {
                verified_entries: HashSet::from_iter(
                    verified_entries.iter().map(|s| s.to_string()),
                ),
                table: UnsyncTable::new(100),
                seen: HashSet::with_capacity(verified_entries.len()),
            }
        }
    }

    impl TransTable for TestTable {
        fn get<PoC: PlayOfCards>(&self, state: &PlayState<PoC>) -> Option<(CardSet, AB)> {
            self.table.get(state)
        }

        fn update<PoC: PlayOfCards>(&mut self, state: &PlayState<PoC>, rel_mask: CardSet, ab: AB) {
            let str = format!(
                "{:?} {} = ({},{})",
                state.next,
                state.deal.format_bpn_masked(rel_mask),
                ab.a,
                ab.b
            );
            assert!(
                self.verified_entries.contains(&str),
                "unverified entry: {str}"
            );
            self.seen.insert(str);
            self.table.update(state, rel_mask, ab);
        }
    }

    impl Drop for TestTable {
        fn drop(&mut self) {
            for e in &self.verified_entries {
                assert!(self.seen.contains(e), "extra entry: {e}");
            }
        }
    }

    #[test]
    fn small_deal_1() {
        let deal = Deal::try_from(ByPlayer::<&str> {
            w: "K...",
            n: "A...",
            e: "T...",
            s: "7...",
        })
        .unwrap();
        let table = TestTable::new(&["W { x... A... x... x... } = (1,1)"]);
        let mut search = InitialPosition {
            deal,
            trumps: None,
            declarer: Player::S,
            next: None,
        }
        .search_impl(table, Logger::default());
        let tricks = search.run_unbounded();
        assert_eq!(1, tricks);
    }

    #[test]
    fn small_deal_2() {
        let deal = Deal::try_from(ByPlayer::<&str> {
            w: "K6...",
            n: "AQ...",
            e: "T9...",
            s: "72...",
        })
        .unwrap();
        let table = TestTable::new(&[
            "N { A... x... x... x... } = (0,0)",
            "W { Kx... AQ... xx... xx... } = (2,2)",
        ]);
        let mut search = InitialPosition {
            deal,
            trumps: None,
            declarer: Player::S,
            next: None,
        }
        .search_impl(table, Logger::default());
        let tricks = search.run_unbounded();
        assert_eq!(2, tricks);
    }

    #[test]
    fn small_deal_3() {
        let deal = Deal::try_from(ByPlayer::<&str> {
            w: "K64...",
            n: "AQJ...",
            e: "T98...",
            s: "752...",
        })
        .unwrap();

        let table = TestTable::new(&[
            "N { Ax... xx... xx... xx... } = (0,1)",
            "N { A... x... x... x... } = (0,0)",
            "N { Kx... Ax... xx... xx... } = (0,1)",
            "W { Kxx... AQJ... xxx... xxx... } = (0,2)",
            "W { x... A... x... x... } = (1,1)",
            "N { Ax... KQ... xx... xx... } = (1,2)",
            "W { Kxx... AQJ... xxx... xxx... } = (2,3)",
        ]);
        let mut search = InitialPosition {
            deal,
            trumps: None,
            declarer: Player::S,
            next: None,
        }
        .search_impl(table, Logger::default());

        assert_eq!(2, search.run_unbounded());
    }

    #[test]
    fn small_deal_4() {
        let deal = Deal::try_from_pbn(".Q9.K. .J.J.K .T.AQ. .AK..A").unwrap();

        let table = TestTable::new(&[
            "W { .x.AQ. .xx..x .xx.K. .x.x.x } = (0,1)",
            "W { .x.. ...x .x.. .A.. } = (1,1)",
            "W { .x.. ...x .A.. ...x } = (0,0)",
            "W { .x.. ...x .A.. .x.. } = (0,0)",
            "W { .x.. .A.. .x.. ...x } = (1,1)",
            "W { .x.x. .A..x .xx.. .x..x } = (1,2)",
            "E { .x.x. .A..x .xx.. .x..x } = (1,2)",
            "W { .x.Ax. .AK..x .xx.K. .x.x.x } = (1,3)",
        ]);
        let mut search = InitialPosition {
            deal: deal.clone(),
            trumps: Some(crate::Suit::S),
            declarer: Player::N,
            next: None,
        }
        .search_impl(table, Logger::default());
        assert_eq!(1, search.run_unbounded());
    }

    #[test]
    fn small_deal_5() {
        let deal = Deal::try_from_pbn(".Q.A. ..Q.A .A.K. .K..K").unwrap();

        let table = TestTable::new(&[
            "N { ..A. ..x. ..x. .x.. } = (0,0)",
            "S { .x.A. ..x.A .A.x. .x..x } = (0,1)",
            "E { ..A. ...x ..x. ...x } = (0,0)",
            "E { ..A. ..x. ..x. ...x } = (0,0)",
            "S { .x.x. ..x.A .x.x. .x..x } = (1,2)",
        ]);
        let mut search = InitialPosition {
            deal: deal.clone(),
            trumps: Some(crate::Suit::S),
            declarer: Player::S,
            next: Some(Player::S),
        }
        .search_impl(table, Logger::default());
        assert_eq!(1, search.run_unbounded());
    }

    #[test]
    fn small_deal_6() {
        let deal = Deal::try_from_pbn(".Q.A. ..Q.A .A.K. .K..K").unwrap();
        println!("{}", deal.format_as_table());
        let table = TestTable::new(&[
            "N { ..A. ..x. ..x. .x.. } = (0,0)",
            "S { .x.A. ..x.A .A.x. .x..x } = (0,1)",
            "E { ..A. ...x ..x. ...x } = (0,0)",
            "E { ..A. ..x. ..x. ...x } = (0,0)",
            "S { .x.x. ..x.A .x.x. .x..x } = (1,2)",
        ]);
        let mut search = InitialPosition {
            deal: deal.clone(),
            trumps: Some(crate::Suit::S),
            declarer: Player::S,
            next: Some(Player::S),
        }
        .search_impl(table, Logger::default());
        assert_eq!(1, search.run_unbounded());
    }

    #[test]
    fn small_deal_7() {
        let deal = Deal::try_from_pbn(".Q.A. ..Q.A .K.K. .A..K").unwrap();

        let table = TestTable::new(&[
            "S { ..x. ...A ..x. ...x } = (1,1)",
            "S { .x.x. ..x.A .x.x. .A..x } = (2,2)",
        ]);
        let mut search = InitialPosition {
            deal: deal.clone(),
            trumps: Some(crate::Suit::S),
            declarer: Player::S,
            next: Some(Player::S),
        }
        .search_impl(table, Logger::default());
        assert_eq!(2, search.run_unbounded());
    }

    #[test]
    fn small_deal_8() {
        let deal = Deal::try_from_pbn("K.Q653..T9 .AT.QJ.KQ6 A.K984.T.8 .J7.AK.AJ7").unwrap();

        let mut search = InitialPosition {
            deal: deal.clone(),
            trumps: Some(crate::Suit::S),
            declarer: Player::N,
            next: Some(Player::S),
        }
        .search_impl(UnsyncTable::new(1000), Empty::default());
        assert_eq!(2, search.run_unbounded());
    }

    #[test]
    fn small_deal_9() {
        let deal = Deal::try_from_pbn("K.Q653..98 .AT.QJ.KQ6 A.K984.T.7 .J7.AK.AJT").unwrap();

        let mut search = InitialPosition {
            deal: deal.clone(),
            trumps: Some(crate::Suit::S),
            declarer: Player::N,
            next: Some(Player::S),
        }
        .search_impl(UnsyncTable::new(1000), Empty::default());
        assert_eq!(2, search.run_unbounded());
    }

    #[test]
    fn small_deal_10() {
        let deal = Deal::try_from_pbn("K.Q653..T9 .AT.QJ.KQ6 A.K984.T.8 .J7.AK.AJ7").unwrap();

        let mut search = InitialPosition {
            deal: deal.clone(),
            trumps: Some(crate::Suit::S),
            declarer: Player::N,
            next: Some(Player::S),
        }
        .search_impl(UnsyncTable::new(1000), Empty::default());
        assert_eq!(2, search.run_unbounded());
    }

    #[test]
    fn small_deal_11() {
        let deal = Deal::try_from_pbn("K.764..98 .AJ.K.KQ6 A.KT95..7 .Q8.A.AJT").unwrap();

        let mut search = InitialPosition {
            deal: deal.clone(),
            trumps: Some(crate::Suit::S),
            declarer: Player::N,
            next: Some(Player::S),
        }
        .search_impl(UnsyncTable::new(1000), Empty::default());
        assert_eq!(3, search.run_unbounded());
    }

    #[test]
    fn small_deal_12() {
        let deal = Deal::try_from_pbn("K.764..Q .AJ.A.AT A.KT95.. .Q8.K.KJ").unwrap();

        let mut search = InitialPosition {
            deal: deal.clone(),
            trumps: Some(crate::Suit::S),
            declarer: Player::N,
            next: Some(Player::S),
        }
        .search_impl(UnsyncTable::new(1000), Empty::default());
        assert_eq!(2, search.run_unbounded());
    }

    #[test]
    fn small_deal_13() {
        let deal = Deal::try_from_pbn("A.KQ9.. .J.K.KQ K.T8..J .A.A.AT").unwrap();

        let mut search = InitialPosition {
            deal: deal.clone(),
            trumps: Some(crate::Suit::S),
            declarer: Player::N,
            next: Some(Player::S),
        }
        .search_impl(UnsyncTable::new(1000), Logger::default());
        assert_eq!(1, search.run_unbounded());
    }

    #[test]
    fn small_deal_14() {
        let deal = Deal::try_from_pbn(".KQT.. .J..KQ A.9..J .A..AT").unwrap();

        let mut search = InitialPosition {
            deal: deal.clone(),
            trumps: Some(crate::Suit::S),
            declarer: Player::S,
            next: Some(Player::W),
        }
        .search_impl(UnsyncTable::new(1000), Logger::default());
        assert_eq!(2, search.run_unbounded());
    }

    #[test]
    fn bermuda_2() {
        let deal = Deal::try_from(ByPlayer::<&str> {
            w: ".Q8..",
            n: ".T6..",
            e: ".AJ..",
            s: "6.K..",
        })
        .unwrap();

        let table = TestTable::new(&[
            "W { .Ax.. x.x.. .xx.. .xx.. } = (0,1)",
            "W { .x.. x... .x.. .x.. } = (1,1)",
            "W { .Ax.. x.K.. .xx.. .xx.. } = (1,2)",
        ]);
        let mut search = InitialPosition {
            deal: deal.clone(),
            trumps: Some(crate::Suit::S),
            declarer: Player::N,
            next: None,
        }
        .search_impl(table, Logger::default());
        assert_eq!(1, search.run_unbounded());

        let table = TestTable::new(&["W { .AJ.. .K..x .Qx.. .xx.. } = (0,0)"]);
        let mut search = InitialPosition {
            deal: deal.clone(),
            trumps: Some(crate::Suit::C),
            declarer: Player::N,
            next: None,
        }
        .search_impl(table, Logger::default());
        assert_eq!(0, search.run_unbounded());

        let table = TestTable::new(&["W { .AJ.. x.K.. .Qx.. .xx.. } = (0,0)"]);
        let mut search = InitialPosition {
            deal: deal.clone(),
            trumps: None,
            declarer: Player::N,
            next: None,
        }
        .search_impl(table, Logger::default());
        assert_eq!(0, search.run_unbounded());
    }

    #[test]
    fn bermuda_3() {
        let deal = Deal::try_from(ByPlayer::<&str> {
            w: ".Q87..",
            n: ".T6.2.",
            e: ".AJ5..",
            s: "6.K9..",
        })
        .unwrap();
        assert_eq!(
            2,
            search(
                &InitialPosition {
                    deal,
                    trumps: Some(crate::Suit::S),
                    declarer: Player::N,
                    next: None,
                },
                &Config::default()
            )
        );
    }

    #[test]
    fn bermuda_4() {
        let deal = Deal::try_from(ByPlayer::<&str> {
            n: ".T6.2.9",
            w: ".Q873..",
            e: ".AJ54..",
            s: "6.K9..4",
        })
        .unwrap();
        // let table = TestTable::new(&[]);
        let mut search = InitialPosition {
            deal: deal,
            trumps: Some(crate::Suit::S),
            declarer: Player::S,
            next: None,
        }
        .search_impl(UnsyncTable::new(1000), Logger::default());
        assert_eq!(3, search.run_unbounded());
    }

    #[test]
    fn bermuda_5() {
        let deal = Deal::try_from(ByPlayer::<&str> {
            w: ".Q873.8.",
            n: ".T6.2.93",
            e: ".AJ54.5.",
            s: "6.K9..Q4",
        })
        .unwrap();
        assert_eq!(
            4,
            search(
                &InitialPosition {
                    deal: deal,
                    trumps: Some(crate::Suit::S),
                    declarer: Player::S,
                    next: None,
                },
                &Config::default()
            )
        );
    }

    #[cfg(not(debug_assertions))]
    #[test]
    fn bermuda_1983() {
        use crate::analyze::ByStrain;

        let deal = Deal::try_from_pbn(
            "854.Q873.Q984.65 KQ32.T6.T72.AJ93 9.AJ542.J653.T87 AJT76.K9.AK.KQ42",
        )
        .unwrap();
        let result = ByPlayer::par_new(|declarer| {
            ByStrain::par_new(|s| {
                let mut search = InitialPosition {
                    deal: deal.clone(),
                    trumps: s.trumps,
                    declarer,
                    next: None,
                }
                .search_impl(UnsyncTable::new(1000), Empty {});
                search.run_unbounded()
            })
        });

        assert_eq!(
            ByPlayer {
                w: ByStrain {
                    c: 1u8,
                    d: 6u8,
                    h: 7u8,
                    s: 1u8,
                    n: 1u8,
                },
                n: ByStrain {
                    c: 12u8,
                    d: 6u8,
                    h: 6u8,
                    s: 12u8,
                    n: 12u8,
                },
                e: ByStrain {
                    c: 1u8,
                    d: 6u8,
                    h: 7u8,
                    s: 1u8,
                    n: 1u8,
                },
                s: ByStrain {
                    c: 12u8,
                    d: 6u8,
                    h: 6u8,
                    s: 12u8,
                    n: 12u8,
                },
            },
            result
        );
    }

    #[test]
    fn bermuda_6() {
        let deal = Deal::try_from_pbn(".Q873.84. 2.T6.2.93 .AJ54.53. 76.K9..Q4").unwrap();
        let table = UnsyncTable::new(100);
        let mut search = InitialPosition {
            deal,
            trumps: Some(Suit::S),
            declarer: Player::N,
            next: None,
        }
        .search_impl(table, Empty {});

        assert_eq!(5, search.run_unbounded());
        assert_eq!(
            "[(E, [🂽, 🂺, 🂱, 🂹]), (E, [🂷, 🂶, 🂻, 🂾]), (S, [🂸, 🂢, 🂴, 🂦]), (S, [🂳, 🃂, 🂵, 🂧]), (S, [🃈, 🃙, 🃅, 🃝]), (S, [🃄, 🃓, 🃃, 🃔])]",
            format!("{:?}", search.optimal_line(5))
        );
    }

    #[test]
    fn bermuda_7() {
        let deal = Deal::try_from(ByPlayer::<&str> {
            n: "32.T6.2.93",
            w: "4.Q873.84.",
            e: ".AJ54.53.7",
            s: "T76.K9..Q4",
        })
        .unwrap();

        let table = UnsyncTable::new(1024);
        let mut search = InitialPosition {
            deal,
            trumps: Some(crate::Suit::S),
            declarer: Player::S,
            next: Some(Player::S),
        }
        .search_impl(table, Empty {});
        assert_eq!(6, search.run_unbounded());
    }

    #[test]
    fn bermuda_8() {
        let deal = Deal::try_from_pbn("4.Q873.984. 32.T6.72.93 .AJ54.653.7 T76.K9.K.Q4").unwrap();
        let table = UnsyncTable::new(1024);
        let mut search = InitialPosition {
            deal,
            trumps: Some(crate::Suit::S),
            declarer: Player::S,
            next: Some(Player::S),
        }
        .search_impl(table, Empty {});
        assert_eq!(7, search.run_unbounded());
    }
}
