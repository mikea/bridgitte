use static_assertions::assert_eq_size;

use crate::{CardSet, Deal, Pair, Player, SetItem, Suit};

#[derive(Debug)]
pub struct Trick {
    pub winner: Player,
    pub cards: CardSet,
    first_suit: CardSet,
}

impl Trick {
    pub(crate) fn target_inc(&self) -> u8 {
        Pair::from(self.winner).target_inc()
    }
}

#[allow(clippy::module_name_repetitions)]
pub trait PlayOfCards: Default + Clone + std::fmt::Debug {
    fn is_empty(&self) -> bool;
    fn suit_to_follow(&self) -> CardSet;
    #[must_use]
    fn add(&self, c: &SetItem, p: Player) -> Self;
    fn trick(&self) -> Option<Trick>;
    fn quick_tricks(d: &Deal, p: Player) -> (u8, CardSet);
    fn rel_mask(trick: &Trick, rel_mask: CardSet) -> CardSet;
}

/// 1-4 cards that were played during 1 trick
#[derive(Debug, Clone, Default)]
pub struct PlayedCardsNT {
    cards: CardSet,
    first_suit: CardSet,
    winner: Option<Player>,
}

impl PlayOfCards for PlayedCardsNT {
    fn add(&self, c: &SetItem, p: Player) -> PlayedCardsNT {
        if self.first_suit.is_empty() {
            // first card was played
            return Self {
                cards: c.card_set,
                first_suit: c.suit,
                winner: Some(p),
            };
        };
        let winner = self.winner;

        debug_assert!(self.cards.size() >= 1);
        debug_assert!(self.cards.size() < 4);

        let card_set = c.card_set;
        let value_suits = self.first_suit;
        let value_cards = self.cards & value_suits;
        // a card is new max if it matches first suit and is greater than any first suit cards
        let new_winner = card_set.intersects(value_suits) && card_set.gt(value_cards);

        let cards = self.cards | card_set;
        let winner = if new_winner { Some(p) } else { winner };

        Self {
            first_suit: self.first_suit,
            winner,
            cards,
        }
    }

    fn trick(&self) -> Option<Trick> {
        if self.cards.size() == 4 {
            self.winner.map(|winner| Trick {
                winner,
                cards: self.cards,
                first_suit: self.first_suit,
            })
        } else {
            None
        }
    }

    fn suit_to_follow(&self) -> CardSet {
        self.first_suit
    }

    fn is_empty(&self) -> bool {
        self.first_suit.is_empty()
    }

    fn quick_tricks(d: &Deal, p: Player) -> (u8, CardSet) {
        d.get(p).high_card_seq()
    }

    fn rel_mask(trick: &Trick, mut rel_mask: CardSet) -> CardSet {
        if !rel_mask.is_empty() {
            rel_mask = rel_mask.unpromote(trick.cards);
        }

        let first_suit_cards = trick.cards & trick.first_suit;
        let rank_win = first_suit_cards.size() > 1;

        if rank_win {
            let first_suit_cards = trick.cards & trick.first_suit;
            first_suit_cards.hi_card_rel_mask(trick.first_suit) | rel_mask
        } else {
            rel_mask
        }
    }
}

/// trump suite is allways SPADES
#[derive(Debug, Clone, Default)]
pub struct PlayedCardsT {
    cards: CardSet,
    first_suit: CardSet,
    winner: Option<Player>,
}

impl PlayOfCards for PlayedCardsT {
    fn add(&self, c: &SetItem, p: Player) -> PlayedCardsT {
        if self.first_suit.is_empty() {
            // first card was played
            return Self {
                cards: c.card_set,
                first_suit: c.suit,
                winner: Some(p),
            };
        };
        debug_assert!(self.cards.size() >= 1);
        debug_assert!(self.cards.size() < 4);

        let card_set: CardSet = c.card_set;

        let value_suits = self.first_suit | CardSet::S;
        let value_cards = self.cards & value_suits;

        // we really use the fact that trumps are spades and spade cards are greater than others.
        let new_winner = card_set.gt(value_cards) && card_set.intersects(value_suits);

        let winner = if new_winner { Some(p) } else { self.winner };
        let cards = self.cards | card_set;

        Self {
            first_suit: self.first_suit,
            winner,
            cards,
        }
    }

    fn trick(&self) -> Option<Trick> {
        if self.cards.size() == 4 {
            self.winner.map(|winner| Trick {
                winner,
                cards: self.cards,
                first_suit: self.first_suit,
            })
        } else {
            None
        }
    }

    fn suit_to_follow(&self) -> CardSet {
        self.first_suit
    }

    fn is_empty(&self) -> bool {
        self.first_suit.is_empty()
    }

    fn quick_tricks(deal: &Deal, player: Player) -> (u8, CardSet) {
        let hand = deal.get(player);
        let trump_count = (deal & CardSet::S).count();

        let opp_trumps = max2(Pair::from(player).opposite().filter(trump_count));
        let s = hand.high_card_suit_seq(CardSet::S);

        if opp_trumps > s.0 {
            // opponents have too many trumps to be sure
            return s;
        }

        let h = hand.high_card_suit_seq(CardSet::H);
        let d = hand.high_card_suit_seq(CardSet::D);
        let c = hand.high_card_suit_seq(CardSet::C);
        (s.0 + h.0 + d.0 + c.0, s.1 | h.1 | d.1 | c.1)
    }

    fn rel_mask(trick: &Trick, mut rel_mask: CardSet) -> CardSet {
        if !rel_mask.is_empty() {
            rel_mask = rel_mask.unpromote(trick.cards);
        }
        let trump_cards = trick.cards & CardSet::S;
        let trump_win = !(trump_cards).is_empty();

        if trump_win {
            let rank_win = trump_cards.size() > 1;
            if rank_win {
                trump_cards.hi_card_rel_mask(CardSet::S) | rel_mask
            } else {
                rel_mask
            }
        } else {
            let first_suit_cards = trick.cards & trick.first_suit;
            let rank_win = first_suit_cards.size() > 1;

            if rank_win {
                first_suit_cards.hi_card_rel_mask(trick.first_suit) | rel_mask
            } else {
                rel_mask
            }
        }
    }
}

fn max2<T: PartialOrd + Copy>(arr: [T; 2]) -> T {
    if arr[0] > arr[1] {
        arr[0]
    } else {
        arr[1]
    }
}

#[derive(Debug)]
pub struct Contract {
    pub declarer: Player,
    pub trumps: Option<Suit>,
    pub total_tricks: u8,
}

#[derive(Debug)]
pub struct Undo<PoC: PlayOfCards> {
    play: PoC,
    next: Player,
    deal: Option<[u64; 4]>,
}
// `ApplyUndo` is allocated on the stack all the time, keep track of its size
assert_eq_size!([u64; 9], Undo<PlayedCardsNT>);

#[derive(Debug)]
#[allow(clippy::module_name_repetitions)]
pub struct PlayState<PoC: PlayOfCards> {
    /// current cards on hand
    pub deal: Deal,
    /// cards already played on the table round
    pub play: PoC,
    /// next player to play a card
    pub next: Player,
}

impl<PoC: PlayOfCards> PlayState<PoC> {
    #[must_use]
    pub(crate) fn new(deal: &Deal, next: Player) -> Self {
        Self {
            deal: deal.clone(),
            next,
            play: PoC::default(),
        }
    }

    // all possible plays
    pub(crate) fn plays(&self) -> CardSet {
        let hand = self.deal.get(self.next);
        let suit_to_follow = self.play.suit_to_follow();

        if suit_to_follow.is_empty() {
            // first player can play any card
            return hand;
        };

        // otherwise the first suit must be followed
        let hand_suit = hand & suit_to_follow;
        if !hand_suit.is_empty() {
            return hand_suit;
        }

        // if first suit not available, then any card can be played
        hand
    }

    pub(crate) fn apply_mut(&mut self, s: &SetItem) -> (Option<Trick>, Undo<PoC>) {
        let mut undo = Undo::<PoC> {
            play: self.play.clone(),
            next: self.next,
            deal: None,
        };

        let player = self.next;
        self.next = player.next();

        self.play = self.play.add(s, player);
        let trick = self.play.trick();
        if let Some(trick) = &trick {
            self.next = trick.winner;
            self.play = PoC::default();
            let cards = trick.cards;
            let mut deal = self.deal.remove_all(cards);
            deal = deal.promote_masked(cards);
            std::mem::swap(&mut deal, &mut self.deal);
            undo.deal = Some(deal.into());
        }

        (trick, undo)
    }

    pub(crate) fn undo(&mut self, undo: Undo<PoC>) {
        self.next = undo.next;
        self.play = undo.play;
        if let Some(deal) = undo.deal {
            self.deal = deal.into();
        }
    }

    /// Number of quick tricks possible without any intervention for current player.
    pub(crate) fn quick_tricks(&self) -> (u8, CardSet) {
        debug_assert!(self.play.is_empty());
        PoC::quick_tricks(&self.deal, self.next)
    }
}
