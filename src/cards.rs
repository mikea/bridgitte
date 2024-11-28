use std::{
    fmt::{Debug, Write},
    ops::{BitAnd, BitOr, Shl, Shr, Sub},
    simd::{num::SimdUint, u64x4, Simd}, str::FromStr,
};

use regex::Regex;
use static_assertions::assert_eq_size;

use crate::{
    bits::{
        extract_highest_bit_set, extract_lowest_bit_set, fill_ones_below_lowest_bit_set,
        mask_from_highest_bit_set, mask_up_to_lowest_bit_set, reset_highest_bit_set,
        reset_lowest_bit_set,
    },
    ByPlayer, Player, PlayerMap,
};

const SUIT_MASK: u8 = 0b1111_0000;
const VALUE_MASK: u8 = 0b0000_1111;

#[derive(Clone, Copy, PartialEq, Eq)]
#[allow(clippy::unreadable_literal)]
#[repr(u8)]
pub enum Suit {
    C = 0b0001_0000,
    D = 0b0010_0000,
    H = 0b0100_0000,
    S = 0b1000_0000,
}

assert_eq_size!([u8; 1], Suit);
assert_eq_size!([u8; 1], Option<Suit>);

impl Suit {
    pub const ALL: [Suit; 4] = [Suit::C, Suit::D, Suit::H, Suit::S];

    #[must_use]
    pub fn symbol(self) -> &'static str {
        match self {
            Suit::C => "\u{2663}",
            Suit::D => "\u{2666}",
            Suit::H => "\u{2665}",
            Suit::S => "\u{2660}",
        }
    }

    fn index(self) -> usize {
        match self {
            Suit::C => 0,
            Suit::D => 1,
            Suit::H => 2,
            Suit::S => 3,
        }
    }

    fn card_mask(self) -> u8 {
        self as u8
    }

    unsafe fn from_mask(mask: u8) -> Self {
        std::mem::transmute(mask)
    }

    fn bit_index(self) -> usize {
        match self {
            Suit::C => 0,
            Suit::D => 16,
            Suit::H => 32,
            Suit::S => 48,
        }
    }
}

impl std::fmt::Debug for Suit {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(self.symbol())
    }
}

impl TryFrom<char> for Suit {
    type Error = ();

    fn try_from(value: char) -> Result<Self, Self::Error> {
        match value {
            '\u{2663}' => Ok(Self::C),
            '\u{2666}' => Ok(Self::D),
            '\u{2665}' => Ok(Self::H),
            '\u{2660}' => Ok(Self::S),
            _ => Err(()),
        }
    }
}

impl FromStr for Suit {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match &s.to_uppercase()[..1] {
            "S" => Ok(Suit::S),
            "H" => Ok(Suit::H),
            "D" => Ok(Suit::D),
            "C" => Ok(Suit::C),
            _ => Err(format!("bad suit: {s}")),
        }
    }
}

#[derive(Clone, Copy, PartialEq, Eq)]
#[repr(u8)]
#[allow(clippy::unreadable_literal)]
pub enum Rank {
    _2 = 0b_0000,
    _3 = 0b_0001,
    _4 = 0b_0010,
    _5 = 0b_0011,
    _6 = 0b_0100,
    _7 = 0b_0101,
    _8 = 0b_0110,
    _9 = 0b_0111,
    _T = 0b_1000,
    _J = 0b_1001,
    _Q = 0b_1010,
    _K = 0b_1011,
    _A = 0b_1100,
}

impl Rank {
    fn symbol(self) -> char {
        match self {
            Rank::_2 => '2',
            Rank::_3 => '3',
            Rank::_4 => '4',
            Rank::_5 => '5',
            Rank::_6 => '6',
            Rank::_7 => '7',
            Rank::_8 => '8',
            Rank::_9 => '9',
            Rank::_T => 'T',
            Rank::_J => 'J',
            Rank::_Q => 'Q',
            Rank::_K => 'K',
            Rank::_A => 'A',
        }
    }

    fn index(self) -> usize {
        self.mask() as usize
    }

    fn mask(self) -> u8 {
        self as u8
    }

    unsafe fn from_index(value: u8) -> Rank {
        debug_assert!(value < 13);
        unsafe { std::mem::transmute(value) }
    }
}

impl Debug for Rank {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_char(self.symbol())
    }
}

impl From<Rank> for u8 {
    fn from(value: Rank) -> Self {
        value as Self
    }
}

impl TryFrom<char> for Rank {
    type Error = String;

    fn try_from(ch: char) -> Result<Self, Self::Error> {
        match ch {
            '2' => Ok(Self::_2),
            '3' => Ok(Self::_3),
            '4' => Ok(Self::_4),
            '5' => Ok(Self::_5),
            '6' => Ok(Self::_6),
            '7' => Ok(Self::_7),
            '8' => Ok(Self::_8),
            '9' => Ok(Self::_9),
            'T' => Ok(Self::_T),
            'J' => Ok(Self::_J),
            'Q' => Ok(Self::_Q),
            'K' => Ok(Self::_K),
            'A' => Ok(Self::_A),
            _ => Err(format!("bad char: {ch}")),
        }
    }
}

#[allow(clippy::unreadable_literal)]
#[derive(Clone, Copy, PartialEq, Eq)]
#[repr(u8)]
pub enum Card {
    C2 = 0b0001_0000,
    C3 = 0b0001_0001,
    C4 = 0b0001_0010,
    C5 = 0b0001_0011,
    C6 = 0b0001_0100,
    C7 = 0b0001_0101,
    C8 = 0b0001_0110,
    C9 = 0b0001_0111,
    CT = 0b0001_1000,
    CJ = 0b0001_1001,
    CQ = 0b0001_1010,
    CK = 0b0001_1011,
    CA = 0b0001_1100,
    D2 = 0b0010_0000,
    D3 = 0b0010_0001,
    D4 = 0b0010_0010,
    D5 = 0b0010_0011,
    D6 = 0b0010_0100,
    D7 = 0b0010_0101,
    D8 = 0b0010_0110,
    D9 = 0b0010_0111,
    DT = 0b0010_1000,
    DJ = 0b0010_1001,
    DQ = 0b0010_1010,
    DK = 0b0010_1011,
    DA = 0b0010_1100,
    H2 = 0b0100_0000,
    H3 = 0b0100_0001,
    H4 = 0b0100_0010,
    H5 = 0b0100_0011,
    H6 = 0b0100_0100,
    H7 = 0b0100_0101,
    H8 = 0b0100_0110,
    H9 = 0b0100_0111,
    HT = 0b0100_1000,
    HJ = 0b0100_1001,
    HQ = 0b0100_1010,
    HK = 0b0100_1011,
    HA = 0b0100_1100,
    S2 = 0b1000_0000,
    S3 = 0b1000_0001,
    S4 = 0b1000_0010,
    S5 = 0b1000_0011,
    S6 = 0b1000_0100,
    S7 = 0b1000_0101,
    S8 = 0b1000_0110,
    S9 = 0b1000_0111,
    ST = 0b1000_1000,
    SJ = 0b1000_1001,
    SQ = 0b1000_1010,
    SK = 0b1000_1011,
    SA = 0b1000_1100,
}

impl Card {
    const ALL: [Card; 52] = [
        Card::C2,
        Card::C3,
        Card::C4,
        Card::C5,
        Card::C6,
        Card::C7,
        Card::C8,
        Card::C9,
        Card::CT,
        Card::CJ,
        Card::CQ,
        Card::CK,
        Card::CA,
        Card::D2,
        Card::D3,
        Card::D4,
        Card::D5,
        Card::D6,
        Card::D7,
        Card::D8,
        Card::D9,
        Card::DT,
        Card::DJ,
        Card::DQ,
        Card::DK,
        Card::DA,
        Card::H2,
        Card::H3,
        Card::H4,
        Card::H5,
        Card::H6,
        Card::H7,
        Card::H8,
        Card::H9,
        Card::HT,
        Card::HJ,
        Card::HQ,
        Card::HK,
        Card::HA,
        Card::S2,
        Card::S3,
        Card::S4,
        Card::S5,
        Card::S6,
        Card::S7,
        Card::S8,
        Card::S9,
        Card::ST,
        Card::SJ,
        Card::SQ,
        Card::SK,
        Card::SA,
    ];

    const BIT_INDEX_TO_INDEX: [u8; 64] = [
        0,
        1,
        2,
        3,
        4,
        5,
        6,
        7,
        8,
        9,
        10,
        11,
        12,
        255,
        255,
        255,
        (13),
        (1 + 13),
        (2 + 13),
        (3 + 13),
        (4 + 13),
        (5 + 13),
        (6 + 13),
        (7 + 13),
        (8 + 13),
        (9 + 13),
        (10 + 13),
        (11 + 13),
        (12 + 13),
        255,
        255,
        255,
        (26),
        (1 + 26),
        (2 + 26),
        (3 + 26),
        (4 + 26),
        (5 + 26),
        (6 + 26),
        (7 + 26),
        (8 + 26),
        (9 + 26),
        (10 + 26),
        (11 + 26),
        (12 + 26),
        255,
        255,
        255,
        (39),
        (1 + 39),
        (2 + 39),
        (3 + 39),
        (4 + 39),
        (5 + 39),
        (6 + 39),
        (7 + 39),
        (8 + 39),
        (9 + 39),
        (10 + 39),
        (11 + 39),
        (12 + 39),
        255,
        255,
        255,
    ];

    unsafe fn from_mask(mask: u8) -> Self {
        std::mem::transmute(mask)
    }

    fn new(value: Rank, suit: Suit) -> Self {
        unsafe { Self::from_mask(value.mask() | suit.card_mask()) }
    }

    #[must_use]
    pub(crate) fn value(self) -> Rank {
        unsafe { Rank::from_index(self.mask() & VALUE_MASK) }
    }

    fn mask(self) -> u8 {
        self as u8
    }

    #[must_use]
    pub(crate) fn suit(self) -> Suit {
        unsafe { Suit::from_mask(self.mask() & SUIT_MASK) }
    }

    unsafe fn from_bit_index(bit_index: usize) -> Card {
        debug_assert!(bit_index < 64);
        let index = *unsafe { Card::BIT_INDEX_TO_INDEX.get_unchecked(bit_index) };
        debug_assert!(index < 52, "{index} >= 52 {bit_index}");
        *unsafe { Card::ALL.get_unchecked(index as usize) }
    }

    #[must_use]
    pub fn symbol(self) -> char {
        match self {
            Self::C2 => '\u{1f0d2}',
            Self::C3 => '\u{1f0d3}',
            Self::C4 => '\u{1f0d4}',
            Self::C5 => '\u{1f0d5}',
            Self::C6 => '\u{1f0d6}',
            Self::C7 => '\u{1f0d7}',
            Self::C8 => '\u{1f0d8}',
            Self::C9 => '\u{1f0d9}',
            Self::CT => '\u{1f0da}',
            Self::CJ => '\u{1f0db}',
            Self::CQ => '\u{1f0dd}',
            Self::CK => '\u{1f0de}',
            Self::CA => '\u{1f0d1}',
            Self::D2 => '\u{1f0c2}',
            Self::D3 => '\u{1f0c3}',
            Self::D4 => '\u{1f0c4}',
            Self::D5 => '\u{1f0c5}',
            Self::D6 => '\u{1f0c6}',
            Self::D7 => '\u{1f0c7}',
            Self::D8 => '\u{1f0c8}',
            Self::D9 => '\u{1f0c9}',
            Self::DT => '\u{1f0ca}',
            Self::DJ => '\u{1f0cb}',
            Self::DQ => '\u{1f0cd}',
            Self::DK => '\u{1f0ce}',
            Self::DA => '\u{1f0c1}',
            Self::H2 => '\u{1f0b2}',
            Self::H3 => '\u{1f0b3}',
            Self::H4 => '\u{1f0b4}',
            Self::H5 => '\u{1f0b5}',
            Self::H6 => '\u{1f0b6}',
            Self::H7 => '\u{1f0b7}',
            Self::H8 => '\u{1f0b8}',
            Self::H9 => '\u{1f0b9}',
            Self::HT => '\u{1f0ba}',
            Self::HJ => '\u{1f0bb}',
            Self::HQ => '\u{1f0bd}',
            Self::HK => '\u{1f0be}',
            Self::HA => '\u{1f0b1}',
            Self::S2 => '\u{1f0a2}',
            Self::S3 => '\u{1f0a3}',
            Self::S4 => '\u{1f0a4}',
            Self::S5 => '\u{1f0a5}',
            Self::S6 => '\u{1f0a6}',
            Self::S7 => '\u{1f0a7}',
            Self::S8 => '\u{1f0a8}',
            Self::S9 => '\u{1f0a9}',
            Self::ST => '\u{1f0aa}',
            Self::SJ => '\u{1f0ab}',
            Self::SQ => '\u{1f0ad}',
            Self::SK => '\u{1f0ae}',
            Self::SA => '\u{1f0a1}',
        }
    }

    fn bit_index(self) -> usize {
        self.suit().bit_index() + self.value().index()
    }

    pub(crate) fn index(self) -> usize {
        self.suit().index() * 13 + self.value().index()
    }
}

impl std::fmt::Debug for Card {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_char(self.symbol())
    }
}

#[derive(Clone)]
pub struct SetItem {
    pub card_set: CardSet,
    pub suit: CardSet,
}
assert_eq_size!([u64; 2], SetItem);

impl SetItem {
    pub(crate) fn card(&self) -> Card {
        self.card_set.lo_card().unwrap()
    }
}

impl Debug for SetItem {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_char(self.card().symbol())
    }
}

struct CardIterator {
    set: CardSet,
    suit: CardSet,
}

impl Iterator for CardIterator {
    type Item = SetItem;

    fn next(&mut self) -> Option<Self::Item> {
        if self.set.is_empty() {
            return None;
        }

        let card_set = self.set.lo_card_set();
        while !card_set.intersects(self.suit) {
            self.suit = self.suit << 16;
        }
        self.set = self.set.clear_lowest_bit_set();

        Some(SetItem {
            card_set,
            suit: self.suit,
        })
    }
}

impl Shl<u32> for CardSet {
    type Output = CardSet;
    fn shl(self, rhs: u32) -> Self::Output {
        CardSet {
            mask: self.mask << rhs,
        }
    }
}

impl Shr<u32> for CardSet {
    type Output = CardSet;
    fn shr(self, rhs: u32) -> Self::Output {
        CardSet {
            mask: self.mask >> rhs,
        }
    }
}

struct CardRevIterator {
    mask: u64,
    suit: CardSet,
}

impl Iterator for CardRevIterator {
    type Item = SetItem;

    fn next(&mut self) -> Option<Self::Item> {
        if self.mask == 0 {
            return None;
        }

        let card_set = CardSet::from(extract_lowest_bit_set(self.mask).reverse_bits());
        while !card_set.intersects(self.suit) {
            self.suit = self.suit >> 16;
        }
        self.mask = reset_lowest_bit_set(self.mask);
        Some(SetItem {
            card_set,
            suit: self.suit,
        })
    }
}

impl CardRevIterator {
    fn new(set: CardSet) -> Self {
        CardRevIterator {
            mask: set.mask.reverse_bits(),
            suit: CardSet::S,
        }
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Hash, Default)]
pub struct CardSet {
    mask: u64,
}
assert_eq_size!([u64; 1], CardSet);

impl CardSet {
    pub const ALL52: CardSet = CardSet {
        mask: 0b_0001_1111_1111_1111_0001_1111_1111_1111_0001_1111_1111_1111_0001_1111_1111_1111,
    };

    pub const C: CardSet = CardSet {
        mask: 0b_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0001_1111_1111_1111,
    };

    pub const D: CardSet = CardSet {
        mask: 0b_0000_0000_0000_0000_0000_0000_0000_0000_0001_1111_1111_1111_0000_0000_0000_0000,
    };

    pub const H: CardSet = CardSet {
        mask: 0b_0000_0000_0000_0000_0001_1111_1111_1111_0000_0000_0000_0000_0000_0000_0000_0000,
    };

    pub const S: CardSet = CardSet {
        mask: 0b_0001_1111_1111_1111_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000,
    };

    pub const _A: CardSet = CardSet {
        mask: 0b_0001_0000_0000_0000_0001_0000_0000_0000_0001_0000_0000_0000_0001_0000_0000_0000,
    };

    // order of Suit values
    pub(crate)  const SUITS: [CardSet; 4] = [Self::C, Self::D, Self::H, Self::S];
    const SUIT_MASKS: u64x4 =
        Simd::from_array([Self::C.mask, Self::D.mask, Self::H.mask, Self::S.mask]);

    #[must_use]
    pub(crate) fn is_empty(self) -> bool {
        self.mask == 0
    }

    #[must_use]
    pub(crate) fn filter_suit(self, suit: Suit) -> CardSet {
        self & Self::SUITS[suit.index()]
    }

    pub(crate) fn iter(self) -> impl Iterator<Item = SetItem> {
        CardIterator {
            set: self,
            suit: CardSet::C,
        }
    }

    pub(crate) fn rev_iter(self) -> impl Iterator<Item = SetItem> {
        CardRevIterator::new(self)
    }

    const BPN_SUITS: [Suit; 4] = [Suit::S, Suit::H, Suit::D, Suit::C];

    /// # Errors
    /// Fails on bad bpn input
    pub(crate) fn try_from_bpn(s: &str) -> Result<CardSet, String> {
        let suits: Vec<&str> = s.split('.').collect();
        if suits.len() != 4 {
            return Err(format!("Wrong number of suits: {s}"));
        }
        let mut set = CardSet::default();

        for (i, suit) in Self::BPN_SUITS.iter().enumerate() {
            for ch in suits[i].chars() {
                let card = Card::new(Rank::try_from(ch)?, *suit);
                set = set | card;
            }
        }
        Ok(set)
    }

    #[must_use]
    pub(crate) fn to_bpn(self) -> String {
        let suits = Self::BPN_SUITS.map(|s| self.to_bpn_single_suit(s));
        suits.join(".")
    }

    #[must_use]
    pub(crate) fn to_bpn_masked(self, mask: CardSet) -> String {
        let suits = Self::BPN_SUITS.map(|s| self.to_bpn_masked_single_suit(s, mask));
        suits.join(".")
    }

    fn to_bpn_single_suit(self, suit: Suit) -> String {
        let mut result = String::new();
        let mut cards: Vec<Card> = self.filter_suit(suit).iter().map(|s| s.card()).collect();
        cards.reverse();
        for c in cards {
            result.push(c.value().symbol());
        }
        result
    }

    fn to_bpn_masked_single_suit(self, suit: Suit, mask: CardSet) -> String {
        let mut result = String::new();
        let mut cards: Vec<Card> = self.filter_suit(suit).iter().map(|s| s.card()).collect();
        cards.reverse();
        for c in cards {
            if mask.contains(c) {
                result.push(c.value().symbol());
            } else {
                result.push('x');
            }
        }
        result
    }

    #[must_use]
    #[allow(clippy::cast_possible_truncation)]
    pub(crate) fn size(self) -> u8 {
        self.mask.count_ones() as u8
    }

    #[must_use]
    pub(crate) fn intersects(self, other: CardSet) -> bool {
        self.mask & other.mask != 0
    }

    // clear lowest bit set
    fn clear_lowest_bit_set(self) -> CardSet {
        debug_assert!(!self.is_empty());
        CardSet {
            mask: reset_lowest_bit_set(self.mask),
        }
    }

    fn lo_card(self) -> Option<Card> {
        if self.is_empty() {
            return None;
        }
        #[allow(clippy::cast_possible_truncation)]
        let tzcnt = self.mask.trailing_zeros();
        let card = unsafe { Card::from_bit_index(tzcnt as usize) };
        Some(card)
    }

    fn lo_card_set(self) -> CardSet {
        debug_assert!(!self.is_empty());
        CardSet {
            mask: extract_lowest_bit_set(self.mask),
        }
    }

    pub(crate) fn hi_card_rel_mask(self, suit: CardSet) -> CardSet {
        debug_assert!(!self.is_empty());
        let mask = mask_from_highest_bit_set(self.mask);
        CardSet::from(mask & suit.mask)
    }

    pub(crate) fn gt(self, cards: CardSet) -> bool {
        self.mask > cards.mask
    }

    /// compute number of high cards in sequence
    pub(crate) fn high_card_seq(self) -> (u8, CardSet) {
        let s = self.high_card_suit_seq(CardSet::S);
        let h = self.high_card_suit_seq(CardSet::H);
        let d = self.high_card_suit_seq(CardSet::D);
        let c = self.high_card_suit_seq(CardSet::C);
        (s.0 + h.0 + d.0 + c.0, s.1 | h.1 | d.1 | c.1)
    }

    #[allow(clippy::cast_possible_truncation)]
    pub(crate) fn high_card_suit_seq(self, suit: CardSet) -> (u8, CardSet) {
        let cards = self & suit;
        if cards.is_empty() {
            return (0, CardSet::default());
        }
        let ones = !suit.mask | cards.mask;
        let rel_mask = reset_lowest_bit_set(mask_from_highest_bit_set(!ones)) & suit.mask;
        (
            (ones.leading_ones() as u8 - suit.mask.leading_zeros() as u8),
            rel_mask.into(),
        )
    }

    fn splat(self) -> u64x4 {
        Simd::splat(self.mask)
    }

    pub(crate) fn unpromote(self, cards: CardSet) -> CardSet {
        let mut result = self.mask;
        for suit in CardSet::SUITS {
            if (suit & cards).is_empty() {
                continue;
            }
            let mut suit_cards = (suit & cards).mask;
            let mut work = self.mask | !suit.mask;

            while suit_cards != 0 {
                let bit = extract_highest_bit_set(suit_cards);
                if bit & work == 0 {
                    break;
                }
                suit_cards = reset_highest_bit_set(suit_cards);
                work = (work >> 1) | work | 0x8000_0000_0000_0000;
            }

            work &= suit.mask;
            result = (result & !suit.mask) | work;
        }
        result.into()
    }

    fn contains(self, c: Card) -> bool {
        !(CardSet::from(c) & self).is_empty()
    }
    
    pub(crate) fn without(self, other: CardSet) -> CardSet {
        CardSet { mask: self.mask & !other.mask }
    }
}

impl From<Card> for CardSet {
    fn from(card: Card) -> Self {
        Self {
            mask: 1 << card.bit_index(),
        }
    }
}

impl std::fmt::Debug for CardSet {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("CardSet")
            .field("mask", &format!("{:064b}", self.mask))
            .finish()
    }
}

impl From<Suit> for CardSet {
    fn from(suit: Suit) -> Self {
        Self::SUITS[suit.index()]
    }
}

impl From<u64> for CardSet {
    fn from(mask: u64) -> Self {
        CardSet { mask }
    }
}

impl From<&[Card]> for CardSet {
    fn from(cards: &[Card]) -> Self {
        let mut result = Self::default();
        for c in cards {
            result = result | *c;
        }
        result
    }
}

impl BitAnd<CardSet> for CardSet {
    type Output = CardSet;

    fn bitand(self, rhs: CardSet) -> Self::Output {
        Self {
            mask: self.mask & rhs.mask,
        }
    }
}

impl BitOr<CardSet> for CardSet {
    type Output = CardSet;

    fn bitor(self, rhs: CardSet) -> Self::Output {
        Self {
            mask: self.mask | rhs.mask,
        }
    }
}

impl BitOr<Card> for CardSet {
    type Output = CardSet;

    fn bitor(self, rhs: Card) -> Self::Output {
        self | CardSet::from(rhs)
    }
}

impl Sub<CardSet> for CardSet {
    type Output = CardSet;

    fn sub(self, rhs: CardSet) -> Self::Output {
        Self {
            mask: self.mask & !rhs.mask,
        }
    }
}

/// A distribution of cards across 4 players.
#[derive(Clone, Eq, PartialEq, Hash)]
pub struct Deal {
    hands: u64x4,
}
assert_eq_size!([u64; 4], Deal);

impl Deal {
    #[must_use]
    pub(crate) fn is_empty(&self) -> bool {
        self.hands.reduce_or() == 0
    }

    #[must_use]
    pub(crate) fn get(&self, p: Player) -> CardSet {
        self.hands[p.index()].into()
        // unsafe { *self.hands.get_unchecked((p as u8) as usize) }
    }

    #[must_use]
    pub(crate) fn size(&self) -> u32 {
        self.hands[0].count_ones()
            + self.hands[1].count_ones()
            + self.hands[2].count_ones()
            + self.hands[3].count_ones()
    }

    #[must_use]
    pub(crate) fn rotate_to_west(&self, player: Player, map: Option<&mut PlayerMap>) -> Deal {
        let i = player.index();
        if let Some(map) = map {
            map.to_absolute = [
                map.to_absolute[i],
                map.to_absolute[(i + 1) % 4],
                map.to_absolute[(i + 2) % 4],
                map.to_absolute[(i + 3) % 4],
            ];

            let i = 4 - i;
            map.from_absolute = [
                map.from_absolute[i % 4],
                map.from_absolute[(i + 1) % 4],
                map.from_absolute[(i + 2) % 4],
                map.from_absolute[(i + 3) % 4],
            ];
        }
        let hands = [
            self.hands[i],
            self.hands[(i + 1) % 4],
            self.hands[(i + 2) % 4],
            self.hands[(i + 3) % 4],
        ];
        Deal {
            hands: hands.into(),
        }
    }

    #[must_use]
    pub(crate) fn promote_all(&self, map: &mut CardMap) -> Deal {
        map.promote(CardSet::ALL52 - self.union());
        self.promote_masked(CardSet::ALL52)
    }

    pub(crate) fn present(&self) -> CardSet {
        self.hands.reduce_or().into()
    }

    #[must_use]
    pub(crate) fn promote_masked(&self, mask: CardSet) -> Deal {
        let mut hands = self.hands;
        let present: u64 = self.hands.reduce_or();

        let suits = CardSet::SUIT_MASKS & Simd::splat(mask.mask);
        if suits[0] != 0 {
            Self::promote_suit::<{ CardSet::C.mask }>(&mut hands, present);
        }
        if suits[1] != 0 {
            Self::promote_suit::<{ CardSet::D.mask }>(&mut hands, present);
        }
        if suits[2] != 0 {
            Self::promote_suit::<{ CardSet::H.mask }>(&mut hands, present);
        }
        if suits[3] != 0 {
            Self::promote_suit::<{ CardSet::S.mask }>(&mut hands, present);
        }

        Deal { hands }
    }

    #[inline]
    fn promote_suit<const SUIT_MASK: u64>(hands: &mut u64x4, present: u64) {
        let present = present & SUIT_MASK;
        if present == 0 {
            // empty suit, nothing to promote
            return;
        }

        let suit_mask1 = !SUIT_MASK;
        let mut to_promote = !(fill_ones_below_lowest_bit_set(present) | suit_mask1);

        let suit_mask = Simd::splat(SUIT_MASK);
        let suit_mask1 = Simd::splat(suit_mask1);

        while to_promote != 0 {
            let x = extract_lowest_bit_set(to_promote);
            let blsmsk = mask_up_to_lowest_bit_set(x);

            let upper_mask = Simd::splat(blsmsk ^ x);
            let lower_mask = Simd::splat(!blsmsk);

            let present = *hands & suit_mask;
            let upper = present & lower_mask;
            let lower = present & upper_mask;
            let lower = lower << 1;
            let present = upper | lower;
            *hands = (*hands & suit_mask1) | present;

            to_promote = reset_lowest_bit_set(to_promote);
        }
    }

    pub(crate) fn swap_suits(&self, s1: Suit, s2: Suit, map: &mut CardMap) -> Deal {
        if s1 == s2 {
            return self.clone();
        }

        let off1 = Simd::splat(s1.bit_index() as u64);
        let off2 = Simd::splat(s2.bit_index() as u64);

        let s1 = CardSet::from(s1);
        let s2 = CardSet::from(s2);

        // update map
        for (i1, i2) in s1.iter().zip(s2.iter()) {
            let i1 = i1.card().index();
            let i2 = i2.card().index();

            map.from_absolute.swap(i1, i2);
            map.to_absolute.swap(i1, i2);
        }

        // update hands
        let m1 = s1.splat();
        let m2 = s2.splat();

        // extract cards for each suite
        let c1 = self.hands & m1;
        let c2 = self.hands & m2;

        // make them all clubs
        let c1 = c1 >> off1;
        let c2 = c2 >> off2;

        // swap and shift them back
        let new_c1 = c2 << off1;
        let new_c2 = c1 << off2;

        // combine the result
        let others = self.hands & !(m1 | m2);
        let hands = others | new_c1 | new_c2;
        Deal { hands }
    }

    pub(crate) fn remove_all(&self, cards: CardSet) -> Deal {
        let mask = cards.splat();
        let hands = self.hands & !mask;
        Deal { hands }
    }

    #[allow(clippy::cast_possible_truncation)]
    pub(crate) fn max_tricks(&self) -> u8 {
        debug_assert_eq!(0, self.size() % 4);
        (self.size() / 4) as u8
    }

    pub(crate) fn union(&self) -> CardSet {
        self.hands.reduce_or().into()
    }

    /// # Errors
    ///
    /// Returns error in case of invalid pbn.
    pub fn try_from_pbn(s: &str) -> Result<Self, String> {
        let re = Regex::new(r"(.:)?(.+)\s+(.+)\s+(.+)\s+(.+)").expect("invalid regexp");

        let Some(cap) = re.captures(s) else {
            return Err(s.to_owned());
        };

        let hands = [
            CardSet::try_from_bpn(cap.get(2).unwrap().as_str())?.mask,
            CardSet::try_from_bpn(cap.get(3).unwrap().as_str())?.mask,
            CardSet::try_from_bpn(cap.get(4).unwrap().as_str())?.mask,
            CardSet::try_from_bpn(cap.get(5).unwrap().as_str())?.mask,
        ];
        let mut deal = Self {
            hands: hands.into(),
        };

        if let Some(player) = cap.get(1) {
            let player = Player::try_from(player.as_str().chars().next().unwrap())?;
            deal = deal.rotate_first(player);
        }

        Ok(deal)
    }

    /// W rotates and becomes the player
    fn rotate_first(&self, player: Player) -> Deal {
        // 0: 0 1 2 3
        // 1: 3 0 1 2
        // 2: 2 3 0 1

        let i = 4 - player.index();
        let hands = [
            self.hands[i % 4],
            self.hands[(i + 1) % 4],
            self.hands[(i + 2) % 4],
            self.hands[(i + 3) % 4],
        ];
        Deal {
            hands: hands.into(),
        }
    }

    #[must_use]
    pub fn format_as_table(&self) -> String {
        fn format_suit(h: CardSet, suit: Suit) -> String {
            format!("{} {}", suit.symbol(), h.to_bpn_single_suit(suit))
        }
        fn format_hand(h: CardSet) -> [String; 4] {
            [
                format_suit(h, Suit::S),
                format_suit(h, Suit::H),
                format_suit(h, Suit::D),
                format_suit(h, Suit::C),
            ]
        }

        let w = format_hand(self.hands[0].into());
        let n = format_hand(self.hands[1].into());
        let e = format_hand(self.hands[2].into());
        let s = format_hand(self.hands[3].into());

        let n: Vec<_> = n.iter().map(|l| "             ".to_owned() + l).collect();
        let s: Vec<_> = s.iter().map(|l| "             ".to_owned() + l).collect();

        let mid = [
            format!("{:15}            {}", &w[0], &e[0]),
            format!("{:15}            {}", &w[1], &e[1]),
            format!("{:15}            {}", &w[2], &e[2]),
            format!("{:15}            {}", &w[3], &e[3]),
        ];

        n.join("\n") + "\n" + &mid.join("\n") + "\n" + &s.join("\n")
    }

    #[must_use]
    #[allow(clippy::cast_possible_truncation)]
    pub fn count(&self) -> [u8; 4] {
        [
            self.hands[0].count_ones() as u8,
            self.hands[1].count_ones() as u8,
            self.hands[2].count_ones() as u8,
            self.hands[3].count_ones() as u8,
        ]
    }

    pub(crate) fn format_bpn_masked(&self, mask: CardSet) -> String {
        format!(
            "{{ {} {} {} {} }}",
            self.get(Player::W).to_bpn_masked(mask),
            self.get(Player::N).to_bpn_masked(mask),
            self.get(Player::E).to_bpn_masked(mask),
            self.get(Player::S).to_bpn_masked(mask)
        )
    }
    
    pub(crate) fn add(&mut self, p: Player, card: Card) {
        let p = p.index();
        self.hands[p] |= CardSet::from(card).mask;
    }

    pub(crate) fn format_as_bpn(&self) -> String {
        format!("{} {} {} {}",
            &CardSet::from(self.hands[0]).to_bpn(),
            &CardSet::from(self.hands[1]).to_bpn(),
            &CardSet::from(self.hands[2]).to_bpn(),
            &CardSet::from(self.hands[3]).to_bpn(),
        )
    }
}

impl BitAnd<CardSet> for &Deal {
    type Output = Deal;

    fn bitand(self, rhs: CardSet) -> Self::Output {
        Deal {
            hands: self.hands & rhs.splat(),
        }
    }
}

impl BitAnd<&Deal> for &Deal {
    type Output = Deal;

    fn bitand(self, rhs: &Deal) -> Self::Output {
        Deal {
            hands: self.hands & rhs.hands,
        }
    }
}

impl std::fmt::Debug for Deal {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str("{ ")?;
        f.write_str(&CardSet::from(self.hands[0]).to_bpn())?;
        f.write_str(" ")?;
        f.write_str(&CardSet::from(self.hands[1]).to_bpn())?;
        f.write_str(" ")?;
        f.write_str(&CardSet::from(self.hands[2]).to_bpn())?;
        f.write_str(" ")?;
        f.write_str(&CardSet::from(self.hands[3]).to_bpn())?;
        f.write_str(" ")?;
        f.write_str("}")?;
        Ok(())
    }
}

impl TryFrom<ByPlayer<&str>> for Deal {
    type Error = String;

    fn try_from(value: ByPlayer<&str>) -> Result<Self, Self::Error> {
        let hands = [
            CardSet::try_from_bpn(value.w)?.mask,
            CardSet::try_from_bpn(value.n)?.mask,
            CardSet::try_from_bpn(value.e)?.mask,
            CardSet::try_from_bpn(value.s)?.mask,
        ];
        Ok(Self {
            hands: hands.into(),
        })
    }
}

impl From<Deal> for [u64; 4] {
    fn from(val: Deal) -> Self {
        val.hands.into()
    }
}

impl From<[u64; 4]> for Deal {
    fn from(value: [u64; 4]) -> Self {
        Self {
            hands: value.into(),
        }
    }
}

pub(crate) struct CardMap {
    from_absolute: [Card; 52],
    to_absolute: [Card; 52],
}
impl CardMap {
    pub(crate) fn promote(&mut self, missing_cards: CardSet) {
        // mis:     *
        //  id: AKQJT98765432
        // fro: AKQJTT9876543
        //  to: AKQJ987654322

        for item in missing_cards.iter() {
            let missing_card = item.card();
            let offset = missing_card.suit().index() * 13;
            for j in (1..=missing_card.value().index()).rev() {
                self.to_absolute[j + offset] = self.to_absolute[j + offset - 1];
            }
        }
        for item in missing_cards.rev_iter() {
            let missing_card = item.card();
            let offset = missing_card.suit().index() * 13;
            for j in 0..missing_card.value().index() {
                self.from_absolute[j + offset] = self.from_absolute[j + offset + 1];
            }
        }
    }

    pub(crate) fn to_absolute(&self, card: Card) -> Card {
        self.to_absolute[card.index()]
    }
}

impl Default for CardMap {
    fn default() -> Self {
        Self {
            from_absolute: Card::ALL,
            to_absolute: Card::ALL,
        }
    }
}

impl std::fmt::Debug for CardMap {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let from_absolute: Vec<_> = self
            .from_absolute
            .iter()
            .rev()
            .map(|c| c.symbol().to_string())
            .collect();
        let to_absolute: Vec<_> = self
            .to_absolute
            .iter()
            .rev()
            .map(|c| c.symbol().to_string())
            .collect();

        f.debug_struct("CardMap")
            .field("from_absolute", &from_absolute.join(""))
            .field("  to_absolute", &to_absolute.join(""))
            .finish()
    }
}

#[cfg(test)]
mod tests {
    use crate::{ByPlayer, BySuit, Card, CardMap, CardSet, Deal, Player, PlayerMap, Suit};

    #[test]
    fn card_set_from_string() {
        let hand = CardSet::try_from_bpn(".63.AKQ987.A9732").unwrap();
        assert_eq!(
            "CardSet { mask: \"0000000000000000000000000001001000011100111000000001000010100011\" }",
            format!("{hand:?}")
        );
        assert_eq!(".63.AKQ987.A9732", hand.to_bpn());
    }

    #[test]
    fn card_set_iterators() {
        let hand = CardSet::try_from_bpn(".63.AKQ987.A9732").unwrap();
        let fwd: Vec<_> = hand.iter().map(|c| c.card().symbol().to_string()).collect();
        assert_eq!("🃒🃓🃗🃙🃑🃇🃈🃉🃍🃎🃁🂳🂶", fwd.join(""));
        let rev: Vec<_> = hand
            .rev_iter()
            .map(|c| c.card().symbol().to_string())
            .collect();
        assert_eq!("🂶🂳🃁🃎🃍🃉🃈🃇🃑🃙🃗🃓🃒", rev.join(""));
    }

    #[test]
    fn bermuda_1983() {
        // https://www.nytimes.com/1983/10/16/arts/bridge-bermuda-bowl-drama.html
        let deal = Deal::try_from(ByPlayer::<&str> {
            w: "854.Q873.Q984.65",
            n: "KQ32.T6.T72.AJ93",
            e: "9.AJ542.J653.T87",
            s: "AJT76.K9.AK.KQ42",
        })
        .unwrap();
        assert_eq!(
            "{ 854.Q873.Q984.65 KQ32.T6.T72.AJ93 9.AJ542.J653.T87 AJT76.K9.AK.KQ42 }",
            format!("{deal:?}")
        );
    }

    #[test]
    fn swap_suits() {
        let deal = Deal::try_from(ByPlayer::<&str> {
            w: "854.Q873.Q984.65",
            n: "KQ32.T6.T72.AJ93",
            e: "9.AJ542.J653.T87",
            s: "AJT76.K9.AK.KQ42",
        })
        .unwrap();
        let mut map = CardMap::default();
        assert_eq!("CardMap { from_absolute: \"🂡🂮🂭🂫🂪🂩🂨🂧🂦🂥🂤🂣🂢🂱🂾🂽🂻🂺🂹🂸🂷🂶🂵🂴🂳🂲🃁🃎🃍🃋🃊🃉🃈🃇🃆🃅🃄🃃🃂🃑🃞🃝🃛🃚🃙🃘🃗🃖🃕🃔🃓🃒\",   to_absolute: \"🂡🂮🂭🂫🂪🂩🂨🂧🂦🂥🂤🂣🂢🂱🂾🂽🂻🂺🂹🂸🂷🂶🂵🂴🂳🂲🃁🃎🃍🃋🃊🃉🃈🃇🃆🃅🃄🃃🃂🃑🃞🃝🃛🃚🃙🃘🃗🃖🃕🃔🃓🃒\" }", 
        &format!("{map:?}"));
        assert_eq!(
            "{ 65.Q873.Q984.854 AJ93.T6.T72.KQ32 T87.AJ542.J653.9 KQ42.K9.AK.AJT76 }",
            format!("{:?}", deal.swap_suits(Suit::C, Suit::S, &mut map))
        );
        assert_eq!("CardMap { from_absolute: \"🃑🃞🃝🃛🃚🃙🃘🃗🃖🃕🃔🃓🃒🂱🂾🂽🂻🂺🂹🂸🂷🂶🂵🂴🂳🂲🃁🃎🃍🃋🃊🃉🃈🃇🃆🃅🃄🃃🃂🂡🂮🂭🂫🂪🂩🂨🂧🂦🂥🂤🂣🂢\",   to_absolute: \"🃑🃞🃝🃛🃚🃙🃘🃗🃖🃕🃔🃓🃒🂱🂾🂽🂻🂺🂹🂸🂷🂶🂵🂴🂳🂲🃁🃎🃍🃋🃊🃉🃈🃇🃆🃅🃄🃃🃂🂡🂮🂭🂫🂪🂩🂨🂧🂦🂥🂤🂣🂢\" }", 
        &format!("{map:?}"));
    }

    #[test]
    fn rotate_to_west() {
        let deal = Deal::try_from(ByPlayer::<&str> {
            w: "854.Q873.Q984.65",
            n: "KQ32.T6.T72.AJ93",
            e: "9.AJ542.J653.T87",
            s: "AJT76.K9.AK.KQ42",
        })
        .unwrap();
        let mut map = PlayerMap::default();
        assert_eq!(
            "PlayerMap { from_absolute: [W, N, E, S], to_absolute: [W, N, E, S] }",
            &format!("{map:?}")
        );
        assert_eq!(
            "{ AJT76.K9.AK.KQ42 854.Q873.Q984.65 KQ32.T6.T72.AJ93 9.AJ542.J653.T87 }",
            format!("{:?}", deal.rotate_to_west(Player::S, Some(&mut map)))
        );
        assert_eq!(
            "PlayerMap { from_absolute: [N, E, S, W], to_absolute: [S, W, N, E] }",
            &format!("{map:?}")
        );
    }

    #[test]
    fn promote_all_deal2() {
        let mut map = CardMap::default();
        let deal2 = Deal::try_from(ByPlayer::<&str> {
            w: ".Q8..",
            n: ".T6..",
            e: ".AJ..",
            s: "6.K..",
        })
        .unwrap();

        assert_eq!(
            "{ .Q9.. .T8.. .AJ.. A.K.. }",
            format!("{:?}", deal2.promote_all(&mut map))
        );
        assert_eq!(
            concat!(
                "CardMap { from_absolute: \"",
                "🂡🂡🂡🂡🂡🂡🂡🂡🂡🂮🂮🂮🂮🂱🂾🂽🂻🂺🂹🂹🂸🂸🂷🂷🂷🂷🃁🃁🃁🃁🃁🃁🃁🃁🃁🃁🃁🃁🃁🃑🃑🃑🃑🃑🃑🃑🃑🃑🃑🃑🃑🃑",
                "\",   to_absolute: \"",
                "🂦🂢🂢🂢🂢🂢🂢🂢🂢🂢🂢🂢🂢🂱🂾🂽🂻🂺🂸🂶🂲🂲🂲🂲🂲🂲🃂🃂🃂🃂🃂🃂🃂🃂🃂🃂🃂🃂🃂🃒🃒🃒🃒🃒🃒🃒🃒🃒🃒🃒🃒🃒",
                "\" }"
            ),
            format!("{map:?}")
        );
    }

    #[test]
    fn promote_all_deal3() {
        let mut map = CardMap::default();
        let deal3 = Deal::try_from(ByPlayer::<&str> {
            n: ".T6.2.",
            w: ".Q87..",
            e: ".AJ5..",
            s: "6.K9..",
        })
        .unwrap();
        assert_eq!(
            "{ .Q87.. .T6.A. .AJ5.. A.K9.. }",
            format!("{:?}", deal3.promote_all(&mut map))
        );
        assert_eq!(
            concat!(
                "CardMap { from_absolute: \"",
                "🂡🂡🂡🂡🂡🂡🂡🂡🂡🂮🂮🂮🂮🂱🂾🂽🂻🂺🂹🂸🂷🂶🂵🂴🂴🂴🃁🃁🃁🃁🃁🃁🃁🃁🃁🃁🃁🃁🃁🃑🃑🃑🃑🃑🃑🃑🃑🃑🃑🃑🃑🃑",
                "\",   to_absolute: \"",
                "🂦🂢🂢🂢🂢🂢🂢🂢🂢🂢🂢🂢🂢🂱🂾🂽🂻🂺🂹🂸🂷🂶🂵🂲🂲🂲🃂🃂🃂🃂🃂🃂🃂🃂🃂🃂🃂🃂🃂🃒🃒🃒🃒🃒🃒🃒🃒🃒🃒🃒🃒🃒",
                "\" }"
            ),
            format!("{map:?}")
        );
    }

    #[test]
    fn promote_all_deal6() {
        let deal6 = Deal::try_from(ByPlayer::<&str> {
            w: ".Q873.84.",
            n: "2.T6.2.93",
            e: ".AJ54.53.",
            s: "76.K9..Q4",
        })
        .unwrap();
        let mut map = CardMap::default();
        assert_eq!(
            "{ .Q873.AQ. Q.T6.T.KJ .AJ54.KJ. AK.K9..AQ }",
            format!("{:?}", deal6.promote_all(&mut map))
        );
        assert_eq!(
            concat!(
                "CardMap { from_absolute: \"",
                "🂡🂡🂡🂡🂡🂡🂡🂡🂮🂭🂭🂭🂭🂱🂾🂽🂻🂺🂹🂸🂷🂶🂵🂴🂳🂲🃁🃁🃁🃁🃁🃁🃁🃎🃎🃎🃍🃋🃊🃑🃑🃑🃞🃞🃞🃝🃝🃝🃝🃝🃛🃚",
                "\",   to_absolute: \"",
                "🂧🂦🂢🂢🂢🂢🂢🂢🂢🂢🂢🂢🂢🂱🂾🂽🂻🂺🂹🂸🂷🂶🂵🂴🂳🂲🃈🃅🃄🃃🃂🃂🃂🃂🃂🃂🃂🃂🃂🃝🃙🃔🃓🃒🃒🃒🃒🃒🃒🃒🃒🃒",
                "\" }"
            ),
            format!("{map:?}")
        );
    }

    #[test]
    fn try_from_pbn() {
        let deal = Deal::try_from_pbn(
            "QJ6.K652.J85.T98 873.J97.AT764.Q4 K5.T83.KQ9.A7652 AT942.AQ4.32.KJ3",
        )
        .unwrap();
        assert_eq!(
            "{ QJ6.K652.J85.T98 873.J97.AT764.Q4 K5.T83.KQ9.A7652 AT942.AQ4.32.KJ3 }",
            format!("{deal:?}")
        );

        let deal = Deal::try_from_pbn(
            "N:QJ6.K652.J85.T98 873.J97.AT764.Q4 K5.T83.KQ9.A7652 AT942.AQ4.32.KJ3",
        )
        .unwrap();
        assert_eq!(
            "{ AT942.AQ4.32.KJ3 QJ6.K652.J85.T98 873.J97.AT764.Q4 K5.T83.KQ9.A7652 }",
            format!("{deal:?}")
        );

        let deal = Deal::try_from_pbn(
            "E:QJ6.K652.J85.T98 873.J97.AT764.Q4 K5.T83.KQ9.A7652 AT942.AQ4.32.KJ3",
        )
        .unwrap();
        assert_eq!(
            "{ K5.T83.KQ9.A7652 AT942.AQ4.32.KJ3 QJ6.K652.J85.T98 873.J97.AT764.Q4 }",
            format!("{deal:?}")
        );

        let deal = Deal::try_from_pbn(
            "S:QJ6.K652.J85.T98 873.J97.AT764.Q4 K5.T83.KQ9.A7652 AT942.AQ4.32.KJ3",
        )
        .unwrap();
        assert_eq!(
            "{ 873.J97.AT764.Q4 K5.T83.KQ9.A7652 AT942.AQ4.32.KJ3 QJ6.K652.J85.T98 }",
            format!("{deal:?}")
        );

        let deal = Deal::try_from_pbn(
            "W:QJ6.K652.J85.T98 873.J97.AT764.Q4 K5.T83.KQ9.A7652 AT942.AQ4.32.KJ3",
        )
        .unwrap();
        assert_eq!(
            "{ QJ6.K652.J85.T98 873.J97.AT764.Q4 K5.T83.KQ9.A7652 AT942.AQ4.32.KJ3 }",
            format!("{deal:?}")
        );
    }

    #[test]
    fn high_card_suit_seq() {
        let deal = Deal::try_from_pbn(
            "N:QJ6.K652.J85.T98 873.J97.AT764.Q4 K5.T83.KQ9.A7652 AT942.AQ4.32.KJ3",
        )
        .unwrap();

        let high_cards = ByPlayer::par_new(|player| {
            BySuit::par_new(|suit| deal.get(player).high_card_suit_seq(suit.into()))
        });

        assert_eq!(
            "ByPlayer { ".to_owned()
                + "w: BySuit { s: (1, CardSet { mask: \"0001000000000000000000000000000000000000000000000000000000000000\" }), h: (1, CardSet { mask: \"0000000000000000000100000000000000000000000000000000000000000000\" }), d: (0, CardSet { mask: \"0000000000000000000000000000000000000000000000000000000000000000\" }), c: (0, CardSet { mask: \"0000000000000000000000000000000000000000000000000000000000000000\" }) }, "
                + "n: BySuit { s: (0, CardSet { mask: \"0000000000000000000000000000000000000000000000000000000000000000\" }), h: (0, CardSet { mask: \"0000000000000000000000000000000000000000000000000000000000000000\" }), d: (0, CardSet { mask: \"0000000000000000000000000000000000000000000000000000000000000000\" }), c: (0, CardSet { mask: \"0000000000000000000000000000000000000000000000000000000000000000\" }) }, "
                + "e: BySuit { s: (0, CardSet { mask: \"0000000000000000000000000000000000000000000000000000000000000000\" }), h: (0, CardSet { mask: \"0000000000000000000000000000000000000000000000000000000000000000\" }), d: (1, CardSet { mask: \"0000000000000000000000000000000000010000000000000000000000000000\" }), c: (0, CardSet { mask: \"0000000000000000000000000000000000000000000000000000000000000000\" }) }, "
                + "s: BySuit { s: (0, CardSet { mask: \"0000000000000000000000000000000000000000000000000000000000000000\" }), h: (0, CardSet { mask: \"0000000000000000000000000000000000000000000000000000000000000000\" }), d: (0, CardSet { mask: \"0000000000000000000000000000000000000000000000000000000000000000\" }), c: (1, CardSet { mask: \"0000000000000000000000000000000000000000000000000001000000000000\" }) } }",
            format!("{high_cards:?}")
        );
    }

    #[test]
    fn card_map_promote() {
        let mut map = CardMap::default();
        assert_eq!(
            concat!(
                "CardMap { from_absolute: \"",
                "🂡🂮🂭🂫🂪🂩🂨🂧🂦🂥🂤🂣🂢🂱🂾🂽🂻🂺🂹🂸🂷🂶🂵🂴🂳🂲🃁🃎🃍🃋🃊🃉🃈🃇🃆🃅🃄🃃🃂🃑🃞🃝🃛🃚🃙🃘🃗🃖🃕🃔🃓🃒",
                "\",   to_absolute: \"",
                "🂡🂮🂭🂫🂪🂩🂨🂧🂦🂥🂤🂣🂢🂱🂾🂽🂻🂺🂹🂸🂷🂶🂵🂴🂳🂲🃁🃎🃍🃋🃊🃉🃈🃇🃆🃅🃄🃃🃂🃑🃞🃝🃛🃚🃙🃘🃗🃖🃕🃔🃓🃒",
                "\" }"
            ),
            format!("{map:?}")
        );

        map.promote([Card::CA, Card::DK, Card::HQ, Card::HT].as_ref().into());
        assert_eq!(
            concat!(
                "CardMap { from_absolute: \"",
                "🂡🂮🂭🂫🂪🂩🂨🂧🂦🂥🂤🂣🂢🂱🂾🂽🂽🂻🂻🂺🂹🂸🂷🂶🂵🂴🃁🃎🃎🃍🃋🃊🃉🃈🃇🃆🃅🃄🃃🃑🃑🃞🃝🃛🃚🃙🃘🃗🃖🃕🃔🃓",
                "\",   to_absolute: \"",
                "🂡🂮🂭🂫🂪🂩🂨🂧🂦🂥🂤🂣🂢🂱🂾🂻🂹🂸🂷🂶🂵🂴🂳🂲🂲🂲🃁🃍🃋🃊🃉🃈🃇🃆🃅🃄🃃🃂🃂🃞🃝🃛🃚🃙🃘🃗🃖🃕🃔🃓🃒🃒",
                "\" }"
            ),
            format!("{map:?}")
        );
    }
}
