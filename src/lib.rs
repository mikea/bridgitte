//! Bridgitte is a contract bridge double dummy solver.
#![feature(portable_simd)]
mod cards;
use rayon::prelude::*;
use std::str::FromStr;

pub use cards::*;
mod play;
pub use play::*;
pub mod analyze;
mod bits;
pub mod counter;
pub mod minmax;
pub mod observers;
pub mod search;
pub mod trans_table;
mod ui;

#[derive(Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
#[repr(u8)]
pub enum Player {
    W = 0,
    N = 1,
    E = 2,
    S = 3,
}
impl Player {
    const ALL: [Player; 4] = [Player::W, Player::N, Player::E, Player::S];

    #[must_use]
    pub(crate) fn next(self) -> Player {
        let idx = self as u8;
        let idx = idx.wrapping_add(1) & 3;
        unsafe { std::mem::transmute(idx) }
    }

    fn index(self) -> usize {
        (self as u8) as usize
    }

    fn symbol(self) -> &'static str {
        match self {
            Self::W => "W",
            Self::N => "N",
            Self::E => "E",
            Self::S => "S",
        }
    }
}

impl std::fmt::Debug for Player {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(self.symbol())
    }
}

impl TryFrom<char> for Player {
    type Error = String;

    fn try_from(value: char) -> Result<Self, Self::Error> {
        match value {
            'N' => Ok(Self::N),
            'E' => Ok(Self::E),
            'W' => Ok(Self::W),
            'S' => Ok(Self::S),
            _ => Err(format!("bad player: {value}")),
        }
    }
}

impl FromStr for Player {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match &s.to_uppercase()[..1] {
            "N" => Ok(Self::N),
            "E" => Ok(Self::E),
            "W" => Ok(Self::W),
            "S" => Ok(Self::S),
            _ => Err(format!("bad player: {s}")),
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct ByPlayer<T> {
    pub w: T,
    pub n: T,
    pub e: T,
    pub s: T,
}

impl<T: Send + Copy> ByPlayer<T> {
    pub(crate) fn par_new<F>(f: F) -> Self
    where
        F: Fn(Player) -> T + Sync + Send,
    {
        let players: Vec<_> = Player::ALL.par_iter().map(|s| f(*s)).collect();
        Self {
            w: players[0],
            n: players[1],
            e: players[2],
            s: players[3],
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub struct BySuit<T> {
    pub s: T,
    pub h: T,
    pub d: T,
    pub c: T,
}

impl<T: Send + Copy> BySuit<T> {
    pub fn par_new<F>(f: F) -> Self
    where
        F: Fn(Suit) -> T + Sync + Send,
    {
        let suits: Vec<_> = Suit::ALL.par_iter().map(|s| f(*s)).collect();
        Self {
            c: suits[0],
            d: suits[1],
            h: suits[2],
            s: suits[3],
        }
    }
}

#[derive(Debug)]
pub(crate) struct PlayerMap {
    pub(crate) from_absolute: [Player; 4],
    pub(crate) to_absolute: [Player; 4],
}
impl PlayerMap {
    fn map_to_absolute(&self, next: Player) -> Player {
        self.to_absolute[next.index()]
    }

    fn map_from_absolute(&self, next: Player) -> Player {
        self.from_absolute[next.index()]
    }
}

impl Default for PlayerMap {
    fn default() -> Self {
        Self {
            from_absolute: Player::ALL,
            to_absolute: Player::ALL,
        }
    }
}

#[derive(Clone, Copy, Debug)]
#[repr(u8)]
pub enum Pair {
    // WE
    WE = 0,
    // NS
    NS = 1,
}

impl Pair {
    #[must_use]
    pub(crate) fn target_inc(self) -> u8 {
        self as u8
    }

    fn opposite(self) -> Pair {
        match self {
            Pair::WE => Self::NS,
            Pair::NS => Self::WE,
        }
    }

    fn filter<T: Copy>(self, arr: [T; 4]) -> [T; 2] {
        match self {
            Pair::WE => [arr[0], arr[2]],
            Pair::NS => [arr[1], arr[3]],
        }
    }
}

impl From<Player> for Pair {
    fn from(p: Player) -> Self {
        match p {
            Player::N | Player::S => Self::NS,
            Player::E | Player::W => Self::WE,
        }
    }
}
