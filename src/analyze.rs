use std::{
    fmt::{Debug, Write},
    str::FromStr,
};

use rayon::prelude::*;

use crate::{
    search::{search, Config, InitialPosition},
    ByPlayer, Deal, Player, Suit,
};

#[derive(Clone, Copy)]
pub struct Strain {
    pub trumps: Option<Suit>,
}

impl Strain {
    const ALL: [Strain; 5] = [
        Strain {
            trumps: Some(Suit::C),
        },
        Strain {
            trumps: Some(Suit::D),
        },
        Strain {
            trumps: Some(Suit::H),
        },
        Strain {
            trumps: Some(Suit::S),
        },
        Strain { trumps: None },
    ];
}

impl FromStr for Strain {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match &s.to_uppercase()[..1] {
            "S" => Ok(Strain {
                trumps: Some(Suit::S),
            }),
            "H" => Ok(Strain {
                trumps: Some(Suit::H),
            }),
            "D" => Ok(Strain {
                trumps: Some(Suit::D),
            }),
            "C" => Ok(Strain {
                trumps: Some(Suit::C),
            }),
            "N" => Ok(Strain { trumps: None }),
            _ => Err(format!("bad strain: {s}")),
        }
    }
}

impl Debug for Strain {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.trumps {
            Some(trumps) => f.write_str(trumps.symbol()),
            None => f.write_char('N'),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct ByStrain<T> {
    pub c: T,
    pub d: T,
    pub h: T,
    pub s: T,
    pub n: T,
}

impl<T: Send + Copy> ByStrain<T> {
    pub(crate) fn par_new<F>(f: F) -> Self
    where
        F: Fn(Strain) -> T + Sync + Send,
    {
        let strains: Vec<_> = Strain::ALL.par_iter().map(|s| f(*s)).collect();
        Self {
            c: strains[0],
            d: strains[1],
            h: strains[2],
            s: strains[3],
            n: strains[4],
        }
    }
}

#[must_use]
pub fn analyze_all(deal: &Deal, config: &Config) -> ByPlayer<ByStrain<u8>> {
    ByPlayer::par_new(|player| ByStrain::par_new(|strain| analyze(deal, player, &strain, config)))
}

#[must_use]
pub fn analyze_strain(deal: &Deal, strain: &Strain, config: &Config) -> ByPlayer<u8> {
    ByPlayer::par_new(|player| analyze(deal, player, strain, config))
}

#[must_use]
pub fn analyze_player(deal: &Deal, player: Player, config: &Config) -> ByStrain<u8> {
    ByStrain::par_new(|strain| analyze(deal, player, &strain, config))
}

#[must_use]
pub fn analyze(deal: &Deal, player: Player, strain: &Strain, config: &Config) -> u8 {
    search(
        &InitialPosition {
            deal: deal.clone(),
            trumps: strain.trumps,
            declarer: player,
            next: None,
        },
        config,
    )
}
