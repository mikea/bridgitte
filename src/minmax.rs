use crate::{
    CardMap, Deal, Pair, PlayOfCards, PlayState, PlayedCardsNT, PlayedCardsT, Player, Suit, Trick,
};

pub(crate) trait Lattice<T> {
    fn bottom() -> T;
    fn join(t1: T, t2: T) -> T;
}

pub(crate) struct Max {}
impl Lattice<u8> for Max {
    fn bottom() -> u8 {
        u8::MIN
    }

    fn join(t1: u8, t2: u8) -> u8 {
        t1.max(t2)
    }
}

pub struct Min {}
impl Lattice<u8> for Min {
    fn bottom() -> u8 {
        u8::MAX
    }

    fn join(t1: u8, t2: u8) -> u8 {
        t1.min(t2)
    }
}

struct Search<PC: PlayOfCards> {
    state: PlayState<PC>,
}

impl<PC: PlayOfCards> Search<PC> {
    fn visit(&mut self) -> u8 {
        match Pair::from(self.state.next) {
            Pair::NS => self.visit_impl::<Max>(),
            Pair::WE => self.visit_impl::<Min>(),
        }
    }

    fn visit_impl<L: Lattice<u8>>(&mut self) -> u8 {
        if self.state.deal.is_empty() {
            return 0;
        }

        let mut result = L::bottom();
        for c in self.state.plays().iter() {
            let z = {
                let (trick, undo) = self.state.apply_mut(&c);
                let z = self.visit();
                self.state.undo(undo);
                z + trick.as_ref().map(Trick::target_inc).unwrap_or_default()
            };
            result = L::join(result, z);
        }
        result
    }
}

#[must_use]
pub fn minmax(deal: &Deal, trumps: Option<Suit>, player: Player) -> u8 {
    if let Some(trumps) = trumps {
        let mut cards = CardMap::default();
        let deal = deal.swap_suits(Suit::S, trumps, &mut cards);
        let state = PlayState::<PlayedCardsT>::new(&deal, player);
        Search { state }.visit()
    } else {
        let state = PlayState::<PlayedCardsNT>::new(deal, player);
        Search { state }.visit()
    }
}

#[cfg(test)]
mod tests {
    use crate::{minmax::minmax, ByPlayer, Deal, Player, Suit};

    #[test]
    fn small_deal_2() {
        let deal = Deal::try_from(ByPlayer::<&str> {
            w: "K6...",
            n: "AQ...",
            e: "T9...",
            s: "72...",
        })
        .unwrap();
        let tricks = minmax(&deal, None, Player::W);
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
        let tricks = minmax(&deal, None, Player::W);
        assert_eq!(2, tricks);
    }

    #[test]
    fn small_deal_4() {
        let deal = Deal::try_from_pbn("A.KQ9.. .J.K.KQ K.T8..J .A.A.AT").unwrap();
        let tricks = minmax(&deal, Some(Suit::S), Player::S);
        assert_eq!(1, tricks);
    }

    #[test]
    fn small_deal_5() {
        let deal = Deal::try_from_pbn(".KQT.. .J..KQ A.9..J .A..AT").unwrap();
        let tricks = minmax(&deal, Some(Suit::S), Player::W);
        assert_eq!(2, tricks);
    }

    #[test]
    fn bermuda_1983() {
        // walk bermuda hand backwards
        let trumps = Some(Suit::S);

        // north
        let deal2 = Deal::try_from(ByPlayer::<&str> {
            n: ".T6..",
            w: ".Q8..",
            e: ".AJ..",
            s: "6.K..",
        })
        .unwrap();
        assert_eq!(1, minmax(&deal2, trumps, Player::N));

        // north
        let deal3 = Deal::try_from(ByPlayer::<&str> {
            n: ".T6.2.",
            w: ".Q87..",
            e: ".AJ5..",
            s: "6.K9..",
        })
        .unwrap();
        assert_eq!(2, minmax(&deal3, trumps, Player::N));

        // south
        let deal4 = Deal::try_from(ByPlayer::<&str> {
            n: ".T6.2.9",
            w: ".Q873..",
            e: ".AJ54..",
            s: "6.K9..4",
        })
        .unwrap();
        assert_eq!(3, minmax(&deal4, trumps, Player::S));

        // too slow for debug builds
        #[cfg(not(debug_assertions))]
        {
            // south
            let deal5 = Deal::try_from(ByPlayer::<&str> {
                n: ".T6.2.93",
                w: ".Q873.8.",
                e: ".AJ54.5.",
                s: "6.K9..Q4",
            })
            .unwrap();
            assert_eq!(4, minmax(&deal5, trumps, Player::S));
        }

        // these hands are beyond minmax in release build
        /*
        // south
        let deal6 = Deal::try_from(ByPlayer::<&str> {
            n: "2.T6.2.93",
            w: ".Q873.84.",
            e: ".AJ54.53.",
            s: "76.K9..Q4",
        })
        .unwrap();

        // south
        let deal7 = Deal::try_from(ByPlayer::<&str> {
            n: "32.T6.2.93",
            w: "4.Q873.84.",
            e: ".AJ54.53.7",
            s: "T76.K9..Q4",
        })
        .unwrap();

        // south
        let deal8 = Deal::try_from(ByPlayer::<&str> {
            n: "32.T6.72.93",
            w: "4.Q873.984.",
            e: ".AJ54.653.7",
            s: "T76.K9.K.Q4",
        })
        .unwrap();

        // south
        let deal9 = Deal::try_from(ByPlayer::<&str> {
            n: "32.T6.T72.93",
            w: "4.Q873.Q984.",
            e: ".AJ54.J653.7",
            s: "T76.K9.AK.Q4",
        })
        .unwrap();

        // north
        let deal10 = Deal::try_from(ByPlayer::<&str> {
            n: "32.T6.T72.J93",
            w: "4.Q873.Q984.6",
            e: ".AJ54.J653.87",
            s: "T76.K9.AK.KQ4",
        })
        .unwrap();

        // north
        let deal11 = Deal::try_from(ByPlayer::<&str> {
            n: "32.T6.T72.AJ93",
            w: "4.Q873.Q984.65",
            e: ".AJ54.J653.T87",
            s: "T76.K9.AK.KQ42",
        })
        .unwrap();
        let deal12 = Deal::try_from(ByPlayer::<&str> {
            n: "Q32.T6.T72.AJ93",
            w: "54.Q873.Q984.65",
            e: ".AJ542.J653.T87",
            s: "JT76.K9.AK.KQ42",
        })
        .unwrap();
        let deal13 = Deal::try_from(ByPlayer::<&str> {
            n: "KQ32.T6.T72.AJ93",
            w: "854.Q873.Q984.65",
            e: "9.AJ542.J653.T87",
            s: "AJT76.K9.AK.KQ42",
        })
        .unwrap();
        */
    }
}
