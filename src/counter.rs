use crate::{CardMap, Contract, Deal, PlayOfCards, PlayState, PlayedCardsNT, PlayedCardsT, Suit};

#[must_use]
pub fn count_nodes(contract: &Contract, deal: &Deal, depth: u8) -> u64 {
    fn count_nodes_impl<PC: PlayOfCards>(result: &mut u64, state: &mut PlayState<PC>, depth: u8) {
        if depth == 0 || state.deal.is_empty() {
            return;
        }

        let depth = depth - 1;
        for c in state.plays().iter() {
            *result += 1;
            let (_win, undo) = state.apply_mut(&c);
            count_nodes_impl(result, state, depth);
            state.undo(undo);
        }
    }

    let mut result = 0u64;
    let player = contract.declarer.next();
    if let Some(trumps) = contract.trumps {
        let deal = deal.swap_suits(Suit::S, trumps, &mut CardMap::default());
        let mut state = PlayState::<PlayedCardsT>::new(&deal, player);
        count_nodes_impl(&mut result, &mut state, depth);
    } else {
        let mut state = PlayState::<PlayedCardsNT>::new(deal, player);
        count_nodes_impl(&mut result, &mut state, depth);
    }
    result
}

#[cfg(test)]
mod tests {
    use crate::{counter::count_nodes, ByPlayer, Contract, Deal, Player};

    #[test]
    fn bermuda_1983() {
        // https://www.nytimes.com/1983/10/16/arts/bridge-bermuda-bowl-drama.html
        let deal = Deal::try_from(ByPlayer::<&str> {
            n: "KQ32.T6.T72.AJ93",
            w: "854.Q873.Q984.65",
            e: "9.AJ542.J653.T87",
            s: "AJT76.K9.AK.KQ42",
        })
        .unwrap();
        let contract = Contract {
            declarer: Player::S,
            trumps: Some(crate::Suit::S),
            total_tricks: 12,
        };
        // west initially has 13 moves
        assert_eq!(13, count_nodes(&contract, &deal, 1));
        // no idea if these are right but hopefully they will converge to the correct number
        assert_eq!(53, count_nodes(&contract, &deal, 2));
        assert_eq!(177, count_nodes(&contract, &deal, 3));
        assert_eq!(509, count_nodes(&contract, &deal, 4));
        assert_eq!(4493, count_nodes(&contract, &deal, 5));
        assert_eq!(16261, count_nodes(&contract, &deal, 6));
        assert_eq!(53682, count_nodes(&contract, &deal, 7));
        assert_eq!(153714, count_nodes(&contract, &deal, 8));

        #[cfg(not(debug_assertions))]
        assert_eq!(1254066, count_nodes(&contract, &deal, 9));
        #[cfg(not(debug_assertions))]
        assert_eq!(4478190, count_nodes(&contract, &deal, 10));
    }

    #[test]
    fn bermuda_1983_nt() {
        let deal = Deal::try_from(ByPlayer::<&str> {
            n: "KQ32.T6.T72.AJ93",
            w: "854.Q873.Q984.65",
            e: "9.AJ542.J653.T87",
            s: "AJT76.K9.AK.KQ42",
        })
        .unwrap();
        let contract = Contract {
            declarer: Player::S,
            trumps: None,
            total_tricks: 12,
        };
        // west initially has 13 moves
        assert_eq!(13, count_nodes(&contract, &deal, 1));
        // no idea if these are right but hopefully they will converge to the correct number
        assert_eq!(53, count_nodes(&contract, &deal, 2));
        assert_eq!(177, count_nodes(&contract, &deal, 3));
        assert_eq!(509, count_nodes(&contract, &deal, 4));
        assert_eq!(4493, count_nodes(&contract, &deal, 5));
        assert_eq!(16261, count_nodes(&contract, &deal, 6));
        assert_eq!(53682, count_nodes(&contract, &deal, 7));
        assert_eq!(153714, count_nodes(&contract, &deal, 8));

        #[cfg(not(debug_assertions))]
        assert_eq!(1254066, count_nodes(&contract, &deal, 9));
        #[cfg(not(debug_assertions))]
        assert_eq!(4478190, count_nodes(&contract, &deal, 10));
    }
}
