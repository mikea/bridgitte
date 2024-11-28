# Bridgitte

Bridgitte is a contract bridge double dummy solver written in Rust.
It was written for (personal) education to explore representing
card sets as 64-bit masks.

## Installation

## Usage

## Design Notes

### Data Structures

- Card, Suit, CardValue are mainly used for IO operations
- CardSet is the main data structure used
  in search. Set of cards represented as a single u64
- Deal - simd optimized group of 4 card sets.
- PlayState represents the search node.

### Search

Search algorithm is straightforward Zero-Window Partitioned Search.

Important details:

- NS is always a declaring pair.
- Trumps (if any) are always Spades.
- All cards are promoted after every trick
- Cards are not removed from the deck until the full trick is played.

## Performance

As of Nov 2024 bridgitte is ~100 time slower than state of the art.
It takes seconds to evaluate "fast" positions and minutes for
"slow" ones.

The following algorithm improvements will probably speed things up:

- quick tricks is applied only at the beginning of the round
- transposition table lookup is linear
- there's no move ordering
