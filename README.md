# Bridgitte

Bridgitte is a contract bridge double dummy solver written in Rust.

## Design

### Data Structures

- Card, Suit, CardValue are mainly
  used for IO operations
- CardSet is the main data structure used
  in search. Set of cards represented
  as a single u64
- Deal - simd optimized group of 4 card sets. 
- PlayState represents the search node


### Search

Important assumptions:
- NS is always a declaring pair
  Higher level code is expected
  to rotate hands. 
- Trumps are always Spades. 

