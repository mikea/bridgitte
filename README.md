# Bridgitte

Bridgitte is a contract bridge double dummy solver written in Rust.
It was written for (personal) education to explore representing
card sets as 64-bit masks.

## Installation

```
cargo +nightly install bridgitte
```

## Usage

You can use bridgitte to fully analyze the deal:

```
$ bridgitte analyze-deal "854.Q873.Q984.65 KQ32.T6.T72.AJ93 9.AJ542.J653.T87 AJT76.K9.AK.KQ42"
             ♠ KQ32
             ♥ T6
             ♦ T72
             ♣ AJ93
♠ 854                      ♠ 9
♥ Q873                     ♥ AJ542
♦ Q984                     ♦ J653
♣ 65                       ♣ T87
             ♠ AJT76
             ♥ K9
             ♦ AK
             ♣ KQ42

   ♣  ♦  ♥  ♠  N
W  1  6  7  1  1
N 12  6  6 12 12
E  1  6  7  1  1
S 12  6  6 12 12
```

When limited to a declarer+strain bridgitte will give a winning play:

```
$ bridgitte analyze-deal --declarer N --strain S '854.Q873.Q984.65 KQ32.T6.T72.AJ93 9.AJ542.J653.T87 AJT76.K9.AK.KQ42'`
             ♠ KQ32
             ♥ T6
             ♦ T72
             ♣ AJ93
♠ 854                      ♠ 9
♥ Q873                     ♥ AJ542
♦ Q984                     ♦ J653
♣ 65                       ♣ T87
             ♠ AJT76
             ♥ K9
             ♦ AK
             ♣ KQ42


   ♠
N 12

     W N E S
 1 E 🂨 🂭 🂩 🂡
 2 S 🂤 🂮 🂱 🂦
 3 N 🂥 🂢 🂻 🂧
 4 S 🂷 🂣 🂴 🂪
 5 S 🂸 🂺 🂵 🂫
 6 S 🂳 🂶 🂲 🂾
 7 S 🂽 🃊 🃋 🂹
 8 W 🃈 🃇 🃅 🃎
 9 S 🃉 🃂 🃆 🃁
10 S 🃕 🃑 🃚 🃝
11 N 🃖 🃙 🃗 🃞
12 S 🃄 🃛 🃘 🃔
13 N 🃍 🃓 🃃 🃒
```

## Design Notes

### Data Structures

- Card, Suit, CardValue are mainly used for IO operations
- CardSet is the main data structure used
  in search. Set of cards represented as a single u64.
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

## Code Contributions

Contributions are welcome. Just open a PR.
