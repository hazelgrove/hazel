open Util;

[@deriving (show({with_path: false}), sexp, yojson)]
type suit =
  | UnknownS
  | Hearts
  | Diamonds
  | Clubs
  | Spades;

[@deriving (show({with_path: false}), sexp, yojson)]
type rank =
  | UnknownR
  | Ace
  | Two
  | Three
  | Four
  | Five
  | Six
  | Seven
  | Eight
  | Nine
  | Ten
  | Jack
  | Queen
  | King;

[@deriving (show({with_path: false}), sexp, yojson)]
type card = (suit, rank);

[@deriving (show({with_path: false}), sexp, yojson)]
type hand = list(card);

let suit_to_int = (suit: suit): int =>
  switch (suit) {
  | Hearts => 0
  | Clubs => 1
  | Diamonds => 2
  | Spades => 3
  | UnknownS => 4
  };

let rank_to_int = (rank: rank): int =>
  switch (rank) {
  | Two => 1
  | Three => 2
  | Four => 3
  | Five => 4
  | Six => 5
  | Seven => 6
  | Eight => 7
  | Nine => 8
  | Ten => 9
  | Jack => 10
  | Queen => 11
  | King => 12
  | Ace => 13
  | UnknownR => 14
  };
