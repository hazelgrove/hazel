open CardTypes;
[@deriving (show({with_path: false}), sexp, yojson)]
type state =
  | Card(card)
  | Hand(hand);
[@deriving (show({with_path: false}), sexp, yojson)]
type mode =
  | Show
  | Choose
  | Flipped;

[@deriving (show({with_path: false}), sexp, yojson)]
type m = {mode};
[@deriving (show({with_path: false}), sexp, yojson)]
type a =
  | SetMode(mode);

include
  RichProbe.RichProbe with
    type model = m and type action = a and type value = state;
