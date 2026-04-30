[@deriving (show({with_path: false}), sexp, yojson)]
type m = unit;
[@deriving (show({with_path: false}), sexp, yojson)]
type a = unit;

include
  RichProbe.RichProbe with
    type model = m and type action = a and type value = CardTypes.collection;
