// module Prop = Prop;
// module Rule = Rule;
// module Base = DerivationBase;
// module Verify = Verify;

[@deriving (show({with_path: false}), sexp, yojson)]
type t =
  | Jdmt
  | Prop
  | Exp
  | Pat
  | Typ
  | TPat;

let repr =
  fun
  | Jdmt => "Jdmt"
  | Prop => "Prop"
  | Exp => "ALFA_Exp"
  | Pat => "ALFA_Pat"
  | Typ => "ALFA_Typ"
  | TPat => "ALFA_TPat";
