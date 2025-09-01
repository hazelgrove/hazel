/**
  The sort of derivation terms.

  (future) for the moment, we are not actually using `Jdmt`, `Ctx`, and `Ctx`
  because of a remolding issue. We are using `Exp` for the above sorts.
 */

[@deriving (show({with_path: false}), sexp, yojson, eq, enumerate)]
type t =
  | Jdmt
  | Ctx
  | Prop
  | Exp
  | Pat
  | Typ
  | TPat;

let class_of =
  fun
  | Jdmt => "Drv"
  | Ctx => "Drv"
  | Prop => "Drv"
  | Exp => "Exp"
  | Pat => "Pat"
  | Typ => "Typ"
  | TPat => "TPat";

let to_string =
  fun
  | Jdmt => "Jdmt"
  | Ctx => "Ctx"
  | Prop => "Prop"
  | Exp => "ALFA_Exp"
  | Pat => "ALFA_Pat"
  | Typ => "ALFA_Typ"
  | TPat => "ALFA_TPat";

let to_string_verbose =
  fun
  | Jdmt => "judgement"
  | Ctx => "context"
  | Prop => "proposition"
  | Exp => "ALFA expression"
  | Pat => "ALFA pattern"
  | Typ => "ALFA type"
  | TPat => "ALFA type pattern";

let consistent = (s, s') =>
  switch (s, s') {
  | (Jdmt | Ctx | Prop | Exp, Jdmt | Ctx | Prop | Exp) => true
  | (Jdmt | Ctx | Prop | Exp, _) => false
  | (Pat, Pat) => true
  | (Pat, _) => false
  | (Typ, Typ) => true
  | (Typ, _) => false
  | (TPat, TPat) => true
  | (TPat, _) => false
  };
