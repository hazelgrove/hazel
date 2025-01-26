/**
  The sort of derivation terms.

  (future) for the moment, we are not actually using `Jdmt`, `Ctx`, and `Ctx`
  because of a remolding issue. We are using `Exp` for the above sorts.
 */

[@deriving (show({with_path: false}), sexp, yojson)]
type t =
  | Jdmt
  | Ctx
  | Prop
  | Exp
  | Rul
  | Pat
  | Typ
  | TPat;

let show =
  fun
  | Jdmt => "Jdmt"
  | Prop => "Prop"
  | Ctx => "Ctx"
  | Exp => "ALFA_Exp"
  | Rul => "ALFA_Rul"
  | Pat => "ALFA_Pat"
  | Typ => "ALFA_Typ"
  | TPat => "ALFA_TPat";

let class_of =
  fun
  | Jdmt => "Drv"
  | Ctx => "Drv"
  | Prop => "Exp"
  | Exp => "Exp"
  | Rul => "Rul"
  | Pat => "Pat"
  | Typ => "Typ"
  | TPat => "TPat";

let all = [Jdmt, Ctx, Prop, Exp, Pat, Typ, TPat];

let to_string =
  fun
  | Jdmt => "Jdmt"
  | Ctx => "Ctx"
  | Prop => "Prop"
  | Exp => "ALFA_Exp"
  | Rul => "ALFA_Rul"
  | Pat => "ALFA_Pat"
  | Typ => "ALFA_Typ"
  | TPat => "ALFA_TPat";

let to_string_verbose =
  fun
  | Jdmt => "judgement"
  | Ctx => "context"
  | Prop => "proposition"
  | Exp => "ALFA expression"
  | Rul => "ALFA rule"
  | Pat => "ALFA pattern"
  | Typ => "ALFA type"
  | TPat => "ALFA type pattern";
