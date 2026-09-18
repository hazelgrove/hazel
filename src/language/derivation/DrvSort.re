/**
  The sort of derivation terms.

  (future) For the moment we are not actually using the `Jdmt`, `Ctx`, and
  `Prop` sorts because of a remolding issue; we use `Exp` for all three.
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
  | Jdmt => "DrvJdmt"
  | Ctx => "DrvCtx"
  | Prop => "DrvProp"
  | Exp => "ALFAExp"
  | Pat => "DrvPat"
  | Typ => "ALFATyp"
  | TPat => "DrvTPat";

/* Terse display form used in UI (e.g. cursor inspector header). Keeps the
   `ALFA` prefix that distinguishes object-language sorts from regular
   Hazel sorts, but drops the `Drv` prefix used on meta-level type names. */
let to_string_short =
  fun
  | Jdmt => "Jdmt"
  | Ctx => "Ctx"
  | Prop => "Prop"
  | Exp => "ALFAExp"
  | Pat => "ALFAPat"
  | Typ => "ALFATyp"
  | TPat => "ALFATPat";
