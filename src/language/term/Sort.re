[@deriving (show({with_path: false}), sexp, yojson, eq)]
type t =
  | Drv(DrvSort.t)
  | Fumola(FumolaSort.t)
  | Bb(BbSort.t)
  | Any
  | Pat
  | Typ
  | TPat
  | Rul
  | Exp
  | Mod
  | Sig
  | MPat;

let to_string =
  fun
  | Drv(s) => DrvSort.to_string(s)
  | Fumola(s) => FumolaSort.to_string(s)
  | Bb(s) => BbSort.to_string(s)
  | _ as s => show(s);

let class_of =
  fun
  | Drv(s) => DrvSort.class_of(s)
  | Fumola(s) => FumolaSort.class_of(s)
  | Bb(s) => BbSort.class_of(s)
  | _ as s => show(s);

/* Fumola is a closed sub-language: its tiles never mix with Hazel's, so a
   token with no Fumola form stays a monotile rather than expanding into the
   enclosing sort's. See Insert.effective_sort. */
let is_fumola =
  fun
  | Fumola(_) => true
  | _ => false;

/* Blackboard is a closed sub-language: its tiles never mix with Hazel's.
   See Insert.effective_sort. */
let is_bb =
  fun
  | Bb(_) => true
  | _ => false;

let all =
  (DrvSort.all |> List.map(s => Drv(s)))
  @ (FumolaSort.all |> List.map(s => Fumola(s)))
  @ (BbSort.all |> List.map(s => Bb(s)))
  @ [Any, Pat, Typ, Rul, Exp, TPat];
