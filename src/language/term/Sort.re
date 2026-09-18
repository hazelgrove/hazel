[@deriving (show({with_path: false}), sexp, yojson, eq)]
type t =
  | Drv(DrvSort.t)
  | Any
  | Pat
  | Typ
  | TPat
  | Rul
  | Exp
  | Mod
  | Sig
  | MPat;

/* The sorts of a module or signature body. Their terms are its items:
   definitions separated by `;`, not forms that nest what follows them. */
let is_mod_or_sig =
  fun
  | Mod
  | Sig => true
  | Drv(_)
  | Any
  | Pat
  | Typ
  | TPat
  | Rul
  | Exp
  | MPat => false;

let to_string =
  fun
  | Drv(s) => DrvSort.to_string(s)
  | _ as s => show(s);

let class_of =
  fun
  | Drv(s) => DrvSort.class_of(s)
  | _ as s => show(s);

let all =
  (DrvSort.all |> List.map(s => Drv(s))) @ [Any, Pat, Typ, Rul, Exp, TPat];
