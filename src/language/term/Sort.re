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
