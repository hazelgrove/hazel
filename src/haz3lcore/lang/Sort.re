module DrvSort = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t =
    | Jdmt
    | Ctx
    | Prop
    | Exp
    | Pat
    | Typ
    | TPat;

  let show =
    fun
    | Jdmt => "Jdmt"
    | Prop => "Prop"
    | Ctx => "Ctx"
    | Exp => "ALFA_Exp"
    | Pat => "ALFA_Pat"
    | Typ => "ALFA_Typ"
    | TPat => "ALFA_TPat";

  let class_of =
    fun
    | Jdmt => "Drv"
    | Ctx => "Drv"
    | Prop => "Exp"
    | Exp => "Exp"
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

  let detail_sort: list(string) => t =
    fun
    | [".val"] => Jdmt
    | ["val", "end"] => Jdmt
    | ["eval", "to"] => Jdmt
    | ["|-"] => Jdmt
    | ["$>"] => Jdmt
    | ["[]"] => Ctx
    | ["[", _] => Ctx
    | _ => Exp;
};

[@deriving (show({with_path: false}), sexp, yojson)]
type t =
  | Drv(DrvSort.t)
  | Any
  | Nul
  | Pat
  | Typ
  | TPat
  | Rul
  | Exp;

let show =
  fun
  | Drv(s) => DrvSort.show(s)
  | _ as s => show(s);

let class_of =
  fun
  | Drv(s) => DrvSort.class_of(s)
  | _ as s => show(s);

let root = Exp;

let all =
  (DrvSort.all |> List.map(s => Drv(s)))
  @ [Any, Nul, Pat, Typ, Rul, Exp, TPat];

let consistent = (s, s') =>
  switch (s, s') {
  | (Any, _)
  | (_, Any) => true
  | (Nul, _)
  | (_, Nul) => false
  | _ => s == s'
  };

let to_string =
  fun
  | Drv(s) => DrvSort.class_of(s)
  | Any => "Any"
  | Nul => "Nul"
  | Pat => "Pat"
  | TPat => "TPat"
  | Typ => "Typ"
  | Rul => "Rul"
  | Exp => "Exp";

let to_string_verbose =
  fun
  | Drv(s) => DrvSort.to_string_verbose(s)
  | Any => "any"
  | Nul => "null"
  | Pat => "pattern"
  | TPat => "type pattern"
  | Typ => "type"
  | Rul => "rule"
  | Exp => "expression";
