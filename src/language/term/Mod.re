[@deriving (show({with_path: false}), sexp, yojson, enumerate, eq)]
type cls =
  | Invalid
  | EmptyHole
  | MultiHole
  | ModLet
  | ModType
  | ModExp;

include TermBase.Mod;

let fast_equal = Equality.syntactic.mod_;
let equal = fast_equal;

let rep_id: t => Id.t = IdTagged.rep_id;

let fresh: term => t = IdTagged.fresh;

let hole = (tms: list(TermBase.Any.t)): TermBase.Mod.term =>
  switch (tms) {
  | [] => EmptyHole
  | [_, ..._] => MultiHole(tms)
  };

let cls_of_term: Grammar.mod_term('a) => cls =
  fun
  | Invalid(_) => Invalid
  | EmptyHole => EmptyHole
  | MultiHole(_) => MultiHole
  | ModLet(_, _) => ModLet
  | ModType(_, _) => ModType
  | ModExp(_) => ModExp;

let show_cls: cls => string =
  fun
  | Invalid => "Invalid module"
  | MultiHole => "Broken module"
  | EmptyHole => "Module hole"
  | ModLet => "Let declaration"
  | ModType => "Type declaration"
  | ModExp => "Module expression";

let temp: term => t =
  term => {
    term,
    annotation: IdTagged.IdTag.temp(),
  };
