[@deriving (show({with_path: false}), sexp, yojson, enumerate, eq)]
type cls =
  | Invalid
  | EmptyHole
  | MultiHole
  | Var
  | Asc;

include TermBase.MPat;

let fresh: term => t = IdTagged.fresh;

let rep_id: t => Id.t = IdTagged.rep_id;

/* Whether the pattern binds the module name `x`. */
let rec binds = (x: string, mp: t): bool =>
  switch (IdTagged.term_of(mp)) {
  | Var(y) => String.equal(x, y)
  | Asc(mp, _) => binds(x, mp)
  | Invalid(_)
  | EmptyHole
  | MultiHole(_) => false
  };

let hole = (tms: list(TermBase.Any.t)): TermBase.MPat.term =>
  switch (tms) {
  | [] => EmptyHole
  | [_, ..._] => MultiHole(tms)
  };

let cls_of_term: Grammar.mpat_term('a) => cls =
  fun
  | Invalid(_) => Invalid
  | EmptyHole => EmptyHole
  | MultiHole(_) => MultiHole
  | Var(_) => Var
  | Asc(_) => Asc;

let show_cls: cls => string =
  fun
  | Invalid => "Invalid module name"
  | MultiHole => "Broken module name"
  | EmptyHole => "Empty module name hole"
  | Var => "Module name"
  | Asc => "Annotated module name";

let temp: term => t =
  term => {
    term,
    annotation: IdTagged.IdTag.temp(),
  };
