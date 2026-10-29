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

/* The module name bound, if the pattern is well formed. */
let rec name = (mp: t): option(Var.t) =>
  switch (mp.term) {
  | Var(x) => Some(x)
  | Asc(inner, _) => name(inner)
  | Invalid(_)
  | EmptyHole
  | MultiHole(_) => None
  };

/* Whether the name is a hole, which a module could still be bound by. */
let rec has_hole = (mp: t): bool =>
  switch (mp.term) {
  | EmptyHole
  | MultiHole(_) => true
  | Asc(inner, _) => has_hole(inner)
  | Var(_)
  | Invalid(_) => false
  };

let temp: term => t =
  term => {
    term,
    annotation: IdTagged.IdTag.temp(),
  };

/* The module name pattern as a value pattern, keeping its ids: a module
   binding is a value binding, checked and matched as one. */
let rec to_pat = (mp: t): TermBase.Pat.t => {
  let at = (term: TermBase.Pat.term) =>
    IdTagged.fast_copy(rep_id(mp), IdTagged.fresh(term));
  switch (mp.term) {
  | Var(name) => at(Var(name))
  | Asc(inner, typ) => at(Asc(to_pat(inner), typ))
  | Invalid(token) => at(Invalid(token))
  | EmptyHole => at(EmptyHole)
  | MultiHole(tms) => at(MultiHole(tms))
  };
};
