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

let temp: term => t =
  term => {
    term,
    annotation: IdTagged.IdTag.temp(),
  };

/* The variable an MPat binds, with its annotation if it has one. */
let rec binder = (mp: t): option((Var.t, option(TermBase.Typ.t))) =>
  switch (IdTagged.term_of(mp)) {
  | Var(x) => Some((x, None))
  | Asc(inner, ty) =>
    binder(inner) |> Option.map(((x, _)) => (x, Some(ty)))
  | Invalid(_)
  | EmptyHole
  | MultiHole(_) => None
  };

let name = (mp: t): option(Var.t) => Option.map(fst, binder(mp));

/* The id of the name inside an MPat (the binding site). */
let rec var_id = (mp: t): option(Id.t) =>
  switch (IdTagged.term_of(mp)) {
  | Var(_) => Some(rep_id(mp))
  | Asc(inner, _) => var_id(inner)
  | Invalid(_)
  | EmptyHole
  | MultiHole(_) => None
  };

let rec map_typ = (f: TermBase.Typ.t => TermBase.Typ.t, mp: t): t =>
  switch (IdTagged.term_of(mp)) {
  | Asc(inner, ty) => {
      ...mp,
      term: Asc(map_typ(f, inner), f(ty)),
    }
  | Var(_)
  | Invalid(_)
  | EmptyHole
  | MultiHole(_) => mp
  };

/* Replace, or add, the annotation of an MPat. */
let with_typ = (mp: t, ty: TermBase.Typ.t): t =>
  switch (IdTagged.term_of(mp)) {
  | Asc(inner, _) => {
      ...mp,
      term: Asc(inner, ty),
    }
  | Var(_)
  | Invalid(_)
  | EmptyHole
  | MultiHole(_) => Asc(mp, ty) |> temp
  };

let rec rename = (mp: t, x: Var.t): t =>
  switch (IdTagged.term_of(mp)) {
  | Var(_) => {
      ...mp,
      term: Var(x),
    }
  | Asc(inner, ty) => {
      ...mp,
      term: Asc(rename(inner, x), ty),
    }
  | Invalid(_)
  | EmptyHole
  | MultiHole(_) => mp
  };
