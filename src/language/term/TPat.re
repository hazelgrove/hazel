[@deriving (show({with_path: false}), sexp, yojson, enumerate, eq)]
type cls =
  | Invalid
  | EmptyHole
  | MultiHole
  | Var
  | Param
  | Tuple
  | Parens;

include TermBase.TPat;

let fast_equal = Equality.syntactic.tpat;
let equal = fast_equal;

let rep_id: t => Id.t = IdTagged.rep_id;

let fresh: term => t = IdTagged.fresh;

let hole = (tms: list(TermBase.Any.t)): TermBase.TPat.term =>
  switch (tms) {
  | [] => EmptyHole
  | [_, ..._] => MultiHole(tms)
  };

let rec cls_of_term: Grammar.tpat_term('a) => cls =
  fun
  | Invalid(_) => Invalid
  | EmptyHole => EmptyHole
  | MultiHole(_) => MultiHole
  | Var(_) => Var
  | Param(_) => Param
  | Tuple(_) => Tuple
  /* Mirror the Exp/Pat/Typ convention: a parenthesized tpat reports
     the inner node's cls so the cursor inspector shows the
     underlying binder's info (e.g. "Type binder tuple" on the parens
     around `(a, b)`) instead of a standalone "Parenthesized" label. */
  | Parens(inner) => cls_of_term(inner.term);

let show_cls: cls => string =
  fun
  | Invalid => "Invalid type pattern"
  | MultiHole => "Broken type pattern"
  | EmptyHole => "Type pattern hole"
  | Var => "Type alias"
  | Param => "Parameterized type alias"
  | Tuple => "Type binder tuple"
  | Parens => "Parenthesized type pattern";

let temp: term => t =
  term => {
    term,
    annotation: IdTagged.IdTag.temp(),
  };

let rec head_name_of = (tpat: t): option(string) =>
  switch (tpat.term) {
  | Var(name) => Some(name)
  | Param(head, _) => head_name_of(head)
  | Parens(inner) => head_name_of(inner)
  | Invalid(_)
  | EmptyHole
  | MultiHole(_)
  | Tuple(_) => None
  };

let rec alias_head = (tpat: t): option((string, list(t))) =>
  switch (tpat.term) {
  | Var(name) => Some((name, []))
  | Param(head, params) =>
    switch (head_name_of(head)) {
    | Some(name) => Some((name, params))
    | None => None
    }
  | Parens(inner) => alias_head(inner)
  | Invalid(_)
  | EmptyHole
  | MultiHole(_)
  | Tuple(_) => None
  };

/* When the binder of a `Poly`/`TypAbs`/`TypFun`/`Rec` is a `Tuple`, it
   stands for a comma-separated list of single binders. `binders_of`
   flattens that list into a list of single-binder tpats; non-tuple
   binders return [tpat] as a singleton. `Parens` is transparent. */
let rec binders_of = (tpat: t): list(t) =>
  switch (tpat.term) {
  | Tuple(tps) => tps
  | Parens(inner) => binders_of(inner)
  | Var(_)
  | Invalid(_)
  | EmptyHole
  | MultiHole(_)
  | Param(_, _) => [tpat]
  };

let tyvars_of = (tpat: t): list(string) =>
  binders_of(tpat) |> List.filter_map(tyvar_of_utpat);
