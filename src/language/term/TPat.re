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

let cls_of_term: Grammar.tpat_term('a) => cls =
  fun
  | Invalid(_) => Invalid
  | EmptyHole => EmptyHole
  | MultiHole(_) => MultiHole
  | Var(_) => Var
  | Param(_) => Param
  | Tuple(_) => Tuple
  | Parens(_) => Parens;

let show_cls: cls => string =
  fun
  | Invalid => "Invalid type alias"
  | MultiHole => "Broken type alias"
  | EmptyHole => "Type alias hole"
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
  | _ => None
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
  | _ => None
  };

/* When the binder of a `Poly`/`TypFun`/`TypLam`/`Rec` is a `Tuple`, it
   stands for a comma-separated list of single binders. `binders_of`
   flattens that list into a list of single-binder tpats; non-tuple
   binders return [tpat] as a singleton. `Parens` is transparent.
   The list flattens one level only — there are no nested `Tuple`s in
   valid surface programs. */
let rec binders_of = (tpat: t): list(t) =>
  switch (tpat.term) {
  | Tuple(tps) => tps
  | Parens(inner) => binders_of(inner)
  | _ => [tpat]
  };

let tyvars_of = (tpat: t): list(string) =>
  binders_of(tpat) |> List.filter_map(tyvar_of_utpat);
