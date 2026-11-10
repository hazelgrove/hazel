[@deriving (show({with_path: false}), sexp, yojson, enumerate, eq)]
type cls =
  | Invalid
  | EmptyHole
  | MultiHole
  | Var;

include TermBase.TPat;

let fast_equal = Equality.syntactic.tpat;
let equal = fast_equal;

let rep_id: t => Id.t = IdTagged.rep_id;

let fresh: term => t = IdTagged.fresh;

/* Whether the pattern binds the type variable `x`. */
let binds = (x: string, tp: t): bool =>
  switch (IdTagged.term_of(tp)) {
  | Var(y) => String.equal(x, y)
  | Invalid(_)
  | EmptyHole
  | MultiHole(_) => false
  };

/* The pattern with its variable named `x`; holes are unchanged. */
let rename = (x: string, tp: t): t => {
  let (term, rewrap) = IdTagged.unwrap(tp);
  switch (term) {
  | Var(_) => (Var(x): term) |> rewrap
  | Invalid(_)
  | EmptyHole
  | MultiHole(_) => tp
  };
};

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
  | Var(_) => Var;

let show_cls: cls => string =
  fun
  | Invalid => "Invalid type alias"
  | MultiHole => "Broken type alias"
  | EmptyHole => "Type alias hole"
  | Var => "Type alias";

let temp: term => t =
  term => {
    term,
    annotation: IdTagged.IdTag.temp(),
  };
