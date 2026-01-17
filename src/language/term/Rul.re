include TermBase.Rul;

[@deriving (show({with_path: false}), sexp, yojson, enumerate)]
type cls =
  | Rule;

// example of awkwardness induced by having forms like rules
// that may have a different-sorted child with no delimiters
// (eg scrut with no rules)
let ids = (~any_ids, {term, annotation: {ids, _}}: t) =>
  switch (ids) {
  | [_, ..._] => ids
  | [] =>
    switch (term) {
    | MultiHole([tm, ..._]) => any_ids(tm)
    | Rules(scrut, []) => IdTagged.ids(scrut)
    | _ => []
    }
  };

let rep_id = (~any_ids, tm) =>
  switch (ids(~any_ids, tm)) {
  | [] => raise(Invalid_argument("Exp.rep_id"))
  | [id, ..._] => id
  };

let unwrap: t => (term, term => t) = IdTagged.unwrap;
