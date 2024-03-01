include TermBase.Rul;

[@deriving (show({with_path: false}), sexp, yojson)]
type cls =
  | Rule;

// example of awkwardness induced by having forms like rules
// that may have a different-sorted child with no delimiters
// (eg scrut with no rules)
let ids = (~any_ids, {ids, term}: t) =>
  switch (ids) {
  | [_, ..._] => ids
  | [] =>
    switch (term) {
    | Hole([tm, ..._]) => any_ids(tm)
    | Rules(scrut, []) => scrut.ids
    | _ => []
    }
  };

let rep_id = (~any_ids, tm) =>
  switch (ids(~any_ids, tm)) {
  | [] => raise(Invalid_argument("UExp.rep_id"))
  | [id, ..._] => id
  };
