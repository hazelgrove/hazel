include TermBase.PRul;

[@deriving (show({with_path: false}), sexp, yojson, enumerate)]
type cls =
  | ProofRule;

/* Proof-rule sort mirroring Rul, used as the child of the `induction` tile.
   As with Rul, a PRul may hold a scrutinee with no rules, so id lookup
   falls back to the scrutinee's ids when the PRul has none of its own. */
let ids = (~any_ids, {term, annotation: {ids, _}}: t) =>
  switch (ids) {
  | [_, ..._] => ids
  | [] =>
    switch (term) {
    | MultiHole([tm, ..._]) => any_ids(tm)
    | ProofRules(scrut, []) => IdTagged.ids(scrut)
    | _ => []
    }
  };

let rep_id = (~any_ids, tm) =>
  switch (ids(~any_ids, tm)) {
  | [] => raise(Invalid_argument("PRul.rep_id"))
  | [id, ..._] => id
  };

let unwrap: t => (term, term => t) = IdTagged.unwrap;
