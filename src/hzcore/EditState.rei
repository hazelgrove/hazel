type cursor =
  | Pointing(Path.t);

// invariant: all paths in cursor are valid w.r.t. term
type t = pri (cursor, Term_exp.t);

let mk = (cursor: cursor, term: Term_exp.t) => {
  // TODO add check with debug flag
  (cursor, term);
};