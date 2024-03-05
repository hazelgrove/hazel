include TermBase.Any;

let is_exp: t => option(TermBase.Exp.t) =
  fun
  | Exp(e) => Some(e)
  | _ => None;
let is_pat: t => option(TermBase.Pat.t) =
  fun
  | Pat(p) => Some(p)
  | _ => None;
let is_typ: t => option(TermBase.TypTerm.t) =
  fun
  | Typ(t) => Some(t)
  | _ => None;

let rec ids =
  fun
  | Exp(tm) => tm.ids
  | Pat(tm) => tm.ids
  | Typ(tm) => tm.ids
  | TPat(tm) => tm.ids
  | Rul(tm) => Rul.ids(~any_ids=ids, tm)
  | Nul ()
  | Any () => [];

// Terms may consist of multiple tiles, eg the commas in an n-tuple,
// the rules of a case expression + the surrounding case-end tile,
// the list brackets tile coupled with the elem-separating commas.
// The _representative id_ is the canonical tile id used to identify
// and look up info about a term.
//
// In instances like case expressions and list literals, where a parent
// tile surrounds the other tiles, the representative id is the parent tile's.
// In other instances like n-tuples, where the commas are all siblings,
// the representative id is one of the comma ids, unspecified which one.
// (This would change for n-tuples if we decided parentheses are necessary.)
let rep_id =
  fun
  | Exp(tm) => Exp.rep_id(tm)
  | Pat(tm) => Pat.rep_id(tm)
  | Typ(tm) => TypTerm.rep_id(tm)
  | TPat(tm) => TPat.rep_id(tm)
  | Rul(tm) => Rul.rep_id(~any_ids=ids, tm)
  | Nul ()
  | Any () => raise(Invalid_argument("Term.rep_id"));
