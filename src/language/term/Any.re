include TermBase.Any;

let fast_equal = Equality.syntactic.any;
let equal = fast_equal;

let is_exp: t => option(TermBase.Exp.t) =
  fun
  | Exp(e) => Some(e)
  | _ => None;
let is_pat: t => option(TermBase.Pat.t) =
  fun
  | Pat(p) => Some(p)
  | _ => None;
let is_typ: t => option(TermBase.Typ.t) =
  fun
  | Typ(t) => Some(t)
  | _ => None;

let is_mod: t => option(TermBase.Mod.t) =
  fun
  | Mod(m) => Some(m)
  | _ => None;

let rec ids: TermBase.any_t => list(Id.t) =
  fun
  | Exp(tm) => IdTagged.ids(tm)
  | Pat(tm) => IdTagged.ids(tm)
  | Typ(tm) => IdTagged.ids(tm)
  | TPat(tm) => IdTagged.ids(tm)
  | Rul(tm) => Rul.ids(~any_ids=ids, tm)
  | Mod(tm) => IdTagged.ids(tm)
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
  | (Exp(tm): TermBase.any_t) => Exp.rep_id(tm)
  | Pat(tm) => Pat.rep_id(tm)
  | Typ(tm) => Typ.rep_id(tm)
  | TPat(tm) => TPat.rep_id(tm)
  | Rul(tm) => Rul.rep_id(~any_ids=ids, tm)
  | Mod(tm) => IdTagged.rep_id(tm)
  | Any () => raise(Invalid_argument("Term.rep_id"));
