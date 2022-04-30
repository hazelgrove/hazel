/* types with holes */

[@deriving sexp]
type unknown_type_provenance =
  | UserGenerated(MetaVar.t)
  | SynPatternVar
  | Internal(internal_provenance)
and internal_provenance =
  // enumerate other base cases here if applicable; try to avoid
  | Matched_arrow_L(unknown_type_provenance)
  | Matched_arrow_R(unknown_type_provenance)
  | Matched_sum_L(unknown_type_provenance)
  | Matched_sum_R(unknown_type_provenance)
  | Matched_prod_L(unknown_type_provenance)
  | Matched_prod_R(unknown_type_provenance)
  | Matched_list(unknown_type_provenance);

// type unknown_type_provenance =
//   | TypHole /* from an actual type hole; will add a MetaVar.t once we have unique IDs for type holes */
//   | SynPatternVar
//   /* from a pattern var being asked to synthesize a type either directly or via matched arrow/prod/etc.
//      When analyzing against such a type, expression can be treated as if synthetic when the distinction matters
//      e.g. for inconsistent branches or cast insertion */
//   | Internal; /* other internally generated unknown types, e.g. the type synthesized by a hole in synthetic position etc. */

[@deriving sexp]
type t =
  | Unknown(unknown_type_provenance)
  | Int
  | Float
  | Bool
  | Arrow(t, t)
  | Sum(t, t)
  | Prod(list(t))
  | List(t);

type join =
  | GLB
  | LUB;

type inf_constraint = (t, t);

let precedence_Prod: int;
let precedence_Arrow: int;
let precedence_Sum: int;
let precedence: t => int;

/* type equality */
let eq: (t, t) => bool;

/* type consistency */
let consistent: (t, t) => bool;

let get_prod_elements: t => list(t);
let get_prod_arity: t => int;

let load_type_variable: t => unit;

let join: (join, t, t) => option(t);
let join_all: (join, list(t)) => option(t);

let is_unknown: t => bool;
