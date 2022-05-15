/* don't pattern match against this */
[@deriving sexp]
type t;

/* head normalized types */
type head_normalized =
  | TyVar(Index.Abs.t, string)
  | TyVarHole(TyVarErrStatus.HoleReason.t, MetaVar.t, string)
  | Hole
  | Int
  | Float
  | Bool
  | Arrow(t, t)
  | Sum(t, t)
  | Prod(list(t))
  | List(t);

/* normalized types */
type normalized = HTypSyntax.t(Index.absolute);

/* escape hatch for unsafe type operations */
type unsafe = HTypSyntax.t(Index.absolute);

type join =
  | GLB
  | LUB;

let unsafe: t => unsafe;
let of_unsafe: unsafe => t;

let to_string: t => string;

/* abstraction */
let of_head_normalized: head_normalized => t;
let of_normalized: normalized => t;

/* construction */
let tyvar: (Index.Abs.t, string) => t;
let tyvarhole: (TyVarErrStatus.HoleReason.t, MetaVar.t, string) => t;
let hole: t;
let int: t;
let float: t;
let bool: t;
let arrow: (t, t) => t;
let sum: (t, t) => t;
let product: list(t) => t;
let list: t => t;

let is_hole: t => bool;
let is_tyvar: t => bool;

let tyvar_index: t => option(Index.Abs.t);
let tyvar_name: t => option(string);

let precedence_Prod: int;
let precedence_Arrow: int;
let precedence_Sum: int;
let precedence: t => int;

let normalize: (Contexts.t, t) => normalized;
let head_normalize: (Contexts.t, t) => head_normalized;
let normalized_consistent: (normalized, normalized) => bool;
let normalized_equivalent: (normalized, normalized) => bool;

let consistent: (Contexts.t, t, t) => bool;
let equivalent: (Contexts.t, t, t) => bool;

let get_prod_elements: head_normalized => list(t);
let get_prod_arity: head_normalized => int;

let matched_arrow: (Contexts.t, t) => option((t, t));
let matched_sum: (Contexts.t, t) => option((t, t));
let matched_list: (Contexts.t, t) => option(t);

let complete: t => bool;

let join: (Contexts.t, join, t, t) => option(t);
let join_all: (Contexts.t, join, list(t)) => option(t);

[@deriving sexp]
type ground_cases =
  | Hole
  | Ground
  | NotGroundOrHole(normalized) /* the argument is the corresponding ground type */;

let grounded_Arrow: ground_cases;
let grounded_Sum: ground_cases;
let grounded_Prod: int => ground_cases;
let grounded_List: ground_cases;

let ground_cases_of: normalized => ground_cases;

let subst_tyvars: (t, list((Index.Abs.t, t))) => t;
