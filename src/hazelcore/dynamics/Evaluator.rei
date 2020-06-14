/**  Invalid Input codes:
     0 = out of fuel
     1 = free or invalid variable
     2 = ap invalid boxed function val
     3 = boxed value not a int literal 2
     4 = boxed value not a int literal 1
     5 = bad pattern match
     6 = Cast BV Hole Ground
     7 = boxed value not a float literal 1
     8 = boxed value not a float literal 2 */

[@deriving sexp]
type result =
  | InvalidInput(int)
  | BoxedValue(DHExp.t)
  | Indet(DHExp.t);

[@deriving sexp]
type ground_cases =
  | Hole
  | Ground
  | NotGroundOrHole(HTyp.t) /* the argument is the corresponding ground type */;

let grounded_Arrow: ground_cases;

let grounded_Sum: ground_cases;

let grounded_Prod: int => ground_cases;

let grounded_List: ground_cases;

let ground_cases_of: HTyp.t => ground_cases;

let eval_bin_bool_op: (DHExp.BinBoolOp.t, bool, bool) => DHExp.t;

let eval_bin_int_op: (DHExp.BinIntOp.t, int, int) => DHExp.t;

let eval_bin_float_op: (DHExp.BinFloatOp.t, float, float) => DHExp.t;

let evaluate: DHExp.t => result;

let evaluate_case:
  (
    option((MetaVar.t, MetaVarInst.t, VarMap.t_(DHExp.t))),
    DHExp.t,
    list(DHExp.rule),
    int
  ) =>
  result;
