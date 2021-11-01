[@deriving sexp]
type invalid_input =
  | FreeOrInvalidVariable // 1
  | ApInvalidBoxedFunctionVal // 2
  | BoxedNotIntLit2 // 3
  | BoxedNotIntLit1 // 4
  | CastBVHoleGround // 6
  | BoxedNotFloatLit1 // 7
  | BoxedNotFloatLit2; //8

[@deriving sexp]
type result =
  | InvalidInput(invalid_input)
  | BoxedValue(DHExp.t)
  | Indet(DHExp.t);

[@deriving sexp]
type ground_cases =
  | Hole
  | Ground
  | NotGroundOrHole(HTyp.t) /* the argument is the corresponding ground type */;

[@deriving sexp]
type assert_eq_map = list((AssertNumber.t, list(DHExp.t)));

[@deriving sexp]
type state = {
  assert_map: AssertMap.t,
  assert_eqs: assert_eq_map,
  step: int,
  fuel: int,
};

[@deriving sexp]
type report = (result, state);

let evaluate: (~state: state=?, DHExp.t) => report;

/* closed substitution [d1/x]d2*/
let subst_var: (DHExp.t, Var.t, DHExp.t) => DHExp.t;

let subst: (Environment.t, DHExp.t) => DHExp.t;
