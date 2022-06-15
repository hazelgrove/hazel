[@deriving sexp]
type ground_cases =
  | Hole
  | Ground
  | NotGroundOrHole(HTyp.t) /* the argument is the corresponding ground type */;

let evaluate: DHExp.t => EvaluatorResult.t;

/* closed substitution [d1/x]d2*/
let subst_var: (DHExp.t, Var.t, DHExp.t) => DHExp.t;

let subst: (Environment.t, DHExp.t) => DHExp.t;

type match_result =
  | Matches(Environment.t)
  | DoesNotMatch
  | Indet;

let matches_cast_Inj:
  (InjSide.t, DHPat.t, DHExp.t, list((HTyp.t, HTyp.t, HTyp.t, HTyp.t))) =>
  match_result;

let matches_cast_Pair:
  (
    DHPat.t,
    DHPat.t,
    DHExp.t,
    list((HTyp.t, HTyp.t)),
    list((HTyp.t, HTyp.t))
  ) =>
  match_result;

let matches_cast_Cons:
  (DHPat.t, DHExp.t, list((HTyp.t, HTyp.t))) => match_result;
