open Util.Sequence;

type t = Util.Sequence.t(DHExp.t);

let first = hd_exn;
let nth = (i, s) => nth_exn(s, i);

// Only the fully instantiated terms
let values =
  filter(~f=d =>
    ValueChecker.check_value((), ClosureEnvironment.empty, d) == Value
  );

// Only the indet terms
let indets =
  filter(~f=d =>
    ValueChecker.check_value((), ClosureEnvironment.empty, d) == Indet
  );

// Only final forms
let finals =
  filter(~f=d =>
    ValueChecker.check_value((), ClosureEnvironment.empty, d) != Expr
  );

// Only expressions
let exprs =
  filter(~f=d =>
    ValueChecker.check_value((), ClosureEnvironment.empty, d) == Expr
  );

// All results which contain failed casts syntactically speaking.
let failed_casts = {
  let rec contains_failed_cast = d => {
    d
    |> IdTagged.term_of
    |> (
      fun
      | FailedCast(_) => true
      | Invalid(_)
      | EmptyHole
      | MultiHole(_)
      | DynamicErrorHole(_)
      | Deferral(_)
      | Undefined
      | Bool(_)
      | Int(_)
      | Float(_)
      | String(_)
      | Constructor(_)
      | Var(_)
      | BuiltinFun(_) => false
      | Fun(_, d, _, _)
      | TypFun(_, d, _)
      | FixF(_, d, _)
      | TyAlias(_, _, d)
      | TypAp(d, _)
      | Test(d)
      | Filter(_, d)
      | Closure(_, d)
      | Parens(d)
      | UnOp(_, d)
      | Cast(d, _, _) => d |> contains_failed_cast
      | Let(_, d1, d2)
      | Ap(_, d1, d2)
      | Seq(d1, d2)
      | Cons(d1, d2)
      | ListConcat(d1, d2)
      | BinOp(_, d1, d2) =>
        contains_failed_cast(d1) || contains_failed_cast(d2)
      | If(d1, d2, d3) =>
        contains_failed_cast(d1)
        || contains_failed_cast(d2)
        || contains_failed_cast(d3)
      | Tuple(ds)
      | ListLit(ds) => ds |> List.exists(contains_failed_cast)
      | DeferredAp(d, ds) => [d, ...ds] |> List.exists(contains_failed_cast)
      | Match(d, pds) =>
        [d, ...pds |> List.map(snd)] |> List.exists(contains_failed_cast):
        DHExp.term => bool
    );
  };

  filter(~f=contains_failed_cast);
};
