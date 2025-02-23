open Util.Sequence;

type t = Util.Sequence.t(DHExp.t);

let first = hd_exn;

// Only the fully instantiated terms
let values = env =>
  filter(~f=d =>
    ValueChecker.check_value((), ClosureEnvironment.of_environment(env), d)
    == Value
  );

// Only the indet terms
let indets = env =>
  filter(~f=d =>
    ValueChecker.check_value((), ClosureEnvironment.of_environment(env), d)
    == Indet
  );

// Only final forms
let finals = env =>
  filter(~f=d =>
    ValueChecker.check_value((), ClosureEnvironment.of_environment(env), d)
    != Expr
  );

// Only expressions
let exprs = env =>
  filter(~f=d =>
    ValueChecker.check_value((), ClosureEnvironment.of_environment(env), d)
    == Expr
  );

// First term evaluating to a cast error:
