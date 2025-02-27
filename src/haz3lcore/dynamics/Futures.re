open Util.Sequence;

type t = Util.Sequence.t(DHExp.t);

let empty = empty;
let first = hd_exn;
let nth = (i, s) => nth_exn(s, i);
let filter = filter;

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
