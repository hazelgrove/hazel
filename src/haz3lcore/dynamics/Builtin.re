open Sexplib.Std;
/* Evaluator alias. */
[@deriving (show({with_path: false}), sexp, yojson)]
type evaluate = (ClosureEnvironment.t, DHExp.t) => DHExp.t;
[@deriving (show({with_path: false}), sexp, yojson)]
type args = list(DHExp.t);
[@deriving (show({with_path: false}), sexp, yojson)]
type builtin_evaluate = args => DHExp.t;
[@deriving (show({with_path: false}), sexp, yojson)]
type t = {
  typ: Typ.t,
  eval: builtin_evaluate,
  elab: DHExp.t,
};

let mk_elab = (name: Var.t, typ: Typ.t): DHExp.t => {
  let rec mk_elab_inner =
          (typ': Typ.t, n: int, bindings: list(Var.t)): DHExp.t => {
    switch (typ') {
    | Arrow(_, typ'') =>
      let var = "x" ++ string_of_int(n);
      Fun(
        Var(var),
        typ',
        Closure(
          ClosureEnvironment.of_environment(Environment.empty),
          mk_elab_inner(typ'', n + 1, [var, ...bindings]),
        ),
        Some(name),
      );
    | _ =>
      let bindings = List.rev_map(x => DHExp.BoundVar(x), bindings);
      ApBuiltin(name, bindings);
    };
  };
  mk_elab_inner(typ, 0, []);
};
let mk = (name: Var.t, typ: Typ.t, eval: builtin_evaluate): t => {
  let elab = mk_elab(name, typ);
  {typ, eval, elab};
};

let mk_zero = (name: Var.t, typ: Typ.t, v: DHExp.t): t => {
  let fn = args => {
    switch (args) {
    | [] => v
    | _ => raise(EvaluatorError.Exception(BadBuiltinAp(name, args)))
    };
  };
  mk(name, typ, fn);
};
let mk_one = (name: Var.t, typ: Typ.t, fn: DHExp.t => DHExp.t): t => {
  let fn = args => {
    switch (args) {
    | [r1] => fn(r1)
    | _ => raise(EvaluatorError.Exception(BadBuiltinAp(name, args)))
    };
  };
  mk(name, typ, fn);
};
let mk_two = (name: Var.t, ty: Typ.t, fn: (DHExp.t, DHExp.t) => DHExp.t): t => {
  let fn = args => {
    switch (args) {
    | [r1, r2] => fn(r1, r2)
    | _ => raise(EvaluatorError.Exception(BadBuiltinAp(name, args)))
    };
  };
  mk(name, ty, fn);
};
