open Grain;

[@deriving sexp]
type fn('f) = {
  name: ident,
  impl: expr,
  call: 'f,
};

type fnn = fn(args => expr);
type fn1 = fn(expr => expr);
type fn2 = fn((expr, expr) => expr);
type fn3 = fn((expr, expr, expr) => expr);
type fn4 = fn((expr, expr, expr, expr) => expr);
type fn5 = fn((expr, expr, expr, expr, expr) => expr);

let name = ({name, _}) => name;
let impl = ({impl, _}) => impl;
let call = ({call, _}) => call;

let get_impl = fn => SLet(pvar(name(fn)), impl(fn));

/**
  Functor to create implementation utilities for a given library module.
 */
module Make = (M: Lib.META) => {
  let fail = () => failwith("bad argument count");

  let mk_fn_lam = (vs, body) => {
    let pats = vs |> List.map(pvar);
    let vars = vs |> List.map(var);
    ELam(pats, body(vars));
  };

  let mk_fn = (name, vs, body): fnn => {
    let impl = mk_fn_lam(vs, body);
    let call = M.ap(name);
    {name, impl, call};
  };

  let mk_fn1 = (name, v1, body): fn1 => {
    let impl =
      mk_fn_lam(
        [v1],
        fun
        | [v1] => body(v1)
        | _ => fail(),
      );
    let call = M.ap1(name);
    {name, impl, call};
  };

  let mk_fn2 = (name, v1, v2, body): fn2 => {
    let impl =
      mk_fn_lam(
        [v1, v2],
        fun
        | [v1, v2] => body(v1, v2)
        | _ => fail(),
      );
    let call = M.ap2(name);
    {name, impl, call};
  };

  let mk_fn3 = (name, v1, v2, v3, body): fn3 => {
    let impl =
      mk_fn_lam(
        [v1, v2, v3],
        fun
        | [v1, v2, v3] => body(v1, v2, v3)
        | _ => fail(),
      );
    let call = M.ap3(name);
    {name, impl, call};
  };

  let mk_fn4 = (name, v1, v2, v3, v4, body): fn4 => {
    let impl =
      mk_fn_lam(
        [v1, v2, v3, v4],
        fun
        | [v1, v2, v3, v4] => body(v1, v2, v3, v4)
        | _ => fail(),
      );
    let call = M.ap4(name);
    {name, impl, call};
  };

  let mk_fn5 = (name, v1, v2, v3, v4, v5, body): fn5 => {
    let impl =
      mk_fn_lam(
        [v1, v2, v3, v4, v5],
        fun
        | [v1, v2, v3, v4, v5] => body(v1, v2, v3, v4, v5)
        | _ => fail(),
      );
    let call = M.ap5(name);
    {name, impl, call};
  };
};
