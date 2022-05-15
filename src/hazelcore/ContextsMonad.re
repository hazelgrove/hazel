module T = {
  type state = Contexts.t;
  type t('result) = state => ('result, state);

  let return: 'a => t('result) = (x, ctx) => (x, ctx);

  let bind: (t('a), 'a => t('b)) => t('b) =
    (a, f, ctx_a) => {
      let (x, ctx_b) = a(ctx_a);
      f(x, ctx_b);
    };

  let map = Monads.MapDefinition.Define_using_bind;
};

include T;
include Monads.State.Make(T);
include Syntax;

/* Expression Variables */
let add_var = (x: Var.t, ty: HTyp.t): t(unit) =>
  ctx => put(Contexts.add_var(ctx, x, HTyp.unsafe(ty)), ctx);

let add_tyvar = (t: string, k: Kind.t): t(unit) =>
  ctx => put(Contexts.add_tyvar(ctx, t, k), ctx);
