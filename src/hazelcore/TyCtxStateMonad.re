module T = {
  type state = TyCtx.t;
  type t('result) = state => ('result, state);

  let return: 'a => t('result) = (x, tyctx) => (x, tyctx);

  let bind: (t('a), 'a => t('b)) => t('b) =
    (a, f, tyctx_a) => {
      let (x, tyctx_b) = a(tyctx_a);
      f(x, tyctx_b);
    };

  let map = Monads.MapDefinition.Define_using_bind;
};

include T;
include Monads.State.Make(T);
include Syntax;

/* Expression Variables */
let bind_var = (x: Var.t, ty: HTyp.t): t(unit) =>
  tyctx => put(TyCtx.bind_var(tyctx, x, HTyp.unsafe(ty)), tyctx);

let push_tyvar = (name: string, kind: Kind.t): t(unit) =>
  tyctx => put(TyCtx.push_tyvar(tyctx, name, kind), tyctx);
