exception KindError(string);

let rec synth_kind = (ctx: Ctx.t, typ: Typ.t): Ctx.kind => {
  switch (typ.term) {
  | Var(name) =>
    switch (Ctx.lookup_alias(ctx, name)) {
    | Some(ty_alias) => synth_kind(ctx, ty_alias)
    | None =>
      switch (Ctx.lookup_tvar(ctx, name)) {
      | None => Ctx.Abstract
      | Some(kind) => kind
      }
    }
  | Ap(t1, t2) =>
    let k_fun = synth_kind(ctx, t1);
    let k_arg = synth_kind(ctx, t2);
    switch (k_fun) {
    | Ctx.Arr(dom_kind, cod_kind) =>
      if (dom_kind == k_arg) {
        cod_kind;
      } else {
        raise(KindError("Type argument kind mismatch"));
      }
    | _ => raise(KindError("non-constructor type application"))
    };
  | Arrow(t_left, t_right) =>
    let k_left = synth_kind(ctx, t_left);
    let k_right = synth_kind(ctx, t_right);
    if (k_left == Ctx.Abstract && k_right == Ctx.Abstract) {
      Ctx.Abstract;
    } else {
      raise(KindError("Function type components must be kind Type"));
    };
  | Prod(ts) =>
    let kinds = List.map(t => synth_kind(ctx, t), ts);
    if (List.for_all(k => k == Ctx.Abstract, kinds)) {
      Ctx.Abstract;
    } else {
      raise(KindError("Tuple components must be kind Type"));
    };
  | Forall(_, t_body) =>
    let k_body = synth_kind(ctx, t_body);
    if (k_body == Ctx.Abstract) {
      Ctx.Abstract;
    } else {
      raise(KindError("Forall body must be kind Type"));
    };
  | Rec(_, t_body) =>
    let k_body = synth_kind(ctx, t_body);
    if (k_body == Ctx.Abstract) {
      Ctx.Abstract;
    } else {
      raise(KindError("Recursive type body must be kind Type"));
    };
  | Atom(_)
  | List(_)
  | Sum(_)
  | Label(_)
  | TupLabel(_, _)
  | Parens(_) => raise(KindError("Unsupported term in kind"))
  | Unknown(_) => Ctx.Abstract
  };
};
