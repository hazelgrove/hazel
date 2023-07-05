open TermBase;
open Util.OptUtil.Syntax;
let rec get_module =
        (name: string, ctx: Ctx.t, ty: UTyp.t)
        : option((string, option(Ctx.t))) => {
  switch (ty.term) {
  | Tag(tag_name)
  | Var(tag_name) =>
    switch (Ctx.lookup_tag(ctx, tag_name)) {
    | Some({typ: Module(inner_ctx), _}) =>
      Some((name ++ tag_name ++ ".", Some(inner_ctx)))
    | _ => Some((name ++ tag_name ++ ".", None))
    }
  | Dot(t, tag_name) =>
    let+ (name, ctx) = get_module(name, ctx, t);
    let inner_ctx = {
      let* ctx = ctx;
      switch (Ctx.lookup_tag(ctx, tag_name)) {
      | Some({typ: Module(inner_ctx), _}) => Some(inner_ctx)
      | _ => None
      };
    };
    (name ++ tag_name ++ ".", inner_ctx);
  | Parens(t) => get_module(name, ctx, t)
  | _ => None
  };
};
