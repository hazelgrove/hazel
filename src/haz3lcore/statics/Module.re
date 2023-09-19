open TermBase;
open Util.OptUtil.Syntax;
/*
 This function generates the name for a dot type,
 'name' is the member name, 'ctx' is the current context, 'ty' is the Module type.
 Outputs the whole name of this term and the inner context of the module.

 input: get_module("T", ctx, Int)
 output: None (Int is not a valid module type)

 input: get_module("T", ctx, Constructor("M")) where M is not in ctx or is not a module.
 output: Some("M.T", None)

 input: get_module("T", ctx, Constructor("M")) where M is of type Module(ctx1) in ctx.
 output: Some("M.T", Some(ctx1))

 input: get_module("T", ctx, Dot(Constructor("M"), M1)) where M is of type Module(M1: Module(ctx1)) in ctx.
 output: Some("M.M1.T", Some(ctx1))
  */
let get_tyname = (ty: UTyp.t) =>
  switch (ty.term) {
  | Var(name)
  | Constructor(name)
  | Invalid(name) => Some(name)
  | _ => None
  };
let rec get_module =
        (name: string, ctx: Ctx.t, ty: UTyp.t)
        : option((string, option(Ctx.t))) => {
  switch (ty.term) {
  | Constructor(tag_name)
  | Var(tag_name) =>
    switch (Ctx.lookup_var(ctx, tag_name)) {
    | Some({typ: Module(inner_ctx), _}) =>
      Some((name ++ tag_name ++ ".", Some(inner_ctx)))
    | _ => Some((name ++ tag_name ++ ".", None))
    }
  | Dot(t1, t2) =>
    let* tag_name = get_tyname(t2);
    let+ (name, ctx) = get_module(name, ctx, t1);
    let inner_ctx = {
      let* ctx = ctx;
      switch (Ctx.lookup_var(ctx, tag_name)) {
      | Some({typ: Module(inner_ctx), _}) => Some(inner_ctx)
      | _ => None
      };
    };
    (name ++ tag_name ++ ".", inner_ctx);
  | Parens(t) => get_module(name, ctx, t)
  | _ => None
  };
};

let foldable = (label: Label.t) => {
  label == ["module", "=", "in"];
};
