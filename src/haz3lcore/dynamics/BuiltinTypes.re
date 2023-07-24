[@deriving (show({with_path: false}), sexp, yojson)]
type t = VarMap.t_(Typ.t);

let ctx = (types: t, ctx: Ctx.t): Ctx.t =>
  List.fold_left(
    (ctx, (name, typ)) => {
      let (ty_def, ctx_body) =
        switch (typ) {
        | Typ.Sum(_) when List.mem(name, Typ.free_vars(typ)) =>
          let ty_rec = Typ.Rec("α", Typ.subst(Var("α"), name, typ));
          let ctx_body = Ctx.extend_alias(ctx, name, Id.invalid, ty_rec);
          (ty_rec, ctx_body);
        | _ => (typ, Ctx.extend_alias(ctx, name, Id.invalid, typ))
        };
      switch (Typ.get_sum_tags(ctx, ty_def)) {
      | Some(sm) => Ctx.add_tags(ctx_body, name, Id.invalid, sm)
      | None => ctx_body
      };
    },
    ctx,
    types,
  );

let using = (name: Var.t, impl: Typ.t, builtin_types: t): t =>
  VarMap.extend(builtin_types, (name, impl));

module Pervasives = {
  module Impls = {
    let option_str =
      Typ.Sum([("OS_None", None), ("OS_Some", Some(String))]);

    let ipat =
      Typ.Sum([
        ("IE_EmptyHole", None),
        ("IP_Wild", None),
        ("IP_ExpandingKeyword", None),
        ("IP_InvalidText", None),
        ("IP_BadTag", None),
        ("IP_Var", Some(String)),
        ("IP_Int", Some(Int)),
        ("IP_Float", Some(Float)),
        ("IP_Bool", Some(Bool)),
        ("IP_String", Some(String)),
        ("IP_ListLit", Some(List(Var("IPat")))),
        ("IP_Cons", Some(Prod([Var("IPat"), Var("IPat")]))),
        ("IP_Tuple", Some(List(Var("IPat")))),
        ("IP_Tag", Some(String)),
        ("IP_Ap", Some(Prod([Var("IPat"), Var("IPat")]))),
      ]);

    let iexp =
      Typ.Sum([
        ("IE_EmptyHole", None),
        ("IE_ExpandingKeyword", None),
        ("IE_Var", Some(String)),
        ("IE_InvalidText", None),
        (
          "IE_Match",
          Some(
            Prod([Var("IExp"), List(Prod([Var("IPat"), Var("IExp")]))]),
          ),
        ),
        ("IE_Sequence", Some(Prod([Var("IExp"), Var("IExp")]))),
        ("IE_Let", Some(Prod([Var("IPat"), Var("IExp"), Var("IExp")]))),
        (
          "IE_Fun",
          Some(Prod([Var("IPat"), Var("IExp"), Var("Option_Str")])),
        ),
        ("IE_Ap", Some(Prod([Var("IExp"), Var("IExp")]))),
        ("IE_ApBuiltin", None),
        ("IE_TestLit", None),
        ("IE_Bool", Some(Bool)),
        ("IE_Int", Some(Int)),
        ("IE_Float", Some(Float)),
        ("IE_String", Some(String)),
        ("IE_BinOp", Some(Prod([Var("IExp"), Var("IExp")]))),
        ("IE_ListLit", Some(List(Var("IExp")))),
        ("IE_Cons", Some(Prod([Var("IExp"), Var("IExp")]))),
        ("IE_Tuple", Some(List(Var("IExp")))),
        ("IE_Prj", Some(Var("IExp"))),
        ("IE_Tag", Some(String)),
      ]);
  };

  let builtin_types =
    VarMap.empty
    |> using("Option_Str", Impls.option_str)
    |> using("IPat", Impls.ipat)
    |> using("IExp", Impls.iexp);
};
