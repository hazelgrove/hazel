open Haz3lcore;

//TODO(andrew): calculate this in a more principled way
let get_info_from_zipper =
    (~settings: Settings.t, ~ctx_init, z: Zipper.t): Statics.Map.t => {
  z
  |> MakeTerm.from_zip_for_sem
  |> fst
  |> Interface.Statics.mk_map_ctx(settings.core, ctx_init);
};
let get_info_and_top_ci_from_zipper =
    (~ctx, z: Zipper.t): (option(Info.exp), Statics.Map.t) => {
  z
  |> MakeTerm.from_zip_for_sem
  |> fst
  |> Interface.Statics.mk_map_and_info_ctx(CoreSettings.on, ctx);
};

let get_ci =
    (~settings: Settings.t, ~ctx_init, editor: Editor.t): option(Info.t) => {
  let z = editor.state.zipper;
  let index = Indicated.index(z);
  switch (index) {
  | Some(index) =>
    let map = get_info_from_zipper(~settings, ~ctx_init, z);
    Haz3lcore.Id.Map.find_opt(index, map);
  | _ => None
  };
};

module Type = {
  let mode =
      (~settings: Settings.t, ~ctx_init, editor: Editor.t): option(Mode.t) =>
    switch (get_ci(~settings, ~ctx_init, editor)) {
    | Some(InfoExp({mode, _})) => Some(mode)
    | Some(InfoPat({mode, _})) => Some(mode)
    | _ => None
    };

  let ctx =
      (~settings: Settings.t, ~ctx_init, editor: Editor.t): option(Ctx.t) =>
    switch (get_ci(~settings, ~ctx_init, editor)) {
    | Some(ci) => Some(Info.ctx_of(ci))
    | _ => None
    };

  let expected_ty_no_lookup = (mode: option(Mode.t)): Typ.t => {
    switch (mode) {
    | Some(Ana(ty)) => ty
    | Some(SynFun) => Arrow(Unknown(Internal), Unknown(Internal))
    | Some(Syn)
    | None => Unknown(SynSwitch)
    };
  };

  let expected_ty = (~ctx, mode: option(Mode.t)): Typ.t => {
    switch (mode) {
    | Some(Ana(Var(name) as _ty)) when Ctx.lookup_alias(ctx, name) != None =>
      let ty_expanded = Ctx.lookup_alias(ctx, name) |> Option.get;
      ty_expanded;
    | _ => expected_ty_no_lookup(mode)
    };
  };

  let format_def = (alias: string, ty: Typ.t): string => {
    Printf.sprintf("type %s = %s in", alias, Typ.to_string(ty));
  };

  let subst_if_rec = ((name: TypVar.t, ty: Typ.t)) => {
    switch (ty) {
    | Rec(name', ty') => (name, Typ.subst(Var(name), name', ty'))
    | _ => (name, ty)
    };
  };

  let collate_aliases = (ctx, expected_ty'): option(string) => {
    let defs =
      Ctx.collect_aliases_deep(ctx, expected_ty')
      |> List.map(subst_if_rec)
      |> List.map(((alias, ty)) => format_def(alias, ty));
    switch (defs) {
    | [] => None
    | _ => Some(defs |> String.concat("\n"))
    };
  };

  let expected = (~ctx, mode: option(Mode.t)): string => {
    /*
     TODO(andrew): maybe include more than just the immediate type.
     like for example, when inside a fn(s), include
     argument types.
     like basically to benefit maximally from included type info,
     want to make sure we're including the full expansion of any type
     we might want to either case on or construct.
     expected type should mostly(?) give us the latter,
     but not always the former
     */
    let prefix = "Hole ?? can be filled by an expression with ";
    switch (mode) {
    | Some(Ana(ty)) =>
      let defs =
        switch (collate_aliases(ctx, expected_ty_no_lookup(mode))) {
        | Some(defs) =>
          " which references the following definitions:\n" ++ defs
        | None => "\n"
        };
      prefix ++ "a type consistent with " ++ Typ.to_string(ty) ++ defs;
    | Some(SynFun) =>
      prefix
      ++ "a type consistent with "
      ++ Typ.to_string(Arrow(Unknown(Internal), Unknown(Internal)))
    | Some(Syn) => prefix ++ "any type"
    | _ => "Not applicable"
    };
  };
};

module Errors = Haz3lcore.ErrorPrint;
