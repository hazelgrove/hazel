open Haz3lcore;
open Sexplib.Std;

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
      |> Util.ListUtil.dedup
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
    let prefix = "# The expected type of the hole ?? is: ";
    switch (mode) {
    | Some(Ana(ty)) =>
      let defs =
        switch (collate_aliases(ctx, expected_ty_no_lookup(mode))) {
        | Some(defs) =>
          "# The following type definitions are likely relevant: #\n" ++ defs
        | None => "\n"
        };
      prefix
      ++ "a type consistent with "
      ++ Typ.to_string(ty)
      ++ " #\n"
      ++ defs;
    | Some(SynFun) =>
      prefix
      ++ "a type consistent with "
      ++ Typ.to_string(Arrow(Unknown(Internal), Unknown(Internal)))
      ++ " #"
    | Some(Syn) => prefix ++ "any type #"
    | _ => "Not applicable"
    };
  };
};

module Errors = Haz3lcore.ErrorPrint;

module RelevantCtx = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type filtered_entry = {
    name: string,
    typ: Typ.t,
    matched_type: Typ.t,
    depth: int,
  };

  let is_list_unk = (ty: Typ.t) =>
    switch (ty) {
    | List(Unknown(_)) => true
    | _ => false
    };

  let returns_base = (ty: Typ.t) =>
    switch (ty) {
    | Arrow(_, ty) => Typ.is_base(ty)
    | _ => false
    };

  let score_type = (ty: Typ.t) => {
    let unk_ratio = Typ.unknown_ratio(ty);
    Typ.is_base(ty) ? 0.8 : unk_ratio;
  };

  let take_up_to_n = (n, xs) =>
    switch (Util.ListUtil.split_n_opt(n, xs)) {
    | Some((xs, _)) => xs
    | None => xs
    };

  let format_def = (name: string, ty: Typ.t) =>
    Printf.sprintf("let %s: %s =  in", name, Typ.to_string(ty));

  let filter_ctx = (ctx: Ctx.t, ty_expect: Typ.t): list(filtered_entry) =>
    List.filter_map(
      fun
      | Ctx.VarEntry({typ, name, _})
          when Typ.is_consistent(ctx, ty_expect, typ) =>
        Some({name, typ, depth: 0, matched_type: typ})
      | Ctx.VarEntry({typ: Arrow(_, return_ty) as typ, name, _})
          when Typ.is_consistent(ctx, ty_expect, return_ty) =>
        Some({name, typ, matched_type: return_ty, depth: 1})
      | Ctx.VarEntry({typ: Arrow(_, Arrow(_, return_ty)) as typ, name, _})
          when Typ.is_consistent(ctx, ty_expect, return_ty) =>
        Some({name, typ, matched_type: return_ty, depth: 2})
      | _ => None,
      ctx,
    );

  let str = (ctx: Ctx.t, mode: Mode.t): string => {
    let primary_goal: Typ.t =
      Type.expected_ty(~ctx, Some(mode)) |> Typ.normalize(ctx);
    let secondary_targets =
      switch (primary_goal) {
      | Arrow(_source, target) =>
        let terts =
          switch (target) {
          | Prod(ts) => ts
          | _ => []
          };
        [target] @ terts;
      | _ => []
      };
    let primary_entries = filter_ctx(ctx, primary_goal);
    let secondary_entries =
      List.concat(List.map(filter_ctx(ctx, _), secondary_targets));
    let combined_entries =
      secondary_entries
      @ primary_entries
      |> Util.ListUtil.dedup
      |> List.sort((t1, t2) =>
           compare(score_type(t2.matched_type), score_type(t1.matched_type))
         )
      |> List.filter(entry => Typ.contains_sum_or_var(entry.typ));
    // List.iter(
    //   fun
    //   | {name, typ, depth, matched_type} =>
    //     Printf.sprintf(
    //       "%s: %s; depth: %d; score: %f",
    //       name,
    //       Typ.show(typ),
    //       depth,
    //       score_type(matched_type),
    //     )
    //     |> print_endline,
    //   combined_entries,
    // );
    let entries =
      combined_entries
      |> take_up_to_n(8)
      |> List.map(({name, typ, _}) => format_def(name, typ))
      |> String.concat("\n");
    "# Consider using these variables relevant to the expected type: #\n"
    ++ entries;
  };
};
