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
    (~settings: Settings.t, ~ctx, z: Zipper.t)
    : (option(Info.exp), Statics.Map.t) => {
  z
  |> MakeTerm.from_zip_for_sem
  |> fst
  |> Interface.Statics.mk_map_and_info_ctx(settings.core, ctx);
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

/*
 ERRORS TODO:
 make multihole an error (say something about ap)
 do a completeness check
  */

module Errors = {
  let prn = Printf.sprintf;

  let common_error: Info.error_common => string =
    fun
    | NoType(MultiError) =>
      /* NOTE: possible cause explanation actually helps.
         e.g. when generating
         "if i == index then (description, not(done)) else (description, done)"
         it would tend not to parethesize the argument to not
          */
      prn(
        "Incomplete syntax (possible cause: remember that function application is c-style and requires parentheses around the argument)",
      )

    | NoType(BadToken(token)) => prn("\"%s\" isn't a valid token", token)
    | NoType(BadTrivAp(ty)) =>
      prn(
        "Function argument type \"%s\" inconsistent with ()",
        Typ.to_string(ty),
      )
    | Inconsistent(WithArrow(ty)) =>
      prn("type %s is not consistent with arrow type", Typ.to_string(ty))
    | NoType(FreeConstructor(_name)) => prn("Constructor is not defined")
    | Inconsistent(Internal(tys)) =>
      prn(
        "Expecting branches to have consistent types but got types: %s",
        List.map(Typ.to_string, tys) |> String.concat(", "),
      )
    | Inconsistent(Expectation({ana, syn})) =>
      prn(
        "Expecting type %s but got inconsistent type %s",
        Typ.to_string(ana),
        Typ.to_string(syn),
      );

  let exp_error: Info.error_exp => string =
    fun
    | FreeVariable(name) => "Variable " ++ name ++ " is not bound"
    | Common(error) => common_error(error);

  let pat_error: Info.error_pat => string =
    fun
    | ExpectedConstructor => "Expected a constructor"
    | Common(error) => common_error(error);

  let typ_error: Info.error_typ => string =
    fun
    | FreeTypeVariable(name) => prn("Type variable %s is not bound", name)
    | BadToken(token) => prn("\"%s\" isn't a valid type token", token)
    | WantConstructorFoundAp => "Expected a constructor, found application"
    | WantConstructorFoundType(ty) =>
      prn("Expected a constructor, found type %s", Typ.to_string(ty))
    | WantTypeFoundAp => "Constructor application must be in sum"
    | DuplicateConstructor(name) =>
      prn("Constructor %s already used in this sum", name);

  let tpat_error: Info.error_tpat => string =
    fun
    | NotAVar(_) => "Not a valid type name"
    | ShadowsType(name) => "Can't shadow base type " ++ name;

  let string_of: Info.error => string =
    fun
    | Exp(error) => exp_error(error)
    | Pat(error) => pat_error(error)
    | Typ(error) => typ_error(error)
    | TPat(error) => tpat_error(error);

  let format_error = (term, error) =>
    prn("Error in term:\n  %s\nNature of error: %s", term, error);

  let collect_static = (info_map: Statics.Map.t) => {
    let errors = Statics.collect_errors(info_map);
    List.map(
      ((id: Id.t, error: Info.error)) =>
        switch (Id.Map.find_opt(id, info_map)) {
        | None => "Can't report error: Id lookup failed"
        | Some(info) =>
          let term = Info.term_string_of(info);
          format_error(term, string_of(error));
        },
      errors,
    );
  };
  /*
   Id.Map.fold(
     (_id, info: Info.t, acc) => {
       switch (Info.error_of(info)) {
       | Some(error) =>
         let term = Info.term_string_of(info);
         List.cons(format_error(term, string_of(error)), acc);
       | None => acc
       }
     },
     info_map,
     [],
   );*/
};
