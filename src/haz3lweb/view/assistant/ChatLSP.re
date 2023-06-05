open Haz3lcore;

//TODO(andrew): calculate this in a more principled way
let get_info_from_zipper = (~ctx=Ctx.empty, z: Zipper.t): Statics.Map.t => {
  z |> MakeTerm.from_zip_for_sem |> fst |> Statics.mk_map_ctx(ctx);
};
let get_info_and_top_ci_from_zipper =
    (~ctx=Ctx.empty, z: Zipper.t): (Info.exp, Statics.Map.t) => {
  z |> MakeTerm.from_zip_for_sem |> fst |> Statics.mk_map_and_info_ctx(ctx);
};

let get_ci = (editor: Editor.t): option(Info.t) => {
  let z = editor.state.zipper;
  let index = Indicated.index(z);
  switch (index) {
  | Some(index) =>
    let map = get_info_from_zipper(z);
    Haz3lcore.Id.Map.find_opt(index, map);
  | _ => None
  };
};

module Type = {
  let mode = (editor: Editor.t): option(Typ.mode) =>
    switch (get_ci(editor)) {
    | Some(InfoExp({mode, _})) => Some(mode)
    | Some(InfoPat({mode, _})) => Some(mode)
    | _ => None
    };

  let ctx = (editor: Editor.t): option(Ctx.t) =>
    switch (get_ci(editor)) {
    | Some(ci) => Some(Info.ctx_of(ci))
    | _ => None
    };

  let expected = (~ctx=Ctx.empty, mode: option(Typ.mode)): string => {
    let prefix = "Hole ?? can be filled by an expression with ";
    switch (mode) {
    | Some(Ana(Var(name) as ty)) when Ctx.lookup_alias(ctx, name) != None =>
      let ty_expanded = Ctx.lookup_alias(ctx, name) |> Option.get;
      prefix
      ++ "a type consistent with "
      ++ Typ.to_string(ty)
      ++ " which is a type alias for "
      ++ Typ.to_string(ty_expanded);
    | Some(Ana(ty)) =>
      prefix ++ "a type consistent with " ++ Typ.to_string(ty)
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
    | MultiError =>
      prn("Incomplete syntax (maybe missing operator, function parens)")

    | BadToken(token) => prn("\"%s\" isn't a valid token", token)
    | InconsistentWithArrow(typ) =>
      prn("type %s is not consistent with arrow type", Typ.to_string(typ))
    | FreeTag => prn("Constructor is not defined")
    | SynInconsistentBranches(tys) =>
      prn(
        "Expecting branches to have consistent types but got types: %s",
        List.map(Typ.to_string, tys) |> String.concat(", "),
      )
    | TypeInconsistent({ana, syn}) =>
      prn(
        "Expecting type %s but got inconsistent type %s",
        Typ.to_string(ana),
        Typ.to_string(syn),
      );

  let exp_error: Info.error_exp => string =
    fun
    | FreeVariable => "Variable is not bound"
    | Common(error) => common_error(error);

  let pat_error: Info.error_pat => string =
    fun
    | ExpectedTag => "Expected a constructor"
    | Common(error) => common_error(error);

  let typ_error: Info.error_typ => string =
    fun
    | FreeTypeVar(name) => prn("Type variable %s is not bound", name)
    | BadToken(token) => prn("\"%s\" isn't a valid type token", token)
    | WantTagFoundAp => "Expected a constructor, found application"
    | WantTagFoundType(ty) =>
      prn("Expected a constructor, found type %s", Typ.to_string(ty))
    | WantTypeFoundAp => "Constructor application must be in sum"
    | DuplicateTag(name) =>
      prn("Constructor %s already used in this sum", name);

  let tpat_error: Info.error_tpat => string =
    fun
    | NotAVar => "Not a valid type name"
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
      ((id: Id.t, error: Info.error)) => {
        let term = Info.term_string_of(Id.Map.find(id, info_map));
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
