open Util_web;
open Language;
open Language.Statics;
open HighLevelNodeMap.Utils;

let subtree_of =
    (
      ~info: Info.t,
      ~orig_info_map: Id.Map.t(Info.t),
      ~of_pat: bool,
      ~of_def: bool,
      ~of_body: bool,
    )
    : Statics.Map.t => {
  let map =
    switch (info) {
    | InfoExp({user_term: term, _}) =>
      switch (Exp.term_of(term)) {
      | Let(pat, def, body) =>
        let pat_info = pat_to_pat(pat, orig_info_map);
        let def_info = exp_to_exp(def, orig_info_map);
        let body_info = exp_to_exp(body, orig_info_map);

        let pat_map =
          of_pat
            ? {
              let (_, _, m) =
                Statics.upat_to_info_map(
                  ~is_synswitch=false,
                  ~ctx=pat_info.ctx,
                  ~co_ctx=pat_info.co_ctx,
                  ~ana=pat_info.ana,
                  ~ancestors=pat_info.ancestors,
                  ~duplicate_bindings=[],
                  pat,
                  Statics.Map.empty,
                );
              m;
            }
            : Statics.Map.empty;

        let def_map =
          of_def
            ? {
              let (_, _, m) =
                Statics.uexp_to_info_map(
                  ~ctx=def_info.ctx,
                  ~ana=def_info.ana,
                  ~is_in_filter=false,
                  ~ancestors=def_info.ancestors,
                  def,
                  pat_map,
                );
              m;
            }
            : pat_map;

        let body_map =
          of_body
            ? {
              let (_, _, m) =
                Statics.uexp_to_info_map(
                  ~ctx=body_info.ctx,
                  ~ana=body_info.ana,
                  ~is_in_filter=false,
                  ~ancestors=body_info.ancestors,
                  body,
                  def_map,
                );
              m;
            }
            : def_map;

        body_map;

      | TyAlias(tpat, tdef, body) =>
        let tpat_info = tpat_to_tpat(tpat, orig_info_map);
        let tdef_info = typ_to_typ(tdef, orig_info_map);
        let body_info = exp_to_exp(body, orig_info_map);

        let tpat_map =
          of_pat
            ? Statics.utpat_to_info_map(
                ~ctx=tpat_info.ctx,
                ~ancestors=tpat_info.ancestors,
                tpat,
                Statics.Map.empty,
              )
              |> snd
            : Statics.Map.empty;
        let tpat_map =
          of_def
            ? Statics.utyp_to_info_map(
                ~ctx=tdef_info.ctx,
                ~ancestors=tdef_info.ancestors,
                tdef,
                tpat_map,
              )
              |> snd
            : tpat_map;

        let body_map =
          of_body
            ? {
              let (_, _, m) =
                Statics.uexp_to_info_map(
                  ~ctx=body_info.ctx,
                  ~ana=body_info.ana,
                  ~is_in_filter=false,
                  ~ancestors=body_info.ancestors,
                  body,
                  tpat_map,
                );
              m;
            }
            : tpat_map;

        body_map;

      | ModuleExp(_, def, body) =>
        let def_info = exp_to_exp(def, orig_info_map);
        let body_info = exp_to_exp(body, orig_info_map);

        /* ModuleExp is expanded to Let in Statics; the MPat gets the same id as the
           expanded Pat, so the map has InfoPat not InfoMPat. We don't need to add
           the MPat - def_info.ctx already has the correct context. */
        let mp_map = Statics.Map.empty;

        let def_map =
          of_def
            ? {
              let (_, _, m) =
                Statics.uexp_to_info_map(
                  ~ctx=def_info.ctx,
                  ~ana=def_info.ana,
                  ~is_in_filter=false,
                  ~ancestors=def_info.ancestors,
                  def,
                  mp_map,
                );
              m;
            }
            : mp_map;

        let body_map =
          of_body
            ? {
              let (_, _, m) =
                Statics.uexp_to_info_map(
                  ~ctx=body_info.ctx,
                  ~ana=body_info.ana,
                  ~is_in_filter=false,
                  ~ancestors=body_info.ancestors,
                  body,
                  def_map,
                );
              m;
            }
            : def_map;

        body_map;

      | _ =>
        raise(
          Failure(
            "UNIMPLEMENTED_NODE_TYPE: Only let, type alias, and module expressions are currently supported as nodes",
          ),
        )
      }
    | _ => raise(Failure("Current node is not an expression"))
    };

  map;
};

let get_refs_to = (curr: Info.t, info_map: Id.Map.t(Info.t)): CoCtx.t => {
  /*
   Returns the CoCtx containing exclusively references to the given let/tyalis expression
   */

  let exp_to_info = (term: Exp.t): Info.t => exp_to_info(term, info_map);

  switch (curr) {
  | InfoExp(info) =>
    let entire_coctx = info.co_ctx;
    let body_coctx =
      switch (Exp.term_of(info.user_term)) {
      | Let(_, _, body)
      | TyAlias(_, _, body)
      | ModuleExp(_, _, body) =>
        switch (exp_to_info(body)) {
        | InfoExp({co_ctx, _}) => co_ctx
        | _ =>
          raise(
            Failure("Body of let/type alias/module is not an expression"),
          )
        }
      | _ =>
        raise(
          Failure(
            "Current node is not a let, type alias, or module expression",
          ),
        )
      };
    // Find variables that appear in body_coctx but not in entire_coctx
    // Effectively takes the set difference of body_coctx and entire_coctx
    VarMap.filter(
      ((var_name, _)) => !VarMap.contains(entire_coctx, var_name),
      body_coctx,
    );
  | _ =>
    raise(
      Failure("Current node is not a let, type alias, or module expression"),
    )
  };
};

let rec var_names_of_pat = (pat: Pat.t): list(string) => {
  switch (pat.term) {
  | Var(name) => [name]
  | Ap(pat1, pat2)
  | TupLabel(pat1, pat2)
  | Cons(pat1, pat2) => var_names_of_pat(pat1) @ var_names_of_pat(pat2)
  | Parens(pat)
  | Asc(pat, _) => var_names_of_pat(pat)
  | ListLit(pats)
  | Tuple(pats) => List.concat_map(var_names_of_pat, pats)
  | Invalid(_)
  | EmptyHole
  | MultiHole(_)
  | Wild
  | Atom(_)
  | Constructor(_, _)
  | Label(_)
  | Projector(_, _)
  | ExplicitNonlabel => []
  };
};

/** After a pattern edit, the post-edit [[Let]]'s [[co_ctx]] can already treat
    stale body spellings (old pattern name) as outer/free, so they wrongly
    survive the [[entire_coctx]] filter in [[get_refs_to]]. Use the pre-edit
    [[Let]]'s [[co_ctx]] for that filter, while still taking the body from the
    post-edit term and resolving it in the current [[info_map]]. */
let get_refs_to_after_pattern_edit =
    (
      ~pre_edit_let_info: Info.t,
      ~post_edit_let_info: Info.t,
      info_map: Id.Map.t(Info.t),
    )
    : CoCtx.t => {
  let exp_to_info = (term: Exp.t): Info.t => exp_to_info(term, info_map);
  switch (pre_edit_let_info, post_edit_let_info) {
  | (InfoExp(pre_let), InfoExp(term_after)) =>
    let entire_coctx = pre_let.co_ctx;
    let body_coctx =
      switch (Exp.term_of(term_after.user_term)) {
      | Let(_, _, body)
      | TyAlias(_, _, body)
      | ModuleExp(_, _, body) =>
        switch (exp_to_info(body)) {
        | InfoExp({co_ctx, _}) => co_ctx
        | _ =>
          raise(
            Failure("Body of let/type alias/module is not an expression"),
          )
        }
      | _ =>
        raise(
          Failure(
            "Current node is not a let, type alias, or module expression",
          ),
        )
      };
    /* Fn-sugar (`let f(x) = def`) scopes f AND the params over the def (it
       desugars to a recursive `let f = fun x -> def`), so recursive calls
       and param uses live in the def's co_ctx; over the body only f is
       bound. Applied only when old and new patterns are both sugar — for
       plain lets the pattern does not scope over the def. Note the
       whole-let co_ctx unions in the def's co_ctx unfiltered, so for
       recursive bindings the entire_coctx filter below would wrongly drop
       the body's refs to f. */
    let sugar_head_name = (p: Pat.t): option(string) =>
      switch (FunctionSugar.detect(p)) {
      | Some((f_name, _, _)) =>
        switch (Pat.term_of(f_name)) {
        | Var(n) => Some(n)
        | _ => None
        }
      | None => None
      };
    switch (
      Exp.term_of(pre_let.user_term),
      Exp.term_of(term_after.user_term),
    ) {
    | (Let(p_old, _, _), Let(p_new, def, _))
        when
          Option.is_some(sugar_head_name(p_old))
          && Option.is_some(FunctionSugar.detect(p_new)) =>
      let old_names = var_names_of_pat(p_old);
      let head_name = Option.get(sugar_head_name(p_old));
      let def_refs =
        switch (exp_to_info(def)) {
        | InfoExp({co_ctx, _}) =>
          VarMap.filter(((n, _)) => List.mem(n, old_names), co_ctx)
        | _ => []
        };
      let body_refs =
        VarMap.filter(((n, _)) => String.equal(n, head_name), body_coctx);
      def_refs @ body_refs;
    | _ =>
      VarMap.filter(
        ((var_name, _)) => !VarMap.contains(entire_coctx, var_name),
        body_coctx,
      )
    };
  | _ =>
    raise(
      Failure(
        "get_refs_to_after_pattern_edit: expected expression infos for both arguments",
      ),
    )
  };
};

let get_var_names_from_pat = (curr: Info.t): list(string) => {
  switch (curr) {
  | InfoPat({user_term: term, _}) => var_names_of_pat(term)
  | _ => raise(Failure("Pat is not a pattern"))
  };
};

/** True iff [name] occurs as an expression variable or a pattern binder
    within the subtree rooted at the term with [root_id] (per statics ancestor
    lists). Conservative capture check for renames: any occurrence in scope —
    even one already shadowed locally — counts. */
let name_occurs_within =
    (~root_id: Id.t, ~info_map: Id.Map.t(Info.t), name: string): bool => {
  Id.Map.exists(
    (_id, info: Info.t) =>
      List.mem(root_id, Info.ancestors_of(info))
      && (
        switch (info) {
        | InfoExp({user_term, _}) =>
          switch (Exp.term_of(user_term)) {
          | Var(n) => String.equal(n, name)
          | _ => false
          }
        | InfoPat({user_term, _}) =>
          switch (Pat.term_of(user_term)) {
          | Var(n) => String.equal(n, name)
          | _ => false
          }
        | _ => false
        }
      ),
    info_map,
  );
};

let update_use_sites_of_var =
    (z: Zipper.t, co_ctx: CoCtx.t, old_name: string, new_name: string)
    : Zipper.t => {
  /*
   Updates the use sites of the given variables in the co-context.
   */
  // Iterate through all variables in the co-context
  List.fold_left(
    (acc_z, (var_name, entries)) =>
      // Only update variables that match the old_name
      if (var_name == old_name) {
        // Iterate through all entries (IDs) for this variable
        List.fold_left(
          (acc_z', entry) => {
            let id = entry.CoCtx.id;
            switch (Select.tile(id, acc_z')) {
            | Some(z') =>
              switch (Parser.to_zipper(~root=Exp, ~zipper_init=z', new_name)) {
              | Some(z'') => z''
              | None => z'
              }
            | None => acc_z'
            };
          },
          acc_z,
          entries,
        );
      } else {
        acc_z;
      },
    z,
    co_ctx,
  );
};

let update_use_sites_of_pat =
    (
      ~z: Zipper.t,
      ~co_ctx: CoCtx.t,
      ~old_names: list(string),
      ~new_names: list(string),
    )
    : Zipper.t =>
  /*
   Updates the use sites of the given variables in the co-context.

   When old/new bind different numbers of names (e.g. (x, y, z) -> (a, b)) we
   cannot determine which new var maps to which old var; callers must reject
   that case up front (see the Update(Pattern) arm in [[CompositionGo]]), so
   hitting it here is a hard error rather than a silent no-op.
   */
  switch (ListUtil.opt_zip(old_names, new_names)) {
  | None =>
    raise(
      Failure(
        "Cannot rewrite use sites: the old pattern binds "
        ++ string_of_int(List.length(old_names))
        ++ " name(s), the new pattern binds "
        ++ string_of_int(List.length(new_names))
        ++ ".",
      ),
    )
  | Some(pairs) =>
    List.fold_left(
      (acc_z, (old_name, new_name)) =>
        update_use_sites_of_var(acc_z, co_ctx, old_name, new_name),
      z,
      pairs,
    )
  };
