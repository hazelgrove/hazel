open Util;
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
    | InfoExp({term, _}) =>
      switch (Exp.term_of(term)) {
      | Let(pat, def, body) =>
        let pat_info = pat_to_pat(pat, orig_info_map);
        let def_info = exp_to_exp(def, orig_info_map);
        let body_info = exp_to_exp(body, orig_info_map);

        let pat_map =
          of_pat
            ? Statics.upat_to_info_map(
                ~is_synswitch=false,
                ~ctx=pat_info.ctx,
                ~co_ctx=pat_info.co_ctx,
                ~ana=pat_info.ana,
                ~ancestors=pat_info.ancestors,
                ~duplicates=[],
                pat,
                Statics.Map.empty,
              )
              |> snd
            : Statics.Map.empty;

        let def_map =
          of_def
            ? Statics.uexp_to_info_map(
                ~ctx=def_info.ctx,
                ~ana=def_info.ana,
                ~is_in_filter=false,
                ~ancestors=def_info.ancestors,
                ~duplicates=[],
                ~expected_labels=None,
                def,
                pat_map,
              )
              |> snd
            : pat_map;

        let body_map =
          of_body
            ? Statics.uexp_to_info_map(
                ~ctx=body_info.ctx,
                ~ana=body_info.ana,
                ~is_in_filter=false,
                ~ancestors=body_info.ancestors,
                ~duplicates=[],
                ~expected_labels=None,
                body,
                def_map,
              )
              |> snd
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
            ? Statics.uexp_to_info_map(
                ~ctx=body_info.ctx,
                ~ana=body_info.ana,
                ~is_in_filter=false,
                ~ancestors=body_info.ancestors,
                ~duplicates=[],
                ~expected_labels=None,
                body,
                tpat_map,
              )
              |> snd
            : tpat_map;

        body_map;

      | _ =>
        raise(
          Failure(
            "UNIMPLEMENTED_NODE_TYPE: Only let and type alias expressions are currently supported as nodes",
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
  | InfoExp(term) =>
    let entire_coctx = term.co_ctx;
    let body_coctx =
      switch (Exp.term_of(term.term)) {
      | Let(_, _, body)
      | TyAlias(_, _, body) =>
        switch (exp_to_info(body)) {
        | InfoExp({co_ctx, _}) => co_ctx
        | _ => raise(Failure("Body of type alias is not an expression"))
        }
      | _ =>
        raise(Failure("Current node is not a let or type alias expression"))
      };
    // Find variables that appear in body_coctx but not in entire_coctx
    // Effectively takes the set difference of body_coctx and entire_coctx
    VarMap.filter(
      ((var_name, _)) => !VarMap.contains(entire_coctx, var_name),
      body_coctx,
    );
  | _ => raise(Failure("Current node is not a let or type alias expression"))
  };
};

let get_var_names_from_pat = (curr: Info.t): list(string) => {
  let rec go = (pat: Pat.t, vars: list(string)): list(string) => {
    switch (pat.term) {
    | Var(name) => vars @ [name]
    | Ap(pat1, pat2)
    | TupLabel(pat1, pat2)
    | Cons(pat1, pat2) => go(pat1, vars) @ go(pat2, vars)
    | Parens(pat)
    | Probe(pat, _)
    | Asc(pat, _) => go(pat, vars)
    | ListLit(pats)
    | Tuple(pats) =>
      List.fold_left((vars, pat) => go(pat, vars), vars, pats)
    | Invalid(_)
    | EmptyHole
    | MultiHole(_)
    | Wild
    | Atom(_)
    | Constructor(_, _)
    | Label(_) => vars
    };
  };
  let pat =
    switch (curr) {
    | InfoPat({term, _}) => term
    | _ => raise(Failure("Pat is not a pattern"))
    };
  go(pat, []);
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
              switch (Parser.to_zipper(~zipper_init=z', new_name)) {
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

   Should be noted that there are special cases:
   - Consider updating the pattern (x, y, z) to (a, b)
     we are unable to determine which new var maps to which old var.
     For now, we will only consider the case where the number of old and new vars are the same.
   */
  switch (ListUtil.opt_zip(old_names, new_names)) {
  | None => z
  | Some(pairs) =>
    List.fold_left(
      (acc_z, (old_name, new_name)) =>
        update_use_sites_of_var(acc_z, co_ctx, old_name, new_name),
      z,
      pairs,
    )
  };
