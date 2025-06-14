/*
 A nice property would be that elaboration is idempotent...
 */

open Util;
open Sets;

exception MissingTypeInfo;

module ElaborationResult = {
  [@deriving sexp]
  type t =
    | Elaborates(DHExp.t, Typ.t, Delta.t)
    | DoesNotElaborate;
};

let fresh_cast = (d: DHExp.t, t1: Typ.t, t2: Typ.t): Exp.t => {
  switch (d.term) {
  | Label(_) => d
  | _ =>
    Typ.eq(t1, t2)
      ? d
      : {
        let d': Exp.t =
          (Cast(d, t1, Typ.temp(Unknown(Internal))): Exp.term)
          |> IdTagged.fresh_deterministic(DHExp.rep_id(d))
          |> Casts.transition_multiple;
        (Cast(d', Typ.temp(Unknown(Internal)), t2): Exp.term)
        |> IdTagged.fresh_deterministic(DHExp.rep_id(d'))
        |> Casts.transition_multiple;
      }
  };
};

let fresh_pat_cast = (p: DHPat.t, t1: Typ.t, t2: Typ.t): DHPat.t => {
  switch (p.term) {
  | Label(_) => p
  | _ =>
    Typ.eq(t1, t2)
      ? p
      : {
        Cast(
          DHPat.fresh(Cast(p, t1, Typ.temp(Unknown(Internal))))
          |> Casts.pattern_fixup,
          Typ.temp(Unknown(Internal)),
          t2,
        )
        |> DHPat.fresh
        |> Casts.pattern_fixup;
      }
  };
};

let elaborated_type =
    (m: Statics.Map.t, uexp: Exp.t): (Typ.t, Ctx.t, CoCtx.t, Exp.t) => {
  let (mode, self_ty, ctx, co_ctx, term) =
    switch (Id.Map.find_opt(Exp.rep_id(uexp), m)) {
    | Some(Info.InfoExp({mode, ty, ctx, co_ctx, term, _})) => (
        mode,
        ty,
        ctx,
        co_ctx,
        term,
      )
    | _ => raise(MissingTypeInfo)
    };
  let elab_ty =
    switch (mode) {
    | Syn => self_ty
    | SynFun =>
      let (ty1, ty2) = Typ.matched_arrow(ctx, self_ty);
      Arrow(ty1, ty2) |> Typ.temp;
    | SynTypFun =>
      let (tpat, ty) = Typ.matched_forall(ctx, self_ty);
      let tpat = Option.value(tpat, ~default=TPat.fresh(EmptyHole));
      Forall(tpat, ty) |> Typ.temp;
    // We need to remove the synswitches from this type.
    | Ana(ana_ty) => Typ.match_synswitch(ana_ty, self_ty)
    };
  (elab_ty |> Typ.normalize(ctx) |> Typ.all_ids_temp, ctx, co_ctx, term);
};

let elaborated_pat_type =
    (m: Statics.Map.t, upat: Pat.t): (Typ.t, Ctx.t, Pat.t) => {
  let (mode, self_ty, ctx, prev_synswitch, term, label_inference) =
    switch (Id.Map.find_opt(Pat.rep_id(upat), m)) {
    | Some(
        Info.InfoPat({
          mode,
          ty,
          ctx,
          prev_synswitch,
          term,
          label_inference,
          _,
        }),
      ) => (
        mode,
        ty,
        ctx,
        prev_synswitch,
        term,
        label_inference,
      )
    | _ => raise(MissingTypeInfo)
    };
  let elab_ty =
    switch (mode) {
    | Syn => self_ty
    | SynFun =>
      let (ty1, ty2) = Typ.matched_arrow(ctx, self_ty);
      Arrow(ty1, ty2) |> Typ.temp;
    | SynTypFun =>
      let (tpat, ty) = Typ.matched_forall(ctx, self_ty);
      let tpat = Option.value(tpat, ~default=TPat.fresh(EmptyHole));
      Forall(tpat, ty) |> Typ.temp;
    | Ana(ana_ty) =>
      switch (prev_synswitch) {
      | None => ana_ty
      | Some(syn_ty) =>
        // Autolabelling for singleton labeled tuples
        switch (label_inference) {
        | Some(SingletonLabelInference({label: l, _})) =>
          Typ.match_synswitch(
            Prod([TupLabel(Label(l) |> Typ.temp, syn_ty) |> Typ.temp])
            |> Typ.temp,
            ana_ty,
          )
        | _ => Typ.match_synswitch(syn_ty, ana_ty)
        }
      }
    };
  (elab_ty |> Typ.normalize(ctx) |> Typ.all_ids_temp, ctx, term);
};

let rec elaborate_pattern =
        (m: Statics.Map.t, upat: Pat.t, in_container: bool): (Pat.t, Typ.t) => {
  // Pulling upat back out of the statics map for statics level singleton tuple autolabeling
  let (elaborated_type, ctx, upat) = elaborated_pat_type(m, upat);
  let elaborate_pattern = (~in_container=false, m, upat) =>
    elaborate_pattern(m, upat, in_container);
  let cast_from = (ty, exp) => fresh_pat_cast(exp, ty, elaborated_type);
  let (term, rewrap) = Pat.unwrap(upat);
  let dpat =
    switch (term) {
    | Int(_) => upat |> cast_from(Int |> Typ.temp)
    | Bool(_) => upat |> cast_from(Bool |> Typ.temp)
    | Float(_) => upat |> cast_from(Float |> Typ.temp)
    | String(_) => upat |> cast_from(String |> Typ.temp)
    | ListLit(ps) =>
      let (ps, tys) = List.map(elaborate_pattern(m), ps) |> ListUtil.unzip;
      let inner_type =
        tys
        |> Typ.join_all(~empty=Unknown(Internal) |> Typ.temp, ctx)
        |> Option.value(~default=Typ.temp(Unknown(Internal)));
      ps
      |> List.map2((p, t) => fresh_pat_cast(p, t, inner_type), _, tys)
      |> (
        ps' =>
          ListLit(ps') |> rewrap |> cast_from(List(inner_type) |> Typ.temp)
      );
    | Cons(p1, p2) =>
      let (p1', ty1) = elaborate_pattern(m, p1);
      let (p2', ty2) = elaborate_pattern(m, p2);
      let ty2_inner = Typ.matched_list(ctx, ty2);
      let ty_inner =
        Typ.join(ctx, ty1, ty2_inner)
        |> Option.value(~default=Typ.temp(Unknown(Internal)));
      let p1'' = fresh_pat_cast(p1', ty1, ty_inner);
      let p2'' = fresh_pat_cast(p2', ty2, List(ty_inner) |> Typ.temp);
      Cons(p1'', p2'') |> rewrap |> cast_from(List(ty_inner) |> Typ.temp);
    | TupLabel(lab, p) =>
      let (plab, labty) = elaborate_pattern(m, lab);
      let (p', pty) = elaborate_pattern(m, p);
      if (in_container) {
        TupLabel(plab, p')
        |> rewrap
        |> cast_from(TupLabel(labty, pty) |> Typ.temp);
      } else {
        Tuple([TupLabel(plab, p') |> rewrap])
        |> DHPat.fresh
        |> cast_from(Prod([TupLabel(labty, pty) |> Typ.temp]) |> Typ.temp);
      };
    | Tuple(ps) =>
      let (ps', tys) =
        List.map(elaborate_pattern(m, ~in_container=true), ps)
        |> ListUtil.unzip;
      let expected_labels: list(option(string)) =
        Typ.get_labels(ctx, elaborated_type);

      let ps' =
        LabeledTuple.rearrange(
          s => Option.map(x => (x, Some(x)), s),
          Pat.match_tup_label,
          expected_labels,
          ps',
          (name, e) => {TupLabel(Label(name) |> Pat.fresh, e) |> Pat.fresh},
        );

      let tys =
        LabeledTuple.rearrange(
          s => Option.map(x => (x, Some(x)), s),
          Typ.match_tup_label,
          expected_labels,
          tys,
          (name, e) => {TupLabel(Label(name) |> Typ.fresh, e) |> Typ.fresh},
        );

      Tuple(ps') |> rewrap |> cast_from(Prod(tys) |> Typ.temp);
    | Label(name) => upat |> cast_from(Label(name) |> Typ.temp)
    | Ap(p1, p2) =>
      let (p1', ty1) = elaborate_pattern(m, p1);
      let (p2', ty2) = elaborate_pattern(m, p2);
      let (ty1l, ty1r) = Typ.matched_arrow(ctx, ty1);
      let p1'' = fresh_pat_cast(p1', ty1, Arrow(ty1l, ty1r) |> Typ.temp);
      let p2'' = fresh_pat_cast(p2', ty2, ty1l);
      Ap(p1'', p2'') |> rewrap |> cast_from(ty1r);
    | Invalid(_)
    | EmptyHole
    | MultiHole(_)
    | Wild => upat |> cast_from(Typ.temp(Unknown(Internal)))
    | Var(v) =>
      upat
      |> cast_from(
           Ctx.lookup_var(ctx, v)
           |> Option.map((x: Ctx.var_entry) =>
                x.typ |> Typ.normalize(ctx) |> Typ.all_ids_temp
              )
           |> Option.value(~default=Typ.temp(Unknown(Internal))),
         )
    // Type annotations should already appear
    | Parens(p)
    | Cast(p, _, _) =>
      let (p', ty) = elaborate_pattern(m, p);
      p' |> cast_from(ty |> Typ.normalize(ctx) |> Typ.all_ids_temp);
    | Constructor(c, _) =>
      let mode =
        switch (Id.Map.find_opt(Pat.rep_id(upat), m)) {
        | Some(Info.InfoPat({mode, _})) => mode
        | _ => raise(MissingTypeInfo)
        };
      let t =
        switch (Mode.ctr_ana_typ(ctx, mode, c), Ctx.lookup_ctr(ctx, c)) {
        | (Some(ana_ty), _) => ana_ty
        | (_, Some({typ: syn_ty, _})) => syn_ty
        | _ =>
          Sum([
            ConstructorMap.Variant(c, [Id.invalid], None),
            ConstructorMap.BadEntry(Unknown(Internal) |> Typ.temp),
          ])
          |> Typ.temp
        };
      let t = t |> Typ.normalize(ctx);
      Constructor(c, t) |> rewrap |> cast_from(t);
    };
  (dpat, elaborated_type);
};

/* The primary goal of elaboration is to convert from a type system
   where we have consistency, to a type system where types are either
   equal or they're not. Anything that was just consistent needs to
   become a cast. [The one other thing elaboration does is make
   recursive let bindings explicit.]

   At the top of this function we work out the "elaborated type" of
   of the expression. We also return this elaborated type so we can
   use it in the recursive call. When elaborate returns, you can trust
   that the returned expression will have the returned type. There is
   however, no guarantee that the returned type is even consistent with
   the "elaborated type" at the top, so you should fresh_cast EVERYWHERE
   just in case.

   Important invariant: any cast in an elaborated expression should have
   normalized types.

   [Matt] A lot of these fresh_cast calls are redundant, however if you
   want to remove one, I'd ask you instead comment it out and leave
   a comment explaining why it's redundant.  */

let rec elaborate = (m: Statics.Map.t, uexp: Exp.t): (DHExp.t, Typ.t) => {
  // In the case of singleton labeled tuples we update the syntax in Statics.
  // We store this syntax with the same ID as the original expression and store it on the Info.exp in the Statics.map
  // We are then pulling this out and using it in place of the actual expression.

  let (elaborated_type, ctx, co_ctx, statics_pseudo_elaborated) =
    elaborated_type(m, uexp);
  let cast_from = (ty, exp) => fresh_cast(exp, ty, elaborated_type);
  let (_, rewrap) = Exp.unwrap(uexp);
  let uexp = rewrap(statics_pseudo_elaborated.term);

  let (term, rewrap) = Exp.unwrap(uexp);
  let dhexp =
    switch (term) {
    | Invalid(_)
    | Undefined
    | EmptyHole => uexp |> cast_from(Typ.temp(Unknown(Internal)))
    | MultiHole(stuff) =>
      Any.map_term(
        ~f_exp=(_, exp) => {elaborate(m, exp) |> fst},
        ~f_pat=(_, pat) => {elaborate_pattern(m, pat, false) |> fst},
        _,
      )
      |> List.map(_, stuff)
      |> (
        stuff =>
          MultiHole(stuff)
          |> rewrap
          |> cast_from(Typ.temp(Unknown(Internal)))
      )
    | DynamicErrorHole(e, err) =>
      let (e', _) = elaborate(m, e);
      DynamicErrorHole(e', err)
      |> rewrap
      |> cast_from(Typ.temp(Unknown(Internal)));
    | Cast(e, _, _) // We remove these casts because they should be re-inserted in the recursive call
    | FailedCast(e, _, _)
    | Parens(e) =>
      let (e', ty) = elaborate(m, e);
      e' |> cast_from(ty);
    | Deferral(_) => uexp
    | Int(_) => uexp |> cast_from(Int |> Typ.temp)
    | Bool(_) => uexp |> cast_from(Bool |> Typ.temp)
    | Float(_) => uexp |> cast_from(Float |> Typ.temp)
    | String(_) => uexp |> cast_from(String |> Typ.temp)
    | ListLit(es) =>
      let (ds, tys) = List.map(elaborate(m), es) |> ListUtil.unzip;
      let inner_type =
        Typ.join_all(~empty=Unknown(Internal) |> Typ.temp, ctx, tys)
        |> Option.value(~default=Typ.temp(Unknown(Internal)));
      let ds' = List.map2((d, t) => fresh_cast(d, t, inner_type), ds, tys);
      ListLit(ds') |> rewrap |> cast_from(List(inner_type) |> Typ.temp);
    | Constructor(c, _) =>
      let mode =
        switch (Id.Map.find_opt(Exp.rep_id(uexp), m)) {
        | Some(Info.InfoExp({mode, _})) => mode
        | _ => raise(MissingTypeInfo)
        };
      let t =
        switch (Mode.ctr_ana_typ(ctx, mode, c), Ctx.lookup_ctr(ctx, c)) {
        | (Some(ana_ty), _) => ana_ty
        | (_, Some({typ: syn_ty, _})) => syn_ty
        | _ =>
          Sum([
            ConstructorMap.Variant(c, [Id.invalid], None),
            ConstructorMap.BadEntry(Unknown(Internal) |> Typ.temp),
          ])
          |> Typ.temp
        };
      let t = t |> Typ.normalize(ctx) |> Typ.all_ids_temp;
      Constructor(c, t) |> rewrap |> cast_from(t);
    | Fun(p, e, _, n) =>
      let (p', typ) = elaborate_pattern(m, p, false);
      let (e', tye) = elaborate(m, e);
      Fun(p', e', Some(typ), n)
      |> rewrap
      |> cast_from(Arrow(typ, tye) |> Typ.temp);
    | TypFun(tpat, e, name) =>
      let (e', tye) = elaborate(m, e);
      TypFun(tpat, e', name)
      |> rewrap
      |> cast_from(Forall(tpat, tye) |> Typ.temp);
    | Tuple(es) =>
      let (ds, tys) = List.map(elaborate(m), es) |> ListUtil.unzip;

      let expected_labels: list(option(string)) =
        Typ.get_labels(ctx, elaborated_type);
      let ds =
        LabeledTuple.rearrange(
          s => Option.map(x => (x, Some(x)), s),
          Exp.match_tup_label,
          expected_labels,
          ds,
          (name, e) => {
            TupLabel(Label(name) |> DHExp.fresh, e) |> DHExp.fresh
          },
        );

      let tys =
        LabeledTuple.rearrange(
          s => Option.map(x => (x, Some(x)), s),
          Typ.match_tup_label,
          expected_labels,
          tys,
          (name, e) => {TupLabel(Label(name) |> Typ.fresh, e) |> Typ.fresh},
        );
      Tuple(ds) |> rewrap |> cast_from(Prod(tys) |> Typ.temp);
    | TupLabel(label, e) =>
      let (label', labty) = elaborate(m, label);
      let (e', ety) = elaborate(m, e);
      TupLabel(label', e')
      |> rewrap
      |> cast_from(TupLabel(labty, ety) |> Typ.temp);
    | Label(name) => uexp |> cast_from(Label(name) |> Typ.temp)
    | Dot(e1, e2) =>
      let (e1, ty1) = elaborate(m, e1);
      // Don't elaborate labels
      let rec elab_dot = (ty1: Typ.t, e2: DHExp.t) =>
        switch (ty1.term, e2.term) {
        | (Parens(ty1), _) => elab_dot(ty1, e2)
        | (Prod(tys), Label(name)) =>
          let element =
            LabeledTuple.find_label(Typ.match_tup_label, tys, name);
          switch (element) {
          | Some({term: TupLabel(_, ty), _}) => ty
          | _ => Unknown(Internal) |> Typ.temp
          };
        | (TupLabel(_, ty), Label(name))
            when
              LabeledTuple.has_same_labels(
                Typ.match_tup_label(ty1),
                Some((name, e2)),
              ) => ty
        | _ => Unknown(Internal) |> Typ.temp
        };
      let ty = elab_dot(ty1, e2);
      Dot(e1, e2) |> rewrap |> cast_from(ty);
    | Var(v) =>
      uexp
      |> cast_from(
           Ctx.lookup_var(ctx, v)
           |> Option.map((x: Ctx.var_entry) =>
                x.typ |> Typ.normalize(ctx) |> Typ.all_ids_temp
              )
           |> Option.value(~default=Typ.temp(Unknown(Internal))),
         )
    | Let(p, def, body) =>
      let add_name: (option(string), DHExp.t) => DHExp.t = (
        (name, exp) => {
          let (term, rewrap) = DHExp.unwrap(exp);
          switch (term) {
          | Fun(p, e, t, _) => Fun(p, e, t, name) |> rewrap
          | TypFun(tpat, e, _) => TypFun(tpat, e, name) |> rewrap
          | _ => exp
          };
        }
      );

      let (searchterm, imprewrap) = Pat.unwrap(p);

      // // attempt without pattern matching
      // let rec insert_forall = (ctx, typ, origtyp) =>
      //   if (Typ.is_arrow(typ)) {
      //     let (ty1, ty2) = Typ.matched_arrow(ctx, typ);
      //     let (term1, rewrap1) = Typ.unwrap(ty1);
      //     let (term2, rewrap2) = Typ.unwrap(ty2);

      //     // print_endline("entering insert forall");
      //     // a -> a -> a
      //     // forall a -> forall a -> forall a
      //     let forall_ty2 =
      //       if (Typ.is_arrow(ty2)) {
      //         // print_endline("found nested arrow");
      //         insert_forall(
      //           ctx,
      //           ty2,
      //           origtyp,
      //         );
      //       } else if (Typ.is_var(ty2)) {
      //         let var_name = Typ.get_var_name(ty2);
      //         // print_endline("found ty2 as lowercase variable");
      //         // print_endline(var_name);
      //         Forall(Var(var_name) |> TPat.fresh, origtyp) |> Typ.temp;
      //       } else {
      //         ty2;
      //       };

      //     // let forall_ty2 = insert_forall(ctx, ty2);

      //     // print_endline("constructed forall_ty2");

      //     if (Typ.is_var(ty1)) {
      //       let var_name = Typ.get_var_name(ty1);
      //       // print_endline("found ty1 as lowercase variable");
      //       // print_endline(var_name);
      //       let forall_ty1 =
      //         Forall(Var(var_name) |> TPat.fresh, forall_ty2) |> Typ.temp;
      //       forall_ty1;
      //     } else {
      //       typ;
      //     };
      //   } else {
      //     typ;
      //   };

      // let (newpat, implins) =
      //   switch (searchterm) {
      //   | Cast(p, d, x) =>
      //     // print_endline("inside new type checker");
      //     if (Typ.is_arrow(d)) {
      //       // print_endline("found arrow type");
      //       let new_typ = insert_forall(ctx, d, d);
      //       if (new_typ != d) {
      //         let newsearch = Cast(p, new_typ, x) |> Pat.fresh;
      //         let (newsearchterm, _) = Pat.unwrap(newsearch);
      //         // print_endline("newsearchterm");
      //         // print_endline(newsearchterm |> Pat.show_term);
      //         (newsearchterm, true);
      //       } else {
      //         (searchterm, false);
      //       };
      //     } else {
      //       (searchterm, false);
      //     }
      //   | _ => (searchterm, false)
      //   };
      // let newp = newpat |> imprewrap;

      // attempt with pattern matching

      let inserted_vars_ref = ref(StringSet.empty);
      let implins = ref(true);
      let rec parse_vars = (ctx, typ: Typ.t, origtyp, inserted_vars_ref) => {
        switch (typ.term) {
        | Parens(inner_typ) =>
          // Recursively process the type inside parentheses
          parse_vars(ctx, inner_typ, origtyp, inserted_vars_ref)

        | TupLabel(_, inner_typ) =>
          // Recursively process the type inside labeled tuples
          parse_vars(ctx, inner_typ, origtyp, inserted_vars_ref)

        | Arrow(ty1, ty2) =>
          parse_vars(ctx, ty1, origtyp, inserted_vars_ref);
          parse_vars(ctx, ty2, origtyp, inserted_vars_ref);

        | Prod(types) =>
          List.iter(
            t => parse_vars(ctx, t, origtyp, inserted_vars_ref),
            types,
          )

        // | Sum(variants) =>
        // Sum causing issues, waiting till merge from live to uncomment
        // List.iter(
        //   v => parse_vars(ctx, v, origtyp, inserted_vars_ref),
        //   List.map(
        //     variant =>
        //       switch (variant) {
        //       | ConstructorMap.Variant(_, _, Some(param)) => param
        //       | _ => Unknown(Internal) |> Typ.temp
        //       },
        //     variants,
        //   ),
        // )

        // | Forall({term: Var(name), _} as utpat, tbody) =>
        //   // Can either exclude from variable set or not allow implicit + explicit foralls

        // | Rec({term: Var(name), _} as utpat, tbody) =>
        //   // Handle recursive types

        // | Ap(t1, t2) =>
        //   // Necessary?

        | Var(_) =>
          // Handle type variables
          // print_endline("Inside Var case of insert_forall");
          if (Typ.is_var(typ)) {
            let var_name = Typ.get_var_name(typ);
            if (!StringSet.mem(var_name, inserted_vars_ref^)) {
              // print_endline("Inserting variable: " ++ var_name);
              inserted_vars_ref := StringSet.add(var_name, inserted_vars_ref^); // let forall_ty1 =
                                                                    //   Forall(Var(var_name) |> TPat.fresh, typ) |> Typ.temp;
            };
          }
        | _ => ()
        // Default case: return the type as-is
        };
      };

      // let newpat = parse_vars(ctx, def, def, inserted_vars_ref);
      // printSet(inserted_vars_ref^);

      let insert_foralls = (typ: Typ.t, vars: StringSet.t): Typ.t => {
        // Convert the set of variables into a list
        let var_list = List.rev(StringSet.elements(vars));

        // Fold over the list of variables to prepend `forall` quantifiers
        List.fold_left(
          (acc_typ, var_name) =>
            Forall(Var(var_name) |> TPat.fresh, acc_typ) |> Typ.temp,
          typ,
          var_list,
        );
      };

      // let newpat = insert_foralls(newpat, inserted_vars_ref^);

      // let (p, ty1) = elaborate_pattern(m, newp, false);

      // print_endline("Old pattern");
      // print_endline(p |> Pat.show);
      // print_endline("Old definition");
      // print_endline(def |> DHExp.show);
      // print_endline("Old searchterm");
      // print_endline(searchterm |> Pat.show_term);
      // print_endline("Old body");
      // print_endline(body |> DHExp.show);

      let p =
        switch (searchterm) {
        | Cast(p, ann, x) =>
          parse_vars(ctx, ann, ann, inserted_vars_ref);
          let newpat = insert_foralls(ann, inserted_vars_ref^);
          let newcast = Cast(p, newpat, x) |> Pat.fresh;
          let (newsearchterm, _) = Pat.unwrap(newcast);
          newsearchterm |> imprewrap;
        | _ => p
        };

      let (p, ty1) = elaborate_pattern(m, p, false);

      // print_endline("Old searchterm");
      // print_endline(searchterm |> Pat.show_term);
      // print_endline("New searchterm");
      // print_endline(newpat |> Pat.show_term);
      print_endline("New elaborated pattern");
      print_endline(p |> Pat.show);
      print_endline("New elaborated type");
      print_endline(ty1 |> Typ.show);
      // attach labels if needed for labeled tuples
      let (def_term, def_rewrap) = DHExp.unwrap(def);
      let def =
        switch (def_term, Typ.term_of(Typ.normalize(ctx, ty1))) {
        | (Tuple(ds), Prod(tys)) =>
          Tuple(
            LabeledTuple.rearrange(
              Typ.match_tup_label, DHExp.match_tup_label, tys, ds, (t, b) =>
              TupLabel(Label(t) |> Exp.fresh, b) |> Exp.fresh
            ),
          )
          |> def_rewrap
        | (_, _) => def
        };
      let is_recursive =
        Statics.is_recursive(ctx, p, def, ty1)
        && Pat.get_bindings(p)
        |> Option.get
        |> List.exists(f => VarMap.lookup(co_ctx, f) != None);
      if (!is_recursive) {
        let (def, ty2) = elaborate(m, def);
        let def = add_name(Pat.get_var(p), def);
        let (body, ty) = elaborate(m, body);
        Let(p, fresh_cast(def, ty2, ty1), body) |> rewrap |> cast_from(ty);
      } else {
        // TODO: Add names to mutually recursive functions
        let (def, ty2) = elaborate(m, def);
        let def = add_name(Option.map(s => s ++ "+", Pat.get_var(p)), def);
        let (body, ty) = elaborate(m, body);
        let fixf =
          (FixF(p, fresh_cast(def, ty2, ty1), None): Exp.term)
          |> IdTagged.fresh_deterministic(DHExp.rep_id(uexp));
        Let(p, fixf, body) |> rewrap |> cast_from(ty);
      };
    | FixF(p, e, env) =>
      let (p', typ) = elaborate_pattern(m, p, false);
      let (e', tye) = elaborate(m, e);
      FixF(p', fresh_cast(e', tye, typ), env) |> rewrap |> cast_from(typ);
    | TyAlias(_, _, e) =>
      let (e', tye) = elaborate(m, e);
      e' |> cast_from(tye);
    | Ap(dir, f, a) =>
      let (f', tyf) = elaborate(m, f);
      let (a', tya) = elaborate(m, a);
      let (tyf1, tyf2) = Typ.matched_arrow(ctx, tyf);
      let f'' = fresh_cast(f', tyf, Arrow(tyf1, tyf2) |> Typ.temp);
      let a'' = fresh_cast(a', tya, tyf1);
      Ap(dir, f'', a'') |> rewrap |> cast_from(tyf2);
    | DeferredAp(f, args) =>
      let (f', tyf) = elaborate(m, f);
      let (args', tys) = List.map(elaborate(m), args) |> ListUtil.unzip;
      let (tyf1, tyf2) = Typ.matched_arrow(ctx, tyf);
      let (args, ty_fargs) =
        Typ.matched_prod(ctx, args, Exp.match_tup_label, tyf1, (name, b) =>
          TupLabel(Label(name) |> Exp.fresh, b) |> Exp.fresh
        );
      let prod_args =
        switch (ty_fargs) {
        | [ty] => ty
        | _ => Prod(ty_fargs) |> Typ.temp
        };
      let f'' = fresh_cast(f', tyf, Arrow(prod_args, tyf2) |> Typ.temp);
      let args'' = ListUtil.map3(fresh_cast, args', tys, ty_fargs);
      let remaining_args =
        List.filter(
          ((arg, _)) => Exp.is_deferral(arg),
          List.combine(args, ty_fargs),
        );
      let remaining_arg_ty =
        List.length(remaining_args) == 1
          ? snd(List.hd(remaining_args))
          : Prod(List.map(snd, remaining_args)) |> Typ.temp;
      DeferredAp(f'', args'')
      |> rewrap
      |> cast_from(Arrow(remaining_arg_ty, tyf2) |> Typ.temp);
    | TypAp(e, ut) =>
      let (e', tye) = elaborate(m, e);
      let (tpat, tye') = Typ.matched_forall(ctx, tye);
      let ut' = Typ.normalize(ctx, ut);
      let tye'' =
        Typ.subst(
          ut',
          tpat |> Option.value(~default=TPat.fresh(EmptyHole)),
          tye',
        );
      TypAp(e', ut) |> rewrap |> cast_from(tye'');
    | If(c, t, f) =>
      let (c', tyc) = elaborate(m, c);
      let (t', tyt) = elaborate(m, t);
      let (f', tyf) = elaborate(m, f);
      let ty =
        Typ.join(ctx, tyt, tyf)
        |> Option.value(~default=Typ.temp(Unknown(Internal)));
      let c'' = fresh_cast(c', tyc, Bool |> Typ.temp);
      let t'' = fresh_cast(t', tyt, ty);
      let f'' = fresh_cast(f', tyf, ty);
      If(c'', t'', f'') |> rewrap |> cast_from(ty);
    | Seq(e1, e2) =>
      let (e1', _) = elaborate(m, e1);
      let (e2', ty2) = elaborate(m, e2);
      Seq(e1', e2') |> rewrap |> cast_from(ty2);
    | Test(e) =>
      let (e', t) = elaborate(m, e);
      Test(fresh_cast(e', t, Bool |> Typ.temp))
      |> rewrap
      |> cast_from(Prod([]) |> Typ.temp);
    | Filter(kind, e) =>
      let (e', t) = elaborate(m, e);
      let kind' =
        switch (kind) {
        | Residue(_) => kind
        | Filter({act, pat}) => Filter({act, pat: elaborate(m, pat) |> fst})
        };
      Filter(kind', e') |> rewrap |> cast_from(t);
    | Closure(env, e) =>
      // Should we be elaborating the contents of the environment?
      let (e', t) = elaborate(m, e);
      Closure(env, e') |> rewrap |> cast_from(t);
    | Cons(e1, e2) =>
      let (e1', ty1) = elaborate(m, e1);
      let (e2', ty2) = elaborate(m, e2);
      let ty2_inner = Typ.matched_list(ctx, ty2);
      let ty_inner =
        Typ.join(ctx, ty1, ty2_inner)
        |> Option.value(~default=Typ.temp(Unknown(Internal)));
      let e1'' = fresh_cast(e1', ty1, ty_inner);
      let e2'' = fresh_cast(e2', ty2, List(ty_inner) |> Typ.temp);
      Cons(e1'', e2'') |> rewrap |> cast_from(List(ty_inner) |> Typ.temp);
    | ListConcat(e1, e2) =>
      let (e1', ty1) = elaborate(m, e1);
      let (e2', ty2) = elaborate(m, e2);
      let ty_inner1 = Typ.matched_list(ctx, ty1);
      let ty_inner2 = Typ.matched_list(ctx, ty2);
      let ty_inner =
        Typ.join(ctx, ty_inner1, ty_inner2)
        |> Option.value(~default=Typ.temp(Unknown(Internal)));
      let e1'' = fresh_cast(e1', ty1, List(ty_inner) |> Typ.temp);
      let e2'' = fresh_cast(e2', ty2, List(ty_inner) |> Typ.temp);
      ListConcat(e1'', e2'')
      |> rewrap
      |> cast_from(List(ty_inner) |> Typ.temp);
    | UnOp(Meta(Unquote), e) =>
      switch (e.term) {
      // TODO: confirm whether these types are correct
      | Var("e") =>
        Constructor("$e", Unknown(Internal) |> Typ.temp) |> rewrap
      | Var("v") =>
        Constructor("$v", Unknown(Internal) |> Typ.temp) |> rewrap
      | _ => EmptyHole |> rewrap |> cast_from(Typ.temp(Unknown(Internal)))
      }
    | UnOp(Int(Minus), e) =>
      let (e', t) = elaborate(m, e);
      UnOp(Int(Minus), fresh_cast(e', t, Int |> Typ.temp))
      |> rewrap
      |> cast_from(Int |> Typ.temp);
    | UnOp(Bool(Not), e) =>
      let (e', t) = elaborate(m, e);
      UnOp(Bool(Not), fresh_cast(e', t, Bool |> Typ.temp))
      |> rewrap
      |> cast_from(Bool |> Typ.temp);
    | BinOp(Int(Plus | Minus | Times | Power | Divide) as op, e1, e2) =>
      let (e1', t1) = elaborate(m, e1);
      let (e2', t2) = elaborate(m, e2);
      BinOp(
        op,
        fresh_cast(e1', t1, Int |> Typ.temp),
        fresh_cast(e2', t2, Int |> Typ.temp),
      )
      |> rewrap
      |> cast_from(Int |> Typ.temp);
    | BinOp(
        Int(
          LessThan | LessThanOrEqual | GreaterThan | GreaterThanOrEqual |
          Equals |
          NotEquals,
        ) as op,
        e1,
        e2,
      ) =>
      let (e1', t1) = elaborate(m, e1);
      let (e2', t2) = elaborate(m, e2);
      BinOp(
        op,
        fresh_cast(e1', t1, Int |> Typ.temp),
        fresh_cast(e2', t2, Int |> Typ.temp),
      )
      |> rewrap
      |> cast_from(Bool |> Typ.temp);
    | BinOp(Bool(And | Or) as op, e1, e2) =>
      let (e1', t1) = elaborate(m, e1);
      let (e2', t2) = elaborate(m, e2);
      BinOp(
        op,
        fresh_cast(e1', t1, Bool |> Typ.temp),
        fresh_cast(e2', t2, Bool |> Typ.temp),
      )
      |> rewrap
      |> cast_from(Bool |> Typ.temp);
    | BinOp(Float(Plus | Minus | Times | Divide | Power) as op, e1, e2) =>
      let (e1', t1) = elaborate(m, e1);
      let (e2', t2) = elaborate(m, e2);
      BinOp(
        op,
        fresh_cast(e1', t1, Float |> Typ.temp),
        fresh_cast(e2', t2, Float |> Typ.temp),
      )
      |> rewrap
      |> cast_from(Float |> Typ.temp);
    | BinOp(
        Float(
          LessThan | LessThanOrEqual | GreaterThan | GreaterThanOrEqual |
          Equals |
          NotEquals,
        ) as op,
        e1,
        e2,
      ) =>
      let (e1', t1) = elaborate(m, e1);
      let (e2', t2) = elaborate(m, e2);
      BinOp(
        op,
        fresh_cast(e1', t1, Float |> Typ.temp),
        fresh_cast(e2', t2, Float |> Typ.temp),
      )
      |> rewrap
      |> cast_from(Bool |> Typ.temp);
    | BinOp(String(Concat) as op, e1, e2) =>
      let (e1', t1) = elaborate(m, e1);
      let (e2', t2) = elaborate(m, e2);
      BinOp(
        op,
        fresh_cast(e1', t1, String |> Typ.temp),
        fresh_cast(e2', t2, String |> Typ.temp),
      )
      |> rewrap
      |> cast_from(String |> Typ.temp);
    | BinOp(String(Equals) as op, e1, e2) =>
      let (e1', t1) = elaborate(m, e1);
      let (e2', t2) = elaborate(m, e2);
      BinOp(
        op,
        fresh_cast(e1', t1, String |> Typ.temp),
        fresh_cast(e2', t2, String |> Typ.temp),
      )
      |> rewrap
      |> cast_from(Bool |> Typ.temp);
    | BuiltinFun(fn) =>
      uexp
      |> cast_from(
           Ctx.lookup_var(Builtins.ctx_init, fn)
           |> Option.map((x: Ctx.var_entry) => x.typ)
           |> Option.value(~default=Typ.temp(Unknown(Internal))),
         )
    | Match(e, cases) =>
      let (e', t) = elaborate(m, e);
      let (ps, es) = ListUtil.unzip(cases);
      let (ps', ptys) =
        List.map(p => elaborate_pattern(m, p, false), ps) |> ListUtil.unzip;
      let joined_pty =
        Typ.join_all(~empty=Unknown(Internal) |> Typ.temp, ctx, ptys)
        |> Option.value(~default=Typ.temp(Unknown(Internal)));
      let ps'' =
        List.map2((p, t) => fresh_pat_cast(p, t, joined_pty), ps', ptys);
      let e'' = fresh_cast(e', t, joined_pty);
      let (es', etys) = List.map(elaborate(m), es) |> ListUtil.unzip;
      let joined_ety =
        Typ.join_all(~empty=Unknown(Internal) |> Typ.temp, ctx, etys)
        |> Option.value(~default=Typ.temp(Unknown(Internal)));
      let es'' =
        List.map2((e, t) => fresh_cast(e, t, joined_ety), es', etys);
      Match(e'', List.combine(ps'', es''))
      |> rewrap
      |> cast_from(joined_ety);
    };
  (dhexp, elaborated_type);
};

//let dhexp_of_uexp = Core.Memo.general(~cache_size_bound=1000, dhexp_of_uexp);

/* This function gives a new id to all the types
   in the expression. It does this to get rid of
   all the invalid ids we added to prevent generating
   too many new ids */
let fix_typ_ids =
  Exp.map_term(~f_typ=(cont, e) => e |> IdTagged.new_ids |> cont);

let uexp_elab = (m: Statics.Map.t, uexp: Exp.t): ElaborationResult.t =>
  switch (elaborate(m, uexp)) {
  | exception MissingTypeInfo => DoesNotElaborate
  | (d, ty) => Elaborates(d |> fix_typ_ids, ty, Delta.empty)
  };
