/*
 A nice property would be that elaboration is idempotent...
 */

open Util;

exception MissingTypeInfo;

module ElaborationResult = {
  [@deriving sexp]
  type t =
    | Elaborates(DHExp.t, TypSlice.t)
    | DoesNotElaborate;
};

let fresh_cast = (d: DHExp.t, t1: TypSlice.t, t2: TypSlice.t): Exp.t => {
  switch (d.term) {
  | Label(_) => d
  | _ =>
    TypSlice.equal(t1, t2)
      ? d
      : {
        let d': Exp.t =
          (Cast(d, t1, TypSlice.temp(`Typ(Unknown(Internal)))): Exp.term)
          |> IdTagged.fresh_deterministic(DHExp.rep_id(d))
          |> Casts.transition_multiple;
        (Cast(d', TypSlice.temp(`Typ(Unknown(Internal))), t2): Exp.term)
        |> IdTagged.fresh_deterministic(DHExp.rep_id(d'))
        |> Casts.transition_multiple;
      }
  };
};

let fresh_pat_cast = (p: DHPat.t, t1: TypSlice.t, t2: TypSlice.t): DHPat.t => {
  switch (p.term) {
  | Label(_) => p
  | _ =>
    TypSlice.equal(t1, t2)
      ? p
      : {
        Cast(
          DHPat.fresh(Cast(p, t1, TypSlice.temp(`Typ(Unknown(Internal)))))
          |> Casts.pattern_fixup,
          TypSlice.temp(`Typ(Unknown(Internal))),
          t2,
        )
        |> DHPat.fresh
        |> Casts.pattern_fixup;
      }
  };
};

let elaborated_type =
    (m: Statics.Map.t, uexp: Exp.t)
    : (TypSlice.t, Typ.t, Ctx.t, CoCtx.t, Exp.t) => {
  let (ana_ty, self_ty, ctx, co_ctx, term) =
    switch (Id.Map.find_opt(Exp.rep_id(uexp), m)) {
    | Some(Info.InfoExp({ana, ty, ctx, co_ctx, term: new_term, _})) => (
        ana,
        ty,
        ctx,
        co_ctx,
        new_term,
      )
    | _ => raise(MissingTypeInfo)
    };
  let elab_ty = Typ.match_synswitch(ana_ty, self_ty);
  (
    elab_ty |> Typ.normalize(ctx) |> Typ.all_ids_temp,
    ana_ty,
    ctx,
    co_ctx,
    term,
  );
};

let elaborated_pat_type =
    (m: Statics.Map.t, upat: Pat.t): (TypSlice.t, Typ.t, Ctx.t, Pat.t) => {
  let (ana_ty, self_ty, ctx, prev_synswitch, term, label_inference) =
    switch (Id.Map.find_opt(Pat.rep_id(upat), m)) {
    | Some(
        Info.InfoPat({
          ana,
          ty,
          ctx,
          prev_synswitch,
          term: new_term,
          label_inference,
          _,
        }),
      ) => (
        ana,
        ty,
        ctx,
        prev_synswitch,
        new_term,
        label_inference,
      )
    | _ => raise(MissingTypeInfo)
    };
  let elab_ty =
    switch (prev_synswitch) {
    | None => Typ.match_synswitch(self_ty, ana_ty)
    | Some(syn_ty) =>
      // Autolabelling for singleton labeled tuples
      switch (label_inference) {
      // TODO: Does anything need to be sliced here?
      | Some(SingletonLabelInference({label: l, _})) =>
        TypSlice.match_synswitch(
          Prod([
            TupLabel(Label(l) |> Typ.temp |> TypSlice.t_of_typ_t, syn_ty)
            |> TypSlice.term_of_slc_typ_term
            |> TypSlice.temp,
          ])
          |> TypSlice.term_of_slc_typ_term
          |> TypSlice.temp,
          ana_ty,
        )
      | _ => Typ.match_synswitch(syn_ty, ana_ty)
      }
    };
  (
    elab_ty |> TypSlice.normalize(ctx) |> TypSlice.all_ids_temp,
    ana_ty,
    ctx,
    term,
  );
};

let rec elaborate_pattern =
        (m: Statics.Map.t, upat: Pat.t, in_container: bool)
        : (Pat.t, TypSlice.t) => {
  // Pulling upat back out of the statics map for statics level singleton tuple autolabeling
  let (elaborated_type, ana, ctx, upat) = elaborated_pat_type(m, upat);
  let elaborate_pattern = (~in_container=false, m, upat) =>
    elaborate_pattern(m, upat, in_container);
  let cast_from = (ty, exp) => fresh_pat_cast(exp, ty, elaborated_type);
  let (term, rewrap) = Pat.unwrap(upat);
  let dpat =
    switch (term) {
    | Atom(c) =>
      let c =
        Operators.replace_literal(c, Typ.is_ana_atom(ana), ctx.use_mode);
      switch (c) {
      | L(c) =>
        Atom(c) |> rewrap |> cast_from(Atom(c |> Atom.cls_of_t) |> Typ.temp)
      | R(BadInt(s)) =>
        Invalid(s) |> rewrap |> cast_from(Unknown(Internal) |> Typ.temp)
      };
    | ListLit(ps) =>
      let (ps, tys) = List.map(elaborate_pattern(m), ps) |> ListUtil.unzip;
      let inner_type =
        tys
        |> TypSlice.join_all(
             ~empty=`Typ(Unknown(Internal)) |> TypSlice.temp,
             ctx,
           )
        |> Option.value(~default=TypSlice.temp(`Typ(Unknown(Internal))));
      ps
      |> List.map2((p, t) => fresh_pat_cast(p, t, inner_type), _, tys)
      |> (
        ps' =>
          ListLit(ps')
          |> rewrap
          |> cast_from(
               List(inner_type)
               |> TypSlice.term_of_slc_typ_term
               |> TypSlice.temp,
             )
      );
    | Cons(p1, p2) =>
      let (p1', ty1) = elaborate_pattern(m, p1);
      let (p2', ty2) = elaborate_pattern(m, p2);
      let ty2_inner = TypSlice.matched_list(ctx, ty2);
      let ty_inner =
        TypSlice.join(ctx, ty1, ty2_inner)
        |> Option.value(~default=TypSlice.temp(`Typ(Unknown(Internal))));
      let p1'' = fresh_pat_cast(p1', ty1, ty_inner);
      let p2'' =
        fresh_pat_cast(
          p2',
          ty2,
          List(ty_inner) |> TypSlice.term_of_slc_typ_term |> TypSlice.temp,
        );
      Cons(p1'', p2'')
      |> rewrap
      |> cast_from(
           List(ty_inner) |> TypSlice.term_of_slc_typ_term |> TypSlice.temp,
         );
    | TupLabel(lab, p) =>
      let (plab, labty) = elaborate_pattern(m, lab);
      let (p', pty) = elaborate_pattern(m, p);
      if (in_container) {
        TupLabel(plab, p')
        |> rewrap
        |> cast_from(
             TupLabel(labty, pty)
             |> TypSlice.term_of_slc_typ_term
             |> TypSlice.temp,
           );
      } else {
        Tuple([TupLabel(plab, p') |> rewrap])
        |> DHPat.fresh
        |> cast_from(
             Prod([
               TupLabel(labty, pty)
               |> TypSlice.term_of_slc_typ_term
               |> TypSlice.temp,
             ])
             |> TypSlice.term_of_slc_typ_term
             |> TypSlice.temp,
           );
      };
    | Tuple(ps) =>
      let (ps', tys) =
        List.map(elaborate_pattern(m, ~in_container=true), ps)
        |> ListUtil.unzip;
      let expected_labels: list(option(string)) =
        TypSlice.get_labels(ctx, elaborated_type);

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
          TypSlice.match_tup_label,
          expected_labels,
          tys,
          (name, e) => {
            TupLabel(Label(name) |> Typ.temp |> TypSlice.t_of_typ_t, e)
            |> TypSlice.term_of_slc_typ_term
            |> TypSlice.temp
          },
        );

      Tuple(ps')
      |> rewrap
      |> cast_from(
           Prod(tys) |> TypSlice.term_of_slc_typ_term |> TypSlice.temp,
         );
    | Label(name) =>
      upat |> cast_from(Label(name) |> Typ.temp |> TypSlice.t_of_typ_t)
    | Ap(p1, p2) =>
      let (p1', ty1) = elaborate_pattern(m, p1);
      let (p2', ty2) = elaborate_pattern(m, p2);
      let (ty1l, ty1r) = TypSlice.matched_arrow(ctx, ty1);
      let p1'' =
        fresh_pat_cast(
          p1',
          ty1,
          Arrow(ty1l, ty1r) |> TypSlice.term_of_slc_typ_term |> TypSlice.temp,
        );
      let p2'' = fresh_pat_cast(p2', ty2, ty1l);
      Ap(p1'', p2'') |> rewrap |> cast_from(ty1r);
    | Invalid(_)
    | EmptyHole
    | MultiHole(_)
    | Wild => upat |> cast_from(TypSlice.temp(`Typ(Unknown(Internal))))
    | Var(v) =>
      upat
      |> cast_from(
           Ctx.lookup_var(ctx, v)
           |> Option.map((x: Ctx.var_entry) =>
                x.typ |> TypSlice.normalize(ctx) |> TypSlice.all_ids_temp
              )
           |> Option.value(~default=TypSlice.temp(`Typ(Unknown(Internal)))),
         )
    // Type annotations should already appeard
    | Parens(p)
    | Cast(p, _, _) =>
      let (p', ty) = elaborate_pattern(m, p);
      p' |> cast_from(ty |> TypSlice.normalize(ctx) |> TypSlice.all_ids_temp);
    | Probe(p, probe) =>
      let (e', ty) = elaborate_pattern(m, p);
      let probe = Dynamics.Probe.instrument_pat(m, Pat.rep_id(upat), probe);
      Probe(
        e'
        |> cast_from(ty |> TypSlice.normalize(ctx) |> TypSlice.all_ids_temp),
        probe,
      )
      |> rewrap;
    | Constructor(c, _) =>
      let ana_ty =
        switch (Id.Map.find_opt(Pat.rep_id(upat), m)) {
        | Some(Info.InfoPat({ana, _})) => ana
        | _ => raise(MissingTypeInfo)
        };
      let t =
        switch (Self.ctr_ana_typ(ctx, ana_ty, c), Ctx.lookup_ctr(ctx, c)) {
        | (Some(ana_ty), _) => Some(Typ.normalize(ctx, ana_ty))
        | (_, Some({typ: syn_ty, _})) => Some(Typ.normalize(ctx, syn_ty))
        | _ => None
        };
      let ty =
        OptUtil.get(
          () =>
            Sum([
              ConstructorMap.Variant(c, [Id.invalid], None),
              ConstructorMap.BadEntry(Unknown(Internal) |> Typ.temp),
            ])
            |> Typ.temp,
          t,
        );
      Constructor(c, Some(t)) |> rewrap |> cast_from(ty);
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

let rec elaborate = (m: Statics.Map.t, uexp: Exp.t): (DHExp.t, TypSlice.t) => {
  // In the case of singleton labeled tuples we update the syntax in Statics.
  // We store this syntax with the same ID as the original expression and store it on the Info.exp in the Statics.map
  // We are then pulling this out and using it in place of the actual expression.

  let (elaborated_type, ana, ctx, co_ctx, statics_pseudo_elaborated) =
    elaborated_type(m, uexp);
  let cast_from = (ty, exp) => fresh_cast(exp, ty, elaborated_type);
  let (_, rewrap) = Exp.unwrap(uexp);
  let uexp = rewrap(statics_pseudo_elaborated.term);

  let (term, rewrap) = Exp.unwrap(uexp);
  let dhexp =
    switch (term) {
    | Invalid(_)
    | Undefined
    | EmptyHole =>
      uexp |> cast_from(TypSlice.temp(`Typ(Unknown(Internal))))
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
          |> cast_from(TypSlice.temp(`Typ(Unknown(Internal))))
      )
    | DynamicErrorHole(e, err) =>
      let (e', _) = elaborate(m, e);
      DynamicErrorHole(e', err)
      |> rewrap
      |> cast_from(TypSlice.temp(`Typ(Unknown(Internal))));
    | Cast(e, _, _) // We remove these casts because they should be re-inserted in the recursive call
    | FailedCast(e, _, _) =>
      let (e', ty) = elaborate(m, e);
      Parens(e' |> cast_from(ty)) |> rewrap;
    | Parens(e) =>
      let (e', ty) = elaborate(m, e);
      e' |> cast_from(ty);
    | Probe(e, probe) =>
      let (e', ty) = elaborate(m, e);
      let probe = Dynamics.Probe.instrument_exp(m, Exp.rep_id(uexp), probe);
      Probe(e' |> cast_from(ty), probe) |> rewrap;
    | Deferral(_) => uexp
    | Atom(c) =>
      let c =
        Operators.replace_literal(c, Typ.is_ana_atom(ana), ctx.use_mode);
      switch (c) {
      | L(c) =>
        Atom(c) |> rewrap |> cast_from(Atom(c |> Atom.cls_of_t) |> Typ.temp)
      | R(BadInt(s)) =>
        Invalid(s) |> rewrap |> cast_from(Unknown(Internal) |> Typ.temp)
      };
    | ListLit(es) =>
      let (ds, tys) = List.map(elaborate(m), es) |> ListUtil.unzip;
      let inner_type =
        TypSlice.join_all(
          ~empty=`Typ(Unknown(Internal)) |> TypSlice.temp,
          ctx,
          tys,
        )
        |> Option.value(~default=TypSlice.temp(`Typ(Unknown(Internal))));
      let ds' = List.map2((d, t) => fresh_cast(d, t, inner_type), ds, tys);
      ListLit(ds')
      |> rewrap
      |> cast_from(
           List(inner_type) |> TypSlice.term_of_slc_typ_term |> TypSlice.temp,
         );
    | Constructor(c, _) =>
      let (self, ty) =
        switch (Id.Map.find_opt(Exp.rep_id(uexp), m)) {
        | Some(Info.InfoExp({self, ty, _})) => (self, ty)
        | _ => raise(MissingTypeInfo)
        };
      let t =
        switch (self) {
        | Common(FreeConstructor(_)) => Some(None)
        | _ => Some(Some(Typ.normalize(ctx, ty)))
        };
      let ty =
        OptUtil.get(
          () =>
            Sum([
              ConstructorMap.Variant(c, [Id.invalid], None),
              ConstructorMap.BadEntry(Unknown(Internal) |> Typ.temp),
            ])
            |> Typ.temp,
          t |> Option.join,
        );
      Constructor(c, t) |> rewrap |> cast_from(ty);
    | Fun(p, e, _, n) =>
      let (p', typ) = elaborate_pattern(m, p, false);
      let (e', tye) = elaborate(m, e);
      Fun(p', e', Some(typ), n)
      |> rewrap
      |> cast_from(
           Arrow(typ, tye) |> TypSlice.term_of_slc_typ_term |> TypSlice.temp,
         );
    | TypFun(tpat, e, name) =>
      let (e', tye) = elaborate(m, e);
      TypFun(tpat, e', name)
      |> rewrap
      |> cast_from(
           Forall(tpat, tye) |> TypSlice.term_of_slc_typ_term |> TypSlice.temp,
         );
    | Tuple(es) =>
      let (ds, tys) = List.map(elaborate(m), es) |> ListUtil.unzip;

      let expected_labels: list(option(string)) =
        TypSlice.get_labels(ctx, elaborated_type);
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
          TypSlice.match_tup_label,
          expected_labels,
          tys,
          (name, e) => {
            TupLabel(Label(name) |> Typ.fresh |> TypSlice.t_of_typ_t, e)
            |> TypSlice.term_of_slc_typ_term
            |> TypSlice.temp
          },
        );
      Tuple(ds)
      |> rewrap
      |> cast_from(
           Prod(tys) |> TypSlice.term_of_slc_typ_term |> TypSlice.temp,
         );
    | TupLabel(label, e) =>
      let (label', labty) = elaborate(m, label);
      let (e', ety) = elaborate(m, e);
      TupLabel(label', e')
      |> rewrap
      |> cast_from(
           TupLabel(labty, ety)
           |> TypSlice.term_of_slc_typ_term
           |> TypSlice.temp,
         );
    | Label(name) =>
      uexp |> cast_from(Label(name) |> Typ.temp |> TypSlice.t_of_typ_t)
    | Dot(e1, e2) =>
      let (e1, ty1) = elaborate(m, e1);
      // Don't elaborate labels
      let rec elab_dot = (ty1: TypSlice.t, e2: DHExp.t) =>
        switch (ty1, e2.term) {
        | (s1, _) when TypSlice.is_parens(s1) =>
          let s1 = TypSlice.unparens(s1);
          elab_dot(s1, e2);
        | (s, Label(name)) when TypSlice.is_prod(s, ~ignore_parens=false) =>
          let tys = TypSlice.unprod(s);
          let element =
            LabeledTuple.find_label(TypSlice.match_tup_label, tys, name);
          switch (element) {
          | Some(s) when TypSlice.is_tuplabel(s, ~ignore_parens=false) =>
            TypSlice.untuplabel(s) |> snd
          | _ => Unknown(Internal) |> Typ.temp |> TypSlice.t_of_typ_t
          };
        | (s, Label(name))
            when
              TypSlice.is_tuplabel(s, ~ignore_parens=false)
              && LabeledTuple.has_same_labels(
                   TypSlice.match_tup_label(s),
                   Some((name, e2)),
                 ) =>
          s |> TypSlice.untuplabel |> snd
        | _ => Unknown(Internal) |> Typ.temp |> TypSlice.t_of_typ_t
        };
      let ty = elab_dot(ty1, e2);
      Dot(e1, e2) |> rewrap |> cast_from(ty);
    | Var(v) =>
      uexp
      |> cast_from(
           Ctx.lookup_var(ctx, v)
           |> Option.map((x: Ctx.var_entry) =>
                x.typ |> TypSlice.normalize(ctx) |> TypSlice.all_ids_temp
              )
           |> Option.value(~default=TypSlice.temp(`Typ(Unknown(Internal)))),
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
      let (p, ty1) = elaborate_pattern(m, p, false);
      // attach labels if needed for labeled tuples
      let (def_term, def_rewrap) = DHExp.unwrap(def);
      let def =
        switch (def_term, TypSlice.normalize(ctx, ty1)) {
        | (Tuple(ds), s) when TypSlice.is_prod(s) =>
          let tys = TypSlice.unprod(s);
          Tuple(
            LabeledTuple.rearrange(
              TypSlice.match_tup_label, DHExp.match_tup_label, tys, ds, (t, b) =>
              TupLabel(Label(t) |> Exp.fresh, b) |> Exp.fresh
            ),
          )
          |> def_rewrap;
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
    // These forms are removed in elaboration
    | Use(_, e)
    | TyAlias(_, _, e) =>
      let (e', tye) = elaborate(m, e);
      e' |> cast_from(tye);
    | Ap(dir, f, a) =>
      let (f', tyf) = elaborate(m, f);
      let (a', tya) = elaborate(m, a);
      let (tyf1, tyf2) = TypSlice.matched_arrow(ctx, tyf);
      let f'' =
        fresh_cast(
          f',
          tyf,
          Arrow(tyf1, tyf2) |> TypSlice.term_of_slc_typ_term |> TypSlice.temp,
        );
      let a'' = fresh_cast(a', tya, tyf1);
      Ap(dir, f'', a'') |> rewrap |> cast_from(tyf2);
    | DeferredAp(f, args) =>
      let (f', tyf) = elaborate(m, f);
      let (args', tys) = List.map(elaborate(m), args) |> ListUtil.unzip;
      let (tyf1, tyf2) = TypSlice.matched_arrow(ctx, tyf);
      let (args, ty_fargs) =
        if (List.length(args) > 1) {
          Typ.matched_prod(ctx, args, Exp.match_tup_label, tyf1, (name, b) =>
            TupLabel(Label(name) |> Exp.fresh, b) |> Exp.fresh
          );
        } else {
          (args, [tyf1]);
        };
      let prod_args =
        switch (ty_fargs) {
        | [ty] => ty
        | _ =>
          Prod(ty_fargs) |> TypSlice.term_of_slc_typ_term |> TypSlice.temp
        };
      let f'' =
        fresh_cast(
          f',
          tyf,
          Arrow(prod_args, tyf2)
          |> TypSlice.term_of_slc_typ_term
          |> TypSlice.temp,
        );
      let args'' = ListUtil.map3(fresh_cast, args', tys, ty_fargs);
      let remaining_args =
        List.filter(
          ((arg, _)) => Exp.is_deferral(arg),
          List.combine(args, ty_fargs),
        );
      let remaining_arg_ty =
        List.length(remaining_args) == 1
          ? snd(List.hd(remaining_args))
          : Prod(List.map(snd, remaining_args))
            |> TypSlice.term_of_slc_typ_term
            |> TypSlice.temp;
      DeferredAp(f'', args'')
      |> rewrap
      |> cast_from(
           Arrow(remaining_arg_ty, tyf2)
           |> TypSlice.term_of_slc_typ_term
           |> TypSlice.temp,
         );
    | TypAp(e, ut) =>
      let (e', tye) = elaborate(m, e);
      let (tpat, tye') = TypSlice.matched_forall(ctx, tye);
      let ut' = TypSlice.normalize(ctx, ut |> TypSlice.t_of_typ_t);
      let tye'' =
        TypSlice.subst(
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
        TypSlice.join(ctx, tyt, tyf)
        |> Option.value(~default=TypSlice.temp(`Typ(Unknown(Internal))));
      let c'' = fresh_cast(c', tyc, Atom(`Typ(Bool)) |> TypSlice.temp);
      let t'' = fresh_cast(t', tyt, ty);
      let f'' = fresh_cast(f', tyf, ty);
      If(c'', t'', f'') |> rewrap |> cast_from(ty);
    | Seq(e1, e2) =>
      let (e1', _) = elaborate(m, e1);
      let (e2', ty2) = elaborate(m, e2);
      Seq(e1', e2') |> rewrap |> cast_from(ty2);
    | Test(e) =>
      let (e', t) = elaborate(m, e);
      Test(fresh_cast(e', t, Atom(`Typ(Bool)) |> TypSlice.temp))
      |> rewrap
      |> cast_from(`Typ(Prod([])) |> TypSlice.temp);
    | Filter(kind, e) =>
      let (e', t) = elaborate(m, e);
      let kind' =
        switch (kind) {
        | Residue(_) => kind
        | Filter({act, pat}) =>
          Filter({
            act,
            pat: elaborate(m, pat) |> fst,
          })
        };
      Filter(kind', e') |> rewrap |> cast_from(t);
    | Closure(env, e) =>
      // Should we be elaborating the contents of the environment?
      let (e', t) = elaborate(m, e);
      Closure(env, e') |> rewrap |> cast_from(t);
    | Cons(e1, e2) =>
      let (e1', ty1) = elaborate(m, e1);
      let (e2', ty2) = elaborate(m, e2);
      let ty2_inner = TypSlice.matched_list(ctx, ty2);
      let ty_inner =
        TypSlice.join(ctx, ty1, ty2_inner)
        |> Option.value(~default=TypSlice.temp(`Typ(Unknown(Internal))));
      let e1'' = fresh_cast(e1', ty1, ty_inner);
      let e2'' =
        fresh_cast(
          e2',
          ty2,
          List(ty_inner) |> TypSlice.term_of_slc_typ_term |> TypSlice.temp,
        );
      Cons(e1'', e2'')
      |> rewrap
      |> cast_from(
           List(ty_inner) |> TypSlice.term_of_slc_typ_term |> TypSlice.temp,
         );
    | ListConcat(e1, e2) =>
      let (e1', ty1) = elaborate(m, e1);
      let (e2', ty2) = elaborate(m, e2);
      let ty_inner1 = TypSlice.matched_list(ctx, ty1);
      let ty_inner2 = TypSlice.matched_list(ctx, ty2);
      let ty_inner =
        TypSlice.join(ctx, ty_inner1, ty_inner2)
        |> Option.value(~default=TypSlice.temp(`Typ(Unknown(Internal))));
      let e1'' =
        fresh_cast(
          e1',
          ty1,
          List(ty_inner) |> TypSlice.term_of_slc_typ_term |> TypSlice.temp,
        );
      let e2'' =
        fresh_cast(
          e2',
          ty2,
          List(ty_inner) |> TypSlice.term_of_slc_typ_term |> TypSlice.temp,
        );
      ListConcat(e1'', e2'')
      |> rewrap
      |> cast_from(
           List(ty_inner) |> TypSlice.term_of_slc_typ_term |> TypSlice.temp,
         );
    | UnOp(Meta(Unquote), e) =>
      switch (e.term) {
      // TODO: confirm whether these types are correct
      | Var("e") =>
        Constructor("$e", Some(Some(Unknown(Internal) |> Typ.fresh)))
        |> rewrap
      | Var("v") =>
        Constructor("$v", Some(Some(Unknown(Internal) |> Typ.fresh)))
        |> rewrap
      | _ =>
        EmptyHole
        |> rewrap
        |> cast_from(TypSlice.temp(`Typ(Unknown(Internal))))
      }
    | UnOp(op, e) =>
      let op = Operators.replace_un_op(op, ctx.use_mode);
      let (e', t) = elaborate(m, e);
      let semantics = Operators.semantics_of_un_op(op);
      switch (semantics) {
      | Undefined(_) =>
        UnOp(op, fresh_cast(e', t, Unknown(Internal) |> Typ.temp))
        |> rewrap
        |> cast_from(Unknown(Internal) |> Typ.temp)
      | Defined(t1, t2, _) =>
        let t1 = Atom(Atom.cls_of_kind(t1)) |> Typ.temp;
        let t2 = Atom(Atom.cls_of_kind(t2)) |> Typ.temp;
        UnOp(op, fresh_cast(e', t, t1)) |> rewrap |> cast_from(t2);
      };
    | BinOp(op, e1, e2) =>
      let op = Operators.replace_bin_op(op, ctx.use_mode);
      let (e1', t1) = elaborate(m, e1);
      let (e2', t2) = elaborate(m, e2);
      let semantics = Operators.semantics_of_bin_op(op);
      switch (semantics) {
      | Undefined(_) =>
        BinOp(
          op,
          fresh_cast(e1', t1, Unknown(Internal) |> Typ.temp),
          fresh_cast(e2', t2, Unknown(Internal) |> Typ.temp),
        )
        |> rewrap
        |> cast_from(Unknown(Internal) |> Typ.temp)
      | Defined(t1', t2', t3', _) =>
        let t1' = Atom(Atom.cls_of_kind(t1')) |> Typ.temp;
        let t2' = Atom(Atom.cls_of_kind(t2')) |> Typ.temp;
        let t3' = Atom(Atom.cls_of_kind(t3')) |> Typ.temp;
        BinOp(op, fresh_cast(e1', t1, t1'), fresh_cast(e2', t2, t2'))
        |> rewrap
        |> cast_from(t3');
      };
    | BuiltinFun(fn) =>
      uexp
      |> cast_from(
           Ctx.lookup_var(Builtins.ctx_init(None), fn)
           |> Option.map((x: Ctx.var_entry) => x.typ)
           |> Option.value(~default=TypSlice.temp(`Typ(Unknown(Internal)))),
         )
    | Match(e, cases) =>
      let (e', t) = elaborate(m, e);
      let (ps, es) = ListUtil.unzip(cases);
      let (ps', ptys) =
        List.map(p => elaborate_pattern(m, p, false), ps) |> ListUtil.unzip;
      let joined_pty =
        TypSlice.join_all(
          ~empty=`Typ(Unknown(Internal)) |> TypSlice.temp,
          ctx,
          ptys,
        )
        |> Option.value(~default=TypSlice.temp(`Typ(Unknown(Internal))));
      let ps'' =
        List.map2((p, t) => fresh_pat_cast(p, t, joined_pty), ps', ptys);
      let e'' = fresh_cast(e', t, joined_pty);
      let (es', etys) = List.map(elaborate(m), es) |> ListUtil.unzip;
      let joined_ety =
        TypSlice.join_all(
          ~empty=`Typ(Unknown(Internal)) |> TypSlice.temp,
          ctx,
          etys,
        )
        |> Option.value(~default=TypSlice.temp(`Typ(Unknown(Internal))));
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
  Exp.map_term(
    ~f_typslice=(cont, e) => e |> IdTagged.new_ids |> cont,
    ~f_typ=(cont, e) => e |> IdTagged.new_ids |> cont,
  );

let uexp_elab = (m: Statics.Map.t, uexp: Exp.t): ElaborationResult.t =>
  switch (elaborate(m, uexp)) {
  | exception MissingTypeInfo => DoesNotElaborate
  | (d, ty) => Elaborates(d |> fix_typ_ids, ty)
  };
