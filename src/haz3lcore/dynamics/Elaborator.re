/*
 A nice property would be that elaboration is idempotent...
 */

open Util;

exception MissingTypeInfo;

module Elaboration = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = {d: DHExp.t};
};

module ElaborationResult = {
  [@deriving sexp]
  type t =
    | Elaborates(DHExp.t, Typ.t, Delta.t)
    | DoesNotElaborate;
};

let fresh_cast = (d: DHExp.t, t1: Typ.t, t2: Typ.t): DHExp.t => {
  Typ.eq(t1, t2)
    ? d
    : {
      let d' =
        DHExp.Cast(d, t1, Typ.mk_fast(Unknown(Internal)))
        |> DHExp.fresh
        |> Casts.transition_multiple;
      DHExp.Cast(d', Typ.mk_fast(Unknown(Internal)), t2)
      |> DHExp.fresh
      |> Casts.transition_multiple;
    };
};

let fresh_pat_cast = (p: DHPat.t, t1: Typ.t, t2: Typ.t): DHPat.t => {
  Typ.eq(t1, t2)
    ? p
    : {
      let _ = print_endline("=====vvvvv===");
      let _ = print_endline(Typ.show(t1));
      let _ = print_endline(Typ.show(t2));
      let _ = print_endline("=====^^^^^===");
      Cast(
        DHPat.fresh(Cast(p, t1, Typ.mk_fast(Unknown(Internal))))
        |> Casts.pattern_fixup,
        Typ.mk_fast(Unknown(Internal)),
        t2,
      )
      |> DHPat.fresh
      |> Casts.pattern_fixup;
    };
};

let elaborated_type = (m: Statics.Map.t, uexp: UExp.t): (Typ.t, Ctx.t) => {
  let (mode, self_ty, ctx) =
    switch (Id.Map.find_opt(Exp.rep_id(uexp), m)) {
    | Some(Info.InfoExp({mode, ty, ctx, _})) => (mode, ty, ctx)
    | _ => raise(MissingTypeInfo)
    };
  let elab_ty =
    switch (mode) {
    | Syn => self_ty
    | SynFun =>
      let (ty1, ty2) = Typ.matched_arrow(ctx, self_ty);
      Typ.Arrow(ty1, ty2) |> Typ.mk_fast;
    | SynTypFun =>
      let (tpat, ty) = Typ.matched_forall(ctx, self_ty);
      let tpat = Option.value(tpat, ~default=TPat.fresh(EmptyHole));
      Typ.Forall(tpat, ty) |> Typ.mk_fast;
    // We need to remove the synswitches from this type.
    | Ana(ana_ty) => Typ.match_synswitch(ana_ty, self_ty)
    };
  (elab_ty |> Typ.normalize(ctx), ctx);
};

let elaborated_pat_type = (m: Statics.Map.t, upat: UPat.t): (Typ.t, Ctx.t) => {
  let (mode, self_ty, ctx, prev_synswitch) =
    switch (Id.Map.find_opt(UPat.rep_id(upat), m)) {
    | Some(Info.InfoPat({mode, ty, ctx, prev_synswitch, _})) => (
        mode,
        ty,
        ctx,
        prev_synswitch,
      )
    | _ => raise(MissingTypeInfo)
    };
  let elab_ty =
    switch (mode) {
    | Syn => self_ty
    | SynFun =>
      let (ty1, ty2) = Typ.matched_arrow(ctx, self_ty);
      Typ.Arrow(ty1, ty2) |> Typ.mk_fast;
    | SynTypFun =>
      let (tpat, ty) = Typ.matched_forall(ctx, self_ty);
      let tpat = Option.value(tpat, ~default=TPat.fresh(EmptyHole));
      Typ.Forall(tpat, ty) |> Typ.mk_fast;
    | Ana(ana_ty) =>
      switch (prev_synswitch) {
      | None => ana_ty
      | Some(syn_ty) => Typ.match_synswitch(syn_ty, ana_ty)
      }
    };
  (elab_ty |> Typ.normalize(ctx), ctx);
};

let rec elaborate_pattern =
        (m: Statics.Map.t, upat: UPat.t): (DHPat.t, Typ.t) => {
  let (elaborated_type, ctx) = elaborated_pat_type(m, upat);
  let cast_from = (ty, exp) => fresh_pat_cast(exp, ty, elaborated_type);
  let (term, rewrap) = UPat.unwrap(upat);
  let dpat =
    switch (term) {
    | Int(_) => upat |> cast_from(Int |> Typ.mk_fast)
    | Bool(_) => upat |> cast_from(Bool |> Typ.mk_fast)
    | Float(_) => upat |> cast_from(Float |> Typ.mk_fast)
    | String(_) => upat |> cast_from(String |> Typ.mk_fast)
    | ListLit(ps) =>
      let (ps, tys) = List.map(elaborate_pattern(m), ps) |> ListUtil.unzip;
      let inner_type =
        tys
        |> Typ.join_all(~empty=Unknown(Internal) |> Typ.mk_fast, ctx)
        |> Option.value(~default=Typ.mk_fast(Unknown(Internal)));
      ps
      |> List.map2((p, t) => fresh_pat_cast(p, t, inner_type), _, tys)
      |> (
        ps' =>
          DHPat.ListLit(ps')
          |> rewrap
          |> cast_from(List(inner_type) |> Typ.mk_fast)
      );
    | Cons(p1, p2) =>
      let (p1', ty1) = elaborate_pattern(m, p1);
      let (p2', ty2) = elaborate_pattern(m, p2);
      let ty2_inner = Typ.matched_list(ctx, ty2);
      let ty_inner =
        Typ.join(~fix=false, ctx, ty1, ty2_inner)
        |> Option.value(~default=Typ.mk_fast(Unknown(Internal)));
      let p1'' = fresh_pat_cast(p1', ty1, ty_inner);
      let p2'' = fresh_pat_cast(p2', ty2, List(ty_inner) |> Typ.mk_fast);
      DHPat.Cons(p1'', p2'')
      |> rewrap
      |> cast_from(List(ty_inner) |> Typ.mk_fast);
    | Tuple(ps) =>
      let (ps', tys) = List.map(elaborate_pattern(m), ps) |> ListUtil.unzip;
      DHPat.Tuple(ps') |> rewrap |> cast_from(Typ.Prod(tys) |> Typ.mk_fast);
    | Ap(p1, p2) =>
      let (p1', ty1) = elaborate_pattern(m, p1);
      let (p2', ty2) = elaborate_pattern(m, p2);
      let (ty1l, ty1r) = Typ.matched_arrow(ctx, ty1);
      let p1'' = fresh_pat_cast(p1', ty1, Arrow(ty1l, ty1r) |> Typ.mk_fast);
      let p2'' = fresh_pat_cast(p2', ty2, ty1l);
      DHPat.Ap(p1'', p2'') |> rewrap |> cast_from(ty1r);
    | Invalid(_)
    | EmptyHole
    | MultiHole(_)
    | Wild => upat |> cast_from(Typ.mk_fast(Unknown(Internal)))
    | Var(v) =>
      upat
      |> cast_from(
           Ctx.lookup_var(ctx, v)
           |> Option.map((x: Ctx.var_entry) => x.typ |> Typ.normalize(ctx))
           |> Option.value(~default=Typ.mk_fast(Unknown(Internal))),
         )
    // Type annotations should already appear
    | Parens(p)
    | Cast(p, _, _) =>
      let (p', ty) = elaborate_pattern(m, p);
      p' |> cast_from(ty);
    | Constructor(c) =>
      upat
      |> cast_from(
           Ctx.lookup_ctr(ctx, c)
           |> Option.map((x: Ctx.var_entry) => x.typ |> Typ.normalize(ctx))
           |> Option.value(
                ~default=
                  Typ.mk_fast(
                    Typ.Sum([BadEntry(Typ.mk_fast(Unknown(Internal)))]),
                  ),
              ),
         )
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

   [Matt] A lot of these fresh_cast calls are redundant, however if you
   want to remove one, I'd ask you instead comment it out and leave
   a comment explaining why it's redundant.  */
let rec elaborate = (m: Statics.Map.t, uexp: UExp.t): (DHExp.t, Typ.t) => {
  let (elaborated_type, ctx) = elaborated_type(m, uexp);
  let cast_from = (ty, exp) => fresh_cast(exp, ty, elaborated_type);
  let (term, rewrap) = UExp.unwrap(uexp);
  let dhexp =
    switch (term) {
    | Invalid(_)
    | EmptyHole => uexp |> cast_from(Typ.mk_fast(Typ.Unknown(Internal)))
    | MultiHole(stuff) =>
      Any.map_term(
        ~f_exp=(_, exp) => {elaborate(m, exp) |> fst},
        ~f_pat=(_, pat) => {elaborate_pattern(m, pat) |> fst},
        _,
      )
      |> List.map(_, stuff)
      |> (
        stuff =>
          DHExp.MultiHole(stuff)
          |> rewrap
          |> cast_from(Typ.mk_fast(Typ.Unknown(Internal)))
      )
    | DynamicErrorHole(e, err) =>
      let (e', _) = elaborate(m, e);
      DynamicErrorHole(e', err)
      |> rewrap
      |> cast_from(Typ.mk_fast(Unknown(Internal)));
    | Cast(e, _, _) // We remove these casts because they should be re-inserted in the recursive call
    | FailedCast(e, _, _)
    | Parens(e) =>
      let (e', ty) = elaborate(m, e);
      e' |> cast_from(ty);
    | Deferral(_) => uexp
    | Int(_) => uexp |> cast_from(Int |> Typ.mk_fast)
    | Bool(_) => uexp |> cast_from(Bool |> Typ.mk_fast)
    | Float(_) => uexp |> cast_from(Float |> Typ.mk_fast)
    | String(_) => uexp |> cast_from(String |> Typ.mk_fast)
    | ListLit(es) =>
      let (ds, tys) = List.map(elaborate(m), es) |> ListUtil.unzip;
      let inner_type =
        Typ.join_all(~empty=Typ.Unknown(Internal) |> Typ.mk_fast, ctx, tys)
        |> Option.value(~default=Typ.mk_fast(Typ.Unknown(Internal)));
      let ds' = List.map2((d, t) => fresh_cast(d, t, inner_type), ds, tys);
      Exp.ListLit(ds')
      |> rewrap
      |> cast_from(List(inner_type) |> Typ.mk_fast);
    | Constructor(c) =>
      uexp
      |> cast_from(
           Ctx.lookup_ctr(ctx, c)
           |> Option.map((x: Ctx.var_entry) => x.typ |> Typ.normalize(ctx))
           |> Option.value(
                ~default=
                  Typ.mk_fast(
                    Typ.Sum([BadEntry(Typ.mk_fast(Unknown(Internal)))]),
                  ),
              ),
         )
    | Fun(p, e, env, n) =>
      let (p', typ) = elaborate_pattern(m, p);
      let (e', tye) = elaborate(m, e);
      Exp.Fun(p', e', env, n)
      |> rewrap
      |> cast_from(Arrow(typ, tye) |> Typ.mk_fast);
    | TypFun(tpat, e, name) =>
      let (e', tye) = elaborate(m, e);
      Exp.TypFun(tpat, e', name)
      |> rewrap
      |> cast_from(Typ.Forall(tpat, tye) |> Typ.mk_fast);
    | Tuple(es) =>
      let (ds, tys) = List.map(elaborate(m), es) |> ListUtil.unzip;
      Exp.Tuple(ds) |> rewrap |> cast_from(Prod(tys) |> Typ.mk_fast);
    | Var(v) =>
      uexp
      |> cast_from(
           Ctx.lookup_var(ctx, v)
           |> Option.map((x: Ctx.var_entry) => x.typ |> Typ.normalize(ctx))
           |> Option.value(~default=Typ.mk_fast(Typ.Unknown(Internal))),
         )
    | Let(p, def, body) =>
      let add_name: (option(string), DHExp.t) => DHExp.t = (
        (name, exp) => {
          let (term, rewrap) = DHExp.unwrap(exp);
          switch (term) {
          | Fun(p, e, ctx, _) => Fun(p, e, ctx, name) |> rewrap
          | _ => exp
          };
        }
      );
      let (p, ty1) = elaborate_pattern(m, p);
      if (!Statics.is_recursive(ctx, p, def, ty1)) {
        let def = add_name(Pat.get_var(p), def);
        let (def, ty2) = elaborate(m, def);
        let (body, ty) = elaborate(m, body);
        Exp.Let(p, fresh_cast(def, ty2, ty1), body)
        |> rewrap
        |> cast_from(ty);
      } else {
        // TODO: Add names to mutually recursive functions
        // TODO: Don't add fixpoint if there already is one
        let def = add_name(Option.map(s => s ++ "+", Pat.get_var(p)), def);
        let (def, ty2) = elaborate(m, def);
        let (body, ty) = elaborate(m, body);
        let fixf = FixF(p, fresh_cast(def, ty2, ty1), None) |> DHExp.fresh;
        Exp.Let(p, fixf, body) |> rewrap |> cast_from(ty);
      };
    | FixF(p, e, env) =>
      let (p', typ) = elaborate_pattern(m, p);
      let (e', tye) = elaborate(m, e);
      Exp.FixF(p', fresh_cast(e', tye, typ), env)
      |> rewrap
      |> cast_from(typ);
    | TyAlias(_, _, e) =>
      let (e', tye) = elaborate(m, e);
      e' |> cast_from(tye);
    | Ap(dir, f, a) =>
      let (f', tyf) = elaborate(m, f);
      let (a', tya) = elaborate(m, a);
      let (tyf1, tyf2) = Typ.matched_arrow(ctx, tyf);
      let f'' = fresh_cast(f', tyf, Arrow(tyf1, tyf2) |> Typ.mk_fast);
      let a'' = fresh_cast(a', tya, tyf1);
      Exp.Ap(dir, f'', a'') |> rewrap |> cast_from(tyf2);
    | DeferredAp(f, args) =>
      let (f', tyf) = elaborate(m, f);
      let (args', tys) = List.map(elaborate(m), args) |> ListUtil.unzip;
      let (tyf1, tyf2) = Typ.matched_arrow(ctx, tyf);
      let ty_fargs = Typ.matched_prod(ctx, List.length(args), tyf1);
      let f'' =
        fresh_cast(
          f',
          tyf,
          Arrow(Prod(ty_fargs) |> Typ.mk_fast, tyf2) |> Typ.mk_fast,
        );
      let args'' = ListUtil.map3(fresh_cast, args', tys, ty_fargs);
      let remaining_args =
        List.filter(
          ((arg, _)) => Exp.is_deferral(arg),
          List.combine(args, ty_fargs),
        );
      let remaining_arg_ty =
        Prod(List.map(snd, remaining_args)) |> Typ.mk_fast;
      DeferredAp(f'', args'')
      |> rewrap
      |> cast_from(Arrow(remaining_arg_ty, tyf2) |> Typ.mk_fast);
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
        Typ.join(~fix=false, ctx, tyt, tyf)
        |> Option.value(~default=Typ.mk_fast(Typ.Unknown(Internal)));
      let c'' = fresh_cast(c', tyc, Bool |> Typ.mk_fast);
      let t'' = fresh_cast(t', tyt, ty);
      let f'' = fresh_cast(f', tyf, ty);
      Exp.If(c'', t'', f'') |> rewrap |> cast_from(ty);
    | Seq(e1, e2) =>
      let (e1', _) = elaborate(m, e1);
      let (e2', ty2) = elaborate(m, e2);
      Seq(e1', e2') |> rewrap |> cast_from(ty2);
    | Test(e) =>
      let (e', t) = elaborate(m, e);
      Test(fresh_cast(e', t, Bool |> Typ.mk_fast))
      |> rewrap
      |> cast_from(Prod([]) |> Typ.mk_fast);
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
        Typ.join(~fix=false, ctx, ty1, ty2_inner)
        |> Option.value(~default=Typ.mk_fast(Unknown(Internal)));
      let e1'' = fresh_cast(e1', ty1, ty_inner);
      let e2'' = fresh_cast(e2', ty2, List(ty_inner) |> Typ.mk_fast);
      Cons(e1'', e2'') |> rewrap |> cast_from(List(ty_inner) |> Typ.mk_fast);
    | ListConcat(e1, e2) =>
      let (e1', ty1) = elaborate(m, e1);
      let (e2', ty2) = elaborate(m, e2);
      let ty_inner1 = Typ.matched_list(ctx, ty1);
      let ty_inner2 = Typ.matched_list(ctx, ty2);
      let ty_inner =
        Typ.join(~fix=false, ctx, ty_inner1, ty_inner2)
        |> Option.value(~default=Typ.mk_fast(Unknown(Internal)));
      let e1'' = fresh_cast(e1', ty1, List(ty_inner) |> Typ.mk_fast);
      let e2'' = fresh_cast(e2', ty2, List(ty_inner) |> Typ.mk_fast);
      ListConcat(e1'', e2'')
      |> rewrap
      |> cast_from(List(ty_inner) |> Typ.mk_fast);
    | UnOp(Meta(Unquote), e) =>
      switch (e.term) {
      | Var("e") => Constructor("$e") |> rewrap
      | Var("v") => Constructor("$v") |> rewrap
      | _ =>
        DHExp.EmptyHole
        |> rewrap
        |> cast_from(Typ.mk_fast(Typ.Unknown(Internal)))
      }
    | UnOp(Int(Minus), e) =>
      let (e', t) = elaborate(m, e);
      UnOp(Int(Minus), fresh_cast(e', t, Int |> Typ.mk_fast))
      |> rewrap
      |> cast_from(Int |> Typ.mk_fast);
    | UnOp(Bool(Not), e) =>
      let (e', t) = elaborate(m, e);
      UnOp(Bool(Not), fresh_cast(e', t, Bool |> Typ.mk_fast))
      |> rewrap
      |> cast_from(Bool |> Typ.mk_fast);
    | BinOp(Int(Plus | Minus | Times | Power | Divide) as op, e1, e2) =>
      let (e1', t1) = elaborate(m, e1);
      let (e2', t2) = elaborate(m, e2);
      BinOp(
        op,
        fresh_cast(e1', t1, Int |> Typ.mk_fast),
        fresh_cast(e2', t2, Int |> Typ.mk_fast),
      )
      |> rewrap
      |> cast_from(Int |> Typ.mk_fast);
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
        fresh_cast(e1', t1, Int |> Typ.mk_fast),
        fresh_cast(e2', t2, Int |> Typ.mk_fast),
      )
      |> rewrap
      |> cast_from(Bool |> Typ.mk_fast);
    | BinOp(Bool(And | Or) as op, e1, e2) =>
      let (e1', t1) = elaborate(m, e1);
      let (e2', t2) = elaborate(m, e2);
      BinOp(
        op,
        fresh_cast(e1', t1, Bool |> Typ.mk_fast),
        fresh_cast(e2', t2, Bool |> Typ.mk_fast),
      )
      |> rewrap
      |> cast_from(Bool |> Typ.mk_fast);
    | BinOp(Float(Plus | Minus | Times | Divide | Power) as op, e1, e2) =>
      let (e1', t1) = elaborate(m, e1);
      let (e2', t2) = elaborate(m, e2);
      BinOp(
        op,
        fresh_cast(e1', t1, Float |> Typ.mk_fast),
        fresh_cast(e2', t2, Float |> Typ.mk_fast),
      )
      |> rewrap
      |> cast_from(Float |> Typ.mk_fast);
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
        fresh_cast(e1', t1, Float |> Typ.mk_fast),
        fresh_cast(e2', t2, Float |> Typ.mk_fast),
      )
      |> rewrap
      |> cast_from(Bool |> Typ.mk_fast);
    | BinOp(String(Concat) as op, e1, e2) =>
      let (e1', t1) = elaborate(m, e1);
      let (e2', t2) = elaborate(m, e2);
      BinOp(
        op,
        fresh_cast(e1', t1, String |> Typ.mk_fast),
        fresh_cast(e2', t2, String |> Typ.mk_fast),
      )
      |> rewrap
      |> cast_from(String |> Typ.mk_fast);
    | BinOp(String(Equals) as op, e1, e2) =>
      let (e1', t1) = elaborate(m, e1);
      let (e2', t2) = elaborate(m, e2);
      BinOp(
        op,
        fresh_cast(e1', t1, String |> Typ.mk_fast),
        fresh_cast(e2', t2, String |> Typ.mk_fast),
      )
      |> rewrap
      |> cast_from(Bool |> Typ.mk_fast);
    | BuiltinFun(fn) =>
      uexp
      |> cast_from(
           Ctx.lookup_var(Builtins.ctx_init, fn)
           |> Option.map((x: Ctx.var_entry) => x.typ)
           |> Option.value(~default=Typ.mk_fast(Typ.Unknown(Internal))),
         )
    | Match(e, cases) =>
      let (e', t) = elaborate(m, e);
      let (ps, es) = ListUtil.unzip(cases);
      let (ps', ptys) =
        List.map(elaborate_pattern(m), ps) |> ListUtil.unzip;
      let joined_pty =
        Typ.join_all(~empty=Typ.Unknown(Internal) |> Typ.mk_fast, ctx, ptys)
        |> Option.value(~default=Typ.mk_fast(Typ.Unknown(Internal)));
      let ps'' =
        List.map2((p, t) => fresh_pat_cast(p, t, joined_pty), ps', ptys);
      let e'' = fresh_cast(e', t, joined_pty);
      let (es', etys) = List.map(elaborate(m), es) |> ListUtil.unzip;
      let joined_ety =
        Typ.join_all(~empty=Typ.Unknown(Internal) |> Typ.mk_fast, ctx, etys)
        |> Option.value(~default=Typ.mk_fast(Typ.Unknown(Internal)));
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

let uexp_elab = (m: Statics.Map.t, uexp: UExp.t): ElaborationResult.t =>
  switch (elaborate(m, uexp)) {
  | exception MissingTypeInfo => DoesNotElaborate
  | (d, ty) => Elaborates(d, ty, Delta.empty)
  };
