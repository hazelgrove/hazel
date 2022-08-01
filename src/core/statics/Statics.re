open Sexplib.Std;

[@deriving (show({with_path: false}), sexp, yojson)]
type info_exp = {
  cls: Term.UExp.cls,
  mode: Typ.mode,
  self: Typ.self,
  ctx: Ctx.t,
  free: Ctx.co,
  // add derived attributes?
  //err: error_status,
  //typ: Typ.t,
};

//TODO(andrew): ctx-like fields to detect duplicates
[@deriving (show({with_path: false}), sexp, yojson)]
type info_pat = {
  cls: Term.UPat.cls,
  mode: Typ.mode,
  self: Typ.self,
};

[@deriving (show({with_path: false}), sexp, yojson)]
type info_typ = {
  cls: Term.UTyp.cls,
  ty: Typ.t,
};

[@deriving (show({with_path: false}), sexp, yojson)]
type info =
  | Invalid
  | InfoExp(info_exp)
  | InfoPat(info_pat)
  | InfoTyp(info_typ);

type info_map = Id.Map.t(info);

[@deriving (show({with_path: false}), sexp, yojson)]
type error =
  | FreeVariable
  | SynInconsistentBranches(list(Typ.t))
  //| InconsistentBranchesAna(list(Typ.t))
  | TypeInconsistent(Typ.t, Typ.t);

[@deriving (show({with_path: false}), sexp, yojson)]
type happy =
  | SynConsistent(Typ.t)
  | AnaConsistent(Typ.t, Typ.t, Typ.t) //ana, syn, join
  | AnaInternalInconsistent(Typ.t, list(Typ.t)) // ana, branches
  | AnaExternalInconsistent(Typ.t, Typ.t); // ana, syn

[@deriving (show({with_path: false}), sexp, yojson)]
type error_status =
  | InHole(error)
  | NotInHole(happy);

let union_m =
  List.fold_left(
    (m1, m2) => Id.Map.union((_, _, b) => Some(b), m1, m2),
    Id.Map.empty,
  );

let error_status = (mode: Typ.mode, self: Typ.self): error_status =>
  switch (mode, self) {
  | (Syn | Ana(_), Free) => InHole(FreeVariable)
  | (Syn, Just(ty)) => NotInHole(SynConsistent(ty))
  | (Syn, Joined(tys_syn))
  | (Ana(Unknown(ModeSwitch)), Joined(tys_syn)) =>
    let tys_syn = Typ.source_tys(tys_syn);
    //TODO: clarify ModeSwitch case
    switch (Typ.join_all(tys_syn)) {
    | None => InHole(SynInconsistentBranches(tys_syn))
    | Some(ty_joined) => NotInHole(SynConsistent(ty_joined))
    };
  | (Ana(ty_ana), Just(ty_syn)) =>
    switch (Typ.join(ty_ana, ty_syn)) {
    | None => InHole(TypeInconsistent(ty_syn, ty_ana))
    | Some(ty_join) => NotInHole(AnaConsistent(ty_ana, ty_syn, ty_join))
    }
  | (Ana(ty_ana), Joined(tys_syn)) =>
    // TODO: review logic of these cases
    switch (Typ.join_all(Typ.source_tys(tys_syn))) {
    | Some(ty_syn) =>
      switch (Typ.join(ty_syn, ty_ana)) {
      | None => NotInHole(AnaExternalInconsistent(ty_ana, ty_syn))
      | Some(ty_join) => NotInHole(AnaConsistent(ty_syn, ty_ana, ty_join))
      }
    | None =>
      NotInHole(AnaInternalInconsistent(ty_ana, Typ.source_tys(tys_syn)))
    }
  };

let typ_after_fix = (mode: Typ.mode, self: Typ.self): Typ.t =>
  switch (error_status(mode, self)) {
  | InHole(_) => Unknown(Internal)
  | NotInHole(SynConsistent(t)) => t
  | NotInHole(AnaConsistent(_, _, ty_join)) => ty_join
  | NotInHole(AnaExternalInconsistent(ty_ana, _)) => ty_ana
  | NotInHole(AnaInternalInconsistent(ty_ana, _)) => ty_ana
  };

let rec uexp_to_info_map =
        (~ctx=Ctx.empty, ~mode=Typ.Syn, {id, term}: Term.UExp.t)
        : (Typ.t, Ctx.co, info_map) => {
  let cls = Term.UExp.cls_of_term(term);
  let go = uexp_to_info_map(~ctx);
  let add = (~self, ~free, m) => (
    typ_after_fix(mode, self),
    free,
    Id.Map.add(id, InfoExp({cls, self, mode, ctx, free}), m),
  );
  let atomic = self => (
    typ_after_fix(mode, self),
    [],
    Id.Map.singleton(id, InfoExp({cls, self, mode, ctx, free: []})),
  );
  let binop = (e1, e2, ty1, ty2, ty_out) => {
    let (_, free1, m1) = go(~mode=ty1, e1);
    let (_, free2, m2) = go(~mode=ty2, e2);
    add(~self=ty_out, ~free=Ctx.union([free1, free2]), union_m([m1, m2]));
  };
  switch (term) {
  | Invalid(_p) => (Unknown(Internal), [], Id.Map.singleton(id, Invalid))
  | EmptyHole => atomic(Just(Unknown(Internal)))
  | Bool(_) => atomic(Just(Bool))
  | Int(_) => atomic(Just(Int))
  | Float(_) => atomic(Just(Float))
  | Var(name) =>
    switch (VarMap.lookup(ctx, name)) {
    | None => atomic(Free)
    | Some(ce) =>
      add(~self=Just(ce.typ), ~free=[(name, [{id, mode}])], Id.Map.empty)
    }
  | OpInt(Plus, e1, e2) => binop(e1, e2, Ana(Int), Ana(Int), Just(Int))
  | OpInt(Lt, e1, e2) => binop(e1, e2, Ana(Int), Ana(Int), Just(Bool))
  | OpFloat(Plus, e1, e2) =>
    binop(e1, e2, Ana(Float), Ana(Float), Just(Float))
  | OpBool(And, e1, e2) => binop(e1, e2, Ana(Bool), Ana(Bool), Just(Bool))
  | Pair(e1, e2) =>
    let (mode_l, mode_r) = Typ.matched_prod_mode(mode);
    let (ty1, free1, m1) = go(~mode=mode_l, e1);
    let (ty2, free2, m2) = go(~mode=mode_r, e2);
    add(
      ~self=Just(Prod(ty1, ty2)),
      ~free=Ctx.union([free1, free2]),
      union_m([m1, m2]),
    );
  | If(cond, e1, e2) =>
    let (_, free_e0, m1) = go(~mode=Ana(Bool), cond);
    let (ty_e1, free_e1, m2) = go(~mode, e1);
    let (ty_e2, free_e2, m3) = go(~mode, e2);
    add(
      ~self=Joined([{id: e1.id, ty: ty_e1}, {id: e2.id, ty: ty_e2}]),
      ~free=Ctx.union([free_e0, free_e1, free_e2]),
      union_m([m1, m2, m3]),
    );
  | Ap(fn, arg) =>
    // NOTE: funpos currently set to Ana instead of Syn
    let (ty_fn, free_fn, m_fn) =
      uexp_to_info_map(
        ~ctx,
        ~mode=Ana(Arrow(Unknown(ModeSwitch), Unknown(ModeSwitch))),
        fn,
      );
    let (ty_in, ty_out) = Typ.matched_arrow(ty_fn);
    let (_, free_arg, m_arg) =
      uexp_to_info_map(~ctx, ~mode=Ana(ty_in), arg);
    add(
      ~self=Just(ty_out),
      ~free=Ctx.union([free_fn, free_arg]),
      union_m([m_fn, m_arg]),
    );
  | Fun(pat, body) =>
    let (mode_pat, mode_body) = Typ.matched_arrow_mode(mode);
    let (ty_pat, ctx_pat, m_pat) = upat_to_info_map(~mode=mode_pat, pat);
    let ctx_body = VarMap.union(ctx, ctx_pat);
    let (ty_body, free_body, m_body) =
      uexp_to_info_map(~ctx=ctx_body, ~mode=mode_body, body);
    add(
      ~self=Just(Arrow(ty_pat, ty_body)),
      ~free=Ctx.subtract(ctx_pat, free_body),
      union_m([m_pat, m_body]),
    );
  | FunAnn(pat, typ, body) =>
    let (ty_ann, m_typ) = utyp_to_info_map(typ);
    let (mode_pat, mode_body) =
      switch (mode) {
      | Syn => (Typ.Syn, Typ.Syn)
      | Ana(ty) =>
        let (ty_in, ty_out) = Typ.matched_arrow(ty);
        let ty_in' = Typ.join_or_fst(ty_ann, ty_in);
        (Ana(ty_in'), Ana(ty_out));
      };
    let (ty_pat, ctx_pat, m_pat) = upat_to_info_map(~mode=mode_pat, pat);
    let ctx_body = VarMap.union(ctx_pat, ctx);
    let (ty_body, free_body, m_body) =
      uexp_to_info_map(~ctx=ctx_body, ~mode=mode_body, body);
    add(
      ~self=Just(Arrow(ty_pat, ty_body)),
      ~free=Ctx.subtract(ctx_pat, free_body),
      union_m([m_pat, m_body, m_typ]),
    );
  | Let(pat, def, body) =>
    let (ty_pat, _ctx_pat, _m_pat) = upat_to_info_map(~mode=Syn, pat);
    let (ty_def, free_def, m_def) =
      uexp_to_info_map(~ctx, ~mode=Ana(ty_pat), def);
    // ana pat to incorporate def type into ctx
    let (_, ctx_pat_ana, m_pat) = upat_to_info_map(~mode=Ana(ty_def), pat);
    let ctx = VarMap.union(ctx, ctx_pat_ana);
    let (ty_body, free_body, m_body) = uexp_to_info_map(~ctx, ~mode, body);
    add(
      ~self=Just(ty_body),
      ~free=Ctx.union([free_def, Ctx.subtract(ctx_pat_ana, free_body)]),
      union_m([m_pat, m_def, m_body]),
    );
  | LetAnn(pat, typ, def, body) =>
    let (ty_ann, m_typ) = utyp_to_info_map(typ);
    let (ty_pat, _ctx_pat, m_pat) =
      upat_to_info_map(~mode=Ana(ty_ann), pat);
    let (ty_def, free_def, m_def) =
      uexp_to_info_map(~ctx, ~mode=Ana(ty_pat), def);
    // join if consistent, otherwise pattern type wins
    let joint_ty = Typ.join_or_fst(ty_pat, ty_def);
    // ana pat to incorporate def type into ctx
    let (_, ctx_pat_ana, _) = upat_to_info_map(~mode=Ana(joint_ty), pat);
    let ctx_body = VarMap.union(ctx, ctx_pat_ana);
    let (ty_body, free_body, m_body) =
      uexp_to_info_map(~ctx=ctx_body, ~mode, body);
    add(
      ~self=Just(ty_body),
      ~free=Ctx.union([free_def, Ctx.subtract(ctx_pat_ana, free_body)]),
      union_m([m_pat, m_typ, m_def, m_body]),
    );
  };
}
and upat_to_info_map =
    (~mode: Typ.mode=Typ.Syn, {id, term}: Term.UPat.t)
    : (Typ.t, Ctx.t, info_map) => {
  let cls = Term.UPat.cls_of_term(term);
  let add = (self: Typ.self) => (
    typ_after_fix(mode, self),
    [],
    Id.Map.singleton(id, InfoPat({cls, mode, self})),
  );
  switch (term) {
  | Invalid(_p) => add(Free)
  | EmptyHole => add(Just(Unknown(ModeSwitch)))
  | Wild => add(Just(Unknown(ModeSwitch)))
  | Int(_) => add(Just(Int))
  | Float(_) => add(Just(Float))
  | Bool(_) => add(Just(Bool))
  | Var(name) =>
    let typ = typ_after_fix(mode, Just(Unknown(ModeSwitch)));
    (
      typ,
      [(name, Ctx.{id, typ})],
      Id.Map.singleton(
        id,
        InfoPat({cls, mode, self: Just(Unknown(ModeSwitch))}),
      ),
    );
  | Pair(p1, p2) =>
    let (mode_l, mode_r) = Typ.matched_prod_mode(mode);
    let (ty_p1, ctx_p1, m_p1) = upat_to_info_map(~mode=mode_l, p1);
    let (ty_p2, ctx_p2, m_p2) = upat_to_info_map(~mode=mode_r, p2);
    let ty = Typ.Prod(ty_p1, ty_p2);
    (
      ty,
      VarMap.union(ctx_p1, ctx_p2),
      Id.Map.add(
        id,
        InfoPat({cls, mode, self: Just(ty)}),
        union_m([m_p1, m_p2]),
      ),
    );
  };
}
and utyp_to_info_map = ({id, term} as utyp: Term.UTyp.t): (Typ.t, info_map) => {
  let cls = Term.UTyp.cls_of_term(term);
  let ty = Term.utyp_to_ty(utyp);
  let ret = m => (ty, Id.Map.add(id, InfoTyp({cls, ty}), m));
  switch (term) {
  | Invalid(_)
  | EmptyHole
  | Int
  | Float
  | Bool => ret(Id.Map.empty)
  | Arrow(t1, t2)
  | Prod(t1, t2) =>
    let (_, m_t1) = utyp_to_info_map(t1);
    let (_, m_t2) = utyp_to_info_map(t2);
    ret(union_m([m_t1, m_t2]));
  };
};

/*

 planning for a better let:

 HAZEL:
        let* ty_p = Statics_Pat.syn_moded(ctx, p);
        let def_ctx = extend_let_def_ctx(ctx, p, def);
        let* _ = ana(def_ctx, def, ty_p);
        let* ty_def = syn(def_ctx, def);
        Statics_Pat.ana(ctx, p, ty_def);

        here: want to make sure that the info in the pattern
        takes into account the type of the definition and vice versa.
        but don't want to have errors in the pattern due to the definition, only augmenting the type.
        we do want to have errors in the definition due to the pattern, as well as augmentation.
        idea:
        get the self types of both (call with mode=Syn?). this doesn't actually need a context atm.
        ignore other returned values from those calls.
        if they are consistent, call again on pat in NEW mode Augment(ty). this is same
        as Ana(ty) in that it propagates type information down but where we also know
        the thing can synethesize something consistent with ty.
        if they are not consistent, use pat info_map from original Syn call.
        for the def, just call Ana(pat_ty) on it and merge with the stuff from the pat.
        finally, call on the body with the new ctx are whatever the original mode was.

        */
