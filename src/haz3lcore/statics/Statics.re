open Term;

/* STATICS.re

   This module determines the statics semantics of a program.
   It makes use of the following modules:

   INFO.re: Defines the Info.t type which is used to represent the
   static STATUS of a term. This STATUS can be either OK or ERROR,
   and is determined by reconcilling two sources of typing information,
   the MODE and the SELF.

   MODE.re: Defines the Mode.t type which is used to represent the
   typing expectations imposed by a term's ancestors.

   SELF.re: Define the Self.t type which is used to represent the
   type information derivable from the term itself.

   The point of STATICS.re itself is to derive a map between each
   term's unique id and that term's static INFO. The below functions
   are intended mostly as infrastructure: The point is to define a
   traversal through the syntax tree which, for each term, passes
   down the MODE, passes up the SELF, calculates the INFO, and adds
   it to the map.

   The architectural intention here is that most type-manipulation
   logic is defined in INFO, MODE, and SELF, and the STATICS module
   itself is dedicated to the piping necessary to (A) introduce and
   (B) propagate the necessary information through the syntax tree.

    */

module Info = Info;

module Map = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = Id.Map.t(Info.t);
};

<<<<<<< HEAD
/* Patterns are assigned a mode (reflecting the static expectations
   if any of their syntactic parent) and a self (reflecting what their
   statics would be in isolation), a context (variables in scope) */
[@deriving (show({with_path: false}), sexp, yojson)]
type info_pat = {
  cls: Term.UPat.cls,
  term: Term.UPat.t,
  mode: Typ.mode,
  self: Typ.self,
  ctx: Ctx.t // TODO: detect in-pattern shadowing
};

/* (Syntactic) Types are assigned their corresponding semantic type. */
[@deriving (show({with_path: false}), sexp, yojson)]
type info_typ = {
  cls: Term.UTyp.cls,
  term: Term.UTyp.t,
  self: Typ.self,
};

[@deriving (show({with_path: false}), sexp, yojson)]
type info_rul = {
  cls: Term.URul.cls,
  term: Term.UExp.t,
};

/* The Info aka Cursorinfo assigned to each subterm. */
[@deriving (show({with_path: false}), sexp, yojson)]
type t =
  | Invalid(TermBase.parse_flag)
  | InfoExp(info_exp)
  | InfoPat(info_pat)
  | InfoTyp(info_typ)
  | InfoRul(info_rul);

/* The InfoMap collating all info for a composite term */
type map = Id.Map.t(t);

let terms = (map: map): Id.Map.t(Term.any) =>
  map
  |> Id.Map.filter_map(_ =>
       fun
       | Invalid(_) => None
       | InfoExp({term, _}) => Some(Term.Exp(term))
       | InfoPat({term, _}) => Some(Term.Pat(term))
       | InfoTyp({term, _}) => Some(Term.Typ(term))
       | InfoRul({term, _}) => Some(Term.Exp(term))
     );

/* Static error classes */
[@deriving (show({with_path: false}), sexp, yojson)]
type error =
  | Free(Typ.free_errors)
  | Multi
  | NoFun(Typ.t)
  | SynInconsistentBranches(list(Typ.t))
  | TypeInconsistent(Typ.t, Typ.t);

/* Statics non-error classes */
[@deriving (show({with_path: false}), sexp, yojson)]
type happy =
  | SynConsistent(Typ.t)
  | AnaConsistent(Typ.t, Typ.t, Typ.t) //ana, syn, join
  | AnaInternalInconsistent(Typ.t, list(Typ.t)) // ana, branches
  | AnaExternalInconsistent(Typ.t, Typ.t); // ana, syn

/* The error status which 'wraps' each term. */
[@deriving (show({with_path: false}), sexp, yojson)]
type error_status =
  | InHole(error)
  | NotInHole(happy);

/* Determines whether an expression or pattern is in an error hole,
   depending on the mode, which represents the expectations of the
   surrounding syntactic context, and the self which represents the
   makeup of the expression / pattern itself. */
let error_status = (mode: Typ.mode, self: Typ.self): error_status =>
  switch (mode, self) {
  | (SynFun, Just(ty)) =>
    switch (
      Typ.join(Arrow(Unknown(NoProvenance), Unknown(NoProvenance)), ty)
    ) {
    | None => InHole(NoFun(ty))
    | Some(_) => NotInHole(SynConsistent(ty))
    }
  | (SynFun, Joined(_wrap, tys_syn)) =>
    let tys_syn = Typ.source_tys(tys_syn);
    switch (Typ.join_all(tys_syn)) {
    | None => InHole(SynInconsistentBranches(tys_syn))
    | Some(ty_joined) =>
      switch (
        Typ.join(
          Arrow(Unknown(NoProvenance), Unknown(NoProvenance)),
          ty_joined,
        )
      ) {
      | None => InHole(NoFun(ty_joined))
      | Some(_) => NotInHole(SynConsistent(ty_joined))
      }
    };
  | (Syn | SynFun | Ana(_), Free(free_error)) => InHole(Free(free_error))
  | (Syn | SynFun | Ana(_), Multi) =>
    NotInHole(SynConsistent(Unknown(NoProvenance)))
  | (Syn, Just(ty)) => NotInHole(SynConsistent(ty))
  | (Syn, Joined(wrap, tys_syn)) =>
    let tys_syn = Typ.source_tys(tys_syn);
    switch (Typ.join_all(tys_syn)) {
    | None => InHole(SynInconsistentBranches(tys_syn))
    | Some(ty_joined) => NotInHole(SynConsistent(wrap(ty_joined)))
    };
  | (Ana(ty_ana), Just(ty_syn)) =>
    switch (Typ.join(ty_ana, ty_syn)) {
    | None => InHole(TypeInconsistent(ty_syn, ty_ana))
    | Some(ty_join) => NotInHole(AnaConsistent(ty_ana, ty_syn, ty_join))
    }
  | (Ana(ty_ana), Joined(wrap, tys_syn)) =>
    // TODO: review logic of these cases
    switch (Typ.join_all(Typ.source_tys(tys_syn))) {
    | Some(ty_syn) =>
      let ty_syn = wrap(ty_syn);
      switch (Typ.join(ty_syn, ty_ana)) {
      | None => NotInHole(AnaExternalInconsistent(ty_ana, ty_syn))
      | Some(ty_join) => NotInHole(AnaConsistent(ty_syn, ty_ana, ty_join))
      };
    | None =>
      NotInHole(AnaInternalInconsistent(ty_ana, Typ.source_tys(tys_syn)))
    }
  };

/* Determines whether any term is in an error hole. Currently types cannot
   be in error, and Invalids (things to which Term was unable to assign a
   parse) are always in error. The error status of expressions and patterns
   are determined by error_status above. */
let is_error = (ci: t): bool => {
  switch (ci) {
  | Invalid(Secondary) => false
  | Invalid(_) => true
  | InfoExp({mode, self, _})
  | InfoPat({mode, self, _}) =>
    switch (error_status(mode, self)) {
    | InHole(_) => true
    | NotInHole(_) => false
    }
  | InfoTyp({self, _}) =>
    switch (self) {
    | Free(TypeVariable) => true
    | _ => false
    }
  | InfoRul(_) => false //TODO
  };
};

/* Determined the type of an expression or pattern 'after hole wrapping';
   that is, all ill-typed terms are considered to be 'wrapped in
   non-empty holes', i.e. assigned Unknown type. */
let typ_after_fix = (mode: Typ.mode, self: Typ.self, termId: Id.t): Typ.t =>
  switch (error_status(mode, self)) {
  | InHole(_) => Unknown(AstNode(termId))
  | NotInHole(SynConsistent(t)) => t
  | NotInHole(AnaConsistent(_, _, ty_join)) => ty_join
  | NotInHole(AnaExternalInconsistent(ty_ana, _)) => ty_ana
  | NotInHole(AnaInternalInconsistent(ty_ana, _)) => ty_ana
  };

/* The type of an expression after hole wrapping */
let exp_typ = (m: map, e: Term.UExp.t): Typ.t =>
  switch (Id.Map.find_opt(Term.UExp.rep_id(e), m)) {
  | Some(InfoExp({mode, self, _})) =>
    typ_after_fix(mode, self, Term.UExp.rep_id(e))
  | Some(InfoPat(_) | InfoTyp(_) | InfoRul(_) | Invalid(_))
  | None => failwith(__LOC__ ++ ": XXX")
  };

let exp_self_typ_id = (m: map, id): Typ.t =>
  switch (Id.Map.find_opt(id, m)) {
  | Some(InfoExp({self, _})) => Typ.t_of_self(self)
  | Some(InfoPat(_) | InfoTyp(_) | InfoRul(_) | Invalid(_))
  | None => failwith(__LOC__ ++ ": XXX")
  };

let exp_self_typ = (m: map, e: Term.UExp.t): Typ.t =>
  exp_self_typ_id(m, Term.UExp.rep_id(e));

let exp_mode_id = (m: map, id): Typ.mode =>
  switch (Id.Map.find_opt(id, m)) {
  | Some(InfoExp({mode, _})) => mode
  | Some(InfoPat(_) | InfoTyp(_) | InfoRul(_) | Invalid(_))
  | None => failwith(__LOC__ ++ ": XXX")
  };

let exp_mode = (m: map, e: Term.UExp.t): Typ.mode =>
  exp_mode_id(m, Term.UExp.rep_id(e));

/* The type of a pattern after hole wrapping */
let pat_typ = (m: map, p: Term.UPat.t): Typ.t =>
  switch (Id.Map.find_opt(Term.UPat.rep_id(p), m)) {
  | Some(InfoPat({mode, self, _})) =>
    typ_after_fix(mode, self, Term.UPat.rep_id(p))
  | Some(InfoExp(_) | InfoTyp(_) | InfoRul(_) | Invalid(_))
  | None => failwith(__LOC__ ++ ": XXX")
  };
let pat_self_typ = (m: map, p: Term.UPat.t): Typ.t =>
  switch (Id.Map.find_opt(Term.UPat.rep_id(p), m)) {
  | Some(InfoPat({self, _})) => Typ.t_of_self(self)
  | Some(InfoExp(_) | InfoTyp(_) | InfoRul(_) | Invalid(_))
  | None => failwith(__LOC__ ++ ": XXX")
  };

let union_m =
=======
let map_m = (f, xs, m: Map.t) =>
>>>>>>> llmass
  List.fold_left(
    ((xs, m), x) => f(x, m) |> (((x, m)) => (xs @ [x], m)),
    ([], m),
    xs,
  );

let add_info = (ids: list(Id.t), info: Info.t, m: Map.t): Map.t =>
  ids |> List.fold_left((m, id) => Id.Map.add(id, info, m), m);

let extend_let_def_ctx =
    (ctx: Ctx.t, pat: UPat.t, pat_ctx: Ctx.t, def: UExp.t): Ctx.t =>
  if (UPat.is_tuple_of_arrows(pat) && UExp.is_tuple_of_functions(def)) {
    pat_ctx;
  } else {
    ctx;
  };

let typ_exp_binop_bin_int: UExp.op_bin_int => Typ.t =
  fun
  | (Plus | Minus | Times | Power | Divide) as _op => Int
  | (
      LessThan | GreaterThan | LessThanOrEqual | GreaterThanOrEqual | Equals |
      NotEquals
    ) as _op =>
    Bool;

let typ_exp_binop_bin_float: UExp.op_bin_float => Typ.t =
  fun
  | (Plus | Minus | Times | Power | Divide) as _op => Float
  | (
      LessThan | GreaterThan | LessThanOrEqual | GreaterThanOrEqual | Equals |
      NotEquals
    ) as _op =>
    Bool;

let typ_exp_binop_bin_string: UExp.op_bin_string => Typ.t =
  fun
  | Concat => String
  | Equals => Bool;

let typ_exp_binop: UExp.op_bin => (Typ.t, Typ.t, Typ.t) =
  fun
  | Bool(And | Or) => (Bool, Bool, Bool)
  | Int(op) => (Int, Int, typ_exp_binop_bin_int(op))
  | Float(op) => (Float, Float, typ_exp_binop_bin_float(op))
  | String(op) => (String, String, typ_exp_binop_bin_string(op));

let typ_exp_unop: UExp.op_un => (Typ.t, Typ.t) =
  fun
  | Bool(Not) => (Bool, Bool)
  | Int(Minus) => (Int, Int);

<<<<<<< HEAD
let join_constraints = (tys: list(Typ.t)): Typ.constraints => {
  // find first elt containing hole and constrain it to every other elt
  let elts_with_hole = List.filter(Typ.contains_hole, tys);
  switch (elts_with_hole) {
  | [] => []
  | [hd, ..._] =>
    let constrain_rep_to_elt =
        (acc: Typ.constraints, curr: Typ.t): Typ.constraints => {
      [(hd, curr), ...acc];
    };
    List.fold_left(constrain_rep_to_elt, [], tys);
  };
};

let subsumption_constraints = (mode: Typ.mode, final_typ: Typ.t) => {
  switch (mode) {
  | Ana(expected_typ) => [(final_typ, expected_typ)]
  | _ => []
  };
};

let rec any_to_info_map =
        (~ctx: Ctx.t, any: Term.any): (Ctx.co, map, Typ.constraints) =>
  switch (any) {
  | Exp(e) =>
    let (_, co, map, constraints) = uexp_to_info_map(~ctx, e);
    (co, map, constraints);
  | Pat(p) =>
    let (_, _, map, constraints) =
      upat_to_info_map(~is_synswitch=false, ~ctx, p);
    (VarMap.empty, map, constraints);
  | Typ(ty) =>
    let (_, map) = utyp_to_info_map(ty);
    (VarMap.empty, map, []);
  // TODO(d) consider Rul case
  | Rul(_)
  | Nul ()
  | Any () => (VarMap.empty, Id.Map.empty, [])
=======
let rec any_to_info_map =
        (~ctx: Ctx.t, ~ancestors, any: any, m: Map.t): (CoCtx.t, Map.t) =>
  switch (any) {
  | Exp(e) =>
    let ({co_ctx, _}: Info.exp, m) =
      uexp_to_info_map(~ctx, ~ancestors, e, m);
    (co_ctx, m);
  | Pat(p) =>
    let m =
      upat_to_info_map(
        ~is_synswitch=false,
        ~co_ctx=CoCtx.empty,
        ~ancestors,
        ~ctx,
        p,
        m,
      )
      |> snd;
    (CoCtx.empty, m);
  | TPat(tp) => (
      CoCtx.empty,
      utpat_to_info_map(~ctx, ~ancestors, tp, m) |> snd,
    )
  | Typ(ty) => (
      CoCtx.empty,
      utyp_to_info_map(~ctx, ~ancestors, ty, m) |> snd,
    )
  | Rul(_)
  | Nul ()
  | Any () => (CoCtx.empty, m)
>>>>>>> llmass
  }
and multi = (~ctx, ~ancestors, m, tms) =>
  List.fold_left(
    ((co_ctxs, m), any) => {
      let (co_ctx, m) = any_to_info_map(~ctx, ~ancestors, any, m);
      (co_ctxs @ [co_ctx], m);
    },
    ([], m),
    tms,
  )
and uexp_to_info_map =
<<<<<<< HEAD
    (~ctx: Ctx.t, ~mode=Typ.Syn, {ids, term} as uexp: Term.UExp.t)
    : (Typ.t, Ctx.co, map, Typ.constraints) => {
  /* Maybe switch mode to syn */
  let mode =
    switch (mode) {
    | Ana(Unknown(SynSwitch(_))) => Typ.Syn
    | _ => mode
    };
  let cls = Term.UExp.cls_of_term(term);
  let go = uexp_to_info_map(~ctx);
  let add = (~self: Typ.self, ~free, m, constraints) => {
    let joined_constraints =
      switch (self) {
      | Joined(wrap, sources) =>
        sources |> Typ.source_tys |> List.map(wrap) |> join_constraints
      | _ => []
      };
    (
      typ_after_fix(mode, self, Term.UExp.rep_id(uexp)),
      free,
      add_info(ids, InfoExp({cls, self, mode, ctx, free, term: uexp}), m),
      joined_constraints @ constraints,
    );
  };
  let atomic = self =>
    add(
      ~self,
      ~free=[],
      Id.Map.empty,
      subsumption_constraints(
        mode,
        typ_after_fix(mode, self, Term.UExp.rep_id(uexp)),
      ),
    );
  switch (term) {
  | Invalid(msg) =>
    let final_typ: Typ.t = Unknown(AstNode(Term.UExp.rep_id(uexp)));
    (
      final_typ,
      [],
      add_info(ids, Invalid(msg), Id.Map.empty),
      subsumption_constraints(mode, final_typ),
    );
  | MultiHole(tms) =>
    let info = tms |> List.map(any_to_info_map(~ctx));
    let free = List.map(((f, _, _)) => f, info);
    let maps = List.map(((_, m, _)) => m, info);
    let constraints = List.map(((_, _, c)) => c, info) |> List.flatten;
    let constraints =
      constraints
      @ subsumption_constraints(
          mode,
          typ_after_fix(mode, Multi, Term.UExp.rep_id(uexp)),
        );
    add(~self=Multi, ~free=Ctx.union(free), union_m(maps), constraints);
  | EmptyHole => atomic(Just(Unknown(AstNode(Term.UExp.rep_id(uexp)))))
=======
    (
      ~ctx: Ctx.t,
      ~mode=Mode.Syn,
      ~ancestors,
      {ids, term} as uexp: UExp.t,
      m: Map.t,
    )
    : (Info.exp, Map.t) => {
  /* Maybe switch mode to syn */
  let mode =
    switch (mode) {
    | Ana(Unknown(SynSwitch)) => Mode.Syn
    | _ => mode
    };
  let add' = (~self, ~co_ctx, m) => {
    let info =
      Info.derived_exp(~uexp, ~ctx, ~mode, ~ancestors, ~self, ~co_ctx);
    (info, add_info(ids, InfoExp(info), m));
  };
  let add = (~self, ~co_ctx, m) => add'(~self=Common(self), ~co_ctx, m);
  let ancestors = [UExp.rep_id(uexp)] @ ancestors;
  let go' = uexp_to_info_map(~ancestors);
  let go = go'(~ctx);
  let map_m_go = m =>
    List.fold_left2(
      ((es, m), mode, e) =>
        go(~mode, e, m) |> (((e, m)) => (es @ [e], m)),
      ([], m),
    );
  let go_pat = upat_to_info_map(~ctx, ~ancestors);
  let atomic = self => add(~self, ~co_ctx=CoCtx.empty, m);
  switch (term) {
  | MultiHole(tms) =>
    let (co_ctxs, m) = multi(~ctx, ~ancestors, m, tms);
    add(~self=IsMulti, ~co_ctx=CoCtx.union(co_ctxs), m);
  | Invalid(token) => atomic(BadToken(token))
  | EmptyHole => atomic(Just(Unknown(Internal)))
>>>>>>> llmass
  | Triv => atomic(Just(Prod([])))
  | Bool(_) => atomic(Just(Bool))
  | Int(_) => atomic(Just(Int))
  | Float(_) => atomic(Just(Float))
  | String(_) => atomic(Just(String))
<<<<<<< HEAD
  | ListLit([]) => atomic(Just(List(Unknown(NoProvenance))))
  | ListLit(es) =>
    let (modes, list_of_match_constraints) =
      List.init(List.length(es), _ =>
        Typ.matched_list_mode(mode, Term.UExp.rep_id(uexp))
      )
      |> List.split;
    let match_constraints = List.flatten(list_of_match_constraints);
    let e_ids = List.map(Term.UExp.rep_id, es);
    let infos = List.map2((e, mode) => go(~mode, e), es, modes);
    let tys = List.map(((ty, _, _, _)) => ty, infos);
    let constraints = List.map(((_, _, _, c)) => c, infos) |> List.flatten;
    let self: Typ.self =
      switch (Typ.join_all(tys)) {
      | None =>
        Joined(
          ty => List(ty),
          List.map2((id, ty) => Typ.{id, ty}, e_ids, tys),
        )
      | Some(ty) => Just(List(ty))
      };
    let free = Ctx.union(List.map(((_, f, _, _)) => f, infos));
    let m = union_m(List.map(((_, _, m, _)) => m, infos));
    add(~self, ~free, m, match_constraints @ constraints);
  | Cons(e1, e2) =>
    let (mode_e, match_constraints) =
      Typ.matched_list_mode(mode, Term.UExp.rep_id(uexp));
    let (ty1, free1, m1, constraints1) = go(~mode=mode_e, e1);
    let (_, free2, m2, constraints2) = go(~mode=Ana(List(ty1)), e2);
    add(
      ~self=Just(List(ty1)),
      ~free=Ctx.union([free1, free2]),
      union_m([m1, m2]),
      match_constraints @ constraints1 @ constraints2,
    );
  | Var(name) =>
    switch (Ctx.lookup_var(ctx, name)) {
    | None => atomic(Free(Variable))
    | Some(var) =>
      add(
        ~self=Just(var.typ),
        ~free=[(name, [{id: Term.UExp.rep_id(uexp), mode}])],
        Id.Map.empty,
        subsumption_constraints(mode, var.typ),
      )
    }
  | Parens(e) =>
    let (ty, free, m, constraints) = go(~mode, e);
    add(~self=Just(ty), ~free, m, constraints);
  | UnOp(op, e) =>
    let (ty_in, ty_out) = typ_exp_unop(op);
    let (_, free, m, constraints) = go(~mode=Ana(ty_in), e);
    add(
      ~self=Just(ty_out),
      ~free,
      m,
      subsumption_constraints(mode, ty_out) @ constraints,
    );
  | BinOp(op, e1, e2) =>
    let (ty1, ty2, ty_out) = typ_exp_binop(op);
    let (_, free1, m1, constraints1) = go(~mode=Ana(ty1), e1);
    let (_, free2, m2, constraints2) = go(~mode=Ana(ty2), e2);
    add(
      ~self=Just(ty_out),
      ~free=Ctx.union([free1, free2]),
      union_m([m1, m2]),
      subsumption_constraints(mode, ty_out) @ constraints1 @ constraints2,
    );
  | Tuple(es) =>
    let (modes, match_constraints) =
      Typ.matched_prod_mode(mode, List.length(es));
    let infos = List.map2((e, mode) => go(~mode, e), es, modes);
    let free = Ctx.union(List.map(((_, f, _, _)) => f, infos));
    let final_typ = Typ.Prod(List.map(((ty, _, _, _)) => ty, infos));
    let self = Typ.Just(final_typ);
    let m = union_m(List.map(((_, _, m, _)) => m, infos));
    let constraints = List.map(((_, _, _, c)) => c, infos) |> List.flatten;
    add(~self, ~free, m, match_constraints @ constraints);
  | Tag(name) =>
    switch (BuiltinADTs.get_tag_typ(name)) {
    | None => atomic(Free(Tag))
    | Some(typ) => atomic(Just(typ))
    }
  | Test(test) =>
    let (_, free_test, m1, constraints) = go(~mode=Ana(Bool), test);
    add(~self=Just(Prod([])), ~free=free_test, m1, constraints);
  | If(cond, e1, e2) =>
    let (_, free_e0, m1, constraints1) = go(~mode=Ana(Bool), cond);
    let (ty_e1, free_e1, m2, constraints2) = go(~mode, e1);
    let (ty_e2, free_e2, m3, constraints3) = go(~mode, e2);
    add(
      ~self=
        Joined(
          Fun.id,
          [
            {id: Term.UExp.rep_id(e1), ty: ty_e1},
            {id: Term.UExp.rep_id(e2), ty: ty_e2},
          ],
        ),
      ~free=Ctx.union([free_e0, free_e1, free_e2]),
      union_m([m1, m2, m3]),
      constraints1 @ constraints2 @ constraints3,
=======
  | ListLit(es) =>
    let ids = List.map(UExp.rep_id, es);
    let modes = Mode.of_list_lit(ctx, List.length(es), mode);
    let (es, m) = map_m_go(m, modes, es);
    let tys = List.map(Info.exp_ty, es);
    add(
      ~self=Self.listlit(~empty=Unknown(Internal), ctx, tys, ids),
      ~co_ctx=CoCtx.union(List.map(Info.exp_co_ctx, es)),
      m,
    );
  | Cons(hd, tl) =>
    let (hd, m) = go(~mode=Mode.of_cons_hd(ctx, mode), hd, m);
    let (tl, m) = go(~mode=Mode.of_cons_tl(ctx, mode, hd.ty), tl, m);
    add(
      ~self=Just(List(hd.ty)),
      ~co_ctx=CoCtx.union([hd.co_ctx, tl.co_ctx]),
      m,
    );
  | ListConcat(e1, e2) =>
    let ids = List.map(Term.UExp.rep_id, [e1, e2]);
    let mode = Mode.of_list_concat(mode);
    let (e1, m) = go(~mode, e1, m);
    let (e2, m) = go(~mode, e2, m);
    add(
      ~self=Self.list_concat(ctx, [e1.ty, e2.ty], ids),
      ~co_ctx=CoCtx.union([e1.co_ctx, e2.co_ctx]),
      m,
    );
  | Var(name) =>
    add'(
      ~self=Self.of_exp_var(ctx, name),
      ~co_ctx=CoCtx.singleton(name, UExp.rep_id(uexp), Mode.ty_of(mode)),
      m,
    )
  | Parens(e) =>
    let (e, m) = go(~mode, e, m);
    add(~self=Just(e.ty), ~co_ctx=e.co_ctx, m);
  | UnOp(op, e) =>
    let (ty_in, ty_out) = typ_exp_unop(op);
    let (e, m) = go(~mode=Ana(ty_in), e, m);
    add(~self=Just(ty_out), ~co_ctx=e.co_ctx, m);
  | BinOp(op, e1, e2) =>
    let (ty1, ty2, ty_out) = typ_exp_binop(op);
    let (e1, m) = go(~mode=Ana(ty1), e1, m);
    let (e2, m) = go(~mode=Ana(ty2), e2, m);
    add(~self=Just(ty_out), ~co_ctx=CoCtx.union([e1.co_ctx, e2.co_ctx]), m);
  | Tuple(es) =>
    let modes = Mode.of_prod(ctx, mode, List.length(es));
    let (es, m) = map_m_go(m, modes, es);
    add(
      ~self=Just(Prod(List.map(Info.exp_ty, es))),
      ~co_ctx=CoCtx.union(List.map(Info.exp_co_ctx, es)),
      m,
>>>>>>> llmass
    );
  | Test(e) =>
    let (e, m) = go(~mode=Ana(Bool), e, m);
    add(~self=Just(Prod([])), ~co_ctx=e.co_ctx, m);
  | Seq(e1, e2) =>
<<<<<<< HEAD
    let (_, free1, m1, constraints1) = go(~mode=Syn, e1);
    let (ty2, free2, m2, constraints2) = go(~mode, e2);
    add(
      ~self=Just(ty2),
      ~free=Ctx.union([free1, free2]),
      union_m([m1, m2]),
      constraints1 @ constraints2,
    );
  | Ap(fn, arg) =>
    /* Function position mode Ana(Hole->Hole) instead of Syn */
    let (ty_fn, free_fn, m_fn, constraints1) =
      uexp_to_info_map(~ctx, ~mode=Typ.ap_mode, fn);
    let ((ty_in, ty_out), match_constraints) =
      Typ.matched_arrow(ty_fn, Term.UExp.rep_id(uexp));
    let (_, free_arg, m_arg, constraints2) =
      uexp_to_info_map(~ctx, ~mode=Ana(ty_in), arg);
    add(
      ~self=Just(ty_out),
      ~free=Ctx.union([free_fn, free_arg]),
      union_m([m_fn, m_arg]),
      match_constraints
      @ constraints1
      @ constraints2
      @ subsumption_constraints(mode, ty_out),
    );
  | Fun(pat, body) =>
    let ((mode_pat, mode_body), match_constraints) =
      Typ.matched_arrow_mode(mode, Term.UExp.rep_id(uexp));
    let (ty_pat, ctx_pat, m_pat, constraints1) =
      upat_to_info_map(~is_synswitch=false, ~mode=mode_pat, pat);
    let ctx_body = VarMap.concat(ctx, ctx_pat);
    let (ty_body, free_body, m_body, constraints2) =
      uexp_to_info_map(~ctx=ctx_body, ~mode=mode_body, body);
    add(
      ~self=Just(Arrow(ty_pat, ty_body)),
      ~free=Ctx.subtract_typ(ctx_pat, free_body),
      union_m([m_pat, m_body]),
      match_constraints @ constraints1 @ constraints2,
    );
  | Let(pat, def, body) =>
    let (ty_pat, ctx_pat, _m_pat, constraints1) =
      upat_to_info_map(~is_synswitch=true, ~mode=Syn, pat);
    let def_ctx = extend_let_def_ctx(ctx, pat, ctx_pat, def);
    let (ty_def, free_def, m_def, constraints2) =
      uexp_to_info_map(~ctx=def_ctx, ~mode=Ana(ty_pat), def);
    /* Analyze pattern to incorporate def type into ctx */
    let (_, ctx_pat_ana, m_pat, constraints3) =
      upat_to_info_map(~is_synswitch=false, ~mode=Ana(ty_def), pat);
    let ctx_body = VarMap.concat(ctx, ctx_pat_ana);
    let (ty_body, free_body, m_body, constraints4) =
      uexp_to_info_map(~ctx=ctx_body, ~mode, body);
    add(
      ~self=Just(ty_body),
      ~free=Ctx.union([free_def, Ctx.subtract_typ(ctx_pat_ana, free_body)]),
      union_m([m_pat, m_def, m_body]),
      constraints1 @ constraints2 @ constraints3 @ constraints4,
    );
  | Match(scrut, rules) =>
    let (ty_scrut, free_scrut, m_scrut, constraints1) = go(~mode=Syn, scrut);
    let (pats, branches) = List.split(rules);
    let pat_infos =
      List.map(
        upat_to_info_map(~is_synswitch=false, ~mode=Typ.Ana(ty_scrut)),
        pats,
      );
    let branch_infos =
      List.map2(
        (branch, (_, ctx_pat, _, _)) =>
          uexp_to_info_map(~ctx=VarMap.concat(ctx, ctx_pat), ~mode, branch),
        branches,
        pat_infos,
      );
    let branch_sources =
      List.map2(
        (e: Term.UExp.t, (ty, _, _, _)) =>
          Typ.{id: Term.UExp.rep_id(e), ty},
        branches,
        branch_infos,
      );
    let pat_ms = List.map(((_, _, m, _)) => m, pat_infos);
    let pat_constraints =
      List.map(((_, _, _, c)) => c, pat_infos) |> List.flatten;
    let branch_ms = List.map(((_, _, m, _)) => m, branch_infos);
    let branch_frees = List.map(((_, free, _, _)) => free, branch_infos);
    let branch_constraints =
      List.map(((_, _, _, c)) => c, branch_infos) |> List.flatten;
    let self = Typ.Joined(Fun.id, branch_sources);
    let free = Ctx.union([free_scrut] @ branch_frees);
    add(
      ~self,
      ~free,
      union_m([m_scrut] @ pat_ms @ branch_ms),
      constraints1 @ pat_constraints @ branch_constraints,
    );
=======
    let (e1, m) = go(~mode=Syn, e1, m);
    let (e2, m) = go(~mode, e2, m);
    add(~self=Just(e2.ty), ~co_ctx=CoCtx.union([e1.co_ctx, e2.co_ctx]), m);
  | Constructor(ctr) => atomic(Self.of_ctr(ctx, ctr))
  | Ap(fn, arg) =>
    let fn_mode = Mode.of_ap(ctx, mode, UExp.ctr_name(fn));
    let (fn, m) = go(~mode=fn_mode, fn, m);
    let (ty_in, ty_out) = Typ.matched_arrow(fn.ty);
    let (arg, m) = go(~mode=Ana(ty_in), arg, m);
    let self: Self.t =
      Id.is_nullary_ap_flag(arg.term.ids)
      && !Typ.is_consistent(ctx, ty_in, Prod([]))
        ? BadTrivAp(ty_in) : Just(ty_out);
    add(~self, ~co_ctx=CoCtx.union([fn.co_ctx, arg.co_ctx]), m);
  | Fun(p, e) =>
    let (mode_pat, mode_body) = Mode.of_arrow(ctx, mode);
    let (p', _) =
      go_pat(~is_synswitch=false, ~co_ctx=CoCtx.empty, ~mode=mode_pat, p, m);
    let (e, m) = go'(~ctx=p'.ctx, ~mode=mode_body, e, m);
    /* add co_ctx to pattern */
    let (p, m) =
      go_pat(~is_synswitch=false, ~co_ctx=e.co_ctx, ~mode=mode_pat, p, m);
    add(
      ~self=Just(Arrow(p.ty, e.ty)),
      ~co_ctx=CoCtx.mk(ctx, p.ctx, e.co_ctx),
      m,
    );
  | Let(p, def, body) =>
    let (p_syn, _) =
      go_pat(~is_synswitch=true, ~co_ctx=CoCtx.empty, ~mode=Syn, p, m);
    let def_ctx = extend_let_def_ctx(ctx, p, p_syn.ctx, def);
    let (def, m) = go'(~ctx=def_ctx, ~mode=Ana(p_syn.ty), def, m);
    /* Analyze pattern to incorporate def type into ctx */
    let (p_ana', _) =
      go_pat(
        ~is_synswitch=false,
        ~co_ctx=CoCtx.empty,
        ~mode=Ana(def.ty),
        p,
        m,
      );
    let (body, m) = go'(~ctx=p_ana'.ctx, ~mode, body, m);
    /* add co_ctx to pattern */
    let (p_ana, m) =
      go_pat(
        ~is_synswitch=false,
        ~co_ctx=body.co_ctx,
        ~mode=Ana(def.ty),
        p,
        m,
      );
    add(
      ~self=Just(body.ty),
      ~co_ctx=
        CoCtx.union([def.co_ctx, CoCtx.mk(ctx, p_ana.ctx, body.co_ctx)]),
      m,
    );
  | If(e0, e1, e2) =>
    let branch_ids = List.map(UExp.rep_id, [e1, e2]);
    let (cond, m) = go(~mode=Ana(Bool), e0, m);
    let (cons, m) = go(~mode, e1, m);
    let (alt, m) = go(~mode, e2, m);
    add(
      ~self=Self.match(ctx, [cons.ty, alt.ty], branch_ids),
      ~co_ctx=CoCtx.union([cond.co_ctx, cons.co_ctx, alt.co_ctx]),
      m,
    );
  | Match(scrut, rules) =>
    let (scrut, m) = go(~mode=Syn, scrut, m);
    let (ps, es) = List.split(rules);
    let branch_ids = List.map(UExp.rep_id, es);
    let (ps', _) =
      map_m(
        go_pat(
          ~is_synswitch=false,
          ~co_ctx=CoCtx.empty,
          ~mode=Mode.Ana(scrut.ty),
        ),
        ps,
        m,
      );
    let p_ctxs = List.map(Info.pat_ctx, ps');
    let (es, m) =
      List.fold_left2(
        ((es, m), e, ctx) =>
          go'(~ctx, ~mode, e, m) |> (((e, m)) => (es @ [e], m)),
        ([], m),
        es,
        p_ctxs,
      );
    let e_tys = List.map(Info.exp_ty, es);
    let e_co_ctxs =
      List.map2(CoCtx.mk(ctx), p_ctxs, List.map(Info.exp_co_ctx, es));
    /* Add co-ctxs to patterns */
    let (_, m) =
      map_m(
        ((p, co_ctx)) =>
          go_pat(~is_synswitch=false, ~co_ctx, ~mode=Mode.Ana(scrut.ty), p),
        List.combine(ps, e_co_ctxs),
        m,
      );
    add(
      ~self=Self.match(ctx, e_tys, branch_ids),
      ~co_ctx=CoCtx.union([scrut.co_ctx] @ e_co_ctxs),
      m,
    );
  | TyAlias(typat, utyp, body) =>
    let m = utpat_to_info_map(~ctx, ~ancestors, typat, m) |> snd;
    switch (typat.term) {
    | Var(name) when !Ctx.shadows_typ(ctx, name) =>
      /* Currently we disallow all type shadowing */
      /* NOTE(andrew): Currently, UTyp.to_typ returns Unknown(TypeHole)
         for any type variable reference not in its ctx. So any free variables
         in the definition won't be noticed. But we need to check for free
         variables to decide whether to make a recursive type or not. So we
         tentatively add an abtract type to the ctx, representing the
         speculative rec parameter. */
      let (ty_def, ctx_def, ctx_body) = {
        let ty_pre = UTyp.to_typ(Ctx.extend_dummy_tvar(ctx, name), utyp);
        switch (utyp.term) {
        | Sum(_) when List.mem(name, Typ.free_vars(ty_pre)) =>
          let ty_rec = Typ.Rec("α", Typ.subst(Var("α"), name, ty_pre));
          let ctx_def =
            Ctx.extend_alias(ctx, name, UTPat.rep_id(typat), ty_rec);
          (ty_rec, ctx_def, ctx_def);
        | _ =>
          let ty = UTyp.to_typ(ctx, utyp);
          (ty, ctx, Ctx.extend_alias(ctx, name, UTPat.rep_id(typat), ty));
        };
      };
      let ctx_body =
        switch (Typ.get_sum_constructors(ctx, ty_def)) {
        | Some(sm) => Ctx.add_ctrs(ctx_body, name, UTyp.rep_id(utyp), sm)
        | None => ctx_body
        };
      let ({co_ctx, ty: ty_body, _}: Info.exp, m) =
        go'(~ctx=ctx_body, ~mode, body, m);
      /* Make sure types don't escape their scope */
      let ty_escape = Typ.subst(ty_def, name, ty_body);
      let m = utyp_to_info_map(~ctx=ctx_def, ~ancestors, utyp, m) |> snd;
      add(~self=Just(ty_escape), ~co_ctx, m);
    | Var(_)
    | Invalid(_)
    | EmptyHole
    | MultiHole(_) =>
      let ({co_ctx, ty: ty_body, _}: Info.exp, m) =
        go'(~ctx, ~mode, body, m);
      let m = utyp_to_info_map(~ctx, ~ancestors, utyp, m) |> snd;
      add(~self=Just(ty_body), ~co_ctx, m);
    };
>>>>>>> llmass
  };
}
and upat_to_info_map =
    (
      ~is_synswitch,
      ~ctx,
      ~co_ctx,
      ~ancestors: Info.ancestors,
      ~mode: Mode.t=Mode.Syn,
      {ids, term} as upat: UPat.t,
      m: Map.t,
    )
<<<<<<< HEAD
    : (Typ.t, Ctx.t, map, Typ.constraints) => {
  let upat_to_info_map = upat_to_info_map(~is_synswitch);
  let unknown =
    Typ.Unknown(
      is_synswitch
        ? SynSwitch(Term.UPat.rep_id(upat))
        : AstNode(Term.UPat.rep_id(upat)),
    );
  let cls = Term.UPat.cls_of_term(term);
  let add = (~self: Typ.self, ~ctx, m, constraints) => {
    let joined_constraints =
      switch (self) {
      | Joined(wrap, sources) =>
        sources |> Typ.source_tys |> List.map(wrap) |> join_constraints
      | _ => []
      };
    (
      typ_after_fix(mode, self, Term.UPat.rep_id(upat)),
      ctx,
      add_info(ids, InfoPat({cls, self, mode, ctx, term: upat}), m),
      joined_constraints @ constraints,
    );
  };
  let atomic = self =>
    add(
      ~self,
      ~ctx,
      Id.Map.empty,
      subsumption_constraints(
        mode,
        typ_after_fix(mode, self, Term.UPat.rep_id(upat)),
      ),
    );
  switch (term) {
  | Invalid(msg) =>
    let final_typ: Typ.t = Unknown(AstNode(Term.UPat.rep_id(upat)));
    (
      final_typ,
      ctx,
      add_info(ids, Invalid(msg), Id.Map.empty),
      subsumption_constraints(mode, final_typ),
    );
  | MultiHole(tms) =>
    let info = tms |> List.map(any_to_info_map(~ctx));
    let maps = List.map(((_, m, _)) => m, info);
    let constraints = List.map(((_, _, c)) => c, info) |> List.flatten;
    let constraints =
      subsumption_constraints(
        mode,
        typ_after_fix(mode, Multi, Term.UPat.rep_id(upat)),
      )
      @ constraints;
    add(~self=Multi, ~ctx, union_m(maps), constraints);
=======
    : (Info.pat, Map.t) => {
  let add = (~self, ~ctx, m) => {
    let info =
      Info.derived_pat(
        ~upat,
        ~ctx,
        ~co_ctx,
        ~mode,
        ~ancestors,
        ~self=Common(self),
      );
    (info, add_info(ids, InfoPat(info), m));
  };
  let atomic = self => add(~self, ~ctx, m);
  let ancestors = [UPat.rep_id(upat)] @ ancestors;
  let go = upat_to_info_map(~is_synswitch, ~ancestors, ~co_ctx);
  let unknown = Typ.Unknown(is_synswitch ? SynSwitch : Internal);
  let ctx_fold = (ctx: Ctx.t, m) =>
    List.fold_left2(
      ((ctx, tys, m), e, mode) =>
        go(~ctx, ~mode, e, m)
        |> (((info, m)) => (info.ctx, tys @ [info.ty], m)),
      (ctx, [], m),
    );
  switch (term) {
  | MultiHole(tms) =>
    let (_, m) = multi(~ctx, ~ancestors, m, tms);
    add(~self=IsMulti, ~ctx, m);
  | Invalid(token) => atomic(BadToken(token))
>>>>>>> llmass
  | EmptyHole => atomic(Just(unknown))
  | Int(_) => atomic(Just(Int))
  | Float(_) => atomic(Just(Float))
  | Triv => atomic(Just(Prod([])))
  | Bool(_) => atomic(Just(Bool))
  | String(_) => atomic(Just(String))
<<<<<<< HEAD
  | ListLit([]) => atomic(Just(List(Unknown(NoProvenance))))
  | ListLit(ps) =>
    let (modes, list_of_match_constraints) =
      List.init(List.length(ps), _ =>
        Typ.matched_list_mode(mode, Term.UPat.rep_id(upat))
      )
      |> List.split;
    let match_constraints = List.flatten(list_of_match_constraints);
    let p_ids = List.map(Term.UPat.rep_id, ps);
    let (ctx, infos) =
      List.fold_left2(
        ((ctx, infos), e, mode) => {
          let (_, ctx, _, _) as info = upat_to_info_map(~ctx, ~mode, e);
          (ctx, infos @ [info]);
        },
        (ctx, []),
        ps,
        modes,
      );
    let tys = List.map(((ty, _, _, _)) => ty, infos);
    let ps_constraints =
      List.map(((_, _, _, c)) => c, infos) |> List.flatten;
    let self: Typ.self =
      switch (Typ.join_all(tys)) {
      | None =>
        Joined(
          ty => List(ty),
          List.map2((id, ty) => Typ.{id, ty}, p_ids, tys),
        )
      | Some(ty) => Just(List(ty))
      };
    let info: t = InfoPat({cls, self, mode, ctx, term: upat});
    let m = union_m(List.map(((_, _, m, _)) => m, infos));
    /* Add an entry for the id of each comma tile */
    let m = List.fold_left((m, id) => Id.Map.add(id, info, m), m, ids);
    (
      typ_after_fix(mode, self, Term.UPat.rep_id(upat)),
      ctx,
      m,
      match_constraints @ ps_constraints,
    );
  | Cons(hd, tl) =>
    let (mode_e, match_constraints) =
      Typ.matched_list_mode(mode, Term.UPat.rep_id(upat));
    let (ty1, ctx, m_hd, constraints1) =
      upat_to_info_map(~ctx, ~mode=mode_e, hd);
    let (_, ctx, m_tl, constraints2) =
      upat_to_info_map(~ctx, ~mode=Ana(List(ty1)), tl);
    add(
      ~self=Just(List(ty1)),
      ~ctx,
      union_m([m_hd, m_tl]),
      match_constraints @ constraints1 @ constraints2,
    );
  | Tag(name) =>
    switch (BuiltinADTs.get_tag_typ(name)) {
    | None => atomic(Free(Tag))
    | Some(typ) => atomic(Just(typ))
    }
  | Wild => atomic(Just(Unknown(NoProvenance)))
  | Var(name) =>
    let upat_rep_id = Term.UPat.rep_id(upat);
    let typ =
      typ_after_fix(
        mode,
        Just(Unknown(AstNode(upat_rep_id))),
        upat_rep_id,
      );
    let entry = Ctx.VarEntry({name, id: upat_rep_id, typ});
    add(
      ~self=Just(unknown),
      ~ctx=Ctx.extend(entry, ctx),
      Id.Map.empty,
      subsumption_constraints(mode, typ),
    );
  | Tuple(ps) =>
    let (modes, match_constraints) =
      Typ.matched_prod_mode(mode, List.length(ps));
    let (ctx, infos) =
      List.fold_left2(
        ((ctx, infos), e, mode) => {
          let (_, ctx, _, _) as info = upat_to_info_map(~mode, ~ctx, e);
          (ctx, infos @ [info]);
        },
        (ctx, []),
        ps,
        modes,
      );
    let self = Typ.Just(Prod(List.map(((ty, _, _, _)) => ty, infos)));
    let m = union_m(List.map(((_, _, m, _)) => m, infos));
    let ps_constraints =
      List.map(((_, _, _, c)) => c, infos) |> List.flatten;
    add(~self, ~ctx, m, match_constraints @ ps_constraints);
  | Parens(p) =>
    let (ty, ctx, m, constraints) = upat_to_info_map(~ctx, ~mode, p);
    add(~self=Just(ty), ~ctx, m, constraints);
  | Ap(fn, arg) =>
    /* Contructor application */
    /* Function position mode Ana(Hole->Hole) instead of Syn */
    let (ty_fn, ctx, m_fn, constraints1) =
      upat_to_info_map(~ctx, ~mode=Typ.ap_mode, fn);
    let ((ty_in, ty_out), match_constraints) =
      Typ.matched_arrow(ty_fn, Term.UPat.rep_id(upat));
    let (_, ctx, m_arg, constraints2) =
      upat_to_info_map(~ctx, ~mode=Ana(ty_in), arg);
    add(
      ~self=Just(ty_out),
      ~ctx,
      union_m([m_fn, m_arg]),
      match_constraints
      @ constraints1
      @ constraints2
      @ subsumption_constraints(mode, ty_out),
    );
  | TypeAnn(p, ty) =>
    let (ty_ann, m_typ) = utyp_to_info_map(ty);
    let (_ty, ctx, m, constraints) =
      upat_to_info_map(~ctx, ~mode=Ana(ty_ann), p);
    add(~self=Just(ty_ann), ~ctx, union_m([m, m_typ]), constraints);
  };
}
and utyp_to_info_map = ({ids, term} as utyp: Term.UTyp.t): (Typ.t, map) => {
  let cls = Term.UTyp.cls_of_term(term);
  let ty = Term.utyp_to_ty(utyp);
  let add = self => add_info(ids, InfoTyp({cls, self, term: utyp}));
  let just = m => (ty, add(Just(ty), m));
  switch (term) {
  | Invalid(msg) => (
      Unknown(AstNode(Term.UTyp.rep_id(utyp))),
      add_info(ids, Invalid(msg), Id.Map.empty),
=======
  | ListLit(ps) =>
    let ids = List.map(UPat.rep_id, ps);
    let modes = Mode.of_list_lit(ctx, List.length(ps), mode);
    let (ctx, tys, m) = ctx_fold(ctx, m, ps, modes);
    add(~self=Self.listlit(~empty=unknown, ctx, tys, ids), ~ctx, m);
  | Cons(hd, tl) =>
    let (hd, m) = go(~ctx, ~mode=Mode.of_cons_hd(ctx, mode), hd, m);
    let (tl, m) =
      go(~ctx=hd.ctx, ~mode=Mode.of_cons_tl(ctx, mode, hd.ty), tl, m);
    add(~self=Just(List(hd.ty)), ~ctx=tl.ctx, m);
  | Wild => atomic(Just(unknown))
  | Var(name) =>
    /* NOTE: The self type assigned to pattern variables (Unknown)
       may be SynSwitch, but SynSwitch is never added to the context;
       Unknown(Internal) is used in this case */
    let ctx_typ =
      Info.fixed_typ_pat(ctx, mode, Common(Just(Unknown(Internal))));
    let entry = Ctx.VarEntry({name, id: UPat.rep_id(upat), typ: ctx_typ});
    add(~self=Just(unknown), ~ctx=Ctx.extend(ctx, entry), m);
  | Tuple(ps) =>
    let modes = Mode.of_prod(ctx, mode, List.length(ps));
    let (ctx, tys, m) = ctx_fold(ctx, m, ps, modes);
    add(~self=Just(Prod(tys)), ~ctx, m);
  | Parens(p) =>
    let (p, m) = go(~ctx, ~mode, p, m);
    add(~self=Just(p.ty), ~ctx=p.ctx, m);
  | Constructor(ctr) => atomic(Self.of_ctr(ctx, ctr))
  | Ap(fn, arg) =>
    let fn_mode = Mode.of_ap(ctx, mode, UPat.ctr_name(fn));
    let (fn, m) = go(~ctx, ~mode=fn_mode, fn, m);
    let (ty_in, ty_out) = Typ.matched_arrow(fn.ty);
    let (arg, m) = go(~ctx, ~mode=Ana(ty_in), arg, m);
    add(~self=Just(ty_out), ~ctx=arg.ctx, m);
  | TypeAnn(p, ann) =>
    let (ann, m) = utyp_to_info_map(~ctx, ~ancestors, ann, m);
    let (p, m) = go(~ctx, ~mode=Ana(ann.ty), p, m);
    add(~self=Just(ann.ty), ~ctx=p.ctx, m);
  };
}
and utyp_to_info_map =
    (
      ~ctx,
      ~expects=Info.TypeExpected,
      ~ancestors,
      {ids, term} as utyp: UTyp.t,
      m: Map.t,
>>>>>>> llmass
    )
    : (Info.typ, Map.t) => {
  let add = m => {
    let info = Info.derived_typ(~utyp, ~ctx, ~ancestors, ~expects);
    (info, add_info(ids, InfoTyp(info), m));
  };
  let ancestors = [UTyp.rep_id(utyp)] @ ancestors;
  let go' = utyp_to_info_map(~ctx, ~ancestors);
  let go = go'(~expects=TypeExpected);
  //TODO(andrew): make this return free, replacing Typ.free_vars
  switch (term) {
  | MultiHole(tms) =>
    let (_, m) = multi(~ctx, ~ancestors, m, tms);
    add(m);
  | Invalid(_)
  | EmptyHole
  | Int
  | Float
  | Bool
  | String => add(m)
  | Var(_)
  | Constructor(_) =>
    /* Names are resolved in Info.status_typ */
    add(m)
  | List(t)
  | Parens(t) => add(go(t, m) |> snd)
  | Arrow(t1, t2) =>
    let m = go(t1, m) |> snd;
    let m = go(t2, m) |> snd;
    add(m);
  | Tuple(ts) =>
<<<<<<< HEAD
    let m = ts |> List.map(utyp_to_info_map) |> List.map(snd) |> union_m;
    just(m);
  | Var(name) =>
    switch (BuiltinADTs.is_typ_var(name)) {
    | None => (
        Unknown(AstNode(Term.UTyp.rep_id(utyp))),
        add(Free(TypeVariable), Id.Map.empty),
      )
    | Some(_) => (Var(name), add(Just(Var(name)), Id.Map.empty))
    }
  | MultiHole(tms) =>
    // TODO thread ctx through to multihole terms once ctx is available
    let info = tms |> List.map(any_to_info_map(~ctx=Ctx.empty));
    let maps = List.map(((_, m, _)) => m, info);
    just(union_m(maps));
  };
};

let mk_map_and_inference_solutions =
  Core.Memo.general(
    ~cache_size_bound=1000,
    e => {
      let (_, _, info_map, constraints) =
        uexp_to_info_map(~ctx=Builtins.ctx(Builtins.Pervasives.builtins), e);

      // rewrite is here
      let ctx = Infer.Ctx.create();
      let _ =
        List.iter(
          c => {
            let (typ1, typ2) = c;
            Infer.constrain(ctx, typ1, typ2);
          },
          constraints,
        );

      (info_map, ctx);
    },
  );

let mk_map = e => {
  let (info_map, _) = mk_map_and_inference_solutions(e);
  info_map;
};

let get_binding_site = (id: Id.t, statics_map: map): option(Id.t) => {
  open OptUtil.Syntax;
  let* opt = Id.Map.find_opt(id, statics_map);
  let* info_exp =
    switch (opt) {
    | InfoExp(info_exp) => Some(info_exp)
    | _ => None
    };

  let+ entry =
    switch (info_exp.term.term) {
    | TermBase.UExp.Var(name) => Ctx.lookup_var(info_exp.ctx, name)
    | _ => None
    };
  entry.id;
};
=======
    let m = map_m(go, ts, m) |> snd;
    add(m);
  | Ap(t1, t2) =>
    let ty_in = UTyp.to_typ(ctx, t2);
    let t1_mode: Info.typ_expects =
      switch (expects) {
      | VariantExpected(m, sum_ty) =>
        ConstructorExpected(m, Arrow(ty_in, sum_ty))
      | _ => ConstructorExpected(Unique, Arrow(ty_in, Unknown(Internal)))
      };
    let m = go'(~expects=t1_mode, t1, m) |> snd;
    let m = go'(~expects=TypeExpected, t2, m) |> snd;
    add(m);
  | Sum(variants) =>
    let ty_sum = UTyp.to_typ(ctx, utyp);
    let (m, _) =
      List.fold_left(
        variant_to_info_map(~ctx, ~ancestors, ~ty_sum),
        (m, []),
        variants,
      );
    add(m);
  };
}
and utpat_to_info_map =
    (~ctx, ~ancestors, {ids, term} as utpat: UTPat.t, m: Map.t)
    : (Info.tpat, Map.t) => {
  let add = m => {
    let info = Info.derived_tpat(~utpat, ~ctx, ~ancestors);
    (info, add_info(ids, InfoTPat(info), m));
  };
  let ancestors = [UTPat.rep_id(utpat)] @ ancestors;
  switch (term) {
  | MultiHole(tms) =>
    let (_, m) = multi(~ctx, ~ancestors, m, tms);
    add(m);
  | Invalid(_)
  | EmptyHole
  | Var(_) => add(m)
  };
}
and variant_to_info_map =
    (~ctx, ~ancestors, ~ty_sum, (m, ctrs), uty: UTyp.variant) => {
  let go = expects => utyp_to_info_map(~ctx, ~ancestors, ~expects);
  switch (uty) {
  | BadEntry(uty) =>
    let m = go(VariantExpected(Unique, ty_sum), uty, m) |> snd;
    (m, ctrs);
  | Variant(ctr, ids, param) =>
    let m =
      go(
        ConstructorExpected(
          List.mem(ctr, ctrs) ? Duplicate : Unique,
          ty_sum,
        ),
        {term: Constructor(ctr), ids},
        m,
      )
      |> snd;
    let m =
      switch (param) {
      | Some(param_ty) => go(TypeExpected, param_ty, m) |> snd
      | None => m
      };
    (m, [ctr, ...ctrs]);
  };
};

let collect_errors = (map: Map.t): list((Id.t, Info.error)) =>
  Id.Map.fold(
    (id, info: Info.t, acc) =>
      Option.to_list(Info.error_of(info) |> Option.map(x => (id, x))) @ acc,
    map,
    [],
  );
>>>>>>> llmass
