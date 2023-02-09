open Sexplib.Std;
open Util;

/* STATICS

     This module determines the statics semantics of the language.
     It takes a term and returns a map which associates the unique
     ids of each term to an 'info' data structure which reflects that
     term's statics. The statics collected depend on the term's sort,
     but every term has a syntactic class (The cls types from Term),
     except Invalid terms which Term could not parse.

     The map generated by this module is intended to be generated once
     from a given term and then reused anywhere there is logic which
     depends on static information.
   */

/* Expressions are assigned a mode (reflecting the static expectations
   if any of their syntactic parent), a self (reflecting what their
   statics would be in isolation), a context (variables in scope), and
   free (variables occuring free in the expression. */
[@deriving (show({with_path: false}), sexp, yojson)]
type info_exp = {
  cls: Term.UExp.cls,
  term: Term.UExp.t,
  mode: Typ.mode,
  self: Typ.self,
  ctx: Ctx.t,
  free: Ctx.co,
  // TODO: add derived attributes like error_status and typ_after_fix?
};

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
    switch (Typ.join(Arrow(Unknown(Anonymous), Unknown(Anonymous)), ty)) {
    | None => InHole(NoFun(ty))
    | Some(_) => NotInHole(SynConsistent(ty))
    }
  | (SynFun, Joined(_wrap, tys_syn)) =>
    let tys_syn = Typ.source_tys(tys_syn);
    switch (Typ.join_all(tys_syn)) {
    | None => InHole(SynInconsistentBranches(tys_syn))
    | Some(ty_joined) =>
      switch (
        Typ.join(Arrow(Unknown(Anonymous), Unknown(Anonymous)), ty_joined)
      ) {
      | None => InHole(NoFun(ty_joined))
      | Some(_) => NotInHole(SynConsistent(ty_joined))
      }
    };
  | (Syn | SynFun | Ana(_), Free(free_error)) => InHole(Free(free_error))
  | (Syn | SynFun | Ana(_), Multi) =>
    NotInHole(SynConsistent(Unknown(Anonymous)))
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
  | InHole(_) => Unknown(Internal(termId))
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
  List.fold_left(
    (m1, m2) => Id.Map.union((_, _, b) => Some(b), m1, m2),
    Id.Map.empty,
  );

let add_info = (ids, info: 'a, m: Ptmap.t('a)) =>
  ids
  |> List.map(id => Id.Map.singleton(id, info))
  |> List.fold_left(Id.Map.disj_union, m);

let extend_let_def_ctx =
    (ctx: Ctx.t, pat: Term.UPat.t, pat_ctx: Ctx.t, def: Term.UExp.t) =>
  if (Term.UPat.is_tuple_of_arrows(pat)
      && Term.UExp.is_tuple_of_functions(def)) {
    VarMap.concat(ctx, pat_ctx);
  } else {
    ctx;
  };

let typ_exp_binop_bin_int: Term.UExp.op_bin_int => Typ.t =
  fun
  | (Plus | Minus | Times | Power | Divide) as _op => Int
  | (LessThan | GreaterThan | LessThanOrEqual | GreaterThanOrEqual | Equals) as _op =>
    Bool;

let typ_exp_binop_bin_float: Term.UExp.op_bin_float => Typ.t =
  fun
  | (Plus | Minus | Times | Power | Divide) as _op => Float
  | (LessThan | GreaterThan | LessThanOrEqual | GreaterThanOrEqual | Equals) as _op =>
    Bool;

let typ_exp_binop_bin_string: Term.UExp.op_bin_string => Typ.t =
  fun
  | Equals as _op => Bool;

let typ_exp_binop: Term.UExp.op_bin => (Typ.t, Typ.t, Typ.t) =
  fun
  | Bool(And | Or) => (Bool, Bool, Bool)
  | Int(op) => (Int, Int, typ_exp_binop_bin_int(op))
  | Float(op) => (Float, Float, typ_exp_binop_bin_float(op))
  | String(op) => (String, String, typ_exp_binop_bin_string(op));

let typ_exp_unop: Term.UExp.op_un => (Typ.t, Typ.t) =
  fun
  | Int(Minus) => (Int, Int);

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
  }
and uexp_to_info_map =
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
    let final_typ: Typ.t = Unknown(Internal(Term.UExp.rep_id(uexp)));
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
  | EmptyHole => atomic(Just(Unknown(Internal(Term.UExp.rep_id(uexp)))))
  | Triv => atomic(Just(Prod([])))
  | Bool(_) => atomic(Just(Bool))
  | Int(_) => atomic(Just(Int))
  | Float(_) => atomic(Just(Float))
  | String(_) => atomic(Just(String))
  | ListLit([]) => atomic(Just(List(Unknown(Anonymous))))
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
    );
  | Seq(e1, e2) =>
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
  };
}
and upat_to_info_map =
    (
      ~is_synswitch,
      ~ctx=Ctx.empty,
      ~mode: Typ.mode=Typ.Syn,
      {ids, term} as upat: Term.UPat.t,
    )
    : (Typ.t, Ctx.t, map, Typ.constraints) => {
  let upat_to_info_map = upat_to_info_map(~is_synswitch);
  let unknown =
    Typ.Unknown(
      is_synswitch
        ? SynSwitch(Term.UPat.rep_id(upat))
        : Internal(Term.UPat.rep_id(upat)),
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
    let final_typ: Typ.t = Unknown(Internal(Term.UPat.rep_id(upat)));
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
  | EmptyHole => atomic(Just(unknown))
  | Int(_) => atomic(Just(Int))
  | Float(_) => atomic(Just(Float))
  | Triv => atomic(Just(Prod([])))
  | Bool(_) => atomic(Just(Bool))
  | String(_) => atomic(Just(String))
  | ListLit([]) => atomic(Just(List(Unknown(Anonymous))))
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
  | Wild => atomic(Just(Unknown(Anonymous)))
  | Var(name) =>
    let upat_rep_id = Term.UPat.rep_id(upat);
    let typ =
      typ_after_fix(
        mode,
        Just(Unknown(Internal(upat_rep_id))),
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
      Unknown(Internal(Term.UTyp.rep_id(utyp))),
      add_info(ids, Invalid(msg), Id.Map.empty),
    )
  | EmptyHole
  | Int
  | Float
  | Bool
  | String => just(Id.Map.empty)
  | List(t)
  | Parens(t) =>
    let (_, m) = utyp_to_info_map(t);
    just(m);
  | Arrow(t1, t2) =>
    let (_, m_t1) = utyp_to_info_map(t1);
    let (_, m_t2) = utyp_to_info_map(t2);
    just(union_m([m_t1, m_t2]));
  | Tuple(ts) =>
    let m = ts |> List.map(utyp_to_info_map) |> List.map(snd) |> union_m;
    just(m);
  | Var(name) =>
    switch (BuiltinADTs.is_typ_var(name)) {
    | None => (
        Unknown(Internal(Term.UTyp.rep_id(utyp))),
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

// Needs to populate editor.state sometimes...
// Specifically, if we care about annotations
// I think its only necessary from:
//  Perform.go_z (called by someone (Perform.go) with an editor)
//  ScratchMode.view (has editor)
//  Cell.get_elab (has editor)
//  ScratchSlide.spliced_elabs (has editor)
//
// Others from LangDoc, EditorUtil, SchoolMode, SchoolExercises
// omitted due to lack of necessity (want only info_map, or color_map, only for validation, etc)
let mk_map_and_annotations =
  Core.Memo.general(
    ~cache_size_bound=1000,
    e => {
      let (_, _, info_map, constraints) =
        uexp_to_info_map(~ctx=Builtins.ctx(Builtins.Pervasives.builtins), e);

      let inference_results = Inference.unify_and_report_status(constraints);
      let global_inference_solutions =
        InferenceResult.get_desired_solutions(inference_results);

      // InferenceResult.add_on_new_annotations(annotation_map);

      (info_map, global_inference_solutions);
    },
  );
let mk_map = e => {
  let (info_map, _) = mk_map_and_annotations(e);
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
