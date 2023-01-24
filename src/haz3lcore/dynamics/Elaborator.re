open Util;

module ElaboratorError = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t =
    | Closure
    | Hole
    | ElaboratedList
    | FixF
    | ElaboratedFun
    | Prj
    | Inj
    | ElaboratedTest
    | Cast
    | InvalidInfoMap;

  [@deriving (show({with_path: false}), sexp, yojson)]
  exception Exception(t);
};

module ElaboratorResult = {
  [@deriving sexp]
  type t =
    | Elaborates(DHExp.t, Typ.t, Delta.t)
    | DoesNotElaborate;
};

open ElaboratorMonad;
open ElaboratorMonad.Syntax;

let int_op_of: Term.UExp.op_bin_int => DHExp.BinIntOp.t =
  fun
  | Plus => Plus
  | Minus => Minus
  | Times => Times
  | Power => Power
  | Divide => Divide
  | LessThan => LessThan
  | LessThanOrEqual => LessThanOrEqual
  | GreaterThan => GreaterThan
  | GreaterThanOrEqual => GreaterThanOrEqual
  | Equals => Equals;

let float_op_of: Term.UExp.op_bin_float => DHExp.BinFloatOp.t =
  fun
  | Plus => Plus
  | Minus => Minus
  | Times => Times
  | Power => Power
  | Divide => Divide
  | LessThan => LessThan
  | LessThanOrEqual => LessThanOrEqual
  | GreaterThan => GreaterThan
  | GreaterThanOrEqual => GreaterThanOrEqual
  | Equals => Equals;

let string_op_of: Term.UExp.op_bin_string => DHExp.BinStringOp.t =
  fun
  | Equals => Equals;

let bool_op_of: Term.UExp.op_bin_bool => DHExp.BinBoolOp.t =
  fun
  | And => And
  | Or => Or;

let exp_binop_of = (op: Term.UExp.op_bin): (Typ.t, (_, _) => DHExp.term) =>
  switch (op) {
  | Int(_) => (Int, ((e1, e2) => BinOp(op, e1, e2)))
  | Float(_) => (Float, ((e1, e2) => BinOp(op, e1, e2)))
  | Bool(_) => (Bool, ((e1, e2) => BinOp(op, e1, e2)))
  | String(_) => (String, ((e1, e2) => BinOp(op, e1, e2)))
  };

let ids_derive = (ids: CH.Ids.t): IdGen.t(CH.Ids.t) => {
  let f = ((base, _)) => {
    open IdGen.Syntax;
    let+ derived = IdGen.fresh;
    (base, derived);
  };
  ListUtil.traverse(f, ids);
};

type m('a) = ElaboratorMonad.t('a);

let rec dhexp_of_uexp = (m: Statics.map, uexp: Term.UExp.t): m(DHExp.t) => {
  /* NOTE: Left out delta for now */
  switch (Id.Map.find_opt(Term.UExp.rep_id(uexp), m)) {
  | Some(InfoExp({mode, self, _})) =>
    let err_status = Statics.error_status(mode, self);
    let maybe_reason: option(ErrStatus.HoleReason.t) =
      switch (err_status) {
      | NotInHole(_) => None
      | InHole(_) => Some(TypeInconsistent)
      };
    let u = Term.UExp.rep_id(uexp).id; /* NOTE: using term uids for hole ids */
    let wrap = (d: DHExp.t): m(DHExp.t) =>
      switch (maybe_reason) {
      | None => d |> return
      | Some(reason) =>
        // We don't assign derived ids here since they should be done
        // outside of wrap.
        DHExp.{
          ids: d.ids,
          term: Hole(Some((u, 0)), NonEmptyHole(reason, d)),
        }
        |> return
      };
    let ids = uexp.ids;
    let ids_derive = ids => with_id(ids_derive(ids));
    switch (uexp.term) {
    | Hole(_, Invalid(_)) /* NOTE: treating invalid as a hole for now */
    | Hole(_, EmptyHole) =>
      let* ids = ids_derive(ids);
      DHExp.{ids, term: Hole(Some((u, 0)), EmptyHole)} |> return;
    | Hole(_, MultiHole(tms)) =>
      // TODO: dhexp, eval for multiholes
      let f = t =>
        switch (t) {
        | Term.Exp(e) => dhexp_of_uexp(m, e)
        | _ =>
          let* ids = ids_derive(CH.Ids.mk(Term.ids(t)));
          let rep_id = Term.rep_id(t).id;
          DHExp.mk(ids, Hole(Some((rep_id, 0)), EmptyHole)) |> return;
        };
      let tms = tms |> List.map(f);
      let* ds = tms |> sequence;
      switch (ds) {
      | [] =>
        let* ids = ids_derive(ids);
        DHExp.mk(ids, Hole(Some((u, 0)), EmptyHole)) |> return;
      | [hd, ...tl] =>
        // placeholder logic: sequence
        // About ids assignment: here ids can either be created from
        // each term, or altogether created from `uexp.ids`.
        let* ds' =
          List.fold_left(
            (acc, d: DHExp.t) => {
              let* acc = acc;
              let* ids = ids_derive(d.ids);
              DHExp.mk(ids, Seq(d, acc)) |> return;
            },
            hd |> return,
            tl,
          );
        wrap(ds');
      };
    | Closure(_) => raise(ElaboratorError.Exception(Closure))
    | Hole(_) => raise(ElaboratorError.Exception(Hole))
    | Triv => wrap({ids, term: Tuple([])})
    | Bool(b) => wrap({ids, term: Bool(b)})
    | Int(n) => wrap({ids, term: Int(n)})
    | Float(n) => wrap({ids, term: Float(n)})
    | String(s) => wrap({ids, term: String(s)})
    | ListLit(es, None) =>
      //TODO: rewrite this whole case
      let ty =
        switch (Statics.exp_mode(m, uexp)) {
        | Syn => Typ.matched_list(Statics.exp_self_typ(m, uexp))
        | Ana(ana_ty) => Typ.matched_list(ana_ty)
        };
      let* ds =
        List.fold_left(
          (acc, e) => {
            let* acc = acc;
            let e_ty = Statics.exp_self_typ(m, e);
            let+ d = dhexp_of_uexp(m, e);
            let dc = DHExp.cast(d, e_ty, ty);
            acc @ [dc];
          },
          [] |> return,
          es,
        );
      DHExp.mk(
        ids,
        ListLit(ds, Some((u, 0, StandardErrStatus(NotInHole), Int))),
      )
      |> wrap;
    | ListLit(_, Some(_)) =>
      raise(ElaboratorError.Exception(ElaboratedList))
    | FixF(_) => raise(ElaboratorError.Exception(FixF))
    | Fun(p, None, body, None) =>
      let* dp = dhpat_of_upat(m, p);
      let* d1 = dhexp_of_uexp(m, body);
      let ty1 = Statics.pat_typ(m, p);
      wrap(DHExp.mk(ids, DHExp.Fun(dp, Some(ty1), d1, None)));
    | Fun(_) => raise(ElaboratorError.Exception(ElaboratedFun))
    | Tuple(es) =>
      let* ds =
        List.fold_right(
          (e, ds: m(list('a))) => {
            let* ds = ds;
            let* d = dhexp_of_uexp(m, e);
            return([d, ...ds]);
          },
          es,
          return([]),
        );
      let* ids = ids_derive(ids);
      return(DHExp.mk(ids, Tuple(ds)));
    | Tag(name) => wrap({ids, term: Tag(name)})
    | Cons(e1, e2) =>
      let* d1 = dhexp_of_uexp(m, e1);
      let* d2 = dhexp_of_uexp(m, e2);
      let ty1 = Statics.exp_self_typ(m, e1);
      let ty2 = Statics.exp_self_typ(m, e2);
      let dc1 =
        switch (Statics.exp_mode(m, uexp)) {
        | Syn => d1
        | Ana(ty_ana) =>
          let ty = Typ.matched_list(ty_ana);
          DHExp.cast(d1, ty1, ty);
        };
      let ty_hd = Typ.matched_list(Statics.exp_self_typ(m, uexp));
      let dc2 = DHExp.cast(d2, ty2, List(ty_hd));
      wrap({ids, term: Cons(dc1, dc2)});
    | Prj(_) => raise(ElaboratorError.Exception(Prj))
    | Inj(_) => raise(ElaboratorError.Exception(Inj))
    | UnOp(op, e) =>
      let* d = dhexp_of_uexp(m, e);
      let ty = Statics.exp_self_typ(m, e);
      let dc = DHExp.cast(d, ty, Int);
      wrap({ids, term: UnOp(op, dc)});
    | BinOp(op, e1, e2) =>
      let (ty, cons) = exp_binop_of(op);
      let* d1 = dhexp_of_uexp(m, e1);
      let* d2 = dhexp_of_uexp(m, e2);
      let ty1 = Statics.exp_self_typ(m, e1);
      let ty2 = Statics.exp_self_typ(m, e2);
      let dc1 = DHExp.cast(d1, ty1, ty);
      let dc2 = DHExp.cast(d2, ty2, ty);
      wrap({ids, term: cons(dc1, dc2)});
    | Parens(e1) => dhexp_of_uexp(m, e1)
    | Seq(e1, e2) =>
      let* d1 = dhexp_of_uexp(m, e1);
      let* d2 = dhexp_of_uexp(m, e2);
      wrap({ids, term: Seq(d1, d2)});
    | Test(test, None) =>
      let* dtest = dhexp_of_uexp(m, test);
      wrap({ids, term: Test(dtest, Some(u))});
    | Test(_, Some(_)) => raise(ElaboratorError.Exception(ElaboratedTest))
    | Var(name) =>
      switch (err_status) {
      | InHole(Free(Variable)) =>
        DHExp.{ids, term: Hole(Some((u, 0)), FreeVar(name))} |> return
      | _ => wrap({ids, term: Var(name)})
      }
    | Let(p, def, body) =>
      switch (Term.UPat.get_recursive_bindings(p)) {
      | None =>
        /* not recursive */
        let* dp = dhpat_of_upat(m, p);
        let* ddef = dhexp_of_uexp(m, def);
        let* ddef =
          switch (ddef.term) {
          | Fun(a, b, c, _) =>
            let* ids = ids_derive(ddef.ids);
            DHExp.{ids, term: DHExp.Fun(a, b, c, Term.UPat.get_var(p))}
            |> return;
          | _ => return(ddef)
          };
        let* dbody = dhexp_of_uexp(m, body);
        wrap({ids, term: Let(dp, ddef, dbody)});
      | Some([f]) =>
        /* simple recursion */
        let* dp = dhpat_of_upat(m, p);
        let* ddef = dhexp_of_uexp(m, def);
        let* ddef =
          switch (ddef) {
          | {term: Fun(a, b, c, _), ids} =>
            let* ids = ids_derive(ids);
            return(DHExp.{term: DHExp.Fun(a, b, c, Some(f)), ids});
          | _ => return(ddef)
          };
        let* dbody = dhexp_of_uexp(m, body);
        let ty = Statics.pat_self_typ(m, p);
        // FixF here is understand as created from the function
        // definition, not the let expression.
        let* fixpoint_ids = ids_derive(ddef.ids);
        let fixpoint = DHExp.{ids: fixpoint_ids, term: FixF(f, ty, ddef)};
        wrap({ids, term: Let(dp, fixpoint, dbody)});
      | Some(fs) =>
        /* mutual recursion */
        let* dp = dhpat_of_upat(m, p);
        let* ddef = dhexp_of_uexp(m, def);
        let ddef =
          switch (ddef) {
          | {term: Tuple(a), ids} =>
            let b =
              List.map2(
                (s, d) => {
                  switch (d) {
                  | DHExp.{ids, term: DHExp.Fun(a, b, c, _)} =>
                    // This step only assign names to function
                    // definitions, so ids is not changed.
                    DHExp.{ids, term: DHExp.Fun(a, b, c, Some(s))}
                  | _ => d
                  }
                },
                fs,
                a,
              );
            // Same as above.
            DHExp.{ids, term: DHExp.Tuple(b)};
          | _ => ddef
          // ddef here is returned without change, so we don't assign
          // new ids to it
          };

        let* dbody = dhexp_of_uexp(m, body);
        let ty = Statics.pat_self_typ(m, p);
        let (base, derived) = List.nth(def.ids, 0);
        let self_id =
          "__mutual__"
          ++ Id.token_of_t(base)
          ++ "_"
          ++ Id.token_of_t(derived);
        let* self_var_ids = ids_derive(def.ids);
        let self_var = DHExp.{ids: self_var_ids, term: DHExp.Var(self_id)};
        let* (_, substituted_def) =
          fs
          |> List.fold_left(
               (acc, f) => {
                 let* (i, ddef) = acc;
                 let* prj_ids = ids_derive(self_var.ids);
                 let prj =
                   DHExp.{ids: prj_ids, term: DHExp.Prj(self_var, i)};
                 let ddef = Substitution.subst_var(prj, f, ddef);
                 return((i + 1, ddef));
               },
               return((0, ddef)),
             );
        let* fixpoint_ids = ids_derive(substituted_def.ids);
        let fixpoint =
          DHExp.{
            ids: fixpoint_ids,
            term: DHExp.FixF(self_id, ty, substituted_def),
          };
        wrap({ids, term: Let(dp, fixpoint, dbody)});
      }
    | Ap(fn, arg) =>
      let* d_fn = dhexp_of_uexp(m, fn);
      let* d_arg = dhexp_of_uexp(m, arg);
      let ty_fn = Statics.exp_self_typ(m, fn);
      let ty_arg = Statics.exp_self_typ(m, arg);
      let (ty_in, ty_out) = Typ.matched_arrow(ty_fn);
      let c_fn = DHExp.cast(d_fn, ty_fn, Typ.Arrow(ty_in, ty_out));
      let c_arg = DHExp.cast(d_arg, ty_arg, ty_in);
      wrap({ids, term: Ap(c_fn, c_arg)});
    | ApBuiltin(_) => failwith("dhexp_of_uexp on ApBuiltin")
    | If(scrut, e1, e2) =>
      let* d_scrut = dhexp_of_uexp(m, scrut);
      let print = (p, x) =>
        print_endline(p ++ Sexplib.Sexp.to_string_hum(x));
      print("d_scrut: ", DHExp.sexp_of_t(d_scrut));
      let* d1 = dhexp_of_uexp(m, e1);
      let* d2 = dhexp_of_uexp(m, e2);
      let* ids1 = ids_derive(ids);
      let* ids2 = ids_derive(ids);
      let d_rules = [
        (DHPat.{ids: ids1, term: Bool(true)}, d1),
        (DHPat.{ids: ids2, term: Bool(false)}, d2),
      ];
      switch (err_status) {
      | InHole(SynInconsistentBranches(_)) =>
        let* ids = ids_derive(ids);
        DHExp.mk(
          ids,
          Hole(Some((u, 0)), InconsistentBranches(d_scrut, d_rules, 0)),
        )
        |> return;
      | _ => wrap({ids, term: Match(d_scrut, d_rules, 0)})
      };
    | Match(scrut, rules, _) =>
      let* d_scrut = dhexp_of_uexp(m, scrut);
      let* d_rules =
        List.map(
          ((p, e)) => {
            let* d_p = dhpat_of_upat(m, p);
            let+ d_e = dhexp_of_uexp(m, e);
            (d_p, d_e);
          },
          rules,
        )
        |> sequence;
      switch (err_status) {
      | InHole(SynInconsistentBranches(_)) =>
        DHExp.mk(
          ids,
          Hole(Some((u, 0)), InconsistentBranches(d_scrut, d_rules, 0)),
        )
        |> return
      | _ => wrap(DHExp.{ids, term: Match(d_scrut, d_rules, 0)})
      };
    | Cast(_) => raise(ElaboratorError.Exception(Cast))
    };
  | Some(InfoPat(_) | InfoTyp(_) | InfoRul(_) | Invalid(_))
  | None => raise(ElaboratorError.Exception(InvalidInfoMap))
  };
}
and dhpat_of_upat = (m: Statics.map, upat: Term.UPat.t): m(DHPat.t) => {
  switch (Id.Map.find_opt(Term.UPat.rep_id(upat), m)) {
  | Some(InfoPat({mode, self, _})) =>
    let err_status = Statics.error_status(mode, self);
    let maybe_reason: option(ErrStatus.HoleReason.t) =
      switch (err_status) {
      | NotInHole(_) => None
      | InHole(_) => Some(TypeInconsistent)
      };
    let u = Term.UPat.rep_id(upat).id; /* NOTE: using term uids for hole ids */
    let wrap = (d: DHPat.t): m(DHPat.t) =>
      switch (maybe_reason) {
      | None => return(d)
      | Some(reason) =>
        DHPat.{
          ids: upat.ids,
          term: Hole(Some((u, 0)), NonEmptyHole(reason, d)),
        }
        |> return
      };
    let ids = upat.ids;
    let ids_derive = ids => with_id(ids_derive(ids));
    switch (upat.term) {
    | Hole(_, Invalid(_)) /* NOTE: treating invalid as a hole for now */
    | Hole(_, EmptyHole) =>
      let* ids = ids_derive(ids);
      return(DHPat.{ids, term: Hole(Some((u, 0)), EmptyHole)});
    | Hole(_, MultiHole(_)) =>
      // TODO: dhexp, eval for multiholes
      return(DHPat.{ids, term: Hole(Some((u, 0)), EmptyHole)})
    | Hole(_) => failwith("dhexp_of_uexp on Hole")
    | Wild => wrap({ids, term: Wild})
    | Bool(b) => wrap({ids, term: Bool(b)})
    | Int(n) => wrap({ids, term: Int(n)})
    | Float(n) => wrap({ids, term: Float(n)})
    | String(s) => wrap({ids, term: String(s)})
    | Triv => wrap({ids, term: Tuple([])})
    | ListLit(ps, None) =>
      let ty = Typ.matched_list(Statics.pat_self_typ(m, upat));
      let* ds =
        List.fold_left(
          (acc, p) => {
            let* acc = acc;
            let+ d = dhpat_of_upat(m, p);
            acc @ [d];
          },
          return([]),
          ps,
        );
      wrap({ids, term: ListLit(ds, Some(ty))});
    | ListLit(_, Some(_)) => failwith("dhpat_of_upat on ListLit(_, Some(_))")
    | Tag(name) => wrap({ids, term: Tag(name)})
    | Cons(hd, tl) =>
      let* d_hd = dhpat_of_upat(m, hd);
      let* d_tl = dhpat_of_upat(m, tl);
      wrap({ids, term: Cons(d_hd, d_tl)});
    | Inj(_) => raise(ElaboratorError.Exception(Inj))
    | Tuple(ps) =>
      let* dps =
        List.fold_right(
          (p, dps) => {
            let* dps = dps;
            let* dp = dhpat_of_upat(m, p);
            return([dp, ...dps]);
          },
          ps,
          return([]),
        );
      let* ids = ids_derive(ids);
      DHPat.mk(ids, Tuple(dps)) |> return;
    | Var(name) => DHPat.{ids, term: Var(name)} |> return
    | Parens(p) => dhpat_of_upat(m, p)
    | Ap(p1, p2) =>
      let* d_p1 = dhpat_of_upat(m, p1);
      let* d_p2 = dhpat_of_upat(m, p2);
      wrap({ids, term: Ap(d_p1, d_p2)});
    | TypeAnn(p, _ty) =>
      let* dp = dhpat_of_upat(m, p);
      wrap(dp);
    };
  | Some(InfoExp(_) | InfoTyp(_) | InfoRul(_) | Invalid(_))
  | None => raise(ElaboratorError.Exception(InvalidInfoMap))
  };
};

let uexp_elab_wrap_builtins = (d: DHExp.t): m(DHExp.t) => {
  let* builtins = Builtins.elabs(Builtins.Pervasives.builtins);
  List.fold_left(
    (d', (ident, (elab, _))) => {
      let* d' = d';
      let* var_id = with_id(IdGen.fresh);
      let var_ids = [(var_id, var_id)];
      let* let_id = with_id(IdGen.fresh);
      let let_ids = [(let_id, let_id)];
      DHExp.{
        ids: var_ids,
        term: Let({ids: let_ids, term: Var(ident)}, elab, d'),
      }
      |> return;
    },
    return(d),
    builtins,
  );
};

let uexp_elab = (m: Statics.map, uexp: Term.UExp.t): ElaboratorResult.t => {
  print_endline(
    "elaborating: " ++ Sexplib.Sexp.to_string_hum(Term.UExp.sexp_of_t(uexp)),
  );
  let es = ElaboratorState.init;
  let (es, r) = dhexp_of_uexp(m, uexp, es);
  switch (r) {
  | exception (ElaboratorError.Exception(error)) =>
    print_endline(
      "does not elaborated: "
      ++ Sexplib.Sexp.to_string_hum(ElaboratorError.sexp_of_t(error)),
    );
    DoesNotElaborate;
  | d =>
    print_endline(
      "elaborated: " ++ Sexplib.Sexp.to_string_hum(DHExp.sexp_of_t(d)),
    );
    let (_, d) = uexp_elab_wrap_builtins(d, es);
    Elaborates(d, Typ.Unknown(Internal), Delta.empty); //TODO: get type from ci
  };
};
