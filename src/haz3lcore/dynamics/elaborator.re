open Util;
open OptUtil.Syntax;

module ElaborationResult = {
  [@deriving sexp]
  type t =
    | Elaborates(DHExp.t, Typ.t, Delta.t)
    | DoesNotElaborate;
};

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

let ids_derive = DHExp.ids_derive;

let rec dhexp_of_uexp = (m: Statics.map, uexp: Term.UExp.t): option(DHExp.t) => {
  /* NOTE: Left out delta for now */
  switch (Id.Map.find_opt(Term.UExp.rep_id(uexp), m)) {
  | Some(InfoExp({mode, self, _})) =>
    let err_status = Statics.error_status(mode, self);
    let maybe_reason: option(ErrStatus.HoleReason.t) =
      switch (err_status) {
      | NotInHole(_) => None
      | InHole(_) => Some(TypeInconsistent)
      };
    let u = Term.UExp.rep_id(uexp).base; /* NOTE: using term uids for hole ids */
    let wrap = (d: DHExp.t): option(DHExp.t) =>
      switch (maybe_reason) {
      | None => Some(d)
      | Some(reason) =>
        // We don't assign derived ids here since they should be done
        // outside of wrap.
        Some({ids: d.ids, term: Hole((u, 0), NonEmpty(reason, d))})
      };
    let ids = uexp.ids;
    switch (uexp.term) {
    | Invalid(_) /* NOTE: treating invalid as a hole for now */
    | EmptyHole => Some(DHExp.{ids, term: Hole((u, 0), Empty)})
    | MultiHole(tms) =>
      // TODO: dhexp, eval for multiholes
      let* ds =
        tms
        |> List.map((t: Term.t) =>
             switch (t) {
             | Term.Exp(e) => dhexp_of_uexp(m, e)
             | _ =>
               Some({
                 ids: Term.ids(t),
                 term: Hole((Term.rep_id(t).base, 0), Empty),
               })
             }
           )
        |> OptUtil.sequence;
      switch (ds) {
      | [] => Some(DHExp.{ids, term: Hole((u, 0), Empty)})
      | [hd, ...tl] =>
        // placeholder logic: sequence
        // About ids assignment: here ids can either be created from
        // each term, or altogether created from `uexp.ids`.
        let (ds', _) =
          tl
          |> List.fold_left(
               // (acc, d) => DHExp.{ids: d.ids, term: DHExp.Seq(d, acc)},
               // (acc, d) => DHExp.{ids: d.ids @ acc.ids, term: DHExp.Seq(d, acc)},
               ((acc, ids), d) =>
                 (
                   DHExp.{ids: ids_derive(ids), term: DHExp.Seq(d, acc)},
                   ids_derive(ids),
                 ),
               (hd, uexp.ids),
             );
        wrap(ds');
      };
    | Closure(_) => None
    | Hole(_) => None
    | Error(_) => None
    | Triv => wrap({ids, term: Tuple([])})
    | Bool(b) => wrap({ids, term: Bool(b)})
    | Int(n) => wrap({ids, term: Int(n)})
    | Float(n) => wrap({ids, term: Float(n)})
    | String(s) => wrap({ids, term: String(s)})
    | ListLit(es, None) =>
      //TODO: rewrite this whole case
      switch (Statics.exp_mode(m, uexp)) {
      | Syn =>
        let ty = Typ.matched_list(Statics.exp_self_typ(m, uexp));
        let* ds =
          List.fold_left(
            (acc, e) => {
              let* acc = acc;
              let e_ty = Statics.exp_self_typ(m, e);
              let+ d = dhexp_of_uexp(m, e);
              let dc = DHExp.cast(d, e_ty, ty);
              acc @ [dc];
            },
            Some([]),
            es,
          );
        // wrap(DHExp.ListLit(u, 0, StandardErrStatus(NotInHole), Int, ds));
        wrap({
          ids,
          term:
            ListLit(ds, Some((u, 0, StandardErrStatus(NotInHole), Int))),
        });
      | Ana(ana_ty) =>
        let ty = Typ.matched_list(ana_ty);
        let* ds =
          List.fold_left(
            (acc, e) => {
              let* acc = acc;
              let e_ty = Statics.exp_self_typ(m, e);
              let+ d = dhexp_of_uexp(m, e);
              let dc = DHExp.cast(d, e_ty, ty);
              acc @ [dc];
            },
            Some([]),
            es,
          );
        wrap({
          ids,
          term:
            ListLit(ds, Some((u, 0, StandardErrStatus(NotInHole), Int))),
        });
      }
    | ListLit(_, Some(_)) => None
    | FixF(_) => None
    | Fun(p, None, body, None) =>
      let* dp = dhpat_of_upat(m, p);
      let* d1 = dhexp_of_uexp(m, body);
      let ty1 = Statics.pat_typ(m, p);
      wrap({ids, term: DHExp.Fun(dp, Some(ty1), d1, None)});
    | Fun(_) => None
    | Tuple(es) =>
      let ds =
        List.fold_right(
          (e, ds_opt) => {
            switch (ds_opt) {
            | None => None
            | Some(ds) =>
              switch (dhexp_of_uexp(m, e)) {
              | None => None
              | Some(d) => Some([d, ...ds])
              }
            }
          },
          es,
          Some([]),
        );
      ds |> Option.map(ds => DHExp.{ids, term: Tuple(ds)});
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
    | Prj(_) => None
    | Inj(_) => None
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
    | Test(_, Some(_)) => failwith("dhexp_of_uexp Test(_, Some(_))")
    | Var(name) =>
      switch (err_status) {
      | InHole(Free(Variable)) =>
        Some({ids, term: Hole((u, 0), FreeVar(name))})
      | _ => wrap({ids, term: Var(name)})
      }
    | Let(p, def, body) =>
      switch (Term.UPat.get_recursive_bindings(p)) {
      | None =>
        /* not recursive */
        let* dp = dhpat_of_upat(m, p);
        let* ddef = dhexp_of_uexp(m, def);
        let ddef =
          switch (ddef.term) {
          | Fun(a, b, c, _) =>
            DHExp.{
              ids: ids_derive(ddef.ids),
              term: DHExp.Fun(a, b, c, Term.UPat.get_var(p)),
            }
          | _ => ddef
          };
        let* dbody = dhexp_of_uexp(m, body);
        wrap({ids, term: Let(dp, ddef, dbody)});
      | Some([f]) =>
        /* simple recursion */
        let* dp = dhpat_of_upat(m, p);
        let* ddef = dhexp_of_uexp(m, def);
        let ddef =
          switch (ddef) {
          | {term: Fun(a, b, c, _), ids} =>
            DHExp.{term: DHExp.Fun(a, b, c, Some(f)), ids: ids_derive(ids)}
          | _ => ddef
          };
        let* dbody = dhexp_of_uexp(m, body);
        let ty = Statics.pat_self_typ(m, p);
        // FixF here is understand as created from the function
        // definition, not the let expression.
        let fixpoint =
          DHExp.{ids: ids_derive(ddef.ids), term: FixF(f, ty, ddef)};
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
        let uniq_id = List.nth(def.ids, 0);
        let self_id =
          "__mutual__"
          ++ string_of_int(uniq_id.base)
          ++ "_"
          ++ string_of_int(uniq_id.derived);
        let self_var =
          DHExp.{ids: ids_derive(def.ids), term: DHExp.Var(self_id)};
        let (_, substituted_def) =
          fs
          |> List.fold_left(
               ((i, ddef), f) => {
                 let prj =
                   DHExp.{
                     ids: ids_derive(~step=i + 1, self_var.ids),
                     term: DHExp.Prj(self_var, i),
                   };
                 let ddef = Substitution.subst_var(prj, f, ddef);
                 (i + 1, ddef);
               },
               (0, ddef),
             );
        let fixpoint =
          DHExp.{
            ids: ids_derive(substituted_def.ids),
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
      let* d1 = dhexp_of_uexp(m, e1);
      let* d2 = dhexp_of_uexp(m, e2);
      // The two branches introduced by If -> Match transformation are
      // assigned with ids derived from if-expression.
      let d_rules = [
        (DHPat.{ids: ids_derive(ids, ~step=1), term: Bool(true)}, d1),
        (DHPat.{ids: ids_derive(ids, ~step=2), term: Bool(false)}, d2),
      ];
      switch (err_status) {
      | InHole(SynInconsistentBranches(_)) =>
        Some(
          DHExp.{
            ids,
            term: Hole((u, 0), InconsistentBranches(d_scrut, d_rules, 0)),
          },
        )
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
        |> OptUtil.sequence;
      switch (err_status) {
      | InHole(SynInconsistentBranches(_)) =>
        Some(
          DHExp.{
            ids,
            term: Hole((u, 0), InconsistentBranches(d_scrut, d_rules, 0)),
          },
        )
      | _ => wrap(DHExp.{ids, term: Match(d_scrut, d_rules, 0)})
      };
    | Cast(_) => None
    };
  | Some(InfoPat(_) | InfoTyp(_) | InfoRul(_) | Invalid(_))
  | None => None
  };
}
and dhpat_of_upat = (m: Statics.map, upat: Term.UPat.t): option(DHPat.t) => {
  switch (Id.Map.find_opt(Term.UPat.rep_id(upat), m)) {
  | Some(InfoPat({mode, self, _})) =>
    let err_status = Statics.error_status(mode, self);
    let maybe_reason: option(ErrStatus.HoleReason.t) =
      switch (err_status) {
      | NotInHole(_) => None
      | InHole(_) => Some(TypeInconsistent)
      };
    let u = Term.UPat.rep_id(upat).base; /* NOTE: using term uids for hole ids */
    let wrap = (d: DHPat.t): option(DHPat.t) =>
      switch (maybe_reason) {
      | None => Some(d)
      | Some(reason) =>
        Some({ids: upat.ids, term: Hole((u, 0), NonEmpty(reason, d))})
      };
    let ids = upat.ids;
    switch (upat.term) {
    | Invalid(_) /* NOTE: treating invalid as a hole for now */
    | EmptyHole => Some({ids, term: Hole((u, 0), Empty)})
    | MultiHole(_) =>
      // TODO: dhexp, eval for multiholes
      Some({ids, term: Hole((u, 0), Empty)})
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
          Some([]),
          ps,
        );
      wrap({ids, term: ListLit(ds, Some(ty))});
    | ListLit(_, Some(_)) => failwith("dhpat_of_upat on ListLit(_, Some(_))")
    | Tag(name) => wrap({ids, term: Tag(name)})
    | Cons(hd, tl) =>
      let* d_hd = dhpat_of_upat(m, hd);
      let* d_tl = dhpat_of_upat(m, tl);
      wrap({ids, term: Cons(d_hd, d_tl)});
    | Inj(_) => None
    | Tuple(ps) =>
      let dps =
        List.fold_right(
          (p, dps_opt) => {
            switch (dps_opt) {
            | None => None
            | Some(dps) =>
              switch (dhpat_of_upat(m, p)) {
              | None => None
              | Some(dp) => Some([dp, ...dps])
              }
            }
          },
          ps,
          Some([]),
        );
      dps |> Option.map(ds => DHPat.{ids, term: Tuple(ds)});
    | Var(name) => Some({ids, term: Var(name)})
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
  | None => None
  };
};

let uexp_elab_wrap_builtins = (d: DHExp.t): DHExp.t =>
  List.fold_left(
    (d', (ident, (elab, _))) =>
      DHExp.{
        ids: [Id.invalid],
        term: Let({ids: [Id.invalid], term: Var(ident)}, elab, d'),
      },
    d,
    Builtins.forms(Builtins.Pervasives.builtins),
  );

let uexp_elab = (m: Statics.map, uexp: Term.UExp.t): ElaborationResult.t =>
  switch (dhexp_of_uexp(m, uexp)) {
  | None => DoesNotElaborate
  | Some(d) =>
    let d = uexp_elab_wrap_builtins(d);
    Elaborates(d, Typ.Unknown(Internal), Delta.empty); //TODO: get type from ci
  };
