open IdGenCmd;
open IdGenCmd.Syntax;

let delta = (u, ty, ctx) =>
  Delta.singleton(u, (Delta.ExpressionHole, ty, Contexts.gamma(ctx)));

let rec elab_fixed =
        (finfo: FrameInfo_exp.t, e: Term_exp.t)
        : IdGenCmd.t((Delta.t, DTerm_exp.t)) => {
  let elaborated = elab(finfo, e);
  switch (finfo.mode) {
  | Syn => elaborated
  | Ana(ty') =>
    let ty = Statics.Subject.Exp.syn(finfo, e);
    let* (delta, d) = elaborated;
    if (Type.consistent(ty, ty')) {
      return((delta, DTerm_exp.cast(d, ty, ty')));
    } else {
      let* u = next_err();
      return((Delta.add(u, ty', delta), TypeErr(u, d)));
    };
  };
}
and elab =
    (finfo: FrameInfo_exp.t, e: Term_exp.t)
    : IdGenCmd.t((Delta.t, DTerm_exp.t)) => {
  let hole_type = FrameInfo_exp.assigned_hole_type(finfo);

  let closure = u => closure(u, finfo.ctx);
  let delta = (u, ty: Type.t) => delta(u, ty, finfo.ctx);

  let return_empty = d => return((Delta.empty, d));

  let elab_op = (op: Term_exp.op) => {
    switch (op) {
    | OpHole(u) =>
      let d = OpHole(closure(u));
      return((delta(u, hole_type), d));
    | OpText(txt) =>
      switch (txt) {
      | IntLit(n) => return_empty(IntLit(IntLit.to_int(n)))
      | FloatLit(f) => return_empty(FloatLit(FloatLit.to_float(f)))
      | BoolLit(b) => return_empty(BoolLit(string_of_bool(b)))
      | Var(x) =>
        switch (VarMap.lookup(Contexts.gamma(frame.ctx), x)) {
        | None =>
          let* u = next_err();
          let d = FreeVar(closure(u), "_");
          return((delta(u, hole_type), d));
        | Some(_) => return((delta0, BoundVar(x)))
        }
      | Keyword(k) =>
        let* u = next_err();
        return((delta(u, hole_type), Keyword(closure(u), k)));
      | Invalid(s) =>
        let* u = next_err();
        return((delta(u, hole_type), Invalid(closure(u), s)));
      }
    | ListNil =>
      // TODO(d) consider better name for hole_type
      let ty_elem =
        switch (Type.matched_list(hole_type)) {
        | None => Type.Hole
        | Some(ty_elem) => ty_elem
        };
      return_empty(ListNil(ty_elem));
    | Paren(body) => elab(finfo, body)
    | Inj(side, body) =>
      let* (delta_body, d_body) =
        elab(Frame.Exp.inj_body(side, finfo), body);
      let ty_other =
        switch (Type.matched_sum(hole_type)) {
        | None => Type.Hole
        | Some(matched) => snd(InjSide.pick(side, matched))
        };
      let d = DTerm_exp.Inj(ty_other, side, d_body);
      return((delta_body, d));
    | Case(scrut, rules) =>
      let* (delta_scrut, d_scrut) =
        elab(Frame.Exp.case_scrut(finfo), scrut);
      let finfo_pat = Frame.Pat.case_pat(scrut, finfo);
      List.combine(rules, Frame.Exp.case_clauses(scrut, rules, finfo))
      |> List.map((((p, clause), finfo_clause)) => {failwith("todo")});
    //   let* (tys_clauses, deltas_rules, ds_rules) =
    //     rules
    //     |> List.map(((p, clause)) => {
    //       let* (_, _, delta_p, d_p) =
    //         Pat.elab(Frame.Pat.case_pat(scrut, finfo), p);
    //       let* (ty_clause, delta_clause, d_clause) =
    //         elab(Frame.Exp.case_clause(scrut, p, finfo), clause);
    //       let delta = Delta.union(delta_p, delta_clause);
    //       return((ty_clause, delta, (d_p, d_clause)));
    //     })
    //     |> sequence
    //     |> map(ListUtil.unzip3);
    //   switch (Type.join_all(GLB, tys_clauses)) {
    //   | None =>
    //     // put in hole
    //   | Some(glb) =>
    //   }
    };
  };

  let elab_pre = ((pre: Term_exp.pre, r: Term_exp.t)) =>
    switch (pre) {
    | Fun(p) =>
      let finfo_pat = Frame.Pat.fun_pat(finfo);
      let (ty_p, _) = Pat.syn(finfo_pat, p);
      let* (delta_p, d_p) = Pat.elab(finfo_pat, p);
      let* (delta_body, d_body) = elab(Frame.Exp.fun_body(p, finfo), r);
      let delta = Delta.union(delta_p, delta_body);
      let d = DTerm_exp.Fun(d_p, ty_p, d_body);
      return((delta, d));
    | Let(p, def) =>
      let* (delta_p, d_p) = Pat.elab(Frame.Pat.let_pat(finfo), p);
      let* (delta_def, d_def) = elab(Frame.Exp.let_def(ty_p, finfo), def);
      let* (delta_body, d_body) =
        elab(Frame.Exp.let_body(p, def, finfo), r, u_gen);
      let delta = Delta.union([delta_p, delta_def, delta_body]);
      let d = DTerm_exp.Let(d_p, d_def, d_body);
      return((delta, d));
    };

  let elab_post = ((l: Term_exp.t, post: Term_exp.post)) =>
    switch (post) {
    | Ap(arg) =>
      // TODO consider computing together
      // let (finfo_fn, finfo_ap) = Frame.ap(l, finfo);
      let* (delta_fn, d_fn) = elab(Frame.Exp.ap_fn(finfo), l);
      let* (delta_arg, d_arg) = elab(Frame.Exp.ap_arg(l, finfo), arg);
      let delta = union([delta_fn, delta_arg]);
      let d = DTerm_exp.Ap(d_fn, d_arg);
      return((delta, d));
    };

  let elab_bin = ((l: Term_exp.t, bin: Term_exp.bin, r: Term_exp.t)) =>
    failwith("todo");

  let elab_tup = (elems: list(Term_exp.t)) => {
    let n = List.length(elems);
    let* (deltas, ds) =
      elems
      |> List.mapi((i, elem) => elab(Frame.Exp.prod_n(i, n, finfo), elem))
      |> sequence
      |> map(List.split);
    let delta = IntMap.union(deltas);
    let d = DTerm_exp.Tuple(ds);
    return((delta, d));
  };

  Term_exp.get(elab_op, elab_pre, elab_post, elab_bin, elab_tup, e);
};

// // external synthesized type
// let ty = syn(finfo, e);
// // would be nice if ty was just the assigned type of elaborated
// // - but what about type shown in cursor inspector?
// // - let's just go with it for now and come back to cursor inspector
// if (FrameInfo_exp.inconsistent(ty, finfo)) {
//   // insert non-empty hole, no cast necessary
//   let* (delta, d) = elaborated;
//   let* u = IdGenCmd.next_err();
//   let d = DTerm
// } else {
//   // check for type equality, insert cast if not
//   // case to consider: unbound variables
//   // if it synthesized hole type, then this would insert
//   // spurious cast and delta would be incorrect
//   // but this should be resolved by treating unbound
//   // variables separately based on mode in syn
//   elaborated;

// let assigned_ty = {
//   let ty = syn(finfo, e);
//   if (Term_exp.is_hole(e) || FrameInfo_exp.inconsistent(ty, finfo)) {
//     hole_type
//   } else {
//     ty
//   };
// }

// // external synthesized type
// let ty = syn(finfo, e);

// let elaborated =
//   Term_exp.get(elab_op, elab_pre, elab_post, elab_bin, elab_tup, e);

// if (FrameInfo_exp.inconsistent(ty, finfo)) {
//   let* (delta, d) = elaborated;
//   let* u = IdGenCmd.next_hole();
//   // TODO add cast to d in addition to type err
//   return((Delta.add(u, Hole, delta), TypeErr(closure(u), d)));
// } else {
//   elaborated;
// };
