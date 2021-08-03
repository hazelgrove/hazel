open Statics_intf;

module rec Subject: Statics_intf.SUBJECT = {
  let closure = (u, ctx) => (u, 0, id_env(Contexts.gamma(ctx)));

  module Exp = {
    let delta = (u, ty, ctx) =>
      Delta.singleton(u, (Delta.ExpressionHole, ty, Contexts.gamma(ctx)));

    let rec elab =
            (finfo: FrameInfo_exp.t, e: Term_exp.t)
            : IdGenCmd.t(SubjectInfo_exp.t) => {
      let hole_type = FrameInfo_exp.assigned_hole_type(finfo);

      let closure = u => closure(u, finfo.ctx);
      let delta = (u, ty: Type.t) => delta(u, ty, finfo.ctx);
      let delta0 = Delta.empty;

      open IdGenCmd;
      open IdGenCmd.Syntax;

      let elab_op = (op: Term_exp.op) => {
        switch (op) {
        | OpHole(u) =>
          let d = OpHole(closure(u));
          return((hole_type, delta(u, hole_type), d));
        | OpText(txt) =>
          switch (txt) {
          | IntLit(n) => return((Int, delta0, IntLit(IntLit.to_int(n)))) 
          | FloatLit(f) => return(
              (Float, delta0, FloatLit(FloatLit.to_float(f))),
            )
          | BoolLit(b) => return((Bool, delta0, BoolLit(string_of_bool(b))))
          | Var(x) =>
            switch (VarMap.lookup(Contexts.gamma(frame.ctx), x)) {
            | None =>
              let* u = next_err();
              let d = FreeVar(closure(u), "_");
              return((hole_type, delta(u, hole_type), d));
            | Some(ty) => return((ty, delta0, BoundVar(x)))
            };
          | Keyword(k) =>
            let* u = next_err();
            return((hole_type, delta(u, hole_type), Keyword(closure(u), k)));
          | Invalid(s) =>
            let* u = next_err();
            return((hole_type, delta(u, hole_type), Invalid(closure(u), s)));
          }
        | ListNil =>
          // TODO(d) consider better name for hole_type
          let ty_elem =
            switch (Type.matched_list(hole_type)) {
            | None => Type.Hole
            | Some(ty_elem) => ty_elem
            };
          return((Type.List(ty_elem), delta0, ListNil(ty_elem)));
        | Paren(body) => elab(finfo, body)
        | Inj(side, body) =>
          let* (ty_body, delta_body, d_body) =
            elab(Frame.Exp.inj_body(side, finfo), body);
          let ty_other =
            switch (Type.matched_sum(hole_type)) {
            | None => Type.Hole
            | Some(matched) => snd(InjSide.pick(side, matched))
            };
          let ty = {
            let (ty_l, ty_r) = InjSide.unpick(side, (ty_body, ty_other));
            Type.Sum(ty_l, ty_r);
          };
          let d = DTerm_exp.Inj(ty_other, side, d_body);
          return((ty, delta_body, d));
        | Case(scrut, rules) =>
          let* (ty_scrut, delta_scrut, d_scrut) =
            elab(Frame.Exp.case_scrut(finfo), scrut);
          let finfo_pat = Frame.Pat.case_pat(scrut, finfo);
          List.combine(rules, Frame.Exp.case_clauses(scrut, rules, finfo))
          |> List.map((((p, clause), finfo_clause)) => {
            let* (_, _, delta_p, d_p) =
          })



          let* (tys_clauses, deltas_rules, ds_rules) =
            rules
            |> List.map(((p, clause)) => {
              let* (_, _, delta_p, d_p) =
                Pat.elab(Frame.Pat.case_pat(scrut, finfo), p);
              let* (ty_clause, delta_clause, d_clause) =
                elab(Frame.Exp.case_clause(scrut, p, finfo), clause);
              let delta = Delta.union(delta_p, delta_clause);
              return((ty_clause, delta, (d_p, d_clause)));
            })
            |> sequence
            |> map(ListUtil.unzip3);
          switch (Type.join_all(GLB, tys_clauses)) {
          | None =>
            // put in hole
          | Some(glb) =>
            
          }
        };
      };

      let elab_pre = ((pre: Term_exp.pre, r: Term_exp.t)) =>
        switch (pre) {
        | Fun(p) =>
          let* (ty_p, _, delta_p, d_p) =
            Pat.elab(Frame.Pat.fun_pat(finfo), p);
          let* (ty_body, delta_body, d_body) =
            elab(Frame.Exp.fun_body(p, finfo), r);
          let ty = Type.Arrow(ty_p, ty_body);
          let delta = Delta.union(delta_p, delta_body);
          let d = DTerm_exp.Fun(d_p, ty_p, d_body);
          return((ty, delta, d));
        | Let(p, def) =>
          let* (ty_p, _, delta_p, d_p) =
            Pat.elab(Frame.Pat.let_pat(finfo), p);
          let* (ty_def, delta_def, d_def) =
            elab(Frame.Exp.let_def(ty_p, finfo), def);
          let* (ty_body, delta_body, d_body) =
            elab(Frame.Exp.let_body(p, def, finfo), r, u_gen);
          let delta = Delta.union([delta_p, delta_def, delta_body]);
          let d = DTerm_exp.Let(d_p, d_def, d_body);
          return((ty_body, delta, d));
        };

      let elab_post = ((l: Term_exp.t, post: Term_exp.post)) =>
        switch (post) {
        | Ap(arg) =>
          // TODO consider computing together
          // let (finfo_fn, finfo_ap) = Frame.ap(l, finfo);
          let* (ty_fn, delta_fn, d_fn) =
            elab(Frame.Exp.ap_fn(finfo), l);
          let* (ty_arg, delta_arg, d_arg) =
            elab(Frame.Exp.ap_arg(l, finfo), arg);
          let ty =
            switch (Type.matched_arrow(ty_fn)) {
            | Some((_ty_in, ty_out)) => ty_out
            | None => Type.Hole
            };
          let delta = union([delta_fn, delta_arg]);
          let d = DTerm_exp.Ap(d_fn, d_arg);
          return((ty, delta, d));
        };

      let elab_bin = ((l: Term_exp.t, bin: Term_exp.bin, r: Term_exp.t)) =>
        failwith("todo");

      let elab_ntup = (elems: list(Term_exp.t)) => {
        let n = List.length(elems);
        let* (tys, deltas, ds) =
          elems
          |> List.mapi((i, elem) =>
            elab(Frame.Exp.prod_n(i, n, finfo), elem)
          )
          |> sequence
          |> map(ListUtil.unzip3);
        let ty = Type.Prod(tys);
        let delta = IntMap.union(deltas);
        let d = DTerm_exp.Tuple(ds);
        return((ty, delta, d));
      };

      let elaborated =
        Term_exp.get(elab_op, elab_pre, elab_post, elab_bin, elab_ntup, e);

      if (FrameInfo_exp.inconsistent(ty, finfo)) {
        let* (_, delta, d) = elaborated;
        let* u = IdGenCmd.next_hole();
        return((Type.Hole, Delta.add(u, Hole, delta), TypeErr(closure(u), d)));
      } else {
        elaborated;
      };
    };

    let syn = (finfo, e) => {
      // note: could re-implement from scratch if fast pass desired,
      // since just synthesizing assigned type can ignore subtrees
      let ((ty, _, _), _) = elab(finfo, e, MetaVarGen.init);
      ty;
    };
  };
}
and Frame: FRAME = {
  module Exp = {
    let rec mk =
            ((skel, (prefix, _) as tiles, ftile): Frame_exp.t, info): info => {
      let info = mk_ftile(ftile, info);
      let m = Skel.root_index(skel);
      let n = List.length(prefix);
      if (m < n) {
        // recurse toward subject
        {};
      } else if (m > n) {
        // recurse toward subject
        {};
      } else {
        // found subject
        info;
      };
    }
    and mk_ftile = (ftile: Frame_exp.ftile, info): info => {
      switch (ftile) {
      | Root => info
      | Paren_body(frame) => mk(frame, info)
      | Case_scrut(_, frame) => case_scrut(mk(frame, info))
      | Case_clause(scrut, p, rules, frame) =>
        case_clause(scrut, p, mk(frame, info))
      | Let_def(p, frame) => let_def(p, mk(frame, info))
      | Ap_arg(frame) =>
        let fn = failwith("todo");
        ap_arg(fn, mk(frame, info));
      };
    };
  };
};
