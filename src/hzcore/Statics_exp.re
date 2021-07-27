open Statics_intf;

module rec Subject: Statics_intf.SUBJECT = {
  let closure = (u, ctx) => (u, 0, id_env(Contexts.gamma(ctx)));

  module Exp = {
    let delta = (u, ty, ctx) =>
      Delta.singleton(u, (Delta.ExpressionHole, ty, Contexts.gamma(ctx)));

    let rec elab =
            (finfo: FrameInfo_exp.t, e: Term_exp.t, u_gen: MetaVarGen.t)
            : ((SubjectInfo_exp.t, MetaVarGen.t) as 'out) => {
      let hole_type = FrameInfo_exp.assigned_hole_type(finfo);

      let closure = u => closure(u, finfo.ctx);
      let delta = (u, ty: Type.t) => delta(u, ty, finfo.ctx);
      let delta0 = Delta.empty;

      let elab_op = (op: Term_exp.op) => {
        switch (op) {
        | OpHole(u) =>
          let d = OpHole(closure(u));
          ((hole_type, delta(u, hole_type), d), u_gen);
        | OpText(txt) =>
          switch (txt) {
          | IntLit(n) => ((Int, delta0, IntLit(IntLit.to_int(n))), u_gen)
          | FloatLit(f) => (
              (Float, delta0, FloatLit(FloatLit.to_float(f))),
              u_gen,
            )
          | BoolLit(b) => ((Bool, delta0, BoolLit(string_of_bool(b))), u_gen)
          | Underscore
          | Var(_) =>
            let x =
              switch (txt) {
              | Var(x) => x
              | _underscore => "_"
              };
            switch (VarMap.lookup(Contexts.gamma(frame.ctx), x)) {
            | None =>
              let (u, u_gen) = MetaVarGen.next(u_gen);
              let d = FreeVar(closure(u), "_");
              ((hole_type, delta(u, hole_type), d), u_gen);
            | Some(ty) => ((ty, delta0, BoundVar(x)), u_gen)
            };
          | Keyword(k) =>
            let (u, u_gen) = MetaVarGen.next(u_gen);
            ((hole_type, delta(u, hole_type), Keyword(closure(u), k)), u_gen);
          | Invalid(s) =>
            let (u, u_gen) = MetaVarGen.next(u_gen);
            ((hole_type, delta(u, hole_type), Invalid(closure(u), s)), u_gen);
          }
        | ListNil =>
        };
      };

      let elab_pre = ((pre: Term_exp.pre, r: Term_exp.t)) =>
        switch (pre) {
        | Fun(p) =>
          let ((ty_p, ctx_body, delta_p, d_p), u_gen) = {
            let finfo_p = Frame.Pat.fun_pat(finfo);
            Statics_pat.elab_s(finfo_p, p, u_gen);
          };
          let ((ty_body, delta_body, d_body), u_gen) = {
            let finfo_body = Frame.Exp.fun_body(p, finfo);
            elab_s(finfo_body, r, u_gen);
          };
          let ty = Type.Arrow(ty_p, ty_body);
          let delta = Delta.union(delta_p, delta_body);
          let d = DTerm_exp.Fun(d_p, ty_p, d_body);
          ((ty, delta, d), u_gen);
        | Let(p, def) =>
          let ((ty_p, _, delta_p, d_p), u_gen) =
            Statics_pat.elab_s(Frame.Pat.let_pat(finfo), p, u_gen);
          let ((ty_def, delta_def, d_def), u_gen) = {
            let finfo_def = Frame.Exp.let_def(ty_p, finfo);
            elab_s(frame_def, def, u_gen);
          };
          let ((ty_body, delta_body, d_body), u_gen) = {
            let finfo_body = Frame.Exp.let_body(p, def, finfo);
            elab_s(frame_body, r, u_gen);
          };
          let delta = Delta.union([delta_p, delta_def, delta_body]);
          let d = DTerm_exp.Let(d_p, d_def, d_body);
          ((ty_body, delta, d), u_gen);
        };

      let elab_post = ((l: Term_exp.t, post: Term_exp.post)) =>
        switch (post) {
        | Ap(arg) =>
          // TODO consider computing together
          // let (finfo_fn, finfo_ap) = Frame.ap(l, finfo);
          let ((ty_fn, delta_fn, d_fn), u_gen) =
            elab_s(Frame.Exp.ap_fn(finfo), l, u_gen);
          let ((ty_arg, delta_arg, d_arg), u_gen) =
            elab_s(Frame.Exp.ap_arg(l, finfo), arg, u_gen);
          let ty =
            switch (Type.matched_arrow(ty_fn)) {
            | Some((_ty_in, ty_out)) => ty_out
            | None => Type.Hole
            };
          let delta = union([delta_fn, delta_arg]);
          let d = DTerm_exp.Ap(d_fn, d_arg);
          ((ty, delta, d), u_gen);
        };

      let elab_bin = ((l: Term_exp.t, bin: Term_exp.bin, r: Term_exp.t)) =>
        failwith("todo");

      let elab_ntup = (elems: list(Term_exp.t)) => {
        let (u_gen, (ds, tys, deltas)) =
          elems
          |> List.mapi((i, elem) => (i, elem))
          |> ListUtil.map_with_accumulator((u_gen, (i, elem)) => {
              let finfo = Frame.Exp.prod_n(i, List.length(elems), finfo);
              TupleUtil.swap(elab_t(finfo, elem, u_gen));
            })
          |> List.mapi((i, skel) => {})
          |> ListUtil.unzip3;
        let ty = Type.Prod(tys);
        ((ty, IntMap.union(deltas), d), u_gen);
      };

      let ((ty, delta, d), u_gen) as r =
        Term_exp.get(elab_op, elab_pre, elab_post, elab_bin, elab_ntup, e);

      if (FrameInfo_exp.inconsistent(ty, finfo)) {
        let (u, u_gen) = MetaVarGen.next(u_gen);
        ((ty, Delta.add(u, Hole, delta), TypeErr(closure(u), d)), u_gen);
      } else {
        r;
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
        {};
      } else if (m > n) {
        {};
      } else {
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
