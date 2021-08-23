open Statics_intf;

module rec Subject: Statics_intf.SUBJECT = {
  let closure = (u, ctx) => (u, 0, id_env(Contexts.gamma(ctx)));

  module Exp = {
    let rec syn_fixed = (finfo: FrameInfo_exp.t, e: Term_exp.t): Type.t => {
      let ty = syn(finfo, e);
      switch (finfo.mode) {
      | Syn => ty
      | Ana(ty') =>
        Type.consistent(ty, ty')
        ? ty : Hole
      | Fn_pos =>
        switch (Type.matches_arrow(ty)) {
        | None => Arrow(Hole, Hole)
        | Some((ty_in, ty_out)) => Arrow(ty_in, ty_out)
        }
      }
    }
    and syn = (finfo: FrameInfo_exp.t, e: Term_exp.t): Type.t => {
      let syn_op = (op: Term_exp.op): Type.t =>
        switch (op) {
        | OpHole(_) => Hole
        | OpText(txt) =>
          switch (txt) {
          | BoolLit(_) => Bool
          | IntLit(_) => Int
          | FloatLit(_) => Float
          | Var(x) =>
            switch (VarMap.lookup(Ctx.gamma(finfo.ctx))) {
            | None => Hole
            | Some(ty) => ty
            }
          | Keyword(_)
          | Invalid(_) => Hole
          }
        | ListNil =>
          // TODO(d) review if this should take into account expected
          List(Hole)
        | Paren(body) => syn_fixed(finfo, body)
        | Inj(side, body) =>
          let ty_body = syn_fixed(Frame.Exp.inj_body(side, finfo), body);
          let (ty_l, ty_r) = InjSide.unpick(side, , Hole);
          Sum(ty_l, ty_r);
        | Case(_) => failwith("todo")
        };

      let syn_pre = ((pre: Term_exp.pre, r: Term_exp.t)): Type.t =>
        switch (pre) {
        | Fun(p) =>
          let ty_p = Pat.syn_fixed(Frame.Pat.fun_pat(finfo), p);
          let ty_body = syn_fixed(Frame.Exp.fun_body(p, finfo), r);
          Arrow(ty_p, ty_body);
        | Let(p, def) =>
          syn_fixed(Frame.Exp.let_body(p, def, finfo), r)
        };

      let syn_post = (l: Term_exp.t, post: Term_exp.post): Type.t =>
        switch (post) {
        | Ap(arg) =>
          let ty_fn = syn_fixed(Frame.Exp.ap_fn(finfo), l);
          switch (Type.matched_arrow(ty_fn)) {
          | None => Hole
          | Some((_ty_in, ty_out)) => ty_out
          }
        };

      let syn_bin = (l: Term_exp.t, bin: Term_exp.bin, r: Term_exp.r): Type.t =>
        switch (bin) {
        | BinHole(_) => Hole
        | BinText(txt) =>
          switch (txt) {
          | BuiltIn(bin) =>
            switch (bin) {
            | Plus | Minus | Times | Divide => Int
            | FPlus | FMinus | FTimes | FDivide => Float
            | LessThan | GreaterThan | Equals
            | FLessThan | FGreaterThan | FEquals
            | And | Or => Bool
            | Cons =>
              let ty_hd = syn_fixed(Frame.Exp.cons_hd(finfo), l);
              List(ty_hd);
            }
          }
        };

      let syn_tup = (elems: list(Term_exp.t)) => {
        let n = List.length(elems);
        let tys =
          elems
          |> List.mapi((i, elem) => syn_fixed(Frame.Exp.prod_n(i, n, finfo), elem));
        Type.Prod(tys);
      };

      Term_exp.get(syn_op, syn_pre, syn_post, syn_bin, syn_tup, e);
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
    and mk_ftile = (
      ftile: Frame_exp.ftile,
      (prefix, suffix): ListFrame.t(Term_exp.tile),
      info: FrameInfo_exp.t,
    ): FrameInfo_exp.t => {
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
