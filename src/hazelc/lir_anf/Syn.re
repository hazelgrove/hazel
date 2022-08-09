open Util.ResultSexp;

open Holes;

[@deriving sexp]
type syn_types = ExprLabel.Map.t(Typ.t);

[@deriving sexp]
type syn_ok = {types: syn_types};

[@deriving sexp]
type syn_error =
  | CaseEmptyRules(ExprLabel.t)
  | UnboundVar(ExprLabel.t)
  | UnboundHole(ExprLabel.t)
  | WrongHoleSort(ExprLabel.t)
  | UnboundVarSigma(ExprLabel.t, Ident.t)
  | TypeNotNecessarilyComplete(ExprLabel.t, Typ.t)
  | TypeNotIndeterminatelyIncomplete(ExprLabel.t, Typ.t)
  | TypeNotNecessarilyIncomplete(ExprLabel.t, Typ.t)
  | TypesNotEqual(ExprLabel.t, Typ.t, Typ.t)
  | BaseTypesNotEqual(ExprLabel.t, Typ.t_, Typ.t_)
  | BaseTypesEqual(ExprLabel.t, Typ.t_, Typ.t_)
  | BaseTypesInconsistent(ExprLabel.t, Typ.t_, Typ.t_)
  | PatScrutTypesNotEqual(PatLabel.t, Typ.t_, Typ.t_);

[@deriving sexp]
type syn_result = result(syn_ok, syn_error);

module SynMonad = {
  module State = {
    [@deriving sexp]
    type t = syn_ok;

    let init = {types: ExprLabel.Map.empty};
  };

  module StateMonad = Util.StateMonad.Make(State);
  include Util.Monads.Make_Monad_B({
    type t('a) = StateMonad.t(result('a, syn_error));

    let return = (v: 'a): t('a) => Ok(v) |> StateMonad.return;

    let bind = (m: t('a), f: 'a => t('b)): t('b) =>
      StateMonad.bind(
        m,
        fun
        | Ok(v) => f(v)
        | Error(err) => StateMonad.return(Error(err)),
      );
  });

  let fail = (err, s) => (s, Error(err));
  let get = s => (s, Ok(s));
  let put = (x, _) => (x, Ok());
  let update = f => bind(get, s => put(f(s)));

  let init = State.init;
};

include SynMonad;
include SynMonad.Syntax;

let extend = (l, ty) => {
  let+ () =
    update(({types}) => {
      let types = ExprLabel.Map.add(l, ty, types);
      {types: types};
    });
  ty;
};

let ty__of_bin_op: Anf.bin_op => Typ.t_ =
  fun
  | OpAnd
  | OpOr => TBool
  | OpPlus
  | OpMinus
  | OpTimes
  | OpDivide
  | OpLessThan
  | OpGreaterThan
  | OpEquals => TInt
  | OpFPlus
  | OpFMinus
  | OpFTimes
  | OpFDivide
  | OpFLessThan
  | OpFGreaterThan
  | OpFEquals => TFloat;

let rec ana_pat =
        (
          ctx,
          {kind, label: l, complete: _}: Pat.t,
          (scrut_cc, scrut_ty_): Typ.t,
        ) => {
  switch (kind) {
  | PPair(p1, p2) =>
    switch (scrut_ty_) {
    | TPair(ty1, ty2) =>
      let* ctx = ana_pat(ctx, p1, (scrut_cc, ty1));
      ana_pat(ctx, p2, (scrut_cc, ty2));
    /* FIXME: Hole is just a placeholder. */
    | _ => PatScrutTypesNotEqual(l, scrut_ty_, TPair(THole, THole)) |> fail
    }

  | PCons(p1, p2) =>
    switch (scrut_ty_) {
    | TList(ty_) =>
      let* ctx = ana_pat(ctx, p1, (scrut_cc, ty_));
      ana_pat(ctx, p2, (scrut_cc, TList(ty_)));

    /* FIXME: Hole is just a placeholder. */
    | _ => PatScrutTypesNotEqual(l, scrut_ty_, TList(THole)) |> fail
    }

  | PInj(side, p') =>
    switch (side, scrut_ty_) {
    | (PInjL, TSum(ty, _))
    | (PInjR, TSum(_, ty)) => ana_pat(ctx, p', (scrut_cc, ty))
    /* FIXME: Hole is just a placeholder. */
    | (PInjL, _) =>
      PatScrutTypesNotEqual(l, scrut_ty_, TSum(THole, THole)) |> fail
    | (PInjR, _) =>
      PatScrutTypesNotEqual(l, scrut_ty_, TSum(THole, THole)) |> fail
    }

  | PWild => ctx |> return
  | PVar(x) => TypContext.add(x, (scrut_cc, scrut_ty_), ctx) |> return

  | PBool(_b) =>
    switch (scrut_ty_) {
    | TBool => ctx |> return
    | _ => PatScrutTypesNotEqual(l, scrut_ty_, TBool) |> fail
    }

  | PInt(_n) =>
    switch (scrut_ty_) {
    | TInt => ctx |> return
    | _ => PatScrutTypesNotEqual(l, scrut_ty_, TInt) |> fail
    }

  | PFloat(_f) =>
    switch (scrut_ty_) {
    | TFloat => ctx |> return
    | _ => PatScrutTypesNotEqual(l, scrut_ty_, TFloat) |> fail
    }

  | PNil =>
    switch (scrut_ty_) {
    | TList(_) => ctx |> return
    /* FIXME: Hole is just a placeholder. */
    | _ => PatScrutTypesNotEqual(l, scrut_ty_, TList(THole)) |> fail
    }

  | PTriv =>
    switch (scrut_ty_) {
    | TUnit => ctx |> return
    | _ => PatScrutTypesNotEqual(l, scrut_ty_, TUnit) |> fail
    }
  };
};

let rec ana' = (label, actual_ty, expected_ty) =>
  Typ.equal(actual_ty, expected_ty)
    ? () |> return : TypesNotEqual(label, actual_ty, expected_ty) |> fail

and ana_block = (ctx, delta, block: Anf.block, ty) => {
  let* block_ty = syn_block(ctx, delta, block);
  ana'(block.block_label, block_ty, ty);
}

and ana_imm = (ctx, delta, im: Anf.imm, ty) => {
  let* im_ty = syn_imm(ctx, delta, im);
  ana'(im.imm_label, im_ty, ty);
}

and ana_imm_nc = (ctx, delta, im: Anf.imm, ty_) => {
  let* im_ty = syn_imm(ctx, delta, im);
  switch (ty_, Typ.is_nc(im_ty)) {
  | (Some(ty_), Some(im_ty_)) =>
    let+ () = ana_base(im.imm_label, ty_, im_ty_);
    im_ty_;
  | (None, Some(im_ty_)) => im_ty_ |> return
  | (_, None) => TypeNotNecessarilyComplete(im.imm_label, im_ty) |> fail
  };
}

and ana_imm_ni = (ctx, delta, im: Anf.imm, ty_) => {
  let* im_ty = syn_imm(ctx, delta, im);
  switch (ty_, Typ.is_ni(im_ty)) {
  | (Some(ty_), Some(im_ty_)) =>
    let+ () = ana_base(im.imm_label, ty_, im_ty_);
    im_ty_;
  | (None, Some(im_ty_)) => im_ty_ |> return
  | (_, None) => TypeNotNecessarilyIncomplete(im.imm_label, im_ty) |> fail
  };
}

and ana_imm_ii = (ctx, delta, im: Anf.imm, ty_) => {
  let* im_ty = syn_imm(ctx, delta, im);
  switch (ty_, Typ.is_ii(im_ty)) {
  | (Some(ty_), Some(im_ty_)) =>
    let+ () = ana_base(im.imm_label, ty_, im_ty_);
    im_ty_;
  | (None, Some(im_ty_)) => im_ty_ |> return
  | (_, None) =>
    TypeNotIndeterminatelyIncomplete(im.imm_label, im_ty) |> fail
  };
}

and ana_base = (l, ty_1, ty_2) =>
  if (Typ.equal_t_(ty_1, ty_2)) {
    BaseTypesNotEqual(l, ty_1, ty_2) |> fail;
  } else {
    () |> return;
  }

and syn_block =
    (ctx, delta, {block_body: (stmts, im), block_label: l, _}: Anf.block) => {
  let* ctx = syn_stmts(ctx, delta, stmts);
  let* ty = syn_imm(ctx, delta, im);
  extend(l, ty);
}

and syn_stmts = (ctx: TypContext.t, delta, stmts) =>
  switch (stmts) {
  | [] => ctx |> return
  | [stmt, ...stmts] =>
    stmts
    |> List.fold_left(
         (acc, stmt) => {
           let* ctx = acc;
           syn_stmt(ctx, delta, stmt);
         },
         syn_stmt(ctx, delta, stmt),
       )
  }

and syn_stmt = (ctx, delta, {stmt_kind, stmt_label: _, _}: Anf.stmt) => {
  switch (stmt_kind) {
  /* ; Δ ; Γ            ⊢ c : υ{τ}
     ; Δ ; Γ , x : υ{τ} ⊢ b : υ'{τ'}
     → Δ ; Γ            ⊢ let x = c in b : υ'{τ'} */
  | SLet(x, c) =>
    let+ ty = syn_comp(ctx, delta, c);
    let ctx = TypContext.add(x, ty, ctx);
    ctx;

  /* ; Δ ; Γ , f : nc{τ → υ'{τ'}}
         , x : u{τ}
         ⊢ b : υ'{τ'}
     → Δ ; Γ ⊢ let rec f = fun (x : υ{τ}) : υ'{τ'} -> b in b' : */
  | SLetRec(f, (param, param_ty), (o_ty, body)) =>
    let ty = Typ.nc(TArrow(param_ty, o_ty));
    let ctx = TypContext.add(f, ty, ctx);

    let ctx' = TypContext.add(param, param_ty, ctx);
    let+ () = ana_block(ctx', delta, body, o_ty);
    ctx;

  /* ; Δ ; Γ             ⊢ im : υ{τ}
     ; Δ ; Γ , x : nc{τ} ⊢ b₁ : υ'{τ'}
     ; Δ ; Γ , x : ni{τ} ⊢ b₂ : υ'{τ'}
     → Δ ; Γ             ⊢ branch x with | x ⇒ b₁ | x ⇒ b₂ : υ'{τ'} */
  | SSwitch(im, x, br1, br2) =>
    let* ty_ = ana_imm_ii(ctx, delta, im, None);

    let ctx1 = TypContext.add(x, Typ.nc(ty_), ctx);
    let ctx2 = TypContext.add(x, Typ.ni(ty_), ctx);

    let* _ = syn_block(ctx1, delta, br1);
    let+ _ = syn_block(ctx2, delta, br2);
    ctx;
  };
}

and syn_comp = (ctx, delta, {comp_kind, comp_label: l, _}: Anf.comp) => {
  switch (comp_kind) {
  | CImm(im) =>
    let* ty = syn_imm(ctx, delta, im);
    extend(l, ty);

  | CBinNC(op, im1, im2) =>
    syn_comp_bin_op(ctx, delta, (op, im1, im2), l, (ana_imm_nc, Typ.nc))
  | CBinNI(op, im1, im2) =>
    syn_comp_bin_op(ctx, delta, (op, im1, im2), l, (ana_imm_ni, Typ.ni))

  | CFunNC((param, param_ty_), body) =>
    syn_comp_fun(ctx, delta, ((param, param_ty_), body), l, Typ.nc)
  | CFunNI((param, param_ty_), body) =>
    syn_comp_fun(ctx, delta, ((param, param_ty_), body), l, Typ.ni)

  | CApNC(fn, arg) => syn_comp_ap(ctx, delta, (fn, arg), l, ana_imm_nc)
  | CApNI(fn, arg) => syn_comp_ap(ctx, delta, (fn, arg), l, ana_imm_ni)

  | CConsNC(im1, im2) =>
    syn_comp_cons(ctx, delta, (im1, im2), l, (ana_imm_nc, Typ.nc))
  | CConsNI(im1, im2) =>
    syn_comp_cons(ctx, delta, (im1, im2), l, (ana_imm_ni, Typ.ni))

  | CPairNC(im1, im2) =>
    syn_comp_pair(ctx, delta, (im1, im2), l, (ana_imm_nc, Typ.nc))
  | CPairNI(im1, im2) =>
    syn_comp_pair(ctx, delta, (im1, im2), l, (ana_imm_ni, Typ.ni))

  | CInjNC(other_ty_, side, im) =>
    syn_comp_inj(ctx, delta, (other_ty_, side, im), l, (ana_imm_nc, Typ.nc))
  | CInjNI(other_ty_, side, im) =>
    syn_comp_inj(ctx, delta, (other_ty_, side, im), l, (ana_imm_ni, Typ.ni))

  | CCaseNC(scrut, rules) =>
    syn_comp_case(ctx, delta, (scrut, rules), l, (ana_imm_nc, Typ.nc))
  | CCaseNI(scrut, rules) =>
    syn_comp_case(ctx, delta, (scrut, rules), l, (ana_imm_ni, Typ.ni))

  | CEHole(u, _i, sigma) => syn_hole(ctx, delta, l, u, sigma)

  | CNEHoleNC(reason, u, i, sigma, im) =>
    syn_comp_nehole(ctx, delta, (reason, u, i, sigma, im), l, ana_imm_nc)
  | CNEHoleNI(reason, u, i, sigma, im) =>
    syn_comp_nehole(ctx, delta, (reason, u, i, sigma, im), l, ana_imm_ni)

  | CCastNC(im, ty_, ty_') =>
    syn_comp_cast(ctx, delta, (im, ty_, ty_'), l, ana_imm_nc)
  | CCastNI(im, ty_, ty_') =>
    syn_comp_cast(ctx, delta, (im, ty_, ty_'), l, ana_imm_ni)

  /* ; Δ ; Γ ⊢ im          : nc{τ}
     → Δ ; Γ ⊢ ni_wrap(im) : ni{τ} */
  | CNIWrapNC(im) =>
    let* ty_ = ana_imm_nc(ctx, delta, im, None);
    extend(l, Typ.ni(ty_));

  /* ; Δ ; Γ ⊢ im          : υ{τ}
     → Δ ; Γ ⊢ ii_wrap(im) : ii{τ} */
  | CIIWrapNC(im) =>
    let* ty_ = ana_imm_nc(ctx, delta, im, None);
    extend(l, Typ.ii(ty_));
  | CIIWrapNI(im) =>
    let* ty_ = ana_imm_ni(ctx, delta, im, None);
    extend(l, Typ.ii(ty_));
  };
}

/* ; Δ ; Γ ⊢ op         : _{τ₁} → _{τ₂} → _{τ}
   ; Δ ; Γ ⊢ im₁        : υ{τ₁}
   ; Δ ; Γ ⊢ im₂        : υ{τ₂}
   → Δ ; Γ ⊢ im₁ op im₂ : υ{τ} */
and syn_comp_bin_op = (ctx, delta, (op, im1, im2), l, (ana_imm_, wrap_ty_)) => {
  let ana_ty_ = ty__of_bin_op(op);
  let* _ty_1 = ana_imm_(ctx, delta, im1, Some(ana_ty_));
  let* _ty_2 = ana_imm_(ctx, delta, im2, Some(ana_ty_));
  extend(l, wrap_ty_(ana_ty_));
}

/* ; Δ ; Γ , x : υ{τ} ⊢ b : υ'{τ'}
   ; Δ ; Γ            ⊢ fun (x : υ{τ}) -> b : nc{u{τ} → u'{τ'}} */
and syn_comp_fun =
    (ctx, delta, ((param, param_ty_), body), l, wrap_param_ty_) => {
  let param_ty = wrap_param_ty_(param_ty_);
  let ctx' = TypContext.add(param, param_ty, ctx);
  let* body_ty = syn_block(ctx', delta, body);
  extend(l, Typ.nc(TArrow(param_ty, body_ty)));
}

/* ; Δ ; Γ ⊢ arg    : υ₁{τ₁}
   ; Δ ; Γ ⊢ fn     : υ{υ₁{τ} → υ₂{τ₂}}
   → Δ ; Γ ⊢ fn arg : υ₂{τ₂}
   */
and syn_comp_ap = (ctx, delta, (fn, arg), l, ana_imm_) => {
  let* fn_ty_ = ana_imm_(ctx, delta, fn, None);
  switch (fn_ty_) {
  | TArrow(ty1, ty2) =>
    /* Argument type must be equal to arrow input type. */
    let* () = ana_imm(ctx, delta, arg, ty1);
    extend(l, ty2);

  | _ =>
    let* arg_ty = syn_imm(ctx, delta, arg);
    /* FIXME: Hole here is just a placeholder for unknown. */
    BaseTypesNotEqual(fn.imm_label, fn_ty_, TArrow(arg_ty, Typ.nc(THole)))
    |> fail;
  };
}

/* ; Δ ; Γ ⊢ im₁      : υ{τ}
   ; Δ ; Γ ⊢ im₂      : υ{τ list}
   → Δ ; Γ ⊢ im₁::im₂ : υ{τ list} */
and syn_comp_cons = (ctx, delta, (im1, im2), l, (ana_imm_, wrap_ty_)) => {
  let* ty_1 = ana_imm_(ctx, delta, im1, None);
  let* ty_2 = ana_imm_(ctx, delta, im2, Some(ty_1));
  switch (ty_2) {
  | TList(ty_2') when Typ.equal_t_(ty_1, ty_2') =>
    extend(l, wrap_ty_(TList(ty_1)))
  | TList(ty_2') => BaseTypesNotEqual(im1.imm_label, ty_1, ty_2') |> fail
  | ty_2 => BaseTypesNotEqual(im2.imm_label, ty_2, TList(ty_1)) |> fail
  };
}

/* ; Δ ; Γ ⊢ im₁        : υ{τ₁}
   ; Δ ; Γ ⊢ im₂        : υ{τ₂}
   → Δ ; Γ ⊢ (im₁, im₂) : υ{τ₁ * τ₂} */
and syn_comp_pair = (ctx, delta, (im1, im2), l, (ana_imm_, wrap_ty_)) => {
  let* ty_1 = ana_imm_(ctx, delta, im1, None);
  let* ty_2 = ana_imm_(ctx, delta, im2, None);
  extend(l, wrap_ty_(TPair(ty_1, ty_2)));
}

/* ; Δ ; Γ ⊢ im : υ{τ}
   → Δ ; Γ ⊢ inj[L, τ'](im) : υ{τ | τ'} */
/* ; Δ ; Γ ⊢ im : υ{τ}
   → Δ ; Γ ⊢ inj[R, τ'](im) : υ{τ' | τ} */
and syn_comp_inj =
    (ctx, delta, (other_ty_, side, im), l, (ana_imm_, wrap_ty_)) => {
  let* this_ty_ = ana_imm_(ctx, delta, im, None);
  let ty: Typ.t_ =
    switch (side) {
    | CInjL => TSum(this_ty_, other_ty_)
    | CInjR => TSum(other_ty_, this_ty_)
    };
  extend(l, wrap_ty_(ty));
}

/* ; Δ ; Γ ⊢ scrut : υ{τ}
   ; ...
   → Δ ; Γ ⊢ case scrut of rules : υ{τ'} */
and syn_comp_case = (ctx, delta, (scrut, rules), l, (ana_imm_, wrap_ty_)) => {
  let* scrut_ty = ana_imm_(ctx, delta, scrut, None);

  /* ; υ{τ} ▷ p ⇒ Γ'
     ; Δ ; Γ ∪ Γ' ⊢ b : υ'{τ'}
     → Δ ; Γ     ⊢ p ⇒ b : υ'{τ'} */
  let syn_rule =
      (ctx, delta, {rule_pat, rule_branch, rule_label: _, _}: Anf.rule) => {
    let* ctx = ana_pat(ctx, rule_pat, wrap_ty_(scrut_ty));
    syn_block(ctx, delta, rule_branch);
  };

  let* ty =
    switch (rules) {
    | [] => CaseEmptyRules(l) |> fail
    | [rule, ...rules] =>
      rules
      |> List.fold_left(
           (acc, rule: Anf.rule) => {
             let* acc_ty = acc;

             let* branch_ty = syn_rule(ctx, delta, rule);
             Typ.equal(acc_ty, branch_ty)
               ? acc_ty |> return
               : TypesNotEqual(
                   rule.rule_branch.block_label,
                   branch_ty,
                   acc_ty,
                 )
                 |> fail;
           },
           syn_rule(ctx, delta, rule),
         )
    };

  extend(l, ty);
}

and syn_comp_nehole = (ctx, delta, (_reason, u, _i, sigma, im), l, ana_imm_) => {
  let* _ = ana_imm_(ctx, delta, im, None);
  syn_hole(ctx, delta, l, u, sigma);
}

/* ; Δ ; Γ ⊢ im : υ{τ}
   ; τ ~ τ'
   → Δ ; Γ ⊢ im<τ ⇒ τ'> : ii{τ'} */
and syn_comp_cast =
    (ctx, delta, ({imm_label: l', _} as im, ty_, ty_'), l, ana_imm_) => {
  let* im_ty_ = ana_imm_(ctx, delta, im, Some(ty_));
  if (Typ.equal_t_(im_ty_, ty_)) {
    if (Typ.consistent_(ty_, ty_')) {
      extend(l, Typ.ii(ty_'));
    } else {
      BaseTypesInconsistent(l', ty_, ty_') |> fail;
    };
  } else {
    BaseTypesNotEqual(l', im_ty_, ty_) |> fail;
  };
}

and syn_imm = (ctx, _delta, {imm_kind, imm_label: l, _}: Anf.imm) =>
  switch (imm_kind) {
  /* Δ ; Γ , x : υ{τ} ⊢ x : υ{τ} */
  | IVar(x) =>
    switch (TypContext.find_opt(x, ctx)) {
    | Some(ty) => extend(l, ty)
    | None => UnboundVar(l) |> fail
    }

  | IConst(const) =>
    let ty =
      switch (const) {
      /* Δ ; Γ ⊢ true : nc{bool} */
      /* Δ ; Γ ⊢ false : nc{bool} */
      | ConstBool(_) => Typ.nc(TBool)
      /* Δ ; Γ ⊢ n : nc{int} */
      | ConstInt(_) => Typ.nc(TInt)
      /* Δ ; Γ ⊢ f : nc{int} */
      | ConstFloat(_) => Typ.nc(TFloat)
      /* Δ ; Γ ⊢ ([]:τ) : nc{τ list} */
      | ConstNil(ty_) => Typ.nc(TList(ty_))
      /* Δ ; Γ ⊢ triv : nc{unit} */
      | ConstTriv => Typ.nc(TUnit)
      };
    extend(l, ty);
  }

and syn_hole = (ctx, delta, l: ExprLabel.t, u: MetaVar.t, sigma: Sigma.t) =>
  switch (MetaVarMap.find_opt(u, delta)) {
  | Some((sort, hole_ty, gamma')) =>
    switch (sort) {
    | Delta.ExpressionHole =>
      let* () = ana_hole_sigma(ctx, delta, l, sigma, gamma');
      extend(l, hole_ty);
    | Delta.PatternHole => WrongHoleSort(l) |> fail
    }
  | None => UnboundHole(l) |> fail
  }

and ana_hole_sigma =
    (ctx, delta, l: ExprLabel.t, sigma: Sigma.t, gamma': TypContext.t) =>
  gamma'
  |> TypContext.bindings
  |> List.fold_left(
       (acc, (x, gamma_ty)) => {
         let* () = acc;

         switch (Sigma.find_opt(x, sigma)) {
         | None => UnboundVarSigma(l, x) |> fail
         | Some(im) =>
           let* sigma_ty = syn_imm(ctx, delta, im);
           Typ.equal(sigma_ty, gamma_ty)
             ? () |> return
             : TypesNotEqual(im.imm_label, sigma_ty, gamma_ty) |> fail;
         };
       },
       return(),
     );

let syn = (ctx, delta, e) => {
  let (ok, r) = syn_block(ctx, delta, e, SynMonad.init);
  r |> Result.map(_ => ok);
};
