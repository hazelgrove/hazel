open OptUtil.Syntax;

let get_pattern_type = (ctx, UHExp.Rule(p, _)) =>
  p |> Statics_Pat.syn(ctx) |> Option.map(((ty, _)) => ty);

let joined_pattern_type = (ctx, rules) => {
  let* tys = rules |> List.map(get_pattern_type(ctx)) |> OptUtil.sequence;
  HTyp.join_all(LUB, tys);
};

module rec M: Statics_Exp_Sig.S = {
  let tuple_zip =
    Statics_common.tuple_zip(~get_tuple_elements=UHExp.get_tuple_elements);

  let recursive_let_id = (p: UHPat.t, def: UHExp.t): option((Var.t, HTyp.t)) => {
    switch (p, def) {
    | (
        OpSeq(_, S(TypeAnn(_, Var(_, NotInVarHole, x), ann), E)),
        [ExpLine(OpSeq(_, S(Lam(_), E)))],
      ) =>
      let ty_ann = UHTyp.expand(ann);
      let+ _ = HTyp.matched_arrow(ty_ann);
      (x, ty_ann);
    | _ => None
    };
  };

  let extend_let_def_ctx =
      (ctx: Contexts.t'('a), p: UHPat.t, def: UHExp.t): Contexts.t'('a) =>
    switch (recursive_let_id(p, def)) {
    | None => ctx
    | Some((id, ty_ann)) => Contexts.extend_gamma(ctx, (id, ty_ann))
    };

  type livelit_types_type = {
    init_ty: HTyp.t,
    update_ty: HTyp.t,
    view_ty: HTyp.t,
    shape_ty: HTyp.t,
    expand_ty: HTyp.t,
  };

  let ll_defun_update_monad_ty = model_type =>
    /*TODO make type more specific */
    HTyp.Sum(
      HTyp.Sum(UHTyp.expand(model_type), HTyp.Hole),
      HTyp.Sum(
        HTyp.Prod([
          HTyp.String,
          HTyp.Prod([HTyp.String, HTyp.Arrow(HTyp.Hole, HTyp.Hole)]),
        ]),
        HTyp.Hole,
      ),
    );

  let ll_init_ty = (model_type, _) => ll_defun_update_monad_ty(model_type);

  let ll_update_ty = (model_type, action_type) =>
    HTyp.Arrow(
      Prod([UHTyp.expand(model_type), UHTyp.expand(action_type)]),
      ll_defun_update_monad_ty(model_type),
    );
  let ll_defun_view_mond_ty =
    HTyp.Sum(
      HTyp.Prod([String, List(Prod([String, String])), List(Hole)]),
      Hole // TODO(andrew): improve this type
    );
  let ll_view_ty = (model_type, _) =>
    HTyp.Arrow(
      Prod([Int, UHTyp.expand(model_type)]),
      ll_defun_view_mond_ty,
    );
  let ll_shape_ty = (_, _) => HTyp.Prod([Bool, Int]);
  let ll_expand_ty = (model_type, _) =>
    HTyp.Arrow(UHTyp.expand(model_type), String);

  let livelit_types = (llrecord: UHExp.livelit_record): livelit_types_type => {
    open UHExp;
    let {model_type, action_type, _} = llrecord;
    {
      init_ty: ll_init_ty(model_type, action_type),
      update_ty: ll_update_ty(model_type, action_type),
      view_ty: ll_view_ty(model_type, action_type),
      shape_ty: ll_shape_ty(model_type, action_type),
      expand_ty: ll_expand_ty(model_type, action_type),
    };
  };

  module Elaborator = Elaborator_Exp.M(M);
  module Evaluator = Evaluator.M(M);

  let serialize_ll_monad = model =>
    SpliceGenCmd.return(DHExp.sexp_of_t(model));

  let eval = (d: DHExp.t): DHExp.t => {
    switch (Evaluator.evaluate(~eval_livelit_holes=false, d)) {
    | InvalidInput(_) => failwith("Statics_Exp eval InvalidInput")
    | Indet(v)
    | BoxedValue(v) => DHExp.strip_casts(v)
    };
  };

  let rec mk_update_monad =
          (ctx: Contexts.t, delta: Delta.t, model_ty: HTyp.t, d: DHExp.t)
          : option(SpliceGenCmd.t(SerializedModel.t)) => {
    /* at this point, we have already evaluated init or update, resulting
          in a dhexp d which is a value of the following type:

       Since algebraic datatype support in Hazel is as yet incomplete,
       the Update and View monads have been assigned the following
       encoding in terms of anonymous pairs and sums:

       UpdateMonad:

       return(x) = inj[L](inj[L](x))
       bindNewSplice((ty, exp), f) = inj[R](inj[L](ty, (exp, f)))
       bindSetSplice((spliceno, ty, exp), f) = inj[R](inj[R]((spliceno, (ty, (exp, f)))))

       ViewMonad:

       return(x) = inj[L](x)
       bindEvalSplice(spliceno, f) = inj[R](spliceno, f)

       Example:

        Spliceslider init (ADT):
        Bind(,
          NewSplice("HTyp.Int", "UHExp.([ExpLine(OpSeq(Placeholder(0), S(IntLit(NotInHole, 100), E)))])")
          fun (spliceno) { Return((spliceno, 50))} )

        Spliceslider update (ADT):
        fun ((spliceno, _), action) { Return((spliceno, action)) }

        Spliceslider init (anon):

        inj[R](inj[L](
          "HTyp.Int",
            ("UHExp.([ExpLine(OpSeq(Placeholder(0), S(IntLit(NotInHole, 100), E)))])",
             fun (spliceno) { inj[L](inj[L]((spliceno, 50))) })))

       sexped str: "((ExpLine(OpSeq(Placeholder 0) (S(IntLit NotInHole 100) E))))"

        Spliceslider update (anon):
        fun ((spliceno, _), action) { inj[L](inj[L]((spliceno, action))) }

        *: uhexps would need to be sexped

       */
    let run' = mk_update_monad(ctx, delta, model_ty);
    let serialize = d => {
      d |> DHExp.strip_casts |> DHExp.sexp_of_t |> SpliceGenCmd.return;
    };
    switch (d) {
    | Inj(_, L, Inj(_, L, d0)) =>
      let* d_ty = Statics_DHExp.syn(ctx, delta, d);
      let updatemonadmodel_ty =
        HTyp.Sum(HTyp.Sum(model_ty, HTyp.Hole), HTyp.Hole);
      HTyp.consistent(d_ty, updatemonadmodel_ty)
        ? Some(serialize(d0)) : None;
    | Inj(
        _,
        R,
        Inj(_, L, Pair(StringLit(s_ty), Pair(StringLit(s_exp), f))),
      ) =>
      let* ty =
        try(Some(s_ty |> Sexplib.Sexp.of_string |> HTyp.t_of_sexp)) {
        | _ => None
        };
      // TODO: handle none case for option(Exp).. but how?
      let* exp =
        try(Some(s_exp |> Sexplib.Sexp.of_string |> UHExp.t_of_sexp)) {
        | _ => None
        };
      let a =
        SpliceGenCmd.new_splice(~init_uhexp_gen=u_gen => (exp, u_gen), ty);
      Some(
        SpliceGenCmd.bind(a, spliceno => {
          switch (DHExp.Ap(f, DHExp.IntLit(spliceno)) |> eval |> run') {
          | None => failwith("mk_update_monad bindNewSplice case")
          | Some(d) => d
          }
        }),
      );
    | Inj(
        _,
        R,
        Inj(
          _,
          R,
          Pair(
            IntLit(spliceno),
            Pair(StringLit(s_ty), Pair(StringLit(s_exp), f)),
          ),
        ),
      ) =>
      // TODO: not sure logic on this case is right
      let ty = s_ty |> Sexplib.Sexp.of_string |> HTyp.t_of_sexp;
      // TODO: handle none case for option(Exp).. but how?
      let exp = s_exp |> Sexplib.Sexp.of_string |> UHExp.t_of_sexp;
      let a =
        SpliceGenCmd.map_splice(spliceno, (_, u_gen) => ((ty, exp), u_gen));
      Some(
        SpliceGenCmd.bind(a, _ => {
          switch (DHExp.Ap(f, DHExp.Triv) |> eval |> run') {
          | None => failwith("mk_update_monad bindSetSplice case")
          | Some(d) => d
          }
        }),
      );

    | _ =>
      print_endline("ERROR: run_init_monad: wrong data type. got:");
      print_endline(Sexplib.Sexp.to_string_hum(DHExp.sexp_of_t(d)));
      None;
    };
  };

  let mk_ll_init =
      (init: UHExp.t, model_ty: HTyp.t)
      : option(SpliceGenCmd.t(SerializedModel.t)) => {
    let elab_ctx = Contexts.empty;
    let elab_delta = Delta.empty;
    switch (Elaborator.syn_elab(elab_ctx, elab_delta, init)) {
    | DoesNotElaborate =>
      print_endline("ERROR: mk_ll_init DoesNotElaborate");
      None;
    | Elaborates(d, _, delta) =>
      switch (d |> eval |> mk_update_monad(elab_ctx, delta, model_ty)) {
      | None =>
        print_endline("ERROR: mk_ll_init mk failed");
        None;
      | Some(sgc) => Some(sgc)
      }
    };
  };

  let mk_ll_update =
      (
        update: UHExp.t,
        model_ty: HTyp.t,
        action: SerializedAction.t,
        model: SerializedModel.t,
      )
      : SpliceGenCmd.t(SerializedModel.t) => {
    let elab_ctx = Contexts.empty;
    let elab_delta = Delta.empty;
    switch (Elaborator.syn_elab(elab_ctx, elab_delta, update)) {
    | DoesNotElaborate => failwith("ERROR: mk_ll_update DoesNotElaborate")
    | Elaborates(update_dhexp, _, delta) =>
      let action_dhexp = DHExp.t_of_sexp(action);
      let model_dhexp = DHExp.t_of_sexp(model);
      let ap = DHExp.Ap(update_dhexp, DHExp.Pair(action_dhexp, model_dhexp));
      switch (ap |> eval |> mk_update_monad(elab_ctx, delta, model_ty)) {
      | None => failwith("ERROR: mk_ll_update mk failed")
      | Some(sgc) => sgc
      };
    };
  };

  let _mk_ll_view_new =
      (view: UHExp.t, llu: int, model: SerializedModel.t): option(DHExp.t) => {
    let model_dhexp = DHExp.t_of_sexp(model);
    let llu_dhexp = DHExp.IntLit(llu);
    switch (Elaborator.syn_elab(Contexts.empty, Delta.empty, view)) {
    | DoesNotElaborate => failwith("mk_ll_view elab DoesNotElaborate")
    | Elaborates(view_dhexp, _, _) =>
      let ap = DHExp.Ap(view_dhexp, DHExp.Pair(llu_dhexp, model_dhexp));
      switch (Evaluator.evaluate(~eval_livelit_holes=false, ap)) {
      | BoxedValue(v) => Some(DHExp.strip_casts(v))
      | _ => None
      };
    };
  };

  let _mk_ll_init_old = (init: UHExp.t, _): SpliceGenCmd.t(SerializedModel.t) => {
    let elab_ctx = Contexts.empty;
    let dh_init = Elaborator.syn_elab(elab_ctx, Delta.empty, init);
    switch (dh_init) {
    | DoesNotElaborate => failwith("mk_ll_init DoesNotElaborate")
    | Elaborates(dh_init, _, _) => serialize_ll_monad(dh_init)
    };
  };

  let _mk_ll_update_old =
      (update: UHExp.t, action: SerializedAction.t, model: SerializedModel.t) => {
    let action_dhexp = DHExp.t_of_sexp(action);
    let model_dhexp = DHExp.t_of_sexp(model);
    let elab_ctx = Contexts.empty;
    let update_dhexp =
      switch (Elaborator.syn_elab(elab_ctx, Delta.empty, update)) {
      | DoesNotElaborate => failwith("mk_ll_update DoesNotElaborate")
      | Elaborates(update, _, _) => update
      };
    let term = DHExp.Ap(update_dhexp, DHExp.Pair(action_dhexp, model_dhexp));
    switch (Evaluator.evaluate(~eval_livelit_holes=false, term)) {
    | InvalidInput(_) => failwith("mk_ll_update eval InvalidInput")
    | Indet(new_model)
    | BoxedValue(new_model) => serialize_ll_monad(new_model)
    };
  };

  let _mk_ll_view =
      (view: UHExp.t, llu: int, model: SerializedModel.t): option(string) => {
    let model_dhexp = DHExp.t_of_sexp(model);
    let llu_dhexp = DHExp.IntLit(llu);
    switch (Elaborator.syn_elab(Contexts.empty, Delta.empty, view)) {
    | DoesNotElaborate => failwith("mk_ll_view elab DoesNotElaborate")
    | Elaborates(view_dhexp, _, _) =>
      let term = DHExp.Ap(view_dhexp, DHExp.Pair(llu_dhexp, model_dhexp));
      switch (Evaluator.evaluate(~eval_livelit_holes=false, term)) {
      | BoxedValue(v) =>
        switch (DHExp.strip_casts(v)) {
        | StringLit(str) => Some(str)
        | _ => None
        }
      | _ => None
      };
    };
  };

  let mk_ll_shape = (shape: UHExp.t): LivelitShape.t => {
    switch (Elaborator.syn_elab(Contexts.empty, Delta.empty, shape)) {
    | Elaborates(Pair(BoolLit(flag), IntLit(num)), _, _) =>
      if (flag) {
        Inline(num);
      } else {
        MultiLine(num);
      }
    | _ => InvalidShape
    };
  };

  let mk_ll_expand =
      (expand: UHExp.t, model: SerializedModel.t)
      : LivelitDefinition.livelit_expand_result => {
    let model_dhexp = DHExp.t_of_sexp(model);
    let elab_ctx = Contexts.empty;
    let expand_dhexp =
      switch (Elaborator.syn_elab(elab_ctx, Delta.empty, expand)) {
      | DoesNotElaborate => failwith("mk_ll_expand elab")
      | Elaborates(expand, _, _) => expand
      };
    let term = DHExp.Ap(expand_dhexp, model_dhexp);
    switch (Evaluator.evaluate(~eval_livelit_holes=false, term)) {
    | BoxedValue(v) =>
      switch (DHExp.strip_casts(v)) {
      | StringLit(str) =>
        let maybeSexp =
          try(Some(Sexplib.Sexp.of_string(str))) {
          | _ => None
          };
        switch (maybeSexp) {
        | None => Failure(NotSexp)
        | Some(sexp) =>
          try(Success(UHExp.Block.wrap(UHExp.operand_of_sexp(sexp)))) {
          | _ => Failure(NotUHExp)
          }
        };
      | _ => Failure(NotStringlit)
      }
    | InvalidInput(_)
    | Indet(_) => Failure(NotStringlit)
    };
  };

  /**
 * Synthesize a type, if possible, for e
 */
  let rec syn = (ctx: Contexts.t, e: UHExp.t): option(HTyp.t) =>
    syn_block(ctx, e)
  and extend_let_body_ctx =
      (ctx: Contexts.t, p: UHPat.t, def: UHExp.t): Contexts.t => {
    /* precondition: (p)attern and (def)inition have consistent types */
    def
    |> syn(extend_let_def_ctx(ctx, p, def))
    |> OptUtil.get(_ => failwith("extend_let_body_ctx: impossible syn"))
    |> Statics_Pat.ana(ctx, p)
    |> OptUtil.get(_ => failwith("extend_let_body_ctx: impossible ana"));
  }
  and extend_livelit_ctx = (ctx: Contexts.t, llrecord) => {
    let (gamma, livelit_ctx) = ctx;
    let {
      name: (_, name_str),
      captures,
      init,
      update,
      view,
      shape,
      expand,
      expansion_type,
      model_type,
      action_type,
      _,
    }: UHExp.livelit_record = llrecord;
    let captures_ty =
      switch (syn(ctx, captures)) {
      | Some(ty) => ty
      | None => HTyp.Hole
      };

    let init_model = mk_ll_init(init, UHTyp.expand(model_type));
    let init_valid =
      switch (init_model) {
      | None => false
      | _ => true
      };

    let ll_def_valid =
      UHExp.is_complete(init)
      && UHExp.is_complete(update)
      && UHExp.is_complete(view)
      && UHExp.is_complete(shape)
      && UHExp.is_complete(expand)
      && init_valid;

    /* NOTE(andrew): Extend the livelit context only if all
     * fields typecheck; otherwise we have a litany of possible
     * failure cases to contend with at the ap site, most of which
     * are already indicated by type holes in the definition. An
     * alternate approach to consider is adding an error field to
     * livelitdefs and dealing with these heterogenously at
     * the ap site. */

    if (ll_def_valid) {
      let init_model = OptUtil.get(() => failwith("No"), init_model);
      (
        gamma,
        LivelitCtx.extend(
          livelit_ctx,
          (
            name_str,
            (
              {
                name: name_str,
                action_ty: UHTyp.expand(action_type),
                model_ty: UHTyp.expand(model_type),
                captures_ty,
                expansion_ty: UHTyp.expand(expansion_type),
                param_tys: [], // TODO: params
                init_model,
                update: mk_ll_update(update, UHTyp.expand(model_type)),
                expand: mk_ll_expand(expand),
              }: LivelitDefinition.t,
              [],
            ),
          ) // TODO: params
        ),
      );
    } else {
      (gamma, livelit_ctx);
    };
  }
  and syn_block = (ctx: Contexts.t, block: UHExp.block): option(HTyp.t) => {
    let* (leading, conclusion) = UHExp.Block.split_conclusion(block);
    let* ctx = syn_lines(ctx, leading);
    syn_opseq(ctx, conclusion);
  }
  and syn_lines =
      (ctx: Contexts.t, lines: list(UHExp.line)): option(Contexts.t) => {
    lines
    |> List.fold_left(
         (opt_ctx: option(Contexts.t), line: UHExp.line) => {
           let* ctx = opt_ctx;
           syn_line(ctx, line);
         },
         Some(ctx),
       );
  }
  and syn_livelit_abbreviation =
      (ctx: Contexts.t, err: AbbrevErrStatus.t, llp: LLPat.t, def: UHExp.t)
      : option(Contexts.t) => {
    let skip_with_reason = reason =>
      switch (err) {
      | InAbbrevHole(reason', _) when reason' == reason => Some(ctx)
      | _ => None
      };
    switch (LLExp.of_uhexp(def)) {
    | None => skip_with_reason(NotLivelitExp)
    | Some({hd, args, meta: _}) =>
      let (gamma, livelit_ctx) = ctx;
      switch (LivelitCtx.lookup(livelit_ctx, hd)) {
      | None => skip_with_reason(Free)
      | Some((old_def, applied_param_tys)) =>
        let unapplied_param_tys =
          ListUtil.drop(List.length(applied_param_tys), old_def.param_tys);
        let (arg_tys, (extra_args, _)) =
          ListUtil.zip_tails(args, unapplied_param_tys);
        let* _ =
          arg_tys
          |> List.map(((arg, (_, ty))) => ana_operand(ctx, arg, ty))
          |> OptUtil.sequence;
        let* _ =
          extra_args |> List.map(syn_operand(ctx)) |> OptUtil.sequence;
        switch (extra_args) {
        | [_, ..._] => skip_with_reason(ExtraneousArgs)
        | [] =>
          let livelit_ctx =
            LivelitCtx.extend(
              livelit_ctx,
              (
                llp,
                (old_def, applied_param_tys @ snd(List.split(arg_tys))),
              ),
            );
          Some((gamma, livelit_ctx));
        };
      };
    };
  }
  and syn_line = (ctx: Contexts.t, line: UHExp.line): option(Contexts.t) =>
    switch (line) {
    | ExpLine(opseq) =>
      let+ _ = syn_opseq(ctx, opseq);
      ctx;
    | EmptyLine
    | CommentLine(_) => Some(ctx)
    | LetLine(err, p, def) =>
      switch (LLPat.of_uhpat(p)) {
      | None =>
        let def_ctx = extend_let_def_ctx(ctx, p, def);
        let* ty_def = syn(def_ctx, def);
        Statics_Pat.ana(ctx, p, ty_def);
      | Some(llp) => syn_livelit_abbreviation(ctx, err, llp, def)
      }
    | LivelitDefLine({init, update, view, shape, expand, _} as llrecord) =>
      let {init_ty, update_ty, view_ty, shape_ty, expand_ty} =
        livelit_types(llrecord);
      OptUtil.sequence([
        ana(ctx, init, init_ty),
        ana(ctx, update, update_ty),
        ana(ctx, view, view_ty),
        ana(ctx, shape, shape_ty),
        ana(ctx, expand, expand_ty),
      ])
      |> Option.map(_ => extend_livelit_ctx(ctx, llrecord));
    }
  and syn_opseq =
      (ctx: Contexts.t, OpSeq(skel, seq): UHExp.opseq): option(HTyp.t) =>
    syn_skel(ctx, skel, seq)
  and syn_skel =
      (ctx: Contexts.t, skel: UHExp.skel, seq: UHExp.seq): option(HTyp.t) =>
    switch (skel) {
    | Placeholder(n) =>
      let en = Seq.nth_operand(n, seq);
      syn_operand(ctx, en);
    | BinOp(InHole(_), op, skel1, skel2) =>
      let skel_not_in_hole = Skel.BinOp(NotInHole, op, skel1, skel2);
      let+ _ = syn_skel(ctx, skel_not_in_hole, seq);
      HTyp.Hole;
    | BinOp(NotInHole, Minus | Plus | Times | Divide, skel1, skel2) =>
      let+ _ = ana_skel(ctx, skel1, seq, HTyp.Int)
      and+ _ = ana_skel(ctx, skel2, seq, Int);
      HTyp.Int;
    | BinOp(NotInHole, FMinus | FPlus | FTimes | FDivide, skel1, skel2) =>
      let+ _ = ana_skel(ctx, skel1, seq, Float)
      and+ _ = ana_skel(ctx, skel2, seq, Float);
      HTyp.Float;
    | BinOp(NotInHole, And | Or, skel1, skel2) =>
      let+ _ = ana_skel(ctx, skel1, seq, Bool)
      and+ _ = ana_skel(ctx, skel2, seq, Bool);
      HTyp.Bool;
    | BinOp(NotInHole, Caret, skel1, skel2) =>
      let+ () = ana_skel(ctx, skel1, seq, HTyp.String)
      and+ () = ana_skel(ctx, skel2, seq, String);
      HTyp.String;
    | BinOp(NotInHole, LessThan | GreaterThan | Equals, skel1, skel2) =>
      let+ _ = ana_skel(ctx, skel1, seq, Int)
      and+ _ = ana_skel(ctx, skel2, seq, Int);
      HTyp.Bool;
    | BinOp(NotInHole, FLessThan | FGreaterThan | FEquals, skel1, skel2) =>
      let+ _ = ana_skel(ctx, skel1, seq, Float)
      and+ _ = ana_skel(ctx, skel2, seq, Float);
      HTyp.Bool;
    | BinOp(NotInHole, Space, skel1, skel2) =>
      let livelit_ap_check = LivelitUtil.check_livelit(ctx, seq, skel);
      switch (livelit_ap_check) {
      | Some((
          ApLivelitData(_, _, _, model, splice_info),
          livelit_defn,
          closed_tys,
          reqd_param_tys,
          args,
        )) =>
        let (_, reqd_param_tys') = List.split(reqd_param_tys);
        let all_args_ana =
          List.for_all2(
            (ty_n, skel_n) =>
              Option.is_some(ana_skel(ctx, skel_n, seq, ty_n)),
            reqd_param_tys',
            args,
          );
        if (all_args_ana) {
          syn_ApLivelit(
            ctx,
            livelit_defn,
            model,
            splice_info,
            closed_tys @ reqd_param_tys,
          );
        } else {
          None;
        };
      | _ =>
        let* ty1 = syn_skel(ctx, skel1, seq);
        let* (ty2, ty) = HTyp.matched_arrow(ty1);
        let+ _ = ana_skel(ctx, skel2, seq, ty2);
        ty;
      };
    | BinOp(NotInHole, Comma, _, _) =>
      skel
      |> UHExp.get_tuple_elements
      |> List.map(skel => syn_skel(ctx, skel, seq))
      |> OptUtil.sequence
      |> Option.map(tys => HTyp.Prod(tys))
    | BinOp(NotInHole, Cons, skel1, skel2) =>
      let* ty1 = syn_skel(ctx, skel1, seq);
      let ty = HTyp.List(ty1);
      let+ _ = ana_skel(ctx, skel2, seq, ty);
      ty;
    }
  and syn_operand = (ctx: Contexts.t, operand: UHExp.operand): option(HTyp.t) =>
    switch (operand) {
    /* in hole */
    | EmptyHole(_) => Some(Hole)
    | InvalidText(_) => Some(Hole)
    | Var(InHole(TypeInconsistent(_), _), _, _)
    | IntLit(InHole(TypeInconsistent(_), _), _)
    | FloatLit(InHole(TypeInconsistent(_), _), _)
    | BoolLit(InHole(TypeInconsistent(_), _), _)
    | StringLit(InHole(TypeInconsistent(_), _), _)
    | ListNil(InHole(TypeInconsistent(_), _))
    | Lam(InHole(TypeInconsistent(_), _), _, _)
    | Inj(InHole(TypeInconsistent(_), _), _, _)
    | Case(StandardErrStatus(InHole(TypeInconsistent(_), _)), _, _)
    | Subscript(InHole(TypeInconsistent(_), _), _, _, _)
    | ApLivelit(_, InHole(TypeInconsistent(None), _), _, _, _, _) =>
      let operand' = UHExp.set_err_status_operand(NotInHole, operand);
      syn_operand(ctx, operand') |> Option.map(_ => HTyp.Hole);
    | Var(InHole(WrongLength, _), _, _)
    | IntLit(InHole(WrongLength, _), _)
    | FloatLit(InHole(WrongLength, _), _)
    | BoolLit(InHole(WrongLength, _), _)
    | StringLit(InHole(WrongLength, _), _)
    | ListNil(InHole(WrongLength, _))
    | Lam(InHole(WrongLength, _), _, _)
    | Inj(InHole(WrongLength, _), _, _)
    | ApLivelit(_, InHole(WrongLength, _), _, _, _, _)
    | Subscript(InHole(WrongLength, _), _, _, _)
    | Case(StandardErrStatus(InHole(WrongLength, _)), _, _) => None
    | Case(InconsistentBranches(rule_types, _), scrut, rules) =>
      switch (syn(ctx, scrut)) {
      | None => None
      | Some(pat_ty) =>
        /* Make sure the rule synthesizes the type the rule_types says it does */
        let correct_rule_types =
          List.for_all2(
            (rule_ty, rule) => {
              switch (syn_rule(ctx, rule, pat_ty)) {
              | None => false
              | Some(syn_ty) => HTyp.eq(rule_ty, syn_ty)
              }
            },
            rule_types,
            rules,
          );
        if (correct_rule_types) {
          Some(HTyp.Hole);
        } else {
          None;
        };
      }
    /* not in hole */
    | Var(NotInHole, NotInVarHole, x) =>
      VarMap.lookup(Contexts.gamma(ctx), x)
    | Var(NotInHole, InVarHole(_), _) => Some(Hole)
    | IntLit(NotInHole, _) => Some(Int)
    | FloatLit(NotInHole, _) => Some(Float)
    | BoolLit(NotInHole, _) => Some(Bool)
    | StringLit(NotInHole, _) => Some(String)
    | ListNil(NotInHole) => Some(List(Hole))
    | Lam(NotInHole, p, body) =>
      let* (ty_p, body_ctx) = Statics_Pat.syn(ctx, p);
      let+ ty_body = syn(body_ctx, body);
      HTyp.Arrow(ty_p, ty_body);
    | Inj(NotInHole, side, body) =>
      let+ ty = syn(ctx, body);
      switch (side) {
      | L => HTyp.Sum(ty, Hole)
      | R => Sum(Hole, ty)
      };
    | Case(StandardErrStatus(NotInHole), scrut, rules) =>
      let* clause_ty = syn(ctx, scrut);
      syn_rules(ctx, rules, clause_ty);
    | Subscript(NotInHole, target, start_, end_) =>
      switch (syn(ctx, target)) {
      | None => None
      | Some(_) =>
        switch (ana(ctx, start_, Int), ana(ctx, end_, Int)) {
        | (Some(_), Some(_)) => Some(String)
        | (_, _) => None
        }
      }

    | ApLivelit(
        _,
        (NotInHole | InHole(TypeInconsistent(Some(_)), _)) as err_status,
        _,
        name,
        serialized_model,
        si,
      ) =>
      let livelit_ctx = Contexts.livelit_ctx(ctx);
      switch (LivelitCtx.lookup(livelit_ctx, name)) {
      | None => None
      | Some((livelit_defn, closed_tys)) =>
        let all_param_tys = livelit_defn.param_tys;
        let reqd_param_tys =
          all_param_tys |> ListUtil.drop(List.length(closed_tys));
        switch (err_status, reqd_param_tys) {
        | (InHole(TypeInconsistent(Some(IllTypedExpansion)), _), []) =>
          actual_livelit_expansion_type(ctx, name, serialized_model, si)
        | (InHole(TypeInconsistent(Some(DecodingError)), _), []) =>
          Some(Hole)
        | (NotInHole, [_, ..._])
        | (InHole(TypeInconsistent(Some(InsufficientParams)), _), []) =>
          None
        | _ =>
          let rslt =
            syn_ApLivelit(
              ctx,
              livelit_defn,
              serialized_model,
              si,
              all_param_tys,
            );
          err_status == NotInHole ? rslt : rslt |> Option.map(_ => HTyp.Hole);
        };
      };
    | FreeLivelit(_, _) => Some(Hole)

    | Parenthesized(body) => syn(ctx, body)
    }
  and syn_rules =
      (ctx: Contexts.t, rules: UHExp.rules, pat_ty: HTyp.t): option(HTyp.t) => {
    let* clause_types =
      List.fold_left(
        (types_opt, r) => {
          let* types = types_opt;
          let+ r_ty = syn_rule(ctx, r, pat_ty);
          [r_ty, ...types];
        },
        Some([]),
        rules,
      );
    HTyp.join_all(GLB, clause_types);
  }
  and syn_rule =
      (ctx: Contexts.t, rule: UHExp.rule, pat_ty: HTyp.t): option(HTyp.t) => {
    let Rule(p, clause) = rule;
    let* ctx = Statics_Pat.ana(ctx, p, pat_ty);
    syn(ctx, clause);
  }
  and syn_ApLivelit =
      (ctx, livelit_defn, serialized_model, splice_info, all_param_tys)
      : option(HTyp.t) => {
    let* _guard =
      // check model type
      switch (livelit_defn.model_ty) {
      | HTyp.Hole =>
        // builtin livelit case
        Some()
      | _ =>
        // user-defined livelit case
        let* model =
          try(Some(DHExp.t_of_sexp(serialized_model))) {
          | _ => None
          };
        let* model_ty = Statics_DHExp.syn(ctx, Delta.empty, model); //TODO: attach real Delta?
        HTyp.consistent(model_ty, livelit_defn.model_ty) ? Some() : None;
      };
    let* splice_ctx =
      ana_splice_map_and_params(ctx, splice_info.splice_map, all_param_tys);
    let {expansion_ty, captures_ty, _}: LivelitDefinition.t = livelit_defn;
    switch (livelit_defn.expand(serialized_model)) {
    | Failure(_) => None
    | Success(expansion) =>
      let expansion_ap_ty = HTyp.Arrow(captures_ty, expansion_ty);
      let* _guard = ana(splice_ctx, expansion, expansion_ap_ty);
      Some(expansion_ty);
    };
  }
  and ana_splice_map_and_params =
      (
        ctx: Contexts.t,
        splice_map: UHExp.splice_map,
        param_tys: list((Var.t, HTyp.t)),
      )
      : option(Contexts.t) => {
    let params_ctx =
      param_tys
      |> List.fold_left(
           (acc, (name, ty)) => Contexts.extend_gamma(acc, (name, ty)),
           Contexts.empty,
         );
    IntMap.fold(
      (splice_name, (ty, e), c) => {
        let+ splice_ctx = c
        and+ _ = ana(ctx, e, ty);
        let splice_var = SpliceInfo.var_of_splice_name(splice_name);
        Contexts.extend_gamma(splice_ctx, (splice_var, ty));
      },
      splice_map,
      Some(params_ctx),
    );
  }
  and declared_livelit_expansion_type =
      (ctx: Contexts.t, name: LivelitName.t): option(HTyp.t) => {
    let livelit_ctx = Contexts.livelit_ctx(ctx);
    let* ({expansion_ty, captures_ty, _}, _) =
      LivelitCtx.lookup(livelit_ctx, name);
    Some(HTyp.Arrow(captures_ty, expansion_ty));
  }
  and actual_livelit_expansion_type =
      (
        ctx: Contexts.t,
        name: LivelitName.t,
        model: SerializedModel.t,
        splice_info: UHExp.splice_info,
      )
      : option(HTyp.t) => {
    let livelit_ctx = Contexts.livelit_ctx(ctx);
    let* (livelit_defn, _) = LivelitCtx.lookup(livelit_ctx, name);
    let* splice_ctx =
      ana_splice_map_and_params(
        ctx,
        splice_info.splice_map,
        livelit_defn.param_tys,
      );
    switch (livelit_defn.expand(model)) {
    | Failure(_) => None
    | Success(expansion) => syn(splice_ctx, expansion)
    };
  }
  and ana = (ctx: Contexts.t, e: UHExp.t, ty: HTyp.t): option(unit) =>
    ana_block(ctx, e, ty)
  and ana_block =
      (ctx: Contexts.t, block: UHExp.block, ty: HTyp.t): option(unit) => {
    let* (leading, conclusion) = UHExp.Block.split_conclusion(block);
    let* ctx = syn_lines(ctx, leading);
    ana_opseq(ctx, conclusion, ty);
  }
  and ana_opseq =
      (ctx: Contexts.t, OpSeq(skel, seq) as opseq: UHExp.opseq, ty: HTyp.t)
      : option(unit) =>
    switch (tuple_zip(skel, ty)) {
    | None =>
      switch (UHExp.get_err_status_opseq(opseq), HTyp.get_prod_elements(ty)) {
      | (InHole(TypeInconsistent(_), _), [_])
      | (InHole(WrongLength, _), _) =>
        let opseq' = UHExp.set_err_status_opseq(NotInHole, opseq);
        let+ _ = syn_opseq(ctx, opseq');
        ();
      | _ => None
      }
    | Some(skel_tys) =>
      let+ _ =
        skel_tys
        |> List.map(((skel, ty)) => ana_skel(ctx, skel, seq, ty))
        |> OptUtil.sequence;
      ();
    }
  and ana_skel =
      (ctx: Contexts.t, skel: UHExp.skel, seq: UHExp.seq, ty: HTyp.t)
      : option(unit) =>
    switch (skel) {
    | BinOp(_, Comma, _, _)
    | BinOp(InHole(WrongLength, _), _, _, _) =>
      failwith(__LOC__ ++ ": tuples handled at opseq level")
    | Placeholder(n) =>
      let en = Seq.nth_operand(n, seq);
      ana_operand(ctx, en, ty);
    | BinOp(NotInHole, Cons, skel1, skel2) =>
      let* ty_elt = HTyp.matched_list(ty);
      let* _ = ana_skel(ctx, skel1, seq, ty_elt);
      ana_skel(ctx, skel2, seq, List(ty_elt));
    | BinOp(InHole(TypeInconsistent(_), _), _, _, _)
    | BinOp(
        NotInHole,
        And | Or | Minus | Plus | Times | Divide | FMinus | FPlus | FTimes |
        FDivide |
        LessThan |
        GreaterThan |
        Equals |
        FLessThan |
        FGreaterThan |
        FEquals |
        Space |
        Caret,
        _,
        _,
      ) =>
      let* ty' = syn_skel(ctx, skel, seq);
      HTyp.consistent(ty, ty') ? Some() : None;
    }
  and ana_operand =
      (ctx: Contexts.t, operand: UHExp.operand, ty: HTyp.t): option(unit) =>
    switch (operand) {
    /* in hole */
    | EmptyHole(_) => Some()
    | InvalidText(_) => Some()
    | Var(InHole(TypeInconsistent(_), _), _, _)
    | IntLit(InHole(TypeInconsistent(_), _), _)
    | FloatLit(InHole(TypeInconsistent(_), _), _)
    | BoolLit(InHole(TypeInconsistent(_), _), _)
    | StringLit(InHole(TypeInconsistent(_), _), _)
    | ListNil(InHole(TypeInconsistent(_), _))
    | Lam(InHole(TypeInconsistent(_), _), _, _)
    | Inj(InHole(TypeInconsistent(_), _), _, _)
    | Case(StandardErrStatus(InHole(TypeInconsistent(_), _)), _, _)
    | Subscript(InHole(TypeInconsistent(_), _), _, _, _)
    | ApLivelit(_, InHole(TypeInconsistent(None), _), _, _, _, _) =>
      let operand' = UHExp.set_err_status_operand(NotInHole, operand);
      let+ _ = syn_operand(ctx, operand');
      (); /* this is a consequence of subsumption and hole universality */
    | Var(InHole(WrongLength, _), _, _)
    | IntLit(InHole(WrongLength, _), _)
    | FloatLit(InHole(WrongLength, _), _)
    | BoolLit(InHole(WrongLength, _), _)
    | StringLit(InHole(WrongLength, _), _)
    | ListNil(InHole(WrongLength, _))
    | Lam(InHole(WrongLength, _), _, _)
    | Inj(InHole(WrongLength, _), _, _)
    | Case(StandardErrStatus(InHole(WrongLength, _)), _, _)
    | Subscript(InHole(WrongLength, _), _, _, _)
    | ApLivelit(_, InHole(WrongLength, _), _, _, _, _) =>
      ty |> HTyp.get_prod_elements |> List.length > 1 ? Some() : None
    | Case(InconsistentBranches(_, _), _, _) => None
    /* not in hole */
    | ListNil(NotInHole) =>
      let+ _ = HTyp.matched_list(ty);
      ();
    | Var(NotInHole, _, _)
    | IntLit(NotInHole, _)
    | FloatLit(NotInHole, _)
    | BoolLit(NotInHole, _)
    | StringLit(NotInHole, _) =>
      let operand' = UHExp.set_err_status_operand(NotInHole, operand);
      let* ty' = syn_operand(ctx, operand');
      HTyp.consistent(ty, ty') ? Some() : None;
    | Lam(NotInHole, p, body) =>
      let* (ty_p_given, ty_body) = HTyp.matched_arrow(ty);
      let* ctx_body = Statics_Pat.ana(ctx, p, ty_p_given);
      ana(ctx_body, body, ty_body);
    | Inj(NotInHole, side, body) =>
      let* (ty1, ty2) = HTyp.matched_sum(ty);
      ana(ctx, body, InjSide.pick(side, ty1, ty2));
    | Case(StandardErrStatus(NotInHole), scrut, rules) =>
      let* ty1 = syn(ctx, scrut);
      ana_rules(ctx, rules, ty1, ty);
    | ApLivelit(_, NotInHole, _, _, _, _)
    | FreeLivelit(_) =>
      let operand' = UHExp.set_err_status_operand(NotInHole, operand);
      ana_subsume_operand(ctx, operand', ty);
    | ApLivelit(_, InHole(TypeInconsistent(Some(_)), _), _, _, _, _) =>
      ana_subsume_operand(ctx, operand, ty)
    | Parenthesized(body) => ana(ctx, body, ty)
    | Subscript(NotInHole, _, _, _) =>
      switch (syn_operand(ctx, operand)) {
      | None => None
      | Some(_) => Some()
      }
    }
  and ana_subsume_operand = (ctx, operand, ty) => {
    let* ty' = syn_operand(ctx, operand);
    HTyp.consistent(ty, ty') ? Some() : None;
  }
  and ana_rules =
      (ctx: Contexts.t, rules: UHExp.rules, pat_ty: HTyp.t, clause_ty: HTyp.t)
      : option(unit) =>
    List.fold_left(
      (b, r) => {
        let* _ = b;
        ana_rule(ctx, r, pat_ty, clause_ty);
      },
      Some(),
      rules,
    )
  and ana_rule =
      (
        ctx: Contexts.t,
        Rule(p, clause): UHExp.rule,
        pat_ty: HTyp.t,
        clause_ty: HTyp.t,
      )
      : option(unit) => {
    let* ctx = Statics_Pat.ana(ctx, p, pat_ty);
    ana(ctx, clause, clause_ty);
  };
  /**
   * Get type mode of nth operand of an opseq in synthetic position
   */
  let rec syn_nth_type_mode =
          (ctx: Contexts.t, n: int, OpSeq(skel, seq): UHExp.opseq)
          : option(Statics.type_mode) =>
    syn_nth_type_mode'(ctx, n, skel, seq)
  and syn_nth_type_mode' =
      (ctx: Contexts.t, n: int, skel: UHExp.skel, seq: UHExp.seq)
      : option(Statics.type_mode) => {
    let ana_go = (skel, ty) => ana_nth_type_mode'(ctx, n, skel, seq, ty);
    let rec go = (skel: UHExp.skel) =>
      switch (skel) {
      | Placeholder(n') =>
        assert(n == n');
        Some(Statics.Syn);
      | BinOp(InHole(_), op, skel1, skel2) =>
        go(BinOp(NotInHole, op, skel1, skel2))
      | BinOp(NotInHole, Comma, skel1, skel2) =>
        n <= Skel.rightmost_tm_index(skel1) ? go(skel1) : go(skel2)
      | BinOp(NotInHole, Space, skel1, skel2) =>
        let livelit_ap_check = LivelitUtil.check_livelit(ctx, seq, skel);
        switch (livelit_ap_check) {
        | Some((_, _, _, reqd_param_tys, _)) =>
          let (_, reqd_param_tys) = List.split(reqd_param_tys);
          let param_offset = n - Skel.leftmost_tm_index(skel);
          if (param_offset == 0) {
            Some(Syn);
          } else {
            Some(Ana(List.nth(reqd_param_tys, param_offset - 1)));
          };
        | None =>
          let* ty1 = syn_skel(ctx, skel1, seq);
          let* (ty_2, _) = HTyp.matched_arrow(ty1);
          n <= Skel.rightmost_tm_index(skel1)
            ? go(skel1) : ana_go(skel2, ty_2);
        };
      | BinOp(NotInHole, Cons, skel1, skel2) =>
        let* ty1 = syn_skel(ctx, skel1, seq);
        n <= Skel.rightmost_tm_index(skel1)
          ? go(skel1) : ana_go(skel2, HTyp.List(ty1));
      | BinOp(
          NotInHole,
          Plus | Minus | Times | Divide | LessThan | GreaterThan,
          skel1,
          skel2,
        ) =>
        n <= Skel.rightmost_tm_index(skel1)
          ? ana_go(skel1, Int) : ana_go(skel2, Int)
      | BinOp(
          NotInHole,
          FPlus | FMinus | FTimes | FDivide | FLessThan | FGreaterThan,
          skel1,
          skel2,
        ) =>
        n <= Skel.rightmost_tm_index(skel1)
          ? ana_go(skel1, Float) : ana_go(skel2, Float)
      | BinOp(NotInHole, And | Or, skel1, skel2) =>
        n <= Skel.rightmost_tm_index(skel1)
          ? ana_go(skel1, Bool) : ana_go(skel2, Bool)
      | BinOp(NotInHole, Caret, skel1, skel2) =>
        n <= Skel.rightmost_tm_index(skel1)
          ? ana_go(skel1, String) : ana_go(skel2, String)
      | BinOp(NotInHole, Equals, skel1, skel2) =>
        if (n <= Skel.rightmost_tm_index(skel1)) {
          go(skel1);
        } else {
          let* ty1 = syn_skel(ctx, skel1, seq);
          ana_go(skel2, ty1);
        }
      | BinOp(NotInHole, FEquals, skel1, skel2) =>
        if (n <= Skel.rightmost_tm_index(skel1)) {
          go(skel1);
        } else {
          let* ty1 = syn_skel(ctx, skel1, seq);
          ana_go(skel2, ty1);
        }
      };
    go(skel);
  }
  /**
   * Get type mode of nth operand of an opseq in analytic position
   */
  and ana_nth_type_mode =
      (
        ctx: Contexts.t,
        n: int,
        OpSeq(skel, seq) as opseq: UHExp.opseq,
        ty: HTyp.t,
      )
      : option(Statics.type_mode) => {
    // handle n-tuples
    switch (tuple_zip(skel, ty)) {
    | None =>
      syn_nth_type_mode(ctx, n, UHExp.set_err_status_opseq(NotInHole, opseq))
    | Some(skel_tys) =>
      let (nskel, nty) =
        skel_tys
        |> List.find(((skel, _)) =>
             Skel.leftmost_tm_index(skel) <= n
             && n <= Skel.rightmost_tm_index(skel)
           );
      ana_nth_type_mode'(ctx, n, nskel, seq, nty);
    };
  }
  and ana_nth_type_mode' =
      (ctx: Contexts.t, n: int, skel: UHExp.skel, seq: UHExp.seq, ty: HTyp.t)
      : option(Statics.type_mode) => {
    let syn_go = skel => syn_nth_type_mode'(ctx, n, skel, seq);
    let rec go = (skel: UHExp.skel, ty: HTyp.t) =>
      switch (skel) {
      | BinOp(_, Comma, _, _)
      | BinOp(InHole(WrongLength, _), _, _, _) =>
        failwith(__LOC__ ++ ": expected tuples to be handled at opseq level")
      | Placeholder(n') =>
        assert(n == n');
        Some(Statics.Ana(ty));
      | BinOp(InHole(TypeInconsistent(_), _), op, skel1, skel2) =>
        let skel_not_in_hole = Skel.BinOp(NotInHole, op, skel1, skel2);
        syn_go(skel_not_in_hole);
      | BinOp(NotInHole, Cons, skel1, skel2) =>
        let* ty_elt = HTyp.matched_list(ty);
        n <= Skel.rightmost_tm_index(skel1)
          ? go(skel1, ty_elt) : go(skel2, ty);
      | BinOp(
          NotInHole,
          And | Or | Minus | Plus | Times | Divide | FMinus | FPlus | FTimes |
          FDivide |
          LessThan |
          GreaterThan |
          Equals |
          FLessThan |
          FGreaterThan |
          FEquals |
          Caret |
          Space,
          _,
          _,
        ) =>
        syn_go(skel)
      };
    go(skel, ty);
  };

  /* If renumber_empty_holes is true, then the metavars in empty holes will be assigned
   * new values in the same namespace as non-empty holes. Non-empty holes are renumbered
   * regardless.
   */
  let rec syn_fix_holes =
          (
            ctx: Contexts.t,
            u_gen: MetaVarGen.t,
            ~renumber_empty_holes=false,
            e: UHExp.t,
          )
          : (UHExp.t, HTyp.t, MetaVarGen.t) =>
    syn_fix_holes_block(ctx, u_gen, ~renumber_empty_holes, e)
  and syn_fix_holes_block =
      (
        ctx: Contexts.t,
        u_gen: MetaVarGen.t,
        ~renumber_empty_holes=false,
        block: UHExp.block,
      )
      : (UHExp.block, HTyp.t, MetaVarGen.t) =>
    switch (block |> UHExp.Block.split_conclusion) {
    | None =>
      let (leading, _ctx, u_gen) =
        syn_fix_holes_lines(ctx, u_gen, ~renumber_empty_holes, block);
      let (conclusion, u_gen) = u_gen |> UHExp.new_EmptyHole;
      (leading @ [UHExp.ExpLine(conclusion |> OpSeq.wrap)], Hole, u_gen);
    | Some((leading, conclusion)) =>
      let (leading, ctx, u_gen) =
        syn_fix_holes_lines(ctx, u_gen, ~renumber_empty_holes, leading);
      let (conclusion, ty, u_gen) =
        syn_fix_holes_opseq(ctx, u_gen, ~renumber_empty_holes, conclusion);
      (leading @ [UHExp.ExpLine(conclusion)], ty, u_gen);
    }
  and syn_fix_holes_lines =
      (
        ctx: Contexts.t,
        u_gen: MetaVarGen.t,
        ~renumber_empty_holes=false,
        lines: list(UHExp.line),
      )
      : (list(UHExp.line), Contexts.t, MetaVarGen.t) => {
    let (rev_fixed_lines, ctx, u_gen) =
      lines
      |> List.fold_left(
           (
             (fixed_lines, ctx, u_gen): (
               list(UHExp.line),
               Contexts.t,
               MetaVarGen.t,
             ),
             line: UHExp.line,
           ) => {
             let (fixed_line, ctx, u_gen) =
               syn_fix_holes_line(ctx, u_gen, ~renumber_empty_holes, line);
             ([fixed_line, ...fixed_lines], ctx, u_gen);
           },
           ([], ctx, u_gen),
         );
    (rev_fixed_lines |> List.rev, ctx, u_gen);
  }
  and syn_fix_holes_line =
      (
        ctx: Contexts.t,
        u_gen: MetaVarGen.t,
        ~renumber_empty_holes=false,
        line: UHExp.line,
      )
      : (UHExp.line, Contexts.t, MetaVarGen.t) =>
    switch (line) {
    | ExpLine(e) =>
      let (e, _, u_gen) =
        syn_fix_holes_opseq(ctx, u_gen, ~renumber_empty_holes, e);
      (ExpLine(e), ctx, u_gen);
    | EmptyLine
    | CommentLine(_) => (line, ctx, u_gen)
    | LetLine(_, p, def) =>
      let (p, ty_p, _, u_gen) =
        Statics_Pat.syn_fix_holes(ctx, u_gen, ~renumber_empty_holes, p);
      switch (LLPat.of_uhpat(p)) {
      | None =>
        let def_ctx = extend_let_def_ctx(ctx, p, def);
        let (def, u_gen) =
          ana_fix_holes(def_ctx, u_gen, ~renumber_empty_holes, def, ty_p);
        let body_ctx = extend_let_body_ctx(ctx, p, def);
        (UHExp.letline(p, def), body_ctx, u_gen);
      | Some(llp) =>
        let (err, def, ctx, u_gen) =
          fix_livelit_abbreviation(
            ~renumber_empty_holes,
            ctx,
            u_gen,
            llp,
            def,
          );
        (UHExp.letline(~err, p, def), ctx, u_gen);
      // switch (LLExp.of_uhexp(def)) {
      // | None =>
      //   let (err, u_gen) = {
      //     let (u, u_gen) = MetaVarGen.next_hole(u_gen);
      //     (AbbrevErrStatus.InAbbrevHole(NotLivelitExp, u), u_gen);
      //   };
      //   let (def, _, u_gen) =
      //     syn_fix_holes(ctx, u_gen, ~renumber_empty_holes, def);
      //   (UHExp.letline(~err, p, def), ctx, u_gen);
      // | Some((lln_old, args)) =>
      //   let (err, args, ctx, u_gen) =
      //     fix_livelit_abbreviation(
      //       ~renumber_empty_holes,
      //       ctx,
      //       u_gen,
      //       lln_new,
      //       lln_old,
      //       args,
      //     );
      //   (LivelitAbbrev.mk(~err, lln_new, (lln_old, args)), ctx, u_gen);
      // }
      };
    | LivelitDefLine({init, update, view, shape, expand, _} as llrecord) =>
      // TODO: captures
      let {init_ty, update_ty, view_ty, shape_ty, expand_ty} =
        livelit_types(llrecord);
      let (init, u_gen) =
        ana_fix_holes(ctx, u_gen, ~renumber_empty_holes, init, init_ty);
      let (update, u_gen) =
        ana_fix_holes(ctx, u_gen, ~renumber_empty_holes, update, update_ty);
      let (view, u_gen) =
        ana_fix_holes(ctx, u_gen, ~renumber_empty_holes, view, view_ty);
      let (shape, u_gen) =
        ana_fix_holes(ctx, u_gen, ~renumber_empty_holes, shape, shape_ty);
      let (expand, u_gen) =
        ana_fix_holes(ctx, u_gen, ~renumber_empty_holes, expand, expand_ty);
      (
        LivelitDefLine({...llrecord, init, update, view, shape, expand}),
        extend_livelit_ctx(ctx, llrecord),
        u_gen,
      );
    }
  and fix_livelit_abbreviation =
      (
        ~renumber_empty_holes,
        ctx: Contexts.t,
        u_gen: MetaVarGen.t,
        llp: LLPat.t,
        e: UHExp.t,
      )
      : (AbbrevErrStatus.t, UHExp.t, Contexts.t, MetaVarGen.t) => {
    switch (LLExp.of_uhexp(e)) {
    | None =>
      let (err, u_gen) = {
        let (u, u_gen) = MetaVarGen.next_hole(u_gen);
        (AbbrevErrStatus.InAbbrevHole(NotLivelitExp, u), u_gen);
      };
      let (e, _, u_gen) =
        syn_fix_holes(ctx, u_gen, ~renumber_empty_holes, e);
      (err, e, ctx, u_gen);
    | Some({hd, args, meta}) =>
      let (gamma, livelit_ctx) = ctx;
      switch (LivelitCtx.lookup(livelit_ctx, hd)) {
      | None =>
        let (e, _, u_gen) =
          syn_fix_holes(ctx, u_gen, ~renumber_empty_holes, e);
        // let (u_gen, args) =
        //   ana_fix_args(u_gen, args |> List.map(_ => HTyp.Hole));
        let (u, u_gen) = MetaVarGen.next_hole(u_gen);
        (InAbbrevHole(Free, u), e, ctx, u_gen);
      | Some((old_def, applied_param_tys)) =>
        let unapplied_param_tys =
          ListUtil.drop(List.length(applied_param_tys), old_def.param_tys);
        let (arg_tys, (extra_args, _)) =
          ListUtil.zip_tails(args, unapplied_param_tys);
        let (u_gen, fixed_args) =
          arg_tys
          |> ListUtil.map_with_accumulator(
               (u_gen, (arg, (_, ty))) => {
                 let (arg, u_gen) =
                   ana_fix_holes_operand(
                     ctx,
                     u_gen,
                     ~renumber_empty_holes,
                     arg,
                     ty,
                   );
                 (u_gen, arg);
               },
               u_gen,
             );
        let (u_gen, fixed_extra_args) =
          extra_args
          |> ListUtil.map_with_accumulator(
               (u_gen, arg) => {
                 let (arg, _, u_gen) =
                   syn_fix_holes_operand(
                     ctx,
                     u_gen,
                     ~renumber_empty_holes,
                     arg,
                   );
                 (u_gen, arg);
               },
               u_gen,
             );

        let (meta: LLExp.meta, u_gen) =
          switch (meta) {
          | Free(u) => (Free(u), u_gen)
          | Ap(r) =>
            let (u, u_gen) =
              renumber_empty_holes
                ? MetaVarGen.next_livelit(u_gen) : (r.u, u_gen);
            (Ap({...r, u, err: NotInHole}), u_gen);
          };
        let e =
          LLExp.to_uhexp({meta, hd, args: fixed_args @ fixed_extra_args});
        switch (extra_args) {
        | [_, ..._] =>
          let (u, u_gen) = MetaVarGen.next_hole(u_gen);
          (InAbbrevHole(ExtraneousArgs, u), e, ctx, u_gen);
        | [] =>
          let livelit_ctx =
            LivelitCtx.extend(
              livelit_ctx,
              (
                llp,
                (old_def, applied_param_tys @ snd(List.split(arg_tys))),
              ),
            );
          (NotInAbbrevHole, e, (gamma, livelit_ctx), u_gen);
        };
      };
    };
  }
  and syn_fix_holes_opseq =
      (
        ctx: Contexts.t,
        u_gen: MetaVarGen.t,
        ~renumber_empty_holes=false,
        OpSeq(skel, seq): UHExp.opseq,
      )
      : (UHExp.opseq, HTyp.t, MetaVarGen.t) => {
    let (skel, seq, ty, u_gen) =
      syn_fix_holes_skel(ctx, u_gen, ~renumber_empty_holes, skel, seq);
    (OpSeq(skel, seq), ty, u_gen);
  }
  and syn_fix_holes_skel =
      (
        ctx: Contexts.t,
        u_gen: MetaVarGen.t,
        ~renumber_empty_holes=false,
        skel: UHExp.skel,
        seq: UHExp.seq,
      )
      : (UHExp.skel, UHExp.seq, HTyp.t, MetaVarGen.t) =>
    switch (skel) {
    | Placeholder(n) =>
      let en = seq |> Seq.nth_operand(n);
      let (en, ty, u_gen) =
        syn_fix_holes_operand(ctx, u_gen, ~renumber_empty_holes, en);
      let seq = seq |> Seq.update_nth_operand(n, en);
      (skel, seq, ty, u_gen);
    | BinOp(_, (Minus | Plus | Times | Divide) as op, skel1, skel2) =>
      let (skel1, seq, u_gen) =
        ana_fix_holes_skel(
          ctx,
          u_gen,
          ~renumber_empty_holes,
          skel1,
          seq,
          HTyp.Int,
        );
      let (skel2, seq, u_gen) =
        ana_fix_holes_skel(
          ctx,
          u_gen,
          ~renumber_empty_holes,
          skel2,
          seq,
          HTyp.Int,
        );
      (BinOp(NotInHole, op, skel1, skel2), seq, Int, u_gen);
    | BinOp(_, (FMinus | FPlus | FTimes | FDivide) as op, skel1, skel2) =>
      let (skel1, seq, u_gen) =
        ana_fix_holes_skel(
          ctx,
          u_gen,
          ~renumber_empty_holes,
          skel1,
          seq,
          HTyp.Float,
        );
      let (skel2, seq, u_gen) =
        ana_fix_holes_skel(
          ctx,
          u_gen,
          ~renumber_empty_holes,
          skel2,
          seq,
          HTyp.Float,
        );
      (BinOp(NotInHole, op, skel1, skel2), seq, Float, u_gen);
    | BinOp(_, (And | Or) as op, skel1, skel2) =>
      let (skel1, seq, u_gen) =
        ana_fix_holes_skel(
          ctx,
          u_gen,
          ~renumber_empty_holes,
          skel1,
          seq,
          HTyp.Bool,
        );
      let (skel2, seq, u_gen) =
        ana_fix_holes_skel(
          ctx,
          u_gen,
          ~renumber_empty_holes,
          skel2,
          seq,
          HTyp.Bool,
        );
      (BinOp(NotInHole, op, skel1, skel2), seq, Bool, u_gen);
    | BinOp(_, Caret as op, skel1, skel2) =>
      let (skel1, seq, u_gen) =
        ana_fix_holes_skel(
          ctx,
          u_gen,
          ~renumber_empty_holes,
          skel1,
          seq,
          HTyp.String,
        );
      let (skel2, seq, u_gen) =
        ana_fix_holes_skel(
          ctx,
          u_gen,
          ~renumber_empty_holes,
          skel2,
          seq,
          HTyp.String,
        );
      (BinOp(NotInHole, op, skel1, skel2), seq, String, u_gen);
    | BinOp(_, (LessThan | GreaterThan | Equals) as op, skel1, skel2) =>
      let (skel1, seq, u_gen) =
        ana_fix_holes_skel(
          ctx,
          u_gen,
          ~renumber_empty_holes,
          skel1,
          seq,
          HTyp.Int,
        );
      let (skel2, seq, u_gen) =
        ana_fix_holes_skel(
          ctx,
          u_gen,
          ~renumber_empty_holes,
          skel2,
          seq,
          HTyp.Int,
        );
      (BinOp(NotInHole, op, skel1, skel2), seq, Bool, u_gen);
    | BinOp(_, (FLessThan | FGreaterThan | FEquals) as op, skel1, skel2) =>
      let (skel1, seq, u_gen) =
        ana_fix_holes_skel(
          ctx,
          u_gen,
          ~renumber_empty_holes,
          skel1,
          seq,
          HTyp.Float,
        );
      let (skel2, seq, u_gen) =
        ana_fix_holes_skel(
          ctx,
          u_gen,
          ~renumber_empty_holes,
          skel2,
          seq,
          HTyp.Float,
        );
      (BinOp(NotInHole, op, skel1, skel2), seq, Bool, u_gen);
    | BinOp(_, Space, skel1, skel2) =>
      let livelit_check =
        LivelitUtil.check_livelit(
          ~permit_free_livelit=true,
          ~permit_insufficient_params_hole=true,
          ctx,
          seq,
          skel,
        );
      switch (livelit_check) {
      | Some((data, livelit_defn, _, reqd_param_tys, args)) =>
        let (_, reqd_param_tys) = List.split(reqd_param_tys);
        let (fixed_ll, fixed_ty, u_gen) =
          switch (data) {
          | ApLivelitData(llu, _, lln, model, splice_info) =>
            syn_fix_holes_ApLivelit(
              ctx,
              u_gen,
              ~renumber_empty_holes,
              false,
              livelit_defn,
              llu,
              lln,
              model,
              splice_info,
            )
          | FreeLivelitData(_, lln) =>
            syn_fix_holes_FreeLivelit(
              ctx,
              u_gen,
              ~renumber_empty_holes,
              false,
              livelit_defn,
              lln,
            )
          };
        let ((seq, u_gen), new_args) =
          ListUtil.map_with_accumulator(
            ((seq, u_gen), (param_ty, arg)) => {
              let (new_arg, seq, u_gen) =
                ana_fix_holes_skel(
                  ctx,
                  u_gen,
                  ~renumber_empty_holes,
                  arg,
                  seq,
                  param_ty,
                );
              ((seq, u_gen), new_arg);
            },
            (seq, u_gen),
            List.combine(reqd_param_tys, args),
          );
        let livelitN = Skel.leftmost_tm_index(skel);
        let seq = seq |> Seq.update_nth_operand(livelitN, fixed_ll);
        let skel =
          new_args
          |> List.fold_left(
               (skel, arg) =>
                 Skel.BinOp(NotInHole, Operators_Exp.Space, skel, arg),
               Placeholder(livelitN),
             );
        (skel, seq, fixed_ty, u_gen);
      | None =>
        let (skel1, seq, ty1, u_gen) =
          syn_fix_holes_skel(ctx, u_gen, ~renumber_empty_holes, skel1, seq);
        switch (HTyp.matched_arrow(ty1)) {
        | Some((ty2, ty)) =>
          let (skel2, seq, u_gen) =
            ana_fix_holes_skel(
              ctx,
              u_gen,
              ~renumber_empty_holes,
              skel2,
              seq,
              ty2,
            );
          (BinOp(NotInHole, Space, skel1, skel2), seq, ty, u_gen);
        | None =>
          let (skel2, seq, u_gen) =
            ana_fix_holes_skel(
              ctx,
              u_gen,
              ~renumber_empty_holes,
              skel2,
              seq,
              HTyp.Hole,
            );
          let (OpSeq(skel1, seq), u_gen) =
            UHExp.mk_inconsistent_opseq(u_gen, OpSeq(skel1, seq));
          (BinOp(NotInHole, Space, skel1, skel2), seq, Hole, u_gen);
        };
      };
    | BinOp(_, Comma, _, _) =>
      let ((u_gen, seq), pairs) =
        skel
        |> UHExp.get_tuple_elements
        |> ListUtil.map_with_accumulator(
             ((u_gen, seq), skel) => {
               let (skel, seq, ty, u_gen) =
                 syn_fix_holes_skel(
                   ctx,
                   u_gen,
                   ~renumber_empty_holes,
                   skel,
                   seq,
                 );
               ((u_gen, seq), (skel, ty));
             },
             (u_gen, seq),
           );
      let (skels, tys) = List.split(pairs);
      (UHExp.mk_tuple(skels), seq, Prod(tys), u_gen);
    | BinOp(_, Cons, skel1, skel2) =>
      let (skel1, seq, ty_elt, u_gen) =
        syn_fix_holes_skel(ctx, u_gen, ~renumber_empty_holes, skel1, seq);
      let ty = HTyp.List(ty_elt);
      let (skel2, seq, u_gen) =
        ana_fix_holes_skel(ctx, u_gen, ~renumber_empty_holes, skel2, seq, ty);
      let skel = Skel.BinOp(NotInHole, Operators_Exp.Cons, skel1, skel2);
      (skel, seq, ty, u_gen);
    }
  and syn_fix_holes_operand =
      (
        ctx: Contexts.t,
        u_gen: MetaVarGen.t,
        ~renumber_empty_holes=false,
        e: UHExp.operand,
      )
      : (UHExp.operand, HTyp.t, MetaVarGen.t) => {
    let e_nih = UHExp.set_err_status_operand(NotInHole, e);
    switch (e) {
    | EmptyHole(_) =>
      if (renumber_empty_holes) {
        let (u, u_gen) = MetaVarGen.next_hole(u_gen);
        (EmptyHole(u), Hole, u_gen);
      } else {
        (e, Hole, u_gen);
      }
    | InvalidText(_) => (e, Hole, u_gen)
    | Var(_, var_err_status, x) =>
      let gamma = Contexts.gamma(ctx);
      switch (VarMap.lookup(gamma, x)) {
      | Some(ty) => (UHExp.Var(NotInHole, NotInVarHole, x), ty, u_gen)
      | None =>
        switch (var_err_status) {
        | InVarHole(_, _) => (e_nih, HTyp.Hole, u_gen)
        | NotInVarHole =>
          let (u, u_gen) = MetaVarGen.next_hole(u_gen);
          let reason: VarErrStatus.HoleReason.t =
            switch (Var.is_let(x), Var.is_case(x)) {
            | (true, _) => Keyword(Let)
            | (_, true) => Keyword(Case)
            | _ => Free
            };
          (Var(NotInHole, InVarHole(reason, u), x), Hole, u_gen);
        }
      };
    | IntLit(_, _) => (e_nih, Int, u_gen)
    | FloatLit(_, _) => (e_nih, Float, u_gen)
    | BoolLit(_, _) => (e_nih, Bool, u_gen)
    | StringLit(_, _) => (e_nih, String, u_gen)
    | ListNil(_) => (e_nih, List(Hole), u_gen)
    | Parenthesized(body) =>
      let (block, ty, u_gen) =
        syn_fix_holes(ctx, u_gen, ~renumber_empty_holes, body);
      (Parenthesized(block), ty, u_gen);
    | Lam(_, p, body) =>
      let (p, ty_p, ctx_body, u_gen) =
        Statics_Pat.syn_fix_holes(ctx, u_gen, ~renumber_empty_holes, p);
      let (body, ty_body, u_gen) =
        syn_fix_holes(ctx_body, u_gen, ~renumber_empty_holes, body);
      (Lam(NotInHole, p, body), Arrow(ty_p, ty_body), u_gen);
    | Inj(_, side, body) =>
      let (body, ty1, u_gen) =
        syn_fix_holes(ctx, u_gen, ~renumber_empty_holes, body);
      let ty =
        switch (side) {
        | L => HTyp.Sum(ty1, Hole)
        | R => HTyp.Sum(Hole, ty1)
        };
      (Inj(NotInHole, side, body), ty, u_gen);
    | Case(_, scrut, rules) =>
      let (scrut, ty1, u_gen) =
        syn_fix_holes(ctx, u_gen, ~renumber_empty_holes, scrut);
      let (rules, u_gen, rule_types, common_type) =
        syn_fix_holes_rules(ctx, u_gen, ~renumber_empty_holes, rules, ty1);
      switch (common_type) {
      | None =>
        let (u, u_gen) = MetaVarGen.next_hole(u_gen);
        (
          Case(InconsistentBranches(rule_types, u), scrut, rules),
          HTyp.Hole,
          u_gen,
        );
      | Some(common_type) => (
          Case(StandardErrStatus(NotInHole), scrut, rules),
          common_type,
          u_gen,
        )
      };
    | Subscript(_, target, start_, end_) =>
      let (target, u_gen) =
        ana_fix_holes(ctx, u_gen, ~renumber_empty_holes, target, String);
      let (start_, u_gen) =
        ana_fix_holes(ctx, u_gen, ~renumber_empty_holes, start_, Int);
      let (end_, u_gen) =
        ana_fix_holes(ctx, u_gen, ~renumber_empty_holes, end_, Int);
      (Subscript(NotInHole, target, start_, end_), String, u_gen);
    | ApLivelit(llu, _, _, lln, model, splice_info) =>
      let livelit_ctx = Contexts.livelit_ctx(ctx);
      switch (LivelitCtx.lookup(livelit_ctx, lln)) {
      | None =>
        let (u, u_gen) = MetaVarGen.next_hole(u_gen);
        (FreeLivelit(u, lln), Hole, u_gen);
      | Some((livelit_defn, closed_tys)) =>
        let put_in_hole =
          List.length(livelit_defn.param_tys) != List.length(closed_tys);
        syn_fix_holes_ApLivelit(
          ctx,
          u_gen,
          ~renumber_empty_holes,
          put_in_hole,
          livelit_defn,
          llu,
          lln,
          model,
          splice_info,
        );
      };
    | FreeLivelit(_, lln) =>
      let livelit_ctx = Contexts.livelit_ctx(ctx);
      switch (LivelitCtx.lookup(livelit_ctx, lln)) {
      | None => (e, Hole, u_gen)
      | Some((livelit_defn, closed_tys)) =>
        let put_in_hole =
          List.length(livelit_defn.param_tys) != List.length(closed_tys);
        syn_fix_holes_FreeLivelit(
          ctx,
          u_gen,
          ~renumber_empty_holes,
          put_in_hole,
          livelit_defn,
          lln,
        );
      };
    };
  }
  and syn_fix_holes_rules =
      (
        ctx: Contexts.t,
        u_gen: MetaVarGen.t,
        ~renumber_empty_holes=false,
        rules: UHExp.rules,
        pat_ty: HTyp.t,
      )
      : (UHExp.rules, MetaVarGen.t, list(HTyp.t), option(HTyp.t)) => {
    let (rev_fixed_rules, u_gen, rule_types) =
      List.fold_left(
        ((rules, u_gen, rule_types), r) => {
          let (r, u_gen, r_ty) =
            syn_fix_holes_rule(ctx, u_gen, ~renumber_empty_holes, r, pat_ty);
          ([r, ...rules], u_gen, [r_ty, ...rule_types]);
        },
        ([], u_gen, []),
        rules,
      );
    let common_type = HTyp.join_all(GLB, rule_types);
    (List.rev(rev_fixed_rules), u_gen, List.rev(rule_types), common_type);
  }
  and syn_fix_holes_rule =
      (
        ctx: Contexts.t,
        u_gen: MetaVarGen.t,
        ~renumber_empty_holes=false,
        rule: UHExp.rule,
        pat_ty: HTyp.t,
      )
      : (UHExp.rule, MetaVarGen.t, HTyp.t) => {
    let Rule(p, clause) = rule;
    let (p, ctx, u_gen) =
      Statics_Pat.ana_fix_holes(ctx, u_gen, ~renumber_empty_holes, p, pat_ty);
    let (clause, clause_ty, u_gen) =
      syn_fix_holes(ctx, u_gen, ~renumber_empty_holes, clause);
    (Rule(p, clause), u_gen, clause_ty);
  }
  and syn_fix_holes_FreeLivelit =
      (ctx, u_gen, ~renumber_empty_holes, put_in_hole, livelit_defn, lln) => {
    /* initialize the livelit if it has come into scope */
    let init_model_cmd = livelit_defn.init_model;
    let (init_serialized_model, init_splice_info, u_gen) =
      SpliceGenCmd.exec(init_model_cmd, SpliceInfo.empty, u_gen);
    let (splice_map, u_gen) =
      ana_fix_holes_splice_map(
        ctx,
        u_gen,
        ~renumber_empty_holes,
        init_splice_info.splice_map,
      );
    let si = SpliceInfo.update_splice_map(init_splice_info, splice_map);
    let (llu, u_gen) = MetaVarGen.next_livelit(u_gen);
    syn_fix_holes_livelit(
      ctx,
      ~put_in_hole,
      u_gen,
      livelit_defn,
      llu,
      lln,
      init_serialized_model,
      si,
    );
  }
  and syn_fix_holes_ApLivelit =
      (
        ctx,
        u_gen,
        ~renumber_empty_holes,
        put_in_hole,
        livelit_defn,
        llu,
        lln,
        model,
        splice_info,
      )
      : (UHExp.operand, HTyp.t, MetaVarGen.t) => {
    let (llu, u_gen) =
      renumber_empty_holes ? MetaVarGen.next_livelit(u_gen) : (llu, u_gen);
    let (splice_map, u_gen) =
      ana_fix_holes_splice_map(
        ctx,
        u_gen,
        ~renumber_empty_holes,
        SpliceInfo.splice_map(splice_info),
      );
    let splice_info = SpliceInfo.update_splice_map(splice_info, splice_map);
    syn_fix_holes_livelit(
      ctx,
      ~put_in_hole,
      u_gen,
      livelit_defn,
      llu,
      lln,
      model,
      splice_info,
    );
  }
  and syn_fix_holes_livelit =
      (
        ctx,
        ~put_in_hole,
        u_gen,
        {captures_ty, expansion_ty, name, param_tys, expand, _},
        llu,
        lln,
        model,
        {splice_map, _} as splice_info,
      ) => {
    let (typ, err_status, u_gen) =
      if (put_in_hole) {
        let (u, u_gen) = MetaVarGen.next_hole(u_gen);
        (
          HTyp.Hole,
          ErrStatus.InHole(TypeInconsistent(Some(InsufficientParams)), u),
          u_gen,
        );
      } else {
        switch (expand(model)) {
        | Failure(_) =>
          let (u, u_gen) = MetaVarGen.next_hole(u_gen);
          (
            HTyp.Hole,
            ErrStatus.InHole(TypeInconsistent(Some(DecodingError)), u),
            u_gen,
          );
        | Success(expansion) =>
          switch (ana_splice_map_and_params(ctx, splice_map, param_tys)) {
          | None =>
            let (u, u_gen) = MetaVarGen.next_hole(u_gen);
            (
              HTyp.Hole,
              ErrStatus.InHole(TypeInconsistent(Some(DecodingError)), u),
              u_gen,
            );
          | Some(splice_ctx) =>
            switch (syn(splice_ctx, expansion)) {
            | None =>
              let (u, u_gen) = MetaVarGen.next_hole(u_gen);
              (
                HTyp.Hole,
                ErrStatus.InHole(TypeInconsistent(Some(DecodingError)), u),
                u_gen,
              );
            | Some(_ty) =>
              let expansion_ap_ty = HTyp.Arrow(captures_ty, expansion_ty);
              switch (ana(splice_ctx, expansion, expansion_ap_ty)) {
              | None =>
                let (u, u_gen) = MetaVarGen.next_hole(u_gen);
                (
                  expansion_ty,
                  ErrStatus.InHole(
                    TypeInconsistent(Some(IllTypedExpansion)),
                    u,
                  ),
                  u_gen,
                );
              | Some(_) => (expansion_ty, NotInHole, u_gen)
              };
            }
          }
        };
      };

    (
      UHExp.ApLivelit(llu, err_status, name, lln, model, splice_info),
      typ,
      u_gen,
    );
  }
  and ana_fix_holes_rules =
      (
        ctx: Contexts.t,
        u_gen: MetaVarGen.t,
        ~renumber_empty_holes=false,
        rules: UHExp.rules,
        pat_ty: HTyp.t,
        clause_ty: HTyp.t,
      )
      : (UHExp.rules, MetaVarGen.t) => {
    let (rev_fixed_rules, u_gen) =
      List.fold_left(
        ((rules, u_gen), r) => {
          let (r, u_gen) =
            ana_fix_holes_rule(
              ctx,
              u_gen,
              ~renumber_empty_holes,
              r,
              pat_ty,
              clause_ty,
            );
          ([r, ...rules], u_gen);
        },
        ([], u_gen),
        rules,
      );
    (List.rev(rev_fixed_rules), u_gen);
  }
  and ana_fix_holes_rule =
      (
        ctx: Contexts.t,
        u_gen: MetaVarGen.t,
        ~renumber_empty_holes=false,
        Rule(p, clause): UHExp.rule,
        pat_ty: HTyp.t,
        clause_ty: HTyp.t,
      )
      : (UHExp.rule, MetaVarGen.t) => {
    let (p, ctx, u_gen) =
      Statics_Pat.ana_fix_holes(ctx, u_gen, ~renumber_empty_holes, p, pat_ty);
    let (clause, u_gen) =
      ana_fix_holes(ctx, u_gen, ~renumber_empty_holes, clause, clause_ty);
    (Rule(p, clause), u_gen);
  }
  and ana_fix_holes_splice_map =
      (
        ctx: Contexts.t,
        u_gen: MetaVarGen.t,
        ~renumber_empty_holes=false,
        splice_map: UHExp.splice_map,
      )
      : (UHExp.splice_map, MetaVarGen.t) =>
    MetaVarMap.fold(
      (splice_name, (ty, e), (new_splice_map, u_gen)) => {
        let (e, u_gen) =
          ana_fix_holes(ctx, u_gen, ~renumber_empty_holes, e, ty);
        let new_splice_map =
          new_splice_map |> MetaVarMap.add(splice_name, (ty, e));
        (new_splice_map, u_gen);
      },
      splice_map,
      (MetaVarMap.empty, u_gen),
    )
  and ana_fix_holes =
      (
        ctx: Contexts.t,
        u_gen: MetaVarGen.t,
        ~renumber_empty_holes=false,
        e: UHExp.t,
        ty: HTyp.t,
      )
      : (UHExp.t, MetaVarGen.t) =>
    ana_fix_holes_block(ctx, u_gen, ~renumber_empty_holes, e, ty)
  and ana_fix_holes_block =
      (
        ctx: Contexts.t,
        u_gen: MetaVarGen.t,
        ~renumber_empty_holes=false,
        block: UHExp.block,
        ty: HTyp.t,
      )
      : (UHExp.block, MetaVarGen.t) =>
    switch (block |> UHExp.Block.split_conclusion) {
    | None =>
      let (leading, _ctx, u_gen) =
        syn_fix_holes_lines(ctx, u_gen, ~renumber_empty_holes, block);
      let (conclusion, u_gen) = u_gen |> UHExp.new_EmptyHole;
      (leading @ [UHExp.ExpLine(conclusion |> OpSeq.wrap)], u_gen);
    | Some((leading, conclusion)) =>
      let (leading, ctx, u_gen) =
        syn_fix_holes_lines(ctx, u_gen, ~renumber_empty_holes, leading);
      let (conclusion, u_gen) =
        ana_fix_holes_opseq(
          ctx,
          u_gen,
          ~renumber_empty_holes,
          conclusion,
          ty,
        );
      (leading @ [UHExp.ExpLine(conclusion)], u_gen);
    }
  and ana_fix_holes_opseq =
      (
        ctx: Contexts.t,
        u_gen: MetaVarGen.t,
        ~renumber_empty_holes=false,
        OpSeq(skel, seq) as opseq: UHExp.opseq,
        ty: HTyp.t,
      )
      : (UHExp.opseq, MetaVarGen.t) => {
    // handle n-tuples
    switch (tuple_zip(skel, ty)) {
    | Some(skel_tys) =>
      skel_tys
      |> List.fold_left(
           (
             (
               rev_skels: list(UHExp.skel),
               seq: UHExp.seq,
               u_gen: MetaVarGen.t,
             ),
             (skel: UHExp.skel, ty: HTyp.t),
           ) => {
             let (skel, seq, u_gen) =
               ana_fix_holes_skel(
                 ctx,
                 u_gen,
                 ~renumber_empty_holes,
                 skel,
                 seq,
                 ty,
               );
             ([skel, ...rev_skels], seq, u_gen);
           },
           ([], seq, u_gen),
         )
      |> (
        fun
        | (rev_skels, seq, u_gen) => {
            let skel = rev_skels |> List.rev |> UHExp.mk_tuple;
            (OpSeq.OpSeq(skel, seq), u_gen);
          }
      )
    | None =>
      if (List.length(HTyp.get_prod_elements(ty)) == 1) {
        skel
        |> UHExp.get_tuple_elements
        |> List.fold_left(
             (
               (
                 rev_skels: list(UHExp.skel),
                 seq: UHExp.seq,
                 u_gen: MetaVarGen.t,
               ),
               skel: UHExp.skel,
             ) => {
               let (skel, seq, _, u_gen) =
                 syn_fix_holes_skel(
                   ctx,
                   u_gen,
                   ~renumber_empty_holes,
                   skel,
                   seq,
                 );
               ([skel, ...rev_skels], seq, u_gen);
             },
             ([], seq, u_gen),
           )
        |> (
          fun
          | (rev_skels, seq, u_gen) => {
              let (u, u_gen) = MetaVarGen.next_hole(u_gen);
              let skel = UHExp.mk_tuple(List.rev(rev_skels));
              let opseq =
                UHExp.set_err_status_opseq(
                  InHole(TypeInconsistent(None), u),
                  OpSeq.OpSeq(skel, seq),
                );
              (opseq, u_gen);
            }
        );
      } else {
        let (u, u_gen) = u_gen |> MetaVarGen.next_hole;
        let (opseq, _, u_gen) =
          syn_fix_holes_opseq(
            ctx,
            u_gen,
            ~renumber_empty_holes,
            opseq |> UHExp.set_err_status_opseq(NotInHole),
          );
        (
          opseq |> UHExp.set_err_status_opseq(InHole(WrongLength, u)),
          u_gen,
        );
      }
    };
  }
  and ana_fix_holes_skel =
      (
        ctx: Contexts.t,
        u_gen: MetaVarGen.t,
        ~renumber_empty_holes=false,
        skel: UHExp.skel,
        seq: UHExp.seq,
        ty: HTyp.t,
      )
      : (UHExp.skel, UHExp.seq, MetaVarGen.t) =>
    switch (skel) {
    | BinOp(_, Comma, _, _) =>
      failwith("Exp.ana_fix_holes_skel: tuples handled at opseq level")
    | Placeholder(n) =>
      let en = seq |> Seq.nth_operand(n);
      let (en, u_gen) =
        ana_fix_holes_operand(ctx, u_gen, ~renumber_empty_holes, en, ty);
      let seq = seq |> Seq.update_nth_operand(n, en);
      (skel, seq, u_gen);
    | BinOp(_, Cons, skel1, skel2) =>
      switch (HTyp.matched_list(ty)) {
      | Some(ty_elt) =>
        let (skel1, seq, u_gen) =
          ana_fix_holes_skel(
            ctx,
            u_gen,
            ~renumber_empty_holes,
            skel1,
            seq,
            ty_elt,
          );
        let ty_list = HTyp.List(ty_elt);
        let (skel2, seq, u_gen) =
          ana_fix_holes_skel(
            ctx,
            u_gen,
            ~renumber_empty_holes,
            skel2,
            seq,
            ty_list,
          );
        let skel = Skel.BinOp(NotInHole, Operators_Exp.Cons, skel1, skel2);
        (skel, seq, u_gen);
      | None =>
        let (skel1, seq, ty_elt, u_gen) =
          syn_fix_holes_skel(ctx, u_gen, ~renumber_empty_holes, skel1, seq);
        let ty_list = HTyp.List(ty_elt);
        let (skel2, seq, u_gen) =
          ana_fix_holes_skel(
            ctx,
            u_gen,
            ~renumber_empty_holes,
            skel2,
            seq,
            ty_list,
          );
        let (u, u_gen) = MetaVarGen.next_hole(u_gen);
        let skel =
          Skel.BinOp(
            InHole(TypeInconsistent(None), u),
            Operators_Exp.Cons,
            skel1,
            skel2,
          );
        (skel, seq, u_gen);
      }
    | BinOp(
        _,
        And | Or | Minus | Plus | Times | Divide | FMinus | FPlus | FTimes |
        FDivide |
        LessThan |
        GreaterThan |
        Equals |
        FLessThan |
        FGreaterThan |
        FEquals |
        Caret |
        Space,
        _,
        _,
      ) =>
      let (skel, seq, ty', u_gen) =
        syn_fix_holes_skel(ctx, u_gen, ~renumber_empty_holes, skel, seq);
      if (HTyp.consistent(ty, ty')) {
        (skel, seq, u_gen);
      } else {
        let (OpSeq(skel, seq), u_gen) =
          UHExp.mk_inconsistent_opseq(u_gen, OpSeq(skel, seq));
        (skel, seq, u_gen);
      };
    }
  and ana_fix_holes_operand =
      (
        ctx: Contexts.t,
        u_gen: MetaVarGen.t,
        ~renumber_empty_holes=false,
        e: UHExp.operand,
        ty: HTyp.t,
      )
      : (UHExp.operand, MetaVarGen.t) =>
    switch (e) {
    | EmptyHole(_) =>
      if (renumber_empty_holes) {
        let (u, u_gen) = MetaVarGen.next_hole(u_gen);
        (EmptyHole(u), u_gen);
      } else {
        (e, u_gen);
      }
    | ApLivelit(
        _,
        InHole(TypeInconsistent(Some(InsufficientParams)), _),
        _,
        _,
        _,
        _,
      ) =>
      let (e, ty', u_gen) =
        syn_fix_holes_operand(ctx, u_gen, ~renumber_empty_holes, e);
      switch (UHExp.get_err_status_operand(e)) {
      | NotInHole when !HTyp.consistent(ty, ty') =>
        let (u, u_gen) = MetaVarGen.next_hole(u_gen);
        (
          UHExp.set_err_status_operand(InHole(TypeInconsistent(None), u), e),
          u_gen,
        );
      | _ => (e, u_gen)
      };
    | InvalidText(_) => (e, u_gen)
    | Var(_, _, _)
    | IntLit(_, _)
    | FloatLit(_, _)
    | BoolLit(_, _)
    | StringLit(_, _)
    | ApLivelit(_)
    | FreeLivelit(_) =>
      let (e, ty', u_gen) =
        syn_fix_holes_operand(ctx, u_gen, ~renumber_empty_holes, e);
      if (HTyp.consistent(ty, ty')) {
        (UHExp.set_err_status_operand(NotInHole, e), u_gen);
      } else {
        let (u, u_gen) = MetaVarGen.next_hole(u_gen);
        (
          UHExp.set_err_status_operand(InHole(TypeInconsistent(None), u), e),
          u_gen,
        );
      };
    | ListNil(_) =>
      switch (HTyp.matched_list(ty)) {
      | Some(_) => (UHExp.set_err_status_operand(NotInHole, e), u_gen)
      | None =>
        let (u, u_gen) = MetaVarGen.next_hole(u_gen);
        (ListNil(InHole(TypeInconsistent(None), u)), u_gen);
      }
    | Parenthesized(body) =>
      let (body, u_gen) =
        ana_fix_holes(ctx, u_gen, ~renumber_empty_holes, body, ty);
      (Parenthesized(body), u_gen);
    | Lam(_, p, def) =>
      switch (HTyp.matched_arrow(ty)) {
      | Some((ty1_given, ty2)) =>
        let (p, ctx, u_gen) =
          Statics_Pat.ana_fix_holes(
            ctx,
            u_gen,
            ~renumber_empty_holes,
            p,
            ty1_given,
          );
        let (def, u_gen) =
          ana_fix_holes(ctx, u_gen, ~renumber_empty_holes, def, ty2);
        (UHExp.Lam(NotInHole, p, def), u_gen);
      | None =>
        let (e', _, u_gen) =
          syn_fix_holes_operand(ctx, u_gen, ~renumber_empty_holes, e);
        let (u, u_gen) = MetaVarGen.next_hole(u_gen);
        (
          UHExp.set_err_status_operand(
            InHole(TypeInconsistent(None), u),
            e',
          ),
          u_gen,
        );
      }
    | Inj(_, side, body) =>
      switch (HTyp.matched_sum(ty)) {
      | Some((ty1, ty2)) =>
        let (e1, u_gen) =
          ana_fix_holes(
            ctx,
            u_gen,
            ~renumber_empty_holes,
            body,
            InjSide.pick(side, ty1, ty2),
          );
        (Inj(NotInHole, side, e1), u_gen);
      | None =>
        let (e', ty', u_gen) =
          syn_fix_holes_operand(ctx, u_gen, ~renumber_empty_holes, e);
        if (HTyp.consistent(ty, ty')) {
          (UHExp.set_err_status_operand(NotInHole, e'), u_gen);
        } else {
          let (u, u_gen) = MetaVarGen.next_hole(u_gen);
          (
            UHExp.set_err_status_operand(
              InHole(TypeInconsistent(None), u),
              e',
            ),
            u_gen,
          );
        };
      }
    | Case(_, scrut, rules) =>
      let (scrut, scrut_ty, u_gen) =
        syn_fix_holes(ctx, u_gen, ~renumber_empty_holes, scrut);
      let (rules, u_gen) =
        ana_fix_holes_rules(
          ctx,
          u_gen,
          ~renumber_empty_holes,
          rules,
          scrut_ty,
          ty,
        );
      (Case(StandardErrStatus(NotInHole), scrut, rules), u_gen);
    | Subscript(_, _, _, _) =>
      let (e', ty', u_gen) =
        syn_fix_holes_operand(ctx, u_gen, ~renumber_empty_holes, e);
      if (HTyp.consistent(ty, ty')) {
        (UHExp.set_err_status_operand(NotInHole, e'), u_gen);
      } else {
        let (u, u_gen) = MetaVarGen.next_hole(u_gen);
        (
          UHExp.set_err_status_operand(
            InHole(TypeInconsistent(None), u),
            e',
          ),
          u_gen,
        );
      };
    };

  let syn_fix_holes_z =
      (ctx: Contexts.t, u_gen: MetaVarGen.t, ze: ZExp.t)
      : (ZExp.t, HTyp.t, MetaVarGen.t) => {
    let path = CursorPath_Exp.of_z(ze);
    let (e, ty, u_gen) = syn_fix_holes(ctx, u_gen, ZExp.erase(ze));
    let ze =
      CursorPath_Exp.follow(path, e)
      |> OptUtil.get(() =>
           failwith(
             "Statics_Exp.syn_fix_holes did not preserve path "
             ++ Sexplib.Sexp.to_string(CursorPath.sexp_of_t(path)),
           )
         );
    (ze, ty, u_gen);
  };

  let syn_fix_holes_zlines =
      (ctx: Contexts.t, u_gen: MetaVarGen.t, zlines: ZExp.zblock)
      : (ZExp.zblock, Contexts.t, MetaVarGen.t) => {
    let path = CursorPath_Exp.of_zblock(zlines);
    let (lines, ctx, u_gen) =
      syn_fix_holes_lines(ctx, u_gen, ZExp.erase_zblock(zlines));
    let zlines =
      CursorPath_Exp.follow_block(path, lines)
      |> OptUtil.get(() =>
           failwith(
             "syn_fix_holes_lines did not preserve path "
             ++ Sexplib.Sexp.to_string(CursorPath.sexp_of_t(path)),
           )
         );
    (zlines, ctx, u_gen);
  };

  let syn_fix_holes_zrules =
      (
        ctx: Contexts.t,
        u_gen: MetaVarGen.t,
        zrules: ZExp.zrules,
        pat_ty: HTyp.t,
      )
      : (ZExp.zrules, list(HTyp.t), option(HTyp.t), MetaVarGen.t) => {
    let path = CursorPath_Exp.of_zrules(zrules);
    let rules = ZExp.erase_zrules(zrules);
    let (rules, u_gen, rule_types, common_type) =
      syn_fix_holes_rules(ctx, u_gen, rules, pat_ty);
    let zrules =
      CursorPath_Exp.follow_rules(path, rules)
      |> OptUtil.get(() =>
           failwith(
             "syn_fix_holes_rules did not preserve path "
             ++ Sexplib.Sexp.to_string(CursorPath.sexp_of_t(path)),
           )
         );
    (zrules, rule_types, common_type, u_gen);
  };

  let fix_livelit_abbreviation_z =
      (
        ~renumber_empty_holes,
        ctx: Contexts.t,
        u_gen: MetaVarGen.t,
        llp: LLPat.t,
        ze: ZExp.t,
      )
      : (AbbrevErrStatus.t, ZExp.t, Contexts.t, MetaVarGen.t) => {
    let path = CursorPath_Exp.of_z(ze);
    let (err, e, ctx, u_gen) =
      fix_livelit_abbreviation(
        ~renumber_empty_holes,
        ctx,
        u_gen,
        llp,
        ZExp.erase(ze),
      );
    let ze =
      CursorPath_Exp.follow(path, e)
      |> OptUtil.get_or_fail("fix_livelit_abbreviation did not preserve path");
    (err, ze, ctx, u_gen);
  };

  let ana_fix_holes_z =
      (ctx: Contexts.t, u_gen: MetaVarGen.t, ze: ZExp.t, ty: HTyp.t)
      : (ZExp.t, MetaVarGen.t) => {
    let path = CursorPath_Exp.of_z(ze);
    let (e, u_gen) = ana_fix_holes(ctx, u_gen, ZExp.erase(ze), ty);
    let ze =
      CursorPath_Exp.follow(path, e)
      |> OptUtil.get(() =>
           failwith(
             "ana_fix_holes did not preserve path "
             ++ Sexplib.Sexp.to_string(CursorPath.sexp_of_t(path)),
           )
         );
    (ze, u_gen);
  };

  /* Only to be used on top-level expressions, as it starts hole renumbering at 0 */
  let fix_and_renumber_holes =
      (ctx: Contexts.t, e: UHExp.t): (UHExp.t, HTyp.t, MetaVarGen.t) =>
    syn_fix_holes(ctx, MetaVarGen.init, ~renumber_empty_holes=true, e);

  let fix_and_renumber_holes_z =
      (ctx: Contexts.t, ze: ZExp.t): Statics.edit_state => {
    let path = CursorPath_Exp.of_z(ze);
    let (e, ty, u_gen) = fix_and_renumber_holes(ctx, ZExp.erase(ze));
    let ze =
      CursorPath_Exp.follow(path, e)
      |> OptUtil.get(() =>
           failwith(
             "fix_and_renumber_holes did not preserve path "
             ++ Sexplib.Sexp.to_string(CursorPath.sexp_of_t(path)),
           )
         );
    (ze, ty, u_gen);
  };

  let rec build_ll_view_ctx = (block: UHExp.t): Statics.livelit_web_view_ctx => {
    let ll_view = build_ll_view_ctx_block(block, VarMap.empty);
    IntMap.map(
      ((view, shape)) => (_mk_ll_view_new(view), mk_ll_shape(shape)),
      ll_view,
    );
  }
  and build_ll_view_ctx_block =
      (block: UHExp.t, def_ctx: Statics.livelit_def_ctx)
      : Statics.livelit_view_ctx => {
    let (_, view_ctx) =
      List.fold_left(
        ((def_ctx_acc, view_ctx_acc), line) => {
          let (new_defs_ctx, new_views_ctx) =
            build_ll_view_ctx_line(line, def_ctx_acc);
          (
            new_defs_ctx,
            MetaVarMap.shadowed_merge(view_ctx_acc, new_views_ctx),
          );
        },
        (def_ctx, MetaVarMap.empty),
        block,
      );
    view_ctx;
  }
  and build_ll_view_ctx_line =
      (line: UHExp.line, def_ctx: Statics.livelit_def_ctx)
      : (Statics.livelit_def_ctx, Statics.livelit_view_ctx) => {
    switch (line) {
    | EmptyLine
    | CommentLine(_) => (def_ctx, MetaVarMap.empty)
    | ExpLine(opseq) => (def_ctx, build_ll_view_ctx_opseq(opseq, def_ctx))
    | LetLine(_, p, def) =>
      switch (UHExp.Line.is_livelit_abbreviation(p, def)) {
      | Some(_) => (def_ctx, MetaVarMap.empty)
      | None => (def_ctx, build_ll_view_ctx_block(def, def_ctx))
      }
    | LivelitDefLine({name: (_, name_str), view, shape, _}) =>
      let new_def_ctx = VarMap.extend(def_ctx, (name_str, (view, shape)));
      (new_def_ctx, MetaVarMap.empty);
    };
  }
  and build_ll_view_ctx_opseq =
      (opseq: UHExp.opseq, def_ctx: Statics.livelit_def_ctx)
      : Statics.livelit_view_ctx => {
    let OpSeq(_, seq) = opseq;
    let operands = Seq.operands(seq);
    List.fold_left(
      (view_ctx_acc, operand) => {
        let new_view_ctx = build_ll_view_ctx_operand(operand, def_ctx);
        MetaVarMap.shadowed_merge(view_ctx_acc, new_view_ctx);
      },
      MetaVarMap.empty,
      operands,
    );
  }
  and build_ll_view_ctx_operand =
      (operand: UHExp.operand, def_ctx: Statics.livelit_def_ctx)
      : Statics.livelit_view_ctx => {
    switch (operand) {
    | EmptyHole(_)
    | InvalidText(_)
    | Var(_)
    | IntLit(_)
    | FloatLit(_)
    | BoolLit(_)
    | StringLit(_)
    | ListNil(_) => MetaVarMap.empty
    | Lam(_, _, block)
    | Inj(_, _, block)
    | Parenthesized(block) => build_ll_view_ctx_block(block, def_ctx)
    | Case(_, scrut, rules) =>
      let scrut_view_ctx = build_ll_view_ctx_block(scrut, def_ctx);
      let rules_view_ctx = build_ll_view_ctx_rules(rules, def_ctx);
      MetaVarMap.shadowed_merge(scrut_view_ctx, rules_view_ctx);
    | Subscript(_, t1, t2, t3) =>
      let t1_view_ctx = build_ll_view_ctx_block(t1, def_ctx);
      let t2_view_ctx = build_ll_view_ctx_block(t2, def_ctx);
      let t3_view_ctx = build_ll_view_ctx_block(t3, def_ctx);
      let t1_and_t2_view_ctx =
        MetaVarMap.shadowed_merge(t1_view_ctx, t2_view_ctx);
      MetaVarMap.shadowed_merge(t1_and_t2_view_ctx, t3_view_ctx);
    | ApLivelit(metavar, _, _base_name, name, _, spliceinfo) =>
      let new_view_ctx1 =
        switch (VarMap.lookup(def_ctx, name)) {
        | None => MetaVarMap.empty
        | Some((view, shape)) =>
          MetaVarMap.singleton(metavar, (view, shape))
        };
      let new_view_ctx2 = build_ll_view_ctx_splice_info(spliceinfo, def_ctx);
      MetaVarMap.shadowed_merge(new_view_ctx2, new_view_ctx1);
    | FreeLivelit(_metavar, _name) => MetaVarMap.empty
    };
  }
  and build_ll_view_ctx_splice_info =
      (splice_info: UHExp.splice_info, def_ctx: Statics.livelit_def_ctx)
      : Statics.livelit_view_ctx => {
    let {SpliceInfo.splice_map, _} = splice_info;
    IntMap.fold(
      (_k, (_, uhexp: UHExp.t), view_ctx_acc: Statics.livelit_view_ctx) => {
        let new_view_ctx = build_ll_view_ctx_block(uhexp, def_ctx);
        MetaVarMap.shadowed_merge(view_ctx_acc, new_view_ctx);
      },
      splice_map,
      MetaVarMap.empty: Statics.livelit_view_ctx,
    );
  }
  and build_ll_view_ctx_rules =
      (rules: UHExp.rules, def_ctx: Statics.livelit_def_ctx)
      : Statics.livelit_view_ctx => {
    List.fold_left(
      (view_ctx_acc, rule) => {
        let new_view_ctx = build_ll_view_ctx_rule(rule, def_ctx);
        MetaVarMap.shadowed_merge(view_ctx_acc, new_view_ctx);
      },
      MetaVarMap.empty,
      rules,
    );
  }
  and build_ll_view_ctx_rule =
      (rule: UHExp.rule, def_ctx: Statics.livelit_def_ctx)
      : Statics.livelit_view_ctx => {
    let Rule(_, rule_expr) = rule;
    build_ll_view_ctx_block(rule_expr, def_ctx);
  };
};

include M;
