open Sexplib.Std;
open OptUtil.Syntax;

[@deriving sexp]
type slice =
  | ExpBlock(ZExp.zblock)
  | ExpLine(ZExp.zline)
  | ExpSeq(ZExp.zopseq)
  | ExpOperand(ZExp.zoperand)
  | ExpOperator(ZExp.zoperator)
  | ExpRules(ZExp.zrules)
  | ExpRule(ZExp.zrule)
  | PatSeq(ZPat.zopseq)
  | PatOperand(ZPat.zoperand)
  | PatOperator(ZPat.zoperator)
  | TypSeq(ZTyp.zopseq)
  | TypOperand(ZTyp.zoperand)
  | TypOperator(ZTyp.zoperator);

[@deriving sexp]
type slice_info = {
  slice,
  ty_e: option(HTyp.t),
  ty_a: option(HTyp.t),
  ctx: Contexts.t,
};

let mk =
    (~ctx, ~ty_e: option(HTyp.t), ~ty_a: option(HTyp.t), ~slice: slice)
    : slice_info => {
  ctx,
  slice,
  ty_e,
  ty_a,
};

[@deriving sexp]
type t = list(slice_info);

let cons = List.cons;

let rec get_frame = (~ctx: Contexts.t, ~ty_e: option(HTyp.t), ze: ZExp.t): t =>
  get_frame_zblock(~ctx, ~ty_e, ze)

and get_frame_zblock =
    (
      ~ctx: Contexts.t,
      ~ty_e: option(HTyp.t),
      (prefix, zline, suffix) as zblock: ZExp.zblock,
    )
    : t => {
  //P.p("get_frame_exp_zblock ty:%s\n", sexp_of_option(HTyp.sexp_of_t, ty_e));
  let new_slice =
    mk(
      ~ctx,
      ~slice=ExpBlock(zblock),
      ~ty_e,
      ~ty_a=Statics_Exp.syn_block(ctx, ZExp.erase(zblock)),
    );
  let ctx' =
    switch (Statics_Exp.syn_lines(ctx, prefix)) {
    | Some(ctx) => ctx
    | None => ctx
    };
  cons(
    new_slice,
    // last line gets type
    get_frame_zline(~ctx=ctx', ~ty_e=suffix == [] ? ty_e : None, zline),
  );
}
and get_frame_zline =
    (~ctx: Contexts.t, ~ty_e: option(HTyp.t), zline: ZExp.zline): t => {
  //P.p("get_frame_exp_zline ty:%s\n", sexp_of_option(HTyp.sexp_of_t, ty_e));
  let slice = mk(~ctx, ~slice=ExpLine(zline), ~ty_e, ~ty_a=None);
  switch (zline) {
  | CursorL(_, ExpLine(opseq)) => [
      mk(
        ~ctx,
        ~slice=ExpLine(zline),
        ~ty_e,
        ~ty_a=Statics_Exp.syn_opseq(ctx, opseq),
      ),
    ]
  | CursorL(_) => [slice]
  | ExpLineZ(zopseq) =>
    cons(slice, get_frame_exp_zopseq(~ctx, ~ty_e, zopseq))
  | LetLineZE(p, zblock) =>
    let def = ZExp.erase(zblock);
    let def_ctx = Statics_Exp.extend_let_def_ctx(ctx, p, def);
    let ty_p =
      switch (Statics_Pat.syn(def_ctx, p)) {
      | Some((ty, _)) => Some(ty)
      | None => Some(HTyp.Hole)
      };
    //let body_ctx = Statics_Exp.extend_let_body_ctx(ctx, p, def);
    cons(slice, get_frame_zblock(~ctx=def_ctx, ~ty_e=ty_p, zblock));
  | LetLineZP(zpat, _) => cons(slice, get_frame_pat(~ctx, ~ty_e=None, zpat))
  };
}
and get_frame_exp_zopseq =
    (
      ~ctx: Contexts.t,
      ~ty_e: option(HTyp.t),
      ZOpSeq(_skel, zseq) as zopseq: ZExp.zopseq,
    )
    : t => {
  //P.p("get_frame_exp_zopseq ty:%s\n", sexp_of_option(HTyp.sexp_of_t, ty_e));
  let opseq = ZExp.erase_zopseq(zopseq);
  let ty_a =
    Statics_Exp.syn_opseq(ctx, UHExp.set_err_status_opseq(NotInHole, opseq));
  let slice = mk(~ctx, ~slice=ExpSeq(zopseq), ~ty_e, ~ty_a);
  switch (zseq) {
  | ZOperand(zop, (prefix, _)) =>
    let operand_index = Seq.length_of_affix(prefix);
    let operand_ty =
      switch (ty_e) {
      | Some(ty_e) =>
        switch (
          Statics_Exp.ana_nth_type_mode(ctx, operand_index, opseq, ty_e)
        ) {
        | Some(Ana(ty)) =>
          //P.p("ana ana ty:%s\n", HTyp.sexp_of_t(ty));
          Some(ty)
        | Some(Syn) =>
          //print_endline("ana_nth_type_mode syn so hole");
          Some(HTyp.Hole)
        | None =>
          //print_endline("ana_nth_type_mode NONE BECAUSE ERROR");
          None //lol
        }
      | None =>
        switch (Statics_Exp.syn_nth_type_mode(ctx, operand_index, opseq)) {
        | Some(Ana(ty)) =>
          //P.p("syn ana ty:%s\n", HTyp.sexp_of_t(ty));
          Some(ty)
        | Some(Syn) =>
          //print_endline("syn_nth_type_mode syn so hole");
          Some(HTyp.Hole)
        | _ =>
          //print_endline("syn_nth_type_mode syn NONE");
          None
        }
      };
    cons(slice, get_frame_exp_zoperand(~ctx, ~ty_e=operand_ty, zop));
  | ZOperator(zop, _) =>
    //TODO(andrew): FIX this is wrong
    cons(slice, get_frame_exp_zoperator(~ctx, ~ty_e=None, zop))
  };
}
and get_frame_exp_zoperator =
    (~ctx: Contexts.t, ~ty_e: option(HTyp.t), zop: ZExp.zoperator): t => {
  [
    // TODO(andrew): fix
    mk(~ctx, ~slice=ExpOperator(zop), ~ty_e, ~ty_a=None),
  ];
}
and get_frame_exp_zoperand =
    (~ctx: Contexts.t, ~ty_e: option(HTyp.t), zoperand: ZExp.zoperand): t => {
  //P.p(
  //  "get_frame_exp_zoperand ty:%s\n",
  //  sexp_of_option(HTyp.sexp_of_t, ty_e),
  //);
  let operand = ZExp.erase_zoperand(zoperand);
  let ty_a =
    Statics_Exp.syn_operand(
      ctx,
      UHExp.set_err_status_operand(NotInHole, operand),
    );
  let slice = mk(~ctx, ~slice=ExpOperand(zoperand), ~ty_e, ~ty_a);
  let tail =
    switch (zoperand) {
    | CursorE(_) => []
    | ApPaletteZ(_) => []
    | ParenthesizedZ(zexp) => get_frame(~ctx, ~ty_e, zexp)
    | LamZE(_, p, zexp) =>
      let (ctx_body, ty_body) =
        switch (Option.map(HTyp.matched_arrow, ty_e)) {
        | Some(Some((ty_p_given, ty_body))) =>
          switch (Statics_Pat.ana(ctx, p, ty_p_given)) {
          | Some(ctx_body) => (ctx_body, Some(ty_body))
          | None => (ctx, None)
          }
        | _ => (ctx, None)
        };
      get_frame(~ctx=ctx_body, ~ty_e=ty_body, zexp);
    | InjZ(_, side, zexp) =>
      let ty_side =
        switch (ty_e) {
        | None => None
        | Some(ty) =>
          switch (HTyp.matched_sum(ty)) {
          | None => None
          | Some((ty1, ty2)) => Some(InjSide.pick(side, ty1, ty2))
          }
        };
      get_frame(~ctx, ~ty_e=ty_side, zexp);
    | CaseZE(_, zexp, _) => get_frame(~ctx, ~ty_e=None, zexp)
    //TODO: consider integrating pattern types into ty_e
    | LamZP(_, zpat, _) => get_frame_pat(~ctx, ~ty_e=None, zpat)
    | CaseZR(_, scrut, zrules) =>
      let ty_scrut = Statics_Exp.syn(ctx, scrut);
      get_frame_zrules(~ctx, ~ty_e, ~ty_scrut, zrules);
    };
  cons(slice, tail);
}
and get_frame_zrules =
    (
      ~ctx: Contexts.t,
      ~ty_e: option(HTyp.t),
      ~ty_scrut: option(HTyp.t),
      (_, zrule, _) as zrules: ZExp.zrules,
    )
    : t => {
  let ty_a =
    switch (ty_scrut) {
    | Some(ty_scrut) =>
      Statics_Exp.syn_rules(ctx, ZExp.erase_zrules(zrules), ty_scrut)
    | None => None
    };
  let slice = mk(~ctx, ~slice=ExpRules(zrules), ~ty_a, ~ty_e=None);
  let tail = get_frame_zrule(~ctx, ~ty_e, ~ty_scrut, zrule);
  cons(slice, tail);
}
and get_frame_zrule =
    (
      ~ctx: Contexts.t,
      ~ty_e: option(HTyp.t),
      ~ty_scrut: option(HTyp.t),
      zrule: ZExp.zrule,
    )
    : t => {
  let ty_a =
    switch (ty_scrut) {
    | Some(ty_scrut) =>
      Statics_Exp.syn_rule(ctx, ZExp.erase_zrule(zrule), ty_scrut)
    | None => None
    };
  let slice = mk(~ctx, ~slice=ExpRule(zrule), ~ty_a, ~ty_e);
  let tail =
    switch (zrule) {
    | CursorR(_) => []
    | RuleZP(zpat, _) => get_frame_pat(~ctx, ~ty_e=ty_scrut, zpat)
    | RuleZE(_, zexp) => get_frame(~ctx, ~ty_e, zexp)
    };
  cons(slice, tail);
}
and get_frame_pat =
    (
      ~ctx: Contexts.t,
      ~ty_e: option(HTyp.t),
      ZOpSeq(_, zseq) as zopseq: ZPat.zopseq,
    )
    : t => {
  let opseq = ZPat.erase_zopseq(zopseq);
  let ty_a =
    Statics_Pat.syn(ctx, UHPat.set_err_status_opseq(NotInHole, opseq))
    |> Option.map(((ty, _)) => ty);
  let slice = mk(~ctx, ~slice=PatSeq(zopseq), ~ty_e, ~ty_a);
  switch (zseq) {
  | ZOperand(zop, (prefix, _)) =>
    let operand_index = Seq.length_of_affix(prefix);
    let operand_ty =
      switch (ty_e) {
      | Some(ty_e) =>
        switch (
          Statics_Pat.ana_nth_type_mode(ctx, operand_index, opseq, ty_e)
        ) {
        // or ana_nth?
        | Some(Ana(ty)) => Some(ty)
        | Some(Syn) => Some(HTyp.Hole)
        | None => None //lol
        }
      | _ => None
      };
    cons(slice, get_frame_pat_zoperand(~ctx, ~ty_e=operand_ty, zop));
  | ZOperator(zop, _) =>
    // TODO(andrew): fix ty_e!!!! adapt syn_nth_type_mode to return operator types
    cons(slice, get_frame_pat_zoperator(~ctx, ~ty_e=None, zop))
  };
}
and get_frame_pat_zoperator = // TODO(andrew): fix
    (~ctx: Contexts.t, ~ty_e as _: option(HTyp.t), zop: ZPat.zoperator): t => {
  [mk(~ctx, ~slice=PatOperator(zop), ~ty_e=None, ~ty_a=None)];
}

and get_frame_pat_zoperand =
    (~ctx: Contexts.t, ~ty_e: option(HTyp.t), zoperand: ZPat.zoperand): t => {
  let operand = ZPat.erase_zoperand(zoperand);
  let ty_a =
    Statics_Pat.syn_operand(
      ctx,
      UHPat.set_err_status_operand(NotInHole, operand),
    )
    |> Option.map(((ty, _)) => ty);
  let slice = mk(~ctx, ~slice=PatOperand(zoperand), ~ty_a, ~ty_e);
  let tail =
    switch (zoperand) {
    | CursorP(_) => []
    | ParenthesizedZ(zpat) => get_frame_pat(~ctx, ~ty_e, zpat)
    | InjZ(_, side, zpat) =>
      let ty_side = {
        let* ty = ty_e;
        let+ (ty1, ty2) = HTyp.matched_sum(ty);
        InjSide.pick(side, ty1, ty2);
      };
      get_frame_pat(~ctx, ~ty_e=ty_side, zpat);
    | TypeAnnZA(_, _, ty_zopseq) => get_frame_typ(ty_zopseq)
    | TypeAnnZP(_, zoperand, ann) =>
      let ty_ann = UHTyp.expand(ann);
      get_frame_pat_zoperand(~ctx, ~ty_e=Some(ty_ann), zoperand);
    };
  cons(slice, tail);
}
and get_frame_typ = (ZOpSeq(_, zseq) as zopseq: ZTyp.zopseq): t => {
  let opseq = ZTyp.erase_zopseq(zopseq);
  let ty_a = Some(UHTyp.expand(opseq)); // not actually true but whatever
  let slice =
    mk(~ctx=Contexts.empty, ~slice=TypSeq(zopseq), ~ty_e=None, ~ty_a);
  let tail =
    switch (zseq) {
    | ZOperand(zop, _) => get_frame_typ_zoperand(zop)
    | ZOperator(zop, _) => get_frame_typ_zoperator(zop)
    };
  cons(slice, tail);
}
and get_frame_typ_zoperator = (zoperator: ZTyp.zoperator): t => {
  // yolo
  // TODO: improve society somewhat
  let t_op_sort_of =
    switch (zoperator) {
    | (_, Arrow) => HTyp.Arrow(HTyp.Hole, HTyp.Hole)
    | (_, Prod) => HTyp.Prod([]) // lol
    | (_, Sum) => HTyp.Sum(HTyp.Hole, HTyp.Hole)
    };
  [
    mk(
      ~ctx=Contexts.empty,
      ~slice=TypOperator(zoperator),
      ~ty_e=None,
      ~ty_a=Some(t_op_sort_of),
    ),
  ];
}

and get_frame_typ_zoperand = (zoperand: ZTyp.zoperand): t => {
  // yolo
  let ty_operand = zoperand |> ZTyp.erase_zoperand |> UHTyp.expand_operand;
  [
    mk(
      ~ctx=Contexts.empty,
      ~slice=TypOperand(zoperand),
      ~ty_e=None,
      ~ty_a=Some(ty_operand),
    ),
  ];
};

let get = (zexp: ZExp.t): t =>
  zexp |> get_frame(~ctx=Contexts.empty, ~ty_e=Some(Hole)) |> List.rev;

let enclosing_zopseq = (zexp: ZExp.t): option(ZExp.zopseq) => {
  let frame = get(zexp);
  let is_expseq = si =>
    switch (si.slice) {
    | ExpSeq(_) => true
    | _ => false
    };
  switch (List.find_opt(is_expseq, frame)) {
  | Some({slice: ExpSeq(zopseq), _}) => Some(zopseq)
  | _ => None
  //print_endline(Sexplib.Sexp.to_string_hum(sexp_of_t(frame)));
  //failwith("INVALID FRAME (get_nearest_opseq)");
  };
};

let enclosing_zopseq_expected_ty = (zexp: ZExp.t): option(HTyp.t) => {
  let frame = get(zexp);
  let is_expseq = si =>
    switch (si.slice) {
    | ExpSeq(_) => true
    | _ => false
    };
  switch (List.find_opt(is_expseq, frame)) {
  | Some({ty_e, _}) =>
    //print_endline("get_expected_type_opseq returned ty_e");
    ty_e
  | _ => None
  //print_endline(Sexplib.Sexp.to_string_hum(sexp_of_t(frame)));
  //failwith("INVALID FRAME (get_nearest_opseq)");
  };
};

let get_cursor_term = (zexp: ZExp.t): option(slice_info) =>
  switch (get(zexp)) {
  | [si, ..._] => Some(si)
  | [] => None
  };

let get_expected_type_cursor_term = (zexp: ZExp.t): option(HTyp.t) => {
  let* slice = get_cursor_term(zexp);
  slice.ty_e;
};

let get_actual_type_cursor_term = (zexp: ZExp.t): option(HTyp.t) => {
  let* slice = get_cursor_term(zexp);
  slice.ty_a;
};

let get_opParent = (zexp: ZExp.t): option(ZExp.zoperand) => {
  let frame = get(zexp);
  let frame =
    switch (frame) {
    | [{slice: ExpOperand(_), _}, ...xs]
    | xs => xs
    };
  let is_expoperand = si =>
    switch (si.slice) {
    | ExpOperand(_) => true
    | _ => false
    };
  switch (List.find_opt(is_expoperand, frame)) {
  | Some({slice: ExpOperand(zoperand), _}) => Some(zoperand)
  | _ => None
  };
};

/*
 to think about: transformations for moving stuff around
 that might be simpler in terms of frame...
 like how about moving lines up or down to different blocks
 or operator-operand pairs to different opseqs?
 actually d's stuff i guess

 maybe still worth doing rezipping transformations...
 like, to replace most local opseq:
 split frame at most local opseq : [cursorterm .... x] old_opseq [y .... zexp]
 then go thru [zexp ... y], ignoring Z part, and inserting new opseq for Z part when get to end
 is this actually any simpler?
  */
