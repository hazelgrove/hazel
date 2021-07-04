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

let mk_typ_slice = (~slice): slice_info =>
  mk(~slice, ~ctx=Contexts.empty, ~ty_e=None, ~ty_a=None);

[@deriving sexp]
type t = list(slice_info);

let syn = (~ctx, slice) =>
  switch (slice) {
  | ExpBlock(zblock) => zblock |> ZExp.erase |> Statics_Exp.syn_block(ctx)
  | ExpLine(_) => None
  | ExpSeq(zopseq) =>
    zopseq
    |> ZExp.erase_zopseq
    |> UHExp.set_err_status_opseq(NotInHole)
    |> Statics_Exp.syn_opseq(ctx)
  | ExpOperand(zoperand) =>
    zoperand
    |> ZExp.erase_zoperand
    |> UHExp.set_err_status_operand(NotInHole)
    |> Statics_Exp.syn_operand(ctx)
  | ExpOperator(_zoperator) => None // TODO(andrew)
  | ExpRules(zrules) =>
    // last arg was scrut_ty  but not sure that makes sense...
    Statics_Exp.syn_rules(ctx, ZExp.erase_zrules(zrules), HTyp.Hole)
  | ExpRule(zrule) =>
    Statics_Exp.syn_rule(ctx, ZExp.erase_zrule(zrule), HTyp.Hole)
  | PatSeq(zopseq) =>
    zopseq
    |> ZPat.erase_zopseq
    |> UHPat.set_err_status_opseq(NotInHole)
    |> Statics_Pat.syn_opseq(ctx)
    |> Option.map(((ty, _)) => ty)
  | PatOperand(zoperand) =>
    zoperand
    |> ZPat.erase_zoperand
    |> UHPat.set_err_status_operand(NotInHole)
    |> Statics_Pat.syn_operand(ctx)
    |> Option.map(((ty, _)) => ty)
  | PatOperator(_zoperator) => None // TODO(andrew)
  | TypSeq(_)
  | TypOperand(_)
  | TypOperator(_) => None
  };

let ana = (~ctx, ~ty_e, slice: slice) =>
  switch (slice) {
  | ExpOperand(CursorE(_))
  | ExpOperand(ApPaletteZ(_))
  | PatOperand(CursorP(_)) => None
  | ExpOperand(ParenthesizedZ(_))
  | PatOperand(ParenthesizedZ(_)) => ty_e
  | ExpOperand(InjZ(_, side, _)) =>
    let* ty = ty_e;
    let+ (ty1, ty2) = HTyp.matched_sum(ty);
    InjSide.pick(side, ty1, ty2);
  | PatOperand(InjZ(_, side, _)) =>
    let* ty = ty_e;
    let+ (ty1, ty2) = HTyp.matched_sum(ty);
    InjSide.pick(side, ty1, ty2);
  | ExpOperand(LamZE(_)) =>
    let* ty_e' = ty_e;
    let+ (_, ty_body) = HTyp.matched_arrow(ty_e');
    ty_body;
  | PatOperand(TypeAnnZP(_, _, ann)) => Some(UHTyp.expand(ann))
  | PatOperand(TypeAnnZA(_, _, _)) => None
  | ExpOperand(CaseZE(_)) => None
  | ExpOperand(LamZP(_)) => None
  | ExpOperand(CaseZR(_)) =>
    // NOTE special case with scrut type...
    // rn this is only the expected type of the clauses
    ty_e

  | ExpSeq(ZOpSeq(_, ZOperand(_, (prefix, _))) as zopseq) =>
    let opseq = ZExp.erase_zopseq(zopseq);
    let operand_index = Seq.length_of_affix(prefix);
    switch (ty_e) {
    | Some(ty_e) =>
      switch (Statics_Exp.ana_nth_type_mode(ctx, operand_index, opseq, ty_e)) {
      | Some(Ana(ty)) => Some(ty)
      | Some(Syn) => Some(HTyp.Hole)
      | None => None
      }
    | None =>
      switch (Statics_Exp.syn_nth_type_mode(ctx, operand_index, opseq)) {
      | Some(Ana(ty)) => Some(ty)
      | Some(Syn) => Some(HTyp.Hole)
      | _ => None
      }
    };
  | PatSeq(ZOpSeq(_, ZOperand(_, (prefix, _))) as zopseq) =>
    let opseq = ZPat.erase_zopseq(zopseq);
    let operand_index = Seq.length_of_affix(prefix);
    switch (ty_e) {
    | Some(ty_e) =>
      switch (Statics_Pat.ana_nth_type_mode(ctx, operand_index, opseq, ty_e)) {
      | Some(Ana(ty)) => Some(ty)
      | Some(Syn) => Some(HTyp.Hole)
      | None => None
      }
    | _ => None
    };
  | ExpSeq(ZOpSeq(_, ZOperator(_zop, _))) =>
    //TODO(andrew): FIX this is wrong
    None
  | PatSeq(ZOpSeq(_, ZOperator(_))) =>
    // TODO(andrew): fix ty_e!!!! adapt syn/ana_nth_type_mode to return operator types
    None

  | _ => None
  };

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
  let slice =
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
  // last line gets type
  let tail =
    get_frame_zline(~ctx=ctx', ~ty_e=suffix == [] ? ty_e : None, zline);
  [slice, ...tail];
}
and get_frame_zline =
    (~ctx: Contexts.t, ~ty_e: option(HTyp.t), zline: ZExp.zline): t => {
  //P.p("get_frame_exp_zline ty:%s\n", sexp_of_option(HTyp.sexp_of_t, ty_e));
  let slice = mk(~ctx, ~slice=ExpLine(zline), ~ty_e, ~ty_a=None);
  let tail =
    switch (zline) {
    | CursorL(_, ExpLine(opseq)) => [
        mk(
          ~ctx,
          ~slice=ExpLine(zline),
          ~ty_e,
          ~ty_a=Statics_Exp.syn_opseq(ctx, opseq),
        ),
      ]
    | CursorL(_) => []
    | ExpLineZ(zopseq) => get_frame_exp_zopseq(~ctx, ~ty_e, zopseq)
    | LetLineZE(p, zblock) =>
      let def = ZExp.erase(zblock);
      let def_ctx = Statics_Exp.extend_let_def_ctx(ctx, p, def);
      let ty_p =
        switch (Statics_Pat.syn(def_ctx, p)) {
        | Some((ty, _)) => Some(ty)
        | None => Some(HTyp.Hole)
        };
      //let body_ctx = Statics_Exp.extend_let_body_ctx(ctx, p, def);
      get_frame_zblock(~ctx=def_ctx, ~ty_e=ty_p, zblock);
    | LetLineZP(zpat, _) => get_frame_pat(~ctx, ~ty_e=None, zpat)
    };
  [slice, ...tail];
}
and get_frame_exp_zopseq =
    (
      ~ctx: Contexts.t,
      ~ty_e: option(HTyp.t),
      ZOpSeq(_skel, zseq) as zopseq: ZExp.zopseq,
    )
    : t => {
  //P.p("get_frame_exp_zopseq ty:%s\n", sexp_of_option(HTyp.sexp_of_t, ty_e));
  let slice = ExpSeq(zopseq);
  let ty_a = syn(~ctx, slice);
  let ty_e_new = ana(~ctx, ~ty_e, slice);
  let slice = mk(~ctx, ~slice, ~ty_e, ~ty_a);
  let tail =
    switch (zseq) {
    | ZOperand(zoperand, _) =>
      get_frame_exp_zoperand(~ctx, ~ty_e=ty_e_new, zoperand)
    | ZOperator(zoperator, _) =>
      get_frame_exp_zoperator(~ctx, ~ty_e=ty_e_new, zoperator)
    };
  [slice, ...tail];
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
  let slice = ExpOperand(zoperand);
  let ty_a = syn(~ctx, slice);
  let si = mk(~ctx, ~slice, ~ty_e, ~ty_a);
  let ty_e_new = ana(~ctx, ~ty_e, slice);
  let tail =
    switch (zoperand) {
    | CursorE(_) => []
    | ApPaletteZ(_) => []
    | ParenthesizedZ(zexp) => get_frame(~ctx, ~ty_e=ty_e_new, zexp)
    | LamZE(_, p, zexp) =>
      let ctx_body =
        switch (
          {
            let* ty_e' = ty_e;
            let* (ty_p_given, _) = HTyp.matched_arrow(ty_e');
            Statics_Pat.ana(ctx, p, ty_p_given);
          }
        ) {
        | None => ctx
        | Some(ctx) => ctx
        };
      get_frame(~ctx=ctx_body, ~ty_e, zexp);
    | InjZ(_, _, zexp) => get_frame(~ctx, ~ty_e=ty_e_new, zexp)
    | CaseZE(_, zexp, _) => get_frame(~ctx, ~ty_e=ty_e_new, zexp)
    //TODO: consider integrating pattern types into ty_e
    | LamZP(_, zpat, _) => get_frame_pat(~ctx, ~ty_e=ty_e_new, zpat)
    | CaseZR(_, scrut, zrules) =>
      //TODO: special case type situations?
      let ty_scrut = Statics_Exp.syn(ctx, scrut);
      get_frame_zrules(~ctx, ~ty_e=ty_e_new, ~ty_scrut, zrules);
    };
  [si, ...tail];
}
and get_frame_zrules =
    (
      ~ctx: Contexts.t,
      ~ty_e: option(HTyp.t),
      ~ty_scrut: option(HTyp.t),
      (_, zrule, _) as zrules: ZExp.zrules,
    )
    : t => {
  let slice = ExpRules(zrules);
  let ty_a = syn(~ctx, slice);
  let slice = mk(~ctx, ~slice, ~ty_a, ~ty_e=None);
  let tail = get_frame_zrule(~ctx, ~ty_e, ~ty_scrut, zrule);
  [slice, ...tail];
}
and get_frame_zrule =
    (
      ~ctx: Contexts.t,
      ~ty_e: option(HTyp.t),
      ~ty_scrut: option(HTyp.t),
      zrule: ZExp.zrule,
    )
    : t => {
  let slice = ExpRule(zrule);
  let ty_a = syn(~ctx, slice);
  let slice = mk(~ctx, ~slice, ~ty_a, ~ty_e);
  let tail =
    switch (zrule) {
    | CursorR(_) => []
    | RuleZP(zpat, _) => get_frame_pat(~ctx, ~ty_e=ty_scrut, zpat)
    | RuleZE(_, zexp) => get_frame(~ctx, ~ty_e, zexp)
    };
  [slice, ...tail];
}
and rec_get_frame_pat = (ZOpSeq(_, zseq): ZPat.zopseq) =>
  switch (zseq) {
  | ZOperand(zop1, _) => get_frame_pat_zoperand(zop1)
  | ZOperator(zop2, _) => get_frame_pat_zoperator(zop2)
  }
and get_frame_pat =
    (
      ~ctx: Contexts.t,
      ~ty_e: option(HTyp.t),
      ZOpSeq(_, _) as zopseq: ZPat.zopseq,
    )
    : t => {
  let slice = PatSeq(zopseq);
  let ty_a = syn(~ctx, slice);
  let si = mk(~ctx, ~slice, ~ty_e, ~ty_a);
  let ty_e_new = ana(~ctx, ~ty_e, slice);
  let tail = rec_get_frame_pat(zopseq, ~ctx, ~ty_e=ty_e_new);
  [si, ...tail];
}
and get_frame_pat_zoperator = // TODO(andrew): fix
    (~ctx: Contexts.t, ~ty_e as _: option(HTyp.t), zop: ZPat.zoperator): t => {
  [mk(~ctx, ~slice=PatOperator(zop), ~ty_e=None, ~ty_a=None)];
}
and get_frame_pat_zoperand =
    (~ctx: Contexts.t, ~ty_e: option(HTyp.t), zoperand: ZPat.zoperand): t => {
  let slice = PatOperand(zoperand);
  let ty_a = syn(~ctx, slice);
  let si = mk(~ctx, ~slice, ~ty_a, ~ty_e);
  let ty_e_new = ana(~ctx, ~ty_e, slice);
  let tail =
    switch (zoperand) {
    | CursorP(_) => []
    | ParenthesizedZ(zpat) => get_frame_pat(~ctx, ~ty_e=ty_e_new, zpat)
    | InjZ(_, _, zpat) => get_frame_pat(~ctx, ~ty_e=ty_e_new, zpat)
    | TypeAnnZA(_, _, ty_zopseq) =>
      get_frame_typ(~ctx, ~ty_e=ty_e_new, ty_zopseq)
    | TypeAnnZP(_, zoperand, _) =>
      get_frame_pat_zoperand(~ctx, ~ty_e=ty_e_new, zoperand)
    };
  [si, ...tail];
}
and get_frame_typ =
    (
      ~ctx as _: Contexts.t,
      ~ty_e as _: option(HTyp.t),
      ZOpSeq(_, zseq) as zopseq: ZTyp.zopseq,
    )
    : t => {
  cons(
    mk_typ_slice(~slice=TypSeq(zopseq)),
    switch (zseq) {
    | ZOperand(zop, _) => get_frame_typ_zoperand(zop)
    | ZOperator(zop, _) => get_frame_typ_zoperator(zop)
    },
  );
}
and get_frame_typ_zoperator = (zoperator: ZTyp.zoperator): t => {
  [mk_typ_slice(~slice=TypOperator(zoperator))];
}
and get_frame_typ_zoperand = (zoperand: ZTyp.zoperand): t => {
  [mk_typ_slice(~slice=TypOperand(zoperand))];
};

let get = (zexp: ZExp.t): t =>
  zexp |> get_frame(~ctx=Contexts.empty, ~ty_e=Some(Hole)) |> List.rev;

// *****************************************************************

let get_cursor_slice = (zexp: ZExp.t): option(slice_info) =>
  switch (get(zexp)) {
  | [si, ..._] => Some(si)
  | [] => None
  };

let first_exp_operand = si =>
  switch (si.slice) {
  | ExpOperand(zop) => Some(zop)
  | _ => None
  };

let first_exp_seq_zopseq = si =>
  switch (si.slice) {
  | ExpSeq(zopseq) => Some(zopseq)
  | _ => None
  };

let first_exp_seq_ty_e = si =>
  switch (si) {
  | {slice: ExpSeq(_), ty_e, _} => ty_e
  | _ => None
  };

// omcaml 4.10.0 sneak peal:
let rec find_map = (f: 'a => option('b), xs: list('a)): option('b) => {
  switch (xs) {
  | [] => None
  | [x, ...xs'] =>
    switch (f(x)) {
    | None => find_map(f, xs')
    | x => x
    }
  };
};

let pop_exp_operand = frame =>
  switch (frame) {
  | [{slice: ExpOperand(_), _}, ...xs]
  | xs => xs
  };

let get_opParent = (zexp: ZExp.t): option(ZExp.zoperand) =>
  // skip cursor_term if it's an operand
  zexp |> get |> pop_exp_operand |> find_map(first_exp_operand);

let enclosing_zopseq = (zexp: ZExp.t): option(ZExp.zopseq) =>
  zexp |> get |> find_map(first_exp_seq_zopseq);

let enclosing_zopseq_expected_ty = (zexp: ZExp.t): option(HTyp.t) =>
  zexp |> get |> find_map(first_exp_seq_ty_e);

let get_expected_type_cursor_term = (zexp: ZExp.t): option(HTyp.t) => {
  let* slice = get_cursor_slice(zexp);
  slice.ty_e;
};

let get_actual_type_cursor_term = (zexp: ZExp.t): option(HTyp.t) => {
  let* slice = get_cursor_slice(zexp);
  slice.ty_a;
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
