open Pretty;
let inline_padding_of_operator:
  UHExp.operator => (UHDoc_common.t, UHDoc_common.t) =
  fun
  | Space
  | Cons => (UHDoc_common.empty_, UHDoc_common.empty_)
  | Plus
  | Minus
  | FPlus
  | FMinus
  | Times
  | Divide
  | FTimes
  | FDivide
  | LessThan
  | GreaterThan
  | Equals
  | FLessThan
  | FGreaterThan
  | FEquals
  | And
  | Or => (UHDoc_common.space_, UHDoc_common.space_)
  | Comma => (UHDoc_common.empty_, UHDoc_common.space_);

let mk_EmptyHole: string => UHDoc_common.t =
  UHDoc_common.mk_EmptyHole(~sort=Exp);
let mk_InvalidText: string => UHDoc_common.t =
  UHDoc_common.mk_InvalidText(~sort=Exp);
let mk_IntLit: (~err: ErrStatus.t, string) => UHDoc_common.t =
  UHDoc_common.mk_IntLit(~sort=Exp);
let mk_FloatLit: (~err: ErrStatus.t, string) => UHDoc_common.t =
  UHDoc_common.mk_FloatLit(~sort=Exp);
let mk_BoolLit: (~err: ErrStatus.t, bool) => UHDoc_common.t =
  UHDoc_common.mk_BoolLit(~sort=Exp);
let mk_ListNil: (~err: ErrStatus.t, unit) => UHDoc_common.t =
  UHDoc_common.mk_ListNil(~sort=Exp);
let mk_Var:
  (~err: ErrStatus.t, ~verr: VarErrStatus.t, string) => UHDoc_common.t =
  UHDoc_common.mk_Var(~sort=Exp);
let mk_Parenthesized: UHDoc_common.formatted_child => UHDoc_common.t =
  UHDoc_common.mk_Parenthesized(~sort=Exp);
let mk_Inj:
  (~err: ErrStatus.t, ~inj_side: InjSide.t, UHDoc_common.formatted_child) =>
  UHDoc_common.t =
  UHDoc_common.mk_Inj(~sort=Exp);
let mk_NTuple =
    (
      ~mk_operand: (~enforce_inline: bool, 'a) => UHDoc_common.t,
      ~mk_operator: UHExp.operator => UHDoc_common.t,
      ~enforce_inline: bool,
      opseq: OpSeq.t('a, UHExp.operator),
    )
    : UHDoc_common.t =>
  UHDoc_common.mk_NTuple(
    ~sort=Exp,
    ~get_tuple_elements=UHExp.get_tuple_elements,
    ~inline_padding_of_operator,
    ~mk_operand,
    ~mk_operator,
    ~enforce_inline,
    ~check_livelit_skel=
      (seq, skel) =>
        LivelitUtil.check_livelit_skel(seq, skel) |> Option.map(fst),
    opseq,
  );

let livelit_handler = (~enforce_inline, go, seq, skel) =>
  switch (LivelitUtil.check_livelit_skel(seq, skel)) {
  | Some((ApLivelitData(llu, base_llname, llname, model, _), _)) =>
    // TODO(livelit definitions): thread ctx
    let ctx = Livelits.initial_livelit_view_ctx;
    switch (VarMap.lookup(ctx, base_llname)) {
    | None => assert(false)
    | Some((_, serialized_view_shape_fn)) =>
      let shape = serialized_view_shape_fn(model);
      let hd_step = Skel.leftmost_tm_index(skel);
      let llview_doc = {
        let spaceholder =
          switch (shape) {
          | Inline(width) => Doc.text(String.make(width, ' '))
          | MultiLine(height) =>
            enforce_inline
              ? Doc.fail()
              : Doc.hcats(ListUtil.replicate(height, Doc.linebreak()))
          };
        Doc.annot(
          UHAnnot.LivelitView({
            llu,
            base_llname,
            llname,
            shape,
            model,
            hd_step,
          }),
          spaceholder,
        );
      };
      Some(
        Doc.hcats([
          UHDoc_common.annot_LivelitExpression(~hd_index=hd_step, go(skel)),
          llview_doc,
        ]),
      );
    };
  | _ => None
  };

let annot_SubBlock =
    (~hd: UHExp.line, ~hd_index: int): (UHDoc_common.t => UHDoc_common.t) =>
  Doc.annot(
    UHAnnot.mk_Term(~sort=Exp, ~shape=SubBlock({hd, hd_index}), ()),
  );

let rec mk =
  lazy(
    UHDoc_common.memoize((~memoize: bool, ~enforce_inline: bool, e: UHExp.t) =>
      (Lazy.force(mk_block_0, ~memoize, ~enforce_inline, e): UHDoc_common.t)
    )
  )
// Two versions of `mk_block` so we can memoize them
and mk_block_0 =
  lazy(
    UHDoc_common.memoize(
      (~memoize: bool, ~enforce_inline: bool, block: UHExp.block) =>
      (mk_block(~offset=0, ~memoize, ~enforce_inline, block): UHDoc_common.t)
    )
  )
and mk_block_1 =
  lazy(
    UHDoc_common.memoize(
      (~memoize: bool, ~enforce_inline: bool, block: UHExp.block) =>
      (mk_block(~offset=1, ~memoize, ~enforce_inline, block): UHDoc_common.t)
    )
  )
and mk_block =
    (~offset: int, ~memoize, ~enforce_inline: bool, block: UHExp.block)
    : UHDoc_common.t =>
  if (enforce_inline && UHExp.Block.num_lines(block) > 1) {
    Doc.fail();
  } else {
    let (leading, (last, last_doc)) =
      block
      |> List.mapi((i, line) =>
           (
             line,
             Lazy.force(mk_line, ~memoize, ~enforce_inline, line)
             |> UHDoc_common.annot_Step(offset + i),
           )
         )
      |> ListUtil.split_last;

    ListUtil.fold_right_i(
      ((i, (hd, hd_doc)), tl_doc) =>
        annot_SubBlock(
          ~hd,
          ~hd_index=offset + i,
          Doc.vsep(
            hd_doc,
            UHDoc_common.annot_OpenChild(
              ~is_enclosed=true,
              ~is_inline=false,
              tl_doc,
            ),
          ),
        ),
      leading,
      annot_SubBlock(
        ~hd=last,
        ~hd_index=offset + UHExp.Block.num_lines(block) - 1,
        last_doc,
      ),
    );
  }
and mk_line =
  lazy(
    UHDoc_common.memoize(
      (~memoize: bool, ~enforce_inline: bool, line: UHExp.line) =>
      (
        switch (line) {
        | EmptyLine =>
          UHDoc_common.empty_
          |> Doc.annot(UHAnnot.mk_Token(~shape=Text(0), ~len=0, ()))
          |> Doc.annot(UHAnnot.EmptyLine)
        | ExpLine(opseq) =>
          Lazy.force(mk_opseq, ~memoize, ~enforce_inline, opseq)
        | AbbrevLine(lln_new, _, lln_old, args) =>
          let formattable = (i, arg, ~enforce_inline) =>
            arg
            |> Lazy.force(mk_operand, ~memoize, ~enforce_inline)
            |> UHDoc_common.annot_Step(i);
          let formatteds =
            args
            |> List.mapi((i, arg) =>
                 enforce_inline
                   ? UHDoc_common.EnforcedInline(
                       formattable(i, arg, ~enforce_inline=true),
                     )
                   : Unformatted(formattable(i, arg))
               );
          UHDoc_common.mk_AbbrevLine(lln_new, lln_old, formatteds)
          |> Doc.annot(UHAnnot.AbbrevLine);
        | LetLine(p, ann, def) =>
          let p =
            UHDoc_Pat.mk_child(~memoize, ~enforce_inline, ~child_step=0, p);
          let ann =
            ann
            |> OptUtil.map(ann =>
                 UHDoc_Typ.mk_child(
                   ~memoize,
                   ~enforce_inline,
                   ~child_step=1,
                   ann,
                 )
               );
          let def = mk_child(~memoize, ~enforce_inline, ~child_step=2, def);
          UHDoc_common.mk_LetLine(p, ann, def) |> Doc.annot(UHAnnot.LetLine);
        }: UHDoc_common.t
      )
    )
  )
and mk_opseq =
  lazy(
    UHDoc_common.memoize(
      (~memoize: bool, ~enforce_inline: bool, opseq: UHExp.opseq) =>
      (
        mk_NTuple(
          ~mk_operand=Lazy.force(mk_operand, ~memoize),
          ~mk_operator,
          ~enforce_inline,
          opseq,
        ): UHDoc_common.t
      )
    )
  )
and mk_operator = (op: UHExp.operator): UHDoc_common.t =>
  op |> Operators_Exp.is_Space
    ? UHDoc_common.mk_space_op
    : UHDoc_common.mk_op(Operators_Exp.to_string(op))
and mk_operand =
  lazy(
    UHDoc_common.memoize(
      (~memoize: bool, ~enforce_inline: bool, operand: UHExp.operand) =>
      (
        switch (operand) {
        | EmptyHole(u) => mk_EmptyHole(UHDoc_common.hole_lbl(u + 1))
        | InvalidText(_, t) => mk_InvalidText(t)
        | Var(err, verr, x) => mk_Var(~err, ~verr, x)
        | IntLit(err, n) => mk_IntLit(~err, n)
        | FloatLit(err, f) => mk_FloatLit(~err, f)
        | BoolLit(err, b) => mk_BoolLit(~err, b)
        | ListNil(err) => mk_ListNil(~err, ())
        | Lam(err, p, ann, body) =>
          let p =
            UHDoc_Pat.mk_child(~memoize, ~enforce_inline, ~child_step=0, p);
          let ann =
            ann
            |> OptUtil.map(ann =>
                 UHDoc_Typ.mk_child(
                   ~memoize,
                   ~enforce_inline,
                   ~child_step=1,
                   ann,
                 )
               );
          let body = mk_child(~memoize, ~enforce_inline, ~child_step=2, body);
          UHDoc_common.mk_Lam(~err, p, ann, body);
        | Inj(err, inj_side, body) =>
          let body = mk_child(~memoize, ~enforce_inline, ~child_step=0, body);
          mk_Inj(~err, ~inj_side, body);
        | Parenthesized(body) =>
          let body = mk_child(~memoize, ~enforce_inline, ~child_step=0, body);
          mk_Parenthesized(body);
        | Case(err, scrut, rules) =>
          if (enforce_inline) {
            Doc.fail();
          } else {
            let scrut =
              mk_child(~memoize, ~enforce_inline=false, ~child_step=0, scrut);
            let rules =
              rules
              |> List.mapi((i, rule) =>
                   Lazy.force(mk_rule, ~memoize, ~enforce_inline, rule)
                   |> UHDoc_common.annot_Step(1 + i)
                 );
            UHDoc_common.mk_Case(~err, scrut, rules);
          }
        | FreeLivelit(_, lln) => UHDoc_common.mk_FreeLivelit(lln)
        | ApLivelit(_, _, _, lln, _, _) => UHDoc_common.mk_ApLivelit(lln)
        }: UHDoc_common.t
      )
    )
  )
and mk_rule =
  lazy(
    UHDoc_common.memoize(
      (
        ~memoize: bool,
        ~enforce_inline as _: bool,
        Rule(p, clause): UHExp.rule,
      ) =>
      (
        {
          let p =
            UHDoc_Pat.mk_child(
              ~memoize,
              ~enforce_inline=false,
              ~child_step=0,
              p,
            );
          let clause =
            mk_child(~memoize, ~enforce_inline=false, ~child_step=1, clause);
          UHDoc_common.mk_Rule(p, clause);
        }: UHDoc_common.t
      )
    )
  )
and mk_child =
    (~memoize: bool, ~enforce_inline: bool, ~child_step: int, e: UHExp.t)
    : UHDoc_common.formatted_child => {
  switch (e) {
  | [EmptyLine, ...subblock] =>
    if (enforce_inline) {
      EnforcedInline(Doc.fail());
    } else {
      let formatted =
        Lazy.force(mk_block_1, ~memoize, ~enforce_inline=false, subblock)
        |> UHDoc_common.annot_Step(child_step);
      UserNewline(formatted);
    }
  | _ =>
    let formattable = (~enforce_inline) =>
      Lazy.force(mk, ~memoize, ~enforce_inline, e)
      |> UHDoc_common.annot_Step(child_step);
    enforce_inline
      ? EnforcedInline(formattable(~enforce_inline=true))
      : Unformatted(formattable);
  };
};

let mk_splices = (e: UHExp.t): UHDoc_common.splices => {
  let rec mk_block = (~splices, block: UHExp.block): UHDoc_common.splices =>
    block
    |> List.fold_left((splices, line) => mk_line(~splices, line), splices)
  and mk_line = (~splices, line: UHExp.line): UHDoc_common.splices =>
    switch (line) {
    | EmptyLine => splices
    | ExpLine(opseq) => mk_opseq(~splices, opseq)
    | AbbrevLine(_, _, _, args) =>
      args
      |> List.fold_left(
           (splices, operand) => mk_operand(~splices, operand),
           splices,
         )
    | LetLine(_, _, def) => mk_block(~splices, def)
    }
  and mk_opseq = (~splices, OpSeq(_, seq): UHExp.opseq): UHDoc_common.splices =>
    Seq.operands(seq)
    |> List.fold_left(
         (splices, operand) => mk_operand(~splices, operand),
         splices,
       )
  and mk_operand = (~splices, operand: UHExp.operand): UHDoc_common.splices =>
    switch (operand) {
    | EmptyHole(_)
    | Var(_)
    | InvalidText(_)
    | IntLit(_)
    | FloatLit(_)
    | BoolLit(_)
    | ListNil(_)
    | FreeLivelit(_) => splices
    | ApLivelit(llu, _, _, _, _, splice_info) =>
      let ap_splices =
        splice_info.splice_map
        |> IntMap.bindings
        |> List.to_seq
        |> SpliceMap.ApMap.of_seq;
      let (ap_docs, splices) =
        SpliceMap.ApMap.fold(
          (splice_name, (_, splice_e), (ap_docs, splices)) =>
            (
              ap_docs
              |> SpliceMap.ApMap.add(
                   splice_name,
                   Lazy.force(
                     mk,
                     ~memoize=false,
                     ~enforce_inline=false,
                     splice_e,
                   ),
                 ),
              mk_block(~splices, splice_e),
            ),
          ap_splices,
          (SpliceMap.ApMap.empty, splices),
        );
      splices |> SpliceMap.put_ap(llu, ap_docs);
    | Lam(_, _, _, e)
    | Inj(_, _, e)
    | Parenthesized(e) => mk_block(~splices, e)
    | Case(_, scrut, rules) =>
      let splices = mk_block(~splices, scrut);
      rules
      |> List.fold_left(
           (splices, UHExp.Rule(_, e)) => mk_block(~splices, e),
           splices,
         );
    };

  mk_block(~splices=SpliceMap.empty, e);
};
