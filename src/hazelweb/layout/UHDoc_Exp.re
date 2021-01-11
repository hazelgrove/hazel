module Doc = Pretty.Doc;

let inline_padding_of_operator: UHExp.operator => (UHDoc.t, UHDoc.t) =
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
  | Or
  | Caret => (UHDoc_common.space_, UHDoc_common.space_)
  | Comma => (UHDoc_common.empty_, UHDoc_common.space_);

let mk_EmptyHole: string => UHDoc.t = UHDoc_common.mk_EmptyHole(~sort=Exp);
let mk_InvalidText: string => UHDoc.t =
  UHDoc_common.mk_InvalidText(~sort=Exp);
let mk_IntLit: string => UHDoc.t = UHDoc_common.mk_IntLit(~sort=Exp);
let mk_FloatLit: string => UHDoc.t = UHDoc_common.mk_FloatLit(~sort=Exp);
let mk_BoolLit: bool => UHDoc.t = UHDoc_common.mk_BoolLit(~sort=Exp);
let mk_StringLit: string => UHDoc.t = UHDoc_common.mk_StringLit(~sort=Exp);
let mk_ListNil: unit => UHDoc.t = UHDoc_common.mk_ListNil(~sort=Exp);
let mk_Var: string => UHDoc.t = UHDoc_common.mk_Var(~sort=Exp);
let mk_Parenthesized: UHDoc_common.formatted_child => UHDoc.t =
  UHDoc_common.mk_Parenthesized(~sort=Exp);
let mk_Inj: (~inj_side: InjSide.t, UHDoc_common.formatted_child) => UHDoc.t =
  UHDoc_common.mk_Inj(~sort=Exp);
let mk_NTuple =
    (
      ~mk_operand: (~enforce_inline: bool, 'a) => UHDoc.t,
      ~mk_operator: UHExp.operator => UHDoc.t,
      ~enforce_inline: bool,
      ~llview_ctx: Statics.livelit_web_view_ctx,
      opseq: OpSeq.t('a, UHExp.operator),
    )
    : UHDoc.t =>
  UHDoc_common.mk_NTuple(
    ~sort=Exp,
    ~get_tuple_elements=UHExp.get_tuple_elements,
    ~inline_padding_of_operator,
    ~mk_operand,
    ~mk_operator,
    ~enforce_inline,
    ~llview_ctx,
    ~check_livelit_skel=
      (seq, skel) =>
        LivelitUtil.check_livelit_skel(seq, skel) |> Option.map(fst),
    opseq,
  );

let livelit_handler = (~enforce_inline, go, seq, skel) =>
  switch (LivelitUtil.check_livelit_skel(seq, skel)) {
  | Some((ApLivelitData(llu, base_llname, llname, model, _), _)) =>
    // TODO(livelit definitions): thread ctx?
    let ctx = Livelits.initial_livelit_view_ctx;
    switch (VarMap.lookup(ctx, base_llname)) {
    | None => assert(false)
    | Some((_, serialized_view_shape_fn)) =>
      let shape = serialized_view_shape_fn(model);
      let hd_step = Skel.leftmost_tm_index(skel);
      let llview_doc = {
        let spaceholder =
          switch (shape) {
          | InvalidShape => Doc.text("Invalid Shape")
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
          UHDoc_common.annot_LivelitExpression(
            ~hd_index=hd_step,
            ~view_shape=shape,
            go(skel),
          ),
          llview_doc,
        ]),
      );
    };
  | _ => None
  };

let annot_SubBlock = (~hd_index: int): (UHDoc.t => UHDoc.t) =>
  Doc.annot(
    UHAnnot.mk_Term(~sort=Exp, ~shape=SubBlock({hd_index: hd_index}), ()),
  );

type llview_ctx = Statics.livelit_web_view_ctx;

let rec mk =
  lazy(
    UHDoc_common.memoize(
      (~memoize: bool, ~enforce_inline: bool, ctx_e: (llview_ctx, UHExp.t)) =>
      (Lazy.force(mk_block_0, ~memoize, ~enforce_inline, ctx_e): UHDoc.t)
    )
  )
and mk_doc = (expr, child_step) => {
  Lazy.force(
    mk,
    ~memoize=false,
    ~enforce_inline=false,
    // TODO livelit view ctx?
    (IntMap.empty, expr),
  )
  |> UHDoc_common.annot_Step(child_step);
}
// Two versions of `mk_block` so we can memoize them
and mk_block_0 =
  lazy(
    UHDoc_common.memoize(
      (
        ~memoize: bool,
        ~enforce_inline: bool,
        ctx_block: (llview_ctx, UHExp.block),
      ) =>
      (mk_block(~offset=0, ~memoize, ~enforce_inline, ctx_block): UHDoc.t)
    )
  )
and mk_block_1 =
  lazy(
    UHDoc_common.memoize(
      (
        ~memoize: bool,
        ~enforce_inline: bool,
        ctx_block: (llview_ctx, UHExp.block),
      ) =>
      (mk_block(~offset=1, ~memoize, ~enforce_inline, ctx_block): UHDoc.t)
    )
  )
and mk_block =
    (
      ~offset: int,
      ~memoize,
      ~enforce_inline: bool,
      (llview_ctx, block: UHExp.block),
    )
    : UHDoc.t =>
  if (enforce_inline && UHExp.Block.num_lines(block) > 1) {
    Doc.fail();
  } else {
    let (leading, (_, last_doc)) =
      block
      |> List.mapi((i, line) =>
           (
             line,
             Lazy.force(
               mk_line,
               ~memoize,
               ~enforce_inline,
               (llview_ctx, line),
             )
             |> UHDoc_common.annot_Step(offset + i),
           )
         )
      |> ListUtil.split_last;

    ListUtil.fold_right_i(
      ((i, (hd, hd_doc)), tl_doc) =>
        switch ((hd: UHExp.line)) {
        | EmptyLine
        | CommentLine(_)
        | ExpLine(_) => Doc.vsep(hd_doc, tl_doc)
        | AbbrevLine(_)
        | LivelitDefLine(_)
        | LetLine(_) =>
          annot_SubBlock(
            ~hd_index=offset + i,
            Doc.vsep(
              hd_doc,
              Doc.annot(UHAnnot.OpenChild(Multiline), tl_doc),
            ),
          )
        },
      leading,
      last_doc,
    );
  }
and mk_line =
  lazy(
    UHDoc_common.memoize(
      (~memoize: bool, ~enforce_inline: bool, (llview_ctx, line: UHExp.line)) =>
      (
        switch (line) {
        | EmptyLine =>
          UHDoc_common.empty_
          |> Doc.annot(
               UHAnnot.mk_Token(~shape=Text({start_index: 0}), ~len=0, ()),
             )
        | CommentLine(comment) =>
          let comment_doc =
            UHDoc_common.mk_text(comment)
            |> Doc.annot(
                 UHAnnot.mk_Token(
                   ~shape=Text({start_index: 0}),
                   ~len=StringUtil.utf8_length(comment),
                   (),
                 ),
               );
          Doc.hcats([
            UHDoc_common.Delim.open_CommentLine(),
            UHDoc_common.space_,
            comment_doc,
          ])
          |> Doc.annot(UHAnnot.CommentLine);
        | ExpLine(opseq) =>
          Lazy.force(
            mk_opseq,
            ~memoize,
            ~enforce_inline,
            (llview_ctx, opseq),
          )
        | AbbrevLine(lln_new, _, lln_old, args) =>
          let formattable = (i, arg, ~enforce_inline) =>
            (llview_ctx, arg)
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
          UHDoc_common.mk_AbbrevLine(lln_new, lln_old, formatteds);
        | LivelitDefLine({
            name: (_, name_str),
            expansion_type,
            captures,
            model_type,
            action_type,
            init,
            update,
            view,
            shape,
            expand,
          }) =>
          UHDoc_common.mk_LivelitDefLine(
            name_str,
            UHDoc_Typ.mk_doc(expansion_type, 1),
            mk_doc(captures, 2),
            UHDoc_Typ.mk_doc(model_type, 3),
            UHDoc_Typ.mk_doc(action_type, 4),
            mk_doc(init, 5),
            mk_doc(update, 6),
            mk_doc(view, 7),
            mk_doc(shape, 8),
            mk_doc(expand, 9),
          )
        | LetLine(p, ann, def) =>
          let p =
            UHDoc_Pat.mk_child(~memoize, ~enforce_inline, ~child_step=0, p);
          let ann =
            ann
            |> Option.map(ann =>
                 UHDoc_Typ.mk_child(
                   ~memoize,
                   ~enforce_inline,
                   ~child_step=1,
                   ann,
                 )
               );
          let def =
            mk_child(
              ~memoize,
              ~enforce_inline,
              ~child_step=2,
              (llview_ctx, def),
            );
          UHDoc_common.mk_LetLine(p, ann, def);
        }: UHDoc.t
      )
    )
  )
and mk_opseq =
  lazy(
    UHDoc_common.memoize(
      (
        ~memoize: bool,
        ~enforce_inline: bool,
        (llview_ctx, opseq: UHExp.opseq),
      ) =>
      (
        mk_NTuple(
          ~mk_operand=
            (~enforce_inline, operand) =>
              Lazy.force(
                mk_operand,
                ~memoize,
                ~enforce_inline,
                (llview_ctx, operand),
              ),
          ~mk_operator,
          ~enforce_inline,
          ~llview_ctx,
          opseq,
        ): UHDoc.t
      )
    )
  )
and mk_operator = (op: UHExp.operator): UHDoc.t =>
  op |> Operators_Exp.is_Space
    ? UHDoc_common.mk_space_op
    : UHDoc_common.mk_op(Operators_Exp.to_string(op))
and mk_operand =
  lazy(
    UHDoc_common.memoize(
      (
        ~memoize: bool,
        ~enforce_inline: bool,
        (llview_ctx, operand: UHExp.operand),
      ) =>
      (
        switch (operand) {
        | EmptyHole(u) => mk_EmptyHole(UHDoc_common.hole_lbl(u + 1))
        | InvalidText(_, t) => mk_InvalidText(t)
        | Var(_, _, x) => mk_Var(x)
        | IntLit(_, n) => mk_IntLit(n)
        | FloatLit(_, f) => mk_FloatLit(f)
        | BoolLit(_, b) => mk_BoolLit(b)
        | StringLit(_, s) => mk_StringLit(s)
        | ListNil(_) => mk_ListNil()
        | Lam(_, p, ann, body) =>
          let p =
            UHDoc_Pat.mk_child(~memoize, ~enforce_inline, ~child_step=0, p);
          let ann =
            ann
            |> Option.map(ann =>
                 UHDoc_Typ.mk_child(
                   ~memoize,
                   ~enforce_inline,
                   ~child_step=1,
                   ann,
                 )
               );
          let body =
            mk_child(
              ~memoize,
              ~enforce_inline,
              ~child_step=2,
              (llview_ctx, body),
            );
          UHDoc_common.mk_Lam(p, ann, body);
        | Inj(_, inj_side, body) =>
          let body =
            mk_child(
              ~memoize,
              ~enforce_inline,
              ~child_step=0,
              (llview_ctx, body),
            );
          mk_Inj(~inj_side, body);
        | Parenthesized(body) =>
          let body =
            mk_child(
              ~memoize,
              ~enforce_inline,
              ~child_step=0,
              (llview_ctx, body),
            );
          mk_Parenthesized(body);
        | Case(_, scrut, rules) =>
          if (enforce_inline) {
            Doc.fail();
          } else {
            let scrut =
              mk_child(
                ~memoize,
                ~enforce_inline=false,
                ~child_step=0,
                (llview_ctx, scrut),
              );
            let rules =
              rules
              |> List.mapi((i, rule) =>
                   Lazy.force(
                     mk_rule,
                     ~memoize,
                     ~enforce_inline,
                     (llview_ctx, rule),
                   )
                   |> UHDoc_common.annot_Step(1 + i)
                 );
            UHDoc_common.mk_Case(scrut, rules);
          }
        | FreeLivelit(_, lln) => UHDoc_common.mk_FreeLivelit(lln)
        | ApLivelit(_, _, _, lln, _, _) => UHDoc_common.mk_ApLivelit(lln)
        | Subscript(_, target, start_, end_) =>
          let target =
            mk_child(
              ~memoize,
              ~enforce_inline,
              ~child_step=0,
              (llview_ctx, target),
            );
          let start_ =
            mk_child(
              ~memoize,
              ~enforce_inline,
              ~child_step=1,
              (llview_ctx, start_),
            );
          let end_ =
            mk_child(
              ~memoize,
              ~enforce_inline,
              ~child_step=2,
              (llview_ctx, end_),
            );
          UHDoc_common.mk_Subscript(target, start_, end_);
        }: UHDoc.t
      )
    )
  )
and mk_rule =
  lazy(
    UHDoc_common.memoize(
      (
        ~memoize: bool,
        ~enforce_inline as _: bool,
        (llview_ctx, Rule(p, clause): UHExp.rule),
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
            mk_child(
              ~memoize,
              ~enforce_inline=false,
              ~child_step=1,
              (llview_ctx, clause),
            );
          UHDoc_common.mk_Rule(p, clause);
        }: UHDoc.t
      )
    )
  )
and mk_child =
    (
      ~memoize: bool,
      ~enforce_inline: bool,
      ~child_step: int,
      (llview_ctx, e: UHExp.t),
    )
    : UHDoc_common.formatted_child => {
  switch (e) {
  | [EmptyLine, ...subblock] =>
    if (enforce_inline) {
      EnforcedInline(Doc.fail());
    } else {
      let formatted =
        Lazy.force(
          mk_block_1,
          ~memoize,
          ~enforce_inline=false,
          (llview_ctx, subblock),
        )
        |> UHDoc_common.annot_Step(child_step);
      UserNewline(formatted);
    }
  | _ =>
    let formattable = (~enforce_inline) =>
      Lazy.force(mk, ~memoize, ~enforce_inline, (llview_ctx, e))
      |> UHDoc_common.annot_Step(child_step);
    enforce_inline
      ? EnforcedInline(formattable(~enforce_inline=true))
      : Unformatted(formattable);
  };
};

let mk_splices = (llview_ctx, e: UHExp.t): UHDoc.splices => {
  let union = MetaVarMap.union((_, _, _) => None);
  let union_join = List.fold_left(union, SpliceMap.empty);
  let rec mk_block = (block: UHExp.block): UHDoc.splices =>
    block |> List.map(mk_line) |> union_join
  and mk_line = (line: UHExp.line): UHDoc.splices =>
    switch (line) {
    | EmptyLine
    | CommentLine(_) => SpliceMap.empty
    | ExpLine(opseq) => mk_opseq(opseq)
    | AbbrevLine(_, _, _, args) => args |> List.map(mk_operand) |> union_join
    | LivelitDefLine({captures, init, update, view, shape, expand, _}) =>
      [captures, init, update, view, shape, expand]
      |> List.map(mk_block)
      |> union_join
    | LetLine(_, _, def) => mk_block(def)
    }
  and mk_opseq = (OpSeq(_, seq): UHExp.opseq): UHDoc.splices =>
    Seq.operands(seq) |> List.map(mk_operand) |> union_join
  and mk_operand = (operand: UHExp.operand): UHDoc.splices =>
    switch (operand) {
    | EmptyHole(_)
    | Var(_)
    | InvalidText(_)
    | IntLit(_)
    | FloatLit(_)
    | BoolLit(_)
    | StringLit(_)
    | ListNil(_)
    | FreeLivelit(_) => SpliceMap.empty
    | ApLivelit(llu, _, _, _, _, splice_info) =>
      let ap_docs =
        splice_info.splice_map
        |> IntMap.bindings
        |> List.to_seq
        |> SpliceMap.ApMap.of_seq
        |> SpliceMap.ApMap.map(((_, splice_e: UHExp.t)) =>
             Lazy.force(
               mk,
               ~memoize=false,
               ~enforce_inline=false,
               (llview_ctx, splice_e),
             )
           );
      let ap_splices =
        splice_info.splice_map
        |> IntMap.bindings
        |> List.map(((_, (_, splice_e))) => mk_block(splice_e))
        |> union_join;
      SpliceMap.put_ap(llu, ap_docs, ap_splices);
    | Lam(_, _, _, e)
    | Inj(_, _, e)
    | Parenthesized(e) => mk_block(e)
    | Case(_, scrut, rules) =>
      let scrut_splices = mk_block(scrut);
      let rules_splices =
        rules |> List.map((UHExp.Rule(_, e)) => mk_block(e)) |> union_join;
      union(scrut_splices, rules_splices);
    | Subscript(_, e1, e2, e3) =>
      let splices_1 = mk_block(e1);
      let splices_2 = mk_block(e2);
      let splices_3 = mk_block(e3);
      union_join([splices_1, splices_2, splices_3]);
    };

  mk_block(e);
};
