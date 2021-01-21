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
  | Or => (UHDoc_common.space_, UHDoc_common.space_)
  | Comma => (UHDoc_common.empty_, UHDoc_common.space_);

let mk_EmptyHole: string => UHDoc.t = UHDoc_common.mk_EmptyHole(~sort=Exp);
let mk_InvalidText: string => UHDoc.t =
  UHDoc_common.mk_InvalidText(~sort=Exp);
let mk_IntLit: string => UHDoc.t = UHDoc_common.mk_IntLit(~sort=Exp);
let mk_FloatLit: string => UHDoc.t = UHDoc_common.mk_FloatLit(~sort=Exp);
let mk_BoolLit: bool => UHDoc.t = UHDoc_common.mk_BoolLit(~sort=Exp);
let mk_ListNil: unit => UHDoc.t = UHDoc_common.mk_ListNil(~sort=Exp);
let mk_Var: string => UHDoc.t = UHDoc_common.mk_Var(~sort=Exp);
let mk_Parenthesized: UHDoc_common.formatted_child => UHDoc.t =
  UHDoc_common.mk_Parenthesized(~sort=Exp);
let mk_Inj: (~inj_side: InjSide.t, UHDoc_common.formatted_child) => UHDoc.t =
  UHDoc_common.mk_Inj(~sort=Exp);
let mk_NTuple:
  (
    ~mk_operand: (~enforce_inline: bool, 'a) => UHDoc.t,
    ~mk_operator: UHExp.operator => UHDoc.t,
    ~enforce_inline: bool,
    OpSeq.t('a, UHExp.operator)
  ) =>
  UHDoc.t =
  UHDoc_common.mk_NTuple(
    ~sort=Exp,
    ~get_tuple_elements=UHExp.get_tuple_elements,
    ~inline_padding_of_operator,
  );

let annot_SubBlock = (~hd_index: int): (UHDoc.t => UHDoc.t) =>
  Doc.annot(
    UHAnnot.mk_Term(~sort=Exp, ~shape=SubBlock({hd_index: hd_index}), ()),
  );

let rec mk =
  lazy(
    UHDoc_common.memoize((~memoize: bool, ~enforce_inline: bool, e: UHExp.t) =>
      (Lazy.force(mk_block_0, ~memoize, ~enforce_inline, e): UHDoc.t)
    )
  )
// Two versions of `mk_block` so we can memoize them
and mk_block_0 =
  lazy(
    UHDoc_common.memoize(
      (~memoize: bool, ~enforce_inline: bool, block: UHExp.block) =>
      (mk_block(~offset=0, ~memoize, ~enforce_inline, block): UHDoc.t)
    )
  )
and mk_block_1 =
  lazy(
    UHDoc_common.memoize(
      (~memoize: bool, ~enforce_inline: bool, block: UHExp.block) =>
      (mk_block(~offset=1, ~memoize, ~enforce_inline, block): UHDoc.t)
    )
  )
and mk_block =
    (~offset: int, ~memoize, ~enforce_inline: bool, block: UHExp.block)
    : UHDoc.t =>
  if (enforce_inline && UHExp.Block.num_lines(block) > 1) {
    Doc.fail();
  } else {
    let (leading, (_, last_doc)) =
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
        switch ((hd: UHExp.line)) {
        | EmptyLine
        | CommentLine(_)
        | ExpLine(_) => Doc.vsep(hd_doc, tl_doc)
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
      (~memoize: bool, ~enforce_inline: bool, line: UHExp.line) =>
      (
        switch (line) {
        | EmptyLine =>
          UHDoc_common.empty_
          |> Doc.annot(UHAnnot.mk_Token(~shape=Text, ~len=0, ()))
        | CommentLine(comment) =>
          let comment_doc =
            UHDoc_common.mk_text(comment)
            |> Doc.annot(
                 UHAnnot.mk_Token(
                   ~shape=Text,
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
          Lazy.force(mk_opseq, ~memoize, ~enforce_inline, opseq)
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
          let def = mk_child(~memoize, ~enforce_inline, ~child_step=2, def);
          UHDoc_common.mk_LetLine(p, ann, def);
        }: UHDoc.t
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
      (~memoize: bool, ~enforce_inline: bool, operand: UHExp.operand) =>
      (
        switch (operand) {
        | EmptyHole(u) => mk_EmptyHole(UHDoc_common.hole_lbl(u + 1))
        | InvalidText(_, t) => mk_InvalidText(t)
        | Var(_, _, x) => mk_Var(x)
        | IntLit(_, n) => mk_IntLit(n)
        | FloatLit(_, f) => mk_FloatLit(f)
        | BoolLit(_, b) => mk_BoolLit(b)
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
          let body = mk_child(~memoize, ~enforce_inline, ~child_step=2, body);
          UHDoc_common.mk_Lam(p, ann, body);
        | Inj(_, inj_side, body) =>
          let body = mk_child(~memoize, ~enforce_inline, ~child_step=0, body);
          mk_Inj(~inj_side, body);
        | Parenthesized(body) =>
          let body = mk_child(~memoize, ~enforce_inline, ~child_step=0, body);
          mk_Parenthesized(body);
        | Case(_, scrut, rules) =>
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
            UHDoc_common.mk_Case(scrut, rules);
          }
        | If(_, t1, t2, t3) =>
          let guard = mk_child(~memoize, ~enforce_inline, ~child_step=0, t1);
          let then_branch =
            mk_child(~memoize, ~enforce_inline, ~child_step=1, t2);
          let else_branch =
            mk_child(~memoize, ~enforce_inline, ~child_step=2, t3);
          UHDoc_common.mk_If(guard, then_branch, else_branch);
        | ApPalette(_) => failwith("unimplemented: mk_exp/ApPalette")
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
        }: UHDoc.t
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
