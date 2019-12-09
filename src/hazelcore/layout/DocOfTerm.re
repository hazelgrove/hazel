open GeneralUtil;

type doc = Doc.t(TermTag.t);

let space = Doc.space;
let indent = Doc.Text("  ");

let indent_and_align = (d: doc): doc => Doc.(hcats([indent, align(d)]));

let doc_of_text =
    (~caret: option(int)=?, ~steps: CursorPath.steps, text: string): doc =>
  Doc.Text(text)
  |> Doc.tag(
       TermTag.mk_Text(~caret?, ~steps, ~length=String.length(text), ()),
     );

let doc_of_comma =
    (
      ~caret: option(Side.t)=?,
      ~steps: CursorPath.steps,
      ~enforce_inline: bool,
      (),
    )
    : doc => {
  let comma_doc =
    Doc.(Text(",") |> tag(TermTag.mk_Op(~caret?, ~steps, ())));
  let padding =
    Doc.(enforce_inline ? Text(" ") : choices([Text(" "), Linebreak]));
  Doc.hcats([comma_doc, padding]);
};

// for non-Comma operators
let pad_operator =
    (~inline_padding as (left, right): (doc, doc), operator: doc): doc =>
  Doc.(
    choices([
      hcats([left, operator, right]),
      hcats([Linebreak, operator, right]),
    ])
  );

let doc_of_op =
    (~caret: option(Side.t)=?, ~steps: CursorPath.steps, op_text: string, ())
    : doc =>
  Doc.Text(op_text) |> Doc.tag(TermTag.mk_Op(~caret?, ~steps, ()));

let pad_child =
    (
      ~inline_padding: (doc, doc)=(Doc.empty, Doc.empty),
      ~enforce_inline: bool,
      child: (~enforce_inline: bool) => doc,
    )
    : doc => {
  let (left, right) = inline_padding;
  enforce_inline
    ? Doc.hcats([left, child(~enforce_inline=true), right])
    : Doc.(
        choices([
          hcats([left, child(~enforce_inline=true), right]),
          hcats([
            Linebreak,
            indent_and_align(child(~enforce_inline=false)),
            Linebreak,
          ]),
        ])
      );
};

let doc_of_Unit =
    (~caret: option(Side.t)=?, ~steps: CursorPath.steps, ()): doc =>
  DocOfDelim.doc(~caret?, ~path=(steps, 0), "()");

let doc_of_Num =
    (~caret: option(Side.t)=?, ~steps: CursorPath.steps, ()): doc =>
  DocOfDelim.doc(~caret?, ~path=(steps, 0), LangUtil.typeN);

let doc_of_Bool =
    (~caret: option(Side.t)=?, ~steps: CursorPath.steps, ()): doc =>
  DocOfDelim.doc(~caret?, ~path=(steps, 0), LangUtil.typeB);

let doc_of_EmptyHole =
    (~caret: option(Side.t)=?, ~steps: CursorPath.steps, u: string): doc =>
  DocOfDelim.doc(~caret?, ~path=(steps, 0), u);

let doc_of_Wild =
    (~caret: option(Side.t)=?, ~steps: CursorPath.steps, ()): doc =>
  DocOfDelim.doc(~caret?, ~path=(steps, 0), "_");

let doc_of_Var =
    (~caret: option(int)=?, ~steps: CursorPath.steps, x: Var.t): doc =>
  doc_of_text(~caret?, ~steps, x);

let doc_of_NumLit =
    (~caret: option(int)=?, ~steps: CursorPath.steps, n: int): doc =>
  doc_of_text(~caret?, ~steps, string_of_int(n));

let doc_of_BoolLit =
    (~caret: option(int)=?, ~steps: CursorPath.steps, b: bool): doc =>
  doc_of_text(~caret?, ~steps, string_of_bool(b));

let doc_of_ListNil =
    (~caret: option(Side.t)=?, ~steps: CursorPath.steps, ()): doc =>
  DocOfDelim.doc(~caret?, ~path=(steps, 0), "[]");

let doc_of_Parenthesized =
    (
      ~steps: CursorPath.steps,
      ~enforce_inline: bool,
      ~open_delim=DocOfDelim.open_Parenthesized(steps),
      ~close_delim=DocOfDelim.close_Parenthesized(steps),
      ~body: (~enforce_inline: bool) => doc,
      (),
    )
    : doc => {
  let open_group = open_delim |> Doc.tag(TermTag.DelimGroup);
  let close_group = close_delim |> Doc.tag(TermTag.DelimGroup);
  Doc.hcats([open_group, body |> pad_child(~enforce_inline), close_group]);
};

let doc_of_List =
    (
      ~steps: CursorPath.steps,
      ~enforce_inline: bool,
      ~open_delim=DocOfDelim.open_List(steps),
      ~close_delim=DocOfDelim.close_List(steps),
      ~body: (~enforce_inline: bool) => doc,
      (),
    )
    : doc => {
  let open_group = open_delim |> Doc.tag(TermTag.DelimGroup);
  let close_group = close_delim |> Doc.tag(TermTag.DelimGroup);
  Doc.hcats([open_group, body |> pad_child(~enforce_inline), close_group]);
};

let doc_of_Inj =
    (
      ~steps: CursorPath.steps,
      ~enforce_inline: bool,
      ~inj_side: InjSide.t,
      ~open_delim=DocOfDelim.open_Inj(steps, inj_side),
      ~close_delim=DocOfDelim.close_Inj(steps),
      ~body: (~enforce_inline: bool) => doc,
      (),
    )
    : doc => {
  let open_group = open_delim |> Doc.tag(TermTag.DelimGroup);
  let close_group = close_delim |> Doc.tag(TermTag.DelimGroup);
  Doc.hcats([open_group, body |> pad_child(~enforce_inline), close_group]);
};

let doc_of_Lam =
    (
      ~steps: CursorPath.steps,
      ~enforce_inline: bool,
      ~lam_delim=DocOfDelim.sym_Lam(steps),
      ~colon_delim=DocOfDelim.colon_Lam(steps),
      ~open_delim=DocOfDelim.open_Lam(steps),
      ~close_delim=DocOfDelim.close_Lam(steps),
      ~p: (~enforce_inline: bool) => doc,
      ~ann: option((~enforce_inline: bool) => doc),
      ~body: (~enforce_inline: bool) => doc,
      (),
    )
    : doc => {
  let open_group = {
    let doc =
      switch (ann) {
      | None =>
        Doc.hcats([lam_delim, p |> pad_child(~enforce_inline), open_delim])
      | Some(ann) =>
        Doc.hcats([
          lam_delim,
          p |> pad_child(~enforce_inline),
          colon_delim,
          ann |> pad_child(~enforce_inline),
          open_delim,
        ])
      };
    doc |> Doc.tag(TermTag.DelimGroup);
  };
  let close_group = close_delim |> Doc.tag(TermTag.DelimGroup);
  Doc.hcats([open_group, body |> pad_child(~enforce_inline), close_group]);
};

let doc_of_Case =
    (
      ~steps: CursorPath.steps,
      ~case_delim=DocOfDelim.open_Case(steps),
      ~end_delim=DocOfDelim.close_Case(steps),
      ~scrut: (~enforce_inline: bool) => doc,
      ~rules: list(doc),
      (),
    )
    : doc => {
  let open_group = case_delim |> Doc.tag(TermTag.DelimGroup);
  let close_group = end_delim |> Doc.tag(TermTag.DelimGroup);
  Doc.(
    vseps(
      [
        choices([
          hseps([open_group, scrut(~enforce_inline=true)]),
          vseps([
            open_group,
            indent_and_align(scrut(~enforce_inline=false)),
          ]),
        ]),
        ...rules,
      ]
      @ [close_group],
    )
  );
};

let doc_of_Case_ann =
    (
      ~steps: CursorPath.steps,
      ~case_delim=DocOfDelim.open_Case(steps),
      ~end_delim=DocOfDelim.close_Case_ann(steps),
      ~scrut: (~enforce_inline: bool) => doc,
      ~rules: list(doc),
      ~ann: (~enforce_inline: bool) => doc,
      (),
    )
    : doc => {
  let open_group = case_delim |> Doc.tag(TermTag.DelimGroup);
  let close_group =
    Doc.(
      choices([
        hseps([end_delim, ann(~enforce_inline=true)]),
        vseps([end_delim, indent_and_align(ann(~enforce_inline=false))]),
      ])
      |> tag(TermTag.DelimGroup)
    );
  Doc.(
    vseps(
      [
        choices([
          hseps([open_group, scrut(~enforce_inline=true)]),
          vseps([
            open_group,
            indent_and_align(scrut(~enforce_inline=false)),
          ]),
        ]),
        ...rules,
      ]
      @ [close_group],
    )
  );
};

let doc_of_Rule =
    (
      ~steps: CursorPath.steps,
      ~bar_delim=DocOfDelim.bar_Rule(steps),
      ~arrow_delim=DocOfDelim.arrow_Rule(steps),
      ~p: (~enforce_inline: bool) => doc,
      ~clause: (~enforce_inline: bool) => doc,
      (),
    )
    : doc => {
  let delim_group =
    Doc.hcats([
      bar_delim,
      p |> pad_child(~inline_padding=(space, space), ~enforce_inline=false),
      arrow_delim,
    ]);
  Doc.(
    choices([
      hseps([delim_group, clause(~enforce_inline=true)]),
      vseps([delim_group, indent_and_align(clause(~enforce_inline=false))]),
    ])
  );
};

let doc_of_LetLine =
    (
      ~steps: CursorPath.steps,
      ~let_delim=DocOfDelim.let_LetLine(steps),
      ~colon_delim=DocOfDelim.colon_LetLine(steps),
      ~eq_delim=DocOfDelim.eq_LetLine(steps),
      ~in_delim=DocOfDelim.in_LetLine(steps),
      ~p: (~enforce_inline: bool) => doc,
      ~ann: option((~enforce_inline: bool) => doc),
      ~def: (~enforce_inline: bool) => doc,
      (),
    )
    : doc => {
  let open_group = {
    let doc =
      switch (ann) {
      | None =>
        Doc.hcats([
          let_delim,
          p
          |> pad_child(~inline_padding=(space, space), ~enforce_inline=false),
          eq_delim,
        ])
      | Some(ann) =>
        Doc.hcats([
          let_delim,
          p
          |> pad_child(~inline_padding=(space, space), ~enforce_inline=false),
          colon_delim,
          ann
          |> pad_child(~inline_padding=(space, space), ~enforce_inline=false),
          eq_delim,
        ])
      };
    doc |> Doc.tag(TermTag.DelimGroup);
  };
  let close_group = in_delim |> Doc.tag(TermTag.DelimGroup);
  Doc.hcats([
    open_group,
    def |> pad_child(~inline_padding=(space, space), ~enforce_inline=false),
    close_group,
  ]);
};

let doc_of_tuple =
    (
      ~steps: CursorPath.steps,
      ~enforce_inline: bool,
      ~seq: Seq.t(_, 'operator),
      elems: list(Skel.t('operator)),
      elem_docs: list(doc),
    )
    : doc =>
  switch (zip(elems, elem_docs)) {
  | [] => failwith(__LOC__ ++ ": found empty tuple")
  | [(_, hd_doc), ...tl] =>
    tl
    |> List.fold_left(
         (tuple_so_far, (elem, elem_doc)) => {
           let comma_index = Skel.rightmost_tm_index(elem) + Seq.length(seq);
           let comma_doc =
             doc_of_comma(~steps=steps @ [comma_index], ~enforce_inline, ());
           Doc.hcats([tuple_so_far, comma_doc, elem_doc]);
         },
         hd_doc,
       )
  };

let rec _doc_of_BinOp =
        (
          ~mk_BinOp_tag:
             (
               ErrStatus.t,
               'operator,
               Skel.t('operator),
               Skel.t('operator)
             ) =>
             TermTag.t,
          ~doc_of_operand:
             (~steps: CursorPath.steps, ~enforce_inline: bool, 'operand) => doc,
          ~doc_of_operator: (~steps: CursorPath.steps, 'operator) => doc,
          ~inline_padding_of_operator: 'operator => (doc, doc),
          ~steps: CursorPath.steps,
          ~enforce_inline: bool,
          ~seq: Seq.t('operand, 'operator),
          skel: Skel.t('operator),
        )
        : doc => {
  let go =
    _doc_of_BinOp(
      ~mk_BinOp_tag,
      ~doc_of_operand,
      ~doc_of_operator,
      ~inline_padding_of_operator,
      ~steps,
      ~enforce_inline,
      ~seq,
    );
  switch (skel) {
  | Placeholder(n) =>
    let operand = seq |> Seq.nth_operand(n);
    doc_of_operand(~steps=steps @ [n], ~enforce_inline, operand);
  | BinOp(err, op, skel1, skel2) =>
    let op_doc = {
      let op_index = Skel.rightmost_tm_index(skel1) + Seq.length(seq);
      doc_of_operator(~steps=steps @ [op_index], op);
    };
    let skel1_doc = go(skel1);
    let skel2_doc = go(skel2);
    Doc.hcats([
      skel1_doc,
      op_doc |> pad_operator(~inline_padding=inline_padding_of_operator(op)),
      skel2_doc,
    ])
    |> Doc.tag(mk_BinOp_tag(err, op, skel1, skel2));
  };
};

let _doc_of_NTuple =
    (
      ~is_comma: 'operator => bool,
      ~get_tuple_elements: Skel.t('operator) => list(Skel.t('operator)),
      ~mk_NTuple_tag: (ErrStatus.t, list(Skel.t('operator))) => TermTag.t,
      ~mk_BinOp_tag:
         (ErrStatus.t, 'operator, Skel.t('operator), Skel.t('operator)) =>
         TermTag.t,
      ~doc_of_operand:
         (~steps: CursorPath.steps, ~enforce_inline: bool, 'operand) => doc,
      ~doc_of_operator: (~steps: CursorPath.steps, 'operator) => doc,
      ~inline_padding_of_operator: 'operator => (doc, doc),
      ~steps: CursorPath.steps,
      ~enforce_inline: bool,
      OpSeq(skel, seq): OpSeq.t('operand, 'operator),
    )
    : doc => {
  let elems = skel |> get_tuple_elements;
  let elem_docs =
    elems
    |> List.map(
         _doc_of_BinOp(
           ~mk_BinOp_tag,
           ~doc_of_operand,
           ~doc_of_operator,
           ~inline_padding_of_operator,
           ~enforce_inline,
           ~steps,
           ~seq,
         ),
       );
  let doc = doc_of_tuple(~steps, ~enforce_inline, ~seq, elems, elem_docs);
  switch (skel) {
  | BinOp(err, op, _, _) when op |> is_comma =>
    doc |> Doc.tag(mk_NTuple_tag(err, elems))
  | _ =>
    // 1-tuple, no need to tag
    doc
  };
};

let rec _doc_of_ZBinOp =
        (
          ~erase_zseq:
             ZSeq.t('operand, 'operator, 'zoperand, 'zoperator) =>
             Seq.t('operand, 'operator),
          ~mk_BinOp_tag:
             (
               ~has_cursor: bool,
               ErrStatus.t,
               'operator,
               Skel.t('operator),
               Skel.t('operator)
             ) =>
             TermTag.t,
          ~doc_of_operand:
             (~steps: CursorPath.steps, ~enforce_inline: bool, 'operand) => doc,
          ~doc_of_operator: (~steps: CursorPath.steps, 'operator) => doc,
          ~doc_of_zoperand:
             (~steps: CursorPath.steps, ~enforce_inline: bool, 'zoperand) =>
             doc,
          ~doc_of_zoperator: (~steps: CursorPath.steps, 'zoperator) => doc,
          ~inline_padding_of_operator: 'operator => (doc, doc),
          ~steps: CursorPath.steps,
          ~enforce_inline: bool,
          ~zseq: ZSeq.t('operand, 'operator, 'zoperand, 'zoperator),
          skel: Skel.t('operator),
        )
        : doc => {
  let seq = zseq |> erase_zseq;
  let go =
    _doc_of_ZBinOp(
      ~erase_zseq,
      ~mk_BinOp_tag,
      ~doc_of_operand,
      ~doc_of_operator,
      ~doc_of_zoperand,
      ~doc_of_zoperator,
      ~inline_padding_of_operator,
      ~steps,
      ~enforce_inline,
      ~zseq,
    );
  let doc_of_BinOp =
    _doc_of_BinOp(
      ~mk_BinOp_tag=mk_BinOp_tag(~has_cursor=false),
      ~doc_of_operand,
      ~doc_of_operator,
      ~inline_padding_of_operator,
      ~steps,
      ~enforce_inline,
      ~seq,
    );
  switch (skel) {
  | Placeholder(_) =>
    let zoperand =
      switch (zseq) {
      | ZOperator(_) => assert(false)
      | ZOperand(zoperand, _) => zoperand
      };
    doc_of_zoperand(~steps, ~enforce_inline, zoperand);
  | BinOp(err, op, skel1, skel2) =>
    let (has_cursor, op_doc, skel1_doc, skel2_doc) =
      if (ZOpSeq.skel_is_rooted_at_cursor(skel, zseq)) {
        let zop_doc =
          switch (zseq) {
          | ZOperand(_) => assert(false)
          | ZOperator(zop, _) => doc_of_zoperator(~steps, zop)
          };
        (true, zop_doc, doc_of_BinOp(skel1), doc_of_BinOp(skel2));
      } else {
        let op_doc = {
          let op_index = Skel.rightmost_tm_index(skel1) + Seq.length(seq);
          doc_of_operator(~steps=steps @ [op_index], op);
        };
        let (skel1_doc, skel2_doc) =
          ZOpSeq.skel_contains_cursor(skel1, zseq)
            ? (go(skel1), doc_of_BinOp(skel2))
            : (doc_of_BinOp(skel1), go(skel2));
        (false, op_doc, skel1_doc, skel2_doc);
      };
    Doc.hcats([
      skel1_doc,
      op_doc |> pad_operator(~inline_padding=inline_padding_of_operator(op)),
      skel2_doc,
    ])
    |> Doc.tag(mk_BinOp_tag(~has_cursor, err, op, skel1, skel2));
  };
};

let _doc_of_ZNTuple =
    (
      ~is_comma: 'operator => bool,
      ~is_zcomma: 'zoperator => bool,
      ~erase_zseq:
         ZSeq.t('operand, 'operator, 'zoperand, 'zoperator) =>
         Seq.t('operand, 'operator),
      ~get_tuple_elements: Skel.t('operator) => list(Skel.t('operator)),
      ~mk_NTuple_tag:
         (~has_cursor: bool, ErrStatus.t, list(Skel.t('operator))) =>
         TermTag.t,
      ~mk_BinOp_tag:
         (
           ~has_cursor: bool,
           ErrStatus.t,
           'operator,
           Skel.t('operator),
           Skel.t('operator)
         ) =>
         TermTag.t,
      ~doc_of_operand:
         (~steps: CursorPath.steps, ~enforce_inline: bool, 'operand) => doc,
      ~doc_of_operator: (~steps: CursorPath.steps, 'operator) => doc,
      ~doc_of_zoperand:
         (~steps: CursorPath.steps, ~enforce_inline: bool, 'zoperand) => doc,
      ~doc_of_zoperator: (~steps: CursorPath.steps, 'zoperator) => doc,
      ~inline_padding_of_operator: 'operator => (doc, doc),
      ~steps: CursorPath.steps,
      ~enforce_inline: bool,
      ZOpSeq(skel, zseq):
        ZOpSeq.t('operand, 'operator, 'zoperand, 'zoperator),
    )
    : doc => {
  let seq = zseq |> erase_zseq;
  let elems = skel |> get_tuple_elements;
  switch (zseq) {
  | ZOperator(zop, _) when zop |> is_zcomma =>
    // cursor on tuple comma
    let doc =
      _doc_of_NTuple(
        ~is_comma,
        ~get_tuple_elements,
        ~mk_NTuple_tag=mk_NTuple_tag(~has_cursor=true),
        ~mk_BinOp_tag=mk_BinOp_tag(~has_cursor=false),
        ~doc_of_operand,
        ~doc_of_operator,
        ~inline_padding_of_operator,
        ~steps,
        ~enforce_inline,
        OpSeq(skel, seq),
      );
    let err =
      switch (skel) {
      | BinOp(err, _, _, _) => err
      | _ => assert(false)
      };
    doc |> Doc.tag(mk_NTuple_tag(~has_cursor=true, err, elems));
  | _ =>
    // cursor within tuple element
    let doc_of_BinOp =
      _doc_of_BinOp(
        ~mk_BinOp_tag=mk_BinOp_tag(~has_cursor=false),
        ~doc_of_operand,
        ~doc_of_operator,
        ~inline_padding_of_operator,
        ~steps,
        ~enforce_inline,
        ~seq,
      );
    let doc_of_ZBinOp =
      _doc_of_ZBinOp(
        ~erase_zseq,
        ~mk_BinOp_tag,
        ~doc_of_operand,
        ~doc_of_operator,
        ~doc_of_zoperand,
        ~doc_of_zoperator,
        ~inline_padding_of_operator,
        ~steps,
        ~enforce_inline,
        ~zseq,
      );
    let elem_docs =
      elems
      |> List.map(elem =>
           ZOpSeq.skel_contains_cursor(elem, zseq)
             ? doc_of_ZBinOp(elem) : doc_of_BinOp(elem)
         );
    let doc = doc_of_tuple(~steps, ~enforce_inline, ~seq, elems, elem_docs);
    switch (skel) {
    | BinOp(err, op, _, _) when op |> is_comma =>
      doc |> Doc.tag(mk_NTuple_tag(~has_cursor=false, err, elems))
    | _ =>
      // 1-tuple, no need to tag
      doc
    };
  };
};

module Typ = {
  let is_comma = UHTyp.is_Prod;
  let is_zcomma = zop => zop |> ZTyp.erase_zoperator |> is_comma;
  let erase_zseq = ZTyp.erase_zseq;
  let get_tuple_elements = UHTyp.get_prod_elements;

  let inline_padding_of_operator =
    fun
    | UHTyp.Prod => (Doc.empty, Doc.space)
    | Arrow
    | Sum => (Doc.space, Doc.space);

  let doc_of_NTuple =
    _doc_of_NTuple(
      ~is_comma,
      ~get_tuple_elements,
      ~mk_NTuple_tag=
        (_err, elems) => TermTag.mk_Term(~shape=TypNProd(elems), ()),
      ~mk_BinOp_tag=
        (_err, op, skel1, skel2) =>
          TermTag.mk_Term(~shape=TypBinOp(op, skel1, skel2), ()),
      ~inline_padding_of_operator,
    );

  let rec doc =
          (~steps: CursorPath.steps, ~enforce_inline: bool, uty: UHTyp.t): doc =>
    switch (uty) {
    | T1(uty1) => doc_of_opseq(~steps, ~enforce_inline, uty1)
    | T0(uty0) => doc_of_operand(~steps, ~enforce_inline, uty0)
    }
  and doc_of_opseq =
      (~steps: CursorPath.steps, ~enforce_inline: bool, opseq: UHTyp.opseq)
      : doc =>
    doc_of_NTuple(
      ~doc_of_operand,
      ~doc_of_operator,
      ~steps,
      ~enforce_inline,
      opseq,
    )
  and doc_of_operator = (~steps: CursorPath.steps, op: UHTyp.operator): doc =>
    doc_of_op(~steps, Associator.Typ.string_of_op(op), ())
  and doc_of_operand =
      (
        ~steps: CursorPath.steps,
        ~enforce_inline: bool,
        operand: UHTyp.operand,
      )
      : doc => {
    let doc =
      switch (operand) {
      | Hole => doc_of_EmptyHole(~steps, "?")
      | Unit => doc_of_Unit(~steps, ())
      | Num => doc_of_Num(~steps, ())
      | Bool => doc_of_Bool(~steps, ())
      | Parenthesized(body) =>
        let body = doc(~steps=steps @ [0], body);
        doc_of_Parenthesized(~steps, ~enforce_inline, ~body, ());
      | List(body) =>
        let body = doc(~steps=steps @ [0], body);
        doc_of_List(~steps, ~enforce_inline, ~body, ());
      };
    doc |> Doc.tag(TermTag.mk_Term(~shape=TypOperand(operand), ()));
  };

  let doc_of_ZNTuple =
    _doc_of_ZNTuple(
      ~is_comma,
      ~is_zcomma,
      ~erase_zseq,
      ~get_tuple_elements,
      ~mk_NTuple_tag=
        (~has_cursor, _err, elems) =>
          TermTag.mk_Term(~has_cursor, ~shape=TypNProd(elems), ()),
      ~mk_BinOp_tag=
        (~has_cursor, _err, op, skel1, skel2) =>
          TermTag.mk_Term(
            ~has_cursor,
            ~shape=TypBinOp(op, skel1, skel2),
            (),
          ),
      ~doc_of_operand,
      ~doc_of_operator,
      ~inline_padding_of_operator,
    );

  let rec doc_of_z =
          (~steps: CursorPath.steps, ~enforce_inline: bool, zty: ZTyp.t): doc =>
    switch (zty) {
    | ZT1(zty1) => doc_of_zopseq(~steps, ~enforce_inline, zty1)
    | ZT0(zty0) => doc_of_zoperand(~steps, ~enforce_inline, zty0)
    }
  and doc_of_zopseq =
      (~steps: CursorPath.steps, ~enforce_inline: bool, zopseq: ZTyp.zopseq)
      : doc =>
    doc_of_ZNTuple(
      ~doc_of_zoperand,
      ~doc_of_zoperator,
      ~steps,
      ~enforce_inline,
      zopseq,
    )
  and doc_of_zoperator = (~steps: CursorPath.steps, zop: ZTyp.zoperator): doc =>
    switch (zop) {
    | (OnDelim(_) | OnText(_), _) =>
      failwith(__LOC__ ++ ": invalid cursor position")
    | (OnOp(side), op) =>
      doc_of_op(~caret=side, ~steps, Associator.Typ.string_of_op(op), ())
    }
  and doc_of_zoperand =
      (
        ~steps: CursorPath.steps,
        ~enforce_inline: bool,
        zoperand: ZTyp.zoperand,
      )
      : doc => {
    let doc =
      switch (zoperand) {
      | CursorT(OnText(_) | OnOp(_), _) =>
        failwith(__LOC__ ++ ": invalid cursor position")

      | CursorT(OnDelim(_, side), Hole) =>
        doc_of_EmptyHole(~caret=side, ~steps, "?")
      | CursorT(OnDelim(_, side), Unit) =>
        doc_of_Unit(~caret=side, ~steps, ())
      | CursorT(OnDelim(_, side), Num) =>
        doc_of_Num(~caret=side, ~steps, ())
      | CursorT(OnDelim(_, side), Bool) =>
        doc_of_Bool(~caret=side, ~steps, ())

      | CursorT(OnDelim(k, side), Parenthesized(body)) =>
        let doc_of_Parenthesized =
          doc_of_Parenthesized(
            ~steps,
            ~enforce_inline,
            ~body=doc(~steps=steps @ [0], body),
          );
        switch (k) {
        | 0 =>
          doc_of_Parenthesized(
            ~open_delim=DocOfDelim.open_Parenthesized(~caret=side, steps),
            (),
          )
        | _one =>
          doc_of_Parenthesized(
            ~close_delim=DocOfDelim.close_Parenthesized(~caret=side, steps),
            (),
          )
        };
      | CursorT(OnDelim(k, side), List(body)) =>
        let doc_of_List =
          doc_of_List(
            ~steps,
            ~enforce_inline,
            ~body=doc(~steps=steps @ [0], body),
          );
        switch (k) {
        | 0 =>
          doc_of_List(
            ~open_delim=DocOfDelim.open_List(~caret=side, steps),
            (),
          )
        | _one =>
          doc_of_List(
            ~close_delim=DocOfDelim.close_List(~caret=side, steps),
            (),
          )
        };

      | ParenthesizedZ(zbody) =>
        let body = doc_of_z(~steps=steps @ [0], zbody);
        doc_of_Parenthesized(~steps, ~enforce_inline, ~body, ());
      | ListZ(zbody) =>
        let body = doc_of_z(~steps=steps @ [0], zbody);
        doc_of_List(~steps, ~enforce_inline, ~body, ());
      };

    let has_cursor =
      switch (zoperand) {
      | CursorT(_) => true
      | _ => false
      };
    let shape = TermTag.TypOperand(zoperand |> ZTyp.erase_zoperand);
    doc |> Doc.tag(TermTag.mk_Term(~has_cursor, ~shape, ()));
  };
};

module Pat = {
  let is_comma = UHPat.is_Comma;
  let is_zcomma = zop => zop |> ZPat.erase_zoperator |> is_comma;
  let erase_zseq = ZPat.erase_zseq;
  let get_tuple_elements = UHPat.get_tuple_elements;

  let inline_padding_of_operator =
    Doc.(
      fun
      | UHPat.Comma => (empty, space)
      | Space
      | Cons => (empty, empty)
    );

  let doc_of_NTuple =
    _doc_of_NTuple(
      ~is_comma,
      ~get_tuple_elements,
      ~mk_NTuple_tag=
        (err, elems) => TermTag.mk_Term(~shape=PatNTuple(err, elems), ()),
      ~mk_BinOp_tag=
        (err, op, skel1, skel2) =>
          TermTag.mk_Term(~shape=PatBinOp(err, op, skel1, skel2), ()),
      ~inline_padding_of_operator,
    );

  let rec doc =
          (~steps: CursorPath.steps, ~enforce_inline: bool, p: UHPat.t): doc =>
    switch (p) {
    | P1(p1) => doc_of_opseq(~steps, ~enforce_inline, p1)
    | P0(p0) => doc_of_operand(~steps, ~enforce_inline, p0)
    }
  and doc_of_opseq =
      (~steps: CursorPath.steps, ~enforce_inline: bool, opseq: UHPat.opseq)
      : doc =>
    doc_of_NTuple(
      ~doc_of_operand,
      ~doc_of_operator,
      ~steps,
      ~enforce_inline,
      opseq,
    )
  and doc_of_operator = (~steps: CursorPath.steps, op: UHPat.operator): doc =>
    doc_of_op(~steps, Associator.Pat.string_of_op(op), ())
  and doc_of_operand =
      (
        ~steps: CursorPath.steps,
        ~enforce_inline: bool,
        operand: UHPat.operand,
      )
      : doc => {
    let doc =
      switch (operand) {
      | EmptyHole(u) => doc_of_EmptyHole(~steps, string_of_int(u))
      | Wild(_) => doc_of_Wild(~steps, ())
      | Var(_, _, x) => doc_of_Var(~steps, x)
      | NumLit(_, n) => doc_of_NumLit(~steps, n)
      | BoolLit(_, b) => doc_of_BoolLit(~steps, b)
      | ListNil(_) => doc_of_ListNil(~steps, ())
      | Parenthesized(body) =>
        let body = doc(~steps=steps @ [0], body);
        doc_of_Parenthesized(~steps, ~enforce_inline, ~body, ());
      | Inj(_, inj_side, body) =>
        let body = doc(~steps=steps @ [0], body);
        doc_of_Inj(~steps, ~enforce_inline, ~inj_side, ~body, ());
      };
    doc |> Doc.tag(TermTag.mk_Term(~shape=PatOperand(operand), ()));
  };

  let doc_of_ZNTuple =
    _doc_of_ZNTuple(
      ~is_comma,
      ~is_zcomma,
      ~erase_zseq,
      ~get_tuple_elements,
      ~mk_NTuple_tag=
        (~has_cursor, err, elems) =>
          TermTag.mk_Term(~has_cursor, ~shape=PatNTuple(err, elems), ()),
      ~mk_BinOp_tag=
        (~has_cursor, err, op, skel1, skel2) =>
          TermTag.mk_Term(
            ~has_cursor,
            ~shape=PatBinOp(err, op, skel1, skel2),
            (),
          ),
      ~doc_of_operand,
      ~doc_of_operator,
      ~inline_padding_of_operator,
    );

  let rec doc_of_z =
          (~steps: CursorPath.steps, ~enforce_inline: bool, zp: ZPat.t): doc =>
    switch (zp) {
    | ZP1(zp1) => doc_of_zopseq(~steps, ~enforce_inline, zp1)
    | ZP0(zp0) => doc_of_zoperand(~steps, ~enforce_inline, zp0)
    }
  and doc_of_zopseq =
      (~steps: CursorPath.steps, ~enforce_inline: bool, zopseq: ZPat.zopseq)
      : doc =>
    doc_of_ZNTuple(
      ~doc_of_zoperand,
      ~doc_of_zoperator,
      ~steps,
      ~enforce_inline,
      zopseq,
    )
  and doc_of_zoperator = (~steps: CursorPath.steps, zop: ZPat.zoperator) =>
    switch (zop) {
    | (OnDelim(_) | OnText(_), _) =>
      failwith(__LOC__ ++ ": invalid cursor position")
    | (OnOp(side), op) =>
      doc_of_op(~caret=side, ~steps, Associator.Pat.string_of_op(op), ())
    }
  and doc_of_zoperand =
      (
        ~steps: CursorPath.steps,
        ~enforce_inline: bool,
        zoperand: ZPat.zoperand,
      )
      : doc => {
    let doc =
      switch (zoperand) {
      | CursorP(OnDelim(_) | OnOp(_), Var(_) | NumLit(_) | BoolLit(_))
      | CursorP(
          OnText(_) | OnOp(_),
          EmptyHole(_) | Wild(_) | ListNil(_) | Inj(_) | Parenthesized(_),
        ) =>
        failwith(__LOC__ ++ ": invalid cursor position")

      | CursorP(OnDelim(_, side), EmptyHole(u)) =>
        doc_of_EmptyHole(~caret=side, ~steps, string_of_int(u))
      | CursorP(OnText(j), Var(_, _, x)) => doc_of_Var(~caret=j, ~steps, x)
      | CursorP(OnText(j), NumLit(_, n)) =>
        doc_of_NumLit(~caret=j, ~steps, n)
      | CursorP(OnText(j), BoolLit(_, b)) =>
        doc_of_BoolLit(~caret=j, ~steps, b)
      | CursorP(OnDelim(_, side), ListNil(_)) =>
        doc_of_ListNil(~caret=side, ~steps, ())
      | CursorP(OnDelim(_, side), Wild(_)) =>
        doc_of_Wild(~caret=side, ~steps, ())

      | CursorP(OnDelim(k, side), Parenthesized(body)) =>
        let doc_of_Parenthesized =
          doc_of_Parenthesized(
            ~steps,
            ~enforce_inline,
            ~body=doc(~steps=steps @ [0], body),
          );
        switch (k) {
        | 0 =>
          doc_of_Parenthesized(
            ~open_delim=DocOfDelim.open_Parenthesized(~caret=side, steps),
            (),
          )
        | _one =>
          doc_of_Parenthesized(
            ~close_delim=DocOfDelim.close_Parenthesized(~caret=side, steps),
            (),
          )
        };
      | CursorP(OnDelim(k, side), Inj(_, inj_side, body)) =>
        let doc_of_Inj =
          doc_of_Inj(
            ~steps,
            ~enforce_inline,
            ~inj_side,
            ~body=doc(~steps=steps @ [0], body),
          );
        switch (k) {
        | 0 =>
          doc_of_Inj(
            ~open_delim=DocOfDelim.open_Inj(~caret=side, steps, inj_side),
            (),
          )
        | _one =>
          doc_of_Inj(
            ~close_delim=DocOfDelim.close_Inj(~caret=side, steps),
            (),
          )
        };

      | ParenthesizedZ(zbody) =>
        let body = doc_of_z(~steps=steps @ [0], zbody);
        doc_of_Parenthesized(~steps, ~enforce_inline, ~body, ());

      | InjZ(_, inj_side, zbody) =>
        let body = doc_of_z(~steps=steps @ [0], zbody);
        doc_of_Inj(~steps, ~enforce_inline, ~inj_side, ~body, ());
      };

    let has_cursor =
      switch (zoperand) {
      | CursorP(_) => true
      | _ => false
      };
    let shape = TermTag.PatOperand(zoperand |> ZPat.erase_zoperand);
    doc |> Doc.tag(TermTag.mk_Term(~has_cursor, ~shape, ()));
  };
};

module Exp = {
  let is_comma = UHExp.is_Comma;
  let is_zcomma = zop => zop |> ZExp.erase_zoperator |> is_comma;
  let erase_zseq = ZExp.erase_zseq;
  let get_tuple_elements = UHExp.get_tuple_elements;

  let inline_padding_of_operator =
    Doc.(
      fun
      | UHExp.Space
      | Times
      | Cons => (empty, empty)
      | Plus
      | Minus
      | LessThan
      | GreaterThan
      | Equals
      | And
      | Or => (space, space)
      | Comma => (empty, space)
    );

  let doc_of_NTuple =
    _doc_of_NTuple(
      ~is_comma,
      ~get_tuple_elements,
      ~mk_NTuple_tag=
        (err, elems) => TermTag.mk_Term(~shape=ExpNTuple(err, elems), ()),
      ~mk_BinOp_tag=
        (err, op, skel1, skel2) =>
          TermTag.mk_Term(~shape=ExpBinOp(err, op, skel1, skel2), ()),
      ~inline_padding_of_operator,
    );

  let rec doc =
          (~steps: CursorPath.steps, ~enforce_inline: bool, e: UHExp.t): doc =>
    switch (e) {
    | E2(e2) => enforce_inline ? Fail : doc_of_block(~steps, e2)
    | E1(e1) => doc_of_opseq(~steps, ~enforce_inline, e1)
    | E0(e0) => doc_of_operand(~steps, ~enforce_inline, e0)
    }
  and doc_of_block = (~steps: CursorPath.steps, block: UHExp.block): doc =>
    block
    |> mapi_zip((i, line) => doc_of_line(~steps=steps @ [i], line))
    |> split_last
    |> (
      fun
      | None =>
        failwith(__LOC__ ++ ": doc_of_block expected a non-empty block")
      | Some((leading, (_, concluding_doc))) =>
        List.fold_right(
          ((hd, hd_doc), tl_doc) =>
            Doc.vsep(hd_doc, tl_doc)
            |> Doc.tag(TermTag.mk_Term(~shape=ExpSubBlock(hd), ())),
          leading,
          concluding_doc,
        )
    )
  and doc_of_line = (~steps: CursorPath.steps, line: UHExp.line): doc =>
    switch (line) {
    | EmptyLine => doc_of_text(~steps, "")
    | ExpLine(opseq) => doc_of_opseq(~steps, ~enforce_inline=false, opseq)
    | LetLine(p, ann, def) =>
      let p = Pat.doc(~steps=steps @ [0], p);
      let ann = ann |> Opt.map(ann => Typ.doc(~steps=steps @ [1], ann));
      let def = doc(~steps=steps @ [2], def);
      doc_of_LetLine(~steps, ~p, ~ann, ~def, ());
    }
  and doc_of_opseq =
      (~steps: CursorPath.steps, ~enforce_inline: bool, opseq: UHExp.opseq)
      : doc =>
    doc_of_NTuple(
      ~doc_of_operand,
      ~doc_of_operator,
      ~steps,
      ~enforce_inline,
      opseq,
    )
  and doc_of_operator = (~steps: CursorPath.steps, op: UHExp.operator): doc =>
    doc_of_op(~steps, Associator.Exp.string_of_op(op), ())
  and doc_of_operand =
      (
        ~steps: CursorPath.steps,
        ~enforce_inline: bool,
        operand: UHExp.operand,
      )
      : doc => {
    let doc =
      switch (operand) {
      | EmptyHole(u) => doc_of_EmptyHole(~steps, string_of_int(u))
      | Var(_, _, x) => doc_of_Var(~steps, x)
      | NumLit(_, n) => doc_of_NumLit(~steps, n)
      | BoolLit(_, b) => doc_of_BoolLit(~steps, b)
      | ListNil(_) => doc_of_ListNil(~steps, ())
      | Lam(_, p, ann, body) =>
        let p = Pat.doc(~steps=steps @ [0], p);
        let ann = ann |> Opt.map(ann => Typ.doc(~steps=steps @ [1], ann));
        let body = doc(~steps=steps @ [2], body);
        doc_of_Lam(~steps, ~enforce_inline, ~p, ~ann, ~body, ());
      | Inj(_, inj_side, body) =>
        let body = doc(~steps=steps @ [0], body);
        doc_of_Inj(~steps, ~enforce_inline, ~inj_side, ~body, ());
      | Parenthesized(body) =>
        let body = doc(~steps=steps @ [0], body);
        doc_of_Parenthesized(~steps, ~enforce_inline, ~body, ());
      | Case(_, scrut, rules, ann) =>
        if (enforce_inline) {
          Fail;
        } else {
          let scrut = doc(~steps=steps @ [0], scrut);
          let rules =
            rules
            |> List.mapi((i, rule) =>
                 doc_of_rule(~steps=steps @ [1 + i], rule)
               );
          switch (ann) {
          | None => doc_of_Case(~steps, ~scrut, ~rules, ())
          | Some(ann) =>
            let ann = Typ.doc(~steps=steps @ [1 + List.length(rules)], ann);
            doc_of_Case_ann(~steps, ~scrut, ~rules, ~ann, ());
          };
        }
      | ApPalette(_) => failwith("unimplemented: doc_of_exp/ApPalette")
      };
    doc |> Doc.tag(TermTag.mk_Term(~shape=ExpOperand(operand), ()));
  }
  and doc_of_rule =
      (~steps: CursorPath.steps, Rule(p, clause) as rule: UHExp.rule): doc => {
    let p = Pat.doc(~steps=steps @ [0], p);
    let clause = doc(~steps=steps @ [1], clause);
    doc_of_Rule(~steps, ~p, ~clause, ())
    |> Doc.tag(TermTag.mk_Term(~shape=ExpRule(rule), ()));
  };

  let doc_of_ZNTuple =
    _doc_of_ZNTuple(
      ~is_comma,
      ~is_zcomma,
      ~erase_zseq,
      ~get_tuple_elements,
      ~mk_NTuple_tag=
        (~has_cursor, err, elems) =>
          TermTag.mk_Term(~has_cursor, ~shape=ExpNTuple(err, elems), ()),
      ~mk_BinOp_tag=
        (~has_cursor, err, op, skel1, skel2) =>
          TermTag.mk_Term(
            ~has_cursor,
            ~shape=ExpBinOp(err, op, skel1, skel2),
            (),
          ),
      ~doc_of_operand,
      ~doc_of_operator,
      ~inline_padding_of_operator,
    );

  let rec doc_of_z =
          (~steps: CursorPath.steps, ~enforce_inline: bool, ze: ZExp.t): doc =>
    switch (ze) {
    | ZE2(ze2) => enforce_inline ? Fail : doc_of_zblock(~steps, ze2)
    | ZE1(ze1) => doc_of_zopseq(~steps, ~enforce_inline, ze1)
    | ZE0(ze0) => doc_of_zoperand(~steps, ~enforce_inline, ze0)
    }
  and doc_of_zblock =
      (
        ~steps: CursorPath.steps,
        (prefix, zline, suffix) as zblock: ZExp.zblock,
      )
      : doc => {
    let block = zblock |> ZExp.erase_zblock;
    let line_docs = {
      let prefix_docs =
        prefix
        |> List.mapi((i, line) => doc_of_line(~steps=steps @ [i], line));
      let zline_doc =
        doc_of_zline(~steps=steps @ [List.length(prefix)], zline);
      let suffix_docs =
        suffix
        |> List.mapi((i, line) =>
             doc_of_line(~steps=steps @ [List.length(prefix) + 1 + i], line)
           );
      prefix_docs @ [zline_doc, ...suffix_docs];
    };
    switch (zip(block, line_docs) |> split_last) {
    | None =>
      failwith(__LOC__ ++ ": doc_of_zblock expected a non-empty block")
    | Some((leading, (_, concluding_doc))) =>
      List.fold_right(
        ((hd, hd_doc), tl_doc) =>
          Doc.vsep(hd_doc, tl_doc)
          |> Doc.tag(TermTag.mk_Term(~shape=ExpSubBlock(hd), ())),
        leading,
        concluding_doc,
      )
    };
  }
  and doc_of_zline = (~steps: CursorPath.steps, zline: ZExp.zline): doc =>
    switch (zline) {
    | CursorL(OnDelim(_) | OnOp(_), EmptyLine)
    | CursorL(OnText(_) | OnOp(_), LetLine(_))
    | CursorL(_, ExpLine(_)) =>
      failwith(__LOC__ ++ ": invalid cursor position")

    | CursorL(OnText(_), EmptyLine) => doc_of_text(~caret=0, ~steps, "")
    | CursorL(OnDelim(k, side), LetLine(p, ann, def)) =>
      let p = Pat.doc(~steps=steps @ [0], p);
      let ann = ann |> Opt.map(ann => Typ.doc(~steps=steps @ [1], ann));
      let def = doc(~steps=steps @ [2], def);
      let doc_of_LetLine = doc_of_LetLine(~steps, ~p, ~ann, ~def);
      switch (k) {
      | 0 =>
        doc_of_LetLine(
          ~let_delim=DocOfDelim.let_LetLine(~caret=side, steps),
          (),
        )
      | 1 =>
        doc_of_LetLine(
          ~colon_delim=DocOfDelim.colon_LetLine(~caret=side, steps),
          (),
        )
      | 2 =>
        doc_of_LetLine(
          ~eq_delim=DocOfDelim.eq_LetLine(~caret=side, steps),
          (),
        )
      | _three =>
        doc_of_LetLine(
          ~in_delim=DocOfDelim.in_LetLine(~caret=side, steps),
          (),
        )
      };

    | ExpLineZ(zopseq) =>
      doc_of_zopseq(~steps, ~enforce_inline=false, zopseq)

    | LetLineZP(zp, ann, def) =>
      let p = Pat.doc_of_z(~steps=steps @ [0], zp);
      let ann = ann |> Opt.map(ann => Typ.doc(~steps=steps @ [1], ann));
      let def = doc(~steps=steps @ [2], def);
      doc_of_LetLine(~steps, ~p, ~ann, ~def, ());
    | LetLineZA(p, zann, def) =>
      let p = Pat.doc(~steps=steps @ [0], p);
      let ann = Some(Typ.doc_of_z(~steps=steps @ [1], zann));
      let def = doc(~steps=steps @ [2], def);
      doc_of_LetLine(~steps, ~p, ~ann, ~def, ());
    | LetLineZE(p, ann, zdef) =>
      let p = Pat.doc(~steps=steps @ [0], p);
      let ann = ann |> Opt.map(ann => Typ.doc(~steps=steps @ [1], ann));
      let def = doc_of_z(~steps=steps @ [2], zdef);
      doc_of_LetLine(~steps, ~p, ~ann, ~def, ());
    }
  and doc_of_zopseq =
      (~steps: CursorPath.steps, ~enforce_inline: bool, zopseq: ZExp.zopseq)
      : doc =>
    doc_of_ZNTuple(
      ~doc_of_zoperand,
      ~doc_of_zoperator,
      ~steps,
      ~enforce_inline,
      zopseq,
    )
  and doc_of_zoperator = (~steps: CursorPath.steps, zop: ZExp.zoperator) =>
    switch (zop) {
    | (OnDelim(_) | OnText(_), _) =>
      failwith(__LOC__ ++ ": invalid cursor position")
    | (OnOp(side), op) =>
      doc_of_op(~caret=side, ~steps, Associator.Exp.string_of_op(op), ())
    }
  and doc_of_zoperand =
      (
        ~steps: CursorPath.steps,
        ~enforce_inline: bool,
        zoperand: ZExp.zoperand,
      )
      : doc => {
    let doc =
      switch (zoperand) {
      | CursorE(
          OnDelim(_) | OnOp(_),
          Var(_) | NumLit(_) | BoolLit(_) | ApPalette(_),
        )
      | CursorE(
          OnText(_) | OnOp(_),
          EmptyHole(_) | ListNil(_) | Lam(_) | Inj(_) | Case(_) |
          Parenthesized(_) |
          ApPalette(_),
        ) =>
        failwith(__LOC__ ++ ": invalid cursor position")

      | CursorE(OnDelim(_, side), EmptyHole(u)) =>
        doc_of_EmptyHole(~caret=side, ~steps, string_of_int(u))
      | CursorE(OnText(j), Var(_, _, x)) => doc_of_Var(~caret=j, ~steps, x)
      | CursorE(OnText(j), NumLit(_, n)) =>
        doc_of_NumLit(~caret=j, ~steps, n)
      | CursorE(OnText(j), BoolLit(_, b)) =>
        doc_of_BoolLit(~caret=j, ~steps, b)
      | CursorE(OnDelim(_, side), ListNil(_)) =>
        doc_of_ListNil(~caret=side, ~steps, ())

      | CursorE(OnDelim(k, side), Parenthesized(body)) =>
        let doc_of_Parenthesized =
          doc_of_Parenthesized(
            ~steps,
            ~enforce_inline,
            ~body=doc(~steps=steps @ [0], body),
          );
        switch (k) {
        | 0 =>
          doc_of_Parenthesized(
            ~open_delim=DocOfDelim.open_Parenthesized(~caret=side, steps),
            (),
          )
        | _one =>
          doc_of_Parenthesized(
            ~close_delim=DocOfDelim.close_Parenthesized(~caret=side, steps),
            (),
          )
        };
      | CursorE(OnDelim(k, side), Inj(_, inj_side, body)) =>
        let doc_of_Inj =
          doc_of_Inj(
            ~steps,
            ~enforce_inline,
            ~inj_side,
            ~body=doc(~steps=steps @ [0], body),
          );
        switch (k) {
        | 0 =>
          doc_of_Inj(
            ~open_delim=DocOfDelim.open_Inj(~caret=side, steps, inj_side),
            (),
          )
        | _one =>
          doc_of_Inj(
            ~close_delim=DocOfDelim.close_Inj(~caret=side, steps),
            (),
          )
        };
      | CursorE(OnDelim(k, side), Lam(_, p, ann, body)) =>
        let doc_of_Lam =
          doc_of_Lam(
            ~steps,
            ~enforce_inline,
            ~p=Pat.doc(~steps=steps @ [0], p),
            ~ann=ann |> Opt.map(ann => Typ.doc(~steps=steps @ [1], ann)),
            ~body=doc(~steps=steps @ [2], body),
          );
        switch (k) {
        | 0 =>
          doc_of_Lam(~lam_delim=DocOfDelim.sym_Lam(~caret=side, steps), ())
        | 1 =>
          doc_of_Lam(
            ~colon_delim=DocOfDelim.colon_Lam(~caret=side, steps),
            (),
          )
        | 2 =>
          doc_of_Lam(~open_delim=DocOfDelim.open_Lam(~caret=side, steps), ())
        | _three =>
          doc_of_Lam(
            ~close_delim=DocOfDelim.close_Lam(~caret=side, steps),
            (),
          )
        };
      | CursorE(OnDelim(k, side), Case(_, scrut, rules, ann)) =>
        let doc_of_Case = {
          let scrut = doc(~steps=steps @ [0], scrut);
          let rules =
            rules
            |> List.mapi((i, rule) =>
                 doc_of_rule(~steps=steps @ [1 + i], rule)
               );
          switch (ann) {
          | None => doc_of_Case(~steps, ~scrut, ~rules)
          | Some(ann) =>
            doc_of_Case_ann(
              ~steps,
              ~scrut,
              ~rules,
              ~ann=Typ.doc(~steps=steps @ [1], ann),
            )
          };
        };
        switch (k) {
        | 0 =>
          doc_of_Case(
            ~case_delim=DocOfDelim.open_Case(~caret=side, steps),
            (),
          )
        | _one =>
          doc_of_Case(
            ~case_delim=DocOfDelim.close_Case(~caret=side, steps),
            (),
          )
        };

      | ParenthesizedZ(zbody) =>
        let body = doc_of_z(~steps=steps @ [0], zbody);
        doc_of_Parenthesized(~steps, ~enforce_inline, ~body, ());

      | InjZ(_, inj_side, zbody) =>
        let body = doc_of_z(~steps=steps @ [0], zbody);
        doc_of_Inj(~steps, ~enforce_inline, ~inj_side, ~body, ());

      | LamZP(_, zp, ann, body) =>
        let p = Pat.doc_of_z(~steps=steps @ [0], zp);
        let ann = ann |> Opt.map(ann => Typ.doc(~steps=steps @ [1], ann));
        let body = doc(~steps=steps @ [2], body);
        doc_of_Lam(~steps, ~enforce_inline, ~p, ~ann, ~body, ());
      | LamZA(_, p, zann, body) =>
        let p = Pat.doc(~steps=steps @ [0], p);
        let ann = Some(Typ.doc_of_z(~steps=steps @ [1], zann));
        let body = doc(~steps=steps @ [2], body);
        doc_of_Lam(~steps, ~enforce_inline, ~p, ~ann, ~body, ());
      | LamZE(_, p, ann, zbody) =>
        let p = Pat.doc(~steps=steps @ [0], p);
        let ann = ann |> Opt.map(ann => Typ.doc(~steps=steps @ [1], ann));
        let body = doc_of_z(~steps=steps @ [2], zbody);
        doc_of_Lam(~steps, ~enforce_inline, ~p, ~ann, ~body, ());

      | CaseZE(_, zscrut, rules, ann) =>
        if (enforce_inline) {
          Fail;
        } else {
          let scrut = doc_of_z(~steps=steps @ [0], zscrut);
          let rules =
            rules
            |> List.mapi((i, rule) =>
                 doc_of_rule(~steps=steps @ [1 + i], rule)
               );
          switch (ann) {
          | None => doc_of_Case(~steps, ~scrut, ~rules, ())
          | Some(ann) =>
            let ann = Typ.doc(~steps=steps @ [1 + List.length(rules)], ann);
            doc_of_Case_ann(~steps, ~scrut, ~rules, ~ann, ());
          };
        }
      | CaseZR(_, scrut, zrules, ann) =>
        if (enforce_inline) {
          Fail;
        } else {
          let scrut = doc(~steps=steps @ [0], scrut);
          let rules =
            zrules
            |> ZList.mapi(
                 (i, zrule) => doc_of_zrule(~steps=steps @ [1 + i], zrule),
                 (i, rule) => doc_of_rule(~steps=steps @ [1 + i], rule),
               )
            |> ZList.join;
          switch (ann) {
          | None => doc_of_Case(~steps, ~scrut, ~rules, ())
          | Some(ann) =>
            let ann = Typ.doc(~steps=steps @ [1 + List.length(rules)], ann);
            doc_of_Case_ann(~steps, ~scrut, ~rules, ~ann, ());
          };
        }
      | CaseZA(_, scrut, rules, zann) =>
        if (enforce_inline) {
          Fail;
        } else {
          let scrut = doc(~steps=steps @ [0], scrut);
          let rules =
            rules
            |> List.mapi((i, rule) =>
                 doc_of_rule(~steps=steps @ [1 + i], rule)
               );
          let ann =
            Typ.doc_of_z(~steps=steps @ [1 + List.length(rules)], zann);
          doc_of_Case_ann(~steps, ~scrut, ~rules, ~ann, ());
        }

      | ApPaletteZ(_) => failwith(__LOC__ ++ ": unimplemented")
      };

    let has_cursor =
      switch (zoperand) {
      | CursorE(_) => true
      | _ => false
      };
    let shape = TermTag.ExpOperand(zoperand |> ZExp.erase_zoperand);
    doc |> Doc.tag(TermTag.mk_Term(~has_cursor, ~shape, ()));
  }
  and doc_of_zrule = (~steps: CursorPath.steps, zrule: ZExp.zrule): doc => {
    let doc =
      switch (zrule) {
      | CursorR(OnText(_) | OnOp(_), _) =>
        failwith("invalid cursor position")
      | CursorR(OnDelim(k, side), Rule(p, clause)) =>
        let doc_of_Rule =
          doc_of_Rule(
            ~steps,
            ~p=Pat.doc(~steps=steps @ [0], p),
            ~clause=doc(~steps=steps @ [1], clause),
          );
        switch (k) {
        | 0 =>
          doc_of_Rule(~bar_delim=DocOfDelim.bar_Rule(~caret=side, steps), ())
        | _one =>
          doc_of_Rule(~bar_delim=DocOfDelim.bar_Rule(~caret=side, steps), ())
        };
      | RuleZP(zp, clause) =>
        let p = Pat.doc_of_z(~steps=steps @ [0], zp);
        let clause = doc(~steps=steps @ [1], clause);
        doc_of_Rule(~steps, ~p, ~clause, ());
      | RuleZE(p, zclause) =>
        let p = Pat.doc(~steps=steps @ [0], p);
        let clause = doc_of_z(~steps=steps @ [1], zclause);
        doc_of_Rule(~steps, ~p, ~clause, ());
      };

    let has_cursor =
      switch (zrule) {
      | CursorR(_) => true
      | _ => false
      };
    let shape = TermTag.ExpRule(zrule |> ZExp.erase_zrule);
    doc |> Doc.tag(TermTag.mk_Term(~has_cursor, ~shape, ()));
  };
};
