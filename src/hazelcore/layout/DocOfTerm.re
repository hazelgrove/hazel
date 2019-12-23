open GeneralUtil;

[@deriving sexp]
type doc = Doc.t(TermTag.t);

let space = Doc.space;
let indent = Doc.indent;

let tag_Indent = Doc.tag(TermTag.Indent);
let tag_Padding = Doc.tag(TermTag.Padding);
let tag_DelimGroup = Doc.tag(TermTag.DelimGroup);
let tag_OpenChild = (~is_inline) =>
  Doc.tag(TermTag.mk_OpenChild(~is_inline, ()));
let tag_ClosedChild = (~is_inline) =>
  Doc.tag(TermTag.mk_ClosedChild(~is_inline, ()));

let indent_and_align = (d: doc): doc =>
  Doc.(hcats([indent |> tag_Padding, align(d)]));

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
    Doc.(
      enforce_inline
        ? tag_Padding(Text(" "))
        : choices([tag_Padding(Text(" ")), Linebreak])
    );
  Doc.hcats([comma_doc, padding]);
};

// for non-Comma operators
let pad_operator =
    (~inline_padding as (left, right): (doc, doc), operator: doc): doc => {
  Doc.(
    choices([
      hcats([left |> tag_Padding, operator, right |> tag_Padding]),
      hcats([Linebreak, operator, right |> tag_Padding]),
    ])
  );
};

let doc_of_op =
    (~caret: option(Side.t)=?, ~steps: CursorPath.steps, op_text: string, ())
    : doc =>
  Doc.Text(op_text) |> Doc.tag(TermTag.mk_Op(~caret?, ~steps, ()));

let doc_of_space_op = (~steps: CursorPath.steps, ()): doc =>
  Doc.space |> Doc.tag(TermTag.SpaceOp({steps: steps}));

let pad_child =
    (
      ~is_open: bool,
      ~inline_padding: (doc, doc)=(Doc.empty, Doc.empty),
      ~enforce_inline: bool,
      child: (~enforce_inline: bool) => doc,
    )
    : doc => {
  let (left, right) = inline_padding;
  let inline_choice =
    Doc.hcats([
      left |> tag_Padding,
      child(~enforce_inline=true),
      right |> tag_Padding,
    ]);
  let para_choice = indent_and_align(child(~enforce_inline=false));
  let (inline_choice, para_choice) =
    is_open
      ? (
        inline_choice |> tag_OpenChild(~is_inline=true),
        para_choice |> tag_OpenChild(~is_inline=false),
      )
      : (
        inline_choice |> tag_ClosedChild(~is_inline=true),
        para_choice |> tag_ClosedChild(~is_inline=false),
      );
  enforce_inline
    ? inline_choice
    : Doc.(
        choices([inline_choice, hcats([Linebreak, para_choice, Linebreak])])
      );
};

let pad_open_child = pad_child(~is_open=true);
let pad_closed_child = pad_child(~is_open=false);

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
  DocOfDelim.empty_hole_doc(~caret?, ~steps, u);

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
  let open_group = open_delim |> tag_DelimGroup;
  let close_group = close_delim |> tag_DelimGroup;
  Doc.hcats([
    open_group,
    body |> pad_open_child(~enforce_inline),
    close_group,
  ]);
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
  let open_group = open_delim |> tag_DelimGroup;
  let close_group = close_delim |> tag_DelimGroup;
  Doc.hcats([
    open_group,
    body |> pad_open_child(~enforce_inline),
    close_group,
  ]);
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
  let open_group = open_delim |> tag_DelimGroup;
  let close_group = close_delim |> tag_DelimGroup;
  Doc.hcats([
    open_group,
    body |> pad_open_child(~enforce_inline),
    close_group,
  ]);
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
        Doc.hcats([
          lam_delim,
          p |> pad_closed_child(~enforce_inline),
          open_delim,
        ])
      | Some(ann) =>
        Doc.hcats([
          lam_delim,
          p |> pad_closed_child(~enforce_inline),
          colon_delim,
          ann |> pad_closed_child(~enforce_inline),
          open_delim,
        ])
      };
    doc |> tag_DelimGroup;
  };
  let close_group = close_delim |> tag_DelimGroup;
  Doc.hcats([
    open_group,
    body |> pad_open_child(~enforce_inline),
    close_group,
  ]);
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
  let open_group = case_delim |> tag_DelimGroup;
  let close_group = end_delim |> tag_DelimGroup;
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
  let open_group = case_delim |> tag_DelimGroup;
  let close_group =
    Doc.(
      choices([
        hseps([end_delim, ann(~enforce_inline=true)]),
        vseps([end_delim, indent_and_align(ann(~enforce_inline=false))]),
      ])
      |> tag_DelimGroup
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
      p
      |> pad_closed_child(
           ~inline_padding=(space, space),
           ~enforce_inline=false,
         ),
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
          |> pad_closed_child(
               ~inline_padding=(space, space),
               ~enforce_inline=false,
             ),
          eq_delim,
        ])
      | Some(ann) =>
        Doc.hcats([
          let_delim,
          p
          |> pad_closed_child(
               ~inline_padding=(space, space),
               ~enforce_inline=false,
             ),
          colon_delim,
          ann
          |> pad_closed_child(
               ~inline_padding=(space, space),
               ~enforce_inline=false,
             ),
          eq_delim,
        ])
      };
    doc |> tag_DelimGroup;
  };
  let close_group = in_delim |> tag_DelimGroup;
  Doc.hcats([
    open_group,
    def
    |> pad_open_child(~inline_padding=(space, space), ~enforce_inline=false),
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
  | [(_, hd_doc)] => hd_doc
  | [(_, hd_doc), ...tl] =>
    tl
    |> List.fold_left(
         (tuple_so_far, (elem, elem_doc)) => {
           let comma_index = Skel.rightmost_tm_index(elem) + Seq.length(seq);
           let comma_doc =
             doc_of_comma(~steps=steps @ [comma_index], ~enforce_inline, ());
           Doc.hcats([
             tuple_so_far,
             comma_doc,
             elem_doc |> tag_OpenChild(~is_inline=true),
           ]);
         },
         hd_doc |> tag_OpenChild(~is_inline=true),
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
      skel1_doc |> tag_OpenChild(~is_inline=true),
      op_doc |> pad_operator(~inline_padding=inline_padding_of_operator(op)),
      skel2_doc |> tag_OpenChild(~is_inline=true),
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

module Typ = {
  let is_space = _ => false;
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

  let rec doc_of_htyp =
          (~steps: CursorPath.steps, ~enforce_inline: bool, ty: HTyp.t): doc =>
    switch (ty) {
    | Hole => doc_of_EmptyHole(~steps, "?")
    | Unit => doc_of_Unit(~steps, ())
    | Num => doc_of_Num(~steps, ())
    | Bool => doc_of_Bool(~steps, ())
    | List(ty) =>
      Doc.hcats([
        Text("["),
        doc_of_htyp(~steps=steps @ [0], ty)
        |> pad_open_child(~enforce_inline),
        Text("]"),
      ])
    | Arrow(ty1, ty2)
    | Prod(ty1, ty2)
    | Sum(ty1, ty2) =>
      let padded_op =
        switch (ty) {
        | Arrow(_) =>
          Doc.(
            hcats([
              choices([Linebreak, space]),
              Text(LangUtil.typeArrowSym ++ " "),
            ])
          )
        | Prod(_) => Doc.(hcats([Text(","), choices([Linebreak, space])]))
        | _sum => Doc.(hcats([choices([Linebreak, space]), Text("| ")]))
        };
      Doc.hcats([
        doc_of_htyp(~steps=steps @ [0], ~enforce_inline, ty1),
        padded_op,
        doc_of_htyp(~steps=steps @ [1], ~enforce_inline, ty2),
      ]);
    };

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
    doc_of_op(~steps, UHTyp.string_of_operator(op), ())
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
};

module Pat = {
  let is_space = UHPat.is_Space;
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
    op |> is_space
      ? doc_of_space_op(~steps, ())
      : doc_of_op(~steps, UHPat.string_of_operator(op), ())
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
};

module Exp = {
  let is_space = UHExp.is_Space;
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
      | Some((leading, (concluding, concluding_doc))) =>
        List.fold_right(
          ((hd, hd_doc), tl_doc) =>
            Doc.vsep(hd_doc, tl_doc)
            |> Doc.tag(TermTag.mk_Term(~shape=ExpSubBlock(hd), ())),
          leading,
          concluding_doc
          |> Doc.tag(TermTag.mk_Term(~shape=ExpSubBlock(concluding), ())),
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
    op |> is_space
      ? doc_of_space_op(~steps, ())
      : doc_of_op(~steps, UHExp.string_of_operator(op), ())
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
};
