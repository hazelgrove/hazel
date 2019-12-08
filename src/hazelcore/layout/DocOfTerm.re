open GeneralUtil;
open ViewUtil;

module Tag = {
  type term_shape =
    | TypOperand(UHTyp.operand)
    // invariant: skels do not contain Prod
    | TypBinOp(UHTyp.operator, UHTyp.skel, UHTyp.skel)
    | TypNProd(list(UHTyp.skel))
    | PatOperand(UHPat.operand)
    // invariant: skels do not contain Comma
    | PatBinOp(ErrStatus.t, UHPat.operator, UHPat.skel, UHPat.skel)
    | PatNTuple(ErrStatus.t, list(UHPat.skel))
    | ExpOperand(UHExp.operand)
    | ExpRule(UHExp.rule)
    // invariant: skels do not contain Comma
    | ExpBinOp(ErrStatus.t, UHExp.operator, UHExp.skel, UHExp.skel)
    | ExpNTuple(ErrStatus.t, list(UHExp.skel))
    // nested blocks starting with header line
    | ExpSubBlock(UHExp.line);

  type t =
    | DelimGroup
    | Padding({
        path_before: CursorPath.t,
        path_after: CursorPath.t,
      })
    | Delim({
        path: delim_path,
        caret: option(Side.t),
      })
    | Op({
        steps: CursorPath.steps,
        caret: option(Side.t),
      })
    | Text({
        steps: CursorPath.steps,
        length: int,
        caret: option(int),
      })
    | Term({
        shape: term_shape,
        has_cursor: bool,
      });

  let mk_Padding = (~path_before: CursorPath.t, ~path_after: CursorPath.t): t =>
    Padding({path_before, path_after});
  let mk_Delim = (~caret: option(Side.t)=?, ~path: delim_path, ()): t =>
    Delim({caret, path});
  let mk_Op = (~caret: option(Side.t)=?, ~steps: CursorPath.steps, ()): t =>
    Op({caret, steps});
  let mk_Text =
      (~caret: option(int)=?, ~steps: CursorPath.steps, ~length: int, ()): t =>
    Text({caret, steps, length});
  let mk_Term = (~has_cursor=false, ~shape: term_shape, ()): t =>
    Term({has_cursor, shape});
};

type doc = Doc.t(Tag.t);

let space = Doc.space;
let indent = Doc.Text("  ");

let indent_and_align = (d: doc): doc => Doc.(hcats([indent, align(d)]));

let doc_of_delim =
    (~caret: option(Side.t)=?, ~path: delim_path, delim_text: string): doc =>
  Doc.Text(delim_text) |> Doc.tag(Tag.mk_Delim(~caret?, ~path, ()));

let doc_of_comma =
    (
      ~caret: option(Side.t)=?,
      ~steps: CursorPath.steps,
      ~enforce_inline: bool,
      (),
    )
    : doc => {
  let comma_doc = Doc.(Text(",") |> tag(Tag.mk_Op(~caret?, ~steps, ())));
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

let doc_of_EmptyHole = (~steps: CursorPath.steps, u: string): doc =>
  doc_of_delim(~path=(steps, 0), u);

let doc_of_Wild = (~steps: CursorPath.steps): doc =>
  Doc.Text("_") |> Doc.tag(Tag.mk_Delim(~path=(steps, 0), ()));

let doc_of_Var = (~steps: CursorPath.steps, x: Var.t): doc =>
  Doc.Text(x) |> Doc.tag(Tag.mk_Text(~steps, ~length=String.length(x), ()));

let doc_of_NumLit = (~steps: CursorPath.steps, n: int): doc => {
  let s = string_of_int(n);
  Doc.Text(s) |> Doc.tag(Tag.mk_Text(~steps, ~length=String.length(s), ()));
};

let doc_of_BoolLit = (~steps: CursorPath.steps, b: bool): doc => {
  let s = string_of_bool(b);
  Doc.Text(s) |> Doc.tag(Tag.mk_Text(~steps, ~length=String.length(s), ()));
};

let doc_of_ListNil = (~steps: CursorPath.steps): doc =>
  Doc.Text("[]") |> Doc.tag(Tag.mk_Delim(~path=(steps, 0), ()));

let doc_of_Parenthesized =
    (
      ~steps: CursorPath.steps,
      ~enforce_inline: bool,
      body: (~enforce_inline: bool) => doc,
    )
    : doc => {
  let open_group =
    doc_of_delim(~path=(steps, 0), "(") |> Doc.tag(Tag.DelimGroup);
  let close_group =
    doc_of_delim(~path=(steps, 1), ")") |> Doc.tag(Tag.DelimGroup);
  Doc.hcats([open_group, body |> pad_child(~enforce_inline), close_group]);
};

let doc_of_List =
    (
      ~steps: CursorPath.steps,
      ~enforce_inline: bool,
      body: (~enforce_inline: bool) => doc,
    )
    : doc => {
  let open_group =
    doc_of_delim(~path=(steps, 0), "[") |> Doc.tag(Tag.DelimGroup);
  let close_group =
    doc_of_delim(~path=(steps, 1), "]") |> Doc.tag(Tag.DelimGroup);
  Doc.hcats([open_group, body |> pad_child(~enforce_inline), close_group]);
};

let doc_of_Inj =
    (
      ~steps: CursorPath.steps,
      ~enforce_inline: bool,
      inj_side: InjSide.t,
      body: (~enforce_inline: bool) => doc,
    )
    : doc => {
  let open_group =
    doc_of_delim(
      ~path=(steps, 0),
      "inj[" ++ InjSide.to_string(inj_side) ++ "](",
    )
    |> Doc.tag(Tag.DelimGroup);
  let close_group =
    doc_of_delim(~path=(steps, 1), ")") |> Doc.tag(Tag.DelimGroup);
  Doc.hcats([open_group, body |> pad_child(~enforce_inline), close_group]);
};

let doc_of_Lam =
    (
      ~steps: CursorPath.steps,
      ~enforce_inline: bool,
      p: (~enforce_inline: bool) => doc,
      ann: option((~enforce_inline: bool) => doc),
      body: (~enforce_inline: bool) => doc,
    )
    : doc => {
  let open_group = {
    let lam_delim = doc_of_delim(~path=(steps, 0), LangUtil.lamSym);
    let dot_delim = doc_of_delim(~path=(steps, 2), ".{");
    let doc =
      switch (ann) {
      | None =>
        Doc.hcats([lam_delim, p |> pad_child(~enforce_inline), dot_delim])
      | Some(ann) =>
        let colon_delim = doc_of_delim(~path=(steps, 1), ":");
        Doc.hcats([
          lam_delim,
          p |> pad_child(~enforce_inline),
          colon_delim,
          ann |> pad_child(~enforce_inline),
          dot_delim,
        ]);
      };
    doc |> Doc.tag(Tag.DelimGroup);
  };
  let close_group =
    doc_of_delim(~path=(steps, 3), "}") |> Doc.tag(Tag.DelimGroup);
  Doc.hcats([open_group, body |> pad_child(~enforce_inline), close_group]);
};

let doc_of_Case =
    (
      ~steps: CursorPath.steps,
      scrut: (~enforce_inline: bool) => doc,
      rules: list(doc),
      ann: option((~enforce_inline: bool) => doc),
    )
    : doc => {
  let open_group =
    doc_of_delim(~path=(steps, 0), "case") |> Doc.tag(Tag.DelimGroup);
  let close_group =
    switch (ann) {
    | None => doc_of_delim(~path=(steps, 1), "end")
    | Some(ann) =>
      let end_delim = doc_of_delim(~path=(steps, 1), "end :");
      Doc.(
        choices([
          hseps([end_delim, ann(~enforce_inline=true)]),
          vseps([end_delim, indent_and_align(ann(~enforce_inline=false))]),
        ])
      );
    };
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

let rec doc_of_BinOp =
        (
          ~mk_BinOp_tag:
             (
               ErrStatus.t,
               'operator,
               Skel.t('operator),
               Skel.t('operator)
             ) =>
             Tag.t,
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
  let _doc_of_BinOp =
    doc_of_BinOp(
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
    let skel1_doc = _doc_of_BinOp(skel1);
    let skel2_doc = _doc_of_BinOp(skel2);
    Doc.hcats([
      skel1_doc,
      op_doc |> pad_operator(~inline_padding=inline_padding_of_operator(op)),
      skel2_doc,
    ])
    |> Doc.tag(mk_BinOp_tag(err, op, skel1, skel2));
  };
};

let doc_of_NTuple =
    (
      ~is_Comma: 'operator => bool,
      ~get_tuple_elements: Skel.t('operator) => list(Skel.t('operator)),
      ~mk_NTuple_tag: (ErrStatus.t, list(Skel.t('operator))) => Tag.t,
      ~mk_BinOp_tag:
         (ErrStatus.t, 'operator, Skel.t('operator), Skel.t('operator)) =>
         Tag.t,
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
         doc_of_BinOp(
           ~mk_BinOp_tag,
           ~doc_of_operand,
           ~doc_of_operator,
           ~inline_padding_of_operator,
           ~enforce_inline,
           ~steps,
           ~seq,
         ),
       );
  let tuple_doc =
    switch (zip(elems, elem_docs)) {
    | [] => failwith(__LOC__ ++ ": found empty tuple")
    | [(_, hd_doc), ...tl] =>
      tl
      |> List.fold_left(
           (tuple_so_far, (elem, elem_doc)) => {
             let comma_index =
               Skel.rightmost_tm_index(elem) + Seq.length(seq);
             let comma_doc =
               doc_of_comma(
                 ~steps=steps @ [comma_index],
                 ~enforce_inline,
                 (),
               );
             Doc.hcats([tuple_so_far, comma_doc, elem_doc]);
           },
           hd_doc,
         )
    };
  switch (skel) {
  | BinOp(err, op, _, _) when op |> is_Comma =>
    tuple_doc |> Doc.tag(mk_NTuple_tag(err, elems))
  | _ =>
    // 1-tuple, no need to tag
    tuple_doc
  };
};

module Typ = {
  let inline_padding_of_operator =
    fun
    | UHTyp.Prod => (Doc.empty, Doc.space)
    | Arrow
    | Sum => (Doc.space, Doc.space);

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
      ~is_Comma=UHTyp.is_Prod,
      ~get_tuple_elements=UHTyp.get_prod_elements,
      ~mk_NTuple_tag=
        (_err, elems) => Tag.mk_Term(~shape=TypNProd(elems), ()),
      ~mk_BinOp_tag=
        (_err, op, skel1, skel2) =>
          Tag.mk_Term(~shape=TypBinOp(op, skel1, skel2), ()),
      ~doc_of_operand,
      ~doc_of_operator,
      ~inline_padding_of_operator,
      ~steps,
      ~enforce_inline,
      opseq,
    )
  and doc_of_operator = (~steps: CursorPath.steps, op: UHTyp.operator): doc =>
    Doc.Text(Associator.Typ.string_of_op(op))
    |> Doc.tag(Tag.mk_Op(~steps, ()))
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
      | Unit => doc_of_delim(~path=(steps, 0), "()")
      | Num => doc_of_delim(~path=(steps, 0), LangUtil.typeN)
      | Bool => doc_of_delim(~path=(steps, 0), LangUtil.typeB)
      | Parenthesized(body) =>
        let body_doc = doc(~steps=steps @ [0], body);
        doc_of_Parenthesized(~steps, ~enforce_inline, body_doc);
      | List(body) =>
        let body_doc = doc(~steps=steps @ [0], body);
        doc_of_List(~steps, ~enforce_inline, body_doc);
      };
    doc |> Doc.tag(Tag.mk_Term(~shape=TypOperand(operand), ()));
  };
};

module Pat = {
  let inline_padding_of_operator =
    Doc.(
      fun
      | UHPat.Comma => (empty, space)
      | Space
      | Cons => (empty, empty)
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
      ~is_Comma=UHPat.is_Comma,
      ~get_tuple_elements=UHPat.get_tuple_elements,
      ~mk_NTuple_tag=
        (err, elems) => Tag.mk_Term(~shape=PatNTuple(err, elems), ()),
      ~mk_BinOp_tag=
        (err, op, skel1, skel2) =>
          Tag.mk_Term(~shape=PatBinOp(err, op, skel1, skel2), ()),
      ~doc_of_operand,
      ~doc_of_operator,
      ~inline_padding_of_operator,
      ~steps,
      ~enforce_inline,
      opseq,
    )
  and doc_of_operator = (~steps: CursorPath.steps, op: UHPat.operator): doc =>
    Doc.Text(Associator.Pat.string_of_op(op))
    |> Doc.tag(Tag.mk_Op(~steps, ()))
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
      | Wild(_) => doc_of_Wild(~steps)
      | Var(_, _, x) => doc_of_Var(~steps, x)
      | NumLit(_, n) => doc_of_NumLit(~steps, n)
      | BoolLit(_, b) => doc_of_BoolLit(~steps, b)
      | ListNil(_) => doc_of_ListNil(~steps)
      | Parenthesized(body) =>
        let body_doc = doc(~steps=steps @ [0], body);
        doc_of_Parenthesized(~steps, ~enforce_inline, body_doc);
      | Inj(_, side, body) =>
        let body_doc = doc(~steps=steps @ [0], body);
        doc_of_Inj(~steps, ~enforce_inline, side, body_doc);
      };
    doc |> Doc.tag(Tag.mk_Term(~shape=PatOperand(operand), ()));
  };
};

module Exp = {
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

  let rec doc =
          (~steps: CursorPath.steps, ~enforce_inline: bool, e: UHExp.t): doc =>
    switch (e) {
    | E2(e2) => enforce_inline ? Fail : doc_of_block(~steps, e2)
    | E1(e1) => doc_of_opseq(~steps, ~enforce_inline, e1)
    | E0(e0) => doc_of_operand(~steps, ~enforce_inline, e0)
    }
  and doc_of_block = (~steps: CursorPath.steps, block: UHExp.block): doc =>
    switch (block |> split_last) {
    | None => failwith(__LOC__ ++ ": doc_of_block expects a non-empty block")
    | Some((leading, concluding)) =>
      fold_right_i(
        ((i, hd_line), tl_doc) => {
          let hd_doc = doc_of_line(~steps=steps @ [i], hd_line);
          Doc.vsep(hd_doc, tl_doc)
          |> Doc.tag(Tag.mk_Term(~shape=ExpSubBlock(hd_line), ()));
        },
        leading,
        doc_of_line(~steps=steps @ [List.length(block) - 1], concluding),
      )
    }
  and doc_of_line = (~steps: CursorPath.steps, line: UHExp.line): doc =>
    switch (line) {
    | EmptyLine =>
      Doc.Text("") |> Doc.tag(Tag.mk_Text(~length=0, ~steps, ()))
    | ExpLine(opseq) => doc_of_opseq(~steps, ~enforce_inline=false, opseq)
    | LetLine(p, ann, def) =>
      let delim_group_1 = {
        let let_doc = doc_of_delim(~path=(steps, 0), "let");
        let p_doc = Pat.doc(~steps=steps @ [0], p);
        let eq_doc = doc_of_delim(~path=(steps, 2), "=");
        let doc =
          switch (ann) {
          | None =>
            Doc.hcats([
              let_doc,
              p_doc
              |> pad_child(
                   ~inline_padding=(space, space),
                   ~enforce_inline=false,
                 ),
              eq_doc,
            ])
          | Some(ann) =>
            let colon_doc = doc_of_delim(~path=(steps, 1), ":");
            let ann_doc = Typ.doc(~steps=steps @ [1], ann);
            Doc.hcats([
              let_doc,
              p_doc
              |> pad_child(
                   ~inline_padding=(space, space),
                   ~enforce_inline=false,
                 ),
              colon_doc,
              ann_doc
              |> pad_child(
                   ~inline_padding=(space, space),
                   ~enforce_inline=false,
                 ),
              eq_doc,
            ]);
          };
        doc |> Doc.tag(Tag.DelimGroup);
      };
      let def_doc = doc(~steps=steps @ [2], def);
      let delim_group_2 =
        doc_of_delim(~path=(steps, 3), "in") |> Doc.tag(Tag.DelimGroup);
      Doc.hcats([
        delim_group_1,
        def_doc
        |> pad_child(~inline_padding=(space, space), ~enforce_inline=false),
        delim_group_2,
      ]);
    }
  and doc_of_opseq =
      (~steps: CursorPath.steps, ~enforce_inline: bool, opseq: UHExp.opseq)
      : doc =>
    doc_of_NTuple(
      ~is_Comma=UHExp.is_Comma,
      ~get_tuple_elements=UHExp.get_tuple_elements,
      ~mk_NTuple_tag=
        (err, elems) => Tag.mk_Term(~shape=ExpNTuple(err, elems), ()),
      ~mk_BinOp_tag=
        (err, op, skel1, skel2) =>
          Tag.mk_Term(~shape=ExpBinOp(err, op, skel1, skel2), ()),
      ~doc_of_operand,
      ~doc_of_operator,
      ~inline_padding_of_operator,
      ~steps,
      ~enforce_inline,
      opseq,
    )
  and doc_of_operator = (~steps: CursorPath.steps, op: UHExp.operator): doc =>
    Doc.Text(Associator.Exp.string_of_op(op))
    |> Doc.tag(Tag.mk_Op(~steps, ()))
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
      | ListNil(_) => doc_of_ListNil(~steps)
      | Lam(_, p, ann, body) =>
        let p_doc = Pat.doc(~steps=steps @ [0], p);
        let ann_doc =
          ann |> Opt.map(ann => Typ.doc(~steps=steps @ [1], ann));
        let body_doc = doc(~steps=steps @ [2], body);
        doc_of_Lam(~steps, ~enforce_inline, p_doc, ann_doc, body_doc);
      | Inj(_, side, body) =>
        let body_doc = doc(~steps=steps @ [0], body);
        doc_of_Inj(~steps, ~enforce_inline, side, body_doc);
      | Parenthesized(body) =>
        let body_doc = doc(~steps=steps @ [0], body);
        doc_of_Parenthesized(~steps, ~enforce_inline, body_doc);
      | Case(_, scrut, rules, ann) =>
        if (enforce_inline) {
          Fail;
        } else {
          let scrut_doc = doc(~steps=steps @ [0], scrut);
          let rule_docs =
            rules
            |> List.mapi((i, rule) =>
                 doc_of_rule(~steps=steps @ [1 + i], rule)
               );
          let ann_doc =
            ann
            |> Opt.map(ann =>
                 Typ.doc(~steps=steps @ [1 + List.length(rules)], ann)
               );
          doc_of_Case(~steps, scrut_doc, rule_docs, ann_doc);
        }
      | ApPalette(_) => failwith("unimplemented: doc_of_exp/ApPalette")
      };
    doc |> Doc.tag(Tag.mk_Term(~shape=ExpOperand(operand), ()));
  }
  and doc_of_rule =
      (~steps: CursorPath.steps, Rule(p, clause) as rule: UHExp.rule): doc => {
    let delim_group = {
      let bar_delim = doc_of_delim(~path=(steps, 0), "|");
      let p_doc = Pat.doc(~steps=steps @ [0], p);
      let arrow_delim =
        doc_of_delim(~path=(steps, 1), LangUtil.caseArrowSym);
      Doc.hcats([
        bar_delim,
        p_doc
        |> pad_child(~inline_padding=(space, space), ~enforce_inline=false),
        arrow_delim,
      ]);
    };
    let clause_doc = doc(~steps=steps @ [1], clause);
    Doc.(
      hcats([
        choices([
          hseps([delim_group, clause_doc(~enforce_inline=true)]),
          vseps([
            delim_group,
            indent_and_align(clause_doc(~enforce_inline=false)),
          ]),
        ]),
      ])
    )
    |> Doc.tag(Tag.mk_Term(~shape=ExpRule(rule), ()));
  };
};
