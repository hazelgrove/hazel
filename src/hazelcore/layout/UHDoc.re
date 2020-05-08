open Pretty;

type t = Doc.t(UHAnnot.t);

let empty_ = Doc.empty();
let space_ = Doc.space();

let indent_and_align_ = doc =>
  Doc.(hcat(annot(UHAnnot.Indent, indent()), align(doc)));

module Delim = {
  let mk = (~index: int, delim_text: string): t =>
    Doc.annot(
      UHAnnot.mk_Token(
        ~len=StringUtil.utf8_length(delim_text),
        ~shape=Delim(index),
        (),
      ),
      Doc.text(delim_text),
    );

  let empty_hole_doc = (hole_lbl: string): t => {
    let len = hole_lbl |> StringUtil.utf8_length;
    Doc.text(hole_lbl)
    |> Doc.annot(UHAnnot.HoleLabel({len: len}))
    |> Doc.annot(UHAnnot.mk_Token(~shape=Delim(0), ~len, ()));
  };

  let open_List = () => mk(~index=0, "[");
  let close_List = () => mk(~index=1, "]");

  let open_Parenthesized = () => mk(~index=0, "(");
  let close_Parenthesized = () => mk(~index=1, ")");

  let open_Inj = (inj_side: InjSide.t) =>
    mk(~index=0, "inj[" ++ InjSide.to_string(inj_side) ++ "](");
  let close_Inj = () => mk(~index=1, ")");

  let sym_Lam = () => mk(~index=0, UnicodeConstants.lamSym);
  let colon_Lam = () => mk(~index=1, ":");
  let open_Lam = () => mk(~index=2, ".{");
  let close_Lam = () => mk(~index=3, "}");

  let open_Case = () => mk(~index=0, "case");
  let close_Case = () => mk(~index=1, "end");
  let close_Case_ann = () => mk(~index=1, "end :");

  let bar_Rule = () => mk(~index=0, "|");
  let arrow_Rule = () => mk(~index=1, UnicodeConstants.caseArrowSym);

  let let_LetLine = () => mk(~index=0, "let");
  let colon_LetLine = () => mk(~index=1, ":");
  let eq_LetLine = () => mk(~index=2, "=");
  let in_LetLine = () => mk(~index=3, "in");
};

let annot_Indent = Doc.annot(UHAnnot.Indent);
let annot_Padding = (d: Doc.t(UHAnnot.t)) =>
  switch (d.doc) {
  | Text("") => d
  | _ => Doc.annot(UHAnnot.Padding, d)
  };
let annot_DelimGroup = Doc.annot(UHAnnot.DelimGroup);
let annot_OpenChild = (~is_inline) =>
  Doc.annot(UHAnnot.mk_OpenChild(~is_inline, ()));
let annot_ClosedChild = (~is_inline) =>
  Doc.annot(UHAnnot.mk_ClosedChild(~is_inline, ()));
let annot_Step = step => Doc.annot(UHAnnot.Step(step));
let annot_Var =
    (~sort: TermSort.t, ~err: ErrStatus.t=NotInHole, ~verr: VarErrStatus.t) =>
  Doc.annot(
    UHAnnot.mk_Term(~sort, ~shape=TermShape.mk_Var(~err, ~verr, ()), ()),
  );
let annot_Operand = (~sort: TermSort.t, ~err: ErrStatus.t=NotInHole) =>
  Doc.annot(
    UHAnnot.mk_Term(~sort, ~shape=TermShape.mk_Operand(~err, ()), ()),
  );
let annot_Case = (~err: ErrStatus.t) =>
  Doc.annot(UHAnnot.mk_Term(~sort=Exp, ~shape=Case({err: err}), ()));

let indent_and_align = (d: t): t =>
  Doc.(hcats([indent() |> annot_Indent, align(d)]));

let mk_text = (s: string): t =>
  Doc.annot(
    UHAnnot.mk_Token(~shape=Text, ~len=StringUtil.utf8_length(s), ()),
    Doc.text(s),
  );

let pad_operator =
    (~inline_padding as (left, right): (t, t), operator: t): t => {
  open Doc;
  let ldoc = left == empty_ ? empty_ : left |> annot_Padding;
  let rdoc = right == empty_ ? empty_ : right |> annot_Padding;
  choices([
    hcats([ldoc, operator, rdoc]),
    hcats([linebreak(), operator, rdoc]),
  ]);
};

let mk_op = (op_text: string): t =>
  Doc.annot(
    UHAnnot.mk_Token(~len=StringUtil.utf8_length(op_text), ~shape=Op, ()),
    Doc.text(op_text),
  );

let mk_space_op = Doc.annot(UHAnnot.SpaceOp, space_);

let user_newline =
  Doc.(
    hcats([
      space_ |> annot_Padding,
      text(UnicodeConstants.user_newline) |> annot(UHAnnot.UserNewline),
    ])
  );

type formattable_child = (~enforce_inline: bool) => t;
type formatted_child =
  | UserNewline(t)
  | EnforcedInline(t)
  | Unformatted(formattable_child);

let pad_child =
    (
      ~is_open: bool,
      ~inline_padding: (t, t)=(empty_, empty_),
      child: formatted_child,
    )
    : t => {
  open Doc;
  // TODO review child annotation and simplify if possible
  let annot_child = is_open ? annot_OpenChild : annot_ClosedChild;
  let inline_choice = child_doc => {
    let (left, right) = inline_padding;
    let lpadding = left == empty_ ? [] : [left |> annot_Padding];
    let rpadding = right == empty_ ? [] : [right |> annot_Padding];
    hcats([
      hcats(List.concat([lpadding, [child_doc], rpadding]))
      |> annot_child(~is_inline=true),
    ]);
  };
  let para_choice = child_doc =>
    child_doc |> indent_and_align |> annot_child(~is_inline=false);
  switch (child) {
  | EnforcedInline(child_doc) => inline_choice(child_doc)
  | UserNewline(child_doc) =>
    hcats([user_newline, linebreak(), para_choice(child_doc), linebreak()])
  | Unformatted(formattable_child) =>
    choices([
      inline_choice(formattable_child(~enforce_inline=true)),
      hcats([
        linebreak(),
        para_choice(formattable_child(~enforce_inline=false)),
        linebreak(),
      ]),
    ])
  };
};

let pad_open_child = pad_child(~is_open=true);
let pad_closed_child = pad_child(~is_open=false);

let pad_left_delimited_child =
    (~is_open: bool, ~inline_padding: t=empty_, child: formatted_child): t => {
  open Doc;
  let annot_child = is_open ? annot_OpenChild : annot_ClosedChild;
  let inline_choice = child_doc => {
    let lpadding =
      inline_padding == empty_ ? [] : [inline_padding |> annot_Padding];
    hcats(lpadding @ [child_doc]) |> annot_child(~is_inline=true);
  };
  let para_choice = child_doc =>
    child_doc |> indent_and_align |> annot_child(~is_inline=false);
  switch (child) {
  | EnforcedInline(child_doc) => inline_choice(child_doc)
  | UserNewline(child_doc) =>
    hcats([user_newline, linebreak(), para_choice(child_doc)])
  | Unformatted(formattable_child) =>
    choices([
      inline_choice(formattable_child(~enforce_inline=true)),
      hcats([
        linebreak(),
        para_choice(formattable_child(~enforce_inline=false)),
      ]),
    ])
  };
};

let mk_Unit = (): t => Delim.mk(~index=0, "()") |> annot_Operand(~sort=Typ);

let mk_Bool = (): t =>
  Delim.mk(~index=0, "Bool") |> annot_Operand(~sort=Typ);

let mk_Int = (): t => Delim.mk(~index=0, "Int") |> annot_Operand(~sort=Typ);

let mk_Float = (): t =>
  Delim.mk(~index=0, "Float") |> annot_Operand(~sort=Typ);

let hole_lbl = (u: MetaVar.t): string => string_of_int(u);
let hole_inst_lbl = (u: MetaVar.t, i: MetaVarInst.t): string =>
  StringUtil.cat([string_of_int(u), ":", string_of_int(i)]);

let mk_EmptyHole = (~sort: TermSort.t, hole_lbl: string): t =>
  Delim.empty_hole_doc(hole_lbl) |> annot_Operand(~sort);

let mk_Wild = (~err: ErrStatus.t): t =>
  Delim.mk(~index=0, "_") |> annot_Operand(~sort=Pat, ~err);

let mk_Var =
    (~sort: TermSort.t, ~err: ErrStatus.t, ~verr: VarErrStatus.t, x: Var.t): t =>
  mk_text(x) |> annot_Var(~sort, ~err, ~verr);

let mk_IntLit = (~sort: TermSort.t, ~err: ErrStatus.t, n: string): t =>
  mk_text(n) |> annot_Operand(~sort, ~err);

let mk_FloatLit = (~sort: TermSort.t, ~err: ErrStatus.t, f: string): t =>
  mk_text(f) |> annot_Operand(~sort, ~err);

let mk_BoolLit = (~sort: TermSort.t, ~err: ErrStatus.t, b: bool): t =>
  mk_text(string_of_bool(b)) |> annot_Operand(~sort, ~err);

let mk_ListNil = (~sort: TermSort.t, ~err: ErrStatus.t, ()): t =>
  Delim.mk(~index=0, "[]") |> annot_Operand(~sort, ~err);

let mk_Parenthesized = (~sort: TermSort.t, body: formatted_child): t => {
  let open_group = Delim.open_Parenthesized() |> annot_DelimGroup;
  let close_group = Delim.close_Parenthesized() |> annot_DelimGroup;
  Doc.hcats([open_group, body |> pad_open_child, close_group])
  |> annot_Operand(~sort);
};

let mk_List = (body: formatted_child): t => {
  let open_group = Delim.open_List() |> annot_DelimGroup;
  let close_group = Delim.close_List() |> annot_DelimGroup;
  Doc.hcats([open_group, body |> pad_open_child, close_group])
  |> annot_Operand(~sort=Typ);
};

let mk_Inj =
    (
      ~sort: TermSort.t,
      ~err: ErrStatus.t,
      ~inj_side: InjSide.t,
      body: formatted_child,
    )
    : t => {
  let open_group = Delim.open_Inj(inj_side) |> annot_DelimGroup;
  let close_group = Delim.close_Inj() |> annot_DelimGroup;
  Doc.hcats([open_group, body |> pad_open_child, close_group])
  |> annot_Operand(~sort, ~err);
};

let mk_Lam =
    (
      ~err: ErrStatus.t,
      p: formatted_child,
      ann: option(formatted_child),
      body: formatted_child,
    )
    : t => {
  let open_group = {
    let lam_delim = Delim.sym_Lam();
    let open_delim = Delim.open_Lam();
    let doc =
      switch (ann) {
      | None => Doc.hcats([lam_delim, p |> pad_closed_child, open_delim])
      | Some(ann) =>
        let colon_delim = Delim.colon_Lam();
        Doc.hcats([
          lam_delim,
          p |> pad_closed_child,
          colon_delim,
          ann |> pad_closed_child,
          open_delim,
        ]);
      };
    doc |> annot_DelimGroup;
  };
  let close_group = Delim.close_Lam() |> annot_DelimGroup;
  Doc.hcats([open_group, body |> pad_open_child, close_group])
  |> annot_Operand(~sort=Exp, ~err);
};

let mk_Case = (~err: ErrStatus.t, scrut: formatted_child, rules: list(t)): t => {
  let open_group = Delim.open_Case() |> annot_DelimGroup;
  let close_group = Delim.close_Case() |> annot_DelimGroup;
  Doc.(
    vseps(
      [
        hcats([
          open_group,
          scrut
          |> pad_left_delimited_child(~is_open=true, ~inline_padding=space_),
        ]),
        ...rules,
      ]
      @ [close_group],
    )
  )
  |> annot_Case(~err);
};

let mk_Case_ann =
    (
      ~err: ErrStatus.t,
      scrut: formatted_child,
      rules: list(t),
      ann: formatted_child,
    )
    : t => {
  let open_group = Delim.open_Case() |> annot_DelimGroup;
  let close_group = {
    let end_delim = Delim.close_Case_ann();
    Doc.hcats([
      end_delim,
      ann |> pad_left_delimited_child(~is_open=false, ~inline_padding=space_),
    ])
    |> annot_DelimGroup;
  };
  Doc.(
    vseps(
      [
        hcats([
          open_group,
          scrut
          |> pad_left_delimited_child(~is_open=true, ~inline_padding=space_),
        ]),
        ...rules,
      ]
      @ [close_group],
    )
  )
  |> annot_Case(~err);
};

let mk_Rule = (p: formatted_child, clause: formatted_child): t => {
  let delim_group =
    Doc.hcats([
      Delim.bar_Rule(),
      p |> pad_closed_child(~inline_padding=(space_, space_)),
      Delim.arrow_Rule(),
    ])
    |> annot_DelimGroup;
  Doc.hcats([
    delim_group,
    clause |> pad_left_delimited_child(~is_open=true, ~inline_padding=space_),
  ])
  |> Doc.annot(UHAnnot.mk_Term(~sort=Exp, ~shape=Rule, ()));
};

let mk_LetLine =
    (p: formatted_child, ann: option(formatted_child), def: formatted_child)
    : t => {
  let open_group = {
    let let_delim = Delim.let_LetLine();
    let eq_delim = Delim.eq_LetLine();
    let doc =
      switch (ann) {
      | None =>
        Doc.hcats([
          let_delim,
          p |> pad_closed_child(~inline_padding=(space_, space_)),
          eq_delim,
        ])
      | Some(ann) =>
        let colon_delim = Delim.colon_LetLine();
        Doc.hcats([
          let_delim,
          p |> pad_closed_child(~inline_padding=(space_, space_)),
          colon_delim,
          ann |> pad_closed_child(~inline_padding=(space_, space_)),
          eq_delim,
        ]);
      };
    doc |> annot_DelimGroup;
  };
  let close_group = Delim.in_LetLine() |> annot_DelimGroup;
  Doc.hcats([
    open_group,
    def |> pad_open_child(~inline_padding=(space_, space_)),
    close_group,
  ]);
};

let rec mk_BinOp =
        (
          ~sort: TermSort.t,
          ~mk_operand: (~enforce_inline: bool, 'operand) => t,
          ~mk_operator: 'operator => t,
          ~inline_padding_of_operator: 'operator => (t, t),
          ~enforce_inline: bool,
          ~seq: Seq.t('operand, 'operator),
          skel: Skel.t('operator),
        )
        : t => {
  let go =
    mk_BinOp(
      ~sort,
      ~mk_operand,
      ~mk_operator,
      ~inline_padding_of_operator,
      ~enforce_inline,
      ~seq,
    );
  switch (skel) {
  | Placeholder(n) =>
    let operand = seq |> Seq.nth_operand(n);
    mk_operand(~enforce_inline, operand) |> annot_Step(n);
  | BinOp(err, op, skel1, skel2) =>
    let op_index = Skel.rightmost_tm_index(skel1) + Seq.length(seq);
    let op_doc = mk_operator(op) |> annot_DelimGroup;
    let skel1_doc = go(skel1);
    let skel2_doc = go(skel2);
    Doc.hcats([
      skel1_doc |> annot_OpenChild(~is_inline=true),
      op_doc
      |> pad_operator(~inline_padding=inline_padding_of_operator(op))
      |> annot_Step(op_index),
      skel2_doc |> annot_OpenChild(~is_inline=true),
    ])
    |> Doc.annot(UHAnnot.mk_Term(~sort, ~shape=BinOp({err, op_index}), ()));
  };
};

let mk_NTuple =
    (
      ~sort: TermSort.t,
      ~get_tuple_elements: Skel.t('operator) => list(Skel.t('operator)),
      ~mk_operand: (~enforce_inline: bool, 'operand) => t,
      ~mk_operator: 'operator => t,
      ~inline_padding_of_operator: 'operator => (t, t),
      ~enforce_inline: bool,
      OpSeq(skel, seq): OpSeq.t('operand, 'operator),
    )
    : t => {
  let mk_BinOp =
    mk_BinOp(
      ~sort,
      ~mk_operand,
      ~mk_operator,
      ~inline_padding_of_operator,
      ~enforce_inline,
      ~seq,
    );
  switch (skel |> get_tuple_elements |> ListUtil.map_zip(mk_BinOp)) {
  | [] => failwith(__LOC__ ++ ": found empty tuple")
  | [(_, singleton_doc)] => singleton_doc
  | [(_, hd_doc), ...tl] =>
    let err =
      switch (skel) {
      | Placeholder(_) => assert(false)
      | BinOp(err, _, _, _) => err
      };
    let (doc, comma_indices) =
      tl
      |> List.fold_left(
           ((tuple, comma_indices), (elem, elem_doc)) => {
             // TODO multi-line tuples
             let comma_index =
               Skel.leftmost_tm_index(elem) - 1 + Seq.length(seq);
             let comma_doc =
               mk_op(",") |> annot_Step(comma_index) |> annot_DelimGroup;
             let doc =
               Doc.hcats([
                 tuple,
                 comma_doc,
                 space_ |> annot_Padding,
                 elem_doc |> annot_OpenChild(~is_inline=true),
               ]);
             (doc, [comma_index, ...comma_indices]);
           },
           (hd_doc |> annot_OpenChild(~is_inline=true), []),
         );
    doc
    |> Doc.annot(
         UHAnnot.mk_Term(~sort, ~shape=NTuple({comma_indices, err}), ()),
       );
  };
};

module Typ = {
  let inline_padding_of_operator =
    fun
    | UHTyp.Prod => (Doc.empty(), Doc.space())
    | Arrow
    | Sum => (Doc.space(), Doc.space());

  let mk_EmptyHole = mk_EmptyHole(~sort=Typ);
  let mk_Parenthesized = mk_Parenthesized(~sort=Typ);
  let mk_NTuple =
    mk_NTuple(
      ~sort=Typ,
      ~get_tuple_elements=UHTyp.get_prod_elements,
      ~inline_padding_of_operator,
    );

  let rec mk = (~enforce_inline: bool, uty: UHTyp.t): t =>
    mk_opseq(~enforce_inline, uty)
  and mk_opseq = (~enforce_inline: bool, opseq: UHTyp.opseq): t =>
    mk_NTuple(~mk_operand, ~mk_operator, ~enforce_inline, opseq)
  and mk_operator = (op: UHTyp.operator): t =>
    mk_op(UHTyp.string_of_operator(op))
  and mk_operand = (~enforce_inline: bool, operand: UHTyp.operand): t =>
    switch (operand) {
    | Hole => mk_EmptyHole("?")
    | Unit => mk_Unit()
    | Int => mk_Int()
    | Float => mk_Float()
    | Bool => mk_Bool()
    | Parenthesized(body) =>
      let body = mk_child(~enforce_inline, ~child_step=0, body);
      mk_Parenthesized(body);
    | List(body) =>
      let body = mk_child(~enforce_inline, ~child_step=0, body);
      mk_List(body);
    }
  and mk_child = (~enforce_inline, ~child_step, uty): formatted_child => {
    let formattable = (~enforce_inline: bool) =>
      mk(~enforce_inline, uty) |> annot_Step(child_step);
    enforce_inline
      ? EnforcedInline(formattable(~enforce_inline=true))
      : Unformatted(formattable);
  };
};

module Pat = {
  let inline_padding_of_operator =
    Doc.(
      fun
      | UHPat.Comma => (empty(), space())
      | Space
      | Cons => (empty(), empty())
    );

  let mk_EmptyHole = mk_EmptyHole(~sort=Pat);
  let mk_IntLit = mk_IntLit(~sort=Pat);
  let mk_FloatLit = mk_FloatLit(~sort=Pat);
  let mk_BoolLit = mk_BoolLit(~sort=Pat);
  let mk_ListNil = mk_ListNil(~sort=Pat);
  let mk_Var = mk_Var(~sort=Pat);
  let mk_Parenthesized = mk_Parenthesized(~sort=Pat);
  let mk_Inj = mk_Inj(~sort=Pat);
  let mk_NTuple =
    mk_NTuple(
      ~sort=Pat,
      ~get_tuple_elements=UHPat.get_tuple_elements,
      ~inline_padding_of_operator,
    );

  let rec mk = (~enforce_inline: bool, p: UHPat.t): t =>
    mk_opseq(~enforce_inline, p)
  and mk_opseq = (~enforce_inline: bool, opseq: UHPat.opseq): t =>
    mk_NTuple(~mk_operand, ~mk_operator, ~enforce_inline, opseq)
  and mk_operator = (op: UHPat.operator): t =>
    op |> UHPat.is_Space ? mk_space_op : mk_op(UHPat.string_of_operator(op))
  and mk_operand = (~enforce_inline: bool, operand: UHPat.operand): t =>
    switch (operand) {
    | EmptyHole(u) => mk_EmptyHole(hole_lbl(u + 1))
    | Wild(err) => mk_Wild(~err)
    | Var(err, verr, x) => mk_Var(~err, ~verr, x)
    | IntLit(err, n) => mk_IntLit(~err, n)
    | FloatLit(err, f) => mk_FloatLit(~err, f)
    | BoolLit(err, b) => mk_BoolLit(~err, b)
    | ListNil(err) => mk_ListNil(~err, ())
    | Parenthesized(body) =>
      let body = mk_child(~enforce_inline, ~child_step=0, body);
      mk_Parenthesized(body);
    | Inj(err, inj_side, body) =>
      let body = mk_child(~enforce_inline, ~child_step=0, body);
      mk_Inj(~err, ~inj_side, body);
    }
  and mk_child = (~enforce_inline, ~child_step, p): formatted_child => {
    let formattable = (~enforce_inline: bool) =>
      mk(~enforce_inline, p) |> annot_Step(child_step);
    enforce_inline
      ? EnforcedInline(formattable(~enforce_inline=true))
      : Unformatted(formattable);
  };
};

module Exp = {
  let inline_padding_of_operator =
    fun
    | UHExp.Space
    | Times
    | FTimes
    | Cons => (empty_, empty_)
    | Plus
    | Minus
    | FPlus
    | FMinus
    | LessThan
    | GreaterThan
    | Equals
    | FLessThan
    | FGreaterThan
    | FEquals
    | And
    | Or => (space_, space_)
    | Comma => (empty_, space_);

  let mk_EmptyHole = mk_EmptyHole(~sort=Exp);
  let mk_IntLit = mk_IntLit(~sort=Exp);
  let mk_FloatLit = mk_FloatLit(~sort=Exp);
  let mk_BoolLit = mk_BoolLit(~sort=Exp);
  let mk_ListNil = mk_ListNil(~sort=Exp);
  let mk_Var = mk_Var(~sort=Exp);
  let mk_Parenthesized = mk_Parenthesized(~sort=Exp);
  let mk_Inj = mk_Inj(~sort=Exp);
  let mk_NTuple =
    mk_NTuple(
      ~sort=Exp,
      ~get_tuple_elements=UHExp.get_tuple_elements,
      ~inline_padding_of_operator,
    );

  let annot_SubBlock = (~hd_index: int) =>
    Doc.annot(
      UHAnnot.mk_Term(~sort=Exp, ~shape=SubBlock({hd_index: hd_index}), ()),
    );

  let rec mk = (~enforce_inline: bool, e: UHExp.t): t =>
    mk_block(~enforce_inline, e)
  and mk_block = (~offset=0, ~enforce_inline: bool, block: UHExp.block): t =>
    if (enforce_inline && UHExp.Block.num_lines(block) > 1) {
      Doc.fail();
    } else {
      block
      |> List.mapi((i, line) =>
           mk_line(~enforce_inline, line) |> annot_Step(offset + i)
         )
      |> ListUtil.split_last
      |> (
        fun
        | None => failwith(__LOC__ ++ ": empty block")
        | Some((leading, concluding)) =>
          ListUtil.fold_right_i(
            ((i, hd_doc), tl_doc) =>
              Doc.vsep(hd_doc, tl_doc)
              |> annot_SubBlock(~hd_index=offset + i),
            leading,
            concluding
            |> annot_SubBlock(
                 ~hd_index=offset + UHExp.Block.num_lines(block) - 1,
               ),
          )
      );
    }
  and mk_line = (~enforce_inline: bool, line: UHExp.line): t =>
    switch (line) {
    | EmptyLine =>
      empty_
      |> Doc.annot(UHAnnot.mk_Token(~shape=Text, ~len=0, ()))
      |> Doc.annot(UHAnnot.EmptyLine)
    | ExpLine(opseq) => mk_opseq(~enforce_inline, opseq)
    | LetLine(p, ann, def) =>
      let p = Pat.mk_child(~enforce_inline, ~child_step=0, p);
      let ann =
        ann
        |> OptUtil.map(ann =>
             Typ.mk_child(~enforce_inline, ~child_step=1, ann)
           );
      let def = mk_child(~enforce_inline, ~child_step=2, def);
      mk_LetLine(p, ann, def) |> Doc.annot(UHAnnot.LetLine);
    }
  and mk_opseq = (~enforce_inline: bool, opseq: UHExp.opseq): t =>
    mk_NTuple(~mk_operand, ~mk_operator, ~enforce_inline, opseq)
  and mk_operator = (op: UHExp.operator): t =>
    op |> UHExp.is_Space ? mk_space_op : mk_op(UHExp.string_of_operator(op))
  and mk_operand = (~enforce_inline: bool, operand: UHExp.operand): t =>
    switch (operand) {
    | EmptyHole(u) => mk_EmptyHole(hole_lbl(u + 1))
    | Var(err, verr, x) => mk_Var(~err, ~verr, x)
    | IntLit(err, n) => mk_IntLit(~err, n)
    | FloatLit(err, f) => mk_FloatLit(~err, f)
    | BoolLit(err, b) => mk_BoolLit(~err, b)
    | ListNil(err) => mk_ListNil(~err, ())
    | Lam(err, p, ann, body) =>
      let p = Pat.mk_child(~enforce_inline, ~child_step=0, p);
      let ann =
        ann
        |> OptUtil.map(ann =>
             Typ.mk_child(~enforce_inline, ~child_step=1, ann)
           );
      let body = mk_child(~enforce_inline, ~child_step=2, body);
      mk_Lam(~err, p, ann, body);
    | Inj(err, inj_side, body) =>
      let body = mk_child(~enforce_inline, ~child_step=0, body);
      mk_Inj(~err, ~inj_side, body);
    | Parenthesized(body) =>
      let body = mk_child(~enforce_inline, ~child_step=0, body);
      mk_Parenthesized(body);
    | Case(err, scrut, rules, ann) =>
      if (enforce_inline) {
        Doc.fail();
      } else {
        let scrut = mk_child(~enforce_inline=false, ~child_step=0, scrut);
        let rules =
          rules
          |> List.mapi((i, rule) => mk_rule(rule) |> annot_Step(1 + i));
        switch (ann) {
        | None => mk_Case(~err, scrut, rules)
        | Some(ann) =>
          let ann =
            Typ.mk_child(
              ~enforce_inline=false,
              ~child_step=1 + List.length(rules),
              ann,
            );
          mk_Case_ann(~err, scrut, rules, ann);
        };
      }
    | ApPalette(_) => failwith("unimplemented: mk_exp/ApPalette")
    }
  and mk_rule = (Rule(p, clause): UHExp.rule): t => {
    let p = Pat.mk_child(~enforce_inline=false, ~child_step=0, p);
    let clause = mk_child(~enforce_inline=false, ~child_step=1, clause);
    mk_Rule(p, clause);
  }
  and mk_child = (~enforce_inline, ~child_step, e): formatted_child => {
    switch (e) {
    | [EmptyLine, ...subblock] =>
      if (enforce_inline) {
        EnforcedInline(Doc.fail());
      } else {
        let formatted =
          mk_block(~offset=1, ~enforce_inline=false, subblock)
          |> annot_Step(child_step);
        UserNewline(formatted);
      }
    | _ =>
      let formattable = (~enforce_inline) =>
        mk(~enforce_inline, e) |> annot_Step(child_step);
      enforce_inline
        ? EnforcedInline(formattable(~enforce_inline=true))
        : Unformatted(formattable);
    };
  };
};
