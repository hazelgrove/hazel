[@deriving sexp]
type t = Doc.t(TermAnnot.t);

let space = Doc.space;
let indent = Doc.indent;

let annot_Indent = Doc.annot(TermAnnot.Indent);
let annot_Padding = d =>
  d == Doc.empty ? d : d |> Doc.annot(TermAnnot.Padding);
let annot_DelimGroup = Doc.annot(TermAnnot.DelimGroup);
let annot_OpenChild = (~is_inline) =>
  Doc.annot(TermAnnot.mk_OpenChild(~is_inline, ()));
let annot_ClosedChild = (~is_inline) =>
  Doc.annot(TermAnnot.mk_ClosedChild(~is_inline, ()));
let annot_Step = step => Doc.annot(TermAnnot.Step(step));
let annot_Var =
    (
      ~family: TermFamily.t,
      ~err: ErrStatus.t=NotInHole,
      ~verr: VarErrStatus.t,
    ) =>
  Doc.annot(
    TermAnnot.mk_Term(~family, ~shape=TermShape.mk_Var(~err, ~verr, ()), ()),
  );
let annot_Operand = (~family: TermFamily.t, ~err: ErrStatus.t=NotInHole) =>
  Doc.annot(
    TermAnnot.mk_Term(~family, ~shape=TermShape.mk_Operand(~err, ()), ()),
  );
let annot_Case = (~err: ErrStatus.t) =>
  Doc.annot(TermAnnot.mk_Term(~family=Exp, ~shape=Case({err: err}), ()));

let indent_and_align = (d: t): t =>
  Doc.(hcats([indent |> annot_Indent, align(d)]));

let mk_text = (s: string): t =>
  Doc.Text(s)
  |> Doc.annot(TermAnnot.mk_Text(~length=StringUtil.utf8_length(s), ()));

let pad_operator =
    (~inline_padding as (left, right): (t, t), operator: t): t => {
  Doc.(
    choices([
      hcats([left |> annot_Padding, operator, right |> annot_Padding]),
      hcats([Linebreak, operator, right |> annot_Padding]),
    ])
  );
};

let mk_op = (op_text: string): t =>
  Doc.Text(op_text) |> Doc.annot(TermAnnot.mk_Op());

let space_op = Doc.space |> Doc.annot(TermAnnot.SpaceOp);

let user_newline =
  Doc.(
    hcats([
      space |> annot_Padding,
      Text(UnicodeConstants.user_newline) |> annot(TermAnnot.UserNewline),
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
      ~inline_padding: (t, t)=(Doc.empty, Doc.empty),
      child: formatted_child,
    )
    : t => {
  // TODO review child annotation and simplify if possible
  let annot_child = is_open ? annot_OpenChild : annot_ClosedChild;
  let inline_choice = child_doc => {
    let (left, right) = inline_padding;
    Doc.hcats([left |> annot_Padding, child_doc, right |> annot_Padding])
    |> annot_child(~is_inline=true);
  };
  let para_choice = child_doc =>
    child_doc |> indent_and_align |> annot_child(~is_inline=false);
  switch (child) {
  | EnforcedInline(child_doc) => inline_choice(child_doc)
  | UserNewline(child_doc) =>
    Doc.hcats([user_newline, Linebreak, para_choice(child_doc), Linebreak])
  | Unformatted(formattable_child) =>
    Doc.(
      choices([
        inline_choice(formattable_child(~enforce_inline=true)),
        hcats([
          Linebreak,
          para_choice(formattable_child(~enforce_inline=false)),
          Linebreak,
        ]),
      ])
    )
  };
};

let pad_open_child = pad_child(~is_open=true);
let pad_closed_child = pad_child(~is_open=false);

let pad_left_delimited_child =
    (~is_open: bool, ~inline_padding: t=Doc.empty, child: formatted_child): t => {
  let annot_child = is_open ? annot_OpenChild : annot_ClosedChild;
  let inline_choice = child_doc =>
    Doc.hcats([inline_padding |> annot_Padding, child_doc])
    |> annot_child(~is_inline=true);
  let para_choice = child_doc =>
    child_doc |> indent_and_align |> annot_child(~is_inline=false);
  switch (child) {
  | EnforcedInline(child_doc) => inline_choice(child_doc)
  | UserNewline(child_doc) =>
    Doc.hcats([user_newline, Linebreak, para_choice(child_doc)])
  | Unformatted(formattable_child) =>
    Doc.(
      choices([
        inline_choice(formattable_child(~enforce_inline=true)),
        hcats([
          Linebreak,
          para_choice(formattable_child(~enforce_inline=false)),
        ]),
      ])
    )
  };
};

let mk_Unit = (): t =>
  DelimDoc.mk(~index=0, "()") |> annot_Operand(~family=Typ);

let mk_Num = (): t =>
  DelimDoc.mk(~index=0, "Num") |> annot_Operand(~family=Typ);

let mk_Bool = (): t =>
  DelimDoc.mk(~index=0, "Bool") |> annot_Operand(~family=Typ);

let hole_lbl = (u: MetaVar.t): string => string_of_int(u);
let hole_inst_lbl = (u: MetaVar.t, i: MetaVarInst.t): string =>
  StringUtil.cat([string_of_int(u), ":", string_of_int(i)]);

let mk_EmptyHole = (~family: TermFamily.t, hole_lbl: string): t =>
  DelimDoc.empty_hole_doc(hole_lbl) |> annot_Operand(~family);

let mk_Wild = (~err: ErrStatus.t): t =>
  DelimDoc.mk(~index=0, "_") |> annot_Operand(~family=Pat, ~err);

let mk_Var =
    (
      ~family: TermFamily.t,
      ~err: ErrStatus.t,
      ~verr: VarErrStatus.t,
      x: Var.t,
    )
    : t =>
  mk_text(x) |> annot_Var(~family, ~err, ~verr);

let mk_NumLit = (~family: TermFamily.t, ~err: ErrStatus.t, n: int): t =>
  mk_text(string_of_int(n)) |> annot_Operand(~family, ~err);

let mk_BoolLit = (~family: TermFamily.t, ~err: ErrStatus.t, b: bool): t =>
  mk_text(string_of_bool(b)) |> annot_Operand(~family, ~err);

let mk_ListNil = (~family: TermFamily.t, ~err: ErrStatus.t, ()): t =>
  DelimDoc.mk(~index=0, "[]") |> annot_Operand(~family, ~err);

let mk_Parenthesized = (~family: TermFamily.t, body: formatted_child): t => {
  let open_group = DelimDoc.open_Parenthesized() |> annot_DelimGroup;
  let close_group = DelimDoc.close_Parenthesized() |> annot_DelimGroup;
  Doc.hcats([open_group, body |> pad_open_child, close_group])
  |> annot_Operand(~family);
};

let mk_List = (body: formatted_child): t => {
  let open_group = DelimDoc.open_List() |> annot_DelimGroup;
  let close_group = DelimDoc.close_List() |> annot_DelimGroup;
  Doc.hcats([open_group, body |> pad_open_child, close_group])
  |> annot_Operand(~family=Typ);
};

let mk_Inj =
    (
      ~family: TermFamily.t,
      ~err: ErrStatus.t,
      ~inj_side: InjSide.t,
      body: formatted_child,
    )
    : t => {
  let open_group = DelimDoc.open_Inj(inj_side) |> annot_DelimGroup;
  let close_group = DelimDoc.close_Inj() |> annot_DelimGroup;
  Doc.hcats([open_group, body |> pad_open_child, close_group])
  |> annot_Operand(~family, ~err);
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
    let lam_delim = DelimDoc.sym_Lam();
    let open_delim = DelimDoc.open_Lam();
    let doc =
      switch (ann) {
      | None => Doc.hcats([lam_delim, p |> pad_closed_child, open_delim])
      | Some(ann) =>
        let colon_delim = DelimDoc.colon_Lam();
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
  let close_group = DelimDoc.close_Lam() |> annot_DelimGroup;
  Doc.hcats([open_group, body |> pad_open_child, close_group])
  |> annot_Operand(~family=Exp, ~err);
};

let mk_Case = (~err: ErrStatus.t, scrut: formatted_child, rules: list(t)): t => {
  let open_group = DelimDoc.open_Case() |> annot_DelimGroup;
  let close_group = DelimDoc.close_Case() |> annot_DelimGroup;
  Doc.(
    vseps(
      [
        hcats([
          open_group,
          scrut
          |> pad_left_delimited_child(~is_open=true, ~inline_padding=space),
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
  let open_group = DelimDoc.open_Case() |> annot_DelimGroup;
  let close_group = {
    let end_delim = DelimDoc.close_Case_ann();
    Doc.hcats([
      end_delim,
      ann |> pad_left_delimited_child(~is_open=false, ~inline_padding=space),
    ])
    |> annot_DelimGroup;
  };
  Doc.(
    vseps(
      [
        hcats([
          open_group,
          scrut
          |> pad_left_delimited_child(~is_open=true, ~inline_padding=space),
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
      DelimDoc.bar_Rule(),
      p |> pad_closed_child(~inline_padding=(space, space)),
      DelimDoc.arrow_Rule(),
    ])
    |> annot_DelimGroup;
  Doc.hcats([
    delim_group,
    clause |> pad_left_delimited_child(~is_open=true, ~inline_padding=space),
  ])
  |> Doc.annot(TermAnnot.mk_Term(~family=Exp, ~shape=Rule, ()));
};

let mk_LetLine =
    (p: formatted_child, ann: option(formatted_child), def: formatted_child)
    : t => {
  let open_group = {
    let let_delim = DelimDoc.let_LetLine();
    let eq_delim = DelimDoc.eq_LetLine();
    let doc =
      switch (ann) {
      | None =>
        Doc.hcats([
          let_delim,
          p |> pad_closed_child(~inline_padding=(space, space)),
          eq_delim,
        ])
      | Some(ann) =>
        let colon_delim = DelimDoc.colon_LetLine();
        Doc.hcats([
          let_delim,
          p |> pad_closed_child(~inline_padding=(space, space)),
          colon_delim,
          ann |> pad_closed_child(~inline_padding=(space, space)),
          eq_delim,
        ]);
      };
    doc |> annot_DelimGroup;
  };
  let close_group = DelimDoc.in_LetLine() |> annot_DelimGroup;
  Doc.hcats([
    open_group,
    def |> pad_open_child(~inline_padding=(space, space)),
    close_group,
  ]);
};

let rec mk_BinOp =
        (
          ~family: TermFamily.t,
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
      ~family,
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
    let op_doc = mk_operator(op) |> annot_Step(op_index) |> annot_DelimGroup;
    let skel1_doc = go(skel1);
    let skel2_doc = go(skel2);
    Doc.hcats([
      skel1_doc |> annot_OpenChild(~is_inline=true),
      op_doc |> pad_operator(~inline_padding=inline_padding_of_operator(op)),
      skel2_doc |> annot_OpenChild(~is_inline=true),
    ])
    |> Doc.annot(
         TermAnnot.mk_Term(~family, ~shape=BinOp({err, op_index}), ()),
       );
  };
};

let mk_NTuple =
    (
      ~family: TermFamily.t,
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
      ~family,
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
               Doc.Text(",")
               |> Doc.annot(TermAnnot.mk_Op())
               |> annot_Step(comma_index)
               |> annot_DelimGroup;
             let doc =
               Doc.hcats([
                 tuple,
                 comma_doc,
                 space |> annot_Padding,
                 elem_doc |> annot_OpenChild(~is_inline=true),
               ]);
             (doc, [comma_index, ...comma_indices]);
           },
           (hd_doc |> annot_OpenChild(~is_inline=true), []),
         );
    doc
    |> Doc.annot(
         TermAnnot.mk_Term(~family, ~shape=NTuple({comma_indices, err}), ()),
       );
  };
};

module Typ = {
  let inline_padding_of_operator =
    fun
    | UHTyp.Prod => (Doc.empty, Doc.space)
    | Arrow
    | Sum => (Doc.space, Doc.space);

  let mk_EmptyHole = mk_EmptyHole(~family=Typ);
  let mk_Parenthesized = mk_Parenthesized(~family=Typ);
  let mk_NTuple =
    mk_NTuple(
      ~family=Typ,
      ~get_tuple_elements=UHTyp.get_prod_elements,
      ~inline_padding_of_operator,
    );

  let precedence_const = 0;
  let precedence_Prod = 1;
  let precedence_Sum = 2;
  let precedence_Arrow = 3;
  let precedence_ty = (ty: HTyp.t): int =>
    switch (ty) {
    | Num
    | Bool
    | Hole
    | Unit
    | List(_) => precedence_const
    | Prod(_, _) => precedence_Prod
    | Sum(_, _) => precedence_Sum
    | Arrow(_, _) => precedence_Arrow
    };

  let rec mk_htyp =
          (~parenthesize=false, ~enforce_inline: bool, ty: HTyp.t): t => {
    let doc =
      switch (ty) {
      | Hole => mk_EmptyHole("?")
      | Unit => mk_Unit()
      | Num => mk_Num()
      | Bool => mk_Bool()
      | List(ty) =>
        Doc.hcats([
          DelimDoc.open_List(),
          mk_htyp_child(
            ~parenthesize=false,
            ~enforce_inline,
            ~child_step=0,
            ty,
          )
          |> pad_open_child,
          DelimDoc.close_List(),
        ])
      | Arrow(ty1, ty2) =>
        let padded_op =
          Doc.(
            hcats([
              choices([Linebreak, space]),
              Text(UnicodeConstants.typeArrowSym ++ " "),
            ])
          );
        let ty1_doc =
          ty1
          |> mk_htyp_child(
               ~parenthesize=precedence_ty(ty1) >= precedence_Arrow,
               ~enforce_inline,
               ~child_step=0,
             )
          |> pad_open_child;
        let ty2_doc =
          ty2
          |> mk_htyp_child(
               ~parenthesize=precedence_ty(ty2) > precedence_Arrow,
               ~enforce_inline,
               ~child_step=1,
             )
          |> pad_open_child;
        Doc.hcats([ty1_doc, padded_op, ty2_doc]);
      | Prod(ty1, ty2) =>
        let padded_op =
          Doc.(hcats([Text(","), choices([Linebreak, space])]));
        let ty1_doc =
          ty1
          |> mk_htyp_child(
               ~parenthesize=precedence_ty(ty1) >= precedence_Prod,
               ~enforce_inline,
               ~child_step=0,
             )
          |> pad_open_child;
        let ty2_doc =
          ty2
          |> mk_htyp_child(
               ~parenthesize=precedence_ty(ty2) > precedence_Prod,
               ~enforce_inline,
               ~child_step=1,
             )
          |> pad_open_child;
        Doc.hcats([ty1_doc, padded_op, ty2_doc]);
      | Sum(ty1, ty2) =>
        let padded_op =
          Doc.(hcats([choices([Linebreak, space]), Text("| ")]));
        let ty1_doc =
          ty1
          |> mk_htyp_child(
               ~parenthesize=precedence_ty(ty1) >= precedence_Sum,
               ~enforce_inline,
               ~child_step=0,
             )
          |> pad_open_child;
        let ty2_doc =
          ty2
          |> mk_htyp_child(
               ~parenthesize=precedence_ty(ty2) > precedence_Sum,
               ~enforce_inline,
               ~child_step=1,
             )
          |> pad_open_child;
        Doc.hcats([ty1_doc, padded_op, ty2_doc]);
      };
    // TODO No concept of steps for display parentheses.
    // Currently doesn't matter, but will need to address
    // if steps ever matter here.
    parenthesize
      ? Doc.hcats([
          DelimDoc.open_Parenthesized(),
          doc,
          DelimDoc.close_Parenthesized(),
        ])
      : doc;
  }
  and mk_htyp_child =
      (~parenthesize, ~enforce_inline, ~child_step, ty): formatted_child => {
    let formattable = (~enforce_inline: bool) =>
      mk_htyp(~parenthesize, ~enforce_inline, ty) |> annot_Step(child_step);
    enforce_inline
      ? EnforcedInline(formattable(~enforce_inline=true))
      : Unformatted(formattable);
  };

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
    | Num => mk_Num()
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
      | UHPat.Comma => (empty, space)
      | Space
      | Cons => (empty, empty)
    );

  let mk_EmptyHole = mk_EmptyHole(~family=Pat);
  let mk_NumLit = mk_NumLit(~family=Pat);
  let mk_BoolLit = mk_BoolLit(~family=Pat);
  let mk_ListNil = mk_ListNil(~family=Pat);
  let mk_Var = mk_Var(~family=Pat);
  let mk_Parenthesized = mk_Parenthesized(~family=Pat);
  let mk_Inj = mk_Inj(~family=Pat);
  let mk_NTuple =
    mk_NTuple(
      ~family=Pat,
      ~get_tuple_elements=UHPat.get_tuple_elements,
      ~inline_padding_of_operator,
    );

  let rec mk = (~enforce_inline: bool, p: UHPat.t): t =>
    mk_opseq(~enforce_inline, p)
  and mk_opseq = (~enforce_inline: bool, opseq: UHPat.opseq): t =>
    mk_NTuple(~mk_operand, ~mk_operator, ~enforce_inline, opseq)
  and mk_operator = (op: UHPat.operator): t =>
    op |> UHPat.is_Space ? space_op : mk_op(UHPat.string_of_operator(op))
  and mk_operand = (~enforce_inline: bool, operand: UHPat.operand): t =>
    switch (operand) {
    | EmptyHole(u) => mk_EmptyHole(hole_lbl(u))
    | Wild(err) => mk_Wild(~err)
    | Var(err, verr, x) => mk_Var(~err, ~verr, x)
    | NumLit(err, n) => mk_NumLit(~err, n)
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

  let mk_EmptyHole = mk_EmptyHole(~family=Exp);
  let mk_NumLit = mk_NumLit(~family=Exp);
  let mk_BoolLit = mk_BoolLit(~family=Exp);
  let mk_ListNil = mk_ListNil(~family=Exp);
  let mk_Var = mk_Var(~family=Exp);
  let mk_Parenthesized = mk_Parenthesized(~family=Exp);
  let mk_Inj = mk_Inj(~family=Exp);
  let mk_NTuple =
    mk_NTuple(
      ~family=Exp,
      ~get_tuple_elements=UHExp.get_tuple_elements,
      ~inline_padding_of_operator,
    );

  let annot_SubBlock = (~hd_index: int) =>
    Doc.annot(
      TermAnnot.mk_Term(
        ~family=Exp,
        ~shape=SubBlock({hd_index: hd_index}),
        (),
      ),
    );

  let rec mk = (~enforce_inline: bool, e: UHExp.t): t =>
    mk_block(~enforce_inline, e)
  and mk_block = (~offset=0, ~enforce_inline: bool, block: UHExp.block): t =>
    if (enforce_inline && UHExp.Block.num_lines(block) > 1) {
      Fail;
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
      // TODO: Once we figure out contenteditable cursors, use `mk_text("")`
      Doc.empty |> Doc.annot(TermAnnot.mk_EmptyLine())
    | ExpLine(opseq) => mk_opseq(~enforce_inline, opseq)
    | LetLine(p, ann, def) =>
      let p = Pat.mk_child(~enforce_inline, ~child_step=0, p);
      let ann =
        ann
        |> OptUtil.map(ann =>
             Typ.mk_child(~enforce_inline, ~child_step=1, ann)
           );
      let def = mk_child(~enforce_inline, ~child_step=2, def);
      mk_LetLine(p, ann, def) |> Doc.annot(TermAnnot.LetLine);
    }
  and mk_opseq = (~enforce_inline: bool, opseq: UHExp.opseq): t =>
    mk_NTuple(~mk_operand, ~mk_operator, ~enforce_inline, opseq)
  and mk_operator = (op: UHExp.operator): t =>
    op |> UHExp.is_Space ? space_op : mk_op(UHExp.string_of_operator(op))
  and mk_operand = (~enforce_inline: bool, operand: UHExp.operand): t =>
    switch (operand) {
    | EmptyHole(u) => mk_EmptyHole(hole_lbl(u))
    | Var(err, verr, x) => mk_Var(~err, ~verr, x)
    | NumLit(err, n) => mk_NumLit(~err, n)
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
        Fail;
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
        EnforcedInline(Fail);
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
