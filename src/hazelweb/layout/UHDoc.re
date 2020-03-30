open Pretty;
open ViewUtil;

module Vdom = Virtual_dom.Vdom;

type t = Doc.t(UHAnnot.t);

module Delim = {
  let mk = (~caret: option(Side.t)=?, ~path: delim_path, delim_text: string) =>
    Doc.text(delim_text) |> Doc.annot(UHAnnot.mk_Delim(~caret?, ~path, ()));

  let empty_hole_doc =
      (~caret: option(Side.t)=?, ~steps: CursorPath.steps, hole_lbl: string) =>
    Doc.text(hole_lbl)
    |> Doc.annot(
         UHAnnot.HoleLabel({len: hole_lbl |> StringUtil.utf8_length}),
       )
    |> Doc.annot(UHAnnot.mk_Delim(~caret?, ~path=(steps, 0), ()));

  let open_List = (~caret=?, steps) => mk(~caret?, ~path=(steps, 0), "[");
  let close_List = (~caret=?, steps) => mk(~caret?, ~path=(steps, 1), "]");

  let open_Parenthesized = (~caret=?, steps) =>
    mk(~caret?, ~path=(steps, 0), "(");
  let close_Parenthesized = (~caret=?, steps) =>
    mk(~caret?, ~path=(steps, 1), ")");

  let open_Inj = (~caret=?, steps, inj_side: InjSide.t) =>
    mk(~caret?, ~path=(steps, 0), InjSide.to_string(inj_side) ++ "(");
  let close_Inj = (~caret=?, steps) => mk(~caret?, ~path=(steps, 1), ")");

  let sym_Lam = (~caret=?, steps) =>
    mk(~caret?, ~path=(steps, 0), UnicodeConstants.lamSym);
  let colon_Lam = (~caret=?, steps) => mk(~caret?, ~path=(steps, 1), ":");
  let open_Lam = (~caret=?, steps) => mk(~caret?, ~path=(steps, 2), ".{");
  let close_Lam = (~caret=?, steps) => mk(~caret?, ~path=(steps, 3), "}");

  let open_Case = (~caret=?, steps) =>
    mk(~caret?, ~path=(steps, 0), "case");
  let close_Case = (~caret=?, steps) =>
    mk(~caret?, ~path=(steps, 1), "end");
  let close_Case_ann = (~caret=?, steps) =>
    mk(~caret?, ~path=(steps, 1), "end :");

  let bar_Rule = (~caret=?, steps) => mk(~caret?, ~path=(steps, 0), "|");
  let arrow_Rule = (~caret=?, steps) =>
    mk(~caret?, ~path=(steps, 1), UnicodeConstants.caseArrowSym);

  let let_LetLine = (~caret=?, steps) =>
    mk(~caret?, ~path=(steps, 0), "let");
  let colon_LetLine = (~caret=?, steps) =>
    mk(~caret?, ~path=(steps, 1), ":");
  let eq_LetLine = (~caret=?, steps) => mk(~caret?, ~path=(steps, 2), "=");
  let in_LetLine = (~caret=?, steps) => mk(~caret?, ~path=(steps, 3), "in");
};

let empty_ = Doc.empty();
let space_ = Doc.space();

let indent_and_align_ = doc =>
  Doc.(hcat(annot(UHAnnot.Indent, indent()), align(doc)));

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
    UHAnnot.mk_Term(~sort, ~shape=UHAnnot.mk_Var(~err, ~verr, ()), ()),
  );
let annot_Operand = (~sort: TermSort.t, ~err: ErrStatus.t=NotInHole) =>
  Doc.annot(
    UHAnnot.mk_Term(~sort, ~shape=UHAnnot.mk_Operand(~err, ()), ()),
  );
let annot_Case = (~err: ErrStatus.t) =>
  Doc.annot(UHAnnot.mk_Term(~sort=Exp, ~shape=Case({err: err}), ()));

let annot_FreeLivelit =
  Doc.annot(UHAnnot.mk_Term(~sort=Exp, ~shape=UHAnnot.FreeLivelit, ()));
let annot_ApLivelit =
    (
      lln: LivelitName.t,
      llview: Livelits.LivelitView.t,
      splice_docs: NatMap.t(t),
      steps: CursorPath.steps,
    ) =>
  Doc.annot(
    UHAnnot.mk_Term(
      ~sort=Exp,
      ~shape=UHAnnot.mk_ApLivelit(~lln, ~llview, ~splice_docs, ~steps),
      (),
    ),
  );

let mk_text = (~steps: CursorPath.steps, text: string): t =>
  Doc.text(text)
  |> Doc.annot(
       UHAnnot.mk_Text(~steps, ~length=StringUtil.utf8_length(text), ()),
     );

let pad_operator =
    (~inline_padding as (left, right): (t, t), operator: t): t => {
  Doc.(
    choices([
      hcats([left |> annot_Padding, operator, right |> annot_Padding]),
      hcats([linebreak(), operator, right |> annot_Padding]),
    ])
  );
};

let mk_op = (~steps: CursorPath.steps, op_text: string, ()): t =>
  Doc.text(op_text) |> Doc.annot(UHAnnot.mk_Op(~steps, ()));

let mk_space_op = space_ |> Doc.annot(UHAnnot.SpaceOp);

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
    hcats([left |> annot_Padding, child_doc, right |> annot_Padding])
    |> annot_child(~is_inline=true);
  };
  let para_choice = child_doc =>
    child_doc |> indent_and_align_ |> annot_child(~is_inline=false);
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
  let inline_choice = child_doc =>
    hcats([inline_padding |> annot_Padding, child_doc])
    |> annot_child(~is_inline=true);
  let para_choice = child_doc =>
    child_doc |> indent_and_align_ |> annot_child(~is_inline=false);
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

let mk_Unit = (~steps: CursorPath.steps, ()): t =>
  Delim.mk(~path=(steps, 0), "()") |> annot_Operand(~sort=Typ);

let mk_Num = (~steps: CursorPath.steps, ()): t =>
  Delim.mk(~path=(steps, 0), "Num") |> annot_Operand(~sort=Typ);

let mk_Bool = (~steps: CursorPath.steps, ()): t =>
  Delim.mk(~path=(steps, 0), "Bool") |> annot_Operand(~sort=Typ);

let mk_EmptyHole =
    (~sort: TermSort.t, ~steps: CursorPath.steps, hole_lbl: string): t =>
  Delim.empty_hole_doc(~steps, hole_lbl) |> annot_Operand(~sort);

let mk_Wild = (~err: ErrStatus.t, ~steps: CursorPath.steps): t =>
  Delim.mk(~path=(steps, 0), "_") |> annot_Operand(~sort=Pat, ~err);

let mk_Var =
    (
      ~sort: TermSort.t,
      ~err: ErrStatus.t,
      ~verr: VarErrStatus.t,
      ~steps: CursorPath.steps,
      x: Var.t,
    )
    : t =>
  mk_text(~steps, x) |> annot_Var(~sort, ~err, ~verr);

let mk_NumLit =
    (~sort: TermSort.t, ~err: ErrStatus.t, ~steps: CursorPath.steps, n: int)
    : t =>
  mk_text(~steps, string_of_int(n)) |> annot_Operand(~sort, ~err);

let mk_BoolLit =
    (~sort: TermSort.t, ~err: ErrStatus.t, ~steps: CursorPath.steps, b: bool)
    : t =>
  mk_text(~steps, string_of_bool(b)) |> annot_Operand(~sort, ~err);

let mk_ListNil =
    (~sort: TermSort.t, ~err: ErrStatus.t, ~steps: CursorPath.steps, ()): t =>
  Delim.mk(~path=(steps, 0), "[]") |> annot_Operand(~sort, ~err);

let mk_Parenthesized =
    (~sort: TermSort.t, ~steps: CursorPath.steps, body: formatted_child): t => {
  let open_group = Delim.open_Parenthesized(steps) |> annot_DelimGroup;
  let close_group = Delim.close_Parenthesized(steps) |> annot_DelimGroup;
  Doc.hcats([open_group, body |> pad_open_child, close_group])
  |> annot_Operand(~sort);
};

let mk_List = (~steps: CursorPath.steps, body: formatted_child): t => {
  let open_group = Delim.open_List(steps) |> annot_DelimGroup;
  let close_group = Delim.close_List(steps) |> annot_DelimGroup;
  Doc.hcats([open_group, body |> pad_open_child, close_group])
  |> annot_Operand(~sort=Typ);
};

let mk_Inj =
    (
      ~sort: TermSort.t,
      ~steps: CursorPath.steps,
      ~err: ErrStatus.t,
      ~inj_side: InjSide.t,
      body: formatted_child,
    )
    : t => {
  let open_group = Delim.open_Inj(steps, inj_side) |> annot_DelimGroup;
  let close_group = Delim.close_Inj(steps) |> annot_DelimGroup;
  Doc.hcats([open_group, body |> pad_open_child, close_group])
  |> annot_Operand(~sort, ~err);
};

let mk_Lam =
    (
      ~steps: CursorPath.steps,
      ~err: ErrStatus.t,
      p: formatted_child,
      ann: option(formatted_child),
      body: formatted_child,
    )
    : t => {
  let open_group = {
    let lam_delim = Delim.sym_Lam(steps);
    let open_delim = Delim.open_Lam(steps);
    let doc =
      switch (ann) {
      | None => Doc.hcats([lam_delim, p |> pad_closed_child, open_delim])
      | Some(ann) =>
        let colon_delim = Delim.colon_Lam(steps);
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
  let close_group = Delim.close_Lam(steps) |> annot_DelimGroup;
  Doc.hcats([open_group, body |> pad_open_child, close_group])
  |> annot_Operand(~sort=Exp, ~err);
};

let mk_Case =
    (
      ~steps: CursorPath.steps,
      ~err: ErrStatus.t,
      scrut: formatted_child,
      rules: list(t),
    )
    : t => {
  let open_group = Delim.open_Case(steps) |> annot_DelimGroup;
  let close_group = Delim.close_Case(steps) |> annot_DelimGroup;
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
      ~steps: CursorPath.steps,
      ~err: ErrStatus.t,
      scrut: formatted_child,
      rules: list(t),
      ann: formatted_child,
    )
    : t => {
  open Doc;
  let open_group = Delim.open_Case(steps) |> annot_DelimGroup;
  let close_group = {
    let end_delim = Delim.close_Case_ann(steps);
    hcats([
      end_delim,
      ann |> pad_left_delimited_child(~is_open=false, ~inline_padding=space_),
    ])
    |> annot_DelimGroup;
  };
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
  |> annot_Case(~err);
};

let mk_Rule =
    (~steps: CursorPath.steps, p: formatted_child, clause: formatted_child): t => {
  let delim_group =
    Doc.hcats([
      Delim.bar_Rule(steps),
      p |> pad_closed_child(~inline_padding=(space_, space_)),
      Delim.arrow_Rule(steps),
    ])
    |> annot_DelimGroup;
  Doc.hcats([
    delim_group,
    clause |> pad_left_delimited_child(~is_open=true, ~inline_padding=space_),
  ])
  |> Doc.annot(UHAnnot.mk_Term(~sort=Exp, ~shape=Rule, ()));
};

let mk_FreeLivelit = (~steps: CursorPath.steps, lln: LivelitName.t): t =>
  annot_FreeLivelit(mk_text(~steps, lln));

let mk_LetLine =
    (
      ~steps: CursorPath.steps,
      p: formatted_child,
      ann: option(formatted_child),
      def: formatted_child,
    )
    : t => {
  let open_group = {
    let let_delim = Delim.let_LetLine(steps);
    let eq_delim = Delim.eq_LetLine(steps);
    let doc =
      switch (ann) {
      | None =>
        Doc.hcats([
          let_delim,
          p |> pad_closed_child(~inline_padding=(space_, space_)),
          eq_delim,
        ])
      | Some(ann) =>
        let colon_delim = Delim.colon_LetLine(steps);
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
  let close_group = Delim.in_LetLine(steps) |> annot_DelimGroup;
  Doc.hcats([
    open_group,
    def |> pad_open_child(~inline_padding=(space_, space_)),
    close_group,
  ]);
};

let rec mk_BinOp =
        (
          ~sort: TermSort.t,
          ~mk_operand:
             (~steps: CursorPath.steps, ~enforce_inline: bool, 'operand) => t,
          ~mk_operator: (~steps: CursorPath.steps, 'operator) => t,
          ~inline_padding_of_operator: 'operator => (t, t),
          ~steps: CursorPath.steps,
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
      ~steps,
      ~enforce_inline,
      ~seq,
    );
  switch (skel) {
  | Placeholder(n) =>
    let operand = seq |> Seq.nth_operand(n);
    mk_operand(~steps=steps @ [n], ~enforce_inline, operand)
    |> annot_Step(n);
  | BinOp(err, op, skel1, skel2) =>
    let op_index = Skel.rightmost_tm_index(skel1) + Seq.length(seq);
    let op_doc =
      mk_operator(~steps=steps @ [op_index], op)
      |> annot_Step(op_index)
      |> annot_DelimGroup;
    let skel1_doc = go(skel1);
    let skel2_doc = go(skel2);
    Doc.hcats([
      skel1_doc |> annot_OpenChild(~is_inline=true),
      op_doc |> pad_operator(~inline_padding=inline_padding_of_operator(op)),
      skel2_doc |> annot_OpenChild(~is_inline=true),
    ])
    |> Doc.annot(UHAnnot.mk_Term(~sort, ~shape=BinOp({err, op_index}), ()));
  };
};

let mk_NTuple =
    (
      ~sort: TermSort.t,
      ~get_tuple_elements: Skel.t('operator) => list(Skel.t('operator)),
      ~mk_operand:
         (~steps: CursorPath.steps, ~enforce_inline: bool, 'operand) => t,
      ~mk_operator: (~steps: CursorPath.steps, 'operator) => t,
      ~inline_padding_of_operator: 'operator => (t, t),
      ~steps: CursorPath.steps,
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
      ~steps,
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
               Doc.text(",")
               |> Doc.annot(
                    UHAnnot.mk_Op(~steps=steps @ [comma_index], ()),
                  )
               |> annot_Step(comma_index)
               |> annot_DelimGroup;
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
    | UHTyp.Prod => (empty_, space_)
    | Arrow
    | Sum => (space_, space_);

  let mk_EmptyHole = mk_EmptyHole(~sort=Typ);
  let mk_Parenthesized = mk_Parenthesized(~sort=Typ);
  let mk_NTuple =
    mk_NTuple(
      ~sort=Typ,
      ~get_tuple_elements=UHTyp.get_prod_elements,
      ~inline_padding_of_operator,
    );

  let rec mk =
          (~steps: CursorPath.steps, ~enforce_inline: bool, uty: UHTyp.t): t =>
    mk_opseq(~steps, ~enforce_inline, uty)
  and mk_opseq =
      (~steps: CursorPath.steps, ~enforce_inline: bool, opseq: UHTyp.opseq): t =>
    mk_NTuple(~mk_operand, ~mk_operator, ~steps, ~enforce_inline, opseq)
  and mk_operator = (~steps: CursorPath.steps, op: UHTyp.operator): t =>
    mk_op(~steps, UHTyp.string_of_operator(op), ())
  and mk_operand =
      (
        ~steps: CursorPath.steps,
        ~enforce_inline: bool,
        operand: UHTyp.operand,
      )
      : t =>
    switch (operand) {
    | Hole => mk_EmptyHole(~steps, "?")
    | Unit => mk_Unit(~steps, ())
    | Num => mk_Num(~steps, ())
    | Bool => mk_Bool(~steps, ())
    | Parenthesized(body) =>
      let body = mk_child(~enforce_inline, ~steps, ~child_step=0, body);
      mk_Parenthesized(~steps, body);
    | List(body) =>
      let body = mk_child(~enforce_inline, ~steps, ~child_step=0, body);
      mk_List(~steps, body);
    }
  and mk_child = (~enforce_inline, ~steps, ~child_step, uty): formatted_child => {
    let formattable = (~enforce_inline: bool) =>
      mk(~steps=steps @ [child_step], ~enforce_inline, uty)
      |> annot_Step(child_step);
    enforce_inline
      ? EnforcedInline(formattable(~enforce_inline=true))
      : Unformatted(formattable);
  };
};

module Pat = {
  let inline_padding_of_operator =
    fun
    | UHPat.Comma => (empty_, space_)
    | Space
    | Cons => (empty_, empty_);

  let mk_EmptyHole = mk_EmptyHole(~sort=Pat);
  let mk_NumLit = mk_NumLit(~sort=Pat);
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

  let rec mk =
          (~steps: CursorPath.steps, ~enforce_inline: bool, p: UHPat.t): t =>
    mk_opseq(~steps, ~enforce_inline, p)
  and mk_opseq =
      (~steps: CursorPath.steps, ~enforce_inline: bool, opseq: UHPat.opseq): t =>
    mk_NTuple(~mk_operand, ~mk_operator, ~steps, ~enforce_inline, opseq)
  and mk_operator = (~steps: CursorPath.steps, op: UHPat.operator): t =>
    op |> UHPat.is_Space
      ? mk_space_op : mk_op(~steps, UHPat.string_of_operator(op), ())
  and mk_operand =
      (
        ~steps: CursorPath.steps,
        ~enforce_inline: bool,
        operand: UHPat.operand,
      )
      : t =>
    switch (operand) {
    | EmptyHole(u) => mk_EmptyHole(~steps, string_of_int(u + 1))
    | Wild(err) => mk_Wild(~err, ~steps)
    | Var(err, verr, x) => mk_Var(~steps, ~err, ~verr, x)
    | NumLit(err, n) => mk_NumLit(~err, ~steps, n)
    | BoolLit(err, b) => mk_BoolLit(~err, ~steps, b)
    | ListNil(err) => mk_ListNil(~err, ~steps, ())
    | Parenthesized(body) =>
      let body = mk_child(~enforce_inline, ~steps, ~child_step=0, body);
      mk_Parenthesized(~steps, body);
    | Inj(err, inj_side, body) =>
      let body = mk_child(~enforce_inline, ~steps, ~child_step=0, body);
      mk_Inj(~err, ~steps, ~inj_side, body);
    }
  and mk_child = (~enforce_inline, ~steps, ~child_step, p): formatted_child => {
    let formattable = (~enforce_inline: bool) =>
      mk(~steps=steps @ [child_step], ~enforce_inline, p)
      |> annot_Step(child_step);
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
    | Cons => (empty_, empty_)
    | Plus
    | Minus
    | LessThan
    | GreaterThan
    | Equals
    | And
    | Or => (space_, space_)
    | Comma => (empty_, space_);

  let mk_EmptyHole = mk_EmptyHole(~sort=Exp);
  let mk_NumLit = mk_NumLit(~sort=Exp);
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

  let mk =
      (
        ~steps: CursorPath.steps,
        ~enforce_inline: bool,
        ~ctx: Livelits.LivelitViewCtx.t,
        ~llii: NodeInstanceInfo.t,
        e: UHExp.t,
      )
      : t => {
    let rec mk_block =
            (
              ~offset=0,
              ~steps: CursorPath.steps,
              ~enforce_inline: bool,
              block: UHExp.block,
            )
            : t =>
      if (enforce_inline && UHExp.Block.num_lines(block) > 1) {
        Doc.fail();
      } else {
        block
        |> List.mapi((i, line) =>
             mk_line(~enforce_inline, ~steps=steps @ [offset + i], line)
             |> annot_Step(offset + i)
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
    and mk_line =
        (~enforce_inline: bool, ~steps: CursorPath.steps, line: UHExp.line): t =>
      switch (line) {
      | EmptyLine =>
        // TODO: Once we figure out contenteditable cursors, use `mk_text(~steps, "")`
        mk_text(~steps, UnicodeConstants.zwsp)
        |> Doc.annot(UHAnnot.EmptyLine)
      | ExpLine(opseq) => mk_opseq(~steps, ~enforce_inline, opseq)
      | LetLine(p, ann, def) =>
        let p = Pat.mk_child(~enforce_inline, ~steps, ~child_step=0, p);
        let ann =
          ann
          |> OptUtil.map(ann =>
               Typ.mk_child(~enforce_inline, ~steps, ~child_step=1, ann)
             );
        let def = mk_child(~enforce_inline, ~steps, ~child_step=2, def);
        mk_LetLine(~steps, p, ann, def) |> Doc.annot(UHAnnot.LetLine);
      }
    and mk_opseq =
        (~steps: CursorPath.steps, ~enforce_inline: bool, opseq: UHExp.opseq)
        : t =>
      mk_NTuple(~mk_operand, ~mk_operator, ~steps, ~enforce_inline, opseq)
    and mk_operator = (~steps: CursorPath.steps, op: UHExp.operator): t =>
      op |> UHExp.is_Space
        ? mk_space_op : mk_op(~steps, UHExp.string_of_operator(op), ())
    and mk_operand =
        (
          ~steps: CursorPath.steps,
          ~enforce_inline: bool,
          operand: UHExp.operand,
        )
        : t =>
      switch (operand) {
      | EmptyHole(u) => mk_EmptyHole(~steps, string_of_int(u + 1))
      | Var(err, verr, x) => mk_Var(~err, ~verr, ~steps, x)
      | NumLit(err, n) => mk_NumLit(~err, ~steps, n)
      | BoolLit(err, b) => mk_BoolLit(~err, ~steps, b)
      | ListNil(err) => mk_ListNil(~err, ~steps, ())
      | Lam(err, p, ann, body) =>
        let p = Pat.mk_child(~enforce_inline, ~steps, ~child_step=0, p);
        let ann =
          ann
          |> OptUtil.map(ann =>
               Typ.mk_child(~enforce_inline, ~steps, ~child_step=1, ann)
             );
        let body = mk_child(~enforce_inline, ~steps, ~child_step=2, body);
        mk_Lam(~err, ~steps, p, ann, body);
      | Inj(err, inj_side, body) =>
        let body = mk_child(~enforce_inline, ~steps, ~child_step=0, body);
        mk_Inj(~err, ~steps, ~inj_side, body);
      | Parenthesized(body) =>
        let body = mk_child(~enforce_inline, ~steps, ~child_step=0, body);
        mk_Parenthesized(~steps, body);
      | Case(err, scrut, rules, ann) =>
        if (enforce_inline) {
          Doc.fail();
        } else {
          let scrut =
            mk_child(~enforce_inline=false, ~steps, ~child_step=0, scrut);
          let rules =
            rules
            |> List.mapi((i, rule) =>
                 mk_rule(~steps=steps @ [1 + i], rule) |> annot_Step(1 + i)
               );
          switch (ann) {
          | None => mk_Case(~err, ~steps, scrut, rules)
          | Some(ann) =>
            let ann =
              Typ.mk_child(
                ~enforce_inline=false,
                ~steps,
                ~child_step=1 + List.length(rules),
                ann,
              );
            mk_Case_ann(~err, ~steps, scrut, rules, ann);
          };
        }
      | ApLivelit(llu, _, lln, m, splice_info) =>
        switch (VarMap.lookup(ctx, lln)) {
        | None => assert(false)
        | Some(svf) =>
          let inst_opt = NodeInstanceInfo.default_instance(llii, llu);
          let env_opt =
            inst_opt
            |> OptUtil.and_then(NodeInstanceInfo.lookup(llii))
            |> OptUtil.map(fst);
          let llview = svf(m, env_opt, _ => Vdom.Event.Ignore);
          mk_ApLivelit(~steps, lln, llview, splice_info);
        }
      | FreeLivelit(_, lln) => mk_FreeLivelit(~steps, lln)
      }
    and mk_ApLivelit =
        (
          ~steps: CursorPath.steps,
          lln: LivelitName.t,
          llview: Livelits.LivelitView.t,
          splice_info: UHExp.splice_info,
        )
        : t => {
      let spaceholder =
        switch (llview) {
        | Inline(_, width) => String.make(width, ' ')
        | MultiLine(_) => "" // TODO multiline spaceholders
        };
      let splice_map = splice_info.splice_map;
      let splice_docs =
        NatMap.map(
          ((_, se)) => mk_block(~steps, ~enforce_inline, se),
          splice_map,
        );
      annot_ApLivelit(
        lln,
        llview,
        splice_docs,
        steps,
        Doc.hcats([mk_text(~steps, lln), mk_text(~steps, spaceholder)]),
      );
    }
    and mk_rule = (~steps: CursorPath.steps, Rule(p, clause): UHExp.rule): t => {
      let p = Pat.mk_child(~enforce_inline=false, ~steps, ~child_step=0, p);
      let clause =
        mk_child(~enforce_inline=false, ~steps, ~child_step=1, clause);
      mk_Rule(~steps, p, clause);
    }
    and mk_child = (~enforce_inline, ~steps, ~child_step, e): formatted_child => {
      switch (e) {
      | [EmptyLine, ...subblock] =>
        if (enforce_inline) {
          EnforcedInline(Doc.fail());
        } else {
          let formatted =
            mk_block(
              ~offset=1,
              ~steps=steps @ [child_step],
              ~enforce_inline=false,
              subblock,
            )
            |> annot_Step(child_step);
          UserNewline(formatted);
        }
      | _ =>
        let formattable = (~enforce_inline) =>
          mk_block(~steps=steps @ [child_step], ~enforce_inline, e)
          |> annot_Step(child_step);
        enforce_inline
          ? EnforcedInline(formattable(~enforce_inline=true))
          : Unformatted(formattable);
      };
    };
    mk_block(~steps, ~enforce_inline, e);
  };
};
