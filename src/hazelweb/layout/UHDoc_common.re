open Pretty;

type t = Doc.t(UHAnnot.t);

type memoization_value('a) = {
  mutable inline_true: option('a),
  mutable inline_false: option('a),
};

let memoize =
    (f: (~memoize: bool, ~enforce_inline: bool, 'k) => 'v)
    : ((~memoize: bool, ~enforce_inline: bool, 'k) => 'v) => {
  let table: WeakMap.t('k, memoization_value('v)) = WeakMap.mk();
  (~memoize: bool, ~enforce_inline: bool, k: 'k) => (
    if (!memoize) {
      f(~memoize, ~enforce_inline, k);
    } else {
      switch (WeakMap.get(table, k)) {
      | None =>
        let v = f(~memoize, ~enforce_inline, k);
        let m =
          if (enforce_inline) {
            {inline_true: Some(v), inline_false: None};
          } else {
            {inline_false: Some(v), inline_true: None};
          };
        let _ = WeakMap.set(table, k, m);
        v;
      | Some((m: memoization_value('v))) =>
        if (enforce_inline) {
          switch (m.inline_true) {
          | Some(v) => v
          | None =>
            let v = f(~memoize, ~enforce_inline, k);
            m.inline_true = Some(v);
            v;
          };
        } else {
          switch (m.inline_false) {
          | Some(v) => v
          | None =>
            let v = f(~memoize, ~enforce_inline, k);
            m.inline_false = Some(v);
            v;
          };
        }
      };
    }: 'v
  );
};

let empty_: t = Doc.empty();
let space_: t = Doc.space();

let indent_and_align_ = (doc: t): t =>
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

  let open_List = (): t => mk(~index=0, "[");
  let close_List = (): t => mk(~index=1, "]");

  let open_Parenthesized = (): t => mk(~index=0, "(");
  let close_Parenthesized = (): t => mk(~index=1, ")");

  let open_Inj = (inj_side: InjSide.t): t =>
    mk(~index=0, "inj[" ++ InjSide.to_string(inj_side) ++ "](");
  let close_Inj = (): t => mk(~index=1, ")");

  let sym_Lam = (): t => mk(~index=0, UnicodeConstants.lamSym);
  let colon_Lam = (): t => mk(~index=1, ":");
  let open_Lam = (): t => mk(~index=2, ".{");
  let close_Lam = (): t => mk(~index=3, "}");

  let open_Case = (): t => mk(~index=0, "case");
  let close_Case = (): t => mk(~index=1, "end");
  let close_Case_ann = (): t => mk(~index=1, "end :");

  let bar_Rule = (): t => mk(~index=0, "|");
  let arrow_Rule = (): t => mk(~index=1, "=>");

  let let_LetLine = (): t => mk(~index=0, "let");
  let colon_LetLine = (): t => mk(~index=1, ":");
  let eq_LetLine = (): t => mk(~index=2, "=");
  let in_LetLine = (): t => mk(~index=3, "in");

  let open_CommentLine = (): t => mk(~index=0, "#");
};

let annot_Indent: t => t = Doc.annot(UHAnnot.Indent);
let annot_Padding = (d: t): t =>
  switch (d.doc) {
  | Text("") => d
  | _ => Doc.annot(UHAnnot.Padding, d)
  };
let annot_DelimGroup: t => t = Doc.annot(UHAnnot.DelimGroup);
let annot_OpenChild = (~is_inline: bool): (t => t) =>
  Doc.annot(UHAnnot.mk_OpenChild(~is_inline, ()));
let annot_ClosedChild = (~is_inline: bool): (t => t) =>
  Doc.annot(UHAnnot.mk_ClosedChild(~is_inline, ()));
let annot_Step = (step: int): (t => t) => Doc.annot(UHAnnot.Step(step));
let annot_Var =
    (~sort: TermSort.t, ~err: ErrStatus.t=NotInHole, ~verr: VarErrStatus.t)
    : (t => t) =>
  Doc.annot(
    UHAnnot.mk_Term(~sort, ~shape=TermShape.mk_Var(~err, ~verr, ()), ()),
  );
let annot_Operand = (~sort: TermSort.t, ~err: ErrStatus.t=NotInHole): (t => t) =>
  Doc.annot(
    UHAnnot.mk_Term(~sort, ~shape=TermShape.mk_Operand(~err, ()), ()),
  );
let annot_Case = (~err: CaseErrStatus.t): (t => t) =>
  Doc.annot(UHAnnot.mk_Term(~sort=Exp, ~shape=Case({err: err}), ()));
let annot_List = (~sort: TermSort.t, ~err: ListErrStatus.t): (t => t) =>
  Doc.annot(UHAnnot.mk_Term(~sort, ~shape=List({err: err}), ()));
let annot_Invalid = (~sort: TermSort.t): (t => t) =>
  Doc.annot(UHAnnot.mk_Term(~sort, ~shape=TermShape.Invalid, ()));

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

let mk_space_op: t = Doc.annot(UHAnnot.SpaceOp, space_);

let user_newline: t =
  Doc.(
    hcats([
      space_ |> annot_Padding,
      text(UnicodeConstants.user_newline) |> annot(UHAnnot.UserNewline),
    ])
  );

type formatted_child =
  | UserNewline(t)
  | EnforcedInline(t)
  | Unformatted((~enforce_inline: bool) => t);

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

let pad_open_child: (~inline_padding: (t, t)=?, formatted_child) => t =
  pad_child(~is_open=true);
let pad_closed_child: (~inline_padding: (t, t)=?, formatted_child) => t =
  pad_child(~is_open=false);

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

let mk_InvalidText = (~sort: TermSort.t, t: string): t =>
  mk_text(t) |> annot_Invalid(~sort);

let mk_Var =
    (~sort: TermSort.t, ~err: ErrStatus.t, ~verr: VarErrStatus.t, x: Var.t): t =>
  mk_text(x) |> annot_Var(~sort, ~err, ~verr);

let mk_IntLit = (~sort: TermSort.t, ~err: ErrStatus.t, n: string): t =>
  mk_text(n) |> annot_Operand(~sort, ~err);

let mk_FloatLit = (~sort: TermSort.t, ~err: ErrStatus.t, f: string): t =>
  mk_text(f) |> annot_Operand(~sort, ~err);

let mk_BoolLit = (~sort: TermSort.t, ~err: ErrStatus.t, b: bool): t =>
  mk_text(string_of_bool(b)) |> annot_Operand(~sort, ~err);

let mk_Parenthesized = (~sort: TermSort.t, body: formatted_child): t => {
  let open_group = Delim.open_Parenthesized() |> annot_DelimGroup;
  let close_group = Delim.close_Parenthesized() |> annot_DelimGroup;
  Doc.hcats([open_group, body |> pad_open_child, close_group])
  |> annot_Operand(~sort);
};

let mk_ListLit =
    (~sort: TermSort.t, ~err: ListErrStatus.t, body: option(formatted_child))
    : t => {
  switch (body) {
  | None => mk_text("[]") |> annot_List(~sort, ~err)
  | Some(body) =>
    let open_group = Delim.open_List() |> annot_DelimGroup;
    let close_group = Delim.close_List() |> annot_DelimGroup;
    Doc.hcats([open_group, body |> pad_open_child, close_group])
    |> annot_List(~sort, ~err);
  };
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

let mk_Case =
    (~err: CaseErrStatus.t, scrut: formatted_child, rules: list(t)): t => {
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
