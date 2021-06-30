module Doc = Pretty.Doc;
open UHDoc;

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
      | Some(m: memoization_value('v)) =>
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
let indent_: t = Doc.indent();
let linebreak_: t = Doc.linebreak();

let indent_and_align_ = (doc: t): t => Doc.(hcat(indent_, align(doc)));

module Delim = {
  let mk =
      (
        ~sort: TermSort.t,
        ~index: int,
        ~delim_class: UHAnnot.delimiter,
        delim_text: string,
      )
      : t =>
    Doc.annot(
      UHAnnot.mk_Token(
        ~ann_class=Delimiter(delim_class),
        ~sort,
        ~len=StringUtil.utf8_length(delim_text),
        ~shape=Delim(index),
        (),
      ),
      Doc.text(delim_text),
    );

  let mk_structural = mk(~delim_class=Structural);
  let mk_exp_structural = mk_structural(~sort=Exp);
  let mk_exp_keyword = mk(~sort=Exp, ~delim_class=Keyword);

  let open_List = (~sort): t => mk_structural(~sort, ~index=0, "[");
  let close_List = (~sort): t => mk_structural(~sort, ~index=1, "]");

  let open_Parenthesized = (~sort: TermSort.t): t =>
    mk_structural(~sort, ~index=0, "(");
  let close_Parenthesized = (~sort: TermSort.t): t =>
    mk_structural(~sort, ~index=1, ")");

  let open_Inj = (inj_side: InjSide.t): t =>
    // TODO(andrew): i can fix him (separate into kw and structural?)
    mk_exp_keyword(~index=0, "inj[" ++ InjSide.to_string(inj_side) ++ "](");
  let close_Inj = (): t => mk_exp_structural(~index=1, ")");

  let sym_Lam = (): t => mk_exp_keyword(~index=0, Unicode.lamSym);
  let open_Lam = (): t => mk_exp_structural(~index=1, ".{");
  let close_Lam = (): t => mk_exp_structural(~index=2, "}");

  let open_Case = (): t => mk_exp_keyword(~index=0, "case");
  let close_Case = (): t => mk_exp_keyword(~index=1, "end");

  let bar_Rule = (): t => mk_exp_structural(~index=0, "|");
  let arrow_Rule = (): t => mk_exp_structural(~index=1, "=>");

  let let_LetLine = (): t => mk_exp_keyword(~index=0, "let");
  let eq_LetLine = (): t => mk_exp_structural(~index=1, "=");
  let in_LetLine = (): t => mk_exp_keyword(~index=2, "in");

  let open_CommentLine = (): t =>
    mk(~sort=Meta, ~delim_class=Structural, ~index=0, "#");

  let colon_Ann = (): t => mk_exp_structural(~index=0, ":");
};

let annot_Tessera: t => t = Doc.annot(UHAnnot.Tessera);
let annot_ClosedChild = (~is_inline: bool, ~sort: TermSort.t): (t => t) =>
  Doc.annot(UHAnnot.ClosedChild({is_inline, sort}));
let annot_Step = (step: int): (t => t) => Doc.annot(UHAnnot.Step(step));
let annot_Operand = (~sort: TermSort.t): (t => t) =>
  Doc.annot(UHAnnot.mk_Term(~sort, ~shape=Operand, ()));
let annot_Case: t => t =
  Doc.annot(UHAnnot.mk_Term(~sort=Exp, ~shape=Case, ()));

let indent_and_align = (d: t): t => Doc.(hcats([indent_, align(d)]));

let mk_Signifier =
    (~sort: TermSort.t, ~sig_class: UHAnnot.signifier, s: string): t =>
  Doc.annot(
    UHAnnot.mk_Token(
      ~sort,
      ~ann_class=Signifier(sig_class),
      ~shape=Text,
      ~len=StringUtil.utf8_length(s),
      (),
    ),
    Doc.text(s),
  );

let mk_comment = comment =>
  comment
  |> mk_Signifier(~sort=Meta, ~sig_class=Comment)
  |> Doc.annot(
       UHAnnot.mk_Token(
         ~shape=Text,
         ~sort=Meta,
         ~ann_class=Signifier(Comment),
         ~len=StringUtil.utf8_length(comment),
         (),
       ),
     );

let emptyline =
  empty_
  |> Doc.annot(
       UHAnnot.mk_Token(
         ~sort=Meta,
         ~shape=Text,
         ~ann_class=Signifier(Comment),
         ~len=0,
         (),
       ),
     );

let mk_op = (~sort, op_text: string): t =>
  Doc.annot(
    UHAnnot.mk_Token(
      ~sort,
      ~ann_class=Signifier(Operator),
      ~len=StringUtil.utf8_length(op_text),
      ~shape=Op,
      (),
    ),
    Doc.text(op_text),
  );

let mk_Lit = (~sort: TermSort.t, text: string): t =>
  Doc.annot(
    UHAnnot.mk_Token(
      ~ann_class=Signifier(Literal),
      ~sort,
      ~len=StringUtil.utf8_length(text),
      ~shape=Delim(0), // TODO(andrew): are these delims for a reason?
      (),
    ),
    Doc.text(text),
  );

let mk_Wild = (): t =>
  mk_Lit(~sort=Pat, "_") |> annot_Tessera |> annot_Operand(~sort=Pat);

let mk_TypeLit = (s: string, ()) =>
  s |> mk_Lit(~sort=Typ) |> annot_Tessera |> annot_Operand(~sort=Typ);

let mk_EmptyHole = (~sort: TermSort.t, hole_lbl: string): t => {
  let len = hole_lbl |> StringUtil.utf8_length;
  Doc.text(hole_lbl)
  |> Doc.annot(UHAnnot.HoleLabel({len: len}))
  |> Doc.annot(
       UHAnnot.mk_Token(
         ~sort,
         ~ann_class=Signifier(Hole),
         ~shape=Delim(0),
         ~len,
         (),
       ),
     )
  |> annot_Tessera
  |> annot_Operand(~sort);
};

let mk_space_op: t = space_;

let user_newline: t =
  Doc.(
    hcats([
      space_,
      text(Unicode.user_newline) |> annot(UHAnnot.UserNewline),
    ])
  );

type formatted_child =
  | UserNewline(t)
  | EnforcedInline(t)
  | Unformatted((~enforce_inline: bool) => t);

let pad = ((left, right), ~newline=[], child_doc) => {
  let lpadding = left == empty_ ? [] : [left];
  let rpadding = right == empty_ ? [] : [right];
  Doc.hcats(newline @ lpadding @ [child_doc] @ rpadding);
};

let pad_open_inline_child = (inline_padding, with_border, child_doc) => {
  child_doc
  |> pad(inline_padding)
  |> Doc.annot(
       UHAnnot.OpenChild(
         with_border ? InlineWithBorder : InlineWithoutBorder,
       ),
     );
};

let pad_closed_inline_child = (inline_padding, sort, child_doc) => {
  child_doc
  |> annot_ClosedChild(~is_inline=true, ~sort)
  |> pad(inline_padding);
};

let pad_open_unformatted_child =
    (multiline_padding, inline_padding, with_border, formattable_child) => {
  let inline =
    pad_open_inline_child(
      inline_padding,
      with_border,
      formattable_child(~enforce_inline=true),
    );
  let multiline =
    pad(
      multiline_padding,
      formattable_child(~enforce_inline=false)
      |> indent_and_align
      |> Doc.annot(UHAnnot.OpenChild(Multiline)),
    );
  Doc.choices([inline, multiline]);
};

let pad_closed_unformatted_child =
    (multiline_padding, inline_padding, sort, formattable_child) => {
  let inline =
    pad_closed_inline_child(
      inline_padding,
      sort,
      formattable_child(~enforce_inline=true),
    );
  let multiline =
    pad(
      multiline_padding,
      formattable_child(~enforce_inline=false)
      |> annot_ClosedChild(~is_inline=false, ~sort)
      |> indent_and_align,
    );
  Doc.choices([inline, multiline]);
};

let pad_delimited_open_child =
    (
      ~inline_padding: (t, t)=(empty_, empty_),
      ~multiline_padding: (t, t)=(empty_, empty_),
      ~with_border: bool=true,
      child: formatted_child,
    )
    : t => {
  switch (child) {
  | EnforcedInline(child_doc) =>
    pad_open_inline_child(inline_padding, with_border, child_doc)
  | UserNewline(child_doc) =>
    pad(~newline=[user_newline], multiline_padding, child_doc)
  | Unformatted(formattable_child) =>
    pad_open_unformatted_child(
      multiline_padding,
      inline_padding,
      with_border,
      formattable_child,
    )
  };
};

let pad_delimited_closed_child =
    (
      ~inline_padding: (t, t)=(empty_, empty_),
      ~multiline_padding: (t, t)=(empty_, empty_),
      ~sort: TermSort.t,
      child: formatted_child,
    ) => {
  switch (child) {
  | EnforcedInline(child_doc) =>
    pad_closed_inline_child(inline_padding, sort, child_doc)
  | UserNewline(child_doc) =>
    pad(
      ~newline=[user_newline],
      multiline_padding,
      child_doc
      |> annot_ClosedChild(~is_inline=false, ~sort)
      |> Doc.indent_and_align,
    )
  | Unformatted(formattable_child) =>
    pad_closed_unformatted_child(
      multiline_padding,
      inline_padding,
      sort,
      formattable_child,
    )
  };
};

let pad_bidelimited_open_child =
  pad_delimited_open_child(~multiline_padding=(linebreak_, linebreak_));

let pad_left_delimited_open_child =
  pad_delimited_open_child(
    ~multiline_padding=(linebreak_, empty_),
    ~inline_padding=(space_, empty_),
  );

let pad_right_delimited_open_child =
  pad_delimited_open_child(
    ~multiline_padding=(empty_, linebreak_),
    ~inline_padding=(empty_, space_),
  );

let pad_closed_child =
  pad_delimited_closed_child(~multiline_padding=(linebreak_, linebreak_));

let pad_left_delimited_closed_child =
  pad_delimited_closed_child(
    ~multiline_padding=(linebreak_, empty_),
    ~inline_padding=(space_, empty_),
  );

let mk_Unit = mk_TypeLit("()");
let mk_Bool = mk_TypeLit("Bool");
let mk_Int = mk_TypeLit("Int");
let mk_Float = mk_TypeLit("Float");

let hole_lbl = (u: MetaVar.t): string => string_of_int(u);
let hole_inst_lbl = (u: MetaVar.t, i: MetaVarInst.t): string =>
  StringUtil.cat([string_of_int(u), ":", string_of_int(i)]);

let mk_InvalidText = (~sort: TermSort.t, t: string): t =>
  mk_Signifier(~sort, ~sig_class=Invalid, t)
  |> annot_Tessera
  |> annot_Operand(~sort);

let mk_Var = (~sort: TermSort.t, x: Var.t): t =>
  mk_Signifier(~sort, ~sig_class=Variable, x)
  |> annot_Tessera
  |> annot_Operand(~sort);

let mk_IntLit = (~sort: TermSort.t, n: string): t =>
  mk_Signifier(~sort, ~sig_class=Literal, n)
  |> annot_Tessera
  |> annot_Operand(~sort);

let mk_FloatLit = (~sort: TermSort.t, f: string): t =>
  mk_Signifier(~sort, ~sig_class=Literal, f)
  |> annot_Tessera
  |> annot_Operand(~sort);

let mk_BoolLit = (~sort: TermSort.t, b: bool): t =>
  mk_Signifier(~sort, ~sig_class=Literal, string_of_bool(b))
  |> annot_Tessera
  |> annot_Operand(~sort);

let mk_ListNil = (~sort: TermSort.t, ()): t =>
  mk_Signifier(~sort, ~sig_class=Literal, "[]")
  |> annot_Tessera
  |> annot_Operand(~sort);

let mk_Parenthesized = (~sort: TermSort.t, body: formatted_child): t => {
  let open_group = Delim.open_Parenthesized(~sort) |> annot_Tessera;
  let close_group = Delim.close_Parenthesized(~sort) |> annot_Tessera;
  Doc.hcats([open_group, body |> pad_bidelimited_open_child, close_group])
  |> annot_Operand(~sort);
};

let mk_List = (body: formatted_child): t => {
  let open_group = Delim.open_List(~sort=Typ) |> annot_Tessera;
  let close_group = Delim.close_List(~sort=Typ) |> annot_Tessera;
  Doc.hcats([open_group, body |> pad_bidelimited_open_child, close_group])
  |> annot_Operand(~sort=Typ);
};

let mk_Inj =
    (~sort: TermSort.t, ~inj_side: InjSide.t, body: formatted_child): t => {
  let open_group = Delim.open_Inj(inj_side) |> annot_Tessera;
  let close_group = Delim.close_Inj() |> annot_Tessera;
  Doc.hcats([open_group, body |> pad_bidelimited_open_child, close_group])
  |> annot_Operand(~sort);
};

let mk_Lam = (p: formatted_child, body: formatted_child): t => {
  let open_group = {
    let lam_delim = Delim.sym_Lam();
    let open_delim = Delim.open_Lam();
    Doc.hcats([lam_delim, p |> pad_closed_child(~sort=Pat), open_delim])
    |> annot_Tessera;
  };
  let close_group = Delim.close_Lam() |> annot_Tessera;
  Doc.hcats([open_group, body |> pad_bidelimited_open_child, close_group])
  |> annot_Operand(~sort=Exp);
};

let mk_Case = (scrut: formatted_child, rules: list(t)): t => {
  let open_group = Delim.open_Case() |> annot_Tessera;
  let close_group = Delim.close_Case() |> annot_Tessera;
  Doc.(
    vseps(
      [
        hcats([
          open_group,
          scrut |> pad_left_delimited_open_child(~with_border=false),
        ]),
        ...rules,
      ]
      @ [close_group],
    )
  )
  |> annot_Case;
};

let mk_Rule = (p: formatted_child, clause: formatted_child): t => {
  let delim_group =
    Doc.hcats([
      Delim.bar_Rule(),
      p |> pad_closed_child(~inline_padding=(space_, space_), ~sort=Pat),
      Delim.arrow_Rule(),
    ])
    |> annot_Tessera;
  Doc.hcats([
    delim_group,
    clause |> pad_left_delimited_open_child(~with_border=false),
  ])
  |> Doc.annot(UHAnnot.mk_Term(~sort=Exp, ~shape=Rule, ()));
};

let mk_LetLine = (p: formatted_child, def: formatted_child): t => {
  let open_group = {
    let let_delim = Delim.let_LetLine();
    let eq_delim = Delim.eq_LetLine();
    Doc.hcats([
      let_delim,
      p |> pad_closed_child(~inline_padding=(space_, space_), ~sort=Pat),
      eq_delim,
    ])
    |> annot_Tessera;
  };
  let close_group = Delim.in_LetLine() |> annot_Tessera;
  Doc.hcats([
    open_group,
    def |> pad_bidelimited_open_child(~inline_padding=(space_, space_)),
    close_group,
  ]);
};

let mk_TypeAnn =
    (~sort: TermSort.t, op: formatted_child, ann: formatted_child): t => {
  Doc.hcats([
    op |> pad_right_delimited_open_child,
    Doc.hcats([
      Delim.colon_Ann(),
      ann |> pad_left_delimited_closed_child(~sort=Typ),
    ])
    |> annot_Tessera,
  ])
  |> annot_Operand(~sort);
};

let pad_operator =
    (~inline_padding as (left, right): (t, t), operator: t): t => {
  open Doc;
  let ldoc = left == empty_ ? empty_ : left;
  let rdoc = right == empty_ ? empty_ : right;
  choices([
    hcats([ldoc, operator, rdoc]),
    hcats([linebreak(), operator, rdoc]),
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
      ~seq,
    );
  switch (skel) {
  | Placeholder(n) =>
    let operand = Seq.nth_operand(n, seq);
    annot_Step(n, mk_operand(~enforce_inline, operand));
  | BinOp(_, op, skel1, skel2) =>
    let op_index = Skel.rightmost_tm_index(skel1) + Seq.length(seq);
    let (lpadding, rpadding) = {
      let (l, r) = inline_padding_of_operator(op);
      (l == empty_ ? [] : [l], r == empty_ ? [] : [r]);
    };
    let op = annot_Tessera(annot_Step(op_index, mk_operator(op)));
    let skel1 = go(skel1);
    let skel2 = go(skel2);
    let inline_choice =
      Doc.(
        hcats([
          annot(
            UHAnnot.OpenChild(InlineWithBorder),
            hcats([skel1(~enforce_inline=true), ...lpadding]),
          ),
          op,
          annot(
            UHAnnot.OpenChild(InlineWithBorder),
            hcats(rpadding @ [skel2(~enforce_inline=true)]),
          ),
        ])
      );
    let multiline_choice =
      Doc.(
        vsep(
          annot(
            UHAnnot.OpenChild(Multiline),
            align(skel1(~enforce_inline=false)),
          ),
          hcat(
            op,
            // TODO need to have a choice here for multiline vs not
            annot(
              UHAnnot.OpenChild(Multiline),
              hcats(rpadding @ [align(skel2(~enforce_inline=false))]),
            ),
          ),
        )
      );
    let choices =
      enforce_inline
        ? inline_choice : Doc.choice(inline_choice, multiline_choice);
    Doc.annot(
      UHAnnot.mk_Term(~sort, ~shape=BinOp({op_index: op_index}), ()),
      choices,
    );
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
      ~seq,
    );

  switch (get_tuple_elements(skel)) {
  | [] => failwith(__LOC__ ++ ": found empty tuple")
  | [singleton] => mk_BinOp(~enforce_inline, singleton)
  | [hd, ...tl] =>
    let hd_doc = (~enforce_inline: bool) =>
      // TODO need to relax is_inline
      Doc.annot(
        UHAnnot.OpenChild(enforce_inline ? InlineWithBorder : Multiline),
        mk_BinOp(~enforce_inline, hd),
      );
    let comma_doc = (step: int) => annot_Step(step, mk_op(~sort, ","));
    let (inline_choice, comma_indices) =
      tl
      |> List.fold_left(
           ((tuple, comma_indices), elem) => {
             let comma_index =
               Skel.leftmost_tm_index(elem) - 1 + Seq.length(seq);
             let elem_doc = mk_BinOp(~enforce_inline=true, elem);
             let doc =
               Doc.hcats([
                 tuple,
                 annot_Tessera(comma_doc(comma_index)),
                 Doc.annot(
                   UHAnnot.OpenChild(InlineWithBorder),
                   Doc.hcat(space_, elem_doc),
                 ),
               ]);
             (doc, [comma_index, ...comma_indices]);
           },
           (hd_doc(~enforce_inline=true), []),
         );
    let multiline_choice =
      tl
      |> List.fold_left(
           (tuple, elem) => {
             let comma_index =
               Skel.leftmost_tm_index(elem) - 1 + Seq.length(seq);
             let elem_doc = mk_BinOp(~enforce_inline=false, elem);
             Doc.(
               vsep(
                 tuple,
                 hcat(
                   annot_Tessera(comma_doc(comma_index)),
                   // TODO need to have a choice here for multiline vs not
                   annot(
                     UHAnnot.OpenChild(Multiline),
                     hcat(space_, align(elem_doc)),
                   ),
                 ),
               )
             );
           },
           hd_doc(~enforce_inline=false),
         );
    let choices =
      enforce_inline
        ? inline_choice : Doc.choice(inline_choice, multiline_choice);
    Doc.annot(
      UHAnnot.mk_Term(
        ~sort,
        ~shape=NTuple({comma_indices: comma_indices}),
        (),
      ),
      choices,
    );
  };
};
