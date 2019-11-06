open Sexplib.Std;
open GeneralUtil;
open SemanticsCommon;

module NodeTag = {
  [@deriving sexp]
  type t = {
    steps: CursorPath.steps,
    shape,
  }
  and shape =
    | Block(UHExp.block)
    | Line(UHExp.line)
    | Exp(UHExp.t)
    | Rule(UHExp.rule)
    | Pat(UHPat.t)
    | Typ(UHTyp.t);

  let mk = (
    ~steps: CursorPath.steps,
    ~shape: shape,
  ): t => { steps, shape };
};

module DelimTag = {
  [@deriving sexp]
  type t = {
    text: string,
    index: delim_index,
    padding_left: string,
    padding_right: string,
  };

  let mk = (
    ~text: string,
    ~index: delim_index,
    ~padding_left: string,
    ~padding_right: string,
  ): t => {
    text,
    index,
    padding_left,
    padding_right,
  }
};

module OpTag = {
  [@deriving sexp]
  type t = {
    text: string,
    index: op_index,
    padding_left: string,
    padding_right: string,
  };

  let mk = (
    ~text: string,
    ~index: op_index,
    ~padding_left: string,
    ~padding_right: string,
  ): t => {
    text,
    index,
    padding_left,
    padding_right,
  }
};

[@deriving sexp]
type tag =
  | Node(NodeTag.t)
  | Delim(DelimTag.t)
  | Op(OpTag.t);

[@deriving sexp]
type doc = Doc.t(tag);

type padding = string;
type child_padding =
  | Left(padding)
  | LeftRight(padding, padding);

type padded_child = (padding, doc, padding);

let doc_of_padding = (padding: padding): doc =>
  padding == "\n"
  ? Linebreak
  : Text(padding);

let doc_of_delim = (
  ~text: string,
  ~index: delim_index,
  ~left_padding: padding,
  ~right_padding: padding,
) =>
  Doc.(
    Tagged(
      Delim({text, index, left_padding, right_padding}),
      hcats([doc_of_padding(left_padding), Text(text), doc_of_padding(right_padding)]),
    )
  );

let tab_length = 2;
let align_and_indent = (~n=1, doc: doc): doc =>
  n === 0
    ? doc : Doc.hcats([Text(String.make(n * tab_length, ' ')), Align(doc)]);

let doc_of_EmptyHole = (u: MetaVarGen.t): doc => Text(string_of_int(u));

let doc_of_Var = (x: Var.t): doc => Text(x);

let doc_of_NumLit = (n: int): doc => Text(string_of_int(n));

let doc_of_BoolLit = (b: bool): doc => Text(string_of_bool(b));

let doc_of_ListNil: doc = Text("[]");

let doc_of_Parenthesized = (sbody_doc: doc): doc => {
  let open_delim = Doc.Text("(");
  let close_delim = Doc.Text(")");
  Doc.hcats([open_delim, sbody_doc, close_delim]);
};

let doc_of_Inj = (side: inj_side, sbody_doc: doc): doc => {
  let open_delim = Doc.Text("inj[" ++ (side == L ? "L" : "R") ++ "](");
  let close_delim = Doc.Text(")");
  Doc.hcats([open_delim, sbody_doc, close_delim]);
};

let doc_of_Lam = (sp_doc: doc, sann_doc: option(doc), sbody_doc: doc): doc => {
  let lam_delim = Doc.Text(LangUtil.lamSym);
  let dot_delim = Doc.Text(".");
  switch (sann_doc) {
  | None => Doc.hcats([lam_delim, sp_doc, dot_delim, sbody_doc])
  | Some(sann_doc) =>
    let colon_delim = Doc.Text(":");
    Doc.hcats([
      lam_delim,
      sp_doc,
      colon_delim,
      sann_doc,
      dot_delim,
      sbody_doc,
    ]);
  };
};

let doc_of_Case =
    (sscrut_doc: doc, rule_docs: list(doc), sann_doc: option(doc)): doc => {
  let case_delim = Doc.Text("case");
  let end_line =
    switch (sann_doc) {
    | None => Doc.Text("end")
    | Some(sann_doc) => Doc.hcats([Text("end"), sann_doc])
    };
  Doc.(
    hcats([
      case_delim,
      sscrut_doc,
      Linebreak,
      vseps(rule_docs),
      Linebreak,
      end_line,
    ])
  );
};

let doc_of_LetLine = (
  p_choices: list(padded_child),
  ann_choices: option(list(padded_child)),
  def_choices: list(padded_child),
): doc => {
  let let_delim = doc_of_delim(~text="let", ~index=0);
  let eq_delim = doc_of_delim(~text="=", ~index=2);
  let in_delim = doc_of_delim(~text="in", ~index=3);
  switch (ann_choices) {
  | None =>
    combos2(p_choices, def_choices)
    |> List.map((((p_left, p_doc, p_right), (def_left, def_doc, def_right))) =>
        Doc.(
          hcats([
            let_delim(~left_padding="", ~right_padding=p_left),
            p_doc,
            eq_delim(~left_padding=p_right, ~right_padding=def_left),
            def_doc,
            in_delim(~left_padding=def_right, ~right_padding=""),
          ])
        )
      )
  | Some(ann_choices) =>
    let colon_delim = doc_of_delim(~text=":", ~index=1);
    combos3(p_choices, ann_choices, def_choices)
    |> List.map(((
        (p_left, p_doc, p_right),
        (ann_left, ann_doc, ann_right),
        (def_left, def_doc, def_right),
      )) =>
        Doc.(
          hcats([
            let_delim(~left_padding="", ~right_padding=p_left),
            p_doc,
            colon_delim(~left_padding=p_right, ~right_padding=ann_left),
            ann_doc,
            eq_delim(~left_padding=ann_right, ~right_padding=def_left),
            def_doc,
            in_delim(~left_padding=def_right, ~right_padding=""),
          ])
        )
    );
  };
};

let apply_seps = (surround: sep_surround, doc: (~may_wrap: bool) => doc): doc =>
  switch (surround) {
  | None => doc(~may_wrap=true)
  | Left(lsep) =>
    Doc.(
      choices([
        hcats([lsep, doc(~may_wrap=false)]),
        hcats([Linebreak, align_and_indent(doc(~may_wrap=true))]),
      ])
    )
  | Both(lsep, rsep) =>
    Doc.(
      choices([
        hcats([lsep, doc(~may_wrap=false), rsep]),
        hcats([
          Linebreak,
          align_and_indent(doc(~may_wrap=true)),
          Linebreak,
        ]),
      ])
    )
  };

let choices_of_padded_child_typ =
    (
      ~surround as _: sep_surround,
      ~may_wrap as _: bool,
      ~steps as _: CursorPath.steps,
      _e: UHTyp.t,
    ) =>
  Doc.Text("typ");

let rec choices_of_padded_child_pat =
        (
          ~surround: sep_surround,
          ~may_wrap as _: bool,
          ~steps: CursorPath.steps,
          p: UHPat.t,
        ) => {
  let p_doc = (~may_wrap: bool) =>
    (
      switch (p) {
      | EmptyHole(u) => doc_of_EmptyHole(u)
      | Wild(_) => Text("_")
      | Var(_, _, x) => doc_of_Var(x)
      | NumLit(_, n) => doc_of_NumLit(n)
      | BoolLit(_, b) => doc_of_BoolLit(b)
      | ListNil(_) => doc_of_ListNil
      | Inj(_, side, body) =>
        let sbody_doc =
          choices_of_padded_child_pat(
            ~single_line_padding=LeftRight(Doc.empty, Doc.empty),
            ~may_wrap,
            ~steps=steps @ [0],
            body,
          );
        doc_of_Inj(side, sbody_doc);
      | Parenthesized(body) =>
        let sbody_doc =
          choices_of_padded_child_pat(
            ~single_line_padding=LeftRight(Doc.empty, Doc.empty),
            ~may_wrap,
            ~steps=steps @ [0],
            body,
          );
        doc_of_Parenthesized(sbody_doc);
      | OpSeq(_, _) => failwith("unimplemented: doc_of_pat/OpSeq")
      }
    )
    |> Doc.tag(Node({steps, shape: Pat(p)}));
  p_doc |> apply_seps(surround);
};

let rec choices_of_padded_child_block =
        (
          ~single_line_padding: child_padding,
          ~steps: CursorPath.steps,
          block: UHExp.block,
        )
        : list((doc, doc, doc)) =>
  switch (block) {
  | Block([], _) =>
    let block_doc = doc_of_block(~steps, block);
    block_doc |> padding_choices(~single_line_padding);
  | Block([_, ..._], _) =>
    // if block has leading lines, then
    // it must be separated by linebreaks
    let block_doc = doc_of_block(~may_wrap=true, ~steps, block);
    switch (single_line_padding) {
    | Left(_) => [(Linebreak, block_doc, Doc.empty)]
    | LeftRight(_, _) => [(Linebreak, block_doc, Linebreak)]
    };
  }
and doc_of_block = (
  ~may_wrap: bool,
  ~steps: CursorPath.steps,
  block: UHExp.block,
): doc => {
  let tag_block = Doc.tag(Node({steps, shape: Block(block)}));
  switch (may_wrap, block) {
  | (_, Block([], conclusion)) =>
    doc_of_exp(~may_wrap, ~steps=steps @ [0], conclusion);
    |> tag_block
  | (false, Block([_, ..._], _)) => Fail
  | (true, Block([_, ..._] as leading, conclusion)) =>
    let leading_docs =
      leading
      |> List.mapi((i, line) => doc_of_line(~steps=steps @ [i], line));
    let conclusion_doc =
      doc_of_exp(
        ~may_wrap=true,
        ~steps=steps @ [List.length(leading)],
        conclusion,
      );
    Doc.vseps(leading_docs @ [conclusion_doc]) |> Doc.align |> tag_block;
  }
}
and doc_of_line = (~steps: CursorPath.steps, line: UHExp.line): doc => {
  let tag_line = Doc.tag(Node({steps, shape: Line(line)}));
  switch (line) {
  | ExpLine(e) =>
    // ghost node, do not tag
    doc_of_exp(~may_wrap=true, ~steps, e)
  | EmptyLine => Doc.Text("") |> tag_line
  | LetLine(p, ann, def) =>
    let p_choices =
      choices_of_padded_child_pat(
        ~single_line_padding=LeftRight(Doc.space, Doc.space),
        ~steps=steps @ [0],
        p,
      );
    let ann_choices =
      ann
      |> Opt.map(
           choices_of_padded_child_typ(
             ~single_line_padding=LeftRight(Doc.space, Doc.space),
             ~steps=steps @ [1],
           ),
         );
    let def_choices =
      choices_of_padded_child_block(
        ~single_line_padding=LeftRight(Doc.space, Doc.space),
        ~steps=steps @ [2],
        def,
      );
    doc_of_LetLine(p_choices, ann_choices, def_choices)
    |> tag_line;
  };
}
and doc_of_exp = (~may_wrap: bool, ~steps: CursorPath.steps, e: UHExp.t): doc => {
  let doc =
    switch (e) {
    | EmptyHole(u) => doc_of_EmptyHole(u)
    | Var(_, _, x) => doc_of_Var(x)
    | NumLit(_, n) => doc_of_NumLit(n)
    | BoolLit(_, b) => doc_of_BoolLit(b)
    | ListNil(_) => doc_of_ListNil
    | Lam(_, p, ann, body) =>
      let sp_doc =
        choices_of_padded_child_pat(
          ~single_line_padding=LeftRight(Doc.empty, Doc.empty),
          ~may_wrap,
          ~steps=steps @ [0],
          p,
        );
      let sann_doc =
        ann
        |> Opt.map(
             choices_of_padded_child_typ(
               ~single_line_padding=LeftRight(Doc.empty, Doc.empty),
               ~steps=steps @ [1],
             ),
           );
      let sbody_doc =
        choices_of_padded_child_block(
          ~single_line_padding=Left(Doc.empty),
          ~steps=steps @ [2],
          body,
        );
      doc_of_Lam(sp_doc, sann_doc, sbody_doc);
    | Inj(_, side, body) =>
      let sbody_doc =
        choices_of_padded_child_block(
          ~single_line_padding=LeftRight(Doc.empty, Doc.empty),
          ~may_wrap,
          ~steps=steps @ [0],
          body,
        );
      doc_of_Inj(side, sbody_doc);
    | Parenthesized(body) =>
      let sbody_doc =
        choices_of_padded_child_block(
          ~single_line_padding=LeftRight(Doc.empty, Doc.empty),
          ~may_wrap,
          ~steps=steps @ [0],
          body,
        );
      doc_of_Parenthesized(sbody_doc);
    | Case(_, scrut, rules, ann) =>
      if (!may_wrap) {
        Fail;
      } else {
        let sscrut_doc =
          choices_of_padded_child_block(
            ~single_line_padding=Left(Doc.space),
            ~may_wrap=true,
            ~steps=steps @ [0],
            scrut,
          );
        let rule_docs =
          rules
          |> List.mapi((i, rule) =>
               doc_of_rule(~steps=steps @ [1 + i], rule)
             );
        let sann_doc =
          ann
          |> Opt.map(
               choices_of_padded_child_typ(
                 ~single_line_padding=Left(Doc.space),
                 ~may_wrap=true,
                 ~steps=steps @ [1 + List.length(rules)],
               ),
             );
        doc_of_Case(sscrut_doc, rule_docs, sann_doc);
      }
    | OpSeq(_, _) => failwith("unimplemented: doc_of_exp/OpSeq")
    | ApPalette(_, _, _, _) =>
      failwith("unimplemented: doc_of_exp/ApPalette")
    };
  Tagged(Node({steps, shape: Exp(e)}), doc);
}
and doc_of_rule =
    (~steps: CursorPath.steps, Rule(p, clause) as rule: UHExp.rule) => {
  let sp_doc =
    choices_of_padded_child_pat(
      ~single_line_padding=LeftRight(Doc.space, Doc.space),
      ~may_wrap=false,
      ~steps=steps @ [0],
      p,
    );
  let sclause_doc =
    choices_of_padded_child_block(
      ~single_line_padding=Left(Doc.space),
      ~may_wrap=true,
      ~steps=steps @ [1],
      clause,
    );
  let rule_doc =
    Doc.hcats([
      Text("|"),
      sp_doc,
      Text(LangUtil.caseArrowSym),
      sclause_doc,
    ]);
  rule_doc |> Doc.tag(Node({steps, shape: Rule(rule)}));
};
let doc_of_block = choices_of_padded_child_block(~surround=None, ~may_wrap=true);
