open GeneralUtil;

[@deriving sexp]
type node_shape =
  | Block(UHExp.block)
  | Line(UHExp.line)
  | Exp(UHExp.t)
  | Rule(UHExp.rule)
  | Pat(UHPat.t)
  | Typ(UHTyp.t);

[@deriving sexp]
type tag = {
  steps: CursorPath.steps,
  node_shape,
};

[@deriving sexp]
type doc = Doc.t(tag);

/**
 * How a node is separated from surrounding
 * delimiters in the parent node (if there is
 * one). Constructor args represent default
 * separators if the surrounded node is inlined.
 */
type sep_surround =
  | None
  | Left(doc)
  | Both(doc, doc);

let tab_length = 2;
let align_and_indent = (~n=1, doc: doc): doc =>
  n === 0
    ? doc : Doc.hcats([Text(String.make(n * tab_length, ' ')), Align(doc)]);

let doc_of_EmptyHole = (u: MetaVarGen.t): doc => Text(string_of_int(u));

let doc_of_Var = (x: Var.t): doc => Text(x);

let doc_of_NumLit = (n: int): doc => Text(string_of_int(n));

let doc_of_BoolLit = (b: bool): doc => Text(string_of_bool(b));

let doc_of_StringLit = (s: string): doc => Text(s);

let doc_of_ListNil: doc = Text("[]");

let doc_of_Parenthesized = (sbody_doc: doc): doc => {
  let open_delim = Doc.Text("(");
  let close_delim = Doc.Text(")");
  Doc.hcats([open_delim, sbody_doc, close_delim]);
};

let doc_of_Inj = (side: InjSide.t, sbody_doc: doc): doc => {
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

let doc_of_LetLine = (sp_doc: doc, sann_doc: option(doc), sdef_doc: doc): doc => {
  let let_delim = Doc.Text("let");
  let eq_delim = Doc.Text("=");
  let in_delim = Doc.Text("in");
  switch (sann_doc) {
  | None => Doc.hcats([let_delim, sp_doc, eq_delim, sdef_doc, in_delim])
  | Some(sann_doc) =>
    let colon_delim = Doc.Text(":");
    Doc.hcats([
      let_delim,
      sp_doc,
      colon_delim,
      sann_doc,
      eq_delim,
      sdef_doc,
      in_delim,
    ]);
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

let doc_of_separated_typ =
    (
      ~surround as _: sep_surround,
      ~may_wrap as _: bool,
      ~steps as _: CursorPath.steps,
      _e: UHTyp.t,
    ) =>
  Doc.Text("typ");

let rec doc_of_separated_pat =
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
      | StringLit(_, s) => doc_of_StringLit(s)
      | ListNil(_) => doc_of_ListNil
      | Inj(_, side, body) =>
        let sbody_doc =
          doc_of_separated_pat(
            ~surround=Both(Doc.empty, Doc.empty),
            ~may_wrap,
            ~steps=steps @ [0],
            body,
          );
        doc_of_Inj(side, sbody_doc);
      | Parenthesized(body) =>
        let sbody_doc =
          doc_of_separated_pat(
            ~surround=Both(Doc.empty, Doc.empty),
            ~may_wrap,
            ~steps=steps @ [0],
            body,
          );
        doc_of_Parenthesized(sbody_doc);
      | OpSeq(_, _) => failwith("unimplemented: doc_of_pat/OpSeq")
      }
    )
    |> Doc.tag({steps, node_shape: Pat(p)});
  p_doc |> apply_seps(surround);
};

let rec doc_of_separated_block =
        (
          ~surround: sep_surround,
          ~may_wrap: bool,
          ~steps: CursorPath.steps,
          block: UHExp.block,
        )
        : doc => {
  let tag = Doc.tag({steps, node_shape: Block(block)});
  switch (may_wrap, block) {
  | (_, Block([], conclusion)) =>
    let block_doc = (~may_wrap: bool) =>
      doc_of_exp(~may_wrap, ~steps=steps @ [0], conclusion) |> tag;
    block_doc |> apply_seps(surround);
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
    let block_doc =
      Doc.vseps(leading_docs @ [conclusion_doc]) |> Doc.align |> tag;
    // if block has leading lines, then it
    // must be separated by linebreaks
    switch (surround) {
    | None => block_doc
    | Left(_) => Doc.hcats([Linebreak, block_doc])
    | Both(_, _) => Doc.hcats([Linebreak, block_doc, Linebreak])
    };
  };
}
and doc_of_line = (~steps: CursorPath.steps, line: UHExp.line): doc => {
  let tag = Doc.tag({steps, node_shape: Line(line)});
  switch (line) {
  | ExpLine(e) => doc_of_exp(~may_wrap=true, ~steps, e) |> tag
  | EmptyLine => Doc.Text("") |> tag
  | LetLine(p, ann, def) =>
    let sp_doc =
      doc_of_separated_pat(
        ~surround=Both(Doc.space, Doc.space),
        ~may_wrap=true,
        ~steps=steps @ [0],
        p,
      );
    let sann_doc =
      ann
      |> Opt.map(
           doc_of_separated_typ(
             ~surround=Both(Doc.space, Doc.space),
             ~may_wrap=true,
             ~steps=steps @ [1],
           ),
         );
    let sdef_doc =
      doc_of_separated_block(
        ~surround=Both(Doc.space, Doc.space),
        ~may_wrap=true,
        ~steps=steps @ [2],
        def,
      );
    doc_of_LetLine(sp_doc, sann_doc, sdef_doc) |> tag;
  };
}
and doc_of_exp = (~may_wrap: bool, ~steps: CursorPath.steps, e: UHExp.t): doc => {
  let doc =
    switch (e) {
    | EmptyHole(u) => doc_of_EmptyHole(u)
    | Var(_, _, x) => doc_of_Var(x)
    | NumLit(_, n) => doc_of_NumLit(n)
    | BoolLit(_, b) => doc_of_BoolLit(b)
    | StringLit(_, s) => doc_of_StringLit(s)
    | ListNil(_) => doc_of_ListNil
    | Lam(_, p, ann, body) =>
      let sp_doc =
        doc_of_separated_pat(
          ~surround=Both(Doc.empty, Doc.empty),
          ~may_wrap,
          ~steps=steps @ [0],
          p,
        );
      let sann_doc =
        ann
        |> Opt.map(
             doc_of_separated_typ(
               ~surround=Both(Doc.empty, Doc.empty),
               ~may_wrap,
               ~steps=steps @ [1],
             ),
           );
      let sbody_doc =
        doc_of_separated_block(
          ~surround=Left(Doc.empty),
          ~may_wrap,
          ~steps=steps @ [2],
          body,
        );
      doc_of_Lam(sp_doc, sann_doc, sbody_doc);
    | Inj(_, side, body) =>
      let sbody_doc =
        doc_of_separated_block(
          ~surround=Both(Doc.empty, Doc.empty),
          ~may_wrap,
          ~steps=steps @ [0],
          body,
        );
      doc_of_Inj(side, sbody_doc);
    | Parenthesized(body) =>
      let sbody_doc =
        doc_of_separated_block(
          ~surround=Both(Doc.empty, Doc.empty),
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
          doc_of_separated_block(
            ~surround=Left(Doc.space),
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
               doc_of_separated_typ(
                 ~surround=Left(Doc.space),
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
  Tagged({steps, node_shape: Exp(e)}, doc);
}
and doc_of_rule =
    (~steps: CursorPath.steps, Rule(p, clause) as rule: UHExp.rule) => {
  let sp_doc =
    doc_of_separated_pat(
      ~surround=Both(Doc.space, Doc.space),
      ~may_wrap=false,
      ~steps=steps @ [0],
      p,
    );
  let sclause_doc =
    doc_of_separated_block(
      ~surround=Left(Doc.space),
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
  rule_doc |> Doc.tag({steps, node_shape: Rule(rule)});
};
let doc_of_block = doc_of_separated_block(~surround=None, ~may_wrap=true);
