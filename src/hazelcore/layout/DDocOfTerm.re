open Sexplib.Std;
open GeneralUtil;

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

  let mk = (~steps: CursorPath.steps, ~shape: shape): t => {steps, shape};
};

module DelimTag = {
  [@deriving sexp]
  type t = {
    text: string,
    index: DelimIndex.t,
  };

  let mk = (~text: string, ~index: DelimIndex.t): t => {text, index};
};

module OpTag = {
  [@deriving sexp]
  type t = {
    text: string,
    index: OpIndex.t,
  };

  let mk = (~text: string, ~index: OpIndex.t): t => {text, index};
};

[@deriving sexp]
module Tag = {
  type t =
    | DelimGroup
    | Node(NodeTag.t)
    | Delim(DelimTag.t)
    | Op(OpTag.t);

  let mk_Node = (~steps, ~shape) => Node(NodeTag.mk(~steps, ~shape));
  let mk_Delim = (~text, ~index) => Delim(DelimTag.mk(~text, ~index));
  let mk_Op = (~text, ~index) => Op(OpTag.mk(~text, ~index));
};

type ddoc = DDoc.t(Tag.t);

let indent = DDoc.Text("  ");

let pad_child = (~inline_padding as (left, right): (ddoc, ddoc), child: ddoc): ddoc =>
  Choice([
    Cat([left, child, right]),
    Cat([Linebreak, Cat([indent, child_doc]), Linebreak]),
  ]);

let child_padding_choices = DDoc.[(Linebreak, Linebreak), (space, space)];

let ddoc_of_delim = (~text: string, ~index: DelimIndex.t) =>
  DDoc.(Tag(Tag.mk_Delim(~text, ~index), Text(text)));

let ddoc_of_EmptyHole = (u: MetaVarGen.t): ddoc => Text(string_of_int(u));

let ddoc_of_Var = (x: Var.t): ddoc => Text(x);

let ddoc_of_NumLit = (n: int): ddoc => Text(string_of_int(n));

let ddoc_of_BoolLit = (b: bool): ddoc => Text(string_of_bool(b));

let ddoc_of_ListNil: ddoc = Text("[]");

let ddoc_of_Parenthesized = (body_doc: ddoc): ddoc => {
  let open_delim = DDoc.Text("(");
  let close_delim = DDoc.Text(")");
  DDoc.(
    Choice([
      Cat([open_delim, body_doc, close_delim]),
      Cat([
        open_delim,
        Linebreak,
        Cat([indent, body_doc]),
        Linebreak,
        close_delim,
      ]),
    ]),
  );
};

let ddoc_of_Inj = (side: InjSide.t, body_doc: ddoc): ddoc => {
  let open_delim = DDoc.Text("inj[" ++ (side == L ? "L" : "R") ++ "](");
  let close_delim = DDoc.Text(")");
  DDoc.(Choice([
    Cat([open_delim, sbody_doc, close_delim]),
    Cat([
      open_delim,
      Linebreak,
      Cat([indent, body_doc]),
      Linebreak,
      close_delim,
    ])
  ]));
};

let ddoc_of_Lam =
    (sp_doc: ddoc, sann_doc: option(ddoc), sbody_doc: ddoc): ddoc => {
  let lam_delim = DDoc.Text(LangUtil.lamSym);
  let dot_delim = DDoc.Text(".");
  switch (sann_doc) {
  | None => DDoc.(Choice([
      Cat([lam_delim, sp_doc, dot_delim, sbody_doc]),

    ]))
  | Some(sann_doc) =>
    let colon_delim = DDoc.Text(":");
    DDoc.Cat([
      lam_delim,
      sp_doc,
      colon_delim,
      sann_doc,
      dot_delim,
      sbody_doc,
    ]);
  };
};

// TODO(d): review structure of Case and make sure
// this leads to reasonable staging behavior
let ddoc_of_Case =
    (scrut_doc: ddoc, rule_docs: list(ddoc), ann_doc: option(ddoc)): ddoc => {
  let delim_group_1: ddoc = {
    let case_delim = DDoc.Text("case");
    Tag(DelimGroup, case_delim);
  };
  let delim_group_2: ddoc = {
    let end_delim = DDoc.Text("end");
    // TODO(d): revisit to support placing type annotation on new line
    switch (ann_doc) {
    | None => end_delim
    | Some(ann_doc) => Cat([end_delim, Text(" : "), ann_doc])
    }
  };
  Choice([
    Box([
      Choice([

      ])
    ])
  ])



  let space_or_linebreak = DDoc.[Linebreak, space];
  combos2(
    space_or_linebreak,
    space_or_linebreak,
  )
  |> List.map(((scrut_left))

  )

  DDoc.(
    Box([
      Cat([case_delim, sscrut_doc])
      case_delim,
      sscrut_doc,
      Linebreak,
      vseps(rule_docs),
      Linebreak,
      end_line,
    ])
  );
};

let ddoc_of_LetLine =
    (p_doc: ddoc, ann_doc: option(ddoc), def_doc: ddoc): ddoc => {
  let delim_group_1: ddoc = {
    let let_delim = ddoc_of_delim(~text="let", ~index=0);
    let eq_delim = ddoc_of_delim(~text="=", ~index=2);
    let doc =
      switch (ann_doc) {
      | None =>
        DDoc.(Choice([
          Cat([
            let_delim,
            p_doc |> pad_child,
            eq_delim,
          ])
        ]))
      | Some(ann_doc) =>
        let colon_delim = ddoc_of_delim(~text=":", ~index=1);
        DDoc.(
          Choice([
            Cat([
              let_delim,
              p_doc |> pad_child,
              colon_delim,
              ann_doc |> pad_child,
              eq_delim,
            ])
          ])
        );
      };
    Tag(DelimGroup, doc);
  };
  let delim_group_2: ddoc = {
    let in_delim = ddoc_of_delim(~text="in", ~index=3);
    Tag(DelimGroup, in_delim);
  };
  Choice([
    Box([Cat([delim_group_1, def_doc, delim_group_2])]),
    Box([
      delim_group_1,
      Cat([indent, def_doc]),
      delim_group_2
    ]),
  ]);
};

let rec ddoc_of_pat = (~steps: CursorPath.steps, p: UHPat.t): ddoc => {
  let doc: ddoc =
    switch (p) {
    | EmptyHole(u) => ddoc_of_EmptyHole(u)
    | Wild(_) => Text("_")
    | Var(_, _, x) => ddoc_of_Var(x)
    | NumLit(_, n) => ddoc_of_NumLit(n)
    | BoolLit(_, b) => ddoc_of_BoolLit(b)
    | ListNil(_) => ddoc_of_ListNil
    | Inj(_, side, body) =>
      let body_doc = ddoc_of_body(~steps=steps @ [0], body);
      ddoc_of_Inj(side, body_doc);
    | Parenthesized(body) =>
      let body_doc = ddoc_of_body(~steps=steps @ [0], body);
      ddoc_of_Parenthesized(body_doc);
    | OpSeq(_, _) => failwith("unimplemented: ddoc_of_pat/OpSeq")
    };
  DDoc.Tag(Tag.mk_Node(~steps, ~shape=Pat(p)), doc);
};

let rec ddoc_of_block = (~steps: CursorPath.steps, block: UHExp.block): ddoc => {
  let doc =
    switch (block) {
    | Block([], conclusion) => ddoc_of_exp(~steps=steps @ [0], conclusion)
    | Block([_, ..._] as leading, conclusion) =>
      let leading_docs =
        leading
        |> List.mapi((i, line) => ddoc_of_line(~steps=steps @ [i], line));
      let conclusion_doc =
        ddoc_of_exp(~steps=steps @ [List.length(leading)], conclusion);
      Box(leading_docs @ [conclusion_doc]);
    };
  DDoc.Tag(Tag.mk_Node(~steps, ~shape=Block(block)), doc);
}
and ddoc_of_line = (~steps: CursorPath.steps, line: UHExp.line): ddoc => {
  let doc =
    switch (line) {
    | EmptyLine => DDoc.Text("")
    | ExpLine(e) => ddoc_of_exp(~steps, e)
    | LetLine(p, ann, def) =>
      let p_doc = ddoc_of_pat(~steps=steps @ [0], p);
      let ann_doc = ann |> Opt.map(ddoc_of_typ(~steps=steps @ [1]));
      let def_doc = ddoc_of_block(~steps=steps @ [2], def);
      ddoc_of_LetLine(p_doc, ann_doc, def_doc);
    };
  DDoc.Tag(Tag.mk_Node(~steps, ~shape=Line(line)), doc);
}
and ddoc_of_exp = (~steps: CursorPath.steps, e: UHExp.t): ddoc => {
  let doc =
    switch (e) {
    | EmptyHole => ddoc_of_EmptyHole(u)
    | Var(_, _, x) => ddoc_of_Var(x)
    | NumLit(_, n) => ddoc_of_NumLit(n)
    | BoolLit(_, b) => ddoc_of_BoolLit(b)
    | ListNil(_) => ddoc_of_ListNil
    | Lam(_, p, ann, body) =>
      let p_doc = ddoc_of_pat(~steps=steps @ [0], p);
      let ann_doc = ann |> Opt.map(ddoc_of_typ(~steps=steps @ [1]));
      let def_doc = ddoc_of_block(~steps=steps @ [2], def);
      ddoc_of_Lam(p_doc, ann_doc, def_doc);
    | Inj(_, side, body) =>
      let body_doc = ddoc_of_block(~steps=steps @ [0], body);
      ddoc_of_Inj(side, body_doc);
    | Parenthesized(body) =>
      let body_doc = ddoc_of_block(~steps=steps @ [0], body);
      ddoc_of_Parenthesized(body_doc);
    | Case(_, scrut, rules, ann) =>
      let scrut_doc = ddoc_of_block(~steps=steps @ [0], scrut);
      let rule_docs =
        rules
        |> List.mapi((i, rule) =>
             ddoc_of_rule(~steps=steps @ [1 + i], rule)
           );
      let ann_doc =
        ann
        |> Opt.map(ddoc_of_ann(~steps=steps @ [1 + List.length(rules)]));
      ddoc_of_Case(scrut_doc, rule_docs, ann_doc);
    | OpSeq(_, _) => failwith("unimplemented: ddoc_of_exp/OpSeq")
    | ApPalette(_, _, _, _) =>
      failwith("unimplemented: ddoc_of_exp/ApPalette")
    };
  Tag(Tag.mk_Node(~steps, ~shape=Exp(e)), doc);
}
and ddoc_of_rule =
    (~steps: CursorPath.steps, Rule(p, clause) as rule: UHExp.rule): ddoc => {
  let p_doc = ddoc_of_pat(~steps=steps @ [0], p);
  let clause_doc = ddoc_of_block(~steps=steps @ [1], clause);
  let doc = {
    let bar_delim = DDoc.Text("|");
    let arrow_delim = DDoc.Text(LangUtil.caseArrowSym);
    DDoc.Choice(
      combos2(
        DDoc.[(Linebreak, Linebreak), (space, space)],
        DDoc.[Linebreak, space],
      )
      |> List.map((((p_left, p_right), clause_left)) =>
           DDoc.Cat([
             bar_delim,
             p_left,
             p_doc,
             p_right,
             arrow_delim,
             clause_left,
             clause_doc,
           ])
         ),
    );
  };
  DDoc.Tag(Tag.mk_Node(~steps, ~shape=Rule(rule)), doc);
};
