open SemanticsCommon;

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

module LayoutOfDoc =
  LayoutOfDoc.Make({
    type t = tag;
  });

let tab_length = 2;
let indent = (~n=1, doc: doc): doc =>
  n === 0
    ? doc : Doc.hcats([Text(String.make(n * tab_length, ' ')), Align(doc)]);

/**
 * Given a tokens representation of an AST node,
 * generates the combinatorial enumeration of doc
 * choices caused by variation in layout depending
 * on whether each child term wraps to multiple lines.
 */;
/*
 let rec choices_of_tokens = (~wrap: bool, tokens: tokens): list(doc) =>
   switch (tokens) {
   | DelimStart(delim_start) => choices_of_delim_start(~wrap, delim_start)
   }
 and choices_of_delim_start =
     (~wrap: bool, delim_start: delim_start): list(doc) =>
   switch (delim_start) {
   | Delim(delim_doc) => [delim_doc]
   | DCons(delim_doc, child_start) =>
     let (single_line_choices, multi_line_choices) =
       choices_of_child_start(child_start);
     let delim_single_line_choices =
       single_line_choices
       |> List.map(choice => Doc.hseps([delim_doc, choice]));
     let delim_multi_line_choices =
       multi_line_choices |> List.map(choice => Doc.vseps([delim_doc, choice]));
     delim_single_line_choices @ delim_multi_line_choices;
   }
 // fst of return pair is set of choices if starting child is single line,
 // snd of return pair is set of choices if starting child is multi line
 and choices_of_child_start: child_start => (list(doc), list(doc)) =
   fun
   | Child(child_doc) => ([child_doc], [child_doc])
   | CCons(child_doc, delim_start) => {
       let choices = choices_of_delim_start(delim_start);
       let single_line_choices =
         choices |> List.map(_choice => Doc.hseps([child_doc, ...choices]));
       let multi_line_choices =
         choices
         |> List.map(_choice => Doc.vseps([indent(child_doc), ...choices]));
       (single_line_choices, multi_line_choices);
     };
 */

let doc_of_EmptyHole = (u: MetaVarGen.t): doc => Text(string_of_int(u));

let doc_of_Var = (x: Var.t): doc => Text(x);

let doc_of_NumLit = (n: int): doc => Text(string_of_int(n));

let doc_of_BoolLit = (b: bool): doc => Text(string_of_bool(b));

let doc_of_ListNil: doc = Text("[]");

let doc_of_Parenthesized = (~wrap: bool, body_doc: (~wrap: bool) => doc): doc => {
  let open_delim = Doc.Text("(");
  let close_delim = Doc.Text(")");
  let single_line_doc =
    Doc.hseps([open_delim, body_doc(~wrap=false), close_delim]);
  wrap
    ? Doc.(
        choices([
          single_line_doc,
          vseps([open_delim, indent(body_doc(~wrap=true)), close_delim]),
        ])
      )
    : single_line_doc;
};

let doc_of_Inj =
    (~wrap: bool, side: inj_side, body_doc: (~wrap: bool) => doc): doc => {
  let open_delim = Doc.Text("inj[" ++ (side == L ? "L" : "R") ++ "](");
  let close_delim = Doc.Text(")");
  let single_line_doc =
    Doc.hseps([open_delim, body_doc(~wrap=false), close_delim]);
  wrap
    ? Doc.(
        choices([
          single_line_doc,
          vseps([open_delim, indent(body_doc(~wrap=true)), close_delim]),
        ])
      )
    : single_line_doc;
};

let doc_of_typ =
    (~wrap as _: bool, ~steps as _: CursorPath.steps, _e: UHTyp.t) =>
  Doc.Text("typ");

let rec doc_of_pat = (~wrap: bool, ~steps: CursorPath.steps, p: UHPat.t) => {
  let doc: doc =
    switch (p) {
    | EmptyHole(u) => doc_of_EmptyHole(u)
    | Wild(_) => Text("_")
    | Var(_, _, x) => doc_of_Var(x)
    | NumLit(_, n) => doc_of_NumLit(n)
    | BoolLit(_, b) => doc_of_BoolLit(b)
    | ListNil(_) => doc_of_ListNil
    | Inj(_, side, body) =>
      let body_doc = doc_of_pat(~steps=steps @ [0], body);
      doc_of_Inj(~wrap, side, body_doc);
    | Parenthesized(body) =>
      let body_doc = doc_of_pat(~steps=steps @ [0], body);
      doc_of_Parenthesized(~wrap, body_doc);
    | OpSeq(_, _) => failwith("unimplemented: doc_of_pat OpSeq")
    };
  Tagged({steps, node_shape: Pat(p)}, doc);
};

let rec doc_of_block =
        (~wrap, ~steps: CursorPath.steps, block: UHExp.block): doc =>
  switch (wrap, block) {
  | (false, Block([_, ..._], _)) => Fail
  | (_, Block(leading, conclusion)) =>
    let leading_docs =
      leading
      |> List.mapi((i, line) => doc_of_line(~steps=steps @ [i], line));
    let conclusion_doc =
      doc_of_exp(
        ~wrap=true,
        ~steps=steps @ [List.length(leading)],
        conclusion,
      );
    let docs = Doc.vseps(leading_docs @ [conclusion_doc]);
    Tagged({steps, node_shape: Block(block)}, docs);
  }
and doc_of_line = (~steps: CursorPath.steps, line: UHExp.line): doc => {
  let tag = {steps, node_shape: Line(line)};
  switch (line) {
  | ExpLine(e) => doc_of_exp(~wrap=true, ~steps, e)
  | EmptyLine => Tagged(tag, Doc.Text(""))
  | LetLine(p, ann, def) =>
    let let_delim = Doc.Text("let");
    let p_doc = doc_of_pat(~steps=steps @ [0], p);
    let eq_delim = Doc.Text("=");
    let def_doc = doc_of_block(~steps=steps @ [2], def);
    let in_delim = Doc.Text("in");
    let choices =
      switch (ann) {
      | None =>
        Doc.(
          choices([
            hseps([
              let_delim,
              p_doc(~wrap=false),
              eq_delim,
              def_doc(~wrap=false),
              in_delim,
            ]),
            vseps([
              let_delim,
              indent(p_doc(~wrap=true)),
              hseps([eq_delim, def_doc(~wrap=false), in_delim]),
            ]),
            vseps([
              hseps([let_delim, p_doc(~wrap=false), eq_delim]),
              indent(def_doc(~wrap=true)),
              in_delim,
            ]),
            vseps([
              let_delim,
              indent(p_doc(~wrap=true)),
              eq_delim,
              indent(def_doc(~wrap=true)),
              in_delim,
            ]),
          ])
        )
      | Some(ann) =>
        let colon_delim = Doc.Text(":");
        let ann_doc = doc_of_typ(~steps=steps @ [1], ann);
        Doc.(
          choices([
            hseps([
              let_delim,
              p_doc(~wrap=false),
              colon_delim,
              ann_doc(~wrap=false),
              eq_delim,
              def_doc(~wrap=false),
              in_delim,
            ]),
            vseps([
              hseps([
                let_delim,
                p_doc(~wrap=false),
                colon_delim,
                ann_doc(~wrap=false),
                eq_delim,
              ]),
              indent(def_doc(~wrap=true)),
              in_delim,
            ]),
            vseps([
              hseps([let_delim, p_doc(~wrap=false), colon_delim]),
              indent(ann_doc(~wrap=true)),
              hseps([eq_delim, def_doc(~wrap=false), in_delim]),
            ]),
            vseps([
              hseps([let_delim, p_doc(~wrap=false), colon_delim]),
              indent(ann_doc(~wrap=true)),
              eq_delim,
              indent(def_doc(~wrap=true)),
              in_delim,
            ]),
            vseps([
              let_delim,
              indent(p_doc(~wrap=true)),
              hseps([
                colon_delim,
                ann_doc(~wrap=false),
                eq_delim,
                def_doc(~wrap=false),
                in_delim,
              ]),
            ]),
            vseps([
              let_delim,
              indent(p_doc(~wrap=true)),
              hseps([colon_delim, ann_doc(~wrap=false), eq_delim]),
              indent(def_doc(~wrap=true)),
              in_delim,
            ]),
            vseps([
              let_delim,
              indent(p_doc(~wrap=true)),
              colon_delim,
              indent(ann_doc(~wrap=true)),
              hseps([eq_delim, def_doc(~wrap=false), in_delim]),
            ]),
            vseps([
              let_delim,
              indent(p_doc(~wrap=true)),
              colon_delim,
              indent(ann_doc(~wrap=true)),
              eq_delim,
              indent(def_doc(~wrap=true)),
              in_delim,
            ]),
          ])
        );
      };
    Tagged(tag, choices);
  };
}
and doc_of_exp = (~wrap: bool, ~steps: CursorPath.steps, e: UHExp.t): doc => {
  let doc: doc =
    switch (e) {
    | EmptyHole(u) => doc_of_EmptyHole(u)
    | Var(_, _, x) => doc_of_Var(x)
    | NumLit(_, n) => doc_of_NumLit(n)
    | BoolLit(_, b) => doc_of_BoolLit(b)
    | ListNil(_) => doc_of_ListNil
    | Lam(_, p, None, body) =>
      let lam_delim = Doc.Text(LangUtil.lamSym);
      let p_doc = doc_of_pat(~steps=steps @ [0], p);
      let dot_delim = Doc.Text(".");
      let body_doc = doc_of_block(~steps=steps @ [2], body);
      let single_line_doc =
        Doc.hseps([
          lam_delim,
          p_doc(~wrap=false),
          dot_delim,
          body_doc(~wrap=false),
        ]);
      wrap
        ? Doc.(
            choices([
              single_line_doc,
              vseps([
                lam_delim,
                indent(p_doc(~wrap=true)),
                hseps([dot_delim, body_doc(~wrap=false)]),
              ]),
              vseps([
                hseps([lam_delim, p_doc(~wrap=false), dot_delim]),
                indent(body_doc(~wrap=true)),
              ]),
              vseps([
                lam_delim,
                indent(p_doc(~wrap=true)),
                dot_delim,
                indent(body_doc(~wrap=true)),
              ]),
            ])
          )
        : single_line_doc;
    | Lam(_, p, Some(ann), body) =>
      let lam_delim = Doc.Text(LangUtil.lamSym);
      let p_doc = doc_of_pat(~steps=steps @ [0], p);
      let colon_delim = Doc.Text(":");
      let ann_doc = doc_of_typ(~steps=steps @ [1], ann);
      let dot_delim = Doc.Text(".");
      let body_doc = doc_of_block(~steps=steps @ [2], body);
      let single_line_doc =
        Doc.hseps([
          lam_delim,
          p_doc(~wrap=false),
          colon_delim,
          ann_doc(~wrap=false),
          dot_delim,
          body_doc(~wrap=false),
        ]);
      wrap
        ? Doc.(
            choices([
              single_line_doc,
              vseps([
                hseps([
                  lam_delim,
                  p_doc(~wrap=false),
                  colon_delim,
                  ann_doc(~wrap=false),
                  dot_delim,
                ]),
                indent(body_doc(~wrap=true)),
              ]),
              vseps([
                hseps([lam_delim, p_doc(~wrap=false), colon_delim]),
                indent(ann_doc(~wrap=true)),
                hseps([dot_delim, body_doc(~wrap=false)]),
              ]),
              vseps([
                hseps([lam_delim, p_doc(~wrap=false), colon_delim]),
                indent(ann_doc(~wrap=true)),
                dot_delim,
                indent(body_doc(~wrap=true)),
              ]),
              vseps([
                lam_delim,
                indent(p_doc(~wrap=true)),
                hseps([
                  colon_delim,
                  ann_doc(~wrap=false),
                  dot_delim,
                  body_doc(~wrap=false),
                ]),
              ]),
              vseps([
                lam_delim,
                indent(p_doc(~wrap=true)),
                hseps([colon_delim, ann_doc(~wrap=false), dot_delim]),
                indent(body_doc(~wrap=true)),
              ]),
              vseps([
                lam_delim,
                indent(p_doc(~wrap=true)),
                colon_delim,
                indent(ann_doc(~wrap=true)),
                hseps([dot_delim, body_doc(~wrap=false)]),
              ]),
              vseps([
                lam_delim,
                indent(p_doc(~wrap=true)),
                colon_delim,
                indent(ann_doc(~wrap=true)),
                dot_delim,
                indent(body_doc(~wrap=true)),
              ]),
            ])
          )
        : single_line_doc;
    | Inj(_, side, body) =>
      let body_doc = doc_of_block(~steps=steps @ [0], body);
      doc_of_Inj(~wrap, side, body_doc);
    | Parenthesized(body) =>
      let body_doc = doc_of_block(~steps=steps @ [0], body);
      doc_of_Parenthesized(~wrap, body_doc);
    | Case(_, scrut, rules, ann) =>
      if (wrap) {
        let case_delim = Doc.Text("case");
        let scrut_doc = doc_of_block(~steps=steps @ [0], scrut);
        let rules_docs =
          rules
          |> List.mapi((i, rule) =>
               doc_of_rule(~steps=steps @ [i + 1], rule)
             );
        switch (ann) {
        | None =>
          let end_delim = Doc.Text("end");
          Doc.choices([
            Doc.vseps(
              [
                Doc.hseps([case_delim, scrut_doc(~wrap=false)]),
                ...rules_docs,
              ]
              @ [end_delim],
            ),
            Doc.vseps(
              [case_delim, indent(scrut_doc(~wrap=true)), ...rules_docs]
              @ [end_delim],
            ),
          ]);
        | Some(ann) =>
          let end_delim = Doc.Text("end :");
          let ann_doc =
            doc_of_typ(~steps=steps @ [1 + List.length(rules)], ann);
          Doc.choices([
            Doc.vseps(
              [
                Doc.hseps([case_delim, scrut_doc(~wrap=false)]),
                ...rules_docs,
              ]
              @ [Doc.hseps([end_delim, ann_doc(~wrap=false)])],
            ),
            Doc.vseps(
              [case_delim, indent(scrut_doc(~wrap=true)), ...rules_docs]
              @ [Doc.hseps([end_delim, ann_doc(~wrap=false)])],
            ),
            Doc.vseps(
              [
                Doc.hseps([case_delim, scrut_doc(~wrap=false)]),
                ...rules_docs,
              ]
              @ [end_delim, indent(ann_doc(~wrap=true))],
            ),
            Doc.vseps(
              [case_delim, indent(scrut_doc(~wrap=true)), ...rules_docs]
              @ [end_delim, indent(ann_doc(~wrap=true))],
            ),
          ]);
        };
      } else {
        Fail;
      }
    | OpSeq(_, _) => Doc.Text("of_exp/OpSeq")
    | ApPalette(_, _, _, _) => failwith("unimplemented: of_exp ApPalette")
    };
  Tagged({steps, node_shape: Exp(e)}, doc);
}
and doc_of_rule = (~steps as _: CursorPath.steps, _e: UHExp.rule) =>
  Doc.Text("rule");
