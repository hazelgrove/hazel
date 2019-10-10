type node_shape =
  | Block(UHExp.block)
  | Line(UHExp.line)
  | Exp(UHExp.t)
  | Rule(UHExp.rule)
  | Pat(UHPat.t)
  | Typ(UHTyp.t);

type tag = {
  steps: CursorPath.steps,
  node_shape,
};

type doc = Doc.t(tag);

let tab_length = 2;
let indent = (~n=1, doc: doc): doc =>
  n === 0
    ? doc : Doc.hcats([Doc.Text(String.make(n * tab_length, ' ')), doc]);

/**
 * Alternating list of delimiters and children in
 * the sequential representation of an AST node.
 * Other than opseqs (which we handle separately),
 * all current AST nodes start with a delimiter.
 */
type tokens =
  | DelimStart(delim_start)
and delim_start =
  | Delim(doc)
  | DCons(doc, child_start)
and child_start =
  | Child(doc)
  | CCons(doc, delim_start);

/**
 * Given a tokens representation of an AST node,
 * generates the combinatorial enumeration of doc
 * choices caused by variation in layout depending
 * on whether each child node is a single line.
 */;
let rec choices_of_tokens: tokens => list(doc) =
  fun
  | DelimStart(delim_start) => choices_of_delim_start(delim_start)
and choices_of_delim_start: delim_start => list(doc) =
  fun
  | Delim(delim_doc) => [delim_doc]
  | DCons(delim_doc, child_start) => {
      let (single_line_choices, multi_line_choices) =
        choices_of_child_start(child_start);
      let delim_single_line_choices =
        single_line_choices
        |> List.map(choice => Doc.hspaces([delim_doc, choice]));
      let delim_multi_line_choices =
        multi_line_choices
        |> List.map(choice => Doc.vcats([delim_doc, choice]));
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
        choices |> List.map(_choice => Doc.hspaces([child_doc, ...choices]));
      let multi_line_choices =
        choices
        |> List.map(_choice => Doc.vcats([indent(child_doc), ...choices]));
      (single_line_choices, multi_line_choices);
    };

let rec doc_of_block = (~steps: CursorPath.steps, block: UHExp.block) => {
  let Block(leading, conclusion) = block;
  let leading_docs =
    leading |> List.mapi((i, line) => doc_of_line(~steps=steps @ [i], line));
  let conclusion_doc =
    doc_of_exp(~steps=steps @ [List.length(leading)], conclusion);
  let docs = Doc.vcats(leading_docs @ [conclusion_doc]);
  Doc.Tagged({steps, node_shape: Block(block)}, docs);
}
and doc_of_line = (~steps: CursorPath.steps, line: UHExp.line) => {
  let tag = {steps, node_shape: Line(line)};
  switch (line) {
  | ExpLine(e) => doc_of_exp(~steps, e)
  | EmptyLine => Doc.Tagged(tag, Doc.Text(""))
  | LetLine(p, ann, def) =>
    let let_delim = Doc.Text("let");
    let p_doc = doc_of_pat(~steps=steps @ [0], p);
    let eq_delim = Doc.Text("=");
    let def_doc = doc_of_block(~steps=steps @ [2], def);
    let in_delim = Doc.Text("in");
    let tokens =
      switch (ann) {
      | None =>
        DelimStart(
          DCons(
            let_delim,
            CCons(p_doc, DCons(eq_delim, CCons(def_doc, Delim(in_delim)))),
          ),
        )
      | Some(ann) =>
        let colon_delim = Doc.Text(":");
        let ann_doc = doc_of_typ(~steps=steps @ [1], ann);
        DelimStart(
          DCons(
            let_delim,
            CCons(
              p_doc,
              DCons(
                colon_delim,
                CCons(
                  ann_doc,
                  DCons(eq_delim, CCons(def_doc, Delim(in_delim))),
                ),
              ),
            ),
          ),
        );
      };
    Tagged(tag, Doc.choices(choices_of_tokens(tokens)));
  };
}
and doc_of_exp = (~steps: CursorPath.steps, e: UHExp.t) => {
  let doc =
    switch (e) {
    | EmptyHole(_) => Doc.Empty
    | Var(_, _, x) => Doc.Text(x)
    | NumLit(_, n) => Doc.Text(string_of_int(n))
    | BoolLit(_, b) => Doc.Text(string_of_bool(b))
    | ListNil(_) => Doc.Text("[]")
    | Lam(_, p, ann, body) =>
      let lam_delim = Doc.Text(LangUtil.lamSym);
      let p_doc = doc_of_pat(~steps=steps @ [0], p);
      let dot_delim = Doc.Text(".");
      let body_doc = doc_of_block(~steps=steps @ [2], body);
      let tokens =
        switch (ann) {
        | None =>
          DelimStart(
            DCons(
              lam_delim,
              CCons(p_doc, DCons(dot_delim, Child(body_doc))),
            ),
          )
        | Some(ann) =>
          let colon_delim = Doc.Text(":");
          let ann_doc = doc_of_typ(~steps=steps @ [1], ann);
          DelimStart(
            DCons(
              lam_delim,
              CCons(
                p_doc,
                DCons(
                  colon_delim,
                  CCons(ann_doc, DCons(dot_delim, Child(body_doc))),
                ),
              ),
            ),
          );
        };
      Doc.choices(choices_of_tokens(tokens));
    | Inj(_, side, body) =>
      let open_delim = Doc.Text("inj[" ++ (side == L ? "L" : "R") ++ "](");
      let body_doc = doc_of_block(~steps=steps @ [0], body);
      let close_delim = Doc.Text(")");
      let tokens =
        DelimStart(DCons(open_delim, CCons(body_doc, Delim(close_delim))));
      Doc.choices(choices_of_tokens(tokens));
    | Parenthesized(body) =>
      let open_delim = Doc.Text("(");
      let body_doc = doc_of_block(~steps=steps @ [0], body);
      let close_delim = Doc.Text(")");
      let tokens =
        DelimStart(DCons(open_delim, CCons(body_doc, Delim(close_delim))));
      Doc.choices(choices_of_tokens(tokens));
    | Case(_, scrut, rules, ann) =>
      let case_delim = Doc.Text("case");
      let scrut_doc = doc_of_block(~steps=steps @ [0], scrut);
      let rules_docs =
        rules
        |> List.mapi((i, rule) => doc_of_rule(~steps=steps @ [i + 1], rule));
      let ann_doc = failwith("unimplemented: ann_doc");
      switch (ann) {
      | None =>
        let end_delim = Doc.Text("end");
        Doc.choices([
          Doc.vcats(
            [Doc.hspaces([case_delim, scrut_doc]), ...rules_docs]
            @ [end_delim],
          ),
          Doc.vcats(
            [case_delim, indent(scrut_doc), ...rules_docs] @ [end_delim],
          ),
        ]);
      | Some(_ann) =>
        let end_delim = Doc.Text("end :");
        Doc.choices([
          Doc.vcats(
            [Doc.hspaces([case_delim, scrut_doc]), ...rules_docs]
            @ [Doc.hspaces([end_delim, ann_doc])],
          ),
          Doc.vcats(
            [case_delim, indent(scrut_doc), ...rules_docs]
            @ [Doc.hspaces([end_delim, ann_doc])],
          ),
          Doc.vcats(
            [Doc.hspaces([case_delim, scrut_doc]), ...rules_docs]
            @ [end_delim, indent(ann_doc)],
          ),
          Doc.vcats(
            [case_delim, indent(scrut_doc), ...rules_docs]
            @ [end_delim, indent(ann_doc)],
          ),
        ]);
      };
    | _ => assert(false)
    };
  Tagged({steps, node_shape: Exp(e)}, doc);
}
and doc_of_typ = (~steps as _: CursorPath.steps, _e: UHTyp.t) => {
  failwith("unimplemented: doc_of_typ");
}
and doc_of_pat = (~steps as _: CursorPath.steps, _e: UHPat.t) => {
  failwith("unimplemented: doc_of_pat");
}
and doc_of_rule = (~steps as _: CursorPath.steps, _e: UHExp.rule) => {
  failwith("unimplemented: doc_of_rule");
};
