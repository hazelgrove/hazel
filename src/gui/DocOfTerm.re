type node_shape =
  | VarExp(VarErrStatus.t)
  | NonVarExp(ErrStatus.t)
  | Block
  | EmptyLine
  | LetLine;

type tag = {
  steps: Path.steps,
  node_shape,
};

type doc = Doc.t(tag);

let space = String(" ");
let hspace = (doc1, doc2) => Doc.hcats([doc1, space, doc2]);
let hspaces = docs => Doc.hcats(GeneralUtil.join(space, docs));

let tab_length = 2;
let indent = (~n=1, doc: doc): doc =>
  n === 0 ? doc : hcats([String(String.make(n * tab_length, " ")), doc]);

/**
 * Alternating list of delimiters and children in
 * the sequential representation of an AST node.
 * Other than opseqs, all current AST nodes start
 * with a delimiter.
 */
type tokens =
  | DelimStart(delim_start)
and delim_start =
  | Delim(doc)
  | DCons(doc, child_start)
and child_start =
  | Child(doc)
  | CCons(doc, delim_start);

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
        |> List.map(choice => hspaces([delim_doc, choice]));
      let delim_multi_line_choices =
        multi_line_choices
        |> List.map(choice => Doc.vcats([delim_doc, choice]));
      delim_single_line_choices @ delim_multi_line_choices;
    }
// first of return pair is set of choices if starting child is single line,
// second of return pair is set of choices if starting child is multi line
and choices_of_child_start: child_start => (list(doc), list(doc)) =
  fun
  | Child(child_doc) => ([SingleLine(child_doc)], [child_doc])
  | CCons(child_doc, delim_start) => {
      let choices = choices_of_delim_start(delim_start);
      let single_line_choices =
        choices |> List.map(choice => hspaces([child_doc, choices]));
      let multi_line_choices =
        choices
        |> List.map(choice => Doc.vcats([indent(child_doc), choices]));
      (single_line_choices, multi_line_choices);
    };

let rec doc_of_block = (~steps: Path.steps) =>
  fun
  | Block(leading, conclusion) => {
      let leading_docs =
        leading
        |> List.mapi((i, line) => doc_of_line(~steps=steps @ [i], line));
      let conclusion_doc =
        doc_of_exp(~steps=steps @ [List.length(leading)], conclusion);
      let docs = Docs.vcats(leading_docs @ [conclusion_doc]);
      Tagged({steps, node_shape: Block}, docs);
    }
and doc_of_line = (~steps: Path.steps) =>
  fun
  | ExpLine(e) => doc_of_exp(~steps, e)
  | EmptyLine => Tagged({steps, node_shape: EmptyLine}, String(""))
  | LetLine(p, ann, def) => {
      let let_delim = String("let");
      let p_doc = doc_of_pat(~steps=steps @ [0], p);
      let eq_delim = String("=");
      let def_doc = doc_of_block(~steps=steps @ [2], def);
      let in_delim = String("in");
      let tokens =
        switch (ann) {
        | None =>
          DelimStart(
            DCons(
              let_delim,
              CCons(
                p_doc,
                DCons(eq_delim, CCons(def_doc, DEnd(in_delim))),
              ),
            ),
          )
        | Some(ann) =>
          let colon_delim = String(":");
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
                    DCons(eq_delim, CCons(def_doc, DEnd(in_delim))),
                  ),
                ),
              ),
            ),
          );
        };
      Doc.choices(choices_of_tokens(tokens));
    }
and doc_of_exp = e =>
  switch (e) {
  | _ => assert(false)
  };
