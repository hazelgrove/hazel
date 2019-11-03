module Js = Js_of_ocaml.Js;
module Vdom = Virtual_dom.Vdom;
open GeneralUtil;
open ViewUtil;

let contenteditable_of_layout: Layout.t('tag) => Vdom.Node.t =
  layout => {
    let record: Layout.text('tag, list(Vdom.Node.t), Vdom.Node.t) = {
      imp_of_string: string => [Vdom.Node.text(string)],
      imp_of_tag: (_, string) => [Vdom.Node.span([], string)], // TODO: add span data
      imp_append: (s1, s2) => s1 @ s2,
      imp_newline: [Vdom.Node.br([])],
      t_of_imp: s => Vdom.Node.span([], s) // TODO: use something other than `span`?
    };
    Layout.make_of_layout(record, layout);
  };

type snode =
  | SNode(DocOfTerm.node_tag, list(sline))
and sline =
  | SLine(int, list(sword))
and sword =
  | SChild(snode)
  | SDelim(DocOfTerm.delim_tag)
  | SPadding(DocOfTerm.padding_tag)
  | SText(string);

let is_whitespace = s => Re.Str.string_match(Re.Str.regexp("\\s*"), s, 0);

let slines_of_layout = (l: Layout.t(DocOfTerm.tag)): list(sline) => {
  // make proper slines from swords grouped by line,
  // trimming all leading whitespace in each sword-line
  // and converting to indentation parameter of sline
  let mk_slines: list(list(sword)) => list(sline) =
    List.map(swords => {
      let (indentation, reversed_swords) =
        swords
        |> List.fold_left(
             ((indentation, reversed_swords), sword) =>
               switch (reversed_swords, sword) {
               | ([], SText(txt)) when txt |> is_whitespace => (
                   indentation + String.length(txt),
                   reversed_swords,
                 )
               | (_, _) => (indentation, [sword, ...reversed_swords])
               },
             (0, []),
           );
      SLine(indentation, reversed_swords |> List.rev);
    });
  // convert layout elements into swords grouped by line
  let rec collect_swords_by_line:
    Layout.t(DocOfTerm.tag) => list(list(sword)) =
    fun
    | Text(s) => [[SText(s)]]
    | Cat(l1, l2) => {
        let swords1 = l1 |> collect_swords_by_line;
        let swords2 = l2 |> collect_swords_by_line;
        switch (swords1 |> split_last, swords2) {
        | (None, _) => swords2
        | (_, []) => swords1
        | (
            Some((swords1_prefix, swords1_last)),
            [swords2_first, ...swords2_suffix],
          ) =>
          swords1_prefix @ [swords1_last @ swords2_first, ...swords2_suffix]
        };
      }
    | Linebreak => [[], []]
    | Align(l) => l |> collect_swords_by_line
    | Tagged(Delim(delim_tag), _) => [[SDelim(delim_tag)]]
    | Tagged(Padding(padding_tag), _) => [[SPadding(padding_tag)]]
    | Tagged(Node(node_tag), l) => [
        [SChild(SNode(node_tag, l |> collect_swords_by_line |> mk_slines))],
      ];
  l |> collect_swords_by_line |> mk_slines;
};

let err_clss: ErrStatus.t => list(cls) =
  fun
  | NotInHole => []
  | InHole(_, _) => ["InHole"];

let var_err_clss: VarErrStatus.t => list(cls) =
  fun
  | NotInVarHole => []
  | InVarHole(_, _) => ["InVarHole"];

let node_shape_clss: DocOfTerm.node_shape => list(cls) =
  fun
  | Block(_) => ["Block"]
  | Line(ExpLine(_)) => failwith("ghost node")
  | Line(EmptyLine) => ["EmptyLine"]
  | Line(LetLine(_, _, _)) => ["LetLine"]
  | Exp(e) => {
      let variant_clss =
        switch (e) {
        | EmptyHole(_) => ["EmptyHole"]
        | Var(_, var_err, _) => ["Var", ...var_err_clss(var_err)]
        | NumLit(_, _) => ["NumLit"]
        | BoolLit(_, _) => ["BoolLit"]
        | ListNil(_) => ["ListNil"]
        | Lam(_, _, _, _) => ["Lam"]
        | Inj(_, _, _) => ["Inj"]
        | Case(_, _, _, _) => ["Case"]
        | ApPalette(_, _, _, _) => ["ApPalette"]
        | Parenthesized(_) => ["Parenthesized"]
        | OpSeq(_, _) => failwith("unimplemented: node_shape_clss/OpSeq")
        };
      let err_clss = e |> UHExp.get_err_status_t |> err_clss;
      err_clss @ variant_clss;
    }
  | Rule(_) => ["Rule"]
  | Pat(p) => {
      let variant_clss =
        switch (p) {
        | EmptyHole(_) => ["EmptyHole"]
        | Var(_, var_err, _) => ["Var", ...var_err_clss(var_err)]
        | Wild(_) => ["Wild"]
        | NumLit(_, _) => ["NumLit"]
        | BoolLit(_, _) => ["BoolLit"]
        | ListNil(_) => ["ListNil"]
        | Inj(_, _, _) => ["Inj"]
        | Parenthesized(_) => ["Parenthesized"]
        | OpSeq(_, _) => failwith("unimplemented: node_shape_clss/OpSeq")
        };
      let err_clss = p |> UHPat.get_err_status_t |> err_clss;
      err_clss @ variant_clss;
    }
  | Typ(Hole) => ["Hole"]
  | Typ(Unit) => ["Unit"]
  | Typ(Num) => ["Num"]
  | Typ(Bool) => ["Bool"]
  | Typ(Parenthesized(_)) => ["Parenthesized"]
  | Typ(List(_)) => ["List"]
  | Typ(OpSeq(_, _)) => failwith("unimplemented: node_shape_clss/OpSeq");

let node_attrs = ({steps, shape}: DocOfTerm.node_tag): list(Vdom.Attr.t) =>
  Vdom.[Attr.id(node_id(steps)), Attr.classes(node_shape_clss(shape))];

let view = (~inject: Update.Action.t => Vdom.Event.t): Vdom.Node.t => {
  let on_click_noneditable =
      (path_before: CursorPath.t, path_after: CursorPath.t, evt) =>
    switch (Js.Opt.to_option(evt##.target)) {
    | None => inject(Update.Action.EditAction(MoveTo(path_before)))
    | Some(target) =>
      let from_left =
        float_of_int(evt##.clientX) -. target##getBoundingClientRect##.left;
      let from_right =
        target##getBoundingClientRect##.right -. float_of_int(evt##.clientX);
      inject(
        Update.Action.EditAction(
          MoveTo(from_left <= from_right ? path_before : path_after),
        ),
      );
    };

  let rec view_of_snode: snode => Vdom.Node.t =
    fun
    | SNode(node_tag, slines) =>
      Vdom.Node.div(
        node_attrs(node_tag),
        slines |> List.map(view_of_sline(~node_steps=node_tag.steps)),
      )
  and view_of_sline =
      (~node_steps: CursorPath.steps, SLine(indentation, swords): sline)
      : Vdom.Node.t =>
    Vdom.(
      Node.div(
        [Attr.classes(["SLine"])],
        [
          Node.div(
            [Attr.classes(["indent"])],
            [Node.text(String.make(indentation, ' '))],
          ),
          ...swords |> List.map(view_of_sword(~node_steps)),
        ],
      )
    )
  and view_of_sword =
      (~node_steps: CursorPath.steps, sword: sword): Vdom.Node.t =>
    switch (sword) {
    | SChild(snode) => view_of_snode(snode)
    | SText(s) =>
      Vdom.(Node.span([Attr.classes(["SText"])], [Node.text(s)]))
    | SDelim(delim_tag) =>
      Vdom.(
        Node.span(
          [
            Attr.classes(["SDelim"]),
            Attr.on_click(
              on_click_noneditable(
                (node_steps, OnDelim(delim_tag.index, Before)),
                (node_steps, OnDelim(delim_tag.index, After)),
              ),
            ),
          ],
          [Node.text(delim_tag.text)],
        )
      )
    | SPadding(padding_tag) =>
      Vdom.(
        Node.span(
          [
            Attr.classes(["SPadding"]),
            Attr.on_click(
              on_click_noneditable(
                padding_tag.path_before,
                padding_tag.path_after,
              ),
            ),
          ],
          [Node.text(padding_tag.text)],
        )
      )
    };
  Vdom.Node.div([], []);
};
