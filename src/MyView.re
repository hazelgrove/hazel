open Incr_dom;
module Vdom = Virtual_dom.Vdom;

type id = string;
type cls = string;

type sbox_shape =
  | Block
  | EmptyLine
  | LetLine
  | EmptyHole
  | Var
  | Wild
  | NumLit
  | BoolLit
  | ListNil
  | Lam
  | Inj
  | Case
  | Rule
  | Parenthesized
  | Unit
  | Num
  | Bool;

type snode =
  | Seq(sseq)
  | Box(sbox)
and sseq = {
  steps: Path.steps,
  cursor: option(cursor_pos),
  is_multi_line: bool,
  head: snode,
  tail: list((stoken, snode)),
  skel: sskel,
}
and sskel =
  | Placeholder(int)
  | BinOp(err_status, string, sskel, sskel)
  | Space(err_status, sskel, sskel)
and sbox = {
  steps: Path.steps,
  cursor: option(cursor_pos),
  shape: sbox_shape,
  err_status,
  var_err_status,
  is_multi_line: bool,
  slines: list(sline),
}
and sline = {
  line_no: int,
  swords: list(sword),
}
and sword =
  | Node(snode)
  | Token(stoken)
and stoken =
  | Delim(delim_index, string)
  | Op(op_index, string)
  | Text(string);

let mk_sbox =
    (
      steps: Path.steps,
      ~cursor=None,
      shape: sbox_shape,
      ~err_status=NotInHole,
      ~var_err_status=NotInVHole,
      ~is_multi_line=false,
      swords_by_line: list(list(sword)),
    ) => {
  {
    steps,
    cursor,
    shape,
    err_status,
    var_err_status,
    is_multi_line,
    slines:
      swords_by_line
      |> List.mapi((line_no, swords) =>
           {parent_shape: shape, line_no, swords}
         ),
  };
};

/* TODO */
let id_of_sbox = (_: sbox): id => "";
let id_of_delim = (_: Path.t): id => "";
let id_of_text = (_: Path.steps): id => "";

let clss_of_sbox_shape = (_shape: sbox_shape): list(cls) => [""];

let clss_of_sbox = (sbox: sbox): list(cls) => {
  let shape_clss = [
    switch (sbox.shape) {
    | Block => "Block"
    | EmptyLine => "EmptyLine"
    | LetLine => "LetLine"
    | EmptyHole => "EmptyHole"
    | Var => "Var"
    | Wild => "Wild"
    | NumLit => "NumLit"
    | BoolLit => "BoolLit"
    | ListNil => "ListNil"
    | Lam => "Lam"
    | Inj => "Inj"
    | Case => "Case"
    | Rule => "Rule"
    | Parenthesized => "Parenthesized"
    | Unit => "Unit"
    | Num => "Num"
    | Bool => "Bool"
    },
  ];
  let err_status_clss =
    switch (sbox.err_status) {
    | NotInHole => []
    | InHole(_, u) => ["in_err_hole", "in_err_hole_" ++ string_of_int(u)]
    };
  let var_err_status_clss =
    switch (sbox.var_err_status) {
    | NotInVHole => []
    | InVHole(Free, u) => ["InVHole", "InVHole_" ++ string_of_int(u)]
    | InVHole(Keyword(_), u) => [
        "InVHole",
        "InVHole_" ++ string_of_int(u),
        "Keyword",
      ]
    };
  let multi_line_clss = sbox.is_multi_line ? ["multi-line"] : [];
  shape_clss @ err_status_clss @ var_err_status_clss @ multi_line_clss;
};
let clss_of_sline = (_: sline): list(cls) => [""];
let clss_of_stoken = (_: stoken): list(cls) => [""];

/*
 let delim_txt_click_handler = evt => {
   switch (Js.Opt.to_option(evt##.target)) {
   | None => JSUtil.move_caret_to(delim_before_elem)
   | Some(target) =>
     let x = evt##.clientX;
     x * 2 <= target##.offsetWidth
       ? JSUtil.move_caret_to(delim_before_elem)
       : JSUtil.move_caret_to(delim_after_elem)
   };
   false;
 }
 */

type is_start_of_top = bool;
type is_end_of_bottom = bool;
type sline_border_style =
  | NoBorder
  | Top(is_start_of_top)
  | Bottom(is_end_of_bottom)
  | TopBottom(is_start_of_top, is_end_of_bottom);

/* TODO need to thread node cursor down to delimiters */
let rec of_snode = (~inject: Update.Action.t => Vdom.Event.t, snode) =>
  switch (snode) {
  | Seq(sseq) => of_sseq(~inject, sseq)
  | Box(sbox) => of_sbox(~inject, sbox)
  }
and of_sseq = (~inject: Update.Action.t => Vdom.Event.t, sseq) => {
  let (vhead, vtail) =
    switch (sseq.cursor) {
    | None =>
      let vhead = of_sline(~inject, OpSeq, sseq.steps, { line_no: 0, [Node(sseq.head)]})
      let vtail =
      sseq.tail
      |> List.mapi((i, (sop, stm)) => of_sline(~inject, sseq.steps, { line_no: i+1, swords: [Token(sop), Node(stm)]}));
      (vhead, vtail);
    | Some(cursor) =>
      let (a, b) = range_of_tree_rooted_at_cursor(cursor, sseq.sskel);
      let vhead_border_style = a == 0 ? TopBottom(true, false) : NoBorder;
      let vhead = of_sline(~inject, ~border=vhead_border_style, sseq.steps, { line_no: 0, [Node(sseq.head)]});
      let vtail =
        sseq.tail
        |> List.mapi((i, (sop, stm)) => {
          let border_style = (i + 1 < b) ? TopBottom(false, false) : TopBottom(false, true);
          of_sline(~inject, ~border=border_style, sseq.steps, { line_no: i+1, swords: [Token(sop), Node(stm)]});
        });
      (vhead, vtail);
    };
  open Vdom;
  Node.div(
    [
      Attr.id(id_of_sseq(sseq)),
      Attr.classes(clss_of_sseq(sseq)),
    ],
    [vhead, ...vtail],
  );
}
and of_sbox = (~inject: Update.Action.t => Vdom.Event.t, sbox) => {
  let vlines =
    sbox.slines |> List.map(of_sline(~inject, sbox.steps, sbox.cursor));
  open Vdom;
  Node.div(
    [
      Attr.id(id_of_sbox(sbox)),
      Attr.classes(clss_of_sbox(sbox)),
    ],
    vlines,
  );
}
and of_sline =
    (~inject: Update.Action.t => unit, node_shape: node_shape, node_steps: Path.steps, sline: sline) => {
  let vwords =
    sline.swords
    |> List.map(sword =>
         switch (sword) {
         | Node(sbox) => of_sbox(do_action, sbox)
         | Token(stoken) => of_stoken(~inject, node_steps, node_cursor, stoken)
         }
       );
  Vdom.(Node.div([Attr.classes(clss_of_sline(sline))], vwords));
}
and of_stoken =
    (~inject: Update.Action.t => Vdom.Event.t, node_steps: Path.steps, stoken: stoken) => {
  open Vdom;
  let clss = clss_of_stoken(stoken);
  switch (stoken) {
  | Delim(k, s) =>
    let delim_before =
      Node.div(
        [
          Attr.id(id_of_delim((node_steps, (k, Before)))),
          Attr.classes(["delim-before"]),
        ],
        [],
      );
    let delim_after =
      Node.div(
        [
          Attr.id(id_of_delim((node_steps, (k, After)))),
          Attr.classes(["delim-after"]),
        ],
        [],
      );
    let delim_txt =
      Node.div(
        [
          Attr.create("contenteditable", "false"),
          Attr.classes(["delim-txt"]),
        ],
        [Node.text(s)],
      );
    Node.div(
      [Attr.classes(clss)],
      [delim_before, delim_txt, delim_after],
    );
  | Text(s) =>
    Node.div(
      [Attr.id(id_of_text(node_steps)), Attr.classes(clss)],
      [Node.text(s)],
    )
  };
};

/* TODO */
let is_multi_line_block = (_: UHExp.block) => false;
let is_multi_line_exp = (_: UHExp.t) => false;

/* TODO */
let snode_of_typ = (steps: Path.steps, _uty: UHTyp.t): snode =>
  mk_snode(steps, EmptyHole, [[]]);
let snode_of_pat = (steps: Path.steps, _p: UHPat.t): snode =>
  mk_snode(steps, EmptyHole, [[]]);

let rec snode_of_block =
        (steps: Path.steps, Block(line_items, e): UHExp.block): snode => {
  let snode_line_items =
    line_items |> List.mapi((i, li) => snode_of_line_item(steps @ [i], li));
  let snode_e = snode_of_exp(steps @ [List.length(line_items)], e);
  let swords_by_line =
    snode_line_items @ [snode_e] |> List.map(snode => [Node(snode)]);
  let is_multi_line = List.length(line_items) != 0;
  mk_snode(steps, Block, ~is_multi_line, swords_by_line);
}
and snode_of_line_item = (steps: Path.steps, li: UHExp.line): snode =>
  switch (li) {
  | EmptyLine => mk_snode(steps, EmptyLine, [[]])
  | ExpLine(e) => snode_of_exp(steps, e) /* ghost node */
  | LetLine(p, ann, def) =>
    let snode_p = snode_of_pat(steps @ [0], p);
    let swords_ann =
      switch (ann) {
      | None => []
      | Some(uty) => [
          Token(Delim(1, ":")),
          Node(snode_of_typ(steps @ [1], uty)),
        ]
      };
    let snode_def = snode_of_block(steps @ [2], def);
    let is_multi_line = is_multi_line_block(def);
    mk_snode(
      steps,
      LetLine,
      ~is_multi_line,
      [
        [Token(Delim(0, "let")), Node(snode_p)]
        @ swords_ann
        @ [Token(Delim(2, "="))],
        [Node(snode_def)],
      ],
    );
  }
and snode_of_exp = (steps: Path.steps, e: UHExp.t): snode =>
  switch (e) {
  /* outer nodes */
  | EmptyHole(u) =>
    mk_snode(steps, EmptyHole, [[Token(Delim(0, string_of_int(u + 1)))]])
  | Var(err_status, var_err_status, x) =>
    mk_snode(steps, Var, ~err_status, ~var_err_status, [[Token(Text(x))]])
  | NumLit(err_status, n) =>
    mk_snode(
      steps,
      NumLit,
      ~err_status,
      [[Token(Text(string_of_int(n)))]],
    )
  | BoolLit(err_status, b) =>
    mk_snode(
      steps,
      BoolLit,
      ~err_status,
      [[Token(Text(string_of_bool(b)))]],
    )
  | ListNil(err_status) =>
    mk_snode(steps, ListNil, ~err_status, [[Token(Delim(0, "[]"))]])
  /* inner nodes */
  | Lam(err_status, arg, ann, body) =>
    let snode_arg = snode_of_pat(steps @ [0], arg);
    let swords_ann =
      switch (ann) {
      | None => []
      | Some(uty) => [
          Token(Delim(1, ":")),
          Node(snode_of_typ(steps @ [1], uty)),
        ]
      };
    let snode_body = snode_of_block(steps @ [2], body);
    let is_multi_line = is_multi_line_block(body);
    mk_snode(
      steps,
      Lam,
      ~err_status,
      ~is_multi_line,
      [
        [Token(Delim(0, LangUtil.lamSym)), Node(snode_arg)]
        @ swords_ann
        @ [Token(Delim(2, "."))],
        [Node(snode_body)],
      ],
    );
  | Inj(err_status, side, body) =>
    let snode_body = snode_of_block(steps @ [0], body);
    let is_multi_line = is_multi_line_block(body);
    mk_snode(
      steps,
      Inj,
      ~err_status,
      ~is_multi_line,
      [
        [Token(Delim(0, "inj[" ++ LangUtil.string_of_side(side) ++ "]("))],
        [Node(snode_body)],
        [Token(Delim(1, ")"))],
      ],
    );
  | Case(err_status, scrut, rules, ann) =>
    let snode_scrut = snode_of_block(steps @ [0], scrut);
    let slines_rules =
      rules
      |> List.mapi((i, rule) => snode_of_rule(steps @ [i], rule))
      |> List.map(snode => [Node(snode)]);
    let swords_end =
      switch (ann) {
      | None => [Token(Delim(1, "end"))]
      | Some(uty) => [
          Token(Delim(1, "end :")),
          Node(snode_of_typ(steps @ [List.length(rules) + 1], uty)),
        ]
      };
    mk_snode(
      steps,
      Case,
      ~err_status,
      ~is_multi_line=true,
      [[Token(Delim(0, "case")), Node(snode_scrut)]]
      @ slines_rules
      @ [swords_end],
    );
  | Parenthesized(body) =>
    let snode_body = snode_of_block(steps @ [0], body);
    let is_multi_line = is_multi_line_block(body);
    mk_snode(
      steps,
      Parenthesized,
      ~is_multi_line,
      [
        [Token(Delim(0, "("))],
        [Node(snode_body)],
        [Token(Delim(1, ")"))],
      ],
    );
  | OpSeq(_skel, _seq) =>
    /* TODO */
    mk_snode(steps, OpSeq, [[]])
  | ApPalette(_, _, _, _) => raise(InvariantViolated)
  }
/* TODO */
and snode_of_rule = (steps: Path.steps, Rule(_pat, _clause): UHExp.rule) =>
  mk_snode(steps, Rule, [[]]);




let instance_click_fn = ((u, _) as inst) => {
  let usi = React.S.value(user_selected_instances_rs);
  user_selected_instances_rf(UserSelectedInstances.update(usi, inst));
  move_to_hole(u);
  selected_instance_rf(Some(inst));
};
let result_view = ({ result: (_, _, result) }: Model.t) => {
  open Vdom;
  switch (result) {
  | InvalidInput(_) =>
    Node.div(
      [],
      Node.text("(internal error: expansion or evaluation invariant violated)"),
    ),
  | BoxedValue(d)
  | Indet(d) =>
    Node.div(
      [],
      [view_of_dhexp(instance_click_fn)]
    )
  };
};

let examples_select = (~inject: Action.t => Vdom.Event.t) =>
  Vdom.(
    Node.select(
      [Attr.on_change(ev => inject(Action.LoadExample(ev##.target##.value))],
      [
        Node.option([Attr.value("just_hole")], Node.text("just a hole")),
        Node.option([Attr.value("holey_lambda")], Node.text("holey lambda")),
        Node.option([Attr.value("let_line")], Node.text("let with extra lines")),
        Node.option([Attr.value("map_example")], Node.text("map")),
        Node.option([Attr.value("qsort_example")], Node.text("qsort")),
      ],
    )
  );

let page_view = (model: Model.t, ~inject: Action.t => Vdom.Event.t) => {
  open Vdom;
  Node.div(
    [Attr.id(["root"])],
    [
      Node.div(
        [Attr.classes(["top-bar"])],
        [
          Node.a(
            [Attr.classes(["logo-text"]), Attr.href("https://hazel.org")],
            [Node.text("Hazel")],
          )
        ],
      ),
      Node.div(
        [Attr.classes(["main-area"])],
        [
          Sidebar.left([the_action_panel /*, the_history_panel*/]),
          Node.div(
            [Attr.classes(["flex-wrapper"])],
            [
              Node.div(
                [Attr.classes(["page-area"])],
                [
                  Node.div(
                    [Attr.classes(["page"])],
                    [
                      Node.div([
                        Node.text("Hazel is an experiment in "),
                        Node.strong([Node.text("live functional programming")]),
                        Node.text(" with "),
                        Node.strong([Node.text("typed holes")]),
                        Node.text(
                          ". Use the actions on the left to construct an expression. Navigate using the text cursor in the usual way.",
                        ),
                      ]),
                      /* TODO add pp_view_parent */
                      div(
                        [a_class(["cell-status"])],
                        [
                          div(
                            [a_class(["type-indicator"])],
                            [
                              div(
                                [a_class(["type-label"])],
                                [txt("Result of type: ")],
                              ),
                              div(
                                [a_class(["htype-view"])],
                                [htype_view],
                              ),
                            ],
                          ),
                        ],
                      ),
                      div(
                        [a_class(["result-view"])],
                        [result_view],
                      ),
                    ]
                  ),
                  examples_select,
                ]
              )
            ]
          ),
          Sidebar.right([
            the_cursor_inspector_panel,
            the_context_inspector_panel,
          ]),
        ]
      )
    ]
  )
};

let of = (model: Model.t, ~inject: Action.t => Vdom.Event.t) => {

};
