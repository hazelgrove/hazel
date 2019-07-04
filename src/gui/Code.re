let _SHOW_CASTS = false;
let _SHOW_FN_BODIES = false;

module Js = Js_of_ocaml.Js;
module Dom = Js_of_ocaml.Dom;
module Dom_html = Js_of_ocaml.Dom_html;
module Vdom = Virtual_dom.Vdom;
open GeneralUtil;
open SemanticsCommon;
open ViewUtil;
open Sexplib.Std;
module Sexp = Sexplib.Sexp;

exception InvariantViolated;

[@deriving sexp]
type id = string;
[@deriving sexp]
type cls = string;
[@deriving sexp]
type is_multi_line = bool;

[@deriving sexp]
type is_start_of_top = bool;
[@deriving sexp]
type is_end_of_bottom = bool;
[@deriving sexp]
type border_style =
  | NoBorder
  | Top(is_start_of_top)
  | Bottom(is_end_of_bottom)
  | TopBottom(is_start_of_top, is_end_of_bottom);

/* TODO */
let border_clss: border_style => list(string) =
  fun
  | NoBorder => []
  | _ => [];

[@deriving sexp]
type relative_indent = int;
[@deriving sexp]
type absolute_indent = int;

[@deriving sexp]
type snode_shape =
  | Seq
  | Box(sbox_shape)
[@deriving sexp]
and sbox_shape =
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
  | Bool
  | List
  | Triv
  | EmptyHoleInstance(
      MetaVar.t,
      Dynamics.inst_num,
      option(VarMap.t_(Dynamics.DHExp.t)),
    )
  | NonEmptyHoleInstance(
      in_hole_reason,
      MetaVar.t,
      Dynamics.inst_num,
      option(VarMap.t_(Dynamics.DHExp.t)),
    )
  | SkelBinOp
  | Let
  | FixF
  | InjAnn
  | Cast
  | FailedCast;

[@deriving sexp]
type snode =
  | SSeq(
      Path.steps,
      option(cursor_position),
      is_multi_line,
      sskel,
      snode,
      list(sseq_tail_segment),
    )
  | SBox(
      border_style,
      Path.steps,
      option(cursor_position),
      is_multi_line,
      err_status,
      sbox_shape,
      list(sline),
    )
[@deriving sexp]
and sskel =
  | Placeholder(int)
  | BinOp(err_status, string, sskel, sskel)
  | Space(err_status, sskel, sskel)
[@deriving sexp]
and sseq_tail_segment =
  | SSegmentSpace(snode)
  | SSegmentOp(list(stoken), snode)
[@deriving sexp]
and sline = (relative_indent, option(Path.steps), list(sword))
[@deriving sexp]
and sword =
  | SNode(snode)
  | SToken(stoken)
[@deriving sexp]
and stoken =
  | SEmptyHole(string)
  | SDelim(option(delim_index), string)
  | SSpaceOp
  | SOp(option(op_index), string)
  | SText(var_err_status, string)
  | SCastArrow
  | SFailedCastArrow
  | SEmptyLine
  /* SSpace as in spacing, not fn ap */
  | SSpace;

let is_multi_line =
  fun
  | SSeq(_, _, is_multi_line, _, _, _)
  | SBox(_, _, _, is_multi_line, _, _, _) => is_multi_line;

let mk_SSeq =
    (
      ~cursor: option(cursor_position)=?,
      ~is_multi_line=false,
      ~steps: Path.steps,
      ~sskel: sskel,
      shead: snode,
      stail,
    )
    : snode =>
  SSeq(steps, cursor, is_multi_line, sskel, shead, stail);

let mk_SBox =
    (
      ~border_style=NoBorder,
      ~cursor: option(cursor_position)=?,
      ~is_multi_line=false,
      ~err_status=NotInHole,
      ~steps: Path.steps,
      ~shape: sbox_shape,
      slines: list(sline),
    )
    : snode => {
  SBox(border_style, steps, cursor, is_multi_line, err_status, shape, slines);
};

let mk_sline =
    (
      ~rel_indent=0,
      ~steps_of_first_sword: option(Path.steps)=?,
      swords: list(sword),
    )
    : sline => (
  rel_indent,
  steps_of_first_sword,
  swords,
);

let sline_of_segment = (~steps, segment: sseq_tail_segment): sline =>
  switch (segment) {
  | SSegmentSpace(snode) =>
    mk_sline(
      ~rel_indent=2,
      ~steps_of_first_sword=steps,
      [SToken(SSpaceOp), SNode(snode)],
    )
  | SSegmentOp(sop_tokens, snode) =>
    mk_sline(
      ~steps_of_first_sword=steps,
      (sop_tokens |> List.map(t => SToken(t))) @ [SNode(snode)],
    )
  };

let steps_of_snode =
  fun
  | SSeq(steps, _, _, _, _, _)
  | SBox(_, steps, _, _, _, _, _) => steps;

let update_border_style = (border_style, snode) =>
  switch (snode) {
  | SSeq(_, _, _, _, _, _) => snode
  | SBox(_, cursor, is_multi_line, err_status, steps, shape, slines) =>
    SBox(
      border_style,
      cursor,
      is_multi_line,
      err_status,
      steps,
      shape,
      slines,
    )
  };

let update_border_style_of_segment_node =
    (border_style, segment): sseq_tail_segment =>
  switch (segment) {
  | SSegmentSpace(snode) =>
    SSegmentSpace(snode |> update_border_style(border_style))
  | SSegmentOp(sop_tokens, snode) =>
    SSegmentOp(sop_tokens, snode |> update_border_style(border_style))
  };

let mk_SDelim = (~index=?, s: string): stoken => SDelim(index, s);

let mk_SOp = (~index=?, s: string): stoken => SOp(index, s);

let mk_SText = (~var_err_status=NotInVHole, s: string): stoken =>
  SText(var_err_status, s);

let string_of_op_typ: UHTyp.op => string =
  fun
  | Arrow => LangUtil.typeArrowSym
  | Sum => "|"
  | Prod => ",";

let string_of_op_pat: UHPat.op => string =
  fun
  | Comma => ","
  | Space => " "
  | Cons => "::";

let string_of_op_exp: UHExp.op => string =
  fun
  | Plus => "+"
  | Times => "*"
  | LessThan => "<"
  | Space => " "
  | Comma => ","
  | Cons => "::";

type space_before = bool;
type space_after = bool;

let space_before_after_op_typ: UHTyp.op => (space_before, space_after) =
  fun
  | Arrow => (true, true)
  | Sum => (true, true)
  | Prod => (false, true);

let space_before_after_op_pat: UHPat.op => (space_before, space_after) =
  fun
  | Comma => (false, true)
  | Space => (false, false)
  | Cons => (false, false);

let space_before_after_op_exp: UHExp.op => (space_before, space_after) =
  fun
  | Plus => (true, true)
  | Times => (true, true)
  | LessThan => (true, true)
  | Space => (false, false)
  | Comma => (false, true)
  | Cons => (false, false);

let rec sskel_of_skel_typ = (skel: UHTyp.skel_t): sskel =>
  switch (skel) {
  | Placeholder(n) => Placeholder(n)
  | BinOp(err_status, op, skel1, skel2) =>
    let sop = string_of_op_typ(op);
    let sskel1 = sskel_of_skel_typ(skel1);
    let sskel2 = sskel_of_skel_typ(skel2);
    BinOp(err_status, sop, sskel1, sskel2);
  };
let rec sskel_of_skel_pat = (skel: UHPat.skel_t): sskel =>
  switch (skel) {
  | Placeholder(n) => Placeholder(n)
  | BinOp(err_status, Space, skel1, skel2) =>
    let sskel1 = sskel_of_skel_pat(skel1);
    let sskel2 = sskel_of_skel_pat(skel2);
    Space(err_status, sskel1, sskel2);
  | BinOp(err_status, op, skel1, skel2) =>
    let sop = string_of_op_pat(op);
    let sskel1 = sskel_of_skel_pat(skel1);
    let sskel2 = sskel_of_skel_pat(skel2);
    BinOp(err_status, sop, sskel1, sskel2);
  };
let rec sskel_of_skel_exp = (skel: UHExp.skel_t): sskel =>
  switch (skel) {
  | Placeholder(n) => Placeholder(n)
  | BinOp(err_status, Space, skel1, skel2) =>
    let sskel1 = sskel_of_skel_exp(skel1);
    let sskel2 = sskel_of_skel_exp(skel2);
    Space(err_status, sskel1, sskel2);
  | BinOp(err_status, op, skel1, skel2) =>
    let sop = string_of_op_exp(op);
    let sskel1 = sskel_of_skel_exp(skel1);
    let sskel2 = sskel_of_skel_exp(skel2);
    BinOp(err_status, sop, sskel1, sskel2);
  };

let cursor_clss = (mb_cursor: option(cursor_position)) =>
  switch (mb_cursor) {
  | None => []
  | Some(_) => ["cursor"]
  };
let multi_line_clss = is_multi_line => is_multi_line ? ["multi-line"] : [];

let err_status_clss =
  fun
  | NotInHole => []
  | InHole(_, u) => ["in_err_hole", "in_err_hole_" ++ string_of_int(u)];

let inline_div_cls = "inline-div";

let rec child_indices_of_snode =
  fun
  | SSeq(_steps, _cursor, _is_multi_line, _sskel, _shead, stail) =>
    range(List.length(stail) + 1)
  | SBox(
      _border_style,
      _steps,
      _cursor,
      _is_multi_line,
      _err_status,
      _shape,
      slines,
    ) =>
    slines
    |> List.fold_left(
         (acc, sline) => acc @ child_indices_of_sline(sline),
         [],
       )
and child_indices_of_sline = ((_, _, swords)) =>
  swords
  |> List.fold_left(
       (acc, sword) =>
         switch (sword) {
         | SToken(_) => acc
         | SNode(snode) =>
           switch (snode |> steps_of_snode |> last) {
           | None => assert(false)
           | Some(k) => acc @ [k]
           }
         },
       [],
     );

[@deriving sexp]
type child_indices = list(int);

let child_indices_of_snode_elem = elem =>
  switch (elem |> JSUtil.get_attr("children")) {
  | None => None
  | Some(schildren) =>
    Some(child_indices_of_sexp(Sexplib.Sexp.of_string(schildren)))
  };

let child_elems_of_snode_elem = elem =>
  switch (
    steps_of_node_id(Js.to_string(elem##.id)),
    child_indices_of_snode_elem(elem),
  ) {
  | (None, _)
  | (_, None) => None
  | (Some(steps), Some(child_indices)) =>
    Some(
      child_indices
      |> List.map(i => steps @ [i])
      |> List.map(node_id)
      |> List.map(JSUtil.force_get_elem_by_id),
    )
  };

let snode_attrs =
    (~inject: Update.Action.t => Vdom.Event.t, snode: snode)
    : list(Vdom.Attr.t) => {
  Vdom.(
    switch (snode) {
    | SSeq(steps, cursor, is_multi_line, _sskel, _shead, _stail) => [
        /* TODO need to do something about err status holes with sskel */
        Attr.id(node_id(steps)),
        Attr.classes(
          ["OpSeq", inline_div_cls]
          @ cursor_clss(cursor)
          @ multi_line_clss(is_multi_line),
        ),
      ]
    | SBox(_, steps, cursor, is_multi_line, err_status, shape, _) =>
      let base_clss =
        ["SNode", inline_div_cls]
        @ cursor_clss(cursor)
        @ multi_line_clss(is_multi_line)
        @ err_status_clss(err_status);
      let shape_attrs =
        switch (shape) {
        | Block => [Attr.classes(["Block", ...base_clss])]
        | EmptyLine => [Attr.classes(["EmptyLine", ...base_clss])]
        | LetLine => [Attr.classes(["LetLine", ...base_clss])]
        | EmptyHole => [Attr.classes(["EmptyHole", ...base_clss])]
        | Var => [Attr.classes(["Var", ...base_clss])]
        | Wild => [Attr.classes(["Wild", ...base_clss])]
        | NumLit => [Attr.classes(["NumLit", ...base_clss])]
        | BoolLit => [Attr.classes(["BoolLit", ...base_clss])]
        | ListNil => [Attr.classes(["ListNil", ...base_clss])]
        | Lam => [Attr.classes(["Lam", ...base_clss])]
        | Inj => [Attr.classes(["Inj", ...base_clss])]
        | Case => [Attr.classes(["Case", ...base_clss])]
        | Rule => [Attr.classes(["Rule", ...base_clss])]
        | Parenthesized => [Attr.classes(["Parenthesized", ...base_clss])]
        | Unit => [Attr.classes(["Unit", ...base_clss])]
        | Num => [Attr.classes(["Num", ...base_clss])]
        | Bool => [Attr.classes(["Bool", ...base_clss])]
        | List => [Attr.classes(["List", ...base_clss])]
        | Triv => [Attr.classes(["Triv", ...base_clss])]
        | EmptyHoleInstance(u, i, _sigma) => [
            Attr.classes([
              "hole-instance",
              "hole-instance-" ++ string_of_int(u) ++ "-" ++ string_of_int(i),
              "selected-instance",
              ...base_clss,
            ]),
            Attr.on_click(_ =>
              Event.Many([
                inject(SelectHoleInstance(u, i)),
                inject(MoveToHole(u)),
              ])
            ),
          ]
        | NonEmptyHoleInstance(_reason, _u, _i, _sigma) => [
            Attr.classes(["NonEmptyHoleInstance", ...base_clss]),
          ]
        | SkelBinOp => [Attr.classes(["SkelBinOp", ...base_clss])]
        | Let => [Attr.classes(["Let", ...base_clss])]
        | FixF => [Attr.classes(["FixF", ...base_clss])]
        | InjAnn => [Attr.classes(["InjAnn", ...base_clss])]
        | Cast => [Attr.classes(["Cast", ...base_clss])]
        | FailedCast => [Attr.classes(["FailedCast", ...base_clss])]
        };
      [
        Attr.id(node_id(steps)),
        // used to draw cursor overlay, see on_display in Hazel.re
        Attr.create(
          "children",
          snode
          |> child_indices_of_snode
          |> sexp_of_child_indices
          |> Sexplib.Sexp.to_string,
        ),
        ...shape_attrs,
      ];
    }
  );
};

let sline_clss = line_no => ["SLine", "SLine-" ++ string_of_int(line_no)];

let var_err_status_clss =
  fun
  | NotInVHole => []
  | InVHole(Free, u) => ["InVHole", "InVHole_" ++ string_of_int(u)]
  | InVHole(Keyword(_), u) => [
      "InVHole",
      "InVHole_" ++ string_of_int(u),
      "Keyword",
    ];

/*
 let on_click_noneditable =
     (
       ~inject,
       steps: Path.steps,
       k: delim_index,
       evt: Js.t(Dom_html.mouseEvent),
     ) => {
   switch (Js.Opt.to_option(evt##.target)) {
   | None => inject(Update.Action.SetCaret((steps, OnDelim(k, Before))))
   | Some(target) =>
     let from_left =
       float_of_int(evt##.clientX) -. target##getBoundingClientRect##.left;
     let from_right =
       target##getBoundingClientRect##.right -. float_of_int(evt##.clientX);
     inject(
       Update.Action.SetCaret((
         steps,
         OnDelim(k, from_left <= from_right ? Before : After),
       )),
     );
   };
 };
 */

/* TODO */
let range_of_tree_rooted_at_cursor = (_cursor, _sskel) => (0, 0);

let vindent_path = path =>
  Vdom.(
    Node.span(
      [
        Attr.classes(["indent"]),
        Attr.create("contenteditable", "false"),
        Attr.create("goto-path", Sexp.to_string(Path.sexp_of_t(path))),
      ],
      [Node.text("  ")],
    )
  );
let vindent_steps = steps =>
  Vdom.(
    Node.span(
      [
        Attr.classes(["indent"]),
        Attr.create("contenteditable", "false"),
        Attr.create(
          "goto-steps",
          Sexp.to_string(Path.sexp_of_steps(steps)),
        ),
      ],
      [Node.text("  ")],
    )
  );
let vindent =
  Vdom.(
    Node.span(
      [Attr.classes(["indent"]), Attr.create("contenteditable", "false")],
      [Node.text("  ")],
    )
  );

let rec view_of_snode =
        (
          ~inject: Update.Action.t => Vdom.Event.t,
          ~abs_indent: absolute_indent=0,
          snode,
        )
        : Vdom.Node.t => {
  let attrs = snode_attrs(~inject, snode);
  switch (snode) {
  | SSeq(steps, cursor, is_multi_line, sskel, shead, stail) =>
    let (vhead: Vdom.Node.t, vtail: list(Vdom.Node.t)) =
      switch (cursor) {
      | None =>
        let vhead =
          view_of_sline(
            ~inject,
            ~node_steps=steps,
            ~is_node_multi_line=is_multi_line,
            ~line_no=0,
            ~abs_indent,
            mk_sline(
              ~steps_of_first_sword=steps_of_snode(shead),
              [SNode(shead)],
            ),
          );
        let vtail =
          stail
          |> List.mapi((i, segment) =>
               view_of_sline(
                 ~inject,
                 ~node_steps=steps,
                 ~is_node_multi_line=is_multi_line,
                 ~line_no=i + 1,
                 ~abs_indent,
                 sline_of_segment(~steps, segment),
               )
             );
        (vhead, vtail);
      | Some(cursor) =>
        let (a, b) = range_of_tree_rooted_at_cursor(cursor, sskel);
        let vhead_border_style = a == 0 ? TopBottom(true, false) : NoBorder;
        let vhead =
          view_of_sline(
            ~inject,
            ~node_steps=steps,
            ~node_cursor=cursor,
            ~is_node_multi_line=is_multi_line,
            ~border_style=vhead_border_style,
            ~line_no=0,
            ~abs_indent,
            mk_sline(
              ~steps_of_first_sword=steps_of_snode(shead),
              [SNode(shead)],
            ),
          );
        let vtail =
          stail
          |> List.mapi((i, segment) => (i + 1, segment))
          |> List.map(((i, segment)) =>
               view_of_sline(
                 ~inject,
                 ~node_steps=steps,
                 ~node_cursor=cursor,
                 ~is_node_multi_line=is_multi_line,
                 ~border_style=
                   a < i && i < b
                     ? TopBottom(false, false) : TopBottom(false, i == b),
                 ~line_no=i,
                 ~abs_indent,
                 i == a
                   ? sline_of_segment(
                       ~steps,
                       segment
                       |> update_border_style_of_segment_node(
                            TopBottom(true, false),
                          ),
                     )
                   : sline_of_segment(~steps, segment),
               )
             );
        (vhead, vtail);
      };
    let vlines = [vhead, ...vtail];
    Vdom.Node.div(
      attrs,
      is_multi_line ? join(Vdom.Node.br([]), vlines) : vlines,
    );
  | SBox(border_style, steps, node_cursor, is_multi_line, _, _shape, slines) =>
    let n = List.length(slines);
    let vlines: list(Vdom.Node.t) =
      slines
      |> List.mapi((i, sline) =>
           view_of_sline(
             ~inject,
             ~node_steps=steps,
             ~node_cursor?,
             ~is_node_multi_line=is_multi_line,
             ~line_no=i,
             ~abs_indent,
             ~border_style=
               switch (n, i, border_style) {
               | (1, _, _) => border_style
               | (_, 0, Top(is_start_of_top))
               | (_, 0, TopBottom(is_start_of_top, _)) =>
                 Top(is_start_of_top)
               | (_, _, Bottom(is_end_of_bottom)) when i == n =>
                 Bottom(is_end_of_bottom)
               | (_, _, TopBottom(_, is_end_of_bottom)) when i == n =>
                 Bottom(is_end_of_bottom)
               | (_, _, _) => NoBorder
               },
             sline,
           )
         );
    Vdom.Node.div(
      attrs,
      is_multi_line ? vlines |> join(Vdom.Node.br([])) : vlines,
    );
  };
}
and view_of_sline =
    (
      ~inject: Update.Action.t => Vdom.Event.t,
      ~node_steps: Path.steps,
      ~node_cursor: option(cursor_position)=?,
      ~is_node_multi_line: bool,
      ~border_style=NoBorder,
      ~line_no: int,
      /* TODO need to rename this, concept unclear when it comes to middle-of-line snodes */
      ~abs_indent: absolute_indent,
      (rel_indent, steps_of_first_sword, swords): sline,
    )
    : Vdom.Node.t => {
  let more_indent = is_node_multi_line ? rel_indent : 0;
  // Slines may contain multi-line snodes, i.e., their slines should
  // each be on their own lines in the view. Only add indentation to
  // view of current sline if it is itself a single line in the view.
  let contains_multi_line =
    swords
    |> List.exists(
         fun
         | SNode(snode) => is_multi_line(snode)
         | SToken(_) => false,
       );
  let vindents =
    switch (contains_multi_line, swords) {
    | (true, _) => []
    | (_, []) => []
    | (false, [sword, ...rest]) =>
      range(abs_indent + more_indent)
      |> List.map(_ =>
           switch (steps_of_first_sword) {
           | None => vindent
           | Some(steps) =>
             switch (sword) {
             | SNode(_) => vindent_steps(steps)
             | SToken(stoken) =>
               switch (stoken) {
               | SEmptyHole(_)
               | SText(_, _)
               | SEmptyLine => vindent_steps(steps)
               | SDelim(Some(k), _)
               | SOp(Some(k), _) =>
                 vindent_path((steps, OnDelim(k, Before)))
               | SSpaceOp =>
                 switch (rest) {
                 | [] => vindent
                 | [SNode(snode), ..._] =>
                   vindent_steps(steps_of_snode(snode))
                 | [_, ..._] => vindent
                 }
               | SOp(None, _)
               | SDelim(None, _)
               | SCastArrow
               | SFailedCastArrow
               | SSpace => vindent
               }
             }
           }
         )
    };
  let vwords =
    swords
    |> List.fold_left(
         (vwords_so_far, sword) =>
           vwords_so_far
           @ [
             switch (sword) {
             | SNode(snode) =>
               view_of_snode(
                 ~inject,
                 ~abs_indent=
                   List.length(vwords_so_far) == 0
                   && is_node_multi_line
                   && contains_multi_line
                     ? abs_indent + more_indent : 0,
                 snode,
               )
             | SToken(stoken) =>
               view_of_stoken(~inject, ~node_steps, ~node_cursor, stoken)
             },
           ],
         [],
       );
  let goto_steps_attrs =
    switch (steps_of_first_sword) {
    | None => []
    | Some(steps) => [
        Vdom.Attr.create(
          "goto-steps",
          Sexp.to_string(Path.sexp_of_steps(steps)),
        ),
      ]
    };
  Vdom.(
    Node.div(
      [
        Attr.classes(
          [inline_div_cls] @ sline_clss(line_no) @ border_clss(border_style),
        ),
        ...goto_steps_attrs,
      ],
      vindents @ vwords,
    )
  );
}
[@warning "-27"]
and view_of_stoken =
    (
      ~inject: Update.Action.t => Vdom.Event.t,
      ~node_steps: Path.steps,
      ~node_cursor: option(cursor_position),
      stoken: stoken,
    )
    : Vdom.Node.t =>
  switch (stoken) {
  | SEmptyHole(lbl) =>
    open Vdom;
    let hole_before =
      Node.span(
        [
          Attr.id(path_id((node_steps, OnDelim(0, Before)))),
          Attr.classes(["SEmptyHole-before", "unselectable-before"]),
        ],
        [Node.text(LangUtil.nondisplay3)],
      );
    let hole_after =
      Node.span(
        [
          Attr.id(path_id((node_steps, OnDelim(0, After)))),
          Attr.classes(["SEmptyHole-after", "unselectable-after"]),
        ],
        [Node.text(LangUtil.nondisplay3)],
      );
    let hole_lbl =
      Node.span(
        [
          Attr.classes(["SEmptyHole-lbl", "unselectable"]),
          Attr.create("contenteditable", "false"),
          Attr.create(
            "path-before",
            Sexp.to_string(
              Path.sexp_of_t((node_steps, OnDelim(0, Before))),
            ),
          ),
          Attr.create(
            "path-after",
            Sexp.to_string(Path.sexp_of_t((node_steps, OnDelim(0, After)))),
          ),
        ],
        [
          Node.text(LangUtil.nondisplay1),
          Node.span([Attr.classes(["SEmptyHole-num"])], [Node.text(lbl)]),
          Node.text(LangUtil.nondisplay1),
        ],
      );
    Node.div(
      [Attr.classes([inline_div_cls, "SEmptyHole"])],
      [hole_before, hole_lbl, hole_after],
    );
  | SDelim(delim_index, s) =>
    open Vdom;
    let (delim_before_nodes, delim_after_nodes, delim_txt_attrs) =
      switch (delim_index) {
      | None => ([], [], [])
      | Some(k) =>
        let delim_before =
          Node.span(
            [
              Attr.id(path_id((node_steps, OnDelim(k, Before)))),
              Attr.classes(["SDelim-before", "unselectable-before"]),
            ],
            [Node.text(LangUtil.nondisplay3)],
          );
        let delim_after =
          Node.span(
            [
              Attr.id(path_id((node_steps, OnDelim(k, After)))),
              Attr.classes(["SDelim-after", "unselectable-after"]),
            ],
            [Node.text(LangUtil.nondisplay3)],
          );
        let attrs = [
          Attr.create(
            "path-before",
            Sexp.to_string(
              Path.sexp_of_t((node_steps, OnDelim(k, Before))),
            ),
          ),
          Attr.create(
            "path-after",
            Sexp.to_string(Path.sexp_of_t((node_steps, OnDelim(k, After)))),
          ),
        ];
        ([delim_before], [delim_after], attrs);
      };
    let delim_txt =
      Node.span(
        [
          Attr.classes(["SDelim-txt", "unselectable"]),
          Attr.create("contenteditable", "false"),
          ...delim_txt_attrs,
        ],
        [Node.text(s)],
      );
    Node.div(
      [Attr.classes([inline_div_cls, "SDelim"])],
      delim_before_nodes @ [delim_txt] @ delim_after_nodes,
    );
  | SOp(op_index, s) =>
    open Vdom;
    let (op_before_nodes, op_after_nodes, op_txt_attrs) =
      switch (op_index) {
      | None => ([], [], [])
      | Some(k) =>
        let op_before =
          Node.span(
            [
              Attr.id(path_id((node_steps, OnDelim(k, Before)))),
              Attr.classes(["SOp-before", "unselectable-before"]),
            ],
            [Node.text(LangUtil.nondisplay3)],
          );
        let op_after =
          Node.span(
            [
              Attr.id(path_id((node_steps, OnDelim(k, After)))),
              Attr.classes(["SOp-after", "unselectable-after"]),
            ],
            [Node.text(LangUtil.nondisplay3)],
          );
        let attrs = [
          Attr.create(
            "path-before",
            Sexp.to_string(
              Path.sexp_of_t((node_steps, OnDelim(k, Before))),
            ),
          ),
          Attr.create(
            "path-after",
            Sexp.to_string(Path.sexp_of_t((node_steps, OnDelim(k, After)))),
          ),
        ];
        ([op_before], [op_after], attrs);
      };
    let op_txt =
      Node.span(
        [
          Attr.create("contenteditable", "false"),
          Attr.classes(["SOp-txt", "unselectable"]),
          ...op_txt_attrs,
        ],
        [Node.text(s)],
      );
    Node.div(
      [Attr.classes([inline_div_cls, "SOp"])],
      op_before_nodes @ [op_txt] @ op_after_nodes,
    );
  | SSpaceOp =>
    Vdom.(
      Node.span(
        [
          Attr.classes(["SSpaceOp"]),
          Attr.create("contenteditable", "false"),
        ],
        [Node.text(" ")],
      )
    )
  | SText(var_err_status, s) =>
    Vdom.(
      Node.div(
        [
          Attr.id(text_id(node_steps)),
          Attr.classes(
            [inline_div_cls, "SText"] @ var_err_status_clss(var_err_status),
          ),
        ],
        [Node.text(s)],
      )
    )
  | SCastArrow =>
    Vdom.(
      Node.div(
        [Attr.classes([inline_div_cls, "SCastArrow"])],
        [Node.text(" ⇨ ")],
      )
    )
  | SFailedCastArrow =>
    Vdom.(
      Node.div(
        [Attr.classes([inline_div_cls, "SFailedCastArrow"])],
        [Node.text(" ⇨ ")],
      )
    )
  | SEmptyLine =>
    Vdom.(
      Node.div(
        [
          Attr.id(text_id(node_steps)),
          Attr.classes([inline_div_cls, "SEmptyLine"]),
        ],
        [Node.text(LangUtil.nondisplay4)],
      )
    )
  | SSpace =>
    Vdom.(
      Node.span(
        [Attr.classes(["SSpace"]), Attr.create("contenteditable", "false")],
        [Node.text(" ")],
      )
    )
  };

let caret_position_of_path =
    ((steps, cursor) as path): option((Js.t(Dom.node), int)) =>
  switch (cursor) {
  | OnDelim(_, _) =>
    switch (JSUtil.get_elem_by_id(path_id(path))) {
    | None => None
    | Some(anchor_parent) =>
      let has_cls = JSUtil.elem_has_cls(anchor_parent);
      let anchor_offset =
        if (has_cls("unselectable-before")) {
          3;
        } else if (has_cls("unselectable-after")) {
          0;
        } else {
          0;
        };
      let anchor_parent_node = (
        anchor_parent: Js.t(Dom_html.element) :> Js.t(Dom.node)
      );
      let anchor =
        Js.Opt.get(anchor_parent_node##.firstChild, () =>
          raise(MalformedView(0))
        );
      Some((anchor, anchor_offset));
    }
  | OnText(j) =>
    switch (JSUtil.get_elem_by_id(text_id(steps))) {
    | None => None
    | Some(elem) =>
      let anchor_parent = (elem: Js.t(Dom_html.element) :> Js.t(Dom.node));
      let anchor =
        Js.Opt.get(anchor_parent##.firstChild, () =>
          raise(MalformedView(1))
        );
      Some((anchor, j));
    }
  };

let is_caret_consistent_with_path = path =>
  switch (caret_position_of_path(path)) {
  | None => false
  | Some((anchor, offset)) =>
    (anchor, offset)
    == (
         Dom_html.window##getSelection##.anchorNode,
         Dom_html.window##getSelection##.anchorOffset,
       )
  };

let snode_of_EmptyHole = (~cursor=?, ~steps, hole_name: string): snode =>
  mk_SBox(
    ~cursor?,
    ~steps,
    ~shape=EmptyHole,
    [
      mk_sline(
        ~steps_of_first_sword=steps,
        [SToken(SEmptyHole(hole_name))],
      ),
    ],
  );

let snode_of_Var =
    (~cursor=?, ~err_status, ~var_err_status, ~steps, x: Var.t): snode =>
  mk_SBox(
    ~cursor?,
    ~err_status,
    ~steps,
    ~shape=Var,
    [
      mk_sline(
        ~steps_of_first_sword=steps,
        [SToken(mk_SText(~var_err_status, x))],
      ),
    ],
  );

let snode_of_NumLit = (~cursor=?, ~err_status, ~steps, n: int): snode =>
  mk_SBox(
    ~cursor?,
    ~err_status,
    ~steps,
    ~shape=NumLit,
    [
      mk_sline(
        ~steps_of_first_sword=steps,
        [SToken(mk_SText(string_of_int(n)))],
      ),
    ],
  );

let snode_of_BoolLit = (~cursor=?, ~err_status, ~steps, b: bool): snode =>
  mk_SBox(
    ~cursor?,
    ~err_status,
    ~steps,
    ~shape=BoolLit,
    [
      mk_sline(
        ~steps_of_first_sword=steps,
        [SToken(mk_SText(string_of_bool(b)))],
      ),
    ],
  );

let snode_of_ListNil = (~cursor=?, ~err_status, ~steps, ()): snode =>
  mk_SBox(
    ~cursor?,
    ~err_status,
    ~steps,
    ~shape=ListNil,
    [
      mk_sline(
        ~steps_of_first_sword=steps,
        [SToken(mk_SDelim(~index=0, "[]"))],
      ),
    ],
  );

let snode_of_LetLine =
    (~cursor=?, ~steps, sp: snode, sann: option(snode), sdef: snode) =>
  mk_SBox(
    ~cursor?,
    ~steps,
    ~shape=LetLine,
    ~is_multi_line=is_multi_line(sdef),
    [
      mk_sline(
        ~steps_of_first_sword=steps,
        [SToken(mk_SDelim(~index=0, "let")), SToken(SSpace), SNode(sp)]
        @ (
          switch (sann) {
          | None => []
          | Some(sann) => [
              SToken(SSpace),
              SToken(mk_SDelim(~index=1, ":")),
              SToken(SSpace),
              SNode(sann),
            ]
          }
        )
        @ [
          SToken(SSpace),
          SToken(mk_SDelim(~index=2, "=")),
          SToken(SSpace),
        ],
      ),
      mk_sline(~rel_indent=1, [SNode(sdef)]),
    ],
  );

let snode_of_Parenthesized = (~cursor=?, ~steps, sbody: snode): snode =>
  mk_SBox(
    ~cursor?,
    ~steps,
    ~shape=Parenthesized,
    ~is_multi_line=is_multi_line(sbody),
    [
      mk_sline(
        ~steps_of_first_sword=steps,
        [SToken(mk_SDelim(~index=0, "("))],
      ),
      mk_sline(
        ~rel_indent=1,
        ~steps_of_first_sword=steps_of_snode(sbody),
        [SNode(sbody)],
      ),
      mk_sline(
        ~steps_of_first_sword=steps,
        [SToken(mk_SDelim(~index=1, ")"))],
      ),
    ],
  );

let snode_of_List = (~cursor=?, ~steps, sbody: snode): snode =>
  mk_SBox(
    ~cursor?,
    ~steps,
    ~shape=List,
    [
      mk_sline(
        ~steps_of_first_sword=steps,
        [SToken(mk_SDelim(~index=0, "List("))],
      ),
      mk_sline(
        ~rel_indent=1,
        ~steps_of_first_sword=steps_of_snode(sbody),
        [SNode(sbody)],
      ),
      mk_sline(
        ~steps_of_first_sword=steps,
        [SToken(mk_SDelim(~index=1, ")"))],
      ),
    ],
  );

let snode_of_OpSeq =
    (
      ~cursor=?,
      ~steps,
      ~sskel,
      stms: list(snode),
      sops: list((space_before, stoken, space_after)),
    )
    : snode => {
  let is_multi_line = stms |> List.exists(is_multi_line);
  let (shead, stail_tms) =
    switch (stms) {
    | [] => assert(false)
    | [hd, ...rst] => (hd, rst)
    };
  let stail =
    List.combine(sops, stail_tms)
    |> List.map((((space_before, sop, space_after), stm)) =>
         switch (sop) {
         | SSpaceOp => SSegmentSpace(stm)
         | SOp(_, _) =>
           let before = space_before ? [SSpace] : [];
           let after = space_after ? [SSpace] : [];
           SSegmentOp(
             is_multi_line ? [sop] @ after : before @ [sop] @ after,
             stm,
           );
         | _ => SSegmentSpace(stm) /* should never happen */
         }
       );
  mk_SSeq(~cursor?, ~is_multi_line, ~steps, ~sskel, shead, stail);
};

let snode_of_Lam =
    (
      ~cursor=?,
      ~err_status,
      ~steps,
      sarg: snode,
      sann: option(snode),
      sbody: snode,
    )
    : snode => {
  let swords_ann =
    switch (sann) {
    | None => []
    | Some(sann) => [SToken(mk_SDelim(~index=1, ":")), SNode(sann)]
    };
  mk_SBox(
    ~cursor?,
    ~err_status,
    ~steps,
    ~shape=Lam,
    ~is_multi_line=is_multi_line(sbody),
    [
      mk_sline(
        ~steps_of_first_sword=steps,
        [SToken(mk_SDelim(~index=0, LangUtil.lamSym)), SNode(sarg)]
        @ swords_ann
        @ [SToken(mk_SDelim(~index=2, "."))],
      ),
      mk_sline(
        ~rel_indent=1,
        ~steps_of_first_sword=steps_of_snode(sbody),
        [SNode(sbody)],
      ),
    ],
  );
};

let snode_of_Inj = (~cursor=?, ~err_status, ~steps, side, sbody: snode): snode =>
  mk_SBox(
    ~cursor?,
    ~err_status,
    ~steps,
    ~shape=Inj,
    ~is_multi_line=is_multi_line(sbody),
    [
      mk_sline(
        ~steps_of_first_sword=steps,
        [
          SToken(
            mk_SDelim(
              ~index=0,
              "inj[" ++ LangUtil.string_of_side(side) ++ "](",
            ),
          ),
        ],
      ),
      mk_sline(
        ~rel_indent=1,
        ~steps_of_first_sword=steps_of_snode(sbody),
        [SNode(sbody)],
      ),
      mk_sline(
        ~steps_of_first_sword=steps,
        [SToken(mk_SDelim(~index=1, ")"))],
      ),
    ],
  );

let snode_of_InjAnn =
    (~cursor=?, ~err_status, ~steps, sty: snode, side, sbody: snode): snode =>
  mk_SBox(
    ~cursor?,
    ~err_status,
    ~steps,
    ~shape=InjAnn,
    ~is_multi_line=is_multi_line(sbody),
    [
      mk_sline(
        ~steps_of_first_sword=steps,
        [
          SToken(mk_SDelim("inj[" ++ LangUtil.string_of_side(side) ++ ",")),
          SNode(sty),
          SToken(mk_SDelim("](")),
        ],
      ),
      mk_sline(
        ~rel_indent=1,
        ~steps_of_first_sword=steps_of_snode(sbody),
        [SNode(sbody)],
      ),
      mk_sline(~steps_of_first_sword=steps, [SToken(mk_SDelim(")"))]),
    ],
  );

let snode_of_Case =
    (
      ~cursor=?,
      ~err_status,
      ~steps,
      sscrut: snode,
      srules: list(snode),
      sann: option(snode),
    )
    : snode => {
  let sline_case =
    mk_sline(
      ~steps_of_first_sword=steps,
      [
        SToken(mk_SDelim(~index=0, "case")),
        SToken(SSpace),
        SNode(sscrut),
      ],
    );
  let slines_rules =
    srules
    |> List.map(snode =>
         mk_sline(
           ~steps_of_first_sword=steps_of_snode(snode),
           [SNode(snode)],
         )
       );
  let sline_end =
    switch (sann) {
    | None =>
      mk_sline(
        ~steps_of_first_sword=steps,
        [SToken(mk_SDelim(~index=1, "end"))],
      )
    | Some(sann) =>
      mk_sline(
        ~steps_of_first_sword=steps,
        [SToken(mk_SDelim(~index=1, "end : ")), SNode(sann)],
      )
    };
  mk_SBox(
    ~cursor?,
    ~err_status,
    ~steps,
    ~shape=Case,
    ~is_multi_line=true,
    [sline_case] @ slines_rules @ [sline_end],
  );
};

let snode_of_Rule = (~cursor=?, ~steps, sp: snode, sclause: snode) =>
  mk_SBox(
    ~cursor?,
    ~steps,
    ~shape=Rule,
    ~is_multi_line=is_multi_line(sclause),
    [
      mk_sline(
        ~steps_of_first_sword=steps,
        [
          SToken(mk_SDelim(~index=0, "|")),
          SToken(SSpace),
          SNode(sp),
          SToken(SSpace),
          SToken(mk_SDelim(~index=1, LangUtil.caseArrowSym)),
          SToken(SSpace),
        ],
      ),
      mk_sline(
        ~rel_indent=1,
        ~steps_of_first_sword=steps_of_snode(sclause),
        [SNode(sclause)],
      ),
    ],
  );

let snode_of_Triv = (~err_status, ~steps) =>
  mk_SBox(
    ~err_status,
    ~steps,
    ~shape=Triv,
    [
      mk_sline(
        ~steps_of_first_sword=steps,
        [SToken(mk_SDelim(~index=0, "()"))],
      ),
    ],
  );

let snode_of_Bool = (~cursor=?, ~steps, ()) =>
  mk_SBox(
    ~cursor?,
    ~steps,
    ~shape=Bool,
    [
      mk_sline(
        ~steps_of_first_sword=steps,
        [SToken(mk_SDelim(~index=0, "Bool"))],
      ),
    ],
  );

let snode_of_Num = (~cursor=?, ~steps, ()) =>
  mk_SBox(
    ~cursor?,
    ~steps,
    ~shape=Num,
    [
      mk_sline(
        ~steps_of_first_sword=steps,
        [SToken(mk_SDelim(~index=0, "Num"))],
      ),
    ],
  );

let snode_of_Unit = (~cursor=?, ~steps, ()) =>
  mk_SBox(
    ~cursor?,
    ~steps,
    ~shape=Unit,
    [
      mk_sline(
        ~steps_of_first_sword=steps,
        [SToken(mk_SDelim(~index=0, "()"))],
      ),
    ],
  );

let rec snode_of_typ = (~cursor=?, ~steps: Path.steps, uty: UHTyp.t): snode =>
  switch (uty) {
  | Hole => snode_of_EmptyHole(~cursor?, ~steps, "?")
  | Unit => snode_of_Unit(~cursor?, ~steps, ())
  | Num => snode_of_Num(~cursor?, ~steps, ())
  | Bool => snode_of_Bool(~cursor?, ~steps, ())
  | Parenthesized(body) =>
    let sbody = snode_of_typ(~steps=steps @ [0], body);
    snode_of_Parenthesized(~cursor?, ~steps, sbody);
  | List(body) =>
    let sbody = snode_of_typ(~steps=steps @ [0], body);
    snode_of_List(~cursor?, ~steps, sbody);
  | OpSeq(skel, seq) =>
    let stms =
      OperatorSeq.tms(seq)
      |> List.mapi((i, tm) => snode_of_typ(~steps=steps @ [i], tm));
    let sops =
      OperatorSeq.ops(seq)
      |> List.mapi((i, op) => {
           let (space_before, space_after) = space_before_after_op_typ(op);
           (
             space_before,
             mk_SOp(~index=i + 1, string_of_op_typ(op)),
             space_after,
           );
         });
    let sskel = sskel_of_skel_typ(skel);
    snode_of_OpSeq(~cursor?, ~steps, ~sskel, stms, sops);
  };

let rec snode_of_pat = (~cursor=?, ~steps: Path.steps, p: UHPat.t): snode =>
  switch (p) {
  | EmptyHole(u) =>
    snode_of_EmptyHole(~cursor?, ~steps, string_of_int(u + 1))
  | Wild(err_status) =>
    mk_SBox(
      ~cursor?,
      ~err_status,
      ~steps,
      ~shape=Wild,
      [
        mk_sline(
          ~steps_of_first_sword=steps,
          [SToken(mk_SDelim(~index=0, "_"))],
        ),
      ],
    )
  | Var(err_status, var_err_status, x) =>
    snode_of_Var(~cursor?, ~err_status, ~var_err_status, ~steps, x)
  | NumLit(err_status, n) =>
    snode_of_NumLit(~cursor?, ~err_status, ~steps, n)
  | BoolLit(err_status, b) =>
    snode_of_BoolLit(~cursor?, ~err_status, ~steps, b)
  | ListNil(err_status) =>
    snode_of_ListNil(~cursor?, ~err_status, ~steps, ())
  | Inj(err_status, side, body) =>
    let sbody = snode_of_pat(~steps=steps @ [0], body);
    snode_of_Inj(~cursor?, ~err_status, ~steps, side, sbody);
  | Parenthesized(body) =>
    let sbody = snode_of_pat(~steps=steps @ [0], body);
    snode_of_Parenthesized(~cursor?, ~steps, sbody);
  | OpSeq(skel, seq) =>
    let stms =
      OperatorSeq.tms(seq)
      |> List.mapi((i, tm) => snode_of_pat(~steps=steps @ [i], tm));
    let sops =
      OperatorSeq.ops(seq)
      |> List.mapi((i, op) => {
           let (space_before, space_after) = space_before_after_op_pat(op);
           (
             space_before,
             switch ((op: UHPat.op)) {
             | Space => SSpaceOp
             | _ => mk_SOp(~index=i + 1, string_of_op_pat(op))
             },
             space_after,
           );
         });
    let sskel = sskel_of_skel_pat(skel);
    snode_of_OpSeq(~cursor?, ~steps, ~sskel, stms, sops);
  };

let rec snode_of_block =
        (~steps: Path.steps=[], Block(line_items, e): UHExp.block): snode => {
  let sline_items =
    line_items
    |> List.mapi((i, li) => snode_of_line_item(~steps=steps @ [i], li));
  let se = snode_of_exp(~steps=steps @ [List.length(line_items)], e);
  mk_SBox(
    ~steps,
    ~shape=Block,
    ~is_multi_line=List.length(sline_items) != 0 || is_multi_line(se),
    sline_items
    @ [se]
    |> List.map(snode =>
         mk_sline(
           ~steps_of_first_sword=steps_of_snode(snode),
           [SNode(snode)],
         )
       ),
  );
}
and snode_of_line_item =
    (~cursor=?, ~steps: Path.steps, li: UHExp.line): snode =>
  switch (li) {
  | EmptyLine =>
    mk_SBox(
      ~cursor?,
      ~steps,
      ~shape=EmptyLine,
      [mk_sline(~steps_of_first_sword=steps, [SToken(SEmptyLine)])],
    )
  | ExpLine(e) => snode_of_exp(~cursor?, ~steps, e) /* ghost node */
  | LetLine(p, ann, def) =>
    let sp = snode_of_pat(~steps=steps @ [0], p);
    let sann =
      switch (ann) {
      | None => None
      | Some(ann) => Some(snode_of_typ(~steps=steps @ [1], ann))
      };
    let sdef = snode_of_block(~steps=steps @ [2], def);
    snode_of_LetLine(~cursor?, ~steps, sp, sann, sdef);
  }
and snode_of_exp = (~cursor=?, ~steps: Path.steps, e: UHExp.t): snode =>
  switch (e) {
  /* outer nodes */
  | EmptyHole(u) =>
    snode_of_EmptyHole(~cursor?, ~steps, string_of_int(u + 1))
  | Var(err_status, var_err_status, x) =>
    snode_of_Var(~cursor?, ~err_status, ~var_err_status, ~steps, x)
  | NumLit(err_status, n) =>
    snode_of_NumLit(~cursor?, ~err_status, ~steps, n)
  | BoolLit(err_status, b) =>
    snode_of_BoolLit(~cursor?, ~err_status, ~steps, b)
  | ListNil(err_status) =>
    snode_of_ListNil(~cursor?, ~err_status, ~steps, ())
  /* inner nodes */
  | Lam(err_status, arg, ann, body) =>
    let sarg = snode_of_pat(~steps=steps @ [0], arg);
    let sann =
      switch (ann) {
      | None => None
      | Some(ann) => Some(snode_of_typ(~steps=steps @ [1], ann))
      };
    let sbody = snode_of_block(~steps=steps @ [2], body);
    snode_of_Lam(~cursor?, ~steps, ~err_status, sarg, sann, sbody);
  | Inj(err_status, side, body) =>
    let sbody = snode_of_block(~steps=steps @ [0], body);
    snode_of_Inj(~cursor?, ~err_status, ~steps, side, sbody);
  | Case(err_status, scrut, rules, ann) =>
    let sscrut = snode_of_block(~steps=steps @ [0], scrut);
    let srules =
      rules
      |> List.mapi((i, rule) => snode_of_rule(~steps=steps @ [i + 1], rule));
    let sann =
      switch (ann) {
      | None => None
      | Some(ann) =>
        Some(snode_of_typ(~steps=steps @ [List.length(rules) + 1], ann))
      };
    snode_of_Case(~cursor?, ~err_status, ~steps, sscrut, srules, sann);
  | Parenthesized(body) =>
    let sbody = snode_of_block(~steps=steps @ [0], body);
    snode_of_Parenthesized(~cursor?, ~steps, sbody);
  | OpSeq(skel, seq) =>
    let stms =
      OperatorSeq.tms(seq)
      |> List.mapi((i, tm) => snode_of_exp(~steps=steps @ [i], tm));
    let sops =
      OperatorSeq.ops(seq)
      |> List.mapi((i, op) => {
           let (space_before, space_after) = space_before_after_op_exp(op);
           (
             space_before,
             switch ((op: UHExp.op)) {
             | Space => SSpaceOp
             | _ => mk_SOp(~index=i + 1, string_of_op_exp(op))
             },
             space_after,
           );
         });
    let sskel = sskel_of_skel_exp(skel);
    snode_of_OpSeq(~cursor?, ~steps, ~sskel, stms, sops);
  | ApPalette(_, _, _, _) => raise(InvariantViolated)
  }
and snode_of_rule =
    (~cursor=?, ~steps: Path.steps, Rule(p, clause): UHExp.rule) => {
  let sp = snode_of_pat(~steps=steps @ [0], p);
  let sclause = snode_of_block(~steps=steps @ [1], clause);
  snode_of_Rule(~cursor?, ~steps, sp, sclause);
};

let rec snode_of_ztyp = (~steps: Path.steps, zty: ZTyp.t): snode =>
  switch (zty) {
  | CursorT(cursor, uty) => snode_of_typ(~cursor, ~steps, uty)
  | ParenthesizedZ(zbody) =>
    let szbody = snode_of_ztyp(~steps=steps @ [0], zbody);
    snode_of_Parenthesized(~steps, szbody);
  | ListZ(zbody) =>
    let szbody = snode_of_ztyp(~steps=steps @ [0], zbody);
    snode_of_List(~steps, szbody);
  | OpSeqZ(skel, ztm, surround) =>
    let (prefix_tms, suffix_tms) = OperatorSeq.tms_of_surround(surround);
    let (prefix_ops, suffix_ops) = OperatorSeq.ops_of_surround(surround);
    let sprefix =
      prefix_tms
      |> List.mapi((i, tm) => snode_of_typ(~steps=steps @ [i], tm));
    let sztm = snode_of_ztyp(~steps=steps @ [List.length(prefix_tms)], ztm);
    let ssuffix =
      suffix_tms
      |> List.mapi((i, tm) =>
           snode_of_typ(
             ~steps=steps @ [i + List.length(prefix_tms) + 1],
             tm,
           )
         );
    let sops =
      prefix_ops
      @ suffix_ops
      |> List.mapi((i, op) => {
           let (space_before, space_after) = space_before_after_op_typ(op);
           (
             space_before,
             mk_SOp(~index=i + 1, string_of_op_typ(op)),
             space_after,
           );
         });
    let sskel = sskel_of_skel_typ(skel);
    snode_of_OpSeq(~steps, ~sskel, sprefix @ [sztm] @ ssuffix, sops);
  };

let rec snode_of_zpat = (~steps: Path.steps, zp: ZPat.t): snode =>
  switch (zp) {
  | CursorP(cursor, p) => snode_of_pat(~cursor, ~steps, p)
  | ParenthesizedZ(zbody) =>
    let szbody = snode_of_zpat(~steps=steps @ [0], zbody);
    snode_of_Parenthesized(~steps, szbody);
  | InjZ(err_status, side, zbody) =>
    let szbody = snode_of_zpat(~steps=steps @ [0], zbody);
    snode_of_Inj(~err_status, ~steps, side, szbody);
  | OpSeqZ(skel, ztm, surround) =>
    let (prefix_tms, suffix_tms) = OperatorSeq.tms_of_surround(surround);
    let (prefix_ops, suffix_ops) = OperatorSeq.ops_of_surround(surround);
    let sprefix =
      prefix_tms
      |> List.mapi((i, tm) => snode_of_pat(~steps=steps @ [i], tm));
    let sztm = snode_of_zpat(~steps=steps @ [List.length(prefix_tms)], ztm);
    let ssuffix =
      suffix_tms
      |> List.mapi((i, tm) =>
           snode_of_pat(
             ~steps=steps @ [i + List.length(prefix_tms) + 1],
             tm,
           )
         );
    let sops =
      prefix_ops
      @ suffix_ops
      |> List.mapi((i, op) => {
           let (space_before, space_after) = space_before_after_op_pat(op);
           (
             space_before,
             switch ((op: UHPat.op)) {
             | Space => SSpaceOp
             | _ => mk_SOp(~index=i + 1, string_of_op_pat(op))
             },
             space_after,
           );
         });
    let sskel = sskel_of_skel_pat(skel);
    snode_of_OpSeq(~steps, ~sskel, sprefix @ [sztm] @ ssuffix, sops);
  };

let rec snode_of_zblock = (~steps: Path.steps=[], zblock: ZExp.zblock): snode =>
  switch (zblock) {
  | BlockZL((prefix, zline_item, suffix), e) =>
    let sprefix =
      prefix
      |> List.mapi((i, li) => snode_of_line_item(~steps=steps @ [i], li));
    let szline_item =
      snode_of_zline_item(~steps=steps @ [List.length(prefix)], zline_item);
    let ssuffix =
      suffix
      |> List.mapi((i, li) =>
           snode_of_line_item(
             ~steps=steps @ [i + List.length(prefix) + 1],
             li,
           )
         );
    let se =
      snode_of_exp(
        ~steps=steps @ [List.length(prefix) + 1 + List.length(suffix)],
        e,
      );
    mk_SBox(
      ~steps,
      ~shape=Block,
      ~is_multi_line=true,
      sprefix
      @ [szline_item]
      @ ssuffix
      @ [se]
      |> List.map(snode =>
           mk_sline(
             ~steps_of_first_sword=steps_of_snode(snode),
             [SNode(snode)],
           )
         ),
    );
  | BlockZE(line_items, ze) =>
    let sline_items =
      line_items
      |> List.mapi((i, li) => snode_of_line_item(~steps=steps @ [i], li));
    let sze = snode_of_zexp(~steps=steps @ [List.length(line_items)], ze);
    mk_SBox(
      ~steps,
      ~shape=Block,
      ~is_multi_line=List.length(sline_items) != 0 || is_multi_line(sze),
      sline_items
      @ [sze]
      |> List.map(snode =>
           mk_sline(
             ~steps_of_first_sword=steps_of_snode(snode),
             [SNode(snode)],
           )
         ),
    );
  }
and snode_of_zline_item = (~steps: Path.steps, zli: ZExp.zline): snode =>
  switch (zli) {
  | CursorL(cursor, li) => snode_of_line_item(~cursor, ~steps, li)
  | ExpLineZ(ze) => snode_of_zexp(~steps, ze)
  | LetLineZP(zp, ann, def) =>
    let szp = snode_of_zpat(~steps=steps @ [0], zp);
    let sann =
      switch (ann) {
      | None => None
      | Some(ann) => Some(snode_of_typ(~steps=steps @ [1], ann))
      };
    let sdef = snode_of_block(~steps=steps @ [2], def);
    snode_of_LetLine(~steps, szp, sann, sdef);
  | LetLineZA(p, zann, def) =>
    let sp = snode_of_pat(~steps=steps @ [0], p);
    let szann = snode_of_ztyp(~steps=steps @ [1], zann);
    let sdef = snode_of_block(~steps=steps @ [2], def);
    snode_of_LetLine(~steps, sp, Some(szann), sdef);
  | LetLineZE(p, ann, zdef) =>
    let sp = snode_of_pat(~steps=steps @ [0], p);
    let sann =
      switch (ann) {
      | None => None
      | Some(ann) => Some(snode_of_typ(~steps=steps @ [1], ann))
      };
    let szdef = snode_of_zblock(~steps=steps @ [2], zdef);
    snode_of_LetLine(~steps, sp, sann, szdef);
  }
and snode_of_zexp = (~steps: Path.steps, ze: ZExp.t) =>
  switch (ze) {
  | CursorE(cursor, e) => snode_of_exp(~cursor, ~steps, e)
  | ParenthesizedZ(zbody) =>
    let szbody = snode_of_zblock(~steps=steps @ [0], zbody);
    snode_of_Parenthesized(~steps, szbody);
  | OpSeqZ(skel, ztm, surround) =>
    let (prefix_tms, suffix_tms) = OperatorSeq.tms_of_surround(surround);
    let (prefix_ops, suffix_ops) = OperatorSeq.ops_of_surround(surround);
    let sprefix =
      prefix_tms
      |> List.mapi((i, tm) => snode_of_exp(~steps=steps @ [i], tm));
    let sztm = snode_of_zexp(~steps=steps @ [List.length(prefix_tms)], ztm);
    let ssuffix =
      suffix_tms
      |> List.mapi((i, tm) =>
           snode_of_exp(
             ~steps=steps @ [i + List.length(prefix_tms) + 1],
             tm,
           )
         );
    let sops =
      prefix_ops
      @ suffix_ops
      |> List.mapi((i, op) => {
           let (space_before, space_after) = space_before_after_op_exp(op);
           (
             space_before,
             switch ((op: UHExp.op)) {
             | Space => SSpaceOp
             | _ => mk_SOp(~index=i + 1, string_of_op_exp(op))
             },
             space_after,
           );
         });
    let sskel = sskel_of_skel_exp(skel);
    snode_of_OpSeq(~steps, ~sskel, sprefix @ [sztm] @ ssuffix, sops);
  | LamZP(err_status, zarg, ann, body) =>
    let szarg = snode_of_zpat(~steps=steps @ [0], zarg);
    let sann =
      switch (ann) {
      | None => None
      | Some(ann) => Some(snode_of_typ(~steps=steps @ [1], ann))
      };
    let sbody = snode_of_block(~steps=steps @ [2], body);
    snode_of_Lam(~steps, ~err_status, szarg, sann, sbody);
  | LamZA(err_status, arg, zann, body) =>
    let sarg = snode_of_pat(~steps=steps @ [0], arg);
    let szann = snode_of_ztyp(~steps=steps @ [1], zann);
    let sbody = snode_of_block(~steps=steps @ [2], body);
    snode_of_Lam(~steps, ~err_status, sarg, Some(szann), sbody);
  | LamZE(err_status, arg, ann, zbody) =>
    let sarg = snode_of_pat(~steps=steps @ [0], arg);
    let sann =
      switch (ann) {
      | None => None
      | Some(ann) => Some(snode_of_typ(~steps=steps @ [1], ann))
      };
    let szbody = snode_of_zblock(~steps=steps @ [2], zbody);
    snode_of_Lam(~steps, ~err_status, sarg, sann, szbody);
  | InjZ(err_status, side, zbody) =>
    let szbody = snode_of_zblock(~steps=steps @ [0], zbody);
    snode_of_Inj(~err_status, ~steps, side, szbody);
  | CaseZE(err_status, zscrut, rules, ann) =>
    let szscrut = snode_of_zblock(~steps=steps @ [0], zscrut);
    let srules =
      rules
      |> List.mapi((i, rule) => snode_of_rule(~steps=steps @ [i + 1], rule));
    let sann =
      switch (ann) {
      | None => None
      | Some(ann) =>
        Some(snode_of_typ(~steps=steps @ [List.length(rules) + 1], ann))
      };
    snode_of_Case(~err_status, ~steps, szscrut, srules, sann);
  | CaseZR(err_status, scrut, (prefix, zrule, suffix), ann) =>
    let sscrut = snode_of_block(~steps=steps @ [0], scrut);
    let szrules =
      (
        prefix
        |> List.mapi((i, rule) =>
             snode_of_rule(~steps=steps @ [i + 1], rule)
           )
      )
      @ [snode_of_zrule(~steps=steps @ [List.length(prefix) + 1], zrule)]
      @ (
        suffix
        |> List.mapi((i, rule) =>
             snode_of_rule(
               ~steps=steps @ [i + List.length(prefix) + 2],
               rule,
             )
           )
      );
    let sann =
      switch (ann) {
      | None => None
      | Some(ann) =>
        Some(snode_of_typ(~steps=steps @ [List.length(szrules) + 1], ann))
      };
    snode_of_Case(~err_status, ~steps, sscrut, szrules, sann);
  | CaseZA(err_status, scrut, rules, zann) =>
    let sscrut = snode_of_block(~steps=steps @ [0], scrut);
    let srules =
      rules
      |> List.mapi((i, rule) => snode_of_rule(~steps=steps @ [i + 1], rule));
    let szann =
      snode_of_ztyp(~steps=steps @ [List.length(rules) + 1], zann);
    snode_of_Case(~err_status, ~steps, sscrut, srules, Some(szann));
  | ApPaletteZ(_, _, _, _) => raise(InvariantViolated)
  }
and snode_of_zrule = (~steps, zrule) =>
  switch (zrule) {
  | CursorR(cursor, rule) => snode_of_rule(~cursor, ~steps, rule)
  | RuleZP(zp, clause) =>
    let sp = snode_of_zpat(~steps=steps @ [0], zp);
    let sclause = snode_of_block(~steps=steps @ [1], clause);
    snode_of_Rule(~steps, sp, sclause);
  | RuleZE(p, zclause) =>
    let sp = snode_of_pat(~steps=steps @ [0], p);
    let sclause = snode_of_zblock(~steps=steps @ [1], zclause);
    snode_of_Rule(~steps, sp, sclause);
  };

let view_of_zblock =
    (~inject: Update.Action.t => Vdom.Event.t, zblock: ZExp.zblock)
    : Vdom.Node.t => {
  view_of_snode(~inject, snode_of_zblock(zblock));
};

let view_of_block =
    (~inject: Update.Action.t => Vdom.Event.t, block: UHExp.block)
    : Vdom.Node.t => {
  view_of_snode(~inject, snode_of_block(block));
};

module DHPat = Dynamics.DHPat;
module DHExp = Dynamics.DHExp;

let cls_of_inst = ((u, i)) =>
  "hole-instance-" ++ string_of_int(u) ++ "-" ++ string_of_int(i);

let maybe_parenthesize = (parenthesize, sline) =>
  parenthesize
    ? [SToken(mk_SDelim("("))] @ sline @ [SToken(mk_SDelim(")"))] : sline;

let hole_label_of = (u, i) =>
  string_of_int(u + 1) ++ ":" ++ string_of_int(i + 1);

let precedence_const = 0;
let precedence_Prod = 1;
let precedence_Sum = 2;
let precedence_Arrow = 3;
let precedence_ty = (ty: HTyp.t): int =>
  switch (ty) {
  | Num
  | Bool
  | Hole
  | Unit
  | List(_) => precedence_const
  | Prod(_, _) => precedence_Prod
  | Sum(_, _) => precedence_Sum
  | Arrow(_, _) => precedence_Arrow
  };

let precedence_Ap = 1;
let precedence_Times = 2;
let precedence_Plus = 3;
let precedence_Cons = 4;
let precedence_LessThan = 5;
let precedence_Comma = 6;
let precedence_max = 7;
let precedence_dhpat = (dp: DHPat.t) =>
  switch (dp) {
  | EmptyHole(_)
  | NonEmptyHole(_, _, _, _)
  | Wild
  | Keyword(_, _, _)
  | Var(_)
  | NumLit(_)
  | BoolLit(_)
  | Inj(_, _)
  | Triv
  | ListNil
  | Pair(_, _) => precedence_const
  | Cons(_, _) => precedence_Cons
  | Ap(_, _) => precedence_Ap
  };
let rec precedence_dhexp = (d: DHExp.t) =>
  switch (d) {
  | BoundVar(_)
  | FreeVar(_, _, _, _)
  | Keyword(_, _, _, _)
  | BoolLit(_)
  | NumLit(_)
  | ListNil(_)
  | Inj(_, _, _)
  | Pair(_, _)
  | EmptyHole(_, _, _)
  | Triv
  | FailedCast(_, _, _) => precedence_const
  | Cast(d1, _, _) => _SHOW_CASTS ? precedence_const : precedence_dhexp(d1)
  | Let(_, _, _)
  | FixF(_, _, _)
  | Lam(_, _, _)
  | Case(_, _, _) => precedence_max
  | Ap(_, _) => precedence_Ap
  | BinNumOp(Times, _, _) => precedence_Times
  | BinNumOp(Plus, _, _) => precedence_Plus
  | BinNumOp(LessThan, _, _) => precedence_LessThan
  | Cons(_, _) => precedence_Cons
  | NonEmptyHole(_, _, _, _, d1) => precedence_dhexp(d1)
  };

let snode_of_BinOp =
    (
      ~mb_par=maybe_parenthesize(false),
      ~err_status=NotInHole,
      ~steps,
      s1: snode,
      sop: string,
      s2: snode,
    )
    : snode =>
  mk_SBox(
    ~err_status,
    ~steps,
    ~shape=SkelBinOp,
    [mk_sline(mb_par([SNode(s1), SToken(mk_SDelim(sop)), SNode(s2)]))],
  );

let snode_of_SpaceOp = (~err_status, ~steps, s1: snode, s2: snode): snode =>
  mk_SBox(
    ~err_status,
    ~steps,
    ~shape=SkelBinOp,
    [mk_sline([SNode(s1), SToken(SSpaceOp), SNode(s2)])],
  );

let snode_of_Let =
    (~err_status, ~steps, sp: snode, sdef: snode, sbody: snode): snode =>
  mk_SBox(
    ~err_status,
    ~steps,
    ~shape=Let,
    ~is_multi_line=true,
    [
      mk_sline([
        SToken(mk_SDelim("let")),
        SNode(sp),
        SToken(mk_SDelim("=")),
      ]),
      mk_sline(~rel_indent=1, [SNode(sdef)]),
      mk_sline([SNode(sbody)]),
    ],
  );

let snode_of_FixF =
    (~err_status, ~steps, sarg: snode, sty: snode, sbody: snode): snode =>
  mk_SBox(
    ~err_status,
    ~steps,
    ~shape=FixF,
    ~is_multi_line=is_multi_line(sbody),
    [
      mk_sline([
        SToken(mk_SDelim("fix")),
        SNode(sarg),
        SToken(mk_SDelim(":")),
        SNode(sty),
        SToken(mk_SDelim(".")),
      ]),
      mk_sline(~rel_indent=1, [SNode(sbody)]),
    ],
  );

let rec snode_of_htyp =
        (~parenthesize=false, ~steps: Path.steps=[], ty: HTyp.t): snode => {
  let mb_par = maybe_parenthesize(parenthesize);
  switch (ty) {
  | Hole => snode_of_EmptyHole(~steps, "?")
  | Bool => snode_of_Bool(~steps, ())
  | Num => snode_of_Num(~steps, ())
  | Unit => snode_of_Unit(~steps, ())
  | List(ty1) =>
    let sty1 = snode_of_htyp(~steps=steps @ [0], ty1);
    snode_of_List(~steps, sty1);
  | Arrow(ty1, ty2) =>
    let sty1 =
      snode_of_htyp(
        ~parenthesize=precedence_ty(ty1) >= precedence_Arrow,
        ~steps=steps @ [0],
        ty1,
      );
    let sty2 =
      snode_of_htyp(
        ~parenthesize=precedence_ty(ty2) > precedence_Arrow,
        ~steps=steps @ [1],
        ty2,
      );
    snode_of_BinOp(~mb_par, ~steps, sty1, string_of_op_typ(Arrow), sty2);
  | Sum(ty1, ty2) =>
    let sty1 =
      snode_of_htyp(
        ~parenthesize=precedence_ty(ty1) >= precedence_Sum,
        ~steps=steps @ [0],
        ty1,
      );
    let sty2 =
      snode_of_htyp(
        ~parenthesize=precedence_ty(ty2) > precedence_Sum,
        ~steps=steps @ [1],
        ty2,
      );
    snode_of_BinOp(~mb_par, ~steps, sty1, string_of_op_typ(Sum), sty2);
  | Prod(ty1, ty2) =>
    let sty1 =
      snode_of_htyp(
        ~parenthesize=precedence_ty(ty1) >= precedence_Prod,
        ~steps=steps @ [0],
        ty1,
      );
    let sty2 =
      snode_of_htyp(
        ~parenthesize=precedence_ty(ty2) > precedence_Prod,
        ~steps=steps @ [1],
        ty2,
      );
    snode_of_BinOp(~mb_par, ~steps, sty1, string_of_op_typ(Prod), sty2);
  };
};

let rec snode_of_dhpat =
        (~parenthesize=false, ~err_status=NotInHole, ~steps=[], dp: DHPat.t)
        : snode => {
  let mb_par = maybe_parenthesize(parenthesize);
  switch (dp) {
  | EmptyHole(u, i) =>
    /* TODO add SHOW_SIGMAS flag */
    mk_SBox(
      ~steps,
      ~shape=EmptyHoleInstance(u, i, None),
      [mk_sline(mb_par([SToken(SEmptyHole(hole_label_of(u, i)))]))],
    )
  | NonEmptyHole(reason, u, i, dp1) =>
    /* TODO revisit this and consider whether err_status should be on sp1 or parent */
    let sp1 =
      snode_of_dhpat(
        ~err_status=InHole(reason, u),
        ~steps=steps @ [0],
        dp1,
      );
    mk_SBox(
      ~steps,
      ~shape=NonEmptyHoleInstance(reason, u, i, None),
      [mk_sline(mb_par([SNode(sp1)]))],
    );
  | Wild =>
    mk_SBox(
      ~err_status,
      ~steps,
      ~shape=Wild,
      [mk_sline([SToken(mk_SDelim(~index=0, "_"))])],
    )
  | Keyword(u, _, k) =>
    snode_of_Var(
      ~err_status,
      ~var_err_status=InVHole(Keyword(k), u),
      ~steps,
      Var.of_keyword(k),
    )
  | Var(x) => snode_of_Var(~err_status, ~var_err_status=NotInVHole, ~steps, x)
  | BoolLit(b) => snode_of_BoolLit(~err_status, ~steps, b)
  | NumLit(n) => snode_of_NumLit(~err_status, ~steps, n)
  | Triv => snode_of_Triv(~err_status, ~steps)
  | ListNil => snode_of_ListNil(~err_status, ~steps, ())
  | Inj(side, dp1) =>
    let sp1 = snode_of_dhpat(~steps=steps @ [0], dp1);
    snode_of_Inj(~err_status, ~steps, side, sp1);
  | Cons(dp1, dp2) =>
    let sp1 =
      snode_of_dhpat(
        ~parenthesize=precedence_dhpat(dp1) > precedence_Cons,
        ~steps=steps @ [0],
        dp1,
      );
    let sp2 =
      snode_of_dhpat(
        ~parenthesize=precedence_dhpat(dp2) >= precedence_Cons,
        ~steps=steps @ [1],
        dp2,
      );
    snode_of_BinOp(~err_status, ~steps, sp1, string_of_op_pat(Cons), sp2);
  | Pair(dp1, dp2) =>
    let sp1 = snode_of_dhpat(~steps=steps @ [0], dp1);
    let sp2 = snode_of_dhpat(~steps=steps @ [1], dp2);
    snode_of_BinOp(~err_status, ~steps, sp1, string_of_op_pat(Comma), sp2);
  | Ap(dp1, dp2) =>
    let sp1 =
      snode_of_dhpat(
        ~parenthesize=precedence_dhpat(dp1) > precedence_Ap,
        ~steps=steps @ [0],
        dp1,
      );
    let sp2 =
      snode_of_dhpat(
        ~parenthesize=precedence_dhpat(dp2) >= precedence_Ap,
        ~steps=steps @ [1],
        dp2,
      );
    snode_of_SpaceOp(~err_status, ~steps, sp1, sp2);
  };
};

let rec snode_of_dhexp =
        (~parenthesize=false, ~err_status=NotInHole, ~steps=[], d: DHExp.t)
        : snode => {
  let mb_par = maybe_parenthesize(parenthesize);
  switch (d) {
  | EmptyHole(u, i, sigma) =>
    /* TODO add SHOW_SIGMAS flag */
    mk_SBox(
      ~steps,
      ~shape=EmptyHoleInstance(u, i, Some(sigma)),
      [mk_sline(mb_par([SToken(SEmptyHole(hole_label_of(u, i)))]))],
    )
  | NonEmptyHole(reason, u, i, sigma, d1) =>
    let s1 =
      snode_of_dhexp(~err_status=InHole(reason, u), ~steps=steps @ [0], d1);
    /* TODO add SHOW_SIGMAS flag */
    mk_SBox(
      ~steps,
      ~shape=NonEmptyHoleInstance(reason, u, i, Some(sigma)),
      [mk_sline(mb_par([SNode(s1)]))],
    );
  | Triv => snode_of_Triv(~err_status, ~steps)
  | BoolLit(b) => snode_of_BoolLit(~err_status, ~steps, b)
  | NumLit(n) => snode_of_NumLit(~err_status, ~steps, n)
  | ListNil(_) => snode_of_ListNil(~err_status, ~steps, ())
  | BoundVar(x) =>
    snode_of_Var(~err_status, ~var_err_status=NotInVHole, ~steps, x)
  | FreeVar(u, _, _, x) =>
    snode_of_Var(~err_status, ~var_err_status=InVHole(Free, u), ~steps, x)
  | Keyword(u, _, _, k) =>
    snode_of_Var(
      ~err_status,
      ~var_err_status=InVHole(Keyword(k), u),
      ~steps,
      Var.of_keyword(k),
    )
  | Let(dp, ddef, dbody) =>
    let sp = snode_of_dhpat(~steps=steps @ [0], dp);
    let sdef = snode_of_dhexp(~steps=steps @ [1], ddef);
    let sbody = snode_of_dhexp(~steps=steps @ [2], dbody);
    snode_of_Let(~err_status, ~steps, sp, sdef, sbody);
  | FixF(x, ty, dbody) =>
    let sx =
      snode_of_Var(
        ~err_status=NotInHole,
        ~var_err_status=NotInVHole,
        ~steps=steps @ [0],
        x,
      );
    let sty = snode_of_htyp(~steps=steps @ [1], ty);
    let sbody = snode_of_dhexp(~steps=steps @ [2], dbody);
    snode_of_FixF(~err_status, ~steps, sx, sty, sbody);
  | Lam(darg, dann, dbody) =>
    let sarg = snode_of_dhpat(~steps=steps @ [0], darg);
    let sann = snode_of_htyp(~steps=steps @ [1], dann);
    let sbody = snode_of_dhexp(~steps=steps @ [2], dbody);
    snode_of_Lam(~err_status, ~steps, sarg, Some(sann), sbody);
  | Inj(ty, side, dbody) =>
    let sty = snode_of_htyp(~steps=steps @ [0], ty);
    let sbody = snode_of_dhexp(~steps=steps @ [1], dbody);
    snode_of_InjAnn(~err_status, ~steps, sty, side, sbody);
  | Case(dscrut, drules, _) =>
    /* TODO: probably need to do something with current rule */
    /* | Case(d1, (x, d2), (y, d3)) => */
    let sscrut = snode_of_dhexp(~steps=steps @ [0], dscrut);
    let srules =
      drules
      |> List.mapi((i, drule) =>
           snode_of_drule(~steps=steps @ [i + 1], drule)
         );
    snode_of_Case(~err_status, ~steps, sscrut, srules, None);
  | BinNumOp(dop, d1, d2) =>
    let sop = string_of_op_exp(DHExp.to_op(dop));
    let s1 =
      snode_of_dhexp(
        ~parenthesize=precedence_dhexp(d1) > precedence_dhexp(d),
        ~steps=steps @ [0],
        d1,
      );
    let s2 =
      snode_of_dhexp(
        ~parenthesize=precedence_dhexp(d2) >= precedence_dhexp(d),
        ~steps=steps @ [1],
        d2,
      );
    snode_of_BinOp(~err_status, ~steps, s1, sop, s2);
  | Ap(d1, d2) =>
    let s1 =
      snode_of_dhexp(
        ~parenthesize=precedence_dhexp(d1) > precedence_Ap,
        ~steps=steps @ [0],
        d1,
      );
    let s2 =
      snode_of_dhexp(
        ~parenthesize=precedence_dhexp(d2) >= precedence_Ap,
        ~steps=steps @ [1],
        d2,
      );
    snode_of_SpaceOp(~err_status, ~steps, s1, s2);
  | Pair(d1, d2) =>
    let s1 = snode_of_dhexp(~steps=steps @ [0], d1);
    let s2 = snode_of_dhexp(~steps=steps @ [1], d2);
    snode_of_BinOp(~err_status, ~steps, s1, string_of_op_exp(Comma), s2);
  | Cons(d1, d2) =>
    let s1 = snode_of_dhexp(~steps=steps @ [0], d1);
    let s2 = snode_of_dhexp(~steps=steps @ [1], d2);
    snode_of_BinOp(~err_status, ~steps, s1, string_of_op_exp(Cons), s2);
  | Cast(Cast(d1, ty1, ty2), ty2', ty3) when HTyp.eq(ty2, ty2') =>
    let s1 =
      snode_of_dhexp(
        ~parenthesize=precedence_dhexp(d1) > precedence_const,
        ~steps,
        d1,
      );
    let sty1 = snode_of_htyp(~steps=steps @ [0, 1], ty1);
    let sty2 = snode_of_htyp(~steps=steps @ [0, 2], ty2);
    let sty3 = snode_of_htyp(~steps=steps @ [2], ty3);
    mk_SBox(
      ~err_status,
      ~steps,
      ~shape=Cast,
      _SHOW_CASTS
        ? [
          mk_sline([
            SNode(s1),
            SToken(mk_SDelim("<")),
            SNode(sty1),
            SToken(SCastArrow),
            SNode(sty2),
            SToken(SCastArrow),
            SNode(sty3),
            SToken(mk_SDelim(">")),
          ]),
        ]
        : [mk_sline([SNode(s1)])],
    );
  | Cast(d1, ty1, ty2) =>
    let s1 =
      snode_of_dhexp(
        ~parenthesize=precedence_dhexp(d1) > precedence_const,
        ~steps=steps @ [0],
        d1,
      );
    let sty1 = snode_of_htyp(~steps=steps @ [1], ty1);
    let sty2 = snode_of_htyp(~steps=steps @ [2], ty2);
    mk_SBox(
      ~err_status,
      ~steps,
      ~shape=Cast,
      _SHOW_CASTS
        ? [
          mk_sline([
            SNode(s1),
            SToken(mk_SDelim("<")),
            SNode(sty1),
            SToken(SCastArrow),
            SNode(sty2),
            SToken(mk_SDelim(">")),
          ]),
        ]
        : [mk_sline([SNode(s1)])],
    );
  | FailedCast(Cast(d1, ty1, ty2), ty2', ty3) when HTyp.eq(ty2, ty2') =>
    let s1 =
      snode_of_dhexp(
        ~parenthesize=precedence_dhexp(d1) > precedence_const,
        ~steps,
        d1,
      );
    let sty1 = snode_of_htyp(~steps=steps @ [0, 1], ty1);
    let sty2 = snode_of_htyp(~steps=steps @ [0, 2], ty2);
    let sty3 = snode_of_htyp(~steps=steps @ [2], ty3);
    mk_SBox(
      ~err_status,
      ~steps,
      ~shape=FailedCast,
      [
        mk_sline([
          SNode(s1),
          SToken(mk_SDelim("<")),
          SNode(sty1),
          SToken(SFailedCastArrow),
          SNode(sty2),
          SToken(SFailedCastArrow),
          SNode(sty3),
          SToken(mk_SDelim(">")),
        ]),
      ],
    );
  | FailedCast(d1, ty1, ty2) =>
    let s1 =
      snode_of_dhexp(
        ~parenthesize=precedence_dhexp(d1) > precedence_const,
        ~steps=steps @ [0],
        d1,
      );
    let sty1 = snode_of_htyp(~steps=steps @ [1], ty1);
    let sty2 = snode_of_htyp(~steps=steps @ [2], ty2);
    mk_SBox(
      ~err_status,
      ~steps,
      ~shape=FailedCast,
      [
        mk_sline([
          SNode(s1),
          SToken(mk_SDelim("<")),
          SNode(sty1),
          SToken(SFailedCastArrow),
          SNode(sty2),
          SToken(mk_SDelim(">")),
        ]),
      ],
    );
  };
}
and snode_of_drule = (~steps, Rule(dp, dclause): DHExp.rule): snode => {
  let sp = snode_of_dhpat(~steps=steps @ [0], dp);
  let sclause = snode_of_dhexp(~steps=steps @ [1], dclause);
  snode_of_Rule(~steps, sp, sclause);
};

let view_of_htyp =
    (~inject: Update.Action.t => Vdom.Event.t, ty: HTyp.t): Vdom.Node.t =>
  view_of_snode(~inject, snode_of_htyp(ty));

let view_of_dhexp = (~inject, d) =>
  view_of_snode(~inject, snode_of_dhexp(d));

let view_of_result =
    (~inject: Update.Action.t => Vdom.Event.t, model: Model.t): Vdom.Node.t =>
  switch (model.result) {
  | (_, _, InvalidInput(_)) =>
    Vdom.Node.div(
      [],
      [
        Vdom.Node.text(
          "(internal error: expansion or evaluation invariant violated)",
        ),
      ],
    )
  | (_, _, BoxedValue(d))
  | (_, _, Indet(d)) => Vdom.Node.div([], [view_of_dhexp(~inject, d)])
  };

let view_of_hole_instance =
    (~inject: Update.Action.t => Vdom.Event.t, (u, i): DHExp.HoleInstance.t) =>
  view_of_dhexp(~inject, EmptyHole(u, i, []));

let view_of_Var =
    (
      ~inject,
      ~err_status=NotInHole,
      ~var_err_status=NotInVHole,
      ~steps=[],
      x: Var.t,
    ) =>
  view_of_snode(
    ~inject,
    snode_of_Var(~err_status, ~var_err_status, ~steps, x),
  );
