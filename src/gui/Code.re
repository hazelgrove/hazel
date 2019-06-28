let _SHOW_CASTS = false;
let _SHOW_FN_BODIES = false;

module Js = Js_of_ocaml.Js;
module Dom = Js_of_ocaml.Dom;
module Dom_html = Js_of_ocaml.Dom_html;
module Vdom = Virtual_dom.Vdom;
open SemanticsCommon;
open ViewUtil;

exception InvariantViolated;

type id = string;
type cls = string;
type is_multi_line = bool;

type snode_shape =
  | Seq
  | Box(sbox_shape)
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

type snode =
  | SSeq(
      Path.steps,
      option(cursor_position),
      is_multi_line,
      sskel,
      sseq_head,
      sseq_tail,
    )
  | SBox(
      Path.steps,
      option(cursor_position),
      is_multi_line,
      err_status,
      sbox_shape,
      list(sline),
    )
and sseq_head = snode
and sseq_tail = list((stoken, snode))
and sskel =
  | Placeholder(int)
  | BinOp(err_status, string, sskel, sskel)
  | Space(err_status, sskel, sskel)
and sline = list(sword)
and sword =
  | SNode(snode)
  | SToken(stoken)
and stoken =
  | SEmptyHole(string)
  | SDelim(option(delim_index), string)
  | SSpaceOp
  | SOp(option(op_index), string)
  | SText(var_err_status, string)
  | SCastArrow
  | SFailedCastArrow
  | SEmptyLine;

let is_multi_line =
  fun
  | SSeq(_, _, is_multi_line, _, _, _)
  | SBox(_, _, is_multi_line, _, _, _) => is_multi_line;

let mk_SSeq =
    (
      ~cursor: option(cursor_position)=?,
      ~is_multi_line=false,
      ~steps: Path.steps,
      ~sskel: sskel,
      (shead, stail): (sseq_head, sseq_tail),
    )
    : snode =>
  SSeq(steps, cursor, is_multi_line, sskel, shead, stail);

let mk_SBox =
    (
      ~cursor: option(cursor_position)=?,
      ~is_multi_line=false,
      ~err_status=NotInHole,
      ~steps: Path.steps,
      ~shape: sbox_shape,
      slines: list(sline),
    )
    : snode => {
  SBox(steps, cursor, is_multi_line, err_status, shape, slines);
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
  | Space => ""
  | Cons => "::";

let string_of_op_exp: UHExp.op => string =
  fun
  | Plus => "+"
  | Times => "*"
  | LessThan => "<"
  | Space => ""
  | Comma => ","
  | Cons => "::";

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

let snode_attrs =
    (~inject: Update.Action.t => Vdom.Event.t, snode: snode)
    : list(Vdom.Attr.t) => {
  Vdom.(
    switch (snode) {
    | SSeq(steps, cursor, is_multi_line, _sskel, _shead, _stail) => [
        Attr.id(node_id(steps)),
        Attr.classes(
          ["OpSeq"] @ cursor_clss(cursor) @ multi_line_clss(is_multi_line),
        ),
      ]
    | SBox(steps, cursor, is_multi_line, _err_status, shape, _) =>
      let base_clss =
        ["SNode", inline_div_cls]
        @ cursor_clss(cursor)
        @ multi_line_clss(is_multi_line);
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
      [Attr.id(node_id(steps)), ...shape_attrs];
    }
  );
};

let sline_clss = line_no => ["SLine", "SLine-" ++ string_of_int(line_no)];

type is_start_of_top = bool;
type is_end_of_bottom = bool;
type sline_border_style =
  | NoBorder
  | Top(is_start_of_top)
  | Bottom(is_end_of_bottom)
  | TopBottom(is_start_of_top, is_end_of_bottom);

let sline_border_clss: sline_border_style => list(string) =
  fun
  | NoBorder => []
  | _ => [];

let var_err_status_clss =
  fun
  | NotInVHole => []
  | InVHole(Free, u) => ["InVHole", "InVHole_" ++ string_of_int(u)]
  | InVHole(Keyword(_), u) => [
      "InVHole",
      "InVHole_" ++ string_of_int(u),
      "Keyword",
    ];

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

/* TODO */
let range_of_tree_rooted_at_cursor = (_cursor, _sskel) => (0, 0);

let rec view_of_snode =
        (~inject: Update.Action.t => Vdom.Event.t, snode): Vdom.Node.t => {
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
            ~line_no=0,
            [SNode(shead)],
          );
        let vtail =
          stail
          |> List.mapi((i, (sop, stm)) =>
               view_of_sline(
                 ~inject,
                 ~node_steps=steps,
                 ~line_no=i + 1,
                 [SToken(sop), SNode(stm)],
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
            ~border_style=vhead_border_style,
            ~line_no=0,
            [SNode(shead)],
          );
        let vtail =
          stail
          |> List.mapi((i, (sop, stm)) => {
               let border_style =
                 i + 1 < b
                   ? TopBottom(false, false) : TopBottom(false, true);
               view_of_sline(
                 ~inject,
                 ~node_steps=steps,
                 ~node_cursor=cursor,
                 ~border_style,
                 ~line_no=i + 1,
                 [SToken(sop), SNode(stm)],
               );
             });
        (vhead, vtail);
      };
    let line_break = is_multi_line ? [Vdom.Node.br([])] : [];
    let lines =
      [vhead]
      @ line_break
      @ List.fold_right(
          (vtail_line, children_so_far) =>
            [vtail_line] @ line_break @ children_so_far,
          vtail,
          [],
        );
    Vdom.Node.div(attrs, lines);
  | SBox(steps, node_cursor, is_multi_line, _, _, slines) =>
    /* TODO add border style */
    let vlines: list(Vdom.Node.t) =
      slines
      |> List.mapi((i, sline) =>
           view_of_sline(
             ~inject,
             ~node_steps=steps,
             ~node_cursor?,
             ~line_no=i,
             sline,
           )
         );
    let line_break = is_multi_line ? [Vdom.Node.br([])] : [];
    let lines =
      List.fold_right(
        (vline, children_so_far) => [vline] @ line_break @ children_so_far,
        vlines,
        [],
      );
    Vdom.Node.div(attrs, lines);
  };
}
and view_of_sline =
    (
      ~inject: Update.Action.t => Vdom.Event.t,
      ~node_steps: Path.steps,
      ~node_cursor: option(cursor_position)=?,
      ~border_style: sline_border_style=NoBorder,
      ~line_no: int,
      sline,
    )
    : Vdom.Node.t =>
  Vdom.(
    Node.div(
      [
        Attr.classes(
          [inline_div_cls]
          @ sline_clss(line_no)
          @ sline_border_clss(border_style),
        ),
      ],
      sline
      |> List.map(sword =>
           switch (sword) {
           | SNode(snode) => view_of_snode(~inject, snode)
           | SToken(stoken) =>
             view_of_stoken(~inject, ~node_steps, ~node_cursor, stoken)
           }
         ),
    )
  )
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
          Attr.classes(["SEmptyHole-before"]),
        ],
        [],
      );
    let hole_after =
      Node.span(
        [
          Attr.id(path_id((node_steps, OnDelim(0, After)))),
          Attr.classes(["SEmptyHole-after"]),
        ],
        [],
      );
    let hole_lbl =
      Node.span(
        [
          Attr.create("contenteditable", "false"),
          Attr.classes(["SEmptyHole-lbl", "not-editable"]),
          Attr.on_click(on_click_noneditable(~inject, node_steps, 0)),
        ],
        [Node.text(lbl)],
      );
    Node.div(
      [Attr.classes([inline_div_cls, "SEmptyHole"])],
      [hole_before, hole_lbl, hole_after],
    );
  | SDelim(delim_index, s) =>
    open Vdom;
    let (delim_before_nodes, delim_after_nodes, on_click_txt_attrs) =
      switch (delim_index) {
      | None => ([], [], [])
      | Some(k) =>
        let delim_before =
          Node.span(
            [
              Attr.id(path_id((node_steps, OnDelim(k, Before)))),
              Attr.classes(["SDelim-before"]),
            ],
            [],
          );
        let delim_after =
          Node.span(
            [
              Attr.id(path_id((node_steps, OnDelim(k, After)))),
              Attr.classes(["SDelim-after"]),
            ],
            [],
          );
        let on_click_txt =
          Attr.on_click(on_click_noneditable(~inject, node_steps, k));
        ([delim_before], [delim_after], [on_click_txt]);
      };
    let delim_txt =
      Node.span(
        [
          Attr.create("contenteditable", "false"),
          Attr.classes(["SDelim-txt", "not-editable"]),
        ]
        @ on_click_txt_attrs,
        [Node.text(s)],
      );
    Node.div(
      [Attr.classes([inline_div_cls, "SDelim"])],
      delim_before_nodes @ [delim_txt] @ delim_after_nodes,
    );
  | SOp(op_index, s) =>
    open Vdom;
    let (op_before_nodes, op_after_nodes, on_click_txt_attrs) =
      switch (op_index) {
      | None => ([], [], [])
      | Some(k) =>
        let op_before =
          Node.span(
            [
              Attr.id(path_id((node_steps, OnDelim(k, Before)))),
              Attr.classes(["SOp-before"]),
            ],
            [],
          );
        let op_after =
          Node.span(
            [
              Attr.id(path_id((node_steps, OnDelim(k, After)))),
              Attr.classes(["SOp-after"]),
            ],
            [],
          );
        let on_click_txt =
          Attr.on_click(on_click_noneditable(~inject, node_steps, k));
        ([op_before], [op_after], [on_click_txt]);
      };
    let op_txt =
      Node.span(
        [
          Attr.create("contenteditable", "false"),
          Attr.classes(["SOp-txt", "not-editable"]),
        ]
        @ on_click_txt_attrs,
        [Node.text(s)],
      );
    Node.div(
      [Attr.classes([inline_div_cls, "SOp"])],
      op_before_nodes @ [op_txt] @ op_after_nodes,
    );
  | SSpaceOp =>
    Vdom.(Node.div([Attr.classes([inline_div_cls, "SSpaceOp"])], []))
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
          Attr.id(path_id((node_steps, OnText(0)))),
          Attr.classes([inline_div_cls, "SEmptyLine"]),
        ],
        [],
      )
    )
  };

let snode_of_EmptyHole = (~cursor=?, ~steps, hole_name: string): snode =>
  mk_SBox(
    ~cursor?,
    ~steps,
    ~shape=EmptyHole,
    [[SToken(SEmptyHole(hole_name))]],
  );

let snode_of_Var =
    (~cursor=?, ~err_status, ~var_err_status, ~steps, x: Var.t): snode =>
  mk_SBox(
    ~cursor?,
    ~err_status,
    ~steps,
    ~shape=Var,
    [[SToken(mk_SText(~var_err_status, x))]],
  );

let snode_of_NumLit = (~cursor=?, ~err_status, ~steps, n: int): snode =>
  mk_SBox(
    ~cursor?,
    ~err_status,
    ~steps,
    ~shape=NumLit,
    [[SToken(mk_SText(string_of_int(n)))]],
  );

let snode_of_BoolLit = (~cursor=?, ~err_status, ~steps, b: bool): snode =>
  mk_SBox(
    ~cursor?,
    ~err_status,
    ~steps,
    ~shape=BoolLit,
    [[SToken(mk_SText(string_of_bool(b)))]],
  );

let snode_of_ListNil = (~cursor=?, ~err_status, ~steps, ()): snode =>
  mk_SBox(
    ~cursor?,
    ~err_status,
    ~steps,
    ~shape=ListNil,
    [[SToken(mk_SDelim(~index=0, "[]"))]],
  );

let snode_of_LetLine =
    (~cursor=?, ~steps, sp: snode, sann: option(snode), sdef: snode) =>
  mk_SBox(
    ~cursor?,
    ~steps,
    ~shape=LetLine,
    ~is_multi_line=is_multi_line(sdef),
    [
      [SToken(mk_SDelim(~index=0, "let")), SNode(sp)]
      @ (
        switch (sann) {
        | None => []
        | Some(sann) => [SToken(mk_SDelim(~index=1, ":")), SNode(sann)]
        }
      )
      @ [SToken(mk_SDelim(~index=2, "="))],
      [SNode(sdef)],
    ],
  );

let snode_of_Parenthesized = (~cursor=?, ~steps, sbody: snode): snode =>
  mk_SBox(
    ~cursor?,
    ~steps,
    ~shape=Parenthesized,
    ~is_multi_line=is_multi_line(sbody),
    [
      [SToken(mk_SDelim(~index=0, "("))],
      [SNode(sbody)],
      [SToken(mk_SDelim(~index=1, ")"))],
    ],
  );

let snode_of_List = (~cursor=?, ~steps, sbody: snode): snode =>
  mk_SBox(
    ~cursor?,
    ~steps,
    ~shape=List,
    [
      [SToken(mk_SDelim(~index=0, "List("))],
      [SNode(sbody)],
      [SToken(mk_SDelim(~index=1, ")"))],
    ],
  );

let snode_of_OpSeq =
    (~cursor=?, ~steps, ~sskel, stms: list(snode), sops: list(stoken))
    : snode => {
  let (shead, srest) =
    switch (stms) {
    | [] => assert(false)
    | [hd, ...rst] => (hd, rst)
    };
  mk_SSeq(
    ~cursor?,
    ~is_multi_line=stms |> List.exists(is_multi_line),
    ~steps,
    ~sskel,
    (shead, List.combine(sops, srest)),
  );
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
      [SToken(mk_SDelim(~index=0, LangUtil.lamSym)), SNode(sarg)]
      @ swords_ann
      @ [SToken(mk_SDelim(~index=2, "."))],
      [SNode(sbody)],
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
      [
        SToken(
          mk_SDelim(
            ~index=0,
            "inj[" ++ LangUtil.string_of_side(side) ++ "](",
          ),
        ),
      ],
      [SNode(sbody)],
      [SToken(mk_SDelim(~index=1, ")"))],
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
      [
        SToken(mk_SDelim("inj[" ++ LangUtil.string_of_side(side) ++ ",")),
        SNode(sty),
        SToken(mk_SDelim("](")),
      ],
      [SNode(sbody)],
      [SToken(mk_SDelim(")"))],
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
  let slines_rules = srules |> List.map(snode => [SNode(snode)]);
  let swords_end =
    switch (sann) {
    | None => [SToken(mk_SDelim(~index=1, "end"))]
    | Some(sann) => [SToken(mk_SDelim(~index=1, "end :")), SNode(sann)]
    };
  mk_SBox(
    ~cursor?,
    ~err_status,
    ~steps,
    ~shape=Case,
    ~is_multi_line=true,
    [[SToken(mk_SDelim(~index=0, "case")), SNode(sscrut)]]
    @ slines_rules
    @ [swords_end],
  );
};

let snode_of_Rule = (~cursor=?, ~steps, sp: snode, sclause: snode) =>
  mk_SBox(
    ~cursor?,
    ~steps,
    ~shape=Rule,
    ~is_multi_line=is_multi_line(sclause),
    [
      [
        SToken(mk_SDelim(~index=0, "|")),
        SNode(sp),
        SToken(mk_SDelim(~index=1, LangUtil.caseArrowSym)),
      ],
      [SNode(sclause)],
    ],
  );

let snode_of_Triv = (~err_status, ~steps) =>
  mk_SBox(
    ~err_status,
    ~steps,
    ~shape=Triv,
    [[SToken(mk_SDelim(~index=0, "()"))]],
  );

let snode_of_Bool = (~cursor=?, ~steps, ()) =>
  mk_SBox(
    ~cursor?,
    ~steps,
    ~shape=Bool,
    [[SToken(mk_SDelim(~index=0, "Bool"))]],
  );

let snode_of_Num = (~cursor=?, ~steps, ()) =>
  mk_SBox(
    ~cursor?,
    ~steps,
    ~shape=Num,
    [[SToken(mk_SDelim(~index=0, "Num"))]],
  );

let snode_of_Unit = (~cursor=?, ~steps, ()) =>
  mk_SBox(
    ~cursor?,
    ~steps,
    ~shape=Unit,
    [[SToken(mk_SDelim(~index=0, "()"))]],
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
      |> List.mapi((i, op) => mk_SOp(~index=i + 1, string_of_op_typ(op)));
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
      [[SToken(mk_SDelim(~index=0, "_"))]],
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
      |> List.mapi((i, op) =>
           switch ((op: UHPat.op)) {
           | Space => SSpaceOp
           | _ => mk_SOp(~index=i + 1, string_of_op_pat(op))
           }
         );
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
    sline_items @ [se] |> List.map(snode => [SNode(snode)]),
  );
}
and snode_of_line_item =
    (~cursor=?, ~steps: Path.steps, li: UHExp.line): snode =>
  switch (li) {
  | EmptyLine =>
    mk_SBox(~cursor?, ~steps, ~shape=EmptyLine, [[SToken(SEmptyLine)]])
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
    snode_of_Lam(~steps, ~err_status, sarg, sann, sbody);
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
      |> List.mapi((i, op) =>
           switch ((op: UHExp.op)) {
           | Space => SSpaceOp
           | _ => mk_SOp(~index=i + 1, string_of_op_exp(op))
           }
         );
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
      |> List.mapi((i, op) => mk_SOp(~index=i + 1, string_of_op_typ(op)));
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
      |> List.mapi((i, op) =>
           switch ((op: UHPat.op)) {
           | Space => SSpaceOp
           | _ => mk_SOp(~index=i + 1, string_of_op_pat(op))
           }
         );
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
      |> List.map(snode => [SNode(snode)]),
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
      sline_items @ [sze] |> List.map(snode => [SNode(snode)]),
    );
  }
and snode_of_zline_item = (~steps: Path.steps, zli: ZExp.zline): snode =>
  switch (zli) {
  | CursorL(cursor, li) => snode_of_line_item(~cursor, ~steps, li)
  | ExpLineZ(ze) => snode_of_zexp(~steps, ze)
  | LetLineZP(zp, ann, def) =>
    let szp = snode_of_zpat(~steps=steps @ [0], zp);
    let swords_ann =
      switch (ann) {
      | None => []
      | Some(uty) => [
          SToken(mk_SDelim(~index=1, ":")),
          SNode(snode_of_typ(~steps=steps @ [1], uty)),
        ]
      };
    let sdef = snode_of_block(~steps=steps @ [2], def);
    mk_SBox(
      ~steps,
      ~shape=LetLine,
      ~is_multi_line=is_multi_line(sdef),
      [
        [SToken(mk_SDelim(~index=0, "let")), SNode(szp)]
        @ swords_ann
        @ [SToken(mk_SDelim(~index=2, "="))],
        [SNode(sdef)],
      ],
    );
  | LetLineZA(p, zann, def) =>
    let sp = snode_of_pat(~steps=steps @ [0], p);
    let szann = snode_of_ztyp(~steps=steps @ [1], zann);
    let sdef = snode_of_block(~steps=steps @ [2], def);
    mk_SBox(
      ~steps,
      ~shape=LetLine,
      ~is_multi_line=is_multi_line(sdef),
      [
        [SToken(mk_SDelim(~index=0, "let")), SNode(sp)]
        @ [SToken(mk_SDelim(~index=1, ":")), SNode(szann)]
        @ [SToken(mk_SDelim(~index=2, "="))],
        [SNode(sdef)],
      ],
    );
  | LetLineZE(p, ann, zdef) =>
    let sp = snode_of_pat(~steps=steps @ [0], p);
    let swords_ann =
      switch (ann) {
      | None => []
      | Some(uty) => [
          SToken(mk_SDelim(~index=1, ":")),
          SNode(snode_of_typ(~steps=steps @ [1], uty)),
        ]
      };
    let szdef = snode_of_zblock(~steps=steps @ [2], zdef);
    mk_SBox(
      ~steps,
      ~shape=LetLine,
      ~is_multi_line=is_multi_line(szdef),
      [
        [SToken(mk_SDelim(~index=0, "let")), SNode(sp)]
        @ swords_ann
        @ [SToken(mk_SDelim(~index=2, "="))],
        [SNode(szdef)],
      ],
    );
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
      |> List.mapi((i, op) =>
           switch ((op: UHExp.op)) {
           | Space => SSpaceOp
           | _ => mk_SOp(~index=i + 1, string_of_op_exp(op))
           }
         );
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
      @ [snode_of_zrule(~steps=steps @ [List.length(prefix)], zrule)]
      @ (
        suffix
        |> List.mapi((i, rule) =>
             snode_of_rule(
               ~steps=steps @ [i + List.length(prefix) + 1],
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
    (~inject: Update.Action.t => Vdom.Event.t, model: Model.t): Vdom.Node.t => {
  let (zblock, _, _) = model.edit_state;
  view_of_snode(~inject, snode_of_zblock(zblock));
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
    [mb_par([SNode(s1), SToken(mk_SDelim(sop)), SNode(s2)])],
  );

let snode_of_SpaceOp = (~err_status, ~steps, s1: snode, s2: snode): snode =>
  mk_SBox(
    ~err_status,
    ~steps,
    ~shape=SkelBinOp,
    [[SNode(s1), SToken(SSpaceOp), SNode(s2)]],
  );

let snode_of_Let =
    (~err_status, ~steps, sp: snode, sdef: snode, sbody: snode): snode =>
  mk_SBox(
    ~err_status,
    ~steps,
    ~shape=Let,
    ~is_multi_line=true,
    [
      [
        SToken(mk_SDelim("let")),
        SNode(sp),
        SToken(mk_SDelim("=")),
        SNode(sdef),
      ],
      [SNode(sbody)],
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
      [
        SToken(mk_SDelim("fix")),
        SNode(sarg),
        SToken(mk_SDelim(":")),
        SNode(sty),
        SToken(mk_SDelim(".")),
      ],
      [SNode(sbody)],
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
      [mb_par([SToken(SEmptyHole(hole_label_of(u, i)))])],
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
      [mb_par([SNode(sp1)])],
    );
  | Wild =>
    mk_SBox(
      ~err_status,
      ~steps,
      ~shape=Wild,
      [[SToken(mk_SDelim(~index=0, "_"))]],
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
      [mb_par([SToken(SEmptyHole(hole_label_of(u, i)))])],
    )
  | NonEmptyHole(reason, u, i, sigma, d1) =>
    let s1 =
      snode_of_dhexp(~err_status=InHole(reason, u), ~steps=steps @ [0], d1);
    /* TODO add SHOW_SIGMAS flag */
    mk_SBox(
      ~steps,
      ~shape=NonEmptyHoleInstance(reason, u, i, Some(sigma)),
      [mb_par([SNode(s1)])],
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
          [
            SNode(s1),
            SToken(mk_SDelim("<")),
            SNode(sty1),
            SToken(SCastArrow),
            SNode(sty2),
            SToken(SCastArrow),
            SNode(sty3),
            SToken(mk_SDelim(">")),
          ],
        ]
        : [[SNode(s1)]],
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
          [
            SNode(s1),
            SToken(mk_SDelim("<")),
            SNode(sty1),
            SToken(SCastArrow),
            SNode(sty2),
            SToken(mk_SDelim(">")),
          ],
        ]
        : [[SNode(s1)]],
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
        [
          SNode(s1),
          SToken(mk_SDelim("<")),
          SNode(sty1),
          SToken(SFailedCastArrow),
          SNode(sty2),
          SToken(SFailedCastArrow),
          SNode(sty3),
          SToken(mk_SDelim(">")),
        ],
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
        [
          SNode(s1),
          SToken(mk_SDelim("<")),
          SNode(sty1),
          SToken(SFailedCastArrow),
          SNode(sty2),
          SToken(mk_SDelim(">")),
        ],
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
