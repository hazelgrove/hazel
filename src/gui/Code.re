module Vdom = Virtual_dom.Vdom;
open SemanticsCommon;

exception InvariantViolated;

type id = string;
type cls = string;

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
  | List;

type is_multi_line = bool;

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
  | SDelim(delim_index, string)
  | SSpaceOp
  | SOp(op_index, string)
  | SText(var_err_status, string);

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

let mk_SText = (~var_err_status=NotInVHole, s: string): stoken =>
  SText(var_err_status, s);

let string_of_op_typ: UHTyp.op => string =
  fun
  | Arrow => "Arrow"
  | Sum => "Sum"
  | Prod => "Prod";

let string_of_op_pat: UHPat.op => string =
  fun
  | Comma => "Comma"
  | Space => "Space"
  | Cons => "Cons";

let string_of_op_exp: UHExp.op => string =
  fun
  | Plus => "Plus"
  | Times => "Times"
  | LessThan => "LessThan"
  | Space => "Space"
  | Comma => "Comma"
  | Cons => "Cons";

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

let clss_of_sbox_shape = (_shape: sbox_shape): list(cls) => [""];

let rec string_of_steps =
  fun
  | [] => ""
  | [x, ...xs] => string_of_int(x) ++ "_" ++ string_of_steps(xs);
let node_id = steps => "node__" ++ string_of_steps(steps);
let path_id = path =>
  "path__" ++ Sexplib.Sexp.to_string(Path.sexp_of_t(path));

let multi_line_clss = is_multi_line => is_multi_line ? ["multi-line"] : [];

let err_status_clss =
  fun
  | NotInHole => []
  | InHole(_, u) => ["in_err_hole", "in_err_hole_" ++ string_of_int(u)];

let string_of_snode_shape =
  fun
  | Seq => "OpSeq"
  | Box(shape) =>
    switch (shape) {
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
    | List => "List"
    };

let sline_clss = (node_shape, line_no) => [
  "sline",
  string_of_snode_shape(node_shape) ++ "-" ++ string_of_int(line_no),
];

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

/* TODO */
let range_of_tree_rooted_at_cursor = (_cursor, _sskel) => (0, 0);

let rec of_snode = (~inject: Update.Action.t => Vdom.Event.t, snode) =>
  switch (snode) {
  | SSeq(steps, cursor, is_multi_line, sskel, shead, stail) =>
    let (vhead, vtail) =
      switch (cursor) {
      | None =>
        let vhead =
          of_sline(
            ~inject,
            ~node_steps=steps,
            ~node_shape=Seq,
            ~line_no=0,
            [SNode(shead)],
          );
        let vtail =
          stail
          |> List.mapi((i, (sop, stm)) =>
               of_sline(
                 ~inject,
                 ~node_steps=steps,
                 ~node_shape=Seq,
                 ~line_no=i + 1,
                 [SToken(sop), SNode(stm)],
               )
             );
        (vhead, vtail);
      | Some(cursor) =>
        let (a, b) = range_of_tree_rooted_at_cursor(cursor, sskel);
        let vhead_border_style = a == 0 ? TopBottom(true, false) : NoBorder;
        let vhead =
          of_sline(
            ~inject,
            ~node_steps=steps,
            ~node_cursor=cursor,
            ~node_shape=Seq,
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
               of_sline(
                 ~inject,
                 ~node_steps=steps,
                 ~node_cursor=cursor,
                 ~node_shape=Seq,
                 ~border_style,
                 ~line_no=i + 1,
                 [SToken(sop), SNode(stm)],
               );
             });
        (vhead, vtail);
      };
    Vdom.(
      Node.div(
        [
          Attr.id(node_id(steps)),
          Attr.classes(multi_line_clss(is_multi_line)),
        ],
        [vhead, ...vtail],
      )
    );
  | SBox(steps, node_cursor, is_multi_line, err_status, shape, slines) =>
    /* TODO add border style */
    let vlines =
      slines
      |> List.mapi((i, sline) =>
           of_sline(
             ~inject,
             ~node_steps=steps,
             ~node_cursor?,
             ~node_shape=Box(shape),
             ~line_no=i,
             sline,
           )
         );
    Vdom.(
      Node.div(
        [
          Attr.id(node_id(steps)),
          Attr.classes(
            err_status_clss(err_status) @ multi_line_clss(is_multi_line),
          ),
        ],
        vlines,
      )
    );
  }
and of_sline =
    (
      ~inject: Update.Action.t => Vdom.Event.t,
      ~node_steps: Path.steps,
      ~node_cursor: option(cursor_position)=?,
      ~node_shape: snode_shape,
      ~border_style: sline_border_style=NoBorder,
      ~line_no: int,
      sline,
    ) => {
  let vwords =
    sline
    |> List.map(sword =>
         switch (sword) {
         | SNode(snode) => of_snode(~inject, snode)
         | SToken(stoken) =>
           of_stoken(~inject, ~node_steps, ~node_cursor, stoken)
         }
       );
  Vdom.(
    Node.div(
      [
        Attr.classes(
          sline_clss(node_shape, line_no) @ sline_border_clss(border_style),
        ),
      ],
      vwords,
    )
  );
}
[@warning "-27"]
and of_stoken =
    (
      ~inject: Update.Action.t => Vdom.Event.t,
      ~node_steps: Path.steps,
      ~node_cursor: option(cursor_position),
      stoken: stoken,
    ) =>
  switch (stoken) {
  | SDelim(k, s) =>
    open Vdom;
    let delim_before =
      Node.div(
        [
          Attr.id(path_id((node_steps, OnDelim(k, Before)))),
          Attr.classes(["SDelim-before"]),
        ],
        [],
      );
    let delim_after =
      Node.div(
        [
          Attr.id(path_id((node_steps, OnDelim(k, After)))),
          Attr.classes(["SDelim-after"]),
        ],
        [],
      );
    let delim_txt =
      Node.div(
        [
          Attr.create("contenteditable", "false"),
          Attr.classes(["SDelim-txt"]),
        ],
        [Node.text(s)],
      );
    Node.div(
      [Attr.classes(["SDelim" /*TODO*/])],
      [delim_before, delim_txt, delim_after],
    );
  | SOp(k, s) =>
    open Vdom;
    let op_before =
      Node.div(
        [
          Attr.id(path_id((node_steps, OnDelim(k, Before)))),
          Attr.classes(["SOp-before"]),
        ],
        [],
      );
    let op_after =
      Node.div(
        [
          Attr.id(path_id((node_steps, OnDelim(k, After)))),
          Attr.classes(["SOp-after"]),
        ],
        [],
      );
    let op_txt =
      Node.div(
        [
          Attr.create("contenteditable", "false"),
          Attr.classes(["SOp-txt"]),
        ],
        [Node.text(s)],
      );
    Node.div(
      [Attr.classes(["SOp" /*TODO*/])],
      [op_before, op_txt, op_after],
    );
  | SSpaceOp => Vdom.(Node.div([Attr.classes(["SSpaceOp"])], []))
  | SText(var_err_status, s) =>
    Vdom.(
      Node.div(
        [
          Attr.id("" /* TODO */),
          Attr.classes(var_err_status_clss(var_err_status) /*TODO*/),
        ],
        [Node.text(s)],
      )
    )
  };

let snode_of_Parenthesized = (~cursor=?, ~steps, sbody: snode): snode =>
  mk_SBox(
    ~cursor?,
    ~steps,
    ~shape=Parenthesized,
    ~is_multi_line=is_multi_line(sbody),
    [
      [SToken(SDelim(0, "("))],
      [SNode(sbody)],
      [SToken(SDelim(1, ")"))],
    ],
  );

let snode_of_List = (~cursor=?, ~steps, sbody: snode): snode =>
  mk_SBox(
    ~cursor?,
    ~steps,
    ~shape=List,
    [
      [SToken(SDelim(0, "List("))],
      [SNode(sbody)],
      [SToken(SDelim(1, ")"))],
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
    | Some(sann) => [SToken(SDelim(1, ":")), SNode(sann)]
    };
  mk_SBox(
    ~cursor?,
    ~err_status,
    ~steps,
    ~shape=Lam,
    ~is_multi_line=is_multi_line(sbody),
    [
      [SToken(SDelim(0, LangUtil.lamSym)), SNode(sarg)]
      @ swords_ann
      @ [SToken(SDelim(2, "."))],
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
      [SToken(SDelim(0, "inj[" ++ LangUtil.string_of_side(side) ++ "]("))],
      [SNode(sbody)],
      [SToken(SDelim(1, ")"))],
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
    | None => [SToken(SDelim(1, "end"))]
    | Some(sann) => [SToken(SDelim(1, "end :")), SNode(sann)]
    };
  mk_SBox(
    ~cursor?,
    ~err_status,
    ~steps,
    ~shape=Case,
    ~is_multi_line=true,
    [[SToken(SDelim(0, "case")), SNode(sscrut)]]
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
        SToken(SDelim(0, "|")),
        SNode(sp),
        SToken(SDelim(1, LangUtil.caseArrowSym)),
      ],
      [SNode(sclause)],
    ],
  );

let rec snode_of_typ = (~cursor=?, ~steps: Path.steps, uty: UHTyp.t): snode =>
  switch (uty) {
  | Hole =>
    mk_SBox(
      ~cursor?,
      ~steps,
      ~shape=EmptyHole,
      [[SToken(SDelim(0, "?"))]],
    )
  | Unit =>
    mk_SBox(~cursor?, ~steps, ~shape=Unit, [[SToken(SDelim(0, "()"))]])
  | Num =>
    mk_SBox(~cursor?, ~steps, ~shape=Num, [[SToken(SDelim(0, "Num"))]])
  | Bool =>
    mk_SBox(~cursor?, ~steps, ~shape=Bool, [[SToken(SDelim(0, "Bool"))]])
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
      |> List.mapi((i, op) => SOp(i + 1, string_of_op_typ(op)));
    let sskel = sskel_of_skel_typ(skel);
    snode_of_OpSeq(~cursor?, ~steps, ~sskel, stms, sops);
  };

let rec snode_of_pat = (~cursor=?, ~steps: Path.steps, p: UHPat.t): snode =>
  switch (p) {
  | EmptyHole(u) =>
    mk_SBox(
      ~cursor?,
      ~steps,
      ~shape=EmptyHole,
      [[SToken(SDelim(0, string_of_int(u + 1)))]],
    )
  | Wild(err_status) =>
    mk_SBox(
      ~cursor?,
      ~err_status,
      ~steps,
      ~shape=Wild,
      [[SToken(SDelim(0, "_"))]],
    )
  | Var(err_status, var_err_status, x) =>
    mk_SBox(
      ~cursor?,
      ~err_status,
      ~steps,
      ~shape=Var,
      [[SToken(mk_SText(~var_err_status, x))]],
    )
  | NumLit(err_status, n) =>
    mk_SBox(
      ~cursor?,
      ~err_status,
      ~steps,
      ~shape=NumLit,
      [[SToken(mk_SText(string_of_int(n)))]],
    )
  | BoolLit(err_status, b) =>
    mk_SBox(
      ~cursor?,
      ~err_status,
      ~steps,
      ~shape=BoolLit,
      [[SToken(mk_SText(string_of_bool(b)))]],
    )
  | ListNil(err_status) =>
    mk_SBox(
      ~steps,
      ~err_status,
      ~shape=ListNil,
      [[SToken(SDelim(0, "[]"))]],
    )
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
           | _ => SOp(i + 1, string_of_op_pat(op))
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
  | EmptyLine => mk_SBox(~cursor?, ~steps, ~shape=EmptyLine, [[]])
  | ExpLine(e) => snode_of_exp(~cursor?, ~steps, e) /* ghost node */
  | LetLine(p, ann, def) =>
    let sp = snode_of_pat(~steps=steps @ [0], p);
    let swords_ann =
      switch (ann) {
      | None => []
      | Some(uty) => [
          SToken(SDelim(1, ":")),
          SNode(snode_of_typ(~steps=steps @ [1], uty)),
        ]
      };
    let sdef = snode_of_block(~steps=steps @ [2], def);
    mk_SBox(
      ~cursor?,
      ~steps,
      ~shape=LetLine,
      ~is_multi_line=is_multi_line(sdef),
      [
        [SToken(SDelim(0, "let")), SNode(sp)]
        @ swords_ann
        @ [SToken(SDelim(2, "="))],
        [SNode(sdef)],
      ],
    );
  }
and snode_of_exp = (~cursor=?, ~steps: Path.steps, e: UHExp.t): snode =>
  switch (e) {
  /* outer nodes */
  | EmptyHole(u) =>
    mk_SBox(
      ~cursor?,
      ~steps,
      ~shape=EmptyHole,
      [[SToken(SDelim(0, string_of_int(u + 1)))]],
    )
  | Var(err_status, var_err_status, x) =>
    mk_SBox(
      ~cursor?,
      ~err_status,
      ~steps,
      ~shape=Var,
      [[SToken(mk_SText(~var_err_status, x))]],
    )
  | NumLit(err_status, n) =>
    mk_SBox(
      ~cursor?,
      ~err_status,
      ~steps,
      ~shape=NumLit,
      [[SToken(mk_SText(string_of_int(n)))]],
    )
  | BoolLit(err_status, b) =>
    mk_SBox(
      ~cursor?,
      ~err_status,
      ~steps,
      ~shape=BoolLit,
      [[SToken(mk_SText(string_of_bool(b)))]],
    )
  | ListNil(err_status) =>
    mk_SBox(
      ~cursor?,
      ~err_status,
      ~steps,
      ~shape=ListNil,
      [[SToken(SDelim(0, "[]"))]],
    )
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
           | _ => SOp(i + 1, string_of_op_exp(op))
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
      |> List.mapi((i, op) => SOp(i + 1, string_of_op_typ(op)));
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
           | _ => SOp(i + 1, string_of_op_pat(op))
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
          SToken(SDelim(1, ":")),
          SNode(snode_of_typ(~steps=steps @ [1], uty)),
        ]
      };
    let sdef = snode_of_block(~steps=steps @ [2], def);
    mk_SBox(
      ~steps,
      ~shape=LetLine,
      ~is_multi_line=is_multi_line(sdef),
      [
        [SToken(SDelim(0, "let")), SNode(szp)]
        @ swords_ann
        @ [SToken(SDelim(2, "="))],
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
        [SToken(SDelim(0, "let")), SNode(sp)]
        @ [SToken(SDelim(1, ":")), SNode(szann)]
        @ [SToken(SDelim(2, "="))],
        [SNode(sdef)],
      ],
    );
  | LetLineZE(p, ann, zdef) =>
    let sp = snode_of_pat(~steps=steps @ [0], p);
    let swords_ann =
      switch (ann) {
      | None => []
      | Some(uty) => [
          SToken(SDelim(1, ":")),
          SNode(snode_of_typ(~steps=steps @ [1], uty)),
        ]
      };
    let szdef = snode_of_zblock(~steps=steps @ [2], zdef);
    mk_SBox(
      ~steps,
      ~shape=LetLine,
      ~is_multi_line=is_multi_line(szdef),
      [
        [SToken(SDelim(0, "let")), SNode(sp)]
        @ swords_ann
        @ [SToken(SDelim(2, "="))],
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
           | _ => SOp(i + 1, string_of_op_exp(op))
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
    let szp = snode_of_zpat(~steps=steps @ [0], zp);
    let sclause = snode_of_block(~steps=steps @ [1], clause);
    snode_of_Rule(~steps, szp, sclause);
  | RuleZE(p, zclause) =>
    let sp = snode_of_pat(~steps=steps @ [0], p);
    let szclause = snode_of_zblock(~steps=steps @ [1], zclause);
    snode_of_Rule(~steps, sp, szclause);
  };

let view =
    (~inject: Update.Action.t => Vdom.Event.t, model: MyModel.t): Vdom.Node.t => {
  let (zblock, _, _) = model.edit_state;
  of_snode(~inject, snode_of_zblock(zblock));
};
