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
      option(cursor_pos),
      is_multi_line,
      sskel,
      sseq_head,
      sseq_tail,
    )
  | SBox(
      Path.steps,
      option(cursor_pos),
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
      ~steps: Path.steps,
      ~cursor: option(cursor_pos)=None,
      ~is_multi_line=false,
      ~sskel: sskel,
      head: sseq_head,
      tail: sseq_tail,
    )
    : snode =>
  SSeq(steps, cursor, is_multi_line, sskel, head, tail);

let mk_SBox =
    (
      ~steps: Path.steps,
      ~cursor: option(cursor_pos)=None,
      ~is_multi_line=false,
      ~err_status=NotInHole,
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
      ~node_cursor: option(cursor_pos)=?,
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
      ~node_cursor: option(cursor_pos),
      stoken: stoken,
    ) =>
  switch (stoken) {
  | SDelim(k, s) =>
    open Vdom;
    let delim_before =
      Node.div(
        [
          Attr.id(path_id((node_steps, (k, Before)))),
          Attr.classes(["SDelim-before"]),
        ],
        [],
      );
    let delim_after =
      Node.div(
        [
          Attr.id(path_id((node_steps, (k, After)))),
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
          Attr.id(path_id((node_steps, (k, Before)))),
          Attr.classes(["SOp-before"]),
        ],
        [],
      );
    let op_after =
      Node.div(
        [
          Attr.id(path_id((node_steps, (k, After)))),
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

let rec snode_of_typ = (~steps: Path.steps, uty: UHTyp.t): snode =>
  switch (uty) {
  | Hole =>
    /* TODO need to do something about delimiter vs text discrepancy */
    mk_SBox(~steps, ~shape=EmptyHole, [[SToken(SDelim(0, "?"))]])
  | Unit => mk_SBox(~steps, ~shape=Unit, [[SToken(SDelim(0, "()"))]])
  | Num => mk_SBox(~steps, ~shape=Num, [[SToken(SDelim(0, "Num"))]])
  | Bool => mk_SBox(~steps, ~shape=Bool, [[SToken(SDelim(0, "Bool"))]])
  | Parenthesized(body) =>
    let sbody = snode_of_typ(~steps=steps @ [0], body);
    mk_SBox(
      ~steps,
      ~shape=Parenthesized,
      [
        [SToken(SDelim(0, "("))],
        [SNode(sbody)],
        [SToken(SDelim(1, ")"))],
      ],
    );
  | List(body) =>
    let sbody = snode_of_typ(~steps=steps @ [0], body);
    mk_SBox(
      ~steps,
      ~shape=List,
      [
        [SToken(SDelim(0, "List("))],
        [SNode(sbody)],
        [SToken(SDelim(1, ")"))],
      ],
    );
  | OpSeq(skel, seq) =>
    let (shd, stl, is_multi_line) = sseq_head_tail_of_seq_typ(~steps, seq);
    mk_SSeq(
      ~steps,
      ~is_multi_line,
      ~sskel=sskel_of_skel_typ(skel),
      shd,
      stl,
    );
  }
and sseq_head_tail_of_seq_typ =
    (~steps: Path.steps, ~leading_tm_index=0, seq: UHTyp.opseq)
    : (sseq_head, sseq_tail, is_multi_line) => {
  let (hd, tl) = OperatorSeq.split0(seq);
  let shd = snode_of_typ(~steps=steps @ [leading_tm_index], hd);
  let (stl, is_multi_line_tl) =
    sseq_tail_of_suffix_typ(
      ~steps,
      ~leading_op_index=leading_tm_index + 1,
      tl,
    );
  (shd, stl, is_multi_line(shd) || is_multi_line_tl);
}
and sseq_tail_of_suffix_typ =
    (~steps: Path.steps, ~leading_op_index=1, suffix: ZTyp.opseq_suffix)
    : (sseq_tail, is_multi_line) =>
  switch (suffix) {
  | ExpSuffix(op, tm) =>
    let sop = SOp(leading_op_index, string_of_op_typ(op));
    let stm = snode_of_typ(~steps=steps @ [leading_op_index], tm);
    ([(sop, stm)], is_multi_line(stm));
  | SeqSuffix(op, seq) =>
    let sop = SOp(leading_op_index, string_of_op_typ(op));
    let (shd, stl, is_multi_line_seq) =
      sseq_head_tail_of_seq_typ(
        ~steps,
        ~leading_tm_index=leading_op_index,
        seq,
      );
    ([(sop, shd), ...stl], is_multi_line_seq);
  };

let rec snode_of_pat = (~steps: Path.steps, p: UHPat.t): snode =>
  switch (p) {
  | EmptyHole(u) =>
    mk_SBox(
      ~steps,
      ~shape=EmptyHole,
      [[SToken(SDelim(0, string_of_int(u + 1)))]],
    )
  | Wild(err_status) =>
    mk_SBox(~steps, ~shape=Wild, ~err_status, [[SToken(SDelim(0, "_"))]])
  | Var(err_status, var_err_status, x) =>
    mk_SBox(
      ~steps,
      ~shape=Var,
      ~err_status,
      [[SToken(mk_SText(~var_err_status, x))]],
    )
  | NumLit(err_status, n) =>
    mk_SBox(
      ~steps,
      ~shape=NumLit,
      ~err_status,
      [[SToken(mk_SText(string_of_int(n)))]],
    )
  | BoolLit(err_status, b) =>
    mk_SBox(
      ~steps,
      ~shape=BoolLit,
      ~err_status,
      [[SToken(mk_SText(string_of_bool(b)))]],
    )
  | ListNil(err_status) =>
    mk_SBox(
      ~steps,
      ~shape=ListNil,
      ~err_status,
      [[SToken(SDelim(0, "[]"))]],
    )
  | Inj(err_status, side, body) =>
    let sbody = snode_of_pat(~steps=steps @ [0], body);
    mk_SBox(
      ~steps,
      ~shape=Inj,
      ~err_status,
      [
        [
          SToken(SDelim(0, "inj[" ++ LangUtil.string_of_side(side) ++ "](")),
        ],
        [SNode(sbody)],
        [SToken(SDelim(1, ")"))],
      ],
    );
  | Parenthesized(body) =>
    let sbody = snode_of_pat(~steps=steps @ [0], body);
    mk_SBox(
      ~steps,
      ~shape=Parenthesized,
      [
        [SToken(SDelim(0, "("))],
        [SNode(sbody)],
        [SToken(SDelim(1, ")"))],
      ],
    );
  | OpSeq(skel, seq) =>
    let (shd, stl, is_multi_line) = sseq_head_tail_of_seq_pat(~steps, seq);
    mk_SSeq(
      ~steps,
      ~is_multi_line,
      ~sskel=sskel_of_skel_pat(skel),
      shd,
      stl,
    );
  }
and sseq_head_tail_of_seq_pat =
    (~steps: Path.steps, ~leading_tm_index=0, seq: UHPat.opseq)
    : (sseq_head, sseq_tail, is_multi_line) => {
  let (hd, tl) = OperatorSeq.split0(seq);
  let shd = snode_of_pat(~steps=steps @ [leading_tm_index], hd);
  let (stl, is_multi_line_tl) =
    sseq_tail_of_suffix_pat(
      ~steps,
      ~leading_op_index=leading_tm_index + 1,
      tl,
    );
  (shd, stl, is_multi_line(shd) || is_multi_line_tl);
}
and sseq_tail_of_suffix_pat =
    (~steps: Path.steps, ~leading_op_index=1, suffix: ZPat.opseq_suffix)
    : (sseq_tail, is_multi_line) =>
  switch (suffix) {
  | ExpSuffix(op, tm) =>
    let sop =
      switch (op) {
      | Space => SSpaceOp
      | _ => SOp(leading_op_index, string_of_op_pat(op))
      };
    let stm = snode_of_pat(~steps=steps @ [leading_op_index], tm);
    ([(sop, stm)], is_multi_line(stm));
  | SeqSuffix(op, seq) =>
    let sop =
      switch (op) {
      | Space => SSpaceOp
      | _ => SOp(leading_op_index, string_of_op_pat(op))
      };
    let (shd, stl, is_multi_line_seq) =
      sseq_head_tail_of_seq_pat(
        ~steps,
        ~leading_tm_index=leading_op_index,
        seq,
      );
    ([(sop, shd), ...stl], is_multi_line_seq);
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
and snode_of_line_item = (~steps: Path.steps, li: UHExp.line): snode =>
  switch (li) {
  | EmptyLine => mk_SBox(~steps, ~shape=EmptyLine, [[]])
  | ExpLine(e) => snode_of_exp(~steps, e) /* ghost node */
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
and snode_of_exp = (~steps: Path.steps, e: UHExp.t): snode =>
  switch (e) {
  /* outer nodes */
  | EmptyHole(u) =>
    mk_SBox(
      ~steps,
      ~shape=EmptyHole,
      [[SToken(SDelim(0, string_of_int(u + 1)))]],
    )
  | Var(err_status, var_err_status, x) =>
    mk_SBox(
      ~steps,
      ~shape=Var,
      ~err_status,
      [[SToken(mk_SText(~var_err_status, x))]],
    )
  | NumLit(err_status, n) =>
    mk_SBox(
      ~steps,
      ~shape=NumLit,
      ~err_status,
      [[SToken(mk_SText(string_of_int(n)))]],
    )
  | BoolLit(err_status, b) =>
    mk_SBox(
      ~steps,
      ~shape=BoolLit,
      ~err_status,
      [[SToken(mk_SText(string_of_bool(b)))]],
    )
  | ListNil(err_status) =>
    /* TODO probably will run into issues with ListNil being a delimiter vs text */
    mk_SBox(
      ~steps,
      ~shape=ListNil,
      ~err_status,
      [[SToken(SDelim(0, "[]"))]],
    )
  /* inner nodes */
  | Lam(err_status, arg, ann, body) =>
    let sarg = snode_of_pat(~steps=steps @ [0], arg);
    let swords_ann =
      switch (ann) {
      | None => []
      | Some(uty) => [
          SToken(SDelim(1, ":")),
          SNode(snode_of_typ(~steps=steps @ [1], uty)),
        ]
      };
    let sbody = snode_of_block(~steps=steps @ [2], body);
    mk_SBox(
      ~steps,
      ~shape=Lam,
      ~err_status,
      ~is_multi_line=is_multi_line(sbody),
      [
        [SToken(SDelim(0, LangUtil.lamSym)), SNode(sarg)]
        @ swords_ann
        @ [SToken(SDelim(2, "."))],
        [SNode(sbody)],
      ],
    );
  | Inj(err_status, side, body) =>
    let sbody = snode_of_block(~steps=steps @ [0], body);
    mk_SBox(
      ~steps,
      ~shape=Inj,
      ~err_status,
      ~is_multi_line=is_multi_line(sbody),
      [
        [
          SToken(SDelim(0, "inj[" ++ LangUtil.string_of_side(side) ++ "](")),
        ],
        [SNode(sbody)],
        [SToken(SDelim(1, ")"))],
      ],
    );
  | Case(err_status, scrut, rules, ann) =>
    let sscrut = snode_of_block(~steps=steps @ [0], scrut);
    let slines_rules =
      rules
      |> List.mapi((i, rule) => snode_of_rule(~steps=steps @ [i], rule))
      |> List.map(snode => [SNode(snode)]);
    let swords_end =
      switch (ann) {
      | None => [SToken(SDelim(1, "end"))]
      | Some(uty) => [
          SToken(SDelim(1, "end :")),
          SNode(
            snode_of_typ(~steps=steps @ [List.length(rules) + 1], uty),
          ),
        ]
      };
    mk_SBox(
      ~steps,
      ~shape=Case,
      ~err_status,
      ~is_multi_line=true,
      [[SToken(SDelim(0, "case")), SNode(sscrut)]]
      @ slines_rules
      @ [swords_end],
    );
  | Parenthesized(body) =>
    let sbody = snode_of_block(~steps=steps @ [0], body);
    mk_SBox(
      ~steps,
      ~shape=Parenthesized,
      ~is_multi_line=is_multi_line(sbody),
      [
        [SToken(SDelim(0, "("))],
        [SNode(sbody)],
        [SToken(SDelim(1, ")"))],
      ],
    );
  | OpSeq(skel, seq) =>
    let (shd, stl, is_multi_line) = sseq_head_tail_of_seq_exp(~steps, seq);
    mk_SSeq(
      ~steps,
      ~is_multi_line,
      ~sskel=sskel_of_skel_exp(skel),
      shd,
      stl,
    );
  | ApPalette(_, _, _, _) => raise(InvariantViolated)
  }
and sseq_head_tail_of_seq_exp =
    (~steps: Path.steps, ~leading_tm_index=0, seq: UHExp.opseq)
    : (sseq_head, sseq_tail, is_multi_line) => {
  let (hd, tl) = OperatorSeq.split0(seq);
  let shd = snode_of_exp(~steps=steps @ [leading_tm_index], hd);
  let (stl, is_multi_line_tl) =
    sseq_tail_of_suffix_exp(
      ~steps,
      ~leading_op_index=leading_tm_index + 1,
      tl,
    );
  (shd, stl, is_multi_line(shd) || is_multi_line_tl);
}
and sseq_tail_of_suffix_exp =
    (~steps: Path.steps, ~leading_op_index=1, suffix: ZExp.opseq_suffix)
    : (sseq_tail, is_multi_line) =>
  switch (suffix) {
  | ExpSuffix(op, tm) =>
    let sop =
      switch (op) {
      | Space => SSpaceOp
      | _ => SOp(leading_op_index, string_of_op_exp(op))
      };
    let stm = snode_of_exp(~steps=steps @ [leading_op_index], tm);
    ([(sop, stm)], is_multi_line(stm));
  | SeqSuffix(op, seq) =>
    let sop =
      switch (op) {
      | Space => SSpaceOp
      | _ => SOp(leading_op_index, string_of_op_exp(op))
      };
    let (shd, stl, is_multi_line_seq) =
      sseq_head_tail_of_seq_exp(
        ~steps,
        ~leading_tm_index=leading_op_index,
        seq,
      );
    ([(sop, shd), ...stl], is_multi_line_seq);
  }
and snode_of_rule = (~steps: Path.steps, Rule(p, clause): UHExp.rule) => {
  let sp = snode_of_pat(~steps=steps @ [0], p);
  let sclause = snode_of_block(~steps=steps @ [1], clause);
  mk_SBox(
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
};

/*
 let instance_click_fn = ((u, _) as inst) => {
   let usi = React.S.value(user_selected_instances_rs);
   user_selected_instances_rf(UserSelectedInstances.update(usi, inst));
   move_to_hole(u);
   selected_instance_rf(Some(inst));
 };
 */
let result_view = (model: MyModel.t) => {
  let (_, _, result) = model.result;
  Vdom.(
    switch (result) {
    | InvalidInput(_) =>
      Node.div(
        [],
        [
          Node.text(
            "(internal error: expansion or evaluation invariant violated)",
          ),
        ],
      )
    | BoxedValue(_d)
    | Indet(_d) =>
      Node.div([], [] /* TODO view_of_dhexp(instance_click_fn) */)
    }
  );
};

let examples_select = (~inject: Update.Action.t => Vdom.Event.t) =>
  Vdom.(
    Node.select(
      [
        Attr.on_change((_ev, _) =>
          inject(
            Update.Action.LoadExample("" /* TODO ev##.target##.value */),
          )
        ),
      ],
      [
        Node.option([Attr.value("just_hole")], [Node.text("just a hole")]),
        Node.option(
          [Attr.value("holey_lambda")],
          [Node.text("holey lambda")],
        ),
        Node.option(
          [Attr.value("let_line")],
          [Node.text("let with extra lines")],
        ),
        Node.option([Attr.value("map_example")], [Node.text("map")]),
        Node.option([Attr.value("qsort_example")], [Node.text("qsort")]),
      ],
    )
  );

let page_view =
    (~inject: Update.Action.t => Vdom.Event.t, model: MyModel.t): Vdom.Node.t => {
  Vdom.(
    Node.div(
      [Attr.id("root")],
      [
        Node.div(
          [Attr.classes(["top-bar"])],
          [
            Node.a(
              [Attr.classes(["logo-text"]), Attr.href("https://hazel.org")],
              [Node.text("Hazel")],
            ),
          ],
        ),
        Node.div(
          [Attr.classes(["main-area"])],
          [
            Sidebar.left(
              ~inject,
              false,
              [] /* TODO the_action_panel*/ /*, the_history_panel*/,
            ),
            Node.div(
              [Attr.classes(["flex-wrapper"])],
              [
                Node.div(
                  [Attr.classes(["page-area"])],
                  [
                    Node.div(
                      [Attr.classes(["page"])],
                      [
                        Node.div(
                          [],
                          [
                            Node.text("Hazel is an experiment in "),
                            Node.strong(
                              [],
                              [Node.text("live functional programming")],
                            ),
                            Node.text(" with "),
                            Node.strong([], [Node.text("typed holes")]),
                            Node.text(
                              ". Use the actions on the left to construct an expression. Navigate using the text cursor in the usual way.",
                            ),
                          ],
                        ),
                        /* TODO add pp_view_parent */
                        Node.div(
                          [Attr.classes(["cell-status"])],
                          [
                            Node.div(
                              [Attr.classes(["type-indicator"])],
                              [
                                Node.div(
                                  [Attr.classes(["type-label"])],
                                  [Node.text("Result of type: ")],
                                ),
                                Node.div(
                                  [Attr.classes(["htype-view"])],
                                  [] /* TODO htype_view */,
                                ),
                              ],
                            ),
                          ],
                        ),
                        Node.div(
                          [Attr.classes(["result-view"])],
                          [result_view(model)],
                        ),
                      ],
                    ),
                    examples_select(~inject),
                  ],
                ),
              ],
            ),
            Sidebar.right(
              ~inject,
              true,
              [] /* TODO the_cursor_inspector_panel, the_context_inspector_panel */,
            ),
          ],
        ),
      ],
    )
  );
};

[@warning "-27"]
let view =
    (~inject: Update.Action.t => Vdom.Event.t, model: MyModel.t): Vdom.Node.t =>
  page_view(~inject, model);
