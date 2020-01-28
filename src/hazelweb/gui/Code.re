let _SHOW_CASTS = false;
let _SHOW_FN_BODIES = false;

module Js = Js_of_ocaml.Js;
module Dom = Js_of_ocaml.Dom;
module Dom_html = Js_of_ocaml.Dom_html;
module Vdom = Virtual_dom.Vdom;
open GeneralUtil;
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
type ap_err_status =
  | NotInApHole
  | InApHole(ErrStatus.t, Skel.range);

[@deriving sexp]
type has_skinny_concluding_let_line = bool;

[@deriving sexp]
type snode_shape =
  | Seq
  | Box(sbox_shape)
[@deriving sexp]
and sbox_shape =
  | Block
      // whether this block concludes with a let line and empty hole
      // and user has not moved empty hole to new line
      (has_skinny_concluding_let_line)
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
      ErrStatus.HoleReason.t,
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
      CursorPath.steps,
      is_multi_line,
      spaced_stms,
      list((op_stokens, spaced_stms)),
    )
  | SBox(
      CursorPath.steps,
      is_multi_line,
      ErrStatus.t,
      ap_err_status,
      sbox_shape,
      list(sline),
    )
[@deriving sexp]
and spaced_stms = (snode, list(snode))
[@deriving sexp]
and op_stokens = list(stoken)
[@deriving sexp]
and sline =
  | SLine(int, option(CursorPath.steps), list(sword))
[@deriving sexp]
and sword =
  | SNode(snode)
  | SToken(stoken)
[@deriving sexp]
and stoken =
  | SEmptyLine
  | SEmptyHole(string)
  | SDelim(DelimIndex.t, string)
  | SOp(OpIndex.t, ErrStatus.t, string)
  | SText(VarErrStatus.t, string)
  | SCastArrow
  | SFailedCastArrow
  | SSpace
  | SReadOnly(string);

let is_multi_line =
  fun
  | SSeq(_, is_multi_line, _, _)
  | SBox(_, is_multi_line, _, _, _, _) => is_multi_line;

let mk_SSeq =
    (
      ~is_multi_line=false,
      ~steps: CursorPath.steps,
      shead: spaced_stms,
      stail: list((op_stokens, spaced_stms)),
    )
    : snode =>
  SSeq(steps, is_multi_line, shead, stail);

let mk_SBox =
    (
      ~is_multi_line=false,
      ~err: ErrStatus.t=NotInHole,
      ~ap_err_status=NotInApHole,
      ~steps: CursorPath.steps,
      ~shape: sbox_shape,
      slines: list(sline),
    )
    : snode => {
  SBox(steps, is_multi_line, err, ap_err_status, shape, slines);
};

let mk_SLine =
    (
      ~rel_indent=0, // relative indentation by tabbing
      ~steps_of_first_sword: option(CursorPath.steps)=?,
      swords: list(sword),
    )
    : sline =>
  SLine(rel_indent, steps_of_first_sword, swords);

let steps_of_first_sword =
  fun
  | SLine(_, steps, _) => steps;

// does nothing if not SBox
let prepend_tokens_on_SBox = (stokens, snode) =>
  switch (snode) {
  | SSeq(_, _, _, _) => snode
  | SBox(a, b, c, d, e, slines) =>
    switch (slines) {
    | [] => snode
    | [SLine(rel_indent, steps, swords), ...slines] =>
      SBox(
        a,
        b,
        c,
        d,
        e,
        [
          SLine(
            rel_indent,
            steps,
            (stokens |> List.map(stoken => SToken(stoken))) @ swords,
          ),
          ...slines,
        ],
      )
    }
  };

let mk_SOp = (~index: OpIndex.t, ~err, s: string) => SOp(index, err, s);

let mk_op_stokens =
    (
      ~index: OpIndex.t,
      ~err: ErrStatus.t=NotInHole,
      ~space_before=false,
      ~space_after=false,
      s: string,
    ) =>
  (space_before ? [SSpace] : [])
  @ [mk_SOp(~err, ~index, s)]
  @ (space_after ? [SSpace] : []);

let steps_of_snode =
  fun
  | SSeq(steps, _, _, _)
  | SBox(steps, _, _, _, _, _) => steps;

let mk_SDelim = (~index: DelimIndex.t, s: string): stoken =>
  SDelim(index, s);

let mk_SText = (~var_err: VarErrStatus.t=NotInVarHole, s: string): stoken =>
  SText(var_err, s);

let sspace_vnode =
  Vdom.(
    Node.span(
      [Attr.classes(["SSpace"]), Attr.create("contenteditable", "false")],
      [Node.text(" ")],
    )
  );

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
  | And => "&"
  | Or => "|"
  | Minus => "-"
  | Plus => "+"
  | Times => "*"
  | LessThan => "<"
  | GreaterThan => ">"
  | Equals => "=="
  | Space => " "
  | Comma => ","
  | Cons => "::";

let space_before_after_op_typ: UHTyp.op => (bool, bool) =
  fun
  | Arrow => (true, true)
  | Sum => (true, true)
  | Prod => (false, true);

let space_before_after_op_pat: UHPat.op => (bool, bool) =
  fun
  | Comma => (false, true)
  | Space => (false, false)
  | Cons => (false, false);

let space_before_after_op_exp: UHExp.op => (bool, bool) =
  fun
  | And => (true, true)
  | Or => (true, true)
  | Minus => (true, true)
  | Plus => (true, true)
  | Times => (true, true)
  | LessThan => (true, true)
  | GreaterThan => (true, true)
  | Equals => (true, true)
  | Space => (false, false)
  | Comma => (false, true)
  | Cons => (false, false);

type spaced_tms_typ = (UHTyp.t, list(UHTyp.t));
let rec partition_into_spaced_tms_typ =
        (skel: UHTyp.skel_t, seq: UHTyp.opseq)
        : (spaced_tms_typ, list((UHTyp.op, spaced_tms_typ))) =>
  switch (skel) {
  | Placeholder(n) =>
    switch (seq |> OperatorSeq.nth_tm(n)) {
    | None => assert(false)
    | Some(p) => ((p, []), [])
    }
  | BinOp(_, op, skel1, skel2) =>
    let (hd1, tl1) = partition_into_spaced_tms_typ(skel1, seq);
    let (hd2, tl2) = partition_into_spaced_tms_typ(skel2, seq);
    (hd1, tl1 @ [(op, hd2)] @ tl2);
  };

type spaced_tms_pat = (ap_err_status, UHPat.t, list(UHPat.t));
let rec partition_into_spaced_tms_pat =
        (skel: UHPat.skel_t, seq: UHPat.opseq)
        : (spaced_tms_pat, list((ErrStatus.t, UHPat.op, spaced_tms_pat))) =>
  switch (skel) {
  | Placeholder(n) =>
    switch (seq |> OperatorSeq.nth_tm(n)) {
    | None => assert(false)
    | Some(p) => ((NotInApHole, p, []), [])
    }
  | BinOp(err, Space, skel1, skel2) =>
    // if we see Space, we know all ops in subtrees are also Space
    let (a, _) = skel1 |> Skel.range;
    let (_, b) = skel2 |> Skel.range;
    switch (seq |> OperatorSeq.tms_of_range((a, b))) {
    | None
    | Some([]) => assert(false)
    | Some([p, ...ps]) =>
      let ap_err_status =
        switch (err) {
        | NotInHole => NotInApHole
        | InHole(_, _) => InApHole(err, (a, b))
        };
      ((ap_err_status, p, ps), []);
    };
  | BinOp(err, op, skel1, skel2) =>
    let (hd1, tl1) = partition_into_spaced_tms_pat(skel1, seq);
    let (hd2, tl2) = partition_into_spaced_tms_pat(skel2, seq);
    (hd1, tl1 @ [(err, op, hd2)] @ tl2);
  };

type spaced_tms_exp = (ap_err_status, UHExp.t, list(UHExp.t));
let rec partition_into_spaced_tms_exp =
        (skel: UHExp.skel_t, seq: UHExp.opseq)
        : (spaced_tms_exp, list((ErrStatus.t, UHExp.op, spaced_tms_exp))) =>
  switch (skel) {
  | Placeholder(n) =>
    switch (seq |> OperatorSeq.nth_tm(n)) {
    | None => assert(false)
    | Some(e) => ((NotInApHole, e, []), [])
    }
  | BinOp(err, Space, skel1, skel2) =>
    // if we see Space, we know all ops in subtrees are also Space
    let (a, _) = skel1 |> Skel.range;
    let (_, b) = skel2 |> Skel.range;
    switch (seq |> OperatorSeq.tms_of_range((a, b))) {
    | None
    | Some([]) => assert(false)
    | Some([e, ...es]) =>
      let ap_err_status = InApHole(err, (a, b));
      ((ap_err_status, e, es), []);
    };
  | BinOp(err, op, skel1, skel2) =>
    let (hd1, tl1) = partition_into_spaced_tms_exp(skel1, seq);
    let (hd2, tl2) = partition_into_spaced_tms_exp(skel2, seq);
    (hd1, tl1 @ [(err, op, hd2)] @ tl2);
  };

let multi_line_clss = is_multi_line => is_multi_line ? ["multi-line"] : [];

let err_status_clss: ErrStatus.t => list(cls) =
  fun
  | NotInHole => []
  | InHole(_, _) => ["in-hole"];

let inline_div_cls = "inline-div";

[@deriving sexp]
type child_indices = list(int);

let rec child_indices_of_snode =
  fun
  | SSeq(_steps, _is_multi_line, _shead, stail) =>
    range(List.length(stail) + 1)
  | SBox(_steps, _is_multi_line, _err_status, _ap_err_status, _shape, slines) =>
    slines |> List.map(child_indices_of_sline) |> List.concat
and child_indices_of_sline =
  fun
  | SLine(_, _, swords) =>
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

let force_get_snode_elem = steps =>
  JSUtil.force_get_elem_by_id(node_id(steps));

let force_get_sdelim_elem = delim_path =>
  JSUtil.force_get_elem_by_id(delim_id(delim_path));

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

/**
 * slines are nested and form a tree. An sline is
 * called *skinny* if every sline in the subtree
 * rooted there contains at most one sline. When
 * constructing vdom nodes, we only create indentation
 * nodes in front of the most ancestral skinny slines.
 */
let is_skinny =
  List.for_all(
    fun
    | SToken(_) => true
    | SNode(snode) => !(snode |> is_multi_line),
  );

let parent_snode_elem_of_snode_elem = elem =>
  switch (steps_of_node_id(Js.to_string(elem##.id))) {
  | None => None
  | Some(steps) =>
    switch (split_last(steps)) {
    | None => None
    | Some((parent_steps, _)) => Some(force_get_snode_elem(parent_steps))
    }
  };

let parent_sline_elem_of_sdelim_elem = elem =>
  switch (delim_path_of_delim_id(Js.to_string(elem##.id))) {
  | None => None
  | Some(_) =>
    let node = (elem: Js.t(Dom_html.element) :> Js.t(Dom.node));
    let parent_node = Js.Opt.get(node##.parentNode, () => assert(false));
    Js.Opt.to_option(Dom_html.CoerceTo.element(parent_node));
  };

let force_get_snode_elem = steps =>
  JSUtil.force_get_elem_by_id(node_id(steps));

let sline_elems_of_snode_elem =
    (
      snode_elem: Js.t(Dom_html.element),
      container_elem: Js.t(Dom_html.element),
    ) => {
  container_elem##querySelectorAll(
    Js.string(
      "[id=\'" ++ Js.to_string(snode_elem##.id) ++ "\'] > ." ++ cls_sline,
    ),
  )
  |> Dom.list_of_nodeList;
};

let line_no_of_sline_elem = sline_elem =>
  sline_elem
  |> JSUtil.clss_of_elem
  |> List.fold_left(
       (found, cls) =>
         switch (found) {
         | Some(_) => found
         | None => line_no_of_sline_cls(cls)
         },
       None,
     );

let cls_SNode = "SNode";
let cls_SBox = "SBox";
let cls_SSeq = "SSeq";

let elem_is_SBox = JSUtil.elem_has_cls(cls_SBox);
let elem_is_SSeq = JSUtil.elem_has_cls(cls_SSeq);

let elem_is_multi_line = JSUtil.elem_has_cls("multi-line");

let elem_is_on_last_line = elem => {
  (elem: Js.t(Dom_html.element) :> Js.t(Dom.node))
  |> JSUtil.query_ancestors(ancestor =>
       ancestor##.nextSibling
       |> Js.Opt.to_option
       |> Opt.map(next =>
            "br" == Js.to_string(next##.nodeName) ? Some() : None
          )
     )
  |> Opt.test;
};

let snode_elem_occupies_full_sline = elem => {
  let steps =
    elem##.id
    |> Js.to_string
    |> steps_of_node_id
    |> Opt.get(() => assert(false));
  let node = (elem: Js.t(Dom_html.element) :> Js.t(Dom.node));
  let parent_node = Js.Opt.get(node##.parentNode, () => assert(false));
  parent_node##.childNodes
  |> Dom.list_of_nodeList
  |> List.for_all(sibling =>
       switch (sibling |> Dom_html.CoerceTo.element |> Js.Opt.to_option) {
       | None => true
       | Some(sibling_elem) =>
         switch (sibling_elem##.id |> Js.to_string |> steps_of_node_id) {
         | None => sibling_elem |> JSUtil.elem_has_cls(indentation_cls)
         | Some(sibling_steps) => steps == sibling_steps
         }
       }
     );
};

/**
 * Internal data type used by view_of_* functions to
 * accumulate and apply indentation.
 */
[@deriving sexp]
type indent_level =
  | ToBeIndented(int)
  | Indented(int);

let tab_length = 2;
let tab = (~k=1) =>
  fun
  | ToBeIndented(m) => ToBeIndented(m + k * tab_length)
  | Indented(m) => Indented(m + k * tab_length);

let increment = (~k=1) =>
  fun
  | ToBeIndented(m) => ToBeIndented(m + k)
  | Indented(m) => Indented(m + k);

let indent_level_of_snode_elem = elem =>
  elem
  |> JSUtil.force_get_attr("indent_level")
  |> Sexp.of_string
  |> indent_level_of_sexp;

let cls_Block = "Block";

let snode_elem_is_Block = JSUtil.elem_has_cls(cls_Block);

let indent_level_attr = (~op_margin=?, indent_level) =>
  switch (op_margin) {
  | None =>
    Vdom.Attr.create(
      "indent_level",
      Sexp.to_string(sexp_of_indent_level(indent_level)),
    )
  | Some(k) =>
    Vdom.Attr.create(
      "indent_level",
      Sexp.to_string(sexp_of_indent_level(indent_level |> increment(~k))),
    )
  };

let snode_attrs =
    (
      ~inject: Update.Action.t => Vdom.Event.t,
      ~op_margin=?,
      ~indent_level,
      snode: snode,
    )
    : list(Vdom.Attr.t) => {
  Vdom.(
    switch (snode) {
    | SSeq(steps, is_multi_line, _shead, _stail) => [
        Attr.id(node_id(steps)),
        Attr.classes(
          [cls_SNode, cls_SSeq, inline_div_cls]
          @ multi_line_clss(is_multi_line),
        ),
        indent_level_attr(indent_level),
      ]
    | SBox(steps, is_multi_line, err, ap_err_status, shape, _) =>
      let base_clss =
        [cls_SNode, cls_SBox, inline_div_cls]
        @ multi_line_clss(is_multi_line)
        @ err_status_clss(err);
      let shape_attrs =
        switch (shape) {
        | Block(_) => [Attr.classes([cls_Block, ...base_clss])]
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
        Attr.create(
          "children",
          snode
          |> child_indices_of_snode
          |> sexp_of_child_indices
          |> Sexplib.Sexp.to_string,
        ),
        indent_level_attr(~op_margin?, indent_level),
        ...shape_attrs,
      ]
      @ (
        switch (ap_err_status) {
        | NotInApHole => []
        | InApHole(_, range) => [
            Attr.create(
              "in-ap-hole",
              Sexplib.Sexp.to_string(Skel.sexp_of_range(range)),
            ),
          ]
        }
      );
    }
  );
};

let var_err_clss: VarErrStatus.t => list(cls) =
  fun
  | NotInVarHole => []
  | InVarHole(Free, u) => ["InVarHole", "InVarHole_" ++ string_of_int(u)]
  | InVarHole(Keyword(_), u) => [
      "InVarHole",
      "InVarHole_" ++ string_of_int(u),
      "Keyword",
    ];

let vindentation = (~steps_of_first_sword=?, ~first_sword=?, m) => {
  // attrs used in editable code to determine where
  // to transport cursor if user clicks indentation
  let goto_path = path => [
    Vdom.Attr.create(
      "goto-path",
      Sexp.to_string(CursorPath.sexp_of_t(path)),
    ),
  ];
  let goto_steps = steps => [
    Vdom.Attr.create(
      "goto-steps",
      Sexp.to_string(CursorPath.sexp_of_steps(steps)),
    ),
  ];
  let goto_attrs =
    switch (steps_of_first_sword, first_sword) {
    | (None, _)
    | (
        _,
        None |
        Some(SToken(SCastArrow | SFailedCastArrow | SSpace | SReadOnly(_))),
      ) =>
      []
    | (
        Some(steps),
        Some(SNode(_) | SToken(SEmptyHole(_) | SText(_, _) | SEmptyLine)),
      ) =>
      goto_steps(steps)
    | (Some(steps), Some(SToken(SDelim(k, _) | SOp(k, _, _)))) =>
      goto_path((steps, OnDelim(k, Before)))
    };
  Vdom.(
    Node.span(
      goto_attrs
      @ [
        Attr.classes([indentation_cls]),
        Attr.create("contenteditable", "false"),
      ],
      [Node.text(" " |> replicate(m) |> String.concat(""))],
    )
  );
};

let view_of_sseq_line =
    (~cls, ~sseq_steps: CursorPath.steps, ~tm_index: ChildIndex.t, vwords)
    : Vdom.Node.t =>
  Vdom.(
    Node.div(
      [
        Attr.classes([cls, inline_div_cls] @ sline_clss(tm_index)),
        Vdom.Attr.create(
          "goto-steps",
          Sexp.to_string(CursorPath.sexp_of_steps(sseq_steps @ [tm_index])),
        ),
      ],
      vwords,
    )
  );

let rec view_of_snode =
        (
          ~inject: Update.Action.t => Vdom.Event.t,
          ~op_margin: option(int)=?,
          ~indent_level=ToBeIndented(0),
          snode,
        )
        : Vdom.Node.t => {
  let attrs = snode_attrs(~inject, ~op_margin?, ~indent_level, snode);
  switch (snode) {
  | SSeq(steps, is_multi_line, shead, stail) =>
    let (shd, sargs) = shead;
    let vhead = [
      view_of_sseq_head(
        ~inject,
        ~sseq_steps=steps,
        ~sseq_indent_level=indent_level,
        shd,
      ),
      ...sargs
         |> List.mapi((i, sarg) =>
              view_of_sseq_head_arg(
                ~inject,
                ~sseq_steps=steps,
                ~tm_index=i + 1,
                ~sseq_indent_level=indent_level,
                sarg,
              )
            ),
    ];
    let (_, vlines) =
      stail
      |> List.fold_left(
           ((num_lines_so_far, vlines_so_far), (op_stokens, spaced_stms)) => {
             let (fst, args) = spaced_stms;
             let new_vlines = [
               view_of_sseq_tail(
                 ~inject,
                 ~sseq_steps=steps,
                 ~sseq_is_multi_line=is_multi_line,
                 ~tm_index=num_lines_so_far,
                 ~sseq_indent_level=indent_level,
                 op_stokens,
                 fst,
               ),
               ...args
                  |> List.mapi((i, arg) =>
                       view_of_sseq_tail_arg(
                         ~inject,
                         ~sseq_steps=steps,
                         ~sseq_is_multi_line=is_multi_line,
                         ~tm_index=num_lines_so_far + 1 + i,
                         ~sseq_indent_level=indent_level,
                         op_stokens,
                         arg,
                       )
                     ),
             ];
             (
               num_lines_so_far + List.length(new_vlines),
               vlines_so_far @ new_vlines,
             );
           },
           (List.length(vhead), vhead),
         );
    Vdom.Node.div(
      attrs,
      is_multi_line ? vlines |> join(Vdom.Node.br([])) : vlines,
    );
  | SBox(steps, is_multi_line, _, _, shape, slines) =>
    let view_of_sline = (~node_indent_level, i, sline) =>
      view_of_sline(
        ~inject,
        ~node_steps=steps,
        ~node_indent_level,
        ~line_no=i,
        sline,
      );
    let indent_levels =
      indent_level
      |> replicate(List.length(slines))
      |> List.mapi((i, indent_level) =>
           switch (indent_level) {
           | Indented(_) => indent_level
           | ToBeIndented(n) =>
             switch (op_margin) {
             | None =>
               switch (shape, i == List.length(slines) - 1) {
               | (Block(true), true) =>
                 // If this SBox is a Block that concludes with a let line
                 // followed by a trailing empty hole, then we do not separate
                 // those last two lines by a newline even when the whole
                 // block is multi-line. Thus, we also need to take care not
                 // to print indentation before the trailing empty hole.
                 Indented(n)
               | (_, _) => indent_level
               }
             | Some(m) =>
               // If this SBox is trailing an operator in a multi-line opseq,
               // then we have already printed the indentation for the first
               // sline of this SBox before the operator. Also need to increase
               // the to-be-applied indentation of the remaining slines.
               i == 0 ? Indented(m + n) : ToBeIndented(m + n)
             }
           }
         );
    let vlines =
      List.combine(slines, indent_levels)
      |> List.mapi((i, (sline, indent_level)) =>
           view_of_sline(~node_indent_level=indent_level, i, sline)
         );
    let newlined_vlines =
      switch (shape) {
      | Block(true) =>
        // do not add a let line between a let line
        // and its concluding trailing empty hole
        switch (vlines |> split_last) {
        | None => assert(false)
        | Some((vleading, vconclusion)) =>
          (is_multi_line ? vleading |> join(Vdom.Node.br([])) : vleading)
          @ [sspace_vnode, vconclusion]
        }
      | _ => is_multi_line ? vlines |> join(Vdom.Node.br([])) : vlines
      };
    Vdom.Node.div(attrs, newlined_vlines);
  };
}
and view_of_sline =
    (
      ~inject: Update.Action.t => Vdom.Event.t,
      ~node_steps: CursorPath.steps,
      ~line_no: int,
      ~node_indent_level,
      sline,
    )
    : Vdom.Node.t => {
  // in rare case that user clicks on the SLine element
  // and not any of its child elements
  let goto_steps_attrs =
    switch (sline |> steps_of_first_sword) {
    | None => []
    | Some(steps) => [
        Vdom.Attr.create(
          "goto-steps",
          Sexp.to_string(CursorPath.sexp_of_steps(steps)),
        ),
      ]
    };
  let (vindentation, vwords) =
    switch (sline) {
    | SLine(rel_indent, steps_of_first_sword, swords) =>
      let is_skinny = swords |> is_skinny;
      switch (node_indent_level |> tab(~k=rel_indent), is_skinny, swords) {
      | (_, _, []) => assert(false)
      | (Indented(_) as indent_level, _, _)
      | (indent_level, false, _) => (
          [],
          swords
          |> List.map(
               fun
               | SNode(snode) => view_of_snode(~inject, ~indent_level, snode)
               | SToken(stoken) =>
                 view_of_stoken(~inject, ~node_steps, stoken),
             ),
        )
      | (ToBeIndented(indentation), true, [first_sword, ..._]) =>
        let vindentation =
          indentation == 0
            ? []
            : [
              vindentation(~steps_of_first_sword?, ~first_sword, indentation),
            ];
        let vwords =
          swords
          |> List.map(sword =>
               switch (sword) {
               | SNode(snode) =>
                 view_of_snode(
                   ~inject,
                   ~indent_level=Indented(indentation),
                   snode,
                 )
               | SToken(stoken) =>
                 view_of_stoken(~inject, ~node_steps, stoken)
               }
             );
        (vindentation, vwords);
      };
    };
  Vdom.(
    Node.div(
      [
        Attr.classes(["SLine", inline_div_cls] @ sline_clss(line_no)),
        ...goto_steps_attrs,
      ],
      vindentation @ vwords,
    )
  );
}
and view_of_sseq_head =
    (
      ~inject: Update.Action.t => Vdom.Event.t,
      ~sseq_steps: CursorPath.steps,
      ~sseq_indent_level: indent_level,
      shd: snode,
    )
    : Vdom.Node.t => {
  let is_skinny = !is_multi_line(shd);
  let (vindentation, vwords) =
    switch (sseq_indent_level, is_skinny) {
    | (Indented(_), _)
    | (_, false) => (
        [],
        [view_of_snode(~inject, ~indent_level=sseq_indent_level, shd)],
      )
    | (ToBeIndented(indentation), true) => (
        indentation == 0
          ? []
          : [
            vindentation(
              ~steps_of_first_sword=sseq_steps @ [0],
              ~first_sword=SNode(shd),
              indentation,
            ),
          ],
        [view_of_snode(~inject, ~indent_level=Indented(indentation), shd)],
      )
    };
  view_of_sseq_line(
    ~cls="SSeqHead",
    ~sseq_steps,
    ~tm_index=0,
    vindentation @ vwords,
  );
}
and view_of_sseq_head_arg =
    (
      ~inject: Update.Action.t => Vdom.Event.t,
      ~sseq_steps: CursorPath.steps,
      ~sseq_indent_level: indent_level,
      ~tm_index: ChildIndex.t,
      sarg: snode,
    )
    : Vdom.Node.t => {
  let is_skinny = !is_multi_line(sarg);
  let (vindentation, vwords) =
    switch (sseq_indent_level |> tab, is_skinny) {
    | (Indented(_), _)
    | (_, false) => (
        [],
        [
          sspace_vnode,
          view_of_snode(~inject, ~indent_level=sseq_indent_level, sarg),
        ],
      )
    | (ToBeIndented(indentation), true) => (
        indentation == 0
          ? []
          : [
            vindentation(
              ~steps_of_first_sword=sseq_steps @ [tm_index],
              ~first_sword=SNode(sarg),
              indentation,
            ),
          ],
        [view_of_snode(~inject, ~indent_level=Indented(indentation), sarg)],
      )
    };
  view_of_sseq_line(
    ~cls="SSeqHeadArg",
    ~sseq_steps,
    ~tm_index,
    vindentation @ vwords,
  );
}
and view_of_sseq_tail =
    (
      ~inject: Update.Action.t => Vdom.Event.t,
      ~sseq_steps: CursorPath.steps,
      ~sseq_indent_level: indent_level,
      ~sseq_is_multi_line: bool,
      ~tm_index: ChildIndex.t,
      op_stokens,
      snode,
    )
    : Vdom.Node.t => {
  // trim leading space if multi-line to align
  // operator flush against left margin of sseq
  let trimmed_op_stokens =
    sseq_is_multi_line
      ? op_stokens |> filteri((i, stoken) => !(i == 0 && stoken == SSpace))
      : op_stokens;
  let op_margin =
    trimmed_op_stokens
    |> List.map(
         fun
         | SOp(_, _, s) => String.length(s)
         | SSpace => 1
         | _ => 0,
       )
    |> List.fold_left((a, b) => a + b, 0);
  let is_skinny = !is_multi_line(snode);
  let indentation =
    switch (sseq_indent_level) {
    | Indented(i)
    | ToBeIndented(i) => i
    };
  let vindentation =
    switch (sseq_indent_level) {
    | Indented(_) => []
    | ToBeIndented(_) =>
      indentation == 0
        ? []
        : [
          vindentation(
            ~steps_of_first_sword=sseq_steps @ [tm_index],
            ~first_sword=SNode(snode),
            indentation,
          ),
        ]
    };
  let vwords =
    (
      trimmed_op_stokens
      |> List.map(view_of_stoken(~inject, ~node_steps=sseq_steps))
    )
    @ [
      is_skinny
        ? view_of_snode(
            ~inject,
            ~indent_level=Indented(indentation + op_margin),
            snode,
          )
        : view_of_snode(
            ~inject,
            ~op_margin,
            ~indent_level=sseq_indent_level,
            snode,
          ),
    ];
  view_of_sseq_line(
    ~cls="SSeqTail",
    ~sseq_steps,
    ~tm_index,
    vindentation @ vwords,
  );
}
and view_of_sseq_tail_arg =
    (
      ~inject: Update.Action.t => Vdom.Event.t,
      ~sseq_steps: CursorPath.steps,
      ~sseq_indent_level: indent_level,
      ~sseq_is_multi_line: bool,
      ~tm_index: ChildIndex.t,
      op_stokens,
      snode,
    )
    : Vdom.Node.t => {
  // trim leading space if multi-line to align
  // operator flush against left margin of sseq
  let trimmed_op_stokens =
    sseq_is_multi_line
      ? op_stokens |> filteri((i, stoken) => !(i == 0 && stoken == SSpace))
      : op_stokens;
  let op_margin =
    trimmed_op_stokens
    |> List.map(
         fun
         | SOp(_, _, s) => String.length(s)
         | SSpace => 1
         | _ => 0,
       )
    |> List.fold_left((a, b) => a + b, 0);
  let is_skinny = !is_multi_line(snode);
  let (vindentation, vwords) =
    switch (sseq_indent_level |> tab, is_skinny) {
    | (Indented(_), _) => (
        [],
        [
          sspace_vnode,
          view_of_snode(~inject, ~indent_level=sseq_indent_level, snode),
        ],
      )
    | (ToBeIndented(indentation), false) => (
        [],
        (
          trimmed_op_stokens
          |> List.map(view_of_stoken(~inject, ~node_steps=sseq_steps))
        )
        @ [
          view_of_snode(
            ~inject,
            ~indent_level=ToBeIndented(indentation + op_margin),
            snode,
          ),
        ],
      )
    | (ToBeIndented(indentation), true) => (
        [
          vindentation(
            ~steps_of_first_sword=sseq_steps @ [tm_index],
            ~first_sword=SNode(snode),
            indentation + op_margin,
          ),
        ],
        [
          view_of_snode(
            ~inject,
            ~indent_level=Indented(indentation + op_margin),
            snode,
          ),
        ],
      )
    };
  view_of_sseq_line(
    ~cls="SSeqTailArg",
    ~sseq_steps,
    ~tm_index,
    vindentation @ vwords,
  );
}
and view_of_stoken =
    (
      ~inject as _: Update.Action.t => Vdom.Event.t, /* we'll need `inject` in the future e.g. invoking node staging by clicking and dragging a delimiter */
      ~node_steps: CursorPath.steps,
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
              CursorPath.sexp_of_t((node_steps, OnDelim(0, Before))),
            ),
          ),
          Attr.create(
            "path-after",
            Sexp.to_string(
              CursorPath.sexp_of_t((node_steps, OnDelim(0, After))),
            ),
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
  | SDelim(index, s) =>
    open Vdom;
    let delim_before =
      Node.span(
        [
          Attr.id(path_id((node_steps, OnDelim(index, Before)))),
          Attr.classes(["SDelim-before", "unselectable-before"]),
        ],
        [Node.text(LangUtil.nondisplay3)],
      );
    let delim_after =
      Node.span(
        [
          Attr.id(path_id((node_steps, OnDelim(index, After)))),
          Attr.classes(["SDelim-after", "unselectable-after"]),
        ],
        [Node.text(LangUtil.nondisplay3)],
      );
    let delim_txt =
      Node.span(
        [
          Attr.classes(["SDelim-txt", "unselectable"]),
          Attr.create("contenteditable", "false"),
          Attr.create(
            "path-before",
            Sexp.to_string(
              CursorPath.sexp_of_t((node_steps, OnDelim(index, Before))),
            ),
          ),
          Attr.create(
            "path-after",
            Sexp.to_string(
              CursorPath.sexp_of_t((node_steps, OnDelim(index, After))),
            ),
          ),
        ],
        [Node.text(s)],
      );
    Node.div(
      [
        Attr.id(delim_id((node_steps, index))),
        Attr.classes([inline_div_cls, "SDelim"]),
      ],
      [delim_before, delim_txt, delim_after],
    );
  | SOp(index, err, s) =>
    open Vdom;
    let op_before =
      Node.span(
        [
          Attr.id(path_id((node_steps, OnDelim(index, Before)))),
          Attr.classes(["SOp-before", "unselectable-before"]),
        ],
        [Node.text(LangUtil.nondisplay3)],
      );
    let op_after =
      Node.span(
        [
          Attr.id(path_id((node_steps, OnDelim(index, After)))),
          Attr.classes(["SOp-after", "unselectable-after"]),
        ],
        [Node.text(LangUtil.nondisplay3)],
      );
    let op_txt =
      Node.span(
        [
          Attr.classes(["SOp-txt", "unselectable"]),
          Attr.create("contenteditable", "false"),
          Attr.create(
            "path-before",
            Sexp.to_string(
              CursorPath.sexp_of_t((node_steps, OnDelim(index, Before))),
            ),
          ),
          Attr.create(
            "path-after",
            Sexp.to_string(
              CursorPath.sexp_of_t((node_steps, OnDelim(index, After))),
            ),
          ),
        ],
        [Node.text(s)],
      );
    Node.div(
      [
        Attr.classes(
          [inline_div_cls, "SOp"]
          @ (
            switch (err) {
            | NotInHole => []
            | InHole(_, _) => ["in-hole"]
            }
          ),
        ),
        Attr.id(op_id(node_steps, index)),
      ],
      [op_before, op_txt, op_after],
    );
  | SText(var_err, s) =>
    Vdom.(
      Node.div(
        [
          Attr.id(text_id(node_steps)),
          Attr.classes([inline_div_cls, "SText"] @ var_err_clss(var_err)),
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
  | SReadOnly(s) =>
    Vdom.(
      Node.div(
        [Attr.classes([inline_div_cls, "SReadOnly"])],
        [Node.text(s)],
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
  | SSpace => sspace_vnode
  };

let caret_position_of_path =
    ((steps, cursor) as path: CursorPath.t): option((Js.t(Dom.node), int)) =>
  switch (cursor) {
  | Staging(_) => None
  | OnDelim(_, _) =>
    switch (JSUtil.get_elem_by_id(path_id(path))) {
    | None => None
    | Some(anchor_parent) =>
      let has_cls = cls => anchor_parent |> JSUtil.elem_has_cls(cls);
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
      Some((anchor, elem |> JSUtil.elem_has_cls("SEmptyLine") ? 2 : j));
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

let snode_of_EmptyHole =
    (~ap_err_status=NotInApHole, ~steps, hole_name: string): snode =>
  mk_SBox(
    ~ap_err_status,
    ~steps,
    ~shape=EmptyHole,
    [
      mk_SLine(
        ~steps_of_first_sword=steps,
        [SToken(SEmptyHole(hole_name))],
      ),
    ],
  );

let snode_of_Var =
    (~ap_err_status=NotInApHole, ~err, ~var_err, ~steps, x: Var.t): snode =>
  mk_SBox(
    ~ap_err_status,
    ~err,
    ~steps,
    ~shape=Var,
    [
      mk_SLine(
        ~steps_of_first_sword=steps,
        [SToken(mk_SText(~var_err, x))],
      ),
    ],
  );

let snode_of_NumLit =
    (~ap_err_status=NotInApHole, ~err, ~steps, n: int): snode =>
  mk_SBox(
    ~ap_err_status,
    ~err,
    ~steps,
    ~shape=NumLit,
    [
      mk_SLine(
        ~steps_of_first_sword=steps,
        [SToken(mk_SText(string_of_int(n)))],
      ),
    ],
  );

let snode_of_BoolLit =
    (~ap_err_status=NotInApHole, ~err, ~steps, b: bool): snode =>
  mk_SBox(
    ~ap_err_status,
    ~err,
    ~steps,
    ~shape=BoolLit,
    [
      mk_SLine(
        ~steps_of_first_sword=steps,
        [SToken(mk_SText(string_of_bool(b)))],
      ),
    ],
  );

let snode_of_ListNil = (~ap_err_status=NotInApHole, ~err, ~steps, ()): snode =>
  mk_SBox(
    ~ap_err_status,
    ~err,
    ~steps,
    ~shape=ListNil,
    [
      mk_SLine(
        ~steps_of_first_sword=steps,
        [SToken(mk_SDelim(~index=0, "[]"))],
      ),
    ],
  );

let snode_of_LetLine =
    (~user_newlines=?, ~steps, sp: snode, sann: option(snode), sdef: snode) => {
  let is_multi_line =
    user_newlines
    |> Opt.map_default(
         ~default=false,
         Model.user_entered_newline_at(steps @ [2]),
       )
    |> (user_newline => user_newline || is_multi_line(sdef));
  let slines = [
    mk_SLine(
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
      @ [SToken(SSpace), SToken(mk_SDelim(~index=2, "=")), SToken(SSpace)],
    ),
    mk_SLine(~rel_indent=1, [SNode(sdef)]),
    mk_SLine(
      is_multi_line
        ? [SToken(mk_SDelim(~index=3, "in"))]
        : [SToken(SSpace), SToken(mk_SDelim(~index=3, "in"))],
    ),
  ];
  mk_SBox(~steps, ~shape=LetLine, ~is_multi_line, slines);
};

let snode_of_Parenthesized =
    (~user_newlines=?, ~ap_err_status=NotInApHole, ~steps, sbody: snode)
    : snode => {
  let is_multi_line =
    user_newlines
    |> Opt.map_default(
         ~default=false,
         Model.user_entered_newline_at(steps @ [0]),
       )
    |> (user_newline => user_newline || is_multi_line(sbody));
  mk_SBox(
    ~ap_err_status,
    ~steps,
    ~shape=Parenthesized,
    ~is_multi_line,
    [
      mk_SLine(
        ~steps_of_first_sword=steps,
        [SToken(mk_SDelim(~index=0, "("))],
      ),
      mk_SLine(
        ~rel_indent=1,
        ~steps_of_first_sword=steps_of_snode(sbody),
        [SNode(sbody)],
      ),
      mk_SLine(
        ~steps_of_first_sword=steps,
        [SToken(mk_SDelim(~index=1, ")"))],
      ),
    ],
  );
};

let snode_of_List = (~steps, sbody: snode): snode =>
  mk_SBox(
    ~steps,
    ~shape=List,
    [
      mk_SLine(
        ~steps_of_first_sword=steps,
        [SToken(mk_SDelim(~index=0, "List("))],
      ),
      mk_SLine(
        ~rel_indent=1,
        ~steps_of_first_sword=steps_of_snode(sbody),
        [SNode(sbody)],
      ),
      mk_SLine(
        ~steps_of_first_sword=steps,
        [SToken(mk_SDelim(~index=1, ")"))],
      ),
    ],
  );

let snode_of_OpSeq =
    (
      ~steps,
      (shd, shd_args) as shead: spaced_stms,
      stail: list((op_stokens, spaced_stms)),
    )
    : snode => {
  mk_SSeq(
    ~is_multi_line=
      is_multi_line(shd)
      || shd_args
      |> List.exists(is_multi_line)
      || stail
      |> List.exists(((_, (stm, stm_args))) =>
           is_multi_line(stm) || stm_args |> List.exists(is_multi_line)
         ),
    ~steps,
    shead,
    stail,
  );
};

let snode_of_Lam =
    (
      ~user_newlines=?,
      ~ap_err_status=NotInApHole,
      ~err,
      ~steps,
      sarg: snode,
      sann: option(snode),
      sbody: snode,
    )
    : snode => {
  let is_multi_line =
    user_newlines
    |> Opt.map_default(
         ~default=false,
         Model.user_entered_newline_at(steps @ [2]),
       )
    |> (user_newline => user_newline || is_multi_line(sbody));
  let swords_ann =
    switch (sann) {
    | None => []
    | Some(sann) => [SToken(mk_SDelim(~index=1, ":")), SNode(sann)]
    };
  mk_SBox(
    ~ap_err_status,
    ~err,
    ~steps,
    ~shape=Lam,
    ~is_multi_line,
    [
      mk_SLine(
        ~steps_of_first_sword=steps,
        [SToken(mk_SDelim(~index=0, LangUtil.lamSym)), SNode(sarg)]
        @ swords_ann
        @ [SToken(mk_SDelim(~index=2, "."))],
      ),
      mk_SLine(
        ~rel_indent=1,
        ~steps_of_first_sword=steps_of_snode(sbody),
        [SNode(sbody)],
      ),
    ],
  );
};

let snode_of_Inj =
    (
      ~user_newlines=?,
      ~ap_err_status=NotInApHole,
      ~err,
      ~steps,
      side,
      sbody: snode,
    )
    : snode => {
  let is_multi_line =
    user_newlines
    |> Opt.map_default(
         ~default=false,
         Model.user_entered_newline_at(steps @ [0]),
       )
    |> (user_newline => user_newline || is_multi_line(sbody));
  mk_SBox(
    ~ap_err_status,
    ~err,
    ~steps,
    ~shape=Inj,
    ~is_multi_line,
    [
      mk_SLine(
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
      mk_SLine(
        ~rel_indent=1,
        ~steps_of_first_sword=steps_of_snode(sbody),
        [SNode(sbody)],
      ),
      mk_SLine(
        ~steps_of_first_sword=steps,
        [SToken(mk_SDelim(~index=1, ")"))],
      ),
    ],
  );
};

let snode_of_InjAnn = (~err, ~steps, sty: snode, side, sbody: snode): snode =>
  mk_SBox(
    ~err,
    ~steps,
    ~shape=InjAnn,
    ~is_multi_line=is_multi_line(sbody),
    [
      mk_SLine(
        ~steps_of_first_sword=steps,
        [
          SToken(SReadOnly("inj[" ++ LangUtil.string_of_side(side) ++ ",")),
          SNode(sty),
          SToken(SReadOnly("](")),
        ],
      ),
      mk_SLine(
        ~rel_indent=1,
        ~steps_of_first_sword=steps_of_snode(sbody),
        [SNode(sbody)],
      ),
      mk_SLine(~steps_of_first_sword=steps, [SToken(SReadOnly(")"))]),
    ],
  );

let snode_of_Case =
    (~err, ~steps, sscrut: snode, srules: list(snode), sann: option(snode))
    : snode => {
  let sline_case =
    mk_SLine(
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
         mk_SLine(
           ~steps_of_first_sword=steps_of_snode(snode),
           [SNode(snode)],
         )
       );
  let sline_end =
    switch (sann) {
    | None =>
      mk_SLine(
        ~steps_of_first_sword=steps,
        [SToken(mk_SDelim(~index=1, "end"))],
      )
    | Some(sann) =>
      mk_SLine(
        ~steps_of_first_sword=steps,
        [SToken(mk_SDelim(~index=1, "end : ")), SNode(sann)],
      )
    };
  mk_SBox(
    ~err,
    ~steps,
    ~shape=Case,
    ~is_multi_line=true,
    [sline_case] @ slines_rules @ [sline_end],
  );
};

let snode_of_Rule = (~user_newlines=?, ~steps, sp: snode, sclause: snode) => {
  let is_multi_line =
    user_newlines
    |> Opt.map_default(
         ~default=false,
         Model.user_entered_newline_at(steps @ [1]),
       )
    |> (user_newline => user_newline || is_multi_line(sclause));
  mk_SBox(
    ~steps,
    ~shape=Rule,
    ~is_multi_line,
    [
      mk_SLine(
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
      mk_SLine(
        ~rel_indent=1,
        ~steps_of_first_sword=steps_of_snode(sclause),
        [SNode(sclause)],
      ),
    ],
  );
};

let snode_of_Triv = (~err, ~steps) =>
  mk_SBox(
    ~err,
    ~steps,
    ~shape=Triv,
    [
      mk_SLine(
        ~steps_of_first_sword=steps,
        [SToken(mk_SDelim(~index=0, "()"))],
      ),
    ],
  );

let snode_of_Bool = (~steps, ()) =>
  mk_SBox(
    ~steps,
    ~shape=Bool,
    [
      mk_SLine(
        ~steps_of_first_sword=steps,
        [SToken(mk_SDelim(~index=0, "Bool"))],
      ),
    ],
  );

let snode_of_Num = (~steps, ()) =>
  mk_SBox(
    ~steps,
    ~shape=Num,
    [
      mk_SLine(
        ~steps_of_first_sword=steps,
        [SToken(mk_SDelim(~index=0, "Num"))],
      ),
    ],
  );

let snode_of_Unit = (~steps, ()) =>
  mk_SBox(
    ~steps,
    ~shape=Unit,
    [
      mk_SLine(
        ~steps_of_first_sword=steps,
        [SToken(mk_SDelim(~index=0, "()"))],
      ),
    ],
  );

let rec snode_of_typ = (~steps: CursorPath.steps=[], uty: UHTyp.t): snode =>
  switch (uty) {
  | Hole => snode_of_EmptyHole(~steps, "?")
  | Unit => snode_of_Unit(~steps, ())
  | Num => snode_of_Num(~steps, ())
  | Bool => snode_of_Bool(~steps, ())
  | Parenthesized(body) =>
    let sbody = snode_of_typ(~steps=steps @ [0], body);
    snode_of_Parenthesized(~steps, sbody);
  | List(body) =>
    let sbody = snode_of_typ(~steps=steps @ [0], body);
    snode_of_List(~steps, sbody);
  | OpSeq(skel, seq) =>
    let (head, tail) = partition_into_spaced_tms_typ(skel, seq);
    let shead = {
      let (hd, hd_args) = head;
      (
        snode_of_typ(~steps=steps @ [0], hd),
        hd_args
        |> List.mapi((i, hd_arg) =>
             snode_of_typ(~steps=steps @ [i + 1], hd_arg)
           ),
      );
    };
    let tail_start = List.length(shead |> snd) + 1;
    let (_, stail) =
      tail
      |> List.fold_left(
           (
             (k: int, stail: list((op_stokens, spaced_stms))),
             (op, spaced_tms),
           ) => {
             let (space_before, space_after) = space_before_after_op_typ(op);
             let op_stokens: list(stoken) =
               mk_op_stokens(
                 ~index=k,
                 ~space_before,
                 ~space_after,
                 string_of_op_typ(op),
               );
             let (tm, args) = spaced_tms;
             let stm = snode_of_typ(~steps=steps @ [k], tm);
             let (k, sargs) =
               args
               |> List.fold_left(
                    ((k: int, sargs: list(snode)), arg) =>
                      (
                        k + 1,
                        sargs @ [snode_of_typ(~steps=steps @ [k], arg)],
                      ),
                    (k + 1, []),
                  );
             (k, stail @ [(op_stokens, (stm, sargs))]);
           },
           (tail_start, []),
         );
    snode_of_OpSeq(~steps, shead, stail);
  };

let rec snode_of_pat =
        (~ap_err_status=NotInApHole, ~steps: CursorPath.steps=[], p: UHPat.t)
        : snode =>
  switch (p) {
  | EmptyHole(u) =>
    snode_of_EmptyHole(~ap_err_status, ~steps, string_of_int(u + 1))
  | Wild(err) =>
    mk_SBox(
      ~err,
      ~steps,
      ~shape=Wild,
      [
        mk_SLine(
          ~steps_of_first_sword=steps,
          [SToken(mk_SDelim(~index=0, "_"))],
        ),
      ],
    )
  | Var(err, var_err, x) =>
    snode_of_Var(~ap_err_status, ~err, ~var_err, ~steps, x)
  | NumLit(err, n) => snode_of_NumLit(~ap_err_status, ~err, ~steps, n)
  | BoolLit(err, b) => snode_of_BoolLit(~ap_err_status, ~err, ~steps, b)
  | ListNil(err) => snode_of_ListNil(~ap_err_status, ~err, ~steps, ())
  | Inj(err, side, body) =>
    let sbody = snode_of_pat(~steps=steps @ [0], body);
    snode_of_Inj(~ap_err_status, ~err, ~steps, side, sbody);
  | Parenthesized(body) =>
    let sbody = snode_of_pat(~steps=steps @ [0], body);
    snode_of_Parenthesized(~ap_err_status, ~steps, sbody);
  | OpSeq(skel, seq) =>
    let (head, tail) = partition_into_spaced_tms_pat(skel, seq);
    // turn head into spaced_stms
    let shead = {
      let (ap_err_status, hd, hd_args) = head;
      (
        snode_of_pat(~ap_err_status, ~steps=steps @ [0], hd),
        hd_args
        |> List.mapi((i, hd_arg) =>
             snode_of_pat(~steps=steps @ [i + 1], hd_arg)
           ),
      );
    };
    let tail_start = List.length(shead |> snd) + 1;
    let (_, stail) =
      tail
      |> List.fold_left(
           (
             (k: int, stail: list((op_stokens, spaced_stms))),
             (err, op, spaced_tms),
           ) => {
             let (space_before, space_after) = space_before_after_op_pat(op);
             let op_stokens: list(stoken) =
               mk_op_stokens(
                 ~index=k,
                 ~err,
                 ~space_before,
                 ~space_after,
                 string_of_op_pat(op),
               );
             let (ap_err_status, tm, args) = spaced_tms;
             let stm = snode_of_pat(~ap_err_status, ~steps=steps @ [k], tm);
             let (k, sargs) =
               args
               |> List.fold_left(
                    ((k: int, sargs: list(snode)), arg) =>
                      (
                        k + 1,
                        sargs @ [snode_of_pat(~steps=steps @ [k], arg)],
                      ),
                    (k + 1, []),
                  );
             (k, stail @ [(op_stokens, (stm, sargs))]);
           },
           (tail_start, []),
         );
    snode_of_OpSeq(~steps, shead, stail);
  };

let rec snode_of_block =
        (
          ~user_newlines: option(Model.user_newlines)=?,
          ~steps: CursorPath.steps=[],
          Block(line_items, e) as block: UHExp.block,
        )
        : snode => {
  let sline_items =
    line_items
    |> List.mapi((i, li) =>
         snode_of_line_item(~user_newlines?, ~steps=steps @ [i], li)
       );
  let se =
    snode_of_exp(
      ~user_newlines?,
      ~steps=steps @ [List.length(line_items)],
      e,
    );
  let has_skinny_concluding_let_line =
    UHExp.has_concluding_let_line(block)
    && (
      switch (user_newlines) {
      | None => true
      | Some(user_newlines) =>
        !(
          user_newlines
          |> Model.user_entered_newline_at(
               steps @ [List.length(line_items)],
             )
        )
      }
    );
  mk_SBox(
    ~steps,
    ~shape=Block(has_skinny_concluding_let_line),
    ~is_multi_line=UHExp.is_multi_line(block),
    sline_items
    @ [se]
    |> List.map(snode =>
         mk_SLine(
           ~steps_of_first_sword=steps_of_snode(snode),
           [SNode(snode)],
         )
       ),
  );
}
and snode_of_line_item =
    (
      ~user_newlines: option(Model.user_newlines)=?,
      ~steps: CursorPath.steps,
      li: UHExp.line,
    )
    : snode =>
  switch (li) {
  | EmptyLine =>
    mk_SBox(
      ~steps,
      ~shape=EmptyLine,
      [mk_SLine(~steps_of_first_sword=steps, [SToken(SEmptyLine)])],
    )
  | ExpLine(e) => snode_of_exp(~user_newlines?, ~steps, e) /* ghost node */
  | LetLine(p, ann, def) =>
    let sp = snode_of_pat(~steps=steps @ [0], p);
    let sann =
      switch (ann) {
      | None => None
      | Some(ann) => Some(snode_of_typ(~steps=steps @ [1], ann))
      };
    let sdef = snode_of_block(~user_newlines?, ~steps=steps @ [2], def);
    snode_of_LetLine(~user_newlines?, ~steps, sp, sann, sdef);
  }
and snode_of_exp =
    (
      ~ap_err_status=NotInApHole,
      ~user_newlines: option(Model.user_newlines)=?,
      ~steps: CursorPath.steps=[],
      e: UHExp.t,
    )
    : snode =>
  switch (e) {
  /* outer nodes */
  | EmptyHole(u) => snode_of_EmptyHole(~steps, string_of_int(u + 1))
  | Var(err, var_err, x) =>
    snode_of_Var(~ap_err_status, ~err, ~var_err, ~steps, x)
  | NumLit(err, n) => snode_of_NumLit(~err, ~steps, n)
  | BoolLit(err, b) => snode_of_BoolLit(~err, ~steps, b)
  | ListNil(err) => snode_of_ListNil(~err, ~steps, ())
  /* inner nodes */
  | Lam(err, arg, ann, body) =>
    let sarg = snode_of_pat(~steps=steps @ [0], arg);
    let sann =
      switch (ann) {
      | None => None
      | Some(ann) => Some(snode_of_typ(~steps=steps @ [1], ann))
      };
    let sbody = snode_of_block(~user_newlines?, ~steps=steps @ [2], body);
    snode_of_Lam(
      ~user_newlines?,
      ~ap_err_status,
      ~steps,
      ~err,
      sarg,
      sann,
      sbody,
    );
  | Inj(err, side, body) =>
    let sbody = snode_of_block(~user_newlines?, ~steps=steps @ [0], body);
    snode_of_Inj(~user_newlines?, ~err, ~steps, side, sbody);
  | Case(err, scrut, rules, ann) =>
    let sscrut = snode_of_block(~user_newlines?, ~steps=steps @ [0], scrut);
    let srules =
      rules
      |> List.mapi((i, rule) =>
           snode_of_rule(~user_newlines?, ~steps=steps @ [i + 1], rule)
         );
    let sann =
      switch (ann) {
      | None => None
      | Some(ann) =>
        Some(snode_of_typ(~steps=steps @ [List.length(rules) + 1], ann))
      };
    snode_of_Case(~err, ~steps, sscrut, srules, sann);
  | Parenthesized(body) =>
    let sbody = snode_of_block(~user_newlines?, ~steps=steps @ [0], body);
    snode_of_Parenthesized(~user_newlines?, ~steps, sbody);
  | OpSeq(skel, seq) =>
    let (head, tail) = partition_into_spaced_tms_exp(skel, seq);
    // turn head into spaced_stms
    let shead = {
      let (ap_err_status, hd, hd_args) = head;
      (
        snode_of_exp(~user_newlines?, ~ap_err_status, ~steps=steps @ [0], hd),
        hd_args
        |> List.mapi((i, hd_arg) =>
             snode_of_exp(~user_newlines?, ~steps=steps @ [i + 1], hd_arg)
           ),
      );
    };
    let tail_start = List.length(shead |> snd) + 1;
    // turn tail elements into op_token-spaced_stms pairs
    let (_, stail) =
      tail
      |> List.fold_left(
           (
             (k: int, stail: list((op_stokens, spaced_stms))),
             (err, op, spaced_tms),
           ) => {
             let (space_before, space_after) = space_before_after_op_exp(op);
             let op_stokens: list(stoken) =
               mk_op_stokens(
                 ~index=k,
                 ~err,
                 ~space_before,
                 ~space_after,
                 string_of_op_exp(op),
               );
             let (ap_err_status, tm, args) = spaced_tms;
             let stm =
               snode_of_exp(
                 ~user_newlines?,
                 ~ap_err_status,
                 ~steps=steps @ [k],
                 tm,
               );
             let (k, sargs) =
               args
               |> List.fold_left(
                    ((k: int, sargs: list(snode)), arg) =>
                      (
                        k + 1,
                        sargs
                        @ [
                          snode_of_exp(
                            ~user_newlines?,
                            ~steps=steps @ [k],
                            arg,
                          ),
                        ],
                      ),
                    (k + 1, []),
                  );
             (k, stail @ [(op_stokens, (stm, sargs))]);
           },
           (tail_start, []),
         );
    snode_of_OpSeq(~steps, shead, stail);
  | ApLivelit(_, _, _, _) => raise(InvariantViolated)
  | FreeLivelit(_, _) => raise(InvariantViolated) // TODO is this correct? use snode_of_Var?
  }
and snode_of_rule =
    (
      ~user_newlines: option(Model.user_newlines)=?,
      ~steps: CursorPath.steps,
      Rule(p, clause): UHExp.rule,
    ) => {
  let sp = snode_of_pat(~steps=steps @ [0], p);
  let sclause = snode_of_block(~user_newlines?, ~steps=steps @ [1], clause);
  snode_of_Rule(~user_newlines?, ~steps, sp, sclause);
};

let rec snode_of_ztyp = (~steps: CursorPath.steps, zty: ZTyp.t): snode =>
  switch (zty) {
  | CursorT(_, uty) => snode_of_typ(~steps, uty)
  | ParenthesizedZ(zbody) =>
    let szbody = snode_of_ztyp(~steps=steps @ [0], zbody);
    snode_of_Parenthesized(~steps, szbody);
  | ListZ(zbody) =>
    let szbody = snode_of_ztyp(~steps=steps @ [0], zbody);
    snode_of_List(~steps, szbody);
  | OpSeqZ(skel, ztm, surround) =>
    let seq =
      OperatorSeq.opseq_of_exp_and_surround(ZTyp.erase(ztm), surround);
    let (head, tail) = partition_into_spaced_tms_typ(skel, seq);
    let snode_of_tm_or_ztm = (k, tm) => {
      k == OperatorSeq.surround_prefix_length(surround)
        ? snode_of_ztyp(~steps=steps @ [k], ztm)
        : snode_of_typ(~steps=steps @ [k], tm);
    };
    let shead: spaced_stms = {
      let (hd, hd_args) = head;
      (
        snode_of_tm_or_ztm(0, hd),
        hd_args
        |> List.mapi((i, hd_arg) => snode_of_tm_or_ztm(i + 1, hd_arg)),
      );
    };
    let tail_start = List.length(shead |> snd) + 1;
    let (_, stail) =
      tail
      |> List.fold_left(
           (
             (k: int, stail: list((op_stokens, spaced_stms))),
             (op, spaced_tms),
           ) => {
             let (space_before, space_after) = space_before_after_op_typ(op);
             let op_stokens: list(stoken) =
               mk_op_stokens(
                 ~index=k,
                 ~space_before,
                 ~space_after,
                 string_of_op_typ(op),
               );
             let (tm, args) = spaced_tms;
             let stm: snode = snode_of_tm_or_ztm(k, tm);
             let (k, sargs) =
               args
               |> List.fold_left(
                    ((k: int, sargs: list(snode)), arg) =>
                      (k + 1, sargs @ [snode_of_tm_or_ztm(k, arg)]),
                    (k + 1, []),
                  );
             (k, stail @ [(op_stokens, (stm, sargs))]);
           },
           (tail_start, []),
         );
    snode_of_OpSeq(~steps, shead, stail);
  };

let rec snode_of_zpat =
        (~ap_err_status=NotInApHole, ~steps: CursorPath.steps, zp: ZPat.t)
        : snode =>
  switch (zp) {
  | CursorP(_, p) => snode_of_pat(~steps, p)
  | ParenthesizedZ(zbody) =>
    let szbody = snode_of_zpat(~steps=steps @ [0], zbody);
    snode_of_Parenthesized(~ap_err_status, ~steps, szbody);
  | InjZ(err, side, zbody) =>
    let szbody = snode_of_zpat(~steps=steps @ [0], zbody);
    snode_of_Inj(~ap_err_status, ~err, ~steps, side, szbody);
  | OpSeqZ(skel, ztm, surround) =>
    let seq =
      OperatorSeq.opseq_of_exp_and_surround(ZPat.erase(ztm), surround);
    let (head, tail) = partition_into_spaced_tms_pat(skel, seq);
    let snode_of_tm_or_ztm = (~ap_err_status=NotInApHole, k, tm) => {
      k == OperatorSeq.surround_prefix_length(surround)
        ? snode_of_zpat(~ap_err_status, ~steps=steps @ [k], ztm)
        : snode_of_pat(~ap_err_status, ~steps=steps @ [k], tm);
    };
    let shead: spaced_stms = {
      let (ap_err_status, hd, hd_args) = head;
      (
        snode_of_tm_or_ztm(~ap_err_status, 0, hd),
        hd_args
        |> List.mapi((i, hd_arg) => snode_of_tm_or_ztm(i + 1, hd_arg)),
      );
    };
    let tail_start = List.length(shead |> snd) + 1;
    let (_, stail) =
      tail
      |> List.fold_left(
           (
             (k: int, stail: list((op_stokens, spaced_stms))),
             (err, op, spaced_tms),
           ) => {
             let (space_before, space_after) = space_before_after_op_pat(op);
             let op_stokens: list(stoken) =
               mk_op_stokens(
                 ~index=k,
                 ~err,
                 ~space_before,
                 ~space_after,
                 string_of_op_pat(op),
               );
             let (ap_err_status, tm, args) = spaced_tms;
             let stm = snode_of_tm_or_ztm(~ap_err_status, k, tm);
             let (k, sargs) =
               args
               |> List.fold_left(
                    ((k: int, sargs: list(snode)), arg) =>
                      (k + 1, sargs @ [snode_of_tm_or_ztm(k, arg)]),
                    (k + 1, []),
                  );
             (k, stail @ [(op_stokens, (stm, sargs))]);
           },
           (tail_start, []),
         );
    snode_of_OpSeq(~steps, shead, stail);
  };

let rec snode_of_zblock =
        (
          ~user_newlines: option(Model.user_newlines)=?,
          ~steps: CursorPath.steps=[],
          zblock: ZExp.zblock,
        )
        : snode =>
  switch (zblock) {
  | BlockZL((prefix, zline_item, suffix), e) =>
    let sprefix =
      prefix
      |> List.mapi((i, li) =>
           snode_of_line_item(~user_newlines?, ~steps=steps @ [i], li)
         );
    let szline_item =
      snode_of_zline_item(
        ~user_newlines?,
        ~steps=steps @ [List.length(prefix)],
        zline_item,
      );
    let ssuffix =
      suffix
      |> List.mapi((i, li) =>
           snode_of_line_item(
             ~user_newlines?,
             ~steps=steps @ [i + List.length(prefix) + 1],
             li,
           )
         );
    let se =
      snode_of_exp(
        ~user_newlines?,
        ~steps=steps @ [List.length(prefix) + 1 + List.length(suffix)],
        e,
      );
    let num_leading = List.length(prefix) + 1 + List.length(suffix);
    let has_skinny_concluding_let_line =
      ZExp.has_concluding_let_line(zblock)
      && (
        switch (user_newlines) {
        | None => true
        | Some(user_newlines) =>
          !(
            user_newlines
            |> Model.user_entered_newline_at(steps @ [num_leading])
          )
        }
      );
    mk_SBox(
      ~steps,
      ~shape=Block(has_skinny_concluding_let_line),
      ~is_multi_line=ZExp.is_multi_line(zblock),
      sprefix
      @ [szline_item]
      @ ssuffix
      @ [se]
      |> List.map(snode =>
           mk_SLine(
             ~steps_of_first_sword=steps_of_snode(snode),
             [SNode(snode)],
           )
         ),
    );
  | BlockZE(line_items, ze) =>
    let sline_items =
      line_items
      |> List.mapi((i, li) =>
           snode_of_line_item(~user_newlines?, ~steps=steps @ [i], li)
         );
    let sze =
      snode_of_zexp(
        ~user_newlines?,
        ~steps=steps @ [List.length(line_items)],
        ze,
      );
    let has_skinny_concluding_let_line =
      ZExp.has_concluding_let_line(zblock)
      && (
        switch (user_newlines) {
        | None => true
        | Some(user_newlines) =>
          !(
            user_newlines
            |> Model.user_entered_newline_at(
                 steps @ [List.length(line_items)],
               )
          )
        }
      );
    mk_SBox(
      ~steps,
      ~shape=Block(has_skinny_concluding_let_line),
      ~is_multi_line=ZExp.is_multi_line(zblock),
      sline_items
      @ [sze]
      |> List.map(snode =>
           mk_SLine(
             ~steps_of_first_sword=steps_of_snode(snode),
             [SNode(snode)],
           )
         ),
    );
  }
and snode_of_zline_item =
    (
      ~user_newlines: option(Model.user_newlines)=?,
      ~steps: CursorPath.steps,
      zli: ZExp.zline,
    )
    : snode =>
  switch (zli) {
  | CursorL(_, li) => snode_of_line_item(~user_newlines?, ~steps, li)
  | ExpLineZ(ze) => snode_of_zexp(~user_newlines?, ~steps, ze)
  | LetLineZP(zp, ann, def) =>
    let szp = snode_of_zpat(~steps=steps @ [0], zp);
    let sann =
      switch (ann) {
      | None => None
      | Some(ann) => Some(snode_of_typ(~steps=steps @ [1], ann))
      };
    let sdef = snode_of_block(~user_newlines?, ~steps=steps @ [2], def);
    snode_of_LetLine(~user_newlines?, ~steps, szp, sann, sdef);
  | LetLineZA(p, zann, def) =>
    let sp = snode_of_pat(~steps=steps @ [0], p);
    let szann = snode_of_ztyp(~steps=steps @ [1], zann);
    let sdef = snode_of_block(~user_newlines?, ~steps=steps @ [2], def);
    snode_of_LetLine(~user_newlines?, ~steps, sp, Some(szann), sdef);
  | LetLineZE(p, ann, zdef) =>
    let sp = snode_of_pat(~steps=steps @ [0], p);
    let sann =
      switch (ann) {
      | None => None
      | Some(ann) => Some(snode_of_typ(~steps=steps @ [1], ann))
      };
    let szdef = snode_of_zblock(~user_newlines?, ~steps=steps @ [2], zdef);
    snode_of_LetLine(~user_newlines?, ~steps, sp, sann, szdef);
  }
and snode_of_zexp =
    (
      ~user_newlines: option(Model.user_newlines)=?,
      ~ap_err_status=NotInApHole,
      ~steps: CursorPath.steps,
      ze: ZExp.t,
    ) =>
  switch (ze) {
  | CursorE(_, e) => snode_of_exp(~user_newlines?, ~steps, e)
  | ParenthesizedZ(zbody) =>
    let szbody = snode_of_zblock(~user_newlines?, ~steps=steps @ [0], zbody);
    snode_of_Parenthesized(~user_newlines?, ~ap_err_status, ~steps, szbody);
  | OpSeqZ(skel, ztm, surround) =>
    let seq =
      OperatorSeq.opseq_of_exp_and_surround(ZExp.erase(ztm), surround);
    let (head, tail) = partition_into_spaced_tms_exp(skel, seq);
    let snode_of_tm_or_ztm = (~ap_err_status=NotInApHole, k, tm) => {
      k == OperatorSeq.surround_prefix_length(surround)
        ? snode_of_zexp(
            ~user_newlines?,
            ~ap_err_status,
            ~steps=steps @ [k],
            ztm,
          )
        : snode_of_exp(
            ~user_newlines?,
            ~ap_err_status,
            ~steps=steps @ [k],
            tm,
          );
    };
    let shead: spaced_stms = {
      let (ap_err_status, hd, hd_args) = head;
      (
        snode_of_tm_or_ztm(~ap_err_status, 0, hd),
        hd_args
        |> List.mapi((i, hd_arg) => snode_of_tm_or_ztm(i + 1, hd_arg)),
      );
    };
    let tail_start = List.length(shead |> snd) + 1;
    let (_, stail) =
      tail
      |> List.fold_left(
           (
             (k: int, stail: list((op_stokens, spaced_stms))),
             (err, op, spaced_tms),
           ) => {
             let (space_before, space_after) = space_before_after_op_exp(op);
             let op_stokens: list(stoken) =
               mk_op_stokens(
                 ~index=k,
                 ~err,
                 ~space_before,
                 ~space_after,
                 string_of_op_exp(op),
               );
             let (ap_err_status, tm, args) = spaced_tms;
             let stm = snode_of_tm_or_ztm(~ap_err_status, k, tm);
             let (k: int, sargs: list(snode)) =
               args
               |> List.fold_left(
                    ((k: int, sargs: list(snode)), arg: UHExp.t) =>
                      (k + 1, sargs @ [snode_of_tm_or_ztm(k, arg)]),
                    (k + 1, []),
                  );
             (k, stail @ [(op_stokens, (stm, sargs))]);
           },
           (tail_start, []),
         );
    snode_of_OpSeq(~steps, shead, stail);
  | LamZP(err, zarg, ann, body) =>
    let szarg = snode_of_zpat(~steps=steps @ [0], zarg);
    let sann =
      switch (ann) {
      | None => None
      | Some(ann) => Some(snode_of_typ(~steps=steps @ [1], ann))
      };
    let sbody = snode_of_block(~user_newlines?, ~steps=steps @ [2], body);
    snode_of_Lam(
      ~user_newlines?,
      ~ap_err_status,
      ~steps,
      ~err,
      szarg,
      sann,
      sbody,
    );
  | LamZA(err, arg, zann, body) =>
    let sarg = snode_of_pat(~steps=steps @ [0], arg);
    let szann = snode_of_ztyp(~steps=steps @ [1], zann);
    let sbody = snode_of_block(~user_newlines?, ~steps=steps @ [2], body);
    snode_of_Lam(
      ~user_newlines?,
      ~ap_err_status,
      ~steps,
      ~err,
      sarg,
      Some(szann),
      sbody,
    );
  | LamZE(err, arg, ann, zbody) =>
    let sarg = snode_of_pat(~steps=steps @ [0], arg);
    let sann =
      switch (ann) {
      | None => None
      | Some(ann) => Some(snode_of_typ(~steps=steps @ [1], ann))
      };
    let szbody = snode_of_zblock(~user_newlines?, ~steps=steps @ [2], zbody);
    snode_of_Lam(
      ~user_newlines?,
      ~ap_err_status,
      ~steps,
      ~err,
      sarg,
      sann,
      szbody,
    );
  | InjZ(err, side, zbody) =>
    let szbody = snode_of_zblock(~user_newlines?, ~steps=steps @ [0], zbody);
    snode_of_Inj(~user_newlines?, ~ap_err_status, ~err, ~steps, side, szbody);
  | CaseZE(err, zscrut, rules, ann) =>
    let szscrut =
      snode_of_zblock(~user_newlines?, ~steps=steps @ [0], zscrut);
    let srules =
      rules
      |> List.mapi((i, rule) =>
           snode_of_rule(~user_newlines?, ~steps=steps @ [i + 1], rule)
         );
    let sann =
      switch (ann) {
      | None => None
      | Some(ann) =>
        Some(snode_of_typ(~steps=steps @ [List.length(rules) + 1], ann))
      };
    snode_of_Case(~err, ~steps, szscrut, srules, sann);
  | CaseZR(err, scrut, (prefix, zrule, suffix), ann) =>
    let sscrut = snode_of_block(~user_newlines?, ~steps=steps @ [0], scrut);
    let szrules =
      (
        prefix
        |> List.mapi((i, rule) =>
             snode_of_rule(~user_newlines?, ~steps=steps @ [i + 1], rule)
           )
      )
      @ [
        snode_of_zrule(
          ~user_newlines?,
          ~steps=steps @ [List.length(prefix) + 1],
          zrule,
        ),
      ]
      @ (
        suffix
        |> List.mapi((i, rule) =>
             snode_of_rule(
               ~user_newlines?,
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
    snode_of_Case(~err, ~steps, sscrut, szrules, sann);
  | CaseZA(err, scrut, rules, zann) =>
    let sscrut = snode_of_block(~user_newlines?, ~steps=steps @ [0], scrut);
    let srules =
      rules
      |> List.mapi((i, rule) =>
           snode_of_rule(~user_newlines?, ~steps=steps @ [i + 1], rule)
         );
    let szann =
      snode_of_ztyp(~steps=steps @ [List.length(rules) + 1], zann);
    snode_of_Case(~err, ~steps, sscrut, srules, Some(szann));
  | ApLivelitZ(_, _, _, _) => raise(InvariantViolated)
  }
and snode_of_zrule =
    (~user_newlines: option(Model.user_newlines)=?, ~steps, zrule) =>
  switch (zrule) {
  | CursorR(_, rule) => snode_of_rule(~user_newlines?, ~steps, rule)
  | RuleZP(zp, clause) =>
    let sp = snode_of_zpat(~steps=steps @ [0], zp);
    let sclause =
      snode_of_block(~user_newlines?, ~steps=steps @ [1], clause);
    snode_of_Rule(~user_newlines?, ~steps, sp, sclause);
  | RuleZE(p, zclause) =>
    let sp = snode_of_pat(~steps=steps @ [0], p);
    let sclause =
      snode_of_zblock(~user_newlines?, ~steps=steps @ [1], zclause);
    snode_of_Rule(~user_newlines?, ~steps, sp, sclause);
  };

let view_of_zblock =
    (
      ~inject: Update.Action.t => Vdom.Event.t,
      ~user_newlines,
      zblock: ZExp.zblock,
    )
    : Vdom.Node.t => {
  view_of_snode(~inject, snode_of_zblock(~user_newlines, zblock));
};

let view_of_block =
    (
      ~inject: Update.Action.t => Vdom.Event.t,
      ~user_newlines: Model.user_newlines,
      block: UHExp.block,
    )
    : Vdom.Node.t => {
  view_of_snode(~inject, snode_of_block(~user_newlines, block));
};

module DHPat = Dynamics.DHPat;
module DHExp = Dynamics.DHExp;

let cls_of_inst = ((u, i)) =>
  "hole-instance-" ++ string_of_int(u) ++ "-" ++ string_of_int(i);

let maybe_parenthesize = (parenthesize, sline) =>
  parenthesize
    ? [SToken(SReadOnly("("))] @ sline @ [SToken(SReadOnly(")"))] : sline;

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
let precedence_Minus = 3;
let precedence_Cons = 4;
let precedence_LessThan = 5;
let precedence_GreaterThan = 5;
let precedence_Equals = 5;
let precedence_And = 6;
let precedence_Or = 7;
let precedence_Comma = 8;
let precedence_max = 9;
let precedence_dhpat = (dp: DHPat.t) =>
  switch (dp) {
  | EmptyHole(_)
  | NonEmptyHole(_, _, _, _)
  | Wild
  | IntermediateKW(_, _, _)
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
  | FreeLivelit(_, _, _, _)
  | IntermediateKW(_, _, _, _)
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
  | BinNumOp(Minus, _, _) => precedence_Minus
  | BinNumOp(LessThan, _, _) => precedence_LessThan
  | BinNumOp(GreaterThan, _, _) => precedence_GreaterThan
  | BinNumOp(Equals, _, _) => precedence_Equals
  | Cons(_, _) => precedence_Cons
  | NonEmptyHole(_, _, _, _, d1) => precedence_dhexp(d1)
  | And(_, _) => precedence_And
  | Or(_, _) => precedence_Or
  };

let snode_of_BinOp =
    (
      ~mb_par=maybe_parenthesize(false),
      ~err: ErrStatus.t=NotInHole,
      ~steps,
      s1: snode,
      sop: string,
      s2: snode,
    )
    : snode =>
  mk_SBox(
    ~err,
    ~steps,
    ~shape=SkelBinOp,
    [mk_SLine(mb_par([SNode(s1), SToken(SReadOnly(sop)), SNode(s2)]))],
  );

let snode_of_SpaceOp = (~err, ~steps, s1: snode, s2: snode): snode =>
  mk_SBox(
    ~err,
    ~steps,
    ~shape=SkelBinOp,
    [mk_SLine([SNode(s1), SToken(SSpace), SNode(s2)])],
  );

let snode_of_Let = (~err, ~steps, sp: snode, sdef: snode, sbody: snode): snode =>
  mk_SBox(
    ~err,
    ~steps,
    ~shape=Let,
    ~is_multi_line=true,
    [
      mk_SLine([
        SToken(SReadOnly("let")),
        SNode(sp),
        SToken(SReadOnly("=")),
      ]),
      mk_SLine(~rel_indent=1, [SNode(sdef)]),
      mk_SLine([SNode(sbody)]),
    ],
  );

let snode_of_FixF =
    (~err, ~steps, sarg: snode, sty: snode, sbody: snode): snode =>
  mk_SBox(
    ~err,
    ~steps,
    ~shape=FixF,
    ~is_multi_line=is_multi_line(sbody),
    [
      mk_SLine([
        SToken(SReadOnly("fix")),
        SNode(sarg),
        SToken(SReadOnly(":")),
        SNode(sty),
        SToken(SReadOnly(".")),
      ]),
      mk_SLine(~rel_indent=1, [SNode(sbody)]),
    ],
  );

let rec snode_of_htyp =
        (~parenthesize=false, ~steps: CursorPath.steps=[], ty: HTyp.t): snode => {
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
        (
          ~parenthesize=false,
          ~err: ErrStatus.t=NotInHole,
          ~steps=[],
          dp: DHPat.t,
        )
        : snode => {
  let mb_par = maybe_parenthesize(parenthesize);
  switch (dp) {
  | EmptyHole(u, i) =>
    /* TODO add SHOW_SIGMAS flag */
    mk_SBox(
      ~steps,
      ~shape=EmptyHoleInstance(u, i, None),
      [mk_SLine(mb_par([SToken(SEmptyHole(hole_label_of(u, i)))]))],
    )
  | NonEmptyHole(reason, u, i, dp1) =>
    /* TODO revisit this and consider whether err should be on sp1 or parent */
    let sp1 =
      snode_of_dhpat(~err=InHole(reason, u), ~steps=steps @ [0], dp1);
    mk_SBox(
      ~steps,
      ~shape=NonEmptyHoleInstance(reason, u, i, None),
      [mk_SLine(mb_par([SNode(sp1)]))],
    );
  | Wild =>
    mk_SBox(
      ~err,
      ~steps,
      ~shape=Wild,
      [mk_SLine([SToken(mk_SDelim(~index=0, "_"))])],
    )
  | IntermediateKW(u, _, kw) =>
    snode_of_Var(
      ~err,
      ~var_err=InVarHole(Keyword(kw), u),
      ~steps,
      IntermediateKeyword.to_var(kw),
    )
  | Var(x) => snode_of_Var(~err, ~var_err=NotInVarHole, ~steps, x)
  | BoolLit(b) => snode_of_BoolLit(~err, ~steps, b)
  | NumLit(n) => snode_of_NumLit(~err, ~steps, n)
  | Triv => snode_of_Triv(~err, ~steps)
  | ListNil => snode_of_ListNil(~err, ~steps, ())
  | Inj(side, dp1) =>
    let sp1 = snode_of_dhpat(~steps=steps @ [0], dp1);
    snode_of_Inj(~err, ~steps, side, sp1);
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
    snode_of_BinOp(~err, ~steps, sp1, string_of_op_pat(Cons), sp2);
  | Pair(dp1, dp2) =>
    let sp1 = snode_of_dhpat(~steps=steps @ [0], dp1);
    let sp2 = snode_of_dhpat(~steps=steps @ [1], dp2);
    snode_of_BinOp(~err, ~steps, sp1, string_of_op_pat(Comma), sp2);
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
    snode_of_SpaceOp(~err, ~steps, sp1, sp2);
  };
};

let rec snode_of_dhexp =
        (
          ~parenthesize=false,
          ~err: ErrStatus.t=NotInHole,
          ~steps=[],
          d: DHExp.t,
        )
        : snode => {
  let mb_par = maybe_parenthesize(parenthesize);
  switch (d) {
  | EmptyHole(u, i, sigma) =>
    /* TODO add SHOW_SIGMAS flag */
    mk_SBox(
      ~steps,
      ~shape=EmptyHoleInstance(u, i, Some(sigma)),
      [mk_SLine(mb_par([SToken(SEmptyHole(hole_label_of(u, i)))]))],
    )
  | NonEmptyHole(reason, u, i, sigma, d1) =>
    let s1 = snode_of_dhexp(~err=InHole(reason, u), ~steps=steps @ [0], d1);
    /* TODO add SHOW_SIGMAS flag */
    mk_SBox(
      ~steps,
      ~shape=NonEmptyHoleInstance(reason, u, i, Some(sigma)),
      [mk_SLine(mb_par([SNode(s1)]))],
    );
  | Triv => snode_of_Triv(~err, ~steps)
  | BoolLit(b) => snode_of_BoolLit(~err, ~steps, b)
  | NumLit(n) => snode_of_NumLit(~err, ~steps, n)
  | ListNil(_) => snode_of_ListNil(~err, ~steps, ())
  | BoundVar(x) => snode_of_Var(~err, ~var_err=NotInVarHole, ~steps, x)
  | FreeVar(u, _, _, x) =>
    snode_of_Var(~err, ~var_err=InVarHole(Free, u), ~steps, x)
  | FreeLivelit(_, _, _, _) =>
    // TODO snode_of_Var(~err_status, ~var_err_status=InVHole(Free, u), ~steps, x)
    raise(InvariantViolated)
  | IntermediateKW(u, _, _, kw) =>
    snode_of_Var(
      ~err,
      ~var_err=InVarHole(Keyword(kw), u),
      ~steps,
      IntermediateKeyword.to_var(kw),
    )
  | Let(dp, ddef, dbody) =>
    let sp = snode_of_dhpat(~steps=steps @ [0], dp);
    let sdef = snode_of_dhexp(~steps=steps @ [1], ddef);
    let sbody = snode_of_dhexp(~steps=steps @ [2], dbody);
    snode_of_Let(~err, ~steps, sp, sdef, sbody);
  | FixF(x, ty, dbody) =>
    let sx =
      snode_of_Var(
        ~err=NotInHole,
        ~var_err=NotInVarHole,
        ~steps=steps @ [0],
        x,
      );
    let sty = snode_of_htyp(~steps=steps @ [1], ty);
    let sbody = snode_of_dhexp(~steps=steps @ [2], dbody);
    snode_of_FixF(~err, ~steps, sx, sty, sbody);
  | Lam(darg, dann, dbody) =>
    let sarg = snode_of_dhpat(~steps=steps @ [0], darg);
    let sann = snode_of_htyp(~steps=steps @ [1], dann);
    let sbody = snode_of_dhexp(~steps=steps @ [2], dbody);
    snode_of_Lam(~err, ~steps, sarg, Some(sann), sbody);
  | Inj(ty, side, dbody) =>
    let sty = snode_of_htyp(~steps=steps @ [0], ty);
    let sbody = snode_of_dhexp(~steps=steps @ [1], dbody);
    snode_of_InjAnn(~err, ~steps, sty, side, sbody);
  | Case(dscrut, drules, _) =>
    /* TODO: probably need to do something with current rule */
    /* | Case(d1, (x, d2), (y, d3)) => */
    let sscrut = snode_of_dhexp(~steps=steps @ [0], dscrut);
    let srules =
      drules
      |> List.mapi((i, drule) =>
           snode_of_drule(~steps=steps @ [i + 1], drule)
         );
    snode_of_Case(~err, ~steps, sscrut, srules, None);
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
    snode_of_BinOp(~err, ~steps, s1, sop, s2);
  | And(d1, d2) =>
    let sop = string_of_op_exp(And);
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
    snode_of_BinOp(~err, ~steps, s1, sop, s2);
  | Or(d1, d2) =>
    let sop = string_of_op_exp(Or);
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
    snode_of_BinOp(~err, ~steps, s1, sop, s2);
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
    snode_of_SpaceOp(~err, ~steps, s1, s2);
  | Pair(d1, d2) =>
    let s1 = snode_of_dhexp(~steps=steps @ [0], d1);
    let s2 = snode_of_dhexp(~steps=steps @ [1], d2);
    snode_of_BinOp(~err, ~steps, s1, string_of_op_exp(Comma), s2);
  | Cons(d1, d2) =>
    let s1 = snode_of_dhexp(~steps=steps @ [0], d1);
    let s2 = snode_of_dhexp(~steps=steps @ [1], d2);
    snode_of_BinOp(~err, ~steps, s1, string_of_op_exp(Cons), s2);
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
      ~err,
      ~steps,
      ~shape=Cast,
      _SHOW_CASTS
        ? [
          mk_SLine([
            SNode(s1),
            SToken(SReadOnly("<")),
            SNode(sty1),
            SToken(SCastArrow),
            SNode(sty2),
            SToken(SCastArrow),
            SNode(sty3),
            SToken(SReadOnly(">")),
          ]),
        ]
        : [mk_SLine([SNode(s1)])],
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
      ~err,
      ~steps,
      ~shape=Cast,
      _SHOW_CASTS
        ? [
          mk_SLine([
            SNode(s1),
            SToken(SReadOnly("<")),
            SNode(sty1),
            SToken(SCastArrow),
            SNode(sty2),
            SToken(SReadOnly(">")),
          ]),
        ]
        : [mk_SLine([SNode(s1)])],
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
      ~err,
      ~steps,
      ~shape=FailedCast,
      [
        mk_SLine([
          SNode(s1),
          SToken(SReadOnly("<")),
          SNode(sty1),
          SToken(SFailedCastArrow),
          SNode(sty2),
          SToken(SFailedCastArrow),
          SNode(sty3),
          SToken(SReadOnly(">")),
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
      ~err,
      ~steps,
      ~shape=FailedCast,
      [
        mk_SLine([
          SNode(s1),
          SToken(SReadOnly("<")),
          SNode(sty1),
          SToken(SFailedCastArrow),
          SNode(sty2),
          SToken(SReadOnly(">")),
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
  view_of_snode(~inject, ~indent_level=Indented(0), snode_of_htyp(ty));

let view_of_dhexp = (~inject, d) =>
  view_of_snode(~inject, snode_of_dhexp(d));

let view_of_result =
    (~inject: Update.Action.t => Vdom.Event.t, model: Model.t): Vdom.Node.t =>
  switch (model.result_state) {
  | ResultsDisabled => Vdom.Node.div([], [])
  | Result(has_result_state) =>
    let result = has_result_state.result;
    switch (result) {
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
  };

let view_of_hole_instance =
    (~inject: Update.Action.t => Vdom.Event.t, (u, i): DHExp.HoleInstance.t) =>
  view_of_dhexp(~inject, EmptyHole(u, i, []));

let view_of_Var =
    (
      ~inject: Update.Action.t => Vdom.Event.t,
      ~err: ErrStatus.t=NotInHole,
      ~var_err: VarErrStatus.t=NotInVarHole,
      ~steps: CursorPath.steps=[],
      x: Var.t,
    ) =>
  view_of_snode(~inject, snode_of_Var(~err, ~var_err, ~steps, x));

let is_multi_line_exp = e => snode_of_exp(e) |> is_multi_line;
let is_multi_line_pat = p => snode_of_pat(p) |> is_multi_line;
let is_multi_line_typ = ty => snode_of_typ(ty) |> is_multi_line;
