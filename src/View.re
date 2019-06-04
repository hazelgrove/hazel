let _SHOW_CASTS = false;
let _SHOW_FN_BODIES = false;

/* Imports */
exception InvariantViolated;
module PP = Pretty.PP;
open SemanticsCommon;
open GeneralUtil;

/* Conveniences */
let (^^) = PP.(^^);
let taggedText = (classes, s) => PP.text(classes, s);
let dollar = taggedText(["dollar"], "$");
let kw = (~classes=[], s) => taggedText(["kw", ...classes], s);
let lparen = (~classes=[], s) => taggedText(["lparen", ...classes], s);
let rparen = (~classes=[], s) => taggedText(["rparen", ...classes], s);
let op = taggedText(["op"]);
let var = taggedText(["var"]);
let paletteName = s => taggedText(["paletteName"], s);
let space = taggedText(["space"], " ");
let smallSpace = taggedText(["small-space"], " ");

/* Helpers */
let rec id_of_rev_path = (prefix: string, rev_path: Path.steps): PP.id =>
  switch (rev_path) {
  | [] => prefix ++ "path_"
  | [x, ...xs] => id_of_rev_path(prefix, xs) ++ "_" ++ string_of_int(x)
  };

let cls_from_classes =
    (err_status: err_status, classes: list(PP.cls)): list(PP.cls) =>
  switch (err_status) {
  | InHole(_, u) => [
      "in_err_hole",
      "in_err_hole_" ++ string_of_int(u),
      ...classes,
    ]
  | NotInHole => classes
  };

let cls_from = (err_status: err_status, cls: PP.cls): list(PP.cls) =>
  switch (err_status) {
  | InHole(_, u) => ["in_err_hole", "in_err_hole_" ++ string_of_int(u), cls]
  | NotInHole => [cls]
  };

let before_child_cls = (before_child_index: int): PP.cls =>
  "before-child-" ++ string_of_int(before_child_index);
let closing_delimiter_cls: PP.cls = "closing-delimiter";
let before_child_skel_cls = (before_child_index: int): PP.cls =>
  "before-child-" ++ string_of_int(before_child_index) ++ "-skel";

let forceGetSkelElement = (id, op_index) => {
  let doc = Js_of_ocaml.Dom_html.document;
  let cls = before_child_skel_cls(op_index);
  Js_of_ocaml.Js.Opt.get(
    doc##querySelector(Js_of_ocaml.Js.string("#" ++ id ++ "." ++ cls)),
    () => {
      JSUtil.log("Skel node not found: id = " ++ id ++ ", cls = " ++ cls);
      assert(false);
    },
  );
};

let before_child_cls_regexp =
  Js_of_ocaml.Regexp.regexp("before-child-(\\d+)");
let before_child_index_of_cls = (cls: PP.cls): option(int) =>
  switch (Js_of_ocaml.Regexp.string_match(before_child_cls_regexp, cls, 0)) {
  | None => None
  | Some(result) =>
    switch (Js_of_ocaml.Regexp.matched_group(result, 1)) {
    | None => None
    | Some(s) => Some(int_of_string(s))
    }
  };
let before_child_skel_cls_regexp =
  Js_of_ocaml.Regexp.regexp("before-child-(\\d+)-skel");
let before_child_index_of_skel_cls = (cls: PP.cls): option(int) =>
  switch (
    Js_of_ocaml.Regexp.string_match(before_child_skel_cls_regexp, cls, 0)
  ) {
  | None => None
  | Some(result) =>
    switch (Js_of_ocaml.Regexp.matched_group(result, 1)) {
    | None => None
    | Some(s) => Some(int_of_string(s))
    }
  };

let term =
    (
      ~preserved_on_deletion=false,
      prefix: string,
      err_status: err_status,
      rev_path: Path.steps,
      classes: list(PP.cls),
      doc: PP.doc,
    )
    : PP.doc => {
  let id' = id_of_rev_path(prefix, rev_path);
  PP.tagged(
    (preserved_on_deletion ? ["preserved-on-deletion"] : [])
    @ ["node", ...cls_from_classes(err_status, classes)],
    Some((id', rev_path)),
    None,
    doc,
  );
};

let block =
    (
      ~preserved_on_deletion=false,
      prefix: string,
      rev_path: Path.steps,
      cls: PP.cls,
      doc: PP.doc,
    )
    : PP.doc => {
  let id' = id_of_rev_path(prefix, rev_path);
  PP.tagged(
    (preserved_on_deletion ? ["preserved-on-deletion"] : []) @ ["node", cls],
    Some((id', rev_path)),
    None,
    doc,
  );
};

let rule =
    (prefix: string, rev_path: Path.steps, cls: PP.cls, doc: PP.doc): PP.doc => {
  let id' = id_of_rev_path(prefix, rev_path);
  PP.tagged(["node", cls], Some((id', rev_path)), None, doc);
};

let term_with_attrs =
    (
      ~preserved_on_deletion=false,
      prefix: string,
      err_status: err_status,
      rev_path: Path.steps,
      classes: list(PP.cls),
      attrs,
      doc: PP.doc,
    )
    : PP.doc => {
  let id' = id_of_rev_path(prefix, rev_path);
  PP.tagged(
    (preserved_on_deletion ? ["preserved-on-deletion"] : [])
    @ ["node", ...cls_from_classes(err_status, classes)],
    Some((id', rev_path)),
    Some(attrs),
    doc,
  );
};

let of_var_binding = (prefix: string, rev_path: Path.steps, x: Var.t): PP.doc => {
  let id = id_of_rev_path(prefix, rev_path);
  PP.tagged(
    ["var_binding"],
    Some((id, rev_path)),
    None,
    taggedText(["var"], x),
  );
};

let optionalBreakSmallSp = PP.optionalBreak(" ");
let optionalBreakSp = PP.optionalBreak(" ");
let optionalBreakNSp = PP.optionalBreak("");

/* Parenthesized */
let of_Parenthesized =
    (
      ~preserved_on_deletion=false,
      is_multi_line: bool,
      prefix: string,
      err_status: err_status,
      rev_path: Path.steps,
      r1: PP.doc,
    )
    : PP.doc => {
  let lp = lparen(~classes=[before_child_cls(0)], "(");
  let rp = rparen(~classes=[closing_delimiter_cls], ")");
  let view =
    if (is_multi_line) {
      PP.blockBoundary
      ^^ lp
      ^^ PP.nestAbsolute(2, r1)
      ^^ PP.mandatoryBreak
      ^^ rp;
    } else {
      lp ^^ smallSpace ^^ r1 ^^ smallSpace ^^ rp;
    };
  term(
    ~preserved_on_deletion,
    prefix,
    err_status,
    rev_path,
    ["Parenthesized"],
    view,
  );
};

/* Generic operator printing */
let of_op =
    (
      ~is_space=false,
      ~op_index=?,
      ~prefix=optionalBreakSmallSp,
      ~suffix=optionalBreakSmallSp,
      op_s: string,
      classes: list(PP.cls),
    )
    : PP.doc => {
  let op_index_classes =
    switch (op_index) {
    | None => []
    | Some(k) => [before_child_cls(k)]
    };
  if (is_space) {
    taggedText(["space", ...op_index_classes], " ");
  } else {
    prefix ^^ taggedText(classes @ op_index_classes, op_s) ^^ suffix;
  };
};

/* Types */
let string_of_ty_op = (op: UHTyp.op): string =>
  switch (op) {
  | Arrow => "Arrow"
  | Sum => "Sum"
  | Prod => "Prod"
  };

let of_ty_op = (~op_index=?, op: UHTyp.op): PP.doc => {
  let op_cls = "op-" ++ string_of_ty_op(op);
  switch (op) {
  | Arrow =>
    of_op(
      ~op_index?,
      ~prefix=optionalBreakSp,
      ~suffix=optionalBreakSp,
      LangUtil.typeArrowSym,
      [op_cls],
    )
  | Sum =>
    of_op(
      ~op_index?,
      ~prefix=optionalBreakSp,
      ~suffix=optionalBreakSp,
      "|",
      [op_cls],
    )
  | Prod => of_op(~op_index?, ~suffix=optionalBreakSp, ",", [op_cls])
  };
};

let of_Hole =
    (
      ~preserved_on_deletion=false,
      prefix: string,
      rev_path: Path.steps,
      classes: list(PP.cls),
      hole_name: string,
    )
    : PP.doc =>
  term(
    ~preserved_on_deletion,
    prefix,
    NotInHole,
    rev_path,
    classes,
    taggedText(["hole-before-1"], "​​")
    ^^ taggedText(["hole-before-2"], "​")
    ^^ taggedText(["holeName"], hole_name)
    ^^ taggedText(["hole-after-1"], "​")
    ^^ taggedText(["hole-after-2"], "​"),
  );

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
let of_Bool =
    (~preserved_on_deletion=false, prefix: string, rev_path: Path.steps)
    : PP.doc =>
  term(
    ~preserved_on_deletion,
    prefix,
    NotInHole,
    rev_path,
    ["Bool"],
    kw("Bool"),
  );
let of_Num =
    (~preserved_on_deletion=false, prefix: string, rev_path: Path.steps)
    : PP.doc =>
  term(
    ~preserved_on_deletion,
    prefix,
    NotInHole,
    rev_path,
    ["Num"],
    kw("Num"),
  );
let of_Unit =
    (~preserved_on_deletion=false, prefix: string, rev_path: Path.steps)
    : PP.doc =>
  term(
    ~preserved_on_deletion,
    prefix,
    NotInHole,
    rev_path,
    ["Unit"],
    kw("Unit"),
  );
let of_ty_BinOp =
    (
      prefix: string,
      err_status: err_status,
      rev_path: Path.steps,
      r1: PP.doc,
      op: UHTyp.op,
      r2: PP.doc,
    )
    : PP.doc =>
  term(
    prefix,
    err_status,
    rev_path,
    [string_of_ty_op(op), "skel-binop"],
    r1 ^^ of_ty_op(op) ^^ r2,
  );
let of_uty_BinOp =
    (
      prefix: string,
      err_status: err_status,
      rev_path: Path.steps,
      r1: PP.doc,
      op: UHTyp.op,
      r2: PP.doc,
      op_index: int,
    )
    : PP.doc =>
  term(
    prefix,
    err_status,
    rev_path,
    [string_of_ty_op(op), "skel-binop", before_child_skel_cls(op_index)],
    r1 ^^ of_ty_op(~op_index, op) ^^ r2,
  );
let of_List =
    (
      ~preserved_on_deletion=false,
      prefix: string,
      rev_path: Path.steps,
      r1: PP.doc,
    )
    : PP.doc =>
  term(
    ~preserved_on_deletion,
    prefix,
    NotInHole,
    rev_path,
    ["List"],
    kw(~classes=[before_child_cls(0)], "List")
    ^^ lparen(~classes=[before_child_cls(0)], "(")
    ^^ smallSpace
    ^^ r1
    ^^ smallSpace
    ^^ rparen(~classes=[closing_delimiter_cls], ")"),
  );
let rec of_htype =
        (parenthesize: bool, prefix: string, rev_path: Path.steps, ty: HTyp.t)
        : PP.doc => {
  let d: PP.doc =
    switch (ty) {
    | Bool => of_Bool(prefix, rev_path)
    | Num => of_Num(prefix, rev_path)
    | Unit => of_Unit(prefix, rev_path)
    | List(ty1) =>
      let rev_path1 = [0, ...rev_path];
      let r1 = of_htype(false, prefix, rev_path1, ty1);
      of_List(prefix, rev_path, r1);
    | Arrow(ty1, ty2) =>
      let rev_path1 = [0, ...rev_path];
      let rev_path2 = [1, ...rev_path];
      let paren1 = precedence_ty(ty1) >= precedence_Arrow;
      let paren2 = precedence_ty(ty2) > precedence_Arrow;
      let r1 = of_htype(paren1, prefix, rev_path1, ty1);
      let r2 = of_htype(paren2, prefix, rev_path2, ty2);
      of_ty_BinOp(prefix, NotInHole, rev_path, r1, UHTyp.Arrow, r2);
    | Sum(ty1, ty2) =>
      let rev_path1 = [0, ...rev_path];
      let rev_path2 = [1, ...rev_path];
      let paren1 = precedence_ty(ty1) >= precedence_Sum;
      let paren2 = precedence_ty(ty2) > precedence_Sum;
      let r1 = of_htype(paren1, prefix, rev_path1, ty1);
      let r2 = of_htype(paren2, prefix, rev_path2, ty2);
      of_ty_BinOp(prefix, NotInHole, rev_path, r1, UHTyp.Sum, r2);
    | Prod(ty1, ty2) =>
      let rev_path1 = [0, ...rev_path];
      let rev_path2 = [1, ...rev_path];
      let paren1 = precedence_ty(ty1) >= precedence_Prod;
      let paren2 = precedence_ty(ty2) > precedence_Prod;
      let r1 = of_htype(paren1, prefix, rev_path1, ty1);
      let r2 = of_htype(paren2, prefix, rev_path2, ty2);
      of_ty_BinOp(prefix, NotInHole, rev_path, r1, UHTyp.Prod, r2);
    | Hole => of_Hole(prefix, rev_path, ["Hole"], "?")
    };
  parenthesize ? lparen("(") ^^ d ^^ rparen(")") : d;
};
let rec of_uhtyp =
        (
          ~preserved_on_deletion=false,
          prefix: string,
          rev_path: Path.steps,
          uty: UHTyp.t,
        )
        : PP.doc =>
  switch (uty) {
  | Parenthesized(uty1) =>
    let rev_path1 = [0, ...rev_path];
    let r1 = of_uhtyp(~preserved_on_deletion=true, prefix, rev_path1, uty1);
    of_Parenthesized(
      ~preserved_on_deletion,
      false,
      prefix,
      NotInHole,
      rev_path,
      r1,
    );
  | Bool => of_Bool(~preserved_on_deletion, prefix, rev_path)
  | Num => of_Num(~preserved_on_deletion, prefix, rev_path)
  | Unit => of_Unit(~preserved_on_deletion, prefix, rev_path)
  | List(uty1) =>
    let rev_path1 = [0, ...rev_path];
    let r1 = of_uhtyp(~preserved_on_deletion=true, prefix, rev_path1, uty1);
    of_List(~preserved_on_deletion, prefix, rev_path, r1);
  | OpSeq(skel, seq) =>
    term(
      ~preserved_on_deletion,
      prefix,
      NotInHole,
      rev_path,
      ["OpSeq"],
      of_uhtyp_skel(prefix, rev_path, skel, seq, 0),
    )
  | Hole => of_Hole(~preserved_on_deletion, prefix, rev_path, ["Hole"], "?")
  }
and of_uhtyp_skel =
    (
      prefix: string,
      rev_path: Path.steps,
      skel: UHTyp.skel_t,
      seq: UHTyp.opseq,
      seq_offset: int,
    )
    : PP.doc =>
  switch (skel) {
  | Placeholder(n) =>
    switch (OperatorSeq.nth_tm(n, seq)) {
    | None => raise(InvariantViolated)
    | Some(utyn) =>
      let rev_path_n = [n, ...rev_path];
      of_uhtyp(prefix, rev_path_n, utyn);
    }
  | BinOp(_, op, skel1, skel2) =>
    let num_tms_before_op = Skel.size(skel1);
    let absolute_op_index = seq_offset + num_tms_before_op;
    let r1 = of_uhtyp_skel(prefix, rev_path, skel1, seq, seq_offset);
    let r2 = of_uhtyp_skel(prefix, rev_path, skel2, seq, absolute_op_index);
    of_uty_BinOp(prefix, NotInHole, rev_path, r1, op, r2, absolute_op_index);
  };

/* Expressions and Patterns */

let classes_of_var_err_status =
    (var_err_status: var_err_status): list(string) =>
  switch (var_err_status) {
  | InVHole(Free, u) => ["InVHole", "InVHole_" ++ string_of_int(u)]
  | InVHole(Keyword(_), u) => [
      "InVHole",
      "InVHole_" ++ string_of_int(u),
      "Keyword",
    ]
  | NotInVHole => []
  };

let of_Wild =
    (
      ~preserved_on_deletion=false,
      prefix: string,
      err_status: err_status,
      rev_path: Path.steps,
    )
    : PP.doc =>
  term(
    ~preserved_on_deletion,
    prefix,
    err_status,
    rev_path,
    ["Wild"],
    var("_"),
  );

let of_Var =
    (
      ~preserved_on_deletion=false,
      prefix: string,
      err_status: err_status,
      var_err_status: var_err_status,
      rev_path: Path.steps,
      x: Var.t,
    )
    : PP.doc =>
  term(
    ~preserved_on_deletion,
    prefix,
    err_status,
    rev_path,
    ["Var", ...classes_of_var_err_status(var_err_status)],
    var(x),
  );

let of_EmptyLine = (prefix: string, rev_path: Path.steps): PP.doc => {
  let id' = id_of_rev_path(prefix, rev_path);
  PP.tagged(
    ["EmptyLine"],
    Some((id', rev_path)),
    None,
    taggedText([], ""),
  );
};

let of_ExpLine = (r1: PP.doc): PP.doc =>
  PP.tagged(
    ["ExpLine"],
    None, /* ExpLine is not a cursor-selectable node */
    None,
    r1,
  );

let of_LetLine =
    (
      prefix: string,
      rev_path: Path.steps,
      rx: PP.doc,
      rann: option(PP.doc),
      r1: PP.doc,
    )
    : PP.doc => {
  let first_part =
    PP.blockBoundary
    ^^ kw(~classes=[before_child_cls(0)], "let")
    ^^ space
    ^^ rx;
  let second_part =
    of_op(
      ~prefix=optionalBreakSp,
      ~suffix=optionalBreakSp,
      "=",
      ["let-equals", before_child_cls(2)],
    )
    ^^ PP.nestAbsolute(2, r1);
  let view =
    switch (rann) {
    | Some(r) =>
      first_part
      ^^ of_op(
           ~prefix=optionalBreakSp,
           ~suffix=optionalBreakSp,
           ":",
           ["ann", before_child_cls(1)],
         )
      ^^ r
      ^^ second_part
    | None => first_part ^^ second_part
    };
  let id' = id_of_rev_path(prefix, rev_path);
  PP.tagged(["LetLine"], Some((id', rev_path)), None, view);
};

let of_lines = (rline_lst: list(PP.doc)): PP.doc =>
  switch (rline_lst) {
  | [] => PP.empty
  | [rli, ...rli_lst] =>
    List.fold_left(
      (rlines, rline) => rlines ^^ PP.mandatoryBreak ^^ rline,
      rli,
      rli_lst,
    )
  };

let of_Block =
    (
      ~preserved_on_deletion=false,
      prefix: string,
      rev_path: Path.steps,
      rlines: PP.doc,
      r1: PP.doc,
    )
    : PP.doc => {
  let r =
    if (PP.isEmpty(rlines)) {
      r1;
    } else {
      rlines ^^ PP.mandatoryBreak ^^ r1;
    };
  block(~preserved_on_deletion, prefix, rev_path, "Block", r);
};

let of_Let =
    (
      prefix: string,
      err_status: err_status,
      rev_path: Path.steps,
      rx: PP.doc,
      rann: option(PP.doc),
      r1: PP.doc,
      r2: PP.doc,
    )
    : PP.doc => {
  let first_part =
    PP.blockBoundary
    ^^ kw(~classes=[before_child_cls(0)], "let")
    ^^ space
    ^^ rx;
  let second_part =
    of_op(
      ~prefix=optionalBreakSp,
      ~suffix=optionalBreakSp,
      "=",
      ["let-equals", before_child_cls(2)],
    )
    ^^ PP.nestAbsolute(2, r1)
    ^^ PP.mandatoryBreak
    ^^ r2;
  let view =
    switch (rann) {
    | Some(r) =>
      first_part
      ^^ of_op(
           ~prefix=optionalBreakSp,
           ~suffix=optionalBreakSp,
           ":",
           ["ann", before_child_cls(1)],
         )
      ^^ r
      ^^ second_part
    | None => first_part ^^ second_part
    };
  term(prefix, err_status, rev_path, ["Let"], view);
};

let of_FixF =
    (
      prefix: string,
      err_status: err_status,
      rev_path: Path.steps,
      rx: PP.doc,
      rty: PP.doc,
      r1: PP.doc,
    )
    : PP.doc => {
  let view =
    kw("fix")
    ^^ space
    ^^ rx
    ^^ of_op(":", ["ann"])
    ^^ rty
    ^^ of_op(".", ["lambda-dot"])
    ^^ r1;
  term(prefix, err_status, rev_path, ["Lam"], view);
};

let of_Lam =
    (
      ~preserved_on_deletion=false,
      prefix: string,
      err_status: err_status,
      rev_path: Path.steps,
      rx: PP.doc,
      rann: option(PP.doc),
      r1: PP.doc,
    )
    : PP.doc => {
  let first_part =
    taggedText(["lambda-sym", before_child_cls(0)], LangUtil.lamSym)
    ^^ smallSpace
    ^^ rx;
  let second_part =
    smallSpace
    ^^ taggedText(["lambda-dot", before_child_cls(2)], ".")
    ^^ smallSpace
    ^^ r1;
  let view =
    switch (rann) {
    | Some(r) =>
      first_part
      ^^ of_op(":", ["ann", before_child_cls(1)])
      ^^ r
      ^^ second_part
    | None => first_part ^^ second_part
    };
  term(~preserved_on_deletion, prefix, err_status, rev_path, ["Lam"], view);
};

let string_of_bool = b => b ? "true" : "false";

let of_BoolLit =
    (
      ~preserved_on_deletion=false,
      prefix: string,
      err_status: err_status,
      rev_path: Path.steps,
      b: bool,
    )
    : PP.doc =>
  term(
    ~preserved_on_deletion,
    prefix,
    err_status,
    rev_path,
    ["BoolLit"],
    taggedText(["boolean"], string_of_bool(b)),
  );
let of_NumLit =
    (
      ~preserved_on_deletion=false,
      prefix: string,
      err_status: err_status,
      rev_path: Path.steps,
      n: int,
    )
    : PP.doc =>
  term(
    ~preserved_on_deletion,
    prefix,
    err_status,
    rev_path,
    ["NumLit"],
    taggedText(["number"], string_of_int(n)),
  );
let of_Triv =
    (
      ~preserved_on_deletion=false,
      prefix: string,
      err_status: err_status,
      rev_path: Path.steps,
    )
    : PP.doc =>
  term(
    ~preserved_on_deletion,
    prefix,
    err_status,
    rev_path,
    ["Triv"],
    taggedText(["triv"], "()"),
  );

let of_ListNil =
    (
      ~preserved_on_deletion=false,
      prefix: string,
      err_status: err_status,
      rev_path: Path.steps,
    )
    : PP.doc =>
  term(
    ~preserved_on_deletion,
    prefix,
    err_status,
    rev_path,
    ["ListNil"],
    kw("[]"),
  );

let of_Inj =
    (
      ~preserved_on_deletion=false,
      prefix: string,
      err_status: err_status,
      rev_path: Path.steps,
      side: inj_side,
      r: PP.doc,
    )
    : PP.doc =>
  term(
    ~preserved_on_deletion,
    prefix,
    err_status,
    rev_path,
    ["Inj"],
    kw(~classes=[before_child_cls(0)], "inj")
    ^^ lparen(~classes=[before_child_cls(0)], "[")
    ^^ kw(~classes=[before_child_cls(0)], LangUtil.string_of_side(side))
    ^^ rparen(~classes=[before_child_cls(0)], "]")
    ^^ lparen(~classes=[before_child_cls(0)], "(")
    ^^ smallSpace
    ^^ r
    ^^ smallSpace
    ^^ rparen(~classes=[closing_delimiter_cls], ")"),
  );

let of_InjAnn =
    (
      prefix: string,
      err_status: err_status,
      rev_path: Path.steps,
      rty: PP.doc,
      side: inj_side,
      r: PP.doc,
    )
    : PP.doc =>
  term(
    prefix,
    err_status,
    rev_path,
    ["Inj"],
    kw("inj")
    ^^ lparen("[")
    ^^ kw(LangUtil.string_of_side(side))
    ^^ kw(",")
    ^^ optionalBreakSp
    ^^ rty
    ^^ rparen("]")
    ^^ lparen("(")
    ^^ PP.optionalBreak("")
    ^^ r
    ^^ rparen(")"),
  );

let of_rule =
    (prefix: string, rev_path: Path.steps, rp: PP.doc, rc: PP.doc): PP.doc => {
  let view =
    of_op(
      ~prefix=PP.empty,
      ~suffix=optionalBreakSp,
      "|",
      ["rule-bar", before_child_cls(0)],
    )
    ^^ rp
    ^^ of_op(
         ~prefix=optionalBreakSp,
         ~suffix=optionalBreakSp,
         LangUtil.caseArrowSym,
         ["rule-arrow", before_child_cls(1)],
       )
    ^^ PP.nestAbsolute(2, rc);
  rule(prefix, rev_path, "Case-rule", view);
};

let of_Case =
    (
      ~preserved_on_deletion=false,
      prefix: string,
      err_status: err_status,
      rev_path: Path.steps,
      r1: PP.doc,
      rpcs: list((PP.doc, PP.doc)),
      rann: option(PP.doc),
    )
    : PP.doc => {
  let num_rules = List.length(rpcs);
  let rrules =
    fold_left_i(
      (rrs: PP.doc, (i: int, (rp: PP.doc, rc: PP.doc))) => {
        let line_break = rrs == PP.empty ? PP.empty : PP.mandatoryBreak;
        let rr = of_rule(prefix, [i + 1, ...rev_path], rp, rc);
        rrs ^^ line_break ^^ rr;
      },
      PP.empty,
      rpcs,
    );
  let view_end =
    switch (rann) {
    | None => kw(~classes=[closing_delimiter_cls], "end")
    | Some(r) =>
      kw(~classes=[before_child_cls(num_rules + 1)], "end")
      ^^ of_op(" : ", ["ann", before_child_cls(num_rules + 1)])
      ^^ r
    };
  let view =
    PP.blockBoundary
    ^^ kw(~classes=[before_child_cls(0)], "case")
    ^^ space
    ^^ r1
    ^^ PP.mandatoryBreak
    ^^ rrules
    ^^ PP.mandatoryBreak
    ^^ view_end;
  term(~preserved_on_deletion, prefix, err_status, rev_path, ["Case"], view);
};

let cast_arrow = op(" ⇨ ");
let of_Cast =
    (
      prefix: string,
      err_status: err_status,
      rev_path: Path.steps,
      r1: PP.doc,
      rty1: PP.doc,
      rty2: PP.doc,
    )
    : PP.doc =>
  term(
    prefix,
    err_status,
    rev_path,
    ["Cast"],
    _SHOW_CASTS
      ? r1 ^^ lparen("⟨") ^^ rty1 ^^ cast_arrow ^^ rty2 ^^ rparen("⟩")
      : r1,
  );

let of_chained_Cast =
    (
      prefix: string,
      err_status: err_status,
      rev_path: Path.steps,
      r1: PP.doc,
      rty1: PP.doc,
      rty2: PP.doc,
      rty4: PP.doc,
    )
    : PP.doc =>
  term(
    prefix,
    err_status,
    rev_path,
    ["Cast"],
    _SHOW_CASTS
      ? r1
        ^^ lparen("<")
        ^^ rty1
        ^^ cast_arrow
        ^^ rty2
        ^^ cast_arrow
        ^^ rty4
        ^^ rparen(">")
      : r1,
  );

let failed_cast_arrow = taggedText(["failed-cast-arrow"], " ⇨ ");
let of_FailedCast =
    (
      prefix: string,
      err_status: err_status,
      rev_path: Path.steps,
      r1: PP.doc,
      rty1: PP.doc,
      rty2: PP.doc,
    )
    : PP.doc =>
  term(
    prefix,
    err_status,
    rev_path,
    ["FailedCast"],
    r1 ^^ lparen("<") ^^ rty1 ^^ failed_cast_arrow ^^ rty2 ^^ rparen(">"),
  );

let of_chained_FailedCast =
    (
      prefix: string,
      err_status: err_status,
      rev_path: Path.steps,
      r1: PP.doc,
      rty1: PP.doc,
      rty2: PP.doc,
      rty4: PP.doc,
    )
    : PP.doc =>
  term(
    prefix,
    err_status,
    rev_path,
    ["FailedCast"],
    r1
    ^^ lparen("<")
    ^^ rty1
    ^^ failed_cast_arrow
    ^^ rty2
    ^^ failed_cast_arrow
    ^^ rty4
    ^^ rparen(">"),
  );

let is_multi_line = (block: UHExp.block): bool =>
  switch (block) {
  | Block([], Case(_, _, _, _)) => true
  | Block([], _) => false
  | Block([_, ..._], _) => true
  };

type palette_stuff = {
  palette_view_ctx: Palettes.PaletteViewCtx.t,
  mk_editor_box: EditorBoxTypes.mk_editor_box,
  do_action: Action.t => unit,
};

let string_of_pat_op = (op: UHPat.op): string =>
  switch (op) {
  | Comma => "Comma"
  | Space => "Space"
  | Cons => "Cons"
  };

let of_pat_op = (op_index: int, op: UHPat.op): PP.doc => {
  let op_cls = "op-" ++ string_of_pat_op(op);
  switch (op) {
  | Comma =>
    of_op(
      ~suffix=optionalBreakSp,
      ",",
      [op_cls, before_child_cls(op_index)],
    )
  | Space => of_op(" ", [op_cls, before_child_cls(op_index)])
  | Cons => of_op("::", [op_cls, before_child_cls(op_index)])
  };
};

let of_pat_BinOp =
    (
      prefix: string,
      err_status: err_status,
      rev_path: Path.steps,
      r1: PP.doc,
      op: UHPat.op,
      r2: PP.doc,
      op_index: int,
    )
    : PP.doc =>
  term(
    prefix,
    err_status,
    rev_path,
    [string_of_pat_op(op), "skel-binop", before_child_skel_cls(op_index)],
    r1 ^^ of_pat_op(op_index, op) ^^ r2,
  );

let rec prepare_Parenthesized_pat = (p: UHPat.t): (err_status, UHPat.t) =>
  switch (p) {
  | Parenthesized(p1) =>
    let (err_status, p1_not_in_hole) = prepare_Parenthesized_pat(p1);
    (err_status, Parenthesized(p1_not_in_hole));
  | Wild(NotInHole as err_status)
  | Var(NotInHole as err_status, _, _)
  | NumLit(NotInHole as err_status, _)
  | BoolLit(NotInHole as err_status, _)
  | ListNil(NotInHole as err_status) => (err_status, p)
  | Wild(err_status)
  | Var(err_status, _, _)
  | NumLit(err_status, _)
  | BoolLit(err_status, _)
  | ListNil(err_status)
  | Inj(err_status, _, _) => (
      err_status,
      UHPat.set_err_status_t(NotInHole, p),
    )
  | EmptyHole(_) => (NotInHole, p)
  | OpSeq(BinOp(NotInHole as err_status, _, _, _), _) => (err_status, p)
  | OpSeq(BinOp(err_status, _, _, _), _) => (
      err_status,
      UHPat.set_err_status_t(NotInHole, p),
    )
  | OpSeq(Placeholder(n) as skel, seq) =>
    switch (OperatorSeq.nth_tm(n, seq)) {
    | None => raise(UHPat.SkelInconsistentWithOpSeq(skel, seq))
    | Some(pn) =>
      let (err_status, pn_nih) = prepare_Parenthesized_pat(pn);
      switch (OperatorSeq.seq_update_nth(n, seq, pn_nih)) {
      | None => raise(UHPat.SkelInconsistentWithOpSeq(skel, seq))
      | Some(seq) => (err_status, OpSeq(skel, seq))
      };
    }
  };

let rec of_hpat =
        (
          ~preserved_on_deletion=false,
          prefix: string,
          rev_path: Path.steps,
          p: UHPat.t,
        )
        : PP.doc =>
  switch (p) {
  | EmptyHole(u) =>
    of_Hole(
      ~preserved_on_deletion,
      prefix,
      rev_path,
      ["EmptyHole"],
      string_of_int(u + 1),
    )
  | OpSeq(skel, seq) =>
    term(
      ~preserved_on_deletion,
      prefix,
      UHPat.get_err_status_t(p),
      rev_path,
      ["OpSeq"],
      of_skel_pat(prefix, rev_path, skel, seq, 0),
    )
  | Parenthesized(p1) =>
    let (err_status, p1_not_in_hole) = prepare_Parenthesized_pat(p1);
    let rev_path1 = [0, ...rev_path];
    let r1 =
      of_hpat(~preserved_on_deletion=true, prefix, rev_path1, p1_not_in_hole);
    of_Parenthesized(
      ~preserved_on_deletion,
      false,
      prefix,
      err_status,
      rev_path,
      r1,
    );
  | Wild(err_status) =>
    of_Wild(~preserved_on_deletion, prefix, err_status, rev_path)
  | Var(_, InVHole(Free, _), _) => raise(FreeVarInPat)
  | Var(err_status, InVHole(Keyword(k), u), x) =>
    of_Var(
      ~preserved_on_deletion,
      prefix,
      err_status,
      InVHole(Keyword(k), u),
      rev_path,
      x,
    )
  | Var(err_status, NotInVHole, x) =>
    of_Var(
      ~preserved_on_deletion,
      prefix,
      err_status,
      NotInVHole,
      rev_path,
      x,
    )
  | NumLit(err_status, n) =>
    of_NumLit(~preserved_on_deletion, prefix, err_status, rev_path, n)
  | BoolLit(err_status, b) =>
    of_BoolLit(~preserved_on_deletion, prefix, err_status, rev_path, b)
  | Inj(err_status, side, p1) =>
    let rev_path1 = [0, ...rev_path];
    let r1 = of_hpat(~preserved_on_deletion=true, prefix, rev_path1, p1);
    of_Inj(~preserved_on_deletion, prefix, err_status, rev_path, side, r1);
  | ListNil(err_status) =>
    of_ListNil(~preserved_on_deletion, prefix, err_status, rev_path)
  }
and of_skel_pat =
    (
      prefix: string,
      rev_path: Path.steps,
      skel: UHPat.skel_t,
      seq: UHPat.opseq,
      seq_offset: int,
    )
    : PP.doc =>
  switch (skel) {
  | Placeholder(n) =>
    switch (OperatorSeq.nth_tm(n, seq)) {
    | None => raise(InvariantViolated)
    | Some(pn) =>
      let rev_path_n = [n, ...rev_path];
      of_hpat(prefix, rev_path_n, pn);
    }
  | BinOp(err_status, op, skel1, skel2) =>
    let num_tms_before_op = Skel.size(skel1);
    let absolute_op_index = seq_offset + num_tms_before_op;
    let r1 = of_skel_pat(prefix, rev_path, skel1, seq, seq_offset);
    let r2 = of_skel_pat(prefix, rev_path, skel2, seq, absolute_op_index);
    of_pat_BinOp(prefix, err_status, rev_path, r1, op, r2, absolute_op_index);
  };

let string_of_exp_op = (op: UHExp.op): string =>
  switch (op) {
  | Plus => "Plus"
  | Times => "Times"
  | LessThan => "LessThan"
  | Space => "Space"
  | Comma => "Comma"
  | Cons => "Cons"
  };

let of_exp_op = (~op_index=?, op: UHExp.op): PP.doc => {
  let op_cls = "op-" ++ string_of_exp_op(op);
  switch (op) {
  | Plus => of_op(~op_index?, "+", [op_cls])
  | Times => of_op(~op_index?, "*", [op_cls])
  | LessThan => of_op(~op_index?, "<", [op_cls])
  | Space => of_op(~is_space=true, ~op_index?, "", [op_cls])
  | Comma => of_op(~suffix=optionalBreakSp, ~op_index?, ",", [op_cls])
  | Cons => of_op(~op_index?, "::", [op_cls])
  };
};

let of_exp_BinOp =
    (
      prefix: string,
      err_status: err_status,
      rev_path: Path.steps,
      r1: PP.doc,
      op: UHExp.op,
      r2: PP.doc,
    )
    : PP.doc =>
  term(
    prefix,
    err_status,
    rev_path,
    [string_of_exp_op(op), "skel-binop"],
    r1 ^^ of_exp_op(op) ^^ r2,
  );

let of_uhexp_BinOp =
    (
      prefix: string,
      err_status: err_status,
      rev_path: Path.steps,
      r1: PP.doc,
      op: UHExp.op,
      r2: PP.doc,
      op_index: int,
    )
    : PP.doc =>
  term(
    ~preserved_on_deletion=true,
    prefix,
    err_status,
    rev_path,
    [string_of_exp_op(op), "skel-binop", before_child_skel_cls(op_index)],
    r1 ^^ of_exp_op(~op_index, op) ^^ r2,
  );

/* special cased below */
let of_Times_with_space =
    (
      prefix: string,
      err_status: err_status,
      rev_path: Path.steps,
      r1: PP.doc,
      op: UHExp.op,
      r2: PP.doc,
      op_index: int,
    )
    : PP.doc =>
  term(
    ~preserved_on_deletion=true,
    prefix,
    err_status,
    rev_path,
    [string_of_exp_op(op), "skel-binop", before_child_skel_cls(op_index)],
    r1 ^^ of_op(~op_index, "*", ["op-Times"]) ^^ r2,
  );

let prepare_Parenthesized_block =
    (block: UHExp.block): (err_status, UHExp.block) => (
  UHExp.get_err_status_block(block),
  UHExp.set_err_status_block(NotInHole, block),
);

let rec of_hblock =
        (
          ~preserved_on_deletion=false,
          palette_stuff: palette_stuff,
          prefix: string,
          rev_path: Path.steps,
          Block(lines, e): UHExp.block,
        ) => {
  let rlines = of_hlines(palette_stuff, prefix, rev_path, lines);
  let r =
    of_hexp(palette_stuff, prefix, [List.length(lines), ...rev_path], e);
  of_Block(~preserved_on_deletion, prefix, rev_path, rlines, r);
}
and of_hlines =
    (
      palette_stuff: palette_stuff,
      prefix: string,
      rev_path: Path.steps,
      lines: UHExp.lines,
    ) => {
  let rline_lst =
    List.mapi(
      (i, line) => of_hline(palette_stuff, prefix, [i, ...rev_path], line),
      lines,
    );
  of_lines(rline_lst);
}
and of_hline =
    (
      palette_stuff: palette_stuff,
      prefix: string,
      rev_path: Path.steps,
      line: UHExp.line,
    ) =>
  switch (line) {
  | EmptyLine => of_EmptyLine(prefix, rev_path)
  | LetLine(p, ann, e1) =>
    let rp = of_hpat(prefix, [0, ...rev_path], p);
    let rann =
      switch (ann) {
      | Some(uty1) => Some(of_uhtyp(prefix, [1, ...rev_path], uty1))
      | None => None
      };
    let r1 = of_hblock(palette_stuff, prefix, [2, ...rev_path], e1);
    of_LetLine(prefix, rev_path, rp, rann, r1);
  | ExpLine(e1) =>
    let r1 = of_hexp(palette_stuff, prefix, rev_path, e1);
    of_ExpLine(r1);
  }
and of_hexp =
    (
      ~preserved_on_deletion=false,
      palette_stuff: palette_stuff,
      prefix: string,
      rev_path: Path.steps,
      e: UHExp.t,
    ) =>
  switch (e) {
  | EmptyHole(u) =>
    of_Hole(
      ~preserved_on_deletion,
      prefix,
      rev_path,
      ["EmptyHole"],
      string_of_int(u + 1),
    )
  | OpSeq(skel, seq) =>
    let r = of_skel(palette_stuff, prefix, rev_path, skel, seq, 0);
    term(
      ~preserved_on_deletion,
      prefix,
      UHExp.get_err_status_t(e),
      rev_path,
      ["OpSeq"],
      r,
    );
  | Parenthesized(block) =>
    let (err_status, block_not_in_hole) = prepare_Parenthesized_block(block);
    let rev_path_block = [0, ...rev_path];
    let r_block =
      of_hblock(
        ~preserved_on_deletion=true,
        palette_stuff,
        prefix,
        rev_path_block,
        block_not_in_hole,
      );
    of_Parenthesized(
      ~preserved_on_deletion,
      is_multi_line(block),
      prefix,
      err_status,
      rev_path,
      r_block,
    );
  | Var(err_status, var_err_status, x) =>
    of_Var(
      ~preserved_on_deletion,
      prefix,
      err_status,
      var_err_status,
      rev_path,
      x,
    )
  | Lam(err_status, p, ann, e1) =>
    let rp = of_hpat(prefix, [0, ...rev_path], p);
    let rann =
      switch (ann) {
      | Some(uty1) => Some(of_uhtyp(prefix, [1, ...rev_path], uty1))
      | None => None
      };
    let r1 = of_hblock(palette_stuff, prefix, [2, ...rev_path], e1);
    of_Lam(
      ~preserved_on_deletion,
      prefix,
      err_status,
      rev_path,
      rp,
      rann,
      r1,
    );
  | BoolLit(err_status, b) =>
    of_BoolLit(~preserved_on_deletion, prefix, err_status, rev_path, b)
  | NumLit(err_status, n) =>
    of_NumLit(~preserved_on_deletion, prefix, err_status, rev_path, n)
  | ListNil(err_status) =>
    of_ListNil(~preserved_on_deletion, prefix, err_status, rev_path)
  | Inj(err_status, side, e) =>
    let rev_path1 = [0, ...rev_path];
    let r1 =
      of_hblock(
        ~preserved_on_deletion=true,
        palette_stuff,
        prefix,
        rev_path1,
        e,
      );
    of_Inj(~preserved_on_deletion, prefix, err_status, rev_path, side, r1);
  | Case(err_status, e1, rules, ann) =>
    let rev_path1 = [0, ...rev_path];
    let r1 = of_hblock(palette_stuff, prefix, rev_path1, e1);
    let rpcs =
      List.mapi(
        (i, Rule(p, c): UHExp.rule) => {
          let rev_pathr = [i + 1, ...rev_path];
          let rev_pathp = [0, ...rev_pathr];
          let rev_pathc = [1, ...rev_pathr];
          let rp = of_hpat(prefix, rev_pathp, p);
          let rc = of_hblock(palette_stuff, prefix, rev_pathc, c);
          (rp, rc);
        },
        rules,
      );
    let rann =
      switch (ann) {
      | None => None
      | Some(uty1) =>
        let rev_path_ann = [List.length(rules) + 1, ...rev_path];
        Some(of_uhtyp(prefix, rev_path_ann, uty1));
      };
    of_Case(
      ~preserved_on_deletion,
      prefix,
      err_status,
      rev_path,
      r1,
      rpcs,
      rann,
    );
  | ApPalette(_, _, _, _) => raise(InvariantViolated)
  /* switch (
       Palettes.PaletteViewCtx.lookup(palette_stuff.palette_view_ctx, name)
     ) {
     | Some(serialized_view_fn) =>
       let updater = serialized_model => {
         palette_stuff.do_action(
           Action.MoveTo((List.rev(rev_path), Before)),
         );
         palette_stuff.do_action(
           Action.UpdateApPalette(UHExp.HoleRefs.Ret(serialized_model)),
         );
       };
       let view = serialized_view_fn(serialized_model, updater);
       let paletteName =
         term(prefix, err_status, rev_path, ["ApPalette"], paletteName(name));
       let paletteDelim =
         term(prefix, err_status, rev_path, ["ApPalette"], dollar);
       let palettePrefix =
         switch (view) {
         | Inline(_) => paletteName
         | MultiLine(_) => paletteName ^^ PP.mandatoryBreak
         };
       let paletteSuffix =
         switch (view) {
         | Inline(_) => paletteDelim
         | MultiLine(_) => paletteDelim ^^ PP.mandatoryBreak
         };
       palettePrefix
       ^^ PP.paletteView(
            rev_path,
            view,
            hole_map,
            palette_stuff.mk_editor_box,
          )
       ^^ paletteSuffix;
     | None => raise(InvariantViolated)
     } */
  }
and of_skel =
    (
      palette_stuff,
      prefix,
      rev_path,
      skel: UHExp.skel_t,
      seq: UHExp.opseq,
      seq_offset: int,
    )
    : PP.doc =>
  switch (skel) {
  | Placeholder(n) =>
    switch (OperatorSeq.nth_tm(n, seq)) {
    | None => raise(UHExp.SkelInconsistentWithOpSeq(skel, seq))
    | Some(en) =>
      let rev_path_n = [n, ...rev_path];
      of_hexp(
        ~preserved_on_deletion=true,
        palette_stuff,
        prefix,
        rev_path_n,
        en,
      );
    }
  | BinOp(err_status, Times as op, skel1, skel2) =>
    let num_tms_before_op = Skel.size(skel1);
    let absolute_op_index = seq_offset + num_tms_before_op;
    let r1 = of_skel(palette_stuff, prefix, rev_path, skel1, seq, seq_offset);
    let r2 =
      of_skel(palette_stuff, prefix, rev_path, skel2, seq, absolute_op_index);
    switch (Skel.rightmost_op(skel1)) {
    | Some(Space) =>
      of_Times_with_space(
        prefix,
        err_status,
        rev_path,
        r1,
        op,
        r2,
        absolute_op_index,
      )
    | _ =>
      switch (Skel.leftmost_op(skel2)) {
      | Some(Space) =>
        of_Times_with_space(
          prefix,
          err_status,
          rev_path,
          r1,
          op,
          r2,
          absolute_op_index,
        )
      | _ =>
        of_uhexp_BinOp(
          prefix,
          err_status,
          rev_path,
          r1,
          op,
          r2,
          absolute_op_index,
        )
      }
    };
  | BinOp(err_status, op, skel1, skel2) =>
    let num_tms_before_op = Skel.size(skel1);
    let absolute_op_index = seq_offset + num_tms_before_op;
    let r1 = of_skel(palette_stuff, prefix, rev_path, skel1, seq, seq_offset);
    let r2 =
      of_skel(palette_stuff, prefix, rev_path, skel2, seq, absolute_op_index);
    of_uhexp_BinOp(
      prefix,
      err_status,
      rev_path,
      r1,
      op,
      r2,
      absolute_op_index,
    );
  };

open Dynamics;
let precedence_Ap = 1;
let precedence_Times = 2;
let precedence_Plus = 3;
let precedence_Cons = 4;
let precedence_LessThan = 5;
let precedence_Comma = 6;
let precedence_max = 7;
let precedence_dhpat = dp =>
  DHPat.(
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
    }
  );
let rec precedence_dhexp = d =>
  DHExp.(
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
    }
  );

let hole_label_s = ((u, i)) =>
  string_of_int(u + 1) ++ ":" ++ string_of_int(i + 1);
let hole_label_of = inst => taggedText(["holeName"], hole_label_s(inst));
let cls_of_inst = ((u, i)) =>
  "hole-instance-" ++ string_of_int(u) ++ "-" ++ string_of_int(i);
let dbg_SHOW_SIGMAS = false;

let rec of_dhpat' =
        (instance_click_fn, parenthesize, prefix, err_status, rev_path, dp) => {
  let doc =
    DHPat.(
      switch (dp) {
      | EmptyHole(u, i) =>
        let inst = (u, i);
        let hole_label = hole_label_of(inst);
        let r = hole_label;
        let attrs = [
          Tyxml_js.Html5.a_onclick(_ => {
            instance_click_fn(inst);
            true;
          }),
        ];

        let inst_cls = cls_of_inst(inst);
        term_with_attrs(
          prefix,
          err_status,
          rev_path,
          ["EmptyHole", "hole-instance", "selected-instance", inst_cls],
          attrs,
          r,
        );
      | NonEmptyHole(reason, u, _i, dp1) =>
        let rev_path1 = [0, ...rev_path];
        let r =
          of_dhpat'(
            instance_click_fn,
            false,
            prefix,
            InHole(reason, u),
            rev_path1,
            dp1,
          );
        term(prefix, err_status, rev_path, ["NonEmptyHole"], r);
      | Wild => of_Wild(prefix, err_status, rev_path)
      | Keyword(u, _, k) =>
        of_Var(
          prefix,
          err_status,
          InVHole(Keyword(k), u),
          rev_path,
          Var.of_keyword(k),
        )
      | Var(x) => of_Var(prefix, err_status, NotInVHole, rev_path, x)
      | BoolLit(b) => of_BoolLit(prefix, err_status, rev_path, b)
      | NumLit(n) => of_NumLit(prefix, err_status, rev_path, n)
      | Triv => of_Triv(prefix, err_status, rev_path)
      | Inj(side, dp1) =>
        /* TODO: pattern inj doesn't need a type, does this cause issues with rev_path? */
        let rev_path1 = [0, ...rev_path];
        let r1 =
          of_dhpat'(
            instance_click_fn,
            false,
            prefix,
            NotInHole,
            rev_path1,
            dp1,
          );
        of_Inj(prefix, err_status, rev_path, side, r1);
      | ListNil => of_ListNil(prefix, err_status, rev_path)
      | Cons(dp1, dp2) =>
        let rev_path1 = [0, ...rev_path];
        let rev_path2 = [1, ...rev_path];
        let paren1 = precedence_dhpat(dp1) > precedence_Cons;
        let paren2 = precedence_dhpat(dp2) >= precedence_Cons;
        let r1 =
          of_dhpat'(
            instance_click_fn,
            paren1,
            prefix,
            NotInHole,
            rev_path1,
            dp1,
          );
        let r2 =
          of_dhpat'(
            instance_click_fn,
            paren2,
            prefix,
            NotInHole,
            rev_path2,
            dp2,
          );
        of_exp_BinOp(prefix, err_status, rev_path, r1, UHExp.Cons, r2);
      | Pair(dp1, dp2) =>
        let rev_path1 = [0, ...rev_path];
        let rev_path2 = [1, ...rev_path];
        let r1 =
          of_dhpat'(
            instance_click_fn,
            false,
            prefix,
            NotInHole,
            rev_path1,
            dp1,
          );
        let r2 =
          of_dhpat'(
            instance_click_fn,
            false,
            prefix,
            NotInHole,
            rev_path2,
            dp2,
          );
        of_exp_BinOp(prefix, err_status, rev_path, r1, UHExp.Comma, r2);
      | Ap(dp1, dp2) =>
        let rev_path1 = [0, ...rev_path];
        let rev_path2 = [1, ...rev_path];
        let paren1 = precedence_dhpat(dp1) > precedence_Ap;
        let paren2 = precedence_dhpat(dp2) >= precedence_Ap;
        let r1 =
          of_dhpat'(
            instance_click_fn,
            paren1,
            prefix,
            NotInHole,
            rev_path1,
            dp1,
          );
        let r2 =
          of_dhpat'(
            instance_click_fn,
            paren2,
            prefix,
            NotInHole,
            rev_path2,
            dp2,
          );
        of_exp_BinOp(prefix, err_status, rev_path, r1, UHExp.Space, r2);
      }
    );
  parenthesize ? lparen("(") ^^ doc ^^ rparen(")") : doc;
};

let of_dhpat = (instance_click_fn, prefix, rev_path, dp) =>
  of_dhpat'(instance_click_fn, false, prefix, NotInHole, rev_path, dp);

let rec of_dhexp' =
        (instance_click_fn, parenthesize, prefix, err_status, rev_path, d) => {
  let doc =
    DHExp.(
      switch (d) {
      | BoundVar(x) => of_Var(prefix, err_status, NotInVHole, rev_path, x)
      | FreeVar(u, _, _, x) =>
        of_Var(prefix, err_status, InVHole(Free, u), rev_path, x)
      | Keyword(u, _, _, k) =>
        of_Var(
          prefix,
          err_status,
          InVHole(Keyword(k), u),
          rev_path,
          Var.of_keyword(k),
        )
      | Let(dp, d1, d2) =>
        let rev_pathp = [0, ...rev_path];
        let rev_path1 = [1, ...rev_path];
        let rev_path2 = [2, ...rev_path];
        let rp = of_dhpat(instance_click_fn, prefix, rev_pathp, dp);
        let r1 =
          of_dhexp'(
            instance_click_fn,
            false,
            prefix,
            NotInHole,
            rev_path1,
            d1,
          );
        let r2 =
          of_dhexp'(
            instance_click_fn,
            false,
            prefix,
            NotInHole,
            rev_path2,
            d2,
          );
        of_Let(prefix, err_status, rev_path, rp, None, r1, r2);
      | FixF(x, ty, d1) =>
        if (_SHOW_FN_BODIES) {
          let rx = of_var_binding(prefix, [0, ...rev_path], x);
          let rty = of_htype(false, prefix, [1, ...rev_path], ty);
          let r1 =
            of_dhexp'(
              instance_click_fn,
              false,
              prefix,
              NotInHole,
              [2, ...rev_path],
              d1,
            );
          of_FixF(prefix, err_status, rev_path, rx, rty, r1);
        } else {
          taggedText(["fn-placeholder"], "<fn>");
        }
      | Lam(dp, ann, d1) =>
        if (_SHOW_FN_BODIES) {
          let rp = of_dhpat(instance_click_fn, prefix, [0, ...rev_path], dp);
          let rann = Some(of_htype(false, prefix, [1, ...rev_path], ann));
          let r1 =
            of_dhexp'(
              instance_click_fn,
              false,
              prefix,
              NotInHole,
              [2, ...rev_path],
              d1,
            );
          of_Lam(prefix, err_status, rev_path, rp, rann, r1);
        } else {
          taggedText(["fn-placeholder"], "<fn>");
        }
      | Ap(d1, d2) =>
        let rev_path1 = [0, ...rev_path];
        let rev_path2 = [1, ...rev_path];
        let paren1 = precedence_dhexp(d1) > precedence_Ap;
        let paren2 = precedence_dhexp(d2) >= precedence_Ap;
        let r1 =
          of_dhexp'(
            instance_click_fn,
            paren1,
            prefix,
            NotInHole,
            rev_path1,
            d1,
          );

        let r2 =
          of_dhexp'(
            instance_click_fn,
            paren2,
            prefix,
            NotInHole,
            rev_path2,
            d2,
          );

        of_exp_BinOp(prefix, err_status, rev_path, r1, UHExp.Space, r2);
      | BoolLit(b) => of_BoolLit(prefix, err_status, rev_path, b)
      | NumLit(n) => of_NumLit(prefix, err_status, rev_path, n)
      | Triv => of_Triv(prefix, err_status, rev_path)
      | BinNumOp(op, d1, d2) =>
        let rev_path1 = [0, ...rev_path];
        let rev_path2 = [1, ...rev_path];
        let prec_d = precedence_dhexp(d);
        let paren1 = precedence_dhexp(d1) > prec_d;
        let paren2 = precedence_dhexp(d2) >= prec_d;
        let r1 =
          of_dhexp'(
            instance_click_fn,
            paren1,
            prefix,
            NotInHole,
            rev_path1,
            d1,
          );

        let r2 =
          of_dhexp'(
            instance_click_fn,
            paren2,
            prefix,
            NotInHole,
            rev_path2,
            d2,
          );

        of_exp_BinOp(
          prefix,
          err_status,
          rev_path,
          r1,
          Dynamics.DHExp.to_op(op),
          r2,
        );
      | Inj(ty, side, d1) =>
        let rev_path1 = [0, ...rev_path];
        let rev_path2 = [1, ...rev_path];
        let r1 = of_htype(false, prefix, rev_path1, ty);
        let r2 =
          of_dhexp'(
            instance_click_fn,
            false,
            prefix,
            NotInHole,
            rev_path2,
            d1,
          );
        of_InjAnn(prefix, err_status, rev_path, r1, side, r2);
      | Pair(d1, d2) =>
        let rev_path1 = [0, ...rev_path];
        let rev_path2 = [1, ...rev_path];
        let paren1 = precedence_dhexp(d1) >= precedence_Comma;
        let paren2 = precedence_dhexp(d2) > precedence_Comma;
        let r1 =
          of_dhexp'(
            instance_click_fn,
            paren1,
            prefix,
            NotInHole,
            rev_path1,
            d1,
          );
        let r2 =
          of_dhexp'(
            instance_click_fn,
            paren2,
            prefix,
            NotInHole,
            rev_path2,
            d2,
          );
        of_exp_BinOp(prefix, err_status, rev_path, r1, UHExp.Comma, r2);
      | ListNil(_) => of_ListNil(prefix, err_status, rev_path)
      | Cons(d1, d2) =>
        let rev_path1 = [0, ...rev_path];
        let rev_path2 = [1, ...rev_path];
        let paren1 = precedence_dhexp(d1) >= precedence_Cons;
        let paren2 = precedence_dhexp(d2) > precedence_Cons;
        let r1 =
          of_dhexp'(
            instance_click_fn,
            paren1,
            prefix,
            NotInHole,
            rev_path1,
            d1,
          );
        let r2 =
          of_dhexp'(
            instance_click_fn,
            paren2,
            prefix,
            NotInHole,
            rev_path2,
            d2,
          );
        of_exp_BinOp(prefix, err_status, rev_path, r1, UHExp.Cons, r2);
      | Case(d1, rules, _) =>
        /* TODO: probably need to do something with current rule */
        /* | Case(d1, (x, d2), (y, d3)) => */
        let rev_path1 = [0, ...rev_path];
        let r1 =
          of_dhexp'(
            instance_click_fn,
            false,
            prefix,
            NotInHole,
            rev_path1,
            d1,
          );
        let rpcs =
          List.mapi(
            (i, rule) => {
              let rev_pathr = [i + 1, ...rev_path];
              switch (rule) {
              | Rule(dp, _) =>
                let rev_pathp = [0, ...rev_pathr];
                let rp = of_dhpat(instance_click_fn, prefix, rev_pathp, dp);
                let rc = taggedText(["elided"], "...");
                (rp, rc);
              };
            },
            rules,
          );
        of_Case(prefix, err_status, rev_path, r1, rpcs, None);
      | EmptyHole(u, i, sigma) =>
        let inst = (u, i);
        let hole_label = hole_label_of(inst);
        let r =
          dbg_SHOW_SIGMAS
            ? hole_label
              ^^ of_sigma(instance_click_fn, prefix, rev_path, sigma)
            : hole_label;
        let attrs = [
          Tyxml_js.Html5.a_onclick(_ => {
            instance_click_fn(inst);
            true;
          }),
        ];

        let inst_cls = cls_of_inst(inst);
        term_with_attrs(
          prefix,
          err_status,
          rev_path,
          ["EmptyHole", "hole-instance", "selected-instance", inst_cls],
          attrs,
          r,
        );
      | NonEmptyHole(reason, u, _, sigma, d1) =>
        let rev_path1 = [0, ...rev_path];
        let r1 =
          of_dhexp'(
            instance_click_fn,
            false,
            prefix,
            InHole(reason, u),
            rev_path1,
            d1,
          );

        let r =
          dbg_SHOW_SIGMAS
            ? r1 ^^ of_sigma(instance_click_fn, prefix, rev_path, sigma) : r1;
        term(prefix, err_status, rev_path, ["NonEmptyHole"], r);
      | Cast(Cast(d1, ty1, ty2), ty3, ty4)
          when _SHOW_CASTS && HTyp.eq(ty2, ty3) =>
        let rev_path1 = [0, ...rev_path];
        let inner_rev_path1 = [0, ...rev_path1];
        let inner_rev_path2 = [1, ...rev_path1];
        let inner_rev_path3 = [2, ...rev_path1];
        let rev_path3 = [2, ...rev_path];
        let paren1 = precedence_dhexp(d1) > precedence_const;
        let r1 =
          of_dhexp'(
            instance_click_fn,
            paren1,
            prefix,
            NotInHole,
            inner_rev_path1,
            d1,
          );

        let r2 = of_htype(false, prefix, inner_rev_path2, ty1);
        let r3 = of_htype(false, prefix, inner_rev_path3, ty2);
        let r5 = of_htype(false, prefix, rev_path3, ty4);
        of_chained_Cast(prefix, err_status, rev_path, r1, r2, r3, r5);
      | Cast(d1, ty1, ty2) =>
        if (_SHOW_CASTS) {
          let rev_path1 = [0, ...rev_path];
          let rev_path2 = [1, ...rev_path];
          let rev_path3 = [2, ...rev_path];
          let paren1 = precedence_dhexp(d1) > precedence_const;
          let r1 =
            of_dhexp'(
              instance_click_fn,
              paren1,
              prefix,
              NotInHole,
              rev_path1,
              d1,
            );

          let r2 = of_htype(false, prefix, rev_path2, ty1);
          let r3 = of_htype(false, prefix, rev_path3, ty2);
          of_Cast(prefix, err_status, rev_path, r1, r2, r3);
        } else {
          let rev_path1 = [0, ...rev_path];
          of_dhexp'(
            instance_click_fn,
            false,
            prefix,
            err_status,
            rev_path1,
            d1,
          );
        }
      | FailedCast(Cast(d1, ty1, ty2), ty3, ty4) when HTyp.eq(ty2, ty3) =>
        let rev_path1 = [0, ...rev_path];
        let inner_rev_path1 = [0, ...rev_path1];
        let inner_rev_path2 = [1, ...rev_path1];
        let inner_rev_path3 = [2, ...rev_path1];
        let rev_path3 = [2, ...rev_path];
        let paren1 = precedence_dhexp(d1) > precedence_const;
        let r1 =
          of_dhexp'(
            instance_click_fn,
            paren1,
            prefix,
            NotInHole,
            inner_rev_path1,
            d1,
          );

        let r2 = of_htype(false, prefix, inner_rev_path2, ty1);
        let r3 = of_htype(false, prefix, inner_rev_path3, ty2);
        let r5 = of_htype(false, prefix, rev_path3, ty4);
        of_chained_FailedCast(prefix, err_status, rev_path, r1, r2, r3, r5);
      | FailedCast(d1, ty1, ty2) =>
        let rev_path1 = [0, ...rev_path];
        let rev_path2 = [1, ...rev_path];
        let rev_path3 = [2, ...rev_path];
        let paren1 = precedence_dhexp(d1) > precedence_const;
        let r1 =
          of_dhexp'(
            instance_click_fn,
            paren1,
            prefix,
            NotInHole,
            rev_path1,
            d1,
          );

        let r2 = of_htype(false, prefix, rev_path2, ty1);
        let r3 = of_htype(false, prefix, rev_path3, ty2);
        of_FailedCast(prefix, err_status, rev_path, r1, r2, r3);
      }
    );

  parenthesize ? lparen("(") ^^ doc ^^ rparen(")") : doc;
}
and of_sigma = (instance_click_fn, prefix, rev_path, sigma) => {
  let map_f = ((x, d)) =>
    of_dhexp'(instance_click_fn, false, prefix, NotInHole, rev_path, d)
    ^^ kw("/")
    ^^ PP.text([""], x);

  let docs = List.map(map_f, sigma);
  let doc' =
    switch (docs) {
    | [] => PP.empty
    | [x, ...xs] =>
      let fold_f = (doc, doc') => doc ^^ kw(", ") ^^ doc';
      List.fold_left(fold_f, x, xs);
    };

  lparen("[") ^^ doc' ^^ rparen("]");
};

let of_dhexp = (instance_click_fn, prefix, d) =>
  of_dhexp'(instance_click_fn, false, prefix, NotInHole, [], d);
let html_of_ty = (width, prefix, ty) => {
  let ty_doc = of_htype(false, prefix, [], ty);
  let rev_paths = EditorBoxTypes.mk_rev_paths();
  let ty_sdoc = Pretty.PP.sdoc_of_doc(width, ty_doc, rev_paths);
  Pretty.HTML_Of_SDoc.html_of_sdoc(ty_sdoc, rev_paths);
};
let html_of_dhexp = (instance_click_fn, width, prefix, d) => {
  let dhexp_doc = of_dhexp(instance_click_fn, prefix, d);
  let rev_paths = EditorBoxTypes.mk_rev_paths();
  let dhexp_sdoc = Pretty.PP.sdoc_of_doc(width, dhexp_doc, rev_paths);
  Pretty.HTML_Of_SDoc.html_of_sdoc(dhexp_sdoc, rev_paths);
};
let html_of_var = (width, prefix, x) =>
  html_of_dhexp(_ => (), width, prefix, DHExp.BoundVar(x));

let html_of_hole_instance = (instance_click_fn, width, prefix, (u, i)) => {
  let d = Dynamics.DHExp.EmptyHole(u, i, []);
  html_of_dhexp(instance_click_fn, width, prefix, d);
};
let string_of_cursor_pos = ((k, side)) =>
  switch (side) {
  | Before => "(" ++ string_of_int(k) ++ ", Before)"
  | After => "(" ++ string_of_int(k) ++ ", After)"
  };
