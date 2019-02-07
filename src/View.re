let _SHOW_CASTS = true;

/* Imports */
exception InvariantViolated;
module PP = Pretty.PP;

/* Conveniences */
let (^^) = PP.(^^);
let taggedText = (cls, s) => PP.text(cls, s);
let dollar = taggedText("dollar", "$");
let kw = taggedText("kw");
let lparen = taggedText("lparen");
let rparen = taggedText("rparen");
let op = taggedText("op");
let var = s => taggedText("var", s);
let paletteName = s => taggedText("paletteName", s);
let space = taggedText("space", " ");

/* Helpers */
let rec id_of_rev_path = (prefix, rev_path) =>
  switch (rev_path) {
  | [] => prefix ++ "path_"
  | [x, ...xs] => id_of_rev_path(prefix, xs) ++ "_" ++ string_of_int(x)
  };

let cls_from_classes = (err_status, classes) =>
  switch (err_status) {
  | InHole(_, u) => [
      "in_err_hole",
      "in_err_hole_" ++ string_of_int(u),
      ...classes,
    ]
  | NotInHole => classes
  };

let cls_from = (err_status, cls) =>
  switch (err_status) {
  | InHole(_, u) => ["in_err_hole", "in_err_hole_" ++ string_of_int(u), cls]
  | NotInHole => [cls]
  };

let term_classes = (prefix, err_status, rev_path, classes, doc) => {
  let id' = id_of_rev_path(prefix, rev_path);
  PP.tagged(
    cls_from_classes(err_status, classes),
    Some((id', rev_path)),
    None,
    doc,
  );
};

let term = (prefix, err_status, rev_path, cls, doc) => {
  let id' = id_of_rev_path(prefix, rev_path);
  PP.tagged(cls_from(err_status, cls), Some((id', rev_path)), None, doc);
};

let term_with_attrs = (prefix, err_status, rev_path, classes, attrs, doc) => {
  let id' = id_of_rev_path(prefix, rev_path);
  PP.tagged(
    cls_from_classes(err_status, classes),
    Some((id', rev_path)),
    Some(attrs),
    doc,
  );
};

let of_var_binding = (prefix, rev_path, x) => {
  let id = id_of_rev_path(prefix, rev_path);
  PP.tagged(
    ["var_binding"],
    Some((id, rev_path)),
    None,
    taggedText("var", x),
  );
};

let optionalBreakSp = PP.optionalBreak(" ");
let optionalBreakNSp = PP.optionalBreak("");

/* Parenthesized */
let of_Parenthesized = (is_block, prefix, rev_path, r1) =>
  term(
    prefix,
    NotInHole,
    rev_path,
    "Parenthesized",
    is_block ?
      PP.blockBoundary
      ^^ lparen("(")
      ^^ PP.nestAbsolute(2, r1)
      ^^ PP.mandatoryBreak
      ^^ rparen(")") :
      lparen("(") ^^ r1 ^^ rparen(")"),
  );

/* Generic operator printing */
let of_op = (op_s, op_cls) =>
  PP.tagged(
    ["seq-op", op_cls],
    None,
    None,
    String.length(op_s) == 1 ?
      taggedText("op-no-margin", op_s) :
      optionalBreakNSp
      ^^ taggedText("op-before-1", "​​")
      ^^ taggedText("op-before-2", "‌")
      ^^ taggedText("op-center", op_s)
      ^^ taggedText("op-after-1", "​")
      ^^ taggedText("op-after-2", "​")
      ^^ optionalBreakNSp,
  );

/* Types */
let string_of_ty_op = op =>
  switch (op) {
  | UHTyp.Arrow => "Arrow"
  | UHTyp.Sum => "Sum"
  | UHTyp.Prod => "Prod"
  };

let of_ty_op = op => {
  let op_cls = "op-" ++ string_of_ty_op(op);
  switch (op) {
  | UHTyp.Arrow => of_op(" " ++ LangUtil.typeArrowSym ++ " ", op_cls)
  | UHTyp.Sum => of_op(" | ", op_cls)
  | UHTyp.Prod => of_op(", ", op_cls)
  };
};

let of_Hole = (prefix, err_status, rev_path, cls, hole_name) =>
  term(
    prefix,
    err_status,
    rev_path,
    cls,
    taggedText("hole-before-1", "​​")
    ^^ taggedText("hole-before-2", "​")
    ^^ taggedText("holeName", hole_name)
    ^^ taggedText("hole-after-1", "​")
    ^^ taggedText("hole-after-2", "​"),
  );

let precedence_const = 0;
let precedence_Prod = 1;
let precedence_Sum = 2;
let precedence_Arrow = 3;
let precedence_ty = ty =>
  switch (ty) {
  | HTyp.Num
  | HTyp.Bool
  | HTyp.Hole
  | HTyp.Unit
  | HTyp.List(_) => precedence_const
  | HTyp.Prod(_, _) => precedence_Prod
  | HTyp.Sum(_, _) => precedence_Sum
  | HTyp.Arrow(_, _) => precedence_Arrow
  };
let of_Bool = (prefix, rev_path) =>
  term(prefix, NotInHole, rev_path, "Bool", kw("Bool"));
let of_Num = (prefix, rev_path) =>
  term(prefix, NotInHole, rev_path, "Num", kw("Num"));
let of_Unit = (prefix, rev_path) =>
  term(prefix, NotInHole, rev_path, "Unit", kw("Unit"));
let of_ty_BinOp = (prefix, err_status, rev_path, r1, op, r2) =>
  term(
    prefix,
    err_status,
    rev_path,
    string_of_ty_op(op),
    r1 ^^ of_ty_op(op) ^^ r2,
  );
let of_List = (prefix, rev_path, r1) =>
  term(
    prefix,
    NotInHole,
    rev_path,
    "List",
    kw("List") ^^ lparen("(") ^^ r1 ^^ rparen(")"),
  );
let rec of_htype = (parenthesize, prefix, rev_path, ty) => {
  let d =
    switch (ty) {
    | HTyp.Bool => of_Bool(prefix, rev_path)
    | HTyp.Num => of_Num(prefix, rev_path)
    | HTyp.Unit => of_Unit(prefix, rev_path)
    | HTyp.List(ty1) =>
      let rev_path1 = [0, ...rev_path];
      let r1 = of_htype(false, prefix, rev_path1, ty1);
      of_List(prefix, rev_path, r1);
    | HTyp.Arrow(ty1, ty2) =>
      let rev_path1 = [0, ...rev_path];
      let rev_path2 = [1, ...rev_path];
      let paren1 = precedence_ty(ty1) >= precedence_Arrow;
      let paren2 = precedence_ty(ty2) > precedence_Arrow;
      let r1 = of_htype(paren1, prefix, rev_path1, ty1);
      let r2 = of_htype(paren2, prefix, rev_path2, ty2);
      of_ty_BinOp(prefix, NotInHole, rev_path, r1, UHTyp.Arrow, r2);
    | HTyp.Sum(ty1, ty2) =>
      let rev_path1 = [0, ...rev_path];
      let rev_path2 = [1, ...rev_path];
      let paren1 = precedence_ty(ty1) >= precedence_Sum;
      let paren2 = precedence_ty(ty2) > precedence_Sum;
      let r1 = of_htype(paren1, prefix, rev_path1, ty1);
      let r2 = of_htype(paren2, prefix, rev_path2, ty2);
      of_ty_BinOp(prefix, NotInHole, rev_path, r1, UHTyp.Sum, r2);
    | HTyp.Prod(ty1, ty2) =>
      let rev_path1 = [0, ...rev_path];
      let rev_path2 = [1, ...rev_path];
      let paren1 = precedence_ty(ty1) >= precedence_Prod;
      let paren2 = precedence_ty(ty2) > precedence_Prod;
      let r1 = of_htype(paren1, prefix, rev_path1, ty1);
      let r2 = of_htype(paren2, prefix, rev_path2, ty2);
      of_ty_BinOp(prefix, NotInHole, rev_path, r1, UHTyp.Prod, r2);
    | HTyp.Hole => of_Hole(prefix, NotInHole, rev_path, "Hole", "?")
    };
  parenthesize ? lparen("(") ^^ d ^^ rparen(")") : d;
};
let rec of_uhtyp = (prefix, rev_path, uty) =>
  switch (uty) {
  | UHTyp.Parenthesized(uty1) =>
    let rev_path1 = [0, ...rev_path];
    let r1 = of_uhtyp(prefix, rev_path1, uty1);
    of_Parenthesized(false, prefix, rev_path, r1);
  | UHTyp.Bool => of_Bool(prefix, rev_path)
  | UHTyp.Num => of_Num(prefix, rev_path)
  | UHTyp.Unit => of_Unit(prefix, rev_path)
  | UHTyp.List(uty1) =>
    let rev_path1 = [0, ...rev_path];
    let r1 = of_uhtyp(prefix, rev_path1, uty1);
    of_List(prefix, rev_path, r1);
  | UHTyp.OpSeq(skel, seq) =>
    term(
      prefix,
      NotInHole,
      rev_path,
      "OpSeq",
      of_uhtyp_skel(prefix, rev_path, skel, seq),
    )
  | UHTyp.Hole => of_Hole(prefix, NotInHole, rev_path, "Hole", "?")
  }
and of_uhtyp_skel = (prefix, rev_path, skel, seq) =>
  switch (skel) {
  | Skel.Placeholder(n) =>
    switch (OperatorSeq.seq_nth(n, seq)) {
    | Some(utyn) =>
      let rev_path_n = [n, ...rev_path];
      of_uhtyp(prefix, rev_path_n, utyn);
    | None => raise(InvariantViolated)
    }
  | Skel.BinOp(_, op, skel1, skel2) =>
    let r1 = of_uhtyp_skel(prefix, rev_path, skel1, seq);
    let r2 = of_uhtyp_skel(prefix, rev_path, skel2, seq);
    of_ty_BinOp(prefix, NotInHole, rev_path, r1, op, r2);
  };

/* Expressions and Patterns */

let of_Asc = (prefix, err_status, rev_path, r1, r2) =>
  term(
    prefix,
    err_status,
    rev_path,
    "Asc",
    r1 ^^ space ^^ op(":") ^^ space ^^ r2,
  );

let classes_of_var_err_status = var_err_status =>
  switch (var_err_status) {
  | InVHole(u) => ["InVHole", "InVHole_" ++ string_of_int(u)]
  | NotInVHole => []
  };

let of_Wild = (prefix, err_status, rev_path) =>
  term(prefix, err_status, rev_path, "Wild", var("_"));

let of_Var = (prefix, err_status, var_err_status, rev_path, x) =>
  term_classes(
    prefix,
    err_status,
    rev_path,
    ["Var", ...classes_of_var_err_status(var_err_status)],
    var(x),
  );

let of_Let = (prefix, err_status, rev_path, rx, rann, r1, r2) => {
  let first_part = PP.blockBoundary ^^ kw("let") ^^ space ^^ rx;
  let second_part =
    of_op(" = ", "let-equals")
    ^^ PP.nestAbsolute(2, r1)
    ^^ PP.mandatoryBreak
    ^^ r2;
  let view =
    switch (rann) {
    | Some(r) => first_part ^^ of_op(" : ", "ann") ^^ r ^^ second_part
    | None => first_part ^^ second_part
    };
  term(prefix, err_status, rev_path, "Let", view);
};

let of_FixF = (prefix, err_status, rev_path, rx, rty, r1) => {
  let view =
    kw("fix")
    ^^ space
    ^^ rx
    ^^ of_op(":", "ann")
    ^^ rty
    ^^ of_op(".", "lambda-dot")
    ^^ r1;
  term(prefix, err_status, rev_path, "Lam", view);
};

let of_Lam = (prefix, err_status, rev_path, rx, rann, r1) => {
  let first_part = taggedText("lambda-sym", LangUtil.lamSym) ^^ rx;
  let second_part = taggedText("lambda-dot", ".") ^^ r1;
  let view =
    switch (rann) {
    | Some(r) => first_part ^^ of_op(":", "ann") ^^ r ^^ second_part
    | None => first_part ^^ second_part
    };
  term(prefix, err_status, rev_path, "Lam", view);
};

let string_of_bool = b => b ? "True" : "False";

let of_BoolLit = (prefix, err_status, rev_path, b) =>
  term(
    prefix,
    err_status,
    rev_path,
    "BoolLit",
    taggedText("boolean", string_of_bool(b)),
  );
let of_NumLit = (prefix, err_status, rev_path, n) =>
  term(
    prefix,
    err_status,
    rev_path,
    "NumLit",
    taggedText("number", string_of_int(n)),
  );
let of_Triv = (prefix, err_status, rev_path) =>
  term(prefix, err_status, rev_path, "Triv", taggedText("triv", "()"));

let of_ListNil = (prefix, err_status, rev_path) =>
  term(prefix, err_status, rev_path, "ListNil", kw("[]"));

let of_Inj = (prefix, err_status, rev_path, side, r) =>
  term(
    prefix,
    err_status,
    rev_path,
    "Inj",
    kw("inj")
    ^^ lparen("[")
    ^^ kw(LangUtil.string_of_side(side))
    ^^ rparen("]")
    ^^ lparen("(")
    ^^ r
    ^^ rparen(")"),
  );

let of_InjAnn = (prefix, err_status, rev_path, rty, side, r) =>
  term(
    prefix,
    err_status,
    rev_path,
    "Inj",
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

let of_Case = (prefix, err_status, rev_path, r1, rpcs) => {
  let rrules =
    List.fold_left(
      (rrs, rpc) => {
        let (rp, rc) = rpc;
        let line_break = rrs == PP.empty ? PP.empty : PP.mandatoryBreak;
        rrs
        ^^ line_break
        ^^ of_op("| ", "rule-bar")
        ^^ rp
        ^^ of_op(" " ++ LangUtil.caseArrowSym ++ " ", "case-arrow")
        ^^ PP.nestAbsolute(2, rc);
      },
      PP.empty,
      rpcs,
    );
  term(
    prefix,
    err_status,
    rev_path,
    "Case",
    PP.blockBoundary
    ^^ kw("case")
    ^^ space
    ^^ r1
    ^^ PP.mandatoryBreak
    ^^ rrules
    ^^ PP.mandatoryBreak
    ^^ kw("end"),
  );
};

let of_CaseAnn = (prefix, err_status, rev_path, r1, rpcs) =>
  of_Case(prefix, err_status, rev_path, r1, rpcs);

let cast_arrow = op(" ⇨ ");
let of_Cast = (prefix, err_status, rev_path, r1, rty1, rty2) =>
  term(
    prefix,
    err_status,
    rev_path,
    "Cast",
    _SHOW_CASTS ?
      r1 ^^ lparen("⟨") ^^ rty1 ^^ cast_arrow ^^ rty2 ^^ rparen("⟩") : r1,
  );

let of_chained_Cast = (prefix, err_status, rev_path, r1, rty1, rty2, rty4) =>
  term(
    prefix,
    err_status,
    rev_path,
    "Cast",
    _SHOW_CASTS ?
      r1
      ^^ lparen("<")
      ^^ rty1
      ^^ cast_arrow
      ^^ rty2
      ^^ cast_arrow
      ^^ rty4
      ^^ rparen(">") :
      r1,
  );

let failed_cast_arrow = taggedText("failed-cast-arrow", " ⇨ ");
let of_FailedCast = (prefix, err_status, rev_path, r1, rty1, rty2) =>
  term(
    prefix,
    err_status,
    rev_path,
    "FailedCast",
    r1 ^^ lparen("<") ^^ rty1 ^^ failed_cast_arrow ^^ rty2 ^^ rparen(">"),
  );

let of_chained_FailedCast =
    (prefix, err_status, rev_path, r1, rty1, rty2, rty4) =>
  term(
    prefix,
    err_status,
    rev_path,
    "FailedCast",
    r1
    ^^ lparen("<")
    ^^ rty1
    ^^ failed_cast_arrow
    ^^ rty2
    ^^ failed_cast_arrow
    ^^ rty4
    ^^ rparen(">"),
  );

let is_block = e =>
  switch (e) {
  | UHExp.Tm(_, UHExp.Let(_, _, _, _)) => true
  | UHExp.Tm(_, UHExp.Case(_, _)) => true
  | _ => false
  };

type palette_stuff = {
  palette_view_ctx: Palettes.PaletteViewCtx.t,
  mk_editor_box: EditorBoxTypes.mk_editor_box,
  do_action: Action.t => unit,
};

let string_of_pat_op = op =>
  switch (op) {
  | UHPat.Comma => "Comma"
  | UHPat.Space => "Space"
  | UHPat.Cons => "Cons"
  };

let of_pat_op = op => {
  let op_cls = "op-" ++ string_of_pat_op(op);
  switch (op) {
  | UHPat.Comma => of_op(", ", op_cls)
  | UHPat.Space => of_op(" ", op_cls)
  | UHPat.Cons => of_op("::", op_cls)
  };
};

let of_pat_BinOp = (prefix, err_status, rev_path, r1, op, r2) =>
  term(
    prefix,
    err_status,
    rev_path,
    string_of_pat_op(op),
    r1 ^^ of_pat_op(op) ^^ r2,
  );

let rec of_hpat = (prefix, rev_path, p) =>
  switch (p) {
  | UHPat.Parenthesized(p1) =>
    let rev_path1 = [0, ...rev_path];
    let r1 = of_hpat(prefix, rev_path1, p1);
    of_Parenthesized(false, prefix, rev_path, r1);
  | UHPat.Pat(err_status, p') =>
    switch (p') {
    | UHPat.EmptyHole(u) =>
      of_Hole(
        prefix,
        err_status,
        rev_path,
        "EmptyHole",
        string_of_int(u + 1),
      )
    | UHPat.Wild => of_Wild(prefix, err_status, rev_path)
    | UHPat.Var(x) => of_Var(prefix, err_status, NotInVHole, rev_path, x)
    | UHPat.NumLit(n) => of_NumLit(prefix, err_status, rev_path, n)
    | UHPat.BoolLit(b) => of_BoolLit(prefix, err_status, rev_path, b)
    | UHPat.Inj(side, p1) =>
      let rev_path1 = [0, ...rev_path];
      let r1 = of_hpat(prefix, rev_path1, p1);
      of_Inj(prefix, err_status, rev_path, side, r1);
    | UHPat.ListNil => of_ListNil(prefix, err_status, rev_path)
    | UHPat.OpSeq(skel, seq) =>
      term(
        prefix,
        err_status,
        rev_path,
        "OpSeq",
        of_skel_pat(prefix, rev_path, skel, seq),
      )
    }
  }
and of_skel_pat = (prefix, rev_path, skel, seq) =>
  switch (skel) {
  | Skel.Placeholder(n) =>
    switch (OperatorSeq.seq_nth(n, seq)) {
    | None => raise(InvariantViolated)
    | Some(pn) =>
      let rev_path_n = [n, ...rev_path];
      of_hpat(prefix, rev_path_n, pn);
    }
  | Skel.BinOp(err_status, op, skel1, skel2) =>
    let r1 = of_skel_pat(prefix, rev_path, skel1, seq);
    let r2 = of_skel_pat(prefix, rev_path, skel2, seq);
    of_pat_BinOp(prefix, err_status, rev_path, r1, op, r2);
  };

let string_of_exp_op = op =>
  switch (op) {
  | UHExp.Plus => "Plus"
  | UHExp.Times => "Times"
  | UHExp.LessThan => "LessThan"
  | UHExp.Space => "Space"
  | UHExp.Comma => "Comma"
  | UHExp.Cons => "Cons"
  };

let of_exp_op = op => {
  let op_cls = "op-" ++ string_of_exp_op(op);
  switch (op) {
  | UHExp.Plus => of_op(" + ", op_cls)
  | UHExp.Times => of_op("*", op_cls)
  | UHExp.LessThan => of_op(" < ", op_cls)
  | UHExp.Space => of_op(" ", op_cls)
  | UHExp.Comma => of_op(", ", op_cls)
  | UHExp.Cons => of_op("::", op_cls)
  };
};

let of_exp_BinOp = (prefix, err_status, rev_path, r1, op, r2) =>
  term(
    prefix,
    err_status,
    rev_path,
    string_of_exp_op(op),
    r1 ^^ of_exp_op(op) ^^ r2,
  );

/* special cased below */
let of_Times_with_space = (prefix, err_status, rev_path, r1, op, r2) =>
  term(
    prefix,
    err_status,
    rev_path,
    string_of_exp_op(op),
    r1 ^^ of_op(" * ", "op-Times") ^^ r2,
  );

let rec of_hexp = (palette_stuff, prefix, rev_path, e) =>
  switch (e) {
  | UHExp.Parenthesized(e1) =>
    let rev_path1 = [0, ...rev_path];
    let r1 = of_hexp(palette_stuff, prefix, rev_path1, e1);
    of_Parenthesized(is_block(e1), prefix, rev_path, r1);
  | UHExp.Tm(err_status, e') =>
    switch (e') {
    | UHExp.Asc(e1, ty) =>
      let rev_path1 = [0, ...rev_path];
      let rev_path2 = [1, ...rev_path];
      let r1 = of_hexp(palette_stuff, prefix, rev_path1, e1);
      let r2 = of_uhtyp(prefix, rev_path2, ty);
      of_Asc(prefix, err_status, rev_path, r1, r2);
    | UHExp.Var(var_err_status, x) =>
      of_Var(prefix, err_status, var_err_status, rev_path, x)
    | UHExp.Let(p, ann, e1, e2) =>
      let rp = of_hpat(prefix, [0, ...rev_path], p);
      let rann =
        switch (ann) {
        | Some(uty1) => Some(of_uhtyp(prefix, [1, ...rev_path], uty1))
        | None => None
        };
      let r1 = of_hexp(palette_stuff, prefix, [2, ...rev_path], e1);
      let r2 = of_hexp(palette_stuff, prefix, [3, ...rev_path], e2);
      of_Let(prefix, err_status, rev_path, rp, rann, r1, r2);
    | UHExp.Lam(p, ann, e1) =>
      let rp = of_hpat(prefix, [0, ...rev_path], p);
      let rann =
        switch (ann) {
        | Some(uty1) => Some(of_uhtyp(prefix, [1, ...rev_path], uty1))
        | None => None
        };
      let r1 = of_hexp(palette_stuff, prefix, [2, ...rev_path], e1);
      of_Lam(prefix, err_status, rev_path, rp, rann, r1);
    | UHExp.BoolLit(b) => of_BoolLit(prefix, err_status, rev_path, b)
    | UHExp.NumLit(n) => of_NumLit(prefix, err_status, rev_path, n)
    | UHExp.ListNil => of_ListNil(prefix, err_status, rev_path)
    | UHExp.Inj(side, e) =>
      let rev_path1 = [0, ...rev_path];
      let r1 = of_hexp(palette_stuff, prefix, rev_path1, e);
      of_Inj(prefix, err_status, rev_path, side, r1);
    | UHExp.Case(e1, rules) =>
      let rev_path1 = [0, ...rev_path];
      let r1 = of_hexp(palette_stuff, prefix, rev_path1, e1);
      let rpcs =
        List.mapi(
          (i, rule) => {
            let rev_pathr = [i + 1, ...rev_path];
            switch (rule) {
            | UHExp.Rule(p, c) =>
              let rev_pathp = [0, ...rev_pathr];
              let rev_pathc = [1, ...rev_pathr];
              let rp = of_hpat(prefix, rev_pathp, p);
              let rc = of_hexp(palette_stuff, prefix, rev_pathc, c);
              (rp, rc);
            };
          },
          rules,
        );
      of_Case(prefix, err_status, rev_path, r1, rpcs);
    | UHExp.EmptyHole(u) =>
      of_Hole(
        prefix,
        err_status,
        rev_path,
        "EmptyHole",
        string_of_int(u + 1),
      )
    | UHExp.OpSeq(skel, seq) =>
      term(
        prefix,
        err_status,
        rev_path,
        "OpSeq",
        of_skel(palette_stuff, prefix, rev_path, skel, seq),
      )
    | UHExp.ApPalette(name, serialized_model, (_, hole_map)) =>
      switch (
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
          term(prefix, err_status, rev_path, "ApPalette", paletteName(name));
        let paletteDelim =
          term(prefix, err_status, rev_path, "ApPalette", dollar);
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
      }
    }
  }
and of_skel = (palette_stuff, prefix, rev_path, skel, seq) =>
  switch (skel) {
  | Skel.Placeholder(n) =>
    switch (OperatorSeq.seq_nth(n, seq)) {
    | Some(en) =>
      let rev_path_n = [n, ...rev_path];
      of_hexp(palette_stuff, prefix, rev_path_n, en);
    | None => raise(InvariantViolated)
    }
  | Skel.BinOp(err_status, UHExp.Times as op, skel1, skel2) =>
    let r1 = of_skel(palette_stuff, prefix, rev_path, skel1, seq);
    let r2 = of_skel(palette_stuff, prefix, rev_path, skel2, seq);
    switch (Skel.rightmost_op(skel1)) {
    | Some(UHExp.Space) =>
      of_Times_with_space(prefix, err_status, rev_path, r1, op, r2)
    | _ =>
      switch (Skel.leftmost_op(skel2)) {
      | Some(UHExp.Space) =>
        of_Times_with_space(prefix, err_status, rev_path, r1, op, r2)
      | _ => of_exp_BinOp(prefix, err_status, rev_path, r1, op, r2)
      }
    };
  | Skel.BinOp(err_status, op, skel1, skel2) =>
    let r1 = of_skel(palette_stuff, prefix, rev_path, skel1, seq);
    let r2 = of_skel(palette_stuff, prefix, rev_path, skel2, seq);
    of_exp_BinOp(prefix, err_status, rev_path, r1, op, r2);
  };

open Dynamics;
let precedence_Ap = 1;
let precedence_Times = 2;
let precedence_Plus = 3;
let precedence_Cons = 4;
let precedence_LessThan = 5;
let precedence_Comma = 6;
let precedence_max = 7;
let rec precedence_dhpat = dp =>
  DHPat.(
    switch (dp) {
    | EmptyHole(_)
    | NonEmptyHole(_, _, _, _)
    | Wild
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
    | BoolLit(_)
    | NumLit(_)
    | ListNil
    | Inj(_, _, _)
    | Pair(_, _)
    | EmptyHole(_, _, _)
    | Cast(_, _, _)
    | Triv
    | FailedCast(_, _, _) => precedence_const
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
let hole_label_of = inst => taggedText("holeName", hole_label_s(inst));
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
      | NonEmptyHole(reason, u, i, dp1) =>
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
        term(prefix, err_status, rev_path, "NonEmptyHole", r);
      | Wild => of_Wild(prefix, err_status, rev_path)
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
        of_Var(prefix, err_status, InVHole(u), rev_path, x)
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
      | Lam(dp, ann, d1) =>
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

        of_exp_BinOp(prefix, err_status, rev_path, r1, DHExp.to_op(op), r2);
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
      | ListNil => of_ListNil(prefix, err_status, rev_path)
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
              | Rule(dp, dc) =>
                let rev_pathp = [0, ...rev_pathr];
                let rp = of_dhpat(instance_click_fn, prefix, rev_pathp, dp);
                let rc = taggedText("elided", "...");
                (rp, rc);
              };
            },
            rules,
          );
        of_CaseAnn(prefix, err_status, rev_path, r1, rpcs);
      | EmptyHole(u, i, sigma) =>
        let inst = (u, i);
        let hole_label = hole_label_of(inst);
        let r =
          dbg_SHOW_SIGMAS ?
            hole_label ^^ of_sigma(instance_click_fn, prefix, rev_path, sigma) :
            hole_label;
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
          dbg_SHOW_SIGMAS ?
            r1 ^^ of_sigma(instance_click_fn, prefix, rev_path, sigma) : r1;
        term(prefix, err_status, rev_path, "NonEmptyHole", r);
      | Cast(Cast(d1, ty1, ty2), ty3, ty4) when HTyp.eq(ty2, ty3) =>
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
    ^^ PP.text("", x);

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
let string_of_cursor_side = cursor_side =>
  switch (cursor_side) {
  | In(n) => "In(" ++ string_of_int(n) ++ ")"
  | Before => "Before"
  | After => "After"
  };
