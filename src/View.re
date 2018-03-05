/* TODO for code cleanup
 * - normalize CSS classes to use dashes instead of underscores
 */
exception InvariantViolated;

/* Utility functions */
module PP = Pretty.PP;

open Semantics.Core;

let (^^) = PP.(^^);

let taggedText cls s => PP.text cls s;

let kw = taggedText "kw";

let lparen = taggedText "lparen";

let rparen = taggedText "rparen";

let op = taggedText "op";

let var s => taggedText "var" s;

let space = taggedText "space" " ";

let rec id_of_rev_path prefix rev_path =>
  switch rev_path {
  | [] => prefix ^ "path_"
  | [x, ...xs] => id_of_rev_path prefix xs ^ "_" ^ string_of_int x
  };

let cls_from_classes err_status classes =>
  switch err_status {
  | InHole u => ["in_err_hole", "in_err_hole_" ^ string_of_int u, ...classes]
  | NotInHole => classes
  };

let cls_from err_status cls =>
  switch err_status {
  | InHole u => ["in_err_hole", "in_err_hole_" ^ string_of_int u, cls]
  | NotInHole => [cls]
  };

let term prefix err_status rev_path cls doc => {
  let id' = id_of_rev_path prefix rev_path;
  PP.tagged (cls_from err_status cls) (Some (id', rev_path)) None doc
};

let term_with_attrs prefix err_status rev_path classes attrs doc => {
  let id' = id_of_rev_path prefix rev_path;
  PP.tagged
    (cls_from_classes err_status classes)
    (Some (id', rev_path))
    (Some attrs)
    doc
};

let optionalBreakSp = PP.optionalBreak " ";

let optionalBreakNSp = PP.optionalBreak "";

let of_Parenthesized is_block prefix rev_path r1 =>
  term
    prefix
    NotInHole
    rev_path
    "Parenthesized"
    (
      is_block ?
        PP.blockBoundary ^^
        taggedText "openParens" "(" ^^
        PP.nestAbsolute 2 r1 ^^
        PP.mandatoryBreak ^^ taggedText "closeParens" ")" :
        taggedText "openParens" "(" ^^ r1 ^^ taggedText "closeParens" ")"
    );

let rec str_of_expr_op op =>
  switch op {
  | UHExp.Plus => ("+", "op-Plus")
  | UHExp.Times => ("*", "op-Times")
  | UHExp.Space => (" ", "op-Space")
  };

let rec str_of_ty_op op =>
  switch op {
  | UHTyp.Sum => ("|", "op-Sum")
  | UHTyp.Arrow => ("\226\134\146", "op-Arrow")
  };

let rec of_op_with_space str_of_op op => {
  let (op_s, op_cls) = str_of_op op;
  PP.tagged
    ["seq-op", op_cls]
    None
    None
    (
      taggedText "op-before-1" "\226\128\139\226\128\139" ^^
      taggedText "op-before-2" "\226\128\140" ^^
      taggedText "op-center" (" " ^ op_s ^ " ") ^^
      taggedText "op-after-1" "\226\128\139" ^^
      taggedText "op-after-2" "\226\128\139"
    )
};

let of_expr_op_with_space = of_op_with_space str_of_expr_op;

let of_ty_op_with_space = of_op_with_space str_of_ty_op;

let rec of_op_no_space str_of_op op => {
  let (op_s, op_cls) = str_of_op op;
  PP.tagged ["seq-op", op_cls] None None (taggedText "op-no-margin" op_s)
};

let of_expr_op_no_space = of_op_no_space str_of_expr_op;

let rec of_expr_op op =>
  switch op {
  | UHExp.Plus => of_expr_op_with_space op
  | UHExp.Times => of_expr_op_no_space op
  | UHExp.Space => of_expr_op_no_space op
  };

/* empty holes */
let of_Hole prefix err_status rev_path cls hole_name =>
  term
    prefix
    err_status
    rev_path
    cls
    (
      taggedText "hole-before-1" "\226\128\139\226\128\139" ^^
      taggedText "hole-before-2" "\226\128\139" ^^
      taggedText "holeName" hole_name ^^
      taggedText "hole-after-1" "\226\128\139" ^^
      taggedText "hole-after-2" "\226\128\139"
    );

/* types */
let precedence_const = 0;

let precedence_Sum = 1;

let precedence_Arrow = 2;

let precedence_ty ty =>
  switch ty {
  | HTyp.Num => precedence_const
  | HTyp.Hole => precedence_const
  | HTyp.Sum _ _ => precedence_Sum
  | HTyp.Arrow _ _ => precedence_Arrow
  };

let of_Num prefix rev_path => term prefix NotInHole rev_path "Num" (kw "num");

let rec of_ty_op cls op_s prefix err_status rev_path r1 r2 =>
  term
    prefix
    err_status
    rev_path
    cls
    (r1 ^^ optionalBreakSp ^^ op op_s ^^ optionalBreakSp ^^ r2);

let of_Arrow = of_ty_op "Arrow" "\226\134\146";

let of_Sum = of_ty_op "Sum" "|";

let rec of_htype parenthesize prefix rev_path ty => {
  let d =
    switch ty {
    | HTyp.Num => of_Num prefix rev_path
    | HTyp.Arrow ty1 ty2 =>
      let rev_path1 = [0, ...rev_path];
      let rev_path2 = [1, ...rev_path];
      let paren1 = precedence_ty ty1 >= precedence_Arrow;
      let paren2 = precedence_ty ty2 > precedence_Arrow;
      let r1 = of_htype paren1 prefix rev_path1 ty1;
      let r2 = of_htype paren2 prefix rev_path2 ty2;
      of_Arrow prefix NotInHole rev_path r1 r2
    | HTyp.Sum ty1 ty2 =>
      let rev_path1 = [0, ...rev_path];
      let rev_path2 = [1, ...rev_path];
      let paren1 = precedence_ty ty1 >= precedence_Sum;
      let paren2 = precedence_ty ty2 > precedence_Sum;
      let r1 = of_htype paren1 prefix rev_path1 ty1;
      let r2 = of_htype paren2 prefix rev_path2 ty2;
      of_Sum prefix NotInHole rev_path r1 r2
    | HTyp.Hole => of_Hole prefix NotInHole rev_path "Hole" "?"
    };
  parenthesize ? lparen "(" ^^ d ^^ rparen ")" : d
};

let rec of_uhtyp prefix rev_path uty =>
  switch uty {
  | UHTyp.Parenthesized uty1 =>
    let rev_path1 = [0, ...rev_path];
    let r1 = of_uhtyp prefix rev_path1 uty1;
    of_Parenthesized false prefix rev_path r1
  | UHTyp.Num => of_Num prefix rev_path
  | UHTyp.OpSeq skel seq =>
    term
      prefix
      NotInHole
      rev_path
      "OpSeq"
      (of_uhtyp_skel prefix rev_path skel seq)
  | UHTyp.Hole => of_Hole prefix NotInHole rev_path "Hole" "?"
  }
and of_uhtyp_skel prefix rev_path skel seq =>
  switch skel {
  | Skel.Placeholder n =>
    switch (OperatorSeq.seq_nth n seq) {
    | Some utyn =>
      let rev_path_n = [n, ...rev_path];
      of_uhtyp prefix rev_path_n utyn
    | None => raise InvariantViolated
    }
  | Skel.BinOp _ op skel1 skel2 =>
    let r1 = of_uhtyp_skel prefix rev_path skel1 seq;
    let r2 = of_uhtyp_skel prefix rev_path skel2 seq;
    let op_pp = of_ty_op_with_space op;
    PP.tagged ["skel-binop"] None None (r1 ^^ op_pp ^^ r2)
  };

/* h-exps and z-exps */
let of_Asc prefix err_status rev_path r1 r2 =>
  term prefix err_status rev_path "Asc" (r1 ^^ space ^^ op ":" ^^ space ^^ r2);

let of_Var prefix err_status rev_path x =>
  term prefix err_status rev_path "Var" (var x);

let of_Let prefix err_status rev_path x r1 r2 =>
  term
    prefix
    err_status
    rev_path
    "Let"
    (
      PP.blockBoundary ^^
      kw "let" ^^
      space ^^
      var x ^^
      space ^^
      op "=" ^^ space ^^ PP.nestAbsolute 2 r1 ^^ PP.mandatoryBreak ^^ r2
    );

let of_Lam prefix err_status rev_path x r =>
  term
    prefix
    err_status
    rev_path
    "Lam"
    (kw "\206\187" ^^ var x ^^ taggedText "lambda-dot" "." ^^ r);

let of_LamAnn prefix err_status rev_path x rty r1 =>
  term
    prefix
    err_status
    rev_path
    "LamAnn"
    (
      kw "\206\187" ^^
      var x ^^ kw ":" ^^ rty ^^ taggedText "lambda-dot" "." ^^ r1
    );

let of_Ap prefix err_status rev_path r1 r2 =>
  term prefix err_status rev_path "Ap" (r1 ^^ space ^^ r2);

let of_NumLit prefix err_status rev_path n =>
  term
    prefix
    err_status
    rev_path
    "NumLit"
    (taggedText "number" (string_of_int n));

let of_Plus prefix err_status rev_path r1 r2 =>
  term
    prefix
    err_status
    rev_path
    "Plus"
    (r1 ^^ optionalBreakNSp ^^ op " +" ^^ optionalBreakSp ^^ r2);

let of_Times prefix err_status rev_path r1 r2 =>
  term
    prefix
    err_status
    rev_path
    "Times"
    (r1 ^^ op "*" ^^ optionalBreakNSp ^^ r2);

let of_Space prefix err_status rev_path r1 r2 =>
  term
    prefix
    err_status
    rev_path
    "Space"
    (r1 ^^ op " " ^^ optionalBreakNSp ^^ r2);

let string_of_side side =>
  switch side {
  | UHExp.L => "L"
  | UHExp.R => "R"
  };

let of_Inj prefix err_status rev_path side r =>
  term
    prefix
    err_status
    rev_path
    "Inj"
    (
      kw "inj" ^^
      lparen "[" ^^
      kw (string_of_side side) ^^ rparen "]" ^^ lparen "(" ^^ r ^^ rparen ")"
    );

let of_InjAnn prefix err_status rev_path rty side r =>
  term
    prefix
    err_status
    rev_path
    "Inj"
    (
      kw "inj" ^^
      lparen "[" ^^
      kw (string_of_side side) ^^
      kw "," ^^
      optionalBreakSp ^^
      rty ^^ rparen "]" ^^ lparen "(" ^^ PP.optionalBreak "" ^^ r ^^ rparen ")"
    );

let of_Case prefix err_status rev_path r1 x r2 y r3 =>
  term
    prefix
    err_status
    rev_path
    "Case"
    (
      PP.blockBoundary ^^
      kw "case" ^^
      space ^^
      r1 ^^
      PP.mandatoryBreak ^^
      kw "L" ^^
      lparen "(" ^^
      var x ^^
      rparen ")" ^^
      space ^^
      op "\226\135\146" ^^
      space ^^
      PP.nestAbsolute 2 r2 ^^
      PP.mandatoryBreak ^^
      kw "R" ^^
      lparen "(" ^^
      var y ^^
      rparen ")" ^^ space ^^ op "\226\135\146" ^^ space ^^ PP.nestAbsolute 2 r3
    );

let of_CaseAnn prefix err_status rev_path r1 x r2 y r3 =>
  term
    prefix
    err_status
    rev_path
    "Case"
    (
      PP.blockBoundary ^^
      kw "case" ^^
      space ^^
      r1 ^^
      PP.mandatoryBreak ^^
      kw "L" ^^
      lparen "(" ^^
      var x ^^
      rparen ")" ^^
      space ^^
      op "\226\135\146" ^^
      space ^^
      PP.nestAbsolute 2 r2 ^^
      PP.mandatoryBreak ^^
      kw "R" ^^
      lparen "(" ^^
      var y ^^
      rparen ")" ^^ space ^^ op "\226\135\146" ^^ space ^^ PP.nestAbsolute 2 r3
    );

let cast_arrow = op " \226\135\168 ";

let of_Cast prefix err_status rev_path r1 rty1 rty2 =>
  term
    prefix
    err_status
    rev_path
    "Cast"
    (r1 ^^ lparen "<" ^^ rty1 ^^ cast_arrow ^^ rty2 ^^ rparen ">");

let of_chained_Cast prefix err_status rev_path r1 rty1 rty2 rty4 =>
  term
    prefix
    err_status
    rev_path
    "Cast"
    (
      r1 ^^
      lparen "<" ^^
      rty1 ^^ cast_arrow ^^ rty2 ^^ cast_arrow ^^ rty4 ^^ rparen ">"
    );

let failed_cast_arrow = taggedText "failed-cast-arrow" " \226\135\168 ";

let of_FailedCast prefix err_status rev_path r1 rty1 rty2 =>
  term
    prefix
    err_status
    rev_path
    "FailedCast"
    (r1 ^^ lparen "<" ^^ rty1 ^^ failed_cast_arrow ^^ rty2 ^^ rparen ">");

let of_chained_FailedCast prefix err_status rev_path r1 rty1 rty2 rty4 =>
  term
    prefix
    err_status
    rev_path
    "FailedCast"
    (
      r1 ^^
      lparen "<" ^^
      rty1 ^^
      failed_cast_arrow ^^ rty2 ^^ failed_cast_arrow ^^ rty4 ^^ rparen ">"
    );

let is_block e =>
  switch e {
  | UHExp.Tm _ (UHExp.Let _ _ _) => true
  | UHExp.Tm _ (UHExp.Case _ _ _) => true
  | _ => false
  };

let rec of_hexp prefix rev_path e =>
  switch e {
  | UHExp.Parenthesized e1 =>
    let rev_path1 = [0, ...rev_path];
    let r1 = of_hexp prefix rev_path1 e1;
    of_Parenthesized (is_block e1) prefix rev_path r1
  | UHExp.Tm err_status e' =>
    switch e' {
    | UHExp.Asc e1 ty =>
      let rev_path1 = [0, ...rev_path];
      let rev_path2 = [1, ...rev_path];
      let r1 = of_hexp prefix rev_path1 e1;
      let r2 = of_uhtyp prefix rev_path2 ty;
      of_Asc prefix err_status rev_path r1 r2
    | UHExp.Var x => of_Var prefix err_status rev_path x
    | UHExp.Let x e e' =>
      let rev_path1 = [0, ...rev_path];
      let rev_path2 = [1, ...rev_path];
      let r1 = of_hexp prefix rev_path1 e;
      let r2 = of_hexp prefix rev_path2 e';
      of_Let prefix err_status rev_path x r1 r2
    | UHExp.Lam x e' =>
      let rev_path1 = [0, ...rev_path];
      let r1 = of_hexp prefix rev_path1 e';
      of_Lam prefix err_status rev_path x r1
    | UHExp.NumLit n => of_NumLit prefix err_status rev_path n
    | UHExp.Inj side e =>
      let rev_path1 = [0, ...rev_path];
      let r1 = of_hexp prefix rev_path1 e;
      of_Inj prefix err_status rev_path side r1
    | UHExp.Case e1 (x, e2) (y, e3) =>
      let rev_path1 = [0, ...rev_path];
      let rev_path2 = [1, ...rev_path];
      let rev_path3 = [2, ...rev_path];
      let r1 = of_hexp prefix rev_path1 e1;
      let r2 = of_hexp prefix rev_path2 e2;
      let r3 = of_hexp prefix rev_path3 e3;
      of_Case prefix err_status rev_path r1 x r2 y r3
    | UHExp.EmptyHole u =>
      of_Hole prefix err_status rev_path "EmptyHole" (string_of_int (u + 1))
    | UHExp.OpSeq skel seq =>
      term
        prefix err_status rev_path "OpSeq" (of_skel prefix rev_path skel seq)
    }
  }
and of_skel prefix rev_path skel seq =>
  switch skel {
  | Skel.Placeholder n =>
    switch (OperatorSeq.seq_nth n seq) {
    | Some en =>
      let rev_path_n = [n, ...rev_path];
      of_hexp prefix rev_path_n en
    | None => raise InvariantViolated
    }
  | Skel.BinOp err_status op skel1 skel2 =>
    let r1 = of_skel prefix rev_path skel1 seq;
    let r2 = of_skel prefix rev_path skel2 seq;
    let op_pp =
      switch op {
      | UHExp.Times =>
        switch (Skel.rightmost_op skel1) {
        | Some UHExp.Space => of_expr_op_with_space op
        | _ =>
          switch (Skel.leftmost_op skel2) {
          | Some UHExp.Space => of_expr_op_with_space op
          | _ => of_expr_op op
          }
        }
      | _ => of_expr_op op
      };
    let cls = "skel-binop";
    let cls' = cls_from err_status cls;
    PP.tagged cls' None None (r1 ^^ op_pp ^^ r2)
  };

open Dynamics;

let rec of_bin_num_op op =>
  switch op {
  | DHExp.Plus => taggedText "bin_num_op" " + "
  | DHExp.Times => taggedText "bin_num_op" "*"
  };

let of_BinNumOp prefix err_status rev_path op r1 r2 =>
  term prefix err_status rev_path "BinNumOp" (r1 ^^ of_bin_num_op op ^^ r2);

let precedence_Ap = 1;

let precedence_Times = 2;

let precedence_Plus = 3;

let precedence_max = 4;

let rec precedence_dhexp d =>
  DHExp.(
    switch d {
    | Var _
    | NumLit _
    | Inj _ _ _
    | EmptyHole _ _ _
    | Cast _ _ _
    | FailedCast _ _ _ => precedence_const
    | Let _ _ _
    | Lam _ _ _
    | Case _ _ _ => precedence_max
    | Ap _ _ => precedence_Ap
    | BinNumOp Times _ _ => precedence_Times
    | BinNumOp Plus _ _ => precedence_Plus
    | NonEmptyHole _ _ _ d1 => precedence_dhexp d1
    }
  );

let hole_label_s (u, i) => string_of_int (u + 1) ^ ":" ^ string_of_int (i + 1);

let hole_label_of inst => taggedText "holeName" (hole_label_s inst);

let dbg_SHOW_SIGMAS = false;

let rec of_dhexp' instance_click_fn parenthesize prefix err_status rev_path d => {
  let doc =
    DHExp.(
      switch d {
      | Var x => of_Var prefix err_status rev_path x
      | Let x d1 d2 =>
        let rev_path1 = [0, ...rev_path];
        let rev_path2 = [1, ...rev_path];
        let r1 =
          of_dhexp' instance_click_fn false prefix NotInHole rev_path1 d1;
        let r2 =
          of_dhexp' instance_click_fn false prefix NotInHole rev_path2 d2;
        of_Let prefix err_status rev_path x r1 r2
      | Lam x ty d1 =>
        let rev_path1 = [0, ...rev_path];
        let rev_path2 = [1, ...rev_path];
        let r1 = of_htype false prefix rev_path1 ty;
        let r2 =
          of_dhexp' instance_click_fn false prefix NotInHole rev_path2 d1;
        of_LamAnn prefix err_status rev_path x r1 r2
      | Ap d1 d2 =>
        let rev_path1 = [0, ...rev_path];
        let rev_path2 = [1, ...rev_path];
        let paren1 = precedence_dhexp d1 > precedence_Ap;
        let paren2 = precedence_dhexp d2 >= precedence_Ap;
        let r1 =
          of_dhexp' instance_click_fn paren1 prefix NotInHole rev_path1 d1;
        let r2 =
          of_dhexp' instance_click_fn paren2 prefix NotInHole rev_path2 d2;
        of_Ap prefix err_status rev_path r1 r2
      | NumLit n => of_NumLit prefix err_status rev_path n
      | BinNumOp op d1 d2 =>
        let rev_path1 = [0, ...rev_path];
        let rev_path2 = [1, ...rev_path];
        let prec_d = precedence_dhexp d;
        let paren1 = precedence_dhexp d1 > prec_d;
        let paren2 = precedence_dhexp d2 >= prec_d;
        let r1 =
          of_dhexp' instance_click_fn paren1 prefix NotInHole rev_path1 d1;
        let r2 =
          of_dhexp' instance_click_fn paren2 prefix NotInHole rev_path2 d2;
        of_BinNumOp prefix err_status rev_path op r1 r2
      | Inj ty side d1 =>
        let rev_path1 = [0, ...rev_path];
        let rev_path2 = [1, ...rev_path];
        let r1 = of_htype false prefix rev_path1 ty;
        let r2 =
          of_dhexp' instance_click_fn false prefix NotInHole rev_path2 d1;
        of_InjAnn prefix err_status rev_path r1 side r2
      | Case d1 (x, d2) (y, d3) =>
        let rev_path1 = [0, ...rev_path];
        let rev_path2 = [1, ...rev_path];
        let rev_path3 = [2, ...rev_path];
        let r1 =
          of_dhexp' instance_click_fn false prefix NotInHole rev_path1 d1;
        let r2 =
          of_dhexp' instance_click_fn false prefix NotInHole rev_path2 d2;
        let r3 =
          of_dhexp' instance_click_fn false prefix NotInHole rev_path3 d3;
        of_CaseAnn prefix err_status rev_path r1 x r2 y r3
      | EmptyHole u i sigma =>
        let hole_label = hole_label_of (u, i);
        let r =
          dbg_SHOW_SIGMAS ?
            hole_label ^^ of_sigma instance_click_fn prefix rev_path sigma :
            hole_label;
        let attrs = [
          Tyxml_js.Html5.a_onclick (
            fun _ => {
              instance_click_fn (u, i);
              true
            }
          )
        ];
        term_with_attrs
          prefix err_status rev_path ["EmptyHole", "hole-instance"] attrs r
      | NonEmptyHole u i sigma d1 =>
        let rev_path1 = [0, ...rev_path];
        let r1 =
          of_dhexp' instance_click_fn false prefix (InHole u) rev_path1 d1;
        let r =
          dbg_SHOW_SIGMAS ?
            r1 ^^ of_sigma instance_click_fn prefix rev_path sigma : r1;
        term prefix err_status rev_path "NonEmptyHole" r
      | Cast (Cast d1 ty1 ty2) ty3 ty4 when HTyp.eq ty2 ty3 =>
        let rev_path1 = [0, ...rev_path];
        let inner_rev_path1 = [0, ...rev_path1];
        let inner_rev_path2 = [1, ...rev_path1];
        let inner_rev_path3 = [2, ...rev_path1];
        /* no rev_path2 because we're not showing them separately */
        let rev_path3 = [2, ...rev_path];
        let paren1 = precedence_dhexp d1 > precedence_const;
        let r1 =
          of_dhexp'
            instance_click_fn paren1 prefix NotInHole inner_rev_path1 d1;
        let r2 = of_htype false prefix inner_rev_path2 ty1;
        let r3 = of_htype false prefix inner_rev_path3 ty2;
        let r5 = of_htype false prefix rev_path3 ty4;
        of_chained_Cast prefix err_status rev_path r1 r2 r3 r5
      | Cast d1 ty1 ty2 =>
        let rev_path1 = [0, ...rev_path];
        let rev_path2 = [1, ...rev_path];
        let rev_path3 = [2, ...rev_path];
        let paren1 = precedence_dhexp d1 > precedence_const;
        let r1 =
          of_dhexp' instance_click_fn paren1 prefix NotInHole rev_path1 d1;
        let r2 = of_htype false prefix rev_path2 ty1;
        let r3 = of_htype false prefix rev_path3 ty2;
        of_Cast prefix err_status rev_path r1 r2 r3
      | FailedCast (Cast d1 ty1 ty2) ty3 ty4 when HTyp.eq ty2 ty3 =>
        let rev_path1 = [0, ...rev_path];
        let inner_rev_path1 = [0, ...rev_path1];
        let inner_rev_path2 = [1, ...rev_path1];
        let inner_rev_path3 = [2, ...rev_path1];
        /* no rev_path2 because we're not showing them separately */
        let rev_path3 = [2, ...rev_path];
        let paren1 = precedence_dhexp d1 > precedence_const;
        let r1 =
          of_dhexp'
            instance_click_fn paren1 prefix NotInHole inner_rev_path1 d1;
        let r2 = of_htype false prefix inner_rev_path2 ty1;
        let r3 = of_htype false prefix inner_rev_path3 ty2;
        let r5 = of_htype false prefix rev_path3 ty4;
        of_chained_FailedCast prefix err_status rev_path r1 r2 r3 r5
      | FailedCast d1 ty1 ty2 =>
        let rev_path1 = [0, ...rev_path];
        let rev_path2 = [1, ...rev_path];
        let rev_path3 = [2, ...rev_path];
        let paren1 = precedence_dhexp d1 > precedence_const;
        let r1 =
          of_dhexp' instance_click_fn paren1 prefix NotInHole rev_path1 d1;
        let r2 = of_htype false prefix rev_path2 ty1;
        let r3 = of_htype false prefix rev_path3 ty2;
        of_FailedCast prefix err_status rev_path r1 r2 r3
      }
    );
  parenthesize ? lparen "(" ^^ doc ^^ rparen ")" : doc
}
and of_sigma instance_click_fn prefix rev_path sigma => {
  let map_f (x, d) =>
    of_dhexp' instance_click_fn false prefix NotInHole rev_path d ^^
    kw "/" ^^ PP.text "" x;
  let docs = List.map map_f sigma;
  let doc' =
    switch docs {
    | [] => PP.empty
    | [x, ...xs] =>
      let fold_f doc doc' => doc ^^ kw ", " ^^ doc';
      List.fold_left fold_f x xs
    };
  lparen "[" ^^ doc' ^^ rparen "]"
};

let of_dhexp instance_click_fn prefix d =>
  of_dhexp' instance_click_fn false prefix NotInHole [] d;

/* Utilities */
let html_of_ty width prefix ty => {
  let ty_doc = of_htype false prefix [] ty;
  let (ty_sdoc, _) = Pretty.PP.sdoc_of_doc width ty_doc;
  Pretty.HTML_Of_SDoc.html_of_sdoc ty_sdoc
};

let html_of_dhexp instance_click_fn width prefix d => {
  let dhexp_doc = of_dhexp instance_click_fn prefix d;
  let (dhexp_sdoc, _) = Pretty.PP.sdoc_of_doc width dhexp_doc;
  Pretty.HTML_Of_SDoc.html_of_sdoc dhexp_sdoc
};

let html_of_var width prefix x =>
  html_of_dhexp (fun _ => ()) width prefix (DHExp.Var x);

let html_of_hole_instance instance_click_fn width prefix (u, i) => {
  let d = Dynamics.DHExp.EmptyHole u i [];
  html_of_dhexp instance_click_fn width prefix d
};

/* Debugging */
/* let of_hii (hii: Dynamics.DHExp.HoleInstanceInfo.t) (u: MetaVar.t) => {
     let doc0 = hole_label_of (u, (-1));
     let doc =
       switch (MetaVarMap.lookup hii u) {
       | Some instances =>
         List.fold_left
           (fun acc (env, _) => acc ^^ kw "; " ^^ of_sigma "dbg" [] env)
           doc0
           instances
       | None => doc0
       };
     let (sdoc, _) = Pretty.PP.sdoc_of_doc 80 doc;
     Pretty.HTML_Of_SDoc.html_of_sdoc sdoc
   }; */
let string_of_cursor_side cursor_side =>
  switch cursor_side {
  | On => "On"
  | Before => "Before"
  | After => "After"
  };
