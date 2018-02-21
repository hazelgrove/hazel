exception InvariantViolated;

/* Utility functions */
module PP = Pretty.PP;

open Semantics.Core;

let (^^) = PP.(^^);

let taggedText cls s => PP.text cls s;

let kw = taggedText "kw";

let parens = taggedText "paren";

let op = taggedText "op";

let var s => taggedText "var" s;

let space = taggedText "space" " ";

let rec id_of_rev_path rev_path =>
  switch rev_path {
  | [] => "path_"
  | [x, ...xs] => id_of_rev_path xs ^ "_" ^ string_of_int x
  };

let cls_from err_status cls =>
  switch err_status {
  | InHole u => [cls, "in_err_hole", "in_err_hole_" ^ string_of_int u]
  | NotInHole => [cls]
  };

let term err_status rev_path cls doc => {
  let id' = id_of_rev_path rev_path;
  PP.tagged (cls_from err_status cls) (Some (id', rev_path)) doc
};

let optionalBreakSp = PP.optionalBreak " ";

let optionalBreakNSp = PP.optionalBreak "";

let of_Parenthesized rev_path r1 =>
  term
    NotInHole
    rev_path
    "Parenthesized"
    (taggedText "openParens" "(" ^^ r1 ^^ taggedText "closeParens" ")");

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
    (
      taggedText "op-before-1" "\226\128\139\226\128\139" ^^
      taggedText "op-before-2" "\226\128\140" ^^
      taggedText "op-center" (" " ^ op_s ^ " ") ^^
      taggedText "op-after-1" "\226\128\139" ^^ taggedText "op-after-2" "\226\128\139"
    )
};

let of_expr_op_with_space = of_op_with_space str_of_expr_op;

let of_ty_op_with_space = of_op_with_space str_of_ty_op;

let rec of_op_no_space str_of_op op => {
  let (op_s, op_cls) = str_of_op op;
  PP.tagged ["seq-op", op_cls] None (taggedText "op-no-margin" op_s)
};

let of_expr_op_no_space = of_op_no_space str_of_expr_op;

let rec of_expr_op op =>
  switch op {
  | UHExp.Plus => of_expr_op_with_space op
  | UHExp.Times => of_expr_op_no_space op
  | UHExp.Space => of_expr_op_no_space op
  };

/* empty holes */
let of_Hole err_status rev_path cls hole_name =>
  term
    err_status
    rev_path
    cls
    (
      taggedText "hole-before-1" "\226\128\139\226\128\139" ^^
      taggedText "hole-before-2" "\226\128\139" ^^
      taggedText "holeName" hole_name ^^
      taggedText "hole-after-1" "\226\128\139" ^^ taggedText "hole-after-2" "\226\128\139"
    );

/* types */
let of_Num rev_path => term NotInHole rev_path "Num" (kw "num");

let rec of_ty_op cls op_s err_status rev_path r1 r2 =>
  term err_status rev_path cls (r1 ^^ optionalBreakSp ^^ op op_s ^^ optionalBreakSp ^^ r2);

let of_Arrow = of_ty_op "Arrow" "\226\134\146";

let of_Sum = of_ty_op "Sum" "|";

let rec of_htype rev_path ty =>
  switch ty {
  | HTyp.Num => of_Num rev_path
  | HTyp.Arrow ty1 ty2 =>
    let rev_path1 = [0, ...rev_path];
    let rev_path2 = [1, ...rev_path];
    let r1 = of_htype rev_path1 ty1;
    let r2 = of_htype rev_path2 ty2;
    of_Arrow NotInHole rev_path r1 r2
  | HTyp.Sum ty1 ty2 =>
    let rev_path1 = [0, ...rev_path];
    let rev_path2 = [1, ...rev_path];
    let r1 = of_htype rev_path1 ty1;
    let r2 = of_htype rev_path2 ty2;
    of_Sum NotInHole rev_path r1 r2
  | HTyp.Hole => of_Hole NotInHole rev_path "Hole" "?"
  };

let rec of_uhtyp rev_path uty =>
  switch uty {
  | UHTyp.Parenthesized uty1 =>
    let rev_path1 = [0, ...rev_path];
    let r1 = of_uhtyp rev_path1 uty1;
    of_Parenthesized rev_path r1
  | UHTyp.Num => of_Num rev_path
  | UHTyp.OpSeq skel seq => term NotInHole rev_path "OpSeq" (of_uhtyp_skel rev_path skel seq)
  | UHTyp.Hole => of_Hole NotInHole rev_path "Hole" "?"
  }
and of_uhtyp_skel rev_path skel seq =>
  switch skel {
  | Skel.Placeholder n =>
    switch (OperatorSeq.seq_nth n seq) {
    | Some utyn =>
      let rev_path_n = [n, ...rev_path];
      of_uhtyp rev_path_n utyn
    | None => raise InvariantViolated
    }
  | Skel.BinOp _ op skel1 skel2 =>
    let r1 = of_uhtyp_skel rev_path skel1 seq;
    let r2 = of_uhtyp_skel rev_path skel2 seq;
    let op_pp = of_ty_op_with_space op;
    PP.tagged ["skel-binop"] None (r1 ^^ op_pp ^^ r2)
  };

/* h-exps and z-exps */
let of_Asc err_status rev_path r1 r2 =>
  term err_status rev_path "Asc" (r1 ^^ space ^^ op ":" ^^ space ^^ r2);

let of_Var err_status rev_path x => term err_status rev_path "Var" (var x);

let of_Let err_status rev_path x r1 r2 =>
  term
    err_status
    rev_path
    "Let"
    (
      PP.blockBoundary ^^
      kw "let" ^^
      space ^^
      var x ^^
      space ^^ op "=" ^^ PP.nestAbsolute 2 (optionalBreakSp ^^ r1) ^^ PP.mandatoryBreak ^^ r2
    );

let of_Lam err_status rev_path x r =>
  term err_status rev_path "Lam" (kw "\206\187" ^^ var x ^^ taggedText "lambda-dot" "." ^^ r);

let of_LamAnn err_status rev_path x rty r1 =>
  term
    err_status
    rev_path
    "LamAnn"
    (
      kw "\206\187" ^^
      var x ^^ kw ":" ^^ rty ^^ kw "." ^^ PP.optionalBreak "" ^^ PP.nestRelative 4 r1
    );

let of_Ap err_status rev_path r1 r2 =>
  term err_status rev_path "Ap" (r1 ^^ parens "(" ^^ r2 ^^ parens ")");

let of_NumLit err_status rev_path n =>
  term err_status rev_path "NumLit" (taggedText "number" (string_of_int n));

let of_Plus err_status rev_path r1 r2 =>
  term err_status rev_path "Plus" (r1 ^^ optionalBreakNSp ^^ op " +" ^^ optionalBreakSp ^^ r2);

let of_Times err_status rev_path r1 r2 =>
  term err_status rev_path "Times" (r1 ^^ op "*" ^^ optionalBreakNSp ^^ r2);

let of_Space err_status rev_path r1 r2 =>
  term err_status rev_path "Space" (r1 ^^ op " " ^^ optionalBreakNSp ^^ r2);

let string_of_side side =>
  switch side {
  | UHExp.L => "L"
  | UHExp.R => "R"
  };

let of_Inj err_status rev_path side r =>
  term
    err_status
    rev_path
    "Inj"
    (
      kw "inj" ^^
      parens "[" ^^
      kw (string_of_side side) ^^
      parens "]" ^^ parens "(" ^^ PP.optionalBreak "" ^^ r ^^ parens ")"
    );

let of_InjAnn err_status rev_path rty side r =>
  term
    err_status
    rev_path
    "Inj"
    (
      kw "inj" ^^
      parens "[" ^^
      kw (string_of_side side) ^^
      kw "," ^^
      optionalBreakSp ^^ rty ^^ parens "]" ^^ parens "(" ^^ PP.optionalBreak "" ^^ r ^^ parens ")"
    );

let of_Case err_status rev_path r1 x r2 y r3 =>
  term
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
      parens "(" ^^
      var x ^^
      parens ")" ^^
      space ^^
      op "\226\135\146" ^^
      optionalBreakSp ^^
      PP.nestAbsolute 2 r2 ^^
      PP.mandatoryBreak ^^
      kw "R" ^^
      parens "(" ^^
      var y ^^ parens ")" ^^ space ^^ op "\226\135\146" ^^ optionalBreakSp ^^ PP.nestAbsolute 2 r3
    );

let of_CaseAnn err_status rev_path r1 x r2 y r3 =>
  term
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
      parens "(" ^^
      var x ^^
      parens ")" ^^
      space ^^
      op "\226\135\146" ^^
      optionalBreakSp ^^
      PP.nestAbsolute 2 r2 ^^
      PP.mandatoryBreak ^^
      kw "R" ^^
      parens "(" ^^
      var y ^^ parens ")" ^^ space ^^ op "\226\135\146" ^^ optionalBreakSp ^^ PP.nestAbsolute 2 r3
    );

let cast_arrow = op " \226\135\168 ";

let of_Cast err_status rev_path r1 rty1 rty2 =>
  term err_status rev_path "Cast" (r1 ^^ parens "<" ^^ rty1 ^^ cast_arrow ^^ rty2 ^^ parens ">");

let of_chained_Cast err_status rev_path r1 rty1 rty2 rty4 =>
  term
    err_status
    rev_path
    "Cast"
    (r1 ^^ parens "<" ^^ rty1 ^^ cast_arrow ^^ rty2 ^^ cast_arrow ^^ rty4 ^^ parens ">");

let failed_cast_arrow = taggedText "failed-cast-arrow" " \226\135\168 ";

let of_FailedCast err_status rev_path r1 rty1 rty2 =>
  term
    err_status
    rev_path
    "FailedCast"
    (r1 ^^ parens "<" ^^ rty1 ^^ failed_cast_arrow ^^ rty2 ^^ parens ">");

let of_chained_FailedCast err_status rev_path r1 rty1 rty2 rty4 =>
  term
    err_status
    rev_path
    "FailedCast"
    (
      r1 ^^
      parens "<" ^^ rty1 ^^ failed_cast_arrow ^^ rty2 ^^ failed_cast_arrow ^^ rty4 ^^ parens ">"
    );

let rec of_hexp rev_path e =>
  switch e {
  | UHExp.Parenthesized e1 =>
    let rev_path1 = [0, ...rev_path];
    let r1 = of_hexp rev_path1 e1;
    of_Parenthesized rev_path r1
  | UHExp.Tm err_status e' =>
    switch e' {
    | UHExp.Asc e1 ty =>
      let rev_path1 = [0, ...rev_path];
      let rev_path2 = [1, ...rev_path];
      let r1 = of_hexp rev_path1 e1;
      let r2 = of_uhtyp rev_path2 ty;
      of_Asc err_status rev_path r1 r2
    | UHExp.Var x => of_Var err_status rev_path x
    | UHExp.Let x e e' =>
      let rev_path1 = [0, ...rev_path];
      let rev_path2 = [1, ...rev_path];
      let r1 = of_hexp rev_path1 e;
      let r2 = of_hexp rev_path2 e';
      of_Let err_status rev_path x r1 r2
    | UHExp.Lam x e' =>
      let rev_path1 = [0, ...rev_path];
      let r1 = of_hexp rev_path1 e';
      of_Lam err_status rev_path x r1
    | UHExp.NumLit n => of_NumLit err_status rev_path n
    | UHExp.Inj side e =>
      let rev_path1 = [0, ...rev_path];
      let r1 = of_hexp rev_path1 e;
      of_Inj err_status rev_path side r1
    | UHExp.Case e1 (x, e2) (y, e3) =>
      let rev_path1 = [0, ...rev_path];
      let rev_path2 = [1, ...rev_path];
      let rev_path3 = [2, ...rev_path];
      let r1 = of_hexp rev_path1 e1;
      let r2 = of_hexp rev_path2 e2;
      let r3 = of_hexp rev_path3 e3;
      of_Case err_status rev_path r1 x r2 y r3
    | UHExp.EmptyHole u => of_Hole err_status rev_path "EmptyHole" (string_of_int u)
    | UHExp.OpSeq skel seq => term err_status rev_path "OpSeq" (of_skel rev_path skel seq)
    }
  }
and of_skel rev_path skel seq =>
  switch skel {
  | Skel.Placeholder n =>
    switch (OperatorSeq.seq_nth n seq) {
    | Some en =>
      let rev_path_n = [n, ...rev_path];
      of_hexp rev_path_n en
    | None => raise InvariantViolated
    }
  | Skel.BinOp err_status op skel1 skel2 =>
    let r1 = of_skel rev_path skel1 seq;
    let r2 = of_skel rev_path skel2 seq;
    let op_pp =
      switch (skel1, op, skel2) {
      | (Skel.BinOp _ UHExp.Space _ _, UHExp.Times, _)
      | (_, UHExp.Times, Skel.BinOp _ UHExp.Space _ _) => of_expr_op_with_space op
      | _ => of_expr_op op
      };
    let cls = "skel-binop";
    let cls' = cls_from err_status cls;
    PP.tagged cls' None (r1 ^^ op_pp ^^ r2)
  };

let rec of_bin_num_op op =>
  switch op {
  | Dynamics.DHExp.Plus => taggedText "bin_num_op" " + "
  | Dynamics.DHExp.Times => taggedText "bin_num_op" "*"
  };

let of_BinNumOp err_status rev_path op r1 r2 =>
  term err_status rev_path "BinNumOp" (parens "(" ^^ r1 ^^ of_bin_num_op op ^^ r2 ^^ parens ")");

let rec of_dhexp err_status rev_path d =>
  Dynamics.DHExp.(
    switch d {
    | Var x => of_Var err_status rev_path x
    | Let x d1 d2 =>
      let rev_path1 = [0, ...rev_path];
      let rev_path2 = [1, ...rev_path];
      let r1 = of_dhexp NotInHole rev_path1 d1;
      let r2 = of_dhexp NotInHole rev_path2 d2;
      of_Let err_status rev_path x r1 r2
    | Lam x ty d1 =>
      let rev_path1 = [0, ...rev_path];
      let rev_path2 = [1, ...rev_path];
      let r1 = of_htype rev_path1 ty;
      let r2 = of_dhexp NotInHole rev_path2 d1;
      of_LamAnn err_status rev_path x r1 r2
    | Ap d1 d2 =>
      let rev_path1 = [0, ...rev_path];
      let rev_path2 = [1, ...rev_path];
      let r1 = of_dhexp NotInHole rev_path1 d1;
      let r2 = of_dhexp NotInHole rev_path2 d2;
      of_Ap err_status rev_path r1 r2
    | NumLit n => of_NumLit err_status rev_path n
    | BinNumOp op d1 d2 =>
      let rev_path1 = [0, ...rev_path];
      let rev_path2 = [1, ...rev_path];
      let r1 = of_dhexp NotInHole rev_path1 d1;
      let r2 = of_dhexp NotInHole rev_path2 d2;
      of_BinNumOp err_status rev_path op r1 r2
    | Inj ty side d1 =>
      let rev_path1 = [0, ...rev_path];
      let rev_path2 = [1, ...rev_path];
      let r1 = of_htype rev_path1 ty;
      let r2 = of_dhexp NotInHole rev_path2 d1;
      of_InjAnn err_status rev_path r1 side r2
    | Case d1 (x, d2) (y, d3) =>
      let rev_path1 = [0, ...rev_path];
      let rev_path2 = [1, ...rev_path];
      let rev_path3 = [2, ...rev_path];
      let r1 = of_dhexp NotInHole rev_path1 d1;
      let r2 = of_dhexp NotInHole rev_path2 d2;
      let r3 = of_dhexp NotInHole rev_path3 d3;
      of_CaseAnn err_status rev_path r1 x r2 y r3
    | EmptyHole u sigma =>
      term
        err_status
        rev_path
        "EmptyHole"
        (
          taggedText "holeName" (string_of_int u) ^^
          PP.tagged
            ["hole-decorations"] None (PP.tagged ["environment"] None (of_sigma rev_path sigma))
        )
    | NonEmptyHole u sigma d1 =>
      let rev_path1 = [0, ...rev_path];
      term
        err_status
        rev_path
        "NonEmptyHole"
        (
          of_dhexp (InHole u) rev_path1 d1 ^^
          PP.tagged
            ["hole-decorations"] None (PP.tagged ["environment"] None (of_sigma rev_path sigma))
        )
    | Cast (Cast d1 ty1 ty2) ty3 ty4 when HTyp.eq ty2 ty3 =>
      let rev_path1 = [0, ...rev_path];
      let inner_rev_path1 = [0, ...rev_path1];
      let inner_rev_path2 = [1, ...rev_path1];
      let inner_rev_path3 = [2, ...rev_path1];
      /* no rev_path2 because we're not showing them separately */
      let rev_path3 = [2, ...rev_path];
      let r1 = of_dhexp NotInHole inner_rev_path1 d1;
      let r2 = of_htype inner_rev_path2 ty1;
      let r3 = of_htype inner_rev_path3 ty2;
      let r5 = of_htype rev_path3 ty4;
      of_chained_Cast err_status rev_path r1 r2 r3 r5
    | Cast d1 ty1 ty2 =>
      let rev_path1 = [0, ...rev_path];
      let rev_path2 = [1, ...rev_path];
      let rev_path3 = [2, ...rev_path];
      let r1 = of_dhexp NotInHole rev_path1 d1;
      let r2 = of_htype rev_path2 ty1;
      let r3 = of_htype rev_path3 ty2;
      of_Cast err_status rev_path r1 r2 r3
    | FailedCast (Cast d1 ty1 ty2) ty3 ty4 when HTyp.eq ty2 ty3 =>
      let rev_path1 = [0, ...rev_path];
      let inner_rev_path1 = [0, ...rev_path1];
      let inner_rev_path2 = [1, ...rev_path1];
      let inner_rev_path3 = [2, ...rev_path1];
      /* no rev_path2 because we're not showing them separately */
      let rev_path3 = [2, ...rev_path];
      let r1 = of_dhexp NotInHole inner_rev_path1 d1;
      let r2 = of_htype inner_rev_path2 ty1;
      let r3 = of_htype inner_rev_path3 ty2;
      let r5 = of_htype rev_path3 ty4;
      of_chained_FailedCast err_status rev_path r1 r2 r3 r5
    | FailedCast d1 ty1 ty2 =>
      let rev_path1 = [0, ...rev_path];
      let rev_path2 = [1, ...rev_path];
      let rev_path3 = [2, ...rev_path];
      let r1 = of_dhexp NotInHole rev_path1 d1;
      let r2 = of_htype rev_path2 ty1;
      let r3 = of_htype rev_path3 ty2;
      of_FailedCast err_status rev_path r1 r2 r3
    }
  )
and of_sigma rev_path sigma => {
  let map_f (x, d) => of_dhexp NotInHole rev_path d ^^ kw "/" ^^ PP.text "" x;
  let docs = List.map map_f sigma;
  let doc' =
    switch docs {
    | [] => PP.empty
    | [x, ...xs] =>
      let fold_f doc doc' => doc ^^ kw ", " ^^ doc';
      List.fold_left fold_f x xs
    };
  parens "[" ^^ doc' ^^ parens "]"
};
