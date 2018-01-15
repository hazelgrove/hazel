module PPView = {
  module PP = Pretty.PP;
  open Semantics.Core;
  let (^^) = PP.(^^);
  /* Utility functions */
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
  let term cls rev_path doc => {
    let id' = id_of_rev_path rev_path;
    PP.tagged cls (Some (id', rev_path)) doc
  };
  let optionalBreakSp = PP.optionalBreak " ";
  let optionalBreakNSp = PP.optionalBreak "";
  /* # types */
  let rec of_Arrow rev_path r1 r2 =>
    term "Arrow" rev_path (r1 ^^ optionalBreakSp ^^ op "\226\134\146" ^^ optionalBreakSp ^^ r2);
  let rec of_Sum rev_path r1 r2 =>
    term "Sum" rev_path (r1 ^^ optionalBreakSp ^^ op "+" ^^ optionalBreakSp ^^ r2);
  /* ## parenthesization helpers */
  let num_precedence = 0;
  let hole_precedence = 0;
  let sum_precedence = 1;
  let arrow_precedence = 2;
  let h_precedence tau =>
    switch tau {
    | HTyp.Num => num_precedence
    | HTyp.Hole => hole_precedence
    | HTyp.Sum _ => sum_precedence
    | HTyp.Arrow _ => arrow_precedence
    };
  /* parenthesize binary right-associative type operators */
  let parenthesize_bi_r_tau prec prec1 prec2 => {
    let paren1 = prec1 !== prec && prec1 !== 0;
    let paren2 = prec2 !== prec && prec2 !== 0;
    (paren1, paren2)
  };
  let rec of_htype' rev_path tau paren =>
    if paren {
      parens "(" ^^ PP.nestRelative 0 (of_htype rev_path tau) ^^ parens ")"
    } else {
      of_htype rev_path tau
    }
  and of_htype rev_path tau =>
    switch tau {
    | HTyp.Num => term "Num" rev_path (kw "num")
    | HTyp.Arrow tau1 tau2 =>
      let (paren1, paren2) =
        parenthesize_bi_r_tau arrow_precedence (h_precedence tau1) (h_precedence tau2);
      let rev_path1 = [0, ...rev_path];
      let rev_path2 = [1, ...rev_path];
      let r1 = of_htype' rev_path1 tau1 paren1;
      let r2 = of_htype' rev_path2 tau2 paren2;
      of_Arrow rev_path r1 r2
    | HTyp.Sum tau1 tau2 =>
      let (paren1, paren2) =
        parenthesize_bi_r_tau sum_precedence (h_precedence tau1) (h_precedence tau2);
      let rev_path1 = [0, ...rev_path];
      let rev_path2 = [1, ...rev_path];
      let r1 = of_htype' rev_path1 tau1 paren1;
      let r2 = of_htype' rev_path2 tau2 paren2;
      of_Sum rev_path r1 r2
    | HTyp.Hole =>
      term
        "Hole"
        rev_path
        (
          taggedText "hole-before-1" "\226\128\139\226\128\139" ^^
          taggedText "hole-before-2" "\226\128\140" ^^
          taggedText "holeName" "?" ^^
          taggedText "hole-after-1" "\226\128\140" ^^ taggedText "hole-after-2" "\226\128\140"
        )
    };
  /* h-exps and z-exps */
  let string_of_side side =>
    switch side {
    | AHExp.L => "L"
    | AHExp.R => "R"
    };
  let of_Asc rev_path r1 r2 => term "Asc" rev_path (r1 ^^ space ^^ op ":" ^^ space ^^ r2);
  let of_Let rev_path x r1 r2 =>
    term
      "Let"
      rev_path
      (
        PP.blockBoundary ^^
        kw "let" ^^
        space ^^
        var x ^^
        space ^^ op "=" ^^ PP.nestAbsolute 2 (optionalBreakSp ^^ r1) ^^ PP.mandatoryBreak ^^ r2
      );
  let of_Lam rev_path x r =>
    term
      "Lam"
      rev_path
      (kw "\206\187" ^^ var x ^^ kw "." ^^ PP.optionalBreak "" ^^ PP.nestRelative 4 r);
  let of_LamAnn rev_path x rty r1 =>
    term
      "LamAnn"
      rev_path
      (
        kw "\206\187" ^^
        var x ^^ kw ":" ^^ rty ^^ kw "." ^^ PP.optionalBreak "" ^^ PP.nestRelative 4 r1
      );
  let of_Ap rev_path r1 r2 => term "Ap" rev_path (r1 ^^ parens "(" ^^ r2 ^^ parens ")");
  let of_Plus rev_path r1 r2 =>
    term "Plus" rev_path (r1 ^^ optionalBreakNSp ^^ op " +" ^^ optionalBreakSp ^^ r2);
  let of_Times rev_path r1 r2 => term "Times" rev_path (r1 ^^ op "*" ^^ optionalBreakNSp ^^ r2);
  let of_Space rev_path r1 r2 => term "Space" rev_path (r1 ^^ op " " ^^ optionalBreakNSp ^^ r2);
  let of_Inj rev_path side r =>
    term
      "Inj"
      rev_path
      (
        kw "inj" ^^
        parens "[" ^^
        kw (string_of_side side) ^^
        parens "]" ^^ parens "(" ^^ PP.optionalBreak "" ^^ r ^^ parens ")"
      );
  let of_InjAnn rev_path rty side r =>
    term
      "Inj"
      rev_path
      (
        kw "inj" ^^
        parens "[" ^^
        kw (string_of_side side) ^^
        kw "," ^^
        optionalBreakSp ^^
        rty ^^ parens "]" ^^ parens "(" ^^ PP.optionalBreak "" ^^ r ^^ parens ")"
      );
  let of_Case rev_path r1 x r2 y r3 =>
    term
      "Case"
      rev_path
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
        var y ^^
        parens ")" ^^ space ^^ op "\226\135\146" ^^ optionalBreakSp ^^ PP.nestAbsolute 2 r3
      );
  let of_CaseAnn rev_path rty r1 x r2 y r3 =>
    term
      "Case"
      rev_path
      (
        PP.blockBoundary ^^
        kw "case" ^^
        parens "[" ^^
        rty ^^
        parens "]" ^^
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
        var y ^^
        parens ")" ^^ space ^^ op "\226\135\146" ^^ optionalBreakSp ^^ PP.nestAbsolute 2 r3
      );
  let of_NonEmptyHole rev_path u r =>
    term
      "NonEmptyHole"
      rev_path
      (
        PP.tagged
          "NonEmptyHoleTerm"
          None
          (r ^^ taggedText "nonEmptyHole-after-inner" "\226\128\139\226\128\139") ^^
        taggedText "nonEmptyHoleName" (string_of_int u) ^^
        taggedText "nonEmptyHole-after-outer" "\226\128\139\226\128\139\226\128\139"
      );
  let of_Cast rev_path rty r1 =>
    term
      "Cast"
      rev_path
      (parens "<" ^^ rty ^^ parens ">(" ^^ PP.optionalBreak "" ^^ r1 ^^ parens ")");
  let asc_precedence = 1;
  let var_precedence = 0;
  let let_precedence = 1;
  let lam_precedence = 1;
  let ap_precedence = 3;
  let numlit_precedence = 0;
  let plus_precedence = 2;
  let times_precedence = 1;
  let inj_precedence = 0;
  let case_precedence = 1;
  let empty_hole_precedence = 0;
  let non_empty_hole_precedence = 0;
  let cast_precedence = 1;
  let hexp_precedence e =>
    UHExp.(
      switch e {
      | Asc _ => asc_precedence
      | Var _ => var_precedence
      | Let _ => let_precedence
      | Lam _ => lam_precedence
      | Ap _ => ap_precedence
      | NumLit _ => numlit_precedence
      | Inj _ => inj_precedence
      | Case _ => case_precedence
      | EmptyHole _ => empty_hole_precedence
      | NonEmptyHole _ _ => non_empty_hole_precedence
      | OpSeq _ => 0 /* TODO need to redo the parenthesization logic anyway */
      }
    );
  let dhexp_precedence e =>
    Dynamics.DHExp.(
      switch e {
      | Var _ => var_precedence
      | Let _ _ _ => let_precedence
      | Lam _ _ _ => lam_precedence
      | Ap _ _ => ap_precedence
      | NumLit _ => numlit_precedence
      | BinOp AHExp.Plus _ _ => plus_precedence
      | BinOp AHExp.Times _ _ => times_precedence
      | BinOp AHExp.Space _ _ => times_precedence
      | Inj _ _ _ => inj_precedence
      | Case _ _ _ _ => case_precedence
      | EmptyHole _ _ _ => empty_hole_precedence
      | NonEmptyHole _ _ _ _ => non_empty_hole_precedence
      | Cast _ _ => cast_precedence
      }
    );
  let rec of_op op =>
    switch op {
    | AHExp.Plus =>
      PP.tagged
        "seq-op"
        None
        (
          taggedText "op-before-1" "\226\128\139\226\128\139" ^^
          taggedText "op-before-2" "\226\128\140" ^^
          taggedText "op-center" " + " ^^
          taggedText "op-after-1" "\226\128\139" ^^ taggedText "op-after-2" "\226\128\139"
        )
    | AHExp.Times => taggedText "seq-op" "*"
    | AHExp.Space => taggedText "seq-op" " "
    };
  let rec of_hexp' rev_path e paren =>
    if paren {
      parens "(" ^^ PP.nestRelative 0 (of_hexp rev_path e) ^^ parens ")"
    } else {
      of_hexp rev_path e
    }
  and of_hexp rev_path e =>
    switch e {
    | UHExp.Asc e' tau =>
      let prec1 = hexp_precedence e';
      let paren1 = prec1 !== 0;
      let rev_path1 = [0, ...rev_path];
      let rev_path2 = [1, ...rev_path];
      let r1 = of_hexp' rev_path1 e' paren1;
      let r2 = of_htype rev_path2 tau;
      of_Asc rev_path r1 r2
    | UHExp.Var x => term "Var" rev_path (var x)
    | UHExp.Let x e e' =>
      let rev_path1 = [0, ...rev_path];
      let rev_path2 = [1, ...rev_path];
      let r1 = of_hexp rev_path1 e;
      let r2 = of_hexp rev_path2 e';
      of_Let rev_path x r1 r2
    | UHExp.Lam x e' =>
      let rev_path1 = [0, ...rev_path];
      let r1 = of_hexp rev_path1 e';
      of_Lam rev_path x r1
    | UHExp.Ap e1 e2 =>
      let rev_path1 = [0, ...rev_path];
      let rev_path2 = [1, ...rev_path];
      let prec1 = hexp_precedence e1;
      let paren1 = prec1 !== 0 && prec1 < ap_precedence;
      let r1 = of_hexp' rev_path1 e1 paren1;
      let prec2 = hexp_precedence e2;
      let paren2 = prec2 !== 0 && prec2 <= ap_precedence;
      let r2 = of_hexp' rev_path2 e2 paren2;
      of_Ap rev_path r1 r2
    | UHExp.NumLit n => term "NumLit" rev_path (taggedText "number" (string_of_int n))
    | UHExp.Inj side e =>
      let rev_path1 = [0, ...rev_path];
      let r1 = of_hexp rev_path1 e;
      of_Inj rev_path side r1
    | UHExp.Case e1 (x, e2) (y, e3) =>
      let rev_path1 = [0, ...rev_path];
      let rev_path2 = [1, ...rev_path];
      let rev_path3 = [2, ...rev_path];
      let r1 = of_hexp rev_path1 e1;
      let r2 = of_hexp rev_path2 e2;
      let r3 = of_hexp rev_path3 e3;
      of_Case rev_path r1 x r2 y r3
    | UHExp.EmptyHole u =>
      term
        "EmptyHole"
        rev_path
        (
          taggedText "hole-before-1" "\226\128\139\226\128\139" ^^
          taggedText "hole-before-2" "\226\128\140" ^^
          /* taggedText "hole" "_" */
          taggedText "holeName" (string_of_int u) ^^
          taggedText "hole-after-1" "\226\128\140" ^^ taggedText "hole-after-2" "\226\128\140"
        )
    | UHExp.NonEmptyHole u e =>
      let rev_path1 = [0, ...rev_path];
      let r1 = of_hexp rev_path1 e;
      of_NonEmptyHole rev_path u r1
    | UHExp.OpSeq seq => term "OpSeq" rev_path (of_opseq rev_path seq)
    }
  and of_opseq' rev_path seq =>
    switch seq {
    | UHExp.BareExp e =>
      let rev_path' = [0, ...rev_path];
      (of_hexp rev_path' e, 1)
    | UHExp.SeqOpExp seq' op e =>
      let (seq_view, pos) = of_opseq' rev_path seq';
      let rev_path' = [pos, ...rev_path];
      (seq_view ^^ of_op op ^^ of_hexp rev_path' e, pos + 1)
    }
  and of_opseq rev_path seq => {
    let (seq_view, _) = of_opseq' rev_path seq;
    seq_view
  };
  let rec of_dhexp' rev_path d paren =>
    if paren {
      parens "(" ^^ PP.nestRelative 0 (of_dhexp rev_path d) ^^ parens ")"
    } else {
      of_dhexp rev_path d
    }
  and of_dhexp rev_path d =>
    Dynamics.(
      DHExp.(
        switch d {
        | Var x => term "Var" rev_path (var x)
        | Let x d1 d2 =>
          let rev_path1 = [0, ...rev_path];
          let rev_path2 = [1, ...rev_path];
          let r1 = of_dhexp rev_path1 d1;
          let r2 = of_dhexp rev_path2 d2;
          of_Let rev_path x r1 r2
        | Lam x ty d1 =>
          let rev_path1 = [0, ...rev_path];
          let rev_path2 = [1, ...rev_path];
          let r1 = of_htype rev_path1 ty;
          let r2 = of_dhexp rev_path2 d1;
          of_LamAnn rev_path x r1 r2
        | Ap d1 d2 =>
          let rev_path1 = [0, ...rev_path];
          let rev_path2 = [1, ...rev_path];
          let prec1 = dhexp_precedence d1;
          let paren1 = prec1 !== 0 && prec1 < ap_precedence;
          let r1 = of_dhexp' rev_path1 d1 paren1;
          let prec2 = dhexp_precedence d2;
          let paren2 = prec2 !== 0 && prec2 <= ap_precedence;
          let r2 = of_dhexp' rev_path2 d2 paren2;
          of_Ap rev_path r1 r2
        | NumLit n => term "NumLit" rev_path (taggedText "number" (string_of_int n))
        | BinOp AHExp.Plus d1 d2 =>
          let rev_path1 = [0, ...rev_path];
          let rev_path2 = [1, ...rev_path];
          let prec1 = dhexp_precedence d1;
          let paren1 = prec1 > 0 && prec1 !== plus_precedence;
          let r1 = of_dhexp' rev_path1 d1 paren1;
          let prec2 = dhexp_precedence d2;
          let paren2 = prec2 > 0 && prec2 !== plus_precedence;
          let r2 = of_dhexp' rev_path2 d2 paren2;
          of_Plus rev_path r1 r2
        | BinOp AHExp.Times d1 d2 =>
          let rev_path1 = [0, ...rev_path];
          let rev_path2 = [1, ...rev_path];
          let prec1 = dhexp_precedence d1;
          let paren1 = prec1 > 0 && prec1 !== times_precedence;
          let r1 = of_dhexp' rev_path1 d1 paren1;
          let prec2 = dhexp_precedence d2;
          let paren2 = prec2 > 0 && prec2 !== times_precedence;
          let r2 = of_dhexp' rev_path2 d2 paren2;
          of_Times rev_path r1 r2
        | BinOp AHExp.Space d1 d2 =>
          let rev_path1 = [0, ...rev_path];
          let rev_path2 = [1, ...rev_path];
          let prec1 = dhexp_precedence d1;
          let paren1 = prec1 > 0 && prec1 !== times_precedence;
          let r1 = of_dhexp' rev_path1 d1 paren1;
          let prec2 = dhexp_precedence d2;
          let paren2 = prec2 > 0 && prec2 !== times_precedence;
          let r2 = of_dhexp' rev_path2 d2 paren2;
          of_Space rev_path r1 r2
        | Inj ty side d1 =>
          let rev_path1 = [0, ...rev_path];
          let rev_path2 = [1, ...rev_path];
          let r1 = of_htype rev_path1 ty;
          let r2 = of_dhexp rev_path2 d1;
          of_InjAnn rev_path r1 side r2
        | Case ty d1 (x, d2) (y, d3) =>
          let rev_path1 = [0, ...rev_path];
          let rev_path2 = [1, ...rev_path];
          let rev_path3 = [2, ...rev_path];
          let rev_path4 = [3, ...rev_path];
          let r1 = of_htype rev_path1 ty;
          let r2 = of_dhexp rev_path2 d1;
          let r3 = of_dhexp rev_path3 d2;
          let r4 = of_dhexp rev_path4 d3;
          of_CaseAnn rev_path r1 r2 x r3 y r4
        | EmptyHole u m sigma =>
          term
            "EmptyHole"
            rev_path
            (
              taggedText "holeName" (string_of_int u) ^^
              PP.tagged
                "hole-decorations" None (PP.tagged "environment" None (of_sigma rev_path sigma))
            )
        | NonEmptyHole u m sigma d1 =>
          let rev_path1 = [0, ...rev_path];
          let r = of_dhexp rev_path1 d1;
          of_NonEmptyHole rev_path u r
        | Cast ty d1 =>
          let rev_path1 = [0, ...rev_path];
          let rev_path2 = [1, ...rev_path];
          let r1 = of_htype rev_path1 ty;
          let r2 = of_dhexp rev_path2 d1;
          of_Cast rev_path r1 r2
        }
      )
    )
  and of_sigma rev_path sigma => {
    let map_f (x, d) => of_dhexp rev_path d ^^ kw "/" ^^ PP.text "" x;
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
};
