module PPView = {
  module PP = Pretty.PP;
  open Semantics.Core;
  let (^^) = PP.(^^);
  /* Utility functions */
  let taggedText cls s => PP.tagged cls None (PP.text s);
  let kw = taggedText "kw";
  let parens = taggedText "paren";
  let op = taggedText "op";
  let var s => taggedText "var" s;
  let space = taggedText "space" " ";
  let rec id_of_path path =>
    switch path {
    | [] => "loc_"
    | [x, ...xs] => id_of_path xs ^ string_of_int x
    };
  let term cls path doc => {
    let id' = id_of_path path;
    PP.tagged cls (Some (id', path)) doc
  };
  let optionalBreakSp = PP.optionalBreak " ";
  /* # types */
  let rec of_Arrow path r1 r2 =>
    term "Arrow" path (r1 ^^ optionalBreakSp ^^ op "\226\134\146" ^^ optionalBreakSp ^^ r2);
  let rec of_Sum path r1 r2 =>
    term "Sum" path (r1 ^^ optionalBreakSp ^^ op "+" ^^ optionalBreakSp ^^ r2);
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
  let z_precedence ztau =>
    switch ztau {
    | ZTyp.CursorT tau => h_precedence tau
    | ZTyp.LeftArrow _
    | ZTyp.RightArrow _ => arrow_precedence
    | ZTyp.LeftSum _
    | ZTyp.RightSum _ => sum_precedence
    };
  /* parenthesize binary right-associative type operators */
  let parenthesize_bi_r_tau prec prec1 prec2 => {
    let paren1 = prec1 !== prec && prec1 !== 0;
    let paren2 = prec2 !== prec && prec2 !== 0;
    (paren1, paren2)
  };
  let rec of_htype' path tau paren =>
    if paren {
      parens "(" ^^ PP.nestRelative 0 (of_htype path tau) ^^ parens ")"
    } else {
      of_htype path tau
    }
  and of_htype path tau =>
    switch tau {
    | HTyp.Num => term "Num" path (kw "num")
    | HTyp.Arrow tau1 tau2 =>
      let (paren1, paren2) =
        parenthesize_bi_r_tau arrow_precedence (h_precedence tau1) (h_precedence tau2);
      let path1 = [0, ...path];
      let path2 = [1, ...path];
      let r1 = of_htype' path1 tau1 paren1;
      let r2 = of_htype' path2 tau2 paren2;
      of_Arrow path r1 r2
    | HTyp.Sum tau1 tau2 =>
      let (paren1, paren2) =
        parenthesize_bi_r_tau sum_precedence (h_precedence tau1) (h_precedence tau2);
      let path1 = [0, ...path];
      let path2 = [1, ...path];
      let r1 = of_htype' path1 tau1 paren1;
      let r2 = of_htype' path2 tau2 paren2;
      of_Sum path r1 r2
    | HTyp.Hole => term "Hole" path (taggedText "hole" "\226\144\163")
    };
  let rec of_ztype' path ztau paren =>
    switch ztau {
    | ZTyp.CursorT tau => PP.tagged "cursor" None (of_htype' path tau paren)
    | _ =>
      if paren {
        parens "(" ^^ PP.nestRelative 0 (of_ztype path ztau) ^^ parens ")"
      } else {
        of_ztype path ztau
      }
    }
  and of_ztype path ztau =>
    switch ztau {
    | ZTyp.CursorT tau => PP.tagged "cursor" None (of_htype path tau)
    | ZTyp.LeftArrow ztau tau =>
      let (paren1, paren2) =
        parenthesize_bi_r_tau arrow_precedence (z_precedence ztau) (h_precedence tau);
      let path1 = [0, ...path];
      let path2 = [1, ...path];
      let r1 = of_ztype' path1 ztau paren1;
      let r2 = of_htype' path2 tau paren2;
      of_Arrow path r1 r2
    | ZTyp.RightArrow tau ztau =>
      let (paren1, paren2) =
        parenthesize_bi_r_tau arrow_precedence (h_precedence tau) (z_precedence ztau);
      let path1 = [0, ...path];
      let path2 = [1, ...path];
      let r1 = of_htype' path1 tau paren1;
      let r2 = of_ztype' path2 ztau paren2;
      of_Arrow path r1 r2
    | ZTyp.LeftSum ztau tau =>
      let (paren1, paren2) =
        parenthesize_bi_r_tau sum_precedence (z_precedence ztau) (h_precedence tau);
      let path1 = [0, ...path];
      let path2 = [1, ...path];
      let r1 = of_ztype' path1 ztau paren1;
      let r2 = of_htype' path2 tau paren2;
      of_Sum path r1 r2
    | ZTyp.RightSum tau ztau =>
      let (paren1, paren2) =
        parenthesize_bi_r_tau sum_precedence (h_precedence tau) (z_precedence ztau);
      let path1 = [0, ...path];
      let path2 = [1, ...path];
      let r1 = of_htype' path1 tau paren1;
      let r2 = of_ztype' path2 ztau paren2;
      of_Sum path r1 r2
    };
  /* h-exps and z-exps */
  let string_of_side side =>
    switch side {
    | HExp.L => "L"
    | HExp.R => "R"
    };
  let of_Asc path r1 r2 => term "Asc" path (r1 ^^ space ^^ op ":" ^^ space ^^ r2);
  let of_Let path x r1 r2 =>
    term
      "Let"
      path
      (
        PP.blockBoundary ^^
        kw "let" ^^
        space ^^
        var x ^^
        space ^^ op "=" ^^ PP.nestAbsolute 2 (optionalBreakSp ^^ r1) ^^ PP.mandatoryBreak ^^ r2
      );
  let of_Lam path x r =>
    term
      "Lam" path (kw "\206\187" ^^ var x ^^ kw "." ^^ PP.optionalBreak "" ^^ PP.nestRelative 4 r);
  let of_LamAnn path x rty r1 =>
    term
      "LamAnn"
      path
      (
        kw "\206\187" ^^
        var x ^^ kw ":" ^^ rty ^^ kw "." ^^ PP.optionalBreak "" ^^ PP.nestRelative 4 r1
      );
  let of_Ap path r1 r2 => term "Ap" path (r1 ^^ optionalBreakSp ^^ r2);
  let of_Plus path r1 r2 =>
    term "Plus" path (r1 ^^ optionalBreakSp ^^ op "+" ^^ optionalBreakSp ^^ r2);
  let of_Inj path side r =>
    term
      "Inj"
      path
      (
        kw "inj" ^^
        parens "[" ^^
        kw (string_of_side side) ^^
        parens "]" ^^ parens "(" ^^ PP.optionalBreak "" ^^ r ^^ parens ")"
      );
  let of_InjAnn path rty side r =>
    term
      "Inj"
      path
      (
        kw "inj" ^^
        parens "[" ^^
        kw (string_of_side side) ^^
        kw "," ^^
        optionalBreakSp ^^
        rty ^^ parens "]" ^^ parens "(" ^^ PP.optionalBreak "" ^^ r ^^ parens ")"
      );
  let of_Case path r1 x r2 y r3 =>
    term
      "Case"
      path
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
  let of_CaseAnn path rty r1 x r2 y r3 =>
    term
      "Case"
      path
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
  let of_NonEmptyHole path u r =>
    term "NonEmptyHole" path r ^^ taggedText "holeName" (string_of_int u);
  let of_Cast path rty r1 =>
    term "Cast" path (parens "<" ^^ rty ^^ parens ">(" ^^ PP.optionalBreak "" ^^ r1 ^^ parens ")");
  let asc_precedence = 1;
  let var_precedence = 0;
  let let_precedence = 1;
  let lam_precedence = 1;
  let ap_precedence = 3;
  let numlit_precedence = 0;
  let plus_precedence = 2;
  let inj_precedence = 0;
  let case_precedence = 1;
  let empty_hole_precedence = 0;
  let non_empty_hole_precedence = 0;
  let cast_precedence = 1;
  let hexp_precedence e =>
    HExp.(
      switch e {
      | Asc _ => asc_precedence
      | Var _ => var_precedence
      | Let _ => let_precedence
      | Lam _ => lam_precedence
      | Ap _ => ap_precedence
      | NumLit _ => numlit_precedence
      | Plus _ => plus_precedence
      | Inj _ => inj_precedence
      | Case _ => case_precedence
      | EmptyHole _ => empty_hole_precedence
      | NonEmptyHole _ _ => non_empty_hole_precedence
      }
    );
  let zexp_precedence ze =>
    ZExp.(
      switch ze {
      | CursorE e => hexp_precedence e
      | LeftAsc _
      | RightAsc _ => asc_precedence
      | LetZ1 _
      | LetZ2 _ => let_precedence
      | LamZ _ => lam_precedence
      | LeftAp _
      | RightAp _ => ap_precedence
      | LeftPlus _
      | RightPlus _ => plus_precedence
      | InjZ _ => inj_precedence
      | CaseZ1 _
      | CaseZ2 _
      | CaseZ3 _ => case_precedence
      | NonEmptyHoleZ _ _ => non_empty_hole_precedence
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
      | Plus _ _ => plus_precedence
      | Inj _ _ _ => inj_precedence
      | Case _ _ _ _ => case_precedence
      | EmptyHole _ _ _ => empty_hole_precedence
      | NonEmptyHole _ _ _ _ => non_empty_hole_precedence
      | Cast _ _ => cast_precedence
      }
    );
  let rec of_hexp' path e paren =>
    if paren {
      parens "(" ^^ PP.nestRelative 0 (of_hexp path e) ^^ parens ")"
    } else {
      of_hexp path e
    }
  and of_hexp path e =>
    switch e {
    | HExp.Asc e' tau =>
      let prec1 = hexp_precedence e';
      let paren1 = prec1 !== 0;
      let path1 = [0, ...path];
      let path2 = [1, ...path];
      let r1 = of_hexp' path1 e' paren1;
      let r2 = of_htype path2 tau;
      of_Asc path r1 r2
    | HExp.Var x => term "Var" path (var x)
    | HExp.Let x e e' =>
      let path1 = [0, ...path];
      let path2 = [1, ...path];
      let r1 = of_hexp path1 e;
      let r2 = of_hexp path2 e';
      of_Let path x r1 r2
    | HExp.Lam x e' =>
      let path1 = [0, ...path];
      let r1 = of_hexp path1 e';
      of_Lam path x r1
    | HExp.Ap e1 e2 =>
      let path1 = [0, ...path];
      let path2 = [1, ...path];
      let prec1 = hexp_precedence e1;
      let paren1 = prec1 !== 0 && prec1 < ap_precedence;
      let r1 = of_hexp' path1 e1 paren1;
      let prec2 = hexp_precedence e2;
      let paren2 = prec2 !== 0 && prec2 <= ap_precedence;
      let r2 = of_hexp' path2 e2 paren2;
      of_Ap path r1 r2
    | HExp.NumLit n => term "NumLit" path (taggedText "number" (string_of_int n))
    | HExp.Plus e1 e2 =>
      let path1 = [0, ...path];
      let path2 = [1, ...path];
      let prec1 = hexp_precedence e1;
      let paren1 = prec1 > 0 && prec1 !== plus_precedence;
      let r1 = of_hexp' path1 e1 paren1;
      let prec2 = hexp_precedence e2;
      let paren2 = prec2 > 0 && prec2 !== plus_precedence;
      let r2 = of_hexp' path2 e2 paren2;
      of_Plus path r1 r2
    | HExp.Inj side e =>
      let path1 = [0, ...path];
      let r1 = of_hexp path1 e;
      of_Inj path side r1
    | HExp.Case e1 (x, e2) (y, e3) =>
      let path1 = [0, ...path];
      let path2 = [1, ...path];
      let path3 = [2, ...path];
      let r1 = of_hexp path1 e1;
      let r2 = of_hexp path2 e2;
      let r3 = of_hexp path3 e3;
      of_Case path r1 x r2 y r3
    | HExp.EmptyHole u =>
      term
        "EmptyHole"
        path
        (taggedText "hole" "\226\144\163" ^^ taggedText "holeName" (string_of_int u))
    | HExp.NonEmptyHole u e =>
      let path1 = [0, ...path];
      let r1 = of_hexp path1 e;
      of_NonEmptyHole path u r1
    };
  let rec of_zexp' path ze paren =>
    switch ze {
    | ZExp.CursorE e => PP.tagged "cursor" None (of_hexp' path e paren)
    | _ =>
      if paren {
        parens "(" ^^ PP.nestRelative 0 (of_zexp path ze) ^^ parens ")"
      } else {
        of_zexp path ze
      }
    }
  and of_zexp path ze =>
    switch ze {
    | ZExp.CursorE e => PP.tagged "cursor" None (of_hexp path e)
    | ZExp.LeftAsc ze tau =>
      let path1 = [0, ...path];
      let path2 = [1, ...path];
      let prec1 = zexp_precedence ze;
      let paren1 = prec1 !== 0;
      let r1 = of_zexp' path1 ze paren1;
      let r2 = of_htype path2 tau;
      of_Asc path r1 r2
    | ZExp.RightAsc e ztau =>
      let path1 = [0, ...path];
      let path2 = [1, ...path];
      let prec1 = hexp_precedence e;
      let paren1 = prec1 !== 0;
      let r1 = of_hexp' path1 e paren1;
      let r2 = of_ztype path2 ztau;
      of_Asc path r1 r2
    | ZExp.LetZ1 x ze e =>
      let path1 = [0, ...path];
      let path2 = [1, ...path];
      let r1 = of_zexp path1 ze;
      let r2 = of_hexp path2 e;
      of_Let path x r1 r2
    | ZExp.LetZ2 x e ze =>
      let path1 = [0, ...path];
      let path2 = [1, ...path];
      let r1 = of_hexp path1 e;
      let r2 = of_zexp path2 ze;
      of_Let path x r1 r2
    | ZExp.LamZ x ze =>
      let path1 = [0, ...path];
      let r1 = of_zexp path1 ze;
      of_Lam path x r1
    | ZExp.LeftAp ze e =>
      let path1 = [0, ...path];
      let path2 = [1, ...path];
      let prec1 = zexp_precedence ze;
      let paren1 = prec1 !== 0 && prec1 < ap_precedence;
      let r1 = of_zexp' path1 ze paren1;
      let prec2 = hexp_precedence e;
      let paren2 = prec2 !== 0 && prec2 <= ap_precedence;
      let r2 = of_hexp' path2 e paren2;
      of_Ap path r1 r2
    | ZExp.RightAp e ze =>
      let path1 = [0, ...path];
      let path2 = [1, ...path];
      let prec1 = hexp_precedence e;
      let paren1 = prec1 !== 0 && prec1 < ap_precedence;
      let r1 = of_hexp' path1 e paren1;
      let prec2 = zexp_precedence ze;
      let paren2 = prec2 !== 0 && prec2 <= ap_precedence;
      let r2 = of_zexp' path2 ze paren2;
      of_Ap path r1 r2
    | ZExp.LeftPlus ze e =>
      let path1 = [0, ...path];
      let path2 = [1, ...path];
      let prec1 = zexp_precedence ze;
      let paren1 = prec1 > 0 && prec1 !== plus_precedence;
      let r1 = of_zexp' path1 ze paren1;
      let prec2 = hexp_precedence e;
      let paren2 = prec2 > 0 && prec2 !== plus_precedence;
      let r2 = of_hexp' path2 e paren2;
      of_Plus path r1 r2
    | ZExp.RightPlus e ze =>
      let path1 = [0, ...path];
      let path2 = [1, ...path];
      let prec1 = hexp_precedence e;
      let paren1 = prec1 > 0 && prec1 !== plus_precedence;
      let r1 = of_hexp' path1 e paren1;
      let prec2 = zexp_precedence ze;
      let paren2 = prec2 > 0 && prec2 !== plus_precedence;
      let r2 = of_zexp' path2 ze paren2;
      of_Plus path r1 r2
    | ZExp.InjZ side ze =>
      let path1 = [0, ...path];
      let r = of_zexp path1 ze;
      of_Inj path side r
    | ZExp.CaseZ1 ze (x, e2) (y, e3) =>
      let path1 = [0, ...path];
      let path2 = [1, ...path];
      let path3 = [2, ...path];
      let r1 = of_zexp path1 ze;
      let r2 = of_hexp path2 e2;
      let r3 = of_hexp path3 e3;
      of_Case path r1 x r2 y r3
    | ZExp.CaseZ2 e1 (x, ze) (y, e3) =>
      let path1 = [0, ...path];
      let path2 = [1, ...path];
      let path3 = [2, ...path];
      let r1 = of_hexp path1 e1;
      let r2 = of_zexp path2 ze;
      let r3 = of_hexp path3 e3;
      of_Case path r1 x r2 y r3
    | ZExp.CaseZ3 e1 (x, e2) (y, ze) =>
      let path1 = [0, ...path];
      let path2 = [1, ...path];
      let path3 = [2, ...path];
      let r1 = of_hexp path1 e1;
      let r2 = of_hexp path2 e2;
      let r3 = of_zexp path3 ze;
      of_Case path r1 x r2 y r3
    | ZExp.NonEmptyHoleZ u ze =>
      let path1 = [0, ...path];
      let r = of_zexp path1 ze;
      of_NonEmptyHole path u r
    };
  let rec of_dhexp' path d paren =>
    if paren {
      parens "(" ^^ PP.nestRelative 0 (of_dhexp path d) ^^ parens ")"
    } else {
      of_dhexp path d
    }
  and of_dhexp path d =>
    Dynamics.(
      DHExp.(
        switch d {
        | Var x => term "Var" path (var x)
        | Let x d1 d2 =>
          let path1 = [0, ...path];
          let path2 = [1, ...path];
          let r1 = of_dhexp path1 d1;
          let r2 = of_dhexp path2 d2;
          of_Let path x r1 r2
        | Lam x ty d1 =>
          let path1 = [0, ...path];
          let path2 = [1, ...path];
          let r1 = of_htype path1 ty;
          let r2 = of_dhexp path2 d1;
          of_LamAnn path x r1 r2
        | Ap d1 d2 =>
          let path1 = [0, ...path];
          let path2 = [1, ...path];
          let prec1 = dhexp_precedence d1;
          let paren1 = prec1 !== 0 && prec1 < ap_precedence;
          let r1 = of_dhexp' path1 d1 paren1;
          let prec2 = dhexp_precedence d2;
          let paren2 = prec2 !== 0 && prec2 <= ap_precedence;
          let r2 = of_dhexp' path2 d2 paren2;
          of_Ap path r1 r2
        | NumLit n => term "NumLit" path (taggedText "number" (string_of_int n))
        | Plus d1 d2 =>
          let path1 = [0, ...path];
          let path2 = [1, ...path];
          let prec1 = dhexp_precedence d1;
          let paren1 = prec1 > 0 && prec1 !== plus_precedence;
          let r1 = of_dhexp' path1 d1 paren1;
          let prec2 = dhexp_precedence d2;
          let paren2 = prec2 > 0 && prec2 !== plus_precedence;
          let r2 = of_dhexp' path2 d2 paren2;
          of_Plus path r1 r2
        | Inj ty side d1 =>
          let path1 = [0, ...path];
          let path2 = [1, ...path];
          let r1 = of_htype path1 ty;
          let r2 = of_dhexp path2 d1;
          of_InjAnn path r1 side r2
        | Case ty d1 (x, d2) (y, d3) =>
          let path1 = [0, ...path];
          let path2 = [1, ...path];
          let path3 = [2, ...path];
          let path4 = [3, ...path];
          let r1 = of_htype path1 ty;
          let r2 = of_dhexp path2 d1;
          let r3 = of_dhexp path3 d2;
          let r4 = of_dhexp path4 d3;
          of_CaseAnn path r1 r2 x r3 y r4
        | EmptyHole u m sigma =>
          let hole_string =
            switch m {
            | Unevaled => "\226\144\163"
            | Evaled => "\226\144\163"
            };
          term
            "EmptyHole"
            path
            (
              taggedText "hole" hole_string ^^
              PP.tagged
                "hole-decorations"
                None
                (
                  taggedText "holeName" (string_of_int u) ^^
                  PP.tagged "environment" None (of_sigma path sigma)
                )
            )
        | NonEmptyHole u m sigma d1 =>
          let path1 = [0, ...path];
          let hole_class =
            switch m {
            | Unevaled => "unevaled-ne-hole"
            | Evaled => "evaled-ne-hole"
            };
          let r = of_dhexp path1 d1;
          term "NonEmptyHole" path (PP.tagged hole_class None r) ^^
          PP.tagged
            "hole-decorations"
            None
            (
              taggedText "holeName" (string_of_int u) ^^
              PP.tagged "environment" None (of_sigma path sigma)
            )
        | Cast ty d1 =>
          let path1 = [0, ...path];
          let path2 = [1, ...path];
          let r1 = of_htype path1 ty;
          let r2 = of_dhexp path2 d1;
          of_Cast path r1 r2
        }
      )
    )
  and of_sigma path sigma => {
    let map_f (x, d) => of_dhexp path d ^^ kw "/" ^^ PP.text x;
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
