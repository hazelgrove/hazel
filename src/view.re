module PPView = {
  module PP = Pretty.PP;
  open Semantics.Core;
  let (^^) = PP.(^^);
  /* Utility functions */
  let taggedText tag s => PP.tagged tag (PP.text s);
  let kw = taggedText "kw";
  let parens = taggedText "paren";
  let op = taggedText "op";
  let var s => taggedText "var" s;
  let space = taggedText "space" " ";
  let term = PP.tagged;
  let optionalBreakSp = PP.optionalBreak " ";
  /* # types */
  let rec of_Arrow r1 r2 =>
    term "Arrow" (r1 ^^ optionalBreakSp ^^ op "\226\134\146" ^^ optionalBreakSp ^^ r2);
  let rec of_Sum r1 r2 => term "Sum" (r1 ^^ optionalBreakSp ^^ op "+" ^^ optionalBreakSp ^^ r2);
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
  let rec of_htype' tau paren =>
    if paren {
      parens "(" ^^ PP.nestRelative 0 (of_htype tau) ^^ parens ")"
    } else {
      of_htype tau
    }
  and of_htype tau =>
    switch tau {
    | HTyp.Num => term "Num" (kw "num")
    | HTyp.Arrow tau1 tau2 [@implicit_arity] =>
      let (paren1, paren2) =
        parenthesize_bi_r_tau arrow_precedence (h_precedence tau1) (h_precedence tau2);
      let r1 = of_htype' tau1 paren1;
      let r2 = of_htype' tau2 paren2;
      of_Arrow r1 r2
    | HTyp.Sum tau1 tau2 [@implicit_arity] =>
      let (paren1, paren2) =
        parenthesize_bi_r_tau sum_precedence (h_precedence tau1) (h_precedence tau2);
      let r1 = of_htype' tau1 paren1;
      let r2 = of_htype' tau2 paren2;
      of_Sum r1 r2
    | HTyp.Hole => term "Hole" (taggedText "hole" "\226\144\163")
    };
  let rec of_ztype' ztau paren =>
    switch ztau {
    | ZTyp.CursorT tau => term "cursor" (of_htype' tau paren)
    | _ =>
      if paren {
        parens "(" ^^ PP.nestRelative 0 (of_ztype ztau) ^^ parens ")"
      } else {
        of_ztype ztau
      }
    }
  and of_ztype ztau =>
    switch ztau {
    | ZTyp.CursorT tau => term "cursor" (of_htype tau)
    | ZTyp.LeftArrow ztau tau [@implicit_arity] =>
      let (paren1, paren2) =
        parenthesize_bi_r_tau arrow_precedence (z_precedence ztau) (h_precedence tau);
      let r1 = of_ztype' ztau paren1;
      let r2 = of_htype' tau paren2;
      of_Arrow r1 r2
    | ZTyp.RightArrow tau ztau [@implicit_arity] =>
      let (paren1, paren2) =
        parenthesize_bi_r_tau arrow_precedence (h_precedence tau) (z_precedence ztau);
      let r1 = of_htype' tau paren1;
      let r2 = of_ztype' ztau paren2;
      of_Arrow r1 r2
    | ZTyp.LeftSum ztau tau [@implicit_arity] =>
      let (paren1, paren2) =
        parenthesize_bi_r_tau sum_precedence (z_precedence ztau) (h_precedence tau);
      let r1 = of_ztype' ztau paren1;
      let r2 = of_htype' tau paren2;
      of_Sum r1 r2
    | ZTyp.RightSum tau ztau [@implicit_arity] =>
      let (paren1, paren2) =
        parenthesize_bi_r_tau sum_precedence (h_precedence tau) (z_precedence ztau);
      let r1 = of_htype' tau paren1;
      let r2 = of_ztype' ztau paren2;
      of_Sum r1 r2
    };
  /* h-exps and z-exps */
  let string_of_side side =>
    switch side {
    | HExp.L => "L"
    | HExp.R => "R"
    };
  let of_Asc r1 r2 => term "Asc" (r1 ^^ space ^^ op ":" ^^ space ^^ r2);
  let of_Let x r1 r2 =>
    term
      "Let"
      (
        PP.blockBoundary ^^
        kw "let" ^^
        space ^^
        var x ^^
        space ^^ op "=" ^^ PP.nestAbsolute 2 (optionalBreakSp ^^ r1) ^^ PP.mandatoryBreak ^^ r2
      );
  let of_Lam x r =>
    term "Lam" (kw "\206\187" ^^ var x ^^ kw "." ^^ PP.optionalBreak "" ^^ PP.nestRelative 4 r);
  let of_LamAnn x rty r1 =>
    term
      "LamAnn"
      (
        kw "\206\187" ^^
        var x ^^ kw ":" ^^ rty ^^ kw "." ^^ PP.optionalBreak "" ^^ PP.nestRelative 4 r1
      );
  let of_Ap r1 r2 => term "Ap" (r1 ^^ optionalBreakSp ^^ r2);
  let of_Plus r1 r2 => term "Plus" (r1 ^^ optionalBreakSp ^^ op "+" ^^ optionalBreakSp ^^ r2);
  let of_Inj side r =>
    term
      "Inj"
      (
        kw "inj" ^^
        parens "[" ^^
        kw (string_of_side side) ^^
        parens "]" ^^ parens "(" ^^ PP.optionalBreak "" ^^ r ^^ parens ")"
      );
  let of_InjAnn rty side r =>
    term
      "Inj"
      (
        kw "inj" ^^
        parens "[" ^^
        kw (string_of_side side) ^^
        kw "," ^^
        optionalBreakSp ^^
        rty ^^ parens "]" ^^ parens "(" ^^ PP.optionalBreak "" ^^ r ^^ parens ")"
      );
  let of_Case r1 x r2 y r3 =>
    term
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
        var y ^^
        parens ")" ^^ space ^^ op "\226\135\146" ^^ optionalBreakSp ^^ PP.nestAbsolute 2 r3
      );
  let of_CaseAnn rty r1 x r2 y r3 =>
    term
      "Case"
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
  let of_NonEmptyHole u r => term "NonEmptyHole" r ^^ taggedText "holeName" (string_of_int u);
  let of_Cast rty r1 =>
    term "Cast" (parens "<" ^^ rty ^^ parens ">(" ^^ PP.optionalBreak "" ^^ r1 ^^ parens ")");
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
  let rec of_hexp' e paren =>
    if paren {
      parens "(" ^^ PP.nestRelative 0 (of_hexp e) ^^ parens ")"
    } else {
      of_hexp e
    }
  and of_hexp e =>
    switch e {
    | HExp.Asc e' tau [@implicit_arity] =>
      let prec1 = hexp_precedence e';
      let paren1 = prec1 !== 0;
      let r1 = of_hexp' e' paren1;
      let r2 = of_htype tau;
      of_Asc r1 r2
    | HExp.Var x => term "Var" (var x)
    | HExp.Let x e e' [@implicit_arity] => of_Let x (of_hexp e) (of_hexp e')
    | HExp.Lam x e' [@implicit_arity] => of_Lam x (of_hexp e')
    | HExp.Ap e1 e2 [@implicit_arity] =>
      let prec1 = hexp_precedence e1;
      let paren1 = prec1 !== 0 && prec1 < ap_precedence;
      let r1 = of_hexp' e1 paren1;
      let prec2 = hexp_precedence e2;
      let paren2 = prec2 !== 0 && prec2 <= ap_precedence;
      let r2 = of_hexp' e2 paren2;
      of_Ap r1 r2
    | HExp.NumLit n => term "NumLit" (taggedText "number" (string_of_int n))
    | HExp.Plus e1 e2 [@implicit_arity] =>
      let prec1 = hexp_precedence e1;
      let paren1 = prec1 > 0 && prec1 !== plus_precedence;
      let r1 = of_hexp' e1 paren1;
      let prec2 = hexp_precedence e2;
      let paren2 = prec2 > 0 && prec2 !== plus_precedence;
      let r2 = of_hexp' e2 paren2;
      of_Plus r1 r2
    | HExp.Inj side e [@implicit_arity] => of_Inj side (of_hexp e)
    | HExp.Case e1 (x, e2) (y, e3) [@implicit_arity] =>
      of_Case (of_hexp e1) x (of_hexp e2) y (of_hexp e3)
    | HExp.EmptyHole u =>
      term
        "EmptyHole" (taggedText "hole" "\226\144\163" ^^ taggedText "holeName" (string_of_int u))
    | HExp.NonEmptyHole u e => of_NonEmptyHole u (of_hexp e)
    };
  let rec of_zexp' ze paren =>
    switch ze {
    | ZExp.CursorE e => term "cursor" (of_hexp' e paren)
    | _ =>
      if paren {
        parens "(" ^^ PP.nestRelative 0 (of_zexp ze) ^^ parens ")"
      } else {
        of_zexp ze
      }
    }
  and of_zexp ze =>
    switch ze {
    | ZExp.CursorE e => term "cursor" (of_hexp e)
    | ZExp.LeftAsc ze tau [@implicit_arity] =>
      let prec1 = zexp_precedence ze;
      let paren1 = prec1 !== 0;
      let r1 = of_zexp' ze paren1;
      let r2 = of_htype tau;
      of_Asc r1 r2
    | ZExp.RightAsc e ztau [@implicit_arity] =>
      let prec1 = hexp_precedence e;
      let paren1 = prec1 !== 0;
      let r1 = of_hexp' e paren1;
      let r2 = of_ztype ztau;
      of_Asc r1 r2
    | ZExp.LetZ1 x ze e [@implicit_arity] => of_Let x (of_zexp ze) (of_hexp e)
    | ZExp.LetZ2 x e ze [@implicit_arity] => of_Let x (of_hexp e) (of_zexp ze)
    | ZExp.LamZ x ze [@implicit_arity] => of_Lam x (of_zexp ze)
    | ZExp.LeftAp ze e [@implicit_arity] =>
      let prec1 = zexp_precedence ze;
      let paren1 = prec1 !== 0 && prec1 < ap_precedence;
      let r1 = of_zexp' ze paren1;
      let prec2 = hexp_precedence e;
      let paren2 = prec2 !== 0 && prec2 <= ap_precedence;
      let r2 = of_hexp' e paren2;
      of_Ap r1 r2
    | ZExp.RightAp e ze [@implicit_arity] =>
      let prec1 = hexp_precedence e;
      let paren1 = prec1 !== 0 && prec1 < ap_precedence;
      let r1 = of_hexp' e paren1;
      let prec2 = zexp_precedence ze;
      let paren2 = prec2 !== 0 && prec2 <= ap_precedence;
      let r2 = of_zexp' ze paren2;
      of_Ap r1 r2
    | ZExp.LeftPlus ze e [@implicit_arity] =>
      let prec1 = zexp_precedence ze;
      let paren1 = prec1 > 0 && prec1 !== plus_precedence;
      let r1 = of_zexp' ze paren1;
      let prec2 = hexp_precedence e;
      let paren2 = prec2 > 0 && prec2 !== plus_precedence;
      let r2 = of_hexp' e paren2;
      of_Plus r1 r2
    | ZExp.RightPlus e ze [@implicit_arity] =>
      let prec1 = hexp_precedence e;
      let paren1 = prec1 > 0 && prec1 !== plus_precedence;
      let r1 = of_hexp' e paren1;
      let prec2 = zexp_precedence ze;
      let paren2 = prec2 > 0 && prec2 !== plus_precedence;
      let r2 = of_zexp' ze paren2;
      of_Plus r1 r2
    | ZExp.InjZ side ze [@implicit_arity] => of_Inj side (of_zexp ze)
    | ZExp.CaseZ1 ze (x, e2) (y, e3) [@implicit_arity] =>
      of_Case (of_zexp ze) x (of_hexp e2) y (of_hexp e3)
    | ZExp.CaseZ2 e1 (x, ze) (y, e3) [@implicit_arity] =>
      of_Case (of_hexp e1) x (of_zexp ze) y (of_hexp e3)
    | ZExp.CaseZ3 e1 (x, e2) (y, ze) [@implicit_arity] =>
      of_Case (of_hexp e1) x (of_hexp e2) y (of_zexp ze)
    | ZExp.NonEmptyHoleZ u ze => of_NonEmptyHole u (of_zexp ze)
    };
  let rec of_dhexp' d paren =>
    if paren {
      parens "(" ^^ PP.nestRelative 0 (of_dhexp d) ^^ parens ")"
    } else {
      of_dhexp d
    }
  and of_dhexp d =>
    Dynamics.(
      DHExp.(
        switch d {
        | Var x => term "Var" (var x)
        | Let x d1 d2 => of_Let x (of_dhexp d1) (of_dhexp d2)
        | Lam x ty d1 => of_LamAnn x (of_htype ty) (of_dhexp d1)
        | Ap d1 d2 =>
          let prec1 = dhexp_precedence d1;
          let paren1 = prec1 !== 0 && prec1 < ap_precedence;
          let r1 = of_dhexp' d1 paren1;
          let prec2 = dhexp_precedence d2;
          let paren2 = prec2 !== 0 && prec2 <= ap_precedence;
          let r2 = of_dhexp' d2 paren2;
          of_Ap r1 r2
        | NumLit n => term "NumLit" (taggedText "number" (string_of_int n))
        | Plus d1 d2 =>
          let prec1 = dhexp_precedence d1;
          let paren1 = prec1 > 0 && prec1 !== plus_precedence;
          let r1 = of_dhexp' d1 paren1;
          let prec2 = dhexp_precedence d2;
          let paren2 = prec2 > 0 && prec2 !== plus_precedence;
          let r2 = of_dhexp' d2 paren2;
          of_Plus r1 r2
        | Inj ty side d1 => of_InjAnn (of_htype ty) side (of_dhexp d1)
        | Case ty d1 (x, d2) (y, d3) =>
          of_CaseAnn (of_htype ty) (of_dhexp d1) x (of_dhexp d2) y (of_dhexp d3)
        | EmptyHole u m sigma =>
          /* TODO: show names, substitutions */
          let hole_string =
            switch m {
            | Unevaled => "\226\144\163"
            | Evaled => "\226\144\163"
            };
          term
            "EmptyHole"
            (
              taggedText "hole" hole_string ^^
              PP.tagged
                "hole-decorations"
                (
                  taggedText "holeName" (string_of_int u) ^^
                  PP.tagged "environment" (of_sigma sigma)
                )
            )
        | NonEmptyHole u m sigma d1 =>
          let hole_class =
            switch m {
            | Unevaled => "unevaled-ne-hole"
            | Evaled => "evaled-ne-hole"
            };
          let r = of_dhexp d1;
          term "NonEmptyHole" (PP.tagged hole_class r) ^^
          PP.tagged
            "hole-decorations"
            (taggedText "holeName" (string_of_int u) ^^ PP.tagged "environment" (of_sigma sigma))
        | Cast ty d1 => of_Cast (of_htype ty) (of_dhexp d1)
        }
      )
    )
  and of_sigma sigma => {
    let map_f (x, d) => of_dhexp d ^^ kw "/" ^^ PP.text x;
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
