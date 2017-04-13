module PPView = struct
  module PP = Pretty.PP
  open Hazel_semantics
  let (^^) = PP.(^^)

  (* Utility functions *)
  let taggedText tag s = 
    PP.tagged tag (PP.text s)
  let kw = taggedText "kw"
  let parens = taggedText "paren" 
  let op = taggedText "op"
  let var = taggedText "var" 
  let space = taggedText "space" " "
  let term = PP.tagged
  let optionalBreakSp = PP.optionalBreak " "

  (* # types *)

  let rec of_Arrow r1 r2 = 
    term "Arrow" (
      r1 ^^ optionalBreakSp ^^ 
      (op "→") ^^ optionalBreakSp ^^
      r2)

  let rec of_Sum r1 r2 = 
    term "Sum" (
      r1 ^^ optionalBreakSp ^^ 
      (op "+") ^^ optionalBreakSp ^^
      r2)

  (* ## parenthesization helpers *)

  let num_precedence = 0 
  let hole_precedence = 0  
  let sum_precedence = 1
  let arrow_precedence = 2
  let h_precedence tau = match tau with 
    | HTyp.Num -> num_precedence
    | HTyp.Hole -> hole_precedence
    | HTyp.Sum _ -> sum_precedence
    | HTyp.Arrow _ -> arrow_precedence
  let z_precedence ztau = match ztau with 
    | ZTyp.CursorT tau -> h_precedence tau
    | ZTyp.LeftArrow _ | ZTyp.RightArrow _ -> arrow_precedence
    | ZTyp.LeftSum _ | ZTyp.RightSum _ -> sum_precedence

  (* parenthesize binary right-associative type operators *)
  let parenthesize_bi_r_tau prec prec1 prec2 = 
    let paren1 = (prec1 != prec) && (prec1 != 0) in 
    let paren2 = (prec2 != prec) && (prec2 != 0) in 
    (paren1, paren2)

  let rec of_htype' tau paren = 
    if paren 
    then 
      (parens "(") ^^ (PP.nestRelative 0 (of_htype tau)) ^^ (parens ")") 
    else
      of_htype tau
  and of_htype tau = match tau with 
    | HTyp.Num -> 
      term "Num" (kw "num")
    | HTyp.Arrow (tau1, tau2) -> 
      let (paren1, paren2) = 
        parenthesize_bi_r_tau arrow_precedence 
          (h_precedence tau1) (h_precedence tau2) in 
      let r1 = of_htype' tau1 paren1 in  
      let r2 = of_htype' tau2 paren2 in 
      of_Arrow r1 r2
    | HTyp.Sum (tau1, tau2) -> 
      let (paren1, paren2) = 
        parenthesize_bi_r_tau sum_precedence 
          (h_precedence tau1) (h_precedence tau2) in 
      let r1 = of_htype' tau1 paren1 in  
      let r2 = of_htype' tau2 paren2 in 
      of_Sum r1 r2
    | HTyp.Hole -> 
      term "Hole" (taggedText "hole" "⦇⦈")

  let rec of_ztype' ztau paren = match ztau with 
    | ZTyp.CursorT tau -> 
      term "cursor" (of_htype' tau paren)
    | _ -> 
      if paren then 
        (parens "(") ^^ (PP.nestRelative 0 (of_ztype ztau)) ^^ (parens ")")
      else of_ztype ztau 
  and of_ztype ztau = match ztau with 
    | ZTyp.CursorT tau -> 
      term "cursor" (of_htype tau)
    | ZTyp.LeftArrow (ztau, tau) -> 
      let (paren1, paren2) = 
        parenthesize_bi_r_tau arrow_precedence 
          (z_precedence ztau) (h_precedence tau) in  
      let r1 = of_ztype' ztau paren1 in 
      let r2 = of_htype' tau paren2 in 
      of_Arrow r1 r2
    | ZTyp.RightArrow (tau, ztau) -> 
      let (paren1, paren2) = 
        parenthesize_bi_r_tau arrow_precedence 
          (h_precedence tau) (z_precedence ztau) in  
      let r1 = of_htype' tau paren1 in 
      let r2 = of_ztype' ztau paren2 in 
      of_Arrow r1 r2
    | ZTyp.LeftSum (ztau, tau) -> 
      let (paren1, paren2) = 
        parenthesize_bi_r_tau sum_precedence 
          (z_precedence ztau) (h_precedence tau) in  
      let r1 = of_ztype' ztau paren1 in 
      let r2 = of_htype' tau paren2 in 
      of_Sum r1 r2
    | ZTyp.RightSum (tau, ztau) -> 
      let (paren1, paren2) = 
        parenthesize_bi_r_tau sum_precedence 
          (h_precedence tau) (z_precedence ztau) in  
      let r1 = of_htype' tau paren1 in 
      let r2 = of_ztype' ztau paren2 in 
      of_Sum r1 r2

  (* h-exps and z-exps *)
  let string_of_side side = match side with 
    | HExp.L -> "L"
    | HExp.R -> "R"

  let of_Asc r1 r2 = 
    term "Asc" (
      r1 ^^ space ^^ (op ":") ^^ space ^^ r2)

  let of_Let x r1 r2 =
    term "Let" (
      (PP.blockBoundary) ^^ 
      (kw "let") ^^ space ^^ 
      (var x) ^^ space ^^ 
      (op "=") ^^ (PP.nestAbsolute 2 (
          optionalBreakSp ^^ 
          r1)) ^^ 
      (PP.mandatoryBreak) ^^ 
      r2)

  let of_Lam x r = 
    term "Lam" (
      (kw "λ") ^^ 
      (var x) ^^ 
      (kw ".") ^^ 
      (PP.optionalBreak "") ^^
      (PP.nestRelative 4 r))

  let of_Ap r1 r2 = 
    term "Ap" (r1 ^^ optionalBreakSp ^^ r2)

  let of_Plus r1 r2 = 
    term "Plus" (
      r1 ^^ optionalBreakSp ^^ 
      (op "+") ^^ optionalBreakSp ^^ r2) 

  let of_Inj side r = 
    term "Inj" (
      (kw "inj") ^^ (parens "[") ^^ 
      (kw (string_of_side side)) ^^ (parens "]") ^^ 
      (parens "(") ^^ optionalBreakSp ^^ r ^^ (parens ")"))

  let of_Case r1 x r2 y r3 = 
    term "Case" (
      (PP.blockBoundary) ^^ 
      (kw "case") ^^ space ^^ r1 ^^ 
      PP.mandatoryBreak ^^ 
      (kw "L") ^^ (parens "(") ^^ (var x) ^^ (parens ")") ^^ 
      space ^^ (op "=>") ^^ optionalBreakSp ^^ (PP.nestAbsolute 2 r2) ^^ 
      PP.mandatoryBreak ^^ 
      (kw "R") ^^ (parens "(") ^^ (var y) ^^ (parens ")") ^^ 
      space ^^ (op "=>") ^^ optionalBreakSp ^^ (PP.nestAbsolute 2 r3))

  let of_NonEmptyHole r = 
    term "NonEmptyHole" (
      (taggedText "hole" "⦇") ^^ r ^^ 
      (taggedText "hole" "⦈")) 

  let asc_precedence = 1
  let var_precedence = 0
  let let_precedence = 1
  let lam_precedence = 1
  let ap_precedence = 1
  let numlit_precedence = 0
  let plus_precedence = 2 
  let inj_precedence = 0
  let case_precedence = 1
  let empty_hole_precedence = 0
  let non_empty_hole_precedence = 0
  let hexp_precedence e = HExp.(match e with 
      | Asc _ -> asc_precedence
      | Var _ -> var_precedence
      | Let _ -> let_precedence
      | Lam _ -> lam_precedence
      | Ap _ -> ap_precedence
      | NumLit _ -> numlit_precedence
      | Plus _ -> plus_precedence
      | Inj _ -> inj_precedence
      | Case _ -> case_precedence
      | EmptyHole -> empty_hole_precedence
      | NonEmptyHole _ -> non_empty_hole_precedence)
  let zexp_precedence ze = ZExp.(match ze with 
      | CursorE e -> hexp_precedence e
      | LeftAsc _ | RightAsc _ -> asc_precedence
      | LetZ1 _ | LetZ2 _ -> let_precedence
      | LamZ _ -> lam_precedence
      | LeftAp _ | RightAp _ -> ap_precedence
      | LeftPlus _ | RightPlus _ -> plus_precedence
      | InjZ _ -> inj_precedence
      | CaseZ1 _ | CaseZ2 _ | CaseZ3 _ -> case_precedence
      | NonEmptyHoleZ _ -> non_empty_hole_precedence)

  let rec of_hexp' e paren = 
    if paren 
    then 
      (parens "(") ^^ (PP.nestRelative 0 (of_hexp e)) ^^ (parens ")")
    else 
      of_hexp e
  and of_hexp e = match e with 
    | HExp.Asc (e', tau) -> 
      let prec1 = hexp_precedence e' in 
      let paren1 = prec1 != 0 in 
      let r1 = of_hexp' e' paren1 in 
      let r2 = of_htype tau in 
      of_Asc r1 r2
    | HExp.Var x -> term "Var" (var x)
    | HExp.Let (x, e, e') -> of_Let x (of_hexp e) (of_hexp e')
    | HExp.Lam (x, e') -> of_Lam x (of_hexp e')
    | HExp.Ap (e1, e2) ->
      let prec1 = hexp_precedence e1 in 
      let paren1 = prec1 != 0 in 
      let r1 = of_hexp' e1 paren1 in 
      let prec2 = hexp_precedence e2 in 
      let paren2 = prec2 != 0 in 
      let r2 = of_hexp' e2 paren2 in  
      of_Ap r1 r2
    | HExp.NumLit n -> 
      term "NumLit" (
        taggedText "number" (string_of_int n))
    | HExp.Plus (e1, e2) -> 
      let prec1 = hexp_precedence e1 in 
      let paren1 = (prec1 > 0) && (prec1 < plus_precedence) in
      let r1 = of_hexp' e1 paren1 in 
      let prec2 = hexp_precedence e2 in 
      let paren2 = (prec2 > 0) && (prec2 < plus_precedence) in 
      let r2 = of_hexp' e2 paren2 in 
      of_Plus r1 r2
    | HExp.Inj (side, e) -> of_Inj side (of_hexp e)
    | HExp.Case (e1, (x, e2), (y, e3)) -> 
      of_Case (of_hexp e1) x (of_hexp e2) y (of_hexp e3)
    | HExp.EmptyHole -> 
      term "EmptyHole" (taggedText "hole" "⦇⦈")
    | HExp.NonEmptyHole e -> of_NonEmptyHole (of_hexp e)

  let rec of_zexp' ze paren = match ze with 
    | ZExp.CursorE e -> 
      term "cursor" (of_hexp' e paren) 
    | _ -> 
      if paren then 
        (parens "(") ^^ (PP.nestRelative 0 (of_zexp ze)) ^^ (parens ")")
      else of_zexp ze 
  and of_zexp ze = match ze with 
    | ZExp.CursorE e -> 
      term "cursor" (of_hexp e)
    | ZExp.LeftAsc (ze, tau) -> 
      let prec1 = zexp_precedence ze in 
      let paren1 = prec1 != 0 in 
      let r1 = of_zexp' ze paren1 in 
      let r2 = of_htype tau in 
      of_Asc r1 r2
    | ZExp.RightAsc (e, ztau) -> 
      let prec1 = hexp_precedence e in 
      let paren1 = prec1 != 0 in 
      let r1 = of_hexp' e paren1 in 
      let r2 = of_ztype ztau in 
      of_Asc r1 r2
    | ZExp.LetZ1 (x, ze, e) -> of_Let x (of_zexp ze) (of_hexp e)
    | ZExp.LetZ2 (x, e, ze) -> of_Let x (of_hexp e) (of_zexp ze)
    | ZExp.LamZ (x, ze) -> of_Lam x (of_zexp ze)
    | ZExp.LeftAp (ze, e) -> 
      let prec1 = zexp_precedence ze in 
      let paren1 = prec1 != 0 in 
      let r1 = of_zexp' ze paren1 in 
      let prec2 = hexp_precedence e in 
      let paren2 = prec2 != 0 in 
      let r2 = of_hexp' e paren2 in  
      of_Ap r1 r2
    | ZExp.RightAp (e, ze) -> 
      let prec1 = hexp_precedence e in 
      let paren1 = prec1 != 0 in 
      let r1 = of_hexp' e paren1 in 
      let prec2 = zexp_precedence ze in 
      let paren2 = prec2 != 0 in 
      let r2 = of_zexp' ze paren2 in  
      of_Ap r1 r2
    | ZExp.LeftPlus (ze, e) -> 
      let prec1 = zexp_precedence ze in 
      let paren1 = (prec1 > 0) && (prec1 < plus_precedence) in
      let r1 = of_zexp' ze paren1 in 
      let prec2 = hexp_precedence e in 
      let paren2 = (prec2 > 0) && (prec2 < plus_precedence) in 
      let r2 = of_hexp' e paren2 in 
      of_Plus r1 r2
    | ZExp.RightPlus (e, ze) ->
      let prec1 = hexp_precedence e in 
      let paren1 = (prec1 > 0) && (prec1 < plus_precedence) in
      let r1 = of_hexp' e paren1 in 
      let prec2 = zexp_precedence ze in 
      let paren2 = (prec2 > 0) && (prec2 < plus_precedence) in 
      let r2 = of_zexp' ze paren2 in 
      of_Plus r1 r2
    | ZExp.InjZ (side, ze) -> of_Inj side (of_zexp ze)
    | ZExp.CaseZ1 (ze, (x, e2), (y, e3)) -> 
      of_Case (of_zexp ze) x (of_hexp e2) y (of_hexp e3)
    | ZExp.CaseZ2 (e1, (x, ze), (y, e3)) -> 
      of_Case (of_hexp e1) x (of_zexp ze) y (of_hexp e3)
    | ZExp.CaseZ3 (e1, (x, e2), (y, ze)) -> 
      of_Case (of_hexp e1) x (of_hexp e2) y (of_zexp ze)
    | ZExp.NonEmptyHoleZ ze -> 
      of_NonEmptyHole (of_zexp ze)
end

