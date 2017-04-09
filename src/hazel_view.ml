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

  (* types *)
  let rec of_Arrow r1 r2 = 
    term "Arrow" (
      (parens "(") ^^ (PP.nestRelative 0 (
          (r1) ^^ 
          (op " ->") ^^
          PP.optionalBreak ^^
          (r2) ^^ 
          (parens ")"))))

  let rec of_Sum r1 r2 = 
    term "Sum" (
      (parens "(") ^^ (PP.nestRelative 0 (
          (r1) ^^ 
          (op " +") ^^
          PP.optionalBreak ^^
          (r2) ^^ 
          (parens ")"))))

  let rec of_htype tau = match tau with 
    | HTyp.Num -> 
      term "Num" (kw "num")
    | HTyp.Arrow (tau1, tau2) -> 
      of_Arrow (of_htype tau1) (of_htype tau2)
    | HTyp.Sum (tau1, tau2) -> 
      of_Sum (of_htype tau1) (of_htype tau2)
    | HTyp.Hole -> 
      term "Hole" (taggedText "hole" "⦇⦈")

  let rec of_ztype ztau = match ztau with 
    | ZTyp.CursorT tau -> 
      term "cursor" (of_htype tau)
    | ZTyp.LeftArrow (ztau, tau) -> 
      of_Arrow (of_ztype ztau) (of_htype tau)
    | ZTyp.RightArrow (tau, ztau) -> 
      of_Arrow (of_htype tau) (of_ztype ztau)
    | ZTyp.LeftSum (ztau, tau) -> 
      of_Sum (of_ztype ztau) (of_htype tau)
    | ZTyp.RightSum (tau, ztau) -> 
      of_Sum (of_htype tau) (of_ztype ztau)

  (* h-exps and z-exps *)
  let string_of_side side = match side with 
    | HExp.L -> "L"
    | HExp.R -> "R"

  let of_Asc r1 r2 = 
    term "Asc" (PP.nestAbsolute 2 (
        (parens "(" ^^ (PP.nestRelative 0 (
             (r1) ^^
             (op " :") ^^
             PP.optionalBreak ^^ 
             (r2) ^^ 
             (parens ")"))))))

  let of_Let x r1 r2 =
    term "Let" (
      (PP.blockBoundary) ^^ 
      (kw "let") ^^ space ^^ 
      (var x) ^^ space ^^ 
      (op "=") ^^ (PP.nestAbsolute 2 (
          PP.optionalBreak ^^ 
          (r1))) ^^ 
      (PP.mandatoryBreak) ^^ 
      (r2))

  let of_Lam x r = 
    term "Lam" (
      (kw "λ") ^^ 
      (var x) ^^ 
      (kw ".") ^^ 
      (PP.nestRelative 4 r))

  let of_Ap r1 r2 = 
    term "Ap" (r1 ^^ PP.optionalBreak ^^ r2)

  let of_Plus r1 r2 = 
    term "Plus" (
      r1 ^^ PP.optionalBreak ^^ 
      (op "+") ^^ PP.optionalBreak ^^ r2) 

  let of_Inj side r = 
    term "Inj" (
      (kw "inj") ^^ (parens "[") ^^ 
      (kw (string_of_side side)) ^^ (parens "]") ^^ 
      (parens "(") ^^ PP.optionalBreak ^^ r ^^ (parens ")"))

  let of_Case r1 x r2 y r3 = 
    term "Case" (
      (kw "case") ^^ space ^^ 
      r1 ^^ space ^^ 
      (kw "of") ^^ (PP.nestAbsolute 2 (
          PP.mandatoryBreak ^^ 
          (kw "L") ^^ (parens "(") ^^ (var x) ^^ (parens ")") ^^ 
          space ^^ (op "=>") ^^ space ^^ (PP.nestAbsolute 2 r2) ^^ 
          PP.mandatoryBreak ^^ 
          (kw "R") ^^ (parens "(") ^^ (var y) ^^ (parens ")") ^^ 
          space ^^ (op "=>") ^^ space ^^ (PP.nestAbsolute 2 r3))))

  let of_NonEmptyHole r = 
    term "NonEmptyHole" (
      (taggedText "hole" "⦇") ^^ r ^^ 
      (taggedText "hole" "⦈")) 

  let rec of_hexp e = match e with 
    | HExp.Asc (e', tau) -> of_Asc (of_hexp e') (of_htype tau)
    | HExp.Var x -> term "Var" (var x)
    | HExp.Let (x, e, e') -> of_Let x (of_hexp e) (of_hexp e')
    | HExp.Lam (x, e') -> of_Lam x (of_hexp e')
    | HExp.Ap (e1, e2) -> of_Ap (of_hexp e1) (of_hexp e2)
    | HExp.NumLit n -> 
      term "NumLit" (
        taggedText "number" (string_of_int n))
    | HExp.Plus (e1, e2) -> of_Plus (of_hexp e1) (of_hexp e2)
    | HExp.Inj (side, e) -> of_Inj side (of_hexp e)
    | HExp.Case (e1, (x, e2), (y, e3)) -> 
      of_Case (of_hexp e1) x (of_hexp e2) y (of_hexp e3)
    | HExp.EmptyHole -> 
      term "EmptyHole" (taggedText "hole" "⦇⦈")
    | HExp.NonEmptyHole e -> of_NonEmptyHole (of_hexp e)

  let rec of_zexp ze = match ze with 
    | ZExp.CursorE e -> 
      term "cursor" (of_hexp e)
    | ZExp.LeftAsc (ze, e) -> of_Asc (of_zexp ze) (of_htype e)
    | ZExp.RightAsc (e, ze) -> of_Asc (of_hexp e) (of_ztype ze)
    | ZExp.LetZ1 (x, ze, e) -> of_Let x (of_zexp ze) (of_hexp e)
    | ZExp.LetZ2 (x, e, ze) -> of_Let x (of_hexp e) (of_zexp ze)
    | ZExp.LamZ (x, ze) -> of_Lam x (of_zexp ze)
    | ZExp.LeftAp (ze, e) -> of_Ap (of_zexp ze) (of_hexp e)
    | ZExp.RightAp (e, ze) -> of_Ap (of_hexp e) (of_zexp ze)
    | ZExp.LeftPlus (ze, e) -> of_Plus (of_zexp ze) (of_hexp e)
    | ZExp.RightPlus (e, ze) -> of_Plus (of_hexp e) (of_zexp ze)
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

