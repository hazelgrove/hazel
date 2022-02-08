/* Action_Exp */

// let%test "empty to non-empty type hole in let pattern annotation" = {
let f = () => {
  let ze =
    {|
(()
 (LetLineZP
  (ZOpSeq
   (Placeholder 0)
   (ZOperand
    (TypeAnnZA
     NotInHole
     (EmptyHole 5)
     (ZOpSeq
      (Placeholder 0)
      (ZOperand (CursorT (OnDelim 0 Before) Hole) (E E))))
    (E E)))
  ((ExpLine (OpSeq (Placeholder 0) (S (EmptyHole 4) E)))))
 ((ExpLine (OpSeq (Placeholder 0) (S (EmptyHole 6) E)))))
|}
    |> Sexplib.Sexp.of_string
    |> ZExp.t_of_sexp;
  let action = Action.Construct(SChar("I"));
  let edit_state = (ze, HTyp.Hole, 7);
  switch (Action_Exp.syn_perform(Contexts.empty, action, edit_state)) {
  | Failed
  | CursorEscaped(_) => false
  | Succeeded(result) =>
    failwith(SexpUtil.show(Action_Exp.sexp_of_syn_done(result)))
  };
};

let g = () => {
  let ze =
    {|
(()
  (ExpLineZ
   (ZOpSeq
    (BinOp NotInHole Space (Placeholder 0) (Placeholder 1))
    (ZOperand
     (CursorE (OnDelim 0 Before) (EmptyHole 2))
     ((A
       Space
       (S
        (Lam
         NotInHole
         (OpSeq (Placeholder 0) (S (Var NotInHole NotInVarHole x) E))
         ((ExpLine (OpSeq (Placeholder 0) (S (EmptyHole 0) E)))))
        E))
      E))))
  ())
      |}
    |> Sexplib.Sexp.of_string
    |> ZExp.t_of_sexp;
  let action = Action.Construct(SChar("x"));
  let edit_state = (ze, HTyp.Hole, 7);
  switch (Action_Exp.syn_perform(Contexts.empty, action, edit_state)) {
  | Failed
  | CursorEscaped(_) => false
  | Succeeded(result) =>
    failwith(SexpUtil.show(Action_Exp.sexp_of_syn_done(result)))
  };
};

let%test "g" = g();

/* Action_Pat */

// let%test "" = {
//   Action_Pat.
