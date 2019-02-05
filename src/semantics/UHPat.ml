module UHPat =
 struct
  type op =
  | Comma
  | Space
  | Cons

  let is_Space = function
  | Space -> true
  | _ -> false

  type skel_t = op Skel.t

  type t =
  | Pat of err_status * t'
  | Parenthesized of t
  and t' =
  | EmptyHole of MetaVar.t
  | Wild
  | Var of Var.t
  | NumLit of int
  | BoolLit of bool
  | Inj of inj_side * t
  | ListNil
  (* | ListLit : list(t) -> t' *)
  | OpSeq of skel_t * (t, op) OperatorSeq.opseq

  type opseq = (t, op) OperatorSeq.opseq

  let rec get_tuple skel1 skel2 =
    match skel2 with
    | Skel.BinOp (_, Comma, skel21, skel22) ->
      skel1::(get_tuple skel21 skel22)
    | Skel.BinOp (_, _, _, _)
    | Skel.Placeholder _ ->
      [skel1; skel2]

  let rec make_tuple err = function
  | [skel1; skel2] ->
    Some (Skel.BinOp (err, Comma, skel1, skel2))
  | skel1::skels ->
    match make_tuple NotInHole skels with
    | None -> None
    | Some skel2 -> Some (Skel.BinOp (err, Comma, skel1, skel2))
  | nil -> None

  (* bidelimited patterns are those that don't have
   * sub-patterns at their outer left or right edge
   * in the concrete syntax *)
  let bidelimited = function
  | Pat (_, (EmptyHole _))
  | Pat (_, Wild)
  | Pat (_, (Var _))
  | Pat (_, (NumLit _))
  | Pat (_, (BoolLit _))
  | Pat (_, (Inj (_, _)))
  | Pat (_, ListNil)
  (* | Pat _ (ListLit _) *)
  | Parenthesized _ -> true
  | Pat (_, (OpSeq (_, _))) -> false

  (* if p is not bidelimited, bidelimit e parenthesizes it *)
  let bidelimit p =
    if bidelimited p then p else Parenthesized p

  (* helper function for constructing a new empty hole *)
  let new_EmptyHole u_gen =
    let u, u_gen = MetaVarGen.next u_gen in
    (Pat (NotInHole, (EmptyHole u))), u_gen

  let is_EmptyHole = function
  match p with
  | Pat (_, (EmptyHole _)) -> true
  | _ -> false

  let rec set_inconsistent u = function
  | Pat (_, p') -> Pat ((InHole (TypeInconsistent, u)), p')
  | Parenthesized p1 -> Parenthesized (set_inconsistent u p1)

  (* put p in a new hole, if it is not already in a hole *)
  let rec make_inconsistent u_gen p = match p with
    match p with
    | Pat NotInHole p'
    | Pat ((InHole (WrongLength, _)), p') ->
      let u, u_gen = MetaVarGen.next u_gen in
      (Pat ((InHole (TypeInconsistent, u)), p'), u_gen)
    | Pat ((InHole (TypeInconsistent, _)), _) -> (p, u_gen)
    | Parenthesized p1 ->
      match make_inconsistent u_gen p1 with
      | (p1, u_gen) -> (Parenthesized p1, u_gen)
 end
