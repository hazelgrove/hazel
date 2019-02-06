type op =
| Arrow
| Prod
| Sum

type skel_t = op Skel.t

type t =
| Parenthesized of t
| Hole
| Unit
| Num
| Bool
| List of t
| OpSeq of skel_t * (t, op) OperatorSeq.opseq

type opseq = (t, op) OperatorSeq.opseq

let bidelimited = function
| Hole
| Unit
| Num
| Bool
| Parenthesized _ -> true
| List ty -> true
| OpSeq (_, _) -> false

let rec well_formed uty =
  match uty with
  | Hole -> true
  | Unit -> true
  | Num -> true
  | Bool -> true
  | Parenthesized uty1 -> well_formed uty1
  | List uty1 -> well_formed uty1
  | OpSeq (skel, seq) ->
    (* NOTE: does not check that skel is the valid parse of seq *)
    well_formed_skel skel seq
and well_formed_skel skel seq =
  match skel with
  | Skel.Placeholder n ->
    begin match OperatorSeq.seq_nth n seq with
    | Some uty_n ->
      bidelimited uty_n && well_formed uty_n
    | None -> false
    end
  | Skel.BinOp (NotInHole, _, skel1, skel2) ->
    well_formed_skel skel1 seq
    && well_formed_skel skel2 seq
  | Skel.BinOp ((InHole(TypeInconsistent, _)), _, _, _) -> false (* no type-level non-empty holes *)
  | Skel.BinOp ((InHole(WrongLength, _)), _, _, _) -> false (* the type is assumed to be the true length *)

(* TODO fix this to only parenthesize when necessary *)
let rec contract ty =
  let mk_opseq = fun op' a b ->
    let ph = fun n -> Skel.Placeholder n in
    let skel = Skel.BinOp (NotInHole, op', (ph 0), (ph 1)) in
    Parenthesized (OpSeq (skel, (OperatorSeq.ExpOpExp (a, op', b))))
    (* Save it for another day
    match (a, b) with
      | (OpSeq skelA opseqA, OpSeq skelB opseqB) ->
      | (OpSeq skelA opseqA, _) ->
      | (_, OpSeq skelB opseqB) ->
      | (_, _) ->
        OpSeq (Skel.BinOp NotInHole op' ?? ??) (OperatorSeq.ExpOpExp a op' b)
    end
    *)
  in
  (match ty with
   | HTyp.Hole -> Hole
   | HTyp.Unit -> Unit
   | HTyp.Num -> Num
   | HTyp.Bool -> Bool
   | HTyp.Arrow (ty1, ty2) -> mk_opseq Arrow (contract ty1) (contract ty2)
   | HTyp.Prod (ty1, ty2) -> mk_opseq Prod (contract ty1) (contract ty2)
   | HTyp.Sum (ty1, ty2) -> mk_opseq Sum (contract ty1) (contract ty2)
   | HTyp.List ty1 -> List (contract ty1))

let rec expand uty =
  match uty with
  | Hole -> HTyp.Hole
  | Unit -> HTyp.Unit
  | Num -> HTyp.Num
  | Bool -> HTyp.Bool
  | Parenthesized uty1 -> expand uty1
  | List uty1 -> HTyp.List (expand uty1)
  | OpSeq (skel, seq) -> expand_skel skel seq
and expand_skel skel seq =
  match skel with
  | Skel.Placeholder n ->
    begin match OperatorSeq.seq_nth n seq with
    | Some uty_n -> expand uty_n
    | None -> HTyp.Hole (* should never happen *)
    end
  | Skel.BinOp (_, Arrow, skel1, skel2) ->
    let uty1 = expand_skel skel1 seq in
    let uty2 = expand_skel skel2 seq in
    HTyp.Arrow (uty1, uty2)
  | Skel.BinOp (_, Prod, skel1, skel2) ->
    let uty1 = expand_skel skel1 seq in
    let uty2 = expand_skel skel2 seq in
    HTyp.Prod (uty1, uty2)
  | Skel.BinOp (_, Sum, skel1, skel2) ->
    let uty1 = expand_skel skel1 seq in
    let uty2 = expand_skel skel2 seq in
    HTyp.Sum (uty1, uty2)
