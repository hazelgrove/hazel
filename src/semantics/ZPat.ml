type cursor_side = SemanticsCommon.cursor_side

type opseq_surround = (UHPat.t, UHPat.op) OperatorSeq.opseq_surround
type opseq_prefix = (UHPat.t, UHPat.op) OperatorSeq.opseq_prefix
type opseq_suffix = (UHPat.t, UHPat.op) OperatorSeq.opseq_suffix

type t =
| CursorP of cursor_side * UHPat.t
| ParenthesizedZ of t
| Deeper of err_status * t'
and t' = 
| InjZ of inj_side * t
| OpSeqZ of UHPat.skel_t * t * opseq_surround

let bidelimit zp = 
  begin match zp with 
  | CursorP(cursor_side, p) -> 
    CursorP(cursor_side, (UHPat.bidelimit p))
  | ParenthesizedZ _ 
  | Deeper(_, (InjZ(_, _)))
  (* | Deeper _ (ListLitZ _) *) 
      -> zp
  | Deeper(_, (OpSeqZ(_, _, _))) -> ParenthesizedZ zp
  end

(* helper function for constructing a new empty hole *)
let new_EmptyHole (u_gen : MetaVarGen.t) : t * MetaVarGen.t =
  let (hole, u_gen) = UHPat.new_EmptyHole(u_gen) in  
  (CursorP(Before, hole), u_gen)

let rec set_inconsistent
  (u : MetaVar.t)
  (zp : t)
  : t =
    begin match zp with 
    | CursorP(cursor_side, p) -> 
      let p = UHPat.set_inconsistent u p in 
      (CursorP(cursor_side, p))
    | Deeper(_, zp') -> 
      Deeper(InHole(TypeInconsistent, u), zp')
    | ParenthesizedZ zp1 -> 
      ParenthesizedZ (set_inconsistent u zp1)
    end

let rec make_inconsistent 
  (u_gen : MetaVarGen.t)
  (zp : t) 
  : (t * MetaVarGen.t) = 
    begin match zp with 
    | CursorP(cursor_side, p) -> 
      let (p, u_gen) = UHPat.make_inconsistent u_gen p in  
      (CursorP(cursor_side, p), u_gen)
    | Deeper(InHole(TypeInconsistent, _), _) -> 
      (zp, u_gen)
    | Deeper(_, zp') -> 
      let (u, u_gen) = MetaVarGen.next u_gen in 
      (Deeper(InHole(TypeInconsistent, u), zp'), u_gen)
    | ParenthesizedZ zp1 -> 
      let (zp1, u_gen) = make_inconsistent u_gen zp1 in 
      (ParenthesizedZ zp1, u_gen)
    end

let rec erase (zp : ZPat.t) : UHPat.t = 
  begin match zp with 
  | ZPat.CursorP(_, p) -> p
  | ZPat.Deeper(err_status, zp') -> UHPat.Pat(err_status, erase' zp')
  | ZPat.ParenthesizedZ zp -> UHPat.Parenthesized (erase zp)
  end
and erase' (zp' : ZPat.t') : UHPat.t' = 
  begin match zp' with 
  | ZPat.InjZ(side, zp1) -> UHPat.Inj(side, erase zp1)
  (* | ZPat.ListLitZ zps -> UHPat.ListLit (ZList.erase zps erase) *)
  | ZPat.OpSeqZ(skel, zp1, surround) -> 
    let p1 = erase zp1 in 
    UHPat.OpSeq(skel, (OperatorSeq.opseq_of_exp_and_surround p1 surround))
  end

let place_Before (p : UHPat.t) : t = 
  begin match p with 
  | UHPat.Parenthesized _ 
  | UHPat.Pat(_, (UHPat.EmptyHole _))
  | UHPat.Pat(_, UHPat.Wild)
  | UHPat.Pat(_, (UHPat.Var _))
  | UHPat.Pat(_, (UHPat.NumLit _))
  | UHPat.Pat(_, (UHPat.BoolLit _))
  | UHPat.Pat(_, (UHPat.Inj(_, _))) 
  (* | UHPat.Pat _ (UHPat.ListLit _) -> *) 
  | UHPat.Pat(_, UHPat.ListNil) -> 
    CursorP(Before, p)
  | UHPat.Pat(err, (UHPat.OpSeq(skel, seq))) -> 
    let (p0, suffix) = OperatorSeq.split0 seq in 
    let surround = OperatorSeq.EmptyPrefix(suffix) in  
    Deeper(err, (OpSeqZ(skel, (CursorP(Before, p0)), surround)))
  end

let place_After (p : UHPat.t) : t = 
  begin match p with 
  | UHPat.Parenthesized _ 
  | UHPat.Pat(_, (UHPat.EmptyHole _))
  | UHPat.Pat(_, UHPat.Wild)
  | UHPat.Pat(_, (UHPat.Var _))
  | UHPat.Pat(_, (UHPat.NumLit _))
  | UHPat.Pat(_, (UHPat.BoolLit _))
  | UHPat.Pat(_, (UHPat.Inj(_, _))) 
  (* | UHPat.Pat _ (UHPat.ListLit _) *) 
  | UHPat.Pat(_, UHPat.ListNil) -> CursorP(After, p)
  | UHPat.Pat(err, UHPat.OpSeq(skel, seq)) -> 
    let (p0, prefix) = OperatorSeq.split_tail seq in 
    let surround = OperatorSeq.EmptySuffix(prefix) in  
    Deeper(err, (OpSeqZ(skel, (CursorP(After, p0)), surround)))
  end

