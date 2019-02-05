type cursor_side = SemanticsCommon.cursor_side

type opseq_surround = (UHTyp.t, UHTyp.op) OperatorSeq.opseq_surround
type opseq_prefix = (UHTyp.t, UHTyp.op) OperatorSeq.opseq_prefix
type opseq_suffix = (UHTyp.t, UHTyp.op) OperatorSeq.opseq_suffix

type t =
| CursorT of cursor_side * UHTyp.t
| ParenthesizedZ of t
| ListZ of t
| OpSeqZ of UHTyp.skel_t * t * opseq_surround

let place_Before (uty : UHTyp.t) : t = 
  begin match uty with 
  | UHTyp.Hole
  | UHTyp.Parenthesized _
  | UHTyp.Unit
  | UHTyp.Num
  | UHTyp.Bool
  | UHTyp.List _ -> CursorT(Before, uty)
  | UHTyp.OpSeq(skel, seq) -> 
    let (uty0, suffix) = OperatorSeq.split0 seq in 
    let surround = OperatorSeq.EmptyPrefix(suffix) in  
    OpSeqZ(skel, (CursorT(Before, uty0)), surround)
  end

let place_After (uty : UHTyp.t) : t = 
  begin match uty with 
  | UHTyp.Hole 
  | UHTyp.Parenthesized _
  | UHTyp.Unit
  | UHTyp.Num
  | UHTyp.Bool 
  | UHTyp.List _ -> CursorT(After, uty)
  | UHTyp.OpSeq(skel, seq) -> 
    let (uty0, prefix) = OperatorSeq.split_tail seq in 
    let surround = OperatorSeq.EmptySuffix(prefix) in  
    OpSeqZ(skel, (CursorT(After, uty0)), surround)
  end

let rec erase (zty : t) : UHTyp.t =
  begin match zty with
  | CursorT(_, ty) -> ty
  | ParenthesizedZ zty1 -> UHTyp.Parenthesized (erase zty1)
  | ListZ zty1 -> UHTyp.List (erase zty1) 
  | OpSeqZ(skel, zty1, surround) -> 
    let uty1 = erase zty1 in 
    UHTyp.OpSeq(skel, 
      (OperatorSeq.opseq_of_exp_and_surround uty1 surround))
  end

