open SemanticsCommon
open Util

type op_shape =  
| SPlus
| STimes
| SLessThan
| SSpace
| SComma
| SArrow
| SVBar
| SCons

let ty_op_of (os : op_shape) : UHTyp.op option = 
  begin match os with 
  | SArrow -> Some UHTyp.Arrow
  | SComma -> Some UHTyp.Prod
  | SVBar -> Some UHTyp.Sum
  | SPlus
  | STimes
  | SLessThan 
  | SSpace
  | SCons -> None
  end

let op_shape_of_ty_op (op : UHTyp.op) : op_shape = 
  begin match op with 
  | UHTyp.Arrow -> SArrow
  | UHTyp.Prod -> SComma
  | UHTyp.Sum -> SVBar
  end

let pat_op_of (os : op_shape) : UHPat.op option =
  begin match os with 
  | SComma -> Some UHPat.Comma
  | SSpace -> Some UHPat.Space 
  | SCons -> Some UHPat.Cons
  | SPlus
  | STimes
  | SLessThan
  | SArrow 
  | SVBar -> None
  end

let op_shape_of_pat_op (op : UHPat.op) : op_shape = 
  begin match op with 
  | UHPat.Comma -> SComma
  | UHPat.Space -> SSpace
  | UHPat.Cons -> SCons
  end

let exp_op_of (os : op_shape) : UHExp.op option = 
  begin match os with 
  | SPlus -> Some UHExp.Plus
  | STimes -> Some UHExp.Times
  | SLessThan -> Some UHExp.LessThan
  | SSpace -> Some UHExp.Space
  | SComma -> Some UHExp.Comma
  | SCons -> Some UHExp.Cons
  | SArrow 
  | SVBar -> None
  end

let op_shape_of_exp_op (op : UHExp.op) : op_shape = 
  begin match op with 
  | UHExp.Plus -> SPlus
  | UHExp.Times -> STimes
  | UHExp.LessThan -> SLessThan
  | UHExp.Space -> SSpace
  | UHExp.Comma -> SComma
  | UHExp.Cons -> SCons
  end

type shape =
| SParenthesized
(* type shapes *)
| SNum
| SBool
| SList
(* expression shapes *)
| SAsc 
| SLet 
| SVar of Var.t * ZExp.cursor_side
| SLam
| SNumLit of int * ZExp.cursor_side 
| SBoolLit of bool * ZExp.cursor_side 
| SListNil
| SInj of inj_side 
| SCase 
| SRule 
| SOp of op_shape 
| SApPalette of PaletteName.t 
(* pattern-only shapes *)
| SWild 

type t =
| MoveTo of Path.t
| MoveToNextHole
| MoveToPrevHole
| UpdateApPalette of PaletteSerializedModel.t UHExp.HoleRefs.m_hole_ref
| Delete
| Backspace 
| Construct of shape

let make_ty_OpSeqZ 
  (zty0 : ZTyp.t) (surround : ZTyp.opseq_surround)
  : ZTyp.t = 
    let uty0 = ZTyp.erase zty0 in 
    let seq = OperatorSeq.opseq_of_exp_and_surround uty0 surround in 
    let skel = Associator.associate_ty seq in 
    ZTyp.OpSeqZ(skel, zty0, surround)

let rec perform_ty (a : t) (zty : ZTyp.t) : ZTyp.t option =
  begin match (a, zty) with
  (* Movement *)
  | (MoveTo path, _) -> 
    let ty = ZTyp.erase zty in 
    Path.follow_ty path ty
  | (MoveToPrevHole, _) ->
    begin match Path.prev_hole_path_ty zty with
    | None -> None
    | Some path -> perform_ty (MoveTo path) zty
    end
  | (MoveToNextHole, _) ->
    begin match Path.next_hole_path_ty zty with
    | None -> None
    | Some path ->
      (* [debug] let path = Helper.log_path path in *)
      perform_ty (MoveTo path) zty
    end
  (* Backspace and Delete *)
  | (Backspace, ZTyp.CursorT(After, uty)) 
  | (Backspace, ZTyp.CursorT(In _, uty)) -> 
    Some (ZTyp.CursorT(Before, UHTyp.Hole))
  | (Backspace, ZTyp.CursorT(Before, _)) -> None
  | (Delete, ZTyp.CursorT(Before, uty)) 
  | (Delete, ZTyp.CursorT(In _, uty)) -> 
    begin match uty with 
    | UHTyp.Hole -> 
      Some (ZTyp.CursorT(After, uty))
    | _ -> 
      Some (ZTyp.CursorT(Before, UHTyp.Hole))
    end
  | (Delete, ZTyp.CursorT(After, uty)) -> None
  | (Backspace, 
      ZTyp.OpSeqZ(_, 
        ((ZTyp.CursorT(Before, uty0)) as zty0), 
        surround)) -> 
    begin match surround with 
    | OperatorSeq.EmptyPrefix _ -> None
    | OperatorSeq.EmptySuffix prefix -> 
      begin match prefix with 
      | OperatorSeq.ExpPrefix(uty1, op1) -> 
        begin match uty0 with 
        | UHTyp.Hole -> 
          (* uty1 op1 |_ -> uty1| *)
          Some (ZTyp.CursorT(After, uty1))
        | _ -> 
          (* uty1 op1 |uty0 -> |uty0 *)
          Some zty0
        end
      | OperatorSeq.SeqPrefix(seq1, op1) -> 
        let (uty1, prefix') = OperatorSeq.split_tail seq1 in 
        begin match uty0 with 
        | UHTyp.Hole -> 
          (* prefix' uty1 op1 |_ --> prefix' uty1| *)
          let surround' = OperatorSeq.EmptySuffix prefix' in 
          let ze1 = ZTyp.CursorT(After, uty1) in 
          Some (make_ty_OpSeqZ ze1 surround')
        | _ -> 
          (* prefix' uty1 op |uty0 --> prefix' |uty0 *)
          let surround' = OperatorSeq.EmptySuffix prefix' in 
          Some (make_ty_OpSeqZ zty0 surround')
        end
      end
    | OperatorSeq.BothNonEmpty(prefix, suffix) -> 
      begin match prefix with 
      | OperatorSeq.ExpPrefix(uty1, op1) -> 
        begin match uty0 with 
        | UHTyp.Hole -> 
          (* uty1 op1 |_ suffix -> uty1| suffix *)
          let surround' = OperatorSeq.EmptyPrefix suffix in 
          let zty1 = ZTyp.CursorT(After, uty1) in 
          Some (make_ty_OpSeqZ zty1 surround')
        | _ -> 
          (* uty1 op1 |uty0 suffix -> |uty0 suffix *)
          let surround' = OperatorSeq.EmptyPrefix suffix in 
          Some (make_ty_OpSeqZ zty0 surround')
        end
      | OperatorSeq.SeqPrefix(seq1, op1) -> 
        let (uty1, prefix') = OperatorSeq.split_tail seq1 in 
        begin match uty0 with 
        | UHTyp.Hole -> 
          (* prefix' uty1 op1 |_ suffix --> prefix' uty1| suffix *)
          let surround' = OperatorSeq.BothNonEmpty(prefix', suffix) in 
          let ze1 = ZTyp.CursorT(After, uty1) in 
          Some (make_ty_OpSeqZ ze1 surround')
        | _ -> 
          (* prefix' uty1 op |uty0 suffix --> prefix' |uty0 suffix *)
          let surround' = OperatorSeq.BothNonEmpty(prefix', suffix) in 
          Some (make_ty_OpSeqZ zty0 surround')
        end
      end
    end
  | (Delete, 
      ZTyp.OpSeqZ(_, 
        ((ZTyp.CursorT(After, uty0)) as zty0), 
        surround)) -> 
    begin match surround with 
    | OperatorSeq.EmptySuffix _ -> None
    | OperatorSeq.EmptyPrefix suffix -> 
      begin match suffix with 
      | OperatorSeq.ExpSuffix(op1, uty1) -> 
        begin match uty0 with 
        | UHTyp.Hole -> 
          (* _| op1 uty1 -> |uty1 *)
          Some (ZTyp.CursorT(Before, uty1))
        | _ -> 
          (* uty0| op1 uty0 -> uty0| *)
          Some zty0
        end
      | OperatorSeq.SeqSuffix(op1, seq1) -> 
        let (uty1, suffix') = OperatorSeq.split0 seq1 in 
        begin match uty0 with 
        | UHTyp.Hole -> 
          (* _| op1 uty1 suffix' --> |uty1 suffix' *)
          let surround' = OperatorSeq.EmptyPrefix suffix' in 
          let ze1 = ZTyp.CursorT(Before, uty1) in 
          Some (make_ty_OpSeqZ ze1 surround')
        | _ -> 
          (* uty0| op1 uty1 suffix' --> uty0| suffix' *)
          let surround' = OperatorSeq.EmptyPrefix suffix' in 
          Some (make_ty_OpSeqZ zty0 surround')
        end
      end
    | OperatorSeq.BothNonEmpty(prefix, suffix) -> 
      begin match suffix with 
      | OperatorSeq.ExpSuffix(op1, uty1) -> 
        begin match uty0 with 
        | UHTyp.Hole -> 
          (* prefix _| op1 uty1 -> prefix |uty1 *)
          let surround' = OperatorSeq.EmptySuffix prefix in 
          let zty1 = ZTyp.CursorT(Before, uty1) in 
          Some (make_ty_OpSeqZ zty1 surround')
        | _ -> 
          (* prefix uty0| op1 uty0 -> prefix uty0| *)
          let surround' = OperatorSeq.EmptySuffix prefix in 
          Some (make_ty_OpSeqZ zty0 surround')
        end
      | OperatorSeq.SeqSuffix(op1, seq1) -> 
        let (uty1, suffix') = OperatorSeq.split0 seq1 in 
        begin match uty0 with 
        | UHTyp.Hole -> 
          (* prefix _| op1 uty1 suffix' --> prefix |uty1 suffix' *)
          let surround' = OperatorSeq.BothNonEmpty(prefix, suffix') in 
          let ze1 = ZTyp.CursorT(Before, uty1) in 
          Some (make_ty_OpSeqZ ze1 surround')
        | _ -> 
          (* prefix uty0| op1 uty1 suffix' --> prefix uty0| suffix' *)
          let surround' = OperatorSeq.BothNonEmpty(prefix, suffix') in 
          Some (make_ty_OpSeqZ zty0 surround')
        end
      end
    end
  (* Construction *)
  | (Construct SParenthesized, ZTyp.CursorT(_, _)) -> 
    Some (ZTyp.ParenthesizedZ zty) 
  | (Construct SNum, ZTyp.CursorT(_, UHTyp.Hole)) -> 
    Some (ZTyp.CursorT(After, UHTyp.Num))
  | (Construct SNum, ZTyp.CursorT(_, _)) -> None
  | (Construct SBool, ZTyp.CursorT(_, UHTyp.Hole)) -> 
    Some (ZTyp.CursorT(After, UHTyp.Bool))
  | (Construct SBool, ZTyp.CursorT(_, _)) -> None
  | (Construct SList, ZTyp.CursorT(_, ty1)) -> 
    Some (ZTyp.ListZ zty)
  | (Construct (SOp os), ZTyp.CursorT(After, uty1)) 
  | (Construct (SOp os), ZTyp.CursorT(In _, uty1)) -> 
    begin match ty_op_of os with
    | None -> None
    | Some op -> 
      let surround = OperatorSeq.EmptySuffix (OperatorSeq.ExpPrefix(uty1, op)) in 
      let zty0 = ZTyp.CursorT(Before, UHTyp.Hole) in 
      Some (make_ty_OpSeqZ zty0 surround)
    end
  | (Construct (SOp os), ZTyp.CursorT(Before, uty1)) -> 
    begin match ty_op_of os with
    | None -> None
    | Some op -> 
      let surround = OperatorSeq.EmptyPrefix (OperatorSeq.ExpSuffix(op, uty1)) in 
      let zty0 = ZTyp.CursorT(Before, UHTyp.Hole) in 
      Some (make_ty_OpSeqZ zty0 surround)
    end
  | (Construct (SOp os), 
      ZTyp.OpSeqZ(_,  
        ((ZTyp.CursorT(After, uty0))),
        surround))
  | (Construct (SOp os), 
      ZTyp.OpSeqZ(_, 
        (ZTyp.CursorT(In _, uty0)),
        surround)) -> 
    begin match ty_op_of os with
    | None -> None
    | Some op -> 
      begin match surround with 
      | OperatorSeq.EmptyPrefix suffix -> 
        (* zty0| suffix -> uty0 op |_ suffix *)
        let prefix' = OperatorSeq.ExpPrefix(uty0, op) in 
        let surround' = OperatorSeq.BothNonEmpty(prefix', suffix) in 
        let zty0' = ZTyp.CursorT(Before, UHTyp.Hole) in 
        Some (make_ty_OpSeqZ zty0' surround')
      | OperatorSeq.EmptySuffix prefix -> 
        (* prefix zty0| -> prefix uty0 op |_ *)
        let prefix' = OperatorSeq.prefix_append_exp prefix uty0 op in 
        let surround' = OperatorSeq.EmptySuffix prefix' in 
        let zty0' = ZTyp.CursorT(Before, UHTyp.Hole) in 
        Some (make_ty_OpSeqZ zty0' surround')
      | OperatorSeq.BothNonEmpty(prefix, suffix) -> 
        (* prefix zty0| suffix -> prefix uty0 op |_ suffix *)
        let prefix' = OperatorSeq.prefix_append_exp prefix uty0 op in 
        let surround' = OperatorSeq.BothNonEmpty(prefix', suffix) in 
        let zty0' = ZTyp.CursorT(Before, UHTyp.Hole) in 
        Some (make_ty_OpSeqZ zty0' surround')
      end
    end
  | (Construct (SOp os), 
      ZTyp.OpSeqZ(_,  
        ((ZTyp.CursorT(Before, uty0))),
        surround)) -> 
    begin match ty_op_of os with
    | None -> None
    | Some op -> 
      begin match surround with 
      | OperatorSeq.EmptyPrefix suffix -> 
        (* |zty0 suffix -> |_ op uty0 suffix *)
        let suffix' = OperatorSeq.suffix_prepend_exp suffix op uty0 in 
        let surround' = OperatorSeq.EmptyPrefix suffix' in 
        let zty0' = ZTyp.CursorT(Before, UHTyp.Hole) in 
        Some (make_ty_OpSeqZ zty0' surround')
      | OperatorSeq.EmptySuffix prefix -> 
        (* prefix |zty0 -> prefix |_ op uty0 *)
        let suffix' = OperatorSeq.ExpSuffix(op, uty0) in 
        let surround' = OperatorSeq.BothNonEmpty(prefix, suffix') in 
        let zty0' = ZTyp.CursorT(Before, UHTyp.Hole) in 
        Some (make_ty_OpSeqZ zty0' surround')
      | OperatorSeq.BothNonEmpty(prefix, suffix) -> 
        (* prefix |zty0 suffix -> prefix |_ op uty0 suffix *)
        let suffix' = OperatorSeq.suffix_prepend_exp suffix op uty0 in 
        let surround' = OperatorSeq.BothNonEmpty(prefix, suffix') in 
        let zty0' = ZTyp.CursorT(Before, UHTyp.Hole) in 
        Some (make_ty_OpSeqZ zty0' surround')
      end
    end
  (* Zipper Cases *)
  | (a, ZTyp.ParenthesizedZ zty1) -> 
    begin match perform_ty a zty1 with 
    | Some zty1' -> 
      Some (ZTyp.ParenthesizedZ zty1')
    | None -> None
    end
  | (a, ZTyp.ListZ zty1) -> 
    begin match perform_ty a zty1 with 
    | Some zty1 -> 
      Some (ZTyp.ListZ zty1)
    | None -> None
    end
  | (a, ZTyp.OpSeqZ(skel, zty0, surround)) -> 
    begin match perform_ty a zty0 with 
    | Some zty0' -> 
      Some (ZTyp.OpSeqZ(skel, zty0', surround))
    | None -> None
    end
  (* Invalid actions at the type level *)
  | (UpdateApPalette _, _)
  | (Construct SAsc, _)
  | (Construct SLet, _) 
  | (Construct (SVar(_, _)), _)
  | (Construct SLam, _) 
  | (Construct (SNumLit(_, _)), _)
  | (Construct (SBoolLit(_, _)), _) 
  | (Construct SListNil, _)
  | (Construct (SInj _), _) 
  | (Construct SCase, _) 
  | (Construct SRule, _)
  | (Construct (SApPalette _), _)
  | (Construct SWild, _) -> None
  end

let abs_perform_Backspace_Before_op
  (combine_for_Backspace_Space : 'e -> 'z -> 'z)
  (z_typecheck_fix_holes : Contexts.t -> MetaVarGen.t -> 'z -> 'm option)
  (make_and_typecheck_OpSeqZ : 
    Contexts.t -> MetaVarGen.t -> 
    'z -> ('e, 'op) OperatorSeq.opseq_surround ->
    'm option)
  (is_EmptyHole : 'e -> bool)
  (is_Space : 'op -> bool)
  (_Space : 'op)
  (_Cursor : cursor_side -> 'e -> 'z)
  (ctx : Contexts.t)
  (u_gen : MetaVarGen.t)
  (e0 : 'e)
  (ze0 : 'z)
  (surround : ('e, 'op) OperatorSeq.opseq_surround)
  : 'm option = 
    begin match surround with 
    | OperatorSeq.EmptyPrefix _ -> None
    | OperatorSeq.EmptySuffix prefix -> 
      begin match prefix with 
      | OperatorSeq.ExpPrefix(e1, op1) -> 
        (* e1 op1 |ze0 *)
        if is_Space op1 then 
          (* e1 |ze0 *)
          let ze0' = combine_for_Backspace_Space e1 ze0 in  
          z_typecheck_fix_holes ctx u_gen ze0' 
        else
          begin match (is_EmptyHole e1, is_EmptyHole e0) with 
          | (true, true) -> 
            (* _1 op1 |_0 --> _1| *)
            let ze0' = _Cursor After e1 in 
            z_typecheck_fix_holes ctx u_gen ze0'
          | (true, _) -> 
            (* _1 op1 |e0 --> |e0 *)
            z_typecheck_fix_holes ctx u_gen ze0
          | (false, true) -> 
            (* e1 op1 |_0 --> e1| *)
            let ze0' = _Cursor After e1 in 
            z_typecheck_fix_holes ctx u_gen ze0'
          | (false, false) -> 
            (* e1 op1 |ze0 --> e1 |ze0 *)
            let surround' = 
              OperatorSeq.EmptySuffix 
                (OperatorSeq.ExpPrefix(e1, _Space)) in 
            make_and_typecheck_OpSeqZ ctx u_gen ze0 surround' 
          end
      | OperatorSeq.SeqPrefix(seq1, op1) -> 
        (* seq1 op1 |ze0 *)
        begin match is_Space op1 with 
        | true ->
          (* seq1 |ze0 *)
          let (e1, prefix') = OperatorSeq.split_tail seq1 in 
          let surround' = OperatorSeq.EmptySuffix prefix' in 
          let ze0' = combine_for_Backspace_Space e1 ze0 in 
          make_and_typecheck_OpSeqZ ctx u_gen ze0' surround'
        | false -> 
          let (e1, prefix') = OperatorSeq.split_tail seq1 in 
          if is_EmptyHole e0 then 
            (* prefix' e1 op1 |_0 --> prefix' e1| *)
            let surround' = OperatorSeq.EmptySuffix prefix' in 
            let ze0' = _Cursor After e1 in 
            make_and_typecheck_OpSeqZ ctx u_gen ze0' surround'
          else if is_EmptyHole e1 then 
            (* prefix' _1 op1 |e0 --> prefix' |e0 *)
            let surround' = OperatorSeq.EmptySuffix prefix' in 
            make_and_typecheck_OpSeqZ ctx u_gen ze0 surround'
          else
            (* seq1 op1 |ze0 --> seq1 |ze0 *)
            let prefix' = OperatorSeq.SeqPrefix(seq1, _Space) in 
            let surround' = OperatorSeq.EmptySuffix prefix' in 
            make_and_typecheck_OpSeqZ ctx u_gen ze0 surround' 
        end
      end
    | OperatorSeq.BothNonEmpty(prefix, suffix) -> 
      begin match prefix with 
      | OperatorSeq.ExpPrefix(e1, op1) -> 
        (* e1 op1 |ze0 ...suffix *)
        begin match is_Space op1 with 
        | true -> 
          (* e1 |ze0 ...suffix *)
          let ze0' = combine_for_Backspace_Space e1 ze0 in  
          let surround' = OperatorSeq.EmptyPrefix suffix in 
          make_and_typecheck_OpSeqZ ctx u_gen ze0' surround'
        | false -> 
          if is_EmptyHole e0 then 
            (* e1 op1 |_0 suffix --> e1| suffix *)
            let surround' = OperatorSeq.EmptyPrefix suffix in 
            let ze0' = _Cursor After e1 in 
            make_and_typecheck_OpSeqZ ctx u_gen ze0' surround'
          else if is_EmptyHole e1 then 
            (* _1 op1 |e0 suffix --> |e0 suffix *)
            let surround' = OperatorSeq.EmptyPrefix suffix in 
            make_and_typecheck_OpSeqZ ctx u_gen ze0 surround'
          else
            (* e1 op1 |ze0 --> e1 |ze0 ...suffix *)
            let surround' = 
              OperatorSeq.BothNonEmpty( 
                (OperatorSeq.ExpPrefix(e1, _Space)),  
                suffix) in 
            make_and_typecheck_OpSeqZ ctx u_gen ze0 surround' 
        end
      | OperatorSeq.SeqPrefix(seq1, op1) -> 
        (* seq1 op1 |ze0 ...suffix *)
        begin match is_Space op1 with 
        | true ->
          (* seq1 |ze0 ...suffix *)
          let (e1, prefix') = OperatorSeq.split_tail seq1 in 
          let ze0' =  combine_for_Backspace_Space e1 ze0 in  
          let surround' = OperatorSeq.BothNonEmpty(prefix', suffix) in 
          make_and_typecheck_OpSeqZ ctx u_gen ze0' surround'
        | false -> 
          let (e1, prefix') = OperatorSeq.split_tail seq1 in 
          if is_EmptyHole e0 then 
            (* prefix' e1 op1 |_0 suffix --> prefix' e1| suffix *)
            let surround' = OperatorSeq.BothNonEmpty(prefix', suffix) in 
            let ze0' = _Cursor After e1 in 
            make_and_typecheck_OpSeqZ ctx u_gen ze0' surround'
          else if is_EmptyHole e1 then 
            (* prefix' _1 op1 |e0 suffix --> prefix' |e0 suffix *)
            let surround' = OperatorSeq.BothNonEmpty(prefix', suffix) in 
            make_and_typecheck_OpSeqZ ctx u_gen ze0 surround'
          else 
            (* seq1 op1 |ze0 suffix --> seq1 |ze0 suffix *)
            let prefix' = OperatorSeq.SeqPrefix(seq1, _Space) in 
            let surround' = OperatorSeq.BothNonEmpty(prefix', suffix) in 
            make_and_typecheck_OpSeqZ ctx u_gen ze0 surround' 
        end
      end
    end      

let abs_perform_Delete_After_op
  (combine_for_Delete_Space : 'z -> 'e -> 'z)
  (z_typecheck_fix_holes : Contexts.t -> MetaVarGen.t -> 'z -> 'm option)
  (make_and_typecheck_OpSeqZ : 
    Contexts.t -> MetaVarGen.t -> 
    'z -> ('e, 'op) OperatorSeq.opseq_surround ->
    'm option)
  (is_EmptyHole : 'e -> bool)
  (is_Space : 'op -> bool)
  (_Space : 'op)
  (_Cursor : cursor_side -> 'e -> 'z)
  (ctx : Contexts.t)
  (u_gen : MetaVarGen.t)
  (e0 : 'e)
  (ze0 : 'z)
  (surround : ('e, 'op) OperatorSeq.opseq_surround)
  : 'm option = 
    begin match surround with 
    | OperatorSeq.EmptySuffix _ -> None (* precluded by pattern begin match above *)
    | OperatorSeq.EmptyPrefix suffix -> 
      begin match suffix with 
      | OperatorSeq.ExpSuffix(op, e1) -> 
        begin match is_Space op with 
        | true -> 
          let ze0' = combine_for_Delete_Space ze0 e1 in 
          z_typecheck_fix_holes ctx u_gen ze0'  
        | false -> 
          begin match (is_EmptyHole e0, is_EmptyHole e1) with 
          | (true, true) -> 
            (* _0| op _1 --> _0| *)
            z_typecheck_fix_holes ctx u_gen ze0
          | (true, false) -> 
            (* _0| op e1 --> |e1 *)
            let ze1 = _Cursor Before e1  in 
            z_typecheck_fix_holes ctx u_gen ze1
          | (false, true) -> 
            (* e0| op _ --> e0| *)
            z_typecheck_fix_holes ctx u_gen ze0
          | (false, false) -> 
            (* e0| op e1 --> e0| e1 *)
            let surround' = 
              OperatorSeq.EmptyPrefix 
                (OperatorSeq.ExpSuffix(_Space, e1)) in 
            make_and_typecheck_OpSeqZ ctx u_gen ze0 surround' 
          end
        end
      | OperatorSeq.SeqSuffix(op, seq) -> 
        begin match is_Space op with 
        | true -> 
          let (e, suffix') = OperatorSeq.split0 seq in
          let surround' = OperatorSeq.EmptyPrefix suffix' in 
          let ze0' = combine_for_Delete_Space ze0 e in 
          make_and_typecheck_OpSeqZ ctx u_gen ze0' surround'
        | false -> 
          let (e1, suffix') = OperatorSeq.split0 seq in 
          if is_EmptyHole e1 then 
            (* e0| op _ suffix' --> e0| suffix' *)
            let surround' = OperatorSeq.EmptyPrefix suffix' in 
            make_and_typecheck_OpSeqZ ctx u_gen ze0 surround'
          else if is_EmptyHole e0 then 
            (* _0| op e1 suffix' --> |e1 suffix' *)
            let surround' = OperatorSeq.EmptyPrefix suffix' in 
            let ze1 = _Cursor Before e1  in 
            make_and_typecheck_OpSeqZ ctx u_gen ze1 surround'
          else
            (* e0| op seq --> e0| seq *)
            let suffix' = OperatorSeq.SeqSuffix(_Space, seq) in 
            let surround' = OperatorSeq.EmptyPrefix suffix' in 
            make_and_typecheck_OpSeqZ ctx u_gen ze0 surround' 
        end
      end
    | OperatorSeq.BothNonEmpty(prefix, suffix) -> 
      begin match suffix with 
      | OperatorSeq.ExpSuffix(op, e1) -> 
        begin match is_Space op with 
        | true -> 
          let ze0' = combine_for_Delete_Space ze0 e1 in 
          let surround' = OperatorSeq.EmptySuffix prefix in 
          make_and_typecheck_OpSeqZ ctx u_gen ze0' surround'
        | false -> 
          if is_EmptyHole e1 then 
            (* prefix e0| op _ --> prefix e0| *)
            let surround' = OperatorSeq.EmptySuffix prefix in 
            make_and_typecheck_OpSeqZ ctx u_gen ze0 surround'
          else if is_EmptyHole e0 then 
            (* prefix _0| op e1 --> prefix |e1 *)
            let surround' = OperatorSeq.EmptySuffix prefix in 
            let ze1 = _Cursor Before e1 in 
            make_and_typecheck_OpSeqZ ctx u_gen ze1 surround'
          else
            (* prefix e0| op e1 --> e0| e1 *)
            let surround' = 
              OperatorSeq.BothNonEmpty(prefix, 
                (OperatorSeq.ExpSuffix(_Space, e1))) in 
            make_and_typecheck_OpSeqZ ctx u_gen ze0 surround'
        end
      | OperatorSeq.SeqSuffix(op, seq) -> 
        begin match is_Space op with 
        | true -> 
          let (e, suffix') = OperatorSeq.split0 seq in 
          let ze0' = combine_for_Delete_Space ze0 e in 
          let surround' = OperatorSeq.BothNonEmpty(prefix, suffix') in 
          make_and_typecheck_OpSeqZ ctx u_gen ze0' surround'
        | false -> 
          let (e1, suffix') = OperatorSeq.split0 seq in 
          if is_EmptyHole e1 then 
            (* prefix e0| op _ suffix' --> prefix e0| suffix' *)
            let surround' = OperatorSeq.BothNonEmpty(prefix, suffix') in 
            make_and_typecheck_OpSeqZ ctx u_gen ze0 surround'
          else if is_EmptyHole e0 then 
            (* prefix _0| op e1 suffix' --> prefix |e1 suffix' *)
            let surround' = OperatorSeq.BothNonEmpty(prefix, suffix') in 
            let ze1 = _Cursor Before e1 in 
            make_and_typecheck_OpSeqZ ctx u_gen ze1 surround'
          else
            (* prefix e| op seq --> e| seq *)
            let suffix' = OperatorSeq.SeqSuffix(_Space, seq) in 
            let surround' = OperatorSeq.BothNonEmpty(prefix, suffix') in 
            make_and_typecheck_OpSeqZ ctx u_gen ze0 surround'
        end
      end
    end

let abs_perform_Construct_SOp_After
  (bidelimit : 'e -> 'e)
  (new_EmptyHole : MetaVarGen.t -> ('z * MetaVarGen.t))
  (make_and_typecheck_OpSeqZ : 
    Contexts.t -> MetaVarGen.t -> 
    'z -> ('e, 'op) OperatorSeq.opseq_surround -> 
    'm option)
  (ctx : Contexts.t)
  (u_gen : MetaVarGen.t)
  (e : 'e) (op : 'op)
  : 'm option = 
    let e' = bidelimit e in 
    let prefix = OperatorSeq.ExpPrefix(e', op) in 
    let surround = OperatorSeq.EmptySuffix prefix in 
    let (ze0, u_gen) = new_EmptyHole u_gen in 
    make_and_typecheck_OpSeqZ ctx u_gen ze0 surround 

let abs_perform_Construct_SOp_Before
  (bidelimit : 'e -> 'e)
  (new_EmptyHole : MetaVarGen.t -> ('z * MetaVarGen.t))
  (make_and_typecheck_OpSeqZ : 
    Contexts.t -> MetaVarGen.t -> 
    'z -> ('e, 'op) OperatorSeq.opseq_surround -> 
    'm option)
  (ctx : Contexts.t)
  (u_gen : MetaVarGen.t)
  (e : 'e) (op : 'op)
  : 'm option = 
    let e' = bidelimit e in 
    let suffix = OperatorSeq.ExpSuffix(op, e') in 
    let surround = OperatorSeq.EmptyPrefix suffix in 
    let (ze0, u_gen) = new_EmptyHole u_gen in 
    make_and_typecheck_OpSeqZ ctx u_gen ze0 surround

let abs_perform_Construct_SOp_After_surround 
  (new_EmptyHole : MetaVarGen.t -> ('z * MetaVarGen.t))
  (make_and_typecheck_OpSeqZ : 
    Contexts.t -> MetaVarGen.t -> 
    'z -> ('e, 'op) OperatorSeq.opseq_surround -> 'm option)
  (is_Space : 'op -> bool)
  (_Space : 'op)
  (_Cursor : cursor_side -> 'e -> 'z)
  (ctx : Contexts.t)
  (u_gen : MetaVarGen.t)
  (e : 'e)
  (op : 'op)
  (surround : ('e, 'op) OperatorSeq.opseq_surround)
  : 'm option = 
    begin match surround with 
    | OperatorSeq.EmptySuffix prefix -> 
      let prefix' = OperatorSeq.prefix_append_exp prefix e op in 
      let surround' = OperatorSeq.EmptySuffix prefix' in 
      let (ze0, u_gen) = new_EmptyHole u_gen in 
      make_and_typecheck_OpSeqZ ctx u_gen ze0 surround' 
    | OperatorSeq.EmptyPrefix suffix -> 
      begin match suffix with 
      | OperatorSeq.ExpSuffix(op', e') -> 
        begin match is_Space op with 
        | true -> 
          (* e| op' e' --> e |_ op' e' *)
          let prefix' = OperatorSeq.ExpPrefix(e, op) in 
          let suffix' = OperatorSeq.ExpSuffix(op', e') in 
          let surround' = OperatorSeq.BothNonEmpty(prefix', suffix') in 
          let (ze0, u_gen) = new_EmptyHole u_gen in 
          make_and_typecheck_OpSeqZ ctx u_gen ze0 surround' 
        | false -> 
          begin match is_Space op' with 
          | true -> 
            (* e| e' --> e op |e' *)
            let prefix' = OperatorSeq.ExpPrefix(e, op) in 
            let surround' = OperatorSeq.EmptySuffix prefix' in 
            let ze0 = _Cursor Before e' in 
            make_and_typecheck_OpSeqZ ctx u_gen ze0 surround'  
          | false -> 
            (* e| op' e' --> e op |_ op' e' *)
            let prefix' = OperatorSeq.ExpPrefix(e, op) in 
            let suffix' = OperatorSeq.ExpSuffix(op', e') in 
            let surround' = OperatorSeq.BothNonEmpty(prefix', suffix') in 
            let (ze0, u_gen) = new_EmptyHole u_gen in 
            make_and_typecheck_OpSeqZ ctx u_gen ze0 surround' 
          end
        end
      | OperatorSeq.SeqSuffix(op', seq') -> 
        begin match is_Space op with 
        | true -> 
          (* e| seq' --> e |_ op' seq' *)
          let prefix' = OperatorSeq.ExpPrefix(e, op) in 
          let surround' = OperatorSeq.BothNonEmpty(prefix', suffix) in 
          let (ze0, u_gen) = new_EmptyHole u_gen in 
          make_and_typecheck_OpSeqZ ctx u_gen ze0 surround'
        | false -> 
          begin match is_Space op' with 
          | true -> 
            (* e| seq' --> e op |seq' *)
            let prefix' = OperatorSeq.ExpPrefix(e, op) in 
            let (e0', suffix') = OperatorSeq.split0 seq' in 
            let surround' = OperatorSeq.BothNonEmpty(prefix', suffix') in 
            let ze0 = _Cursor Before e0' in 
            make_and_typecheck_OpSeqZ ctx u_gen ze0 surround' 
          | false -> 
            (* e| op' seq' --> e op |_ op' seq' *)
            let prefix' = OperatorSeq.ExpPrefix(e, op) in 
            let surround' = OperatorSeq.BothNonEmpty(prefix', suffix) in 
            let (ze0, u_gen) = new_EmptyHole u_gen in 
            make_and_typecheck_OpSeqZ ctx u_gen ze0 surround' 
          end
        end
      end
    | OperatorSeq.BothNonEmpty(prefix, suffix) -> 
      begin match suffix with 
      | OperatorSeq.ExpSuffix(op', e') -> 
        begin match is_Space op with 
        | true -> 
          (* prefix e| op' e' --> prefix e |_ op' e' *)
          let prefix' = OperatorSeq.prefix_append_exp prefix e op in 
          let suffix' = OperatorSeq.ExpSuffix(op', e') in 
          let surround' = OperatorSeq.BothNonEmpty(prefix', suffix') in 
          let (ze0, u_gen) = new_EmptyHole u_gen in 
          make_and_typecheck_OpSeqZ ctx u_gen ze0 surround' 
        | false -> 
          begin match is_Space op' with 
          | true -> 
            (* prefix e| e' --> prefix e op |e' *)
            let prefix' = OperatorSeq.prefix_append_exp prefix e op in 
            let surround' = OperatorSeq.EmptySuffix prefix' in 
            let ze0 = _Cursor Before e' in 
            make_and_typecheck_OpSeqZ ctx u_gen ze0 surround' 
          | false -> 
            (* prefix e| op' e' --> prefix e op |_ op' e' *)
            let prefix' = OperatorSeq.prefix_append_exp prefix e op in 
            let suffix' = OperatorSeq.ExpSuffix(op', e') in 
            let surround' = OperatorSeq.BothNonEmpty(prefix', suffix') in 
            let (ze0, u_gen) = new_EmptyHole u_gen in 
            make_and_typecheck_OpSeqZ ctx u_gen ze0 surround' 
          end
        end
      | OperatorSeq.SeqSuffix(op', seq') -> 
        begin match is_Space op with 
        | true -> 
          (* prefix e| op' seq' --> prefix e |_ op' seq' *)
          let prefix' = OperatorSeq.prefix_append_exp prefix e op in 
          let surround' = OperatorSeq.BothNonEmpty(prefix', suffix) in 
          let (ze0, u_gen) = new_EmptyHole u_gen in 
          make_and_typecheck_OpSeqZ ctx u_gen ze0 surround'
        | false -> 
          begin match is_Space op' with 
          | true -> 
            (* prefix e| seq' --> prefix e op |seq' *)
            let prefix' = OperatorSeq.prefix_append_exp prefix e op in 
            let (e0', suffix') = OperatorSeq.split0 seq' in 
            let surround' = OperatorSeq.BothNonEmpty(prefix', suffix') in 
            let ze0' = _Cursor Before e0' in 
            make_and_typecheck_OpSeqZ ctx u_gen ze0' surround' 
          | false -> 
            (* prefix e| op' seq' --> prefix e op |_ op' seq' *)
            let prefix' = OperatorSeq.prefix_append_exp prefix e op in 
            let surround' = OperatorSeq.BothNonEmpty(prefix', suffix) in 
            let (ze0, u_gen) = new_EmptyHole u_gen in 
            make_and_typecheck_OpSeqZ ctx u_gen ze0 surround'
          end
        end
      end
    end

let abs_perform_Construct_SOp_Before_surround 
  (erase : 'z -> 'e)
  (new_EmptyHole : MetaVarGen.t -> ('z * MetaVarGen.t))
  (make_and_typecheck_OpSeqZ : 
    Contexts.t -> MetaVarGen.t -> 
    'z -> ('e, 'op) OperatorSeq.opseq_surround -> 'm option)
  (is_Space : 'op -> bool)
  (_Space : 'op)
  (_Cursor : cursor_side -> 'e -> 'z)
  (ctx : Contexts.t)
  (u_gen : MetaVarGen.t)
  (ze0 : 'z)
  (op : 'op)
  (surround : ('e, 'op) OperatorSeq.opseq_surround)
  : 'm option = 
    begin match surround with 
    | OperatorSeq.EmptyPrefix suffix -> 
      (* |ze0 ... --> |_ op e0 ... *)
      let e0 = erase ze0 in 
      let suffix' = OperatorSeq.suffix_prepend_exp suffix op e0 in 
      let surround' = OperatorSeq.EmptyPrefix suffix' in 
      let (ze0, u_gen) = new_EmptyHole u_gen in 
      make_and_typecheck_OpSeqZ ctx u_gen ze0 surround' 
    | OperatorSeq.EmptySuffix ((OperatorSeq.ExpPrefix(e1, op')) as prefix) -> 
      begin match is_Space op' with 
      | true -> 
        begin match is_Space op with 
        | true -> 
          (* e1 |ze0 --> e1 |_ e0 *)
          let e0 = erase ze0 in 
          let suffix' = OperatorSeq.ExpSuffix(_Space, e0) in 
          let surround' = OperatorSeq.BothNonEmpty(prefix, suffix') in 
          let (ze0, u_gen) = new_EmptyHole u_gen in 
          make_and_typecheck_OpSeqZ ctx u_gen ze0 surround' 
        | false -> 
          (* e1 |ze0 --> e1 op |ze0 *)
          let surround' = OperatorSeq.EmptySuffix (OperatorSeq.ExpPrefix(e1, op)) in 
          make_and_typecheck_OpSeqZ ctx u_gen ze0 surround' 
        end
      | false -> 
        (* prefix [^ ] |ze0 --> prefix |_ op e0 *)
        let e0 = erase ze0 in 
        let suffix' = OperatorSeq.ExpSuffix(op, e0) in 
        let surround' = OperatorSeq.BothNonEmpty(prefix, suffix') in 
        let (ze0, u_gen) = new_EmptyHole u_gen in 
        make_and_typecheck_OpSeqZ ctx u_gen ze0 surround'
      end
    | OperatorSeq.EmptySuffix ((OperatorSeq.SeqPrefix(seq1, op')) as prefix) -> 
      begin match is_Space op' with 
      | true -> 
        begin match is_Space op with 
        | true -> 
          (* seq1 |ze0 --> seq1 |_ e0 *)
          let e0 = erase ze0 in 
          let suffix' = OperatorSeq.ExpSuffix(_Space, e0) in 
          let surround' = OperatorSeq.BothNonEmpty(prefix, suffix') in 
          let (ze0, u_gen) = new_EmptyHole u_gen in 
          make_and_typecheck_OpSeqZ ctx u_gen ze0 surround'
        | false -> 
          (* seq1 |ze0 --> seq1 op |ze0 *)
          let surround' = OperatorSeq.EmptySuffix (OperatorSeq.SeqPrefix(seq1, op)) in 
          make_and_typecheck_OpSeqZ ctx u_gen ze0 surround' 
        end
      | false -> 
        (* prefix [^ ] |ze0 --> prefix |_ op e0 *)
        let e0 = erase ze0 in 
        let suffix' = OperatorSeq.ExpSuffix(op, e0) in 
        let surround' = OperatorSeq.BothNonEmpty(prefix, suffix') in 
        let (ze0, u_gen) = new_EmptyHole u_gen in 
        make_and_typecheck_OpSeqZ ctx u_gen ze0 surround'
      end
    | OperatorSeq.BothNonEmpty((OperatorSeq.ExpPrefix(e1, op')) as prefix, suffix) -> 
      begin match is_Space op' with 
      | true -> 
        begin match is_Space op with 
        | true -> 
          (* e1 |ze0 suffix --> e1 |_ e0 suffix *)
          let e0 = erase ze0 in 
          let suffix' = OperatorSeq.suffix_prepend_exp suffix _Space e0 in 
          let surround' = OperatorSeq.BothNonEmpty(prefix, suffix') in 
          let (ze0, u_gen) = new_EmptyHole u_gen in 
          make_and_typecheck_OpSeqZ ctx u_gen ze0 surround'
        | false -> 
          (* e1 |ze0 suffix --> e1 op |ze0 suffix *)
          let prefix' = OperatorSeq.ExpPrefix(e1, op) in 
          let surround' = OperatorSeq.BothNonEmpty(prefix', suffix) in 
          make_and_typecheck_OpSeqZ ctx u_gen ze0 surround'
        end
      | false -> 
        (* prefix [^ ] |ze0 suffix --> prefix |_ op e0 suffix *)
        let e0 = erase ze0 in 
        let suffix' = OperatorSeq.suffix_prepend_exp suffix op e0 in 
        let surround' = OperatorSeq.BothNonEmpty(prefix, suffix') in 
        let (ze0, u_gen) = new_EmptyHole u_gen in 
        make_and_typecheck_OpSeqZ ctx u_gen ze0 surround'
      end
    | OperatorSeq.BothNonEmpty((OperatorSeq.SeqPrefix(seq1, op')) as prefix, suffix) -> 
      begin match is_Space op' with 
      | true -> 
        begin match is_Space op with 
        | true -> 
          (* seq1 |ze0 suffix --> seq1 |_ e0 suffix *)
          let e0 = erase ze0 in 
          let suffix' = OperatorSeq.suffix_prepend_exp suffix _Space e0 in 
          let surround' = OperatorSeq.BothNonEmpty(prefix, suffix') in 
          let (ze0, u_gen) = new_EmptyHole u_gen in 
          make_and_typecheck_OpSeqZ ctx u_gen ze0 surround'
        | false -> 
          (* seq1 |ze0 suffix --> seq1 op |ze0 suffix *)
          let prefix' = OperatorSeq.SeqPrefix(seq1, op) in 
          let surround' = OperatorSeq.BothNonEmpty(prefix', suffix) in 
          make_and_typecheck_OpSeqZ ctx u_gen ze0 surround'
        end
      | false -> 
        (* prefix [^ ] |ze0 suffix --> prefix |_ op e0 suffix *)
        let e0 = erase ze0 in 
        let suffix' = OperatorSeq.suffix_prepend_exp suffix op e0 in 
        let surround' = OperatorSeq.BothNonEmpty(prefix, suffix') in 
        let (ze0, u_gen) = new_EmptyHole u_gen in 
        make_and_typecheck_OpSeqZ ctx u_gen ze0 surround'
      end
    end

let syn_zpat_fix_holes
  (ctx : Contexts.t)
  (u_gen : MetaVarGen.t)
  (zp : ZPat.t)
  : (ZPat.t * HTyp.t * Contexts.t * MetaVarGen.t) option = 
    let path = Path.of_zpat zp in 
    let p = ZPat.erase zp in 
    begin match UHExp.syn_pat_fix_holes ctx u_gen false p with 
    | None -> None
    | Some (p, ty, ctx, u_gen) -> 
      begin match Path.follow_pat path p with 
      | None -> None
      | Some zp -> Some (zp, ty, ctx, u_gen)
      end
    end

let ana_zpat_fix_holes
  (ctx : Contexts.t)
  (u_gen : MetaVarGen.t)
  (zp : ZPat.t)
  (ty : HTyp.t)
  : (ZPat.t * Contexts.t * MetaVarGen.t) option = 
    let path = Path.of_zpat zp in 
    let p = ZPat.erase zp in 
    begin match UHExp.ana_pat_fix_holes ctx u_gen false p ty with 
    | None -> None
    | Some (p, ctx, u_gen) -> 
      begin match Path.follow_pat path p with 
      | None -> None
      | Some zp -> Some (zp, ctx, u_gen)
      end
    end

let make_and_syn_OpSeqZ_pat
  (ctx : Contexts.t)
  (u_gen : MetaVarGen.t)
  (zp0 : ZPat.t)
  (surround : ZPat.opseq_surround)
  : (ZPat.t * HTyp.t * Contexts.t * MetaVarGen.t) option = 
    (* figure out the current path so that we can follow it again 
     * to reconstitute the Z-exp after calling into the UHExp hole 
     * insertion logic (otherwise we'd have to do a version of that
     * logic specific to Z-exps) *)
    let path0 = Path.of_OpSeqZ_pat zp0 surround in 
    let p0 = ZPat.erase zp0 in 
    let seq = OperatorSeq.opseq_of_exp_and_surround p0 surround in 
    let skel = Associator.associate_pat seq in 
    begin match UHExp.syn_skel_pat_fix_holes ctx u_gen false skel seq with 
    | Some (skel, seq, ty, ctx, u_gen) -> 
      let p = UHPat.Pat(NotInHole, UHPat.OpSeq(skel, seq)) in 
      begin match Path.follow_pat path0 p with 
      | Some zp -> Some (zp, ty, ctx, u_gen)
      | None -> None
      end
    | None -> None
    end

let make_and_ana_OpSeqZ_pat
  (ctx : Contexts.t)
  (u_gen : MetaVarGen.t)
  (zp0 : ZPat.t)
  (surround : ZPat.opseq_surround)
  (ty : HTyp.t)
  : (ZPat.t * Contexts.t * MetaVarGen.t) option = 
    (* figure out the current path so that we can follow it again 
     * to reconstitute the Z-exp after calling into the UHExp hole 
     * insertion logic (otherwise we'd have to do a version of that
     * logic specific to Z-exps) *)
    let path0 = Path.of_OpSeqZ_pat zp0 surround in 
    let p0 = ZPat.erase zp0 in 
    let seq = OperatorSeq.opseq_of_exp_and_surround p0 surround in 
    let skel = Associator.associate_pat seq in 
    begin match UHExp.ana_skel_pat_fix_holes ctx u_gen false skel seq ty with 
    | Some (Skel.BinOp(err, _, _, _) as skel, seq, ctx, u_gen) -> 
      let p = UHPat.Pat(err, UHPat.OpSeq(skel, seq)) in 
      begin match Path.follow_pat path0 p with 
      | Some zp -> Some (zp, ctx, u_gen)
      | None -> None
      end
    | Some (Skel.Placeholder _, _, _, _) 
    | None -> None
    end

let combine_for_Backspace_Space_pat p1 zp0 = 
  begin match zp0 with 
  | ZPat.CursorP(_, UHPat.Pat(_, UHPat.EmptyHole _)) -> 
    (* p1 |_ --> p1| *)
    ZPat.CursorP(After, p1)
  | _ -> 
    (* p1 |zp0 --> |zp0 *)
    zp0
  end

let combine_for_Delete_Space_pat zp0 p = 
  begin match (zp0, p) with 
  | (ZPat.CursorP(After, UHPat.Pat(_, UHPat.EmptyHole _)),
     UHPat.Pat(_, UHPat.EmptyHole _)) -> 
    (* _| _ --> _| *)
    zp0
  | (ZPat.CursorP(After, UHPat.Pat(_, UHPat.EmptyHole _)),
     _) -> 
    (* _| p  --> |p *)
    ZPat.CursorP(Before, p)
  | _ -> 
    zp0
  end

let rec perform_syn_pat
  (ctx : Contexts.t)
  (u_gen : MetaVarGen.t)
  (a : t)
  (zp : ZPat.t)
  : (ZPat.t * HTyp.t * Contexts.t * MetaVarGen.t) option = 
  begin match (a, zp) with
  (* Movement *)
  (* NOTE: we don't need to handle movement actions here for the purposes of the UI,
   * since it's handled at the top (expression) level, but for the sake of API completeness
   * we include it *)
  | (MoveTo path, _) -> 
    let p = ZPat.erase zp in
    begin match UHExp.syn_pat ctx p with 
    | None -> None
    | Some (ty, _) -> 
      begin match Path.follow_pat path p with
      | Some zp -> Some (zp, ty, ctx, u_gen)
      | None -> None
      end
    end
  | (MoveToPrevHole, _) ->
    begin match Path.prev_hole_path_pat zp with
    | None -> None
    | Some path -> perform_syn_pat ctx u_gen (MoveTo path) zp
    end
  | (MoveToNextHole, _) ->
    begin match Path.next_hole_path_pat zp with
    | None -> None
    | Some path -> perform_syn_pat ctx u_gen (MoveTo path) zp
    end
  (* Backspace and Delete *)
  | (Backspace, ZPat.CursorP(After, p)) -> 
    begin match p with 
    | UHPat.Pat(_, UHPat.EmptyHole _) -> 
      Some (ZPat.CursorP(Before, p), HTyp.Hole, ctx, u_gen)
    | _ -> 
      let (p, u_gen) = UHPat.new_EmptyHole u_gen in 
      Some (ZPat.CursorP(Before, p), HTyp.Hole, ctx, u_gen)
    end
  | (Backspace, ZPat.CursorP(Before, _)) -> None
  | (Delete, ZPat.CursorP(Before, p)) -> 
    begin match p with 
    | UHPat.Pat(_, UHPat.EmptyHole _) -> 
      Some (ZPat.CursorP(After, p), HTyp.Hole, ctx, u_gen)
    | _ -> 
      let (p, u_gen) = UHPat.new_EmptyHole u_gen in 
      Some (ZPat.CursorP(Before, p), HTyp.Hole, ctx, u_gen)
    end
  | (Delete, ZPat.CursorP(After, _)) -> None
  | (Backspace, ZPat.CursorP(In _, _))
  | (Delete, ZPat.CursorP(In _, _)) -> 
    let (p, u_gen) = UHPat.new_EmptyHole u_gen in 
    let zp = ZPat.CursorP(Before, p) in 
    Some (zp, HTyp.Hole, ctx, u_gen)
  | (Backspace, ZPat.Deeper(_, 
      (ZPat.OpSeqZ(_, 
        ((ZPat.CursorP(Before, p0)) as zp0),  
        ((OperatorSeq.EmptySuffix _) as surround)))))
  | (Backspace, ZPat.Deeper(_, 
      (ZPat.OpSeqZ(_, 
        ((ZPat.CursorP(Before, p0)) as zp0), 
        ((OperatorSeq.BothNonEmpty(_, _)) as surround))))) ->
    abs_perform_Backspace_Before_op 
      combine_for_Backspace_Space_pat
      syn_zpat_fix_holes
      make_and_syn_OpSeqZ_pat
      UHPat.is_EmptyHole
      UHPat.is_Space
      UHPat.Space
      (fun side p -> ZPat.CursorP (side, p))
      ctx u_gen p0 zp0 surround
  | (Delete, ZPat.Deeper(_, 
      (ZPat.OpSeqZ(_, 
        ((ZPat.CursorP(After, p0)) as zp0),
        ((OperatorSeq.EmptyPrefix _) as surround)))))
  | (Delete, ZPat.Deeper(_,  
      (ZPat.OpSeqZ(_, 
        ((ZPat.CursorP(After, p0)) as zp0), 
        ((OperatorSeq.BothNonEmpty(_, _)) as surround))))) -> 
    abs_perform_Delete_After_op
      combine_for_Delete_Space_pat
      syn_zpat_fix_holes
      make_and_syn_OpSeqZ_pat
      UHPat.is_EmptyHole
      UHPat.is_Space
      UHPat.Space
      (fun side p -> ZPat.CursorP (side, p))
      ctx u_gen p0 zp0 surround
  (* Construct *)
  | (Construct SParenthesized, ZPat.CursorP(_, p)) -> 
    begin match UHExp.syn_pat ctx p with 
    | None -> None
    | Some (ty, ctx) -> 
      Some (
        ZPat.ParenthesizedZ zp, 
        ty,
        ctx,
        u_gen)
    end
  | (Construct (SVar(x, side)), ZPat.CursorP(_, (UHPat.Pat(_, (UHPat.EmptyHole _))))) 
  | (Construct (SVar(x, side)), ZPat.CursorP(_, (UHPat.Pat(_, UHPat.Wild))))
  | (Construct (SVar(x, side)), ZPat.CursorP(_, (UHPat.Pat(_, (UHPat.Var _)))))
  | (Construct (SVar(x, side)), ZPat.CursorP(_, (UHPat.Pat(_, (UHPat.NumLit _)))))
  | (Construct (SVar(x, side)), ZPat.CursorP(_, (UHPat.Pat(_, (UHPat.BoolLit _))))) ->
    Var.check_valid x (
    let ctx = Contexts.extend_gamma ctx (x, HTyp.Hole) in 
    Some
      (ZPat.CursorP(side, (UHPat.Pat(NotInHole, (UHPat.Var x)))), 
       HTyp.Hole,
       ctx, 
       u_gen)
    )
  | (Construct (SVar(_, _)), ZPat.CursorP(_, _)) -> None
  | (Construct SWild, ZPat.CursorP(_, (UHPat.Pat(_, (UHPat.EmptyHole _))))) 
  | (Construct SWild, ZPat.CursorP(_, (UHPat.Pat(_, UHPat.Wild))))
  | (Construct SWild, ZPat.CursorP(_, (UHPat.Pat(_, (UHPat.Var _)))))
  | (Construct SWild, ZPat.CursorP(_, (UHPat.Pat(_, (UHPat.NumLit _))))) 
  | (Construct SWild, ZPat.CursorP(_, (UHPat.Pat(_, (UHPat.BoolLit _))))) ->
    Some
      (ZPat.CursorP(After, (UHPat.Pat(NotInHole, UHPat.Wild))), 
       HTyp.Hole,
       ctx, 
       u_gen)
  | (Construct SWild, ZPat.CursorP(_, _)) -> None
  | (Construct (SNumLit(n, side)), ZPat.CursorP(_, (UHPat.Pat(_, (UHPat.EmptyHole _))))) 
  | (Construct (SNumLit(n, side)), ZPat.CursorP(_, (UHPat.Pat(_, UHPat.Wild))))
  | (Construct (SNumLit(n, side)), ZPat.CursorP(_, (UHPat.Pat(_, (UHPat.Var _)))))
  | (Construct (SNumLit(n, side)), ZPat.CursorP(_, (UHPat.Pat(_, (UHPat.NumLit _)))))
  | (Construct (SNumLit(n, side)), ZPat.CursorP(_, (UHPat.Pat(_, (UHPat.BoolLit _))))) ->
    Some
      (ZPat.CursorP(side, (UHPat.Pat(NotInHole, UHPat.NumLit n))), 
       HTyp.Num, 
       ctx, 
       u_gen)
  | (Construct (SNumLit(_, _)), ZPat.CursorP(_, _)) -> None
  | (Construct (SBoolLit(b, side)), ZPat.CursorP(_, (UHPat.Pat(_, (UHPat.EmptyHole _))))) 
  | (Construct (SBoolLit(b, side)), ZPat.CursorP(_, (UHPat.Pat(_, UHPat.Wild))))
  | (Construct (SBoolLit(b, side)), ZPat.CursorP(_, (UHPat.Pat(_, (UHPat.Var _)))))  
  | (Construct (SBoolLit(b, side)), ZPat.CursorP(_, (UHPat.Pat(_, (UHPat.NumLit _)))))  
  | (Construct (SBoolLit(b, side)), ZPat.CursorP(_, (UHPat.Pat(_, (UHPat.BoolLit _))))) ->  
    Some
      (ZPat.CursorP(side, (UHPat.Pat(NotInHole, (UHPat.BoolLit b)))), 
       HTyp.Bool, 
       ctx, 
       u_gen)
  | (Construct (SBoolLit(_, _)), ZPat.CursorP(_, _)) -> None
  | (Construct (SInj side), ZPat.CursorP(_, p1)) -> 
    begin match UHExp.syn_pat ctx p1 with 
    | None -> None
    | Some (ty1, ctx) -> 
      let zp = ZPat.Deeper(NotInHole,  
        (ZPat.InjZ(side, zp))) in 
      let ty = 
        begin match side with 
        | L -> HTyp.Sum(ty1, HTyp.Hole)
        | R -> HTyp.Sum(HTyp.Hole, ty1)
        end in 
      Some (zp, ty, ctx, u_gen)
    end
  | (Construct SListNil, (ZPat.CursorP(_, (UHPat.Pat(_, (UHPat.EmptyHole _)))))) -> 
    let zp = ZPat.CursorP(After, (UHPat.Pat(NotInHole, UHPat.ListNil))) in 
    let ty = HTyp.List HTyp.Hole in 
    Some (zp, ty, ctx, u_gen)
  | (Construct SListNil, ZPat.CursorP(_, _)) -> None
  | (Construct (SOp os), ZPat.Deeper(_, (
      ZPat.OpSeqZ(_, (ZPat.CursorP(In _, p)), surround))))
  | (Construct (SOp os), ZPat.Deeper(_, (
      ZPat.OpSeqZ(_, (ZPat.CursorP(After, p)), surround)))) -> 
    begin match pat_op_of os with
    | None -> None
    | Some op -> 
      abs_perform_Construct_SOp_After_surround 
        ZPat.new_EmptyHole
        make_and_syn_OpSeqZ_pat
        UHPat.is_Space
        UHPat.Space
        (fun side p -> ZPat.CursorP (side, p))
        ctx u_gen p op surround
    end
  | (Construct (SOp os), 
      ZPat.Deeper(_, (ZPat.OpSeqZ(_, 
        ((ZPat.CursorP(Before, _)) as zp0), surround)))) ->
    begin match pat_op_of os with 
    | None -> None
    | Some op -> 
      abs_perform_Construct_SOp_Before_surround
        ZPat.erase
        ZPat.new_EmptyHole
        make_and_syn_OpSeqZ_pat
        UHPat.is_Space
        UHPat.Space
        (fun side p -> ZPat.CursorP (side, p))
        ctx u_gen zp0 op surround
    end
  | (Construct (SOp os), ZPat.CursorP(In _, p))
  | (Construct (SOp os), ZPat.CursorP(After, p)) -> 
    begin match pat_op_of os with 
    | None -> None
    | Some op -> 
      abs_perform_Construct_SOp_After 
        UHPat.bidelimit
        ZPat.new_EmptyHole
        make_and_syn_OpSeqZ_pat
        ctx u_gen p op
    end
  | (Construct (SOp os), ZPat.CursorP(Before, p)) -> 
    begin match pat_op_of os with 
    | None -> None
    | Some op -> 
      abs_perform_Construct_SOp_Before
        UHPat.bidelimit
        ZPat.new_EmptyHole
        make_and_syn_OpSeqZ_pat
        ctx u_gen p op
    end
  (* Zipper *)
  | (_, ZPat.ParenthesizedZ zp1) -> 
    begin match perform_syn_pat ctx u_gen a zp1 with 
    | None -> None
    | Some (zp1, ty, ctx, u_gen) -> 
      Some (
        ZPat.ParenthesizedZ zp1,
        ty,
        ctx,
        u_gen)
    end
  | (_, ZPat.Deeper(_, (ZPat.InjZ(side, zp1)))) -> 
    begin match perform_syn_pat ctx u_gen a zp1 with 
    | None -> None
    | Some (zp1, ty1, ctx, u_gen) -> 
      let zp = ZPat.Deeper(NotInHole,  
        (ZPat.InjZ(side, zp1))) in 
      let ty = 
        begin match side with 
        | L -> HTyp.Sum(ty1, HTyp.Hole)
        | R -> HTyp.Sum(HTyp.Hole, ty1)
        end in 
      Some (zp, ty, ctx, u_gen)
    end
  | (_, ZPat.Deeper(_, (ZPat.OpSeqZ(_, zp0, surround)))) -> 
    let i = OperatorSeq.surround_prefix_length surround in 
    begin match ZPat.erase zp with 
    | UHPat.Pat(_, (UHPat.OpSeq(skel, seq))) -> 
      begin match UHExp.syn_skel_pat ctx skel seq (Some i) with 
      | Some (ty, ctx, Some mode) ->
          begin match mode with 
          | UHExp.AnalyzedAgainst ty0 -> 
            begin match perform_ana_pat ctx u_gen a zp0 ty0 with 
            | None -> None
            | Some (zp0, ctx, u_gen) -> 
              let zp0 = ZPat.bidelimit zp0 in  
              Some (
                ZPat.Deeper(NotInHole, (ZPat.OpSeqZ(skel, zp0, surround))), 
                ty, ctx, u_gen)
            end
          | UHExp.Synthesized ty0 ->
            begin match perform_syn_pat ctx u_gen a zp0 with 
            | Some (zp0, ty0, ctx, u_gen) -> 
              let zp0 = ZPat.bidelimit zp0 in 
              make_and_syn_OpSeqZ_pat ctx u_gen zp0 surround
            | None -> None
            end
          end
      | Some _ -> None (* should never happen *)
      | None -> None (* should never happen *)
      end
    | _ -> None (* should never happen *)
    end
  | (UpdateApPalette _, _)
  | (Construct (SApPalette _), _)
  | (Construct SNum, _) 
  | (Construct SBool, _)
  | (Construct SList, _) 
  | (Construct SAsc, _) 
  | (Construct SLet, _) 
  | (Construct SLam, _) 
  | (Construct SCase, _)
  | (Construct SRule, _) -> None
  end
and perform_ana_pat 
  (ctx : Contexts.t)
  (u_gen : MetaVarGen.t)
  (a : t)
  (zp : ZPat.t)
  (ty : HTyp.t)
  : (ZPat.t * Contexts.t * MetaVarGen.t) option = 
  begin match (a, zp) with
  (* Movement *)
  (* NOTE: we don't need to handle movement actions here for the purposes of the UI,
   * since it's handled at the top (expression) level, but for the sake of API completeness
   * we include it *)
  | (MoveTo path, _) -> 
    let p = ZPat.erase zp in
    begin match Path.follow_pat path p with
    | Some zp -> Some (zp, ctx, u_gen)
    | None -> None
    end
  | (MoveToPrevHole, _) ->
    begin match Path.prev_hole_path_pat zp with
    | None -> None
    | Some path -> perform_ana_pat ctx u_gen (MoveTo path) zp ty
    end
  | (MoveToNextHole, _) ->
    begin match Path.next_hole_path_pat zp with
    | None -> None
    | Some path -> perform_ana_pat ctx u_gen (MoveTo path) zp ty
    end
  (* switch to synthesis if in a hole *)
  | (_, ZPat.Deeper(InHole(TypeInconsistent, u), zp1)) -> 
    let zp1_not_in_hole = ZPat.Deeper(NotInHole, zp1) in 
    let p1 = ZPat.erase zp1_not_in_hole in 
    begin match UHExp.syn_pat ctx p1 with 
    | None -> None
    | Some (ty1, _) -> 
      begin match perform_syn_pat ctx u_gen a zp1_not_in_hole with 
      | None -> None
      | Some (zp1, ty', ctx, u_gen) -> 
        if HTyp.consistent ty ty' then 
          Some (zp1, ctx, u_gen)
        else 
          Some (ZPat.set_inconsistent u zp1, ctx, u_gen)
      end
    end
  (* Backspace and Delete *)
  | (Backspace, ZPat.CursorP(After, p)) -> 
    begin match p with 
    | UHPat.Pat(_, (UHPat.EmptyHole _)) -> 
      Some (ZPat.CursorP(Before, p), ctx, u_gen)
    | _ -> 
      let (p, u_gen) = UHPat.new_EmptyHole u_gen in 
      Some (ZPat.CursorP(Before, p), ctx, u_gen)
    end
  | (Backspace, ZPat.CursorP(Before, p)) -> None
  | (Delete, ZPat.CursorP(Before, p)) -> 
    begin match p with 
    | UHPat.Pat(_, (UHPat.EmptyHole _)) -> 
      Some (ZPat.CursorP(After, p), ctx, u_gen)
    | _ -> 
      let (p, u_gen) = UHPat.new_EmptyHole u_gen in 
      Some (ZPat.CursorP(Before, p), ctx, u_gen)
    end
  | (Backspace, ZPat.CursorP(In _, _))
  | (Delete, ZPat.CursorP(In _, _)) -> 
    let (p, u_gen) = UHPat.new_EmptyHole u_gen in 
    let zp = ZPat.CursorP(Before, p) in 
    Some (zp, ctx, u_gen)
  | (Delete, ZPat.CursorP(After, _)) -> None
  (* Construct *)
  | (Construct SParenthesized, ZPat.CursorP(_, p)) -> 
    begin match UHExp.ana_pat ctx p ty with 
    | None -> None
    | Some ctx -> 
      Some (
        ZPat.ParenthesizedZ zp, 
        ctx,
        u_gen)
    end
  | (Construct (SVar(x, side)), ZPat.CursorP(_, (UHPat.Pat(_, (UHPat.EmptyHole _))))) 
  | (Construct (SVar(x, side)), ZPat.CursorP(_, (UHPat.Pat(_, UHPat.Wild))))
  | (Construct (SVar(x, side)), ZPat.CursorP(_, (UHPat.Pat(_, (UHPat.Var _)))))
  | (Construct (SVar(x, side)), ZPat.CursorP(_, (UHPat.Pat(_, (UHPat.NumLit _)))))
  | (Construct (SVar(x, side)), ZPat.CursorP(_, (UHPat.Pat(_, (UHPat.BoolLit _))))) -> 
    Var.check_valid x (
    let ctx = Contexts.extend_gamma ctx (x, ty) in 
    Some
      (ZPat.CursorP(side, (UHPat.Pat(NotInHole, (UHPat.Var x)))), 
       ctx, 
       u_gen)
    )
  | (Construct (SVar(_, _)), ZPat.CursorP(_, _)) -> None
  | (Construct SWild, ZPat.CursorP(_, (UHPat.Pat(_, (UHPat.EmptyHole _))))) 
  | (Construct SWild, ZPat.CursorP(_, (UHPat.Pat(_, UHPat.Wild))))
  | (Construct SWild, ZPat.CursorP(_, (UHPat.Pat(_, (UHPat.Var _)))))
  | (Construct SWild, ZPat.CursorP(_, (UHPat.Pat(_, (UHPat.NumLit _)))))
  | (Construct SWild, ZPat.CursorP(_, (UHPat.Pat(_, (UHPat.BoolLit _))))) ->
    Some
      (ZPat.CursorP(After, (UHPat.Pat(NotInHole, UHPat.Wild))), 
       ctx, 
       u_gen)
  | (Construct SWild, ZPat.CursorP(_, _)) -> None
  | (Construct (SInj side), ZPat.CursorP(cursor_side, p1)) -> 
    begin match HTyp.matched_sum ty with 
    | Some (tyL, tyR) -> 
      let ty1 = pick_side side tyL tyR in 
      begin match UHExp.ana_pat_fix_holes ctx u_gen false p1 ty1 with 
      | None -> None
      | Some (p1, ctx, u_gen) -> 
        let zp = 
          ZPat.Deeper(NotInHole, 
            (ZPat.InjZ(side,  
              (ZPat.CursorP(cursor_side, p1))))) in 
        Some (zp, ctx, u_gen)
      end
    | None -> 
      begin match UHExp.syn_pat_fix_holes ctx u_gen false p1 with 
      | None -> None
      | Some (p1, _, ctx, u_gen) -> 
        let (u, u_gen) = MetaVarGen.next u_gen in 
        let zp = 
          ZPat.Deeper(InHole(TypeInconsistent, u), 
            (ZPat.InjZ(side,  
              (ZPat.CursorP(cursor_side, p1))))) in 
        Some (zp, ctx, u_gen)
      end
    end
  | (Construct (SOp os), ZPat.Deeper(_, (
      ZPat.OpSeqZ(_, ZPat.CursorP (In _, p), surround))))
  | (Construct (SOp os), ZPat.Deeper(_, (
      ZPat.OpSeqZ(_, (ZPat.CursorP(After, p)), surround)))) -> 
    begin match pat_op_of os with
    | None -> None
    | Some op -> 
      abs_perform_Construct_SOp_After_surround 
        ZPat.new_EmptyHole
        (fun ctx u_gen zp surround -> 
          make_and_ana_OpSeqZ_pat ctx u_gen zp surround ty)
        UHPat.is_Space
        UHPat.Space
        (fun side p -> ZPat.CursorP (side, p))
        ctx u_gen p op surround
    end
  | (Construct (SOp os), 
      ZPat.Deeper(_, (ZPat.OpSeqZ(_, 
        ((ZPat.CursorP(Before, _)) as zp0), surround)))) ->
    begin match pat_op_of os with 
    | None -> None
    | Some op -> 
      abs_perform_Construct_SOp_Before_surround
        ZPat.erase
        ZPat.new_EmptyHole
        (fun ctx u_gen zp surround -> 
          make_and_ana_OpSeqZ_pat ctx u_gen zp surround ty)
        UHPat.is_Space
        UHPat.Space
        (fun side p -> ZPat.CursorP (side, p))
        ctx u_gen zp0 op surround
    end
  | (Construct (SOp os), ZPat.CursorP(In _,  p))
  | (Construct (SOp os), ZPat.CursorP(After, p)) -> 
    begin match pat_op_of os with 
    | None -> None
    | Some op -> 
      abs_perform_Construct_SOp_After 
        UHPat.bidelimit
        ZPat.new_EmptyHole
        (fun ctx u_gen zp surround -> 
          make_and_ana_OpSeqZ_pat ctx u_gen zp surround ty)
        ctx u_gen p op
    end
  | (Construct (SOp os), ZPat.CursorP(Before, p)) -> 
    begin match pat_op_of os with 
    | None -> None
    | Some op -> 
      abs_perform_Construct_SOp_Before
        UHPat.bidelimit
        ZPat.new_EmptyHole
        (fun ctx u_gen zp surround -> 
          make_and_ana_OpSeqZ_pat ctx u_gen zp surround ty)
        ctx u_gen p op
    end
  (* Zipper *)
  | (_, ZPat.ParenthesizedZ zp1) -> 
    begin match perform_ana_pat ctx u_gen a zp1 ty with 
    | None -> None
    | Some (zp1, ctx, u_gen) -> 
      Some (
        ZPat.ParenthesizedZ zp1,
        ctx,
        u_gen)
    end
  | (_, ZPat.Deeper(_, (ZPat.InjZ(side, zp1)))) -> 
    begin match HTyp.matched_sum ty with 
    | None -> None
    | Some (tyL, tyR) -> 
      let ty1 = pick_side side tyL tyR in 
      begin match perform_ana_pat ctx u_gen a zp1 ty1 with 
      | None -> None
      | Some (zp1, ctx, u_gen) -> 
        let zp = ZPat.Deeper(NotInHole, (ZPat.InjZ(side, zp1))) in 
        Some (zp, ctx, u_gen)
      end
    end
  | (_, ZPat.Deeper(_, (ZPat.OpSeqZ(_, zp0, surround)))) -> 
    let i = OperatorSeq.surround_prefix_length surround in 
    begin match ZPat.erase zp with 
    | UHPat.Pat(_, (UHPat.OpSeq(skel, seq))) -> 
      begin match UHExp.ana_skel_pat ctx skel seq ty (Some i) with 
      | Some (ctx, Some mode) ->
          begin match mode with 
          | UHExp.AnalyzedAgainst ty0 -> 
            begin match perform_ana_pat ctx u_gen a zp0 ty0 with 
            | None -> None
            | Some (zp0, ctx, u_gen) -> 
              let zp0 = ZPat.bidelimit zp0 in  
              Some (
                ZPat.Deeper(NotInHole, (ZPat.OpSeqZ(skel, zp0, surround))), 
                ctx, u_gen)
            end
          | UHExp.Synthesized ty0 ->
            begin match perform_syn_pat ctx u_gen a zp0 with 
            | Some (zp0, ty0, ctx, u_gen) -> 
              let zp0 = ZPat.bidelimit zp0 in 
              make_and_ana_OpSeqZ_pat ctx u_gen zp0 surround ty 
            | None -> None
            end
          end
      | Some _ -> None (* should never happen *)
      | None -> None (* should never happen *)
      end
    | _ -> None (* should never happen *)
    end
  (* Subsumption *)
  | (Construct (SNumLit(_, _)), _)
  | (Construct (SBoolLit(_, _)), _)
  | (Construct SListNil, _) -> 
    begin match perform_syn_pat ctx u_gen a zp with 
    | None -> None
    | Some (zp, ty', ctx, u_gen) -> 
      if HTyp.consistent ty ty' then 
        Some (zp, ctx, u_gen)
      else 
        let (zp, u_gen) = ZPat.make_inconsistent u_gen zp in
        Some (zp, ctx, u_gen)
    end
  (* Invalid actions at the pattern level *)
  | (UpdateApPalette _, _)
  | (Construct (SApPalette _), _) 
  | (Construct SNum, _) 
  | (Construct SBool, _) 
  | (Construct SList, _) 
  | (Construct SAsc, _) 
  | (Construct SLet, _) 
  | (Construct SLam, _)
  | (Construct SCase, _) 
  | (Construct SRule, _) -> None 
  end

let zexp_syn_fix_holes
  (ctx : Contexts.t)
  (u_gen : MetaVarGen.t)
  (ze : ZExp.t)
  : (ZExp.t * HTyp.t * MetaVarGen.t) option = 
    let path = Path.of_zexp ze in 
    let e = ZExp.erase ze in 
    begin match UHExp.syn_fix_holes ctx u_gen e with 
    | Some (e', ty, u_gen') -> 
      begin match Path.follow_e path e' with 
      | Some ze' -> Some (ze', ty, u_gen')
      | None -> None
      end
    | None -> None
    end

let zexp_ana_fix_holes
  (ctx : Contexts.t)
  (u_gen : MetaVarGen.t)
  (ze : ZExp.t)
  (ty : HTyp.t)
  : (ZExp.t * MetaVarGen.t) option = 
    let path = Path.of_zexp ze in 
    let e = ZExp.erase ze in 
    begin match UHExp.ana_fix_holes ctx u_gen e ty with 
    | Some (e', u_gen') -> 
      begin match Path.follow_e path e' with 
      | Some ze' -> Some (ze', u_gen')
      | None -> None
      end
    | None -> None
    end

let make_and_syn_OpSeqZ 
  (ctx : Contexts.t)
  (u_gen : MetaVarGen.t)
  (ze0 : ZExp.t)
  (surround : ZExp.opseq_surround)
  : (ZExp.t * HTyp.t * MetaVarGen.t) option = 
    (* figure out the current path so that we can follow it again 
     * to reconstitute the Z-exp after calling into the UHExp hole 
     * insertion logic (otherwise we'd have to do a version of that
     * logic specific to Z-exps) *)
    let path0 = Path.of_OpSeqZ ze0 surround in 
    let e0 = ZExp.erase ze0 in 
    let seq = OperatorSeq.opseq_of_exp_and_surround e0 surround in 
    let skel = Associator.associate_exp seq in 
    begin match UHExp.syn_skel_fix_holes ctx u_gen false skel seq with 
    | Some (skel', seq', ty, u_gen') -> 
      let e' = UHExp.Tm(NotInHole, (UHExp.OpSeq(skel', seq'))) in 
      begin match Path.follow_e path0 e' with 
      | Some ze' -> Some (ze', ty, u_gen')
      | None -> None
      end
    | None -> None
    end

let make_and_ana_OpSeqZ 
  (ctx : Contexts.t)
  (u_gen : MetaVarGen.t)
  (ze0 : ZExp.t)
  (surround : ZExp.opseq_surround)
  (ty : HTyp.t)
  : (ZExp.t * MetaVarGen.t) option = 
    (* figure out the current path so that we can follow it again 
     * to reconstitute the Z-exp after calling into the UHExp hole 
     * insertion logic (otherwise we'd have to do a version of that
     * logic specific to Z-exps) *)
    let path0 = Path.of_OpSeqZ ze0 surround in 
    let e0 = ZExp.erase ze0 in 
    let seq = OperatorSeq.opseq_of_exp_and_surround e0 surround in 
    let skel = Associator.associate_exp seq in 
    begin match UHExp.ana_skel_fix_holes ctx u_gen false skel seq ty with 
    | Some ((Skel.BinOp(err, _, _, _)) as skel, seq, u_gen) -> 
      let e = UHExp.Tm(err, (UHExp.OpSeq(skel, seq))) in 
      begin match Path.follow_e path0 e with 
      | Some ze -> Some (ze, u_gen)
      | None -> None
      end
    | Some (Skel.Placeholder _, _, _)  
    | None -> None
    end

let combine_for_Backspace_Space e1 ze0 = 
  begin match (e1, ze0) with 
  | (_, ZExp.CursorE(_, (UHExp.Tm(_, (UHExp.EmptyHole _))))) -> 
    (* e1 |_ --> e1| *)
    ZExp.CursorE(After, e1)
  | _ -> ze0
  end

let combine_for_Delete_Space ze0 e = 
  begin match (ze0, e) with 
  | ((ZExp.CursorE(After, (UHExp.Tm(_, (UHExp.EmptyHole _))))),
     UHExp.Tm(_, (UHExp.EmptyHole _))) -> 
    (* _| _ --> _| *)
    ze0
  | ((ZExp.CursorE(After, (UHExp.Tm(_, (UHExp.EmptyHole _))))),
     _) -> 
    (* _| e --> |e *)
    ZExp.CursorE(Before, e)
  | _ -> 
    ze0
  end

let rec perform_syn 
    (ctx: Contexts.t) 
    (a: t) 
    (ze_ty : ZExp.t * HTyp.t * MetaVarGen.t) 
    : (ZExp.t * HTyp.t * MetaVarGen.t) option =
  let (ze, ty, u_gen) = ze_ty in
  begin match (a, ze) with
  (* Movement *)
  | (MoveTo path, _) -> 
    let e = ZExp.erase ze in
    begin match Path.follow_e path e with
    | Some ze' -> Some (ze', ty, u_gen)
    | None -> None
    end
  | (MoveToPrevHole, _) ->
    begin match Path.prev_hole_path ze with
    | None -> None
    | Some path -> perform_syn ctx (MoveTo path) ze_ty
    end
  | (MoveToNextHole, _) ->
    begin match Path.next_hole_path ze with
    | None -> None
    | Some path ->
      (* let path = Helper.log_path path in *)
      perform_syn ctx (MoveTo path) ze_ty
    end
  (* Backspace & Deletion *)
  | (Backspace, ZExp.CursorE(After, e)) -> 
    begin match e with 
    | UHExp.Tm(_, (UHExp.EmptyHole _)) -> 
      Some (ZExp.CursorE(Before, e), ty, u_gen)
    | _ -> 
      let (e', u_gen') = UHExp.new_EmptyHole u_gen in 
      Some (ZExp.CursorE(Before, e'), HTyp.Hole, u_gen')
    end
  | (Backspace, ZExp.CursorE(Before, e)) -> None
  | (Delete, ZExp.CursorE(Before, e)) -> 
    begin match e with 
    | UHExp.Tm(_, (UHExp.EmptyHole _)) -> 
      Some (ZExp.CursorE(After, e), ty, u_gen)
    | _ -> 
      let (e', u_gen') = UHExp.new_EmptyHole u_gen in 
      Some (ZExp.CursorE(Before, e'), HTyp.Hole, u_gen)
    end
  | (Delete, ZExp.CursorE(After, e)) -> None
  | (Backspace, 
      ZExp.Deeper(_, (ZExp.AscZ2(e1,  
        (ZTyp.CursorT(Before, _)))))) 
  | (Backspace,
      ZExp.Deeper(_, (ZExp.AscZ2(e1,  
        (ZTyp.OpSeqZ(_,  
          (ZTyp.CursorT(Before, _)),
          (OperatorSeq.EmptyPrefix _))))))) -> 
    let ze' = ZExp.CursorE(After, e1) in 
    zexp_syn_fix_holes ctx u_gen ze'
  | (Delete,
      ZExp.Deeper(_, (ZExp.AscZ1
        ((ZExp.CursorE(After, e1)), _)))) -> 
    begin match UHExp.syn_fix_holes ctx u_gen e1 with 
    | Some (e1', ty', u_gen) -> 
      let ze' = ZExp.CursorE(After, e1') in 
      Some (ze', ty', u_gen)
    | None -> None
    end
  | (Backspace, ZExp.Deeper(_,  
      ZExp.LetZA(p, ZTyp.CursorT(Before, _), e1, e2)))
  | (Backspace, ZExp.Deeper(_,
      ZExp.LetZA(
        p,
        ZTyp.OpSeqZ(_, ZTyp.CursorT(Before, _), OperatorSeq.EmptyPrefix _),
        e1,
        e2))) ->
    begin match UHExp.syn_fix_holes ctx u_gen e1 with 
    | None -> None
    | Some (e1, ty1, u_gen) -> 
      begin match UHExp.ana_pat_fix_holes ctx u_gen false p ty1 with 
      | None -> None
      | Some (p, ctx, u_gen) -> 
        begin match UHExp.syn_fix_holes ctx u_gen e2 with 
        | None -> None
        | Some (e2, ty, u_gen) -> 
          let ze = 
            ZExp.Deeper(NotInHole, 
              ZExp.LetZP(ZPat.CursorP(After, p), None, e1, e2)) in 
          Some (ze, ty, u_gen)
        end
      end
    end
  | (Delete, ZExp.Deeper(_, 
      ZExp.LetZP(
        (ZPat.CursorP(After, _) as zp),
        Some _,
        e1,
        e2)))
  | (Delete, ZExp.Deeper(_,
      (ZExp.LetZP (
        (ZPat.Deeper(_,
          (ZPat.OpSeqZ(_, (ZPat.CursorP(After, _)),
             (OperatorSeq.EmptySuffix _)))) as zp),
        Some _,
        e1, 
        e2)))) ->
    begin match UHExp.syn_fix_holes ctx u_gen e1 with 
    | None -> None
    | Some (e1, ty1, u_gen) -> 
      begin match ana_zpat_fix_holes ctx u_gen zp ty1 with 
      | None -> None
      | Some (zp, ctx, u_gen) -> 
        begin match UHExp.syn_fix_holes ctx u_gen e2 with 
        | None -> None
        | Some (e2, ty, u_gen) -> 
          let ze = 
            ZExp.Deeper(NotInHole, 
              (ZExp.LetZP(zp, None, e1, e2))) in 
          Some (ze, ty, u_gen)
        end
      end
    end
  | (Backspace, ZExp.Deeper(_,  
      (ZExp.LamZA(p, ZTyp.CursorT(Before, _), e1))))
  | (Backspace, ZExp.Deeper(_,
      (ZExp.LamZA(p,
        ZTyp.OpSeqZ(_,
          (ZTyp.CursorT(Before, _)),
          (OperatorSeq.EmptyPrefix _)), e1)))) -> 
    begin match UHExp.ana_pat_fix_holes ctx u_gen false p HTyp.Hole with 
    | None -> None
    | Some (p, ctx, u_gen) -> 
      begin match UHExp.syn_fix_holes ctx u_gen e1 with 
      | None -> None
      | Some (e1, ty2, u_gen) -> 
        let ze = ZExp.Deeper(NotInHole,  
          (ZExp.LamZP((ZPat.CursorP(After, p)), None, e1))) in 
        Some (ze, HTyp.Arrow(HTyp.Hole, ty2), u_gen)
      end
    end
  | (Delete, ZExp.Deeper(_,
      ZExp.LamZP(
        ((ZPat.CursorP(After, _)) as zp), 
        Some _,
        e1)))
  | (Delete, ZExp.Deeper(_, 
      ZExp.LamZP (
        (ZPat.Deeper(_,
          (ZPat.OpSeqZ(_, 
            (ZPat.CursorP(After, _)), 
            (OperatorSeq.EmptySuffix _)))) as zp),
        Some _,
        e1))) -> 
    begin match ana_zpat_fix_holes ctx u_gen zp HTyp.Hole with 
    | None -> None
    | Some (zp, ctx, u_gen) -> 
      begin match UHExp.syn_fix_holes ctx u_gen e1 with 
      | None -> None
      | Some (e1, ty2, u_gen) -> 
        let ze = ZExp.Deeper(NotInHole, (ZExp.LamZP(zp, None, e1))) in 
        Some (ze, HTyp.Arrow(HTyp.Hole, ty2), u_gen)
      end
    end
  | (Backspace, ZExp.Deeper(_, 
      (ZExp.OpSeqZ(_, 
        ((ZExp.CursorE(Before, e0)) as ze0),  
        ((OperatorSeq.EmptySuffix _) as surround)))))
  | (Backspace, ZExp.Deeper(_, 
      (ZExp.OpSeqZ(_, 
        ((ZExp.CursorE(Before, e0)) as ze0),  
        ((OperatorSeq.BothNonEmpty(_, _)) as surround))))) ->
    abs_perform_Backspace_Before_op 
      combine_for_Backspace_Space
      zexp_syn_fix_holes
      make_and_syn_OpSeqZ
      UHExp.is_EmptyHole
      UHExp.is_Space
      UHExp.Space
      (fun side e -> ZExp.CursorE (side, e))
      ctx u_gen e0 ze0 surround
  | (Delete, ZExp.Deeper(_, 
      (ZExp.OpSeqZ(_, 
        ((ZExp.CursorE(After, e0)) as ze0), 
        ((OperatorSeq.EmptyPrefix _) as surround)))))
  | (Delete, ZExp.Deeper(_,  
      (ZExp.OpSeqZ(_, 
        ((ZExp.CursorE(After, e0)) as ze0), 
        ((OperatorSeq.BothNonEmpty(_, _)) as surround))))) -> 
    abs_perform_Delete_After_op
      combine_for_Delete_Space
      zexp_syn_fix_holes
      make_and_syn_OpSeqZ
      UHExp.is_EmptyHole
      UHExp.is_Space
      UHExp.Space
      (fun side e -> ZExp.CursorE (side, e))
      ctx u_gen e0 ze0 surround
  | (Backspace, ZExp.CursorE((In _), e))
  | (Delete, ZExp.CursorE(In _, e)) -> 
    let (e', u_gen') = UHExp.new_EmptyHole u_gen in 
    let ze' = ZExp.CursorE(Before, e') in 
    Some (ze', HTyp.Hole, u_gen')
  (* Construction *)
  | (Construct SParenthesized, ZExp.CursorE(cursor_side, e)) -> 
    Some (
      ZExp.ParenthesizedZ ze, 
      ty,
      u_gen)
  | (Construct SAsc, ZExp.CursorE(_, e)) ->
    let e' = UHExp.bidelimit e in 
    Some (
      ZExp.Deeper(NotInHole,  
        (ZExp.AscZ2(e', (ZTyp.CursorT(Before, UHTyp.Hole))))), 
      ty, 
      u_gen)
  | (Construct SAsc, ZExp.Deeper(err_status, (ZExp.LetZP(zp, None, e1, e2)))) -> 
    begin match UHExp.syn ctx e1 with 
    | None -> None
    | Some ty1 -> 
      let uty1 = UHTyp.contract ty1 in 
      let ze = ZExp.Deeper(err_status,  
        ZExp.LetZA(ZPat.erase zp, ZTyp.place_Before uty1, e1, e2)) in
      Some (ze, ty, u_gen)
    end
  | (Construct SAsc, ZExp.Deeper(err_status, (ZExp.LamZP(zp, None, e1)))) -> 
    let ze = ZExp.Deeper(err_status,  
      (ZExp.LamZA((ZPat.erase zp), (ZTyp.place_Before UHTyp.Hole), e1))) in 
    Some (ze, ty, u_gen)
  | (Construct SAsc, ZExp.Deeper(err_status, (ZExp.LetZP(zp, (Some uty1), e1, e2)))) -> 
    (* just move the cursor over if there is already an ascription *)
    let ze = ZExp.Deeper(err_status, 
      (ZExp.LetZA((ZPat.erase zp), (ZTyp.place_Before uty1), e1, e2))) in 
    Some (ze, ty, u_gen)
  | (Construct SAsc, ZExp.Deeper(err_status, (ZExp.LamZP(zp, (Some uty1), e1)))) -> 
    (* just move the cursor over if there is already an ascription *)
    let ze = ZExp.Deeper(err_status, 
      (ZExp.LamZA((ZPat.erase zp), (ZTyp.place_Before uty1), e1))) in 
    Some (ze, ty, u_gen)
  | (Construct (SVar(x, side)), ZExp.CursorE(_, (UHExp.Tm(_, (UHExp.EmptyHole _))))) 
  | (Construct (SVar(x, side)), ZExp.CursorE(_, (UHExp.Tm(_, (UHExp.Var(_, _))))))
  | (Construct (SVar(x, side)), ZExp.CursorE(_, (UHExp.Tm(_, (UHExp.NumLit _))))) 
  | (Construct (SVar(x, side)), ZExp.CursorE(_, (UHExp.Tm(_, (UHExp.BoolLit _))))) ->
    Var.check_valid x ( 
    let (gamma, _) = ctx in 
    begin match VarMap.lookup gamma x with
    | Some xty -> Some (ZExp.CursorE(side , 
      (UHExp.Tm(NotInHole, (UHExp.Var(NotInVHole, x))))), 
      xty, u_gen)
    | None -> 
      let (u, u_gen) = MetaVarGen.next u_gen in 
      Some (ZExp.CursorE(side, 
        (UHExp.Tm(NotInHole, (UHExp.Var((InVHole u), x))))),
        HTyp.Hole, u_gen)
    end)
  | (Construct (SVar(_, _)), ZExp.CursorE(_, _)) -> None
  | (Construct SLet, ZExp.CursorE(_, e1)) ->
    let (zp, u_gen) = ZPat.new_EmptyHole u_gen in 
    let (e2, u_gen) = UHExp.new_EmptyHole u_gen in 
    let ze = ZExp.Deeper(NotInHole,  
      (ZExp.LetZP(zp, None, e1, e2))) in 
    Some (ze, HTyp.Hole, u_gen)
  | (Construct SLam, ZExp.CursorE(_, e1)) ->
    let (zp, u_gen) = ZPat.new_EmptyHole u_gen in 
    let ze = 
      ZExp.Deeper(NotInHole,  
        (ZExp.LamZP(zp, (Some UHTyp.Hole), e1))) in 
    let ty' = HTyp.Arrow(HTyp.Hole, ty) in 
    Some (ze, ty', u_gen)
  | (Construct (SNumLit(n, side)), ZExp.CursorE(_, (UHExp.Tm(_, (UHExp.EmptyHole _)))))
  | (Construct (SNumLit(n, side)), ZExp.CursorE(_, (UHExp.Tm(_, (UHExp.NumLit _)))))
  | (Construct (SNumLit(n, side)), ZExp.CursorE(_, (UHExp.Tm(_, (UHExp.BoolLit _)))))
  | (Construct (SNumLit(n, side)), ZExp.CursorE(_, (UHExp.Tm(_, (UHExp.Var(_, _)))))) ->
      Some (ZExp.CursorE(side, (UHExp.Tm(NotInHole, (UHExp.NumLit n)))), HTyp.Num, u_gen)
  | (Construct (SNumLit(_, _)), ZExp.CursorE(_, _)) -> None
  | (Construct (SBoolLit(b, side)), ZExp.CursorE(_, (UHExp.Tm(_, (UHExp.EmptyHole _)))))
  | (Construct (SBoolLit(b, side)), ZExp.CursorE(_, (UHExp.Tm(_, (UHExp.NumLit _)))))
  | (Construct (SBoolLit(b, side)), ZExp.CursorE(_, (UHExp.Tm(_, (UHExp.BoolLit _)))))
  | (Construct (SBoolLit(b, side)), ZExp.CursorE(_, (UHExp.Tm(_, (UHExp.Var(_, _)))))) ->
      Some (ZExp.CursorE(side, (UHExp.Tm(NotInHole, (UHExp.BoolLit b)))), HTyp.Bool, u_gen)
  | (Construct (SBoolLit(_, _)), ZExp.CursorE(_, _)) -> None
  | (Construct (SInj side), (ZExp.CursorE(_, e))) -> 
    let ze' = 
      ZExp.Deeper(NotInHole,  
        (ZExp.InjZ(side, ze))) in 
    let ty' = 
      begin match side with 
      | L -> HTyp.Sum(ty, HTyp.Hole)
      | R -> HTyp.Sum(HTyp.Hole, ty) 
      end in 
    Some (ze', ty', u_gen)
  | (Construct SListNil, ZExp.CursorE(_, (UHExp.Tm(_, (UHExp.EmptyHole _))))) -> 
    let ze = ZExp.CursorE(After, (UHExp.Tm(NotInHole, UHExp.ListNil))) in 
    let ty = HTyp.List HTyp.Hole in 
    Some (ze, ty, u_gen)
  | (Construct SListNil, ZExp.CursorE(_, _)) -> None
  | (Construct SCase, (ZExp.CursorE(_, e1))) ->
    begin match e1 with 
    | UHExp.Tm(_, (UHExp.EmptyHole _)) -> 
      let (rule_p, u_gen) = UHPat.new_EmptyHole u_gen in 
      let (rule_e, u_gen) = UHExp.new_EmptyHole u_gen in  
      let rule = UHExp.Rule(rule_p, rule_e) in 
      let rules = [rule] in 
      let caseze = ZExp.Deeper(NotInHole, (ZExp.CaseZE(ze, rules))) in 
      let ze = ZExp.Deeper(NotInHole, (ZExp.AscZ1(caseze, (UHTyp.Hole)))) in 
      Some (ze, HTyp.Hole, u_gen) 
    | _ -> 
      let (zp, u_gen) = ZPat.new_EmptyHole u_gen in 
      let (rule_e, u_gen) = UHExp.new_EmptyHole u_gen in 
      let zrule = ZExp.RuleZP(zp, rule_e) in  
      let zrules = ZList.singleton zrule in 
      let caseze = ZExp.Deeper(NotInHole, (ZExp.CaseZR(e1, zrules))) in 
      let ze = ZExp.Deeper(NotInHole, (ZExp.AscZ1(caseze, (UHTyp.Hole)))) in 
      Some (ze, HTyp.Hole, u_gen) 
    end 
  | (Construct (SOp os), ZExp.Deeper(_, (
      ZExp.OpSeqZ(_, ZExp.CursorE(In _, e), surround))))
  | (Construct (SOp os), ZExp.Deeper(_, (
      ZExp.OpSeqZ(_, ZExp.CursorE(After, e), surround)))) ->
    begin match exp_op_of os with
    | None -> None
    | Some op -> 
      abs_perform_Construct_SOp_After_surround 
        ZExp.new_EmptyHole
        make_and_syn_OpSeqZ
        UHExp.is_Space
        UHExp.Space
        (fun side e -> ZExp.CursorE (side, e))
        ctx u_gen e op surround
    end
  | (Construct (SOp os), 
      ZExp.Deeper(_, (ZExp.OpSeqZ(_,
        ((ZExp.CursorE(Before, _)) as ze0), surround)))) ->
    begin match exp_op_of os with 
    | None -> None
    | Some op -> 
      abs_perform_Construct_SOp_Before_surround
        ZExp.erase
        ZExp.new_EmptyHole
        make_and_syn_OpSeqZ
        UHExp.is_Space
        UHExp.Space
        (fun side e -> ZExp.CursorE (side, e))
        ctx u_gen ze0 op surround
    end
  | (Construct (SOp os), ZExp.CursorE(In _, e))
  | (Construct (SOp os), ZExp.CursorE(After, e)) ->
    begin match exp_op_of os with 
    | None -> None
    | Some op -> 
      abs_perform_Construct_SOp_After 
        UHExp.bidelimit
        ZExp.new_EmptyHole
        make_and_syn_OpSeqZ
        ctx u_gen e op
    end
  | (Construct (SOp os), ZExp.CursorE(Before, e)) -> 
    begin match exp_op_of os with 
    | None -> None
    | Some op -> 
      abs_perform_Construct_SOp_Before
        UHExp.bidelimit
        ZExp.new_EmptyHole
        make_and_syn_OpSeqZ
        ctx u_gen e op
    end
  | (Construct SRule, ZExp.CursorE(_, _)) -> None
  | (Construct (SApPalette name), ZExp.CursorE(_, (UHExp.Tm(_, (UHExp.EmptyHole _))))) -> 
    let (_, palette_ctx) = ctx in 
    begin match PaletteCtx.lookup palette_ctx name with 
    | Some palette_defn -> 
      let m_initial_model = UHExp.PaletteDefinition.initial_model palette_defn in 
      let (q, u_gen) = UHExp.HoleRefs.exec m_initial_model (UHExp.PaletteHoleData.empty) u_gen in 
      let (initial_model, initial_hole_data) = q in  
      let expansion_ty = UHExp.PaletteDefinition.expansion_ty palette_defn in
      let expansion = (UHExp.PaletteDefinition.to_exp palette_defn) initial_model in 
      let (_, initial_hole_map) = initial_hole_data in
      let expansion_ctx = UHExp.PaletteHoleData.extend_ctx_with_hole_map ctx initial_hole_map in
      begin match (UHExp.ana expansion_ctx expansion expansion_ty) with 
      | Some _ -> 
        Some (ZExp.CursorE(After, (UHExp.Tm(NotInHole, (UHExp.ApPalette(name, initial_model, initial_hole_data))))), 
              expansion_ty, u_gen)
      | None -> None
      end
    | None -> None
    end
  | (Construct (SApPalette _), ZExp.CursorE(_, _)) -> None
  | (UpdateApPalette monad, 
      ZExp.CursorE(_, (UHExp.Tm(_, (UHExp.ApPalette(name, _, hole_data)))))) -> 
    let (_, palette_ctx) = ctx in 
    begin match PaletteCtx.lookup palette_ctx name with 
    | Some palette_defn -> 
      let (q, u_gen') = UHExp.HoleRefs.exec monad hole_data u_gen in
      let (serialized_model, hole_data') = q in 
      let expansion_ty = UHExp.PaletteDefinition.expansion_ty palette_defn in
      let expansion = (UHExp.PaletteDefinition.to_exp palette_defn) serialized_model in 
      let (_, hole_map') = hole_data' in
      let expansion_ctx = UHExp.PaletteHoleData.extend_ctx_with_hole_map ctx hole_map' in
      begin match (UHExp.ana expansion_ctx expansion expansion_ty) with 
      | Some _ -> 
        Some (ZExp.CursorE(After, (UHExp.Tm(NotInHole, (UHExp.ApPalette(name, serialized_model, hole_data'))))), 
              expansion_ty, u_gen)
      | None -> None
      end
    | None -> None
    end
  | (UpdateApPalette _, ZExp.CursorE(_, _)) -> None
  (* Zipper Cases *)
  | (_, ZExp.ParenthesizedZ ze1) -> 
    begin match perform_syn ctx a (ze1, ty, u_gen) with 
    | Some (ze1', ty', u_gen') -> 
      Some (
        ZExp.ParenthesizedZ ze1',
        ty',
        u_gen')
    | None -> None
    end
  | (_, ZExp.Deeper(_, (ZExp.AscZ1(ze, uty1)))) ->
    let ty1 = UHTyp.expand uty1 in 
    begin match perform_ana u_gen ctx a ze ty1 with 
    | Some (ze', u_gen') -> 
      let ze'' = ZExp.bidelimit ze' in 
      Some (
        ZExp.Deeper(NotInHole, (ZExp.AscZ1(ze'', uty1))), 
        ty, 
        u_gen')
    | None -> None
    end
  | (_, ZExp.Deeper(_, (ZExp.AscZ2(e, zty)))) ->
    begin match perform_ty a zty with 
    | Some zty' -> 
      let uty' = ZTyp.erase zty' in
      let ty' = UHTyp.expand uty' in 
      begin match UHExp.ana_fix_holes ctx u_gen e ty' with 
      | None -> None
      | Some (e', u_gen') -> 
        Some (
          ZExp.Deeper(NotInHole, (ZExp.AscZ2(e', zty'))), 
          ty', 
          u_gen')
      end
    | None -> 
      None
    end
  | (_, ZExp.Deeper(_, (ZExp.LetZP(zp, ann, e1, e2)))) -> 
    begin match ann with 
    | Some uty1 -> 
      let ty1 = UHTyp.expand uty1 in 
      begin match perform_ana_pat ctx u_gen a zp ty1 with 
      | None -> None
      | Some (zp, ctx2, u_gen) -> 
        let p = ZPat.erase zp in 
        let ctx1 = UHExp.ctx_for_let ctx p ty1 e1 in 
        begin match UHExp.ana_fix_holes ctx1 u_gen e1 ty1 with 
        | None -> None
        | Some (e1, u_gen) -> 
          begin match UHExp.syn_fix_holes ctx2 u_gen e2 with 
          | None -> None
          | Some (e2, ty, u_gen) -> 
            let ze = ZExp.Deeper(NotInHole, (ZExp.LetZP(zp, ann, e1, e2))) in 
            Some (ze, ty, u_gen)
          end
        end
      end
    | None -> 
      begin match UHExp.syn ctx e1 with
      | None -> None
      | Some ty1 -> 
        begin match perform_ana_pat ctx u_gen a zp ty1 with 
        | None -> None
        | Some (zp, ctx2, u_gen) -> 
          begin match UHExp.syn_fix_holes ctx2 u_gen e2 with 
          | None -> None
          | Some (e2, ty, u_gen) -> 
            let ze = ZExp.Deeper(NotInHole, (ZExp.LetZP(zp, ann, e1, e2))) in 
            Some (ze, ty, u_gen)
          end
        end
      end
    end
  | (_, ZExp.Deeper(_, (ZExp.LetZA(p, zann, e1, e2)))) -> 
    (* (ctx) let p (ctx2) : ty = (ctx1) e1 in (ctx2) e2 *) 
    begin match perform_ty a zann with 
    | None -> None
    | Some zann -> 
      let ty1 = UHTyp.expand (ZTyp.erase zann) in 
      begin match UHExp.ana_pat_fix_holes ctx u_gen false p ty1 with 
      | None -> None
      | Some (p, ctx2, u_gen) -> 
        let ctx1 = UHExp.ctx_for_let ctx p ty1 e1 in 
        begin match UHExp.ana_fix_holes ctx1 u_gen e1 ty1 with 
        | None -> None
        | Some (e1, u_gen) -> 
          begin match UHExp.syn_fix_holes ctx2 u_gen e2 with 
          | None -> None
          | Some (e2, ty, u_gen) -> 
            let ze = ZExp.Deeper(NotInHole, (ZExp.LetZA(p, zann, e1, e2))) in 
            Some (ze, ty, u_gen)
          end
        end
      end
    end
  | (_, ZExp.Deeper(_, (ZExp.LetZE1(p, ann, ze1, e2)))) ->
    begin match ann with 
    | Some ann_ty -> 
      let ty1 = UHTyp.expand ann_ty in 
      let ctx1 = UHExp.ctx_for_let ctx p ty1 (ZExp.erase ze1) in  
      begin match perform_ana u_gen ctx1 a ze1 ty1 with 
      | None -> None
      | Some (ze1, u_gen) -> 
        let ze = ZExp.Deeper(NotInHole, (ZExp.LetZE1(p, ann, ze1, e2))) in 
        Some (ze, ty, u_gen)
      end
    | None -> 
      let e1 = ZExp.erase ze1 in
      begin match UHExp.syn ctx e1 with 
      | None -> None
      | Some ty1 -> 
        begin match perform_syn ctx a (ze1, ty1, u_gen) with
        | None -> None
        | Some (ze1, ty1, u_gen) -> 
          begin match UHExp.ana_pat_fix_holes ctx u_gen false p ty1 with 
          | None -> None
          | Some (p, ctx2, u_gen) -> 
            begin match UHExp.syn_fix_holes ctx2 u_gen e2 with 
            | None -> None
            | Some (e2, ty, u_gen) -> 
              let ze = ZExp.Deeper(NotInHole, (ZExp.LetZE1(p, ann, ze1, e2))) in 
              Some (ze, ty, u_gen)
            end
          end
        end
      end
    end
  | (_, ZExp.Deeper(_, (ZExp.LetZE2(p, ann, e1, ze2)))) ->
    let ty1 = 
      begin match ann with 
      | Some uty1 -> Some (UHTyp.expand uty1) 
      | None -> UHExp.syn ctx e1
      end in 
    begin match ty1 with 
    | None -> None
    | Some ty1 -> 
      begin match UHExp.ana_pat ctx p ty1 with 
      | None -> None
      | Some ctx2 -> 
        begin match perform_syn ctx2 a (ze2, ty, u_gen) with 
        | None -> None
        | Some (ze2, ty, u_gen) -> 
          let ze = ZExp.Deeper(NotInHole, (ZExp.LetZE2(p, ann, e1, ze2))) in 
          Some (ze, ty, u_gen)
        end
      end
    end
  | (_, ZExp.Deeper(_, (ZExp.LamZP(zp, ann, e1)))) -> 
    let ty1 = 
      begin match ann with 
      | Some uty1 -> UHTyp.expand uty1
      | None -> HTyp.Hole
      end in 
    begin match perform_ana_pat ctx u_gen a zp ty1 with 
    | None -> None
    | Some (zp, ctx, u_gen) -> 
      begin match UHExp.syn_fix_holes ctx u_gen e1 with 
      | None -> None
      | Some (e1, ty2, u_gen) -> 
        let ty = HTyp.Arrow(ty1, ty2) in 
        let ze = ZExp.Deeper(NotInHole, (ZExp.LamZP(zp, ann, e1))) in 
        Some (ze, ty, u_gen)
      end
    end
  | (_, ZExp.Deeper(_, (ZExp.LamZA(p, zann, e1)))) -> 
    begin match perform_ty a zann with 
    | None -> None
    | Some zann -> 
      let ty1 = UHTyp.expand (ZTyp.erase zann) in 
      begin match UHExp.ana_pat_fix_holes ctx u_gen false p ty1 with 
      | None -> None
      | Some (p, ctx, u_gen) -> 
        begin match UHExp.syn_fix_holes ctx u_gen e1 with 
        | None -> None
        | Some (e1, ty2, u_gen) -> 
          let ze = ZExp.Deeper(NotInHole, (ZExp.LamZA(p, zann, e1))) in 
          Some (ze, HTyp.Arrow(ty1, ty2), u_gen)
        end
      end
    end
  | (_, ZExp.Deeper(_, (ZExp.LamZE(p, ann, ze1)))) -> 
    begin match HTyp.matched_arrow ty with 
    | None -> None 
    | Some (_, ty2) -> 
      let ty1 = 
        begin match ann with 
        | Some uty1 -> UHTyp.expand uty1
        | None -> HTyp.Hole
        end in 
      begin match UHExp.ana_pat ctx p ty1 with 
      | None -> None
      | Some ctx -> 
        begin match perform_syn ctx a (ze1, ty2, u_gen) with 
        | None -> None
        | Some (ze1, ty2, u_gen) -> 
          let ze = ZExp.Deeper(NotInHole, (ZExp.LamZE(p, ann, ze1))) in 
          Some (ze, HTyp.Arrow(ty1, ty2), u_gen)
        end
      end
    end
  | (_, ZExp.Deeper(_, (ZExp.InjZ(side, ze1)))) -> 
    begin match ty with 
    | HTyp.Sum(ty1, ty2) -> 
      let ty_side = pick_side side ty1 ty2 in 
      begin match perform_syn ctx a (ze1, ty_side, u_gen) with 
      | None -> None
      | Some (ze1', ty_side', u_gen') -> 
        let ty' = 
          begin match side with 
          | L -> HTyp.Sum(ty_side', ty2)
          | R -> HTyp.Sum(ty1, ty_side')
          end in 
        Some (
          ZExp.Deeper(NotInHole, (ZExp.InjZ(side, ze1'))),
          ty',
          u_gen')
      end
    | _ -> None (* should never happen *)
    end
  | (_, ZExp.Deeper(_, (ZExp.OpSeqZ(_, ze0, surround)))) -> 
    let i = OperatorSeq.surround_prefix_length surround in 
    begin match ZExp.erase ze with 
    | UHExp.Tm(_, (UHExp.OpSeq(skel, seq))) -> 
      begin match UHExp.syn_skel ctx skel seq (Some i) with 
      | Some (ty, Some mode) ->
          begin match mode with 
          | UHExp.AnalyzedAgainst ty0 -> 
            begin match perform_ana u_gen ctx a ze0 ty0 with 
            | None -> None
            | Some (ze0', u_gen) -> 
              let ze0'' = ZExp.bidelimit ze0' in  
              Some (
                ZExp.Deeper(NotInHole, (ZExp.OpSeqZ(skel, ze0'', surround))), 
                ty, u_gen)
            end
          | UHExp.Synthesized ty0 ->
            begin match perform_syn ctx a (ze0, ty0, u_gen) with 
            | None -> None
            | Some (ze0', ty0', u_gen) -> 
              let ze0'' = ZExp.bidelimit ze0' in 
              make_and_syn_OpSeqZ ctx u_gen ze0'' surround 
            end
          end
      | Some _ -> None (* should never happen *)
      | None -> None (* should never happen *)
      end
    | _ -> None (* should never happen *)
    end
  | (_, ZExp.Deeper(_, (ZExp.ApPaletteZ(name, serialized_model, z_hole_data)))) -> 
    let (next_lbl, z_nat_map) = z_hole_data in 
    let (rest_map, z_data) = z_nat_map in 
    let (cell_lbl, cell_data) = z_data in
    let (cell_ty, cell_ze) = cell_data in 
    begin match perform_ana u_gen ctx a cell_ze cell_ty with 
    | None -> None
    | Some(cell_ze', u_gen') -> 
        let z_hole_data' = (next_lbl, (rest_map, (cell_lbl, (cell_ty, cell_ze')))) in 
        Some(
          ZExp.Deeper(NotInHole, (ZExp.ApPaletteZ(name, serialized_model, z_hole_data'))),
          ty, 
          u_gen')
    end
  | (_, ZExp.Deeper(_, (ZExp.CaseZE(_, _)))) -> None
  | (_, ZExp.Deeper(_, (ZExp.CaseZR(_, _)))) -> None
  (* Invalid actions at expression level *)
  | (Construct SNum, _)
  | (Construct SBool, _) 
  | (Construct SList, _) 
  | (Construct SWild, _) -> None
  end
and perform_ana 
  (u_gen : MetaVarGen.t) 
  (ctx: Contexts.t) 
  (a: t) 
  (ze: ZExp.t) 
  (ty: HTyp.t)
  : (ZExp.t * MetaVarGen.t) option =
  begin match (a, ze) with
  | (_, ZExp.Deeper(InHole(TypeInconsistent, u), ze1')) -> 
    let ze' = ZExp.Deeper(NotInHole, ze1') in 
    let e' = ZExp.erase ze' in 
    begin match UHExp.syn ctx e' with
    | Some ty1 -> 
      begin match perform_syn ctx a (ze', ty1, u_gen) with 
      | Some (ze', ty1', u_gen') -> 
        if HTyp.consistent ty1' ty then 
          Some (ze', u_gen')
        else 
          Some (ZExp.set_inconsistent u ze', u_gen')
      | None -> None
      end
    | None -> None
    end
  (* Movement *)
  | (MoveTo path, _) -> 
    let e = ZExp.erase ze in
    begin match Path.follow_e path e with
    | Some ze' -> Some (ze', u_gen)
    | None -> None
    end
  | (MoveToPrevHole, _) ->
    begin match Path.prev_hole_path ze with
    | None -> None
    | Some path -> perform_ana u_gen ctx (MoveTo path) ze ty
    end
  | (MoveToNextHole, _) ->
    begin match Path.next_hole_path ze with
    | None -> None
    | Some path ->
      (* [debug] let path = Helper.log_path path in *)
      perform_ana u_gen ctx (MoveTo path) ze ty
    end
  (* Backspace & Delete *)
  | (Backspace, ZExp.CursorE(After, e)) -> 
    begin match e with 
    | UHExp.Tm(_, (UHExp.EmptyHole _)) -> 
      Some (ZExp.CursorE(Before, e), u_gen)
    | _ -> 
      let (e', u_gen) = UHExp.new_EmptyHole u_gen in 
      Some (ZExp.CursorE(Before, e'), u_gen)
    end
  | (Backspace, ZExp.CursorE(Before, e)) -> None
  | (Delete, ZExp.CursorE(Before, e)) -> 
    begin match e with 
    | UHExp.Tm(_, (UHExp.EmptyHole _)) -> 
      Some (ZExp.CursorE(After, e), u_gen)
    | _ -> 
      let (e', u_gen) = UHExp.new_EmptyHole u_gen in 
      Some (ZExp.CursorE(Before, e'), u_gen)
    end
  | (Delete, ZExp.CursorE(After, e)) -> None
  | (Backspace, ZExp.CursorE(In _, e))
  | (Delete, ZExp.CursorE(In _, e)) -> 
    let (e', u_gen) = UHExp.new_EmptyHole u_gen in 
    let ze' = ZExp.CursorE(Before, e') in 
    Some (ze', u_gen)
  | (Backspace, 
      ZExp.Deeper(_, ZExp.AscZ2(e1,  
        (ZTyp.CursorT(Before, uty1))))) -> 
    let ze' = ZExp.CursorE(After, e1) in 
    zexp_ana_fix_holes ctx u_gen ze' ty
  | (Backspace,
      ZExp.Deeper(_, (ZExp.AscZ2(e1,  
        (ZTyp.OpSeqZ(_,  
          (ZTyp.CursorT(Before, _)),
          (OperatorSeq.EmptyPrefix _))))))) -> 
    let ze' = ZExp.CursorE(After, e1) in 
    zexp_ana_fix_holes ctx u_gen ze' ty
  | (Delete,
      ZExp.Deeper(_, (ZExp.AscZ1(
        (ZExp.CursorE(After, e1)),
        _)))) -> 
    begin match UHExp.ana_fix_holes ctx u_gen e1 ty with 
    | Some (e1', u_gen) -> 
      let ze' = ZExp.CursorE(After, e1') in 
      Some (ze', u_gen)
    | None -> None
    end
  | (Backspace, ZExp.Deeper(_,  
      (ZExp.LetZA(p, (ZTyp.CursorT(Before, _)), e1, e2))))
  | (Backspace, ZExp.Deeper(_, 
      (ZExp.LetZA(p,   
        (ZTyp.OpSeqZ(_, 
          (ZTyp.CursorT(Before, _)),
          (OperatorSeq.EmptyPrefix _))), e1, e2)))) ->  
    begin match UHExp.syn_fix_holes ctx u_gen e1 with 
    | None -> None
    | Some (e1, ty1, u_gen) -> 
      begin match UHExp.ana_pat_fix_holes ctx u_gen false p ty1 with 
      | None -> None
      | Some (p, ctx, u_gen) -> 
        begin match UHExp.ana_fix_holes ctx u_gen e2 ty with 
        | None -> None
        | Some (e2, u_gen) -> 
          let ze = 
            ZExp.Deeper(NotInHole, 
              (ZExp.LetZP((ZPat.place_After p), None, e1, e2))) in 
          Some (ze, u_gen)
        end
      end
    end
  | (Delete, ZExp.Deeper(_, 
      ZExp.LetZP ((ZPat.CursorP(After, _) as zp), Some _, e1, e2)))
  | (Delete, ZExp.Deeper(_, 
      (ZExp.LetZP( 
        ((ZPat.Deeper(_, (ZPat.OpSeqZ(_, 
          (ZPat.CursorP(After, _)),
          (OperatorSeq.EmptySuffix _))))) as zp),
        (Some _),
        e1, e2)))) -> 
    begin match UHExp.syn_fix_holes ctx u_gen e1 with 
    | None -> None
    | Some (e1, ty1, u_gen) -> 
      begin match ana_zpat_fix_holes ctx u_gen zp ty1 with 
      | None -> None
      | Some (zp, ctx, u_gen) -> 
        begin match UHExp.ana_fix_holes ctx u_gen e2 ty with 
        | None -> None
        | Some (e2, u_gen) -> 
          let ze = ZExp.Deeper(NotInHole, (ZExp.LetZP(zp, None, e1, e2))) in 
          Some (ze, u_gen)
        end
      end
    end
  | (Backspace, ZExp.Deeper(_,  
      (ZExp.LamZA(p, (ZTyp.CursorT(Before, _)), e1))))
  | (Backspace, ZExp.Deeper(_,
      ZExp.LamZA(p,
        ZTyp.OpSeqZ(_,
          ZTyp.CursorT(Before, _),
          (OperatorSeq.EmptyPrefix _)), 
        e1))) ->
    begin match HTyp.matched_arrow ty with
    | None -> None
    | Some (ty1, ty2) -> 
      begin match UHExp.ana_pat_fix_holes ctx u_gen false p ty1 with 
      | None -> None
      | Some (p, ctx, u_gen) -> 
        begin match UHExp.ana_fix_holes ctx u_gen e1 ty2 with 
        | None -> None
        | Some (e1, u_gen) -> 
          let zp = ZPat.place_After p in 
          let ze = ZExp.Deeper(NotInHole, (ZExp.LamZP(zp, None, e1))) in 
          Some (ze, u_gen)
        end
      end
    end
  | (Delete, ZExp.Deeper(_, 
      (ZExp.LamZP(((ZPat.CursorP(After, _)) as zp), (Some _), e1)))) 
  | (Delete, ZExp.Deeper(_,
      (ZExp.LamZP(
        (ZPat.Deeper(_,
          (ZPat.OpSeqZ(_,
            ZPat.CursorP(After, _),
            OperatorSeq.EmptySuffix _))) as zp),
        Some _,
        e1)))) -> 
    begin match HTyp.matched_arrow ty with
    | None -> None
    | Some (ty1, ty2) -> 
      begin match ana_zpat_fix_holes ctx u_gen zp ty1 with 
      | None -> None
      | Some (zp, ctx, u_gen) -> 
        begin match UHExp.ana_fix_holes ctx u_gen e1 ty2 with 
        | None -> None
        | Some (e1, u_gen) -> 
          let ze = ZExp.Deeper(NotInHole, (ZExp.LamZP(zp, None, e1))) in 
          Some (ze, u_gen)
        end
      end
    end
  | (Backspace,
      ZExp.Deeper(_, 
        (ZExp.CaseZR(e1,  
          (prefix, ZExp.RuleZP((ZPat.CursorP(Before, _)), _), suffix)))))
  | (Backspace,
      ZExp.Deeper(_,
        (ZExp.CaseZR(e1, 
          (prefix, ZExp.RuleZP( 
            (ZPat.Deeper(_,
              (ZPat.OpSeqZ(_, 
                (ZPat.CursorP(Before, _)),
                (OperatorSeq.EmptyPrefix _))))), 
            _), suffix))))) -> 
    begin match suffix with 
    | [] -> 
      begin match prefix with 
      | [] -> 
        let (zrule, u_gen) = ZExp.empty_zrule u_gen in  
        let ze = ZExp.Deeper(NotInHole,  
          (ZExp.CaseZR(e1,  
            (prefix, zrule, suffix)))) in 
        Some (ze, u_gen)
      | _ :: _ -> 
        begin match List.rev prefix with 
        | [] -> None
        | (UHExp.Rule(p2, e2)) :: rev_prefix' -> 
          let prefix' = List.rev rev_prefix' in 
          let zrule = ZExp.RuleZP((ZPat.place_Before p2), e2) in 
          let ze = ZExp.Deeper(NotInHole,  
            (ZExp.CaseZR(e1,  
              (prefix', zrule, suffix)))) in 
          Some (ze, u_gen)
        end
      end
    | (UHExp.Rule(p2, e2)) :: suffix' -> 
      let zrule = ZExp.RuleZP((ZPat.place_Before p2), e2) in 
      let ze = ZExp.Deeper(NotInHole, 
        (ZExp.CaseZR(e1,  
          (prefix, zrule, suffix')))) in 
      Some (ze, u_gen)
    end
  | (Backspace, ZExp.Deeper(_, 
      (ZExp.OpSeqZ(_, 
        ((ZExp.CursorE(Before, e0)) as ze0),  
        ((OperatorSeq.EmptySuffix _) as surround)))))
  | (Backspace, ZExp.Deeper(_, 
      (ZExp.OpSeqZ(_, 
        ((ZExp.CursorE(Before, e0)) as ze0), 
        ((OperatorSeq.BothNonEmpty(_, _)) as surround))))) ->
    abs_perform_Backspace_Before_op 
      combine_for_Backspace_Space
      (fun ctx u_gen ze -> 
        zexp_ana_fix_holes ctx u_gen ze ty)
      (fun ctx u_gen ze surround -> 
        make_and_ana_OpSeqZ ctx u_gen ze surround ty) 
      UHExp.is_EmptyHole
      UHExp.is_Space
      UHExp.Space
      (fun side e -> ZExp.CursorE (side, e))
      ctx u_gen e0 ze0 surround
  | (Delete, ZExp.Deeper(_,
      (ZExp.OpSeqZ(_,
        ((ZExp.CursorE(After, e0)) as ze0),
        ((OperatorSeq.EmptyPrefix _) as surround)))))
  | (Delete, ZExp.Deeper(_,  
      (ZExp.OpSeqZ(_, 
        ((ZExp.CursorE(After, e0)) as ze0), 
        ((OperatorSeq.BothNonEmpty(_, _)) as surround))))) -> 
    abs_perform_Delete_After_op
      combine_for_Delete_Space
      (fun ctx u_gen ze -> 
        zexp_ana_fix_holes ctx u_gen ze ty)
      (fun ctx u_gen ze surround -> 
        make_and_ana_OpSeqZ ctx u_gen ze surround ty) 
      UHExp.is_EmptyHole
      UHExp.is_Space
      UHExp.Space
      (fun side e -> ZExp.CursorE (side, e))
      ctx u_gen e0 ze0 surround
  (* Construction *)
  | (Construct SParenthesized, ZExp.CursorE(_, e)) -> 
    Some (
      ZExp.ParenthesizedZ ze, 
      u_gen)
  | (Construct SAsc, ZExp.CursorE(_, e)) ->
    let e' = UHExp.bidelimit e in 
    let uty = UHTyp.contract ty in 
    Some (
      ZExp.Deeper(NotInHole,  
        (ZExp.AscZ2(e', (ZTyp.place_Before uty)))), 
      u_gen)
  | (Construct SAsc, ZExp.Deeper(err_status, (ZExp.LetZP(zp, None, e1, e2)))) -> 
    begin match UHExp.syn ctx e1 with 
    | None -> None
    | Some ty1 -> 
      let uty1 = UHTyp.contract ty1 in 
      let ze = ZExp.Deeper(err_status, 
        (ZExp.LetZA((ZPat.erase zp), (ZTyp.place_Before uty1), e1, e2))) in 
      Some (ze, u_gen)
    end
  | (Construct SAsc, ZExp.Deeper(err_status, (ZExp.LamZP(zp, None, e1)))) -> 
    let ze = ZExp.Deeper(err_status, 
      (ZExp.LamZA((ZPat.erase zp), (ZTyp.CursorT(Before, UHTyp.Hole)), e1))) in 
    Some (ze, u_gen)
  | (Construct SAsc, ZExp.Deeper(err_status, (ZExp.LetZP(zp, (Some uty1), e1, e2)))) -> 
    (* just move the cursor over if there is already an ascription *)
    let ze = ZExp.Deeper(err_status, 
      (ZExp.LetZA((ZPat.erase zp), (ZTyp.place_Before uty1), e1, e2))) in 
    Some (ze, u_gen)
  | (Construct SAsc, ZExp.Deeper(err_status, (ZExp.LamZP(zp, (Some uty1), e1)))) -> 
    (* just move the cursor over if there is already an ascription *)
    let ze = ZExp.Deeper(err_status, 
      (ZExp.LamZA((ZPat.erase zp), (ZTyp.place_Before uty1), e1))) in 
    Some (ze, u_gen)
  | (Construct SLet, ZExp.CursorE(_, e1)) -> 
    begin match UHExp.syn_fix_holes ctx u_gen e1 with 
    | Some (e1, ty1, u_gen) -> 
      let (zp, u_gen) = ZPat.new_EmptyHole u_gen in 
      let (e2, u_gen) = UHExp.new_EmptyHole u_gen in 
      let ze = ZExp.Deeper(NotInHole,  
        (ZExp.LetZP(zp, None, e1, e2))) in 
      Some (ze, u_gen)
    | None -> 
      let (zp, u_gen) = ZPat.new_EmptyHole u_gen in 
      let (e2, u_gen) = UHExp.new_EmptyHole u_gen in 
      let ann = Some (UHTyp.contract ty) in 
      let ze = ZExp.Deeper(NotInHole,  
        (ZExp.LetZP(zp, ann, e1, e2))) in 
      Some (ze, u_gen)
    end
  | (Construct SLam, ZExp.CursorE(_, e)) -> 
    begin match HTyp.matched_arrow ty with 
    | Some (_, ty2) -> 
      begin match UHExp.ana_fix_holes ctx u_gen e ty2 with 
      | None -> None
      | Some (e, u_gen) -> 
        let (zp, u_gen) = ZPat.new_EmptyHole u_gen in 
        let ze = ZExp.Deeper(NotInHole, (ZExp.LamZP(zp, None, e))) in 
        Some (ze, u_gen)
      end
    | None -> 
      begin match UHExp.syn_fix_holes ctx u_gen e with 
      | None -> None
      | Some (e, _, u_gen) -> 
        let (zp, u_gen) = ZPat.new_EmptyHole u_gen in 
        let (u, u_gen) = MetaVarGen.next u_gen in 
        let ze = ZExp.Deeper(InHole(TypeInconsistent, u), (ZExp.LamZP(zp, None, e))) in 
        Some (ze, u_gen)
      end
    end
  | (Construct (SInj side), ZExp.CursorE(cursor_side, e1)) -> 
    begin match HTyp.matched_sum ty with 
    | Some (tyL, tyR) -> 
       let ty1 = pick_side side tyL tyR in 
       begin match UHExp.ana_fix_holes ctx u_gen e1 ty1 with 
       | None -> None
       | Some (e1, u_gen) -> 
         let ze = ZExp.Deeper(NotInHole,  
           (ZExp.InjZ(side, (ZExp.CursorE(cursor_side, e1))))) in 
         Some (ze, u_gen) 
       end
    | None -> 
      begin match UHExp.syn_fix_holes ctx u_gen e1 with 
      | None -> None
      | Some (e1, _, u_gen) -> 
        let (u, u_gen) = MetaVarGen.next u_gen in 
        let ze = ZExp.Deeper((InHole(TypeInconsistent, u), 
          (ZExp.InjZ(side, 
            (ZExp.CursorE(cursor_side, e1)))))) in 
        Some (ze, u_gen)
      end
    end
  | (Construct SCase, ZExp.CursorE(_, e1)) -> 
    begin match e1 with 
    | UHExp.Tm(_, (UHExp.EmptyHole _)) -> 
      let (rule, u_gen) = UHExp.empty_rule u_gen in 
      let rules = [rule] in 
      let ze = ZExp.Deeper(NotInHole, (ZExp.CaseZE(ze, rules))) in 
      Some (ze, u_gen) 
    | _ -> 
      let (zrule, u_gen) = ZExp.empty_zrule u_gen in 
      let zrules = ZList.singleton zrule in 
      begin match UHExp.syn_fix_holes ctx u_gen e1 with 
      | None -> None
      | Some (e1, _, u_gen) ->  
        let ze = ZExp.Deeper(NotInHole, (ZExp.CaseZR(e1, zrules))) in 
        Some (ze, u_gen) 
      end
    end 
  | (Construct SRule, 
      ZExp.Deeper(_,  
        (ZExp.CaseZR(e1,  
          (prefix, 
            ZExp.RuleZP(
              (ZPat.CursorP(Before, p)), re),
          suffix))))) -> 
    let (zrule, u_gen) = ZExp.empty_zrule u_gen in 
    let prev_rule = UHExp.Rule(p, re) in 
    let suffix = prev_rule::suffix in 
    let ze = 
      ZExp.Deeper(NotInHole,  
        (ZExp.CaseZR(e1,  
          (prefix, 
           zrule, 
           suffix)))) in 
    Some (ze, u_gen)
  | (Construct SRule, 
      ZExp.Deeper(_,  
        (ZExp.CaseZR(e1, (
          prefix, 
          (ZExp.RuleZE(_,  
            ((ZExp.CursorE(After, _)))) as zrule),
          suffix
        )))
      )
    )
  | (Construct SRule, 
      ZExp.Deeper(_,  
        (ZExp.CaseZR(e1, (
          prefix, 
          (ZExp.RuleZE(_,  
            (ZExp.Deeper(_, (ZExp.OpSeqZ(_,  
              (ZExp.CursorE(After, _)),
              (OperatorSeq.EmptySuffix _)))))) as zrule),
          suffix)))))
  | (Construct SRule,
      ZExp.Deeper(_, 
        (ZExp.CaseZR(e1, 
          (prefix,
            (ZExp.RuleZP(
              (ZPat.CursorP(After, _)), 
              _) as zrule), suffix)))))
  | (Construct SRule,
      ZExp.Deeper(_,
        ZExp.CaseZR(e1,
          (prefix,
           (ZExp.RuleZP(
             ZPat.Deeper(_, 
              ZPat.OpSeqZ(_,
                ZPat.CursorP(After, _),
                OperatorSeq.EmptySuffix _
              )
             ),
             _
           ) as zrule),
          suffix)
        )
      )) -> 
    let prev_rule = ZExp.erase_rule zrule in 
    let (zrule, u_gen) = ZExp.empty_zrule u_gen in  
    let prefix = prefix @ [prev_rule] in 
    let ze = 
      ZExp.Deeper(NotInHole, 
        (ZExp.CaseZR(e1,  
          (prefix, 
           zrule,
           suffix)))) in 
    Some (ze, u_gen)
  | (Construct SRule, ZExp.CursorE(_, _)) -> None
  | (Construct (SOp os), ZExp.Deeper(_,
      ZExp.OpSeqZ(_, ZExp.CursorE(In _, e), surround)))
  | (Construct (SOp os), ZExp.Deeper(_,
      ZExp.OpSeqZ(_, ZExp.CursorE(After, e), surround))) ->
    begin match exp_op_of os with
    | None -> None
    | Some op -> 
      abs_perform_Construct_SOp_After_surround 
        ZExp.new_EmptyHole
        (fun ctx u_gen ze surround -> 
          make_and_ana_OpSeqZ ctx u_gen ze surround ty)
        UHExp.is_Space
        UHExp.Space
        (fun side e -> ZExp.CursorE (side, e))
        ctx u_gen e op surround
    end
  | (Construct (SOp os), 
      ZExp.Deeper(_, (ZExp.OpSeqZ(_, 
        ((ZExp.CursorE(Before, _)) as ze0), surround)))) ->
    begin match exp_op_of os with 
    | None -> None
    | Some op -> 
      abs_perform_Construct_SOp_Before_surround
        ZExp.erase
        ZExp.new_EmptyHole
        (fun ctx u_gen ze surround -> 
          make_and_ana_OpSeqZ ctx u_gen ze surround ty)
        UHExp.is_Space
        UHExp.Space
        (fun side e -> ZExp.CursorE (side, e))
        ctx u_gen ze0 op surround
    end
  | (Construct (SOp os), ZExp.CursorE(In _, e))
  | (Construct (SOp os), ZExp.CursorE(After, e)) -> 
    begin match exp_op_of os with 
    | None -> None
    | Some op -> 
      abs_perform_Construct_SOp_After 
        UHExp.bidelimit
        ZExp.new_EmptyHole
        (fun ctx u_gen ze surround -> 
          make_and_ana_OpSeqZ ctx u_gen ze surround ty)
        ctx u_gen e op
    end
  | (Construct (SOp os), ZExp.CursorE(Before, e)) -> 
    begin match exp_op_of os with 
    | None -> None
    | Some op -> 
      abs_perform_Construct_SOp_Before
        UHExp.bidelimit
        ZExp.new_EmptyHole
        (fun ctx u_gen ze surround -> 
          make_and_ana_OpSeqZ ctx u_gen ze surround ty)
        ctx u_gen e op
    end
  (* Zipper Cases *)
  | (_, ZExp.ParenthesizedZ ze1) -> 
    begin match perform_ana u_gen ctx a ze1 ty with 
    | Some (ze1', u_gen') -> 
      Some (
        ZExp.ParenthesizedZ ze1',
        u_gen')
    | None -> None
    end
  | (_, ZExp.Deeper(_, (ZExp.LetZP(zp, ann, e1, e2)))) -> 
    begin match ann with 
    | Some uty1 -> 
      let ty1 = UHTyp.expand uty1 in 
      begin match perform_ana_pat ctx u_gen a zp ty1 with 
      | None -> None
      | Some (zp, ctx2, u_gen) -> 
        let p = ZPat.erase zp in 
        let ctx1 = UHExp.ctx_for_let ctx p ty1 e1 in 
        begin match UHExp.ana_fix_holes ctx1 u_gen e1 ty1 with 
        | None -> None
        | Some (e1, u_gen) -> 
          begin match UHExp.ana_fix_holes ctx2 u_gen e2 ty with 
          | None -> None
          | Some (e2, u_gen) -> 
            let ze = ZExp.Deeper(NotInHole, (ZExp.LetZP(zp, ann, e1, e2))) in 
            Some (ze, u_gen)
          end
        end
      end
    | None -> 
      begin match UHExp.syn ctx e1 with
      | None -> None
      | Some ty1 -> 
        begin match perform_ana_pat ctx u_gen a zp ty1 with 
        | None -> None
        | Some (zp, ctx2, u_gen) -> 
          begin match UHExp.ana_fix_holes ctx2 u_gen e2 ty with 
          | None -> None
          | Some (e2, u_gen) -> 
            let ze = ZExp.Deeper(NotInHole, (ZExp.LetZP(zp, ann, e1, e2))) in 
            Some (ze, u_gen)
          end
        end
      end
    end
  | (_, ZExp.Deeper(_, (ZExp.LetZA(p, zann, e1, e2)))) -> 
    (* (ctx) let p (ctx2) : ty = (ctx1) e1 in (ctx2) e2 *) 
    begin match perform_ty a zann with 
    | None -> None
    | Some zann -> 
      let ty1 = UHTyp.expand (ZTyp.erase zann) in 
      begin match UHExp.ana_pat_fix_holes ctx u_gen false p ty1 with 
      | None -> None
      | Some (p, ctx2, u_gen) -> 
        let ctx1 = UHExp.ctx_for_let ctx p ty1 e1 in 
        begin match UHExp.ana_fix_holes ctx1 u_gen e1 ty1 with 
        | None -> None
        | Some (e1, u_gen) -> 
          begin match UHExp.ana_fix_holes ctx2 u_gen e2 ty with 
          | None -> None
          | Some (e2, u_gen) -> 
            let ze = ZExp.Deeper(NotInHole, (ZExp.LetZA(p, zann, e1, e2))) in 
            Some (ze, u_gen)
          end
        end
      end
    end
  | (_, ZExp.Deeper(_, (ZExp.LetZE1(p, ann, ze1, e2)))) ->
    begin match ann with 
    | Some ann_ty -> 
      let ty1 = UHTyp.expand ann_ty in 
      let ctx1 = UHExp.ctx_for_let ctx p ty1 (ZExp.erase ze1) in  
      begin match perform_ana u_gen ctx1 a ze1 ty1 with 
      | None -> None
      | Some (ze1, u_gen) -> 
        let ze = ZExp.Deeper(NotInHole, (ZExp.LetZE1(p, ann, ze1, e2))) in 
        Some (ze, u_gen)
      end
    | None -> 
      let e1 = ZExp.erase ze1 in
      begin match UHExp.syn ctx e1 with 
      | None -> None
      | Some ty1 -> 
        begin match perform_syn ctx a (ze1, ty1, u_gen) with
        | None -> None
        | Some (ze1, ty1, u_gen) -> 
          begin match UHExp.ana_pat_fix_holes ctx u_gen false p ty1 with 
          | None -> None
          | Some (p, ctx2, u_gen) -> 
            begin match UHExp.ana_fix_holes ctx2 u_gen e2 ty with 
            | None -> None
            | Some (e2, u_gen) -> 
              let ze = ZExp.Deeper(NotInHole, (ZExp.LetZE1(p, ann, ze1, e2))) in 
              Some (ze, u_gen)
            end
          end
        end
      end
    end
  | (_, ZExp.Deeper(_, (ZExp.LetZE2(p, ann, e1, ze2)))) ->
    let ty1 = 
      begin match ann with 
      | Some uty1 -> Some (UHTyp.expand uty1) 
      | None -> UHExp.syn ctx e1
      end in 
    begin match ty1 with 
    | None -> None
    | Some ty1 -> 
      begin match UHExp.ana_pat ctx p ty1 with 
      | None -> None
      | Some ctx2 -> 
        begin match perform_ana u_gen ctx2 a ze2 ty with 
        | None -> None
        | Some (ze2, u_gen) -> 
          let ze = ZExp.Deeper(NotInHole, (ZExp.LetZE2(p, ann, e1, ze2))) in 
          Some (ze, u_gen)
        end
      end
    end
  | (_, ZExp.Deeper(_, (ZExp.LamZP(zp, ann, e1)))) -> 
    begin match HTyp.matched_arrow ty with 
    | None -> None
    | Some (ty1_given, ty2) -> 
      let ty1 = 
        begin match ann with 
        | Some uty1 -> UHTyp.expand uty1
        | None -> ty1_given
        end in 
      begin match perform_ana_pat ctx u_gen a zp ty1 with 
      | None -> None
      | Some (zp, ctx, u_gen) -> 
        begin match UHExp.ana_fix_holes ctx u_gen e1 ty2 with 
        | None -> None
        | Some (e1, u_gen) -> 
          let ze = ZExp.Deeper(NotInHole, (ZExp.LamZP(zp, ann, e1))) in 
          Some (ze, u_gen)
        end
      end
    end
  | (_, ZExp.Deeper(_, (ZExp.LamZA(p, zann, e1)))) -> 
    begin match HTyp.matched_arrow ty with 
    | None -> None
    | Some (ty1_given, ty2) -> 
      begin match perform_ty a zann with 
      | None -> None
      | Some zann -> 
        let ty1 = UHTyp.expand (ZTyp.erase zann) in 
        begin match HTyp.consistent ty1 ty1_given with 
        | true -> 
          begin match UHExp.ana_pat_fix_holes ctx u_gen false p ty1 with 
          | None -> None
          | Some (p, ctx, u_gen) -> 
            begin match UHExp.ana_fix_holes ctx u_gen e1 ty2 with 
            | None -> None
            | Some (e1, u_gen) -> 
              let ze = ZExp.Deeper(NotInHole, (ZExp.LamZA(p, zann, e1))) in 
              Some (ze, u_gen)
            end
          end
        | false -> 
          begin match UHExp.ana_pat_fix_holes ctx u_gen false p ty1 with 
          | None -> None
          | Some (p, ctx, u_gen) -> 
            begin match UHExp.syn_fix_holes ctx u_gen e1 with 
            | None -> None
            | Some (e1, _, u_gen) -> 
              let (u, u_gen) = MetaVarGen.next u_gen in 
              let ze = ZExp.Deeper(InHole(TypeInconsistent, u), (ZExp.LamZA(p, zann, e1))) in 
              Some (ze, u_gen)
            end
          end
        end
      end
    end
  | (_, ZExp.Deeper(_, (ZExp.LamZE(p, ann, ze1)))) -> 
    begin match HTyp.matched_arrow ty with 
    | None -> None 
    | Some (_, ty2) -> 
      let ty1 = 
        begin match ann with 
        | Some uty1 -> UHTyp.expand uty1
        | None -> HTyp.Hole
        end in 
      begin match UHExp.ana_pat ctx p ty1 with 
      | None -> None
      | Some ctx -> 
        begin match perform_ana u_gen ctx a ze1 ty2 with 
        | None -> None
        | Some (ze1, u_gen) -> 
          let ze = ZExp.Deeper(NotInHole, (ZExp.LamZE(p, ann, ze1))) in 
          Some (ze, u_gen)
        end
      end
    end
  | (_, ZExp.Deeper(_, (ZExp.InjZ(side, ze)))) ->
    begin match HTyp.matched_sum ty with 
    | Some (ty1, ty2) -> 
      let picked = pick_side side ty1 ty2 in 
      begin match perform_ana u_gen ctx a ze picked with 
      | Some (ze', u_gen) -> Some (
          ZExp.Deeper(NotInHole, 
            ZExp.InjZ(side, ze')), u_gen)
      | None -> None
      end
    | None -> None
    end
  | (_, ZExp.Deeper(_, (ZExp.CaseZE(ze1, rules)))) ->
    begin match UHExp.syn ctx (ZExp.erase ze1) with 
    | None -> None
    | Some ty1 -> 
      begin match perform_syn ctx a (ze1, ty1, u_gen) with 
      | None -> None
      | Some (ze1, ty1, u_gen) -> 
        begin match UHExp.ana_rules_fix_holes ctx u_gen false rules ty1 ty with 
        | None -> None
        | Some (rules, u_gen) -> 
          let ze = ZExp.Deeper(NotInHole, (ZExp.CaseZE(ze1, rules))) in 
          Some (ze, u_gen)
        end
      end
    end
  | (_, ZExp.Deeper(_, (ZExp.CaseZR(e1, zrules)))) -> 
    begin match UHExp.syn ctx e1 with 
    | None -> None
    | Some ty1 -> 
      begin match ZList.prj_z zrules with 
      | ZExp.RuleZP(zp, e) -> 
        begin match perform_ana_pat ctx u_gen a zp ty1 with 
        | None -> None
        | Some(zp, ctx, u_gen) -> 
          begin match UHExp.ana_fix_holes ctx u_gen e ty with 
          | None -> None
          | Some (e, u_gen) -> 
            let zrule = ZExp.RuleZP(zp, e) in 
            let ze = ZExp.Deeper(NotInHole,  
              (ZExp.CaseZR(e1, (ZList.replace_z zrules zrule)))) in 
            Some (ze, u_gen)
          end
        end
      | ZExp.RuleZE(p, ze) ->
        begin match UHExp.ana_pat ctx p ty1 with 
        | None -> None
        | Some ctx -> 
          begin match perform_ana u_gen ctx a ze ty with 
          | None -> None
          | Some (ze, u_gen) -> 
            let zrule = ZExp.RuleZE(p, ze) in 
            let ze = ZExp.Deeper(NotInHole,  
              (ZExp.CaseZR(e1, (ZList.replace_z zrules zrule)))) in 
            Some (ze, u_gen)
          end
        end
      end
    end
  | (_, ZExp.Deeper(_, (ZExp.OpSeqZ(_, ze0, surround)))) -> 
    let i = OperatorSeq.surround_prefix_length surround in 
    begin match ZExp.erase ze with 
    | UHExp.Tm(_, (UHExp.OpSeq(skel, seq))) -> 
      begin match UHExp.ana_skel ctx skel seq ty (Some i) with 
      | Some (Some mode) ->
          begin match mode with 
          | UHExp.AnalyzedAgainst ty0 -> 
            begin match perform_ana u_gen ctx a ze0 ty0 with 
            | None -> None
            | Some (ze0', u_gen) -> 
              let ze0'' = ZExp.bidelimit ze0' in  
              Some (
                ZExp.Deeper(NotInHole, (ZExp.OpSeqZ(skel, ze0'', surround))), 
                u_gen)
            end
          | UHExp.Synthesized ty0 ->
            begin match perform_syn ctx a (ze0, ty0, u_gen) with 
            | None -> None
            | Some (ze0', ty0', u_gen) -> 
              let ze0'' = ZExp.bidelimit ze0' in 
              make_and_ana_OpSeqZ ctx u_gen ze0'' surround ty
            end
          end
      | Some _ -> None (* should never happen *)
      | None -> None (* should never happen *)
      end
    | _ -> None (* should never happen *)
    end
  (* Subsumption *)
  | (UpdateApPalette _, _)
  | (Construct (SApPalette _), _) 
  | (Construct (SVar(_, _)), _)
  | (Construct (SNumLit(_, _)), _)
  | (Construct (SBoolLit(_, _)), _)
  | (Construct SListNil, _)
  | (_, ZExp.Deeper(_, (ZExp.AscZ1(_, _))))
  | (_, ZExp.Deeper(_, (ZExp.AscZ2(_, _))))
  | (_, ZExp.Deeper(_, (ZExp.ApPaletteZ(_, _, _)))) -> 
    perform_ana_subsume u_gen ctx a ze ty
  (* Invalid actions at expression level *)
  | (Construct SNum, _)
  | (Construct SBool, _)
  | (Construct SList, _) 
  | (Construct SWild, _) -> None
  end
and perform_ana_subsume 
  (u_gen : MetaVarGen.t) 
  (ctx : Contexts.t) 
  (a : t) 
  (ze : ZExp.t) 
  (ty : HTyp.t)
  : (ZExp.t * MetaVarGen.t) option =
    begin match UHExp.syn ctx (ZExp.erase ze) with 
    | Some ty1 -> 
      begin match perform_syn ctx a (ze, ty1, u_gen) with 
      | Some (ze', ty1', u_gen') -> 
        if HTyp.consistent ty ty1' then 
          Some (ze', u_gen') 
        else 
          let (ze'', u_gen'') = ZExp.make_inconsistent u_gen' ze' in 
          Some (ze'', u_gen'')
      | None -> None
      end
    | None -> None
    end

let can_perform
  (ctx : Contexts.t)
  (edit_state : ZExp.t * HTyp.t * MetaVarGen.t) 
  (ci : ZExp.cursor_info)
  (a  : t)
  : bool = 
    begin match a with 
    | Construct SParenthesized -> true
    | Construct SAsc -> 
      let sort = ci.sort in 
      begin match sort with 
      | ZExp.IsExpr _ -> true
      | ZExp.IsPat _ -> true
      | ZExp.IsType -> false
      end
    | Construct SLet  
    | Construct SLam
    | Construct (SInj _)
    | Construct SCase -> 
      begin match ci.mode with 
      | ZExp.AnaOnly _ -> false
      | ZExp.AnaAnnotatedLambda(_, _)
      | ZExp.AnaTypeInconsistent(_, _)
      | ZExp.AnaWrongLength(_, _, _)
      | ZExp.AnaFree _
      | ZExp.AnaSubsumed(_, _)
      | ZExp.SynOnly _
      | ZExp.SynFree
      | ZExp.SynErrorArrow(_, _)
      | ZExp.SynMatchingArrow(_, _)
      | ZExp.SynFreeArrow _ -> true
      | ZExp.TypePosition -> false
      | ZExp.PatAnaOnly _
      | ZExp.PatAnaTypeInconsistent(_, _)
      | ZExp.PatAnaWrongLength(_, _, _) 
      | ZExp.PatAnaSubsumed(_, _)
      | ZExp.PatSynOnly _ -> false
      end
    | Construct SListNil
    | Construct (SApPalette _)
    -> 
      begin match ci.sort with
      | ZExp.IsExpr (UHExp.Tm(_, (UHExp.EmptyHole _))) -> true
      | ZExp.IsExpr _ -> false
      | ZExp.IsPat (UHPat.Pat(_, (UHPat.EmptyHole _))) -> true
      | ZExp.IsPat _ -> false
      | ZExp.IsType -> false
      end
    | (Construct (SOp SArrow))
    | (Construct (SOp SVBar))
    | Construct SList -> 
      begin match ci.sort with 
      | ZExp.IsType -> true
      | ZExp.IsExpr _ 
      | ZExp.IsPat _ -> false
      end
    | Construct (SVar(_, _)) (* see can_enter_varchar below *) 
    | Construct SWild
    | Construct (SNumLit(_, _)) (* see can_enter_numeral below *)
    | Construct (SBoolLit(_, _))
    | Construct SRule
    | Construct (SOp _)
    | Construct SNum  (* TODO enrich cursor_info to allow simplifying these type cases *)
    | Construct SBool (* TODO enrich cursor_info to allow simplifying these type cases *)
    | MoveTo _
    | MoveToNextHole
    | MoveToPrevHole
    | UpdateApPalette _
    | Delete
    | Backspace -> 
      begin match perform_syn ctx a edit_state with 
      | Some _ -> true
      | None -> false
      end
    end

let can_enter_varchar 
  (ci : ZExp.cursor_info)
  : bool =
    begin match ci.sort with 
    | ZExp.IsExpr (UHExp.Tm(_, (UHExp.Var(_, _))))
    | ZExp.IsExpr (UHExp.Tm(_, (UHExp.EmptyHole _)))
    | ZExp.IsExpr (UHExp.Tm(_, (UHExp.BoolLit _)))
    | ZExp.IsPat (UHPat.Pat(_, (UHPat.Var _)))
    | ZExp.IsPat (UHPat.Pat(_, (UHPat.EmptyHole _)))
    | ZExp.IsPat (UHPat.Pat(_, (UHPat.BoolLit _)))
    -> true
    | ZExp.IsExpr (UHExp.Tm(_, (UHExp.NumLit _)))
    | ZExp.IsPat (UHPat.Pat(_, (UHPat.NumLit _)))
    -> 
      begin match ci.side with 
      | Before -> true
      | In _ | After -> false
      end
    | ZExp.IsExpr _
    | ZExp.IsPat _
    | ZExp.IsType 
    -> false
    end

let can_enter_numeral
  (ci : ZExp.cursor_info)
  : bool = 
    begin match ci.sort with 
    | ZExp.IsExpr (UHExp.Tm(_, (UHExp.NumLit _)))
    | ZExp.IsExpr (UHExp.Tm(_, (UHExp.EmptyHole _)))
    | ZExp.IsPat (UHPat.Pat(_, (UHPat.NumLit _)))
    | ZExp.IsPat (UHPat.Pat(_, (UHPat.EmptyHole _))) -> true
    | ZExp.IsExpr _ 
    | ZExp.IsPat _
    | ZExp.IsType -> false
    end
 
let can_construct_palette
  (ci : ZExp.cursor_info)
  : bool = 
    begin match ci.sort with 
    | ZExp.IsExpr (UHExp.Tm(_, (UHExp.EmptyHole _))) -> true
    | _ -> false
    end

