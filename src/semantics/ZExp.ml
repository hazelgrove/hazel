type nat = int

type cursor_side = SemanticsCommon.cursor_side

type opseq_surround = (UHExp.t, UHExp.op) OperatorSeq.opseq_surround
type opseq_prefix = (UHExp.t, UHExp.op) OperatorSeq.opseq_prefix
type opseq_suffix = (UHExp.t, UHExp.op) OperatorSeq.opseq_suffix

type t = 
| CursorE of cursor_side * UHExp.t
(* | CursorPalette : PaletteName.t -> PaletteSerializedModel.t -> hole_ref -> t -> t *)
| Deeper of err_status * t'
| ParenthesizedZ of t
and t' = 
| AscZ1 of t * UHTyp.t
| AscZ2 of UHExp.t * ZTyp.t 
| LetZP of ZPat.t * UHTyp.t option * UHExp.t * UHExp.t 
| LetZA of UHPat.t * ZTyp.t * UHExp.t * UHExp.t 
| LetZE1 of UHPat.t * UHTyp.t option * t * UHExp.t 
| LetZE2 of UHPat.t * UHTyp.t option * UHExp.t * t 
| LamZP of ZPat.t * UHTyp.t option * UHExp.t 
| LamZA of UHPat.t * ZTyp.t * UHExp.t 
| LamZE of UHPat.t * UHTyp.t option * t 
| InjZ of inj_side * t 
| CaseZE of t * UHExp.rule list 
| CaseZR of UHExp.t * (zrule, UHExp.rule) ZList.t  
| OpSeqZ of UHExp.skel_t * t * OperatorSeq.opseq_surround UHExp.t UHExp.op 
| ApPaletteZ of PaletteName.t * 
               PaletteSerializedModel.t * 
               (UHExp.PaletteHoleData.hole_ref_lbl * 
                ((HTyp.t * UHExp.t), (HTyp.t * t)) ZNatMap.t) 
and zrule = 
| RuleZP of ZPat.t * UHExp.t 
| RuleZE of UHPat.t * t 

type zrules = (zrule, UHExp.rule) ZList.t

module ZPaletteHoleData = struct
  type z_hole_map = ((HTyp.t * UHExp.t), (HTyp.t * t)) ZNatMap.t
  type t = (UHExp.PaletteHoleData.hole_ref_lbl * z_hole_map)
end

let bidelimit ze = 
  begin match ze with 
  | CursorE(cursor_side, e) -> 
    CursorE(cursor_side, (UHExp.bidelimit e))
  | ParenthesizedZ _ 
  | Deeper(_, (InjZ(_, _)))
  | Deeper(_, (ApPaletteZ(_, _, _)))
  | Deeper(_, (CaseZE(_, _))) 
  | Deeper(_, (CaseZR(_, _))) 
  (* | Deeper _ (ListLitZ _) *) 
    -> ze
  | Deeper(_, (AscZ1(_, _))) 
  | Deeper(_, (AscZ2(_, _)))  
  | Deeper(_, (LetZP(_, _, _, _))) 
  | Deeper(_, (LetZA(_, _, _, _)))
  | Deeper(_, (LetZE1(_, _, _, _)))
  | Deeper(_, (LetZE2(_, _, _, _)))
  | Deeper(_, (LamZP(_, _, _)))
  | Deeper(_, (LamZA(_, _, _)))
  | Deeper(_, (LamZE(_, _, _)))
  | Deeper(_, (OpSeqZ(_, _, _))) -> 
    ParenthesizedZ ze
  end

let rec set_inconsistent
  (u : MetaVar.t)
  (ze : t)
  : t =
    begin match ze with 
    | CursorE(cursor_side, e) -> 
      let e' = UHExp.set_inconsistent u e in 
      (CursorE(cursor_side, e'))
    | Deeper(_, ze') -> 
      Deeper(InHole(TypeInconsistent, u), ze')
    | ParenthesizedZ ze1 -> 
      ParenthesizedZ (set_inconsistent u ze1)
    end

let rec make_inconsistent 
  (u_gen : MetaVarGen.t)
  (ze : t) 
  : (t * MetaVarGen.t) = 
    begin match ze with 
    | CursorE(cursor_side, e) -> 
      let (e', u_gen) = UHExp.make_inconsistent u_gen e in  
      (CursorE(cursor_side, e'), u_gen)
    | Deeper(InHole(TypeInconsistent, _), _) -> 
      (ze, u_gen)
    | Deeper(_, ze') -> 
      let (u', u_gen) = MetaVarGen.next u_gen in 
      (Deeper(InHole(TypeInconsistent, u'), ze'), u_gen)
    | ParenthesizedZ ze1 -> 
      let (ze1', u_gen) = make_inconsistent u_gen ze1 in 
      (ParenthesizedZ ze1, u_gen)
    end

let new_EmptyHole (u_gen : MetaVarGen.t) = 
  let (e, u_gen) = UHExp.new_EmptyHole(u_gen) in 
  (CursorE(Before, e), u_gen)

let rec cursor_on_outer_expr (ze : t) : (UHExp.t * cursor_side) option = 
  begin match ze with 
  | CursorE(side, e) -> Some ((UHExp.drop_outer_parentheses e), side)
  | ParenthesizedZ ze' -> cursor_on_outer_expr ze'
  | Deeper(_, _) -> None
  end

let empty_zrule (u_gen : MetaVarGen.t) : zrule * MetaVarGen.t = 
  let (zp, u_gen) = ZPat.new_EmptyHole(u_gen) in  
  let (rule_e, u_gen) = UHExp.new_EmptyHole(u_gen) in  
  let zrule = ZExp.RuleZP(zp, rule_e) in   
  (zrule, u_gen)

let rec erase (ze : t) : UHExp.t =
  begin match ze with
  | CursorE(_, e) -> e
  | Deeper(err_state, ze') -> 
    let e' = erase' ze' in 
    UHExp.Tm(err_state, e')
  | ParenthesizedZ ze1 -> 
    UHExp.Parenthesized (erase ze1)
  end
and erase' (ze : t') : UHExp.t' = 
  begin match ze with 
  | AscZ1(ze', ty) -> UHExp.Asc(erase ze', ty)
  | AscZ2(e', zty) -> UHExp.Asc(e', ZTyp.erase zty)
  | LetZP(zp, ann, e1, e2) -> UHExp.Let(ZPat.erase zp, ann, e1, e2)
  | LetZA(p, zann, e1, e2) -> UHExp.Let(p, (Some (ZTyp.erase zann)), e1, e2) 
  | LetZE1(p, ann, ze, e) -> UHExp.Let(p, ann, (erase ze), e)
  | LetZE2(p, ann, e, ze) -> UHExp.Let(p, ann, e, erase ze)
  | LamZP(zp, ann, e1) -> UHExp.Lam(ZPat.erase zp, ann, e1)
  | LamZA(p, zann, e1) -> UHExp.Lam(p, (Some (ZTyp.erase zann)), e1)
  | LamZE(p, ann, ze1) -> UHExp.Lam(p, ann, erase ze1)
  | InjZ(side, ze) -> UHExp.Inj(side, erase ze)
  (* | ListLitZ zes -> UHExp.ListLit (ZList.erase zes erase) *)  
  | CaseZE(ze1, rules) -> UHExp.Case(erase ze1, rules)
  | CaseZR(e1, zrules) -> UHExp.Case(e1, ZList.erase zrules erase_rule)
  | OpSeqZ(skel, ze', surround) -> 
     let e = erase ze' in 
     UHExp.OpSeq(skel, (OperatorSeq.opseq_of_exp_and_surround e surround))
  | ApPaletteZ(palette_name, serialized_model, zhole_data) -> 
     let (next_hole_ref, zholemap) = zhole_data in 
     let (holemap, z) = zholemap in 
     let (hole_ref, tz) = z in
     let (ty, ze) = tz in 
     let holemap' = NatMap.extend holemap (hole_ref, (ty, erase ze)) in 
     let hole_data' = (next_hole_ref, holemap') in 
     UHExp.ApPalette(palette_name, serialized_model, hole_data')
  end
and erase_rule (zr : zrule) : UHExp.rule = 
  begin match zr with 
  | RuleZP(zp, e) -> UHExp.Rule(ZPat.erase zp, e)
  | RuleZE(p, ze) -> UHExp.Rule(p, erase ze)
  end

type cursor_mode = 
(* cursor in analytic position *)
| AnaOnly of HTyp.t 
| AnaAnnotatedLambda of HTyp.t * HTyp.t 
| AnaTypeInconsistent of HTyp.t * HTyp.t 
| AnaWrongLength of 
    nat (* expected length *) * nat (* got length *) 
    * HTyp.t (* expected type *) 
| AnaFree of HTyp.t 
| AnaSubsumed of HTyp.t * HTyp.t 
(* cursor in synthetic position *)
| SynOnly of HTyp.t 
| SynFree 
| SynErrorArrow of HTyp.t (* expected *) * HTyp.t (* got *) 
| SynMatchingArrow of HTyp.t * HTyp.t 
| SynFreeArrow of HTyp.t 
(* cursor in type position *)
| TypePosition 
(* cursor in analytic pattern position *)
| PatAnaOnly of HTyp.t 
| PatAnaTypeInconsistent of HTyp.t * HTyp.t 
| PatAnaWrongLength of 
    nat (* expected length *) * nat (* got length *) 
    * HTyp.t (* expected type *) 
| PatAnaSubsumed of HTyp.t * HTyp.t 
(* cursor in synthetic pattern position *)
| PatSynOnly of HTyp.t 

type cursor_sort = 
| IsExpr of UHExp.t
| IsPat of UHPat.t
| IsType

type cursor_info = {
  mode : cursor_mode;
  sort : cursor_sort;
  side : cursor_side;
  ctx : Contexts.t
}

let mk_cursor_info mode sort side ctx = { mode=mode; sort=sort; side=side; ctx=ctx }

let update_sort (ci : cursor_info) (sort : cursor_sort) : cursor_info = 
  let { mode=mode; sort=_; side=side; ctx=ctx } = ci in 
  { mode=mode; sort=sort; side=side; ctx=ctx }

let rec ana_pat_cursor_found
  (ctx : Contexts.t)
  (p : UHPat.t)
  (ty : HTyp.t)
  (side : cursor_side)
  : cursor_info option = 
    begin match p with 
    | UHPat.Parenthesized p1 ->
      begin match ana_pat_cursor_found ctx p1 ty side with
      | None -> None
      | Some ci -> 
        Some (update_sort ci (IsPat p))
      end
    | UHPat.Pat(InHole(TypeInconsistent, _), p') -> 
      begin match UHExp.syn_pat' ctx p' with 
      | None -> None
      | Some (ty', _) -> 
        Some 
          (mk_cursor_info
            (PatAnaTypeInconsistent(ty, ty'))
            (IsPat p)
            side
            ctx)
      end
    | UHPat.Pat NotInHole (UHPat.EmptyHole _) -> 
      Some 
        (mk_cursor_info
          (PatAnaSubsumed(ty, HTyp.Hole))
          (IsPat p)
          side
          ctx)
    | UHPat.Pat(NotInHole, UHPat.Wild)
    | UHPat.Pat NotInHole (UHPat.Var _) ->
      Some
        (mk_cursor_info
          (PatAnaOnly ty)
          (IsPat p)
          side
          ctx)
    | UHPat.Pat NotInHole (UHPat.NumLit _) -> 
      Some
        (mk_cursor_info
          (PatAnaSubsumed(ty, HTyp.Num))
          (IsPat p)
          side
          ctx)
    | UHPat.Pat NotInhole (UHPat.BoolLit _) -> 
      Some
        (mk_cursor_info 
          (PatAnaSubsumed(ty, HTyp.Bool))
          (IsPat p)
          side
          ctx)
    | UHPat.Pat NotInHole (UHPat.Inj(_, _)) -> 
        Some
          (mk_cursor_info
            (PatAnaOnly ty)
            (IsPat p)
            side
            ctx)
    | UHPat.Pat(NotInHole, UHPat.ListNil) -> 
      Some
        (mk_cursor_info 
          (PatAnaOnly ty)
          (IsPat p)
          side
          ctx)
    (* | UHPat.Pat NotInHole (UHPat.ListLit _) -> 
      Some
        (mk_cursor_info
          (PatAnaOnly ty)
          (IsPat p)
          side
          ctx) *)
    | UHPat.Pat(NotInHole, (UHPat.OpSeq((Skel.BinOp(NotInHole, Comma, skel1, skel2)), seq))) -> 
      Some
        (mk_cursor_info 
          (PatAnaOnly ty)
          (IsPat p)
          side
          ctx)
    | UHPat.Pat(InHole(WrongLength, _), 
        (UHPat.OpSeq(Skel.BinOp(InHole(WrongLength, _), Comma, skel1, skel2), _))) -> 
      begin match ty with 
      | HTyp.Prod(ty1, ty2) -> 
        let n_elts = List.length (UHPat.get_tuple skel1 skel2) in 
        let n_types = List.length (HTyp.get_tuple ty1 ty2) in  
        Some
          (mk_cursor_info
            (PatAnaWrongLength(n_types, n_elts, ty))
            (IsPat p)
            side
            ctx)
      | _ -> None
      end
    | UHPat.Pat(InHole(WrongLength, _), _) -> None
    | UHPat.Pat(NotInHole, 
      (UHPat.OpSeq(Skel.BinOp(InHole(_, _), Comma, skel1, skel2), seq))) -> None 
    | UHPat.Pat(NotInHole, UHPat.OpSeq(Skel.Placeholder(_, _), _)) -> None
    end

let rec syn_pat_cursor_info
  (ctx : Contexts.t)
  (zp : ZPat.t)
  : cursor_info option = 
    begin match zp with 
    | ZPat.CursorP(side, p) -> 
      begin match UHExp.syn_pat ctx p with 
      | None -> None
      | Some (ty, _) -> 
        Some
          (mk_cursor_info
            (PatSynOnly ty)
            (IsPat p)
            side
            ctx)
      end
    | ZPat.Deeper(_, zp') -> 
      syn_pat_cursor_info' ctx zp'
    | ZPat.ParenthesizedZ zp1 -> 
      syn_pat_cursor_info ctx zp1
    end
and syn_pat_cursor_info'
  (ctx : Contexts.t)
  (zp' : ZPat.t')
  : cursor_info option = 
    begin match zp' with 
    | ZPat.InjZ(side, zp1) -> syn_pat_cursor_info ctx zp1
    (* | ZPat.ListLitZ ((prefix, zp), _) -> 
      begin match prefix with 
      | nil -> syn_pat_cursor_info ctx zp
      | cons _ _ -> 
        let opt_result = List.fold_left (fun opt_result p -> 
          begin match opt_result with 
          | None -> None
          | Some (ty, ctx) -> 
            begin match UHExp.syn_pat ctx p with 
            | Some (ty', ctx) -> 
              begin match HTyp.join ty ty' with 
              | Some ty_joined -> Some (ty_joined, ctx)
              | None -> 
                begin match UHExp.ana_pat ctx p ty with 
                | None -> None
                | Some ctx -> Some (ty, ctx)
                end
              end
            | None -> 
              begin match UHExp.ana_pat ctx p ty with 
              | None -> None
              | Some ctx -> Some (ty, ctx)
              end
            end
          end) prefix (Some (HTyp.Hole, ctx)) in 
        begin match opt_result with 
        | None -> None
        | Some (ty, ctx) -> ana_pat_cursor_info ctx zp ty
        end
      end *)
    | ZPat.OpSeqZ(skel, zp1, surround) -> 
      let p1 = ZPat.erase zp1 in 
      let seq = OperatorSeq.opseq_of_exp_and_surround p1 surround in 
      let n = OperatorSeq.surround_prefix_length surround in 
      syn_skel_pat_cursor_info ctx skel seq n zp1
    end 
and syn_skel_pat_cursor_info 
  (ctx : Contexts.t)
  (skel : UHPat.skel_t)
  (seq : UHPat.opseq)
  (n : nat)
  (zp1 : ZPat.t)
  : cursor_info option = 
    begin match skel with 
    | Skel.Placeholder(_, n') -> 
      if Nat.eqb n n' then 
        syn_pat_cursor_info ctx zp1
      else None
    | Skel.BinOp(_, UHPat.Comma, skel1, skel2) -> 
      begin match syn_skel_pat_cursor_info ctx skel1 seq n zp1 with 
      | (Some _) as result -> result
      | None -> syn_skel_pat_cursor_info ctx skel2 seq n zp1 
      end
    | Skel.BinOp(_, UHPat.Space, skel1, skel2) -> 
      begin match syn_skel_pat_cursor_info ctx skel1 seq n zp1 with 
      | (Some _) as result -> result
      | None -> syn_skel_pat_cursor_info ctx skel2 seq n zp1
      end
    | Skel.BinOp(_, UHPat.Cons, skel1, skel2) -> 
      begin match syn_skel_pat_cursor_info ctx skel1 seq n zp1 with 
      | (Some _) as result -> result
      | None -> 
        begin match UHExp.syn_skel_pat ctx skel1 seq None with 
        | None -> None
        | Some (ty_elt, ctx, _) -> 
          let list_ty = HTyp.List(ty_elt) in  
          ana_skel_pat_cursor_info ctx skel2 seq n zp1 list_ty
        end
      end
    end
and ana_pat_cursor_info
  (ctx : Contexts.t)
  (zp : ZPat.t) 
  (ty : HTyp.t)
  : cursor_info option = 
    begin match zp with  
    | ZPat.CursorP(side, p) -> 
      ana_pat_cursor_found ctx p ty side
    | ZPat.Deeper(InHole(TypeInconsistent, u), zp') -> 
      syn_pat_cursor_info' ctx zp'
    | ZPat.Deeper(NotInHole, zp') 
    | ZPat.Deeper(InHole(WrongLength, _), 
      ((ZPat.OpSeqZ(Skel.BinOp(_, UHPat.Comma, _, _), _, _) as zp'))) -> 
      ana_pat_cursor_info' ctx zp' ty
    | ZPat.Deeper(InHole(WrongLength, _), _) -> None
    | ZPat.ParenthesizedZ zp -> 
      ana_pat_cursor_info ctx zp ty
    end
and ana_pat_cursor_info'
  (ctx : Contexts.t)
  (zp' : ZPat.t')
  (ty : HTyp.t)
  : cursor_info option = 
    begin match zp' with 
    | ZPat.InjZ(side, zp1) -> 
      begin match HTyp.matched_sum ty with 
      | None -> None
      | Some (tyL, tyR) -> 
        let ty1 = pick_side side tyL tyR in 
        ana_pat_cursor_info ctx zp1 ty1
      end
    (* | ZPat.ListLitZ zps -> 
      begin match HTyp.matched_list ty with 
      | None -> None
      | Some ty_elt -> 
        let zp = ZList.prj_z zps in 
        ana_pat_cursor_info ctx zp ty_elt
      end *)
    | ZPat.OpSeqZ(skel, zp1, surround) -> 
      let p1 = ZPat.erase zp1 in 
      let seq = OperatorSeq.opseq_of_exp_and_surround p1 surround in 
      let n = OperatorSeq.surround_prefix_length surround in 
      ana_skel_pat_cursor_info ctx skel seq n zp1 ty
    end
and ana_skel_pat_cursor_info 
  (ctx : Contexts.t)
  (skel : UHPat.skel_t)
  (seq : UHPat.opseq)
  (n : nat)
  (zp1 : ZPat.t)
  (ty : HTyp.t)
  : cursor_info option = 
    begin match skel with 
    | Skel.Placeholder(_, n') -> 
      if Nat.eqb n n' then 
        ana_pat_cursor_info ctx zp1 ty
      else None
    | Skel.BinOp (InHole(TypeInconsistent, _), _, skel1, skel2) -> 
      syn_skel_pat_cursor_info ctx skel seq n zp1 
    | Skel.BinOp(NotInHole, UHPat.Comma, skel1, skel2) -> 
      begin match ty with 
      | HTyp.Hole -> 
        begin match ana_skel_pat_cursor_info ctx skel1 seq n zp1 HTyp.Hole with 
        | (Some _) as result -> result
        | None -> ana_skel_pat_cursor_info ctx skel2 seq n zp1 HTyp.Hole 
        end
      | HTyp.Prod(ty1, ty2) -> 
        let types = HTyp.get_tuple ty1 ty2 in 
        let skels = UHPat.get_tuple skel1 skel2 in 
        begin match Util.zip_eq skels types with 
        | None -> None
        | Some zipped -> 
          List.fold_left (fun opt_result (skel_ty : UHPat.skel_t * HTyp.t) -> 
            begin match opt_result with 
            | (Some _) as result -> result
            | None -> 
              let (skel, ty) = skel_ty in 
              ana_skel_pat_cursor_info ctx skel seq n zp1 ty
            end) zipped None
        end
      | _ -> None
      end
    | Skel.BinOp(InHole(WrongLength, _), UHPat.Comma, skel1, skel2) -> 
      begin match ty with 
      | HTyp.Prod(ty1, ty2) ->  
        let types = HTyp.get_tuple ty1 ty2 in 
        let skels = UHPat.get_tuple skel1 skel2 in 
        let (zipped, remainder) = HTyp.zip_with_skels skels types in 
        let ana_zipped = 
          List.fold_left (fun opt_result (skel_ty : UHPat.skel_t * HTyp.t) -> 
            begin match opt_result with 
            | (Some _) as result -> result
            | None -> 
              let (skel, ty) = skel_ty in 
              ana_skel_pat_cursor_info ctx skel seq n zp1 ty
            end) zipped None in 
        begin match ana_zipped with 
        | (Some _) as result -> result
        | None -> 
          List.fold_left (fun opt_result skel -> 
            begin match opt_result with 
            | (Some _) as result -> result
            | None -> syn_skel_pat_cursor_info ctx skel seq n zp1
            end) remainder None
        end
      | _ -> None
      end
    | Skel.BinOp(InHole(WrongLength, _), _, _, _) -> None
    | Skel.BinOp(NotInHole, UHPat.Cons, skel1, skel2) -> 
      begin match HTyp.matched_list ty with 
      | None -> None
      | Some ty_elt -> 
        begin match ana_skel_pat_cursor_info ctx skel1 seq n zp1 ty_elt with 
        | (Some _) as result -> result
        | None -> 
          let ty_list = HTyp.List(ty_elt) in  
          ana_skel_pat_cursor_info ctx skel2 seq n zp1 ty_list 
        end
      end
    | Skel.BinOp(NotInHole, UHPat.Space, _, _) -> 
      syn_skel_pat_cursor_info ctx skel seq n zp1 
    end

let rec ana_cursor_found
  (ctx : Contexts.t)
  (e : UHExp.t) (ty : HTyp.t) 
  (side : cursor_side)
  : cursor_info option = 
  begin match e with
  | UHExp.Parenthesized e' -> 
    begin match ana_cursor_found ctx e' ty side with
    | None -> None
    | Some ci -> 
      Some (update_sort ci (IsExpr e))
    end
  | UHExp.Tm(InHole(TypeInconsistent, _),
      (UHExp.OpSeq(Skel.BinOp(_, op, skel1, skel2), surround))) -> 
    let e' = UHExp.OpSeq(Skel.BinOp(NotInHole, op, skel1, skel2), surround) in 
    begin match UHExp.syn' ctx e' with 
    | None -> None
    | Some ty' -> 
      Some 
        (mk_cursor_info 
          (AnaTypeInconsistent(ty, ty'))
          (IsExpr e)
          side
          ctx)
    end
  | UHExp.Tm(InHole(TypeInconsistent, _), e') -> 
    begin match UHExp.syn' ctx e' with 
    | None -> None 
    | Some ty' -> 
      Some (
        mk_cursor_info 
          (AnaTypeInconsistent(ty, ty'))
          (IsExpr e)
          side
          ctx
      )
    end
  | UHExp.Tm(_, (UHExp.Var(InVHole _, _))) -> 
    Some (
      mk_cursor_info
        (AnaFree ty)
        (IsExpr e)
        side
        ctx
    )
  | UHExp.Tm NotInHole (UHExp.Let(_, _, _, _))
  | UHExp.Tm NotInHole (UHExp.Case(_, _)) 
  | UHExp.Tm(NotInHole, UHExp.ListNil) 
  (* | UHExp.Tm NotInHole (UHExp.ListLit _) *) ->
    Some (
      mk_cursor_info 
        (AnaOnly ty)
        (IsExpr e)
        side
        ctx
    )
  | UHExp.Tm(NotInHole, (UHExp.OpSeq(Skel.BinOp(NotInHole, UHExp.Comma, _, _), surround)))
  | UHExp.Tm(NotInHole, (UHExp.OpSeq(Skel.BinOp(NotInHole, UHExp.Cons, _, _), surround))) -> 
    Some
      (mk_cursor_info
        (AnaOnly ty)
        (IsExpr e)
        side
        ctx)
  | UHExp.Tm(InHole(WrongLength, _), (UHExp.OpSeq(Skel.BinOp(InHole(WrongLength, _),
      UHExp.Comma, skel1, skel2), _))) -> 
    begin match ty with 
    | HTyp.Prod(ty1, ty2) -> 
      let n_elts = List.length (UHExp.get_tuple skel1 skel2) in 
      let n_types = List.length (HTyp.get_tuple ty1 ty2) in 
      Some (mk_cursor_info
        (AnaWrongLength(n_types, n_elts, ty))
        (IsExpr e)
        side
        ctx)
    | _ -> None
    end
  | UHExp.Tm(InHole(WrongLength, _), _) -> None
  | UHExp.Tm(NotInHole(UHExp.OpSeq(Skel.BinOp(InHole(WrongLength, _), _, _, _), _))) -> None
  | UHExp.Tm(NotInHole(UHExp.OpSeq(Skel.BinOp(NotInHole, UHExp.Plus, _, _), _))) 
  | UHExp.Tm(NotInHole(UHExp.OpSeq(Skel.BinOp(NotInHole, UHExp.Times, _, _), _)))
  | UHExp.Tm(NotInHole(UHExp.OpSeq(Skel.BinOp(NotInHole, UHExp.LessThan, _, _), _)))
  | UHExp.Tm(NotInHole(UHExp.OpSeq(Skel.BinOp(NotInHole, UHExp.Space, _, _), _)))
  | UHExp.Tm(NotInHole(UHExp.EmptyHole _))
  | UHExp.Tm(NotInHole(UHExp.Asc(_, _)))
  | UHExp.Tm(NotInHole(UHExp.Var(NotInVHole, _)))
  | UHExp.Tm(NotInHole(UHExp.NumLit _))
  | UHExp.Tm(NotInHole(UHExp.BoolLit _))
  | UHExp.Tm(NotInHole(UHExp.ApPalette(_, _, _))) -> 
    begin match UHExp.syn ctx e with
    | Some ty' ->
      if HTyp.consistent ty ty' then 
        Some (
          mk_cursor_info 
            (AnaSubsumed(ty, ty'))
            (IsExpr e)
            side
            ctx
        )
      else None
    | None -> None
    end
  | UHExp.Tm(NotInHole, (UHExp.Lam(_, ann, _))) -> 
    begin match HTyp.matched_arrow ty with 
    | None -> None 
    | Some (ty1_given, ty2) -> 
      begin match ann with 
      | Some uty1 -> 
        let ty1_ann = UHTyp.expand uty1 in 
        begin match HTyp.consistent ty1_ann ty1_given with 
        | false -> None
        | true -> 
          Some
            (mk_cursor_info
              (AnaAnnotatedLambda(
                ty,
                (HTyp.Arrow(ty1_ann, ty2))))
              (IsExpr e)
              side
              ctx)
        end
      | None -> 
        Some 
          (mk_cursor_info 
            (AnaOnly ty)
            (IsExpr e)
            side
            ctx)
      end
    end
  | UHExp.Tm(NotInHole, (UHExp.Inj(_, _))) -> 
    begin match ty with 
    | HTyp.Sum(_, _) -> 
      Some (
        mk_cursor_info
          (AnaOnly ty)
          (IsExpr e)
          side
          ctx
      )
    | _ -> None 
    end
  | UHExp.Tm(NotInHole, UHExp.OpSeq
      (Skel.BinOp(InHole(TypeInconsistent, _), _, _, _), surround)) -> None
  | UHExp.Tm(NotInHole, UHExp.OpSeq(Skel.Placeholder(_, _), surround)) -> None
  end

let rec syn_cursor_info
  (ctx : Contexts.t) 
  (ze : t) : cursor_info option =
  begin match ze with 
  | CursorE(side, UHExp.Tm(_, UHExp.Var(InVHole _, _)) as e) -> 
    Some (
      mk_cursor_info
        SynFree
        (IsExpr e)
        side
        ctx
    )
  | CursorE(side, e) -> 
    begin match UHExp.syn ctx e with 
    | Some ty -> 
      Some (
        mk_cursor_info
          (SynOnly ty)
          (IsExpr e)
          side
          ctx
      )
    | None -> None
    end
  | ParenthesizedZ ze1 -> 
    syn_cursor_info ctx ze1
  | Deeper(_, ze1') -> 
    syn_cursor_info' ctx ze1'
  end
and ana_cursor_info
  (ctx : Contexts.t)
  (ze : t) (ty : HTyp.t) : cursor_info option = 
  begin match ze with 
  | CursorE(side, e) ->
    ana_cursor_found ctx e ty side
  | ParenthesizedZ ze1 -> 
    ana_cursor_info ctx ze1 ty 
  | Deeper(InHole(TypeInconsistent, u), ze1') -> 
    syn_cursor_info' ctx ze1'
  | Deeper(InHole(WrongLength, _), 
      (ZExp.OpSeqZ(Skel.BinOp(_, UHExp.Comma, _, _), _, _) as ze1')) 
  | Deeper(NotInHole, ze1') -> 
    ana_cursor_info' ctx ze1' ty 
  | Deeper(InHole(WrongLength, _), _) -> None
  end
and syn_cursor_info'
  (ctx : Contexts.t) 
  (ze : t') : cursor_info option = 
  begin match ze with 
  | AscZ1(ze1, uty) -> 
    let ty = UHTyp.expand uty in 
    let e1 = erase ze1 in 
    if UHExp.bidelimited e1 then 
      ana_cursor_info ctx ze1 ty
    else None
  | AscZ2(e1, zty) -> 
    Some 
      (mk_cursor_info
        TypePosition
        IsType 
        Before (* TODO fix this once we use cursor info in type position! *)
        ctx)
  | LetZP(zp, ann, e1, e2) -> 
    begin match ann with 
    | Some uty1 -> 
      let ty1 = UHTyp.expand uty1 in 
      ana_pat_cursor_info ctx zp ty1
    | None ->  
      begin match UHExp.syn ctx e1 with 
      | None -> None
      | Some ty1 -> ana_pat_cursor_info ctx zp ty1 
      end
    end
  | LetZA(p, zann, e1, e2) -> 
    Some
      (mk_cursor_info
        TypePosition
        IsType
        Before (* TODO fix this once we use cursor info in type position! *)
        ctx)
  | LetZE1(p, ann, ze1, e2) -> 
    begin match ann with 
    | Some uty1 -> 
      let ty1 = UHTyp.expand uty1 in 
      let ctx1 = UHExp.ctx_for_let ctx p ty1 (erase ze1) in 
      ana_cursor_info ctx1 ze1 ty1
    | None -> syn_cursor_info ctx ze1
    end
  | LetZE2(p, ann, e1, ze2) -> 
    begin match ann with 
    | Some uty1 -> 
      let ty1 = UHTyp.expand uty1 in 
      begin match UHExp.ana_pat ctx p ty1 with 
      | None -> None
      | Some ctx2 -> 
        syn_cursor_info ctx2 ze2
      end
    | None -> 
      begin match UHExp.syn ctx e1 with 
      | None -> None
      | Some ty1 -> 
        begin match UHExp.ana_pat ctx p ty1 with 
        | None -> None
        | Some ctx2 -> 
          syn_cursor_info ctx2 ze2
        end
      end
    end
  | LamZP(zp, ann, _) -> 
    let ty1 = 
      begin match ann with 
      | Some uty1 -> UHTyp.expand uty1
      | None -> HTyp.Hole
      end in 
    ana_pat_cursor_info ctx zp ty1
  | LamZA(_, zann, _) -> 
    Some
      (mk_cursor_info
        TypePosition
        IsType
        Before (* TODO fix this once we use cursor info in type position *)
        ctx)
  | LamZE(p, ann, ze1) -> 
    let ty1 = 
      begin match ann with 
      | Some uty1 -> UHTyp.expand uty1
      | None -> HTyp.Hole
      end in 
    begin match UHExp.ana_pat ctx p ty1 with 
    | None -> None
    | Some ctx1 -> 
      syn_cursor_info ctx1 ze1
    end
  | InjZ(side, ze1) -> 
    syn_cursor_info ctx ze1
  (* | ListLitZ ((prefix, ze), _) -> 
    begin match prefix with 
    | nil -> syn_cursor_info ctx ze
    | cons _ _ -> 
      let opt_result = List.fold_left (fun opt_result e -> 
        begin match opt_result with 
        | None -> None
        | Some ty -> 
          begin match UHExp.syn ctx e with 
          | None -> None
          | Some ty' -> 
            begin match HTyp.join ty ty' with 
            | Some ty_joined -> Some ty_joined
            | None -> 
              begin match UHExp.ana ctx e ty with 
              | None -> None
              | Some _ -> Some ty
              end
            end
          end
        end) prefix (Some HTyp.Hole) in 
      begin match opt_result with 
      | None -> None
      | Some ty -> ana_cursor_info ctx ze ty
      end
    end *)
  | CaseZE(_, _)
  | CaseZR(_, _) -> None
  | OpSeqZ(skel, ze0, surround) -> 
    let e0 = erase ze0 in 
    let seq = OperatorSeq.opseq_of_exp_and_surround e0 surround in 
    let n = OperatorSeq.surround_prefix_length surround in 
    syn_skel_cursor_info ctx skel seq n ze0 
  | ApPaletteZ(_, _, zholedata) -> 
    let (_, zholemap) = zholedata in 
    let (_, tz) = zholemap in 
    let (_, tz') = tz in
    let (ty, ze) = tz' in 
    ana_cursor_info ctx ze ty 
  end
and ana_cursor_info'
  (ctx : Contexts.t) 
  (ze : t') (ty : HTyp.t) : cursor_info option = 
  begin match ze with 
  | LetZP(zp, ann, e1, e2) -> 
    begin match ann with 
    | Some uty1 -> 
      let ty1 = UHTyp.expand uty1 in 
      ana_pat_cursor_info ctx zp ty1 
    | None ->  
      begin match UHExp.syn ctx e1 with 
      | None -> None
      | Some ty1 -> 
        ana_pat_cursor_info ctx zp ty1 
      end
    end
  | LetZA(_, zann, e1, e2) -> 
    Some
      (mk_cursor_info
        TypePosition
        IsType
        Before (* TODO fix this once we use cursor info in type position! *)
        ctx)
  | LetZE1(p, ann, ze1, e2) -> 
    begin match ann with 
    | Some uty1 -> 
      let ty1 = UHTyp.expand uty1 in 
      let ctx1 = UHExp.ctx_for_let ctx p ty1 (erase ze1) in 
      ana_cursor_info ctx1 ze1 ty1
    | None -> syn_cursor_info ctx ze1
    end
  | LetZE2(p, ann, e1, ze2) -> 
    begin match ann with 
    | Some uty1 -> 
      let ty1 = UHTyp.expand uty1 in 
      begin match UHExp.ana_pat ctx p ty1 with 
      | None -> None
      | Some ctx2 -> 
        ana_cursor_info ctx2 ze2 ty
      end
    | None -> 
      begin match UHExp.syn ctx e1 with 
      | None -> None
      | Some ty1 -> 
        begin match UHExp.ana_pat ctx p ty1 with 
        | None -> None
        | Some ctx2 -> 
          ana_cursor_info ctx2 ze2 ty
        end
      end
    end
  | LamZP(p, ann, e) -> 
    begin match HTyp.matched_arrow ty with 
    | None -> None
    | Some (ty1_given, ty2) -> 
      let ty1 = 
        begin match ann with 
        | Some uty1 -> UHTyp.expand uty1
        | None -> ty1_given
        end in 
      ana_pat_cursor_info ctx p ty1
    end
  | LamZA(_, zann, _) -> 
    Some
      (mk_cursor_info
        TypePosition
        IsType
        Before (* TODO fix this once we use cursor info in type position *)
        ctx)
  | LamZE(p, ann, ze1) -> 
    begin match HTyp.matched_arrow ty with 
    | None -> None
    | Some (ty1_given, ty2) -> 
      let ty1 = 
        begin match ann with 
        | Some uty1 -> UHTyp.expand uty1
        | None -> ty1_given
        end in 
      begin match UHExp.ana_pat ctx p ty1 with 
      | None -> None
      | Some ctx -> 
        ana_cursor_info ctx ze1 ty2
      end
    end
  | InjZ(side, ze1) -> 
    begin match HTyp.matched_sum ty with 
    | None -> None
    | Some (ty1, ty2) -> 
      ana_cursor_info ctx ze1 
        (pick_side side ty1 ty2)
    end
  (* | ListLitZ zes -> 
    begin match HTyp.matched_list ty with 
    | None -> None
    | Some ty_elt -> 
      let ze0 = ZList.prj_z zes in  
      ana_cursor_info ctx ze0 ty_elt
    end *)
  | CaseZE(ze1, rules) -> 
    syn_cursor_info ctx ze1
  | CaseZR(e1, zrules) -> 
    begin match UHExp.syn ctx e1 with 
    | None -> None
    | Some ty1 -> 
      let zrule = ZList.prj_z zrules in 
      ana_rule_cursor_info ctx zrule ty1 ty
    end
  | OpSeqZ(skel, ze0, surround) -> 
    let e0 = erase ze0 in 
    let seq = OperatorSeq.opseq_of_exp_and_surround e0 surround in 
    let n = OperatorSeq.surround_prefix_length surround in 
    ana_skel_cursor_info ctx skel seq n ze0 ty 
  | AscZ1(_, _) 
  | AscZ2(_, _) 
  | ApPaletteZ(_, _, _) -> 
    syn_cursor_info' ctx ze 
  end
and ana_rule_cursor_info
  (ctx : Contexts.t)
  (zrule : ZExp.zrule)
  (pat_ty : HTyp.t)
  (clause_ty : HTyp.t)
  : cursor_info option = 
  begin match zrule with 
  | RuleZP(zp, e) -> 
    ana_pat_cursor_info ctx zp pat_ty
  | RuleZE(p, ze) -> 
    begin match UHExp.ana_pat ctx p pat_ty with  
    | None -> None
    | Some ctx -> 
      ana_cursor_info ctx ze clause_ty
    end
  end
and syn_skel_cursor_info
  (ctx : Contexts.t) 
  (skel : UHExp.skel_t) (seq : UHExp.opseq) 
  (n : nat) (ze_n : ZExp.t) : cursor_info option = 
  begin match skel with 
  | Skel.Placeholder(_, n') -> 
    if Nat.eqb n n' then 
      syn_cursor_info ctx ze_n
    else None
  | Skel.BinOp(_, UHExp.Plus, skel1, skel2) 
  | Skel.BinOp(_, UHExp.Times, skel1, skel2) 
  | Skel.BinOp(_, UHExp.LessThan, skel1, skel2) -> 
    begin match ana_skel_cursor_info ctx skel1 seq n ze_n HTyp.Num with 
    | (Some _) as result -> result
    | None ->
      begin match ana_skel_cursor_info ctx skel2 seq n ze_n HTyp.Num with 
      | (Some _) as result -> result
      | None -> None
      end
    end
  | Skel.BinOp(_, UHExp.Space, (Skel.Placeholder(_, n')) as skel1, skel2) -> 
    if Nat.eqb n n' then 
      begin match cursor_on_outer_expr ze_n with 
      | Some ((UHExp.Tm(InHole(TypeInconsistent, u), e_n')) as e_n, side) -> 
        begin match UHExp.syn' ctx e_n' with 
        | Some ty -> Some 
            (mk_cursor_info
              (SynErrorArrow((HTyp.Arrow(HTyp.Hole, HTyp.Hole)), ty))
              (IsExpr e_n)
              side
              ctx)
        | None -> None
        end
      | Some (((UHExp.Tm(_, (UHExp.Var(InVHole _, _)))) as e_n), side) -> 
        Some 
          (mk_cursor_info
            (SynFreeArrow (HTyp.Arrow(HTyp.Hole, HTyp.Hole)))
            (IsExpr e_n)
            side
            ctx)
      | Some (e_n, side) -> 
        begin match UHExp.syn ctx e_n with 
        | Some ty -> 
          begin match HTyp.matched_arrow ty with 
          | Some (ty1, ty2) -> 
            Some 
              (mk_cursor_info 
                (SynMatchingArrow(ty, (HTyp.Arrow(ty1, ty2))))
                (IsExpr e_n)
                side
                ctx)
          | None -> None
          end
        | None -> None
        end
      | None -> 
        syn_cursor_info ctx ze_n
      end
    else
      begin match UHExp.syn_skel ctx skel1 seq None with 
      | None -> None
      | Some (ty, _) -> 
        begin match HTyp.matched_arrow ty with 
        | Some (ty1, ty2) -> 
          ana_skel_cursor_info ctx skel2 seq n ze_n ty1
        | None -> None
        end
      end
  | Skel.BinOp(_, UHExp.Space, skel1, skel2) -> 
    begin match syn_skel_cursor_info ctx skel1 seq n ze_n with 
    | (Some _) as result -> result
    | None -> 
      begin match UHExp.syn_skel ctx skel1 seq None with 
      | None -> None
      | Some (ty, _) -> 
        begin match HTyp.matched_arrow ty with 
        | None -> None
        | Some (ty1, ty2) -> 
          ana_skel_cursor_info ctx skel2 seq n ze_n ty2
        end
      end
    end
  | Skel.BinOp(_, UHExp.Comma, skel1, skel2) -> 
    begin match syn_skel_cursor_info ctx skel1 seq n ze_n with 
    | (Some _) as result -> result
    | None -> syn_skel_cursor_info ctx skel2 seq n ze_n
    end
  | Skel.BinOp(_, UHExp.Cons, skel1, skel2) -> 
    begin match syn_skel_cursor_info ctx skel1 seq n ze_n with 
    | (Some _) as result -> result
    | None -> 
      begin match UHExp.syn_skel ctx skel1 seq None with 
      | None -> None
      | Some (ty_elt, _) -> 
        let ty_list = HTyp.List(ty_elt) in  
        ana_skel_cursor_info ctx skel2 seq n ze_n ty_list
      end
    end
  end
and ana_skel_cursor_info
  (ctx : Contexts.t) 
  (skel : UHExp.skel_t) (seq : UHExp.opseq)
  (n : nat) (ze_n : t) (ty : HTyp.t) : cursor_info option = 
  begin match skel with 
  | Skel.Placeholder(_, n') -> 
    if Nat.eqb n n' then 
      ana_cursor_info ctx ze_n ty 
    else None
  | Skel.BinOp(InHole(TypeInconsistent, _), _, _, _) -> 
    syn_skel_cursor_info ctx skel seq n ze_n 
  | Skel.BinOp(NotInHole, UHExp.Comma, skel1, skel2) -> 
    begin match ty with 
    | HTyp.Hole -> 
      begin match ana_skel_cursor_info ctx skel1 seq n ze_n HTyp.Hole with 
      | (Some _) as result -> result
      | None -> ana_skel_cursor_info ctx skel2 seq n ze_n HTyp.Hole 
      end
    | HTyp.Prod(ty1, ty2) -> 
      let types = HTyp.get_tuple ty1 ty2 in 
      let skels = UHExp.get_tuple skel1 skel2 in 
      begin match Util.zip_eq skels types with 
      | None -> None
      | Some zipped -> 
        List.fold_left (fun opt_result (skel_ty : UHExp.skel_t * HTyp.t) -> 
          begin match opt_result with 
          | (Some _) as result -> result
          | None -> 
            let (skel, ty) = skel_ty in 
            ana_skel_cursor_info ctx skel seq n ze_n ty
          end) zipped None
      end
    | _ -> None
    end
  | Skel.BinOp (InHole(WrongLength, _), UHExp.Comma, skel1, skel2) -> 
    begin match ty with 
    | HTyp.Prod(ty1, ty2) ->  
      let types = HTyp.get_tuple ty1 ty2 in 
      let skels = UHExp.get_tuple skel1 skel2 in 
      let (zipped, remainder) = HTyp.zip_with_skels skels types in 
      let ana_zipped = 
        List.fold_left (fun opt_result (skel_ty : UHExp.skel_t * HTyp.t) -> 
          begin match opt_result with 
          | (Some _) as result -> result
          | None -> 
            let (skel, ty) = skel_ty in 
            ana_skel_cursor_info ctx skel seq n ze_n ty
          end) zipped None in 
      begin match ana_zipped with 
      | (Some _) as result -> result
      | None -> 
        List.fold_left (fun opt_result skel -> 
          begin match opt_result with 
          | (Some _) as result -> result
          | None -> syn_skel_cursor_info ctx skel seq n ze_n
          end) remainder None
      end
    | _ -> None
    end
  | Skel.BinOp(InHole(WrongLength, _), _, _, _) -> None
  | Skel.BinOp(NotInHole, UHExp.Cons, skel1, skel2) -> 
    begin match HTyp.matched_list ty with 
    | None -> None
    | Some ty_elt -> 
      begin match ana_skel_cursor_info ctx skel1 seq n ze_n ty_elt with 
      | (Some _) as result -> result
      | None -> 
        let ty_list = HTyp.List(ty_elt) in  
        ana_skel_cursor_info ctx skel2 seq n ze_n ty_list
      end
    end
  | Skel.BinOp(_, UHExp.Plus, _, _)
  | Skel.BinOp(_, UHExp.Times, _, _)
  | Skel.BinOp(_, UHExp.LessThan, _, _)
  | Skel.BinOp(_, UHExp.Space, _, _) ->  
    syn_skel_cursor_info ctx skel seq n ze_n 
  end

