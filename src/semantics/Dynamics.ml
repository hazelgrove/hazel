open SemanticsCommon

type nat = int

type hole_sort =  
| ExpressionHole
| PatternHole

module Delta = struct
  type t = (hole_sort * HTyp.t * VarCtx.t) MetaVarMap.t
  let empty : t = MetaVarMap.empty
end

(* hole instance numbers are all 0 after expansion and during evaluation --
 * renumbering is done on the final result (see below) *)
type inst_num = nat

module DHPat = struct
  type t =
  | EmptyHole of MetaVar.t * inst_num 
  | NonEmptyHole of in_hole_reason * MetaVar.t * inst_num * t
  | Wild 
  | Var of Var.t
  | NumLit of nat
  | BoolLit of bool
  | Inj of inj_side * t
  | ListNil
  | Cons of t * t
  | Pair of t * t
  | Triv (* unit intro *)
  | Ap of t * t

  let rec make_tuple (ds : t list) : t =  
    begin match ds with 
    | [d1; d2] -> Pair(d1, d2)
    | [d1] -> d1
    | d1 :: ds -> 
      let d2 = make_tuple ds in 
      Pair(d1, d2)
    | [] -> Triv
    end

  (* whether dp contains the variable x outside of a hole *)
  let rec binds_var (x : Var.t) (dp : t) : bool =
    begin match dp with
    | EmptyHole(_, _)
    | NonEmptyHole(_, _, _, _)
    | Wild
    | NumLit(_)
    | BoolLit(_)
    | Triv
    | ListNil -> false
    | Var y -> Var.eq x y
    | Inj(_, dp1) -> binds_var x dp1
    | Pair(dp1, dp2) -> binds_var x dp1 || binds_var x dp2
    | Cons(dp1, dp2) -> binds_var x dp1 || binds_var x dp2
    | Ap(dp1, dp2) -> false
    end

  type expand_result =
  | Expands of t * HTyp.t * Contexts.t * Delta.t
  | DoesNotExpand

  let rec syn_expand
    (ctx : Contexts.t)
    (delta : Delta.t)
    (p : UHPat.t)
    : expand_result =
    begin match p with
    | UHPat.Pat(InHole(TypeInconsistent as reason, u), p')
    | UHPat.Pat(InHole(WrongLength as reason, u),  
        (UHPat.OpSeq(Skel.BinOp(InHole(WrongLength, _), UHPat.Comma, _, _), _) as p')) ->
      begin match syn_expand' ctx delta p' with
      | DoesNotExpand -> DoesNotExpand
      | Expands(dp, _, ctx, delta) ->
        let gamma = Contexts.gamma ctx in 
        let delta = MetaVarMap.extend delta (u, (PatternHole, HTyp.Hole, gamma)) in
        Expands(
          NonEmptyHole(reason, u, 0, dp),
          HTyp.Hole,
          ctx,
          delta)
      end
    | UHPat.Pat(InHole(WrongLength, _), _) -> DoesNotExpand
    | UHPat.Pat(NotInHole, p') -> syn_expand' ctx delta p'
    | UHPat.Parenthesized p1 -> syn_expand ctx delta p1
    end
  and syn_expand'
    (ctx : Contexts.t)
    (delta : Delta.t)
    (p' : UHPat.t')
    : expand_result =
    begin match p' with
    | UHPat.EmptyHole u ->
      let gamma = Contexts.gamma ctx in
      let dp = EmptyHole(u, 0) in
      let ty = HTyp.Hole in
      let delta = MetaVarMap.extend delta (u, (PatternHole, ty, gamma)) in
      Expands(dp, ty, ctx, delta)
    | UHPat.Wild -> Expands(Wild, HTyp.Hole, ctx, delta)
    | UHPat.Var x ->
      let ctx = Contexts.extend_gamma ctx (x, HTyp.Hole) in
      Expands(Var x, HTyp.Hole, ctx, delta)
    | UHPat.NumLit n -> Expands(NumLit n, HTyp.Num, ctx, delta)
    | UHPat.BoolLit b -> Expands(BoolLit b, HTyp.Bool, ctx, delta)
    | UHPat.Inj(side, p) ->
      begin match syn_expand ctx delta p with
      | DoesNotExpand -> DoesNotExpand
      | Expands(dp1, ty1, ctx, delta) ->
        let dp = Inj(side, dp1) in
        let ty =
          begin match side with
          | L -> HTyp.Sum(ty1, HTyp.Hole)
          | R -> HTyp.Sum(HTyp.Hole, ty1)
          end in
        Expands(dp, ty, ctx, delta)
      end
    | UHPat.ListNil -> Expands(ListNil, HTyp.List HTyp.Hole, ctx, delta)
    | UHPat.OpSeq(skel, seq) -> syn_expand_skel ctx delta skel seq
    end
  and syn_expand_skel
    (ctx : Contexts.t)
    (delta : Delta.t)
    (skel : UHPat.skel_t)
    (seq : UHPat.opseq)
    : expand_result =
      begin match skel with
      | Skel.Placeholder n ->
        begin match OperatorSeq.seq_nth n seq with
        | None -> DoesNotExpand
        | Some pn ->
          syn_expand ctx delta pn
        end
      | Skel.BinOp(InHole(TypeInconsistent as reason, u), op, skel1, skel2) 
      | Skel.BinOp(InHole(WrongLength as reason, u), (UHPat.Comma as op), skel1, skel2) -> 
        let skel_not_in_hole = Skel.BinOp(NotInHole, op, skel1, skel2) in
        begin match syn_expand_skel ctx delta skel_not_in_hole seq with
        | DoesNotExpand -> DoesNotExpand
        | Expands(dp, _, ctx, delta) ->
          let gamma = Contexts.gamma ctx in
          let delta = MetaVarMap.extend delta (u, (PatternHole, HTyp.Hole, gamma)) in
          Expands(
            NonEmptyHole(reason, u, 0, dp),
            HTyp.Hole,
            ctx,
            delta)
        end
      | Skel.BinOp(InHole(WrongLength, _), _, _, _) -> DoesNotExpand
      | Skel.BinOp(NotInHole, UHPat.Comma, skel1, skel2) ->
        begin match syn_expand_skel ctx delta skel1 seq with
        | DoesNotExpand -> DoesNotExpand
        | Expands(dp1, ty1, ctx, delta) ->
          begin match syn_expand_skel ctx delta skel2 seq with
          | DoesNotExpand -> DoesNotExpand
          | Expands(dp2, ty2, ctx, delta) ->
            let dp = Pair(dp1, dp2) in
            Expands(dp, HTyp.Prod(ty1, ty2), ctx, delta)
          end
        end
      | Skel.BinOp(NotInHole, UHPat.Space, skel1, skel2) ->
        begin match syn_expand_skel ctx delta skel1 seq with
        | DoesNotExpand -> DoesNotExpand
        | Expands(dp1, ty1, ctx, delta) ->
          begin match syn_expand_skel ctx delta skel2 seq with
          | DoesNotExpand -> DoesNotExpand
          | Expands(dp2, ty2, ctx, delta) ->
            let dp = Ap(dp1, dp2) in
            Expands(dp, HTyp.Hole, ctx, delta)
          end
        end
      | Skel.BinOp(NotInHole, UHPat.Cons, skel1, skel2) ->
        begin match syn_expand_skel ctx delta skel1 seq with
        | DoesNotExpand -> DoesNotExpand
        | Expands(dp1, ty1, ctx, delta) ->
          let ty = HTyp.List ty1 in
          begin match ana_expand_skel ctx delta skel2 seq ty with
          | DoesNotExpand -> DoesNotExpand
          | Expands(dp2, ty', ctx, delta) ->
            begin match HTyp.join ty ty' with 
            | None -> DoesNotExpand
            | Some ty -> 
              let dp = Cons(dp1, dp2) in
              Expands(dp, ty, ctx, delta)
            end
          end
        end
      end
  and ana_expand
    (ctx : Contexts.t)
    (delta : Delta.t)
    (p : UHPat.t)
    (ty : HTyp.t)
    : expand_result =
      begin match p with
      | UHPat.Pat(NotInHole, p') -> 
        ana_expand' ctx delta p' ty
      | UHPat.Pat(InHole(TypeInconsistent as reason, u), p') ->
        begin match syn_expand' ctx delta p' with
        | DoesNotExpand -> DoesNotExpand
        | Expands(dp1, _, ctx, delta) ->
          let dp = NonEmptyHole(reason, u, 0, dp1) in
          let gamma = Contexts.gamma ctx in 
          let delta = MetaVarMap.extend delta (u, (PatternHole, ty, gamma)) in
          Expands(dp, ty, ctx, delta)
        end
      | UHPat.Pat(InHole(WrongLength as reason, u),  
          ((UHPat.OpSeq(Skel.BinOp(InHole(WrongLength, _), UHPat.Comma, _, _), _) as p'))) -> 
        begin match ana_expand' ctx delta p' ty with 
        | DoesNotExpand -> DoesNotExpand
        | Expands(dp1, _, ctx, delta) -> 
          let dp = NonEmptyHole(reason, u, 0, dp1) in 
          let gamma = Contexts.gamma ctx in 
          let delta = MetaVarMap.extend delta (u, (PatternHole, ty, gamma)) in 
          Expands(dp, ty, ctx, delta)
        end
      | UHPat.Pat(InHole(WrongLength, _), _) -> DoesNotExpand
      | UHPat.Parenthesized p -> ana_expand ctx delta p ty
      end
  and ana_expand'
    (ctx : Contexts.t)
    (delta : Delta.t)
    (p' : UHPat.t')
    (ty : HTyp.t)
    : expand_result =
      begin match p' with
      | UHPat.EmptyHole u ->
        let gamma = Contexts.gamma ctx in
        let dp = EmptyHole(u, 0) in
        let delta = MetaVarMap.extend delta (u, (PatternHole, ty, gamma)) in
        Expands(dp, ty, ctx, delta)
      | UHPat.Var x ->
        let ctx = Contexts.extend_gamma ctx (x, ty) in
        Expands(Var x, ty, ctx, delta)
      | UHPat.Wild -> 
        Expands(Wild, ty, ctx, delta)
      | UHPat.NumLit _
      | UHPat.BoolLit _ -> syn_expand' ctx delta p'
      | UHPat.Inj(side, p1) ->
        begin match HTyp.matched_sum ty with
        | None -> DoesNotExpand
        | Some (tyL, tyR) ->
          let ty1 = pick_side side tyL tyR in
          begin match ana_expand ctx delta p1 ty1 with 
          | DoesNotExpand -> DoesNotExpand
          | Expands(dp1, ty1, ctx, delta) -> 
            let ty = 
              begin match side with 
              | L -> HTyp.Sum(ty1, tyR)
              | R -> HTyp.Sum(tyL, ty1)
              end in 
            Expands(Inj(side, dp1), ty, ctx, delta)
          end
        end
      | UHPat.ListNil ->
        begin match HTyp.matched_list ty with
        | None -> DoesNotExpand
        | Some ty_elt -> Expands(ListNil, HTyp.List ty_elt, ctx, delta)
        end
      | UHPat.OpSeq(skel, seq) -> ana_expand_skel ctx delta skel seq ty
      end
  and ana_expand_skel
    (ctx : Contexts.t)
    (delta : Delta.t)
    (skel : UHPat.skel_t)
    (seq : UHPat.opseq)
    (ty : HTyp.t)
    : expand_result =
      begin match skel with
      | Skel.Placeholder n ->
        begin match OperatorSeq.seq_nth n seq with
        | None -> DoesNotExpand
        | Some pn -> ana_expand ctx delta pn ty
        end
      | Skel.BinOp(InHole(TypeInconsistent as reason, u), op, skel1, skel2) ->
        let skel_not_in_hole = Skel.BinOp(NotInHole, op, skel1, skel2) in
        begin match syn_expand_skel ctx delta skel_not_in_hole seq with
        | DoesNotExpand -> DoesNotExpand
        | Expands(dp1, _, ctx, delta) ->
          let dp = NonEmptyHole(reason, u, 0, dp1) in
          let gamma = Contexts.gamma ctx in
          let delta = MetaVarMap.extend delta (u, (PatternHole, ty, gamma)) in
          Expands(dp, ty, ctx, delta)
        end
      | Skel.BinOp(NotInHole, UHPat.Comma, skel1, skel2) ->
        begin match HTyp.matched_prod ty with
        | None -> DoesNotExpand
        | Some (ty1, ty2) ->
          begin match ana_expand_skel ctx delta skel1 seq ty1 with
          | DoesNotExpand -> DoesNotExpand
          | Expands(dp1, ty1, ctx, delta) ->
            begin match ana_expand_skel ctx delta skel2 seq ty2 with
            | DoesNotExpand -> DoesNotExpand
            | Expands(dp2, ty2, ctx, delta) ->
              let dp = Pair(dp1, dp2) in
              Expands(dp, HTyp.Prod(ty1, ty2), ctx, delta)
            end
          end
        end
      | Skel.BinOp(InHole(WrongLength, u), UHPat.Comma, skel1, skel2) -> 
        begin match ty with 
        | HTyp.Prod(ty1, ty2) -> 
          let types = HTyp.get_tuple ty1 ty2 in 
          let skels = UHPat.get_tuple skel1 skel2 in 
          let (zipped, remainder) = HTyp.zip_with_skels skels types in 
          let processed1 = 
            List.fold_right (fun (skel_ty : UHPat.skel_t * HTyp.t) opt_result -> 
              begin match opt_result with 
              | None -> None
              | Some (elts, ctx, delta) -> 
                let (skel, ty) = skel_ty in 
                begin match ana_expand_skel ctx delta skel seq ty with 
                | DoesNotExpand -> None
                | Expands(dp, ty, ctx, delta) -> 
                  Some ((dp, ty)::elts, ctx, delta)
                end
              end) zipped (Some ([], ctx, delta)) in
          begin match processed1 with 
          | None -> DoesNotExpand
          | Some (elts1, ctx, delta) -> 
            let processed2 = 
              List.fold_right (fun (skel : UHPat.skel_t) opt_result -> 
                begin match opt_result with 
                | None -> None
                | Some (elts, ctx, delta) -> 
                  begin match syn_expand_skel ctx delta skel seq with 
                  | DoesNotExpand -> None
                  | Expands(dp, ty, ctx, delta) -> 
                    Some ((dp, ty)::elts, ctx, delta)
                  end
                end) remainder (Some ([], ctx, delta)) in
            begin match processed2 with 
            | None -> DoesNotExpand
            | Some (elts2, ctx, delta) -> 
              let (ds, tys) = Util.unzip (elts1 @ elts2) in 
              let d = make_tuple ds in 
              let ty = HTyp.make_tuple tys in 
              Expands(d, ty, ctx, delta)
            end
          end
        | _ -> DoesNotExpand
        end
      | Skel.BinOp(InHole(WrongLength, _), _, _, _) -> DoesNotExpand
      | Skel.BinOp(NotInHole, UHPat.Space, skel1, skel2) -> DoesNotExpand
      | Skel.BinOp(NotInHole, UHPat.Cons, skel1, skel2) ->
        begin match HTyp.matched_list ty with
        | None -> DoesNotExpand
        | Some ty_elt ->
          begin match ana_expand_skel ctx delta skel1 seq ty_elt with
          | DoesNotExpand -> DoesNotExpand
          | Expands(dp1, ty_elt', ctx, delta) ->
            let ty_list = HTyp.List ty_elt in
            begin match ana_expand_skel ctx delta skel2 seq ty_list with
            | DoesNotExpand -> DoesNotExpand
            | Expands(dp2, HTyp.List ty_elt'', ctx, delta) ->
              begin match HTyp.join ty_elt' ty_elt'' with 
              | None -> DoesNotExpand
              | Some ty_elt -> 
                let ty = HTyp.List ty_elt in 
                let dp = Cons(dp1, dp2) in
                Expands(dp, ty, ctx, delta)
              end
            | Expands(_, _, _, _) -> DoesNotExpand 
            end
          end
        end
      end
end

module DHExp = struct 
  type bin_num_op =  
  | Plus
  | Times
  | LessThan

  let of_op op = 
    begin match op with 
    | UHExp.Plus -> Some (Plus, HTyp.Num)
    | UHExp.Times -> Some (Times, HTyp.Num)
    | UHExp.LessThan -> Some (LessThan, HTyp.Bool)
    | UHExp.Space 
    | UHExp.Cons 
    | UHExp.Comma -> None
    end
  
  let to_op bno = 
    begin match bno with 
    | Plus -> UHExp.Plus
    | Times -> UHExp.Times
    | LessThan -> UHExp.LessThan
    end

  module DHExp = struct
    type t =  
    | EmptyHole of MetaVar.t * inst_num * t VarMap.t_ 
    | NonEmptyHole of in_hole_reason * MetaVar.t * inst_num * t VarMap.t_ * t
    | FreeVar of MetaVar.t * inst_num * t VarMap.t_ * Var.t
    | BoundVar of Var.t
    | Let of DHPat.t * t * t 
    | FixF of Var.t * HTyp.t * t
    | Lam of DHPat.t * HTyp.t * t
    | Ap of t * t 
    | BoolLit of bool
    | NumLit of nat
    | BinNumOp of bin_num_op * t * t
    | ListNil
    | Cons of t * t
    | Inj of HTyp.t * inj_side * t
    | Pair of t * t
    | Triv
    | Case of t * rule list * nat
    | Cast of t * HTyp.t * HTyp.t
    | FailedCast of t * HTyp.t * HTyp.t
    and rule =
    | Rule of DHPat.t * t
  end
  include DHExp

  let rec make_tuple (ds : t list) : t = 
    begin match ds with 
    | [d1; d2] -> Pair(d1, d2)
    | [d1] -> d1
    | d1 :: ds -> 
      let d2 = make_tuple ds in 
      Pair(d1, d2)
    | [] -> Triv
    end

  let rec cast (d : t) (t1 : HTyp.t) (t2 : HTyp.t) : t = 
    if HTyp.eq t1 t2 then d else Cast(d, t1, t2)

  let apply_casts (d : t) (casts : (HTyp.t * HTyp.t) list) : t = 
    List.fold_left (fun d (c : HTyp.t * HTyp.t) -> 
      let (ty1, ty2) = c in 
      cast d ty1 ty2) d casts

  module Environment = struct 
    type t = DHExp.t VarMap.t_
    include VarMap
  end

  (* closed substitution [d1/x]d2*)
  let rec subst_var (d1 : t) (x : Var.t) (d2 : t) : t =
    begin match d2 with 
    | BoundVar y -> if Var.eq x y then d1 else d2
    | FreeVar(_, _, _, _) -> d2
    | Let(dp, d3, d4) ->
      let d3 = subst_var d1 x d3 in
      let d4 = if DHPat.binds_var x dp then d4 else subst_var d1 x d4 in
      Let(dp, d3, d4)
    | FixF(y, ty, d3) -> 
      let d3 = if Var.eq x y then d3 else subst_var d1 x d3 in
      FixF(y, ty, d3)
    | Lam(dp, ty, d3) ->
      if DHPat.binds_var x dp then d2 else
      let d3 = subst_var d1 x d3 in
      Lam(dp, ty, d3)
    | Ap(d3, d4) -> 
      let d3 = subst_var d1 x d3 in
      let d4 = subst_var d1 x d4 in
      Ap(d3, d4)
    | BoolLit _
    | NumLit _
    | ListNil
    | Triv -> d2
    | Cons(d3, d4) ->
      let d3 = subst_var d1 x d3 in
      let d4 = subst_var d1 x d4 in
      Cons(d3, d4)
    | BinNumOp(op, d3, d4) -> 
      let d3 = subst_var d1 x d3 in
      let d4 = subst_var d1 x d4 in
      BinNumOp(op, d3, d4)
    | Inj(ty, side, d3) -> 
      let d3 = subst_var d1 x d3 in
      Inj(ty, side, d3)
    | Pair(d3, d4) ->
      let d3 = subst_var d1 x d3 in
      let d4 = subst_var d1 x d4 in
      Pair(d3, d4)
    | Case(d3, rules, n) ->
      let d3 = subst_var d1 x d3 in
      let rules = subst_var_rules d1 x rules in
      Case(d3, rules, n)
    | EmptyHole(u, i, sigma) -> 
      let sigma' = subst_var_env d1 x sigma in
      EmptyHole(u, i, sigma')
    | NonEmptyHole(reason, u, i, sigma, d3) -> 
      let d3' = subst_var d1 x d3 in
      let sigma' = subst_var_env d1 x sigma in
      NonEmptyHole(reason, u, i, sigma', d3')
    | Cast(d, ty1, ty2) -> 
      let d' = subst_var d1 x d in
      Cast(d', ty1, ty2) 
    | FailedCast(d, ty1, ty2) -> 
      let d' = subst_var d1 x d in
      FailedCast(d', ty1, ty2)
    end
  and subst_var_rules (d1 : t) (x : Var.t) (rules : rule list) =
    List.map (fun (r : rule) ->
      begin match r with
      | Rule(dp, d2) -> if DHPat.binds_var x dp then r else Rule(dp, subst_var d1 x d2)
      end) rules
  and subst_var_env (d1 : t) (x : Var.t) (sigma : Environment.t) =
    List.map 
      (fun xd -> 
        let (y, d) = xd in
        (y, subst_var d1 x d))
      sigma

  let rec subst
    (env : Environment.t)
    (d : t)
    : t =
    List.fold_left (fun d2 (xd : Var.t * t) ->
      let (x, d1) = xd in
      subst_var d1 x d2) d env

  type match_result =
  | Matches of Environment.t
  | DoesNotMatch
  | Indet

  let rec matches
    (dp : DHPat.t)
    (d : t)
    : match_result =
    begin match (dp, d) with
    | (_, BoundVar _) -> DoesNotMatch
    | (DHPat.EmptyHole(_, _), _)
    | (DHPat.NonEmptyHole(_, _, _, _), _) -> Indet
    | (DHPat.Wild, _) -> Matches Environment.empty
    | (DHPat.Var x, _) ->
      let env = Environment.extend Environment.empty (x, d) in
      Matches env
    | (_, EmptyHole(_, _, _)) -> Indet
    | (_, NonEmptyHole(_, _, _, _, _)) -> Indet
    | (_, FailedCast(_, _, _)) -> Indet
    | (_, FreeVar(_, _, _, _)) -> Indet
    | (_, Let(_, _, _)) -> Indet
    | (_, FixF(_, _, _)) -> DoesNotMatch
    | (_, Lam(_, _, _)) -> DoesNotMatch
    | (_, Ap(_, _)) -> Indet
    | (_, BinNumOp(_, _, _)) -> Indet
    | (_, Case(_, _, _)) -> Indet
    | (DHPat.BoolLit b1, BoolLit b2) ->
      if b1 = b2 then Matches Environment.empty else DoesNotMatch
    | (DHPat.BoolLit _, Cast(d, HTyp.Bool, HTyp.Hole)) -> matches dp d 
    | (DHPat.BoolLit _, Cast(d, HTyp.Hole, HTyp.Bool)) -> matches dp d
    | (DHPat.BoolLit _, _) -> DoesNotMatch
    | (DHPat.NumLit n1, NumLit n2) ->
      if n1 = n2 then Matches Environment.empty else DoesNotMatch
    | (DHPat.NumLit _, Cast(d, HTyp.Num, HTyp.Hole)) -> matches dp d 
    | (DHPat.NumLit _, Cast(d, HTyp.Hole, HTyp.Num)) -> matches dp d
    | (DHPat.NumLit _, _) -> DoesNotMatch
    | (DHPat.Inj(side1, dp), Inj(_, side2, d)) ->
      begin match (side1, side2) with
      | (L, L) | (R, R) -> matches dp d
      | _ -> DoesNotMatch
      end
    | (DHPat.Inj(side, dp), Cast(d, HTyp.Sum(tyL1, tyR1), HTyp.Sum(tyL2, tyR2))) -> 
      matches_cast_Inj side dp d [(tyL1, tyR1, tyL2, tyR2)]
    | (DHPat.Inj(_, _), Cast(d, HTyp.Sum(_, _), HTyp.Hole)) -> matches dp d
    | (DHPat.Inj(_, _), Cast(d, HTyp.Hole, HTyp.Sum(_, _))) -> matches dp d
    | (DHPat.Inj(_, _), _) -> DoesNotMatch
    | (DHPat.Pair(dp1, dp2), Pair(d1, d2)) ->
      begin match matches dp1 d1 with 
      | DoesNotMatch -> DoesNotMatch
      | Indet -> Indet
      | Matches env1 -> 
        begin match matches dp2 d2 with 
        | DoesNotMatch -> DoesNotMatch
        | Indet -> Indet
        | Matches env2 -> 
          Matches (Environment.union env1 env2)
        end
      end
    | (DHPat.Pair(dp1, dp2), Cast(d, HTyp.Prod(tyL1, tyR1), HTyp.Prod(tyL2, tyR2))) -> 
      matches_cast_Pair dp1 dp2 d [(tyL1, tyL2)] [(tyR1, tyR2)]
    | (DHPat.Pair(dp1, dp2), Cast(d, HTyp.Hole, HTyp.Prod(_, _))) -> matches dp d
    | (DHPat.Pair(dp1, dp2), Cast(d, HTyp.Prod(_, _), HTyp.Hole)) -> matches dp d
    | (DHPat.Pair(_, _), _) -> DoesNotMatch
    | (DHPat.Triv, Triv) -> Matches Environment.empty
    | (DHPat.Triv, Cast(d, HTyp.Hole, HTyp.Unit)) -> matches dp d
    | (DHPat.Triv, Cast(d, HTyp.Unit, HTyp.Hole)) -> matches dp d
    | (DHPat.Triv, _) -> DoesNotMatch
    | (DHPat.ListNil, ListNil) -> Matches Environment.empty
    | (DHPat.ListNil, Cast(d, HTyp.Hole, HTyp.List _)) -> matches dp d
    | (DHPat.ListNil, Cast(d, HTyp.List _, HTyp.Hole)) -> matches dp d
    | (DHPat.ListNil, _) -> DoesNotMatch
    | (DHPat.Cons(dp1, dp2), Cons(d1, d2)) ->
      begin match matches dp1 d1 with
      | DoesNotMatch -> DoesNotMatch
      | Indet -> Indet
      | Matches env1 ->
        begin match matches dp2 d2 with
        | DoesNotMatch -> DoesNotMatch
        | Indet -> Indet
        | Matches env2 ->
          Matches (Environment.union env1 env2)
        end
      end
    | (DHPat.Cons(dp1, dp2), Cast(d, HTyp.List ty1, HTyp.List ty2)) -> 
      matches_cast_Cons dp1 dp2 d [(ty1, ty2)]
    | (DHPat.Cons(_, _), Cast(d, HTyp.Hole, HTyp.List _)) -> matches dp d
    | (DHPat.Cons(_, _), Cast(d, (HTyp.List _), HTyp.Hole)) -> matches dp d 
    | (DHPat.Cons(_, _), _) -> DoesNotMatch
    | (DHPat.Ap(_, _), _) -> DoesNotMatch
    end
  and matches_cast_Inj
    (side : inj_side) 
    (dp : DHPat.t)
    (d : t)
    (casts : (HTyp.t * HTyp.t * HTyp.t * HTyp.t) list)
    : match_result = 
      begin match d with 
      | Inj(_, side', d') -> 
        begin match (side, side') with 
        | (L, L) | (R, R) -> 
          let side_casts = 
            List.map (fun (c : HTyp.t * HTyp.t * HTyp.t * HTyp.t) -> 
              let (tyL1, tyR1, tyL2, tyR2) = c in 
              begin match side with 
              | L -> (tyL1, tyL2)
              | R -> (tyR1, tyR2)
              end) casts in 
          matches dp (apply_casts d' side_casts)
        | _ -> DoesNotMatch
        end
      | Cast(d', HTyp.Sum(tyL1, tyR1), HTyp.Sum(tyL2, tyR2)) -> 
        matches_cast_Inj side dp d' ((tyL1, tyR1, tyL2, tyR2)::casts)
      | Cast(d', HTyp.Sum(_, _), HTyp.Hole)
      | Cast(d', HTyp.Hole, HTyp.Sum(_, _)) -> 
        matches_cast_Inj side dp d' casts
      | Cast(_, _, _) -> DoesNotMatch
      | BoundVar _ -> DoesNotMatch
      | FreeVar(_, _, _, _) -> Indet
      | Let(_, _, _) -> Indet
      | FixF(_, _, _) -> DoesNotMatch
      | Lam(_, _, _) -> DoesNotMatch
      | Ap(_, _) -> Indet
      | BinNumOp(_, _, _) -> Indet
      | BoolLit _ -> DoesNotMatch
      | NumLit _ -> DoesNotMatch
      | ListNil -> DoesNotMatch
      | Cons(_, _) -> DoesNotMatch
      | Pair(_, _) -> DoesNotMatch
      | Triv -> DoesNotMatch
      | Case(_, _, _) -> Indet
      | EmptyHole(_, _, _) -> Indet
      | NonEmptyHole(_, _, _, _, _) -> Indet
      | FailedCast(_, _, _) -> Indet
      end
  and matches_cast_Pair
    (dp1 : DHPat.t)
    (dp2 : DHPat.t)
    (d : t)
    (left_casts : (HTyp.t * HTyp.t) list)
    (right_casts : (HTyp.t * HTyp.t) list)
    : match_result = 
      begin match d with 
      | Pair(d1, d2) -> 
        begin match matches dp1 (apply_casts d1 left_casts) with 
        | DoesNotMatch -> DoesNotMatch
        | Indet -> Indet
        | Matches env1 -> 
          begin match matches dp2 (apply_casts d2 right_casts) with 
          | DoesNotMatch -> DoesNotMatch
          | Indet -> Indet
          | Matches env2 -> 
            Matches (Environment.union env1 env2)
          end
        end
      | Cast(d', HTyp.Prod(tyL1, tyR1), HTyp.Prod(tyL2, tyR2)) -> 
        matches_cast_Pair dp1 dp2 d' 
          ((tyL1, tyL2)::left_casts)
          ((tyR1, tyR2)::right_casts)
      | Cast(d', HTyp.Prod(_, _), HTyp.Hole)  
      | Cast(d', HTyp.Hole, HTyp.Prod(_, _)) -> 
        matches_cast_Pair dp1 dp2 d' left_casts right_casts
      | Cast(_, _, _) -> DoesNotMatch
      | BoundVar _ -> DoesNotMatch
      | FreeVar(_, _, _, _) -> Indet
      | Let(_, _, _) -> Indet
      | FixF(_, _, _) -> DoesNotMatch
      | Lam(_, _, _) -> DoesNotMatch
      | Ap(_, _) -> Indet
      | BinNumOp(_, _, _) -> Indet
      | BoolLit _ -> DoesNotMatch
      | NumLit _ -> DoesNotMatch
      | Inj(_, _, _) -> DoesNotMatch
      | ListNil -> DoesNotMatch
      | Cons(_, _) -> DoesNotMatch
      | Triv -> DoesNotMatch
      | Case(_, _, _) -> Indet
      | EmptyHole(_, _, _) -> Indet
      | NonEmptyHole(_, _, _, _, _) -> Indet
      | FailedCast(_, _, _) -> Indet
      end
  and matches_cast_Cons
    (dp1 : DHPat.t)
    (dp2 : DHPat.t)
    (d : t)
    (elt_casts : (HTyp.t * HTyp.t) list)
    : match_result = 
      begin match d with 
      | Cons(d1, d2) -> 
        begin match matches dp1 (apply_casts d1 elt_casts) with 
        | DoesNotMatch -> DoesNotMatch
        | Indet -> Indet
        | Matches env1 -> 
          let list_casts = List.map (fun (c : HTyp.t * HTyp.t) -> 
            let (ty1, ty2) = c in 
            (HTyp.List ty1, HTyp.List ty2)) elt_casts in 
          begin match matches dp2 (apply_casts d2 list_casts) with 
          | DoesNotMatch -> DoesNotMatch
          | Indet -> Indet
          | Matches env2 -> 
            Matches (Environment.union env1 env2)
          end
        end
      | Cast(d', HTyp.List ty1, HTyp.List ty2) -> 
        matches_cast_Cons dp1 dp2 d' ((ty1, ty2)::elt_casts)
      | Cast(d', HTyp.List _, HTyp.Hole) -> 
        matches_cast_Cons dp1 dp2 d' elt_casts
      | Cast(d', HTyp.Hole, HTyp.List _) -> 
        matches_cast_Cons dp1 dp2 d' elt_casts
      | Cast(_, _, _) -> DoesNotMatch
      | BoundVar _ -> DoesNotMatch
      | FreeVar(_, _, _, _) -> Indet
      | Let(_, _, _) -> Indet
      | FixF(_, _, _) -> DoesNotMatch
      | Lam(_, _, _) -> DoesNotMatch
      | Ap(_, _) -> Indet
      | BinNumOp(_, _, _) -> Indet
      | BoolLit _ -> DoesNotMatch
      | NumLit _ -> DoesNotMatch
      | Inj(_, _, _) -> DoesNotMatch
      | ListNil -> DoesNotMatch
      | Pair(_, _) -> DoesNotMatch
      | Triv -> DoesNotMatch
      | Case(_, _, _) -> Indet
      | EmptyHole(_, _, _) -> Indet
      | NonEmptyHole(_, _, _, _, _) -> Indet
      | FailedCast(_, _, _) -> Indet
      end

  (* Implementation of type assignment judgment in POPL 2019 paper.
   * Not actually called anywhere, now stale.
  Inductive type_result : Type = 
  | WellTyped : HTyp.t -> type_result
  | IllTyped.

  Fixpoint assign_type 
    (fuel : Fuel.t) 
    (gamma : VarCtx.t) (delta : Delta.t) 
    (d : t) 
    : type_result = 
      begin match with 
      | Fuel.Kicked -> IllTyped
      | Fuel.More -> 
      let assign_type = assign_type in 
      begin match d with 
      | BoundVar x -> 
        begin match (Var.is_valid x, VarMap.lookup gamma x) with 
        | (true, Some ty) -> WellTyped ty
        | _ -> IllTyped
        end
      | FreeVar u _ sigma x -> 
        if (Var.is_valid x) then 
          begin match MetaVarMap.lookup delta u with 
          | Some (ty, gamma') -> 
            if check_type_env gamma delta sigma gamma' then
              WellTyped ty
            else IllTyped
          | None -> IllTyped
          end
        else IllTyped
      |  d1 d2 -> 
        begin match (Var.is_valid_binder x, assign_type gamma delta d1) with 
        | (true, WellTyped ty1) -> 
          let gamma' = VarMap.extend gamma (x, ty1) in 
          assign_type gamma' delta d2
        | _ -> IllTyped
        end
      | FixF x ((HTyp.Arrow _ _) as ty1) d1 -> 
        let gamma' = VarMap.extend gamma (x, ty1) in 
        begin match (Var.is_valid_binder x, assign_type gamma' delta d1) with 
        | (true, WellTyped ty2) -> 
          if HTyp.eq ty1 ty2 then WellTyped ty2 else IllTyped
        | _ -> IllTyped
        end
      | FixF x _ d1 -> IllTyped
      | Lam x ty1 d1 -> 
        let gamma' = VarMap.extend gamma (x, ty1) in 
        begin match (Var.is_valid_binder x, assign_type gamma' delta d1) with 
        | (true, WellTyped ty2) -> WellTyped (HTyp.Arrow ty1 ty2)
        | _ -> IllTyped
        end
      | Ap d1 d2 -> 
        begin match assign_type gamma delta d1 with 
        | IllTyped -> IllTyped
        | WellTyped (HTyp.Arrow ty2 ty) -> 
          begin match assign_type gamma delta d2 with 
          | IllTyped -> IllTyped
          | WellTyped ty2' -> 
            if HTyp.eq ty2 ty2' then WellTyped ty
            else IllTyped
          end
        | WellTyped _ -> IllTyped
        end
      | NumLit _ -> WellTyped HTyp.Num
      | BinNumOp _ d1 d2 -> 
        begin match (assign_type gamma delta d1, 
               assign_type gamma delta d2) with 
        | (WellTyped HTyp.Num, WellTyped HTyp.Num) -> 
          WellTyped HTyp.Num
        | _ -> IllTyped
        end
      | Inj other_ty side d1 -> 
        begin match assign_type gamma delta d1 with
        | IllTyped -> IllTyped
        | WellTyped ty1 -> 
          begin match side with 
          | L -> WellTyped (HTyp.Sum ty1 other_ty)
          | R -> WellTyped (HTyp.Sum other_ty ty1)
          end
        end
      | Case d1 (x, d2) (y, d3) -> 
        begin match ((Var.is_valid_binder x) && (Var.is_valid_binder y), assign_type gamma delta d1) with 
        | (true, WellTyped (HTyp.Sum tyL tyR)) -> 
          let gamma1 = VarMap.extend gamma (x, tyL) in 
          let gamma2 = VarMap.extend gamma (y, tyR) in 
          begin match (assign_type gamma1 delta d2,
                 assign_type gamma2 delta d3) with 
          | (WellTyped ty2, WellTyped ty3) -> 
            if HTyp.eq ty2 ty3 then WellTyped ty2
            else IllTyped
          | _ -> IllTyped
          end
        | _ -> IllTyped
        end
      | EmptyHole u _ sigma -> 
        begin match MetaVarMap.lookup delta u with 
        | Some (ty, gamma') -> 
          if check_type_env gamma delta sigma gamma' then 
            WellTyped ty
          else IllTyped
        | None -> IllTyped
        end
      | NonEmptyHole reason u _ sigma d1 -> 
        begin match assign_type gamma delta d1 with 
        | WellTyped _ -> 
          begin match MetaVarMap.lookup delta u with 
          | Some (ty, gamma') -> 
            if check_type_env gamma delta sigma gamma' then
              WellTyped ty
            else IllTyped
          | None -> IllTyped
          end
        | IllTyped -> IllTyped
        end
      | Cast d1 ty1 ty2 
      | FailedCast d1 ty1 ty2 -> 
        begin match assign_type gamma delta d1 with 
        | IllTyped -> IllTyped
        | WellTyped ty1' -> 
          if HTyp.eq ty1 ty1' && 
           HTyp.consistent ty1 ty2
          then WellTyped ty2
          else IllTyped
        end
      end
      end
  with check_type_env (fuel : Fuel.t)
          (gamma : VarCtx.t) (delta : Delta.t) 
          (sigma : Environment.t) 
          (gamma' : VarCtx.t) : bool = 
      begin match with 
      | Fuel.More -> 
          Coq.Lists.List.forallb  
            (fun xd : Var.t * t -> 
              let (x, d) = xd in 
              begin match assign_type gamma delta d with 
              | WellTyped ty -> 
                begin match VarMap.lookup gamma' x with 
                | Some ty' -> HTyp.consistent ty ty'
                | None -> false
                end
              | IllTyped -> false
              end)
            sigma
      | Fuel.Kicked -> false
      end.
  *)
  
  type expand_result = 
  | Expands of t * HTyp.t * Delta.t
  | DoesNotExpand

  let id_env (ctx : VarCtx.t) : Environment.t = 
    VarMap.map
      (fun xt -> 
        let (x, _) = xt in DHExp.BoundVar x)
      ctx

  let rec syn_expand 
    (ctx : Contexts.t) 
    (delta : Delta.t)
    (e : UHExp.t) 
    : expand_result = 
      begin match e with 
      | UHExp.Parenthesized e1 -> syn_expand ctx delta e1
      | UHExp.Tm(NotInHole, e') -> syn_expand' ctx delta e'
      | UHExp.Tm(InHole(TypeInconsistent as reason, u), e')  
      | UHExp.Tm(InHole((WrongLength as reason), u),
        ((UHExp.OpSeq(Skel.BinOp(InHole(WrongLength, _), UHExp.Comma, _, _), _) as e'))) ->
        begin match syn_expand' ctx delta e' with 
        | Expands(d, _, delta) -> 
          let gamma = Contexts.gamma ctx in 
          let sigma = id_env gamma in 
          let delta = MetaVarMap.extend delta (u, (ExpressionHole, HTyp.Hole, gamma)) in 
          Expands(
            NonEmptyHole(reason, u, 0, sigma, d),
            HTyp.Hole,
            delta)
        | DoesNotExpand -> DoesNotExpand
        end
      | UHExp.Tm(InHole(WrongLength, _), _) -> DoesNotExpand
      end
  and syn_expand'
    (ctx : Contexts.t)
    (delta : Delta.t)
    (e : UHExp.t')
    : expand_result = 
      begin match e with 
      | UHExp.EmptyHole u -> 
        let gamma = Contexts.gamma ctx in 
        let sigma = id_env gamma in 
        let d = DHExp.EmptyHole(u, 0, sigma) in 
        let ty = HTyp.Hole in 
        let delta = MetaVarMap.extend delta (u, (ExpressionHole, ty, gamma)) in 
        Expands(d, ty, delta)
      | UHExp.Asc(e1, uty) -> 
        let ty = UHTyp.expand uty in 
        begin match ana_expand ctx delta e1 ty with 
        | DoesNotExpand -> DoesNotExpand
        | Expands(d1, ty', delta) -> 
          Expands( 
            cast d1 ty' ty,
            ty,
            delta)
        end
      | UHExp.Var(NotInVHole, x) -> 
        let gamma = Contexts.gamma ctx in 
        begin match VarMap.lookup gamma x with 
        | Some ty -> Expands(DHExp.BoundVar x, ty, delta)
        | None -> DoesNotExpand
        end
      | UHExp.Var(InVHole u, x) -> 
        let gamma = Contexts.gamma ctx in 
        let sigma = id_env gamma in 
        let delta = MetaVarMap.extend delta (u, (ExpressionHole, HTyp.Hole, gamma)) in 
        Expands(
          DHExp.FreeVar(u, 0, sigma, x),
          HTyp.Hole,
          delta)
      | UHExp.Lam(p, ann, e1) ->
        let ty1 =
          begin match ann with 
          | Some uty1 -> UHTyp.expand uty1
          | None -> HTyp.Hole
          end in
        begin match DHPat.ana_expand ctx delta p ty1 with
        | DHPat.DoesNotExpand -> DoesNotExpand
        | DHPat.Expands(dp, ty1, ctx, delta) ->
          begin match syn_expand ctx delta e1 with
          | DoesNotExpand -> DoesNotExpand
          | Expands(d1, ty2, delta) ->
            let d = Lam(dp, ty1, d1) in
            Expands(d, HTyp.Arrow(ty1, ty2), delta)
          end
        end
      | UHExp.Let(p, ann, e1, e2) ->
        begin match ann with
        | Some uty1 ->
          let ty1 = UHTyp.expand uty1 in
          let (ctx1, is_recursive_fn) = UHExp.ctx_for_let' ctx p ty1 e1 in
          begin match ana_expand ctx1 delta e1 ty1 with
          | DoesNotExpand -> DoesNotExpand
          | Expands(d1, ty1, delta) ->
            let d1 =
              begin match is_recursive_fn with
              | None -> d1
              | Some x -> FixF(x, ty1, d1)
              end in
            begin match DHPat.ana_expand ctx delta p ty1 with
            | DHPat.DoesNotExpand -> DoesNotExpand
            | DHPat.Expands(dp, typ, ctx, delta) ->
              begin match syn_expand ctx delta e2 with
              | DoesNotExpand -> DoesNotExpand
              | Expands(d2, ty, delta) -> 
                let d = Let(dp, d1, d2) in
                Expands(d, ty, delta)
              end
            end
          end
        | None ->
          begin match syn_expand ctx delta e1 with
          | DoesNotExpand -> DoesNotExpand
          | Expands(d1, ty1, delta) ->
            begin match DHPat.ana_expand ctx delta p ty1 with
            | DHPat.DoesNotExpand -> DoesNotExpand
            | DHPat.Expands(dp, ty1, ctx, delta) ->
              begin match syn_expand ctx delta e2 with
              | DoesNotExpand -> DoesNotExpand
              | Expands(d2, ty, delta2) -> 
                let d = Let(dp, d1, d2) in
                Expands(d, ty, delta)
              end
            end
          end
        end
      | UHExp.NumLit n -> 
        Expands(NumLit n, HTyp.Num, delta) 
      | UHExp.BoolLit b ->
        Expands(BoolLit b, HTyp.Bool, delta) 
      | UHExp.Inj(side, e1) -> 
        begin match syn_expand ctx delta e1 with 
        | DoesNotExpand -> DoesNotExpand
        | Expands(d1, ty1, delta) -> 
          let d = DHExp.Inj(HTyp.Hole, side, d1) in 
          let ty = 
            begin match side with 
            | L -> HTyp.Sum(ty1, HTyp.Hole)
            | R -> HTyp.Sum(HTyp.Hole, ty1)
            end in 
          Expands(d, ty, delta)
        end
      | UHExp.ListNil -> Expands(ListNil, HTyp.List HTyp.Hole, delta)
      | UHExp.Case(_, _) -> DoesNotExpand
      | UHExp.OpSeq(skel, seq) -> 
        syn_expand_skel ctx delta skel seq
      | UHExp.ApPalette(name, serialized_model, hole_data) -> DoesNotExpand
        (* TODO fix me *)
        (* let (_, palette_ctx) = ctx in 
        begin match (VarMap.lookup palette_ctx name) with
        | Some palette_defn -> 
          let expansion_ty = UHExp.PaletteDefinition.expansion_ty palette_defn in  
          let to_exp = UHExp.PaletteDefinition.to_exp palette_defn in
          let expansion = to_exp serialized_model in 
          let (_, hole_map) = hole_data in
          (* bind each free variable in expansion by wrapping expansion
           * in lambda, then apply lambda to args in hole data
           *)
          let bound_expansion :=
              NatMap.fold hole_map
                (fun bound entry ->
                  let (n, typ_exp) = entry in
                  let (htyp, hexp) = typ_exp in
                  let lam = UHExp.Tm NotInHole (UHExp.Lam (UHExp.PaletteHoleData.mk_hole_ref_var_name n) bound) in
                  let hexp_ann = UHExp.Tm NotInHole (UHExp.Asc (UHExp.Parenthesized hexp) (UHTyp.contract htyp)) in
                  let opseq = OperatorSeq.ExpOpExp (UHExp.Parenthesized lam) UHExp.Space (UHExp.Parenthesized hexp_ann) in
                  let ap = UHExp.OpSeq (Associator.associate_exp opseq) opseq in
                  UHExp.Tm NotInHole ap
                )
                expansion in
          ana_expand ctx bound_expansion expansion_ty
        | None -> DoesNotExpand
        end *)
      end
  and syn_expand_skel 
    (ctx : Contexts.t)
    (delta : Delta.t)
    (skel : UHExp.skel_t)
    (seq : UHExp.opseq)
    : expand_result = 
      begin match skel with 
      | Skel.Placeholder n -> 
        begin match OperatorSeq.seq_nth n seq with 
        | None -> DoesNotExpand
        | Some en -> syn_expand ctx delta en
        end
      | Skel.BinOp(InHole(TypeInconsistent as reason, u), op, skel1, skel2)  
      | Skel.BinOp(InHole(WrongLength as reason, u), (UHExp.Comma as op), skel1, skel2) -> 
        let skel_not_in_hole = Skel.BinOp(NotInHole, op, skel1, skel2) in 
        begin match syn_expand_skel ctx delta skel_not_in_hole seq with 
        | DoesNotExpand -> DoesNotExpand
        | Expands(d, _, delta) -> 
          let gamma = Contexts.gamma ctx in 
          let sigma = id_env gamma in 
          let delta = MetaVarMap.extend delta (u, (ExpressionHole, HTyp.Hole, gamma)) in 
          Expands(
            NonEmptyHole(reason, u, 0, sigma, d),
            HTyp.Hole,
            delta)
        end
      | Skel.BinOp(InHole(WrongLength, _), _, _, _) -> DoesNotExpand
      | Skel.BinOp(NotInHole, UHExp.Space, skel1, skel2) -> 
        begin match UHExp.syn_skel ctx skel1 seq None with 
        | None -> DoesNotExpand
        | Some (ty1, _) -> 
          begin match HTyp.matched_arrow ty1 with 
          | None -> DoesNotExpand
          | Some (ty2, ty) -> 
            let ty2_arrow_ty = HTyp.Arrow(ty2, ty) in 
            begin match ana_expand_skel ctx delta skel1 seq ty2_arrow_ty with 
            | DoesNotExpand -> DoesNotExpand
            | Expands(d1, ty1', delta) -> 
              begin match ana_expand_skel ctx delta skel2 seq ty2 with 
              | DoesNotExpand -> DoesNotExpand
              | Expands(d2, ty2', delta) -> 
                let dc1 = cast d1 ty1' ty2_arrow_ty in 
                let dc2 = cast d2 ty2' ty2 in 
                let d = Ap(dc1, dc2) in 
                Expands(d, ty, delta)
              end
            end
          end
        end
      | Skel.BinOp(NotInHole, UHExp.Comma, skel1, skel2) ->
        begin match syn_expand_skel ctx delta skel1 seq  with
        | DoesNotExpand -> DoesNotExpand
        | Expands(d1, ty1, delta) ->
          begin match syn_expand_skel ctx delta skel2 seq with
          | DoesNotExpand -> DoesNotExpand
          | Expands(d2, ty2, delta) ->
            let d = Pair(d1, d2) in
            let ty = HTyp.Prod(ty1, ty2) in
            Expands(d, ty, delta)
          end
        end
      | Skel.BinOp(NotInHole, UHExp.Cons, skel1, skel2) ->
        begin match syn_expand_skel ctx delta skel1 seq with
        | DoesNotExpand -> DoesNotExpand
        | Expands(d1, ty1, delta) ->
          let ty = HTyp.List ty1 in
          begin match ana_expand_skel ctx delta skel2 seq ty with
          | DoesNotExpand -> DoesNotExpand
          | Expands(d2, ty', delta) ->
            begin match HTyp.join ty ty' with 
            | None -> DoesNotExpand
            | Some ty -> 
              let d = Cons(d1, d2) in
              Expands(d, ty, delta)
            end
          end
        end
      | Skel.BinOp(NotInHole, (UHExp.Plus as op), skel1, skel2)
      | Skel.BinOp(NotInHole, (UHExp.Times as op), skel1, skel2)
      | Skel.BinOp(NotInHole, (UHExp.LessThan as op), skel1, skel2) ->
        begin match ana_expand_skel ctx delta skel1 seq HTyp.Num with 
        | DoesNotExpand -> DoesNotExpand
        | Expands(d1, ty1, delta) -> 
          begin match ana_expand_skel ctx delta skel2 seq HTyp.Num with 
          | DoesNotExpand -> DoesNotExpand
          | Expands(d2, ty2, delta) -> 
            let dc1 = cast d1 ty1 HTyp.Num in 
            let dc2 = cast d2 ty2 HTyp.Num in 
            begin match of_op op with 
            | None -> DoesNotExpand
            | Some (op, ty) -> 
              let d = BinNumOp(op, dc1, dc2) in 
              Expands(d, ty, delta)
            end
          end
        end
      end
  and ana_expand 
    (ctx : Contexts.t) 
    (delta : Delta.t)
    (e : UHExp.t)
    (ty : HTyp.t) 
    : expand_result = 
      begin match e with 
      | UHExp.Tm (NotInHole, e') -> ana_expand' ctx delta e' ty
      | UHExp.Tm(InHole(TypeInconsistent as reason, u), e') ->
        begin match syn_expand' ctx delta e' with 
        | DoesNotExpand -> DoesNotExpand
        | Expands(d, _, delta) -> 
          let gamma = Contexts.gamma ctx in 
          let sigma = id_env gamma in 
          let delta = MetaVarMap.extend delta (u, (ExpressionHole, ty, gamma)) in 
          Expands( 
            NonEmptyHole(reason, u, 0, sigma, d),
            ty,
            delta)
        end
      | UHExp.Tm(InHole(WrongLength as reason, u), 
          ((UHExp.OpSeq(Skel.BinOp(InHole(WrongLength, _), UHExp.Comma, _, _), _) as e'))) -> 
        begin match ana_expand' ctx delta e' ty with 
        | DoesNotExpand -> DoesNotExpand
        | Expands(d1, _, delta) -> 
          let gamma = Contexts.gamma ctx in 
          let sigma = id_env gamma in  
          let delta = MetaVarMap.extend delta (u, (ExpressionHole, ty, gamma)) in 
          let d = NonEmptyHole(reason, u, 0, sigma, d1) in 
          Expands(d, ty, delta)
        end
      | UHExp.Tm (InHole(WrongLength, _), _) -> DoesNotExpand
      | UHExp.Parenthesized e1 -> ana_expand ctx delta e1 ty
      end
  and ana_expand'
    (ctx : Contexts.t)
    (delta : Delta.t)
    (e : UHExp.t')
    (ty : HTyp.t) 
    : expand_result = 
      begin match e with 
      | UHExp.EmptyHole u -> 
        let gamma = Contexts.gamma ctx in 
        let sigma = id_env gamma in 
        let d = EmptyHole(u, 0, sigma) in 
        let delta = MetaVarMap.extend delta (u, (ExpressionHole, ty, gamma)) in 
        Expands(d, ty, delta)
      | UHExp.Var(InVHole u, x) -> 
        let gamma = Contexts.gamma ctx in 
        let sigma = id_env gamma in 
        let delta = MetaVarMap.extend delta (u, (ExpressionHole, ty, gamma)) in 
        Expands(
          FreeVar(u, 0, sigma, x),
          ty,
          delta)
      | UHExp.Let(p, ann, e1, e2) ->
        begin match ann with
        | Some uty1 ->
          let ty1 = UHTyp.expand uty1 in
          let (ctx1, is_recursive_fn) = UHExp.ctx_for_let' ctx p ty1 e1 in
          begin match ana_expand ctx1 delta e1 ty1 with
          | DoesNotExpand -> DoesNotExpand
          | Expands(d1, ty1, delta) ->
            let d1 =
              begin match is_recursive_fn with
              | None -> d1
              | Some x -> FixF(x, ty1, d1)
              end in
            begin match DHPat.ana_expand ctx delta p ty1 with
            | DHPat.DoesNotExpand -> DoesNotExpand
            | DHPat.Expands(dp, _, ctx, delta) ->
              begin match ana_expand ctx delta e2 ty with
              | DoesNotExpand -> DoesNotExpand
              | Expands(d2, ty, delta) ->
                let d = Let(dp, d1, d2) in
                Expands(d, ty, delta)
              end
            end
          end
        | None ->
          begin match syn_expand ctx delta e1 with
          | DoesNotExpand -> DoesNotExpand
          | Expands(d1, ty1, delta) ->
            begin match DHPat.ana_expand ctx delta p ty1 with
            | DHPat.DoesNotExpand -> DoesNotExpand
            | DHPat.Expands(dp, ty1, ctx, delta) ->
              begin match ana_expand ctx delta e2 ty with
              | DoesNotExpand -> DoesNotExpand
              | Expands(d2, ty, delta) ->
                let d = Let(dp, d1, d2) in
                Expands(d, ty, delta)
              end
            end
          end
        end
      | UHExp.Lam(p, ann, e1) ->
        begin match HTyp.matched_arrow ty with
        | None -> DoesNotExpand
        | Some (ty1_given, ty2) ->
          begin match ann with
          | Some uty1 ->
            let ty1_ann = UHTyp.expand uty1 in
            begin match HTyp.consistent ty1_ann ty1_given with
            | false -> DoesNotExpand
            | true ->
              begin match DHPat.ana_expand ctx delta p ty1_ann with
              | DHPat.DoesNotExpand -> DoesNotExpand
              | DHPat.Expands(dp, ty1p, ctx, delta) ->
                begin match ana_expand ctx delta e1 ty2 with
                | DoesNotExpand -> DoesNotExpand
                | Expands(d1, ty2, delta) ->
                  let ty = HTyp.Arrow(ty1p, ty2) in
                  let d = Lam(dp, ty1p, d1) in
                  Expands(d, ty, delta)
                end
              end
            end
          | None ->
            begin match DHPat.ana_expand ctx delta p ty1_given with
            | DHPat.DoesNotExpand -> DoesNotExpand
            | DHPat.Expands(dp, ty1, ctx, delta) ->
              begin match ana_expand ctx delta e1 ty2 with
              | DoesNotExpand -> DoesNotExpand
              | Expands(d1, ty2, delta) ->
                let ty = HTyp.Arrow(ty1, ty2) in
                let d = Lam(dp, ty1, d1) in
                Expands(d, ty, delta)
              end
            end
          end
        end
      | UHExp.Inj(side, e1) -> 
        begin match HTyp.matched_sum ty with 
        | None -> DoesNotExpand
        | Some (ty1, ty2) -> 
          let e1ty = pick_side side ty1 ty2 in 
          begin match ana_expand ctx delta e1 e1ty with 
          | DoesNotExpand -> DoesNotExpand
          | Expands(d1, e1ty', delta) -> 
            let (ann_ty, ty) = 
              begin match side with 
              | L -> (ty2, HTyp.Sum(e1ty', ty2)) 
              | R -> (ty1, HTyp.Sum(ty1, e1ty'))
              end in 
            let d = Inj(ann_ty, side, d1) in 
            Expands(d, ty, delta)
          end
        end
      | UHExp.ListNil ->
        begin match HTyp.matched_list ty with
        | None -> DoesNotExpand
        | Some _ -> Expands(ListNil, ty, delta)
        end
      | UHExp.Case(e1, rules) ->
        begin match syn_expand ctx delta e1 with
        | DoesNotExpand -> DoesNotExpand
        | Expands(d1, ty1, delta) ->
          begin match ana_expand_rules ctx delta rules ty1 ty with
          | None -> DoesNotExpand
          | Some (drs, delta) ->
            let d = Case(d1, drs, 0) in
            Expands(d, ty, delta)
          end
        end
      | UHExp.OpSeq(skel, seq) -> ana_expand_skel ctx delta skel seq ty
      | UHExp.Asc(_, _) 
      | UHExp.Var(NotInVHole, _)
      | UHExp.BoolLit _
      | UHExp.NumLit _
      | UHExp.ApPalette(_, _, _) -> 
        (* subsumption *)
        syn_expand' ctx delta e
      end
  and ana_expand_rules
    (ctx : Contexts.t)
    (delta : Delta.t)
    (rules :UHExp.rule list)
    (pat_ty : HTyp.t)
    (clause_ty : HTyp.t)
    : (rule list * Delta.t) option =
      List.fold_left (fun b r ->
        begin match b with
        | None -> None
        | Some (drs, delta) ->
          begin match ana_expand_rule ctx delta r pat_ty clause_ty with
          | None -> None
          | Some (dr, delta) ->
            let drs = drs @ [dr] in
            Some (drs, delta)
          end
        end) (Some ([], delta)) rules
  and ana_expand_rule
    (ctx : Contexts.t)
    (delta : Delta.t)
    (r : UHExp.rule)
    (pat_ty : HTyp.t)
    (clause_ty : HTyp.t)
    : (rule * Delta.t) option =
      let UHExp.Rule (p, e) = r in
      begin match DHPat.ana_expand ctx delta p pat_ty with
      | DHPat.DoesNotExpand -> None
      | DHPat.Expands(dp, _, ctx, delta) ->
        begin match ana_expand ctx delta e clause_ty with
        | DoesNotExpand -> None
        | Expands(d1, ty1, delta) ->
          Some (Rule(dp, cast d1 ty1 clause_ty), delta)
        end
      end
  and ana_expand_skel
    (ctx : Contexts.t)
    (delta : Delta.t)
    (skel : UHExp.skel_t)
    (seq : UHExp.opseq)
    (ty : HTyp.t)
    : expand_result = 
      begin match skel with 
      | Skel.Placeholder n ->
        begin match OperatorSeq.seq_nth n seq with 
        | None -> DoesNotExpand
        | Some en -> ana_expand ctx delta en ty
        end
      | Skel.BinOp(InHole(TypeInconsistent as reason, u), op, skel1, skel2) ->
        let skel_not_in_hole = Skel.BinOp(NotInHole, op, skel1, skel2) in
        begin match syn_expand_skel ctx delta skel_not_in_hole seq with
        | DoesNotExpand -> DoesNotExpand
        | Expands(d1, _, delta) ->
          let gamma = Contexts.gamma ctx in
          let sigma = id_env gamma in 
          let delta = MetaVarMap.extend delta (u, (ExpressionHole, ty, gamma)) in
          let d = DHExp.NonEmptyHole(reason, u, 0, sigma, d1) in
          Expands(d, ty, delta)
        end
      | Skel.BinOp(NotInHole, UHExp.Comma, skel1, skel2) ->
        begin match HTyp.matched_prod ty with
        | None -> DoesNotExpand
        | Some (ty1, ty2) ->
          begin match ana_expand_skel ctx delta skel1 seq ty1 with
          | DoesNotExpand -> DoesNotExpand
          | Expands(d1, ty1, delta) ->
            begin match ana_expand_skel ctx delta skel2 seq ty2 with
            | DoesNotExpand -> DoesNotExpand
            | Expands(d2, ty2, delta) ->
              let d = Pair(d1, d2) in
              Expands(d, HTyp.Prod(ty1, ty2), delta)
            end
          end
        end
      | Skel.BinOp(InHole(WrongLength, u), UHExp.Comma, skel1, skel2) -> 
        begin match ty with 
        | HTyp.Prod(ty1, ty2) -> 
          let types = HTyp.get_tuple ty1 ty2 in 
          let skels = UHExp.get_tuple skel1 skel2 in 
          let (zipped, remainder) = HTyp.zip_with_skels skels types in 
          let processed1 = 
            List.fold_right (fun (skel_ty : UHExp.skel_t * HTyp.t) opt_result -> 
              begin match opt_result with 
              | None -> None
              | Some (elts, delta) -> 
                let (skel, ty) = skel_ty in 
                begin match ana_expand_skel ctx delta skel seq ty with 
                | DoesNotExpand -> None
                | Expands(d, ty, delta) -> 
                  Some ((d, ty)::elts, delta)
                end
              end) zipped (Some ([], delta)) in 
          begin match processed1 with 
          | None -> DoesNotExpand
          | Some (elts1, delta) -> 
            let processed2 = 
              List.fold_right (fun (skel : UHExp.skel_t) opt_result -> 
                begin match opt_result with 
                | None -> None
                | Some (elts, delta) -> 
                  begin match syn_expand_skel ctx delta skel seq with 
                  | DoesNotExpand -> None
                  | Expands(d, ty, delta) -> 
                    Some ((d, ty)::elts, delta)
                  end
                end) remainder (Some ([], delta)) in 
            begin match processed2 with 
            | None -> DoesNotExpand
            | Some (elts2, delta) -> 
              let (ds, tys) = Util.unzip (elts1 @ elts2) in 
              let d = make_tuple ds in 
              let ty = HTyp.make_tuple tys in 
              Expands(d, ty, delta)
            end
          end
        | _ -> DoesNotExpand
        end
      | Skel.BinOp(InHole(WrongLength, _), _, _, _) -> DoesNotExpand
      | Skel.BinOp(NotInHole, UHExp.Cons, skel1, skel2) ->
        begin match HTyp.matched_list ty with
        | None -> DoesNotExpand
        | Some ty_elt ->
          begin match ana_expand_skel ctx delta skel1 seq ty_elt with
          | DoesNotExpand -> DoesNotExpand
          | Expands(d1, ty_elt', delta) ->
            let ty_list = HTyp.List ty_elt in
            begin match ana_expand_skel ctx delta skel2 seq ty_list with
            | DoesNotExpand -> DoesNotExpand
            | Expands(d2, HTyp.List ty_elt'', delta) ->
              begin match HTyp.join ty_elt' ty_elt'' with 
              | None -> DoesNotExpand
              | Some ty_elt -> 
                let ty = HTyp.List ty_elt in 
                let d = Cons(d1, d2) in
                Expands(d, ty, delta)
              end
            | Expands(_, _, _) -> DoesNotExpand 
            end
          end
        end
      | Skel.BinOp(_, UHExp.Plus, _, _) 
      | Skel.BinOp(_, UHExp.Times, _, _)
      | Skel.BinOp(_, UHExp.LessThan, _, _) 
      | Skel.BinOp(_, UHExp.Space, _, _) -> 
        begin match syn_expand_skel ctx delta skel seq with 
        | DoesNotExpand -> DoesNotExpand
        | Expands(d, ty', delta) -> 
          if HTyp.consistent ty ty' then 
            Expands(d, ty', delta)
          else DoesNotExpand
        end
      end

  module HoleInstance = struct
    type t = MetaVar.t * inst_num
  end

  module InstancePath = struct
    type t = (HoleInstance.t * Var.t) list 
  end

  module HoleInstanceInfo = struct
    type t = ((Environment.t * InstancePath.t) list) MetaVarMap.t

    let empty : t = MetaVarMap.empty

    let next (hii : t) (u : MetaVar.t) (sigma : Environment.t) (path : InstancePath.t) : nat * t = 
      let (envs, hii) = 
        MetaVarMap.insert_or_map hii u (fun _ -> [(sigma, path)]) (
          fun envs -> 
            (sigma, path)::envs 
        ) in
      ((List.length envs) - 1, hii)

    let update_environment (hii : t) (inst : HoleInstance.t) (sigma : Environment.t) : t = 
      let (u, i) = inst in 
      let (_, hii) = MetaVarMap.update_with
        (
          fun instances -> 
            let length = List.length instances in 
            Util.update_nth (length - i - 1) instances 
            (fun (inst_info : Environment.t * InstancePath.t) -> 
              let (_, path) = inst_info in 
              (sigma, path)
            )
        )
        u hii [] in 
      hii 

    let num_instances (hii : t) (u : MetaVar.t) = 
      begin match MetaVarMap.lookup hii u with 
      | Some envs -> List.length envs
      | None -> 0
      end

    let default_instance (hii : t) (u : MetaVar.t) = 
      begin match MetaVarMap.lookup hii u with 
      | Some envs -> 
        begin match envs with 
        | [] -> None
        | _ :: _ -> Some (u, 0)
        end
      | None -> None
      end

    let lookup (hii : t) (inst : HoleInstance.t) = 
      let (u, i) = inst in 
      begin match MetaVarMap.lookup hii u with 
      | Some envs -> 
        let length = List.length envs in 
        List.nth_opt envs (length - i - 1)
      | None -> None
      end
  end

  let rec renumber_result_only_pat
    (path : InstancePath.t)
    (hii : HoleInstanceInfo.t)
    (dp : DHPat.t)
    : (DHPat.t * HoleInstanceInfo.t) =
    begin match dp with
    | DHPat.Wild
    | DHPat.Var _
    | DHPat.NumLit _
    | DHPat.BoolLit _
    | DHPat.ListNil
    | DHPat.Triv -> (dp, hii)
    | DHPat.EmptyHole(u, _) ->
      (* TODO: Pattern holes don't need environments. Maybe this calls
       * for a refactoring of types to reflect this, e.g., a separate
       * PatHoleInstance type. Passing in an empty environment for now. *)
      let sigma = Environment.empty in
      let (i, hii) = HoleInstanceInfo.next hii u sigma path in
      (DHPat.EmptyHole(u, i), hii)
    | DHPat.NonEmptyHole(reason, u, _, dp1) ->
      (* TODO: see above *)
      let sigma = Environment.empty in
      let (i, hii) = HoleInstanceInfo.next hii u sigma path in
      let (dp1, hii) = renumber_result_only_pat path hii dp1 in
      (DHPat.NonEmptyHole(reason, u, i, dp1), hii)
    | DHPat.Inj(side, dp1) ->
      let (dp1, hii) = renumber_result_only_pat path hii dp1 in
      (DHPat.Inj(side, dp1), hii)
    | DHPat.Pair(dp1, dp2) ->
      let (dp1, hii) = renumber_result_only_pat path hii dp1 in
      let (dp2, hii) = renumber_result_only_pat path hii dp2 in
      (DHPat.Pair(dp1, dp2), hii)
    | DHPat.Cons(dp1, dp2) ->
      let (dp1, hii) = renumber_result_only_pat path hii dp1 in
      let (dp2, hii) = renumber_result_only_pat path hii dp2 in
      (DHPat.Cons(dp1, dp2), hii)
    | DHPat.Ap(dp1, dp2) ->
      let (dp1, hii) = renumber_result_only_pat path hii dp1 in
      let (dp2, hii) = renumber_result_only_pat path hii dp2 in
      (DHPat.Pair(dp1, dp2), hii) 
    end

  let rec renumber_result_only
    (path : InstancePath.t) (hii : HoleInstanceInfo.t) (d : DHExp.t) 
    : (DHExp.t * HoleInstanceInfo.t) =
    begin match d with 
    | BoundVar _
    | BoolLit _
    | NumLit _
    | ListNil
    | Triv -> (d, hii)
    | Let(dp, d1, d2) -> 
      let (d1, hii) = renumber_result_only path hii d1 in 
      let (d2, hii) = renumber_result_only path hii d2 in 
      (Let(dp, d1, d2), hii)
    | FixF(x, ty, d1) -> 
      let (d1, hii) = renumber_result_only path hii d1 in 
      (FixF(x, ty, d1), hii)
    | Lam(x, ty, d1) -> 
      let (d1, hii) = renumber_result_only path hii d1 in 
      (Lam(x, ty, d1), hii)
    | Ap(d1, d2) -> 
      let (d1, hii) = renumber_result_only path hii d1 in
      let (d2, hii) = renumber_result_only path hii d2 in 
      (Ap(d1, d2), hii)
    | BinNumOp(op, d1, d2) -> 
      let (d1, hii) = renumber_result_only path hii d1 in
      let (d2, hii) = renumber_result_only path hii d2 in 
      (BinNumOp(op, d1, d2), hii)
    | Inj(ty, side, d1) -> 
      let (d1, hii) = renumber_result_only path hii d1 in 
      (Inj(ty, side, d1), hii)
    | Pair(d1, d2) ->
      let (d1, hii) = renumber_result_only path hii d1 in
      let (d2, hii) = renumber_result_only path hii d2 in
      (Pair(d1, d2), hii)
    | Cons(d1, d2) ->
      let (d1, hii) = renumber_result_only path hii d1 in
      let (d2, hii) = renumber_result_only path hii d2 in
      (Cons(d1, d2), hii)
    | Case(d1, rules, n) ->
      let (d1, hii) = renumber_result_only path hii d1 in
      let (drules, hii) = renumber_result_only_rules path hii rules in 
      (Case(d1, drules, n), hii)
    | EmptyHole(u, _, sigma) -> 
      let (i, hii) = HoleInstanceInfo.next hii u sigma path in 
      (EmptyHole(u, i, sigma), hii)
    | NonEmptyHole(reason, u, _, sigma, d1) -> 
      let (i, hii) = HoleInstanceInfo.next hii u sigma path in 
      let (d1, hii) = renumber_result_only path hii d1 in 
      (NonEmptyHole(reason, u, i, sigma, d1), hii)
    | FreeVar(u, _, sigma, x) -> 
      let (i, hii) = HoleInstanceInfo.next hii u sigma path in 
      (FreeVar(u, i, sigma, x), hii)
    | Cast(d1, ty1, ty2) -> 
      let (d1, hii) = renumber_result_only path hii d1 in 
      (Cast(d1, ty1, ty2), hii)
    | FailedCast(d1, ty1, ty2) -> 
      let (d1, hii) = renumber_result_only path hii d1 in 
      (FailedCast(d1, ty1, ty2), hii)
    end
  and renumber_result_only_rules
    (path : InstancePath.t)
    (hii : HoleInstanceInfo.t)
    (rules : rule list)
    : (rule list * HoleInstanceInfo.t) =
    List.fold_left (fun b r ->
      let (rs, hii) = b in
      begin match r with
      | Rule(dp, d) ->
        let (dp, hii) = renumber_result_only_pat path hii dp in
        let (d, hii) = renumber_result_only path hii d in
        (rs @ [Rule(dp, d)], hii)
      end) ([], hii) rules

  let rec renumber_sigmas_only
    (path : InstancePath.t) (hii : HoleInstanceInfo.t) (d : DHExp.t) 
    : (DHExp.t * HoleInstanceInfo.t) = 
    begin match d with 
    | BoundVar _
    | BoolLit _
    | NumLit _
    | ListNil
    | Triv -> (d, hii)
    | Let(dp, d1, d2) -> 
      let (d1, hii) = renumber_sigmas_only path hii d1 in 
      let (d2, hii) = renumber_sigmas_only path hii d2 in 
      (Let(dp, d1, d2), hii)
    | FixF(x, ty, d1) -> 
      let (d1, hii) = renumber_sigmas_only path hii d1 in 
      (FixF(x, ty, d1), hii)
    | Lam(x, ty, d1) -> 
      let (d1, hii) = renumber_sigmas_only path hii d1 in 
      (Lam(x, ty, d1), hii)
    | Ap(d1, d2) -> 
      let (d1, hii) = renumber_sigmas_only path hii d1 in
      let (d2, hii) = renumber_sigmas_only path hii d2 in 
      (Ap(d1, d2), hii)
    | BinNumOp(op, d1, d2) -> 
      let (d1, hii) = renumber_sigmas_only path hii d1 in
      let (d2, hii) = renumber_sigmas_only path hii d2 in 
      (BinNumOp(op, d1, d2), hii)
    | Inj(ty, side, d1) -> 
      let (d1, hii) = renumber_sigmas_only path hii d1 in 
      (Inj(ty, side, d1), hii)
    | Pair(d1, d2) ->
      let (d1, hii) = renumber_sigmas_only path hii d1 in
      let (d2, hii) = renumber_sigmas_only path hii d2 in
      (Pair(d1, d2), hii)
    | Cons(d1, d2) ->
      let (d1, hii) = renumber_sigmas_only path hii d1 in
      let (d2, hii) = renumber_sigmas_only path hii d2 in
      (Cons(d1, d2), hii)
    | Case(d1, rules, n) ->
      let (d1, hii) = renumber_sigmas_only path hii d1 in
      let (rules, hii) = renumber_sigmas_only_rules path hii rules in
      (Case(d1, rules, n), hii)
    | EmptyHole(u, i, sigma) -> 
      let (sigma, hii) = renumber_sigma path u i hii sigma in
      let hii = HoleInstanceInfo.update_environment hii (u, i) sigma in 
      (EmptyHole(u, i, sigma), hii)
    | NonEmptyHole(reason, u, i, sigma, d1) -> 
      let (sigma, hii) = renumber_sigma path u i hii sigma in 
      let hii = HoleInstanceInfo.update_environment hii (u, i) sigma in 
      let (d1, hii) = renumber_sigmas_only path hii d1 in 
      (NonEmptyHole(reason, u, i, sigma, d1), hii)
    | FreeVar(u, i, sigma, x) -> 
      let (sigma, hii) = renumber_sigma path u i hii sigma in 
      let hii = HoleInstanceInfo.update_environment hii (u, i) sigma in 
      (FreeVar(u, i, sigma, x), hii)
    | Cast(d1, ty1, ty2) -> 
      let (d1, hii) = renumber_sigmas_only path hii d1 in 
      (Cast(d1, ty1, ty2), hii)
    | FailedCast(d1, ty1, ty2) -> 
      let (d1, hii) = renumber_sigmas_only path hii d1 in 
      (FailedCast(d1, ty1, ty2), hii)
    end
  and renumber_sigmas_only_rules
    (path : InstancePath.t)
    (hii : HoleInstanceInfo.t)
    (rules : rule list)
    : (rule list * HoleInstanceInfo.t) =
    List.fold_left (fun b r ->
      let (rs, hii) = b in
      begin match r with
      | Rule(dp, d) ->
        (* pattern holes don't have environments *)
        let (d, hii) = renumber_sigmas_only path hii d in
        (rs @ [Rule(dp, d)], hii)
      end) ([], hii) rules
  and renumber_sigma
    (path : InstancePath.t) (u : MetaVar.t) (i : inst_num)
    (hii : HoleInstanceInfo.t) (sigma : Environment.t) 
    : (Environment.t * HoleInstanceInfo.t) = 
    let (sigma, hii) = List.fold_right 
      (fun (xd : Var.t * DHExp.t) (acc : Environment.t * HoleInstanceInfo.t) -> 
        let (x, d) = xd in 
        let (sigma_in, hii) = acc in 
        let path = ((u, i), x)::path in
        let (d, hii) = renumber_result_only path hii d in 
        let sigma_out = (x, d)::sigma_in in 
        (sigma_out, hii)
      )
      sigma
      ([], hii)
      in 
    List.fold_right
      (fun (xd : Var.t * DHExp.t) (acc : Environment.t * HoleInstanceInfo.t) -> 
        let (x, d) = xd in 
        let (sigma_in, hii) = acc in 
        let path = ((u, i), x)::path in 
        let (d, hii) = renumber_sigmas_only path hii d in 
        let sigma_out = (x, d)::sigma_in in 
        (sigma_out, hii)
      )
      sigma
      ([], hii)
  
  let renumber
    (path : InstancePath.t) (hii : HoleInstanceInfo.t) (d : DHExp.t) 
    : (t * HoleInstanceInfo.t) = 
    let (d, hii) = renumber_result_only path hii d in
    renumber_sigmas_only path hii d
end

module Evaluator = struct
  type result = 
  | InvalidInput of nat 
  | BoxedValue of DHExp.t 
  | Indet of DHExp.t

  (* 
    0 = out of fuel
    1 = free or invalid variable
    2 = ap invalid boxed function val
    3 = boxed value not a number literal 2
    4 = boxed value not a number literal 1
    5 = bad pattern match
    6 = Cast BV Hole Ground
  *)

  type ground_cases = 
  | Hole
  | Ground 
  | NotGroundOrHole of HTyp.t (* the argument is the corresponding ground type *)

  let grounded_Arrow = NotGroundOrHole(HTyp.Arrow(HTyp.Hole, HTyp.Hole))
  let grounded_Sum = NotGroundOrHole(HTyp.Sum(HTyp.Hole, HTyp.Hole))
  let grounded_Prod = NotGroundOrHole(HTyp.Prod(HTyp.Hole, HTyp.Hole))
  let grounded_List = NotGroundOrHole(HTyp.List HTyp.Hole)

  let ground_cases_of ty = 
    begin match ty with 
    | HTyp.Hole -> Hole
    | HTyp.Bool
    | HTyp.Num
    | HTyp.Unit
    | HTyp.Arrow(HTyp.Hole, HTyp.Hole)
    | HTyp.Sum(HTyp.Hole, HTyp.Hole)
    | HTyp.Prod(HTyp.Hole, HTyp.Hole)
    | HTyp.List(HTyp.Hole) -> Ground
    | HTyp.Arrow(_, _) -> grounded_Arrow
    | HTyp.Sum(_, _) -> grounded_Sum
    | HTyp.Prod(_, _) -> grounded_Prod
    | HTyp.List _ -> grounded_List
    end

  let eval_bin_num_op op n1 n2 = 
    begin match op with 
    | DHExp.Plus -> DHExp.NumLit (n1 + n2)
    | DHExp.Times -> DHExp.NumLit (n1 * n2)
    | DHExp.LessThan -> DHExp.BoolLit (n1 < n2)
    end

  let rec evaluate 
    (d : DHExp.t) 
    : result = 
      begin match d with 
      | DHExp.BoundVar _ -> InvalidInput 1
      | DHExp.Let(dp, d1, d2) ->
        begin match evaluate d1 with 
        | InvalidInput msg -> InvalidInput msg
        | BoxedValue d1 | Indet d1 ->
          begin match DHExp.matches dp d1 with
          | DHExp.Indet -> Indet d
          | DHExp.DoesNotMatch -> Indet d
          | DHExp.Matches env -> evaluate (DHExp.subst env d2)
          end
        end
      | DHExp.FixF(x, ty, d1) -> 
        evaluate (DHExp.subst_var d x d1)
      | DHExp.Lam(_, _, _) ->
        BoxedValue d
      | DHExp.Ap(d1, d2) -> 
        begin match evaluate d1 with 
        | InvalidInput msg -> InvalidInput msg
        | BoxedValue DHExp.Lam(dp, tau, d3) ->
          begin match evaluate d2 with
          | InvalidInput msg -> InvalidInput msg
          | BoxedValue d2 | Indet d2 ->
            begin match DHExp.matches dp d2 with
            | DHExp.DoesNotMatch -> Indet d
            | DHExp.Indet -> Indet d
            | DHExp.Matches env ->
              (* beta rule *)
              evaluate (DHExp.subst env d3)
            end
          end
        | BoxedValue (DHExp.Cast(d1', HTyp.Arrow(ty1, ty2), HTyp.Arrow(ty1', ty2')))
        | Indet (DHExp.Cast(d1', HTyp.Arrow(ty1, ty2), HTyp.Arrow(ty1', ty2'))) -> 
          begin match evaluate d2 with 
          | InvalidInput msg -> InvalidInput msg
          | BoxedValue d2' | Indet d2' -> 
            (* ap cast rule *)
            evaluate  
              (DHExp.Cast(
                DHExp.Ap(
                  d1', 
                  DHExp.Cast
                    (d2', ty1', ty1)),
                ty2, ty2'))
          end
        | BoxedValue _ -> InvalidInput 2
        | Indet d1' -> 
          begin match evaluate d2 with 
          | InvalidInput msg -> InvalidInput msg
          | BoxedValue d2' | Indet d2' -> 
            Indet (DHExp.Ap(d1', d2'))
          end
        end
      | DHExp.ListNil
      | DHExp.BoolLit _
      | DHExp.NumLit _
      | DHExp.Triv -> BoxedValue d
      | DHExp.BinNumOp(op, d1, d2) -> 
        begin match evaluate d1 with 
        | InvalidInput msg -> InvalidInput msg
        | BoxedValue (DHExp.NumLit n1 as d1')  -> 
          begin match evaluate d2 with 
          | InvalidInput msg -> InvalidInput msg
          | BoxedValue (DHExp.NumLit n2) -> 
            BoxedValue (eval_bin_num_op op n1 n2)
          | BoxedValue _ -> InvalidInput 3 
          | Indet d2' -> 
            Indet (DHExp.BinNumOp(op, d1', d2'))
          end
        | BoxedValue _ -> InvalidInput 4
        | Indet d1' -> 
          begin match evaluate d2 with 
          | InvalidInput msg -> InvalidInput msg
          | BoxedValue d2' | Indet d2' -> 
            Indet (DHExp.BinNumOp(op, d1', d2'))
          end
        end
      | DHExp.Inj(ty, side, d1) -> 
        begin match evaluate d1 with 
        | InvalidInput msg -> InvalidInput msg
        | BoxedValue d1' -> BoxedValue (DHExp.Inj(ty, side, d1'))
        | Indet d1' -> Indet (DHExp.Inj(ty, side, d1'))
        end
      | DHExp.Pair(d1, d2) ->
        begin match (evaluate d1,
               evaluate d2) with
        | (InvalidInput msg, _)
        | (_, InvalidInput msg) -> InvalidInput msg
        | (Indet d1, Indet d2)
        | (Indet d1, BoxedValue d2)
        | (BoxedValue d1, Indet d2) -> Indet (DHExp.Pair(d1, d2))
        | (BoxedValue d1, BoxedValue d2) -> BoxedValue (DHExp.Pair(d1, d2))
        end
      | DHExp.Cons(d1, d2) ->
        begin match (evaluate d1,
               evaluate d2) with
        | (InvalidInput msg, _)
        | (_, InvalidInput msg) -> InvalidInput msg
        | (Indet d1, Indet d2)
        | (Indet d1, BoxedValue d2)
        | (BoxedValue d1, Indet d2) -> Indet (DHExp.Cons(d1, d2))
        | (BoxedValue d1, BoxedValue d2) -> BoxedValue (DHExp.Cons(d1, d2))
        end
      | DHExp.Case(d1, rules, n) -> evaluate_case d1 rules n
      | DHExp.EmptyHole(u, i, sigma) -> 
        Indet d  
      | DHExp.NonEmptyHole(reason, u, i, sigma, d1) -> 
        begin match evaluate d1 with 
        | InvalidInput msg -> InvalidInput msg
        | BoxedValue d1' | Indet d1' -> 
          Indet (DHExp.NonEmptyHole(reason, u, i, sigma, d1'))
        end
      | DHExp.FreeVar(u, i, sigma, x) -> 
        Indet d
      | DHExp.Cast(d1, ty, ty') -> 
        begin match evaluate d1 with 
        | InvalidInput msg -> InvalidInput msg
        | (BoxedValue d1' as result) -> 
          begin match (ground_cases_of ty, ground_cases_of ty') with 
          | (Hole, Hole) -> result
          | (Ground, Ground) ->  
            (* if two types are ground and consistent, then they are eq *)
            result
          | (Ground, Hole) -> 
            (* can't remove the cast or do anything else here, so we're done *)
            BoxedValue (DHExp.Cast(d1', ty, ty'))
          | (Hole, Ground) -> 
            (* by canonical forms, d1' must be of the form d<ty'' -> ?> *)
            begin match d1' with 
            | DHExp.Cast(d1'', ty'', HTyp.Hole) -> 
              if HTyp.eq ty'' ty' then BoxedValue d1''
              else Indet (DHExp.FailedCast(d1', ty, ty'))
            | _ -> InvalidInput 6
            end
          | (Hole, NotGroundOrHole ty'_grounded) -> 
            (* ITExpand rule *)
            let d' = 
              DHExp.Cast
                (DHExp.Cast(d1', ty, ty'_grounded),
                ty'_grounded, ty') in 
            evaluate d'
          | (NotGroundOrHole ty_grounded, Hole) -> 
            (* ITGround rule *)
             let d' = 
               DHExp.Cast
                 (DHExp.Cast(d1', ty, ty_grounded),
                 ty_grounded, ty') in 
             evaluate d'
          | (Ground, NotGroundOrHole _)  
          | (NotGroundOrHole _, Ground) -> 
            (* can't do anything when casting between diseq, non-hole types *)
            BoxedValue (DHExp.Cast(d1', ty, ty'))
          | (NotGroundOrHole _, NotGroundOrHole _) -> 
            (* they might be eq in this case, so remove cast if so *)
            if HTyp.eq ty ty' then result 
            else BoxedValue (DHExp.Cast(d1', ty, ty'))
          end
        | (Indet d1' as result) -> 
          begin match (ground_cases_of ty, ground_cases_of ty') with 
          | (Hole, Hole) -> result
          | (Ground, Ground) ->  
            (* if two types are ground and consistent, then they are eq *)
            result
          | (Ground, Hole) -> 
            (* can't remove the cast or do anything else here, so we're done *)
            Indet (DHExp.Cast(d1', ty, ty'))
          | (Hole, Ground) -> 
            begin match d1' with 
            | DHExp.Cast(d1'', ty'', HTyp.Hole) -> 
              if HTyp.eq ty'' ty' then Indet d1''
              else Indet (DHExp.FailedCast(d1', ty, ty'))
            | _ -> 
              Indet (DHExp.Cast(d1', ty, ty'))
            end
          | (Hole, NotGroundOrHole ty'_grounded) -> 
            (* ITExpand rule *)
            let d' = 
              DHExp.Cast
                (DHExp.Cast(d1', ty, ty'_grounded), 
                ty'_grounded, ty') in 
            evaluate d'
          | (NotGroundOrHole ty_grounded, Hole) -> 
            (* ITGround rule *)
             let d' = 
               DHExp.Cast
                 (DHExp.Cast(d1', ty, ty_grounded),
                 ty_grounded, ty') in 
             evaluate d'
          | (Ground, NotGroundOrHole _)  
          | (NotGroundOrHole _, Ground) -> 
            (* can't do anything when casting between diseq, non-hole types *)
            Indet (DHExp.Cast(d1', ty, ty'))
          | (NotGroundOrHole _, NotGroundOrHole _) -> 
            (* it might be eq in this case, so remove cast if so *)
            if HTyp.eq ty ty' then result else Indet (DHExp.Cast(d1', ty, ty'))
          end
        end
      | DHExp.FailedCast(d1, ty, ty') -> 
        begin match evaluate d1 with 
        | InvalidInput msg -> InvalidInput msg
        | BoxedValue d1' | Indet d1' -> 
          Indet (DHExp.FailedCast(d1', ty, ty'))
        end
      end
  and evaluate_case
    (scrut : DHExp.t)
    (rules : DHExp.rule list)
    (current_rule_index : nat)
    : result =
    begin match evaluate scrut with
    | InvalidInput msg -> InvalidInput msg
    | BoxedValue scrut | Indet scrut ->
      begin match List.nth_opt rules current_rule_index with
      | None -> Indet (DHExp.Case(scrut, rules, current_rule_index))
      | Some (DHExp.Rule(dp, d)) ->
        begin match DHExp.matches dp scrut with
        | DHExp.Indet -> Indet (DHExp.Case(scrut, rules, current_rule_index))
        | DHExp.Matches env -> evaluate (DHExp.subst env d)
        | DHExp.DoesNotMatch -> evaluate_case scrut rules (current_rule_index+1)
        end
      end
    end
 end
