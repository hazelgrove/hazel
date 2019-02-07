open SemanticsCommon
open Util

type op =
| Plus
| Times
| LessThan
| Space
| Comma
| Cons

let is_Space = function
| Space -> true
| _ -> false

type skel_t = op Skel.t

module UHExp = struct
  type t =
  | Tm of err_status * t'
  | Parenthesized of t
  and t' =
  | Asc of t * UHTyp.t
  | Var of var_err_status * Var.t
  | Let of UHPat.t * UHTyp.t option * t * t
  | Lam of UHPat.t * UHTyp.t option * t
  | NumLit of int
  | BoolLit of bool
  | Inj of inj_side * t
  | Case of t * rule list
  | ListNil
  (* | ListCons : list(t) -> t' *)
  | EmptyHole of MetaVar.t
  | OpSeq of skel_t * (t, op) OperatorSeq.opseq (* invariant: skeleton is consistent with opseq *)
  | ApPalette of PaletteName.t * PaletteSerializedModel.t
    * (int * (HTyp.t * t) Util.NatMap.t) (* = PaletteHoleData.t *)
  and rule =
  | Rule of UHPat.t * t
end
include UHExp

type rules = rule list

let rec get_tuple skel1 skel2 =
  match skel2 with
  | Skel.BinOp (_, Comma, skel21, skel22) -> skel1::(get_tuple skel21 skel22)
  | Skel.BinOp (_, _, _, _)
  | Skel.Placeholder _ -> [skel1; skel2]

let rec make_tuple err = function
| [skel1; skel2] ->
  Some (Skel.BinOp (err, Comma, skel1, skel2))
| skel1::skels ->
  (match make_tuple NotInHole skels with
  | None -> None
  | Some skel2 -> Some (Skel.BinOp (err, Comma, skel1, skel2)))
| [] -> None

(* helper function for constructing a new empty hole *)
let new_EmptyHole u_gen =
  let u',u_gen' = MetaVarGen.next u_gen in
  (Tm (NotInHole, (EmptyHole u'))),u_gen'

let is_EmptyHole = function
| Tm (_, EmptyHole _) -> true
| _ -> false

let empty_rule u_gen =
  let rule_p,u_gen = UHPat.new_EmptyHole u_gen in
  let rule_e,u_gen = new_EmptyHole u_gen in
  let rule = Rule (rule_p, rule_e) in
  rule,u_gen

module PaletteHoleData =
 struct
  type hole_ref_lbl = int
  type hole_map = (HTyp.t * t) NatMap.t
  type t = hole_ref_lbl * hole_map
  let empty = 0,NatMap.empty
  let mk_hole_ref_var_name lbl =
    "__hole_ref__" ^ (Helper.Helper.string_of_nat lbl) ^ "__"
  let next_ref_lbl x = 1 + x
  let new_hole_ref u_gen hd ty =
    let cur_ref_lbl,cur_map = hd in
    let next_ref_lbl = next_ref_lbl cur_ref_lbl in
    let initial_exp,u_gen = new_EmptyHole u_gen in
    let next_map = NatMap.extend cur_map (cur_ref_lbl,(ty,initial_exp)) in
    (cur_ref_lbl,(next_ref_lbl,next_map)),u_gen
  let extend_ctx_with_hole_map ctx hm =
    let gamma,palette_ctx = ctx in
    let gamma' =
      NatMap.fold hm (fun gamma1 hole_mapping ->
        let id,v = hole_mapping in
        let htyp,_ = v in
        let var_name = mk_hole_ref_var_name id in
        VarCtx.extend gamma1 (var_name,htyp)) gamma
    in
    gamma',palette_ctx
 end

module HoleRefs =
 struct
  type hole_ref = PaletteHoleData.hole_ref_lbl * HTyp.t
  let lbl_of = function
  | lbl,_ -> lbl
  let type_of = function
  | _,ty -> ty

  (* cant define m_hole_ref using Inductive due to Coq limitation *)
  type 'x m_hole_ref' =
  | NewHoleRef of HTyp.t
  | Bnd of { args: 'a.'a m_hole_ref' * ('a -> 'x m_hole_ref') }
  | Ret of 'x
  type 'x m_hole_ref = 'x m_hole_ref'
  let new_hole_ref x = NewHoleRef x
  (*let bind : ( 'a . 'a m_hole_ref' * ('a -> 'x m_hole_ref') ) -> 'x m_hole_ref' = Bnd *)
  (*let ret = Ret *)

  (* TODO: restructure m_hole_ref type to avoid use of Obj.magic below *)
  let rec exec mhr phd u_gen =
    match mhr with
    | NewHoleRef ty ->
      let q,u_gen' = PaletteHoleData.new_hole_ref u_gen phd ty in
      let lbl,phd' = q in ((Obj.magic (lbl,ty)),phd'),u_gen'
    | Bnd { args=(mhra, f) } ->
      let q,u_gen' = exec (Obj.magic mhra) phd u_gen in
      let x,phd' = q in let mhrb = Obj.magic f x in exec mhrb phd' u_gen'
    | Ret x -> (x,phd),u_gen
 end

module PaletteDefinition =
 struct
  type t = { expansion_ty : HTyp.t;
             initial_model : PaletteSerializedModel.t HoleRefs.m_hole_ref;
             to_exp : (PaletteSerializedModel.t -> UHExp.t) }
  let expansion_ty t0 =
    t0.expansion_ty
  let initial_model t0 =
    t0.initial_model
  let to_exp t0 =
    t0.to_exp
 end

module PaletteCtx =
 struct
  type t = PaletteDefinition.t VarMap.t_
  include VarMap
 end

module Contexts =
 struct
  type t = VarCtx.t * PaletteCtx.t
  let gamma = function
  | gamma,_ -> gamma
  let extend_gamma contexts binding =
    let gamma,palette_ctx = contexts in
    let gamma' = VarCtx.extend gamma binding in
    gamma',palette_ctx
  let gamma_union contexts gamma' =
    let gamma,palette_ctx = contexts in
    let gamma'' = VarCtx.union gamma gamma' in
    gamma'',palette_ctx
  let gamma_contains contexts x =
    VarCtx.contains (gamma contexts) x
 end

type opseq = (t, op) OperatorSeq.opseq

(* bidelimited expressions are those that don't have
 * sub-expressions at their outer left or right edge
 * in the concrete syntax *)
let bidelimited = function
(* bidelimited cases *)
| Tm (_, (EmptyHole _))
| Tm (_, (Var (_, _)))
| Tm (_, (NumLit _))
| Tm (_, (BoolLit _))
| Tm (_, (Inj (_, _)))
| Tm (_, (Case (_, _)))
| Tm (_, ListNil)
(* | Tm _ (ListLit _) *)
| Tm (_, (ApPalette (_, _, _)))
| Parenthesized _ -> true
(* non-bidelimited cases *)
| Tm (_, (Asc (_, _)))
| Tm (_, (Let (_, _, _, _)))
| Tm (_, (Lam (_, _, _)))
| Tm (_, (OpSeq (_, _))) -> false

(* if e is not bidelimited, bidelimit e parenthesizes it *)
let bidelimit e =
  if bidelimited e then e else Parenthesized e

(* put e in the specified hole *)
let rec set_inconsistent u = function
| Tm (_, e') -> Tm ((InHole (TypeInconsistent, u)), e')
| Parenthesized e' -> Parenthesized (set_inconsistent u e')

(* put e in a new hole, if it is not already in a hole *)
let rec make_inconsistent u_gen e = match e with
| Tm (NotInHole, e')
| Tm ((InHole (WrongLength, _)), e') ->
  let u, u_gen = MetaVarGen.next u_gen in
  Tm ((InHole (TypeInconsistent, u)) e'), u_gen
| Tm ((InHole (TypeInconsistent, _)), _) -> (e, u_gen)
| Parenthesized e1 ->
  begin match make_inconsistent u_gen e1 with
  | (e1', u_gen') -> (Parenthesized e1', u_gen')
  end

(* put skel in a new hole, if it is not already in a hole *)
let make_skel_inconsistent u_gen skel seq =
  match skel with
  | Skel.Placeholder n ->
    begin match OperatorSeq.seq_nth n seq with
    | Some en ->
      let (en', u_gen') = make_inconsistent u_gen en in
      begin match OperatorSeq.seq_update_nth n seq en' with
      | Some seq' -> Some (skel, seq', u_gen')
      | None -> None
      end
    | None -> None
    end
  | Skel.BinOp ((InHole (TypeInconsistent, _)), _, _, _) -> Some (skel, seq, u_gen)
  | Skel.BinOp (NotInHole, op, skel1, skel2)
  | Skel.BinOp ((InHole (WrongLength, _)), op, skel1, skel2) ->
    let (u', u_gen') = MetaVarGen.next u_gen in
    Some (Skel.BinOp ((InHole (TypeInconsistent, u')), op, skel1, skel2), seq, u_gen')

let rec drop_outer_parentheses e = match e with
| Tm (_, _) -> e
| Parenthesized e' -> drop_outer_parentheses e'

(* see syn_skel and ana_skel below *)
type type_mode =
| AnalyzedAgainst of HTyp.t
| Synthesized of HTyp.t

let combine_modes mode1 mode2 =
  match (mode1, mode2) with
  | (Some _, _) -> mode1
  | (_, Some _) -> mode2
  | (None, None) -> None

let rec syn_pat ctx p =
  match p with
  | UHPat.Pat ((InHole (TypeInconsistent, _)), p')
  | UHPat.Pat ((InHole (WrongLength, _)),
      ((UHPat.OpSeq ((Skel.BinOp ((InHole (WrongLength, _)), UHPat.Comma, _, _)), _)) as p')) ->
    begin match syn_pat' ctx p' with
    | None -> None
    | Some (_, gamma) -> Some (HTyp.Hole, gamma)
    end
  | UHPat.Pat ((InHole (WrongLength, _)), _) -> None
  | UHPat.Pat (NotInHole, p') ->
    syn_pat' ctx p'
  | UHPat.Parenthesized p ->
    syn_pat ctx p
and syn_pat' ctx p =
  match p with
  | UHPat.EmptyHole _ -> Some (HTyp.Hole, ctx)
  | UHPat.Wild -> Some (HTyp.Hole, ctx)
  | UHPat.Var x ->
    Var.check_valid x (Some
      (HTyp.Hole,(Contexts.extend_gamma ctx (x,HTyp.Hole))))
  | UHPat.NumLit _ -> Some (HTyp.Num,ctx)
  | UHPat.BoolLit _ -> Some (HTyp.Bool,ctx)
  | UHPat.Inj (side, p1) ->
    (match syn_pat ctx p1 with
     | Some (ty1, ctx) ->
       let ty =
         (match side with
         | L -> HTyp.Sum (ty1, HTyp.Hole)
         | R -> HTyp.Sum (HTyp.Hole, ty1))
       in
       Some (ty,ctx)
     | None -> None)
  | UHPat.ListNil -> Some ((HTyp.List HTyp.Hole),ctx)
  (* | UHPat.ListLit ps ->
    List.fold_left (fun opt_result elt ->
      match opt_result with
      | None -> None
      | Some (ty, ctx) ->
        match syn_pat ctx elt with
        | None -> None
        | Some (ty_elt, ctx) ->
          match HTyp.join ty ty_elt with
          | Some ty -> Some (ty, ctx)
          | None -> None
          end
        end
      end) ps (Some (HTyp.Hole, ctx)) *)
  | UHPat.OpSeq (skel, seq) ->
    (match syn_skel_pat ctx skel seq None with
     | Some (ty, ctx, _) -> Some (ty, ctx)
     | None -> None)
and syn_skel_pat ctx skel seq monitor =
  match skel with
  | Skel.Placeholder n ->
    begin match OperatorSeq.seq_nth n seq with
    | None -> None
    | Some pn ->
      begin match UHPat.bidelimited pn with
      | false -> None
      | true ->
        begin match syn_pat ctx pn with
        | None -> None
        | Some (ty, ctx) ->
          let mode =
            begin match monitor with
            | None -> None
            | Some n' ->
              if Nat.eqb n n'
              then Some (Synthesized ty)
              else None
            end in
          Some (ty, ctx, mode)
        end
      end
    end
  | Skel.BinOp ((InHole (TypeInconsistent, u)), op, skel1, skel2)
  | Skel.BinOp ((InHole (WrongLength, u)), (UHPat.Comma as op), skel1, skel2) ->
    let skel_not_in_hole = Skel.BinOp (NotInHole, op, skel1, skel2) in
    begin match syn_skel_pat ctx skel_not_in_hole seq monitor with
    | None -> None
    | Some (_, ctx, mode) -> Some (HTyp.Hole, ctx, mode)
    end
  | Skel.BinOp ((InHole (WrongLength, u)), _, _, _) -> None
  | Skel.BinOp (NotInHole, UHPat.Comma, skel1, skel2) ->
    begin match syn_skel_pat ctx skel1 seq monitor with
    | None -> None
    | Some (ty1, ctx, mode1) ->
      begin match syn_skel_pat ctx skel2 seq monitor with
      | None -> None
      | Some (ty2, ctx, mode2) ->
        let ty = HTyp.Prod (ty1, ty2) in
        let mode = combine_modes mode1 mode2 in
        Some (ty, ctx, mode)
      end
    end
  | Skel.BinOp (NotInHole, UHPat.Space, skel1, skel2) ->
    begin match syn_skel_pat ctx skel1 seq monitor with
    | None -> None
    | Some (ty1, ctx, mode1) ->
      begin match syn_skel_pat ctx skel2 seq monitor with
      | None -> None
      | Some (ty2, ctx, mode2) ->
        let ty = HTyp.Hole in
        let mode = combine_modes mode1 mode2 in
        Some (ty, ctx, mode)
      end
    end
  | Skel.BinOp (NotInHole, UHPat.Cons, skel1, skel2) ->
    begin match syn_skel_pat ctx skel1 seq monitor with
    | None -> None
    | Some (ty1, ctx, mode1) ->
      let ty = HTyp.List ty1 in
      begin match ana_skel_pat ctx skel2 seq ty monitor with
      | None -> None
      | Some (ctx, mode2) ->
        let mode = combine_modes mode1 mode2 in
        Some (ty, ctx, mode)
      end
    end
and ana_pat ctx p ty =
  match p with
  | UHPat.Pat ((InHole (TypeInconsistent, _)), p') ->
    begin match syn_pat' ctx p' with
    | None -> None
    | Some (_, ctx) -> Some ctx
    end
  | UHPat.Pat (
      (InHole (WrongLength, _)),
      ((UHPat.OpSeq ((Skel.BinOp ((InHole WrongLength _), UHPat.Comma, (_, _))), _)) as p'))
  | UHPat.Pat (NotInHole, p') ->
    ana_pat' ctx p' ty
  | UHPat.Pat ((InHole (WrongLength, _)), _) -> None
  | UHPat.Parenthesized p ->
    ana_pat ctx p ty
and ana_pat' ctx p ty =
  match p with
  | UHPat.Var x ->
    Var.check_valid x (Some (Contexts.extend_gamma ctx (x,ty)))
  | UHPat.EmptyHole _
  | UHPat.Wild -> Some ctx
  | UHPat.NumLit _
  | UHPat.BoolLit _ ->
    (match syn_pat' ctx p with
     | None -> None
     | Some p ->
       let ty',ctx1 = p in
       if HTyp.consistent ty ty' then Some ctx1 else None)
  | UHPat.Inj (side, p1) ->
    (match HTyp.matched_sum ty with
     | None -> None
     | Some (tyL, tyR) ->
       let ty1 = pick_side side tyL tyR in
       ana_pat ctx p1 ty1)
  | UHPat.ListNil ->
    (match HTyp.matched_list ty with
     | Some _ -> Some ctx
     | None -> None)
 (* | UHPat.ListLit ps ->
   match HTyp.matched_list ty with
   | None -> None
   | Some ty_elts ->
     List.fold_left (fun optctx p ->
       match optctx with
       | None -> None
       | Some ctx -> ana_pat ctx p ty_elts
       end) ps (Some ctx)
   end *)
  | UHPat.OpSeq (skel, seq) ->
    (match ana_skel_pat ctx skel seq ty None with
     | Some (ctx, _) -> Some ctx
     | None -> None)
and ana_skel_pat ctx skel seq ty monitor =
  match skel with
  | Skel.Placeholder n ->
    begin match OperatorSeq.seq_nth n seq with
    | None -> None
    | Some pn ->
      begin match UHPat.bidelimited pn with
      | false -> None
      | true ->
        begin match ana_pat ctx pn ty with
        | None -> None
        | Some ctx ->
          let mode =
            begin match monitor with
            | None -> None
            | Some n' ->
              if n = n'
              then Some (AnalyzedAgainst ty)
              else None
            end in
          Some (ctx, mode)
        end
      end
    end
  | Skel.BinOp ((InHole (TypeInconsistent, u)), op, skel1, skel2) ->
    let skel_not_in_hole = Skel.BinOp (NotInHole, op, skel1, skel2) in
    begin match syn_skel_pat ctx skel_not_in_hole seq monitor with
    | None -> None
    | Some (_, ctx, mode) -> Some (ctx, mode)
    end
  | Skel.BinOp (NotInHole, UHPat.Comma, skel1, skel2) ->
    begin match ty with
    | HTyp.Hole ->
      begin match ana_skel_pat ctx skel1 seq HTyp.Hole monitor with
      | None -> None
      | Some (ctx, mode1) ->
        begin match ana_skel_pat ctx skel2 seq HTyp.Hole monitor with
        | None -> None
        | Some (ctx, mode2) ->
          let mode = combine_modes mode1 mode2 in
          Some (ctx, mode)
        end
      end
    | HTyp.Prod (ty1, ty2) ->
      let types = HTyp.get_tuple ty1 ty2 in
      let skels = UHPat.get_tuple skel1 skel2 in
      begin match Util.zip_eq skels types with
      | None -> None
      | Some zipped ->
        List.fold_left (fun opt_result (skel_ty : UHPat.skel_t * HTyp.t) ->
          begin match opt_result with
          | None -> None
          | Some (ctx, mode) ->
            let (skel, ty) = skel_ty in
            begin match ana_skel_pat ctx skel seq ty monitor with
            | None -> None
            | Some (ctx, mode') ->
              let mode = combine_modes mode mode' in
              Some (ctx, mode)
            end
          end) (Some (ctx, None)) zipped
      end
    | _ -> None
    end
  | Skel.BinOp ((InHole (WrongLength, u)), (UHPat.Comma as op), skel1, skel2) ->
    begin match ty with
    | HTyp.Prod (ty1, ty2) ->
      let types = HTyp.get_tuple ty1 ty2 in
      let skels = UHPat.get_tuple skel1 skel2 in
      let n_types = List.length types in
      let n_skels = List.length skels in
      begin match n_types = n_skels with
      | true -> None (* make sure the lengths are actually different *)
      | false ->
        let (zipped, remainder) = HTyp.zip_with_skels skels types in
        let ana_zipped : (Contexts.t * type_mode option) option =
          List.fold_left (fun opt_result (skel_ty : UHPat.skel_t * HTyp.t) ->
            begin match opt_result with
            | None -> None
            | Some (ctx, mode) ->
              let (skel, ty) = skel_ty in
              begin match ana_skel_pat ctx skel seq ty monitor with
              | None -> None
              | Some (ctx, mode') ->
                let mode = combine_modes mode mode' in
                Some (ctx, mode)
              end
            end) zipped (Some (ctx, None)) in
        begin match ana_zipped with
        | None -> None
        | Some (ctx, mode) ->
          List.fold_left (fun opt_result skel ->
            begin match opt_result with
            | None -> None
            | Some (ctx, mode) ->
              begin match syn_skel_pat ctx skel seq monitor with
              | None -> None
              | Some (_, ctx, mode') ->
                let mode = combine_modes mode mode' in
                Some (ctx, mode)
              end
            end) (Some (ctx, mode)) remainder
        end
      end
    | _ -> None
    end
  | Skel.BinOp ((InHole (WrongLength, _)), _, _, _) -> None
  | Skel.BinOp (NotInHole, UHPat.Space, skel1, skel2) ->
    None
  | Skel.BinOp (NotInHole, UHPat.Cons, skel1, skel2) ->
    begin match HTyp.matched_list ty with
    | None -> None
    | Some ty_elt ->
      begin match ana_skel_pat ctx skel1 seq ty_elt monitor with
      | None -> None
      | Some (ctx, mode1) ->
        let ty_list = HTyp.List ty_elt in
        begin match ana_skel_pat ctx skel2 seq ty_list monitor with
        | None -> None
        | Some (ctx, mode2) ->
          let mode = combine_modes mode1 mode2 in
          Some (ctx, mode)
        end
      end
    end

let ctx_for_let ctx p ty1 e1 =
  match (p, e1) with
  | (UHPat.Pat (_, (UHPat.Var x)),
     Tm (_, (Lam (_, _, _)))) ->
    begin match HTyp.matched_arrow ty1 with
    | Some _ -> Contexts.extend_gamma ctx (x, ty1)
    | None -> ctx
    end
  | _ -> ctx

(* returns recursive ctx + name of recursively defined var *)
let ctx_for_let' ctx p ty1 e1 =
  match (p, e1) with
  | (UHPat.Pat (_, (UHPat.Var x)),
     Tm (_, (Lam (_, _, _)))) ->
    begin match HTyp.matched_arrow ty1 with
    | Some _ -> (Contexts.extend_gamma ctx (x, ty1), Some x)
    | None -> (ctx, None)
    end
  | _ -> (ctx, None)

(* synthesize a type, if possible, for e *)
let rec syn ctx e =
  match e with
  | Tm ((InHole (TypeInconsistent, _)), e')
  | Tm ((InHole (WrongLength, _)), ((UHExp.OpSeq ((Skel.BinOp ((InHole (WrongLength, _)), UHExp.Comma, _, _)), _)) as e')) ->
    begin match syn' ctx e' with
    | Some _ -> Some HTyp.Hole
    | None -> None
    end
  | Tm ((InHole (WrongLength, _)), _) -> None
  | Tm (NotInHole, e') -> syn' ctx e'
  | Parenthesized e1 -> syn ctx e1
and syn' ctx e =
  match e with
  | EmptyHole _ -> Some HTyp.Hole
  | Asc (e1, uty) ->
    let ty = UHTyp.expand uty in
    if bidelimited e1
    then (match ana ctx e1 ty with
          | Some _ -> Some ty
          | None -> None)
    else None
  | Var (NotInVHole, x) ->
    let gamma,_ = ctx in VarMap.lookup gamma x
  | Var (NotInVHole, x) ->
    Some HTyp.Hole
  | Lam (p, ann, e1) ->
    let ty1 =
      match ann with
      | Some uty -> UHTyp.expand uty
      | None -> HTyp.Hole
    in
    (match ana_pat ctx p ty1 with
     | None -> None
     | Some ctx1 ->
       (match syn ctx1 e1 with
        | None -> None
        | Some ty2 -> Some (HTyp.Arrow (ty1, ty2))))
  | Inj (side, e1) ->
    (match syn ctx e1 with
     | None -> None
     | Some ty1 ->
       (match side with
        | L -> Some (HTyp.Sum (ty1, HTyp.Hole))
        | R -> Some (HTyp.Sum (HTyp.Hole, ty1))))
  | Let (p, ann, e1, e2) ->
    (match ann with
     | Some uty1 ->
       let ty1 = UHTyp.expand uty1 in
       let ctx1 = ctx_for_let ctx p ty1 e1 in
       (match ana ctx1 e1 ty1 with
        | None -> None
        | Some _ ->
          (match ana_pat ctx p ty1 with
           | None -> None
           | Some ctx2 -> syn ctx2 e2))
     | None ->
       (match syn ctx e1 with
        | None -> None
        | Some ty1 ->
          (match ana_pat ctx p ty1 with
           | None -> None
           | Some ctx2 -> syn ctx2 e2)))
  | NumLit _ -> Some HTyp.Num
  | BoolLit _ -> Some HTyp.Bool
  | ListNil -> Some (HTyp.List HTyp.Hole)
  (* | ListLit es ->
    List.fold_left (fun opt_result elt ->
      match opt_result with
      | None -> None
      | Some ty ->
        match syn ctx elt with
        | None -> None
        | Some ty_elt -> HTyp.join ty ty_elt
        end
      end) es (Some HTyp.Hole) *)
  | OpSeq (skel, seq) ->
    (* NOTE: doesn't check if skel is the correct parse of seq!!! *)
    (match syn_skel ctx skel seq None with
     | Some (ty, _) -> Some ty
     | None -> None)
  | Case (_, _) -> None
  | ApPalette (name, serialized_model, hole_data) ->
    let _,palette_ctx = ctx in
    (match VarMap.lookup palette_ctx name with
     | None -> None
     | Some palette_defn ->
       (match ana_hole_data ctx hole_data with
        | None -> None
        | Some _ ->
          let expansion_ty = PaletteDefinition.expansion_ty palette_defn in
          let to_exp = PaletteDefinition.to_exp palette_defn in
          let expansion = to_exp serialized_model in
          let _,hole_map = hole_data in
          let expansion_ctx =
            PaletteHoleData.extend_ctx_with_hole_map ctx hole_map
          in
          (match ana expansion_ctx expansion expansion_ty with
           | None -> None
           | Some _ -> Some expansion_ty)))
and ana_hole_data ctx hole_data =
  let _,hole_map = hole_data in
  NatMap.fold hole_map (fun c v ->
    let _,ty_e = v in
    let ty,e = ty_e in
    (match c with
     | None -> None
     | Some _ -> ana ctx e ty)) (Some ())
and ana ctx e ty =
  match e with
  | Tm ((InHole (TypeInconsistent, _)), e') ->
    begin match syn' ctx e' with
    | None -> None
    | Some _ -> Some tt (* this is a consequence of subsumption and hole universality *)
    end
  | Tm (
      (InHole (WrongLength, _)),
      ((UHExp.OpSeq ((Skel.BinOp ((InHole (WrongLength, _)), UHExp.Comma, _, _)), _)) as e'))
  | Tm (NotInHole, e') ->
    ana' ctx e' ty
  | Tm ((InHole (WrongLength, _)), _) -> None
  | Parenthesized e1 -> ana ctx e1 ty
and ana' ctx e ty =
  match e with
  | Let (p, ann, e1, e2) ->
    (match ann with
     | Some uty1 ->
       let ty1 = UHTyp.expand uty1 in
       let ctx1 = ctx_for_let ctx p ty1 e1 in
       (match ana ctx1 e1 ty1 with
        | None -> None
        | Some _ ->
          (match ana_pat ctx p ty1 with
           | None -> None
           | Some ctx2 -> ana ctx2 e2 ty))
     | None ->
       (match syn ctx e1 with
        | None -> None
        | Some ty1 ->
          (match ana_pat ctx p ty1 with
           | None -> None
           | Some ctx2 -> ana ctx2 e2 ty)))
  | Lam (p, ann, e1) ->
    (match HTyp.matched_arrow ty with
     | None -> None
     | Some (ty1_given,ty2) ->
       (match ann with
        | Some uty1 ->
          let ty1_ann = UHTyp.expand uty1 in
          (match HTyp.consistent ty1_ann ty1_given with
           | false -> None
           | true ->
             (match ana_pat ctx p ty1_ann with
              | None -> None
              | Some ctx1 -> ana ctx1 e1 ty2))
        | None ->
          (match ana_pat ctx p ty1_given with
           | None -> None
           | Some ctx1 -> ana ctx1 e1 ty2)))
  | Inj (side, e') ->
    (match HTyp.matched_sum ty with
     | None -> None
     | Some (ty1,ty2) ->
       ana ctx e' (pick_side side ty1 ty2))
  | ListNil ->
    (match HTyp.matched_list ty with
     | None -> None
     | Some _ -> Some ())
  (* | ListLit es ->
    match HTyp.matched_list ty with
    | None -> None
    | Some ty_elt ->
      List.fold_left (fun optresult elt ->
        match optresult with
        | None -> None
        | Some _ -> ana ctx elt ty_elt
        end) es (Some tt)
    end *)
  | Case (e1, rules) ->
    (match syn ctx e1 with
     | None -> None
     | Some ty1 -> ana_rules ctx rules ty1 ty)
  | OpSeq (skel, seq) ->
    (match ana_skel ctx skel seq ty None with
     | None -> None
     | Some _ -> Some ())
  | EmptyHole _
  | Asc (_, _)
  | Var (_, _)
  | NumLit _
  | BoolLit _
  | ApPalette (_, _, _) ->
    (match syn' ctx e with
     | None -> None
     | Some ty' -> if HTyp.consistent ty ty' then Some () else None)
and ana_rules ctx rules pat_ty clause_ty =
  List.fold_left (fun b r ->
    match b with
    | None -> None
    | Some _ -> ana_rule ctx r pat_ty clause_ty) (Some ()) rules
and ana_rule ctx rule pat_ty clause_ty =
  let Rule (p, e) = rule in
  (match ana_pat ctx p pat_ty with
   | None -> None
   | Some ctx1 -> ana ctx1 e clause_ty)
and syn_skel ctx skel seq monitor =
  begin match skel with
  | Skel.Placeholder n ->
    begin match OperatorSeq.seq_nth n seq with
    | None -> None
    | Some en ->
      begin match bidelimited en with
      | false -> None
      | true ->
        begin match syn ctx en with
        | None -> None
        | Some ty ->
          let mode =
            begin match monitor with
            | Some n' ->
              if n = n' then Some (Synthesized ty)
              else None
            | None -> None
            end in
          Some (ty, mode)
        end
      end
    end
  | Skel.BinOp ((InHole (TypeInconsistent, u)), op, skel1, skel2)
  | Skel.BinOp ((InHole (WrongLength, u)), (UHExp.Comma as op), skel1, skel2) ->
    let skel_not_in_hole = Skel.BinOp (NotInHole, op, skel1, skel2) in
    begin match syn_skel ctx skel_not_in_hole seq monitor with
    | None -> None
    | Some (ty, mode) -> Some (HTyp.Hole, mode)
    end
  | Skel.BinOp ((InHole (WrongLength, _)), _, _, _) -> None
  | Skel.BinOp (NotInHole, UHExp.Plus, skel1, skel2)
  | Skel.BinOp (NotInHole, UHExp.Times, skel1, skel2) ->
    begin match ana_skel ctx skel1 seq HTyp.Num monitor with
    | None -> None
    | Some mode1 ->
      begin match ana_skel ctx skel2 seq HTyp.Num monitor with
      | None -> None
      | Some mode2 ->
        Some (HTyp.Num, combine_modes mode1 mode2)
      end
    end
  | Skel.BinOp (NotInHole, UHExp.LessThan, skel1, skel2) ->
    begin match ana_skel ctx skel1 seq HTyp.Num monitor with
    | None -> None
    | Some mode1 ->
      begin match ana_skel ctx skel2 seq HTyp.Num monitor with
      | None -> None
      | Some mode2 ->
        Some (HTyp.Bool, combine_modes mode1 mode2)
      end
    end
  | Skel.BinOp (NotInHole, UHExp.Space, skel1, skel2) ->
    begin match syn_skel ctx skel1 seq monitor with
    | None -> None
    | Some (ty1, mode1) ->
      begin match HTyp.matched_arrow ty1 with
      | None -> None
      | Some (ty2, ty) ->
        begin match ana_skel ctx skel2 seq ty2 monitor with
        | None -> None
        | Some mode2 ->
          Some (ty, combine_modes mode1 mode2)
        end
      end
    end
  | Skel.BinOp (NotInHole, UHExp.Comma, skel1, skel2) ->
    begin match syn_skel ctx skel1 seq monitor with
    | None -> None
    | Some (ty1, mode1) ->
      begin match syn_skel ctx skel2 seq monitor with
      | None -> None
      | Some (ty2, mode2) ->
        let mode = combine_modes mode1 mode2 in
        let ty = HTyp.Prod (ty1, ty2) in
        Some (ty, mode)
      end
    end
  | Skel.BinOp (NotInHole, UHExp.Cons, skel1, skel2) ->
    begin match syn_skel ctx skel1 seq monitor with
    | None -> None
    | Some (ty1, mode1) ->
      let ty = HTyp.List ty1 in
      begin match ana_skel ctx skel2 seq ty monitor with
      | None -> None
      | Some mode2 ->
        Some (ty, combine_modes mode1 mode2)
      end
    end
  end
and ana_skel ctx skel seq ty monitor =
  begin match skel with
  | Skel.Placeholder n ->
    begin match OperatorSeq.seq_nth n seq with
    | None -> None
    | Some en ->
      begin match bidelimited en with
      | false -> None
      | true ->
        begin match ana ctx en ty with
        | None -> None
        | Some _ ->
          begin match monitor with
          | Some n' ->
            if n = n'
            then Some (Some (AnalyzedAgainst ty))
            else Some (None)
          | None -> Some (None)
          end
        end
      end
    end
  | Skel.BinOp (NotInHole, UHExp.Comma, skel1, skel2) ->
    begin match ty with
    | HTyp.Hole ->
      begin match ana_skel ctx skel1 seq HTyp.Hole monitor with
      | None -> None
      | Some mode1 ->
        begin match ana_skel ctx skel2 seq HTyp.Hole monitor with
        | None -> None
        | Some mode2 ->
          let mode = combine_modes mode1 mode2 in
          Some mode
        end
      end
    | HTyp.Prod (ty1, ty2) ->
      let types = HTyp.get_tuple ty1 ty2 in
      let skels = UHExp.get_tuple skel1 skel2 in
      begin match Util.zip_eq skels types with
      | None -> None
      | Some zipped ->
        List.fold_left (fun opt_result (skel_ty : UHExp.skel_t * HTyp.t) ->
          begin match opt_result with
          | None -> None
          | Some mode ->
            let (skel, ty) = skel_ty in
            begin match ana_skel ctx skel seq ty monitor with
            | None -> None
            | Some mode' ->
              let mode = combine_modes mode mode' in
              Some mode
            end
          end) (Some None) zipped
      end
    | _ -> None
    end
  | Skel.BinOp ((InHole (WrongLength, u)), (UHExp.Comma as op), skel1, skel2) ->
    begin match ty with
    | HTyp.Prod (ty1, ty2) ->
      let types = HTyp.get_tuple ty1 ty2 in
      let skels = UHExp.get_tuple skel1 skel2 in
      let n_types = List.length types in
      let n_skels = List.length skels in
      begin match n_types = n_skels with
      | true -> None (* make sure the lengths are actually different *)
      | false ->
        let (zipped, remainder) = HTyp.zip_with_skels skels types in
        let ana_zipped : (type_mode option) option =
          List.fold_left (fun opt_result (skel_ty : UHExp.skel_t * HTyp.t) ->
            begin match opt_result with
            | None -> None
            | Some (mode) ->
              let (skel, ty) = skel_ty in
              begin match ana_skel ctx skel seq ty monitor with
              | None -> None
              | Some mode' ->
                let mode = combine_modes mode mode' in
                Some (mode)
              end
            end) zipped (Some None) in
        begin match ana_zipped with
        | None -> None
        | Some mode ->
          List.fold_left (fun opt_result skel ->
            begin match opt_result with
            | None -> None
            | Some mode ->
              begin match syn_skel ctx skel seq monitor with
              | None -> None
              | Some (_, mode') ->
                let mode = combine_modes mode mode' in
                Some mode
              end
            end) (Some mode) remainder
        end
      end
    | _ -> None
    end
  | Skel.BinOp ((InHole (WrongLength, _)), _, _, _) -> None
  | Skel.BinOp (NotInHole, UHExp.Cons, skel1, skel2) ->
    begin match HTyp.matched_list ty with
    | None -> None
    | Some ty_elt ->
      begin match ana_skel ctx skel1 seq ty_elt monitor with
      | None -> None
      | Some mode1 ->
        let ty_list = HTyp.List ty_elt in
        begin match ana_skel ctx skel2 seq ty_list monitor with
        | None -> None
        | Some mode2 ->
          Some (combine_modes mode1 mode2)
        end
      end
    end
  | Skel.BinOp ((InHole (TypeInconsistent, _)), _, _, _)
  | Skel.BinOp (NotInHole, UHExp.Plus, _, _)
  | Skel.BinOp (NotInHole, UHExp.Times, _, _)
  | Skel.BinOp (NotInHole, UHExp.LessThan, _, _)
  | Skel.BinOp (NotInHole, UHExp.Space, _, _) ->
    begin match syn_skel ctx skel seq monitor with
    | None -> None
    | Some (ty', mode) ->
      if HTyp.consistent ty ty' then Some mode else None
    end
  end

let rec syn_pat_fix_holes ctx u_gen renumber_empty_holes p =
  match p with
  | UHPat.Pat (_, p') ->
    (match syn_pat_fix_holes' ctx u_gen renumber_empty_holes p' with
     | None -> None
     | Some (p', ty, ctx, u_gen) ->
       Some (UHPat.Pat (NotInHole, p'),ty,ctx,u_gen))
  | UHPat.Parenthesized p ->
    (match syn_pat_fix_holes ctx u_gen renumber_empty_holes p with
     | None -> None
     | Some (p, ty, ctx, u_gen) ->
       Some (UHPat.Parenthesized p,ty,ctx,u_gen))
and syn_pat_fix_holes' ctx u_gen renumber_empty_holes p =
  match p with
  | UHPat.EmptyHole _ ->
    if renumber_empty_holes
    then
      let u,u_gen = MetaVarGen.next u_gen in
      Some (UHPat.EmptyHole u,HTyp.Hole,ctx,u_gen)
    else
      Some (p,HTyp.Hole,ctx,u_gen)
  | UHPat.Wild -> Some (p,HTyp.Hole,ctx,u_gen)
  | UHPat.Var x ->
    Var.check_valid x
      (let ctx = Contexts.extend_gamma ctx (x,HTyp.Hole) in
       Some (p,HTyp.Hole,ctx,u_gen))
  | UHPat.NumLit _ -> Some (p,HTyp.Num,ctx,u_gen)
  | UHPat.BoolLit _ -> Some (p,HTyp.Bool,ctx,u_gen)
  | UHPat.ListNil -> Some (p,HTyp.List HTyp.Hole,ctx,u_gen)
  (* | UHPat.ListLit ps ->
    let opt_result = List.fold_left (fun opt_result p ->
      match opt_result with
      | None -> None
      | Some (ps, ty, ctx, u_gen) ->
        match syn_pat_fix_holes ctx u_gen renumber_empty_holes p with
        | Some (p, ty', ctx, u_gen) ->
          match HTyp.join ty ty' with
          | Some ty_joined -> Some (cons p ps, ty_joined, ctx, u_gen)
          | None ->
            match ana_pat_fix_holes ctx u_gen renumber_empty_holes p ty with
            | None -> None
            | Some (p, ctx, u_gen) -> Some (cons p ps, ty, ctx, u_gen)
            end
          end
        | None ->
          match ana_pat_fix_holes ctx u_gen renumber_empty_holes p ty with
          | None -> None
          | Some (p, ctx, u_gen) -> Some (cons p ps, ty, ctx, u_gen)
          end
        end
      end) ps (Some ([], HTyp.Hole, ctx, u_gen)) in
    match opt_result with
    | None -> None
    | Some (ps, ty, ctx, u_gen) ->
      Some (UHPat.ListLit ps, HTyp.List ty, ctx, u_gen)
    end *)
  | UHPat.Inj (side, p1) ->
    (match syn_pat_fix_holes ctx u_gen renumber_empty_holes p1 with
     | None -> None
     | Some (p1, ty1, ctx, u_gen) ->
       let ty =
         match side with
         | L -> HTyp.Sum (ty1, HTyp.Hole)
         | R -> HTyp.Sum (HTyp.Hole, ty1)
       in
       Some (UHPat.Inj (side, p1),ty,ctx,u_gen))
  | UHPat.OpSeq (skel, seq) ->
    (match syn_skel_pat_fix_holes ctx u_gen renumber_empty_holes
             skel seq with
     | None -> None
     | Some (skel, seq, ty, ctx, u_gen) ->
       Some (UHPat.OpSeq (skel, seq),ty,ctx,u_gen))
and syn_skel_pat_fix_holes ctx u_gen renumber_empty_holes skel seq =
  begin match skel with
  | Skel.Placeholder n ->
    begin match OperatorSeq.seq_nth n seq with
    | None -> None
    | Some pn ->
      begin match UHPat.bidelimited pn with
      | false -> None
      | true ->
        begin match syn_pat_fix_holes ctx u_gen renumber_empty_holes pn with
        | None -> None
        | Some (pn, ty, ctx, u_gen) ->
          begin match OperatorSeq.seq_update_nth n seq pn with
          | None -> None
          | Some seq ->
            Some (skel, seq, ty, ctx, u_gen)
          end
        end
      end
    end
  | Skel.BinOp (_, UHPat.Comma, skel1, skel2) ->
    begin match syn_skel_pat_fix_holes ctx u_gen renumber_empty_holes skel1 seq with
    | None -> None
    | Some (skel1, seq, ty1, ctx, u_gen) ->
      begin match syn_skel_pat_fix_holes ctx u_gen renumber_empty_holes skel2 seq with
      | None -> None
      | Some (skel2, seq, ty2, ctx, u_gen) ->
        let skel = Skel.BinOp (NotInHole, UHPat.Comma, skel1, skel2) in
        let ty = HTyp.Prod (ty1, ty2) in
        Some (skel, seq, ty, ctx, u_gen)
      end
    end
  | Skel.BinOp (_, UHPat.Space, skel1, skel2) ->
    begin match syn_skel_pat_fix_holes ctx u_gen renumber_empty_holes skel1 seq with
    | None -> None
    | Some (skel1, seq, ty1, ctx, u_gen) ->
      begin match syn_skel_pat_fix_holes ctx u_gen renumber_empty_holes skel2 seq with
      | None -> None
      | Some (skel2, seq, ty2, ctx, u_gen) ->
        let skel = Skel.BinOp (NotInHole, UHPat.Comma, skel1, skel2) in
        let ty = HTyp.Hole in
        Some (skel, seq, ty, ctx, u_gen)
      end
    end
  | Skel.BinOp (_, UHPat.Cons, skel1, skel2) ->
    begin match syn_skel_pat_fix_holes ctx u_gen renumber_empty_holes skel1 seq with
    | None -> None
    | Some (skel1, seq, ty_elt, ctx, u_gen) ->
      let ty = HTyp.List ty_elt in
      begin match ana_skel_pat_fix_holes ctx u_gen renumber_empty_holes skel2 seq ty with
      | None -> None
      | Some (skel2, seq, ctx, u_gen) ->
        let skel = Skel.BinOp (NotInHole, UHPat.Cons, skel1, skel2) in
        Some (skel, seq, ty, ctx, u_gen)
      end
    end
  end
and ana_pat_fix_holes ctx u_gen renumber_empty_holes p ty =
  match p with
  | UHPat.Pat (_, p') ->
    (match ana_pat_fix_holes' ctx u_gen renumber_empty_holes p'
             ty with
     | None -> None
     | Some (err_status, p', ctx, u_gen) ->
       Some (UHPat.Pat (err_status, p'),ctx,u_gen))
  | UHPat.Parenthesized p ->
    (match ana_pat_fix_holes ctx u_gen renumber_empty_holes p ty with
     | None -> None
     | Some (p, ctx, u_gen) ->
       Some (UHPat.Parenthesized p,ctx,u_gen))
and ana_pat_fix_holes' ctx u_gen renumber_empty_holes p ty =
  match p with
  | UHPat.Wild -> Some (NotInHole,p,ctx,u_gen)
  | UHPat.Var x ->
    Var.check_valid x
      (let ctx = Contexts.extend_gamma ctx (x,ty) in
       Some (NotInHole,p,ctx,u_gen))
  | UHPat.EmptyHole _
  | UHPat.NumLit _
  | UHPat.BoolLit _ ->
    (match syn_pat_fix_holes' ctx u_gen renumber_empty_holes p with
     | None -> None
     | Some (p', ty', ctx, u_gen) ->
      if HTyp.consistent ty ty'
      then Some (NotInHole,p',ctx,u_gen)
      else
        let u,u_gen = MetaVarGen.next u_gen in
        Some (InHole (TypeInconsistent, u),p',ctx,u_gen))
  | UHPat.Inj (side, p1) ->
    (match HTyp.matched_sum ty with
     | Some (tyL, tyR) ->
       let ty1 = pick_side side tyL tyR in
       (match ana_pat_fix_holes ctx u_gen renumber_empty_holes p1 ty1 with
        | None -> None
        | Some (p1, ctx, u_gen) ->
          Some (NotInHole,UHPat.Inj (side, p1),ctx,u_gen))
     | None ->
       (match syn_pat_fix_holes ctx u_gen renumber_empty_holes p1 with
        | None -> None
        | Some (p1, ty, ctx, u_gen) ->
          Some (InHole (TypeInconsistent, u),UHPat.Inj (side, p1),ctx,u_gen)))
  | UHPat.ListNil ->
    (match HTyp.matched_list ty with
     | Some _ -> Some (NotInHole,p,ctx,u_gen)
     | None ->
       let u,u_gen = MetaVarGen.next u_gen in
       Some (InHole (TypeInconsistent, u),p,ctx,u_gen))
   (* | UHPat.ListLit ps ->
    match HTyp.matched_list ty with
    | Some ty_elt ->
      let ps_result =
        List.fold_left (fun opt_result elt ->
          match opt_result with
          | None -> None
          | Some (ps, ctx, u_gen) ->
            match ana_pat_fix_holes ctx u_gen renumber_empty_holes elt ty_elt with
            | None -> None
            | Some (elt, ctx, u_gen) ->
              Some (cons elt ps, ctx, u_gen)
            end
          end) ps (Some ([], ctx, u_gen)) in
      match ps_result with
      | None -> None
      | Some (ps, ctx, u_gen) ->
        Some (NotInHole, UHPat.ListLit ps, ctx, u_gen)
      end
    | None -> None (* TODO should return InHole *)
    end *)
  | UHPat.OpSeq (skel, seq) ->
    (match ana_skel_pat_fix_holes ctx u_gen renumber_empty_holes skel seq ty with
     | None -> None
     | Some (Skel.Placeholder (_, _), _, _, _) -> None
     | Some ((Skel.BinOp (err, _, _, _)) as skel, seq, ctx, u_gen) ->
       let p = UHPat.OpSeq (skel, seq) in
       Some (err, p, ctx, u_gen))
and ana_skel_pat_fix_holes ctx u_gen renumber_empty_holes skel seq ty =
  begin match skel with
  | Skel.Placeholder n ->
    begin match OperatorSeq.seq_nth n seq with
    | None -> None
    | Some pn ->
      begin match UHPat.bidelimited pn with
      | false -> None
      | true ->
        begin match ana_pat_fix_holes ctx u_gen renumber_empty_holes pn ty with
        | None -> None
        | Some (pn, ctx, u_gen) ->
          begin match OperatorSeq.seq_update_nth n seq pn with
          | Some seq -> Some (skel, seq, ctx, u_gen)
          | None -> None
          end
        end
      end
    end
  | Skel.BinOp (_, UHPat.Comma, skel1, skel2) ->
    begin match ty with
    | HTyp.Hole ->
      begin match ana_skel_pat_fix_holes ctx u_gen renumber_empty_holes skel1 seq HTyp.Hole with
      | None -> None
      | Some (skel1, seq, ctx, u_gen) ->
        begin match ana_skel_pat_fix_holes ctx u_gen renumber_empty_holes skel2 seq HTyp.Hole with
        | None -> None
        | Some (skel2, seq, ctx, u_gen) ->
          let skel = Skel.BinOp (NotInHole, UHPat.Comma, skel1, skel2) in
          Some (skel, seq, ctx, u_gen)
        end
      end
    | HTyp.Prod (ty1, ty2) ->
      let types = HTyp.get_tuple ty1 ty2 in
      let skels = UHPat.get_tuple skel1 skel2 in
      begin match Util.zip_eq skels types with
      | Some zipped ->
        let fixed =
          List.fold_right (fun (skel_ty : UHPat.skel_t * HTyp.t) opt_result ->
            begin match opt_result with
            | None -> None
            | Some (skels, seq, ctx, u_gen) ->
              let (skel, ty) = skel_ty in
              begin match ana_skel_pat_fix_holes ctx u_gen renumber_empty_holes skel seq ty with
              | None -> None
              | Some (skel, seq, ctx, u_gen) ->
                Some (skel::skels, seq, ctx, u_gen)
              end
            end) zipped (Some ([], seq, ctx, u_gen)) in
        begin match fixed with
        | None -> None
        | Some (skels, seq, ctx, u_gen) ->
          begin match UHPat.make_tuple NotInHole skels with
          | None -> None
          | Some skel -> Some (skel, seq, ctx, u_gen)
          end
        end
      | None ->
        let (zipped, remainder) = HTyp.zip_with_skels skels types in
        let fixed1 =
          List.fold_right (fun (skel_ty : UHPat.skel_t * HTyp.t) opt_result ->
            begin match opt_result with
            | None -> None
            | Some (skels, seq, ctx, u_gen) ->
              let (skel, ty) = skel_ty in
              begin match ana_skel_pat_fix_holes ctx u_gen renumber_empty_holes skel seq ty with
              | None -> None
              | Some (skel, seq, ctx, u_gen) ->
                Some (skel::skels, seq, ctx, u_gen)
              end
            end) zipped (Some ([], seq, ctx, u_gen)) in
        begin match fixed1 with
        | None -> None
        | Some (skels1, seq, ctx, u_gen) ->
          let fixed2 =
            List.fold_right (fun (skel : UHPat.skel_t) opt_result ->
              begin match opt_result with
              | None -> None
              | Some (skels, seq, ctx, u_gen) ->
                begin match syn_skel_pat_fix_holes ctx u_gen renumber_empty_holes skel seq with
                | None -> None
                | Some (skel, seq, ty, ctx, u_gen) ->
                  Some (skel::skels, seq, ctx, u_gen)
                end
              end) remainder (Some ([], seq, ctx, u_gen)) in
          begin match fixed2 with
          | None -> None
          | Some (skels2, seq, ctx, u_gen) ->
            let skels = skels1 @ skels2 in
            let (u, u_gen) = MetaVarGen.next u_gen in
            begin match UHPat.make_tuple (InHole (WrongLength, u)) skels with
            | None -> None
            | Some skel -> Some (skel, seq, ctx, u_gen)
            end
          end
        end
      end
    | _ ->
      begin match syn_skel_pat_fix_holes ctx u_gen renumber_empty_holes skel1 seq with
      | None -> None
      | Some (skel1, seq, _, ctx, u_gen) ->
        begin match syn_skel_pat_fix_holes ctx u_gen renumber_empty_holes skel2 seq with
        | None -> None
        | Some (skel2, seq, _, ctx, u_gen) ->
          let (u, u_gen) = MetaVarGen.next u_gen in
          let skel = Skel.BinOp ((InHole (TypeInconsistent, u)), UHPat.Comma, skel1, skel2) in
          Some (skel, seq, ctx, u_gen)
        end
      end
    end
  | Skel.BinOp (_, UHPat.Space, skel1, skel2) ->
    begin match syn_skel_pat_fix_holes ctx u_gen renumber_empty_holes skel1 seq with
    | None -> None
    | Some (skel1, seq, _, ctx, u_gen) ->
      begin match syn_skel_pat_fix_holes ctx u_gen renumber_empty_holes skel2 seq with
      | None -> None
      | Some (skel2, seq, _, ctx, u_gen) ->
        let (u, u_gen) = MetaVarGen.next u_gen in
        let skel = Skel.BinOp ((InHole (TypeInconsistent, u)), UHPat.Space, skel1, skel2) in
        Some (skel, seq, ctx, u_gen)
      end
    end
  | Skel.BinOp (_, UHPat.Cons, skel1, skel2) ->
    begin match HTyp.matched_list ty with
    | Some ty_elt ->
      begin match ana_skel_pat_fix_holes ctx u_gen renumber_empty_holes skel1 seq ty_elt with
      | None -> None
      | Some (skel1, seq, ctx, u_gen) ->
        let ty_list = HTyp.List ty_elt in
        begin match ana_skel_pat_fix_holes ctx u_gen renumber_empty_holes skel2 seq ty_list with
        | None -> None
        | Some (skel2, seq, ctx, u_gen) ->
          let skel = Skel.BinOp (NotInHole, UHPat.Cons, skel1, skel2) in
          Some (skel, seq, ctx, u_gen)
        end
      end
    | None ->
      begin match syn_skel_pat_fix_holes ctx u_gen renumber_empty_holes skel1 seq with
      | None -> None
      | Some (skel1, seq, ty_elt, ctx, u_gen) ->
        let ty_list = HTyp.List ty_elt in
        begin match ana_skel_pat_fix_holes ctx u_gen renumber_empty_holes skel2 seq ty_list with
        | None -> None
        | Some (skel2, seq, ctx, u_gen) ->
          let (u, u_gen) = MetaVarGen.next u_gen in
          let skel = Skel.BinOp ((InHole (TypeInconsistent, u)), UHPat.Cons, skel1, skel2) in
          Some (skel, seq, ctx, u_gen)
        end
      end
    end
  end

(* need to pass a reference to the ana_fix_holes_internal function here
 * rather than defining it mutually to avoid a stack overflow error seemingly
 * related to too many mutually recursive definitions in Coq *)
let ana_rule_fix_holes ctx u_gen renumber_empty_holes rule pat_ty clause_ty ana_fix_holes_internal =
  let Rule (pat, e) = rule in
  (match ana_pat_fix_holes ctx u_gen renumber_empty_holes pat pat_ty with
   | None -> None
   | Some (pat', ctx, u_gen) ->
     (match ana_fix_holes_internal ctx u_gen
              renumber_empty_holes e clause_ty with
      | None -> None
      | Some (e',u_gen) -> Some (Rule (pat', e'),u_gen)))

(* see above re: ana_fix_holes_internal *)
let ana_rules_fix_holes_internal ctx u_gen renumber_empty_holes rules pat_ty clause_ty ana_fix_holes_internal =
  List.fold_right (fun r b ->
    match b with
    | None -> None
    | Some (rules, u_gen) ->
      (match ana_rule_fix_holes ctx u_gen renumber_empty_holes
               r pat_ty clause_ty ana_fix_holes_internal with
       | None -> None
       | Some (r, u_gen) ->
         Some (r::rules,u_gen))) rules (Some ([],u_gen))

(* If renumber_empty_holes is true, then the metavars in empty holes will be assigned
 * new values in the same namespace as non-empty holes. Non-empty holes are renumbered
 * regardless.
 *)
let rec syn_fix_holes_internal ctx u_gen renumber_empty_holes e =
  match e with
  | Tm (_, e') ->
    (match syn_fix_holes' ctx u_gen renumber_empty_holes e' with
     | None -> None
     | Some (e'', ty, u_gen') ->
       Some (Tm (NotInHole, e''),ty,u_gen'))
  | Parenthesized e1 ->
    (match syn_fix_holes_internal ctx u_gen renumber_empty_holes e1 with
     | None -> None
     | Some (e1', ty, u_gen') ->
       Some (Parenthesized e1',ty,u_gen'))
and syn_fix_holes' ctx u_gen renumber_empty_holes e =
  match e with
  | EmptyHole u ->
    if renumber_empty_holes
    then
      let u',u_gen'' = MetaVarGen.next u_gen in
      Some (EmptyHole u',HTyp.Hole,u_gen'')
    else
      Some (EmptyHole u,HTyp.Hole,u_gen)
  | Asc (e1, uty) ->
    begin match bidelimited e1 with
    | false -> None
    | true ->
      let ty = UHTyp.expand uty in
      (match ana_fix_holes_internal ctx u_gen renumber_empty_holes e1 ty with
       | None -> None
       | Some (e1', u_gen') ->
         Some (Asc (e1', uty),ty,u_gen'))
    end
  | Var (var_err_status, x) ->
    let gamma,_ = ctx in
    (match VarMap.lookup gamma x with
     | Some ty -> Some (Var (NotInVHole, x),ty,u_gen)
     | None ->
       (match var_err_status with
        | InVHole _ -> Some (e,HTyp.Hole,u_gen)
        | NotInVHole ->
          let u,u_gen = MetaVarGen.next u_gen in
          Some (Var ((InVHole u), x),HTyp.Hole,u_gen)))
  | Lam (p, ann, e1) ->
    let ty1 =
      match ann with
      | Some uty1 -> UHTyp.expand uty1
      | None -> HTyp.Hole
    in
    (match ana_pat_fix_holes ctx u_gen renumber_empty_holes p ty1 with
     | None -> None
     | Some (p, ctx1, u_gen) ->
       (match syn_fix_holes_internal ctx1 u_gen renumber_empty_holes e1 with
        | None -> None
        | Some (e1, ty2, u_gen) ->
          Some (Lam (p, ann, e1),HTyp.Arrow (ty1, ty2),u_gen)))
  | Let (p, ann, e1, e2) ->
    (match ann with
     | Some uty1 ->
       let ty1 = UHTyp.expand uty1 in
       let ctx1 = ctx_for_let ctx p ty1 e1 in
       (match ana_fix_holes_internal ctx1 u_gen renumber_empty_holes e1 ty1 with
        | None -> None
        | Some (e1, u_gen) ->
          (match ana_pat_fix_holes ctx u_gen renumber_empty_holes p ty1 with
           | None -> None
           | Some (p, ctx, u_gen) ->
             (match syn_fix_holes_internal ctx u_gen renumber_empty_holes e2 with
              | None -> None
              | Some (e2, ty, u_gen) ->
                Some (Let (p, ann, e1, e2),ty,u_gen))))
     | None ->
       (match syn_fix_holes_internal ctx u_gen renumber_empty_holes e1 with
        | None -> None
        | Some (e1, ty1, u_gen) ->
          (match ana_pat_fix_holes ctx u_gen renumber_empty_holes p ty1 with
           | None -> None
           | Some (p, ctx, u_gen) ->
             (match syn_fix_holes_internal ctx u_gen renumber_empty_holes e2 with
              | None -> None
              | Some (e2, ty, u_gen) ->
                Some (Let (p, ann, e1, e2),ty,u_gen)))))
  | NumLit _ -> Some (e,HTyp.Num,u_gen)
  | BoolLit _ -> Some (e,HTyp.Bool,u_gen)
  | ListNil -> Some (e,HTyp.List HTyp.Hole,u_gen)
  (* | ListLit es ->
    let opt_result = List.fold_left (fun opt_result e ->
      match opt_result with
      | None -> None
      | Some (es, ty, u_gen) ->
        match syn_fix_holes_internal ctx u_gen renumber_empty_holes e with
        | Some (e, ty', u_gen) ->
          match HTyp.join ty ty' with
          | Some ty_joined -> Some (cons e es, ty_joined, u_gen)
          | None ->
            match ana_fix_holes_internal ctx u_gen renumber_empty_holes e ty with
            | None -> None
            | Some (e, u_gen) -> Some (cons e es, ty, u_gen)
            end
          end
        | None ->
          match ana_fix_holes_internal ctx u_gen renumber_empty_holes e ty with
          | None -> None
          | Some (e, u_gen) -> Some (cons e es, ty, u_gen)
          end
        end
      end) es (Some ([], HTyp.Hole, u_gen)) in
    match opt_result with
    | None -> None
    | Some (es, ty, u_gen) ->
      Some (UHExp.ListLit es, HTyp.List ty, u_gen)
    end *)
  | OpSeq (skel, seq) ->
    (match syn_skel_fix_holes ctx u_gen renumber_empty_holes skel seq with
     | None -> None
     | Some (Skel.Placeholder _, _, _, _) -> None
     | Some (skel, seq, ty, u_gen) -> Some (OpSeq (skel, seq),ty,u_gen))
  | Inj (side, e1) ->
    (match syn_fix_holes_internal ctx u_gen renumber_empty_holes e1 with
     | None -> None
     | Some (e1', ty1, u_gen') ->
       let e' = Inj (side, e1') in
       let ty' =
         match side with
         | L -> HTyp.Sum (ty1, HTyp.Hole)
         | R -> HTyp.Sum (HTyp.Hole, ty1)
       in
       Some (e',ty',u_gen'))
  | Case (_, _) -> None
  | ApPalette (name, serialized_model, hole_data) ->
    let _,palette_ctx = ctx in
    (match VarMap.lookup palette_ctx name with
     | None -> None
     | Some palette_defn ->
       (match ana_fix_holes_hole_data ctx u_gen renumber_empty_holes hole_data with
        | None -> None
        | Some (hole_data', u_gen') ->
          let expansion_ty = PaletteDefinition.expansion_ty palette_defn in
          let to_exp = PaletteDefinition.to_exp palette_defn in
          let expansion = to_exp serialized_model in
          let _,hole_map = hole_data in
          let expansion_ctx = PaletteHoleData.extend_ctx_with_hole_map ctx hole_map in
          (match ana expansion_ctx expansion expansion_ty with
           | None -> None
           | Some _ ->
             Some (ApPalette (name, serialized_model,
               hole_data'),expansion_ty,u_gen'))))
and ana_fix_holes_hole_data ctx u_gen renumber_empty_holes hole_data =
  let next_ref,hole_map = hole_data in
  let init = NatMap.empty,u_gen in
  let hole_map_opt' =
    NatMap.fold hole_map (fun c v ->
      let i,ty_e = v in
      let ty,e = ty_e in
      (match c with
       | None -> None
       | Some (xs, u_gen) ->
         (match ana_fix_holes_internal ctx u_gen renumber_empty_holes e ty with
          | None -> None
          | Some (e', u_gen') ->
            let xs' = NatMap.extend xs (i,(ty,e')) in
            Some (xs',u_gen')))) (Some init)
  in
  (match hole_map_opt' with
   | None -> None
   | Some (hole_map', u_gen') ->
     Some ((next_ref,hole_map'),u_gen'))
and ana_fix_holes_internal ctx u_gen renumber_empty_holes e ty =
  match e with
  | Tm (_, e1) ->
    (match ana_fix_holes' ctx u_gen renumber_empty_holes e1 ty with
     | None -> None
     | Some (err_status, e1, u_gen) ->
       Some (Tm (err_status, e1),u_gen))
  | Parenthesized e1 ->
    (match ana_fix_holes_internal ctx u_gen renumber_empty_holes e1 ty with
     | None -> None
     | Some (e1, u_gen) ->
       Some (Parenthesized e1,u_gen))
and ana_fix_holes' ctx u_gen renumber_empty_holes e ty =
  match e with
  | Let (p, ann, e1, e2) ->
    (match ann with
     | Some uty1 ->
       let ty1 = UHTyp.expand uty1 in
       let ctx1 = ctx_for_let ctx p ty1 e1 in
       (match ana_fix_holes_internal ctx1 u_gen renumber_empty_holes e1 ty1 with
        | None -> None
        | Some (e1, u_gen) ->
          (match ana_pat_fix_holes ctx u_gen renumber_empty_holes p ty1 with
           | None -> None
           | Some (p, ctx, u_gen) ->
             (match ana_fix_holes_internal ctx u_gen renumber_empty_holes e2 ty with
              | None -> None
              | Some (e2, u_gen) ->
                Some (NotInHole,Let (p, ann, e1, e2),u_gen))))
     | None ->
       (match syn_fix_holes_internal ctx u_gen
                renumber_empty_holes e1 with
        | None -> None
        | Some (e1, ty1, u_gen) ->
          (match ana_pat_fix_holes ctx u_gen renumber_empty_holes p ty1 with
           | None -> None
           | Some (p, ctx, u_gen) ->
             (match ana_fix_holes_internal ctx u_gen renumber_empty_holes e2 ty with
              | None -> None
              | Some (e2, u_gen) ->
                Some (NotInHole,Let (p, ann, e1, e2),u_gen)))))
  | Lam (p, ann, e1) ->
    (match HTyp.matched_arrow ty with
     | Some (ty1_given, ty2) ->
       (match ann with
        | Some uty1 ->
          let ty1_ann = UHTyp.expand uty1 in
          if HTyp.consistent ty1_ann ty1_given
          then (match ana_pat_fix_holes ctx u_gen
                        renumber_empty_holes p ty1_ann with
                | None -> None
                | Some (p, ctx, u_gen) ->
                  (match ana_fix_holes_internal ctx u_gen renumber_empty_holes e1 ty2 with
                   | None -> None
                   | Some (e1, u_gen) ->
                     Some (NotInHole,Lam (p, ann, e1),u_gen)))
          else (match syn_fix_holes' ctx u_gen
                        renumber_empty_holes e with
                | None -> None
                | Some (e, ty, u_gen) ->
                  let u,u_gen = MetaVarGen.next u_gen in
                  Some (InHole (TypeInconsistent, u),e,u_gen))
        | None ->
          (match ana_pat_fix_holes ctx u_gen
                   renumber_empty_holes p ty1_given with
           | None -> None
           | Some (p, ctx, u_gen) ->
             (match ana_fix_holes_internal ctx u_gen
                      renumber_empty_holes e1 ty2 with
              | None -> None
              | Some (e1, u_gen) ->
                Some (NotInHole,Lam (p, ann, e1),u_gen))))
     | None ->
       (match syn_fix_holes' ctx u_gen renumber_empty_holes e with
        | None -> None
        | Some (e, ty', u_gen) ->
          let u,u_gen = MetaVarGen.next u_gen in
          Some (InHole (TypeInconsistent, u),e,u_gen)))
  | Inj (side, e1) ->
    (match HTyp.matched_sum ty with
     | Some (ty1, ty2) ->
       (match ana_fix_holes_internal ctx u_gen
                renumber_empty_holes e1 (pick_side side ty1 ty2) with
        | None -> None
        | Some (e1', u_gen') ->
          Some (NotInHole,Inj (side, e1'),u_gen'))
     | None ->
       (match syn_fix_holes' ctx u_gen renumber_empty_holes e with
        | None -> None
        | Some (e', ty', u_gen') ->
          if HTyp.consistent ty ty'
          then
            Some (NotInHole,e',u_gen')
          else
            let u,u_gen'' = MetaVarGen.next u_gen' in
            Some (InHole (TypeInconsistent, u),e',u_gen'')))
  | ListNil ->
    (match HTyp.matched_list ty with
     | Some _ -> Some (NotInHole,e,u_gen)
     | None ->
       let u,u_gen = MetaVarGen.next u_gen in
       Some (InHole (TypeInconsistent, u),e,u_gen))
  (* | ListLit es ->
    match HTyp.matched_list ty with
    | Some ty_elt ->
      let opt_es = List.fold_left (fun opt_result elt ->
        match opt_result with
        | None -> None
        | Some (es, u_gen) ->
          match ana_fix_holes_internal ctx u_gen renumber_empty_holes elt ty_elt with
          | None -> None
          | Some (elt, u_gen) ->
            Some (cons elt es, u_gen)
          end
        end) es (Some ([], u_gen)) in
      match opt_es with
      | None -> None
      | Some (es, u_gen) -> Some (NotInHole, ListLit es, u_gen)
      end
    | None -> None (* TODO put in hole if not a list *)
    end *)
  | Case (e1, rules) ->
    (match syn_fix_holes_internal ctx u_gen renumber_empty_holes e1 with
     | None -> None
     | Some (e1', ty1, u_gen) ->
       (match ana_rules_fix_holes_internal ctx u_gen
                renumber_empty_holes rules ty1 ty
                ana_fix_holes_internal with
        | None -> None
        | Some (rules', u_gen) ->
          Some (NotInHole,Case (e1', rules'),u_gen)))
  | OpSeq (skel, seq) ->
    (match ana_skel_fix_holes ctx u_gen renumber_empty_holes
             skel seq ty with
     | None -> None
     | Some (Skel.Placeholder _, _, _) -> None
     | Some ((Skel.BinOp (err, _, _, _) as skel), seq, u_gen) ->
       Some (err,OpSeq (skel, seq),u_gen))
  | EmptyHole _
  | Asc (_, _)
  | Var (_, _)
  | NumLit _
  | BoolLit _
  | ApPalette (_, _, _) ->
    (match syn_fix_holes' ctx u_gen renumber_empty_holes e with
     | None -> None
     | Some (e', ty', u_gen') ->
       if HTyp.consistent ty ty'
       then Some (NotInHole,e',u_gen')
       else let u,u_gen'' = MetaVarGen.next u_gen' in
            Some (InHole (TypeInconsistent, u),e',u_gen''))
and syn_skel_fix_holes ctx u_gen renumber_empty_holes skel seq =
  begin match skel with
  | Skel.Placeholder n ->
    begin match OperatorSeq.seq_nth n seq with
    | None -> None
    | Some en ->
      begin match bidelimited en with
      | false -> None
      | true ->
        begin match syn_fix_holes_internal ctx u_gen renumber_empty_holes en with
        | None -> None
        | Some (en, ty, u_gen) ->
          begin match OperatorSeq.seq_update_nth n seq en with
          | None -> None
          | Some seq ->
            Some (skel, seq, ty, u_gen)
          end
        end
      end
    end
  | Skel.BinOp (_, (UHExp.Plus as op), skel1, skel2)
  | Skel.BinOp (_, (UHExp.Times as op), skel1, skel2) ->
    begin match ana_skel_fix_holes ctx u_gen renumber_empty_holes skel1 seq HTyp.Num with
    | Some (skel1, seq, u_gen) ->
      begin match ana_skel_fix_holes ctx u_gen renumber_empty_holes skel2 seq HTyp.Num with
      | Some (skel2, seq, u_gen) ->
        Some (Skel.BinOp (NotInHole, op, skel1, skel2), seq, HTyp.Num, u_gen)
      | None -> None
      end
    | None -> None
    end
  | Skel.BinOp (_, (UHExp.LessThan as op), skel1, skel2) ->
    begin match ana_skel_fix_holes ctx u_gen renumber_empty_holes skel1 seq HTyp.Num with
    | Some (skel1, seq, u_gen) ->
      begin match ana_skel_fix_holes ctx u_gen renumber_empty_holes skel2 seq HTyp.Num with
      | Some (skel2, seq, u_gen) ->
        Some (Skel.BinOp (NotInHole, op, skel1, skel2), seq, HTyp.Bool, u_gen)
      | None -> None
      end
    | None -> None
    end
  | Skel.BinOp (_, UHExp.Space, skel1, skel2) ->
    begin match syn_skel_fix_holes ctx u_gen renumber_empty_holes skel1 seq with
    | Some (skel1', seq1, ty1, u_gen1) ->
      begin match HTyp.matched_arrow ty1 with
      | Some (ty2, ty) ->
        begin match ana_skel_fix_holes ctx u_gen1 renumber_empty_holes skel2 seq1 ty2 with
        | Some (skel2', seq2, u_gen2) ->
          Some (Skel.BinOp (NotInHole, Space, skel1', skel2'), seq2, ty, u_gen2)
        | None -> None
        end
      | None ->
        begin match ana_skel_fix_holes ctx u_gen1 renumber_empty_holes skel2 seq1 HTyp.Hole with
        | Some (skel2', seq2, u_gen2) ->
          begin match UHExp.make_skel_inconsistent u_gen2 skel1' seq2 with
          | Some (skel1'', seq3, u_gen3) ->
            Some (Skel.BinOp (NotInHole, Space, skel1'', skel2'), seq3, HTyp.Hole, u_gen3)
          | None -> None
          end
        | None -> None
        end
      end
    | None -> None
    end
  | Skel.BinOp (_, UHExp.Comma, skel1, skel2) ->
    begin match syn_skel_fix_holes ctx u_gen renumber_empty_holes skel1 seq with
    | None -> None
    | Some (skel1, seq, ty1, u_gen) ->
      begin match syn_skel_fix_holes ctx u_gen renumber_empty_holes skel2 seq with
      | None -> None
      | Some (skel2, seq, ty2, u_gen) ->
        let skel = Skel.BinOp (NotInHole, Comma, skel1, skel2) in
        let ty = HTyp.Prod (ty1, ty2) in
        Some (skel, seq, ty, u_gen)
      end
    end
  | Skel.BinOp (_, UHExp.Cons, skel1, skel2) ->
    begin match syn_skel_fix_holes ctx u_gen renumber_empty_holes skel1 seq with
    | None -> None
    | Some (skel1, seq, ty_elt, u_gen) ->
      let ty = HTyp.List ty_elt in
      begin match ana_skel_fix_holes ctx u_gen renumber_empty_holes skel2 seq ty with
      | None -> None
      | Some (skel2, seq, u_gen) ->
        let skel = Skel.BinOp (NotInHole, Cons, skel1, skel2) in
        Some (skel, seq, ty, u_gen)
      end
    end
  end
and ana_skel_fix_holes ctx u_gen renumber_empty_holes skel seq ty =
  begin match skel with
  | Skel.Placeholder n ->
    begin match OperatorSeq.seq_nth n seq with
    | None -> None
    | Some en ->
      begin match bidelimited en with
      | false -> None
      | true ->
        begin match ana_fix_holes_internal ctx u_gen renumber_empty_holes en ty with
        | None -> None
        | Some (en, u_gen) ->
          begin match OperatorSeq.seq_update_nth n seq en with
          | Some seq -> Some (skel, seq, u_gen)
          | None -> None
          end
        end
      end
    end
  | Skel.BinOp (_, UHExp.Comma, skel1, skel2) ->
    begin match ty with
    | HTyp.Hole ->
      begin match ana_skel_fix_holes ctx u_gen renumber_empty_holes skel1 seq HTyp.Hole with
      | None -> None
      | Some (skel1, seq, u_gen) ->
        begin match ana_skel_fix_holes ctx u_gen renumber_empty_holes skel2 seq HTyp.Hole with
        | None -> None
        | Some (skel2, seq, u_gen) ->
          let skel = Skel.BinOp (NotInHole, UHExp.Comma, skel1, skel2) in
          Some (skel, seq, u_gen)
        end
      end
    | HTyp.Prod (ty1, ty2) ->
      let types = HTyp.get_tuple ty1 ty2 in
      let skels = UHExp.get_tuple skel1 skel2 in
      let num_types = List.length types in
      let num_skels = List.length skels in
      begin match Util.zip_eq skels types with
      | Some zipped ->
        let fixed =
          List.fold_right (fun (skel_ty : UHExp.skel_t * HTyp.t) opt_result ->
            begin match opt_result with
            | None -> None
            | Some (skels, seq, u_gen) ->
              let (skel, ty) = skel_ty in
              begin match ana_skel_fix_holes ctx u_gen renumber_empty_holes skel seq ty with
              | None -> None
              | Some (skel, seq, u_gen) ->
                Some (skel::skels, seq, u_gen)
              end
            end) zipped (Some ([], seq, u_gen)) in
        begin match fixed with
        | None -> None
        | Some (skels, seq, u_gen) ->
          begin match UHExp.make_tuple NotInHole skels with
          | None -> None
          | Some skel -> Some (skel, seq, u_gen)
          end
        end
      | None ->
        let (zipped, remainder) = HTyp.zip_with_skels skels types in
        let fixed1 =
          List.fold_right (fun (skel_ty : UHExp.skel_t * HTyp.t) opt_result ->
            begin match opt_result with
            | None -> None
            | Some (skels, seq, u_gen) ->
              let (skel, ty) = skel_ty in
              begin match ana_skel_fix_holes ctx u_gen renumber_empty_holes skel seq ty with
              | None -> None
              | Some (skel, seq, u_gen) ->
                Some (skel::skels, seq, u_gen)
              end
            end) zipped (Some ([], seq, u_gen)) in
        begin match fixed1 with
        | None -> None
        | Some (skels1, seq, u_gen) ->
          let fixed2 =
            List.fold_right (fun (skel : UHExp.skel_t) opt_result ->
              begin match opt_result with
              | None -> None
              | Some (skels, seq, u_gen) ->
                begin match syn_skel_fix_holes ctx u_gen renumber_empty_holes skel seq with
                | None -> None
                | Some (skel, seq, ty, u_gen) ->
                  Some (skel::skels, seq, u_gen)
                end
              end) remainder (Some ([], seq, u_gen)) in
          begin match fixed2 with
          | None -> None
          | Some (skels2, seq, u_gen) ->
            let skels = skels1 @ skels2 in
            let (u, u_gen) = MetaVarGen.next u_gen in
            begin match UHExp.make_tuple (InHole (WrongLength, u)) skels with
            | None -> None
            | Some skel -> Some (skel, seq, u_gen)
            end
          end
        end
      end
    | _ ->
      begin match syn_skel_fix_holes ctx u_gen renumber_empty_holes skel1 seq with
      | None -> None
      | Some (skel1, seq, _, u_gen) ->
        begin match syn_skel_fix_holes ctx u_gen renumber_empty_holes skel2 seq with
        | None -> None
        | Some (skel2, seq, _, u_gen) ->
          let (u, u_gen) = MetaVarGen.next u_gen in
          let skel = Skel.BinOp ((InHole (TypeInconsistent, u)), UHExp.Comma, skel1, skel2) in
          Some (skel, seq, u_gen)
        end
      end
    end
  | Skel.BinOp (_, UHExp.Cons, skel1, skel2) ->
    begin match HTyp.matched_list ty with
    | Some ty_elt ->
      begin match ana_skel_fix_holes ctx u_gen renumber_empty_holes skel1 seq ty_elt with
      | None -> None
      | Some (skel1, seq, u_gen) ->
        let ty_list = HTyp.List ty_elt in
        begin match ana_skel_fix_holes ctx u_gen renumber_empty_holes skel2 seq ty_list with
        | None -> None
        | Some (skel2, seq, u_gen) ->
          let skel = Skel.BinOp (NotInHole, Cons, skel1, skel2) in
          Some (skel, seq, u_gen)
        end
      end
    | None ->
      begin match syn_skel_fix_holes ctx u_gen renumber_empty_holes skel1 seq with
      | None -> None
      | Some (skel1, seq, ty_elt, u_gen) ->
        let ty_list = HTyp.List ty_elt in
        begin match ana_skel_fix_holes ctx u_gen renumber_empty_holes skel2 seq ty_list with
        | None -> None
        | Some (skel2, seq, u_gen) ->
          let (u, u_gen) = MetaVarGen.next u_gen in
          let skel = Skel.BinOp ((InHole (TypeInconsistent, u)), Cons, skel1, skel2) in
          Some (skel, seq, u_gen)
        end
      end
    end
  | Skel.BinOp (_, UHExp.Plus, _, _)
  | Skel.BinOp (_, UHExp.Times, _, _)
  | Skel.BinOp (_, UHExp.LessThan, _, _)
  | Skel.BinOp (_, UHExp.Space, _, _) ->
    begin match syn_skel_fix_holes ctx u_gen renumber_empty_holes skel seq with
    | Some (skel', seq', ty', u_gen') ->
      if HTyp.consistent ty ty' then Some (skel', seq', u_gen')
      else
        make_skel_inconsistent u_gen' skel' seq'
    | None -> None
    end
  end

let syn_fix_holes ctx u_gen e =
  syn_fix_holes_internal ctx u_gen false e

let ana_fix_holes ctx u_gen e ty =
  ana_fix_holes_internal ctx u_gen false e ty

let ana_rules_fix_holes ctx u_gen renumber_empty_holes rules pat_ty clause_ty =
  ana_rules_fix_holes_internal ctx u_gen renumber_empty_holes
    rules pat_ty clause_ty ana_fix_holes_internal

(* Only to be used on top-level expressions, as it starts hole renumbering at 0 *)
let fix_and_renumber_holes ctx e =
  syn_fix_holes_internal ctx MetaVarGen.init true e
