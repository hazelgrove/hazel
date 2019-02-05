open SemanticsCommon

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

module Coq__2 = struct
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
include Coq__2

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
| nil -> None

(* helper function for constructing a new empty hole *)
let new_EmptyHole u_gen =
  let u',u_gen' = MetaVarGen.next u_gen in
  (Tm (NotInHole, (EmptyHole u'))),u_gen'

let is_EmptyHole = function
| Tm (_, EmptyHole _) -> true
| Parenthesized _ -> false

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
    "__hole_ref__" ^ (Debug.string_of_nat lbl) ^ "__"
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

module type HOLEREFS =
 sig
  type hole_ref
  val lbl_of : hole_ref -> PaletteHoleData.hole_ref_lbl
  val type_of : hole_ref -> HTyp.t

  type 'x m_hole_ref
  val new_hole_ref : HTyp.t -> hole_ref m_hole_ref
  val bind : 'a1 m_hole_ref -> ('a1 -> 'a2 m_hole_ref) -> 'a2 m_hole_ref
  val ret : 'a1 -> 'a1 m_hole_ref

  val exec :
    'a1 m_hole_ref -> PaletteHoleData.t -> MetaVarGen.t ->
    ('a1 * PaletteHoleData.t) * MetaVarGen.t
 end

module HoleRefs =
 struct
  type hole_ref = PaletteHoleData.hole_ref_lbl * HTyp.t
  let lbl_of = function
  | lbl,_ -> lbl
  let type_of = function
  | _,ty -> ty

  type 'x m_hole_ref' =
  | NewHoleRef of HTyp.t
  | Bnd of __ m_hole_ref' * (__ -> 'x m_hole_ref')
  | Ret of 'x

  type 'x m_hole_ref = 'x m_hole_ref'

  (** val new_hole_ref : HTyp.t -> hole_ref m_hole_ref' **)

  let new_hole_ref x =
    NewHoleRef x

  (** val bind :
      'a1 m_hole_ref' -> ('a1 -> 'a2 m_hole_ref') -> 'a2 m_hole_ref' **)

  let bind x x0 =
    Bnd ((Obj.magic x), (Obj.magic x0))

  (** val ret : 'a1 -> 'a1 m_hole_ref' **)

  let ret x =
    Ret x

  (** val exec :
      'a1 m_hole_ref -> PaletteHoleData.t -> MetaVarGen.t ->
      ('a1 * PaletteHoleData.t) * MetaVarGen.t **)

  let rec exec mhr phd u_gen =
    match mhr with
    | NewHoleRef ty ->
      let q,u_gen' = PaletteHoleData.new_hole_ref u_gen phd ty in
      let lbl,phd' = q in ((Obj.magic (lbl,ty)),phd'),u_gen'
    | Bnd (mhra, f) ->
      let q,u_gen' = exec (Obj.magic mhra) phd u_gen in
      let x,phd' = q in let mhrb = Obj.magic f x in exec mhrb phd' u_gen'
    | Ret x -> (x,phd),u_gen
 end

module PaletteDefinition =
 struct
  type t = { expansion_ty : HTyp.t;
             initial_model : PaletteSerializedModel.t HoleRefs.m_hole_ref;
             to_exp : (PaletteSerializedModel.t -> Coq__2.t) }

  (** val expansion_ty : t -> HTyp.t **)

  let expansion_ty t0 =
    t0.expansion_ty

  (** val initial_model :
      t -> PaletteSerializedModel.t HoleRefs.m_hole_ref **)

  let initial_model t0 =
    t0.initial_model

  (** val to_exp : t -> PaletteSerializedModel.t -> Coq__2.t **)

  let to_exp t0 =
    t0.to_exp
 end

module PaletteCtx =
 struct
  type t = PaletteDefinition.t VarMap.t_

  type 'a t_ = (Var.t * 'a) list

  (** val empty : 'a1 t_ **)

  let empty =
    []

  (** val is_empty : 'a1 t_ -> bool **)

  let is_empty = function
  | [] -> true
  | _::_ -> false

  (** val drop : 'a1 t_ -> Var.t -> 'a1 t_ **)

  let rec drop ctx0 x =
    match ctx0 with
    | [] -> ctx0
    | p::ctx' ->
      let y,elt = p in if Var.eq x y then ctx' else (y,elt)::(drop ctx' x)

  (** val extend : 'a1 t_ -> (Var.t * 'a1) -> 'a1 t_ **)

  let extend ctx0 xa = match xa with
  | x,_ -> xa::(drop ctx0 x)

  (** val union : 'a1 t_ -> 'a1 t_ -> 'a1 t_ **)

  let union ctx1 ctx2 =
    fold_left extend ctx2 ctx1

  (** val lookup : 'a1 t_ -> Var.t -> 'a1 option **)

  let rec lookup ctx0 x =
    match ctx0 with
    | [] -> None
    | p::ctx' ->
      let y,elt = p in if Var.eq x y then Some elt else lookup ctx' x

  (** val contains : 'a1 t_ -> Var.t -> bool **)

  let contains ctx0 x =
    match lookup ctx0 x with
    | Some _ -> true
    | None -> false

  (** val map : ((Var.t * 'a1) -> 'a2) -> 'a1 t_ -> (Var.t * 'a2) list **)

  let map f xs =
    map (fun xa -> let x,_ = xa in x,(f xa)) xs

  (** val length : 'a1 t_ -> int **)

  let rec length = function
  | [] -> 0
  | _::ctx' -> ((+) 1) (length ctx')

  (** val to_list : 'a1 t_ -> (Var.t * 'a1) list **)

  let to_list ctx0 =
    ctx0
 end

module Contexts =
 struct
  type t = VarCtx.t * PaletteCtx.t

  (** val gamma : t -> VarCtx.t **)

  let gamma = function
  | gamma0,_ -> gamma0

  (** val extend_gamma : t -> (Var.t * HTyp.t) -> t **)

  let extend_gamma contexts binding =
    let gamma0,palette_ctx = contexts in
    let gamma' = VarCtx.extend gamma0 binding in gamma',palette_ctx

  (** val gamma_union : t -> VarCtx.t -> t **)

  let gamma_union contexts gamma' =
    let gamma0,palette_ctx = contexts in
    let gamma'' = VarCtx.union gamma0 gamma' in gamma'',palette_ctx

  (** val gamma_contains : t -> Var.t -> bool **)

  let gamma_contains contexts x =
    VarCtx.contains (gamma contexts) x
 end

type opseq = (t, op) OperatorSeq.opseq

(** val bidelimited : t -> bool **)

let bidelimited = function
| Tm (_, t0) ->
  (match t0 with
   | Asc (_, _) -> false
   | Let (_, _, _, _) -> false
   | Lam (_, _, _) -> false
   | OpSeq (_, _) -> false
   | _ -> true)
| Parenthesized _ -> true

(** val bidelimit : t -> t **)

let bidelimit e =
  if bidelimited e then e else Parenthesized e

(** val set_inconsistent : MetaVar.t -> t -> t **)

let rec set_inconsistent u = function
| Tm (_, e') -> Tm ((InHole (TypeInconsistent, u)), e')
| Parenthesized e' -> Parenthesized (set_inconsistent u e')

(** val make_inconsistent : MetaVarGen.t -> t -> t * MetaVarGen.t **)

let rec make_inconsistent u_gen e = match e with
| Tm (e0, e') ->
  (match e0 with
   | NotInHole ->
     let u,u_gen' = MetaVarGen.next u_gen in
     (Tm ((InHole (TypeInconsistent, u)), e')),u_gen'
   | InHole (i, _) ->
     (match i with
      | TypeInconsistent -> e,u_gen
      | WrongLength ->
        let u,u_gen' = MetaVarGen.next u_gen in
        (Tm ((InHole (TypeInconsistent, u)), e')),u_gen'))
| Parenthesized e1 ->
  let e1',u_gen' = make_inconsistent u_gen e1 in
  (Parenthesized e1'),u_gen'

(** val make_skel_inconsistent :
    MetaVarGen.t -> skel_t -> opseq -> ((skel_t * (t, op)
    OperatorSeq.opseq) * MetaVarGen.t) option **)

let make_skel_inconsistent u_gen skel seq =
  match skel with
  | Skel.Placeholder n0 ->
    (match OperatorSeq.seq_nth n0 seq with
     | Some en ->
       let en',u_gen' = make_inconsistent u_gen en in
       (match OperatorSeq.seq_update_nth n0 seq en' with
        | Some seq' -> Some ((skel,seq'),u_gen')
        | None -> None)
     | None -> None)
  | Skel.BinOp (e, op0, skel1, skel2) ->
    (match e with
     | NotInHole ->
       let u',u_gen' = MetaVarGen.next u_gen in
       Some (((Skel.BinOp ((InHole (TypeInconsistent, u')), op0, skel1,
       skel2)),seq),u_gen')
     | InHole (i, _) ->
       (match i with
        | TypeInconsistent -> Some ((skel,seq),u_gen)
        | WrongLength ->
          let u',u_gen' = MetaVarGen.next u_gen in
          Some (((Skel.BinOp ((InHole (TypeInconsistent, u')), op0,
          skel1, skel2)),seq),u_gen')))

(** val drop_outer_parentheses : t -> t **)

let rec drop_outer_parentheses e = match e with
| Tm (_, _) -> e
| Parenthesized e' -> drop_outer_parentheses e'

type type_mode =
| AnalyzedAgainst of HTyp.t
| Synthesized of HTyp.t

(** val combine_modes :
    type_mode option -> type_mode option -> type_mode option **)

let combine_modes mode1 mode2 =
  match mode1 with
  | Some _ -> mode1
  | None -> (match mode2 with
             | Some _ -> mode2
             | None -> None)

(** val syn_pat :
    unit -> Contexts.t -> UHPat.t -> (HTyp.t * Contexts.t) option **)

let rec syn_pat fuel ctx0 p =
  (fun fMore _ fKicked -> fMore ())
    (fun fuel0 ->
    match p with
    | UHPat.Pat (e, p') ->
      (match e with
       | NotInHole -> syn_pat' fuel0 ctx0 p'
       | InHole (i, _) ->
         (match i with
          | TypeInconsistent ->
            (match syn_pat' fuel0 ctx0 p' with
             | Some p0 -> let _,gamma0 = p0 in Some (HTyp.Hole,gamma0)
             | None -> None)
          | WrongLength ->
            (match p' with
             | UHPat.OpSeq (s, _) ->
               (match s with
                | Skel.Placeholder _ -> None
                | Skel.BinOp (e0, o, _, _) ->
                  (match e0 with
                   | NotInHole -> None
                   | InHole (i0, _) ->
                     (match i0 with
                      | TypeInconsistent -> None
                      | WrongLength ->
                        (match o with
                         | UHPat.Comma ->
                           (match syn_pat' fuel0 ctx0 p' with
                            | Some p0 ->
                              let _,gamma0 = p0 in Some (HTyp.Hole,gamma0)
                            | None -> None)
                         | _ -> None))))
             | _ -> None)))
    | UHPat.Parenthesized p0 -> syn_pat fuel0 ctx0 p0)
    (fun _ -> None)
    fuel

(** val syn_pat' :
    unit -> Contexts.t -> UHPat.t' -> (HTyp.t * Contexts.t) option **)

and syn_pat' fuel ctx0 p =
  (fun fMore _ fKicked -> fMore ())
    (fun fuel0 ->
    match p with
    | UHPat.Var x ->
      Var.check_valid x (Some
        (HTyp.Hole,(Contexts.extend_gamma ctx0 (x,HTyp.Hole))))
    | UHPat.NumLit _ -> Some (HTyp.Num,ctx0)
    | UHPat.BoolLit _ -> Some (HTyp.Bool,ctx0)
    | UHPat.Inj (side0, p1) ->
      (match syn_pat fuel0 ctx0 p1 with
       | Some p0 ->
         let ty1,ctx1 = p0 in
         let ty =
           match side0 with
           | L -> HTyp.Sum (ty1, HTyp.Hole)
           | R -> HTyp.Sum (HTyp.Hole, ty1)
         in
         Some (ty,ctx1)
       | None -> None)
    | UHPat.ListNil -> Some ((HTyp.List HTyp.Hole),ctx0)
    | UHPat.OpSeq (skel, seq) ->
      (match syn_skel_pat fuel0 ctx0 skel seq None with
       | Some p0 -> let p1,_ = p0 in Some p1
       | None -> None)
    | _ -> Some (HTyp.Hole,ctx0))
    (fun _ -> None)
    fuel

(** val syn_skel_pat :
    unit -> Contexts.t -> UHPat.skel_t -> UHPat.opseq -> int option ->
    ((HTyp.t * Contexts.t) * type_mode option) option **)

and syn_skel_pat fuel ctx0 skel seq monitor =
  (fun fMore _ fKicked -> fMore ())
    (fun fuel0 ->
    match skel with
    | Skel.Placeholder n0 ->
      (match OperatorSeq.seq_nth n0 seq with
       | Some pn ->
         if UHPat.bidelimited pn
         then (match syn_pat fuel0 ctx0 pn with
               | Some p ->
                 let ty,ctx1 = p in
                 let mode0 =
                   match monitor with
                   | Some n' ->
                     if eqb n0 n' then Some (Synthesized ty) else None
                   | None -> None
                 in
                 Some ((ty,ctx1),mode0)
               | None -> None)
         else None
       | None -> None)
    | Skel.BinOp (e, op0, skel1, skel2) ->
      (match e with
       | NotInHole ->
         (match op0 with
          | UHPat.Comma ->
            (match syn_skel_pat fuel0 ctx0 skel1 seq monitor with
             | Some p ->
               let p0,mode1 = p in
               let ty1,ctx1 = p0 in
               (match syn_skel_pat fuel0 ctx1 skel2 seq monitor with
                | Some p1 ->
                  let p2,mode2 = p1 in
                  let ty2,ctx2 = p2 in
                  let ty = HTyp.Prod (ty1, ty2) in
                  let mode0 = combine_modes mode1 mode2 in
                  Some ((ty,ctx2),mode0)
                | None -> None)
             | None -> None)
          | UHPat.Space ->
            (match syn_skel_pat fuel0 ctx0 skel1 seq monitor with
             | Some p ->
               let p0,mode1 = p in
               let _,ctx1 = p0 in
               (match syn_skel_pat fuel0 ctx1 skel2 seq monitor with
                | Some p1 ->
                  let p2,mode2 = p1 in
                  let _,ctx2 = p2 in
                  let ty = HTyp.Hole in
                  let mode0 = combine_modes mode1 mode2 in
                  Some ((ty,ctx2),mode0)
                | None -> None)
             | None -> None)
          | UHPat.Cons ->
            (match syn_skel_pat fuel0 ctx0 skel1 seq monitor with
             | Some p ->
               let p0,mode1 = p in
               let ty1,ctx1 = p0 in
               let ty = HTyp.List ty1 in
               (match ana_skel_pat fuel0 ctx1 skel2 seq ty monitor with
                | Some p1 ->
                  let ctx2,mode2 = p1 in
                  let mode0 = combine_modes mode1 mode2 in
                  Some ((ty,ctx2),mode0)
                | None -> None)
             | None -> None))
       | InHole (i, _) ->
         (match i with
          | TypeInconsistent ->
            let skel_not_in_hole = Skel.BinOp (NotInHole, op0, skel1,
              skel2)
            in
            (match syn_skel_pat fuel0 ctx0 skel_not_in_hole seq monitor with
             | Some p ->
               let p0,mode0 = p in
               let _,ctx1 = p0 in Some ((HTyp.Hole,ctx1),mode0)
             | None -> None)
          | WrongLength ->
            (match op0 with
             | UHPat.Comma ->
               let skel_not_in_hole = Skel.BinOp (NotInHole, op0, skel1,
                 skel2)
               in
               (match syn_skel_pat fuel0 ctx0 skel_not_in_hole seq monitor with
                | Some p ->
                  let p0,mode0 = p in
                  let _,ctx1 = p0 in Some ((HTyp.Hole,ctx1),mode0)
                | None -> None)
             | _ -> None))))
    (fun _ -> None)
    fuel

(** val ana_pat :
    unit -> Contexts.t -> UHPat.t -> HTyp.t -> Contexts.t option **)

and ana_pat fuel ctx0 p ty =
  (fun fMore _ fKicked -> fMore ())
    (fun fuel0 ->
    match p with
    | UHPat.Pat (e, p') ->
      (match e with
       | NotInHole -> ana_pat' fuel0 ctx0 p' ty
       | InHole (i, _) ->
         (match i with
          | TypeInconsistent ->
            (match syn_pat' fuel0 ctx0 p' with
             | Some p0 -> let _,ctx1 = p0 in Some ctx1
             | None -> None)
          | WrongLength ->
            (match p' with
             | UHPat.OpSeq (s, _) ->
               (match s with
                | Skel.Placeholder _ -> None
                | Skel.BinOp (e0, o, _, _) ->
                  (match e0 with
                   | NotInHole -> None
                   | InHole (i0, _) ->
                     (match i0 with
                      | TypeInconsistent -> None
                      | WrongLength ->
                        (match o with
                         | UHPat.Comma -> ana_pat' fuel0 ctx0 p' ty
                         | _ -> None))))
             | _ -> None)))
    | UHPat.Parenthesized p0 -> ana_pat fuel0 ctx0 p0 ty)
    (fun _ -> None)
    fuel

(** val ana_pat' :
    unit -> Contexts.t -> UHPat.t' -> HTyp.t -> Contexts.t option **)

and ana_pat' fuel ctx0 p ty =
  (fun fMore _ fKicked -> fMore ())
    (fun fuel0 ->
    match p with
    | UHPat.EmptyHole _ -> Some ctx0
    | UHPat.Wild -> Some ctx0
    | UHPat.Var x ->
      Var.check_valid x (Some (Contexts.extend_gamma ctx0 (x,ty)))
    | UHPat.Inj (side0, p1) ->
      (match HTyp.matched_sum ty with
       | Some p0 ->
         let tyL,tyR = p0 in
         let ty1 = pick_side side0 tyL tyR in ana_pat fuel0 ctx0 p1 ty1
       | None -> None)
    | UHPat.ListNil ->
      (match HTyp.matched_list ty with
       | Some _ -> Some ctx0
       | None -> None)
    | UHPat.OpSeq (skel, seq) ->
      (match ana_skel_pat fuel0 ctx0 skel seq ty None with
       | Some p0 -> let ctx1,_ = p0 in Some ctx1
       | None -> None)
    | _ ->
      (match syn_pat' fuel0 ctx0 p with
       | Some p0 ->
         let ty',ctx1 = p0 in
         if HTyp.consistent ty ty' then Some ctx1 else None
       | None -> None))
    (fun _ -> None)
    fuel

(** val ana_skel_pat :
    unit -> Contexts.t -> UHPat.skel_t -> UHPat.opseq -> HTyp.t -> int
    option -> (Contexts.t * type_mode option) option **)

and ana_skel_pat fuel ctx0 skel seq ty monitor =
  (fun fMore _ fKicked -> fMore ())
    (fun fuel0 ->
    match skel with
    | Skel.Placeholder n0 ->
      (match OperatorSeq.seq_nth n0 seq with
       | Some pn ->
         if UHPat.bidelimited pn
         then (match ana_pat fuel0 ctx0 pn ty with
               | Some ctx1 ->
                 let mode0 =
                   match monitor with
                   | Some n' ->
                     if eqb n0 n' then Some (AnalyzedAgainst ty) else None
                   | None -> None
                 in
                 Some (ctx1,mode0)
               | None -> None)
         else None
       | None -> None)
    | Skel.BinOp (e, op0, skel1, skel2) ->
      (match e with
       | NotInHole ->
         (match op0 with
          | UHPat.Comma ->
            (match ty with
             | HTyp.Hole ->
               (match ana_skel_pat fuel0 ctx0 skel1 seq HTyp.Hole monitor with
                | Some p ->
                  let ctx1,mode1 = p in
                  (match ana_skel_pat fuel0 ctx1 skel2 seq HTyp.Hole
                           monitor with
                   | Some p0 ->
                     let ctx2,mode2 = p0 in
                     let mode0 = combine_modes mode1 mode2 in
                     Some (ctx2,mode0)
                   | None -> None)
                | None -> None)
             | HTyp.Prod (ty1, ty2) ->
               let types = HTyp.get_tuple ty1 ty2 in
               let skels = UHPat.get_tuple skel1 skel2 in
               (match Util.zip_eq skels types with
                | Some zipped ->
                  fold_left (fun opt_result skel_ty ->
                    match opt_result with
                    | Some y ->
                      let ctx1,mode0 = y in
                      let skel0,ty0 = skel_ty in
                      (match ana_skel_pat fuel0 ctx1 skel0 seq ty0 monitor with
                       | Some p ->
                         let ctx2,mode' = p in
                         let mode1 = combine_modes mode0 mode' in
                         Some (ctx2,mode1)
                       | None -> None)
                    | None -> None) zipped (Some (ctx0,None))
                | None -> None)
             | _ -> None)
          | UHPat.Space -> None
          | UHPat.Cons ->
            (match HTyp.matched_list ty with
             | Some ty_elt ->
               (match ana_skel_pat fuel0 ctx0 skel1 seq ty_elt monitor with
                | Some p ->
                  let ctx1,mode1 = p in
                  let ty_list = HTyp.List ty_elt in
                  (match ana_skel_pat fuel0 ctx1 skel2 seq ty_list monitor with
                   | Some p0 ->
                     let ctx2,mode2 = p0 in
                     let mode0 = combine_modes mode1 mode2 in
                     Some (ctx2,mode0)
                   | None -> None)
                | None -> None)
             | None -> None))
       | InHole (i, _) ->
         (match i with
          | TypeInconsistent ->
            let skel_not_in_hole = Skel.BinOp (NotInHole, op0, skel1,
              skel2)
            in
            (match syn_skel_pat fuel0 ctx0 skel_not_in_hole seq monitor with
             | Some p ->
               let p0,mode0 = p in let _,ctx1 = p0 in Some (ctx1,mode0)
             | None -> None)
          | WrongLength ->
            (match op0 with
             | UHPat.Comma ->
               (match ty with
                | HTyp.Prod (ty1, ty2) ->
                  let types = HTyp.get_tuple ty1 ty2 in
                  let skels = UHPat.get_tuple skel1 skel2 in
                  let n_types = length types in
                  let n_skels = length skels in
                  if eqb n_types n_skels
                  then None
                  else let zipped,remainder =
                         HTyp.zip_with_skels skels types
                       in
                       let ana_zipped =
                         fold_left (fun opt_result skel_ty ->
                           match opt_result with
                           | Some y ->
                             let ctx1,mode0 = y in
                             let skel0,ty0 = skel_ty in
                             (match ana_skel_pat fuel0 ctx1 skel0 seq ty0
                                      monitor with
                              | Some p ->
                                let ctx2,mode' = p in
                                let mode1 = combine_modes mode0 mode' in
                                Some (ctx2,mode1)
                              | None -> None)
                           | None -> None) zipped (Some (ctx0,None))
                       in
                       (match ana_zipped with
                        | Some p ->
                          fold_left (fun opt_result skel0 ->
                            match opt_result with
                            | Some y ->
                              let ctx1,mode0 = y in
                              (match syn_skel_pat fuel0 ctx1 skel0 seq
                                       monitor with
                               | Some p0 ->
                                 let p1,mode' = p0 in
                                 let _,ctx2 = p1 in
                                 let mode1 = combine_modes mode0 mode' in
                                 Some (ctx2,mode1)
                               | None -> None)
                            | None -> None) remainder (Some p)
                        | None -> None)
                | _ -> None)
             | _ -> None))))
    (fun _ -> None)
    fuel

(** val ctx_for_let :
    Contexts.t -> UHPat.t -> HTyp.t -> t -> Contexts.t **)

let ctx_for_let ctx0 p ty1 e1 =
  match p with
  | UHPat.Pat (_, t0) ->
    (match t0 with
     | UHPat.Var x ->
       (match e1 with
        | Tm (_, t1) ->
          (match t1 with
           | Lam (_, _, _) ->
             (match HTyp.matched_arrow ty1 with
              | Some _ -> Contexts.extend_gamma ctx0 (x,ty1)
              | None -> ctx0)
           | _ -> ctx0)
        | Parenthesized _ -> ctx0)
     | _ -> ctx0)
  | UHPat.Parenthesized _ -> ctx0

(** val ctx_for_let' :
    Contexts.t -> UHPat.t -> HTyp.t -> t -> Contexts.t * Var.t option **)

let ctx_for_let' ctx0 p ty1 e1 =
  match p with
  | UHPat.Pat (_, t0) ->
    (match t0 with
     | UHPat.Var x ->
       (match e1 with
        | Tm (_, t1) ->
          (match t1 with
           | Lam (_, _, _) ->
             (match HTyp.matched_arrow ty1 with
              | Some _ -> (Contexts.extend_gamma ctx0 (x,ty1)),(Some x)
              | None -> ctx0,None)
           | _ -> ctx0,None)
        | Parenthesized _ -> ctx0,None)
     | _ -> ctx0,None)
  | UHPat.Parenthesized _ -> ctx0,None

(** val syn : unit -> Contexts.t -> t -> HTyp.t option **)

let rec syn fuel ctx0 e =
  (fun fMore _ fKicked -> fMore ())
    (fun fuel0 ->
    match e with
    | Tm (e0, e') ->
      (match e0 with
       | NotInHole -> syn' fuel0 ctx0 e'
       | InHole (i, _) ->
         (match i with
          | TypeInconsistent ->
            (match syn' fuel0 ctx0 e' with
             | Some _ -> Some HTyp.Hole
             | None -> None)
          | WrongLength ->
            (match e' with
             | OpSeq (s, _) ->
               (match s with
                | Skel.Placeholder _ -> None
                | Skel.BinOp (e1, o, _, _) ->
                  (match e1 with
                   | NotInHole -> None
                   | InHole (i0, _) ->
                     (match i0 with
                      | TypeInconsistent -> None
                      | WrongLength ->
                        (match o with
                         | Comma ->
                           (match syn' fuel0 ctx0 e' with
                            | Some _ -> Some HTyp.Hole
                            | None -> None)
                         | _ -> None))))
             | _ -> None)))
    | Parenthesized e1 -> syn fuel0 ctx0 e1)
    (fun _ -> None)
    fuel

(** val syn' : unit -> Contexts.t -> t' -> HTyp.t option **)

and syn' fuel ctx0 e =
  (fun fMore _ fKicked -> fMore ())
    (fun fuel0 ->
    match e with
    | Asc (e1, uty) ->
      let ty = UHTyp.expand fuel0 uty in
      if bidelimited e1
      then (match ana fuel0 ctx0 e1 ty with
            | Some _ -> Some ty
            | None -> None)
      else None
    | Var (v, x) ->
      (match v with
       | NotInVHole -> let gamma0,_ = ctx0 in VarMap.lookup gamma0 x
       | InVHole _ -> Some HTyp.Hole)
    | Let (p, ann, e1, e2) ->
      (match ann with
       | Some uty1 ->
         let ty1 = UHTyp.expand fuel0 uty1 in
         let ctx1 = ctx_for_let ctx0 p ty1 e1 in
         (match ana fuel0 ctx1 e1 ty1 with
          | Some _ ->
            (match ana_pat fuel0 ctx0 p ty1 with
             | Some ctx2 -> syn fuel0 ctx2 e2
             | None -> None)
          | None -> None)
       | None ->
         (match syn fuel0 ctx0 e1 with
          | Some ty1 ->
            (match ana_pat fuel0 ctx0 p ty1 with
             | Some ctx2 -> syn fuel0 ctx2 e2
             | None -> None)
          | None -> None))
    | Lam (p, ann, e1) ->
      let ty1 =
        match ann with
        | Some uty -> UHTyp.expand fuel0 uty
        | None -> HTyp.Hole
      in
      (match ana_pat fuel0 ctx0 p ty1 with
       | Some ctx1 ->
         (match syn fuel0 ctx1 e1 with
          | Some ty2 -> Some (HTyp.Arrow (ty1, ty2))
          | None -> None)
       | None -> None)
    | NumLit _ -> Some HTyp.Num
    | BoolLit _ -> Some HTyp.Bool
    | Inj (side0, e1) ->
      (match syn fuel0 ctx0 e1 with
       | Some ty1 ->
         (match side0 with
          | L -> Some (HTyp.Sum (ty1, HTyp.Hole))
          | R -> Some (HTyp.Sum (HTyp.Hole, ty1)))
       | None -> None)
    | Case (_, _) -> None
    | ListNil -> Some (HTyp.List HTyp.Hole)
    | EmptyHole _ -> Some HTyp.Hole
    | OpSeq (skel, seq) ->
      (match syn_skel fuel0 ctx0 skel seq None with
       | Some p -> let ty,_ = p in Some ty
       | None -> None)
    | ApPalette (name, serialized_model, hole_data) ->
      let _,palette_ctx = ctx0 in
      (match VarMap.lookup palette_ctx name with
       | Some palette_defn ->
         (match ana_hole_data fuel0 ctx0 hole_data with
          | Some _ ->
            let expansion_ty0 =
              PaletteDefinition.expansion_ty palette_defn
            in
            let to_exp0 = PaletteDefinition.to_exp palette_defn in
            let expansion = to_exp0 serialized_model in
            let _,hole_map0 = hole_data in
            let expansion_ctx =
              PaletteHoleData.extend_ctx_with_hole_map ctx0 hole_map0
            in
            (match ana fuel0 expansion_ctx expansion expansion_ty0 with
             | Some _ -> Some expansion_ty0
             | None -> None)
          | None -> None)
       | None -> None))
    (fun _ -> None)
    fuel

(** val ana_hole_data :
    unit -> Contexts.t -> PaletteHoleData.t -> unit option **)

and ana_hole_data fuel ctx0 hole_data =
  (fun fMore _ fKicked -> fMore ())
    (fun fuel0 ->
    let _,hole_map0 = hole_data in
    NatMap.fold hole_map0 (fun c v ->
      let _,ty_e = v in
      let ty,e = ty_e in
      (match c with
       | Some _ -> ana fuel0 ctx0 e ty
       | None -> None)) (Some ()))
    (fun _ -> None)
    fuel

(** val ana : unit -> Contexts.t -> t -> HTyp.t -> unit option **)

and ana fuel ctx0 e ty =
  (fun fMore _ fKicked -> fMore ())
    (fun fuel0 ->
    match e with
    | Tm (e0, e') ->
      (match e0 with
       | NotInHole -> ana' fuel0 ctx0 e' ty
       | InHole (i, _) ->
         (match i with
          | TypeInconsistent ->
            (match syn' fuel0 ctx0 e' with
             | Some _ -> Some ()
             | None -> None)
          | WrongLength ->
            (match e' with
             | OpSeq (s, _) ->
               (match s with
                | Skel.Placeholder _ -> None
                | Skel.BinOp (e1, o, _, _) ->
                  (match e1 with
                   | NotInHole -> None
                   | InHole (i0, _) ->
                     (match i0 with
                      | TypeInconsistent -> None
                      | WrongLength ->
                        (match o with
                         | Comma -> ana' fuel0 ctx0 e' ty
                         | _ -> None))))
             | _ -> None)))
    | Parenthesized e1 -> ana fuel0 ctx0 e1 ty)
    (fun _ -> None)
    fuel

(** val ana' : unit -> Contexts.t -> t' -> HTyp.t -> unit option **)

and ana' fuel ctx0 e ty =
  (fun fMore _ fKicked -> fMore ())
    (fun fuel0 ->
    match e with
    | Let (p, ann, e1, e2) ->
      (match ann with
       | Some uty1 ->
         let ty1 = UHTyp.expand fuel0 uty1 in
         let ctx1 = ctx_for_let ctx0 p ty1 e1 in
         (match ana fuel0 ctx1 e1 ty1 with
          | Some _ ->
            (match ana_pat fuel0 ctx0 p ty1 with
             | Some ctx2 -> ana fuel0 ctx2 e2 ty
             | None -> None)
          | None -> None)
       | None ->
         (match syn fuel0 ctx0 e1 with
          | Some ty1 ->
            (match ana_pat fuel0 ctx0 p ty1 with
             | Some ctx2 -> ana fuel0 ctx2 e2 ty
             | None -> None)
          | None -> None))
    | Lam (p, ann, e1) ->
      (match HTyp.matched_arrow ty with
       | Some p0 ->
         let ty1_given,ty2 = p0 in
         (match ann with
          | Some uty1 ->
            let ty1_ann = UHTyp.expand fuel0 uty1 in
            if HTyp.consistent ty1_ann ty1_given
            then (match ana_pat fuel0 ctx0 p ty1_ann with
                  | Some ctx1 -> ana fuel0 ctx1 e1 ty2
                  | None -> None)
            else None
          | None ->
            (match ana_pat fuel0 ctx0 p ty1_given with
             | Some ctx1 -> ana fuel0 ctx1 e1 ty2
             | None -> None))
       | None -> None)
    | Inj (side0, e') ->
      (match HTyp.matched_sum ty with
       | Some p ->
         let ty1,ty2 = p in ana fuel0 ctx0 e' (pick_side side0 ty1 ty2)
       | None -> None)
    | Case (e1, rules0) ->
      (match syn fuel0 ctx0 e1 with
       | Some ty1 -> ana_rules fuel0 ctx0 rules0 ty1 ty
       | None -> None)
    | ListNil ->
      (match HTyp.matched_list ty with
       | Some _ -> Some ()
       | None -> None)
    | OpSeq (skel, seq) ->
      (match ana_skel fuel0 ctx0 skel seq ty None with
       | Some _ -> Some ()
       | None -> None)
    | _ ->
      (match syn' fuel0 ctx0 e with
       | Some ty' -> if HTyp.consistent ty ty' then Some () else None
       | None -> None))
    (fun _ -> None)
    fuel

(** val ana_rules :
    unit -> Contexts.t -> rule list -> HTyp.t -> HTyp.t -> unit option **)

and ana_rules fuel ctx0 rules0 pat_ty clause_ty =
  (fun fMore _ fKicked -> fMore ())
    (fun fuel0 ->
    fold_left (fun b r ->
      match b with
      | Some _ -> ana_rule fuel0 ctx0 r pat_ty clause_ty
      | None -> None) rules0 (Some ()))
    (fun _ -> None)
    fuel

(** val ana_rule :
    unit -> Contexts.t -> rule -> HTyp.t -> HTyp.t -> unit option **)

and ana_rule fuel ctx0 rule0 pat_ty clause_ty =
  (fun fMore _ fKicked -> fMore ())
    (fun fuel0 ->
    let Rule (p, e) = rule0 in
    (match ana_pat fuel0 ctx0 p pat_ty with
     | Some ctx1 -> ana fuel0 ctx1 e clause_ty
     | None -> None))
    (fun _ -> None)
    fuel

(** val syn_skel :
    unit -> Contexts.t -> skel_t -> opseq -> int option ->
    (HTyp.t * type_mode option) option **)

and syn_skel fuel ctx0 skel seq monitor =
  (fun fMore _ fKicked -> fMore ())
    (fun fuel0 ->
    match skel with
    | Skel.Placeholder n0 ->
      (match OperatorSeq.seq_nth n0 seq with
       | Some en ->
         if bidelimited en
         then (match syn fuel0 ctx0 en with
               | Some ty ->
                 let mode0 =
                   match monitor with
                   | Some n' ->
                     if eqb n0 n' then Some (Synthesized ty) else None
                   | None -> None
                 in
                 Some (ty,mode0)
               | None -> None)
         else None
       | None -> None)
    | Skel.BinOp (e, op0, skel1, skel2) ->
      (match e with
       | NotInHole ->
         (match op0 with
          | LessThan ->
            (match ana_skel fuel0 ctx0 skel1 seq HTyp.Num monitor with
             | Some mode1 ->
               (match ana_skel fuel0 ctx0 skel2 seq HTyp.Num monitor with
                | Some mode2 ->
                  Some (HTyp.Bool,(combine_modes mode1 mode2))
                | None -> None)
             | None -> None)
          | Space ->
            (match syn_skel fuel0 ctx0 skel1 seq monitor with
             | Some p ->
               let ty1,mode1 = p in
               (match HTyp.matched_arrow ty1 with
                | Some p0 ->
                  let ty2,ty = p0 in
                  (match ana_skel fuel0 ctx0 skel2 seq ty2 monitor with
                   | Some mode2 -> Some (ty,(combine_modes mode1 mode2))
                   | None -> None)
                | None -> None)
             | None -> None)
          | Comma ->
            (match syn_skel fuel0 ctx0 skel1 seq monitor with
             | Some p ->
               let ty1,mode1 = p in
               (match syn_skel fuel0 ctx0 skel2 seq monitor with
                | Some p0 ->
                  let ty2,mode2 = p0 in
                  let mode0 = combine_modes mode1 mode2 in
                  let ty = HTyp.Prod (ty1, ty2) in Some (ty,mode0)
                | None -> None)
             | None -> None)
          | Cons ->
            (match syn_skel fuel0 ctx0 skel1 seq monitor with
             | Some p ->
               let ty1,mode1 = p in
               let ty = HTyp.List ty1 in
               (match ana_skel fuel0 ctx0 skel2 seq ty monitor with
                | Some mode2 -> Some (ty,(combine_modes mode1 mode2))
                | None -> None)
             | None -> None)
          | _ ->
            (match ana_skel fuel0 ctx0 skel1 seq HTyp.Num monitor with
             | Some mode1 ->
               (match ana_skel fuel0 ctx0 skel2 seq HTyp.Num monitor with
                | Some mode2 ->
                  Some (HTyp.Num,(combine_modes mode1 mode2))
                | None -> None)
             | None -> None))
       | InHole (i, _) ->
         (match i with
          | TypeInconsistent ->
            let skel_not_in_hole = Skel.BinOp (NotInHole, op0, skel1,
              skel2)
            in
            (match syn_skel fuel0 ctx0 skel_not_in_hole seq monitor with
             | Some p -> let _,mode0 = p in Some (HTyp.Hole,mode0)
             | None -> None)
          | WrongLength ->
            (match op0 with
             | Comma ->
               let skel_not_in_hole = Skel.BinOp (NotInHole, op0, skel1,
                 skel2)
               in
               (match syn_skel fuel0 ctx0 skel_not_in_hole seq monitor with
                | Some p -> let _,mode0 = p in Some (HTyp.Hole,mode0)
                | None -> None)
             | _ -> None))))
    (fun _ -> None)
    fuel

(** val ana_skel :
    unit -> Contexts.t -> skel_t -> opseq -> HTyp.t -> int option ->
    type_mode option option **)

and ana_skel fuel ctx0 skel seq ty monitor =
  (fun fMore _ fKicked -> fMore ())
    (fun fuel0 ->
    match skel with
    | Skel.Placeholder n0 ->
      (match OperatorSeq.seq_nth n0 seq with
       | Some en ->
         if bidelimited en
         then (match ana fuel0 ctx0 en ty with
               | Some _ ->
                 (match monitor with
                  | Some n' ->
                    if eqb n0 n'
                    then Some (Some (AnalyzedAgainst ty))
                    else Some None
                  | None -> Some None)
               | None -> None)
         else None
       | None -> None)
    | Skel.BinOp (e, op0, skel1, skel2) ->
      (match e with
       | NotInHole ->
         (match op0 with
          | Comma ->
            (match ty with
             | HTyp.Hole ->
               (match ana_skel fuel0 ctx0 skel1 seq HTyp.Hole monitor with
                | Some mode1 ->
                  (match ana_skel fuel0 ctx0 skel2 seq HTyp.Hole monitor with
                   | Some mode2 ->
                     let mode0 = combine_modes mode1 mode2 in Some mode0
                   | None -> None)
                | None -> None)
             | HTyp.Prod (ty1, ty2) ->
               let types = HTyp.get_tuple ty1 ty2 in
               let skels = get_tuple skel1 skel2 in
               (match Util.zip_eq skels types with
                | Some zipped ->
                  fold_left (fun opt_result skel_ty ->
                    match opt_result with
                    | Some mode0 ->
                      let skel0,ty0 = skel_ty in
                      (match ana_skel fuel0 ctx0 skel0 seq ty0 monitor with
                       | Some mode' ->
                         let mode1 = combine_modes mode0 mode' in
                         Some mode1
                       | None -> None)
                    | None -> None) zipped (Some None)
                | None -> None)
             | _ -> None)
          | Cons ->
            (match HTyp.matched_list ty with
             | Some ty_elt ->
               (match ana_skel fuel0 ctx0 skel1 seq ty_elt monitor with
                | Some mode1 ->
                  let ty_list = HTyp.List ty_elt in
                  (match ana_skel fuel0 ctx0 skel2 seq ty_list monitor with
                   | Some mode2 -> Some (combine_modes mode1 mode2)
                   | None -> None)
                | None -> None)
             | None -> None)
          | _ ->
            (match syn_skel fuel0 ctx0 skel seq monitor with
             | Some p ->
               let ty',mode0 = p in
               if HTyp.consistent ty ty' then Some mode0 else None
             | None -> None))
       | InHole (i, _) ->
         (match i with
          | TypeInconsistent ->
            (match syn_skel fuel0 ctx0 skel seq monitor with
             | Some p ->
               let ty',mode0 = p in
               if HTyp.consistent ty ty' then Some mode0 else None
             | None -> None)
          | WrongLength ->
            (match op0 with
             | Comma ->
               (match ty with
                | HTyp.Prod (ty1, ty2) ->
                  let types = HTyp.get_tuple ty1 ty2 in
                  let skels = get_tuple skel1 skel2 in
                  let n_types = length types in
                  let n_skels = length skels in
                  if eqb n_types n_skels
                  then None
                  else let zipped,remainder =
                         HTyp.zip_with_skels skels types
                       in
                       let ana_zipped =
                         fold_left (fun opt_result skel_ty ->
                           match opt_result with
                           | Some mode0 ->
                             let skel0,ty0 = skel_ty in
                             (match ana_skel fuel0 ctx0 skel0 seq ty0
                                      monitor with
                              | Some mode' ->
                                let mode1 = combine_modes mode0 mode' in
                                Some mode1
                              | None -> None)
                           | None -> None) zipped (Some None)
                       in
                       (match ana_zipped with
                        | Some mode0 ->
                          fold_left (fun opt_result skel0 ->
                            match opt_result with
                            | Some mode1 ->
                              (match syn_skel fuel0 ctx0 skel0 seq monitor with
                               | Some p ->
                                 let _,mode' = p in
                                 let mode2 = combine_modes mode1 mode' in
                                 Some mode2
                               | None -> None)
                            | None -> None) remainder (Some mode0)
                        | None -> None)
                | _ -> None)
             | _ -> None))))
    (fun _ -> None)
    fuel

(** val syn_pat_fix_holes :
    unit -> Contexts.t -> MetaVarGen.t -> bool -> UHPat.t ->
    (((UHPat.t * HTyp.t) * Contexts.t) * MetaVarGen.t) option **)

let rec syn_pat_fix_holes fuel ctx0 u_gen renumber_empty_holes p =
  (fun fMore _ fKicked -> fMore ())
    (fun fuel0 ->
    match p with
    | UHPat.Pat (_, p') ->
      (match syn_pat_fix_holes' fuel0 ctx0 u_gen renumber_empty_holes p' with
       | Some p0 ->
         let p1,u_gen0 = p0 in
         let p2,ctx1 = p1 in
         let p'0,ty = p2 in
         Some ((((UHPat.Pat (NotInHole, p'0)),ty),ctx1),u_gen0)
       | None -> None)
    | UHPat.Parenthesized p0 ->
      (match syn_pat_fix_holes fuel0 ctx0 u_gen renumber_empty_holes p0 with
       | Some p1 ->
         let p2,u_gen0 = p1 in
         let p3,ctx1 = p2 in
         let p4,ty = p3 in
         Some ((((UHPat.Parenthesized p4),ty),ctx1),u_gen0)
       | None -> None))
    (fun _ -> None)
    fuel

(** val syn_pat_fix_holes' :
    unit -> Contexts.t -> MetaVarGen.t -> bool -> UHPat.t' ->
    (((UHPat.t' * HTyp.t) * Contexts.t) * MetaVarGen.t) option **)

and syn_pat_fix_holes' fuel ctx0 u_gen renumber_empty_holes p =
  (fun fMore _ fKicked -> fMore ())
    (fun fuel0 ->
    match p with
    | UHPat.EmptyHole _ ->
      if renumber_empty_holes
      then let u,u_gen0 = MetaVarGen.next u_gen in
           Some ((((UHPat.EmptyHole u),HTyp.Hole),ctx0),u_gen0)
      else Some (((p,HTyp.Hole),ctx0),u_gen)
    | UHPat.Wild -> Some (((p,HTyp.Hole),ctx0),u_gen)
    | UHPat.Var x ->
      Var.check_valid x
        (let ctx1 = Contexts.extend_gamma ctx0 (x,HTyp.Hole) in
         Some (((p,HTyp.Hole),ctx1),u_gen))
    | UHPat.NumLit _ -> Some (((p,HTyp.Num),ctx0),u_gen)
    | UHPat.BoolLit _ -> Some (((p,HTyp.Bool),ctx0),u_gen)
    | UHPat.Inj (side0, p1) ->
      (match syn_pat_fix_holes fuel0 ctx0 u_gen renumber_empty_holes p1 with
       | Some p0 ->
         let p2,u_gen0 = p0 in
         let p3,ctx1 = p2 in
         let p4,ty1 = p3 in
         let ty =
           match side0 with
           | L -> HTyp.Sum (ty1, HTyp.Hole)
           | R -> HTyp.Sum (HTyp.Hole, ty1)
         in
         Some ((((UHPat.Inj (side0, p4)),ty),ctx1),u_gen0)
       | None -> None)
    | UHPat.ListNil -> Some (((p,(HTyp.List HTyp.Hole)),ctx0),u_gen)
    | UHPat.OpSeq (skel, seq) ->
      (match syn_skel_pat_fix_holes fuel0 ctx0 u_gen renumber_empty_holes
               skel seq with
       | Some p0 ->
         let p1,u_gen0 = p0 in
         let p2,ctx1 = p1 in
         let p3,ty = p2 in
         let skel0,seq0 = p3 in
         Some ((((UHPat.OpSeq (skel0, seq0)),ty),ctx1),u_gen0)
       | None -> None))
    (fun _ -> None)
    fuel

(** val syn_skel_pat_fix_holes :
    unit -> Contexts.t -> MetaVarGen.t -> bool -> UHPat.skel_t ->
    UHPat.opseq ->
    ((((UHPat.skel_t * UHPat.opseq) * HTyp.t) * Contexts.t) * MetaVarGen.t)
    option **)

and syn_skel_pat_fix_holes fuel ctx0 u_gen renumber_empty_holes skel seq =
  (fun fMore _ fKicked -> fMore ())
    (fun fuel0 ->
    match skel with
    | Skel.Placeholder n0 ->
      (match OperatorSeq.seq_nth n0 seq with
       | Some pn ->
         if UHPat.bidelimited pn
         then (match syn_pat_fix_holes fuel0 ctx0 u_gen
                       renumber_empty_holes pn with
               | Some p ->
                 let p0,u_gen0 = p in
                 let p1,ctx1 = p0 in
                 let pn0,ty = p1 in
                 (match OperatorSeq.seq_update_nth n0 seq pn0 with
                  | Some seq0 -> Some ((((skel,seq0),ty),ctx1),u_gen0)
                  | None -> None)
               | None -> None)
         else None
       | None -> None)
    | Skel.BinOp (_, o, skel1, skel2) ->
      (match o with
       | UHPat.Comma ->
         (match syn_skel_pat_fix_holes fuel0 ctx0 u_gen
                  renumber_empty_holes skel1 seq with
          | Some p ->
            let p0,u_gen0 = p in
            let p1,ctx1 = p0 in
            let p2,ty1 = p1 in
            let skel3,seq0 = p2 in
            (match syn_skel_pat_fix_holes fuel0 ctx1 u_gen0
                     renumber_empty_holes skel2 seq0 with
             | Some p3 ->
               let p4,u_gen1 = p3 in
               let p5,ctx2 = p4 in
               let p6,ty2 = p5 in
               let skel4,seq1 = p6 in
               let skel0 = Skel.BinOp (NotInHole, UHPat.Comma, skel3,
                 skel4)
               in
               let ty = HTyp.Prod (ty1, ty2) in
               Some ((((skel0,seq1),ty),ctx2),u_gen1)
             | None -> None)
          | None -> None)
       | UHPat.Space ->
         (match syn_skel_pat_fix_holes fuel0 ctx0 u_gen
                  renumber_empty_holes skel1 seq with
          | Some p ->
            let p0,u_gen0 = p in
            let p1,ctx1 = p0 in
            let p2,_ = p1 in
            let skel3,seq0 = p2 in
            (match syn_skel_pat_fix_holes fuel0 ctx1 u_gen0
                     renumber_empty_holes skel2 seq0 with
             | Some p3 ->
               let p4,u_gen1 = p3 in
               let p5,ctx2 = p4 in
               let p6,_ = p5 in
               let skel4,seq1 = p6 in
               let skel0 = Skel.BinOp (NotInHole, UHPat.Comma, skel3,
                 skel4)
               in
               let ty = HTyp.Hole in
               Some ((((skel0,seq1),ty),ctx2),u_gen1)
             | None -> None)
          | None -> None)
       | UHPat.Cons ->
         (match syn_skel_pat_fix_holes fuel0 ctx0 u_gen
                  renumber_empty_holes skel1 seq with
          | Some p ->
            let p0,u_gen0 = p in
            let p1,ctx1 = p0 in
            let p2,ty_elt = p1 in
            let skel3,seq0 = p2 in
            let ty = HTyp.List ty_elt in
            (match ana_skel_pat_fix_holes fuel0 ctx1 u_gen0
                     renumber_empty_holes skel2 seq0 ty with
             | Some p3 ->
               let p4,u_gen1 = p3 in
               let p5,ctx2 = p4 in
               let skel4,seq1 = p5 in
               let skel0 = Skel.BinOp (NotInHole, UHPat.Cons, skel3,
                 skel4)
               in
               Some ((((skel0,seq1),ty),ctx2),u_gen1)
             | None -> None)
          | None -> None)))
    (fun _ -> None)
    fuel

(** val ana_pat_fix_holes :
    unit -> Contexts.t -> MetaVarGen.t -> bool -> UHPat.t -> HTyp.t ->
    ((UHPat.t * Contexts.t) * MetaVarGen.t) option **)

and ana_pat_fix_holes fuel ctx0 u_gen renumber_empty_holes p ty =
  (fun fMore _ fKicked -> fMore ())
    (fun fuel0 ->
    match p with
    | UHPat.Pat (_, p') ->
      (match ana_pat_fix_holes' fuel0 ctx0 u_gen renumber_empty_holes p'
               ty with
       | Some p0 ->
         let p1,u_gen0 = p0 in
         let p2,ctx1 = p1 in
         let err_status0,p'0 = p2 in
         Some (((UHPat.Pat (err_status0, p'0)),ctx1),u_gen0)
       | None -> None)
    | UHPat.Parenthesized p0 ->
      (match ana_pat_fix_holes fuel0 ctx0 u_gen renumber_empty_holes p0 ty with
       | Some p1 ->
         let p2,u_gen0 = p1 in
         let p3,ctx1 = p2 in Some (((UHPat.Parenthesized p3),ctx1),u_gen0)
       | None -> None))
    (fun _ -> None)
    fuel

(** val ana_pat_fix_holes' :
    unit -> Contexts.t -> MetaVarGen.t -> bool -> UHPat.t' -> HTyp.t ->
    (((err_status * UHPat.t') * Contexts.t) * MetaVarGen.t) option **)

and ana_pat_fix_holes' fuel ctx0 u_gen renumber_empty_holes p ty =
  (fun fMore _ fKicked -> fMore ())
    (fun fuel0 ->
    match p with
    | UHPat.Wild -> Some (((NotInHole,p),ctx0),u_gen)
    | UHPat.Var x ->
      Var.check_valid x
        (let ctx1 = Contexts.extend_gamma ctx0 (x,ty) in
         Some (((NotInHole,p),ctx1),u_gen))
    | UHPat.Inj (side0, p1) ->
      (match HTyp.matched_sum ty with
       | Some p0 ->
         let tyL,tyR = p0 in
         let ty1 = pick_side side0 tyL tyR in
         (match ana_pat_fix_holes fuel0 ctx0 u_gen renumber_empty_holes
                  p1 ty1 with
          | Some p2 ->
            let p3,u_gen0 = p2 in
            let p4,ctx1 = p3 in
            Some (((NotInHole,(UHPat.Inj (side0, p4))),ctx1),u_gen0)
          | None -> None)
       | None ->
         (match syn_pat_fix_holes fuel0 ctx0 u_gen renumber_empty_holes p1 with
          | Some p0 ->
            let p2,u_gen0 = p0 in
            let p3,ctx1 = p2 in
            let p4,_ = p3 in
            let u,u_gen1 = MetaVarGen.next u_gen0 in
            Some ((((InHole (TypeInconsistent, u)),(UHPat.Inj (side0,
            p4))),ctx1),u_gen1)
          | None -> None))
    | UHPat.ListNil ->
      (match HTyp.matched_list ty with
       | Some _ -> Some (((NotInHole,p),ctx0),u_gen)
       | None ->
         let u,u_gen0 = MetaVarGen.next u_gen in
         Some ((((InHole (TypeInconsistent, u)),p),ctx0),u_gen0))
    | UHPat.OpSeq (skel, seq) ->
      (match ana_skel_pat_fix_holes fuel0 ctx0 u_gen renumber_empty_holes
               skel seq ty with
       | Some p0 ->
         let p1,u_gen0 = p0 in
         let p2,ctx1 = p1 in
         let skel0,seq0 = p2 in
         (match skel0 with
          | Skel.Placeholder _ -> None
          | Skel.BinOp (err, _, _, _) ->
            let p3 = UHPat.OpSeq (skel0, seq0) in
            Some (((err,p3),ctx1),u_gen0))
       | None -> None)
    | _ ->
      (match syn_pat_fix_holes' fuel0 ctx0 u_gen renumber_empty_holes p with
       | Some p0 ->
         let p1,u_gen0 = p0 in
         let p2,ctx1 = p1 in
         let p',ty' = p2 in
         if HTyp.consistent ty ty'
         then Some (((NotInHole,p'),ctx1),u_gen0)
         else let u,u_gen1 = MetaVarGen.next u_gen0 in
              Some ((((InHole (TypeInconsistent, u)),p'),ctx1),u_gen1)
       | None -> None))
    (fun _ -> None)
    fuel

(** val ana_skel_pat_fix_holes :
    unit -> Contexts.t -> MetaVarGen.t -> bool -> UHPat.skel_t ->
    UHPat.opseq -> HTyp.t ->
    (((UHPat.skel_t * UHPat.opseq) * Contexts.t) * MetaVarGen.t) option **)

and ana_skel_pat_fix_holes fuel ctx0 u_gen renumber_empty_holes skel seq ty =
  (fun fMore _ fKicked -> fMore ())
    (fun fuel0 ->
    match skel with
    | Skel.Placeholder n0 ->
      (match OperatorSeq.seq_nth n0 seq with
       | Some pn ->
         if UHPat.bidelimited pn
         then (match ana_pat_fix_holes fuel0 ctx0 u_gen
                       renumber_empty_holes pn ty with
               | Some p ->
                 let p0,u_gen0 = p in
                 let pn0,ctx1 = p0 in
                 (match OperatorSeq.seq_update_nth n0 seq pn0 with
                  | Some seq0 -> Some (((skel,seq0),ctx1),u_gen0)
                  | None -> None)
               | None -> None)
         else None
       | None -> None)
    | Skel.BinOp (_, o, skel1, skel2) ->
      (match o with
       | UHPat.Comma ->
         (match ty with
          | HTyp.Hole ->
            (match ana_skel_pat_fix_holes fuel0 ctx0 u_gen
                     renumber_empty_holes skel1 seq HTyp.Hole with
             | Some p ->
               let p0,u_gen0 = p in
               let p1,ctx1 = p0 in
               let skel3,seq0 = p1 in
               (match ana_skel_pat_fix_holes fuel0 ctx1 u_gen0
                        renumber_empty_holes skel2 seq0 HTyp.Hole with
                | Some p2 ->
                  let p3,u_gen1 = p2 in
                  let p4,ctx2 = p3 in
                  let skel4,seq1 = p4 in
                  let skel0 = Skel.BinOp (NotInHole, UHPat.Comma, skel3,
                    skel4)
                  in
                  Some (((skel0,seq1),ctx2),u_gen1)
                | None -> None)
             | None -> None)
          | HTyp.Prod (ty1, ty2) ->
            let types = HTyp.get_tuple ty1 ty2 in
            let skels = UHPat.get_tuple skel1 skel2 in
            (match Util.zip_eq skels types with
             | Some zipped ->
               let fixed =
                 fold_right (fun skel_ty opt_result ->
                   match opt_result with
                   | Some y ->
                     let y0,u_gen0 = y in
                     let y1,ctx1 = y0 in
                     let skels0,seq0 = y1 in
                     let skel0,ty0 = skel_ty in
                     (match ana_skel_pat_fix_holes fuel0 ctx1 u_gen0
                              renumber_empty_holes skel0 seq0 ty0 with
                      | Some p ->
                        let p0,u_gen1 = p in
                        let p1,ctx2 = p0 in
                        let skel3,seq1 = p1 in
                        Some ((((skel3::skels0),seq1),ctx2),u_gen1)
                      | None -> None)
                   | None -> None) (Some ((([],seq),ctx0),u_gen)) zipped
               in
               (match fixed with
                | Some p ->
                  let p0,u_gen0 = p in
                  let p1,ctx1 = p0 in
                  let skels0,seq0 = p1 in
                  (match UHPat.make_tuple NotInHole skels0 with
                   | Some skel0 -> Some (((skel0,seq0),ctx1),u_gen0)
                   | None -> None)
                | None -> None)
             | None ->
               let zipped,remainder = HTyp.zip_with_skels skels types in
               let fixed1 =
                 fold_right (fun skel_ty opt_result ->
                   match opt_result with
                   | Some y ->
                     let y0,u_gen0 = y in
                     let y1,ctx1 = y0 in
                     let skels0,seq0 = y1 in
                     let skel0,ty0 = skel_ty in
                     (match ana_skel_pat_fix_holes fuel0 ctx1 u_gen0
                              renumber_empty_holes skel0 seq0 ty0 with
                      | Some p ->
                        let p0,u_gen1 = p in
                        let p1,ctx2 = p0 in
                        let skel3,seq1 = p1 in
                        Some ((((skel3::skels0),seq1),ctx2),u_gen1)
                      | None -> None)
                   | None -> None) (Some ((([],seq),ctx0),u_gen)) zipped
               in
               (match fixed1 with
                | Some p ->
                  let p0,u_gen0 = p in
                  let p1,ctx1 = p0 in
                  let skels1,seq0 = p1 in
                  let fixed2 =
                    fold_right (fun skel0 opt_result ->
                      match opt_result with
                      | Some y ->
                        let y0,u_gen1 = y in
                        let y1,ctx2 = y0 in
                        let skels0,seq1 = y1 in
                        (match syn_skel_pat_fix_holes fuel0 ctx2 u_gen1
                                 renumber_empty_holes skel0 seq1 with
                         | Some p2 ->
                           let p3,u_gen2 = p2 in
                           let p4,ctx3 = p3 in
                           let p5,_ = p4 in
                           let skel3,seq2 = p5 in
                           Some ((((skel3::skels0),seq2),ctx3),u_gen2)
                         | None -> None)
                      | None -> None) (Some ((([],seq0),ctx1),u_gen0))
                      remainder
                  in
                  (match fixed2 with
                   | Some p2 ->
                     let p3,u_gen1 = p2 in
                     let p4,ctx2 = p3 in
                     let skels2,seq1 = p4 in
                     let skels0 = app skels1 skels2 in
                     let u,u_gen2 = MetaVarGen.next u_gen1 in
                     (match UHPat.make_tuple (InHole (WrongLength, u))
                              skels0 with
                      | Some skel0 -> Some (((skel0,seq1),ctx2),u_gen2)
                      | None -> None)
                   | None -> None)
                | None -> None))
          | _ ->
            (match syn_skel_pat_fix_holes fuel0 ctx0 u_gen
                     renumber_empty_holes skel1 seq with
             | Some p ->
               let p0,u_gen0 = p in
               let p1,ctx1 = p0 in
               let p2,_ = p1 in
               let skel3,seq0 = p2 in
               (match syn_skel_pat_fix_holes fuel0 ctx1 u_gen0
                        renumber_empty_holes skel2 seq0 with
                | Some p3 ->
                  let p4,u_gen1 = p3 in
                  let p5,ctx2 = p4 in
                  let p6,_ = p5 in
                  let skel4,seq1 = p6 in
                  let u,u_gen2 = MetaVarGen.next u_gen1 in
                  let skel0 = Skel.BinOp ((InHole (TypeInconsistent, u)),
                    UHPat.Comma, skel3, skel4)
                  in
                  Some (((skel0,seq1),ctx2),u_gen2)
                | None -> None)
             | None -> None))
       | UHPat.Space ->
         (match syn_skel_pat_fix_holes fuel0 ctx0 u_gen
                  renumber_empty_holes skel1 seq with
          | Some p ->
            let p0,u_gen0 = p in
            let p1,ctx1 = p0 in
            let p2,_ = p1 in
            let skel3,seq0 = p2 in
            (match syn_skel_pat_fix_holes fuel0 ctx1 u_gen0
                     renumber_empty_holes skel2 seq0 with
             | Some p3 ->
               let p4,u_gen1 = p3 in
               let p5,ctx2 = p4 in
               let p6,_ = p5 in
               let skel4,seq1 = p6 in
               let u,u_gen2 = MetaVarGen.next u_gen1 in
               let skel0 = Skel.BinOp ((InHole (TypeInconsistent, u)),
                 UHPat.Space, skel3, skel4)
               in
               Some (((skel0,seq1),ctx2),u_gen2)
             | None -> None)
          | None -> None)
       | UHPat.Cons ->
         (match HTyp.matched_list ty with
          | Some ty_elt ->
            (match ana_skel_pat_fix_holes fuel0 ctx0 u_gen
                     renumber_empty_holes skel1 seq ty_elt with
             | Some p ->
               let p0,u_gen0 = p in
               let p1,ctx1 = p0 in
               let skel3,seq0 = p1 in
               let ty_list = HTyp.List ty_elt in
               (match ana_skel_pat_fix_holes fuel0 ctx1 u_gen0
                        renumber_empty_holes skel2 seq0 ty_list with
                | Some p2 ->
                  let p3,u_gen1 = p2 in
                  let p4,ctx2 = p3 in
                  let skel4,seq1 = p4 in
                  let skel0 = Skel.BinOp (NotInHole, UHPat.Cons, skel3,
                    skel4)
                  in
                  Some (((skel0,seq1),ctx2),u_gen1)
                | None -> None)
             | None -> None)
          | None ->
            (match syn_skel_pat_fix_holes fuel0 ctx0 u_gen
                     renumber_empty_holes skel1 seq with
             | Some p ->
               let p0,u_gen0 = p in
               let p1,ctx1 = p0 in
               let p2,ty_elt = p1 in
               let skel3,seq0 = p2 in
               let ty_list = HTyp.List ty_elt in
               (match ana_skel_pat_fix_holes fuel0 ctx1 u_gen0
                        renumber_empty_holes skel2 seq0 ty_list with
                | Some p3 ->
                  let p4,u_gen1 = p3 in
                  let p5,ctx2 = p4 in
                  let skel4,seq1 = p5 in
                  let u,u_gen2 = MetaVarGen.next u_gen1 in
                  let skel0 = Skel.BinOp ((InHole (TypeInconsistent, u)),
                    UHPat.Cons, skel3, skel4)
                  in
                  Some (((skel0,seq1),ctx2),u_gen2)
                | None -> None)
             | None -> None))))
    (fun _ -> None)
    fuel

(** val ana_rule_fix_holes :
    unit -> Contexts.t -> MetaVarGen.t -> bool -> rule -> HTyp.t ->
    HTyp.t -> (unit -> Contexts.t -> MetaVarGen.t -> bool -> t -> HTyp.t
    -> (t * MetaVarGen.t) option) -> (rule * MetaVarGen.t) option **)

let ana_rule_fix_holes fuel ctx0 u_gen renumber_empty_holes rule0 pat_ty clause_ty ana_fix_holes_internal0 =
  (fun fMore _ fKicked -> fMore ())
    (fun fuel0 ->
    let Rule (pat, e) = rule0 in
    (match ana_pat_fix_holes fuel0 ctx0 u_gen renumber_empty_holes pat
             pat_ty with
     | Some p ->
       let p0,u_gen0 = p in
       let pat',ctx1 = p0 in
       (match ana_fix_holes_internal0 fuel0 ctx1 u_gen0
                renumber_empty_holes e clause_ty with
        | Some p1 -> let e',u_gen1 = p1 in Some ((Rule (pat', e')),u_gen1)
        | None -> None)
     | None -> None))
    (fun _ -> None)
    fuel

(** val ana_rules_fix_holes_internal :
    unit -> Contexts.t -> MetaVarGen.t -> bool -> rule list -> HTyp.t ->
    HTyp.t -> (unit -> Contexts.t -> MetaVarGen.t -> bool -> t -> HTyp.t
    -> (t * MetaVarGen.t) option) -> (rule list * MetaVarGen.t) option **)

let ana_rules_fix_holes_internal fuel ctx0 u_gen renumber_empty_holes rules0 pat_ty clause_ty ana_fix_holes_internal0 =
  (fun fMore _ fKicked -> fMore ())
    (fun fuel0 ->
    fold_right (fun r b ->
      match b with
      | Some y ->
        let rules1,u_gen0 = y in
        (match ana_rule_fix_holes fuel0 ctx0 u_gen0 renumber_empty_holes
                 r pat_ty clause_ty ana_fix_holes_internal0 with
         | Some p -> let r0,u_gen1 = p in Some ((r0::rules1),u_gen1)
         | None -> None)
      | None -> None) (Some ([],u_gen)) rules0)
    (fun _ -> None)
    fuel

(** val syn_fix_holes_internal :
    unit -> Contexts.t -> MetaVarGen.t -> bool -> t ->
    ((t * HTyp.t) * MetaVarGen.t) option **)

let rec syn_fix_holes_internal fuel ctx0 u_gen renumber_empty_holes e =
  (fun fMore _ fKicked -> fMore ())
    (fun fuel0 ->
    match e with
    | Tm (_, e') ->
      (match syn_fix_holes' fuel0 ctx0 u_gen renumber_empty_holes e' with
       | Some p ->
         let p0,u_gen' = p in
         let e'',ty = p0 in Some (((Tm (NotInHole, e'')),ty),u_gen')
       | None -> None)
    | Parenthesized e1 ->
      (match syn_fix_holes_internal fuel0 ctx0 u_gen renumber_empty_holes
               e1 with
       | Some p ->
         let p0,u_gen' = p in
         let e1',ty = p0 in Some (((Parenthesized e1'),ty),u_gen')
       | None -> None))
    (fun _ -> None)
    fuel

(** val syn_fix_holes' :
    unit -> Contexts.t -> MetaVarGen.t -> bool -> t' ->
    ((t' * HTyp.t) * MetaVarGen.t) option **)

and syn_fix_holes' fuel ctx0 u_gen renumber_empty_holes e =
  (fun fMore _ fKicked -> fMore ())
    (fun fuel0 ->
    match e with
    | Asc (e1, uty) ->
      if bidelimited e1
      then let ty = UHTyp.expand fuel0 uty in
           (match ana_fix_holes_internal fuel0 ctx0 u_gen
                    renumber_empty_holes e1 ty with
            | Some p ->
              let e1',u_gen' = p in Some (((Asc (e1', uty)),ty),u_gen')
            | None -> None)
      else None
    | Var (var_err_status0, x) ->
      let gamma0,_ = ctx0 in
      (match VarMap.lookup gamma0 x with
       | Some ty -> Some (((Var (NotInVHole, x)),ty),u_gen)
       | None ->
         (match var_err_status0 with
          | NotInVHole ->
            let u,u_gen0 = MetaVarGen.next u_gen in
            Some (((Var ((InVHole u), x)),HTyp.Hole),u_gen0)
          | InVHole _ -> Some ((e,HTyp.Hole),u_gen)))
    | Let (p, ann, e1, e2) ->
      (match ann with
       | Some uty1 ->
         let ty1 = UHTyp.expand fuel0 uty1 in
         let ctx1 = ctx_for_let ctx0 p ty1 e1 in
         (match ana_fix_holes_internal fuel0 ctx1 u_gen
                  renumber_empty_holes e1 ty1 with
          | Some p0 ->
            let e3,u_gen0 = p0 in
            (match ana_pat_fix_holes fuel0 ctx0 u_gen0
                     renumber_empty_holes p ty1 with
             | Some p1 ->
               let p2,u_gen1 = p1 in
               let p3,ctx2 = p2 in
               (match syn_fix_holes_internal fuel0 ctx2 u_gen1
                        renumber_empty_holes e2 with
                | Some p4 ->
                  let p5,u_gen2 = p4 in
                  let e4,ty = p5 in
                  Some (((Let (p3, ann, e3, e4)),ty),u_gen2)
                | None -> None)
             | None -> None)
          | None -> None)
       | None ->
         (match syn_fix_holes_internal fuel0 ctx0 u_gen
                  renumber_empty_holes e1 with
          | Some p0 ->
            let p1,u_gen0 = p0 in
            let e3,ty1 = p1 in
            (match ana_pat_fix_holes fuel0 ctx0 u_gen0
                     renumber_empty_holes p ty1 with
             | Some p2 ->
               let p3,u_gen1 = p2 in
               let p4,ctx1 = p3 in
               (match syn_fix_holes_internal fuel0 ctx1 u_gen1
                        renumber_empty_holes e2 with
                | Some p5 ->
                  let p6,u_gen2 = p5 in
                  let e4,ty = p6 in
                  Some (((Let (p4, ann, e3, e4)),ty),u_gen2)
                | None -> None)
             | None -> None)
          | None -> None))
    | Lam (p, ann, e1) ->
      let ty1 =
        match ann with
        | Some uty1 -> UHTyp.expand fuel0 uty1
        | None -> HTyp.Hole
      in
      (match ana_pat_fix_holes fuel0 ctx0 u_gen renumber_empty_holes p ty1 with
       | Some p0 ->
         let p1,u_gen0 = p0 in
         let p2,ctx1 = p1 in
         (match syn_fix_holes_internal fuel0 ctx1 u_gen0
                  renumber_empty_holes e1 with
          | Some p3 ->
            let p4,u_gen1 = p3 in
            let e2,ty2 = p4 in
            Some (((Lam (p2, ann, e2)),(HTyp.Arrow (ty1, ty2))),u_gen1)
          | None -> None)
       | None -> None)
    | NumLit _ -> Some ((e,HTyp.Num),u_gen)
    | BoolLit _ -> Some ((e,HTyp.Bool),u_gen)
    | Inj (side0, e1) ->
      (match syn_fix_holes_internal fuel0 ctx0 u_gen renumber_empty_holes
               e1 with
       | Some p ->
         let p0,u_gen' = p in
         let e1',ty1 = p0 in
         let e' = Inj (side0, e1') in
         let ty' =
           match side0 with
           | L -> HTyp.Sum (ty1, HTyp.Hole)
           | R -> HTyp.Sum (HTyp.Hole, ty1)
         in
         Some ((e',ty'),u_gen')
       | None -> None)
    | Case (_, _) -> None
    | ListNil -> Some ((e,(HTyp.List HTyp.Hole)),u_gen)
    | EmptyHole u ->
      if renumber_empty_holes
      then let u',u_gen'' = MetaVarGen.next u_gen in
           Some (((EmptyHole u'),HTyp.Hole),u_gen'')
      else Some (((EmptyHole u),HTyp.Hole),u_gen)
    | OpSeq (skel, seq) ->
      (match syn_skel_fix_holes fuel0 ctx0 u_gen renumber_empty_holes
               skel seq with
       | Some p ->
         let p0,u_gen0 = p in
         let p1,ty = p0 in
         let skel0,seq0 = p1 in
         (match skel0 with
          | Skel.Placeholder _ -> None
          | Skel.BinOp (_, _, _, _) ->
            Some (((OpSeq (skel0, seq0)),ty),u_gen0))
       | None -> None)
    | ApPalette (name, serialized_model, hole_data) ->
      let _,palette_ctx = ctx0 in
      (match VarMap.lookup palette_ctx name with
       | Some palette_defn ->
         (match ana_fix_holes_hole_data fuel0 ctx0 u_gen
                  renumber_empty_holes hole_data with
          | Some p ->
            let hole_data',u_gen' = p in
            let expansion_ty0 =
              PaletteDefinition.expansion_ty palette_defn
            in
            let to_exp0 = PaletteDefinition.to_exp palette_defn in
            let expansion = to_exp0 serialized_model in
            let _,hole_map0 = hole_data in
            let expansion_ctx =
              PaletteHoleData.extend_ctx_with_hole_map ctx0 hole_map0
            in
            (match ana fuel0 expansion_ctx expansion expansion_ty0 with
             | Some _ ->
               Some (((ApPalette (name, serialized_model,
                 hole_data')),expansion_ty0),u_gen')
             | None -> None)
          | None -> None)
       | None -> None))
    (fun _ -> None)
    fuel

(** val ana_fix_holes_hole_data :
    unit -> Contexts.t -> MetaVarGen.t -> bool -> PaletteHoleData.t ->
    (PaletteHoleData.t * MetaVarGen.t) option **)

and ana_fix_holes_hole_data fuel ctx0 u_gen renumber_empty_holes hole_data =
  (fun fMore _ fKicked -> fMore ())
    (fun fuel0 ->
    let next_ref,hole_map0 = hole_data in
    let init0 = NatMap.empty,u_gen in
    let hole_map_opt' =
      NatMap.fold hole_map0 (fun c v ->
        let i,ty_e = v in
        let ty,e = ty_e in
        (match c with
         | Some p ->
           let xs,u_gen0 = p in
           (match ana_fix_holes_internal fuel0 ctx0 u_gen0
                    renumber_empty_holes e ty with
            | Some p0 ->
              let e',u_gen' = p0 in
              let xs' = NatMap.extend xs (i,(ty,e')) in Some (xs',u_gen')
            | None -> None)
         | None -> None)) (Some init0)
    in
    (match hole_map_opt' with
     | Some p ->
       let hole_map',u_gen' = p in Some ((next_ref,hole_map'),u_gen')
     | None -> None))
    (fun _ -> None)
    fuel

(** val ana_fix_holes_internal :
    unit -> Contexts.t -> MetaVarGen.t -> bool -> t -> HTyp.t ->
    (t * MetaVarGen.t) option **)

and ana_fix_holes_internal fuel ctx0 u_gen renumber_empty_holes e ty =
  (fun fMore _ fKicked -> fMore ())
    (fun fuel0 ->
    match e with
    | Tm (_, e1) ->
      (match ana_fix_holes' fuel0 ctx0 u_gen renumber_empty_holes e1 ty with
       | Some p ->
         let p0,u_gen0 = p in
         let err_status0,e2 = p0 in Some ((Tm (err_status0, e2)),u_gen0)
       | None -> None)
    | Parenthesized e1 ->
      (match ana_fix_holes_internal fuel0 ctx0 u_gen renumber_empty_holes
               e1 ty with
       | Some p -> let e2,u_gen0 = p in Some ((Parenthesized e2),u_gen0)
       | None -> None))
    (fun _ -> None)
    fuel

(** val ana_fix_holes' :
    unit -> Contexts.t -> MetaVarGen.t -> bool -> t' -> HTyp.t ->
    ((err_status * t') * MetaVarGen.t) option **)

and ana_fix_holes' fuel ctx0 u_gen renumber_empty_holes e ty =
  (fun fMore _ fKicked -> fMore ())
    (fun fuel0 ->
    match e with
    | Let (p, ann, e1, e2) ->
      (match ann with
       | Some uty1 ->
         let ty1 = UHTyp.expand fuel0 uty1 in
         let ctx1 = ctx_for_let ctx0 p ty1 e1 in
         (match ana_fix_holes_internal fuel0 ctx1 u_gen
                  renumber_empty_holes e1 ty1 with
          | Some p0 ->
            let e3,u_gen0 = p0 in
            (match ana_pat_fix_holes fuel0 ctx0 u_gen0
                     renumber_empty_holes p ty1 with
             | Some p1 ->
               let p2,u_gen1 = p1 in
               let p3,ctx2 = p2 in
               (match ana_fix_holes_internal fuel0 ctx2 u_gen1
                        renumber_empty_holes e2 ty with
                | Some p4 ->
                  let e4,u_gen2 = p4 in
                  Some ((NotInHole,(Let (p3, ann, e3, e4))),u_gen2)
                | None -> None)
             | None -> None)
          | None -> None)
       | None ->
         (match syn_fix_holes_internal fuel0 ctx0 u_gen
                  renumber_empty_holes e1 with
          | Some p0 ->
            let p1,u_gen0 = p0 in
            let e3,ty1 = p1 in
            (match ana_pat_fix_holes fuel0 ctx0 u_gen0
                     renumber_empty_holes p ty1 with
             | Some p2 ->
               let p3,u_gen1 = p2 in
               let p4,ctx1 = p3 in
               (match ana_fix_holes_internal fuel0 ctx1 u_gen1
                        renumber_empty_holes e2 ty with
                | Some p5 ->
                  let e4,u_gen2 = p5 in
                  Some ((NotInHole,(Let (p4, ann, e3, e4))),u_gen2)
                | None -> None)
             | None -> None)
          | None -> None))
    | Lam (p, ann, e1) ->
      (match HTyp.matched_arrow ty with
       | Some p0 ->
         let ty1_given,ty2 = p0 in
         (match ann with
          | Some uty1 ->
            let ty1_ann = UHTyp.expand fuel0 uty1 in
            if HTyp.consistent ty1_ann ty1_given
            then (match ana_pat_fix_holes fuel0 ctx0 u_gen
                          renumber_empty_holes p ty1_ann with
                  | Some p1 ->
                    let p2,u_gen0 = p1 in
                    let p3,ctx1 = p2 in
                    (match ana_fix_holes_internal fuel0 ctx1 u_gen0
                             renumber_empty_holes e1 ty2 with
                     | Some p4 ->
                       let e2,u_gen1 = p4 in
                       Some ((NotInHole,(Lam (p3, ann, e2))),u_gen1)
                     | None -> None)
                  | None -> None)
            else (match syn_fix_holes' fuel0 ctx0 u_gen
                          renumber_empty_holes e with
                  | Some p1 ->
                    let p2,u_gen0 = p1 in
                    let e0,_ = p2 in
                    let u,u_gen1 = MetaVarGen.next u_gen0 in
                    Some (((InHole (TypeInconsistent, u)),e0),u_gen1)
                  | None -> None)
          | None ->
            (match ana_pat_fix_holes fuel0 ctx0 u_gen
                     renumber_empty_holes p ty1_given with
             | Some p1 ->
               let p2,u_gen0 = p1 in
               let p3,ctx1 = p2 in
               (match ana_fix_holes_internal fuel0 ctx1 u_gen0
                        renumber_empty_holes e1 ty2 with
                | Some p4 ->
                  let e2,u_gen1 = p4 in
                  Some ((NotInHole,(Lam (p3, ann, e2))),u_gen1)
                | None -> None)
             | None -> None))
       | None ->
         (match syn_fix_holes' fuel0 ctx0 u_gen renumber_empty_holes e with
          | Some p0 ->
            let p1,u_gen0 = p0 in
            let e0,_ = p1 in
            let u,u_gen1 = MetaVarGen.next u_gen0 in
            Some (((InHole (TypeInconsistent, u)),e0),u_gen1)
          | None -> None))
    | Inj (side0, e1) ->
      (match HTyp.matched_sum ty with
       | Some p ->
         let ty1,ty2 = p in
         (match ana_fix_holes_internal fuel0 ctx0 u_gen
                  renumber_empty_holes e1 (pick_side side0 ty1 ty2) with
          | Some p0 ->
            let e1',u_gen' = p0 in
            Some ((NotInHole,(Inj (side0, e1'))),u_gen')
          | None -> None)
       | None ->
         (match syn_fix_holes' fuel0 ctx0 u_gen renumber_empty_holes e with
          | Some p ->
            let p0,u_gen' = p in
            let e',ty' = p0 in
            if HTyp.consistent ty ty'
            then Some ((NotInHole,e'),u_gen')
            else let u,u_gen'' = MetaVarGen.next u_gen' in
                 Some (((InHole (TypeInconsistent, u)),e'),u_gen'')
          | None -> None))
    | Case (e1, rules0) ->
      (match syn_fix_holes_internal fuel0 ctx0 u_gen renumber_empty_holes
               e1 with
       | Some p ->
         let p0,u_gen0 = p in
         let e1',ty1 = p0 in
         (match ana_rules_fix_holes_internal fuel0 ctx0 u_gen0
                  renumber_empty_holes rules0 ty1 ty
                  ana_fix_holes_internal with
          | Some p1 ->
            let rules',u_gen1 = p1 in
            Some ((NotInHole,(Case (e1', rules'))),u_gen1)
          | None -> None)
       | None -> None)
    | ListNil ->
      (match HTyp.matched_list ty with
       | Some _ -> Some ((NotInHole,e),u_gen)
       | None ->
         let u,u_gen0 = MetaVarGen.next u_gen in
         Some (((InHole (TypeInconsistent, u)),e),u_gen0))
    | OpSeq (skel, seq) ->
      (match ana_skel_fix_holes fuel0 ctx0 u_gen renumber_empty_holes
               skel seq ty with
       | Some p ->
         let p0,u_gen0 = p in
         let skel0,seq0 = p0 in
         (match skel0 with
          | Skel.Placeholder _ -> None
          | Skel.BinOp (err, _, _, _) ->
            Some ((err,(OpSeq (skel0, seq0))),u_gen0))
       | None -> None)
    | _ ->
      (match syn_fix_holes' fuel0 ctx0 u_gen renumber_empty_holes e with
       | Some p ->
         let p0,u_gen' = p in
         let e',ty' = p0 in
         if HTyp.consistent ty ty'
         then Some ((NotInHole,e'),u_gen')
         else let u,u_gen'' = MetaVarGen.next u_gen' in
              Some (((InHole (TypeInconsistent, u)),e'),u_gen'')
       | None -> None))
    (fun _ -> None)
    fuel

(** val syn_skel_fix_holes :
    unit -> Contexts.t -> MetaVarGen.t -> bool -> skel_t -> opseq ->
    (((skel_t * opseq) * HTyp.t) * MetaVarGen.t) option **)

and syn_skel_fix_holes fuel ctx0 u_gen renumber_empty_holes skel seq =
  (fun fMore _ fKicked -> fMore ())
    (fun fuel0 ->
    match skel with
    | Skel.Placeholder n0 ->
      (match OperatorSeq.seq_nth n0 seq with
       | Some en ->
         if bidelimited en
         then (match syn_fix_holes_internal fuel0 ctx0 u_gen
                       renumber_empty_holes en with
               | Some p ->
                 let p0,u_gen0 = p in
                 let en0,ty = p0 in
                 (match OperatorSeq.seq_update_nth n0 seq en0 with
                  | Some seq0 -> Some (((skel,seq0),ty),u_gen0)
                  | None -> None)
               | None -> None)
         else None
       | None -> None)
    | Skel.BinOp (_, op0, skel1, skel2) ->
      (match op0 with
       | LessThan ->
         (match ana_skel_fix_holes fuel0 ctx0 u_gen renumber_empty_holes
                  skel1 seq HTyp.Num with
          | Some p ->
            let p0,u_gen0 = p in
            let skel3,seq0 = p0 in
            (match ana_skel_fix_holes fuel0 ctx0 u_gen0
                     renumber_empty_holes skel2 seq0 HTyp.Num with
             | Some p1 ->
               let p2,u_gen1 = p1 in
               let skel4,seq1 = p2 in
               Some ((((Skel.BinOp (NotInHole, op0, skel3,
               skel4)),seq1),HTyp.Bool),u_gen1)
             | None -> None)
          | None -> None)
       | Space ->
         (match syn_skel_fix_holes fuel0 ctx0 u_gen renumber_empty_holes
                  skel1 seq with
          | Some p ->
            let p0,u_gen1 = p in
            let p1,ty1 = p0 in
            let skel1',seq1 = p1 in
            (match HTyp.matched_arrow ty1 with
             | Some p2 ->
               let ty2,ty = p2 in
               (match ana_skel_fix_holes fuel0 ctx0 u_gen1
                        renumber_empty_holes skel2 seq1 ty2 with
                | Some p3 ->
                  let p4,u_gen2 = p3 in
                  let skel2',seq2 = p4 in
                  Some ((((Skel.BinOp (NotInHole, Space, skel1',
                  skel2')),seq2),ty),u_gen2)
                | None -> None)
             | None ->
               (match ana_skel_fix_holes fuel0 ctx0 u_gen1
                        renumber_empty_holes skel2 seq1 HTyp.Hole with
                | Some p2 ->
                  let p3,u_gen2 = p2 in
                  let skel2',seq2 = p3 in
                  (match make_skel_inconsistent u_gen2 skel1' seq2 with
                   | Some p4 ->
                     let p5,u_gen3 = p4 in
                     let skel1'',seq3 = p5 in
                     Some ((((Skel.BinOp (NotInHole, Space, skel1'',
                     skel2')),seq3),HTyp.Hole),u_gen3)
                   | None -> None)
                | None -> None))
          | None -> None)
       | Comma ->
         (match syn_skel_fix_holes fuel0 ctx0 u_gen renumber_empty_holes
                  skel1 seq with
          | Some p ->
            let p0,u_gen0 = p in
            let p1,ty1 = p0 in
            let skel3,seq0 = p1 in
            (match syn_skel_fix_holes fuel0 ctx0 u_gen0
                     renumber_empty_holes skel2 seq0 with
             | Some p2 ->
               let p3,u_gen1 = p2 in
               let p4,ty2 = p3 in
               let skel4,seq1 = p4 in
               let skel0 = Skel.BinOp (NotInHole, Comma, skel3, skel4) in
               let ty = HTyp.Prod (ty1, ty2) in
               Some (((skel0,seq1),ty),u_gen1)
             | None -> None)
          | None -> None)
       | Cons ->
         (match syn_skel_fix_holes fuel0 ctx0 u_gen renumber_empty_holes
                  skel1 seq with
          | Some p ->
            let p0,u_gen0 = p in
            let p1,ty_elt = p0 in
            let skel3,seq0 = p1 in
            let ty = HTyp.List ty_elt in
            (match ana_skel_fix_holes fuel0 ctx0 u_gen0
                     renumber_empty_holes skel2 seq0 ty with
             | Some p2 ->
               let p3,u_gen1 = p2 in
               let skel4,seq1 = p3 in
               let skel0 = Skel.BinOp (NotInHole, Cons, skel3, skel4) in
               Some (((skel0,seq1),ty),u_gen1)
             | None -> None)
          | None -> None)
       | _ ->
         (match ana_skel_fix_holes fuel0 ctx0 u_gen renumber_empty_holes
                  skel1 seq HTyp.Num with
          | Some p ->
            let p0,u_gen0 = p in
            let skel3,seq0 = p0 in
            (match ana_skel_fix_holes fuel0 ctx0 u_gen0
                     renumber_empty_holes skel2 seq0 HTyp.Num with
             | Some p1 ->
               let p2,u_gen1 = p1 in
               let skel4,seq1 = p2 in
               Some ((((Skel.BinOp (NotInHole, op0, skel3,
               skel4)),seq1),HTyp.Num),u_gen1)
             | None -> None)
          | None -> None)))
    (fun _ -> None)
    fuel

(** val ana_skel_fix_holes :
    unit -> Contexts.t -> MetaVarGen.t -> bool -> skel_t -> opseq ->
    HTyp.t -> ((skel_t * opseq) * MetaVarGen.t) option **)

and ana_skel_fix_holes fuel ctx0 u_gen renumber_empty_holes skel seq ty =
  (fun fMore _ fKicked -> fMore ())
    (fun fuel0 ->
    match skel with
    | Skel.Placeholder n0 ->
      (match OperatorSeq.seq_nth n0 seq with
       | Some en ->
         if bidelimited en
         then (match ana_fix_holes_internal fuel0 ctx0 u_gen
                       renumber_empty_holes en ty with
               | Some p ->
                 let en0,u_gen0 = p in
                 (match OperatorSeq.seq_update_nth n0 seq en0 with
                  | Some seq0 -> Some ((skel,seq0),u_gen0)
                  | None -> None)
               | None -> None)
         else None
       | None -> None)
    | Skel.BinOp (_, o, skel1, skel2) ->
      (match o with
       | Comma ->
         (match ty with
          | HTyp.Hole ->
            (match ana_skel_fix_holes fuel0 ctx0 u_gen
                     renumber_empty_holes skel1 seq HTyp.Hole with
             | Some p ->
               let p0,u_gen0 = p in
               let skel3,seq0 = p0 in
               (match ana_skel_fix_holes fuel0 ctx0 u_gen0
                        renumber_empty_holes skel2 seq0 HTyp.Hole with
                | Some p1 ->
                  let p2,u_gen1 = p1 in
                  let skel4,seq1 = p2 in
                  let skel0 = Skel.BinOp (NotInHole, Comma, skel3, skel4)
                  in
                  Some ((skel0,seq1),u_gen1)
                | None -> None)
             | None -> None)
          | HTyp.Prod (ty1, ty2) ->
            let types = HTyp.get_tuple ty1 ty2 in
            let skels = get_tuple skel1 skel2 in
            (match Util.zip_eq skels types with
             | Some zipped ->
               let fixed =
                 fold_right (fun skel_ty opt_result ->
                   match opt_result with
                   | Some y ->
                     let y0,u_gen0 = y in
                     let skels0,seq0 = y0 in
                     let skel0,ty0 = skel_ty in
                     (match ana_skel_fix_holes fuel0 ctx0 u_gen0
                              renumber_empty_holes skel0 seq0 ty0 with
                      | Some p ->
                        let p0,u_gen1 = p in
                        let skel3,seq1 = p0 in
                        Some (((skel3::skels0),seq1),u_gen1)
                      | None -> None)
                   | None -> None) (Some (([],seq),u_gen)) zipped
               in
               (match fixed with
                | Some p ->
                  let p0,u_gen0 = p in
                  let skels0,seq0 = p0 in
                  (match make_tuple NotInHole skels0 with
                   | Some skel0 -> Some ((skel0,seq0),u_gen0)
                   | None -> None)
                | None -> None)
             | None ->
               let zipped,remainder = HTyp.zip_with_skels skels types in
               let fixed1 =
                 fold_right (fun skel_ty opt_result ->
                   match opt_result with
                   | Some y ->
                     let y0,u_gen0 = y in
                     let skels0,seq0 = y0 in
                     let skel0,ty0 = skel_ty in
                     (match ana_skel_fix_holes fuel0 ctx0 u_gen0
                              renumber_empty_holes skel0 seq0 ty0 with
                      | Some p ->
                        let p0,u_gen1 = p in
                        let skel3,seq1 = p0 in
                        Some (((skel3::skels0),seq1),u_gen1)
                      | None -> None)
                   | None -> None) (Some (([],seq),u_gen)) zipped
               in
               (match fixed1 with
                | Some p ->
                  let p0,u_gen0 = p in
                  let skels1,seq0 = p0 in
                  let fixed2 =
                    fold_right (fun skel0 opt_result ->
                      match opt_result with
                      | Some y ->
                        let y0,u_gen1 = y in
                        let skels0,seq1 = y0 in
                        (match syn_skel_fix_holes fuel0 ctx0 u_gen1
                                 renumber_empty_holes skel0 seq1 with
                         | Some p1 ->
                           let p2,u_gen2 = p1 in
                           let p3,_ = p2 in
                           let skel3,seq2 = p3 in
                           Some (((skel3::skels0),seq2),u_gen2)
                         | None -> None)
                      | None -> None) (Some (([],seq0),u_gen0)) remainder
                  in
                  (match fixed2 with
                   | Some p1 ->
                     let p2,u_gen1 = p1 in
                     let skels2,seq1 = p2 in
                     let skels0 = app skels1 skels2 in
                     let u,u_gen2 = MetaVarGen.next u_gen1 in
                     (match make_tuple (InHole (WrongLength, u)) skels0 with
                      | Some skel0 -> Some ((skel0,seq1),u_gen2)
                      | None -> None)
                   | None -> None)
                | None -> None))
          | _ ->
            (match syn_skel_fix_holes fuel0 ctx0 u_gen
                     renumber_empty_holes skel1 seq with
             | Some p ->
               let p0,u_gen0 = p in
               let p1,_ = p0 in
               let skel3,seq0 = p1 in
               (match syn_skel_fix_holes fuel0 ctx0 u_gen0
                        renumber_empty_holes skel2 seq0 with
                | Some p2 ->
                  let p3,u_gen1 = p2 in
                  let p4,_ = p3 in
                  let skel4,seq1 = p4 in
                  let u,u_gen2 = MetaVarGen.next u_gen1 in
                  let skel0 = Skel.BinOp ((InHole (TypeInconsistent, u)),
                    Comma, skel3, skel4)
                  in
                  Some ((skel0,seq1),u_gen2)
                | None -> None)
             | None -> None))
       | Cons ->
         (match HTyp.matched_list ty with
          | Some ty_elt ->
            (match ana_skel_fix_holes fuel0 ctx0 u_gen
                     renumber_empty_holes skel1 seq ty_elt with
             | Some p ->
               let p0,u_gen0 = p in
               let skel3,seq0 = p0 in
               let ty_list = HTyp.List ty_elt in
               (match ana_skel_fix_holes fuel0 ctx0 u_gen0
                        renumber_empty_holes skel2 seq0 ty_list with
                | Some p1 ->
                  let p2,u_gen1 = p1 in
                  let skel4,seq1 = p2 in
                  let skel0 = Skel.BinOp (NotInHole, Cons, skel3, skel4)
                  in
                  Some ((skel0,seq1),u_gen1)
                | None -> None)
             | None -> None)
          | None ->
            (match syn_skel_fix_holes fuel0 ctx0 u_gen
                     renumber_empty_holes skel1 seq with
             | Some p ->
               let p0,u_gen0 = p in
               let p1,ty_elt = p0 in
               let skel3,seq0 = p1 in
               let ty_list = HTyp.List ty_elt in
               (match ana_skel_fix_holes fuel0 ctx0 u_gen0
                        renumber_empty_holes skel2 seq0 ty_list with
                | Some p2 ->
                  let p3,u_gen1 = p2 in
                  let skel4,seq1 = p3 in
                  let u,u_gen2 = MetaVarGen.next u_gen1 in
                  let skel0 = Skel.BinOp ((InHole (TypeInconsistent, u)),
                    Cons, skel3, skel4)
                  in
                  Some ((skel0,seq1),u_gen2)
                | None -> None)
             | None -> None))
       | _ ->
         (match syn_skel_fix_holes fuel0 ctx0 u_gen renumber_empty_holes
                  skel seq with
          | Some p ->
            let p0,u_gen' = p in
            let p1,ty' = p0 in
            let skel',seq' = p1 in
            if HTyp.consistent ty ty'
            then Some ((skel',seq'),u_gen')
            else make_skel_inconsistent u_gen' skel' seq'
          | None -> None)))
    (fun _ -> None)
    fuel

(** val syn_fix_holes :
    unit -> Contexts.t -> MetaVarGen.t -> t ->
    ((t * HTyp.t) * MetaVarGen.t) option **)

let syn_fix_holes fuel ctx0 u_gen e =
  syn_fix_holes_internal fuel ctx0 u_gen false e

(** val ana_fix_holes :
    unit -> Contexts.t -> MetaVarGen.t -> t -> HTyp.t ->
    (t * MetaVarGen.t) option **)

let ana_fix_holes fuel ctx0 u_gen e ty =
  ana_fix_holes_internal fuel ctx0 u_gen false e ty

(** val ana_rules_fix_holes :
    unit -> Contexts.t -> MetaVarGen.t -> bool -> rule list -> HTyp.t ->
    HTyp.t -> (rule list * MetaVarGen.t) option **)

let ana_rules_fix_holes fuel ctx0 u_gen renumber_empty_holes rules0 pat_ty clause_ty =
  ana_rules_fix_holes_internal fuel ctx0 u_gen renumber_empty_holes
    rules0 pat_ty clause_ty ana_fix_holes_internal

(** val fix_and_renumber_holes :
    unit -> Contexts.t -> t -> ((t * HTyp.t) * MetaVarGen.t) option **)

let fix_and_renumber_holes fuel ctx0 e =
  syn_fix_holes_internal fuel ctx0 MetaVarGen.init true e
