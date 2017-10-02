Require Coq.Bool.Bool. Open Scope bool.
Require Coq.Strings.String. Open Scope string_scope.
Require Coq.Arith.PeanoNat. Open Scope nat_scope.
Require Coq.Lists.List. Open Scope list_scope.

Module Core.
    Module HTyp.
      (* types with holes *)
      Inductive t : Type :=
      | Num : t
      | Arrow : t -> t -> t
      | Sum : t -> t -> t
      | Hole : t.

      (* equality *)
      Fixpoint eq (ty1 : t) (ty2 : t) : bool :=
        match (ty1, ty2) with
        | (Num, Num) => true
        | (Arrow ty1_left ty1_right, Arrow ty2_left ty2_right) =>
          andb (eq ty1_left ty2_left) (eq ty1_right ty2_right)
        | (Sum ty1_left ty1_right, Sum ty2_left ty2_right) =>
          andb (eq ty1_left ty2_left) (eq ty1_right ty2_right)
        | (Hole, Hole) => true
        | _ => false
        end.

      Theorem eq_refl : forall (x : t),
        eq x x = true.
      Proof.
        induction x; (simpl; auto with *).
      Qed.

      Theorem eq_sound : forall x y : t,
        x = y -> (eq x y = true).
      Proof.
        intros.
        rewrite -> H.
        apply eq_refl.
      Qed.

      Lemma and_proj_1 : forall b1 b2 : bool,
        (b1 && b2) = true -> b1 = true.
      Proof.
        apply Coq.Bool.Bool.andb_true_iff.
      Qed.

      Lemma and_proj_2 : forall b1 b2 : bool,
        (b1 && b2) = true -> b2 = true.
      Proof.
        apply Coq.Bool.Bool.andb_true_iff.
      Qed.

      Theorem eq_complete : forall x y : t,
        (eq x y = true) -> x = y.
      Proof.
        induction x;

        (induction y;
        ((simpl; reflexivity) || discriminate)) ||

        (induction y; (
        discriminate ||
        (simpl;
        intro H;
        assert (x1 = y1) as H0;
        [(apply IHx1;
          apply and_proj_1 with (eq x2 y2);
          exact H)
        |(assert (x2 = y2) as H1;
          [(apply IHx2;
            apply and_proj_2 with (eq x1 y1);
            exact H)
          |(rewrite <- H0;
            rewrite <- H1;
            reflexivity
          )]
        )])
        )).
      Qed.

      (* type consistency *)
      Fixpoint consistent (x y : t) : bool :=
        (eq x y) ||
        match (x, y) with
        | (Hole, _)
        | (_, Hole) =>  true
        | (Arrow x' y', Arrow x'' y'')
        | (Sum x' y', Sum x'' y'') =>
          (consistent x' x'') && (consistent y' y'')
        | _ => false
        end.

      Definition inconsistent (ty1 : t) (ty2 : t) : bool :=
        negb (consistent ty1 ty2).

      Theorem eq_implies_consistent : forall x y : t,
        ((eq x y) = true) -> ((consistent x y) = true).
      Proof.
        intuition.
        unfold consistent.
        destruct x;
        repeat rewrite -> H;
        simpl;
        reflexivity.
      Qed.

      (* matched arrow types *)
      Definition matched_arrow (ty : t) : option (t * t) :=
        match ty with
        | Arrow ty1 ty2 => Some (ty1, ty2)
        | Hole => Some (Hole, Hole)
        | _ => None
        end.

      Definition has_matched_arrow (ty : t) : bool :=
        match matched_arrow ty with
        | Some _ => true
        | None => false
        end.

      (* matched sum types *)
      Definition matched_sum (ty : t) : option (t * t) :=
        match ty with
        | Sum ty1 ty2 => Some (ty1, ty2)
        | Hole => Some (Hole, Hole)
        | _ => None
        end.

      Definition has_matched_sum (ty : t) : bool :=
        match matched_sum ty with
        | Some _ => true
        | None => false
        end.

      (* complete (i.e. does not have any holes) *)
      Fixpoint complete (ty : t) : bool :=
        match ty with
        | Num => true
        | Arrow ty1 ty2 => andb (complete ty1) (complete ty2)
        | Sum ty1 ty2 => andb (complete ty1) (complete ty2)
        | Hole => false
        end.
    End HTyp.

    Definition str_eqb (s1 s2 : Coq.Strings.String.string) : bool := 
        if Coq.Strings.String.string_dec s1 s2 then true else false.

    Module Var.
      Definition t := Coq.Strings.String.string.

      Definition equal (x : t) (y : t) : bool := str_eqb x y.
    End Var.

    Module MetaVar.
        Definition t := nat.
        Fixpoint equal (x : t) (y : t) : bool := Nat.eqb x y.

        Definition gen : Type := nat.
        Definition new_gen : gen := O.
        Definition next (x : gen) : t * gen := 
            let n := S(x) in (n, n).
    End MetaVar.

    Module Type CTX.
      Parameter t : Type.
      Parameter empty : t.
      Parameter extend : t -> Var.t * HTyp.t -> t.
      Parameter lookup : t -> Var.t -> option HTyp.t.
      Parameter map : forall U : Type, (Var.t * HTyp.t -> U) -> t -> (list U).
    End CTX.
    Module Ctx <: CTX.
      Definition t := list (Var.t * HTyp.t).

      Definition empty : t := nil.

      Definition extend (ctx : t) (x : Var.t * HTyp.t)
        : t :=
        match x with
        | (x, ty) => cons (x, ty) ctx
        end.

      Fixpoint lookup (ctx : t) (x : Var.t) : option HTyp.t :=
        match ctx with
        | nil => None
        | cons (y, ty) ctx' =>
          match Var.equal x y with
          | true => Some ty
          | false => lookup ctx' x
          end
        end.

       Definition map (U : Type) (f : Var.t * HTyp.t -> U) (xs : t) := 
         Coq.Lists.List.map f xs.
    End Ctx.

    Inductive Fuel : Type :=
      | More : Fuel -> Fuel
      | Kicked : Fuel.

    Fixpoint map_option {A B : Type} (f : A -> B) (input : option A) : option B :=
      match input with
      | Some x => Some (f x)
      | None => None
      end.

    Fixpoint flatmap_option {A B : Type} (f : A -> option B) (input : option A) : option B :=
      match input with
      | Some x => f x
      | None => None
      end.

    Definition pipe_forward {A B : Type} (x : A) (f : A -> B) : B := f x.
    Notation "X |> F" := (pipe_forward X F)
      (at level 40, left associativity) : core_scope.

    Module HExp.
      Inductive inj_side : Type :=
      | L : inj_side
      | R : inj_side.

      Definition pick_side {A : Type} (side : inj_side) (l : A) (r : A) : A :=
        match side with
        | L => l
        | R => r
        end.

      Inductive t : Type :=
      | Asc : t -> HTyp.t -> t
      | Var : Var.t -> t
      | Let : Var.t -> t -> t -> t
      | Lam : Var.t -> t -> t
      | Ap : t -> t -> t
      | NumLit : nat -> t
      | Plus : t -> t -> t
      | Inj : inj_side -> t -> t
      | Case : t -> (Var.t * t) -> (Var.t * t) -> t
      | EmptyHole : MetaVar.t -> t
      | NonEmptyHole : MetaVar.t -> t -> t.

      Fixpoint syn (fuel : Fuel) (ctx : Ctx.t) (e : t)
        : option (HTyp.t * list(MetaVar.t)) :=
        match fuel with
        | More fuel =>
          match e with
          | Asc e' ty (* SAsc *) =>
             match ana fuel ctx e' ty with 
             | None => None
             | Some us =>  Some (ty, us)
             end
          | Var x (* SVar *) => 
             let ty := Ctx.lookup ctx x in 
             match ty with 
             | None => None
             | Some ty => Some (ty, nil)
             end
          | Let x e1 e2 =>
            match syn fuel ctx e1 with 
            | None => None
            | Some (ty1, us1) => 
                let ctx' := Ctx.extend ctx (x, ty1) in 
                match syn fuel ctx' e2 with 
                | None => None
                | Some (ty, us2) => 
                    let us := us1 ++ us2 in 
                    Some (ty, us)
                end
            end
          | Ap e1 e2 (* SAp *) =>
            match syn fuel ctx e1 with
            | None => None
            | Some (ty1, us1) =>
              match HTyp.matched_arrow ty1 with
              | None => None
              | Some (ty1_left, ty1_right) =>
                  match ana fuel ctx e2 ty1_left with 
                  | None => None
                  | Some us2 =>
                      let us := us1 ++ us2 in Some
                      (ty1_right, us) 
                  end
              end
            end
          | NumLit i (* SNum *) => Some (HTyp.Num, nil)
          | Plus e1 e2 (* 3e *) =>
            match ana fuel ctx e1 HTyp.Num with 
            | None => None
            | Some us1 => 
                match ana fuel ctx e2 HTyp.Num with 
                | None => None
                | Some us2 => Some (HTyp.Num, us1 ++ us2)
                end
            end
          | EmptyHole u (* SHole *) => Some (HTyp.Hole, u :: nil)
          | NonEmptyHole u e' (* SNEHole *) =>
            match syn fuel ctx e' with 
            | None => None
            | Some (_, us) => Some (HTyp.Hole, u :: us) 
            end
          | Lam _ _ => None
          | Inj _ _ => None
          | Case _ _ _ => None
          end
        | Kicked => None
        end
      with ana (fuel : Fuel) (ctx : Ctx.t) (e : t) (ty : HTyp.t)
        : option (list(MetaVar.t)) :=
        match fuel with
        | More fuel =>
          match e with
          | Let x e1 e2 =>
              match syn fuel ctx e1 with 
              | None => None
              | Some (ty1, us1) => 
                  let ctx' := Ctx.extend ctx (x, ty1) in
                  match ana fuel ctx' e2 ty with 
                  | None => None
                  | Some us2 => Some (us1 ++ us2)
                  end
              end
          | Lam x e' (* ALam *) =>
            match HTyp.matched_arrow ty with
            | None => None
            | Some (ty1, ty2) =>
              let ctx' := Ctx.extend ctx (x, ty1) in
              ana fuel ctx' e' ty2
            end
          | Inj side e' (* 21a *) =>
            match HTyp.matched_sum ty with
            | None => None
            | Some (ty1, ty2) => ana fuel ctx e' (pick_side side ty1 ty2)
            end
          | Case e' (x, e1) (y, e2) (* 21b *) =>
            match syn fuel ctx e' with 
            | None => None
            | Some (e'_ty, us1) =>
              match HTyp.matched_sum e'_ty with
              | None => None
              | Some (ty1, ty2) =>
                let ctx1 := Ctx.extend ctx (x, ty1) in
                match ana fuel ctx1 e1 ty with
                | None => None 
                | Some us2 =>
                  let ctx2 := Ctx.extend ctx (y, ty2) in
                  match ana fuel ctx2 e2 ty with 
                  | None => None
                  | Some us3 => Some (us1 ++ us2 ++ us3)
                  end
                end
              end
            end
          | _ (* subsumption *) =>
            match syn fuel ctx e with
            | Some (ty', us) =>
                if HTyp.consistent ty ty' then (Some us) else None
            | None => None
            end
          end
        | Kicked => None
        end.

      Definition hsyn (fuel: Fuel) (ctx: Ctx.t) (e: HExp.t): option HTyp.t := 
          match HExp.syn fuel ctx e with 
          | None => None
          | Some (ty, _) => Some ty 
          end.

      Definition hana (fuel: Fuel) (ctx: Ctx.t) (e: HExp.t) (ty: HTyp.t): option unit := 
          match HExp.ana fuel ctx e ty with 
          | None => None
          | Some _ => Some tt
          end.

      Fixpoint complete (e : t) : bool :=
        match e with
        | Asc e' ty => andb (complete e') (HTyp.complete ty)
        | Var _ => true
        | Let _ e e' => andb (complete e) (complete e')
        | Lam _ e' => complete e'
        | Ap e1 e2 => andb (complete e1) (complete e2)
        | NumLit _ => true
        | Plus e1 e2 => andb (complete e1) (complete e2)
        | Inj _ e => complete e
        | Case e (x, e1) (y, e2) =>
          andb (complete e) (andb (complete e1) (complete e2))
        | EmptyHole _ => false
        | NonEmptyHole _ _ => false
        end.
    End HExp.

    Module ZTyp.
      Inductive t : Type :=
      | CursorT : HTyp.t -> t
      | LeftArrow : t -> HTyp.t -> t
      | RightArrow : HTyp.t -> t -> t
      | LeftSum : t -> HTyp.t -> t
      | RightSum : HTyp.t -> t -> t.

      Fixpoint erase (zty : t) : HTyp.t :=
        match zty with
        | CursorT ty => ty
        | LeftArrow zty1 ty2 => HTyp.Arrow (erase zty1) ty2
        | RightArrow ty1 zty2 => HTyp.Arrow ty1 (erase zty2)
        | LeftSum zty1 ty2 => HTyp.Sum (erase zty1) ty2
        | RightSum ty1 zty2 => HTyp.Sum ty1 (erase zty2)
        end.
    End ZTyp.

    Module ZExp.
      Inductive t : Type :=
      | CursorE : HExp.t -> t
      | LeftAsc : t -> HTyp.t -> t
      | RightAsc : HExp.t -> ZTyp.t -> t
      | LetZ1 : Var.t -> t -> HExp.t -> t
      | LetZ2 : Var.t -> HExp.t -> t -> t
      | LamZ : Var.t -> t -> t
      | LeftAp : t -> HExp.t -> t
      | RightAp : HExp.t -> t -> t
      | LeftPlus : t -> HExp.t -> t
      | RightPlus : HExp.t -> t -> t
      | InjZ : HExp.inj_side -> t -> t
      | CaseZ1 : t -> (Var.t * HExp.t) -> (Var.t * HExp.t) -> t
      | CaseZ2 : HExp.t -> (Var.t * t) -> (Var.t * HExp.t) -> t
      | CaseZ3 : HExp.t -> (Var.t * HExp.t) -> (Var.t * t) -> t
      | NonEmptyHoleZ : MetaVar.t -> t -> t.

      Fixpoint erase (ze : t) : HExp.t :=
        match ze with
        | CursorE e => e
        | LeftAsc ze' ty => HExp.Asc (erase ze') ty
        | RightAsc e' zty => HExp.Asc e' (ZTyp.erase zty)
        | LetZ1 x ze e => HExp.Let x (erase ze) e
        | LetZ2 x e ze => HExp.Let x e (erase ze)
        | LamZ x ze' => HExp.Lam x (erase ze')
        | LeftAp ze' e => HExp.Ap (erase ze') e
        | RightAp e ze' => HExp.Ap e (erase ze')
        | LeftPlus ze' e => HExp.Plus (erase ze') e
        | RightPlus e ze' => HExp.Plus e (erase ze')
        | InjZ side ze => HExp.Inj side (erase ze)
        | CaseZ1 ze branch1 branch2 => HExp.Case (erase ze) branch1 branch2
        | CaseZ2 e (x, ze) branch2 => HExp.Case e (x, (erase ze)) branch2
        | CaseZ3 e branch1 (y, ze) => HExp.Case e branch1 (y, (erase ze))
        | NonEmptyHoleZ u ze' => HExp.NonEmptyHole u (erase ze')
        end.
    End ZExp.

    Module Action.
      Inductive direction : Type :=
      | Child : nat -> direction
      | Parent : direction.

      Inductive shape : Type :=
      | SArrow : shape
      | SNum : shape
      | SSum : shape
      | SAsc : shape
      | SLet : Var.t -> shape
      | SVar : Var.t -> shape
      | SLam : Var.t -> shape
      | SAp : shape
      | SLit : nat -> shape
      | SPlus : shape
      | SInj : HExp.inj_side -> shape
      | SCase : Var.t -> Var.t -> shape
      | SNEHole : shape.

      Inductive t : Type :=
      | Move : direction -> t
      | Del : t
      | Construct : shape -> t
      | Finish : t.

      Fixpoint performTyp (a : t) (zty : ZTyp.t) : option ZTyp.t :=
        match (a, zty) with
        | (Move (Child 1), ZTyp.CursorT (HTyp.Arrow ty1 ty2)) =>
          Some (ZTyp.LeftArrow (ZTyp.CursorT ty1) ty2)
        | (Move (Child 2), ZTyp.CursorT (HTyp.Arrow ty1 ty2)) =>
          Some (ZTyp.RightArrow ty1 (ZTyp.CursorT ty2))
        | (Move Parent, ZTyp.LeftArrow (ZTyp.CursorT ty1) ty2) =>
          Some (ZTyp.CursorT (HTyp.Arrow ty1 ty2))
        | (Move Parent, ZTyp.RightArrow ty1 (ZTyp.CursorT ty2)) =>
          Some (ZTyp.CursorT (HTyp.Arrow ty1 ty2))
        | (Move (Child 1), ZTyp.CursorT (HTyp.Sum ty1 ty2)) =>
          Some (ZTyp.LeftSum (ZTyp.CursorT ty1) ty2)
        | (Move (Child 2), ZTyp.CursorT (HTyp.Sum ty1 ty2)) =>
          Some (ZTyp.RightSum ty1 (ZTyp.CursorT ty2))
        | (Move Parent, ZTyp.LeftSum (ZTyp.CursorT ty1) ty2) =>
          Some (ZTyp.CursorT (HTyp.Sum ty1 ty2))
        | (Move Parent, ZTyp.RightSum ty1 (ZTyp.CursorT ty2)) =>
          Some (ZTyp.CursorT (HTyp.Sum ty1 ty2))
        | (Del, ZTyp.CursorT ty) => Some (ZTyp.CursorT HTyp.Hole)
        | (Construct SArrow, ZTyp.CursorT ty) =>
          Some (ZTyp.RightArrow ty (ZTyp.CursorT HTyp.Hole))
        | (Construct SNum, ZTyp.CursorT HTyp.Hole) => Some (ZTyp.CursorT HTyp.Num)
        | (Construct SSum, ZTyp.CursorT ty) =>
          Some (ZTyp.RightSum ty (ZTyp.CursorT HTyp.Hole))
        | (_, ZTyp.LeftArrow zty1 ty2) =>
            (performTyp a zty1) |>
              flatmap_option(fun zty1' => Some (ZTyp.LeftArrow zty1' ty2))
        | (_, ZTyp.RightArrow ty1 zty2) => (performTyp a zty2) |>
              flatmap_option(fun zty2' => Some (ZTyp.RightArrow ty1 zty2'))
        | (_, ZTyp.LeftSum zty1 ty2) => (performTyp a zty1) |>
              flatmap_option(fun zty1' => Some (ZTyp.LeftSum zty1' ty2))
        | (_, ZTyp.RightSum ty1 zty2) => (performTyp a zty2) |>
              flatmap_option(fun zty2' => Some (ZTyp.RightSum ty1 zty2'))
        | _ => None
        end.

      Fixpoint performEMove (action : t) (ze : ZExp.t)
        : option ZExp.t :=
        match action with
        | Move direction =>
          match (direction, ze) with
          | (Child 1, ZExp.CursorE (HExp.Asc e ty)) =>
            Some (ZExp.LeftAsc (ZExp.CursorE e) ty)
          | (Child 2, ZExp.CursorE (HExp.Asc e ty)) =>
            Some (ZExp.RightAsc e (ZTyp.CursorT ty))
          | (Parent, ZExp.LeftAsc (ZExp.CursorE e) ty) =>
            Some (ZExp.CursorE (HExp.Asc e ty))
          | (Parent, ZExp.RightAsc e (ZTyp.CursorT ty)) =>
            Some (ZExp.CursorE (HExp.Asc e ty))
          | (Child 1, ZExp.CursorE (HExp.Let x e e')) =>
            Some (ZExp.LetZ1 x (ZExp.CursorE e) e')
          | (Child 2, ZExp.CursorE (HExp.Let x e e')) =>
            Some (ZExp.LetZ2 x e (ZExp.CursorE e'))
          | (Parent, ZExp.LetZ1 x (ZExp.CursorE e) e') =>
            Some (ZExp.CursorE (HExp.Let x e e'))
          | (Parent, ZExp.LetZ2 x e (ZExp.CursorE e')) =>
            Some (ZExp.CursorE (HExp.Let x e e'))
          | (Child 1, ZExp.CursorE (HExp.Lam x e)) =>
            Some (ZExp.LamZ x (ZExp.CursorE e))
          | (Parent, ZExp.LamZ x (ZExp.CursorE e)) =>
            Some (ZExp.CursorE (HExp.Lam x e))
          | (Child 1, ZExp.CursorE (HExp.Ap e1 e2)) =>
            Some (ZExp.LeftAp (ZExp.CursorE e1) e2)
          | (Child 2, ZExp.CursorE (HExp.Ap e1 e2)) =>
            Some (ZExp.RightAp e1 (ZExp.CursorE e2))
          | (Parent, ZExp.LeftAp (ZExp.CursorE e1) e2) =>
            Some (ZExp.CursorE (HExp.Ap e1 e2))
          | (Parent, ZExp.RightAp e1 (ZExp.CursorE e2)) =>
            Some (ZExp.CursorE (HExp.Ap e1 e2))
          | (Child 1, ZExp.CursorE (HExp.Plus e1 e2)) =>
            Some (ZExp.LeftPlus (ZExp.CursorE e1) e2)
          | (Child 2, ZExp.CursorE (HExp.Plus e1 e2)) =>
            Some (ZExp.RightPlus e1 (ZExp.CursorE e2))
          | (Parent, ZExp.LeftPlus (ZExp.CursorE e1) e2) =>
            Some (ZExp.CursorE (HExp.Plus e1 e2))
          | (Parent, ZExp.RightPlus e1 (ZExp.CursorE e2)) =>
            Some (ZExp.CursorE (HExp.Plus e1 e2))
          | (Child 1, ZExp.CursorE (HExp.Inj side e)) =>
            Some (ZExp.InjZ side (ZExp.CursorE e))
          | (Parent, ZExp.InjZ side (ZExp.CursorE e)) =>
            Some (ZExp.CursorE (HExp.Inj side e))
          | (Child 1, ZExp.CursorE (HExp.Case e branch1 branch2)) =>
            Some (ZExp.CaseZ1 (ZExp.CursorE e) branch1 branch2)
          | (Child 2, ZExp.CursorE (HExp.Case e (x, e1) branch2)) =>
            Some (ZExp.CaseZ2 e (x, (ZExp.CursorE e1)) branch2)
          | (Child 3, ZExp.CursorE (HExp.Case e branch1 (y, e2))) =>
            Some (ZExp.CaseZ3 e branch1 (y, (ZExp.CursorE e2)))
          | (Parent, ZExp.CaseZ1 (ZExp.CursorE e) branch1 branch2) =>
            Some (ZExp.CursorE (HExp.Case e branch1 branch2))
          | (Parent, ZExp.CaseZ2 e (x, ZExp.CursorE e1) branch2) =>
            Some (ZExp.CursorE (HExp.Case e (x, e1) branch2))
          | (Parent, ZExp.CaseZ3 e branch1 (y, ZExp.CursorE e2)) =>
            Some (ZExp.CursorE (HExp.Case e branch1 (y, e2)))
          | (Child 1, ZExp.CursorE (HExp.NonEmptyHole u e)) =>
            Some (ZExp.NonEmptyHoleZ u (ZExp.CursorE e))
          | (Parent, ZExp.NonEmptyHoleZ u (ZExp.CursorE e)) =>
            Some (ZExp.CursorE (HExp.NonEmptyHole u e))
          | _ => None
          end
        | _ => None
        end.

      Fixpoint performSyn (fuel: Fuel) (ctx: Ctx.t) (a: t) (ze_ty: (ZExp.t * HTyp.t) * MetaVar.gen): option ((ZExp.t * HTyp.t) * MetaVar.gen) :=
        let hsyn := HExp.hsyn in 
        let hana := HExp.hana in 
        match ze_ty with 
        | (ze, ty, u_gen) => 
        match fuel with
        | Kicked => None
        | More fuel =>
        match performEMove a ze with
        | Some ze1 => Some (ze1, ty, u_gen) (* SAMove *)
        | None =>
          match (a, (ze, ty)) with
            (* Deletion *)
            | (Del, (ZExp.CursorE e, _)) (* SADel *) =>
              let (u', u_gen') := MetaVar.next u_gen in 
              Some ((ZExp.CursorE (HExp.EmptyHole u')), HTyp.Hole, u_gen')
            (* Construction *)
            | (Construct SAsc, (ZExp.CursorE e, ty)) (* SAConAsc *) =>
              Some (ZExp.RightAsc e (ZTyp.CursorT ty), ty, u_gen)
            | (Construct (SVar x), (ZExp.CursorE (HExp.EmptyHole _), HTyp.Hole)) (* SAConVar *) =>
              match Ctx.lookup ctx x with
              | Some xty => Some (ZExp.CursorE (HExp.Var x), xty, u_gen)
              | None => None
              end
            | (Construct (SLet x), (ZExp.CursorE e, ty)) =>
              match e with
                | HExp.EmptyHole u =>
                  let (u', u_gen') := MetaVar.next u_gen in 
                  Some ((ZExp.LetZ1 x (ZExp.CursorE e) (HExp.EmptyHole u'), HTyp.Hole, u_gen'))
                | _ =>
                  let (u', u_gen') := MetaVar.next u_gen in 
                  Some ((ZExp.LetZ2 x e (ZExp.CursorE (HExp.EmptyHole u')), HTyp.Hole, u_gen'))
              end
            | (Construct (SLam x), (ZExp.CursorE (HExp.EmptyHole u), HTyp.Hole)) (* SAConLam *) =>
              let (u', u_gen') := MetaVar.next u_gen in 
              Some (ZExp.RightAsc
                  (HExp.Lam x (HExp.EmptyHole u'))
                  (ZTyp.LeftArrow (ZTyp.CursorT HTyp.Hole) HTyp.Hole),
               (HTyp.Arrow HTyp.Hole HTyp.Hole), u_gen')
            | (Construct SAp, (ZExp.CursorE e, ty)) =>
              match HTyp.matched_arrow ty with
                | Some (_, ty2) (* SAConApArr *) => 
                      let (u', u_gen') := MetaVar.next u_gen in 
                      Some (
                        (ZExp.RightAp e (ZExp.CursorE (HExp.EmptyHole u'))),
                      ty2, u_gen')
                | None (* SAConApOtw *) => 
                    let (u1, u_gen') := MetaVar.next u_gen in 
                    let (u2, u_gen'') := MetaVar.next u_gen' in 
                    Some (
                      (ZExp.RightAp (HExp.NonEmptyHole u1 e) (ZExp.CursorE (HExp.EmptyHole u2))),
                      HTyp.Hole, u_gen'')
              end
            | (Construct (SLit n), (ZExp.CursorE (HExp.EmptyHole u), HTyp.Hole)) (* SAConNumLit *) =>
                Some (ZExp.CursorE (HExp.NumLit n), HTyp.Num, u_gen)
            | (Construct SPlus, (ZExp.CursorE e, _)) =>
              if HTyp.consistent ty HTyp.Num (* SAConPlus1 *) then
                let (u', u_gen') := MetaVar.next u_gen in 
                Some ((ZExp.RightPlus e (ZExp.CursorE (HExp.EmptyHole u'))), HTyp.Num, u_gen')
              else (* SAConPlus2 *)
                let (u1, u_gen') := MetaVar.next u_gen in  
                let (u2, u_gen'') := MetaVar.next u_gen' in 
                Some ((ZExp.RightPlus (HExp.NonEmptyHole u1 e) (ZExp.CursorE (HExp.EmptyHole u2))), HTyp.Num, u_gen'')
            | (Construct (SInj side), (ZExp.CursorE (HExp.EmptyHole u), HTyp.Hole)) (* 24a *) =>
              let (u', u_gen') := MetaVar.next u_gen in 
              Some (
                (ZExp.RightAsc (HExp.Inj side (HExp.EmptyHole u')) (ZTyp.LeftSum (ZTyp.CursorT HTyp.Hole) HTyp.Hole)),
                (HTyp.Sum HTyp.Hole HTyp.Hole), u_gen'
              )
            | (Construct (SCase x y), ((ZExp.CursorE e), ty)) =>
              match HTyp.matched_sum ty with
                | Some _ (* 24b *) =>
                    let (u1, u_gen') := MetaVar.next u_gen in 
                    let (u2, u_gen'') := MetaVar.next u_gen' in 
                    let casez2 := (ZExp.CaseZ2 e
                                  (x, ZExp.CursorE (HExp.EmptyHole u1))
                                  (y, (HExp.EmptyHole u2))) in
                    Some (ZExp.LeftAsc casez2 HTyp.Hole, HTyp.Hole, u_gen'')
                | None (* 24c *) => 
                    let (u1, u_gen') := MetaVar.next u_gen in 
                    let (u2, u_gen'') := MetaVar.next u_gen' in 
                    let (u3, u_gen''') := MetaVar.next u_gen'' in 
                    Some (
                    (ZExp.LeftAsc
                      (ZExp.CaseZ1 (ZExp.NonEmptyHoleZ u1 (ZExp.CursorE e))
                                    (x, HExp.EmptyHole u2)
                                    (y, HExp.EmptyHole u3))
                      HTyp.Hole
                    ),
                    HTyp.Hole, u_gen''')
              end
            | (Construct SNEHole, (ZExp.CursorE e', ty)) (* SAConNEHole *) =>
              let (u', u_gen') := MetaVar.next u_gen in 
              Some (ZExp.NonEmptyHoleZ u' (ZExp.CursorE e'), HTyp.Hole, u_gen')
            (* Finish *)
            | (Finish, (ZExp.CursorE (HExp.NonEmptyHole u e), HTyp.Hole)) (* SAFinish *) =>
              (hsyn fuel ctx e) |>
                  map_option(fun ty' => (ZExp.CursorE e, ty', u_gen))
            (* Zipper Cases *)
            | (a, (ZExp.LeftAsc ze ty, _)) (* SAZipAsc1 *) =>
              match performAna fuel u_gen ctx a ze ty with 
              | None => None
              | Some (ze', u_gen') => Some (ZExp.LeftAsc ze' ty, ty, u_gen')
              end
            | (a, (ZExp.RightAsc e zty, _)) (* SAZipAsc2 *) =>
              match performTyp a zty with 
              | None => None
              | Some zty' => 
                  let ty' := ZTyp.erase zty' in
                  match hana fuel ctx e ty' with 
                  | None => None
                  | Some tt => 
                      Some (ZExp.RightAsc e zty', ty', u_gen)
                  end
              end
            | (a, (ZExp.LetZ1 x ze1 e2, _)) =>
              let e1 := ZExp.erase ze1 in
              (hsyn fuel ctx e1) |>
                  flatmap_option(fun ty1 => performSyn fuel ctx a (ze1, ty1, u_gen)) |>
                  flatmap_option(fun ze_ty1' =>
                    match ze_ty1' with
                    | (ze1', ty1', u_gen') =>
                      let ctx' := Ctx.extend ctx (x, ty1') in
                      (hsyn fuel ctx' e2) |>
                        map_option(fun ty2' => (ZExp.LetZ1 x ze1' e2, ty2', u_gen'))
                    end
                  )
            | (a, (ZExp.LetZ2 x e1 ze2, _)) =>
              (hsyn fuel ctx e1) |>
                  flatmap_option(fun ty1 =>
                    let ctx' := Ctx.extend ctx (x, ty1) in
                    performSyn fuel ctx' a (ze2, ty, u_gen)
                  ) |>
                  map_option(fun ze_ty2' =>
                    match ze_ty2' with
                    | (ze2', ty2', u_gen') =>
                      (ZExp.LetZ2 x e1 ze2', ty2', u_gen')
                    end)
            | (_, (ZExp.LeftAp ze1 e2, _)) (* SAZipApArr *) =>
              let e1 := ZExp.erase ze1 in
              (hsyn fuel ctx e1) |>
                  flatmap_option(fun ty2 => performSyn fuel ctx a (ze1, ty2, u_gen)) |>
                  flatmap_option(fun ze1'_ty3 =>
                    match ze1'_ty3 with
                    | (ze1', ty3, u_gen') =>
                      (HTyp.matched_arrow ty3) |>
                          flatmap_option(fun ty4_ty5 =>
                            match ty4_ty5 with
                            | (ty4, ty5) =>
                            (hana fuel ctx e2 ty5) |>
                              map_option(fun _ => (ZExp.LeftAp ze1' e2, ty5, u_gen'))
                            end
                          )
                    end
                  )
            | (_, (ZExp.RightAp e1 ze2, _)) (* SAZipApAna *) =>
              (hsyn fuel ctx e1) |>
                  flatmap_option(fun ty2 => HTyp.matched_arrow ty2) |>
                  flatmap_option(fun ty3_ty4 =>
                    match ty3_ty4 with
                    | (ty3, ty4) =>
                    (performAna fuel u_gen ctx a ze2 ty3) |>
                      map_option(fun ana_out => 
                        match ana_out with 
                        | (ze2', u_gen') => 
                            (ZExp.RightAp e1 ze2', ty4, u_gen')
                        end)
                    end
                  )
            | (_, (ZExp.LeftPlus ze1 e2, _)) (* SAZipPlus1 *) =>
              (performAna fuel u_gen ctx a ze1 HTyp.Num) |>
                  map_option(fun ana_out => 
                    match ana_out with 
                    | (ze1', u_gen') => 
                        (ZExp.LeftPlus ze1' e2, HTyp.Num, u_gen')
                    end)
            | (_, (ZExp.RightPlus e1 ze2, _)) (* SAZipPlus2 *) =>
              (performAna fuel u_gen ctx a ze2 HTyp.Num) |>
                  map_option(fun ana_out => 
                      match ana_out with 
                      | (ze2', u_gen') => 
                          (ZExp.RightPlus e1 ze2', HTyp.Num, u_gen')
                      end)
            | (_, (ZExp.NonEmptyHoleZ u ze1, _)) (* SAZipHole *) =>
              let e1 := ZExp.erase ze1 in
              (hsyn fuel ctx e1) |>
                  flatmap_option(fun ty1 => performSyn fuel ctx a (ze1, ty1, u_gen)) |>
                  map_option(fun syn_out =>
                    match syn_out with
                    | (ze1', _, u_gen') => (ZExp.NonEmptyHoleZ u ze1', HTyp.Hole, u_gen')
                    end)
            | _ => None
            end
          end
          end
        end
      with performAna (fuel: Fuel) (u_gen : MetaVar.gen) (ctx: Ctx.t) (a: t) (ze: ZExp.t) (ty: HTyp.t): option (ZExp.t * MetaVar.gen) :=
        match fuel with
        | Kicked => None
        | More fuel =>
          match a with
          | Move _ (* AAMove *) =>
            (* try to use the non-zipper move actions *)
            match performEMove a ze with
            | Some x => Some (x, u_gen)
            | None =>
                (* if it doesn't work, keep going --
                 * it'll hit the subsumption rule at the bottom *)
                performAna_postMoveCheck fuel u_gen ctx a ze ty
            end
          | _ => performAna_postMoveCheck fuel u_gen ctx a ze ty
          end
        end
      with performAna_postMoveCheck (fuel : Fuel) (u_gen : MetaVar.gen) (ctx : Ctx.t) (a : t) (ze : ZExp.t) (ty : HTyp.t) : option (ZExp.t * MetaVar.gen) :=
        match fuel with
        | Kicked => None
        | More fuel =>
        match (a, ze, ty) with
        (* Deletion *)
        | (Del, ZExp.CursorE e, _) (* AADel *) =>
          let (u', u_gen') := MetaVar.next u_gen in 
          Some (ZExp.CursorE (HExp.EmptyHole u'), u_gen')
        (* Construction *)
        | (Construct SAsc, ZExp.CursorE e, _) (* AAConAsc *) =>
          Some (ZExp.RightAsc e (ZTyp.CursorT ty), u_gen)
        | (Construct (SVar x), ZExp.CursorE (HExp.EmptyHole u), ty) (* SAConVar *) =>
          let ty_valid := match Ctx.lookup ctx x with
            | Some xty => HTyp.inconsistent ty xty
            | None => false end
          in
            if ty_valid then
              Some (ZExp.NonEmptyHoleZ u (ZExp.CursorE (HExp.Var x)), u_gen) else
              performAna_subsume fuel u_gen ctx a ze ty
        | (Construct (SLet x), ZExp.CursorE (HExp.EmptyHole u), _) =>
          let (u', u_gen') := MetaVar.next u_gen in 
          Some (ZExp.LetZ1 x ze (HExp.EmptyHole u'), u_gen')
        | (Construct (SLam x), ZExp.CursorE (HExp.EmptyHole u), ty) =>
          match HTyp.matched_arrow ty with
            | Some _ (* AAConLam1 *) => Some (ZExp.LamZ x ze, u_gen)
            | None (* AAConLam2 *) => 
                let (u', u_gen') := MetaVar.next u_gen in 
                Some (
                    ZExp.NonEmptyHoleZ u (
                      ZExp.RightAsc
                        (HExp.Lam x (HExp.EmptyHole u'))
                        (ZTyp.LeftArrow (ZTyp.CursorT HTyp.Hole) HTyp.Hole)
                      ), u_gen')
          end
        | (Construct (SLit n), ZExp.CursorE (HExp.EmptyHole u), ty) (* AAConNumLit *) =>
          if HTyp.inconsistent ty HTyp.Num then
            Some (ZExp.NonEmptyHoleZ u (ZExp.CursorE (HExp.NumLit n)), u_gen)
          else
            performAna_subsume fuel u_gen ctx a ze ty
        | (Construct (SInj side), ZExp.CursorE (HExp.EmptyHole u), ty) =>
          match HTyp.matched_sum ty with
            | Some _ (* 23a *) => Some (ZExp.InjZ side ze, u_gen)
            | None (* 23b *) => 
                let (u', u_gen') := MetaVar.next u_gen in 
                Some (
                    ZExp.NonEmptyHoleZ u (
                      ZExp.RightAsc
                        (HExp.Inj side (HExp.EmptyHole u'))
                        (ZTyp.LeftSum (ZTyp.CursorT HTyp.Hole) HTyp.Hole)
                    ), u_gen')
          end
        | (Construct (SCase x y), ZExp.CursorE (HExp.EmptyHole u), ty) (* 23c *) =>
          let (u1, u_gen') := MetaVar.next u_gen in 
          let (u2, u_gen'') := MetaVar.next u_gen' in 
          Some (
            ZExp.CaseZ1
              ze 
              (x, HExp.EmptyHole u1)
              (y, HExp.EmptyHole u2),
            u_gen''
          )
        (* Finishing *)
        | (Finish, ZExp.CursorE (HExp.NonEmptyHole u e), _) (* AAFinish *) =>
          (HExp.hana fuel ctx e ty) |>
              map_option(fun _ => (ZExp.CursorE e, u_gen))
        (* Zipper Cases *)
        | (_, ZExp.LetZ1 x ze1 e2, _) =>
          let e1 := ZExp.erase ze1 in
          (HExp.hsyn fuel ctx e1) |>
              flatmap_option(fun ty1 => performSyn fuel ctx a (ze1, ty1, u_gen)) |>
              flatmap_option(fun syn_out =>
                match syn_out with
                | (ze1', ty1', u_gen') =>
                  let ctx' := Ctx.extend ctx (x, ty1') in
                  (HExp.hana fuel ctx' e2 ty) |>
                    map_option(fun _ => (ZExp.LetZ1 x ze1' e2, u_gen'))
                end)
        | (_, ZExp.LetZ2 x e1 ze2, _) =>
          (HExp.hsyn fuel ctx e1) |>
              flatmap_option(fun ty1 =>
                let ctx' := Ctx.extend ctx (x, ty1) in
                performAna fuel u_gen ctx' a ze2 ty) |>
              map_option(fun ana_out => 
                match ana_out with 
                | (ze2', u_gen') => (ZExp.LetZ2 x e1 ze2', u_gen')
                end)
        | (_, ZExp.LamZ x ze', ty) (* AAZipLam *) =>
          (HTyp.matched_arrow ty) |>
              flatmap_option(fun ty1_ty2 => match ty1_ty2 with
              | (ty1, ty2) =>
                let ctx' := Ctx.extend ctx (x, ty1) in
                performAna fuel u_gen ctx' a ze' ty2
              end) |>
              map_option(fun ana_out => 
                match ana_out with 
                | (ze'', u_gen') => (ZExp.LamZ x ze'', u_gen')
                end)
        | (_, ZExp.InjZ side ze, ty) (* 23d *) =>
          (HTyp.matched_sum ty) |>
              flatmap_option(fun ty1_ty2 => match ty1_ty2 with
              | (ty1, ty2) =>
                performAna fuel u_gen ctx a ze
                  (HExp.pick_side side ty1 ty2)
              end) |>
              map_option(fun ana_out => 
                match ana_out with 
                | (ze', u_gen') => (ZExp.InjZ HExp.L ze', u_gen')
                end)
        | (_, ZExp.CaseZ1 ze (x, e1) (y, e2), ty) (* 23e *) =>
          let e0 := ZExp.erase ze in
          match HExp.hsyn fuel ctx e0 with 
          | None => None
          | Some ty0 => 
              match performSyn fuel ctx a (ze, ty0, u_gen) with 
              | None => None
              | Some (ze', ty0', u_gen') => 
                match HTyp.matched_sum ty0' with 
                | None => None
                | Some (ty1, ty2) =>
                  let ctx1 := Ctx.extend ctx (x, ty1) in
                  match HExp.hana fuel ctx1 e1 ty with 
                  | None => None
                  | Some _ => 
                      let ctx2 := Ctx.extend ctx (y, ty2) in
                      match HExp.hana fuel ctx2 e2 ty with 
                      | None => None
                      | Some _ => 
                          Some (ZExp.CaseZ1 ze' (x, e1) (y, e2), u_gen')
                      end
                  end
                end
              end
          end
        | (_, ZExp.CaseZ2 e0 (x, ze1) (y, e2), ty) (* 23f *) =>
          (HExp.hsyn fuel ctx e0) |>
              flatmap_option(fun ty0 => HTyp.matched_sum ty0) |>
              flatmap_option(fun ty1_ty2 => match ty1_ty2 with
              | (ty1, ty2) =>
                let ctx1 := Ctx.extend ctx (x, ty1) in
                performAna fuel u_gen ctx1 a ze1 ty
              end) |>
              map_option(fun ana_out => 
                  match ana_out with 
                  | (ze1', u_gen') => (ZExp.CaseZ2 e0 (x, ze1') (y, e2), u_gen')
                  end)
        | (_, ZExp.CaseZ3 e0 (x, e1) (y, ze2), ty) (* 23g *) =>
          (HExp.hsyn fuel ctx e0) |>
              flatmap_option(fun ty0 => HTyp.matched_sum ty0) |>
              flatmap_option(fun ty1_ty2 => match ty1_ty2 with
              | (ty1, ty2) =>
                let ctx2 := Ctx.extend ctx (y, ty2) in
                performAna fuel u_gen ctx2 a ze2 ty
              end) |>
              map_option(fun ana_out => 
                  match ana_out with 
                  | (ze2', u_gen') => (ZExp.CaseZ3 e0 (x, e1) (y, ze2'), u_gen')
                  end)
        (* Subsumption *)
        | _ (* AASubsume *) =>
          performAna_subsume fuel u_gen ctx a ze ty
        end
        end
      with performAna_subsume (fuel : Fuel) (u_gen : MetaVar.gen) (ctx : Ctx.t) (a : t) (ze : ZExp.t) (ty : HTyp.t) : option (ZExp.t * MetaVar.gen) :=
        match fuel with
        | Kicked => None
        | More fuel =>
          let e := ZExp.erase ze in
          (HExp.hsyn fuel ctx e) |>
            flatmap_option(fun ty1 => performSyn fuel ctx a (ze, ty1, u_gen)) |>
            flatmap_option(fun syn_out =>
              match syn_out with
              | (ze', ty1', u_gen') =>
                if HTyp.consistent ty ty1' then Some (ze', u_gen') else None
              end
            )
        end.
    End Action.

    Module Dynamics.
        Module Environment.
            (* polymorphic to avoid having to do mutual definition with DHExp.t *)
            Definition t (a : Type) : Type := list(Var.t * a).
        End Environment.

        Module Type METAVARCTX.
            Parameter t : Type.
            Parameter empty : t.
            Parameter extend : t -> MetaVar.t * HTyp.t * Ctx.t -> t.
            Parameter union : t -> t -> t.
            Parameter lookup : t -> MetaVar.t -> option (HTyp.t * Ctx.t).
        End METAVARCTX.

        Module MetaVarCtx <: METAVARCTX.
          Definition t := list (MetaVar.t * HTyp.t * Ctx.t).

          Definition empty : t := nil.

          Definition extend (delta : t) (x : MetaVar.t * HTyp.t * Ctx.t)
            : t := cons x delta.

          Definition union (delta1 : t) (delta2 : t) : t := delta1 ++ delta2.

          Fixpoint lookup (delta : t) (x : MetaVar.t) : option (HTyp.t * Ctx.t) :=
            match delta with
            | nil => None
            | cons (y, ty, ctx) delta' =>
              match MetaVar.equal x y with
              | true => Some (ty, ctx)
              | false => lookup delta' x
              end
            end.
        End MetaVarCtx.            

        Module DHExp.
            Inductive eval_state : Type := 
            | Evaled : eval_state
            | Unevaled : eval_state.

            Inductive t : Type := 
            | Var : Var.t -> t
            | Let : Var.t -> t -> t -> t
            | Lam : Var.t -> HTyp.t -> t -> t
            | Ap  : t -> t -> t
            | NumLit : nat -> t
            | Plus : t -> t -> t
            | Inj : HExp.inj_side -> t -> t
            | Case : t -> (Var.t * t) -> (Var.t * t) -> t
            | EmptyHole : MetaVar.t -> eval_state -> Environment.t(t) -> t 
            | NonEmptyHole : MetaVar.t -> eval_state -> Environment.t(t) -> t -> t
            | Cast : HTyp.t -> t -> t.

            (* closed substitution [d1/x]d2*)
            Fixpoint subst (fuel : Fuel) (d1 : t) (x : Var.t) (d2 : t) : t := 
                match fuel with 
                | More fuel => let subst := subst fuel in 
                    match d2 with 
                    | Var y => if Var.equal x y then d1 else d2
                    | Let y d3 d4 =>
                        let d3' := subst d1 x d3 in 
                        let d4' := if Var.equal x y then d4 else subst d1 x d4 in 
                        Let y d3' d4'
                    | Lam y ty d3 => 
                        if Var.equal x y then d2 else 
                        let d3' := subst d1 x d3 in 
                        Lam y ty d3'
                    | Ap d3 d4 => 
                        let d3' := subst d1 x d3 in 
                        let d4' := subst d1 x d4 in 
                        Ap d3' d4'
                    | NumLit _ => d2
                    | Plus d3 d4 => 
                        let d3' := subst d1 x d3 in 
                        let d4' := subst d1 x d4 in 
                        Plus d3' d4'
                    | Inj side d3 => 
                        let d3' := subst d1 x d3 in 
                        Inj side d3' 
                    | Case d3 (y4, d4) (y5, d5) => 
                        let d3' := subst d1 x d3 in 
                        let d4' := if Var.equal x y4 then d4 else subst d1 x d4 in 
                        let d5' := if Var.equal x y5 then d5 else subst d1 x d5 in 
                        Case d3' (y4, d4') (y5, d5')
                    | EmptyHole u m sigma => 
                        let sigma' := env_subst fuel d1 x sigma in 
                        EmptyHole u m sigma' 
                    | NonEmptyHole u m sigma d3 => 
                        let d3' := subst d1 x d3 in 
                        let sigma' := env_subst fuel d1 x sigma in 
                        NonEmptyHole u m sigma' d3'
                    | Cast ty d => 
                        let d' := subst d1 x d in 
                        Cast ty d' 
                    end
                | Kicked => d2
                end
            with env_subst (fuel : Fuel) (d1 : t) (x : Var.t) (sigma : Environment.t(t)) := 
                match fuel with 
                | More fuel => 
                    Coq.Lists.List.map (
                      fun xd : Var.t * t => 
                          let (y, d) := xd in
                          (y, subst fuel d1 x d)) 
                      sigma
                | Kicked => sigma
                end.

            Inductive type_result : Type := 
            | WellTyped : HTyp.t -> type_result
            | IllTyped.

            Fixpoint assign_type 
                (fuel : Fuel) 
                (gamma : Ctx.t) (delta : MetaVarCtx.t) 
                (d : t) : type_result := 
                match fuel with 
                | More fuel' => 
                    let assign_type := assign_type fuel' in 
                    match d with 
                    | Var x => 
                        match Ctx.lookup gamma x with 
                        | Some ty => WellTyped ty
                        | None => IllTyped
                        end
                    | Let x d1 d2 => 
                        match assign_type gamma delta d1 with 
                        | WellTyped ty1 => 
                            let gamma' := Ctx.extend gamma (x, ty1) in 
                            assign_type gamma' delta d2
                        | IllTyped => IllTyped
                        end
                    | Lam x ty1 d1 => 
                        let gamma' := Ctx.extend gamma (x, ty1) in 
                        match assign_type gamma' delta d1 with 
                        | WellTyped ty2 => WellTyped (HTyp.Arrow ty1 ty2)
                        | IllTyped => IllTyped
                        end
                    | Ap d1 d2 => 
                        match assign_type gamma delta d1 with 
                        | WellTyped ty1 => 
                            match HTyp.matched_arrow ty1 with 
                            | Some (ty2, ty) => 
                                match assign_type gamma delta d2 with 
                                | WellTyped ty2' => 
                                    if HTyp.consistent ty2 ty2' then 
                                        WellTyped ty
                                     else IllTyped
                                | IllTyped => IllTyped
                                end
                            | None => IllTyped
                            end
                        | IllTyped => IllTyped
                        end
                    | NumLit _ => WellTyped HTyp.Num
                    | Plus d1 d2 => 
                        match (assign_type gamma delta d1, 
                               assign_type gamma delta d2) with 
                        | (WellTyped ty1, WellTyped ty2) => 
                            if HTyp.consistent ty1 HTyp.Num && 
                               HTyp.consistent ty2 HTyp.Num then 
                                 WellTyped HTyp.Num 
                            else IllTyped
                        | _ => IllTyped
                        end
                    | Inj side d1 => IllTyped (* TODO *)
                    | Case d1 (x, d2) (y, d3) => IllTyped (* TODO *)
                    | EmptyHole u m sigma => 
                        match MetaVarCtx.lookup delta u with 
                        | Some (ty, gamma') => 
                            if check_type_env fuel' gamma delta sigma gamma' then 
                                WellTyped ty
                            else IllTyped
                        | None => IllTyped
                        end
                    | NonEmptyHole u m sigma d1 => 
                        match assign_type gamma delta d1 with 
                        | WellTyped _ => 
                            match MetaVarCtx.lookup delta u with 
                            | Some (ty, gamma') => 
                                if check_type_env fuel' gamma delta sigma gamma' then
                                    WellTyped ty
                                else IllTyped
                            | None => IllTyped
                            end
                        | IllTyped => IllTyped
                        end
                    | Cast ty d1 => 
                        match assign_type gamma delta d1 with 
                        | WellTyped ty' => 
                            if HTyp.consistent ty ty' then 
                                WellTyped ty
                            else IllTyped
                        | IllTyped => IllTyped
                        end
                    end
                | Kicked => IllTyped
                end
            with check_type_env (fuel : Fuel)
                    (gamma : Ctx.t) (delta : MetaVarCtx.t) 
                    (sigma : Environment.t(t)) 
                    (gamma' : Ctx.t) : bool := 
                match fuel with 
                | More fuel' => 
                    Coq.Lists.List.forallb  
                      (fun xd : Var.t * t => 
                        let (x, d) := xd in 
                        match assign_type fuel' gamma delta d with 
                        | WellTyped ty => 
                          match Ctx.lookup gamma' x with 
                          | Some ty' => HTyp.consistent ty ty'
                          | None => false
                          end
                        | IllTyped => false
                        end)
                      sigma
                | Kicked => false
                end.
            
            Inductive expand_result : Type := 
            | Expands : t -> HTyp.t -> MetaVarCtx.t -> expand_result
            | DoesNotExpand.

            Definition id_env (gamma : Ctx.t) : Environment.t(t) := 
                Ctx.map (Var.t * t) (
                  fun xt : Var.t * HTyp.t => 
                      let (x, t) := xt in 
                      (x, DHExp.Var x)) gamma.

            Fixpoint syn_expand (fuel : Fuel) (gamma : Ctx.t) (e : HExp.t) : expand_result := 
                match fuel with 
                | Kicked => DoesNotExpand
                | More fuel => let syn_expand := syn_expand fuel in 
                    match e with 
                    | HExp.Asc e1 ty => 
                        match ana_expand fuel gamma e1 ty with 
                        | Expands d1 ty' delta => 
                            let d := 
                                if HTyp.eq ty ty' then d1
                                else Cast ty d1 in 
                            Expands d ty delta 
                        | DoesNotExpand => DoesNotExpand
                        end
                    | HExp.Var x => 
                        match Ctx.lookup gamma x with 
                        | Some ty => Expands (DHExp.Var x) ty MetaVarCtx.empty
                        | None => DoesNotExpand
                        end
                    | HExp.Let x e1 e2 => 
                        match syn_expand gamma e1 with 
                        | Expands d1 ty1 delta1 => 
                            let gamma' := Ctx.extend gamma (x, ty1) in 
                            match syn_expand gamma' e2 with 
                            | Expands d2 ty delta2 => 
                                let d := Let x d1 d2 in 
                                let delta12 := MetaVarCtx.union delta1 delta2 in 
                                Expands d ty delta12 
                            | DoesNotExpand => DoesNotExpand
                            end
                        | DoesNotExpand => DoesNotExpand
                        end
                    | HExp.Lam x e1 => DoesNotExpand
                    | HExp.Ap e1 e2 => 
                        match HExp.hsyn fuel gamma e1 with 
                        | Some HTyp.Hole => 
                            match ana_expand fuel gamma e2 HTyp.Hole with 
                            | Expands d2 ty2 delta2 => 
                                let ty2h := HTyp.Arrow ty2 HTyp.Hole in 
                                match ana_expand fuel gamma e1 ty2h with 
                                | Expands d1 ty1 delta1 => 
                                    let f := Cast ty2h d1 in 
                                    let d := Ap f d2 in 
                                    let delta12 := MetaVarCtx.union delta1 delta2 in 
                                    Expands d HTyp.Hole delta12
                                | DoesNotExpand => DoesNotExpand
                                end
                           | DoesNotExpand => DoesNotExpand
                            end
                        | Some (HTyp.Arrow ty2 ty) => 
                            match syn_expand gamma e1 with 
                            | Expands d1 _ delta1 => 
                                match ana_expand fuel gamma e2 ty2 with 
                                | Expands d2 ty2' delta2 => 
                                    let arg := 
                                        if HTyp.eq ty2 ty2' then d2
                                        else Cast ty2 d2 in 
                                    let d := Ap d1 arg in 
                                    let delta12 := MetaVarCtx.union delta1 delta2 in 
                                    Expands d ty delta12
                                | DoesNotExpand => DoesNotExpand
                                end
                            | DoesNotExpand => DoesNotExpand
                            end
                        | _ => DoesNotExpand
                        end
                    | HExp.NumLit n => 
                        Expands (DHExp.NumLit n) HTyp.Num MetaVarCtx.empty 
                    | HExp.Plus e1 e2 => 
                        match ana_expand fuel gamma e1 HTyp.Num with 
                        | Expands d1 ty1 delta1 => 
                            match ana_expand fuel gamma e2 HTyp.Num with 
                            | Expands d2 ty2 delta2 => 
                                let d1' := 
                                    if HTyp.eq ty1 HTyp.Num then d1 
                                    else Cast HTyp.Num d1 in 
                                let d2' := 
                                    if HTyp.eq ty2 HTyp.Num then d2
                                    else Cast HTyp.Num d2 in 
                                let d := Plus d1' d2' in 
                                let delta12 := MetaVarCtx.union delta1 delta2 in 
                                Expands d HTyp.Num delta12
                            | DoesNotExpand => DoesNotExpand
                            end
                        | DoesNotExpand => DoesNotExpand
                        end
                    | HExp.Inj side e1 => DoesNotExpand (* TODO *)
                    | HExp.Case e1 (x, e2) (y, e3) => DoesNotExpand (* TODO *)
                    | HExp.EmptyHole u => 
                        let sigma := id_env gamma in 
                        let d := DHExp.EmptyHole u Unevaled sigma in 
                        let ty := HTyp.Hole in 
                        let delta := MetaVarCtx.extend MetaVarCtx.empty 
                                     (u, ty, gamma) in 
                        Expands d ty delta
                    | HExp.NonEmptyHole u e1 => 
                        let sigma := id_env gamma in 
                        match syn_expand gamma e1 with 
                        | Expands d1 _ delta1 => 
                            let d := DHExp.NonEmptyHole u Unevaled sigma d1 in 
                            let ty := HTyp.Hole in 
                            let delta := MetaVarCtx.extend delta1
                                          (u, ty, gamma) in 
                            Expands d ty delta
                        | DoesNotExpand => DoesNotExpand
                        end
                     end
                end
            with ana_expand (fuel : Fuel) 
                            (gamma : Ctx.t) (e : HExp.t) (ty : HTyp.t) : expand_result := 
                match fuel with 
                | Kicked => DoesNotExpand
                | More fuel => let ana_expand := ana_expand fuel in 
                    match e with 
                    | HExp.Lam x e1 => 
                        match ty with 
                        | HTyp.Arrow ty1 ty2 =>
                            let gamma' := Ctx.extend gamma (x, ty1) in 
                            match ana_expand gamma' e1 ty2 with 
                            | Expands d1 ty2' delta => 
                                let d := Lam x ty1 d1 in 
                                let ty' := HTyp.Arrow ty1 ty2' in 
                                Expands d ty' delta
                            | DoesNotExpand => DoesNotExpand
                            end
                        | HTyp.Hole => 
                            let gamma' := Ctx.extend gamma (x, HTyp.Hole) in 
                            match ana_expand gamma' e1 HTyp.Hole with 
                            | Expands d1 ty2' delta => 
                                let d := Lam x HTyp.Hole d1 in 
                                let ty' := HTyp.Arrow HTyp.Hole ty2' in 
                                Expands d ty' delta 
                            | DoesNotExpand => DoesNotExpand
                            end
                        | _ => DoesNotExpand
                        end
                    | HExp.EmptyHole u => 
                        let sigma := id_env gamma in 
                        let d := EmptyHole u Unevaled sigma in 
                        let delta := MetaVarCtx.extend MetaVarCtx.empty (u, ty, gamma) in 
                        Expands d ty delta
                    | HExp.NonEmptyHole u e1 => 
                        let sigma := id_env gamma in 
                        match syn_expand fuel gamma e1 with 
                        | Expands d1 _ delta1 => 
                            let d := DHExp.NonEmptyHole u Unevaled sigma d1 in 
                            let delta := MetaVarCtx.extend delta1
                                          (u, ty, gamma) in 
                            Expands d ty delta
                        | DoesNotExpand => DoesNotExpand
                        end
                    | HExp.Asc _ _ 
                    | HExp.Var _ 
                    | HExp.Let _ _ _ 
                    | HExp.Ap _ _
                    | HExp.NumLit _
                    | HExp.Plus _ _ 
                    | HExp.Inj _ _ 
                    | HExp.Case _ _ _ => 
                        match syn_expand fuel gamma e with 
                        | DoesNotExpand => DoesNotExpand
                        | (Expands d ty' delta) as result => 
                            if HTyp.consistent ty ty'  
                            then result
                            else DoesNotExpand
                        end
                    end
                end.
        End DHExp.

        Module Evaluator.
            Inductive result := 
            | InvalidInput : result (* not well-typed or otherwise invalid *)
            | CastError : result
            | Value : DHExp.t -> result
            | Indet : DHExp.t -> result.

            Fixpoint evaluate (fuel : Fuel) (delta : MetaVarCtx.t) (d : DHExp.t) : result := 
                match fuel with 
                | Kicked => InvalidInput
                | More(fuel') => 
                    match d with 
                    | DHExp.Var _ => InvalidInput
                    | DHExp.Let x d1 d2 => 
                        match evaluate fuel' delta d1 with 
                        | InvalidInput => InvalidInput
                        | CastError => CastError
                        | Value d1' | Indet d1' => 
                                evaluate fuel' delta (DHExp.subst fuel d1' x d2)
                        end
                    | DHExp.Lam _ _ _ => Value d
                    | DHExp.Ap d1 d2 => 
                        match evaluate fuel' delta d1 with 
                        | InvalidInput => InvalidInput
                        | CastError => CastError
                        | Value (DHExp.Lam x tau d1') => 
                            match evaluate fuel' delta d2 with 
                            | InvalidInput => InvalidInput
                            | CastError => CastError
                            | Value d2' | Indet d2' => 
                                    evaluate fuel' delta (DHExp.subst fuel d2' x d1')
                            end
                        | Value _ => InvalidInput
                        | Indet d1' => 
                            match evaluate fuel' delta d2 with 
                            | InvalidInput => InvalidInput
                            | CastError => CastError
                            | Value d2' | Indet d2' => 
                                    Indet (DHExp.Ap d1' d2')
                            end
                        end
                    | DHExp.NumLit _ => Value d
                    | DHExp.Plus d1 d2 => 
                        match evaluate fuel' delta d1 with 
                        | InvalidInput => InvalidInput
                        | CastError => CastError
                        | Value (DHExp.NumLit n1 as d1')  => 
                            match evaluate fuel' delta d2 with 
                            | InvalidInput => InvalidInput
                            | CastError => CastError
                            | Value (DHExp.NumLit n2) => 
                                Value (DHExp.NumLit (n1 + n2))
                            | Value _ => InvalidInput
                            | Indet d2' => 
                                    Indet (DHExp.Plus d1' d2')
                            end
                        | Value _ => InvalidInput
                        | Indet d1' => 
                            match evaluate fuel' delta d2 with 
                            | InvalidInput => InvalidInput
                            | CastError => CastError
                            | Value d2' | Indet d2' => 
                                Indet (DHExp.Plus d1' d2')
                            end
                        end
                    | DHExp.Inj side d1 => InvalidInput (* TODO *)
                    | DHExp.Case d1 (x, d2) (y, d3) => InvalidInput (* TODO *)
                    | DHExp.EmptyHole _ _ _ => Indet d 
                    | DHExp.NonEmptyHole u m sigma d1 => 
                        match evaluate fuel' delta d1 with 
                        | InvalidInput => InvalidInput
                        | CastError => CastError
                        | Value d1' | Indet d1' => 
                                Indet (DHExp.NonEmptyHole u m sigma d1')
                        end
                    | DHExp.Cast ty1 d1 => 
                        match evaluate fuel' delta d1 with 
                        | InvalidInput => InvalidInput
                        | CastError => CastError
                        | (Value d1' as res) | (Indet d1' as res) => 
                            match DHExp.assign_type fuel' Ctx.empty delta d1' with 
                            | DHExp.IllTyped => CastError
                            | DHExp.WellTyped ty2 => 
                                if HTyp.consistent ty1 ty2 
                                then res
                                else CastError
                            end
                        end
                    end
                end.
        End Evaluator.
    End Dynamics.

  Extract Constant str_eqb => "String.equal".
  Extract Inductive Fuel => "unit" [ "()" "()" ] "(fun fMore _ fKicked -> fMore ())".
End Core.

Extract Inductive Coq.Strings.String.string => "string" ["""""" "String"].
Extract Inductive bool => "bool" ["true" "false"].
Extract Inductive unit => "unit" ["()"].
Extract Constant negb => "not".
Extract Constant Coq.Arith.PeanoNat.Nat.eqb => "(=)".
Extract Constant Coq.Lists.List.map => "List.map".
Extract Inductive option => "option" ["Some" "None"].
Extract Inductive prod => "( * )" ["(,)"].
Extract Inductive list => "list" [ "[]" "(::)" ].
Extract Inductive nat => int [ "0" "((+) 1)" ]
       "(fun fO fS n -> if n=0 then fO () else fS (n-1))".
(*Extract Inlined Constant pipe_forward => "(|>)".*)

Extraction Core.
