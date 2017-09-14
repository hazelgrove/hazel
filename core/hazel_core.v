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

    Module Type CTX.
      Parameter t : Type.
      Parameter empty : t.
      Parameter extend : t -> Var.t * HTyp.t -> t.
      Parameter lookup : t -> Var.t -> option HTyp.t.
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
      | EmptyHole : t
      | NonEmptyHole : t -> t.

      Inductive Mode : Type :=
      | Syn : Mode
      | Ana : HTyp.t -> Mode.

      Inductive Result : Type :=
      | SynOK : HTyp.t -> Result
      | AnaOK : Result
      | IllTyped.

      Fixpoint syn (fuel : Fuel) (ctx : Ctx.t) (e : t)
        : option HTyp.t :=
        match fuel with
        | More fuel =>
          match e with
          | Asc e' ty (* SAsc *) =>
            let _ := ana fuel ctx e' ty in Some ty
          | Var x (* SVar *) => Ctx.lookup ctx x
          | Let x e1 e2 =>
            (syn fuel ctx e1) |>
                (map_option (fun ty1 => Ctx.extend ctx (x, ty1))) |>
                (flatmap_option (fun ctx' => syn fuel ctx' e2))
          | Ap e1 e2 (* SAp *) =>
            let _ty1 := syn fuel ctx e1 in
            match _ty1 with
            | Some ty1 =>
              match HTyp.matched_arrow ty1 with
              | Some (ty1_left, ty1_right) =>
                  let _ := ana fuel ctx e2 ty1_left in Some ty1_right
              | None => None
              end
            | None => None
            end
          | NumLit i (* SNum *) => Some HTyp.Num
          | Plus e1 e2 (* 3e *) =>
            let _ := ana fuel ctx e1 HTyp.Num in
            let _ := ana fuel ctx e2 HTyp.Num in
            Some HTyp.Num
          | EmptyHole (* SHole *) => Some HTyp.Hole
          | NonEmptyHole e' (* SNEHole *) =>
            let _ := syn fuel ctx e' in
            Some HTyp.Hole
          | _ => None
          end
        | Kicked => None
        end
      with ana (fuel : Fuel) (ctx : Ctx.t) (e : t) (ty : HTyp.t)
        : option unit :=
        match fuel with
        | More fuel =>
          match e with
          | Let x e1 e2 =>
              syn fuel ctx e1 |>
                  map_option (fun ty1 => Ctx.extend ctx (x, ty1)) |>
                  flatmap_option (fun ctx' => ana fuel ctx' e2 ty)
          | Lam x e' (* ALam *) =>
            match HTyp.matched_arrow ty with
            | Some (ty1, ty2) =>
              let ctx' := Ctx.extend ctx (x, ty1) in
              ana fuel ctx' e' ty2
            | _ => None
            end
          | Inj side e' (* 21a *) =>
            match HTyp.matched_sum ty with
            | Some (ty1, ty2) => ana fuel ctx e' (pick_side side ty1 ty2)
            | None => None
            end
          | Case e' (x, e1) (y, e2) (* 21b *) =>
            let _e'_ty := syn fuel ctx e' in
            match _e'_ty with
            | Some e'_ty =>
              match HTyp.matched_sum e'_ty with
              | Some (ty1, ty2) =>
                let ctx1 := Ctx.extend ctx (x, ty1) in
                match (ana fuel ctx1 e1 ty) with
                | Some _ =>
                  let ctx2 := Ctx.extend ctx (y, ty2) in
                  ana fuel ctx2 e2 ty
                | None => None
                end
              | None => None
              end
            | None => None
            end
          | _ (* subsumption *) =>
            match syn fuel ctx e with
            | Some ty' =>
                if HTyp.consistent ty ty' then (Some tt) else None
            | None => None
            end
          end
        | Kicked => None
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
        | EmptyHole => false
        | NonEmptyHole _ => false
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
      | NonEmptyHoleZ : t -> t.

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
        | NonEmptyHoleZ ze' => HExp.NonEmptyHole (erase ze')
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
          | (Child 1, ZExp.CursorE (HExp.NonEmptyHole e)) =>
            Some (ZExp.NonEmptyHoleZ (ZExp.CursorE e))
          | (Parent, ZExp.NonEmptyHoleZ (ZExp.CursorE e)) =>
            Some (ZExp.CursorE (HExp.NonEmptyHole e))
          | _ => None
          end
        | _ => None
        end.

      Definition hsyn (fuel: Fuel) (ctx: Ctx.t) (e: HExp.t): option HTyp.t := HExp.syn fuel ctx e.

      Definition hana (fuel: Fuel) (ctx: Ctx.t) (e: HExp.t) (ty: HTyp.t): option unit := HExp.ana fuel ctx e ty.

      Fixpoint performSyn (fuel: Fuel) (ctx: Ctx.t) (a: t) (ze_ty: ZExp.t * HTyp.t): option (ZExp.t * HTyp.t) :=
        match ze_ty with
        | (ze, ty) =>
        match fuel with
        | Kicked => None
        | More fuel =>
        match performEMove a ze with
        | Some ze1 => Some (ze1, ty) (* SAMove *)
        | None =>
          match (a, (ze, ty)) with
            (* Deletion *)
            | (Del, (ZExp.CursorE e, _)) (* SADel *) =>
              Some ((ZExp.CursorE HExp.EmptyHole), HTyp.Hole)
            (* Construction *)
            | (Construct SAsc, (ZExp.CursorE e, ty)) (* SAConAsc *) =>
              Some (ZExp.RightAsc e (ZTyp.CursorT ty), ty)
            | (Construct (SVar x), (ZExp.CursorE HExp.EmptyHole, HTyp.Hole)) (* SAConVar *) =>
              match Ctx.lookup ctx x with
              | Some xty => Some (ZExp.CursorE (HExp.Var x), xty)
              | None => None
              end
            | (Construct (SLet x), (ZExp.CursorE e, ty)) =>
              match e with
                | HExp.EmptyHole =>
                  Some ((ZExp.LetZ1 x (ZExp.CursorE e) HExp.EmptyHole), HTyp.Hole)
                | _ =>
                  Some ((ZExp.LetZ2 x e (ZExp.CursorE HExp.EmptyHole)), HTyp.Hole)
              end
            | (Construct (SLam x), (ZExp.CursorE HExp.EmptyHole, HTyp.Hole)) (* SAConLam *) =>
              Some (ZExp.RightAsc
                  (HExp.Lam x HExp.EmptyHole)
                  (ZTyp.LeftArrow (ZTyp.CursorT HTyp.Hole) HTyp.Hole),
               (HTyp.Arrow HTyp.Hole HTyp.Hole))
            | (Construct SAp, (ZExp.CursorE e, ty)) =>
              match HTyp.matched_arrow ty with
                | Some (_, ty2) (* SAConApArr *) => Some (
                      (ZExp.RightAp e (ZExp.CursorE HExp.EmptyHole)),
                      ty2)
                | None (* SAConApOtw *) => Some (
                    (ZExp.RightAp (HExp.NonEmptyHole e) (ZExp.CursorE HExp.EmptyHole)),
                    HTyp.Hole)
              end
            | (Construct (SLit n), (ZExp.CursorE HExp.EmptyHole, HTyp.Hole)) (* SAConNumLit *) =>
                Some (ZExp.CursorE (HExp.NumLit n), HTyp.Num)
            | (Construct SPlus, (ZExp.CursorE e, _)) =>
              if HTyp.consistent ty HTyp.Num (* SAConPlus1 *) then
                Some ((ZExp.RightPlus e (ZExp.CursorE HExp.EmptyHole)), HTyp.Num)
              else (* SAConPlus2 *)
                Some ((ZExp.RightPlus (HExp.NonEmptyHole e) (ZExp.CursorE HExp.EmptyHole)), HTyp.Num)
            | (Construct (SInj side), (ZExp.CursorE HExp.EmptyHole, HTyp.Hole)) (* 24a *) =>
              Some (
                (ZExp.RightAsc (HExp.Inj side HExp.EmptyHole) (ZTyp.LeftSum (ZTyp.CursorT HTyp.Hole) HTyp.Hole)),
                (HTyp.Sum HTyp.Hole HTyp.Hole)
              )
            | (Construct (SCase x y), ((ZExp.CursorE e), ty)) =>
              match HTyp.matched_sum ty with
                | Some _ (* 24b *) =>
                    let casez2 := (ZExp.CaseZ2 e
                                  (x, ZExp.CursorE HExp.EmptyHole)
                                  (y, HExp.EmptyHole)) in
                    Some (ZExp.LeftAsc casez2 HTyp.Hole, HTyp.Hole)
                | None (* 24c *) => Some (
                    (ZExp.LeftAsc
                      (ZExp.CaseZ1 (ZExp.NonEmptyHoleZ (ZExp.CursorE e))
                                    (x, HExp.EmptyHole)
                                    (y, HExp.EmptyHole))
                      HTyp.Hole
                    ),
                    HTyp.Hole)
              end
            | (Construct SNEHole, (ZExp.CursorE e', ty)) (* SAConNEHole *) =>
              Some (ZExp.NonEmptyHoleZ (ZExp.CursorE e'), HTyp.Hole)
            (* Finish *)
            | (Finish, (ZExp.CursorE (HExp.NonEmptyHole e), HTyp.Hole)) (* SAFinish *) =>
              (hsyn fuel ctx e) |>
                  map_option(fun ty' => (ZExp.CursorE e, ty'))
            (* Zipper Cases *)
            | (a, (ZExp.LeftAsc ze ty, _)) (* SAZipAsc1 *) =>
              (performAna fuel ctx a ze ty) |>
                  map_option(fun ze' => (ZExp.LeftAsc ze' ty, ty))
            | (a, (ZExp.RightAsc e zty, _)) (* SAZipAsc2 *) =>
              (performTyp a zty) |>
                  flatmap_option(fun zty' => let ty' := ZTyp.erase zty' in
                    (hana fuel ctx e ty') |>
                      map_option(fun tt => (ZExp.RightAsc e zty', ty'))
                  )
            | (a, (ZExp.LetZ1 x ze1 e2, _)) =>
              let e1 := ZExp.erase ze1 in
              (hsyn fuel ctx e1) |>
                  flatmap_option(fun ty1 => performSyn fuel ctx a (ze1, ty1)) |>
                  flatmap_option(fun ze_ty1' =>
                    match ze_ty1' with
                    | (ze1', ty1') =>
                      let ctx' := Ctx.extend ctx (x, ty1') in
                      (hsyn fuel ctx' e2) |>
                        map_option(fun ty2' => (ZExp.LetZ1 x ze1' e2, ty2'))
                    end
                  )
            | (a, (ZExp.LetZ2 x e1 ze2, _)) =>
              (hsyn fuel ctx e1) |>
                  flatmap_option(fun ty1 =>
                    let ctx' := Ctx.extend ctx (x, ty1) in
                    performSyn fuel ctx' a (ze2, ty)
                  ) |>
                  map_option(fun ze_ty2' =>
                    match ze_ty2' with
                    | (ze2', ty2') =>
                      (ZExp.LetZ2 x e1 ze2', ty2')
                    end)
            | (_, (ZExp.LeftAp ze1 e2, _)) (* SAZipApArr *) =>
              let e1 := ZExp.erase ze1 in
              (hsyn fuel ctx e1) |>
                  flatmap_option(fun ty2 => performSyn fuel ctx a (ze1, ty2)) |>
                  flatmap_option(fun ze1'_ty3 =>
                    match ze1'_ty3 with
                    | (ze1', ty3) =>
                      (HTyp.matched_arrow ty3) |>
                          flatmap_option(fun ty4_ty5 =>
                            match ty4_ty5 with
                            | (ty4, ty5) =>
                            (hana fuel ctx e2 ty5) |>
                              map_option(fun _ => (ZExp.LeftAp ze1' e2, ty5))
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
                    (performAna fuel ctx a ze2 ty3) |>
                      map_option(fun ze2' => (ZExp.RightAp e1 ze2', ty4))
                    end
                  )
            | (_, (ZExp.LeftPlus ze1 e2, _)) (* SAZipPlus1 *) =>
              (performAna fuel ctx a ze1 HTyp.Num) |>
                  map_option(fun ze1' => (ZExp.LeftPlus ze1' e2, HTyp.Num))
            | (_, (ZExp.RightPlus e1 ze2, _)) (* SAZipPlus2 *) =>
              (performAna fuel ctx a ze2 HTyp.Num) |>
                  map_option(fun ze2' => (ZExp.RightPlus e1 ze2', HTyp.Num))
            | (_, (ZExp.NonEmptyHoleZ ze1, _)) (* SAZipHole *) =>
              let e1 := ZExp.erase ze1 in
              (hsyn fuel ctx e1) |>
                  flatmap_option(fun ty1 => performSyn fuel ctx a (ze1, ty1)) |>
                  map_option(fun ze1'_ty1' =>
                    match ze1'_ty1' with
                    | (ze1', _) => (ZExp.NonEmptyHoleZ ze1', HTyp.Hole)
                    end)
            | _ => None
            end
          end
          end
          end
      with performAna (fuel: Fuel) (ctx: Ctx.t) (a: t) (ze: ZExp.t) (ty: HTyp.t): option ZExp.t :=
        match fuel with
        | Kicked => None
        | More fuel =>
          match a with
          | Move _ (* AAMove *) =>
            (* try to use the non-zipper move actions *)
            match performEMove a ze with
            | Some x => Some x
            | None =>
                (* if it doesn't work, keep going --
                 * it'll hit the subsumption rule at the bottom *)
                performAna_postMoveCheck fuel ctx a ze ty
            end
          | _ => performAna_postMoveCheck fuel ctx a ze ty
          end
        end
      with performAna_subsume (fuel : Fuel) (ctx : Ctx.t) (a : t) (ze : ZExp.t) (ty : HTyp.t) : option ZExp.t :=
        match fuel with
        | Kicked => None
        | More fuel =>
          let e := ZExp.erase ze in
          (hsyn fuel ctx e) |>
            flatmap_option(fun ty1 => performSyn fuel ctx a (ze, ty1)) |>
            flatmap_option(fun ze_ty1 =>
              match ze_ty1 with
              | (ze', ty1') =>
                if HTyp.consistent ty ty1' then Some ze' else None
              end
            )
        end
      with performAna_postMoveCheck (fuel : Fuel) (ctx : Ctx.t) (a : t) (ze : ZExp.t) (ty : HTyp.t) : option ZExp.t :=
        match fuel with
        | Kicked => None
        | More fuel =>
        match (a, ze, ty) with
        (* Deletion *)
        | (Del, ZExp.CursorE e, _) (* AADel *) =>
          Some (ZExp.CursorE HExp.EmptyHole)
        (* Construction *)
        | (Construct SAsc, ZExp.CursorE e, _) (* AAConAsc *) =>
          Some (ZExp.RightAsc e (ZTyp.CursorT ty))
        | (Construct (SVar x), ZExp.CursorE HExp.EmptyHole, ty) (* SAConVar *) =>
          let ty_valid := match Ctx.lookup ctx x with
            | Some xty => HTyp.inconsistent ty xty
            | None => false end
          in
            if ty_valid then
              Some (ZExp.NonEmptyHoleZ (ZExp.CursorE (HExp.Var x))) else
              performAna_subsume fuel ctx a ze ty
        | (Construct (SLet x), ZExp.CursorE HExp.EmptyHole, _) =>
          Some (ZExp.LetZ1 x ze HExp.EmptyHole)
        | (Construct (SLam x), ZExp.CursorE HExp.EmptyHole, ty) =>
          match HTyp.matched_arrow ty with
            | Some _ (* AAConLam1 *) => Some (ZExp.LamZ x ze)
            | None (* AAConLam2 *) => Some (
                ZExp.NonEmptyHoleZ (
                  ZExp.RightAsc
                    (HExp.Lam x HExp.EmptyHole)
                    (ZTyp.LeftArrow (ZTyp.CursorT HTyp.Hole) HTyp.Hole)
                  )
            )
          end
        | (Construct (SLit n), ZExp.CursorE HExp.EmptyHole, ty) (* AAConNumLit *) =>
          if HTyp.inconsistent ty HTyp.Num then
            Some (ZExp.NonEmptyHoleZ (ZExp.CursorE (HExp.NumLit n)))
          else
            performAna_subsume fuel ctx a ze ty
        | (Construct (SInj side), ZExp.CursorE HExp.EmptyHole, ty) =>
          match HTyp.matched_sum ty with
            | Some _ (* 23a *) => Some (ZExp.InjZ side ze)
            | None (* 23b *) => Some (
                ZExp.NonEmptyHoleZ (
                  ZExp.RightAsc
                    (HExp.Inj side HExp.EmptyHole)
                    (ZTyp.LeftSum (ZTyp.CursorT HTyp.Hole) HTyp.Hole)
                )
            )
          end
        | (Construct (SCase x y), ZExp.CursorE e, ty) (* 23c *) =>
          Some (
            ZExp.CaseZ1
              (ZExp.CursorE HExp.EmptyHole)
              (x, HExp.EmptyHole)
              (y, HExp.EmptyHole)
          )
        (* Finishing *)
        | (Finish, ZExp.CursorE (HExp.NonEmptyHole e), _) (* AAFinish *) =>
          (hana fuel ctx e ty) |>
              map_option(fun _ => ZExp.CursorE e)
        (* Zipper Cases *)
        | (_, ZExp.LetZ1 x ze1 e2, _) =>
          let e1 := ZExp.erase ze1 in
          (hsyn fuel ctx e1) |>
              flatmap_option(fun ty1 => performSyn fuel ctx a (ze1, ty1)) |>
              flatmap_option(fun ze1'_ty1' =>
                match ze1'_ty1' with
                | (ze1', ty1') =>
                  let ctx' := Ctx.extend ctx (x, ty1') in
                  (hana fuel ctx' e2 ty) |>
                    map_option(fun _ => ZExp.LetZ1 x ze1' e2)
                end)
        | (_, ZExp.LetZ2 x e1 ze2, _) =>
          (hsyn fuel ctx e1) |>
              flatmap_option(fun ty1 =>
                let ctx' := Ctx.extend ctx (x, ty1) in
                performAna fuel ctx' a ze2 ty) |>
              map_option(fun ze2' => ZExp.LetZ2 x e1 ze2')
        | (_, ZExp.LamZ x ze', ty) (* AAZipLam *) =>
          (HTyp.matched_arrow ty) |>
              flatmap_option(fun ty1_ty2 => match ty1_ty2 with
              | (ty1, ty2) =>
                let ctx' := Ctx.extend ctx (x, ty1) in
                performAna fuel ctx' a ze' ty2
              end) |>
              map_option(fun ze'' => ZExp.LamZ x ze'')
        | (_, ZExp.InjZ side ze, ty) (* 23d *) =>
          (HTyp.matched_sum ty) |>
              flatmap_option(fun ty1_ty2 => match ty1_ty2 with
              | (ty1, ty2) =>
                performAna fuel ctx a ze
                  (HExp.pick_side side ty1 ty2)
              end) |>
              map_option(fun ze' => ZExp.InjZ HExp.L ze')
        | (_, ZExp.CaseZ1 ze (x, e1) (y, e2), ty) (* 23e *) =>
          let e0 := ZExp.erase ze in
          (hsyn fuel ctx e0) |>
              flatmap_option(fun ty0 => performSyn fuel ctx a (ze, ty0)) |>
              flatmap_option(fun ze'_ty0' => match ze'_ty0' with
              | (ze', ty0') => (HTyp.matched_sum ty0') |>
                flatmap_option(fun ty1_ty2 => match ty1_ty2 with
                | (ty1, ty2) =>
                  let ctx1 := Ctx.extend ctx (x, ty1) in
                  (hana fuel ctx1 e1 ty) |>
                      flatmap_option(fun _ =>
                        let ctx2 := Ctx.extend ctx (y, ty2) in
                        hana fuel ctx2 e2 ty)
                end) |>
                map_option(fun _ => ZExp.CaseZ1 ze' (x, e1) (y, e2))
              end)
        | (_, ZExp.CaseZ2 e0 (x, ze1) (y, e2), ty) (* 23f *) =>
          (hsyn fuel ctx e0) |>
              flatmap_option(fun ty0 => HTyp.matched_sum ty0) |>
              flatmap_option(fun ty1_ty2 => match ty1_ty2 with
              | (ty1, ty2) =>
                let ctx1 := Ctx.extend ctx (x, ty1) in
                performAna fuel ctx1 a ze1 ty
              end) |>
              map_option(fun ze1' => ZExp.CaseZ2 e0 (x, ze1') (y, e2))
        | (_, ZExp.CaseZ3 e0 (x, e1) (y, ze2), ty) (* 23g *) =>
          (hsyn fuel ctx e0) |>
              flatmap_option(fun ty0 => HTyp.matched_sum ty0) |>
              flatmap_option(fun ty1_ty2 => match ty1_ty2 with
              | (ty1, ty2) =>
                let ctx2 := Ctx.extend ctx (y, ty2) in
                performAna fuel ctx2 a ze2 ty
              end) |>
              map_option(fun ze2' => ZExp.CaseZ3 e0 (x, e1) (y, ze2'))
        (* Subsumption *)
        | _ (* AASubsume *) =>
          performAna_subsume fuel ctx a ze ty
        end
        end.

    End Action.

  Extract Constant str_eqb => "String.equal".
  Extract Inductive Fuel => "unit" [ "()" "()" ] "(fun fMore _ fKicked -> fMore ())".
End Core.

Extract Inductive Coq.Strings.String.string => "string" ["""""" "String"].
Extract Inductive bool => "bool" ["true" "false"].
Extract Inductive unit => "unit" ["()"].
Extract Constant negb => "not".
Extract Constant Coq.Arith.PeanoNat.Nat.eqb => "(=)".
Extract Inductive option => "option" ["Some" "None"].
Extract Inductive prod => "(*)" ["(,)"].
Extract Inductive list => "list" [ "[]" "(::)" ].
Extract Inductive nat => int [ "0" "succ" ]
       "(fun fO fS n -> if n=0 then fO () else fS (n-1))".
(*Extract Inlined Constant pipe_forward => "(|>)".*)

Extraction Core.
