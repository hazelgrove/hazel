Require Coq.Bool.Bool. Open Scope bool.
Require Import OCaml.OCaml.

Local Open Scope Z_scope.
Local Open Scope type_scope.
Import ListNotations.

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

Module HTyp.
  Inductive t : Type :=
  | Num : t
  | Arrow : t -> t -> t
  | Sum : t -> t -> t
  | Hole : t.

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

  Fixpoint complete (ty : t) : bool :=
    match ty with
    | Num => true
    | Arrow ty1 ty2 => andb (complete ty1) (complete ty2)
    | Sum ty1 ty2 => andb (complete ty1) (complete ty2)
    | Hole => false
    end.
End HTyp.

Module Var.
  Definition t := Z.

  Definition equal (x : t) (y : t) : bool := equiv_decb x y.
End Var.

Module Ctx.
  Definition t := list (Var.t * HTyp.t).

  Definition empty {A : Type} : list A := [].

  Definition extend {A B : Type} (ctx : list (A * B)) (x : A * B)
    : list (A * B) :=
    match x with
    | (x, ty) => cons (x, ty) ctx
    end.

  Fixpoint lookup {A : Type} (ctx : list (Var.t * A)) (x : Var.t) : option A :=
    match ctx with
    | [] => None
    | cons (y, ty) ctx' =>
      match Var.equal x y with
      | true => Some ty
      | false => lookup ctx' x
      end
    end.
End Ctx.

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
  | NumLit : Z -> t
  | Plus : t -> t -> t
  | Inj : inj_side -> t -> t
  | Case : t -> (Var.t * t) -> (Var.t * t) -> t
  | EmptyHole : t
  | NonEmptyHole : t -> t.

  Definition IllTyped := Effect.make unit unit.

  Definition raise_IllTyped {A : Type} (x : unit) : M [ IllTyped ] A :=
    fun s => (inr (inl x), s).

  Fixpoint syn_rec (counter : nat) (ctx : Ctx.t) (e : t)
    : M [ NonTermination; IllTyped ] HTyp.t :=
    match counter with
    | O => lift [_;_] "10" (not_terminated tt)
    | S counter =>
      match e with
      | Asc e' ty =>
        let! x := (ana_rec counter) ctx e' ty in
        match x with
        | _ => ret ty
        end
      | Var x =>
        lift [_;_] "01"
          match Ctx.lookup ctx x with
          | Some ty => ret ty
          | None => raise_IllTyped tt
          end
      | Let x e1 e2 =>
        let! ty1 := (syn_rec counter) ctx e1 in
        let ctx' := Ctx.extend ctx (x, ty1) in
        (syn_rec counter) ctx' e2
      | Ap e1 e2 =>
        let! ty1 := (syn_rec counter) ctx e1 in
        match HTyp.matched_arrow ty1 with
        | Some (ty1_left, ty1_right) =>
          let! x := (ana_rec counter) ctx e2 ty1_left in
          match x with
          | _ => ret ty1_right
          end
        | _ => lift [_;_] "01" (raise_IllTyped tt)
        end
      | NumLit i =>
        lift [_;_] "01"
          (if OCaml.Pervasives.lt i 0 then
            raise_IllTyped tt
          else
            ret HTyp.Num)
      | Plus e1 e2 =>
        let! x := (ana_rec counter) ctx e1 HTyp.Num in
        match x with
        | _ =>
          let! x := (ana_rec counter) ctx e2 HTyp.Num in
          match x with
          | _ => ret HTyp.Num
          end
        end
      | EmptyHole => ret HTyp.Hole
      | NonEmptyHole e' =>
        let! x := (syn_rec counter) ctx e' in
        match x with
        | _ => ret HTyp.Hole
        end
      | _ => lift [_;_] "01" (raise_IllTyped tt)
      end
    end

  with ana_rec (counter : nat) (ctx : Ctx.t) (e : t) (ty : HTyp.t)
    : M [ NonTermination; IllTyped ] unit :=
    match counter with
    | O => lift [_;_] "10" (not_terminated tt)
    | S counter =>
      match e with
      | Let x e1 e2 =>
        let! ty1 := (syn_rec counter) ctx e1 in
        let ctx' := Ctx.extend ctx (x, ty1) in
        (ana_rec counter) ctx' e2 ty
      | Lam x e' =>
        match HTyp.matched_arrow ty with
        | Some (ty1, ty2) =>
          let ctx' := Ctx.extend ctx (x, ty1) in
          (ana_rec counter) ctx' e' ty2
        | _ => lift [_;_] "01" (raise_IllTyped tt)
        end
      | Inj side e' =>
        match HTyp.matched_sum ty with
        | Some (ty1, ty2) => (ana_rec counter) ctx e' (pick_side side ty1 ty2)
        | None => lift [_;_] "01" (raise_IllTyped tt)
        end
      | Case e' (x, e1) (y, e2) =>
        let! e'_ty := (syn_rec counter) ctx e' in
        match HTyp.matched_sum e'_ty with
        | Some (ty1, ty2) =>
          let ctx1 := Ctx.extend ctx (x, ty1) in
          let! x_1 := (ana_rec counter) ctx1 e1 ty in
          match x_1 with
          | tt =>
            let ctx2 := Ctx.extend ctx (y, ty2) in
            (ana_rec counter) ctx2 e2 ty
          end
        | _ => lift [_;_] "01" (raise_IllTyped tt)
        end
      | _ =>
        let! ty' := (syn_rec counter) ctx e in
        lift [_;_] "01"
          (if HTyp.consistent ty ty' then
            ret tt
          else
            raise_IllTyped tt)
      end
    end.

  Definition syn (ctx : Ctx.t) (e : t)
    : M [ Counter; NonTermination; IllTyped ] HTyp.t :=
    let! x := lift [_;_;_] "100" (read_counter tt) in
    lift [_;_;_] "011" (syn_rec x ctx e).

  Definition ana (ctx : Ctx.t) (e : t) (ty : HTyp.t)
    : M [ Counter; NonTermination; IllTyped ] unit :=
    let! x := lift [_;_;_] "100" (read_counter tt) in
    lift [_;_;_] "011" (ana_rec x ctx e ty).

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
  | Child : Z -> direction
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
  | SLit : Z -> shape
  | SPlus : shape
  | SInj : HExp.inj_side -> shape
  | SCase : Var.t -> Var.t -> shape
  | SNEHole : shape.

  Inductive t : Type :=
  | Move : direction -> t
  | Del : t
  | Construct : shape -> t
  | Finish : t.

  Definition InvalidAction := Effect.make unit unit.

  Definition raise_InvalidAction {A : Type} (x : unit) : M [ InvalidAction ] A :=
    fun s => (inr (inl x), s).

  Definition Impossible := Effect.make unit unit.

  Definition raise_Impossible {A : Type} (x : unit) : M [ Impossible ] A :=
    fun s => (inr (inl x), s).

  Fixpoint performTyp (a : t) (zty : ZTyp.t) : M [ InvalidAction ] ZTyp.t :=
    match (a, zty) with
    | (Move (Child 1), ZTyp.CursorT (HTyp.Arrow ty1 ty2)) =>
      ret (ZTyp.LeftArrow (ZTyp.CursorT ty1) ty2)
    | (Move (Child 2), ZTyp.CursorT (HTyp.Arrow ty1 ty2)) =>
      ret (ZTyp.RightArrow ty1 (ZTyp.CursorT ty2))
    | (Move Parent, ZTyp.LeftArrow (ZTyp.CursorT ty1) ty2) =>
      ret (ZTyp.CursorT (HTyp.Arrow ty1 ty2))
    | (Move Parent, ZTyp.RightArrow ty1 (ZTyp.CursorT ty2)) =>
      ret (ZTyp.CursorT (HTyp.Arrow ty1 ty2))
    | (Move (Child 1), ZTyp.CursorT (HTyp.Sum ty1 ty2)) =>
      ret (ZTyp.LeftSum (ZTyp.CursorT ty1) ty2)
    | (Move (Child 2), ZTyp.CursorT (HTyp.Sum ty1 ty2)) =>
      ret (ZTyp.RightSum ty1 (ZTyp.CursorT ty2))
    | (Move Parent, ZTyp.LeftSum (ZTyp.CursorT ty1) ty2) =>
      ret (ZTyp.CursorT (HTyp.Sum ty1 ty2))
    | (Move Parent, ZTyp.RightSum ty1 (ZTyp.CursorT ty2)) =>
      ret (ZTyp.CursorT (HTyp.Sum ty1 ty2))
    | (Del, ZTyp.CursorT ty) => ret (ZTyp.CursorT HTyp.Hole)
    | (Construct SArrow, ZTyp.CursorT ty) =>
      ret (ZTyp.RightArrow ty (ZTyp.CursorT HTyp.Hole))
    | (Construct SNum, ZTyp.CursorT HTyp.Hole) => ret (ZTyp.CursorT HTyp.Num)
    | (Construct SSum, ZTyp.CursorT ty) =>
      ret (ZTyp.RightSum ty (ZTyp.CursorT HTyp.Hole))
    | (_, ZTyp.LeftArrow zty1 ty2) =>
      let! zty1' := performTyp a zty1 in
      ret (ZTyp.LeftArrow zty1' ty2)
    | (_, ZTyp.RightArrow ty1 zty2) =>
      let! zty2' := performTyp a zty2 in
      ret (ZTyp.RightArrow ty1 zty2')
    | (_, ZTyp.LeftSum zty1 ty2) =>
      let! zty1' := performTyp a zty1 in
      ret (ZTyp.LeftSum zty1' ty2)
    | (_, ZTyp.RightSum ty1 zty2) =>
      let! zty2' := performTyp a zty2 in
      ret (ZTyp.RightSum ty1 zty2')
    | _ => raise_InvalidAction tt
    end.

  Fixpoint performEMove (action : t) (ze : ZExp.t)
    : M [ InvalidAction ] ZExp.t :=
    match action with
    | Move direction =>
      match (direction, ze) with
      | (Child 1, ZExp.CursorE (HExp.Asc e ty)) =>
        ret (ZExp.LeftAsc (ZExp.CursorE e) ty)
      | (Child 2, ZExp.CursorE (HExp.Asc e ty)) =>
        ret (ZExp.RightAsc e (ZTyp.CursorT ty))
      | (Parent, ZExp.LeftAsc (ZExp.CursorE e) ty) =>
        ret (ZExp.CursorE (HExp.Asc e ty))
      | (Parent, ZExp.RightAsc e (ZTyp.CursorT ty)) =>
        ret (ZExp.CursorE (HExp.Asc e ty))
      | (Child 1, ZExp.CursorE (HExp.Let x e e')) =>
        ret (ZExp.LetZ1 x (ZExp.CursorE e) e')
      | (Child 2, ZExp.CursorE (HExp.Let x e e')) =>
        ret (ZExp.LetZ2 x e (ZExp.CursorE e'))
      | (Parent, ZExp.LetZ1 x (ZExp.CursorE e) e') =>
        ret (ZExp.CursorE (HExp.Let x e e'))
      | (Parent, ZExp.LetZ2 x e (ZExp.CursorE e')) =>
        ret (ZExp.CursorE (HExp.Let x e e'))
      | (Child 1, ZExp.CursorE (HExp.Lam x e)) =>
        ret (ZExp.LamZ x (ZExp.CursorE e))
      | (Parent, ZExp.LamZ x (ZExp.CursorE e)) =>
        ret (ZExp.CursorE (HExp.Lam x e))
      | (Child 1, ZExp.CursorE (HExp.Ap e1 e2)) =>
        ret (ZExp.LeftAp (ZExp.CursorE e1) e2)
      | (Child 2, ZExp.CursorE (HExp.Ap e1 e2)) =>
        ret (ZExp.RightAp e1 (ZExp.CursorE e2))
      | (Parent, ZExp.LeftAp (ZExp.CursorE e1) e2) =>
        ret (ZExp.CursorE (HExp.Ap e1 e2))
      | (Parent, ZExp.RightAp e1 (ZExp.CursorE e2)) =>
        ret (ZExp.CursorE (HExp.Ap e1 e2))
      | (Child 1, ZExp.CursorE (HExp.Plus e1 e2)) =>
        ret (ZExp.LeftPlus (ZExp.CursorE e1) e2)
      | (Child 2, ZExp.CursorE (HExp.Plus e1 e2)) =>
        ret (ZExp.RightPlus e1 (ZExp.CursorE e2))
      | (Parent, ZExp.LeftPlus (ZExp.CursorE e1) e2) =>
        ret (ZExp.CursorE (HExp.Plus e1 e2))
      | (Parent, ZExp.RightPlus e1 (ZExp.CursorE e2)) =>
        ret (ZExp.CursorE (HExp.Plus e1 e2))
      | (Child 1, ZExp.CursorE (HExp.Inj side e)) =>
        ret (ZExp.InjZ side (ZExp.CursorE e))
      | (Parent, ZExp.InjZ side (ZExp.CursorE e)) =>
        ret (ZExp.CursorE (HExp.Inj side e))
      | (Child 1, ZExp.CursorE (HExp.Case e branch1 branch2)) =>
        ret (ZExp.CaseZ1 (ZExp.CursorE e) branch1 branch2)
      | (Child 2, ZExp.CursorE (HExp.Case e (x, e1) branch2)) =>
        ret (ZExp.CaseZ2 e (x, (ZExp.CursorE e1)) branch2)
      | (Child 3, ZExp.CursorE (HExp.Case e branch1 (y, e2))) =>
        ret (ZExp.CaseZ3 e branch1 (y, (ZExp.CursorE e2)))
      | (Parent, ZExp.CaseZ1 (ZExp.CursorE e) branch1 branch2) =>
        ret (ZExp.CursorE (HExp.Case e branch1 branch2))
      | (Parent, ZExp.CaseZ2 e (x, ZExp.CursorE e1) branch2) =>
        ret (ZExp.CursorE (HExp.Case e (x, e1) branch2))
      | (Parent, ZExp.CaseZ3 e branch1 (y, ZExp.CursorE e2)) =>
        ret (ZExp.CursorE (HExp.Case e branch1 (y, e2)))
      | (Child 1, ZExp.CursorE (HExp.NonEmptyHole e)) =>
        ret (ZExp.NonEmptyHoleZ (ZExp.CursorE e))
      | (Parent, ZExp.NonEmptyHoleZ (ZExp.CursorE e)) =>
        ret (ZExp.CursorE (HExp.NonEmptyHole e))
      | _ => raise_InvalidAction tt
      end
    | _ => raise_InvalidAction tt
    end.

  Definition hsyn (ctx : Ctx.t) (e : HExp.t)
    : M [ Counter; NonTermination; InvalidAction ] HTyp.t :=
    let! x := lift [_;_;_] "110" (Exception.run 2 (HExp.syn ctx e) tt) in
    match x with
    | inl x => ret x
    | inr tt => lift [_;_;_] "001" (raise_InvalidAction tt)
    end.

  Definition hana (ctx : Ctx.t) (e : HExp.t) (ty : HTyp.t)
    : M [ Counter; NonTermination; InvalidAction ] unit :=
    let! x := lift [_;_;_] "110" (Exception.run 2 (HExp.ana ctx e ty) tt) in
    match x with
    | inl x => ret x
    | inr tt => lift [_;_;_] "001" (raise_InvalidAction tt)
    end.

End Action.

Extract Inductive bool => "bool" ["true" "false"].
Extract Constant negb => "not".
Extract Inductive option => "option" ["Some" "None"].
Extract Inductive prod => "(*)" ["(,)"].
Extract Inductive list => "list" [ "[]" "(::)" ].
Extraction Action.
