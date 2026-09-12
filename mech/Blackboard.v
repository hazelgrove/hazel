(* ------------------------------------------------------------------------ *)
(*  Blackboard.v                                                            *)
(*                                                                          *)
(*  Mechanized metatheory for the core logic of Blackboard: A Clean Slate  *)
(*  Proof Assistant (Figure 1), in both the form first printed and the     *)
(*  author's corrected form (arrow+ with premise G |- T1 : type).           *)
(*  Part I: Theorem 1, consistency: no closed instance of (X : type) -> X  *)
(*  is derivable.  Parts II-III: Theorem 2, conservativity of construction *)
(*  blocks.  Part IV: a five-point model settling regularity questions.    *)
(*                                                                          *)
(*  Method: a two-point model.  Every term denotes a Boolean; "t : T" is    *)
(*  inhabited iff T is; the dependent function type is implication, with   *)
(*  its bound variable ranging over {true,false}; applications denote      *)
(*  true.  All eight rules preserve truth; (X : type) -> X denotes          *)
(*  "for all b, b", which is false.                                         *)
(*                                                                          *)
(*  Checked with Rocq 9.2.  No axioms (see Print Assumptions at the end).  *)
(* ------------------------------------------------------------------------ *)

From Stdlib Require Import List Bool Arith Lia.
Import ListNotations.

(** * Syntax (Figure 1, first three lines), with de Bruijn indices *)

Inductive term : Type :=
| Var : nat -> term            (* identifier x                        *)
| Mem : term -> term -> term   (* t : T   (internal type membership)  *)
| Ty  : term                   (* type                                *)
| Pi  : term -> term -> term   (* (x : A) -> B;  B binds Var 0         *)
| App : term -> term -> term.  (* t u                                 *)

(** Renaming and parallel substitution. *)

Definition up_ren (xi : nat -> nat) : nat -> nat :=
  fun n => match n with 0 => 0 | S n => S (xi n) end.

Fixpoint rename (xi : nat -> nat) (t : term) : term :=
  match t with
  | Var n   => Var (xi n)
  | Mem t T => Mem (rename xi t) (rename xi T)
  | Ty      => Ty
  | Pi A B  => Pi (rename xi A) (rename (up_ren xi) B)
  | App t u => App (rename xi t) (rename xi u)
  end.

(** Weakening by one variable, and by n. *)
Definition shift : term -> term := rename S.

Fixpoint shiftn (k : nat) (t : term) : term :=
  match k with 0 => t | S k => shift (shiftn k t) end.

Definition up_sub (sigma : nat -> term) : nat -> term :=
  fun n => match n with 0 => Var 0 | S n => shift (sigma n) end.

Fixpoint subst (sigma : nat -> term) (t : term) : term :=
  match t with
  | Var n   => sigma n
  | Mem t T => Mem (subst sigma t) (subst sigma T)
  | Ty      => Ty
  | Pi A B  => Pi (subst sigma A) (subst (up_sub sigma) B)
  | App t u => App (subst sigma t) (subst sigma u)
  end.

Definition scons (a : term) (sigma : nat -> term) : nat -> term :=
  fun n => match n with 0 => a | S n => sigma n end.

(** B[a/x], the substitution used by the ap rule. *)
Definition subst1 (B a : term) : term := subst (scons a Var) B.

(** * The eight rules of Figure 1 *)

(** A context is a list of types.  Entry n is the type of Var n and lives
    in the context after position n, so at position n it is weakened by
    n+1.  The paper's side condition x \notin fv(T') on cut is expressed
    by requiring the premise's goal to be a weakening (shift T').  There
    is no well-formedness condition on contexts: none is in Figure 1.

    Two versions of arrow+ are in play.  As first printed, arrow+ had the
    single premise  G, x : T1 |- T2.  The author's corrected Figure 1 adds
    the premise  G |- T1 : type.  Everything below is developed for both at
    once: the section variable s selects the corrected form (s = true) or
    the printed form (s = false); the extra premise is guarded by s = true.
    After the section, [corrected] and [printed] name the two systems. *)

Definition ctx := list term.

Section Rules.
Variable s : bool.

Reserved Notation "G |- T" (at level 70).

Inductive derivable : ctx -> term -> Prop :=
| r_hyp G n T :
    nth_error G n = Some T ->
    G |- Mem (Var n) (shiftn (S n) T)
| r_in G t T T' :
    G |- Mem T Ty ->
    G |- Mem t T' ->
    G |- Mem (Mem t T) Ty
| r_in_minus G t T :
    G |- Mem t T ->
    G |- T
| r_cut G T T' :
    G |- T ->
    (T :: G) |- shift T' ->
    G |- T'
| r_type G :
    G |- Mem Ty Ty
| r_arrow G A B :
    G |- Mem A Ty ->
    (A :: G) |- Mem B Ty ->
    G |- Mem (Pi A B) Ty
| r_ap G f a A B :
    G |- Mem f (Pi A B) ->
    G |- Mem a A ->
    G |- Mem (App f a) (subst1 B a)
| r_arrow_plus G A B :
    (s = true -> G |- Mem A Ty) ->     (* corrected form only *)
    (A :: G) |- B ->
    G |- Pi A B
where "G |- T" := (derivable G T).

(** * The two-point model *)

Definition env := nat -> bool.
Definition econs (b : bool) (rho : env) : env :=
  fun n => match n with 0 => b | S n => rho n end.
Definition etail (rho : env) : env := fun n => rho (S n).

(** [eval T rho = true] reads "T is inhabited under rho".
    For Pi we write  A -> (B[true] /\ B[false]),  which is the same
    truth function as  forall b in {true,false}, A -> B[b]. *)
Fixpoint eval (t : term) (rho : env) : bool :=
  match t with
  | Var n   => rho n
  | Mem _ T => eval T rho
  | Ty      => true
  | Pi A B  => implb (eval A rho) (eval B (econs true rho) && eval B (econs false rho))
  | App _ _ => true
  end.

(** rho satisfies G when every type in G is inhabited under the
    environment for its own position. *)
Fixpoint sat (rho : env) (G : ctx) : Prop :=
  match G with
  | []      => True
  | T :: G' => eval T (etail rho) = true /\ sat (etail rho) G'
  end.

(** ** Environments are used extensionally *)

Lemma econs_ext : forall b rho rho',
  (forall n, rho n = rho' n) -> forall n, econs b rho n = econs b rho' n.
Proof. intros b rho rho' H [|n]; simpl; auto. Qed.

Lemma eval_ext : forall t rho rho',
  (forall n, rho n = rho' n) -> eval t rho = eval t rho'.
Proof.
  induction t as [n | t IHt T IHT | | A IHA B IHB | t IHt u IHu];
    intros rho rho' H; simpl.
  - apply H.
  - exact (IHT rho rho' H).
  - reflexivity.
  - rewrite (IHA rho rho' H),
            (IHB (econs true rho) (econs true rho') (econs_ext _ _ _ H)),
            (IHB (econs false rho) (econs false rho') (econs_ext _ _ _ H)).
    reflexivity.
  - reflexivity.
Qed.

(** ** Renaming lemma *)

Lemma up_ren_econs : forall xi b rho n,
  econs b rho (up_ren xi n) = econs b (fun m => rho (xi m)) n.
Proof. intros xi b rho [|n]; reflexivity. Qed.

Lemma eval_rename : forall t xi rho,
  eval (rename xi t) rho = eval t (fun n => rho (xi n)).
Proof.
  induction t as [n | t IHt T IHT | | A IHA B IHB | t IHt u IHu];
    intros xi rho; simpl.
  - reflexivity.
  - apply IHT.
  - reflexivity.
  - rewrite IHA,
            (IHB (up_ren xi) (econs true rho)),
            (IHB (up_ren xi) (econs false rho)),
            (eval_ext B _ _ (up_ren_econs xi true rho)),
            (eval_ext B _ _ (up_ren_econs xi false rho)).
    reflexivity.
  - reflexivity.
Qed.

Lemma eval_shift : forall t rho, eval (shift t) rho = eval t (etail rho).
Proof. intros t rho. unfold shift. rewrite eval_rename. reflexivity. Qed.

Fixpoint etailn (k : nat) (rho : env) : env :=
  match k with 0 => rho | S k => etailn k (etail rho) end.

Lemma eval_shiftn : forall k t rho, eval (shiftn k t) rho = eval t (etailn k rho).
Proof.
  induction k as [|k IH]; intros t rho.
  - reflexivity.
  - change (eval (shift (shiftn k t)) rho = eval t (etailn k (etail rho))).
    rewrite eval_shift. apply IH.
Qed.

(** ** Substitution lemma *)

Lemma up_sub_econs : forall sigma b rho n,
  eval (up_sub sigma n) (econs b rho) = econs b (fun m => eval (sigma m) rho) n.
Proof.
  intros sigma b rho n. unfold up_sub, shift. destruct n as [|n].
  - reflexivity.
  - rewrite eval_rename. apply eval_ext. intro k. reflexivity.
Qed.

Lemma eval_subst : forall t sigma rho,
  eval (subst sigma t) rho = eval t (fun n => eval (sigma n) rho).
Proof.
  induction t as [n | t IHt T IHT | | A IHA B IHB | t IHt u IHu];
    intros sigma rho; simpl.
  - reflexivity.
  - apply IHT.
  - reflexivity.
  - rewrite IHA,
            (IHB (up_sub sigma) (econs true rho)),
            (IHB (up_sub sigma) (econs false rho)),
            (eval_ext B _ _ (up_sub_econs sigma true rho)),
            (eval_ext B _ _ (up_sub_econs sigma false rho)).
    reflexivity.
  - reflexivity.
Qed.

Lemma eval_subst1 : forall B a rho,
  eval (subst1 B a) rho = eval B (econs (eval a rho) rho).
Proof.
  intros B a rho. unfold subst1. rewrite eval_subst.
  apply eval_ext. intros [|n]; reflexivity.
Qed.

(** ** Contexts *)

Lemma sat_nth : forall G n T rho,
  sat rho G -> nth_error G n = Some T -> eval T (etailn (S n) rho) = true.
Proof.
  induction G as [|T0 G IH]; intros n T rho Hsat Hnth.
  - destruct n; discriminate.
  - destruct n as [|n]; simpl in Hnth.
    + injection Hnth as Heq. subst T. destruct Hsat as [H _]. exact H.
    + destruct Hsat as [_ Hsat']. exact (IH n T (etail rho) Hsat' Hnth).
Qed.

(** A context entry constrains only the type, never the value of the
    variable: any b extends a satisfying rho. *)
Lemma sat_econs : forall G A b rho,
  sat rho G -> eval A rho = true -> sat (econs b rho) (A :: G).
Proof.
  intros G A b rho Hsat HA. split.
  - rewrite (eval_ext A (etail (econs b rho)) rho (fun n => eq_refl)). exact HA.
  - exact Hsat.
Qed.

(** * Soundness: every derivable judgment is true in the model *)

Theorem soundness : forall G T, G |- T -> forall rho, sat rho G -> eval T rho = true.
Proof.
  intros G T D. induction D as [G n T H | G t T T' D1 IHD1 D2 IHD2 | G t T D IHD | G T T' D1 IHD1 D2 IHD2 | G | G A B D1 IHD1 D2 IHD2 | G f a A B D1 IHD1 D2 IHD2 | G A B H1 IHD1 D2 IHD2]; intros rho Hsat.
  - (* hyp *)
    change (eval (shiftn (S n) T) rho = true).
    rewrite eval_shiftn. exact (sat_nth G n T rho Hsat H).
  - (* in *)      reflexivity.
  - (* in- *)     exact (IHD rho Hsat).
  - (* cut *)
    specialize (IHD2 (econs true rho) (sat_econs G T true rho Hsat (IHD1 rho Hsat))).
    rewrite eval_shift in IHD2.
    rewrite (eval_ext T' (etail (econs true rho)) rho (fun n => eq_refl)) in IHD2.
    exact IHD2.
  - (* type *)    reflexivity.
  - (* arrow *)   reflexivity.
  - (* ap *)
    change (eval (subst1 B a) rho = true).
    rewrite eval_subst1.
    specialize (IHD1 rho Hsat). specialize (IHD2 rho Hsat).
    change (eval A rho = true) in IHD2.
    change (implb (eval A rho) (eval B (econs true rho) && eval B (econs false rho)) = true) in IHD1.
    rewrite IHD2 in IHD1. simpl in IHD1.
    apply andb_true_iff in IHD1. destruct IHD1 as [Ht Hf].
    destruct (eval a rho); assumption.
  - (* arrow+ *)
    simpl. destruct (eval A rho) eqn:HA.
    + simpl.
      rewrite (IHD2 (econs true rho) (sat_econs G A true rho Hsat HA)),
              (IHD2 (econs false rho) (sat_econs G A false rho Hsat HA)).
      reflexivity.
    + reflexivity.
Qed.

(** * Consistency *)

(** (X : type) -> X, in de Bruijn form. *)
Definition absurd : term := Pi Ty (Var 0).

Theorem consistency : ~ ([] |- absurd).
Proof.
  intro D.
  pose proof (soundness [] absurd D (fun _ => false) I) as H.
  discriminate H.
Qed.

(** More generally: any closed type that is false under some valuation
    is underivable in the empty context. *)
Corollary underivable_if_refutable : forall T rho,
  eval T rho = false -> ~ ([] |- T).
Proof.
  intros T rho Hf D. rewrite (soundness [] T D rho I) in Hf. discriminate.
Qed.

(** * Remarks checked alongside *)

(** Two of the paper's postulates, with eq as a free variable (Var 0 at
    the outer level, so shifted under binders).  The type of refl is
    valid in the model; the type of replace is not.  This is why
    Theorem 1 says nothing about the Section 4 assumption blocks: the
    model validates formation and introduction postulates but not the
    ones with logical content. *)

(* refl : (A : type) -> (a : A) -> eq A a a *)
Definition refl_ty : term :=
  Pi Ty (Pi (Var 0) (App (App (App (Var 2) (Var 1)) (Var 0)) (Var 0))).

(* replace : (A1 A2 : type) -> eq type A1 A2 -> A1 -> A2 *)
Definition replace_ty : term :=
  Pi Ty (Pi Ty (Pi (App (App (App (Var 2) Ty) (Var 1)) (Var 0))
                   (Pi (Var 2) (Var 2)))).

Example refl_ty_valid : forall rho, eval refl_ty rho = true.
Proof. intro rho. reflexivity. Qed.

Example replace_ty_not_valid : eval replace_ty (fun _ => false) = false.
Proof. reflexivity. Qed.

Corollary replace_not_derivable_bare : ~ ([] |- replace_ty).
Proof. exact (underivable_if_refutable replace_ty (fun _ => false) replace_ty_not_valid). Qed.


(* ======================================================================== *)
(*  Part II.  Renaming of derivations, weakening, and Theorem 2:            *)
(*  a construction block with one declaration is a conservative extension.  *)
(* ======================================================================== *)

(** * Substitution algebra (the usual de Bruijn lemmas) *)

Lemma up_ren_ext : forall xi xi',
  (forall n, xi n = xi' n) -> forall n, up_ren xi n = up_ren xi' n.
Proof. intros xi xi' H [|n]; simpl; auto. Qed.

Lemma rename_ext : forall t xi xi',
  (forall n, xi n = xi' n) -> rename xi t = rename xi' t.
Proof.
  induction t as [n | t IHt T IHT | | A IHA B IHB | t IHt u IHu];
    intros xi xi' H; simpl.
  - rewrite H. reflexivity.
  - rewrite (IHt _ _ H), (IHT _ _ H). reflexivity.
  - reflexivity.
  - rewrite (IHA _ _ H), (IHB _ _ (up_ren_ext _ _ H)). reflexivity.
  - rewrite (IHt _ _ H), (IHu _ _ H). reflexivity.
Qed.

Lemma up_ren_comp : forall xi zeta n,
  up_ren xi (up_ren zeta n) = up_ren (fun m => xi (zeta m)) n.
Proof. intros xi zeta [|n]; reflexivity. Qed.

Lemma rename_comp : forall t xi zeta,
  rename xi (rename zeta t) = rename (fun n => xi (zeta n)) t.
Proof.
  induction t as [n | t IHt T IHT | | A IHA B IHB | t IHt u IHu];
    intros xi zeta; simpl.
  - reflexivity.
  - rewrite IHt, IHT. reflexivity.
  - reflexivity.
  - rewrite IHA, IHB, (rename_ext B _ _ (up_ren_comp xi zeta)). reflexivity.
  - rewrite IHt, IHu. reflexivity.
Qed.

Lemma up_sub_ext : forall sigma sigma',
  (forall n, sigma n = sigma' n) -> forall n, up_sub sigma n = up_sub sigma' n.
Proof. intros sigma sigma' H [|n]; simpl; [reflexivity | rewrite H; reflexivity]. Qed.

Lemma subst_ext : forall t sigma sigma',
  (forall n, sigma n = sigma' n) -> subst sigma t = subst sigma' t.
Proof.
  induction t as [n | t IHt T IHT | | A IHA B IHB | t IHt u IHu];
    intros sigma sigma' H; simpl.
  - apply H.
  - rewrite (IHt _ _ H), (IHT _ _ H). reflexivity.
  - reflexivity.
  - rewrite (IHA _ _ H), (IHB _ _ (up_sub_ext _ _ H)). reflexivity.
  - rewrite (IHt _ _ H), (IHu _ _ H). reflexivity.
Qed.

Lemma up_sub_ren : forall sigma zeta n,
  up_sub sigma (up_ren zeta n) = up_sub (fun m => sigma (zeta m)) n.
Proof. intros sigma zeta [|n]; reflexivity. Qed.

Lemma subst_rename : forall t sigma zeta,
  subst sigma (rename zeta t) = subst (fun n => sigma (zeta n)) t.
Proof.
  induction t as [n | t IHt T IHT | | A IHA B IHB | t IHt u IHu];
    intros sigma zeta; simpl.
  - reflexivity.
  - rewrite IHt, IHT. reflexivity.
  - reflexivity.
  - rewrite IHA, IHB, (subst_ext B _ _ (up_sub_ren sigma zeta)). reflexivity.
  - rewrite IHt, IHu. reflexivity.
Qed.

Lemma up_ren_sub : forall xi sigma n,
  rename (up_ren xi) (up_sub sigma n) = up_sub (fun m => rename xi (sigma m)) n.
Proof.
  intros xi sigma [|n].
  - reflexivity.
  - unfold up_sub, shift. rewrite !rename_comp. apply rename_ext. intro m. reflexivity.
Qed.

Lemma rename_subst : forall t xi sigma,
  rename xi (subst sigma t) = subst (fun n => rename xi (sigma n)) t.
Proof.
  induction t as [n | t IHt T IHT | | A IHA B IHB | t IHt u IHu];
    intros xi sigma; simpl.
  - reflexivity.
  - rewrite IHt, IHT. reflexivity.
  - reflexivity.
  - rewrite IHA, IHB, (subst_ext B _ _ (up_ren_sub xi sigma)). reflexivity.
  - rewrite IHt, IHu. reflexivity.
Qed.

Lemma up_sub_id : forall n, up_sub Var n = Var n.
Proof. intros [|n]; reflexivity. Qed.

Lemma subst_id : forall t, subst Var t = t.
Proof.
  induction t as [n | t IHt T IHT | | A IHA B IHB | t IHt u IHu]; simpl.
  - reflexivity.
  - rewrite IHt, IHT. reflexivity.
  - reflexivity.
  - rewrite IHA, (subst_ext B _ _ up_sub_id), IHB. reflexivity.
  - rewrite IHt, IHu. reflexivity.
Qed.

(** The three facts the derivations below actually use. *)

Lemma rename_subst1 : forall xi B a,
  rename xi (subst1 B a) = subst1 (rename (up_ren xi) B) (rename xi a).
Proof.
  intros xi B a. unfold subst1. rewrite rename_subst, subst_rename.
  apply subst_ext. intros [|n]; reflexivity.
Qed.

Lemma shift_rename : forall xi t, rename (up_ren xi) (shift t) = shift (rename xi t).
Proof.
  intros xi t. unfold shift. rewrite !rename_comp. apply rename_ext. intro n. reflexivity.
Qed.

(** Substituting into a weakened term is the identity: (shift T)[a/x] = T. *)
Lemma subst_scons_shift : forall a X, subst (scons a Var) (shift X) = X.
Proof.
  intros a X. unfold shift. rewrite subst_rename.
  transitivity (subst Var X); [apply subst_ext; intro n; reflexivity | apply subst_id].
Qed.

Lemma shift_subst1 : forall a T, subst1 (shift T) a = T.
Proof. intros a T. apply subst_scons_shift. Qed.

(** * Renaming of derivations *)

(** [ren_ok xi G G'] : the renaming xi sends every variable of G to a
    variable of G' with the correspondingly renamed type. *)
Definition ren_ok (xi : nat -> nat) (G G' : ctx) : Prop :=
  forall n T, nth_error G n = Some T ->
    exists T', nth_error G' (xi n) = Some T'
            /\ shiftn (S (xi n)) T' = rename xi (shiftn (S n) T).

Lemma ren_ok_up : forall xi G G' A,
  ren_ok xi G G' -> ren_ok (up_ren xi) (A :: G) (rename xi A :: G').
Proof.
  intros xi G G' A H [|n] T Hn.
  - simpl in Hn. injection Hn as Heq. subst T.
    exists (rename xi A). split; [reflexivity |].
    change (shift (rename xi A) = rename (up_ren xi) (shift A)).
    symmetry. apply shift_rename.
  - simpl in Hn. destruct (H n T Hn) as [T' [HT' Heq]].
    exists T'. split; [exact HT' |].
    change (shift (shiftn (S (xi n)) T') = rename (up_ren xi) (shift (shiftn (S n) T))).
    rewrite Heq. symmetry. apply shift_rename.
Qed.

Lemma derivable_rename : forall G T,
  G |- T -> forall xi G', ren_ok xi G G' -> G' |- rename xi T.
Proof.
  intros G T D. induction D as [G n T H | G t T T' D1 IHD1 D2 IHD2 | G t T D IHD | G T T' D1 IHD1 D2 IHD2 | G | G A B D1 IHD1 D2 IHD2 | G f a A B D1 IHD1 D2 IHD2 | G A B H1 IHD1 D2 IHD2]; intros xi G' Hok; cbn [rename].
  - (* hyp *)
    destruct (Hok n T H) as [T' [HT' Heq]].
    rewrite <- Heq. apply r_hyp. exact HT'.
  - (* in *)
    apply r_in with (T' := rename xi T'); [exact (IHD1 xi G' Hok) | exact (IHD2 xi G' Hok)].
  - (* in- *)
    apply r_in_minus with (t := rename xi t). exact (IHD xi G' Hok).
  - (* cut *)
    apply r_cut with (T := rename xi T); [exact (IHD1 xi G' Hok) |].
    rewrite <- shift_rename. exact (IHD2 (up_ren xi) _ (ren_ok_up xi G G' T Hok)).
  - (* type *)
    apply r_type.
  - (* arrow *)
    apply r_arrow; [exact (IHD1 xi G' Hok) | exact (IHD2 (up_ren xi) _ (ren_ok_up xi G G' A Hok))].
  - (* ap *)
    rewrite rename_subst1.
    apply r_ap with (A := rename xi A); [exact (IHD1 xi G' Hok) | exact (IHD2 xi G' Hok)].
  - (* arrow+ *)
    apply r_arrow_plus.
    + intro Hs. exact (IHD1 Hs xi G' Hok).
    + exact (IHD2 (up_ren xi) _ (ren_ok_up xi G G' A Hok)).
Qed.

(** * Weakening *)

Lemma ren_ok_shift : forall G U, ren_ok S G (U :: G).
Proof. intros G U n T Hn. exists T. split; [exact Hn | reflexivity]. Qed.

Theorem weakening : forall G T U, G |- T -> (U :: G) |- shift T.
Proof. intros G T U D. exact (derivable_rename G T D S (U :: G) (ren_ok_shift G U)). Qed.

(** * Theorem 2 (one declaration): construction blocks are conservative *)

(** The obligation of a construction block with signature x : T1, from
    l. 150-151 of the paper:  (M : type) -> ((x : T1) -> M) -> M.
    In de Bruijn form M is Var 1 under the two inner binders, and T1 is
    weakened past the binder for M. *)
Definition witness1 (T1 : term) : term :=
  Pi Ty (Pi (Pi (shift T1) (Var 1)) (Var 1)).

Lemma shift_witness1 : forall X, shift (witness1 X) = witness1 (shift X).
Proof.
  intro X. unfold witness1.
  change (Pi Ty (Pi (Pi (rename (up_ren S) (shift X)) (Var 1)) (Var 1))
          = Pi Ty (Pi (Pi (shift (shift X)) (Var 1)) (Var 1))).
  rewrite shift_rename. reflexivity.
Qed.

(** ((x : X) -> M) -> M with M := a. *)
Lemma subst1_body : forall X a,
  subst1 (Pi (Pi (shift X) (Var 1)) (Var 1)) a = Pi (Pi X (shift a)) (shift a).
Proof.
  intros X a. unfold subst1.
  change (Pi (Pi (subst (scons a Var) (shift X)) (shift a)) (shift a)
          = Pi (Pi X (shift a)) (shift a)).
  rewrite subst_scons_shift. reflexivity.
Qed.

(** If the block's obligation is met, then anything proved with the new
    declaration x : T1, and not mentioning x, was already provable,
    PROVIDED it is a type.  The last hypothesis is not in the paper: it is
    forced by the second premise of the ap rule, since the witness has to
    be applied to T' : type. *)
Theorem conservativity1 : forall G T1 T',
  (s = true -> G |- Mem T1 Ty) ->  (* the declaration's type is a type *)
  G |- witness1 T1 ->
  (T1 :: G) |- shift T' ->
  G |- Mem T' Ty ->
  G |- T'.
Proof.
  intros G T1 T' HT1 HW HD HT.
  set (S1 := Pi T1 (shift T')).
  (* (x : T1) -> T' is inhabited, by arrow+ *)
  assert (HS : G |- S1) by (apply r_arrow_plus; [exact HT1 | exact HD]).
  (* cut in y : witness1 T1, then z : (x : T1) -> T' *)
  apply r_cut with (T := witness1 T1); [exact HW |].
  apply r_cut with (T := shift S1); [apply weakening; exact HS |].
  set (G2 := shift S1 :: witness1 T1 :: G).
  set (T2 := shift (shift T')).
  (* y = Var 1 *)
  assert (Hy : G2 |- Mem (Var 1) (witness1 (shift (shift T1)))).
  { rewrite <- !shift_witness1. apply (r_hyp G2 1 (witness1 T1)). reflexivity. }
  (* z = Var 0 *)
  assert (Hz : G2 |- Mem (Var 0) (Pi (shift (shift T1)) (shift T2))).
  { assert (E : shift (shift S1) = Pi (shift (shift T1)) (shift T2)).
    { unfold S1, T2.
      change (Pi (shift (shift T1)) (rename (up_ren S) (rename (up_ren S) (shift T')))
              = Pi (shift (shift T1)) (shift (shift (shift T')))).
      rewrite !shift_rename. reflexivity. }
    rewrite <- E. apply (r_hyp G2 0 (shift S1)). reflexivity. }
  (* T' is still a type two hypotheses later *)
  assert (HT2 : G2 |- Mem T2 Ty).
  { apply (weakening (witness1 T1 :: G) (Mem (shift T') Ty) (shift S1)).
    apply (weakening G (Mem T' Ty) (witness1 T1)). exact HT. }
  (* y T' : ((x : T1) -> T') -> T' *)
  assert (Hy1 : G2 |- Mem (App (Var 1) T2) (Pi (Pi (shift (shift T1)) (shift T2)) (shift T2))).
  { rewrite <- subst1_body. apply r_ap with (A := Ty); [exact Hy | exact HT2]. }
  (* y T' z : T' *)
  pose proof (r_ap G2 (App (Var 1) T2) (Var 0) (Pi (shift (shift T1)) (shift T2)) (shift T2) Hy1 Hz) as Hyz.
  rewrite shift_subst1 in Hyz.
  exact (r_in_minus G2 _ T2 Hyz).
Qed.


(* ======================================================================== *)
(*  Part III.  Theorem 2 in general: a construction block with a signature *)
(*  x1 : T1, ..., xn : Tn is a conservative extension.                       *)
(* ======================================================================== *)

(** * Conservativity for an arbitrary body Q *)

(** The one-declaration proof above only used the shape
      (M : type) -> Q[M] -> M
    of the witness type, never the shape of Q.  So prove it once for any Q. *)
Theorem conservativity_gen : forall G Q T',
  G |- Pi Ty (Pi Q (Var 1)) ->
  G |- subst1 Q T' ->
  G |- Mem T' Ty ->
  G |- T'.
Proof.
  intros G Q T' HW HS HT.
  apply r_cut with (T := Pi Ty (Pi Q (Var 1))); [exact HW |].
  apply r_cut with (T := shift (subst1 Q T')); [apply weakening; exact HS |].
  set (W := Pi Ty (Pi Q (Var 1))).
  set (G2 := shift (subst1 Q T') :: W :: G).
  set (T2 := shift (shift T')).
  set (Q2 := rename (up_ren S) (rename (up_ren S) Q)).
  (* y = Var 1 : (M : type) -> Q2[M] -> M *)
  assert (Hy : G2 |- Mem (Var 1) (Pi Ty (Pi Q2 (Var 1)))).
  { apply (r_hyp G2 1 W). reflexivity. }
  (* z = Var 0 : Q2[T'] *)
  assert (Hz : G2 |- Mem (Var 0) (subst1 Q2 T2)).
  { unfold Q2, T2, shift. rewrite <- !rename_subst1.
    apply (r_hyp G2 0 (shift (subst1 Q T'))). reflexivity. }
  (* T' is still a type *)
  assert (HT2 : G2 |- Mem T2 Ty).
  { apply (weakening (W :: G) (Mem (shift T') Ty)).
    apply (weakening G (Mem T' Ty)). exact HT. }
  (* y T' : Q2[T'] -> T' *)
  assert (Hy1 : G2 |- Mem (App (Var 1) T2) (Pi (subst1 Q2 T2) (shift T2))).
  { change (G2 |- Mem (App (Var 1) T2) (subst1 (Pi Q2 (Var 1)) T2)).
    apply r_ap with (A := Ty); [exact Hy | exact HT2]. }
  (* y T' z : T' *)
  pose proof (r_ap G2 (App (Var 1) T2) (Var 0) (subst1 Q2 T2) (shift T2) Hy1 Hz) as Hyz.
  rewrite shift_subst1 in Hyz.
  exact (r_in_minus G2 _ T2 Hyz).
Qed.

(** * Signatures as telescopes *)

(** A signature is a list of types, outermost first; T_{i+1} may mention
    x_1 .. x_i.  [tele Ts M] is (x1 : T1) -> ... -> (xn : Tn) -> M. *)
Fixpoint tele (Ts : list term) (M : term) : term :=
  match Ts with [] => M | T :: Ts' => Pi T (tele Ts' M) end.

(** Renaming a signature, deeper entries under more binders. *)
Fixpoint rename_sig (xi : nat -> nat) (Ts : list term) : list term :=
  match Ts with [] => [] | T :: Ts' => rename xi T :: rename_sig (up_ren xi) Ts' end.

(** The obligation of a construction block (l. 150-151):
      (M : type) -> ((x1 : T1) -> ... -> (xn : Tn) -> M) -> M.
    The signature is weakened past the binder for M; under the n binders
    of the telescope, M is Var n. *)
Definition witness (Ts : list term) : term :=
  Pi Ty (Pi (tele (rename_sig S Ts) (Var (length Ts))) (Var 1)).

Lemma witness_one : forall T1, witness [T1] = witness1 T1.
Proof. reflexivity. Qed.

(** A well-formed signature: each type is a type in the context of the
    entries before it (the obligation of an assumption block, l. 149).
    Only demanded in the corrected system. *)
Fixpoint wf_sig (G : ctx) (Ts : list term) : Prop :=
  match Ts with
  | []      => True
  | T :: Ts' => (s = true -> G |- Mem T Ty) /\ wf_sig (T :: G) Ts'
  end.

(** n uses of arrow+. *)
Lemma tele_intro : forall Ts G M, wf_sig G Ts -> (rev Ts ++ G) |- M -> G |- tele Ts M.
Proof.
  induction Ts as [|T Ts IH]; intros G M Hwf H.
  - exact H.
  - destruct Hwf as [HT Hwf]. simpl. apply r_arrow_plus; [exact HT |].
    apply IH; [exact Hwf |]. simpl in H. rewrite <- app_assoc in H. exact H.
Qed.

(** ** Iterated up, and how it acts on variables *)

Fixpoint upn (k : nat) (xi : nat -> nat) : nat -> nat :=
  match k with 0 => xi | S k => up_ren (upn k xi) end.

Fixpoint upsn (k : nat) (sigma : nat -> term) : nat -> term :=
  match k with 0 => sigma | S k => up_sub (upsn k sigma) end.

Lemma upn_lt : forall k xi m, m < k -> upn k xi m = m.
Proof.
  induction k as [|k IH]; intros xi m Hm.
  - inversion Hm.
  - destruct m as [|m]; [reflexivity | simpl; rewrite (IH xi m); [reflexivity | lia]].
Qed.

Lemma upn_ge : forall k xi j, upn k xi (k + j) = k + xi j.
Proof.
  induction k as [|k IH]; intros xi j.
  - reflexivity.
  - simpl. rewrite IH. reflexivity.
Qed.

Lemma upsn_lt : forall k sigma m, m < k -> upsn k sigma m = Var m.
Proof.
  induction k as [|k IH]; intros sigma m Hm.
  - inversion Hm.
  - destruct m as [|m]; [reflexivity | simpl; rewrite (IH sigma m); [reflexivity | lia]].
Qed.

Lemma shiftn_var : forall k j, shiftn k (Var j) = Var (k + j).
Proof.
  induction k as [|k IH]; intros j; [reflexivity | simpl; rewrite IH; reflexivity].
Qed.

Lemma upsn_ge : forall k sigma j, upsn k sigma (k + j) = shiftn k (sigma j).
Proof.
  induction k as [|k IH]; intros sigma j.
  - reflexivity.
  - simpl. rewrite IH. reflexivity.
Qed.

(** Under k binders, substituting a for the variable just outside them
    undoes the weakening past that variable. *)
Lemma upsn_upn_cancel : forall k a m, upsn k (scons a Var) (upn k S m) = Var m.
Proof.
  intros k a m. destruct (Nat.lt_ge_cases m k) as [Hlt | Hge].
  - rewrite (upn_lt k S m Hlt). apply upsn_lt. exact Hlt.
  - replace m with (k + (m - k)) by lia.
    rewrite upn_ge, upsn_ge. cbn [scons]. apply shiftn_var.
Qed.

Lemma subst_upsn_rename_upn : forall k a T,
  subst (upsn k (scons a Var)) (rename (upn k S) T) = T.
Proof.
  intros k a T. rewrite subst_rename.
  transitivity (subst Var T); [apply subst_ext; intro m; apply upsn_upn_cancel | apply subst_id].
Qed.

(** The one computation the general theorem needs:
      ((x1 : T1) -> ... -> (xn : Tn) -> M)[T'/M]  =  (x1 : T1) -> ... -> (xn : Tn) -> T'
    (with T' weakened past the n binders), stated under k extra binders so
    that the induction goes through. *)
Lemma subst_tele : forall Ts k a,
  subst (upsn k (scons a Var)) (tele (rename_sig (upn k S) Ts) (Var (length Ts + k)))
  = tele Ts (shiftn (length Ts + k) a).
Proof.
  induction Ts as [|T Ts IH]; intros k a.
  - change (upsn k (scons a Var) k = shiftn k a).
    pose proof (upsn_ge k (scons a Var) 0) as E. rewrite Nat.add_0_r in E. exact E.
  - change (Pi (subst (upsn k (scons a Var)) (rename (upn k S) T))
               (subst (upsn (S k) (scons a Var))
                      (tele (rename_sig (upn (S k) S) Ts) (Var (S (length Ts + k)))))
            = Pi T (tele Ts (shiftn (S (length Ts + k)) a))).
    rewrite subst_upsn_rename_upn.
    rewrite <- (Nat.add_succ_r (length Ts) k).
    rewrite (IH (S k) a). reflexivity.
Qed.

(** * Theorem 2, in general *)

(** If a construction block with signature x1 : T1, ..., xn : Tn meets its
    obligation, then anything provable using the block and not mentioning
    the xi was already provable, provided it is a type. *)
Theorem conservativity : forall G Ts T',
  wf_sig G Ts ->
  G |- witness Ts ->
  (rev Ts ++ G) |- shiftn (length Ts) T' ->
  G |- Mem T' Ty ->
  G |- T'.
Proof.
  intros G Ts T' Hwf HW HD HT.
  apply (conservativity_gen G (tele (rename_sig S Ts) (Var (length Ts))) T'); [exact HW | | exact HT].
  pose proof (subst_tele Ts 0 T') as E.
  rewrite Nat.add_0_r in E. cbn [upsn upn] in E.
  unfold subst1. rewrite E. apply tele_intro; [exact Hwf | exact HD].
Qed.

(** The one-declaration theorem is the special case Ts = [T1]. *)
Corollary conservativity1' : forall G T1 T',
  (s = true -> G |- Mem T1 Ty) ->
  G |- witness1 T1 -> (T1 :: G) |- shift T' -> G |- Mem T' Ty -> G |- T'.
Proof.
  intros G T1 T' HT1 HW HD HT. exact (conservativity G [T1] T' (conj HT1 I) HW HD HT).
Qed.


(* ======================================================================== *)
(*  Part IV.  Towards Theorem 3: a five-point model.                        *)
(*                                                                          *)
(*  The two-point model of Part I collapses every application to 1, so it  *)
(*  cannot tell a type from junk and validates no postulate with logical    *)
(*  content.  This model adds just enough structure to distinguish          *)
(*     vU  the type of types          vT  the inhabited type (and its only  *)
(*     vF  the empty type                 element)                          *)
(*     vN  inhabited, but not a type  vJ  junk (empty, not a type).        *)
(*  Application is still degenerate: vT and vN act as the constant-vT       *)
(*  function; everything else applied to anything is junk.                 *)
(*                                                                          *)
(*  It settles the half of Review 1's regularity remark that Part I could  *)
(*  not: the printed rules derive (x : type type) -> type but NOT its       *)
(*  formation ((x : type type) -> type) : type.  And it shows, by            *)
(*  computation, that any model with degenerate application still fails    *)
(*  the type of replace, so the equality block needs genuine functions.     *)
(* ======================================================================== *)

Inductive V : Type := vU | vT | vF | vN | vJ.

(** ext a v : v is an element of a. *)
Definition ext (a v : V) : bool :=
  match a, v with
  | vU, vU | vU, vT | vU, vF => true
  | vT, vT => true
  | vN, vT => true
  | _, _ => false
  end.

Definition inh (a : V) : bool := match a with vU | vT | vN => true | vF | vJ => false end.
Definition istype (a : V) : bool := ext vU a.
Definition vapp (f a : V) : V := match f with vT | vN => vT | _ => vJ end.
Definition allV : list V := [vU; vT; vF; vN; vJ].

Lemma allV_full : forall v, In v allV.
Proof.
  intro v; destruct v;
    [left | right; left | right; right; left | right; right; right; left | right; right; right; right; left];
    reflexivity.
Qed.

(** The value of (x : A) -> B, given the value a of A and the values b v of
    B at each v.  It is a type iff a is a type and every b v (v in a) is;
    it is inhabited iff every b v (v in a) is. *)
Definition pi_val (a : V) (b : V -> V) : V :=
  let dom := filter (ext a) allV in
  let ty  := istype a && forallb (fun v => istype (b v)) dom in
  let ok  := forallb (fun v => inh (b v)) dom in
  if ty then (if ok then vT else vF) else (if ok then vN else vJ).

Lemma forallb_ext : forall (A : Type) (f g : A -> bool) (l : list A),
  (forall x, f x = g x) -> forallb f l = forallb g l.
Proof. intros A f g l H. induction l as [|x l IH]; simpl; [reflexivity | rewrite H, IH; reflexivity]. Qed.

Lemma pi_val_ext : forall a b b', (forall v, b v = b' v) -> pi_val a b = pi_val a b'.
Proof.
  intros a b b' H. unfold pi_val.
  rewrite (forallb_ext _ (fun v => istype (b v)) (fun v => istype (b' v))) by (intro v; rewrite H; reflexivity).
  rewrite (forallb_ext _ (fun v => inh (b v)) (fun v => inh (b' v))) by (intro v; rewrite H; reflexivity).
  reflexivity.
Qed.

Lemma forallb_dom : forall a (p : V -> bool),
  (forall v, ext a v = true -> p v = true) -> forallb p (filter (ext a) allV) = true.
Proof.
  intros a p H. apply (proj2 (forallb_forall _ _)). intros v Hin.
  apply filter_In in Hin. apply H, Hin.
Qed.

Lemma forallb_dom_inv : forall a (p : V -> bool),
  forallb p (filter (ext a) allV) = true -> forall v, ext a v = true -> p v = true.
Proof.
  intros a p H v Hv. apply (proj1 (forallb_forall _ _) H).
  apply filter_In. split; [apply allV_full | exact Hv].
Qed.

Lemma pi_val_inh : forall a b,
  (forall v, ext a v = true -> inh (b v) = true) -> inh (pi_val a b) = true.
Proof.
  intros a b H. unfold pi_val. rewrite (forallb_dom a _ H).
  destruct (istype a && forallb (fun v => istype (b v)) (filter (ext a) allV)); reflexivity.
Qed.

Lemma pi_val_istype : forall a b,
  istype a = true -> (forall v, ext a v = true -> istype (b v) = true) ->
  istype (pi_val a b) = true.
Proof.
  intros a b Ha Hb. unfold pi_val. rewrite Ha, (forallb_dom a _ Hb). cbn [andb].
  destruct (forallb (fun v => inh (b v)) (filter (ext a) allV)); reflexivity.
Qed.

Lemma pi_val_ext_inv : forall a b w,
  ext (pi_val a b) w = true -> w = vT /\ (forall v, ext a v = true -> inh (b v) = true).
Proof.
  intros a b w H. unfold pi_val in H.
  destruct (istype a && forallb (fun v => istype (b v)) (filter (ext a) allV));
  destruct (forallb (fun v => inh (b v)) (filter (ext a) allV)) eqn:Hok;
  destruct w; simpl in H; try discriminate H; split; try reflexivity;
  apply (forallb_dom_inv a _ Hok).
Qed.

Lemma inh_ext_T : forall w, inh w = true -> ext w vT = true.
Proof. intros w H; destruct w; try discriminate; reflexivity. Qed.

Lemma ext_inh : forall a w, ext a w = true -> inh a = true.
Proof. intros a w H; destruct a; destruct w; try discriminate; reflexivity. Qed.

(** ** Evaluation *)

Definition env5 := nat -> V.
Definition econs5 (v : V) (rho : env5) : env5 := fun n => match n with 0 => v | S n => rho n end.
Definition etail5 (rho : env5) : env5 := fun n => rho (S n).

Fixpoint eval5 (t : term) (rho : env5) : V :=
  match t with
  | Var n   => rho n
  | Ty      => vU
  | Mem t T => if ext (eval5 T rho) (eval5 t rho) then vT else vF
  | Pi A B  => pi_val (eval5 A rho) (fun v => eval5 B (econs5 v rho))
  | App t u => vapp (eval5 t rho) (eval5 u rho)
  end.

(** rho satisfies G when each variable's value is an element of its type. *)
Fixpoint sat5 (rho : env5) (G : ctx) : Prop :=
  match G with
  | []      => True
  | T :: G' => ext (eval5 T (etail5 rho)) (rho 0) = true /\ sat5 (etail5 rho) G'
  end.

Lemma econs5_ext : forall v rho rho',
  (forall n, rho n = rho' n) -> forall n, econs5 v rho n = econs5 v rho' n.
Proof. intros v rho rho' H [|n]; simpl; auto. Qed.

Lemma eval5_ext : forall t rho rho',
  (forall n, rho n = rho' n) -> eval5 t rho = eval5 t rho'.
Proof.
  induction t as [n | t IHt T IHT | | A IHA B IHB | t IHt u IHu];
    intros rho rho' H; simpl.
  - apply H.
  - rewrite (IHt _ _ H), (IHT _ _ H). reflexivity.
  - reflexivity.
  - rewrite (IHA _ _ H). apply pi_val_ext. intro v. apply IHB. apply econs5_ext. exact H.
  - rewrite (IHt _ _ H), (IHu _ _ H). reflexivity.
Qed.

Lemma up_ren_econs5 : forall xi v rho n,
  econs5 v rho (up_ren xi n) = econs5 v (fun m => rho (xi m)) n.
Proof. intros xi v rho [|n]; reflexivity. Qed.

Lemma eval5_rename : forall t xi rho,
  eval5 (rename xi t) rho = eval5 t (fun n => rho (xi n)).
Proof.
  induction t as [n | t IHt T IHT | | A IHA B IHB | t IHt u IHu];
    intros xi rho; simpl.
  - reflexivity.
  - rewrite IHt, IHT. reflexivity.
  - reflexivity.
  - rewrite IHA. apply pi_val_ext. intro v. rewrite IHB.
    apply eval5_ext. intro n. apply up_ren_econs5.
  - rewrite IHt, IHu. reflexivity.
Qed.

Lemma eval5_shift : forall t rho, eval5 (shift t) rho = eval5 t (etail5 rho).
Proof. intros t rho. unfold shift. rewrite eval5_rename. reflexivity. Qed.

Fixpoint etailn5 (k : nat) (rho : env5) : env5 :=
  match k with 0 => rho | S k => etailn5 k (etail5 rho) end.

Lemma eval5_shiftn : forall k t rho, eval5 (shiftn k t) rho = eval5 t (etailn5 k rho).
Proof.
  induction k as [|k IH]; intros t rho.
  - reflexivity.
  - change (eval5 (shift (shiftn k t)) rho = eval5 t (etailn5 k (etail5 rho))).
    rewrite eval5_shift. apply IH.
Qed.

Lemma etailn5_0 : forall n rho, etailn5 n rho 0 = rho n.
Proof. induction n as [|n IH]; intros rho; [reflexivity | apply IH]. Qed.

Lemma up_sub_econs5 : forall sigma v rho n,
  eval5 (up_sub sigma n) (econs5 v rho) = econs5 v (fun m => eval5 (sigma m) rho) n.
Proof.
  intros sigma v rho n. unfold up_sub, shift. destruct n as [|n].
  - reflexivity.
  - rewrite eval5_rename. apply eval5_ext. intro k. reflexivity.
Qed.

Lemma eval5_subst : forall t sigma rho,
  eval5 (subst sigma t) rho = eval5 t (fun n => eval5 (sigma n) rho).
Proof.
  induction t as [n | t IHt T IHT | | A IHA B IHB | t IHt u IHu];
    intros sigma rho; simpl.
  - reflexivity.
  - rewrite IHt, IHT. reflexivity.
  - reflexivity.
  - rewrite IHA. apply pi_val_ext. intro v. rewrite IHB.
    apply eval5_ext. intro n. apply up_sub_econs5.
  - rewrite IHt, IHu. reflexivity.
Qed.

Lemma eval5_subst1 : forall B a rho,
  eval5 (subst1 B a) rho = eval5 B (econs5 (eval5 a rho) rho).
Proof.
  intros B a rho. unfold subst1. rewrite eval5_subst.
  apply eval5_ext. intros [|n]; reflexivity.
Qed.

Lemma sat5_nth : forall G n T rho,
  sat5 rho G -> nth_error G n = Some T -> ext (eval5 T (etailn5 (S n) rho)) (rho n) = true.
Proof.
  induction G as [|T0 G IH]; intros n T rho Hsat Hnth.
  - destruct n; discriminate.
  - destruct n as [|n]; simpl in Hnth.
    + injection Hnth as Heq. subst T. destruct Hsat as [H _]. exact H.
    + destruct Hsat as [_ Hsat']. exact (IH n T (etail5 rho) Hsat' Hnth).
Qed.

Lemma sat5_econs : forall G A v rho,
  sat5 rho G -> ext (eval5 A rho) v = true -> sat5 (econs5 v rho) (A :: G).
Proof.
  intros G A v rho Hsat HA. split.
  - rewrite (eval5_ext A (etail5 (econs5 v rho)) rho (fun n => eq_refl)). exact HA.
  - exact Hsat.
Qed.

(** Extracting "X is a type" from the truth of X : type. *)
Lemma mem_ty_inv : forall X rho,
  inh (eval5 (Mem X Ty) rho) = true -> istype (eval5 X rho) = true.
Proof.
  intros X rho H. change (inh (if ext vU (eval5 X rho) then vT else vF) = true) in H.
  destruct (ext vU (eval5 X rho)) eqn:E; [exact E | discriminate H].
Qed.

(** ** Soundness *)

Theorem soundness5 : forall G T, G |- T -> forall rho, sat5 rho G -> inh (eval5 T rho) = true.
Proof.
  intros G T D. induction D as [G n T H | G t T T' D1 IHD1 D2 IHD2 | G t T D IHD | G T T' D1 IHD1 D2 IHD2 | G | G A B D1 IHD1 D2 IHD2 | G f a A B D1 IHD1 D2 IHD2 | G A B H1 IHD1 D2 IHD2]; intros rho Hsat.
  - (* hyp *)
    change (inh (if ext (eval5 (shiftn (S n) T) rho) (rho n) then vT else vF) = true).
    rewrite eval5_shiftn, (sat5_nth G n T rho Hsat H). reflexivity.
  - (* in *)
    change (inh (if ext vU (if ext (eval5 T rho) (eval5 t rho) then vT else vF) then vT else vF) = true).
    destruct (ext (eval5 T rho) (eval5 t rho)); reflexivity.
  - (* in- *)
    specialize (IHD rho Hsat).
    change (inh (if ext (eval5 T rho) (eval5 t rho) then vT else vF) = true) in IHD.
    destruct (ext (eval5 T rho) (eval5 t rho)) eqn:E; [exact (ext_inh _ _ E) | discriminate IHD].
  - (* cut *)
    specialize (IHD1 rho Hsat).
    specialize (IHD2 (econs5 vT rho) (sat5_econs G T vT rho Hsat (inh_ext_T _ IHD1))).
    rewrite eval5_shift in IHD2.
    rewrite (eval5_ext T' (etail5 (econs5 vT rho)) rho (fun n => eq_refl)) in IHD2.
    exact IHD2.
  - (* type *)
    reflexivity.
  - (* arrow *)
    change (inh (if istype (pi_val (eval5 A rho) (fun v => eval5 B (econs5 v rho))) then vT else vF) = true).
    rewrite pi_val_istype; [reflexivity | exact (mem_ty_inv A rho (IHD1 rho Hsat)) |].
    intros v Hv. exact (mem_ty_inv B (econs5 v rho) (IHD2 (econs5 v rho) (sat5_econs G A v rho Hsat Hv))).
  - (* ap *)
    specialize (IHD1 rho Hsat). specialize (IHD2 rho Hsat).
    change (inh (if ext (pi_val (eval5 A rho) (fun v => eval5 B (econs5 v rho))) (eval5 f rho) then vT else vF) = true) in IHD1.
    change (inh (if ext (eval5 A rho) (eval5 a rho) then vT else vF) = true) in IHD2.
    destruct (ext (pi_val (eval5 A rho) (fun v => eval5 B (econs5 v rho))) (eval5 f rho)) eqn:Ef; [| discriminate IHD1].
    destruct (ext (eval5 A rho) (eval5 a rho)) eqn:Ea; [| discriminate IHD2].
    destruct (pi_val_ext_inv _ _ _ Ef) as [Hf Hb].
    change (inh (if ext (eval5 (subst1 B a) rho) (vapp (eval5 f rho) (eval5 a rho)) then vT else vF) = true).
    rewrite eval5_subst1, Hf. simpl vapp.
    pose proof (inh_ext_T _ (Hb _ Ea)) as E. cbn beta in E. rewrite E. reflexivity.
  - (* arrow+ *)
    simpl. apply pi_val_inh. intros v Hv.
    exact (IHD2 (econs5 v rho) (sat5_econs G A v rho Hsat Hv)).
Qed.

(** ** What the five-point model settles *)

Corollary underivable_if_uninhabited5 : forall T rho,
  inh (eval5 T rho) = false -> ~ ([] |- T).
Proof. intros T rho Hf D. rewrite (soundness5 [] T D rho I) in Hf. discriminate. Qed.

(** Consistency again (the model refutes (X : type) -> X). *)
Example absurd_5pt : eval5 absurd (fun _ => vJ) = vF.
Proof. reflexivity. Qed.

(** The junk arrow (x : type type) -> type is inhabited in the model
    (value vN) but NOT a type: its formation judgment is false, hence
    underivable, in either system. *)
Example junk_arrow_5pt : eval5 (Pi (App Ty Ty) Ty) (fun _ => vJ) = vN.
Proof. reflexivity. Qed.

Theorem junk_formation_underivable : ~ ([] |- Mem (Pi (App Ty Ty) Ty) Ty).
Proof. exact (underivable_if_uninhabited5 (Mem (Pi (App Ty Ty) Ty) Ty) (fun _ => vJ) eq_refl). Qed.

(** The equality block, once more.  The type of eq,
      (A : type) -> (a1 a2 : A) -> type,
    evaluates to vT, whose only element is vT; so in this model the constant
    eq can only be interpreted as vT, every equation a1 = a2 then evaluates
    to vT (true), and the type of replace is uninhabited.  Any model with
    degenerate application meets the same fate: validating replace needs
    an interpretation of eq that actually looks at its arguments. *)
Definition eq_ty : term := Pi Ty (Pi (Var 0) (Pi (Var 1) Ty)).

Example eq_ty_val : eval5 eq_ty (fun _ => vJ) = vT.
Proof. reflexivity. Qed.

Example eq_forced_to_vT : forall v, ext (eval5 eq_ty (fun _ => vJ)) v = true -> v = vT.
Proof. intros v H. rewrite eq_ty_val in H. destruct v; try discriminate; reflexivity. Qed.

Example replace_ty_uninhabited_5pt : inh (eval5 replace_ty (fun _ => vT)) = false.
Proof. reflexivity. Qed.

End Rules.

(* ======================================================================== *)
(*  The two systems by name, and what separates them.                       *)
(* ======================================================================== *)

(** The author's corrected Figure 1 (arrow+ with the premise G |- T1 : type). *)
Definition corrected := derivable true.
(** Figure 1 as first printed (arrow+ without it). *)
Definition printed := derivable false.

(** Every corrected derivation is a printed derivation, so every theorem
    proved above for printed (consistency in particular) transfers. *)
Lemma corrected_printed : forall G T, corrected G T -> printed G T.
Proof.
  intros G T D. unfold corrected in D. unfold printed. induction D as [G n T H | G t T T' D1 IHD1 D2 IHD2 | G t T D IHD | G T T' D1 IHD1 D2 IHD2 | G | G A B D1 IHD1 D2 IHD2 | G f a A B D1 IHD1 D2 IHD2 | G A B H1 IHD1 D2 IHD2].
  - apply r_hyp; assumption.
  - apply r_in with (T' := T'); assumption.
  - apply r_in_minus with (t := t); assumption.
  - apply r_cut with (T := T); assumption.
  - apply r_type.
  - apply r_arrow; assumption.
  - apply r_ap with (A := A); assumption.
  - apply r_arrow_plus; [intro Hs; discriminate Hs | assumption].
Qed.

(** The printed rules derive an inhabited arrow whose domain, type type, is
    not a type (Review 1, point 3) ... *)
Example junk_inhabited : printed [] (Pi (App Ty Ty) Ty).
Proof.
  unfold printed. apply r_arrow_plus; [intro Hs; discriminate Hs |].
  apply r_in_minus with (t := Ty). apply r_type.
Qed.

(** ... and, by the five-point model, cannot derive its formation.  So the
    printed rules are not regular. *)
Theorem printed_rules_not_regular :
  printed [] (Pi (App Ty Ty) Ty) /\ ~ printed [] (Mem (Pi (App Ty Ty) Ty) Ty).
Proof. split; [exact junk_inhabited | exact (junk_formation_underivable false)]. Qed.

(** In the corrected system the same derivation is blocked by the new
    premise, which would need type type : type; the five-point model shows
    that judgment underivable there too.  Whether the corrected system is
    regular remains open (see the discussion in the review). *)
Theorem corrected_junk_premise_underivable : ~ corrected [] (Mem (App Ty Ty) Ty).
Proof. exact (underivable_if_uninhabited5 true (Mem (App Ty Ty) Ty) (fun _ => vJ) eq_refl). Qed.

Print Assumptions consistency.
Print Assumptions soundness.
Print Assumptions weakening.
Print Assumptions conservativity1.
Print Assumptions conservativity.
Print Assumptions soundness5.
Print Assumptions printed_rules_not_regular.
Print Assumptions corrected_junk_premise_underivable.
