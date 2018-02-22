Require Coq.Bool.Bool. Open Scope bool.
Require Coq.Strings.String. Open Scope string_scope.
Require Coq.Arith.PeanoNat. Open Scope nat_scope.
Require Coq.Lists.List. Open Scope list_scope.
Require Import BinInt.
Require Extraction.

Set Implicit Arguments.

Module Core.
  Module Fuel.
    Inductive t : Type :=
    | More : t -> t
    | Kicked : t.
  End Fuel.

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

  Inductive err_status : Type := 
  | NotInHole : err_status
  | InHole : MetaVar.t -> err_status.

  Module OperatorSeq.
    Inductive opseq (tm : Type) (op : Type) : Type := 
    | ExpOpExp : tm -> op -> tm -> opseq tm op
    | SeqOpExp : opseq tm op -> op -> tm -> opseq tm op.

    (* concatenates two opseqs *)
    Fixpoint seq_op_seq {tm op : Type} 
      (seq1 : opseq tm op) (op1 : op) (seq2 : opseq tm op) 
      : opseq tm op := 
      match seq2 with 
      | ExpOpExp e1 op2 e2 => SeqOpExp (SeqOpExp seq1 op1 e1) op2 e2 
      | SeqOpExp seq2' op2 ue' => 
          SeqOpExp (seq_op_seq seq1 op1 seq2') op2 ue'
      end.

    (* prepends an expression to seq *)
    Fixpoint exp_op_seq {tm op : Type} 
      (e1 : tm) (op1 : op) (seq : opseq tm op) 
      : opseq tm op := 
      match seq with 
      | ExpOpExp e2 op2 e3 => 
        SeqOpExp (ExpOpExp e1 op1 e2) op2 e3
      | SeqOpExp seq' op' e' => 
        SeqOpExp (exp_op_seq e1 op1 seq') op' e'
      end.

    (* returns number of expressions in seq (not ops) *)
    Fixpoint seq_length {tm op : Type} 
      (seq : opseq tm op) : nat := 
      match seq with 
      | ExpOpExp _ _ _ => S(S(O))
      | SeqOpExp seq' _ _ => S(seq_length seq')
      end.

    (* nth expression in seq, if it exists *) 
    Fixpoint seq_nth {tm op : Type}
      (n : nat) (seq : opseq tm op) : option(tm) := 
      match (n, seq) with 
      | (O, ExpOpExp e1 _ _) => Some e1
      | (S O, ExpOpExp _ _ e2) => Some e2
      | (_, ExpOpExp _ _ _) => None
      | (_, SeqOpExp seq' _ e) => 
        let len := seq_length seq' in 
        if Nat.eqb n len then Some e else seq_nth n seq' 
      end.

    (* update the nth expression in seq, if it exists *)
    Fixpoint seq_update_nth {tm op : Type}
      (n : nat) (seq : opseq tm op) (e : tm) : option(opseq tm op) := 
      match (n, seq) with 
      | (O, ExpOpExp _ op e2) => Some (ExpOpExp e op e2)
      | (S O, ExpOpExp e1 op _) => Some (ExpOpExp e1 op e)
      | (_, ExpOpExp _ _ _) => None
      | (_, SeqOpExp seq' op e') => 
        let len := seq_length seq' in 
        if Nat.eqb n len then Some (SeqOpExp seq' op e) 
        else match seq_update_nth n seq' e with 
        | Some seq'' => Some (SeqOpExp seq'' op e')
        | None => None
        end
      end.

    Inductive opseq_surround (tm : Type) (op : Type) : Type := 
    (* set up this way to enforce the requirement that there be at least one op *)
    (* if the prefix is empty, there must be a non-empty suffix *)
    | EmptyPrefix : opseq_suffix tm op -> opseq_surround tm op
    (* if the suffix is empty, there must be a non-empty prefix *)
    | EmptySuffix : opseq_prefix tm op -> opseq_surround tm op
    (* both can be non-empty *)
    | BothNonEmpty : opseq_prefix tm op -> opseq_suffix tm op -> opseq_surround tm op 
    with opseq_prefix (tm : Type) (op : Type) : Type := 
    (* a non-empty prefix is either one that contains a single expression *)
    | ExpPrefix : tm -> op -> opseq_prefix tm op 
    (* or one that contains two or more expressions, i.e. another opseq *)
    | SeqPrefix : opseq tm op -> op -> opseq_prefix tm op 
    with opseq_suffix (tm : Type) (op : Type) : Type :=
    (* analagous to opseq_prefix *)
    | ExpSuffix : op -> tm -> opseq_suffix tm op 
    | SeqSuffix : op -> opseq tm op -> opseq_suffix tm op.

    (* append an exp to a prefix *)
    Definition prefix_append_exp {tm op : Type}
      (prefix : opseq_prefix tm op)
      (e : tm)
      (op2 : op)
      : opseq_prefix tm op := 
      match prefix with 
      | ExpPrefix e1 op1 => 
        SeqPrefix (OperatorSeq.ExpOpExp e1 op1 e) op2
      | SeqPrefix seq1 op1 => 
        SeqPrefix (OperatorSeq.SeqOpExp seq1 op1 e) op2
      end.

    (* prepend an exp to a suffix *)
    Definition suffix_prepend_exp {tm op : Type}
      (suffix : opseq_suffix tm op)
      (op1 : op)
      (e : tm)
      : opseq_suffix tm op :=
      match suffix with 
      | ExpSuffix op2 e' => 
        SeqSuffix op1 (OperatorSeq.ExpOpExp e op2 e')
      | SeqSuffix op2 seq' => 
        SeqSuffix op1 (OperatorSeq.exp_op_seq e op2 seq')
      end.

    (* append an exp to a suffix *)
    Definition suffix_append_exp {tm op : Type}
      (suffix : opseq_suffix tm op)
      (op2 : op)
      (e : tm)
      : opseq_suffix tm op := 
      match suffix with 
      | ExpSuffix op1 e' => 
          SeqSuffix op1 (OperatorSeq.ExpOpExp e' op2 e)
      | SeqSuffix op1 seq => 
          SeqSuffix op1 (OperatorSeq.SeqOpExp seq op2 e)
      end.

    (* append an exp to the suffix of a surround *)
    Definition surround_suffix_append_exp {tm op : Type}
      (surround : opseq_surround tm op)
      (op1 : op)
      (e : tm)
      : opseq_surround tm op := 
        match surround with 
        | EmptyPrefix suffix => 
          let suffix' := suffix_append_exp suffix op1 e in 
          EmptyPrefix suffix'
        | EmptySuffix prefix => 
          let suffix' := ExpSuffix op1 e in 
          BothNonEmpty prefix suffix' 
        | BothNonEmpty prefix suffix => 
          let suffix' := suffix_append_exp suffix op1 e in 
          BothNonEmpty prefix suffix'
        end.

    Fixpoint split {tm op : Type}
      (n : nat) (seq : opseq tm op) 
      : option(tm * opseq_surround tm op) := 
      match (n, seq) with 
      | (O, OperatorSeq.ExpOpExp e1 op e2) => 
        Some (e1, EmptyPrefix (ExpSuffix op e2))
      | (S O, OperatorSeq.ExpOpExp e1 op e2) => 
        Some (e2, EmptySuffix (ExpPrefix e1 op))
      | (_, OperatorSeq.ExpOpExp _ _ _) => 
        None
      | (_, OperatorSeq.SeqOpExp seq' op e) => 
        let length' := OperatorSeq.seq_length seq' in 
        if Nat.ltb n length' then 
          match split n seq' with 
          | Some (e', surround) => 
            let surround' := surround_suffix_append_exp surround op e in 
            Some (e', surround')
          | None => None
          end
        else if Nat.eqb n length' then 
          let prefix' := SeqPrefix seq' op in 
          let surround' := EmptySuffix prefix' in 
          Some (e, surround')
        else None
      end.

    Fixpoint split0 {tm op : Type}
      (seq : opseq tm op) 
      : tm * opseq_suffix tm op := 
      match seq with 
      | OperatorSeq.ExpOpExp e1 op e2 => 
        (e1, ExpSuffix op e2)
      | OperatorSeq.SeqOpExp seq' op e => 
        let (e0, suffix') := split0 seq' in
        (e0, suffix_append_exp suffix' op e)
      end.

    Definition split_tail {tm op : Type}
      (seq : opseq tm op) : tm * opseq_prefix tm op := 
      match seq with 
      | OperatorSeq.ExpOpExp e1 op e2 => 
        (e2, ExpPrefix e1 op)
      | OperatorSeq.SeqOpExp seq' op e => 
        (e, SeqPrefix seq' op)
      end.

    Definition prefix_length {tm op : Type}
      (prefix : opseq_prefix tm op) : nat := 
      match prefix with 
      | ExpPrefix _ _ => S(O)
      | SeqPrefix seq _ => OperatorSeq.seq_length seq
      end.

    Definition surround_prefix_length {tm op : Type}
      (surround : opseq_surround tm op)
      : nat := 
      match surround with 
      | EmptyPrefix _ => O
      | EmptySuffix prefix
      | BothNonEmpty prefix _ => prefix_length prefix
      end.

    Definition opseq_of_exp_and_surround {tm op : Type}
      (e : tm)
      (surround : opseq_surround tm op)
      : opseq tm op := 
      match surround with 
      | EmptyPrefix suffix => 
        match suffix with 
        | ExpSuffix op e2 => OperatorSeq.ExpOpExp e op e2
        | SeqSuffix op seq => OperatorSeq.exp_op_seq e op seq
        end
      | EmptySuffix prefix => 
        match prefix with 
        | ExpPrefix e1 op => OperatorSeq.ExpOpExp e1 op e
        | SeqPrefix seq op => OperatorSeq.SeqOpExp seq op e
        end
      | BothNonEmpty prefix suffix => 
        match (prefix, suffix) with 
        | (ExpPrefix e1 op1, ExpSuffix op2 e2) => 
            OperatorSeq.SeqOpExp
              (OperatorSeq.ExpOpExp e1 op1 e)
              op2 e2
        | (ExpPrefix e1 op1, SeqSuffix op2 seq2) => 
            OperatorSeq.seq_op_seq
              (OperatorSeq.ExpOpExp e1 op1 e)
              op2 seq2
        | (SeqPrefix seq1 op1, ExpSuffix op2 e2) => 
            OperatorSeq.SeqOpExp 
              (OperatorSeq.SeqOpExp seq1 op1 e) op2 e2
        | (SeqPrefix seq1 op1, SeqSuffix op2 seq2) => 
            OperatorSeq.seq_op_seq
              (OperatorSeq.SeqOpExp seq1 op1 e) op2 seq2
        end
      end.
  End OperatorSeq.

  Module Skel.
    Inductive t (op : Type) : Type := 
    | Placeholder : nat -> t op
    | BinOp : err_status -> op -> t op -> t op -> t op.

    Fixpoint leftmost_op {op : Type} (skel : t op) : option(op) := 
      match skel with 
      | Placeholder _ _ => None
      | BinOp _ op skel1 _ => 
        match leftmost_op skel1 with 
        | (Some op) as result => result
        | None => Some op
        end
      end.

    Fixpoint rightmost_op {op : Type} (skel : t op) : option(op) := 
      match skel with 
      | Placeholder _ _ => None
      | BinOp _ op _ skel2 => 
        match rightmost_op skel2 with 
        | (Some op) as result => result
        | None => Some op
        end
      end.
  End Skel.

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

    (* Theorem eq_refl : forall (x : t),
      eq x x = true.
    Proof.
      induction x; (simpl; auto with * ).
    Qed. *)

    (* Theorem eq_sound : forall x y : t,
      x = y -> (eq x y = true).
    Proof.
      intros.
      rewrite -> H.
      apply eq_refl.
    Qed. *)

    (* Lemma and_proj_1 : forall b1 b2 : bool,
      (b1 && b2) = true -> b1 = true.
    Proof.
      apply Coq.Bool.Bool.andb_true_iff.
    Qed. *)

    (* Lemma and_proj_2 : forall b1 b2 : bool,
      (b1 && b2) = true -> b2 = true.
    Proof.
      apply Coq.Bool.Bool.andb_true_iff.
    Qed. *)

    (* Theorem eq_complete : forall x y : t,
      (eq x y = true) -> x = y.
    Proof.
      induction x;

      (induction y;
      ((simpl; reflexivity) || discriminate)) ||

      (induction y; (
      discriminate ||
      (simpl;
      intro H;
      assert (x1 = y1) as H0;iii
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
    Qed. *)

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

    (* Theorem eq_implies_consistent : forall x y : t,
      ((eq x y) = true) -> ((consistent x y) = true).
    Proof.
      intuition.
      unfold consistent.
      destruct x;
      repeat rewrite -> H;
      simpl;
      reflexivity.
    Qed. *)

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
    
    Fixpoint join ty1 ty2 := 
      match (ty1, ty2) with
      | (HTyp.Num, HTyp.Num) => Some ty1
      | (_, HTyp.Hole) => Some ty1
      | (HTyp.Hole, _) => Some ty2
      | (HTyp.Arrow ty11 ty12, HTyp.Arrow ty21 ty22) => 
        match (join ty11 ty21, join ty12 ty22) with 
        | (Some ty1, Some ty2) => Some (HTyp.Arrow ty1 ty2)
        | _ => None
        end
      | (HTyp.Sum ty11 ty12, HTyp.Sum ty21 ty22) => 
        match (join ty11 ty21, join ty12 ty22) with 
        | (Some ty1, Some ty2) => Some (HTyp.Sum ty1 ty2)
        | _ => None
        end
      | _ => None
      end.
  End HTyp.

  Module UHTyp.
    Inductive op : Type := 
    | Arrow : op
    | Sum : op.

    Definition skel_t : Type := Skel.t op.

    Inductive t : Type := 
    | Parenthesized : t -> t
    | Num : t
    | Hole : t
    | OpSeq : skel_t -> OperatorSeq.opseq t op -> t.

    Definition opseq : Type := OperatorSeq.opseq t op.

    Definition bidelimited (uty : t) : bool := 
      match uty with 
      | Parenthesized _
      | Num
      | Hole => true
      | OpSeq _ _ => false
      end.

    Fixpoint well_formed (fuel : Fuel.t) (uty : t) : bool := 
      match fuel with 
      | Fuel.Kicked => false
      | Fuel.More fuel => 
      match uty with 
      | Parenthesized uty1 => well_formed fuel uty1
      | Num => true
      | Hole => true
      | OpSeq skel seq => 
        (* NOTE: does not check that skel is the valid parse of seq *)
        well_formed_skel fuel skel seq
      end
      end
    with well_formed_skel (fuel : Fuel.t) (skel : skel_t) (seq : opseq) : bool :=  
      match fuel with 
      | Fuel.Kicked => false
      | Fuel.More fuel => 
      match skel with 
      | Skel.Placeholder _ n => 
        match OperatorSeq.seq_nth n seq with
        | Some uty_n => 
          bidelimited uty_n && well_formed fuel uty_n
        | None => false
        end
      | Skel.BinOp NotInHole _ skel1 skel2 => 
        well_formed_skel fuel skel1 seq 
        && well_formed_skel fuel skel2 seq
      | Skel.BinOp (InHole _) _ _ _ => false (* no type-level non-empty holes *)
      end
      end.

    Fixpoint expand (fuel : Fuel.t) (uty : t) : HTyp.t := 
      match fuel with 
      | Fuel.Kicked => HTyp.Hole
      | Fuel.More fuel => 
      match uty with 
      | Parenthesized uty1 => expand fuel uty1
      | Num => HTyp.Num
      | Hole => HTyp.Hole
      | OpSeq skel seq => 
        expand_skel fuel skel seq
      end
      end
    with expand_skel (fuel : Fuel.t) (skel : skel_t) (seq : opseq) := 
      match fuel with 
      | Fuel.Kicked => HTyp.Hole
      | Fuel.More fuel => 
      match skel with 
      | Skel.Placeholder _ n => 
        match OperatorSeq.seq_nth n seq with  
        | Some uty_n => expand fuel uty_n
        | None => HTyp.Hole (* should never happen *)
        end
      | Skel.BinOp _ Arrow skel1 skel2 => 
        let uty_1 := expand_skel fuel skel1 seq in 
        let uty_2 := expand_skel fuel skel2 seq in 
        HTyp.Arrow uty_1 uty_2
      | Skel.BinOp _ Sum skel1 skel2 => 
        let uty_1 := expand_skel fuel skel1 seq in 
        let uty_2 := expand_skel fuel skel2 seq in 
        HTyp.Sum uty_1 uty_2
      end
      end.
  End UHTyp.

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

    Fixpoint update (ctx : t) (x : Var.t) (ty : HTyp.t) : option t := 
      match ctx with 
      | nil => None
      | cons (y, ty') ctx' => 
        match Var.equal x y with 
        | true => Some (cons (y, ty) ctx')
        | false => 
          match update ctx' x ty with 
          | Some ctx' => Some (cons (y, ty') ctx')
          | None => None
          end
        end
      end.

    Definition extend (ctx : t) (x : Var.t * HTyp.t)
      : t :=
      match x with
      | (x, ty) => 
        match update ctx x ty with 
        | Some ctx' => ctx'
        | None => cons (x, ty) ctx
        end
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

  Module UHExp. (* unassociated H-expressions *)
    Inductive inj_side : Type :=
    | L : inj_side
    | R : inj_side.

    Definition pick_side {A : Type} (side : inj_side) (l : A) (r : A) : A :=
      match side with
      | L => l
      | R => r
      end.

    Inductive op : Type := 
    | Plus : op
    | Times : op
    | Space : op.

    Definition skel_t := Skel.t op.

    Inductive t : Type := 
    | Tm : err_status -> t' -> t
    | Parenthesized : t -> t
    with t' : Type := 
    | Asc : t -> UHTyp.t -> t'
    | Var : Var.t -> t'
    | Let : Var.t -> t -> t -> t'
    | Lam : Var.t -> t -> t'
    | NumLit : nat -> t'
    | Inj : inj_side -> t -> t'
    | Case : t -> (Var.t * t) -> (Var.t * t) -> t'
    | EmptyHole : MetaVar.t -> t'
    | OpSeq : skel_t -> OperatorSeq.opseq t op -> t'.

    Definition opseq := OperatorSeq.opseq t op.

    (* bidelimited expressions are those that don't have 
     * sub-expressions at their outer left or right edge
     * in the concrete syntax *)
    Definition is_bidelimited (e : t) := 
      match e with 
      (* bidelimited cases *)
      | Parenthesized _ => true
      | Tm _ (Var _) => true
      | Tm _ (NumLit _) => true
      | Tm _ (EmptyHole _) => true
      | Tm _ (Inj _ _) => true
      (* non-bidelimited cases *)
      | Tm _ (Asc _ _) => false
      | Tm _ (Let _ _ _) => false
      | Tm _ (Lam _ _) => false
      | Tm _ (Case _ _ _) => false
      | Tm _ (OpSeq _ _) => false
      end.

    (* if e is not bidelimited, bidelimit e parenthesizes it *)
    Definition bidelimit (e : t) := 
      if is_bidelimited e then e else Parenthesized e.

    (* helper function for constructing a new empty hole *)
    Definition new_EmptyHole (u_gen : MetaVar.gen) : t * MetaVar.gen :=
      let (u', u_gen') := MetaVar.next u_gen in 
      (Tm NotInHole (EmptyHole u'), u_gen').

    (* put e in the specified hole *)
    Fixpoint put_in_hole (u : MetaVar.t) (e : t) := 
      match e with 
      | Tm _ e' => Tm (InHole u) e'
      | Parenthesized e' => Parenthesized (put_in_hole u e')
      end.

    (* put e in a new hole, if it is not already in a hole *)
    Fixpoint put_in_new_hole (u_gen : MetaVar.gen) (e : t) := 
      match e with
      | Tm NotInHole e' => 
        let (u, u_gen') := MetaVar.next u_gen in 
        (Tm (InHole u) e', u_gen')
      | Tm (InHole _) _ => (e, u_gen)
      | Parenthesized e1 => 
        match put_in_new_hole u_gen e1 with 
        | (e1', u_gen') => (Parenthesized e1', u_gen')
        end
      end.

    (* put skel in a new hole, if it is not already in a hole *)
    Definition put_skel_in_new_hole (u_gen : MetaVar.gen) (skel : skel_t) (seq : opseq) := 
      match skel with 
      | Skel.Placeholder _ n => 
        match OperatorSeq.seq_nth n seq with 
        | Some en => 
          let (en', u_gen') := put_in_new_hole u_gen en in 
          match OperatorSeq.seq_update_nth n seq en' with 
          | Some seq' => Some (skel, seq', u_gen')
          | None => None
          end
        | None => None
        end
      | Skel.BinOp (InHole _) _ _ _ => Some (skel, seq, u_gen)
      | Skel.BinOp NotInHole op skel1 skel2 => 
        let (u', u_gen') := MetaVar.next u_gen in 
        Some (Skel.BinOp (InHole u') op skel1 skel2, seq, u_gen')
      end.

    (* see syn_skel and ana_skel below *) 
    Inductive type_mode : Type := 
    | AnalyzedAgainst : HTyp.t -> type_mode
    | Synthesized : HTyp.t -> type_mode.

    (* synthesize a type, if possible, for e *)
    Fixpoint syn
      (fuel : Fuel.t) 
      (ctx : Ctx.t)
      (e : UHExp.t) 
      : option(HTyp.t) := 
        match fuel with 
        | Fuel.Kicked => None
        | Fuel.More fuel => 
        match e with
        | Tm (InHole _) e' => 
          match syn' fuel ctx e' with 
          | Some _ => Some HTyp.Hole
          | None => None
          end
        | Tm NotInHole e' => syn' fuel ctx e'
        | Parenthesized e1 => syn fuel ctx e1 
        end
        end
    with syn'
      (fuel : Fuel.t)
      (ctx : Ctx.t)
      (e : UHExp.t')
      : option(HTyp.t) := 
        match fuel with
        | Fuel.Kicked => None
        | Fuel.More fuel =>
          match e with 
          | Asc e1 uty =>
             let ty := UHTyp.expand fuel uty in  
             if is_bidelimited e1 then 
               match ana fuel ctx e1 ty with 
               | None => None
               | Some _ => Some ty
               end
             else None
          | Var x => 
            Ctx.lookup ctx x 
          | Lam x e1 => 
            let ctx' := Ctx.extend ctx (x, HTyp.Hole) in 
            match syn fuel ctx' e1 with 
            | Some ty2 => Some (HTyp.Arrow HTyp.Hole ty2)
            | None => None
            end
          | Let x e1 e2 =>
            match syn fuel ctx e1 with 
            | None => None
            | Some ty1 => 
              let ctx' := Ctx.extend ctx (x, ty1) in 
              syn fuel ctx' e2 
            end
          | NumLit i => Some HTyp.Num
          | EmptyHole u => Some HTyp.Hole
          | OpSeq skel seq => 
            (* NOTE: doesn't check if skel is the correct parse of seq!!! *)
            match syn_skel fuel ctx skel seq None with 
            | Some (ty, _) => Some ty 
            | None => None
            end
          | Inj side e1 => 
            match syn fuel ctx e1 with 
            | None => None
            | Some ty1 => 
              match side with 
              | L => Some (HTyp.Sum ty1 HTyp.Hole)
              | R => Some (HTyp.Sum HTyp.Hole ty1)
              end
            end
          | Case _ _ _ => None
          end
        end
    with ana
      (fuel : Fuel.t)
      (ctx : Ctx.t)
      (e : UHExp.t)
      (ty : HTyp.t)
      : option(unit) := 
        match fuel with 
        | Fuel.Kicked => None
        | Fuel.More fuel => 
        match e with 
        | Tm (InHole _) e' => 
          match syn' fuel ctx e' with 
          | Some _ => Some tt (* this is a consequence of subsumption and hole universality *)
          | None => None
          end
        | Tm NotInHole e' => ana' fuel ctx e' ty
        | Parenthesized e1 => ana fuel ctx e1 ty
        end
        end
    with ana'
      (fuel : Fuel.t)
      (ctx : Ctx.t)
      (e : UHExp.t')
      (ty : HTyp.t)
      : option(unit) := 
      match fuel with
      | Fuel.More fuel =>
        match e with
        | Let x e1 e2 =>
          match syn fuel ctx e1 with 
          | None => None
          | Some ty1 => 
              let ctx' := Ctx.extend ctx (x, ty1) in
              ana fuel ctx' e2 ty
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
          | Some (ty1, ty2) => 
            ana fuel ctx e' (pick_side side ty1 ty2)
          end
        | Case e' (x, e1) (y, e2) (* 21b *) =>
          match syn fuel ctx e' with 
          | None => None
          | Some e'_ty =>
            match HTyp.matched_sum e'_ty with
            | None => None
            | Some (ty1, ty2) =>
              let ctx1 := Ctx.extend ctx (x, ty1) in
              match ana fuel ctx1 e1 ty with
              | None => None 
              | Some _ =>
                let ctx2 := Ctx.extend ctx (y, ty2) in
                ana fuel ctx2 e2 ty 
              end
            end
          end
        | Asc _ _
        | Var _
        | NumLit _ 
        | EmptyHole _
        | OpSeq _ _ =>
          match syn' fuel ctx e with
          | Some ty' =>
            if HTyp.consistent ty ty' then (Some tt) else None
          | None => None
          end
        end
      | Fuel.Kicked => None
      end
    with syn_skel
      (fuel : Fuel.t)
      (ctx : Ctx.t)
      (skel : skel_t)
      (seq : opseq)
      (monitor : option(nat))
      : option(HTyp.t * option(type_mode)) := 
        match fuel with 
        | Fuel.Kicked => None
        | Fuel.More fuel => 
        match skel with 
        | Skel.Placeholder _ n => 
          match OperatorSeq.seq_nth n seq with 
          | Some en => 
            if is_bidelimited en then 
              match syn fuel ctx en with
              | Some ty => 
                let mode := 
                  match monitor with 
                  | Some n' => 
                    if Nat.eqb n n' then Some (Synthesized ty)
                    else None
                  | None => None
                  end in 
                Some (ty, mode)
              | None => None
              end
            else None
          | None => None
          end
        | Skel.BinOp (InHole u) op skel1 skel2 => 
          let skel_not_in_hole := Skel.BinOp NotInHole op skel1 skel2 in 
          match syn_skel fuel ctx skel_not_in_hole seq monitor with 
          | None => None
          | Some (ty, mode) => Some (HTyp.Hole, mode)
          end
        | Skel.BinOp NotInHole Plus skel1 skel2
        | Skel.BinOp NotInHole Times skel1 skel2 => 
          match ana_skel fuel ctx skel1 seq HTyp.Num monitor with 
          | Some mode1 => 
            match ana_skel fuel ctx skel2 seq HTyp.Num monitor with 
            | Some mode2 => 
              let ty := HTyp.Num in 
              match mode1 with 
              | Some _ => Some (ty, mode1)
              | None => Some (ty, mode2)
              end
            | None => None
            end
          | None => None
          end
        | Skel.BinOp NotInHole Space skel1 skel2 => 
          match syn_skel fuel ctx skel1 seq monitor with 
          | Some (ty1, mode1) => 
            match HTyp.matched_arrow ty1 with 
            | None => None
            | Some (ty2, ty) => 
              match ana_skel fuel ctx skel2 seq ty2 monitor with 
              | Some mode2 => 
                match mode1 with 
                | Some _ => Some (ty, mode1)
                | None => Some (ty, mode2)
                end
              | None => None
              end
            end
          | None => None
          end
        end
        end
    with ana_skel
      (fuel : Fuel.t)
      (ctx : Ctx.t)
      (skel : skel_t)
      (seq : opseq)
      (ty : HTyp.t)
      (monitor : option(nat))
      : option(option(type_mode)) := 
        match fuel with 
        | Fuel.Kicked => None
        | Fuel.More fuel => 
        match skel with 
        | Skel.Placeholder _ n => 
          match OperatorSeq.seq_nth n seq with 
          | Some en => 
            if is_bidelimited en then   
              match ana fuel ctx en ty with 
              | Some _ => 
                match monitor with 
                | Some n' => 
                  if Nat.eqb n n' then 
                    Some (Some (AnalyzedAgainst ty))
                  else Some (None)
                | None => Some (None)
                end
              | None => None
              end
            else None
          | None => None
          end
        | _ => 
          match syn_skel fuel ctx skel seq monitor with 
          | Some (ty', mode) => 
            if HTyp.consistent ty ty' then Some mode else None
          | None => None
          end
        end
        end.

    Fixpoint syn_fix_holes
      (fuel : Fuel.t)
      (ctx : Ctx.t)
      (u_gen : MetaVar.gen)
      (e : t)
      : option(t * HTyp.t * MetaVar.gen) := 
        match fuel with 
        | Fuel.Kicked => None
        | Fuel.More fuel => 
        match e with 
        | Tm _ e' => 
          match syn_fix_holes' fuel ctx u_gen e' with 
          | Some (e'', ty, u_gen') => 
            Some (Tm NotInHole e'', ty, u_gen')
          | None => None
          end
        | Parenthesized e1 => 
          match syn_fix_holes fuel ctx u_gen e1 with 
          | Some (e1', ty, u_gen') => 
            Some (Parenthesized e1', ty, u_gen')
          | None => None
          end
        end
        end
    with syn_fix_holes'
      (fuel : Fuel.t)
      (ctx : Ctx.t)
      (u_gen : MetaVar.gen)
      (e : t')
      : option(t' * HTyp.t * MetaVar.gen) := 
        match fuel with 
        | Fuel.Kicked => None
        | Fuel.More fuel => 
        match e with 
        | Asc e1 uty => 
          if is_bidelimited e1 then 
            let ty := UHTyp.expand fuel uty in 
            match ana_fix_holes fuel ctx u_gen e1 ty with 
            | Some (e1', u_gen') => Some (Asc e1' uty, ty, u_gen')
            | None => None
            end
          else None
        | Var x => 
          match Ctx.lookup ctx x with 
          | Some ty => Some (e, ty, u_gen)
          | None => None
          end
        | Lam x e1 => 
          let ctx' := Ctx.extend ctx (x, HTyp.Hole) in 
          match syn_fix_holes fuel ctx' u_gen e1 with 
          | Some (e1', ty2, u_gen') => 
            Some (Lam x e1', HTyp.Arrow HTyp.Hole ty2, u_gen')
          | None => None
          end
        | Let x e1 e2 => 
          match syn_fix_holes fuel ctx u_gen e1 with 
          | Some (e1', ty1, u_gen1) => 
            let ctx1 := Ctx.extend ctx (x, ty1) in 
            match syn_fix_holes fuel ctx1 u_gen1 e2 with 
            | Some (e2', ty2, u_gen2) => 
              Some (Let x e1' e2', ty2, u_gen2)
            | None => None
            end
          | None => None
          end
        | NumLit i => Some (e, HTyp.Num, u_gen)
        | EmptyHole u => Some (e, HTyp.Hole, u_gen)
        | OpSeq skel seq => 
          match syn_skel_fix_holes fuel ctx u_gen skel seq with 
          | Some (skel', seq', ty, u_gen') => 
            Some (OpSeq skel' seq', ty, u_gen')
          | None => None
          end
        | Inj side e1 => 
          match syn_fix_holes fuel ctx u_gen e1 with 
          | Some (e1', ty1, u_gen') => 
            let e' := Inj side e1' in 
            let ty' := 
              match side with 
              | L => HTyp.Sum ty1 HTyp.Hole
              | R => HTyp.Sum HTyp.Hole ty1
              end in 
            Some (e', ty', u_gen')
          | None => None
          end
        | Case _ _ _ => None
        end
        end
    with ana_fix_holes
      (fuel : Fuel.t)
      (ctx : Ctx.t)
      (u_gen : MetaVar.gen)
      (e : t)
      (ty : HTyp.t)
      : option(t * MetaVar.gen) := 
        match fuel with 
        | Fuel.Kicked => None
        | Fuel.More fuel => 
        match e with 
        | Tm _ e1 => 
          match ana_fix_holes' fuel ctx u_gen e1 ty with 
          | Some (err_status, e1', u_gen') => 
            Some (Tm err_status e1', u_gen')
          | None => None
          end
        | Parenthesized e1 => 
          match ana_fix_holes fuel ctx u_gen e1 ty with 
          | Some (e1', u_gen') => 
            Some (Parenthesized e1', u_gen')
          | None => None
          end
        end
        end
    with ana_fix_holes'
      (fuel : Fuel.t) (ctx : Ctx.t) (u_gen : MetaVar.gen)
      (e : t') (ty : HTyp.t)
      : option(err_status * t' * MetaVar.gen) := 
        match fuel with 
        | Fuel.Kicked => None
        | Fuel.More fuel => 
        match e with 
        | Let x e1 e2 => 
          match syn_fix_holes fuel ctx u_gen e1 with 
          | Some (e1', ty1, u_gen1) => 
            let ctx1 := Ctx.extend ctx (x, ty1) in 
            match ana_fix_holes fuel ctx1 u_gen1 e2 ty with 
            | Some (e2', u_gen2) => 
              Some (NotInHole, Let x e1' e2', u_gen2)
            | None => None
            end
          | None => None
          end
        | Lam x e1 => 
          match HTyp.matched_arrow ty with 
          | Some (ty1, ty2) => 
            let ctx' := Ctx.extend ctx (x, ty1) in 
            match ana_fix_holes fuel ctx' u_gen e1 ty2 with 
            | Some (e1', u_gen') => 
              Some (NotInHole, Lam x e1', u_gen')
            | None => None
            end
          | None => 
            match syn_fix_holes' fuel ctx u_gen e with 
            | Some (e', ty', u_gen') => 
              if HTyp.consistent ty ty' then 
                Some (NotInHole, e', u_gen')
              else 
                let (u, u_gen'') := MetaVar.next u_gen' in 
                Some (InHole u, e', u_gen'')
            | None => None
            end
          end
        | Inj side e1 => 
          match HTyp.matched_sum ty with 
          | Some (ty1, ty2) => 
            match ana_fix_holes fuel ctx u_gen e1 (pick_side side ty1 ty2) with 
            | Some (e1', u_gen') => 
              Some (NotInHole, Inj side e1', u_gen')
            | None => None
            end
          | None => 
            match syn_fix_holes' fuel ctx u_gen e with 
            | Some (e', ty', u_gen') => 
              if HTyp.consistent ty ty' then 
                Some (NotInHole, e', u_gen')
              else 
                let (u, u_gen'') := MetaVar.next u_gen' in 
                Some (InHole u, e', u_gen'')
            | None => None
            end
          end
        | Case e1 (x, e2) (y, e3) => 
          match syn_fix_holes fuel ctx u_gen e1 with 
          | Some (e1', ty1, u_gen1) => 
            match 
              match HTyp.matched_sum ty1 with 
              | Some (ty2, ty3) => (e1', u_gen1, ty2, ty3)
              | None => 
                let (e1'', u_gen1') := put_in_new_hole u_gen1 e1' in
                let (ty2, ty3) := (HTyp.Hole, HTyp.Hole) in 
                (e1'', u_gen1', ty2, ty3)
              end with 
            | (e1', u_gen1, ty2, ty3) => 
              let ctx2 := Ctx.extend ctx (x, ty2) in 
              match ana_fix_holes fuel ctx2 u_gen1 e2 ty with 
              | Some (e2', u_gen2) => 
                let ctx3 := Ctx.extend ctx (y, ty3) in 
                match ana_fix_holes fuel ctx3 u_gen2 e3 ty with 
                | Some (e3', u_gen3) => 
                  Some (
                    NotInHole, 
                    Case e1' (x, e2') (y, e3'), 
                    u_gen3)
                | None => None
                end
              | None => None
              end
            end
          | None => None
          end
        | Asc _ _
        | Var _ 
        | NumLit _ 
        | EmptyHole _ 
        | OpSeq _ _ => 
          match syn_fix_holes' fuel ctx u_gen e with 
          | Some (e', ty', u_gen') => 
            if HTyp.consistent ty ty' then 
              Some (NotInHole, e', u_gen')
            else 
              let (u, u_gen'') := MetaVar.next u_gen' in 
              Some (InHole u, e', u_gen'')
          | None => None
          end
        end
        end
    with syn_skel_fix_holes
      (fuel : Fuel.t)
      (ctx : Ctx.t)
      (u_gen : MetaVar.gen)
      (skel : skel_t)
      (seq : opseq)
      : option(skel_t * opseq * HTyp.t * MetaVar.gen) := 
        match fuel with 
        | Fuel.Kicked => None
        | Fuel.More fuel => 
        match skel with 
        | Skel.Placeholder _ n => 
          match OperatorSeq.seq_nth n seq with
          | Some en => 
            if is_bidelimited en then 
              match syn_fix_holes fuel ctx u_gen en with 
              | Some (en', ty, u_gen') => 
                match OperatorSeq.seq_update_nth n seq en' with
                | Some seq' => 
                  Some (skel, seq', ty, u_gen')
                | None => None
                end
              | None => None
              end
            else None
          | None => None
          end
        | Skel.BinOp _ (Plus as op) skel1 skel2 
        | Skel.BinOp _ (Times as op) skel1 skel2 => 
          match ana_skel_fix_holes fuel ctx u_gen skel1 seq HTyp.Num with 
          | Some (skel1', seq1, u_gen1) => 
            match ana_skel_fix_holes fuel ctx u_gen1 skel2 seq1 HTyp.Num with 
            | Some (skel2', seq2, u_gen2) => 
              Some (Skel.BinOp NotInHole op skel1' skel2', seq2, HTyp.Num, u_gen2)
            | None => None
            end
          | None => None
          end
        | Skel.BinOp _ Space skel1 skel2 => 
          match syn_skel_fix_holes fuel ctx u_gen skel1 seq with 
          | Some (skel1', seq1, ty1, u_gen1) => 
            match HTyp.matched_arrow ty1 with 
            | Some (ty2, ty) => 
              match ana_skel_fix_holes fuel ctx u_gen1 skel2 seq1 ty2 with 
              | Some (skel2', seq2, u_gen2) => 
                Some (Skel.BinOp NotInHole Space skel1' skel2', seq2, ty, u_gen2)
              | None => None
              end
            | None =>
              match ana_skel_fix_holes fuel ctx u_gen1 skel2 seq1 HTyp.Hole with 
              | Some (skel2', seq2, u_gen2) => 
                match UHExp.put_skel_in_new_hole u_gen2 skel1' seq2 with 
                | Some (skel1'', seq3, u_gen3) => 
                  Some (Skel.BinOp NotInHole Space skel1'' skel2', seq3, HTyp.Hole, u_gen3)
                | None => None
                end
              | None => None
              end
            end
          | None => None
          end
        end
        end
    with ana_skel_fix_holes
      (fuel : Fuel.t)
      (ctx : Ctx.t)
      (u_gen : MetaVar.gen)
      (skel : skel_t)
      (seq : opseq)
      (ty : HTyp.t)
      : option(skel_t * opseq * MetaVar.gen) := 
        match fuel with 
        | Fuel.Kicked => None
        | Fuel.More fuel => 
          match skel with 
          | Skel.Placeholder _ n => 
            match OperatorSeq.seq_nth n seq with
            | Some en => 
              if is_bidelimited en then 
                match ana_fix_holes fuel ctx u_gen en ty with 
                | Some (en', u_gen') => 
                  match OperatorSeq.seq_update_nth n seq en' with 
                  | Some seq' => Some (skel, seq', u_gen')
                  | None => None
                  end
                | None => None
                end
              else None
            | None => None
            end
          | _ => 
            match syn_skel_fix_holes fuel ctx u_gen skel seq with 
            | Some (skel', seq', ty', u_gen') => 
              if HTyp.consistent ty ty' then Some (skel', seq', u_gen')
              else 
                put_skel_in_new_hole u_gen' skel' seq'
            | None => None
            end
          end
        end.
  End UHExp.

  Inductive cursor_side : Type := 
  | Before : cursor_side
  | After : cursor_side
  | On : cursor_side.

  Module ZTyp.
    Definition cursor_side : Type := cursor_side.

    Inductive t : Type :=
    | CursorT : cursor_side -> UHTyp.t -> t
    | ParenthesizedZ : t -> t
    | OpSeqZ : UHTyp.skel_t -> t -> OperatorSeq.opseq_surround UHTyp.t UHTyp.op -> t.

    Definition opseq_surround : Type := 
      OperatorSeq.opseq_surround UHTyp.t UHTyp.op.
    Definition opseq_prefix : Type := 
      OperatorSeq.opseq_prefix UHTyp.t UHTyp.op.
    Definition opseq_suffix : Type :=
      OperatorSeq.opseq_suffix UHTyp.t UHTyp.op.

    (* |_ -> _ *)
    Definition ZHole_Arrow_Hole : t := 
      OpSeqZ 
        (Skel.BinOp NotInHole UHTyp.Arrow
          (Skel.Placeholder _ O)
          (Skel.Placeholder _ (S O)))
        (CursorT Before UHTyp.Hole)
        (OperatorSeq.EmptyPrefix
          (OperatorSeq.ExpSuffix UHTyp.Arrow UHTyp.Hole)).

    (* |_ + _ *)
    Definition ZHole_Sum_Hole : t := 
      OpSeqZ 
        (Skel.BinOp NotInHole UHTyp.Sum
          (Skel.Placeholder _ O)
          (Skel.Placeholder _ (S O)))
        (CursorT Before UHTyp.Hole)
        (OperatorSeq.EmptyPrefix
          (OperatorSeq.ExpSuffix UHTyp.Sum UHTyp.Hole)).

    Fixpoint erase (zty : t) : UHTyp.t :=
      match zty with
      | CursorT _ ty => ty
      | ParenthesizedZ zty1 => UHTyp.Parenthesized (erase zty1)
      | OpSeqZ skel zty1 surround => 
        let uty1 := erase zty1 in 
        UHTyp.OpSeq skel 
          (OperatorSeq.opseq_of_exp_and_surround uty1 surround)
      end.
  End ZTyp.

  Module ZExp.
    Definition cursor_side : Type := cursor_side.

    Inductive t : Type := 
    | CursorE : cursor_side -> UHExp.t -> t
    | Deeper : err_status -> t' -> t
    | ParenthesizedZ : t -> t
    with t' : Type := 
    | AscZ1 : t -> UHTyp.t -> t'
    | AscZ2 : UHExp.t -> ZTyp.t -> t'
    | LetZ1 : Var.t -> t -> UHExp.t -> t'
    | LetZ2 : Var.t -> UHExp.t -> t -> t'
    | LamZ : Var.t -> t -> t'
    | InjZ : UHExp.inj_side -> t -> t'
    | CaseZ1 : t -> (Var.t * UHExp.t) -> (Var.t * UHExp.t) -> t'
    | CaseZ2 : UHExp.t -> (Var.t * t) -> (Var.t * UHExp.t) -> t'
    | CaseZ3 : UHExp.t -> (Var.t * UHExp.t) -> (Var.t * t) -> t'
    | OpSeqZ : UHExp.skel_t -> t -> OperatorSeq.opseq_surround UHExp.t UHExp.op -> t'.

    Definition opseq_surround : Type := OperatorSeq.opseq_surround UHExp.t UHExp.op.
    Definition opseq_prefix : Type := OperatorSeq.opseq_prefix UHExp.t UHExp.op.
    Definition opseq_suffix : Type := OperatorSeq.opseq_suffix UHExp.t UHExp.op.

    Definition bidelimit ze := 
      match ze with 
      | CursorE cursor_side e => 
        CursorE cursor_side (UHExp.bidelimit e)
      | ParenthesizedZ _ 
      | Deeper _ (InjZ _ _) => ze
      | Deeper _ (AscZ1 _ _) 
      | Deeper _ (AscZ2 _ _)  
      | Deeper _ (LetZ1 _ _ _)
      | Deeper _ (LetZ2 _ _ _)
      | Deeper _ (LamZ _ _)
      | Deeper _ (CaseZ1 _ _ _)
      | Deeper _ (CaseZ2 _ _ _)
      | Deeper _ (CaseZ3 _ _ _)
      | Deeper _ (OpSeqZ _ _ _) => 
        ParenthesizedZ ze
      end.

    Fixpoint put_in_new_hole 
      (u_gen : MetaVar.gen)
      (ze : t) 
      : (t * MetaVar.gen) := 
        match ze with 
        | CursorE cursor_side e => 
          let (e', u_gen') := UHExp.put_in_new_hole u_gen e in  
          (CursorE cursor_side e', u_gen')
        | Deeper (InHole _) _ => 
          (ze, u_gen)
        | Deeper NotInHole ze' => 
          let (u', u_gen') := MetaVar.next u_gen in 
          (Deeper (InHole u') ze', u_gen')
        | ParenthesizedZ ze1 => 
          let (ze1', u_gen') := put_in_new_hole u_gen ze1 in 
          (ParenthesizedZ ze1, u_gen')
        end.

    Fixpoint erase (ze : t) : UHExp.t :=
      match ze with
      | CursorE _ e => e
      | Deeper err_state ze' => 
        let e' := erase' ze' in 
        UHExp.Tm err_state e'
      | ParenthesizedZ ze1 => 
        UHExp.Parenthesized (erase ze1)
      end
    with erase' (ze : t') : UHExp.t' := 
      match ze with 
      | AscZ1 ze' ty => (UHExp.Asc (erase ze') ty)
      | AscZ2 e' zty => UHExp.Asc e' (ZTyp.erase zty)
      | LetZ1 x ze e => UHExp.Let x (erase ze) e
      | LetZ2 x e ze => UHExp.Let x e (erase ze)
      | LamZ x ze' => UHExp.Lam x (erase ze')
      | InjZ side ze => UHExp.Inj side (erase ze)
      | CaseZ1 ze branch1 branch2 => UHExp.Case (erase ze) branch1 branch2
      | CaseZ2 e (x, ze) branch2 => UHExp.Case e (x, (erase ze)) branch2
      | CaseZ3 e branch1 (y, ze) => UHExp.Case e branch1 (y, (erase ze))
      | OpSeqZ skel ze' surround => 
         let e := erase ze' in 
         UHExp.OpSeq skel (OperatorSeq.opseq_of_exp_and_surround e surround)
      end.
  End ZExp.

  Module Path.
    Definition t : Type := list(nat) * ZExp.cursor_side.

    Definition cons' (step : nat) (r : t) : t := 
        match r with (steps, side) => (cons step steps, side) end. 

    Fixpoint of_ztyp (zty : ZTyp.t) : t := 
      match zty with 
      | ZTyp.CursorT cursor_side _ => (nil, cursor_side)
      | ZTyp.ParenthesizedZ zty1 => cons' O (of_ztyp zty1)
      | ZTyp.OpSeqZ _ zty1 surround => 
        let n := OperatorSeq.surround_prefix_length surround in 
        cons' n (of_ztyp zty1)
      end.

    Fixpoint of_zexp (ze : ZExp.t) : t := 
      match ze with 
      | ZExp.CursorE cursor_side _ => (nil, cursor_side)
      | ZExp.Deeper _ ze' => of_zexp' ze'
      | ZExp.ParenthesizedZ ze1 => cons' O (of_zexp ze1)
      end
    with of_zexp' (ze : ZExp.t') : t := 
      match ze with 
      | ZExp.AscZ1 ze' _ => cons' O (of_zexp ze')
      | ZExp.AscZ2 _ ze' => cons' (S O) (of_ztyp ze')
      | ZExp.LetZ1 _ ze' _ => cons' O (of_zexp ze') 
      | ZExp.LetZ2 _ _ ze' => cons' (S O) (of_zexp ze')
      | ZExp.LamZ _ ze' => cons' O (of_zexp ze')
      | ZExp.InjZ _ ze' => cons' O (of_zexp ze')
      | ZExp.CaseZ1 ze' _ _ => cons' O (of_zexp ze')
      | ZExp.CaseZ2 _ (_, ze') _ => cons' (S O) (of_zexp ze')
      | ZExp.CaseZ3 _ _ (_, ze') => cons' (S (S O)) (of_zexp ze')
      | ZExp.OpSeqZ _ ze' surround => 
        let n := OperatorSeq.surround_prefix_length surround in 
        cons' n (of_zexp ze')
      end.

    Definition of_OpSeqZ (ze : ZExp.t) (surround : ZExp.opseq_surround) := 
      let n := OperatorSeq.surround_prefix_length surround in 
      cons' n (of_zexp ze).

    Fixpoint follow_ty (fuel : Fuel.t) (path : t) (uty : UHTyp.t) : option(ZTyp.t) := 
      match fuel with 
      | Fuel.Kicked => None
      | Fuel.More fuel => 
      match path with
      | (nil, cursor_side) => Some (ZTyp.CursorT cursor_side uty)
      | (cons x xs, cursor_side) => 
        match uty with 
        | UHTyp.Num => None
        | UHTyp.Hole => None
        | UHTyp.Parenthesized uty1 => 
          match x with 
          | O => 
            match follow_ty fuel (xs, cursor_side) uty1 with 
            | Some zty => Some (ZTyp.ParenthesizedZ zty)
            | None => None
            end
          | _ => None
          end
        | UHTyp.OpSeq skel seq => 
          match OperatorSeq.split x seq with 
          | Some (uty_n, surround) => 
            match follow_ty fuel (xs, cursor_side) uty_n with 
            | Some zty_n => 
              Some (ZTyp.OpSeqZ skel zty_n surround)
            | None => None
            end
          | None => None
          end
        end
      end
      end.

    Fixpoint follow_e (fuel : Fuel.t) (path : t) (e : UHExp.t) : option(ZExp.t) := 
      match fuel with 
      | Fuel.Kicked => None
      | Fuel.More fuel' => 
      let follow_e := follow_e fuel' in  
      match path with 
      | (nil, cursor_side) => Some (ZExp.CursorE cursor_side e)
      | (cons x xs, cursor_side) => 
        match e with 
        | UHExp.Parenthesized e1 => 
          match x with 
          | O => 
            match follow_e (xs, cursor_side) e1 with 
            | Some ze1 => Some (ZExp.ParenthesizedZ ze1)
            | None => None
            end
          | _ => None
          end
        | UHExp.Tm err_status e => 
          match (x, e) with 
          | (O, UHExp.Asc e1 ty) => 
            match follow_e (xs, cursor_side) e1 with 
            | Some ze => Some (ZExp.Deeper err_status (ZExp.AscZ1 ze ty))
            | None => None
            end
          | (S(O), UHExp.Asc e1 ty) => 
            match follow_ty fuel (xs, cursor_side) ty with 
            | Some ztau => Some (ZExp.Deeper err_status (ZExp.AscZ2 e1 ztau))
            | None => None
            end
          | (_, UHExp.Asc _ _) => None
          | (_, UHExp.Var _) => None
          | (O, UHExp.Let x e1 e2) => 
            match follow_e (xs, cursor_side) e1 with 
            | Some ze => Some (ZExp.Deeper err_status (ZExp.LetZ1 x ze e2))
            | None => None
            end
          | (S(O), UHExp.Let x e1 e2) => 
            match follow_e (xs, cursor_side) e2 with 
            | Some ze => Some (ZExp.Deeper err_status (ZExp.LetZ2 x e1 ze))
            | None => None
            end
          | (_, UHExp.Let _ _ _) => None
          | (O, UHExp.Lam x e1) => 
            match follow_e (xs, cursor_side) e1 with 
            | Some ze => Some (ZExp.Deeper err_status (ZExp.LamZ x ze))
            | None => None
            end
          | (_, UHExp.Lam _ _ ) => None
          | (_, UHExp.NumLit _) => None
          | (O, UHExp.Inj side e1) => 
            match follow_e (xs, cursor_side) e1 with 
            | Some ze => Some (ZExp.Deeper err_status (ZExp.InjZ side ze))
            | None => None
            end
          | (_, UHExp.Inj _ _) => None
          | (O, UHExp.Case e1 (x, e2) (y, e3)) => 
            match follow_e (xs, cursor_side) e1 with 
            | Some ze => Some (ZExp.Deeper err_status (ZExp.CaseZ1 ze (x, e2) (y, e3)))
            | None => None
            end
          | (S(O), UHExp.Case e1 (x, e2) (y, e3)) => 
            match follow_e (xs, cursor_side) e2 with 
            | Some ze => Some (ZExp.Deeper err_status (ZExp.CaseZ2 e1 (x, ze) (y, e3)))
            | None => None
            end
          | (S(S(O)), UHExp.Case e1 (x, e2) (y, e3)) => 
            match follow_e (xs, cursor_side) e3 with 
            | Some ze => Some (ZExp.Deeper err_status (ZExp.CaseZ3 e1 (x, e2) (y, ze)))
            | None => None
            end
          | (_, UHExp.Case _ _ _) => None
          | (_, UHExp.EmptyHole _) => None
          | (n, UHExp.OpSeq skel seq) => 
            match OperatorSeq.split n seq with 
            | Some (e, surround) => 
                match follow_e (xs, cursor_side) e with 
                | Some ze => 
                    Some (ZExp.Deeper err_status (ZExp.OpSeqZ skel ze surround))
                | None => None
                end
            | None => None
            end
          end
        end
      end
      end.
  End Path.

  Module Type ASSOCIATOR.
    (* calls the parser (from OCaml) to produce a skel from an opseq. 
     * initially, there are no errors marked in the skel. *)
    Parameter associate_exp : UHExp.opseq -> UHExp.skel_t.
    Parameter associate_ty : UHTyp.opseq -> UHTyp.skel_t.
    Parameter some_A : forall (A : Type), A -> option(A).
    Parameter none_A : forall (A : Type), unit -> option(A).
  End ASSOCIATOR.

  Module FAction (Associator : ASSOCIATOR).
    Inductive direction : Type :=
    | Child : nat -> direction
    | Parent : direction.

    Inductive shape : Type :=
    (* both types and expressions *)
    | SParenthesized : shape
    (* type shapes *)
    | SNum : shape
    | STyOp : UHTyp.op -> shape
    (* expression shapes *)
    | SAsc : shape
    | SLet : Var.t -> shape
    | SVar : Var.t -> shape
    | SLam : Var.t -> shape
    | SLit : nat -> shape
    | SInj : UHExp.inj_side -> shape
    | SCase : Var.t -> Var.t -> shape
    | SOp : UHExp.op -> shape.

    Inductive t : Type :=
    | MoveTo : Path.t -> t
    | Delete : t
    | Backspace : t
    | Construct : shape -> t.

    Definition make_ty_OpSeqZ 
      (zty0 : ZTyp.t) (surround : ZTyp.opseq_surround)
      : ZTyp.t := 
        let uty0 := ZTyp.erase zty0 in 
        let seq := OperatorSeq.opseq_of_exp_and_surround uty0 surround in 
        let skel := Associator.associate_ty seq in 
        ZTyp.OpSeqZ skel zty0 surround.

    Fixpoint performTyp (fuel : Fuel.t) (a : t) (zty : ZTyp.t) : option ZTyp.t :=
      match (a, zty) with
      (* Movement *)
      | (MoveTo path, _) => 
        let ty := ZTyp.erase zty in 
        Path.follow_ty fuel path ty
      (* Backspace and Delete *)
      | (Backspace, ZTyp.CursorT After uty) 
      | (Backspace, ZTyp.CursorT On uty) => 
        Some (ZTyp.CursorT Before UHTyp.Hole)
      | (Delete, ZTyp.CursorT Before uty) 
      | (Delete, ZTyp.CursorT On uty) => 
        match uty with 
        | UHTyp.Hole => 
          Some (ZTyp.CursorT After uty)
        | _ => 
          Some (ZTyp.CursorT Before UHTyp.Hole)
        end
      | (Backspace, 
          ZTyp.OpSeqZ _
            ((ZTyp.CursorT Before uty0) as zty0)
            surround) => 
        match surround with 
        | OperatorSeq.EmptyPrefix _ => Associator.none_A ZTyp.t tt
        | OperatorSeq.EmptySuffix prefix => 
          match prefix with 
          | OperatorSeq.ExpPrefix uty1 op1 => 
            match uty0 with 
            | UHTyp.Hole => 
              (* uty1 op1 |_ -> uty1| *)
              Some (ZTyp.CursorT After uty1)
            | _ => 
              (* uty1 op1 |uty0 -> |uty0 *)
              Some zty0
            end
          | OperatorSeq.SeqPrefix seq1 op1 => 
            let (uty1, prefix') := OperatorSeq.split_tail seq1 in 
            match uty0 with 
            | UHTyp.Hole => 
              (* prefix' uty1 op1 |_ --> prefix' uty1| *)
              let surround' := OperatorSeq.EmptySuffix prefix' in 
              let ze1 := ZTyp.CursorT After uty1 in 
              Some (make_ty_OpSeqZ ze1 surround')
            | _ => 
              (* prefix' uty1 op |uty0 --> prefix' |uty0 *)
              let surround' := OperatorSeq.EmptySuffix prefix' in 
              Some (make_ty_OpSeqZ zty0 surround')
            end
          end
        | OperatorSeq.BothNonEmpty prefix suffix => 
          match prefix with 
          | OperatorSeq.ExpPrefix uty1 op1 => 
            match uty0 with 
            | UHTyp.Hole => 
              (* uty1 op1 |_ suffix -> uty1| suffix *)
              let surround' := OperatorSeq.EmptyPrefix suffix in 
              let zty1 := ZTyp.CursorT After uty1 in 
              Some (make_ty_OpSeqZ zty1 surround')
            | _ => 
              (* uty1 op1 |uty0 suffix -> |uty0 suffix *)
              let surround' := OperatorSeq.EmptyPrefix suffix in 
              Some (make_ty_OpSeqZ zty0 surround')
            end
          | OperatorSeq.SeqPrefix seq1 op1 => 
            let (uty1, prefix') := OperatorSeq.split_tail seq1 in 
            match uty0 with 
            | UHTyp.Hole => 
              (* prefix' uty1 op1 |_ suffix --> prefix' uty1| suffix *)
              let surround' := OperatorSeq.BothNonEmpty prefix' suffix in 
              let ze1 := ZTyp.CursorT After uty1 in 
              Some (make_ty_OpSeqZ ze1 surround')
            | _ => 
              (* prefix' uty1 op |uty0 suffix --> prefix' |uty0 suffix *)
              let surround' := OperatorSeq.BothNonEmpty prefix' suffix in 
              Some (make_ty_OpSeqZ zty0 surround')
            end
          end
        end
      | (Delete, 
          ZTyp.OpSeqZ _
            ((ZTyp.CursorT After uty0) as zty0)
            surround) => 
        match surround with 
        | OperatorSeq.EmptySuffix _ => None
        | OperatorSeq.EmptyPrefix suffix => 
          match suffix with 
          | OperatorSeq.ExpSuffix op1 uty1 => 
            match uty0 with 
            | UHTyp.Hole => 
              (* _| op1 uty1 -> |uty1 *)
              Some (ZTyp.CursorT Before uty1)
            | _ => 
              (* uty0| op1 uty0 -> uty0| *)
              Some zty0
            end
          | OperatorSeq.SeqSuffix op1 seq1 => 
            let (uty1, suffix') := OperatorSeq.split0 seq1 in 
            match uty0 with 
            | UHTyp.Hole => 
              (* _| op1 uty1 suffix' --> |uty1 suffix' *)
              let surround' := OperatorSeq.EmptyPrefix suffix' in 
              let ze1 := ZTyp.CursorT Before uty1 in 
              Some (make_ty_OpSeqZ ze1 surround')
            | _ => 
              (* uty0| op1 uty1 suffix' --> uty0| suffix' *)
              let surround' := OperatorSeq.EmptyPrefix suffix' in 
              Some (make_ty_OpSeqZ zty0 surround')
            end
          end
        | OperatorSeq.BothNonEmpty prefix suffix => 
          match suffix with 
          | OperatorSeq.ExpSuffix op1 uty1 => 
            match uty0 with 
            | UHTyp.Hole => 
              (* prefix _| op1 uty1 -> prefix |uty1 *)
              let surround' := OperatorSeq.EmptySuffix prefix in 
              let zty1 := ZTyp.CursorT Before uty1 in 
              Some (make_ty_OpSeqZ zty1 surround')
            | _ => 
              (* prefix uty0| op1 uty0 -> prefix uty0| *)
              let surround' := OperatorSeq.EmptySuffix prefix in 
              Some (make_ty_OpSeqZ zty0 surround')
            end
          | OperatorSeq.SeqSuffix op1 seq1 => 
            let (uty1, suffix') := OperatorSeq.split0 seq1 in 
            match uty0 with 
            | UHTyp.Hole => 
              (* prefix _| op1 uty1 suffix' --> prefix |uty1 suffix' *)
              let surround' := OperatorSeq.BothNonEmpty prefix suffix' in 
              let ze1 := ZTyp.CursorT Before uty1 in 
              Some (make_ty_OpSeqZ ze1 surround')
            | _ => 
              (* prefix uty0| op1 uty1 suffix' --> prefix uty0| suffix' *)
              let surround' := OperatorSeq.BothNonEmpty prefix suffix' in 
              Some (make_ty_OpSeqZ zty0 surround')
            end
          end
        end
      (* Construction *)
      | (Construct SParenthesized, ZTyp.CursorT _ _) => 
        Some (ZTyp.ParenthesizedZ zty) 
      | (Construct SNum, ZTyp.CursorT _ UHTyp.Hole) => 
        Some (ZTyp.CursorT After UHTyp.Num)
      | (Construct (STyOp op), ZTyp.CursorT After uty1) 
      | (Construct (STyOp op), ZTyp.CursorT On uty1) => 
        let surround := OperatorSeq.EmptySuffix (OperatorSeq.ExpPrefix uty1 op) in 
        let zty0 := ZTyp.CursorT Before UHTyp.Hole in 
        Some (make_ty_OpSeqZ zty0 surround)
      | (Construct (STyOp op), ZTyp.CursorT Before uty1) => 
        let surround := OperatorSeq.EmptyPrefix (OperatorSeq.ExpSuffix op uty1) in 
        let zty0 := ZTyp.CursorT Before UHTyp.Hole in 
        Some (make_ty_OpSeqZ zty0 surround)
      | (Construct (STyOp op), 
          ZTyp.OpSeqZ _ 
            ((ZTyp.CursorT After uty0) as zty0)
            surround)
      | (Construct (STyOp op), 
          ZTyp.OpSeqZ _ 
            ((ZTyp.CursorT On uty0) as zty0)
            surround) => 
        match surround with 
        | OperatorSeq.EmptyPrefix suffix => 
          (* zty0| suffix -> uty0 op |_ suffix *)
          let prefix' := OperatorSeq.ExpPrefix uty0 op in 
          let surround' := OperatorSeq.BothNonEmpty prefix' suffix in 
          let zty0' := ZTyp.CursorT Before UHTyp.Hole in 
          Some (make_ty_OpSeqZ zty0' surround')
        | OperatorSeq.EmptySuffix prefix => 
          (* prefix zty0| -> prefix uty0 op |_ *)
          let prefix' := OperatorSeq.prefix_append_exp prefix uty0 op in 
          let surround' := OperatorSeq.EmptySuffix prefix' in 
          let zty0' := ZTyp.CursorT Before UHTyp.Hole in 
          Some (make_ty_OpSeqZ zty0' surround')
        | OperatorSeq.BothNonEmpty prefix suffix => 
          (* prefix zty0| suffix -> prefix uty0 op |_ suffix *)
          let prefix' := OperatorSeq.prefix_append_exp prefix uty0 op in 
          let surround' := OperatorSeq.BothNonEmpty prefix' suffix in 
          let zty0' := ZTyp.CursorT Before UHTyp.Hole in 
          Some (make_ty_OpSeqZ zty0' surround')
        end
      | (Construct (STyOp op), 
          ZTyp.OpSeqZ _ 
            ((ZTyp.CursorT Before uty0) as zty0)
            surround) => 
        match surround with 
        | OperatorSeq.EmptyPrefix suffix => 
          (* |zty0 suffix -> |_ op uty0 suffix *)
          let suffix' := OperatorSeq.suffix_prepend_exp suffix op uty0 in 
          let surround' := OperatorSeq.EmptyPrefix suffix' in 
          let zty0' := ZTyp.CursorT Before UHTyp.Hole in 
          Some (make_ty_OpSeqZ zty0' surround')
        | OperatorSeq.EmptySuffix prefix => 
          (* prefix |zty0 -> prefix |_ op uty0 *)
          let suffix' := OperatorSeq.ExpSuffix op uty0 in 
          let surround' := OperatorSeq.BothNonEmpty prefix suffix' in 
          let zty0' := ZTyp.CursorT Before UHTyp.Hole in 
          Some (make_ty_OpSeqZ zty0' surround')
        | OperatorSeq.BothNonEmpty prefix suffix => 
          (* prefix |zty0 suffix -> prefix |_ op uty0 suffix *)
          let suffix' := OperatorSeq.suffix_prepend_exp suffix op uty0 in 
          let surround' := OperatorSeq.BothNonEmpty prefix suffix' in 
          let zty0' := ZTyp.CursorT Before UHTyp.Hole in 
          Some (make_ty_OpSeqZ zty0' surround')
        end
      (* Zipper Cases *)
      | (a, ZTyp.ParenthesizedZ zty1) => 
        match performTyp fuel a zty1 with 
        | Some zty1' => 
          Some (ZTyp.ParenthesizedZ zty1')
        | None => None
        end
      | (a, ZTyp.OpSeqZ skel zty0 surround) => 
        match performTyp fuel a zty0 with 
        | Some zty0' => 
          Some (ZTyp.OpSeqZ skel zty0' surround)
        | None => None
        end
      | _ => None
      end.

    Definition zexp_syn_fix_holes
      (fuel : Fuel.t)
      (ctx : Ctx.t)
      (u_gen : MetaVar.gen)
      (ze : ZExp.t)
      : option (ZExp.t * HTyp.t * MetaVar.gen) := 
        let path := Path.of_zexp ze in 
        let e := ZExp.erase ze in 
        match UHExp.syn_fix_holes fuel ctx u_gen e with 
        | Some (e', ty, u_gen') => 
          match Path.follow_e fuel path e' with 
          | Some ze' => Some (ze', ty, u_gen')
          | None => None
          end
        | None => None
        end.
    
    Definition zexp_ana_fix_holes
      (fuel : Fuel.t)
      (ctx : Ctx.t)
      (u_gen : MetaVar.gen)
      (ze : ZExp.t)
      (ty : HTyp.t)
      : option (ZExp.t * MetaVar.gen) := 
        let path := Path.of_zexp ze in 
        let e := ZExp.erase ze in 
        match UHExp.ana_fix_holes fuel ctx u_gen e ty with 
        | Some (e', u_gen') => 
          match Path.follow_e fuel path e' with 
          | Some ze' => Some (ze', u_gen')
          | None => None
          end
        | None => None
        end.

    Definition make_and_syn_OpSeqZ 
      (fuel : Fuel.t)
      (ctx : Ctx.t)
      (u_gen : MetaVar.gen)
      (ze0 : ZExp.t)
      (surround : ZExp.opseq_surround)
      : option (ZExp.t * HTyp.t * MetaVar.gen) := 
        (* figure out the current path so that we can follow it again 
         * to reconstitute the Z-exp after calling into the UHExp hole 
         * insertion logic (otherwise we'd have to do a version of that
         * logic specific to Z-exps) *)
        let path0 := Path.of_OpSeqZ ze0 surround in 
        let e0 := ZExp.erase ze0 in 
        let seq := OperatorSeq.opseq_of_exp_and_surround e0 surround in 
        let skel := Associator.associate_exp seq in 
        match UHExp.syn_skel_fix_holes fuel ctx u_gen skel seq with 
        | Some (skel', seq', ty, u_gen') => 
          let e' := UHExp.Tm NotInHole (UHExp.OpSeq skel' seq') in 
          match Path.follow_e fuel path0 e' with 
          | Some ze' => Some (ze', ty, u_gen')
          | None => None
          end
        | None => None
        end.

    Definition combine_for_Backspace_Space e1 ze0 := 
      match (e1, ze0) with 
      | (_, ZExp.CursorE _ (UHExp.Tm _ (UHExp.EmptyHole _))) => 
        (* e1 |_ --> e1| *)
        ZExp.CursorE After e1
      | _ => ze0
      end.

    Definition combine_for_Delete_Space ze0 e := 
      match (ze0, e) with 
      | ((ZExp.CursorE After (UHExp.Tm _ (UHExp.EmptyHole _))),
         UHExp.Tm _ (UHExp.EmptyHole _)) => 
        (* _| _ --> _| *)
        ze0
      | ((ZExp.CursorE After (UHExp.Tm _ (UHExp.EmptyHole _))),
         _) => 
        (* _| e --> |e *)
        ZExp.CursorE Before e
      | _ => 
        ze0
      end.

    Fixpoint performSyn 
        (fuel: Fuel.t) 
        (ctx: Ctx.t) 
        (a: t) 
        (ze_ty: (ZExp.t * HTyp.t) * MetaVar.gen) 
        : option ((ZExp.t * HTyp.t) * MetaVar.gen) :=
      match fuel with
      | Fuel.Kicked => None
      | Fuel.More fuel =>
      match ze_ty with 
      | (ze, ty, u_gen) => 
        match (a, ze) with
          (* Movement *)
          | (MoveTo path, _) => 
            let e := ZExp.erase ze in
            match Path.follow_e fuel path e with
            | Some ze' => Some (ze', ty, u_gen)
            | None => None
            end
          (* Backspace & Deletion *)
          | (Backspace, ZExp.CursorE After e) => 
            match e with 
            | UHExp.Tm _ (UHExp.EmptyHole _) => 
              Some (ZExp.CursorE Before e, ty, u_gen)
            | _ => 
              let (e', u_gen') := UHExp.new_EmptyHole u_gen in 
              Some (ZExp.CursorE Before e', HTyp.Hole, u_gen')
            end
          | (Delete, ZExp.CursorE Before e) => 
            match e with 
            | UHExp.Tm _ (UHExp.EmptyHole _) => 
              Some (ZExp.CursorE After e, ty, u_gen)
            | _ => 
              let (e', u_gen') := UHExp.new_EmptyHole u_gen in 
              Some (ZExp.CursorE Before e', HTyp.Hole, u_gen)
            end
          | (Backspace, ZExp.CursorE On e)
          | (Delete, ZExp.CursorE On e) => 
            let (e', u_gen') := UHExp.new_EmptyHole u_gen in 
            let ze' := ZExp.CursorE Before e' in 
            Some (ze', HTyp.Hole, u_gen')
          | (Backspace, 
              ZExp.Deeper _ (ZExp.AscZ2 e1 
                (ZTyp.CursorT Before uty1))) => 
            let ze' := ZExp.CursorE After e1 in 
            zexp_syn_fix_holes fuel ctx u_gen ze'
          | (Backspace,
              ZExp.Deeper _ (ZExp.AscZ2 e1 
                (ZTyp.OpSeqZ _ 
                  (ZTyp.CursorT Before _)
                  (OperatorSeq.EmptyPrefix _)))) => 
            let ze' := ZExp.CursorE After e1 in 
            zexp_syn_fix_holes fuel ctx u_gen ze'
          | (Backspace, ZExp.Deeper _
              (ZExp.OpSeqZ _
                ((ZExp.CursorE Before e0) as ze0) 
                ((OperatorSeq.EmptySuffix _) as surround)))
          | (Backspace, ZExp.Deeper _
              (ZExp.OpSeqZ _
                ((ZExp.CursorE Before e0) as ze0) 
                ((OperatorSeq.BothNonEmpty _ _) as surround))) =>
              match surround with 
              | OperatorSeq.EmptyPrefix _ => None (* precluded by pattern match above *)
              | OperatorSeq.EmptySuffix prefix => 
                match prefix with 
                | OperatorSeq.ExpPrefix e1 op1 => 
                  (* e1 op1 |ze0 *)
                  match op1 with 
                  | UHExp.Space => 
                    (* e1 |ze0 *)
                    let ze0' := combine_for_Backspace_Space e1 ze0 in  
                    zexp_syn_fix_holes fuel ctx u_gen ze0' 
                  | _ => 
                    match (e1, e0) with 
                    | (UHExp.Tm _ (UHExp.EmptyHole _), 
                       UHExp.Tm _ (UHExp.EmptyHole _)) => 
                      (* _1 op1 |_0 --> _1| *)
                      let ze0' := ZExp.CursorE After e1 in 
                      Some (ze0', HTyp.Hole, u_gen)
                    | (UHExp.Tm _ (UHExp.EmptyHole _), _) => 
                      (* _1 op1 |e0 --> |e0 *)
                      zexp_syn_fix_holes fuel ctx u_gen ze0
                    | (_, UHExp.Tm _ (UHExp.EmptyHole _)) => 
                      (* e1 op1 |_0 --> e1| *)
                      let ze0' := ZExp.CursorE After e1 in 
                      zexp_syn_fix_holes fuel ctx u_gen ze0'
                    | _ => 
                      (* e1 op1 |ze0 --> e1 |ze0 *)
                      let surround' := 
                        OperatorSeq.EmptySuffix 
                          (OperatorSeq.ExpPrefix e1 UHExp.Space) in 
                      make_and_syn_OpSeqZ fuel ctx u_gen ze0 surround' 
                    end
                  end
                | OperatorSeq.SeqPrefix seq1 op1 => 
                  (* seq1 op1 |ze0 *)
                  match op1 with 
                  | UHExp.Space =>
                    (* seq1 |ze0 *)
                    let (e1, prefix') := OperatorSeq.split_tail seq1 in 
                    let surround' := OperatorSeq.EmptySuffix prefix' in 
                    let ze0' := combine_for_Backspace_Space e1 ze0 in 
                    make_and_syn_OpSeqZ fuel ctx u_gen ze0' surround'
                  | _ => 
                    let (e1, prefix') := OperatorSeq.split_tail seq1 in 
                    match (e1, e0) with 
                    | (_, UHExp.Tm _ (UHExp.EmptyHole _)) => 
                      (* prefix' e1 op1 |_0 --> prefix' e1| *)
                      let surround' := OperatorSeq.EmptySuffix prefix' in 
                      let ze0' := ZExp.CursorE After e1 in 
                      make_and_syn_OpSeqZ fuel ctx u_gen ze0' surround'
                    | (UHExp.Tm _ (UHExp.EmptyHole _), _) => 
                      (* prefix' _1 op1 |e0 --> prefix' |e0 *)
                      let surround' := OperatorSeq.EmptySuffix prefix' in 
                      make_and_syn_OpSeqZ fuel ctx u_gen ze0 surround'
                    | _ => 
                      (* seq1 op1 |ze0 --> seq1 |ze0 *)
                      let prefix' := OperatorSeq.SeqPrefix seq1 (UHExp.Space) in 
                      let surround' := OperatorSeq.EmptySuffix prefix' in 
                      make_and_syn_OpSeqZ fuel ctx u_gen ze0 surround' 
                    end
                  end
                end
              | OperatorSeq.BothNonEmpty prefix suffix => 
                match prefix with 
                | OperatorSeq.ExpPrefix e1 op1 => 
                  (* e1 op1 |ze0 ...suffix *)
                  match op1 with 
                  | UHExp.Space => 
                    (* e1 |ze0 ...suffix *)
                    let ze0' := combine_for_Backspace_Space e1 ze0 in  
                    let surround' := OperatorSeq.EmptyPrefix suffix in 
                    make_and_syn_OpSeqZ fuel ctx u_gen ze0' surround'
                  | _ => 
                    match (e1, e0) with 
                    | (_, UHExp.Tm _ (UHExp.EmptyHole _)) => 
                      (* e1 op1 |_0 suffix --> e1| suffix *)
                      let surround' := OperatorSeq.EmptyPrefix suffix in 
                      let ze0' := ZExp.CursorE After e1 in 
                      make_and_syn_OpSeqZ fuel ctx u_gen ze0' surround'
                    | (UHExp.Tm _ (UHExp.EmptyHole _), _) => 
                      (* _1 op1 |e0 suffix --> |e0 suffix *)
                      let surround' := OperatorSeq.EmptyPrefix suffix in 
                      make_and_syn_OpSeqZ fuel ctx u_gen ze0 surround'
                    | _ => 
                    (* e1 op1 |ze0 --> e1 |ze0 ...suffix *)
                      let surround' := 
                        OperatorSeq.BothNonEmpty 
                          (OperatorSeq.ExpPrefix e1 UHExp.Space) 
                          suffix in 
                      make_and_syn_OpSeqZ fuel ctx u_gen ze0 surround' 
                    end
                  end
                | OperatorSeq.SeqPrefix seq1 op1 => 
                  (* seq1 op1 |ze0 ...suffix *)
                  match op1 with 
                  | UHExp.Space =>
                    (* seq1 |ze0 ...suffix *)
                    let (e1, prefix') := OperatorSeq.split_tail seq1 in 
                    let ze0' :=  combine_for_Backspace_Space e1 ze0 in  
                    let surround' := OperatorSeq.BothNonEmpty prefix' suffix in 
                    make_and_syn_OpSeqZ fuel ctx u_gen ze0' surround'
                  | _ => 
                    let (e1, prefix') := OperatorSeq.split_tail seq1 in 
                    match (e1, e0) with 
                    | (_, UHExp.Tm _ (UHExp.EmptyHole _)) => 
                      (* prefix' e1 op1 |_0 suffix --> prefix' e1| suffix *)
                      let surround' := OperatorSeq.BothNonEmpty prefix' suffix in 
                      let ze0' := ZExp.CursorE After e1 in 
                      make_and_syn_OpSeqZ fuel ctx u_gen ze0' surround'
                    | (UHExp.Tm _ (UHExp.EmptyHole _), _) => 
                      (* prefix' _1 op1 |e0 suffix --> prefix' |e0 suffix *)
                      let surround' := OperatorSeq.BothNonEmpty prefix' suffix in 
                      make_and_syn_OpSeqZ fuel ctx u_gen ze0 surround'
                    | _ => 
                      (* seq1 op1 |ze0 suffix --> seq1 |ze0 suffix *)
                      let prefix' := OperatorSeq.SeqPrefix seq1 (UHExp.Space) in 
                      let surround' := OperatorSeq.BothNonEmpty prefix' suffix in 
                      make_and_syn_OpSeqZ fuel ctx u_gen ze0 surround' 
                    end
                  end
                end
              end
          | (Delete, ZExp.Deeper _
              (ZExp.OpSeqZ _
                ((ZExp.CursorE After e0) as ze0)
                ((OperatorSeq.EmptyPrefix _) as surround)))
          | (Delete, ZExp.Deeper _ 
              (ZExp.OpSeqZ _
                ((ZExp.CursorE After e0) as ze0)
                ((OperatorSeq.BothNonEmpty _ _) as surround))) => 
            match surround with 
            | OperatorSeq.EmptySuffix _ => None (* precluded by pattern match above *)
            | OperatorSeq.EmptyPrefix suffix => 
              match suffix with 
              | OperatorSeq.ExpSuffix op e1 => 
                match op with 
                | UHExp.Space => 
                  let ze0' := combine_for_Delete_Space ze0 e1 in 
                  zexp_syn_fix_holes fuel ctx u_gen ze0'  
                | _ => 
                  match (e0, e1) with 
                  | (UHExp.Tm _ (UHExp.EmptyHole _), 
                     UHExp.Tm _ (UHExp.EmptyHole _)) => 
                    (* _0| op _1 --> _0| *)
                    Some (ze0, HTyp.Hole, u_gen)
                  | (UHExp.Tm _ (UHExp.EmptyHole _), _) => 
                    (* _0| op e1 --> |e1 *)
                    let ze1 := ZExp.CursorE Before e1  in 
                    zexp_syn_fix_holes fuel ctx u_gen ze1
                  | (_, UHExp.Tm _ (UHExp.EmptyHole _)) => 
                    (* e0| op _ --> e0| *)
                    zexp_syn_fix_holes fuel ctx u_gen ze0
                  | _ => 
                    (* e0| op e1 --> e0| e1 *)
                    let surround' := 
                      OperatorSeq.EmptyPrefix 
                        (OperatorSeq.ExpSuffix UHExp.Space e1) in 
                    make_and_syn_OpSeqZ fuel ctx u_gen ze0 surround' 
                  end
                end
              | OperatorSeq.SeqSuffix op seq => 
                match op with 
                | UHExp.Space => 
                  let (e, suffix') := OperatorSeq.split0 seq in
                  let surround' := OperatorSeq.EmptyPrefix suffix' in 
                  let ze0' := combine_for_Delete_Space ze0 e in 
                  make_and_syn_OpSeqZ fuel ctx u_gen ze0' surround'
                | _ => 
                  let (e1, suffix') := OperatorSeq.split0 seq in 
                  match (e0, e1) with 
                  | (_, UHExp.Tm _ (UHExp.EmptyHole _)) => 
                    (* e0| op _ suffix' --> e0| suffix' *)
                    let surround' := OperatorSeq.EmptyPrefix suffix' in 
                    make_and_syn_OpSeqZ fuel ctx u_gen ze0 surround'
                  | (UHExp.Tm _ (UHExp.EmptyHole _), _) => 
                    (* _0| op e1 suffix' --> |e1 suffix' *)
                    let surround' := OperatorSeq.EmptyPrefix suffix' in 
                    let ze1 := ZExp.CursorE Before e1  in 
                    make_and_syn_OpSeqZ fuel ctx u_gen ze1 surround'
                  | _ => 
                    (* e0| op seq --> e0| seq *)
                    let suffix' := OperatorSeq.SeqSuffix UHExp.Space seq in 
                    let surround' := OperatorSeq.EmptyPrefix suffix' in 
                    make_and_syn_OpSeqZ fuel ctx u_gen ze0 surround' 
                  end
                end
              end
            | OperatorSeq.BothNonEmpty prefix suffix => 
              match suffix with 
              | OperatorSeq.ExpSuffix op e1 => 
                match op with 
                | UHExp.Space => 
                  let ze0' := combine_for_Delete_Space ze0 e1 in 
                  let surround' := OperatorSeq.EmptySuffix prefix in 
                  make_and_syn_OpSeqZ fuel ctx u_gen ze0' surround'
                | _ => 
                  match (e0, e1) with 
                  | (_, UHExp.Tm _ (UHExp.EmptyHole _)) => 
                    (* prefix e0| op _ --> prefix e0| *)
                    let surround' := OperatorSeq.EmptySuffix prefix in 
                    make_and_syn_OpSeqZ fuel ctx u_gen ze0 surround'
                  | (UHExp.Tm _ (UHExp.EmptyHole _), _) => 
                    (* prefix _0| op e1 --> prefix |e1 *)
                    let surround' := OperatorSeq.EmptySuffix prefix in 
                    let ze1 := ZExp.CursorE Before e1  in 
                    make_and_syn_OpSeqZ fuel ctx u_gen ze1 surround'
                  | _ => 
                    (* prefix e0| op e1 --> e0| e1 *)
                    let surround' := 
                      OperatorSeq.BothNonEmpty prefix 
                        (OperatorSeq.ExpSuffix UHExp.Space e1) in 
                    make_and_syn_OpSeqZ fuel ctx u_gen ze0 surround'
                  end
                end
              | OperatorSeq.SeqSuffix op seq => 
                match op with 
                | UHExp.Space => 
                  let (e, suffix') := OperatorSeq.split0 seq in 
                  let ze0' := combine_for_Delete_Space ze0 e in 
                  let surround' := OperatorSeq.BothNonEmpty prefix suffix' in 
                  make_and_syn_OpSeqZ fuel ctx u_gen ze0' surround'
                | _ => 
                  let (e1, suffix') := OperatorSeq.split0 seq in 
                  match (e0, e1) with 
                  | (_, UHExp.Tm _ (UHExp.EmptyHole _)) => 
                    (* prefix e0| op _ suffix' --> prefix e0| suffix' *)
                    let surround' := OperatorSeq.BothNonEmpty prefix suffix' in 
                    make_and_syn_OpSeqZ fuel ctx u_gen ze0 surround'
                  | (UHExp.Tm _ (UHExp.EmptyHole _), _) => 
                    (* prefix _0| op e1 suffix' --> prefix |e1 suffix' *)
                    let surround' := OperatorSeq.BothNonEmpty prefix suffix' in 
                    let ze1 := ZExp.CursorE Before e1  in 
                    make_and_syn_OpSeqZ fuel ctx u_gen ze1 surround'
                  | _ => 
                    (* prefix e| op seq --> e| seq *)
                    let suffix' := OperatorSeq.SeqSuffix UHExp.Space seq in 
                    let surround' := OperatorSeq.BothNonEmpty prefix suffix' in 
                    make_and_syn_OpSeqZ fuel ctx u_gen ze0 surround'
                  end
                end
              end
            end
          (* Construction *)
          | (Construct SParenthesized, ZExp.CursorE cursor_side e) => 
            Some (
              ZExp.ParenthesizedZ ze, 
              ty,
              u_gen)
          | (Construct SAsc, ZExp.CursorE _ e) =>
            let e' := UHExp.bidelimit e in 
            Some (
              ZExp.Deeper NotInHole 
                (ZExp.AscZ2 e' (ZTyp.CursorT Before UHTyp.Hole)), 
              ty, 
              u_gen)
          | (Construct (SVar x), ZExp.CursorE _ (UHExp.Tm _ (UHExp.EmptyHole _))) (* SAConVar *) =>
            match Ctx.lookup ctx x with
            | Some xty => Some (ZExp.CursorE After 
              (UHExp.Tm NotInHole (UHExp.Var x)), 
              xty, u_gen)
            | None => None
            end
          | (Construct (SLet x), ZExp.CursorE _ e) =>
            match e with
            | UHExp.Tm _ (UHExp.EmptyHole u) =>
              let (new_hole, u_gen') := UHExp.new_EmptyHole u_gen in 
              Some (ZExp.Deeper NotInHole 
                (ZExp.LetZ1 
                  x (ZExp.CursorE Before e) 
                  new_hole),
                HTyp.Hole, 
                u_gen')
            | _ =>
              let (new_hole, u_gen') := UHExp.new_EmptyHole u_gen in 
              Some (ZExp.Deeper NotInHole 
                (ZExp.LetZ2 
                  x e 
                  (ZExp.CursorE Before new_hole)), 
                HTyp.Hole, 
                u_gen')
            end
          | (Construct (SLam x), ZExp.CursorE _ (UHExp.Tm _ (UHExp.EmptyHole u))) =>
            let (new_hole, u_gen') := UHExp.new_EmptyHole u_gen in 
            Some (ZExp.Deeper NotInHole (ZExp.AscZ2
                (UHExp.Parenthesized (UHExp.Tm NotInHole (UHExp.Lam x new_hole)))
                ZTyp.ZHole_Arrow_Hole), 
             HTyp.Arrow HTyp.Hole HTyp.Hole, u_gen')
          | (Construct (SLit n), ZExp.CursorE _ (UHExp.Tm _ (UHExp.EmptyHole u))) (* SAConNumLit *) =>
              Some (ZExp.CursorE After (UHExp.Tm NotInHole (UHExp.NumLit n)), HTyp.Num, u_gen)
          | (Construct (SInj side), ZExp.CursorE _ (UHExp.Tm _ (UHExp.EmptyHole u))) (* 24a *) =>
            let (new_hole, u_gen') := UHExp.new_EmptyHole u_gen in
            Some (ZExp.Deeper NotInHole 
              (ZExp.AscZ2 
                (UHExp.Tm NotInHole (UHExp.Inj side new_hole)) 
                ZTyp.ZHole_Sum_Hole),
              (HTyp.Sum HTyp.Hole HTyp.Hole), u_gen'
            )
          | (Construct (SCase x y), (ZExp.CursorE _ e)) =>
            match HTyp.matched_sum ty with
            | Some _ => 
              let (new_hole1, u_gen') := UHExp.new_EmptyHole u_gen in 
              let (new_hole2, u_gen'') := UHExp.new_EmptyHole u_gen' in 
              let casez2 := 
                ZExp.Deeper NotInHole 
                  (ZExp.CaseZ2 e
                    (x, ZExp.CursorE Before new_hole1)
                    (y, new_hole2)) in
              Some (
                ZExp.Deeper NotInHole (
                  ZExp.AscZ1 (ZExp.ParenthesizedZ casez2) UHTyp.Hole), 
                HTyp.Hole, u_gen'')
            | None => 
              let (ze', u_gen') := ZExp.put_in_new_hole u_gen ze in 
              let (new_hole2, u_gen'') := UHExp.new_EmptyHole u_gen' in 
              let (new_hole3, u_gen''') := UHExp.new_EmptyHole u_gen'' in 
              Some (
                (ZExp.Deeper NotInHole (ZExp.AscZ1
                  (ZExp.ParenthesizedZ (ZExp.Deeper NotInHole (
                    ZExp.CaseZ1 ze' (x, new_hole2) (y, new_hole3))))
                  UHTyp.Hole)), 
              HTyp.Hole, u_gen''')
            end
          | (Construct (SOp op), ZExp.Deeper _ (
              ZExp.OpSeqZ _ (ZExp.CursorE On e) surround))
          | (Construct (SOp op), ZExp.Deeper _ (
              ZExp.OpSeqZ _ (ZExp.CursorE After e) surround)) => 
            match surround with 
            | OperatorSeq.EmptySuffix prefix => 
              let prefix' := OperatorSeq.prefix_append_exp prefix e op in 
              let surround' := OperatorSeq.EmptySuffix prefix' in 
              let (new_hole, u_gen') := UHExp.new_EmptyHole u_gen in 
              let ze0' := ZExp.CursorE Before new_hole in 
              make_and_syn_OpSeqZ fuel ctx u_gen' ze0' surround' 
            | OperatorSeq.EmptyPrefix suffix => 
              match suffix with 
              | OperatorSeq.ExpSuffix op' e' => 
                match op with 
                | UHExp.Space => 
                  (* e| op' e' --> e |_ op' e' *)
                  let prefix' := OperatorSeq.ExpPrefix e op in 
                  let suffix' := OperatorSeq.ExpSuffix op' e' in 
                  let surround' := OperatorSeq.BothNonEmpty prefix' suffix' in 
                  let (new_hole, u_gen') := UHExp.new_EmptyHole u_gen in 
                  let ze0' := ZExp.CursorE Before new_hole in 
                  make_and_syn_OpSeqZ fuel ctx u_gen' ze0' surround' 
                | _ =>                     
                  match op' with 
                  | UHExp.Space => 
                    (* e| e' --> e op |e' *)
                    let prefix' := OperatorSeq.ExpPrefix e op in 
                    let surround' := OperatorSeq.EmptySuffix prefix' in 
                    let ze0' := ZExp.CursorE Before e' in 
                    make_and_syn_OpSeqZ fuel ctx u_gen ze0' surround'  
                  | _ => 
                    (* e| op' e' --> e op |_ op' e' *)
                    let prefix' := OperatorSeq.ExpPrefix e op in 
                    let suffix' := OperatorSeq.ExpSuffix op' e' in 
                    let surround' := OperatorSeq.BothNonEmpty prefix' suffix' in 
                    let (new_hole, u_gen') := UHExp.new_EmptyHole u_gen in 
                    let ze0' := ZExp.CursorE Before new_hole in 
                    make_and_syn_OpSeqZ fuel ctx u_gen' ze0' surround' 
                  end
                end
              | OperatorSeq.SeqSuffix op' seq' => 
                match op with 
                | UHExp.Space => 
                  (* e| seq' --> e |_ op' seq' *)
                  let prefix' := OperatorSeq.ExpPrefix e op in 
                  let surround' := OperatorSeq.BothNonEmpty prefix' suffix in 
                  let (new_hole, u_gen') := UHExp.new_EmptyHole u_gen in 
                  let ze0' := ZExp.CursorE Before new_hole in 
                  make_and_syn_OpSeqZ fuel ctx u_gen' ze0' surround'
                | _ => 
                  match op' with 
                  | UHExp.Space => 
                    (* e| seq' --> e op |seq' *)
                    let prefix' := OperatorSeq.ExpPrefix e op in 
                    let (e0', suffix') := OperatorSeq.split0 seq' in 
                    let surround' := OperatorSeq.BothNonEmpty prefix' suffix' in 
                    let ze0' := ZExp.CursorE Before e0' in 
                    make_and_syn_OpSeqZ fuel ctx u_gen ze0' surround' 
                  | _ => 
                    (* e| op' seq' --> e op |_ op' seq' *)
                    let prefix' := OperatorSeq.ExpPrefix e op in 
                    let surround' := OperatorSeq.BothNonEmpty prefix' suffix in 
                    let (new_hole, u_gen') := UHExp.new_EmptyHole u_gen in 
                    let ze0' := ZExp.CursorE Before new_hole in 
                    make_and_syn_OpSeqZ fuel ctx u_gen' ze0' surround' 
                  end
                end
              end
            | OperatorSeq.BothNonEmpty prefix suffix => 
              match suffix with 
              | OperatorSeq.ExpSuffix op' e' => 
                match op with 
                | UHExp.Space => 
                  (* prefix e| op' e' --> prefix e |_ op' e' *)
                  let prefix' := OperatorSeq.prefix_append_exp prefix e op in 
                  let suffix' := OperatorSeq.ExpSuffix op' e' in 
                  let surround' := OperatorSeq.BothNonEmpty prefix' suffix' in 
                  let (new_hole, u_gen') := UHExp.new_EmptyHole u_gen in 
                  let ze0' := ZExp.CursorE Before new_hole in 
                  make_and_syn_OpSeqZ fuel ctx u_gen' ze0' surround' 
                | _ => 
                  match op' with 
                  | UHExp.Space => 
                    (* prefix e| e' --> prefix e op |e' *)
                    let prefix' := OperatorSeq.prefix_append_exp prefix e op in 
                    let surround' := OperatorSeq.EmptySuffix prefix' in 
                    let ze0' := ZExp.CursorE Before e' in 
                    make_and_syn_OpSeqZ fuel ctx u_gen ze0' surround' 
                  | _ => 
                    (* prefix e| op' e' --> prefix e op |_ op' e' *)
                    let prefix' := OperatorSeq.prefix_append_exp prefix e op in 
                    let suffix' := OperatorSeq.ExpSuffix op' e' in 
                    let surround' := OperatorSeq.BothNonEmpty prefix' suffix' in 
                    let (new_hole, u_gen') := UHExp.new_EmptyHole u_gen in 
                    let ze0' := ZExp.CursorE Before new_hole in 
                    make_and_syn_OpSeqZ fuel ctx u_gen' ze0' surround' 
                  end
                end
              | OperatorSeq.SeqSuffix op' seq' => 
                match op with 
                | UHExp.Space => 
                  (* prefix e| op' seq' --> prefix e |_ op' seq' *)
                  let prefix' := OperatorSeq.prefix_append_exp prefix e op in 
                  let surround' := OperatorSeq.BothNonEmpty prefix' suffix in 
                  let (new_hole, u_gen') := UHExp.new_EmptyHole u_gen in 
                  let ze0' := ZExp.CursorE Before new_hole in 
                  make_and_syn_OpSeqZ fuel ctx u_gen' ze0' surround'
                | _ => 
                  match op' with 
                  | UHExp.Space => 
                    (* prefix e| seq' --> prefix e op |seq' *)
                    let prefix' := OperatorSeq.prefix_append_exp prefix e op in 
                    let (e0', suffix') := OperatorSeq.split0 seq' in 
                    let surround' := OperatorSeq.BothNonEmpty prefix' suffix' in 
                    let ze0' := ZExp.CursorE Before e0' in 
                    make_and_syn_OpSeqZ fuel ctx u_gen ze0' surround' 
                  | _ => 
                    (* prefix e| op' seq' --> prefix e op |_ op' seq' *)
                    let prefix' := OperatorSeq.prefix_append_exp prefix e op in 
                    let surround' := OperatorSeq.BothNonEmpty prefix' suffix in 
                    let (new_hole, u_gen') := UHExp.new_EmptyHole u_gen in 
                    let ze0' := ZExp.CursorE Before new_hole in 
                    make_and_syn_OpSeqZ fuel ctx u_gen' ze0' surround'
                  end
                end
              end
            end
          | (Construct (SOp op), 
              ZExp.Deeper _ (ZExp.OpSeqZ _
                ((ZExp.CursorE Before _) as ze0) surround)) =>
            match surround with 
            | OperatorSeq.EmptyPrefix suffix => 
              (* |ze0 ... --> |_ op e0 ... *)
              let e0 := ZExp.erase ze0 in 
              let suffix' := OperatorSeq.suffix_prepend_exp suffix op e0 in 
              let surround' := OperatorSeq.EmptyPrefix suffix' in 
              let (new_hole, u_gen') := UHExp.new_EmptyHole u_gen in 
              let ze0' := ZExp.CursorE Before new_hole in 
              make_and_syn_OpSeqZ fuel ctx u_gen' ze0' surround' 
            | OperatorSeq.EmptySuffix ((OperatorSeq.ExpPrefix e1 (UHExp.Space)) as prefix) => 
              match op with 
              | UHExp.Space => 
                (* e1 |ze0 --> e1 |_ e0 *)
                let e0 := ZExp.erase ze0 in 
                let suffix' := OperatorSeq.ExpSuffix UHExp.Space e0 in 
                let surround' := OperatorSeq.BothNonEmpty prefix suffix' in 
                let (new_hole, u_gen') := UHExp.new_EmptyHole u_gen in 
                let ze0' := ZExp.CursorE Before new_hole in 
                make_and_syn_OpSeqZ fuel ctx u_gen' ze0' surround' 
              | _ => 
                (* e1 |ze0 --> e1 op |ze0 *)
                let surround' := OperatorSeq.EmptySuffix (OperatorSeq.ExpPrefix e1 op) in 
                make_and_syn_OpSeqZ fuel ctx u_gen ze0 surround' 
              end
            | OperatorSeq.EmptySuffix ((OperatorSeq.SeqPrefix seq1 (UHExp.Space)) as prefix) => 
              match op with 
              | UHExp.Space => 
                (* seq1 |ze0 --> seq1 |_ e0 *)
                let e0 := ZExp.erase ze0 in 
                let suffix' := OperatorSeq.ExpSuffix UHExp.Space e0 in 
                let surround' := OperatorSeq.BothNonEmpty prefix suffix' in 
                let (new_hole, u_gen') := UHExp.new_EmptyHole u_gen in 
                let ze0' := ZExp.CursorE Before new_hole in 
                make_and_syn_OpSeqZ fuel ctx u_gen' ze0' surround'
              | _ => 
                (* seq1 |ze0 --> seq1 op |ze0 *)
                let surround' := OperatorSeq.EmptySuffix (OperatorSeq.SeqPrefix seq1 op) in 
                make_and_syn_OpSeqZ fuel ctx u_gen ze0 surround' 
              end
            | OperatorSeq.EmptySuffix prefix => 
              (* prefix [^ ] |ze0 --> prefix |_ op e0 *)
              let e0 := ZExp.erase ze0 in 
              let suffix' := OperatorSeq.ExpSuffix op e0 in 
              let surround' := OperatorSeq.BothNonEmpty prefix suffix' in 
              let (new_hole, u_gen') := UHExp.new_EmptyHole u_gen in 
              let ze0' := ZExp.CursorE Before new_hole in 
              make_and_syn_OpSeqZ fuel ctx u_gen' ze0' surround' 
            | OperatorSeq.BothNonEmpty ((OperatorSeq.ExpPrefix e1 (UHExp.Space)) as prefix) suffix => 
              match op with 
              | UHExp.Space => 
                (* e1 |ze0 suffix --> e1 |_ e0 suffix *)
                let e0 := ZExp.erase ze0 in 
                let suffix' := OperatorSeq.suffix_prepend_exp suffix UHExp.Space e0 in 
                let surround' := OperatorSeq.BothNonEmpty prefix suffix' in 
                let (new_hole, u_gen') := UHExp.new_EmptyHole u_gen in 
                let ze0' := ZExp.CursorE Before new_hole in 
                make_and_syn_OpSeqZ fuel ctx u_gen' ze0' surround'
              | _ => 
                (* e1 |ze0 suffix --> e1 op |ze0 suffix *)
                let prefix' := OperatorSeq.ExpPrefix e1 op in 
                let surround' := OperatorSeq.BothNonEmpty prefix' suffix in 
                make_and_syn_OpSeqZ fuel ctx u_gen ze0 surround'
              end
            | OperatorSeq.BothNonEmpty ((OperatorSeq.SeqPrefix seq1 (UHExp.Space)) as prefix) suffix => 
              match op with 
              | UHExp.Space => 
                (* seq1 |ze0 suffix --> seq1 |_ e0 suffix *)
                let e0 := ZExp.erase ze0 in 
                let suffix' := OperatorSeq.suffix_prepend_exp suffix UHExp.Space e0 in 
                let surround' := OperatorSeq.BothNonEmpty prefix suffix' in 
                let (new_hole, u_gen') := UHExp.new_EmptyHole u_gen in 
                let ze0' := ZExp.CursorE Before new_hole in 
                make_and_syn_OpSeqZ fuel ctx u_gen' ze0' surround'
              | _ => 
                (* seq1 |ze0 suffix --> seq1 op |ze0 suffix *)
                let prefix' := OperatorSeq.SeqPrefix seq1 op in 
                let surround' := OperatorSeq.BothNonEmpty prefix' suffix in 
                make_and_syn_OpSeqZ fuel ctx u_gen ze0 surround'
              end
            | OperatorSeq.BothNonEmpty prefix suffix => 
              (* prefix [^ ] |ze0 suffix --> prefix |_ op e0 suffix *)
              let e0 := ZExp.erase ze0 in 
              let suffix' := OperatorSeq.suffix_prepend_exp suffix op e0 in 
              let surround' := OperatorSeq.BothNonEmpty prefix suffix' in 
              let (new_hole, u_gen') := UHExp.new_EmptyHole u_gen in 
              let ze0' := ZExp.CursorE Before new_hole in 
              make_and_syn_OpSeqZ fuel ctx u_gen' ze0' surround' 
            end
          | (Construct (SOp op), ZExp.CursorE On e)
          | (Construct (SOp op), ZExp.CursorE After e) => 
            let e' := UHExp.bidelimit e in 
            let prefix := OperatorSeq.ExpPrefix e' op in 
            let surround := OperatorSeq.EmptySuffix prefix in 
            let (e0, u_gen') := UHExp.new_EmptyHole u_gen in 
            let ze0' := ZExp.CursorE Before e0 in
            make_and_syn_OpSeqZ fuel ctx u_gen' ze0' surround 
          | (Construct (SOp op), ZExp.CursorE Before e) => 
            let e' := UHExp.bidelimit e in 
            let suffix := OperatorSeq.ExpSuffix op e' in 
            let surround := OperatorSeq.EmptyPrefix suffix in 
            let (e0, u_gen') := UHExp.new_EmptyHole u_gen in 
            let ze0' := ZExp.CursorE Before e0 in
            make_and_syn_OpSeqZ fuel ctx u_gen' ze0' surround 
          (* Zipper Cases *)
          | (_, ZExp.ParenthesizedZ ze1) => 
            match performSyn fuel ctx a (ze1, ty, u_gen) with 
            | Some (ze1', ty', u_gen') => 
              Some (
                ZExp.ParenthesizedZ ze1',
                ty',
                u_gen')
            | None => None
            end
          | (_, ZExp.Deeper _ (ZExp.AscZ1 ze uty1)) =>
            let ty1 := UHTyp.expand fuel uty1 in 
            match performAna fuel u_gen ctx a ze ty1 with 
            | Some (ze', u_gen') => 
              let ze'' := ZExp.bidelimit ze' in 
              Some (
                ZExp.Deeper NotInHole (ZExp.AscZ1 ze'' uty1), 
                ty, 
                u_gen')
            | None => None
            end
          | (_, ZExp.Deeper _ (ZExp.AscZ2 e zty)) =>
            match performTyp fuel a zty with 
            | Some zty' => 
              let uty' := ZTyp.erase zty' in
              let ty' := UHTyp.expand fuel uty' in 
              match UHExp.ana_fix_holes fuel ctx u_gen e ty' with 
              | None => None
              | Some (e', u_gen') => 
                Some (
                  ZExp.Deeper NotInHole (ZExp.AscZ2 e' zty'), 
                  ty', 
                  u_gen')
              end
            | None => 
              None
            end
          | (_, ZExp.Deeper _ (ZExp.LetZ1 x ze1 e2)) =>
            let e1 := ZExp.erase ze1 in
            match UHExp.syn fuel ctx e1 with 
            | Some ty1 => 
              match performSyn fuel ctx a (ze1, ty1, u_gen) with
              | Some (ze1', ty1', u_gen') => 
                let ctx' := Ctx.extend ctx (x, ty1') in
                match UHExp.syn_fix_holes fuel ctx' u_gen' e2 with 
                | Some (e2', ty2', u_gen'') => 
                  Some (
                    ZExp.Deeper NotInHole (ZExp.LetZ1 x ze1' e2'), 
                    ty2', 
                    u_gen'')
                | None => None
                end
              | None => None
              end
            | None => None
            end
          | (_, ZExp.Deeper _ (ZExp.LetZ2 x e1 ze2)) =>
            match UHExp.syn fuel ctx e1 with 
            | Some ty1 => 
              let ctx' := Ctx.extend ctx (x, ty1) in
              match performSyn fuel ctx' a (ze2, ty, u_gen) with 
              | Some (ze2', ty2', u_gen') => 
                Some (
                  ZExp.Deeper NotInHole (ZExp.LetZ2 x e1 ze2'), 
                  ty2', u_gen')
              | None => None
              end
            | None => None
            end
          | (_, ZExp.Deeper _ (ZExp.LamZ x ze1)) => 
            match HTyp.matched_arrow ty with 
            | None => None
            | Some (ty1, ty2) => 
              let ctx' := Ctx.extend ctx (x, ty1) in 
              match performSyn fuel ctx' a (ze1, ty2, u_gen) with 
              | None => None
              | Some (ze1', ty2', u_gen') => 
                Some (
                  (ZExp.Deeper NotInHole (ZExp.LamZ x ze1')),
                  HTyp.Arrow ty1 ty2',
                  u_gen')
              end
            end
          | (_, ZExp.Deeper _ (ZExp.InjZ side ze1)) => 
            match ty with 
            | HTyp.Sum ty1 ty2 => 
              let ty_side := UHExp.pick_side side ty1 ty2 in 
              match performSyn fuel ctx a (ze1, ty_side, u_gen) with 
              | None => None
              | Some (ze1', ty_side', u_gen') => 
                let ty' := 
                  match side with 
                  | UHExp.L => HTyp.Sum ty_side' ty2
                  | UHExp.R => HTyp.Sum ty1 ty_side'
                  end in 
                Some (
                  ZExp.Deeper NotInHole (ZExp.InjZ side ze1'),
                  ty',
                  u_gen')
              end
            | _ => None (* should never happen *)
            end
          | (_, ZExp.Deeper _ (ZExp.OpSeqZ skel ze0 surround)) => 
            let i := OperatorSeq.surround_prefix_length surround in 
            match ZExp.erase ze with 
            | UHExp.Tm _ (UHExp.OpSeq skel seq) => 
              match UHExp.syn_skel fuel ctx skel seq (Some i) with 
              | Some (ty, Some mode) =>
                  match mode with 
                  | UHExp.AnalyzedAgainst ty0 => 
                    match performAna fuel u_gen ctx a ze0 ty0 with 
                    | Some (ze0', u_gen') => 
                      let ze0'' := ZExp.bidelimit ze0' in  
                      Some (
                        ZExp.Deeper NotInHole (ZExp.OpSeqZ skel ze0'' surround), 
                        ty, u_gen')
                    | None => None
                    end
                  | UHExp.Synthesized ty0 =>
                    match performSyn fuel ctx a (ze0, ty0, u_gen) with 
                    | Some (ze0', ty0', u_gen') => 
                      let ze0'' := ZExp.bidelimit ze0' in 
                      match make_and_syn_OpSeqZ fuel ctx u_gen' ze0'' surround with
                      | Some (ze', ty', u_gen'') => 
                        Some (ze', ty', u_gen')
                      | None => None
                      end
                    | None => None
                    end
                  end
              | Some _ => None (* should never happen *)
              | None => None (* should never happen *)
              end
            | _ => None (* should never happen *)
            end
          | _ => None
          end
        end
      end
    with performAna 
      (fuel: Fuel.t) 
      (u_gen : MetaVar.gen) 
      (ctx: Ctx.t) 
      (a: t) 
      (ze: ZExp.t) 
      (ty: HTyp.t)
      : option (ZExp.t * MetaVar.gen) :=
      match fuel with
      | Fuel.Kicked => None
      | Fuel.More fuel =>
      match (a, ze) with
      (* Movement *)
      | (MoveTo path, _) => 
        let e := ZExp.erase ze in
        match Path.follow_e fuel path e with
        | Some ze' => Some (ze', u_gen)
        | None => None
        end
      (* Backspace & Delete *)
      | (Backspace, ZExp.CursorE After e) => 
        match e with 
        | UHExp.Tm _ (UHExp.EmptyHole _) => 
          Some (ZExp.CursorE Before e, u_gen)
        | _ => 
          let (e', u_gen') := UHExp.new_EmptyHole u_gen in 
          Some (ZExp.CursorE Before e', u_gen')
        end
      | (Delete, ZExp.CursorE Before e) => 
        match e with 
        | UHExp.Tm _ (UHExp.EmptyHole _) => 
          Some (ZExp.CursorE After e, u_gen)
        | _ => 
          let (e', u_gen') := UHExp.new_EmptyHole u_gen in 
          Some (ZExp.CursorE Before e', u_gen)
        end
      | (Backspace, ZExp.CursorE On e)
      | (Delete, ZExp.CursorE On e) => 
        let (e', u_gen') := UHExp.new_EmptyHole u_gen in 
        let ze' := ZExp.CursorE Before e' in 
        Some (ze', u_gen')
      | (Backspace, 
          ZExp.Deeper _ (ZExp.AscZ2 e1 
            (ZTyp.CursorT Before uty1))) => 
        let ze' := ZExp.CursorE After e1 in 
        zexp_ana_fix_holes fuel ctx u_gen ze' ty
      | (Backspace,
          ZExp.Deeper _ (ZExp.AscZ2 e1 
            (ZTyp.OpSeqZ _ 
              (ZTyp.CursorT Before _)
              (OperatorSeq.EmptyPrefix _)))) => 
        let ze' := ZExp.CursorE After e1 in 
        zexp_ana_fix_holes fuel ctx u_gen ze' ty
      (* special cases for backspace/delete that can turn 
       * an opseq back into a single expression *)
      | (Backspace, 
          ZExp.Deeper _ (ZExp.OpSeqZ _
            ((ZExp.CursorE Before _) as ze0)
            (OperatorSeq.EmptySuffix
              (OperatorSeq.ExpPrefix e1 UHExp.Space)))) => 
        let ze0' := combine_for_Backspace_Space e1 ze0 in 
        zexp_ana_fix_holes fuel ctx u_gen ze0' ty
      | (Backspace, 
          ZExp.Deeper _ (ZExp.OpSeqZ _
            ((ZExp.CursorE Before 
              (UHExp.Tm _ (UHExp.EmptyHole _))) as ze0)
            (OperatorSeq.EmptySuffix 
              (OperatorSeq.ExpPrefix 
                ((UHExp.Tm _ (UHExp.EmptyHole _)) as e1) _)))) => 
        let ze1 := ZExp.CursorE After e1 in 
        Some (ze1, u_gen)
      | (Backspace, 
          ZExp.Deeper _ (ZExp.OpSeqZ _
            ((ZExp.CursorE Before _) as ze0)
            (OperatorSeq.EmptySuffix 
              (OperatorSeq.ExpPrefix 
                (UHExp.Tm _ (UHExp.EmptyHole _)) _)))) => 
        zexp_ana_fix_holes fuel ctx u_gen ze0 ty
      | (Backspace, 
          ZExp.Deeper _ (ZExp.OpSeqZ _
            ((ZExp.CursorE Before 
              (UHExp.Tm _ (UHExp.EmptyHole _))) as ze0)
            (OperatorSeq.EmptySuffix 
              (OperatorSeq.ExpPrefix e1 _)))) => 
        let ze1 := ZExp.CursorE After e1 in 
        zexp_ana_fix_holes fuel ctx u_gen ze1 ty
      | (Delete, 
          ZExp.Deeper _
            (ZExp.OpSeqZ _
              ((ZExp.CursorE After _) as ze0)
              (OperatorSeq.EmptyPrefix
                (OperatorSeq.ExpSuffix UHExp.Space e1)))) => 
        let ze0' := combine_for_Delete_Space ze0 e1 in 
        zexp_ana_fix_holes fuel ctx u_gen ze0' ty
      | (Delete, 
          ZExp.Deeper _ (ZExp.OpSeqZ _
            ((ZExp.CursorE After 
              (UHExp.Tm _ (UHExp.EmptyHole _))) as ze0)
            (OperatorSeq.EmptyPrefix 
              (OperatorSeq.ExpSuffix _ 
                ((UHExp.Tm _ (UHExp.EmptyHole _)) as e1))))) => 
        Some (ze0, u_gen)
      | (Delete, 
          ZExp.Deeper _ (ZExp.OpSeqZ _
            ((ZExp.CursorE After 
              (UHExp.Tm _ (UHExp.EmptyHole _))) as ze0)
            (OperatorSeq.EmptyPrefix 
              (OperatorSeq.ExpSuffix _ e1)))) => 
        let ze1 := ZExp.CursorE Before e1 in 
        zexp_ana_fix_holes fuel ctx u_gen ze1 ty
      | (Delete, 
          ZExp.Deeper _ (ZExp.OpSeqZ _
            ((ZExp.CursorE After _) as ze0)
            (OperatorSeq.EmptyPrefix 
              (OperatorSeq.ExpSuffix _ 
                (UHExp.Tm _ (UHExp.EmptyHole _)))))) => 
        zexp_ana_fix_holes fuel ctx u_gen ze0 ty
      (* Construction *)
      | (Construct SParenthesized, ZExp.CursorE _ e) => 
        Some (
          ZExp.ParenthesizedZ ze, 
          u_gen)
      | (Construct (SLet x), 
          ZExp.CursorE _ 
            (UHExp.Tm _ (UHExp.EmptyHole u))) =>
        let (new_hole, u_gen') := UHExp.new_EmptyHole u_gen in 
        Some (
          ZExp.Deeper NotInHole
            (ZExp.LetZ1 x ze new_hole), 
          u_gen')
      | (Construct (SLam x), 
          ZExp.CursorE _ 
            (UHExp.Tm _ (UHExp.EmptyHole u))) =>
        match HTyp.matched_arrow ty with
        | Some _ => 
          Some (
            ZExp.Deeper NotInHole (ZExp.LamZ x ze), 
            u_gen)
        | None => 
            Some (
              ZExp.Deeper (InHole u) 
                (ZExp.LamZ x ze)
            , u_gen)
        end
      | (Construct (SInj side), 
          ZExp.CursorE _ (UHExp.Tm _ (UHExp.EmptyHole u))) =>
        match HTyp.matched_sum ty with
        | Some _ => Some (
          ZExp.Deeper NotInHole 
            (ZExp.InjZ side ze), 
          u_gen)
        | None => 
          Some (
            ZExp.Deeper (InHole u) 
              (ZExp.InjZ side ze),
            u_gen)
        end
      | (Construct (SCase x y), 
          ZExp.CursorE _ (UHExp.Tm _ (UHExp.EmptyHole u))) (* 23c *) =>
        let (new_hole1, u_gen') := UHExp.new_EmptyHole u_gen in 
        let (new_hole2, u_gen'') := UHExp.new_EmptyHole u_gen' in 
        Some (
          ZExp.Deeper NotInHole (
            ZExp.CaseZ1
              ze 
              (x, new_hole1)
              (y, new_hole2)),
          u_gen''
        )
      (* Zipper Cases *)
      | (_, ZExp.ParenthesizedZ ze1) => 
        match performAna fuel u_gen ctx a ze1 ty with 
        | Some (ze1', u_gen') => 
          Some (
            ZExp.ParenthesizedZ ze1',
            u_gen')
        | None => None
        end
      | (_, ZExp.Deeper _ (ZExp.LetZ1 x ze1 e2)) =>
        match UHExp.syn fuel ctx (ZExp.erase ze1) with 
        | Some ty1 => 
          match performSyn fuel ctx a (ze1, ty1, u_gen) with 
          | Some (ze1', ty1', u_gen') => 
            let ctx' := Ctx.extend ctx (x, ty1') in
            match UHExp.ana_fix_holes fuel ctx' u_gen' e2 ty with 
            | Some (e2', u_gen'') => 
              Some (ZExp.Deeper (NotInHole) (
                ZExp.LetZ1 x ze1' e2'), u_gen'')
            | None => None
            end
          | None => None
          end
        | None => None
        end
      | (_, ZExp.Deeper _ (ZExp.LetZ2 x e1 ze2)) =>
        match UHExp.syn fuel ctx e1 with 
        | Some ty1 => 
          let ctx' := Ctx.extend ctx (x, ty1) in
          match performAna fuel u_gen ctx' a ze2 ty with
          | Some (ze2', u_gen') => 
            Some (ZExp.Deeper (NotInHole) (
              ZExp.LetZ2 x e1 ze2'), u_gen')
          | None => None
          end
       | None => None
        end
      | (_, ZExp.Deeper _ (ZExp.LamZ x ze')) =>
        match HTyp.matched_arrow ty with 
        | Some (ty1, ty2) => 
          let ctx' := Ctx.extend ctx (x, ty1) in
          match performAna fuel u_gen ctx' a ze' ty2 with 
          | Some (ze'', u_gen') => Some (
              ZExp.Deeper (NotInHole) (ZExp.LamZ x ze''), 
              u_gen')
          | None => None
          end
        | None => None
        end
      | (_, ZExp.Deeper _ (ZExp.InjZ side ze)) =>
        match HTyp.matched_sum ty with 
        | Some (ty1, ty2) => 
          let picked := UHExp.pick_side side ty1 ty2 in 
          match performAna fuel u_gen ctx a ze picked with 
          | Some (ze', u_gen') => Some (
              ZExp.Deeper (NotInHole) (
                ZExp.InjZ side ze'), u_gen')
          | None => None
          end
        | None => None
        end
      | (_, ZExp.Deeper _ (ZExp.CaseZ1 ze0 (x, e1) (y, e2))) =>
        match UHExp.syn fuel ctx (ZExp.erase ze0) with 
        | None => None
        | Some ty0 => 
          match performSyn fuel ctx a (ze0, ty0, u_gen) with 
          | None => None
          | Some (ze0', ty0', u_gen') => 
            let ze' := ZExp.Deeper NotInHole (
              ZExp.CaseZ1 ze0' (x, e1) (y, e2)) in 
            zexp_ana_fix_holes fuel ctx u_gen' ze' ty 
          end
        end
      | (_, ZExp.Deeper err_state (ZExp.CaseZ2 e0 (x, ze1) (y, e2))) =>
        match UHExp.syn fuel ctx e0 with 
        | Some ty0 => 
          match HTyp.matched_sum ty0 with 
          | Some (ty1, ty2) => 
            let ctx1 := Ctx.extend ctx (x, ty1) in
            match performAna fuel u_gen ctx1 a ze1 ty with 
            | Some (ze1', u_gen') => 
              Some (
                ZExp.Deeper 
                  err_state 
                  (ZExp.CaseZ2 e0 (x, ze1') (y, e2)), 
                u_gen')
            | None => None
            end
          | None => None
          end
        | None => None
        end
      | (_, ZExp.Deeper err_state (ZExp.CaseZ3 e0 (x, e1) (y, ze2))) =>
        match UHExp.syn fuel ctx e0 with 
        | Some ty0 => 
          match HTyp.matched_sum ty0 with
          | Some (ty1, ty2) => 
            let ctx2 := Ctx.extend ctx (y, ty2) in
            match performAna fuel u_gen ctx2 a ze2 ty with 
            | Some (ze2', u_gen') => 
              Some (
                ZExp.Deeper err_state 
                  (ZExp.CaseZ3 e0 (x, e1) (y, ze2')), 
                u_gen')
            | None => None
            end
          | None => None
          end
        | None => None
        end
      (* Subsumption *)
      | _ =>
        performAna_subsume fuel u_gen ctx a ze ty
      end
      end
    with performAna_subsume 
      (fuel : Fuel.t) 
      (u_gen : MetaVar.gen) 
      (ctx : Ctx.t) 
      (a : t) 
      (ze : ZExp.t) 
      (ty : HTyp.t)
      : option (ZExp.t * MetaVar.gen) :=
      match fuel with
      | Fuel.Kicked => None
      | Fuel.More fuel =>
        match UHExp.syn fuel ctx (ZExp.erase ze) with 
        | Some ty1 => 
          match performSyn fuel ctx a (ze, ty1, u_gen) with 
          | Some (ze', ty1', u_gen') => 
            if HTyp.consistent ty ty1' then 
              Some (ze', u_gen') 
            else 
              let (ze'', u_gen'') := ZExp.put_in_new_hole u_gen' ze' in 
              Some (ze'', u_gen'')
          | None => None
          end
        | None => None
        end
      end.
  End FAction.

  Module Dynamics.
      Module Environment.
          (* polymorphic to avoid having to do mutual definition with UHExp.t *)
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
        Inductive bin_num_op : Type := 
        | Plus : bin_num_op
        | Times : bin_num_op.

        Definition of_op op := 
          match op with 
          | UHExp.Plus => Some Plus
          | UHExp.Times => Some Times
          | _ => None
          end.

        Inductive t : Type := 
        | Var : Var.t -> t
        | Let : Var.t -> t -> t -> t
        | Lam : Var.t -> HTyp.t -> t -> t
        | Ap  : t -> t -> t
        | NumLit : nat -> t
        | BinNumOp : bin_num_op -> t -> t -> t
        | Inj : HTyp.t -> UHExp.inj_side -> t -> t
        | Case : t -> (Var.t * t) -> (Var.t * t) -> t
        | EmptyHole : MetaVar.t -> Environment.t(t) -> t 
        | NonEmptyHole : MetaVar.t -> Environment.t(t) -> t -> t
        | Cast : t -> HTyp.t -> HTyp.t -> t
        | FailedCast : t -> HTyp.t -> HTyp.t -> t.

        (* closed substitution [d1/x]d2*)
        Fixpoint subst (fuel : Fuel.t) (d1 : t) (x : Var.t) (d2 : t) : t := 
          match fuel with 
          | Fuel.Kicked => d2
          | Fuel.More fuel => let subst := subst fuel in 
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
            | BinNumOp op d3 d4 => 
              let d3' := subst d1 x d3 in 
              let d4' := subst d1 x d4 in 
              BinNumOp op d3' d4'
            | Inj ty side d3 => 
              let d3' := subst d1 x d3 in 
              Inj ty side d3' 
            | Case d3 (y4, d4) (y5, d5) => 
              let d3' := subst d1 x d3 in 
              let d4' := if Var.equal x y4 then d4 else subst d1 x d4 in 
              let d5' := if Var.equal x y5 then d5 else subst d1 x d5 in 
              Case d3' (y4, d4') (y5, d5')
            | EmptyHole u sigma => 
              let sigma' := env_subst fuel d1 x sigma in 
              EmptyHole u sigma' 
            | NonEmptyHole u sigma d3 => 
              let d3' := subst d1 x d3 in 
              let sigma' := env_subst fuel d1 x sigma in 
              NonEmptyHole u sigma' d3'
            | Cast d ty1 ty2 => 
              let d' := subst d1 x d in 
              Cast d' ty1 ty2 
            | FailedCast d ty1 ty2 => 
              let d' := subst d1 x d in 
              FailedCast d' ty1 ty2
            end
          end
        with env_subst (fuel : Fuel.t) (d1 : t) (x : Var.t) (sigma : Environment.t(t)) := 
          match fuel with 
          | Fuel.Kicked => sigma
          | Fuel.More fuel => 
            Coq.Lists.List.map 
              (fun xd : Var.t * t => 
                let (y, d) := xd in
                (y, subst fuel d1 x d)) 
              sigma
          end.

        Inductive type_result : Type := 
        | WellTyped : HTyp.t -> type_result
        | IllTyped.

        Fixpoint assign_type 
          (fuel : Fuel.t) 
          (gamma : Ctx.t) (delta : MetaVarCtx.t) 
          (d : t) 
          : type_result := 
            match fuel with 
            | Fuel.Kicked => IllTyped
            | Fuel.More fuel' => 
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
              | IllTyped => IllTyped
              | WellTyped (HTyp.Arrow ty2 ty) => 
                match assign_type gamma delta d2 with 
                | IllTyped => IllTyped
                | WellTyped ty2' => 
                  if HTyp.eq ty2 ty2' then WellTyped ty
                  else IllTyped
                end
              | WellTyped _ => IllTyped
              end
            | NumLit _ => WellTyped HTyp.Num
            | BinNumOp _ d1 d2 => 
              match (assign_type gamma delta d1, 
                     assign_type gamma delta d2) with 
              | (WellTyped HTyp.Num, WellTyped HTyp.Num) => 
                WellTyped HTyp.Num
              | _ => IllTyped
              end
            | Inj other_ty side d1 => 
              match assign_type gamma delta d1 with
              | IllTyped => IllTyped
              | WellTyped ty1 => 
                match side with 
                | UHExp.L => WellTyped (HTyp.Sum ty1 other_ty)
                | UHExp.R => WellTyped (HTyp.Sum other_ty ty1)
                end
              end
            | Case d1 (x, d2) (y, d3) => 
              match assign_type gamma delta d1 with 
              | IllTyped => IllTyped
              | WellTyped (HTyp.Sum ty11 ty12) => 
                let gamma1 := Ctx.extend gamma (x, ty11) in 
                let gamma2 := Ctx.extend gamma (y, ty12) in 
                match (assign_type gamma1 delta d2,
                       assign_type gamma2 delta d3) with 
                | (WellTyped ty2, WellTyped ty3) => 
                  if HTyp.eq ty2 ty3 then WellTyped ty2
                  else IllTyped
                | _ => IllTyped
                end
              | WellTyped _ => IllTyped
              end
            | EmptyHole u sigma => 
              match MetaVarCtx.lookup delta u with 
              | Some (ty, gamma') => 
                if check_type_env fuel' gamma delta sigma gamma' then 
                  WellTyped ty
                else IllTyped
              | None => IllTyped
              end
            | NonEmptyHole u sigma d1 => 
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
            | Cast d1 ty1 ty2 
            | FailedCast d1 ty1 ty2 => 
              match assign_type gamma delta d1 with 
              | IllTyped => IllTyped
              | WellTyped ty1' => 
                if HTyp.eq ty1 ty1' && 
                 HTyp.consistent ty1 ty2
                then WellTyped ty2
                else IllTyped
              end
            end
            end
        with check_type_env (fuel : Fuel.t)
                (gamma : Ctx.t) (delta : MetaVarCtx.t) 
                (sigma : Environment.t(t)) 
                (gamma' : Ctx.t) : bool := 
            match fuel with 
            | Fuel.More fuel' => 
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
            | Fuel.Kicked => false
            end.
        
        Inductive expand_result : Type := 
        | Expands : t -> HTyp.t -> MetaVarCtx.t -> expand_result
        | DoesNotExpand.

        Definition id_env (gamma : Ctx.t) : Environment.t(t) := 
          Ctx.map
            (fun xt : Var.t * HTyp.t => 
              let (x, t) := xt in 
              (x, DHExp.Var x)) 
            gamma.

        Fixpoint syn_expand 
          (fuel : Fuel.t) 
          (gamma : Ctx.t) 
          (e : UHExp.t) 
          : expand_result := 
            match fuel with 
            | Fuel.Kicked => DoesNotExpand
            | Fuel.More fuel => 
            match e with 
            | UHExp.Tm (NotInHole) e' => syn_expand' fuel gamma e'
            | UHExp.Tm (InHole u) e' => 
              match syn_expand' fuel gamma e' with 
              | Expands d _ delta => 
                let sigma := id_env gamma in 
                let delta' := MetaVarCtx.extend delta (u, HTyp.Hole, gamma) in 
                Expands 
                  (NonEmptyHole u sigma d)
                  (HTyp.Hole)
                  (delta')
              | DoesNotExpand => DoesNotExpand
              end
            | UHExp.Parenthesized e1 => syn_expand fuel gamma e1
            end
            end
        with syn_expand'
          (fuel : Fuel.t)
          (gamma : Ctx.t)
          (e : UHExp.t')
          : expand_result := 
            match fuel with 
            | Fuel.Kicked => DoesNotExpand
            | Fuel.More fuel => 
            match e with 
            | UHExp.Asc e1 uty => 
              let ty := UHTyp.expand fuel uty in 
              match ana_expand fuel gamma e1 ty with 
              | DoesNotExpand => DoesNotExpand
              | Expands d1 ty' delta => 
                Expands 
                  (Cast d1 ty' ty)
                  ty 
                  delta
              end
            | UHExp.Var x => 
              match Ctx.lookup gamma x with 
              | Some ty => Expands (DHExp.Var x) ty MetaVarCtx.empty
              | None => DoesNotExpand
              end
            | UHExp.Lam x e1 => 
              let gamma' := Ctx.extend gamma (x, HTyp.Hole) in 
              match syn_expand fuel gamma' e1 with 
              | DoesNotExpand => DoesNotExpand
              | Expands d1 ty2 delta1 => 
                let d := Lam x HTyp.Hole d1 in 
                Expands d (HTyp.Arrow HTyp.Hole ty2) delta1
              end
            | UHExp.Let x e1 e2 => 
              match syn_expand fuel gamma e1 with 
              | DoesNotExpand => DoesNotExpand
              | Expands d1 ty1 delta1 => 
                let gamma' := Ctx.extend gamma (x, ty1) in 
                match syn_expand fuel gamma' e2 with 
                | DoesNotExpand => DoesNotExpand
                | Expands d2 ty delta2 => 
                  let d := Let x d1 d2 in 
                  let delta12 := MetaVarCtx.union delta1 delta2 in 
                  Expands d ty delta12 
                end
              end
            | UHExp.NumLit n => 
              Expands (NumLit n) HTyp.Num MetaVarCtx.empty 
            | UHExp.EmptyHole u => 
              let sigma := id_env gamma in 
              let d := DHExp.EmptyHole u sigma in 
              let ty := HTyp.Hole in 
              let delta := MetaVarCtx.extend MetaVarCtx.empty 
                           (u, ty, gamma) in 
              Expands d ty delta
            | UHExp.OpSeq skel seq => 
              syn_expand_skel fuel gamma skel seq
            | UHExp.Inj side e1 => 
              match syn_expand fuel gamma e1 with 
              | DoesNotExpand => DoesNotExpand
              | Expands d1 ty1 delta1 => 
                let d := DHExp.Inj HTyp.Hole side d1 in 
                let ty := 
                  match side with 
                  | UHExp.L => HTyp.Sum ty1 HTyp.Hole
                  | UHExp.R => HTyp.Sum HTyp.Hole ty1
                  end in 
                Expands d ty delta1
              end
            | UHExp.Case _ _ _ => DoesNotExpand
            end
            end
        with syn_expand_skel 
          (fuel : Fuel.t)
          (gamma : Ctx.t)
          (skel : UHExp.skel_t)
          (seq : UHExp.opseq)
          : expand_result := 
            match fuel with 
            | Fuel.Kicked => DoesNotExpand
            | Fuel.More fuel => 
            match skel with 
            | Skel.Placeholder _ n => 
              match OperatorSeq.seq_nth n seq with 
              | None => DoesNotExpand
              | Some en => syn_expand fuel gamma en
              end
            | Skel.BinOp (InHole u) op skel1 skel2 => 
              let skel_not_in_hole := Skel.BinOp NotInHole op skel1 skel2 in 
              match syn_expand_skel fuel gamma skel_not_in_hole seq with 
              | DoesNotExpand => DoesNotExpand
              | Expands d _ delta => 
                let sigma := id_env gamma in 
                let delta' := MetaVarCtx.extend delta (u, HTyp.Hole, gamma) in 
                Expands
                  (NonEmptyHole u sigma d)
                  HTyp.Hole delta'
              end
            | Skel.BinOp NotInHole UHExp.Space skel1 skel2 => 
              match UHExp.syn_skel fuel gamma skel1 seq None with 
              | None => DoesNotExpand
              | Some (ty1, _) => 
                match HTyp.matched_arrow ty1 with 
                | None => DoesNotExpand
                | Some (ty2, ty) => 
                  let ty2_arrow_ty := HTyp.Arrow ty2 ty in 
                  match ana_expand_skel fuel gamma skel1 seq ty2_arrow_ty with 
                  | DoesNotExpand => DoesNotExpand
                  | Expands d1 ty1' delta1 => 
                    match ana_expand_skel fuel gamma skel2 seq ty2 with 
                    | DoesNotExpand => DoesNotExpand
                    | Expands d2 ty2' delta2 => 
                      let delta := MetaVarCtx.union delta1 delta2 in 
                      let dc1 := Cast d1 ty1' ty2_arrow_ty in 
                      let dc2 := Cast d2 ty2' ty2 in 
                      let d := Ap dc1 dc2 in 
                      Expands d ty delta
                    end
                  end
                end
              end
            | Skel.BinOp NotInHole (UHExp.Plus as op) skel1 skel2
            | Skel.BinOp NotInHole (UHExp.Times as op) skel1 skel2 => 
              match (ana_expand_skel fuel gamma skel1 seq HTyp.Num,
                     ana_expand_skel fuel gamma skel2 seq HTyp.Num) with 
              | (Expands d1 ty1 delta1, Expands d2 ty2 delta2) => 
                let delta := MetaVarCtx.union delta1 delta2 in 
                let dc1 := Cast d1 ty1 HTyp.Num in 
                let dc2 := Cast d2 ty2 HTyp.Num in 
                match of_op op with 
                | None => DoesNotExpand (* should not happen due to pattern matching above *)
                | Some op' => 
                  let d := BinNumOp op' dc1 dc2 in 
                  Expands d HTyp.Num delta
                end
              | _ => DoesNotExpand
              end
            end
            end
        with ana_expand 
          (fuel : Fuel.t) 
          (gamma : Ctx.t) 
          (e : UHExp.t)
          (ty : HTyp.t) 
          : expand_result := 
            match fuel with 
            | Fuel.Kicked => DoesNotExpand
            | Fuel.More fuel => 
            match e with 
            | UHExp.Tm (InHole u) e' =>
              match syn_expand' fuel gamma e' with 
              | DoesNotExpand => DoesNotExpand
              | Expands d _ delta => 
                let sigma := id_env gamma in 
                let delta' := MetaVarCtx.extend delta (u, ty, gamma) in 
                Expands 
                  (NonEmptyHole u sigma d)
                  ty
                  delta'
              end
            | UHExp.Tm NotInHole e' => ana_expand' fuel gamma e' ty
            | UHExp.Parenthesized e1 => ana_expand fuel gamma e1 ty
            end
            end
        with ana_expand'
          (fuel : Fuel.t)
          (gamma : Ctx.t)
          (e : UHExp.t')
          (ty : HTyp.t) 
          : expand_result := 
            match fuel with 
            | Fuel.Kicked => DoesNotExpand
            | Fuel.More fuel => 
            match e with 
            | UHExp.Lam x e1 => 
              match HTyp.matched_arrow ty with 
              | None => DoesNotExpand
              | Some (ty1, ty2) => 
                let gamma' := Ctx.extend gamma (x, ty1) in 
                match ana_expand fuel gamma' e1 ty2 with 
                | DoesNotExpand => DoesNotExpand
                | Expands d1 ty2' delta => 
                  let ty := HTyp.Arrow ty1 ty2' in 
                  let d := Lam x ty1 d1 in 
                  Expands d ty delta
                end
              end
            | UHExp.Inj side e1 => 
              match HTyp.matched_sum ty with 
              | None => DoesNotExpand
              | Some (ty1, ty2) => 
                let e1ty := UHExp.pick_side side ty1 ty2 in 
                match ana_expand fuel gamma e1 e1ty with 
                | DoesNotExpand => DoesNotExpand
                | Expands d1 e1ty' delta => 
                  let (ann_ty, ty) := 
                    match side with 
                    | UHExp.L => (ty2, HTyp.Sum e1ty' ty2) 
                    | UHExp.R => (ty1, HTyp.Sum ty1 e1ty')
                    end in 
                  let d := Inj ann_ty side d1 in 
                  Expands d ty delta
                end
              end
            | UHExp.Case e1 (x, e2) (y, e3) => 
              match syn_expand fuel gamma e1 with 
              | DoesNotExpand => DoesNotExpand
              | Expands d1 ty1 delta1 => 
                match HTyp.matched_sum ty1 with 
                | None => DoesNotExpand
                | Some (ty1L, ty1R) => 
                  let gammaL := Ctx.extend gamma (x, ty1L) in 
                  let gammaR := Ctx.extend gamma (y, ty1R) in 
                  match (ana_expand fuel gammaL e2 ty,
                         ana_expand fuel gammaR e3 ty) with 
                  | (Expands d2 ty2 delta2, Expands d3 ty3 delta3) => 
                    match HTyp.join ty2 ty3 with
                    | None => DoesNotExpand
                    | Some ty' => 
                      let d := Case  
                        (Cast d1 ty1 (HTyp.Sum ty1L ty1R))
                        (x, (Cast d2 ty2 ty')) 
                        (y, (Cast d3 ty3 ty')) in 
                      let delta := 
                        MetaVarCtx.union 
                          (MetaVarCtx.union delta1 delta2) delta3 in 
                      Expands d ty delta
                    end
                  | _ => DoesNotExpand
                  end
                end
              end
            | UHExp.EmptyHole u => 
              let sigma := id_env gamma in 
              let d := EmptyHole u sigma in 
              let delta := MetaVarCtx.extend MetaVarCtx.empty (u, ty, gamma) in 
              Expands d ty delta
            | UHExp.Asc _ _ 
            | UHExp.Var _ 
            | UHExp.Let _ _ _ 
            | UHExp.NumLit _
            | UHExp.OpSeq _ _ => 
              (* subsumption *)
              match syn_expand' fuel gamma e with 
              | DoesNotExpand => DoesNotExpand
              | (Expands d ty' delta) as result => 
                if HTyp.consistent ty ty'  
                then result
                else DoesNotExpand
              end
            end
            end
        with ana_expand_skel
          (fuel : Fuel.t)
          (gamma : Ctx.t)
          (skel : UHExp.skel_t)
          (seq : UHExp.opseq)
          (ty : HTyp.t)
          : expand_result := 
            match fuel with 
            | Fuel.Kicked => DoesNotExpand
            | Fuel.More fuel => 
            match skel with 
            | Skel.Placeholder _ n => 
              match OperatorSeq.seq_nth n seq with 
              | None => DoesNotExpand
              | Some en => ana_expand fuel gamma en ty
              end
            | _ => 
              match syn_expand_skel fuel gamma skel seq with 
              | DoesNotExpand => DoesNotExpand
              | Expands d ty' delta => 
                if HTyp.consistent ty ty' then 
                  Expands d ty' delta
                else DoesNotExpand
              end
            end
            end.
      End DHExp.

      Module Evaluator.
          Inductive result := 
          | InvalidInput : result (* not well-typed or otherwise invalid *)
          | BoxedValue : DHExp.t -> result
          | Indet : DHExp.t -> result.

          Inductive ground_cases := 
          | Hole : ground_cases
          | Ground : ground_cases
          | NotGroundOrHole : HTyp.t -> ground_cases. (* the argument is the corresponding ground type *)

          Definition grounded_Arrow := NotGroundOrHole (HTyp.Arrow HTyp.Hole HTyp.Hole).
          Definition grounded_Sum := NotGroundOrHole (HTyp.Sum HTyp.Hole HTyp.Hole).

          Definition ground_cases_of ty := 
            match ty with 
            | HTyp.Hole => Hole
            | HTyp.Num => Ground
            | HTyp.Arrow HTyp.Hole HTyp.Hole => Ground
            | HTyp.Sum HTyp.Hole HTyp.Hole => Ground
            | HTyp.Arrow _ _ => grounded_Arrow
            | HTyp.Sum _ _ => grounded_Sum
            end.

          Definition eval_bin_num_op op n1 n2 :=
            match op with 
            | DHExp.Plus => n1 + n2
            | DHExp.Times => n1 * n2
            end.

          Fixpoint evaluate 
            (fuel : Fuel.t) 
            (d : DHExp.t) 
            : result := 
              match fuel with 
              | Fuel.Kicked => InvalidInput
              | Fuel.More(fuel') => 
              match d with 
              | DHExp.Var _ => InvalidInput
              | DHExp.Let x d1 d2 => 
                match evaluate fuel' d1 with 
                | InvalidInput => InvalidInput
                | BoxedValue d1' | Indet d1' => 
                  evaluate fuel' (DHExp.subst fuel' d1' x d2)
                end
              | DHExp.Lam _ _ _ => BoxedValue d
              | DHExp.Ap d1 d2 => 
                match evaluate fuel' d1 with 
                | InvalidInput => InvalidInput
                | BoxedValue (DHExp.Lam x tau d1') => 
                  match evaluate fuel' d2 with 
                  | InvalidInput => InvalidInput
                  | BoxedValue d2' | Indet d2' => 
                    (* beta rule *)
                    evaluate fuel' (DHExp.subst fuel d2' x d1')
                  end
                | BoxedValue (DHExp.Cast d1' (HTyp.Arrow ty1 ty2) (HTyp.Arrow ty1' ty2'))
                | Indet (DHExp.Cast d1' (HTyp.Arrow ty1 ty2) (HTyp.Arrow ty1' ty2')) => 
                  match evaluate fuel' d2 with 
                  | InvalidInput => InvalidInput
                  | BoxedValue d2' | Indet d2' => 
                    (* ap cast rule *)
                    evaluate fuel'  
                      (DHExp.Cast 
                        (DHExp.Ap 
                          d1'
                          (DHExp.Cast
                            d2' ty1' ty1))
                        ty2 ty2')
                  end
                | BoxedValue _ => InvalidInput
                | Indet d1' => 
                  match evaluate fuel' d2 with 
                  | InvalidInput => InvalidInput
                  | BoxedValue d2' | Indet d2' => 
                    Indet (DHExp.Ap d1' d2')
                  end
                end
              | DHExp.NumLit _ => BoxedValue d
              | DHExp.BinNumOp op d1 d2 => 
                match evaluate fuel' d1 with 
                | InvalidInput => InvalidInput
                | BoxedValue (DHExp.NumLit n1 as d1')  => 
                  match evaluate fuel' d2 with 
                  | InvalidInput => InvalidInput
                  | BoxedValue (DHExp.NumLit n2) => 
                    BoxedValue (DHExp.NumLit (eval_bin_num_op op n1 n2))
                  | BoxedValue _ => InvalidInput
                  | Indet d2' => 
                    Indet (DHExp.BinNumOp op d1' d2')
                  end
                | BoxedValue _ => InvalidInput
                | Indet d1' => 
                  match evaluate fuel' d2 with 
                  | InvalidInput => InvalidInput
                  | BoxedValue d2' | Indet d2' => 
                    Indet (DHExp.BinNumOp op d1' d2')
                  end
                end
              | DHExp.Inj ty side d1 => 
                match evaluate fuel' d1 with 
                | InvalidInput => InvalidInput
                | BoxedValue d1' => BoxedValue (DHExp.Inj ty side d1')
                | Indet d1' => Indet (DHExp.Inj ty side d1')
                end
              | DHExp.Case d1 (x, d2) (y, d3) =>
                match evaluate fuel' d1 with 
                | InvalidInput => InvalidInput
                | BoxedValue d1' => 
                  match d1' with 
                  | DHExp.Inj _ side d1'' => 
                    let (xb, db) := UHExp.pick_side side (x, d2) (y, d3) in
                    let branch := DHExp.subst fuel' d1'' xb db in 
                    evaluate fuel' branch
                  | DHExp.Cast d1'' (HTyp.Sum ty1 ty2) (HTyp.Sum ty1' ty2') => 
                    let d' := 
                      DHExp.Case d1'' 
                        (x, DHExp.subst fuel' (DHExp.Cast (DHExp.Var x) ty1 ty1') x d2)
                        (y, DHExp.subst fuel' (DHExp.Cast (DHExp.Var y) ty2 ty2') y d3) in 
                    evaluate fuel' d'
                  | _ => InvalidInput
                  end
                | Indet d1' => 
                  match d1' with 
                  | DHExp.Inj _ side d1'' => 
                      let (xb, db) := UHExp.pick_side side (x, d2) (y, d3) in 
                      let branch := DHExp.subst fuel' d1'' xb db in 
                      evaluate fuel' branch
                  | DHExp.Cast d1'' (HTyp.Sum ty1 ty2) (HTyp.Sum ty1' ty2') => 
                    let d' := 
                      DHExp.Case d1'' 
                        (x, DHExp.subst fuel' (DHExp.Cast (DHExp.Var x) ty1 ty1') x d2)
                        (y, DHExp.subst fuel' (DHExp.Cast (DHExp.Var y) ty2 ty2') y d3) in 
                    evaluate fuel' d'
                  | _ => Indet (DHExp.Case d1' (x, d2) (y, d3))
                  end
                end
              | DHExp.EmptyHole u sigma => 
                Indet (DHExp.EmptyHole u sigma)  
              | DHExp.NonEmptyHole u sigma d1 => 
                match evaluate fuel' d1 with 
                | InvalidInput => InvalidInput
                | BoxedValue d1' | Indet d1' => 
                  Indet (DHExp.NonEmptyHole u sigma d1')
                end
              | DHExp.Cast d1 ty ty' => 
                match evaluate fuel' d1 with 
                | InvalidInput => InvalidInput
                | (BoxedValue d1' as result) => 
                  match (ground_cases_of ty, ground_cases_of ty') with 
                  | (Hole, Hole) => result
                  | (Ground, Ground) =>  
                    (* if two types are ground and consistent, then they are equal *)
                    result
                  | (Ground, Hole) => 
                    (* can't remove the cast or do anything else here, so we're done *)
                    BoxedValue (DHExp.Cast d1' ty ty')
                  | (Hole, Ground) => 
                    (* by canonical forms, d1' must be of the form d<ty'' => ?> *)
                    match d1' with 
                    | DHExp.Cast d1'' ty'' HTyp.Hole => 
                      if HTyp.eq ty'' ty' then BoxedValue d1''
                      else Indet (DHExp.FailedCast d1' ty ty')
                    | _ => InvalidInput
                    end
                  | (Hole, NotGroundOrHole ty'_grounded) => 
                    (* ITExpand rule *)
                    let d' := 
                      DHExp.Cast
                        (DHExp.Cast d1' ty ty'_grounded)
                        ty'_grounded ty' in 
                    evaluate fuel' d'
                  | (NotGroundOrHole ty_grounded, Hole) => 
                    (* ITGround rule *)
                     let d' := 
                       DHExp.Cast
                         (DHExp.Cast d1' ty ty_grounded)
                         ty_grounded ty' in 
                     evaluate fuel' d'
                  | (Ground, NotGroundOrHole _)  
                  | (NotGroundOrHole _, Ground) => 
                    (* can't do anything when casting between disequal, non-hole types *)
                    BoxedValue (DHExp.Cast d1' ty ty')
                  | (NotGroundOrHole _, NotGroundOrHole _) => 
                    (* they might be equal in this case, so remove cast if so *)
                    if HTyp.eq ty ty' then result 
                    else BoxedValue (DHExp.Cast d1' ty ty')
                  end
                | (Indet d1' as result) => 
                  match (ground_cases_of ty, ground_cases_of ty') with 
                  | (Hole, Hole) => result
                  | (Ground, Ground) =>  
                    (* if two types are ground and consistent, then they are equal *)
                    result
                  | (Ground, Hole) => 
                    (* can't remove the cast or do anything else here, so we're done *)
                    Indet (DHExp.Cast d1' ty ty')
                  | (Hole, Ground) => 
                    match d1' with 
                    | DHExp.Cast d1'' ty'' HTyp.Hole => 
                      if HTyp.eq ty'' ty' then BoxedValue d1''
                      else Indet (DHExp.FailedCast d1' ty ty')
                    | _ => 
                      Indet (DHExp.Cast d1' ty ty')
                    end
                  | (Hole, NotGroundOrHole ty'_grounded) => 
                    (* ITExpand rule *)
                    let d' := 
                      DHExp.Cast
                        (DHExp.Cast d1' ty ty'_grounded)
                        ty'_grounded ty' in 
                    evaluate fuel' d'
                  | (NotGroundOrHole ty_grounded, Hole) => 
                    (* ITGround rule *)
                     let d' := 
                       DHExp.Cast
                         (DHExp.Cast d1' ty ty_grounded)
                         ty_grounded ty' in 
                     evaluate fuel' d'
                  | (Ground, NotGroundOrHole _)  
                  | (NotGroundOrHole _, Ground) => 
                    (* can't do anything when casting between disequal, non-hole types *)
                    Indet (DHExp.Cast d1' ty ty')
                  | (NotGroundOrHole _, NotGroundOrHole _) => 
                    (* it might be equal in this case, so remove cast if so *)
                    if HTyp.eq ty ty' then result else Indet (DHExp.Cast d1' ty ty')
                  end
                end
              | DHExp.FailedCast d1 ty ty' => 
                match evaluate fuel' d1 with 
                | InvalidInput => InvalidInput
                | BoxedValue d1' | Indet d1' => 
                  Indet (DHExp.FailedCast d1' ty ty')
                end
              end
              end.
      End Evaluator.
  End Dynamics.

End Core.

Extract Constant Core.str_eqb => "String.equal".
Extract Inductive Core.Fuel.t => "unit" [ "()" "()" ] "(fun fMore _ fKicked -> fMore ())".
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
Extract Constant plus => "(+)".
Extract Constant mult => "( * )".
Extract Constant Nat.eqb => "(=)".
Extract Constant Nat.leb => "(<=)".
Extract Constant Nat.ltb => "(<)".

Extraction Core.
