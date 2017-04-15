Require Coq.Bool.Bool. Open Scope bool.

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

  Inductive t : Set := 
    Num : t
  | Arrow : t -> t -> t
  | Sum : t -> t -> t
  | Hole.
  
  Fixpoint eq (x y : t) : bool := 
    match (x, y) with 
    | (Num, Num) => true 
    | (Arrow x' y', Arrow x'' y'') 
    | (Sum x' y', Sum x'' y'') => 
      (eq x' x'') && (eq y' y'')
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
    | (Arrow x' y', Arrow x'' y'') => 
      (consistent x' x'') && (consistent y' y'')
    | _ => false
    end.
    
  Definition inconsistent (x y : t) : bool := 
    negb (consistent x y).

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
  

End HTyp.

Extract Inductive bool => "bool" ["true" "false"].
Extract Constant negb => "not".
Extraction HTyp.

