Require Coq.Bool.Bool. Open Scope bool.
Require Coq.Strings.String. Open Scope string_scope.
Require Coq.Strings.Ascii. Open Scope char_scope.
Require Coq.Arith.PeanoNat. Open Scope nat_scope.
Require Coq.Lists.List. Open Scope list_scope.
Require Import BinInt.
(* Require Extraction. *)

Set Implicit Arguments.
Unset Elimination Schemes.

(* TODO: code cleanup
* - shadow instead of prime, esp with u_gen
* - indent 2 spaces
* - naming reconsiderations?
* - more...
*)

Module Util.

  Section ListUtil.

  Fixpoint update_nth {A : Type} (n : nat) (xs : list A) (f : A -> A) := 
    match (n, xs) with 
    | (_, nil) => nil
    | (0, cons x xs) => cons (f x) xs
    | (S n, cons x xs) => cons x (update_nth n xs f)
    end.

  Fixpoint _findmapi {A B : Type} (i : nat) (xs : list A) (f : nat -> A -> option B) : option(B) := 
    match xs with 
    | nil => None
    | cons x xs => 
      match f i x with 
      | (Some b) as result => result
      | None => 
        _findmapi (i + 1) xs f 
      end
    end.

  Definition findmapi {A B : Type} (xs : list A) (f : nat -> A -> option B) : option(B) := 
    _findmapi 0 xs f.

  End ListUtil.

  Section StringUtil.

  Definition str_eqb (s1 s2 : Coq.Strings.String.string) : bool := 
    if Coq.Strings.String.string_dec s1 s2 then true else false.

  Definition char_le_b (ch1 ch2 : Coq.Strings.Ascii.ascii) : bool :=
      Nat.leb (Coq.Strings.Ascii.nat_of_ascii ch1) (Coq.Strings.Ascii.nat_of_ascii ch2).
  Definition char_eq_b (ch1 ch2 : Coq.Strings.Ascii.ascii) : bool :=
      Nat.eqb (Coq.Strings.Ascii.nat_of_ascii ch1) (Coq.Strings.Ascii.nat_of_ascii ch2).
  Definition char_in_range_b (ch s e : Coq.Strings.Ascii.ascii) : bool :=
      (char_le_b s ch) && (char_le_b ch e).

  End StringUtil.

  (* Finite maps over nats, used in various places (e.g. MetaVarMap below) *)
  Module Type NATMAP.
    Parameter t : Type -> Type.
    Parameter empty : forall (A : Type), t A.
    Parameter extend : forall (A : Type), t A -> nat * A -> t A.
    Parameter drop : forall A : Type, t A -> nat -> option (t A * A).
    Parameter union : forall (A : Type), t A -> t A -> t A.
    Parameter lookup : forall (A : Type), t A -> nat -> option A.
    Parameter insert_or_update : forall (A : Type), t A -> nat * A -> t A.
    Parameter insert_or_map : forall (A : Type), t A -> nat -> (unit -> A) -> (A -> A) -> A * t A.
    Parameter map : forall (A B : Type), (A -> B) -> t A -> t B.
    Parameter update_with : forall (A : Type), (A -> A) -> nat -> t A -> A -> (A * t A).
    Parameter length : forall (A : Type), t A -> nat.
    Parameter to_list : forall (A : Type), t A -> list (nat * A).
    Parameter fold : forall (A B : Type), t A -> (B -> (nat * A) -> B) -> B -> B.
  End NATMAP.

  Module NatMap <: NATMAP.
    Definition t (A : Type) := list (nat * A).

    Definition empty {A : Type} : t A := nil.

    Definition extend {A : Type} (delta : t A) (x : nat * A)
      : t A := cons x delta.

    Fixpoint drop {A : Type} (delta : t A) (n : nat) : option (t A * A) :=
      match delta with 
      | nil => None
      | cons (y, a) delta' => 
        match Nat.eqb n y with 
        | true => Some (delta', a)
        | false => drop delta' n
        end
      end.

    Definition union {A : Type} (delta1 : t A) (delta2 : t A) : t A := delta1 ++ delta2.

    Fixpoint lookup {A : Type} (delta : t A) (x : nat) : option A :=
      match delta with
      | nil => None
      | cons (y, a) delta' =>
        match Nat.eqb x y with
        | true => Some a
        | false => lookup delta' x
        end
      end.
    
    Fixpoint insert_or_update {A : Type} (delta : t A) (x : nat * A) : t A := 
      let (u, a) := x in 
      match delta with 
      | nil => cons x delta
      | cons (u', a') delta' => 
        match Nat.eqb u u' with 
        | true => cons (u', a) delta'
        | false => cons (u', a') (insert_or_update delta' x)
        end
      end.

    Fixpoint insert_or_map {A : Type} (delta : t A) (u : nat) 
      (a0 : unit -> A) (f : A -> A) : A * t A := 
      match delta with 
      | nil => 
        let a0 := a0 tt in 
        (a0, cons (u, a0) delta)
      | cons (u', a) delta' => 
        if Nat.eqb u u' then 
          let a' := f a in 
          (a', cons (u', a') delta')
        else
          let (a', delta'') := insert_or_map delta' u a0 f in 
          (a', cons (u', a) delta'')
      end.

    Fixpoint map {A B : Type} (f : A -> B) (delta : t A) : t B := 
      match delta with 
      | nil => nil
      | cons (u, a) delta' => cons (u, f a) (map f delta')
      end.

    Fixpoint update_with {A : Type} (f : A -> A) (u : nat) (delta : t A) (u_nil : A) : (A * t A) := 
      match delta with 
      | nil => (u_nil, delta)
      | cons (u', a) delta' => 
        match Nat.eqb u u' with 
        | true => 
          let a' := f a in 
          (a', cons (u', a') delta')
        | false => 
          let (a', delta'') := update_with f u delta' u_nil in 
          (a', cons (u', a) delta'')
        end
      end.

    Definition length {A : Type} (delta : t A) := List.length delta.
    Definition to_list {A : Type} (delta : t A) := delta.

    Definition fold {A B : Type} (delta : t A) (f : (B -> (nat * A) -> B)) (b : B) : B := 
      List.fold_left f delta b.
  End NatMap.            
End Util.

(* Fuel is used to work around Coq's limited termination check for now.
 * During extraction, it is rewritten to unit so it has no run-time cost. *) 
Module Fuel.
  Inductive t : Type :=
  | More : t -> t
  | Kicked : t.
End Fuel.

(* Debugging aids that are implemented on the OCaml side *)
Module Type DEBUG.
  Parameter log_nat : nat -> nat.
  Parameter log_string : Coq.Strings.String.string -> Coq.Strings.String.string.
  Parameter string_of_nat : nat -> Coq.Strings.String.string.
  Parameter log_path : forall A : Type, (list(nat) * A) -> (list(nat) * A).
  Parameter log_natlist : list(nat) -> list(nat).
  Parameter log_a : forall A : Type, nat -> A -> A.
End DEBUG.

(* The Debug parameter is provided in SemanticsCore.re *)
Module FCore(Debug : DEBUG).
  Module NatMap := Util.NatMap.

  Module Var.
    Definition t := Coq.Strings.String.string.

    Definition eq (x : t) (y : t) : bool := Util.str_eqb x y.

    (* is_valid_internal s = true iff s is a string valid as the suffix of a variable *)
    Fixpoint is_valid_suffix (s : t) : bool :=
      match s with
      | Coq.Strings.String.EmptyString => true
      | Coq.Strings.String.String ch rest =>
        (
          (Util.char_eq_b ch "_") ||
          (Util.char_in_range_b ch "a" "z") ||
          (Util.char_in_range_b ch "A" "Z") ||
          (Util.char_in_range_b ch "0" "9") ||
          (Util.char_eq_b ch "'")
        ) && is_valid_suffix rest
      end.

    (* is_valid s = true iff s is a valid variable *)
    (* should be equivalent to the OCaml rules: "[_a-z][_a-zA-Z0-9']*" *)
    Definition is_valid (s : t) : bool :=
      match s with
      | Coq.Strings.String.EmptyString => false
      | Coq.Strings.String.String first_char suffix =>
        ((Util.char_eq_b first_char "_") || (Util.char_in_range_b first_char "a" "z")) &&
        is_valid_suffix suffix
      end.

    (* helper function for guarding options with is_valid *)
    Definition check_valid {A : Type}
        (s : t)
        (result : option(A))
        : option(A) :=
        if is_valid s then result else None.
  End Var.

  (* TODO turn this into Util.StringMap ala NatMap *)
  (* Module Type VARMAP.
    Parameter t : Type -> Type.
    Parameter empty : forall (a : Type), t a.
    Parameter extend : forall (a : Type), t a -> Var.t * a -> t a.
    Parameter lookup : forall (a : Type), t a -> Var.t -> option a.
    Parameter map : forall (a b : Type), (Var.t * a -> b) -> t a -> t b.
    Parameter length : forall (a : Type), t a -> nat.
    Parameter to_list : forall (a : Type), t a -> list (Var.t * a).
  End VARMAP. *)

  Module VarMap.
    Definition t_ (a : Type) := list (Var.t * a).

    Definition empty {a : Type} : t_ a := nil.

    Definition is_empty {a : Type} (ctx : t_ a) : bool := 
      match ctx with 
      | nil => true
      | _ => false
      end.

    (* Fixpoint update {a : Type} (ctx : t a) (x : Var.t) (elt : a) : option (t a) := 
      match ctx with 
      | nil => None
      | cons (y, elt') ctx' => 
        match Var.eq x y with 
        | true => Some (cons (y, elt) ctx')
        | false => 
          match update ctx' x elt with 
          | Some ctx' => Some (cons (y, elt') ctx')
          | None => None
          end
        end
      end. *)

    Fixpoint drop {a : Type} (ctx : t_ a) (x : Var.t) : t_ a := 
      match ctx with 
      | nil => ctx
      | cons (y, elt) ctx' => 
        match Var.eq x y with 
        | true => ctx'
        | false => cons (y, elt) (drop ctx' x)
        end
      end.

    Definition extend {a : Type} (ctx : t_ a) (xa : Var.t * a)
      : t_ a :=
      let (x, elt) := xa in  
      cons xa (drop ctx x).

    Definition union {a : Type} (ctx1 : t_ a) (ctx2 : t_ a) : t_ a := 
      List.fold_left extend ctx2 ctx1.

    Fixpoint lookup {a : Type} (ctx : t_ a) (x : Var.t) : option a :=
      match ctx with
      | nil => None
      | cons (y, elt) ctx' =>
        match Var.eq x y with
        | true => Some elt
        | false => lookup ctx' x
        end
      end.

     Definition map {a b : Type} (f : Var.t * a -> b) (xs : t_ a) := 
       Coq.Lists.List.map (fun (xa : Var.t * a) => 
         let (x, _) := xa in 
         (x, f xa)) xs.

     Fixpoint length {a : Type} (ctx : t_ a) : nat := 
       match ctx with 
       | nil => O
       | cons _ ctx' => S (length ctx')
       end.

     Definition to_list {a : Type} (ctx : t_ a) : list(Var.t * a) := ctx.
  End VarMap.

  (* Metavariables, a.k.a. hole names *)
  Module MetaVar.
    Definition t := nat.
    Fixpoint eq (x : t) (y : t) : bool := Nat.eqb x y.
  End MetaVar.

  (* A simple metavariable generator *)
  Module MetaVarGen.
    Definition t : Type := MetaVar.t.
    Definition init : MetaVarGen.t := 0.
    Definition next (x : t) : MetaVar.t * MetaVarGen.t := 
      let n := S(x) in (x, n).
  End MetaVarGen.

  Module MetaVarMap := Util.NatMap.

  Inductive err_status : Type := 
  | NotInHole : err_status
  | InHole : MetaVar.t -> err_status.

  Inductive var_err_status : Type := 
  | NotInVHole : var_err_status
  | InVHole : MetaVar.t -> var_err_status.

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
      | ExpOpExp _ _ _ => 2
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
      | ExpPrefix _ _ => 1
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

    Definition suffix_length {tm op : Type}
      (suffix : opseq_suffix tm op) : nat :=
      match suffix with
      | ExpSuffix _ _ => 1
      | SeqSuffix _ seq => OperatorSeq.seq_length seq
      end.

    Definition surround_suffix_length {tm op : Type}
      (surround : opseq_surround tm op)
      : nat :=
      match surround with
      | EmptySuffix _ => O
      | EmptyPrefix suffix
      | BothNonEmpty _ suffix => suffix_length suffix
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
    | Hole : t
    | Num : t
    | Bool : t
    | Arrow : t -> t -> t
    | Prod : t -> t -> t
    | Sum : t -> t -> t
    | List : t -> t.

    (* eqity *)
    Fixpoint eq (ty1 : t) (ty2 : t) : bool :=
      match (ty1, ty2) with
      | (Hole, Hole) => true
      | (Hole, _) => false
      | (Num, Num) => true
      | (Num, _) => false
      | (Bool, Bool) => true
      | (Bool, _) => false
      | (Arrow ty1 ty2, Arrow ty1' ty2') =>
        andb (eq ty1 ty1') (eq ty2 ty2')
      | (Arrow _ _, _) => false
      | (Prod ty1 ty2, Prod ty1' ty2') =>
        andb (eq ty1 ty1') (eq ty2 ty2')
      | (Prod _ _, _) => false
      | (Sum ty1 ty2, Sum ty1' ty2') =>
        andb (eq ty1 ty1') (eq ty2 ty2')
      | (Sum _ _, _) => false
      | (List ty, List ty') => 
        eq ty ty'
      | (List _, _) => false
      end.

    (* type consistency *)
    Fixpoint consistent (x y : t) : bool :=
      match (x, y) with
      | (Hole, _)
      | (_, Hole) =>  true
      | (Num, Num) => true
      | (Num, _) => false
      | (Bool, Bool) => true
      | (Bool, _) => false
      | (Arrow ty1 ty2, Arrow ty1' ty2')
      | (Prod ty1 ty2, Prod ty1' ty2') 
      | (Sum ty1 ty2, Sum ty1' ty2') =>
        (consistent ty1 ty1') && (consistent ty2 ty2')
      | (Arrow _ _, _) => false
      | (Prod _ _, _) => false
      | (Sum _ _, _) => false
      | (List ty, List ty') => 
        consistent ty ty'
      | (List _, _) => false
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
      match ty with 
      | Arrow _ _ => true
      | Hole => true
      | _ => false
      end.

    (* matched product types *)
    Definition matched_prod (ty : t) : option (t * t) :=
      match ty with
      | Prod ty1 ty2 => Some (ty1, ty2)
      | Hole => Some (Hole, Hole)
      | _ => None
      end.

    Definition has_matched_prod (ty : t) : bool :=
      match ty with
      | Prod _ _ => true
      | Hole => true
      | _ => false
      end.

    (* matched sum types *)
    Definition matched_sum (ty : t) : option (t * t) :=
      match ty with
      | Sum tyL tyR => Some (tyL, tyR)
      | Hole => Some (Hole, Hole)
      | _ => None
      end.

    Definition has_matched_sum (ty : t) : bool :=
      match ty with
      | Sum tyL tyR => true
      | Hole => true
      | _ => false
      end.

    (* matched sum types *)
    Definition matched_list (ty : t) : option t :=
      match ty with
      | List ty => Some ty
      | Hole => Some Hole
      | _ => None
      end.

    Definition has_matched_list (ty : t) : bool :=
      match ty with
      | List ty => true
      | Hole => true
      | _ => false
      end.

    (* complete (i.e. does not have any holes) *)
    Fixpoint complete (ty : t) : bool :=
      match ty with
      | Hole => false
      | Num => true
      | Bool => true
      | Arrow ty1 ty2
      | Prod ty1 ty2 
      | Sum ty1 ty2 
        => andb (complete ty1) (complete ty2)
      | List ty => complete ty
      end.
    
    Fixpoint join ty1 ty2 := 
      match (ty1, ty2) with
      | (_, Hole) => Some ty1
      | (Hole, _) => Some ty2
      | (Num, Num) => Some ty1
      | (Num, _) => None
      | (Bool, Bool) => Some ty1
      | (Bool, _) => None
      | (Arrow ty1 ty2, Arrow ty1' ty2') => 
        match (join ty1 ty1', join ty2 ty2') with 
        | (Some ty1, Some ty2) => Some (Arrow ty1 ty2)
        | _ => None
        end
      | (Arrow _ _, _) => None
      | (Prod ty1 ty2, Prod ty1' ty2') => 
        match (join ty1 ty1', join ty2 ty2') with 
        | (Some ty1, Some ty2) => Some (Prod ty1 ty2)
        | _ => None
        end
      | (Prod _ _, _) => None
      | (Sum ty1 ty2, Sum ty1' ty2') => 
        match (join ty1 ty1', join ty2 ty2') with 
        | (Some ty1, Some ty2) => Some (Sum ty1 ty2)
        | _ => None
        end
      | (Sum _ _, _) => None
      | (List ty, List ty') => 
        match join ty ty' with 
        | Some ty => Some (List ty)
        | None => None
        end
      | (List ty, _) => None
      end.
  End HTyp.

  Module UHTyp.
    Inductive op : Type := 
    | Arrow : op
    | Prod : op
    | Sum : op.

    Definition skel_t : Type := Skel.t op.

    Inductive t : Type := 
    | Parenthesized : t -> t
    | Hole : t
    | Num : t
    | Bool : t
    | List : t -> t
    | OpSeq : skel_t -> OperatorSeq.opseq t op -> t.

    Definition opseq : Type := OperatorSeq.opseq t op.

    Definition bidelimited (uty : t) : bool := 
      match uty with 
      | Num
      | Bool 
      | Hole
      | Parenthesized _ => true
      | List ty => true
      | OpSeq _ _ => false
      end.

    Fixpoint well_formed (fuel : Fuel.t) (uty : t) : bool := 
      match fuel with 
      | Fuel.Kicked => false
      | Fuel.More fuel => 
      match uty with 
      | Hole => true
      | Num => true
      | Bool => true
      | Parenthesized uty1 => well_formed fuel uty1
      | List uty1 => well_formed fuel uty1
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

    (* TODO fix this to only parenthesize when necessary *)
    Fixpoint contract (ty : HTyp.t) : t :=
      let mk_opseq (op' : op) (a b : t) :=
        let ph n := Skel.Placeholder op n in
        let skel :=
          (Skel.BinOp NotInHole op' (ph 0) (ph 1))
        in
        Parenthesized
          (OpSeq skel (OperatorSeq.ExpOpExp a op' b))
        (* Save it for another day
        match (a, b) with
          | (OpSeq skelA opseqA, OpSeq skelB opseqB) =>
          | (OpSeq skelA opseqA, _) =>
          | (_, OpSeq skelB opseqB) =>
          | (_, _) =>
            OpSeq (Skel.BinOp NotInHole op' ?? ??) (OperatorSeq.ExpOpExp a op' b)
        end
        *)
      in
      match ty with
      | HTyp.Hole      => Hole
      | HTyp.Num       => Num
      | HTyp.Bool      => Bool
      | HTyp.Arrow ty1 ty2 => mk_opseq Arrow (contract ty1) (contract ty2)
      | HTyp.Prod  ty1 ty2 => mk_opseq Prod  (contract ty1) (contract ty2)
      | HTyp.Sum   ty1 ty2 => mk_opseq Sum   (contract ty1) (contract ty2)
      | HTyp.List ty1 => List (contract ty1)
      end.

    Fixpoint expand (fuel : Fuel.t) (uty : t) : HTyp.t := 
      match fuel with 
      | Fuel.Kicked => HTyp.Hole
      | Fuel.More fuel => 
      match uty with 
      | Hole => HTyp.Hole
      | Num => HTyp.Num
      | Bool => HTyp.Bool
      | Parenthesized uty1 => expand fuel uty1
      | List uty1 => HTyp.List (expand fuel uty1)
      | OpSeq skel seq => expand_skel fuel skel seq
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
        let uty1 := expand_skel fuel skel1 seq in 
        let uty2 := expand_skel fuel skel2 seq in 
        HTyp.Arrow uty1 uty2
      | Skel.BinOp _ Prod skel1 skel2 => 
        let uty1 := expand_skel fuel skel1 seq in 
        let uty2 := expand_skel fuel skel2 seq in 
        HTyp.Prod uty1 uty2
      | Skel.BinOp _ Sum skel1 skel2 => 
        let uty1 := expand_skel fuel skel1 seq in 
        let uty2 := expand_skel fuel skel2 seq in 
        HTyp.Sum uty1 uty2
      end
      end.
  End UHTyp.

  Module PaletteName.
    Definition t := Coq.Strings.String.string.
    Definition eq (x : t) (y : t) : bool := Util.str_eqb x y.
    Fixpoint _is_valid_internal (s : t) : bool := 
      Var.is_valid s.
    Definition is_valid (s : t) : bool :=
        (* should be equivalent to the OCaml rules: "[_a-z][_a-zA-Z0-9']*" *)
        match s with
        | Coq.Strings.String.EmptyString => false
        | Coq.Strings.String.String first_char rest =>
          (Util.char_eq_b first_char "$") &&
          _is_valid_internal rest
        end.
    Definition check_valid {A : Type}
        (s : t)
        (result : option(A))
        : option(A) :=
        if is_valid s then result else None.
  End PaletteName.

  Module PaletteSerializedModel.
    Definition t : Type := Coq.Strings.String.string.
  End PaletteSerializedModel.

  Module VarCtx.
    Definition t := VarMap.t_ (HTyp.t).
    Include VarMap.
  End VarCtx.

  Inductive inj_side : Type :=
  | L : inj_side
  | R : inj_side.

  Definition pick_side {A : Type} (side : inj_side) (l : A) (r : A) : A :=
    match side with
    | L => l
    | R => r
    end.

  Module UHPat.
    Inductive op : Type := 
    | Comma : op
    | Space : op.

    Definition is_Space op := 
      match op with 
      | Space => true
      | _ => false
      end.

    Definition skel_t : Type := Skel.t op.

    Inductive t : Type := 
    | Pat : err_status -> t' -> t
    | Parenthesized : t -> t
    with t' : Type := 
    | EmptyHole : MetaVar.t -> t'
    | Wild : t'
    | Var : Var.t -> t'
    | NumLit : nat -> t'
    | BoolLit : bool -> t'
    | Inj : inj_side -> t -> t'
    | ListLit : list(t) -> t'
    | OpSeq : skel_t -> OperatorSeq.opseq t op -> t'.

    Definition opseq : Type := OperatorSeq.opseq t op.

    (* bidelimited patterns are those that don't have 
     * sub-patterns at their outer left or right edge
     * in the concrete syntax *)
    Definition bidelimited (p : t) : bool := 
      match p with 
      | Pat _ (EmptyHole _)
      | Pat _ Wild
      | Pat _ (Var _)
      | Pat _ (NumLit _) 
      | Pat _ (BoolLit _)
      | Pat _ (Inj _ _) 
      | Pat _ (ListLit _)
      | Parenthesized _ => true
      | Pat _ (OpSeq _ _) => false
      end.

    (* if p is not bidelimited, bidelimit e parenthesizes it *)
    Definition bidelimit (p : t) := 
      if bidelimited p then p else Parenthesized p.

    (* helper function for constructing a new empty hole *)
    Definition new_EmptyHole (u_gen : MetaVarGen.t) : t * MetaVarGen.t :=
      let (u, u_gen) := MetaVarGen.next u_gen in 
      (Pat NotInHole (EmptyHole u), u_gen).

    Definition is_EmptyHole p := 
      match p with 
      | Pat _ (EmptyHole _) => true
      | _ => false
      end.

    Fixpoint put_in_hole (u : MetaVar.t) (p : t) := 
      match p with 
      | Pat _ p' => Pat (InHole u) p'
      | Parenthesized p1 => Parenthesized (put_in_hole u p1)
      end.

    (* put p in a new hole, if it is not already in a hole *)
    Fixpoint put_in_new_hole (u_gen : MetaVarGen.t) (p : t) := 
      match p with
      | Pat NotInHole p' => 
        let (u, u_gen) := MetaVarGen.next u_gen in 
        (Pat (InHole u) p', u_gen)
      | Pat (InHole _) _ => (p, u_gen)
      | Parenthesized p1 => 
        match put_in_new_hole u_gen p1 with 
        | (p1, u_gen) => (Parenthesized p1, u_gen)
        end
      end.
  End UHPat.

  Module UHExp. (* unassociated H-expressions *) 
    Inductive op : Type := 
    | Plus : op
    | Times : op
    | Space : op
    | Comma : op.

    Definition is_Space op := 
      match op with 
      | Space => true
      | _ => false
      end.

    Definition skel_t := Skel.t op.

    Inductive t : Type := 
    | Tm : err_status -> t' -> t
    | Parenthesized : t -> t
    with t' : Type := 
    | Asc : t -> UHTyp.t -> t'
    | Var : var_err_status -> Var.t -> t'
    | Let : UHPat.t -> option(UHTyp.t) -> t -> t -> t'
    | Lam : UHPat.t -> option(UHTyp.t) -> t -> t'
    | NumLit : nat -> t'
    | BoolLit : bool -> t'
    | Inj : inj_side -> t -> t'
    | Case : t -> list(rule) -> t'
    | ListLit : list(t) -> t'
    | EmptyHole : MetaVar.t -> t'
    | OpSeq : skel_t -> OperatorSeq.opseq t op -> t' (* invariant: skeleton is consistent with opseq *)
    | ApPalette : PaletteName.t -> 
                  PaletteSerializedModel.t -> 
                  (nat * Util.NatMap.t(HTyp.t * t)) (* = PaletteHoleData.t *) -> 
                  t'
    with rule : Type := 
    | Rule : UHPat.t -> t -> rule.

    Definition rules : Type := list(rule).

    (* helper function for constructing a new empty hole *)
    Definition new_EmptyHole (u_gen : MetaVarGen.t) : t * MetaVarGen.t :=
      let (u', u_gen') := MetaVarGen.next u_gen in 
      (Tm NotInHole (EmptyHole u'), u_gen').

    Definition is_EmptyHole e := 
      match e with 
      | Tm _ (EmptyHole _) => true
      | _ => false
      end.

    Definition empty_rule (u_gen : MetaVarGen.t) : rule * MetaVarGen.t := 
      let (rule_p, u_gen) := UHPat.new_EmptyHole u_gen in 
      let (rule_e, u_gen) := UHExp.new_EmptyHole u_gen in  
      let rule := UHExp.Rule rule_p rule_e in 
      (rule, u_gen).

    Module PaletteHoleData.
      Local Open Scope string_scope.
      Definition hole_ref_lbl : Type := nat.
      Definition hole_map : Type := NatMap.t(HTyp.t * t).
      Definition t : Type := (hole_ref_lbl * hole_map).
      Definition empty : t := (0, NatMap.empty).
      Definition mk_hole_ref_var_name (lbl : hole_ref_lbl) : Var.t :=
        Coq.Strings.String.append "__hole_ref_"
          (Coq.Strings.String.append (Debug.string_of_nat lbl) "__").
      Definition next_ref_lbl (x : hole_ref_lbl) := S(x).
      Definition new_hole_ref 
        (u_gen : MetaVarGen.t) 
        (hd : t) 
        (ty : HTyp.t) : (hole_ref_lbl * t * MetaVarGen.t) :=
          let (cur_ref_lbl, cur_map) := hd in 
          let next_ref_lbl := next_ref_lbl(cur_ref_lbl) in 
          let (initial_exp, u_gen) := UHExp.new_EmptyHole u_gen in 
          let next_map := NatMap.extend cur_map (cur_ref_lbl, (ty, initial_exp)) in 
          (cur_ref_lbl, (next_ref_lbl, next_map), u_gen).
      Definition extend_ctx_with_hole_map
        {A : Type}
        (ctx : VarCtx.t * A)
        (hm : hole_map)
        : VarCtx.t * A :=
          let (gamma, palette_ctx) := ctx in
          let gamma' :=
            NatMap.fold hm
              (fun gamma hole_mapping =>
                let (id, v) := hole_mapping in
                let (htyp, _) := v in
                let var_name := mk_hole_ref_var_name id in
                VarCtx.extend gamma (var_name, htyp)
              )
              gamma
          in
          (gamma', palette_ctx).
    End PaletteHoleData.

    Module Type HOLEREFS.
      Parameter hole_ref : Type.
      Parameter lbl_of : hole_ref -> PaletteHoleData.hole_ref_lbl.
      Parameter type_of : hole_ref -> HTyp.t.

      Parameter m_hole_ref : Type -> Type.
      Parameter new_hole_ref : HTyp.t -> m_hole_ref(hole_ref).
      Parameter bind : forall (A B : Type), m_hole_ref(A) -> (A -> m_hole_ref(B)) -> m_hole_ref(B).
      Parameter ret : forall (A : Type), A -> m_hole_ref(A).

      Parameter exec : forall (A : Type), m_hole_ref(A) -> 
        PaletteHoleData.t -> 
        MetaVarGen.t -> 
        A * PaletteHoleData.t * MetaVarGen.t. 
    End HOLEREFS.

    Module HoleRefs : HOLEREFS.
      Definition hole_ref : Type := (PaletteHoleData.hole_ref_lbl * HTyp.t).
      Definition lbl_of (hr : hole_ref) := 
        let (lbl, _) := hr in lbl.
      Definition type_of (hr : hole_ref) := 
        let (_, ty) := hr in ty.

      (* cant define m_hole_ref using Inductive due to Coq limitation *)
      Inductive m_hole_ref' : Type -> Type := 
      | NewHoleRef : HTyp.t -> m_hole_ref'(hole_ref)
      | Bnd : forall (A B : Type), m_hole_ref'(A) -> (A -> m_hole_ref'(B)) -> m_hole_ref'(B)
      | Ret : forall (A : Type), A -> m_hole_ref'(A).
      Definition m_hole_ref := m_hole_ref'. 
      Definition new_hole_ref := NewHoleRef.
      Definition bind := Bnd.
      Definition ret := Ret.
      
      Fixpoint exec {A : Type} 
        (mhr : m_hole_ref(A)) 
        (phd : UHExp.PaletteHoleData.t) 
        (u_gen : MetaVarGen.t) 
        : (A * UHExp.PaletteHoleData.t * MetaVarGen.t) := 
          match mhr with 
          | NewHoleRef ty => 
            let (q, u_gen') := UHExp.PaletteHoleData.new_hole_ref u_gen phd ty in 
            let (lbl, phd') := q in 
            ((lbl, ty), phd', u_gen')
          | Bnd mhra f => 
            let (q, u_gen') := exec mhra phd u_gen in 
            let (x, phd') := q in 
            let mhrb := f x in 
            exec mhrb phd' u_gen'
          | Ret x => (x, phd, u_gen)
          end.
    End HoleRefs.

    Module PaletteDefinition.
      Record t : Type := MkPalette {
        expansion_ty : HTyp.t;
        initial_model : HoleRefs.m_hole_ref(PaletteSerializedModel.t);
        to_exp : PaletteSerializedModel.t -> UHExp.t;
      }.
    End PaletteDefinition.

    Module PaletteCtx.
      Definition t := VarMap.t_ (PaletteDefinition.t).
      Include VarMap.
    End PaletteCtx.

    Module Contexts.
      Definition t : Type := VarCtx.t * PaletteCtx.t.
      Definition gamma (ctx : t) : VarCtx.t := 
        let (gamma, _) := ctx in gamma.
      Definition extend_gamma (contexts : t) (binding : Var.t * HTyp.t) : t := 
        let (x, ty) := binding in 
        let (gamma, palette_ctx) := contexts in 
        let gamma' := VarCtx.extend gamma (x, ty) in
        (gamma', palette_ctx).
      Definition gamma_union (contexts : t) (gamma' : VarCtx.t) : t := 
        let (gamma, palette_ctx) := contexts in 
        let gamma'' := VarCtx.union gamma gamma' in 
        (gamma'', palette_ctx).
    End Contexts.

    Definition opseq := OperatorSeq.opseq t op.

    (* bidelimited expressions are those that don't have 
     * sub-expressions at their outer left or right edge
     * in the concrete syntax *)
    Definition bidelimited (e : t) := 
      match e with 
      (* bidelimited cases *)
      | Tm _ (EmptyHole _)
      | Tm _ (Var _ _)
      | Tm _ (NumLit _)
      | Tm _ (BoolLit _)
      | Tm _ (Inj _ _)
      | Tm _ (Case _ _)
      | Tm _ (ListLit _) 
      | Tm _ (ApPalette _ _ _)
      | Parenthesized _ => true
      (* non-bidelimited cases *)
      | Tm _ (Asc _ _)
      | Tm _ (Let _ _ _ _)
      | Tm _ (Lam _ _ _)
      | Tm _ (OpSeq _ _) => false
      end.

    (* if e is not bidelimited, bidelimit e parenthesizes it *)
    Definition bidelimit (e : t) := 
      if bidelimited e then e else Parenthesized e.

    (* put e in the specified hole *)
    Fixpoint put_in_hole (u : MetaVar.t) (e : t) := 
      match e with 
      | Tm _ e' => Tm (InHole u) e'
      | Parenthesized e' => Parenthesized (put_in_hole u e')
      end.

    (* put e in a new hole, if it is not already in a hole *)
    Fixpoint put_in_new_hole (u_gen : MetaVarGen.t) (e : t) := 
      match e with
      | Tm NotInHole e' => 
        let (u, u_gen') := MetaVarGen.next u_gen in 
        (Tm (InHole u) e', u_gen')
      | Tm (InHole _) _ => (e, u_gen)
      | Parenthesized e1 => 
        match put_in_new_hole u_gen e1 with 
        | (e1', u_gen') => (Parenthesized e1', u_gen')
        end
      end.

    (* put skel in a new hole, if it is not already in a hole *)
    Definition put_skel_in_new_hole (u_gen : MetaVarGen.t) (skel : skel_t) (seq : opseq) := 
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
        let (u', u_gen') := MetaVarGen.next u_gen in 
        Some (Skel.BinOp (InHole u') op skel1 skel2, seq, u_gen')
      end.

    Fixpoint drop_outer_parentheses (e : t) : t := 
      match e with 
      | Tm _ _ => e
      | Parenthesized e' => drop_outer_parentheses e'
      end.

    (* see syn_skel and ana_skel below *) 
    Inductive type_mode : Type := 
    | AnalyzedAgainst : HTyp.t -> type_mode
    | Synthesized : HTyp.t -> type_mode.

    Fixpoint syn_pat 
      (fuel : Fuel.t)
      (ctx  : Contexts.t)
      (p    : UHPat.t)
      : option(HTyp.t * Contexts.t) := 
        match fuel with 
        | Fuel.Kicked => None
        | Fuel.More fuel => 
        match p with 
        | UHPat.Pat (InHole _) p' => 
          match syn_pat' fuel ctx p' with 
          | None => None
          | Some (_, gamma) => Some(HTyp.Hole, gamma)
          end
        | UHPat.Pat NotInHole p' => 
          syn_pat' fuel ctx p'
        | UHPat.Parenthesized p => 
          syn_pat fuel ctx p
        end
        end
    with syn_pat'
      (fuel : Fuel.t)
      (ctx  : Contexts.t)
      (p    : UHPat.t')
      : option(HTyp.t * Contexts.t) := 
        match fuel with 
        | Fuel.Kicked => None
        | Fuel.More fuel => 
        match p with 
        | UHPat.EmptyHole _ => Some (HTyp.Hole, ctx) 
        | UHPat.Wild => Some (HTyp.Hole, ctx)
        | UHPat.Var x =>
          (* TODO: add linearity check *)
          Var.check_valid x (
          Some (HTyp.Hole, 
                Contexts.extend_gamma ctx (x, HTyp.Hole)))
        | UHPat.NumLit _ => Some (HTyp.Num, ctx)
        | UHPat.BoolLit _ => Some (HTyp.Bool, ctx)
        | UHPat.Inj side p1 => 
          match syn_pat fuel ctx p1 with 
          | None => None
          | Some (ty1, ctx) => 
            let ty := 
              match side with 
              | L => HTyp.Sum ty1 HTyp.Hole
              | R => HTyp.Sum HTyp.Hole ty1
              end in 
            Some (ty, ctx)
          end
        | UHPat.ListLit ps => None 
        | UHPat.OpSeq skel seq => 
          match syn_skel_pat fuel ctx skel seq None with 
          | None => None
          | Some (ty, ctx, _) => Some (ty, ctx)
          end
        end
        end
    with syn_skel_pat 
      (fuel : Fuel.t)
      (ctx : Contexts.t)
      (skel : UHPat.skel_t)
      (seq  : UHPat.opseq)
      (monitor : option(nat))
      : option(HTyp.t * Contexts.t * option(type_mode)) := 
        match fuel with 
        | Fuel.Kicked => None
        | Fuel.More fuel => 
        match skel with 
        | Skel.Placeholder _ n => 
          match OperatorSeq.seq_nth n seq with 
          | None => None
          | Some pn => 
            match UHPat.bidelimited pn with 
            | false => None
            | true => 
              match syn_pat fuel ctx pn with 
              | None => None
              | Some (ty, ctx) => 
                let mode := 
                  match monitor with 
                  | None => None
                  | Some n' => 
                    if Nat.eqb n n' 
                    then Some (Synthesized ty) 
                    else None
                  end in 
                Some (ty, ctx, mode)
              end
            end
          end
        | Skel.BinOp (InHole u) op skel1 skel2 => 
          let skel_not_in_hole := Skel.BinOp NotInHole op skel1 skel2 in 
          match syn_skel_pat fuel ctx skel_not_in_hole seq monitor with 
          | None => None
          | Some (ty, ctx, mode) => Some (HTyp.Hole, ctx, mode)
          end
        | Skel.BinOp NotInHole UHPat.Comma skel1 skel2 => 
          match syn_skel_pat fuel ctx skel1 seq monitor with 
          | None => None
          | Some (ty1, ctx, mode1) => 
            match syn_skel_pat fuel ctx skel2 seq monitor with 
            | None => None
            | Some (ty2, ctx, mode2) => 
              let ty := HTyp.Prod ty1 ty2 in 
              let mode := 
                match (mode1, mode2) with 
                | (Some _, _) => mode1
                | (None, Some _) => mode2
                | (None, None) => None
                end in 
              Some (ty, ctx, mode)
            end
          end
        | Skel.BinOp NotInHole UHPat.Space skel1 skel2 => 
          match syn_skel_pat fuel ctx skel1 seq monitor with 
          | None => None
          | Some (ty1, ctx, mode1) => 
            match syn_skel_pat fuel ctx skel2 seq monitor with 
            | None => None
            | Some (ty2, ctx, mode2) => 
              let ty := HTyp.Hole in 
              let mode := 
                match (mode1, mode2) with 
                | (Some _, _) => mode1
                | (None, Some _) => mode2
                | (None, None) => None
                end in 
              Some (ty, ctx, mode)
            end
          end
        end
        end.

    Fixpoint ana_pat 
      (fuel : Fuel.t)
      (ctx  : Contexts.t)
      (p    : UHPat.t)
      (ty   : HTyp.t)
      : option(Contexts.t) := 
        match fuel with 
        | Fuel.Kicked => None
        | Fuel.More fuel => 
        match p with 
        | UHPat.Pat (InHole _) p' => 
          match syn_pat' fuel ctx p' with 
          | None => None
          | Some (_, ctx) => Some ctx
          end
        | UHPat.Pat NotInHole p' => 
          ana_pat' fuel ctx p' ty
        | UHPat.Parenthesized p => 
          ana_pat fuel ctx p ty
        end
        end
    with ana_pat'
      (fuel : Fuel.t)
      (ctx  : Contexts.t)
      (p    : UHPat.t')
      (ty   : HTyp.t)
      : option(Contexts.t) := 
        match fuel with 
        | Fuel.Kicked => None
        | Fuel.More fuel => 
        match p with 
        | UHPat.Var x =>
          (* TODO: add linearity check *)
          Var.check_valid x (
          Some (Contexts.extend_gamma ctx (x, ty)))
        | UHPat.OpSeq skel seq => 
          match ana_skel_pat fuel ctx skel seq ty None with 
          | None => None
          | Some (ctx, _) => Some ctx
          end
        | UHPat.EmptyHole _
        | UHPat.Wild => Some ctx
        | UHPat.NumLit _ 
        | UHPat.BoolLit _ => 
          match syn_pat' fuel ctx p with 
          | None => None
          | Some (ty', ctx) =>
            match HTyp.consistent ty ty' with 
            | true => Some ctx
            | false => None
            end
          end
        | UHPat.Inj side p1 => 
          match HTyp.matched_sum ty with 
          | None => None
          | Some (tyL, tyR) => 
            let ty1 := pick_side side tyL tyR in 
            ana_pat fuel ctx p1 ty1  
          end
        | UHPat.ListLit ps => 
          match HTyp.matched_list ty with 
          | None => None
          | Some ty_elem => 
            List.fold_left (fun optctx p => 
              match optctx with 
              | None => None
              | Some ctx => ana_pat fuel ctx p ty_elem 
              end) ps (Some ctx)
          end
        end
        end
    with ana_skel_pat
      (fuel    : Fuel.t)
      (ctx     : Contexts.t)
      (skel    : UHPat.skel_t)
      (seq     : UHPat.opseq)
      (ty      : HTyp.t)
      (monitor : option(nat))
      : option(Contexts.t * option(type_mode)) := 
        match fuel with 
        | Fuel.Kicked => None
        | Fuel.More fuel => 
        match skel with 
        | Skel.Placeholder _ n => 
          match OperatorSeq.seq_nth n seq with 
          | None => None
          | Some pn => 
            match UHPat.bidelimited pn with 
            | false => None
            | true => 
              match ana_pat fuel ctx pn ty with 
              | None => None
              | Some ctx => 
                let mode := 
                  match monitor with 
                  | None => None
                  | Some n' => 
                    if Nat.eqb n n' 
                    then Some (AnalyzedAgainst ty) 
                    else None
                  end in 
                Some (ctx, mode)
              end
            end
          end
        | Skel.BinOp (InHole u) op skel1 skel2 => 
          let skel_not_in_hole := Skel.BinOp NotInHole op skel1 skel2 in 
          match syn_skel_pat fuel ctx skel_not_in_hole seq monitor with 
          | None => None
          | Some (_, ctx, mode) => Some (ctx, mode)
          end
        | Skel.BinOp NotInHole UHPat.Comma skel1 skel2 => 
          match HTyp.matched_prod ty with 
          | None => None
          | Some (ty1, ty2) => 
            match ana_skel_pat fuel ctx skel1 seq ty1 monitor with 
            | None => None
            | Some (ctx, mode1) => 
              match ana_skel_pat fuel ctx skel2 seq ty2 monitor with 
              | None => None
              | Some (ctx, mode2) => 
                let mode := 
                  match (mode1, mode2) with 
                  | (Some _, _) => mode1
                  | (None, Some _) => mode2
                  | (None, None) => None
                  end in 
                Some (ctx, mode)
              end
            end
          end
        | Skel.BinOp NotInHole UHPat.Space skel1 skel2 => 
          None
        end
        end.

    Definition ctx_for_let
      (ctx : Contexts.t)
      (p   : UHPat.t)
      (ty1 : HTyp.t)
      (e1  : UHExp.t)
      : Contexts.t := 
        match (p, e1) with 
        | (UHPat.Pat _ (UHPat.Var x), 
           Tm _ (Lam _ _ _)) => 
          match HTyp.matched_arrow ty1 with 
          | Some _ => Contexts.extend_gamma ctx (x, ty1) 
          | None => ctx 
          end
        | _ => ctx
        end.

    (* returns recursive ctx + name of recursively defined var *)
    Definition ctx_for_let'
      (ctx : Contexts.t)
      (p   : UHPat.t)
      (ty1 : HTyp.t)
      (e1  : UHExp.t)
      : Contexts.t * option(Var.t) :=
        match (p, e1) with
        | (UHPat.Pat _ (UHPat.Var x),
           Tm _ (Lam _ _ _)) =>
          match HTyp.matched_arrow ty1 with
          | Some _ => (Contexts.extend_gamma ctx (x, ty1), Some x)
          | None => (ctx, None)
          end
        | _ => (ctx, None)
        end.

    (* synthesize a type, if possible, for e *)
    Fixpoint syn
      (fuel : Fuel.t) 
      (ctx : Contexts.t)
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
      (ctx : Contexts.t)
      (e : UHExp.t')
      : option(HTyp.t) := 
        match fuel with
        | Fuel.Kicked => None
        | Fuel.More fuel =>
          match e with 
          | EmptyHole u => Some HTyp.Hole
          | Asc e1 uty =>
            let ty := UHTyp.expand fuel uty in  
            if bidelimited e1 then 
              match ana fuel ctx e1 ty with 
              | None => None
              | Some _ => Some ty
              end
            else None
          | Var NotInVHole x => 
            let (gamma, _) := ctx in 
            VarMap.lookup gamma x 
          | Var (InVHole _) _ =>
            Some (HTyp.Hole)
          | Lam p ann e1 => 
            let ty1 := 
              match ann with 
              | Some uty => UHTyp.expand fuel uty
              | None => HTyp.Hole
              end in 
            match ana_pat fuel ctx p ty1 with 
            | None => None
            | Some ctx => 
              match syn fuel ctx e1 with 
              | Some ty2 => Some (HTyp.Arrow ty1 ty2)
              | None => None
              end
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
          | Let p ann e1 e2 =>
            match ann with 
            | Some uty1 => 
              let ty1 := UHTyp.expand fuel uty1 in 
              let ctx1 := ctx_for_let ctx p ty1 e1 in   
              match ana fuel ctx1 e1 ty1 with 
              | None => None
              | Some _ => 
                match ana_pat fuel ctx p ty1 with 
                | None => None
                | Some ctx2 => syn fuel ctx2 e2
                end
              end
            | None => 
              match syn fuel ctx e1 with 
              | None => None
              | Some ty1 => 
                match ana_pat fuel ctx p ty1 with 
                | None => None
                | Some ctx2 => syn fuel ctx2 e2 
                end
              end
            end
          | NumLit _ => Some HTyp.Num
          | BoolLit _ => Some HTyp.Bool
          | ListLit _ => None
          | OpSeq skel seq => 
            (* NOTE: doesn't check if skel is the correct parse of seq!!! *)
            match syn_skel fuel ctx skel seq None with 
            | Some (ty, _) => Some ty 
            | None => None
            end
          | Case _ _ => None
          | ApPalette name serialized_model hole_data => 
              let (_, palette_ctx) := ctx in 
              match (VarMap.lookup palette_ctx name) with
              | Some palette_defn => 
                match (ana_hole_data fuel ctx hole_data) with 
                | None => None
                | Some _ => 
                  let expansion_ty := PaletteDefinition.expansion_ty palette_defn in  
                  let to_exp := PaletteDefinition.to_exp palette_defn in 
                  let expansion := to_exp serialized_model in 
                  let (_, hole_map) := hole_data in
                  let expansion_ctx := PaletteHoleData.extend_ctx_with_hole_map ctx hole_map in
                  match ana fuel expansion_ctx expansion expansion_ty with
                  | Some _ => 
                    Some expansion_ty
                  | None => None
                  end
                end
              | None => None
              end
          end
        end
    with ana_hole_data
      (fuel : Fuel.t)
      (ctx : Contexts.t)
      (hole_data : PaletteHoleData.t)
      : option(unit) :=
        match fuel with 
        | Fuel.Kicked => None
        | Fuel.More fuel => 
          let (_, hole_map) := hole_data in 
          NatMap.fold hole_map (fun c v => 
            let (_, ty_e) := v in
            let (ty, e) := ty_e in 
            match c with 
            | None => None
            | Some _ => ana fuel ctx e ty
            end) (Some tt)
        end
    with ana
      (fuel : Fuel.t)
      (ctx : Contexts.t)
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
      (ctx : Contexts.t)
      (e : UHExp.t')
      (ty : HTyp.t)
      : option(unit) := 
      match fuel with
      | Fuel.More fuel =>
        match e with
        | Let p ann e1 e2 =>
          match ann with 
          | Some uty1 => 
            let ty1 := UHTyp.expand fuel uty1 in 
            let ctx1 := ctx_for_let ctx p ty1 e1 in   
            match ana fuel ctx1 e1 ty1 with 
            | None => None
            | Some _ => 
              match ana_pat fuel ctx p ty1 with 
              | None => None
              | Some ctx2 => ana fuel ctx2 e2 ty
              end
            end
          | None => 
            match syn fuel ctx e1 with 
            | None => None
            | Some ty1 => 
              match ana_pat fuel ctx p ty1 with 
              | None => None
              | Some ctx2 => ana fuel ctx2 e2 ty 
              end
            end
          end
        | Lam p ann e1 =>
          match HTyp.matched_arrow ty with
          | None => None
          | Some (ty1_given, ty2) =>
            match ann with 
            | Some uty1 => 
              let ty1_ann := UHTyp.expand fuel uty1 in 
              match HTyp.consistent ty1_ann ty1_given with 
              | false => None
              | true => 
                match ana_pat fuel ctx p ty1_ann with 
                | None => None
                | Some ctx => ana fuel ctx e1 ty2
                end
              end
            | None => 
              match ana_pat fuel ctx p ty1_given with 
              | None => None
              | Some ctx => ana fuel ctx e1 ty2
              end
            end
          end
        | Inj side e' =>
          match HTyp.matched_sum ty with
          | None => None
          | Some (ty1, ty2) => 
            ana fuel ctx e' (pick_side side ty1 ty2)
          end
        | ListLit es => 
          match HTyp.matched_list ty with 
          | None => None
          | Some ty_elem => 
            List.fold_left (fun optresult elem => 
              match optresult with 
              | None => None
              | Some _ => ana fuel ctx elem ty_elem 
              end) es (Some tt)
          end
        | Case e1 rules =>
          match syn fuel ctx e1 with 
          | None => None
          | Some ty1 =>
            ana_rules fuel ctx rules ty1 ty 
          end
        | OpSeq skel seq => 
          match ana_skel fuel ctx skel seq ty None with 
          | None => None
          | Some _ => Some tt
          end
        | EmptyHole _
        | Asc _ _
        | Var _ _
        | NumLit _ 
        | BoolLit _ 
        | ApPalette _ _ _ =>
          match syn' fuel ctx e with
          | Some ty' =>
            if HTyp.consistent ty ty' then (Some tt) else None
          | None => None
          end
        end
      | Fuel.Kicked => None
      end
    with ana_rules
      (fuel : Fuel.t)
      (ctx : Contexts.t)
      (rules : list(UHExp.rule))
      (pat_ty : HTyp.t)
      (clause_ty : HTyp.t)
      : option(unit) :=
      match fuel with 
      | Fuel.Kicked => None
      | Fuel.More fuel => 
      List.fold_left (fun b r =>  
        match b with
        | None => None
        | Some _ => ana_rule fuel ctx r pat_ty clause_ty
        end) rules (Some tt)
      end
    with ana_rule
      (fuel : Fuel.t)
      (ctx : Contexts.t)
      (rule : UHExp.rule)
      (pat_ty : HTyp.t)
      (clause_ty : HTyp.t)
      : option(unit) := 
        match fuel with 
        | Fuel.Kicked => None
        | Fuel.More fuel => 
        let (p, e) := rule in 
        match ana_pat fuel ctx p pat_ty with 
        | None => None
        | Some ctx => ana fuel ctx e clause_ty 
        end
        end
    with syn_skel
      (fuel : Fuel.t)
      (ctx : Contexts.t)
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
            if bidelimited en then 
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
        | Skel.BinOp NotInHole Comma skel1 skel2 => 
          match syn_skel fuel ctx skel1 seq monitor with 
          | None => None
          | Some (ty1, mode1) => 
            match syn_skel fuel ctx skel2 seq monitor with 
            | None => None
            | Some (ty2, mode2) => 
              let ty := HTyp.Prod ty1 ty2 in 
              match mode1 with 
              | Some _ => Some (ty, mode1)
              | None => Some (ty, mode2)
              end
            end
          end
        end
        end
    with ana_skel
      (fuel : Fuel.t)
      (ctx : Contexts.t)
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
            if bidelimited en then   
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
        | Skel.BinOp NotInHole Comma skel1 skel2 => 
          match HTyp.matched_prod ty with 
          | None => None
          | Some (ty1, ty2) => 
            match ana_skel fuel ctx skel1 seq ty1 monitor with 
            | None => None
            | Some mode1 => 
              match ana_skel fuel ctx skel2 seq ty2 monitor with 
              | None => None
              | Some mode2 => 
                let mode := 
                  match mode1 with 
                  | Some _ => mode1
                  | None => mode2
                  end in 
                Some mode
              end
            end
          end
        | Skel.BinOp (InHole _) _ _ _
        | Skel.BinOp NotInHole Plus _ _ 
        | Skel.BinOp NotInHole Times _ _ 
        | Skel.BinOp NotInHole Space _ _ => 
          match syn_skel fuel ctx skel seq monitor with 
          | Some (ty', mode) => 
            if HTyp.consistent ty ty' then Some mode else None
          | None => None
          end
        end
        end.

    Fixpoint syn_pat_fix_holes
      (fuel  : Fuel.t)
      (ctx   : Contexts.t)
      (u_gen : MetaVarGen.t)
      (renumber_empty_holes : bool)
      (p     : UHPat.t)
      : option(UHPat.t * HTyp.t * Contexts.t * MetaVarGen.t) := 
        match fuel with 
        | Fuel.Kicked => None
        | Fuel.More fuel => 
        match p with 
        | UHPat.Pat _ p' => 
          match syn_pat_fix_holes' fuel ctx u_gen renumber_empty_holes p' with 
          | None => None
          | Some (p', ty, ctx, u_gen) => Some(UHPat.Pat NotInHole p', ty, ctx, u_gen)
          end
        | UHPat.Parenthesized p => 
          match syn_pat_fix_holes fuel ctx u_gen renumber_empty_holes p with 
          | None => None
          | Some (p, ty, ctx, u_gen) => Some (UHPat.Parenthesized p, ty, ctx, u_gen)
          end
        end
        end
    with syn_pat_fix_holes'
      (fuel : Fuel.t)
      (ctx  : Contexts.t)
      (u_gen : MetaVarGen.t)
      (renumber_empty_holes : bool)
      (p  : UHPat.t')
      : option(UHPat.t' * HTyp.t * Contexts.t * MetaVarGen.t) := 
        match fuel with 
        | Fuel.Kicked => None
        | Fuel.More fuel => 
        match p with 
        | UHPat.EmptyHole u => 
          if renumber_empty_holes then 
            let (u, u_gen) := MetaVarGen.next u_gen in 
            Some (UHPat.EmptyHole u, HTyp.Hole, ctx, u_gen)
          else
            Some (p, HTyp.Hole, ctx, u_gen)
        | UHPat.Wild => Some (p, HTyp.Hole, ctx, u_gen)
        | UHPat.Var x =>
          Var.check_valid x ( 
          let ctx := Contexts.extend_gamma ctx (x, HTyp.Hole) in
          Some (p, HTyp.Hole, ctx, u_gen))
        | UHPat.NumLit _ => Some (p, HTyp.Num, ctx, u_gen)
        | UHPat.BoolLit _ => Some (p, HTyp.Bool, ctx, u_gen)
        | UHPat.Inj side p1 => 
          match syn_pat_fix_holes fuel ctx u_gen renumber_empty_holes p1 with 
          | None => None
          | Some (p1, ty1, ctx, u_gen) => 
            let ty := 
              match side with 
              | L => HTyp.Sum ty1 HTyp.Hole
              | R => HTyp.Sum HTyp.Hole ty1
              end in 
            Some (UHPat.Inj side p1, ty, ctx, u_gen)
          end
        | UHPat.ListLit _ => None
        | UHPat.OpSeq skel seq => 
          match syn_skel_pat_fix_holes fuel ctx u_gen renumber_empty_holes skel seq with 
          | None => None
          | Some (skel, seq, ty, ctx, u_gen) => 
            Some (UHPat.OpSeq skel seq, ty, ctx, u_gen)
          end
        end
        end
    with syn_skel_pat_fix_holes 
      (fuel  : Fuel.t)
      (ctx   : Contexts.t)
      (u_gen : MetaVarGen.t)
      (renumber_empty_holes : bool)
      (skel  : UHPat.skel_t)
      (seq   : UHPat.opseq)
      : option(UHPat.skel_t * UHPat.opseq * HTyp.t * Contexts.t * MetaVarGen.t) := 
        match fuel with 
        | Fuel.Kicked => None
        | Fuel.More fuel => 
        match skel with 
        | Skel.Placeholder _ n => 
          match OperatorSeq.seq_nth n seq with
          | None => None
          | Some pn => 
            match UHPat.bidelimited pn with
            | false => None
            | true => 
              match syn_pat_fix_holes fuel ctx u_gen renumber_empty_holes pn with 
              | None => None
              | Some (pn, ty, ctx, u_gen) => 
                match OperatorSeq.seq_update_nth n seq pn with
                | None => None
                | Some seq => 
                  Some (skel, seq, ty, ctx, u_gen)
                end
              end
            end
          end
        | Skel.BinOp _ UHPat.Comma skel1 skel2 => 
          match syn_skel_pat_fix_holes fuel ctx u_gen renumber_empty_holes skel1 seq with 
          | None => None
          | Some (skel1, seq, ty1, ctx, u_gen) =>
            match syn_skel_pat_fix_holes fuel ctx u_gen renumber_empty_holes skel2 seq with 
            | None => None
            | Some (skel2, seq, ty2, ctx, u_gen) => 
              let skel := Skel.BinOp NotInHole UHPat.Comma skel1 skel2 in 
              let ty := HTyp.Prod ty1 ty2 in 
              Some (skel, seq, ty, ctx, u_gen)
            end
          end
        | Skel.BinOp _ UHPat.Space skel1 skel2 => 
          match syn_skel_pat_fix_holes fuel ctx u_gen renumber_empty_holes skel1 seq with 
          | None => None
          | Some (skel1, seq, ty1, ctx, u_gen) =>
            match syn_skel_pat_fix_holes fuel ctx u_gen renumber_empty_holes skel2 seq with 
            | None => None
            | Some (skel2, seq, ty2, ctx, u_gen) => 
              let skel := Skel.BinOp NotInHole UHPat.Comma skel1 skel2 in 
              let ty := HTyp.Hole in 
              Some (skel, seq, ty, ctx, u_gen)
            end
          end
        end
        end.

    Fixpoint ana_pat_fix_holes 
      (fuel : Fuel.t)
      (ctx  : Contexts.t)
      (u_gen : MetaVarGen.t)
      (renumber_empty_holes : bool)
      (p  : UHPat.t)
      (ty   : HTyp.t)
      : option(UHPat.t * Contexts.t * MetaVarGen.t) := 
        match fuel with 
        | Fuel.Kicked => None
        | Fuel.More fuel => 
        match p with 
        | UHPat.Pat _ p' => 
          match ana_pat_fix_holes' fuel ctx u_gen renumber_empty_holes p' ty with 
          | None => None
          | Some (err_status, p', ctx, u_gen) => 
            Some (UHPat.Pat err_status p', ctx, u_gen)
          end
        | UHPat.Parenthesized p => 
          match ana_pat_fix_holes fuel ctx u_gen renumber_empty_holes p ty with 
          | None => None
          | Some (p, ctx, u_gen) => 
            Some (UHPat.Parenthesized p, ctx, u_gen)
          end
        end
        end
    with ana_pat_fix_holes'
      (fuel : Fuel.t)
      (ctx  : Contexts.t)
      (u_gen : MetaVarGen.t)
      (renumber_empty_holes : bool)
      (p  : UHPat.t')
      (ty   : HTyp.t)
      : option(err_status * UHPat.t' * Contexts.t * MetaVarGen.t) := 
        match fuel with 
        | Fuel.Kicked => None
        | Fuel.More fuel => 
        match p with 
        | UHPat.Wild => Some (NotInHole, p, ctx, u_gen)
        | UHPat.Var x => 
          Var.check_valid x (
          let ctx := Contexts.extend_gamma ctx (x, ty) in 
          Some (NotInHole, p, ctx, u_gen))
        | UHPat.OpSeq skel seq => 
          match ana_skel_pat_fix_holes fuel ctx u_gen renumber_empty_holes skel seq ty with 
          | None => None
          | Some (Skel.Placeholder _ _, _, _, _) => None 
          | Some ((Skel.BinOp err _ _ _) as skel, seq, ctx, u_gen) =>  
            let p := UHPat.OpSeq skel seq in 
            Some (err, p, ctx, u_gen)
          end
        | UHPat.EmptyHole _  
        | UHPat.NumLit _ 
        | UHPat.BoolLit _ =>  
          match syn_pat_fix_holes' fuel ctx u_gen renumber_empty_holes p with 
          | None => None
          | Some (p', ty', ctx, u_gen) => 
            match HTyp.consistent ty ty' with 
            | true => Some (NotInHole, p', ctx, u_gen)
            | false => 
              let (u, u_gen) := MetaVarGen.next u_gen in 
              Some (InHole u, p', ctx, u_gen)
            end
          end
        | UHPat.Inj side p1 => 
          match HTyp.matched_sum ty with 
          | Some (tyL, tyR) => 
            let ty1 := pick_side side tyL tyR in 
            match ana_pat_fix_holes fuel ctx u_gen renumber_empty_holes p1 ty1 with 
            | None => None
            | Some (p1, ctx, u_gen) => 
              Some (NotInHole, UHPat.Inj side p1, ctx, u_gen)
            end
          | None => 
            match syn_pat_fix_holes fuel ctx u_gen renumber_empty_holes p1 with 
            | None => None
            | Some (p1, ty, ctx, u_gen) => 
              let (u, u_gen) := MetaVarGen.next u_gen in 
              Some (InHole u, UHPat.Inj side p1, ctx, u_gen)
            end
          end
        | UHPat.ListLit ps => 
          match HTyp.matched_list ty with 
          | Some ty_elem => 
            let ps_result := 
              List.fold_left (fun opt_result elem => 
                match opt_result with 
                | None => None
                | Some (ps, ctx, u_gen) => 
                  match ana_pat_fix_holes fuel ctx u_gen renumber_empty_holes elem ty_elem with
                  | None => None
                  | Some (elem, ctx, u_gen) => 
                    Some (cons elem ps, ctx, u_gen)
                  end
                end) ps (Some (nil, ctx, u_gen)) in 
            match ps_result with 
            | None => None
            | Some (ps, ctx, u_gen) => 
              Some (NotInHole, UHPat.ListLit ps, ctx, u_gen)
            end
          | None => None
          end
        end
        end
    with ana_skel_pat_fix_holes
      (fuel : Fuel.t)
      (ctx  : Contexts.t)
      (u_gen : MetaVarGen.t)
      (renumber_empty_holes : bool)
      (skel  : UHPat.skel_t)
      (seq : UHPat.opseq)
      (ty   : HTyp.t)
      : option(UHPat.skel_t * UHPat.opseq * Contexts.t * MetaVarGen.t) :=
        match fuel with 
        | Fuel.Kicked => None
        | Fuel.More fuel => 
        match skel with 
        | Skel.Placeholder _ n => 
          match OperatorSeq.seq_nth n seq with
          | None => None
          | Some pn => 
            match UHPat.bidelimited pn with
            | false => None
            | true => 
              match ana_pat_fix_holes fuel ctx u_gen renumber_empty_holes pn ty with 
              | None => None
              | Some (pn, ctx, u_gen) => 
                match OperatorSeq.seq_update_nth n seq pn with 
                | Some seq => Some (skel, seq, ctx, u_gen)
                | None => None
                end
              end
            end
          end
        | Skel.BinOp _ UHPat.Comma skel1 skel2 => 
          match HTyp.matched_prod ty with 
          | Some (ty1, ty2) => 
            match ana_skel_pat_fix_holes fuel ctx u_gen renumber_empty_holes skel1 seq ty1 with 
            | None => None
            | Some (skel1, seq, ctx, u_gen) => 
              match ana_skel_pat_fix_holes fuel ctx u_gen renumber_empty_holes skel2 seq ty2 with 
              | None => None
              | Some (skel2, seq, ctx, u_gen) => 
                let skel := Skel.BinOp NotInHole UHPat.Comma skel1 skel2 in 
                Some (skel, seq, ctx, u_gen)
              end
            end
          | None => 
            match syn_skel_pat_fix_holes fuel ctx u_gen renumber_empty_holes skel1 seq with 
            | None => None
            | Some (skel1, seq, _, ctx, u_gen) => 
              match syn_skel_pat_fix_holes fuel ctx u_gen renumber_empty_holes skel2 seq with 
              | None => None
              | Some (skel2, seq, _, ctx, u_gen) => 
                let (u, u_gen) := MetaVarGen.next u_gen in 
                let skel := Skel.BinOp (InHole u) UHPat.Comma skel1 skel2 in 
                Some (skel, seq, ctx, u_gen)
              end
            end
          end
        | Skel.BinOp _ UHPat.Space skel1 skel2 => 
          match syn_skel_pat_fix_holes fuel ctx u_gen renumber_empty_holes skel1 seq with 
          | None => None
          | Some (skel1, seq, _, ctx, u_gen) => 
            match syn_skel_pat_fix_holes fuel ctx u_gen renumber_empty_holes skel2 seq with 
            | None => None
            | Some (skel2, seq, _, ctx, u_gen) => 
              let (u, u_gen) := MetaVarGen.next u_gen in 
              let skel := Skel.BinOp (InHole u) UHPat.Space skel1 skel2 in 
              Some (skel, seq, ctx, u_gen)
            end
          end
        end
        end.


      Definition ana_rule_fix_holes
        (fuel : Fuel.t)
        (ctx : Contexts.t)
        (u_gen : MetaVarGen.t) 
        (renumber_empty_holes : bool)
        (rule : UHExp.rule)
        (pat_ty : HTyp.t)
        (clause_ty : HTyp.t)
        (* need to pass a reference to the ana_fix_holes_internal function here
         * rather than defining it mutually to avoid a stack overflow error seemingly
         * related to too many mutually recursive definitions in Coq *)
        (ana_fix_holes_internal : Fuel.t -> Contexts.t -> MetaVarGen.t -> bool -> UHExp.t -> HTyp.t -> option(UHExp.t * MetaVarGen.t))
        : option(UHExp.rule * MetaVarGen.t) :=
          match fuel with 
          | Fuel.Kicked => None
          | Fuel.More fuel => 
            match rule with 
            | Rule pat e => 
              match ana_pat_fix_holes fuel ctx u_gen renumber_empty_holes pat pat_ty with 
              | None => None
              | Some ((pat', ctx), u_gen) => 
                match ana_fix_holes_internal fuel ctx u_gen renumber_empty_holes e clause_ty with 
                | None => None
                | Some (e', u_gen) => Some(Rule pat' e', u_gen)
                end
              end
            end
          end.
      
      Definition ana_rules_fix_holes_internal 
        (fuel : Fuel.t)
        (ctx : Contexts.t)
        (u_gen : MetaVarGen.t) 
        (renumber_empty_holes : bool)
        (rules : list(UHExp.rule))
        (pat_ty : HTyp.t)
        (clause_ty : HTyp.t)
        (* see above *)
        (ana_fix_holes_internal : Fuel.t -> Contexts.t -> MetaVarGen.t -> bool -> UHExp.t -> HTyp.t -> option(UHExp.t * MetaVarGen.t))
        : option(list(UHExp.rule) * MetaVarGen.t) :=
          match fuel with 
          | Fuel.Kicked => None
          | Fuel.More fuel => 
          List.fold_right (fun r b =>  
            match b with
            | None => None
            | Some (rules, u_gen) => 
              match ana_rule_fix_holes fuel ctx u_gen renumber_empty_holes r pat_ty clause_ty ana_fix_holes_internal with 
              | None => None
              | Some (r, u_gen) => Some (cons r rules, u_gen)
              end
            end) (Some (nil, u_gen)) rules
          end.
    
    (* If renumber_empty_holes is true, then the metavars in empty holes will be assigned
     * new values in the same namespace as non-empty holes. Non-empty holes are renumbered
     * regardless.
     *)
    Fixpoint syn_fix_holes_internal
      (fuel : Fuel.t)
      (ctx : Contexts.t)
      (u_gen : MetaVarGen.t)
      (renumber_empty_holes : bool)
      (e : t)
      : option(t * HTyp.t * MetaVarGen.t) := 
        match fuel with 
        | Fuel.Kicked => None
        | Fuel.More fuel => 
        match e with 
        | Tm _ e' => 
          match syn_fix_holes' fuel ctx u_gen renumber_empty_holes e' with 
          | Some (e'', ty, u_gen') => 
            Some (Tm NotInHole e'', ty, u_gen')
          | None => None
          end
        | Parenthesized e1 => 
          match syn_fix_holes_internal fuel ctx u_gen renumber_empty_holes e1 with 
          | Some (e1', ty, u_gen') => 
            Some (Parenthesized e1', ty, u_gen')
          | None => None
          end
        end
        end
    with syn_fix_holes'
      (fuel : Fuel.t)
      (ctx : Contexts.t)
      (u_gen : MetaVarGen.t)
      (renumber_empty_holes : bool)
      (e : t')
      : option(t' * HTyp.t * MetaVarGen.t) := 
        match fuel with 
        | Fuel.Kicked => None
        | Fuel.More fuel => 
        match e with 
        | EmptyHole u =>
          if renumber_empty_holes then
            let (u', u_gen'') := MetaVarGen.next u_gen in 
            Some (EmptyHole u', HTyp.Hole, u_gen'')
          else
            Some (EmptyHole u, HTyp.Hole, u_gen)
        | Asc e1 uty => 
          if bidelimited e1 then 
            let ty := UHTyp.expand fuel uty in 
            match ana_fix_holes_internal fuel ctx u_gen renumber_empty_holes e1 ty with 
            | Some (e1', u_gen') => Some (Asc e1' uty, ty, u_gen')
            | None => None
            end
          else None
        | Var var_err_status x => 
          let (gamma, _) := ctx in 
          match VarMap.lookup gamma x with 
          | Some ty => Some (Var NotInVHole x, ty, u_gen)
          | None => 
            match var_err_status with 
            | InVHole u => Some (e, HTyp.Hole, u_gen)
            | NotInVHole => 
              let (u, u_gen) := MetaVarGen.next u_gen in 
              Some (Var (InVHole u) x, HTyp.Hole, u_gen)
            end
          end
        | Lam p ann e1 => 
          let ty1 := 
            match ann with 
            | Some uty1 => UHTyp.expand fuel uty1
            | None => HTyp.Hole
            end in 
          match ana_pat_fix_holes fuel ctx u_gen renumber_empty_holes p ty1 with 
          | None => None
          | Some (p, ctx1, u_gen) => 
            match syn_fix_holes_internal fuel ctx1 u_gen renumber_empty_holes e1 with 
            | None => None
            | Some (e1, ty2, u_gen) => 
              Some (Lam p ann e1, HTyp.Arrow ty1 ty2, u_gen)
            end
          end
        | Let p ann e1 e2 => 
          match ann with 
          | Some uty1 => 
            let ty1 := UHTyp.expand fuel uty1 in 
            let ctx1 := ctx_for_let ctx p ty1 e1 in 
            match ana_fix_holes_internal fuel ctx1 u_gen renumber_empty_holes e1 ty1 with
            | None => None
            | Some (e1, u_gen) => 
              match ana_pat_fix_holes fuel ctx u_gen renumber_empty_holes p ty1 with 
              | None => None
              | Some (p, ctx2, u_gen) => 
                match syn_fix_holes_internal fuel ctx u_gen renumber_empty_holes e2 with 
                | Some (e2, ty, u_gen) => 
                  Some (Let p ann e1 e2, ty, u_gen)
                | None => None
                end
              end
            end
          | None => 
            match syn_fix_holes_internal fuel ctx u_gen renumber_empty_holes e1 with 
            | Some (e1, ty1, u_gen) => 
              match ana_pat_fix_holes fuel ctx u_gen renumber_empty_holes p ty1 with 
              | None => None
              | Some (p, ctx2, u_gen) => 
                match syn_fix_holes_internal fuel ctx2 u_gen renumber_empty_holes e2 with 
                | Some (e2, ty, u_gen) => 
                  Some (Let p ann e1 e2, ty, u_gen)
                | None => None
                end
              end
            | None => None
            end
          end
        | NumLit i => Some (e, HTyp.Num, u_gen)
        | BoolLit b => Some (e, HTyp.Bool, u_gen)
        | ListLit _ => None
        | OpSeq skel seq => 
          match syn_skel_fix_holes fuel ctx u_gen renumber_empty_holes skel seq with 
          | None => None
          | Some (Skel.Placeholder _ _, _, _, _) => None
          | Some (skel, seq, ty, u_gen) => 
            Some (OpSeq skel seq, ty, u_gen)
          end
        | Inj side e1 => 
          match syn_fix_holes_internal fuel ctx u_gen renumber_empty_holes e1 with 
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
        | Case _ _ => None
        | ApPalette name serialized_model hole_data => 
          let (_, palette_ctx) := ctx in 
          match (VarMap.lookup palette_ctx name) with
          | None => None
          | Some palette_defn => 
            match (ana_fix_holes_hole_data fuel ctx u_gen renumber_empty_holes hole_data) with 
            | None => None
            | Some (hole_data', u_gen') => 
              let expansion_ty := PaletteDefinition.expansion_ty palette_defn in  
              let to_exp := PaletteDefinition.to_exp palette_defn in 
              let expansion := to_exp serialized_model in 
              let (_, hole_map) := hole_data in
              let expansion_ctx := PaletteHoleData.extend_ctx_with_hole_map ctx hole_map in
              match ana fuel expansion_ctx expansion expansion_ty with
              | Some _ => 
                Some (ApPalette name serialized_model hole_data', expansion_ty, u_gen')
              | None => None
              end
            end
          end
        end
        end
    with ana_fix_holes_hole_data
      (fuel : Fuel.t)
      (ctx : Contexts.t)
      (u_gen : MetaVarGen.t)
      (renumber_empty_holes : bool)
      (hole_data : PaletteHoleData.t)
      : option(PaletteHoleData.t * MetaVarGen.t) := 
        match fuel with 
        | Fuel.Kicked => None
        | Fuel.More fuel => 
          let (next_ref, hole_map) := hole_data in 
          let init : (PaletteHoleData.hole_map * MetaVarGen.t) := (NatMap.empty, u_gen) in 
          let hole_map_opt' := NatMap.fold hole_map (fun (c : option(PaletteHoleData.hole_map * MetaVarGen.t)) v => 
            let (i, ty_e) := v in 
            let (ty, e) := ty_e in 
            match c with 
            | None => None
            | Some (xs, u_gen) => 
              match (ana_fix_holes_internal fuel ctx u_gen renumber_empty_holes e ty) with 
              | Some (e', u_gen') => 
                let xs' := NatMap.extend xs (i, (ty, e')) in 
                Some (xs', u_gen')
              | None => None
              end
            end) (Some init) in 
          match hole_map_opt' with 
          | Some (hole_map', u_gen') => Some ((next_ref, hole_map'), u_gen')
          | None => None
          end
        end
    with ana_fix_holes_internal
      (fuel : Fuel.t)
      (ctx : Contexts.t)
      (u_gen : MetaVarGen.t)
      (renumber_empty_holes : bool)
      (e : t)
      (ty : HTyp.t)
      : option(t * MetaVarGen.t) := 
        match fuel with 
        | Fuel.Kicked => None
        | Fuel.More fuel => 
        match e with 
        | Tm _ e1 => 
          match ana_fix_holes' fuel ctx u_gen renumber_empty_holes e1 ty with 
          | Some (err_status, e1', u_gen') => 
            Some (Tm err_status e1', u_gen')
          | None => None
          end
        | Parenthesized e1 => 
          match ana_fix_holes_internal fuel ctx u_gen renumber_empty_holes e1 ty with 
          | Some (e1', u_gen') => 
            Some (Parenthesized e1', u_gen')
          | None => None
          end
        end
        end
    with ana_fix_holes'
      (fuel : Fuel.t) (ctx : Contexts.t) (u_gen : MetaVarGen.t) (renumber_empty_holes : bool)
      (e : t') (ty : HTyp.t)
      : option(err_status * t' * MetaVarGen.t) := 
        match fuel with 
        | Fuel.Kicked => None
        | Fuel.More fuel => 
        match e with 
        | Let p ann e1 e2 => 
          match ann with 
          | Some uty1 => 
            let ty1 := UHTyp.expand fuel uty1 in 
            let ctx1 := ctx_for_let ctx p ty1 e1 in 
            match ana_fix_holes_internal fuel ctx1 u_gen renumber_empty_holes e1 ty1 with
            | None => None
            | Some (e1, u_gen) => 
              match ana_pat_fix_holes fuel ctx u_gen renumber_empty_holes p ty1 with 
              | None => None
              | Some (p, ctx2, u_gen) => 
                match ana_fix_holes_internal fuel ctx u_gen renumber_empty_holes e2 ty with 
                | Some (e2, u_gen) => 
                  Some (NotInHole, Let p ann e1 e2, u_gen)
                | None => None
                end
              end
            end
          | None => 
            match syn_fix_holes_internal fuel ctx u_gen renumber_empty_holes e1 with 
            | Some (e1, ty1, u_gen) => 
              match ana_pat_fix_holes fuel ctx u_gen renumber_empty_holes p ty1 with 
              | None => None
              | Some (p, ctx2, u_gen) => 
                match ana_fix_holes_internal fuel ctx2 u_gen renumber_empty_holes e2 ty with 
                | Some (e2, u_gen) => 
                  Some (NotInHole, Let p ann e1 e2, u_gen)
                | None => None
                end
              end
            | None => None
            end
          end
        | Lam p ann e1 => 
          match HTyp.matched_arrow ty with 
          | Some (ty1_given, ty2) => 
            match ann with 
            | Some uty1 => 
              let ty1_ann := UHTyp.expand fuel uty1 in 
              match HTyp.consistent ty1_ann ty1_given with 
              | true => 
                match ana_pat_fix_holes fuel ctx u_gen renumber_empty_holes p ty1_ann with 
                | None => None
                | Some (p, ctx, u_gen) => 
                  match ana_fix_holes_internal fuel ctx u_gen renumber_empty_holes e1 ty2 with 
                  | None => None
                  | Some (e1, u_gen) => 
                    Some (NotInHole, Lam p ann e1, u_gen)
                  end
                end
              | false => 
                match syn_fix_holes' fuel ctx u_gen renumber_empty_holes e with 
                | None => None
                | Some (e, ty, u_gen) => 
                  let (u, u_gen) := MetaVarGen.next u_gen in 
                  Some (InHole u, e, u_gen)
                end
              end
            | None => 
              match ana_pat_fix_holes fuel ctx u_gen renumber_empty_holes p ty1_given with 
              | None => None
              | Some (p, ctx, u_gen) => 
                match ana_fix_holes_internal fuel ctx u_gen renumber_empty_holes e1 ty2 with 
                | Some (e1, u_gen) => 
                  Some (NotInHole, Lam p ann e1, u_gen)
                | None => None
                end
              end
            end
          | None => 
            match syn_fix_holes' fuel ctx u_gen renumber_empty_holes e with 
            | None => None
            | Some (e, ty', u_gen) => 
              let (u, u_gen) := MetaVarGen.next u_gen in 
              Some (InHole u, e, u_gen)
            end
          end
        | Inj side e1 => 
          match HTyp.matched_sum ty with 
          | Some (ty1, ty2) => 
            match ana_fix_holes_internal fuel ctx u_gen renumber_empty_holes e1 (pick_side side ty1 ty2) with 
            | Some (e1', u_gen') => 
              Some (NotInHole, Inj side e1', u_gen')
            | None => None
            end
          | None => 
            match syn_fix_holes' fuel ctx u_gen renumber_empty_holes e with 
            | Some (e', ty', u_gen') => 
              if HTyp.consistent ty ty' then 
                Some (NotInHole, e', u_gen')
              else 
                let (u, u_gen'') := MetaVarGen.next u_gen' in 
                Some (InHole u, e', u_gen'')
            | None => None
            end
          end
        | ListLit es => 
          match HTyp.matched_list ty with 
          | Some ty_elem => 
            let opt_es := List.fold_left (fun opt_result elem => 
              match opt_result with 
              | None => None
              | Some (es, u_gen) => 
                match ana_fix_holes_internal fuel ctx u_gen renumber_empty_holes elem ty_elem with 
                | None => None
                | Some (elem, u_gen) => 
                  Some (cons elem es, u_gen)
                end
              end) es (Some (nil, u_gen)) in 
            match opt_es with 
            | None => None
            | Some (es, u_gen) => Some (NotInHole, ListLit es, u_gen)
            end
          | None => None
          end
        | Case e1 rules => 
          match syn_fix_holes_internal fuel ctx u_gen renumber_empty_holes e1 with 
          | None => None
          | Some (e1', ty1, u_gen) => 
            match ana_rules_fix_holes_internal fuel ctx u_gen renumber_empty_holes rules ty1 ty 
                    ana_fix_holes_internal with 
            | None => None
            | Some (rules', u_gen) => 
              Some (NotInHole, Case e1' rules', u_gen)
            end
          end
        | OpSeq skel seq =>  
          match ana_skel_fix_holes fuel ctx u_gen renumber_empty_holes skel seq ty with 
          | None => None
          | Some (Skel.Placeholder _ _, _, _) => None
          | Some ((Skel.BinOp err _ _ _) as skel, seq, u_gen) => 
            Some (err, OpSeq skel seq, u_gen)
          end
        | EmptyHole _ 
        | Asc _ _
        | Var _ _ 
        | NumLit _ 
        | BoolLit _ 
        | ApPalette _ _ _ =>  
          match syn_fix_holes' fuel ctx u_gen renumber_empty_holes e with 
          | Some (e', ty', u_gen') => 
            if HTyp.consistent ty ty' then 
              Some (NotInHole, e', u_gen')
            else 
              let (u, u_gen'') := MetaVarGen.next u_gen' in 
              Some (InHole u, e', u_gen'')
          | None => None
          end
        end
        end
    with syn_skel_fix_holes
      (fuel : Fuel.t)
      (ctx : Contexts.t)
      (u_gen : MetaVarGen.t)
      (renumber_empty_holes : bool)
      (skel : skel_t)
      (seq : opseq)
      : option(skel_t * opseq * HTyp.t * MetaVarGen.t) := 
        match fuel with 
        | Fuel.Kicked => None
        | Fuel.More fuel => 
        match skel with 
        | Skel.Placeholder _ n => 
          match OperatorSeq.seq_nth n seq with
          | None => None
          | Some en => 
            match bidelimited en with
            | false => None
            | true => 
              match syn_fix_holes_internal fuel ctx u_gen renumber_empty_holes en with 
              | None => None
              | Some (en, ty, u_gen) => 
                match OperatorSeq.seq_update_nth n seq en with
                | None => None
                | Some seq => 
                  Some (skel, seq, ty, u_gen)
                end
              end
            end
          end
        | Skel.BinOp _ (Plus as op) skel1 skel2 
        | Skel.BinOp _ (Times as op) skel1 skel2 => 
          match ana_skel_fix_holes fuel ctx u_gen renumber_empty_holes skel1 seq HTyp.Num with 
          | Some (skel1', seq1, u_gen1) => 
            match ana_skel_fix_holes fuel ctx u_gen1 renumber_empty_holes skel2 seq1 HTyp.Num with 
            | Some (skel2', seq2, u_gen2) => 
              Some (Skel.BinOp NotInHole op skel1' skel2', seq2, HTyp.Num, u_gen2)
            | None => None
            end
          | None => None
          end
        | Skel.BinOp _ Space skel1 skel2 => 
          match syn_skel_fix_holes fuel ctx u_gen renumber_empty_holes skel1 seq with 
          | Some (skel1', seq1, ty1, u_gen1) => 
            match HTyp.matched_arrow ty1 with 
            | Some (ty2, ty) => 
              match ana_skel_fix_holes fuel ctx u_gen1 renumber_empty_holes skel2 seq1 ty2 with 
              | Some (skel2', seq2, u_gen2) => 
                Some (Skel.BinOp NotInHole Space skel1' skel2', seq2, ty, u_gen2)
              | None => None
              end
            | None =>
              match ana_skel_fix_holes fuel ctx u_gen1 renumber_empty_holes skel2 seq1 HTyp.Hole with 
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
        | Skel.BinOp _ Comma skel1 skel2 => 
          match syn_skel_fix_holes fuel ctx u_gen renumber_empty_holes skel1 seq with 
          | None => None
          | Some (skel1, seq, ty1, u_gen) => 
            match syn_skel_fix_holes fuel ctx u_gen renumber_empty_holes skel2 seq with 
            | None => None
            | Some (skel2, seq, ty2, u_gen) =>
              let skel := Skel.BinOp NotInHole Comma skel1 skel2 in 
              let ty := HTyp.Prod ty1 ty2 in 
              Some (skel, seq, ty, u_gen)
            end
          end
        end
        end
    with ana_skel_fix_holes
      (fuel : Fuel.t)
      (ctx : Contexts.t)
      (u_gen : MetaVarGen.t)
      (renumber_empty_holes : bool)
      (skel : skel_t)
      (seq : opseq)
      (ty : HTyp.t)
      : option(skel_t * opseq * MetaVarGen.t) := 
        match fuel with 
        | Fuel.Kicked => None
        | Fuel.More fuel => 
        match skel with 
        | Skel.Placeholder _ n => 
          match OperatorSeq.seq_nth n seq with
          | None => None
          | Some en => 
            match bidelimited en with
            | false => None
            | true => 
              match ana_fix_holes_internal fuel ctx u_gen renumber_empty_holes en ty with 
              | None => None
              | Some (en, u_gen) => 
                match OperatorSeq.seq_update_nth n seq en with 
                | Some seq => Some (skel, seq, u_gen)
                | None => None
                end
              end
            end
          end
        | Skel.BinOp _ Comma skel1 skel2 => 
          match HTyp.matched_prod ty with 
          | Some (ty1, ty2) => 
            match ana_skel_fix_holes fuel ctx u_gen renumber_empty_holes skel1 seq ty1 with 
            | None => None
            | Some (skel1, seq, u_gen) => 
              match ana_skel_fix_holes fuel ctx u_gen renumber_empty_holes skel2 seq ty2 with 
              | None => None
              | Some (skel2, seq, u_gen) => 
                let skel := Skel.BinOp NotInHole Comma skel1 skel2 in 
                Some (skel, seq, u_gen)
              end
            end
          | None => 
            match syn_skel_fix_holes fuel ctx u_gen renumber_empty_holes skel1 seq with 
            | None => None
            | Some (skel1, seq, _, u_gen) => 
              match syn_skel_fix_holes fuel ctx u_gen renumber_empty_holes skel2 seq with 
              | None => None
              | Some (skel2, seq, _, u_gen) =>
                let (u, u_gen) := MetaVarGen.next u_gen in 
                let skel := Skel.BinOp (InHole u) Comma skel1 skel2 in 
                Some (skel, seq, u_gen)
              end
            end
          end
        | Skel.BinOp _ Plus _ _  
        | Skel.BinOp _ Times _ _
        | Skel.BinOp _ Space _ _ =>  
          match syn_skel_fix_holes fuel ctx u_gen renumber_empty_holes skel seq with 
          | Some (skel', seq', ty', u_gen') => 
            if HTyp.consistent ty ty' then Some (skel', seq', u_gen')
            else 
              put_skel_in_new_hole u_gen' skel' seq'
          | None => None
          end
        end
        end.

    Definition syn_fix_holes
      (fuel : Fuel.t)
      (ctx : Contexts.t)
      (u_gen : MetaVarGen.t)
      (e : t)
      : option(t * HTyp.t * MetaVarGen.t) := 
      syn_fix_holes_internal fuel ctx u_gen false e.

    Definition ana_fix_holes
      (fuel : Fuel.t)
      (ctx : Contexts.t)
      (u_gen : MetaVarGen.t)
      (e : t)
      (ty : HTyp.t)
      : option(t * MetaVarGen.t) := 
      ana_fix_holes_internal fuel ctx u_gen false e ty.
    
    Definition ana_rules_fix_holes 
      (fuel : Fuel.t)
      (ctx : Contexts.t)
      (u_gen : MetaVarGen.t) 
      (renumber_empty_holes : bool)
      (rules : list(UHExp.rule))
      (pat_ty : HTyp.t)
      (clause_ty : HTyp.t) := 
        ana_rules_fix_holes_internal fuel ctx u_gen renumber_empty_holes rules pat_ty clause_ty
          ana_fix_holes_internal.

    (* Only to be used on top-level expressions, as it starts hole renumbering at 0 *)
    Definition fix_and_renumber_holes
      (fuel : Fuel.t)
      (ctx : Contexts.t)
      (e : t)
      : option(t * HTyp.t * MetaVarGen.t) := 
      syn_fix_holes_internal fuel ctx MetaVarGen.init true e.
  End UHExp.

  Module Contexts := UHExp.Contexts.
  Module PaletteCtx := UHExp.PaletteCtx.

  Inductive cursor_side : Type := 
  | Before : cursor_side
  | After : cursor_side
  | In : nat -> cursor_side.
 
  Module ZList.
    Definition t (Z A : Type) : Type := list(A) * Z * list(A).

    Definition singleton {A Z : Type} (z : Z) : t Z A := 
      (nil, z, nil).

    Fixpoint split_at {A : Type} (n : nat) (xs : list A) : option(t A A) := 
      match (n, xs) with 
      | (_, nil) => None
      | (0, cons x xs) => 
        let prefix := nil in 
        let suffix := xs in 
        Some ((prefix, x), suffix)
      | (S n', cons x xs) => 
        match split_at n' xs with 
        | None => None
        | Some ((prefix, z), suffix) => 
          let prefix' := cons x prefix in 
          Some ((prefix', z), suffix)
        end
      end.

    Fixpoint replace_z {A Z : Type} 
      (zs : t Z A)
      (z : Z) 
      : t Z A := 
        match zs with 
        | ((prefix, _), suffix) => ((prefix, z), suffix)
        end.

    Fixpoint optmap_z {A Z1 Z2 : Type} 
      (f : Z1 -> option(Z2))
      (zs : t Z1 A)
      : option(t Z2 A) := 
        match zs with 
        | ((prefix, z), suffix) => 
          match f z with 
          | None => None
          | Some z' => Some ((prefix, z'), suffix)
          end
        end.

    Definition prj_prefix {Z A : Type} (zxs : t Z A) : list(A) := 
      let (az, _) := zxs in 
      let (prefix, _) := az in 
      prefix.

    Definition prefix_length {Z A : Type} (zxs : t Z A) : nat := 
      List.length (prj_prefix zxs).

    Definition prj_z {Z A : Type} (zxs : t Z A) : Z := 
      let (az, _) := zxs in 
      let (_, z) := az in 
      z.

    Definition prj_suffix {Z A : Type} (zxs : t Z A) : list(A) := 
      let (_, suffix) := zxs in 
      suffix.

    Definition erase {Z A : Type} (xs : t Z A) (erase_z : Z -> A) := 
      let (az, xs_after) := xs in 
      let (xs_before, z) := az in 
      let a := erase_z z in 
      xs_before ++ (a :: xs_after).
  End ZList.

  (* Zippered finite map over nats, used with Z expressions 
   * i.e. there is a selected element of type Z and the rest is a nat map of type A *)
  Module ZNatMap.
    Definition t (A Z : Type) : Type := NatMap.t(A) * (nat * Z). 
    Definition new {A Z : Type} (m : NatMap.t(A)) (nz : nat * Z) : option(t A Z) :=
      let (n, z) := nz in 
      match NatMap.lookup m n with 
      | Some _ => None
      | None => Some (m, nz)
      end.
  End ZNatMap.

  Module ZTyp.
    Definition cursor_side : Type := cursor_side.

    Inductive t : Type :=
    | CursorT : cursor_side -> UHTyp.t -> t
    | ParenthesizedZ : t -> t
    | ListZ : t -> t
    | OpSeqZ : UHTyp.skel_t -> t -> OperatorSeq.opseq_surround UHTyp.t UHTyp.op -> t.

    Definition opseq_surround : Type := 
      OperatorSeq.opseq_surround UHTyp.t UHTyp.op.
    Definition opseq_prefix : Type := 
      OperatorSeq.opseq_prefix UHTyp.t UHTyp.op.
    Definition opseq_suffix : Type :=
      OperatorSeq.opseq_suffix UHTyp.t UHTyp.op.

    Definition place_Before (uty : UHTyp.t) : t := 
      match uty with 
      | UHTyp.Hole
      | UHTyp.Parenthesized _
      | UHTyp.Num
      | UHTyp.Bool
      | UHTyp.List _ => CursorT Before uty
      | UHTyp.OpSeq skel seq => 
        let (uty0, suffix) := OperatorSeq.split0 seq in 
        let surround := OperatorSeq.EmptyPrefix suffix in 
        OpSeqZ skel (CursorT Before uty0) surround
      end.

    Definition place_After (uty : UHTyp.t) : t := 
      match uty with 
      | UHTyp.Hole 
      | UHTyp.Parenthesized _
      | UHTyp.Num
      | UHTyp.Bool 
      | UHTyp.List _ => CursorT After uty
      | UHTyp.OpSeq skel seq => 
        let (uty0, prefix) := OperatorSeq.split_tail seq in 
        let surround := OperatorSeq.EmptySuffix prefix in 
        OpSeqZ skel (CursorT After uty0) surround
      end.

    (* |_ -> _ *)
    Definition ZHole_Arrow_Hole : t := 
      OpSeqZ 
        (Skel.BinOp NotInHole UHTyp.Arrow
          (Skel.Placeholder _ O)
          (Skel.Placeholder _ 1))
        (CursorT Before UHTyp.Hole)
        (OperatorSeq.EmptyPrefix
          (OperatorSeq.ExpSuffix UHTyp.Arrow UHTyp.Hole)).

    (* |_ + _ *)
    Definition ZHole_Sum_Hole : t := 
      OpSeqZ 
        (Skel.BinOp NotInHole UHTyp.Sum
          (Skel.Placeholder _ O)
          (Skel.Placeholder _ 1))
        (CursorT Before UHTyp.Hole)
        (OperatorSeq.EmptyPrefix
          (OperatorSeq.ExpSuffix UHTyp.Sum UHTyp.Hole)).

    Fixpoint erase (zty : t) : UHTyp.t :=
      match zty with
      | CursorT _ ty => ty
      | ParenthesizedZ zty1 => UHTyp.Parenthesized (erase zty1)
      | ListZ zty1 => UHTyp.List (erase zty1) 
      | OpSeqZ skel zty1 surround => 
        let uty1 := erase zty1 in 
        UHTyp.OpSeq skel 
          (OperatorSeq.opseq_of_exp_and_surround uty1 surround)
      end.
  End ZTyp.

  Module ZPat.
    Definition cursor_side : Type := cursor_side.

    Inductive t : Type :=
    | CursorP : cursor_side -> UHPat.t -> t
    | ParenthesizedZ : t -> t
    | Deeper : err_status -> t' -> t
    with t' : Type := 
    | InjZ : inj_side -> t -> t'
    | ListLitZ : ZList.t t UHPat.t -> t'
    | OpSeqZ : UHPat.skel_t -> t -> OperatorSeq.opseq_surround UHPat.t UHPat.op -> t'.

    Definition opseq_surround : Type := OperatorSeq.opseq_surround UHPat.t UHPat.op.
    Definition opseq_prefix : Type := OperatorSeq.opseq_prefix UHPat.t UHPat.op.
    Definition opseq_suffix : Type := OperatorSeq.opseq_suffix UHPat.t UHPat.op.

    Definition bidelimit zp := 
      match zp with 
      | CursorP cursor_side p => 
        CursorP cursor_side (UHPat.bidelimit p)
      | ParenthesizedZ _ 
      | Deeper _ (InjZ _ _)
      | Deeper _ (ListLitZ _) => zp
      | Deeper _ (OpSeqZ _ _ _) => ParenthesizedZ zp
      end.

    (* helper function for constructing a new empty hole *)
    Definition new_EmptyHole (u_gen : MetaVarGen.t) : t * MetaVarGen.t :=
      let (hole, u_gen) := UHPat.new_EmptyHole u_gen in 
      (CursorP Before hole, u_gen).

    Fixpoint put_in_hole
      (u : MetaVar.t)
      (zp : t)
      : t :=
        match zp with 
        | CursorP cursor_side p => 
          let p := UHPat.put_in_hole u p in 
          (CursorP cursor_side p)
        | Deeper (InHole _) zp' => 
          Deeper (InHole u) zp'
        | Deeper NotInHole zp' => 
          Deeper (InHole u) zp'
        | ParenthesizedZ zp1 => 
          ParenthesizedZ (put_in_hole u zp1)
        end.

    Fixpoint put_in_new_hole 
      (u_gen : MetaVarGen.t)
      (zp : t) 
      : (t * MetaVarGen.t) := 
        match zp with 
        | CursorP cursor_side p => 
          let (p, u_gen) := UHPat.put_in_new_hole u_gen p in  
          (CursorP cursor_side p, u_gen)
        | Deeper (InHole _) _ => 
          (zp, u_gen)
        | Deeper NotInHole zp' => 
          let (u, u_gen) := MetaVarGen.next u_gen in 
          (Deeper (InHole u) zp', u_gen)
        | ParenthesizedZ zp1 => 
          let (zp1, u_gen) := put_in_new_hole u_gen zp1 in 
          (ParenthesizedZ zp1, u_gen)
        end.

    Fixpoint erase (zp : ZPat.t) : UHPat.t := 
      match zp with 
      | ZPat.CursorP _ p => p
      | ZPat.Deeper err_status zp' => UHPat.Pat err_status (erase' zp')
      | ZPat.ParenthesizedZ zp => UHPat.Parenthesized (erase zp)
      end
    with erase' (zp' : ZPat.t') : UHPat.t' := 
      match zp' with 
      | ZPat.InjZ side zp1 => UHPat.Inj side (erase zp1)
      | ZPat.ListLitZ zps => UHPat.ListLit (ZList.erase zps erase)
      | ZPat.OpSeqZ skel zp1 surround => 
        let p1 := erase zp1 in 
        UHPat.OpSeq skel (OperatorSeq.opseq_of_exp_and_surround p1 surround)
      end.

    Definition place_Before (p : UHPat.t) : t := 
      match p with 
      | UHPat.Parenthesized _ 
      | UHPat.Pat _ (UHPat.EmptyHole _)
      | UHPat.Pat _ UHPat.Wild
      | UHPat.Pat _ (UHPat.Var _)
      | UHPat.Pat _ (UHPat.NumLit _)
      | UHPat.Pat _ (UHPat.BoolLit _)
      | UHPat.Pat _ (UHPat.Inj _ _) 
      | UHPat.Pat _ (UHPat.ListLit _) => CursorP Before p
      | UHPat.Pat err (UHPat.OpSeq skel seq) => 
        let (p0, suffix) := OperatorSeq.split0 seq in 
        let surround := OperatorSeq.EmptyPrefix suffix in 
        Deeper err (OpSeqZ skel (CursorP Before p) surround)
      end.

    Definition place_After (p : UHPat.t) : t := 
      match p with 
      | UHPat.Parenthesized _ 
      | UHPat.Pat _ (UHPat.EmptyHole _)
      | UHPat.Pat _ UHPat.Wild
      | UHPat.Pat _ (UHPat.Var _)
      | UHPat.Pat _ (UHPat.NumLit _)
      | UHPat.Pat _ (UHPat.BoolLit _)
      | UHPat.Pat _ (UHPat.Inj _ _) 
      | UHPat.Pat _ (UHPat.ListLit _) => CursorP After p
      | UHPat.Pat err (UHPat.OpSeq skel seq) => 
        let (p0, prefix) := OperatorSeq.split_tail seq in 
        let surround := OperatorSeq.EmptySuffix prefix in 
        Deeper err (OpSeqZ skel (CursorP After p0) surround)
      end.
  End ZPat.

  Module ZExp.
    Definition cursor_side : Type := cursor_side.

    Inductive t : Type := 
    | CursorE : cursor_side -> UHExp.t -> t
    (* | CursorPalette : PaletteName.t -> PaletteSerializedModel.t -> hole_ref -> t -> t *)
    | Deeper : err_status -> t' -> t
    | ParenthesizedZ : t -> t
    with t' : Type := 
    | AscZ1 : t -> UHTyp.t -> t'
    | AscZ2 : UHExp.t -> ZTyp.t -> t'
    | LetZP : ZPat.t -> option(UHTyp.t) -> UHExp.t -> UHExp.t -> t'
    | LetZA : UHPat.t -> ZTyp.t -> UHExp.t -> UHExp.t -> t'
    | LetZE1 : UHPat.t -> option(UHTyp.t) -> t -> UHExp.t -> t'
    | LetZE2 : UHPat.t -> option(UHTyp.t) -> UHExp.t -> t -> t'
    | LamZP : ZPat.t -> option(UHTyp.t) -> UHExp.t -> t'
    | LamZA : UHPat.t -> ZTyp.t -> UHExp.t -> t'
    | LamZE : UHPat.t -> option(UHTyp.t) -> t -> t'
    | InjZ : inj_side -> t -> t'
    | ListLitZ : ZList.t t UHExp.t -> t'
    | CaseZE : t -> list(UHExp.rule) -> t'
    | CaseZR : UHExp.t -> ZList.t zrule UHExp.rule -> t'
    | OpSeqZ : UHExp.skel_t -> t -> OperatorSeq.opseq_surround UHExp.t UHExp.op -> t'
    | ApPaletteZ : PaletteName.t -> 
                   PaletteSerializedModel.t -> 
                   (UHExp.PaletteHoleData.hole_ref_lbl * ZNatMap.t (HTyp.t * UHExp.t) (HTyp.t * t)) -> (* = ZPaletteHoleData.t *)
                   t'
    with zrule : Type := 
    | RuleZP : ZPat.t -> UHExp.t -> zrule
    | RuleZE : UHPat.t -> t -> zrule.

    Definition zrules : Type := ZList.t zrule UHExp.rule.

    Module ZPaletteHoleData.
      Definition z_hole_map : Type := ZNatMap.t (HTyp.t * t) (HTyp.t * ZExp.t).
      Definition t : Type := (UHExp.PaletteHoleData.hole_ref_lbl * 
                              ZNatMap.t (HTyp.t * UHExp.t) (HTyp.t * ZExp.t)).
    End ZPaletteHoleData.

    Definition opseq_surround : Type := OperatorSeq.opseq_surround UHExp.t UHExp.op.
    Definition opseq_prefix : Type := OperatorSeq.opseq_prefix UHExp.t UHExp.op.
    Definition opseq_suffix : Type := OperatorSeq.opseq_suffix UHExp.t UHExp.op.

    Definition bidelimit ze := 
      match ze with 
      | CursorE cursor_side e => 
        CursorE cursor_side (UHExp.bidelimit e)
      | ParenthesizedZ _ 
      | Deeper _ (InjZ _ _)
      | Deeper _ (ApPaletteZ _ _ _)
      | Deeper _ (CaseZE _ _) 
      | Deeper _ (CaseZR _ _) 
      | Deeper _ (ListLitZ _) => ze
      | Deeper _ (AscZ1 _ _) 
      | Deeper _ (AscZ2 _ _)  
      | Deeper _ (LetZP _ _ _ _) 
      | Deeper _ (LetZA _ _ _ _)
      | Deeper _ (LetZE1 _ _ _ _)
      | Deeper _ (LetZE2 _ _ _ _)
      | Deeper _ (LamZP _ _ _)
      | Deeper _ (LamZA _ _ _)
      | Deeper _ (LamZE _ _ _)
      | Deeper _ (OpSeqZ _ _ _) => 
        ParenthesizedZ ze
      end.

    Fixpoint put_in_hole
      (u : MetaVar.t)
      (ze : t)
      : t :=
        match ze with 
        | CursorE cursor_side e => 
          let e' := UHExp.put_in_hole u e in 
          (CursorE cursor_side e')
        | Deeper (InHole _) ze' => 
          Deeper (InHole u) ze'
        | Deeper NotInHole ze' => 
          Deeper (InHole u) ze'
        | ParenthesizedZ ze1 => 
          ParenthesizedZ (put_in_hole u ze1)
        end.

    Fixpoint put_in_new_hole 
      (u_gen : MetaVarGen.t)
      (ze : t) 
      : (t * MetaVarGen.t) := 
        match ze with 
        | CursorE cursor_side e => 
          let (e', u_gen') := UHExp.put_in_new_hole u_gen e in  
          (CursorE cursor_side e', u_gen')
        | Deeper (InHole _) _ => 
          (ze, u_gen)
        | Deeper NotInHole ze' => 
          let (u', u_gen') := MetaVarGen.next u_gen in 
          (Deeper (InHole u') ze', u_gen')
        | ParenthesizedZ ze1 => 
          let (ze1', u_gen') := put_in_new_hole u_gen ze1 in 
          (ParenthesizedZ ze1, u_gen')
        end.

    Definition new_EmptyHole (u_gen : MetaVarGen.t) := 
      let (e, u_gen) := UHExp.new_EmptyHole u_gen in 
      (CursorE Before e, u_gen).

    Fixpoint cursor_on_outer_expr (ze : t) : option(UHExp.t * cursor_side) := 
      match ze with 
      | CursorE side e => Some ((UHExp.drop_outer_parentheses e), side)
      | ParenthesizedZ ze' => cursor_on_outer_expr ze'
      | Deeper _ _ => None
      end.

    Definition empty_zrule (u_gen : MetaVarGen.t) : zrule * MetaVarGen.t:= 
      let (zp, u_gen) := ZPat.new_EmptyHole u_gen in 
      let (rule_e, u_gen) := UHExp.new_EmptyHole u_gen in 
      let zrule := ZExp.RuleZP zp rule_e in  
      (zrule, u_gen).

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
      | LetZP zp ann e1 e2 => UHExp.Let (ZPat.erase zp) ann e1 e2
      | LetZA p zann e1 e2 => UHExp.Let p (Some (ZTyp.erase zann)) e1 e2 
      | LetZE1 p ann ze e => UHExp.Let p ann (erase ze) e
      | LetZE2 p ann e ze => UHExp.Let p ann e (erase ze)
      | LamZP zp ann e1 => UHExp.Lam (ZPat.erase zp) ann e1
      | LamZA p zann e1 => UHExp.Lam p (Some (ZTyp.erase zann)) e1
      | LamZE p ann ze1 => UHExp.Lam p ann (erase ze1)
      | InjZ side ze => UHExp.Inj side (erase ze)
      | ListLitZ zes => UHExp.ListLit (ZList.erase zes erase)  
      | CaseZE ze1 rules => UHExp.Case (erase ze1) rules
      | CaseZR e1 zrules => UHExp.Case e1 (ZList.erase zrules erase_rule)
      | OpSeqZ skel ze' surround => 
         let e := erase ze' in 
         UHExp.OpSeq skel (OperatorSeq.opseq_of_exp_and_surround e surround)
      | ApPaletteZ palette_name serialized_model zhole_data => 
         let (next_hole_ref, zholemap) := zhole_data in 
         let (holemap, z) := zholemap in 
         let (hole_ref, tz) := z in
         let (ty, ze) := tz in 
         let holemap' := NatMap.extend holemap (hole_ref, (ty, erase ze)) in 
         let hole_data' := (next_hole_ref, holemap') in 
         UHExp.ApPalette palette_name serialized_model hole_data'
      end
    with erase_rule (zr : zrule) : UHExp.rule := 
      match zr with 
      | RuleZP zp e => UHExp.Rule (ZPat.erase zp) e
      | RuleZE p ze => UHExp.Rule p (erase ze)
      end.

    Inductive cursor_mode := 
    (* cursor in analytic position *)
    | AnaOnly : HTyp.t -> cursor_mode
    | AnaAnnotatedLambda : HTyp.t -> HTyp.t -> cursor_mode 
    | TypeInconsistent : HTyp.t -> HTyp.t -> cursor_mode
    | AnaFree : HTyp.t -> cursor_mode
    | Subsumed : HTyp.t -> HTyp.t -> cursor_mode
    (* cursor in synthetic position *)
    | SynOnly : HTyp.t -> cursor_mode
    | SynFree : cursor_mode
    | SynErrorArrow : HTyp.t (* expected *) -> HTyp.t (* got *) -> cursor_mode
    | SynMatchingArrow : HTyp.t -> HTyp.t -> cursor_mode
    | SynFreeArrow : HTyp.t -> cursor_mode
    (* cursor in type position *)
    | TypePosition : cursor_mode
    (* cursor in analytic pattern position *)
    | PatAnaOnly : HTyp.t -> cursor_mode
    | PatTypeInconsistent : HTyp.t -> HTyp.t -> cursor_mode
    | PatSubsumed : HTyp.t -> HTyp.t -> cursor_mode
    (* cursor in synthetic pattern position *)
    | PatSynOnly : HTyp.t -> cursor_mode.

    Inductive cursor_sort := 
    | IsExpr : UHExp.t -> cursor_sort
    | IsPat : UHPat.t -> cursor_sort 
    | IsType : cursor_sort.
    
    Record cursor_info : Type := mk_cursor_info {
      mode : cursor_mode;
      sort : cursor_sort;
      side : cursor_side;
      ctx : Contexts.t
    }.

    Definition update_sort (ci : cursor_info) (sort : cursor_sort) : cursor_info := 
      let mode := mode ci in 
      let side := side ci in 
      let ctx := ctx ci in 
      mk_cursor_info mode sort side ctx.

    Fixpoint ana_pat_cursor_found
      (fuel : Fuel.t)
      (ctx : Contexts.t)
      (p : UHPat.t)
      (ty : HTyp.t)
      (side : cursor_side)
      : option(cursor_info) := 
        match fuel with 
        | Fuel.Kicked => None
        | Fuel.More fuel => 
        match p with 
        | UHPat.Parenthesized p1 =>
          match ana_pat_cursor_found fuel ctx p1 ty side with
          | None => None
          | Some ci => 
            Some (update_sort ci (IsPat p))
          end
        | UHPat.Pat (InHole _) p' => 
          match UHExp.syn_pat' fuel ctx p' with 
          | None => None
          | Some (ty', _) => 
            Some 
              (mk_cursor_info
                (PatTypeInconsistent ty ty')
                (IsPat p)
                side
                ctx)
          end
        | UHPat.Pat NotInHole (UHPat.EmptyHole _) => 
          Some 
            (mk_cursor_info
              (PatSubsumed ty HTyp.Hole)
              (IsPat p)
              side
              ctx)
        | UHPat.Pat NotInHole UHPat.Wild
        | UHPat.Pat NotInHole (UHPat.Var _) =>
          Some
            (mk_cursor_info
              (PatAnaOnly ty)
              (IsPat p)
              side
              ctx)
        | UHPat.Pat NotInHole (UHPat.NumLit _) => 
          Some
            (mk_cursor_info
              (PatSubsumed ty HTyp.Num)
              (IsPat p)
              side
              ctx)
        | UHPat.Pat NotInhole (UHPat.BoolLit _) => 
          Some
            (mk_cursor_info 
              (PatSubsumed ty HTyp.Bool)
              (IsPat p)
              side
              ctx)
        | UHPat.Pat NotInHole (UHPat.Inj _ _) => 
          match UHExp.syn_pat fuel ctx p with 
          | None => None
          | Some (ty', _) => 
            Some
              (mk_cursor_info
                (PatSubsumed ty ty')
                (IsPat p)
                side
                ctx)
          end
        | UHPat.Pat NotInHole (UHPat.ListLit _) => 
          Some
            (mk_cursor_info
              (PatAnaOnly ty)
              (IsPat p)
              side
              ctx)
        | UHPat.Pat NotInHole (UHPat.OpSeq (Skel.BinOp NotInHole Comma skel1 skel2) seq) => 
          Some
            (mk_cursor_info 
              (PatAnaOnly ty)
              (IsPat p)
              side
              ctx)
        | UHPat.Pat NotInHole (UHPat.OpSeq (Skel.BinOp (InHole_) Comma skel1 skel2) seq) => None 
        | UHPat.Pat NotInHole (UHPat.OpSeq (Skel.Placeholder _ _) _) => None
        end
        end.

    Fixpoint syn_pat_cursor_info
      (fuel : Fuel.t)
      (ctx : Contexts.t)
      (zp : ZPat.t)
      : option(cursor_info) := 
        match fuel with 
        | Fuel.Kicked => None
        | Fuel.More fuel => 
        match zp with 
        | ZPat.CursorP side p => 
          match UHExp.syn_pat fuel ctx p with 
          | None => None
          | Some (ty, _) => 
            Some
              (mk_cursor_info
                (PatSynOnly ty)
                (IsPat p)
                side
                ctx)
          end
        | ZPat.Deeper _ zp' => 
          syn_pat_cursor_info' fuel ctx zp'
        | ZPat.ParenthesizedZ zp1 => 
          syn_pat_cursor_info fuel ctx zp1
        end
        end
    with syn_pat_cursor_info'
      (fuel : Fuel.t)
      (ctx : Contexts.t)
      (zp' : ZPat.t')
      : option(cursor_info) := 
        match fuel with 
        | Fuel.Kicked => None
        | Fuel.More fuel => 
        match zp' with 
        | ZPat.InjZ side zp1 => syn_pat_cursor_info fuel ctx zp1
        | ZPat.ListLitZ _ => None
        | ZPat.OpSeqZ skel zp1 surround => 
          let p1 := ZPat.erase zp1 in 
          let seq := OperatorSeq.opseq_of_exp_and_surround p1 surround in 
          let n := OperatorSeq.surround_prefix_length surround in 
          syn_skel_pat_cursor_info fuel ctx skel seq n zp1
        end 
        end
    with syn_skel_pat_cursor_info 
      (fuel : Fuel.t)
      (ctx : Contexts.t)
      (skel : UHPat.skel_t)
      (seq : UHPat.opseq)
      (n : nat)
      (zp1 : ZPat.t)
      : option(cursor_info) := 
        match fuel with 
        | Fuel.Kicked => None
        | Fuel.More fuel => 
        match skel with 
        | Skel.Placeholder _ n' => 
          if Nat.eqb n n' then 
            syn_pat_cursor_info fuel ctx zp1
          else None
        | Skel.BinOp _ Comma skel1 skel2 => 
          match syn_skel_pat_cursor_info fuel ctx skel1 seq n zp1 with 
          | (Some _) as result => result
          | None => syn_skel_pat_cursor_info fuel ctx skel2 seq n zp1 
          end
        end
        end.
    
    Fixpoint ana_pat_cursor_info
      (fuel : Fuel.t)
      (ctx : Contexts.t)
      (zp : ZPat.t) 
      (ty : HTyp.t)
      : option(cursor_info) := 
        match fuel with 
        | Fuel.Kicked => None
        | Fuel.More fuel => 
        match zp with  
        | ZPat.CursorP side p => 
          ana_pat_cursor_found fuel ctx p ty side
        | ZPat.Deeper (InHole u) zp' => 
          syn_pat_cursor_info' fuel ctx zp'
        | ZPat.Deeper NotInHole zp' => 
          ana_pat_cursor_info' fuel ctx zp' ty
        | ZPat.ParenthesizedZ zp => 
          ana_pat_cursor_info fuel ctx zp ty
        end
        end
    with ana_pat_cursor_info'
      (fuel : Fuel.t)
      (ctx : Contexts.t)
      (zp' : ZPat.t')
      (ty : HTyp.t)
      : option(cursor_info) := 
        match fuel with 
        | Fuel.Kicked => None
        | Fuel.More fuel => 
        match zp' with 
        | ZPat.InjZ side zp1 => 
          match HTyp.matched_sum ty with 
          | None => None
          | Some (tyL, tyR) => 
            let ty1 := pick_side side tyL tyR in 
            ana_pat_cursor_info fuel ctx zp1 ty1
          end
        | ZPat.ListLitZ zps => 
          match HTyp.matched_list ty with 
          | None => None
          | Some ty_elem => 
            let zp := ZList.prj_z zps in 
            ana_pat_cursor_info fuel ctx zp ty_elem
          end
        | ZPat.OpSeqZ skel zp1 surround => 
          let p1 := ZPat.erase zp1 in 
          let seq := OperatorSeq.opseq_of_exp_and_surround p1 surround in 
          let n := OperatorSeq.surround_prefix_length surround in 
          ana_skel_pat_cursor_info fuel ctx skel seq n zp1 ty
        end
        end
    with ana_skel_pat_cursor_info 
      (fuel : Fuel.t)
      (ctx : Contexts.t)
      (skel : UHPat.skel_t)
      (seq : UHPat.opseq)
      (n : nat)
      (zp1 : ZPat.t)
      (ty : HTyp.t)
      : option(cursor_info) := 
        match fuel with 
        | Fuel.Kicked => None
        | Fuel.More fuel => 
        match skel with 
        | Skel.Placeholder _ n' => 
          if Nat.eqb n n' then 
            ana_pat_cursor_info fuel ctx zp1 ty
          else None
        | Skel.BinOp (InHole _) Comma skel1 skel2 => 
          match syn_skel_pat_cursor_info fuel ctx skel1 seq n zp1 with 
          | (Some _) as result => result
          | None => 
            match syn_skel_pat_cursor_info fuel ctx skel2 seq n zp1 with 
            | (Some _) as result => result
            | None => None
            end
          end
        | Skel.BinOp NotInHole Comma skel1 skel2 => 
          match HTyp.matched_prod ty with 
          | None => None
          | Some (ty1, ty2) => 
            match ana_skel_pat_cursor_info fuel ctx skel1 seq n zp1 ty1 with 
            | (Some _) as result => result
            | None => 
              match ana_skel_pat_cursor_info fuel ctx skel2 seq n zp1 ty2 with 
              | (Some _) as result => result
              | None => None
              end
            end
          end
        end
        end.

    Fixpoint ana_cursor_found
      (fuel : Fuel.t) (ctx : Contexts.t)
      (e : UHExp.t) (ty : HTyp.t) 
      (side : cursor_side)
      : option(cursor_info) := 
      match fuel with 
      | Fuel.Kicked => None
      | Fuel.More fuel => 
      match e with
      | UHExp.Parenthesized e' => 
        match ana_cursor_found fuel ctx e' ty side with
        | None => None
        | Some ci => 
          Some (update_sort ci (IsExpr e))
        end
      | UHExp.Tm (InHole _) e' => 
        match UHExp.syn' fuel ctx e' with 
        | None => None 
        | Some ty' => 
          Some (
            mk_cursor_info 
              (TypeInconsistent ty ty')
              (IsExpr e)
              side
              ctx
          )
        end
      | UHExp.Tm _ (UHExp.Var (InVHole _) _) => 
        Some (
          mk_cursor_info
            (AnaFree ty)
            (IsExpr e)
            side
            ctx
        )
      | UHExp.Tm NotInHole (UHExp.Let _ _ _ _)
      | UHExp.Tm NotInHole (UHExp.Case _ _) 
      | UHExp.Tm NotInHole (UHExp.ListLit _) =>
        Some (
          mk_cursor_info 
            (AnaOnly ty)
            (IsExpr e)
            side
            ctx
        )
      | UHExp.Tm NotInHole (UHExp.OpSeq (Skel.BinOp NotInHole UHExp.Comma _ _) surround) =>
        Some
          (mk_cursor_info
            (AnaOnly ty)
            (IsExpr e)
            side
            ctx)
      | UHExp.Tm NotInHole (UHExp.OpSeq (Skel.BinOp NotInHole UHExp.Plus _ _) _) 
      | UHExp.Tm NotInHole (UHExp.OpSeq (Skel.BinOp NotInHole UHExp.Times _ _) _)
      | UHExp.Tm NotInHole (UHExp.OpSeq (Skel.BinOp NotInHole UHExp.Space _ _) _)
      | UHExp.Tm NotInHole (UHExp.EmptyHole _)
      | UHExp.Tm NotInHole (UHExp.Asc _ _)
      | UHExp.Tm NotInHole (UHExp.Var NotInVHole _)
      | UHExp.Tm NotInHole (UHExp.NumLit _)
      | UHExp.Tm NotInHole (UHExp.BoolLit _)
      | UHExp.Tm NotInHole (UHExp.ApPalette _ _ _) => 
        match UHExp.syn fuel ctx e with
        | Some ty' =>
          if HTyp.consistent ty ty' then 
            Some (
              mk_cursor_info 
                (Subsumed ty ty')
                (IsExpr e)
                side
                ctx
            )
          else None
        | None => None
        end
      | UHExp.Tm NotInHole (UHExp.Lam _ ann _) => 
        match HTyp.matched_arrow ty with 
        | None => None 
        | Some (ty1_given, ty2) => 
          match ann with 
          | Some uty1 => 
            let ty1_ann := UHTyp.expand fuel uty1 in 
            match HTyp.consistent ty1_ann ty1_given with 
            | false => None
            | true => 
              Some
                (mk_cursor_info
                  (AnaAnnotatedLambda
                    ty
                    (HTyp.Arrow ty1_ann ty2))
                  (IsExpr e)
                  side
                  ctx)
            end
          | None => 
            Some 
              (mk_cursor_info 
                (AnaOnly ty)
                (IsExpr e)
                side
                ctx)
          end
        end
      | UHExp.Tm NotInHole (UHExp.Inj _ _) => 
        match ty with 
        | HTyp.Sum _ _ => 
          Some (
            mk_cursor_info
              (AnaOnly ty)
              (IsExpr e)
              side
              ctx
          )
        | _ => 
          match UHExp.syn fuel ctx e with 
          | Some ty' => 
            if HTyp.consistent ty ty' then 
              Some (
                mk_cursor_info
                  (Subsumed ty ty')
                  (IsExpr e)
                  side
                  ctx
              )
            else None
          | None => 
            Some 
              (mk_cursor_info
                (AnaOnly ty)
                (IsExpr e)
                side
                ctx
              )
          end
        end
      | UHExp.Tm NotInHole (UHExp.OpSeq (Skel.BinOp (InHole _) _ _ _) surround) => None
      | UHExp.Tm NotInHole (UHExp.OpSeq (Skel.Placeholder _ _) surround) => None
      end
      end.

    Fixpoint syn_cursor_info
      (fuel : Fuel.t) (ctx : Contexts.t) 
      (ze : t) : option(cursor_info) :=
      match fuel with 
      | Fuel.Kicked => None
      | Fuel.More fuel => 
      match ze with 
      | CursorE side (UHExp.Tm _ (UHExp.Var (InVHole _) _) as e) => 
        Some (
          mk_cursor_info
            SynFree
            (IsExpr e)
            side
            ctx
        )
      | CursorE side e => 
        match UHExp.syn fuel ctx e with 
        | Some ty => 
          Some (
            mk_cursor_info
              (SynOnly ty)
              (IsExpr e)
              side
              ctx
          )
        | None => None
        end
      | ParenthesizedZ ze1 => 
        syn_cursor_info fuel ctx ze1
      | Deeper _ ze1' => 
        syn_cursor_info' fuel ctx ze1'
      end
      end
    with ana_cursor_info
      (fuel : Fuel.t) (ctx : Contexts.t)
      (ze : t) (ty : HTyp.t) : option(cursor_info) := 
      match fuel with 
      | Fuel.Kicked => None
      | Fuel.More fuel => 
      match ze with 
      | CursorE side e =>
        ana_cursor_found fuel ctx e ty side
      | ParenthesizedZ ze1 => 
        ana_cursor_info fuel ctx ze1 ty 
      | Deeper (InHole u) ze1' => 
        syn_cursor_info' fuel ctx ze1'
      | Deeper NotInHole ze1' => 
        ana_cursor_info' fuel ctx ze1' ty 
      end
      end
    with syn_cursor_info'
      (fuel : Fuel.t) (ctx : Contexts.t) 
      (ze : t') : option(cursor_info) := 
      match fuel with 
      | Fuel.Kicked => None
      | Fuel.More fuel => 
      match ze with 
      | AscZ1 ze1 uty => 
        let ty := UHTyp.expand fuel uty in 
        let e1 := erase ze1 in 
        if UHExp.bidelimited e1 then 
          ana_cursor_info fuel ctx ze1 ty
        else None
      | AscZ2 e1 zty => 
        Some 
          (mk_cursor_info
            TypePosition
            IsType 
            Before (* TODO fix this once we use cursor info in type position! *)
            ctx)
      | LetZP zp ann e1 e2 => 
        match ann with 
        | Some uty1 => 
          let ty1 := UHTyp.expand fuel uty1 in 
          ana_pat_cursor_info fuel ctx zp ty1
        | None =>  
          match UHExp.syn fuel ctx e1 with 
          | None => None
          | Some ty1 => ana_pat_cursor_info fuel ctx zp ty1 
          end
        end
      | LetZA p zann e1 e2 => 
        Some
          (mk_cursor_info
            TypePosition
            IsType
            Before (* TODO fix this once we use cursor info in type position! *)
            ctx)
      | LetZE1 p ann ze1 e2 => 
        match ann with 
        | Some uty1 => 
          let ty1 := UHTyp.expand fuel uty1 in 
          let ctx1 := UHExp.ctx_for_let ctx p ty1 (erase ze1) in 
          ana_cursor_info fuel ctx1 ze1 ty1
        | None => syn_cursor_info fuel ctx ze1
        end
      | LetZE2 p ann e1 ze2 => 
        match ann with 
        | Some uty1 => 
          let ty1 := UHTyp.expand fuel uty1 in 
          match UHExp.ana_pat fuel ctx p ty1 with 
          | None => None
          | Some ctx2 => 
            syn_cursor_info fuel ctx2 ze2
          end
        | None => 
          match UHExp.syn fuel ctx e1 with 
          | None => None
          | Some ty1 => 
            match UHExp.ana_pat fuel ctx p ty1 with 
            | None => None
            | Some ctx2 => 
              syn_cursor_info fuel ctx2 ze2
            end
          end
        end
      | LamZP zp ann _ => 
        let ty1 := 
          match ann with 
          | Some uty1 => UHTyp.expand fuel uty1
          | None => HTyp.Hole
          end in 
        ana_pat_cursor_info fuel ctx zp ty1
      | LamZA _ zann _ => 
        Some
          (mk_cursor_info
            TypePosition
            IsType
            Before (* TODO fix this once we use cursor info in type position *)
            ctx)
      | LamZE p ann ze1 => 
        let ty1 := 
          match ann with 
          | Some uty1 => UHTyp.expand fuel uty1
          | None => HTyp.Hole
          end in 
        match UHExp.ana_pat fuel ctx p ty1 with 
        | None => None
        | Some ctx1 => 
          syn_cursor_info fuel ctx1 ze1
        end
      | InjZ side ze1 => 
        syn_cursor_info fuel ctx ze1
      | ListLitZ _ => None
      | CaseZE _ _
      | CaseZR _ _ => None
      | OpSeqZ skel ze0 surround => 
        let e0 := erase ze0 in 
        let seq := OperatorSeq.opseq_of_exp_and_surround e0 surround in 
        let n := OperatorSeq.surround_prefix_length surround in 
        syn_skel_cursor_info fuel ctx skel seq n ze0 
      | ApPaletteZ _ _ zholedata => 
        let (_, zholemap) := zholedata in 
        let (_, tz) := zholemap in 
        let (_, tz') := tz in
        let (ty, ze) := tz' in 
        ana_cursor_info fuel ctx ze ty 
      end
      end
    with ana_cursor_info'
      (fuel : Fuel.t) (ctx : Contexts.t) 
      (ze : t') (ty : HTyp.t) : option(cursor_info) := 
      match fuel with 
      | Fuel.Kicked => None
      | Fuel.More fuel => 
      match ze with 
      | LetZP zp ann e1 e2 => 
        match ann with 
        | Some uty1 => 
          let ty1 := UHTyp.expand fuel uty1 in 
          ana_pat_cursor_info fuel ctx zp ty1 
        | None =>  
          match UHExp.syn fuel ctx e1 with 
          | None => None
          | Some ty1 => 
            ana_pat_cursor_info fuel ctx zp ty1 
          end
        end
      | LetZA _ zann e1 e2 => 
        Some
          (mk_cursor_info
            TypePosition
            IsType
            Before (* TODO fix this once we use cursor info in type position! *)
            ctx)
      | LetZE1 p ann ze1 e2 => 
        match ann with 
        | Some uty1 => 
          let ty1 := UHTyp.expand fuel uty1 in 
          let ctx1 := UHExp.ctx_for_let ctx p ty1 (erase ze1) in 
          ana_cursor_info fuel ctx1 ze1 ty1
        | None => syn_cursor_info fuel ctx ze1
        end
      | LetZE2 p ann e1 ze2 => 
        match ann with 
        | Some uty1 => 
          let ty1 := UHTyp.expand fuel uty1 in 
          match UHExp.ana_pat fuel ctx p ty1 with 
          | None => None
          | Some ctx2 => 
            ana_cursor_info fuel ctx2 ze2 ty
          end
        | None => 
          match UHExp.syn fuel ctx e1 with 
          | None => None
          | Some ty1 => 
            match UHExp.ana_pat fuel ctx p ty1 with 
            | None => None
            | Some ctx2 => 
              ana_cursor_info fuel ctx2 ze2 ty
            end
          end
        end
      | LamZP p ann e => 
        match HTyp.matched_arrow ty with 
        | None => None
        | Some (ty1_given, ty2) => 
          let ty1 := 
            match ann with 
            | Some uty1 => UHTyp.expand fuel uty1
            | None => ty1_given
            end in 
          ana_pat_cursor_info fuel ctx p ty1
        end
      | LamZA _ zann _ => 
        Some
          (mk_cursor_info
            TypePosition
            IsType
            Before (* TODO fix this once we use cursor info in type position *)
            ctx)
      | LamZE p ann ze1 => 
        match HTyp.matched_arrow ty with 
        | None => None
        | Some (ty1_given, ty2) => 
          let ty1 := 
            match ann with 
            | Some uty1 => UHTyp.expand fuel uty1
            | None => ty1_given
            end in 
          match UHExp.ana_pat fuel ctx p ty1 with 
          | None => None
          | Some ctx => 
            ana_cursor_info fuel ctx ze1 ty2
          end
        end
      | InjZ side ze1 => 
        match HTyp.matched_sum ty with 
        | None => None
        | Some (ty1, ty2) => 
          ana_cursor_info fuel ctx ze1 
            (pick_side side ty1 ty2)
        end
      | ListLitZ zes => 
        match HTyp.matched_list ty with 
        | None => None
        | Some ty_elem => 
          let ze0 := ZList.prj_z zes in  
          ana_cursor_info fuel ctx ze0 ty_elem
        end
      | CaseZE ze1 rules => 
        syn_cursor_info fuel ctx ze1
      | CaseZR e1 zrules => 
        match UHExp.syn fuel ctx e1 with 
        | None => None
        | Some ty1 => 
          let zrule := ZList.prj_z zrules in 
          ana_rule_cursor_info fuel ctx zrule ty1 ty
        end
      | OpSeqZ ((Skel.BinOp NotInHole UHExp.Comma _ _) as skel) ze0 surround =>
        let e0 := erase ze0 in 
        let seq := OperatorSeq.opseq_of_exp_and_surround e0 surround in 
        let n := OperatorSeq.surround_prefix_length surround in 
        ana_skel_cursor_info fuel ctx skel seq n ze0 ty 
      | OpSeqZ (Skel.BinOp NotInHole UHExp.Plus _ _) _ _
      | OpSeqZ (Skel.BinOp NotInHole UHExp.Times _ _) _ _ 
      | OpSeqZ (Skel.BinOp NotInHole UHExp.Space _ _) _ _ 
      | AscZ1 _ _ 
      | AscZ2 _ _ 
      | ApPaletteZ _ _ _ => 
        syn_cursor_info' fuel ctx ze 
      | OpSeqZ (Skel.BinOp (InHole _) _ _ _) _ _
      | OpSeqZ (Skel.Placeholder _ _) _ _ => None
      end
      end
    with ana_rule_cursor_info
      (fuel : Fuel.t)
      (ctx : Contexts.t)
      (zrule : ZExp.zrule)
      (pat_ty : HTyp.t)
      (clause_ty : HTyp.t)
      : option(cursor_info) := 
      match fuel with 
      | Fuel.Kicked => None
      | Fuel.More fuel => 
      match zrule with 
      | RuleZP zp e => 
        ana_pat_cursor_info fuel ctx zp pat_ty
      | RuleZE p ze => 
        match UHExp.ana_pat fuel ctx p pat_ty with  
        | None => None
        | Some ctx => 
          ana_cursor_info fuel ctx ze clause_ty
        end
      end
      end
    with syn_skel_cursor_info
      (fuel : Fuel.t) (ctx : Contexts.t) 
      (skel : UHExp.skel_t) (seq : UHExp.opseq) 
      (n : nat) (ze_n : ZExp.t) : option(cursor_info) := 
      match fuel with 
      | Fuel.Kicked => None
      | Fuel.More fuel => 
      match skel with 
      | Skel.Placeholder _ n' => 
        if Nat.eqb n n' then 
          syn_cursor_info fuel ctx ze_n
        else None
      | Skel.BinOp _ UHExp.Plus skel1 skel2 
      | Skel.BinOp _ UHExp.Times skel1 skel2 => 
        match ana_skel_cursor_info fuel ctx skel1 seq n ze_n HTyp.Num with 
        | (Some _) as result => result
        | None =>
          match ana_skel_cursor_info fuel ctx skel2 seq n ze_n HTyp.Num with 
          | (Some _) as result => result
          | None => None
          end
        end
      | Skel.BinOp _ UHExp.Space ((Skel.Placeholder _ n') as skel1) skel2 => 
        if Nat.eqb n n' then 
          match cursor_on_outer_expr ze_n with 
          | Some ((UHExp.Tm (InHole u) e_n') as e_n, side) => 
            match UHExp.syn' fuel ctx e_n' with 
            | Some ty => Some 
                (mk_cursor_info
                  (SynErrorArrow (HTyp.Arrow HTyp.Hole HTyp.Hole) ty)
                  (IsExpr e_n)
                  side
                  ctx)
            | None => None
            end
          | Some (((UHExp.Tm _ (UHExp.Var (InVHole _) _)) as e_n), side) => 
            Some 
              (mk_cursor_info
                (SynFreeArrow (HTyp.Arrow HTyp.Hole HTyp.Hole))
                (IsExpr e_n)
                side
                ctx)
          | Some (e_n, side) => 
            match UHExp.syn fuel ctx e_n with 
            | Some ty => 
              match HTyp.matched_arrow ty with 
              | Some (ty1, ty2) => 
                Some 
                  (mk_cursor_info 
                    (SynMatchingArrow ty (HTyp.Arrow ty1 ty2))
                    (IsExpr e_n)
                    side
                    ctx)
              | None => None
              end
            | None => None
            end
          | None => 
            syn_cursor_info fuel ctx ze_n
          end
        else
          match UHExp.syn_skel fuel ctx skel1 seq None with 
          | None => None
          | Some (ty, _) => 
            match HTyp.matched_arrow ty with 
            | Some (ty1, ty2) => 
              ana_skel_cursor_info fuel ctx skel2 seq n ze_n ty1
            | None => None
            end
          end
      | Skel.BinOp _ UHExp.Space skel1 skel2 => 
        match syn_skel_cursor_info fuel ctx skel1 seq n ze_n with 
        | (Some _) as result => result
        | None => 
          match UHExp.syn_skel fuel ctx skel1 seq None with 
          | None => None
          | Some (ty, _) => 
            match HTyp.matched_arrow ty with 
            | None => None
            | Some (ty1, ty2) => 
              ana_skel_cursor_info fuel ctx skel2 seq n ze_n ty2
            end
          end
        end
      | Skel.BinOp _ UHExp.Comma skel1 skel2 => 
        match syn_skel_cursor_info fuel ctx skel1 seq n ze_n with 
        | (Some _) as result => result
        | None => syn_skel_cursor_info fuel ctx skel2 seq n ze_n
        end
      end
      end
    with ana_skel_cursor_info
      (fuel : Fuel.t) (ctx : Contexts.t) 
      (skel : UHExp.skel_t) (seq : UHExp.opseq)
      (n : nat) (ze_n : t) (ty : HTyp.t) : option(cursor_info) := 
      match fuel with 
      | Fuel.Kicked => None
      | Fuel.More fuel => 
      match skel with 
      | Skel.Placeholder _ n' => 
        if Nat.eqb n n' then 
          ana_cursor_info fuel ctx ze_n ty 
        else None
      | Skel.BinOp NotInHole UHExp.Comma skel1 skel2 => 
        match HTyp.matched_prod ty with 
        | None => None
        | Some (ty1, ty2) => 
          match ana_skel_cursor_info fuel ctx skel1 seq n ze_n ty1 with 
          | (Some _) as result => result
          | None => ana_skel_cursor_info fuel ctx skel2 seq n ze_n ty2 
          end
        end
      | Skel.BinOp (InHole _) UHExp.Comma _ _
      | Skel.BinOp _ UHExp.Plus _ _
      | Skel.BinOp _ UHExp.Times _ _
      | Skel.BinOp _ UHExp.Space _ _ =>  
        syn_skel_cursor_info fuel ctx skel seq n ze_n 
      end
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
      | ZTyp.ListZ zty1 => cons' O (of_ztyp zty1)
      | ZTyp.OpSeqZ _ zty1 surround => 
        let n := OperatorSeq.surround_prefix_length surround in 
        cons' n (of_ztyp zty1)
      end.

    Fixpoint of_zpat (zp : ZPat.t) : t := 
      match zp with 
      | ZPat.CursorP cursor_side _ => (nil, cursor_side)
      | ZPat.Deeper _ zp' => of_zpat' zp'
      | ZPat.ParenthesizedZ zp1 => cons' 0 (of_zpat zp1)
      end
    with of_zpat' (zp' : ZPat.t') : t := 
      match zp' with 
      | ZPat.InjZ _ zp1 => cons' 0 (of_zpat zp1)
      | ZPat.ListLitZ zps => 
        let prefix_length := ZList.prefix_length zps in 
        let zp0 := ZList.prj_z zps in 
        cons' prefix_length (of_zpat zp0)
      | ZPat.OpSeqZ _ zp1 surround => 
        let n := OperatorSeq.surround_prefix_length surround in 
        cons' n (of_zpat zp1)
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
      | ZExp.AscZ2 _ zty => cons' 1 (of_ztyp zty)
      | ZExp.LetZP zp _ _ _ => cons' O (of_zpat zp) 
      | ZExp.LetZA _ zann _ _ => cons' 1 (of_ztyp zann)
      | ZExp.LetZE1 _ _ ze1 _ => cons' 2 (of_zexp ze1) 
      | ZExp.LetZE2 _ _ _ ze2 => cons' 3 (of_zexp ze2)
      | ZExp.LamZP zp _ _ => cons' O (of_zpat zp)
      | ZExp.LamZA _ zann _ => cons' 1 (of_ztyp zann)
      | ZExp.LamZE _ ann ze' => cons' 2 (of_zexp ze')
      | ZExp.InjZ _ ze' => cons' O (of_zexp ze')
      | ZExp.ListLitZ zes => 
        let prefix_length := ZList.prefix_length zes in 
        let ze0 := ZList.prj_z zes in 
        cons' prefix_length (of_zexp ze0)
      | ZExp.CaseZE ze1 _ => cons' O (of_zexp ze1)
      | ZExp.CaseZR _ zrules => 
        let prefix_len := List.length (ZList.prj_prefix zrules) in 
        let zrule := ZList.prj_z zrules in 
        cons' (S prefix_len) (of_zrule zrule) 
      | ZExp.OpSeqZ _ ze' surround => 
        let n := OperatorSeq.surround_prefix_length surround in 
        cons' n (of_zexp ze')
      | ZExp.ApPaletteZ _ _ zholedata => 
        let (_, zholemap) := zholedata in 
        let (_, tz) := zholemap in 
        let (n, tz') := tz in
        let (_, ze') := tz' in 
        cons' n (of_zexp ze')
      end
    with of_zrule (zrule : ZExp.zrule) : t := 
      match zrule with 
      | ZExp.RuleZP zp _ => cons' O (of_zpat zp)
      | ZExp.RuleZE _ ze => cons' 1 (of_zexp ze)
      end.

    Definition of_OpSeqZ (ze : ZExp.t) (surround : ZExp.opseq_surround) := 
      let n := OperatorSeq.surround_prefix_length surround in 
      cons' n (of_zexp ze).

    Definition of_OpSeqZ_pat (zp : ZPat.t) (surround : ZPat.opseq_surround) := 
      let n := OperatorSeq.surround_prefix_length surround in 
      cons' n (of_zpat zp).

    Fixpoint follow_ty (fuel : Fuel.t) (path : t) (uty : UHTyp.t) : option(ZTyp.t) := 
      match fuel with 
      | Fuel.Kicked => None
      | Fuel.More fuel => 
      match path with
      | (nil, cursor_side) => Some (ZTyp.CursorT cursor_side uty)
      | (cons x xs, cursor_side) => 
        match uty with 
        | UHTyp.Hole
        | UHTyp.Num
        | UHTyp.Bool => None
        | UHTyp.Parenthesized uty1 => 
          match x with 
          | O => 
            match follow_ty fuel (xs, cursor_side) uty1 with 
            | Some zty => Some (ZTyp.ParenthesizedZ zty)
            | None => None
            end
          | _ => None
          end
        | UHTyp.List uty1 => 
          match x with 
          | O => 
            match follow_ty fuel (xs, cursor_side) uty1 with 
            | None => None
            | Some zty => Some (ZTyp.ListZ zty)
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

    Fixpoint follow_pat (fuel : Fuel.t) (path : t) (p : UHPat.t) : option(ZPat.t) := 
      match fuel with 
      | Fuel.Kicked => None
      | Fuel.More fuel => 
      let follow_pat := follow_pat fuel in  
      match path with 
      | (nil, cursor_side) => Some (ZPat.CursorP cursor_side p)
      | (cons x xs, cursor_side) => 
        match p with 
        | UHPat.Parenthesized p1 => 
          match x with 
          | 0 => 
            match follow_pat (xs, cursor_side) p1 with 
            | None => None
            | Some zp1 => Some (ZPat.ParenthesizedZ zp1)
            end
          | _ => None
          end
        | UHPat.Pat err_status p' => 
          match (x, p') with 
          | (_, UHPat.EmptyHole _)
          | (_, UHPat.Wild)
          | (_, UHPat.Var _)
          | (_, UHPat.NumLit _)
          | (_, UHPat.BoolLit _) => None
          | (n, UHPat.ListLit ps) => 
            match ZList.split_at n ps with 
            | None => None
            | Some psz => 
              match ZList.optmap_z (follow_pat (xs, cursor_side)) psz with 
              | None => None
              | Some zps => 
                Some (ZPat.Deeper err_status (ZPat.ListLitZ zps))
              end
            end
          | (0, UHPat.Inj side p1) => 
            match follow_pat (xs, cursor_side) p1 with 
            | None => None
            | Some zp1 => Some (ZPat.Deeper err_status (ZPat.InjZ side zp1))
            end
          | (_, UHPat.Inj _ _) => None
          | (n, UHPat.OpSeq skel seq) => 
            match OperatorSeq.split n seq with 
            | None => None
            | Some (p, surround) => 
                match follow_pat (xs, cursor_side) p with 
                | Some zp => 
                    Some (ZPat.Deeper err_status (ZPat.OpSeqZ skel zp surround))
                | None => None
                end
            end
          end
        end
      end
      end.

    Fixpoint follow_e (fuel : Fuel.t) (path : t) (e : UHExp.t) : option(ZExp.t) := 
      match fuel with 
      | Fuel.Kicked => None
      | Fuel.More fuel => 
      let follow_e := follow_e fuel in  
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
          | (_, UHExp.EmptyHole _) => None
          | (O, UHExp.Asc e1 ty) => 
            match follow_e (xs, cursor_side) e1 with 
            | Some ze => Some (ZExp.Deeper err_status (ZExp.AscZ1 ze ty))
            | None => None
            end
          | (1, UHExp.Asc e1 ty) => 
            match follow_ty fuel (xs, cursor_side) ty with 
            | Some ztau => Some (ZExp.Deeper err_status (ZExp.AscZ2 e1 ztau))
            | None => None
            end
          | (_, UHExp.Asc _ _) => None
          | (_, UHExp.Var _ _) => None
          | (O, UHExp.Let p ann e1  e2) => 
            match follow_pat fuel (xs, cursor_side) p with 
            | None => None
            | Some zp => 
              Some (ZExp.Deeper err_status (ZExp.LetZP zp ann e1 e2))
            end
          | (1, UHExp.Let p ann e1 e2) => 
            match ann with 
            | None => None
            | Some ann_ty => 
              match follow_ty fuel (xs, cursor_side) ann_ty with 
              | None => None
              | Some zann => Some (ZExp.Deeper err_status (ZExp.LetZA p zann e1 e2)) 
              end
            end
          | (2, UHExp.Let p ann e1 e2) => 
            match follow_e (xs, cursor_side) e1 with 
            | Some ze1 => Some (ZExp.Deeper err_status (ZExp.LetZE1 p ann ze1 e2))
            | None => None
            end
          | (3, UHExp.Let p ann e1 e2) => 
            match follow_e (xs, cursor_side) e2 with 
            | Some ze2 => Some (ZExp.Deeper err_status (ZExp.LetZE2 p ann e1 ze2))
            | None => None
            end
          | (_, UHExp.Let _ _ _ _) => None
          | (O, UHExp.Lam p ann e1) => 
            match follow_pat fuel (xs, cursor_side) p with 
            | None => None
            | Some zp => 
              Some (ZExp.Deeper err_status (ZExp.LamZP zp ann e1))
            end
          | (1, UHExp.Lam p ann e1) => 
            match ann with 
            | None => None
            | Some ann_ty => 
              match follow_ty fuel (xs, cursor_side) ann_ty with 
              | None => None
              | Some zann => 
                Some (ZExp.Deeper err_status (ZExp.LamZA p zann e1))
              end
            end
          | (2, UHExp.Lam p ann e1) => 
            match follow_e (xs, cursor_side) e1 with 
            | None => None
            | Some ze => Some (ZExp.Deeper err_status (ZExp.LamZE p ann ze))
            end
          | (_, UHExp.Lam _ _ _) => None
          | (_, UHExp.NumLit _) => None
          | (_, UHExp.BoolLit _) => None
          | (O, UHExp.Inj side e1) => 
            match follow_e (xs, cursor_side) e1 with 
            | Some ze => Some (ZExp.Deeper err_status (ZExp.InjZ side ze))
            | None => None
            end
          | (_, UHExp.Inj _ _) => None
          | (n, UHExp.ListLit es) => 
            match ZList.split_at n es with 
            | None => None
            | Some esz => 
              match ZList.optmap_z (follow_e (xs, cursor_side)) esz with 
              | None => None
              | Some zes => 
                Some (ZExp.Deeper err_status (ZExp.ListLitZ zes))
              end
            end
          | (O, UHExp.Case e1 rules) => 
            match follow_e (xs, cursor_side) e1 with 
            | Some ze => Some (ZExp.Deeper err_status (ZExp.CaseZE ze rules))
            | None => None
            end
          | (S x, UHExp.Case e1 rules) => 
            match ZList.split_at x rules with 
            | None => None
            | Some split_rules => 
              match ZList.optmap_z (follow_rule fuel (xs, cursor_side)) split_rules with 
              | None => None
              | Some zrules => 
                Some (ZExp.Deeper err_status (ZExp.CaseZR e1 zrules))
              end
            end
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
          | (hole_ref, UHExp.ApPalette name serialized_model hole_data) => 
            let (next_hole_ref, holemap) := hole_data in 
            match NatMap.drop holemap hole_ref with
            | None => None
            | Some (holemap', te) =>
              let (ty, e') := te in 
              match follow_e (xs, cursor_side) e' with 
              | None => None
              | Some ze => 
                let zholemap := (holemap', (hole_ref, (ty, ze))) in 
                let zholedata := (next_hole_ref, zholemap) in 
                Some (ZExp.Deeper NotInHole (ZExp.ApPaletteZ name serialized_model zholedata))
              end
            end
          end
        end
      end
      end
    with follow_rule (fuel : Fuel.t) (path : t) (rule : UHExp.rule) : option(ZExp.zrule) := 
      match fuel with 
      | Fuel.Kicked => None
      | Fuel.More fuel => 
      match rule with 
      | UHExp.Rule p e => 
        match path with 
        | (nil, _) => None
        | (cons 0 xs, cursor_side) => 
          match follow_pat fuel (xs, cursor_side) p with 
          | None => None
          | Some zp => Some (ZExp.RuleZP zp e)
          end
        | (cons 1 xs, cursor_side) => 
          match follow_e fuel (xs, cursor_side) e with 
          | None => None
          | Some ze => Some (ZExp.RuleZE p ze)
          end
        | (cons _ _, _) => None
        end
      end
      end.

    Definition cons_opt (n : nat) (x : option(list(nat))) : option(list(nat)) := 
      match x with 
      | None => None
      | Some xs => Some (cons n xs)
      end.

    Definition cons_opt2 
      (n1 : nat) (x1 : option(list(nat)))
      (n2 : nat) (x2 : unit -> option(list(nat)))
      : option(list(nat)) := 
        match x1 with 
        | Some xs => Some (cons n1 xs)
        | None => 
          match x2 tt with 
          | Some xs => Some (cons n2 xs)
          | None => None
          end
        end.

    Definition cons_opt3 
      (n1 : nat) (x1 : option(list(nat)))
      (n2 : nat) (x2 : unit -> option(list(nat)))
      (n3 : nat) (x3 : unit -> option(list(nat)))
      : option(list(nat)) := 
        match x1 with 
        | Some xs => Some (cons n1 xs)
        | None => 
          match x2 tt with 
          | Some xs => Some (cons n2 xs)
          | None => 
            match x3 tt with 
            | Some xs => Some (cons n3 xs)
            | None => None
            end
          end
        end.

    Fixpoint steps_to_hole_pat (fuel : Fuel.t) (p : UHPat.t) (u : MetaVar.t) : option(list(nat)) := 
      match fuel with 
      | Fuel.Kicked => None
      | Fuel.More fuel => 
      match p with 
      | UHPat.Pat _ (UHPat.EmptyHole u') => 
        if MetaVar.eq u u' then 
          Some nil
        else None
      | UHPat.Parenthesized p1 => 
        cons_opt 0 (steps_to_hole_pat fuel p1 u)
      | UHPat.Pat _ UHPat.Wild
      | UHPat.Pat _ (UHPat.Var _) 
      | UHPat.Pat _ (UHPat.NumLit _) 
      | UHPat.Pat _ (UHPat.BoolLit _) => None
      | UHPat.Pat _ (UHPat.ListLit ps) => 
        Util.findmapi ps (fun i p => 
          match steps_to_hole_pat fuel p u with 
          | None => None
          | Some ns => Some (cons i ns)
          end)
      | UHPat.Pat _ (UHPat.Inj _ p1) => 
        cons_opt 0 (steps_to_hole_pat fuel p1 u)
      | UHPat.Pat _ (UHPat.OpSeq skel seq) => 
        steps_to_hole_seq_pat fuel seq u  
      end
      end
    with steps_to_hole_seq_pat (fuel : Fuel.t) (seq : UHPat.opseq) (u : MetaVar.t) : option(list(nat)) :=  
      match fuel with 
      | Fuel.Kicked => None
      | Fuel.More fuel => 
      match seq with 
      | OperatorSeq.ExpOpExp p1 _ p2 => 
        cons_opt2
          0 (steps_to_hole_pat fuel p1 u)
          1 (fun _ => steps_to_hole_pat fuel p2 u)
      | OperatorSeq.SeqOpExp seq1 op p1 => 
        match steps_to_hole_seq_pat fuel seq1 u with 
        | (Some steps) as path => path
        | None => cons_opt (OperatorSeq.seq_length seq1) (steps_to_hole_pat fuel p1 u)
        end
      end
      end.

    Fixpoint steps_to_hole (fuel : Fuel.t) (e : UHExp.t) (u : MetaVar.t) : option(list(nat)) := 
      match fuel with 
      | Fuel.Kicked => None
      | Fuel.More fuel => 
      match e with 
      | UHExp.Tm _ (UHExp.EmptyHole u') => 
        if MetaVar.eq u u' then 
          Some nil
        else None
      | UHExp.Parenthesized e1 =>   
        cons_opt O (steps_to_hole fuel e1 u)
      | UHExp.Tm _ (UHExp.Var _ _)
      | UHExp.Tm _ (UHExp.NumLit _)
      | UHExp.Tm _ (UHExp.BoolLit _) => None
      | UHExp.Tm _ (UHExp.Asc e1 _)  
      | UHExp.Tm _ (UHExp.Inj _ e1) => 
        cons_opt O (steps_to_hole fuel e1 u)
      | UHExp.Tm _ (UHExp.ListLit es) => 
        Util.findmapi es (fun i e => 
          match steps_to_hole fuel e u with 
          | None => None
          | Some ns => Some (cons i ns)
          end)
      | UHExp.Tm _ (UHExp.Lam p _ e1) => 
        cons_opt2 
          0 (steps_to_hole_pat fuel p u)
          2 (fun _ => steps_to_hole fuel e1 u)
      | UHExp.Tm _ (UHExp.Let p ann e1 e2) => 
        cons_opt3 
          0 (steps_to_hole_pat fuel p u)
          2 (fun _ => steps_to_hole fuel e1 u)
          3 (fun _ => steps_to_hole fuel e2 u)
      | UHExp.Tm _ (UHExp.Case e1 rules) => 
        match steps_to_hole fuel e1 u with 
        | Some steps => Some (cons 0 steps) 
        | None => 
          Util.findmapi rules (
            fun i rule => 
              match rule with 
              | UHExp.Rule p e => 
                match steps_to_hole_pat fuel p u with 
                | Some steps => Some (cons (S i) (cons 0 steps))
                | None => 
                  match steps_to_hole fuel e u with 
                  | Some steps => Some (cons (S i) (cons 1 steps))
                  | None => None
                  end
                end
              end) 
        end
      | UHExp.Tm _ (UHExp.OpSeq skel seq) => 
        steps_to_hole_seq fuel seq u  
      | UHExp.Tm _ (UHExp.ApPalette _ _ holedata) => 
        let (_, holemap) := holedata in 
        NatMap.fold holemap (fun c v => 
          match c with 
          | Some _ => c
          | None => 
            let (_, te) := v in
            let (_, e) := te in 
            steps_to_hole fuel e u 
          end
        ) None
      end
      end
    with steps_to_hole_seq (fuel : Fuel.t) (seq : UHExp.opseq) (u : MetaVar.t) : option(list(nat)) :=  
      match fuel with 
      | Fuel.Kicked => None
      | Fuel.More fuel => 
      match seq with 
      | OperatorSeq.ExpOpExp e1 _ e2 => 
        cons_opt2
          0 (steps_to_hole fuel e1 u)
          1 (fun _ => steps_to_hole fuel e2 u)
      | OperatorSeq.SeqOpExp seq1 op e1 => 
        match steps_to_hole_seq fuel seq1 u with 
        | (Some steps) as path => path
        | None => cons_opt (OperatorSeq.seq_length seq1) (steps_to_hole fuel e1 u)
        end
      end
      end.

    Definition path_to_hole (fuel : Fuel.t) (e : UHExp.t) (u : MetaVar.t) : option(t) := 
      match steps_to_hole fuel e u with 
      | Some steps => Some (steps, Before)
      | None => None
      end.

    Fixpoint first_hole_steps_ty (fuel : Fuel.t) (uty : UHTyp.t) : option (list nat) :=
      match fuel with
      | Fuel.Kicked => None
      | Fuel.More fuel =>
      match uty with
      | UHTyp.Parenthesized uty' => cons_opt O (first_hole_steps_ty fuel uty')
      | UHTyp.Num 
      | UHTyp.Bool => None
      | UHTyp.Hole => Some nil
      | UHTyp.List uty1 => cons_opt 0 (first_hole_steps_ty fuel uty1)
      | UHTyp.OpSeq _ opseq => first_hole_steps_ty_opseq fuel opseq 0
      end
      end
    (* return an optional path of the first hole in opseq starting with the nth term *)
    with first_hole_steps_ty_opseq (fuel : Fuel.t) (opseq : OperatorSeq.opseq UHTyp.t UHTyp.op) (n : nat) : option (list nat) :=
      match fuel with
      | Fuel.Kicked => None
      | Fuel.More fuel =>
        if Nat.leb (OperatorSeq.seq_length opseq) n
        then None
        else
          match OperatorSeq.seq_nth n opseq with
          | None => None (* degenerate case *)
          | Some uty' =>
            match first_hole_steps_ty fuel uty' with
            | Some ns => Some (cons n ns)
            | None => first_hole_steps_ty_opseq fuel opseq (n+1)
            end
          end
      end.

    Fixpoint first_hole_steps_pat (fuel : Fuel.t) (p : UHPat.t) : option(list nat) := 
      match fuel with
      | Fuel.Kicked => None
      | Fuel.More fuel =>
      match p with 
      | UHPat.Parenthesized p1 => cons_opt 0 (first_hole_steps_pat fuel p1)
      | UHPat.Pat _ (UHPat.EmptyHole _) => Some nil
      | UHPat.Pat _ UHPat.Wild
      | UHPat.Pat _ (UHPat.Var _)
      | UHPat.Pat _ (UHPat.NumLit _) 
      | UHPat.Pat _ (UHPat.BoolLit _) => None
      | UHPat.Pat _ (UHPat.Inj _ p1) => cons_opt 0 (first_hole_steps_pat fuel p1)
      | UHPat.Pat _ (UHPat.ListLit ps) => 
        Util.findmapi ps (fun i p => 
          match first_hole_steps_pat fuel p with 
          | None => None
          | Some ns => Some (cons i ns)
          end)
      | UHPat.Pat _ (UHPat.OpSeq _ seq) => first_hole_steps_opseq_pat fuel seq 0
      end
      end
    with first_hole_steps_opseq_pat (fuel : Fuel.t) (opseq : UHPat.opseq) (n : nat) : option (list nat) :=
      match fuel with
      | Fuel.Kicked => None
      | Fuel.More fuel =>
        if Nat.leb (OperatorSeq.seq_length opseq) n
        then None
        else
          match OperatorSeq.seq_nth n opseq with
          | None => None
          | Some ue =>
            match first_hole_steps_pat fuel ue with
            | Some ns => Some (cons n ns)
            | None => first_hole_steps_opseq_pat fuel opseq (n+1)
            end
          end
      end.

    Fixpoint first_hole_steps (fuel : Fuel.t) (ue : UHExp.t) : option (list nat) :=
      match fuel with
      | Fuel.Kicked => None
      | Fuel.More fuel =>
      match ue with
      | UHExp.Parenthesized ue1 => cons_opt 0 (first_hole_steps fuel ue1)
      | UHExp.Tm _ ue' =>
        match ue' with
        | UHExp.EmptyHole _ => Some nil
        | UHExp.Asc ue1 uty =>
          cons_opt2 
            0 (first_hole_steps fuel ue1)
            1 (fun _ => first_hole_steps_ty fuel uty)
        | UHExp.Var _ _ => None
        | UHExp.Let p ann ue1 ue2 =>
          match first_hole_steps_pat fuel p with 
          | Some ns => Some (cons 0 ns)
          | None => 
            match ann with 
            | Some ann_ty => 
              cons_opt3 
                1 (first_hole_steps_ty fuel ann_ty)
                2 (fun _ => first_hole_steps fuel ue1)
                3 (fun _ => first_hole_steps fuel ue2)
            | None => 
              cons_opt2
                2 (first_hole_steps fuel ue1)
                3 (fun _ => first_hole_steps fuel ue2)
            end
          end
        | UHExp.Lam p ann e1 => 
          match first_hole_steps_pat fuel p with 
          | Some ns => Some (cons 0 ns)
          | None => 
            match ann with 
            | Some uty => 
              cons_opt2 
                1 (first_hole_steps_ty fuel uty)
                2 (fun _ => first_hole_steps fuel e1)
            | None => cons_opt 2 (first_hole_steps fuel e1)
            end
          end
        | UHExp.NumLit _ => None
        | UHExp.BoolLit _ => None
        | UHExp.ListLit es => 
          Util.findmapi es (fun i e => 
            match first_hole_steps fuel e with 
            | None => None
            | Some ns => Some (cons i ns)
            end)
        | UHExp.Inj _ e1 => cons_opt 0 (first_hole_steps fuel e1)
        | UHExp.Case e1 rules =>
          match first_hole_steps fuel e1 with
          | Some ns => Some (cons 0 ns)
          | None => first_hole_steps_rules fuel rules
          end
        | UHExp.OpSeq _ opseq => first_hole_steps_opseq fuel opseq 0
        | UHExp.ApPalette _ _ _ => None (* TODO figure out tab order protocol *)
        end
      end
      end
    with first_hole_steps_rules (fuel : Fuel.t) (rules : UHExp.rules) : option (list nat) := 
      match fuel with
      | Fuel.Kicked => None
      | Fuel.More fuel =>
        Util.findmapi rules (
          fun i rule => 
            match rule with 
            | UHExp.Rule p e => 
              match first_hole_steps_pat fuel p with 
              | Some ns => Some (cons (S i) (cons 0 ns))
              | None => 
                match first_hole_steps fuel e with 
                | Some ns => Some (cons (S i) (cons 1 ns))
                | None => None
                end
              end
            end)
      end
    (* return an optional path of the first hole in opseq starting with the nth term )*)
    with first_hole_steps_opseq (fuel : Fuel.t) (opseq : OperatorSeq.opseq UHExp.t UHExp.op) (n : nat) : option (list nat) :=
      match fuel with
      | Fuel.Kicked => None
      | Fuel.More fuel =>
        if Nat.leb (OperatorSeq.seq_length opseq) n
        then None
        else
          match OperatorSeq.seq_nth n opseq with
          | None => None
          | Some ue =>
            match first_hole_steps fuel ue with
            | Some ns => Some (cons n ns)
            | None => first_hole_steps_opseq fuel opseq (n+1)
            end
          end
      end.

    Fixpoint next_hole_steps_ty (fuel : Fuel.t) (zty : ZTyp.t) : option (list nat) :=
      match fuel with
      | Fuel.Kicked => None
      | Fuel.More fuel =>
      match zty with
      | ZTyp.CursorT cursor_side uty =>
        match cursor_side, uty with
        | _, UHTyp.Hole => None
        | Before, _ => first_hole_steps_ty fuel uty
        | After, _ => None
        | In _, _ => None
        end
      | ZTyp.ParenthesizedZ zty' => cons_opt 0 (next_hole_steps_ty fuel zty')
      | ZTyp.ListZ zty1 => cons_opt 0 (next_hole_steps_ty fuel zty1) 
      | ZTyp.OpSeqZ _ zty' surround =>
        let n := OperatorSeq.surround_prefix_length surround in
        match next_hole_steps_ty fuel zty' with
        | Some ns => Some (cons n ns)
        | None =>
          let uty' := ZTyp.erase zty' in
          let opseq := OperatorSeq.opseq_of_exp_and_surround uty' surround in
          first_hole_steps_ty_opseq fuel opseq (n+1)
        end
      end
      end.

    Fixpoint next_hole_path_ty (fuel : Fuel.t) (zty : ZTyp.t) : option Path.t :=
      match fuel with
      | Fuel.Kicked => None
      | Fuel.More fuel =>
      match next_hole_steps_ty fuel zty with
      | None => None
      | Some path => Some (path, Before)
      end
      end.

    Fixpoint next_hole_steps_pat (fuel : Fuel.t) (zp : ZPat.t) : option(list nat) := 
      match fuel with
      | Fuel.Kicked => None
      | Fuel.More fuel =>
      match zp with 
      | ZPat.ParenthesizedZ zp1 => cons_opt 0 (next_hole_steps_pat fuel zp1)
      | ZPat.CursorP cursor_side p => 
        match cursor_side, p with 
        | _, (UHPat.Pat _ (UHPat.EmptyHole _)) => None
        | After, _ => None
        | Before, _ => first_hole_steps_pat fuel p
        | In k, _ => 
          match p with 
          | UHPat.Parenthesized _ => None
          | UHPat.Pat err p' => 
            match p' with 
            | UHPat.Wild
            | UHPat.Var _
            | UHPat.NumLit _
            | UHPat.BoolLit _ 
            | UHPat.ListLit _
            | UHPat.OpSeq _ _ => None
            | UHPat.Inj _ p1 => first_hole_steps_pat fuel p1 
            | UHPat.EmptyHole _ => None
            end
          end
        end
      | ZPat.Deeper _ (ZPat.InjZ _ zp1) => cons_opt 0 (next_hole_steps_pat fuel zp1)
      | ZPat.Deeper _ (ZPat.ListLitZ zps) => 
        let prefix_length := ZList.prefix_length zps in 
        let zp0 := ZList.prj_z zps in 
        match next_hole_steps_pat fuel zp0 with 
        | Some ns => 
          Some (cons prefix_length ns)
        | None =>
          let suffix := ZList.prj_suffix zps in 
          Util.findmapi suffix (fun i p => 
            match first_hole_steps_pat fuel p with 
            | None => None
            | Some ns => Some (cons (prefix_length + i + 1) ns)
            end)
        end
      | ZPat.Deeper _ (ZPat.OpSeqZ _ zp1 surround) =>
        let n := OperatorSeq.surround_prefix_length surround in
        match next_hole_steps_pat fuel zp1 with
        | Some ns => Some (cons n ns)
        | None =>
          let p := ZPat.erase zp1 in
          let opseq := OperatorSeq.opseq_of_exp_and_surround p surround in
          first_hole_steps_opseq_pat fuel opseq (n+1)
        end
      end
      end.

    Fixpoint next_hole_path_pat (fuel : Fuel.t) (zp : ZPat.t) : option Path.t :=
      match fuel with
      | Fuel.Kicked => None
      | Fuel.More fuel =>
      match next_hole_steps_pat fuel zp with
      | None => None
      | Some path => Some (path, Before)
      end
      end.

    Fixpoint next_hole_steps (fuel : Fuel.t) (ze : ZExp.t) : option (list nat) :=
      match fuel with
      | Fuel.Kicked => None
      | Fuel.More fuel =>
      match ze with
      | ZExp.CursorE cursor_side ue =>
        match cursor_side, ue with
        | _, (UHExp.Tm _ (UHExp.EmptyHole _)) => None
        | After, _ => None
        | Before, _ => first_hole_steps fuel ue
        | In k, _ =>
          match ue with
          | UHExp.Parenthesized _ => None
          | UHExp.Tm err ue' =>
            match ue' with
            | UHExp.Asc _ uty => cons_opt 1 (first_hole_steps_ty fuel uty)
            | UHExp.Var _ _ => None
            | UHExp.Let p ann ue1 ue2 => 
              first_hole_steps fuel ue 
            | UHExp.Lam _ _ _ => 
              first_hole_steps fuel ue
            | UHExp.NumLit _
            | UHExp.BoolLit _
            | UHExp.ListLit _ => None
            | UHExp.Inj _ ue'' => 
              first_hole_steps fuel ue 
            | UHExp.Case e1 rules => 
              match k with 
              | 0 => first_hole_steps fuel ue
              | 1 => None
              | _ => None
              end
            | UHExp.EmptyHole _ => None
            | UHExp.OpSeq _ _ => None
            | UHExp.ApPalette _ _ _ => None (* TODO move into palette holes *)
            end
          end
        end
      | ZExp.Deeper _ ze' =>
        match ze' with
        | ZExp.AscZ1 ze'' uty =>
          cons_opt2
            0 (next_hole_steps fuel ze'')
            1 (fun _ => first_hole_steps_ty fuel uty)
        | ZExp.AscZ2 _ zty => cons_opt 1 (next_hole_steps_ty fuel zty)
        | ZExp.LetZP zp ann ue1 ue2 => 
          match next_hole_steps_pat fuel zp with 
          | Some ns => Some (cons 0 ns)
          | None => 
            match ann with 
            | Some ann_ty => 
              cons_opt3 
                1 (first_hole_steps_ty fuel ann_ty) 
                2 (fun _ => first_hole_steps fuel ue1) 
                3 (fun _ => first_hole_steps fuel ue2)
            | None => 
              cons_opt2 
                2 (first_hole_steps fuel ue1)
                3 (fun _ => first_hole_steps fuel ue2)
            end
          end
        | ZExp.LetZA _ zann e1 e2 => 
          cons_opt3 
            1 (next_hole_steps_ty fuel zann)
            2 (fun _ => first_hole_steps fuel e1) 
            3 (fun _ => first_hole_steps fuel e2)
        | ZExp.LetZE1 _ _ ze1 e2 =>
          cons_opt2
            2 (next_hole_steps fuel ze1)
            3 (fun _ => first_hole_steps fuel e2)
        | ZExp.LetZE2 _ _ _ ze2 => 
          cons_opt 
            3 (next_hole_steps fuel ze2)
        | ZExp.LamZP zp ann e1 => 
          match next_hole_steps_pat fuel zp with 
          | Some ns => Some (cons 0 ns)
          | None => 
            match ann with 
            | Some uty => 
              cons_opt2 
                1 (first_hole_steps_ty fuel uty)
                2 (fun _ => first_hole_steps fuel e1)
            | None => 
               cons_opt 
                 2 (first_hole_steps fuel e1)
            end
          end
        | ZExp.LamZA _ zann e1 => 
          cons_opt2
            1 (next_hole_steps_ty fuel zann)
            2 (fun _ => first_hole_steps fuel e1)
        | ZExp.LamZE _ _ ze1 => 
          cons_opt 
            2 (next_hole_steps fuel ze1)
        | ZExp.InjZ _ ze'' => 
          cons_opt 
            0 (next_hole_steps fuel ze'')
        | ZExp.ListLitZ zes => 
          let prefix_length := ZList.prefix_length zes in 
          let ze0 := ZList.prj_z zes in 
          match next_hole_steps fuel ze0 with 
          | Some ns => 
            Some (cons prefix_length ns)
          | None =>
            let suffix := ZList.prj_suffix zes in 
            Util.findmapi suffix (fun i e => 
              match first_hole_steps fuel e with 
              | None => None
              | Some ns => Some (cons (prefix_length + i + 1) ns)
              end)
          end
        | ZExp.CaseZE ze1 rules =>
          match next_hole_steps fuel ze1 with
          | Some ns => Some (cons 0 ns)
          | None => first_hole_steps_rules fuel rules
          end
        | ZExp.CaseZR _ zrules => 
          let zr := ZList.prj_z zrules in 
          let prefix_len := List.length (ZList.prj_prefix zrules) in 
          match zr with 
          | ZExp.RuleZP zp e => 
            match next_hole_steps_pat fuel zp with 
            | Some ns => Some (cons (S prefix_len) (cons 0 ns))
            | None => 
              match first_hole_steps fuel e with 
              | Some ns => Some (cons (S prefix_len) (cons 1 ns))
              | None => 
                let suffix := ZList.prj_suffix zrules in 
                match first_hole_steps_rules fuel suffix with 
                | Some (cons offset ns) => Some (cons (prefix_len + offset + 1) ns)
                | Some nil => None (* should never happen *)
                | None => None
                end
              end
            end
          | ZExp.RuleZE _ ze => 
            match next_hole_steps fuel ze with 
            | Some ns => Some (cons (S prefix_len) (cons 1 ns))
            | None => 
              let suffix := ZList.prj_suffix zrules in 
              match first_hole_steps_rules fuel suffix with 
              | Some (cons offset ns) => Some (cons (prefix_len + offset + 1) ns)
              | Some nil => None (* should never happen *)
              | None => None
              end
            end
          end
        | ZExp.OpSeqZ _ ze'' surround =>
          let n := OperatorSeq.surround_prefix_length surround in
          match next_hole_steps fuel ze'' with
          | Some ns => Some (cons n ns)
          | None =>
            let ue'' := ZExp.erase ze'' in
            let opseq := OperatorSeq.opseq_of_exp_and_surround ue'' surround in
            first_hole_steps_opseq fuel opseq (n+1)
          end
        | ZExp.ApPaletteZ _ _ _ => None (* TODO figure out tab order protocol *)
        end
      | ZExp.ParenthesizedZ ze' => cons_opt 0 (next_hole_steps fuel ze')
      end
      end.

    Fixpoint next_hole_path (fuel : Fuel.t) (ze : ZExp.t) : option Path.t :=
      match fuel with
      | Fuel.Kicked => None
      | Fuel.More fuel =>
      match next_hole_steps fuel ze with
      | None => None
      | Some path => Some (path, Before)
      end
      end.

    Fixpoint last_hole_steps_ty (fuel : Fuel.t) (uty : UHTyp.t) : option (list nat) :=
      match fuel with
      | Fuel.Kicked => None
      | Fuel.More fuel =>
      match uty with
      | UHTyp.Hole => Some nil
      | UHTyp.Parenthesized uty' => cons_opt 0 (last_hole_steps_ty fuel uty')
      | UHTyp.Num 
      | UHTyp.Bool => None
      | UHTyp.List uty1 => cons_opt 0 (last_hole_steps_ty fuel uty1)
      | UHTyp.OpSeq _ opseq => last_hole_steps_ty_opseq fuel opseq 0
      end
      end
    (* return an optional path of the last hole in opseq starting with the mth term from the end
       (e.g., the 0th and 1st term from the end of `1 + 2 + 3` are 3 and 2 respectively) *)
    with last_hole_steps_ty_opseq (fuel : Fuel.t) (opseq : OperatorSeq.opseq UHTyp.t UHTyp.op) (m : nat) : option (list nat) :=
      match fuel with
      | Fuel.Kicked => None
      | Fuel.More fuel =>
        let l := OperatorSeq.seq_length opseq in
        if Nat.leb l m
        then None
        else
          let n := l-m-1 in
          match OperatorSeq.seq_nth n opseq with
          | None => None (* degenerate case *)
          | Some uty' =>
            match last_hole_steps_ty fuel uty' with
            | Some ns => Some (cons n ns)
            | None => last_hole_steps_ty_opseq fuel opseq (m+1)
            end
          end
      end.

    Fixpoint last_hole_steps_pat (fuel : Fuel.t) (p : UHPat.t) : option(list nat) := 
      match fuel with
      | Fuel.Kicked => None
      | Fuel.More fuel =>
      match p with 
      | UHPat.Parenthesized p1 => cons_opt 0 (last_hole_steps_pat fuel p1)
      | UHPat.Pat _ (UHPat.EmptyHole _) => Some nil
      | UHPat.Pat _ UHPat.Wild
      | UHPat.Pat _ (UHPat.Var _)
      | UHPat.Pat _ (UHPat.NumLit _) 
      | UHPat.Pat _ (UHPat.BoolLit _) => None
      | UHPat.Pat _ (UHPat.Inj _ p1) => cons_opt 0 (last_hole_steps_pat fuel p1)
      | UHPat.Pat _ (UHPat.OpSeq _ opseq) => last_hole_steps_opseq_pat fuel opseq 0
      end
      end
    with last_hole_steps_opseq_pat (fuel : Fuel.t) (opseq : UHPat.opseq) (m : nat) : option (list nat) :=
      match fuel with
      | Fuel.Kicked => None
      | Fuel.More fuel =>
        let l := OperatorSeq.seq_length opseq in
        if Nat.leb l m
        then None
        else
          let n := l-m-1 in
          match OperatorSeq.seq_nth n opseq with
          | None => None
          | Some ue =>
            match last_hole_steps_pat fuel ue with
            | Some ns => Some (cons n ns)
            | None => last_hole_steps_opseq_pat fuel opseq (m+1)
            end
          end
      end.

    Fixpoint last_hole_steps (fuel : Fuel.t) (ue : UHExp.t) : option (list nat) :=
      match fuel with
      | Fuel.Kicked => None
      | Fuel.More fuel =>
      match ue with
      | UHExp.Parenthesized ue' => cons_opt 0 (last_hole_steps fuel ue')
      | UHExp.Tm _ ue' =>
        match ue' with
        | UHExp.EmptyHole _ => Some nil
        | UHExp.Asc ue0 uty1 =>
          cons_opt2 
            1 (last_hole_steps_ty fuel uty1)
            0 (fun _ => last_hole_steps fuel ue0)
        | UHExp.Var _ _ => None
        | UHExp.Let p ann e1 e2 =>
          match last_hole_steps fuel e2 with
          | Some ns => Some (cons 3 ns)
          | None => 
            match last_hole_steps fuel e1 with 
            | Some ns => Some (cons 2 ns) 
            | None => 
              match ann with 
              | Some ann_ty => 
                cons_opt2
                  1 (last_hole_steps_ty fuel ann_ty)
                  0 (fun _ => last_hole_steps_pat fuel p)
              | None => 
                cons_opt 0 (last_hole_steps_pat fuel p)
              end
            end 
          end
        | UHExp.Lam p ann e1 => 
          match last_hole_steps fuel e1 with
          | Some ns => Some (cons 2 ns)
          | None => 
            match ann with 
            | Some uty1 => 
              cons_opt2
                1 (last_hole_steps_ty fuel uty1)
                0 (fun _ => last_hole_steps_pat fuel p)
            | None => 
              cons_opt 0 (last_hole_steps_pat fuel p)
            end
          end
        | UHExp.NumLit _ 
        | UHExp.BoolLit _ => None
        | UHExp.Inj _ ue0 => cons_opt 0 (last_hole_steps fuel ue0)
        | UHExp.Case e1 rules =>
          match last_hole_steps_rules fuel rules with 
          | (Some ns) as result => result
          | None => cons_opt 0 (last_hole_steps fuel e1)
          end
        | UHExp.OpSeq _ opseq => last_hole_steps_opseq fuel opseq 0
        | UHExp.ApPalette _ _ _ => None (* TODO figure out tab order protocol *)
        end
      end
      end
    with last_hole_steps_rules 
      (fuel : Fuel.t)
      (rules : UHExp.rules)
      : option(list nat) :=  
        match fuel with
        | Fuel.Kicked => None
        | Fuel.More fuel =>
          let n_rules := List.length rules in 
          Util.findmapi (List.rev rules) (
            fun i rule => 
              match rule with 
              | UHExp.Rule p e => 
                match last_hole_steps fuel e with 
                | Some ns => Some (cons (n_rules - i) (cons 1 ns))
                | None => 
                  match last_hole_steps_pat fuel p with 
                  | Some ns => Some (cons (n_rules - i) (cons 0 ns))
                  | None => None
                  end
                end
              end)
        end
    (* return an optional path of the last hole in opseq starting with the mth term from the end
       (e.g., the 0th and 1st term from the end of `1 + 2 + 3` are 3 and 2 respectively) *)
    with last_hole_steps_opseq (fuel : Fuel.t) (opseq : OperatorSeq.opseq UHExp.t UHExp.op) (m : nat) : option (list nat) :=
      match fuel with
      | Fuel.Kicked => None
      | Fuel.More fuel =>
        let l := OperatorSeq.seq_length opseq in
        if Nat.leb l m
        then None
        else
          let n := l-m-1 in
          match OperatorSeq.seq_nth n opseq with
          | None => None
          | Some ue =>
            match last_hole_steps fuel ue with
            | Some ns => Some (cons n ns)
            | None => last_hole_steps_opseq fuel opseq (m+1)
            end
          end
      end.

    Fixpoint prev_hole_steps_ty (fuel : Fuel.t) (zty : ZTyp.t) : option (list nat) :=
      match fuel with
      | Fuel.Kicked => None
      | Fuel.More fuel =>
      match zty with
      | ZTyp.CursorT cursor_side uty =>
        match cursor_side, uty with
        | _, UHTyp.Hole => None
        | Before, _ => None
        | After, _ => last_hole_steps_ty fuel uty
        | In _, _ => None
        end
      | ZTyp.ParenthesizedZ zty' => cons_opt 0 (prev_hole_steps_ty fuel zty')
      | ZTyp.OpSeqZ _ zty' surround =>
        let n := OperatorSeq.surround_prefix_length surround in
        match prev_hole_steps_ty fuel zty' with
        | Some ns => Some (cons n ns)
        | None =>
          let uty' := ZTyp.erase zty' in
          let opseq := OperatorSeq.opseq_of_exp_and_surround uty' surround in
          let m := OperatorSeq.surround_suffix_length surround in
          last_hole_steps_ty_opseq fuel opseq (m+1)
        end
      end
      end.

    Fixpoint prev_hole_path_ty (fuel : Fuel.t) (zty : ZTyp.t) : option Path.t :=
      match fuel with
      | Fuel.Kicked => None
      | Fuel.More fuel =>
      match prev_hole_steps_ty fuel zty with
      | None => None
      | Some path => Some (path, Before)
      end
      end.

    Fixpoint prev_hole_steps_pat (fuel : Fuel.t) (zp : ZPat.t) : option(list nat) := 
      match fuel with
      | Fuel.Kicked => None
      | Fuel.More fuel =>
      match zp with 
      | ZPat.ParenthesizedZ zp1 => cons_opt 0 (prev_hole_steps_pat fuel zp1)
      | ZPat.CursorP cursor_side p => 
        match cursor_side, p with 
        | _, (UHPat.Pat _ (UHPat.EmptyHole _)) => None
        | Before, _ => None
        | After, _ => last_hole_steps_pat fuel p
        | In k, _ => 
          match p with 
          | UHPat.Parenthesized _ => None
          | UHPat.Pat err p' => 
            match p' with 
            | UHPat.EmptyHole _ => None
            | UHPat.Wild
            | UHPat.Var _
            | UHPat.NumLit _
            | UHPat.BoolLit _ => None
            | UHPat.Inj _ p1 => None
            | UHPat.OpSeq _ _ => None
            end
          end
        end
      | ZPat.Deeper _ (ZPat.InjZ _ zp1) => cons_opt 0 (prev_hole_steps_pat fuel zp1)
      | ZPat.Deeper _ (ZPat.OpSeqZ _ zp1 surround) => 
        let n := OperatorSeq.surround_prefix_length surround in
        match prev_hole_steps_pat fuel zp1 with
        | Some ns => Some (cons n ns)
        | None =>
          let ue_n := ZPat.erase zp1 in
          let opseq := OperatorSeq.opseq_of_exp_and_surround ue_n surround in
          let m := OperatorSeq.surround_suffix_length surround in
          last_hole_steps_opseq_pat fuel opseq (m+1)
        end
      end
      end.

    Fixpoint prev_hole_path_pat (fuel : Fuel.t) (zp : ZPat.t) : option Path.t :=
      match fuel with
      | Fuel.Kicked => None
      | Fuel.More fuel =>
      match prev_hole_steps_pat fuel zp with
      | None => None
      | Some path => Some (path, Before)
      end
      end.

    Fixpoint prev_hole_steps (fuel : Fuel.t) (ze : ZExp.t) : option (list nat) :=
      match fuel with
      | Fuel.Kicked => None
      | Fuel.More fuel =>
      match ze with
      | ZExp.CursorE cursor_side ue =>
        match cursor_side, ue with
        | _, (UHExp.Tm _ (UHExp.EmptyHole _)) => None
        | After, _ => last_hole_steps fuel ue
        | Before, _ => None
        | In k, _ =>
          match ue with
          | UHExp.Parenthesized _ => None (* cannot be In Parenthesized term *)
          | UHExp.Tm err ue' =>
            match ue' with
            | UHExp.Asc ue'' _ => cons_opt 0 (last_hole_steps fuel ue'')
            | UHExp.Var _ _ => None
            | UHExp.Let _ _ _ _ => None
            | UHExp.Lam _ _ _ => None
            | UHExp.NumLit _
            | UHExp.BoolLit _ => None
            | UHExp.Inj _ _ => None
            | UHExp.Case _ _ => 
              match k with 
              | 0 => None
              | 1 => last_hole_steps fuel ue
              | _ => None
              end
            | UHExp.EmptyHole _ => None
            | UHExp.OpSeq _ _ => None
            | UHExp.ApPalette _ _ _ => None (* TODO *)
            end
          end
        end
      | ZExp.Deeper _ ze' =>
        match ze' with
        | ZExp.AscZ1 ze0 _ => 
          cons_opt 0 (prev_hole_steps fuel ze0)
        | ZExp.AscZ2 ue0 zty1 =>
          cons_opt2
            1 (prev_hole_steps_ty fuel zty1)
            0 (fun _ => last_hole_steps fuel ue0)
        | ZExp.LetZP zp _ _ _ => 
          cons_opt 0 (prev_hole_steps_pat fuel zp) 
        | ZExp.LetZA p zann _ _ => 
          cons_opt2
            1 (prev_hole_steps_ty fuel zann)
            0 (fun _ => last_hole_steps_pat fuel p)
        | ZExp.LetZE1 p ann ze1 _ => 
          match prev_hole_steps fuel ze1 with
          | Some ns => Some (cons 2 ns)
          | None => 
            match ann with 
            | Some ann_ty => 
              cons_opt2 
                1 (last_hole_steps_ty fuel ann_ty)
                0 (fun _ => last_hole_steps_pat fuel p)
            | None => 
              cons_opt 0 (last_hole_steps_pat fuel p)
            end
          end
        | ZExp.LetZE2 p ann e1 ze2 => 
          match prev_hole_steps fuel ze2 with
          | Some ns => Some (cons 3 ns)
          | None => 
            match last_hole_steps fuel e1 with 
            | Some ns => Some (cons 2 ns)
            | None => 
              match ann with 
              | Some ann_ty => 
                cons_opt2
                  1 (last_hole_steps_ty fuel ann_ty)
                  0 (fun _ => last_hole_steps_pat fuel p)
              | None => None
              end
            end
          end
        | ZExp.LamZP zp _ _ => prev_hole_steps_pat fuel zp 
        | ZExp.LamZA p zann _ => 
          cons_opt2 
            1 (prev_hole_steps_ty fuel zann)
            0 (fun _ => last_hole_steps_pat fuel p)
        | ZExp.LamZE p ann ze1 => 
          match prev_hole_steps fuel ze1 with 
          | Some ns => Some (cons 2 ns)
          | None => 
            match ann with 
            | Some uty1 => 
              cons_opt2 
                1 (last_hole_steps_ty fuel uty1)
                0 (fun _ => last_hole_steps_pat fuel p)
            | None => cons_opt 0 (last_hole_steps_pat fuel p)
            end
          end
        | ZExp.InjZ _ ze0 => cons_opt 0 (prev_hole_steps fuel ze0)
        | ZExp.CaseZE ze rules => 
          cons_opt 0 (prev_hole_steps fuel ze)
        | ZExp.CaseZR e zrules => 
          let zr := ZList.prj_z zrules in 
          let prefix := ZList.prj_prefix zrules in 
          let prefix_len := List.length prefix in 
          match zr with 
          | ZExp.RuleZP zp e => 
            match prev_hole_steps_pat fuel zp with 
            | Some ns => Some (cons (S prefix_len) (cons 0 ns)) 
            | None => last_hole_steps_rules fuel prefix 
            end
          | ZExp.RuleZE p ze => 
            match prev_hole_steps fuel ze with 
            | Some ns => Some (cons (S prefix_len) (cons 1 ns))
            | None => 
              match last_hole_steps_pat fuel p with 
              | Some ns => Some (cons (S prefix_len) (cons 0 ns))
              | None => last_hole_steps_rules fuel prefix 
              end
            end
          end
        | ZExp.OpSeqZ _ ze_n surround =>
          let n := OperatorSeq.surround_prefix_length surround in
          match prev_hole_steps fuel ze_n with
          | Some ns => Some (cons n ns)
          | None =>
            let ue_n := ZExp.erase ze_n in
            let opseq := OperatorSeq.opseq_of_exp_and_surround ue_n surround in
            let m := OperatorSeq.surround_suffix_length surround in
            last_hole_steps_opseq fuel opseq (m+1)
          end
        | ZExp.ApPaletteZ _ _ _ => None (* TODO figure out tab order protocol *)
        end
      | ZExp.ParenthesizedZ ze0 => cons_opt 0 (prev_hole_steps fuel ze0)
      end
      end.

    Fixpoint prev_hole_path (fuel : Fuel.t) (ze : ZExp.t) : option Path.t :=
      match fuel with
      | Fuel.Kicked => None
      | Fuel.More fuel =>
      match prev_hole_steps fuel ze with
      | None => None
      | Some path => Some (path, Before)
      end
      end.
  End Path.

  Module Type ASSOCIATOR.
    (* calls the parser (from OCaml) to produce a skel from an opseq. 
     * initially, there are no errors marked in the skel. *)
    Parameter associate_ty : UHTyp.opseq -> UHTyp.skel_t.
    Parameter associate_pat : UHPat.opseq -> UHPat.skel_t.
    Parameter associate_exp : UHExp.opseq -> UHExp.skel_t.
  End ASSOCIATOR.

  Module FAction (Associator : ASSOCIATOR).
    Inductive op_shape : Type := 
    | SPlus : op_shape
    | STimes : op_shape
    | SSpace : op_shape
    | SComma : op_shape
    | SArrow : op_shape
    | SVBar  : op_shape.

    Definition ty_op_of (os : op_shape) : option(UHTyp.op) := 
      match os with 
      | SArrow => Some UHTyp.Arrow
      | SComma => Some UHTyp.Prod
      | SVBar => Some UHTyp.Sum
      | SPlus
      | STimes
      | SSpace => None
      end.

    Definition op_shape_of_ty_op (op : UHTyp.op) : op_shape := 
      match op with 
      | UHTyp.Arrow => SArrow
      | UHTyp.Prod => SComma
      | UHTyp.Sum => SVBar
      end.

    Definition pat_op_of (os : op_shape) : option(UHPat.op) :=
      match os with 
      | SComma => Some UHPat.Comma
      | SSpace => Some UHPat.Space 
      | SPlus
      | STimes
      | SArrow 
      | SVBar => None
      end.

    Definition op_shape_of_pat_op (op : UHPat.op) : op_shape := 
      match op with 
      | UHPat.Comma => SComma
      | UHPat.Space => SSpace
      end.

    Definition exp_op_of (os : op_shape) : option(UHExp.op) := 
      match os with 
      | SPlus => Some UHExp.Plus
      | STimes => Some UHExp.Times
      | SSpace => Some UHExp.Space
      | SComma => Some UHExp.Comma
      | SArrow 
      | SVBar => None
      end.

    Definition op_shape_of_exp_op (op : UHExp.op) : op_shape := 
      match op with 
      | UHExp.Plus => SPlus
      | UHExp.Times => STimes
      | UHExp.Space => SSpace
      | UHExp.Comma => SComma
      end.

    Inductive shape : Type :=
    | SParenthesized : shape
    (* type shapes *)
    | SNum : shape
    | SBool : shape
    (* expression shapes *)
    | SAsc : shape
    | SLet : shape
    | SVar : Var.t -> ZExp.cursor_side -> shape
    | SLam : shape
    | SNumLit : nat -> ZExp.cursor_side -> shape
    | SBoolLit : bool -> ZExp.cursor_side -> shape
    | SInj : inj_side -> shape
    | SCase : shape
    | SRule : shape
    | SOp : op_shape -> shape
    | SApPalette : PaletteName.t -> shape
    (* pattern-only shapes *)
    | SWild : shape.

    Inductive t : Type :=
    | MoveTo : Path.t -> t
    | MoveToNextHole : t
    | MoveToPrevHole : t
    | UpdateApPalette : UHExp.HoleRefs.m_hole_ref(PaletteSerializedModel.t) -> t
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

    Fixpoint perform_ty (fuel : Fuel.t) (a : t) (zty : ZTyp.t) : option ZTyp.t :=
      match fuel with
      | Fuel.Kicked => None
      | Fuel.More fuel =>
      match (a, zty) with
      (* Movement *)
      | (MoveTo path, _) => 
        let ty := ZTyp.erase zty in 
        Path.follow_ty fuel path ty
      | (MoveToPrevHole, _) =>
        match Path.prev_hole_path_ty fuel zty with
        | None => None
        | Some path => perform_ty fuel (MoveTo path) zty
        end
      | (MoveToNextHole, _) =>
        match Path.next_hole_path_ty fuel zty with
        | None => None
        | Some path =>
          (* [debug] let path := Helper.log_path path in *)
          perform_ty fuel (MoveTo path) zty
        end
      (* Backspace and Delete *)
      | (Backspace, ZTyp.CursorT After uty) 
      | (Backspace, ZTyp.CursorT (In _) uty) => 
        Some (ZTyp.CursorT Before UHTyp.Hole)
      | (Delete, ZTyp.CursorT Before uty) 
      | (Delete, ZTyp.CursorT (In _) uty) => 
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
        | OperatorSeq.EmptyPrefix _ => None
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
      | (Construct (SOp os), ZTyp.CursorT After uty1) 
      | (Construct (SOp os), ZTyp.CursorT (In _) uty1) => 
        match ty_op_of os with
        | None => None
        | Some op => 
          let surround := OperatorSeq.EmptySuffix (OperatorSeq.ExpPrefix uty1 op) in 
          let zty0 := ZTyp.CursorT Before UHTyp.Hole in 
          Some (make_ty_OpSeqZ zty0 surround)
        end
      | (Construct (SOp os), ZTyp.CursorT Before uty1) => 
        match ty_op_of os with
        | None => None
        | Some op => 
          let surround := OperatorSeq.EmptyPrefix (OperatorSeq.ExpSuffix op uty1) in 
          let zty0 := ZTyp.CursorT Before UHTyp.Hole in 
          Some (make_ty_OpSeqZ zty0 surround)
        end
      | (Construct (SOp os), 
          ZTyp.OpSeqZ _ 
            ((ZTyp.CursorT After uty0) as zty0)
            surround)
      | (Construct (SOp os), 
          ZTyp.OpSeqZ _ 
            ((ZTyp.CursorT (In _) uty0) as zty0)
            surround) => 
        match ty_op_of os with
        | None => None
        | Some op => 
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
        end
      | (Construct (SOp os), 
          ZTyp.OpSeqZ _ 
            ((ZTyp.CursorT Before uty0) as zty0)
            surround) => 
        match ty_op_of os with
        | None => None
        | Some op => 
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
        end
      (* Zipper Cases *)
      | (a, ZTyp.ParenthesizedZ zty1) => 
        match perform_ty fuel a zty1 with 
        | Some zty1' => 
          Some (ZTyp.ParenthesizedZ zty1')
        | None => None
        end
      | (a, ZTyp.OpSeqZ skel zty0 surround) => 
        match perform_ty fuel a zty0 with 
        | Some zty0' => 
          Some (ZTyp.OpSeqZ skel zty0' surround)
        | None => None
        end
      | _ => None
      end
      end.

    Definition abs_perform_Backspace_Before_op
      {E Z op M : Type}
      (combine_for_Backspace_Space : E -> Z -> Z)
      (z_typecheck_fix_holes : Fuel.t -> Contexts.t -> MetaVarGen.t -> Z -> option(M))
      (make_and_typecheck_OpSeqZ : 
        Fuel.t -> Contexts.t -> MetaVarGen.t -> 
        Z -> OperatorSeq.opseq_surround E op ->
        option(M))
      (is_EmptyHole : E -> bool)
      (is_Space : op -> bool)
      (Space : op)
      (Cursor : cursor_side -> E -> Z)
      (fuel : Fuel.t)
      (ctx : Contexts.t)
      (u_gen : MetaVarGen.t)
      (e0 : E)
      (ze0 : Z)
      (surround : OperatorSeq.opseq_surround E op)
      : option(M) := 
        match surround with 
        | OperatorSeq.EmptyPrefix _ => None
        | OperatorSeq.EmptySuffix prefix => 
          match prefix with 
          | OperatorSeq.ExpPrefix e1 op1 => 
            (* e1 op1 |ze0 *)
            if is_Space op1 then 
              (* e1 |ze0 *)
              let ze0' := combine_for_Backspace_Space e1 ze0 in  
              z_typecheck_fix_holes fuel ctx u_gen ze0' 
            else
              match (is_EmptyHole e1, is_EmptyHole e0) with 
              | (true, true) => 
                (* _1 op1 |_0 --> _1| *)
                let ze0' := Cursor After e1 in 
                z_typecheck_fix_holes fuel ctx u_gen ze0'
              | (true, _) => 
                (* _1 op1 |e0 --> |e0 *)
                z_typecheck_fix_holes fuel ctx u_gen ze0
              | (false, true) => 
                (* e1 op1 |_0 --> e1| *)
                let ze0' := Cursor After e1 in 
                z_typecheck_fix_holes fuel ctx u_gen ze0'
              | (false, false) => 
                (* e1 op1 |ze0 --> e1 |ze0 *)
                let surround' := 
                  OperatorSeq.EmptySuffix 
                    (OperatorSeq.ExpPrefix e1 Space) in 
                make_and_typecheck_OpSeqZ fuel ctx u_gen ze0 surround' 
              end
          | OperatorSeq.SeqPrefix seq1 op1 => 
            (* seq1 op1 |ze0 *)
            match is_Space op1 with 
            | true =>
              (* seq1 |ze0 *)
              let (e1, prefix') := OperatorSeq.split_tail seq1 in 
              let surround' := OperatorSeq.EmptySuffix prefix' in 
              let ze0' := combine_for_Backspace_Space e1 ze0 in 
              make_and_typecheck_OpSeqZ fuel ctx u_gen ze0' surround'
            | false => 
              let (e1, prefix') := OperatorSeq.split_tail seq1 in 
              if is_EmptyHole e0 then 
                (* prefix' e1 op1 |_0 --> prefix' e1| *)
                let surround' := OperatorSeq.EmptySuffix prefix' in 
                let ze0' := Cursor After e1 in 
                make_and_typecheck_OpSeqZ fuel ctx u_gen ze0' surround'
              else if is_EmptyHole e1 then 
                (* prefix' _1 op1 |e0 --> prefix' |e0 *)
                let surround' := OperatorSeq.EmptySuffix prefix' in 
                make_and_typecheck_OpSeqZ fuel ctx u_gen ze0 surround'
              else
                (* seq1 op1 |ze0 --> seq1 |ze0 *)
                let prefix' := OperatorSeq.SeqPrefix seq1 Space in 
                let surround' := OperatorSeq.EmptySuffix prefix' in 
                make_and_typecheck_OpSeqZ fuel ctx u_gen ze0 surround' 
            end
          end
        | OperatorSeq.BothNonEmpty prefix suffix => 
          match prefix with 
          | OperatorSeq.ExpPrefix e1 op1 => 
            (* e1 op1 |ze0 ...suffix *)
            match is_Space op1 with 
            | true => 
              (* e1 |ze0 ...suffix *)
              let ze0' := combine_for_Backspace_Space e1 ze0 in  
              let surround' := OperatorSeq.EmptyPrefix suffix in 
              make_and_typecheck_OpSeqZ fuel ctx u_gen ze0' surround'
            | false => 
              if is_EmptyHole e0 then 
                (* e1 op1 |_0 suffix --> e1| suffix *)
                let surround' := OperatorSeq.EmptyPrefix suffix in 
                let ze0' := Cursor After e1 in 
                make_and_typecheck_OpSeqZ fuel ctx u_gen ze0' surround'
              else if is_EmptyHole e1 then 
                (* _1 op1 |e0 suffix --> |e0 suffix *)
                let surround' := OperatorSeq.EmptyPrefix suffix in 
                make_and_typecheck_OpSeqZ fuel ctx u_gen ze0 surround'
              else
                (* e1 op1 |ze0 --> e1 |ze0 ...suffix *)
                let surround' := 
                  OperatorSeq.BothNonEmpty 
                    (OperatorSeq.ExpPrefix e1 Space) 
                    suffix in 
                make_and_typecheck_OpSeqZ fuel ctx u_gen ze0 surround' 
            end
          | OperatorSeq.SeqPrefix seq1 op1 => 
            (* seq1 op1 |ze0 ...suffix *)
            match is_Space op1 with 
            | true =>
              (* seq1 |ze0 ...suffix *)
              let (e1, prefix') := OperatorSeq.split_tail seq1 in 
              let ze0' :=  combine_for_Backspace_Space e1 ze0 in  
              let surround' := OperatorSeq.BothNonEmpty prefix' suffix in 
              make_and_typecheck_OpSeqZ fuel ctx u_gen ze0' surround'
            | false => 
              let (e1, prefix') := OperatorSeq.split_tail seq1 in 
              if is_EmptyHole e0 then 
                (* prefix' e1 op1 |_0 suffix --> prefix' e1| suffix *)
                let surround' := OperatorSeq.BothNonEmpty prefix' suffix in 
                let ze0' := Cursor After e1 in 
                make_and_typecheck_OpSeqZ fuel ctx u_gen ze0' surround'
              else if is_EmptyHole e1 then 
                (* prefix' _1 op1 |e0 suffix --> prefix' |e0 suffix *)
                let surround' := OperatorSeq.BothNonEmpty prefix' suffix in 
                make_and_typecheck_OpSeqZ fuel ctx u_gen ze0 surround'
              else 
                (* seq1 op1 |ze0 suffix --> seq1 |ze0 suffix *)
                let prefix' := OperatorSeq.SeqPrefix seq1 Space in 
                let surround' := OperatorSeq.BothNonEmpty prefix' suffix in 
                make_and_typecheck_OpSeqZ fuel ctx u_gen ze0 surround' 
            end
          end
        end.      

    Definition abs_perform_Delete_After_op
      {E Z op M : Type}
      (combine_for_Delete_Space : Z -> E -> Z)
      (z_typecheck_fix_holes : Fuel.t -> Contexts.t -> MetaVarGen.t -> Z -> option(M))
      (make_and_typecheck_OpSeqZ : 
        Fuel.t -> Contexts.t -> MetaVarGen.t -> 
        Z -> OperatorSeq.opseq_surround E op ->
        option(M))
      (is_EmptyHole : E -> bool)
      (is_Space : op -> bool)
      (Space : op)
      (Cursor : cursor_side -> E -> Z)
      (fuel : Fuel.t)
      (ctx : Contexts.t)
      (u_gen : MetaVarGen.t)
      (e0 : E)
      (ze0 : Z)
      (surround : OperatorSeq.opseq_surround E op)
      : option(M) := 
        match surround with 
        | OperatorSeq.EmptySuffix _ => None (* precluded by pattern match above *)
        | OperatorSeq.EmptyPrefix suffix => 
          match suffix with 
          | OperatorSeq.ExpSuffix op e1 => 
            match is_Space op with 
            | true => 
              let ze0' := combine_for_Delete_Space ze0 e1 in 
              z_typecheck_fix_holes fuel ctx u_gen ze0'  
            | false => 
              match (is_EmptyHole e0, is_EmptyHole e1) with 
              | (true, true) => 
                (* _0| op _1 --> _0| *)
                z_typecheck_fix_holes fuel ctx u_gen ze0
              | (true, false) => 
                (* _0| op e1 --> |e1 *)
                let ze1 := Cursor Before e1  in 
                z_typecheck_fix_holes fuel ctx u_gen ze1
              | (false, true) => 
                (* e0| op _ --> e0| *)
                z_typecheck_fix_holes fuel ctx u_gen ze0
              | (false, false) => 
                (* e0| op e1 --> e0| e1 *)
                let surround' := 
                  OperatorSeq.EmptyPrefix 
                    (OperatorSeq.ExpSuffix Space e1) in 
                make_and_typecheck_OpSeqZ fuel ctx u_gen ze0 surround' 
              end
            end
          | OperatorSeq.SeqSuffix op seq => 
            match is_Space op with 
            | true => 
              let (e, suffix') := OperatorSeq.split0 seq in
              let surround' := OperatorSeq.EmptyPrefix suffix' in 
              let ze0' := combine_for_Delete_Space ze0 e in 
              make_and_typecheck_OpSeqZ fuel ctx u_gen ze0' surround'
            | false => 
              let (e1, suffix') := OperatorSeq.split0 seq in 
              if is_EmptyHole e1 then 
                (* e0| op _ suffix' --> e0| suffix' *)
                let surround' := OperatorSeq.EmptyPrefix suffix' in 
                make_and_typecheck_OpSeqZ fuel ctx u_gen ze0 surround'
              else if is_EmptyHole e0 then 
                (* _0| op e1 suffix' --> |e1 suffix' *)
                let surround' := OperatorSeq.EmptyPrefix suffix' in 
                let ze1 := Cursor Before e1  in 
                make_and_typecheck_OpSeqZ fuel ctx u_gen ze1 surround'
              else
                (* e0| op seq --> e0| seq *)
                let suffix' := OperatorSeq.SeqSuffix Space seq in 
                let surround' := OperatorSeq.EmptyPrefix suffix' in 
                make_and_typecheck_OpSeqZ fuel ctx u_gen ze0 surround' 
            end
          end
        | OperatorSeq.BothNonEmpty prefix suffix => 
          match suffix with 
          | OperatorSeq.ExpSuffix op e1 => 
            match is_Space op with 
            | true => 
              let ze0' := combine_for_Delete_Space ze0 e1 in 
              let surround' := OperatorSeq.EmptySuffix prefix in 
              make_and_typecheck_OpSeqZ fuel ctx u_gen ze0' surround'
            | false => 
              if is_EmptyHole e1 then 
                (* prefix e0| op _ --> prefix e0| *)
                let surround' := OperatorSeq.EmptySuffix prefix in 
                make_and_typecheck_OpSeqZ fuel ctx u_gen ze0 surround'
              else if is_EmptyHole e0 then 
                (* prefix _0| op e1 --> prefix |e1 *)
                let surround' := OperatorSeq.EmptySuffix prefix in 
                let ze1 := Cursor Before e1 in 
                make_and_typecheck_OpSeqZ fuel ctx u_gen ze1 surround'
              else
                (* prefix e0| op e1 --> e0| e1 *)
                let surround' := 
                  OperatorSeq.BothNonEmpty prefix 
                    (OperatorSeq.ExpSuffix Space e1) in 
                make_and_typecheck_OpSeqZ fuel ctx u_gen ze0 surround'
            end
          | OperatorSeq.SeqSuffix op seq => 
            match is_Space op with 
            | true => 
              let (e, suffix') := OperatorSeq.split0 seq in 
              let ze0' := combine_for_Delete_Space ze0 e in 
              let surround' := OperatorSeq.BothNonEmpty prefix suffix' in 
              make_and_typecheck_OpSeqZ fuel ctx u_gen ze0' surround'
            | false => 
              let (e1, suffix') := OperatorSeq.split0 seq in 
              if is_EmptyHole e1 then 
                (* prefix e0| op _ suffix' --> prefix e0| suffix' *)
                let surround' := OperatorSeq.BothNonEmpty prefix suffix' in 
                make_and_typecheck_OpSeqZ fuel ctx u_gen ze0 surround'
              else if is_EmptyHole e0 then 
                (* prefix _0| op e1 suffix' --> prefix |e1 suffix' *)
                let surround' := OperatorSeq.BothNonEmpty prefix suffix' in 
                let ze1 := Cursor Before e1 in 
                make_and_typecheck_OpSeqZ fuel ctx u_gen ze1 surround'
              else
                (* prefix e| op seq --> e| seq *)
                let suffix' := OperatorSeq.SeqSuffix Space seq in 
                let surround' := OperatorSeq.BothNonEmpty prefix suffix' in 
                make_and_typecheck_OpSeqZ fuel ctx u_gen ze0 surround'
            end
          end
        end.

    Definition abs_perform_Construct_SOp_After
      {E Z Op M : Type}
      (bidelimit : E -> E)
      (new_EmptyHole : MetaVarGen.t -> (Z * MetaVarGen.t))
      (make_and_typecheck_OpSeqZ : 
        Fuel.t -> Contexts.t -> MetaVarGen.t -> 
        Z -> OperatorSeq.opseq_surround E Op -> 
        option(M))
      (fuel : Fuel.t)
      (ctx : Contexts.t)
      (u_gen : MetaVarGen.t)
      (e : E) (op : Op)
      : option(M) := 
        let e' := bidelimit e in 
        let prefix := OperatorSeq.ExpPrefix e' op in 
        let surround := OperatorSeq.EmptySuffix prefix in 
        let (ze0, u_gen) := new_EmptyHole u_gen in 
        make_and_typecheck_OpSeqZ fuel ctx u_gen ze0 surround. 
    
    Definition abs_perform_Construct_SOp_Before
      {E Z Op M : Type}
      (bidelimit : E -> E)
      (new_EmptyHole : MetaVarGen.t -> (Z * MetaVarGen.t))
      (make_and_typecheck_OpSeqZ : 
        Fuel.t -> Contexts.t -> MetaVarGen.t -> 
        Z -> OperatorSeq.opseq_surround E Op -> 
        option(M))
      (fuel : Fuel.t)
      (ctx : Contexts.t)
      (u_gen : MetaVarGen.t)
      (e : E) (op : Op)
      : option(M) := 
        let e' := bidelimit e in 
        let suffix := OperatorSeq.ExpSuffix op e' in 
        let surround := OperatorSeq.EmptyPrefix suffix in 
        let (ze0, u_gen') := new_EmptyHole u_gen in 
        make_and_typecheck_OpSeqZ fuel ctx u_gen ze0 surround.

    Definition abs_perform_Construct_SOp_After_surround 
      {E Z Op M : Type}
      (new_EmptyHole : MetaVarGen.t -> (Z * MetaVarGen.t))
      (make_and_typecheck_OpSeqZ : 
        Fuel.t -> Contexts.t -> MetaVarGen.t -> 
        Z -> OperatorSeq.opseq_surround E Op -> option(M))
      (is_Space : Op -> bool)
      (Space : Op)
      (Cursor : cursor_side -> E -> Z)
      (fuel : Fuel.t)
      (ctx : Contexts.t)
      (u_gen : MetaVarGen.t)
      (e : E)
      (op : Op)
      (surround : OperatorSeq.opseq_surround E Op)
      : option(M) := 
        match surround with 
        | OperatorSeq.EmptySuffix prefix => 
          let prefix' := OperatorSeq.prefix_append_exp prefix e op in 
          let surround' := OperatorSeq.EmptySuffix prefix' in 
          let (ze0, u_gen) := new_EmptyHole u_gen in 
          make_and_typecheck_OpSeqZ fuel ctx u_gen ze0 surround' 
        | OperatorSeq.EmptyPrefix suffix => 
          match suffix with 
          | OperatorSeq.ExpSuffix op' e' => 
            match is_Space op with 
            | true => 
              (* e| op' e' --> e |_ op' e' *)
              let prefix' := OperatorSeq.ExpPrefix e op in 
              let suffix' := OperatorSeq.ExpSuffix op' e' in 
              let surround' := OperatorSeq.BothNonEmpty prefix' suffix' in 
              let (ze0, u_gen) := new_EmptyHole u_gen in 
              make_and_typecheck_OpSeqZ fuel ctx u_gen ze0 surround' 
            | false => 
              match is_Space op' with 
              | true => 
                (* e| e' --> e op |e' *)
                let prefix' := OperatorSeq.ExpPrefix e op in 
                let surround' := OperatorSeq.EmptySuffix prefix' in 
                let ze0 := Cursor Before e' in 
                make_and_typecheck_OpSeqZ fuel ctx u_gen ze0 surround'  
              | false => 
                (* e| op' e' --> e op |_ op' e' *)
                let prefix' := OperatorSeq.ExpPrefix e op in 
                let suffix' := OperatorSeq.ExpSuffix op' e' in 
                let surround' := OperatorSeq.BothNonEmpty prefix' suffix' in 
                let (ze0, u_gen) := new_EmptyHole u_gen in 
                make_and_typecheck_OpSeqZ fuel ctx u_gen ze0 surround' 
              end
            end
          | OperatorSeq.SeqSuffix op' seq' => 
            match is_Space op with 
            | true => 
              (* e| seq' --> e |_ op' seq' *)
              let prefix' := OperatorSeq.ExpPrefix e op in 
              let surround' := OperatorSeq.BothNonEmpty prefix' suffix in 
              let (ze0, u_gen) := new_EmptyHole u_gen in 
              make_and_typecheck_OpSeqZ fuel ctx u_gen ze0 surround'
            | false => 
              match is_Space op' with 
              | true => 
                (* e| seq' --> e op |seq' *)
                let prefix' := OperatorSeq.ExpPrefix e op in 
                let (e0', suffix') := OperatorSeq.split0 seq' in 
                let surround' := OperatorSeq.BothNonEmpty prefix' suffix' in 
                let ze0 := Cursor Before e0' in 
                make_and_typecheck_OpSeqZ fuel ctx u_gen ze0 surround' 
              | false => 
                (* e| op' seq' --> e op |_ op' seq' *)
                let prefix' := OperatorSeq.ExpPrefix e op in 
                let surround' := OperatorSeq.BothNonEmpty prefix' suffix in 
                let (ze0, u_gen) := new_EmptyHole u_gen in 
                make_and_typecheck_OpSeqZ fuel ctx u_gen ze0 surround' 
              end
            end
          end
        | OperatorSeq.BothNonEmpty prefix suffix => 
          match suffix with 
          | OperatorSeq.ExpSuffix op' e' => 
            match is_Space op with 
            | true => 
              (* prefix e| op' e' --> prefix e |_ op' e' *)
              let prefix' := OperatorSeq.prefix_append_exp prefix e op in 
              let suffix' := OperatorSeq.ExpSuffix op' e' in 
              let surround' := OperatorSeq.BothNonEmpty prefix' suffix' in 
              let (ze0, u_gen) := new_EmptyHole u_gen in 
              make_and_typecheck_OpSeqZ fuel ctx u_gen ze0 surround' 
            | false => 
              match is_Space op' with 
              | true => 
                (* prefix e| e' --> prefix e op |e' *)
                let prefix' := OperatorSeq.prefix_append_exp prefix e op in 
                let surround' := OperatorSeq.EmptySuffix prefix' in 
                let ze0 := Cursor Before e' in 
                make_and_typecheck_OpSeqZ fuel ctx u_gen ze0 surround' 
              | false => 
                (* prefix e| op' e' --> prefix e op |_ op' e' *)
                let prefix' := OperatorSeq.prefix_append_exp prefix e op in 
                let suffix' := OperatorSeq.ExpSuffix op' e' in 
                let surround' := OperatorSeq.BothNonEmpty prefix' suffix' in 
                let (ze0, u_gen) := new_EmptyHole u_gen in 
                make_and_typecheck_OpSeqZ fuel ctx u_gen ze0 surround' 
              end
            end
          | OperatorSeq.SeqSuffix op' seq' => 
            match is_Space op with 
            | true => 
              (* prefix e| op' seq' --> prefix e |_ op' seq' *)
              let prefix' := OperatorSeq.prefix_append_exp prefix e op in 
              let surround' := OperatorSeq.BothNonEmpty prefix' suffix in 
              let (ze0, u_gen) := new_EmptyHole u_gen in 
              make_and_typecheck_OpSeqZ fuel ctx u_gen ze0 surround'
            | false => 
              match is_Space op' with 
              | true => 
                (* prefix e| seq' --> prefix e op |seq' *)
                let prefix' := OperatorSeq.prefix_append_exp prefix e op in 
                let (e0', suffix') := OperatorSeq.split0 seq' in 
                let surround' := OperatorSeq.BothNonEmpty prefix' suffix' in 
                let ze0' := Cursor Before e0' in 
                make_and_typecheck_OpSeqZ fuel ctx u_gen ze0' surround' 
              | false => 
                (* prefix e| op' seq' --> prefix e op |_ op' seq' *)
                let prefix' := OperatorSeq.prefix_append_exp prefix e op in 
                let surround' := OperatorSeq.BothNonEmpty prefix' suffix in 
                let (ze0, u_gen) := new_EmptyHole u_gen in 
                make_and_typecheck_OpSeqZ fuel ctx u_gen ze0 surround'
              end
            end
          end
        end.

    Definition abs_perform_Construct_SOp_Before_surround 
      {E Z Op M : Type}
      (erase : Z -> E)
      (new_EmptyHole : MetaVarGen.t -> (Z * MetaVarGen.t))
      (make_and_typecheck_OpSeqZ : 
        Fuel.t -> Contexts.t -> MetaVarGen.t -> 
        Z -> OperatorSeq.opseq_surround E Op -> option(M))
      (is_Space : Op -> bool)
      (Space : Op)
      (Cursor : cursor_side -> E -> Z)
      (fuel : Fuel.t)
      (ctx : Contexts.t)
      (u_gen : MetaVarGen.t)
      (ze0 : Z)
      (op : Op)
      (surround : OperatorSeq.opseq_surround E Op)
      : option(M) := 
        match surround with 
        | OperatorSeq.EmptyPrefix suffix => 
          (* |ze0 ... --> |_ op e0 ... *)
          let e0 := erase ze0 in 
          let suffix' := OperatorSeq.suffix_prepend_exp suffix op e0 in 
          let surround' := OperatorSeq.EmptyPrefix suffix' in 
          let (ze0, u_gen) := new_EmptyHole u_gen in 
          make_and_typecheck_OpSeqZ fuel ctx u_gen ze0 surround' 
        | OperatorSeq.EmptySuffix ((OperatorSeq.ExpPrefix e1 op') as prefix) => 
          match is_Space op' with 
          | true => 
            match is_Space op with 
            | true => 
              (* e1 |ze0 --> e1 |_ e0 *)
              let e0 := erase ze0 in 
              let suffix' := OperatorSeq.ExpSuffix Space e0 in 
              let surround' := OperatorSeq.BothNonEmpty prefix suffix' in 
              let (ze0, u_gen) := new_EmptyHole u_gen in 
              make_and_typecheck_OpSeqZ fuel ctx u_gen ze0 surround' 
            | false => 
              (* e1 |ze0 --> e1 op |ze0 *)
              let surround' := OperatorSeq.EmptySuffix (OperatorSeq.ExpPrefix e1 op) in 
              make_and_typecheck_OpSeqZ fuel ctx u_gen ze0 surround' 
            end
          | false => 
            (* prefix [^ ] |ze0 --> prefix |_ op e0 *)
            let e0 := erase ze0 in 
            let suffix' := OperatorSeq.ExpSuffix op e0 in 
            let surround' := OperatorSeq.BothNonEmpty prefix suffix' in 
            let (ze0, u_gen) := new_EmptyHole u_gen in 
            make_and_typecheck_OpSeqZ fuel ctx u_gen ze0 surround'
          end
        | OperatorSeq.EmptySuffix ((OperatorSeq.SeqPrefix seq1 op') as prefix) => 
          match is_Space op' with 
          | true => 
            match is_Space op with 
            | true => 
              (* seq1 |ze0 --> seq1 |_ e0 *)
              let e0 := erase ze0 in 
              let suffix' := OperatorSeq.ExpSuffix Space e0 in 
              let surround' := OperatorSeq.BothNonEmpty prefix suffix' in 
              let (ze0, u_gen) := new_EmptyHole u_gen in 
              make_and_typecheck_OpSeqZ fuel ctx u_gen ze0 surround'
            | false => 
              (* seq1 |ze0 --> seq1 op |ze0 *)
              let surround' := OperatorSeq.EmptySuffix (OperatorSeq.SeqPrefix seq1 op) in 
              make_and_typecheck_OpSeqZ fuel ctx u_gen ze0 surround' 
            end
          | false => 
            (* prefix [^ ] |ze0 --> prefix |_ op e0 *)
            let e0 := erase ze0 in 
            let suffix' := OperatorSeq.ExpSuffix op e0 in 
            let surround' := OperatorSeq.BothNonEmpty prefix suffix' in 
            let (ze0, u_gen) := new_EmptyHole u_gen in 
            make_and_typecheck_OpSeqZ fuel ctx u_gen ze0 surround'
          end
        | OperatorSeq.BothNonEmpty ((OperatorSeq.ExpPrefix e1 op') as prefix) suffix => 
          match is_Space op' with 
          | true => 
            match is_Space op with 
            | true => 
              (* e1 |ze0 suffix --> e1 |_ e0 suffix *)
              let e0 := erase ze0 in 
              let suffix' := OperatorSeq.suffix_prepend_exp suffix Space e0 in 
              let surround' := OperatorSeq.BothNonEmpty prefix suffix' in 
              let (ze0, u_gen) := new_EmptyHole u_gen in 
              make_and_typecheck_OpSeqZ fuel ctx u_gen ze0 surround'
            | false => 
              (* e1 |ze0 suffix --> e1 op |ze0 suffix *)
              let prefix' := OperatorSeq.ExpPrefix e1 op in 
              let surround' := OperatorSeq.BothNonEmpty prefix' suffix in 
              make_and_typecheck_OpSeqZ fuel ctx u_gen ze0 surround'
            end
          | false => 
            (* prefix [^ ] |ze0 suffix --> prefix |_ op e0 suffix *)
            let e0 := erase ze0 in 
            let suffix' := OperatorSeq.suffix_prepend_exp suffix op e0 in 
            let surround' := OperatorSeq.BothNonEmpty prefix suffix' in 
            let (ze0, u_gen) := new_EmptyHole u_gen in 
            make_and_typecheck_OpSeqZ fuel ctx u_gen ze0 surround'
          end
        | OperatorSeq.BothNonEmpty ((OperatorSeq.SeqPrefix seq1 op') as prefix) suffix => 
          match is_Space op' with 
          | true => 
            match is_Space op with 
            | true => 
              (* seq1 |ze0 suffix --> seq1 |_ e0 suffix *)
              let e0 := erase ze0 in 
              let suffix' := OperatorSeq.suffix_prepend_exp suffix Space e0 in 
              let surround' := OperatorSeq.BothNonEmpty prefix suffix' in 
              let (ze0, u_gen) := new_EmptyHole u_gen in 
              make_and_typecheck_OpSeqZ fuel ctx u_gen ze0 surround'
            | false => 
              (* seq1 |ze0 suffix --> seq1 op |ze0 suffix *)
              let prefix' := OperatorSeq.SeqPrefix seq1 op in 
              let surround' := OperatorSeq.BothNonEmpty prefix' suffix in 
              make_and_typecheck_OpSeqZ fuel ctx u_gen ze0 surround'
            end
          | false => 
            (* prefix [^ ] |ze0 suffix --> prefix |_ op e0 suffix *)
            let e0 := erase ze0 in 
            let suffix' := OperatorSeq.suffix_prepend_exp suffix op e0 in 
            let surround' := OperatorSeq.BothNonEmpty prefix suffix' in 
            let (ze0, u_gen) := new_EmptyHole u_gen in 
            make_and_typecheck_OpSeqZ fuel ctx u_gen ze0 surround'
          end
        end.

    Definition syn_zpat_fix_holes
      (fuel : Fuel.t)
      (ctx : Contexts.t)
      (u_gen : MetaVarGen.t)
      (zp : ZPat.t)
      : option (ZPat.t * HTyp.t * Contexts.t * MetaVarGen.t) := 
        let path := Path.of_zpat zp in 
        let p := ZPat.erase zp in 
        match UHExp.syn_pat_fix_holes fuel ctx u_gen false p with 
        | None => None
        | Some (p, ty, ctx, u_gen) => 
          match Path.follow_pat fuel path p with 
          | None => None
          | Some zp => Some (zp, ty, ctx, u_gen)
          end
        end.
    
    Definition ana_zpat_fix_holes
      (fuel : Fuel.t)
      (ctx : Contexts.t)
      (u_gen : MetaVarGen.t)
      (zp : ZPat.t)
      (ty : HTyp.t)
      : option (ZPat.t * Contexts.t * MetaVarGen.t) := 
        let path := Path.of_zpat zp in 
        let p := ZPat.erase zp in 
        match UHExp.ana_pat_fix_holes fuel ctx u_gen false p ty with 
        | None => None
        | Some (p, ctx, u_gen) => 
          match Path.follow_pat fuel path p with 
          | None => None
          | Some zp => Some (zp, ctx, u_gen)
          end
        end.

    Definition make_and_syn_OpSeqZ_pat
      (fuel : Fuel.t)
      (ctx : Contexts.t)
      (u_gen : MetaVarGen.t)
      (zp0 : ZPat.t)
      (surround : ZPat.opseq_surround)
      : option (ZPat.t * HTyp.t * Contexts.t * MetaVarGen.t) := 
        (* figure out the current path so that we can follow it again 
         * to reconstitute the Z-exp after calling into the UHExp hole 
         * insertion logic (otherwise we'd have to do a version of that
         * logic specific to Z-exps) *)
        let path0 := Path.of_OpSeqZ_pat zp0 surround in 
        let p0 := ZPat.erase zp0 in 
        let seq := OperatorSeq.opseq_of_exp_and_surround p0 surround in 
        let skel := Associator.associate_pat seq in 
        match UHExp.syn_skel_pat_fix_holes fuel ctx u_gen false skel seq with 
        | Some (skel, seq, ty, ctx, u_gen) => 
          let p := UHPat.Pat NotInHole (UHPat.OpSeq skel seq) in 
          match Path.follow_pat fuel path0 p with 
          | Some zp => Some (zp, ty, ctx, u_gen)
          | None => None
          end
        | None => None
        end.

    Definition make_and_ana_OpSeqZ_pat
      (fuel : Fuel.t)
      (ctx : Contexts.t)
      (u_gen : MetaVarGen.t)
      (zp0 : ZPat.t)
      (surround : ZPat.opseq_surround)
      (ty : HTyp.t)
      : option (ZPat.t * Contexts.t * MetaVarGen.t) := 
        (* figure out the current path so that we can follow it again 
         * to reconstitute the Z-exp after calling into the UHExp hole 
         * insertion logic (otherwise we'd have to do a version of that
         * logic specific to Z-exps) *)
        let path0 := Path.of_OpSeqZ_pat zp0 surround in 
        let p0 := ZPat.erase zp0 in 
        let seq := OperatorSeq.opseq_of_exp_and_surround p0 surround in 
        let skel := Associator.associate_pat seq in 
        match UHExp.ana_skel_pat_fix_holes fuel ctx u_gen false skel seq ty with 
        | Some (skel, seq, ctx, u_gen) => 
          let p := UHPat.Pat NotInHole (UHPat.OpSeq skel seq) in 
          match Path.follow_pat fuel path0 p with 
          | Some zp => Some (zp, ctx, u_gen)
          | None => None
          end
        | None => None
        end.

    Definition combine_for_Backspace_Space_pat p1 zp0 := 
      match zp0 with 
      | ZPat.CursorP _ (UHPat.Pat _ (UHPat.EmptyHole _)) => 
        (* p1 |_ --> p1| *)
        ZPat.CursorP After p1
      | _ => 
        (* p1 |zp0 --> |zp0 *)
        zp0
      end.

    Definition combine_for_Delete_Space_pat zp0 p := 
      match (zp0, p) with 
      | ((ZPat.CursorP After (UHPat.Pat _ (UHPat.EmptyHole _))),
         UHPat.Pat _ (UHPat.EmptyHole _)) => 
        (* _| _ --> _| *)
        zp0
      | ((ZPat.CursorP After (UHPat.Pat _ (UHPat.EmptyHole _))),
         _) => 
        (* _| p  --> |p *)
        ZPat.CursorP Before p
      | _ => 
        zp0
      end.

    Fixpoint perform_syn_pat
      (fuel : Fuel.t)
      (ctx : Contexts.t)
      (u_gen : MetaVarGen.t)
      (a : t)
      (zp : ZPat.t)
      : option(ZPat.t * HTyp.t * Contexts.t * MetaVarGen.t) := 
      match fuel with
      | Fuel.Kicked => None
      | Fuel.More fuel =>
      match (a, zp) with
      (* Movement *)
      (* NOTE: we don't need to handle movement actions here for the purposes of the UI,
       * since it's handled at the top (expression) level, but for the sake of API completeness
       * we include it *)
      | (MoveTo path, _) => 
        let p := ZPat.erase zp in
        match UHExp.syn_pat fuel ctx p with 
        | None => None
        | Some (ty, _) => 
          match Path.follow_pat fuel path p with
          | Some zp => Some (zp, ty, ctx, u_gen)
          | None => None
          end
        end
      | (MoveToPrevHole, _) =>
        match Path.prev_hole_path_pat fuel zp with
        | None => None
        | Some path => perform_syn_pat fuel ctx u_gen (MoveTo path) zp
        end
      | (MoveToNextHole, _) =>
        match Path.next_hole_path_pat fuel zp with
        | None => None
        | Some path => perform_syn_pat fuel ctx u_gen (MoveTo path) zp
        end
      (* Backspace and Delete *)
      | (Backspace, ZPat.CursorP After p) => 
        match p with 
        | UHPat.Pat _ (UHPat.EmptyHole _) => 
          Some (ZPat.CursorP Before p, HTyp.Hole, ctx, u_gen)
        | _ => 
          let (p, u_gen) := UHPat.new_EmptyHole u_gen in 
          Some (ZPat.CursorP Before p, HTyp.Hole, ctx, u_gen)
        end
      | (Backspace, ZPat.CursorP Before _) => None
      | (Delete, ZPat.CursorP Before p) => 
        match p with 
        | UHPat.Pat _ (UHPat.EmptyHole _) => 
          Some (ZPat.CursorP After p, HTyp.Hole, ctx, u_gen)
        | _ => 
          let (e', u_gen') := UHExp.new_EmptyHole u_gen in 
          Some (ZPat.CursorP Before p, HTyp.Hole, ctx, u_gen)
        end
      | (Delete, ZPat.CursorP After _) => None
      | (Backspace, ZPat.CursorP (In _) _)
      | (Delete, ZPat.CursorP (In _) _) => 
        let (p, u_gen) := UHPat.new_EmptyHole u_gen in 
        let zp := ZPat.CursorP Before p in 
        Some (zp, HTyp.Hole, ctx, u_gen)
      | (Backspace, ZPat.Deeper _
          (ZPat.OpSeqZ _
            ((ZPat.CursorP Before p0) as zp0) 
            ((OperatorSeq.EmptySuffix _) as surround)))
      | (Backspace, ZPat.Deeper _
          (ZPat.OpSeqZ _
            ((ZPat.CursorP Before p0) as zp0) 
            ((OperatorSeq.BothNonEmpty _ _) as surround))) =>
        abs_perform_Backspace_Before_op 
          combine_for_Backspace_Space_pat
          syn_zpat_fix_holes
          make_and_syn_OpSeqZ_pat
          UHPat.is_EmptyHole
          UHPat.is_Space
          UHPat.Space
          ZPat.CursorP
          fuel ctx u_gen p0 zp0 surround
      | (Delete, ZPat.Deeper _
          (ZPat.OpSeqZ _
            ((ZPat.CursorP After p0) as zp0)
            ((OperatorSeq.EmptyPrefix _) as surround)))
      | (Delete, ZPat.Deeper _ 
          (ZPat.OpSeqZ _
            ((ZPat.CursorP After p0) as zp0)
            ((OperatorSeq.BothNonEmpty _ _) as surround))) => 
        abs_perform_Delete_After_op
          combine_for_Delete_Space_pat
          syn_zpat_fix_holes
          make_and_syn_OpSeqZ_pat
          UHPat.is_EmptyHole
          UHPat.is_Space
          UHPat.Space
          ZPat.CursorP
          fuel ctx u_gen p0 zp0 surround
      (* Construct *)
      | (Construct SParenthesized, ZPat.CursorP _ p) => 
        match UHExp.syn_pat fuel ctx p with 
        | None => None
        | Some (ty, ctx) => 
          Some (
            ZPat.ParenthesizedZ zp, 
            ty,
            ctx,
            u_gen)
        end
      | (Construct (SVar x side), ZPat.CursorP _ (UHPat.Pat _ (UHPat.EmptyHole _))) 
      | (Construct (SVar x side), ZPat.CursorP _ (UHPat.Pat _ UHPat.Wild))
      | (Construct (SVar x side), ZPat.CursorP _ (UHPat.Pat _ (UHPat.Var _)))
      | (Construct (SVar x side), ZPat.CursorP _ (UHPat.Pat _ (UHPat.NumLit _)))
      | (Construct (SVar x side), ZPat.CursorP _ (UHPat.Pat _ (UHPat.BoolLit _))) =>
        Var.check_valid x (
        let ctx := Contexts.extend_gamma ctx (x, HTyp.Hole) in 
        Some
          (ZPat.CursorP side (UHPat.Pat NotInHole (UHPat.Var x)), 
           HTyp.Hole,
           ctx, 
           u_gen)
        )
      | (Construct (SVar _ _), _) => None
      | (Construct SWild, ZPat.CursorP _ (UHPat.Pat _ (UHPat.EmptyHole _))) 
      | (Construct SWild, ZPat.CursorP _ (UHPat.Pat _ UHPat.Wild))
      | (Construct SWild, ZPat.CursorP _ (UHPat.Pat _ (UHPat.Var _)))
      | (Construct SWild, ZPat.CursorP _ (UHPat.Pat _ (UHPat.NumLit _))) 
      | (Construct SWild, ZPat.CursorP _ (UHPat.Pat _ (UHPat.BoolLit _))) =>
        Some
          (ZPat.CursorP After (UHPat.Pat NotInHole UHPat.Wild), 
           HTyp.Hole,
           ctx, 
           u_gen)
      | (Construct SWild, _) => None
      | (Construct (SNumLit n side), ZPat.CursorP _ (UHPat.Pat _ (UHPat.EmptyHole _))) 
      | (Construct (SNumLit n side), ZPat.CursorP _ (UHPat.Pat _ UHPat.Wild))
      | (Construct (SNumLit n side), ZPat.CursorP _ (UHPat.Pat _ (UHPat.Var _)))
      | (Construct (SNumLit n side), ZPat.CursorP _ (UHPat.Pat _ (UHPat.NumLit _)))
      | (Construct (SNumLit n side), ZPat.CursorP _ (UHPat.Pat _ (UHPat.BoolLit _))) =>
        Some
          (ZPat.CursorP side (UHPat.Pat NotInHole (UHPat.NumLit n)), 
           HTyp.Num, 
           ctx, 
           u_gen)
      | (Construct (SNumLit _ _), _) => None
      | (Construct (SBoolLit b side), ZPat.CursorP _ (UHPat.Pat _ (UHPat.EmptyHole _))) 
      | (Construct (SBoolLit b side), ZPat.CursorP _ (UHPat.Pat _ UHPat.Wild))  
      | (Construct (SBoolLit b side), ZPat.CursorP _ (UHPat.Pat _ (UHPat.Var _)))  
      | (Construct (SBoolLit b side), ZPat.CursorP _ (UHPat.Pat _ (UHPat.NumLit _)))  
      | (Construct (SBoolLit b side), ZPat.CursorP _ (UHPat.Pat _ (UHPat.BoolLit _))) =>  
        Some
          (ZPat.CursorP side (UHPat.Pat NotInHole (UHPat.BoolLit b)), 
           HTyp.Bool, 
           ctx, 
           u_gen)
      | (Construct (SBoolLit _ _), _) => None
      | (Construct (SInj side), ZPat.CursorP _ p1) => 
        match UHExp.syn_pat fuel ctx p1 with 
        | None => None
        | Some (ty1, ctx) => 
          let zp := ZPat.Deeper NotInHole 
            (ZPat.InjZ side zp) in 
          let ty := 
            match side with 
            | L => HTyp.Sum ty1 HTyp.Hole
            | R => HTyp.Sum HTyp.Hole ty1
            end in 
          Some (zp, ty, ctx, u_gen)
        end
      | (Construct (SOp os), ZPat.Deeper _ (
          ZPat.OpSeqZ _ (ZPat.CursorP (In _) p) surround))
      | (Construct (SOp os), ZPat.Deeper _ (
          ZPat.OpSeqZ _ (ZPat.CursorP After p) surround)) => 
        match pat_op_of os with
        | None => None
        | Some op => 
          abs_perform_Construct_SOp_After_surround 
            ZPat.new_EmptyHole
            make_and_syn_OpSeqZ_pat
            UHPat.is_Space
            UHPat.Space
            ZPat.CursorP
            fuel ctx u_gen p op surround
        end
      | (Construct (SOp os), 
          ZPat.Deeper _ (ZPat.OpSeqZ _
            ((ZPat.CursorP Before _) as zp0) surround)) =>
        match pat_op_of os with 
        | None => None
        | Some op => 
          abs_perform_Construct_SOp_Before_surround
            ZPat.erase
            ZPat.new_EmptyHole
            make_and_syn_OpSeqZ_pat
            UHPat.is_Space
            UHPat.Space
            ZPat.CursorP
            fuel ctx u_gen zp0 op surround
        end
      | (Construct (SOp os), ZPat.CursorP (In _) p)
      | (Construct (SOp os), ZPat.CursorP After p) => 
        match pat_op_of os with 
        | None => None
        | Some op => 
          abs_perform_Construct_SOp_After 
            UHPat.bidelimit
            ZPat.new_EmptyHole
            make_and_syn_OpSeqZ_pat
            fuel ctx u_gen p op
        end
      | (Construct (SOp os), ZPat.CursorP Before p) => 
        match pat_op_of os with 
        | None => None
        | Some op => 
          abs_perform_Construct_SOp_Before
            UHPat.bidelimit
            ZPat.new_EmptyHole
            make_and_syn_OpSeqZ_pat
            fuel ctx u_gen p op
        end
      (* Zipper *)
      | (_, ZPat.ParenthesizedZ zp1) => 
        match perform_syn_pat fuel ctx u_gen a zp1 with 
        | None => None
        | Some (zp1, ty, ctx, u_gen) => 
          Some (
            ZPat.ParenthesizedZ zp1,
            ty,
            ctx,
            u_gen)
        end
      | (_, ZPat.Deeper _ (ZPat.InjZ side zp1)) => 
        match perform_syn_pat fuel ctx u_gen a zp1 with 
        | None => None
        | Some (zp1, ty1, ctx, u_gen) => 
          let zp := ZPat.Deeper NotInHole 
            (ZPat.InjZ side zp1) in 
          let ty := 
            match side with 
            | L => HTyp.Sum ty1 HTyp.Hole
            | R => HTyp.Sum HTyp.Hole ty1
            end in 
          Some (zp, ty, ctx, u_gen)
        end
      | (_, ZPat.Deeper _ (ZPat.OpSeqZ _ zp0 surround)) => 
        let i := OperatorSeq.surround_prefix_length surround in 
        match ZPat.erase zp with 
        | UHPat.Pat _ (UHPat.OpSeq skel seq) => 
          match UHExp.syn_skel_pat fuel ctx skel seq (Some i) with 
          | Some (ty, ctx, Some mode) =>
              match mode with 
              | UHExp.AnalyzedAgainst ty0 => 
                match perform_ana_pat fuel ctx u_gen a zp0 ty0 with 
                | None => None
                | Some (zp0, ctx, u_gen) => 
                  let zp0 := ZPat.bidelimit zp0 in  
                  Some (
                    ZPat.Deeper NotInHole (ZPat.OpSeqZ skel zp0 surround), 
                    ty, ctx, u_gen)
                end
              | UHExp.Synthesized ty0 =>
                match perform_syn_pat fuel ctx u_gen a zp0 with 
                | Some (zp0, ty0, ctx, u_gen) => 
                  let zp0 := ZPat.bidelimit zp0 in 
                  make_and_syn_OpSeqZ_pat fuel ctx u_gen zp0 surround
                | None => None
                end
              end
          | Some _ => None (* should never happen *)
          | None => None (* should never happen *)
          end
        | _ => None (* should never happen *)
        end
      | (UpdateApPalette _, _)
      | (Construct (SApPalette _), _)
      | (Construct SNum, _) 
      | (Construct SBool, _)
      | (Construct SAsc, _) 
      | (Construct SLet, _) 
      | (Construct SLam, _) 
      | (Construct SCase, _)
      | (Construct SRule, _) => None
      end
      end
    with perform_ana_pat 
      (fuel : Fuel.t)
      (ctx : Contexts.t)
      (u_gen : MetaVarGen.t)
      (a : t)
      (zp : ZPat.t)
      (ty : HTyp.t)
      : option(ZPat.t * Contexts.t * MetaVarGen.t) := 
      match fuel with
      | Fuel.Kicked => None
      | Fuel.More fuel =>
      match (a, zp) with
      (* Movement *)
      (* NOTE: we don't need to handle movement actions here for the purposes of the UI,
       * since it's handled at the top (expression) level, but for the sake of API completeness
       * we include it *)
      | (MoveTo path, _) => 
        let p := ZPat.erase zp in
        match Path.follow_pat fuel path p with
        | Some zp => Some (zp, ctx, u_gen)
        | None => None
        end
      | (MoveToPrevHole, _) =>
        match Path.prev_hole_path_pat fuel zp with
        | None => None
        | Some path => perform_ana_pat fuel ctx u_gen (MoveTo path) zp ty
        end
      | (MoveToNextHole, _) =>
        match Path.next_hole_path_pat fuel zp with
        | None => None
        | Some path => perform_ana_pat fuel ctx u_gen (MoveTo path) zp ty
        end
      (* switch to synthesis if in a hole *)
      | (_, ZPat.Deeper (InHole u) zp1) => 
        let zp1_not_in_hole := ZPat.Deeper NotInHole zp1 in 
        let p1 := ZPat.erase zp1_not_in_hole in 
        match UHExp.syn_pat fuel ctx p1 with 
        | None => None
        | Some (ty1, _) => 
          match perform_syn_pat fuel ctx u_gen a zp1_not_in_hole with 
          | None => None
          | Some (zp1, ty', ctx, u_gen) => 
            if HTyp.consistent ty ty' then 
              Some (zp1, ctx, u_gen)
            else 
              Some (ZPat.put_in_hole u zp1, ctx, u_gen)
          end
        end
      (* Backspace and Delete *)
      | (Backspace, ZPat.CursorP After p) => 
        match p with 
        | UHPat.Pat _ (UHPat.EmptyHole _) => 
          Some (ZPat.CursorP Before p, ctx, u_gen)
        | _ => 
          let (p, u_gen) := UHPat.new_EmptyHole u_gen in 
          Some (ZPat.CursorP Before p, ctx, u_gen)
        end
      | (Backspace, ZPat.CursorP Before p) => None
      | (Delete, ZPat.CursorP Before p) => 
        match p with 
        | UHPat.Pat _ (UHPat.EmptyHole _) => 
          Some (ZPat.CursorP After p, ctx, u_gen)
        | _ => 
          let (e', u_gen') := UHExp.new_EmptyHole u_gen in 
          Some (ZPat.CursorP Before p, ctx, u_gen)
        end
      | (Backspace, ZPat.CursorP (In _) _)
      | (Delete, ZPat.CursorP (In _) _) => 
        let (p, u_gen) := UHPat.new_EmptyHole u_gen in 
        let zp := ZPat.CursorP Before p in 
        Some (zp, ctx, u_gen)
      | (Delete, ZPat.CursorP After _) => None
      (* Construct *)
      | (Construct SParenthesized, ZPat.CursorP _ p) => 
        match UHExp.ana_pat fuel ctx p ty with 
        | None => None
        | Some ctx => 
          Some (
            ZPat.ParenthesizedZ zp, 
            ctx,
            u_gen)
        end
      | (Construct (SVar x side), ZPat.CursorP _ (UHPat.Pat _ (UHPat.EmptyHole _))) 
      | (Construct (SVar x side), ZPat.CursorP _ (UHPat.Pat _ UHPat.Wild))
      | (Construct (SVar x side), ZPat.CursorP _ (UHPat.Pat _ (UHPat.Var _)))
      | (Construct (SVar x side), ZPat.CursorP _ (UHPat.Pat _ (UHPat.NumLit _)))
      | (Construct (SVar x side), ZPat.CursorP _ (UHPat.Pat _ (UHPat.BoolLit _))) => 
        Var.check_valid x (
        let ctx := Contexts.extend_gamma ctx (x, ty) in 
        Some
          (ZPat.CursorP side (UHPat.Pat NotInHole (UHPat.Var x)), 
           ctx, 
           u_gen)
        )
      | (Construct (SVar _ _), _) => None
      | (Construct SWild, ZPat.CursorP _ (UHPat.Pat _ (UHPat.EmptyHole _))) 
      | (Construct SWild, ZPat.CursorP _ (UHPat.Pat _ UHPat.Wild))
      | (Construct SWild, ZPat.CursorP _ (UHPat.Pat _ (UHPat.Var _)))
      | (Construct SWild, ZPat.CursorP _ (UHPat.Pat _ (UHPat.NumLit _))) 
      | (Construct SWild, ZPat.CursorP _ (UHPat.Pat _ (UHPat.BoolLit _))) =>
        Some
          (ZPat.CursorP After (UHPat.Pat NotInHole UHPat.Wild), 
           ctx, 
           u_gen)
      | (Construct SWild, _) => None
      | (Construct (SInj side), ZPat.CursorP cursor_side p1) => 
        match HTyp.matched_sum ty with 
        | Some (tyL, tyR) => 
          let ty1 := pick_side side tyL tyR in 
          match UHExp.ana_pat_fix_holes fuel ctx u_gen false p1 ty1 with 
          | None => None
          | Some (p1, ctx, u_gen) => 
            let zp := 
              ZPat.Deeper NotInHole
                (ZPat.InjZ side 
                  (ZPat.CursorP cursor_side p1)) in 
            Some (zp, ctx, u_gen)
          end
        | None => 
          match UHExp.syn_pat_fix_holes fuel ctx u_gen false p1 with 
          | None => None
          | Some (p1, _, ctx, u_gen) => 
            let (u, u_gen) := MetaVarGen.next u_gen in 
            let zp := 
              ZPat.Deeper (InHole u)
                (ZPat.InjZ side 
                  (ZPat.CursorP cursor_side p1)) in 
            Some (zp, ctx, u_gen)
          end
        end
      | (Construct (SOp os), ZPat.Deeper _ (
          ZPat.OpSeqZ _ (ZPat.CursorP (In _) p) surround))
      | (Construct (SOp os), ZPat.Deeper _ (
          ZPat.OpSeqZ _ (ZPat.CursorP After p) surround)) => 
        match pat_op_of os with
        | None => None
        | Some op => 
          abs_perform_Construct_SOp_After_surround 
            ZPat.new_EmptyHole
            (fun fuel ctx u_gen zp surround => 
              make_and_ana_OpSeqZ_pat fuel ctx u_gen zp surround ty)
            UHPat.is_Space
            UHPat.Space
            ZPat.CursorP
            fuel ctx u_gen p op surround
        end
      | (Construct (SOp os), 
          ZPat.Deeper _ (ZPat.OpSeqZ _
            ((ZPat.CursorP Before _) as zp0) surround)) =>
        match pat_op_of os with 
        | None => None
        | Some op => 
          abs_perform_Construct_SOp_Before_surround
            ZPat.erase
            ZPat.new_EmptyHole
            (fun fuel ctx u_gen zp surround => 
              make_and_ana_OpSeqZ_pat fuel ctx u_gen zp surround ty)
            UHPat.is_Space
            UHPat.Space
            ZPat.CursorP
            fuel ctx u_gen zp0 op surround
        end
      | (Construct (SOp os), ZPat.CursorP (In _) p)
      | (Construct (SOp os), ZPat.CursorP After p) => 
        match pat_op_of os with 
        | None => None
        | Some op => 
          abs_perform_Construct_SOp_After 
            UHPat.bidelimit
            ZPat.new_EmptyHole
            (fun fuel ctx u_gen zp surround => 
              make_and_ana_OpSeqZ_pat fuel ctx u_gen zp surround ty)
            fuel ctx u_gen p op
        end
      | (Construct (SOp os), ZPat.CursorP Before p) => 
        match pat_op_of os with 
        | None => None
        | Some op => 
          abs_perform_Construct_SOp_Before
            UHPat.bidelimit
            ZPat.new_EmptyHole
            (fun fuel ctx u_gen zp surround => 
              make_and_ana_OpSeqZ_pat fuel ctx u_gen zp surround ty)
            fuel ctx u_gen p op
        end
      (* Zipper *)
      | (_, ZPat.ParenthesizedZ zp1) => 
        match perform_ana_pat fuel ctx u_gen a zp1 ty with 
        | None => None
        | Some (zp1, ctx, u_gen) => 
          Some (
            ZPat.ParenthesizedZ zp1,
            ctx,
            u_gen)
        end
      | (_, ZPat.Deeper _ (ZPat.InjZ side zp1)) => 
        match HTyp.matched_sum ty with 
        | None => None
        | Some (tyL, tyR) => 
          let ty1 := pick_side side tyL tyR in 
          match perform_ana_pat fuel ctx u_gen a zp1 ty1 with 
          | None => None
          | Some (zp1, ctx, u_gen) => 
            let zp := ZPat.Deeper NotInHole (ZPat.InjZ side zp1) in 
            Some (zp, ctx, u_gen)
          end
        end
      | (_, ZPat.Deeper _ (ZPat.OpSeqZ _ zp0 surround)) => 
        let i := OperatorSeq.surround_prefix_length surround in 
        match ZPat.erase zp with 
        | UHPat.Pat _ (UHPat.OpSeq skel seq) => 
          match UHExp.ana_skel_pat fuel ctx skel seq ty (Some i) with 
          | Some (ctx, Some mode) =>
              match mode with 
              | UHExp.AnalyzedAgainst ty0 => 
                match perform_ana_pat fuel ctx u_gen a zp0 ty0 with 
                | None => None
                | Some (zp0, ctx, u_gen) => 
                  let zp0 := ZPat.bidelimit zp0 in  
                  Some (
                    ZPat.Deeper NotInHole (ZPat.OpSeqZ skel zp0 surround), 
                    ctx, u_gen)
                end
              | UHExp.Synthesized ty0 =>
                match perform_syn_pat fuel ctx u_gen a zp0 with 
                | Some (zp0, ty0, ctx, u_gen) => 
                  let zp0 := ZPat.bidelimit zp0 in 
                  make_and_ana_OpSeqZ_pat fuel ctx u_gen zp0 surround ty 
                | None => None
                end
              end
          | Some _ => None (* should never happen *)
          | None => None (* should never happen *)
          end
        | _ => None (* should never happen *)
        end
      (* Subsumption *)
      | (Construct (SNumLit _ _), _)
      | (Construct (SBoolLit _ _), _) => 
        match perform_syn_pat fuel ctx u_gen a zp with 
        | None => None
        | Some (zp, ty', ctx, u_gen) => 
          if HTyp.consistent ty ty' then 
            Some (zp, ctx, u_gen)
          else 
            let (zp, u_gen) := ZPat.put_in_new_hole u_gen zp in
            Some (zp, ctx, u_gen)
        end
      (* Invalid actions at the pattern level *)
      | (UpdateApPalette _, _)
      | (Construct (SApPalette _), _) 
      | (Construct SNum, _) 
      | (Construct SBool, _) 
      | (Construct SAsc, _) 
      | (Construct SLet, _) 
      | (Construct SLam, _)
      | (Construct SCase, _) 
      | (Construct SRule, _) => None 
      end
      end.

    Definition zexp_syn_fix_holes
      (fuel : Fuel.t)
      (ctx : Contexts.t)
      (u_gen : MetaVarGen.t)
      (ze : ZExp.t)
      : option (ZExp.t * HTyp.t * MetaVarGen.t) := 
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
      (ctx : Contexts.t)
      (u_gen : MetaVarGen.t)
      (ze : ZExp.t)
      (ty : HTyp.t)
      : option (ZExp.t * MetaVarGen.t) := 
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
      (ctx : Contexts.t)
      (u_gen : MetaVarGen.t)
      (ze0 : ZExp.t)
      (surround : ZExp.opseq_surround)
      : option (ZExp.t * HTyp.t * MetaVarGen.t) := 
        (* figure out the current path so that we can follow it again 
         * to reconstitute the Z-exp after calling into the UHExp hole 
         * insertion logic (otherwise we'd have to do a version of that
         * logic specific to Z-exps) *)
        let path0 := Path.of_OpSeqZ ze0 surround in 
        let e0 := ZExp.erase ze0 in 
        let seq := OperatorSeq.opseq_of_exp_and_surround e0 surround in 
        let skel := Associator.associate_exp seq in 
        match UHExp.syn_skel_fix_holes fuel ctx u_gen false skel seq with 
        | Some (skel', seq', ty, u_gen') => 
          let e' := UHExp.Tm NotInHole (UHExp.OpSeq skel' seq') in 
          match Path.follow_e fuel path0 e' with 
          | Some ze' => Some (ze', ty, u_gen')
          | None => None
          end
        | None => None
        end.

    Definition make_and_ana_OpSeqZ 
      (fuel : Fuel.t)
      (ctx : Contexts.t)
      (u_gen : MetaVarGen.t)
      (ze0 : ZExp.t)
      (surround : ZExp.opseq_surround)
      (ty : HTyp.t)
      : option (ZExp.t * MetaVarGen.t) := 
        (* figure out the current path so that we can follow it again 
         * to reconstitute the Z-exp after calling into the UHExp hole 
         * insertion logic (otherwise we'd have to do a version of that
         * logic specific to Z-exps) *)
        let path0 := Path.of_OpSeqZ ze0 surround in 
        let e0 := ZExp.erase ze0 in 
        let seq := OperatorSeq.opseq_of_exp_and_surround e0 surround in 
        let skel := Associator.associate_exp seq in 
        match UHExp.ana_skel_fix_holes fuel ctx u_gen false skel seq ty with 
        | Some (skel', seq', u_gen') => 
          let e' := UHExp.Tm NotInHole (UHExp.OpSeq skel' seq') in 
          match Path.follow_e fuel path0 e' with 
          | Some ze' => Some (ze', u_gen')
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

    Fixpoint perform_syn 
        (fuel: Fuel.t) 
        (ctx: Contexts.t) 
        (a: t) 
        (ze_ty: (ZExp.t * HTyp.t) * MetaVarGen.t) 
        : option ((ZExp.t * HTyp.t) * MetaVarGen.t) :=
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
      | (MoveToPrevHole, _) =>
        match Path.prev_hole_path fuel ze with
        | None => None
        | Some path => perform_syn fuel ctx (MoveTo path) ze_ty
        end
      | (MoveToNextHole, _) =>
        match Path.next_hole_path fuel ze with
        | None => None
        | Some path =>
          (* let path := Helper.log_path path in *)
          perform_syn fuel ctx (MoveTo path) ze_ty
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
      | (Backspace, ZExp.CursorE Before e) => None
      | (Delete, ZExp.CursorE Before e) => 
        match e with 
        | UHExp.Tm _ (UHExp.EmptyHole _) => 
          Some (ZExp.CursorE After e, ty, u_gen)
        | _ => 
          let (e', u_gen') := UHExp.new_EmptyHole u_gen in 
          Some (ZExp.CursorE Before e', HTyp.Hole, u_gen)
        end
      | (Delete, ZExp.CursorE After e) => None
      | (Backspace, 
          ZExp.Deeper _ (ZExp.AscZ2 e1 
            (ZTyp.CursorT Before _))) 
      | (Backspace,
          ZExp.Deeper _ (ZExp.AscZ2 e1 
            (ZTyp.OpSeqZ _ 
              (ZTyp.CursorT Before _)
              (OperatorSeq.EmptyPrefix _)))) => 
        let ze' := ZExp.CursorE After e1 in 
        zexp_syn_fix_holes fuel ctx u_gen ze'
      | (Delete,
          ZExp.Deeper _ (ZExp.AscZ1
            (ZExp.CursorE After e1)
            _)) => 
        match UHExp.syn_fix_holes fuel ctx u_gen e1 with 
        | Some (e1', ty', u_gen) => 
          let ze' := ZExp.CursorE After e1' in 
          Some (ze', ty', u_gen)
        | None => None
        end
      | (Backspace, ZExp.Deeper _ 
          (ZExp.LetZA p (ZTyp.CursorT Before _) e1 e2))
      | (Backspace, ZExp.Deeper _
          (ZExp.LetZA p  
            (ZTyp.OpSeqZ _
              (ZTyp.CursorT Before _)
              (OperatorSeq.EmptyPrefix _)) e1 e2)) =>  
        match UHExp.syn_fix_holes fuel ctx u_gen e1 with 
        | None => None
        | Some (e1, ty1, u_gen) => 
          match UHExp.ana_pat_fix_holes fuel ctx u_gen false p ty1 with 
          | None => None
          | Some (p, ctx, u_gen) => 
            match UHExp.syn_fix_holes fuel ctx u_gen e2 with 
            | None => None
            | Some (e2, ty, u_gen) => 
              let ze := 
                ZExp.Deeper NotInHole
                  (ZExp.LetZP (ZPat.CursorP After p) None e1 e2) in 
              Some (ze, ty, u_gen)
            end
          end
        end
      | (Delete, ZExp.Deeper _
          (ZExp.LetZP ((ZPat.CursorP After _) as zp) (Some _) e1 e2))
      | (Delete, ZExp.Deeper _
          (ZExp.LetZP 
            ((ZPat.Deeper _ 
              (ZPat.OpSeqZ _ (ZPat.CursorP After _) 
                 (OperatorSeq.EmptySuffix _))) as zp)
            (Some _)
            e1 e2)) => 
        match UHExp.syn_fix_holes fuel ctx u_gen e1 with 
        | None => None
        | Some (e1, ty1, u_gen) => 
          match ana_zpat_fix_holes fuel ctx u_gen zp ty1 with 
          | None => None
          | Some (zp, ctx, u_gen) => 
            match UHExp.syn_fix_holes fuel ctx u_gen e2 with 
            | None => None
            | Some (e2, ty, u_gen) => 
              let ze := 
                ZExp.Deeper NotInHole
                  (ZExp.LetZP zp None e1 e2) in 
              Some (ze, ty, u_gen)
            end
          end
        end
      | (Backspace, ZExp.Deeper _ 
          (ZExp.LamZA p (ZTyp.CursorT Before _) e1))
      | (Backspace, ZExp.Deeper _
          (ZExp.LamZA p 
            (ZTyp.OpSeqZ _
              (ZTyp.CursorT Before _)
              (OperatorSeq.EmptyPrefix _)) e1)) => 
        match UHExp.ana_pat_fix_holes fuel ctx u_gen false p HTyp.Hole with 
        | None => None
        | Some (p, ctx, u_gen) => 
          match UHExp.syn_fix_holes fuel ctx u_gen e1 with 
          | None => None
          | Some (e1, ty2, u_gen) => 
            let ze := ZExp.Deeper NotInHole 
              (ZExp.LamZP (ZPat.CursorP After p) None e1) in 
            Some (ze, HTyp.Arrow HTyp.Hole ty2, u_gen)
          end
        end
      | (Delete, ZExp.Deeper _
          (ZExp.LamZP ((ZPat.CursorP After _) as zp) (Some _) e1))
      | (Delete, ZExp.Deeper _
          (ZExp.LamZP
            ((ZPat.Deeper _
              (ZPat.OpSeqZ _
                (ZPat.CursorP After _)
                (OperatorSeq.EmptySuffix _))) as zp)
            (Some _)
            e1)) => 
        match ana_zpat_fix_holes fuel ctx u_gen zp HTyp.Hole with 
        | None => None
        | Some (zp, ctx, u_gen) => 
          match UHExp.syn_fix_holes fuel ctx u_gen e1 with 
          | None => None
          | Some (e1, ty2, u_gen) => 
            let ze := ZExp.Deeper NotInHole (ZExp.LamZP zp None e1) in 
            Some (ze, HTyp.Arrow HTyp.Hole ty2, u_gen)
          end
        end
      | (Backspace, ZExp.Deeper _
          (ZExp.OpSeqZ _
            ((ZExp.CursorE Before e0) as ze0) 
            ((OperatorSeq.EmptySuffix _) as surround)))
      | (Backspace, ZExp.Deeper _
          (ZExp.OpSeqZ _
            ((ZExp.CursorE Before e0) as ze0) 
            ((OperatorSeq.BothNonEmpty _ _) as surround))) =>
        abs_perform_Backspace_Before_op 
          combine_for_Backspace_Space
          zexp_syn_fix_holes
          make_and_syn_OpSeqZ
          UHExp.is_EmptyHole
          UHExp.is_Space
          UHExp.Space
          ZExp.CursorE
          fuel ctx u_gen e0 ze0 surround
      | (Delete, ZExp.Deeper _
          (ZExp.OpSeqZ _
            ((ZExp.CursorE After e0) as ze0)
            ((OperatorSeq.EmptyPrefix _) as surround)))
      | (Delete, ZExp.Deeper _ 
          (ZExp.OpSeqZ _
            ((ZExp.CursorE After e0) as ze0)
            ((OperatorSeq.BothNonEmpty _ _) as surround))) => 
        abs_perform_Delete_After_op
          combine_for_Delete_Space
          zexp_syn_fix_holes
          make_and_syn_OpSeqZ
          UHExp.is_EmptyHole
          UHExp.is_Space
          UHExp.Space
          ZExp.CursorE
          fuel ctx u_gen e0 ze0 surround
      | (Backspace, ZExp.CursorE (In _) e)
      | (Delete, ZExp.CursorE (In _) e) => 
        let (e', u_gen') := UHExp.new_EmptyHole u_gen in 
        let ze' := ZExp.CursorE Before e' in 
        Some (ze', HTyp.Hole, u_gen')
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
      | (Construct SAsc, ZExp.Deeper err_status (ZExp.LetZP zp None e1 e2)) => 
        match UHExp.syn fuel ctx e1 with 
        | None => None
        | Some ty1 => 
          let uty1 := UHTyp.contract ty1 in 
          let ze := ZExp.Deeper err_status 
            (ZExp.LetZA (ZPat.erase zp) (ZTyp.place_Before uty1) e1 e2) in 
          Some (ze, ty, u_gen)
        end
      | (Construct SAsc, ZExp.Deeper err_status (ZExp.LamZP zp None e1)) => 
        let ze := ZExp.Deeper err_status 
          (ZExp.LamZA (ZPat.erase zp) (ZTyp.place_Before UHTyp.Hole) e1) in 
        Some (ze, ty, u_gen)
      | (Construct SAsc, ZExp.Deeper err_status (ZExp.LetZP zp (Some uty1) e1 e2)) => 
        (* just move the cursor over if there is already an ascription *)
        let ze := ZExp.Deeper err_status
          (ZExp.LetZA (ZPat.erase zp) (ZTyp.place_Before uty1) e1 e2) in 
        Some (ze, ty, u_gen)
      | (Construct SAsc, ZExp.Deeper err_status (ZExp.LamZP zp (Some uty1) e1)) => 
        (* just move the cursor over if there is already an ascription *)
        let ze := ZExp.Deeper err_status
          (ZExp.LamZA (ZPat.erase zp) (ZTyp.place_Before uty1) e1) in 
        Some (ze, ty, u_gen)
      | (Construct (SVar x side), ZExp.CursorE _ (UHExp.Tm _ (UHExp.EmptyHole _))) 
      | (Construct (SVar x side), ZExp.CursorE _ (UHExp.Tm _ (UHExp.Var _ _)))
      | (Construct (SVar x side), ZExp.CursorE _ (UHExp.Tm _ (UHExp.NumLit _))) 
      | (Construct (SVar x side), ZExp.CursorE _ (UHExp.Tm _ (UHExp.BoolLit _))) =>
        Var.check_valid x ( 
        let (gamma, _) := ctx in 
        match VarMap.lookup gamma x with
        | Some xty => Some (ZExp.CursorE side 
          (UHExp.Tm NotInHole (UHExp.Var NotInVHole x)), 
          xty, u_gen)
        | None => 
          let (u, u_gen) := MetaVarGen.next u_gen in 
          Some (ZExp.CursorE side
            (UHExp.Tm NotInHole (UHExp.Var (InVHole u) x)),
            HTyp.Hole, u_gen)
        end)
      | (Construct (SVar _ _), _) => None
      | (Construct SLet, ZExp.CursorE _ e1) =>
        let (zp, u_gen) := ZPat.new_EmptyHole u_gen in 
        let (e2, u_gen) := UHExp.new_EmptyHole u_gen in 
        let ze := ZExp.Deeper NotInHole 
          (ZExp.LetZP zp None e1 e2) in 
        Some (ze, HTyp.Hole, u_gen)
      | (Construct SLam, ZExp.CursorE _ e1) =>
        let (zp, u_gen) := ZPat.new_EmptyHole u_gen in 
        let ze := 
          ZExp.Deeper NotInHole 
            (ZExp.LamZP zp (Some UHTyp.Hole) e1) in 
        let ty' := HTyp.Arrow HTyp.Hole ty in 
        Some (ze, ty', u_gen)
      | (Construct (SNumLit n side), ZExp.CursorE _ (UHExp.Tm _ (UHExp.EmptyHole _)))
      | (Construct (SNumLit n side), ZExp.CursorE _ (UHExp.Tm _ (UHExp.NumLit _)))
      | (Construct (SNumLit n side), ZExp.CursorE _ (UHExp.Tm _ (UHExp.BoolLit _)))
      | (Construct (SNumLit n side), ZExp.CursorE _ (UHExp.Tm _ (UHExp.Var _ _))) =>
          Some (ZExp.CursorE side (UHExp.Tm NotInHole (UHExp.NumLit n)), HTyp.Num, u_gen)
      | (Construct (SNumLit _ _), _) => None
      | (Construct (SBoolLit b side), ZExp.CursorE _ (UHExp.Tm _ (UHExp.EmptyHole _)))
      | (Construct (SBoolLit b side), ZExp.CursorE _ (UHExp.Tm _ (UHExp.NumLit _)))
      | (Construct (SBoolLit b side), ZExp.CursorE _ (UHExp.Tm _ (UHExp.BoolLit _)))
      | (Construct (SBoolLit b side), ZExp.CursorE _ (UHExp.Tm _ (UHExp.Var _ _))) =>
          Some (ZExp.CursorE side (UHExp.Tm NotInHole (UHExp.BoolLit b)), HTyp.Num, u_gen)
      | (Construct (SBoolLit _ _), _) => None
      | (Construct (SInj side), (ZExp.CursorE _ e)) => 
        let ze' := 
          ZExp.Deeper NotInHole 
            (ZExp.InjZ side ze) in 
        let ty' := 
          match side with 
          | L => HTyp.Sum ty HTyp.Hole
          | R => HTyp.Sum HTyp.Hole ty 
          end in 
        Some (ze', ty', u_gen)
      | (Construct SCase, (ZExp.CursorE _ e1)) =>
        match e1 with 
        | UHExp.Tm _ (UHExp.EmptyHole _) => 
          let (rule_p, u_gen) := UHPat.new_EmptyHole u_gen in 
          let (rule_e, u_gen) := UHExp.new_EmptyHole u_gen in  
          let rule := UHExp.Rule rule_p rule_e in 
          let rules := cons rule nil in 
          let caseze := ZExp.Deeper NotInHole (ZExp.CaseZE ze rules) in 
          let ze := ZExp.Deeper NotInHole (ZExp.AscZ1 caseze (UHTyp.Hole)) in 
          Some (ze, HTyp.Hole, u_gen) 
        | _ => 
          let (zp, u_gen) := ZPat.new_EmptyHole u_gen in 
          let (rule_e, u_gen) := UHExp.new_EmptyHole u_gen in 
          let zrule := ZExp.RuleZP zp rule_e in  
          let zrules := ZList.singleton zrule in 
          let caseze := ZExp.Deeper NotInHole (ZExp.CaseZR e1 zrules) in 
          let ze := ZExp.Deeper NotInHole (ZExp.AscZ1 caseze (UHTyp.Hole)) in 
          Some (ze, HTyp.Hole, u_gen) 
        end  
      | (Construct (SOp os), ZExp.Deeper _ (
          ZExp.OpSeqZ _ (ZExp.CursorE (In _) e) surround))
      | (Construct (SOp os), ZExp.Deeper _ (
          ZExp.OpSeqZ _ (ZExp.CursorE After e) surround)) => 
        match exp_op_of os with
        | None => None
        | Some op => 
          abs_perform_Construct_SOp_After_surround 
            ZExp.new_EmptyHole
            make_and_syn_OpSeqZ
            UHExp.is_Space
            UHExp.Space
            ZExp.CursorE
            fuel ctx u_gen e op surround
        end
      | (Construct (SOp os), 
          ZExp.Deeper _ (ZExp.OpSeqZ _
            ((ZExp.CursorE Before _) as ze0) surround)) =>
        match exp_op_of os with 
        | None => None
        | Some op => 
          abs_perform_Construct_SOp_Before_surround
            ZExp.erase
            ZExp.new_EmptyHole
            make_and_syn_OpSeqZ
            UHExp.is_Space
            UHExp.Space
            ZExp.CursorE
            fuel ctx u_gen ze0 op surround
        end
      | (Construct (SOp os), ZExp.CursorE (In _) e)
      | (Construct (SOp os), ZExp.CursorE After e) => 
        match exp_op_of os with 
        | None => None
        | Some op => 
          abs_perform_Construct_SOp_After 
            UHExp.bidelimit
            ZExp.new_EmptyHole
            make_and_syn_OpSeqZ
            fuel ctx u_gen e op
        end
      | (Construct (SOp os), ZExp.CursorE Before e) => 
        match exp_op_of os with 
        | None => None
        | Some op => 
          abs_perform_Construct_SOp_Before
            UHExp.bidelimit
            ZExp.new_EmptyHole
            make_and_syn_OpSeqZ
            fuel ctx u_gen e op
        end
      | (Construct SRule, _) => None
      | (Construct (SApPalette name), ZExp.CursorE _ (UHExp.Tm _ (UHExp.EmptyHole _))) => 
        let (_, palette_ctx) := ctx in 
        match PaletteCtx.lookup palette_ctx name with 
        | Some palette_defn => 
          let m_initial_model := UHExp.PaletteDefinition.initial_model palette_defn in 
          let (q, u_gen) := UHExp.HoleRefs.exec m_initial_model (UHExp.PaletteHoleData.empty) u_gen in 
          let (initial_model, initial_hole_data) := q in  
          let expansion_ty := UHExp.PaletteDefinition.expansion_ty palette_defn in
          let expansion := (UHExp.PaletteDefinition.to_exp palette_defn) initial_model in 
          let (_, initial_hole_map) := initial_hole_data in
          let expansion_ctx := UHExp.PaletteHoleData.extend_ctx_with_hole_map ctx initial_hole_map in
          match (UHExp.ana fuel expansion_ctx expansion expansion_ty) with 
          | Some _ => 
            Some (ZExp.CursorE After (UHExp.Tm NotInHole (UHExp.ApPalette name initial_model initial_hole_data)), 
                  expansion_ty, u_gen)
          | None => None
          end
        | None => None
        end
      | (Construct (SApPalette _), _) => None
      | (UpdateApPalette monad, 
          ZExp.CursorE _ (UHExp.Tm _ (UHExp.ApPalette name _ hole_data))) => 
        let (_, palette_ctx) := ctx in 
        match PaletteCtx.lookup palette_ctx name with 
        | Some palette_defn => 
          let (q, u_gen') := UHExp.HoleRefs.exec monad hole_data u_gen in
          let (serialized_model, hole_data') := q in 
          let expansion_ty := UHExp.PaletteDefinition.expansion_ty palette_defn in
          let expansion := (UHExp.PaletteDefinition.to_exp palette_defn) serialized_model in 
          let (_, hole_map') := hole_data' in
          let expansion_ctx := UHExp.PaletteHoleData.extend_ctx_with_hole_map ctx hole_map' in
          match (UHExp.ana fuel expansion_ctx expansion expansion_ty) with 
          | Some _ => 
            Some (ZExp.CursorE After (UHExp.Tm NotInHole (UHExp.ApPalette name serialized_model hole_data')), 
                  expansion_ty, u_gen)
          | None => None
          end
        | None => None
        end
      | (UpdateApPalette _, _) => None
      (* Zipper Cases *)
      | (_, ZExp.ParenthesizedZ ze1) => 
        match perform_syn fuel ctx a (ze1, ty, u_gen) with 
        | Some (ze1', ty', u_gen') => 
          Some (
            ZExp.ParenthesizedZ ze1',
            ty',
            u_gen')
        | None => None
        end
      | (_, ZExp.Deeper _ (ZExp.AscZ1 ze uty1)) =>
        let ty1 := UHTyp.expand fuel uty1 in 
        match perform_ana fuel u_gen ctx a ze ty1 with 
        | Some (ze', u_gen') => 
          let ze'' := ZExp.bidelimit ze' in 
          Some (
            ZExp.Deeper NotInHole (ZExp.AscZ1 ze'' uty1), 
            ty, 
            u_gen')
        | None => None
        end
      | (_, ZExp.Deeper _ (ZExp.AscZ2 e zty)) =>
        match perform_ty fuel a zty with 
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
      | (_, ZExp.Deeper _ (ZExp.LetZP zp ann e1 e2)) => 
        match ann with 
        | Some uty1 => 
          let ty1 := UHTyp.expand fuel uty1 in 
          match perform_ana_pat fuel ctx u_gen a zp ty1 with 
          | None => None
          | Some (zp, ctx2, u_gen) => 
            let p := ZPat.erase zp in 
            let ctx1 := UHExp.ctx_for_let ctx p ty1 e1 in 
            match UHExp.ana_fix_holes fuel ctx1 u_gen e1 ty1 with 
            | None => None
            | Some (e1, u_gen) => 
              match UHExp.syn_fix_holes fuel ctx2 u_gen e2 with 
              | None => None
              | Some (e2, ty, u_gen) => 
                let ze := ZExp.Deeper NotInHole (ZExp.LetZP zp ann e1 e2) in 
                Some (ze, ty, u_gen)
              end
            end
          end
        | None => 
          match UHExp.syn fuel ctx e1 with
          | None => None
          | Some ty1 => 
            match perform_ana_pat fuel ctx u_gen a zp ty1 with 
            | None => None
            | Some (zp, ctx2, u_gen) => 
              match UHExp.syn_fix_holes fuel ctx2 u_gen e2 with 
              | None => None
              | Some (e2, ty, u_gen) => 
                let ze := ZExp.Deeper NotInHole (ZExp.LetZP zp ann e1 e2) in 
                Some (ze, ty, u_gen)
              end
            end
          end
        end 
      | (_, ZExp.Deeper _ (ZExp.LetZA p zann e1 e2)) => 
        (* (ctx) let p (ctx2) : ty = (ctx1) e1 in (ctx2) e2 *) 
        match perform_ty fuel a zann with 
        | None => None
        | Some zann => 
          let ty1 := UHTyp.expand fuel (ZTyp.erase zann) in 
          match UHExp.ana_pat_fix_holes fuel ctx u_gen false p ty1 with 
          | None => None
          | Some (p, ctx2, u_gen) => 
            let ctx1 := UHExp.ctx_for_let ctx p ty1 e1 in 
            match UHExp.ana_fix_holes fuel ctx1 u_gen e1 ty1 with 
            | None => None
            | Some (e1, u_gen) => 
              match UHExp.syn_fix_holes fuel ctx2 u_gen e2 with 
              | None => None
              | Some (e2, ty, u_gen) => 
                let ze := ZExp.Deeper NotInHole (ZExp.LetZA p zann e1 e2) in 
                Some (ze, ty, u_gen)
              end
            end
          end
        end
      | (_, ZExp.Deeper _ (ZExp.LetZE1 p ann ze1 e2)) =>
        match ann with 
        | Some ann_ty => 
          let ty1 := UHTyp.expand fuel ann_ty in 
          let ctx1 := UHExp.ctx_for_let ctx p ty1 (ZExp.erase ze1) in  
          match perform_ana fuel u_gen ctx1 a ze1 ty1 with 
          | None => None
          | Some (ze1, u_gen) => 
            let ze := ZExp.Deeper NotInHole (ZExp.LetZE1 p ann ze1 e2) in 
            Some (ze, ty, u_gen)
          end
        | None => 
          let e1 := ZExp.erase ze1 in
          match UHExp.syn fuel ctx e1 with 
          | None => None
          | Some ty1 => 
            match perform_syn fuel ctx a (ze1, ty1, u_gen) with
            | None => None
            | Some (ze1, ty1, u_gen) => 
              match UHExp.ana_pat_fix_holes fuel ctx u_gen false p ty1 with 
              | None => None
              | Some (p, ctx2, u_gen) => 
                match UHExp.syn_fix_holes fuel ctx2 u_gen e2 with 
                | None => None
                | Some (e2, ty, u_gen) => 
                  let ze := ZExp.Deeper NotInHole (ZExp.LetZE1 p ann ze1 e2) in 
                  Some (ze, ty, u_gen)
                end
              end
            end
          end
        end
      | (_, ZExp.Deeper _ (ZExp.LetZE2 p ann e1 ze2)) =>
        let ty1 := 
          match ann with 
          | Some uty1 => Some (UHTyp.expand fuel uty1) 
          | None => UHExp.syn fuel ctx e1
          end in 
        match ty1 with 
        | None => None
        | Some ty1 => 
          match UHExp.ana_pat fuel ctx p ty1 with 
          | None => None
          | Some ctx2 => 
            match perform_syn fuel ctx2 a (ze2, ty, u_gen) with 
            | None => None
            | Some (ze2, ty, u_gen) => 
              let ze := ZExp.Deeper NotInHole (ZExp.LetZE2 p ann e1 ze2) in 
              Some (ze, ty, u_gen)
            end
          end
        end
      | (_, ZExp.Deeper _ (ZExp.LamZP zp ann e1)) => 
        let ty1 := 
          match ann with 
          | Some uty1 => UHTyp.expand fuel uty1
          | None => HTyp.Hole
          end in 
        match perform_ana_pat fuel ctx u_gen a zp ty1 with 
        | None => None
        | Some (zp, ctx, u_gen) => 
          match UHExp.syn_fix_holes fuel ctx u_gen e1 with 
          | None => None
          | Some (e1, ty2, u_gen) => 
            let ty := HTyp.Arrow ty1 ty2 in 
            let ze := ZExp.Deeper NotInHole (ZExp.LamZP zp ann e1) in 
            Some (ze, ty, u_gen)
          end
        end
      | (_, ZExp.Deeper _ (ZExp.LamZA p zann e1)) => 
        match perform_ty fuel a zann with 
        | None => None
        | Some zann => 
          let ty1 := UHTyp.expand fuel (ZTyp.erase zann) in 
          match UHExp.ana_pat_fix_holes fuel ctx u_gen false p ty1 with 
          | None => None
          | Some (p, ctx, u_gen) => 
            match UHExp.syn_fix_holes fuel ctx u_gen e1 with 
            | None => None
            | Some (e1, ty2, u_gen) => 
              let ze := ZExp.Deeper NotInHole (ZExp.LamZA p zann e1) in 
              Some (ze, HTyp.Arrow ty1 ty2, u_gen)
            end
          end
        end
      | (_, ZExp.Deeper _ (ZExp.LamZE p ann ze1)) => 
        match HTyp.matched_arrow ty with 
        | None => None 
        | Some (_, ty2) => 
          let ty1 := 
            match ann with 
            | Some uty1 => UHTyp.expand fuel uty1
            | None => HTyp.Hole
            end in 
          match UHExp.ana_pat fuel ctx p ty1 with 
          | None => None
          | Some ctx => 
            match perform_syn fuel ctx a (ze1, ty2, u_gen) with 
            | None => None
            | Some (ze1, ty2, u_gen) => 
              let ze := ZExp.Deeper NotInHole (ZExp.LamZE p ann ze1) in 
              Some (ze, HTyp.Arrow ty1 ty2, u_gen)
            end
          end
        end
      | (_, ZExp.Deeper _ (ZExp.InjZ side ze1)) => 
        match ty with 
        | HTyp.Sum ty1 ty2 => 
          let ty_side := pick_side side ty1 ty2 in 
          match perform_syn fuel ctx a (ze1, ty_side, u_gen) with 
          | None => None
          | Some (ze1', ty_side', u_gen') => 
            let ty' := 
              match side with 
              | L => HTyp.Sum ty_side' ty2
              | R => HTyp.Sum ty1 ty_side'
              end in 
            Some (
              ZExp.Deeper NotInHole (ZExp.InjZ side ze1'),
              ty',
              u_gen')
          end
        | _ => None (* should never happen *)
        end
      | (_, ZExp.Deeper _ (ZExp.OpSeqZ _ ze0 surround)) => 
        let i := OperatorSeq.surround_prefix_length surround in 
        match ZExp.erase ze with 
        | UHExp.Tm _ (UHExp.OpSeq skel seq) => 
          match UHExp.syn_skel fuel ctx skel seq (Some i) with 
          | Some (ty, Some mode) =>
              match mode with 
              | UHExp.AnalyzedAgainst ty0 => 
                match perform_ana fuel u_gen ctx a ze0 ty0 with 
                | None => None
                | Some (ze0', u_gen) => 
                  let ze0'' := ZExp.bidelimit ze0' in  
                  Some (
                    ZExp.Deeper NotInHole (ZExp.OpSeqZ skel ze0'' surround), 
                    ty, u_gen)
                end
              | UHExp.Synthesized ty0 =>
                match perform_syn fuel ctx a (ze0, ty0, u_gen) with 
                | None => None
                | Some (ze0', ty0', u_gen) => 
                  let ze0'' := ZExp.bidelimit ze0' in 
                  make_and_syn_OpSeqZ fuel ctx u_gen ze0'' surround 
                end
              end
          | Some _ => None (* should never happen *)
          | None => None (* should never happen *)
          end
        | _ => None (* should never happen *)
        end
      | (_, ZExp.Deeper _ (ZExp.ApPaletteZ name serialized_model z_hole_data)) => 
        let (next_lbl, z_nat_map) := z_hole_data in 
        let (rest_map, z_data) := z_nat_map in 
        let (cell_lbl, cell_data) := z_data in
        let (cell_ty, cell_ze) := cell_data in 
        match perform_ana fuel u_gen ctx a cell_ze cell_ty with 
        | None => None
        | Some(cell_ze', u_gen') => 
            let z_hole_data' := (next_lbl, (rest_map, (cell_lbl, (cell_ty, cell_ze')))) in 
            Some(
              ZExp.Deeper NotInHole (ZExp.ApPaletteZ name serialized_model z_hole_data'),
              ty, 
              u_gen')
        end
      | (_, ZExp.Deeper _ (ZExp.CaseZE _ _)) => None
      | (_, ZExp.Deeper _ (ZExp.CaseZR _ _)) => None
      (* Invalid actions at expression level *)
      | (Construct SNum, _)
      | (Construct SBool, _) 
      | (Construct SWild, _) => None
      end
      end
      end
    with perform_ana 
      (fuel: Fuel.t) 
      (u_gen : MetaVarGen.t) 
      (ctx: Contexts.t) 
      (a: t) 
      (ze: ZExp.t) 
      (ty: HTyp.t)
      : option (ZExp.t * MetaVarGen.t) :=
      match fuel with
      | Fuel.Kicked => None
      | Fuel.More fuel =>
      match (a, ze) with
      | (_, ZExp.Deeper (InHole u) ze1') => 
        let ze' := ZExp.Deeper NotInHole ze1' in 
        let e' := ZExp.erase ze' in 
        match UHExp.syn fuel ctx e' with
        | Some ty1 => 
          match perform_syn fuel ctx a (ze', ty1, u_gen) with 
          | Some (ze', ty1', u_gen') => 
            if HTyp.consistent ty1' ty then 
              Some (ze', u_gen')
            else 
              Some (ZExp.put_in_hole u ze', u_gen')
          | None => None
          end
        | None => None
        end
      (* Movement *)
      | (MoveTo path, _) => 
        let e := ZExp.erase ze in
        match Path.follow_e fuel path e with
        | Some ze' => Some (ze', u_gen)
        | None => None
        end
      | (MoveToPrevHole, _) =>
        match Path.prev_hole_path fuel ze with
        | None => None
        | Some path => perform_ana fuel u_gen ctx (MoveTo path) ze ty
        end
      | (MoveToNextHole, _) =>
        match Path.next_hole_path fuel ze with
        | None => None
        | Some path =>
          (* [debug] let path := Helper.log_path path in *)
          perform_ana fuel u_gen ctx (MoveTo path) ze ty
        end
      (* Backspace & Delete *)
      | (Backspace, ZExp.CursorE After e) => 
        match e with 
        | UHExp.Tm _ (UHExp.EmptyHole _) => 
          Some (ZExp.CursorE Before e, u_gen)
        | _ => 
          let (e', u_gen) := UHExp.new_EmptyHole u_gen in 
          Some (ZExp.CursorE Before e', u_gen)
        end
      | (Backspace, ZExp.CursorE Before e) => None
      | (Delete, ZExp.CursorE Before e) => 
        match e with 
        | UHExp.Tm _ (UHExp.EmptyHole _) => 
          Some (ZExp.CursorE After e, u_gen)
        | _ => 
          let (e', u_gen) := UHExp.new_EmptyHole u_gen in 
          Some (ZExp.CursorE Before e', u_gen)
        end
      | (Delete, ZExp.CursorE After e) => None
      | (Backspace, ZExp.CursorE (In _) e)
      | (Delete, ZExp.CursorE (In _) e) => 
        let (e', u_gen) := UHExp.new_EmptyHole u_gen in 
        let ze' := ZExp.CursorE Before e' in 
        Some (ze', u_gen)
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
      | (Delete,
          ZExp.Deeper _ (ZExp.AscZ1
            (ZExp.CursorE After e1)
            _)) => 
        match UHExp.ana_fix_holes fuel ctx u_gen e1 ty with 
        | Some (e1', u_gen) => 
          let ze' := ZExp.CursorE After e1' in 
          Some (ze', u_gen)
        | None => None
        end
      | (Backspace, ZExp.Deeper _ 
          (ZExp.LetZA p (ZTyp.CursorT Before _) e1 e2))
      | (Backspace, ZExp.Deeper _
          (ZExp.LetZA p  
            (ZTyp.OpSeqZ _
              (ZTyp.CursorT Before _)
              (OperatorSeq.EmptyPrefix _)) e1 e2)) =>  
        match UHExp.syn_fix_holes fuel ctx u_gen e1 with 
        | None => None
        | Some (e1, ty1, u_gen) => 
          match UHExp.ana_pat_fix_holes fuel ctx u_gen false p ty1 with 
          | None => None
          | Some (p, ctx, u_gen) => 
            match UHExp.ana_fix_holes fuel ctx u_gen e2 ty with 
            | None => None
            | Some (e2, u_gen) => 
              let ze := 
                ZExp.Deeper NotInHole
                  (ZExp.LetZP (ZPat.place_After p) None e1 e2) in 
              Some (ze, u_gen)
            end
          end
        end
      | (Delete, ZExp.Deeper _
          (ZExp.LetZP ((ZPat.CursorP After _) as zp) (Some _) e1 e2))
      | (Delete, ZExp.Deeper _
          (ZExp.LetZP 
            ((ZPat.Deeper _ (ZPat.OpSeqZ _ 
              (ZPat.CursorP After _)
              (OperatorSeq.EmptySuffix _))) as zp)
            (Some _)
            e1 e2)) => 
        match UHExp.syn_fix_holes fuel ctx u_gen e1 with 
        | None => None
        | Some (e1, ty1, u_gen) => 
          match ana_zpat_fix_holes fuel ctx u_gen zp ty1 with 
          | None => None
          | Some (zp, ctx, u_gen) => 
            match UHExp.ana_fix_holes fuel ctx u_gen e2 ty with 
            | None => None
            | Some (e2, u_gen) => 
              let ze := ZExp.Deeper NotInHole (ZExp.LetZP zp None e1 e2) in 
              Some (ze, u_gen)
            end
          end
        end
      | (Backspace, ZExp.Deeper _ 
          (ZExp.LamZA p (ZTyp.CursorT Before _) e1))
      | (Backspace, ZExp.Deeper _
          (ZExp.LamZA p 
            (ZTyp.OpSeqZ _
              (ZTyp.CursorT Before _)
              (OperatorSeq.EmptyPrefix _)) e1)) =>  
        match HTyp.matched_arrow ty with
        | None => None
        | Some (ty1, ty2) => 
          match UHExp.ana_pat_fix_holes fuel ctx u_gen false p ty1 with 
          | None => None
          | Some (p, ctx, u_gen) => 
            match UHExp.ana_fix_holes fuel ctx u_gen e1 ty2 with 
            | None => None
            | Some (e1, u_gen) => 
              let zp := ZPat.place_After p in 
              let ze := ZExp.Deeper NotInHole (ZExp.LamZP zp None e1) in 
              Some (ze, u_gen)
            end
          end
        end
      | (Delete, ZExp.Deeper _
          (ZExp.LamZP ((ZPat.CursorP After _) as zp) (Some _) e1)) 
      | (Delete, ZExp.Deeper _
          (ZExp.LamZP 
            ((ZPat.Deeper _
              (ZPat.OpSeqZ _ 
                (ZPat.CursorP After _)
                (OperatorSeq.EmptySuffix _))) as zp) 
            (Some _) 
            e1)) => 
        match HTyp.matched_arrow ty with
        | None => None
        | Some (ty1, ty2) => 
          match ana_zpat_fix_holes fuel ctx u_gen zp ty1 with 
          | None => None
          | Some (zp, ctx, u_gen) => 
            match UHExp.ana_fix_holes fuel ctx u_gen e1 ty2 with 
            | None => None
            | Some (e1, u_gen) => 
              let ze := ZExp.Deeper NotInHole (ZExp.LamZP zp None e1) in 
              Some (ze, u_gen)
            end
          end
        end
      | (Backspace, 
          ZExp.Deeper _ 
            (ZExp.CaseZR e1 
              (prefix, 
                ZExp.RuleZE p 
                  ((ZExp.CursorE After _) as rze),
              suffix))) => 
        match suffix with 
        | nil => 
          match List.rev prefix with 
          | nil => 
            let (zrule, u_gen) := ZExp.empty_zrule u_gen in  
            let zrules := ZList.singleton zrule in 
            let ze := 
              ZExp.Deeper NotInHole
                (ZExp.CaseZR e1 zrules) in 
            Some (ze, u_gen)
          | cons (UHExp.Rule p e) tl => 
            let zrule := ZExp.RuleZP (ZPat.place_Before p) e in 
            let rules := List.rev tl in 
            let zrules := (rules, zrule, suffix) in 
            let ze := 
              ZExp.Deeper NotInHole
                (ZExp.CaseZR e1 zrules) in 
            Some (ze, u_gen)
          end
        | cons (UHExp.Rule p e) suffix => 
          let zrule := ZExp.RuleZP (ZPat.place_Before p) e in 
          let zrules := (prefix, zrule, suffix) in 
          let ze := 
            ZExp.Deeper NotInHole
              (ZExp.CaseZR e1 zrules) in 
          Some (ze, u_gen)
        end
      | (Backspace, ZExp.Deeper _
          (ZExp.OpSeqZ _
            ((ZExp.CursorE Before e0) as ze0) 
            ((OperatorSeq.EmptySuffix _) as surround)))
      | (Backspace, ZExp.Deeper _
          (ZExp.OpSeqZ _
            ((ZExp.CursorE Before e0) as ze0) 
            ((OperatorSeq.BothNonEmpty _ _) as surround))) =>
        abs_perform_Backspace_Before_op 
          combine_for_Backspace_Space
          (fun fuel ctx u_gen ze => 
            zexp_ana_fix_holes fuel ctx u_gen ze ty)
          (fun fuel ctx u_gen ze surround => 
            make_and_ana_OpSeqZ fuel ctx u_gen ze surround ty) 
          UHExp.is_EmptyHole
          UHExp.is_Space
          UHExp.Space
          ZExp.CursorE
          fuel ctx u_gen e0 ze0 surround
      | (Delete, ZExp.Deeper _
          (ZExp.OpSeqZ _
            ((ZExp.CursorE After e0) as ze0)
            ((OperatorSeq.EmptyPrefix _) as surround)))
      | (Delete, ZExp.Deeper _ 
          (ZExp.OpSeqZ _
            ((ZExp.CursorE After e0) as ze0)
            ((OperatorSeq.BothNonEmpty _ _) as surround))) => 
        abs_perform_Delete_After_op
          combine_for_Delete_Space
          (fun fuel ctx u_gen ze => 
            zexp_ana_fix_holes fuel ctx u_gen ze ty)
          (fun fuel ctx u_gen ze surround => 
            make_and_ana_OpSeqZ fuel ctx u_gen ze surround ty) 
          UHExp.is_EmptyHole
          UHExp.is_Space
          UHExp.Space
          ZExp.CursorE
          fuel ctx u_gen e0 ze0 surround
      (* Construction *)
      | (Construct SParenthesized, ZExp.CursorE _ e) => 
        Some (
          ZExp.ParenthesizedZ ze, 
          u_gen)
      | (Construct SAsc, ZExp.CursorE _ e) =>
        let e' := UHExp.bidelimit e in 
        let uty := UHTyp.contract ty in 
        Some (
          ZExp.Deeper NotInHole 
            (ZExp.AscZ2 e' (ZTyp.place_Before uty)), 
          u_gen)
      | (Construct SAsc, ZExp.Deeper err_status (ZExp.LetZP zp None e1 e2)) => 
        match UHExp.syn fuel ctx e1 with 
        | None => None
        | Some ty1 => 
          let uty1 := UHTyp.contract ty1 in 
          let ze := ZExp.Deeper err_status 
            (ZExp.LetZA (ZPat.erase zp) (ZTyp.place_Before uty1) e1 e2) in 
          Some (ze, u_gen)
        end
      | (Construct SAsc, ZExp.Deeper err_status (ZExp.LamZP zp None e1)) => 
        let ze := ZExp.Deeper err_status 
          (ZExp.LamZA (ZPat.erase zp) (ZTyp.CursorT Before UHTyp.Hole) e1) in 
        Some (ze, u_gen)
      | (Construct SAsc, ZExp.Deeper err_status (ZExp.LetZP zp (Some uty1) e1 e2)) => 
        (* just move the cursor over if there is already an ascription *)
        let ze := ZExp.Deeper err_status
          (ZExp.LetZA (ZPat.erase zp) (ZTyp.place_Before uty1) e1 e2) in 
        Some (ze, u_gen)
      | (Construct SAsc, ZExp.Deeper err_status (ZExp.LamZP zp (Some uty1) e1)) => 
        (* just move the cursor over if there is already an ascription *)
        let ze := ZExp.Deeper err_status
          (ZExp.LamZA (ZPat.erase zp) (ZTyp.place_Before uty1) e1) in 
        Some (ze, u_gen)
      | (Construct SLet, ZExp.CursorE _ e1) => 
        match UHExp.syn_fix_holes fuel ctx u_gen e1 with 
        | Some (e1, ty1, u_gen) => 
          let (zp, u_gen) := ZPat.new_EmptyHole u_gen in 
          let (e2, u_gen) := UHExp.new_EmptyHole u_gen in 
          let ze := ZExp.Deeper NotInHole 
            (ZExp.LetZP zp None e1 e2) in 
          Some (ze, u_gen)
        | None => 
          let (zp, u_gen) := ZPat.new_EmptyHole u_gen in 
          let (e2, u_gen) := UHExp.new_EmptyHole u_gen in 
          let ann := Some (UHTyp.contract ty) in 
          let ze := ZExp.Deeper NotInHole 
            (ZExp.LetZP zp ann e1 e2) in 
          Some (ze, u_gen)
        end
      | (Construct SLam, ZExp.CursorE _ e) => 
        match HTyp.matched_arrow ty with 
        | Some (_, ty2) => 
          match UHExp.ana_fix_holes fuel ctx u_gen e ty2 with 
          | None => None
          | Some (e, u_gen) => 
            let (zp, u_gen) := ZPat.new_EmptyHole u_gen in 
            let ze := ZExp.Deeper NotInHole (ZExp.LamZP zp None e) in 
            Some (ze, u_gen)
          end
        | None => 
          match UHExp.syn_fix_holes fuel ctx u_gen e with 
          | None => None
          | Some (e, _, u_gen) => 
            let (zp, u_gen) := ZPat.new_EmptyHole u_gen in 
            let (u, u_gen) := MetaVarGen.next u_gen in 
            let ze := ZExp.Deeper (InHole u) (ZExp.LamZP zp None e) in 
            Some (ze, u_gen)
          end
        end
      | (Construct (SInj side), ZExp.CursorE cursor_side e1) => 
        match HTyp.matched_sum ty with 
        | Some (tyL, tyR) => 
           let ty1 := pick_side side tyL tyR in 
           match UHExp.ana_fix_holes fuel ctx u_gen e1 ty1 with 
           | None => None
           | Some (e1, u_gen) => 
             let ze := ZExp.Deeper NotInHole 
               (ZExp.InjZ side (ZExp.CursorE cursor_side e1)) in 
             Some (ze, u_gen) 
           end
        | None => 
          match UHExp.syn_fix_holes fuel ctx u_gen e1 with 
          | None => None
          | Some (e1, _, u_gen) => 
            let (u, u_gen) := MetaVarGen.next u_gen in 
            let ze := ZExp.Deeper (InHole u) 
              (ZExp.InjZ side 
                (ZExp.CursorE cursor_side e1)) in 
            Some (ze, u_gen)
          end
        end
      | (Construct SCase, ZExp.CursorE _ e1) => 
        match e1 with 
        | UHExp.Tm _ (UHExp.EmptyHole _) => 
          let (rule, u_gen) := UHExp.empty_rule u_gen in 
          let rules := cons rule nil in 
          let ze := ZExp.Deeper NotInHole (ZExp.CaseZE ze rules) in 
          Some (ze, u_gen) 
        | _ => 
          let (zrule, u_gen) := ZExp.empty_zrule u_gen in 
          let zrules := ZList.singleton zrule in 
          match UHExp.syn_fix_holes fuel ctx u_gen e1 with 
          | None => None
          | Some (e1, _, u_gen) =>  
            let ze := ZExp.Deeper NotInHole (ZExp.CaseZR e1 zrules) in 
            Some (ze, u_gen) 
          end
        end  
      | (Construct SRule, 
          ZExp.Deeper _ 
            (ZExp.CaseZR e1 
              (prefix, 
                ZExp.RuleZP 
                  (ZPat.CursorP Before p) re,
              suffix))) => 
        let (zrule, u_gen) := ZExp.empty_zrule u_gen in 
        let prev_rule := UHExp.Rule p re in 
        let suffix := cons prev_rule suffix in 
        let ze := 
          ZExp.Deeper NotInHole 
            (ZExp.CaseZR e1 
              (prefix, 
               zrule, 
               suffix)) in 
        Some (ze, u_gen)
      | (Construct SRule, 
          ZExp.Deeper _ 
            (ZExp.CaseZR e1 
              (prefix, 
                ZExp.RuleZE p 
                  ((ZExp.CursorE After _) as rze),
              suffix)))
      | (Construct SRule, 
          ZExp.Deeper _ 
            (ZExp.CaseZR e1 
              (prefix, 
                ZExp.RuleZE p 
                  ((ZExp.Deeper _ (ZExp.OpSeqZ _ 
                    (ZExp.CursorE After _)
                    (OperatorSeq.EmptySuffix _))) as rze),
              suffix))) => 
        let (zp, u_gen) := ZPat.new_EmptyHole u_gen in 
        let (rule_e, u_gen) := UHExp.new_EmptyHole u_gen in 
        let zrule := ZExp.RuleZP zp rule_e in  
        let re := ZExp.erase rze in 
        let prev_rule := UHExp.Rule p re in 
        let prefix := prefix ++ (cons prev_rule nil) in 
        let ze := 
          ZExp.Deeper NotInHole
            (ZExp.CaseZR e1 
              (prefix, 
               zrule,
               suffix)) in 
        Some (ze, u_gen)
      | (Construct SRule, _) => None
      | (Construct (SOp os), ZExp.Deeper _ (
          ZExp.OpSeqZ _ (ZExp.CursorE (In _) e) surround))
      | (Construct (SOp os), ZExp.Deeper _ (
          ZExp.OpSeqZ _ (ZExp.CursorE After e) surround)) => 
        match exp_op_of os with
        | None => None
        | Some op => 
          abs_perform_Construct_SOp_After_surround 
            ZExp.new_EmptyHole
            (fun fuel ctx u_gen ze surround => 
              make_and_ana_OpSeqZ fuel ctx u_gen ze surround ty)
            UHExp.is_Space
            UHExp.Space
            ZExp.CursorE
            fuel ctx u_gen e op surround
        end
      | (Construct (SOp os), 
          ZExp.Deeper _ (ZExp.OpSeqZ _
            ((ZExp.CursorE Before _) as ze0) surround)) =>
        match exp_op_of os with 
        | None => None
        | Some op => 
          abs_perform_Construct_SOp_Before_surround
            ZExp.erase
            ZExp.new_EmptyHole
            (fun fuel ctx u_gen ze surround => 
              make_and_ana_OpSeqZ fuel ctx u_gen ze surround ty)
            UHExp.is_Space
            UHExp.Space
            ZExp.CursorE
            fuel ctx u_gen ze0 op surround
        end
      | (Construct (SOp os), ZExp.CursorE (In _) e)
      | (Construct (SOp os), ZExp.CursorE After e) => 
        match exp_op_of os with 
        | None => None
        | Some op => 
          abs_perform_Construct_SOp_After 
            UHExp.bidelimit
            ZExp.new_EmptyHole
            (fun fuel ctx u_gen ze surround => 
              make_and_ana_OpSeqZ fuel ctx u_gen ze surround ty)
            fuel ctx u_gen e op
        end
      | (Construct (SOp os), ZExp.CursorE Before e) => 
        match exp_op_of os with 
        | None => None
        | Some op => 
          abs_perform_Construct_SOp_Before
            UHExp.bidelimit
            ZExp.new_EmptyHole
            (fun fuel ctx u_gen ze surround => 
              make_and_ana_OpSeqZ fuel ctx u_gen ze surround ty)
            fuel ctx u_gen e op
        end
      (* Zipper Cases *)
      | (_, ZExp.ParenthesizedZ ze1) => 
        match perform_ana fuel u_gen ctx a ze1 ty with 
        | Some (ze1', u_gen') => 
          Some (
            ZExp.ParenthesizedZ ze1',
            u_gen')
        | None => None
        end
      | (_, ZExp.Deeper _ (ZExp.LetZP zp ann e1 e2)) => 
        match ann with 
        | Some uty1 => 
          let ty1 := UHTyp.expand fuel uty1 in 
          match perform_ana_pat fuel ctx u_gen a zp ty1 with 
          | None => None
          | Some (zp, ctx2, u_gen) => 
            let p := ZPat.erase zp in 
            let ctx1 := UHExp.ctx_for_let ctx p ty1 e1 in 
            match UHExp.ana_fix_holes fuel ctx1 u_gen e1 ty1 with 
            | None => None
            | Some (e1, u_gen) => 
              match UHExp.ana_fix_holes fuel ctx2 u_gen e2 ty with 
              | None => None
              | Some (e2, u_gen) => 
                let ze := ZExp.Deeper NotInHole (ZExp.LetZP zp ann e1 e2) in 
                Some (ze, u_gen)
              end
            end
          end
        | None => 
          match UHExp.syn fuel ctx e1 with
          | None => None
          | Some ty1 => 
            match perform_ana_pat fuel ctx u_gen a zp ty1 with 
            | None => None
            | Some (zp, ctx2, u_gen) => 
              match UHExp.ana_fix_holes fuel ctx2 u_gen e2 ty with 
              | None => None
              | Some (e2, u_gen) => 
                let ze := ZExp.Deeper NotInHole (ZExp.LetZP zp ann e1 e2) in 
                Some (ze, u_gen)
              end
            end
          end
        end 
      | (_, ZExp.Deeper _ (ZExp.LetZA p zann e1 e2)) => 
        (* (ctx) let p (ctx2) : ty = (ctx1) e1 in (ctx2) e2 *) 
        match perform_ty fuel a zann with 
        | None => None
        | Some zann => 
          let ty1 := UHTyp.expand fuel (ZTyp.erase zann) in 
          match UHExp.ana_pat_fix_holes fuel ctx u_gen false p ty1 with 
          | None => None
          | Some (p, ctx2, u_gen) => 
            let ctx1 := UHExp.ctx_for_let ctx p ty1 e1 in 
            match UHExp.ana_fix_holes fuel ctx1 u_gen e1 ty1 with 
            | None => None
            | Some (e1, u_gen) => 
              match UHExp.ana_fix_holes fuel ctx2 u_gen e2 ty with 
              | None => None
              | Some (e2, u_gen) => 
                let ze := ZExp.Deeper NotInHole (ZExp.LetZA p zann e1 e2) in 
                Some (ze, u_gen)
              end
            end
          end
        end
      | (_, ZExp.Deeper _ (ZExp.LetZE1 p ann ze1 e2)) =>
        match ann with 
        | Some ann_ty => 
          let ty1 := UHTyp.expand fuel ann_ty in 
          let ctx1 := UHExp.ctx_for_let ctx p ty1 (ZExp.erase ze1) in  
          match perform_ana fuel u_gen ctx1 a ze1 ty1 with 
          | None => None
          | Some (ze1, u_gen) => 
            let ze := ZExp.Deeper NotInHole (ZExp.LetZE1 p ann ze1 e2) in 
            Some (ze, u_gen)
          end
        | None => 
          let e1 := ZExp.erase ze1 in
          match UHExp.syn fuel ctx e1 with 
          | None => None
          | Some ty1 => 
            match perform_syn fuel ctx a (ze1, ty1, u_gen) with
            | None => None
            | Some (ze1, ty1, u_gen) => 
              match UHExp.ana_pat_fix_holes fuel ctx u_gen false p ty1 with 
              | None => None
              | Some (p, ctx2, u_gen) => 
                match UHExp.ana_fix_holes fuel ctx2 u_gen e2 ty with 
                | None => None
                | Some (e2, u_gen) => 
                  let ze := ZExp.Deeper NotInHole (ZExp.LetZE1 p ann ze1 e2) in 
                  Some (ze, u_gen)
                end
              end
            end
          end
        end
      | (_, ZExp.Deeper _ (ZExp.LetZE2 p ann e1 ze2)) =>
        let ty1 := 
          match ann with 
          | Some uty1 => Some (UHTyp.expand fuel uty1) 
          | None => UHExp.syn fuel ctx e1
          end in 
        match ty1 with 
        | None => None
        | Some ty1 => 
          match UHExp.ana_pat fuel ctx p ty1 with 
          | None => None
          | Some ctx2 => 
            match perform_ana fuel u_gen ctx2 a ze2 ty with 
            | None => None
            | Some (ze2, u_gen) => 
              let ze := ZExp.Deeper NotInHole (ZExp.LetZE2 p ann e1 ze2) in 
              Some (ze, u_gen)
            end
          end
        end
      | (_, ZExp.Deeper _ (ZExp.LamZP zp ann e1)) => 
        match HTyp.matched_arrow ty with 
        | None => None
        | Some (ty1_given, ty2) => 
          let ty1 := 
            match ann with 
            | Some uty1 => UHTyp.expand fuel uty1
            | None => ty1_given
            end in 
          match perform_ana_pat fuel ctx u_gen a zp ty1 with 
          | None => None
          | Some (zp, ctx, u_gen) => 
            match UHExp.ana_fix_holes fuel ctx u_gen e1 ty2 with 
            | None => None
            | Some (e1, u_gen) => 
              let ze := ZExp.Deeper NotInHole (ZExp.LamZP zp ann e1) in 
              Some (ze, u_gen)
            end
          end
        end
      | (_, ZExp.Deeper _ (ZExp.LamZA p zann e1)) => 
        match HTyp.matched_arrow ty with 
        | None => None
        | Some (ty1_given, ty2) => 
          match perform_ty fuel a zann with 
          | None => None
          | Some zann => 
            let ty1 := UHTyp.expand fuel (ZTyp.erase zann) in 
            match HTyp.consistent ty1 ty1_given with 
            | true => 
              match UHExp.ana_pat_fix_holes fuel ctx u_gen false p ty1 with 
              | None => None
              | Some (p, ctx, u_gen) => 
                match UHExp.ana_fix_holes fuel ctx u_gen e1 ty2 with 
                | None => None
                | Some (e1, u_gen) => 
                  let ze := ZExp.Deeper NotInHole (ZExp.LamZA p zann e1) in 
                  Some (ze, u_gen)
                end
              end
            | false => 
              match UHExp.ana_pat_fix_holes fuel ctx u_gen false p ty1 with 
              | None => None
              | Some (p, ctx, u_gen) => 
                match UHExp.syn_fix_holes fuel ctx u_gen e1 with 
                | None => None
                | Some (e1, _, u_gen) => 
                  let (u, u_gen) := MetaVarGen.next u_gen in 
                  let ze := ZExp.Deeper (InHole u) (ZExp.LamZA p zann e1) in 
                  Some (ze, u_gen)
                end
              end
            end
          end
        end
      | (_, ZExp.Deeper _ (ZExp.LamZE p ann ze1)) => 
        match HTyp.matched_arrow ty with 
        | None => None 
        | Some (_, ty2) => 
          let ty1 := 
            match ann with 
            | Some uty1 => UHTyp.expand fuel uty1
            | None => HTyp.Hole
            end in 
          match UHExp.ana_pat fuel ctx p ty1 with 
          | None => None
          | Some ctx => 
            match perform_ana fuel u_gen ctx a ze1 ty2 with 
            | None => None
            | Some (ze1, u_gen) => 
              let ze := ZExp.Deeper NotInHole (ZExp.LamZE p ann ze1) in 
              Some (ze, u_gen)
            end
          end
        end
      | (_, ZExp.Deeper _ (ZExp.InjZ side ze)) =>
        match HTyp.matched_sum ty with 
        | Some (ty1, ty2) => 
          let picked := pick_side side ty1 ty2 in 
          match perform_ana fuel u_gen ctx a ze picked with 
          | Some (ze', u_gen') => Some (
              ZExp.Deeper (NotInHole) (
                ZExp.InjZ side ze'), u_gen')
          | None => None
          end
        | None => None
        end
      | (_, ZExp.Deeper _ (ZExp.CaseZE ze1 rules)) =>
        match UHExp.syn fuel ctx (ZExp.erase ze1) with 
        | None => None
        | Some ty1 => 
          match perform_syn fuel ctx a (ze1, ty1, u_gen) with 
          | None => None
          | Some (ze1, ty1, u_gen) => 
            match UHExp.ana_rules_fix_holes fuel ctx u_gen false rules ty1 ty with 
            | None => None
            | Some (rules, u_gen) => 
              let ze := ZExp.Deeper NotInHole (ZExp.CaseZE ze1 rules) in 
              Some (ze, u_gen)
            end
          end
        end
      | (_, ZExp.Deeper _ (ZExp.CaseZR e1 zrules)) => 
        match UHExp.syn fuel ctx e1 with 
        | None => None
        | Some ty1 => 
          match ZList.prj_z zrules with 
          | ZExp.RuleZP zp e => 
            match perform_ana_pat fuel ctx u_gen a zp ty1 with 
            | None => None
            | Some(zp, ctx, u_gen) => 
              match UHExp.ana_fix_holes fuel ctx u_gen e ty with 
              | None => None
              | Some (e, u_gen) => 
                let zrule := ZExp.RuleZP zp e in 
                let ze := ZExp.Deeper NotInHole 
                  (ZExp.CaseZR e1 (ZList.replace_z zrules zrule)) in 
                Some (ze, u_gen)
              end
            end
          | ZExp.RuleZE p ze =>
            match UHExp.ana_pat fuel ctx p ty1 with 
            | None => None
            | Some ctx => 
              match perform_ana fuel u_gen ctx a ze ty with 
              | None => None
              | Some (ze, u_gen) => 
                let zrule := ZExp.RuleZE p ze in 
                let ze := ZExp.Deeper NotInHole 
                  (ZExp.CaseZR e1 (ZList.replace_z zrules zrule)) in 
                Some (ze, u_gen)
              end
            end
          end
        end
      | (_, ZExp.Deeper _ (ZExp.OpSeqZ _ ze0 surround)) => 
        let i := OperatorSeq.surround_prefix_length surround in 
        match ZExp.erase ze with 
        | UHExp.Tm _ (UHExp.OpSeq skel seq) => 
          match UHExp.ana_skel fuel ctx skel seq ty (Some i) with 
          | Some (Some mode) =>
              match mode with 
              | UHExp.AnalyzedAgainst ty0 => 
                match perform_ana fuel u_gen ctx a ze0 ty0 with 
                | None => None
                | Some (ze0', u_gen) => 
                  let ze0'' := ZExp.bidelimit ze0' in  
                  Some (
                    ZExp.Deeper NotInHole (ZExp.OpSeqZ skel ze0'' surround), 
                    u_gen)
                end
              | UHExp.Synthesized ty0 =>
                match perform_syn fuel ctx a (ze0, ty0, u_gen) with 
                | None => None
                | Some (ze0', ty0', u_gen) => 
                  let ze0'' := ZExp.bidelimit ze0' in 
                  make_and_ana_OpSeqZ fuel ctx u_gen ze0'' surround ty
                end
              end
          | Some _ => None (* should never happen *)
          | None => None (* should never happen *)
          end
        | _ => None (* should never happen *)
        end
      (* Subsumption *)
      | (UpdateApPalette _, _)
      | (Construct (SApPalette _), _) 
      | (Construct (SVar _ _), _)
      | (Construct (SNumLit _ _), _)
      | (Construct (SBoolLit _ _), _)
      | (_, ZExp.Deeper _ (ZExp.AscZ1 _ _))
      | (_, ZExp.Deeper _ (ZExp.AscZ2 _ _))
      | (_, ZExp.Deeper _ (ZExp.ApPaletteZ _ _ _)) => 
        perform_ana_subsume fuel u_gen ctx a ze ty
      (* Invalid actions at expression level *)
      | (Construct SNum, _)
      | (Construct SBool, _)
      | (Construct SWild, _) => None
      end
      end
    with perform_ana_subsume 
      (fuel : Fuel.t) 
      (u_gen : MetaVarGen.t) 
      (ctx : Contexts.t) 
      (a : t) 
      (ze : ZExp.t) 
      (ty : HTyp.t)
      : option (ZExp.t * MetaVarGen.t) :=
      match fuel with
      | Fuel.Kicked => None
      | Fuel.More fuel =>
        match UHExp.syn fuel ctx (ZExp.erase ze) with 
        | Some ty1 => 
          match perform_syn fuel ctx a (ze, ty1, u_gen) with 
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

  Module FDynamics (Associator : ASSOCIATOR).
      Module Delta.
        Definition t : Type := MetaVarMap.t (HTyp.t * VarCtx.t).
      End Delta.

      (* hole instance numbers are all 0 after expansion and during evaluation --
       * renumbering is done on the final result (see below) *)
      Definition inst_num : Type := nat.

      Module DHPat.
        Inductive t : Type :=
        | EmptyHole : MetaVar.t -> inst_num -> t
        | NonEmptyHole : MetaVar.t -> inst_num -> t -> t
        | Wild : t
        | Var : Var.t -> t
        | NumLit : nat -> t
        | BoolLit : bool -> t
        | Inj : inj_side -> t -> t
        | Pair : t -> t -> t
        | Ap : t -> t -> t.

        (* whether dp contains the variable x outside of a hole *)
        Fixpoint binds_var (x : Var.t) (dp : t) : bool :=
          match dp with
          | EmptyHole _ _
          | NonEmptyHole _ _ _
          | Wild
          | NumLit _
          | BoolLit _ => false
          | Var y => Var.eq x y
          | Inj _ dp1 => binds_var x dp1
          | Pair dp1 dp2
          | Ap dp1 dp2 => binds_var x dp1 || binds_var x dp2
          end.

        Inductive expand_result : Type :=
        | Expands : t -> HTyp.t -> Contexts.t -> Delta.t -> expand_result
        | DoesNotExpand : expand_result.

        Fixpoint syn_expand
          (fuel : Fuel.t)
          (ctx : Contexts.t)
          (p : UHPat.t)
          : expand_result :=
          match fuel with
          | Fuel.Kicked => DoesNotExpand
          | Fuel.More fuel => let syn_expand := syn_expand fuel in
          match p with
          | UHPat.Parenthesized p1 => syn_expand ctx p1
          | UHPat.Pat NotInHole p' => syn_expand' fuel ctx p'
          | UHPat.Pat (InHole u) p' =>
            match syn_expand' fuel ctx p' with
            | DoesNotExpand => DoesNotExpand
            | Expands dp _ ctx delta =>
              let (gamma, _) := ctx in
              let delta := MetaVarMap.extend delta (u, (HTyp.Hole, gamma)) in
              Expands
                (NonEmptyHole u 0 dp)
                HTyp.Hole
                ctx
                delta
            end
          end
          end
        with syn_expand'
          (fuel : Fuel.t)
          (ctx : Contexts.t)
          (p' : UHPat.t')
          : expand_result :=
          match fuel with
          | Fuel.Kicked => DoesNotExpand
          | Fuel.More fuel => let syn_expand' := syn_expand' fuel in
          match p' with
          | UHPat.EmptyHole u =>
            let gamma := Contexts.gamma ctx in
            let dp := EmptyHole u 0 in
            let ty := HTyp.Hole in
            let delta := MetaVarMap.extend MetaVarMap.empty
                         (u, (ty, gamma)) in
            Expands dp ty ctx delta
          | UHPat.Wild => Expands Wild HTyp.Hole ctx MetaVarMap.empty
          | UHPat.Var x =>
            (* TODO: add linearity check *)
            if Var.is_valid x then
              let ctx := Contexts.extend_gamma ctx (x, HTyp.Hole) in
              Expands (Var x) HTyp.Hole ctx MetaVarMap.empty
            else
              DoesNotExpand
          | UHPat.NumLit n => Expands (NumLit n) HTyp.Num ctx MetaVarMap.empty
          | UHPat.BoolLit b => Expands (BoolLit b) HTyp.Bool ctx MetaVarMap.empty
          | UHPat.Inj side p =>
            match syn_expand fuel ctx p with
            | DoesNotExpand => DoesNotExpand
            | Expands dp1 ty1 ctx delta =>
              let dp := Inj side dp1 in
              let ty :=
                match side with
                | L => HTyp.Sum ty1 HTyp.Hole
                | R => HTyp.Sum HTyp.Hole ty1
                end in
              Expands dp ty ctx delta
            end
          | UHPat.OpSeq skel seq => syn_expand_skel fuel ctx skel seq
          end
          end
        with syn_expand_skel
          (fuel : Fuel.t)
          (ctx : Contexts.t)
          (skel : UHPat.skel_t)
          (seq : UHPat.opseq)
          : expand_result :=
            match fuel with
            | Fuel.Kicked => DoesNotExpand
            | Fuel.More fuel => let syn_expand_skel := syn_expand_skel fuel in
            match skel with
            | Skel.Placeholder _ n =>
              match OperatorSeq.seq_nth n seq with
              | None => DoesNotExpand
              | Some pn =>
                if UHPat.bidelimited pn then
                  syn_expand fuel ctx pn
                else
                  DoesNotExpand
              end
            | Skel.BinOp (InHole u) op skel1 skel2 =>
              let skel_not_in_hole := Skel.BinOp NotInHole op skel1 skel2 in
              match syn_expand_skel ctx skel_not_in_hole seq with
              | DoesNotExpand => DoesNotExpand
              | Expands dp _ ctx delta =>
                let gamma := Contexts.gamma ctx in
                let delta := MetaVarMap.extend delta (u, (HTyp.Hole, gamma)) in
                Expands
                  (NonEmptyHole u 0 dp)
                  HTyp.Hole
                  ctx
                  delta
              end
            | Skel.BinOp NotInHole UHPat.Comma skel1 skel2 =>
              match syn_expand_skel ctx skel1 seq with
              | DoesNotExpand => DoesNotExpand
              | Expands dp1 ty1 ctx delta1 =>
                match syn_expand_skel ctx skel2 seq with
                | DoesNotExpand => DoesNotExpand
                | Expands dp2 ty2 ctx delta2 =>
                  let delta := MetaVarMap.union delta1 delta2 in
                  let dp := Pair dp1 dp2 in
                  Expands dp (HTyp.Prod ty1 ty2) ctx delta
                end
              end
            | Skel.BinOp NotInHole UHPat.Space skel1 skel2 =>
              (* TODO: need to review this re: casts *)
              match syn_expand_skel ctx skel1 seq with
              | DoesNotExpand => DoesNotExpand
              | Expands dp1 ty1 ctx delta1 =>
                match HTyp.matched_arrow ty1 with
                | None => DoesNotExpand
                | Some (ty2, ty) =>
                  match ana_expand_skel fuel ctx skel2 seq ty2 with
                  | DoesNotExpand => DoesNotExpand
                  | Expands dp2 ty2 ctx delta2 =>
                    let dp := Ap dp1 dp2 in
                    let delta := MetaVarMap.union delta1 delta2 in
                    Expands dp ty ctx delta
                  end
                end
              end
            end
            end
        with ana_expand_skel
          (fuel : Fuel.t)
          (ctx : Contexts.t)
          (skel : UHPat.skel_t)
          (seq : UHPat.opseq)
          (ty : HTyp.t)
          : expand_result :=
            match fuel with
            | Fuel.Kicked => DoesNotExpand
            | Fuel.More fuel => let ana_expand_skel := ana_expand_skel fuel in
            match skel with
            | Skel.Placeholder _ n =>
              match OperatorSeq.seq_nth n seq with
              | None => DoesNotExpand
              | Some pn =>
                if UHPat.bidelimited pn then
                  ana_expand fuel ctx pn ty
                else
                  DoesNotExpand
              end
            | Skel.BinOp (InHole u) op skel1 skel2 =>
              let skel_not_in_hole := Skel.BinOp NotInHole op skel1 skel2 in
              match syn_expand_skel fuel ctx skel_not_in_hole seq with
              | DoesNotExpand => DoesNotExpand
              | Expands dp1 _ ctx delta1 =>
                let dp := DHPat.NonEmptyHole u 0 dp1 in
                let gamma := Contexts.gamma ctx in
                let delta := MetaVarMap.extend delta1 (u, (ty, gamma)) in
                Expands dp ty ctx delta
              end
            | Skel.BinOp NotInHole UHPat.Comma skel1 skel2 =>
              match HTyp.matched_prod ty with
              | None => DoesNotExpand
              | Some (ty1, ty2) =>
                match ana_expand_skel ctx skel1 seq ty1 with
                | DoesNotExpand => DoesNotExpand
                | Expands dp1 ty1 ctx delta1 =>
                  match ana_expand_skel ctx skel2 seq ty2 with
                  | DoesNotExpand => DoesNotExpand
                  | Expands dp2 ty2 ctx delta2 =>
                    let dp := Pair dp1 dp2 in
                    let delta := MetaVarMap.union delta1 delta2 in
                    Expands dp ty ctx delta
                  end
                end
              end
            | Skel.BinOp NotInHole UHPat.Space skel1 skel2 => DoesNotExpand (* TODO *)
            end
            end
        with ana_expand
          (fuel : Fuel.t)
          (ctx : Contexts.t)
          (p : UHPat.t)
          (ty : HTyp.t)
          : expand_result :=
            match fuel with
            | Fuel.Kicked => DoesNotExpand
            | Fuel.More fuel => let ana_expand := ana_expand fuel in
            match p with
            | UHPat.Parenthesized p1 => ana_expand ctx p1 ty
            | UHPat.Pat NotInHole p' => ana_expand' fuel ctx p' ty
            | UHPat.Pat (InHole u) p' =>
              match syn_expand' fuel ctx p' with
              | DoesNotExpand => DoesNotExpand
              | Expands dp1 _ ctx delta =>
                let dp := NonEmptyHole u 0 dp1 in
                let (gamma, _) := ctx in
                let delta := MetaVarMap.extend delta (u, (ty, gamma)) in
                Expands dp ty ctx delta
              end
            end
            end
        with ana_expand'
          (fuel : Fuel.t)
          (ctx : Contexts.t)
          (p' : UHPat.t')
          (ty : HTyp.t)
          : expand_result :=
            match fuel with
            | Fuel.Kicked => DoesNotExpand
            | Fuel.More fuel => let ana_expand' := ana_expand' fuel in
            match p' with
            | UHPat.EmptyHole u =>
              let gamma := Contexts.gamma ctx in
              let dp := EmptyHole u 0 in
              let delta := MetaVarMap.extend MetaVarMap.empty
                                             (u, (ty, gamma)) in
              Expands dp ty ctx delta
            | UHPat.Var x =>
              (* TODO: add linearity check *)
              if Var.is_valid x then
                let ctx := Contexts.extend_gamma ctx (x, ty) in
                Expands (DHPat.Var x) ty ctx MetaVarMap.empty
              else
                DoesNotExpand
            | UHPat.Inj side p =>
              match HTyp.matched_sum ty with
              | None => DoesNotExpand
              | Some (tyL, tyR) => ana_expand fuel ctx p (pick_side side tyL tyR)
              end
            | UHPat.Wild
            | UHPat.NumLit _
            | UHPat.BoolLit _ =>
              match syn_expand' fuel ctx p' with
              | DoesNotExpand => DoesNotExpand
              | Expands dp ty1 ctx delta =>
                if HTyp.consistent ty ty1 then
                  Expands dp ty1 ctx delta
                else
                  DoesNotExpand
              end
            | UHPat.OpSeq skel seq => ana_expand_skel fuel ctx skel seq ty
            end
            end.
      End DHPat.

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
        | BoundVar : Var.t -> t
        | FreeVar : MetaVar.t -> inst_num -> VarMap.t_(t) -> Var.t -> t
        | Let : DHPat.t -> t -> t -> t
        | FixF : Var.t -> HTyp.t -> t -> t
        | Lam : DHPat.t -> HTyp.t -> t -> t
        | Ap  : t -> t -> t
        | BoolLit : bool -> t
        | NumLit : nat -> t
        | BinNumOp : bin_num_op -> t -> t -> t
        | Inj : HTyp.t -> inj_side -> t -> t
        | Pair : t -> t -> t
        | Case : t -> list(rule) -> t
        | EmptyHole : MetaVar.t -> inst_num -> VarMap.t_(t) -> t 
        | NonEmptyHole : MetaVar.t -> inst_num -> VarMap.t_(t) -> t -> t
        | Cast : t -> HTyp.t -> HTyp.t -> t
        | FailedCast : t -> HTyp.t -> HTyp.t -> t
        with rule : Type :=
        | Rule : DHPat.t -> t -> rule.

        Definition cast (d : t) (t1 : HTyp.t) (t2 : HTyp.t) : t := 
          if HTyp.eq t1 t2 then d else Cast d t1 t2.

        Module Environment.
          Definition t : Type := VarMap.t_(t).
          Include VarMap.
        End Environment.

        (* closed substitution [d1/x]d2*)
        Fixpoint subst (fuel : Fuel.t) (d1 : t) (x : Var.t) (d2 : t) : t := 
          match fuel with 
          | Fuel.Kicked => d2
          | Fuel.More fuel => let subst := subst fuel in 
            match d2 with 
            | BoundVar y => if Var.eq x y then d1 else d2
            | FreeVar _ _ _ _ => d2
            | Let dp d3 d4 =>
              let d3 := subst d1 x d3 in
              let d4 := if DHPat.binds_var x dp then d4 else subst d1 x d4 in
              Let dp d3 d4
            | FixF y ty d3 => 
              let d3 := if Var.eq x y then d3 else subst d1 x d3 in
              FixF y ty d3
            | Lam dp ty d3 =>
              if DHPat.binds_var x dp then d2 else
              let d3 := subst d1 x d3 in
              Lam dp ty d3
            | Ap d3 d4 => 
              let d3 := subst d1 x d3 in
              let d4 := subst d1 x d4 in
              Ap d3 d4
            | BoolLit _
            | NumLit _ => d2
            | BinNumOp op d3 d4 => 
              let d3 := subst d1 x d3 in
              let d4 := subst d1 x d4 in
              BinNumOp op d3 d4
            | Inj ty side d3 => 
              let d3 := subst d1 x d3 in
              Inj ty side d3
            | Pair d3 d4 =>
              let d3 := subst d1 x d3 in
              let d4 := subst d1 x d4 in
              Pair d3 d4
            | Case d3 rules =>
              let d3 := subst d1 x d3 in
              let rules := rules_subst fuel d1 x d2 rules in
              Case d3 rules
            | EmptyHole u i sigma => 
              let sigma' := env_subst fuel d1 x sigma in 
              EmptyHole u i sigma' 
            | NonEmptyHole u i sigma d3 => 
              let d3' := subst d1 x d3 in 
              let sigma' := env_subst fuel d1 x sigma in 
              NonEmptyHole u i sigma' d3'
            | Cast d ty1 ty2 => 
              let d' := subst d1 x d in 
              Cast d' ty1 ty2 
            | FailedCast d ty1 ty2 => 
              let d' := subst d1 x d in 
              FailedCast d' ty1 ty2
            end
          end
        with rules_subst (fuel : Fuel.t) (d1 : t) (x : Var.t) (d2 : t) (rules : list(rule)) :=
          match fuel with
          | Fuel.Kicked => rules
          | Fuel.More fuel =>
            List.map (fun (r : rule) =>
              match r with
              | Rule dp d3 => if DHPat.binds_var x dp then Rule dp (subst fuel d1 x d3) else r
              end) rules
          end
        with env_subst (fuel : Fuel.t) (d1 : t) (x : Var.t) (sigma : Environment.t) := 
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

        (* Implementation of type assignment judgment in POPL 2019 paper.
         * Not actually called anywhere, now stale.
        Fixpoint assign_type 
          (fuel : Fuel.t) 
          (gamma : VarCtx.t) (delta : Delta.t) 
          (d : t) 
          : type_result := 
            match fuel with 
            | Fuel.Kicked => IllTyped
            | Fuel.More fuel => 
            let assign_type := assign_type fuel in 
            match d with 
            | BoundVar x => 
              match (Var.is_valid x, VarMap.lookup gamma x) with 
              | (true, Some ty) => WellTyped ty
              | _ => IllTyped
              end
            | FreeVar u _ sigma x => 
              if (Var.is_valid x) then 
                match MetaVarMap.lookup delta u with 
                | Some (ty, gamma') => 
                  if check_type_env fuel gamma delta sigma gamma' then
                    WellTyped ty
                  else IllTyped
                | None => IllTyped
                end
              else IllTyped
            | Let x d1 d2 => 
              match (Var.is_valid_binder x, assign_type gamma delta d1) with 
              | (true, WellTyped ty1) => 
                let gamma' := VarMap.extend gamma (x, ty1) in 
                assign_type gamma' delta d2
              | _ => IllTyped
              end
            | FixF x ((HTyp.Arrow _ _) as ty1) d1 => 
              let gamma' := VarMap.extend gamma (x, ty1) in 
              match (Var.is_valid_binder x, assign_type gamma' delta d1) with 
              | (true, WellTyped ty2) => 
                if HTyp.eq ty1 ty2 then WellTyped ty2 else IllTyped
              | _ => IllTyped
              end
            | FixF x _ d1 => IllTyped
            | Lam x ty1 d1 => 
              let gamma' := VarMap.extend gamma (x, ty1) in 
              match (Var.is_valid_binder x, assign_type gamma' delta d1) with 
              | (true, WellTyped ty2) => WellTyped (HTyp.Arrow ty1 ty2)
              | _ => IllTyped
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
                | L => WellTyped (HTyp.Sum ty1 other_ty)
                | R => WellTyped (HTyp.Sum other_ty ty1)
                end
              end
            | Case d1 (x, d2) (y, d3) => 
              match ((Var.is_valid_binder x) && (Var.is_valid_binder y), assign_type gamma delta d1) with 
              | (true, WellTyped (HTyp.Sum tyL tyR)) => 
                let gamma1 := VarMap.extend gamma (x, tyL) in 
                let gamma2 := VarMap.extend gamma (y, tyR) in 
                match (assign_type gamma1 delta d2,
                       assign_type gamma2 delta d3) with 
                | (WellTyped ty2, WellTyped ty3) => 
                  if HTyp.eq ty2 ty3 then WellTyped ty2
                  else IllTyped
                | _ => IllTyped
                end
              | _ => IllTyped
              end
            | EmptyHole u _ sigma => 
              match MetaVarMap.lookup delta u with 
              | Some (ty, gamma') => 
                if check_type_env fuel gamma delta sigma gamma' then 
                  WellTyped ty
                else IllTyped
              | None => IllTyped
              end
            | NonEmptyHole u _ sigma d1 => 
              match assign_type gamma delta d1 with 
              | WellTyped _ => 
                match MetaVarMap.lookup delta u with 
                | Some (ty, gamma') => 
                  if check_type_env fuel gamma delta sigma gamma' then
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
                (gamma : VarCtx.t) (delta : Delta.t) 
                (sigma : Environment.t) 
                (gamma' : VarCtx.t) : bool := 
            match fuel with 
            | Fuel.More fuel => 
                Coq.Lists.List.forallb  
                  (fun xd : Var.t * t => 
                    let (x, d) := xd in 
                    match assign_type fuel gamma delta d with 
                    | WellTyped ty => 
                      match VarMap.lookup gamma' x with 
                      | Some ty' => HTyp.consistent ty ty'
                      | None => false
                      end
                    | IllTyped => false
                    end)
                  sigma
            | Fuel.Kicked => false
            end.
        *)
        
        Inductive expand_result : Type := 
        | Expands : t -> HTyp.t -> Delta.t -> expand_result
        | DoesNotExpand.

        Definition id_env (ctx : VarCtx.t) : Environment.t := 
          VarMap.map
            (fun xt : Var.t * HTyp.t => 
              let (x, _) := xt in DHExp.BoundVar x)
            ctx.

        Fixpoint syn_expand 
          (fuel : Fuel.t) 
          (ctx : Contexts.t) 
          (e : UHExp.t) 
          : expand_result := 
            match fuel with 
            | Fuel.Kicked => DoesNotExpand
            | Fuel.More fuel => 
            match e with 
            | UHExp.Tm (NotInHole) e' => syn_expand' fuel ctx e'
            | UHExp.Tm (InHole u) e' => 
              match syn_expand' fuel ctx e' with 
              | Expands d _ delta => 
                let (gamma, _) := ctx in 
                let sigma := id_env gamma in 
                let delta' := MetaVarMap.extend delta (u, (HTyp.Hole, gamma)) in 
                Expands 
                  (NonEmptyHole u 0 sigma d)
                  (HTyp.Hole)
                  (delta')
              | DoesNotExpand => DoesNotExpand
              end
            | UHExp.Parenthesized e1 => syn_expand fuel ctx e1
            end
            end
        with syn_expand'
          (fuel : Fuel.t)
          (ctx : Contexts.t)
          (e : UHExp.t')
          : expand_result := 
            match fuel with 
            | Fuel.Kicked => DoesNotExpand
            | Fuel.More fuel => 
            match e with 
            | UHExp.Asc e1 uty => 
              let ty := UHTyp.expand fuel uty in 
              match ana_expand fuel ctx e1 ty with 
              | DoesNotExpand => DoesNotExpand
              | Expands d1 ty' delta => 
                Expands 
                  (cast d1 ty' ty)
                  ty 
                  delta
              end
            | UHExp.Var (NotInVHole) x => 
              let (gamma, _) := ctx in 
              match VarMap.lookup gamma x with 
              | Some ty => Expands (DHExp.BoundVar x) ty MetaVarMap.empty
              | None => DoesNotExpand
              end
            | UHExp.Var (InVHole u) x => 
              let gamma := Contexts.gamma ctx in 
              let sigma := id_env gamma in 
              let delta := MetaVarMap.extend (MetaVarMap.empty) (u, (HTyp.Hole, gamma)) in 
              Expands
                (DHExp.FreeVar u 0 sigma x)
                (HTyp.Hole)
                delta
            | UHExp.Lam p ann e1 =>
              let ty1 :=
                match ann with 
                | Some uty1 => UHTyp.expand fuel uty1
                | None => HTyp.Hole
                end in
              match DHPat.ana_expand fuel ctx p ty1 with
              | DHPat.DoesNotExpand => DoesNotExpand
              | DHPat.Expands dp ty1 ctx deltap =>
                match syn_expand fuel ctx e1 with 
                | DoesNotExpand => DoesNotExpand
                | Expands d1 ty2 delta1 =>
                  let d := Lam dp ty1 d1 in
                  let delta := MetaVarMap.union deltap delta1 in
                  Expands d (HTyp.Arrow ty1 ty2) delta
                end
              end
            | UHExp.Let p ann e1 e2 =>
              match ann with
              | Some uty1 =>
                let ty1 := UHTyp.expand fuel uty1 in
                let (ctx1, is_recursive_fn) := UHExp.ctx_for_let' ctx p ty1 e1 in
                match ana_expand fuel ctx1 e1 ty1 with
                | DoesNotExpand => DoesNotExpand
                | Expands d1 ty1 delta1 =>
                  let d1 :=
                    match is_recursive_fn with
                    | None => d1
                    | Some x => FixF x ty1 d1
                    end in
                  match DHPat.ana_expand fuel ctx p ty1 with
                  | DHPat.DoesNotExpand => DoesNotExpand
                  | DHPat.Expands dp ty1 ctx2 deltap =>
                    match syn_expand fuel ctx2 e2 with 
                    | DoesNotExpand => DoesNotExpand
                    | Expands d2 ty delta2 => 
                      let d := Let dp d1 d2 in
                      let delta12 := MetaVarMap.union delta1 delta2 in
                      let delta := MetaVarMap.union delta12 deltap in
                      Expands d ty delta
                    end
                  end
                end
              | None =>
                match syn_expand fuel ctx e1 with
                | DoesNotExpand => DoesNotExpand
                | Expands d1 ty1 delta1 =>
                  match DHPat.ana_expand fuel ctx p ty1 with
                  | DHPat.DoesNotExpand => DoesNotExpand
                  | DHPat.Expands dp ty1 ctx' deltap =>
                    match syn_expand fuel ctx' e2 with 
                    | DoesNotExpand => DoesNotExpand
                    | Expands d2 ty delta2 => 
                      let d := Let dp d1 d2 in
                      let delta12 := MetaVarMap.union delta1 delta2 in
                      let delta := MetaVarMap.union delta12 deltap in
                      Expands d ty delta
                    end
                  end
                end
              end
            | UHExp.NumLit n => 
              Expands (NumLit n) HTyp.Num MetaVarMap.empty
            | UHExp.BoolLit b =>
              Expands (BoolLit b) HTyp.Bool MetaVarMap.empty
            | UHExp.EmptyHole u => 
              let gamma := Contexts.gamma ctx in 
              let sigma := id_env gamma in 
              let d := DHExp.EmptyHole u 0 sigma in 
              let ty := HTyp.Hole in 
              let delta := MetaVarMap.extend MetaVarMap.empty 
                           (u, (ty, gamma)) in 
              Expands d ty delta
            | UHExp.OpSeq skel seq => 
              syn_expand_skel fuel ctx skel seq
            | UHExp.Inj side e1 => 
              match syn_expand fuel ctx e1 with 
              | DoesNotExpand => DoesNotExpand
              | Expands d1 ty1 delta1 => 
                let d := DHExp.Inj HTyp.Hole side d1 in 
                let ty := 
                  match side with 
                  | L => HTyp.Sum ty1 HTyp.Hole
                  | R => HTyp.Sum HTyp.Hole ty1
                  end in 
                Expands d ty delta1
              end
            | UHExp.Case _ _ => DoesNotExpand
            | UHExp.ApPalette name serialized_model hole_data => DoesNotExpand
              (* TODO fix me *)
              (* let (_, palette_ctx) := ctx in 
              match (VarMap.lookup palette_ctx name) with
              | Some palette_defn => 
                let expansion_ty := UHExp.PaletteDefinition.expansion_ty palette_defn in  
                let to_exp := UHExp.PaletteDefinition.to_exp palette_defn in
                let expansion := to_exp serialized_model in 
                let (_, hole_map) := hole_data in
                (* bind each free variable in expansion by wrapping expansion
                 * in lambda, then apply lambda to args in hole data
                 *)
                let bound_expansion :=
                    NatMap.fold hole_map
                      (fun bound entry =>
                        let (n, typ_exp) := entry in
                        let (htyp, hexp) := typ_exp in
                        let lam := UHExp.Tm NotInHole (UHExp.Lam (UHExp.PaletteHoleData.mk_hole_ref_var_name n) bound) in
                        let hexp_ann := UHExp.Tm NotInHole (UHExp.Asc (UHExp.Parenthesized hexp) (UHTyp.contract htyp)) in
                        let opseq := OperatorSeq.ExpOpExp (UHExp.Parenthesized lam) UHExp.Space (UHExp.Parenthesized hexp_ann) in
                        let ap := UHExp.OpSeq (Associator.associate_exp opseq) opseq in
                        UHExp.Tm NotInHole ap
                      )
                      expansion in
                ana_expand fuel ctx bound_expansion expansion_ty
              | None => DoesNotExpand
              end *)
            end
            end
        with syn_expand_skel 
          (fuel : Fuel.t)
          (ctx : Contexts.t)
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
              | Some en => syn_expand fuel ctx en
              end
            | Skel.BinOp (InHole u) op skel1 skel2 => 
              let skel_not_in_hole := Skel.BinOp NotInHole op skel1 skel2 in 
              match syn_expand_skel fuel ctx skel_not_in_hole seq with 
              | DoesNotExpand => DoesNotExpand
              | Expands d _ delta => 
                let gamma := Contexts.gamma ctx in 
                let sigma := id_env gamma in 
                let delta' := MetaVarMap.extend delta (u, (HTyp.Hole, gamma)) in 
                Expands
                  (NonEmptyHole u 0 sigma d)
                  HTyp.Hole delta'
              end
            | Skel.BinOp NotInHole UHExp.Space skel1 skel2 => 
              match UHExp.syn_skel fuel ctx skel1 seq None with 
              | None => DoesNotExpand
              | Some (ty1, _) => 
                match HTyp.matched_arrow ty1 with 
                | None => DoesNotExpand
                | Some (ty2, ty) => 
                  let ty2_arrow_ty := HTyp.Arrow ty2 ty in 
                  match ana_expand_skel fuel ctx skel1 seq ty2_arrow_ty with 
                  | DoesNotExpand => DoesNotExpand
                  | Expands d1 ty1' delta1 => 
                    match ana_expand_skel fuel ctx skel2 seq ty2 with 
                    | DoesNotExpand => DoesNotExpand
                    | Expands d2 ty2' delta2 => 
                      let delta := MetaVarMap.union delta1 delta2 in 
                      let dc1 := cast d1 ty1' ty2_arrow_ty in 
                      let dc2 := cast d2 ty2' ty2 in 
                      let d := Ap dc1 dc2 in 
                      Expands d ty delta
                    end
                  end
                end
              end
            | Skel.BinOp NotInHole UHExp.Comma skel1 skel2 =>
              match syn_expand_skel fuel ctx skel1 seq  with
              | DoesNotExpand => DoesNotExpand
              | Expands d1 ty1 delta1 =>
                match syn_expand_skel fuel ctx skel2 seq with
                | DoesNotExpand => DoesNotExpand
                | Expands d2 ty2 delta2 =>
                  let d := Pair d1 d2 in
                  let ty := HTyp.Prod ty1 ty2 in
                  let delta := MetaVarMap.union delta1 delta2 in
                  Expands d ty delta
                end
              end
            | Skel.BinOp NotInHole (UHExp.Plus as op) skel1 skel2
            | Skel.BinOp NotInHole (UHExp.Times as op) skel1 skel2 => 
              match (ana_expand_skel fuel ctx skel1 seq HTyp.Num,
                     ana_expand_skel fuel ctx skel2 seq HTyp.Num) with 
              | (Expands d1 ty1 delta1, Expands d2 ty2 delta2) => 
                let delta := MetaVarMap.union delta1 delta2 in 
                let dc1 := cast d1 ty1 HTyp.Num in 
                let dc2 := cast d2 ty2 HTyp.Num in 
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
          (ctx : Contexts.t) 
          (e : UHExp.t)
          (ty : HTyp.t) 
          : expand_result := 
            match fuel with 
            | Fuel.Kicked => DoesNotExpand
            | Fuel.More fuel => 
            match e with 
            | UHExp.Tm (InHole u) e' =>
              match syn_expand' fuel ctx e' with 
              | DoesNotExpand => DoesNotExpand
              | Expands d _ delta => 
                let gamma := Contexts.gamma ctx in 
                let sigma := id_env gamma in 
                let delta' := MetaVarMap.extend delta (u, (ty, gamma)) in 
                Expands 
                  (NonEmptyHole u 0 sigma d)
                  ty
                  delta'
              end
            | UHExp.Tm NotInHole e' => ana_expand' fuel ctx e' ty
            | UHExp.Parenthesized e1 => ana_expand fuel ctx e1 ty
            end
            end
        with ana_expand'
          (fuel : Fuel.t)
          (ctx : Contexts.t)
          (e : UHExp.t')
          (ty : HTyp.t) 
          : expand_result := 
            match fuel with 
            | Fuel.Kicked => DoesNotExpand
            | Fuel.More fuel => 
            match e with 
            | UHExp.Lam p ann e1 =>
              match HTyp.matched_arrow ty with
              | None => DoesNotExpand
              | Some (ty1_given, ty2) =>
                let ty1 :=
                    match ann with 
                    | Some uty1 => UHTyp.expand fuel uty1
                    | None => ty1_given
                    end in
                match DHPat.ana_expand fuel ctx p ty1 with
                | DHPat.DoesNotExpand => DoesNotExpand
                | DHPat.Expands dp ty1 ctx deltap =>
                  match ana_expand fuel ctx e1 ty2 with
                  | DoesNotExpand => DoesNotExpand
                  | Expands d1 ty2 delta1 =>
                    let ty := HTyp.Arrow ty1 ty2 in
                    let d := Lam dp ty1 d1 in
                    let delta := MetaVarMap.union deltap delta1 in
                    Expands d ty delta
                  end
                end
              end
            | UHExp.Inj side e1 => 
              match HTyp.matched_sum ty with 
              | None => DoesNotExpand
              | Some (ty1, ty2) => 
                let e1ty := pick_side side ty1 ty2 in 
                match ana_expand fuel ctx e1 e1ty with 
                | DoesNotExpand => DoesNotExpand
                | Expands d1 e1ty' delta => 
                  let (ann_ty, ty) := 
                    match side with 
                    | L => (ty2, HTyp.Sum e1ty' ty2) 
                    | R => (ty1, HTyp.Sum ty1 e1ty')
                    end in 
                  let d := Inj ann_ty side d1 in 
                  Expands d ty delta
                end
              end
            | UHExp.Case e1 rules =>
              match syn_expand fuel ctx e1 with
              | DoesNotExpand => DoesNotExpand
              | Expands d1 ty1 delta1 =>
                match ana_expand_rules fuel ctx rules ty1 ty with
                | None => DoesNotExpand
                | Some (drs, delta2) =>
                  let d := Case d1 drs in
                  let delta := MetaVarMap.union delta1 delta2 in
                  Expands d1 ty delta
                end
              end
            | UHExp.EmptyHole u => 
              let gamma := Contexts.gamma ctx in 
              let sigma := id_env gamma in 
              let d := EmptyHole u 0 sigma in 
              let delta := MetaVarMap.extend MetaVarMap.empty (u, (ty, gamma)) in 
              Expands d ty delta
            | UHExp.Var (InVHole u) x => 
              let gamma := Contexts.gamma ctx in 
              let sigma := id_env gamma in 
              let delta := MetaVarMap.extend (MetaVarMap.empty) (u, (ty, gamma)) in 
              Expands 
                (FreeVar u 0 sigma x)
                ty
                delta
            | UHExp.Let p ann e1 e2 =>
              match ann with
              | Some uty1 =>
                let ty1 := UHTyp.expand fuel uty1 in
                (* TODO: is there a reason we don't handle recursive functions here? *)
                match ana_expand fuel ctx e1 ty1 with
                | DoesNotExpand => DoesNotExpand
                | Expands d1 ty1 delta1 =>
                  match DHPat.ana_expand fuel ctx p ty1 with
                  | DHPat.DoesNotExpand => DoesNotExpand
                  | DHPat.Expands dp ty1 ctx2 deltap =>
                    match ana_expand fuel ctx2 e2 ty with
                    | DoesNotExpand => DoesNotExpand
                    | Expands d2 ty2 delta2 =>
                      let d := Let dp d1 d2 in
                      let delta12 := MetaVarMap.union delta1 delta2 in
                      let delta := MetaVarMap.union delta12 deltap in
                      Expands d ty2 delta
                    end
                  end
                end
              | None =>
                match syn_expand fuel ctx e1 with
                | DoesNotExpand => DoesNotExpand
                | Expands d1 ty1 delta1 =>
                  match DHPat.ana_expand fuel ctx p ty1 with
                  | DHPat.DoesNotExpand => DoesNotExpand
                  | DHPat.Expands dp ty1 ctx2 deltap =>
                    match ana_expand fuel ctx2 e2 ty with
                    | DoesNotExpand => DoesNotExpand
                    | Expands d2 ty2 delta2 =>
                      let d := Let dp d1 d2 in
                      let delta12 := MetaVarMap.union delta1 delta2 in
                      let delta := MetaVarMap.union delta12 deltap in
                      Expands d ty2 delta
                    end
                  end
                end
              end
            | UHExp.Asc _ _ 
            | UHExp.Var NotInVHole _
            | UHExp.BoolLit _
            | UHExp.NumLit _
            | UHExp.OpSeq _ _
            | UHExp.ApPalette _ _ _ => 
              (* subsumption *)
              match syn_expand' fuel ctx e with 
              | DoesNotExpand => DoesNotExpand
              | (Expands d ty' delta) as result => 
                if HTyp.consistent ty ty'  
                then result
                else DoesNotExpand
              end
            end
            end
        with ana_expand_rules
          (fuel : Fuel.t)
          (ctx : Contexts.t)
          (rules : list(UHExp.rule))
          (pat_ty : HTyp.t)
          (clause_ty : HTyp.t)
          : option(list(rule) * Delta.t) :=
          match fuel with
          | Fuel.Kicked => None
          | Fuel.More fuel =>
            List.fold_left (fun b r =>
              match b with
              | None => None
              | Some (drs, delta) =>
                match ana_expand_rule fuel ctx r pat_ty clause_ty with
                | None => None
                | Some (dr, deltar) =>
                  (* TODO: make faster *)
                  let drs := drs ++ (cons dr nil) in
                  let delta := MetaVarMap.union delta deltar in
                  Some (drs, delta)
                end
              end) rules (Some (nil, MetaVarMap.empty))
          end
        with ana_expand_rule
          (fuel : Fuel.t)
          (ctx : Contexts.t)
          (r : UHExp.rule)
          (pat_ty : HTyp.t)
          (clause_ty : HTyp.t)
          : option(rule * Delta.t) :=
          match fuel with
          | Fuel.Kicked => None
          | Fuel.More fuel =>
            let (p, e) := r in
            match DHPat.ana_expand fuel ctx p pat_ty with
            | DHPat.DoesNotExpand => None
            | DHPat.Expands dp _ ctx delta1 =>
              match ana_expand fuel ctx e clause_ty with
              | DoesNotExpand => None
              | Expands d _ delta2 =>
                (* TODO: is it fine to ignore the type here? maybe necessary if case expression is in a cast? if so, not sure what to do with all the expanded clause types *)
                let delta := MetaVarMap.union delta1 delta2 in
                Some (Rule dp d, delta)
              end
            end
          end
        with ana_expand_skel
          (fuel : Fuel.t)
          (ctx : Contexts.t)
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
              | Some en => ana_expand fuel ctx en ty
              end
            | _ => 
              match syn_expand_skel fuel ctx skel seq with 
              | DoesNotExpand => DoesNotExpand
              | Expands d ty' delta => 
                if HTyp.consistent ty ty' then 
                  Expands d ty' delta
                else DoesNotExpand
              end
            end
            end.

        Module HoleInstance.
          Definition t : Type := MetaVar.t * inst_num.
        End HoleInstance.

        Module InstancePath.
          Definition t : Type := list(HoleInstance.t * Var.t). 
        End InstancePath.

        Module HoleInstanceInfo.
          Definition t : Type := MetaVarMap.t (list (Environment.t * InstancePath.t)).

          Definition empty : t := MetaVarMap.empty.

          Definition next (hii : t) (u : MetaVar.t) (sigma : Environment.t) (path : InstancePath.t) : nat * t := 
            let (envs, hii) := 
              MetaVarMap.insert_or_map hii u (fun _ => (cons (sigma, path) nil)) (
                fun envs => 
                  cons (sigma, path) envs 
              ) in
            ((List.length envs) - 1, hii).

          Definition update_environment (hii : t) (inst : HoleInstance.t) (sigma : Environment.t) : t := 
            let (u, i) := inst in 
            let (_, hii) := MetaVarMap.update_with
              (
                fun instances => 
                  let length := List.length instances in 
                  Util.update_nth (length - i - 1) instances 
                  (fun (inst_info : Environment.t * InstancePath.t) => 
                    let (_, path) := inst_info in 
                    (sigma, path)
                  )
              )
              u hii nil in 
            hii. 

          Definition num_instances (hii : t) (u : MetaVar.t) := 
            match MetaVarMap.lookup hii u with 
            | Some envs => List.length envs
            | None => 0
            end.

          Definition default_instance (hii : t) (u : MetaVar.t) := 
            match MetaVarMap.lookup hii u with 
            | Some envs => 
              match envs with 
              | nil => None
              | cons _ _ => Some (u, 0)
              end
            | None => None
            end.

          Definition lookup (hii : t) (inst : HoleInstance.t) := 
            let (u, i) := inst in 
            match MetaVarMap.lookup hii u with 
            | Some envs => 
              let length := List.length envs in 
              List.nth_error envs (length - i - 1)
            | None => None
            end.
        End HoleInstanceInfo.

        Fixpoint renumber_result_only_pat
          (path : InstancePath.t)
          (hii : HoleInstanceInfo.t)
          (dp : DHPat.t)
          : (DHPat.t * HoleInstanceInfo.t) :=
          match dp with
          | DHPat.Wild
          | DHPat.Var _
          | DHPat.NumLit _
          | DHPat.BoolLit _ => (dp, hii)
          | DHPat.EmptyHole u _ =>
            (* TODO: Pattern holes don't need environments. Maybe this calls
             * for a refactoring of types to reflect this, e.g., a separate
             * PatHoleInstance type. Passing in an empty environment for now. *)
            let sigma := Environment.empty in
            let (i, hii) := HoleInstanceInfo.next hii u sigma path in
            (DHPat.EmptyHole u i, hii)
          | DHPat.NonEmptyHole u _ dp1 =>
            (* TODO: see above *)
            let sigma := Environment.empty in
            let (i, hii) := HoleInstanceInfo.next hii u sigma path in
            let (dp1, hii) := renumber_result_only_pat path hii dp1 in
            (DHPat.NonEmptyHole u i dp1, hii)
          | DHPat.Inj side dp1 =>
            let (dp1, hii) := renumber_result_only_pat path hii dp1 in
            (DHPat.Inj side dp1, hii)
          | DHPat.Pair dp1 dp2 =>
            let (dp1, hii) := renumber_result_only_pat path hii dp1 in
            let (dp2, hii) := renumber_result_only_pat path hii dp2 in
            (DHPat.Pair dp1 dp2, hii)
          | DHPat.Ap dp1 dp2 =>
            let (dp1, hii) := renumber_result_only_pat path hii dp1 in
            let (dp2, hii) := renumber_result_only_pat path hii dp2 in
            (DHPat.Ap dp1 dp2, hii)
          end.

        Fixpoint renumber_result_only
          (fuel : Fuel.t)
          (path : InstancePath.t) (hii : HoleInstanceInfo.t) (d : DHExp.t) 
          : (DHExp.t * HoleInstanceInfo.t) :=
          match fuel with
          | Fuel.Kicked => (d, hii)
          | Fuel.More fuel => let renumber_result_only := renumber_result_only fuel in
          match d with 
          | BoundVar _
          | BoolLit _
          | NumLit _ => (d, hii)
          | Let x d1 d2 => 
            let (d1, hii) := renumber_result_only path hii d1 in 
            let (d2, hii) := renumber_result_only path hii d2 in 
            (Let x d1 d2, hii)
          | FixF x ty d1 => 
            let (d1, hii) := renumber_result_only path hii d1 in 
            (FixF x ty d1, hii)
          | Lam x ty d1 => 
            let (d1, hii) := renumber_result_only path hii d1 in 
            (Lam x ty d1, hii)
          | Ap d1 d2 => 
            let (d1, hii) := renumber_result_only path hii d1 in
            let (d2, hii) := renumber_result_only path hii d2 in 
            (Ap d1 d2, hii)
          | BinNumOp op d1 d2 => 
            let (d1, hii) := renumber_result_only path hii d1 in
            let (d2, hii) := renumber_result_only path hii d2 in 
            (BinNumOp op d1 d2, hii)
          | Inj ty side d1 => 
            let (d1, hii) := renumber_result_only path hii d1 in 
            (Inj ty side d1, hii)
          | Pair d1 d2 =>
            let (d1, hii) := renumber_result_only path hii d1 in
            let (d2, hii) := renumber_result_only path hii d2 in
            (Pair d1 d2, hii)
          | Case d1 rules => 
            let (d1, hii) := renumber_result_only path hii d1 in
            let (drules, hii) := renumber_result_only_rules fuel path hii rules in 
            (Case d1 drules, hii)
          | EmptyHole u _ sigma => 
            let (i, hii) := HoleInstanceInfo.next hii u sigma path in 
            (EmptyHole u i sigma, hii)
          | NonEmptyHole u _ sigma d1 => 
            let (i, hii) := HoleInstanceInfo.next hii u sigma path in 
            let (d1, hii) := renumber_result_only path hii d1 in 
            (NonEmptyHole u i sigma d1, hii)
          | FreeVar u _ sigma x => 
            let (i, hii) := HoleInstanceInfo.next hii u sigma path in 
            (FreeVar u i sigma x, hii)
          | Cast d1 ty1 ty2 => 
            let (d1, hii) := renumber_result_only path hii d1 in 
            (Cast d1 ty1 ty2, hii)
          | FailedCast d1 ty1 ty2 => 
            let (d1, hii) := renumber_result_only path hii d1 in 
            (FailedCast d1 ty1 ty2, hii)
          end
          end
        with renumber_result_only_rules
          (fuel : Fuel.t)
          (path : InstancePath.t)
          (hii : HoleInstanceInfo.t)
          (rules : list(rule))
          : (list(rule) * HoleInstanceInfo.t) :=
          match fuel with
          | Fuel.Kicked => (rules, hii)
          | Fuel.More fuel =>
          List.fold_left (fun (b : list(rule) * HoleInstanceInfo.t) r =>
            let (rs, hii) := b in
            match r with
            | Rule dp d =>
              let (dp, hii) := renumber_result_only_pat path hii dp in
              let (d, hii) := renumber_result_only fuel path hii d in
              (rs ++ (cons (Rule dp d) nil), hii)
            end) rules (nil, hii)
          end.

        Fixpoint renumber_sigmas_only (fuel : Fuel.t) 
          (path : InstancePath.t) (hii : HoleInstanceInfo.t) (d : DHExp.t) 
          : (DHExp.t * HoleInstanceInfo.t) := 
          match fuel with 
          | Fuel.Kicked => (d, hii)
          | Fuel.More fuel => 
          match d with 
          | BoundVar _
          | BoolLit _
          | NumLit _ => (d, hii)
          | Let x d1 d2 => 
            let (d1, hii) := renumber_sigmas_only fuel path hii d1 in 
            let (d2, hii) := renumber_sigmas_only fuel path hii d2 in 
            (Let x d1 d2, hii)
          | FixF x ty d1 => 
            let (d1, hii) := renumber_sigmas_only fuel path hii d1 in 
            (FixF x ty d1, hii)
          | Lam x ty d1 => 
            let (d1, hii) := renumber_sigmas_only fuel path hii d1 in 
            (Lam x ty d1, hii)
          | Ap d1 d2 => 
            let (d1, hii) := renumber_sigmas_only fuel path hii d1 in
            let (d2, hii) := renumber_sigmas_only fuel path hii d2 in 
            (Ap d1 d2, hii)
          | BinNumOp op d1 d2 => 
            let (d1, hii) := renumber_sigmas_only fuel path hii d1 in
            let (d2, hii) := renumber_sigmas_only fuel path hii d2 in 
            (BinNumOp op d1 d2, hii)
          | Inj ty side d1 => 
            let (d1, hii) := renumber_sigmas_only fuel path hii d1 in 
            (Inj ty side d1, hii)
          | Pair d1 d2 => 
            let (d1, hii) := renumber_sigmas_only fuel path hii d1 in
            let (d2, hii) := renumber_sigmas_only fuel path hii d2 in 
            (Pair d1 d2, hii)
          | Case dp rules =>
            (* pattern holes don't have environments *)
            let (rules, hii) := renumber_sigmas_only_rules fuel path hii rules in
            (Case dp rules, hii)
          | EmptyHole u i sigma => 
            let (sigma, hii) := renumber_sigma fuel path u i hii sigma in
            let hii := HoleInstanceInfo.update_environment hii (u, i) sigma in 
            (EmptyHole u i sigma, hii)
          | NonEmptyHole u i sigma d1 => 
            let (sigma, hii) := renumber_sigma fuel path u i hii sigma in 
            let hii := HoleInstanceInfo.update_environment hii (u, i) sigma in 
            let (d1, hii) := renumber_sigmas_only fuel path hii d1 in 
            (NonEmptyHole u i sigma d1, hii)
          | FreeVar u i sigma x => 
            let (sigma, hii) := renumber_sigma fuel path u i hii sigma in 
            let hii := HoleInstanceInfo.update_environment hii (u, i) sigma in 
            (FreeVar u i sigma x, hii)
          | Cast d1 ty1 ty2 => 
            let (d1, hii) := renumber_sigmas_only fuel path hii d1 in 
            (Cast d1 ty1 ty2, hii)
          | FailedCast d1 ty1 ty2 => 
            let (d1, hii) := renumber_sigmas_only fuel path hii d1 in 
            (FailedCast d1 ty1 ty2, hii)
          end
          end
        with renumber_sigmas_only_rules
          (fuel : Fuel.t)
          (path : InstancePath.t)
          (hii : HoleInstanceInfo.t)
          (rules : list(rule))
          : (list(rule) * HoleInstanceInfo.t) :=
          match fuel with
          | Fuel.Kicked => (rules, hii)
          | Fuel.More fuel =>
          List.fold_left (fun (b : list(rule) * HoleInstanceInfo.t) r =>
            let (rs, hii) := b in
            match r with
            | Rule dp d =>
              let (d, hii) := renumber_sigmas_only fuel path hii d in
              (rs ++ (cons (Rule dp d) nil), hii)
            end) rules (nil, hii)
          end
        with renumber_sigma (fuel : Fuel.t) 
          (path : InstancePath.t) (u : MetaVar.t) (i : inst_num)
          (hii : HoleInstanceInfo.t) (sigma : DHExp.Environment.t) 
          : (DHExp.Environment.t * HoleInstanceInfo.t) := 
          match fuel with 
          | Fuel.Kicked => (sigma, hii)
          | Fuel.More fuel => 
          let (sigma, hii) := List.fold_right 
            (fun (xd : Var.t * DHExp.t) (acc : DHExp.Environment.t * HoleInstanceInfo.t) => 
              let (x, d) := xd in 
              let (sigma_in, hii) := acc in 
              let path := cons (u, i, x) path in 
              let (d, hii) := renumber_result_only fuel path hii d in 
              let sigma_out := cons (x, d) sigma_in in 
              (sigma_out, hii)
            )
            (nil, hii) 
            sigma
            in 
          List.fold_right
            (fun (xd : Var.t * DHExp.t) (acc : DHExp.Environment.t * HoleInstanceInfo.t) => 
              let (x, d) := xd in 
              let (sigma_in, hii) := acc in 
              let path := cons (u, i, x) path in 
              let (d, hii) := renumber_sigmas_only fuel path hii d in 
              let sigma_out := cons (x, d) sigma_in in 
              (sigma_out, hii)
            )
            (nil, hii)
            sigma
          end.
        
        Fixpoint renumber (fuel : Fuel.t) 
          (path : InstancePath.t) (hii : HoleInstanceInfo.t) (d : DHExp.t) 
          : (DHExp.t * HoleInstanceInfo.t) := 
          match fuel with 
          | Fuel.Kicked => (d, hii)
          | Fuel.More fuel => 
          let (d, hii) := renumber_result_only fuel path hii d in 
          renumber_sigmas_only fuel path hii d
          end.
      End DHExp.

      Module Evaluator.
        Inductive result := 
        | InvalidInput : nat -> result (* not well-typed or otherwise invalid *)
        | BoxedValue : DHExp.t -> result
        | Indet : DHExp.t -> result.

        (* 
          0 = out of fuel
          1 = free or invalid variable
          2 = ap invalid boxed function val
          3 = boxed value not a number literal 2
          4 = boxed value not a number literal 1
          5 = Case scrutinee bad
          6 = Cast BV Hole Ground
        *)

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
            | Fuel.Kicked => InvalidInput 0
            | Fuel.More(fuel) => 
            match d with 
            | DHExp.BoundVar _ => InvalidInput 1
            | DHExp.Let x d1 d2 => 
              match evaluate fuel d1 with 
              | InvalidInput msg => InvalidInput msg
              | BoxedValue d1' | Indet d1' => 
                evaluate fuel (DHExp.subst fuel d1' x d2)
              end
            | DHExp.FixF x ty d1 => 
              evaluate fuel (DHExp.subst fuel d x d1)
            | DHExp.Lam x _ _ =>
              BoxedValue d
            | DHExp.Ap d1 d2 => 
              match evaluate fuel d1 with 
              | InvalidInput msg => InvalidInput msg
              | BoxedValue (DHExp.Lam x tau d1') => 
                match evaluate fuel d2 with 
                | InvalidInput msg => InvalidInput msg
                | BoxedValue d2' | Indet d2' => 
                  (* beta rule *)
                  evaluate fuel (DHExp.subst fuel d2' x d1')
                end
              | BoxedValue (DHExp.Cast d1' (HTyp.Arrow ty1 ty2) (HTyp.Arrow ty1' ty2'))
              | Indet (DHExp.Cast d1' (HTyp.Arrow ty1 ty2) (HTyp.Arrow ty1' ty2')) => 
                match evaluate fuel d2 with 
                | InvalidInput msg => InvalidInput msg
                | BoxedValue d2' | Indet d2' => 
                  (* ap cast rule *)
                  evaluate fuel  
                    (DHExp.Cast 
                      (DHExp.Ap 
                        d1'
                        (DHExp.Cast
                          d2' ty1' ty1))
                      ty2 ty2')
                end
              | BoxedValue _ => InvalidInput 2
              | Indet d1' => 
                match evaluate fuel d2 with 
                | InvalidInput msg => InvalidInput msg
                | BoxedValue d2' | Indet d2' => 
                  Indet (DHExp.Ap d1' d2')
                end
              end
            | DHExp.NumLit _ => BoxedValue d
            | DHExp.BinNumOp op d1 d2 => 
              match evaluate fuel d1 with 
              | InvalidInput msg => InvalidInput msg
              | BoxedValue (DHExp.NumLit n1 as d1')  => 
                match evaluate fuel d2 with 
                | InvalidInput msg => InvalidInput msg
                | BoxedValue (DHExp.NumLit n2) => 
                  BoxedValue (DHExp.NumLit (eval_bin_num_op op n1 n2))
                | BoxedValue _ => InvalidInput 3 
                | Indet d2' => 
                  Indet (DHExp.BinNumOp op d1' d2')
                end
              | BoxedValue _ => InvalidInput 4
              | Indet d1' => 
                match evaluate fuel d2 with 
                | InvalidInput msg => InvalidInput msg
                | BoxedValue d2' | Indet d2' => 
                  Indet (DHExp.BinNumOp op d1' d2')
                end
              end
            | DHExp.Inj ty side d1 => 
              match evaluate fuel d1 with 
              | InvalidInput msg => InvalidInput msg
              | BoxedValue d1' => BoxedValue (DHExp.Inj ty side d1')
              | Indet d1' => Indet (DHExp.Inj ty side d1')
              end
            | DHExp.Case d1 (x, d2) (y, d3) =>
              if (Var.is_valid_binder x) && (Var.is_valid_binder y) then
                match evaluate fuel d1 with 
                | InvalidInput msg => InvalidInput msg
                | BoxedValue d1' => 
                  match d1' with 
                  | DHExp.Inj _ side d1'' => 
                    let (xb, db) := pick_side side (x, d2) (y, d3) in
                    let branch := DHExp.subst fuel d1'' xb db in 
                    evaluate fuel branch
                  | DHExp.Cast d1'' (HTyp.Sum ty1 ty2) (HTyp.Sum ty1' ty2') => 
                    let d' := 
                      DHExp.Case d1'' 
                        (x, DHExp.subst fuel (DHExp.Cast (DHExp.BoundVar x) ty1 ty1') x d2)
                        (y, DHExp.subst fuel (DHExp.Cast (DHExp.BoundVar y) ty2 ty2') y d3) in 
                      evaluate fuel d'
                  | _ => InvalidInput 5
                  end
                | Indet d1' => 
                  match d1' with 
                  | DHExp.Inj _ side d1'' => 
                      let (xb, db) := pick_side side (x, d2) (y, d3) in 
                      let branch := DHExp.subst fuel d1'' xb db in 
                      evaluate fuel branch
                  | DHExp.Cast d1'' (HTyp.Sum ty1 ty2) (HTyp.Sum ty1' ty2') => 
                    let d' := 
                      DHExp.Case d1'' 
                        (x, DHExp.subst fuel (DHExp.Cast (DHExp.BoundVar x) ty1 ty1') x d2)
                        (y, DHExp.subst fuel (DHExp.Cast (DHExp.BoundVar y) ty2 ty2') y d3) in 
                    evaluate fuel d'
                  | _ => Indet (DHExp.Case d1' (x, d2) (y, d3))
                  end
                end
              else
                InvalidInput 1
            | DHExp.EmptyHole u i sigma => 
              Indet d  
            | DHExp.NonEmptyHole u i sigma d1 => 
              match evaluate fuel d1 with 
              | InvalidInput msg => InvalidInput msg
              | BoxedValue d1' | Indet d1' => 
                Indet (DHExp.NonEmptyHole u i sigma d1')
              end
            | DHExp.FreeVar u i sigma x => 
              Indet d
            | DHExp.Cast d1 ty ty' => 
              match evaluate fuel d1 with 
              | InvalidInput msg => InvalidInput msg
              | (BoxedValue d1' as result) => 
                match (ground_cases_of ty, ground_cases_of ty') with 
                | (Hole, Hole) => result
                | (Ground, Ground) =>  
                  (* if two types are ground and consistent, then they are eq *)
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
                  | _ => InvalidInput 6
                  end
                | (Hole, NotGroundOrHole ty'_grounded) => 
                  (* ITExpand rule *)
                  let d' := 
                    DHExp.Cast
                      (DHExp.Cast d1' ty ty'_grounded)
                      ty'_grounded ty' in 
                  evaluate fuel d'
                | (NotGroundOrHole ty_grounded, Hole) => 
                  (* ITGround rule *)
                   let d' := 
                     DHExp.Cast
                       (DHExp.Cast d1' ty ty_grounded)
                       ty_grounded ty' in 
                   evaluate fuel d'
                | (Ground, NotGroundOrHole _)  
                | (NotGroundOrHole _, Ground) => 
                  (* can't do anything when casting between diseq, non-hole types *)
                  BoxedValue (DHExp.Cast d1' ty ty')
                | (NotGroundOrHole _, NotGroundOrHole _) => 
                  (* they might be eq in this case, so remove cast if so *)
                  if HTyp.eq ty ty' then result 
                  else BoxedValue (DHExp.Cast d1' ty ty')
                end
              | (Indet d1' as result) => 
                match (ground_cases_of ty, ground_cases_of ty') with 
                | (Hole, Hole) => result
                | (Ground, Ground) =>  
                  (* if two types are ground and consistent, then they are eq *)
                  result
                | (Ground, Hole) => 
                  (* can't remove the cast or do anything else here, so we're done *)
                  Indet (DHExp.Cast d1' ty ty')
                | (Hole, Ground) => 
                  match d1' with 
                  | DHExp.Cast d1'' ty'' HTyp.Hole => 
                    if HTyp.eq ty'' ty' then Indet d1''
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
                  evaluate fuel d'
                | (NotGroundOrHole ty_grounded, Hole) => 
                  (* ITGround rule *)
                   let d' := 
                     DHExp.Cast
                       (DHExp.Cast d1' ty ty_grounded)
                       ty_grounded ty' in 
                   evaluate fuel d'
                | (Ground, NotGroundOrHole _)  
                | (NotGroundOrHole _, Ground) => 
                  (* can't do anything when casting between diseq, non-hole types *)
                  Indet (DHExp.Cast d1' ty ty')
                | (NotGroundOrHole _, NotGroundOrHole _) => 
                  (* it might be eq in this case, so remove cast if so *)
                  if HTyp.eq ty ty' then result else Indet (DHExp.Cast d1' ty ty')
                end
              end
            | DHExp.FailedCast d1 ty ty' => 
              match evaluate fuel d1 with 
              | InvalidInput msg => InvalidInput msg
              | BoxedValue d1' | Indet d1' => 
                Indet (DHExp.FailedCast d1' ty ty')
              end
            end
            end.
      End Evaluator.
  End FDynamics.
End FCore.

(* Extract Constant Util.str_eqb => "String.equal".
Extract Constant Coq.Strings.String.append => "(^)".
Extract Inductive Coq.Strings.String.string => "string"
       ["""""" "(fun (c, s) -> (String.make 1 c) ^ s)"]
       "(fun fES fS s -> if s="""" then fES () else fS s.[0] (String.sub s 1 (String.length s - 1)))".
(* TODO: stolen from Coq.extraction.ExtrOcamlString, but I don't know how to import only
   some of the `Extract` clauses in that library without importing others, so for now I'm
   copy-pasting
*)
Extract Inductive Coq.Strings.Ascii.ascii => "char"
       [
           "(fun (b0,b1,b2,b3,b4,b5,b6,b7) -> let f b i = if b then 1 lsl i else 0 in Char.chr (f b0 0 + f b1 1 + f b2 2 + f b3 3 + f b4 4 + f b5 5 + f b6 6 + f b7 7))"
       ]
       "(fun f c -> let n = Char.code c in let h i = (n land (1 lsl i)) <> 0 in f (h 0) (h 1) (h 2) (h 3) (h 4) (h 5) (h 6) (h 7))".
Extract Inductive Fuel.t => "unit" [ "()" "()" ] "(fun fMore _ fKicked -> fMore ())".
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

Extraction FCore. *)
