/**
  This module defines the mapping between `RuleImage.t` and `Rule.t`.

  Because certain rules can vary based on context, this mapping ensures proper
  interpretation. For example, `T_LetAnn` in Recursive ALFA requires an extra
  premise for type validation, while in ALFA it does not.
 */

[@deriving (show({with_path: false}), sexp, yojson, enumerate)]
type t =
  // Type consistency
  | C_Refl
  | C_UnkL
  | C_UnkR
  | C_Sum
  | C_Prod
  | C_Arrow
  // Type Matched
  | MS_Hole
  | MS_Sum
  | MP_Hole
  | MP_Prod
  | MA_Hole
  | MA_Arrow
  // Type Validity
  | TV_Num
  | TV_Bool
  | TV_Unit
  | TV_Arrow
  | TV_Prod
  | TV_Sum
  | TV_Rec
  | TV_TVar
  // Typing
  | A_Subsumption
  | S_Hole
  // - Booleans
  | T_True
  | S_True
  | T_False
  | S_False
  | T_If
  | S_If
  | A_If
  // - Numbers
  | T_Num
  | S_Num
  | T_Neg
  | S_Neg
  | T_Plus
  | S_Plus
  | T_Minus
  | S_Minus
  | T_Times
  | S_Times
  | T_Lt
  | S_Lt
  | T_Gt
  | S_Gt
  | T_Eq
  | S_Eq
  // - Variables
  | T_Var
  | S_Var
  | T_LetAnn
  | S_LetAnn
  | A_LetAnn
  | T_Let
  | S_Let
  | A_Let
  // - Functions
  | T_FunAnn
  | S_FunAnn
  | A_FunAnn
  | T_Fun
  | A_Fun
  | T_Ap
  | S_Ap
  // - Products
  | T_Triv
  | S_Triv
  | T_Pair
  | S_Pair
  | A_Pair
  | T_LetPair
  | S_LetPair
  | A_LetPair
  | T_PrjL
  | S_PrjL
  | T_PrjR
  | S_PrjR
  // - Sums
  | T_InjL
  | A_InjL
  | T_InjR
  | A_InjR
  | T_Case
  | S_Case
  | A_Case
  // - Fixpoints
  | T_Fix
  | T_FixAnn
  // - Recursive
  | T_Roll
  | T_Unroll
  // Values
  | V_Num
  | V_True
  | V_False
  | V_Fun
  | V_Pair
  | V_Triv
  | V_InjL
  | V_InjR
  | V_Roll
  // Evaluation
  // - Value Evaluation
  | E_Val
  // - Booleans
  | E_If_T
  | E_If_F
  // - Numbers
  | E_Num
  | E_Neg
  | E_Plus
  | E_Minus
  | E_Times
  | E_Lt_T
  | E_Lt_F
  | E_Gt_T
  | E_Gt_F
  | E_Eq_T
  | E_Eq_F
  // - Variables
  | E_Let
  | E_Ap
  // - Products
  | E_Pair
  | E_PrjL
  | E_PrjR
  | E_LetPair
  // - Sums
  | E_InjL
  | E_InjR
  | E_Case_L
  | E_Case_R
  // - Fixpoints
  | E_Fix
  // - Recursive
  | E_Roll
  | E_Unroll
  // Propositional logic
  | Assumption
  | And_I
  | And_E_L
  | And_E_R
  | Or_I_L
  | Or_I_R
  | Or_E
  | Implies_I
  | Implies_E
  | Truth_I
  | Falsity_E;

let show = rule => show(rule) |> String.map(c => c == '_' ? '-' : c);

let repr =
  fun
  // // Propositional logic
  | Assumption => "Asm."
  | And_I => "∧-I"
  | And_E_L => "∧-E-L"
  | And_E_R => "∧-E-R"
  | Or_I_L => "∨-I-L"
  | Or_I_R => "∨-I-R"
  | Or_E => "∨-E"
  | Implies_I => "⊃-I"
  | Implies_E => "⊃-E"
  | Truth_I => "⊤-I"
  | Falsity_E => "⊥-E"
  | A_Subsumption => "A-Sub."
  | _ as rule => show(rule);

[@deriving (show({with_path: false}), sexp, yojson, enumerate)]
type corpus =
  | PropositionalLogic
  | AL
  | ALB
  | ALF
  | ALFp
  | ALFA
  | RecursiveALFA
  | GradualALFA;

let corpus_of_string = str =>
  switch (str) {
  | "PropositionalLogic" => PropositionalLogic
  | "AL" => AL
  | "ALB" => ALB
  | "ALF" => ALF
  | "ALFp" => ALFp
  | "ALFA" => ALFA
  | "RecursiveALFA" => RecursiveALFA
  | "GradualALFA" => GradualALFA
  | _ => failwith("Unknown version: " ++ str)
  };

let rec to_rule: (corpus, t) => option(Rule.t) =
  // Note(zhiyao): we extensively use `_` in this function,
  // plz don't rely on the compiler checking the correctness.
  fun
  | PropositionalLogic => (
      fun
      | Assumption => Some(Assumption)
      | And_I => Some(And_I)
      | And_E_L => Some(And_E_L)
      | And_E_R => Some(And_E_R)
      | Or_I_L => Some(Or_I_L)
      | Or_I_R => Some(Or_I_R)
      | Or_E => Some(Or_E)
      | Implies_I => Some(Implies_I)
      | Implies_E => Some(Implies_E)
      | Truth_I => Some(Truth_I)
      | Falsity_E => Some(Falsity_E)
      | _ => None
    )
  | AL => (
      fun
      | V_Num => Some(V_Num)
      | E_Num => Some(E_Num)
      | E_Neg => Some(E_Neg)
      | E_Plus => Some(E_Plus)
      | E_Minus => Some(E_Minus)
      | E_Times => Some(E_Times)
      | _ => None
    )
  | ALB => (
      fun
      | E_Num => None // deprecated
      | V_True => Some(V_True)
      | V_False => Some(V_False)
      | E_Val => Some(E_Val)
      | E_Lt_T => Some(E_Lt_T)
      | E_Lt_F => Some(E_Lt_F)
      | E_Gt_T => Some(E_Gt_T)
      | E_Gt_F => Some(E_Gt_F)
      | E_Eq_T => Some(E_Eq_T)
      | E_Eq_F => Some(E_Eq_F)
      | E_If_T => Some(E_If_T)
      | E_If_F => Some(E_If_F)
      | _ as rule => to_rule(AL, rule)
    )
  | ALF => (
      fun
      | V_Fun => Some(V_Fun)
      | E_Let => Some(E_Let)
      | E_Ap => Some(E_Ap)
      // Note(zhiyao): following typing rules are actually introduced in ALFp
      // but we put them here for convenience.
      | T_Num => Some(T_Num)
      | T_Neg => Some(T_Neg)
      | T_Plus => Some(T_Plus)
      | T_Minus => Some(T_Minus)
      | T_Times => Some(T_Times)
      | T_Lt => Some(T_Lt)
      | T_Gt => Some(T_Gt)
      | T_Eq => Some(T_Eq)
      | T_True => Some(T_True)
      | T_False => Some(T_False)
      | T_If => Some(T_If)
      | T_Var => Some(T_Var)
      | T_LetAnn => Some(T_LetAnn)
      | T_Let => Some(T_Let)
      | T_FunAnn => Some(T_FunAnn)
      | T_Fun => Some(T_Fun)
      | T_Ap => Some(T_Ap)
      | _ as rule => to_rule(ALB, rule)
    )
  | ALFp => (
      fun
      // Products
      | E_Pair => Some(E_Pair)
      | V_Pair => Some(V_Pair)
      | T_Pair => Some(T_Pair)
      | E_PrjL => Some(E_PrjL)
      | E_PrjR => Some(E_PrjR)
      | T_PrjL => Some(T_PrjL)
      | T_PrjR => Some(T_PrjR)
      | E_LetPair => Some(E_LetPair)
      | T_LetPair => Some(T_LetPair)
      | V_Triv => Some(V_Triv)
      | T_Triv => Some(T_Triv)
      // Bidirectional
      | S_Num => Some(S_Num)
      | S_True => Some(S_True)
      | S_False => Some(S_False)
      | A_Subsumption => Some(A_Subsumption)
      | S_Var => Some(S_Var)
      | A_Fun => Some(A_Fun)
      | S_FunAnn => Some(S_FunAnn)
      | A_FunAnn => Some(A_FunAnn)
      | S_Ap => Some(S_Ap)
      | S_Neg => Some(S_Neg)
      | S_Plus => Some(S_Plus)
      | S_Minus => Some(S_Minus)
      | S_Times => Some(S_Times)
      | S_Lt => Some(S_Lt)
      | S_Gt => Some(S_Gt)
      | S_Eq => Some(S_Eq)
      | S_LetAnn => Some(S_LetAnn)
      | A_LetAnn => Some(A_LetAnn)
      | S_Let => Some(S_Let)
      | A_Let => Some(A_Let)
      | S_Pair => Some(S_Pair)
      | A_Pair => Some(A_Pair)
      | S_LetPair => Some(S_LetPair)
      | A_LetPair => Some(A_LetPair)
      | S_PrjL => Some(S_PrjL)
      | S_PrjR => Some(S_PrjR)
      | S_Triv => Some(S_Triv)
      | A_If => Some(A_If)
      | S_If => Some(S_If)
      | _ as rule => to_rule(ALF, rule)
    )
  | ALFA => (
      fun
      | A_InjL => Some(A_InjL)
      | A_InjR => Some(A_InjR)
      | T_InjL => Some(T_InjL) // Note(zhiyao): not in HW
      | T_InjR => Some(T_InjR) // Note(zhiyao): not in HW
      | E_InjL => Some(E_InjL)
      | E_InjR => Some(E_InjR)
      | V_InjL => Some(V_InjL) // Note(zhiyao): not in HW
      | V_InjR => Some(V_InjR) // Note(zhiyao): not in HW
      | A_Case => Some(A_Case)
      | S_Case => Some(S_Case)
      | T_Case => Some(T_Case) // Note(zhiyao): not in HW
      | E_Case_L => Some(E_Case_L)
      | E_Case_R => Some(E_Case_R)
      | _ as rule => to_rule(ALFp, rule)
    )
  | RecursiveALFA => (
      fun
      | E_Fix => Some(E_Fix)
      | T_Fix => Some(T_Fix) // Note(zhiyao): not in HW
      | T_Roll => Some(T_Roll)
      | T_Unroll => Some(T_Unroll)
      | E_Roll => Some(E_Roll)
      | E_Unroll => Some(E_Unroll)
      | TV_Num => Some(TV_Num)
      | TV_Bool => Some(TV_Bool)
      | TV_Unit => Some(TV_Unit)
      | TV_Arrow => Some(TV_Arrow)
      | TV_Prod => Some(TV_Prod)
      | TV_Sum => Some(TV_Sum)
      | TV_Rec => Some(TV_Rec)
      | TV_TVar => Some(TV_TVar)
      | T_LetAnn => Some(T_LetAnn_TV) // Note(zhiyao): replace
      | T_FunAnn => Some(T_FunAnn_TV) // Note(zhiyao): replace
      | T_FixAnn => Some(T_FixAnn_TV) // Note(zhiyao): replace
      | _ as rule => to_rule(ALFA, rule)
    )
  | GradualALFA => (
      fun
      | C_Refl => Some(C_Refl)
      | C_UnkL => Some(C_UnkL)
      | C_UnkR => Some(C_UnkR)
      | C_Sum => Some(C_Sum)
      | C_Prod => Some(C_Prod)
      | C_Arrow => Some(C_Arrow)
      | MS_Hole => Some(MS_Hole)
      | MS_Sum => Some(MS_Sum)
      | MP_Hole => Some(MP_Hole)
      | MP_Prod => Some(MP_Prod)
      | MA_Hole => Some(MA_Hole)
      | MA_Arrow => Some(MA_Arrow)
      | S_Hole => Some(S_Hole)
      // Gradual Typing Rules Replace
      | A_Subsumption => Some(A_Subsumption_GT)
      | S_If => Some(S_If_GT)
      | S_PrjL => Some(S_PrjL_GT)
      | S_PrjR => Some(S_PrjR_GT)
      | A_Pair => Some(A_Pair_GT)
      | S_LetPair => Some(S_LetPair_GT)
      | A_LetPair => Some(A_LetPair_GT)
      | A_InjL => Some(A_InjL_GT)
      | A_InjR => Some(A_InjR_GT)
      | A_Case => Some(A_Case_GT)
      | S_Case => Some(S_Case_GT)
      | A_FunAnn => Some(A_FunAnn_GT)
      | A_Fun => Some(A_Fun_GT)
      | S_Ap => Some(S_Ap_GT)
      | _ as rule => to_rule(RecursiveALFA, rule)
    );

let all_rules_of_version: corpus => list(t) =
  version =>
    all |> List.filter(rule => to_rule(version, rule) |> Option.is_some);

[@deriving (show({with_path: false}), sexp, yojson)]
type kind =
  | TypeConsistency
  | TypeMatched
  | TypeValidity
  | Synthesis
  | Analysis
  | Typing
  | Values
  | Evaluation
  | PropositionalLogic;

let of_kind: t => kind =
  fun
  // - Type consistency
  | C_Refl
  | C_UnkL
  | C_UnkR
  | C_Sum
  | C_Prod
  | C_Arrow => TypeConsistency
  | MS_Hole
  | MS_Sum
  | MP_Hole
  | MP_Prod
  | MA_Hole
  | MA_Arrow => TypeMatched
  | TV_Num
  | TV_Bool
  | TV_Unit
  | TV_Arrow
  | TV_Prod
  | TV_Sum
  | TV_Rec
  | TV_TVar => TypeValidity
  | S_Hole
  | S_True
  | S_False
  | S_If
  | S_Num
  | S_Neg
  | S_Plus
  | S_Minus
  | S_Times
  | S_Lt
  | S_Gt
  | S_Eq
  | S_Var
  | S_LetAnn
  | S_Let
  | S_FunAnn
  | S_Ap
  | S_Triv
  | S_Pair
  | S_LetPair
  | S_PrjL
  | S_PrjR
  | S_Case => Synthesis
  | A_Subsumption
  | A_If
  | A_LetAnn
  | A_Let
  | A_FunAnn
  | A_Fun
  | A_Pair
  | A_LetPair
  | A_InjL
  | A_InjR
  | A_Case => Analysis
  | T_True
  | T_False
  | T_If
  | T_Num
  | T_Neg
  | T_Plus
  | T_Minus
  | T_Times
  | T_Lt
  | T_Gt
  | T_Eq
  | T_Var
  | T_LetAnn
  | T_Let
  | T_FunAnn
  | T_Fun
  | T_Ap
  | T_Triv
  | T_Pair
  | T_LetPair
  | T_PrjL
  | T_PrjR
  | T_InjL
  | T_InjR
  | T_Case
  | T_Fix
  | T_FixAnn
  | T_Roll
  | T_Unroll => Typing
  | V_Num
  | V_True
  | V_False
  | V_Fun
  | V_Pair
  | V_Triv
  | V_InjL
  | V_InjR
  | V_Roll => Values
  | E_Val
  | E_If_T
  | E_If_F
  | E_Num
  | E_Neg
  | E_Plus
  | E_Minus
  | E_Times
  | E_Lt_T
  | E_Lt_F
  | E_Gt_T
  | E_Gt_F
  | E_Eq_T
  | E_Eq_F
  | E_Let
  | E_Ap
  | E_Pair
  | E_PrjL
  | E_PrjR
  | E_LetPair
  | E_InjL
  | E_InjR
  | E_Case_L
  | E_Case_R
  | E_Fix
  | E_Roll
  | E_Unroll => Evaluation
  | Assumption
  | And_I
  | Or_I_L
  | Or_I_R
  | Implies_I
  | Truth_I
  | And_E_L
  | And_E_R
  | Or_E
  | Implies_E
  | Falsity_E => PropositionalLogic;

[@deriving (show({with_path: false}), sexp, yojson)]
type sort =
  | TypeConsistency
  | TypeMatched
  | TypeValidity
  | S_Hole
  | A_Subsumption
  | E_Val
  | Boolean
  | Number
  | Variable
  | Function
  | Product
  | Sum
  | Fixpoint
  | Recursive
  | PropositionalLogic;

let of_sort: t => sort =
  fun
  | C_Refl
  | C_UnkL
  | C_UnkR
  | C_Sum
  | C_Prod
  | C_Arrow => TypeConsistency
  | MS_Hole
  | MS_Sum
  | MP_Hole
  | MP_Prod
  | MA_Hole
  | MA_Arrow => TypeMatched
  | TV_Num
  | TV_Bool
  | TV_Unit
  | TV_Arrow
  | TV_Prod
  | TV_Sum
  | TV_Rec
  | TV_TVar => TypeValidity
  | T_True
  | S_True
  | V_True
  | T_False
  | S_False
  | V_False
  | T_If
  | S_If
  | A_If
  | E_If_T
  | E_If_F => Boolean
  | T_Num
  | S_Num
  | E_Num
  | V_Num
  | T_Neg
  | S_Neg
  | E_Neg
  | T_Plus
  | S_Plus
  | E_Plus
  | T_Minus
  | S_Minus
  | E_Minus
  | T_Times
  | S_Times
  | E_Times
  | T_Lt
  | S_Lt
  | E_Lt_T
  | E_Lt_F
  | T_Gt
  | S_Gt
  | E_Gt_T
  | E_Gt_F
  | T_Eq
  | S_Eq
  | E_Eq_T
  | E_Eq_F => Number
  | T_Var
  | S_Var
  | T_LetAnn
  | S_LetAnn
  | A_LetAnn
  | T_Let
  | S_Let
  | A_Let
  | E_Let => Variable
  | T_FunAnn
  | S_FunAnn
  | A_FunAnn
  | T_Fun
  | A_Fun
  | V_Fun
  | T_Ap
  | S_Ap
  | E_Ap => Function
  | T_Triv
  | S_Triv
  | V_Triv
  | T_Pair
  | S_Pair
  | A_Pair
  | V_Pair
  | E_Pair
  | T_LetPair
  | S_LetPair
  | A_LetPair
  | E_LetPair
  | T_PrjL
  | S_PrjL
  | E_PrjL
  | T_PrjR
  | S_PrjR
  | E_PrjR => Product
  | T_InjL
  | A_InjL
  | V_InjL
  | E_InjL
  | T_InjR
  | A_InjR
  | V_InjR
  | E_InjR
  | T_Case
  | S_Case
  | A_Case
  | E_Case_L
  | E_Case_R => Sum
  | T_FixAnn
  | T_Fix
  | E_Fix => Fixpoint
  | T_Roll
  | V_Roll
  | E_Roll
  | T_Unroll
  | E_Unroll => Recursive
  | S_Hole => S_Hole
  | A_Subsumption => A_Subsumption
  | E_Val => E_Val
  | Assumption
  | And_I
  | And_E_L
  | And_E_R
  | Or_I_L
  | Or_I_R
  | Or_E
  | Implies_I
  | Implies_E
  | Truth_I
  | Falsity_E => PropositionalLogic;

// Note(zhiyao): The keywords are used for facilitating the
// searching in NinjaKeys.
let keywords: t => list(string) =
  rule =>
    (show(rule) |> String.split_on_char('_'))
    @ (
      switch (of_kind(rule)) {
      | TypeConsistency => ["Type", "Consistency", "consist", "tc", "cons"]
      | TypeMatched => ["Type", "Matched", "match", "tm", "mat"]
      | TypeValidity => ["Type", "Validity", "typ", "tv", "val"]
      | Synthesis => ["Synthesis", "syn"]
      | Analysis => ["Analysis", "ana"]
      | Typing => ["Type", "Typing", "typ"]
      | Values => ["Values", "val"]
      | Evaluation => ["Evaluation", "eval"]
      | PropositionalLogic => ["Propositional", "Logic", "prop"]
      }
    );

let to_image: Rule.t => t =
  fun
  // Type consistency
  | C_Refl => C_Refl
  | C_UnkL => C_UnkL
  | C_UnkR => C_UnkR
  | C_Sum => C_Sum
  | C_Prod => C_Prod
  | C_Arrow => C_Arrow
  // Type Matched
  | MS_Hole => MS_Hole
  | MS_Sum => MS_Sum
  | MP_Hole => MP_Hole
  | MP_Prod => MP_Prod
  | MA_Hole => MA_Hole
  | MA_Arrow => MA_Arrow
  // Type Validity
  | TV_Num => TV_Num
  | TV_Bool => TV_Bool
  | TV_Unit => TV_Unit
  | TV_Arrow => TV_Arrow
  | TV_Prod => TV_Prod
  | TV_Sum => TV_Sum
  | TV_Rec => TV_Rec
  | TV_TVar => TV_TVar
  // Typing
  | A_Subsumption_GT
  | A_Subsumption => A_Subsumption
  | S_Hole => S_Hole
  // - Booleans
  | T_True => T_True
  | S_True => S_True
  | T_False => T_False
  | S_False => S_False
  | T_If => T_If
  | S_If_GT
  | S_If => S_If
  | A_If => A_If
  // - Numbers
  | T_Num => T_Num
  | S_Num => S_Num
  | T_Neg => T_Neg
  | S_Neg => S_Neg
  | T_Plus => T_Plus
  | S_Plus => S_Plus
  | T_Minus => T_Minus
  | S_Minus => S_Minus
  | T_Times => T_Times
  | S_Times => S_Times
  | T_Lt => T_Lt
  | S_Lt => S_Lt
  | T_Gt => T_Gt
  | S_Gt => S_Gt
  | T_Eq => T_Eq
  | S_Eq => S_Eq
  // - Variables
  | T_Var => T_Var
  | S_Var => S_Var
  | T_LetAnn_TV
  | T_LetAnn => T_LetAnn
  | S_LetAnn => S_LetAnn
  | A_LetAnn => A_LetAnn
  | T_Let => T_Let
  | S_Let => S_Let
  | A_Let => A_Let
  // - Functions
  | T_FunAnn_TV
  | T_FunAnn => T_FunAnn
  | S_FunAnn => S_FunAnn
  | A_FunAnn_GT
  | A_FunAnn => A_FunAnn
  | T_Fun => T_Fun
  | A_Fun_GT
  | A_Fun => A_Fun
  | T_Ap => T_Ap
  | S_Ap_GT
  | S_Ap => S_Ap
  // - Products
  | T_Triv => T_Triv
  | S_Triv => S_Triv
  | T_Pair => T_Pair
  | S_Pair => S_Pair
  | A_Pair_GT
  | A_Pair => A_Pair
  | T_LetPair => T_LetPair
  | S_LetPair_GT
  | S_LetPair => S_LetPair
  | A_LetPair_GT
  | A_LetPair => A_LetPair
  | T_PrjL => T_PrjL
  | S_PrjL_GT
  | S_PrjL => S_PrjL
  | T_PrjR => T_PrjR
  | S_PrjR_GT
  | S_PrjR => S_PrjR
  // - Sums
  | T_InjL => T_InjL
  | A_InjL_GT
  | A_InjL => A_InjL
  | T_InjR => T_InjR
  | A_InjR_GT
  | A_InjR => A_InjR
  | T_Case => T_Case
  | S_Case_GT
  | S_Case => S_Case
  | A_Case_GT
  | A_Case => A_Case
  // - Fixpoints
  | T_Fix => T_Fix
  | T_FixAnn_TV
  | T_FixAnn => T_FixAnn
  // - Recursive
  | T_Roll => T_Roll
  | T_Unroll => T_Unroll
  // Values
  | V_Num => V_Num
  | V_True => V_True
  | V_False => V_False
  | V_Fun => V_Fun
  | V_Pair => V_Pair
  | V_Triv => V_Triv
  | V_InjL => V_InjL
  | V_InjR => V_InjR
  | V_Roll => V_Roll
  // Evaluation
  // - Value Evaluation
  | E_Val => E_Val
  // - Booleans
  | E_If_T => E_If_T
  | E_If_F => E_If_F
  // - Numbers
  | E_Num => E_Num
  | E_Neg => E_Neg
  | E_Plus => E_Plus
  | E_Minus => E_Minus
  | E_Times => E_Times
  | E_Lt_T => E_Lt_T
  | E_Lt_F => E_Lt_F
  | E_Gt_T => E_Gt_T
  | E_Gt_F => E_Gt_F
  | E_Eq_T => E_Eq_T
  | E_Eq_F => E_Eq_F
  // - Variables
  | E_Let => E_Let
  | E_Ap => E_Ap
  // - Products
  | E_Pair => E_Pair
  | E_PrjL => E_PrjL
  | E_PrjR => E_PrjR
  | E_LetPair => E_LetPair
  // - Sums
  | E_InjL => E_InjL
  | E_InjR => E_InjR
  | E_Case_L => E_Case_L
  | E_Case_R => E_Case_R
  // - Fixpoints
  | E_Fix => E_Fix
  // - Recursive
  | E_Roll => E_Roll
  | E_Unroll => E_Unroll
  // Propositional logic
  | Assumption => Assumption
  | And_I => And_I
  | And_E_L => And_E_L
  | And_E_R => And_E_R
  | Or_I_L => Or_I_L
  | Or_I_R => Or_I_R
  | Or_E => Or_E
  | Implies_I => Implies_I
  | Implies_E => Implies_E
  | Truth_I => Truth_I
  | Falsity_E => Falsity_E;
