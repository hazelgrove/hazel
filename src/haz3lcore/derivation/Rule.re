/**
  This module contains the definition of the derivation rules.
 */

[@deriving (show({with_path: false}), sexp, yojson, enumerate)]
type t =
  // Gradual Typing
  // - Type consistency
  | C_Refl
  | C_UnkL
  | C_UnkR
  | C_Sum
  | C_Prod
  | C_Arrow
  // - Type Matched
  | MS_Hole
  | MS_Sum
  | MP_Hole
  | MP_Prod
  | MA_Hole
  | MA_Arrow
  // - Gradual Typing Rules
  | S_Hole
  | A_Subsumption_GT
  | S_If_GT
  | S_PrjL_GT
  | S_PrjR_GT
  | A_Pair_GT
  | S_LetPair_GT
  | A_LetPair_GT
  | A_InjL_GT
  | A_InjR_GT
  | A_Case_GT
  | S_Case_GT
  | A_FunAnn_GT
  | A_Fun_GT
  | S_Ap_GT
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
  | T_LetAnn_TV
  | S_LetAnn
  | A_LetAnn
  | T_Let
  | S_Let
  | A_Let
  // - Functions
  | T_FunAnn
  | T_FunAnn_TV
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
  | T_FixAnn_TV
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
  | E_Num // only used in AL, predicated in later versions
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

// Deriving by @enumerate
// let all = [...]
