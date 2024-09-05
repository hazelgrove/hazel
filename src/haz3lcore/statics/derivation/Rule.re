[@deriving (show({with_path: false}), sexp, yojson)]
type t =
  | A_Subsumption
  // Type Validity
  // | TV_Num
  // | TV_Bool
  // | TV_Unit
  // | TV_Arrow
  // | TV_Prod
  // | TV_Sum
  // | TV_Rec
  // | TV_TVar
  // Typing
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
  // | T_LetAnn_TV
  | S_LetAnn
  | A_LetAnn
  | T_Let
  | S_Let
  | A_Let
  // - Functions
  | T_FunAnn
  // | T_FunAnn_TV
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
  // | T_FixAnn_TV
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

let repr =
  fun
  // Propositional logic
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
  | A_Subsumption => "A-Sub"
  | rule => show(rule) |> String.map(c => c == '_' ? '-' : c);

let prems_num =
  fun
  | A_Subsumption => 1
  | E_Val => 1
  | S_Num
  | T_Num => 0
  | V_Num => 0
  | S_True
  | T_True => 0
  | V_True => 0
  | S_False
  | T_False => 0
  | V_False => 0
  | S_Triv
  | T_Triv => 0
  | V_Triv => 0
  | S_Neg
  | T_Neg => 1
  | E_Neg => 1
  | S_Plus
  | T_Plus => 2
  | E_Plus => 2
  | S_Minus
  | T_Minus => 2
  | E_Minus => 2
  | S_Times
  | T_Times => 2
  | E_Times => 2
  | S_Lt
  | T_Lt => 2
  | E_Lt_T
  | E_Lt_F => 2
  | S_Gt
  | T_Gt => 2
  | E_Gt_T
  | E_Gt_F => 2
  | S_Eq
  | T_Eq => 2
  | E_Eq_T
  | E_Eq_F => 2
  | A_If
  | S_If
  | T_If => 3
  | E_If_T
  | E_If_F => 3
  | S_Var
  | T_Var => 0
  | S_LetAnn
  | A_LetAnn
  | T_LetAnn => 2
  | S_Let
  | A_Let
  | T_Let => 2
  | E_Let => 2
  | S_FunAnn
  | A_FunAnn
  | T_FunAnn => 1
  | A_Fun
  | T_Fun => 1
  | V_Fun => 0
  | T_Fix => 1
  | T_FixAnn => 1
  | E_Fix => 1
  | S_Ap
  | T_Ap => 2
  | E_Ap => 3
  | S_Pair
  | A_Pair
  | T_Pair => 2
  | E_Pair => 2
  | V_Pair => 2
  | S_LetPair
  | A_LetPair
  | T_LetPair => 2
  | E_LetPair => 2
  | S_PrjL
  | T_PrjL => 1
  | E_PrjL => 1
  | S_PrjR
  | T_PrjR => 1
  | E_PrjR => 1
  | A_InjL
  | T_InjL => 1
  | E_InjL => 1
  | V_InjL => 1
  | A_InjR
  | T_InjR => 1
  | E_InjR => 1
  | V_InjR => 1
  | A_Case
  | S_Case
  | T_Case => 3
  | E_Case_L
  | E_Case_R => 2
  | T_Roll => 1
  | E_Roll => 1
  | V_Roll => 1
  | T_Unroll => 1
  | E_Unroll => 1
  // Propositional logic
  | Assumption => 0
  | And_I => 2
  | And_E_L => 1
  | And_E_R => 1
  | Or_I_L => 1
  | Or_I_R => 1
  | Or_E => 3
  | Implies_I => 1
  | Implies_E => 2
  | Truth_I => 0
  | Falsity_E => 1;

[@deriving (show({with_path: false}), sexp, yojson)]
type kind =
  | TypeValidity
  | Synthesis
  | Analysis
  | Typing
  | Values
  | Evaluation
  | PropositionalLogic;

let of_kind: t => kind =
  fun
  // | TV_Num
  // | TV_Bool
  // | TV_Unit
  // | TV_Arrow
  // | TV_Prod
  // | TV_Sum
  // | TV_Rec
  // | TV_TVar => TypeValidity
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
  // | T_LetAnn_TV
  | T_Let
  | T_FunAnn
  // | T_FunAnn_TV
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
  // | T_FixAnn_TV
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
  | And_E_L
  | And_E_R
  | Or_I_L
  | Or_I_R
  | Or_E
  | Implies_I
  | Implies_E
  | Truth_I
  | Falsity_E => PropositionalLogic;

[@deriving (show({with_path: false}), sexp, yojson)]
type sort =
  | TypeValidity
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
  // | TV_Num
  // | TV_Bool
  // | TV_Unit
  // | TV_Arrow
  // | TV_Prod
  // | TV_Sum
  // | TV_Rec
  // | TV_TVar => TypeValidity
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
  // | T_LetAnn_TV
  | S_LetAnn
  | A_LetAnn
  | T_Let
  | S_Let
  | A_Let
  | E_Let => Variable
  | T_FunAnn
  // | T_FunAnn_TV
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
  // | T_FixAnn_TV
  | T_Fix
  | E_Fix => Fixpoint
  | T_Roll
  | V_Roll
  | E_Roll
  | T_Unroll
  | E_Unroll => Recursive
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

[@deriving (show({with_path: false}), sexp, yojson)]
type version =
  | Logic
  | AL
  | ALB
  | ALF
  | ALFp
  | ALFpBD
  | ALFA
  | ALFArec
  | ALFArecTV;

let of_version: t => version =
  fun
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
  | Falsity_E => Logic
  | V_Num
  | E_Neg
  | E_Plus
  | E_Minus
  | E_Times => AL
  | V_True
  | V_False
  | E_Val
  | E_Lt_T
  | E_Lt_F
  | E_Gt_T
  | E_Gt_F
  | E_Eq_T
  | E_Eq_F
  | E_If_T
  | E_If_F => ALB
  | V_Fun
  | E_Let
  | E_Ap
  | T_Var
  | T_Let
  | T_LetAnn
  | T_Num
  | T_True
  | T_False
  | T_Neg
  | T_Plus
  | T_Minus
  | T_Times
  | T_Lt
  | T_Gt
  | T_Eq
  | T_If
  | T_Fun
  | T_FunAnn
  | T_Ap => ALF
  | E_Pair
  | V_Pair
  | T_Pair
  | E_PrjL
  | E_PrjR
  | T_PrjL
  | T_PrjR
  | E_LetPair
  | T_LetPair
  | V_Triv
  | T_Triv => ALFp
  | S_Num
  | S_True
  | S_False
  | A_Subsumption
  | S_Var
  | A_Fun
  | S_FunAnn
  | A_FunAnn
  | S_Ap
  | S_Neg
  | S_Plus
  | S_Minus
  | S_Times
  | S_Lt
  | S_Gt
  | S_Eq
  | S_LetAnn
  | A_LetAnn
  | S_Let
  | A_Let
  | S_Pair
  | A_Pair
  | S_LetPair
  | A_LetPair
  | S_PrjL
  | S_PrjR
  | S_Triv
  | A_If
  | S_If => ALFpBD
  | A_InjL
  | A_InjR
  | T_InjL
  | T_InjR
  | E_InjL
  | E_InjR
  | V_InjL
  | V_InjR
  | A_Case
  | S_Case
  | T_Case
  | E_Case_L
  | E_Case_R => ALFA
  | E_Fix
  | T_Fix
  | T_FixAnn
  | T_Roll
  | V_Roll
  | T_Unroll
  | E_Roll
  | E_Unroll => ALFArec;

let all: list(t) = [
  A_Subsumption,
  // Type Validity
  // | TV_Num
  // | TV_Bool
  // | TV_Unit
  // | TV_Arrow
  // | TV_Prod
  // | TV_Sum
  // | TV_Rec
  // | TV_TVar
  // Typing
  // - Booleans
  T_True,
  S_True,
  T_False,
  S_False,
  T_If,
  S_If,
  A_If,
  // - Numbers
  T_Num,
  S_Num,
  T_Neg,
  S_Neg,
  T_Plus,
  S_Plus,
  T_Minus,
  S_Minus,
  T_Times,
  S_Times,
  T_Lt,
  S_Lt,
  T_Gt,
  S_Gt,
  T_Eq,
  S_Eq,
  // - Variables
  T_Var,
  S_Var,
  T_LetAnn,
  // , T_LetAnn_TV
  S_LetAnn,
  A_LetAnn,
  T_Let,
  S_Let,
  A_Let,
  // - Functions
  T_FunAnn,
  // , T_FunAnn_TV
  S_FunAnn,
  A_FunAnn,
  T_Fun,
  A_Fun,
  T_Ap,
  S_Ap,
  // - Products
  T_Triv,
  S_Triv,
  T_Pair,
  S_Pair,
  A_Pair,
  T_LetPair,
  S_LetPair,
  A_LetPair,
  T_PrjL,
  S_PrjL,
  T_PrjR,
  S_PrjR,
  // - Sums
  T_InjL,
  A_InjL,
  T_InjR,
  A_InjR,
  T_Case,
  S_Case,
  A_Case,
  // - Fixpoints
  T_Fix,
  T_FixAnn,
  // , T_FixAnn_TV
  // - Recursive
  T_Roll,
  T_Unroll,
  // Values
  V_Num,
  V_True,
  V_False,
  V_Fun,
  V_Pair,
  V_Triv,
  V_InjL,
  V_InjR,
  V_Roll,
  // Evaluation
  // - Value Evaluation
  E_Val,
  // - Booleans
  E_If_T,
  E_If_F,
  // - Numbers
  E_Neg,
  E_Plus,
  E_Minus,
  E_Times,
  E_Lt_T,
  E_Lt_F,
  E_Gt_T,
  E_Gt_F,
  E_Eq_T,
  E_Eq_F,
  // - Variables
  E_Let,
  E_Ap,
  // - Products
  E_Pair,
  E_PrjL,
  E_PrjR,
  E_LetPair,
  // - Sums
  E_InjL,
  E_InjR,
  E_Case_L,
  E_Case_R,
  // - Fixpoints
  E_Fix,
  // - Recursive
  E_Roll,
  E_Unroll,
  // Propositional logic
  Assumption,
  And_I,
  And_E_L,
  And_E_R,
  Or_I_L,
  Or_I_R,
  Or_E,
  Implies_I,
  Implies_E,
  Truth_I,
  Falsity_E,
];
