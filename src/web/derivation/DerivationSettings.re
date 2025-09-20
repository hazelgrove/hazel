let filename = "haz3l-demo";
let log_key = filename;
let exercises: list(DerivationTree.spec) = [
  Ex_DerivationEmpty.exercise("Ⅰ"),
  Ex_DerivationEmpty.exercise("ⅠⅠ"),
  Ex_DerivationEmpty.exercise("ⅠⅠⅠ"),
  Ex_Closed_Substitution.exercise,
  Ex_Curried_Function_Derivation.exercise,
  Ex_PairMap_Derivation.exercise,
  Ex_Shadowing_And_Closures.exercise,
  Ex_Type_Validation_Derivation.exercise,
  T1.exercise,
  T2.exercise,
  T3.exercise,
  T4.exercise,
  T5.exercise,
  T6.exercise,
  T7.exercise,
];
