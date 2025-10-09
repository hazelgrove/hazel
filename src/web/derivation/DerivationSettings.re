let filename = "haz3l-demo";
let log_key = filename;
let exercises: list(DerivationTree.spec) = [
  Ex_Closed_Substitution.exercise,
  Ex_Curried_Function_Derivation.exercise,
  Ex_PairMap_Derivation.exercise,
  Ex_Shadowing_And_Closures.exercise,
  Ex_Type_Validation_Derivation.exercise,
  Ex_DerivationEmpty.exercise("Ⅰ"),
  Ex_DerivationEmpty.exercise("ⅠⅠ"),
  Ex_DerivationEmpty.exercise("ⅠⅠⅠ"),
];
