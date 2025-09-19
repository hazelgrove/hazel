let filename = "haz3l-demo";
let log_key = filename;
let exercises: list(Exercise.exercise_spec) = [
  Implementation(Ex_OddlyRecursive.exercise),
  Implementation(Ex_RecursiveFibonacci.exercise),
  Implementation(BlankExercise.exercise),
  Theorem(TheoremTemplate.exercise),
  Derivation(Ex_Curried_Function_Derivation.exercise),
  Derivation(T1.exercise),
  Derivation(Ex_DerivationEmpty.exercise("$")),
];
