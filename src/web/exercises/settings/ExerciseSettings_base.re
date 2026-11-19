let filename = "haz3l-demo";
let log_key = filename;

/* Code and theorem exercises are the .hzt text in hazel-programs/exercises/
   (format in its README.md), copied next to src/web/dune and embedded here
   as (filename, contents); ExerciseText parses them at startup. Adding an
   exercise means adding its file and a line here. */
let sources: list((string, string)) = [
  ("oddly-recursive.hzt", [%blob "../../oddly-recursive.hzt"]),
  ("recursive-fibonacci.hzt", [%blob "../../recursive-fibonacci.hzt"]),
  ("reverse-reverse.hzt", [%blob "../../reverse-reverse.hzt"]),
];

/* Exercise order is this list's order. Derivation exercises have no text
   form yet and stay OCaml modules. */
let exercises: list(Exercise.t) =
  List.map(((rel, text)) => ExerciseText.of_text(~rel, text), sources)
  @ [Ex_EvaluationDerivation.exercise];
