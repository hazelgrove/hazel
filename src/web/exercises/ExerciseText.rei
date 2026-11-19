/* Exercises-mode code and theorem exercises as .hzt text. The format is
   documented in hazel-programs/exercises/README.md; the section syntax is
   HztText's, shared with the tutorial lessons. */

/* The exercise a .hzt source describes; `rel` is its filename, which names
   it in error messages and gives its module name unless its @flags set
   `module=`. A file with @lemmas or @theorem is a theorem exercise, any
   other a code exercise. Fails on a malformed source (no `id=`, a bad
   number or key, an unreadable @syntax predicate): a bundled exercise is
   committed text, so that is an authoring error, caught by the tests. An
   editor whose text does not parse loads empty, as a tutorial's does. */
let of_text: (~rel: string, string) => Exercise.t;

/* The .hzt source of an exercise. Parsing it back gives the same exercise
   (editors compared as text). */
let code_to_text: CodeExercise.spec => string;
let theorem_to_text: TheoremExercise.spec => string;

/* (filename, source) for "Export Exercise Module"; None for a derivation
   exercise, which has no text form. */
let to_file: Exercise.t => option((string, string));

/* "oddly-recursive.hzt" -> "Ex_OddlyRecursive", and back. */
let module_name_of: string => string;
let filename_of: string => string;
