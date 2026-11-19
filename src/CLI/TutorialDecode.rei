/* The inverse of Web.TutorialText: render the compiled-in lessons back to
   the .hzt text they were loaded from, and check that doing so reproduces
   their source. Both are CLI entry points -- `./hazel tutorial-decode` and
   `./hazel tutorial-verify`; see hazel-programs/tutorial/README.md. */

/* Write every lesson to hazel-programs/tutorial-imported/, or, given a
   substring, print the lessons whose title contains it to stdout instead. */
let decode: option(string) => unit;

/* Print, per lesson, whether its impl and its hidden tests are a fixed point
   of the text round-trip, then a pass/fail summary. Verbose also prints the
   before and after text of each mismatch. */
let verify: bool => unit;
