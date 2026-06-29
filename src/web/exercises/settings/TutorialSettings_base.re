let filename = "haz3l-demo";
let log_key = filename;

/* The tutorial sequence is now generated from text files in
   hazel-programs/tutorial/ (basics/ then probes/) via `./hazel gen-tutorial`.
   The hand-written Tu_*.ml lessons remain in examples/ for reference but are
   no longer wired in. To restore one, add it back to this list. */
let lessons: list(Tutorial.spec) = TutorialGenerated.all;
