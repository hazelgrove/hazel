let filename = "haz3l-demo";
let log_key = filename;

/* The tutorial sequence is generated from text files in
   hazel-programs/tutorial/ via `./hazel gen-tutorial`. */
let lessons: list(Tutorial.spec) = TutorialGenerated.all;
