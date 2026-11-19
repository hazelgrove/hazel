let filename = "haz3l-demo";
let log_key = filename;

/* The tutorial sequence is the .hzt text in hazel-programs/tutorial/,
   embedded at compile time and parsed at startup (see TutorialText). */
let lessons: list(Tutorial.spec) = TutorialText.all;
