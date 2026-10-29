/* The Tutorial-mode lesson sources: the .hzt files in
   hazel-programs/tutorial, embedded at compile time (ppx_blob).
   Web.TutorialText parses these into Tutorial.spec records at startup. */

/* Each lesson as (path relative to hazel-programs/tutorial, file contents).
   The order here is the order lessons appear in Tutorial mode, so adding a
   lesson means adding its file and a line to Slides.re. */
let all: list((string, string));
