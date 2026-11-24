/* Building Tutorial.spec records from the .hzt lesson sources embedded by
   the tutorialslides library: the files are compiled in as raw text
   (ppx_blob) and parsed here at startup, so editing a lesson means editing
   its .hzt and rebuilding. The marker format is documented in
   hazel-programs/tutorial/README.md; the inverse direction (spec -> text)
   lives in src/CLI/TutorialDecode.re. */

/* Every lesson in Tutorialslides.Slides.all, in that list's order. */
let all: list(Tutorial.spec);

/* The title a lesson falls back to when its .hzt has no `@title`: the
   filename with its number kept and the rest title-cased, and any directory
   segments as the SlidePath folders it sits in.

     title_of("01-holes.hzt")        == "01 - Holes"
     title_of("basics/01-holes.hzt") == "Basics / 01 - Holes"

   A "task" or "extra" token right after the number is called out as its own
   segment: "26-task-grove-name.hzt" == "26 - Task - Grove Name". */
let title_of: string => string;

/* A lesson's sections, as read from its text (`hazel migrate` rewrites
   them in place). */
type sections = {
  title: string,
  prompt: string,
  code: string,
  test: string,
  solution: option(string),
  hint: string,
  reference: option(string),
  hints: list(string),
  wrapper: bool,
  show_report: bool,
  version: int,
  id: option(string),
};

let parse_sections: string => sections;

/* The section a marker line opens; text before any marker is `Code`. */
[@deriving eq]
type marker =
  | Title
  | Prompt
  | Code
  | Test
  | Solution
  | Hint
  | Reference
  | Hints
  | Flags;

let marker_of_line: string => option(marker);
