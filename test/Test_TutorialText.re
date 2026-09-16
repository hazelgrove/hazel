open Alcotest;
open Web;

/* TutorialText.title_of derives a lesson's title from its filename when the
   .hzt has no @title. Every lesson currently sets one, so this is the path
   that only runs for a new lesson that omits it -- notably one filed in a
   subdirectory, which is how a lesson picks up a folder without an explicit
   title. */

let title_of_tests =
  [
    ("a numbered lesson", "01-holes.hzt", "01 - Holes"),
    (
      "several words",
      "02-the-tylr-parser-and-backpack.hzt",
      "02 - The Tylr Parser And Backpack",
    ),
    /* A "task"/"extra" token right after the number is called out. */
    ("a category token", "26-task-grove-name.hzt", "26 - Task - Grove Name"),
    ("no leading number", "holes.hzt", "Holes"),
    /* Directory segments become the folders of the resulting SlidePath. */
    ("a subdirectory", "basics/01-holes.hzt", "Basics / 01 - Holes"),
    (
      "nested subdirectories",
      "table-study/tasks/03-tidy-term.hzt",
      "Table Study / Tasks / 03 - Tidy Term",
    ),
    /* An empty segment must not become an empty folder. */
    ("a doubled separator", "basics//01-holes.hzt", "Basics / 01 - Holes"),
  ]
  |> List.map(((name, rel, expected)) =>
       test_case(name, `Quick, () =>
         check(string, rel, expected, TutorialText.title_of(rel))
       )
     );

let tests = [("TutorialText.title_of", title_of_tests)];
