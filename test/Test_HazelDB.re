open Alcotest;
open Web;

/* HazelDB names its IndexedDB database after the git branch the build came
   from, keeping dev and branchless builds on the original name. */
let name_tests =
  [
    ("dev keeps the original name", "dev", "hazel"),
    ("a build outside a git checkout", "unknown", "hazel"),
    ("a detached HEAD", "HEAD", "hazel"),
    (
      "a feature branch",
      "fix/read-only-streaming-eval",
      "hazel:fix/read-only-streaming-eval",
    ),
    ("a branch named like a default", "main", "hazel:main"),
  ]
  |> List.map(~f=((name, branch, expected)) =>
       test_case(name, `Quick, () =>
         check(string, branch, expected, HazelDB.db_name_for_branch(branch))
       )
     );

let tests = [("HazelDB.db_name_for_branch", name_tests)];
