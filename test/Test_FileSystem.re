open Web;
open Alcotest;

let path_eq =
  testable(
    Fmt.of_to_string(path =>
      "["
      ++ String.concat(", ", List.map(s => "\"" ++ s ++ "\"", path))
      ++ "]"
    ),
    (==),
  );

let unwrap_success = (result: FileSystem.Update.result): FileSystem.Model.t =>
  switch (result) {
  | Success(model) => model
  | Failure(msg) => Alcotest.fail("Expected Success, got Failure: " ++ msg)
  };

let tests = [
  (
    "FileSystem",
    [
      test_case(
        "init creates a current file",
        `Quick,
        () => {
          let model = FileSystem.Utils.init();
          let file = FileSystem.Utils.current_file(model);
          switch (file) {
          | Some(f) => check(string, "file name", "main.hz", f.name)
          | None => Alcotest.fail("No current file after init")
          };
        },
      ),
      test_case(
        "rename updates current path",
        `Quick,
        () => {
          let model = FileSystem.Utils.init();
          let result =
            FileSystem.Update.rename(model, ["", "main.hz"], "app");
          let model' = unwrap_success(result);
          check(path_eq, "current path", ["", "app.hz"], model'.current);
        },
      ),
    ],
  ),
];
