open Tezt;
open Tezt.Base;

// A simple unit test
let () =
  Test.register(
    ~__FILE__,
    ~title="example unit test",
    ~tags=["example", "addition"],
    () => {
      if (1 + 1 != 2) {
        Test.fail("1 + 1 is not 2");
      };
      unit;
    },
  );

// A simple integration test
let () =
  Test.register(
    ~__FILE__,
    ~title="example integration test",
    ~tags=["example", "cat"],
    () => {
      let filename = Temp.file("test.txt");
      with_open_out(filename, ch =>
        output_string(ch, "test" ++ string_of_int(Random.int(1000)))
      );
      let* output = Process.run_and_read_stdout("cat", [filename]);
      if (output =~! rex(~opts=[`Multiline], "^test\\d{1,4}$")) {
        Test.fail("got %S instead of the expected output", output);
      };
      unit;
    },
  );

// A simple regression test
//
// Use "make reset-regression-tests" to record and "make test" to check
let () =
  Regression.register(
    ~__FILE__,
    ~title="example regression test",
    ~tags=["example"],
    ~output_file="example.txt",
    () => {
      let* _output = Process.run(~hooks=Regression.hooks, "git", ["--help"]);
      Regression.capture(string_of_int(1 + 1));
      unit;
    },
  );

let () = Test.run();
