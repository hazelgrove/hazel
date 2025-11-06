open Alcotest;
open Util;

module StringWriter = {
  [@deriving sexp]
  type t = string;
  let empty = "";
  let append = (s1, s2) => s1 ++ s2;
};

module StringWriterMonad = Util.WriterMonad.Make(StringWriter);

let tests = (
  "WriterMonad",
  [
    test_case(
      "return produces empty writer",
      `Quick,
      () => {
        let result = StringWriterMonad.return(42);
        check(
          pair(string, int),
          "return with empty writer",
          ("", 42),
          result,
        );
      },
    ),
    test_case(
      "tell adds to writer",
      `Quick,
      () => {
        let result = StringWriterMonad.tell("hello");
        check(
          pair(string, unit),
          "tell adds message",
          ("hello", ()),
          result,
        );
      },
    ),
    test_case(
      "bind combines writers",
      `Quick,
      () => {
        let computation =
          StringWriterMonad.Syntax.(
            let* () = StringWriterMonad.tell("start ");
            let* () = StringWriterMonad.tell("middle ");
            let* () = StringWriterMonad.tell("end");
            StringWriterMonad.return("done")
          );
        check(
          pair(string, string),
          "bind combines writers",
          ("start middle end", "done"),
          computation,
        );
      },
    ),
    test_case(
      "listen captures writer",
      `Quick,
      () => {
        let computation =
          StringWriterMonad.Syntax.(
            let* () = StringWriterMonad.tell("log1 ");
            let* () = StringWriterMonad.tell("log2");
            StringWriterMonad.return(123)
          );
        let result = StringWriterMonad.listen(computation);
        check(
          pair(string, pair(int, string)),
          "listen captures writer",
          ("log1 log2", (123, "log1 log2")),
          result,
        );
      },
    ),
    test_case(
      "pass modifies writer",
      `Quick,
      () => {
        let computation =
          StringWriterMonad.Syntax.(
            let* () = StringWriterMonad.tell("original");
            StringWriterMonad.return(("result", w => "[" ++ w ++ "]"))
          );
        let result = StringWriterMonad.pass(computation);
        check(
          pair(string, string),
          "pass modifies writer",
          ("[original]", "result"),
          result,
        );
      },
    ),
    test_case(
      "complex computation with let syntax",
      `Quick,
      () => {
        let computation =
          StringWriterMonad.Syntax.(
            let* () = StringWriterMonad.tell("Begin ");
            let* x = StringWriterMonad.return(10);
            let* () =
              StringWriterMonad.tell(
                "Processing " ++ string_of_int(x) ++ " ",
              );
            let* y = StringWriterMonad.return(x * 2);
            let* () =
              StringWriterMonad.tell("Result: " ++ string_of_int(y) ++ " ");
            StringWriterMonad.return(y + 5)
          );
        check(
          pair(string, int),
          "complex computation",
          ("Begin Processing 10 Result: 20 ", 25),
          computation,
        );
      },
    ),
  ],
);
