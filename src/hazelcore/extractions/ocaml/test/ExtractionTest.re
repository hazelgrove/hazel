// To create an expectation test, print to the console,
// then [%expect{|"___"|}] will diff the output of the expected
// output with your test's output.
//
// More at https://github.com/janestreet/ppx_expect

open Js_of_ocaml_toplevel;

let%expect_test "expect_test" = {
  let () = JsooTop.initialize();

  let execute: string => string =
    code => {
      let buffer = Buffer.create(100);
      let formatter = Format.formatter_of_buffer(buffer);
      JsooTop.execute(true, formatter, code);
      let result = Buffer.contents(buffer);
      result;
    };

  let eval: string => unit =
    code => {
      let _result = execute(code);
      ();
    };

  eval("let () = print_endline \"hazel\";;");
  %expect
  {|hazel|};
  // print_endline("hazel");
  // %expect
  // {|hazel|};
};

// To create an inline test, write a function () -> bool
// whose result is the assertion you wish to test.
//
// More at https://github.com/janestreet/ppx_inline_test
let%test "inline_test" = 12 == 12;

// Note: Make these tests fail to see sample output, otherwise
// `make test` will be silent.
