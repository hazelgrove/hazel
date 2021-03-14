// To create an expectation test, print to the console,
// then [%expect{|"___"|}] will diff the output of the expected
// output with your test's output.
//
// More at https://github.com/janestreet/ppx_expect
let%expect_test "expect_test" = {
  let eval = code => {
    let as_buf = Lexing.from_string(code);
    let parsed = Toploop.parse_toplevel_phrase^(as_buf);
    ignore(Toploop.execute_phrase(true, Format.std_formatter, parsed));
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
