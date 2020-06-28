// To create an expectation test, print to the console,
// then [%expect{|"___"|}] will diff the output of the expected
// output with your test's output.
let%expect_test "expect_test" = {
  print_endline("hazel");
  %expect
  {|"lezah"|};
};

// To create an inline test, write a function () -> bool
// who's result is the assertion you wish to test.
let%test "inline_test" = 12 == 12;
