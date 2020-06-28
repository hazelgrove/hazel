let%expect_test "exp_test" = {
  print_endline("peep");

  %expect
  {|peep|};
};
// let%test "test1" = 12 == 12
// let%test "test2" = 2 == 12
