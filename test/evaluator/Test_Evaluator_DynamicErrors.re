open Alcotest;
open Language;
open Test_Evaluator_Prelude;
open IdTagged.FreshGrammar;
open Exp;

/* A dynamic error (7 / 0, int_mod(7, 0), string_sub out of range) is a
   final hole around the call or operation that failed. Its contents are
   already-computed values, so nothing runs inside it again: evaluating it,
   projecting it out of a structure, or stepping it leaves it as it is. */

let call = (name, args) => ap(Forward, builtin_fun(name), tuple(args));
let mod_by_zero = (name, m, n) =>
  dynamic_error_hole(call(name, [m, n]), DivideByZero);
let int_mod_hole = mod_by_zero("int_mod", int(7), int(0));
let div_hole =
  dynamic_error_hole(bin_op(Int(Divide), int(7), int(0)), DivideByZero);
let (nat7, nat0) = (nat(Bigint.of_int(7)), nat(Bigint.zero));

/* Exact structure, dynamic errors included (not ignored). */
let evaluates_to = (src, expected) =>
  test_case(src, `Quick, () =>
    evaluation_test(src, expected, elaborate(parse_exp(src)))
  );
/* For literals the parser has no syntax for (SInt, Nat). */
let term_evaluates_to = (msg, d, expected) =>
  test_case(msg, `Quick, () => evaluation_test(msg, expected, d));

let evaluation_tests = [
  /* the whole call, under the builtin's own name (it named a builtin
     "mod", which does not exist, and evaluating it raised InvalidBuiltin) */
  evaluates_to("int_mod(7, 0)", int_mod_hole),
  evaluates_to(
    "float_mod(7., 0.)",
    mod_by_zero("float_mod", float(7.), float(0.)),
  ),
  term_evaluates_to(
    "sint_mod(7, 0)",
    call("sint_mod", [sint(7), sint(0)]),
    mod_by_zero("sint_mod", sint(7), sint(0)),
  ),
  term_evaluates_to(
    "nat_mod(7, 0)",
    call("nat_mod", [nat7, nat0]),
    mod_by_zero("nat_mod", nat7, nat0),
  ),
  evaluates_to(
    {|string_sub("abc", 5, 2)|},
    dynamic_error_hole(
      call("string_sub", [string("abc"), int(5), int(2)]),
      IndexOutOfBounds,
    ),
  ),
  /* the hole appears once, not nested */
  evaluates_to(
    "int_mod(7, 0) + 1",
    bin_op(Int(Plus), int_mod_hole, int(1)),
  ),
  evaluates_to(
    "let x = int_mod(7, 0) in (x, x)",
    tuple([int_mod_hole, int_mod_hole]),
  ),
  /* evaluating the hole again (here, after projecting it out) keeps it */
  evaluates_to("[(a = int_mod(7, 0), b = 1)].a", list_lit([int_mod_hole])),
  evaluates_to("[(a = 7 / 0, b = 1)].a", list_lit([div_hole])),
  evaluates_to("fst((7 / 0, 1))", div_hole),
];

/* The stepper takes one step to the hole, and then has nothing to step:
   it used to find the hole's contents steppable and step forever, each
   step adding another error layer. */
let final_after_one_step = (msg, d, expected) =>
  test_case(
    "Stepper: " ++ msg ++ " is final after one step",
    `Quick,
    () => {
      let stepped = single_step(d);
      check(
        option(dhexp_typ),
        msg ++ ": one step",
        Some(expected),
        stepped,
      );
      check(
        option(dhexp_typ),
        msg ++ ": then final",
        None,
        Option.bind(stepped, ~f=single_step),
      );
    },
  );

let stepper_tests = [
  final_after_one_step(
    "int_mod(7, 0)",
    call("int_mod", [int(7), int(0)]),
    int_mod_hole,
  ),
  final_after_one_step(
    "7 / 0",
    bin_op(Int(Divide), int(7), int(0)),
    div_hole,
  ),
];

let tests = ("Evaluator.DynamicErrors", evaluation_tests @ stepper_tests);
