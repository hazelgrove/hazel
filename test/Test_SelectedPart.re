open Alcotest;
open Haz3lcore;

/* What a selection names, and what an editor that expands its selection
 * draws for it. In a chain of an associative operator the selection
 * snaps to the run of operands it touches; anywhere else it expands to
 * the smallest node holding it. */

let parts = (z: Zipper.t) => {
  let term = MakeTerm.from_zip_for_sem(z, ~root=Exp).term;
  let statics =
    CachedStatics.init_from_term(
      ~settings=Test_Editing.default_settings,
      ~is_dynamic_term=true,
      term,
    );
  let syntax =
    CachedSyntax.mk(z, ~info_map=statics.info_map, ~dyn_map=Id.Map.empty);
  let measured = CachedSyntax.measured(syntax);
  (
    term,
    SelectedPart.of_zipper(~measured, ~term_data=syntax.term_data, ~term, z),
    SelectedPart.expanded(~measured, ~term_data=syntax.term_data, ~term, z),
  );
};

let print_segment =
  Printer.of_segment(~holes="?", ~concave_holes="~", ~indent=" ");

let print_exp = (e: Language.Exp.t): string =>
  ExpToSegment.exp_to_segment(
    ~settings=ExpToSegment.Settings.editable(~inline=true),
    e,
  )
  |> print_segment;

let selecting = (input: string) => parts(Test_Editing.mk_zipper(input));

let after = (input: string, actions: list(Action.t)) =>
  parts(
    Test_Editing.perform(Zipper.init(), Test_Editing.mk(input) @ actions),
  );

/* A drag from one column of the first row to another. */
let drag = (from: int, to_: int): list(Action.t) => [
  Move(
    Point(
      {
        row: 0,
        col: from,
      },
      None,
    ),
  ),
  Select(
    Resize(
      Point(
        {
          row: 0,
          col: to_,
        },
        None,
      ),
    ),
  ),
];

/* The pieces drawn. */
let drawn = ((_, _, expanded)) =>
  Option.value_map(~default="", ~f=print_segment, expanded);

/* The expression the part's steps start from. */
let named = ((_, part, _)) =>
  Option.value_map(
    ~default="",
    ~f=part => print_exp(Language.Occurrence.exp(part)),
    part,
  );

let is_run = ((_, part, _)) =>
  switch (part) {
  | Some(Language.Occurrence.Run(_)) => true
  | Some(Node(_))
  | None => false
  };

/* The term with the part replaced by `with_input`. */
let replaced = (~with_input, (term, part, _)) => {
  let with_exp =
    MakeTerm.from_zip_for_sem(Test_Editing.mk_zipper(with_input), ~root=Exp).
      term;
  Option.value_map(
    ~default="",
    ~f=part => print_exp(Language.Occurrence.replace(part, term, with_exp)),
    part,
  );
};

let case = (name, expected, actual, parts) =>
  test_case(name, `Quick, () =>
    check(string, name, expected, actual(Lazy.force(parts)))
  );

let run = (name, expected, parts) =>
  test_case(name, `Quick, () =>
    check(bool, name, expected, is_run(Lazy.force(parts)))
  );

let tests = (
  "SelectedPart",
  [
    case(
      "an operator snaps to the operands it joins, past a nested product",
      {|4 * 5 * 6 + "abc"|},
      drawn,
      lazy(selecting({|2 * 3 + 4 * 5 * 6 §+ "abc"¦|})),
    ),
    case(
      "a run in the middle of a sum is drawn alone",
      {|3 + 4|},
      drawn,
      lazy(selecting({|1 + 2 + §3 + 4¦ + 5|})),
    ),
    case(
      "an operand and the operator after it snap to the run",
      {|3 + 4|},
      drawn,
      lazy(selecting({|1 + 2 + §3 +¦ 4 + 5|})),
    ),
    case(
      "an operator and the operand after it snap to the run",
      {|3 + 4|},
      drawn,
      lazy(selecting({|1 + 2 + 3 §+ 4¦ + 5|})),
    ),
    case(
      "a drag over a run is drawn alone",
      {|3 + 4|},
      drawn,
      lazy(after({|¦1 + 2 + 3 + 4 + 5|}, drag(8, 13))),
    ),
    case(
      "a drag from an operator snaps to the operands it joins",
      {|3 + 4|},
      drawn,
      lazy(after({|¦1 + 2 + 3 + 4 + 5|}, drag(10, 13))),
    ),
    case(
      "a left-to-right drag leaves out the space at its ends",
      {|1 + 2 + 3|},
      drawn,
      lazy(after({|¦2 + 1 + 2 + 3 + 4|}, drag(3, 13))),
    ),
    case(
      "a right-to-left drag leaves out the space at its ends",
      {|2 + 3 + 4|},
      drawn,
      lazy(after({|¦1 + 2 + 3 + 4|}, drag(13, 3))),
    ),
    case(
      "space before a run is left out",
      {|1 + 2|},
      drawn,
      lazy(selecting({|2 +§ 1 + 2¦ + 3|})),
    ),
    case(
      "space after a run is left out",
      {|1 + 2|},
      drawn,
      lazy(selecting({|2 + §1 + 2 ¦+ 3|})),
    ),
    case(
      "the steps start from the run a drag draws",
      {|1 + 2 + 3|},
      named,
      lazy(after({|¦2 + 1 + 2 + 3 + 4|}, drag(3, 13))),
    ),
    case(
      "replacing a dragged run puts the chain back together",
      {|2 + 9 + 4|},
      replaced(~with_input={|¦9|}),
      lazy(after({|¦2 + 1 + 2 + 3 + 4|}, drag(3, 13))),
    ),
    case(
      "a drag over a product's run is drawn alone",
      {|3 * 4|},
      drawn,
      lazy(after({|¦2 * 3 * 4 * 5|}, drag(3, 9))),
    ),
    case(
      "a right-to-left drag over all of a chain is all of it",
      {|x ** 2 + 3 * x + 4|},
      drawn,
      lazy(after({|¦x ** 2 + 3 * x + 4|}, drag(20, 0))),
    ),
    case(
      "a right-to-left drag over the end of a chain is that run",
      {|3 * x + 4|},
      drawn,
      lazy(after({|¦x ** 2 + 3 * x + 4|}, drag(20, 9))),
    ),
    case(
      "a minus snaps to the operands it joins after a plus",
      {|3 * x - 4|},
      drawn,
      lazy(selecting({|x ** 2 + 3 * x §-¦ 4|})),
    ),
    case(
      "the steps start from the run a minus snaps to",
      {|3 * x - 4|},
      named,
      lazy(selecting({|x ** 2 + 3 * x §-¦ 4|})),
    ),
    run(
      "a minus after a plus is a run",
      true,
      lazy(selecting({|x ** 2 + 3 * x §-¦ 4|})),
    ),
    run(
      "a minus after a minus is not: it would change the sign",
      false,
      lazy(selecting({|8 - 4 §-¦ 2|})),
    ),
    run(
      "a division is not: division isn't associative",
      false,
      lazy(selecting({|8 / 4 §/¦ 2|})),
    ),
    case(
      "the steps on a division start from the node it is in",
      {|8 / 4 / 2|},
      named,
      lazy(selecting({|8 / 4 §/¦ 2|})),
    ),
    case(
      "replacing an operand replaces just it",
      {|9 + 2|},
      replaced(~with_input={|¦9|}),
      lazy(selecting({|§1¦ + 2|})),
    ),
    run("a times is a run", true, lazy(selecting({|8 * 4 §*¦ 2|}))),
    case(
      "space before a last operand leaves just the operand",
      {|4|},
      named,
      lazy(selecting({|x ** 2 + 3 * x +§ 4¦|})),
    ),
    run(
      "space before a last operand is not a run",
      false,
      lazy(selecting({|x ** 2 + 3 * x +§ 4¦|})),
    ),
    case(
      "all of a chain with a minus is its node",
      {|x ** 2 + 3 * x - 4|},
      named,
      lazy(selecting({|§x ** 2 + 3 * x - 4¦|})),
    ),
    case(
      "the steps start from the run drawn at a chain's end",
      {|3 * x + 4|},
      named,
      lazy(selecting({|x ** 2 + §3 * x + 4¦|})),
    ),
    case(
      "replacing a run at a chain's end",
      {|x ** 2 + 9|},
      replaced(~with_input={|¦9|}),
      lazy(selecting({|x ** 2 + §3 * x + 4¦|})),
    ),
    case(
      "replacing a run inside an argument",
      {|sin(1 + 9 + 4)|},
      replaced(~with_input={|¦9|}),
      lazy(selecting({|sin(1 + §2 + 3¦ + 4)|})),
    ),
    case(
      "a selection inside an argument stays there",
      {|x+y|},
      drawn,
      lazy(selecting({|sin(§x+y¦)|})),
    ),
    case(
      "a comma and an argument expand to the arguments",
      {|x ** 2 , x|},
      drawn,
      lazy(selecting({|diff(x ** 2 §, x¦) + diff(2 * x, x)|})),
    ),
    case(
      "a function and part of its arguments expand to the call",
      {|diff(x ** 2, x)|},
      drawn,
      lazy(selecting({|§diff(x ** 2,¦ x) + diff(2 * x, x)|})),
    ),
    case(
      "the steps on a function and part of its arguments start from the call",
      {|diff(x ** 2, x)|},
      named,
      lazy(selecting({|§diff(x ** 2,¦ x) + diff(2 * x, x)|})),
    ),
    case(
      "an argument and the closing parenthesis expand to the call",
      {|diff(x ** 2, x)|},
      drawn,
      lazy(selecting({|diff(x ** 2, §x)¦ + diff(2 * x, x)|})),
    ),
    case(
      "the steps on an argument and the closing parenthesis start from the call",
      {|diff(x ** 2, x)|},
      named,
      lazy(selecting({|diff(x ** 2, §x)¦ + diff(2 * x, x)|})),
    ),
    run(
      "an argument and the closing parenthesis are not a run",
      false,
      lazy(selecting({|diff(x ** 2, §x)¦ + diff(2 * x, x)|})),
    ),
  ],
);
