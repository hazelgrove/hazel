open Alcotest;
open Haz3lcore;

let setup = (z: Zipper.t) => {
  let term = MakeTerm.from_zip_for_sem(z, ~root=Exp).term;
  let statics =
    CachedStatics.init_from_term(
      ~settings=Test_Editing.default_settings,
      ~is_dynamic_term=true,
      term,
    );
  let syntax =
    CachedSyntax.mk(z, ~info_map=statics.info_map, ~dyn_map=Id.Map.empty);
  (term, statics, syntax);
};

let effective_selection = (z: Zipper.t) => {
  let (term, statics, syntax) = setup(z);
  let selection =
    SelectionEffective.effective_selection(
      ~info_map=statics.info_map,
      ~measured=syntax.measured,
      ~term_data=syntax.term_data,
      z,
    );
  (term, syntax, selection);
};

let exp_string = (exp: Language.Exp.t): string =>
  ExpToSegment.exp_to_segment(
    ~settings=ExpToSegment.Settings.editable(~inline=true),
    exp,
  )
  |> Printer.of_segment(~holes="?", ~concave_holes="~", ~indent=" ");

let effective_segment_string_from_zipper = (z: Zipper.t): string => {
  let (_, _, selection) = effective_selection(z);
  selection.segment
  |> Printer.of_segment(~holes="?", ~concave_holes="~", ~indent=" ");
};

let effective_exp_string_from_zipper = (z: Zipper.t): string => {
  let (term, _, selection) = effective_selection(z);
  SelectionEffective.selected_exp(~full_exp=term, selection)
  |> Option.map(exp_string)
  |> Option.value(~default="");
};

let effective_segment_string = (input: string): string =>
  Test_Editing.mk_zipper(input) |> effective_segment_string_from_zipper;

let effective_exp_string = (input: string): string => {
  Test_Editing.mk_zipper(input) |> effective_exp_string_from_zipper;
};

let replacement_string_from_zipper =
    (~with_input: string, z: Zipper.t): string => {
  let (full_exp, syntax, selection) = effective_selection(z);
  let with_exp =
    Test_Editing.mk_zipper(with_input)
    |> MakeTerm.from_zip_for_sem(~root=Exp)
    |> (result => result.term);
  switch (
    SelectionEffective.replacement(
      ~selection,
      ~with_exp,
      ~full_exp,
      ~term_data=syntax.term_data,
    )
  ) {
  | None => ""
  | Some({at_exp, with_exp}) =>
    Language.ProofHacks.replace_exp_id(
      Language.Exp.rep_id(at_exp),
      full_exp,
      with_exp,
    )
    |> exp_string
  };
};

let replacement_string = (~input: string, ~with_input: string): string =>
  Test_Editing.mk_zipper(input)
  |> replacement_string_from_zipper(~with_input);

let effective_root_string = (input: string): string => {
  let z = Test_Editing.mk_zipper(input);
  let (term, _, selection) = effective_selection(z);
  SelectionEffective.root_id(selection)
  |> Option.bind(_, id => Language.ProofHacks.find_exp_id(id, term))
  |> Option.map(exp_string)
  |> Option.value(~default="");
};

let test = (~name, ~input, ~expected) =>
  test_case(name, `Quick, () =>
    check(
      testable(Fmt.string, String.equal),
      expected,
      expected,
      effective_segment_string(input),
    )
  );

let test_exp = (~name, ~input, ~expected) =>
  test_case(name, `Quick, () =>
    check(
      testable(Fmt.string, String.equal),
      expected,
      expected,
      effective_exp_string(input),
    )
  );

let test_replacement = (~name, ~input, ~with_input, ~expected) =>
  test_case(name, `Quick, () =>
    check(
      testable(Fmt.string, String.equal),
      expected,
      expected,
      replacement_string(~input, ~with_input),
    )
  );

let test_root = (~name, ~input, ~expected) =>
  test_case(name, `Quick, () =>
    check(
      testable(Fmt.string, String.equal),
      expected,
      expected,
      effective_root_string(input),
    )
  );

let test_virtual = (~name, ~input, ~expected) =>
  test_case(
    name,
    `Quick,
    () => {
      let z = Test_Editing.mk_zipper(input);
      let (_, _, selection) = effective_selection(z);
      check(
        testable(Fmt.bool, Bool.equal),
        "virtual selection",
        expected,
        SelectionEffective.is_virtual(selection),
      );
    },
  );

let effective_segment_after_actions =
    (~input: string, ~actions: list(Action.t)): string => {
  let z =
    Test_Editing.perform(Zipper.init(), Test_Editing.mk(input) @ actions);
  effective_segment_string_from_zipper(z);
};

let zipper_after_actions = (~input: string, ~actions: list(Action.t)) =>
  Test_Editing.perform(Zipper.init(), Test_Editing.mk(input) @ actions);

let test_actions = (~name, ~input, ~actions, ~expected) =>
  test_case(name, `Quick, () =>
    check(
      testable(Fmt.string, String.equal),
      expected,
      expected,
      effective_segment_after_actions(~input, ~actions),
    )
  );

let test_actions_exp = (~name, ~input, ~actions, ~expected) =>
  test_case(name, `Quick, () =>
    check(
      testable(Fmt.string, String.equal),
      expected,
      expected,
      zipper_after_actions(~input, ~actions)
      |> effective_exp_string_from_zipper,
    )
  );

let test_actions_replacement =
    (~name, ~input, ~actions, ~with_input, ~expected) =>
  test_case(name, `Quick, () =>
    check(
      testable(Fmt.string, String.equal),
      expected,
      expected,
      zipper_after_actions(~input, ~actions)
      |> replacement_string_from_zipper(~with_input),
    )
  );

let tests = (
  "SelectionEffective",
  [
    test(
      ~name="associative selection snaps to left edge of nested product",
      ~input={|2 * 3 + 4 * 5 * 6 §+ "abc"¦|},
      ~expected={|4 * 5 * 6 + "abc"|},
    ),
    test(
      ~name="middle addition selection stays on selected addends",
      ~input={|1 + 2 + §3 + 4¦ + 5|},
      ~expected={|3 + 4|},
    ),
    test_root(
      ~name="middle addition selection expands to containing root",
      ~input={|1 + 2 + §3 + 4¦ + 5|},
      ~expected={|1 + 2 + 3 + 4|},
    ),
    test(
      ~name="middle addition prefix snaps to selected addends",
      ~input={|1 + 2 + §3 +¦ 4 + 5|},
      ~expected={|3 + 4|},
    ),
    test(
      ~name="middle addition suffix snaps to selected addends",
      ~input={|1 + 2 + 3 §+ 4¦ + 5|},
      ~expected={|3 + 4|},
    ),
    test_actions(
      ~name="mouse drag over middle addition stays on selected addends",
      ~input={|¦1 + 2 + 3 + 4 + 5|},
      ~actions=[
        Action.Move(
          Point(
            {
              row: 0,
              col: 8,
            },
            None,
          ),
        ),
        Action.Select(
          Resize(
            Point(
              {
                row: 0,
                col: 13,
              },
              None,
            ),
          ),
        ),
      ],
      ~expected={|3 + 4|},
    ),
    test_actions(
      ~name="mouse drag over middle addition operator snaps to addends",
      ~input={|¦1 + 2 + 3 + 4 + 5|},
      ~actions=[
        Action.Move(
          Point(
            {
              row: 0,
              col: 10,
            },
            None,
          ),
        ),
        Action.Select(
          Resize(
            Point(
              {
                row: 0,
                col: 13,
              },
              None,
            ),
          ),
        ),
      ],
      ~expected={|3 + 4|},
    ),
    test_actions(
      ~name="left-to-right drag discards boundary whitespace",
      ~input={|¦2 + 1 + 2 + 3 + 4|},
      ~actions=[
        Action.Move(
          Point(
            {
              row: 0,
              col: 3,
            },
            None,
          ),
        ),
        Action.Select(
          Resize(
            Point(
              {
                row: 0,
                col: 13,
              },
              None,
            ),
          ),
        ),
      ],
      ~expected={|1 + 2 + 3|},
    ),
    test_actions(
      ~name="right-to-left drag discards boundary whitespace",
      ~input={|¦1 + 2 + 3 + 4|},
      ~actions=[
        Action.Move(
          Point(
            {
              row: 0,
              col: 13,
            },
            None,
          ),
        ),
        Action.Select(
          Resize(
            Point(
              {
                row: 0,
                col: 3,
              },
              None,
            ),
          ),
        ),
      ],
      ~expected={|2 + 3 + 4|},
    ),
    test(
      ~name="associative selection discards only leading whitespace",
      ~input={|2 +§ 1 + 2¦ + 3|},
      ~expected={|1 + 2|},
    ),
    test(
      ~name="associative selection discards only trailing whitespace",
      ~input={|2 + §1 + 2 ¦+ 3|},
      ~expected={|1 + 2|},
    ),
    test_actions_exp(
      ~name="checker follows whitespace-boundary associative drag",
      ~input={|¦2 + 1 + 2 + 3 + 4|},
      ~actions=[
        Action.Move(
          Point(
            {
              row: 0,
              col: 3,
            },
            None,
          ),
        ),
        Action.Select(
          Resize(
            Point(
              {
                row: 0,
                col: 13,
              },
              None,
            ),
          ),
        ),
      ],
      ~expected={|1 + 2 + 3|},
    ),
    test_actions_replacement(
      ~name="replacement follows whitespace-boundary associative drag",
      ~input={|¦2 + 1 + 2 + 3 + 4|},
      ~actions=[
        Action.Move(
          Point(
            {
              row: 0,
              col: 3,
            },
            None,
          ),
        ),
        Action.Select(
          Resize(
            Point(
              {
                row: 0,
                col: 13,
              },
              None,
            ),
          ),
        ),
      ],
      ~with_input={|¦9|},
      ~expected={|2 + (9) + 4|},
    ),
    test_actions(
      ~name="multiplication drag discards boundary whitespace",
      ~input={|¦2 * 3 * 4 * 5|},
      ~actions=[
        Action.Move(
          Point(
            {
              row: 0,
              col: 3,
            },
            None,
          ),
        ),
        Action.Select(
          Resize(
            Point(
              {
                row: 0,
                col: 9,
              },
              None,
            ),
          ),
        ),
      ],
      ~expected={|3 * 4|},
    ),
    test_actions(
      ~name="right-to-left full drag stays on the full expression",
      ~input={|¦x ** 2 + 3 * x + 4|},
      ~actions=[
        Action.Move(
          Point(
            {
              row: 0,
              col: 20,
            },
            None,
          ),
        ),
        Action.Select(
          Resize(
            Point(
              {
                row: 0,
                col: 0,
              },
              None,
            ),
          ),
        ),
      ],
      ~expected={|x ** 2 + 3 * x + 4|},
    ),
    test_actions(
      ~name="right-to-left suffix drag snaps to the suffix",
      ~input={|¦x ** 2 + 3 * x + 4|},
      ~actions=[
        Action.Move(
          Point(
            {
              row: 0,
              col: 20,
            },
            None,
          ),
        ),
        Action.Select(
          Resize(
            Point(
              {
                row: 0,
                col: 9,
              },
              None,
            ),
          ),
        ),
      ],
      ~expected={|3 * x + 4|},
    ),
    test(
      ~name="subtraction operator snaps over preceding additive term",
      ~input={|x ** 2 + 3 * x §-¦ 4|},
      ~expected={|3 * x - 4|},
    ),
    test_exp(
      ~name="subtraction checker expression matches highlight",
      ~input={|x ** 2 + 3 * x §-¦ 4|},
      ~expected={|3 * x - 4|},
    ),
    test_virtual(
      ~name="subtraction suffix is a virtual associative selection",
      ~input={|x ** 2 + 3 * x §-¦ 4|},
      ~expected=true,
    ),
    test_virtual(
      ~name="repeated subtraction falls back to dev selection",
      ~input={|8 - 4 §-¦ 2|},
      ~expected=false,
    ),
    test_virtual(
      ~name="division falls back to dev selection",
      ~input={|8 / 4 §/¦ 2|},
      ~expected=false,
    ),
    test_exp(
      ~name="division checker uses dev expression",
      ~input={|8 / 4 §/¦ 2|},
      ~expected={|8 / 4 / 2|},
    ),
    test_replacement(
      ~name="standard replacement falls through to dev selection",
      ~input={|§1¦ + 2|},
      ~with_input={|¦9|},
      ~expected={|9 + 2|},
    ),
    test_virtual(
      ~name="multiplication remains a virtual associative selection",
      ~input={|8 * 4 §*¦ 2|},
      ~expected=true,
    ),
    test_exp(
      ~name="space before final atom uses dev checker selection",
      ~input={|x ** 2 + 3 * x +§ 4¦|},
      ~expected={|4|},
    ),
    test_virtual(
      ~name="space before final atom stays on dev selection",
      ~input={|x ** 2 + 3 * x +§ 4¦|},
      ~expected=false,
    ),
    test_root(
      ~name="full additive selection root is the full expression",
      ~input={|§x ** 2 + 3 * x - 4¦|},
      ~expected={|x ** 2 + 3 * x - 4|},
    ),
    test_exp(
      ~name="reassociated additive selection expression matches highlight",
      ~input={|x ** 2 + §3 * x + 4¦|},
      ~expected={|3 * x + 4|},
    ),
    test_replacement(
      ~name="reassociated additive replacement uses highlighted segment",
      ~input={|x ** 2 + §3 * x + 4¦|},
      ~with_input={|¦9|},
      ~expected={|x ** 2 + (9)|},
    ),
    test_replacement(
      ~name="nested associative replacement uses its candidate container",
      ~input={|sin(1 + §2 + 3¦ + 4)|},
      ~with_input={|¦9|},
      ~expected={|sin(1 + (9) + 4)|},
    ),
    test(
      ~name="associative selection inside function argument stays in argument",
      ~input={|sin(§x+y¦)|},
      ~expected={|x+y|},
    ),
    test(
      ~name="tuple comma selection inside application stays in arguments",
      ~input={|diff(x ** 2 §, x¦) + diff(2 * x, x)|},
      ~expected={|x ** 2 , x|},
    ),
    test(
      ~name="selection from function through comma completes application",
      ~input={|§diff(x ** 2,¦ x) + diff(2 * x, x)|},
      ~expected={|diff(x ** 2, x)|},
    ),
    test_exp(
      ~name="function-comma checker expression matches highlight",
      ~input={|§diff(x ** 2,¦ x) + diff(2 * x, x)|},
      ~expected={|deriv x ** 2 by x|},
    ),
    test(
      ~name="application argument-closing delimiter selection selects app",
      ~input={|diff(x ** 2, §x)¦ + diff(2 * x, x)|},
      ~expected={|diff(x ** 2, x)|},
    ),
    test_exp(
      ~name="application closing delimiter checker matches highlight",
      ~input={|diff(x ** 2, §x)¦ + diff(2 * x, x)|},
      ~expected={|deriv x ** 2 by x|},
    ),
    test_virtual(
      ~name="application closing delimiter falls back to dev selection",
      ~input={|diff(x ** 2, §x)¦ + diff(2 * x, x)|},
      ~expected=false,
    ),
    test(
      ~name="derivative body selection stays inside its operator",
      ~input={|deriv §x ** 3¦ by x + deriv 2 * x by x|},
      ~expected={|x ** 3|},
    ),
    test(
      ~name="selection through by completes the derivative operator",
      ~input={|§deriv x ** 3 by¦ x + deriv 2 * x by x|},
      ~expected={|deriv x ** 3 by x|},
    ),
    test_exp(
      ~name="derivative boundary checker expression matches highlight",
      ~input={|§deriv x ** 3 by¦ x + deriv 2 * x by x|},
      ~expected={|deriv x ** 3 by x|},
    ),
    test_virtual(
      ~name="derivative boundary uses standard structural selection",
      ~input={|§deriv x ** 3 by¦ x + deriv 2 * x by x|},
      ~expected=false,
    ),
    test(
      ~name=
        "associative suffix beginning with derivative stays on selected terms",
      ~input={|deriv x ** 3 by x + §deriv 2 * x by x + 3¦ + 4|},
      ~expected={|deriv 2 * x by x + 3|},
    ),
    test_exp(
      ~name="derivative-led associative suffix checker matches highlight",
      ~input={|deriv x ** 3 by x + §deriv 2 * x by x + 3¦ + 4|},
      ~expected={|deriv 2 * x by x + 3|},
    ),
  ],
);
