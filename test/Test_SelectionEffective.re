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

type effective_selection = {
  segment: Segment.t,
  exp: option(Language.Exp.t),
  override: option(SelectionEffective.associative_override),
  root_id: option(Id.t),
};

let effective_selection = (z: Zipper.t) => {
  let (term, statics, syntax) = setup(z);
  let override =
    SelectionEffective.associative_override(
      ~info_map=statics.info_map,
      ~measured=syntax.measured,
      ~term_data=syntax.term_data,
      z,
    );
  let standard_root_id =
    TermData.get_root_id_using_ranges(
      z.selection.content,
      syntax.term_data,
      syntax.measured,
    );
  let selection =
    switch (override) {
    | Some(override) => {
        segment: override.segment,
        exp: Some(override.exp),
        override: Some(override),
        root_id: Some(override.container_id),
      }
    | None => {
        segment:
          SelectionEffective.expanded_segment(
            ~measured=syntax.measured,
            ~term_data=syntax.term_data,
            z,
          ),
        exp:
          standard_root_id
          |> Option.bind(_, id => Language.ProofHacks.find_exp_id(id, term)),
        override: None,
        root_id: standard_root_id,
      }
    };
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

let effective_segment_string = (input: string): string =>
  Test_Editing.mk_zipper(input) |> effective_segment_string_from_zipper;

let effective_exp_string = (input: string): string => {
  let z = Test_Editing.mk_zipper(input);
  let (_, _, selection) = effective_selection(z);
  selection.exp |> Option.map(exp_string) |> Option.value(~default="");
};

let replacement_string = (~input: string, ~with_input: string): string => {
  let z = Test_Editing.mk_zipper(input);
  let (full_exp, syntax, selection) = effective_selection(z);
  let with_exp =
    Test_Editing.mk_zipper(with_input)
    |> MakeTerm.from_zip_for_sem(~root=Exp)
    |> (result => result.term);
  switch (
    selection.override
    |> Option.bind(_, override =>
         SelectionEffective.replacement_for_override(
           ~override,
           ~with_exp,
           ~full_exp,
           ~term_data=syntax.term_data,
         )
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

let reparenthesized_selection_strings =
    (input: string): option((string, string)) => {
  let z = Test_Editing.mk_zipper(input);
  let (full_exp, _, selection) = effective_selection(z);
  switch (selection.override) {
  | None => None
  | Some(override) =>
    switch (SelectionEffective.reparenthesize_override(~override, ~full_exp)) {
    | None => None
    | Some(result) =>
      Language.Reparenthesize.selected_exp(result)
      |> Option.map(selected_exp =>
           (exp_string(result.exp), exp_string(selected_exp))
         )
    }
  };
};

let effective_root_string = (input: string): string => {
  let z = Test_Editing.mk_zipper(input);
  let (term, _, selection) = effective_selection(z);
  selection.root_id
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

let test_override = (~name, ~input, ~expected) =>
  test_case(
    name,
    `Quick,
    () => {
      let z = Test_Editing.mk_zipper(input);
      let (_, _, selection) = effective_selection(z);
      check(
        testable(Fmt.bool, Bool.equal),
        "override presence",
        expected,
        Option.is_some(selection.override),
      );
    },
  );

let effective_segment_after_actions =
    (~input: string, ~actions: list(Action.t)): string => {
  let z =
    Test_Editing.perform(Zipper.init(), Test_Editing.mk(input) @ actions);
  effective_segment_string_from_zipper(z);
};

let test_actions = (~name, ~input, ~actions, ~expected) =>
  test_case(name, `Quick, () =>
    check(
      testable(Fmt.string, String.equal),
      expected,
      expected,
      effective_segment_after_actions(~input, ~actions),
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
    test_override(
      ~name="subtraction suffix is a virtual associative selection",
      ~input={|x ** 2 + 3 * x §-¦ 4|},
      ~expected=true,
    ),
    test_override(
      ~name="repeated subtraction falls back to dev selection",
      ~input={|8 - 4 §-¦ 2|},
      ~expected=false,
    ),
    test_override(
      ~name="division falls back to dev selection",
      ~input={|8 / 4 §/¦ 2|},
      ~expected=false,
    ),
    test_exp(
      ~name="division checker uses dev expression",
      ~input={|8 / 4 §/¦ 2|},
      ~expected={|8 / 4 / 2|},
    ),
    test_override(
      ~name="multiplication remains a virtual associative selection",
      ~input={|8 * 4 §*¦ 2|},
      ~expected=true,
    ),
    test_exp(
      ~name="space before final atom uses dev checker selection",
      ~input={|x ** 2 + 3 * x +§ 4¦|},
      ~expected={|4|},
    ),
    test_override(
      ~name="space before final atom does not create an override",
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
      ~expected={|x ** 2 + 9|},
    ),
    test_replacement(
      ~name="middle arithmetic replacement preserves surrounding addends",
      ~input={|1 + 2 + §3 + 4¦ + 5|},
      ~with_input={|¦7|},
      ~expected={|1 + 2 + 7 + 5|},
    ),
    test_case(
      "middle arithmetic axiom target can be reparenthesized", `Quick, () =>
      check(
        option(pair(string, string)),
        "reparenthesized expression and selected subtree",
        Some(("1 + 2 + (3 + 4) + 5", "3 + 4")),
        reparenthesized_selection_strings({|1 + 2 + §3 + 4¦ + 5|}),
      )
    ),
    test_replacement(
      ~name="compound associative replacement remains grouped",
      ~input={|1 + 2 + §3 + 4¦ + 5|},
      ~with_input={|¦7 - 1|},
      ~expected={|1 + 2 + (7 - 1) + 5|},
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
    test_override(
      ~name="application closing delimiter falls back to dev selection",
      ~input={|diff(x ** 2, §x)¦ + diff(2 * x, x)|},
      ~expected=false,
    ),
  ],
);
