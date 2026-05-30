open Alcotest;
open Language;

let parse_exp = (s: string) => {
  switch (Haz3lcore.Parser.to_term(s, ~root=Exp)) {
  | Some(e) => e
  | None => Alcotest.fail("Failed to parse expression: " ++ s)
  };
};

let statics = exp =>
  Statics.mk(CoreSettings.on, Builtins.ctx_init(Some(Int)), exp) |> fst;

let take_selected_step = (~selected_id, exp) => {
  let steps =
    switch (
      EvaluatorStep.get_status(
        ~settings=CoreSettings.on,
        exp,
        Environment.empty,
      )
    ) {
    | AutoStep(step) => [step]
    | AvailableSteps(steps) => steps
    };
  switch (
    steps
    |> List.find_opt(step =>
         switch (EvaluatorStep.get_step_id_in(step, exp)) {
         | Some(id) => id == selected_id
         | None => EvaluatorStep.get_step_id(step) == selected_id
         }
       )
  ) {
  | Some(step) => EvaluatorStep.take_step(step)
  | None => None
  };
};

let rec find_plus_with_right_five_after_times = (exp: Exp.t): option(Exp.t) =>
  switch (exp.term) {
  | BinOp(Operators.Int(Operators.Plus), left, {term: Atom(Int(i)), _})
      when Bigint.to_int(i) == Some(5) =>
    switch (left.term) {
    | BinOp(
        Operators.Int(Operators.Plus),
        _,
        {term: BinOp(Operators.Int(Operators.Times), _, _), _},
      )
    | BinOp(Operators.Int(Operators.Times), _, _) => Some(exp)
    | _ =>
      switch (find_plus_with_right_five_after_times(left)) {
      | Some(_) as result => result
      | None => None
      }
    }
  | BinOp(_, left, right) =>
    switch (find_plus_with_right_five_after_times(left)) {
    | Some(_) as result => result
    | None => find_plus_with_right_five_after_times(right)
    }
  | Parens(inner) => find_plus_with_right_five_after_times(inner)
  | _ => None
  };

let test_mixed_precedence_step_here = () => {
  let exp = parse_exp("3 + 4 * 5");
  let (plus_id, times_id, five_id) =
    switch (exp.term) {
    | BinOp(_, _, {term: BinOp(_, _, five), _} as times) => (
        Exp.rep_id(exp),
        Exp.rep_id(times),
        Exp.rep_id(five),
      )
    | _ => Alcotest.fail("Unexpected parse tree for 3 + 4 * 5")
    };
  let visual_ids = AssocSelection.find_assoc_for_id(plus_id, statics(exp));
  check(
    bool,
    "mixed-precedence visual snap reaches right edge",
    true,
    List.mem(five_id, visual_ids),
  );
  let snapped_ids =
    AssocSelection.find_reparenthesize_for_id(plus_id, statics(exp));
  check(
    bool,
    "mixed-precedence snap keeps right operand whole",
    true,
    List.mem(times_id, snapped_ids) && !List.mem(five_id, snapped_ids),
  );
  switch (
    Reparenthesize.reparenthesize_selection(~selected_ids=snapped_ids, exp)
  ) {
  | Some({selected_is_single_binop, _}) =>
    check(
      bool,
      "mixed-precedence selection is parenthesize-only",
      false,
      selected_is_single_binop,
    )
  | None =>
    Alcotest.fail("Expected Step here selection to reparenthesize 3 + 4 * 5")
  };
};

let test_mixed_precedence_left_operand_step_here = () => {
  let exp = parse_exp("4 * 5 + 5");
  let (plus_id, times_id, four_id) =
    switch (exp.term) {
    | BinOp(_, {term: BinOp(_, four, _), _} as times, _) => (
        Exp.rep_id(exp),
        Exp.rep_id(times),
        Exp.rep_id(four),
      )
    | _ => Alcotest.fail("Unexpected parse tree for 4 * 5 + 5")
    };
  let visual_ids = AssocSelection.find_assoc_for_id(plus_id, statics(exp));
  check(
    bool,
    "mixed-precedence visual snap reaches left edge",
    true,
    List.mem(four_id, visual_ids),
  );
  let snapped_ids =
    AssocSelection.find_reparenthesize_for_id(plus_id, statics(exp));
  check(
    bool,
    "mixed-precedence action snap keeps left operand whole",
    true,
    List.mem(times_id, snapped_ids) && !List.mem(four_id, snapped_ids),
  );
  switch (
    Reparenthesize.reparenthesize_selection(~selected_ids=snapped_ids, exp)
  ) {
  | Some({selected_is_single_binop, _}) =>
    check(
      bool,
      "mixed-precedence left selection is parenthesize-only",
      false,
      selected_is_single_binop,
    )
  | None =>
    Alcotest.fail("Expected Step here selection to reparenthesize 4 * 5 + 5")
  };
};

let test_mixed_precedence_inside_plus_chain_step_here = () => {
  let exp = parse_exp("1 + 2 + 3 + 4 * 5 + 5 + 6");
  let selected_plus =
    switch (find_plus_with_right_five_after_times(exp)) {
    | Some(selected_plus) => selected_plus
    | None => Alcotest.fail("Expected to find selected plus in full chain")
    };
  let times_id =
    switch (selected_plus.term) {
    | BinOp(
        _,
        {
          term:
            BinOp(
              _,
              _,
              {term: BinOp(Operators.Int(Operators.Times), _, _), _} as times,
            ),
          _,
        },
        _,
      ) =>
      Exp.rep_id(times)
    | _ => Alcotest.fail("Unexpected selected plus shape")
    };
  let snapped_ids =
    AssocSelection.find_reparenthesize_for_id(
      Exp.rep_id(selected_plus),
      statics(exp),
    );
  check(
    bool,
    "mixed-precedence action snap keeps left chain operand whole",
    true,
    List.mem(times_id, snapped_ids),
  );
  switch (
    Reparenthesize.reparenthesize_selection(~selected_ids=snapped_ids, exp)
  ) {
  | Some({selected_is_single_binop, _}) =>
    check(
      bool,
      "larger mixed chain selection is parenthesize-only",
      false,
      selected_is_single_binop,
    )
  | None =>
    Alcotest.fail(
      "Expected Step here selection to reparenthesize full mixed chain",
    )
  };
};

let test_single_binop_step_here_can_evaluate_after_parenthesizing = () => {
  let exp = parse_exp("1 + 2 + 3");
  let outer_plus_id = Exp.rep_id(exp);
  let snapped_ids =
    AssocSelection.find_reparenthesize_for_id(outer_plus_id, statics(exp));
  switch (
    Reparenthesize.reparenthesize_selection(~selected_ids=snapped_ids, exp)
  ) {
  | Some({exp: reparenthesized, selected_id, selected_is_single_binop}) =>
    check(
      bool,
      "simple associative selection is a single BinOp",
      true,
      selected_is_single_binop,
    );
    switch (take_selected_step(~selected_id, reparenthesized)) {
    | Some(_) => ()
    | None =>
      Alcotest.fail(
        "Expected reparenthesized single BinOp selection to have an evaluation step",
      )
    };
  | None =>
    Alcotest.fail("Expected Step here selection to reparenthesize 1 + 2 + 3")
  };
};

let tests = (
  "Reparenthesize",
  [
    test_case(
      "Step here evaluates a selected single BinOp after parenthesizing",
      `Quick,
      test_single_binop_step_here_can_evaluate_after_parenthesizing,
    ),
    test_case(
      "Step here works across mixed + and * precedence",
      `Quick,
      test_mixed_precedence_step_here,
    ),
    test_case(
      "Step here works when mixed precedence is on the left",
      `Quick,
      test_mixed_precedence_left_operand_step_here,
    ),
    test_case(
      "Step here works inside a larger mixed plus chain",
      `Quick,
      test_mixed_precedence_inside_plus_chain_step_here,
    ),
  ],
);
