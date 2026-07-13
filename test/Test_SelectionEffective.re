open Alcotest;
open Haz3lcore;

let effective_segment_string_from_zipper = (z: Zipper.t): string => {
  let term = MakeTerm.from_zip_for_sem(z, ~root=Exp).term;
  let statics =
    CachedStatics.init_from_term(
      ~settings=Test_Editing.default_settings,
      ~is_dynamic_term=true,
      term,
    );
  let syntax =
    CachedSyntax.mk(z, ~info_map=statics.info_map, ~dyn_map=Id.Map.empty);
  SelectionEffective.associative_segment(
    ~info_map=statics.info_map,
    ~term_data=syntax.term_data,
    z,
  )
  |> Printer.of_segment(~holes="?", ~concave_holes="~", ~indent=" ");
};

let effective_segment_string = (input: string): string => {
  let z = Test_Editing.mk_zipper(input);
  effective_segment_string_from_zipper(z);
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

let effective_segment_after_drag =
    (~input: string, ~start_col: int, ~end_col: int): string => {
  let z =
    Test_Editing.perform(
      Zipper.init(),
      Test_Editing.mk(input)
      @ [
        Test_Editing.move_point(~col=start_col, ()),
        Test_Editing.resize_point(~col=end_col, ()),
      ],
    );
  effective_segment_string_from_zipper(z);
};

let test_drag = (~name, ~input, ~start_col, ~end_col, ~expected) =>
  test_case(name, `Quick, () =>
    check(
      testable(Fmt.string, String.equal),
      expected,
      expected,
      effective_segment_after_drag(~input, ~start_col, ~end_col),
    )
  );

let effective_rewrite_source_string = (input: string): string => {
  let z = Test_Editing.mk_zipper(input);
  let term = MakeTerm.from_zip_for_sem(z, ~root=Exp).term;
  let statics =
    CachedStatics.init_from_term(
      ~settings=Test_Editing.default_settings,
      ~is_dynamic_term=true,
      term,
    );
  let syntax =
    CachedSyntax.mk(z, ~info_map=statics.info_map, ~dyn_map=Id.Map.empty);
  let selected_ids =
    SelectionEffective.ids(
      ~mode=SelectionEffective.Associative,
      ~info_map=statics.info_map,
      ~measured=syntax.measured,
      ~term_data=syntax.term_data,
      z,
    );
  let whole_selected_ids =
    switch (
      TermData.get_root_id_using_ranges(
        z.selection.content,
        syntax.term_data,
        syntax.measured,
      )
    ) {
    | Some(id) =>
      switch (TermData.segment(id, syntax.term_data)) {
      | Some(segment) when segment == z.selection.content => [id]
      | _ => []
      }
    | None => []
    };
  let selected_exp =
    switch (
      Language.Reparenthesize.reparenthesize_selection(
        ~whole_selected_ids,
        ~selected_ids,
        statics.term,
      )
    ) {
    | Some(result) => Language.Reparenthesize.selected_exp(result)
    | None =>
      switch (
        TermData.get_root_id_using_ranges(
          z.selection.content,
          syntax.term_data,
          syntax.measured,
        )
      ) {
      | Some(id) => Language.ProofHacks.find_exp_id(id, statics.term)
      | None => None
      }
    };
  switch (selected_exp) {
  | Some(selected_exp) =>
    selected_exp
    |> ExpToSegment.exp_to_segment(
         ~settings=ExpToSegment.Settings.editable(~inline=true),
         _,
       )
    |> Printer.of_segment(~holes="?", ~concave_holes="~", ~indent=" ")
  | None => "<missing selected expression>"
  };
};

let test_effective_rewrite_source = (~name, ~input, ~expected) =>
  test_case(name, `Quick, () =>
    check(
      testable(Fmt.string, String.equal),
      expected,
      expected,
      effective_rewrite_source_string(input),
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
      ~name="equality selection snaps over application left operand",
      ~input={|rev(rev(xs)) §== xs¦|},
      ~expected={|rev(rev(xs)) == xs|},
    ),
    test(
      ~name="equality selection snaps over projected left operand",
      ~input={|^^fold(rev(rev(xs))) §== xs¦|},
      ~expected={|^^fold(rev(rev(xs))) == xs|},
    ),
    test(
      ~name="associative selection inside function argument stays in argument",
      ~input={|sin(§x+y¦)|},
      ~expected={|x+y|},
    ),
    test(
      ~name=
        "function name selection with associative argument stays on function",
      ~input={|§sin¦(x+y)|},
      ~expected={|sin|},
    ),
    test(
      ~name="function name selection with simple argument stays on function",
      ~input={|§f¦(x)|},
      ~expected={|f|},
    ),
    test_effective_rewrite_source(
      ~name=
        "effective associative source is used for rewrite and proof actions",
      ~input={|1 + (§1 / 2 - cos(2 * x) + 1 / 2 * y¦)|},
      ~expected={|1 / 2 - cos(2 * x) + 1 / 2 * y|},
    ),
    test_effective_rewrite_source(
      ~name=
        "effective associative source keeps a selected product operand whole",
      ~input=
        {|1 + §2 * (1 / 2 * (1 / 2 - cos(2 * x)) + 1 / 2 * cos(2 * x) ** 2)¦|},
      ~expected=
        {|2 * (1 / 2 * (1 / 2 - cos(2 * x)) + 1 / 2 * cos(2 * x) ** 2)|},
    ),
    test_effective_rewrite_source(
      ~name="selected subtraction keeps its trailing operand",
      ~input={|§x ** 2 + 3 * x - 4¦|},
      ~expected={|x ** 2 + 3 * x - 4|},
    ),
    test(
      ~name="subtraction suffix selection stays on the suffix",
      ~input={|(x + 1) ** 2 §- 1 - 4¦|},
      ~expected={|- 1 - 4|},
    ),
    test_effective_rewrite_source(
      ~name="selected subtraction suffix stays narrow",
      ~input={|(x + 1) ** 2 §- 1 - 4¦|},
      ~expected={|- 1 - 4|},
    ),
    test(
      ~name="tuple comma selection snaps over all expressions",
      ~input={|(1 §, 2¦, 3)|},
      ~expected={|1 , 2, 3|},
    ),
    test_drag(
      ~name="tuple drag crossing adjacent comma snaps over all expressions",
      ~input={|(1, 2, 3, 4, 5)¦|},
      ~start_col=8,
      ~end_col=9,
      ~expected={|1, 2, 3, 4, 5|},
    ),
    test(
      ~name="tuple item-to-comma selection snaps over all expressions",
      ~input={|(1, 2, §3,¦ 4, 5)|},
      ~expected={|1, 2, 3, 4, 5|},
    ),
    test(
      ~name=
        "tuple compound item-to-comma selection snaps over all expressions",
      ~input={|(1, 2, §3 + 4 + 5,¦ 4, 5)|},
      ~expected={|1, 2, 3 + 4 + 5, 4, 5|},
    ),
    test(
      ~name=
        "tuple comma selection snaps over all expressions from later comma",
      ~input={|(1, 2 §, 3¦)|},
      ~expected={|1, 2 , 3|},
    ),
    test(
      ~name=
        "tuple item-to-item selection across comma snaps over all expressions",
      ~input={|(1, §3, 4¦, 5)|},
      ~expected={|1, 3, 4, 5|},
    ),
    test(
      ~name="nested tuple comma selection stays inside inner tuple",
      ~input={|(1, (2 §, 3¦), 4)|},
      ~expected={|2 , 3|},
    ),
    test(
      ~name="labeled tuple comma selection snaps over all expressions",
      ~input={|(a = 2, b = §3, 1¦ + 2 + 3)|},
      ~expected={|a = 2, b = 3, 1 + 2 + 3|},
    ),
    test(
      ~name=
        "labeled tuple value-to-comma selection snaps over all expressions",
      ~input={|(a = 2, b = §3,¦ 4, 5)|},
      ~expected={|a = 2, b = 3, 4, 5|},
    ),
    test(
      ~name=
        "labeled tuple compound value-to-comma selection snaps over all expressions",
      ~input={|(a = 2, b = §3 + 4 + 5,¦ 4, 5)|},
      ~expected={|a = 2, b = 3 + 4 + 5, 4, 5|},
    ),
    test(
      ~name=
        "labeled tuple assignment-to-comma selection snaps over all expressions",
      ~input={|(a = 2, §b = 3,¦ 4, 5)|},
      ~expected={|a = 2, b = 3, 4, 5|},
    ),
    test(
      ~name="list comma selection snaps over all expressions",
      ~input={|[1 §, 2¦, 3]|},
      ~expected={|1 , 2, 3|},
    ),
    test(
      ~name="tuple pattern comma selection snaps over all patterns",
      ~input={|fun (x §, y¦, z) -> x|},
      ~expected={|x , y, z|},
    ),
    test(
      ~name="tuple type comma selection snaps over all types",
      ~input={|let x : (Int §, Bool¦, String) = (1, true, "s") in x|},
      ~expected={|Int , Bool, String|},
    ),
    test(
      ~name="type arrow selection snaps right-associatively",
      ~input=
        {|let f : Int §-> Bool¦ -> String = fun x -> fun y -> "s" in f|},
      ~expected={|Int -> Bool -> String|},
    ),
    test(
      ~name="nested type arrow selection snaps over its adjacent types",
      ~input=
        {|let f : Int -> Bool §-> String¦ = fun x -> fun y -> "s" in f|},
      ~expected={|Bool -> String|},
    ),
  ],
);
