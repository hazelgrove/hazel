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
    ~measured=syntax.measured,
    ~term_data=syntax.term_data,
    z,
  )
  |> Printer.of_segment(~holes="?", ~concave_holes="~", ~indent=" ");
};

let effective_segment_string = (input: string): string => {
  let z = Test_Editing.mk_zipper(input);
  effective_segment_string_from_zipper(z);
};

let effective_root_string = (input: string): string => {
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
  switch (
    SelectionEffective.root_id(
      ~mode=Associative,
      ~info_map=statics.info_map,
      ~measured=syntax.measured,
      ~term_data=syntax.term_data,
      z,
    )
  ) {
  | None => ""
  | Some(id) =>
    switch (Language.ProofHacks.find_exp_id(id, term)) {
    | None => ""
    | Some(exp) =>
      ExpToSegment.exp_to_segment(
        ~settings=ExpToSegment.Settings.editable(~inline=true),
        exp,
      )
      |> Printer.of_segment(~holes="?", ~concave_holes="~", ~indent=" ")
    }
  };
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

let test_root = (~name, ~input, ~expected) =>
  test_case(name, `Quick, () =>
    check(
      testable(Fmt.string, String.equal),
      expected,
      expected,
      effective_root_string(input),
    )
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
  let model_selected_exp =
    switch (
      TermData.get_root_id_using_ranges(
        z.selection.content,
        syntax.term_data,
        syntax.measured,
      )
    ) {
    | Some(id) => Language.ProofHacks.find_exp_id(id, statics.term)
    | None => None
    };
  let selected_exp =
    switch (model_selected_exp) {
    | Some(model_exp)
        when
          !Language.Equality.ignoring_ascriptions.exp(model_exp, statics.term) =>
      Some(model_exp)
    | _ =>
      switch (
        Language.Reparenthesize.reparenthesize_selection(
          ~whole_selected_ids,
          ~selected_ids,
          statics.term,
        )
      ) {
      | Some(result) => Language.Reparenthesize.selected_exp(result)
      | None => model_selected_exp
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
      ~name=
        "nested selected power stays authoritative over subtraction suffix",
      ~input={|1 + 2 * §((1 - cos(2 * x)) / 2) ** 2¦|},
      ~expected={|((1 - cos(2 * x)) / 2) ** 2|},
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
      ~name="if delimiter selection snaps over full conditional",
      ~input={|let conditional = §if !true then 1 else¦ 2 in conditional|},
      ~expected={|if !true then 1 else 2|},
    ),
    test(
      ~name="unary minus operand selection snaps over negation",
      ~input={|let num = 1 in let arithmetic = -§num¦ * 1 in arithmetic|},
      ~expected={|-num|},
    ),
    test_root(
      ~name="unary minus operand selection root is negation",
      ~input={|let num = 1 in -§1¦ * num|},
      ~expected={|- 1|},
    ),
    test_root(
      ~name="unary minus expanded selection root is negation",
      ~input={|let num = 1 in §-1¦ * num|},
      ~expected={|- 1|},
    ),
    test_root(
      ~name="unary minus operator selection root is negation",
      ~input={|let num = 1 in §-¦1 * num|},
      ~expected={|- 1|},
    ),
    test_root(
      ~name="unary minus variable selection root is negation",
      ~input={|let num = 1 in -§num¦ * 1|},
      ~expected={|- num|},
    ),
    test(
      ~name="unary minus token selection snaps over negation",
      ~input={|let arithmetic = §-¦42 in arithmetic|},
      ~expected={|-42|},
    ),
    test(
      ~name="tuple comma selection snaps over all expressions",
      ~input={|(1 §, 2¦, 3)|},
      ~expected={|1 , 2, 3|},
    ),
    test(
      ~name="tuple comma token selection snaps over all expressions",
      ~input={|(1 §,¦ 2, 3)|},
      ~expected={|1 , 2, 3|},
    ),
    test(
      ~name="tuple comparison comma selection snaps over all expressions",
      ~input=
        {|let comparison = (0 == 0, 0 §< 1, 1 <=¦ 1, 2 > 1, 1 >= 1) in comparison|},
      ~expected={|0 == 0, 0 < 1, 1 <= 1, 2 > 1, 1 >= 1|},
    ),
    test(
      ~name="cons selection snaps over right-associated tail",
      ~input={|let xs : ([Int]) = 1 :: §2 ::¦ 3 :: [] : [Int] in xs|},
      ~expected={|2 :: 3 :: [] : [Int]|},
    ),
    test(
      ~name="cons operator token selection snaps over right-associated list",
      ~input={|let xs : ([Int]) = 1 §::¦ 2 :: 3 :: [] : [Int] in xs|},
      ~expected={|1 :: 2 :: 3 :: [] : [Int]|},
    ),
    test_root(
      ~name="cons operator token selection root is full list",
      ~input={|let xs : ([Int]) = 1 §::¦ 2 :: 3 :: [] : [Int] in xs|},
      ~expected={|1:: 2:: 3:: []:[Int]|},
    ),
    test(
      ~name="case keyword selection snaps over full case",
      ~input=
        {|let f = fun xs : ([Int]) -> §case¦ xs | [] => 0 | _ => 1 end in f|},
      ~expected={|case xs | [] => 0 | _ => 1 end|},
    ),
    test(
      ~name="let keyword selection snaps through definition body",
      ~input=
        {|§let¦ list_length : (poly a -> [a] -> Int) = typfun a -> fun l : ([a]) -> case l | [] => 0 | _hd::tl => 1 + list_length'@<a>(tl) end in list_length|},
      ~expected=
        {|let list_length : (poly a -> [a] -> Int) = typfun a -> fun l : ([a]) -> case l | [] => 0 | _hd::tl => 1 + list_length'@<a>(tl) end in list_length|},
    ),
    test(
      ~name="let in token selection snaps through body",
      ~input={|let num = 1 : Int §in¦ num + 1|},
      ~expected={|let num = 1 : Int in num + 1|},
    ),
    test(
      ~name="case keyword selection snaps over list-length case",
      ~input=
        {|let list_length : (poly a -> [a] -> Int) = typfun a -> fun l : ([a]) -> §case¦ l | [] => 0 | _hd::tl => 1 + list_length'@<a>(tl) end in list_length|},
      ~expected={|case l | [] => 0 | _hd::tl => 1 + list_length'@<a>(tl) end|},
    ),
    test(
      ~name="case rule bar selection snaps over list-length case",
      ~input=
        {|let list_length : (poly a -> [a] -> Int) = typfun a -> fun l : ([a]) -> case l §|¦ [] => 0 | _hd::tl => 1 + list_length'@<a>(tl) end in list_length|},
      ~expected={|case l | [] => 0 | _hd::tl => 1 + list_length'@<a>(tl) end|},
    ),
    test(
      ~name="case rule arrow selection snaps over list-length case",
      ~input=
        {|let list_length : (poly a -> [a] -> Int) = typfun a -> fun l : ([a]) -> case l | [] §=>¦ 0 | _hd::tl => 1 + list_length'@<a>(tl) end in list_length|},
      ~expected={|case l | [] => 0 | _hd::tl => 1 + list_length'@<a>(tl) end|},
    ),
    test(
      ~name="case pattern cons selection snaps over cons pattern",
      ~input=
        {|let length = fun xs : ([Int]) -> case xs | [] => 0 | _hd§::¦tl => 1 end in length|},
      ~expected={|_hd::tl|},
    ),
    test_actions(
      ~name="case keyword uses standard current-term selection",
      ~input=
        {|let f = fun xs : ([Int]) -> ¦case xs | [] => 0 | _ => 1 end in f|},
      ~actions=[Action.Select(Term(Current))],
      ~expected={|case xs | [] => 0 | _ => 1 end|},
    ),
    test_actions(
      ~name="case rule selection escalates through standard selection",
      ~input=
        {|let f = fun xs : ([Int]) -> case xs | [] => false |¦ _ :: [] => false | _ :: _ :: _ => true end in f|},
      ~actions=[
        Action.Select(Term(Current)),
        Action.Select(Term(Current)),
      ],
      ~expected=
        {|case xs | [] => false | _ :: [] => false | _ :: _ :: _ => true end|},
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
    test(
      ~name="adt recursive arrow selection snaps over full sum body",
      ~input=
        {|let exp_equal : ((rec Exp §->¦ + Var(String) + Lam((String, Exp))+ Ap((Exp, Exp))), (rec Exp -> + Var(String) + Lam((String, Exp))+ Ap((Exp, Exp)))) -> Bool = ? in exp_equal|},
      ~expected=
        {|rec Exp -> + Var(String) + Lam((String, Exp))+ Ap((Exp, Exp))|},
    ),
  ],
);
