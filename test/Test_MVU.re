open Alcotest;
open Language;
open Test_Evaluator_Prelude;
open IdTagged.FreshGrammar;

// == Helpers ==

// evaluate_direct: evaluate without re-elaboration (for applying Closures)
let evaluate_direct = (exp: Exp.t): Exp.t =>
  Evaluator.evaluate(~env=Builtins.env_init, exp) |> fst;

// Check if an expression is a function (Closure from evaluation)
let is_function = (exp: Exp.t): bool =>
  switch (exp.term) {
  | Fun(_)
  | FixF(_)
  | Closure(_, {term: Fun(_), _})
  | Closure(_, {term: FixF(_), _}) => true
  | _ => false
  };

// Extract 4-tuple components from evaluated MVU app
let extract_elm_app = (exp: Exp.t): option((Exp.t, Exp.t, Exp.t, Exp.t)) =>
  switch (Haz3lcore.HazelDOM.strip_wrappers(exp).term) {
  | Tuple([init_model, update_fn, view_fn, subs_fn])
      when is_function(update_fn) && is_function(view_fn) =>
    Some((init_model, update_fn, view_fn, subs_fn))
  | _ => None
  };

// Apply a function to an argument via evaluate_direct
let apply = (fn: Exp.t, arg: Exp.t): Exp.t =>
  evaluate_direct(Exp.ap(Forward, fn, arg));

// Check that a DHExp represents valid HTML (has a recognized constructor at top)
let rec is_valid_html = (exp: Exp.t): bool => {
  switch (Haz3lcore.HazelDOM.of_constructor(exp)) {
  | Some(("Text", body)) =>
    // Text requires a string argument
    switch (Haz3lcore.HazelDOM.strip_wrappers(body).term) {
    | Atom(String(_)) => true
    | _ => false
    }
  | Some(("Int", body)) =>
    switch (Haz3lcore.HazelDOM.strip_wrappers(body).term) {
    | Atom(Int(_)) => true
    | _ => false
    }
  | Some(("Float", body)) =>
    switch (Haz3lcore.HazelDOM.strip_wrappers(body).term) {
    | Atom(Float(_)) => true
    | _ => false
    }
  | Some(("Bool", body)) =>
    switch (Haz3lcore.HazelDOM.strip_wrappers(body).term) {
    | Atom(Bool(_)) => true
    | _ => false
    }
  | Some(("Br" | "Hr", _)) => true
  | Some((
      "Div" | "Span" | "P" | "Pre" | "Code" | "Blockquote" | "H1" | "H2" | "H3" |
      "H4" |
      "H5" |
      "H6" |
      "Ul" |
      "Ol" |
      "Li" |
      "Form" |
      "Label" |
      "Button" |
      "Select" |
      "Option" |
      "Table" |
      "Thead" |
      "Tbody" |
      "Tr" |
      "Th" |
      "Td" |
      "Header" |
      "Footer" |
      "Nav" |
      "Main" |
      "Section" |
      "Article" |
      "Aside" |
      "Node",
      body,
    )) =>
    // Container elements: check children are valid HTML
    switch (Haz3lcore.HazelDOM.strip_wrappers(body).term) {
    | Tuple([_attrs, children]) =>
      switch (Haz3lcore.HazelDOM.strip_wrappers(children).term) {
      | ListLit(items) => List.for_all(is_valid_html, items)
      | _ => false
      }
    | _ => false
    }
  | Some(("Input" | "TextArea" | "Img" | "A", _)) => true
  | Some(("Checkbox" | "Radio" | "Range", _)) => true
  | _ => false
  };
};

// Describe what's wrong with invalid HTML for error messages
let describe_html_issue = (exp: Exp.t): string => {
  switch (Haz3lcore.HazelDOM.of_constructor(exp)) {
  | None =>
    "Not an HTML constructor. Got: "
    ++ (
      switch (exp.term) {
      | Atom(Int(n)) => "Int(" ++ Bigint.to_string(n) ++ ")"
      | Atom(String(s)) => "String(" ++ s ++ ")"
      | Tuple(_) => "Tuple"
      | Closure(_, _) => "Closure"
      | Ap(_, _, _) => "Ap (unevaluated application)"
      | _ => "unknown term"
      }
    )
  | Some((name, body)) =>
    "Constructor "
    ++ name
    ++ " has invalid body: "
    ++ (
      switch (Haz3lcore.HazelDOM.strip_wrappers(body).term) {
      | Atom(Int(n)) => "Int(" ++ Bigint.to_string(n) ++ ")"
      | Atom(String(s)) => "String(\"" ++ s ++ "\")"
      | Tuple(items) =>
        "Tuple(" ++ string_of_int(List.length(items)) ++ " items)"
      | Ap(_, _, _) => "Ap (unevaluated application)"
      | _ => "other"
      }
    )
  };
};

// Assert that an expression is valid HTML
let assert_valid_html = (msg: string, exp: Exp.t) =>
  if (!is_valid_html(exp)) {
    fail(msg ++ ": " ++ describe_html_issue(exp));
  };

// == MVU Counter Tests ==

let counter_program = {|
let update : (Int, Int) -> Int = fun (msg, model) -> model + msg in
let view : Int -> HTML = fun model -> Div(
  [Class("counter"), Style([("text-align", "center"), ("padding", "20px")])],
  [
    H2([], [Text("MVU Counter")]),
    Div(
      [Style([("font-size", "48px"), ("margin", "20px")])],
      [Int(model)]
    ),
    Div(
      [],
      [
        Button([OnClick(-1), Style([("font-size", "24px")])], [Text("-")]),
        Button([OnClick(1), Style([("font-size", "24px")])], [Text("+")])
      ]
    )
  ]
) in
let subs : Int -> Sub = fun _model -> SubNone in
(0, update, view, subs)
|};

let counter_detects_as_elm_app =
  test_case(
    "Counter detects as Elm app",
    `Quick,
    () => {
      let result = parse_and_evaluate(counter_program);
      switch (extract_elm_app(result)) {
      | Some(_) => ()
      | None => fail("Counter should detect as 4-tuple Elm app")
      };
    },
  );

let counter_init_model_is_zero =
  test_case(
    "Counter init model is 0",
    `Quick,
    () => {
      let result = parse_and_evaluate(counter_program);
      switch (extract_elm_app(result)) {
      | Some((init_model, _, _, _)) =>
        check(dhexp_typ, "init_model should be 0", Exp.int(0), init_model)
      | None => fail("Not an Elm app")
      };
    },
  );

let counter_view_produces_valid_html =
  test_case(
    "Counter view(0) produces valid HTML",
    `Quick,
    () => {
      let result = parse_and_evaluate(counter_program);
      switch (extract_elm_app(result)) {
      | Some((init_model, _, view_fn, _)) =>
        let html = apply(view_fn, init_model);
        assert_valid_html("view(0)", html);
      | None => fail("Not an Elm app")
      };
    },
  );

let counter_update_increment =
  test_case(
    "Counter update(1, 0) = 1",
    `Quick,
    () => {
      let result = parse_and_evaluate(counter_program);
      switch (extract_elm_app(result)) {
      | Some((init_model, update_fn, _, _)) =>
        let new_model =
          apply(update_fn, Exp.tuple([Exp.int(1), init_model]));
        let new_model = Haz3lcore.HazelDOM.strip_wrappers(new_model);
        check(dhexp_typ, "model should be 1", Exp.int(1), new_model);
      | None => fail("Not an Elm app")
      };
    },
  );

let counter_update_decrement =
  test_case(
    "Counter update(-1, 0) = -1",
    `Quick,
    () => {
      let result = parse_and_evaluate(counter_program);
      switch (extract_elm_app(result)) {
      | Some((init_model, update_fn, _, _)) =>
        let new_model =
          apply(update_fn, Exp.tuple([Exp.int(-1), init_model]));
        let new_model = Haz3lcore.HazelDOM.strip_wrappers(new_model);
        check(dhexp_typ, "model should be -1", Exp.int(-1), new_model);
      | None => fail("Not an Elm app")
      };
    },
  );

let counter_view_after_update =
  test_case(
    "Counter view(update(1, 0)) produces valid HTML",
    `Quick,
    () => {
      let result = parse_and_evaluate(counter_program);
      switch (extract_elm_app(result)) {
      | Some((init_model, update_fn, view_fn, _)) =>
        let new_model =
          apply(update_fn, Exp.tuple([Exp.int(1), init_model]));
        let new_model = Haz3lcore.HazelDOM.strip_wrappers(new_model);
        let html = apply(view_fn, new_model);
        assert_valid_html("view(1)", html);
      | None => fail("Not an Elm app")
      };
    },
  );

let counter_full_cycle =
  test_case(
    "Counter: 5 increments then 2 decrements",
    `Quick,
    () => {
      let result = parse_and_evaluate(counter_program);
      switch (extract_elm_app(result)) {
      | Some((init_model, update_fn, view_fn, _)) =>
        // 5 increments
        let model = ref(init_model);
        for (_ in 1 to 5) {
          let new_model = apply(update_fn, Exp.tuple([Exp.int(1), model^]));
          model := Haz3lcore.HazelDOM.strip_wrappers(new_model);
        };
        check(dhexp_typ, "after 5 increments", Exp.int(5), model^);
        // 2 decrements
        for (_ in 1 to 2) {
          let new_model =
            apply(update_fn, Exp.tuple([Exp.int(-1), model^]));
          model := Haz3lcore.HazelDOM.strip_wrappers(new_model);
        };
        check(dhexp_typ, "after 2 decrements", Exp.int(3), model^);
        // View should still be valid
        let html = apply(view_fn, model^);
        assert_valid_html("view(3)", html);
      | None => fail("Not an Elm app")
      };
    },
  );

// == Keyboard Game Tests ==

let keyboard_game_program = {|
let max : (Int, Int) -> Int = fun (a, b) -> if a > b then a else b in
let min : (Int, Int) -> Int = fun (a, b) -> if a < b then a else b in
let step : Int = 20 in
let update = fun (msg, model) ->
  let x = fst(model) in
  let y = snd(model) in
  if msg == "ArrowUp" then
    (x, max(0, y - step))
  else if msg == "ArrowDown" then
    (x, min(360, y + step))
  else if msg == "ArrowLeft" then
    (max(0, x - step), y)
  else if msg == "ArrowRight" then
    (min(360, x + step), y)
  else
    model
in
let view : (Int, Int) -> HTML = fun model ->
  let x = fst(model) in
  let y = snd(model) in
  Div(
    [Id("game"), Style([("width", "400px"), ("height", "400px"), ("position", "relative")])],
    [
      Div([Style([("text-align", "center")])], [Text("Use arrow keys")]),
      Div(
        [Style([("width", "40px"), ("height", "40px"), ("position", "absolute"),
                ("left", string_of_int(x) ++ "px"), ("top", string_of_int(y) ++ "px")])],
        []
      ),
      Div(
        [Style([("position", "absolute"), ("bottom", "10px")])],
        [Text("Position: (" ++ string_of_int(x) ++ ", " ++ string_of_int(y) ++ ")")]
      )
    ]
  )
in
let subs : (Int, Int) -> Sub = fun _model ->
  OnDocumentKeyDown(fun (key, _code, _ctrl, _shift, _alt, _meta) -> key)
in
((180, 180), update, view, subs)
|};

let keyboard_detects_as_elm_app =
  test_case(
    "Keyboard game detects as Elm app",
    `Quick,
    () => {
      let result = parse_and_evaluate(keyboard_game_program);
      switch (extract_elm_app(result)) {
      | Some(_) => ()
      | None => fail("Keyboard game should detect as 4-tuple Elm app")
      };
    },
  );

let keyboard_view_produces_valid_html =
  test_case(
    "Keyboard view((180,180)) produces valid HTML",
    `Quick,
    () => {
      let result = parse_and_evaluate(keyboard_game_program);
      switch (extract_elm_app(result)) {
      | Some((init_model, _, view_fn, _)) =>
        let html = apply(view_fn, init_model);
        assert_valid_html("view((180,180))", html);
      | None => fail("Not an Elm app")
      };
    },
  );

let keyboard_update_arrow_right =
  test_case(
    "Keyboard update(ArrowRight, (180,180))",
    `Quick,
    () => {
      let result = parse_and_evaluate(keyboard_game_program);
      switch (extract_elm_app(result)) {
      | Some((init_model, update_fn, _, _)) =>
        // msg is just the key string (extracted from KeyEvent in subs handler)
        let msg = Exp.string("ArrowRight");
        let new_model = apply(update_fn, Exp.tuple([msg, init_model]));
        let new_model = Haz3lcore.HazelDOM.strip_wrappers(new_model);
        // Should be (200, 180) - moved right by step=20
        check(
          dhexp_typ,
          "model after ArrowRight",
          Exp.tuple([Exp.int(200), Exp.int(180)]),
          new_model,
        );
      | None => fail("Not an Elm app")
      };
    },
  );

let keyboard_view_after_move =
  test_case(
    "Keyboard view after ArrowRight produces valid HTML",
    `Quick,
    () => {
      let result = parse_and_evaluate(keyboard_game_program);
      switch (extract_elm_app(result)) {
      | Some((init_model, update_fn, view_fn, _)) =>
        let msg = Exp.string("ArrowRight");
        let new_model = apply(update_fn, Exp.tuple([msg, init_model]));
        let new_model = Haz3lcore.HazelDOM.strip_wrappers(new_model);
        let html = apply(view_fn, new_model);
        assert_valid_html("view after ArrowRight", html);
      | None => fail("Not an Elm app")
      };
    },
  );

// == Strip Wrappers / Constructor Extraction Tests ==

let strip_wrappers_basic =
  test_case(
    "strip_wrappers handles Asc",
    `Quick,
    () => {
      let inner = Exp.int(42);
      let wrapped = Exp.asc(inner, Typ.int());
      let result = Haz3lcore.HazelDOM.strip_wrappers(wrapped);
      check(dhexp_typ, "should strip Asc", inner, result);
    },
  );

let strip_wrappers_parens =
  test_case(
    "strip_wrappers handles Parens",
    `Quick,
    () => {
      let inner = Exp.int(42);
      let wrapped = Exp.parens(inner);
      let result = Haz3lcore.HazelDOM.strip_wrappers(wrapped);
      check(dhexp_typ, "should strip Parens", inner, result);
    },
  );

let of_constructor_basic =
  test_case(
    "of_constructor on Text(\"hello\")",
    `Quick,
    () => {
      let exp = parse_and_evaluate({|Text("hello")|});
      switch (Haz3lcore.HazelDOM.of_constructor(exp)) {
      | Some(("Text", _)) => ()
      | Some((name, _)) => fail("Expected Text constructor, got: " ++ name)
      | None => fail("of_constructor returned None")
      };
    },
  );

let of_constructor_nested =
  test_case(
    "of_constructor on Div([], [Text(\"hi\")])",
    `Quick,
    () => {
      let exp = parse_and_evaluate({|Div([], [Text("hi")])|});
      switch (Haz3lcore.HazelDOM.of_constructor(exp)) {
      | Some(("Div", _)) => ()
      | Some((name, _)) => fail("Expected Div constructor, got: " ++ name)
      | None => fail("of_constructor returned None")
      };
    },
  );

// == All tests ==

let tests = (
  "MVU",
  [
    // Counter
    counter_detects_as_elm_app,
    counter_init_model_is_zero,
    counter_view_produces_valid_html,
    counter_update_increment,
    counter_update_decrement,
    counter_view_after_update,
    counter_full_cycle,
    // Keyboard game
    keyboard_detects_as_elm_app,
    keyboard_view_produces_valid_html,
    keyboard_update_arrow_right,
    keyboard_view_after_move,
    // Utility
    strip_wrappers_basic,
    strip_wrappers_parens,
    of_constructor_basic,
    of_constructor_nested,
  ],
);
