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

// Extract components from a 2-tuple (e.g., (model, cmd) from update)
let extract_pair = (exp: Exp.t): option((Exp.t, Exp.t)) =>
  switch (Haz3lcore.HazelDOM.strip_wrappers(exp).term) {
  | Tuple([a, b]) =>
    Some((
      Haz3lcore.HazelDOM.strip_wrappers(a),
      Haz3lcore.HazelDOM.strip_wrappers(b),
    ))
  | _ => None
  };

// Assert that an expression is a specific constructor
let assert_constructor = (msg: string, expected_name: string, exp: Exp.t) =>
  switch (Haz3lcore.HazelDOM.of_constructor(exp)) {
  | Some((name, _)) when name == expected_name => ()
  | Some((name, _)) =>
    fail(msg ++ ": expected " ++ expected_name ++ ", got " ++ name)
  | None =>
    fail(msg ++ ": not a constructor (expected " ++ expected_name ++ ")")
  };

// Apply update_fn to (msg, model) and extract (new_model, cmd) pair
let apply_update =
    (update_fn: Exp.t, msg: Exp.t, model: Exp.t): (Exp.t, Exp.t) => {
  let result = apply(update_fn, Exp.tuple([msg, model]));
  switch (extract_pair(result)) {
  | Some(pair) => pair
  | None => fail("update did not return a 2-tuple")
  };
};

// Check that a subscription expression has a recognized constructor
let is_valid_sub = (exp: Exp.t): bool =>
  switch (Haz3lcore.HazelDOM.of_constructor(exp)) {
  | Some((
      "SubNone" | "SubBatch" | "Every" | "AnimationFrame" | "OnResize" |
      "OnVisibilityChange" |
      "OnDocumentKeyDown" |
      "OnDocumentKeyUp",
      _,
    )) =>
    true
  | _ => false
  };

// Extract a named field from a labeled tuple
let extract_field = (name: string, exp: Exp.t): option(Exp.t) => {
  switch (Haz3lcore.HazelDOM.strip_wrappers(exp).term) {
  | Tuple(fields) =>
    let rec find = (
      fun
      | [] => None
      | [field, ...rest] =>
        switch (Haz3lcore.HazelDOM.strip_wrappers(field).term) {
        | TupLabel({term: Label(label), _}, value) when label == name =>
          Some(Haz3lcore.HazelDOM.strip_wrappers(value))
        | _ => find(rest)
        }
    );
    find(fields);
  | _ => None
  };
};

// ============================================================
// == MVU Counter Tests (existing, legacy update pattern) ==
// ============================================================

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

// ============================================================
// == Keyboard Game Tests (existing) ==
// ============================================================

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

// ============================================================
// == Strip Wrappers / Constructor Extraction Tests (existing) ==
// ============================================================

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

// ============================================================
// == Real MVU Pattern Tests ==
// == Tests the actual (Action, Model) -> (Model, Cmd) pattern ==
// ============================================================

let real_mvu_program = {|
type Action = + Inc + Dec + SetTo(Int) in
let init = (count=0, label="test") in
let update = fun (action, model) ->
  case action
  | Inc => ((count=model.count + 1, label=model.label), CmdNone)
  | Dec => ((count=model.count - 1, label=model.label), CmdNone)
  | SetTo(n) => ((count=n, label=model.label), CmdNone)
  end
in
let view = fun model ->
  Div([], [
    H2([], [Text(model.label)]),
    P([], [Int(model.count)]),
    Button([OnClick(Inc)], [Text("+")]),
    Button([OnClick(Dec)], [Text("-")])
  ])
in
let subs = fun _model -> SubNone in
(init, update, view, subs)
|};

let real_mvu_detects_as_elm_app =
  test_case(
    "Real MVU detects as Elm app",
    `Quick,
    () => {
      let result = parse_and_evaluate(real_mvu_program);
      switch (extract_elm_app(result)) {
      | Some(_) => ()
      | None => fail("Real MVU should detect as 4-tuple Elm app")
      };
    },
  );

let real_mvu_update_returns_pair =
  test_case(
    "Real MVU update returns (model, cmd) pair",
    `Quick,
    () => {
      let result = parse_and_evaluate(real_mvu_program);
      switch (extract_elm_app(result)) {
      | Some((init_model, update_fn, _, _)) =>
        let inc_msg = Exp.constructor("Inc", None);
        let update_result =
          apply(update_fn, Exp.tuple([inc_msg, init_model]));
        switch (extract_pair(update_result)) {
        | Some(_) => ()
        | None => fail("update should return a 2-tuple")
        };
      | None => fail("Not an Elm app")
      };
    },
  );

let real_mvu_cmd_is_cmdnone =
  test_case(
    "Real MVU update returns CmdNone",
    `Quick,
    () => {
      let result = parse_and_evaluate(real_mvu_program);
      switch (extract_elm_app(result)) {
      | Some((init_model, update_fn, _, _)) =>
        let inc_msg = Exp.constructor("Inc", None);
        let (_, cmd) = apply_update(update_fn, inc_msg, init_model);
        assert_constructor("cmd after Inc", "CmdNone", cmd);
      | None => fail("Not an Elm app")
      };
    },
  );

let real_mvu_update_inc_field =
  test_case(
    "Real MVU Inc increments count field",
    `Quick,
    () => {
      let result = parse_and_evaluate(real_mvu_program);
      switch (extract_elm_app(result)) {
      | Some((init_model, update_fn, _, _)) =>
        let inc_msg = Exp.constructor("Inc", None);
        let (model, _) = apply_update(update_fn, inc_msg, init_model);
        switch (extract_field("count", model)) {
        | Some(count) =>
          check(dhexp_typ, "count after Inc", Exp.int(1), count)
        | None => fail("could not extract count field")
        };
      | None => fail("Not an Elm app")
      };
    },
  );

let real_mvu_update_setto =
  test_case(
    "Real MVU SetTo(42) sets count to 42",
    `Quick,
    () => {
      let result = parse_and_evaluate(real_mvu_program);
      switch (extract_elm_app(result)) {
      | Some((init_model, update_fn, _, _)) =>
        let setto_msg =
          Exp.ap(Forward, Exp.constructor("SetTo", None), Exp.int(42));
        let (model, cmd) = apply_update(update_fn, setto_msg, init_model);
        assert_constructor("cmd after SetTo", "CmdNone", cmd);
        switch (extract_field("count", model)) {
        | Some(count) =>
          check(dhexp_typ, "count after SetTo(42)", Exp.int(42), count)
        | None => fail("could not extract count field")
        };
      | None => fail("Not an Elm app")
      };
    },
  );

let real_mvu_view_valid =
  test_case(
    "Real MVU view(init) produces valid HTML",
    `Quick,
    () => {
      let result = parse_and_evaluate(real_mvu_program);
      switch (extract_elm_app(result)) {
      | Some((init_model, _, view_fn, _)) =>
        let html = apply(view_fn, init_model);
        assert_valid_html("view(init)", html);
      | None => fail("Not an Elm app")
      };
    },
  );

let real_mvu_view_after_update =
  test_case(
    "Real MVU view valid after Inc",
    `Quick,
    () => {
      let result = parse_and_evaluate(real_mvu_program);
      switch (extract_elm_app(result)) {
      | Some((init_model, update_fn, view_fn, _)) =>
        let inc_msg = Exp.constructor("Inc", None);
        let (model, _) = apply_update(update_fn, inc_msg, init_model);
        let html = apply(view_fn, model);
        assert_valid_html("view after Inc", html);
      | None => fail("Not an Elm app")
      };
    },
  );

let real_mvu_full_cycle =
  test_case(
    "Real MVU: Inc, Inc, Dec, SetTo(10)",
    `Quick,
    () => {
      let result = parse_and_evaluate(real_mvu_program);
      switch (extract_elm_app(result)) {
      | Some((init_model, update_fn, view_fn, _)) =>
        let inc = Exp.constructor("Inc", None);
        let dec = Exp.constructor("Dec", None);
        let setto10 =
          Exp.ap(Forward, Exp.constructor("SetTo", None), Exp.int(10));
        // Inc twice
        let (m1, _) = apply_update(update_fn, inc, init_model);
        let (m2, _) = apply_update(update_fn, inc, m1);
        // Dec once
        let (m3, _) = apply_update(update_fn, dec, m2);
        // SetTo(10)
        let (m4, _) = apply_update(update_fn, setto10, m3);
        switch (extract_field("count", m4)) {
        | Some(count) =>
          check(dhexp_typ, "count after full cycle", Exp.int(10), count)
        | None => fail("could not extract count field")
        };
        // View should still be valid
        let html = apply(view_fn, m4);
        assert_valid_html("view after full cycle", html);
      | None => fail("Not an Elm app")
      };
    },
  );

// ============================================================
// == Command Structure Tests ==
// ============================================================

let cmd_program = {|
type Action = + Save + Notify(String) + Multi + FocusInput in
let init = (saved=false, msg="") in
let update = fun (action, model) ->
  case action
  | Save => ((saved=true, msg=model.msg), CmdNone)
  | Notify(text) => ((saved=model.saved, msg=text), Log(text))
  | Multi => ((saved=true, msg="done"), CmdBatch([Log("a"), Log("b")]))
  | FocusInput => (model, Focus("my-input"))
  end
in
let view = fun model ->
  Div([], [Text(if model.saved then "saved" else "unsaved")])
in
let subs = fun _model -> SubNone in
(init, update, view, subs)
|};

let cmd_save_returns_cmdnone =
  test_case(
    "Cmd: Save returns CmdNone",
    `Quick,
    () => {
      let result = parse_and_evaluate(cmd_program);
      switch (extract_elm_app(result)) {
      | Some((init_model, update_fn, _, _)) =>
        let save_msg = Exp.constructor("Save", None);
        let (_, cmd) = apply_update(update_fn, save_msg, init_model);
        assert_constructor("cmd after Save", "CmdNone", cmd);
      | None => fail("Not an Elm app")
      };
    },
  );

let cmd_notify_returns_log =
  test_case(
    "Cmd: Notify returns Log",
    `Quick,
    () => {
      let result = parse_and_evaluate(cmd_program);
      switch (extract_elm_app(result)) {
      | Some((init_model, update_fn, _, _)) =>
        let notify_msg =
          Exp.ap(
            Forward,
            Exp.constructor("Notify", None),
            Exp.string("hello"),
          );
        let (_, cmd) = apply_update(update_fn, notify_msg, init_model);
        assert_constructor("cmd after Notify", "Log", cmd);
      | None => fail("Not an Elm app")
      };
    },
  );

let cmd_multi_returns_cmdbatch =
  test_case(
    "Cmd: Multi returns CmdBatch",
    `Quick,
    () => {
      let result = parse_and_evaluate(cmd_program);
      switch (extract_elm_app(result)) {
      | Some((init_model, update_fn, _, _)) =>
        let multi_msg = Exp.constructor("Multi", None);
        let (_, cmd) = apply_update(update_fn, multi_msg, init_model);
        assert_constructor("cmd after Multi", "CmdBatch", cmd);
      | None => fail("Not an Elm app")
      };
    },
  );

let cmd_cmdbatch_has_list_body =
  test_case(
    "Cmd: CmdBatch body is a list of commands",
    `Quick,
    () => {
      let result = parse_and_evaluate(cmd_program);
      switch (extract_elm_app(result)) {
      | Some((init_model, update_fn, _, _)) =>
        let multi_msg = Exp.constructor("Multi", None);
        let (_, cmd) = apply_update(update_fn, multi_msg, init_model);
        switch (Haz3lcore.HazelDOM.of_constructor(cmd)) {
        | Some(("CmdBatch", body)) =>
          switch (Haz3lcore.HazelDOM.strip_wrappers(body).term) {
          | ListLit(items) =>
            check(
              Alcotest.int,
              "CmdBatch should have 2 items",
              2,
              List.length(items),
            );
            List.iter(
              item => assert_constructor("CmdBatch item", "Log", item),
              items,
            );
          | _ => fail("CmdBatch body is not a list")
          }
        | _ => fail("expected CmdBatch constructor")
        };
      | None => fail("Not an Elm app")
      };
    },
  );

let cmd_focus_returns_focus =
  test_case(
    "Cmd: FocusInput returns Focus",
    `Quick,
    () => {
      let result = parse_and_evaluate(cmd_program);
      switch (extract_elm_app(result)) {
      | Some((init_model, update_fn, _, _)) =>
        let focus_msg = Exp.constructor("FocusInput", None);
        let (_, cmd) = apply_update(update_fn, focus_msg, init_model);
        assert_constructor("cmd after FocusInput", "Focus", cmd);
      | None => fail("Not an Elm app")
      };
    },
  );

let cmd_view_after_update =
  test_case(
    "Cmd: view valid after command-producing update",
    `Quick,
    () => {
      let result = parse_and_evaluate(cmd_program);
      switch (extract_elm_app(result)) {
      | Some((init_model, update_fn, view_fn, _)) =>
        let notify_msg =
          Exp.ap(
            Forward,
            Exp.constructor("Notify", None),
            Exp.string("test"),
          );
        let (model, _) = apply_update(update_fn, notify_msg, init_model);
        let html = apply(view_fn, model);
        assert_valid_html("view after Notify", html);
      | None => fail("Not an Elm app")
      };
    },
  );

// ============================================================
// == Subscription Tests ==
// ============================================================

let sub_program = {|
type Action = + Tick + Toggle in
let init = (count=0, running=false) in
let update = fun (action, model) ->
  case action
  | Tick => ((count=model.count + 1, running=model.running), CmdNone)
  | Toggle => ((count=model.count, running=if model.running then false else true), CmdNone)
  end
in
let view = fun model ->
  Div([], [Text(string_of_int(model.count))])
in
let subs = fun model ->
  if model.running then Every(1000.0, fun _ts -> Tick) else SubNone
in
(init, update, view, subs)
|};

let sub_stopped_returns_subnone =
  test_case(
    "Sub: stopped model returns SubNone",
    `Quick,
    () => {
      let result = parse_and_evaluate(sub_program);
      switch (extract_elm_app(result)) {
      | Some((init_model, _, _, subs_fn)) =>
        let sub = apply(subs_fn, init_model);
        assert_constructor("subs when stopped", "SubNone", sub);
      | None => fail("Not an Elm app")
      };
    },
  );

let sub_running_returns_every =
  test_case(
    "Sub: running model returns Every",
    `Quick,
    () => {
      let result = parse_and_evaluate(sub_program);
      switch (extract_elm_app(result)) {
      | Some((init_model, update_fn, _, subs_fn)) =>
        // Toggle to running
        let toggle_msg = Exp.constructor("Toggle", None);
        let (running_model, _) =
          apply_update(update_fn, toggle_msg, init_model);
        let sub = apply(subs_fn, running_model);
        assert_constructor("subs when running", "Every", sub);
      | None => fail("Not an Elm app")
      };
    },
  );

let sub_keyboard_returns_ondocumentkeydown =
  test_case(
    "Sub: keyboard game returns OnDocumentKeyDown",
    `Quick,
    () => {
      let result = parse_and_evaluate(keyboard_game_program);
      switch (extract_elm_app(result)) {
      | Some((init_model, _, _, subs_fn)) =>
        let sub = apply(subs_fn, init_model);
        assert_constructor("keyboard subs", "OnDocumentKeyDown", sub);
      | None => fail("Not an Elm app")
      };
    },
  );

let sub_counter_returns_subnone =
  test_case(
    "Sub: counter returns SubNone",
    `Quick,
    () => {
      let result = parse_and_evaluate(counter_program);
      switch (extract_elm_app(result)) {
      | Some((init_model, _, _, subs_fn)) =>
        let sub = apply(subs_fn, init_model);
        assert_constructor("counter subs", "SubNone", sub);
      | None => fail("Not an Elm app")
      };
    },
  );

let sub_is_valid_sub_check =
  test_case(
    "Sub: is_valid_sub recognizes Sub constructors",
    `Quick,
    () => {
      // SubNone
      let result = parse_and_evaluate(counter_program);
      switch (extract_elm_app(result)) {
      | Some((init_model, _, _, subs_fn)) =>
        let sub = apply(subs_fn, init_model);
        check(
          Alcotest.bool,
          "SubNone is valid sub",
          true,
          is_valid_sub(sub),
        );
      | None => fail("Not an Elm app")
      };
      // Every
      let result = parse_and_evaluate(sub_program);
      switch (extract_elm_app(result)) {
      | Some((init_model, update_fn, _, subs_fn)) =>
        let toggle_msg = Exp.constructor("Toggle", None);
        let (running_model, _) =
          apply_update(update_fn, toggle_msg, init_model);
        let sub = apply(subs_fn, running_model);
        check(Alcotest.bool, "Every is valid sub", true, is_valid_sub(sub));
      | None => fail("Not an Elm app")
      };
      // A bare int should not be a valid sub
      check(
        Alcotest.bool,
        "Int is not valid sub",
        false,
        is_valid_sub(Exp.int(42)),
      );
    },
  );

// ============================================================
// == HTML Element Coverage Tests ==
// ============================================================

let html_table_tr_td =
  test_case(
    "HTML: Table > Tr > Td is valid",
    `Quick,
    () => {
      let exp =
        parse_and_evaluate({|Table([], [Tr([], [Td([], [Text("cell")])])])|});
      assert_valid_html("Table/Tr/Td", exp);
    },
  );

let html_table_thead_tbody =
  test_case(
    "HTML: Table with Thead and Tbody is valid",
    `Quick,
    () => {
      let exp =
        parse_and_evaluate(
          {|Table([], [Thead([], [Tr([], [Th([], [Text("Header")])])]), Tbody([], [Tr([], [Td([], [Text("Data")])])])])|},
        );
      assert_valid_html("Table/Thead/Tbody", exp);
    },
  );

let html_select_option =
  test_case(
    "HTML: Select > Option is valid",
    `Quick,
    () => {
      let exp =
        parse_and_evaluate(
          {|Select([], [Option([], [Text("A")]), Option([], [Text("B")])])|},
        );
      assert_valid_html("Select/Option", exp);
    },
  );

let html_ol_li =
  test_case(
    "HTML: Ol > Li is valid",
    `Quick,
    () => {
      let exp =
        parse_and_evaluate(
          {|Ol([], [Li([], [Text("one")]), Li([], [Text("two")])])|},
        );
      assert_valid_html("Ol/Li", exp);
    },
  );

let html_h3 =
  test_case(
    "HTML: H3 is valid",
    `Quick,
    () => {
      let exp = parse_and_evaluate({|H3([], [Text("heading")])|});
      assert_valid_html("H3", exp);
    },
  );

let html_hr =
  test_case(
    "HTML: Hr is valid",
    `Quick,
    () => {
      let exp = parse_and_evaluate({|Hr|});
      assert_valid_html("Hr", exp);
    },
  );

let html_br =
  test_case(
    "HTML: Br is valid",
    `Quick,
    () => {
      let exp = parse_and_evaluate({|Br|});
      assert_valid_html("Br", exp);
    },
  );

let html_a_element =
  test_case(
    "HTML: A element is valid",
    `Quick,
    () => {
      let exp =
        parse_and_evaluate(
          {|A([Href("https://example.com")], [Text("link")])|},
        );
      assert_valid_html("A", exp);
    },
  );

let html_nested_structure =
  test_case(
    "HTML: deeply nested structure is valid",
    `Quick,
    () => {
      let exp =
        parse_and_evaluate(
          {|Div([], [
            Ul([], [
              Li([], [Span([], [Text("item 1")])]),
              Li([], [Span([], [Text("item 2")])])
            ]),
            P([], [Text("paragraph")])
          ])|},
        );
      assert_valid_html("nested structure", exp);
    },
  );

let html_semantic_elements =
  test_case(
    "HTML: semantic elements are valid",
    `Quick,
    () => {
      let exp =
        parse_and_evaluate(
          {|Div([], [
            Header([], [Text("header")]),
            Nav([], [Text("nav")]),
            Main([], [
              Section([], [Text("section")]),
              Article([], [Text("article")])
            ]),
            Footer([], [Text("footer")])
          ])|},
        );
      assert_valid_html("semantic elements", exp);
    },
  );

// ============================================================
// == Negative / Edge Case Tests ==
// ============================================================

let neg_3tuple_not_elm_app =
  test_case(
    "Neg: 3-tuple is not an Elm app",
    `Quick,
    () => {
      let result =
        parse_and_evaluate(
          {|(0, fun (m, a) -> m + a, fun m -> Div([], [Text("hi")]))|},
        );
      switch (extract_elm_app(result)) {
      | Some(_) => fail("3-tuple should not be detected as Elm app")
      | None => ()
      };
    },
  );

let neg_5tuple_not_elm_app =
  test_case(
    "Neg: 5-tuple is not an Elm app",
    `Quick,
    () => {
      let result =
        parse_and_evaluate(
          {|(0, fun (m, a) -> m, fun m -> Div([], []), fun m -> SubNone, 99)|},
        );
      switch (extract_elm_app(result)) {
      | Some(_) => fail("5-tuple should not be detected as Elm app")
      | None => ()
      };
    },
  );

let neg_bare_string_not_html =
  test_case("Neg: bare string is not valid HTML", `Quick, () => {
    check(
      Alcotest.bool,
      "bare string not HTML",
      false,
      is_valid_html(Exp.string("hello")),
    )
  });

let neg_bare_int_not_html =
  test_case("Neg: bare int is not valid HTML", `Quick, () => {
    check(
      Alcotest.bool,
      "bare int not HTML",
      false,
      is_valid_html(Exp.int(42)),
    )
  });

let neg_unknown_constructor_not_html =
  test_case(
    "Neg: unknown constructor is not valid HTML",
    `Quick,
    () => {
      let exp = parse_and_evaluate({|FakeElement([], [Text("hi")])|});
      check(
        Alcotest.bool,
        "FakeElement not HTML",
        false,
        is_valid_html(exp),
      );
    },
  );

// ============================================================
// == All tests ==
// ============================================================

let tests = (
  "MVU",
  [
    // Counter (legacy pattern)
    counter_detects_as_elm_app,
    counter_init_model_is_zero,
    counter_view_produces_valid_html,
    counter_update_increment,
    counter_update_decrement,
    counter_view_after_update,
    counter_full_cycle,
    // Keyboard game (legacy pattern)
    keyboard_detects_as_elm_app,
    keyboard_view_produces_valid_html,
    keyboard_update_arrow_right,
    keyboard_view_after_move,
    // Utility
    strip_wrappers_basic,
    strip_wrappers_parens,
    of_constructor_basic,
    of_constructor_nested,
    // Real MVU pattern: (Action, Model) -> (Model, Cmd)
    real_mvu_detects_as_elm_app,
    real_mvu_update_returns_pair,
    real_mvu_cmd_is_cmdnone,
    real_mvu_update_inc_field,
    real_mvu_update_setto,
    real_mvu_view_valid,
    real_mvu_view_after_update,
    real_mvu_full_cycle,
    // Command structure
    cmd_save_returns_cmdnone,
    cmd_notify_returns_log,
    cmd_multi_returns_cmdbatch,
    cmd_cmdbatch_has_list_body,
    cmd_focus_returns_focus,
    cmd_view_after_update,
    // Subscriptions
    sub_stopped_returns_subnone,
    sub_running_returns_every,
    sub_keyboard_returns_ondocumentkeydown,
    sub_counter_returns_subnone,
    sub_is_valid_sub_check,
    // HTML element coverage
    html_table_tr_td,
    html_table_thead_tbody,
    html_select_option,
    html_ol_li,
    html_h3,
    html_hr,
    html_br,
    html_a_element,
    html_nested_structure,
    html_semantic_elements,
    // Negative / edge cases
    neg_3tuple_not_elm_app,
    neg_5tuple_not_elm_app,
    neg_bare_string_not_html,
    neg_bare_int_not_html,
    neg_unknown_constructor_not_html,
  ],
);
