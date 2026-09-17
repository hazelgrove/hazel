open Alcotest;
open Language;
open Test_Evaluator_Prelude;
open IdTagged.FreshGrammar;

// == Helpers ==

// evaluate_direct: evaluate without re-elaboration (for applying Closures)
let evaluate_direct = (exp: Exp.t): Exp.t =>
  Evaluator.evaluate(~env=Builtins.env_init, exp) |> fst;

// Check if an expression is a function (Closure from evaluation)
let is_function = Haz3lcore.MvuShape.is_function;

// Extract 4-tuple components from evaluated MVU app
let extract_elm_app = (exp: Exp.t): option((Exp.t, Exp.t, Exp.t, Exp.t)) =>
  switch (Haz3lcore.MvuShape.strip_wrappers(exp).term) {
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
  switch (Haz3lcore.MvuShape.of_constructor(exp)) {
  | Some(("Text", body)) =>
    // Text requires a string argument
    switch (Haz3lcore.MvuShape.strip_wrappers(body).term) {
    | Atom(String(_)) => true
    | _ => false
    }
  | Some(("Int", body)) =>
    switch (Haz3lcore.MvuShape.strip_wrappers(body).term) {
    | Atom(Int(_)) => true
    | _ => false
    }
  | Some(("Float", body)) =>
    switch (Haz3lcore.MvuShape.strip_wrappers(body).term) {
    | Atom(Float(_)) => true
    | _ => false
    }
  | Some(("Bool", body)) =>
    switch (Haz3lcore.MvuShape.strip_wrappers(body).term) {
    | Atom(Bool(_)) => true
    | _ => false
    }
  | Some(("Br" | "Hr", _)) => true
  | Some(("Input" | "TextArea" | "Img" | "A", _)) => true
  // Remaining HTML constructors (per MvuShape's derived name set) are
  // container elements: check children are valid HTML
  | Some((name, body)) when Haz3lcore.MvuShape.is_html_constructor(name) =>
    switch (Haz3lcore.MvuShape.strip_wrappers(body).term) {
    | Tuple([_attrs, children]) =>
      switch (Haz3lcore.MvuShape.strip_wrappers(children).term) {
      | ListLit(items) => List.for_all(is_valid_html, items)
      | _ => false
      }
    | _ => false
    }
  | _ => false
  };
};

// Describe what's wrong with invalid HTML for error messages
let describe_html_issue = (exp: Exp.t): string => {
  switch (Haz3lcore.MvuShape.of_constructor(exp)) {
  | None =>
    "Not an Html.T constructor. Got: "
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
      switch (Haz3lcore.MvuShape.strip_wrappers(body).term) {
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
  switch (Haz3lcore.MvuShape.strip_wrappers(exp).term) {
  | Tuple([a, b]) =>
    Some((
      Haz3lcore.MvuShape.strip_wrappers(a),
      Haz3lcore.MvuShape.strip_wrappers(b),
    ))
  | _ => None
  };

// Assert that an expression is a specific constructor
let assert_constructor = (msg: string, expected_name: string, exp: Exp.t) =>
  switch (Haz3lcore.MvuShape.of_constructor(exp)) {
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
  switch (Haz3lcore.MvuShape.of_constructor(exp)) {
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
  switch (Haz3lcore.MvuShape.strip_wrappers(exp).term) {
  | Tuple(fields) =>
    let rec find = (
      fun
      | [] => None
      | [field, ...rest] =>
        switch (Haz3lcore.MvuShape.strip_wrappers(field).term) {
        | TupLabel({term: Label(label), _}, value) when label == name =>
          Some(Haz3lcore.MvuShape.strip_wrappers(value))
        | _ => find(rest)
        }
    );
    find(fields);
  | _ => None
  };
};

// ============================================================
// == MVU Counter Tests (bare-model update: update returns model,
// == not (model, cmd); the runtime falls back to CmdNone) ==
// ============================================================

let counter_program = {|
let update : (Int, Int) -> Int = fun (msg, model) -> model + msg in
let view : Int -> Html.T = fun model -> Html.div(
  [Attr.class("counter"), Attr.style([("text-align", "center"), ("padding", "20px")])],
  [
    Html.h2([], [Html.text("MVU Counter")]),
    Html.div(
      [Attr.style([("font-size", "48px"), ("margin", "20px")])],
      [Html.int(model)]
    ),
    Html.div(
      [],
      [
        Html.button([Attr.on_click(-1), Attr.style([("font-size", "24px")])], [Html.text("-")]),
        Html.button([Attr.on_click(1), Attr.style([("font-size", "24px")])], [Html.text("+")])
      ]
    )
  ]
) in
let subs : Int -> Sub.T = fun _model -> Sub.none in
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
    "Counter view(0) produces valid Html.T",
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
        let new_model = Haz3lcore.MvuShape.strip_wrappers(new_model);
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
        let new_model = Haz3lcore.MvuShape.strip_wrappers(new_model);
        check(dhexp_typ, "model should be -1", Exp.int(-1), new_model);
      | None => fail("Not an Elm app")
      };
    },
  );

let svg_tag_routing =
  test_case(
    "Node tag routing: SVG tags recognized, Html.T tags not",
    `Quick,
    () => {
      let is_svg = Haz3lcore.HazelDOM.is_svg_tag;
      List.iter(
        t => check(bool, t ++ " routes to SVG namespace", true, is_svg(t)),
        ["svg", "circle", "path", "text", "g", "linearGradient"],
      );
      List.iter(
        t => check(bool, t ++ " stays Html.T", false, is_svg(t)),
        ["div", "span", "a", "script", "style", "canvas"],
      );
    },
  );

let counter_view_after_update =
  test_case(
    "Counter view(update(1, 0)) produces valid Html.T",
    `Quick,
    () => {
      let result = parse_and_evaluate(counter_program);
      switch (extract_elm_app(result)) {
      | Some((init_model, update_fn, view_fn, _)) =>
        let new_model =
          apply(update_fn, Exp.tuple([Exp.int(1), init_model]));
        let new_model = Haz3lcore.MvuShape.strip_wrappers(new_model);
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
          model := Haz3lcore.MvuShape.strip_wrappers(new_model);
        };
        check(dhexp_typ, "after 5 increments", Exp.int(5), model^);
        // 2 decrements
        for (_ in 1 to 2) {
          let new_model =
            apply(update_fn, Exp.tuple([Exp.int(-1), model^]));
          model := Haz3lcore.MvuShape.strip_wrappers(new_model);
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
let view : (Int, Int) -> Html.T = fun model ->
  let x = fst(model) in
  let y = snd(model) in
  Html.div(
    [Attr.id("game"), Attr.style([("width", "400px"), ("height", "400px"), ("position", "relative")])],
    [
      Html.div([Attr.style([("text-align", "center")])], [Html.text("Use arrow keys")]),
      Html.div(
        [Attr.style([("width", "40px"), ("height", "40px"), ("position", "absolute"),
                ("left", string_of_int(x) ++ "px"), ("top", string_of_int(y) ++ "px")])],
        []
      ),
      Html.div(
        [Attr.style([("position", "absolute"), ("bottom", "10px")])],
        [Html.text("Position: (" ++ string_of_int(x) ++ ", " ++ string_of_int(y) ++ ")")]
      )
    ]
  )
in
let subs : (Int, Int) -> Sub.T = fun _model ->
  Sub.on_document_key_down(fun (key, _code, _ctrl, _shift, _alt, _meta) -> key)
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
    "Keyboard view((180,180)) produces valid Html.T",
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
        let new_model = Haz3lcore.MvuShape.strip_wrappers(new_model);
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
    "Keyboard view after ArrowRight produces valid Html.T",
    `Quick,
    () => {
      let result = parse_and_evaluate(keyboard_game_program);
      switch (extract_elm_app(result)) {
      | Some((init_model, update_fn, view_fn, _)) =>
        let msg = Exp.string("ArrowRight");
        let new_model = apply(update_fn, Exp.tuple([msg, init_model]));
        let new_model = Haz3lcore.MvuShape.strip_wrappers(new_model);
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
      let result = Haz3lcore.MvuShape.strip_wrappers(wrapped);
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
      let result = Haz3lcore.MvuShape.strip_wrappers(wrapped);
      check(dhexp_typ, "should strip Parens", inner, result);
    },
  );

let of_constructor_basic =
  test_case(
    "of_constructor on Html.text(\"hello\")",
    `Quick,
    () => {
      let exp = parse_and_evaluate({|Html.text("hello")|});
      switch (Haz3lcore.MvuShape.of_constructor(exp)) {
      | Some(("Text", _)) => ()
      | Some((name, _)) => fail("Expected Text constructor, got: " ++ name)
      | None => fail("of_constructor returned None")
      };
    },
  );

let of_constructor_nested =
  test_case(
    "of_constructor on Html.div([], [Html.text(\"hi\")])",
    `Quick,
    () => {
      let exp = parse_and_evaluate({|Html.div([], [Html.text("hi")])|});
      switch (Haz3lcore.MvuShape.of_constructor(exp)) {
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
  | Inc => ((count=model.count + 1, label=model.label), Cmd.none)
  | Dec => ((count=model.count - 1, label=model.label), Cmd.none)
  | SetTo(n) => ((count=n, label=model.label), Cmd.none)
  end
in
let view = fun model ->
  Html.div([], [
    Html.h2([], [Html.text(model.label)]),
    Html.p([], [Html.int(model.count)]),
    Html.button([Attr.on_click(Inc)], [Html.text("+")]),
    Html.button([Attr.on_click(Dec)], [Html.text("-")])
  ])
in
let subs = fun _model -> Sub.none in
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
    "Real MVU update returns Cmd.none",
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
    "Real MVU view(init) produces valid Html.T",
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
  | Save => ((saved=true, msg=model.msg), Cmd.none)
  | Notify(text) => ((saved=model.saved, msg=text), Cmd.log(text))
  | Multi => ((saved=true, msg="done"), Cmd.batch([Cmd.log("a"), Cmd.log("b")]))
  | FocusInput => (model, Cmd.focus("my-input"))
  end
in
let view = fun model ->
  Html.div([], [Html.text(if model.saved then "saved" else "unsaved")])
in
let subs = fun _model -> Sub.none in
(init, update, view, subs)
|};

let cmd_save_returns_cmdnone =
  test_case(
    "Cmd.T: Save returns Cmd.none",
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
    "Cmd.T: Notify returns Log",
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
    "Cmd.T: Multi returns CmdBatch",
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
    "Cmd.T: CmdBatch body is a list of commands",
    `Quick,
    () => {
      let result = parse_and_evaluate(cmd_program);
      switch (extract_elm_app(result)) {
      | Some((init_model, update_fn, _, _)) =>
        let multi_msg = Exp.constructor("Multi", None);
        let (_, cmd) = apply_update(update_fn, multi_msg, init_model);
        switch (Haz3lcore.MvuShape.of_constructor(cmd)) {
        | Some(("CmdBatch", body)) =>
          switch (Haz3lcore.MvuShape.strip_wrappers(body).term) {
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
    "Cmd.T: FocusInput returns Focus",
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
    "Cmd.T: view valid after command-producing update",
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
  | Tick => ((count=model.count + 1, running=model.running), Cmd.none)
  | Toggle => ((count=model.count, running=if model.running then false else true), Cmd.none)
  end
in
let view = fun model ->
  Html.div([], [Html.text(string_of_int(model.count))])
in
let subs = fun model ->
  if model.running then Sub.every(1000.0, fun _ts -> Tick) else Sub.none
in
(init, update, view, subs)
|};

let sub_stopped_returns_subnone =
  test_case(
    "Sub.T: stopped model returns Sub.none",
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
    "Sub.T: running model returns Every",
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
    "Sub.T: keyboard game returns OnDocumentKeyDown",
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
    "Sub.T: counter returns Sub.none",
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
    "Sub.T: is_valid_sub recognizes Sub.T constructors",
    `Quick,
    () => {
      // SubNone
      let result = parse_and_evaluate(counter_program);
      switch (extract_elm_app(result)) {
      | Some((init_model, _, _, subs_fn)) =>
        let sub = apply(subs_fn, init_model);
        check(
          Alcotest.bool,
          "Sub.none is valid sub",
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
    "Html.T: Table > Tr > Td is valid",
    `Quick,
    () => {
      let exp =
        parse_and_evaluate(
          {|Html.table([], [Html.tr([], [Html.td([], [Html.text("cell")])])])|},
        );
      assert_valid_html("Table/Tr/Td", exp);
    },
  );

let html_table_thead_tbody =
  test_case(
    "Html.T: Table with Thead and Tbody is valid",
    `Quick,
    () => {
      let exp =
        parse_and_evaluate(
          {|Html.table([], [Html.thead([], [Html.tr([], [Html.th([], [Html.text("Header")])])]), Html.tbody([], [Html.tr([], [Html.td([], [Html.text("Data")])])])])|},
        );
      assert_valid_html("Table/Thead/Tbody", exp);
    },
  );

let html_select_option =
  test_case(
    "Html.T: Select > Option is valid",
    `Quick,
    () => {
      let exp =
        parse_and_evaluate(
          {|Html.select([], [Html.option([], [Html.text("A")]), Html.option([], [Html.text("B")])])|},
        );
      assert_valid_html("Select/Option", exp);
    },
  );

let html_ol_li =
  test_case(
    "Html.T: Ol > Li is valid",
    `Quick,
    () => {
      let exp =
        parse_and_evaluate(
          {|Html.ol([], [Html.li([], [Html.text("one")]), Html.li([], [Html.text("two")])])|},
        );
      assert_valid_html("Ol/Li", exp);
    },
  );

let html_h3 =
  test_case(
    "Html.T: H3 is valid",
    `Quick,
    () => {
      let exp = parse_and_evaluate({|Html.h3([], [Html.text("heading")])|});
      assert_valid_html("H3", exp);
    },
  );

let html_hr =
  test_case(
    "Html.T: Html.hr is valid",
    `Quick,
    () => {
      let exp = parse_and_evaluate({|Html.hr|});
      assert_valid_html("Hr", exp);
    },
  );

let html_br =
  test_case(
    "Html.T: Html.br is valid",
    `Quick,
    () => {
      let exp = parse_and_evaluate({|Html.br|});
      assert_valid_html("Br", exp);
    },
  );

let html_a_element =
  test_case(
    "Html.T: A element is valid",
    `Quick,
    () => {
      let exp =
        parse_and_evaluate(
          {|Html.a([Attr.href("https://example.com")], [Html.text("link")])|},
        );
      assert_valid_html("A", exp);
    },
  );

let html_nested_structure =
  test_case(
    "Html.T: deeply nested structure is valid",
    `Quick,
    () => {
      let exp =
        parse_and_evaluate(
          {|Html.div([], [
            Html.ul([], [
              Html.li([], [Html.span([], [Html.text("item 1")])]),
              Html.li([], [Html.span([], [Html.text("item 2")])])
            ]),
            Html.p([], [Html.text("paragraph")])
          ])|},
        );
      assert_valid_html("nested structure", exp);
    },
  );

let html_semantic_elements =
  test_case(
    "Html.T: semantic elements are valid",
    `Quick,
    () => {
      let exp =
        parse_and_evaluate(
          {|Html.div([], [
            Html.header([], [Html.text("header")]),
            Html.nav([], [Html.text("nav")]),
            Html.main([], [
              Html.section([], [Html.text("section")]),
              Html.article([], [Html.text("article")])
            ]),
            Html.footer([], [Html.text("footer")])
          ])|},
        );
      assert_valid_html("semantic elements", exp);
    },
  );

// ============================================================
// == Syntax-Commit Msg Tests ==
// == Inline projector: update = apply. A msg is an Html -> Html ==
// == transform; committing evaluates msg(model). ==
// ============================================================

// Simple events: the handler IS the msg.
let syntax_commit_simple_msg =
  test_case(
    "Syntax commit: simple handler is an Html -> Html msg",
    `Quick,
    () => {
      let msg =
        parse_and_evaluate(
          {|fun m -> Html.div([], [m, Html.text("clicked")])|},
        );
      let model = parse_and_evaluate({|Html.span([], [Html.text("hi")])|});
      let new_html = apply(msg, model);
      assert_valid_html("msg(model)", new_html);
      assert_constructor("msg(model)", "Div", new_html);
    },
  );

// Payload events wrap the handler shape (model, payload) -> model into a
// msg: fun m -> handler((m, payload)) (HazelDOM.payload_transform).
let syntax_commit_payload_msg =
  test_case(
    "Syntax commit: payload msg = fun m -> handler((m, payload))",
    `Quick,
    () => {
      let handler =
        parse_and_evaluate({|fun (m, s) -> Html.div([], [Html.text(s)])|});
      let msg =
        Haz3lcore.HazelDOM.payload_transform(handler, Exp.string("typed"));
      let model = parse_and_evaluate({|Html.div([], [Html.text("old")])|});
      let new_html = apply(msg, model);
      assert_valid_html("payload msg applied", new_html);
      // The payload must have reached the handler: child is Text("typed")
      let text_child =
        switch (Haz3lcore.MvuShape.of_constructor(new_html)) {
        | Some(("Div", body)) =>
          switch (Haz3lcore.MvuShape.of_tuple(body)) {
          | Some([_attrs, children]) =>
            switch (Haz3lcore.MvuShape.of_list(children)) {
            | Some([child]) =>
              switch (Haz3lcore.MvuShape.of_constructor(child)) {
              | Some(("Text", text_body)) =>
                Haz3lcore.MvuShape.of_string(text_body)
              | _ => None
              }
            | _ => None
            }
          | _ => None
          }
        | _ => None
        };
      check(
        Alcotest.option(Alcotest.string),
        "payload reached the transform",
        Some("typed"),
        text_child,
      );
    },
  );

// A msg may also produce (Html, Cmd); the commit splices the Html and runs
// the Cmd.
let syntax_commit_html_cmd_result =
  test_case(
    "Syntax commit: msg producing (Html, Cmd.T)",
    `Quick,
    () => {
      let msg =
        parse_and_evaluate(
          {|fun m -> (Html.div([], [m]), Cmd.log("committed"))|},
        );
      let model = parse_and_evaluate({|Html.text("x")|});
      let result = apply(msg, model);
      switch (extract_pair(result)) {
      | Some((html, cmd)) =>
        assert_valid_html("html half", html);
        assert_constructor("cmd half", "Log", cmd);
      | None => fail("msg(model) should be an (Html, Cmd.T) pair")
      };
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
          {|(0, fun (m, a) -> m + a, fun m -> Html.div([], [Html.text("hi")]))|},
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
          {|(0, fun (m, a) -> m, fun m -> Html.div([], []), fun m -> Sub.none, 99)|},
        );
      switch (extract_elm_app(result)) {
      | Some(_) => fail("5-tuple should not be detected as Elm app")
      | None => ()
      };
    },
  );

// == Labeled app tuples ==
// detect_app_kind also accepts (init=…, update=…, view=…, subs=…), matched
// by name rather than by position.

let labeled_app_program = (fields: string) =>
  {|
let update(msg, model) = model + msg in
let view(model) = Html.div([], [Html.int(model)]) in
let subs(_model) = Sub.none in
|}
  ++ fields;

let check_labeled_app = (what: string, fields: string) => {
  let result = parse_and_evaluate(labeled_app_program(fields));
  switch (Haz3lcore.MvuShape.detect_app_kind(result)) {
  | Some(ElmApp(init_model, _, _, _)) =>
    switch (Haz3lcore.MvuShape.strip_wrappers(init_model).term) {
    | Atom(Int(n)) when Bigint.to_int(n) == Some(0) => ()
    | _ => fail(what ++ ": init should be the value labeled `init`")
    }
  | None => fail(what ++ " should detect as Elm app")
  };
};

let labeled_app_detected =
  test_case("Labeled app tuple detects as Elm app", `Quick, () =>
    check_labeled_app(
      "Labeled app",
      {|(init=0, update=update, view=view, subs=subs)|},
    )
  );

let labeled_app_permuted_detected =
  test_case("Permuted labeled app tuple detects as Elm app", `Quick, () =>
    check_labeled_app(
      "Permuted labeled app",
      {|(view=view, subs=subs, init=0, update=update)|},
    )
  );

let neg_bare_string_not_html =
  test_case("Neg: bare string is not valid Html.T", `Quick, () => {
    check(
      Alcotest.bool,
      "bare string not Html.T",
      false,
      is_valid_html(Exp.string("hello")),
    )
  });

let neg_bare_int_not_html =
  test_case("Neg: bare int is not valid Html.T", `Quick, () => {
    check(
      Alcotest.bool,
      "bare int not Html.T",
      false,
      is_valid_html(Exp.int(42)),
    )
  });

let neg_unknown_constructor_not_html =
  test_case(
    "Neg: unknown constructor is not valid Html.T",
    `Quick,
    () => {
      let exp = parse_and_evaluate({|FakeElement([], [Html.text("hi")])|});
      check(
        Alcotest.bool,
        "FakeElement not Html.T",
        false,
        is_valid_html(exp),
      );
    },
  );

// ============================================================
// == Projector dynamics ==
// == HTMLProj decides which commit mode to use from the live
// == value of the syntax it replaces, which it gets by asking
// == for dynamics (Projector.dynamics -> a probe on its id).
// ============================================================

let projector_dynamics_records_a_sample =
  test_case(
    "A dynamics projector's id gets a sample of its expression's value",
    `Quick,
    () => {
      // Wrap an expression in a Projector node, as MakeTerm does for a
      // projected piece; the projector's id is the node's id.
      let inner = parse_exp("Html.div([], [Html.text(\"hi\")])");
      let term: Grammar.exp_term(IdTagged.IdTag.t) =
        Projector(
          {
            kind: ProjectorKind.HTML,
            model: "",
          },
          inner,
        );
      let projected: Exp.t = IdTagged.fresh(term);
      let id = IdTagged.rep_id(projected);
      let probe_ids = Id.Map.singleton(id, ());
      let settings = CoreSettings.on;
      let (info_map, elaborated) =
        Statics.mk(
          ~probe_ids,
          settings,
          Builtins.ctx_init(Some(Int)),
          projected,
        );
      let targets =
        Haz3lcore.CachedStatics.compute_targets(
          ~settings,
          ~info_map,
          ~probe_ids,
        );
      let (_, state) =
        Evaluator.evaluate(
          ~eval_info=EvalInfo.of_targets(targets),
          ~env=Builtins.env_init,
          elaborated,
        );
      switch (
        Sample.Map.lookup(id, EvaluatorState.get_probes(state))
        |> Option.value(~default=[])
      ) {
      | [] => fail("no sample recorded at the projector's id")
      | [sample, ..._] =>
        check(
          Alcotest.bool,
          "sample value is the evaluated Html.T",
          true,
          Haz3lcore.MvuShape.is_html(sample.value),
        )
      };
    },
  );

let projector_dynamics_flag_selects_probe_ids =
  test_case(
    "Only projectors asking for dynamics become probe targets",
    `Quick,
    () => {
      let piece: Haz3lcore.Base.piece =
        Grout({
          id: Id.mk(),
          shape: Convex,
        });
      let mk = kind => {
        let id = Id.mk();
        (id, Haz3lcore.ProjectorCore.mk(~id, kind, piece, ""));
      };
      let (html_id, html) = mk(ProjectorKind.HTML);
      let (fold_id, fold) = mk(ProjectorKind.Fold);
      let ids =
        Haz3lcore.CachedStatics.projector_probe_ids(
          Id.Map.of_list([(html_id, html), (fold_id, fold)]),
        );
      check(
        Alcotest.bool,
        "html projector probed",
        true,
        Id.Map.mem(html_id, ids),
      );
      check(
        Alcotest.bool,
        "fold projector not probed",
        false,
        Id.Map.mem(fold_id, ids),
      );
    },
  );

// ============================================================
// == Checkpoints ==
// == An app model is persisted in the projector's model unless it
// == carries a captured environment, and restored only if the
// == current view still renders it.
// ============================================================

// True if any subterm satisfies `pred`
let contains = (pred: Exp.t => bool, d: Exp.t): bool => {
  let found = ref(false);
  let f_exp = (continue, e: Exp.t) => {
    if (pred(e)) {
      found := true;
    };
    continue(e);
  };
  let _ = Language.Exp.map_term(~f_exp, d);
  found^;
};

/* Round-trip evidence: serializes, parses back to an equal term whose own
   serialization is byte-identical, and still evaluates. */
let assert_roundtrips = (label: string, model: Exp.t) =>
  switch (Haz3lcore.MvuShape.serialize_model(model)) {
  | None => fail(label ++ ": should be checkpointable")
  | Some(s) =>
    switch (Haz3lcore.MvuShape.deserialize_model(s)) {
    | None => fail(label ++ ": should deserialize")
    | Some(restored) =>
      check(dhexp_typ, label ++ ": round-trip equal", model, restored);
      switch (Haz3lcore.MvuShape.serialize_model(restored)) {
      | None => fail(label ++ ": restored term should re-serialize")
      | Some(s') => check(Alcotest.string, label ++ ": stable sexp", s, s')
      };
      switch (Haz3lcore.MvuShape.safe_evaluate(restored)) {
      | Ok(_) => ()
      | Error(m) => fail(label ++ ": restored term should evaluate: " ++ m)
      };
    }
  };

let checkpoint_program = {|
let init = (count=1, label="hi") in
let update = fun (msg, model) -> (model, Cmd.none) in
let view = fun model -> Html.div([], [Html.text(model.label), Html.int(model.count)]) in
let subs = fun _model -> Sub.none in
(init, update, view, subs)
|};

// Same app, but the view now expects a constructor model
let checkpoint_program_v2 = {|
type Shape = + Circle + Square in
let init : Shape = Circle in
let update = fun (msg, model) -> (model, Cmd.none) in
let view = fun model -> case model | Circle => Html.div([], [Html.text("circle")]) end in
let subs = fun _model -> Sub.none in
(init, update, view, subs)
|};

// A model that carries a function; the function is closed after evaluation
let function_model_program = {|
let init = (count=1, fmt=fun x -> x) in
let update = fun (msg, model) -> (model, Cmd.none) in
let view = fun model -> Html.div([], [Html.int(model.count)]) in
let subs = fun _model -> Sub.none in
(init, update, view, subs)
|};

let app_of = (program: string) =>
  switch (extract_elm_app(parse_and_evaluate(program))) {
  | Some(app) => app
  | None => failwith("test program is not an Elm app")
  };

let checkpoint_plain_model_is_checkpointable =
  test_case(
    "Plain model is checkpointable and serializes",
    `Quick,
    () => {
      let (init_model, _, _, _) = app_of(checkpoint_program);
      check(
        Alcotest.bool,
        "checkpointable",
        true,
        Haz3lcore.MvuShape.is_checkpointable(init_model),
      );
      check(
        Alcotest.bool,
        "serializes",
        true,
        Option.is_some(Haz3lcore.MvuShape.serialize_model(init_model)),
      );
    },
  );

let checkpoint_function_model_accepted =
  test_case(
    "Model carrying a function is checkpointed and restores",
    `Quick,
    () => {
      let (init_model, _, view_fn, _) = app_of(function_model_program);
      assert_roundtrips("function model", init_model);
      let s = Option.get(Haz3lcore.MvuShape.serialize_model(init_model));
      switch (Haz3lcore.MvuShape.restore_model(~view_fn, s)) {
      | None => fail("checkpoint should be restored")
      | Some((model, html)) =>
        check(dhexp_typ, "restored model", init_model, model);
        assert_valid_html("view(restored)", html);
      };
    },
  );

let checkpoint_bare_lambda_accepted =
  test_case("Bare lambda is checkpointable", `Quick, () =>
    check(
      Alcotest.bool,
      "lambda accepted",
      true,
      Haz3lcore.MvuShape.is_checkpointable(
        Exp.fn(Pat.var("x"), Exp.var("x"), None, None),
      ),
    )
  );

/* One per function form a model can legitimately hold: each is closed
   after evaluation (Evaluator.evaluate substitutes environments away), so
   each is ordinary syntax that survives the sexp round trip. */

let checkpoint_form_roundtrip = (label, program, present: Exp.t => bool) =>
  test_case(
    "Checkpoint round-trips a model holding a " ++ label,
    `Quick,
    () => {
      let model = parse_and_evaluate(program);
      check(
        Alcotest.bool,
        label ++ " present after evaluation",
        true,
        contains(present, model),
      );
      assert_roundtrips(label, model);
    },
  );

let checkpoint_closed_fun_roundtrip =
  test_case(
    "Checkpoint round-trips a model holding a closed Fun",
    `Quick,
    () => {
      // `n` is substituted in, so nothing in the value refers to it
      let model =
        parse_and_evaluate({|let n = 5 in let f = fun x -> x + n in (f, 1)|});
      check(
        Alcotest.bool,
        "Fun present",
        true,
        contains(
          e =>
            switch (e.term) {
            | Fun(_) => true
            | _ => false
            },
          model,
        ),
      );
      check(
        Alcotest.bool,
        "no free n",
        false,
        contains(
          e =>
            switch (e.term) {
            | Var("n") => true
            | _ => false
            },
          model,
        ),
      );
      assert_roundtrips("closed Fun", model);
    },
  );

let checkpoint_typfun_roundtrip =
  checkpoint_form_roundtrip("TypFun", {|(typfun a -> fun (x : a) -> x, 1)|}, e =>
    switch (e.term) {
    | TypFun(_) => true
    | _ => false
    }
  );

let checkpoint_fixf_roundtrip =
  checkpoint_form_roundtrip(
    "FixF", {|let f = fun x -> if x == 0 then 0 else f(x - 1) in (f, 1)|}, e =>
    switch (e.term) {
    | FixF(_, _, None) => true
    | _ => false
    }
  );

let checkpoint_builtin_fun_roundtrip =
  checkpoint_form_roundtrip("BuiltinFun", {|(abs, 1)|}, e =>
    switch (e.term) {
    | BuiltinFun(_) => true
    | _ => false
    }
  );

/* Environments are what the guard actually rejects. Evaluator.evaluate
   never returns one (its INVARIANT), so this is defensive. */
let checkpoint_captured_env_rejected =
  test_case(
    "Terms carrying an environment are not checkpointed",
    `Quick,
    () => {
      let env = Environment.of_list([("y", Exp.int(3))]);
      let body = Exp.fn(Pat.var("x"), Exp.var("y"), None, None);
      List.iter(
        ((label, e)) => {
          check(
            Alcotest.bool,
            label ++ ": not checkpointable",
            false,
            Haz3lcore.MvuShape.is_checkpointable(e),
          );
          check(
            Alcotest.bool,
            label ++ ": no checkpoint",
            true,
            Haz3lcore.MvuShape.serialize_model(e) == None,
          );
        },
        [
          ("Closure", Exp.closure(env, body)),
          ("FixF with env", Exp.fix_f(Pat.var("f"), body, Some(env))),
          (
            "nested Closure",
            Exp.tuple([Exp.int(1), Exp.closure(env, body)]),
          ),
        ],
      );
    },
  );

let checkpoint_roundtrip =
  test_case(
    "Checkpoint round-trips",
    `Quick,
    () => {
      let (init_model, _, _, _) = app_of(checkpoint_program);
      switch (Haz3lcore.MvuShape.serialize_model(init_model)) {
      | None => fail("model should be serializable")
      | Some(s) =>
        switch (Haz3lcore.MvuShape.deserialize_model(s)) {
        | None => fail("checkpoint should deserialize")
        | Some(restored) =>
          check(dhexp_typ, "round-trip equal", init_model, restored)
        }
      };
    },
  );

let checkpoint_restore_accepted =
  test_case(
    "Checkpoint restores under the same view",
    `Quick,
    () => {
      let (init_model, _, view_fn, _) = app_of(checkpoint_program);
      let s = Option.get(Haz3lcore.MvuShape.serialize_model(init_model));
      switch (Haz3lcore.MvuShape.restore_model(~view_fn, s)) {
      | None => fail("checkpoint should be restored")
      | Some((model, html)) =>
        check(dhexp_typ, "restored model", init_model, model);
        assert_valid_html("view(restored)", html);
      };
    },
  );

let checkpoint_restore_rejected_by_changed_view =
  test_case(
    "Checkpoint incompatible with a changed view is discarded",
    `Quick,
    () => {
      let (init_model, _, _, _) = app_of(checkpoint_program);
      let (_, _, view_fn_v2, _) = app_of(checkpoint_program_v2);
      let s = Option.get(Haz3lcore.MvuShape.serialize_model(init_model));
      check(
        Alcotest.bool,
        "discarded",
        true,
        Haz3lcore.MvuShape.restore_model(~view_fn=view_fn_v2, s) == None,
      );
    },
  );

let checkpoint_restore_rejected_when_unreadable =
  test_case(
    "Unreadable checkpoint is discarded",
    `Quick,
    () => {
      let (_, _, view_fn, _) = app_of(checkpoint_program);
      check(
        Alcotest.bool,
        "discarded",
        true,
        Haz3lcore.MvuShape.restore_model(~view_fn, "(not a model") == None,
      );
    },
  );

// ============================================================
// == All tests ==
// ============================================================

let tests = (
  "MVU",
  [
    // Counter (bare-model update pattern)
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
    svg_tag_routing,
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
    // Syntax-commit msgs (inline projector: update = apply)
    syntax_commit_simple_msg,
    syntax_commit_payload_msg,
    syntax_commit_html_cmd_result,
    // Projector dynamics
    projector_dynamics_records_a_sample,
    projector_dynamics_flag_selects_probe_ids,
    // Checkpoints
    checkpoint_plain_model_is_checkpointable,
    checkpoint_function_model_accepted,
    checkpoint_bare_lambda_accepted,
    checkpoint_closed_fun_roundtrip,
    checkpoint_typfun_roundtrip,
    checkpoint_fixf_roundtrip,
    checkpoint_builtin_fun_roundtrip,
    checkpoint_captured_env_rejected,
    checkpoint_roundtrip,
    checkpoint_restore_accepted,
    checkpoint_restore_rejected_by_changed_view,
    checkpoint_restore_rejected_when_unreadable,
    // Labeled app tuples
    labeled_app_detected,
    labeled_app_permuted_detected,
    // Negative / edge cases
    neg_3tuple_not_elm_app,
    neg_5tuple_not_elm_app,
    neg_bare_string_not_html,
    neg_bare_int_not_html,
    neg_unknown_constructor_not_html,
  ],
);
