open Alcotest;
open Haz3lcore;
open Test_Editing;
module Fresh = Language.IdTagged.FreshGrammar;
open Utils_AssistantTests;

let test =
    (~name, ~init: string, ~acts: list(CompositionTools.action), ~goal)
    : test_case(_) => {
  let acts =
    List.map(a => Action.Composition(CompositionActions.default(a)), acts);
  test_case(name, `Quick, () =>
    check(
      testable(Fmt.string, String.equal),
      goal,
      goal,
      acts |> apply_actions(init) |> printer,
    )
  );
};

/* =============================== */
/* |||| EDIT TOOL TESTS |||| */
/* =============================== */
// - UpdateDefinition(string)
// - UpdateBody(string)
// - UpdatePattern(string)
// - UpdateBindingClause(string)
// - DeleteExpression
// - DeleteBody
// - InsertAfter(string)
// - InsertBefore(string)

let update_definition_tests = {
  [
    test(
      ~name="Update Definition (\"Simplest\" Case)",
      ~init="let x = 1¦ in x",
      ~acts=[Edit(UpdateDefinition(LLM("2")))],
      ~goal="§let x = 2 in¦ x",
    ),
    test(
      ~name="Update Definition (Simple - V2)",
      ~init={|let x = (1, 2)¦ in x|},
      ~acts=[Edit(UpdateDefinition(LLM("(0, 2, 4)")))],
      ~goal={|§let x = (0, 2, 4) in¦ x|},
    ),
  ];
};

let update_body_tests = {
  [
    test(
      ~name="Update Body (\"Simplest\" Case)",
      ~init="let x = 1 in¦ x + 2",
      ~acts=[Edit(UpdateBody(LLM("x * 2")))],
      ~goal="§let x = 1 in¦ x * 2",
    ),
  ];
};

let update_pattern_tests = {
  [
    // little note:
    // Would be nice to handle on our backend, the renaming of all use sites of the pattern as well
    test(
      ~name="Update Pattern (\"Simplest\" Case)",
      ~init="let x = 1 in¦ x",
      ~acts=[Edit(UpdatePattern(LLM("a")))],
      ~goal="§let a = 1 in¦ a",
    ),
    test(
      ~name=
        "Update Pattern (Variable Renaming Case (Cursor should relocate back to original pattern))",
      ~init="let x = 1 in¦ let y = 2 in x + y",
      ~acts=[Edit(UpdatePattern(LLM("a")))],
      ~goal="§let a = 1 in¦ let y = 2 in a + y",
    ),
    test(
      ~name="Update Pattern (Tuple)",
      ~init="let (a, b) = ? in¦ ?",
      ~acts=[Edit(UpdatePattern(LLM("(x, y)")))],
      ~goal="§let (x, y) = ? in¦ ?",
    ),
    test(
      ~name="Update Pattern (Annotated Atomic Type)",
      ~init="let u : Int = ? in¦ ?",
      ~acts=[Edit(UpdatePattern(LLM("u : Float")))],
      ~goal="§let u : Float = ? in¦ ?",
    ),
    test(
      ~name="Update Pattern (Annotated Arrow Type)",
      ~init="let u : Int -> Int = ? in¦ ?",
      ~acts=[Edit(UpdatePattern(LLM("u : Float -> Float")))],
      ~goal="§let u : Float -> Float = ? in¦ ?",
    ),
    test(
      ~name="Update Pattern (Annotated Arrow Type 2 (Remove Annotation))",
      ~init="let u : Int -> Int = ? in¦ ?",
      ~acts=[Edit(UpdatePattern(LLM("u")))],
      ~goal="§let u = ? in¦ ?",
    ),
    test(
      ~name="Update Pattern (Annotated Arrow Type 3 (Introduce Annotation))",
      ~init="let u = ? in¦ ?",
      ~acts=[Edit(UpdatePattern(LLM("u : Int -> Int")))],
      ~goal="§let u : Int -> Int = ? in¦ ?",
    ),
  ];
};

let update_binding_clause_tests = {
  [
    test(
      ~name="Update Binding Clause (\"Simplest\" Case)",
      ~init="let x = 1¦ in x",
      ~acts=[Edit(UpdateBindingClause(LLM("let x = 2 in")))],
      ~goal="§let x = 2 in¦ x",
    ),
    test(
      ~name=
        "Update Binding Clause (Multiple Bindings Case (Cursor should go to tail binding))",
      ~init="let x = 1¦ in x",
      ~acts=[Edit(UpdateBindingClause(LLM("let x = 2 in let y = 3 in")))],
      ~goal="let x = 2 in §let y = 3 in¦ x",
    ),
  ];
};

let delete_binding_clause_tests = {
  [
    test(
      ~name="Delete Binding Clause (\"Simplest\" Case)",
      ~init="let a = 1¦ in let b = 2 in a + b",
      ~acts=[Edit(DeleteBindingClause)],
      ~goal=" §let b = 2 in¦ a + b" // todo: Update this action to handle the extra space that is now present
    ),
  ];
};

let delete_body_tests = {
  [
    test(
      ~name="Delete Body (\"Simplest\" Case)",
      ~init="let a = 1¦ in a * 5",
      ~acts=[Edit(DeleteBody)],
      ~goal="§let a = 1 in¦ ?",
    ),
    // ================================
    // Special Cases: Final Body and Empty Hole(s)
    test(
      ~name="Delete Body (Two Bindings (Final Body Empty Hole)))",
      ~init="let a = 0¦ in let b = 1 in ",
      ~acts=[Edit(DeleteBody)],
      ~goal="§let a = 0 in¦ ?",
    ),
    test(
      ~name="Delete Body (Two Bindings (Final Body Non-Empty)))",
      ~init="let a = 1¦ in let b = 2 in a + b",
      ~acts=[Edit(DeleteBody)],
      ~goal="§let a = 1 in¦ ?",
    ),
    test(
      ~name=
        "Delete Body (Multiple Bindings (Final Body's Expression Contains an Empty Hole)))",
      ~init="let a = -1¦ in let b = 0 in let c = 1 in a + ",
      ~acts=[Edit(DeleteBody)],
      ~goal="§let a = -1 in¦ ?",
    ),
    // Below is failing as select with defs_exclude_bodies=false is not working when
    // final body token is an implicit hole.
    test(
      ~name=
        "Delete Body (Multiple Bindings (Final Body Expression is an Explicit Hole)))",
      ~init="let a = 10¦ in let b = 11 in let c = 12 in ?",
      ~acts=[Edit(DeleteBody)],
      ~goal="§let a = 10 in¦ ?",
    ),
    test(
      ~name=
        "Delete Body (Multiple Bindings (Final Body Expression is an Implicit Empty Hole)))",
      ~init="let a = 0¦ in let b = 1 in let c = 2 in ",
      ~acts=[Edit(DeleteBody)],
      ~goal="§let a = 0 in¦ ?",
    ),
    test(
      ~name=
        "Delete Body (Multiple Bindings (Final Body Completely Non-Empty)))",
      ~init="let a = 1¦ in let b = 2 in let c = 3 in a + b + c",
      ~acts=[Edit(DeleteBody)],
      ~goal="§let a = 1 in¦ ?",
    ),
    // End Special Cases: Final Body and Empty Hole(s)
    // ================================
  ];
};

let insert_before_tests = {
  [
    test(
      ~name="Insert Before (\"Simplest\" Case)",
      ~init="let b = 2¦ in a * b",
      ~acts=[Edit(InsertBefore(LLM("let a = 1 in")))],
      ~goal="§let a = 1 in¦ let b = 2 in a * b",
    ),
    test(
      ~name="Insert Before (Between Two Bindings Case)",
      ~init="let a = 1 in let c = 3 in¦ a * c",
      ~acts=[Edit(InsertBefore(LLM("let b = 2 in")))],
      ~goal="let a = 1 in §let b = 2 in¦ let c = 3 in a * c",
    ),
    test(
      ~name="Insert Before (Multiple Bindings Case)",
      ~init="let b = 2¦ in a * b",
      ~acts=[Edit(InsertBefore(LLM("let a = 1 in let c = 3 in")))],
      ~goal="let a = 1 in §let c = 3 in¦ let b = 2 in a * b",
    ),
  ];
};

let insert_after_tests = {
  [
    test(
      ~name="Insert After (\"Simplest\" Case)",
      ~init="let a = 1¦ in a",
      ~acts=[Edit(InsertAfter(LLM("let b = 2 in")))],
      ~goal="let a = 1 in §let b = 2 in¦ a",
    ),
    test(
      ~name="Insert After (Between Two Bindings)",
      ~init="let a = 10¦ in let c = 30 in a + c",
      ~acts=[Edit(InsertAfter(LLM("let b = 20 in")))],
      ~goal="let a = 10 in §let b = 20 in¦ let c = 30 in a + c",
    ),
    test(
      ~name="Insert After (Multiple Bindings Case)",
      ~init="let a = 0¦ in a",
      ~acts=[Edit(InsertAfter(LLM("let b = 10 in let c = 100 in")))],
      ~goal="let a = 0 in let b = 10 in §let c = 100 in¦ a",
    ),
  ];
};

let edit_tests =
  update_definition_tests
  @ update_body_tests
  @ update_pattern_tests  // todo: (fix) relocate cursor back to pattern after variable renaming
  @ update_binding_clause_tests
  @ delete_binding_clause_tests
  @ delete_body_tests
  @ insert_before_tests
  @ insert_after_tests;
