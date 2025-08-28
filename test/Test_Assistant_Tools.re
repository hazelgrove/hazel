open Alcotest;
open Haz3lcore;
open Test_Editing;
module Fresh = Language.IdTagged.FreshGrammar;

let mk_statics = (z: Zipper.t) =>
  Language.(
    Statics.mk(
      CoreSettings.on,
      Builtins.ctx_init(Some(Operators.default_mode)),
      MakeTerm.from_zip_for_sem(z).term,
    )
  );

let apply_actions = (init: string, actions: list(Action.t)): Zipper.t =>
  perform(perform(Zipper.init(), mk(init)), actions);

let test =
    (~name, ~init: string, ~acts: list(CompositionTools.action), ~goal)
    : test_case(_) => {
  let acts = List.map(a => Action.AssistantComposition(a), acts);
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
/* |||| NAVIGATION TOOL TESTS |||| */
/* =============================== */
// - GoToParent
// - GoToChild(string, option(int))
// - GoToSibling(via) ('via' being either (string, option(int)) or Direction.t)

let goto_parent_tests = {
  [
    test(
      ~name="Goto Parent (Simple - V1)",
      ~init={|let x = let y = let z = 7¦ in z in y in x|},
      ~acts=[Nav(GoToParent)],
      // Goes from 'z' to its parent, 'y'
      ~goal={|let x = §let y = let z = 7 in z in¦ y in x|},
    ),
    test(
      ~name="Goto Parent (Double go to, Simple - V2)",
      ~init={|let x = let y = let z = 7¦ in z in y in x|},
      ~acts=[Nav(GoToParent), Nav(GoToParent)],
      // Goes from 'z' to its parent, 'y', and then to its parent, 'x'
      ~goal={|§let x = let y = let z = 7 in z in y in¦ x|},
    ),
  ];
};

let goto_child_tests = {
  [
    test(
      ~name="Goto Child (Name-Identified - Simple - V1)",
      ~init={|let x = let a = 3 in let b = 4 in a + b in¦ x|},
      ~acts=[Nav(GoToChild("b", None))],
      // Goes from 'x' to its child, 'b'
      ~goal={|let x = let a = 3 in §let b = 4 in¦ a + b in x|},
    ),
    test(
      ~name="Goto Child (Index-Identified - Shadowed-Case - V1)",
      ~init={|let x = let b = 3 in let b = 4 in b in¦ x|},
      ~acts=[Nav(GoToChild("b", Some(0)))],
      // Goes from 'x' to its first child, 'b'
      ~goal={|let x = §let b = 3 in¦ let b = 4 in b in x|},
    ),
    test(
      ~name="Goto Child (Index-Identified - Shadowed-Case - V2)",
      ~init={|let x = let b = 3 in let b = 4 in b in¦ x|},
      ~acts=[Nav(GoToChild("b", Some(1)))],
      // Goes from 'x' to its second child, 'b'
      ~goal={|let x = let b = 3 in §let b = 4 in¦ b in x|},
    ),
    test(
      ~name="Goto Child (Index-Identified - Shadowed-Case - V3)",
      ~init=
        {|
    let x = let b = 3 in let h = 10 in let b = 4 in b + h in¦ x
    |},
      ~acts=[Nav(GoToChild("b", Some(2)))],
      // Goes from 'x' to its third child, 'b' (the second 'b' in this case)
      ~goal=
        {|
    let x = let b = 3 in let h = 10 in §let b = 4 in¦ b + h in x
    |},
    ),
  ];
};

let goto_sibling_tests = {
  [
    test(
      ~name="Goto Sibling (Simple - V1)",
      ~init={|let x = 4 in let y = 5¦ in x + y|},
      ~acts=[Nav(GoToSibling(NameAndIdx("x", None)))],
      ~goal={|§let x = 4 in¦ let y = 5 in x + y|},
    ),
    test(
      ~name="Goto Sibling (Simple - V2)",
      ~init={|let x = 4 in let y = 6 in let z = 7¦ in x + y + z|},
      ~acts=[Nav(GoToSibling(NameAndIdx("x", None)))],
      ~goal={|§let x = 4 in¦ let y = 6 in let z = 7 in x + y + z|},
    ),
    test(
      ~name="Goto Sibling (Replication V1)",
      ~init=
        "type MyOption = + Some(?) + None in let unwrap : Option -> ? = fun x -> case x | Some(v) => v | None => ? end in¦ ?",
      ~acts=[Nav(GoToSibling(NameAndIdx("let", Some(0))))],
      ~goal=
        "§type MyOption = + Some(?) + None in¦ let unwrap : Option -> ? = fun x -> case x | Some(v) => v | None => ? end in ?",
    ),
    // test(
    //   ~name="Goto Sibling (Replication V2)",
    //   ~init="type t = + T in let u : old_t -> ? = ? in¦ ?",
    //   ~acts=[
    //     Edit(UpdateBindingClause("let u : t -> ? = ? in")),
    //     Nav(GoToSibling("t", Some(0))),
    //   ],
    //   ~goal="§type t = + T in¦ let u : t -> ? = ? in ?",
    // ),
  ];
};

let nav_tests = goto_parent_tests @ goto_child_tests @ goto_sibling_tests;

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
      ~acts=[Edit(UpdateDefinition("2"))],
      ~goal="let x = 2¦ in x",
    ),
    test(
      ~name="Update Definition (Simple - V2)",
      ~init={|let x = (1, 2)¦ in x|},
      ~acts=[Edit(UpdateDefinition("(foo, 2, bar)"))],
      ~goal={|let x = (foo, 2, bar)¦ in x|},
    ),
  ];
};

let update_body_tests = {
  [
    test(
      ~name="Update Body (\"Simplest\" Case)",
      ~init="let x = 1 in¦ x + 2",
      ~acts=[Edit(UpdateBody("x * 2"))],
      ~goal="let x = 1 in x * 2¦",
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
      ~acts=[Edit(UpdatePattern("a"))],
      ~goal="let a¦ = 1 in x",
    ),
    test(
      ~name="Update Pattern (Tuple)",
      ~init="let (a, b) = ? in¦ ?",
      ~acts=[Edit(UpdatePattern("(x, y)"))],
      ~goal="let (x, y)¦ = ? in ?",
    ),
    test(
      ~name="Update Pattern (Annotated Atomic Type)",
      ~init="let u : Int = ? in¦ ?",
      ~acts=[Edit(UpdatePattern("u : Float"))],
      ~goal="let u : Float¦ = ? in ?",
    ),
    test(
      ~name="Update Pattern (Annotated Arrow Type)",
      ~init="let u : Int -> Int = ? in¦ ?",
      ~acts=[Edit(UpdatePattern("u : Float -> Float"))],
      ~goal="let u : Float -> Float¦ = ? in ?",
    ),
    test(
      ~name="Update Pattern (Annotated Arrow Type 2 (Remove Annotation))",
      ~init="let u : Int -> Int = ? in¦ ?",
      ~acts=[Edit(UpdatePattern("u"))],
      ~goal="let u¦ = ? in ?",
    ),
    test(
      ~name="Update Pattern (Annotated Arrow Type 3 (Introduce Annotation))",
      ~init="let u = ? in¦ ?",
      ~acts=[Edit(UpdatePattern("u : Int -> Int"))],
      ~goal="let u : Int -> Int¦ = ? in ?",
    ),
  ];
};

let update_binding_clause_tests = {
  [
    test(
      ~name="Update Binding Clause (\"Simplest\" Case)",
      ~init="let x = 1¦ in x",
      ~acts=[Edit(UpdateBindingClause("let x = 2 in"))],
      ~goal="let x = 2 in¦ x",
    ),
  ];
};

let delete_binding_clause_tests = {
  [
    test(
      ~name="Delete Binding Clause (\"Simplest\" Case)",
      ~init="let a = 1¦ in let b = 2 in a + b",
      ~acts=[Edit(DeleteBindingClause)],
      ~goal="¦ let b = 2 in a + b" // todo: better handle relocation of cursor
    ),
  ];
};

let delete_body_tests = {
  [
    test(
      ~name="Delete Body (\"Simplest\" Case)",
      ~init="let a = 1¦ in a * 5",
      ~acts=[Edit(DeleteBody)],
      ~goal="let a = 1 in ¦?",
    ),
    test(
      ~name="Delete Body (Two Bindings (Final Body Empty Hole)))",
      ~init="let a = 0¦ in let b = 1 in ",
      ~acts=[Edit(DeleteBody)],
      ~goal="let a = 0 in ¦ ?",
    ),
    test(
      ~name="Delete Body (Two Bindings (Final Body Non-Empty)))",
      ~init="let a = 1¦ in let b = 2 in a + b",
      ~acts=[Edit(DeleteBody)],
      ~goal="let a = 1 in ¦?",
    ),
    test(
      ~name=
        "Delete Body (Multiple Bindings (Final Body Partially Empty Hole)))",
      ~init="let a = 0¦ in let b = 1 in let c = 2 in a + ",
      ~acts=[Edit(DeleteBody)],
      ~goal="let a = 0 in ¦ ?",
    ),
    test(
      ~name="Delete Body (Multiple Bindings (Final Body Empty Hole)))",
      ~init="let a = 0¦ in let b = 1 in let c = 2 in ",
      ~acts=[Edit(DeleteBody)],
      ~goal="let a = 0 in ¦ ?",
    ),
    test(
      ~name="Delete Body (Multiple Bindings (Final Body Non-Empty)))",
      ~init="let a = 1¦ in let b = 2 in let c = 3 in a + b + c",
      ~acts=[Edit(DeleteBody)],
      ~goal="let a = 1 in ¦?",
    ),
  ];
};

let insert_before_tests = {
  [];
    /*
     test(
         ~name="Insert Before (\"Simplest\" Case)",
         ~init="let b = 2¦ in a * b",
         ~acts=[Edit(InsertBefore("let a = 1 in"))],
         ~goal="let a = 1 in¦ let b = 2 in a * b",
       ),
       */
};

let insert_after_tests = {
  [
    test(
      ~name="Insert After (\"Simplest\" Case)",
      ~init="let a = 1¦ in a * b",
      ~acts=[Edit(InsertAfter("let b = 2 in"))],
      ~goal="let a = 1 in let b = 2¦ in a * b",
    ),
  ];
};

let edit_tests =
  update_definition_tests
  @ update_body_tests
  @ update_pattern_tests
  @ update_binding_clause_tests
  @ delete_binding_clause_tests
  @ delete_body_tests
  @ insert_before_tests
  @ insert_after_tests;

/* =============================== */
/* |||| VIEW DEFINITION TESTS |||| */
/* =============================== */
// Tests to validate that our local code map display, and collapsing of definitions work
// as expected.

// For testing that we display the proper contents for the assistant
// (Note: This is not a tool call. This is a function that we use to display the relevant sketch content for the assistant
//        on each iteration.)
let test_view_definition = (~name, ~init: string, ~goal): test_case(_) => {
  let z = perform(Zipper.init(), mk(init));
  let info_map = mk_statics(z);
  let curr_node_info =
    Option.get(AssistantTreeHelper.build_curr_node_info(z, info_map));
  let sketch_seg_str =
    CompositionUtil.View.definition(z, curr_node_info)
    |> Printer.of_segment(~holes="?", ~special_folds=true);
  test_case(name, `Quick, () =>
    check(testable(Fmt.string, String.equal), goal, goal, sketch_seg_str)
  );
};

let view_definition_tests = [
  test_view_definition(
    ~name="View Definition (Simple - V1)",
    ~init={|let x = 4¦ in x|},
    ~goal={|let x = 4 in x|},
  ),
  test_view_definition(
    ~name="View Definition (Simple - V1)",
    ~init={|let x = 4¦ in let y = 5 in x + y|},
    ~goal={|let x = 4 in let y = ⋱ in x + y|},
  ),
];

/* =============================== */
/* |||| JOINED TESTS |||| */
/* =============================== */

let tests = [
  ("AssistantComposition.Navigation", nav_tests),
  ("AssistantComposition.Editing", edit_tests),
  ("AssistantTreeHelper.ViewDefinition", view_definition_tests),
];
