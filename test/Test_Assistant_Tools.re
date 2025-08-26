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

let apply_tool_actions =
    (init: string, actions: list(CompositionTools.action)): Zipper.t =>
  List.fold_left(
    (z, action) =>
      z
      |> mk_statics
      |> CompositionTools.derive_actions(z, _, action)
      |> snd
      |> perform(z),
    perform(Zipper.init(), mk(init)),
    actions,
  );

let test =
    (~name, ~init: string, ~acts: list(CompositionTools.action), ~goal)
    : test_case(_) =>
  test_case(name, `Quick, () =>
    check(
      testable(Fmt.string, String.equal),
      goal,
      goal,
      acts |> apply_tool_actions(init) |> printer,
    )
  );

let tests_edit_tools = [
  test(
    ~name="Update Definition (Simple - V1)",
    ~init={|let x = ¦4 in x|},
    ~acts=[Edit(UpdateDefinition("foo"))],
    ~goal={|let x = foo¦ in x|},
  ),
  test(
    ~name="Update Definition (Simple - V2)",
    ~init={|let x = (1, 2)¦ in x|},
    ~acts=[Edit(UpdateDefinition("(foo, 2, bar)"))],
    ~goal={|let x = (foo, 2, bar)¦ in x|},
  ),
  // test(
  //   ~name="Update Pattern (Replication V1)",
  //   ~init="type t = + T in let u : old_t -> ? = ? in¦ ?",
  //   ~acts=[Edit(UpdatePattern("u : t -> ?"))],
  //   ~goal="type t = + T in let u : t -> ?¦ = ? in ?",
  // ),
];

let tests_nav_tools = [
  test(
    ~name="Goto Sibling (Simple - V1)",
    ~init={|let x = 4 in let y = 5¦ in x + y|},
    ~acts=[Nav(GoToSibling("x", None))],
    ~goal={|§let x = 4 in¦ let y = 5 in x + y|},
  ),
  test(
    ~name="Goto Sibling (Simple - V2)",
    ~init={|let x = 4 in let y = 6 in let z = 7¦ in x + y + z|},
    ~acts=[Nav(GoToSibling("x", None))],
    ~goal={|§let x = 4 in¦ let y = 6 in let z = 7 in x + y + z|},
  ),
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
  test(
    ~name="Goto Sibling (Replication V1)",
    ~init=
      "type MyOption = + Some(?) + None in let unwrap : Option -> ? = fun x -> case x | Some(v) => v | None => ? end in¦ ?",
    ~acts=[Nav(GoToSibling("let", Some(0)))],
    ~goal=
      "§type MyOption = + Some(?) + None in¦ let unwrap : Option -> ? = fun x -> case x | Some(v) => v | None => ? end in ?",
  ),
  // test(
  //   ~name="Goto Sibling (Replication V2)",
  //   ~init="type t = + T in let u : old_t -> ? = ? in¦ ?",
  //   ~acts=[
  //     Edit(UpdateExpression("let u : t -> ? = ? in")),
  //     Nav(GoToSibling("t", Some(0))),
  //   ],
  //   ~goal="§type t = + T in¦ let u : t -> ? = ? in ?",
  // ),
];

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

let tests_view_definition = [
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

let tests = [
  ("Edit tools", tests_edit_tools),
  ("Nav tools", tests_nav_tools),
  ("View definition", tests_view_definition),
];
