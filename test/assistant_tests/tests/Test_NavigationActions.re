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
