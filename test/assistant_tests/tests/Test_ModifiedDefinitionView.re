open Alcotest;
open Haz3lcore;
open Test_Editing;
module Fresh = Language.IdTagged.FreshGrammar;
open Utils_AssistantTests;

/* =============================== */
/* |||| MODIFIED DEFINITION VIEW TESTS |||| */
/* =============================== */
// Tests to validate that our local code map display, and collapsing of definitions work
// as expected.

// For testing that we display the proper contents for the assistant
// (Note: This is not a tool call. This is a function that we use to display the relevant sketch content for the assistant
//        on each iteration.)
let test = (~name, ~init: string, ~goal): test_case(_) => {
  let z = perform(Zipper.init(), mk(init));
  let info_map = mk_statics(z);
  let curr_node_info = AssistantTreeHelper.HighLevelNode.build(z, info_map);
  let prepped_z_str =
    switch (curr_node_info) {
    | Some(node) =>
      // The actual function we're testing
      CompositionView.prepare_definition(z, node) |> CompositionView.printer
    | None => ""
    };
  test_case(name, `Quick, () =>
    check(testable(Fmt.string, String.equal), goal, goal, prepped_z_str)
  );
};

let view_definition_tests = [
  // Basic cases
  test(
    ~name="View Definition (\"Simplest\" Case)",
    ~init={|let x = 4¦ in x|},
    ~goal={|§let x = 4 in¦ x|},
  ),
  test(
    ~name="View Definition (Single Sibling - At First)",
    ~init={|let x = 4 in¦ let y = 5 in x + y|},
    ~goal={|§let x = 4 in¦ let y = ⋱ in x + y|},
  ),
  test(
    ~name="View Definition (Single Sibling - At Second)",
    ~init={|let x = 4 in let y = 5 in¦ x + y|},
    ~goal={|let x = ⋱ in §let y = 5 in¦ x + y|},
  ),
  // Parent with two children
  test(
    ~name="View Definition (Single Parent, Two Children - At Parent)",
    ~init={|let par1 = let chi1 = 0 in let chi2 = 1 in chi1 + chi2 in¦ par1|},
    ~goal=
      {|§let par1 = let chi1 = ⋱ in let chi2 = ⋱ in chi1 + chi2 in¦ par1|},
  ),
  test(
    ~name="View Definition (Single Parent, Two Children - At 1st Child)",
    ~init={|let par1 = let chi1 = 1 in¦ let chi2 = 2 in chi1 + chi2 in par1|},
    ~goal=
      {|let par1 = §let chi1 = 1 in¦ let chi2 = ⋱ in chi1 + chi2 in ⋱|},
  ),
  test(
    ~name="View Definition (Single Parent, Two Children - At 2nd Child)",
    ~init={|let par1 = let chi1 = 2 in let chi2 = 3 in¦ chi1 + chi2 in par1|},
    ~goal=
      {|let par1 = let chi1 = ⋱ in §let chi2 = 3 in¦ chi1 + chi2 in ⋱|},
  ),
  // Multiple siblings at same level
  test(
    ~name="View Definition (Three Siblings - At First)",
    ~init={|let a = 1 in¦ let b = 2 in let c = 3 in a + b + c|},
    ~goal={|§let a = 1 in¦ let b = ⋱ in let c = ⋱ in a + b + c|},
  ),
  test(
    ~name="View Definition (Three Siblings - At Second)",
    ~init={|let a = 1 in let b = 2 in¦ let c = 3 in a + b + c|},
    ~goal={|let a = ⋱ in §let b = 2 in¦ let c = ⋱ in a + b + c|},
  ),
  test(
    ~name="View Definition (Three Siblings - At Third)",
    ~init={|let a = 1 in let b = 2 in let c = 3 in¦ a + b + c|},
    ~goal={|let a = ⋱ in let b = ⋱ in §let c = 3 in¦ a + b + c|},
  ),
  // Nested let expressions (grandparent -> parent -> child)
  test(
    ~name="View Definition (Three Levels - At Grandparent)",
    ~init=
      {|let grand = let parent = let child = 42 in child + 1 in parent * 2 in¦ grand|},
    ~goal={|§let grand = let parent = ⋱ in parent * 2 in¦ grand|},
  ),
  test(
    ~name="View Definition (Three Levels - At Parent)",
    ~init=
      {|let grand = let parent = let child = 42 in child + 1 in¦ parent * 2 in grand|},
    ~goal=
      {|let grand = §let parent = let child = ⋱ in child + 1 in¦ parent * 2 in ⋱|},
  ),
  test(
    ~name="View Definition (Three Levels - At Child)",
    ~init=
      {|let grand = let parent = let child = 42 in¦ child + 1 in parent * 2 in grand|},
    ~goal={|let parent = §let child = 42 in¦ child + 1 in ⋱|},
  ),
  // Type definitions
  test(
    ~name="View Definition (Type Definition - At Type)",
    ~init={|type Color = Red | Blue | Green¦ in let x = Red in x|},
    ~goal={|§type Color = Red | Blue | Green in¦ let x = ⋱ in x|},
  ),
  // Type definitions should never be collapsed
  test(
    ~name="View Definition (Type Definition - At Variable)",
    ~init={|type Color = Red | Blue | Green in let x = Red in¦ x|},
    ~goal={|type Color = Red | Blue | Green in §let x = Red in¦ x|},
  ),
  // Function definitions
  test(
    ~name="View Definition (Function Definition - At Function)",
    ~init={|let add = fun (x, y) -> x + y in¦ let z = 3 in add(1, 2)|},
    ~goal={|§let add = fun (x, y) -> x + y in¦ let z = ⋱ in add(1, 2)|},
  ),
  test(
    ~name="View Definition (Function Definition - At Const Var)",
    ~init={|let add = fun (x, y) -> x + y in let z = 3 in¦ add(1, 2)|},
    ~goal={|let add = ⋱ in §let z = 3 in¦ add(1, 2)|},
  ),
  test(
    ~name="View Definition (Function Definition - At Call Site)",
    ~init={|let add = fun (x, y) -> x + y in let z = 3 in add(1, 2)¦|},
    ~goal={|let add = ⋱ in §let z = 3 in¦ add(1, 2)|},
  ),
  // Pattern caseing
  test(
    ~name="View Definition (Pattern case - At case Expression)",
    ~init={|let x = Some(42) in case x | Some(n) => n | None => 0¦|},
    ~goal={|§let x = Some(42) in¦ case x | Some(n) => n | None => 0|},
  ),
  test(
    ~name="View Definition (Pattern case - At Variable)",
    ~init={|let x = Some(42) in¦ case x | Some(n) => n | None => 0|},
    ~goal={|§let x = Some(42) in¦ case x | Some(n) => n | None => 0|},
  ),
  test(
    ~name="View Definition (Single Expression - No Let)",
    ~init={|42 + 1¦|},
    // todo, currently this fails. we should refactor CompositioView and ultiamtely
    // test a function that makes the view-string for us
    ~goal={|§42 + 1¦|},
  ),
  test(
    ~name="View Definition (Empty Let Body)",
    ~init={|let x = 42 in¦ ?|},
    ~goal={|§let x = 42 in¦ ?|},
  ),
];
