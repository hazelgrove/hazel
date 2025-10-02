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
let test_prepare_definition = (~name, ~init: string, ~goal): test_case(_) => {
  let z = perform(Zipper.init(), mk(init));
  let info_map = mk_statics(z);
  let curr_node_info =
    Option.get(AssistantTreeHelper.build_curr_node_info(z, info_map));
  let prepped_z_str =
    CompositionView.prepare_definition(z, curr_node_info)
    |> CompositionView.printer;
  test_case(name, `Quick, () =>
    check(testable(Fmt.string, String.equal), goal, goal, prepped_z_str)
  );
};

let view_definition_tests = [
  test_prepare_definition(
    ~name="View Definition (\"Simplest\" Case)",
    ~init={|let x = 4¦ in x|},
    ~goal={|let x = 4 in x|},
  ),
  test_prepare_definition(
    ~name="View Definition (Single Sibling)",
    ~init={|let x = 4¦ in let y = 5 in x + y|},
    ~goal={|let x = 4 in let y = ⋱ in x + y|},
  ),
  test_prepare_definition(
    ~name="View Definition (Single Sibling)",
    ~init={|let x = 4 in let y = 5 in¦ x + y|},
    ~goal={|let x = ⋱ in let y = 5 in x + y|},
  ),
  test_prepare_definition(
    ~name="View Definition (Single Parent, Two Children - At Parent)",
    ~init={|let par1 = let chi1 = 0 in let chi2 = 1 in chi1 + chi2 in¦ par1|},
    ~goal=
      {|let par1 = let chi1 = ⋱ in let chi2 = ⋱ in chi1 + chi2 in par1|},
  ),
  test_prepare_definition(
    ~name="View Definition (Single Parent, Two Children - At 1st Child)",
    ~init={|let par1 = let chi1 = 1 in¦ let chi2 = 2 in chi1 + chi2 in par1|},
    ~goal={|let par1 = let chi1 = 1 in let chi2 = ⋱ in chi1 + chi2 in ⋱|},
  ),
  test_prepare_definition(
    ~name="View Definition (Single Parent, Two Children - At 2nd Child)",
    ~init={|let par1 = let chi1 = 2 in let chi2 = 3 in¦ chi1 + chi2 in par1|},
    ~goal={|let par1 = let chi1 = ⋱ in let chi2 = 3 in chi1 + chi2 in ⋱|},
  ),
];
