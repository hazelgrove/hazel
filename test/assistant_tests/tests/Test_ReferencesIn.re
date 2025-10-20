open Alcotest;
open Haz3lcore;
open Test_Editing;
module Fresh = Language.IdTagged.FreshGrammar;
open Utils_AssistantTests;

/* =============================== */
/* |||| VIEW REFS TESTS |||| */
/* =============================== */

open Language;
let refs_list_to_str_for_testing_only = (refs: list(Binding.t)): string => {
  List.map((binding: Binding.t) => binding.name, refs) |> String.concat(" ");
};

let test =
    (
      ~exclude_rec_refs: bool,
      ~exclude_body_refs: bool,
      ~name,
      ~init: string,
      ~goal,
    )
    : test_case(_) => {
  let z = perform(Zipper.init(), mk(init));
  let info_map = mk_statics(z);
  let curr_node_info =
    Option.get(AssistantTreeHelper.build_curr_node_info(z, info_map));
  let refs_in =
    refs_list_to_str_for_testing_only(
      CompositionView.refs_in(
        ~exclude_rec_refs,
        ~exclude_body_refs,
        curr_node_info,
        info_map,
      ),
    );
  test_case(name, `Quick, () =>
    check(testable(Fmt.string, String.equal), goal, goal, refs_in)
  );
};

let view_refs_tests = [
  test(
    ~name="View Refs (Include Rec Refs)",
    ~exclude_rec_refs=false,
    ~exclude_body_refs=false,
    ~init=
      {|
    let p1 =
      let c0 = 8 in
      let c1 = 5 in
      let f1¦ = fun x ->
        if true then x + c0
        else f1(x-1)
      in
      let c2 = 6 + c1 in
      c1 + c2 + c0
    in
    |},
    ~goal="c0 f1 c1",
  ),
  test(
    ~name="View Refs (Exclude Rec Refs)",
    ~exclude_rec_refs=true,
    ~exclude_body_refs=false,
    ~init=
      {|
    let p1 =
      let c0 = 8 in
      let c1 = 5 in
      let f1¦ = fun x ->
        if true then x + c0
        else f1(x-1)
      in
      let c2 = 6 + c1 in
      c1 + c2 + c0
    in
    |},
    ~goal="c0 c1",
  ),
  test(
    ~name="View Refs (Include Body Refs)",
    ~exclude_rec_refs=false,
    ~exclude_body_refs=true,
    ~init=
      {|
    let p1 =
      let c0 = 8 in
      let c1 = 5 in
      let f1¦ = fun x ->
        if true then x + c0
        else f1(x-1)
      in
      let c2 = 6 + c1 in
      c1 + c2 + c0
    in
    |},
    ~goal="c0 f1",
  ),
  test(
    ~name="View Refs (Include All)",
    ~exclude_rec_refs=true,
    ~exclude_body_refs=true,
    ~init=
      {|
    let p1 =
      let c0 = 8 in
      let c1 = 5 in
      let f1¦ = fun x ->
        if true then x + c0
        else f1(x-1)
      in
      let c2 = 6 + c1 in
      c1 + c2 + c0
    in
    |},
    ~goal="c0",
  ),
];
