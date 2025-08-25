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
];

let tests = [("Edit tools", tests_edit_tools)];
