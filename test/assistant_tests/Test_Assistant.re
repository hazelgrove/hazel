module Fresh = Language.IdTagged.FreshGrammar;

/* =============================== */
/* |||| JOINED TESTS |||| */
/* =============================== */

let tests = [
  ("Composition.Navigation", Test_NavigationActions.nav_tests),
  ("Composition.Editing", Test_EditActions.edit_tests),
  (
    "AssistantTreeHelper.ViewDefinition",
    Test_ModifiedDefinitionView.view_definition_tests,
  ),
  ("AssistantTreeHelper.ViewRefs", Test_ReferencesIn.view_refs_tests),
];
