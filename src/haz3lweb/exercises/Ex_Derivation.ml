let prompt = Ex_Derivation_prompt.prompt

let exercise : Exercise.spec =
  Exercise.transition
    {
      title = "Derivation";
      version = 1;
      module_name = "Ex_Derivation";
      prompt;
      point_distribution =
        { test_validation = 1; mutation_testing = 1; impl_grading = 2 };
      prelude =
        "let a = atom(\"A\") in\n\
         let b = atom(\"B\") in\n\
         let gamma = [a /\\ b] in\n";
      correct_impl = "";
      your_tests = { tests = ""; required = 0; provided = 0 };
      your_impl = "";
      hidden_bugs = [];
      hidden_tests = { tests = ""; hints = [ "no hints" ] };
      syntax_tests = [];
      derivation =
        Node
          ( ("gamma |- b /\\ a", And_I),
            [
              Node
                ( ("gamma |- b", And_E_R),
                  [ Node (("gamma |- a /\\ b", Assumption), []) ] );
              Node
                ( ("gamma |- a", And_E_L),
                  [ Node (("gamma |- a /\\ b", Assumption), []) ] );
            ] )
        |> Exercise.mk_derivation_tree;
    }
