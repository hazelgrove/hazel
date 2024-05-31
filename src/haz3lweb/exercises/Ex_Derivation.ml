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
        {
          value = { concl = "gamma |- b /\\ a"; rule = And_I };
          child =
            [
              {
                value = { concl = "gamma |- b"; rule = And_E_R };
                child =
                  [
                    {
                      value = { concl = "gamma |- b"; rule = Assumption };
                      child = [];
                    };
                  ];
              };
              {
                value = { concl = "gamma |- b /\\ a"; rule = And_E_L };
                child =
                  [
                    {
                      value = { concl = "gamma |- a /\\ b"; rule = Assumption };
                      child = [];
                    };
                  ];
              };
            ];
        };
    }
