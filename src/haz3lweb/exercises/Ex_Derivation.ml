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
      prelude = "";
      correct_impl = "";
      your_tests = { tests = ""; required = 0; provided = 0 };
      your_impl = "let a = atom(\"A\") in";
      hidden_bugs = [];
      hidden_tests = { tests = ""; hints = [ "no hints" ] };
      syntax_tests = [];
      derivation =
        {
          (* concl = "[a /\\ b] |- b /\\ a";
             rule = And_I; *)
          value = { concl = "[a /\\ b] |- b /\\ a"; rule = And_I };
          child =
            [
              {
                (* concl = "[a /\\ b] |- b";
                   rule = And_E_L; *)
                value = { concl = "[a /\\ b] |- b"; rule = And_E_L };
                child =
                  [
                    {
                      (* concl = "[a /\\ b] |- a /\\ b";
                         rule = Assumption; *)
                      value =
                        { concl = "[a /\\ b] |- a /\\ b"; rule = Assumption };
                      child = [];
                    };
                  ];
              };
              {
                (* concl = "[a /\\ b] |- a";
                   rule = And_E_R; *)
                value = { concl = "[a /\\ b] |- a"; rule = And_E_R };
                child =
                  [
                    {
                      (* concl = "[a /\\ b] |- a /\\ b";
                         rule = Assumption; *)
                      value =
                        { concl = "[a /\\ b] |- a /\\ b"; rule = Assumption };
                      child = [];
                    };
                  ];
              };
            ];
        };
    }
