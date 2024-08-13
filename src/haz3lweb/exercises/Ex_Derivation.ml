let prompt = Ex_Derivation_prompt.prompt

let exercise : Exercise.spec =
  Exercise.transition
    {
      header =
        {
          title = "Derivation";
          version = 1;
          module_name = "Ex_Derivation";
          prompt;
        };
      pos = Proof Prelude;
      model =
        Proof
          {
            prelude =
              "let a = atom(\"A\") in\n\
               let b = atom(\"B\") in\n\
               let gamma = [a /\\ b] in\n";
            tree =
              Node
                ( { jdmt = "gamma |- b /\\ a"; rule = And_I },
                  [
                    Node
                      ( { jdmt = "gamma |- b"; rule = And_E_R },
                        [
                          Node
                            ( { jdmt = "gamma |- a /\\ b"; rule = Assumption },
                              [] );
                        ] );
                    Node
                      ( { jdmt = "gamma |- a"; rule = And_E_L },
                        [
                          Node
                            ( { jdmt = "gamma |- a /\\ b"; rule = Assumption },
                              [] );
                        ] );
                  ] );
          };
    }
