let prompt = Ex_Abbr_prompt.prompt

let exercise : Exercise.spec =
  Exercise.transition
    {
      header =
        {
          title = "Abbr Derivation";
          version = 1;
          module_name = "Ex_Abbr";
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
            trees =
              [
                Node
                  ( Just { jdmt = "gamma |- a /\\ b"; rule = Some Assumption },
                    [] );
                Node
                  ( Just { jdmt = "gamma |- b /\\ a"; rule = Some And_I },
                    [
                      Node
                        ( Just { jdmt = "gamma |- b"; rule = Some And_E_R },
                          [ Node (Abbr (Some 0), []) ] );
                      Node
                        ( Just { jdmt = "gamma |- a"; rule = Some And_E_L },
                          [ Node (Abbr (Some 0), []) ] );
                    ] );
              ];
          };
    }
