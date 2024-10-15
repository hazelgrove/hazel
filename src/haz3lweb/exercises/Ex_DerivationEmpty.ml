let prompt = Ex_DerivationEmpty_prompt.prompt

let exercise : Exercise.spec =
  Exercise.transition
    {
      header =
        {
          title = "Derivation Playground";
          version = 1;
          module_name = "Ex_DerivationPlayground";
          prompt;
        };
      pos = Proof Prelude;
      model =
        Proof
          {
            prelude = "";
            setup = "";
            trees = [ Node (Just { jdmt = ""; rule = None }, []) ];
          };
    }
