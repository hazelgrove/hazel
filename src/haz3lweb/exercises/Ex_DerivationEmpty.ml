let prompt = Ex_DerivationEmpty_prompt.prompt

let exercise : Exercise.spec =
  Exercise.transition
    {
      header =
        {
          title = "Derivation Empty";
          version = 1;
          module_name = "Ex_DerivationEmpty";
          prompt;
        };
      pos = Proof Prelude;
      model = Proof { prelude = ""; trees = [] };
    }
