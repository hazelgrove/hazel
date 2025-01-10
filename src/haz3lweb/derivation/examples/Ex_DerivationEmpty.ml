let prompt = Ex_DerivationEmpty_prompt.prompt

let exercise : DerivationTree.spec =
  DerivationTree.transition
    {
      title = "Derivation Playground";
      version = 1;
      module_name = "Ex_DerivationPlayground";
      prompt;
      ruleset = Haz3lcore.RuleImage.GradualALFA;
      prelude = "";
      setup = "";
      trees = [ Node (Just { jdmt = ""; rule = None }, []) ];
    }
