let exercise version : DerivationTree.spec =
  DerivationTree.transition
    {
      title = "Derivation Playground " ^ version;
      version = 0;
      module_name = "Ex_DerivationPlayground";
      prompt = "Try proving anything you want.";
      corpus = Language.RuleImage.GradualALFA;
      prelude = "";
      setup = "";
      trees = [ Node (Just { jdmt = ""; rule = None }, []) ];
    }
