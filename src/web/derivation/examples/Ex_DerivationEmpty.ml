let exercise version : DerivationTree.spec =
  DerivationTree.transition
    {
      id = Haz3lcore.Id.mk ();
      title = "Derivation Playground " ^ version;
      module_name = "Ex_DerivationPlayground";
      prompt = "Try proving anything you want.";
      corpus = Language.RuleImage.GradualALFA;
      prelude = "";
      setup = "";
      trees = [ Node (Just { jdmt = ""; rule = None }, []) ];
    }
