let exercise : DerivationTree.spec =
  DerivationTree.transition
    {
      title = "Practice Task: Transcription";
      version = 0;
      module_name = "t0";
      prompt =
        "Transcribe the derivation tree into Hazel Deriver. No worry if the \
         derivation is not correct.";
      corpus = ALFA;
      prelude = "";
      setup = "";
      trees = [ Node (Just { jdmt = ""; rule = None }, []) ];
    }
