let exercise : DerivationTree.spec =
  DerivationTree.transition
    {
      title = "Task 1 of 7: Transcription";
      version = 0;
      module_name = "t1";
      prompt =
        "Transcribe the derivation tree into Hazel Deriver. No worry if the \
         derivation is not correct.";
      corpus = ALFA;
      prelude = "";
      setup = "";
      trees = [ Node (Just { jdmt = ""; rule = None }, []) ];
    }
