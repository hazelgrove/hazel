let exercise : DerivationExercise.spec =
  DerivationExercise.transition
    {
      id =
        Option.get
          (Haz3lcore.Id.of_string "1040866d-20f7-42e3-96c4-a9d9a4b239d3");
      title = "conjunction commutativity";
      module_name = "conjunction commutativity";
      prompt = "TODO: prompt";
      max_points = 10;
      prelude = "";
      setup = "let $ab = of_ctx [A /\\ B] end in ";
      rule_set = PropositionalLogic;
      trees =
        [
          Node (Just { jdmt = "$ab |- A /\\ B"; rule = Some Assumption }, []);
          Node
            ( Just
                {
                  jdmt = "[] |- (A /\\ B) ==> (B /\\ A)";
                  rule = Some Implies_I;
                },
              [
                Node
                  ( Just { jdmt = "$ab |- B /\\ A"; rule = Some And_I },
                    [
                      Node
                        ( Just { jdmt = "$ab |- B"; rule = Some And_E_R },
                          [ Node (Abbr (Some 0), []) ] );
                      Node
                        ( Just { jdmt = "$ab |- A"; rule = Some And_E_L },
                          [ Node (Abbr (Some 0), []) ] );
                    ] );
              ] );
        ];
    }
