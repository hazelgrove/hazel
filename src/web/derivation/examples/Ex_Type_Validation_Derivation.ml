let exercise : DerivationExercise.spec =
  DerivationExercise.transition
    {
      id = Haz3lcore.Id.v "f73cdb5d-76b5-4675-82cd-b7ccf757dd27";
      title = "Type Validation Derivation";
      module_name = "Ex_Type_Validation_Derivation";
      prompt = "";
      max_points = 10;
      prelude = "";
      setup =
        "let $delta = of_ctx [valid A end] end in\n\
         let $delta' = of_ctx (x : A)::$delta end in";
      rule_set = RecursiveALFA;
      trees =
        [
          Node
            ( Just
                {
                  jdmt = "$delta |- (fun x : A -> x) : A -> A";
                  rule = Some T_FunAnn;
                },
              [
                Node
                  ( Just { jdmt = "$delta |- valid A end"; rule = Some TV_TVar },
                    [] );
                Node (Just { jdmt = "$delta' |- x : A"; rule = Some T_Var }, []);
              ] );
        ];
    }
