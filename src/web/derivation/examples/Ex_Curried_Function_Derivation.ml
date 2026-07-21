let exercise : DerivationExercise.spec =
  DerivationExercise.transition
    {
      id =
        Option.get
          (Haz3lcore.Id.of_string "6877c1e5-4421-40c8-9da3-9b41f3e48a3d");
      title = "Curried Function Derivation";
      module_name = "Ex_Curried_Function_Derivation";
      prompt =
        "Provide a derivation of the following judgement, which establishes \
         that the curried \226\128\156min\226\128\157 function in ALFp has \
         type: Num \226\134\146 Num \226\134\146 Num";
      max_points = 10;
      prelude = "";
      setup =
        "let $ctx_a = of_ctx (a : Num)::[] end in\n\
         let $ctx_ab = of_ctx (b : Num)::$ctx_a end in";
      rule_set = ALFp;
      trees =
        [
          Node (Just { jdmt = "$ctx_ab |- a : Num"; rule = Some T_Var }, []);
          Node (Just { jdmt = "$ctx_ab |- b : Num"; rule = Some T_Var }, []);
          Node
            ( Just
                {
                  jdmt =
                    "|- (fun a : Num -> fun b : Num -> if a < b then a else b) \
                     : Num -> Num -> Num";
                  rule = Some T_FunAnn;
                },
              [
                Node
                  ( Just
                      {
                        jdmt =
                          "$ctx_a |- (fun b : Num -> if a < b then a else b) : \
                           Num -> Num";
                        rule = Some T_FunAnn;
                      },
                    [
                      Node
                        ( Just
                            {
                              jdmt = "$ctx_ab |- (if a < b then a else b) : Num";
                              rule = Some T_If;
                            },
                          [
                            Node
                              ( Just
                                  {
                                    jdmt = "$ctx_ab |- (a < b) : Bool";
                                    rule = Some T_Lt;
                                  },
                                [
                                  Node (Abbr (Some 0), []);
                                  Node (Abbr (Some 1), []);
                                ] );
                            Node (Abbr (Some 0), []);
                            Node (Abbr (Some 1), []);
                          ] );
                    ] );
              ] );
        ];
    }
