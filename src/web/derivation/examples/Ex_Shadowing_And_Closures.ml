let exercise : DerivationExercise.spec =
  DerivationExercise.transition
    {
      id = Haz3lcore.Id.v "280ed584-a895-40ee-8292-8622b0aab321";
      title = "Shadowing and Closures";
      module_name = "Ex_Shadowing_And_Closures";
      prompt = "let us derive the judgement e_example \226\135\147 4.";
      max_points = 10;
      prelude = "";
      setup =
        "let $e2 = of_alfa_exp \n\
         let y = y - 3 in f(y)\n\
         end in\n\
         let $e1 = of_alfa_exp\n\
         let f = fun z -> y * z in $e2\n\
         end in\n\
         let $e_example = of_alfa_exp\n\
         let y = 4 in $e1\n\
         end in";
      rule_set = ALF;
      trees =
        [
          Node
            ( Just { jdmt = "4 \\=/ 4"; rule = Some E_Val },
              [ Node (Just { jdmt = "val 4 end"; rule = Some V_Num }, []) ] );
          Node
            ( Just { jdmt = "1 \\=/ 1"; rule = Some E_Val },
              [ Node (Just { jdmt = "val 1 end"; rule = Some V_Num }, []) ] );
          Node
            ( Just
                {
                  jdmt = "fun z -> 4 * z \\=/ fun z -> 4 * z";
                  rule = Some E_Val;
                },
              [
                Node
                  ( Just { jdmt = "val fun z -> 4 * z end"; rule = Some V_Fun },
                    [] );
              ] );
          Node
            ( Just
                {
                  jdmt = "let y = 4 - 3 in (fun z -> 4 * z)(y) \\=/ 4";
                  rule = Some E_Let;
                },
              [
                Node
                  ( Just { jdmt = "4 - 3 \\=/ 1"; rule = Some E_Minus },
                    [
                      Node (Abbr (Some 0), []);
                      Node
                        ( Just { jdmt = "3 \\=/ 3"; rule = Some E_Val },
                          [
                            Node
                              ( Just { jdmt = "val 3 end"; rule = Some V_Num },
                                [] );
                          ] );
                    ] );
                Node
                  ( Just
                      { jdmt = "(fun z -> 4 * z)(1) \\=/ 4"; rule = Some E_Ap },
                    [
                      Node (Abbr (Some 2), []);
                      Node (Abbr (Some 1), []);
                      Node
                        ( Just { jdmt = "4 * 1 \\=/ 4"; rule = Some E_Times },
                          [ Node (Abbr (Some 0), []); Node (Abbr (Some 1), []) ]
                        );
                    ] );
              ] );
          Node
            ( Just
                {
                  jdmt =
                    "let f = fun z -> 4 * z in let y = 4 - 3 in f(y) \\=/ 4";
                  rule = Some E_Let;
                },
              [ Node (Abbr (Some 2), []); Node (Abbr (Some 3), []) ] );
          Node
            ( Just { jdmt = "$e_example \\=/ 4"; rule = Some E_Let },
              [ Node (Abbr (Some 0), []); Node (Abbr (Some 4), []) ] );
        ];
    }
