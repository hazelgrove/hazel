let exercise : DerivationExercise.spec =
  DerivationExercise.transition
    {
      id =
        Option.get
          (Haz3lcore.Id.of_string "bf42c3bf-f3aa-4e0e-b180-8df80e1aaa8f");
      title = "PairMap Derivation";
      module_name = "Ex_PairMap_Derivation";
      prompt =
        "Provide a derivation using the Bidirectional Type System rules of the \
         following judgement, which synthesizes a type for the pairNegate \
         function shown above as being defined by partially applying pairmap. \
         This derivation shows how type analysis allows us to avoid having to \
         annotate the function argument.";
      max_points = 10;
      prelude = " ";
      setup =
        "let $tau_pm = of_alfa_typ (Bool -> Bool) -> (Bool * Bool) -> (Bool * \
         Bool) end in\n\
         let $gamma_pm = of_ctx [pairmap : $tau_pm] end in\n\
         let $gamma_pmz = of_ctx (z: Bool)::$gamma_pm end in \n";
      rule_set = ALFp;
      trees =
        [
          Node
            ( Just
                {
                  jdmt =
                    "$gamma_pm |- (fun z -> if z then False else True) <= \
                     (Bool -> Bool)";
                  rule = Some A_Fun;
                },
              [
                Node
                  ( Just
                      {
                        jdmt =
                          "$gamma_pmz |- (if z then False else True) <= Bool";
                        rule = Some A_If;
                      },
                    [
                      Node
                        ( Just
                            {
                              jdmt = "$gamma_pmz |- z <= Bool";
                              rule = Some A_Subsumption;
                            },
                          [
                            Node
                              ( Just
                                  {
                                    jdmt = "$gamma_pmz |- z => Bool";
                                    rule = Some S_Var;
                                  },
                                [] );
                          ] );
                      Node
                        ( Just
                            {
                              jdmt = "$gamma_pmz |- False <= Bool";
                              rule = Some A_Subsumption;
                            },
                          [
                            Node
                              ( Just
                                  {
                                    jdmt = "$gamma_pmz |- False => Bool";
                                    rule = Some S_False;
                                  },
                                [] );
                          ] );
                      Node
                        ( Just
                            {
                              jdmt = "$gamma_pmz |- True <= Bool";
                              rule = Some A_Subsumption;
                            },
                          [
                            Node
                              ( Just
                                  {
                                    jdmt = "$gamma_pmz |- True => Bool";
                                    rule = Some S_True;
                                  },
                                [] );
                          ] );
                    ] );
              ] );
          Node
            ( Just
                {
                  jdmt =
                    "$gamma_pm |- pairmap (fun z -> if z then False else True) \
                     => (Bool * Bool) -> (Bool * Bool)";
                  rule = Some S_Ap;
                },
              [
                Node
                  ( Just
                      {
                        jdmt = "$gamma_pm |- pairmap => $tau_pm";
                        rule = Some S_Var;
                      },
                    [] );
                Node (Abbr (Some 0), []);
              ] );
        ];
    }
