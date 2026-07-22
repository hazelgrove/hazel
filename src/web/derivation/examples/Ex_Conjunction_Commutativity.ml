let exercise : DerivationExercise.spec =
  DerivationExercise.of_persistent
    {
      id = Haz3lcore.Id.v "1040866d-20f7-42e3-96c4-a9d9a4b239d3";
      title = "conjunction commutativity";
      module_name = "conjunction commutativity";
      prompt = "TODO: prompt";
      max_points = 10;
      prelude =
        {
          zipper =
            "((selection((focus Left)(content())(mode Normal)(anchor_caret \
             Outer)(smart_rounded false)))(relatives((siblings(()((Grout((id \
             4608e291-678c-449b-b492-663b763b207b)(shape \
             Convex))))))(ancestors())))(caret \
             Outer)(refractors((manuals())(multis((ids())(suppressed())(ephemerals())))(sample_focus((call_stack())(index \
             -1)(pinned_stack())(indicated_call())(time())(seq \
             0)(step_range())(pending_focus())))(autoprobe_target())(pending_probe_cursor()))))";
          backup_text = "";
        };
      setup =
        {
          zipper =
            "((selection((focus Left)(content())(mode Normal)(anchor_caret \
             Outer)(smart_rounded false)))(relatives((siblings(((Tile((id \
             f5af2ad7-8ec7-4b18-a593-74b71be3a765)(form(Compound \
             Let))(shards(0 1 2))(children(((Secondary((id \
             e9f00a16-4711-4e8d-9692-a7b452ad6d3f)(content(Whitespace\" \
             \"))))(Tile((id 2b48cf77-c01e-40b6-b6d7-7cd8659c50d8)(form(Tok \
             $ab))(sort Pat)))(Secondary((id \
             d2e2e77a-fa31-4599-b440-b2165c5a5717)(content(Whitespace\" \
             \")))))((Secondary((id \
             2a8bbf94-f606-48dc-8d34-db6559eba3e6)(content(Whitespace\" \
             \"))))(Tile((id \
             30e188ea-1c65-4749-bebe-6b6ff32b7413)(form(Compound \
             OfCtx))(shards(0 1))(children(((Secondary((id \
             8e0fa923-e655-4c07-8c94-bbf49578538e)(content(Whitespace\" \
             \"))))(Tile((id \
             241b2ced-ca02-4585-bddc-1fe401367e9b)(form(Compound \
             ListLit))(sort(Drv Exp))(shards(0 1))(children(((Tile((id \
             0e89eed7-b644-4bde-befd-cf20159604fc)(form(Tok A))(sort(Drv \
             Exp))))(Secondary((id \
             b5aa21ba-6abf-445f-be05-ed1a6e3d5631)(content(Whitespace\" \
             \"))))(Tile((id \
             c9088313-c557-45bd-ac7b-6328d61b85fc)(form(Compound \
             And))(sort(Drv Exp))))(Secondary((id \
             c3333b46-c208-4195-8de8-7c2d678bc675)(content(Whitespace\" \
             \"))))(Tile((id 94e646f4-bee0-4240-8438-6d405de2c5b4)(form(Tok \
             B))(sort(Drv Exp)))))))))(Secondary((id \
             0ea55e1c-eecb-4207-9375-36d88629f4aa)(content(Whitespace\" \
             \")))))))))(Secondary((id \
             f25b80fe-f9e8-4f73-94e0-c922bf1d87e9)(content(Whitespace\" \
             \")))))))))(Secondary((id \
             2fbff963-f2a2-4c85-923a-de38f337bec7)(content(Whitespace\" \
             \")))))((Grout((id 7b511b73-9d61-4a77-9114-579206175a33)(shape \
             Convex))))))(ancestors())))(caret \
             Outer)(refractors((manuals())(multis((ids())(suppressed())(ephemerals())))(sample_focus((call_stack())(index \
             -1)(pinned_stack())(indicated_call())(time())(seq \
             0)(step_range())(pending_focus())))(autoprobe_target())(pending_probe_cursor()))))";
          backup_text = "let $ab = of_ctx [A /\\ B] end in ";
        };
      rule_set = PropositionalLogic;
      trees =
        [
          Node
            ( Just
                {
                  jdmt =
                    {
                      zipper =
                        "((selection((focus Left)(content())(mode \
                         Normal)(anchor_caret Outer)(smart_rounded \
                         false)))(relatives((siblings(((Tile((id \
                         2015673a-36fd-4910-b26b-c31c817d053a)(form(Tok \
                         $ab))(sort(Drv Exp))))(Secondary((id \
                         e158068c-3408-4ac6-80bc-5028f00418e6)(content(Whitespace\" \
                         \"))))(Tile((id \
                         192eb4a2-03e8-4802-936c-19b8d443b348)(form(Compound \
                         Entail))(sort(Drv Exp))))(Secondary((id \
                         b70e0377-5998-4d83-8c20-5fc9ec00c6db)(content(Whitespace\" \
                         \"))))(Tile((id \
                         6d089f71-e60f-436f-9b66-ffe396826038)(form(Tok \
                         A))(sort(Drv Exp))))(Secondary((id \
                         68f3f1ca-4792-4ebc-bb3d-747811f46986)(content(Whitespace\" \
                         \"))))(Tile((id \
                         37b7bf1d-7086-412d-bc1d-d2329c3515a9)(form(Compound \
                         And))(sort(Drv Exp))))(Secondary((id \
                         4d1e36d6-1cde-40c2-a659-bb871b7c3e80)(content(Whitespace\" \
                         \"))))(Tile((id \
                         6c695218-ee9a-4082-9a66-6099cff1ba35)(form(Tok \
                         B))(sort(Drv Exp)))))()))(ancestors())))(caret \
                         Outer)(refractors((manuals())(multis((ids())(suppressed())(ephemerals())))(sample_focus((call_stack())(index \
                         -1)(pinned_stack())(indicated_call())(time())(seq \
                         0)(step_range())(pending_focus())))(autoprobe_target())(pending_probe_cursor()))))";
                      backup_text = "$ab |- A /\\ B";
                    };
                  rule = Some Assumption;
                },
              [] );
          Node
            ( Just
                {
                  jdmt =
                    {
                      zipper =
                        "((selection((focus Left)(content())(mode \
                         Normal)(anchor_caret Outer)(smart_rounded \
                         false)))(relatives((siblings(((Tile((id \
                         702881a5-280c-4060-a1e0-b6ddbc80723b)(form(Tok \
                         []))(sort(Drv Exp))))(Secondary((id \
                         eaaaa299-65ed-46fe-b30b-13104f119468)(content(Whitespace\" \
                         \"))))(Tile((id \
                         d3f30556-cdde-41eb-bfdc-ad1946209359)(form(Compound \
                         Entail))(sort(Drv Exp))))(Secondary((id \
                         ec75f7f2-5ac3-432f-a597-d678d4d5be29)(content(Whitespace\" \
                         \"))))(Tile((id \
                         0ab6175f-6d31-4d22-976c-03acd5bd2592)(form(Compound \
                         Parens))(sort(Drv Exp))(shards(0 \
                         1))(children(((Tile((id \
                         7314a824-01d6-476b-b6ac-2e5e5eee2055)(form(Tok \
                         A))(sort(Drv Exp))))(Secondary((id \
                         72c5d9ab-11b7-4b0b-acdf-fbb9dd3bd7e6)(content(Whitespace\" \
                         \"))))(Tile((id \
                         24675a2c-e600-4283-8e68-907f9265282f)(form(Compound \
                         And))(sort(Drv Exp))))(Secondary((id \
                         878db26c-7bb5-42ab-b878-a535c02563b4)(content(Whitespace\" \
                         \"))))(Tile((id \
                         6730d5f2-097f-4bf5-b6b8-668a64b94a66)(form(Tok \
                         B))(sort(Drv Exp)))))))))(Secondary((id \
                         dddf1249-daaa-4eb7-8bcb-52e5724ccc29)(content(Whitespace\" \
                         \"))))(Tile((id \
                         e1e64744-3712-4dc2-8632-a977ff098d6f)(form(Compound \
                         Impl))(sort(Drv Exp))))(Secondary((id \
                         b746e7bc-0426-4398-9c99-936f95ac2b9e)(content(Whitespace\" \
                         \"))))(Tile((id \
                         17f897c7-adfe-4144-9103-defa8a2070d7)(form(Compound \
                         Parens))(sort(Drv Exp))(shards(0 \
                         1))(children(((Tile((id \
                         483978ac-74d1-4ea8-99b3-5b38c0b57660)(form(Tok \
                         B))(sort(Drv Exp))))(Secondary((id \
                         464453d5-2a70-4e26-a0e8-2b1319c8b172)(content(Whitespace\" \
                         \"))))(Tile((id \
                         ee54286d-fa6d-4828-89e6-0e34846d29d4)(form(Compound \
                         And))(sort(Drv Exp))))(Secondary((id \
                         e58e1847-c5f3-4bdf-8a8b-224bc39bcb2c)(content(Whitespace\" \
                         \"))))(Tile((id \
                         fcc47be3-2c2e-411d-a3d9-8760f269edac)(form(Tok \
                         A))(sort(Drv Exp))))))))))()))(ancestors())))(caret \
                         Outer)(refractors((manuals())(multis((ids())(suppressed())(ephemerals())))(sample_focus((call_stack())(index \
                         -1)(pinned_stack())(indicated_call())(time())(seq \
                         0)(step_range())(pending_focus())))(autoprobe_target())(pending_probe_cursor()))))";
                      backup_text = "[] |- (A /\\ B) ==> (B /\\ A)";
                    };
                  rule = Some Implies_I;
                },
              [
                Node
                  ( Just
                      {
                        jdmt =
                          {
                            zipper =
                              "((selection((focus Left)(content())(mode \
                               Normal)(anchor_caret Outer)(smart_rounded \
                               false)))(relatives((siblings(((Tile((id \
                               3c7eccb5-b10c-4144-bae1-791529ea05a0)(form(Tok \
                               $ab))(sort(Drv Exp))))(Secondary((id \
                               14c5e54e-cbdd-46cb-8b45-494c25c4bc59)(content(Whitespace\" \
                               \"))))(Tile((id \
                               3da34abf-ec3e-42bd-b96c-8c7bbf120494)(form(Compound \
                               Entail))(sort(Drv Exp))))(Secondary((id \
                               95551790-e641-4b18-b43f-5d0dae04923f)(content(Whitespace\" \
                               \"))))(Tile((id \
                               da0d4546-b66a-42c7-906a-67c8e01ebc14)(form(Tok \
                               B))(sort(Drv Exp))))(Secondary((id \
                               4f1ed6a5-1711-4cf0-83ca-846217e50425)(content(Whitespace\" \
                               \"))))(Tile((id \
                               512c3d91-5cad-49cd-ac5c-b23e65ef262a)(form(Compound \
                               And))(sort(Drv Exp))))(Secondary((id \
                               697f84d8-17e9-4bd1-a49a-4db065b7157a)(content(Whitespace\" \
                               \"))))(Tile((id \
                               996cc49c-e5d7-4031-a076-1841a8639e8b)(form(Tok \
                               A))(sort(Drv Exp)))))()))(ancestors())))(caret \
                               Outer)(refractors((manuals())(multis((ids())(suppressed())(ephemerals())))(sample_focus((call_stack())(index \
                               -1)(pinned_stack())(indicated_call())(time())(seq \
                               0)(step_range())(pending_focus())))(autoprobe_target())(pending_probe_cursor()))))";
                            backup_text = "$ab |- B /\\ A";
                          };
                        rule = Some And_I;
                      },
                    [
                      Node
                        ( Just
                            {
                              jdmt =
                                {
                                  zipper =
                                    "((selection((focus Left)(content())(mode \
                                     Normal)(anchor_caret Outer)(smart_rounded \
                                     false)))(relatives((siblings(((Tile((id \
                                     e5f44296-5619-4178-a579-5d4d0b7a82ae)(form(Tok \
                                     $ab))(sort(Drv Exp))))(Secondary((id \
                                     12cb7cfc-57c4-4731-93f0-9c7143a37050)(content(Whitespace\" \
                                     \"))))(Tile((id \
                                     e47b0c8f-0df6-47ba-856b-c9be8141c2aa)(form(Compound \
                                     Entail))(sort(Drv Exp))))(Secondary((id \
                                     0289ae54-bb11-4ca8-a4d1-7b50ed9cb382)(content(Whitespace\" \
                                     \"))))(Tile((id \
                                     dfd798d4-c53f-47bd-ad3c-1bf477457340)(form(Tok \
                                     B))(sort(Drv \
                                     Exp)))))()))(ancestors())))(caret \
                                     Outer)(refractors((manuals())(multis((ids())(suppressed())(ephemerals())))(sample_focus((call_stack())(index \
                                     -1)(pinned_stack())(indicated_call())(time())(seq \
                                     0)(step_range())(pending_focus())))(autoprobe_target())(pending_probe_cursor()))))";
                                  backup_text = "$ab |- B";
                                };
                              rule = Some And_E_R;
                            },
                          [ Node (Abbr (Some 0), []) ] );
                      Node
                        ( Just
                            {
                              jdmt =
                                {
                                  zipper =
                                    "((selection((focus Left)(content())(mode \
                                     Normal)(anchor_caret Outer)(smart_rounded \
                                     false)))(relatives((siblings(((Tile((id \
                                     e52a13c3-434f-42b0-ae0a-c8f880bdaf3f)(form(Tok \
                                     $ab))(sort(Drv Exp))))(Secondary((id \
                                     de4611f1-f3ed-436c-828d-61b7b9752172)(content(Whitespace\" \
                                     \"))))(Tile((id \
                                     1ec2c2d2-a78f-46b4-9295-e658938dc930)(form(Compound \
                                     Entail))(sort(Drv Exp))))(Secondary((id \
                                     3ab68d07-db88-4647-b751-8ee56bc315ca)(content(Whitespace\" \
                                     \"))))(Tile((id \
                                     ce52b876-7256-448a-a000-98b39dc245e9)(form(Tok \
                                     A))(sort(Drv \
                                     Exp)))))()))(ancestors())))(caret \
                                     Outer)(refractors((manuals())(multis((ids())(suppressed())(ephemerals())))(sample_focus((call_stack())(index \
                                     -1)(pinned_stack())(indicated_call())(time())(seq \
                                     0)(step_range())(pending_focus())))(autoprobe_target())(pending_probe_cursor()))))";
                                  backup_text = "$ab |- A";
                                };
                              rule = Some And_E_L;
                            },
                          [ Node (Abbr (Some 0), []) ] );
                    ] );
              ] );
        ];
    }
