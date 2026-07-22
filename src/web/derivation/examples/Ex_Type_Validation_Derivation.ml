let exercise : DerivationExercise.spec =
  DerivationExercise.of_persistent
    {
      id = Haz3lcore.Id.v "f73cdb5d-76b5-4675-82cd-b7ccf757dd27";
      title = "Type Validation Derivation";
      module_name = "Ex_Type_Validation_Derivation";
      prompt = "";
      max_points = 10;
      prelude =
        {
          zipper =
            "((selection((focus Left)(content())(mode Normal)(anchor_caret \
             Outer)(smart_rounded false)))(relatives((siblings(()((Grout((id \
             efc74579-6572-44f2-aee4-ded4e05dfd42)(shape \
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
             ea4e8e49-f788-4555-bf43-aa2e86aadfdc)(form(Compound \
             Let))(shards(0 1 2))(children(((Secondary((id \
             a99970f0-2825-490b-b0d8-9ac7a9cb169d)(content(Whitespace\" \
             \"))))(Tile((id 73edde71-179a-43e4-925d-d07c9556e5e6)(form(Tok \
             $delta))(sort Pat)))(Secondary((id \
             2841ed8b-cb83-412a-a01a-b1dd96287796)(content(Whitespace\" \
             \")))))((Secondary((id \
             cc6d4679-b805-443e-8d04-cf0ca8d33d8b)(content(Whitespace\" \
             \"))))(Tile((id \
             2b957ca5-4818-40bf-95a9-3a212f7f1fbe)(form(Compound \
             OfCtx))(shards(0 1))(children(((Secondary((id \
             66b422fc-b59b-4290-9960-8ebad9a9ca28)(content(Whitespace\" \
             \"))))(Tile((id \
             17ea0517-02a3-4235-b2e3-3ee05cdcc0bf)(form(Compound \
             ListLit))(sort(Drv Exp))(shards(0 1))(children(((Tile((id \
             e865fc60-5f83-4390-9d92-76f5da185e1f)(form(Compound \
             Valid))(sort(Drv Exp))(shards(0 1))(children(((Secondary((id \
             fb1c6779-c9f9-4042-8602-a51d84a0a115)(content(Whitespace\" \
             \"))))(Tile((id ec168d40-47cc-4ae9-966e-4c0aba983f40)(form(Tok \
             A))(sort(Drv Typ))))(Secondary((id \
             52683f9a-5430-4332-95af-ea129deedb0f)(content(Whitespace\" \
             \"))))))))))))))(Secondary((id \
             3175b831-ecda-4bfb-8a24-6af29484c28e)(content(Whitespace\" \
             \")))))))))(Secondary((id \
             03cb6fc9-9a95-4723-93da-66fa6b8f333e)(content(Whitespace\" \
             \")))))))))(Secondary((id \
             cd9bffec-6017-4ce5-a8c6-a66458906a0d)(content(Whitespace\"\\n\"))))(Tile((id \
             749e52ea-a567-402a-9c90-a46b4bac42b8)(form(Compound \
             Let))(shards(0 1 2))(children(((Secondary((id \
             655de83f-207e-4e74-910f-8aac136e540b)(content(Whitespace\" \
             \"))))(Tile((id 1b9f1a43-6f94-4f68-bcd0-68e7f40553f4)(form(Tok \
             $delta'))(sort Pat)))(Secondary((id \
             0cd7d91c-1714-4ac4-bf8e-32eb098558d5)(content(Whitespace\" \
             \")))))((Secondary((id \
             1c01deef-fdef-4cf5-87de-a58f06a7e873)(content(Whitespace\" \
             \"))))(Tile((id \
             de5e03fe-67f4-40a2-b76b-46dc72812136)(form(Compound \
             OfCtx))(shards(0 1))(children(((Secondary((id \
             1ea30ffe-b43c-47ad-a8ba-c90159baadd5)(content(Whitespace\" \
             \"))))(Tile((id \
             29893dc1-9047-48a8-8ebc-1bbe08fc6380)(form(Compound \
             Parens))(sort(Drv Exp))(shards(0 1))(children(((Tile((id \
             58277b54-a926-45c6-abcd-15421a9cb45a)(form(Tok x))(sort(Drv \
             Exp))))(Secondary((id \
             9a0aba9d-033e-4465-8963-770dd48f264f)(content(Whitespace\" \
             \"))))(Tile((id \
             be82200c-17e0-4e44-8c8a-f557c0ab829b)(form(Compound \
             TypeAsc))(sort(Drv Exp))))(Secondary((id \
             df6c4491-c321-46cb-9a63-bd504557306c)(content(Whitespace\" \
             \"))))(Tile((id 0fb633b2-e196-4757-841c-a3d8ed9068f7)(form(Tok \
             A))(sort(Drv Typ)))))))))(Tile((id \
             a07f4453-3f0d-4512-a5c8-2cbfac3ef0de)(form(Compound \
             Cons))(sort(Drv Exp))))(Tile((id \
             4aaba933-aa7c-4fbd-acb3-b2501d88c50c)(form(Tok $delta))(sort(Drv \
             Exp))))(Secondary((id \
             07739aca-4d41-49f8-8393-5962c2b0c9f4)(content(Whitespace\" \
             \")))))))))(Secondary((id \
             44416a57-b035-4143-878e-d9e4ebd3069c)(content(Whitespace\" \
             \"))))))))))((Grout((id \
             32e53e29-00db-400c-ac2d-f50181ae32e4)(shape \
             Convex))))))(ancestors())))(caret \
             Outer)(refractors((manuals())(multis((ids())(suppressed())(ephemerals())))(sample_focus((call_stack())(index \
             -1)(pinned_stack())(indicated_call())(time())(seq \
             0)(step_range())(pending_focus())))(autoprobe_target())(pending_probe_cursor()))))";
          backup_text =
            "let $delta = of_ctx [valid A end] end in\n\
             let $delta' = of_ctx (x : A)::$delta end in";
        };
      rule_set = RecursiveALFA;
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
                         4ef3992d-3603-49ed-a720-7cda9d1eeb25)(form(Tok \
                         $delta))(sort(Drv Exp))))(Secondary((id \
                         e9527773-5d26-48cd-bc21-c04d52c301de)(content(Whitespace\" \
                         \"))))(Tile((id \
                         dd2eea06-6633-424e-9c0b-ce9c3d095941)(form(Compound \
                         Entail))(sort(Drv Exp))))(Secondary((id \
                         0a00a6e3-ffbd-48a3-b9e3-79e8b0cbe161)(content(Whitespace\" \
                         \"))))(Tile((id \
                         2966710b-e4fb-4d87-afa2-612e1ac06e63)(form(Compound \
                         Parens))(sort(Drv Exp))(shards(0 \
                         1))(children(((Tile((id \
                         f6f9bfe4-9951-44ea-8f20-bd2080fee2ea)(form(Compound \
                         Fun))(sort(Drv Exp))(shards(0 \
                         1))(children(((Secondary((id \
                         dfbbc135-13b3-435b-a3d2-936a3c428525)(content(Whitespace\" \
                         \"))))(Tile((id \
                         cef7f9d3-526f-4144-8a25-8fe7949eec25)(form(Tok \
                         x))(sort(Drv Pat))))(Secondary((id \
                         b079327e-6620-4ebd-bf5f-fb86fc32b8c2)(content(Whitespace\" \
                         \"))))(Tile((id \
                         8c86efaf-9b28-4fa0-817f-6f9850955ab4)(form(Compound \
                         TypeAsc))(sort(Drv Pat))))(Secondary((id \
                         2c4ecd25-4445-45ca-aaf0-fdcd330e8c46)(content(Whitespace\" \
                         \"))))(Tile((id \
                         fe40a9db-3912-4ca5-ab59-0b011fe9b849)(form(Tok \
                         A))(sort(Drv Typ))))(Secondary((id \
                         ae97589e-f864-437f-b57b-d23886278406)(content(Whitespace\" \
                         \")))))))))(Secondary((id \
                         7192caeb-7e70-4f66-9230-ed9747349ec7)(content(Whitespace\" \
                         \"))))(Tile((id \
                         e01bde5c-f15c-405d-ae94-7856c6dd1929)(form(Tok \
                         x))(sort(Drv Exp)))))))))(Secondary((id \
                         e4d8403e-44f8-4fe9-8513-82ee34a86054)(content(Whitespace\" \
                         \"))))(Tile((id \
                         e7a43e81-efbc-414a-8a04-b9fa51ff594c)(form(Compound \
                         TypeAsc))(sort(Drv Exp))))(Secondary((id \
                         3bde8d54-89d8-4dd8-b08e-5a449e2c625c)(content(Whitespace\" \
                         \"))))(Tile((id \
                         4fecae4c-cc47-4644-8bb6-929a82e282b3)(form(Tok \
                         A))(sort(Drv Typ))))(Secondary((id \
                         05eafb45-e6c9-45f9-8f87-4b3e54e927a8)(content(Whitespace\" \
                         \"))))(Tile((id \
                         59ac74bc-925b-4be8-9ccf-f744865a9d98)(form(Compound \
                         TypeArrow))(sort(Drv Typ))))(Secondary((id \
                         d251984e-e72a-4e80-91bb-dc1a6479a0fc)(content(Whitespace\" \
                         \"))))(Tile((id \
                         ea30a991-2c1d-4a74-abc8-8f722dceaf57)(form(Tok \
                         A))(sort(Drv Typ)))))()))(ancestors())))(caret \
                         Outer)(refractors((manuals())(multis((ids())(suppressed())(ephemerals())))(sample_focus((call_stack())(index \
                         -1)(pinned_stack())(indicated_call())(time())(seq \
                         0)(step_range())(pending_focus())))(autoprobe_target())(pending_probe_cursor()))))";
                      backup_text = "$delta |- (fun x : A -> x) : A -> A";
                    };
                  rule = Some T_FunAnn;
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
                               c22ea463-7fbc-48b5-94c1-356af6bdf567)(form(Tok \
                               $delta))(sort(Drv Exp))))(Secondary((id \
                               2a868d06-d848-4f4c-9b17-0ad384ca5d06)(content(Whitespace\" \
                               \"))))(Tile((id \
                               cb62de06-a23a-4d25-b8ec-f5249ce3dbc4)(form(Compound \
                               Entail))(sort(Drv Exp))))(Secondary((id \
                               62a370d9-d7f7-4671-9c36-6bad8cab9796)(content(Whitespace\" \
                               \"))))(Tile((id \
                               ffa38068-589d-4fcc-bef1-6e4ee34dc24d)(form(Compound \
                               Valid))(sort(Drv Exp))(shards(0 \
                               1))(children(((Secondary((id \
                               dccfe72d-3858-4dfe-8912-f7eb46b8df83)(content(Whitespace\" \
                               \"))))(Tile((id \
                               9c1b2b63-c4e3-4270-baaf-c025642bf6b7)(form(Tok \
                               A))(sort(Drv Typ))))(Secondary((id \
                               5f130cc6-9023-47b5-ae0f-85551b61f85f)(content(Whitespace\" \
                               \"))))))))))()))(ancestors())))(caret \
                               Outer)(refractors((manuals())(multis((ids())(suppressed())(ephemerals())))(sample_focus((call_stack())(index \
                               -1)(pinned_stack())(indicated_call())(time())(seq \
                               0)(step_range())(pending_focus())))(autoprobe_target())(pending_probe_cursor()))))";
                            backup_text = "$delta |- valid A end";
                          };
                        rule = Some TV_TVar;
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
                               f4ed0ea8-9dde-4c62-8e35-8b131f4fe3bd)(form(Tok \
                               $delta'))(sort(Drv Exp))))(Secondary((id \
                               c4c6d2d6-563f-4a79-bdc1-3fe84d31acec)(content(Whitespace\" \
                               \"))))(Tile((id \
                               8b5a6faa-b279-4e42-b596-f26cab63dbb8)(form(Compound \
                               Entail))(sort(Drv Exp))))(Secondary((id \
                               ae582de4-933e-4d39-afe6-6ee895b66113)(content(Whitespace\" \
                               \"))))(Tile((id \
                               561d401c-fc34-497f-9cd8-a4e50f55ee58)(form(Tok \
                               x))(sort(Drv Exp))))(Secondary((id \
                               306482a5-f820-4352-acd6-36726074496c)(content(Whitespace\" \
                               \"))))(Tile((id \
                               2aec344a-dd13-41f7-b044-719a6aa7c5d7)(form(Compound \
                               TypeAsc))(sort(Drv Exp))))(Secondary((id \
                               8e5d7b8e-08b7-40d3-ad1f-33c90e85a9cf)(content(Whitespace\" \
                               \"))))(Tile((id \
                               5a536b52-485d-40c7-925e-4c694e4ec61d)(form(Tok \
                               A))(sort(Drv Typ)))))()))(ancestors())))(caret \
                               Outer)(refractors((manuals())(multis((ids())(suppressed())(ephemerals())))(sample_focus((call_stack())(index \
                               -1)(pinned_stack())(indicated_call())(time())(seq \
                               0)(step_range())(pending_focus())))(autoprobe_target())(pending_probe_cursor()))))";
                            backup_text = "$delta' |- x : A";
                          };
                        rule = Some T_Var;
                      },
                    [] );
              ] );
        ];
    }
