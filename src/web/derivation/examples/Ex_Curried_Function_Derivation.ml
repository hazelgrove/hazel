let exercise : DerivationExercise.spec =
  DerivationExercise.of_persistent
    {
      id = Haz3lcore.Id.v "6877c1e5-4421-40c8-9da3-9b41f3e48a3d";
      title = "Curried Function Derivation";
      module_name = "Ex_Curried_Function_Derivation";
      prompt =
        "Provide a derivation of the following judgement, which establishes \
         that the curried \226\128\156min\226\128\157 function in ALFp has \
         type: Num \226\134\146 Num \226\134\146 Num";
      max_points = 10;
      prelude =
        {
          zipper =
            "((selection((focus Left)(content())(mode Normal)(anchor_caret \
             Outer)(smart_rounded false)))(relatives((siblings(()((Grout((id \
             42b8e844-b468-4b26-b6e0-dae13ea4cfdd)(shape \
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
             80edce76-d618-4026-95be-c1538b6460fe)(form(Compound \
             Let))(shards(0 1 2))(children(((Secondary((id \
             32e8c432-092a-4f6a-abde-0a8bd8d729f7)(content(Whitespace\" \
             \"))))(Tile((id 8e481961-ead2-4502-b33b-a0106afbb7e8)(form(Tok \
             $ctx_a))(sort Pat)))(Secondary((id \
             dba9569d-506c-45a6-bba6-fffd49a23988)(content(Whitespace\" \
             \")))))((Secondary((id \
             1f805ffb-78b2-49b3-9937-80946f7b4b8f)(content(Whitespace\" \
             \"))))(Tile((id \
             0f16bb2b-68f3-4ceb-a689-e83f09178456)(form(Compound \
             OfCtx))(shards(0 1))(children(((Secondary((id \
             b51725fc-6ca9-4c9c-a251-84bfe47278e8)(content(Whitespace\" \
             \"))))(Tile((id \
             55147ab7-4b52-43e4-aff4-4275b3b28b0b)(form(Compound \
             Parens))(sort(Drv Exp))(shards(0 1))(children(((Tile((id \
             99080d99-8800-4b12-b355-371055151b3e)(form(Tok a))(sort(Drv \
             Exp))))(Secondary((id \
             3307a821-e669-44e4-b9e6-6bc7f2f83071)(content(Whitespace\" \
             \"))))(Tile((id \
             871fdc5a-9ef1-4167-b3ba-0f8f6e36464c)(form(Compound \
             TypeAsc))(sort(Drv Exp))))(Secondary((id \
             addf12b6-78fe-4b2b-a59d-309ff1c1341f)(content(Whitespace\" \
             \"))))(Tile((id 17244263-91fb-4082-9340-22f5154c1c23)(form(Tok \
             Num))(sort(Drv Typ)))))))))(Tile((id \
             f402f2c5-fb00-42ef-a5ff-9e70612806cd)(form(Compound \
             Cons))(sort(Drv Exp))))(Tile((id \
             fa377bde-0b0f-4eff-a9ba-2559fd5daa51)(form(Tok []))(sort(Drv \
             Exp))))(Secondary((id \
             8360ba6e-3317-443e-b07c-d593bbb38a37)(content(Whitespace\" \
             \")))))))))(Secondary((id \
             74a9c11c-7523-4e99-8f64-aa95201ee3d9)(content(Whitespace\" \
             \")))))))))(Secondary((id \
             bc099970-61f0-473c-8cee-3559f79a4214)(content(Whitespace\"\\n\"))))(Tile((id \
             c59f0ab5-cf80-43a0-9418-a1511fb5a6d5)(form(Compound \
             Let))(shards(0 1 2))(children(((Secondary((id \
             7fcfa33d-3543-4ae1-acb8-d0c5bb328cee)(content(Whitespace\" \
             \"))))(Tile((id ea65f0e9-92f0-4925-b762-224938dffbf9)(form(Tok \
             $ctx_ab))(sort Pat)))(Secondary((id \
             ee5294f8-80f2-466f-8b4b-819b81961ad6)(content(Whitespace\" \
             \")))))((Secondary((id \
             6234a57b-144d-40db-8022-c0b0b846082a)(content(Whitespace\" \
             \"))))(Tile((id \
             60f61db6-660d-4f95-9c91-c7437a935bcb)(form(Compound \
             OfCtx))(shards(0 1))(children(((Secondary((id \
             874b7597-e717-441c-8de7-d16226adf7e4)(content(Whitespace\" \
             \"))))(Tile((id \
             f93902fe-3847-4d78-9c2d-2d0a175aa9e0)(form(Compound \
             Parens))(sort(Drv Exp))(shards(0 1))(children(((Tile((id \
             6189268f-4558-407e-a29a-b5e2efd5c8c5)(form(Tok b))(sort(Drv \
             Exp))))(Secondary((id \
             b431771a-507d-4fc7-9975-40a7acc1971f)(content(Whitespace\" \
             \"))))(Tile((id \
             bdda5a1a-67ae-48a2-afaa-9eeab6ca77d8)(form(Compound \
             TypeAsc))(sort(Drv Exp))))(Secondary((id \
             cac4d002-a872-4165-a018-3627d8c16788)(content(Whitespace\" \
             \"))))(Tile((id 5505d465-dd55-49b2-b290-f5d3c4062d1d)(form(Tok \
             Num))(sort(Drv Typ)))))))))(Tile((id \
             4fb47eb4-9549-4794-b3f4-ace826f7a65b)(form(Compound \
             Cons))(sort(Drv Exp))))(Tile((id \
             089c6765-6037-43d5-9d89-5c7eab49460d)(form(Tok $ctx_a))(sort(Drv \
             Exp))))(Secondary((id \
             7e8279af-a149-4462-804b-f88cb42f0da9)(content(Whitespace\" \
             \")))))))))(Secondary((id \
             1725294a-5f42-4d02-99ce-29d426226bcd)(content(Whitespace\" \
             \"))))))))))((Grout((id \
             4b9328bc-5f8b-4a5b-ad6e-24e77283fe10)(shape \
             Convex))))))(ancestors())))(caret \
             Outer)(refractors((manuals())(multis((ids())(suppressed())(ephemerals())))(sample_focus((call_stack())(index \
             -1)(pinned_stack())(indicated_call())(time())(seq \
             0)(step_range())(pending_focus())))(autoprobe_target())(pending_probe_cursor()))))";
          backup_text =
            "let $ctx_a = of_ctx (a : Num)::[] end in\n\
             let $ctx_ab = of_ctx (b : Num)::$ctx_a end in";
        };
      rule_set = ALFp;
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
                         93ad105a-4aa2-464e-9bd0-95c9398a945e)(form(Tok \
                         $ctx_ab))(sort(Drv Exp))))(Secondary((id \
                         19dad835-a642-4a23-bf7f-e90a84f21162)(content(Whitespace\" \
                         \"))))(Tile((id \
                         39184a60-8401-4f92-8e74-af1b77bd89be)(form(Compound \
                         Entail))(sort(Drv Exp))))(Secondary((id \
                         322d35ef-f080-4bab-b669-904abe9743d9)(content(Whitespace\" \
                         \"))))(Tile((id \
                         1744af66-d16f-4be8-9d18-5927e3bbbfcf)(form(Tok \
                         a))(sort(Drv Exp))))(Secondary((id \
                         2588df2b-e020-42b3-a9b8-42b871b7b3f5)(content(Whitespace\" \
                         \"))))(Tile((id \
                         c44c104a-e09a-4037-93a8-ac716ed2acb5)(form(Compound \
                         TypeAsc))(sort(Drv Exp))))(Secondary((id \
                         7caa7696-1f3d-4aaf-bf68-dd4e521813e9)(content(Whitespace\" \
                         \"))))(Tile((id \
                         f7982761-dd07-4f7c-9645-5765ea94f8bc)(form(Tok \
                         Num))(sort(Drv Typ)))))()))(ancestors())))(caret \
                         Outer)(refractors((manuals())(multis((ids())(suppressed())(ephemerals())))(sample_focus((call_stack())(index \
                         -1)(pinned_stack())(indicated_call())(time())(seq \
                         0)(step_range())(pending_focus())))(autoprobe_target())(pending_probe_cursor()))))";
                      backup_text = "$ctx_ab |- a : Num";
                    };
                  rule = Some T_Var;
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
                         67040479-eb11-49d2-9ae3-7386abe43f78)(form(Tok \
                         $ctx_ab))(sort(Drv Exp))))(Secondary((id \
                         72d9bd57-3124-44bc-a26f-2b09b748b9ad)(content(Whitespace\" \
                         \"))))(Tile((id \
                         b72a4fd6-ce12-41fd-8d0c-1145a0777fd6)(form(Compound \
                         Entail))(sort(Drv Exp))))(Secondary((id \
                         54365f6d-f0bc-4c17-9705-bd2ea2dd7e79)(content(Whitespace\" \
                         \"))))(Tile((id \
                         90ab05f0-373d-4104-ae83-9422b14ceb63)(form(Tok \
                         b))(sort(Drv Exp))))(Secondary((id \
                         dba399ff-3598-4eee-9af4-f91ce6e18ea9)(content(Whitespace\" \
                         \"))))(Tile((id \
                         f7b5bdc5-f980-4a57-bb42-48bf3bc3a99c)(form(Compound \
                         TypeAsc))(sort(Drv Exp))))(Secondary((id \
                         766debed-0acc-4730-b3b3-04b18a7d15f7)(content(Whitespace\" \
                         \"))))(Tile((id \
                         d6466a4b-8bf5-4f12-9f59-5d7bd6ee1e63)(form(Tok \
                         Num))(sort(Drv Typ)))))()))(ancestors())))(caret \
                         Outer)(refractors((manuals())(multis((ids())(suppressed())(ephemerals())))(sample_focus((call_stack())(index \
                         -1)(pinned_stack())(indicated_call())(time())(seq \
                         0)(step_range())(pending_focus())))(autoprobe_target())(pending_probe_cursor()))))";
                      backup_text = "$ctx_ab |- b : Num";
                    };
                  rule = Some T_Var;
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
                         71466475-db41-49d8-8f3c-24431968f326)(form(Compound \
                         UnaryEntail))(sort(Drv Exp))))(Secondary((id \
                         7be0c8ab-7253-465a-b721-f70fc8fb584b)(content(Whitespace\" \
                         \"))))(Tile((id \
                         7ea2e5fc-feaa-42d3-adfd-36a8abba04de)(form(Compound \
                         Parens))(sort(Drv Exp))(shards(0 \
                         1))(children(((Tile((id \
                         13682bc0-a27b-491a-b278-eb1665ee98de)(form(Compound \
                         Fun))(sort(Drv Exp))(shards(0 \
                         1))(children(((Secondary((id \
                         405986f3-b539-4e7d-8c30-3812ee1251f1)(content(Whitespace\" \
                         \"))))(Tile((id \
                         a5d6e827-6fb5-4a65-a9e8-f6502b3ca526)(form(Tok \
                         a))(sort(Drv Pat))))(Secondary((id \
                         b3b3ce29-b799-4d19-ba3a-0b0d55cc135c)(content(Whitespace\" \
                         \"))))(Tile((id \
                         763c4026-5a31-4b95-9cb8-7b8aaed55afd)(form(Compound \
                         TypeAsc))(sort(Drv Pat))))(Secondary((id \
                         70a2548e-895e-4691-a1f0-e7c6a50fdb41)(content(Whitespace\" \
                         \"))))(Tile((id \
                         8beeb565-1dd4-4ef1-bbcb-9d6071595f07)(form(Tok \
                         Num))(sort(Drv Typ))))(Secondary((id \
                         06cdbd03-df69-4cbe-bed1-55e7c5daa2aa)(content(Whitespace\" \
                         \")))))))))(Secondary((id \
                         ffb78975-cc60-4891-8dfe-2eabc135c9ff)(content(Whitespace\" \
                         \"))))(Tile((id \
                         028f2c63-c8e0-470e-9454-ef41074e0267)(form(Compound \
                         Fun))(sort(Drv Exp))(shards(0 \
                         1))(children(((Secondary((id \
                         5303057a-c40a-4759-9740-f9a65c26d2df)(content(Whitespace\" \
                         \"))))(Tile((id \
                         15ae5093-26bf-46ea-b8b5-6ea01848f3dd)(form(Tok \
                         b))(sort(Drv Pat))))(Secondary((id \
                         c5963d09-2aff-4ee6-a25a-ae7ed1dd820c)(content(Whitespace\" \
                         \"))))(Tile((id \
                         211edcc1-3823-46cd-9430-61095600ec83)(form(Compound \
                         TypeAsc))(sort(Drv Pat))))(Secondary((id \
                         838cc9e6-d598-4c3e-8200-298e89e0f614)(content(Whitespace\" \
                         \"))))(Tile((id \
                         01121056-0be6-4915-8429-e31b85464fad)(form(Tok \
                         Num))(sort(Drv Typ))))(Secondary((id \
                         8e41fe3c-5820-4f11-9d38-97d1b41a92a3)(content(Whitespace\" \
                         \")))))))))(Secondary((id \
                         6c9f952e-628f-438d-a384-3ba0e0586d8f)(content(Whitespace\" \
                         \"))))(Tile((id \
                         b342bd14-b6cb-4a37-a303-f45533d86cca)(form(Compound \
                         If))(sort(Drv Exp))(shards(0 1 \
                         2))(children(((Secondary((id \
                         147493dd-6221-4f29-b923-1e26b001bf55)(content(Whitespace\" \
                         \"))))(Tile((id \
                         f94587c5-a444-4cd1-b3be-92bd40594208)(form(Tok \
                         a))(sort(Drv Exp))))(Secondary((id \
                         804a1cc7-5eac-4ad2-b448-47cf55538318)(content(Whitespace\" \
                         \"))))(Tile((id \
                         2441d522-a56f-49e4-893d-67ced48d795a)(form(Compound \
                         Lt))(sort(Drv Exp))))(Secondary((id \
                         8958d30e-a950-4f6f-a794-a35c26c69a20)(content(Whitespace\" \
                         \"))))(Tile((id \
                         45a532ab-eb9d-4ad8-99a4-1c57ecbb4524)(form(Tok \
                         b))(sort(Drv Exp))))(Secondary((id \
                         8a71ef6b-4f83-48be-b197-8cb6db015487)(content(Whitespace\" \
                         \")))))((Secondary((id \
                         7470bf42-a5ff-4258-987f-afabbdb76d0c)(content(Whitespace\" \
                         \"))))(Tile((id \
                         e405331e-e1d8-4f0e-9b8e-65d59c1b7b3b)(form(Tok \
                         a))(sort(Drv Exp))))(Secondary((id \
                         9e04b648-c8ef-48e4-901f-6c437a2631f1)(content(Whitespace\" \
                         \")))))))))(Secondary((id \
                         fd37aa24-1ea9-4e5d-8850-1f6e855ac1d4)(content(Whitespace\" \
                         \"))))(Tile((id \
                         896872bf-3b7a-40dd-9cf5-90416dd8df77)(form(Tok \
                         b))(sort(Drv Exp)))))))))(Secondary((id \
                         5aa4ebbc-fa7f-40cb-a1aa-0d9309fee99b)(content(Whitespace\" \
                         \"))))(Tile((id \
                         f03f0aca-012a-48ac-aada-695cc319f66d)(form(Compound \
                         TypeAsc))(sort(Drv Exp))))(Secondary((id \
                         97230722-37b5-48b1-96b5-9c943d936a34)(content(Whitespace\" \
                         \"))))(Tile((id \
                         57540bc2-6b85-47a3-a7c8-47501e9e0da8)(form(Tok \
                         Num))(sort(Drv Typ))))(Secondary((id \
                         237458fc-6597-4fab-a46a-74785568bf56)(content(Whitespace\" \
                         \"))))(Tile((id \
                         2a94b019-90af-4e6e-a049-0470a319c42c)(form(Compound \
                         TypeArrow))(sort(Drv Typ))))(Secondary((id \
                         21d3aa57-69d4-48aa-b585-58e359e12124)(content(Whitespace\" \
                         \"))))(Tile((id \
                         8b4ad9d5-3727-4d20-a7ad-544d62e3fd65)(form(Tok \
                         Num))(sort(Drv Typ))))(Secondary((id \
                         98d74d8c-bc56-4ea3-bc48-102e8ca88792)(content(Whitespace\" \
                         \"))))(Tile((id \
                         7435a0fc-0dd1-44f3-8510-097532396f77)(form(Compound \
                         TypeArrow))(sort(Drv Typ))))(Secondary((id \
                         48625ab3-3e99-477c-9faf-973ffbb5ae27)(content(Whitespace\" \
                         \"))))(Tile((id \
                         70eaae77-3061-4950-9b99-e176db51b5b3)(form(Tok \
                         Num))(sort(Drv Typ)))))()))(ancestors())))(caret \
                         Outer)(refractors((manuals())(multis((ids())(suppressed())(ephemerals())))(sample_focus((call_stack())(index \
                         -1)(pinned_stack())(indicated_call())(time())(seq \
                         0)(step_range())(pending_focus())))(autoprobe_target())(pending_probe_cursor()))))";
                      backup_text =
                        "|- (fun a : Num -> fun b : Num -> if a < b then a \
                         else b) : Num -> Num -> Num";
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
                               2c46d9b9-a81d-43a5-a2b3-e1149335d785)(form(Tok \
                               $ctx_a))(sort(Drv Exp))))(Secondary((id \
                               360ebb5e-1be0-435f-a5d7-c894bbec95e2)(content(Whitespace\" \
                               \"))))(Tile((id \
                               7341dd05-cc52-46cf-9dff-2fa154a681be)(form(Compound \
                               Entail))(sort(Drv Exp))))(Secondary((id \
                               2cf79d0a-7185-493a-8cb9-c1af945e6139)(content(Whitespace\" \
                               \"))))(Tile((id \
                               413214a5-f0cb-46eb-91e6-d67b4de9677b)(form(Compound \
                               Parens))(sort(Drv Exp))(shards(0 \
                               1))(children(((Tile((id \
                               23e82faa-d419-418c-925d-4cf90580ec35)(form(Compound \
                               Fun))(sort(Drv Exp))(shards(0 \
                               1))(children(((Secondary((id \
                               78b6c2d9-9f5f-4a94-b1fc-eb6c76c72787)(content(Whitespace\" \
                               \"))))(Tile((id \
                               d3e9abf3-504d-4e29-ae07-11c605c0b41b)(form(Tok \
                               b))(sort(Drv Pat))))(Secondary((id \
                               7a13bc95-8a29-43a5-8587-90251681e929)(content(Whitespace\" \
                               \"))))(Tile((id \
                               ee817f62-9cb0-4ba5-b7b7-b72f43664d15)(form(Compound \
                               TypeAsc))(sort(Drv Pat))))(Secondary((id \
                               78d65832-02c9-4b20-bdc3-e37d5e2aac89)(content(Whitespace\" \
                               \"))))(Tile((id \
                               eff7438a-2234-4b2e-bd65-0d3c4ba0ec9a)(form(Tok \
                               Num))(sort(Drv Typ))))(Secondary((id \
                               2043b0ca-c830-4664-9ce7-fe18b61147d5)(content(Whitespace\" \
                               \")))))))))(Secondary((id \
                               7748f83e-7127-4816-a359-0d61536e132c)(content(Whitespace\" \
                               \"))))(Tile((id \
                               c1bd9ce1-131e-4bb0-ad7a-5b715f0a09f8)(form(Compound \
                               If))(sort(Drv Exp))(shards(0 1 \
                               2))(children(((Secondary((id \
                               dd476224-f3a5-42e0-a900-e0edec29da6b)(content(Whitespace\" \
                               \"))))(Tile((id \
                               c5dbe75d-c48f-4ded-9900-b674a7f2df56)(form(Tok \
                               a))(sort(Drv Exp))))(Secondary((id \
                               06b7d9b3-d187-4936-a96c-5b7bc38c5fb6)(content(Whitespace\" \
                               \"))))(Tile((id \
                               0109490c-3b7f-410d-8e7d-709c71719575)(form(Compound \
                               Lt))(sort(Drv Exp))))(Secondary((id \
                               fe57fa20-9497-41b1-a122-763c2c5ab559)(content(Whitespace\" \
                               \"))))(Tile((id \
                               2175c4e4-1a40-4f80-9df7-e7ab8e66df95)(form(Tok \
                               b))(sort(Drv Exp))))(Secondary((id \
                               6a3b67e0-2a81-4623-ad81-5d87e4942f2a)(content(Whitespace\" \
                               \")))))((Secondary((id \
                               b2a3c86a-98c4-460b-bebb-e793b0a158ef)(content(Whitespace\" \
                               \"))))(Tile((id \
                               4d29ef0f-b109-4781-9a67-386ec68a3a47)(form(Tok \
                               a))(sort(Drv Exp))))(Secondary((id \
                               97507602-f178-486a-96d2-321fadee2862)(content(Whitespace\" \
                               \")))))))))(Secondary((id \
                               da0b6ef0-b35c-46be-a7eb-6722281ac3e1)(content(Whitespace\" \
                               \"))))(Tile((id \
                               6c2ccf79-83cf-44f4-9a66-4972455cc31f)(form(Tok \
                               b))(sort(Drv Exp)))))))))(Secondary((id \
                               cbd8b3e1-0749-447b-8587-f7836d7b59b5)(content(Whitespace\" \
                               \"))))(Tile((id \
                               adc0785c-5dba-4d0e-812f-29265d0b6602)(form(Compound \
                               TypeAsc))(sort(Drv Exp))))(Secondary((id \
                               fadbcb25-0d2c-448c-9da6-c0ab7d83e089)(content(Whitespace\" \
                               \"))))(Tile((id \
                               ba2e2c74-aec6-46fb-965a-36ec837f2458)(form(Tok \
                               Num))(sort(Drv Typ))))(Secondary((id \
                               73201545-79c9-4777-81db-462e107bb0d4)(content(Whitespace\" \
                               \"))))(Tile((id \
                               edfc5c66-5aae-49e0-96bd-a0c205ed34ef)(form(Compound \
                               TypeArrow))(sort(Drv Typ))))(Secondary((id \
                               7b996c52-599d-46fd-aac3-6e07ac5854a8)(content(Whitespace\" \
                               \"))))(Tile((id \
                               2d1a2fb5-4d75-4edb-8c31-46fa40d60001)(form(Tok \
                               Num))(sort(Drv \
                               Typ)))))()))(ancestors())))(caret \
                               Outer)(refractors((manuals())(multis((ids())(suppressed())(ephemerals())))(sample_focus((call_stack())(index \
                               -1)(pinned_stack())(indicated_call())(time())(seq \
                               0)(step_range())(pending_focus())))(autoprobe_target())(pending_probe_cursor()))))";
                            backup_text =
                              "$ctx_a |- (fun b : Num -> if a < b then a else \
                               b) : Num -> Num";
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
                                     9ea23534-3da2-48f8-b3ef-640727649253)(form(Tok \
                                     $ctx_ab))(sort(Drv Exp))))(Secondary((id \
                                     7d5058d3-4153-4a8c-b875-6e2138270cfd)(content(Whitespace\" \
                                     \"))))(Tile((id \
                                     ca2806bb-4980-4984-a0ea-5190fd3fbd10)(form(Compound \
                                     Entail))(sort(Drv Exp))))(Secondary((id \
                                     3b021696-33ae-4a61-b71c-7fa34bb97d58)(content(Whitespace\" \
                                     \"))))(Tile((id \
                                     bbe23531-12b3-4434-8b2c-82beb4ea7c86)(form(Compound \
                                     Parens))(sort(Drv Exp))(shards(0 \
                                     1))(children(((Tile((id \
                                     558d4e19-178d-4990-82da-ae32aed896c4)(form(Compound \
                                     If))(sort(Drv Exp))(shards(0 1 \
                                     2))(children(((Secondary((id \
                                     beff8fdb-900b-4053-b626-594e56bc2f15)(content(Whitespace\" \
                                     \"))))(Tile((id \
                                     7f879360-ab8b-4f3c-af58-1a967368e55e)(form(Tok \
                                     a))(sort(Drv Exp))))(Secondary((id \
                                     d519f593-34da-41d6-84e6-c8b3df9eb19a)(content(Whitespace\" \
                                     \"))))(Tile((id \
                                     9725a143-26bb-4859-96cd-f9f38d162546)(form(Compound \
                                     Lt))(sort(Drv Exp))))(Secondary((id \
                                     bf813e20-9422-45e9-9926-451e5db94f91)(content(Whitespace\" \
                                     \"))))(Tile((id \
                                     99083085-1ed5-4cbd-bbc5-2f4c2b0096e5)(form(Tok \
                                     b))(sort(Drv Exp))))(Secondary((id \
                                     294c5ed4-dfaf-48af-93a3-80368c3ba3be)(content(Whitespace\" \
                                     \")))))((Secondary((id \
                                     355f257f-9a05-4ff1-a00f-906119b4e7be)(content(Whitespace\" \
                                     \"))))(Tile((id \
                                     b6edac25-db1b-416e-9417-b356662d8a60)(form(Tok \
                                     a))(sort(Drv Exp))))(Secondary((id \
                                     b51c81f6-8e47-4639-8a00-7793ab76ac62)(content(Whitespace\" \
                                     \")))))))))(Secondary((id \
                                     06370eb3-bbf8-41da-bdb0-43b2d43ac278)(content(Whitespace\" \
                                     \"))))(Tile((id \
                                     d982c052-4ca6-4fa4-b772-ab4e35eb49de)(form(Tok \
                                     b))(sort(Drv Exp)))))))))(Secondary((id \
                                     999a92b8-087c-4432-b48e-ef412d2f7270)(content(Whitespace\" \
                                     \"))))(Tile((id \
                                     bffc8781-0d41-466c-bbb7-663e5d217af9)(form(Compound \
                                     TypeAsc))(sort(Drv Exp))))(Secondary((id \
                                     1bd692e0-a603-4570-8200-f6d644f4dbd8)(content(Whitespace\" \
                                     \"))))(Tile((id \
                                     34727f0c-8f47-4cfd-be03-332d06d568d7)(form(Tok \
                                     Num))(sort(Drv \
                                     Typ)))))()))(ancestors())))(caret \
                                     Outer)(refractors((manuals())(multis((ids())(suppressed())(ephemerals())))(sample_focus((call_stack())(index \
                                     -1)(pinned_stack())(indicated_call())(time())(seq \
                                     0)(step_range())(pending_focus())))(autoprobe_target())(pending_probe_cursor()))))";
                                  backup_text =
                                    "$ctx_ab |- (if a < b then a else b) : Num";
                                };
                              rule = Some T_If;
                            },
                          [
                            Node
                              ( Just
                                  {
                                    jdmt =
                                      {
                                        zipper =
                                          "((selection((focus \
                                           Left)(content())(mode \
                                           Normal)(anchor_caret \
                                           Outer)(smart_rounded \
                                           false)))(relatives((siblings(((Tile((id \
                                           a7362c3e-a66d-4b4a-bc70-b63176c44568)(form(Tok \
                                           $ctx_ab))(sort(Drv \
                                           Exp))))(Secondary((id \
                                           8081069d-b94c-4311-ab78-1f2444842406)(content(Whitespace\" \
                                           \"))))(Tile((id \
                                           6904c71c-e5f0-45f4-a3d5-efacb52f8d7e)(form(Compound \
                                           Entail))(sort(Drv \
                                           Exp))))(Secondary((id \
                                           f0346b93-60cb-4d53-b962-18ecc2e7f669)(content(Whitespace\" \
                                           \"))))(Tile((id \
                                           67233546-e61c-459f-b80b-7fae22076ce3)(form(Compound \
                                           Parens))(sort(Drv Exp))(shards(0 \
                                           1))(children(((Tile((id \
                                           c3fbee8d-2300-469f-b1d7-2b3832a0b6e3)(form(Tok \
                                           a))(sort(Drv Exp))))(Secondary((id \
                                           3bd2c455-1d5c-4b19-b464-1c56f3947ab5)(content(Whitespace\" \
                                           \"))))(Tile((id \
                                           b0c0e035-c86b-4efe-9072-41c418bdf223)(form(Compound \
                                           Lt))(sort(Drv Exp))))(Secondary((id \
                                           71cd74ab-a44e-4761-9cee-874777dca8cf)(content(Whitespace\" \
                                           \"))))(Tile((id \
                                           8bb73c71-af6a-488c-ab0b-ae79821873df)(form(Tok \
                                           b))(sort(Drv \
                                           Exp)))))))))(Secondary((id \
                                           7b7d4aa7-7dd7-42bf-ba1f-772fff7f3435)(content(Whitespace\" \
                                           \"))))(Tile((id \
                                           1caf089b-1cb4-4e5c-ae93-c787399c288b)(form(Compound \
                                           TypeAsc))(sort(Drv \
                                           Exp))))(Secondary((id \
                                           55fa4a55-21c3-49a2-bf12-efc863d5d311)(content(Whitespace\" \
                                           \"))))(Tile((id \
                                           c13feebb-188f-4ba1-a01b-372b845c21fa)(form(Tok \
                                           Bool))(sort(Drv \
                                           Typ)))))()))(ancestors())))(caret \
                                           Outer)(refractors((manuals())(multis((ids())(suppressed())(ephemerals())))(sample_focus((call_stack())(index \
                                           -1)(pinned_stack())(indicated_call())(time())(seq \
                                           0)(step_range())(pending_focus())))(autoprobe_target())(pending_probe_cursor()))))";
                                        backup_text =
                                          "$ctx_ab |- (a < b) : Bool";
                                      };
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
