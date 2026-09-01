let exercise : DerivationExercise.spec =
  DerivationExercise.of_persistent
    {
      id = Haz3lcore.Id.v "bf42c3bf-f3aa-4e0e-b180-8df80e1aaa8f";
      title = "PairMap Derivation";
      module_name = "Ex_PairMap_Derivation";
      prompt =
        "Provide a derivation using the Bidirectional Type System rules of the \
         following judgement, which synthesizes a type for the pairNegate \
         function shown above as being defined by partially applying pairmap. \
         This derivation shows how type analysis allows us to avoid having to \
         annotate the function argument.";
      max_points = 10;
      prelude =
        {
          zipper =
            "((selection((focus Left)(content())(mode Normal)(anchor_caret \
             Outer)(smart_rounded false)))(relatives((siblings(((Secondary((id \
             29da2b63-dd10-4b81-9217-90aa5176215b)(content(Whitespace\" \
             \")))))((Grout((id bbfcab4e-72c2-48dd-bc9e-fa4ae356e50c)(shape \
             Convex))))))(ancestors())))(caret \
             Outer)(refractors((manuals())(multis((ids())(suppressed())(ephemerals())))(sample_focus((call_stack())(index \
             -1)(pinned_stack())(indicated_call())(time())(seq \
             0)(step_range())(pending_focus())))(autoprobe_target())(pending_probe_cursor()))))";
          backup_text = " ";
        };
      setup =
        {
          zipper =
            "((selection((focus Left)(content())(mode Normal)(anchor_caret \
             Outer)(smart_rounded false)))(relatives((siblings(((Tile((id \
             4c139545-0e66-449d-b77c-5b72a5e6c0b6)(form(Compound \
             Let))(shards(0 1 2))(children(((Secondary((id \
             9e69a860-bdc5-4077-91b9-f1806fb7fc93)(content(Whitespace\" \
             \"))))(Tile((id 436a82ae-3033-4374-b475-81e3831c9860)(form(Tok \
             $tau_pm))(sort Pat)))(Secondary((id \
             1bfcbe2a-aef1-4f74-b8b1-b45f3d9ba086)(content(Whitespace\" \
             \")))))((Secondary((id \
             97ae9f52-f423-4e7f-846b-74ccb043e975)(content(Whitespace\" \
             \"))))(Tile((id \
             7f3d255b-f04f-46e8-a938-04e3a99f1da3)(form(Compound \
             OfAlfaTyp))(shards(0 1))(children(((Secondary((id \
             c5edce9e-f000-4988-8860-45a47ffeda40)(content(Whitespace\" \
             \"))))(Tile((id \
             b545eff4-3cf0-4920-a597-fad53b818117)(form(Compound \
             Parens))(sort(Drv Typ))(shards(0 1))(children(((Tile((id \
             b1d2e2fa-2585-458a-9e8b-eac70773b04f)(form(Tok Bool))(sort(Drv \
             Typ))))(Secondary((id \
             aa26b38a-b34a-48dd-a27d-bf0172345e52)(content(Whitespace\" \
             \"))))(Tile((id \
             394a1707-a2a1-4296-8bf0-3345ccfe6458)(form(Compound \
             TypeArrow))(sort(Drv Typ))))(Secondary((id \
             33b5e576-4be2-4e8a-8645-25b2291fe18c)(content(Whitespace\" \
             \"))))(Tile((id a6f6a6f0-59cd-43b0-85b1-b54871c17811)(form(Tok \
             Bool))(sort(Drv Typ)))))))))(Secondary((id \
             22689d4f-f8fa-424e-9618-dfb643764b8f)(content(Whitespace\" \
             \"))))(Tile((id \
             031361e0-e5b9-4204-80a6-368cb64ccf53)(form(Compound \
             TypeArrow))(sort(Drv Typ))))(Secondary((id \
             aa2b2826-d88d-496d-b5bb-41970ad27804)(content(Whitespace\" \
             \"))))(Tile((id \
             1a8e35ef-5390-4faa-a941-c4984bc64604)(form(Compound \
             Parens))(sort(Drv Typ))(shards(0 1))(children(((Tile((id \
             6217f937-7a89-43fc-94d2-82cb9dd2c3c8)(form(Tok Bool))(sort(Drv \
             Typ))))(Secondary((id \
             5b4d913e-52e7-4c4f-9847-2c4c5712c2d7)(content(Whitespace\" \
             \"))))(Tile((id \
             eb086cd1-826e-4c79-8644-ebf22655666f)(form(Compound \
             Times))(sort(Drv Typ))))(Secondary((id \
             49f2337e-9dd8-4511-a650-36feb6c77b03)(content(Whitespace\" \
             \"))))(Tile((id e105da27-f84c-4dc4-a07c-dd3528a23d5e)(form(Tok \
             Bool))(sort(Drv Typ)))))))))(Secondary((id \
             1e428916-38f2-493b-9e3e-4c30d17819bb)(content(Whitespace\" \
             \"))))(Tile((id \
             1f779c66-a5b4-4e82-8e3f-7db9fae8d943)(form(Compound \
             TypeArrow))(sort(Drv Typ))))(Secondary((id \
             d23e43a1-b750-47be-a3d0-4621d87134e5)(content(Whitespace\" \
             \"))))(Tile((id \
             10d39778-1998-40f3-8972-d8e04d85fcc0)(form(Compound \
             Parens))(sort(Drv Typ))(shards(0 1))(children(((Tile((id \
             75b5710c-c8a6-48e6-a3fa-7eacb72ef168)(form(Tok Bool))(sort(Drv \
             Typ))))(Secondary((id \
             2b13d7f1-1e12-4b7a-a5a8-27d0372c43fa)(content(Whitespace\" \
             \"))))(Tile((id \
             0386ad47-63bc-4394-8849-9b6b10eb87af)(form(Compound \
             Times))(sort(Drv Typ))))(Secondary((id \
             8072e90f-17da-4f4b-a1e5-9bcea0b4d2bb)(content(Whitespace\" \
             \"))))(Tile((id e460cfdd-0474-4e34-a5d5-bd5e702da00a)(form(Tok \
             Bool))(sort(Drv Typ)))))))))(Secondary((id \
             3fdd037f-82c9-41db-87cc-8396351be01f)(content(Whitespace\" \
             \")))))))))(Secondary((id \
             81e28bd1-2a6c-4073-afa3-98c3d927b2e3)(content(Whitespace\" \
             \")))))))))(Secondary((id \
             a85f89f6-6795-436a-a0fa-1d9085699c2b)(content(Whitespace\"\\n\"))))(Tile((id \
             f5557abd-e442-4a0d-a1ae-ed1574a98a64)(form(Compound \
             Let))(shards(0 1 2))(children(((Secondary((id \
             cac08a59-cdd5-43bc-967a-1cf080544fbd)(content(Whitespace\" \
             \"))))(Tile((id 2fa0c709-2e31-41c5-9cd1-ecb77dcb3491)(form(Tok \
             $gamma_pm))(sort Pat)))(Secondary((id \
             3825a320-8a13-4c6b-8573-0806f2d6972d)(content(Whitespace\" \
             \")))))((Secondary((id \
             628539d6-db13-40dd-bf4a-d6756b154f4d)(content(Whitespace\" \
             \"))))(Tile((id \
             2397e8c0-90e5-4c9c-a86f-e539e89a93f1)(form(Compound \
             OfCtx))(shards(0 1))(children(((Secondary((id \
             0a77ae49-3a6a-4759-89ee-ca05966e6606)(content(Whitespace\" \
             \"))))(Tile((id \
             937e2b04-c6c8-4018-96cc-2132b4a76ccc)(form(Compound \
             ListLit))(sort(Drv Exp))(shards(0 1))(children(((Tile((id \
             e41341b7-70fb-4bc1-8f0b-4f95fade06d3)(form(Tok pairmap))(sort(Drv \
             Exp))))(Secondary((id \
             0789ff73-3583-4fde-b496-665a3c24ff9f)(content(Whitespace\" \
             \"))))(Tile((id \
             8c29917a-34f7-43c0-9797-7fe746c782e6)(form(Compound \
             TypeAsc))(sort(Drv Exp))))(Secondary((id \
             134603b1-5522-4a59-ba1e-ec9307937611)(content(Whitespace\" \
             \"))))(Tile((id 4e08ccfc-c8bb-4fe8-8370-aeca1242f4d9)(form(Tok \
             $tau_pm))(sort(Drv Typ)))))))))(Secondary((id \
             9b4cbf35-e708-4f1f-a410-81c2fb19aca8)(content(Whitespace\" \
             \")))))))))(Secondary((id \
             26d8c722-6b23-4ca9-898f-378fae9490b4)(content(Whitespace\" \
             \")))))))))(Secondary((id \
             03ddc86c-a9d2-401d-9b3e-815e598a49b5)(content(Whitespace\"\\n\"))))(Tile((id \
             9c01937a-c33d-4d5a-bb7a-db1c0f822f79)(form(Compound \
             Let))(shards(0 1 2))(children(((Secondary((id \
             77774620-8e72-4fe6-a07e-5ad5e395362e)(content(Whitespace\" \
             \"))))(Tile((id d2432812-d11f-41d4-9fad-fd3c3b59dad6)(form(Tok \
             $gamma_pmz))(sort Pat)))(Secondary((id \
             c23320c4-5ec1-4429-ba14-b506fa40cfb1)(content(Whitespace\" \
             \")))))((Secondary((id \
             d271f69d-00f4-4263-a7e6-1263bc0a45b3)(content(Whitespace\" \
             \"))))(Tile((id \
             ec52876d-5fe8-4bea-898d-8474bcc9b05a)(form(Compound \
             OfCtx))(shards(0 1))(children(((Secondary((id \
             193b4662-e7f9-491e-a2d1-c8e8008b7a11)(content(Whitespace\" \
             \"))))(Tile((id \
             e467a9ce-ddb4-481b-b9d1-0650896dfbc1)(form(Compound \
             Parens))(sort(Drv Exp))(shards(0 1))(children(((Tile((id \
             0ef26f59-7c0c-4aec-9dbc-6d0976b1f4ba)(form(Tok z))(sort(Drv \
             Exp))))(Tile((id \
             f919d2a2-bffd-4705-babd-a1c217f202f1)(form(Compound \
             TypeAsc))(sort(Drv Exp))))(Secondary((id \
             04456d3f-874c-408d-bb3c-fcd51f04ea67)(content(Whitespace\" \
             \"))))(Tile((id de0418f9-0c55-46e8-a761-419b57a1d553)(form(Tok \
             Bool))(sort(Drv Typ)))))))))(Tile((id \
             005c3d3a-606b-4842-843a-2770f82215f0)(form(Compound \
             Cons))(sort(Drv Exp))))(Tile((id \
             df7bed55-281c-4fab-b5fa-10ea1f495280)(form(Tok \
             $gamma_pm))(sort(Drv Exp))))(Secondary((id \
             92205935-402d-4ffa-9f8a-451e01528ef0)(content(Whitespace\" \
             \")))))))))(Secondary((id \
             8817062d-6def-41a9-9977-9e7777c06663)(content(Whitespace\" \
             \")))))))))(Secondary((id \
             ddfe4246-e93e-486b-8a80-b0482769e581)(content(Whitespace\" \
             \"))))(Secondary((id \
             a5598551-0275-4593-870d-bfdb08259d4a)(content(Whitespace\"\\n\")))))((Grout((id \
             7f089dd8-7a3b-421f-8fd7-fe113b5ef3f6)(shape \
             Convex))))))(ancestors())))(caret \
             Outer)(refractors((manuals())(multis((ids())(suppressed())(ephemerals())))(sample_focus((call_stack())(index \
             -1)(pinned_stack())(indicated_call())(time())(seq \
             0)(step_range())(pending_focus())))(autoprobe_target())(pending_probe_cursor()))))";
          backup_text =
            "let $tau_pm = of_alfa_typ (Bool -> Bool) -> (Bool * Bool) -> \
             (Bool * Bool) end in\n\
             let $gamma_pm = of_ctx [pairmap : $tau_pm] end in\n\
             let $gamma_pmz = of_ctx (z: Bool)::$gamma_pm end in \n";
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
                         5fcfb6c8-2b8f-4339-8ac3-db2484b20919)(form(Tok \
                         $gamma_pm))(sort(Drv Exp))))(Secondary((id \
                         cde0723d-27c2-4a7c-b0fa-bc30a642595a)(content(Whitespace\" \
                         \"))))(Tile((id \
                         1dea8ca0-ea99-4657-9b8c-c512cdaad008)(form(Compound \
                         Entail))(sort(Drv Exp))))(Secondary((id \
                         a4ebb5e9-975b-4669-bba4-e710673bbc9d)(content(Whitespace\" \
                         \"))))(Tile((id \
                         5a89a8ec-49fb-4ce8-97d5-26b15560e405)(form(Compound \
                         Parens))(sort(Drv Exp))(shards(0 \
                         1))(children(((Tile((id \
                         5ff780d9-8c89-43d7-a2ae-1671cf0c95bd)(form(Compound \
                         Fun))(sort(Drv Exp))(shards(0 \
                         1))(children(((Secondary((id \
                         29f7b5c2-132c-4f72-a600-6c281735e929)(content(Whitespace\" \
                         \"))))(Tile((id \
                         890ec98f-c8ab-4dbe-ad3c-bbbe143778da)(form(Tok \
                         z))(sort(Drv Pat))))(Secondary((id \
                         8ee7afb2-6d8d-4077-a03c-9b44d73505b7)(content(Whitespace\" \
                         \")))))))))(Secondary((id \
                         bc208498-9cf7-4e08-84de-bd2b899fccd2)(content(Whitespace\" \
                         \"))))(Tile((id \
                         ef9a3c49-7bb2-41ec-b67a-58a702b77843)(form(Compound \
                         If))(sort(Drv Exp))(shards(0 1 \
                         2))(children(((Secondary((id \
                         f06f99c1-0ffd-4294-b050-c0fec02f187a)(content(Whitespace\" \
                         \"))))(Tile((id \
                         08883a89-bd51-4466-9d23-71129e9dcc7c)(form(Tok \
                         z))(sort(Drv Exp))))(Secondary((id \
                         7f59e48b-44cf-4d30-8d5d-abc561d9a0f4)(content(Whitespace\" \
                         \")))))((Secondary((id \
                         e54a3dcf-22de-4ccb-9466-dca8e78d1261)(content(Whitespace\" \
                         \"))))(Tile((id \
                         92dd90df-e3fb-499a-977d-33abea92d769)(form(Tok \
                         False))(sort(Drv Exp))))(Secondary((id \
                         79ac8f81-f885-4113-8b4e-d8d9bcccb66c)(content(Whitespace\" \
                         \")))))))))(Secondary((id \
                         e80f0e25-9752-42b2-8f2e-b9a7ada82ce7)(content(Whitespace\" \
                         \"))))(Tile((id \
                         0dfc83ec-29eb-4158-8b46-455d3fb0ffe0)(form(Tok \
                         True))(sort(Drv Exp)))))))))(Secondary((id \
                         3f919592-3afb-44d0-b852-184883db1db8)(content(Whitespace\" \
                         \"))))(Tile((id \
                         ea97f9dd-9f94-4c50-9106-20df0a3bdce4)(form(Compound \
                         Lte))(sort(Drv Exp))))(Secondary((id \
                         44c012aa-a143-40ef-af84-4d711801ffd0)(content(Whitespace\" \
                         \"))))(Tile((id \
                         b8845e9b-db29-440a-a709-17278448ee51)(form(Compound \
                         Parens))(sort(Drv Typ))(shards(0 \
                         1))(children(((Tile((id \
                         e9028438-cf33-4dc1-adfa-8a77ad57e3d5)(form(Tok \
                         Bool))(sort(Drv Typ))))(Secondary((id \
                         ff17abaf-f76b-42d5-9a96-4325dd5760b3)(content(Whitespace\" \
                         \"))))(Tile((id \
                         e96d4a7d-66ac-44a9-bf8a-0dd4cd215303)(form(Compound \
                         TypeArrow))(sort(Drv Typ))))(Secondary((id \
                         388e63ba-94e6-4cc7-88dd-390a8fdeb3aa)(content(Whitespace\" \
                         \"))))(Tile((id \
                         83c34125-0287-4bdc-b506-b443ed26cc4e)(form(Tok \
                         Bool))(sort(Drv \
                         Typ))))))))))()))(ancestors())))(caret \
                         Outer)(refractors((manuals())(multis((ids())(suppressed())(ephemerals())))(sample_focus((call_stack())(index \
                         -1)(pinned_stack())(indicated_call())(time())(seq \
                         0)(step_range())(pending_focus())))(autoprobe_target())(pending_probe_cursor()))))";
                      backup_text =
                        "$gamma_pm |- (fun z -> if z then False else True) <= \
                         (Bool -> Bool)";
                    };
                  rule = Some A_Fun;
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
                               59a3119e-9356-4146-90ef-12c3b27df018)(form(Tok \
                               $gamma_pmz))(sort(Drv Exp))))(Secondary((id \
                               94c3ef1d-54e3-4131-b171-114070ecfefe)(content(Whitespace\" \
                               \"))))(Tile((id \
                               2ac694cb-fd59-4bc6-b47e-1bdf94363516)(form(Compound \
                               Entail))(sort(Drv Exp))))(Secondary((id \
                               eccdbdf7-3c82-4808-82a0-be51c6e6b5e4)(content(Whitespace\" \
                               \"))))(Tile((id \
                               288425a3-f327-48d2-8396-2b5edbc02091)(form(Compound \
                               Parens))(sort(Drv Exp))(shards(0 \
                               1))(children(((Tile((id \
                               f4831062-a3ab-4214-a0bb-a829c8a7644c)(form(Compound \
                               If))(sort(Drv Exp))(shards(0 1 \
                               2))(children(((Secondary((id \
                               23af20f9-09ba-4f0b-a46e-05acb837d59d)(content(Whitespace\" \
                               \"))))(Tile((id \
                               39dda3e1-6b15-4cb4-b8c6-f2cd1b8013b2)(form(Tok \
                               z))(sort(Drv Exp))))(Secondary((id \
                               8b3a06d2-56d6-4497-bbf2-a5753b9efc95)(content(Whitespace\" \
                               \")))))((Secondary((id \
                               1d41363c-7617-4717-8085-7c92b5930f55)(content(Whitespace\" \
                               \"))))(Tile((id \
                               dfd49a3a-33f8-4866-91b6-1365ee8027f2)(form(Tok \
                               False))(sort(Drv Exp))))(Secondary((id \
                               1a867663-5567-4db0-9e7f-d02ce0552986)(content(Whitespace\" \
                               \")))))))))(Secondary((id \
                               0cd8c3fd-358d-440f-b193-41adab536313)(content(Whitespace\" \
                               \"))))(Tile((id \
                               caf84671-9a61-4a6d-91e6-4cee99a9afb5)(form(Tok \
                               True))(sort(Drv Exp)))))))))(Secondary((id \
                               cacbd2eb-3552-431e-97de-726578c9c288)(content(Whitespace\" \
                               \"))))(Tile((id \
                               652b4add-0fce-440a-9260-2afa3318fbdc)(form(Compound \
                               Lte))(sort(Drv Exp))))(Secondary((id \
                               4cf09efe-cfcf-41e6-bf1d-2cb5bcdfcd78)(content(Whitespace\" \
                               \"))))(Tile((id \
                               cd00bf6c-a9fc-4c63-80d9-676b88d196c0)(form(Tok \
                               Bool))(sort(Drv \
                               Typ)))))()))(ancestors())))(caret \
                               Outer)(refractors((manuals())(multis((ids())(suppressed())(ephemerals())))(sample_focus((call_stack())(index \
                               -1)(pinned_stack())(indicated_call())(time())(seq \
                               0)(step_range())(pending_focus())))(autoprobe_target())(pending_probe_cursor()))))";
                            backup_text =
                              "$gamma_pmz |- (if z then False else True) <= \
                               Bool";
                          };
                        rule = Some A_If;
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
                                     6de9836e-e6e6-45d6-92be-c386c238caea)(form(Tok \
                                     $gamma_pmz))(sort(Drv \
                                     Exp))))(Secondary((id \
                                     5ec0b9d0-bbbe-44d5-b1fd-fc7b8587b7d8)(content(Whitespace\" \
                                     \"))))(Tile((id \
                                     37173c6a-f5b2-4f40-bc03-58b3ca55eeda)(form(Compound \
                                     Entail))(sort(Drv Exp))))(Secondary((id \
                                     31c75892-7608-4bb1-aeff-5ade3c794db7)(content(Whitespace\" \
                                     \"))))(Tile((id \
                                     d702c38f-a88c-4c1f-bbcf-f72d5bfcbbf9)(form(Tok \
                                     z))(sort(Drv Exp))))(Secondary((id \
                                     a3a81ccf-f669-40c0-aa54-bcf763f365b4)(content(Whitespace\" \
                                     \"))))(Tile((id \
                                     b2025389-ce97-403d-a3bc-3d5de56857f2)(form(Compound \
                                     Lte))(sort(Drv Exp))))(Secondary((id \
                                     3703f5a9-3ed7-4999-aab8-cd7392f0b803)(content(Whitespace\" \
                                     \"))))(Tile((id \
                                     bb2ba9dd-34bc-4c36-8d13-bc16151d45a9)(form(Tok \
                                     Bool))(sort(Drv \
                                     Typ)))))()))(ancestors())))(caret \
                                     Outer)(refractors((manuals())(multis((ids())(suppressed())(ephemerals())))(sample_focus((call_stack())(index \
                                     -1)(pinned_stack())(indicated_call())(time())(seq \
                                     0)(step_range())(pending_focus())))(autoprobe_target())(pending_probe_cursor()))))";
                                  backup_text = "$gamma_pmz |- z <= Bool";
                                };
                              rule = Some A_Subsumption;
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
                                           8c7b40a4-d724-41be-97ef-a099866b7fb2)(form(Tok \
                                           $gamma_pmz))(sort(Drv \
                                           Exp))))(Secondary((id \
                                           16191162-6fce-43c5-b168-a7972b047aae)(content(Whitespace\" \
                                           \"))))(Tile((id \
                                           adc765d4-4356-4ecf-882d-0489c570ced7)(form(Compound \
                                           Entail))(sort(Drv \
                                           Exp))))(Secondary((id \
                                           12e249fd-dc1d-421f-bbff-53ee8062d44f)(content(Whitespace\" \
                                           \"))))(Tile((id \
                                           25f77677-d3e2-42b7-ab63-2bdecd49a32a)(form(Tok \
                                           z))(sort(Drv Exp))))(Secondary((id \
                                           5c8ba908-d688-4c9a-a32e-250534d5ad36)(content(Whitespace\" \
                                           \"))))(Tile((id \
                                           b423cb6d-f381-497f-8e01-52b95a5ed162)(form(Compound \
                                           Syn))(sort(Drv \
                                           Exp))))(Secondary((id \
                                           b33b97ee-7704-4883-b15a-e6b481ee7236)(content(Whitespace\" \
                                           \"))))(Tile((id \
                                           0f5eb434-418e-474e-82c8-4e0f29c1a3f4)(form(Tok \
                                           Bool))(sort(Drv \
                                           Typ)))))()))(ancestors())))(caret \
                                           Outer)(refractors((manuals())(multis((ids())(suppressed())(ephemerals())))(sample_focus((call_stack())(index \
                                           -1)(pinned_stack())(indicated_call())(time())(seq \
                                           0)(step_range())(pending_focus())))(autoprobe_target())(pending_probe_cursor()))))";
                                        backup_text = "$gamma_pmz |- z => Bool";
                                      };
                                    rule = Some S_Var;
                                  },
                                [] );
                          ] );
                      Node
                        ( Just
                            {
                              jdmt =
                                {
                                  zipper =
                                    "((selection((focus Left)(content())(mode \
                                     Normal)(anchor_caret Outer)(smart_rounded \
                                     false)))(relatives((siblings(((Tile((id \
                                     dcafefda-801c-42de-90a9-d3745e749b14)(form(Tok \
                                     $gamma_pmz))(sort(Drv \
                                     Exp))))(Secondary((id \
                                     ac6e28d5-1d8f-423f-8d07-3a50d4c88eb8)(content(Whitespace\" \
                                     \"))))(Tile((id \
                                     472c6e17-ee56-4346-8cce-66e9440e2da1)(form(Compound \
                                     Entail))(sort(Drv Exp))))(Secondary((id \
                                     f191abcf-9361-422a-9b2f-67beac5c0f01)(content(Whitespace\" \
                                     \"))))(Tile((id \
                                     cec951d5-b589-4019-a54a-e0dbec12530c)(form(Tok \
                                     False))(sort(Drv Exp))))(Secondary((id \
                                     93ed51b0-e42c-44a1-9760-dc684ba42c34)(content(Whitespace\" \
                                     \"))))(Tile((id \
                                     f26c3a0d-2a13-47b9-bf62-268b12d08086)(form(Compound \
                                     Lte))(sort(Drv Exp))))(Secondary((id \
                                     3cb8d7b6-f07b-4cea-8b50-063c5892f93d)(content(Whitespace\" \
                                     \"))))(Tile((id \
                                     83acbdcc-5b02-471f-9648-fbe908d87345)(form(Tok \
                                     Bool))(sort(Drv \
                                     Typ)))))()))(ancestors())))(caret \
                                     Outer)(refractors((manuals())(multis((ids())(suppressed())(ephemerals())))(sample_focus((call_stack())(index \
                                     -1)(pinned_stack())(indicated_call())(time())(seq \
                                     0)(step_range())(pending_focus())))(autoprobe_target())(pending_probe_cursor()))))";
                                  backup_text = "$gamma_pmz |- False <= Bool";
                                };
                              rule = Some A_Subsumption;
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
                                           366bb0b2-6f09-4bf0-95d3-a4eab5ecc459)(form(Tok \
                                           $gamma_pmz))(sort(Drv \
                                           Exp))))(Secondary((id \
                                           572b968b-d499-4aae-99c7-eaeae9f24122)(content(Whitespace\" \
                                           \"))))(Tile((id \
                                           c1195573-9553-451d-b346-bac66820f363)(form(Compound \
                                           Entail))(sort(Drv \
                                           Exp))))(Secondary((id \
                                           e85efc8e-fbfc-4ef1-8a77-cd239e064364)(content(Whitespace\" \
                                           \"))))(Tile((id \
                                           d31fda1f-ce31-45ca-a7d3-0690502f1db1)(form(Tok \
                                           False))(sort(Drv \
                                           Exp))))(Secondary((id \
                                           f567f10e-84ac-4c58-81be-fd4d13296e0b)(content(Whitespace\" \
                                           \"))))(Tile((id \
                                           7bbd9517-d9c4-4507-b8e1-0c2c27efede1)(form(Compound \
                                           Syn))(sort(Drv \
                                           Exp))))(Secondary((id \
                                           e28721ff-7ee3-49ce-915f-45f5482eb8d4)(content(Whitespace\" \
                                           \"))))(Tile((id \
                                           9dd12390-1b5e-4c8f-bc7e-8db6fe5f7ff6)(form(Tok \
                                           Bool))(sort(Drv \
                                           Typ)))))()))(ancestors())))(caret \
                                           Outer)(refractors((manuals())(multis((ids())(suppressed())(ephemerals())))(sample_focus((call_stack())(index \
                                           -1)(pinned_stack())(indicated_call())(time())(seq \
                                           0)(step_range())(pending_focus())))(autoprobe_target())(pending_probe_cursor()))))";
                                        backup_text =
                                          "$gamma_pmz |- False => Bool";
                                      };
                                    rule = Some S_False;
                                  },
                                [] );
                          ] );
                      Node
                        ( Just
                            {
                              jdmt =
                                {
                                  zipper =
                                    "((selection((focus Left)(content())(mode \
                                     Normal)(anchor_caret Outer)(smart_rounded \
                                     false)))(relatives((siblings(((Tile((id \
                                     8dd486bf-b435-4d22-9ebd-f28103a20b40)(form(Tok \
                                     $gamma_pmz))(sort(Drv \
                                     Exp))))(Secondary((id \
                                     10d20b63-a135-4b8e-8888-5a3a0fe0fafa)(content(Whitespace\" \
                                     \"))))(Tile((id \
                                     2a7325d6-a86c-478a-b1ea-9519014b13fc)(form(Compound \
                                     Entail))(sort(Drv Exp))))(Secondary((id \
                                     dc25b35b-9a51-4bcf-b352-7c4467db8645)(content(Whitespace\" \
                                     \"))))(Tile((id \
                                     704281b2-0e28-4827-be4e-a8ba2aa09e77)(form(Tok \
                                     True))(sort(Drv Exp))))(Secondary((id \
                                     a6b22b15-4e66-4553-b9a0-b48fd72c09ea)(content(Whitespace\" \
                                     \"))))(Tile((id \
                                     ab32b744-60f2-4d6b-a1a4-fe73d9cf150a)(form(Compound \
                                     Lte))(sort(Drv Exp))))(Secondary((id \
                                     41cd97f5-9969-44b9-b743-081a5e8fdf65)(content(Whitespace\" \
                                     \"))))(Tile((id \
                                     f1c7e487-303d-4c83-9556-408b9be0ef20)(form(Tok \
                                     Bool))(sort(Drv \
                                     Typ)))))()))(ancestors())))(caret \
                                     Outer)(refractors((manuals())(multis((ids())(suppressed())(ephemerals())))(sample_focus((call_stack())(index \
                                     -1)(pinned_stack())(indicated_call())(time())(seq \
                                     0)(step_range())(pending_focus())))(autoprobe_target())(pending_probe_cursor()))))";
                                  backup_text = "$gamma_pmz |- True <= Bool";
                                };
                              rule = Some A_Subsumption;
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
                                           01b5c56f-a50e-430a-a127-0e6793f28106)(form(Tok \
                                           $gamma_pmz))(sort(Drv \
                                           Exp))))(Secondary((id \
                                           2db60639-4234-4479-a275-316cdaa09625)(content(Whitespace\" \
                                           \"))))(Tile((id \
                                           78970d10-325b-4f60-9506-1bb1782841f6)(form(Compound \
                                           Entail))(sort(Drv \
                                           Exp))))(Secondary((id \
                                           9376bf78-bdf5-408d-a444-7593546159e2)(content(Whitespace\" \
                                           \"))))(Tile((id \
                                           9628635f-dbaf-46fd-a974-dff363a5f968)(form(Tok \
                                           True))(sort(Drv \
                                           Exp))))(Secondary((id \
                                           c9f48373-af65-4a47-972a-cbc4d41144dc)(content(Whitespace\" \
                                           \"))))(Tile((id \
                                           a69e0910-d534-4233-a252-76daca50d5e9)(form(Compound \
                                           Syn))(sort(Drv \
                                           Exp))))(Secondary((id \
                                           5fb21a23-13e6-479d-ac87-0484d8735798)(content(Whitespace\" \
                                           \"))))(Tile((id \
                                           f3b62cbc-86b5-441c-b397-de4e1b911b69)(form(Tok \
                                           Bool))(sort(Drv \
                                           Typ)))))()))(ancestors())))(caret \
                                           Outer)(refractors((manuals())(multis((ids())(suppressed())(ephemerals())))(sample_focus((call_stack())(index \
                                           -1)(pinned_stack())(indicated_call())(time())(seq \
                                           0)(step_range())(pending_focus())))(autoprobe_target())(pending_probe_cursor()))))";
                                        backup_text =
                                          "$gamma_pmz |- True => Bool";
                                      };
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
                    {
                      zipper =
                        "((selection((focus Left)(content())(mode \
                         Normal)(anchor_caret Outer)(smart_rounded \
                         false)))(relatives((siblings(((Tile((id \
                         745ef21d-ee3d-4961-abcd-10c4cfa45eda)(form(Tok \
                         $gamma_pm))(sort(Drv Exp))))(Secondary((id \
                         f08aa274-6e18-4f06-b30a-c19007dfee2e)(content(Whitespace\" \
                         \"))))(Tile((id \
                         c82ce586-8614-437c-85ab-75634442fcd0)(form(Compound \
                         Entail))(sort(Drv Exp))))(Secondary((id \
                         ae52efa1-40e6-4ba9-9621-49708eb17b42)(content(Whitespace\" \
                         \"))))(Tile((id \
                         fa58d3df-e974-4947-ab4d-114eef8d53e1)(form(Tok \
                         pairmap))(sort(Drv Exp))))(Secondary((id \
                         33e74af2-cca0-462b-a74a-5411855e3184)(content(Whitespace\" \
                         \"))))(Tile((id \
                         036658fa-40a2-4f3e-b9d2-d59b57ba3a84)(form(Compound \
                         Ap))(sort(Drv Exp))(shards(0 1))(children(((Tile((id \
                         50eb72b4-9e93-4a80-bb68-dc3718d5c088)(form(Compound \
                         Fun))(sort(Drv Exp))(shards(0 \
                         1))(children(((Secondary((id \
                         27768e32-5e9f-4406-b4b7-0b507091ede7)(content(Whitespace\" \
                         \"))))(Tile((id \
                         141d4582-a4f6-4477-948c-d302aee6821b)(form(Tok \
                         z))(sort(Drv Pat))))(Secondary((id \
                         d460f86c-51c3-4808-8848-32f5b209691e)(content(Whitespace\" \
                         \")))))))))(Secondary((id \
                         c3c182b4-6706-4650-ba95-498a02619633)(content(Whitespace\" \
                         \"))))(Tile((id \
                         16c13bc2-709d-4af5-84c4-636bd4ef95ab)(form(Compound \
                         If))(sort(Drv Exp))(shards(0 1 \
                         2))(children(((Secondary((id \
                         68e51df3-6380-40be-95e5-7d2bb1fbf35e)(content(Whitespace\" \
                         \"))))(Tile((id \
                         bdf09a6f-6133-4835-a61f-01f193ed94dd)(form(Tok \
                         z))(sort(Drv Exp))))(Secondary((id \
                         ddd3db19-5eb9-4b1f-ab10-d6cb2d50f653)(content(Whitespace\" \
                         \")))))((Secondary((id \
                         e990f207-e7bb-4e44-81b4-7b58f82fc8e0)(content(Whitespace\" \
                         \"))))(Tile((id \
                         c519c261-431e-4730-8ba5-cb27ff56462a)(form(Tok \
                         False))(sort(Drv Exp))))(Secondary((id \
                         8d4be8a1-8ca9-42ba-8704-a3417ce4e8ba)(content(Whitespace\" \
                         \")))))))))(Secondary((id \
                         d4427eba-21e1-4613-a9ff-d67cb80e9374)(content(Whitespace\" \
                         \"))))(Tile((id \
                         52535337-6c5f-4c8f-a906-bb19d9377943)(form(Tok \
                         True))(sort(Drv Exp)))))))))(Secondary((id \
                         aa9dc65e-5939-4d1c-b408-c3e1f089a623)(content(Whitespace\" \
                         \"))))(Tile((id \
                         d685f9ba-b613-4e50-a611-b68dfcc6d756)(form(Compound \
                         Syn))(sort(Drv Exp))))(Secondary((id \
                         7cb61d3d-0488-4fe8-8cda-fe22ce8b6bd4)(content(Whitespace\" \
                         \"))))(Tile((id \
                         271006c5-1020-42bc-9682-9ff955a4997a)(form(Compound \
                         Parens))(sort(Drv Typ))(shards(0 \
                         1))(children(((Tile((id \
                         f60f1e3c-0bf7-4542-aa6d-8869e75f1e67)(form(Tok \
                         Bool))(sort(Drv Typ))))(Secondary((id \
                         fe6aed91-6ed1-417a-9bd8-bf878723f9d4)(content(Whitespace\" \
                         \"))))(Tile((id \
                         37d23c4f-afb8-4c88-bcdd-4b48267019e1)(form(Compound \
                         Times))(sort(Drv Typ))))(Secondary((id \
                         18ddb8cb-613c-45f0-a820-801208914513)(content(Whitespace\" \
                         \"))))(Tile((id \
                         7728a1a5-efe2-4ab1-a6da-631db3a4f414)(form(Tok \
                         Bool))(sort(Drv Typ)))))))))(Secondary((id \
                         fe31a4eb-61f8-4f74-896f-0b1a961795ca)(content(Whitespace\" \
                         \"))))(Tile((id \
                         cbe20272-9046-45b2-8786-2725d9089a0b)(form(Compound \
                         TypeArrow))(sort(Drv Typ))))(Secondary((id \
                         43da4c42-7707-4bb5-b812-05b62b03ad4e)(content(Whitespace\" \
                         \"))))(Tile((id \
                         2b05a180-fa73-445c-b741-c52550ca5795)(form(Compound \
                         Parens))(sort(Drv Typ))(shards(0 \
                         1))(children(((Tile((id \
                         0da381ea-9e4f-443b-91b7-5b166e1cb20b)(form(Tok \
                         Bool))(sort(Drv Typ))))(Secondary((id \
                         9d967aa7-3174-46b1-b1e3-51d9536c7aca)(content(Whitespace\" \
                         \"))))(Tile((id \
                         a26bb6fe-5be2-43d8-9aaf-37e883dd893a)(form(Compound \
                         Times))(sort(Drv Typ))))(Secondary((id \
                         c576dfdb-4cc9-4dd4-9484-2a5fbea10c2f)(content(Whitespace\" \
                         \"))))(Tile((id \
                         863dc5ed-77f4-4df0-8624-740a7c10c6db)(form(Tok \
                         Bool))(sort(Drv \
                         Typ))))))))))()))(ancestors())))(caret \
                         Outer)(refractors((manuals())(multis((ids())(suppressed())(ephemerals())))(sample_focus((call_stack())(index \
                         -1)(pinned_stack())(indicated_call())(time())(seq \
                         0)(step_range())(pending_focus())))(autoprobe_target())(pending_probe_cursor()))))";
                      backup_text =
                        "$gamma_pm |- pairmap (fun z -> if z then False else \
                         True) => (Bool * Bool) -> (Bool * Bool)";
                    };
                  rule = Some S_Ap;
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
                               50bedc3e-b1c7-43fd-99fc-9ba7bc2ec5c5)(form(Tok \
                               $gamma_pm))(sort(Drv Exp))))(Secondary((id \
                               4da29aa5-492f-47bd-8b66-bf961e3788e8)(content(Whitespace\" \
                               \"))))(Tile((id \
                               cb775f47-1a92-48c2-b0d2-1e0d143b1498)(form(Compound \
                               Entail))(sort(Drv Exp))))(Secondary((id \
                               b653ad33-6284-450d-b39f-8d26f593b186)(content(Whitespace\" \
                               \"))))(Tile((id \
                               8903cdc1-ff0a-491d-8c38-68f841598d74)(form(Tok \
                               pairmap))(sort(Drv Exp))))(Secondary((id \
                               2a7e1027-aef2-4187-a6c8-64a7ec3f2ec3)(content(Whitespace\" \
                               \"))))(Tile((id \
                               6a29f2de-8829-48d1-96c4-cfa82f3ba18d)(form(Compound \
                               Syn))(sort(Drv Exp))))(Secondary((id \
                               9ce5e87e-7494-4dfd-9bc4-72a11e104475)(content(Whitespace\" \
                               \"))))(Tile((id \
                               82ac6a15-fa28-4bf4-91e6-ba966d431822)(form(Tok \
                               $tau_pm))(sort(Drv \
                               Typ)))))()))(ancestors())))(caret \
                               Outer)(refractors((manuals())(multis((ids())(suppressed())(ephemerals())))(sample_focus((call_stack())(index \
                               -1)(pinned_stack())(indicated_call())(time())(seq \
                               0)(step_range())(pending_focus())))(autoprobe_target())(pending_probe_cursor()))))";
                            backup_text = "$gamma_pm |- pairmap => $tau_pm";
                          };
                        rule = Some S_Var;
                      },
                    [] );
                Node (Abbr (Some 0), []);
              ] );
        ];
    }
