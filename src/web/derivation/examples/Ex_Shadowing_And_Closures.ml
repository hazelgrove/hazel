let exercise : DerivationExercise.spec =
  DerivationExercise.of_persistent
    {
      id = Haz3lcore.Id.v "280ed584-a895-40ee-8292-8622b0aab321";
      title = "Shadowing and Closures";
      module_name = "Ex_Shadowing_And_Closures";
      prompt = "let us derive the judgement e_example \226\135\147 4.";
      max_points = 10;
      prelude =
        {
          zipper =
            "((selection((focus Left)(content())(mode Normal)(anchor_caret \
             Outer)(smart_rounded false)))(relatives((siblings(()((Grout((id \
             cd34f700-a3dc-4c19-9101-b183ef19a43e)(shape \
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
             24054ce1-d706-424c-b3c7-a84699601ddd)(form(Compound \
             Let))(shards(0 1 2))(children(((Secondary((id \
             02477ad8-346e-474c-afdf-63daadcafbef)(content(Whitespace\" \
             \"))))(Tile((id 7bf0c0d5-e6dd-46a9-877d-be0fdd380313)(form(Tok \
             $e2))(sort Pat)))(Secondary((id \
             36e24613-0e26-487f-99e0-05c96cb055d0)(content(Whitespace\" \
             \")))))((Secondary((id \
             8a19b714-78a3-4f8d-a760-68677cf095d6)(content(Whitespace\" \
             \"))))(Tile((id \
             fa28fb6c-1afc-4a90-a7f0-d935840fd45e)(form(Compound \
             OfAlfaExp))(shards(0 1))(children(((Secondary((id \
             9aa57a37-fbca-487c-8159-8484fe818669)(content(Whitespace\" \
             \"))))(Secondary((id \
             621e534b-f259-4a6a-b9c4-418d234e16e4)(content(Whitespace\"\\n\"))))(Tile((id \
             380b3d8e-eef4-48c5-a776-d099a678158e)(form(Compound \
             Let))(sort(Drv Exp))(shards(0 1 2))(children(((Secondary((id \
             6a537a57-abe6-4efe-8780-3e426e4569c5)(content(Whitespace\" \
             \"))))(Tile((id 64d7b95f-5506-4acc-9ba4-a808afcff87e)(form(Tok \
             y))(sort(Drv Pat))))(Secondary((id \
             95047bf9-6591-4ca4-b322-b5f3f2e9484c)(content(Whitespace\" \
             \")))))((Secondary((id \
             999c1eab-be1c-4fc5-b02a-28f8fbf73b26)(content(Whitespace\" \
             \"))))(Tile((id 24537006-b926-4b39-8c7c-1ca2ab453414)(form(Tok \
             y))(sort(Drv Exp))))(Secondary((id \
             9c3356bd-3c1c-4fc4-b8c1-76ba42fae241)(content(Whitespace\" \
             \"))))(Tile((id \
             36e48234-a642-43cc-9125-1269ec3723c3)(form(Compound \
             Minus))(sort(Drv Exp))))(Secondary((id \
             20ce6d63-8f0e-4f8f-94cd-14d5ac6b0c44)(content(Whitespace\" \
             \"))))(Tile((id 63949da0-6781-49ef-97a6-516534f1ce6f)(form(Tok \
             3))(sort(Drv Exp))))(Secondary((id \
             7332af5f-ac84-46bc-a967-b8aa9c258eee)(content(Whitespace\" \
             \")))))))))(Secondary((id \
             e88a1bed-a992-42bc-888c-0a05f815bae7)(content(Whitespace\" \
             \"))))(Tile((id 416058a4-da65-4255-8608-d94ff1a036cd)(form(Tok \
             f))(sort(Drv Exp))))(Tile((id \
             130f782f-5e2d-4f72-bd8e-5dec7dcb9807)(form(Compound Ap))(sort(Drv \
             Exp))(shards(0 1))(children(((Tile((id \
             78167743-988d-4517-8df0-16dfb5e832eb)(form(Tok y))(sort(Drv \
             Exp)))))))))(Secondary((id \
             5f5dfeae-eea2-4049-8f42-99b5d7ca798e)(content(Whitespace\"\\n\")))))))))(Secondary((id \
             dbdfe7e9-e1e1-4860-a29f-7eae67bf1bd0)(content(Whitespace\" \
             \")))))))))(Secondary((id \
             3e36b7e1-8929-4432-bd6a-59ee441751b6)(content(Whitespace\"\\n\"))))(Tile((id \
             7d6aab21-f8b8-461b-8b15-273c2fd91bb7)(form(Compound \
             Let))(shards(0 1 2))(children(((Secondary((id \
             1c9f5951-da6e-4f53-91ca-d45ffd4cd0f7)(content(Whitespace\" \
             \"))))(Tile((id 709a7c34-b735-4014-95e7-8b9083df682f)(form(Tok \
             $e1))(sort Pat)))(Secondary((id \
             e1a05c74-8c99-4017-bf62-22d9cad34c59)(content(Whitespace\" \
             \")))))((Secondary((id \
             e72dc16e-448a-4596-960c-e5140b083434)(content(Whitespace\" \
             \"))))(Tile((id \
             05195389-b0fa-41bd-85bf-746f5b37d819)(form(Compound \
             OfAlfaExp))(shards(0 1))(children(((Secondary((id \
             6762fc97-4b88-4add-b4bf-ad3b0e043698)(content(Whitespace\"\\n\"))))(Tile((id \
             b9680f12-72a9-4449-9154-0ce29c0ed73d)(form(Compound \
             Let))(sort(Drv Exp))(shards(0 1 2))(children(((Secondary((id \
             07fe03dd-ca8e-4f3a-8825-9888358455a9)(content(Whitespace\" \
             \"))))(Tile((id 1e837f42-6980-4aa9-8a7a-b0fe9f831a8e)(form(Tok \
             f))(sort(Drv Pat))))(Secondary((id \
             93935f83-4e08-4fdc-8d21-312c03f7b999)(content(Whitespace\" \
             \")))))((Secondary((id \
             75365600-4294-43b6-8d31-61d6d3b76968)(content(Whitespace\" \
             \"))))(Tile((id \
             3ecc0661-6902-4253-9399-4e4c456c0fdd)(form(Compound \
             Fun))(sort(Drv Exp))(shards(0 1))(children(((Secondary((id \
             f5e2383f-68e4-43d3-9b52-4f80df67ab6c)(content(Whitespace\" \
             \"))))(Tile((id 1b1e6860-c4b4-423f-bc6b-789f0ddf9f13)(form(Tok \
             z))(sort(Drv Pat))))(Secondary((id \
             361207f8-3451-40a5-acdc-b83f12589993)(content(Whitespace\" \
             \")))))))))(Secondary((id \
             f892f448-2501-4680-82d7-ed7e3ad4a34c)(content(Whitespace\" \
             \"))))(Tile((id 3727d8cd-e46c-452c-8305-1f56a840c87c)(form(Tok \
             y))(sort(Drv Exp))))(Secondary((id \
             3880ee86-dbbe-42e6-83c2-31fac21c757b)(content(Whitespace\" \
             \"))))(Tile((id \
             46016aed-511b-45f6-b7ba-2bfdae47eed9)(form(Compound \
             Times))(sort(Drv Exp))))(Secondary((id \
             1f42db46-77ef-4191-ad9b-83b26db66dbf)(content(Whitespace\" \
             \"))))(Tile((id cd364147-b8f1-4e6b-a7d9-6f2f9e7776be)(form(Tok \
             z))(sort(Drv Exp))))(Secondary((id \
             283091ad-1857-4ff8-97cd-1dbcb56abad2)(content(Whitespace\" \
             \")))))))))(Secondary((id \
             986ca6cd-5582-4236-be74-c834d1314e0d)(content(Whitespace\" \
             \"))))(Tile((id 3c614fe7-06fb-480b-93dc-4695bf5bfa42)(form(Tok \
             $e2))(sort(Drv Exp))))(Secondary((id \
             2f8282f4-ccbe-4882-a7dd-64647eb69e01)(content(Whitespace\"\\n\")))))))))(Secondary((id \
             77612c9a-ea31-4239-b3a9-654b081c5ae4)(content(Whitespace\" \
             \")))))))))(Secondary((id \
             0b932052-ce88-436e-98b9-831d46824ba7)(content(Whitespace\"\\n\"))))(Tile((id \
             e502a032-027f-47eb-88bb-2daca3d8bee4)(form(Compound \
             Let))(shards(0 1 2))(children(((Secondary((id \
             3b4e125e-0eb2-4f4b-8989-57102cbb5c73)(content(Whitespace\" \
             \"))))(Tile((id e97d9c85-9d4c-4d1f-ab62-c2ef69ead4cb)(form(Tok \
             $e_example))(sort Pat)))(Secondary((id \
             fea029de-2e5d-4a7b-accf-8365156e5271)(content(Whitespace\" \
             \")))))((Secondary((id \
             75dc218f-1aed-410d-b3b8-0192c48f9dbb)(content(Whitespace\" \
             \"))))(Tile((id \
             bad0269f-0bcc-4cac-8b0f-85f9bf96cb8d)(form(Compound \
             OfAlfaExp))(shards(0 1))(children(((Secondary((id \
             7ada529e-7c5e-464f-922e-e4ee9e4ab1af)(content(Whitespace\"\\n\"))))(Tile((id \
             35487ee8-6b9f-4556-bf0f-1dfbedf8ac4b)(form(Compound \
             Let))(sort(Drv Exp))(shards(0 1 2))(children(((Secondary((id \
             1b386f69-7e8c-4787-b245-23bfe9506319)(content(Whitespace\" \
             \"))))(Tile((id 33fc8815-b177-4427-8c36-5c2d6ca2f53a)(form(Tok \
             y))(sort(Drv Pat))))(Secondary((id \
             d528881f-0a57-489f-abca-300139bddf56)(content(Whitespace\" \
             \")))))((Secondary((id \
             923f4025-9779-4ab6-9186-0cc5010f7ad1)(content(Whitespace\" \
             \"))))(Tile((id a4aee586-f440-4fac-8f10-b6cab46735c2)(form(Tok \
             4))(sort(Drv Exp))))(Secondary((id \
             901a0f50-f7b3-44aa-ab41-aad64486f1a6)(content(Whitespace\" \
             \")))))))))(Secondary((id \
             e8fb5c66-ec47-45fe-ab6d-6e6d1ad97927)(content(Whitespace\" \
             \"))))(Tile((id 72a3a0cd-b95b-42b2-8d22-75dba802bcce)(form(Tok \
             $e1))(sort(Drv Exp))))(Secondary((id \
             da37a939-757e-4e25-9f71-b2a03158017d)(content(Whitespace\"\\n\")))))))))(Secondary((id \
             75687838-4b0e-43c7-845f-34843d88205d)(content(Whitespace\" \
             \"))))))))))((Grout((id \
             905fd5cf-e3b2-478f-95c6-aa2989721336)(shape \
             Convex))))))(ancestors())))(caret \
             Outer)(refractors((manuals())(multis((ids())(suppressed())(ephemerals())))(sample_focus((call_stack())(index \
             -1)(pinned_stack())(indicated_call())(time())(seq \
             0)(step_range())(pending_focus())))(autoprobe_target())(pending_probe_cursor()))))";
          backup_text =
            "let $e2 = of_alfa_exp \n\
             let y = y - 3 in f(y)\n\
             end in\n\
             let $e1 = of_alfa_exp\n\
             let f = fun z -> y * z in $e2\n\
             end in\n\
             let $e_example = of_alfa_exp\n\
             let y = 4 in $e1\n\
             end in";
        };
      rule_set = ALF;
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
                         8cb933de-8a16-4b91-a65e-4a659ff21cef)(form(Tok \
                         4))(sort(Drv Exp))))(Secondary((id \
                         067f6bb8-fd54-4cc2-af18-f3bbb2b16539)(content(Whitespace\" \
                         \"))))(Tile((id \
                         c21c98ef-1a8b-4d56-bd7c-e5068c15cb17)(form(Compound \
                         Eval))(sort(Drv Exp))))(Secondary((id \
                         3341dae1-29d8-4d99-a72c-0cb63dc303b8)(content(Whitespace\" \
                         \"))))(Tile((id \
                         4223870f-d0e2-4303-bef2-7dc45e2cca95)(form(Tok \
                         4))(sort(Drv Exp)))))()))(ancestors())))(caret \
                         Outer)(refractors((manuals())(multis((ids())(suppressed())(ephemerals())))(sample_focus((call_stack())(index \
                         -1)(pinned_stack())(indicated_call())(time())(seq \
                         0)(step_range())(pending_focus())))(autoprobe_target())(pending_probe_cursor()))))";
                      backup_text = "4 \\=/ 4";
                    };
                  rule = Some E_Val;
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
                               b8632abe-b07d-419e-a113-60602b97f160)(form(Compound \
                               Val))(sort(Drv Exp))(shards(0 \
                               1))(children(((Secondary((id \
                               871b21dd-ac21-40c6-981d-4ec4ab8a2d23)(content(Whitespace\" \
                               \"))))(Tile((id \
                               ba705854-8fae-480b-a4ab-220052d8c09e)(form(Tok \
                               4))(sort(Drv Exp))))(Secondary((id \
                               77cee27b-df37-4835-b66b-d7b27090a07e)(content(Whitespace\" \
                               \"))))))))))()))(ancestors())))(caret \
                               Outer)(refractors((manuals())(multis((ids())(suppressed())(ephemerals())))(sample_focus((call_stack())(index \
                               -1)(pinned_stack())(indicated_call())(time())(seq \
                               0)(step_range())(pending_focus())))(autoprobe_target())(pending_probe_cursor()))))";
                            backup_text = "val 4 end";
                          };
                        rule = Some V_Num;
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
                         e578e7e7-2ab0-406d-862d-3f64fdf0372b)(form(Tok \
                         1))(sort(Drv Exp))))(Secondary((id \
                         30566239-0aa5-428b-b8ef-b22467d0fd0c)(content(Whitespace\" \
                         \"))))(Tile((id \
                         90cc3c16-d658-489c-9a28-fe0ccbc913f8)(form(Compound \
                         Eval))(sort(Drv Exp))))(Secondary((id \
                         c097992b-853b-437f-af4a-0fa01adc1bff)(content(Whitespace\" \
                         \"))))(Tile((id \
                         97aea718-4fbc-42ac-9f2d-a8f0c6aaeed3)(form(Tok \
                         1))(sort(Drv Exp)))))()))(ancestors())))(caret \
                         Outer)(refractors((manuals())(multis((ids())(suppressed())(ephemerals())))(sample_focus((call_stack())(index \
                         -1)(pinned_stack())(indicated_call())(time())(seq \
                         0)(step_range())(pending_focus())))(autoprobe_target())(pending_probe_cursor()))))";
                      backup_text = "1 \\=/ 1";
                    };
                  rule = Some E_Val;
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
                               afc12e92-e070-4fff-af86-67553d8cdb77)(form(Compound \
                               Val))(sort(Drv Exp))(shards(0 \
                               1))(children(((Secondary((id \
                               afb4d3cc-6659-46f8-9924-a63746abf2ef)(content(Whitespace\" \
                               \"))))(Tile((id \
                               15be7525-7af6-4667-8ca3-f1b7de5e67df)(form(Tok \
                               1))(sort(Drv Exp))))(Secondary((id \
                               16525be7-bdc8-4570-9397-cb95f759e8d2)(content(Whitespace\" \
                               \"))))))))))()))(ancestors())))(caret \
                               Outer)(refractors((manuals())(multis((ids())(suppressed())(ephemerals())))(sample_focus((call_stack())(index \
                               -1)(pinned_stack())(indicated_call())(time())(seq \
                               0)(step_range())(pending_focus())))(autoprobe_target())(pending_probe_cursor()))))";
                            backup_text = "val 1 end";
                          };
                        rule = Some V_Num;
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
                         242bed50-a437-4a03-bc96-a4d27f28340f)(form(Compound \
                         Fun))(sort(Drv Exp))(shards(0 \
                         1))(children(((Secondary((id \
                         5d570f0f-9252-4dc0-9b92-1f2134947bc9)(content(Whitespace\" \
                         \"))))(Tile((id \
                         10b0a792-af86-46a7-a34e-cae4bc758fdd)(form(Tok \
                         z))(sort(Drv Pat))))(Secondary((id \
                         3a76f5f9-8b18-4ec6-8b7a-b8102905eae8)(content(Whitespace\" \
                         \")))))))))(Secondary((id \
                         3cf812e8-c35e-49f7-81ca-b52510130bb2)(content(Whitespace\" \
                         \"))))(Tile((id \
                         8b468022-dfe1-4c6f-b7af-f030c090b147)(form(Tok \
                         4))(sort(Drv Exp))))(Secondary((id \
                         0fd0307e-b67f-4c61-b93f-3f2a288e2d6e)(content(Whitespace\" \
                         \"))))(Tile((id \
                         1fdae411-275f-438a-90e2-902f891b0e99)(form(Compound \
                         Times))(sort(Drv Exp))))(Secondary((id \
                         a6dacfed-7ee8-42ab-a830-b4f83d049065)(content(Whitespace\" \
                         \"))))(Tile((id \
                         03ccb043-1963-4bc1-98c4-54cf6adcdb3a)(form(Tok \
                         z))(sort(Drv Exp))))(Secondary((id \
                         febe3f41-3cd3-4fec-b74f-211aa3ad675c)(content(Whitespace\" \
                         \"))))(Tile((id \
                         c8ee7dd9-7c1e-4687-8b4e-f0c6010a91e5)(form(Compound \
                         Eval))(sort(Drv Exp))))(Secondary((id \
                         2c14efd3-47de-4644-baa1-c1fa53791c04)(content(Whitespace\" \
                         \"))))(Tile((id \
                         ead1c43d-19c3-4feb-a2b2-9418ef50522b)(form(Compound \
                         Fun))(sort(Drv Exp))(shards(0 \
                         1))(children(((Secondary((id \
                         ffd50708-5fa0-4d02-bfcd-4069153bc2c8)(content(Whitespace\" \
                         \"))))(Tile((id \
                         ad9d5aa1-d9d4-425d-8d54-6c76eb56ddfc)(form(Tok \
                         z))(sort(Drv Pat))))(Secondary((id \
                         eaac476f-6738-4cec-a041-dea0390da683)(content(Whitespace\" \
                         \")))))))))(Secondary((id \
                         72051148-5df7-41f8-983a-cfb5e97666b7)(content(Whitespace\" \
                         \"))))(Tile((id \
                         86a83078-6ac2-460a-b497-33727261c9ae)(form(Tok \
                         4))(sort(Drv Exp))))(Secondary((id \
                         390302f7-6184-4615-9632-8077479d62e2)(content(Whitespace\" \
                         \"))))(Tile((id \
                         022cc3b9-f565-4a24-a6d2-448a5a1f3e98)(form(Compound \
                         Times))(sort(Drv Exp))))(Secondary((id \
                         0bd19253-9762-41ef-82b0-a101970da989)(content(Whitespace\" \
                         \"))))(Tile((id \
                         88791522-2079-4296-b232-80e88be60367)(form(Tok \
                         z))(sort(Drv Exp)))))()))(ancestors())))(caret \
                         Outer)(refractors((manuals())(multis((ids())(suppressed())(ephemerals())))(sample_focus((call_stack())(index \
                         -1)(pinned_stack())(indicated_call())(time())(seq \
                         0)(step_range())(pending_focus())))(autoprobe_target())(pending_probe_cursor()))))";
                      backup_text = "fun z -> 4 * z \\=/ fun z -> 4 * z";
                    };
                  rule = Some E_Val;
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
                               3085afc0-b244-4908-bac4-c5d1bffdd357)(form(Compound \
                               Val))(sort(Drv Exp))(shards(0 \
                               1))(children(((Secondary((id \
                               2e4ed7ff-00d8-4edc-a728-daada6153a19)(content(Whitespace\" \
                               \"))))(Tile((id \
                               bb213148-8423-4aea-8657-c33f8e0b9353)(form(Compound \
                               Fun))(sort(Drv Exp))(shards(0 \
                               1))(children(((Secondary((id \
                               807bc837-9bc4-494a-bcaf-fc98ccf01cd9)(content(Whitespace\" \
                               \"))))(Tile((id \
                               3c635cf3-c22e-4a96-8f23-c75425548a6d)(form(Tok \
                               z))(sort(Drv Pat))))(Secondary((id \
                               f5ff5dfb-4b00-47b1-82c6-b3862721c2ec)(content(Whitespace\" \
                               \")))))))))(Secondary((id \
                               98e786b5-03f2-44c2-a1f1-00c93cf74629)(content(Whitespace\" \
                               \"))))(Tile((id \
                               2b071bff-3fc7-40e5-bd8d-1a4bdd34ed79)(form(Tok \
                               4))(sort(Drv Exp))))(Secondary((id \
                               03f04b76-e235-4171-85f6-ad585800450c)(content(Whitespace\" \
                               \"))))(Tile((id \
                               f5381e9c-e467-416e-b2b5-748be1e2e51c)(form(Compound \
                               Times))(sort(Drv Exp))))(Secondary((id \
                               f558b105-1d6d-4c87-a783-399e562cc5b7)(content(Whitespace\" \
                               \"))))(Tile((id \
                               4521507d-491d-4613-b851-d0affb83ecb3)(form(Tok \
                               z))(sort(Drv Exp))))(Secondary((id \
                               381a65d3-7dc6-4020-b557-c9738c23d9ca)(content(Whitespace\" \
                               \"))))))))))()))(ancestors())))(caret \
                               Outer)(refractors((manuals())(multis((ids())(suppressed())(ephemerals())))(sample_focus((call_stack())(index \
                               -1)(pinned_stack())(indicated_call())(time())(seq \
                               0)(step_range())(pending_focus())))(autoprobe_target())(pending_probe_cursor()))))";
                            backup_text = "val fun z -> 4 * z end";
                          };
                        rule = Some V_Fun;
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
                         dee4d931-32e5-4bec-9f5c-473b22844751)(form(Compound \
                         Let))(sort(Drv Exp))(shards(0 1 \
                         2))(children(((Secondary((id \
                         2b984b82-16a9-4ccb-be96-797f6daf9607)(content(Whitespace\" \
                         \"))))(Tile((id \
                         75ef5aa6-2e04-4881-9fd3-ca450824b087)(form(Tok \
                         y))(sort(Drv Pat))))(Secondary((id \
                         5a62acfb-380c-4c2c-a5d8-e4fbbde60cca)(content(Whitespace\" \
                         \")))))((Secondary((id \
                         05883bb2-d515-459f-8f30-9d8f6603bc5a)(content(Whitespace\" \
                         \"))))(Tile((id \
                         f6f2fcc9-0d7a-41bb-8e3a-3de5f9e89411)(form(Tok \
                         4))(sort(Drv Exp))))(Secondary((id \
                         04d57a9a-2988-4dbf-b4f4-0859c55a012d)(content(Whitespace\" \
                         \"))))(Tile((id \
                         0fe6e0c7-c3fd-4291-878b-393e971c29ee)(form(Compound \
                         Minus))(sort(Drv Exp))))(Secondary((id \
                         aed7cca5-3522-4391-ad55-1143eb67f320)(content(Whitespace\" \
                         \"))))(Tile((id \
                         8d7d9733-d404-4f5b-abe7-c36951c49e27)(form(Tok \
                         3))(sort(Drv Exp))))(Secondary((id \
                         41b49ee7-def6-4823-a556-37018831a7d1)(content(Whitespace\" \
                         \")))))))))(Secondary((id \
                         13bb4eb5-86c6-4843-b24c-939517cc8479)(content(Whitespace\" \
                         \"))))(Tile((id \
                         8864eb7f-1004-485b-af0c-d20187189d45)(form(Compound \
                         Parens))(sort(Drv Exp))(shards(0 \
                         1))(children(((Tile((id \
                         3dbb343d-a60c-4888-806e-34b23eaee7f1)(form(Compound \
                         Fun))(sort(Drv Exp))(shards(0 \
                         1))(children(((Secondary((id \
                         e2780ffd-7210-44de-abf9-1a71713afe7f)(content(Whitespace\" \
                         \"))))(Tile((id \
                         c053c871-2038-448a-8de9-bd41192333f1)(form(Tok \
                         z))(sort(Drv Pat))))(Secondary((id \
                         97f59180-5e2d-491c-be40-e4005aa24af1)(content(Whitespace\" \
                         \")))))))))(Secondary((id \
                         1c7718db-6c8c-4c15-80ec-5ff5db11ef96)(content(Whitespace\" \
                         \"))))(Tile((id \
                         1edb9f67-3a00-4872-95d8-f1b9714f15c4)(form(Tok \
                         4))(sort(Drv Exp))))(Secondary((id \
                         82fbcc06-3cae-438a-bd65-5dcc5610b616)(content(Whitespace\" \
                         \"))))(Tile((id \
                         d76000db-803b-4c5e-87aa-d89206e751e4)(form(Compound \
                         Times))(sort(Drv Exp))))(Secondary((id \
                         ba9089ac-0b25-4d11-b346-aa2bb0ad75ca)(content(Whitespace\" \
                         \"))))(Tile((id \
                         b50f760f-ce21-43ce-a2d5-319e95b007de)(form(Tok \
                         z))(sort(Drv Exp)))))))))(Tile((id \
                         eefe2644-9b6a-494a-bed5-7dd5d5a01f5e)(form(Compound \
                         Ap))(sort(Drv Exp))(shards(0 1))(children(((Tile((id \
                         d390f9bc-612f-4800-9387-b080ae80365f)(form(Tok \
                         y))(sort(Drv Exp)))))))))(Secondary((id \
                         95791711-11e5-4445-8413-3ad194f279a7)(content(Whitespace\" \
                         \"))))(Tile((id \
                         da213791-7814-4a40-b906-d5588e7ef101)(form(Compound \
                         Eval))(sort(Drv Exp))))(Secondary((id \
                         f6d5636d-f665-4a73-b4ee-71c38e233399)(content(Whitespace\" \
                         \"))))(Tile((id \
                         e9c300ae-0de3-43f5-bdec-86be5d702c6d)(form(Tok \
                         4))(sort(Drv Exp)))))()))(ancestors())))(caret \
                         Outer)(refractors((manuals())(multis((ids())(suppressed())(ephemerals())))(sample_focus((call_stack())(index \
                         -1)(pinned_stack())(indicated_call())(time())(seq \
                         0)(step_range())(pending_focus())))(autoprobe_target())(pending_probe_cursor()))))";
                      backup_text =
                        "let y = 4 - 3 in (fun z -> 4 * z)(y) \\=/ 4";
                    };
                  rule = Some E_Let;
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
                               bfbe8433-4b90-460a-b3e4-8fed25e2c477)(form(Tok \
                               4))(sort(Drv Exp))))(Secondary((id \
                               e4220038-1df8-4186-aad5-6e1ccc72b4be)(content(Whitespace\" \
                               \"))))(Tile((id \
                               5f22cc5d-8f16-4896-9948-de072aae6cce)(form(Compound \
                               Minus))(sort(Drv Exp))))(Secondary((id \
                               04a28061-8214-4da8-a613-44b0066d1865)(content(Whitespace\" \
                               \"))))(Tile((id \
                               59891dbf-3eb5-42ba-a63f-a18d9ae04b27)(form(Tok \
                               3))(sort(Drv Exp))))(Secondary((id \
                               9a72e49e-121d-43a9-8968-de8da82e2d9e)(content(Whitespace\" \
                               \"))))(Tile((id \
                               c1675c0b-6e49-4d56-b80a-5f7a3450e653)(form(Compound \
                               Eval))(sort(Drv Exp))))(Secondary((id \
                               5886b042-3210-46b1-95c8-4b99c1255dce)(content(Whitespace\" \
                               \"))))(Tile((id \
                               1971988d-d47d-482d-93c2-266329058723)(form(Tok \
                               1))(sort(Drv Exp)))))()))(ancestors())))(caret \
                               Outer)(refractors((manuals())(multis((ids())(suppressed())(ephemerals())))(sample_focus((call_stack())(index \
                               -1)(pinned_stack())(indicated_call())(time())(seq \
                               0)(step_range())(pending_focus())))(autoprobe_target())(pending_probe_cursor()))))";
                            backup_text = "4 - 3 \\=/ 1";
                          };
                        rule = Some E_Minus;
                      },
                    [
                      Node (Abbr (Some 0), []);
                      Node
                        ( Just
                            {
                              jdmt =
                                {
                                  zipper =
                                    "((selection((focus Left)(content())(mode \
                                     Normal)(anchor_caret Outer)(smart_rounded \
                                     false)))(relatives((siblings(((Tile((id \
                                     10b96b29-ac04-4a2d-ae73-97258af9e039)(form(Tok \
                                     3))(sort(Drv Exp))))(Secondary((id \
                                     84306179-e22b-467b-9982-85a26b4958b0)(content(Whitespace\" \
                                     \"))))(Tile((id \
                                     2eec0d7f-3402-4269-b04b-9a0b65c64b9e)(form(Compound \
                                     Eval))(sort(Drv Exp))))(Secondary((id \
                                     af89bee3-1c2c-4f89-8c34-4e2dfa5cf920)(content(Whitespace\" \
                                     \"))))(Tile((id \
                                     298c8bf9-958f-4f59-aa52-a92f8e67d36a)(form(Tok \
                                     3))(sort(Drv \
                                     Exp)))))()))(ancestors())))(caret \
                                     Outer)(refractors((manuals())(multis((ids())(suppressed())(ephemerals())))(sample_focus((call_stack())(index \
                                     -1)(pinned_stack())(indicated_call())(time())(seq \
                                     0)(step_range())(pending_focus())))(autoprobe_target())(pending_probe_cursor()))))";
                                  backup_text = "3 \\=/ 3";
                                };
                              rule = Some E_Val;
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
                                           54d7957e-3ea3-420b-8b86-7e58a0c1cc0a)(form(Compound \
                                           Val))(sort(Drv Exp))(shards(0 \
                                           1))(children(((Secondary((id \
                                           45c1f4d0-ca04-4df3-9c5f-f0bceb616a86)(content(Whitespace\" \
                                           \"))))(Tile((id \
                                           eac335fa-d449-4740-9bdb-147acf41c213)(form(Tok \
                                           3))(sort(Drv Exp))))(Secondary((id \
                                           23716e46-2391-4a2f-9792-bad4e344b28e)(content(Whitespace\" \
                                           \"))))))))))()))(ancestors())))(caret \
                                           Outer)(refractors((manuals())(multis((ids())(suppressed())(ephemerals())))(sample_focus((call_stack())(index \
                                           -1)(pinned_stack())(indicated_call())(time())(seq \
                                           0)(step_range())(pending_focus())))(autoprobe_target())(pending_probe_cursor()))))";
                                        backup_text = "val 3 end";
                                      };
                                    rule = Some V_Num;
                                  },
                                [] );
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
                               2af5cd5b-2203-49e1-840d-1563a7485fd8)(form(Compound \
                               Parens))(sort(Drv Exp))(shards(0 \
                               1))(children(((Tile((id \
                               23f9e963-0e2d-4dbd-9af8-8cbd1231f418)(form(Compound \
                               Fun))(sort(Drv Exp))(shards(0 \
                               1))(children(((Secondary((id \
                               6c0552f6-109c-478a-9388-e3d7e4a1ce35)(content(Whitespace\" \
                               \"))))(Tile((id \
                               c9bb6ffd-d0b4-4ac1-9dda-330de5e9be71)(form(Tok \
                               z))(sort(Drv Pat))))(Secondary((id \
                               aa24184d-4db4-4a81-9bfa-d4786ae4e24f)(content(Whitespace\" \
                               \")))))))))(Secondary((id \
                               9cc16fcd-f024-45cf-95d7-b36ba438e84c)(content(Whitespace\" \
                               \"))))(Tile((id \
                               b7f18ec1-e489-4fad-8783-456af8c3d42b)(form(Tok \
                               4))(sort(Drv Exp))))(Secondary((id \
                               a4bb8c94-f44e-48ad-9ee8-223840ff0bf7)(content(Whitespace\" \
                               \"))))(Tile((id \
                               48dfbb00-6e04-435b-8e52-e3da64cd5e47)(form(Compound \
                               Times))(sort(Drv Exp))))(Secondary((id \
                               9321d486-fc80-4f93-8dbb-12d41dceb349)(content(Whitespace\" \
                               \"))))(Tile((id \
                               65c4b963-8739-49f3-b7e0-488c70c95380)(form(Tok \
                               z))(sort(Drv Exp)))))))))(Tile((id \
                               01d0d06a-313f-4312-b18b-35912094716d)(form(Compound \
                               Ap))(sort(Drv Exp))(shards(0 \
                               1))(children(((Tile((id \
                               9893b64b-b035-46f0-92f8-aa9fcfce094f)(form(Tok \
                               1))(sort(Drv Exp)))))))))(Secondary((id \
                               9faac9f5-adb6-4c2e-852d-5765828ec369)(content(Whitespace\" \
                               \"))))(Tile((id \
                               d9abcf84-119f-4c87-86ec-98ad71239c82)(form(Compound \
                               Eval))(sort(Drv Exp))))(Secondary((id \
                               fd39f5cf-0522-432b-a4b3-5e236c891be5)(content(Whitespace\" \
                               \"))))(Tile((id \
                               90ba887a-9440-4596-bbd8-b9f175d03c8a)(form(Tok \
                               4))(sort(Drv Exp)))))()))(ancestors())))(caret \
                               Outer)(refractors((manuals())(multis((ids())(suppressed())(ephemerals())))(sample_focus((call_stack())(index \
                               -1)(pinned_stack())(indicated_call())(time())(seq \
                               0)(step_range())(pending_focus())))(autoprobe_target())(pending_probe_cursor()))))";
                            backup_text = "(fun z -> 4 * z)(1) \\=/ 4";
                          };
                        rule = Some E_Ap;
                      },
                    [
                      Node (Abbr (Some 2), []);
                      Node (Abbr (Some 1), []);
                      Node
                        ( Just
                            {
                              jdmt =
                                {
                                  zipper =
                                    "((selection((focus Left)(content())(mode \
                                     Normal)(anchor_caret Outer)(smart_rounded \
                                     false)))(relatives((siblings(((Tile((id \
                                     3f141c88-1267-491d-af4f-12634920b469)(form(Tok \
                                     4))(sort(Drv Exp))))(Secondary((id \
                                     5fe0b8bb-1466-45db-949a-445f1d1c4938)(content(Whitespace\" \
                                     \"))))(Tile((id \
                                     58da0fd7-6a7c-44c3-9d49-9d53d64b6c8a)(form(Compound \
                                     Times))(sort(Drv Exp))))(Secondary((id \
                                     745db6fd-6d38-4190-9edb-1ee3233cbb19)(content(Whitespace\" \
                                     \"))))(Tile((id \
                                     4d9d030b-7281-46c8-9695-cf7d24463bfa)(form(Tok \
                                     1))(sort(Drv Exp))))(Secondary((id \
                                     b0c0db23-32fe-4b61-aa95-d0754b3f2578)(content(Whitespace\" \
                                     \"))))(Tile((id \
                                     49373b22-4cea-48ab-9f32-97f948b8d4cd)(form(Compound \
                                     Eval))(sort(Drv Exp))))(Secondary((id \
                                     6d6e8a22-57c7-4023-adff-8e108794938e)(content(Whitespace\" \
                                     \"))))(Tile((id \
                                     c8ebe102-a99e-433a-a879-8cc12804f399)(form(Tok \
                                     4))(sort(Drv \
                                     Exp)))))()))(ancestors())))(caret \
                                     Outer)(refractors((manuals())(multis((ids())(suppressed())(ephemerals())))(sample_focus((call_stack())(index \
                                     -1)(pinned_stack())(indicated_call())(time())(seq \
                                     0)(step_range())(pending_focus())))(autoprobe_target())(pending_probe_cursor()))))";
                                  backup_text = "4 * 1 \\=/ 4";
                                };
                              rule = Some E_Times;
                            },
                          [ Node (Abbr (Some 0), []); Node (Abbr (Some 1), []) ]
                        );
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
                         5de8d19a-6ed2-42f5-8f22-3d9337735251)(form(Compound \
                         Let))(sort(Drv Exp))(shards(0 1 \
                         2))(children(((Secondary((id \
                         9a10efb9-6196-486a-9a91-25689164b50b)(content(Whitespace\" \
                         \"))))(Tile((id \
                         a79507c5-4432-4b28-8462-3084993ce385)(form(Tok \
                         f))(sort(Drv Pat))))(Secondary((id \
                         8e13b97a-5edf-436d-aab2-129d3d794e53)(content(Whitespace\" \
                         \")))))((Secondary((id \
                         1bf0add0-26b6-45ae-a3b1-6b77967c674f)(content(Whitespace\" \
                         \"))))(Tile((id \
                         3f62e2c5-77f7-4d98-ac8c-3efe6e63c00e)(form(Compound \
                         Fun))(sort(Drv Exp))(shards(0 \
                         1))(children(((Secondary((id \
                         667df314-7a59-4124-a303-89a555d2768a)(content(Whitespace\" \
                         \"))))(Tile((id \
                         1678331d-f89b-47b0-8084-58b9fba7a3c8)(form(Tok \
                         z))(sort(Drv Pat))))(Secondary((id \
                         8ffe77c5-30df-4f81-99bb-478353d23eb1)(content(Whitespace\" \
                         \")))))))))(Secondary((id \
                         d0ab8023-0b42-4bb4-ac6c-aacb26ae7a66)(content(Whitespace\" \
                         \"))))(Tile((id \
                         ef143641-f37a-4711-bf82-9857f586881a)(form(Tok \
                         4))(sort(Drv Exp))))(Secondary((id \
                         ebae3510-71c8-4dca-a744-44283247d111)(content(Whitespace\" \
                         \"))))(Tile((id \
                         fcb46390-35d7-424c-a0a8-b544a94d5697)(form(Compound \
                         Times))(sort(Drv Exp))))(Secondary((id \
                         c12d8dbb-1f49-4a84-a75b-b150392e133c)(content(Whitespace\" \
                         \"))))(Tile((id \
                         420124b6-e924-4004-83f1-8fa3e655caa3)(form(Tok \
                         z))(sort(Drv Exp))))(Secondary((id \
                         89e3a3c9-2fc4-4724-a0f0-dbd0e3c0a341)(content(Whitespace\" \
                         \")))))))))(Secondary((id \
                         1e8f2110-3cf8-462f-a522-b0244aec97ed)(content(Whitespace\" \
                         \"))))(Tile((id \
                         580b65f4-b1e2-4adb-b73d-ab1b9e8eedfb)(form(Compound \
                         Let))(sort(Drv Exp))(shards(0 1 \
                         2))(children(((Secondary((id \
                         dee78414-d889-4ffb-8b65-fa5679e0936f)(content(Whitespace\" \
                         \"))))(Tile((id \
                         c081d6a1-350a-43e8-91fd-c108fb3cd6ea)(form(Tok \
                         y))(sort(Drv Pat))))(Secondary((id \
                         5bb1d08c-4c24-4405-abec-720b32ae6bff)(content(Whitespace\" \
                         \")))))((Secondary((id \
                         3c2d83a4-e42a-4123-b97e-a2d138c49e51)(content(Whitespace\" \
                         \"))))(Tile((id \
                         edd7e90c-de50-4b8d-9caa-c3f545f9c3c3)(form(Tok \
                         4))(sort(Drv Exp))))(Secondary((id \
                         eb2809cb-b944-4243-8d7b-58dac6cb4f2b)(content(Whitespace\" \
                         \"))))(Tile((id \
                         0d0be291-8e96-4cc8-8584-e635ca1b3f32)(form(Compound \
                         Minus))(sort(Drv Exp))))(Secondary((id \
                         86be1663-48dd-43ac-b175-e33dfa97f252)(content(Whitespace\" \
                         \"))))(Tile((id \
                         02020347-0c1f-4aed-b868-bda3189f2f95)(form(Tok \
                         3))(sort(Drv Exp))))(Secondary((id \
                         dc9e53aa-8783-4607-a718-607955eb53b0)(content(Whitespace\" \
                         \")))))))))(Secondary((id \
                         ab63b963-c41b-4c8c-967d-0b068130b64c)(content(Whitespace\" \
                         \"))))(Tile((id \
                         7455cc58-a172-47b5-bf07-a89b6f7f302f)(form(Tok \
                         f))(sort(Drv Exp))))(Tile((id \
                         30d8567f-69b2-499d-aac5-505ce473f1cf)(form(Compound \
                         Ap))(sort(Drv Exp))(shards(0 1))(children(((Tile((id \
                         65f4199e-72f3-428c-b93d-a1c3b4347629)(form(Tok \
                         y))(sort(Drv Exp)))))))))(Secondary((id \
                         65807295-60bd-4091-be00-66fda8a44160)(content(Whitespace\" \
                         \"))))(Tile((id \
                         6ab87de4-cc54-4a8c-b33e-2cc74f4c58e9)(form(Compound \
                         Eval))(sort(Drv Exp))))(Secondary((id \
                         83c9f93e-40ea-4170-982a-73e496db8aa3)(content(Whitespace\" \
                         \"))))(Tile((id \
                         e5935322-c5cd-446a-ae04-6bf5314d2ae8)(form(Tok \
                         4))(sort(Drv Exp)))))()))(ancestors())))(caret \
                         Outer)(refractors((manuals())(multis((ids())(suppressed())(ephemerals())))(sample_focus((call_stack())(index \
                         -1)(pinned_stack())(indicated_call())(time())(seq \
                         0)(step_range())(pending_focus())))(autoprobe_target())(pending_probe_cursor()))))";
                      backup_text =
                        "let f = fun z -> 4 * z in let y = 4 - 3 in f(y) \\=/ 4";
                    };
                  rule = Some E_Let;
                },
              [ Node (Abbr (Some 2), []); Node (Abbr (Some 3), []) ] );
          Node
            ( Just
                {
                  jdmt =
                    {
                      zipper =
                        "((selection((focus Left)(content())(mode \
                         Normal)(anchor_caret Outer)(smart_rounded \
                         false)))(relatives((siblings(((Tile((id \
                         533555b1-a552-497c-8029-c1850129b959)(form(Tok \
                         $e_example))(sort(Drv Exp))))(Secondary((id \
                         a8e2afc8-69a1-4457-b028-01b72dd9de13)(content(Whitespace\" \
                         \"))))(Tile((id \
                         6202ee44-b6a2-4991-af46-d86cabc5a9c1)(form(Compound \
                         Eval))(sort(Drv Exp))))(Secondary((id \
                         81c6cfad-c9b0-4fbb-9e3d-49cfee635959)(content(Whitespace\" \
                         \"))))(Tile((id \
                         b11036eb-f867-459c-bcbf-882bb3acd470)(form(Tok \
                         4))(sort(Drv Exp)))))()))(ancestors())))(caret \
                         Outer)(refractors((manuals())(multis((ids())(suppressed())(ephemerals())))(sample_focus((call_stack())(index \
                         -1)(pinned_stack())(indicated_call())(time())(seq \
                         0)(step_range())(pending_focus())))(autoprobe_target())(pending_probe_cursor()))))";
                      backup_text = "$e_example \\=/ 4";
                    };
                  rule = Some E_Let;
                },
              [ Node (Abbr (Some 0), []); Node (Abbr (Some 4), []) ] );
        ];
    }
