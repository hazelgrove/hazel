let exercise : Exercise.t =
  Code
    (CodeExercise.of_persistent
       {
         id = Haz3lcore.Id.v "12f5e34d-d211-4332-91e2-815e9e183885";
         title = "Recursive Fibonacci";
         module_name = "Ex_RecursiveFibonacci";
         prompt =
           "Write test cases for, and then implement, a function that \
            recursively determines the nth Fibonacci number. \n\
            `fib(n)` is equivalent to the `n`th Fibonacci number, assuming `n \
            >= 0`.";
         point_distribution =
           { test_validation = 1; mutation_testing = 1; impl_grading = 2 };
         prelude =
           {
             zipper =
               "((selection((focus Left)(content())(mode Normal)(anchor_caret \
                Outer)(smart_rounded \
                false)))(relatives((siblings(()((Grout((id \
                2af57678-102b-4438-ac42-17e459994d2e)(shape \
                Convex))))))(ancestors())))(caret \
                Outer)(refractors((manuals())(multis((ids())(suppressed())(ephemerals())))(sample_focus((call_stack())(index \
                -1)(pinned_stack())(indicated_call())(time())(seq \
                0)(step_range())(pending_focus())))(autoprobe_target())(pending_probe_cursor()))))";
             backup_text = "";
           };
         correct_impl =
           {
             zipper =
               "((selection((focus Left)(content())(mode Normal)(anchor_caret \
                Outer)(smart_rounded false)))(relatives((siblings(((Tile((id \
                04d7a3fe-5be9-4166-ab6d-5ce1738f3438)(form(Compound \
                Let))(shards(0 1 2))(children(((Secondary((id \
                6807482e-fb7d-4f8f-998c-40df41ed4f4a)(content(Whitespace\" \
                \"))))(Tile((id 5036b873-6b36-4fe1-b9a9-02d74d5d32ad)(form(Tok \
                fib))(sort Pat)))(Tile((id \
                797e3bc5-3665-4d75-b449-305f7a1f7ec2)(form(Compound \
                TypeAsc))(sort Pat)))(Secondary((id \
                f7c0e12f-1783-4162-8e56-14a3817d0d9f)(content(Whitespace\" \
                \"))))(Tile((id 21e5991a-e673-43f6-a63f-e8b4f78c6bca)(form(Tok \
                Int))(sort Typ)))(Secondary((id \
                598cdb1b-1ce5-4b95-8f13-437339ebffe3)(content(Whitespace\" \
                \"))))(Tile((id \
                b918df3d-806a-44a9-b9f1-45ae98b97653)(form(Compound \
                TypeArrow))(sort Typ)))(Secondary((id \
                ae4dc9fd-9546-48e7-a684-62c9def50240)(content(Whitespace\" \
                \"))))(Tile((id 9b542fe7-73d8-4a10-acc7-4bdb045d7621)(form(Tok \
                Int))(sort Typ)))(Secondary((id \
                36ff5dc6-e835-4e69-a575-90ac23bb90ff)(content(Whitespace\" \
                \")))))((Secondary((id \
                a4c70f8c-23c8-4191-a9f9-d71cbc6977e6)(content(Whitespace\" \
                \"))))(Secondary((id \
                482b8028-8848-4338-b972-4f9ce61e36e2)(content(Whitespace\"\\n\"))))(Tile((id \
                45c380c8-dfd0-46ca-9919-037a040b4999)(form(Compound \
                Fun))(shards(0 1))(children(((Secondary((id \
                3ff47d28-1cf3-40c8-848f-0da4148cdd60)(content(Whitespace\" \
                \"))))(Tile((id 4103512f-545f-4897-941c-454c467c9821)(form(Tok \
                x))(sort Pat)))(Secondary((id \
                6bbbcc67-0ae5-4d48-85e2-6543f00113bb)(content(Whitespace\" \
                \")))))))))(Secondary((id \
                09c4a903-498f-482c-8424-fa7abf6289bb)(content(Whitespace\" \
                \"))))(Secondary((id \
                0db2875f-0c06-4c5a-84f1-b39cdb518a9c)(content(Whitespace\"\\n\"))))(Tile((id \
                9bbf2c6b-53af-46a6-8551-f4961632851d)(form(Compound \
                If))(shards(0 1 2))(children(((Secondary((id \
                0f44f5de-7991-41b3-a8ae-3cf7a84e22e6)(content(Whitespace\" \
                \"))))(Tile((id fee229a0-a87e-40bc-8f13-af38696d6d0d)(form(Tok \
                x))))(Secondary((id \
                19e395ed-f864-4e06-a763-80f02250c3a8)(content(Whitespace\" \
                \"))))(Tile((id \
                384bffb6-7537-4119-af55-17251f084cba)(form(Compound \
                Lt))))(Secondary((id \
                9cb8cafb-3d76-48bd-b9ed-145b8515e7a9)(content(Whitespace\" \
                \"))))(Tile((id c8382cab-6fee-496a-bed6-bcdae76e14f4)(form(Tok \
                2))))(Secondary((id \
                58b61873-821a-44ce-a543-e12e62b53ea2)(content(Whitespace\" \
                \")))))((Secondary((id \
                ec646985-dd0b-4ce5-b82d-dfbc749f8efa)(content(Whitespace\" \
                \"))))(Tile((id da319f12-c8d0-4cd0-b0df-c1cb83e6307d)(form(Tok \
                1))))(Secondary((id \
                42214a76-1b07-407c-8354-d1243877e1e0)(content(Whitespace\" \
                \"))))(Secondary((id \
                69ecd84b-2014-46f0-a4a0-589e8c99e81a)(content(Whitespace\"\\n\")))))))))(Secondary((id \
                9c097983-d745-4a47-8b63-5943c484d387)(content(Whitespace\" \
                \"))))(Tile((id fb8d06a0-e2f8-4b4f-bdfb-02164d34a39e)(form(Tok \
                fib))))(Tile((id \
                5c9401de-f0bf-43cb-8f0a-5b60f08c51f0)(form(Compound \
                Ap))(shards(0 1))(children(((Tile((id \
                0b7b16b4-5837-4acd-b460-e7c509d84148)(form(Tok \
                x))))(Secondary((id \
                353ba1c3-6881-459d-9d67-041fb49b3647)(content(Whitespace\" \
                \"))))(Tile((id \
                9e7f220f-83b7-4d20-943f-a15a03e59804)(form(Compound \
                Minus))))(Secondary((id \
                a1830c8e-d0b8-42b8-8f96-23da263c43cb)(content(Whitespace\" \
                \"))))(Tile((id 707ceba5-563d-4151-937a-07dbd7b8ec9d)(form(Tok \
                1)))))))))(Secondary((id \
                4e3dace2-572e-4953-b015-b34b14e9a058)(content(Whitespace\" \
                \"))))(Tile((id \
                7e53b7f4-27c2-4758-a6c7-17fa7327c669)(form(Compound \
                Plus))))(Secondary((id \
                aab278ec-b77f-4197-980e-6d321f735f5f)(content(Whitespace\" \
                \"))))(Tile((id 9df8f4be-e327-448e-ac23-6cb6c1cd6ae7)(form(Tok \
                fib))))(Tile((id \
                0a775383-33a8-4f99-bd89-b03d7b2ee4e4)(form(Compound \
                Ap))(shards(0 1))(children(((Tile((id \
                9411c072-f6a1-4459-9a43-62b750d01411)(form(Tok \
                x))))(Secondary((id \
                2982efee-dd35-4667-bcb8-ea194224697a)(content(Whitespace\" \
                \"))))(Tile((id \
                983b0c18-2193-4d73-97c9-ee0638da7ecf)(form(Compound \
                Minus))))(Secondary((id \
                665e263f-01f6-4ce5-a163-814c9e1fc5a6)(content(Whitespace\" \
                \"))))(Tile((id abdcd4e5-9d94-4d09-8294-876a14126b53)(form(Tok \
                2)))))))))(Secondary((id \
                50b93566-f938-40cf-97dd-be6c21e8f642)(content(Whitespace\" \
                \"))))(Secondary((id \
                7e6bbab7-e654-46dc-a455-4e445eae6462)(content(Whitespace\"\\n\")))))))))(Secondary((id \
                594e2462-c189-41c8-99a1-28f20098b356)(content(Whitespace\" \
                \")))))((Grout((id acbb5393-7a49-429b-8ad3-0d5172b56c63)(shape \
                Convex))))))(ancestors())))(caret \
                Outer)(refractors((manuals())(multis((ids())(suppressed())(ephemerals())))(sample_focus((call_stack())(index \
                -1)(pinned_stack())(indicated_call())(time())(seq \
                0)(step_range())(pending_focus())))(autoprobe_target())(pending_probe_cursor()))))";
             backup_text =
               "let fib: Int -> Int = \n\
                fun x -> \n\
                if x < 2 then 1 \n\
                else fib(x - 1) + fib(x - 2) \n\
                in ";
           };
         your_tests =
           {
             tests =
               {
                 zipper =
                   "((selection((focus Left)(content())(mode \
                    Normal)(anchor_caret Outer)(smart_rounded \
                    false)))(relatives((siblings(()((Grout((id \
                    4b1de491-bcca-45eb-ac42-363ef5e29e4d)(shape \
                    Convex))))))(ancestors())))(caret \
                    Outer)(refractors((manuals())(multis((ids())(suppressed())(ephemerals())))(sample_focus((call_stack())(index \
                    -1)(pinned_stack())(indicated_call())(time())(seq \
                    0)(step_range())(pending_focus())))(autoprobe_target())(pending_probe_cursor()))))";
                 backup_text = "";
               };
             required = 5;
             provided = 0;
           };
         your_impl =
           {
             zipper =
               "((selection((focus Left)(content())(mode Normal)(anchor_caret \
                Outer)(smart_rounded false)))(relatives((siblings(((Tile((id \
                1d5c9f41-9f00-4fce-8cc2-1ec4b304b8a6)(form(Compound \
                Let))(shards(0 1 2))(children(((Secondary((id \
                12e5cea8-8cc8-4996-b4e7-80efd4f8f584)(content(Whitespace\" \
                \"))))(Tile((id 02f34dbf-b103-40e4-a7cc-00ea4df55adb)(form(Tok \
                fib))(sort Pat)))(Secondary((id \
                a73081a1-1206-48d7-b2ab-122fcf86a03d)(content(Whitespace\" \
                \"))))(Tile((id \
                7d06a9b7-a2fb-40f0-ab0d-ec41b17b2aab)(form(Compound \
                TypeAsc))(sort Pat)))(Secondary((id \
                6616e7d3-e1d8-4430-90de-1fe4daebd8ad)(content(Whitespace\" \
                \"))))(Tile((id 133e16bb-69cb-4a8f-aeb6-3605293cf49c)(form(Tok \
                Int))(sort Typ)))(Secondary((id \
                d6bb3d2e-7765-488b-b475-e874d097c5f1)(content(Whitespace\" \
                \"))))(Tile((id \
                88dacd74-0016-4d03-926c-2260afefc260)(form(Compound \
                TypeArrow))(sort Typ)))(Secondary((id \
                70c33c70-1276-42ef-9bfa-0676b49964ba)(content(Whitespace\" \
                \"))))(Tile((id ffdca45f-52a3-4f91-9a5c-18b92e231fc9)(form(Tok \
                Int))(sort Typ)))(Secondary((id \
                95b817ac-62f2-4c16-92c6-8498fa9bfd01)(content(Whitespace\" \
                \")))))((Secondary((id \
                212bd2d0-dab5-4716-b0ac-862551bf9f45)(content(Whitespace\" \
                \"))))(Secondary((id \
                ba301a3e-2311-40aa-8e05-80c8202fe021)(content(Whitespace\"\\n\"))))(Tile((id \
                a1eb2de3-f302-4438-af87-9a5e85a5c022)(form(Compound \
                Fun))(shards(0 1))(children(((Secondary((id \
                2243a934-ed68-4108-aca6-44f85b95b9a6)(content(Whitespace\" \
                \"))))(Tile((id 6b7d292f-5629-4b4a-b393-8c9ea347b4a9)(form(Tok \
                n))(sort Pat)))(Secondary((id \
                019622f8-94e1-4bc6-a432-d7c3743b59e3)(content(Whitespace\" \
                \")))))))))(Grout((id \
                2a36e02f-5b47-4f87-98f6-c27c3910b2c3)(shape \
                Convex)))(Secondary((id \
                fee6ece0-76a2-4589-8d93-5aec53d75723)(content(Whitespace\" \
                \"))))(Secondary((id \
                a7fd3784-835a-4867-b49b-855b1b184256)(content(Whitespace\"\\n\")))))))))(Secondary((id \
                ad6ddecf-06bb-446d-bf59-35d9450629aa)(content(Whitespace\" \
                \")))))((Grout((id 6612b274-1a79-4800-bca5-3d33604bcf33)(shape \
                Convex))))))(ancestors())))(caret \
                Outer)(refractors((manuals())(multis((ids())(suppressed())(ephemerals())))(sample_focus((call_stack())(index \
                -1)(pinned_stack())(indicated_call())(time())(seq \
                0)(step_range())(pending_focus())))(autoprobe_target())(pending_probe_cursor()))))";
             backup_text = "let fib : Int -> Int = \nfun n -> \nin ";
           };
         hidden_bugs =
           [
             {
               impl =
                 {
                   zipper =
                     "((selection((focus Left)(content())(mode \
                      Normal)(anchor_caret Outer)(smart_rounded \
                      false)))(relatives((siblings(((Tile((id \
                      906f9f3c-2e0d-4369-82b1-7d19882cb1ba)(form(Compound \
                      Let))(shards(0 1 2))(children(((Secondary((id \
                      9c655358-0204-4de6-a493-e6ae8391cd02)(content(Whitespace\" \
                      \"))))(Tile((id \
                      1f426925-aa22-4f64-89d6-72f17d8816ec)(form(Tok \
                      fib))(sort Pat)))(Tile((id \
                      134f505e-6004-48df-bfd6-f461b9aad4d6)(form(Compound \
                      TypeAsc))(sort Pat)))(Secondary((id \
                      bb31bd3e-7050-4f86-a0e5-292f8bdc38d0)(content(Whitespace\" \
                      \"))))(Tile((id \
                      32a334cd-58ed-4cd9-b56e-71cae9082f7d)(form(Tok \
                      Int))(sort Typ)))(Secondary((id \
                      0c358fd3-55d3-44ee-ae08-a513f03a7cfb)(content(Whitespace\" \
                      \"))))(Tile((id \
                      37d4f2dc-bb0e-4fda-b3e0-094eb77b09dc)(form(Compound \
                      TypeArrow))(sort Typ)))(Secondary((id \
                      3dca7647-ea24-4008-9c1d-3908216b6f3a)(content(Whitespace\" \
                      \"))))(Tile((id \
                      dcf19463-13c2-4d04-97e8-b927f78c424b)(form(Tok \
                      Int))(sort Typ)))(Secondary((id \
                      4c4416dc-00ab-4de1-bece-90e0909a5604)(content(Whitespace\" \
                      \")))))((Secondary((id \
                      e1d5af16-dedf-4071-b336-101c0e43c8f1)(content(Whitespace\" \
                      \"))))(Secondary((id \
                      88527005-9e0a-41c0-a4a3-71698d8c5735)(content(Whitespace\"\\n\"))))(Tile((id \
                      32cc45c6-6368-4dca-8cf2-8ac9f8bafd31)(form(Compound \
                      Fun))(shards(0 1))(children(((Secondary((id \
                      bcafc02d-b496-4f4a-8438-fea6d17705af)(content(Whitespace\" \
                      \"))))(Tile((id \
                      46bba948-a96e-48ce-ad18-0b412c8b4b01)(form(Tok x))(sort \
                      Pat)))(Secondary((id \
                      99db2964-faa6-4158-a48f-2db2b4aa5d35)(content(Whitespace\" \
                      \")))))))))(Secondary((id \
                      b139a293-f6dc-4074-a4cb-b7dc24bbe708)(content(Whitespace\" \
                      \"))))(Secondary((id \
                      11082931-6b3a-4c08-8978-b85b4446f6a4)(content(Whitespace\"\\n\"))))(Tile((id \
                      a3a5876b-fb06-4123-84cd-d423de06dd66)(form(Compound \
                      If))(shards(0 1 2))(children(((Secondary((id \
                      0fc256d4-f29d-447b-8c47-13946a9369b7)(content(Whitespace\" \
                      \"))))(Tile((id \
                      a60290d2-3eae-47d1-86ad-1f5097a8c43c)(form(Tok \
                      x))))(Secondary((id \
                      53780aef-96f0-4446-a4e5-2d3d701e8ad8)(content(Whitespace\" \
                      \"))))(Tile((id \
                      270a44aa-dfbb-4e71-950e-1fa8a673dbc1)(form(Compound \
                      Lt))))(Secondary((id \
                      1fc9b939-4559-4929-b458-db9881697d60)(content(Whitespace\" \
                      \"))))(Tile((id \
                      a8ba18e4-bd02-46f4-bdf3-3cfa8c6fd4de)(form(Tok \
                      1))))(Secondary((id \
                      e846b71c-bf22-4070-9d50-adc5cfbc2b01)(content(Whitespace\" \
                      \")))))((Secondary((id \
                      2052b2d8-4489-4c99-a343-e3c2823a12ba)(content(Whitespace\" \
                      \"))))(Tile((id \
                      a5152233-3a6e-45c8-8e9d-f77046343e6f)(form(Tok \
                      0))))(Secondary((id \
                      e3fb0c1c-6410-461e-a067-109c74203265)(content(Whitespace\" \
                      \"))))(Secondary((id \
                      24982656-d73d-4ef8-842e-f8baed76fe1f)(content(Whitespace\"\\n\")))))))))(Secondary((id \
                      a6dadc4c-ea30-45ba-a1e0-648c776f5c05)(content(Whitespace\" \
                      \"))))(Tile((id \
                      15e21fde-2ce9-4edd-b27e-871747dd499a)(form(Compound \
                      If))(shards(0 1 2))(children(((Secondary((id \
                      ebc4e435-a5c3-4844-a9e5-61ba16464e70)(content(Whitespace\" \
                      \"))))(Tile((id \
                      eef9cd5a-1495-41e0-a812-0aba65b36c2d)(form(Tok \
                      x))))(Secondary((id \
                      ecff3b15-e48b-47de-95af-55e26c82bfe4)(content(Whitespace\" \
                      \"))))(Tile((id \
                      6f6ad8fa-fea1-4cec-9ff4-684031b19d2f)(form(Compound \
                      Lt))))(Secondary((id \
                      eb093199-4c22-43af-8d9c-880534c3df47)(content(Whitespace\" \
                      \"))))(Tile((id \
                      3c7134a0-e371-4fbd-999f-8fea5e56a2d0)(form(Tok \
                      2))))(Secondary((id \
                      7c5accce-df31-4d69-bdda-e64cc9e36f41)(content(Whitespace\" \
                      \")))))((Secondary((id \
                      64a25bb6-a9cd-4f73-b0b5-1d41baa83577)(content(Whitespace\" \
                      \"))))(Tile((id \
                      016901dc-09c4-4e76-acde-6e974ba5d848)(form(Tok \
                      1))))(Secondary((id \
                      48fbea4a-2068-435a-94fe-5390bba297a3)(content(Whitespace\" \
                      \"))))(Secondary((id \
                      6cc3f813-67f5-4ccd-b4ba-200c223855bb)(content(Whitespace\"\\n\")))))))))(Secondary((id \
                      f685d555-d868-4657-813a-21cd1b4c7227)(content(Whitespace\" \
                      \"))))(Tile((id \
                      821e356a-dece-4ae3-bc47-b32dd8408b04)(form(Tok \
                      fib))))(Tile((id \
                      1cb4044b-c177-422f-bc52-9bb6ad90df6c)(form(Compound \
                      Ap))(shards(0 1))(children(((Tile((id \
                      8dec786c-e23d-4527-a1a5-cb127c55a4d0)(form(Tok \
                      x))))(Secondary((id \
                      589530d3-0320-4a64-ac42-1b2bab5192f7)(content(Whitespace\" \
                      \"))))(Tile((id \
                      f7890c6b-efec-44b5-a2b8-47a32d9fb6d8)(form(Compound \
                      Minus))))(Secondary((id \
                      653b91af-5087-4ecd-bfe6-3efe2b79d1c9)(content(Whitespace\" \
                      \"))))(Tile((id \
                      f3e2237b-efec-48aa-8e2a-0c9bdc133e8c)(form(Tok \
                      1)))))))))(Secondary((id \
                      0e282b0c-fa0f-4e35-865d-5a415f12f87e)(content(Whitespace\" \
                      \"))))(Tile((id \
                      406a8bb5-a626-4a14-a246-f3f3dcffe0a4)(form(Compound \
                      Plus))))(Secondary((id \
                      f46dbc34-4c7c-4733-86e4-c3564bc0fdec)(content(Whitespace\" \
                      \"))))(Tile((id \
                      3ba86b20-4c17-4b01-8854-2d2c8d6a9fb0)(form(Tok \
                      fib))))(Tile((id \
                      9118825a-7ad5-4d51-b5b4-5c5cf2c795d0)(form(Compound \
                      Ap))(shards(0 1))(children(((Tile((id \
                      dceb865e-f4d2-483d-a40b-f1c5054b751c)(form(Tok \
                      x))))(Secondary((id \
                      37a61e66-a216-41db-82ef-b8950cdf3f91)(content(Whitespace\" \
                      \"))))(Tile((id \
                      4a59b721-c9fa-4816-9d31-4b976497937a)(form(Compound \
                      Minus))))(Secondary((id \
                      b6120edc-fed8-4150-a16a-42939c342749)(content(Whitespace\" \
                      \"))))(Tile((id \
                      237b69ac-cf9b-420b-a476-cb445bc9a130)(form(Tok \
                      2)))))))))(Secondary((id \
                      d74db662-bea8-4798-951b-3929e52f78fe)(content(Whitespace\" \
                      \"))))(Secondary((id \
                      bd21eb38-ea7e-4f46-91de-5a50ebd98c26)(content(Whitespace\" \
                      \"))))(Secondary((id \
                      ddfd9130-8371-4a96-870a-44a75d19de86)(content(Whitespace\"\\n\")))))))))(Secondary((id \
                      3bc8a310-aa4c-4145-909e-7e708c760a97)(content(Whitespace\" \
                      \")))))((Grout((id \
                      df31a9f5-448f-4464-98aa-d575516b0216)(shape \
                      Convex))))))(ancestors())))(caret \
                      Outer)(refractors((manuals())(multis((ids())(suppressed())(ephemerals())))(sample_focus((call_stack())(index \
                      -1)(pinned_stack())(indicated_call())(time())(seq \
                      0)(step_range())(pending_focus())))(autoprobe_target())(pending_probe_cursor()))))";
                   backup_text =
                     "let fib: Int -> Int = \n\
                      fun x -> \n\
                      if x < 1 then 0 \n\
                      else if x < 2 then 1 \n\
                      else fib(x - 1) + fib(x - 2)  \n\
                      in ";
                 };
               hint = "incorrect base cases";
             };
             {
               impl =
                 {
                   zipper =
                     "((selection((focus Left)(content())(mode \
                      Normal)(anchor_caret Outer)(smart_rounded \
                      false)))(relatives((siblings(((Tile((id \
                      9e29609b-cbad-4c65-b304-8d91ffbfbce6)(form(Compound \
                      Let))(shards(0 1 2))(children(((Secondary((id \
                      daaaca43-5de9-4243-a769-21a6227e5227)(content(Whitespace\" \
                      \"))))(Tile((id \
                      016357bc-de85-4877-a33b-8c01dc3f03c7)(form(Tok \
                      fib))(sort Pat)))(Tile((id \
                      f1b27ad1-1f21-4166-98cc-29c5b7f9a837)(form(Compound \
                      TypeAsc))(sort Pat)))(Secondary((id \
                      838d2839-e1ef-47a7-b305-40a3543a893c)(content(Whitespace\" \
                      \"))))(Tile((id \
                      1f36db3f-004d-4ff1-8597-457030f29586)(form(Tok \
                      Int))(sort Typ)))(Secondary((id \
                      0461131e-7dbb-4df1-a093-35161e69e33a)(content(Whitespace\" \
                      \"))))(Tile((id \
                      151a52db-7e18-4644-937a-fd1b4d48a5e3)(form(Compound \
                      TypeArrow))(sort Typ)))(Secondary((id \
                      03b09f4b-418c-41ea-b864-d56b247269fb)(content(Whitespace\" \
                      \"))))(Tile((id \
                      d858c690-d420-4a44-9b05-58dfb165b58b)(form(Tok \
                      Int))(sort Typ)))(Secondary((id \
                      3d95838b-9e08-46c4-bd6d-bcb8d95b2da4)(content(Whitespace\" \
                      \")))))((Secondary((id \
                      07b7d517-c5a2-4498-aa61-df4b8af08d0c)(content(Whitespace\" \
                      \"))))(Secondary((id \
                      f528b48b-a9e8-46ee-87df-66a5d27100e6)(content(Whitespace\"\\n\"))))(Tile((id \
                      fa64d3d2-11d9-4020-bfce-e075f04dc87d)(form(Compound \
                      Fun))(shards(0 1))(children(((Secondary((id \
                      f3ec4914-f7dc-414b-8cad-92caaaabe6d8)(content(Whitespace\" \
                      \"))))(Tile((id \
                      077d5701-0f2f-4fde-8945-02551e55e452)(form(Tok x))(sort \
                      Pat)))(Secondary((id \
                      c3415376-92bc-4041-a9d7-a1b2023dcbfc)(content(Whitespace\" \
                      \")))))))))(Secondary((id \
                      6a93e37f-8080-4250-8850-67cb12d1ba07)(content(Whitespace\" \
                      \"))))(Secondary((id \
                      1ef0a9fe-c5dd-4b58-9d63-4d469b2aa9da)(content(Whitespace\"\\n\"))))(Tile((id \
                      e8f96df8-4cc1-43ce-a75c-fb9c8c3a8dfa)(form(Compound \
                      If))(shards(0 1 2))(children(((Secondary((id \
                      9ab8594d-1205-4a68-8f97-c75f7d550d3c)(content(Whitespace\" \
                      \"))))(Tile((id \
                      5175ca9d-a38e-48d9-ab9f-e6c9bcc2abb1)(form(Tok \
                      x))))(Secondary((id \
                      3a881741-fcef-4004-a097-1ea6047afd53)(content(Whitespace\" \
                      \"))))(Tile((id \
                      a7fcd7a4-10a6-4896-af66-7f2acf316eeb)(form(Compound \
                      Lt))))(Secondary((id \
                      1f179468-14d1-46e9-9e91-ead0dde281f4)(content(Whitespace\" \
                      \"))))(Tile((id \
                      83e1c102-c13b-4cec-9ac1-efd9ba4b6118)(form(Tok \
                      2))))(Secondary((id \
                      6665b2f9-1b24-49eb-b1ab-0307bdb00f65)(content(Whitespace\" \
                      \")))))((Secondary((id \
                      a3da4e68-efd6-4340-b8c1-3679f39b8f01)(content(Whitespace\" \
                      \"))))(Tile((id \
                      3a914443-433b-439e-a886-81cf366114d0)(form(Tok \
                      1))))(Secondary((id \
                      1303fe89-000f-401e-8c43-acc87fcf7314)(content(Whitespace\" \
                      \"))))(Secondary((id \
                      852f52d2-25f3-41d7-a896-6f01e5da6d99)(content(Whitespace\" \
                      \"))))(Secondary((id \
                      779edf97-aa97-4b58-a55a-aecf2db8c3b2)(content(Whitespace\"\\n\")))))))))(Secondary((id \
                      27956a33-c682-4ccb-9f83-b40a0c7e3663)(content(Whitespace\" \
                      \"))))(Tile((id \
                      ba5a6cb9-b64e-47dd-8322-e6d6ab732b19)(form(Tok \
                      fib))))(Tile((id \
                      33c9ee18-96d6-4866-959e-9bd2b2ff497e)(form(Compound \
                      Ap))(shards(0 1))(children(((Tile((id \
                      7bfc55a0-05db-40de-86ab-051f23ab6b52)(form(Tok \
                      x))))(Secondary((id \
                      8f83db2f-a806-4d17-9b49-847dcd99f427)(content(Whitespace\" \
                      \"))))(Tile((id \
                      d760311e-b6c9-4d31-be3d-303415cc1fd8)(form(Compound \
                      Minus))))(Secondary((id \
                      aa0995f3-3070-42f7-bcf3-3b92167d5d0d)(content(Whitespace\" \
                      \"))))(Tile((id \
                      a45122a0-7cf0-4046-a5c7-40c3522dc3bf)(form(Tok \
                      2)))))))))(Secondary((id \
                      96ee33e9-fa0a-4baa-8f1c-6044922854a2)(content(Whitespace\" \
                      \"))))(Tile((id \
                      94e2a16e-1aa3-446a-8944-f66617505cba)(form(Compound \
                      Plus))))(Secondary((id \
                      c7731f57-d854-4b5b-9026-fde86046997d)(content(Whitespace\" \
                      \"))))(Tile((id \
                      ea535432-5e60-4c2f-a5ff-49e3d67df116)(form(Tok \
                      fib))))(Tile((id \
                      cfa9e4ee-b468-43eb-a9d4-c7b598176cd4)(form(Compound \
                      Ap))(shards(0 1))(children(((Tile((id \
                      7fe9c2b5-e3ba-4f20-ab3d-e3589fc3a6ce)(form(Tok \
                      x))))(Secondary((id \
                      5c1f2439-811b-4aeb-abf8-d00e38dc9118)(content(Whitespace\" \
                      \"))))(Tile((id \
                      8fe4f8a2-fddf-4c48-8a2c-eef2ee1d667c)(form(Compound \
                      Minus))))(Secondary((id \
                      595702dd-0f4a-4f5b-8a85-ebc13d7447dd)(content(Whitespace\" \
                      \"))))(Tile((id \
                      93f93780-7914-441b-9d2c-3727d880f68e)(form(Tok \
                      2)))))))))(Secondary((id \
                      1211b062-45ba-44fd-82c1-3a8ca3286699)(content(Whitespace\" \
                      \"))))(Secondary((id \
                      7c30b777-a7ca-4bec-9a50-9bf849d99e63)(content(Whitespace\"\\n\")))))))))(Secondary((id \
                      35777fa2-52a2-46dd-b39e-4ab07077875e)(content(Whitespace\" \
                      \")))))((Grout((id \
                      4a85c9d5-d254-400f-bf23-0d2c85cdc9ee)(shape \
                      Convex))))))(ancestors())))(caret \
                      Outer)(refractors((manuals())(multis((ids())(suppressed())(ephemerals())))(sample_focus((call_stack())(index \
                      -1)(pinned_stack())(indicated_call())(time())(seq \
                      0)(step_range())(pending_focus())))(autoprobe_target())(pending_probe_cursor()))))";
                   backup_text =
                     "let fib: Int -> Int = \n\
                      fun x -> \n\
                      if x < 2 then 1  \n\
                      else fib(x - 2) + fib(x - 2) \n\
                      in ";
                 };
               hint = "incorrect recursion";
             };
           ];
         hidden_tests =
           {
             tests =
               {
                 zipper =
                   "((selection((focus Left)(content())(mode \
                    Normal)(anchor_caret Outer)(smart_rounded \
                    false)))(relatives((siblings(((Tile((id \
                    101088f7-1ddb-4604-99b3-3e05ce148785)(form(Compound \
                    Test))(shards(0 1))(children(((Secondary((id \
                    ba4c0b33-2ba5-46ab-b130-596a0c951c30)(content(Whitespace\" \
                    \"))))(Tile((id \
                    dd1fd548-95df-4c80-bd23-13bbe3841fcc)(form(Tok \
                    fib))))(Tile((id \
                    d0395d15-e384-4fc6-8fa6-04d38a4c6617)(form(Compound \
                    Ap))(shards(0 1))(children(((Tile((id \
                    ae0c8508-03a5-40b9-afb0-4875e12ef24d)(form(Tok \
                    0)))))))))(Secondary((id \
                    71826e11-dff1-4bc7-a8fb-f7efa1c39816)(content(Whitespace\" \
                    \"))))(Tile((id \
                    15d95ee4-7c5c-4da8-b7d6-0d1254e92d1d)(form(Compound \
                    Equals))))(Secondary((id \
                    6bee18a6-ff84-4f82-9cb6-7ffa71ef84dc)(content(Whitespace\" \
                    \"))))(Tile((id \
                    9a8772ae-acce-42e7-a1ac-a1f573d41497)(form(Tok \
                    1))))(Secondary((id \
                    e98fe4c9-9f1d-4ba2-ae43-40363e37194d)(content(Whitespace\" \
                    \")))))))))(Tile((id \
                    42bbeadf-7010-40df-9f60-8f6b74fa57ca)(form(Compound \
                    CellJoin))))(Secondary((id \
                    3542781f-ce29-49dd-ba13-e921d0131c60)(content(Whitespace\"\\n\"))))(Tile((id \
                    c1e8b434-06d0-4e65-91e2-41f372e7daae)(form(Compound \
                    Test))(shards(0 1))(children(((Secondary((id \
                    315691e2-7417-47d1-832d-6a66783194e2)(content(Whitespace\" \
                    \"))))(Tile((id \
                    4ac8ea89-eb30-4e8d-a248-03cb8e0c3066)(form(Tok \
                    fib))))(Tile((id \
                    40866965-24e0-4aed-8edc-c55a59be0b1a)(form(Compound \
                    Ap))(shards(0 1))(children(((Tile((id \
                    73beb688-76b3-4345-8bbe-85505c1d509d)(form(Tok \
                    1)))))))))(Secondary((id \
                    87ad9140-9cdf-427b-9f62-bcce1fe53aea)(content(Whitespace\" \
                    \"))))(Tile((id \
                    d7610927-c42a-4cc4-b211-f7312793571c)(form(Compound \
                    Equals))))(Secondary((id \
                    6d00450d-f54b-4a88-8652-46dc007bdf99)(content(Whitespace\" \
                    \"))))(Tile((id \
                    d96436c0-7b09-437e-a5b7-0673c8049601)(form(Tok \
                    1))))(Secondary((id \
                    39e00f32-42cf-459d-8fb4-f676448353b4)(content(Whitespace\" \
                    \")))))))))(Tile((id \
                    c2865bc8-b56e-4b70-97e3-c95d21ec32b3)(form(Compound \
                    CellJoin))))(Secondary((id \
                    0c1f17ac-dee1-460b-a4b3-086f18df74a9)(content(Whitespace\"\\n\"))))(Tile((id \
                    e11e2186-8e1f-47c2-acb8-f6445c77c3e0)(form(Compound \
                    Test))(shards(0 1))(children(((Secondary((id \
                    635bb97e-6fca-4dab-8ea8-1ce4a03c9df6)(content(Whitespace\" \
                    \"))))(Tile((id \
                    99a8eba5-decb-4532-893d-fe4dde01f4ec)(form(Tok \
                    fib))))(Tile((id \
                    49b28d41-73a3-4381-9029-d70d967ae368)(form(Compound \
                    Ap))(shards(0 1))(children(((Tile((id \
                    81b2e638-eac3-4d2f-94ca-aa51bed5b389)(form(Tok \
                    2)))))))))(Secondary((id \
                    cc2a10ea-d149-46b1-a78f-63675b97c8f7)(content(Whitespace\" \
                    \"))))(Tile((id \
                    762c03f1-16d0-4e6b-97c6-1f542ab7e81b)(form(Compound \
                    Equals))))(Secondary((id \
                    a5338871-fbd1-41a3-8b17-5bb639aa4b1c)(content(Whitespace\" \
                    \"))))(Tile((id \
                    55ebbdf3-8322-4815-b65f-d0c7db859487)(form(Tok \
                    2))))(Secondary((id \
                    21580a3a-ccba-410b-9efa-761de20273f4)(content(Whitespace\" \
                    \")))))))))(Tile((id \
                    3b8bd931-ef64-44b8-9a0c-54335f2da089)(form(Compound \
                    CellJoin))))(Secondary((id \
                    eb467468-2479-4ab1-a385-52007cd7157e)(content(Whitespace\"\\n\"))))(Tile((id \
                    e1f7a6b3-8640-4829-abb3-848273774a0f)(form(Compound \
                    Test))(shards(0 1))(children(((Secondary((id \
                    1ca48ba3-ef9f-4318-8288-da3bdb538a36)(content(Whitespace\" \
                    \"))))(Tile((id \
                    8ea3e905-283a-4aa2-bfaa-711073128d35)(form(Tok \
                    fib))))(Tile((id \
                    484d5dcf-c0d6-427d-bf87-86cc195ff56c)(form(Compound \
                    Ap))(shards(0 1))(children(((Tile((id \
                    3d34a7f5-0d76-44d2-a5a9-10494129e62e)(form(Tok \
                    3)))))))))(Secondary((id \
                    1bb04f3f-295f-44c7-8005-dc519735c6f8)(content(Whitespace\" \
                    \"))))(Tile((id \
                    08257bd1-97ee-4f72-83fc-630d50b42bab)(form(Compound \
                    Equals))))(Secondary((id \
                    d6f9d5fc-1bd1-413b-8906-1e4c86164b63)(content(Whitespace\" \
                    \"))))(Tile((id \
                    4fc309c2-9e89-47ac-9802-577d2f43cd15)(form(Tok \
                    3))))(Secondary((id \
                    d42e11d6-843c-4c74-8d01-33f47f2c6052)(content(Whitespace\" \
                    \")))))))))(Tile((id \
                    c56dc59f-f5f7-4152-b756-51d930b62469)(form(Compound \
                    CellJoin))))(Secondary((id \
                    162be25f-e24b-47cb-b8dc-4fc357e74b23)(content(Whitespace\"\\n\"))))(Tile((id \
                    40a8ae9f-56bb-4494-9df1-868b783cf5c9)(form(Compound \
                    Test))(shards(0 1))(children(((Secondary((id \
                    d9398ac3-e1b1-4087-8fdb-d554ee49fd11)(content(Whitespace\" \
                    \"))))(Tile((id \
                    e67e4eae-a433-45e5-bd04-94c4967e283d)(form(Tok \
                    fib))))(Tile((id \
                    195ed6ab-2d78-4483-bcb6-87456b910d84)(form(Compound \
                    Ap))(shards(0 1))(children(((Tile((id \
                    c88e8c5d-bc81-4d3c-b29e-c53a191cac9a)(form(Tok \
                    4)))))))))(Secondary((id \
                    f964a952-4dc5-4395-96ec-f743944f19df)(content(Whitespace\" \
                    \"))))(Tile((id \
                    44dc751b-233f-47a7-a648-4a53e06bca4b)(form(Compound \
                    Equals))))(Secondary((id \
                    00bb0a55-7363-49a9-8ee6-bd35f3c80680)(content(Whitespace\" \
                    \"))))(Tile((id \
                    5ab7f632-5913-41ba-972f-13b12518760c)(form(Tok \
                    5))))(Secondary((id \
                    a5b56cd7-52ca-4cff-aa3a-02419f4f60d3)(content(Whitespace\" \
                    \")))))))))(Tile((id \
                    02aa6cbc-8a24-460a-aeaf-f02a007b98e5)(form(Compound \
                    CellJoin))))(Secondary((id \
                    0e4a92cd-7581-43d4-a606-25247b2b29e8)(content(Whitespace\"\\n\"))))(Tile((id \
                    3680c440-9a96-4a8f-abe1-911bf0f6e01d)(form(Compound \
                    Test))(shards(0 1))(children(((Secondary((id \
                    3c0d56dc-4213-4461-bd67-2bb97bd8b74e)(content(Whitespace\" \
                    \"))))(Tile((id \
                    83c7ac5e-ee5b-4ab6-8f05-73697e6563dc)(form(Tok \
                    fib))))(Tile((id \
                    04fedb62-2130-4d8c-8f25-97236880390b)(form(Compound \
                    Ap))(shards(0 1))(children(((Tile((id \
                    00a842eb-1985-4dbc-b395-6f55055ac922)(form(Tok \
                    5)))))))))(Secondary((id \
                    cced232f-6c07-4235-a6bf-af58a0e14fd8)(content(Whitespace\" \
                    \"))))(Tile((id \
                    ce5cdb0c-e952-452b-8b53-e25c6da854e7)(form(Compound \
                    Equals))))(Secondary((id \
                    6615033f-5df6-4dfe-8b8d-1b35225818b2)(content(Whitespace\" \
                    \"))))(Tile((id \
                    18cd3098-576d-41bb-bab3-b32bd126fcd2)(form(Tok \
                    8))))(Secondary((id \
                    51420d71-ddc5-4532-a647-17fa5199c8ab)(content(Whitespace\" \
                    \")))))))))(Tile((id \
                    bd024e37-1921-47a2-baf8-1608dab5cc21)(form(Compound \
                    CellJoin))))(Secondary((id \
                    43b2261a-6ff4-42a1-ad30-9802590d4806)(content(Whitespace\"\\n\"))))(Tile((id \
                    fdc5c65c-a85f-4a06-99b0-9674abc50246)(form(Compound \
                    Test))(shards(0 1))(children(((Secondary((id \
                    daa07296-277e-442f-9fd4-f1dc6d178e3a)(content(Whitespace\" \
                    \"))))(Tile((id \
                    3924f5a5-a77b-4eed-b06c-2a4f2491d806)(form(Tok \
                    fib))))(Tile((id \
                    89f42226-4ea4-413e-b4e5-2ad6aaad3e66)(form(Compound \
                    Ap))(shards(0 1))(children(((Tile((id \
                    f6fcfad1-563d-47dd-938b-3c6cb8c6c02d)(form(Tok \
                    6)))))))))(Secondary((id \
                    e73b94bb-a35c-42f1-a3b8-15817b24c4d3)(content(Whitespace\" \
                    \"))))(Tile((id \
                    b1c69c86-872a-4128-89c7-cd19fe3b2064)(form(Compound \
                    Equals))))(Secondary((id \
                    c1e4dbc0-85fe-4aa2-ad3a-3c4beb6e8df3)(content(Whitespace\" \
                    \"))))(Tile((id \
                    0ca3381b-1cfd-4005-ae27-552576565950)(form(Tok \
                    13))))(Secondary((id \
                    7d9e2e6e-a256-4a71-877e-67deb68f5b25)(content(Whitespace\" \
                    \")))))))))(Tile((id \
                    da6c3d7e-1e3e-4a83-8e96-26d25c3ecda8)(form(Compound \
                    CellJoin))))(Secondary((id \
                    25041ac4-0f33-45d7-8123-021870228baf)(content(Whitespace\"\\n\"))))(Tile((id \
                    2a8027dc-de84-49c7-824e-f0f62ecf668f)(form(Compound \
                    Test))(shards(0 1))(children(((Secondary((id \
                    e84a82d7-2f30-41bd-97c1-2dd8886307b8)(content(Whitespace\" \
                    \"))))(Tile((id \
                    c0b97244-e66d-4666-85e9-f9f6106f8468)(form(Tok \
                    fib))))(Tile((id \
                    eb0a5410-0395-44dd-999c-be55cf27fee2)(form(Compound \
                    Ap))(shards(0 1))(children(((Tile((id \
                    f8ec395f-bfce-4e90-a14f-009c6ca0971d)(form(Tok \
                    7)))))))))(Secondary((id \
                    fe075ed2-29b0-4b04-a7c8-01914994caec)(content(Whitespace\" \
                    \"))))(Tile((id \
                    15784728-7834-4bdd-b139-b81b6936bce1)(form(Compound \
                    Equals))))(Secondary((id \
                    9fa36007-7957-4990-8a4b-21d0803244ac)(content(Whitespace\" \
                    \"))))(Tile((id \
                    94ce7674-cf28-4740-959d-2d16e79b55d4)(form(Tok \
                    21))))(Secondary((id \
                    9acd3acc-e292-4b5f-aca7-00b44a852800)(content(Whitespace\" \
                    \")))))))))(Tile((id \
                    c6598aae-5783-4881-900f-f33e3f38c257)(form(Compound \
                    CellJoin))))(Secondary((id \
                    5c0bf859-069c-4265-8579-ca24eacb777d)(content(Whitespace\"\\n\"))))(Tile((id \
                    21fe8390-e14e-46d8-8789-6777b5f32227)(form(Compound \
                    Test))(shards(0 1))(children(((Secondary((id \
                    b407d756-d94b-4023-a020-1ac3bd324c2c)(content(Whitespace\" \
                    \"))))(Tile((id \
                    0eabecbe-95a6-418b-a2be-54c447ee4786)(form(Tok \
                    fib))))(Tile((id \
                    5aaa8670-ec58-4ef8-8d23-6fc1f21fbeff)(form(Compound \
                    Ap))(shards(0 1))(children(((Tile((id \
                    b09a6711-ffc4-4a96-97bf-95f14408fc6e)(form(Tok \
                    8)))))))))(Secondary((id \
                    656c0b11-9e1f-44e3-b1fe-d1be6b0598c0)(content(Whitespace\" \
                    \"))))(Tile((id \
                    33975646-871d-486e-8f59-0731e15a097a)(form(Compound \
                    Equals))))(Secondary((id \
                    3dba5736-f582-4b77-b127-694105ee24c3)(content(Whitespace\" \
                    \"))))(Tile((id \
                    b35072ee-31ca-4e09-8ded-941aee8c36dd)(form(Tok \
                    34))))(Secondary((id \
                    1fe49176-c2a2-4a53-ad16-0e06d2246b33)(content(Whitespace\" \
                    \")))))))))(Tile((id \
                    9004a6ed-bae5-48f5-9da5-5a87a219ee9b)(form(Compound \
                    CellJoin))))(Secondary((id \
                    205536c5-f7ec-47d5-bcac-e11c1bd21d6e)(content(Whitespace\"\\n\")))))((Grout((id \
                    54990759-6182-441d-8acf-d41a18993b8e)(shape \
                    Convex))))))(ancestors())))(caret \
                    Outer)(refractors((manuals())(multis((ids())(suppressed())(ephemerals())))(sample_focus((call_stack())(index \
                    -1)(pinned_stack())(indicated_call())(time())(seq \
                    0)(step_range())(pending_focus())))(autoprobe_target())(pending_probe_cursor()))))";
                 backup_text =
                   "test fib(0) == 1 end;\n\
                    test fib(1) == 1 end;\n\
                    test fib(2) == 2 end;\n\
                    test fib(3) == 3 end;\n\
                    test fib(4) == 5 end;\n\
                    test fib(5) == 8 end;\n\
                    test fib(6) == 13 end;\n\
                    test fib(7) == 21 end;\n\
                    test fib(8) == 34 end;\n";
               };
             hints = [];
           };
         syntax_tests = [ ("fib is recursive", IsRecursive "fib") ];
       })
