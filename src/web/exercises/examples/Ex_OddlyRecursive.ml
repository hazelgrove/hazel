let exercise : Exercise.t =
  Code
    (CodeExercise.of_persistent
       {
         id = Haz3lcore.Id.v "3335e34d-d211-4332-91e2-815e9e183885";
         title = "Oddly Recursive";
         module_name = "Ex_OddlyRecursive";
         prompt =
           "Write a recursive function that determines whether the given \
            integer is odd. \n\n\
            `odd(n)` is equivalent to `true` iff `n` is odd.";
         point_distribution =
           { test_validation = 1; mutation_testing = 1; impl_grading = 2 };
         prelude =
           {
             zipper =
               "((selection((focus Left)(content())(mode Normal)(anchor_caret \
                Outer)(smart_rounded false)))(relatives((siblings(((Tile((id \
                4e71a6c3-906f-462a-9d76-e442603007da)(form(Compound \
                Let))(shards(0 1 2))(children(((Secondary((id \
                bcda3ca3-b9e8-43c1-ad6b-1290ad7fd5ec)(content(Whitespace\" \
                \"))))(Tile((id a742fd41-beff-42ad-a1e5-103fd843ddc6)(form(Tok \
                not))(sort Pat)))(Secondary((id \
                67ff754a-160d-4580-affe-a3795defd42d)(content(Whitespace\" \
                \"))))(Tile((id \
                37b9f37b-27cc-4dd5-b1d7-567827b23a69)(form(Compound \
                TypeAsc))(sort Pat)))(Secondary((id \
                2ddc32c3-9bb9-4e22-a1a8-a20f60df1a7a)(content(Whitespace\" \
                \"))))(Tile((id ac126c8c-b350-4541-b57d-046f62164b6c)(form(Tok \
                Bool))(sort Typ)))(Secondary((id \
                67eb2c7a-e1d6-4678-9394-2355020e7ed7)(content(Whitespace\" \
                \"))))(Tile((id \
                dd398d5a-f28d-4241-b484-de01dfcafbcf)(form(Compound \
                TypeArrow))(sort Typ)))(Secondary((id \
                90034f52-46ba-43b7-ba62-5fec7ab36dde)(content(Whitespace\" \
                \"))))(Tile((id 087be15c-bac6-44ed-adbd-8dd9fbd858a6)(form(Tok \
                Bool))(sort Typ)))(Secondary((id \
                e7813911-425b-4066-94fe-6ec7ef1ad5b8)(content(Whitespace\" \
                \")))))((Secondary((id \
                3f171d7d-fc92-41d1-ad59-3cc711c464d5)(content(Whitespace\"\\n\"))))(Tile((id \
                04e4e0f3-fceb-40fb-b1cf-611db36b9dc0)(form(Compound \
                Fun))(shards(0 1))(children(((Secondary((id \
                7fc2d108-321d-4cba-b8cd-c0cf7ce2ca10)(content(Whitespace\" \
                \"))))(Tile((id 09528d74-f07f-459c-a723-51627f89b3e8)(form(Tok \
                x))(sort Pat)))(Secondary((id \
                bcbfb85e-2a1f-4944-af55-551d4e8e848d)(content(Whitespace\" \
                \")))))))))(Secondary((id \
                4abff390-1cda-433a-a0c4-b888b9533286)(content(Whitespace\"\\n\"))))(Tile((id \
                185107ad-d06a-48b6-9300-ae8b1dae7be6)(form(Compound \
                If))(shards(0 1 2))(children(((Secondary((id \
                cad4144a-61c4-42a4-a3bd-739beb98d6be)(content(Whitespace\" \
                \"))))(Tile((id 48966d45-24a1-493a-9a7c-3e52edfe478d)(form(Tok \
                x))))(Secondary((id \
                896fe4f2-1ea8-4b06-ab99-e323d47da672)(content(Whitespace\" \
                \")))))((Secondary((id \
                8e3deccc-cbb1-4537-9974-207a5264c4a7)(content(Whitespace\" \
                \"))))(Tile((id 0b9a7af9-7a97-435b-866b-257f98642ab6)(form(Tok \
                false))))(Secondary((id \
                bfc02d18-3449-4452-bb2b-a1d842f72d7e)(content(Whitespace\" \
                \")))))))))(Secondary((id \
                a7f8543b-37a6-4d71-9ad2-d4de0fd2f914)(content(Whitespace\" \
                \"))))(Tile((id b9a7cdaa-516d-476b-b412-05b3131f4c7f)(form(Tok \
                true))))(Secondary((id \
                937f86ac-1b7a-4614-b68d-1bdc50de9b29)(content(Whitespace\" \
                \"))))(Secondary((id \
                e431488c-a699-447d-856c-2e40869d3bbf)(content(Whitespace\"\\n\")))))))))(Secondary((id \
                ddb3e489-c2df-4e67-a10f-7f13bc2a94a0)(content(Whitespace\" \
                \")))))((Grout((id eb5dbce3-88e3-494e-aa29-0d512045a946)(shape \
                Convex))))))(ancestors())))(caret \
                Outer)(refractors((manuals())(multis((ids())(suppressed())(ephemerals())))(sample_focus((call_stack())(index \
                -1)(pinned_stack())(indicated_call())(time())(seq \
                0)(step_range())(pending_focus())))(autoprobe_target())(pending_probe_cursor()))))";
             backup_text =
               "let not : Bool -> Bool =\n\
                fun x ->\n\
                if x then false else true \n\
                in ";
           };
         correct_impl =
           {
             zipper =
               "((selection((focus Left)(content())(mode Normal)(anchor_caret \
                Outer)(smart_rounded false)))(relatives((siblings(((Tile((id \
                a86e7e73-e4ab-4f7d-a946-9c149470f017)(form(Compound \
                Let))(shards(0 1 2))(children(((Secondary((id \
                d313759a-67e2-44c4-af81-4e371637c3d8)(content(Whitespace\" \
                \"))))(Tile((id 6241dd7c-e3a9-4794-91b7-e4f2cb71ade3)(form(Tok \
                odd))(sort Pat)))(Tile((id \
                5bec64e4-d9b4-40db-be21-78b0bfc6b704)(form(Compound \
                TypeAsc))(sort Pat)))(Tile((id \
                7a572212-61b7-46bd-a6bd-ddd811b0c45c)(form(Tok Int))(sort \
                Typ)))(Tile((id \
                817030b9-9f6f-4721-8511-270df0137f15)(form(Compound \
                TypeArrow))(sort Typ)))(Tile((id \
                f4424db1-5f01-4fc6-a382-a61dc83a3c29)(form(Tok Bool))(sort \
                Typ)))(Secondary((id \
                8c915808-7e3f-4575-a492-f772be800e54)(content(Whitespace\" \
                \")))))((Secondary((id \
                0b8deaf1-df7b-46a1-b047-a5851902a25b)(content(Whitespace\"\\n\"))))(Tile((id \
                a664609e-0c7a-4a64-99ef-9dae7a609297)(form(Compound \
                Fun))(shards(0 1))(children(((Secondary((id \
                ca161e2a-a4e9-4cce-883a-aeb788167cdc)(content(Whitespace\" \
                \"))))(Tile((id 5575edc5-ade8-4b3e-915f-06e618f2c406)(form(Tok \
                x))(sort Pat)))(Secondary((id \
                5307f49c-0488-4d95-b2e5-433067d50573)(content(Whitespace\" \
                \")))))))))(Secondary((id \
                3bb7ff8d-e3f7-490c-82fc-561fe3832b74)(content(Whitespace\"\\n\"))))(Tile((id \
                e39b4a92-6531-47e8-8362-5d87bbf56dad)(form(Compound \
                If))(shards(0 1 2))(children(((Secondary((id \
                6e47406b-4bbf-4f90-b42c-b9a625ee2072)(content(Whitespace\" \
                \"))))(Tile((id f4afb6bc-009e-48b5-92a5-a6715a2a202d)(form(Tok \
                x))))(Secondary((id \
                f22d808a-69c8-4af1-af87-5bccc4d3af60)(content(Whitespace\" \
                \"))))(Tile((id \
                234f4426-1e7b-4e1c-a9c2-0481fec87200)(form(Compound \
                Lt))))(Secondary((id \
                2a73dc1f-a816-4692-9411-825813f8d8a6)(content(Whitespace\" \
                \"))))(Tile((id d952b52a-369a-4b49-a37e-c63f0e392dbb)(form(Tok \
                0))))(Secondary((id \
                f460d81d-a447-4986-a613-f4db634e4deb)(content(Whitespace\" \
                \"))))(Secondary((id \
                936de00d-3a5b-423a-a8cb-8562a0c7ab11)(content(Whitespace\"\\n\")))))((Secondary((id \
                0cbff51d-a1ab-4e0e-8b53-283b029d7a9b)(content(Whitespace\" \
                \"))))(Tile((id 3382edeb-6835-4c96-8ef2-12d559b821e9)(form(Tok \
                odd))))(Tile((id \
                e9c3a5e7-4795-4973-839d-4c0c4e033c3a)(form(Compound \
                Ap))(shards(0 1))(children(((Tile((id \
                7f3f8e49-2571-40b6-93d5-5117054c8b8b)(form(Compound \
                UnaryMinus))))(Tile((id \
                75faa842-247a-49e8-ad40-5356c4200d24)(form(Tok \
                x)))))))))(Secondary((id \
                56a1c604-1592-483d-9c59-f95821753f82)(content(Whitespace\" \
                \"))))(Secondary((id \
                757c8ef0-d3dd-474c-8748-5572131f68f0)(content(Whitespace\"\\n\")))))))))(Secondary((id \
                88d6d2b1-d726-4132-8c79-7117b5e8ed65)(content(Whitespace\" \
                \"))))(Tile((id \
                e71ed853-b75c-471d-82bb-6afb936c106b)(form(Compound \
                If))(shards(0 1 2))(children(((Secondary((id \
                c14c3dee-208a-4772-9e21-c3d9055f93f4)(content(Whitespace\" \
                \"))))(Tile((id 8a8e4749-01df-4964-99e4-2831674af010)(form(Tok \
                x))))(Secondary((id \
                76b9e710-3f0f-45f7-be27-1e9c6477411f)(content(Whitespace\" \
                \"))))(Tile((id \
                185ed72e-35bc-4e3b-8e86-ba5cb5769c13)(form(Compound \
                Equals))))(Secondary((id \
                49eab495-5524-499c-adad-40b61e520356)(content(Whitespace\" \
                \"))))(Tile((id 46ae3923-f239-46a8-ad00-ac2ebfa2e7c6)(form(Tok \
                0))))(Secondary((id \
                fdaf95b2-7328-4490-898c-1d4be1c494c8)(content(Whitespace\" \
                \")))))((Secondary((id \
                2cc69692-3d52-4bea-b572-aad2d905b526)(content(Whitespace\" \
                \"))))(Tile((id d5e763aa-dd6c-4405-aa17-512ce11f4973)(form(Tok \
                false))))(Secondary((id \
                51962bc1-8e26-4c92-bf3d-675cfcd6108a)(content(Whitespace\" \
                \"))))(Secondary((id \
                ba30d58b-f236-4bb0-9f4d-eede08e5db5c)(content(Whitespace\"\\n\")))))))))(Secondary((id \
                09655095-aed8-4357-ab62-9c95836f67b5)(content(Whitespace\" \
                \"))))(Tile((id e97b86a7-ed8d-4369-8cde-097e03e0397b)(form(Tok \
                not))))(Tile((id \
                34267258-8ac3-46fa-990d-d0d20b0111d7)(form(Compound \
                Ap))(shards(0 1))(children(((Tile((id \
                2690d83c-4c69-4a93-9c68-ffd4fa02f9c4)(form(Tok \
                odd))))(Tile((id \
                0ffcdb0d-c432-4d7b-bc6c-73605a8c1666)(form(Compound \
                Ap))(shards(0 1))(children(((Tile((id \
                bfddac43-1a1d-4b7d-b7dd-e7ce4d69ae0a)(form(Tok x))))(Tile((id \
                4f676288-d544-44cf-8bea-dc825742d7de)(form(Compound \
                Minus))))(Tile((id \
                b2a10cde-e2bc-4ff0-bf7b-91e634b4b04e)(form(Tok \
                1))))))))))))))(Secondary((id \
                ee2028ce-9634-42b8-b88b-1ec7e1b00a71)(content(Whitespace\" \
                \"))))(Secondary((id \
                74f410c5-b8ef-40b9-a28c-471e7103cf46)(content(Whitespace\"\\n\")))))))))(Secondary((id \
                1d00e963-0561-4bd0-a5a4-7a6fda480c58)(content(Whitespace\" \
                \")))))((Grout((id 84e0eb4d-d8da-43b7-966f-643bac9fe1be)(shape \
                Convex))))))(ancestors())))(caret \
                Outer)(refractors((manuals())(multis((ids())(suppressed())(ephemerals())))(sample_focus((call_stack())(index \
                -1)(pinned_stack())(indicated_call())(time())(seq \
                0)(step_range())(pending_focus())))(autoprobe_target())(pending_probe_cursor()))))";
             backup_text =
               "let odd:Int->Bool =\n\
                fun x ->\n\
                if x < 0 \n\
                then odd(-x) \n\
                else if x == 0 then false \n\
                else not(odd(x-1)) \n\
                in ";
           };
         your_tests =
           {
             tests =
               {
                 zipper =
                   "((selection((focus Left)(content())(mode \
                    Normal)(anchor_caret Outer)(smart_rounded \
                    false)))(relatives((siblings(((Tile((id \
                    4142c3d1-f629-4d74-908e-fd8402732989)(form(Compound \
                    Test))(shards(0 1))(children(((Secondary((id \
                    4caf0c88-55f6-457e-8218-4d52aa9fe6b5)(content(Whitespace\" \
                    \"))))(Tile((id \
                    4d163702-e1e2-4b3c-bbb6-cbec28cac381)(form(Tok \
                    not))))(Tile((id \
                    c337414c-111a-4548-a2ed-b34877b143c7)(form(Compound \
                    Ap))(shards(0 1))(children(((Tile((id \
                    0a28e966-bf13-4640-a8e8-74a40ebe42ab)(form(Tok \
                    false)))))))))(Secondary((id \
                    fea67cf3-4799-4c51-8b3f-aee1c0c843b8)(content(Whitespace\" \
                    \")))))))))(Tile((id \
                    556c750e-be46-444e-876a-094d54a5676a)(form(Compound \
                    CellJoin))))(Secondary((id \
                    3270cf91-eb04-435e-b4c0-d4b6e3d50a87)(content(Whitespace\"\\n\"))))(Tile((id \
                    7f7483ea-d0bc-463b-97ee-882798d740cb)(form(Compound \
                    Test))(shards(0 1))(children(((Secondary((id \
                    237517ea-a5ab-4fbd-835f-5ea2f48d32f6)(content(Whitespace\" \
                    \"))))(Tile((id \
                    b97ff10a-f74f-45a2-b582-377d2d0d667e)(form(Tok \
                    not))))(Tile((id \
                    68fe5aaa-2900-4203-9ac6-3f782def6fae)(form(Compound \
                    Ap))(shards(0 1))(children(((Tile((id \
                    a2727d0a-c88a-43e9-afd3-23d6f698bcd2)(form(Tok \
                    not))))(Tile((id \
                    017c212f-92c3-43fa-8050-618df1304a52)(form(Compound \
                    Ap))(shards(0 1))(children(((Tile((id \
                    8ee2e1cd-c093-4b3b-9ead-9d4eb8c26326)(form(Tok \
                    true))))))))))))))(Secondary((id \
                    cf632069-7848-4a16-a930-87f2071c06d1)(content(Whitespace\" \
                    \")))))))))(Tile((id \
                    04849dcf-fe29-49ed-a7fe-a7903f74564c)(form(Compound \
                    CellJoin))))(Secondary((id \
                    cd70e0d0-5ca3-4c20-8a10-ff089ff15ee7)(content(Whitespace\" \
                    \"))))(Secondary((id \
                    99e9c2cd-80ca-4a1e-8a2b-6c8d3a529c1a)(content(Whitespace\"\\n\")))))((Grout((id \
                    3b710475-5b43-4e13-b7f3-323880c553ac)(shape \
                    Convex))))))(ancestors())))(caret \
                    Outer)(refractors((manuals())(multis((ids())(suppressed())(ephemerals())))(sample_focus((call_stack())(index \
                    -1)(pinned_stack())(indicated_call())(time())(seq \
                    0)(step_range())(pending_focus())))(autoprobe_target())(pending_probe_cursor()))))";
                 backup_text =
                   "test not(false) end;\ntest not(not(true)) end; \n";
               };
             required = 6;
             provided = 2;
           };
         your_impl =
           {
             zipper =
               "((selection((focus Left)(content())(mode Normal)(anchor_caret \
                Outer)(smart_rounded false)))(relatives((siblings(((Tile((id \
                f692a50f-d402-4550-9477-430fddd42972)(form(Compound \
                Let))(shards(0 1 2))(children(((Secondary((id \
                cedae1c1-0b36-44d0-ab74-477a6a81e56b)(content(Whitespace\" \
                \"))))(Tile((id 36cac850-a8ed-4d3f-8b3c-3673879fdcf9)(form(Tok \
                odd))(sort Pat)))(Tile((id \
                0bc4e878-4677-47a7-9146-2aa606906653)(form(Compound \
                TypeAsc))(sort Pat)))(Secondary((id \
                0053bfb0-38f3-4740-ac6e-9ff70aa17c6d)(content(Whitespace\" \
                \"))))(Tile((id 5436c6d8-a494-43bd-83dc-843f09a21523)(form(Tok \
                Int))(sort Typ)))(Secondary((id \
                c02af651-a285-40ca-86c2-fb4967a01c91)(content(Whitespace\" \
                \"))))(Tile((id \
                b15ff597-7c12-4a8b-9c5b-21327fa8ff30)(form(Compound \
                TypeArrow))(sort Typ)))(Secondary((id \
                ba3e9475-a249-475b-af77-548246496f9e)(content(Whitespace\" \
                \"))))(Tile((id 45d03add-bb84-48f6-8f91-e1ce1bb1ebcd)(form(Tok \
                Bool))(sort Typ)))(Secondary((id \
                2b2d2f28-9555-4697-98dd-7f3d5757de3a)(content(Whitespace\" \
                \")))))((Secondary((id \
                ae8f5091-ff79-4a63-ab39-ff6ec1179613)(content(Whitespace\"\\n\"))))(Tile((id \
                9d51d9e0-77c3-4cfe-9267-478c9e2e7a49)(form(Compound \
                Fun))(shards(0 1))(children(((Secondary((id \
                7dee8688-6976-4e45-9eb7-0cd5637842af)(content(Whitespace\" \
                \"))))(Tile((id 239612eb-9992-4a62-a3b5-9764252923c6)(form(Tok \
                n))(sort Pat)))(Secondary((id \
                da2a7be3-1a40-4bc7-9de5-f3859e03b495)(content(Whitespace\" \
                \")))))))))(Grout((id \
                4ab2939e-2d54-4728-bb7b-1fbf3b6192e8)(shape \
                Convex)))(Secondary((id \
                27bbaacf-8885-4eb0-bc80-5bbe8a7ca50e)(content(Whitespace\" \
                \"))))(Secondary((id \
                681a6e65-2617-4514-be47-fa05dbe21603)(content(Whitespace\" \
                \"))))(Secondary((id \
                d31c9ca7-19b3-498b-a852-f7c8165bbe94)(content(Whitespace\"\\n\")))))))))(Secondary((id \
                33139dcf-83e0-47f9-97d9-9bf090573326)(content(Whitespace\" \
                \")))))((Grout((id 2f69627f-253b-4d80-af1b-d2321abf0fbe)(shape \
                Convex))))))(ancestors())))(caret \
                Outer)(refractors((manuals())(multis((ids())(suppressed())(ephemerals())))(sample_focus((call_stack())(index \
                -1)(pinned_stack())(indicated_call())(time())(seq \
                0)(step_range())(pending_focus())))(autoprobe_target())(pending_probe_cursor()))))";
             backup_text = "let odd: Int -> Bool =\nfun n ->  \nin ";
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
                      aca07a51-c336-4e13-bb78-99164fdfa893)(form(Compound \
                      Let))(shards(0 1 2))(children(((Secondary((id \
                      fc2a9da1-7635-46b1-8529-53e197656ae1)(content(Whitespace\" \
                      \"))))(Tile((id \
                      f911cbda-34f5-4059-a138-9a200cbc3beb)(form(Tok \
                      odd))(sort Pat)))(Tile((id \
                      5734e44b-81ff-4951-8171-7df05f461a2b)(form(Compound \
                      TypeAsc))(sort Pat)))(Secondary((id \
                      9c23c66f-7d2f-4489-8727-4606670c6aed)(content(Whitespace\" \
                      \"))))(Tile((id \
                      334ec537-efb1-4e95-9962-2f766f543fa4)(form(Tok \
                      Int))(sort Typ)))(Secondary((id \
                      8adac1e3-815d-40c4-9754-820891f86797)(content(Whitespace\" \
                      \"))))(Tile((id \
                      5600048c-34af-46f3-93a3-8c4bf9d60c57)(form(Compound \
                      TypeArrow))(sort Typ)))(Secondary((id \
                      ab577800-b64a-46d6-85e2-73bad9035a0a)(content(Whitespace\" \
                      \"))))(Tile((id \
                      3315360a-dd95-4238-a6ed-0e48efb6bc56)(form(Tok \
                      Bool))(sort Typ)))(Secondary((id \
                      1fc06934-5bca-4dbf-93f5-bd15232e3f46)(content(Whitespace\" \
                      \")))))((Secondary((id \
                      d1e9c48e-da40-4abd-965c-eb219c8d3a91)(content(Whitespace\"\\n\"))))(Tile((id \
                      fd7bacf7-55a2-4084-aedf-e6cdbd892b30)(form(Compound \
                      Fun))(shards(0 1))(children(((Secondary((id \
                      7d8a6e39-5bad-446d-9b11-48b813df94f3)(content(Whitespace\" \
                      \"))))(Tile((id \
                      4d6dfeb0-4190-4d68-9701-c5e7cb9984b9)(form(Tok x))(sort \
                      Pat)))(Secondary((id \
                      b1b989dd-6eb3-49ed-8492-0190a7022879)(content(Whitespace\" \
                      \")))))))))(Secondary((id \
                      47b23d45-f3a4-4a94-a900-d6a702fa99dd)(content(Whitespace\" \
                      \"))))(Tile((id \
                      31a0a631-795c-4abe-9592-7de891bffcae)(form(Tok \
                      false))))(Secondary((id \
                      107972d2-9a4b-456a-b949-01e6a7ef3ea9)(content(Whitespace\" \
                      \"))))(Secondary((id \
                      b1c7ef1b-a3a4-42cc-8209-e00db4e22106)(content(Whitespace\"\\n\")))))))))(Secondary((id \
                      1f47b6a1-b371-4100-a00d-b34da2f1e3ab)(content(Whitespace\" \
                      \")))))((Grout((id \
                      1c84de0f-5625-4704-9b2f-d0b97e6a9bd3)(shape \
                      Convex))))))(ancestors())))(caret \
                      Outer)(refractors((manuals())(multis((ids())(suppressed())(ephemerals())))(sample_focus((call_stack())(index \
                      -1)(pinned_stack())(indicated_call())(time())(seq \
                      0)(step_range())(pending_focus())))(autoprobe_target())(pending_probe_cursor()))))";
                   backup_text = "let odd: Int -> Bool =\nfun x -> false \nin ";
                 };
               hint = "always returns false";
             };
             {
               impl =
                 {
                   zipper =
                     "((selection((focus Left)(content())(mode \
                      Normal)(anchor_caret Outer)(smart_rounded \
                      false)))(relatives((siblings(((Tile((id \
                      ebf66f23-ef74-49cd-ba8e-aab36c43b1e5)(form(Compound \
                      Let))(shards(0 1 2))(children(((Secondary((id \
                      34f02df9-2c0c-4614-9f14-7b68a894a3d3)(content(Whitespace\" \
                      \"))))(Tile((id \
                      218a0bf0-563d-4bda-bd2f-54018b57001f)(form(Tok \
                      odd))(sort Pat)))(Tile((id \
                      f6f92059-ff14-41cb-bdd5-93f64fe70173)(form(Compound \
                      TypeAsc))(sort Pat)))(Secondary((id \
                      cbac98d6-39b6-45eb-b504-8e85b942494b)(content(Whitespace\" \
                      \"))))(Tile((id \
                      6b5713b0-fe24-4e4d-9095-0d9ec52c1867)(form(Tok \
                      Int))(sort Typ)))(Secondary((id \
                      a14a52e7-ef62-4082-9b56-9a4c7ed1e5aa)(content(Whitespace\" \
                      \"))))(Tile((id \
                      f64e42d9-5ced-4e62-9b5b-b0e3e5bf8e70)(form(Compound \
                      TypeArrow))(sort Typ)))(Secondary((id \
                      72eead6c-6713-4ec1-83da-6732abe17007)(content(Whitespace\" \
                      \"))))(Tile((id \
                      73179e4f-d761-4148-b4fa-5f2826dd586c)(form(Tok \
                      Bool))(sort Typ)))(Secondary((id \
                      b649f062-403d-4352-ad48-0cb717f2c256)(content(Whitespace\" \
                      \")))))((Secondary((id \
                      bac8c6e6-f6ce-49f7-9b0f-ae4eb8b97b18)(content(Whitespace\"\\n\"))))(Tile((id \
                      e7066c2f-8b57-4f66-b39f-7dba3429028f)(form(Compound \
                      Fun))(shards(0 1))(children(((Secondary((id \
                      9c2b439c-391d-4672-8911-f364f6541ed1)(content(Whitespace\" \
                      \"))))(Tile((id \
                      a974808c-7ff6-4824-82cc-c514d5e90d6a)(form(Tok x))(sort \
                      Pat)))(Secondary((id \
                      759de0a5-0641-4341-aff7-697704862735)(content(Whitespace\" \
                      \")))))))))(Secondary((id \
                      906144b9-e590-4d3c-85c1-01b5191f9efa)(content(Whitespace\" \
                      \"))))(Tile((id \
                      f172fa21-3f22-4d58-b881-5bf7318de608)(form(Tok \
                      true))))(Secondary((id \
                      ea0fa47c-42fa-4c37-ae73-8cecea2e49b5)(content(Whitespace\" \
                      \"))))(Secondary((id \
                      6dbcee0b-7c32-486d-a83c-049334295cf1)(content(Whitespace\"\\n\")))))))))(Secondary((id \
                      66394188-7d90-4927-b2aa-fbc9637ebd90)(content(Whitespace\" \
                      \")))))((Grout((id \
                      0b3175f5-9638-44ba-9d58-1b311dfa7264)(shape \
                      Convex))))))(ancestors())))(caret \
                      Outer)(refractors((manuals())(multis((ids())(suppressed())(ephemerals())))(sample_focus((call_stack())(index \
                      -1)(pinned_stack())(indicated_call())(time())(seq \
                      0)(step_range())(pending_focus())))(autoprobe_target())(pending_probe_cursor()))))";
                   backup_text = "let odd: Int -> Bool =\nfun x -> true \nin ";
                 };
               hint = "always returns true";
             };
             {
               impl =
                 {
                   zipper =
                     "((selection((focus Left)(content())(mode \
                      Normal)(anchor_caret Outer)(smart_rounded \
                      false)))(relatives((siblings(((Tile((id \
                      7fda51b1-700f-464c-a38b-8042b96efcdd)(form(Compound \
                      Let))(shards(0 1 2))(children(((Secondary((id \
                      c8437893-bce9-47c7-81c5-872c89261396)(content(Whitespace\" \
                      \"))))(Tile((id \
                      3a930583-02e2-4a3e-9de4-8cd29aadfbd4)(form(Tok \
                      odd))(sort Pat)))(Tile((id \
                      aa40f235-ef2c-4424-b535-add8c535a5bf)(form(Compound \
                      TypeAsc))(sort Pat)))(Secondary((id \
                      7dd3cc18-d75d-449d-a25a-cd53ce2e5207)(content(Whitespace\" \
                      \"))))(Tile((id \
                      4114018a-b4ca-42f0-b6dd-af84363f55a3)(form(Tok \
                      Int))(sort Typ)))(Secondary((id \
                      82a7489b-b400-48ac-b663-d8cb6583d2b5)(content(Whitespace\" \
                      \"))))(Tile((id \
                      793aa2fd-431c-484f-954b-9bd8d7141d39)(form(Compound \
                      TypeArrow))(sort Typ)))(Secondary((id \
                      83d499f3-45e3-4c87-9059-938631c24e9f)(content(Whitespace\" \
                      \"))))(Tile((id \
                      bc7f1fcc-1cd4-4f90-92de-5f3306c4b8cd)(form(Tok \
                      Bool))(sort Typ)))(Secondary((id \
                      f1e0869a-6df4-4ff7-98c1-3c532d14ebf3)(content(Whitespace\" \
                      \")))))((Secondary((id \
                      6923d685-cfd5-4fb4-a9b9-721b028d0196)(content(Whitespace\"\\n\"))))(Tile((id \
                      b58f1827-f56e-4d88-85cb-078aee96b5ba)(form(Compound \
                      Fun))(shards(0 1))(children(((Secondary((id \
                      028cc77c-1e46-4297-858f-dce75b0d9b50)(content(Whitespace\" \
                      \"))))(Tile((id \
                      b0ee873b-35ec-4967-9a05-788b332512cf)(form(Tok x))(sort \
                      Pat)))(Secondary((id \
                      fe93eef1-86cf-4cf1-8c23-a1b438c4290a)(content(Whitespace\" \
                      \")))))))))(Secondary((id \
                      a0499fdd-3894-4083-a167-57226047feef)(content(Whitespace\" \
                      \"))))(Tile((id \
                      96454d42-861c-4eb8-ac70-a94b9b7d6a20)(form(Compound \
                      If))(shards(0 1 2))(children(((Secondary((id \
                      6e1ef49b-a816-4504-a4d3-c5fea23b7b03)(content(Whitespace\" \
                      \"))))(Tile((id \
                      9e8b4685-74b9-4957-afe5-107c7715a0c1)(form(Tok \
                      x))))(Secondary((id \
                      c5be81be-f7f2-4070-94f7-0e230f05e94b)(content(Whitespace\" \
                      \"))))(Tile((id \
                      e13c8f57-20f1-4491-89a9-f8b49ef0022a)(form(Compound \
                      Lt))))(Secondary((id \
                      ca1cfeac-e735-4c05-9976-21f697e532d0)(content(Whitespace\" \
                      \"))))(Tile((id \
                      427b2bfa-8132-43b0-b007-f432205f5320)(form(Tok \
                      0))))(Secondary((id \
                      5de7ce5c-0498-4bdc-b9c8-14da39cbe69a)(content(Whitespace\" \
                      \")))))((Secondary((id \
                      15798c75-8364-4a0a-a870-5ce13310f40c)(content(Whitespace\" \
                      \"))))(Tile((id \
                      307e671a-27d0-4794-bc89-ee16b37af9ae)(form(Tok \
                      odd))))(Tile((id \
                      c68be975-8cfa-4ef9-ab1d-892546727877)(form(Compound \
                      Ap))(shards(0 1))(children(((Tile((id \
                      d015689e-c9f3-44f7-a358-9b896e5c5ecf)(form(Compound \
                      UnaryMinus))))(Tile((id \
                      b2e3b725-2148-4daf-81d2-bcea2272fbda)(form(Tok \
                      x)))))))))(Secondary((id \
                      039be315-9a0c-404d-8a07-dee2b73018ed)(content(Whitespace\" \
                      \"))))(Secondary((id \
                      fb4ada72-e245-4d02-bce5-dead304afb0b)(content(Whitespace\"\\n\")))))))))(Secondary((id \
                      3dd9d919-8c72-4224-bf61-b2f615781b39)(content(Whitespace\" \
                      \"))))(Tile((id \
                      b20988c9-1bf5-4387-929b-deb3a28b55eb)(form(Compound \
                      If))(shards(0 1 2))(children(((Secondary((id \
                      bcbffb2f-1905-4562-bfc3-58149c5aca41)(content(Whitespace\" \
                      \"))))(Tile((id \
                      5541847d-74b5-4283-aa09-1064b9f17a70)(form(Tok \
                      x))))(Secondary((id \
                      de8b9c8d-505f-48ce-835a-82ae408a8481)(content(Whitespace\" \
                      \"))))(Tile((id \
                      8061fb5c-d857-4f9d-97ba-e5ef844ef562)(form(Compound \
                      Equals))))(Secondary((id \
                      3113259b-da2f-4a4e-b1a2-e10323deece6)(content(Whitespace\" \
                      \"))))(Tile((id \
                      ac4962e6-529e-4254-a5f0-76b474b937f8)(form(Tok \
                      0))))(Secondary((id \
                      d2f28863-cbc0-459b-b8fe-ccb1d5878dfa)(content(Whitespace\" \
                      \")))))((Secondary((id \
                      2dcfb507-25a9-462a-b32c-2217ab222360)(content(Whitespace\" \
                      \"))))(Tile((id \
                      df9ea67e-7c7d-4b52-8cb4-7df4c17ec3f3)(form(Tok \
                      true))))(Secondary((id \
                      fba30df5-e3e7-4369-8d52-c177e5fa299a)(content(Whitespace\" \
                      \"))))(Secondary((id \
                      4c6d60c2-7572-41fa-bc69-445d8e809b03)(content(Whitespace\"\\n\")))))))))(Secondary((id \
                      01b37045-3543-4f16-b982-41e5cbb1ac85)(content(Whitespace\" \
                      \"))))(Tile((id \
                      12fb648a-dc32-46ac-b14d-88a5f13b49fd)(form(Compound \
                      If))(shards(0 1 2))(children(((Secondary((id \
                      d25b2cbc-ae1a-479e-89fd-11db1ba20da2)(content(Whitespace\" \
                      \"))))(Tile((id \
                      fabec551-90f8-426d-af39-1a84b0afb700)(form(Tok \
                      x))))(Secondary((id \
                      5b5fc667-6dae-4715-8ae7-dfdd2f067c0c)(content(Whitespace\" \
                      \"))))(Tile((id \
                      f68605c8-4e0a-471f-a259-9c12a352783c)(form(Compound \
                      Equals))))(Secondary((id \
                      f93c31e9-bce9-407f-a04e-8bbb606f18de)(content(Whitespace\" \
                      \"))))(Tile((id \
                      386b4ff6-ef93-4737-9a15-858e9be9915d)(form(Tok \
                      1))))(Secondary((id \
                      a5554a43-4e87-469b-865a-b32f8e020828)(content(Whitespace\" \
                      \")))))((Secondary((id \
                      d74f6077-b54a-4cba-9cab-fdac2cfbaa4c)(content(Whitespace\" \
                      \"))))(Tile((id \
                      1b9769e4-ad1e-48da-94dc-1d7023d4f08d)(form(Tok \
                      true))))(Secondary((id \
                      dc60cfc8-320e-428b-a889-29449502bda4)(content(Whitespace\" \
                      \"))))(Secondary((id \
                      e2ccbaf4-3260-45aa-bd14-3cfef75af0c6)(content(Whitespace\"\\n\")))))))))(Secondary((id \
                      de0a5aa3-658f-4925-bd33-a9f843366bab)(content(Whitespace\" \
                      \"))))(Tile((id \
                      ccaa5a2e-2e2d-4d48-a9c2-61d3c5b54fb0)(form(Tok \
                      odd))))(Tile((id \
                      a2a7692c-ed99-426c-9d61-eaf15d8f9309)(form(Compound \
                      Ap))(shards(0 1))(children(((Tile((id \
                      44398f6b-abdb-43d6-8cab-eeb756033b93)(form(Tok \
                      x))))(Secondary((id \
                      dc042968-f834-495a-a157-9b136d64b319)(content(Whitespace\" \
                      \"))))(Tile((id \
                      a0647ff5-54ed-4b46-bc75-661eda1e603a)(form(Compound \
                      Minus))))(Secondary((id \
                      7d0667e3-53fb-47b9-9af0-3ed34c94385e)(content(Whitespace\" \
                      \"))))(Tile((id \
                      fb20739a-5acd-4c6c-b27f-ab486c8e3eb3)(form(Tok \
                      1)))))))))(Secondary((id \
                      65a5a846-d9d3-4078-a399-c8df2b6b013d)(content(Whitespace\" \
                      \")))))))))(Secondary((id \
                      68fc234f-0497-4d94-9d78-be550e4c7490)(content(Whitespace\"\\n\")))))((Grout((id \
                      561709df-4ca9-4cd7-b7cd-915dab6bd3ec)(shape \
                      Convex))))))(ancestors())))(caret \
                      Outer)(refractors((manuals())(multis((ids())(suppressed())(ephemerals())))(sample_focus((call_stack())(index \
                      -1)(pinned_stack())(indicated_call())(time())(seq \
                      0)(step_range())(pending_focus())))(autoprobe_target())(pending_probe_cursor()))))";
                   backup_text =
                     "let odd: Int -> Bool =\n\
                      fun x -> if x < 0 then odd(-x) \n\
                      else if x == 0 then true \n\
                      else if x == 1 then true \n\
                      else odd(x - 1) in\n";
                 };
               hint = "incorrect base case";
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
                    a3ced276-6158-458b-b685-15f6411e18ec)(form(Compound \
                    Test))(shards(0 1))(children(((Secondary((id \
                    0ce763eb-34a4-4338-af4a-a297c227c6e0)(content(Whitespace\" \
                    \"))))(Tile((id \
                    09372ffd-fada-4d77-9994-61ec7f6efe7b)(form(Tok \
                    not))))(Tile((id \
                    4916fc39-5051-43d4-83fc-b0a31eebfc38)(form(Compound \
                    Ap))(shards(0 1))(children(((Tile((id \
                    8fffb7be-ef30-43b6-9852-1977cfccf73c)(form(Tok \
                    odd))))(Tile((id \
                    1766cb8e-8a78-47f2-80e2-18cf0b1ecd18)(form(Compound \
                    Ap))(shards(0 1))(children(((Tile((id \
                    44242273-3f0d-454f-8189-59f005c53ee5)(form(Tok \
                    0))))))))))))))(Secondary((id \
                    4e58df93-6c5d-4a44-830b-1d5e63b60f80)(content(Whitespace\" \
                    \")))))))))(Tile((id \
                    d905e08e-084a-4e00-b1d6-de884f52cc7e)(form(Compound \
                    CellJoin))))(Secondary((id \
                    19ba46e9-6d88-4b16-8b17-54015f778a2f)(content(Whitespace\"\\n\"))))(Tile((id \
                    eaad638d-85bc-4548-8f3b-cd09656ce1c8)(form(Compound \
                    Test))(shards(0 1))(children(((Secondary((id \
                    fcfa1e23-8e28-4d9a-9c92-6eddeae4474c)(content(Whitespace\" \
                    \"))))(Tile((id \
                    3219ee2f-cba8-4442-a80a-24be08630bcd)(form(Tok \
                    odd))))(Tile((id \
                    3ab982a1-07e8-465e-81da-bf6a9e454f28)(form(Compound \
                    Ap))(shards(0 1))(children(((Tile((id \
                    18045238-6167-463c-9aff-3b58944205fb)(form(Tok \
                    1)))))))))(Secondary((id \
                    bdf95b73-551d-4726-bae6-658c177e7e4e)(content(Whitespace\" \
                    \")))))))))(Tile((id \
                    c258b337-bacf-435b-afe1-4505e066228f)(form(Compound \
                    CellJoin))))(Secondary((id \
                    f1b98744-a58f-4d49-bd25-42266033fb43)(content(Whitespace\"\\n\"))))(Tile((id \
                    30cf8e5a-9605-4909-8ad4-754654c99b7a)(form(Compound \
                    Test))(shards(0 1))(children(((Secondary((id \
                    f8a3fb05-8bc3-4f9d-a9e2-33fd0770b8b8)(content(Whitespace\" \
                    \"))))(Tile((id \
                    c9058ca8-5bd9-479d-9b07-5845a0dd21d6)(form(Tok \
                    not))))(Tile((id \
                    d82f78e3-6202-4724-8816-312a03772df6)(form(Compound \
                    Ap))(shards(0 1))(children(((Tile((id \
                    f2cf4125-e107-46c1-a395-f585efca540e)(form(Tok \
                    odd))))(Tile((id \
                    efed3c88-144f-4f00-a9e2-57b5efcef7fc)(form(Compound \
                    Ap))(shards(0 1))(children(((Tile((id \
                    5ee0a181-f559-4aff-8841-47f678a9061b)(form(Tok \
                    2))))))))))))))(Secondary((id \
                    2a7299f8-1bf4-4732-a292-7b0c40019df8)(content(Whitespace\" \
                    \")))))))))(Tile((id \
                    f985727a-0dcb-4efd-a475-3ead1702a062)(form(Compound \
                    CellJoin))))(Secondary((id \
                    d1c69abd-7542-41da-baea-54201dda053c)(content(Whitespace\"\\n\"))))(Tile((id \
                    e7e871cc-9a21-45a7-850a-930909854920)(form(Compound \
                    Test))(shards(0 1))(children(((Secondary((id \
                    38ced88e-71d4-400e-aaf2-1ae53047a54d)(content(Whitespace\" \
                    \"))))(Tile((id \
                    8a0c5603-50cc-42a3-884f-01f68a2f8b30)(form(Tok \
                    odd))))(Tile((id \
                    37091a2c-96df-48f8-8d53-da6e3946af2a)(form(Compound \
                    Ap))(shards(0 1))(children(((Tile((id \
                    99b7e4f1-c0ce-4aa5-b0b4-d35a29565506)(form(Tok \
                    3)))))))))(Secondary((id \
                    77cbfc5d-caf9-48bc-9deb-7f44fae29907)(content(Whitespace\" \
                    \")))))))))(Tile((id \
                    a07d12db-2d9b-468b-adc0-55dd4c9555df)(form(Compound \
                    CellJoin))))(Secondary((id \
                    6afc6dd3-2771-4550-8c72-7ea4d26b9e04)(content(Whitespace\"\\n\"))))(Tile((id \
                    6e5a9987-b639-4333-a551-16b773de8cde)(form(Compound \
                    Test))(shards(0 1))(children(((Secondary((id \
                    b0f0c4a3-80c4-4f4f-a972-efc8f5d55c0b)(content(Whitespace\" \
                    \"))))(Tile((id \
                    c5a35d65-c515-45b3-8d8d-4aac97a08a5c)(form(Tok \
                    not))))(Tile((id \
                    3bd46c05-484c-4cbb-a177-a866d0e35931)(form(Compound \
                    Ap))(shards(0 1))(children(((Tile((id \
                    948a9e97-dda9-4fbb-aaf6-2506f3343042)(form(Tok \
                    odd))))(Tile((id \
                    e6047083-3743-419f-ab96-905fe905946d)(form(Compound \
                    Ap))(shards(0 1))(children(((Tile((id \
                    a288e592-801b-4746-b02d-bba7bb59f84a)(form(Tok \
                    42))))))))))))))(Secondary((id \
                    a27239ef-5897-4650-9045-8e33d08319d2)(content(Whitespace\" \
                    \")))))))))(Tile((id \
                    887c067f-6272-4599-bf8a-67d2df177d62)(form(Compound \
                    CellJoin))))(Secondary((id \
                    015cb67f-34b6-41fd-aa16-6080eeaeb202)(content(Whitespace\" \
                    \"))))(Secondary((id \
                    7e6e5b1c-523a-4030-874d-40d74925087d)(content(Whitespace\"\\n\"))))(Tile((id \
                    368990a7-6099-4714-ba85-5654eb637521)(form(Compound \
                    Test))(shards(0 1))(children(((Secondary((id \
                    359386ea-8bbc-4f2b-8093-967011f40159)(content(Whitespace\" \
                    \"))))(Tile((id \
                    ac99f095-844c-420a-ac7c-5e6c1659fa7c)(form(Tok \
                    odd))))(Tile((id \
                    69cdc225-d08c-4c36-a50a-2616d09d34fc)(form(Compound \
                    Ap))(shards(0 1))(children(((Tile((id \
                    ca91a9bd-eacf-4722-9477-27240f8c0bcd)(form(Tok \
                    27)))))))))(Secondary((id \
                    5e56d9c6-5039-421e-962d-a368419b98c8)(content(Whitespace\" \
                    \"))))))))))()))(ancestors())))(caret \
                    Outer)(refractors((manuals())(multis((ids())(suppressed())(ephemerals())))(sample_focus((call_stack())(index \
                    -1)(pinned_stack())(indicated_call())(time())(seq \
                    0)(step_range())(pending_focus())))(autoprobe_target())(pending_probe_cursor()))))";
                 backup_text =
                   "test not(odd(0)) end;\n\
                    test odd(1) end;\n\
                    test not(odd(2)) end;\n\
                    test odd(3) end;\n\
                    test not(odd(42)) end; \n\
                    test odd(27) end";
               };
             hints = [ "zero" ];
           };
         syntax_tests = [ ("odd is recursive", IsRecursive "odd") ];
       })
