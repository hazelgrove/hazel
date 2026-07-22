let exercise : Exercise.t =
  Theorem
    (TheoremExercise.of_persistent
       {
         id = Haz3lcore.Id.v "f2132f9f-a452-481b-ba9a-c40e7d2346aa";
         title = "Reverse! Reverse!";
         module_name = "Ex_ReverseReverse";
         prompt =
           "Show that this implementation of list reverse is its own inverse \
            (i.e. that `rev` is *involutive*).";
         max_points = 10;
         prelude =
           {
             zipper =
               "((selection((focus Left)(content())(mode Normal)(anchor_caret \
                Outer)(smart_rounded false)))(relatives((siblings(((Tile((id \
                fd102aa6-29ed-43b8-8908-a26aad4352ac)(form(Compound \
                Let))(shards(0 1 2))(children(((Secondary((id \
                a4b7b78f-c530-4f02-904e-2d6c17544120)(content(Whitespace\" \
                \"))))(Tile((id 054ac2d0-cd39-407d-8a06-52e8c49c5d5c)(form(Tok \
                snoc))(sort Pat)))(Secondary((id \
                e39c3e41-c638-465b-8c9f-56dbff66bac5)(content(Whitespace\" \
                \")))))((Secondary((id \
                1d79510e-0a23-466a-85ad-5fa3a5155125)(content(Whitespace\" \
                \"))))(Tile((id \
                72329dcb-d478-4eeb-970b-9c0c4b78beae)(form(Compound \
                Fun))(shards(0 1))(children(((Secondary((id \
                5d5cc071-b93a-4498-99a0-efb5a84db7b4)(content(Whitespace\" \
                \"))))(Tile((id \
                f8760d12-ebd8-4826-9ae2-73b233612036)(form(Compound \
                Parens))(sort Pat)(shards(0 1))(children(((Tile((id \
                e37757e0-f9de-4050-893d-0122186ba9f5)(form(Tok t))(sort \
                Pat)))(Tile((id \
                a46d7204-9b08-49e5-9068-54587cecc18b)(form(Compound \
                Comma))(sort Pat)))(Secondary((id \
                5a40cc16-2adf-47d3-8688-30eab4f4235f)(content(Whitespace\" \
                \"))))(Tile((id 59a0d168-eb74-4f9c-bc52-1c1d07459df5)(form(Tok \
                h))(sort Pat))))))))(Secondary((id \
                3dab3fa5-67df-4d6a-b9ce-e790d8fe74f4)(content(Whitespace\" \
                \")))))))))(Secondary((id \
                ed31dec1-bf6b-4f4a-98bd-0bfba9779f53)(content(Whitespace\"\\n\"))))(Tile((id \
                291f4309-f87b-46c4-b8b8-b84d7d797514)(form(Compound \
                Case))(shards(0 1))(children(((Secondary((id \
                c6d6c20f-c275-47c1-b72e-cac9c4cb9718)(content(Whitespace\" \
                \"))))(Tile((id e01c2cf4-bdb5-4588-95e7-57af5fe47f24)(form(Tok \
                t))))(Secondary((id \
                4f1203a1-7150-4074-8dac-72bfa0c7cab2)(content(Whitespace\"\\n\"))))(Tile((id \
                ba5108d9-2b86-49be-8da7-0dcd68e9e8ff)(form(Compound \
                Rule))(sort Rul)(shards(0 1))(children(((Secondary((id \
                1de0cf3d-fdfa-4939-9690-1dd89deee2da)(content(Whitespace\" \
                \"))))(Tile((id 3445e911-facf-4c4f-bc77-4ca110190e30)(form(Tok \
                []))(sort Pat)))(Secondary((id \
                6d67c215-2443-492b-b216-b9f225853803)(content(Whitespace\" \
                \")))))))))(Secondary((id \
                5ab07199-0e32-4562-9afb-0c084bf672b8)(content(Whitespace\" \
                \"))))(Tile((id d786018d-3e8d-4946-86a2-0e1e83b5f61f)(form(Tok \
                h))))(Secondary((id \
                34f9a049-51d2-4c1e-b157-4d482d484568)(content(Whitespace\" \
                \"))))(Tile((id \
                3db16e1c-2548-4e52-ad1a-82fd711c9505)(form(Compound \
                Cons))))(Secondary((id \
                d0b0a8a4-2ea2-4a35-b05e-9a820bbbfbf3)(content(Whitespace\" \
                \"))))(Tile((id 018627d1-ab93-413d-80ea-c3996fe47892)(form(Tok \
                []))))(Secondary((id \
                f29e5383-b1cd-4b16-9c6f-575c9185beb1)(content(Whitespace\"\\n\"))))(Tile((id \
                d5d1cd0d-ad61-409f-aad8-f518b8e289e6)(form(Compound \
                Rule))(sort Rul)(shards(0 1))(children(((Secondary((id \
                37673775-95b1-494f-8c6e-2085b92f2c8a)(content(Whitespace\" \
                \"))))(Tile((id e78bae23-514d-4eb5-a6dc-729c6bd8cde2)(form(Tok \
                h'))(sort Pat)))(Secondary((id \
                984931b9-39bc-4496-b975-e362f30333f5)(content(Whitespace\" \
                \"))))(Tile((id \
                7dde53fe-beb5-4120-88c0-ef9070004ade)(form(Compound \
                Cons))(sort Pat)))(Secondary((id \
                9b95b6f5-278e-445b-a846-033296bc56d6)(content(Whitespace\" \
                \"))))(Tile((id 460f9bd3-efa3-42a9-a231-e693cf0d28f7)(form(Tok \
                t))(sort Pat)))(Secondary((id \
                bdd89846-2f75-429f-8d7d-a6e32ab7dd56)(content(Whitespace\" \
                \")))))))))(Secondary((id \
                2d1512e9-056f-4a8c-97ff-a5eb8d54d3fe)(content(Whitespace\" \
                \"))))(Tile((id af19ebeb-b21c-4f40-9be8-cfd03b557cfb)(form(Tok \
                h'))))(Secondary((id \
                5a42e81d-8916-4dd7-a163-f675a34f4a6f)(content(Whitespace\" \
                \"))))(Tile((id \
                3f10cfff-29d2-4302-961b-71c577daffea)(form(Compound \
                Cons))))(Secondary((id \
                6711eed5-8004-41a9-80ec-07a2cfe94b47)(content(Whitespace\" \
                \"))))(Tile((id 96be2ade-afbb-437e-94f6-27c05657538a)(form(Tok \
                snoc))))(Tile((id \
                ebfd6d63-5b98-455c-bd4b-bcb391f20ad9)(form(Compound \
                Ap))(shards(0 1))(children(((Tile((id \
                050126dc-7380-4253-b0bc-ab94b5701c5f)(form(Tok t))))(Tile((id \
                a6d61c79-15a5-4512-9d05-47125b8ae3c8)(form(Compound \
                Comma))))(Secondary((id \
                83f98941-65c2-488a-98c0-0ef70140fa91)(content(Whitespace\" \
                \"))))(Tile((id b96ba142-416c-4b44-a4e5-7aa3811b8db6)(form(Tok \
                h)))))))))(Secondary((id \
                4be53d23-0d27-49d1-9374-9a17e9fcc250)(content(Whitespace\"\\n\")))))))))(Secondary((id \
                329c83ba-c52a-4c3d-8c15-11f3be77ee8f)(content(Whitespace\"\\n\")))))))))(Secondary((id \
                a963f34e-5cde-44d1-b512-c76ad832469f)(content(Whitespace\"\\n\"))))(Secondary((id \
                677aca17-309a-4669-ba39-566760ece62a)(content(Whitespace\"\\n\"))))(Tile((id \
                84eeacdb-289f-4090-9f85-41b1377302df)(form(Compound \
                Let))(shards(0 1 2))(children(((Secondary((id \
                1aaad735-b949-4ee4-88c1-400e790985a4)(content(Whitespace\" \
                \"))))(Tile((id 3d5f673e-9b3a-44c8-83e4-43b798df83e3)(form(Tok \
                rev))(sort Pat)))(Secondary((id \
                f6880092-3aa7-4883-90f7-353a96ff241c)(content(Whitespace\" \
                \")))))((Secondary((id \
                69f3f3ff-6eff-4451-9d34-326263f6bd8b)(content(Whitespace\" \
                \"))))(Tile((id \
                14adc50e-ccb3-4b89-9184-823c5f96d9e1)(form(Compound \
                Fun))(shards(0 1))(children(((Secondary((id \
                ce6cdf4b-1a65-4257-9d1e-80db79765e85)(content(Whitespace\" \
                \"))))(Tile((id 27b9b660-3471-4657-8092-529066a4dba2)(form(Tok \
                l))(sort Pat)))(Secondary((id \
                c2410021-f25b-4656-a6a1-229a2f4d1f8d)(content(Whitespace\" \
                \")))))))))(Secondary((id \
                96b78252-7e67-4db3-ad46-446225f66545)(content(Whitespace\"\\n\"))))(Tile((id \
                109e88c3-ab1e-435e-a76e-739d887c6f28)(form(Compound \
                Case))(shards(0 1))(children(((Secondary((id \
                085b76f6-f801-42d3-90d6-cd2733774bca)(content(Whitespace\" \
                \"))))(Tile((id 96cde698-d501-4dfc-bcff-c8d9c1614a42)(form(Tok \
                l))))(Secondary((id \
                6a046f3d-1a82-4138-bd63-33f4ad7c3b69)(content(Whitespace\"\\n\"))))(Tile((id \
                8559bcf5-3214-457f-b85b-25c760347fbe)(form(Compound \
                Rule))(sort Rul)(shards(0 1))(children(((Secondary((id \
                372f3b31-1214-46a6-8beb-7eba6714be09)(content(Whitespace\" \
                \"))))(Tile((id 55ce380c-c016-45e9-bffc-31e25cdf259a)(form(Tok \
                []))(sort Pat)))(Secondary((id \
                28cd1677-aaa9-4c4c-96c0-637603faac5b)(content(Whitespace\" \
                \")))))))))(Secondary((id \
                0ed96a3a-cdaf-4fbf-a5f5-6b7252872b5a)(content(Whitespace\" \
                \"))))(Tile((id 0f7b6a1c-4e59-4ea2-bb7a-e73bd295657e)(form(Tok \
                []))))(Secondary((id \
                0df02313-128c-476e-99db-99f33004fd5c)(content(Whitespace\"\\n\"))))(Tile((id \
                5b0aaa13-4c92-499d-bca1-0802db244d8b)(form(Compound \
                Rule))(sort Rul)(shards(0 1))(children(((Secondary((id \
                1fd65fc7-26eb-4143-ba37-34d254dac2f4)(content(Whitespace\" \
                \"))))(Tile((id aecee435-7b0c-4710-82e0-7dab6f11eec4)(form(Tok \
                h))(sort Pat)))(Secondary((id \
                4a161b53-b999-495c-b023-059c55d76232)(content(Whitespace\" \
                \"))))(Tile((id \
                1d9ef008-ca63-486a-8e3d-a52f3b5ccce7)(form(Compound \
                Cons))(sort Pat)))(Secondary((id \
                27ce2ef1-8be1-49dd-ad53-296f2b21a42a)(content(Whitespace\" \
                \"))))(Tile((id 38be71ec-5604-464b-b30e-ce1c75dc3b8d)(form(Tok \
                t))(sort Pat)))(Secondary((id \
                1710ec7d-bc7b-4e9e-8ba9-604f6251ac71)(content(Whitespace\" \
                \")))))))))(Secondary((id \
                79268f83-dc8b-400e-94c3-dc2c9809c3dc)(content(Whitespace\" \
                \"))))(Tile((id ed8613a9-8329-4e6e-a895-d05712e1fa97)(form(Tok \
                snoc))))(Tile((id \
                f6537cb5-1835-40b3-b71d-bf68eb5c0419)(form(Compound \
                Ap))(shards(0 1))(children(((Tile((id \
                74cd684e-ee87-49f6-8767-eddc9c6e56cc)(form(Tok \
                rev))))(Tile((id \
                9b2b8d74-7670-41a4-a58b-68fa2d1ecc60)(form(Compound \
                Ap))(shards(0 1))(children(((Tile((id \
                da807ef9-6093-4a2f-8d25-ad81662bcb47)(form(Tok \
                t)))))))))(Tile((id \
                1e2c6769-a05b-4946-9b5e-1433574feb0a)(form(Compound \
                Comma))))(Secondary((id \
                84006450-623f-4429-9614-68bc956e88c6)(content(Whitespace\" \
                \"))))(Tile((id f95acf35-5537-4088-b782-d44e04168c4a)(form(Tok \
                h)))))))))(Secondary((id \
                c5cf865d-1ce0-4e11-879e-0c8f71eb3067)(content(Whitespace\"\\n\")))))))))(Secondary((id \
                b0446624-cf06-4cf0-8faa-cb09d9333d4e)(content(Whitespace\"\\n\")))))))))(Secondary((id \
                719d3da6-0de4-4085-b589-9ad7108c76f3)(content(Whitespace\" \
                \")))))((Grout((id 2567f067-c8de-4ca2-940e-b2d796774665)(shape \
                Convex))))))(ancestors())))(caret \
                Outer)(refractors((manuals())(multis((ids())(suppressed())(ephemerals())))(sample_focus((call_stack())(index \
                -1)(pinned_stack())(indicated_call())(time())(seq \
                0)(step_range())(pending_focus())))(autoprobe_target())(pending_probe_cursor()))))";
             backup_text =
               "let snoc = fun (t, h) ->\n\
                case t\n\
                | [] => h :: []\n\
                | h' :: t => h' :: snoc(t, h)\n\
                end\n\
                in\n\n\
                let rev = fun l ->\n\
                case l\n\
                | [] => []\n\
                | h :: t => snoc(rev(t), h)\n\
                end\n\
                in ";
           };
         lemmas =
           {
             zipper =
               "((selection((focus Left)(content())(mode Normal)(anchor_caret \
                Outer)(smart_rounded \
                false)))(relatives((siblings(()((Grout((id \
                d99aef7b-4e33-4302-8845-ad23ac1f0225)(shape \
                Convex))))))(ancestors())))(caret \
                Outer)(refractors((manuals())(multis((ids())(suppressed())(ephemerals())))(sample_focus((call_stack())(index \
                -1)(pinned_stack())(indicated_call())(time())(seq \
                0)(step_range())(pending_focus())))(autoprobe_target())(pending_probe_cursor()))))";
             backup_text = "";
           };
         theorem =
           {
             zipper =
               "((selection((focus Left)(content())(mode Normal)(anchor_caret \
                Outer)(smart_rounded false)))(relatives((siblings(((Tile((id \
                2ce68936-a94d-4932-8767-c57ade46325d)(form(Compound \
                Theorem))(shards(0 1 2))(children(((Secondary((id \
                aa8ce070-cf13-49b3-bdfa-90b4cf4f0752)(content(Whitespace\" \
                \"))))(Tile((id 29951d44-4ce5-4364-8e7b-1c928918632a)(form(Tok \
                rev_rev))(sort Pat)))(Secondary((id \
                bb01c1dd-bf7b-4ed5-89f5-8b402104e137)(content(Whitespace\" \
                \")))))((Secondary((id \
                ff682821-22cc-41cf-a838-08f00f6eda26)(content(Whitespace\"\\n\"))))(Tile((id \
                01ff84f5-33b8-4841-a9c4-c381c4b2415d)(form(Compound \
                Forall))(shards(0 1))(children(((Secondary((id \
                b12f30d5-c323-4b01-8f8e-e0cb407913a4)(content(Whitespace\" \
                \"))))(Tile((id b59d7499-4381-47da-a6a3-b17752b08869)(form(Tok \
                xs))(sort Pat)))(Tile((id \
                85638ea9-e054-4b0b-919e-99d8e1018301)(form(Compound \
                TypeAsc))(sort Pat)))(Tile((id \
                f86d96f5-24a7-4f6e-b8c3-25ae9a34701e)(form(Compound \
                ListLit))(sort Typ)(shards(0 1))(children(((Tile((id \
                f79cb4dd-acd7-4783-abca-e216503a96c8)(form(Tok Int))(sort \
                Typ))))))))(Secondary((id \
                aaa01c90-f652-48cb-ae2e-172c7c937e1b)(content(Whitespace\" \
                \")))))))))(Secondary((id \
                c3d89e8e-ad7e-4576-8d56-d2ea33026a4b)(content(Whitespace\" \
                \"))))(Tile((id aeffd275-5137-49e1-8d14-da38a675e7a8)(form(Tok \
                rev))))(Tile((id \
                cf8c618d-5244-44ab-8826-653258b62886)(form(Compound \
                Ap))(shards(0 1))(children(((Tile((id \
                1f399d07-0f9e-4ff7-b4f6-9bc73bf25acf)(form(Tok \
                rev))))(Tile((id \
                857b88d4-8707-46d4-b210-03006825ce6d)(form(Compound \
                Ap))(shards(0 1))(children(((Tile((id \
                aece9f7f-bda1-473f-875d-5155e0dca213)(form(Tok \
                xs))))))))))))))(Secondary((id \
                24757eee-858f-4faf-a6fb-48eb895b3404)(content(Whitespace\" \
                \"))))(Tile((id \
                4b13823b-ba1c-47ea-b3b8-6436a75dd121)(form(Compound \
                Equals))))(Secondary((id \
                e8683895-8d70-4109-8a62-f1d6bf60bd79)(content(Whitespace\" \
                \"))))(Tile((id 9ff9171e-216b-4b4d-8e3f-5c737fe48c48)(form(Tok \
                xs))))(Secondary((id \
                54ab1c04-c94e-4eef-b863-e513ce02e855)(content(Whitespace\"\\n\")))))))))(Secondary((id \
                35640d7b-aa55-4a90-ba71-066d414ebd0a)(content(Whitespace\" \
                \")))))((Grout((id d672e3e8-daae-461d-88f8-ad3a8deaa4a7)(shape \
                Convex))))))(ancestors())))(caret \
                Outer)(refractors((manuals())(multis((ids())(suppressed())(ephemerals())))(sample_focus((call_stack())(index \
                -1)(pinned_stack())(indicated_call())(time())(seq \
                0)(step_range())(pending_focus())))(autoprobe_target())(pending_probe_cursor()))))";
             backup_text =
               "theorem rev_rev =\nforall xs:[Int] -> rev(rev(xs)) == xs\nin ";
           };
       })
