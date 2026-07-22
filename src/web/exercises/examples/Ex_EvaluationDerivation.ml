let exercise : Exercise.t =
  Derivation
    (DerivationExercise.of_persistent
       {
         id = Haz3lcore.Id.v "2f0137e3-672e-47f8-8493-5a593e1959c3";
         title = "Evaluation Derivation";
         module_name = "Ex_EvaluationDerivation";
         prompt =
           "Derive the following judgement using the evaluation rules for ALF. \
            Substitution should be performed inline.";
         max_points = 10;
         prelude =
           {
             zipper =
               "((selection((focus Left)(content())(mode Normal)(anchor_caret \
                Outer)(smart_rounded \
                false)))(relatives((siblings(()((Grout((id \
                4142fcba-10d7-46a5-aa4f-39e5e83fea37)(shape \
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
                Outer)(smart_rounded \
                false)))(relatives((siblings(()((Grout((id \
                54bc9a91-0c32-4fce-a146-23a705a914c1)(shape \
                Convex))))))(ancestors())))(caret \
                Outer)(refractors((manuals())(multis((ids())(suppressed())(ephemerals())))(sample_focus((call_stack())(index \
                -1)(pinned_stack())(indicated_call())(time())(seq \
                0)(step_range())(pending_focus())))(autoprobe_target())(pending_probe_cursor()))))";
             backup_text = "";
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
                            f7f0a7a8-744f-4897-a9dc-ecdc225fe9ca)(form(Compound \
                            Let))(sort(Drv Exp))(shards(0 1 \
                            2))(children(((Secondary((id \
                            316995ff-326a-4dcf-a7f6-6246d5c5fa35)(content(Whitespace\" \
                            \"))))(Tile((id \
                            9d58b2b8-bb85-457e-a643-8a27f6e360f8)(form(Tok \
                            isNat))(sort(Drv Pat))))(Secondary((id \
                            d4a922f9-4dee-4f18-91c4-53a4c70c31c0)(content(Whitespace\" \
                            \")))))((Secondary((id \
                            8453d5f2-7f55-4499-aa6f-3947a2f83880)(content(Whitespace\" \
                            \"))))(Tile((id \
                            97b8dcd9-2f46-457b-97a1-f2d5ba8f4a05)(form(Compound \
                            Fun))(sort(Drv Exp))(shards(0 \
                            1))(children(((Secondary((id \
                            d7336c9d-04a1-48db-af12-c6edadfc67d2)(content(Whitespace\" \
                            \"))))(Tile((id \
                            6d59caac-7e52-4019-a004-fb8681ee89f8)(form(Tok \
                            a))(sort(Drv Pat))))(Secondary((id \
                            638bd11b-18fb-4eea-a46b-ef37777bd58c)(content(Whitespace\" \
                            \")))))))))(Secondary((id \
                            357c0516-c3b8-42e6-a0f7-5ad9d8c5f13a)(content(Whitespace\" \
                            \"))))(Tile((id \
                            701be285-f044-45c6-8b17-79b05e69ddda)(form(Tok \
                            a))(sort(Drv Exp))))(Secondary((id \
                            596c93d2-3f3d-40e7-852e-44850227b4ae)(content(Whitespace\" \
                            \"))))(Tile((id \
                            e4e3dbae-6482-47bc-8181-c466a122e051)(form(Compound \
                            Gt))(sort(Drv Exp))))(Secondary((id \
                            320537a3-7142-41d6-bc5a-7c616cce2cd5)(content(Whitespace\" \
                            \"))))(Tile((id \
                            a9e834fc-e944-4e60-ae9a-71edc4fefbb7)(form(Compound \
                            UnaryMinus))(sort(Drv Exp))))(Tile((id \
                            2472f99e-0030-426b-9376-acdc70c3905c)(form(Tok \
                            1))(sort(Drv Exp))))(Secondary((id \
                            43cdf0ea-4204-4acb-890d-ae0dda740045)(content(Whitespace\" \
                            \")))))))))(Secondary((id \
                            1a5804e1-81c6-41d1-9ff2-8e20716ec227)(content(Whitespace\" \
                            \"))))(Tile((id \
                            af2268f1-958d-42d8-aced-419b076c355b)(form(Compound \
                            If))(sort(Drv Exp))(shards(0 1 \
                            2))(children(((Secondary((id \
                            66ea0dd6-8b64-42d5-8ff9-cddaaf109900)(content(Whitespace\" \
                            \"))))(Tile((id \
                            04f8f9d0-6597-490b-a763-2fd70ee7927f)(form(Tok \
                            isNat))(sort(Drv Exp))))(Secondary((id \
                            d60c8bf1-842f-4092-8f07-c4d0bf85bd8a)(content(Whitespace\" \
                            \"))))(Tile((id \
                            3307f4de-108d-4cd2-af76-37cad811df13)(form(Compound \
                            Ap))(sort(Drv Exp))(shards(0 \
                            1))(children(((Tile((id \
                            1b883d49-d81d-41b8-bb15-486eadc2d5c1)(form(Tok \
                            5))(sort(Drv Exp))))(Secondary((id \
                            ff33926e-7e9a-482d-a65d-c9d0494f5fe8)(content(Whitespace\" \
                            \"))))(Tile((id \
                            3e005c72-859e-4b3a-b893-29a868426d56)(form(Compound \
                            Minus))(sort(Drv Exp))))(Secondary((id \
                            35bdd74f-e852-4179-8ee9-16442cb5992a)(content(Whitespace\" \
                            \"))))(Tile((id \
                            7099363d-57a2-4aa3-babd-2ebc052073ab)(form(Tok \
                            3))(sort(Drv Exp)))))))))(Secondary((id \
                            4eb70536-f641-4957-914a-07d790018072)(content(Whitespace\" \
                            \")))))((Secondary((id \
                            a270788f-4c13-4628-a23f-01abbcbb3a63)(content(Whitespace\" \
                            \"))))(Tile((id \
                            be9ef34f-22a7-4c24-b5a6-3ffb16f39409)(form(Tok \
                            1))(sort(Drv Exp))))(Secondary((id \
                            3c47c5aa-e658-44a6-832c-4838a1e85d5e)(content(Whitespace\" \
                            \")))))))))(Secondary((id \
                            34ffd936-800d-4ff4-bedf-1d40bc1cf4ea)(content(Whitespace\" \
                            \"))))(Tile((id \
                            e3841067-9eef-4e15-bd45-42046b69930e)(form(Tok \
                            2))(sort(Drv Exp))))(Secondary((id \
                            b461a1d4-5c9b-44ac-9d4a-72d982c44b56)(content(Whitespace\" \
                            \"))))(Tile((id \
                            9b46c425-fc3a-4224-b912-e1ac6ff230ff)(form(Compound \
                            Eval))(sort(Drv Exp))))(Secondary((id \
                            dc9af9c8-3658-43cc-82ea-d0cfa2c844db)(content(Whitespace\" \
                            \"))))(Tile((id \
                            cd5a80ac-17d5-42e9-9b2c-f8f1d756c470)(form(Tok \
                            1))(sort(Drv Exp)))))()))(ancestors())))(caret \
                            Outer)(refractors((manuals())(multis((ids())(suppressed())(ephemerals())))(sample_focus((call_stack())(index \
                            -1)(pinned_stack())(indicated_call())(time())(seq \
                            0)(step_range())(pending_focus())))(autoprobe_target())(pending_probe_cursor()))))";
                         backup_text =
                           "let isNat = fun a -> a > -1 in if isNat (5 - 3) \
                            then 1 else 2 \\=/ 1";
                       };
                     rule = None;
                   },
                 [] );
           ];
       })
