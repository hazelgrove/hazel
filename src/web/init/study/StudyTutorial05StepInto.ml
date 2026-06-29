let out : string * Haz3lcore.PersistentSegment.t =
  ( "Study / tutorial / 05-step-into",
    {
      segment =
        "((Secondary((id \
         58582536-cbeb-4a1e-8ba7-cef6bcf94433)(content(Comment\"# PROBES \
         TUTORIAL - PART 5: STEP INTO AND THE DYNAMIC CURSOR BAR \
         #\"))))(Secondary((id \
         a130c185-60d6-4450-8c4c-665e82aaf8ea)(content(Whitespace\"\\n\"))))(Secondary((id \
         37d26d95-acde-4f51-87f6-b3826f847a45)(content(Comment\"# You've \
         pinned a call and can see values inside a function. \
         #\"))))(Secondary((id \
         0e6282a1-3680-4ed7-95c5-e62a57a562f8)(content(Whitespace\"\\n\"))))(Secondary((id \
         266db03d-647c-4f57-ac81-7e8377eb5b04)(content(Comment\"# But what if \
         the bug is deeper, inside a function that your #\"))))(Secondary((id \
         d7dafd66-be37-4354-9276-f858fa7f5ad9)(content(Whitespace\"\\n\"))))(Secondary((id \
         7d6e3a33-8d17-4c90-af9c-1de86ccec729)(content(Comment\"# function \
         calls? Step Into follows the call stack down. #\"))))(Secondary((id \
         93085753-157e-4c5c-ac73-957b8acbcf7b)(content(Whitespace\"\\n\"))))(Secondary((id \
         61a0cc74-98c6-4e1c-b499-12cccb79e708)(content(Whitespace\"\\n\"))))(Secondary((id \
         9fddf872-586c-4005-a290-23c53390beca)(content(Comment\"# TRY THIS: \
         #\"))))(Secondary((id \
         ac3e9e98-4add-4b2f-9ef0-f03447266a46)(content(Whitespace\"\\n\"))))(Secondary((id \
         d3bedc3d-b112-4452-9419-775355769a28)(content(Comment\"# 1. Turn on \
         auto-probe and click inside `daily_water` #\"))))(Secondary((id \
         67eca792-7d90-4d3e-8bbe-ab1736622724)(content(Whitespace\"\\n\"))))(Secondary((id \
         008ecd81-2345-4fab-889c-f5db1a546281)(content(Comment\"# 2. Pin one \
         of the test calls (click a sample > Pin) #\"))))(Secondary((id \
         47acfd3d-3bd0-415a-83a6-19510afa6250)(content(Whitespace\"\\n\"))))(Secondary((id \
         37743317-4166-433e-b177-7cd2285429a9)(content(Comment\"# 3. Now add a \
         probe to the `phase_multiplier(phase)` call #\"))))(Secondary((id \
         6ffa2d42-188e-4598-a1cf-b64f05a25535)(content(Whitespace\"\\n\"))))(Secondary((id \
         099bfc24-8d5f-4a56-b356-3731e86aeb58)(content(Comment\"#    inside \
         `daily_water` (click on `phase_multiplier`) #\"))))(Secondary((id \
         16fdd5aa-9553-478e-b376-4a4f0a50f551)(content(Whitespace\"\\n\"))))(Secondary((id \
         4140d291-02ae-47be-802a-0b1e4175fba9)(content(Comment\"# 4. Click \
         that sample and choose \\\"Step Into\\\" from the \
         #\"))))(Secondary((id \
         57c959c7-b68c-4fe6-a7a9-965f05534133)(content(Whitespace\"\\n\"))))(Secondary((id \
         a3cb4d14-e7c3-4ce2-9de5-905c8a5300cf)(content(Comment\"#    dropdown \
         (or press Enter) #\"))))(Secondary((id \
         c2941ec2-8559-48d1-b8ae-c9ce20dedd54)(content(Whitespace\"\\n\"))))(Secondary((id \
         d08f56f3-8d78-4342-b24a-e313a09260b0)(content(Comment\"# 5. Your \
         cursor jumps into `phase_multiplier`! The probes \
         #\"))))(Secondary((id \
         50d406df-2a42-4b33-b32b-1fd8bfdf26d4)(content(Whitespace\"\\n\"))))(Secondary((id \
         cef60692-4cfd-4eef-bd20-5fffa02afe21)(content(Comment\"#    there \
         show only values from your pinned context. #\"))))(Secondary((id \
         dd27cc72-91d9-4477-ad6e-621e44cbb240)(content(Whitespace\"\\n\"))))(Secondary((id \
         65aa0152-312f-41b2-a4df-d136342bcf44)(content(Whitespace\"\\n\"))))(Secondary((id \
         38ad6a49-e7fb-4686-97a2-fd8d917338dc)(content(Comment\"# THE DYNAMIC \
         CURSOR BAR #\"))))(Secondary((id \
         29da477d-e80a-4bb7-8553-5408e93f52a9)(content(Whitespace\"\\n\"))))(Secondary((id \
         3cb85ccc-dfcc-4f6d-9c48-743f3530b2ab)(content(Comment\"# Look at the \
         bar at the top of the editor after stepping in. #\"))))(Secondary((id \
         4030eeec-1200-4984-9e29-6d7f635dd6a4)(content(Whitespace\"\\n\"))))(Secondary((id \
         e6132e6a-1b41-49db-9f55-ff254ecae3b5)(content(Comment\"# It shows \
         your position in the call stack as breadcrumbs: #\"))))(Secondary((id \
         7471686c-a398-4a7f-b5f2-9ecbb96507a6)(content(Whitespace\"\\n\"))))(Secondary((id \
         f9177924-b7b6-47cd-a698-9d4d64a19f40)(content(Comment\"#   top-level \
         > daily_water > phase_multiplier #\"))))(Secondary((id \
         44c14fb3-885c-4411-bb44-7b57d16bfb34)(content(Whitespace\"\\n\"))))(Secondary((id \
         7a1fba45-7a36-4dff-b0b6-a6c3f98a51ab)(content(Comment\"# Click a \
         function name to jump to its definition. #\"))))(Secondary((id \
         49a3691e-bbdd-4884-8eb3-4856723ce8fa)(content(Whitespace\"\\n\"))))(Secondary((id \
         51e4013b-01f6-4608-b2ca-053972e59f71)(content(Comment\"# Click a \
         chevron (>) to jump to the call site. #\"))))(Secondary((id \
         6402b414-0470-465f-855d-e7075c590476)(content(Whitespace\"\\n\"))))(Secondary((id \
         ef17a796-ec6b-4ca6-a173-00e586a3d11d)(content(Comment\"# This lets \
         you move up and down the call stack freely. #\"))))(Secondary((id \
         99a52e7f-814e-4828-a7ef-a9568ba2852d)(content(Whitespace\"\\n\"))))(Secondary((id \
         1b1d063f-20d6-4eea-bf6b-a2c93519019b)(content(Whitespace\"\\n\"))))(Tile((id \
         e40ff960-c1f9-4e30-85ab-640b5bd4bfda)(label(type = in))(mold((out \
         Exp)(in_(TPat Typ))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         4a578aa2-c455-42ea-bc5e-58d8618b31dc)(content(Whitespace\" \
         \"))))(Tile((id \
         03bcc2f8-a5d4-4dc9-9563-e7fc34d6fd85)(label(MoonPhase))(mold((out \
         TPat)(in_())(nibs(((shape Convex)(sort TPat))((shape Convex)(sort \
         TPat))))))(shards(0))(children())))(Secondary((id \
         44600bb1-3212-4540-a4b4-b69aecb28cae)(content(Whitespace\" \
         \")))))((Secondary((id \
         8816f4b0-119c-403a-a837-f7a34a26bdd0)(content(Whitespace\" \
         \"))))(Tile((id \
         10ae69cf-5344-4f8f-ad03-7a15dc2391cf)(label(+))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape(Concave 33))(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         5b586618-a786-49b2-8b39-a365f0b7cf0f)(content(Whitespace\" \
         \"))))(Tile((id \
         eb9a1ff2-7038-46f1-b544-144a0f3d6196)(label(New))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         3b64a551-ef1c-4237-a94d-b10f0217c60b)(content(Whitespace\" \
         \"))))(Tile((id \
         327455e0-ab0d-4e10-957e-2ad10fe480e3)(label(+))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 12))(sort Typ))((shape(Concave \
         12))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         8e3726e9-4086-4e89-8f14-78f7e218884b)(content(Whitespace\" \
         \"))))(Tile((id \
         cd933287-dccb-4365-87d3-2339618718ec)(label(Waxing))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         99c4dccd-694d-4749-a18c-19207da895b0)(content(Whitespace\" \
         \"))))(Tile((id \
         ec649e1e-a2af-46fa-85b9-daeb0c1f83d0)(label(+))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 12))(sort Typ))((shape(Concave \
         12))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         4c80579a-d2fa-4491-a559-31089be64793)(content(Whitespace\" \
         \"))))(Tile((id \
         81fcb38a-beef-4e56-b425-0d2a6071d0dd)(label(Full))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         7a18787c-4f99-4949-90d4-16a809b89eb2)(content(Whitespace\" \
         \"))))(Tile((id \
         4377d444-f520-4b91-bb20-af063a2c47cf)(label(+))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 12))(sort Typ))((shape(Concave \
         12))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         04134d47-1814-4605-bf34-8ea2bca90552)(content(Whitespace\" \
         \"))))(Tile((id \
         085b9423-5f8f-4929-a427-ebcd3f77ec63)(label(Waning))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         5ff001ef-e8be-49e9-bf4e-0cc2907643bd)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         32f04c40-d1e8-4c93-9a82-4095d9152e00)(content(Whitespace\"\\n\"))))(Secondary((id \
         450a6ea6-fbed-46c9-bb3d-c32afa3fd021)(content(Whitespace\"\\n\"))))(Tile((id \
         fed4eabb-e0dc-4a2b-9963-5d758b586d26)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         0cd0b064-ec06-4cb4-9d55-eda681513d13)(content(Whitespace\" \
         \"))))(Tile((id \
         1f61954b-e368-4d0e-9f9d-ff139de22ac9)(label(phase_multiplier))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         bd28d0ca-f96e-45db-af1a-53ebed51fbbe)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 24))(sort Pat))((shape(Concave \
         24))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         4feddae4-8b09-4d28-bdc5-20fc4ec69c63)(content(Whitespace\" \
         \"))))(Tile((id \
         67ad2c84-c149-416d-ba57-e18944040e51)(label(MoonPhase))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         9ee142e9-9c76-4762-a943-37550a4c8864)(content(Whitespace\" \
         \"))))(Tile((id \
         400caadc-8ad4-44ee-a54c-6b9c38474b28)(label(->))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 13))(sort Typ))((shape(Concave \
         13))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         108cb571-49f9-4cab-bfb1-b01f82cfdc44)(content(Whitespace\" \
         \"))))(Tile((id \
         89318bf1-ff36-406b-b1d0-0fb70aabb191)(label(Float))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         85f9ebff-7cae-4d30-a10e-25603d0c41a2)(content(Whitespace\" \
         \")))))((Secondary((id \
         b96dec34-2da4-453f-8490-f2122f4e6c8e)(content(Whitespace\"\\n\"))))(Tile((id \
         a566bdf1-f70a-4ce7-a56d-6c3b5c477e36)(label(fun ->))(mold((out \
         Exp)(in_(Pat))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         37))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         777246e6-2509-4b60-bc16-40a923e56a1d)(content(Whitespace\" \
         \"))))(Tile((id \
         a151053d-260a-4be8-a258-7cd4d61d005e)(label(phase))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         e3a252d8-3944-466c-aabd-5f0727003cfc)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         9512a8ed-f468-41b0-a2ac-84b2d76ba654)(content(Whitespace\" \
         \"))))(Tile((id 1170c96b-de10-4433-9b5a-0c3bea4a0c49)(label(case \
         end))(mold((out Exp)(in_(Rul))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         fc4a90f7-b6c5-4a5a-87fb-a5a34987f921)(content(Whitespace\" \
         \"))))(Tile((id \
         b0db1459-1db2-49e2-a260-022ace71e25a)(label(phase))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         539f2b4a-915c-494c-a43d-9e6582019b45)(content(Whitespace\"\\n\"))))(Tile((id \
         f18dde2a-37e6-471a-873a-d43905443897)(label(| =>))(mold((out \
         Rul)(in_(Pat))(nibs(((shape(Concave 46))(sort Exp))((shape(Concave \
         46))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         c27a1826-b315-4dfd-a5e9-0b7b387a6262)(content(Whitespace\" \
         \"))))(Tile((id \
         ea993afc-dcb4-4e07-afd9-632c6dff75f6)(label(New))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         8746e57a-9df0-4297-b354-a72a14311bf7)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         0587e9eb-61a0-4776-8c78-3de017e0f9cb)(content(Whitespace\" \
         \"))))(Tile((id \
         32d2ab90-b9e2-4baa-a72a-4b3d662cc619)(label(1.2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         6973fa98-c716-4da3-a9c7-002ff7386a1a)(content(Whitespace\"\\n\"))))(Tile((id \
         4699a071-a70d-4d94-a729-899768fdd242)(label(| =>))(mold((out \
         Rul)(in_(Pat))(nibs(((shape(Concave 46))(sort Exp))((shape(Concave \
         46))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         d7eef342-ab0a-49b3-b4c4-093faa9a451b)(content(Whitespace\" \
         \"))))(Tile((id \
         e1b84459-a8af-4483-94a2-ad8d3625fe59)(label(Waxing))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         fa1051fa-cdd6-4bbd-ae41-1e956e7e5bf0)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         30abd858-4a62-48b1-bccd-8e7e7ee51f4f)(content(Whitespace\" \
         \"))))(Tile((id \
         a47d1440-9a43-4fab-a997-d9c37a83cd79)(label(1.1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         8b48d126-7333-4552-8f94-6eb76285cab4)(content(Whitespace\"\\n\"))))(Tile((id \
         9f6f69fe-a46a-4a0c-a04c-a5250a8f4a31)(label(| =>))(mold((out \
         Rul)(in_(Pat))(nibs(((shape(Concave 46))(sort Exp))((shape(Concave \
         46))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         ccf4f174-b532-409f-9ba6-90a857e47457)(content(Whitespace\" \
         \"))))(Tile((id \
         9b6ef33a-495c-45f1-8646-06522e879a56)(label(Full))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         d4681a17-7c0e-4a65-bb80-f1ab1811fe71)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         0a7f355d-6f8b-4e76-8a0e-8a304ad844aa)(content(Whitespace\" \
         \"))))(Tile((id \
         48bb1ab0-d92d-4cac-97ad-ba32d816fad8)(label(0.88))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         e0a42319-eb1b-4a5c-9a2b-0e7578d903af)(content(Whitespace\"\\n\"))))(Tile((id \
         97142045-9320-4d4a-a487-7af59fe69a35)(label(| =>))(mold((out \
         Rul)(in_(Pat))(nibs(((shape(Concave 46))(sort Exp))((shape(Concave \
         46))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         d532dec0-a3cc-41d8-aa6c-7129dcf7351d)(content(Whitespace\" \
         \"))))(Tile((id \
         5108a400-0f2d-43c0-8433-9718d660c23b)(label(Waning))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         3445daaf-86cf-4296-9074-c7717009bee8)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         33d54ae1-d6f4-4732-9a68-0f3e11a11807)(content(Whitespace\" \
         \"))))(Tile((id \
         f5e2011c-55c3-4a58-b836-35fa61c45ba5)(label(0.95))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         15b5392b-5079-45ba-ada0-40b8a83b9423)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         82b15e37-2eda-4ada-bbb1-84c88ea338c8)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         cd1d9fa9-92ad-4c7a-a211-5fb82d44daec)(content(Whitespace\"\\n\"))))(Secondary((id \
         ffa46f8e-12ec-4e59-9bd2-29e61ffa5397)(content(Whitespace\"\\n\"))))(Tile((id \
         da44460e-fb42-4603-b04c-8d50f988bd01)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         10724f49-0756-4dad-ad92-f1bbe9529a11)(content(Whitespace\" \
         \"))))(Tile((id \
         a6014ad6-11d0-4d41-a08f-c76dfe5a32f5)(label(shade_multiplier))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         e7be74f9-1a5a-499a-836b-d960106dea03)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 24))(sort Pat))((shape(Concave \
         24))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         6c353e56-96bf-4fbd-b310-87aa672df818)(content(Whitespace\" \
         \"))))(Tile((id \
         4430bca2-b568-481d-a799-f287ac4c92d1)(label(Int))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         ebbdfa77-6593-41e6-a64f-5fce67318eb4)(content(Whitespace\" \
         \"))))(Tile((id \
         4acd02b3-1f96-4c0e-9ad5-aea34e0b9720)(label(->))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 13))(sort Typ))((shape(Concave \
         13))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         c0c51c91-7107-4c10-932c-a91d2e766c09)(content(Whitespace\" \
         \"))))(Tile((id \
         4d3918c5-b0e1-4160-95c8-7baf922cc632)(label(Float))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         294aba68-1d75-40d9-9b60-c6b0ab619f2a)(content(Whitespace\" \
         \")))))((Secondary((id \
         ba489cd1-40fb-4310-9ca1-71fb5e36d21a)(content(Whitespace\"\\n\"))))(Tile((id \
         3fd70a6e-13c4-49bd-a394-8b1fbb09ef07)(label(fun ->))(mold((out \
         Exp)(in_(Pat))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         37))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         c42f2e61-81e4-4e76-97d1-ef5f76090975)(content(Whitespace\" \
         \"))))(Tile((id \
         c6d3cd91-f67d-4595-937a-fb0701c82957)(label(shade))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         82d1b008-cb99-46e2-a299-0bef2d184990)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         bb369078-c098-4978-a670-185e91a3e7fd)(content(Whitespace\" \
         \"))))(Tile((id 8ac3a743-baf1-474d-bba0-42d111271d1e)(label(case \
         end))(mold((out Exp)(in_(Rul))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         9c38e15c-cfa2-4eb9-a782-37bf908ce665)(content(Whitespace\" \
         \"))))(Tile((id \
         ed7f3700-3b57-4d5a-b8ef-e257c3cd8220)(label(shade))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         4a93f825-6745-468d-bb16-7f658be6d651)(content(Whitespace\"\\n\"))))(Tile((id \
         20163c28-7da1-472a-857d-ce83e0ad060a)(label(| =>))(mold((out \
         Rul)(in_(Pat))(nibs(((shape(Concave 46))(sort Exp))((shape(Concave \
         46))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         9ce46c45-de1e-46c0-88d9-b9da925df371)(content(Whitespace\" \
         \"))))(Tile((id \
         12964220-09ca-46cd-b36a-e28a273df648)(label(0))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         98bacf8e-2093-4471-bc31-3642cd3d72d5)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         ec8ebbc0-f22d-4a18-9228-61c31b9a486e)(content(Whitespace\" \
         \"))))(Tile((id \
         f97afaff-eff3-4139-9296-14fdb83be698)(label(1.0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         ed872275-5011-41da-bca3-e94b2999d8df)(content(Whitespace\"\\n\"))))(Tile((id \
         a0272c78-1048-44dd-af7f-7b9202951779)(label(| =>))(mold((out \
         Rul)(in_(Pat))(nibs(((shape(Concave 46))(sort Exp))((shape(Concave \
         46))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         127a425d-9790-4d5d-9010-1e034f2f429b)(content(Whitespace\" \
         \"))))(Tile((id \
         a4d570d8-4e68-4333-b7cc-0eba3f132cd4)(label(1))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         2bfb0c34-fbea-409c-9f95-1ce36eed5922)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         763ee2aa-4635-4e4d-b812-d3790ed00635)(content(Whitespace\" \
         \"))))(Tile((id \
         08422701-40d1-4af4-9500-a7b9714477a4)(label(0.9))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         b621632b-323a-494e-89d7-998ff3a59830)(content(Whitespace\"\\n\"))))(Tile((id \
         978f8e9e-05d5-4bd9-88d4-7260704f281e)(label(| =>))(mold((out \
         Rul)(in_(Pat))(nibs(((shape(Concave 46))(sort Exp))((shape(Concave \
         46))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         a398e3d7-ca37-48b6-8c37-c8c0ddffd63f)(content(Whitespace\" \
         \"))))(Tile((id \
         3df9818f-321e-4c9b-b5dd-61d2e43eb909)(label(2))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         7ea7c052-19b0-4076-b7ab-2c66a014a47a)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         3b7933cf-cbd3-4b57-9742-3dc103a0b8bf)(content(Whitespace\" \
         \"))))(Tile((id \
         69fde3c8-a763-4164-aa76-6e0cc05a3255)(label(0.75))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         47f15c52-d616-46db-9eb7-c6ab0adc1719)(content(Whitespace\"\\n\"))))(Tile((id \
         e3b4dc9e-ae42-4c71-b508-2e54bb939cc0)(label(| =>))(mold((out \
         Rul)(in_(Pat))(nibs(((shape(Concave 46))(sort Exp))((shape(Concave \
         46))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         45e7a078-7f90-43ab-b69d-77c43564e91c)(content(Whitespace\" \
         \"))))(Tile((id \
         6185e16d-d59c-4bcb-bee1-5dcd6d9ec972)(label(_))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         6e47ae9e-b7bf-4be1-84bc-a1cdec729193)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         8109c419-7056-435c-883d-fef65d6c1962)(content(Whitespace\" \
         \"))))(Tile((id \
         e8319cfa-9c69-4941-bf4f-fd6bc44df134)(label(0.6))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         5461a454-c2b4-457c-bd51-15e1cd4b753e)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         81420a84-863a-4404-b004-c5086066d37f)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         de79d13f-4c64-44e6-8ebf-e1134d809162)(content(Whitespace\"\\n\"))))(Secondary((id \
         c294cf7e-275b-46cf-baff-d3d04a29b41a)(content(Whitespace\"\\n\"))))(Tile((id \
         2dfd2b4d-3f1f-4b7f-84d4-7a22c1b9782a)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         a9271607-e52b-4b90-9fa0-c8c4a0f3ffad)(content(Whitespace\" \
         \"))))(Tile((id \
         75cb8afc-ac95-42e3-bac9-8c7413015939)(label(daily_water))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         aeb819e3-3823-4514-a814-2daf9e192a2d)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 24))(sort Pat))((shape(Concave \
         24))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         f42f4ac3-ec74-4c5f-b228-d256d9883bf5)(content(Whitespace\" \
         \"))))(Tile((id \
         5b81b452-2b1a-4425-8753-a7c0a2a149c5)(label(\"(\"\")\"))(mold((out \
         Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0 1))(children(((Tile((id \
         47e7583b-f983-4505-bed0-21bc7b1a0065)(label(Int))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         48e92404-6168-4351-a801-18740083ed7b)(label(,))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 44))(sort Typ))((shape(Concave \
         44))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         d52f008e-0be3-4b68-9aa0-aca44d31c22a)(content(Whitespace\" \
         \"))))(Tile((id \
         ceabb261-cc71-479b-a041-3d00016cc2b7)(label(Int))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         22a6939d-4684-4319-9ac7-02405d53631b)(label(,))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 44))(sort Typ))((shape(Concave \
         44))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         c3774869-9d7b-475a-9efa-64a35be55d43)(content(Whitespace\" \
         \"))))(Tile((id \
         fa86f04e-d75a-4671-8f35-43f30dd583a8)(label(MoonPhase))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children()))))))))(Secondary((id \
         73d1533e-4a51-40db-9dee-3c042f042f8a)(content(Whitespace\" \
         \"))))(Tile((id \
         93113962-9029-4bab-bb5a-b5b4abf82af2)(label(->))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 13))(sort Typ))((shape(Concave \
         13))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         01ba4b01-3412-4b22-9409-1a241a305285)(content(Whitespace\" \
         \"))))(Tile((id \
         3fcc7148-b13d-4327-a580-6c26b0c30805)(label(Int))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         08162bcf-7a89-4b20-b6cf-d9cfdac66673)(content(Whitespace\" \
         \")))))((Secondary((id \
         065ed1ec-6a18-4e9c-835f-86717bf27413)(content(Whitespace\"\\n\"))))(Tile((id \
         11302575-d630-4a56-aec2-65759fd50fad)(label(fun ->))(mold((out \
         Exp)(in_(Pat))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         37))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         8b2a7805-4f86-44ad-93f9-01e79cf8c379)(content(Whitespace\" \
         \"))))(Tile((id \
         c87105d5-77b5-420a-8740-6976fa5216ee)(label(\"(\"\")\"))(mold((out \
         Pat)(in_(Pat))(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0 1))(children(((Tile((id \
         c05af048-8b69-43f3-8a70-17023effbb82)(label(base))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         36df10e2-c6e6-46ee-8628-3d5d6f2c3f80)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 44))(sort Pat))((shape(Concave \
         44))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         e9274901-92f2-41c2-a89b-28e496b5cd23)(content(Whitespace\" \
         \"))))(Tile((id \
         c706ef3d-37f2-4978-81b7-428a349f0529)(label(shade))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         9eb0f385-07a3-491b-9ed2-095c62ab9c8a)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 44))(sort Pat))((shape(Concave \
         44))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         659f8a6e-be33-4887-882c-448dc7f831d2)(content(Whitespace\" \
         \"))))(Tile((id \
         39fa386a-a4c1-4dac-9ee3-19a6fbab65cd)(label(phase))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children()))))))))(Secondary((id \
         c51d8940-d3b8-4956-a14b-49875f519ed5)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         4da7bb1a-6591-4fa7-b5cb-f5b034835d21)(content(Whitespace\"\\n\"))))(Tile((id \
         c73d3ebd-5cbe-4457-a042-605ff52d9ca1)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         daf4bece-b85e-4f71-a1b7-e997e4f44dc6)(content(Whitespace\" \
         \"))))(Tile((id \
         40ed962e-590e-444d-a19f-77de58a19d7b)(label(base_f))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         89078f09-9484-49c7-b97b-674622dc366c)(content(Whitespace\" \
         \")))))((Secondary((id \
         53157c6f-50e3-4a1c-95f2-32f94df9dffc)(content(Whitespace\" \
         \"))))(Tile((id \
         eda91140-eb4e-4822-b44d-1d655696efe2)(label(float_of_int))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         26ab8ae8-5653-48c9-8cd1-7ed1cad0180f)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         96741728-82d9-4f60-9d48-935e97537093)(label(base))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         90671dcb-f17d-49bd-9336-a459ec1f86ff)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         7479df75-0cdb-4b92-890c-16239dfe0560)(content(Whitespace\"\\n\"))))(Tile((id \
         597e0799-0eff-42b6-a21b-b04522904aec)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         ef3af6b4-08e4-424c-aeb7-bbaea10b3eca)(content(Whitespace\" \
         \"))))(Tile((id \
         382a3f46-92de-4f35-956a-56d12de6c586)(label(phase_adj))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         f593419d-22b6-482f-bbd3-5d8098a7db4f)(content(Whitespace\" \
         \")))))((Secondary((id \
         8275f01d-c65f-49a4-bd2b-6de110b6c646)(content(Whitespace\" \
         \"))))(Tile((id \
         f8a14917-aa2d-4ac2-ae07-777dbb2d22d5)(label(base_f))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         4d5e7994-6407-4505-b55f-b01692519637)(content(Whitespace\" \
         \"))))(Tile((id \
         545004b7-0f49-4ef5-8427-6dc41d203f7b)(label(*.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 27))(sort Exp))((shape(Concave \
         27))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         7fe538fc-c9cb-4abc-a0f2-e82468b4e561)(content(Whitespace\" \
         \"))))(Tile((id \
         0b4d1c16-529b-4198-b587-1e6a919f1e5c)(label(phase_multiplier))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         c7486df3-5c20-45a2-abfe-fac2977680ca)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         7bf523a4-4f9a-41ac-b11b-9327c86ecc06)(label(phase))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         b8fdb4a4-5e92-4d98-ba1d-71fb35243d1d)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         234d942d-20a1-4416-a447-d836f148df69)(content(Whitespace\"\\n\"))))(Tile((id \
         0e0c07d4-27aa-40f4-8b9a-3c9469a5989a)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         efe0835d-5866-469c-bf26-623671537a75)(content(Whitespace\" \
         \"))))(Tile((id \
         ef4772f6-3083-4eec-8e05-c6722140ab6b)(label(shade_adj))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         6282c027-a9d6-4521-86b3-33ee40bfa9dd)(content(Whitespace\" \
         \")))))((Secondary((id \
         bdadef29-6f4b-4c6c-aa3a-001b11bfe2a2)(content(Whitespace\" \
         \"))))(Tile((id \
         ec68efac-643b-4318-98ce-f9e6121721ab)(label(phase_adj))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         e3b152bf-0269-454f-a3e6-acfefc78e16e)(content(Whitespace\" \
         \"))))(Tile((id \
         715dc97d-b4a3-468e-b579-50dce7db4809)(label(*.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 27))(sort Exp))((shape(Concave \
         27))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         c5db7af8-eb2a-420c-80e7-8dba27938117)(content(Whitespace\" \
         \"))))(Tile((id \
         25b3c266-051c-4b5a-af7a-09cc16cb950b)(label(shade_multiplier))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         6891c216-d8d9-42b9-ad9d-be64fbfc54eb)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         0292c302-2694-4dd9-b9ea-be605d204691)(label(shade))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         ae4d063b-34ca-4a66-867b-81a911a65780)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         8d600ccc-5695-4b7b-a0de-c2d9b9258844)(content(Whitespace\"\\n\"))))(Tile((id \
         5cdd0478-22e3-40ac-8a1d-ce290a24960c)(label(int_of_float))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         2ceafb04-1066-43a4-a6d3-0dc29519e46f)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         48c1f4a2-0432-4dc0-89f5-53d43ec05874)(label(shade_adj))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         d32bea6d-3a04-4013-972d-dcdf17cbeaa6)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         b59ab5af-1667-45f3-9de7-2b19b8dee474)(content(Whitespace\"\\n\"))))(Secondary((id \
         7f0d1604-fbdb-4a1a-839a-926a1777e773)(content(Whitespace\"\\n\"))))(Tile((id \
         97a5651c-7a8c-443c-9b8b-af5af143807e)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         72e8b6f5-37fc-4f40-b9b8-36ef28333e40)(content(Whitespace\" \
         \"))))(Tile((id \
         436934e8-cf2b-40e1-8f68-358eb3fc0257)(label(daily_water))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         f3ca074e-ea71-4bda-afaf-bd39a4fa8342)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         77c14b07-d3a7-4308-8757-c08958d16d25)(label(250))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         fb4e995d-1b91-4cc3-a392-54d914143c06)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         29f63158-e2ab-484f-a16a-fabada6acd84)(content(Whitespace\" \
         \"))))(Tile((id \
         19033452-359f-4ce9-8201-7be054e8adc4)(label(2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         e58d1185-5061-43c0-8e61-001c3f60aa74)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         e1c63a32-e8e7-4c18-9892-7de75755ed4b)(content(Whitespace\" \
         \"))))(Tile((id \
         f2689de3-9220-4e0d-b2da-711018b3f879)(label(Full))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         d8d45fa1-4488-4a14-93cc-40cbd5eef553)(content(Whitespace\" \
         \"))))(Tile((id \
         67e1c82a-6f68-4a7f-8350-035efcc223da)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         258c8c06-3a13-4565-8bbb-368f53e7f1f3)(content(Whitespace\" \
         \"))))(Tile((id \
         fea9edeb-0219-496b-bef1-dab58860f144)(label(165))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         f38551d9-45da-4044-ae18-da6c4f6c5497)(content(Whitespace\" \
         \")))))))))(Tile((id \
         5dcad44e-747b-4e97-9752-1cbe3660fd29)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         02587fcd-61d2-47e5-9d67-5d0865fd9fe9)(content(Whitespace\"\\n\"))))(Tile((id \
         750d8c78-aa63-4e10-a1f1-990d83d01cc2)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         8dd00522-4346-43ed-a380-869ecdb9cd7f)(content(Whitespace\" \
         \"))))(Tile((id \
         30b5213d-18e2-4334-9b98-934e9a43c447)(label(daily_water))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         500d9a06-681b-4406-8e4c-03b2307eab28)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         4753b707-8e99-4a70-bd54-629b3fc514a1)(label(50))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         f1bfde29-0ccf-4e2e-8d99-efc6b6a60e62)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         d018a4fe-3914-4af1-8df0-a59156c6cdd2)(content(Whitespace\" \
         \"))))(Tile((id \
         8cb12b3b-3db5-4981-a144-8785f4cfa4c0)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         65d0fd0c-d2c7-46f5-8e7d-f6c7ec34dbf4)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         2acd135b-8ddf-436d-b499-a7851fec80da)(content(Whitespace\" \
         \"))))(Tile((id \
         cc553491-23c0-4d6c-952c-2b758e41a7a2)(label(New))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         26a48f31-18c8-43fb-b552-0ab4cd5aa1cb)(content(Whitespace\" \
         \"))))(Tile((id \
         821fd5d1-12c6-4217-a503-48c15c1c3507)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         9f03bf3b-6093-4375-8766-1cac3267b11c)(content(Whitespace\" \
         \"))))(Tile((id \
         aafbcc51-c8f7-4455-83d2-dbe5628e77a5)(label(60))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         b037415f-a0da-47d7-9f17-37ca61774360)(content(Whitespace\" \
         \")))))))))(Tile((id \
         7438a22b-1f49-4f8e-82a0-40d87d229b62)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         6708dfc6-b6fd-4f7c-b720-51b264a637a8)(content(Whitespace\"\\n\"))))(Tile((id \
         110c1996-aa62-407e-9aad-9a306e0a89c3)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         ed441e33-66d1-475f-b128-aa24dfc9ec2e)(content(Whitespace\" \
         \"))))(Tile((id \
         9b59f8da-1c83-438c-bc43-bd3021ea2055)(label(daily_water))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         3c537020-f342-40dc-bf32-c4a23d534eca)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         5dd6f559-6198-4ae0-93fa-61a2fbd5c5a7)(label(180))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         96e0677a-8e88-4bdd-97df-9cfab63fc3c5)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         1772e705-bc04-4ada-91c9-dbffd7193ded)(content(Whitespace\" \
         \"))))(Tile((id \
         89e64e87-3c7e-4023-9d6a-0a58abb5aa87)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         29a3b3ac-e82c-4461-adcb-593101562e44)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         d03d2b25-4094-43c7-9446-4d9328dfd934)(content(Whitespace\" \
         \"))))(Tile((id \
         4bf20394-2ccd-4914-81fc-6e6f3d5e1181)(label(Waning))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         bf90b6f2-1ab0-449e-b68b-7915c641152c)(content(Whitespace\" \
         \"))))(Tile((id \
         78e1956c-8100-4cde-83c1-3d9291453ccd)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         f95a0adc-6ccb-46a7-ad28-4b8dea5cc8cb)(content(Whitespace\" \
         \"))))(Tile((id \
         31b6276f-9ef9-4888-856e-227dcb05fd12)(label(171))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         276a8330-547c-4fb3-a87f-8041951c50c5)(content(Whitespace\" \
         \")))))))))(Tile((id \
         d2755239-fb46-4dda-ab19-d673f2ad0960)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         30e3ce5a-7855-40d0-8f2b-1f5f432bc0ca)(content(Whitespace\"\\n\"))))(Tile((id \
         4cac4ca1-61da-4804-91b0-60ab3da6dae9)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         ff51d0a1-deec-4b8f-a663-60f8a82dbf46)(content(Whitespace\" \
         \"))))(Tile((id \
         a5db7e3a-4be4-42de-8b2f-daaf77e47efd)(label(daily_water))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         3820fd21-f822-43a7-9d79-de945bfa7691)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         9a8d0f83-e94b-4070-8bb8-7f04ad617444)(label(200))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         87776063-3956-4f4d-b29a-05d7e50bfb0f)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         fc18e3b4-1425-456f-90f7-0450111d6fe6)(content(Whitespace\" \
         \"))))(Tile((id \
         045a5170-8354-4f72-81c8-544cf55ab588)(label(2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         29615e1d-b5a2-43e9-9272-c88e066e70e7)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         d06b9f02-79ee-469b-818b-19300d8c89f6)(content(Whitespace\" \
         \"))))(Tile((id \
         b66d36a5-8de1-42a5-8853-db21fa75fb6a)(label(Waxing))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         d5d22cca-30e4-4437-bf74-805b476a39b5)(content(Whitespace\" \
         \"))))(Tile((id \
         ddd8a8c6-3ad7-42f9-a04c-2165d6241710)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         8ac927b6-18c8-490c-853a-d5a99ccbedf6)(content(Whitespace\" \
         \"))))(Tile((id \
         6665cf11-e9b1-434b-a576-957de0218b7d)(label(165))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         ef53e1af-fe9b-4e06-8be4-d0ab50f3d540)(content(Whitespace\" \
         \")))))))))(Tile((id \
         a10754a1-4895-49dd-8c7e-3c9485bff879)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         c8c3226c-09ba-4c49-acaa-e7e437a8ed17)(content(Whitespace\"\\n\"))))(Secondary((id \
         bb0bc231-e930-4c59-9320-2d3d0d406858)(content(Whitespace\"\\n\"))))(Tile((id \
         b198b9a0-32c9-4c23-8d24-4a9c89989b19)(label(daily_water))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         460aa73d-e57f-4d1b-b73a-bcc4876c3136)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         0c4e0913-7acd-41cb-8f4b-eabed50384b9)(label(100))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         526ac66f-6fdf-4e88-863d-f8f42c2ef0a5)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         32cdd74f-32fd-444b-8050-2953a27cebac)(content(Whitespace\" \
         \"))))(Tile((id \
         d9e3192c-f123-45cf-9a8d-637408634299)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         a42c25e8-357b-4bab-898e-4365055517dc)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         cf5ecc72-12b8-4ac4-a60a-c65823cf87cb)(content(Whitespace\" \
         \"))))(Tile((id \
         c0a100d1-3b42-4f3f-b8cd-c6374e016783)(label(Full))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         0d555c1b-c8a0-4382-b1a7-4a919fda58de)(content(Whitespace\"\\n\"))))(Secondary((id \
         5b5d3641-88fd-4ef2-ad7d-78f6c0d5cf20)(content(Whitespace\"\\n\"))))(Secondary((id \
         154393d1-6a94-45d7-ae01-1c3beba26a20)(content(Comment\"# END OF PART \
         5 - Select the next slide from the top menu #\"))))(Secondary((id \
         2d2e2173-b18d-4794-95fc-2dba665cc7b5)(content(Whitespace\"\\n\")))))";
      backup_text =
        "# PROBES TUTORIAL - PART 5: STEP INTO AND THE DYNAMIC CURSOR BAR #\n\
         # You've pinned a call and can see values inside a function. #\n\
         # But what if the bug is deeper, inside a function that your #\n\
         # function calls? Step Into follows the call stack down. #\n\n\
         # TRY THIS: #\n\
         # 1. Turn on auto-probe and click inside `daily_water` #\n\
         # 2. Pin one of the test calls (click a sample > Pin) #\n\
         # 3. Now add a probe to the `phase_multiplier(phase)` call #\n\
         #    inside `daily_water` (click on `phase_multiplier`) #\n\
         # 4. Click that sample and choose \"Step Into\" from the #\n\
         #    dropdown (or press Enter) #\n\
         # 5. Your cursor jumps into `phase_multiplier`! The probes #\n\
         #    there show only values from your pinned context. #\n\n\
         # THE DYNAMIC CURSOR BAR #\n\
         # Look at the bar at the top of the editor after stepping in. #\n\
         # It shows your position in the call stack as breadcrumbs: #\n\
         #   top-level > daily_water > phase_multiplier #\n\
         # Click a function name to jump to its definition. #\n\
         # Click a chevron (>) to jump to the call site. #\n\
         # This lets you move up and down the call stack freely. #\n\n\
         type MoonPhase = + New + Waxing + Full + Waning in\n\n\
         let phase_multiplier: MoonPhase -> Float =\n\
         fun phase -> case phase\n\
         | New => 1.2\n\
         | Waxing => 1.1\n\
         | Full => 0.88\n\
         | Waning => 0.95\n\
         end\n\
         in\n\n\
         let shade_multiplier: Int -> Float =\n\
         fun shade -> case shade\n\
         | 0 => 1.0\n\
         | 1 => 0.9\n\
         | 2 => 0.75\n\
         | _ => 0.6\n\
         end\n\
         in\n\n\
         let daily_water: (Int, Int, MoonPhase) -> Int =\n\
         fun (base, shade, phase) ->\n\
         let base_f = float_of_int(base) in\n\
         let phase_adj = base_f *. phase_multiplier(phase) in\n\
         let shade_adj = phase_adj *. shade_multiplier(shade) in\n\
         int_of_float(shade_adj)\n\
         in\n\n\
         test daily_water(250, 2, Full) == 165 end;\n\
         test daily_water(50, 0, New) == 60 end;\n\
         test daily_water(180, 0, Waning) == 171 end;\n\
         test daily_water(200, 2, Waxing) == 165 end;\n\n\
         daily_water(100, 1, Full)\n\n\
         # END OF PART 5 - Select the next slide from the top menu #\n";
      refractors = "()";
    } )
