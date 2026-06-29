let out : string * Haz3lcore.PersistentSegment.t =
  ( "Study / writing / garden-survey / garden-survey-sketch",
    {
      segment =
        "((Secondary((id \
         f0c7dcb9-84ef-4a31-9530-c3fc79efab95)(content(Comment\"# Garden \
         Survey Notes                                  #\"))))(Secondary((id \
         c89c82d1-10d3-4b87-a506-944f3b71791f)(content(Whitespace\"\\n\"))))(Secondary((id \
         40f2897b-317e-469e-908d-473886ee3a4c)(content(Comment\"#                                                      \
         #\"))))(Secondary((id \
         f2c14940-b524-4294-89fa-c71a0b4b1c14)(content(Whitespace\"\\n\"))))(Secondary((id \
         ef656f01-1904-4670-9fb1-13001fdffc82)(content(Comment\"# After each \
         moonlit garden walk, visitors fill out    #\"))))(Secondary((id \
         98879be2-de9c-428b-8864-f3584ff3d563)(content(Whitespace\"\\n\"))))(Secondary((id \
         170b0c04-a7a9-4b37-a4e8-470e003a77a2)(content(Comment\"# a short \
         survey. Entries look like:                   #\"))))(Secondary((id \
         fc389082-a743-4dd7-8835-8862ac0e19ba)(content(Whitespace\"\\n\"))))(Secondary((id \
         7070c916-c4c5-441f-b3de-170d35e77a0a)(content(Comment\"#   \\\"Q1: \
         yes -- the moonbloom beds were stunning\\\"      \
         #\"))))(Secondary((id \
         aec22759-c882-4783-949c-ee971f79f281)(content(Whitespace\"\\n\"))))(Secondary((id \
         a83c1930-d41a-4ffb-adb3-92b338514521)(content(Comment\"#   \\\"Q2: no \
         -- too many weeds near the starfern\\\"       #\"))))(Secondary((id \
         a0682793-a514-4ed1-aa16-95b4a34d4c32)(content(Whitespace\"\\n\"))))(Secondary((id \
         54082f0b-cffd-40cf-986f-b15e46c99b1f)(content(Comment\"#                                                      \
         #\"))))(Secondary((id \
         e5db3089-402d-402c-a8a3-e066aecf512f)(content(Whitespace\"\\n\"))))(Secondary((id \
         36390476-051c-4b53-a9da-4776f096c8b6)(content(Comment\"# Extract just \
         the notes from positive responses.      #\"))))(Secondary((id \
         2ea0aa74-23e6-4f51-8408-58d8b9173e25)(content(Whitespace\"\\n\"))))(Secondary((id \
         7b85d06d-ce56-4cee-9dc5-6c0233d0d3b6)(content(Comment\"# For the data \
         below, the result should be:            #\"))))(Secondary((id \
         758478eb-7aba-4c2e-8f9a-e9da74ef00a0)(content(Whitespace\"\\n\"))))(Secondary((id \
         3511b2e1-69d4-4de4-aef0-712a47ced337)(content(Comment\"#   [\\\"the \
         moonbloom beds were stunning\\\",               #\"))))(Secondary((id \
         8796e6fd-5bc7-4bc0-8f5c-29c44033b69b)(content(Whitespace\"\\n\"))))(Secondary((id \
         2a4f1654-6359-4a18-a3bf-2bc15b259416)(content(Comment\"#    \\\"loved \
         the new duskrose pathway\\\",                 #\"))))(Secondary((id \
         2e02a1dd-b60b-4542-aaa3-1bce38f8cb33)(content(Whitespace\"\\n\"))))(Secondary((id \
         dc35ac91-1a38-4bd1-8740-afcedf3e3fb6)(content(Comment\"#    \\\"the \
         nightshade corner was magical\\\"]              #\"))))(Secondary((id \
         0ee02552-3c7c-4b18-b05f-eea478c5613b)(content(Whitespace\"\\n\"))))(Secondary((id \
         03bb2379-5839-4c95-82b6-5314d937ab67)(content(Comment\"#                                                      \
         #\"))))(Secondary((id \
         bb5cbe56-6ab7-4729-b313-5654bdee6ec0)(content(Whitespace\"\\n\"))))(Secondary((id \
         ccaf06e2-a628-4caa-901c-b5cb29761d4e)(content(Comment\"# \
         Steps:                                               \
         #\"))))(Secondary((id \
         1cd9c063-cfe0-498d-9eb4-886d0f60e430)(content(Whitespace\"\\n\"))))(Secondary((id \
         069b3b37-361f-4ab0-8d81-6edded87ce36)(content(Comment\"#   1. \
         is_positive: check if a response is \\\"yes\\\"       \
         #\"))))(Secondary((id \
         41c58b1b-a735-4e11-85e9-7c23b73c861f)(content(Whitespace\"\\n\"))))(Secondary((id \
         c42ca67a-9cb8-42e7-9716-b7df25d31025)(content(Comment\"#   2. \
         extract_note: get the text after the --         #\"))))(Secondary((id \
         f02f499a-8426-444f-a87b-006955d2e0df)(content(Whitespace\"\\n\"))))(Secondary((id \
         e761701d-2e2d-44ee-b52d-4b5a41473cb0)(content(Comment\"#   3. \
         positive_notes: filter then extract             #\"))))(Secondary((id \
         5600a7f6-3ba4-4411-9fe2-6eb348012978)(content(Whitespace\"\\n\"))))(Secondary((id \
         6cd6aa3b-ad44-4085-9ea5-937b5b55de25)(content(Comment\"#                                                      \
         #\"))))(Secondary((id \
         e0c0e05f-08a9-4135-9ccb-e02c4daa929e)(content(Whitespace\"\\n\"))))(Secondary((id \
         990319c9-f59c-429e-8d3e-a615f3b09bc4)(content(Comment\"# Available \
         functions:                                 #\"))))(Secondary((id \
         af93e0cc-4d7a-4a07-b7a3-3193dc708a9c)(content(Whitespace\"\\n\"))))(Secondary((id \
         fa90a525-c536-4504-aa39-f534a2d36fb4)(content(Comment\"#   \
         string_match: (String, String) -> Bool             \
         #\"))))(Secondary((id \
         40d2bded-622e-4778-8b29-184973f0aff3)(content(Whitespace\"\\n\"))))(Secondary((id \
         18404c8e-fbf8-4fa7-b54c-91239e35f562)(content(Comment\"#   \
         string_split: (String, String) -> [String]         \
         #\"))))(Secondary((id \
         99479efb-68ca-4102-9f92-e0b588697abc)(content(Whitespace\"\\n\"))))(Secondary((id \
         3187b922-b9b7-4575-b3dc-d760b48a8efd)(content(Comment\"#   \
         string_trim: String -> String                      \
         #\"))))(Secondary((id \
         543794eb-3e2f-47a7-afde-fa3ca3dadd3b)(content(Whitespace\"\\n\"))))(Secondary((id \
         ec3bbfe1-04a2-490e-8e9f-40ad02cc7c8e)(content(Comment\"#   nth: ([?], \
         Int) -> ?                               #\"))))(Secondary((id \
         6fc6bba8-2e3b-4119-b0f3-9a08abcc30bb)(content(Whitespace\"\\n\"))))(Secondary((id \
         f3879e94-a5ce-4043-9186-f19e04af82fb)(content(Comment\"#   filter: \
         ([?], ? -> Bool) -> [?]                    #\"))))(Secondary((id \
         4260e2b6-64c6-45da-9df7-5c68935abbf3)(content(Whitespace\"\\n\"))))(Secondary((id \
         c30abf42-240b-4144-a918-179836c00ec7)(content(Comment\"#   map: ([?], \
         ? -> ?) -> [?]                          #\"))))(Secondary((id \
         4f63ea99-4f28-4fdc-b2ac-8a2d4666aed9)(content(Whitespace\"\\n\"))))(Secondary((id \
         46728574-0ff7-45e2-b0e1-75d691126fcf)(content(Comment\"#                                                      \
         #\"))))(Secondary((id \
         96e7b6ab-70fc-4350-82ae-89b9554ddde8)(content(Whitespace\"\\n\"))))(Secondary((id \
         db39964a-834b-4fbb-8350-ab3aacd02026)(content(Comment\"# string_match \
         checks whether a regex pattern          #\"))))(Secondary((id \
         8bcb9f99-9d21-4f4c-a9a9-da3879ba044f)(content(Whitespace\"\\n\"))))(Secondary((id \
         73b9bee2-ece5-4c76-b240-8803574ecbd2)(content(Comment\"# appears \
         anywhere in the string. Anywhere!            #\"))))(Secondary((id \
         719259a7-8bee-4685-8c6a-6bb97c58cedc)(content(Whitespace\"\\n\"))))(Secondary((id \
         2553d23d-0c4f-4a4a-ae3c-bd67e845b75b)(content(Comment\"#                                                      \
         #\"))))(Secondary((id \
         b3a32ca7-feaf-4fab-8c63-336cb3f91c7a)(content(Whitespace\"\\n\"))))(Secondary((id \
         e2d770d8-3458-4208-9466-12dbf6539591)(content(Comment\"# Tip: Probe \
         each function with the test data          #\"))))(Secondary((id \
         56812e24-d449-4bf5-a7e6-1d3c21e18ca9)(content(Whitespace\"\\n\"))))(Secondary((id \
         be25b26e-cb9a-4a2a-9068-fa372752813c)(content(Comment\"# before \
         combining them. Regex substring matching      #\"))))(Secondary((id \
         ef7c5d8c-4a2f-4aa8-a4d2-a491caac7afd)(content(Whitespace\"\\n\"))))(Secondary((id \
         35225807-4f43-417e-ba7a-dbec204cee19)(content(Comment\"# can be \
         surprising -- check carefully!                #\"))))(Secondary((id \
         5e1a2f97-033f-47c4-933d-22e9de7937d1)(content(Whitespace\"\\n\"))))(Secondary((id \
         309b72cd-dcbf-4dd1-9b7b-d285b69c85b4)(content(Whitespace\"\\n\"))))(Tile((id \
         e016f9a2-c693-4517-960e-0defd887380b)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         a8b6a6e6-a039-489b-9d52-7e3139261572)(content(Whitespace\" \
         \"))))(Tile((id \
         6081070a-edae-4e65-82fc-5cb1862ac763)(label(entries))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         83f37ba0-806b-4fd6-859b-7df5db9916ec)(content(Whitespace\" \
         \")))))((Secondary((id \
         b9ba4203-4ce3-4377-b78d-34ab1b7fdee6)(content(Whitespace\" \
         \"))))(Tile((id d0834840-a5d6-4d8d-8718-b2cda0b70c6e)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         e74001d9-2543-47e5-8992-6af07f1e756b)(content(Whitespace\"\\n\"))))(Tile((id \
         fc7674c4-9408-4644-ae79-41d8229baa82)(label(\"\\\"Q1: yes -- the \
         moonbloom beds were stunning\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         1ee87536-d7d9-4f02-93f9-7cdbbfb0e8b6)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         2dc80971-3b42-4a7c-9737-c47a2e1a29c3)(content(Whitespace\"\\n\"))))(Tile((id \
         efba0f2a-fddd-4ae0-90df-0edab289e6fc)(label(\"\\\"Q2: no -- too many \
         weeds near the starfern\\\"\"))(mold((out Exp)(in_())(nibs(((shape \
         Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         a78dcb0c-1b4b-4c0a-898b-caa61f9e1261)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         f1abdc10-1404-4022-91d4-898f870661e1)(content(Whitespace\"\\n\"))))(Tile((id \
         97ae9e5a-9d27-459c-b5fb-327cfc80c68e)(label(\"\\\"Q3: yes -- loved \
         the new duskrose pathway\\\"\"))(mold((out Exp)(in_())(nibs(((shape \
         Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         1c39070b-161d-4168-b926-f0d2e9dda143)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         3f164018-1d35-4ba3-8d99-419529444ee4)(content(Whitespace\"\\n\"))))(Tile((id \
         7a92f4a4-66c6-4e7c-903b-3fddca1b30e9)(label(\"\\\"Q4: no -- \
         yesterday's rain left puddles\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         1a07eb50-73db-4d96-bb99-fed71aeba7a2)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         64b3fedb-f01f-4aec-bb0f-e4444321649e)(content(Whitespace\"\\n\"))))(Tile((id \
         9cc11915-8e21-4661-bcf1-d715dd6ab9c5)(label(\"\\\"Q5: yes -- the \
         nightshade corner was magical\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         fb78a5a0-05c1-49f8-9da2-9fe4e9b05470)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         cd37ef34-440f-44c6-855c-980a0700fa16)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         38138745-38b1-47b7-9f59-362339c8c17c)(content(Whitespace\"\\n\"))))(Secondary((id \
         b4362bc7-331e-42bd-94d4-4175e098ca6a)(content(Whitespace\"\\n\"))))(Secondary((id \
         2d2329d9-5736-4797-ba88-decffbe149e3)(content(Comment\"# Does this \
         survey entry have a positive response? #\"))))(Secondary((id \
         8c77ade0-b85a-450c-a5f2-6252d9e4ee3d)(content(Whitespace\"\\n\"))))(Tile((id \
         f0bbaff5-4464-4f14-bb44-3b953370020a)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         b61cb595-eff3-4100-8173-59055c535c9a)(content(Whitespace\" \
         \"))))(Tile((id \
         220d974a-7bab-4d3a-bb0e-a9e8d2d65f61)(label(is_positive))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         edabbf94-e484-4ac7-ae8b-b8241b6b85f7)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 24))(sort Pat))((shape(Concave \
         24))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         960c40e4-8a4b-467e-bcbf-b90b14fbd63e)(content(Whitespace\" \
         \"))))(Tile((id \
         5d36b30f-109a-4e3c-840c-1e69942e2e4e)(label(String))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         0bdf3098-b0a1-4db3-8ece-18f5b3ba868b)(content(Whitespace\" \
         \"))))(Tile((id \
         ea5af377-4c75-4547-bd26-8913ae5af21e)(label(->))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 13))(sort Typ))((shape(Concave \
         13))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         82cb1dd5-475f-4273-85f4-0fe8ecdef17a)(content(Whitespace\" \
         \"))))(Tile((id \
         0f9efe02-f006-45f2-9f95-90ade8274812)(label(Bool))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         7f29d61d-aa84-4aa3-90ee-4cd669115e1f)(content(Whitespace\" \
         \")))))((Secondary((id \
         d6ce08f8-f4a3-4bac-a2e6-9ee6f1d894d9)(content(Whitespace\" \
         \"))))(Tile((id b3a62f48-d62a-4154-8a33-0102c2bc526f)(label(fun \
         ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 37))(sort Exp))))))(shards(0 \
         1))(children(((Secondary((id \
         bf3e6450-0ec5-4046-b224-6f7337ca7a46)(content(Whitespace\" \
         \"))))(Tile((id \
         5867db13-fd72-4fc3-a7e3-a837a5adf6fa)(label(entry))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         84d1dd92-61fb-450b-89a8-da23f3a58975)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         3834b6f5-579e-4cce-a302-355cae6b8e50)(content(Whitespace\"\\n\"))))(Tile((id \
         f3682f29-5806-4d51-a784-398653588a02)(label(?))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         45fc648b-1489-4da7-8ff9-73683990b6a8)(content(Whitespace\"\\n\"))))(Secondary((id \
         047667ba-b56a-4865-a3d8-cc7c43869a15)(content(Whitespace\"\\n\"))))(Secondary((id \
         d7ace252-e19e-42a6-b22e-e196183d0adb)(content(Whitespace\"\\n\"))))(Secondary((id \
         de0e380d-26bc-480b-90a3-0734128d3ead)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         099d1394-ceff-4118-83ab-5b916d59770e)(content(Whitespace\"\\n\"))))(Secondary((id \
         67fc8ae1-b3ed-4ee2-b34f-33f617d8c8dc)(content(Whitespace\"\\n\"))))(Secondary((id \
         ddab89cb-867a-4284-90b3-4662759ef104)(content(Comment\"# Extract the \
         note text from a survey entry #\"))))(Secondary((id \
         b5daf281-a3b3-4f98-8470-c4a6706f34a3)(content(Whitespace\"\\n\"))))(Tile((id \
         8e19ba5d-9b60-414e-a511-7ad95d0c3bbb)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         3bd2c956-a42b-4888-8d3f-d2ec6844f076)(content(Whitespace\" \
         \"))))(Tile((id \
         e44c41e4-a8f9-4461-8b99-78db53d4484f)(label(extract_note))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         24a7816b-257d-45f1-af01-af6d0ef9b07c)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 24))(sort Pat))((shape(Concave \
         24))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         ab7146f1-3141-42b7-aa3f-4685e18ae2d3)(content(Whitespace\" \
         \"))))(Tile((id \
         2903ef15-e97c-4521-8d17-c7d26a2776f9)(label(String))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         8c2601c9-d5b4-43b0-b05e-64c377c92f48)(content(Whitespace\" \
         \"))))(Tile((id \
         319531ec-2900-4e0b-ba3c-7e58de83403e)(label(->))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 13))(sort Typ))((shape(Concave \
         13))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         0a4ba1ea-1533-41b1-9545-d29cc3c20c8c)(content(Whitespace\" \
         \"))))(Tile((id \
         c8573f11-a80a-41c8-b8ba-63fff7fb31aa)(label(String))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         a5093801-d106-455b-9b16-c06e488f878d)(content(Whitespace\" \
         \")))))((Secondary((id \
         46527de6-3a39-4fb8-9b91-11c489720d16)(content(Whitespace\" \
         \"))))(Tile((id 1630aa7f-c78b-4b33-91b2-256ca5c0cdd3)(label(fun \
         ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 37))(sort Exp))))))(shards(0 \
         1))(children(((Secondary((id \
         8ef8015d-933b-4e87-9e96-ea1db1d4b88d)(content(Whitespace\" \
         \"))))(Tile((id \
         4f939cac-70cb-430a-9c35-a130c70cde77)(label(entry))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         82691ed8-a3df-4ee9-a179-ff2b24419ba6)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         bfdf6125-255c-459f-99da-c1ec3660b563)(content(Whitespace\"\\n\"))))(Tile((id \
         e507c652-ba1f-41ca-9e98-ff4a2c42321b)(label(?))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         e4decb5e-9cf1-4751-b559-cdf1256132ee)(content(Whitespace\"\\n\"))))(Secondary((id \
         11e65a7d-e344-499e-834d-0cd6721a2f33)(content(Whitespace\"\\n\"))))(Secondary((id \
         8f0a0fa0-85f3-4788-845c-f37a07b30188)(content(Whitespace\"\\n\"))))(Secondary((id \
         5a5eeb63-d253-4216-aa1f-e0505ef49a23)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         5aca4b3d-4590-4082-b844-1245e6f71689)(content(Whitespace\"\\n\"))))(Secondary((id \
         3f361c0a-f327-49dc-aafe-59e829eed6b5)(content(Whitespace\"\\n\"))))(Secondary((id \
         342a39a3-02c6-428a-b1ba-cbb3f4952f58)(content(Comment\"# Get only the \
         notes from positive responses #\"))))(Secondary((id \
         dc1b1884-b728-4829-9fb3-61178180acdb)(content(Whitespace\"\\n\"))))(Tile((id \
         e21d710e-21a7-442b-b05c-019cdd67321e)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         63aeb16d-6878-47d5-9c44-bc368cc1b82f)(content(Whitespace\" \
         \"))))(Tile((id \
         67d35d5a-e639-4e9c-84b1-a5f7ee995489)(label(positive_notes))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         9c0b1d43-2bf3-4143-9fdc-5190267f5759)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 24))(sort Pat))((shape(Concave \
         24))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         fb965fb5-ee93-4f63-a0a3-2b8af97d6a3a)(content(Whitespace\" \
         \"))))(Tile((id 21e4a4bd-ecf6-48eb-920c-b18c967f3658)(label([ \
         ]))(mold((out Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape \
         Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
         a4c1d221-a7fb-4aaf-9d62-3777fd98c036)(label(String))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children()))))))))(Secondary((id \
         c50685b7-d185-4c83-ade7-9e1adfe4b1d2)(content(Whitespace\" \
         \"))))(Tile((id \
         35e7a403-207d-4c3a-9bb3-7811f1aa65ba)(label(->))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 13))(sort Typ))((shape(Concave \
         13))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         5b21b927-5059-4d86-9d4d-f743c9b17bc1)(content(Whitespace\" \
         \"))))(Tile((id b6a5149c-12b3-4819-b62a-b99fc56fee0b)(label([ \
         ]))(mold((out Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape \
         Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
         ee2cfc6f-4f1b-4065-a2f9-d075e1da1905)(label(String))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children()))))))))(Secondary((id \
         aca119a8-9e33-4e6f-b7ab-cc9af1fb9061)(content(Whitespace\" \
         \")))))((Secondary((id \
         3dd32ef1-266c-40b6-b40b-804dadafe94c)(content(Whitespace\" \
         \"))))(Tile((id 3177cede-5a42-44d7-87fb-9eb71a3a0d43)(label(fun \
         ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 37))(sort Exp))))))(shards(0 \
         1))(children(((Secondary((id \
         bec8c2f5-3117-4121-a4c7-379d0d77f1fd)(content(Whitespace\" \
         \"))))(Tile((id \
         bef526e2-0bc3-404c-8e70-010b4c9b81bd)(label(entries))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         dce6d995-723b-4242-8a36-40e6b252cd9f)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         650ce6db-0b26-45cb-bd4f-81709f94d926)(content(Whitespace\"\\n\"))))(Tile((id \
         1eb225f6-7b90-491f-83d7-742ac86a491e)(label(?))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         e74a03ed-8723-43cf-91cd-7a4003e124d2)(content(Whitespace\"\\n\"))))(Secondary((id \
         ee5c66a3-d0b3-47a4-b56f-fb6f4a93b0de)(content(Whitespace\"\\n\"))))(Secondary((id \
         02316410-c716-460f-b778-de97e7f87d7d)(content(Whitespace\"\\n\"))))(Secondary((id \
         801777b5-656e-4533-9a23-fb9f33e89e28)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         e4a25bed-0bfe-4cb5-987f-681bd4584d67)(content(Whitespace\"\\n\"))))(Secondary((id \
         b9f14c93-f560-4a8c-a66d-1880c8cb908a)(content(Whitespace\"\\n\"))))(Tile((id \
         c52e5a03-6b1d-4893-b745-ceceff30b8ba)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         d7fa4dee-874d-4a2a-9842-aea4eccdf3bc)(content(Whitespace\" \
         \"))))(Tile((id \
         5a228fec-ed11-4815-81f2-261ab534c920)(label(positive_notes))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         9002dbb9-48bf-4db7-8422-23320004c87e)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         94295483-30c2-42cd-a597-763959969854)(label(entries))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         220e0185-bb18-42ba-b697-ba69f5d91901)(content(Whitespace\" \
         \"))))(Tile((id \
         3f720ec3-01cb-4c35-8963-4899baada282)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         ddb14417-8ac0-4545-b5c0-41c84cdc8914)(content(Whitespace\" \
         \"))))(Tile((id e263528b-aad2-4a78-b740-2182959a304a)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         1d7fd043-4b14-44c6-86e8-c968fa345181)(content(Whitespace\"\\n\"))))(Tile((id \
         8e90e1b9-6e51-45c6-bb68-cd1711e881a9)(label(\"\\\"the moonbloom beds \
         were stunning\\\"\"))(mold((out Exp)(in_())(nibs(((shape Convex)(sort \
         Exp))((shape Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
         41d9aefc-ab97-4ac3-aa1d-c66814c8fe4a)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         5de7a202-9ab7-40e1-960f-0f19195a6d86)(content(Whitespace\"\\n\"))))(Tile((id \
         bfc0d3f7-be35-4a72-982f-db639f9da488)(label(\"\\\"loved the new \
         duskrose pathway\\\"\"))(mold((out Exp)(in_())(nibs(((shape \
         Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         fd387906-56a7-474b-af36-1e5ea6bb4222)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         e5301800-b0a0-4c9f-9329-737a9dd21007)(content(Whitespace\"\\n\"))))(Tile((id \
         68285071-50c2-42d3-a099-257b1c48ed4a)(label(\"\\\"the nightshade \
         corner was magical\\\"\"))(mold((out Exp)(in_())(nibs(((shape \
         Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         642e22a8-e06f-45d5-9a01-57c679abf1c6)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         5a13d627-78aa-4243-bb68-dc39a7cf2dbc)(content(Whitespace\" \
         \")))))))))(Tile((id \
         5125c144-52ef-47c2-adaf-188dbcab0a7f)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         df80fdc2-f0f6-4119-bd05-567866083d80)(content(Whitespace\"\\n\"))))(Secondary((id \
         60404e71-9f66-4eeb-92a9-59918463c463)(content(Whitespace\"\\n\"))))(Tile((id \
         8f20b57f-6a45-43b6-9a6b-a935f2f37aa1)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         4f05bdfa-29c3-4a28-b0a0-e3f77d62052a)(content(Whitespace\" \
         \"))))(Tile((id \
         e361aea7-9f10-47d7-9efd-a4e67677b377)(label(is_positive))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         1c9f7e46-216f-469e-a119-f07c9062b437)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         6f07ceca-f70b-4589-8c73-07ddcff90b49)(label(\"\\\"Q1: yes -- \
         moonblooms lovely\\\"\"))(mold((out Exp)(in_())(nibs(((shape \
         Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         7eb53670-9de5-4be5-948c-6dc692dd74d0)(content(Whitespace\" \
         \"))))(Tile((id \
         df7d430b-ff60-4428-aba8-a9a424de382c)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         0047c6f7-8d64-4605-840c-f84a647f1ed4)(content(Whitespace\" \
         \"))))(Tile((id \
         f3afe54a-a689-4fbd-b5f0-636b1427a177)(label(true))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         687b5fca-52ab-4d0c-9cbb-0d04f291e212)(content(Whitespace\" \
         \")))))))))(Tile((id \
         82ca39b4-a620-4e75-b68a-310a801e91c3)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         e6fcd662-f8e7-47fc-a5f3-d2d260074a84)(content(Whitespace\"\\n\"))))(Secondary((id \
         d811ae82-0fc9-43e7-a476-34f589b3c896)(content(Whitespace\"\\n\"))))(Tile((id \
         3015c5ad-b60e-4c86-83df-3a04480667ff)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         4bc31eec-d242-4ad3-a807-1a53b5bb6568)(content(Whitespace\" \
         \"))))(Tile((id \
         6a14a51a-3eee-4040-a5dd-a9e61ec3b5d3)(label(is_positive))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         d994fea7-8e14-4a14-834a-8c2bac713d8d)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         2b9dfaf7-299f-48a6-987d-cf9870c70340)(label(\"\\\"Q2: no -- needs \
         more starfern\\\"\"))(mold((out Exp)(in_())(nibs(((shape Convex)(sort \
         Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         98d99183-dc21-4cee-94eb-8eba8aeed5af)(content(Whitespace\" \
         \"))))(Tile((id \
         89dc8b7c-0cd9-4fad-bf28-5f986227738e)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         14a3227a-901b-4bd7-936e-549f9d89a9ee)(content(Whitespace\" \
         \"))))(Tile((id \
         cf198160-da17-433e-b0fb-8d8dbe34fbed)(label(false))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         16a7198e-bcd6-48a1-81f7-1e47013c30d2)(content(Whitespace\" \
         \")))))))))(Tile((id \
         ecc15230-d28a-4d32-9a58-667fdd062bdb)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         d9c82ddc-c0d6-4063-a554-99ef7fce9057)(content(Whitespace\"\\n\"))))(Secondary((id \
         25527057-ad9f-4e54-831d-7a8d65a257e6)(content(Whitespace\"\\n\"))))(Tile((id \
         2cbe24e8-c8b7-4984-8f4f-318b766384a3)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         0bfd305e-2686-4ef7-954e-db9707e8457d)(content(Whitespace\" \
         \"))))(Tile((id \
         0a40bb33-3d92-47ef-89ed-35ac4fd950ab)(label(is_positive))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         fc3f94f9-5c7c-4238-b4c9-c227608d85f2)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         bcc3df1c-1235-4a97-8b97-6809376bd408)(label(\"\\\"Q4: no -- yesterday \
         was rainy\\\"\"))(mold((out Exp)(in_())(nibs(((shape Convex)(sort \
         Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         c25566d8-18b4-4d81-8536-19d3604bab41)(content(Whitespace\" \
         \"))))(Tile((id \
         b639b2a1-42f3-47d8-8bf8-25913b1fa838)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         a6c99b55-0c24-4a21-b1a3-f2a3d5eab8c1)(content(Whitespace\" \
         \"))))(Tile((id \
         b064b1b6-2b50-4a86-b2c3-687f52a052c0)(label(false))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         3f23d5df-5a83-4413-af1a-3aa1f5c49075)(content(Whitespace\" \
         \")))))))))(Tile((id \
         08d14cb3-6589-43e2-b08f-94504b1b26c5)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         2231ef36-c0c9-41da-901a-8c07a28a27b1)(content(Whitespace\"\\n\"))))(Secondary((id \
         950c232f-19ed-4f55-81af-41b962ea4430)(content(Whitespace\"\\n\"))))(Tile((id \
         811bb45e-0b55-45ff-8bf2-6a04586aa2cd)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         bfad374b-0d8f-4fe5-b9c8-c64da29aec89)(content(Whitespace\" \
         \"))))(Tile((id \
         5a447a99-abd2-4f9e-97e1-8963d80ee448)(label(extract_note))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         1a8f8897-b332-431a-9c3f-48fdf76c2989)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         20b16ed9-1ac3-4565-b667-bc9204698668)(label(\"\\\"Q3: yes -- duskrose \
         pathway\\\"\"))(mold((out Exp)(in_())(nibs(((shape Convex)(sort \
         Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         7c448361-058a-4fa8-98a1-3545b7e93e41)(content(Whitespace\"\\n\"))))(Tile((id \
         d88a5702-d343-4a2c-84d6-08766b92d02f)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         033f2f0c-2a48-4729-b750-504734a0a053)(content(Whitespace\" \
         \"))))(Tile((id \
         8de88c51-549a-4da8-85ec-640614fac4e1)(label(\"\\\"duskrose \
         pathway\\\"\"))(mold((out Exp)(in_())(nibs(((shape Convex)(sort \
         Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         adc2436e-ba84-4bed-a96c-f64d02e46823)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         58ed08ec-ba07-4f2f-82be-5d7456a8c8d2)(content(Whitespace\"\\n\")))))";
      backup_text =
        "# Garden Survey Notes                                  #\n\
         #                                                      #\n\
         # After each moonlit garden walk, visitors fill out    #\n\
         # a short survey. Entries look like:                   #\n\
         #   \"Q1: yes -- the moonbloom beds were stunning\"      #\n\
         #   \"Q2: no -- too many weeds near the starfern\"       #\n\
         #                                                      #\n\
         # Extract just the notes from positive responses.      #\n\
         # For the data below, the result should be:            #\n\
         #   [\"the moonbloom beds were stunning\",               #\n\
         #    \"loved the new duskrose pathway\",                 #\n\
         #    \"the nightshade corner was magical\"]              #\n\
         #                                                      #\n\
         # Steps:                                               #\n\
         #   1. is_positive: check if a response is \"yes\"       #\n\
         #   2. extract_note: get the text after the --         #\n\
         #   3. positive_notes: filter then extract             #\n\
         #                                                      #\n\
         # Available functions:                                 #\n\
         #   string_match: (String, String) -> Bool             #\n\
         #   string_split: (String, String) -> [String]         #\n\
         #   string_trim: String -> String                      #\n\
         #   nth: ([?], Int) -> ?                               #\n\
         #   filter: ([?], ? -> Bool) -> [?]                    #\n\
         #   map: ([?], ? -> ?) -> [?]                          #\n\
         #                                                      #\n\
         # string_match checks whether a regex pattern          #\n\
         # appears anywhere in the string. Anywhere!            #\n\
         #                                                      #\n\
         # Tip: Probe each function with the test data          #\n\
         # before combining them. Regex substring matching      #\n\
         # can be surprising -- check carefully!                #\n\n\
         let entries = [\n\
         \"Q1: yes -- the moonbloom beds were stunning\",\n\
         \"Q2: no -- too many weeds near the starfern\",\n\
         \"Q3: yes -- loved the new duskrose pathway\",\n\
         \"Q4: no -- yesterday's rain left puddles\",\n\
         \"Q5: yes -- the nightshade corner was magical\"\n\
         ] in\n\n\
         # Does this survey entry have a positive response? #\n\
         let is_positive: String -> Bool = fun entry ->\n\
         ?\n\n\n\n\
         in\n\n\
         # Extract the note text from a survey entry #\n\
         let extract_note: String -> String = fun entry ->\n\
         ?\n\n\n\n\
         in\n\n\
         # Get only the notes from positive responses #\n\
         let positive_notes: [String] -> [String] = fun entries ->\n\
         ?\n\n\n\n\
         in\n\n\
         test positive_notes(entries) == [\n\
         \"the moonbloom beds were stunning\",\n\
         \"loved the new duskrose pathway\",\n\
         \"the nightshade corner was magical\"\n\
         ] end;\n\n\
         test is_positive(\"Q1: yes -- moonblooms lovely\") == true end;\n\n\
         test is_positive(\"Q2: no -- needs more starfern\") == false end;\n\n\
         test is_positive(\"Q4: no -- yesterday was rainy\") == false end;\n\n\
         test extract_note(\"Q3: yes -- duskrose pathway\")\n\
         == \"duskrose pathway\" end\n";
      refractors = "()";
    } )
