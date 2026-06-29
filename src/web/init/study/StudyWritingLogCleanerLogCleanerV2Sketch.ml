let out : string * Haz3lcore.PersistentSegment.t =
  ( "Study / writing / log-cleaner / log-cleaner-v2-sketch",
    {
      segment =
        "((Secondary((id \
         b9f3dacf-c67a-4edc-a819-4be7a0a3c267)(content(Comment\"# Moonphase \
         Log Cleaner v2                             #\"))))(Secondary((id \
         6b7a2b04-fbf0-4962-9fa2-79b2bd531118)(content(Whitespace\"\\n\"))))(Secondary((id \
         50a91334-6cd1-4dd4-a0e1-697d39e9f360)(content(Comment\"#                                                      \
         #\"))))(Secondary((id \
         05be1704-dbf4-4659-a9fe-3eafdd5a56c9)(content(Whitespace\"\\n\"))))(Secondary((id \
         1d7d892c-2f5c-4687-beef-0e475724b6a4)(content(Comment\"# Garden \
         keepers record observations in a messy        #\"))))(Secondary((id \
         78331d29-d293-4ab0-ba4c-3346b47e109b)(content(Whitespace\"\\n\"))))(Secondary((id \
         27875454-60a5-47f9-9fd4-de3f43d23122)(content(Comment\"# log with \
         entry numbers, emoji markers, inconsistent  #\"))))(Secondary((id \
         06cae01a-8071-4b32-9131-0dd87d2e3ac4)(content(Whitespace\"\\n\"))))(Secondary((id \
         ffdec9e9-5d5c-4623-9e0b-018fed5a3299)(content(Comment\"# dashes, and \
         extra whitespace. Implement clean_entry  #\"))))(Secondary((id \
         7c874fe8-237d-4824-873a-d320910ce385)(content(Whitespace\"\\n\"))))(Secondary((id \
         91b05a32-969d-499e-98fe-91b48b4e8b29)(content(Comment\"# to \
         standardize each log entry.                       \
         #\"))))(Secondary((id \
         af4636b1-b9fd-4db4-9311-f07741b721a8)(content(Whitespace\"\\n\"))))(Secondary((id \
         7ec8cfa2-c88a-4fc4-87b1-36dafbd60c1a)(content(Comment\"#                                                      \
         #\"))))(Secondary((id \
         5de3aa51-b5fb-49a3-a7d6-8bfe4426e73d)(content(Whitespace\"\\n\"))))(Secondary((id \
         8243d4f0-6bb6-48d2-97fa-99f4eb83acff)(content(Comment\"# Each raw \
         entry has a number marker (a hash sign      #\"))))(Secondary((id \
         bfb38e8f-bf4a-47ca-a578-6edd5b6c31fc)(content(Whitespace\"\\n\"))))(Secondary((id \
         d10cea4d-8de5-4923-926d-0f318f763b55)(content(Comment\"# then \
         digits), a moon emoji, a phase name, and        #\"))))(Secondary((id \
         bd3d9d26-0f24-4564-9290-66ace0a8682b)(content(Whitespace\"\\n\"))))(Secondary((id \
         455c7e68-4aa3-4117-a075-61f9f8431d6b)(content(Comment\"# notes after \
         dashes. See the test cases below         #\"))))(Secondary((id \
         2f2495f1-982a-430d-a540-9c815556522d)(content(Whitespace\"\\n\"))))(Secondary((id \
         7d1d868e-3a04-4ec8-becd-0ac82341c829)(content(Comment\"# for \
         examples.                                        \
         #\"))))(Secondary((id \
         2f664bca-339b-43e5-a76a-08ed5fc188f2)(content(Whitespace\"\\n\"))))(Secondary((id \
         9d2df247-f64d-4803-983d-a5698e0054bf)(content(Comment\"#                                                      \
         #\"))))(Secondary((id \
         c5a98d8b-27d8-43cb-b2f2-dd7583f7d358)(content(Whitespace\"\\n\"))))(Secondary((id \
         8feaab60-d749-4a52-841b-329e07dd8748)(content(Comment\"# Cleaned \
         entries should look like:                    #\"))))(Secondary((id \
         59b58ad4-76b4-4b8a-a2b6-d4cce5b10ab7)(content(Whitespace\"\\n\"))))(Secondary((id \
         f4cc8ea9-e72b-4aa3-bc43-4da14242b1cc)(content(Comment\"#   \
         \\\"\\240\\159\\140\\149 Full Moon: clear skies, planted \
         moonbloom\\\"     #\"))))(Secondary((id \
         646c9532-ba0d-4145-9a9f-495f879a0eee)(content(Whitespace\"\\n\"))))(Secondary((id \
         5b004d1d-7cef-4f56-988e-db38f20b83fb)(content(Comment\"#   \
         \\\"\\240\\159\\140\\145 New Moon: cloudy, harvested \
         starfern\\\"          #\"))))(Secondary((id \
         fc573e15-9e1d-4027-b0f1-713e6fac367f)(content(Whitespace\"\\n\"))))(Secondary((id \
         129cab44-7e0b-46a7-9c22-84a863b3986d)(content(Comment\"#                                                      \
         #\"))))(Secondary((id \
         864b4eaf-9234-477c-b06e-55d6e86994be)(content(Whitespace\"\\n\"))))(Secondary((id \
         5e90d75c-1585-4d9f-9621-36d08bf10ba5)(content(Comment\"# The moon \
         emojis stay! Only the entry numbers         #\"))))(Secondary((id \
         b7fc5593-b1c9-4691-b493-785efed1d391)(content(Whitespace\"\\n\"))))(Secondary((id \
         916ebb20-1f2d-47a7-bac7-b583426007df)(content(Comment\"# should be \
         removed.                                   #\"))))(Secondary((id \
         d122229a-05f8-4f3e-bcf0-4c82bb4f11ff)(content(Whitespace\"\\n\"))))(Secondary((id \
         e7b493d4-e7cd-4868-8647-347ad41051d8)(content(Comment\"#                                                      \
         #\"))))(Secondary((id \
         da5fa9ad-75e0-446c-9b5d-ad10cfe03252)(content(Whitespace\"\\n\"))))(Secondary((id \
         14af931d-2900-45ca-8683-fd47e569693b)(content(Comment\"# \
         Steps:                                               \
         #\"))))(Secondary((id \
         7eb5cb18-16e9-41b1-8945-7964e38233a2)(content(Whitespace\"\\n\"))))(Secondary((id \
         d9a39380-8a43-4093-856c-e4327f62d3ca)(content(Comment\"#   1. Trim \
         leading/trailing whitespace                #\"))))(Secondary((id \
         7a332de8-7804-4ebd-ad81-c5d77b38aa73)(content(Whitespace\"\\n\"))))(Secondary((id \
         7e68551d-429c-4f9d-b815-c18bdef16b4a)(content(Comment\"#   2. Remove \
         entry numbers (hash followed by digits)  #\"))))(Secondary((id \
         4f7c05a7-6948-4e94-b31d-ff08de29bd6d)(content(Whitespace\"\\n\"))))(Secondary((id \
         e5e65447-1c74-4e3f-817f-66411f57382e)(content(Comment\"#   3. \
         Normalize \\\" -- \\\" or \\\"--\\\" into \\\": \\\"              \
         #\"))))(Secondary((id \
         c93a409f-d2f1-458b-87c8-00613c0c4e64)(content(Whitespace\"\\n\"))))(Secondary((id \
         62a2aa22-7607-4bd4-9321-19f84189f728)(content(Comment\"#   4. \
         Collapse multiple spaces into one               #\"))))(Secondary((id \
         c4f60325-b2aa-40bd-9b9e-5fefe0d75dbd)(content(Whitespace\"\\n\"))))(Secondary((id \
         4c5c797d-78ff-4f17-b4eb-7c7209107f6c)(content(Comment\"#   5. Final \
         trim for any leftover edge spaces         #\"))))(Secondary((id \
         e8c7a005-abb0-487c-9ff3-e51ac4d7d034)(content(Whitespace\"\\n\"))))(Secondary((id \
         46cbc8fc-1504-45bf-9142-b5c22cfb7f5f)(content(Comment\"#                                                      \
         #\"))))(Secondary((id \
         acdb8ef2-a4b2-4589-b9ee-a8de7d694b69)(content(Whitespace\"\\n\"))))(Secondary((id \
         c8be489d-67b0-422e-b7e1-7c200394584e)(content(Comment\"# Available \
         functions:                                 #\"))))(Secondary((id \
         4c2cd061-34ea-4102-8821-757dd5959d92)(content(Whitespace\"\\n\"))))(Secondary((id \
         a1225d18-efae-4734-afaf-c871cc1c6f12)(content(Comment\"#   \
         string_trim: String -> String                      \
         #\"))))(Secondary((id \
         04fe879b-d0e9-42d7-960c-a635be888877)(content(Whitespace\"\\n\"))))(Secondary((id \
         459545a9-37c6-49a1-bf5a-a0e83e0bcf09)(content(Comment\"#   \
         string_match: (String, String) -> Bool             \
         #\"))))(Secondary((id \
         39e2a0d2-a91f-4b02-9a6c-6664ad2a4aec)(content(Whitespace\"\\n\"))))(Secondary((id \
         a7027a25-3c10-4da0-87b2-8a1661ed9823)(content(Comment\"#   \
         string_replace: (String, String, String) -> String \
         #\"))))(Secondary((id \
         17c7b5ce-795c-4575-83c6-9558d7dac9fe)(content(Whitespace\"\\n\"))))(Secondary((id \
         03ae02ca-d331-4b7a-94fc-84c6e5f67d13)(content(Comment\"#                                                      \
         #\"))))(Secondary((id \
         9dd027cd-2bbd-48e0-bdb8-e5d957aad22c)(content(Whitespace\"\\n\"))))(Secondary((id \
         702cbef2-a937-4997-819a-2846af042798)(content(Comment\"# These \
         functions are tragically underdocumented!      #\"))))(Secondary((id \
         2c341385-597b-489f-8521-76eab8db0549)(content(Whitespace\"\\n\"))))(Secondary((id \
         a040ce34-d68c-458d-98fe-b17b807b4fe7)(content(Comment\"# You will \
         have to figure out what those String        #\"))))(Secondary((id \
         59926199-7020-4f98-ba6c-71a215908930)(content(Whitespace\"\\n\"))))(Secondary((id \
         d634081b-66eb-4bce-9770-ff682735e9f6)(content(Comment\"# arguments \
         mean by experimenting with probes.         #\"))))(Secondary((id \
         613f6134-f259-4914-a35f-ccfdb8223154)(content(Whitespace\"\\n\"))))(Secondary((id \
         01d1d838-7a2d-4647-a8a7-37b6700063a6)(content(Comment\"#                                                      \
         #\"))))(Secondary((id \
         0cee3038-d4bd-45fd-8523-f90091a87314)(content(Whitespace\"\\n\"))))(Secondary((id \
         d07e3ac8-e061-438a-88f9-fc36e4960599)(content(Comment\"# One of the \
         String arguments is a regex pattern.      #\"))))(Secondary((id \
         ebcfaf9e-9c4f-4164-ad3d-4ed544826310)(content(Whitespace\"\\n\"))))(Secondary((id \
         00cf6233-1b34-4d63-805d-e8dd0701f655)(content(Comment\"# Some useful \
         regex building blocks:                   #\"))))(Secondary((id \
         cc422cd0-7c97-42fc-9fcd-8ed04a413640)(content(Whitespace\"\\n\"))))(Secondary((id \
         86fc5289-158e-432d-bfc2-971f2d21c401)(content(Comment\"#   + means \
         \\\"one or more of the preceding\\\"             \
         #\"))))(Secondary((id \
         5a20dfb2-859f-44ba-ab7f-7ba22fb5925a)(content(Whitespace\"\\n\"))))(Secondary((id \
         e191c205-4ae4-4170-8cea-0b5c1d4d9065)(content(Comment\"#   * means \
         \\\"zero or more of the preceding\\\"            \
         #\"))))(Secondary((id \
         2de3d677-49f9-4d22-8368-760afa861169)(content(Whitespace\"\\n\"))))(Secondary((id \
         8cda9d19-fdfc-4d6e-ad75-beec62170914)(content(Comment\"#   [abc] \
         matches any one character from the set       #\"))))(Secondary((id \
         39f8b081-9530-42c4-a3cc-7bcb65002dd6)(content(Whitespace\"\\n\"))))(Secondary((id \
         6ccd1b7a-7235-4bb8-9523-49ed1013fb37)(content(Comment\"#   [0-9] \
         matches any digit                            #\"))))(Secondary((id \
         fb65e8a0-92e8-4833-8d8b-57389e81ac60)(content(Whitespace\"\\n\"))))(Secondary((id \
         2ae32728-7dc4-4bcf-a1b3-e840da78f7ce)(content(Comment\"#   A space in \
         a pattern matches a literal space       #\"))))(Secondary((id \
         7e7f319d-f78d-4fc4-88c7-7112e4bf1b6b)(content(Whitespace\"\\n\"))))(Secondary((id \
         e50b4247-11c8-4997-8a14-eb471fed10b8)(content(Comment\"#                                                      \
         #\"))))(Secondary((id \
         271464e9-0c68-4f03-bab8-005d41d5f5be)(content(Whitespace\"\\n\"))))(Secondary((id \
         05c3728d-8750-4d95-969c-7045e9bb7c45)(content(Comment\"# Tip: Build \
         one step at a time! After each line,      #\"))))(Secondary((id \
         4c921bf3-6b7c-4646-b0a3-a47d2cb3b324)(content(Whitespace\"\\n\"))))(Secondary((id \
         d25d6c2f-942d-44b1-877f-ddee626ec2bf)(content(Comment\"# check the \
         probe to see what your pattern did.        #\"))))(Secondary((id \
         edb33206-daff-48ab-9b9d-3cda66238201)(content(Whitespace\"\\n\"))))(Secondary((id \
         22083f22-51cc-44b2-89ba-386d47ee28b6)(content(Whitespace\"\\n\"))))(Tile((id \
         16e96cbe-4f2d-490f-93f4-c92dad3dbf91)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         464041ee-48c5-4cd0-8b30-21db041eff80)(content(Whitespace\" \
         \"))))(Tile((id \
         11e80d05-7917-4148-9e06-885eb63e54db)(label(clean_entry))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         0bdbc26e-02d7-42a1-97e1-37804e5eb444)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 24))(sort Pat))((shape(Concave \
         24))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         555cf6bc-c4d9-4584-8899-c2eda117a735)(content(Whitespace\" \
         \"))))(Tile((id \
         82f03d64-c983-4c10-83e2-6b93cabc3f37)(label(String))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         7df4437a-9f96-4cd5-b740-c21f42f5118a)(content(Whitespace\" \
         \"))))(Tile((id \
         81e992ea-e8d2-4b54-bfd6-fd5166f4113a)(label(->))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 13))(sort Typ))((shape(Concave \
         13))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         6d7a26fe-96e8-4fa0-b3a8-4121c365c0c8)(content(Whitespace\" \
         \"))))(Tile((id \
         ccd46d52-cef3-4cff-8239-12099ef60ec2)(label(String))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         623f35aa-5364-46e1-9c1f-2e34e4d7af58)(content(Whitespace\" \
         \")))))((Secondary((id \
         c0224a56-7294-4424-b196-5f3b7d47d81d)(content(Whitespace\" \
         \"))))(Tile((id 07a79ede-d952-421e-933f-89bdcfe84831)(label(fun \
         ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 37))(sort Exp))))))(shards(0 \
         1))(children(((Secondary((id \
         6445d5b6-bef4-45df-b32e-44616b1c5789)(content(Whitespace\" \
         \"))))(Tile((id \
         e34cd98e-0f6d-4687-a29d-efa26f0cf5ca)(label(entry))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         63e30c9d-1b93-4c63-b7cd-07e6ad46a340)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         469f9ce0-8b59-466c-905b-e646406f9929)(content(Whitespace\"\\n\"))))(Tile((id \
         9e4691c8-164d-47ee-a379-13cee2fafcee)(label(?))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         ef87a45d-1f86-4861-9749-99d0a2648c57)(content(Whitespace\"\\n\"))))(Secondary((id \
         edf06068-6602-4bca-81ef-f57aeba30d72)(content(Whitespace\"\\n\"))))(Secondary((id \
         31c792be-6ed4-400b-94ab-7bf17ac58f3b)(content(Whitespace\"\\n\"))))(Secondary((id \
         1ed60dc4-6820-453a-849b-f5d6c5c61aad)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         7f4668f7-d92e-4894-97c5-db67f59c2e7f)(content(Whitespace\"\\n\"))))(Secondary((id \
         c067f87a-e06d-4eac-ae38-b8d471fa5d82)(content(Whitespace\"\\n\"))))(Tile((id \
         d0429144-46cf-4346-855b-c36d6456685c)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         f40aaaa5-dd04-4190-868f-2a4fe1fe0461)(content(Whitespace\"\\n\"))))(Tile((id \
         ccea322a-d779-4716-8faa-710727f40b58)(label(clean_entry))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         474752bf-56e2-4db6-9118-93183d29bdc1)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         7fae962a-f0d0-4b52-9253-5470f07e2e5f)(label(\"\\\"  #42 \
         \\240\\159\\140\\149 Full Moon -- clear skies, planted moonbloom  \
         \\\"\"))(mold((out Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0))(children()))))))))(Secondary((id \
         84ae00ad-0775-49fe-a6b7-47dc760bc906)(content(Whitespace\" \
         \"))))(Tile((id \
         20d6ded5-2cef-4b1f-9cd7-a9dbc9e04dad)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         94840de0-da28-4b69-99f9-4a7e8e9dc614)(content(Whitespace\"\\n\"))))(Tile((id \
         6e9aaa50-2131-439c-be49-f0052b69c8ef)(label(\"\\\"\\240\\159\\140\\149 \
         Full Moon: clear skies, planted moonbloom\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         7e1b2950-f597-4e62-a368-e46093e5c2e7)(content(Whitespace\"\\n\")))))))))(Tile((id \
         7d801d83-16e5-4d98-a470-738966dfe2c7)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         57ac6966-3e80-40e0-aae8-5e8ea5938a08)(content(Whitespace\"\\n\"))))(Secondary((id \
         5408bf14-460e-4f58-9a96-3f343ab027f1)(content(Whitespace\"\\n\"))))(Tile((id \
         4089aa76-48d7-4f94-947f-0b957f5de106)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         90ea5f96-273a-4335-94aa-e20ac5985a46)(content(Whitespace\"\\n\"))))(Tile((id \
         6bab9310-8015-4069-9bad-d2a3d6400d52)(label(clean_entry))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         b8d1bdcb-26e7-4c7f-9099-2c634f1a35b6)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         866db298-544e-427e-a4aa-54a6cd7c3910)(label(\"\\\"#7 \
         \\240\\159\\140\\145  New Moon--cloudy,   harvested \
         starfern\\\"\"))(mold((out Exp)(in_())(nibs(((shape Convex)(sort \
         Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         34d84420-773e-4495-a329-614e2832bd83)(content(Whitespace\" \
         \"))))(Tile((id \
         8e789229-53f7-43ba-bc13-9c7ef626e361)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         0da2822a-b2a8-474c-a014-def7d67126bd)(content(Whitespace\"\\n\"))))(Tile((id \
         8cb59310-4532-487c-8ed1-580cfdc68fa3)(label(\"\\\"\\240\\159\\140\\145 \
         New Moon: cloudy, harvested starfern\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         6588d39a-5bbd-49b8-937a-b2b50778842e)(content(Whitespace\"\\n\")))))))))(Tile((id \
         021ef052-9c9f-40ba-9fff-46388ec5eef3)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         0c254b27-d292-4e1c-bf46-07c5878b0d86)(content(Whitespace\"\\n\"))))(Secondary((id \
         0c32fca4-d7fb-419c-bbd0-fce3fa8b9393)(content(Whitespace\"\\n\"))))(Tile((id \
         30c04280-6f42-4abe-b2f1-26557cb574cd)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         fac325bb-19e2-4e1c-8ac5-e2fe1626df9f)(content(Whitespace\"\\n\"))))(Tile((id \
         1b44a625-d1fd-4f2f-80aa-f866ac213977)(label(clean_entry))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         acaf92c4-c67a-4940-9259-0bd0f1e2095c)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         5150cc2c-24c6-41da-9aa3-8349a1746a48)(label(\"\\\"  #103 \
         \\240\\159\\140\\147 Half Moon -- light rain, pruned duskrose  \
         \\\"\"))(mold((out Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0))(children()))))))))(Secondary((id \
         8894e985-1fc3-4c3f-9e25-0d5e8bdd8594)(content(Whitespace\" \
         \"))))(Tile((id \
         269de53c-cd63-497a-90dd-de52a568d80e)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         49c4670d-db2a-4ad8-b415-232ae27aa6c2)(content(Whitespace\"\\n\"))))(Tile((id \
         b252c0db-fa1a-4d95-adbc-63b282145757)(label(\"\\\"\\240\\159\\140\\147 \
         Half Moon: light rain, pruned duskrose\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         9a562021-7f74-4051-b5ef-f8ad9a5fb02e)(content(Whitespace\"\\n\")))))))))(Tile((id \
         23968274-1b59-4adc-ad4a-21fa0966e9f4)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         fa64b4e1-ad91-4fd8-ba92-218fded8b6f7)(content(Whitespace\"\\n\"))))(Secondary((id \
         425db049-d9a3-4a63-a4f3-39acafa4da09)(content(Whitespace\"\\n\"))))(Tile((id \
         74803977-8792-4e43-8282-1e22b3e75c9a)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         80315477-25c1-4840-a540-11afb69e472d)(content(Whitespace\"\\n\"))))(Tile((id \
         ba544b81-6999-41f0-92b9-6c286debab5d)(label(clean_entry))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         aee9c1f1-ca1f-44f9-8fb6-754414ef8a64)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         9cf8b698-2747-4a77-9f22-02f353cdd405)(label(\"\\\"#15 \
         \\240\\159\\140\\151 Crescent--foggy,  checked   moth \
         traps\\\"\"))(mold((out Exp)(in_())(nibs(((shape Convex)(sort \
         Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         f84c1299-979b-474d-ab6a-66025c333358)(content(Whitespace\" \
         \"))))(Tile((id \
         a8e99fde-dfd9-4cc2-9c46-4005556379e4)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         a7ec4dd6-b637-449c-a8bf-2330abc566a0)(content(Whitespace\"\\n\"))))(Tile((id \
         1a2f7dc7-8f0f-4df6-8956-0a06c14a10ed)(label(\"\\\"\\240\\159\\140\\151 \
         Crescent: foggy, checked moth traps\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         a264f570-7b8c-4d27-9f83-841f4885dcfc)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         9984e672-abd2-4c45-a7f4-4dd5d22975ae)(content(Whitespace\"\\n\")))))";
      backup_text =
        "# Moonphase Log Cleaner v2                             #\n\
         #                                                      #\n\
         # Garden keepers record observations in a messy        #\n\
         # log with entry numbers, emoji markers, inconsistent  #\n\
         # dashes, and extra whitespace. Implement clean_entry  #\n\
         # to standardize each log entry.                       #\n\
         #                                                      #\n\
         # Each raw entry has a number marker (a hash sign      #\n\
         # then digits), a moon emoji, a phase name, and        #\n\
         # notes after dashes. See the test cases below         #\n\
         # for examples.                                        #\n\
         #                                                      #\n\
         # Cleaned entries should look like:                    #\n\
         #   \"\240\159\140\149 Full Moon: clear skies, planted \
         moonbloom\"     #\n\
         #   \"\240\159\140\145 New Moon: cloudy, harvested \
         starfern\"          #\n\
         #                                                      #\n\
         # The moon emojis stay! Only the entry numbers         #\n\
         # should be removed.                                   #\n\
         #                                                      #\n\
         # Steps:                                               #\n\
         #   1. Trim leading/trailing whitespace                #\n\
         #   2. Remove entry numbers (hash followed by digits)  #\n\
         #   3. Normalize \" -- \" or \"--\" into \": \"              #\n\
         #   4. Collapse multiple spaces into one               #\n\
         #   5. Final trim for any leftover edge spaces         #\n\
         #                                                      #\n\
         # Available functions:                                 #\n\
         #   string_trim: String -> String                      #\n\
         #   string_match: (String, String) -> Bool             #\n\
         #   string_replace: (String, String, String) -> String #\n\
         #                                                      #\n\
         # These functions are tragically underdocumented!      #\n\
         # You will have to figure out what those String        #\n\
         # arguments mean by experimenting with probes.         #\n\
         #                                                      #\n\
         # One of the String arguments is a regex pattern.      #\n\
         # Some useful regex building blocks:                   #\n\
         #   + means \"one or more of the preceding\"             #\n\
         #   * means \"zero or more of the preceding\"            #\n\
         #   [abc] matches any one character from the set       #\n\
         #   [0-9] matches any digit                            #\n\
         #   A space in a pattern matches a literal space       #\n\
         #                                                      #\n\
         # Tip: Build one step at a time! After each line,      #\n\
         # check the probe to see what your pattern did.        #\n\n\
         let clean_entry: String -> String = fun entry ->\n\
         ?\n\n\n\n\
         in\n\n\
         test\n\
         clean_entry(\"  #42 \240\159\140\149 Full Moon -- clear skies, \
         planted moonbloom  \") ==\n\
         \"\240\159\140\149 Full Moon: clear skies, planted moonbloom\"\n\
         end;\n\n\
         test\n\
         clean_entry(\"#7 \240\159\140\145  New Moon--cloudy,   harvested \
         starfern\") ==\n\
         \"\240\159\140\145 New Moon: cloudy, harvested starfern\"\n\
         end;\n\n\
         test\n\
         clean_entry(\"  #103 \240\159\140\147 Half Moon -- light rain, pruned \
         duskrose  \") ==\n\
         \"\240\159\140\147 Half Moon: light rain, pruned duskrose\"\n\
         end;\n\n\
         test\n\
         clean_entry(\"#15 \240\159\140\151 Crescent--foggy,  checked   moth \
         traps\") ==\n\
         \"\240\159\140\151 Crescent: foggy, checked moth traps\"\n\
         end\n";
      refractors = "()";
    } )
