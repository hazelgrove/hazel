let out : string * Haz3lcore.PersistentSegment.t =
  ( "Study / tasks-draft / 05W-log-cleaner",
    {
      segment =
        "((Secondary((id \
         90be44b3-f2f8-42d3-b05f-9c23b2ea8fe5)(content(Comment\"# Moonphase \
         Log Cleaner v2                             #\"))))(Secondary((id \
         da07a171-4a27-4640-ae77-94e4db9bb92e)(content(Whitespace\"\\n\"))))(Secondary((id \
         eb609a92-9390-4438-9d57-80dcda541b9d)(content(Comment\"#                                                      \
         #\"))))(Secondary((id \
         96201dd4-6df6-42eb-adf0-d72b2958741f)(content(Whitespace\"\\n\"))))(Secondary((id \
         d2b4c9c8-6011-48d4-9901-2a3416ca4ac4)(content(Comment\"# Garden \
         keepers record observations in a messy        #\"))))(Secondary((id \
         1c6c6b76-29e4-4140-bc8e-4a39c201debe)(content(Whitespace\"\\n\"))))(Secondary((id \
         1248363b-51fb-4da7-9445-739b1140ffe6)(content(Comment\"# log with \
         entry numbers, emoji markers, inconsistent  #\"))))(Secondary((id \
         9eb9ddde-50cb-402b-95a7-e5a2a66787d6)(content(Whitespace\"\\n\"))))(Secondary((id \
         7213b4a6-7c5a-4185-abe5-07424a967d6e)(content(Comment\"# dashes, and \
         extra whitespace. Implement clean_entry  #\"))))(Secondary((id \
         e232469d-dc91-4423-9f05-fbeee9e3de8c)(content(Whitespace\"\\n\"))))(Secondary((id \
         a9c88199-4f34-40b6-a904-bf68626c3546)(content(Comment\"# to \
         standardize each log entry.                       \
         #\"))))(Secondary((id \
         cf16e3e3-3e6b-4697-935a-3191768c2c90)(content(Whitespace\"\\n\"))))(Secondary((id \
         87ec1f47-86d2-4fad-9793-a94c1aadc1e5)(content(Comment\"#                                                      \
         #\"))))(Secondary((id \
         c5ae8839-75ba-4ad4-890e-4b1a879fb45b)(content(Whitespace\"\\n\"))))(Secondary((id \
         6891dbbc-1c0c-4471-82b0-14234c5339bc)(content(Comment\"# Each raw \
         entry has a number marker (a hash sign      #\"))))(Secondary((id \
         95543e99-5a17-437f-963f-df9667af5e9a)(content(Whitespace\"\\n\"))))(Secondary((id \
         8d57c98d-0d3c-459c-85ee-ebf2c0ad25a7)(content(Comment\"# then \
         digits), a moon emoji, a phase name, and        #\"))))(Secondary((id \
         e50d6ed5-6090-4861-a4fe-f6ef86326e0a)(content(Whitespace\"\\n\"))))(Secondary((id \
         d64a2701-802c-4580-a9a2-3b576fc27ce1)(content(Comment\"# notes after \
         dashes. See the test cases below         #\"))))(Secondary((id \
         cd6025f7-fcbc-4185-839d-55d8cb7a0e28)(content(Whitespace\"\\n\"))))(Secondary((id \
         a588d3f4-31ae-4a1f-9982-4fef5cb3eb17)(content(Comment\"# for \
         examples.                                        \
         #\"))))(Secondary((id \
         fd0fdccc-51c0-4f34-aece-81a6126484e5)(content(Whitespace\"\\n\"))))(Secondary((id \
         ee9c01f0-2d71-4488-ae2d-d5ac8bc55592)(content(Comment\"#                                                      \
         #\"))))(Secondary((id \
         f5f9a7af-b10a-4555-8652-c3899a7df103)(content(Whitespace\"\\n\"))))(Secondary((id \
         ab9226b7-c96a-4c8e-983c-8433a62a102b)(content(Comment\"# Cleaned \
         entries should look like:                    #\"))))(Secondary((id \
         32d22e3e-d808-49ef-948d-b23c99300024)(content(Whitespace\"\\n\"))))(Secondary((id \
         ed0d4665-8355-44c4-8817-51d2360869de)(content(Comment\"#   \
         \\\"\\240\\159\\140\\149 Full Moon: clear skies, planted \
         moonbloom\\\"     #\"))))(Secondary((id \
         04476c65-d5a6-4652-90f3-8bbae25000ea)(content(Whitespace\"\\n\"))))(Secondary((id \
         154e0e4b-85f7-4d2e-9096-d729a10a691a)(content(Comment\"#   \
         \\\"\\240\\159\\140\\145 New Moon: cloudy, harvested \
         starfern\\\"          #\"))))(Secondary((id \
         846e5b8a-bc01-446a-93c9-0a33dccbb4f6)(content(Whitespace\"\\n\"))))(Secondary((id \
         61295a8e-0d77-48e6-80ab-248d066fa152)(content(Comment\"#                                                      \
         #\"))))(Secondary((id \
         6d974e99-9bf9-41db-bb03-4388226481e4)(content(Whitespace\"\\n\"))))(Secondary((id \
         a2edbc55-ee77-4cf9-974e-e836cc2baf1b)(content(Comment\"# The moon \
         emojis stay! Only the entry numbers         #\"))))(Secondary((id \
         2f840d6f-b16f-4b88-beca-9beb9af630f8)(content(Whitespace\"\\n\"))))(Secondary((id \
         52e02047-883e-4175-93db-855de6c0747e)(content(Comment\"# should be \
         removed.                                   #\"))))(Secondary((id \
         4e132bc5-db48-4a53-ac02-e41a6c7bb0fc)(content(Whitespace\"\\n\"))))(Secondary((id \
         17717bb8-256b-4c7e-b759-bc08bc2c6308)(content(Comment\"#                                                      \
         #\"))))(Secondary((id \
         33c3a2aa-01d9-4aa9-aee9-0f86dde4d665)(content(Whitespace\"\\n\"))))(Secondary((id \
         620ac293-52a9-4116-9f5b-263492b7f67c)(content(Comment\"# \
         Steps:                                               \
         #\"))))(Secondary((id \
         41f2fd9a-f591-48ef-9e61-b9b70292c335)(content(Whitespace\"\\n\"))))(Secondary((id \
         72d9b8f9-311b-487c-ab8b-c8ef83d3d41a)(content(Comment\"#   1. Trim \
         leading/trailing whitespace                #\"))))(Secondary((id \
         5101e9c2-2e9f-4bc9-a84a-c8bbeab7b62a)(content(Whitespace\"\\n\"))))(Secondary((id \
         e21ee8a7-d642-4962-b347-f70ad6cf1222)(content(Comment\"#   2. Remove \
         entry numbers (hash followed by digits)  #\"))))(Secondary((id \
         b4693066-e710-41e0-abee-d30fb362ad9d)(content(Whitespace\"\\n\"))))(Secondary((id \
         1ae9cfc9-9c9b-4561-ae4a-f17995e36b7e)(content(Comment\"#   3. \
         Normalize \\\" -- \\\" or \\\"--\\\" into \\\": \\\"              \
         #\"))))(Secondary((id \
         3712f8bd-bbc5-49fc-bb11-39d3e10b0da2)(content(Whitespace\"\\n\"))))(Secondary((id \
         6fdd8ccc-c48c-4e4a-8aba-dbabe8420691)(content(Comment\"#   4. \
         Collapse multiple spaces into one               #\"))))(Secondary((id \
         511865f0-e6cb-4043-a9de-7febe2307e1a)(content(Whitespace\"\\n\"))))(Secondary((id \
         f228c6b1-aefb-4c04-b647-9898ada3032d)(content(Comment\"#   5. Final \
         trim for any leftover edge spaces         #\"))))(Secondary((id \
         9ee9a1b2-56fb-4be1-9241-7ec06e738a8e)(content(Whitespace\"\\n\"))))(Secondary((id \
         5ab09e6b-fabd-4d85-8025-6fa3ed314260)(content(Comment\"#                                                      \
         #\"))))(Secondary((id \
         859859cf-ccaa-4a02-bd03-49473c22f0bd)(content(Whitespace\"\\n\"))))(Secondary((id \
         536b8754-00a9-4911-a954-149af0f0479b)(content(Comment\"# Available \
         functions:                                 #\"))))(Secondary((id \
         7125e481-b85d-4f02-aa1b-bef810764d4d)(content(Whitespace\"\\n\"))))(Secondary((id \
         563726bf-9936-4b8e-b3e4-e7f2b938e0b4)(content(Comment\"#   \
         string_trim: String -> String                      \
         #\"))))(Secondary((id \
         6d50557e-ac6d-46e7-a609-fe577c82cf05)(content(Whitespace\"\\n\"))))(Secondary((id \
         73597305-b0bd-4d91-a40f-d6be6479913a)(content(Comment\"#   \
         string_match: (String, String) -> Bool             \
         #\"))))(Secondary((id \
         7d619d40-7999-43c1-9091-d19b01cc6911)(content(Whitespace\"\\n\"))))(Secondary((id \
         3e637af1-9166-4f31-87cb-f1f327b7c38d)(content(Comment\"#   \
         string_replace: (String, String, String) -> String \
         #\"))))(Secondary((id \
         898b3aeb-1d56-434a-9b24-f3d1a3811051)(content(Whitespace\"\\n\"))))(Secondary((id \
         5f3d6822-f3cc-4376-948e-16b3ad6d1938)(content(Comment\"#                                                      \
         #\"))))(Secondary((id \
         637f50b6-7b0f-4547-91d8-36daf3972e31)(content(Whitespace\"\\n\"))))(Secondary((id \
         4dbf3207-9088-4dea-8026-11fea4bd7905)(content(Comment\"# These \
         functions are tragically underdocumented!      #\"))))(Secondary((id \
         b8fceca7-7615-43bb-9ffc-f4d00e2f7080)(content(Whitespace\"\\n\"))))(Secondary((id \
         41a6c610-f81c-4362-81e1-a4897e4fdb95)(content(Comment\"# You will \
         have to figure out what those String        #\"))))(Secondary((id \
         83111df2-c9c2-4eca-a336-e160b2815718)(content(Whitespace\"\\n\"))))(Secondary((id \
         9f7e2e82-a981-4b95-8bba-730f85a013da)(content(Comment\"# arguments \
         mean by experimenting with probes.         #\"))))(Secondary((id \
         558d82e0-7e91-4d44-b65b-952e79dbeb78)(content(Whitespace\"\\n\"))))(Secondary((id \
         2d04d0ce-2f50-42af-9fa9-c68de7d40668)(content(Comment\"#                                                      \
         #\"))))(Secondary((id \
         97dca5ca-9370-4403-9d28-04e72b042dc4)(content(Whitespace\"\\n\"))))(Secondary((id \
         3bdd4b74-c0fe-40e6-aebd-2a314161036e)(content(Comment\"# One of the \
         String arguments is a regex pattern.      #\"))))(Secondary((id \
         3486a04c-a097-46f9-a36d-26f2aef32d5e)(content(Whitespace\"\\n\"))))(Secondary((id \
         baf70d88-1ddc-4b1c-85f2-756d2506e268)(content(Comment\"# Some useful \
         regex building blocks:                   #\"))))(Secondary((id \
         0d02e52a-15db-49fe-a6d9-e44dea8ff8ec)(content(Whitespace\"\\n\"))))(Secondary((id \
         156e485d-73b8-4f0c-a825-92fe8ac61552)(content(Comment\"#   + means \
         \\\"one or more of the preceding\\\"             \
         #\"))))(Secondary((id \
         ff4dcef3-d5eb-4e3f-a535-1c8fa0b16136)(content(Whitespace\"\\n\"))))(Secondary((id \
         805a88ea-ee81-46a8-98ec-f6ecc6a4ecf9)(content(Comment\"#   * means \
         \\\"zero or more of the preceding\\\"            \
         #\"))))(Secondary((id \
         c565ce5b-af1c-47f7-9634-9db0202cdcba)(content(Whitespace\"\\n\"))))(Secondary((id \
         0bacca6b-8f17-4659-b084-02286edff573)(content(Comment\"#   [abc] \
         matches any one character from the set       #\"))))(Secondary((id \
         232f0300-9312-443c-8baa-7a892cc2adad)(content(Whitespace\"\\n\"))))(Secondary((id \
         6ba9c254-1a22-46ba-a1d3-e41f33061bea)(content(Comment\"#   [0-9] \
         matches any digit                            #\"))))(Secondary((id \
         2c515eb1-fa5c-4075-8486-a76e4a027cdd)(content(Whitespace\"\\n\"))))(Secondary((id \
         913eccac-563f-4fcb-9715-01758a260729)(content(Comment\"#   A space in \
         a pattern matches a literal space       #\"))))(Secondary((id \
         92b171a6-db4e-4e67-a560-4b632b31194a)(content(Whitespace\"\\n\"))))(Secondary((id \
         1245e830-edc4-4272-8d94-c9ef7447e248)(content(Comment\"#                                                      \
         #\"))))(Secondary((id \
         abd0b4c5-c7db-4c00-a797-47525ea5f73e)(content(Whitespace\"\\n\"))))(Secondary((id \
         9263e4be-e02a-429f-b01e-7e7e5d481cfb)(content(Comment\"# Tip: Build \
         one step at a time! After each line,      #\"))))(Secondary((id \
         4347cae8-fbcf-43f8-961c-e5ad21b4349b)(content(Whitespace\"\\n\"))))(Secondary((id \
         823cc486-d18e-4d6d-b44c-e87de48edfaf)(content(Comment\"# check the \
         probe to see what your pattern did.        #\"))))(Secondary((id \
         6117db40-63f7-4ea0-b201-2d972d429335)(content(Whitespace\"\\n\"))))(Secondary((id \
         03754b82-9a4c-455a-bfb1-f3e4b6157dec)(content(Whitespace\"\\n\"))))(Tile((id \
         2786d9db-1c25-4998-9c08-35d9b70ed453)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         a827944b-a7be-4840-9162-389b4811906e)(content(Whitespace\" \
         \"))))(Tile((id \
         ac8d1178-7282-4080-b256-c571b5e2a04e)(label(clean_entry))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         b4282fd8-8e83-408d-a389-fee5d6978c54)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 24))(sort Pat))((shape(Concave \
         24))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         1f26121f-318d-4a36-b118-d233c1f59acb)(content(Whitespace\" \
         \"))))(Tile((id \
         9e2d0c25-c42f-4f41-8393-657fd7a18772)(label(String))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         01b1e45f-1591-4974-b693-cefdc783bbb7)(content(Whitespace\" \
         \"))))(Tile((id \
         2a8c3b57-ab94-497b-92c5-fb8227172979)(label(->))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 13))(sort Typ))((shape(Concave \
         13))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         0a3a7cfa-19d0-4718-a916-60894da6b666)(content(Whitespace\" \
         \"))))(Tile((id \
         ec490290-af02-42ed-bb00-5a42c5e85877)(label(String))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         48612f16-6a69-440a-9f96-9d83d7cb9ea9)(content(Whitespace\" \
         \")))))((Secondary((id \
         7e0d76e8-67f0-40be-85a2-6707b9f4b54f)(content(Whitespace\" \
         \"))))(Tile((id 73e9117f-77ab-426a-b1e6-b228e0f6a710)(label(fun \
         ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 37))(sort Exp))))))(shards(0 \
         1))(children(((Secondary((id \
         48efd75e-b25f-420d-afdc-47d83120074e)(content(Whitespace\" \
         \"))))(Tile((id \
         ccafe647-f78f-45d6-9dbf-57509c5680be)(label(entry))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         1a8a60ca-68e6-4892-aabe-e6468d868adc)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         1abc3db9-8cb5-4a78-a537-a4569732ebd2)(content(Whitespace\"\\n\"))))(Tile((id \
         f5dd8797-5013-4fb1-a5e5-62f04e11ae4f)(label(?))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         18943dc9-e03e-42c7-9d82-986b2a448a95)(content(Whitespace\"\\n\"))))(Secondary((id \
         8867af9e-5e00-40d0-aed6-90fa49590578)(content(Whitespace\"\\n\"))))(Secondary((id \
         17954a6a-c3d9-4733-a637-a0e09af05a00)(content(Whitespace\"\\n\"))))(Secondary((id \
         62ca4c55-2113-4f64-8dfa-fe7b42b4af0d)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         a4f87848-5d1a-4bd7-bd45-ef255a5cfe8a)(content(Whitespace\"\\n\"))))(Secondary((id \
         652cfbea-9465-4e46-946d-d85087ca0a62)(content(Whitespace\"\\n\"))))(Tile((id \
         a5fa9ae1-0a83-4557-be3a-e93b2e095e50)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         20d891ee-2041-4632-82d8-0a92d42e36f4)(content(Whitespace\"\\n\"))))(Tile((id \
         17b5dc2d-031a-4df6-b470-4c870d12c7bc)(label(clean_entry))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         aa84aafe-994e-42e8-bd11-e0f90d402768)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         23fcac52-604d-4502-b7da-8c993693401c)(label(\"\\\"  #42 \
         \\240\\159\\140\\149 Full Moon -- clear skies, planted moonbloom  \
         \\\"\"))(mold((out Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0))(children()))))))))(Secondary((id \
         e44be115-d9cf-42c6-9ede-4700c3e8593e)(content(Whitespace\" \
         \"))))(Tile((id \
         07c31690-8878-4f96-a13b-135ef6f7218c)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         cd78f0fa-0ed3-4655-b581-a5951d6b55ef)(content(Whitespace\"\\n\"))))(Tile((id \
         1a6e893c-d5b0-4ea4-b785-31f71bea9604)(label(\"\\\"\\240\\159\\140\\149 \
         Full Moon: clear skies, planted moonbloom\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         9edb171b-4c68-4c13-a1b8-e05ebf5918b7)(content(Whitespace\"\\n\")))))))))(Tile((id \
         5ae894a7-e358-454f-8536-f89a6f49c408)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         10efb7b2-47d3-44ea-9f49-32237d5c592b)(content(Whitespace\"\\n\"))))(Secondary((id \
         2d41b25d-8fa6-438b-9224-f78d6a5853c7)(content(Whitespace\"\\n\"))))(Tile((id \
         8f9a2fca-ff7e-4020-a42f-32669c3a7193)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         fb3622a2-bfc1-47c9-a4c6-dc482d67b3f3)(content(Whitespace\"\\n\"))))(Tile((id \
         e9b6d7fc-2407-415c-899a-05bba4f71734)(label(clean_entry))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         4060a778-8227-446a-9b97-02adb28b3798)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         65610cd8-8d67-41f7-acb5-9a2af8d7dca7)(label(\"\\\"#7 \
         \\240\\159\\140\\145  New Moon--cloudy,   harvested \
         starfern\\\"\"))(mold((out Exp)(in_())(nibs(((shape Convex)(sort \
         Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         5f02d993-a733-4d58-b2bc-4c4cd40b82aa)(content(Whitespace\" \
         \"))))(Tile((id \
         79f80ac8-26ac-4761-97ac-66b950474453)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         5fc91038-caf3-4b7f-a686-0c292c0edce4)(content(Whitespace\"\\n\"))))(Tile((id \
         3c5a6e98-c9bc-4ee8-bb92-f0d4a37bd033)(label(\"\\\"\\240\\159\\140\\145 \
         New Moon: cloudy, harvested starfern\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         5be2cc1e-6805-4e5d-be16-af6c7839da9a)(content(Whitespace\"\\n\")))))))))(Tile((id \
         1c82b69a-b00b-4a49-b92b-8eecd3f7d0c3)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         6334942b-ab9a-4e2f-aee9-a785faf7f3d3)(content(Whitespace\"\\n\"))))(Secondary((id \
         d6613582-31af-44d5-9843-b98ccb754455)(content(Whitespace\"\\n\"))))(Tile((id \
         acadb59d-9748-40d4-8ed1-c09d7f1f5235)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         3ddc2bfd-5787-4fee-8daa-aea6def3c85f)(content(Whitespace\"\\n\"))))(Tile((id \
         37725685-33ce-4a2d-81d0-e21819b2e711)(label(clean_entry))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         a64d26e0-529b-4f0a-8081-014f7c0ff261)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         a11d1fc8-e073-4f56-b437-263dd99ce018)(label(\"\\\"  #103 \
         \\240\\159\\140\\147 Half Moon -- light rain, pruned duskrose  \
         \\\"\"))(mold((out Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0))(children()))))))))(Secondary((id \
         7fd8ec07-2957-42cb-ac5b-35ea46bb5b2a)(content(Whitespace\" \
         \"))))(Tile((id \
         cae8b6d1-2e3c-4c2c-b387-cdc40b7c4911)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         c6e0cc3a-9ead-4e97-9be1-a16a24b396c2)(content(Whitespace\"\\n\"))))(Tile((id \
         22c3e9a1-8901-44ec-ae9b-fd9013944dce)(label(\"\\\"\\240\\159\\140\\147 \
         Half Moon: light rain, pruned duskrose\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         c6437aad-becf-47fe-bb91-2cdd1d017686)(content(Whitespace\"\\n\")))))))))(Tile((id \
         cfb2106d-e25f-490e-8190-1c13692d3efd)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         b4125cce-4369-423f-8bf5-7b2d526c819d)(content(Whitespace\"\\n\"))))(Secondary((id \
         3d8c26d6-28ce-4896-95a5-1ad506066d63)(content(Whitespace\"\\n\"))))(Tile((id \
         fbedff42-9dd1-4a57-8bf6-a581e4876602)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         49db78aa-8a39-468c-89c1-16c3c97f329f)(content(Whitespace\"\\n\"))))(Tile((id \
         806951b2-192b-4b06-9c13-ba22c476e136)(label(clean_entry))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         6c07811f-4db4-44b6-9a45-9a2f82e09de9)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         675981a8-92ed-4590-a88d-ac59f760bd9e)(label(\"\\\"#15 \
         \\240\\159\\140\\151 Crescent--foggy,  checked   moth \
         traps\\\"\"))(mold((out Exp)(in_())(nibs(((shape Convex)(sort \
         Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         95ec1c06-d023-484a-b3bb-409833e005eb)(content(Whitespace\" \
         \"))))(Tile((id \
         e1cd884e-38bd-4a26-947a-87118e216c15)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         427cbd44-8a19-4d96-9ef8-d46dc3631856)(content(Whitespace\"\\n\"))))(Tile((id \
         76a9950a-4b57-4d90-be82-357469bf6784)(label(\"\\\"\\240\\159\\140\\151 \
         Crescent: foggy, checked moth traps\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         ef9545fc-58ee-4a60-88f4-f0b9f427a01d)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         1199f4e3-b7bb-4819-9114-175ac695b95f)(content(Whitespace\"\\n\")))))";
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
