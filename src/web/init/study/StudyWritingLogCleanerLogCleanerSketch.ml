let out : string * Haz3lcore.PersistentSegment.t =
  ( "Study / writing / log-cleaner / log-cleaner-sketch",
    {
      segment =
        "((Secondary((id \
         633bf96c-5e49-4eed-97bf-a588c6200994)(content(Comment\"# MOONPHASE \
         LOG CLEANER TASK                      #\"))))(Secondary((id \
         3a5cd385-950c-4a40-b649-ef6a71d96ddb)(content(Whitespace\"\\n\"))))(Secondary((id \
         4284c528-4d1a-4a77-b4f5-5edda62ed23f)(content(Comment\"#                                                  \
         #\"))))(Secondary((id \
         051ce098-88a5-46ae-b823-38cf8236003f)(content(Whitespace\"\\n\"))))(Secondary((id \
         a8c564f0-a013-43d1-bed1-c20b5407469a)(content(Comment\"# Garden \
         keepers record observations in a messy    #\"))))(Secondary((id \
         ac2d2508-1254-459d-a13d-f943702e2f74)(content(Whitespace\"\\n\"))))(Secondary((id \
         d20855e3-b00a-4245-b028-ace9f9bad151)(content(Comment\"# log with \
         emoji markers, inconsistent dashes,     #\"))))(Secondary((id \
         16298065-2e50-488c-98f5-1b2648b7e1bb)(content(Whitespace\"\\n\"))))(Secondary((id \
         e257adbe-6eaa-4be7-925c-f941533138eb)(content(Comment\"# and extra \
         whitespace. Implement clean_entry to   #\"))))(Secondary((id \
         2ac22bd0-2852-42e1-845a-71dc691babed)(content(Whitespace\"\\n\"))))(Secondary((id \
         f96920c9-b175-45f3-8798-eba70ca1e7f4)(content(Comment\"# standardize \
         each log entry.                      #\"))))(Secondary((id \
         2b66fda8-6daf-49dd-a72c-12951d0a9db4)(content(Whitespace\"\\n\"))))(Secondary((id \
         e3a128ca-ee17-4e29-93b6-b5f42bd97511)(content(Comment\"#                                                  \
         #\"))))(Secondary((id \
         cdb6e7e5-b14c-4a49-bb3d-d48e2f3bee6e)(content(Whitespace\"\\n\"))))(Secondary((id \
         2c28f82d-5cfb-4ecd-8b6f-95fbe12e6d48)(content(Comment\"# Raw entries \
         look like:                           #\"))))(Secondary((id \
         5ab0ef7d-90e9-472f-bf26-df6943653632)(content(Whitespace\"\\n\"))))(Secondary((id \
         94b0e0af-6ef5-404a-971c-200bd0adc677)(content(Comment\"#   \\\"  \
         \\240\\159\\140\\149 Full Moon -- clear skies, planted moonvine  \
         \\\"  #\"))))(Secondary((id \
         31d69638-d305-408d-bbe9-a113674dacbb)(content(Whitespace\"\\n\"))))(Secondary((id \
         c4519330-c609-4892-8e09-03d9a7f404f5)(content(Comment\"#   \
         \\\"\\240\\159\\140\\145  New Moon--cloudy,   harvested \
         starbloom\\\"  #\"))))(Secondary((id \
         b02ebac3-ceb1-46ec-afda-1ffe82a84c3e)(content(Whitespace\"\\n\"))))(Secondary((id \
         18fa190d-3221-4bc7-8a15-5154f8a12680)(content(Comment\"#                                                  \
         #\"))))(Secondary((id \
         fc58c3a2-a3fa-4341-849b-e6525a74d4a2)(content(Whitespace\"\\n\"))))(Secondary((id \
         cb778585-1857-4b00-899a-8b7670b75815)(content(Comment\"# Cleaned \
         entries should look like:                #\"))))(Secondary((id \
         86e2709e-c401-4857-9d6b-d4d9e08801a1)(content(Whitespace\"\\n\"))))(Secondary((id \
         cc478b6a-d5c3-4543-aafc-336db5407482)(content(Comment\"#   \\\"Full \
         Moon: clear skies, planted moonvine\\\"     #\"))))(Secondary((id \
         b4358620-deb3-4efa-a26f-5c6a92a8fdf2)(content(Whitespace\"\\n\"))))(Secondary((id \
         f649d8be-62a1-4062-9f48-addfb1020f4c)(content(Comment\"#   \\\"New \
         Moon: cloudy, harvested starbloom\\\"         #\"))))(Secondary((id \
         93659d8e-b3bb-492a-b1a9-98b5112c8809)(content(Whitespace\"\\n\"))))(Secondary((id \
         4e20d42e-1611-4a79-9bf6-dd86592350e9)(content(Comment\"#                                                  \
         #\"))))(Secondary((id \
         e4cd2966-079a-4f1e-9c3e-0cc8005f9dc2)(content(Whitespace\"\\n\"))))(Secondary((id \
         8ac60f17-1e24-4b70-a66a-66ca6c6b0397)(content(Comment\"# \
         Steps:                                           \
         #\"))))(Secondary((id \
         255aa69f-786a-498d-9f6b-82be4ce0b30d)(content(Whitespace\"\\n\"))))(Secondary((id \
         c905b1bf-3381-4802-9358-e0c12b106098)(content(Comment\"#   1. Trim \
         leading/trailing whitespace            #\"))))(Secondary((id \
         bdcfb5cf-c911-4ec4-891d-288101c74e70)(content(Whitespace\"\\n\"))))(Secondary((id \
         a0f729c7-2d24-4804-8d1f-a0b9adaa5e22)(content(Comment\"#   2. Remove \
         moon emoji symbols                   #\"))))(Secondary((id \
         ca0ad1e7-c6f2-4c5d-b92b-2c135ab41285)(content(Whitespace\"\\n\"))))(Secondary((id \
         de56280d-fb34-443b-8cf6-92847afd6caf)(content(Comment\"#   3. \
         Normalize \\\" -- \\\" or \\\"--\\\" into \\\": \\\"          \
         #\"))))(Secondary((id \
         95ac4c03-b771-4c52-97fc-64dc9fa20ebd)(content(Whitespace\"\\n\"))))(Secondary((id \
         86e75d4b-5b31-497c-a2e1-1d2487847ccb)(content(Comment\"#   4. \
         Collapse multiple spaces into one           #\"))))(Secondary((id \
         902d22b7-f171-40f0-ba74-99b2656ee45a)(content(Whitespace\"\\n\"))))(Secondary((id \
         2e5faefa-1935-436f-a1eb-ec0eafd56d73)(content(Comment\"#   5. Final \
         trim for any leftover edge spaces     #\"))))(Secondary((id \
         66a3148b-105d-4f6e-9844-056d7a11d1ff)(content(Whitespace\"\\n\"))))(Secondary((id \
         86547dc6-8e66-42ee-98e7-84af434031b3)(content(Comment\"#                                                  \
         #\"))))(Secondary((id \
         147f649f-612f-43e1-bdcc-15f01513a62b)(content(Whitespace\"\\n\"))))(Secondary((id \
         f8c47fae-33e1-475a-a49d-9632d10caaa9)(content(Comment\"# Available \
         functions:                             #\"))))(Secondary((id \
         be59fb87-c3ca-419c-8d68-7a8325838bdc)(content(Whitespace\"\\n\"))))(Secondary((id \
         aa984b5e-ba9b-43fb-88af-2c02663cac08)(content(Comment\"#   \
         string_trim(str) -> String                     #\"))))(Secondary((id \
         4253d1c7-b4e0-463f-a6cc-dbe5619fdc6d)(content(Whitespace\"\\n\"))))(Secondary((id \
         015eb595-38c1-4b9a-bb41-7da634dc5b83)(content(Comment\"#   \
         string_replace(pattern, str, replacement)      #\"))))(Secondary((id \
         b23d68ed-12b4-4368-95f6-e0d2035ed9b9)(content(Whitespace\"\\n\"))))(Secondary((id \
         b5e89d64-b309-4322-be24-d651bbee62f9)(content(Comment\"#     -> \
         String (replaces ALL matches)             #\"))))(Secondary((id \
         264be6f4-1fe9-4f3a-9bf9-0d10880a38f5)(content(Whitespace\"\\n\"))))(Secondary((id \
         3ad54aef-f964-41de-b1e7-d6c0ef9cd06f)(content(Comment\"#   \
         string_match(pattern, str) -> Bool             #\"))))(Secondary((id \
         339ba88a-2997-4df1-98a1-c2c67b929716)(content(Whitespace\"\\n\"))))(Secondary((id \
         06a7ee2c-93f3-4f51-9d0a-cbbff8e4cf03)(content(Comment\"#                                                  \
         #\"))))(Secondary((id \
         f1fa810f-6246-4fd2-9370-10c88fdccf2d)(content(Whitespace\"\\n\"))))(Secondary((id \
         0a159eae-0cbc-4b89-a8f6-cd4a81a14568)(content(Comment\"# Patterns are \
         regex:                              #\"))))(Secondary((id \
         26b072b6-3a8e-443f-97ee-7727aa01b483)(content(Whitespace\"\\n\"))))(Secondary((id \
         5347974a-0128-4e1c-a20d-ce947802a610)(content(Comment\"#   \\\" +\\\" \
         matches one or more spaces                #\"))))(Secondary((id \
         a34ce46b-cb1d-4277-9999-46eb8ee753ec)(content(Whitespace\"\\n\"))))(Secondary((id \
         b1737c78-273d-4c7d-9880-70d3a1728b06)(content(Comment\"#   \\\" *-- \
         *\\\" matches -- with optional spaces       #\"))))(Secondary((id \
         e56eedeb-b901-4077-8c4e-543c6bd7dac6)(content(Whitespace\"\\n\"))))(Secondary((id \
         e07b9dfb-0c3c-4c54-95f9-56e6386101b1)(content(Comment\"#   \
         \\\"[abc]\\\" matches any character in the set       \
         #\"))))(Secondary((id \
         0e239de2-7454-4372-986e-a9896a4a7bf2)(content(Whitespace\"\\n\"))))(Secondary((id \
         542b8d41-8ad9-4189-9bec-a7f786596a8f)(content(Comment\"#                                                  \
         #\"))))(Secondary((id \
         f4b198f5-ce04-4f45-b424-1f9f44d8270f)(content(Whitespace\"\\n\"))))(Secondary((id \
         0e2652aa-7f34-4870-9e85-4523715d6805)(content(Comment\"# Tip: Build \
         one step at a time! After each line,  #\"))))(Secondary((id \
         25d1d82e-740b-49fa-b7d5-4ddbaa62c89c)(content(Whitespace\"\\n\"))))(Secondary((id \
         c6e342ef-f149-4bfd-99c1-4c504223f7a7)(content(Comment\"# check the \
         probe to see what your pattern did.    #\"))))(Secondary((id \
         7efdeebd-1191-4787-9427-58c4cb0c1fb5)(content(Whitespace\"\\n\"))))(Secondary((id \
         2e00adfc-6b5f-4fa3-a610-6c5f6f4bb38e)(content(Whitespace\"\\n\"))))(Tile((id \
         15acbf98-ac7f-4f4d-9f6f-bd73b2d978a3)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         b48430cc-bbb3-43d1-8c2b-29d66925e5a0)(content(Whitespace\" \
         \"))))(Tile((id \
         053b539b-5134-4c25-a715-c430f757251c)(label(clean_entry))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         9cff8b9c-1326-4b12-be43-33ae1a3868ea)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 24))(sort Pat))((shape(Concave \
         24))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         ff4e8e4f-60f6-4e22-9496-09983da576c8)(content(Whitespace\" \
         \"))))(Tile((id \
         638a10f4-4eee-407c-910f-1c0aea3d11c0)(label(String))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         80287715-682b-4da9-b44f-9d0e28162fda)(content(Whitespace\" \
         \"))))(Tile((id \
         b4541e47-a6cd-4045-8ca1-228573194b53)(label(->))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 13))(sort Typ))((shape(Concave \
         13))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         d0a23165-25f7-4932-92ad-e4245fdec4a0)(content(Whitespace\" \
         \"))))(Tile((id \
         787446d1-164c-4aa6-b4f0-bba2cc12daaf)(label(String))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         74e2b6b2-821e-48b1-8e62-e55f2bac77af)(content(Whitespace\" \
         \")))))((Secondary((id \
         ef16c0bf-0238-4c14-b153-592628d01e35)(content(Whitespace\" \
         \"))))(Tile((id be4024cd-181d-457f-83fa-29fd471a5fab)(label(fun \
         ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 37))(sort Exp))))))(shards(0 \
         1))(children(((Secondary((id \
         9e4d200d-a16f-4563-b796-086c511703f6)(content(Whitespace\" \
         \"))))(Tile((id \
         b4f6552f-08f4-40bc-8b68-5fdd38e47c9b)(label(entry))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         bfd99c6d-2cd6-4736-9007-1a9997d0e0b7)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         1478693b-9c81-4b42-8de1-a148d136b6c1)(content(Whitespace\"\\n\"))))(Tile((id \
         f4e81b40-3805-48c1-963a-52b7e06e9fe3)(label(?))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         e738564e-4e75-47df-9ce5-7a039f8f2b09)(content(Whitespace\"\\n\"))))(Secondary((id \
         fea91965-266c-4ff7-94b4-5ec09970b613)(content(Whitespace\"\\n\"))))(Secondary((id \
         ada31bf3-3058-475e-9641-505d984d62b1)(content(Whitespace\"\\n\"))))(Secondary((id \
         56561a9c-98a6-444b-ad34-9484f1a0fa91)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         f10a5775-6604-4535-b15d-5193ccadeedd)(content(Whitespace\"\\n\"))))(Secondary((id \
         30a50115-01dd-46b4-a371-90c8747868f4)(content(Whitespace\"\\n\"))))(Tile((id \
         df53e4fd-0d6d-4741-9025-0acff0477bcc)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         70cb27d4-6895-4e6f-b844-b9bec90e067d)(content(Whitespace\"\\n\"))))(Tile((id \
         e2f44fd3-ae96-4998-8d92-54b7607ef7a7)(label(clean_entry))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         f776eca4-f95b-4ba4-8f89-f51f2361a2ed)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         438ab92f-100c-41f6-8a3d-8befb056ce5d)(label(\"\\\"  \
         \\240\\159\\140\\149 Full Moon -- clear skies, planted moonvine  \
         \\\"\"))(mold((out Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0))(children()))))))))(Secondary((id \
         5eb42b9a-7668-4d26-97ca-31ed5b038566)(content(Whitespace\"\\n\"))))(Tile((id \
         44cae452-1ad3-46df-a070-65f1a1312c08)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         505e403d-1913-492c-934e-7d1b747c5597)(content(Whitespace\" \
         \"))))(Tile((id \
         5c001ae1-a573-4904-9192-f5c14e690d06)(label(\"\\\"Full Moon: clear \
         skies, planted moonvine\\\"\"))(mold((out Exp)(in_())(nibs(((shape \
         Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         3b9eb35b-1218-498c-9188-5caacb943240)(content(Whitespace\"\\n\")))))))))(Tile((id \
         0764e681-c606-4d15-b230-7561c58cc231)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         cbc2bcc9-30ef-4ceb-976d-5695e251a8cc)(content(Whitespace\"\\n\"))))(Secondary((id \
         b13582a2-1ee1-4b8f-a123-6142f94b50f6)(content(Whitespace\"\\n\"))))(Tile((id \
         e7a58a97-89b2-43c2-958d-40819ffd4ec3)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         30bf8260-00ec-4d77-9832-2166f6c650af)(content(Whitespace\"\\n\"))))(Tile((id \
         7cac2ea7-ffd3-4ae3-b90f-b708305e10c9)(label(clean_entry))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         6474cfd5-8442-4a47-92d9-a96e1c87828f)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         b7579cd5-f89d-4e17-b961-01128e7623ae)(label(\"\\\"\\240\\159\\140\\145  \
         New Moon--cloudy,   harvested starbloom\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         3f6fb93d-d3e8-42f5-8885-7a78c3365a1a)(content(Whitespace\"\\n\"))))(Tile((id \
         e6b31ef5-7009-4674-a4ba-b71089bd7fd7)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         1707f567-1def-4728-bdcb-c4e7b819dcbd)(content(Whitespace\" \
         \"))))(Tile((id 26b46a44-8190-4420-948e-4843c2a2a775)(label(\"\\\"New \
         Moon: cloudy, harvested starbloom\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         1722ac79-732d-4670-849e-f35794ec852c)(content(Whitespace\"\\n\")))))))))(Tile((id \
         1d6c2562-16c4-4ef4-94d9-3126e8702dd4)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         36e79d04-81af-4a21-8a92-777810f05df9)(content(Whitespace\"\\n\"))))(Secondary((id \
         b1b2b7e4-474f-4da5-b2cc-420886374641)(content(Whitespace\"\\n\"))))(Tile((id \
         1a7ca06d-a595-45eb-a75d-6a8b75e58fa2)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         8e483c73-8efd-4edf-b7d3-2a53ac588ecc)(content(Whitespace\"\\n\"))))(Tile((id \
         74845c1c-d8f6-4472-b988-63aeb09473f0)(label(clean_entry))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         059e4127-d770-4a95-9430-96841da346e2)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         3f48e5de-50c2-461b-8b43-68e0b12adb3f)(label(\"\\\"  \
         \\240\\159\\140\\147 Half Moon -- light rain, pruned thornrose  \
         \\\"\"))(mold((out Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0))(children()))))))))(Secondary((id \
         f59ed5d0-6d84-419c-96b4-540391e8bdff)(content(Whitespace\"\\n\"))))(Tile((id \
         5633c0fb-3131-4014-84a0-e0f6e92a1839)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         0b323364-83d0-45c0-ada1-7280deac2fec)(content(Whitespace\" \
         \"))))(Tile((id \
         1e1a184f-5233-45a9-841f-32b57f1e6164)(label(\"\\\"Half Moon: light \
         rain, pruned thornrose\\\"\"))(mold((out Exp)(in_())(nibs(((shape \
         Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         607b07a9-1016-4c92-b55f-bcc77e7e987b)(content(Whitespace\"\\n\")))))))))(Tile((id \
         9f5d0f36-cc71-4cbf-ad76-a8c26dc4c08d)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         c004a289-8aa6-4f20-9e2c-1a1d27381315)(content(Whitespace\"\\n\"))))(Secondary((id \
         a1862e87-e811-40e1-bb41-3e498cdf860f)(content(Whitespace\"\\n\"))))(Tile((id \
         a5942dd0-4e49-4c6f-a6ef-0e89044067f7)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         ecd742a3-0490-4515-815c-ebd4caef26d0)(content(Whitespace\"\\n\"))))(Tile((id \
         3c8b59f3-f92a-4695-9321-a3f6786d2b8b)(label(clean_entry))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         db6e2f1d-df69-41f4-9c89-c102a71a6144)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         cfc28da4-78ee-4b9d-8767-87a727246316)(label(\"\\\"\\240\\159\\140\\151 \
         Crescent--foggy,  checked   moth traps\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         43380098-e882-4f12-aac5-f562cde3f652)(content(Whitespace\"\\n\"))))(Tile((id \
         66a05104-7bce-4829-8076-75d91cd9b4b5)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         60e57951-56bf-47b3-8277-223fb93866f5)(content(Whitespace\" \
         \"))))(Tile((id \
         053ad474-7cf0-4482-bab3-d26f3a73ec8a)(label(\"\\\"Crescent: foggy, \
         checked moth traps\\\"\"))(mold((out Exp)(in_())(nibs(((shape \
         Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         48640b06-f3a8-4bb8-92e9-56d1f417e11e)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         c1f066cf-5be5-49ea-b916-c087fef0146c)(content(Whitespace\"\\n\")))))";
      backup_text =
        "# MOONPHASE LOG CLEANER TASK                      #\n\
         #                                                  #\n\
         # Garden keepers record observations in a messy    #\n\
         # log with emoji markers, inconsistent dashes,     #\n\
         # and extra whitespace. Implement clean_entry to   #\n\
         # standardize each log entry.                      #\n\
         #                                                  #\n\
         # Raw entries look like:                           #\n\
         #   \"  \240\159\140\149 Full Moon -- clear skies, planted moonvine  \
         \"  #\n\
         #   \"\240\159\140\145  New Moon--cloudy,   harvested starbloom\"  #\n\
         #                                                  #\n\
         # Cleaned entries should look like:                #\n\
         #   \"Full Moon: clear skies, planted moonvine\"     #\n\
         #   \"New Moon: cloudy, harvested starbloom\"         #\n\
         #                                                  #\n\
         # Steps:                                           #\n\
         #   1. Trim leading/trailing whitespace            #\n\
         #   2. Remove moon emoji symbols                   #\n\
         #   3. Normalize \" -- \" or \"--\" into \": \"          #\n\
         #   4. Collapse multiple spaces into one           #\n\
         #   5. Final trim for any leftover edge spaces     #\n\
         #                                                  #\n\
         # Available functions:                             #\n\
         #   string_trim(str) -> String                     #\n\
         #   string_replace(pattern, str, replacement)      #\n\
         #     -> String (replaces ALL matches)             #\n\
         #   string_match(pattern, str) -> Bool             #\n\
         #                                                  #\n\
         # Patterns are regex:                              #\n\
         #   \" +\" matches one or more spaces                #\n\
         #   \" *-- *\" matches -- with optional spaces       #\n\
         #   \"[abc]\" matches any character in the set       #\n\
         #                                                  #\n\
         # Tip: Build one step at a time! After each line,  #\n\
         # check the probe to see what your pattern did.    #\n\n\
         let clean_entry: String -> String = fun entry ->\n\
         ?\n\n\n\n\
         in\n\n\
         test\n\
         clean_entry(\"  \240\159\140\149 Full Moon -- clear skies, planted \
         moonvine  \")\n\
         == \"Full Moon: clear skies, planted moonvine\"\n\
         end;\n\n\
         test\n\
         clean_entry(\"\240\159\140\145  New Moon--cloudy,   harvested \
         starbloom\")\n\
         == \"New Moon: cloudy, harvested starbloom\"\n\
         end;\n\n\
         test\n\
         clean_entry(\"  \240\159\140\147 Half Moon -- light rain, pruned \
         thornrose  \")\n\
         == \"Half Moon: light rain, pruned thornrose\"\n\
         end;\n\n\
         test\n\
         clean_entry(\"\240\159\140\151 Crescent--foggy,  checked   moth \
         traps\")\n\
         == \"Crescent: foggy, checked moth traps\"\n\
         end\n";
      refractors = "()";
    } )
