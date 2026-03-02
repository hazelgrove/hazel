let out : string * Haz3lcore.PersistentSegment.t =
  ( "Study / writing / log-cleaner / log-cleaner-sketch",
    {
      segment =
        "((Secondary((id \
         f05d068e-e3c7-4df4-830a-fc2de82862b6)(content(Comment\"# Moonphase \
         Log Cleaner                            #\"))))(Secondary((id \
         10fedd0b-aa73-44a4-9d45-f893f0b2c9a0)(content(Whitespace\"\\n\"))))(Secondary((id \
         b90740b4-9dc5-494e-923d-9cd7db5447d7)(content(Comment\"#                                                  \
         #\"))))(Secondary((id \
         0d0cdf22-3695-41ab-a410-ab31a93e9172)(content(Whitespace\"\\n\"))))(Secondary((id \
         d41bcede-7031-446f-95aa-3c534ef95a80)(content(Comment\"# Garden \
         keepers record observations in a messy    #\"))))(Secondary((id \
         43015751-0946-4cca-9d7e-86bccd9bbd3a)(content(Whitespace\"\\n\"))))(Secondary((id \
         adba40d2-99c1-44a0-bfc9-5df9bd708b02)(content(Comment\"# log with \
         emoji markers, inconsistent dashes,     #\"))))(Secondary((id \
         783f6866-ba9c-4410-8a3a-40aad812d203)(content(Whitespace\"\\n\"))))(Secondary((id \
         2fe2d3bc-fb05-4869-a259-ee9fd780d3fa)(content(Comment\"# and extra \
         whitespace. Implement clean_entry to   #\"))))(Secondary((id \
         eb3aa462-f662-4a10-9026-4f162dbfc0a6)(content(Whitespace\"\\n\"))))(Secondary((id \
         d885ba9d-abbf-4d65-8b27-1706146f1d0a)(content(Comment\"# standardize \
         each log entry.                      #\"))))(Secondary((id \
         960edd40-9991-432d-ab36-98cdae7137bd)(content(Whitespace\"\\n\"))))(Secondary((id \
         92d1edda-ed21-4696-b087-f8099b6c9d48)(content(Comment\"#                                                  \
         #\"))))(Secondary((id \
         9af07396-dc03-4398-a068-56c51056775b)(content(Whitespace\"\\n\"))))(Secondary((id \
         517bec84-7819-43a8-874b-15b4c4535190)(content(Comment\"# Raw entries \
         look like:                           #\"))))(Secondary((id \
         2e1d6368-a330-410a-a1b7-be36475a92f7)(content(Whitespace\"\\n\"))))(Secondary((id \
         10944c32-c28c-4dda-8ecf-c3158185cbb3)(content(Comment\"#   \\\"  \
         \\240\\159\\140\\149 Full Moon -- clear skies, planted moonbloom  \
         \\\"  #\"))))(Secondary((id \
         3d323c85-0b15-478b-a078-aafa2b6c9ae3)(content(Whitespace\"\\n\"))))(Secondary((id \
         5077a34c-25df-400b-a465-8e42a7bd3bb8)(content(Comment\"#   \
         \\\"\\240\\159\\140\\145  New Moon--cloudy,   harvested starfern\\\"  \
         #\"))))(Secondary((id \
         10e1940f-042b-404b-97ce-7409e6ec09b3)(content(Whitespace\"\\n\"))))(Secondary((id \
         4492f982-5df3-4770-926a-75a91aaad362)(content(Comment\"#                                                  \
         #\"))))(Secondary((id \
         e706296b-d835-402a-b1b0-0c4e72b12c04)(content(Whitespace\"\\n\"))))(Secondary((id \
         d8740670-3e71-4847-b11b-72e45e727ccc)(content(Comment\"# Cleaned \
         entries should look like:                #\"))))(Secondary((id \
         3acbd918-71e7-422e-bf80-b7f604a34fc5)(content(Whitespace\"\\n\"))))(Secondary((id \
         28ced709-f7ca-45a4-ba74-6a417ba8b8f7)(content(Comment\"#   \\\"Full \
         Moon: clear skies, planted moonbloom\\\"     #\"))))(Secondary((id \
         ea228cb2-d34d-4fe2-86aa-28c721009b95)(content(Whitespace\"\\n\"))))(Secondary((id \
         ede75d69-5507-4379-b32f-da17537f320b)(content(Comment\"#   \\\"New \
         Moon: cloudy, harvested starfern\\\"         #\"))))(Secondary((id \
         9dac00c9-37e4-45ad-a886-afef41a50650)(content(Whitespace\"\\n\"))))(Secondary((id \
         1f7276d3-1ac9-4da8-a2b4-9af147886196)(content(Comment\"#                                                  \
         #\"))))(Secondary((id \
         0a9c5929-4c46-4185-b31c-63280e11db5e)(content(Whitespace\"\\n\"))))(Secondary((id \
         33b77fdd-c11b-4722-9a0f-114941d78bb5)(content(Comment\"# \
         Steps:                                           \
         #\"))))(Secondary((id \
         cdb2b5e5-8627-442a-8dd8-72fb713e1cc3)(content(Whitespace\"\\n\"))))(Secondary((id \
         72eae80e-8ed1-4431-bc3d-f174e371f24c)(content(Comment\"#   1. Trim \
         leading/trailing whitespace            #\"))))(Secondary((id \
         696cbaea-9fd4-4d0a-81e7-973b9a43929b)(content(Whitespace\"\\n\"))))(Secondary((id \
         d09a2466-9bf9-436a-8fba-bf797215cd5e)(content(Comment\"#   2. Remove \
         moon emoji symbols                   #\"))))(Secondary((id \
         cdb44513-c996-45c3-a4ab-5380ab355a18)(content(Whitespace\"\\n\"))))(Secondary((id \
         75fc3fe6-bd00-4821-9c8b-b2941d845c6a)(content(Comment\"#   3. \
         Normalize \\\" -- \\\" or \\\"--\\\" into \\\": \\\"          \
         #\"))))(Secondary((id \
         07085912-ac8e-4c4d-a857-7dc24323352e)(content(Whitespace\"\\n\"))))(Secondary((id \
         3ff6a5c1-09d4-454c-9127-25a2d3741aa1)(content(Comment\"#   4. \
         Collapse multiple spaces into one           #\"))))(Secondary((id \
         da9cdf7c-f8bd-4b5c-8ab8-0629aec23223)(content(Whitespace\"\\n\"))))(Secondary((id \
         ac4d0b19-991c-443c-a838-a9f5034daf37)(content(Comment\"#   5. Final \
         trim for any leftover edge spaces     #\"))))(Secondary((id \
         a97d745c-d307-4206-9b37-27b72538d043)(content(Whitespace\"\\n\"))))(Secondary((id \
         0b0cb7de-84e4-4d87-b0e8-cfa65a6a82b4)(content(Comment\"#                                                  \
         #\"))))(Secondary((id \
         ce7d7542-2c60-45e3-8d02-1ac2c18ecf8f)(content(Whitespace\"\\n\"))))(Secondary((id \
         0bc1c828-4aba-46a8-a7df-eed1a0f5acf7)(content(Comment\"# Available \
         functions:                             #\"))))(Secondary((id \
         c2b83fbe-20fb-4488-a760-92ebc17f283d)(content(Whitespace\"\\n\"))))(Secondary((id \
         f87d3ad2-e053-4fe5-bdd9-540b0110edda)(content(Comment\"#   \
         string_trim(str) -> String                     #\"))))(Secondary((id \
         cd0defcc-4742-480e-bbd4-0372d0ae53eb)(content(Whitespace\"\\n\"))))(Secondary((id \
         3fa5920d-bb89-4ca8-a31f-f05477768f74)(content(Comment\"#   \
         string_replace(pattern, str, replacement)      #\"))))(Secondary((id \
         fdf4aad6-a9c2-4e71-91df-b7369cc1a82f)(content(Whitespace\"\\n\"))))(Secondary((id \
         959e67c9-97b2-478b-ad33-c4b791d0a1ba)(content(Comment\"#     -> \
         String (replaces ALL matches)             #\"))))(Secondary((id \
         e2affdc7-09a0-49df-87f1-2d9caa5ef442)(content(Whitespace\"\\n\"))))(Secondary((id \
         4cfbc6a4-7667-49ba-af5d-cccc17c7e0c4)(content(Comment\"#   \
         string_match(pattern, str) -> Bool             #\"))))(Secondary((id \
         05e5f5e9-63db-411a-81f9-24159a17b8ab)(content(Whitespace\"\\n\"))))(Secondary((id \
         154a6f48-3f44-430c-8043-3f5d9b4701ea)(content(Comment\"#                                                  \
         #\"))))(Secondary((id \
         b89a171a-8cc1-44be-a253-5a4721dd0777)(content(Whitespace\"\\n\"))))(Secondary((id \
         bfcac847-5550-4328-b030-0a5ed3213fd8)(content(Comment\"# Patterns are \
         regex:                              #\"))))(Secondary((id \
         0a3a8b41-b5ad-4bcc-80c2-407e13a9ecee)(content(Whitespace\"\\n\"))))(Secondary((id \
         abcba84e-2a1a-42d6-8d31-85efac68b597)(content(Comment\"#   \\\" +\\\" \
         matches one or more spaces                #\"))))(Secondary((id \
         da67ffb0-b653-43d4-94a2-34bd45e6f309)(content(Whitespace\"\\n\"))))(Secondary((id \
         dbcc0956-41ee-46bd-b3b5-ff07e4dd641f)(content(Comment\"#   \\\" *-- \
         *\\\" matches -- with optional spaces       #\"))))(Secondary((id \
         9e0db846-205a-4dfa-946c-b3dd0c7d3599)(content(Whitespace\"\\n\"))))(Secondary((id \
         09f07cd6-70ed-46af-9572-bb933679e03d)(content(Comment\"#   \
         \\\"[abc]\\\" matches any character in the set       \
         #\"))))(Secondary((id \
         5fbcd1aa-3c46-40b0-9ff1-5f2207377cb9)(content(Whitespace\"\\n\"))))(Secondary((id \
         60db3b33-f69b-4938-a159-f603218681f5)(content(Comment\"#                                                  \
         #\"))))(Secondary((id \
         c64fd3ca-3965-4d40-9bc5-92702e06c005)(content(Whitespace\"\\n\"))))(Secondary((id \
         9f419833-6d2e-4029-9b0f-58aafb84671a)(content(Comment\"# Tip: Build \
         one step at a time! After each line,  #\"))))(Secondary((id \
         9412f3a1-f506-45fb-ba82-f9864157f178)(content(Whitespace\"\\n\"))))(Secondary((id \
         5a2052f1-3d42-474a-8bad-5fa4242e56ca)(content(Comment\"# check the \
         probe to see what your pattern did.    #\"))))(Secondary((id \
         db7b2ef4-5fd6-46cb-9b84-cafcb931cf83)(content(Whitespace\"\\n\"))))(Secondary((id \
         5b39a158-365e-4745-aaf6-64994b9f28b3)(content(Whitespace\"\\n\"))))(Tile((id \
         8778909d-49be-4655-87ef-ecafa64050d7)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         1aefc0be-fa40-4611-8f8e-7b7a6f4956f7)(content(Whitespace\" \
         \"))))(Tile((id \
         cb6f8ed3-aa4b-42f8-bf2f-c53e900a0198)(label(clean_entry))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         c8a76132-0512-4e20-8409-c05222c72c12)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 24))(sort Pat))((shape(Concave \
         24))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         309f10a5-68ef-41cc-8ae6-be414e008254)(content(Whitespace\" \
         \"))))(Tile((id \
         f7b272ea-8d76-4dc9-a3ea-c0ed1061b9ba)(label(String))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         79bb3d86-fef2-4a18-8944-a3e917c2ab19)(content(Whitespace\" \
         \"))))(Tile((id \
         c6acddd4-157b-4bec-8a38-423356f74d6e)(label(->))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 13))(sort Typ))((shape(Concave \
         13))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         977c075b-da28-41c4-8be0-8b10eb8e251c)(content(Whitespace\" \
         \"))))(Tile((id \
         66c437dc-5354-4791-a7ae-ef321d6b50e4)(label(String))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         61ac64cf-dfb3-4e4e-825d-ac549ca48fe4)(content(Whitespace\" \
         \")))))((Secondary((id \
         3b5b0560-0147-4f3e-8753-a5802e3908b2)(content(Whitespace\" \
         \"))))(Tile((id 2f0f7aa8-4ce8-453f-a2a2-cfe0de6fb643)(label(fun \
         ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 37))(sort Exp))))))(shards(0 \
         1))(children(((Secondary((id \
         de1269a0-0004-4350-b732-1288b3b1035b)(content(Whitespace\" \
         \"))))(Tile((id \
         6bae921a-afe3-427e-bc61-4be072cfb66a)(label(entry))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         62a7b040-6952-4767-bd0c-38fa96eba061)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         f72aa68b-214e-4f64-83ec-2794e7d7b5dd)(content(Whitespace\"\\n\"))))(Tile((id \
         52396a03-b70b-45b1-9a64-f5e90dace5fb)(label(?))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         7fcb2821-e0d8-46fb-8b7e-7b9a0a42eeb0)(content(Whitespace\"\\n\"))))(Secondary((id \
         e9f44452-c5bb-4564-9a1f-41fe16fe651c)(content(Whitespace\"\\n\"))))(Secondary((id \
         955c8f51-b75c-4259-a57d-37a10381dd98)(content(Whitespace\"\\n\"))))(Secondary((id \
         913d5be3-47b9-4ea3-a64e-05cef23d28ce)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         049f983f-8411-45b9-baaa-f9b91bfcb463)(content(Whitespace\"\\n\"))))(Secondary((id \
         333f1802-4d24-4447-8f10-a2d333179e2c)(content(Whitespace\"\\n\"))))(Tile((id \
         358bc589-b438-4863-818f-6280864b3b01)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         eb1ecb5a-06a0-4fc8-8fe2-535e0ad5188b)(content(Whitespace\"\\n\"))))(Tile((id \
         85f96d93-829b-46b9-b086-32d287978abc)(label(clean_entry))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         75677786-b7da-40b8-bdf8-0d3a91e4e8cb)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         93ac5102-529b-4b70-a708-64fa2cf1475e)(label(\"\\\"  \
         \\240\\159\\140\\149 Full Moon -- clear skies, planted moonbloom  \
         \\\"\"))(mold((out Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0))(children()))))))))(Secondary((id \
         83a83402-ff70-474b-82c6-9645f0b371d6)(content(Whitespace\"\\n\"))))(Tile((id \
         653c1dac-813e-4138-8f89-3ac2e412ce29)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         42957c57-f2fe-4cdd-acc5-d19fb9855ed3)(content(Whitespace\" \
         \"))))(Tile((id \
         47a360d8-7ec9-427a-8048-b513151443c6)(label(\"\\\"Full Moon: clear \
         skies, planted moonbloom\\\"\"))(mold((out Exp)(in_())(nibs(((shape \
         Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         41330af1-6b2e-4731-96c1-81f1050f2c1e)(content(Whitespace\"\\n\")))))))))(Tile((id \
         93cf4f10-6313-4b6d-9901-4e95625d9603)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         a844570f-75b5-44d6-bbe3-0bf712a1a225)(content(Whitespace\"\\n\"))))(Secondary((id \
         856fe078-1671-47b0-9e23-caaec9690c04)(content(Whitespace\"\\n\"))))(Tile((id \
         8f695a05-7661-44fe-8e85-ff12e89a39f6)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         af195e46-3a63-44e2-9556-3be44ffeb83e)(content(Whitespace\"\\n\"))))(Tile((id \
         0cb28af7-30a6-44c8-bb29-4800df4f5a94)(label(clean_entry))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         1e594531-8080-4984-b51b-34a9dde97f99)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         509faed8-6644-4ad2-b0dc-4d8a064a1f6a)(label(\"\\\"\\240\\159\\140\\145  \
         New Moon--cloudy,   harvested starfern\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         c55de226-5f4e-48ae-b7ee-f5b49b03ae63)(content(Whitespace\"\\n\"))))(Tile((id \
         aff95321-620f-4797-90b8-84b37b5d69e5)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         e221a54c-6191-4212-966e-79aecfe33dd0)(content(Whitespace\" \
         \"))))(Tile((id 73b7f38f-cb31-4ad3-9a77-9981e9747ad1)(label(\"\\\"New \
         Moon: cloudy, harvested starfern\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         317a6f2b-3f39-4c30-928d-48b25ad41d81)(content(Whitespace\"\\n\")))))))))(Tile((id \
         5cca8e46-88bf-4930-975f-99adfafde290)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         da7a9ca6-daf7-4f9e-9cbf-1ac7e20561e7)(content(Whitespace\"\\n\"))))(Secondary((id \
         a829de8a-34a4-4c33-a48a-e88d4fef2359)(content(Whitespace\"\\n\"))))(Tile((id \
         dd0255e1-deca-4780-943e-9a819da5d970)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         39062e52-2478-4f52-962e-7aefc3456fcc)(content(Whitespace\"\\n\"))))(Tile((id \
         c3d8b6c1-b5d7-4874-b74b-e1ac75b8800c)(label(clean_entry))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         3d5acf36-a957-436d-8b80-2db4d2a545e4)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         0f7f8036-a5f6-4bda-bd61-b5d88c35386b)(label(\"\\\"  \
         \\240\\159\\140\\147 Half Moon -- light rain, pruned duskrose  \
         \\\"\"))(mold((out Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0))(children()))))))))(Secondary((id \
         9644651a-7be2-425e-a643-866c5bdfbbf1)(content(Whitespace\"\\n\"))))(Tile((id \
         9506413a-ebaf-4e5f-b87e-355855533b5f)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         c1cc5f8f-262a-42d8-9de8-ea455b11ef25)(content(Whitespace\" \
         \"))))(Tile((id \
         e7adb597-e8f1-4744-b478-515bafd9831b)(label(\"\\\"Half Moon: light \
         rain, pruned duskrose\\\"\"))(mold((out Exp)(in_())(nibs(((shape \
         Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         45560863-fb05-4dde-a0d6-1019e6f1468d)(content(Whitespace\"\\n\")))))))))(Tile((id \
         1c260f9b-e0d6-4f72-a269-9f0893ee4fd9)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         9258c531-e427-40b2-a6bf-ce19889503cf)(content(Whitespace\"\\n\"))))(Secondary((id \
         c07c8ebe-02c1-4075-a22e-c28e0804d2c9)(content(Whitespace\"\\n\"))))(Tile((id \
         6b319ea9-d458-409d-b911-f8a79a4dba4f)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         82cc82e4-fdba-4823-8dcc-5f35d310f1e4)(content(Whitespace\"\\n\"))))(Tile((id \
         c562baf3-3023-4ae8-b859-6b7010e26aa1)(label(clean_entry))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         158607db-6ae3-41c9-a5a9-db0a67a8543c)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         1819e350-ff62-44c0-a55e-be11250a31a6)(label(\"\\\"\\240\\159\\140\\151 \
         Crescent--foggy,  checked   moth traps\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         c8925840-cbe0-4401-a579-05e1f4973e86)(content(Whitespace\"\\n\"))))(Tile((id \
         d73e50e5-c928-4ad2-8302-de5dee604571)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         d39f9b19-f7bd-478b-a7e7-f784cd0158fa)(content(Whitespace\" \
         \"))))(Tile((id \
         8728d9f3-2ffd-4eb0-b62c-a1559e4a5b22)(label(\"\\\"Crescent: foggy, \
         checked moth traps\\\"\"))(mold((out Exp)(in_())(nibs(((shape \
         Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         d9b34601-5d9c-4ec2-83d8-afe5ef2225ca)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         2ddf70fc-6238-4826-9793-2e849c2dff8a)(content(Whitespace\"\\n\")))))";
      backup_text =
        "# Moonphase Log Cleaner                            #\n\
         #                                                  #\n\
         # Garden keepers record observations in a messy    #\n\
         # log with emoji markers, inconsistent dashes,     #\n\
         # and extra whitespace. Implement clean_entry to   #\n\
         # standardize each log entry.                      #\n\
         #                                                  #\n\
         # Raw entries look like:                           #\n\
         #   \"  \240\159\140\149 Full Moon -- clear skies, planted moonbloom  \
         \"  #\n\
         #   \"\240\159\140\145  New Moon--cloudy,   harvested starfern\"  #\n\
         #                                                  #\n\
         # Cleaned entries should look like:                #\n\
         #   \"Full Moon: clear skies, planted moonbloom\"     #\n\
         #   \"New Moon: cloudy, harvested starfern\"         #\n\
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
         moonbloom  \")\n\
         == \"Full Moon: clear skies, planted moonbloom\"\n\
         end;\n\n\
         test\n\
         clean_entry(\"\240\159\140\145  New Moon--cloudy,   harvested \
         starfern\")\n\
         == \"New Moon: cloudy, harvested starfern\"\n\
         end;\n\n\
         test\n\
         clean_entry(\"  \240\159\140\147 Half Moon -- light rain, pruned \
         duskrose  \")\n\
         == \"Half Moon: light rain, pruned duskrose\"\n\
         end;\n\n\
         test\n\
         clean_entry(\"\240\159\140\151 Crescent--foggy,  checked   moth \
         traps\")\n\
         == \"Crescent: foggy, checked moth traps\"\n\
         end\n";
      refractors = "()";
    } )
