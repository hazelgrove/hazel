let out : string * Haz3lcore.PersistentSegment.t =
  ( "Study / tasks-draft / 01W-garden-path",
    {
      segment =
        "((Secondary((id \
         731eab24-ceec-4843-b0b1-334b043e0bef)(content(Comment\"# GARDEN PATH \
         TASK                               #\"))))(Secondary((id \
         eb5e8718-2efe-4a41-af60-03c5df159ad0)(content(Whitespace\"\\n\"))))(Secondary((id \
         af0b9fbb-b77d-4a28-94d0-bcd6663567a5)(content(Comment\"#                                                \
         #\"))))(Secondary((id \
         e5e6aea2-dd4c-4c78-80c6-2ce8ad3c9d2c)(content(Whitespace\"\\n\"))))(Secondary((id \
         26a4366f-c778-4608-9901-6182fc87af21)(content(Comment\"# Implement \
         grove_name: extract the first        #\"))))(Secondary((id \
         3756bd19-68ce-4b11-a9c5-1fc8fa351a90)(content(Whitespace\"\\n\"))))(Secondary((id \
         35e4487a-6e01-4059-80f0-a60000a6b718)(content(Comment\"# section from \
         a garden path.                    #\"))))(Secondary((id \
         dfc4cea9-e9c2-455a-8a6f-4537353088df)(content(Whitespace\"\\n\"))))(Secondary((id \
         927eb2bc-ef97-4717-ab84-7e85a20ea7a1)(content(Comment\"#                                                \
         #\"))))(Secondary((id \
         86d401f8-c928-489a-95d8-856c5f14d300)(content(Whitespace\"\\n\"))))(Secondary((id \
         60278dca-35f5-4864-a809-a5673fd38223)(content(Comment\"# \
         Examples:                                      #\"))))(Secondary((id \
         7788209b-2506-4f48-b29e-f9f6ef1ba06c)(content(Whitespace\"\\n\"))))(Secondary((id \
         cf038a63-e4b5-4011-b67c-10d99d7cf948)(content(Comment\"#   \
         grove_name(\\\"/moonlit-grove/ferns\\\")           \
         #\"))))(Secondary((id \
         392e71a8-09aa-4f85-ab6e-9efd2e4958d9)(content(Whitespace\"\\n\"))))(Secondary((id \
         02747d13-b8d3-4c70-8847-ba555e03c4e9)(content(Comment\"#     == \
         \\\"moonlit-grove\\\"                         #\"))))(Secondary((id \
         cdba7bec-db71-4b97-ac8d-c829f70288eb)(content(Whitespace\"\\n\"))))(Secondary((id \
         76ff86eb-08d8-4f5d-b53e-9a5365788e05)(content(Comment\"#   \
         grove_name(\\\"/night-garden/herbs/thyme\\\")      \
         #\"))))(Secondary((id \
         428ece7e-9b2e-421f-90f4-3f85cac57d5a)(content(Whitespace\"\\n\"))))(Secondary((id \
         8a17dde6-f0e5-4f77-a12b-a9c1368d2dd2)(content(Comment\"#     == \
         \\\"night-garden\\\"                          #\"))))(Secondary((id \
         11d4f49a-b886-436f-8244-14b37fd32a29)(content(Whitespace\"\\n\"))))(Secondary((id \
         3a306eda-88e4-4516-93ca-f7b00a00e1ef)(content(Comment\"#   \
         grove_name(\\\"/\\\") == \\\"\\\"                        \
         #\"))))(Secondary((id \
         6e5dac75-392d-454b-8143-170363616675)(content(Whitespace\"\\n\"))))(Secondary((id \
         67fc4c6f-cc08-4167-8ec4-d9e3d2d45eeb)(content(Comment\"#                                                \
         #\"))))(Secondary((id \
         a7902ee8-4858-4eb9-b5e3-53a5ad355a07)(content(Whitespace\"\\n\"))))(Secondary((id \
         1295d40e-ec85-4bbe-997a-57f6db21eeba)(content(Comment\"# Available \
         functions:                           #\"))))(Secondary((id \
         4bda6999-28a0-4ed4-843e-44bd81949cbf)(content(Whitespace\"\\n\"))))(Secondary((id \
         cc96ea92-a5d4-43ff-809b-171e78e652fa)(content(Comment\"#   \
         string_split(sep, str) -> [String]           #\"))))(Secondary((id \
         ee33f6d3-4db6-4bd3-908c-908682d76601)(content(Whitespace\"\\n\"))))(Secondary((id \
         5fb7f1cc-4a66-43ec-9fd7-f77ddec8c77f)(content(Comment\"#   \
         string_concat(s1, s2) -> String              #\"))))(Secondary((id \
         4c5c7763-3708-4deb-ba5a-d378a570502d)(content(Whitespace\"\\n\"))))(Secondary((id \
         3d86b35e-412b-4be2-9e48-104af0e63346)(content(Comment\"#   \
         string_length(s) -> Int                      #\"))))(Secondary((id \
         d0bf5a70-c8f3-4cf0-aec0-fea9f1c9f1cd)(content(Whitespace\"\\n\"))))(Secondary((id \
         e70844c3-a75e-40a9-874c-43509f3cd59b)(content(Comment\"#   \
         string_sub(str, pos, len) -> String          #\"))))(Secondary((id \
         cfe8677c-a07f-4c24-8aec-61774559637b)(content(Whitespace\"\\n\"))))(Secondary((id \
         bb64f7aa-3fb0-40af-95dc-5a5edb3882b0)(content(Comment\"#   nth(list, \
         index) -> element                  #\"))))(Secondary((id \
         96da1eb6-1b22-49d9-be3c-314967fb8841)(content(Whitespace\"\\n\"))))(Secondary((id \
         cc0dfccb-77e7-48b1-a30d-053ed665f73b)(content(Comment\"#   \
         length(list) -> Int                          #\"))))(Secondary((id \
         ca31597c-aefc-4270-a208-6828cf28343d)(content(Whitespace\"\\n\"))))(Secondary((id \
         ea6ec272-05c8-4855-b980-f5c836946029)(content(Comment\"#   map(list, \
         fn) -> list                        #\"))))(Secondary((id \
         b522c977-af44-4cdd-8bb9-e0a6e7d2c5b9)(content(Whitespace\"\\n\"))))(Secondary((id \
         a53a9271-0648-40bf-9eac-94f2e6b39e12)(content(Comment\"#   \
         filter(list, pred) -> list                   #\"))))(Secondary((id \
         74ae06c1-1d3c-4c8d-9950-bde3f8ae8b59)(content(Whitespace\"\\n\"))))(Secondary((id \
         eb5e0cf8-145c-43fa-986b-54047fccf895)(content(Comment\"#                                                \
         #\"))))(Secondary((id \
         1c962876-1fdb-48f4-90bf-a24f95838b18)(content(Whitespace\"\\n\"))))(Secondary((id \
         fd344811-0bf4-43b5-957c-3b8421c4ecca)(content(Comment\"# Function \
         syntax: fun param -> body             #\"))))(Secondary((id \
         6cbbd85b-2e83-4ee8-b73c-bcb8eff79e04)(content(Whitespace\"\\n\"))))(Secondary((id \
         85ddf736-6c84-4a95-8734-a3dfba7ebb9e)(content(Comment\"# Let binding: \
         let name = value in ...           #\"))))(Secondary((id \
         7c1ba58d-4089-44c6-945d-f0f13dd8e2a4)(content(Whitespace\"\\n\"))))(Secondary((id \
         a8bbcd34-d5a1-452e-bbcf-f0971936c403)(content(Comment\"#                                                \
         #\"))))(Secondary((id \
         4ce67692-7c3f-4758-b0a2-bc32580563cc)(content(Whitespace\"\\n\"))))(Secondary((id \
         ed0c5ce1-4e3c-480f-bea5-dac3e74b76a9)(content(Comment\"# Tip: Turn on \
         auto-probe (microscope toggle)    #\"))))(Secondary((id \
         465df82c-beb9-4424-8959-b39a560080ae)(content(Whitespace\"\\n\"))))(Secondary((id \
         539fefae-3960-46eb-8998-794f7b162a62)(content(Comment\"# to see \
         intermediate values as you type.        #\"))))(Secondary((id \
         19347aa2-8bd5-4bf7-ae11-a46575b0168b)(content(Whitespace\"\\n\"))))(Secondary((id \
         bdc57e3c-895e-4507-9ace-4edabb2029a6)(content(Whitespace\"\\n\"))))(Tile((id \
         1d652ff7-3e4b-4739-89da-12aa7ce28ee4)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         5dc3e274-a811-4a75-990b-2fecaccb9605)(content(Whitespace\" \
         \"))))(Tile((id \
         25e165b0-56f7-4235-87b7-d737529a5ee3)(label(grove_name))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         652e7c33-902b-449a-b1ad-2b5e32645806)(content(Whitespace\" \
         \")))))((Secondary((id \
         f3a60fd1-714e-42c7-a2ce-a1af4739ffca)(content(Whitespace\" \
         \"))))(Tile((id f6f163e0-f3a5-4b53-ae43-a6f4c131817b)(label(fun \
         ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 37))(sort Exp))))))(shards(0 \
         1))(children(((Secondary((id \
         3cbed15e-b66a-4bee-b83e-21da39d51bd7)(content(Whitespace\" \
         \"))))(Tile((id \
         797421fc-84c7-40f0-8f70-00b0ba87beb4)(label(path))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         b4d22976-0b3e-4980-a83a-c7a3afc28c30)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         809702af-e6bf-4e9c-97cb-e9b0b24e0bcd)(content(Whitespace\"\\n\"))))(Tile((id \
         930e12dc-df88-4d62-85b3-dec1f85e7b35)(label(?))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         c914e6aa-61b4-409d-ada4-7d0d69168eb2)(content(Whitespace\"\\n\"))))(Secondary((id \
         c9352867-997b-41b7-845d-a1f4280a0541)(content(Whitespace\"\\n\"))))(Secondary((id \
         204d43ce-9836-4c77-a6da-442b4edf7a0f)(content(Whitespace\"\\n\"))))(Secondary((id \
         1dd2a0ef-664b-4cdd-894b-da8d5d2be36f)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         5ed5cb39-3ad6-4a80-ad94-897416a72918)(content(Whitespace\"\\n\"))))(Secondary((id \
         d7297296-a9ae-4a14-bf13-41d0d1fc65c2)(content(Whitespace\"\\n\"))))(Tile((id \
         d23f6b9b-5490-49f0-9948-9abefe998b72)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         b1007221-19e9-47ce-8d30-845143591689)(content(Whitespace\"\\n\"))))(Tile((id \
         76baad12-1bde-4338-893f-921588d12ae0)(label(grove_name))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         2f0d4e6b-6068-43ed-87bd-e8b2a2ef3fb6)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         50aef597-6a31-4800-bd46-2186ca20760b)(label(\"\\\"/moonlit-grove/ferns/watering\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         40728e20-316c-47fe-a972-52b836d911dd)(content(Whitespace\"\\n\"))))(Tile((id \
         15a33056-2cc3-4852-8617-1abdae260a5a)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         c3bb62d7-fa04-4015-9eeb-3a9a4a0ddbd1)(content(Whitespace\" \
         \"))))(Tile((id \
         74472b0e-445d-47b0-9ce5-d792dbaea938)(label(\"\\\"moonlit-grove\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         b2a7c240-9ff5-4685-a4fc-f5307b6ada08)(content(Whitespace\"\\n\")))))))))(Tile((id \
         107ab769-1336-4343-9549-a47410c77097)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         38ddde92-93b2-4d38-857a-081ea45e110f)(content(Whitespace\"\\n\"))))(Secondary((id \
         b6895815-22e4-42f9-9565-3ab1b05ed2c7)(content(Whitespace\"\\n\"))))(Tile((id \
         2fe830f6-fdb1-4ea7-88ed-65bd590353dc)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         ff05b48f-eb42-4b59-bc46-66b2ca13fcaf)(content(Whitespace\"\\n\"))))(Tile((id \
         174cc75a-9b6f-4c7a-83c4-5a4ec06e014e)(label(grove_name))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         7e97f7d2-1b1f-4737-8359-defdf743f0fe)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         e469c476-8862-4123-a336-9b92a2b987dc)(label(\"\\\"/night-garden/herbs/rosemary\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         15844063-118f-4d6b-965b-d2365b27d782)(content(Whitespace\"\\n\"))))(Tile((id \
         15262de4-c5e5-49a5-aff6-a2e1a782d780)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         847a7077-8bad-4073-a878-db627ba2524e)(content(Whitespace\" \
         \"))))(Tile((id \
         0a4a9021-0bc8-4d5a-81d1-9fce5cf6ede4)(label(\"\\\"night-garden\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         b1abd400-44b5-4bc9-a6dd-7536276cacdf)(content(Whitespace\"\\n\")))))))))(Tile((id \
         2f207f73-d9ef-45c2-b63d-c5e87a7fa943)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         e5317270-924c-4d2b-b145-0ac27b6502f0)(content(Whitespace\"\\n\"))))(Secondary((id \
         5243e49c-43aa-403a-9b07-65d5521e44bc)(content(Whitespace\"\\n\"))))(Tile((id \
         8add0799-2266-4cf1-a28f-72830a30b612)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         ba1fd7df-5868-49e0-9b93-2bef2a332cb8)(content(Whitespace\"\\n\"))))(Tile((id \
         dd6bea2e-b4cf-4613-a226-2d85b4b3deee)(label(grove_name))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         a3229953-0c18-444b-ba32-3d41ce7df6e1)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         6f3e7d0d-d6b3-49df-8785-1470d22a8d64)(label(\"\\\"/\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         823a8ad4-8f2e-4be0-8b90-d68313a7db56)(content(Whitespace\"\\n\"))))(Tile((id \
         2695368c-5bf8-4838-b2ae-b3a7787a4675)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         c612c705-af34-4411-8079-8209492cc79b)(content(Whitespace\" \
         \"))))(Tile((id \
         0fbfa9ef-25f1-49e1-9092-8f73b07fb1b6)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         00c98ce5-9ad6-48e4-b9b6-2449413ade95)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         5a929577-2ab8-4bec-9bcd-c71f5625ec6f)(content(Whitespace\"\\n\")))))";
      backup_text =
        "# GARDEN PATH TASK                               #\n\
         #                                                #\n\
         # Implement grove_name: extract the first        #\n\
         # section from a garden path.                    #\n\
         #                                                #\n\
         # Examples:                                      #\n\
         #   grove_name(\"/moonlit-grove/ferns\")           #\n\
         #     == \"moonlit-grove\"                         #\n\
         #   grove_name(\"/night-garden/herbs/thyme\")      #\n\
         #     == \"night-garden\"                          #\n\
         #   grove_name(\"/\") == \"\"                        #\n\
         #                                                #\n\
         # Available functions:                           #\n\
         #   string_split(sep, str) -> [String]           #\n\
         #   string_concat(s1, s2) -> String              #\n\
         #   string_length(s) -> Int                      #\n\
         #   string_sub(str, pos, len) -> String          #\n\
         #   nth(list, index) -> element                  #\n\
         #   length(list) -> Int                          #\n\
         #   map(list, fn) -> list                        #\n\
         #   filter(list, pred) -> list                   #\n\
         #                                                #\n\
         # Function syntax: fun param -> body             #\n\
         # Let binding: let name = value in ...           #\n\
         #                                                #\n\
         # Tip: Turn on auto-probe (microscope toggle)    #\n\
         # to see intermediate values as you type.        #\n\n\
         let grove_name = fun path ->\n\
         ?\n\n\n\n\
         in\n\n\
         test\n\
         grove_name(\"/moonlit-grove/ferns/watering\")\n\
         == \"moonlit-grove\"\n\
         end;\n\n\
         test\n\
         grove_name(\"/night-garden/herbs/rosemary\")\n\
         == \"night-garden\"\n\
         end;\n\n\
         test\n\
         grove_name(\"/\")\n\
         == \"\"\n\
         end\n";
      refractors = "()";
    } )
