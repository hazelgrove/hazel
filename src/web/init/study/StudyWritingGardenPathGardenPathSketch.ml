let out : string * Haz3lcore.PersistentSegment.t =
  ( "Study / writing / garden-path / garden-path-sketch",
    {
      segment =
        "((Secondary((id \
         06c104d1-0d7f-4725-a41d-0ffc1ad673d4)(content(Comment\"# GARDEN PATH \
         TASK                               #\"))))(Secondary((id \
         30b7cfa0-7e15-408e-a483-7511a97714cc)(content(Whitespace\"\\n\"))))(Secondary((id \
         e6f923d0-862d-4ca6-8ab2-ad3a71b5f6fe)(content(Comment\"#                                                \
         #\"))))(Secondary((id \
         bbb7c227-b88a-4729-ab2b-f4d12980a50a)(content(Whitespace\"\\n\"))))(Secondary((id \
         12c4b023-c512-4f59-a0f1-e804bcc61582)(content(Comment\"# Implement \
         grove_name: extract the first        #\"))))(Secondary((id \
         a839e801-6c77-4bbb-b469-974bc33f015f)(content(Whitespace\"\\n\"))))(Secondary((id \
         a38367fe-3bac-45bb-b921-510cf14dd614)(content(Comment\"# section from \
         a garden path.                    #\"))))(Secondary((id \
         50abf786-7f70-4421-9480-1beaaaee3191)(content(Whitespace\"\\n\"))))(Secondary((id \
         5afb3e26-f79d-4fd0-8eb5-8259f3ee95d3)(content(Comment\"#                                                \
         #\"))))(Secondary((id \
         29254e8c-a823-4c7c-b9fd-c65f3729aa22)(content(Whitespace\"\\n\"))))(Secondary((id \
         cf49aa36-20cd-44b3-9732-e8dd7c210a66)(content(Comment\"# \
         Examples:                                      #\"))))(Secondary((id \
         82b15c14-7d17-48f0-90f0-2accee356a97)(content(Whitespace\"\\n\"))))(Secondary((id \
         411f84a0-b84c-48a1-b2bb-7a47c4f0b1f5)(content(Comment\"#   \
         grove_name(\\\"/moonlit-grove/ferns\\\")           \
         #\"))))(Secondary((id \
         8d11da37-c531-4c00-b060-d2a7f838deb6)(content(Whitespace\"\\n\"))))(Secondary((id \
         b5c88ef7-a26f-46f5-b264-19f74ed10b06)(content(Comment\"#     == \
         \\\"moonlit-grove\\\"                         #\"))))(Secondary((id \
         6e45eaee-4bb1-4ade-98be-f01dddb4909b)(content(Whitespace\"\\n\"))))(Secondary((id \
         6aeb119a-e0c1-4043-8585-b89742d8b17f)(content(Comment\"#   \
         grove_name(\\\"/night-garden/herbs/thyme\\\")      \
         #\"))))(Secondary((id \
         ed30c98b-c148-40f0-98c0-771975c1e5b6)(content(Whitespace\"\\n\"))))(Secondary((id \
         8e83e40a-7e9f-4891-ba99-3f4e4589ff25)(content(Comment\"#     == \
         \\\"night-garden\\\"                          #\"))))(Secondary((id \
         7ea95957-9bf5-42a2-81da-725d13f1d32b)(content(Whitespace\"\\n\"))))(Secondary((id \
         6768f84b-ef69-4981-b358-65b3edb359aa)(content(Comment\"#   \
         grove_name(\\\"/\\\") == \\\"\\\"                        \
         #\"))))(Secondary((id \
         41125a86-f55a-4479-86a5-ea484c41b2e9)(content(Whitespace\"\\n\"))))(Secondary((id \
         5c449efb-c829-4d8e-9d89-890a1f8ffdb5)(content(Comment\"#                                                \
         #\"))))(Secondary((id \
         2673e29a-57f9-4487-9b24-8ca4f293b966)(content(Whitespace\"\\n\"))))(Secondary((id \
         36ed5b21-bfb1-4379-8c92-80ec875a33b4)(content(Comment\"# Available \
         functions:                           #\"))))(Secondary((id \
         b7034f92-fad3-446a-a09a-bdc24fe0dc18)(content(Whitespace\"\\n\"))))(Secondary((id \
         85ef5132-87c8-4c63-89a9-6179f1fd4390)(content(Comment\"#   \
         string_split(sep, str) -> [String]           #\"))))(Secondary((id \
         d283ce96-a4d9-4878-8bc1-bd877a533c8d)(content(Whitespace\"\\n\"))))(Secondary((id \
         7e8b43a2-b194-41fc-a101-cbc73613a60e)(content(Comment\"#   \
         string_concat(s1, s2) -> String              #\"))))(Secondary((id \
         dfd61c34-8165-40e8-a8dc-70993a9ef266)(content(Whitespace\"\\n\"))))(Secondary((id \
         3068d74b-408c-496b-9167-f4880f6ff75c)(content(Comment\"#   \
         string_length(s) -> Int                      #\"))))(Secondary((id \
         e36b4473-a26f-4cd6-8398-92f49ea4aacf)(content(Whitespace\"\\n\"))))(Secondary((id \
         d70fab6a-dd4d-45c4-8a9f-08e66464f9ca)(content(Comment\"#   \
         string_sub(str, pos, len) -> String          #\"))))(Secondary((id \
         6d29d746-1a38-4062-814d-094967259256)(content(Whitespace\"\\n\"))))(Secondary((id \
         a95f8d58-eaa2-41b2-a208-232d7ad46caf)(content(Comment\"#   nth(list, \
         index) -> element                  #\"))))(Secondary((id \
         16359d18-d7d6-463b-933a-2dac70057b69)(content(Whitespace\"\\n\"))))(Secondary((id \
         05b90670-3a3d-4176-b932-6e452ca09bde)(content(Comment\"#   \
         length(list) -> Int                          #\"))))(Secondary((id \
         05362e2b-12a8-446c-87c3-6027a9a7c0f1)(content(Whitespace\"\\n\"))))(Secondary((id \
         4657f376-d4a3-445c-a1d1-8b7b079bf309)(content(Comment\"#   map(list, \
         fn) -> list                        #\"))))(Secondary((id \
         3e45ac9f-9ec4-460b-b2f0-0f682ff71e9e)(content(Whitespace\"\\n\"))))(Secondary((id \
         af58f0a9-4d38-4344-a312-bdc0dbb2e9c8)(content(Comment\"#   \
         filter(list, pred) -> list                   #\"))))(Secondary((id \
         efd6755d-f705-436a-bae8-e6fe530f633d)(content(Whitespace\"\\n\"))))(Secondary((id \
         e0ffc748-180a-4015-a1b1-65260aa5e40e)(content(Comment\"#                                                \
         #\"))))(Secondary((id \
         0e91289d-d151-48b7-a128-f522660fc4a3)(content(Whitespace\"\\n\"))))(Secondary((id \
         c7b33fc8-5d76-45c5-990c-20449336dea4)(content(Comment\"# Function \
         syntax: fun param -> body             #\"))))(Secondary((id \
         413b7215-43a4-47bb-a84b-394af23470e0)(content(Whitespace\"\\n\"))))(Secondary((id \
         1c39c6f6-4d94-4bf6-b2f9-547c6b31bf0e)(content(Comment\"# Let binding: \
         let name = value in ...           #\"))))(Secondary((id \
         1c053300-9a7c-4027-b465-d54fef624206)(content(Whitespace\"\\n\"))))(Secondary((id \
         666cf85b-987c-46fd-a278-1b5d37c0d147)(content(Comment\"#                                                \
         #\"))))(Secondary((id \
         6a595f9b-c3f9-4bd2-b0ac-cbdfd279d16d)(content(Whitespace\"\\n\"))))(Secondary((id \
         f8e0c69f-5d04-4714-96db-1a744c75d0fc)(content(Comment\"# Tip: Turn on \
         auto-probe (microscope toggle)    #\"))))(Secondary((id \
         29b5861c-f0d8-47c0-969a-bc2e4cb3c04a)(content(Whitespace\"\\n\"))))(Secondary((id \
         cb00c09c-9479-48a8-97a8-300f1c7914f2)(content(Comment\"# to see \
         intermediate values as you type.        #\"))))(Secondary((id \
         57a019b7-ad3e-430a-bad4-070f144ff064)(content(Whitespace\"\\n\"))))(Secondary((id \
         6ce40c13-6fa0-4cc9-9964-82e451f0ca61)(content(Whitespace\"\\n\"))))(Tile((id \
         506c4061-2274-4ca3-b18d-6a71e40c4b3e)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         40b7824a-ed59-4aba-b97e-33c1c0b08492)(content(Whitespace\" \
         \"))))(Tile((id \
         3c289d27-d40c-4fda-9202-c7418d350d7a)(label(grove_name))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         380c9728-6048-4017-b6c5-f4fd9dfad6d0)(content(Whitespace\" \
         \")))))((Secondary((id \
         71c02a5c-789a-4bf3-a845-bc1a91cbd4bc)(content(Whitespace\" \
         \"))))(Tile((id 8aa38e73-6778-4bdb-85d1-43aef48bd3c3)(label(fun \
         ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 37))(sort Exp))))))(shards(0 \
         1))(children(((Secondary((id \
         f751f46b-90a5-41d3-ba3a-c0b7cd874e8e)(content(Whitespace\" \
         \"))))(Tile((id \
         687215cf-f195-46f2-a6cc-788a085062cc)(label(path))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         0b25cba8-9357-43fd-804b-485c9f92da59)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         065957b4-9e2a-47f7-ba31-9f21794eabe5)(content(Whitespace\"\\n\"))))(Tile((id \
         b5d18b83-c446-41a5-8ae5-23258681efaa)(label(?))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         6c1dfb3b-fe6e-456f-8939-5b60fa19500b)(content(Whitespace\"\\n\"))))(Secondary((id \
         0fd3e336-4b30-43d1-9d9a-f532cd1d7c73)(content(Whitespace\"\\n\"))))(Secondary((id \
         2a9fa1c7-fc39-4e69-ba75-841de92a2b5c)(content(Whitespace\"\\n\"))))(Secondary((id \
         e3721112-1ff4-4946-80cb-47f4b19ff96c)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         00e3f2e2-dd73-4ea4-8ee3-78c1ceb34fbc)(content(Whitespace\"\\n\"))))(Secondary((id \
         29c8d822-0cc9-4aa7-a268-646f1b5a0a63)(content(Whitespace\"\\n\"))))(Tile((id \
         1fce7fee-a4c0-48e2-b741-8faa41e47916)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         1fffa2b9-8851-4124-865c-c0f5968b89d6)(content(Whitespace\"\\n\"))))(Tile((id \
         b111763b-d292-428a-9847-a51c74824046)(label(grove_name))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         75e3814d-61e4-47ba-857c-f25824b6a219)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         b45749f6-402b-404c-881c-e5f42adda5f8)(label(\"\\\"/moonlit-grove/ferns/watering\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         542cd040-9b98-4c35-bc7b-e0f195994a31)(content(Whitespace\"\\n\"))))(Tile((id \
         8f8b56fc-9c6c-4caa-8d60-112dd37b60a3)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         9266a845-3c19-459b-ae4e-0090a5c5e204)(content(Whitespace\" \
         \"))))(Tile((id \
         c09767a9-9616-475b-8e43-695fd2e4d9ab)(label(\"\\\"moonlit-grove\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         6c24cb2a-5067-43c6-9aab-a7aab975df74)(content(Whitespace\"\\n\")))))))))(Tile((id \
         025102bc-5e3d-44c7-a7ec-57ae1be9f24a)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         87bc6841-c3a5-47eb-9c13-3299a6c901de)(content(Whitespace\"\\n\"))))(Secondary((id \
         fc83f031-df7d-42c0-8849-d4f57c1943ee)(content(Whitespace\"\\n\"))))(Tile((id \
         df0851ac-a693-455e-a015-ed12bbdc50ad)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         e1d5934a-7dcf-4f2f-af99-dedee7ed60ed)(content(Whitespace\"\\n\"))))(Tile((id \
         66e9e0f9-6981-48d5-90b2-f803842141fb)(label(grove_name))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         b0a3e401-a022-4545-99bd-81e3c8f777a1)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         2f419f60-3495-4a54-aeba-d4953c700bec)(label(\"\\\"/night-garden/herbs/rosemary\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         cf8b9e6f-b534-473b-b527-e83f262cdb89)(content(Whitespace\"\\n\"))))(Tile((id \
         01366b2d-046e-4d9f-aae9-159660710e05)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         1f4b323b-2b41-4b77-89d3-2b182dbf3c60)(content(Whitespace\" \
         \"))))(Tile((id \
         8cd4f842-ae82-4b29-a60b-b1c7ec43da36)(label(\"\\\"night-garden\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         2cf04197-878f-4f3c-9348-311dcaa94e0f)(content(Whitespace\"\\n\")))))))))(Tile((id \
         c0024cb0-9fd2-4da2-aea9-45d298dd1dc0)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         b77789cd-1cba-48ee-9e51-2c1c7e340128)(content(Whitespace\"\\n\"))))(Secondary((id \
         996eb6bd-7ed2-400d-b9db-281c11ffadff)(content(Whitespace\"\\n\"))))(Tile((id \
         b795124d-ce9e-478e-aff7-df8e6d4aec1d)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         d237dbb3-6ba8-451f-aad5-c09bd3b5d8fa)(content(Whitespace\"\\n\"))))(Tile((id \
         8177e56e-4172-4bb0-a67f-db1ab6167415)(label(grove_name))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         87d7cea6-9f95-437e-ae11-d575dd390722)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         8f3b1078-5c80-46ae-9004-20a04af22af4)(label(\"\\\"/\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         48a5ea21-af04-4008-9da6-13e7b8c9a0c0)(content(Whitespace\"\\n\"))))(Tile((id \
         7d6ae633-2691-4d74-91ea-d67d81c1e5a9)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         9717ae8c-5e1f-41d9-96b1-e021faeccace)(content(Whitespace\" \
         \"))))(Tile((id \
         86352546-228a-475c-a89d-9795776180fd)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         5a5b470e-e8da-4d15-943f-56d07e6551bf)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         0f5c00bf-8cc8-4d57-bf65-b53536b24c09)(content(Whitespace\"\\n\")))))";
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
