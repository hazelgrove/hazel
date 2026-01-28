let out : string * Haz3lcore.PersistentSegment.t =
  ( "Examples / study-write / basepoint / basepoint-sketch",
    {
      segment =
        "((Secondary((id \
         790c165a-17a9-4bef-b7d0-0dd8c9516ce5)(content(Comment\"# BASE ROUTE \
         TASK                              #\"))))(Secondary((id \
         fdd6c309-1e50-4a92-b285-73c7fd476df6)(content(Whitespace\"\\n\"))))(Secondary((id \
         d10c5028-b3f0-411c-ae15-2abf858be5ba)(content(Comment\"#                                              \
         #\"))))(Secondary((id \
         91f18100-fb93-4e3a-bc96-39e5f815e96b)(content(Whitespace\"\\n\"))))(Secondary((id \
         87a27bb6-207a-4b0a-886b-8ffdc1f05e54)(content(Comment\"# Implement \
         base_route: extract the first      #\"))))(Secondary((id \
         79852fa7-3182-4516-9465-ff2d597b9ca3)(content(Whitespace\"\\n\"))))(Secondary((id \
         ef0f0fa1-23e8-40b2-b0b2-b8c264757964)(content(Comment\"# path segment \
         from a URL path.                #\"))))(Secondary((id \
         b37ae445-6ff2-41c6-bfd2-8fa856183a5e)(content(Whitespace\"\\n\"))))(Secondary((id \
         ae6c62a8-5bca-4077-a56f-688af35faaee)(content(Comment\"#                                              \
         #\"))))(Secondary((id \
         f6907781-2ef8-445c-87ba-2d04730d7111)(content(Whitespace\"\\n\"))))(Secondary((id \
         33472fc9-49d2-4917-bf55-52643ac2e08a)(content(Comment\"# \
         Examples:                                    #\"))))(Secondary((id \
         28d7b5f4-9427-4e4e-98a0-58c0213782e3)(content(Whitespace\"\\n\"))))(Secondary((id \
         62fd42a6-9dfa-433c-9dfb-3844b832deb6)(content(Comment\"#   \
         base_route(\\\"/api/v1\\\") == \\\"api\\\"             \
         #\"))))(Secondary((id \
         6b699ab9-3e90-46bf-9803-c49344557f5f)(content(Whitespace\"\\n\"))))(Secondary((id \
         209eb99e-3abc-43c2-90e6-f204b28e0679)(content(Comment\"#   \
         base_route(\\\"/api/actions/rm\\\") == \\\"api\\\"     \
         #\"))))(Secondary((id \
         35c0cad6-ab9e-4e91-b6d1-a785e39bef22)(content(Whitespace\"\\n\"))))(Secondary((id \
         fe149715-e846-4610-949d-def083355496)(content(Comment\"#   \
         base_route(\\\"/\\\") == \\\"\\\"                      \
         #\"))))(Secondary((id \
         ba8a83b0-8bfb-462b-beaa-2986ae8862a2)(content(Whitespace\"\\n\"))))(Secondary((id \
         bf0c1d8d-7361-44b7-afa3-4c622fc07999)(content(Comment\"#                                              \
         #\"))))(Secondary((id \
         fe6e6f59-3801-4575-a121-31baaf7d4fdc)(content(Whitespace\"\\n\"))))(Secondary((id \
         d58b2b3f-56ef-4e98-a696-2b9c189bdb15)(content(Comment\"# Available \
         functions:                         #\"))))(Secondary((id \
         86d29071-dab8-43de-bb8a-0c58cbf4cb6d)(content(Whitespace\"\\n\"))))(Secondary((id \
         0b4f4b6e-bc43-4ad5-b881-e396f3567185)(content(Comment\"#   \
         string_split(sep, str) -> [String]         #\"))))(Secondary((id \
         3e5ef393-345a-4d3e-8420-85f21789db5a)(content(Whitespace\"\\n\"))))(Secondary((id \
         47b09f55-cfa7-4c2c-b999-1ccbe6521e55)(content(Comment\"#   \
         string_concat(s1, s2) -> String            #\"))))(Secondary((id \
         f3693af5-d2a0-43ef-b5a2-d706c8a80f3e)(content(Whitespace\"\\n\"))))(Secondary((id \
         ff68e6e3-5ad2-407f-a777-70eab6899edc)(content(Comment\"#   \
         string_length(s) -> Int                    #\"))))(Secondary((id \
         b78caaf6-fe9d-4bc7-a426-e84e68a88f17)(content(Whitespace\"\\n\"))))(Secondary((id \
         f2dd201c-e101-4223-a974-b471ce2f0a32)(content(Comment\"#   \
         string_sub(str, pos, len) -> String        #\"))))(Secondary((id \
         c9a63d99-1b2e-44b2-9d50-6bef4b18902f)(content(Whitespace\"\\n\"))))(Secondary((id \
         f69e4830-2eee-4a00-8258-245bc3b4d333)(content(Comment\"#   nth(list, \
         index) -> element                #\"))))(Secondary((id \
         a5e2eff8-b878-4faf-8061-95db11673382)(content(Whitespace\"\\n\"))))(Secondary((id \
         eb5c3e90-acbf-4853-a2ac-c82bea20fb67)(content(Comment\"#   \
         length(list) -> Int                        #\"))))(Secondary((id \
         d2e67f80-5057-428f-afbd-d6d9f361f3dd)(content(Whitespace\"\\n\"))))(Secondary((id \
         9de27922-b67a-4858-bbf5-829bdc04f499)(content(Comment\"#   map(list, \
         fn) -> list                      #\"))))(Secondary((id \
         f3fc580b-bd20-4c3a-97b6-026513d7016e)(content(Whitespace\"\\n\"))))(Secondary((id \
         d0f608b2-e790-46a0-a339-2f3e32b5aa9c)(content(Comment\"#   \
         filter(list, pred) -> list                 #\"))))(Secondary((id \
         a0487ec9-4adf-4880-a58f-a6d116a7af65)(content(Whitespace\"\\n\"))))(Secondary((id \
         59a51822-63ca-4e4e-9927-8f496d7110e2)(content(Comment\"#                                              \
         #\"))))(Secondary((id \
         9569f5dc-8cba-4459-889b-d8df437a7f9f)(content(Whitespace\"\\n\"))))(Secondary((id \
         d9062d33-5134-4226-be65-0eaec5e58dd9)(content(Comment\"# Function \
         syntax: fun param -> body           #\"))))(Secondary((id \
         a7513f70-9c3a-473d-8fee-14fadc453291)(content(Whitespace\"\\n\"))))(Secondary((id \
         85cd2c4d-863f-4380-a428-d399eec57fba)(content(Comment\"# Let binding: \
         let name = value in ...         #\"))))(Secondary((id \
         3c9d9e5f-7c44-467b-9ad5-0160f9e2f28a)(content(Whitespace\"\\n\"))))(Secondary((id \
         e9e24877-ffca-4cc9-9672-c71946e49306)(content(Comment\"#                                              \
         #\"))))(Secondary((id \
         7b9fbb34-4911-468e-b003-451d505cb494)(content(Whitespace\"\\n\"))))(Secondary((id \
         b32628e9-ec3f-4b6c-bff4-8ae30a4d7c74)(content(Comment\"# Tip: Turn on \
         auto-probe (microscope toggle)  #\"))))(Secondary((id \
         42ad6318-63fa-4a2f-8822-2d219595a6b9)(content(Whitespace\"\\n\"))))(Secondary((id \
         075d9b97-63af-4cf6-87c0-c5218e289e60)(content(Comment\"# to see \
         intermediate values as you type.      #\"))))(Secondary((id \
         c5bd71a4-4dff-40e1-95b8-9f07d74e0fe9)(content(Whitespace\"\\n\"))))(Secondary((id \
         cbf78619-793f-4a14-8451-650b6f704049)(content(Whitespace\"\\n\"))))(Tile((id \
         fbe8992d-627a-4897-b9f6-1474d405c36d)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         40))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         510a2532-2a2c-46e6-86c1-7ecbbbc61cb8)(content(Whitespace\" \
         \"))))(Tile((id \
         99fa548b-0064-4307-937d-d506fe6a5d39)(label(base_route))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         d9dd144e-d653-4003-be64-4b0562fe8aaa)(content(Whitespace\" \
         \")))))((Secondary((id \
         1556d4ab-8ffd-466b-aac0-74be76695472)(content(Whitespace\" \
         \"))))(Tile((id 0344eacb-5edc-41e9-8ed2-ea6d8780bf6e)(label(fun \
         ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 36))(sort Exp))))))(shards(0 \
         1))(children(((Secondary((id \
         4948c78b-f921-41ac-8749-d4b8f17379b2)(content(Whitespace\" \
         \"))))(Tile((id \
         add23c3a-a4d8-404d-b7e7-962474135ea9)(label(path))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         27de5a57-2af2-4ede-903b-3ee6a2e3901f)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         f2b50048-3008-4397-9204-cb77eaa01ffd)(content(Whitespace\"\\n\"))))(Tile((id \
         f723e949-f3f3-452f-bf4f-a96e97b62cd5)(label(?))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         2451879c-1e61-4abe-8f6a-d7a84c676a5e)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         8e2733be-adbd-4d5f-8f2d-94bc5e756653)(content(Whitespace\"\\n\"))))(Secondary((id \
         f29c4504-26a2-408d-ab23-b87685e85e3b)(content(Whitespace\"\\n\"))))(Tile((id \
         a532000f-1ec6-4e9d-860a-7908dd12bfe4)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         3cfa2ea0-607f-4e13-8374-fac498dd7d88)(content(Whitespace\"\\n\"))))(Tile((id \
         b26c83be-b23a-492d-8084-080bac6b27c1)(label(base_route))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         0e989ff6-a3d4-4e0f-b5de-bd8fe17969de)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         e528c9f2-abc9-43e4-ae7b-197e24bda931)(label(\"\\\"/api/v1\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         9a6b5680-dec2-41c5-a2b0-3b213b8713ba)(content(Whitespace\"\\n\"))))(Tile((id \
         deef83fc-9909-4354-8baa-703a33491597)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         9ff7059f-127a-45ca-96ea-e66ed6f842e6)(content(Whitespace\" \
         \"))))(Tile((id \
         0fa95309-fc41-4421-814f-b7e743d15279)(label(\"\\\"api\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         d8c25f68-5e39-4f07-828e-0e056ead0149)(content(Whitespace\"\\n\")))))))))(Tile((id \
         1d3132bd-5d41-491f-96d8-2da3842df59a)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 38))(sort Exp))((shape(Concave \
         38))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         6aa9e8c8-aa38-4937-b7f1-0f16c027f6c7)(content(Whitespace\"\\n\"))))(Secondary((id \
         cbef6b12-669a-41c2-ad1d-8bbaaef6d002)(content(Whitespace\"\\n\"))))(Tile((id \
         48c90887-878c-4979-93f3-587cfbc40494)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         37a5f19e-310a-495b-9c3a-a17766b54367)(content(Whitespace\"\\n\"))))(Tile((id \
         5be4660e-6fe5-4fad-8a98-0e376e94d82a)(label(base_route))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         131bc128-a305-4162-8889-df7df5c377b5)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         5cfed3d5-ea0c-4183-9e98-e2c5dc9b04c2)(label(\"\\\"/api/actions/rm\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         ea45450e-9b92-418e-b4bc-a0a6edbfbcee)(content(Whitespace\"\\n\"))))(Tile((id \
         d400078d-774d-4d25-9393-80cf7ff03c91)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         55b8ec9e-4389-40bc-809a-e54c270be0cc)(content(Whitespace\" \
         \"))))(Tile((id \
         754d8f4c-b981-437f-b2aa-9c3d96a3bc21)(label(\"\\\"api\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         39072c3d-7350-448d-b500-f5c0c99eb114)(content(Whitespace\"\\n\")))))))))(Tile((id \
         9240746e-ea92-4c44-9329-25a3c2a5015f)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 38))(sort Exp))((shape(Concave \
         38))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         fa6e0dac-ee42-40cb-b40e-f441871d2c7d)(content(Whitespace\"\\n\"))))(Secondary((id \
         0d8daca6-7e70-4bec-af56-beb13cc24315)(content(Whitespace\"\\n\"))))(Tile((id \
         c533b377-36a7-4cd2-896c-633c3f9a0cd6)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         9ecf767c-b971-44da-b3d5-69192b685fe7)(content(Whitespace\"\\n\"))))(Tile((id \
         a764c892-0159-4e52-98dc-b8e30e99a081)(label(base_route))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         aade9abb-2a41-4c37-8072-e963107ad73f)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         e2c815fe-2bfc-44d9-bd73-9856137e4381)(label(\"\\\"/\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         7540489e-6c7e-4bfb-bc20-1e0c8991c767)(content(Whitespace\"\\n\"))))(Tile((id \
         3d965f68-6ee3-4e57-a914-621e2aa6c4f0)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         c89e8486-c05d-4f58-914c-690e6e0a7a07)(content(Whitespace\" \
         \"))))(Tile((id \
         cf126e08-0a67-40c1-a535-e53ad7e6a339)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         29c6e9a5-1ef6-4157-9758-177f7f3d9fe9)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         504b5861-877f-4996-9824-984a3fc9467f)(content(Whitespace\"\\n\")))))";
      backup_text =
        "# BASE ROUTE TASK                              #\n\
         #                                              #\n\
         # Implement base_route: extract the first      #\n\
         # path segment from a URL path.                #\n\
         #                                              #\n\
         # Examples:                                    #\n\
         #   base_route(\"/api/v1\") == \"api\"             #\n\
         #   base_route(\"/api/actions/rm\") == \"api\"     #\n\
         #   base_route(\"/\") == \"\"                      #\n\
         #                                              #\n\
         # Available functions:                         #\n\
         #   string_split(sep, str) -> [String]         #\n\
         #   string_concat(s1, s2) -> String            #\n\
         #   string_length(s) -> Int                    #\n\
         #   string_sub(str, pos, len) -> String        #\n\
         #   nth(list, index) -> element                #\n\
         #   length(list) -> Int                        #\n\
         #   map(list, fn) -> list                      #\n\
         #   filter(list, pred) -> list                 #\n\
         #                                              #\n\
         # Function syntax: fun param -> body           #\n\
         # Let binding: let name = value in ...         #\n\
         #                                              #\n\
         # Tip: Turn on auto-probe (microscope toggle)  #\n\
         # to see intermediate values as you type.      #\n\n\
         let base_route = fun path ->\n\
         ?\n\
         in\n\n\
         test\n\
         base_route(\"/api/v1\")\n\
         == \"api\"\n\
         end;\n\n\
         test\n\
         base_route(\"/api/actions/rm\")\n\
         == \"api\"\n\
         end;\n\n\
         test\n\
         base_route(\"/\")\n\
         == \"\"\n\
         end\n";
      refractors = "()";
    } )
