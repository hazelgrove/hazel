let out : string * Haz3lcore.PersistentSegment.t =
  ( "Study / writing / basepoint / basepoint-sketch",
    {
      segment =
        "((Secondary((id \
         4c9bd350-ea0e-4010-87e0-279e047dae12)(content(Comment\"# BASE ROUTE \
         TASK                              #\"))))(Secondary((id \
         5ceaa65b-21a0-4d25-987f-4310f6637827)(content(Whitespace\"\\n\"))))(Secondary((id \
         82093665-d094-484a-b05c-af1e17c508af)(content(Comment\"#                                              \
         #\"))))(Secondary((id \
         83ed02a5-1923-4b31-9af5-3a03bf9b1fb6)(content(Whitespace\"\\n\"))))(Secondary((id \
         e0a8b15e-b0b1-407d-97a4-1332f237e986)(content(Comment\"# Implement \
         base_route: extract the first      #\"))))(Secondary((id \
         04d3e9f2-b8db-4ead-97b0-b404f113efd1)(content(Whitespace\"\\n\"))))(Secondary((id \
         e9d96793-ad81-4c43-bb1c-e7e1eccd3633)(content(Comment\"# path segment \
         from a URL path.                #\"))))(Secondary((id \
         102fb509-8aea-46d0-a8ee-421be6765231)(content(Whitespace\"\\n\"))))(Secondary((id \
         345ab504-d70b-4f0e-ba06-1c8cd7d4bc46)(content(Comment\"#                                              \
         #\"))))(Secondary((id \
         69d94052-4b69-4812-9382-b98942cf0f02)(content(Whitespace\"\\n\"))))(Secondary((id \
         0da530e5-ef4e-4aa8-b054-0f30feb9125b)(content(Comment\"# \
         Examples:                                    #\"))))(Secondary((id \
         ad8a6efe-cb0a-44de-af1c-091fd86e05b8)(content(Whitespace\"\\n\"))))(Secondary((id \
         4ce2f2eb-91c0-4a3c-b315-2bfc391fd67d)(content(Comment\"#   \
         base_route(\\\"/api/v1\\\") == \\\"api\\\"             \
         #\"))))(Secondary((id \
         c5cf95ea-e904-4edd-9370-9570872ed15f)(content(Whitespace\"\\n\"))))(Secondary((id \
         467eaa3d-1469-457a-b04e-67df374ee5f9)(content(Comment\"#   \
         base_route(\\\"/api/actions/rm\\\") == \\\"api\\\"     \
         #\"))))(Secondary((id \
         5ccf5bb1-df8a-44c4-8040-a9556bf98d5c)(content(Whitespace\"\\n\"))))(Secondary((id \
         6781c39c-2063-49ab-8b22-f8b00c11de76)(content(Comment\"#   \
         base_route(\\\"/\\\") == \\\"\\\"                      \
         #\"))))(Secondary((id \
         e072cdd6-a258-4e92-9422-defdd83d56bd)(content(Whitespace\"\\n\"))))(Secondary((id \
         69f4b9d3-65ba-4f7b-af59-7c8c309f83d2)(content(Comment\"#                                              \
         #\"))))(Secondary((id \
         07a11400-6b7e-4713-bbf9-fc306ff811d4)(content(Whitespace\"\\n\"))))(Secondary((id \
         e3f68d58-e8b3-4257-9d57-b6ec5fc50842)(content(Comment\"# Available \
         functions:                         #\"))))(Secondary((id \
         f23e1fc9-d1e7-4968-bd23-50ad76d4fe3e)(content(Whitespace\"\\n\"))))(Secondary((id \
         f785f03d-f7cb-4425-9023-8f14d39a2608)(content(Comment\"#   \
         string_split(sep, str) -> [String]         #\"))))(Secondary((id \
         d629c54f-f150-4d51-8a63-400e16bf1b21)(content(Whitespace\"\\n\"))))(Secondary((id \
         d8828e9c-5ed0-49b4-a683-735bd296c1a1)(content(Comment\"#   \
         string_concat(s1, s2) -> String            #\"))))(Secondary((id \
         d686c074-50c6-4073-a20d-f047eb51e473)(content(Whitespace\"\\n\"))))(Secondary((id \
         0c5b0721-7344-46f8-a213-846d76773be0)(content(Comment\"#   \
         string_length(s) -> Int                    #\"))))(Secondary((id \
         f409394a-cab7-4f3c-a810-0f1a279e7676)(content(Whitespace\"\\n\"))))(Secondary((id \
         06e50512-8b06-4282-96ed-5c55d9486cd3)(content(Comment\"#   \
         string_sub(str, pos, len) -> String        #\"))))(Secondary((id \
         ce79942e-c41e-4d33-a7ea-c12cb08b32e2)(content(Whitespace\"\\n\"))))(Secondary((id \
         10b1101a-ef60-4ab1-8a57-5e3c04ea0faf)(content(Comment\"#   nth(list, \
         index) -> element                #\"))))(Secondary((id \
         328ea85a-ee93-4665-a1e7-7a810e707d3c)(content(Whitespace\"\\n\"))))(Secondary((id \
         a653ff22-c083-47e8-9c88-3e124d3870af)(content(Comment\"#   \
         length(list) -> Int                        #\"))))(Secondary((id \
         944d456d-e208-4a32-b4d8-f6dc5b65b95e)(content(Whitespace\"\\n\"))))(Secondary((id \
         8e667fb5-7c27-4e70-b5be-cbbd9488a5c9)(content(Comment\"#   map(list, \
         fn) -> list                      #\"))))(Secondary((id \
         110d3b75-d7dc-4b9c-826c-a62bea842f8c)(content(Whitespace\"\\n\"))))(Secondary((id \
         2931d239-403a-44c1-ba9d-8f53512542c4)(content(Comment\"#   \
         filter(list, pred) -> list                 #\"))))(Secondary((id \
         00aca0ba-a8ad-419e-8e67-65ac8243695b)(content(Whitespace\"\\n\"))))(Secondary((id \
         89b6c476-9d61-4c02-8c89-122517b4738e)(content(Comment\"#                                              \
         #\"))))(Secondary((id \
         5370307e-81f6-44ef-9190-4a64abb4f7b7)(content(Whitespace\"\\n\"))))(Secondary((id \
         1fd33844-14e9-42fa-a746-54a7ee563d94)(content(Comment\"# Function \
         syntax: fun param -> body           #\"))))(Secondary((id \
         7b2ae537-5ae1-49c5-a76b-cea304999c40)(content(Whitespace\"\\n\"))))(Secondary((id \
         6d6b94a8-ee09-45f6-ad9f-77e0349f1abb)(content(Comment\"# Let binding: \
         let name = value in ...         #\"))))(Secondary((id \
         014afb23-1be0-4d3c-9a9d-70219172035c)(content(Whitespace\"\\n\"))))(Secondary((id \
         57c4e867-1710-46f0-abe3-0f14d63b8633)(content(Comment\"#                                              \
         #\"))))(Secondary((id \
         f2deeae9-5fb1-4848-a858-2a3ba8e49af5)(content(Whitespace\"\\n\"))))(Secondary((id \
         4e0afbd2-1fcf-4e54-a216-cd73ea351748)(content(Comment\"# Tip: Turn on \
         auto-probe (microscope toggle)  #\"))))(Secondary((id \
         e4abaea7-7713-4717-a3b1-5e20bb3a2b92)(content(Whitespace\"\\n\"))))(Secondary((id \
         de190295-8d86-45e2-b01f-356b20d6f406)(content(Comment\"# to see \
         intermediate values as you type.      #\"))))(Secondary((id \
         f8b9cb04-0dca-4677-ab9e-0e20383c453a)(content(Whitespace\"\\n\"))))(Secondary((id \
         1b48e8f2-5445-4471-97d6-9b4778bcc428)(content(Whitespace\"\\n\"))))(Tile((id \
         d780d627-3f66-41fa-8070-699b264d6120)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         ea1f9955-11d3-4e1b-8537-d98d5ab7f2bd)(content(Whitespace\" \
         \"))))(Tile((id \
         8f5fc281-81da-4aea-9b6e-4688496b8dd1)(label(base_route))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         0e05b94a-96d2-4024-a9d1-01b1f800690e)(content(Whitespace\" \
         \")))))((Secondary((id \
         1dbc3f75-49db-4e75-87a1-4e472b5840f5)(content(Whitespace\" \
         \"))))(Tile((id f49a79d5-e3d8-4b8a-b86c-8f31a69c3633)(label(fun \
         ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 37))(sort Exp))))))(shards(0 \
         1))(children(((Secondary((id \
         70a60712-d933-4645-bb30-451d4a41cff7)(content(Whitespace\" \
         \"))))(Tile((id \
         875cd8f7-47cf-4542-a148-525b87883381)(label(path))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         1d2708a5-42ca-48e5-afa8-65a42159e7bf)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         7a202213-815a-4c0a-812d-65b4b68e63a6)(content(Whitespace\"\\n\"))))(Tile((id \
         5b924df4-1756-4609-b60e-1c145690f3b2)(label(?))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         528d62a8-8de9-4d10-87dc-25df838623a6)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         5a8501fc-20d0-40cc-9077-2f9334cef321)(content(Whitespace\"\\n\"))))(Secondary((id \
         306829dc-592d-47e9-87c8-ecb6dfc66c52)(content(Whitespace\"\\n\"))))(Tile((id \
         6bd82506-67a3-4502-ae2a-fb90b91fd763)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         9b91b5b5-6812-434b-9f5b-6f3860a35e80)(content(Whitespace\"\\n\"))))(Tile((id \
         a5e4ee00-0307-4295-8069-e4b2a3967f36)(label(base_route))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         7a8244b4-6a55-4898-ba3c-8ab1ccb60dc8)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         ac781c95-1a72-4684-8595-dd1449fad424)(label(\"\\\"/api/v1\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         95469b3e-da0d-4d45-8598-276eaa0bfaa4)(content(Whitespace\"\\n\"))))(Tile((id \
         067a4aaf-2e1d-4834-9c1e-1b58dbf9d021)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         04ab5482-afab-4e29-a922-b55ae16c1b83)(content(Whitespace\" \
         \"))))(Tile((id \
         3612dd64-e43c-4280-a188-b6412c7f8035)(label(\"\\\"api\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         4d141776-b373-4565-bf42-298d7b40606e)(content(Whitespace\"\\n\")))))))))(Tile((id \
         815855a9-39c5-4f50-ab4e-8ea8b9e5dfd2)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         88b84f4f-d1f4-4c4c-b9ea-ef81c53dfb9d)(content(Whitespace\"\\n\"))))(Secondary((id \
         ea863536-95d2-44a0-ab64-aaa4f15482be)(content(Whitespace\"\\n\"))))(Tile((id \
         6b236deb-2c90-49be-9540-0311e7e2edee)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         6f8e7d1d-5a13-4cdb-936a-11a99235932c)(content(Whitespace\"\\n\"))))(Tile((id \
         2b23bdd6-53e5-4694-a6ce-24fabd294d53)(label(base_route))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         0750401c-a561-4ee7-a1c2-1f8fb7717a64)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         a7a68327-0928-4c22-ab7a-7be8b6188d9b)(label(\"\\\"/api/actions/rm\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         b29230b2-1ff4-4052-a267-6dd1aee1b925)(content(Whitespace\"\\n\"))))(Tile((id \
         56b30e21-41c7-40ca-82a4-64d01311de97)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         731ca0c3-7277-4519-a02e-3ae8bfaffa6b)(content(Whitespace\" \
         \"))))(Tile((id \
         3184cd38-34ca-475c-8b5e-5f0e108eaaa4)(label(\"\\\"api\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         7c113d0f-ec41-461d-92a8-c4628c8f189a)(content(Whitespace\"\\n\")))))))))(Tile((id \
         ba79841d-3996-49a5-a352-2d223998d745)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         71710c12-6cff-4dd1-8c2c-a4b2c0cb29aa)(content(Whitespace\"\\n\"))))(Secondary((id \
         868dd60f-facc-4b38-b768-58a3cd1614b1)(content(Whitespace\"\\n\"))))(Tile((id \
         4f9bfe7b-bb1c-4d42-b327-539c22b92d7d)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         5328990b-1805-4c62-883d-2842af16772d)(content(Whitespace\"\\n\"))))(Tile((id \
         9cc928de-051c-4cb8-9cf0-73ac95e97725)(label(base_route))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         31bc2749-d1bc-4dab-b7d3-d629e6584cdb)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         495c6d75-5e0e-4c5a-8018-e029d66e8abe)(label(\"\\\"/\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         c100ccd3-bd8f-44f5-b43f-327563396cdc)(content(Whitespace\"\\n\"))))(Tile((id \
         456d37d5-4138-451b-8c34-16695a212838)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         c400cdf6-8e24-48a8-be54-4f6367d2e964)(content(Whitespace\" \
         \"))))(Tile((id \
         b5ce268d-fd22-4578-bba8-343eed1087f9)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         a29d96c3-8242-48a2-80ac-2a106e8b1650)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         eaa6842f-f092-4266-82d7-2497d06305c5)(content(Whitespace\"\\n\")))))";
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
