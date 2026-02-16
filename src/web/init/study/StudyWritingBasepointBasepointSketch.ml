let out : string * Haz3lcore.PersistentSegment.t =
  ( "Study / writing / basepoint / basepoint-sketch",
    {
      segment =
        "((Secondary((id \
         fe4f3410-c9aa-4af2-8f90-6dd8bc61fd05)(content(Comment\"# BASE ROUTE \
         TASK                              #\"))))(Secondary((id \
         13ee1cd4-bc7c-40bc-862f-9d3212e915ba)(content(Whitespace\"\\n\"))))(Secondary((id \
         2c2f5563-366d-4116-9ecf-19bc9cb4f6f6)(content(Comment\"#                                              \
         #\"))))(Secondary((id \
         db946665-9563-4f85-a68f-08ab17ec34db)(content(Whitespace\"\\n\"))))(Secondary((id \
         58244563-0dd4-43e4-8a58-debcc69c42e2)(content(Comment\"# Implement \
         base_route: extract the first      #\"))))(Secondary((id \
         d00ac6f9-c330-4e14-a249-6eeccae16938)(content(Whitespace\"\\n\"))))(Secondary((id \
         101b8e29-5258-4b1d-a2b9-7d0cf2f51d4e)(content(Comment\"# path segment \
         from a URL path.                #\"))))(Secondary((id \
         85ee77a5-210a-49c2-b96f-0d2e62cd5550)(content(Whitespace\"\\n\"))))(Secondary((id \
         27349091-9d0a-44a8-87fd-999fda384bbd)(content(Comment\"#                                              \
         #\"))))(Secondary((id \
         1cdb488f-86e9-45e9-9e58-55e6864ef455)(content(Whitespace\"\\n\"))))(Secondary((id \
         078dddf7-2499-47d2-9e68-733591dfe069)(content(Comment\"# \
         Examples:                                    #\"))))(Secondary((id \
         3deac025-71fa-4ef6-b23e-2a89c5ddbea9)(content(Whitespace\"\\n\"))))(Secondary((id \
         596907e2-6be9-4e82-89c9-bd54a58ea325)(content(Comment\"#   \
         base_route(\\\"/api/v1\\\") == \\\"api\\\"             \
         #\"))))(Secondary((id \
         a824163a-1ff9-4215-9ff6-36f1657603c5)(content(Whitespace\"\\n\"))))(Secondary((id \
         4f6196b3-2a9b-45db-a449-72fb12fc368b)(content(Comment\"#   \
         base_route(\\\"/api/actions/rm\\\") == \\\"api\\\"     \
         #\"))))(Secondary((id \
         e6f6dd5e-eb7f-4844-b4e0-05e1880332c0)(content(Whitespace\"\\n\"))))(Secondary((id \
         186b17b4-2122-418e-b82e-0cb6b6735524)(content(Comment\"#   \
         base_route(\\\"/\\\") == \\\"\\\"                      \
         #\"))))(Secondary((id \
         2ead2981-24f3-440d-8581-ba4f7f259e36)(content(Whitespace\"\\n\"))))(Secondary((id \
         ca9494ff-b6df-4704-a602-bdb59cce49be)(content(Comment\"#                                              \
         #\"))))(Secondary((id \
         0f400120-769c-44f9-b6e0-483d02c66335)(content(Whitespace\"\\n\"))))(Secondary((id \
         987be478-7eef-4b70-8222-2ade5276394b)(content(Comment\"# Available \
         functions:                         #\"))))(Secondary((id \
         f1baefbd-8143-47c2-96d1-bb80343d33a9)(content(Whitespace\"\\n\"))))(Secondary((id \
         1fdc69dc-4027-4598-acb7-2761b36efcae)(content(Comment\"#   \
         string_split(sep, str) -> [String]         #\"))))(Secondary((id \
         93bd33c6-f8fd-41e3-b3e8-a92403be1365)(content(Whitespace\"\\n\"))))(Secondary((id \
         7f2c7c4e-7fef-4c70-8be8-27a9df628c1b)(content(Comment\"#   \
         string_concat(s1, s2) -> String            #\"))))(Secondary((id \
         ebfc07b3-568e-47b5-b8f6-521b25814c92)(content(Whitespace\"\\n\"))))(Secondary((id \
         30257051-7ecb-4313-ad9f-835a4f006f9e)(content(Comment\"#   \
         string_length(s) -> Int                    #\"))))(Secondary((id \
         0b40a919-bc31-413e-984c-fe669179cdf0)(content(Whitespace\"\\n\"))))(Secondary((id \
         996f8a85-5f3b-48b2-9978-72224ccdd514)(content(Comment\"#   \
         string_sub(str, pos, len) -> String        #\"))))(Secondary((id \
         80d5571c-63ca-4665-865f-b8f1ae67ea0e)(content(Whitespace\"\\n\"))))(Secondary((id \
         03c11848-6844-421e-a238-28cd5763cf6e)(content(Comment\"#   nth(list, \
         index) -> element                #\"))))(Secondary((id \
         df067e90-9519-4d49-b3e2-772718ff99e2)(content(Whitespace\"\\n\"))))(Secondary((id \
         e3cd6c43-b922-4708-994e-f9c2c0222778)(content(Comment\"#   \
         length(list) -> Int                        #\"))))(Secondary((id \
         0bf52aed-e967-4786-8060-f55683298f9e)(content(Whitespace\"\\n\"))))(Secondary((id \
         7aeebe19-e5fe-4ca6-a0f3-166e4a3e42ad)(content(Comment\"#   map(list, \
         fn) -> list                      #\"))))(Secondary((id \
         f629cab5-5d85-4e76-94fb-c04ce15267f7)(content(Whitespace\"\\n\"))))(Secondary((id \
         afcd0516-619c-40f2-bb6b-60c46753ba7c)(content(Comment\"#   \
         filter(list, pred) -> list                 #\"))))(Secondary((id \
         d423836f-ce49-4a1d-8c42-02949d7e9f74)(content(Whitespace\"\\n\"))))(Secondary((id \
         7e948e1b-1f0b-473d-b645-6f7845277b9f)(content(Comment\"#                                              \
         #\"))))(Secondary((id \
         dba57551-2ac4-473c-b672-c7f38053cc6b)(content(Whitespace\"\\n\"))))(Secondary((id \
         da591742-2372-46ce-8ca0-09dedff7e84e)(content(Comment\"# Function \
         syntax: fun param -> body           #\"))))(Secondary((id \
         6d530082-2a1b-43e5-a588-834e3e061cec)(content(Whitespace\"\\n\"))))(Secondary((id \
         f8a44570-cf26-4c84-86df-e8889ea356a6)(content(Comment\"# Let binding: \
         let name = value in ...         #\"))))(Secondary((id \
         9dde249d-5246-4981-8be5-89041a3b99ee)(content(Whitespace\"\\n\"))))(Secondary((id \
         20cace25-1637-4f86-9fb7-afe716b38ba2)(content(Comment\"#                                              \
         #\"))))(Secondary((id \
         59de306d-f459-4622-9936-d1be858e7ffa)(content(Whitespace\"\\n\"))))(Secondary((id \
         9891a4fe-a18d-46f9-9a87-ca58c0b5aa92)(content(Comment\"# Tip: Turn on \
         auto-probe (microscope toggle)  #\"))))(Secondary((id \
         37c8e5db-f082-4a20-8f6c-57ec06fe388e)(content(Whitespace\"\\n\"))))(Secondary((id \
         8150a29b-ecdd-43ba-9b24-d3075404c125)(content(Comment\"# to see \
         intermediate values as you type.      #\"))))(Secondary((id \
         49603ead-6d1c-4563-b903-e3ed6a2a8f78)(content(Whitespace\"\\n\"))))(Secondary((id \
         8e48ea1b-7e87-4ff5-b78e-1a88cff60784)(content(Whitespace\"\\n\"))))(Tile((id \
         0df3c39b-f663-470f-a3f1-cf6b21d0eafd)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         4edd1eb5-2f06-4d4c-b352-0904ea2d563f)(content(Whitespace\" \
         \"))))(Tile((id \
         407bddd6-d9e0-4734-9ea7-7fb3025da1ed)(label(base_route))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         26705c6b-a977-46fd-9268-71f7b1541055)(content(Whitespace\" \
         \")))))((Secondary((id \
         6f9951ed-8e2e-49e5-bfff-a6f615b60df7)(content(Whitespace\" \
         \"))))(Tile((id fc1b0762-3262-49dc-a28a-999ff359f796)(label(fun \
         ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 37))(sort Exp))))))(shards(0 \
         1))(children(((Secondary((id \
         4f7a0980-79fd-49d9-aaaa-9f60fdaad023)(content(Whitespace\" \
         \"))))(Tile((id \
         1163356e-81b5-4a65-b825-8f2e967149c7)(label(path))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         81b91e19-7e23-41ba-ba0a-9c83b9a6e161)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         653ed87d-f3d3-4b75-b4a6-6ba2e89543d7)(content(Whitespace\"\\n\"))))(Tile((id \
         6eb3a073-54a3-40bd-935f-607af6ad4b45)(label(?))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         4e34b0c9-eec7-4259-9487-ead5cad1a1a7)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         257ce850-683a-479e-8b93-19963bd64deb)(content(Whitespace\"\\n\"))))(Secondary((id \
         a1b38549-92c9-44c2-8ec9-b7e0b2a8b552)(content(Whitespace\"\\n\"))))(Tile((id \
         dcb0100e-be34-4c72-bbd4-f0e0f3364725)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         644269a7-ff0f-4189-b671-cc6eaf16ad09)(content(Whitespace\"\\n\"))))(Tile((id \
         1047312a-e3d0-4a7a-93cf-08a1bba2f6de)(label(base_route))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         801bfee0-1b6e-4e75-9a6b-f12504179e6b)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         14a7919c-0630-4d17-880c-e64ec567bc6b)(label(\"\\\"/api/v1\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         95642ac8-0ef0-4757-9bc3-a83ed2126c63)(content(Whitespace\"\\n\"))))(Tile((id \
         45fb2b38-829d-416e-b939-bef96e348687)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         66df0b67-ad6c-4cdf-9263-c2d337afd18b)(content(Whitespace\" \
         \"))))(Tile((id \
         f24efd0c-9827-4a6a-97eb-c82264388e6a)(label(\"\\\"api\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         5d42b745-971e-458a-98ab-ec848410f217)(content(Whitespace\"\\n\")))))))))(Tile((id \
         7ab5f81e-dc2e-419d-a65a-2473a86878a9)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         0d83dc16-b3db-4b01-b4bf-c09cfa02b838)(content(Whitespace\"\\n\"))))(Secondary((id \
         1931f156-064b-4623-8129-ace4f2389cd9)(content(Whitespace\"\\n\"))))(Tile((id \
         5f537c6c-25b9-4d95-8b25-dbdc421f41f1)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         23e07b6d-1be1-4105-8b6b-9a720cb4520a)(content(Whitespace\"\\n\"))))(Tile((id \
         cfd81a68-6e6a-4c72-ad4a-e15a7c88abf2)(label(base_route))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         39fda699-6ee5-4749-b884-1654991d0449)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         14db2e81-7780-40e9-ad78-608d922e75b0)(label(\"\\\"/api/actions/rm\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         d53cfa8e-7474-4365-84ea-b7fba96da181)(content(Whitespace\"\\n\"))))(Tile((id \
         21816a7a-b8fe-410d-af9d-5842637c76cd)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         0e9ce6c4-22fd-4d64-b49a-900cb9f99f93)(content(Whitespace\" \
         \"))))(Tile((id \
         f8ed2100-7440-4785-b3b3-253168ca2aa2)(label(\"\\\"api\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         38560b9e-bbcb-481a-947c-a61148acd4e6)(content(Whitespace\"\\n\")))))))))(Tile((id \
         7b66d0ba-a329-4a9c-9825-fedb34e950a4)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         e1ae4ff4-a188-48bb-9d11-1fac88afc9cd)(content(Whitespace\"\\n\"))))(Secondary((id \
         ac6b829f-33af-477e-b214-00250396ccc6)(content(Whitespace\"\\n\"))))(Tile((id \
         c89218bf-ab29-4df5-b199-30eca6b7087d)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         dc58a4cf-22c9-42f4-893a-4a0b4f4c4376)(content(Whitespace\"\\n\"))))(Tile((id \
         14246298-6de0-4b90-8412-30e1e6c59390)(label(base_route))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         8a490497-dee9-4a76-bd65-b26ed17c75b4)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         ceb6b635-005a-49a5-adc5-dafc51697916)(label(\"\\\"/\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         9b9b5026-b157-4a38-9267-13cfb70a5a71)(content(Whitespace\"\\n\"))))(Tile((id \
         a4426fb1-c9bd-4847-bb0b-b5752bafeb30)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         b8963b7a-9ce4-4d29-be12-1e91d02495c2)(content(Whitespace\" \
         \"))))(Tile((id \
         8b0b8071-17dc-40a6-9b59-2c79fe4f444d)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         d9860693-1aa1-445c-a0d1-ca387f1fba4b)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         a236a198-3746-4824-899f-2d68c2a105a0)(content(Whitespace\"\\n\")))))";
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
