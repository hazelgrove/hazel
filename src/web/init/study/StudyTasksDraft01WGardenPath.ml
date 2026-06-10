let out : string * Haz3lcore.PersistentSegment.t =
  ( "Study / tasks-draft / 01W-garden-path",
    {
      segment =
        "((Secondary((id \
         af2bb1a0-97bc-4ae8-bae1-8c831834fcb8)(content(Comment\"# GARDEN PATH \
         TASK                               #\"))))(Secondary((id \
         4dbb3407-5b7b-4bb5-9f76-e00feaf3ff25)(content(Whitespace\"\\n\"))))(Secondary((id \
         5ea69e95-d84a-474f-a71a-222fb904f1ea)(content(Comment\"#                                                \
         #\"))))(Secondary((id \
         bbd0cd6d-9ddc-45ec-a7e5-50687a794f75)(content(Whitespace\"\\n\"))))(Secondary((id \
         19f17976-f717-4a7e-9653-38379510942b)(content(Comment\"# Implement \
         grove_name: extract the first        #\"))))(Secondary((id \
         610944da-d332-41d8-a226-8466d7d4d1ce)(content(Whitespace\"\\n\"))))(Secondary((id \
         4f304139-7d47-4a07-b815-0b56c9d398ee)(content(Comment\"# section from \
         a garden path.                    #\"))))(Secondary((id \
         a4e80fa5-4486-40e8-bd63-6005fdf5273c)(content(Whitespace\"\\n\"))))(Secondary((id \
         621e460e-be0d-4da6-9c10-0207abc10140)(content(Comment\"#                                                \
         #\"))))(Secondary((id \
         66784d99-3973-406f-b70e-40d7315b7c67)(content(Whitespace\"\\n\"))))(Secondary((id \
         2aa8aa0c-d1c8-45a6-adc7-c4aaff11afd9)(content(Comment\"# \
         Examples:                                      #\"))))(Secondary((id \
         6ab713f3-83e5-475d-ab26-da86c08266bc)(content(Whitespace\"\\n\"))))(Secondary((id \
         2cfd6a89-a24b-4546-b77c-3cef7f206222)(content(Comment\"#   \
         grove_name(\\\"/moonlit-grove/ferns\\\")           \
         #\"))))(Secondary((id \
         9dcc8116-0351-45c5-a8b1-cd9de9a323a9)(content(Whitespace\"\\n\"))))(Secondary((id \
         f47c5c6d-f254-4400-bbf7-386b4a5edd93)(content(Comment\"#     == \
         \\\"moonlit-grove\\\"                         #\"))))(Secondary((id \
         13345797-19ab-4862-b048-45ee89909e60)(content(Whitespace\"\\n\"))))(Secondary((id \
         56469b47-010e-4933-ae76-eaf300a85469)(content(Comment\"#   \
         grove_name(\\\"/night-garden/herbs/thyme\\\")      \
         #\"))))(Secondary((id \
         905f527f-48b7-4fa2-9e72-26fa2e308143)(content(Whitespace\"\\n\"))))(Secondary((id \
         dcb501dd-2cf9-403b-9ce6-1ab946409727)(content(Comment\"#     == \
         \\\"night-garden\\\"                          #\"))))(Secondary((id \
         e10abe42-8bb5-4986-9770-3d9ea6f313e1)(content(Whitespace\"\\n\"))))(Secondary((id \
         2eeb6beb-dc88-497c-abe4-22e15e5558dd)(content(Comment\"#   \
         grove_name(\\\"/\\\") == \\\"\\\"                        \
         #\"))))(Secondary((id \
         32296b39-c550-44f7-b469-c50dc6e5694b)(content(Whitespace\"\\n\"))))(Secondary((id \
         bdc4961b-c0c0-40a6-b526-06cf9194146f)(content(Comment\"#                                                \
         #\"))))(Secondary((id \
         4c80ad70-ed80-4233-8796-fc9cbe5a0506)(content(Whitespace\"\\n\"))))(Secondary((id \
         13a38081-5055-4dc6-996b-2fc3ae3f2f63)(content(Comment\"# Some \
         standard library functions that may be    #\"))))(Secondary((id \
         47479b27-0cf7-4bb4-9afe-c10d9d0fbcba)(content(Whitespace\"\\n\"))))(Secondary((id \
         b1178ac1-8c47-4b93-bb90-41c6be76e33b)(content(Comment\"# useful (work \
         out the argument order with       #\"))))(Secondary((id \
         43694c55-9b55-4a73-84e7-386d4f722100)(content(Whitespace\"\\n\"))))(Secondary((id \
         8531bc2d-6d71-4f48-b599-a130a5b208ef)(content(Comment\"# \
         probes):                                       #\"))))(Secondary((id \
         8b9d49e1-c715-4cdd-a74b-c4a4f2d3824d)(content(Whitespace\"\\n\"))))(Secondary((id \
         4cb9ab6d-a886-47a0-af6a-a316903289b9)(content(Comment\"#   \
         string_split : (String, String) -> [String]  #\"))))(Secondary((id \
         0e778cfd-27c8-4a28-a0d3-2a1fea829937)(content(Whitespace\"\\n\"))))(Secondary((id \
         93780091-3618-4d72-8936-6c2983cff9d3)(content(Comment\"#   string_sub \
         : (String, Int, Int) -> String    #\"))))(Secondary((id \
         617bbe9e-b6bd-4a64-b421-c2f3906c90a8)(content(Whitespace\"\\n\"))))(Secondary((id \
         313f0c06-545b-4b06-ad7e-a7c101a02c7e)(content(Comment\"#   \
         string_length : String -> Int                #\"))))(Secondary((id \
         9af3a6cf-869f-4cc4-9c00-89913bde62c0)(content(Whitespace\"\\n\"))))(Secondary((id \
         077de492-6191-4aa6-827b-ac0373e4fc90)(content(Comment\"#   nth : \
         ([String], Int) -> String              #\"))))(Secondary((id \
         f3d1b362-ac03-4644-a356-f81123a62bb1)(content(Whitespace\"\\n\"))))(Secondary((id \
         c09cc974-ab83-47f1-8d7f-a5f484d7f096)(content(Comment\"#   length : \
         [String] -> Int                     #\"))))(Secondary((id \
         56b74e8f-3362-454b-ae0a-49908f1b0b13)(content(Whitespace\"\\n\"))))(Secondary((id \
         bfa26ca1-8240-479a-9022-bfc1e7c2f0f2)(content(Comment\"#                                                \
         #\"))))(Secondary((id \
         52ab499f-c0be-4b27-8211-f41ccfc618fe)(content(Whitespace\"\\n\"))))(Secondary((id \
         1134b1d9-8652-4114-bd6e-7ed71bca068a)(content(Comment\"# Tip: turn on \
         auto-probe to see intermediate    #\"))))(Secondary((id \
         cacd2974-5ced-4c3a-ba64-c3fed33a9d33)(content(Whitespace\"\\n\"))))(Secondary((id \
         4cf05893-71e5-4247-9c1a-9435ee46462f)(content(Comment\"# values as \
         you type.                            #\"))))(Secondary((id \
         22757709-d90e-473f-bde1-ae3bc7e129bc)(content(Whitespace\"\\n\"))))(Secondary((id \
         0e529367-4bc9-460b-8c7a-ad8d4859cc48)(content(Whitespace\"\\n\"))))(Tile((id \
         2cec3658-18b4-4b58-bb91-8e8010ec3c88)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         94eaeeb7-8fd0-4b1f-a64d-b9ba42bfed05)(content(Whitespace\" \
         \"))))(Tile((id \
         66640050-c22a-402e-8f20-4650f6f1f314)(label(grove_name))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         1ac1fd49-8363-40be-9451-c159932d42bc)(content(Whitespace\" \
         \")))))((Secondary((id \
         d3d13a18-4aa3-4ee4-a83d-2003464e6bc6)(content(Whitespace\" \
         \"))))(Tile((id 0e165ee1-63b1-4894-bd9e-18d1d50017ec)(label(fun \
         ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 37))(sort Exp))))))(shards(0 \
         1))(children(((Secondary((id \
         3bd89d78-24c1-4ac0-87db-7c9012c85a63)(content(Whitespace\" \
         \"))))(Tile((id \
         e5954b00-d526-47d0-ada3-0f896ba84670)(label(path))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         11b70dd4-3688-4eb8-baa4-0f22495bba48)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         e61179b4-182e-4660-b85f-2d70fcd0f3e4)(content(Whitespace\"\\n\"))))(Tile((id \
         111fc7b7-0e51-439e-9edb-833ac8458911)(label(?))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         3d581056-780c-49d6-a85c-ad297d1d4c7a)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         36371a35-586d-4ea4-8050-6193942d9069)(content(Whitespace\"\\n\"))))(Secondary((id \
         3c35e8c3-530f-444a-8658-dd55b49e85fc)(content(Whitespace\"\\n\"))))(Tile((id \
         b8fa7153-94e4-4728-92ef-720fd54d0114)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         bd3a8003-e8dc-463a-9d2c-8b4189733eaf)(content(Whitespace\"\\n\"))))(Tile((id \
         561ae612-433d-4a57-9f73-5a29c87a5b61)(label(grove_name))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         99d35564-e88e-452d-8f3e-9d484953fd75)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         e8fdc64e-65dd-43ee-a921-592d0ea65a96)(label(\"\\\"/moonlit-grove/ferns/watering\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         438d8551-e9b3-4bb1-964a-38dd81a7749a)(content(Whitespace\"\\n\"))))(Tile((id \
         49813cfa-bcc1-4fb1-840b-67f06f55c3a3)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         ba0e3fc6-b5d6-43e6-96d1-809bd5af2881)(content(Whitespace\" \
         \"))))(Tile((id \
         24e5ad15-7dc8-4ac2-9f05-361127d902df)(label(\"\\\"moonlit-grove\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         64ce7e56-e746-4e27-8b48-f029fe39d976)(content(Whitespace\"\\n\")))))))))(Tile((id \
         aec90532-206c-4d63-8a0a-f147241ca898)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         ec664759-d66c-4a74-81ea-462386d69f5f)(content(Whitespace\"\\n\"))))(Secondary((id \
         d1068a85-e18d-4e81-9a9b-da3300f0a7cf)(content(Whitespace\"\\n\"))))(Tile((id \
         2b52d017-1da4-4fe9-9365-be464e18903c)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         3621bca8-6c6b-4401-97ea-27196f2aeed2)(content(Whitespace\"\\n\"))))(Tile((id \
         6ea26c0a-c2b1-4b56-8f8c-27f97b52853b)(label(grove_name))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         4837d018-59bd-46c2-8335-09cc5bc2e409)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         cb36c59a-d206-4a24-a164-4a0062b824a0)(label(\"\\\"/night-garden/herbs/rosemary\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         341dd22c-9ce2-46e6-8360-23abd7a35b97)(content(Whitespace\"\\n\"))))(Tile((id \
         4b822ad7-4edc-4762-8269-8c026c374feb)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         379c88fd-69d0-49e7-a05e-f75721d9b22d)(content(Whitespace\" \
         \"))))(Tile((id \
         2661d94a-cc87-4fc6-b178-478b3afdbb5e)(label(\"\\\"night-garden\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         78c7cfe1-f4ea-4373-a9be-19206fd7d5d1)(content(Whitespace\"\\n\")))))))))(Tile((id \
         7a95b000-3fd2-4bfa-80ac-c16bdfd85fa8)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         e45b6307-6960-43be-ac15-bd7b0949b316)(content(Whitespace\"\\n\"))))(Secondary((id \
         0a8b498f-ccb9-450a-b514-6a051d4501cb)(content(Whitespace\"\\n\"))))(Tile((id \
         a18633e8-2407-403b-ad8c-822af7b61dbd)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         422d5d26-7178-411f-abe3-1dae13ebb4a2)(content(Whitespace\"\\n\"))))(Tile((id \
         f137d578-13f3-461b-913c-0d70784642cc)(label(grove_name))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         a451cef3-fde7-4250-b8d9-9da1b26f71c4)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         2e657977-0236-4134-9386-09c663c96199)(label(\"\\\"/\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         6ef61752-8c03-40c9-bf81-b58757732514)(content(Whitespace\"\\n\"))))(Tile((id \
         6f786e17-e8e5-4de4-9dc7-4ec2cf7ffabe)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         03f3b998-818b-4bd5-b596-83b73bac3365)(content(Whitespace\" \
         \"))))(Tile((id \
         030ee37f-9290-4a19-8911-80a21c1233a4)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         c5e9e769-ebbd-482a-bcb2-731e713df862)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         9edccb0a-f02d-43b0-85aa-78d3004a4148)(content(Whitespace\"\\n\")))))";
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
         # Some standard library functions that may be    #\n\
         # useful (work out the argument order with       #\n\
         # probes):                                       #\n\
         #   string_split : (String, String) -> [String]  #\n\
         #   string_sub : (String, Int, Int) -> String    #\n\
         #   string_length : String -> Int                #\n\
         #   nth : ([String], Int) -> String              #\n\
         #   length : [String] -> Int                     #\n\
         #                                                #\n\
         # Tip: turn on auto-probe to see intermediate    #\n\
         # values as you type.                            #\n\n\
         let grove_name = fun path ->\n\
         ?\n\
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
