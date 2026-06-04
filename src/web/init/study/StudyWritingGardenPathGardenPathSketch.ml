let out : string * Haz3lcore.PersistentSegment.t =
  ( "Study / writing / garden-path / garden-path-sketch",
    {
      segment =
        "((Secondary((id \
         c4cbdcb0-ea97-4f49-9770-30bb5d663da8)(content(Comment\"# GARDEN PATH \
         TASK                               #\"))))(Secondary((id \
         fc363192-d6d3-4116-ba6f-dcd401344277)(content(Whitespace\"\\n\"))))(Secondary((id \
         726475d0-cbc5-4be7-9bad-3c7b9406f36b)(content(Comment\"#                                                \
         #\"))))(Secondary((id \
         4c533bea-e660-493e-b9f6-4a230bcb88af)(content(Whitespace\"\\n\"))))(Secondary((id \
         847302fc-32f2-488e-824f-09078614dec2)(content(Comment\"# Implement \
         grove_name: extract the first        #\"))))(Secondary((id \
         75b223b4-1318-4202-aaff-f48fcd71f390)(content(Whitespace\"\\n\"))))(Secondary((id \
         0633b5df-6df8-4153-9a02-7b1e0b5c0f66)(content(Comment\"# section from \
         a garden path.                    #\"))))(Secondary((id \
         59d282ad-936e-46a0-870e-559f6be5192b)(content(Whitespace\"\\n\"))))(Secondary((id \
         ae34b00d-921c-40d3-8c11-e72ccaca4ac3)(content(Comment\"#                                                \
         #\"))))(Secondary((id \
         e759e9a7-67b8-472d-a8c4-47fa2161916f)(content(Whitespace\"\\n\"))))(Secondary((id \
         af60a2ba-8166-474c-96bd-3babcdfd73ce)(content(Comment\"# \
         Examples:                                      #\"))))(Secondary((id \
         e6a98b48-e14f-4584-a974-32186bc7b726)(content(Whitespace\"\\n\"))))(Secondary((id \
         537bf605-f289-44c8-bd08-6c7d3f1ca3ca)(content(Comment\"#   \
         grove_name(\\\"/moonlit-grove/ferns\\\")           \
         #\"))))(Secondary((id \
         2aac9c2b-9a8a-4775-9da9-dd6e275dc71b)(content(Whitespace\"\\n\"))))(Secondary((id \
         9e25f6c6-8579-4352-8ca1-52f0e040ddab)(content(Comment\"#     == \
         \\\"moonlit-grove\\\"                         #\"))))(Secondary((id \
         3154c0ac-28a1-4a9e-aeb1-74e2ee299296)(content(Whitespace\"\\n\"))))(Secondary((id \
         2db4b481-456d-4dbb-935f-3abf59bb561c)(content(Comment\"#   \
         grove_name(\\\"/night-garden/herbs/thyme\\\")      \
         #\"))))(Secondary((id \
         041f666b-573a-443f-a566-20881234a1b2)(content(Whitespace\"\\n\"))))(Secondary((id \
         6ec917c6-2968-464f-8c7b-81d59ab610ea)(content(Comment\"#     == \
         \\\"night-garden\\\"                          #\"))))(Secondary((id \
         471811a8-9204-4ea1-a835-689cbfc9ea57)(content(Whitespace\"\\n\"))))(Secondary((id \
         ddca5261-4021-4278-a4d8-319c3e53fa2e)(content(Comment\"#   \
         grove_name(\\\"/\\\") == \\\"\\\"                        \
         #\"))))(Secondary((id \
         fe35be94-cf77-4056-bd4e-e83ccbfad858)(content(Whitespace\"\\n\"))))(Secondary((id \
         b7db5b6e-c6f6-4e42-ad3a-ab0dda707d59)(content(Comment\"#                                                \
         #\"))))(Secondary((id \
         35d37cba-2d15-4578-bd80-b579e12a0033)(content(Whitespace\"\\n\"))))(Secondary((id \
         4888ac77-08b2-4932-aad3-6f1cf833b620)(content(Comment\"# Available \
         functions:                           #\"))))(Secondary((id \
         b16c0032-90ca-454a-b989-711221bf4439)(content(Whitespace\"\\n\"))))(Secondary((id \
         32d19cac-8e27-4389-92fb-ea255edd11df)(content(Comment\"#   \
         string_split(sep, str) -> [String]           #\"))))(Secondary((id \
         ac5555cd-3400-4a4d-a92a-27cd114bdedc)(content(Whitespace\"\\n\"))))(Secondary((id \
         9c59d7fc-a8cf-41cf-88d3-a9c1b6cb2929)(content(Comment\"#   \
         string_concat(s1, s2) -> String              #\"))))(Secondary((id \
         453538ba-b955-4570-8976-e781e0b0fcd4)(content(Whitespace\"\\n\"))))(Secondary((id \
         1462721c-3249-4073-ba5d-41fef9282ed7)(content(Comment\"#   \
         string_length(s) -> Int                      #\"))))(Secondary((id \
         bc97a987-d79f-4c5d-98ed-f2d6b9d9d6da)(content(Whitespace\"\\n\"))))(Secondary((id \
         21e0ed31-62d5-4550-abd0-06d43826d554)(content(Comment\"#   \
         string_sub(str, pos, len) -> String          #\"))))(Secondary((id \
         69ed1289-eb6c-4b08-bb9a-0ebe21bdd470)(content(Whitespace\"\\n\"))))(Secondary((id \
         0e70e2ec-6e1f-4b85-91cd-1cf71bc613b3)(content(Comment\"#   nth(list, \
         index) -> element                  #\"))))(Secondary((id \
         ea26d08d-c533-4665-a13e-ae9873be1f81)(content(Whitespace\"\\n\"))))(Secondary((id \
         5aed3a0e-f250-4837-963d-3c8b8a67b658)(content(Comment\"#   \
         length(list) -> Int                          #\"))))(Secondary((id \
         4a7d5521-19d9-4577-874e-19f420d572fc)(content(Whitespace\"\\n\"))))(Secondary((id \
         1b6b944f-a443-43dd-807e-fd040a9cb63f)(content(Comment\"#   map(list, \
         fn) -> list                        #\"))))(Secondary((id \
         29ddbd79-168b-41d9-96f2-7c4ccf132827)(content(Whitespace\"\\n\"))))(Secondary((id \
         4aff86e7-764a-4eb8-97e7-10b5a3b32d7a)(content(Comment\"#   \
         filter(list, pred) -> list                   #\"))))(Secondary((id \
         1588e90f-554f-4666-a330-36b2012cce09)(content(Whitespace\"\\n\"))))(Secondary((id \
         0cd9f894-4218-4973-94ee-9050ef7b8be4)(content(Comment\"#                                                \
         #\"))))(Secondary((id \
         1f819dcc-f465-4e3d-9619-32ea6980ea0a)(content(Whitespace\"\\n\"))))(Secondary((id \
         f655d6f3-ea94-495b-a0f4-c054d81494da)(content(Comment\"# Function \
         syntax: fun param -> body             #\"))))(Secondary((id \
         67b1790c-c709-4333-af20-43ff80ae7e3f)(content(Whitespace\"\\n\"))))(Secondary((id \
         fc1c1a20-05f0-4b53-9354-ddf91fad3613)(content(Comment\"# Let binding: \
         let name = value in ...           #\"))))(Secondary((id \
         cd3be60b-48f6-44c2-a676-63f4cf6268a7)(content(Whitespace\"\\n\"))))(Secondary((id \
         0d05ff8d-55d6-4c4f-a981-5eb66df26da1)(content(Comment\"#                                                \
         #\"))))(Secondary((id \
         fbdf3fb2-ab0b-4dcc-8d5c-422d03f8bee6)(content(Whitespace\"\\n\"))))(Secondary((id \
         a94fe052-7e47-4eda-b8ce-df8b780fbf52)(content(Comment\"# Tip: Turn on \
         auto-probe (microscope toggle)    #\"))))(Secondary((id \
         2406c604-8ca8-4775-88b1-8f0ca2862761)(content(Whitespace\"\\n\"))))(Secondary((id \
         f5b7224c-1cf8-4b84-8117-03a7a8ea13a4)(content(Comment\"# to see \
         intermediate values as you type.        #\"))))(Secondary((id \
         80a64560-24e5-4aa7-88ac-d43d1350a27a)(content(Whitespace\"\\n\"))))(Secondary((id \
         342954ff-def7-4f90-8305-5e4f6fcc8a08)(content(Whitespace\"\\n\"))))(Tile((id \
         f00331dd-4607-4d1f-b48e-8ea240a80efc)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         4ae459f0-d07b-4025-83e5-7453c878e60b)(content(Whitespace\" \
         \"))))(Tile((id \
         a5422e79-c2cb-4dcb-b307-7f4ccff502d3)(label(grove_name))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         337c0028-4ae5-413e-aab7-410b46d54f9d)(content(Whitespace\" \
         \")))))((Secondary((id \
         25842bb2-7efd-4972-b4b8-a16d54ebee3c)(content(Whitespace\" \
         \"))))(Tile((id 6932cbe0-76b8-41e0-9049-fbcc89456426)(label(fun \
         ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 37))(sort Exp))))))(shards(0 \
         1))(children(((Secondary((id \
         aca0d3af-f906-4b10-b1bf-306e5dcdcc90)(content(Whitespace\" \
         \"))))(Tile((id \
         299bebc7-db5e-4ffb-921e-84c21f71753b)(label(path))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         748c4caa-ea1c-49c7-a573-079f0f70a735)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         bdcc490e-3ce7-4aba-83e8-3707e3cadc9a)(content(Whitespace\"\\n\"))))(Tile((id \
         360382bc-f754-4f24-8505-fbcf1fbadfa2)(label(?))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         44035bb7-a92d-4c5e-9c97-3b895ed4d469)(content(Whitespace\"\\n\"))))(Secondary((id \
         2c422f6a-d0cb-48a7-a095-c1d59603452b)(content(Whitespace\"\\n\"))))(Secondary((id \
         863c9ce0-a236-46cc-8821-1dee0dab351e)(content(Whitespace\"\\n\"))))(Secondary((id \
         73b08f69-4267-4ea1-a83d-37da79c93835)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         4bcb54b7-3ddd-40c3-a444-5c74dbd2b511)(content(Whitespace\"\\n\"))))(Secondary((id \
         59cd4fee-bfa5-4709-96c0-61e067cb605c)(content(Whitespace\"\\n\"))))(Tile((id \
         9041b100-464c-460c-8cd8-890126699490)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         04bd8c2d-18d1-4c71-bf5f-a3d770a349e4)(content(Whitespace\"\\n\"))))(Tile((id \
         6b5a924e-fa50-40b5-b5e8-5c3718e3dcb0)(label(grove_name))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         1a3d7bce-fd15-485a-91a5-f5d80786308c)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         70c29305-5761-4f1a-bc81-8017c0353ddb)(label(\"\\\"/moonlit-grove/ferns/watering\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         5e0d4828-879c-4644-9914-899818c72ed7)(content(Whitespace\"\\n\"))))(Tile((id \
         51b69305-a4df-4c9f-b29c-b83244758778)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         83321816-013d-40fc-ac81-9ca0f3c32fbc)(content(Whitespace\" \
         \"))))(Tile((id \
         2e94ab9c-08b2-4fc3-82e4-3cc4d4988115)(label(\"\\\"moonlit-grove\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         6778122b-5976-4b26-93a4-b22aacc7d1d6)(content(Whitespace\"\\n\")))))))))(Tile((id \
         9c00d6ce-f826-4573-989c-f7bc2475e0d1)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         032a1fd0-bddd-49a8-a6e5-08c9a33fb24a)(content(Whitespace\"\\n\"))))(Secondary((id \
         c0f27489-9005-4f3b-9739-54704ff8ee42)(content(Whitespace\"\\n\"))))(Tile((id \
         c206a35b-95c1-496b-91a5-057f078f39cb)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         f35637d1-9418-4396-8518-ff71b48f2988)(content(Whitespace\"\\n\"))))(Tile((id \
         ebbad274-32a6-4235-a180-b3e64481badb)(label(grove_name))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         46645e1e-8130-4ea5-9966-8deb7ca42c7a)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         1b0c6da6-d81c-4ea6-96df-2fe7d29da8c6)(label(\"\\\"/night-garden/herbs/rosemary\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         ee2fff61-5653-4fc9-b0ad-f69f37428973)(content(Whitespace\"\\n\"))))(Tile((id \
         f43e5b25-bdf0-4fd8-bd88-c5e898961754)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         d4c60314-1dbd-4ada-8d02-0fe4cfc865dd)(content(Whitespace\" \
         \"))))(Tile((id \
         f11ddc91-1d7d-44e0-b562-8739e62b3044)(label(\"\\\"night-garden\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         2ed5eca2-4702-4b4b-b955-85d351613eb5)(content(Whitespace\"\\n\")))))))))(Tile((id \
         4da47ce2-e167-4e6d-962a-88e38a0077c9)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         dcb2e1c7-a628-4cdd-92f4-c4a9f2f93b39)(content(Whitespace\"\\n\"))))(Secondary((id \
         396cfa48-0ec1-4e1d-b324-05ecb17bcc65)(content(Whitespace\"\\n\"))))(Tile((id \
         64288399-68b7-4f96-a272-8b13f9a84f7f)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         4052bf90-c93a-4baa-ae4d-844530c449a5)(content(Whitespace\"\\n\"))))(Tile((id \
         0000171d-60f6-494b-9210-bcdf0884289d)(label(grove_name))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         611d8868-0892-42b6-93a4-b55bd142050c)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         cc2fdb75-6858-4cd8-a574-b8c5e8863bd4)(label(\"\\\"/\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         6c3a302e-5ec1-47b7-b99b-ec97549d0186)(content(Whitespace\"\\n\"))))(Tile((id \
         84aeaffd-faf1-46b3-a3b1-c52fdf633506)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         dc930f2a-506a-4b76-b214-b6914790d93b)(content(Whitespace\" \
         \"))))(Tile((id \
         859974d7-7cae-4e64-9fa5-8cfb706142fb)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         5cd3a98d-0ebd-4079-a524-86173b59e755)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         b0ad047f-4b07-4e2a-b7f4-dbd15fdb1d6c)(content(Whitespace\"\\n\")))))";
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
