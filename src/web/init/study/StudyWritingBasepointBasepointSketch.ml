let out : string * Haz3lcore.PersistentSegment.t =
  ( "Study / writing / basepoint / basepoint-sketch",
    {
      segment =
        "((Secondary((id \
         4272abb6-de27-4835-a8ac-cde460f7e74b)(content(Comment\"# BASE ROUTE \
         TASK                              #\"))))(Secondary((id \
         37a22a6d-7bfd-40c9-a1e2-0999d34c5cdd)(content(Whitespace\"\\n\"))))(Secondary((id \
         1476cfb6-9a2c-43d2-9d6c-2a252cdc1d3f)(content(Comment\"#                                              \
         #\"))))(Secondary((id \
         e92ec2ce-60c8-4660-ae86-de5631c2d3c7)(content(Whitespace\"\\n\"))))(Secondary((id \
         8702a4ea-11f5-4901-8bf3-16eb6cd9d383)(content(Comment\"# Implement \
         base_route: extract the first      #\"))))(Secondary((id \
         49498848-8cba-4a4b-b402-45785db41e3c)(content(Whitespace\"\\n\"))))(Secondary((id \
         0dbf6515-3a2c-42e8-90b3-027a019619c8)(content(Comment\"# path segment \
         from a URL path.                #\"))))(Secondary((id \
         4518348e-66e0-4681-938d-31ce3d75b3f6)(content(Whitespace\"\\n\"))))(Secondary((id \
         c6f5c73b-2e89-46be-a73d-e6d8e7977b19)(content(Comment\"#                                              \
         #\"))))(Secondary((id \
         ae72f4b8-67e5-4faa-a520-846c96d8e695)(content(Whitespace\"\\n\"))))(Secondary((id \
         389bbc7d-5238-4a42-a69f-d0e819d31e33)(content(Comment\"# \
         Examples:                                    #\"))))(Secondary((id \
         63955ed4-09c9-491b-8dff-b3a804549bd3)(content(Whitespace\"\\n\"))))(Secondary((id \
         38160437-ed62-4d61-adc4-a986efdc54cc)(content(Comment\"#   \
         base_route(\\\"/api/v1\\\") == \\\"api\\\"             \
         #\"))))(Secondary((id \
         42a9ff8a-9334-43a3-a780-0b87a8d0a4d4)(content(Whitespace\"\\n\"))))(Secondary((id \
         3c4e8d8c-e88e-40dd-b16b-191c948d8c62)(content(Comment\"#   \
         base_route(\\\"/api/actions/rm\\\") == \\\"api\\\"     \
         #\"))))(Secondary((id \
         4f6c839f-f2e2-402e-a433-bf10af85c6b3)(content(Whitespace\"\\n\"))))(Secondary((id \
         5ff89d90-94f5-466b-8f19-1b5d7df81fd3)(content(Comment\"#   \
         base_route(\\\"/\\\") == \\\"\\\"                      \
         #\"))))(Secondary((id \
         0a2dfd70-67eb-452e-8d60-789115f9dee5)(content(Whitespace\"\\n\"))))(Secondary((id \
         dd459777-2ba7-4ab2-aba4-3624d85644b7)(content(Comment\"#                                              \
         #\"))))(Secondary((id \
         23be5c7c-a29c-4d99-8555-374cf539689b)(content(Whitespace\"\\n\"))))(Secondary((id \
         366bb42c-9661-459f-8be6-665c024ae62d)(content(Comment\"# Available \
         functions:                         #\"))))(Secondary((id \
         febcb2ff-031d-483f-86a9-5ca0167826a1)(content(Whitespace\"\\n\"))))(Secondary((id \
         28549a86-40bb-4cac-a1f7-19d8a30f879b)(content(Comment\"#   \
         string_split(sep, str) -> [String]         #\"))))(Secondary((id \
         321669ab-9ed0-448a-a4cf-012adf1e7ea6)(content(Whitespace\"\\n\"))))(Secondary((id \
         130e3420-288e-40da-8e49-23c7ebdac77e)(content(Comment\"#   \
         string_concat(s1, s2) -> String            #\"))))(Secondary((id \
         7a0daa46-557d-4786-8bf1-5c17e0c8d039)(content(Whitespace\"\\n\"))))(Secondary((id \
         f02f7b9b-05df-4cd5-b01a-d1d81a074f51)(content(Comment\"#   \
         string_length(s) -> Int                    #\"))))(Secondary((id \
         d6b981f8-e1f3-46ce-846c-7ec65f71ea57)(content(Whitespace\"\\n\"))))(Secondary((id \
         fd121fc0-45e9-422a-a5a4-67ecec76aa00)(content(Comment\"#   \
         string_sub(str, pos, len) -> String        #\"))))(Secondary((id \
         61ba621b-6b07-4e77-9d6c-ea7b1c1e834f)(content(Whitespace\"\\n\"))))(Secondary((id \
         e153c51e-2c63-4a76-a2bb-9583e5f98cc3)(content(Comment\"#   nth(list, \
         index) -> element                #\"))))(Secondary((id \
         e7bafb62-b9d5-44d4-b676-c8df2bd5b7b3)(content(Whitespace\"\\n\"))))(Secondary((id \
         49e3c7fc-a9e8-4e9c-8c08-9e688e9d263d)(content(Comment\"#   \
         length(list) -> Int                        #\"))))(Secondary((id \
         68ba6446-c71e-450f-b4cb-1cf2a64d8d3e)(content(Whitespace\"\\n\"))))(Secondary((id \
         c24c8937-3d42-4799-825d-7103c3c64f7d)(content(Comment\"#   map(list, \
         fn) -> list                      #\"))))(Secondary((id \
         7ac031cf-16e1-4248-bf87-90d04577a604)(content(Whitespace\"\\n\"))))(Secondary((id \
         c9dc7a7b-f9f6-4171-8c68-adb5c3adf417)(content(Comment\"#   \
         filter(list, pred) -> list                 #\"))))(Secondary((id \
         c211814a-57eb-417b-804c-8a1dfe8c6457)(content(Whitespace\"\\n\"))))(Secondary((id \
         c676a049-e3ac-4cd3-b621-c05acb018163)(content(Comment\"#                                              \
         #\"))))(Secondary((id \
         fab844e3-0c09-4036-93c1-7ad661c5e2f0)(content(Whitespace\"\\n\"))))(Secondary((id \
         77c62d8d-7255-4dba-b564-234e44d1a268)(content(Comment\"# Function \
         syntax: fun param -> body           #\"))))(Secondary((id \
         52e1d908-bdc8-4ac0-b702-1c109f9f22cf)(content(Whitespace\"\\n\"))))(Secondary((id \
         ecf5c8b2-0170-436e-a40c-71a0095a02de)(content(Comment\"# Let binding: \
         let name = value in ...         #\"))))(Secondary((id \
         b9fbc2a6-bb45-4f39-b35e-d30d313d36ad)(content(Whitespace\"\\n\"))))(Secondary((id \
         c70f9a27-1023-41de-ab42-09339bc765e9)(content(Comment\"#                                              \
         #\"))))(Secondary((id \
         32de5e00-6f7c-40fc-a6bd-c843c92802fa)(content(Whitespace\"\\n\"))))(Secondary((id \
         790df7c1-b552-43a0-9612-f6a1ff120682)(content(Comment\"# Tip: Turn on \
         auto-probe (microscope toggle)  #\"))))(Secondary((id \
         5858ad48-7176-4cd3-9309-c0fe796f9bb4)(content(Whitespace\"\\n\"))))(Secondary((id \
         46d6d6a3-d2cf-4968-abf2-e57ca13ba310)(content(Comment\"# to see \
         intermediate values as you type.      #\"))))(Secondary((id \
         c58dd4ef-916a-4ce1-b1c9-63ed92bce587)(content(Whitespace\"\\n\"))))(Secondary((id \
         2ea822c4-a4f1-4b8b-b056-65f75d20a508)(content(Whitespace\"\\n\"))))(Tile((id \
         eea3d099-828d-443e-b04c-759cd06b6278)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         557d858f-1b7c-482b-a0d1-39b27c996691)(content(Whitespace\" \
         \"))))(Tile((id \
         57c0b562-8c8f-48a3-beab-aa95ff81edab)(label(base_route))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         caa3a154-50dc-4c92-add0-436bdcefb324)(content(Whitespace\" \
         \")))))((Secondary((id \
         b3b55eab-bf6a-44f7-8ced-f627620e07a4)(content(Whitespace\" \
         \"))))(Tile((id cf853832-dd74-4cd2-bfc1-1805ad54304f)(label(fun \
         ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 37))(sort Exp))))))(shards(0 \
         1))(children(((Secondary((id \
         8430099d-6f4d-4b65-aa4b-7bf6b4afba17)(content(Whitespace\" \
         \"))))(Tile((id \
         ba04e43e-6a9e-4bed-8739-218d281600c6)(label(path))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         bebb856c-2747-4efc-8253-572062a432d6)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         61d8a84c-2343-4ca7-b146-0bcb41b6c991)(content(Whitespace\"\\n\"))))(Tile((id \
         7ab0e49e-de8c-4c69-8408-ee82037b947d)(label(?))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         fa45f60a-81ab-48fc-bf76-226f319e5037)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         d606e2a5-36dd-411b-9f22-0349fd520bac)(content(Whitespace\"\\n\"))))(Secondary((id \
         f0969a83-343d-41af-b2b4-d3a9601d1016)(content(Whitespace\"\\n\"))))(Tile((id \
         d1c4188e-c87d-4386-b77f-5f4ed4edaa46)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         93e22443-82f9-4be1-af97-326a10a6c438)(content(Whitespace\"\\n\"))))(Tile((id \
         8b5a09f1-f7f4-4b2a-aab1-6d8e5ef664ee)(label(base_route))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         86516db1-22e8-41f5-b7ac-6d02213324a4)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         4e5d8ac7-7ba9-4214-b62e-fe4690751e12)(label(\"\\\"/api/v1\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         6b5a763a-83bb-4552-a9a0-0c54107353f9)(content(Whitespace\"\\n\"))))(Tile((id \
         8dc5d2bb-aa83-48ff-be3d-a872b94d4df9)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         30a9d89d-a361-4a39-8027-0b9ba227540f)(content(Whitespace\" \
         \"))))(Tile((id \
         24ab3a7b-5bc9-4e00-89f7-df50564d107a)(label(\"\\\"api\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         618196ee-e0db-49ba-91f3-8eac4cef8ca3)(content(Whitespace\"\\n\")))))))))(Tile((id \
         fd821daf-a34c-4f33-a8e6-b1d378059d83)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         698856cc-8eb3-48f8-98a1-a0827275df2f)(content(Whitespace\"\\n\"))))(Secondary((id \
         e8ac1ca9-047d-45ae-9c39-fda5c364bc0e)(content(Whitespace\"\\n\"))))(Tile((id \
         bd921cfd-0a6d-4ed3-be55-722e0c5aab63)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         e80c3911-9321-4932-bdf8-08187db96c07)(content(Whitespace\"\\n\"))))(Tile((id \
         ba749df8-22d1-48b7-adec-3794b1128796)(label(base_route))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         c31bae6a-85a7-424e-9a5e-9c53634aea81)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         3db32461-5d6f-4ae1-a7d8-2b7e1dd026af)(label(\"\\\"/api/actions/rm\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         dfd44a6c-0145-4941-8dc5-37117db40dc9)(content(Whitespace\"\\n\"))))(Tile((id \
         cf59c6b9-0408-4c81-ae4d-ed5530b6beb7)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         ab4324fc-46ad-419f-8f69-6d590b45eedb)(content(Whitespace\" \
         \"))))(Tile((id \
         8d307b32-5be4-4966-b4a7-02c1351a3859)(label(\"\\\"api\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         57e65e62-fc06-4a1a-813a-3c5a232ac696)(content(Whitespace\"\\n\")))))))))(Tile((id \
         62f26f8d-0698-4758-afd9-4ad8f07e65a6)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         05ee6e2c-2b99-455f-b4bf-6463ed9e93c1)(content(Whitespace\"\\n\"))))(Secondary((id \
         50b65472-e3a9-4dd7-ac03-721f900f7183)(content(Whitespace\"\\n\"))))(Tile((id \
         728f57d3-a566-4c20-a717-874c5bce31e4)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         7acc20e4-2d54-4802-a444-4dae5fc5ee3c)(content(Whitespace\"\\n\"))))(Tile((id \
         6efddff6-8ab2-4b6a-a6b4-f1206cc27f4e)(label(base_route))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         583f8be8-bec7-4111-a9b7-4a276916483a)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         e9b6c197-8240-45fc-86ce-10029594143e)(label(\"\\\"/\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         5187df8a-7b63-4bc4-a130-33283d14259f)(content(Whitespace\"\\n\"))))(Tile((id \
         b08e2d0b-961a-4267-a6b9-ba397ff55715)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         b22e28bc-1cb9-4a4e-b778-c31ddd231c9c)(content(Whitespace\" \
         \"))))(Tile((id \
         1f4fd18a-28d4-4a56-a7fa-5cad7c8b9815)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         e9051526-8c01-41cf-ad5f-42b7de08bbd6)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         4535260c-dd00-466b-a565-44fbabba66e8)(content(Whitespace\"\\n\")))))";
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
