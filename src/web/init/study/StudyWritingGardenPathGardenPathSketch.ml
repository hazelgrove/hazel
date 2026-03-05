let out : string * Haz3lcore.PersistentSegment.t =
  ( "Study / writing / garden-path / garden-path-sketch",
    {
      segment =
        "((Secondary((id \
         25807268-294f-4f94-b13c-c92e899b4350)(content(Comment\"# GARDEN PATH \
         TASK                               #\"))))(Secondary((id \
         598eaac9-7cd3-4518-b479-4f719509fced)(content(Whitespace\"\\n\"))))(Secondary((id \
         a75ac6a3-7788-4989-9964-c3f227d25b3f)(content(Comment\"#                                                \
         #\"))))(Secondary((id \
         1b140850-bc5f-417b-9961-43398f9e5876)(content(Whitespace\"\\n\"))))(Secondary((id \
         c98a19d6-4333-4491-8510-0d68fab8b685)(content(Comment\"# Implement \
         grove_name: extract the first        #\"))))(Secondary((id \
         4965de6a-dfe6-4aa5-878d-51912854ab41)(content(Whitespace\"\\n\"))))(Secondary((id \
         01a1bff5-43d6-47b4-b435-b702c67d16ff)(content(Comment\"# section from \
         a garden path.                    #\"))))(Secondary((id \
         7f305a2d-9df7-4c00-8ef9-48c789bfa18e)(content(Whitespace\"\\n\"))))(Secondary((id \
         ac88b04e-868d-4abd-9a53-e6de689f5a59)(content(Comment\"#                                                \
         #\"))))(Secondary((id \
         44a7b162-192a-4d50-afd0-32b9f761e67f)(content(Whitespace\"\\n\"))))(Secondary((id \
         a15a3bb7-29d9-4640-98a6-b869b9167c8c)(content(Comment\"# \
         Examples:                                      #\"))))(Secondary((id \
         cea5dbd1-1b02-45fb-8377-11f6dbe4c62e)(content(Whitespace\"\\n\"))))(Secondary((id \
         56b69794-b33f-42d3-a665-943e3b7daafd)(content(Comment\"#   \
         grove_name(\\\"/moonlit-grove/ferns\\\")           \
         #\"))))(Secondary((id \
         b7dd6146-b432-4f80-a291-68134c6c2eb7)(content(Whitespace\"\\n\"))))(Secondary((id \
         286f11e9-00f1-4550-900f-a0292824c744)(content(Comment\"#     == \
         \\\"moonlit-grove\\\"                         #\"))))(Secondary((id \
         dffa4f38-35ed-4751-a204-087d51b132bd)(content(Whitespace\"\\n\"))))(Secondary((id \
         cf5a16e5-f47d-4e93-80f2-34cfdb71ce03)(content(Comment\"#   \
         grove_name(\\\"/night-garden/herbs/thyme\\\")      \
         #\"))))(Secondary((id \
         46427568-5ae4-4f98-a04c-ec8516cd7a13)(content(Whitespace\"\\n\"))))(Secondary((id \
         583ea2ed-077d-4e6c-8950-0ec4aa592438)(content(Comment\"#     == \
         \\\"night-garden\\\"                          #\"))))(Secondary((id \
         ec33bf0d-0c00-413f-ad96-4b8416b01a87)(content(Whitespace\"\\n\"))))(Secondary((id \
         efd530be-ea07-4353-8190-85232b54f432)(content(Comment\"#   \
         grove_name(\\\"/\\\") == \\\"\\\"                        \
         #\"))))(Secondary((id \
         07f66dfa-e344-41bc-8257-4dc29c1161b9)(content(Whitespace\"\\n\"))))(Secondary((id \
         d5b540da-5e41-40ad-9994-1cf899897e05)(content(Comment\"#                                                \
         #\"))))(Secondary((id \
         8a5994d0-5529-4a61-bbaa-92abfe58a218)(content(Whitespace\"\\n\"))))(Secondary((id \
         da648545-2645-4724-ae52-5ff9243b6562)(content(Comment\"# Available \
         functions:                           #\"))))(Secondary((id \
         575da2fa-9875-4f5b-92a4-247f43c3f83c)(content(Whitespace\"\\n\"))))(Secondary((id \
         fc31b870-db63-4437-a42b-edca19cae394)(content(Comment\"#   \
         string_split(sep, str) -> [String]           #\"))))(Secondary((id \
         71d5b0a2-be89-45da-995f-645c91901935)(content(Whitespace\"\\n\"))))(Secondary((id \
         48646428-a538-46c0-be58-614656deac29)(content(Comment\"#   \
         string_concat(s1, s2) -> String              #\"))))(Secondary((id \
         911f43bb-b08c-4de0-91c3-1e367473ef23)(content(Whitespace\"\\n\"))))(Secondary((id \
         aeb32a43-24ff-4e50-bbd1-ed066cbef652)(content(Comment\"#   \
         string_length(s) -> Int                      #\"))))(Secondary((id \
         611dfd64-a82a-4296-b5bf-e2edb236b6c8)(content(Whitespace\"\\n\"))))(Secondary((id \
         b98bda2b-a1e3-4da3-836d-33b98a536b11)(content(Comment\"#   \
         string_sub(str, pos, len) -> String          #\"))))(Secondary((id \
         a213a7d1-279f-4e6a-8092-d711feaa1fdc)(content(Whitespace\"\\n\"))))(Secondary((id \
         131c2256-c7c6-4399-9789-5bd2f15a5955)(content(Comment\"#   nth(list, \
         index) -> element                  #\"))))(Secondary((id \
         1a895ab7-9041-4252-8811-b7f829452da1)(content(Whitespace\"\\n\"))))(Secondary((id \
         5307ff1a-ce0b-4d2e-bf51-5bbc8177449f)(content(Comment\"#   \
         length(list) -> Int                          #\"))))(Secondary((id \
         8ef93ced-cf17-4edf-8b80-e7e16d7a4e80)(content(Whitespace\"\\n\"))))(Secondary((id \
         df8d0d91-05b1-4b6c-9c0e-7a0f8c7e26dc)(content(Comment\"#   map(list, \
         fn) -> list                        #\"))))(Secondary((id \
         87849b75-088c-4d22-abd7-a72de5b7d7b6)(content(Whitespace\"\\n\"))))(Secondary((id \
         615e5e19-9c09-4188-8cfc-a1471281fa94)(content(Comment\"#   \
         filter(list, pred) -> list                   #\"))))(Secondary((id \
         dbe85a38-2c68-43fe-bd26-ca9d0288a6e7)(content(Whitespace\"\\n\"))))(Secondary((id \
         d8f0fe1f-8713-4660-b4aa-3193326f84a3)(content(Comment\"#                                                \
         #\"))))(Secondary((id \
         0d4273ad-52fb-4af0-b58d-a5db98dac073)(content(Whitespace\"\\n\"))))(Secondary((id \
         173d20e0-81b5-48c3-b25b-397caee06c40)(content(Comment\"# Function \
         syntax: fun param -> body             #\"))))(Secondary((id \
         96829f84-a49e-471c-ad12-f8eb7ddcdff4)(content(Whitespace\"\\n\"))))(Secondary((id \
         9e60b970-00da-4c69-ab2e-44a7c9ef6c62)(content(Comment\"# Let binding: \
         let name = value in ...           #\"))))(Secondary((id \
         e3f42878-be79-41fe-a653-124b9f66a03e)(content(Whitespace\"\\n\"))))(Secondary((id \
         8faa03ea-7b9c-4fc2-a6b5-2e8c40674536)(content(Comment\"#                                                \
         #\"))))(Secondary((id \
         40f27656-840e-4f0d-a61f-c63daf544ade)(content(Whitespace\"\\n\"))))(Secondary((id \
         eb07cbe9-7119-4e4f-af9a-450331761393)(content(Comment\"# Tip: Turn on \
         auto-probe (microscope toggle)    #\"))))(Secondary((id \
         ae9dbee9-806b-4d0c-a149-f08bc770a340)(content(Whitespace\"\\n\"))))(Secondary((id \
         9d15a8e6-0742-46b5-b4eb-5ad22eb1bcab)(content(Comment\"# to see \
         intermediate values as you type.        #\"))))(Secondary((id \
         44a00421-66a5-4869-bc90-ce646f643f57)(content(Whitespace\"\\n\"))))(Secondary((id \
         70a2cbeb-75bd-4ef1-b5f7-3817f46f77c2)(content(Whitespace\"\\n\"))))(Tile((id \
         a82ac76a-f052-489e-ae86-9b068e209dfb)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         43deab8c-425e-4a1c-9dd6-e5fe2616dfdb)(content(Whitespace\" \
         \"))))(Tile((id \
         b865fe11-8632-4ca3-a519-7b5528b08cc0)(label(grove_name))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         6dd449cb-36ca-43b7-ae74-f99d30c9d448)(content(Whitespace\" \
         \")))))((Secondary((id \
         4d1b5b48-4f7d-4c06-a6eb-dcd64e6ec492)(content(Whitespace\" \
         \"))))(Tile((id db3aa38b-c9a8-4923-be7d-e7218b2b3baf)(label(fun \
         ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 37))(sort Exp))))))(shards(0 \
         1))(children(((Secondary((id \
         1dd394c0-e7cb-4955-8338-0f71493ba514)(content(Whitespace\" \
         \"))))(Tile((id \
         f7c13947-79f1-4302-b65c-f5edab4a71e3)(label(path))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         49da0575-d5ec-4351-b8da-38117eee17c3)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         56ed696e-d514-48fe-aa51-ed79ed7d5dd1)(content(Whitespace\"\\n\"))))(Tile((id \
         ad9d47f0-8cd9-4c28-878a-a5a4c872139a)(label(?))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         6682e0b3-3e9e-4816-b083-fbd6e30d42ab)(content(Whitespace\"\\n\"))))(Secondary((id \
         e7134700-b447-4181-ae1b-25a50b224f68)(content(Whitespace\"\\n\"))))(Secondary((id \
         c1437eaf-29d1-48ef-8b79-6eec3d2a243d)(content(Whitespace\"\\n\"))))(Secondary((id \
         98efe4de-6a49-492a-9a34-3085b58acdf8)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         cc6dc29c-ce10-4a34-9dad-38a835f3d8cf)(content(Whitespace\"\\n\"))))(Secondary((id \
         b86b0b64-f2b0-4392-85ed-e57728398c33)(content(Whitespace\"\\n\"))))(Tile((id \
         ebec7810-bd7c-46c7-8e33-0caaa3e2eb7b)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         bec94518-c300-440d-a016-44a5ad201721)(content(Whitespace\"\\n\"))))(Tile((id \
         6e8d4f35-94c3-4da9-a86d-60cd8abcf798)(label(grove_name))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         7291c83a-ab66-4a91-b1a7-1f7310a03b41)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         782e7c8a-65e7-4cf1-a4ff-016f584088e1)(label(\"\\\"/moonlit-grove/ferns/watering\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         ac5b3c7e-f160-4a69-b0e9-78f37f7b00e5)(content(Whitespace\"\\n\"))))(Tile((id \
         d8fbd3ec-bda8-49e5-ae98-92a78c2bd4d9)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         d9bef7aa-b440-4c71-9086-e5e01e0bfee6)(content(Whitespace\" \
         \"))))(Tile((id \
         57429182-dc67-4e1b-89bd-bb240bee934b)(label(\"\\\"moonlit-grove\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         57dbe21f-d069-430c-ad0c-d819179689de)(content(Whitespace\"\\n\")))))))))(Tile((id \
         15696e91-d0f0-4429-b2e8-27996660a99d)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         4bec4d69-9dc0-4c71-b218-2e3193b1c39c)(content(Whitespace\"\\n\"))))(Secondary((id \
         97b1eb72-6fbc-41a7-a574-9defb9bf2f4d)(content(Whitespace\"\\n\"))))(Tile((id \
         c4fef013-13f1-4048-b3f5-b98facff0ec1)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         339bcfe6-f79f-4391-956a-145461e57cb7)(content(Whitespace\"\\n\"))))(Tile((id \
         9c574411-e2fc-4497-91a9-441229810bcd)(label(grove_name))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         3d2b4d73-ca56-4714-81d9-253648b84bb5)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         13816b07-5199-4743-8b82-d4827daa7223)(label(\"\\\"/night-garden/herbs/rosemary\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         a36dbd59-ca3b-404c-90e0-147ff8e39760)(content(Whitespace\"\\n\"))))(Tile((id \
         2d136177-c5a5-4e4b-ab01-a3965cd7c4fc)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         a0098cdd-5ac3-49fe-80e8-e1ed413e6276)(content(Whitespace\" \
         \"))))(Tile((id \
         f65d9a20-7262-4009-a310-041ce829aa2d)(label(\"\\\"night-garden\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         062a37b8-d90a-439d-8785-8bced6ba87d0)(content(Whitespace\"\\n\")))))))))(Tile((id \
         69e19761-2721-489a-bc1f-d7ea2f758af2)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         9fde7042-940a-4360-8e4a-996278cca5ac)(content(Whitespace\"\\n\"))))(Secondary((id \
         52719065-089f-4e8c-9347-f295cfbf5b4c)(content(Whitespace\"\\n\"))))(Tile((id \
         e4fb50c4-3a6c-4c7b-9ccc-791b502054a7)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         3d5437ee-4e2d-41b8-bc48-9a615dbf16fd)(content(Whitespace\"\\n\"))))(Tile((id \
         c6ea09f4-f909-4bf1-8b8f-d1cb4973e6c7)(label(grove_name))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         ec012694-b6a0-4b23-b581-1a7a5f9222ee)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         0b8f1990-99b7-4a04-8271-d992e14252ba)(label(\"\\\"/\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         318f42de-b982-43db-b65a-3c3017efb03c)(content(Whitespace\"\\n\"))))(Tile((id \
         f0cb794e-b449-49e3-80ec-12fc8d39e45a)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         dd216267-d0c4-40c2-b560-dfbc29c26759)(content(Whitespace\" \
         \"))))(Tile((id \
         473f6ad1-a170-4765-a805-c32e5b15710e)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         463a8831-ebff-4b49-b9af-9ba90a252b87)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         20bd930f-ddf9-4931-98cc-de84cf0b15ee)(content(Whitespace\"\\n\")))))";
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
