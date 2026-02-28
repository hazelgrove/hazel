let out : string * Haz3lcore.PersistentSegment.t =
  ( "Study / writing / grove-path / grove-path-sketch",
    {
      segment =
        "((Secondary((id \
         3d75b412-02dc-4bdf-9136-542a54e40ade)(content(Comment\"# GROVE PATH \
         TASK                                #\"))))(Secondary((id \
         5afecd67-29cf-486e-ae9e-7afac7a14fd5)(content(Whitespace\"\\n\"))))(Secondary((id \
         a5654087-ddf2-4023-9d51-84d110adb319)(content(Comment\"#                                                \
         #\"))))(Secondary((id \
         75f0051f-9939-4cc1-bdea-1ba9300155cf)(content(Whitespace\"\\n\"))))(Secondary((id \
         0fd170ac-8201-4432-985c-93851eae6cd0)(content(Comment\"# Implement \
         grove_name: extract the first        #\"))))(Secondary((id \
         7a02282d-af5f-4b5a-8278-e100b66c2e9c)(content(Whitespace\"\\n\"))))(Secondary((id \
         567ef625-6bac-4155-8775-7f5c402701f0)(content(Comment\"# section from \
         a garden path.                    #\"))))(Secondary((id \
         52fcd574-f236-4965-bb14-6d99441f170d)(content(Whitespace\"\\n\"))))(Secondary((id \
         c041dd61-aad7-418d-9121-80597e3ca68a)(content(Comment\"#                                                \
         #\"))))(Secondary((id \
         4b02b931-da5d-4856-881f-52b0d1a7c1ec)(content(Whitespace\"\\n\"))))(Secondary((id \
         32bb7153-32e9-47bb-a682-6dc486c5290b)(content(Comment\"# \
         Examples:                                      #\"))))(Secondary((id \
         3acb88f9-fe3d-4814-bb1c-664d67d3c9b3)(content(Whitespace\"\\n\"))))(Secondary((id \
         3797d081-440c-43a8-8701-59195799ce6a)(content(Comment\"#   \
         grove_name(\\\"/moonlit-grove/ferns\\\") == \\\"moonlit-grove\\\"  \
         #\"))))(Secondary((id \
         95c8059f-e941-4f52-bf4a-4cb9b7ba6f53)(content(Whitespace\"\\n\"))))(Secondary((id \
         1fca2196-6762-4384-b14e-ebe053887ba9)(content(Comment\"#   \
         grove_name(\\\"/night-garden/herbs/rosemary\\\") == \
         \\\"night-garden\\\"  #\"))))(Secondary((id \
         0e0c637f-1616-4a4b-aea7-3045ce438fad)(content(Whitespace\"\\n\"))))(Secondary((id \
         70edad03-d460-4202-a959-58161f5fa1e5)(content(Comment\"#   \
         grove_name(\\\"/\\\") == \\\"\\\"                        \
         #\"))))(Secondary((id \
         7d7cd37f-3239-4f16-8353-3c285cc554d8)(content(Whitespace\"\\n\"))))(Secondary((id \
         0151b52e-5364-49f6-b119-de3ed7e98196)(content(Comment\"#                                                \
         #\"))))(Secondary((id \
         5ad74ddf-a9c8-44ea-abe4-23f758eca137)(content(Whitespace\"\\n\"))))(Secondary((id \
         b7d7d3d5-1d23-49ad-97b4-822519122f9d)(content(Comment\"# Available \
         functions:                           #\"))))(Secondary((id \
         57d0510d-4dd1-432a-a918-abc547a541fa)(content(Whitespace\"\\n\"))))(Secondary((id \
         443537f5-d4cc-4b24-aebc-45166c04c91e)(content(Comment\"#   \
         string_split(sep, str) -> [String]           #\"))))(Secondary((id \
         dffc0c8d-7d75-4975-b669-e48d2835ba9b)(content(Whitespace\"\\n\"))))(Secondary((id \
         3f9c3732-140a-4031-bfd0-4887da725562)(content(Comment\"#   \
         string_concat(s1, s2) -> String              #\"))))(Secondary((id \
         c226cc94-ab28-44c6-ad0c-875cd1d5c3f2)(content(Whitespace\"\\n\"))))(Secondary((id \
         77b914ce-769d-4ab2-bf23-d497dd977ac0)(content(Comment\"#   \
         string_length(s) -> Int                      #\"))))(Secondary((id \
         e6eaf5e7-3508-41cf-a21d-b0913fbd0d72)(content(Whitespace\"\\n\"))))(Secondary((id \
         f5cebc84-ba6a-4044-a700-0134bc098556)(content(Comment\"#   \
         string_sub(str, pos, len) -> String          #\"))))(Secondary((id \
         c02b1937-2941-472e-aea8-659847b06fcc)(content(Whitespace\"\\n\"))))(Secondary((id \
         96cf3e0c-0232-4748-bb97-d430692ba04c)(content(Comment\"#   nth(list, \
         index) -> element                  #\"))))(Secondary((id \
         656cfdb2-9e72-437b-822e-8bfa22e25353)(content(Whitespace\"\\n\"))))(Secondary((id \
         c91840f1-97e7-4d13-b98b-852b93205d8a)(content(Comment\"#   \
         length(list) -> Int                          #\"))))(Secondary((id \
         22346dd6-cf56-4224-bc74-0c4eab66f2fe)(content(Whitespace\"\\n\"))))(Secondary((id \
         831c6493-d0f5-459e-87dd-8852744d0d8e)(content(Comment\"#   map(list, \
         fn) -> list                        #\"))))(Secondary((id \
         c0703dae-ea9e-443a-a950-2e1e089e406d)(content(Whitespace\"\\n\"))))(Secondary((id \
         cab50801-e32e-42b4-93bf-ab6b8194dcf5)(content(Comment\"#   \
         filter(list, pred) -> list                   #\"))))(Secondary((id \
         d8368fea-fad4-414d-8964-f97dd57675da)(content(Whitespace\"\\n\"))))(Secondary((id \
         50cfcc00-c080-441d-aa68-5fcf12a5bb2b)(content(Comment\"#                                                \
         #\"))))(Secondary((id \
         d3b08aaa-6197-46d5-86f3-a19b724ca7f2)(content(Whitespace\"\\n\"))))(Secondary((id \
         fc555f70-e3fb-45bc-92d3-62f4df73b634)(content(Comment\"# Function \
         syntax: fun param -> body             #\"))))(Secondary((id \
         b75400b6-9da4-4c90-b8bd-65eab3a0a325)(content(Whitespace\"\\n\"))))(Secondary((id \
         122c13bc-98a9-4416-b822-33ecf10e02a2)(content(Comment\"# Let binding: \
         let name = value in ...           #\"))))(Secondary((id \
         b9ea265d-5fc4-4b66-a4e0-694202511156)(content(Whitespace\"\\n\"))))(Secondary((id \
         cedaee91-500f-48ee-87d6-776b330acd5e)(content(Comment\"#                                                \
         #\"))))(Secondary((id \
         b6620df5-7a90-4532-94d0-636eccf1e287)(content(Whitespace\"\\n\"))))(Secondary((id \
         aec15ee4-e4c0-4f80-bbc7-b0517f09c5b2)(content(Comment\"# Tip: Turn on \
         auto-probe (microscope toggle)    #\"))))(Secondary((id \
         0afd932a-7f61-46d5-9aaf-936fb01d488e)(content(Whitespace\"\\n\"))))(Secondary((id \
         fdf08824-ad90-4c2f-bb37-3acbf0b1b4bc)(content(Comment\"# to see \
         intermediate values as you type.        #\"))))(Secondary((id \
         947b27d0-0232-426a-a8dc-e2c343f03eee)(content(Whitespace\"\\n\"))))(Secondary((id \
         9cb28c62-a1b3-4f06-969c-08cbb6775d40)(content(Whitespace\"\\n\"))))(Tile((id \
         40fb8ecc-085d-4c7b-87be-a5c7bd8b651a)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         83358891-77aa-43a1-a158-de5f86cebcc9)(content(Whitespace\" \
         \"))))(Tile((id \
         4ba2c550-a2a5-4200-86ee-6d9b143fd660)(label(grove_name))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         686f178d-1f14-4cc3-bcec-95f5a128403d)(content(Whitespace\" \
         \")))))((Secondary((id \
         e78b6fb6-700c-4f43-8382-6782e52fc591)(content(Whitespace\" \
         \"))))(Tile((id 5013483d-31e4-4ec2-b89d-81011203865a)(label(fun \
         ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 37))(sort Exp))))))(shards(0 \
         1))(children(((Secondary((id \
         093189bb-42bd-48c7-9fc8-603b6821e3ac)(content(Whitespace\" \
         \"))))(Tile((id \
         119a01be-83b2-4192-86c6-5d6a9e207cdd)(label(path))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         9c2d3423-4e3f-48d8-b741-0ea941f9a070)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         d940d656-b09e-431f-8c41-645352dae6cd)(content(Whitespace\"\\n\"))))(Tile((id \
         da73c166-1489-41bb-bf18-8fd6d5e78e53)(label(?))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         9811859e-013f-4715-b053-3e32f398f224)(content(Whitespace\"\\n\"))))(Secondary((id \
         f572a48d-e0aa-462a-8231-a47a44d9c83e)(content(Whitespace\"\\n\"))))(Secondary((id \
         2851c1ce-4099-4686-84fa-f125f81ca8b6)(content(Whitespace\"\\n\"))))(Secondary((id \
         88aed664-a4f2-4b48-bfa7-2a439a451832)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         e32214b9-0bb8-45e3-9fd0-7e31c5034794)(content(Whitespace\"\\n\"))))(Secondary((id \
         a55f83ac-fba8-497c-baa9-a93e040848bf)(content(Whitespace\"\\n\"))))(Tile((id \
         a95bba32-502f-4ed9-a70e-d02b3e0558c9)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         9ad90dcb-304c-48a1-b332-544c24d8df25)(content(Whitespace\"\\n\"))))(Tile((id \
         47f5b226-e6bd-4fb6-9d8d-e9a7ad937058)(label(grove_name))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         cb44ebe6-40aa-4874-b05f-e9dd637ba35e)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         eff1f57f-58d1-4fc7-9b65-2f7fc02c2d1c)(label(\"\\\"/moonlit-grove/ferns/watering\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         d3adbb10-2d9e-49ff-ad24-399efd568572)(content(Whitespace\"\\n\"))))(Tile((id \
         e38c984a-5f5d-42a7-80ba-3069fcb6d501)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         30fbce49-e29c-461a-a1b8-60f714f675f0)(content(Whitespace\" \
         \"))))(Tile((id \
         53fffe36-8b7d-47a6-bc74-7b2dc481d475)(label(\"\\\"moonlit-grove\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         6ee12e63-6f0a-4bc1-a6a6-26448cd1a386)(content(Whitespace\"\\n\")))))))))(Tile((id \
         ed661b35-eb94-439e-8706-af7f1eaf7f03)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         9ca2f6ec-0600-403a-88a1-5d480bca34fc)(content(Whitespace\"\\n\"))))(Secondary((id \
         945154c3-e3da-4b0f-992b-28d1896f33ae)(content(Whitespace\"\\n\"))))(Tile((id \
         eb5ccf73-e82c-43fe-bf39-d50d59206b8c)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         75a5236f-9064-4d8d-aa58-c39bc7e9e6ab)(content(Whitespace\"\\n\"))))(Tile((id \
         38f3c422-dc93-4b1f-a115-a3a113f23cb7)(label(grove_name))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         6b4865df-96e1-4b78-8eb1-722518dce199)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         c4b13641-3423-4a53-a161-0d6a5c55af29)(label(\"\\\"/night-garden/herbs/rosemary\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         3741ef50-750b-4e7c-ba23-c5ac057f51f6)(content(Whitespace\"\\n\"))))(Tile((id \
         1f38a222-6a75-4b3f-9308-2d2b001e7d8b)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         3cd36a6e-52a3-4f01-a2f2-e63e7f1a1e27)(content(Whitespace\" \
         \"))))(Tile((id \
         7fb2498c-f546-406a-9cd9-bb35b343964f)(label(\"\\\"night-garden\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         b9d27442-0f6c-4890-b7d9-29c36d395cd3)(content(Whitespace\"\\n\")))))))))(Tile((id \
         bd101671-21cc-4e8a-89b2-d995501cdfde)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         af54d8d7-df54-406a-b30d-0e50149651c7)(content(Whitespace\"\\n\"))))(Secondary((id \
         2f65b735-ff75-4a58-99a7-979039c8394d)(content(Whitespace\"\\n\"))))(Tile((id \
         57e3892e-c13b-4ad9-bf00-cecc3eff1b8b)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         0da198f7-6fa6-446f-9bbd-958a0bda8a7f)(content(Whitespace\"\\n\"))))(Tile((id \
         00abf13d-4399-4f4a-a81b-2edf7ac95a0f)(label(grove_name))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         8801ac11-80a5-4823-ac3e-b1dca2f3e688)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         a6138b4c-f0aa-4381-94cb-19dd7ce51e11)(label(\"\\\"/\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         2e6e405f-15e7-4777-90d5-46df8bdd88cd)(content(Whitespace\"\\n\"))))(Tile((id \
         7012ddaf-e086-4ae2-9f5e-6c03a2185e1a)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         bf2239d7-95fd-42ea-8447-d6c5535f8d20)(content(Whitespace\" \
         \"))))(Tile((id \
         17f427f6-7e68-48ac-afd5-7689cfb00437)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         19f36db2-08f6-4931-923a-0d3f1fd8ec8e)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         e426ac03-608d-4560-9751-5c32b9d1bea5)(content(Whitespace\"\\n\")))))";
      backup_text =
        "# GROVE PATH TASK                                #\n\
         #                                                #\n\
         # Implement grove_name: extract the first        #\n\
         # section from a garden path.                    #\n\
         #                                                #\n\
         # Examples:                                      #\n\
         #   grove_name(\"/moonlit-grove/ferns\") == \"moonlit-grove\"  #\n\
         #   grove_name(\"/night-garden/herbs/rosemary\") == \"night-garden\"  #\n\
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
