let out : string * Haz3lcore.PersistentSegment.t =
  ( "Examples / study-write / running-sum / running-sum-sketch",
    {
      segment =
        "((Secondary((id \
         4abbcdf0-94a6-4a98-a796-b1e570d6aa84)(content(Comment\"# RUNNING SUM \
         TASK                             #\"))))(Secondary((id \
         9e7461b4-92af-47a0-93cf-855b7b563104)(content(Whitespace\"\\n\"))))(Secondary((id \
         f1099451-791a-4e5e-960f-e20f9de26b4b)(content(Comment\"#                                              \
         #\"))))(Secondary((id \
         2aedfe8d-be01-4d53-a4a5-187ce5d58bcc)(content(Whitespace\"\\n\"))))(Secondary((id \
         96aaf8a2-08e3-4168-b73f-d75adf50e12f)(content(Comment\"# Implement \
         running_sum: compute a list where  #\"))))(Secondary((id \
         bd53aee3-c108-4325-87d2-b86f7ea1908c)(content(Whitespace\"\\n\"))))(Secondary((id \
         8b306f89-d47e-4f50-a24e-0c94a76b7863)(content(Comment\"# each element \
         is the sum of all elements up   #\"))))(Secondary((id \
         2a2cef84-ed63-40ca-ade3-53559d3724be)(content(Whitespace\"\\n\"))))(Secondary((id \
         2d8a640a-3d4e-487b-8f09-becf26d6040b)(content(Comment\"# to and \
         including that position.              #\"))))(Secondary((id \
         26732dda-fcb4-4bc9-8614-426e49193e19)(content(Whitespace\"\\n\"))))(Secondary((id \
         b1706f21-eff9-4df4-8849-7222068eac9f)(content(Comment\"#                                              \
         #\"))))(Secondary((id \
         627c8335-a59d-4b59-8911-549d4389c1b7)(content(Whitespace\"\\n\"))))(Secondary((id \
         f8277f1b-9936-4e55-a477-c77b6c678a29)(content(Comment\"# \
         Examples:                                    #\"))))(Secondary((id \
         6baccc2a-bb96-4858-9004-0917a04e2ef3)(content(Whitespace\"\\n\"))))(Secondary((id \
         23839021-c846-4fe6-96e9-af8b62bd95ef)(content(Comment\"#   \
         running_sum([1, 2, 3]) == [1, 3, 6]        #\"))))(Secondary((id \
         fa41af06-a2c0-4d9d-9da7-badcbed76496)(content(Whitespace\"\\n\"))))(Secondary((id \
         78895411-49cd-4519-9282-243eca4f3912)(content(Comment\"#   \
         running_sum([5]) == [5]                    #\"))))(Secondary((id \
         6b6ad930-0b13-48d3-aaa8-14b8454d9ac3)(content(Whitespace\"\\n\"))))(Secondary((id \
         5d96b327-9348-4463-bba7-3ef498ab3b3c)(content(Comment\"#   \
         running_sum([]) == []                      #\"))))(Secondary((id \
         40b7a639-094e-40ed-a964-fb3c7d8f3e48)(content(Whitespace\"\\n\"))))(Secondary((id \
         c635beba-0044-4b8e-8fe2-a3b1907fec09)(content(Comment\"#                                              \
         #\"))))(Secondary((id \
         bce40d96-82f8-4190-976e-b2b641ec7815)(content(Whitespace\"\\n\"))))(Secondary((id \
         76a6b1f3-ca6d-4d8a-8d96-506a09bd22af)(content(Comment\"# Available \
         functions:                         #\"))))(Secondary((id \
         ab6398d2-bf36-4604-b512-99c90ddcff3e)(content(Whitespace\"\\n\"))))(Secondary((id \
         12576649-6c7e-4dee-afe3-816af261d5fb)(content(Comment\"#   \
         fold_left(list, fn, init) -> result        #\"))))(Secondary((id \
         a6c9ee14-a289-4817-862b-58f6b44081fb)(content(Whitespace\"\\n\"))))(Secondary((id \
         6726414e-fbd1-49cb-a352-00285882fb8a)(content(Comment\"#     fn takes \
         (accumulator, element)          #\"))))(Secondary((id \
         1b67bedf-42c9-454f-8968-711617935949)(content(Whitespace\"\\n\"))))(Secondary((id \
         478cea65-95a0-4b96-a253-4f40d70f6b83)(content(Comment\"#   \
         append(list1, list2) -> list               #\"))))(Secondary((id \
         4c8b4cbc-c0dc-4805-bacc-0e1f4792c650)(content(Whitespace\"\\n\"))))(Secondary((id \
         7e0c6714-98fe-4d7f-8515-b1601ff09f58)(content(Comment\"#   rev(list) \
         -> list                          #\"))))(Secondary((id \
         bc454bdd-afaa-438a-ab5e-78136c41efe2)(content(Whitespace\"\\n\"))))(Secondary((id \
         bc2f4cec-2bbc-45e7-981f-0b8020ad5821)(content(Comment\"#   map(list, \
         fn) -> list                      #\"))))(Secondary((id \
         f9656498-457f-484d-99a2-0a3c150a9390)(content(Whitespace\"\\n\"))))(Secondary((id \
         b4b50822-b884-4c86-a984-51a230499493)(content(Comment\"#   \
         length(list) -> Int                        #\"))))(Secondary((id \
         5ef1d2de-b941-427a-840f-8c3884460383)(content(Whitespace\"\\n\"))))(Secondary((id \
         c22173ed-020d-4e05-9bfe-2543d409c888)(content(Comment\"#                                              \
         #\"))))(Secondary((id \
         1433e70e-1f6e-476f-a25d-6e996ebc0c1b)(content(Whitespace\"\\n\"))))(Secondary((id \
         01165d01-44be-4fdb-93de-40c7e87e1290)(content(Comment\"# Syntax \
         reminders:                            #\"))))(Secondary((id \
         a83a2703-e083-4a52-b204-3542fe6014fd)(content(Whitespace\"\\n\"))))(Secondary((id \
         ebe945c6-8598-4e16-b591-b973603d7298)(content(Comment\"#   Tuple: (a, \
         b) = ...                        #\"))))(Secondary((id \
         45eddf7e-5755-4981-b77c-37c88d3ce961)(content(Whitespace\"\\n\"))))(Secondary((id \
         6b395495-cee2-46d4-b507-3f9992d125c3)(content(Comment\"#   Tuple \
         access via pattern: let (x, y) = t   #\"))))(Secondary((id \
         a345481b-b194-45ea-a971-5621f8c99ee7)(content(Whitespace\"\\n\"))))(Secondary((id \
         8f6c3fe0-7eb9-4165-ba46-c38c6043914d)(content(Comment\"#   List cons: \
         x::xs, List literal: [1, 2, 3]  #\"))))(Secondary((id \
         ddca39ae-7916-4d35-8625-c01d959cc8eb)(content(Whitespace\"\\n\"))))(Secondary((id \
         6ec88aee-aeab-4299-9f8e-f87c7a896ffa)(content(Comment\"#                                              \
         #\"))))(Secondary((id \
         dc09d1c7-079a-4013-9c21-698015fb1488)(content(Whitespace\"\\n\"))))(Secondary((id \
         2a2e7695-637a-4254-9234-6ce489d639b4)(content(Comment\"# Tip: You may \
         need to track both the running  #\"))))(Secondary((id \
         7551175b-a9fd-44cf-8bc3-f6f78d90a2a4)(content(Whitespace\"\\n\"))))(Secondary((id \
         26f16eed-b9f5-4671-9a6f-2e336cad2aeb)(content(Comment\"# total and \
         the result list in your fold.      #\"))))(Secondary((id \
         7fe3727e-93fa-47db-8622-a09755ca4e98)(content(Whitespace\"\\n\"))))(Secondary((id \
         edd9a005-c4ef-471c-b406-bb9aaa794edd)(content(Whitespace\"\\n\"))))(Tile((id \
         630f1d87-30af-4253-a86f-43b9bf89faf0)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         40))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         2b04c683-0500-485a-9138-87768f6a8c11)(content(Whitespace\" \
         \"))))(Tile((id \
         bc4572c7-7f07-41e5-95f4-6bf5404208a0)(label(running_sum))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         e8e63f5b-95e7-4acf-9c1c-2b2fbc9bf061)(content(Whitespace\" \
         \")))))((Secondary((id \
         f5c0757d-f033-47ae-bd3e-bd542b652625)(content(Whitespace\" \
         \"))))(Tile((id ddb9c328-2715-4f5c-9152-7caa330e370d)(label(fun \
         ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 36))(sort Exp))))))(shards(0 \
         1))(children(((Secondary((id \
         d587b3b8-716e-4e7a-b610-a95e1a4d4edb)(content(Whitespace\" \
         \"))))(Tile((id \
         76ebb5f0-8ebe-4c69-8f8e-d1e4284f29bd)(label(nums))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         007891bd-bad8-4c66-8627-877e356453a5)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         59904e9a-0ea8-4d5c-a40b-6c0e4a4f751f)(content(Whitespace\"\\n\"))))(Tile((id \
         a0393213-d905-47dc-acd6-24003d8f7c93)(label(?))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         656f6bc8-1fb6-427d-b41c-89d9f584ea80)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         d366d684-06c8-475d-b2e4-9a5bf2dd73ed)(content(Whitespace\"\\n\"))))(Secondary((id \
         ca2d75a8-5a65-4dd4-b6f0-f0e291ec8c25)(content(Whitespace\"\\n\"))))(Tile((id \
         086753e8-add2-4c43-8405-f2cfa2ac5b91)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         56b902d9-7d57-4505-a180-78dd21d6b49d)(content(Whitespace\"\\n\"))))(Tile((id \
         9c4000ff-f292-4d28-b306-8ffe7cbb97ae)(label(running_sum))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         726e5e8d-0d33-4ab1-afd8-c3ad0c713715)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         7f07452c-ace8-46a8-b017-a64e331db28d)(label([ ]))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         e9c10c97-cf01-4962-b1ab-43755e62235c)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         e4a3b768-532d-4ef4-a77b-9f7904dc1631)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         d40591b9-013f-4c0f-baeb-04d292be12c3)(content(Whitespace\" \
         \"))))(Tile((id \
         990fa496-0a35-44f4-aa66-06e3adbc929e)(label(2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         1f808ace-ed00-41a4-8dbb-8770d4d86d78)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         cb8e28ca-a81e-48ed-a64c-a6b8fe84c2ad)(content(Whitespace\" \
         \"))))(Tile((id \
         7e4adc74-c95c-4968-ad49-1230d274fb43)(label(3))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))))))))))))(Secondary((id \
         6ec637ca-a555-4bdb-95e0-7e1fcc48b163)(content(Whitespace\"\\n\"))))(Tile((id \
         2cd38d6e-4bdc-4841-b5f7-5b3e3811cb39)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         f3350445-9f02-48eb-81ff-f7fbc4bae7e8)(content(Whitespace\" \
         \"))))(Tile((id 7acfe833-c2bc-485b-ad86-5e8514b4bede)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         141d5b04-b9b0-4eae-8fb4-7f13dcb66c1c)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         9c3e86da-3c9c-4c9c-a3b7-eddb0ef752cd)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         6dca9716-088d-4a5a-9139-6c5c8086de96)(content(Whitespace\" \
         \"))))(Tile((id \
         08aaf5db-45c5-4414-b681-fadfd9cf464a)(label(3))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         8b2f5bfe-83ee-47af-bb06-7885360e31f0)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         fc66842c-d517-420f-82f2-dee996837626)(content(Whitespace\" \
         \"))))(Tile((id \
         1d042ab3-38dd-4581-8557-1f136c30037d)(label(6))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         cd492f45-8f9f-4f45-b8a2-a647c757e985)(content(Whitespace\"\\n\")))))))))(Tile((id \
         fd5ef251-e16d-4bc7-829a-75e3a5eba849)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 38))(sort Exp))((shape(Concave \
         38))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         95132037-a87a-4170-a204-89807d7fb723)(content(Whitespace\"\\n\"))))(Secondary((id \
         18a9497f-6f3c-4285-b04c-b1300b835520)(content(Whitespace\"\\n\"))))(Tile((id \
         1b667e92-7896-4804-ba40-a735e76b1c64)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         1983ff28-7abe-4bfb-bc12-f704147817f9)(content(Whitespace\"\\n\"))))(Tile((id \
         743c331e-a8d9-45a1-9b29-568d10919ee6)(label(running_sum))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         7a3d755d-688f-4833-9af9-c36cb985ff9e)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         e98565ac-d01f-46d5-a9bd-b33a2d3c0e3d)(label([ ]))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         41190779-5cb6-41af-88e9-6c0e2cef8436)(label(5))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))))))))))))(Secondary((id \
         e2875411-5291-4c03-ba69-ea9c2fb6cafc)(content(Whitespace\"\\n\"))))(Tile((id \
         03b95f69-2bac-481d-a8d8-368388a297c5)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         e3b43e2b-0b5c-4bc2-8302-21ba6f7ac98b)(content(Whitespace\" \
         \"))))(Tile((id fb4516e1-ff80-4820-ac3c-7029299ca2e1)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         237d2bd1-6ce7-4124-925a-d6f44c4f18b6)(label(5))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         ff1a6dfa-9029-4848-8291-a6bca6e04b8b)(content(Whitespace\"\\n\")))))))))(Tile((id \
         027fdd87-5e7e-4dd5-afbd-97975c57ab54)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 38))(sort Exp))((shape(Concave \
         38))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         357a5b4e-7864-4f29-af6e-67ccd6ffab18)(content(Whitespace\"\\n\"))))(Secondary((id \
         70deb93d-8479-457c-b212-7fe8503b756e)(content(Whitespace\"\\n\"))))(Tile((id \
         a856d1e7-e1f9-4e61-9329-3c8ac2567486)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         3dcda9e2-556c-4fb1-b111-0dc0a098bb74)(content(Whitespace\"\\n\"))))(Tile((id \
         449022f4-94ff-40f3-85eb-2e2aa4863e20)(label(running_sum))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         26eadf13-0943-48b1-b1f6-2d520f94a1ad)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         20de9440-9f9b-4360-acc9-a3a86c77f278)(label([]))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         5fbc32e1-7b19-4ba2-845b-e88ddcfe8ac7)(content(Whitespace\"\\n\"))))(Tile((id \
         5706de41-44f4-42b1-8899-c017562e8bc6)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         0926365c-74d8-4665-9cf7-4efb9878b001)(content(Whitespace\" \
         \"))))(Tile((id \
         281e6497-8638-41d7-99b4-6eab59464f61)(label([]))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         e2731aee-d8fa-4e5d-99b0-55d86d1e6f92)(content(Whitespace\"\\n\")))))))))(Tile((id \
         a76c2bec-31aa-4600-9679-3e9054537225)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 38))(sort Exp))((shape(Concave \
         38))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         5f9fefe5-97a0-4e2e-b3c2-20ba493408b3)(content(Whitespace\"\\n\"))))(Secondary((id \
         a060fe8e-9cd4-4bf2-bdb1-e9578f5a4118)(content(Whitespace\"\\n\"))))(Tile((id \
         4204776a-7ae8-4ea2-8f03-7b03248df098)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         d50a72d2-1817-49f4-bcab-88b5412521cf)(content(Whitespace\"\\n\"))))(Tile((id \
         bd019ec2-6991-401e-a885-c09fba949a1e)(label(running_sum))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         69d63503-6780-4345-9e1f-b902563e9cfd)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         5780f7f3-03f1-4bd9-89b7-05d5135824d8)(label([ ]))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         1b8100c0-3bef-4d9e-b11e-3d81c1e63938)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         eeea51ee-8353-41db-9d63-fbfacbc6e549)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         039b1532-c491-4b8a-87d4-644fb57f4560)(content(Whitespace\" \
         \"))))(Tile((id \
         fb3fa3b6-8697-440c-94ca-e48c2c52bc7b)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         718fead2-0e6a-4879-a13b-0bc57edd0386)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         88ba7167-f07e-4d05-b5b3-647359de6fb3)(content(Whitespace\" \
         \"))))(Tile((id \
         ff5805d6-415b-4308-b88e-04c5c81a8165)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         575c5ae6-2c30-491c-b3d3-f48c53f16dd1)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         bdaa1fc5-a82e-469f-ac9f-843d229de577)(content(Whitespace\" \
         \"))))(Tile((id \
         664f9dea-783d-4105-9e5b-1eeac0af1799)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))))))))))))(Secondary((id \
         a78eb47a-f886-4754-8549-493756a19701)(content(Whitespace\"\\n\"))))(Tile((id \
         702c308b-b59c-4708-bf3e-a033a92950e1)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         772cbaac-88f3-4c8a-b184-c12a3156a166)(content(Whitespace\" \
         \"))))(Tile((id 5e88fcf4-1092-4a2e-a7f6-bc8cb4244575)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         d0c4f67a-6653-47d3-a96a-5515bbf9c7d1)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         33b40cb7-7c3c-4efd-8e83-23d22f9b76b1)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         3d620831-72b1-4504-b3ca-665ed9a738d0)(content(Whitespace\" \
         \"))))(Tile((id \
         a62dd8a0-a352-4c29-abd0-f9ca11bfa154)(label(2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         5d9f5407-b6e6-44ff-8657-f2d3a1bfdccd)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         a47ca755-5db6-4ac9-9870-57316909a372)(content(Whitespace\" \
         \"))))(Tile((id \
         f7a1a064-895f-406b-8131-be973fddb3eb)(label(3))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         8a935916-eef0-45fc-9592-f66a21a3d897)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         673d1ebc-9fc9-4afe-9426-cde7db2aed4b)(content(Whitespace\" \
         \"))))(Tile((id \
         f938208e-b813-4e73-83d4-cf2b7445f1ac)(label(4))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         0c82ef86-9763-4abf-815c-923c654c79e7)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         027eb6f5-613a-4ea4-901c-ba853f284e80)(content(Whitespace\"\\n\")))))";
      backup_text =
        "# RUNNING SUM TASK                             #\n\
         #                                              #\n\
         # Implement running_sum: compute a list where  #\n\
         # each element is the sum of all elements up   #\n\
         # to and including that position.              #\n\
         #                                              #\n\
         # Examples:                                    #\n\
         #   running_sum([1, 2, 3]) == [1, 3, 6]        #\n\
         #   running_sum([5]) == [5]                    #\n\
         #   running_sum([]) == []                      #\n\
         #                                              #\n\
         # Available functions:                         #\n\
         #   fold_left(list, fn, init) -> result        #\n\
         #     fn takes (accumulator, element)          #\n\
         #   append(list1, list2) -> list               #\n\
         #   rev(list) -> list                          #\n\
         #   map(list, fn) -> list                      #\n\
         #   length(list) -> Int                        #\n\
         #                                              #\n\
         # Syntax reminders:                            #\n\
         #   Tuple: (a, b) = ...                        #\n\
         #   Tuple access via pattern: let (x, y) = t   #\n\
         #   List cons: x::xs, List literal: [1, 2, 3]  #\n\
         #                                              #\n\
         # Tip: You may need to track both the running  #\n\
         # total and the result list in your fold.      #\n\n\
         let running_sum = fun nums ->\n\
         ?\n\
         in\n\n\
         test\n\
         running_sum([1, 2, 3])\n\
         == [1, 3, 6]\n\
         end;\n\n\
         test\n\
         running_sum([5])\n\
         == [5]\n\
         end;\n\n\
         test\n\
         running_sum([])\n\
         == []\n\
         end;\n\n\
         test\n\
         running_sum([1, 1, 1, 1])\n\
         == [1, 2, 3, 4]\n\
         end\n";
      refractors = "()";
    } )
