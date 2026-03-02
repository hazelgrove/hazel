let out : string * Haz3lcore.PersistentSegment.t =
  ( "Study / writing / running-sum / running-sum-sketch",
    {
      segment =
        "((Secondary((id \
         127e14fe-cc18-43d8-8389-2172d48802d8)(content(Comment\"# RUNNING SUM \
         TASK                             #\"))))(Secondary((id \
         3ba113f9-47ce-4f1e-bb0d-0bdfc689b9d4)(content(Whitespace\"\\n\"))))(Secondary((id \
         0edd3205-e0cd-4301-8bdb-bd0820bf2890)(content(Comment\"#                                              \
         #\"))))(Secondary((id \
         820f3710-d2de-4623-a8bc-3b653de109d9)(content(Whitespace\"\\n\"))))(Secondary((id \
         3a646e48-4ffa-4e4f-a292-a3f787523a27)(content(Comment\"# Implement \
         running_sum: compute a list where  #\"))))(Secondary((id \
         cbbe43f0-f3d6-48cd-8102-a33ec1dbba61)(content(Whitespace\"\\n\"))))(Secondary((id \
         1f8ee7ed-2238-4ea9-8a06-d862b1fda7bf)(content(Comment\"# each element \
         is the sum of all elements up   #\"))))(Secondary((id \
         c6de7a57-d459-42d4-b731-146115551f42)(content(Whitespace\"\\n\"))))(Secondary((id \
         b09a17a9-276d-4012-a3c6-342d75c5ce0f)(content(Comment\"# to and \
         including that position.              #\"))))(Secondary((id \
         0153a217-d56a-46c8-94ec-da065df96d99)(content(Whitespace\"\\n\"))))(Secondary((id \
         57b01e4a-5dd0-47ac-beba-2adfc8ceb006)(content(Comment\"#                                              \
         #\"))))(Secondary((id \
         e4de5ceb-40d6-4711-b312-9761402da4c7)(content(Whitespace\"\\n\"))))(Secondary((id \
         e0e80f4b-088a-49c8-8981-6b7878f4e20a)(content(Comment\"# \
         Examples:                                    #\"))))(Secondary((id \
         7b511435-6c42-488e-b225-5e824a0910b6)(content(Whitespace\"\\n\"))))(Secondary((id \
         2e164a79-286b-4725-b9b1-8b72d8588126)(content(Comment\"#   \
         running_sum([1, 2, 3]) == [1, 3, 6]        #\"))))(Secondary((id \
         37ef9d00-b738-4529-a538-b1859bd71760)(content(Whitespace\"\\n\"))))(Secondary((id \
         2d920284-b2c4-404b-916b-e81bec9bc5cf)(content(Comment\"#   \
         running_sum([5]) == [5]                    #\"))))(Secondary((id \
         e93b1278-6cf9-4437-9202-b399782e365d)(content(Whitespace\"\\n\"))))(Secondary((id \
         495e897e-fa08-49a3-b6fc-8fad10dcbfb0)(content(Comment\"#   \
         running_sum([]) == []                      #\"))))(Secondary((id \
         6a942636-6e54-4377-80d3-2fd1a31761e2)(content(Whitespace\"\\n\"))))(Secondary((id \
         94bf98a5-2b58-43a6-aa8b-1903311f655b)(content(Comment\"#                                              \
         #\"))))(Secondary((id \
         8cdb759b-a801-4c12-993d-b44549254244)(content(Whitespace\"\\n\"))))(Secondary((id \
         41c5727b-d4e6-4efe-b472-4615d38c2b40)(content(Comment\"# Available \
         functions:                         #\"))))(Secondary((id \
         94744ad1-595c-431d-b89e-2ba3798114b6)(content(Whitespace\"\\n\"))))(Secondary((id \
         1da4e86c-3ed3-4dfe-832a-57e8cbfd7b31)(content(Comment\"#   \
         fold_left(list, fn, init) -> result        #\"))))(Secondary((id \
         73adc835-d004-4223-8da5-abb6b52b9ba1)(content(Whitespace\"\\n\"))))(Secondary((id \
         a77d68b4-8369-4077-a50c-c7d11b12a914)(content(Comment\"#     fn takes \
         (accumulator, element)          #\"))))(Secondary((id \
         423561eb-ec4f-4970-8c7d-e518b9cb9a39)(content(Whitespace\"\\n\"))))(Secondary((id \
         84484483-464f-4fde-ac6f-6dc54f3e57a4)(content(Comment\"#   \
         append(list1, list2) -> list               #\"))))(Secondary((id \
         00d48012-0b58-47e7-86b1-c0a8516534c3)(content(Whitespace\"\\n\"))))(Secondary((id \
         c1f1aa34-c4a5-4a51-ad90-06d6b90306e7)(content(Comment\"#   rev(list) \
         -> list                          #\"))))(Secondary((id \
         c7adadd6-d7a9-44a5-9831-cd2b94e4e28b)(content(Whitespace\"\\n\"))))(Secondary((id \
         ae4c5998-476d-4d46-8a61-77dafc69cd18)(content(Comment\"#   map(list, \
         fn) -> list                      #\"))))(Secondary((id \
         1becd7f2-2c78-41f2-9648-375759260377)(content(Whitespace\"\\n\"))))(Secondary((id \
         5303d558-b5ed-40df-99a4-34755f43a3bc)(content(Comment\"#   \
         length(list) -> Int                        #\"))))(Secondary((id \
         1e34c4e9-e996-4ef5-9d1c-550b1bf50d64)(content(Whitespace\"\\n\"))))(Secondary((id \
         cefda5f1-daf5-4662-ab98-d4347e5c6789)(content(Comment\"#                                              \
         #\"))))(Secondary((id \
         17874aa5-bfde-4032-b027-22bc8780e6fb)(content(Whitespace\"\\n\"))))(Secondary((id \
         6efbb493-b884-458a-a982-a3aeb91c8aca)(content(Comment\"# Syntax \
         reminders:                            #\"))))(Secondary((id \
         6cb9a778-05cb-4edb-8273-b6e49508b1a8)(content(Whitespace\"\\n\"))))(Secondary((id \
         f5671b3a-df9c-4214-825a-be3cae5491f8)(content(Comment\"#   Tuple: (a, \
         b) = ...                        #\"))))(Secondary((id \
         5a00826d-b884-4cc5-9dfe-8af13ed8a571)(content(Whitespace\"\\n\"))))(Secondary((id \
         289715f4-5eb5-4087-a151-5da378f613ee)(content(Comment\"#   Tuple \
         access via pattern: let (x, y) = t   #\"))))(Secondary((id \
         8940b949-4043-4757-91a0-e7275c3d63a6)(content(Whitespace\"\\n\"))))(Secondary((id \
         a7e558e2-ebc5-4951-bcd0-04ea5cb7f063)(content(Comment\"#   List cons: \
         x::xs, List literal: [1, 2, 3]  #\"))))(Secondary((id \
         06265373-7a2f-4b0f-8500-484315ba7a24)(content(Whitespace\"\\n\"))))(Secondary((id \
         2405c005-4298-4ff2-a4fe-511b8848c8ed)(content(Comment\"#                                              \
         #\"))))(Secondary((id \
         d3731dfb-56e9-4897-a805-b1df3cdca2c9)(content(Whitespace\"\\n\"))))(Secondary((id \
         0e292ebb-44f7-4012-950e-a2d9aa153fc4)(content(Comment\"# Tip: You may \
         need to track both the running  #\"))))(Secondary((id \
         a8545b77-05cc-4d0c-b5eb-4f3ccbe65ded)(content(Whitespace\"\\n\"))))(Secondary((id \
         dcf2e818-3c45-4662-a18c-7af46f30ff8a)(content(Comment\"# total and \
         the result list in your fold.      #\"))))(Secondary((id \
         9a979cea-7e66-4a9b-babe-0845e8c9c064)(content(Whitespace\"\\n\"))))(Secondary((id \
         e9877c86-aa4a-4551-a370-a58bf9729d09)(content(Whitespace\"\\n\"))))(Tile((id \
         1aba835a-081f-426f-bf5a-0f8bcdda4397)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         dfc19011-648f-4bb8-80ca-e719be60a336)(content(Whitespace\" \
         \"))))(Tile((id \
         8ebecc87-9a65-430c-91a4-611136e583f2)(label(running_sum))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         00d3d92d-42b9-483b-b95b-7388e510ebc3)(content(Whitespace\" \
         \")))))((Secondary((id \
         7c24a0fd-27b6-4a64-a317-3f6f9bb77de8)(content(Whitespace\" \
         \"))))(Tile((id 4e2e1ad7-f898-4a85-b4a4-d42f0f01fe63)(label(fun \
         ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 37))(sort Exp))))))(shards(0 \
         1))(children(((Secondary((id \
         c1f0addf-2311-4abb-84ae-4bd20a9f710c)(content(Whitespace\" \
         \"))))(Tile((id \
         19efe63d-7512-4e86-9f55-342f2a41ade1)(label(nums))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         f2447791-a68b-44ba-8c00-df7d54fe14ae)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         d28b5136-ab0b-4605-b776-6a1f3863bc29)(content(Whitespace\"\\n\"))))(Tile((id \
         d5f8adf7-b829-47f9-8523-a7a4ac14f82e)(label(?))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         79db3754-4408-45b8-a912-49721df63c26)(content(Whitespace\"\\n\"))))(Secondary((id \
         9d127cc3-a07d-427a-8b7d-40c05086101c)(content(Whitespace\"\\n\"))))(Secondary((id \
         88703a3e-b8e4-4a09-9b28-c15a8efdd8b3)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         7170522d-4f16-4803-97c9-7da716fafc6e)(content(Whitespace\"\\n\"))))(Secondary((id \
         bbf84bec-20b1-45e6-963d-11a46f33a60a)(content(Whitespace\"\\n\"))))(Tile((id \
         6acbf52d-0883-4557-8782-94149ce50962)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         149f660c-196a-4262-8d81-0db6555fabca)(content(Whitespace\"\\n\"))))(Tile((id \
         63c5b1c1-2aef-46f1-809f-087d7009772b)(label(running_sum))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         140e5985-df19-4427-98b5-a7e6ec8a2a80)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         2cb7c06b-924e-4cc4-a7c4-39acaa661552)(label([ ]))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         90d782d7-035a-404f-858a-6a8dcd7d26d2)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         66ffa2f8-49b8-4bd8-b16d-f9927bee079f)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         de784f72-6232-46f9-96ce-036d0237c664)(content(Whitespace\" \
         \"))))(Tile((id \
         9bc87e47-321b-45a8-8ca0-feb8eddbf3fe)(label(2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         b32ac4f8-9adb-4d49-b41b-b39dcd789a6a)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         44cd9a53-2a0c-4800-8739-21a4b3277654)(content(Whitespace\" \
         \"))))(Tile((id \
         0e205d7f-73d9-4b19-ba55-04f43c02369c)(label(3))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))))))))))))(Secondary((id \
         082ecf29-91f8-48f7-a68b-712681eaf2e0)(content(Whitespace\"\\n\"))))(Tile((id \
         7eea3329-5e76-4ee2-85eb-21c249984580)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         55eaacfe-1e5e-4a4c-a696-c40a78e8d2ba)(content(Whitespace\" \
         \"))))(Tile((id a590116f-397f-415a-8e04-eb37dd7d12ae)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         d1fc3a35-3378-4c05-acd7-373368ac765c)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         a3b97d4b-45eb-4af3-8260-adb64d7dc1bb)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         9b31a636-636d-4780-a781-ed6f80d19fdc)(content(Whitespace\" \
         \"))))(Tile((id \
         b6b2978e-00aa-44c5-9407-2eff75214c9a)(label(3))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         a630bfb0-11da-49a1-8aab-17d04fe0b542)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         ee8f8677-0cff-4caa-9a9e-6cd478b32913)(content(Whitespace\" \
         \"))))(Tile((id \
         be5a6235-5717-4026-ba10-2e7d9d780c2e)(label(6))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         db97bad0-cc4d-4f91-bfe9-40b586aadfe3)(content(Whitespace\"\\n\")))))))))(Tile((id \
         6c73597c-6190-4d84-af0d-78bfa5f51d73)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         812f3a15-8af7-4164-b635-b28739f94393)(content(Whitespace\"\\n\"))))(Secondary((id \
         7874f7af-de61-4076-8971-ff9749843839)(content(Whitespace\"\\n\"))))(Tile((id \
         13535210-6715-45eb-b520-306a93a44842)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         37843e9e-f7bf-47db-b639-e9ad82fbff36)(content(Whitespace\"\\n\"))))(Tile((id \
         4868c6c5-9f88-4e7c-9153-07514cd17590)(label(running_sum))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         78f348a2-60b2-4f01-ac7a-c85132746995)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         36d34a14-2b3e-4c23-9b51-00e0eda39b69)(label([ ]))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         152a5233-687c-4555-bd15-b7f4e6d46f59)(label(5))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))))))))))))(Secondary((id \
         7e06f96e-40bd-4929-8448-780a8346912e)(content(Whitespace\"\\n\"))))(Tile((id \
         ab406bf9-fbbd-4716-b9b7-ac8699fd0e67)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         305e60c5-ee86-49db-8bf8-21f1f7a51d88)(content(Whitespace\" \
         \"))))(Tile((id 50f5020f-4a15-4a35-ba8c-27ad44ee358b)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         f0e6ccf6-f613-42c8-bf27-cfdf4a484fc9)(label(5))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         864e48b4-4091-4a8c-807e-3903587b4673)(content(Whitespace\"\\n\")))))))))(Tile((id \
         5d74ac3e-204f-485e-a2f2-8abd74ead927)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         cd58f0ae-72bb-40a8-8ccc-abecb3581787)(content(Whitespace\"\\n\"))))(Secondary((id \
         c436b7db-9f47-41bb-a385-aeffbcf5df1c)(content(Whitespace\"\\n\"))))(Tile((id \
         c3310a34-2e67-48f7-8ca2-8567cbe0bf04)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         9cc09f73-8a3b-4eb2-a579-82181a567277)(content(Whitespace\"\\n\"))))(Tile((id \
         2e98d9fd-896b-400a-8579-62e43623f2c2)(label(running_sum))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         28c66e63-69a1-4018-9a08-b75a8153d55d)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         0c1e18d8-7ed5-4e67-be9a-8c4554471b75)(label([]))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         6345be19-f5ae-405b-a176-783e3421087a)(content(Whitespace\"\\n\"))))(Tile((id \
         3e1d4397-44a6-40e3-b57e-b32b1f6307ed)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         ecdf5889-e926-434f-855b-042de865bc33)(content(Whitespace\" \
         \"))))(Tile((id \
         299b9e20-a366-4868-9680-52cb8c6ba6fa)(label([]))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         4775cc0c-81fe-4df5-86db-dbd9e13aae4a)(content(Whitespace\"\\n\")))))))))(Tile((id \
         be8a09b6-57a2-4004-a543-9d3966ae6b98)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         97e57df4-46e1-4768-be83-d2c412d30ae2)(content(Whitespace\"\\n\"))))(Secondary((id \
         cda88eb2-08ae-4b33-880b-295701e3a8f3)(content(Whitespace\"\\n\"))))(Tile((id \
         9917bda6-b946-4127-b7cd-2e6016cb9811)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         8c44e2e5-db21-40cf-9967-2736189b39d5)(content(Whitespace\"\\n\"))))(Tile((id \
         37fb5855-6370-4639-b894-ab644f10c499)(label(running_sum))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         68322cb3-fdd8-45fe-815a-115ca74a741d)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         b006c9ce-1a94-4da8-af6c-0f3a390a31b6)(label([ ]))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         7b45ba65-53d6-49b3-86ef-eb40e348647c)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         33525388-3a02-4979-a921-02d5c0ef3bc0)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         537475a1-f086-4f3e-96b5-f0a3f8629293)(content(Whitespace\" \
         \"))))(Tile((id \
         a04d587c-ab1c-429e-9684-8c5a851eb0fa)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         fbc98474-7328-47fe-97b7-7738dec1465d)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         14ec07e9-862b-4fe3-86c9-8bdfe80d0278)(content(Whitespace\" \
         \"))))(Tile((id \
         e254cb0c-e92a-4948-8ad3-da2ec82646a3)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         1ebea8f8-5722-4335-b2fb-7a04dc719e36)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         31ea39b7-18e8-4a31-9734-668f2f420551)(content(Whitespace\" \
         \"))))(Tile((id \
         e7cfc94e-4212-4bc5-9ef9-70ea2157abfc)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))))))))))))(Secondary((id \
         d4a674be-64b6-46b4-aa3a-9c87593ca666)(content(Whitespace\"\\n\"))))(Tile((id \
         43a49d17-face-41ce-995a-68959f499af4)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         41f193ce-a067-4eef-ba31-15fef15bed6c)(content(Whitespace\" \
         \"))))(Tile((id c811d631-a94e-4daa-bc66-3254d21273a8)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         f81ded6d-3a64-46f9-a231-f91a4935ad54)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         91a6c123-f1fa-44e6-9a15-6eaf1414dbee)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         b7eb9db1-ea8a-4e54-b3ae-6b34d6843eba)(content(Whitespace\" \
         \"))))(Tile((id \
         78b10aa2-1bf7-43ff-b2d5-ca935d6a8425)(label(2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         90c4006b-54f2-4f3a-8c4e-246b3ca67d45)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         246a71eb-1b93-4a4a-9ae1-3e94f52ed2e9)(content(Whitespace\" \
         \"))))(Tile((id \
         04ec178f-68a7-4d41-978d-894ce24b68e2)(label(3))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         7be84cb1-e1fa-4a4b-aa40-838ae17639c6)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         466f8d66-4010-4180-8efe-4f6f22b0d262)(content(Whitespace\" \
         \"))))(Tile((id \
         7a9faee5-f0c0-41c3-9f6e-e1ca4ffe45cc)(label(4))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         a36b18e6-a091-4341-9659-67ad9c211b17)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         0e44f618-5413-46ff-920a-7c1d494a444b)(content(Whitespace\"\\n\")))))";
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
         ?\n\n\n\
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
