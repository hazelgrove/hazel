let out : string * Haz3lcore.PersistentSegment.t =
  ( "Study / writing / running-sum / running-sum-sketch",
    {
      segment =
        "((Secondary((id \
         725ae4fd-4765-4e70-ada6-e673efa08c47)(content(Comment\"# RUNNING SUM \
         TASK                             #\"))))(Secondary((id \
         540f8daa-07c6-4b92-b1a5-c23e87aabb19)(content(Whitespace\"\\n\"))))(Secondary((id \
         5ac18395-d68d-4310-a376-27fbfaedb837)(content(Comment\"#                                              \
         #\"))))(Secondary((id \
         d1c0d9b3-df8f-464f-b3af-bea3a9523199)(content(Whitespace\"\\n\"))))(Secondary((id \
         dbbaaf8d-24b0-4dbc-8d94-d79120e59783)(content(Comment\"# Implement \
         running_sum: compute a list where  #\"))))(Secondary((id \
         4cb9e705-a268-461b-afa7-48eabc52946f)(content(Whitespace\"\\n\"))))(Secondary((id \
         1381a060-bcb9-417f-ab9f-107752b17aac)(content(Comment\"# each element \
         is the sum of all elements up   #\"))))(Secondary((id \
         127bd96b-59f1-4b9b-bdc2-1c14b73d6a6d)(content(Whitespace\"\\n\"))))(Secondary((id \
         4fef83b5-9c5a-4c62-90df-1e0267146bf7)(content(Comment\"# to and \
         including that position.              #\"))))(Secondary((id \
         391d694a-f6f2-425c-884e-a0cb1b6bac36)(content(Whitespace\"\\n\"))))(Secondary((id \
         a7e6a30e-472a-4e4c-acfa-ff95fc89f1fb)(content(Comment\"#                                              \
         #\"))))(Secondary((id \
         f8b0fdfc-0ce1-414e-b5b3-3b12a66e19d9)(content(Whitespace\"\\n\"))))(Secondary((id \
         de6e9d2a-68f5-4538-a9c9-18ecd217de1a)(content(Comment\"# \
         Examples:                                    #\"))))(Secondary((id \
         84ea365e-a7e2-4567-bd24-89c794705dfe)(content(Whitespace\"\\n\"))))(Secondary((id \
         e6fba27f-9790-45e8-ab3f-a8b471c34b86)(content(Comment\"#   \
         running_sum([1, 2, 3]) == [1, 3, 6]        #\"))))(Secondary((id \
         f961a428-42fa-4a5e-812d-5b350250f480)(content(Whitespace\"\\n\"))))(Secondary((id \
         1fc35f1e-499b-4696-a415-749db775229f)(content(Comment\"#   \
         running_sum([5]) == [5]                    #\"))))(Secondary((id \
         663c7f3f-e3f5-40e4-96d8-0b7794d947cf)(content(Whitespace\"\\n\"))))(Secondary((id \
         9553a096-965b-400a-b430-8aa1ef9fbb08)(content(Comment\"#   \
         running_sum([]) == []                      #\"))))(Secondary((id \
         26e87a34-17a6-42d8-a07f-c34b0811f235)(content(Whitespace\"\\n\"))))(Secondary((id \
         fe24c9fb-f498-42c3-9ae1-645b3db2a3b2)(content(Comment\"#                                              \
         #\"))))(Secondary((id \
         d2f87d48-62f4-4af6-adcc-b76cbea8823d)(content(Whitespace\"\\n\"))))(Secondary((id \
         d39be452-cf69-4909-a13a-dcc297ae64af)(content(Comment\"# Available \
         functions:                         #\"))))(Secondary((id \
         adc925fb-3173-40cd-a135-3d6b672ae640)(content(Whitespace\"\\n\"))))(Secondary((id \
         f8f2eea7-1f49-4e2d-ad1d-39d988d3f248)(content(Comment\"#   \
         fold_left(list, fn, init) -> result        #\"))))(Secondary((id \
         2b24dda3-910b-4fee-a524-3b633dce644a)(content(Whitespace\"\\n\"))))(Secondary((id \
         1649a0d0-86ef-4c91-9c4c-d52d2f341b6f)(content(Comment\"#     fn takes \
         (accumulator, element)          #\"))))(Secondary((id \
         cd8e9e5b-ea36-4d9a-8664-5538de50d525)(content(Whitespace\"\\n\"))))(Secondary((id \
         37781a3f-9891-48bb-99b1-5ce12b77903f)(content(Comment\"#   \
         append(list1, list2) -> list               #\"))))(Secondary((id \
         6d51db67-7c3e-4b3c-bef4-9e0660043aa1)(content(Whitespace\"\\n\"))))(Secondary((id \
         5e4055f4-c365-4b65-bc11-ea4c8214e72c)(content(Comment\"#   rev(list) \
         -> list                          #\"))))(Secondary((id \
         88117371-6c5a-4590-b68c-568ec9366da1)(content(Whitespace\"\\n\"))))(Secondary((id \
         0ef90134-a67e-4890-acd8-074423b9b8d3)(content(Comment\"#   map(list, \
         fn) -> list                      #\"))))(Secondary((id \
         b198a6da-4ca3-459c-b4b6-c58d9761b345)(content(Whitespace\"\\n\"))))(Secondary((id \
         87a779fe-5544-4a80-a3ea-95b0e598f35a)(content(Comment\"#   \
         length(list) -> Int                        #\"))))(Secondary((id \
         0de8edc1-36a3-4374-87a6-ae9ac7612542)(content(Whitespace\"\\n\"))))(Secondary((id \
         b96401b5-ca45-4e29-b989-67229e0dafa8)(content(Comment\"#                                              \
         #\"))))(Secondary((id \
         35160ad2-84c3-4c9d-97b4-f55bb74407a8)(content(Whitespace\"\\n\"))))(Secondary((id \
         6dddc03f-7482-4620-b507-ccdd9385b874)(content(Comment\"# Syntax \
         reminders:                            #\"))))(Secondary((id \
         65420bc2-f3fd-412f-9784-79d25f85cdf7)(content(Whitespace\"\\n\"))))(Secondary((id \
         96cc4fb0-f792-4c1d-a3d7-269360b7779c)(content(Comment\"#   Tuple: (a, \
         b) = ...                        #\"))))(Secondary((id \
         8952be74-8541-4702-9fec-1fcf0efacedc)(content(Whitespace\"\\n\"))))(Secondary((id \
         55833518-eeda-4e5f-9dbb-bbd8444f6067)(content(Comment\"#   Tuple \
         access via pattern: let (x, y) = t   #\"))))(Secondary((id \
         2a01ba40-2855-4f80-970d-78195f4a74d2)(content(Whitespace\"\\n\"))))(Secondary((id \
         79dfa012-19e7-4aae-940b-5bcf642392af)(content(Comment\"#   List cons: \
         x::xs, List literal: [1, 2, 3]  #\"))))(Secondary((id \
         a156de46-3785-473b-b70e-16833121ad6c)(content(Whitespace\"\\n\"))))(Secondary((id \
         e63b969c-ff14-44cc-8882-0be0dee20504)(content(Comment\"#                                              \
         #\"))))(Secondary((id \
         25f69fef-460a-4d05-81c2-25b72d93466b)(content(Whitespace\"\\n\"))))(Secondary((id \
         199d30da-7285-407f-ae89-38a9ffa7a73a)(content(Comment\"# Tip: You may \
         need to track both the running  #\"))))(Secondary((id \
         8e086f10-f7dc-4efe-b789-1d3b6ae820c9)(content(Whitespace\"\\n\"))))(Secondary((id \
         a08db235-28b3-4ec2-97ea-b64194517b5c)(content(Comment\"# total and \
         the result list in your fold.      #\"))))(Secondary((id \
         9e499f15-7b8b-43a2-a253-b2bf21993e97)(content(Whitespace\"\\n\"))))(Secondary((id \
         4945d48f-6de5-45f5-a795-f3f8af3777e2)(content(Whitespace\"\\n\"))))(Tile((id \
         98575669-9112-4705-8abe-5e383550f2dc)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         e32e615a-daaf-40a5-8710-05387c9ee590)(content(Whitespace\" \
         \"))))(Tile((id \
         4bfc438d-a1cc-4bd6-8ff0-17839988db2b)(label(running_sum))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         ef498535-884f-4949-9ce5-eb6f97e7bf7a)(content(Whitespace\" \
         \")))))((Secondary((id \
         c6d16871-c56e-47ff-921b-e16cf5cb64a1)(content(Whitespace\" \
         \"))))(Tile((id 9d37df4b-9a17-4468-90c3-af9c10a141d7)(label(fun \
         ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 37))(sort Exp))))))(shards(0 \
         1))(children(((Secondary((id \
         e3fa5812-29df-4415-a23e-290298fc496c)(content(Whitespace\" \
         \"))))(Tile((id \
         439c5aa1-0843-4507-b73f-5d37aa4dc6d8)(label(nums))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         da5c1504-7815-46b7-adde-c65748d935dc)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         4f82c2f8-a150-4c56-9eed-acc2c0959291)(content(Whitespace\"\\n\"))))(Tile((id \
         c691bd90-8167-4b6b-ab6d-b48233465e07)(label(?))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         edda3217-4950-444c-9acc-d354ac8c59e9)(content(Whitespace\"\\n\"))))(Secondary((id \
         472c7574-f7b1-417d-9fb8-69e90c9512be)(content(Whitespace\"\\n\"))))(Secondary((id \
         a74edfc9-d4f0-4489-b580-adccc5a132e8)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         9a8b2e91-4dda-4690-85e0-85fce8250fed)(content(Whitespace\"\\n\"))))(Secondary((id \
         aa4a03d8-607f-4b26-8d91-e5700b924321)(content(Whitespace\"\\n\"))))(Tile((id \
         6bdb1b59-0aad-4be4-b4d7-ab1c3e4e6d76)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         1bd75bfd-2f3c-4b86-be5a-6ff833ca776d)(content(Whitespace\"\\n\"))))(Tile((id \
         c4c1b7cc-defc-42e8-8376-3f3685bfd82a)(label(running_sum))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         2ccd6453-5fbe-46ab-9457-5dcb583d7ae0)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         a269f7e3-3e13-4dae-90ce-d6d779adc380)(label([ ]))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         f8a91f81-e0ea-47e5-9c0b-a891635d6bb0)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         e1bfb226-f54e-4185-9794-3999041e2a6e)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         fb2be0d6-f046-467b-9d72-cf0db5d4f94e)(content(Whitespace\" \
         \"))))(Tile((id \
         9807ad08-be61-4b65-bb35-07b91d78c331)(label(2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         86128021-ade8-4610-b044-156fa1179e09)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         22f383ed-8e85-4b7a-a038-b115f527adda)(content(Whitespace\" \
         \"))))(Tile((id \
         43f302af-d195-4bc6-b093-617189cb485b)(label(3))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))))))))))))(Secondary((id \
         55edd3ca-802a-45bd-bd32-3c36d997d51e)(content(Whitespace\"\\n\"))))(Tile((id \
         1a89eb1f-6f2e-48a4-9f89-c3b0a1bbf54a)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         296b17d1-31b8-4f1e-8f3a-c896146f6e5d)(content(Whitespace\" \
         \"))))(Tile((id 651aa9cd-d6d7-4833-8ac5-abc8da517b66)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         a35e0c80-b7f3-4d36-b5ee-6ca59e53d64f)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         2b49574f-5e08-4323-8b70-86993ceb0030)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         33e0c6ff-ec81-48c9-a23a-81ea5658272c)(content(Whitespace\" \
         \"))))(Tile((id \
         2b4fd59a-3d9f-4638-8ca0-0916474ee240)(label(3))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         d806f9fd-6eb2-47fa-a36b-0b780ea17312)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         fed78f88-d35d-43de-b72b-464f6d840ba0)(content(Whitespace\" \
         \"))))(Tile((id \
         506d2bfc-7bbe-4a88-ae79-2ac073c96372)(label(6))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         aac1d3c8-ac82-447e-b056-35733ebbe6fa)(content(Whitespace\"\\n\")))))))))(Tile((id \
         e29f477d-ddc0-4ab2-a433-1dc35b592667)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         2323c0d5-7616-44b7-a691-57058b32dec5)(content(Whitespace\"\\n\"))))(Secondary((id \
         66cab982-7843-4097-8580-60a7ab3c7159)(content(Whitespace\"\\n\"))))(Tile((id \
         7378a5a1-51ed-407b-bf82-2fb452f18512)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         3356b38a-9299-4880-ab20-48be46ac4b13)(content(Whitespace\"\\n\"))))(Tile((id \
         97ed2952-0bba-492d-a392-faa37a903263)(label(running_sum))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         174a383f-0f80-4d0b-aecb-73f3f954ca5e)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         49ae3844-8053-4150-a4ac-d92f20be747b)(label([ ]))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         f41a6e61-6c17-4c97-89be-d4188d240d85)(label(5))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))))))))))))(Secondary((id \
         c8df8967-5aff-4a3b-adce-81a2f872e815)(content(Whitespace\"\\n\"))))(Tile((id \
         882ca083-1618-46f1-bf9b-ddb72a1d9a7c)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         7cc9f7e9-7923-404b-a0ff-6b61f454b99c)(content(Whitespace\" \
         \"))))(Tile((id de6c3fde-9822-4d87-92f3-7273939d3e25)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         ed016c1e-97e3-4dca-a815-9b71a70ec46b)(label(5))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         baba331a-d5e7-4f21-a50f-797274490940)(content(Whitespace\"\\n\")))))))))(Tile((id \
         d7a3f7d1-a0a7-4493-bf4c-b5912370467b)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         aed2844e-c36c-406d-9715-fc31346aac4d)(content(Whitespace\"\\n\"))))(Secondary((id \
         966fb8a9-64ea-4f0f-9632-957652f82775)(content(Whitespace\"\\n\"))))(Tile((id \
         d64de472-c491-45e2-b834-9a1be9b5efe3)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         da55918c-ee1b-43f9-a9cc-52b43be138aa)(content(Whitespace\"\\n\"))))(Tile((id \
         563397ce-94de-4878-a003-592fd0fa75dc)(label(running_sum))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         525a28ab-7a2b-41c7-b782-34f11a32fb0b)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         003f5d29-4551-403c-94ce-9b8168d47ba1)(label([]))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         33a6fad4-c360-43d7-b264-ad4dd18cb52a)(content(Whitespace\"\\n\"))))(Tile((id \
         1ccb4b92-a527-4cd7-8d6b-bafa8d2c62fa)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         99ba8bcf-779f-435f-8a88-7f7a5489331d)(content(Whitespace\" \
         \"))))(Tile((id \
         ebed1d16-137b-43dc-a7b4-f83c010fa9f8)(label([]))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         e6cfb2ff-0520-4b4d-bfda-1112867ff1ac)(content(Whitespace\"\\n\")))))))))(Tile((id \
         d8a54e47-3e0e-4a44-8222-513de78c8d90)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         a98ed8f3-5fd4-472b-8c74-7e22d28d4dbd)(content(Whitespace\"\\n\"))))(Secondary((id \
         6a8a8475-0c3a-4a9f-ab2e-671a2af11381)(content(Whitespace\"\\n\"))))(Tile((id \
         0de42f25-1767-42b8-aebd-fb01c2bc6ef9)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         a6e51332-5cb7-4a3e-8c9f-6eab7ebb93f4)(content(Whitespace\"\\n\"))))(Tile((id \
         2e8571d3-32f5-4f0c-a541-0ce4c204c416)(label(running_sum))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         68b9ce84-39eb-421e-bb7c-5227469a65ee)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         ac8172c0-eeb7-4e37-9558-3ef9095dc54e)(label([ ]))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         7a83d8aa-8c1e-41a9-b83e-a7e674ffc8f0)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         ac562e92-f906-4dd0-9184-22d5394b2144)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         163a7170-7baf-405b-8f27-5d98e8232137)(content(Whitespace\" \
         \"))))(Tile((id \
         c3bab089-203d-4f85-a473-bff2e9c36435)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         310de1ac-0e37-4a28-b6df-cf9ea22368f3)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         102128e3-9dc7-4b38-b1bb-9615cbf1daa8)(content(Whitespace\" \
         \"))))(Tile((id \
         53133c55-a441-4a98-82fb-ed0ba3b6ba1b)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         5229c995-f753-48c6-9b8c-9a8dd9773a11)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         4b20cb43-d762-438d-8907-7a8feb956b27)(content(Whitespace\" \
         \"))))(Tile((id \
         c49f5f17-f023-49fc-bc3e-9c17138cefe7)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))))))))))))(Secondary((id \
         1bfebf0d-f40e-405c-a081-aa11db248d4b)(content(Whitespace\"\\n\"))))(Tile((id \
         40afbab1-e1b4-4a29-8045-558454b16bfe)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         3d68798f-8ef7-4fd1-9e27-ce6101a09759)(content(Whitespace\" \
         \"))))(Tile((id c6b91cbe-9814-4f8b-9fae-c73a2a6937aa)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         c2d6ac67-46a2-4016-bd28-d662e4c7bccd)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         ee7b6395-5881-47ce-87e5-d18341239305)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         b15e0dbb-6eb9-4c30-bf1f-8774b3c7321b)(content(Whitespace\" \
         \"))))(Tile((id \
         5b912c11-78a5-4e8d-84dd-691fb6f53b62)(label(2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         867d50db-6d21-4124-a37e-91e1fc73245e)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         9f391a17-843d-43ef-871a-864498dce460)(content(Whitespace\" \
         \"))))(Tile((id \
         f4d472ae-4931-44a2-ade9-3d2abb417057)(label(3))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         b63aff0a-1bd5-4467-b03b-60b595df4c47)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         7ea987d6-0f7f-4f35-9c94-089a84d2a604)(content(Whitespace\" \
         \"))))(Tile((id \
         815948e6-96b8-494c-bdd3-ba3430a51c03)(label(4))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         466902d0-fca5-4fd9-b366-5d2de24bbeb2)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         fa922d7e-9450-44b2-a6ab-1e72fd702b5d)(content(Whitespace\"\\n\")))))";
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
