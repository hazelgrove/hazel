let out : string * Haz3lcore.PersistentSegment.t =
  ( "Study / writing / running-sum / running-sum-sketch",
    {
      segment =
        "((Secondary((id \
         c01ca78c-9751-459a-9406-7e996ebf43c0)(content(Comment\"# RUNNING SUM \
         TASK                             #\"))))(Secondary((id \
         38951200-f273-4ec2-a1e1-185cef7318e6)(content(Whitespace\"\\n\"))))(Secondary((id \
         639d2dfd-45c5-4c4e-bca6-5f042fe33957)(content(Comment\"#                                              \
         #\"))))(Secondary((id \
         db5ac031-d82d-4b98-b158-9f7c2c29a7ce)(content(Whitespace\"\\n\"))))(Secondary((id \
         308e13b5-5855-4e23-905f-f1bce2e0a801)(content(Comment\"# Implement \
         running_sum: compute a list where  #\"))))(Secondary((id \
         bceeb33f-0f7e-4487-8d52-375a295bd720)(content(Whitespace\"\\n\"))))(Secondary((id \
         34b8b912-5fd4-44b5-865d-e7b337abb927)(content(Comment\"# each element \
         is the sum of all elements up   #\"))))(Secondary((id \
         5b08895c-e35a-474b-8ee2-9a7e1b6743a6)(content(Whitespace\"\\n\"))))(Secondary((id \
         599557f9-12f5-40d2-8f51-fff986c69cde)(content(Comment\"# to and \
         including that position.              #\"))))(Secondary((id \
         9cbac35f-8896-4ec5-bd8f-00f15d18dc51)(content(Whitespace\"\\n\"))))(Secondary((id \
         1652967a-197b-48ae-a0c5-0704ad1dc9bc)(content(Comment\"#                                              \
         #\"))))(Secondary((id \
         501feb55-32c0-4e53-9f67-0fccb04af7d0)(content(Whitespace\"\\n\"))))(Secondary((id \
         81c932b7-5b5b-40de-bfd6-ccda63312fec)(content(Comment\"# \
         Examples:                                    #\"))))(Secondary((id \
         65d2457c-91ef-4aa7-ab9d-e536a99dccc2)(content(Whitespace\"\\n\"))))(Secondary((id \
         d46e8941-f44b-46ae-a83b-05904b16152f)(content(Comment\"#   \
         running_sum([1, 2, 3]) == [1, 3, 6]        #\"))))(Secondary((id \
         3bf23e9e-2ab2-4ace-bf14-5b1b680c5482)(content(Whitespace\"\\n\"))))(Secondary((id \
         b73ad5ac-267c-4460-9967-460bcb28e27a)(content(Comment\"#   \
         running_sum([5]) == [5]                    #\"))))(Secondary((id \
         312c0071-f39d-4cc8-b184-118403d4339a)(content(Whitespace\"\\n\"))))(Secondary((id \
         d3edbeb8-2f02-496a-91c1-3a348d9a8ae3)(content(Comment\"#   \
         running_sum([]) == []                      #\"))))(Secondary((id \
         d7ff5963-54e2-44f4-9bde-95f7aac95ae6)(content(Whitespace\"\\n\"))))(Secondary((id \
         8622d96f-af7c-4cfd-95c1-760dbfa18c9f)(content(Comment\"#                                              \
         #\"))))(Secondary((id \
         a2ac79d2-c7d6-4fea-8466-5ecfd607fab3)(content(Whitespace\"\\n\"))))(Secondary((id \
         b1adbe81-8b03-4025-9764-07c841ba10b1)(content(Comment\"# Available \
         functions:                         #\"))))(Secondary((id \
         d1096b67-a630-4055-bcd3-e4e40f49ada4)(content(Whitespace\"\\n\"))))(Secondary((id \
         addbf970-bad0-4aa3-ac51-84e9ee61e232)(content(Comment\"#   \
         fold_left(list, fn, init) -> result        #\"))))(Secondary((id \
         3949d116-d5ec-4eb8-b019-80861c0e6587)(content(Whitespace\"\\n\"))))(Secondary((id \
         1377563e-c6f6-460e-92f3-fb839c1a26a4)(content(Comment\"#     fn takes \
         (accumulator, element)          #\"))))(Secondary((id \
         6ba4051a-fa87-4716-8ce7-641a9a74969c)(content(Whitespace\"\\n\"))))(Secondary((id \
         92ad5c4d-a97e-4a8f-8e4f-a88631a28640)(content(Comment\"#   \
         append(list1, list2) -> list               #\"))))(Secondary((id \
         3b878b8d-553e-49dc-aac3-0046af0e3393)(content(Whitespace\"\\n\"))))(Secondary((id \
         0a84dfaa-160a-4e37-ace7-7c2896494dea)(content(Comment\"#   rev(list) \
         -> list                          #\"))))(Secondary((id \
         54963e77-2c29-4db0-9010-0de03030808e)(content(Whitespace\"\\n\"))))(Secondary((id \
         8e7a2a4e-8a69-4b61-9c56-5ec0a3191ab9)(content(Comment\"#   map(list, \
         fn) -> list                      #\"))))(Secondary((id \
         46c8cf96-fe7d-42d6-9520-f57477e72196)(content(Whitespace\"\\n\"))))(Secondary((id \
         bc5e692d-f5f3-4815-a476-89bad9dbce61)(content(Comment\"#   \
         length(list) -> Int                        #\"))))(Secondary((id \
         300fc965-5cc5-475b-8477-720159527a30)(content(Whitespace\"\\n\"))))(Secondary((id \
         3968f03e-cdd4-472a-86d8-d6ec0a3e9170)(content(Comment\"#                                              \
         #\"))))(Secondary((id \
         556e9bce-5601-43d3-8635-4f1e51f4538e)(content(Whitespace\"\\n\"))))(Secondary((id \
         e799c3ff-673a-4d4f-989e-f1fe172c886f)(content(Comment\"# Syntax \
         reminders:                            #\"))))(Secondary((id \
         87b6f2ea-1042-4474-b7f5-a8243316c893)(content(Whitespace\"\\n\"))))(Secondary((id \
         af676d90-9b2e-4c1a-80c8-b04d42f4842f)(content(Comment\"#   Tuple: (a, \
         b) = ...                        #\"))))(Secondary((id \
         46dafa4e-9182-4c05-bbc3-e67f3187f470)(content(Whitespace\"\\n\"))))(Secondary((id \
         f0149621-b844-47ec-b32c-b6c0b5f23a7a)(content(Comment\"#   Tuple \
         access via pattern: let (x, y) = t   #\"))))(Secondary((id \
         b78275dd-a75d-43f8-b9a2-4c550f801925)(content(Whitespace\"\\n\"))))(Secondary((id \
         70606f7d-7467-4b6b-b4c5-a8c3993684a5)(content(Comment\"#   List cons: \
         x::xs, List literal: [1, 2, 3]  #\"))))(Secondary((id \
         99e8be84-f1ea-48da-a869-4ca0b3381b81)(content(Whitespace\"\\n\"))))(Secondary((id \
         5f94b18a-43c0-4caf-b059-b042e5271126)(content(Comment\"#                                              \
         #\"))))(Secondary((id \
         fb0b3762-2dfb-4d16-a4b5-0c6d59912b5d)(content(Whitespace\"\\n\"))))(Secondary((id \
         1656b32f-3192-41c0-9b8a-f50b9dd3cc55)(content(Comment\"# Tip: You may \
         need to track both the running  #\"))))(Secondary((id \
         f45ca425-9a47-421c-82b9-636debab48b4)(content(Whitespace\"\\n\"))))(Secondary((id \
         49c6a32c-8696-4943-b22b-db23033894b2)(content(Comment\"# total and \
         the result list in your fold.      #\"))))(Secondary((id \
         f46fc19e-7328-48ae-b1a6-769195af1453)(content(Whitespace\"\\n\"))))(Secondary((id \
         a219c044-44da-49b6-a052-5d216b1cc868)(content(Whitespace\"\\n\"))))(Tile((id \
         2f147312-84ce-4345-a613-a85b1b43b93d)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         93d243e3-a769-4704-aa51-009fca89a88b)(content(Whitespace\" \
         \"))))(Tile((id \
         7ae0f92b-238f-4ad6-9067-a191d93e1923)(label(running_sum))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         5010473b-3d69-4e5b-8101-80d631422e5b)(content(Whitespace\" \
         \")))))((Secondary((id \
         134485e2-5990-421a-b48d-bee952c66e2b)(content(Whitespace\" \
         \"))))(Tile((id 25fda233-bf1e-4290-9a7a-6f1be8a7be19)(label(fun \
         ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 37))(sort Exp))))))(shards(0 \
         1))(children(((Secondary((id \
         40146981-7bc3-4845-9c37-f4cd920aeb14)(content(Whitespace\" \
         \"))))(Tile((id \
         95d7014d-d683-448a-a488-41f37a414a7e)(label(nums))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         0848dc42-9c20-4212-b002-346b32e76423)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         2391e2f4-117b-4d2f-9d60-a5c568d8e3d6)(content(Whitespace\"\\n\"))))(Tile((id \
         3042eb62-45c2-42cb-ab3f-3f7d026a4602)(label(?))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         e217d72b-d8d4-4522-9c93-31a897c0aa75)(content(Whitespace\"\\n\"))))(Secondary((id \
         fc338fde-654c-4d91-8145-95f22371f063)(content(Whitespace\"\\n\"))))(Secondary((id \
         af28dfc0-e8ba-4c70-b1cc-1baa603a6f43)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         1e244f8c-4c1c-4279-bcd1-c03f032eed23)(content(Whitespace\"\\n\"))))(Secondary((id \
         f6975623-9824-403e-809f-b75c6cc67a79)(content(Whitespace\"\\n\"))))(Tile((id \
         1748a6ca-d462-436e-bce7-5a5b7e072580)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         a2d77487-a2d5-4151-84ed-b40851a42250)(content(Whitespace\"\\n\"))))(Tile((id \
         a39565e6-fc20-4ee4-b96e-48c0bdd5c386)(label(running_sum))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         54fe0ae2-103f-40b2-98d4-06274b2d08bc)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         e21f32ca-1f44-40da-ae89-5db10b96a146)(label([ ]))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         d51c7f79-533a-4ebe-a2f8-b7f493a00f28)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         fa2ecbe2-0e9a-48a8-a659-d8b10ef36cec)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         9568dd3f-ac47-41f5-9ec5-27c3a9afa59b)(content(Whitespace\" \
         \"))))(Tile((id \
         2ef9a0fe-0996-4d81-ba86-83a23e3b9aaf)(label(2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         6bd0aae9-a6dd-4625-b899-cb870e1cd663)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         f0107d8b-250a-49b2-afc9-c184fe02d942)(content(Whitespace\" \
         \"))))(Tile((id \
         8b93f9b2-63d2-4dbc-845b-f5086e7cf618)(label(3))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))))))))))))(Secondary((id \
         1862cc09-9bde-4e26-97f1-8546f562fd08)(content(Whitespace\"\\n\"))))(Tile((id \
         557edea1-c08e-44d0-90c9-d96b78589c67)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         2028beac-d1d5-499f-94cc-91962fed3d4b)(content(Whitespace\" \
         \"))))(Tile((id d45b0f24-8703-41b4-b886-9ae05ead6eb4)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         c3b1775b-413d-4eb2-bf4d-ddd01c92ecb8)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         9f7892d6-24df-414c-8f29-3f58885661ab)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         4c84e5e9-cd6c-459a-82dc-ed30a4013ad8)(content(Whitespace\" \
         \"))))(Tile((id \
         1b209ede-d90b-40dc-b8a6-79da239c4cb1)(label(3))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         295a5f8a-3f26-4357-9c91-f6564d1bf593)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         f0d37edc-4b72-42ff-bda5-fecb0d075f68)(content(Whitespace\" \
         \"))))(Tile((id \
         4c88a1c5-9843-4f3e-a6c6-ba8e51458697)(label(6))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         f286b7db-cbc2-4bdb-a80f-c7b63d81def3)(content(Whitespace\"\\n\")))))))))(Tile((id \
         7bb0fac3-aa74-4297-a426-5fb03f03e477)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         c4c19170-b8e0-40b7-96a1-fd6d3b53d83e)(content(Whitespace\"\\n\"))))(Secondary((id \
         254d9a3c-46f8-4a87-bca5-44c1186af5c7)(content(Whitespace\"\\n\"))))(Tile((id \
         005ae30d-0630-4228-8e6c-b1e211ef8d4d)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         9d7dd877-66df-4969-9d39-15e9157d155b)(content(Whitespace\"\\n\"))))(Tile((id \
         9be65a27-3c91-429d-b145-96e1dca1ec7b)(label(running_sum))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         e7e1cbf7-cf92-4ef0-97fe-10b7f064422b)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         8ac1022d-a7db-4938-b235-29241aa07a07)(label([ ]))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         4cade7c4-753c-47a3-be3f-fcb6b293337f)(label(5))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))))))))))))(Secondary((id \
         9d71bda2-81b3-40ce-abad-164ae79f5fe6)(content(Whitespace\"\\n\"))))(Tile((id \
         1f721153-d094-4d0f-b674-71aa420416fc)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         d925b4af-8a40-4804-b00c-63267688de68)(content(Whitespace\" \
         \"))))(Tile((id fdd3c6c2-49a5-4c64-aa90-db50d4195c4b)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         dd195055-684d-4009-a10a-d09f30e97af4)(label(5))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         67c1198e-d07f-473b-9b6c-5df07db4ba95)(content(Whitespace\"\\n\")))))))))(Tile((id \
         361e8301-8b4e-4e90-b891-d03e24d876bc)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         a1aef589-b347-4816-b48b-6ff60f856840)(content(Whitespace\"\\n\"))))(Secondary((id \
         ebbaac8d-6770-4ed8-a2b7-56f09e9a5672)(content(Whitespace\"\\n\"))))(Tile((id \
         eea6496e-80e0-42cb-b188-ff7441c15d1c)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         385fe86b-5a44-4421-bf80-fc6c02c0063c)(content(Whitespace\"\\n\"))))(Tile((id \
         043398cc-13b1-49ea-b0bf-6f4d336299c9)(label(running_sum))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         de18b967-0e0b-46e0-a8f6-b46fb8bf6c36)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         3827f7bc-3257-4bf7-bb90-28f71d2bbca5)(label([]))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         107b1594-ae44-4f38-bf31-4c540c163cce)(content(Whitespace\"\\n\"))))(Tile((id \
         6ce150f4-5765-47a1-b65c-06d821653185)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         e5fc5770-3190-4ec6-a0b7-e392ba981aec)(content(Whitespace\" \
         \"))))(Tile((id \
         175bfe42-d05a-4f2f-800e-de7fdc5ee606)(label([]))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         4aef7cf2-70be-49ec-bc97-741a2ed6076a)(content(Whitespace\"\\n\")))))))))(Tile((id \
         573a3aa9-bb39-4208-b039-bf66c7d5a467)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         d978d983-0006-497f-967f-120d5cca64d8)(content(Whitespace\"\\n\"))))(Secondary((id \
         d6964b4b-5028-482d-80a8-db69f9b31cad)(content(Whitespace\"\\n\"))))(Tile((id \
         800093d9-3d76-459f-bf02-d5fb60579fde)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         1595a3da-4be8-4d8e-93b7-2e98151aadb5)(content(Whitespace\"\\n\"))))(Tile((id \
         48004e0a-c053-496f-bae9-10c390364937)(label(running_sum))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         de0c423e-3a90-4735-ba78-686f36eff672)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         0fb64136-ed51-4a61-b75f-e763fa613b2f)(label([ ]))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         49389e9d-b5e0-4b58-8230-ab5329626777)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         b7fb2784-56f3-4a21-96b9-c318049ea9e7)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         21658a7a-dd11-4485-88cc-8329549d90f0)(content(Whitespace\" \
         \"))))(Tile((id \
         b2abadc8-c8cb-43b1-870a-310b361c515b)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         62f02598-2da9-4cad-83a2-23f123f1209b)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         e95b9d48-7116-4f4a-86a9-4116e7f3abaf)(content(Whitespace\" \
         \"))))(Tile((id \
         4914e18b-e933-4c2b-860f-ef7cd4ebf5fc)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         6cc5a8f5-d134-4bd7-bacb-1d5e218810d3)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         ac814ebe-2c4c-4438-8470-c4c32477e72b)(content(Whitespace\" \
         \"))))(Tile((id \
         51e6421e-3027-4c20-b925-e15c9b1a2061)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))))))))))))(Secondary((id \
         5ee8f777-36fc-482f-b2e5-9844c583b034)(content(Whitespace\"\\n\"))))(Tile((id \
         6f400f88-b9fe-4168-aa69-814f24c7b149)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         61e2195a-bede-4c0a-97ec-afe864af1477)(content(Whitespace\" \
         \"))))(Tile((id ab8fd0ad-a89c-47ac-bd92-edca4a54aaa9)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         0feb469e-b4b7-4b79-b105-2de2918b5737)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         d3ce9517-5cfa-45ba-97ad-51666c435758)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         1bf01d19-c5a9-45dd-9179-03c32f37a551)(content(Whitespace\" \
         \"))))(Tile((id \
         e1960b62-94c7-4e90-a7ae-f3e109358cf3)(label(2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         e0b69a53-43a0-4ecb-9699-8bbd6f52c3d4)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         19adea2a-cdb7-4f63-bb3a-178aca1dccbd)(content(Whitespace\" \
         \"))))(Tile((id \
         1796258b-1cf5-415d-a6bc-3bdefd512190)(label(3))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         f56eb5d8-31ce-4751-b455-a15939c2a33f)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         fb12ba93-d12d-40a0-8fe7-af40927181f5)(content(Whitespace\" \
         \"))))(Tile((id \
         5a6e9bf7-db2e-459a-bb50-51ea9e871bd7)(label(4))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         ed9e7e03-cfe0-4278-9948-986c804720cc)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         54af4c36-5618-4918-a9c6-b9d6e5969081)(content(Whitespace\"\\n\")))))";
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
