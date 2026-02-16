let out : string * Haz3lcore.PersistentSegment.t =
  ( "Study / writing / running-sum / running-sum-sketch",
    {
      segment =
        "((Secondary((id \
         725be2ab-963b-448d-9bcb-2b3f24e3533d)(content(Comment\"# RUNNING SUM \
         TASK                             #\"))))(Secondary((id \
         d4169817-5dcd-4c81-a6f0-7f14ead0bdba)(content(Whitespace\"\\n\"))))(Secondary((id \
         20ba1987-0a7e-4dc5-922f-15251ae5f22b)(content(Comment\"#                                              \
         #\"))))(Secondary((id \
         d8be8de1-92d4-4cc8-9f49-e61c41dd96dd)(content(Whitespace\"\\n\"))))(Secondary((id \
         3ae2023c-81c4-4cfe-811b-43d8b5e57ad6)(content(Comment\"# Implement \
         running_sum: compute a list where  #\"))))(Secondary((id \
         43b3310b-c902-4cd1-94df-b21cb599af0a)(content(Whitespace\"\\n\"))))(Secondary((id \
         1dd3e050-405d-449b-853f-c83c62eaa234)(content(Comment\"# each element \
         is the sum of all elements up   #\"))))(Secondary((id \
         3b9d670b-39b4-4e44-9e85-c3a8ac2d6eb5)(content(Whitespace\"\\n\"))))(Secondary((id \
         c5f5c7e2-8be2-4f31-bbd3-f48ea68aa35a)(content(Comment\"# to and \
         including that position.              #\"))))(Secondary((id \
         3c177621-5cf4-4140-8c5a-afd071e352a3)(content(Whitespace\"\\n\"))))(Secondary((id \
         2fc4d854-35d1-46ba-9dca-7b0e523b7f43)(content(Comment\"#                                              \
         #\"))))(Secondary((id \
         71b7b546-773f-4ad6-8816-753b65b82fcc)(content(Whitespace\"\\n\"))))(Secondary((id \
         9f12535d-5ec9-4254-be3b-a75f4acf534e)(content(Comment\"# \
         Examples:                                    #\"))))(Secondary((id \
         aace634c-2760-40e6-916f-e50565e461df)(content(Whitespace\"\\n\"))))(Secondary((id \
         105a19f5-642a-48fe-b29e-5944ba9ce2a7)(content(Comment\"#   \
         running_sum([1, 2, 3]) == [1, 3, 6]        #\"))))(Secondary((id \
         01496285-c5ad-4581-865a-389813cdb973)(content(Whitespace\"\\n\"))))(Secondary((id \
         cd240866-bfb9-42ff-964d-d94128eb348a)(content(Comment\"#   \
         running_sum([5]) == [5]                    #\"))))(Secondary((id \
         36323028-73af-449f-a0e9-e97f6a2b8c05)(content(Whitespace\"\\n\"))))(Secondary((id \
         5395bb9d-a14b-4bd7-828c-0665aa9e8ca1)(content(Comment\"#   \
         running_sum([]) == []                      #\"))))(Secondary((id \
         1a38316c-bc4f-41a1-b96b-7b00dea3ba2c)(content(Whitespace\"\\n\"))))(Secondary((id \
         d83d2c4a-65ce-40f0-8a23-17cdd554e5dc)(content(Comment\"#                                              \
         #\"))))(Secondary((id \
         40e4bce1-2b98-4017-979f-b2610b735b09)(content(Whitespace\"\\n\"))))(Secondary((id \
         e5187284-c6df-4f6d-8412-9abd08e596ec)(content(Comment\"# Available \
         functions:                         #\"))))(Secondary((id \
         c93c98fb-3a6c-417a-889e-8e6b695560f8)(content(Whitespace\"\\n\"))))(Secondary((id \
         594bdda7-2161-4833-8b3c-b4be3cf63815)(content(Comment\"#   \
         fold_left(list, fn, init) -> result        #\"))))(Secondary((id \
         73474e87-91fb-469c-975f-d8099a976299)(content(Whitespace\"\\n\"))))(Secondary((id \
         d9e43252-dc3d-4f2d-a07b-ffea746f30d0)(content(Comment\"#     fn takes \
         (accumulator, element)          #\"))))(Secondary((id \
         31438bac-b97c-446a-a32f-ca907c4c169c)(content(Whitespace\"\\n\"))))(Secondary((id \
         770d0937-a406-402b-ae6c-7ee307edba0d)(content(Comment\"#   \
         append(list1, list2) -> list               #\"))))(Secondary((id \
         ed860760-3123-45a7-a360-f253f37fa62f)(content(Whitespace\"\\n\"))))(Secondary((id \
         c15b22c9-eea7-4481-8bc9-65784aef5065)(content(Comment\"#   rev(list) \
         -> list                          #\"))))(Secondary((id \
         f154b209-701e-4e55-a1ac-cca6053c7427)(content(Whitespace\"\\n\"))))(Secondary((id \
         7eece226-de72-43bc-a606-1915a8bf584c)(content(Comment\"#   map(list, \
         fn) -> list                      #\"))))(Secondary((id \
         07769a08-e543-4a3f-9160-d9c8efe27962)(content(Whitespace\"\\n\"))))(Secondary((id \
         2797e696-5702-48a3-8883-384780347f43)(content(Comment\"#   \
         length(list) -> Int                        #\"))))(Secondary((id \
         a29582e4-d2d5-4bf7-9251-cb0ca5fc4554)(content(Whitespace\"\\n\"))))(Secondary((id \
         24efa181-eb89-4ee1-a2b8-e60da7497137)(content(Comment\"#                                              \
         #\"))))(Secondary((id \
         3dacd5b7-c2ca-42d8-b04c-d0e9e88110bb)(content(Whitespace\"\\n\"))))(Secondary((id \
         2f381b21-0822-48ca-a6ff-a897669ec6cc)(content(Comment\"# Syntax \
         reminders:                            #\"))))(Secondary((id \
         29130ed7-28d3-4122-bc44-d35612af403a)(content(Whitespace\"\\n\"))))(Secondary((id \
         1b433f39-8f6b-4e9c-b1b7-1c0fa4dff3a3)(content(Comment\"#   Tuple: (a, \
         b) = ...                        #\"))))(Secondary((id \
         a7c0cfb9-db3f-499a-af8c-9f7aae9c6ea0)(content(Whitespace\"\\n\"))))(Secondary((id \
         cd2e44ca-494e-448f-8ce7-06e0eefbc600)(content(Comment\"#   Tuple \
         access via pattern: let (x, y) = t   #\"))))(Secondary((id \
         b9dc5890-9e56-49e2-8f88-dea1ec929314)(content(Whitespace\"\\n\"))))(Secondary((id \
         0e631a50-0be5-4ca8-bd94-1ded311c29f9)(content(Comment\"#   List cons: \
         x::xs, List literal: [1, 2, 3]  #\"))))(Secondary((id \
         f05148c7-cf83-4dde-8f85-0925aa922b43)(content(Whitespace\"\\n\"))))(Secondary((id \
         f5f679ec-e8fa-4b04-a444-361a2a5420cc)(content(Comment\"#                                              \
         #\"))))(Secondary((id \
         a61ad85a-5d8d-4aab-9d78-2022fa1266b2)(content(Whitespace\"\\n\"))))(Secondary((id \
         d5895c8d-4cd4-4c0b-a943-6bb0c2ef29dd)(content(Comment\"# Tip: You may \
         need to track both the running  #\"))))(Secondary((id \
         2eba07f9-37f5-4635-8930-09c84dd669e3)(content(Whitespace\"\\n\"))))(Secondary((id \
         f2b2e1fc-e9af-4442-9928-375d7b01a46d)(content(Comment\"# total and \
         the result list in your fold.      #\"))))(Secondary((id \
         3fd388da-b683-4d9b-9240-4881384f85ac)(content(Whitespace\"\\n\"))))(Secondary((id \
         f716f1c1-31f5-48c6-bd14-8e806d4f480e)(content(Whitespace\"\\n\"))))(Tile((id \
         9d6d960b-7141-4b03-b5cb-c2ff1cf31697)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         70379075-22da-40d9-98fc-d2adb869df3e)(content(Whitespace\" \
         \"))))(Tile((id \
         dd98d56c-59e0-4f77-ad54-2ead37546d7f)(label(running_sum))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         72a5c92c-322e-427c-ac6e-7c785d2c5f97)(content(Whitespace\" \
         \")))))((Secondary((id \
         4eb3609e-6592-46d6-bc72-7bc14f7d36d5)(content(Whitespace\" \
         \"))))(Tile((id 4f2ab1a2-d177-4f51-b03c-312dbad209d2)(label(fun \
         ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 37))(sort Exp))))))(shards(0 \
         1))(children(((Secondary((id \
         52cef32e-f29a-4261-a385-4685039de207)(content(Whitespace\" \
         \"))))(Tile((id \
         e0fdc20a-03b5-4f4c-99b3-bd9945adf7ea)(label(nums))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         b89c40e2-a78a-4df6-9097-907897a41794)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         5a35d3ab-5634-47e8-9222-1a4deaa2b0f9)(content(Whitespace\"\\n\"))))(Tile((id \
         af199665-3a0e-4e1a-96d5-46f06bb73719)(label(?))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         ef9f7af2-270f-4fda-a036-da0d16c2cb1e)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         c7bee9c0-5068-481b-ba2e-b70cff861aee)(content(Whitespace\"\\n\"))))(Secondary((id \
         644b043b-98b7-4049-a9ed-6555ecbdae03)(content(Whitespace\"\\n\"))))(Tile((id \
         c8ed83b4-ad7b-4ea2-b7d2-259353d94fb4)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         a319a812-1fc0-4afb-9918-440cca4a41f0)(content(Whitespace\"\\n\"))))(Tile((id \
         b9a1a899-fd7f-423c-a78c-3c0812a8a987)(label(running_sum))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         21994df4-5d54-4d81-9391-ad093ecd63a8)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         cd0b6678-8965-4701-8f5d-b5d02eb83f20)(label([ ]))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         b012b072-42f2-4d1b-b0c7-2600c2ec9a65)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         008e4980-2ee3-40c5-9d0c-6e187c217edd)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         c5d824a5-7eb6-416c-aa94-6d2dfd0a67c4)(content(Whitespace\" \
         \"))))(Tile((id \
         10ee0fa6-de11-4223-922d-b64000274480)(label(2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         2f806591-b876-4dbe-9e77-f9b966177b80)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         968af038-5b48-4ba6-bf8c-50f19727fe27)(content(Whitespace\" \
         \"))))(Tile((id \
         8fc6d0a6-0225-4453-94f0-4038211760bd)(label(3))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))))))))))))(Secondary((id \
         85feefaf-7184-468d-ab58-291ba8179bf6)(content(Whitespace\"\\n\"))))(Tile((id \
         e7c02905-df2c-4da3-b1df-03245d4afea1)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         705b1be0-58b9-44dc-8c30-11f3a3192c16)(content(Whitespace\" \
         \"))))(Tile((id 5950eb47-b031-4eea-8a90-5e97c77f3afa)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         cd51bd90-786c-4184-95b2-1097477fd7b5)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         a2c9bf66-a096-4c61-a644-40f0a0399dda)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         5e00e6d9-d12d-491d-8244-684c2d1a7c06)(content(Whitespace\" \
         \"))))(Tile((id \
         9aed803a-652e-41fc-8074-a743cee1ad35)(label(3))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         b6dabab5-f5fb-40e1-9cc7-887ae317e2c8)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         c7c88229-8261-4466-a5ea-89ea87496038)(content(Whitespace\" \
         \"))))(Tile((id \
         4f35430b-40b2-4f1d-9657-469431a321d3)(label(6))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         a098aad7-0f86-4d23-9fdd-f609d98d22ee)(content(Whitespace\"\\n\")))))))))(Tile((id \
         9ff78cea-dc44-4a77-9f7c-f45eef87d734)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         c4a83c5f-584d-454e-85ad-fd202e38f855)(content(Whitespace\"\\n\"))))(Secondary((id \
         f15da09b-d87d-44d0-8aa0-52003acf42d4)(content(Whitespace\"\\n\"))))(Tile((id \
         88d33998-a371-4925-a665-6ad1df02a675)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         e9522d2a-84bc-4423-b6d9-3784d2c3484e)(content(Whitespace\"\\n\"))))(Tile((id \
         1306c8bc-9caf-4c1f-ad78-940142be70fd)(label(running_sum))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         115155b2-49ad-4563-83d4-3dbb9ff2c2c0)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         53360c79-7519-4e77-9861-6d45802633ca)(label([ ]))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         faa4a571-7c3b-4c71-b6ef-e82f09cce11e)(label(5))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))))))))))))(Secondary((id \
         79c1b452-cc11-4c2e-bed1-cc274ee9b2e3)(content(Whitespace\"\\n\"))))(Tile((id \
         f7172acf-e2dc-4903-a288-0a23f95eff7b)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         876b7477-c146-490b-ae05-c24b1a0a2cf3)(content(Whitespace\" \
         \"))))(Tile((id 6fe40dc9-1af9-4bcc-b8a0-4fdad5c7d598)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         17aa387c-a783-4dad-bbd0-261ba5c4a76d)(label(5))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         1155757c-69b9-4e98-8aa2-7c71eb5d12e4)(content(Whitespace\"\\n\")))))))))(Tile((id \
         7ccafa38-bbcc-4366-aa91-167395fda75c)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         aa5440ef-13ea-4256-a2ae-eec75816ca72)(content(Whitespace\"\\n\"))))(Secondary((id \
         d016c886-a166-4ef7-ac76-fc9eceeed51d)(content(Whitespace\"\\n\"))))(Tile((id \
         6c510337-cfa1-4048-9d18-23b701ba66b0)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         d9e9607a-6e3a-49fc-9808-46b3d0d048f1)(content(Whitespace\"\\n\"))))(Tile((id \
         1553a9a7-ef35-4ba1-ac1d-c586a5b85a41)(label(running_sum))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         f6971928-9d0d-4155-843b-94571f8220ab)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         6ccc4c5f-6056-4485-80f9-29026d66ed93)(label([]))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         46ca7920-69ae-43f1-8f13-d0b3197cc036)(content(Whitespace\"\\n\"))))(Tile((id \
         8b6a5849-63a7-4517-af93-2687abc7c029)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         25165b60-4ba6-42a2-89fc-cebadde71fd9)(content(Whitespace\" \
         \"))))(Tile((id \
         4c7d0e6b-1055-4ec3-83d9-251a2b34e8e8)(label([]))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         06fe7213-74ad-4720-a343-73a763c57455)(content(Whitespace\"\\n\")))))))))(Tile((id \
         b8f9ff9f-e122-4579-9c99-c7e41399b140)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         f815b281-de4e-4aae-8271-9ae0360c159d)(content(Whitespace\"\\n\"))))(Secondary((id \
         9f2be767-444a-4e3d-af1f-71241b5a3ddf)(content(Whitespace\"\\n\"))))(Tile((id \
         88b2f43d-baeb-4025-99e6-e089dcfd764e)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         cbc93558-a6c8-4513-b07f-4619c5914d29)(content(Whitespace\"\\n\"))))(Tile((id \
         abe66785-32e4-43ff-93f2-7b60338a66ad)(label(running_sum))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         e5156951-b8bb-49e4-af26-e7f2d5b85cce)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         909ed221-f242-49d6-bccf-15afdb69f812)(label([ ]))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         b65b6bf5-aebb-4536-8ba8-f5852bacffe9)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         965fe7b4-c0fb-4600-a673-6d402d68744a)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         c8d2610d-ce38-4b1f-ae10-76c06ea22186)(content(Whitespace\" \
         \"))))(Tile((id \
         6bee6897-83a7-49a4-bd35-200d1aaba98d)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         766807f4-ce2e-450c-986e-83e104e9f079)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         884ca8bc-b633-4a0e-a508-2d10897e7d9e)(content(Whitespace\" \
         \"))))(Tile((id \
         b1fc4fb7-c61f-4f14-95c4-5530ecdea001)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         ca072a53-4b24-44d4-ab45-ef7c03300300)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         ba01a195-7aec-4325-8d82-b0881ef7d8e3)(content(Whitespace\" \
         \"))))(Tile((id \
         c7be8ca4-038f-419b-ab7e-c427960d3edf)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))))))))))))(Secondary((id \
         cd678240-5330-4eee-ab8b-3ba3dc901b42)(content(Whitespace\"\\n\"))))(Tile((id \
         393a0fb8-fa3e-4eef-901c-4b305407b39e)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         0e757548-09fa-4de4-aea6-85bd830d30f8)(content(Whitespace\" \
         \"))))(Tile((id c50f265c-05c0-4577-b239-e49a12c53d57)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         96aed244-eac7-4906-906f-21c55006cf3a)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         70bc71f6-08e1-40e6-8e15-cbcae6c4b310)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         c08c3e5e-7ee7-44f3-9893-00675fb43165)(content(Whitespace\" \
         \"))))(Tile((id \
         ef35b3bb-224d-4065-8dc1-62f85c24fef0)(label(2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         7147cb82-0160-4b3f-a571-58065bc700e9)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         ee66ba84-6b3f-4789-ae96-4dce5a8ec442)(content(Whitespace\" \
         \"))))(Tile((id \
         66eb6702-a417-4384-981f-e1bc38dba4ca)(label(3))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         ba13efe5-35e9-4ffa-8515-30619ceb7f61)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         a93b2454-dc25-48af-abda-e9012451f2c9)(content(Whitespace\" \
         \"))))(Tile((id \
         09b1d4d6-87da-4626-bcc9-d77e224c6903)(label(4))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         0976448b-451f-4985-af03-9b46440744bf)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         e26fafb4-b962-4304-a03e-c3e8c8ee757c)(content(Whitespace\"\\n\")))))";
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
