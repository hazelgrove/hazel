let out : string * Haz3lcore.PersistentSegment.t =
  ( "Study / writing / mentions / mentions-sketch",
    {
      segment =
        "((Secondary((id \
         19d9be19-d2c2-4a3d-a5f3-ad6a9ab9d46d)(content(Comment\"# MENTION \
         EXTRACTOR TASK                        #\"))))(Secondary((id \
         60f997c0-1ef3-435e-8f63-15f1a68e7fe7)(content(Whitespace\"\\n\"))))(Secondary((id \
         c72dc158-eb55-4af0-ae8c-4e1761b86ecc)(content(Comment\"#                                               \
         #\"))))(Secondary((id \
         3c7a3355-97cd-4435-96bc-124924fc47a1)(content(Whitespace\"\\n\"))))(Secondary((id \
         2da52f38-cde3-4d07-aff7-c9c6ad9d4489)(content(Comment\"# Extract \
         @mentions from a garden message.      #\"))))(Secondary((id \
         53a70fc0-1940-46b3-860a-9a204c3fedb7)(content(Whitespace\"\\n\"))))(Secondary((id \
         ce753859-ebd2-46f4-8325-7f8939456ed9)(content(Comment\"# Given \
         \\\"Hey @luna the moonblooms are opening\\\", #\"))))(Secondary((id \
         ddb16fc4-7da9-4d3a-82a2-1b7f436350cb)(content(Whitespace\"\\n\"))))(Secondary((id \
         dfe3bcc8-b380-4595-9d1d-8e1bf986956b)(content(Comment\"# return \
         [\\\"luna\\\"].                              #\"))))(Secondary((id \
         180aea31-e704-4ade-939e-b3c76a79e344)(content(Whitespace\"\\n\"))))(Secondary((id \
         e2190540-ae52-4b5d-8967-259c33642ff8)(content(Comment\"#                                               \
         #\"))))(Secondary((id \
         0245c029-3951-410f-abe8-173ac520c275)(content(Whitespace\"\\n\"))))(Secondary((id \
         42c0c30a-375a-49cd-9e90-3de65078f9dd)(content(Comment\"# \
         Steps:                                        #\"))))(Secondary((id \
         409b9686-0e10-4918-bd94-8794fdf268bc)(content(Whitespace\"\\n\"))))(Secondary((id \
         6ec6b2e2-11a3-4938-ad5a-8fb1be17618e)(content(Comment\"#   1. Split \
         message into words                 #\"))))(Secondary((id \
         86070171-bdff-4657-afcf-92aa15c6e3ca)(content(Whitespace\"\\n\"))))(Secondary((id \
         a407e30d-e1cf-4e70-b3b7-9b13fe176801)(content(Comment\"#   2. Keep \
         only words starting with @          #\"))))(Secondary((id \
         fc28be01-6c4f-43eb-805e-e7a146d90a61)(content(Whitespace\"\\n\"))))(Secondary((id \
         4614277a-af88-4b5a-bc13-612c082f976d)(content(Comment\"#   3. Remove \
         the @ from each                   #\"))))(Secondary((id \
         cc327cf4-95f4-4419-835c-9db97b486d99)(content(Whitespace\"\\n\"))))(Secondary((id \
         9f338100-3866-4523-8b01-5994cf1266dd)(content(Comment\"#                                               \
         #\"))))(Secondary((id \
         fa1e08bb-fe75-4859-9a34-57037ed3c190)(content(Whitespace\"\\n\"))))(Secondary((id \
         79c6d9db-0d4a-4583-a9ad-4699a79fe71d)(content(Comment\"# Available \
         functions:                          #\"))))(Secondary((id \
         628f892b-82be-4ae4-9edc-b91172e85868)(content(Whitespace\"\\n\"))))(Secondary((id \
         2c21b7b7-14fa-46d3-8705-51c9fa67c463)(content(Comment\"#   \
         string_split(sep, str) -> [String]          #\"))))(Secondary((id \
         fd609c8e-1883-439e-83b5-d881741acbd8)(content(Whitespace\"\\n\"))))(Secondary((id \
         79204e20-db35-47e2-a4a8-84c9dcff748d)(content(Comment\"#   \
         string_sub(str, start, length) -> String    #\"))))(Secondary((id \
         3ba055e7-4d64-4c63-834f-319de93c102a)(content(Whitespace\"\\n\"))))(Secondary((id \
         8863288e-769e-450e-907e-7ddd4751a17b)(content(Comment\"#   \
         string_length(str) -> Int                   #\"))))(Secondary((id \
         7a950775-65cc-44f7-941c-38e2014d0c24)(content(Whitespace\"\\n\"))))(Secondary((id \
         ae800726-99cc-4040-8130-debe9d6e437a)(content(Comment\"#   \
         filter(list, predicate) -> list             #\"))))(Secondary((id \
         885c9e0c-8a8f-4306-b461-6f9ae8d3cd44)(content(Whitespace\"\\n\"))))(Secondary((id \
         b898a68b-b4a3-4a00-a177-78d1e953ef15)(content(Comment\"#   map(list, \
         fn) -> list                       #\"))))(Secondary((id \
         a43d5f79-7065-4c1b-9bd2-976082bdf313)(content(Whitespace\"\\n\"))))(Secondary((id \
         5157b1b9-fca2-41e7-96e1-955b298d1669)(content(Comment\"#                                               \
         #\"))))(Secondary((id \
         15de604e-088b-4c81-bdf7-b10e1114a610)(content(Whitespace\"\\n\"))))(Secondary((id \
         9e5454e5-66d4-4fb0-9803-d948fbba9af7)(content(Comment\"# Syntax \
         reminder:                              #\"))))(Secondary((id \
         11a0a0ef-a0b6-4b37-8c9b-984f8ea621c2)(content(Whitespace\"\\n\"))))(Secondary((id \
         68ba2581-1c40-47c2-8b00-de4a877f217c)(content(Comment\"#   let name = \
         expr in body                     #\"))))(Secondary((id \
         47cb23fa-84d2-4ef7-a03e-f8bd67dce405)(content(Whitespace\"\\n\"))))(Secondary((id \
         8ec0cde6-d1f5-4400-af32-058596eeb1eb)(content(Comment\"#   fun x -> \
         body                               #\"))))(Secondary((id \
         56f3c4d7-37d5-48ec-905f-99663230a286)(content(Whitespace\"\\n\"))))(Secondary((id \
         14095bbb-9e1f-4d94-8630-ed31cf62730e)(content(Comment\"#                                               \
         #\"))))(Secondary((id \
         10097f73-7971-44a6-9550-9ad72ed2bd00)(content(Whitespace\"\\n\"))))(Secondary((id \
         b8e42852-3b0d-4ed2-ba45-47c3c667affa)(content(Comment\"# Tip: Build \
         incrementally! Write one step,    #\"))))(Secondary((id \
         012185c7-028b-454e-b913-0ef661e38a50)(content(Whitespace\"\\n\"))))(Secondary((id \
         f1a0277a-4053-40b1-a7a0-b0f281f45ced)(content(Comment\"# check the \
         probe output, then add the next.   #\"))))(Secondary((id \
         ce8ab0d3-9e04-42aa-8968-14e7b80ea6b5)(content(Whitespace\"\\n\"))))(Secondary((id \
         ebb52819-c3cd-478e-a359-d165a559ff26)(content(Whitespace\"\\n\"))))(Secondary((id \
         a07b5a5f-11e6-4589-89aa-7fad5382e58b)(content(Comment\"# Helper: \
         check if a word starts with @ #\"))))(Secondary((id \
         106715e0-20e5-496f-a14a-776c424960e7)(content(Whitespace\"\\n\"))))(Tile((id \
         fbd0074d-2e19-4250-af9f-860985e44407)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         6c3b2b90-22a8-40ba-8159-b44c79852d73)(content(Whitespace\" \
         \"))))(Tile((id \
         b2ddc65c-c6b4-4a91-9bfb-90f7c9c65290)(label(starts_with_at))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         30c6254c-bdc1-4472-8cce-ab6c3dac4dde)(content(Whitespace\" \
         \")))))((Secondary((id \
         28b91fa2-90ef-47c1-a9ee-c122585d5db8)(content(Whitespace\" \
         \"))))(Tile((id c1356276-e303-4413-b741-a00d3dcfc382)(label(fun \
         ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 37))(sort Exp))))))(shards(0 \
         1))(children(((Secondary((id \
         1859becd-b0e5-46bd-b0bc-1d91773ac847)(content(Whitespace\" \
         \"))))(Tile((id \
         d5b73867-2a71-4f27-abf0-3ede3cfca3de)(label(word))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         d1e312bb-b346-4be3-8872-88ac5ad9da08)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         40f42fb0-aa81-4d1e-9c13-da6faa0a2872)(content(Whitespace\"\\n\"))))(Tile((id \
         f80a1875-409f-4819-88ff-64c2fb9c031c)(label(?))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         c67c65e4-a414-4d80-94b8-3c2b78dd2688)(content(Whitespace\"\\n\"))))(Secondary((id \
         fbf346c6-964a-43eb-9b7d-794f24f0c21a)(content(Whitespace\"\\n\"))))(Secondary((id \
         51b7123b-ade0-4951-8ca4-44d1ddd53386)(content(Whitespace\"\\n\"))))(Secondary((id \
         fca98ba4-6184-4d16-a501-91edd2b7326c)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         77f3d3ff-d77c-45f4-8f8d-93548e105ebc)(content(Whitespace\"\\n\"))))(Secondary((id \
         9b088688-e78e-4b40-a6b0-a09397d4fc9b)(content(Whitespace\"\\n\"))))(Secondary((id \
         34ed04d6-c49c-48af-8f83-775f8d2a68ca)(content(Comment\"# Helper: \
         remove the @ prefix from a word #\"))))(Secondary((id \
         a1c0a1ea-26be-4461-92f3-5303472e4232)(content(Whitespace\"\\n\"))))(Tile((id \
         f34d8bca-0de8-43d2-93de-908c0a8ab4e3)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         bbec6038-d6f1-45de-aff2-e380f827e839)(content(Whitespace\" \
         \"))))(Tile((id \
         85cae9f7-3d2f-46b3-8bf0-076dfcacf1fb)(label(strip_at))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         128a0cd2-5462-4cd7-8436-72ffd482c134)(content(Whitespace\" \
         \")))))((Secondary((id \
         86bb0163-fc16-456c-82ff-fe392b7a0513)(content(Whitespace\" \
         \"))))(Tile((id 77a6149d-aef1-439b-9dae-35b2810e15cd)(label(fun \
         ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 37))(sort Exp))))))(shards(0 \
         1))(children(((Secondary((id \
         6b902fd2-2734-47be-b784-702c4f54370c)(content(Whitespace\" \
         \"))))(Tile((id \
         ef225288-1581-4030-97a9-bda5bc57e684)(label(word))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         0a64422e-9e5e-437e-8636-6984eeeb5181)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         2ece513f-5c48-40c0-97b9-d5920c597761)(content(Whitespace\"\\n\"))))(Tile((id \
         41c9deed-caeb-4d3c-bcda-a523c35c8ec5)(label(?))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         ac9eeab3-978c-4735-be59-fcccdf253084)(content(Whitespace\"\\n\"))))(Secondary((id \
         48cf353d-a752-4172-92b9-c1e462e03c02)(content(Whitespace\"\\n\"))))(Secondary((id \
         821a8e0e-8e50-40d1-847e-4477467332fc)(content(Whitespace\"\\n\"))))(Secondary((id \
         fc725ac3-7164-4483-9f02-5cd2bad841a9)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         09b24fd7-9ba0-4004-b1d7-0ee86442e21d)(content(Whitespace\"\\n\"))))(Secondary((id \
         2cc8898a-4749-46ab-86ce-fe6eb78b4f52)(content(Whitespace\"\\n\"))))(Secondary((id \
         f89a03c3-60de-41bb-a13b-facfae5f8c62)(content(Comment\"# Main \
         function: extract mentions from message #\"))))(Secondary((id \
         95fd09ae-e846-4877-8b2d-c7cab1a5725c)(content(Whitespace\"\\n\"))))(Tile((id \
         0ef9e53e-bf4f-400a-b9fd-5e8ce890b3e0)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         9b308e8c-d6ae-4f39-ac2b-1a6c00e707ae)(content(Whitespace\" \
         \"))))(Tile((id \
         2ab5d7a3-cd76-45fb-9081-06b180429f38)(label(extract_mentions))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         8d7c7e19-de2f-46c3-8524-9dca3fb61f5f)(content(Whitespace\" \
         \")))))((Secondary((id \
         436c9511-b5c2-4c30-a65f-cb561ba6ffcd)(content(Whitespace\" \
         \"))))(Tile((id 61fda830-87b9-4f14-bbfd-9119353f9274)(label(fun \
         ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 37))(sort Exp))))))(shards(0 \
         1))(children(((Secondary((id \
         8fc1427e-86be-46a5-9c6d-f9bd38c4f587)(content(Whitespace\" \
         \"))))(Tile((id \
         e7f040d8-5c47-466b-b405-5f2ad3f73828)(label(message))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         9f93d63d-80c8-4e76-9d6e-1922613e28b5)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         e6dc5766-0da9-4566-9331-7677f3a13b60)(content(Whitespace\"\\n\"))))(Tile((id \
         191c75f2-d2ac-4686-8522-55996d35238a)(label(?))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         85d80ea5-d907-4140-828b-810805281967)(content(Whitespace\"\\n\"))))(Secondary((id \
         0b8d1d14-2d9b-49b2-9363-bca7462f87aa)(content(Whitespace\"\\n\"))))(Secondary((id \
         2427e2a9-ea67-451d-9ecb-9d199a9c4687)(content(Whitespace\"\\n\"))))(Secondary((id \
         883620c0-372d-4421-a3b1-51ff47cf4bc3)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         9d028a3d-d9aa-4f58-ba51-39bf4f7cee7f)(content(Whitespace\"\\n\"))))(Secondary((id \
         bfd73aaf-21a6-4b7c-b2cf-a6fca848735f)(content(Whitespace\"\\n\"))))(Tile((id \
         0f27803e-f0d0-4cda-a21a-8938ca558b1f)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         9d83ef1c-fcc9-47e1-abfd-a34a0134211f)(content(Whitespace\"\\n\"))))(Tile((id \
         a66908c4-9daa-4e23-9126-65996f087766)(label(extract_mentions))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         5f87fd6f-2934-4e54-8801-83b9906146fc)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         d79eb2dc-7a62-4c03-846f-081eb886972b)(label(\"\\\"Hey @luna the \
         moonblooms are opening\\\"\"))(mold((out Exp)(in_())(nibs(((shape \
         Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         a3c3f84e-1b1d-4b53-b72f-d033aea2a0fc)(content(Whitespace\"\\n\"))))(Tile((id \
         b8deda76-c361-452a-aab2-bfcf545eb636)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         787fe5e6-8cfb-4588-b5df-0f0c50b93c2f)(content(Whitespace\" \
         \"))))(Tile((id 5c0938ec-db8a-46d9-93dd-2164bd956ca5)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         473ad8c8-ec49-490c-9b5a-489f49ada12b)(label(\"\\\"luna\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         11764d89-6000-4dd8-9a9e-0e8b8606d712)(content(Whitespace\"\\n\")))))))))(Tile((id \
         b5e732e1-ccd2-478e-a362-07aa5d77217a)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         432d2b02-eab2-4e14-89dc-e511ce0f6d3c)(content(Whitespace\"\\n\"))))(Secondary((id \
         152d6ed9-f8af-46ce-8542-991305f97f79)(content(Whitespace\"\\n\"))))(Tile((id \
         36d91a01-d131-43f1-8482-d1e59083eb8f)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         88772c4b-6b11-4125-a383-19a06f9ea7a1)(content(Whitespace\"\\n\"))))(Tile((id \
         0d2357cc-f448-4b06-ae86-2773d18c5683)(label(extract_mentions))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         ea0a50b7-d12c-49d3-86b1-5b655fcc90c3)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         b9083a13-3a65-464d-a55a-4fc6e77cf6eb)(label(\"\\\"@thorn @moss check \
         the greenhouse\\\"\"))(mold((out Exp)(in_())(nibs(((shape \
         Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         f16daafb-1733-4c35-b0c7-0a2ca1b5f668)(content(Whitespace\"\\n\"))))(Tile((id \
         5785b834-3dd9-4f2b-935d-ac85e83840e7)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         36b29ecd-7e23-4099-89ec-6a4ed2984502)(content(Whitespace\" \
         \"))))(Tile((id e4f0b5ea-45c6-4e14-aace-7c1bfa2b6038)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         68f3a2ed-2a51-4e67-8154-f00184551ca3)(label(\"\\\"thorn\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         b6621c54-8ad1-4453-988f-81a65d56de97)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         8d3ef259-ecd6-4dc2-9824-80ef10c3eab3)(content(Whitespace\" \
         \"))))(Tile((id \
         540e8135-8cbc-41b7-958a-8a389e73532e)(label(\"\\\"moss\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         ed280c59-45b3-4859-b8a2-ee5983bfbe83)(content(Whitespace\"\\n\")))))))))(Tile((id \
         a0828786-c848-401a-92a5-2369ee8eb0c4)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         4c4b123b-f5b1-4f4d-8a6b-cd5b8fbcc3fb)(content(Whitespace\"\\n\"))))(Secondary((id \
         ff116f54-73b1-417b-b141-f42caef0f970)(content(Whitespace\"\\n\"))))(Tile((id \
         4cd0f7c3-dcd2-4eb2-a3a8-d20b6022eee6)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         ce7e5a8d-ef29-483d-af87-faedabd38453)(content(Whitespace\"\\n\"))))(Tile((id \
         0c78a9ed-43e8-4c47-a3cc-649832eae2de)(label(extract_mentions))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         9befec4e-fa7d-4bd5-9f87-106cb1ee7af6)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         3a1be299-bee0-44df-a294-0780c590a9d7)(label(\"\\\"the night air is \
         still\\\"\"))(mold((out Exp)(in_())(nibs(((shape Convex)(sort \
         Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         70d77374-ff59-4c9e-ae5a-828ca144fd4a)(content(Whitespace\"\\n\"))))(Tile((id \
         7db3fd77-a2fe-41e9-872d-84345c26c899)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         4f9060a7-8d39-489c-a1fa-15faae425d9a)(content(Whitespace\" \
         \"))))(Tile((id \
         7fc0acab-08f4-47d9-a442-0162c8d18eb2)(label([]))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         7e693a8b-2c28-47c5-b1f4-aa3151871164)(content(Whitespace\"\\n\")))))))))(Tile((id \
         e6b636e8-1994-4f19-95a3-75f038bc529a)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         3e9bdc6c-4485-4342-9cd7-2a1fadb63c39)(content(Whitespace\"\\n\"))))(Secondary((id \
         1080c902-4010-4637-9b3e-799128594d74)(content(Whitespace\"\\n\"))))(Tile((id \
         b03fdf89-1afb-4322-a555-74340bd8bba8)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         795c31cc-318f-4010-a413-67a2d0b52eac)(content(Whitespace\"\\n\"))))(Tile((id \
         d3de8e4a-0200-4700-9a4c-904a8ff0901f)(label(extract_mentions))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         443adbf0-0007-4164-b6bb-733f17e4042c)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         ef7543d8-2aa0-42f4-9e7f-2293451e285e)(label(\"\\\"@fern\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         7f60e264-5511-4549-8867-1a97efe25002)(content(Whitespace\"\\n\"))))(Tile((id \
         b89dafa3-79b9-4f1b-8127-0072b5c3171d)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         c51b8aa8-4ef2-4aa1-b6e8-970b1fb02fca)(content(Whitespace\" \
         \"))))(Tile((id 76688f2b-c7f8-4fcc-b630-a68a6c23349b)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         6204ee69-eb76-416c-8607-a3fc5811dcb1)(label(\"\\\"fern\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         1d80bde6-aaa8-41b6-a1f6-d2f883d16a03)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         a24b7d04-557a-46b6-9b0b-c87efdae7442)(content(Whitespace\"\\n\")))))";
      backup_text =
        "# MENTION EXTRACTOR TASK                        #\n\
         #                                               #\n\
         # Extract @mentions from a garden message.      #\n\
         # Given \"Hey @luna the moonblooms are opening\", #\n\
         # return [\"luna\"].                              #\n\
         #                                               #\n\
         # Steps:                                        #\n\
         #   1. Split message into words                 #\n\
         #   2. Keep only words starting with @          #\n\
         #   3. Remove the @ from each                   #\n\
         #                                               #\n\
         # Available functions:                          #\n\
         #   string_split(sep, str) -> [String]          #\n\
         #   string_sub(str, start, length) -> String    #\n\
         #   string_length(str) -> Int                   #\n\
         #   filter(list, predicate) -> list             #\n\
         #   map(list, fn) -> list                       #\n\
         #                                               #\n\
         # Syntax reminder:                              #\n\
         #   let name = expr in body                     #\n\
         #   fun x -> body                               #\n\
         #                                               #\n\
         # Tip: Build incrementally! Write one step,    #\n\
         # check the probe output, then add the next.   #\n\n\
         # Helper: check if a word starts with @ #\n\
         let starts_with_at = fun word ->\n\
         ?\n\n\n\n\
         in\n\n\
         # Helper: remove the @ prefix from a word #\n\
         let strip_at = fun word ->\n\
         ?\n\n\n\n\
         in\n\n\
         # Main function: extract mentions from message #\n\
         let extract_mentions = fun message ->\n\
         ?\n\n\n\n\
         in\n\n\
         test\n\
         extract_mentions(\"Hey @luna the moonblooms are opening\")\n\
         == [\"luna\"]\n\
         end;\n\n\
         test\n\
         extract_mentions(\"@thorn @moss check the greenhouse\")\n\
         == [\"thorn\", \"moss\"]\n\
         end;\n\n\
         test\n\
         extract_mentions(\"the night air is still\")\n\
         == []\n\
         end;\n\n\
         test\n\
         extract_mentions(\"@fern\")\n\
         == [\"fern\"]\n\
         end\n";
      refractors = "()";
    } )
