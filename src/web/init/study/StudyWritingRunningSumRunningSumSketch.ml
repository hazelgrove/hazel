let out : string * Haz3lcore.PersistentSegment.t =
  ( "Study / writing / running-sum / running-sum-sketch",
    {
      segment =
        "((Secondary((id \
         c513c090-a4a2-4015-8d37-8ec26e4ee169)(content(Comment\"# RUNNING SUM \
         TASK                             #\"))))(Secondary((id \
         a16691b3-7d51-437b-98b1-bec13018d443)(content(Whitespace\"\\n\"))))(Secondary((id \
         8c09eeb4-33f8-4cd7-8ad2-0c14d69c748f)(content(Comment\"#                                              \
         #\"))))(Secondary((id \
         3f754e55-2579-4be3-9988-5c264a137374)(content(Whitespace\"\\n\"))))(Secondary((id \
         1c331eb8-a9a2-4425-a970-c0255dde6f84)(content(Comment\"# Implement \
         running_sum: compute a list where  #\"))))(Secondary((id \
         068fd6ae-ae55-45ec-9ca8-dd39097a0ec6)(content(Whitespace\"\\n\"))))(Secondary((id \
         dfc73a10-e9ea-4ef5-9182-cb18e0b23bc5)(content(Comment\"# each element \
         is the sum of all elements up   #\"))))(Secondary((id \
         677e757f-9855-4967-beea-757fe203620e)(content(Whitespace\"\\n\"))))(Secondary((id \
         e6039e66-764d-4e4d-881a-9007cde17dfd)(content(Comment\"# to and \
         including that position.              #\"))))(Secondary((id \
         5e96c9d8-5822-40f4-8abd-64f1bcaf11b2)(content(Whitespace\"\\n\"))))(Secondary((id \
         31e6f38b-c108-4a84-83f7-52f029b889fb)(content(Comment\"#                                              \
         #\"))))(Secondary((id \
         41757440-c985-420f-aba5-1f55067b4b5e)(content(Whitespace\"\\n\"))))(Secondary((id \
         870d2fac-c7ab-4480-b888-cdbe79fb114d)(content(Comment\"# \
         Examples:                                    #\"))))(Secondary((id \
         c21bc70c-e3df-438d-8e67-218f41f7c68d)(content(Whitespace\"\\n\"))))(Secondary((id \
         0342306f-8a53-4f41-ae10-56ad761d9ca7)(content(Comment\"#   \
         running_sum([1, 2, 3]) == [1, 3, 6]        #\"))))(Secondary((id \
         82172037-0b73-476b-84d2-c799626a81d6)(content(Whitespace\"\\n\"))))(Secondary((id \
         ea488b31-a6a2-4933-98f5-9e3dfc771229)(content(Comment\"#   \
         running_sum([5]) == [5]                    #\"))))(Secondary((id \
         36513426-1ee7-41b9-bc3b-419d1465d8f9)(content(Whitespace\"\\n\"))))(Secondary((id \
         7b0af0dc-024f-4a5c-9022-f2bba8ae13d1)(content(Comment\"#   \
         running_sum([]) == []                      #\"))))(Secondary((id \
         dd8e629a-1f49-412d-a6a7-12157359f2a8)(content(Whitespace\"\\n\"))))(Secondary((id \
         70383b9c-22ff-449c-96ac-48d16d0127ef)(content(Comment\"#                                              \
         #\"))))(Secondary((id \
         33d3c000-8317-4e4a-9533-a0f91770641e)(content(Whitespace\"\\n\"))))(Secondary((id \
         54f8f995-e5d7-4827-bcfc-aeaa8c010c89)(content(Comment\"# Available \
         functions:                         #\"))))(Secondary((id \
         a86091f6-8d21-423d-9daa-9c812e6b76c6)(content(Whitespace\"\\n\"))))(Secondary((id \
         df838312-ee9c-41ba-81c8-66f1493b1f92)(content(Comment\"#   \
         fold_left(list, fn, init) -> result        #\"))))(Secondary((id \
         53ba18bd-a16f-4715-b29f-acffbf4ddc5e)(content(Whitespace\"\\n\"))))(Secondary((id \
         c2160b0c-f97a-45c9-9450-88b803a052b5)(content(Comment\"#     fn takes \
         (accumulator, element)          #\"))))(Secondary((id \
         545b84cc-7eb2-4b9b-8679-0a92283fe2ab)(content(Whitespace\"\\n\"))))(Secondary((id \
         4d8461f7-4e8a-4bc6-9e8f-a4ddb958a0e1)(content(Comment\"#   \
         append(list1, list2) -> list               #\"))))(Secondary((id \
         5f5d5321-f955-4891-befa-2466e388a7c6)(content(Whitespace\"\\n\"))))(Secondary((id \
         4d311ce2-3367-4c4c-8046-bdfce2a1e62a)(content(Comment\"#   rev(list) \
         -> list                          #\"))))(Secondary((id \
         379d05d2-f0a2-4dbf-9d87-ac7698fb0ce3)(content(Whitespace\"\\n\"))))(Secondary((id \
         1d23439b-99d9-4843-8031-ceecec2c2be1)(content(Comment\"#   map(list, \
         fn) -> list                      #\"))))(Secondary((id \
         7e696f46-64c6-44bd-8090-c550c3c8e303)(content(Whitespace\"\\n\"))))(Secondary((id \
         26eab789-3b77-425d-a156-8c94f9c58a82)(content(Comment\"#   \
         length(list) -> Int                        #\"))))(Secondary((id \
         6462f8aa-0650-40fb-b7f0-65928e70d870)(content(Whitespace\"\\n\"))))(Secondary((id \
         661289a8-408a-4cff-9e7b-3011b26efb1c)(content(Comment\"#                                              \
         #\"))))(Secondary((id \
         e8827d4d-9a86-40eb-ad99-3c4f5b3126ee)(content(Whitespace\"\\n\"))))(Secondary((id \
         757ccf38-9bf5-404a-9487-e2903146d790)(content(Comment\"# Syntax \
         reminders:                            #\"))))(Secondary((id \
         d2bed4ea-2abb-4c93-ae6e-42876c3cfbb1)(content(Whitespace\"\\n\"))))(Secondary((id \
         4b363549-56d9-45d7-acab-5a72d50768ae)(content(Comment\"#   Tuple: (a, \
         b) = ...                        #\"))))(Secondary((id \
         1b768741-ae52-4940-9660-8dab0fd86d8f)(content(Whitespace\"\\n\"))))(Secondary((id \
         3bf9dcfa-110d-4512-9db2-d042112daba2)(content(Comment\"#   Tuple \
         access via pattern: let (x, y) = t   #\"))))(Secondary((id \
         7cb48c88-70d8-4a60-b518-d4608d474bce)(content(Whitespace\"\\n\"))))(Secondary((id \
         0c424e6b-7068-4b64-90b7-e4109d1ddefe)(content(Comment\"#   List cons: \
         x::xs, List literal: [1, 2, 3]  #\"))))(Secondary((id \
         65ec4311-685f-48b7-b35f-c1cb6348c0d4)(content(Whitespace\"\\n\"))))(Secondary((id \
         045d2341-bebf-470e-84ec-d94c46ba9600)(content(Comment\"#                                              \
         #\"))))(Secondary((id \
         580bc11a-b893-42d9-911d-519bb2997453)(content(Whitespace\"\\n\"))))(Secondary((id \
         b1a4fd40-3d03-4bf8-8cf2-46c5d3873e27)(content(Comment\"# Tip: You may \
         need to track both the running  #\"))))(Secondary((id \
         eff20c4a-5c04-4498-a14e-3cffce635aef)(content(Whitespace\"\\n\"))))(Secondary((id \
         59ab5698-a422-4e32-b537-1cd99c1dca48)(content(Comment\"# total and \
         the result list in your fold.      #\"))))(Secondary((id \
         e6e217ca-cad2-491a-8636-8c3c19e2018b)(content(Whitespace\"\\n\"))))(Secondary((id \
         7b7dc84b-c414-4762-af87-b9e79fbe51fe)(content(Whitespace\"\\n\"))))(Tile((id \
         fcd2913e-7ac9-4e7c-9375-f5ee4c8c9a04)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         a0909717-f7c3-43ab-ade0-68768b1a9ff9)(content(Whitespace\" \
         \"))))(Tile((id \
         805cd511-4b07-4c86-9837-6caf22ecf577)(label(running_sum))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         776d30e9-4f08-47ff-a3b8-924a41874aef)(content(Whitespace\" \
         \")))))((Secondary((id \
         a7a59464-b592-4935-819c-632be315899e)(content(Whitespace\" \
         \"))))(Tile((id 24506477-0a33-4eb1-92b6-5c35886db396)(label(fun \
         ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 37))(sort Exp))))))(shards(0 \
         1))(children(((Secondary((id \
         f9f40c22-7088-4462-a501-0b0bcccbf3e4)(content(Whitespace\" \
         \"))))(Tile((id \
         358fb8c0-563c-413f-8d83-0b023d829e6c)(label(nums))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         d4d67560-8f34-4072-bc53-6f5a472cf1c0)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         4564ec0d-737b-42c8-b8f7-c80228dec84a)(content(Whitespace\"\\n\"))))(Tile((id \
         ed469ef4-5bec-4517-a21e-66194a126e6c)(label(?))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         187c9ba6-0008-4395-a1b3-4d604f815c5e)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         e7cc2ffe-7bf8-4c63-9b89-73893379a648)(content(Whitespace\"\\n\"))))(Secondary((id \
         e288a6f3-f19d-4a9a-809b-3140050e5da0)(content(Whitespace\"\\n\"))))(Tile((id \
         b045d665-a219-4eaf-9eb3-b5c984681d2b)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         50cdc0ba-ce70-4b3d-af52-e8fed39b8679)(content(Whitespace\"\\n\"))))(Tile((id \
         0aba8162-c9e5-40e4-a7f7-80ab48f20440)(label(running_sum))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         76d7bd4e-3295-48d5-b9b8-8f16e68b8e02)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         1931fe5b-f9df-4317-88ef-3757d1426a0a)(label([ ]))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         92ee85bb-822a-4b35-8332-0674cbf8e201)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         488280c5-6f5e-4333-bcb6-d82b84a17eb9)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         a3932b35-0ba8-4db2-b72d-d255489f7882)(content(Whitespace\" \
         \"))))(Tile((id \
         272fdb27-ea9e-44ce-8158-ccef1a6dbba0)(label(2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         fa74762d-480f-4894-b9b5-5e4d33c91370)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         f7c09c87-6698-4dd2-bcd1-897893a8fb6f)(content(Whitespace\" \
         \"))))(Tile((id \
         a9e4069b-55a0-4790-9b79-0f4f570ff02a)(label(3))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))))))))))))(Secondary((id \
         e84db002-8ee4-4c3f-8722-84431091be1a)(content(Whitespace\"\\n\"))))(Tile((id \
         1434ee89-445f-4481-bc16-73ee68bede9b)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         0dde65d3-eb7e-4ad1-8888-d97dce3fa876)(content(Whitespace\" \
         \"))))(Tile((id d01338fe-ff5a-4bb5-8f83-0c94feeddfc5)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         fe2552f4-ec22-480e-967a-a165bb1aab7b)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         b1230e37-97d6-46ee-a008-1898930e1627)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         a8fb8782-075f-4337-86d3-7c77f9863560)(content(Whitespace\" \
         \"))))(Tile((id \
         de0e62d2-093e-4c14-86d5-8b0423b74b1a)(label(3))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         2e70670b-f7c9-46a6-93ad-9df09e4c02c0)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         f8167d28-44a4-4f47-abf6-43b1aa2248f9)(content(Whitespace\" \
         \"))))(Tile((id \
         0ee581e2-ee6f-4dc0-984f-844f55d0ceaf)(label(6))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         bac1468b-6f8d-4889-b7b0-0dee746bbd5d)(content(Whitespace\"\\n\")))))))))(Tile((id \
         227d897b-df99-40b9-8542-0b40cf82d0e6)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         62a46e81-7085-4d53-a838-1f7a50a386a8)(content(Whitespace\"\\n\"))))(Secondary((id \
         8975ea72-8749-446e-ab64-de3bd209f960)(content(Whitespace\"\\n\"))))(Tile((id \
         82cd0806-1451-4463-b683-3eb669f8df2d)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         23283547-a7ac-427a-8293-2f70033695b9)(content(Whitespace\"\\n\"))))(Tile((id \
         0094aef2-b599-4fd5-b375-ce409d11b980)(label(running_sum))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         65e1161b-f1c4-4f40-9daf-7e517593c5b5)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         957f29e5-5680-4efd-9ac9-c40ac539a43b)(label([ ]))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         f0ba3eb4-a558-499e-9650-6294d98fa258)(label(5))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))))))))))))(Secondary((id \
         4ff394d6-b75d-4fe5-b62c-01876be8eb32)(content(Whitespace\"\\n\"))))(Tile((id \
         f418fdc2-026b-464b-9124-e9d8f1fe4b00)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         fd491ab1-2bfb-4c3b-b810-89aa636a8439)(content(Whitespace\" \
         \"))))(Tile((id 157459ba-3027-41b2-8f2f-ec76b9cf923b)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         95dee05d-bd9a-4a83-af1c-3119e56b7715)(label(5))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         aabe053a-50c7-4e1f-a2fe-392d995dd42d)(content(Whitespace\"\\n\")))))))))(Tile((id \
         b2d2e826-9f55-4ee4-aba1-78c92bf0be9f)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         343f4754-c2ac-46d3-b710-e82fdf646f79)(content(Whitespace\"\\n\"))))(Secondary((id \
         80eafc2b-7f2d-4e29-9ee1-3506e56eed12)(content(Whitespace\"\\n\"))))(Tile((id \
         022738c9-eb4c-4785-b4fc-5235a774e438)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         c579b7a1-4704-433b-97ee-f94945a521ca)(content(Whitespace\"\\n\"))))(Tile((id \
         e94f9575-ba82-4d14-a606-4c607026cf2c)(label(running_sum))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         fc759279-7f6c-4366-a252-d095680a71f5)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         791bdee3-2ff2-4c81-a50a-653005c4dbdd)(label([]))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         16c09a43-faa1-4656-a931-72ae905fedfb)(content(Whitespace\"\\n\"))))(Tile((id \
         6b3935ac-24f7-458b-97d5-f39f69a7a1a9)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         77ba3e4c-7da8-4064-a7f1-4975c76c93dd)(content(Whitespace\" \
         \"))))(Tile((id \
         7a3ba3db-c212-4c00-a353-80d7dcece736)(label([]))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         cabe5297-f3e5-49c6-b60b-02c803e771af)(content(Whitespace\"\\n\")))))))))(Tile((id \
         b1e346ff-d499-4483-b2a6-c5cdc143165b)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         295aaa19-0db5-477d-b7e0-2561d057182a)(content(Whitespace\"\\n\"))))(Secondary((id \
         61118224-8f19-432b-acde-eee0ee9e6213)(content(Whitespace\"\\n\"))))(Tile((id \
         693b4bf6-7f8c-4b1b-8ae4-75b9bdda493f)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         bc6bf3b5-11b1-46a0-88b7-b4758f9e36ed)(content(Whitespace\"\\n\"))))(Tile((id \
         26a00274-430d-432e-ad35-34987195a17b)(label(running_sum))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         1f053ab7-8630-4f48-b5cc-fcb3fb1a3478)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         9496406b-f04e-4e46-a3d9-6f1b897d27da)(label([ ]))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         c3b2d63e-76da-466b-87bf-02252c1f4e57)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         cfa85bbd-8a34-457a-ac0a-d2e055547a0b)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         b73072b4-b12e-4d89-adb6-221a44c1b6e0)(content(Whitespace\" \
         \"))))(Tile((id \
         0d940a58-2c8a-4594-b451-ee808ab98d67)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         988990ac-7389-4d86-a637-ad8dd09e878a)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         be79541f-e448-4e08-a159-f95438f67b9a)(content(Whitespace\" \
         \"))))(Tile((id \
         88eb0f4e-0907-4ae8-b242-8a310ff54bad)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         d7ef4b47-437a-4fa6-a9a8-2b017b9f7f9b)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         222bdb18-e89f-4da3-8324-92a2b7a8d508)(content(Whitespace\" \
         \"))))(Tile((id \
         584b2386-3d9e-4e1c-9288-8f7841fdd3bc)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))))))))))))(Secondary((id \
         00f8da26-c49e-4e7a-8ff5-e1287f92117d)(content(Whitespace\"\\n\"))))(Tile((id \
         2d23f3dc-779f-4ab2-9745-637209278ae2)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         5ed71705-4e11-436c-b3e0-704efea282f8)(content(Whitespace\" \
         \"))))(Tile((id a8261bca-e072-4869-835b-8663f621ec37)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         fb0470d9-bd3d-4756-a4a3-fd944481ad89)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         fd33877d-4cc0-4c2d-a5a4-69e5298f8395)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         4acf27a9-adab-4e8c-8f51-275011c1938c)(content(Whitespace\" \
         \"))))(Tile((id \
         b23c4976-5a1e-41c4-832f-cae6957fa138)(label(2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         b2bee144-3b85-4718-87d9-9fc59e949641)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         8ec09ffa-66d7-4830-9cf7-74c7e9c6ad6c)(content(Whitespace\" \
         \"))))(Tile((id \
         6c84df8a-9af0-4f14-ad25-b4273568f0f5)(label(3))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         a4516fbc-3267-4bbb-a963-d6cfa61560f4)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         3e325369-e335-4412-9173-5eb2bd1a4b40)(content(Whitespace\" \
         \"))))(Tile((id \
         cd2db82b-dba0-4c38-9f5a-b5b61068f7a4)(label(4))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         161b7d35-d88a-41c4-932c-f9bedeedb5aa)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         87f91a3c-3911-4f5c-9334-b392bcca016d)(content(Whitespace\"\\n\")))))";
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
