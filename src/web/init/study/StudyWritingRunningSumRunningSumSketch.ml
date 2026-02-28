let out : string * Haz3lcore.PersistentSegment.t =
  ( "Study / writing / running-sum / running-sum-sketch",
    {
      segment =
        "((Secondary((id \
         a770a14b-2a10-4645-92c6-f47dbf8e1e8e)(content(Comment\"# RUNNING SUM \
         TASK                             #\"))))(Secondary((id \
         6e828d13-35ec-4dd2-a21a-27dc4076c58a)(content(Whitespace\"\\n\"))))(Secondary((id \
         e9ec4fbe-6430-418a-83de-7535e886d5f4)(content(Comment\"#                                              \
         #\"))))(Secondary((id \
         a236a4ae-dd0a-4145-98bf-e0285c5f8324)(content(Whitespace\"\\n\"))))(Secondary((id \
         5b38ac19-2abb-4bfb-b9f2-6cae9474663e)(content(Comment\"# Implement \
         running_sum: compute a list where  #\"))))(Secondary((id \
         d4b209d2-f6b7-43c5-91e1-9f02b0a04487)(content(Whitespace\"\\n\"))))(Secondary((id \
         49ae7d21-d9bb-4405-9c42-e0343cbd6a83)(content(Comment\"# each element \
         is the sum of all elements up   #\"))))(Secondary((id \
         3c62907a-0a14-492f-8e61-c1000c4cc2c7)(content(Whitespace\"\\n\"))))(Secondary((id \
         059721c1-8cc1-43c0-9a02-73685ea418a5)(content(Comment\"# to and \
         including that position.              #\"))))(Secondary((id \
         a7b0a1ac-754f-4a7c-a2e1-1b0fc2fd650e)(content(Whitespace\"\\n\"))))(Secondary((id \
         952fedc6-93c7-4b4b-83e6-212c6f51d888)(content(Comment\"#                                              \
         #\"))))(Secondary((id \
         e9610dd8-4b84-4287-8af3-d8c0de5d84db)(content(Whitespace\"\\n\"))))(Secondary((id \
         af080b02-0043-479e-9377-3c84c3a4d237)(content(Comment\"# \
         Examples:                                    #\"))))(Secondary((id \
         cfd8917c-a5c2-4282-adaa-428fa285e844)(content(Whitespace\"\\n\"))))(Secondary((id \
         ed046bc3-8ca9-48ad-b560-595b20ea3793)(content(Comment\"#   \
         running_sum([1, 2, 3]) == [1, 3, 6]        #\"))))(Secondary((id \
         bd9f1762-9bbe-4e0d-9521-ecfed287dded)(content(Whitespace\"\\n\"))))(Secondary((id \
         2a9f4278-ab50-4bf9-bad8-1c474a14b721)(content(Comment\"#   \
         running_sum([5]) == [5]                    #\"))))(Secondary((id \
         7027248d-50ee-42f8-bb71-f8c7af5af1a6)(content(Whitespace\"\\n\"))))(Secondary((id \
         10e1e19b-b89b-46e8-8d2d-3e8bd0ed4833)(content(Comment\"#   \
         running_sum([]) == []                      #\"))))(Secondary((id \
         6c80aa30-d10f-41b7-a194-224dcf201807)(content(Whitespace\"\\n\"))))(Secondary((id \
         edb38903-b9fc-4efb-971d-dfbd1b2c64b3)(content(Comment\"#                                              \
         #\"))))(Secondary((id \
         6f0b280c-7995-401c-a0fe-2af0e89dfeb2)(content(Whitespace\"\\n\"))))(Secondary((id \
         984279ca-7812-4b9c-bb6b-22189889f809)(content(Comment\"# Available \
         functions:                         #\"))))(Secondary((id \
         b6fbd9cb-b28c-42a3-bafe-4f977f7c5422)(content(Whitespace\"\\n\"))))(Secondary((id \
         979e647f-0b95-48ff-b572-e2916b4a471c)(content(Comment\"#   \
         fold_left(list, fn, init) -> result        #\"))))(Secondary((id \
         1916dad1-2f50-4e8d-bb7f-bf03cc2b5dd3)(content(Whitespace\"\\n\"))))(Secondary((id \
         334f9821-0e8c-4a69-9434-14056d35b0ed)(content(Comment\"#     fn takes \
         (accumulator, element)          #\"))))(Secondary((id \
         ba0fe7c7-9213-48af-b900-4193cfbd866e)(content(Whitespace\"\\n\"))))(Secondary((id \
         1018a2a3-3d0e-4305-a9eb-840cd6590858)(content(Comment\"#   \
         append(list1, list2) -> list               #\"))))(Secondary((id \
         1afda996-089b-46cd-8d5b-9bc60ddb154d)(content(Whitespace\"\\n\"))))(Secondary((id \
         a4ff91ad-c775-4c01-a268-a295f4d5f235)(content(Comment\"#   rev(list) \
         -> list                          #\"))))(Secondary((id \
         5f6b6646-f178-4048-8e57-55e40b41c241)(content(Whitespace\"\\n\"))))(Secondary((id \
         9354bf1c-967a-4765-a781-d4affcf101b1)(content(Comment\"#   map(list, \
         fn) -> list                      #\"))))(Secondary((id \
         a2319e01-f816-475e-bb93-867aa72b77a1)(content(Whitespace\"\\n\"))))(Secondary((id \
         02e54b35-0236-4b54-bfe6-ecb25d67966c)(content(Comment\"#   \
         length(list) -> Int                        #\"))))(Secondary((id \
         66902954-9b77-4030-80c5-796e833a2b05)(content(Whitespace\"\\n\"))))(Secondary((id \
         bb72a426-941a-43e3-a3f0-cf1ef05e43d3)(content(Comment\"#                                              \
         #\"))))(Secondary((id \
         5198e845-c5f3-46f5-ab5d-bf0d5a1d75d3)(content(Whitespace\"\\n\"))))(Secondary((id \
         2c81f07f-c7e9-4c62-8e42-370f05c7da97)(content(Comment\"# Syntax \
         reminders:                            #\"))))(Secondary((id \
         788ec9f7-84dd-4bf5-8bd2-36ed9dde67d5)(content(Whitespace\"\\n\"))))(Secondary((id \
         e2359a7b-d912-496d-a73e-946570e1e036)(content(Comment\"#   Tuple: (a, \
         b) = ...                        #\"))))(Secondary((id \
         6ab2f101-6df3-477d-a40a-577b01b9c5a1)(content(Whitespace\"\\n\"))))(Secondary((id \
         2d6fcc26-b246-4fac-9bca-6932b5e121b8)(content(Comment\"#   Tuple \
         access via pattern: let (x, y) = t   #\"))))(Secondary((id \
         00afe825-fa26-4807-821c-d0e19f90a32b)(content(Whitespace\"\\n\"))))(Secondary((id \
         d2baf322-6532-4635-8e0d-57a6b46fa6ae)(content(Comment\"#   List cons: \
         x::xs, List literal: [1, 2, 3]  #\"))))(Secondary((id \
         16a1f84a-1f4f-4e37-bbd4-aac8c725ba5c)(content(Whitespace\"\\n\"))))(Secondary((id \
         fb535d9a-8c87-42a5-adfb-e6774c068fcc)(content(Comment\"#                                              \
         #\"))))(Secondary((id \
         8fa27d7d-c272-4d84-8a6b-8099e2b2b1a3)(content(Whitespace\"\\n\"))))(Secondary((id \
         81cd492a-afa2-4797-8fd2-5a045078819d)(content(Comment\"# Tip: You may \
         need to track both the running  #\"))))(Secondary((id \
         8f9c8eaf-d03c-4bcb-a9e7-a53743bf87e1)(content(Whitespace\"\\n\"))))(Secondary((id \
         6ce0db3a-34af-4877-84f3-ade790dd0be5)(content(Comment\"# total and \
         the result list in your fold.      #\"))))(Secondary((id \
         c3c2c205-6036-41c8-a5bb-f195aa3ada0a)(content(Whitespace\"\\n\"))))(Secondary((id \
         fa15c29f-ca57-41d3-9e22-24f477cfc066)(content(Whitespace\"\\n\"))))(Tile((id \
         95c51cd8-9ff6-4f73-ae8e-d2d53ae88951)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         32d75188-cfc1-448c-b816-a6d0de75cf64)(content(Whitespace\" \
         \"))))(Tile((id \
         d3524d57-dc18-4988-a2f1-8612057684fb)(label(running_sum))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         ac4554e1-b1a7-48b5-9a17-4765aa628c4d)(content(Whitespace\" \
         \")))))((Secondary((id \
         f94988d4-2d82-4a0d-aaae-07982f001eda)(content(Whitespace\" \
         \"))))(Tile((id 09a7401d-09f4-4a6b-9c66-4452cf170547)(label(fun \
         ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 37))(sort Exp))))))(shards(0 \
         1))(children(((Secondary((id \
         411d57ee-9c48-4603-9e37-d53b3271f4dc)(content(Whitespace\" \
         \"))))(Tile((id \
         097b0ccc-4f52-45d5-ae5b-e666f521a1cf)(label(nums))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         a406c07f-f8af-4d59-a9a3-36831e753b63)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         22eaee37-b85a-4090-ae8b-7d69afabc967)(content(Whitespace\"\\n\"))))(Tile((id \
         5854568b-38a4-4046-9bef-f4849ce1e3bb)(label(?))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         b68ac258-6317-4ac7-9637-7742317e8849)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         550a2aa9-b705-47c8-8c03-e8fa44e9f774)(content(Whitespace\"\\n\"))))(Secondary((id \
         da0a0daa-13fd-40c6-b040-63456fa5ddb2)(content(Whitespace\"\\n\"))))(Tile((id \
         376cf0a9-7d27-4e64-88d4-cc89ddc7ca58)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         09fc89a7-1fb3-414b-984d-d2212fa87cec)(content(Whitespace\"\\n\"))))(Tile((id \
         f8d148de-7249-4f20-82ae-fa60799234c6)(label(running_sum))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         33ba8ff2-31cf-456a-9726-3d1e07775b17)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         2d5f4cbc-d505-4823-9873-46d00c18b4ac)(label([ ]))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         6e9419ae-c07f-4131-90e3-dbe1eca9c745)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         9f564f64-e123-45c0-821e-8c15dbcc194a)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         8c91ab40-9dd7-4439-ba1a-88bbedd5d356)(content(Whitespace\" \
         \"))))(Tile((id \
         9d0bbf02-b87e-4f66-af3b-5f5d5dd19293)(label(2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         fb1fe342-914b-487d-930a-c2786a6ed90d)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         1ca520e6-1edc-4f50-ac25-6f42ecdc411f)(content(Whitespace\" \
         \"))))(Tile((id \
         1c4dac2f-a7ce-439b-9a64-d11db7b5f591)(label(3))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))))))))))))(Secondary((id \
         962cb5e3-8525-4a05-8df2-4946e708aa8b)(content(Whitespace\"\\n\"))))(Tile((id \
         9790b86a-23f2-449b-a3f3-67e62d57cce3)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         55bafe03-ea5e-4db8-98e1-b9185691618c)(content(Whitespace\" \
         \"))))(Tile((id 63d7ffea-30eb-4530-b231-93ab626fa741)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         5da6c6aa-54b3-4a43-bb48-3fd8a16601c5)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         e0a22184-84bb-49c0-af9c-c065901b0539)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         c7da3fc2-91dc-4264-940c-d5237ae7f956)(content(Whitespace\" \
         \"))))(Tile((id \
         18a85b27-3626-4522-9aaa-5024b1235e4a)(label(3))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         2ceb9778-b374-42eb-b4f7-1d836a54622c)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         f9e852d4-99c4-4451-9af9-1cc165d86877)(content(Whitespace\" \
         \"))))(Tile((id \
         c33cc98f-797a-497e-b2c0-4d7543a6c6ab)(label(6))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         af58dfd5-1ba1-4e0a-951a-59504f8a2124)(content(Whitespace\"\\n\")))))))))(Tile((id \
         cbcc4de1-0a9d-451c-bf68-ccd6da4db9fa)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         e985d690-ca16-42e3-bc2c-dd02ceca292f)(content(Whitespace\"\\n\"))))(Secondary((id \
         4ffb7228-4d22-481d-9399-db1c07268eb5)(content(Whitespace\"\\n\"))))(Tile((id \
         12dbb177-c4b4-4c4b-8d2e-13ce8d30aca3)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         acf4bb6a-11f2-4918-98a0-61613074832b)(content(Whitespace\"\\n\"))))(Tile((id \
         3fad2a27-7b80-40dd-a83d-5bf23a4cb0fb)(label(running_sum))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         6f86f527-e0ae-43c3-88ee-95bfc9478890)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         0c9b8fe2-cccb-41f2-ad1e-6f2333030cb9)(label([ ]))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         aad44522-e1b8-4d07-9b8c-2655cacb4d02)(label(5))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))))))))))))(Secondary((id \
         44b2ab6d-a99c-4747-9da8-b274f7d58929)(content(Whitespace\"\\n\"))))(Tile((id \
         4ab6efd4-8718-4a73-86df-3283ca69f6e8)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         5669a426-f061-48e4-aead-5bd8ca5c3d77)(content(Whitespace\" \
         \"))))(Tile((id f58abf3d-29ee-4839-8434-1dbc1aac619e)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         d019f6a2-902a-454d-b356-8920599d1371)(label(5))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         155fe666-8ed2-4c1a-ba01-8f865ab2252e)(content(Whitespace\"\\n\")))))))))(Tile((id \
         f3f2386d-ac39-4252-9407-21b8a573f422)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         574cf825-060f-4415-8206-e9efb6534b25)(content(Whitespace\"\\n\"))))(Secondary((id \
         f9818c3f-f5be-42e7-83f2-aa8260a39b53)(content(Whitespace\"\\n\"))))(Tile((id \
         34974c2a-8e0d-4a4c-b2bc-dc91bbc04195)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         7e66cfb8-b677-4a63-9b53-84266e88fba1)(content(Whitespace\"\\n\"))))(Tile((id \
         b9960bfc-70a8-47bc-ac9a-89a2ba9f0de6)(label(running_sum))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         33e9160a-6158-449d-a876-95690dd04a9d)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         854b3b32-3ab9-45a8-a4ab-d3d121accac3)(label([]))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         dc50739a-6bca-492d-a606-1eab52389c58)(content(Whitespace\"\\n\"))))(Tile((id \
         3e13fd21-8d90-4163-9384-08636117d021)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         61c3ae6f-1f20-4e5a-9962-e4271a0ae79d)(content(Whitespace\" \
         \"))))(Tile((id \
         2a6b74b1-15f0-4195-a81e-6abfc2e1394e)(label([]))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         be103807-64e5-4435-a224-080c9fe3a1f2)(content(Whitespace\"\\n\")))))))))(Tile((id \
         508d4ffe-74cd-4eb9-afb0-ab151c157e84)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         4531bea3-1025-4f87-9350-40fe8d0758e0)(content(Whitespace\"\\n\"))))(Secondary((id \
         2bee70ec-2571-46e2-8b18-123f40728b92)(content(Whitespace\"\\n\"))))(Tile((id \
         ad210887-3e13-4930-b41f-d9caae24aa8a)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         1d0b1585-dec7-4e4c-9dc3-8cbc79b6f65f)(content(Whitespace\"\\n\"))))(Tile((id \
         1e459509-573c-4dd6-a866-c77463339cb3)(label(running_sum))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         8b83754c-c527-49b7-be34-64254788e49c)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         b099f12e-a148-4694-bed0-cbec1302e79d)(label([ ]))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         af9b008e-fdfa-4467-aff2-8572472556bd)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         18d0faa2-2361-440f-81a3-4398c9c0406b)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         be43de84-0dd6-4cae-8035-f1791ac45542)(content(Whitespace\" \
         \"))))(Tile((id \
         fd364aaa-6ee1-4ee6-b7a7-f35aa9fb2020)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         8d37a54d-8c3d-4626-8e84-f177d2015d4d)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         8012f632-3d49-4c9f-8bd0-106153001f2a)(content(Whitespace\" \
         \"))))(Tile((id \
         9cfe491b-97eb-414d-8993-ba49749c7cc1)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         287d6fed-13b1-4e24-84f0-ae6ed976328d)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         e056bb61-4f49-4cd5-95aa-d83d71e29a65)(content(Whitespace\" \
         \"))))(Tile((id \
         91704161-fa4e-4506-bcd0-c408a50dda41)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))))))))))))(Secondary((id \
         bdefe323-c588-4c56-b2f6-0a1f3c5ae9b9)(content(Whitespace\"\\n\"))))(Tile((id \
         4d9e888d-0d6b-42e4-b171-e0de747c15ab)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         ce43850d-9eac-4ecf-9af7-2089858577fe)(content(Whitespace\" \
         \"))))(Tile((id 5540c98b-97c3-4aca-9164-11ad24724441)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         ae3ea8ab-72e8-4af8-8912-5f0a3c4b2dc5)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         ed15ad1b-5fac-4acf-8716-e634d70debed)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         bf3c399f-417e-4a7d-b376-f2a611cd1f63)(content(Whitespace\" \
         \"))))(Tile((id \
         2ca33d14-a8bf-46ef-8f5d-44ff3f8330ed)(label(2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         f0a1408f-b268-4a04-8e98-8b191a0d9718)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         235f0b80-99b8-4a6d-af16-a7dd4891c388)(content(Whitespace\" \
         \"))))(Tile((id \
         b785bd71-8a42-4c5d-8191-5a44b6576350)(label(3))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         fd18cc1b-0ba5-4672-8b34-1a828b223490)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         1f819b3a-0a1e-4a89-8cad-1d10c22552a6)(content(Whitespace\" \
         \"))))(Tile((id \
         752f6fc1-2445-45ac-ab7c-eb845e0fb940)(label(4))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         e843e0f1-235f-45a0-8c95-630b909497e7)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         49432c81-99c8-4067-8cd0-b82ee1774bab)(content(Whitespace\"\\n\")))))";
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
