let out : string * Haz3lcore.PersistentSegment.t =
  ( "Study / tutorial / 09-running-sum",
    {
      segment =
        "((Secondary((id \
         2cdb2af0-4ce5-47cb-9030-16dc0727bfae)(content(Comment\"# PROBES \
         TUTORIAL - PART 9: WRITING EXERCISE (FOLD)                \
         #\"))))(Secondary((id \
         54c3f65d-59e9-4757-89cb-142a4be46318)(content(Whitespace\"\\n\"))))(Secondary((id \
         0b495508-e152-430c-b990-5f6c7bdd260c)(content(Whitespace\"\\n\"))))(Secondary((id \
         7857418a-7670-420f-8b93-fa6b43efbe2b)(content(Comment\"# Implement \
         `running_sum`: compute a list where each element       \
         #\"))))(Secondary((id \
         51b5ac85-e3c5-4ac0-89f1-7a98e7f6f213)(content(Whitespace\"\\n\"))))(Secondary((id \
         eb135258-b9d3-4c26-b218-96485364ee8b)(content(Comment\"# is the sum \
         of all elements up to that position.                   \
         #\"))))(Secondary((id \
         4e22d938-c5c6-421a-93d7-25306c49cb0b)(content(Whitespace\"\\n\"))))(Secondary((id \
         e44ca92f-d9cb-4bfd-94f6-9e76e9f8484c)(content(Comment\"#                                                                   \
         #\"))))(Secondary((id \
         c1dd3f5c-b5c9-41b3-916d-76d80932e9cd)(content(Whitespace\"\\n\"))))(Secondary((id \
         abeb74a2-eaf4-48ec-8ab0-c6e0ba1270df)(content(Comment\"#   \
         running_sum([1, 2, 3]) == [1, 3, 6]                            \
         #\"))))(Secondary((id \
         15d6065e-a63b-4540-88e5-a1dc4e096d1f)(content(Whitespace\"\\n\"))))(Secondary((id \
         4261f3c0-6a51-4a83-8753-9d536031cabe)(content(Comment\"#   \
         running_sum([5]) == [5]                                         \
         #\"))))(Secondary((id \
         fe845849-5de5-40a5-8d81-a8107c8da6f0)(content(Whitespace\"\\n\"))))(Secondary((id \
         e6a14cf2-51c4-4603-b7b4-5aa99006fffe)(content(Comment\"#   \
         running_sum([]) == []                                           \
         #\"))))(Secondary((id \
         8bac516d-066e-4530-aca1-f07af294255c)(content(Whitespace\"\\n\"))))(Secondary((id \
         2813db35-7b4c-49c3-aea9-1deae5ca2885)(content(Comment\"#                                                                   \
         #\"))))(Secondary((id \
         79d03fdb-620d-4f38-baae-f28a66466139)(content(Whitespace\"\\n\"))))(Secondary((id \
         b04dd090-880e-4d7c-adf9-e1d43f75b0e4)(content(Comment\"# Use \
         fold_left to walk through the list:                           \
         #\"))))(Secondary((id \
         5a4d010b-e641-493c-b585-dc4004702650)(content(Whitespace\"\\n\"))))(Secondary((id \
         009b6a8f-570b-4c87-993a-adcaf4635749)(content(Comment\"#   \
         fold_left(list, fn, init) -> result                             \
         #\"))))(Secondary((id \
         bda83266-c93b-4aae-83ab-aa67e1b055f1)(content(Whitespace\"\\n\"))))(Secondary((id \
         fccaa0f0-44f1-45fb-ba2e-6830bee81526)(content(Comment\"#   fn takes \
         (accumulator, element) and returns new accumulator     \
         #\"))))(Secondary((id \
         51679c55-8033-4cfb-9b7c-2ce12167cad2)(content(Whitespace\"\\n\"))))(Secondary((id \
         8c775237-ef97-478c-885b-b27d1fe9e99a)(content(Comment\"#                                                                   \
         #\"))))(Secondary((id \
         7cf6b7bd-7f71-4c17-ab5b-e30c486ffa20)(content(Whitespace\"\\n\"))))(Secondary((id \
         e5c331f8-fc5f-42c3-b4b3-c457a729296a)(content(Comment\"# Tip: You'll \
         need to track both a running total and the result    \
         #\"))))(Secondary((id \
         1827da4a-6f6f-4487-91d5-f62cf3548162)(content(Whitespace\"\\n\"))))(Secondary((id \
         13b22741-f761-48bc-8d75-822738bac4ad)(content(Comment\"# list. Use a \
         tuple (total, result_list) as your accumulator.      \
         #\"))))(Secondary((id \
         cf4468ab-7be8-4413-9c79-1458500c238e)(content(Whitespace\"\\n\"))))(Secondary((id \
         985fb768-8f22-4a0c-8290-9025e88a00ac)(content(Comment\"#                                                                   \
         #\"))))(Secondary((id \
         ccffa4eb-b81c-44eb-a0ed-fa825352b000)(content(Whitespace\"\\n\"))))(Secondary((id \
         758aa457-ab79-480a-ae31-2713432bddf4)(content(Comment\"# Other useful \
         functions:                                           \
         #\"))))(Secondary((id \
         b457ef09-ed24-4e2d-b5ac-118a3ade59f4)(content(Whitespace\"\\n\"))))(Secondary((id \
         76717486-b339-4187-a755-3c665a588988)(content(Comment\"#   \
         append(list1, list2) -> list                                    \
         #\"))))(Secondary((id \
         0f7dd570-cfbe-4a89-accc-7ceef2f1aa7e)(content(Whitespace\"\\n\"))))(Secondary((id \
         e0504c07-ea27-4e9f-a291-3f5138454f85)(content(Comment\"#   Tuple \
         access via pattern: let (x, y) = t                       \
         #\"))))(Secondary((id \
         a8f33895-0050-4bcb-887c-6fddb282750c)(content(Whitespace\"\\n\"))))(Secondary((id \
         ce24ebd8-1bc0-4c99-859a-e2f732dd3aeb)(content(Comment\"#                                                                   \
         #\"))))(Secondary((id \
         54c3122d-df76-45bc-bb25-70cde817df1f)(content(Whitespace\"\\n\"))))(Secondary((id \
         4f7f1eed-e0e3-4af7-b4d5-c7ac09358c0a)(content(Comment\"# Turn on \
         auto-probe and click inside your fold callback.           \
         #\"))))(Secondary((id \
         ed4c7b11-6796-44d3-ad38-ce8b527c7c9d)(content(Whitespace\"\\n\"))))(Secondary((id \
         a5c07ad9-8550-482c-9cf7-09eaa09580e0)(content(Comment\"# In Many mode \
         you can see the accumulator at each step.            \
         #\"))))(Secondary((id \
         82e51ee5-e0ca-4a4f-b0ed-aa04211cdc17)(content(Whitespace\"\\n\"))))(Secondary((id \
         9147cd5b-a883-4503-9702-b72e094bff24)(content(Comment\"# \
         =============================================================== \
         #\"))))(Secondary((id \
         0c6f8211-c766-473b-9439-231ceb93ce18)(content(Whitespace\"\\n\"))))(Secondary((id \
         0014d5cf-1821-441a-b08f-9ca73958de98)(content(Whitespace\"\\n\"))))(Tile((id \
         c2a6045b-a1f3-4bed-bb22-ac89e6b1fecd)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         3ce7b04d-9d36-4b13-a674-64d505d8c3a9)(content(Whitespace\" \
         \"))))(Tile((id \
         e2b6d537-7fae-4537-bb66-caf971c6356c)(label(running_sum))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         1e0664fb-8a7f-461f-bd65-d5f1e4d6e165)(content(Whitespace\" \
         \")))))((Secondary((id \
         b1b9ee88-3978-4002-a759-ff0b6c5588a0)(content(Whitespace\" \
         \"))))(Tile((id 15c2bf7f-8831-4fb1-b39d-68db51616bba)(label(fun \
         ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 37))(sort Exp))))))(shards(0 \
         1))(children(((Secondary((id \
         3ba27369-8772-4b15-9f5e-1e3c3182c90e)(content(Whitespace\" \
         \"))))(Tile((id \
         2f5e0e4b-104f-409a-8458-efba13a7ceb0)(label(nums))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         faf9d74a-5351-473b-be99-26b28304ea7b)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         bf062f88-7e53-4a30-af44-462c3a27043f)(content(Whitespace\"\\n\"))))(Tile((id \
         433b2d83-4bd5-4c3b-a39e-adc46e56af3c)(label(?))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         73af8679-eb9e-4541-b6d3-8132bd197a9c)(content(Whitespace\"\\n\"))))(Secondary((id \
         884e92b4-68b5-4e0c-ad75-9b100cefcdf7)(content(Whitespace\"\\n\"))))(Secondary((id \
         3887c4ef-baea-4482-84f8-bb16957fe8ca)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         f8d55dba-a53a-4f8d-83c9-98873ea60781)(content(Whitespace\"\\n\"))))(Secondary((id \
         c32629c0-b5e2-42f8-bcfa-7c622049f5e3)(content(Whitespace\"\\n\"))))(Tile((id \
         057701cf-cfc1-4f35-842d-bfa599915775)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         4f1a1c9e-3d9c-4cd2-9f4a-ef3e2a125825)(content(Whitespace\"\\n\"))))(Tile((id \
         357861c4-8acb-4875-9425-17585f36ad4b)(label(running_sum))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         dc4d6412-9aab-4f52-8122-c9b12dc2b9db)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         fa7e22c1-4417-4cf6-9395-bac5fc650db5)(label([ ]))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         9c34a0f7-fcf6-41be-a657-aced1e21c983)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         af1c5652-30b6-4ab0-aa90-8d4e50e8a795)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         54776546-0ef8-4df5-a392-8c9b1cdc75cb)(content(Whitespace\" \
         \"))))(Tile((id \
         93da8f5b-048f-4ee3-b466-0e241ff05658)(label(2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         2c6963ca-e16e-41dd-8079-a1faa29f771b)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         9b85fb53-2ec9-46d3-bf22-016d45b1c4b6)(content(Whitespace\" \
         \"))))(Tile((id \
         d584f441-251a-4bca-8e98-8b70f14bb870)(label(3))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))))))))))))(Secondary((id \
         d4b72cb8-d729-46e6-835b-9bd29ce9e500)(content(Whitespace\"\\n\"))))(Tile((id \
         a7b9ea59-d0e6-458e-9f51-e2b5718ccf2e)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         99d21b64-22f4-4bb5-b225-f8781f62575a)(content(Whitespace\" \
         \"))))(Tile((id 870128d7-4a39-48c0-8ad1-d6bfe812ccb6)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         733087f4-3f59-4992-af40-d61ff780f0f9)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         15a79603-d19c-4080-a9ff-ab7562371335)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         b379013b-69db-41d6-887f-1e69969b0ae5)(content(Whitespace\" \
         \"))))(Tile((id \
         3561ee9c-f515-44ce-8a80-08de073cae39)(label(3))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         7354776e-a6ba-4ec9-aac7-e9f1c76589a5)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         c817017b-385b-4ac0-b004-93a45d896c43)(content(Whitespace\" \
         \"))))(Tile((id \
         29cec559-d40b-4320-a33e-378a707bdc55)(label(6))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         b422fc0a-abff-4e3c-ad7b-57e6441e07e4)(content(Whitespace\"\\n\")))))))))(Tile((id \
         18bfb00b-5723-439a-96bd-921753562cf1)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         6bad3dbb-2bfd-4093-9eee-708ec322c6dd)(content(Whitespace\"\\n\"))))(Secondary((id \
         3dd3043c-8f98-439f-958b-91176f4851df)(content(Whitespace\"\\n\"))))(Tile((id \
         cf3cffff-42cd-45ac-8862-2ebd6cfe0d50)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         239773bb-35b9-4fad-be04-9c0f3b46ebc8)(content(Whitespace\"\\n\"))))(Tile((id \
         10b76adc-15d8-4cdd-bc60-88294b639986)(label(running_sum))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         d3cbaf2a-bc60-4954-be03-5b4c8aa7863f)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         bcd2db33-20f5-4d46-9864-12ad18c49c11)(label([ ]))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         0ca6fc54-69c7-4def-904e-9b8a14d625e9)(label(5))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))))))))))))(Secondary((id \
         29672cd9-3861-4586-972a-ab54d1760545)(content(Whitespace\"\\n\"))))(Tile((id \
         061c0433-7c2f-4e60-8ae5-724a50d9b3aa)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         ff7be5c4-39b1-4862-a819-abc556f176bd)(content(Whitespace\" \
         \"))))(Tile((id e4a69f0b-64a7-44c9-a27a-8a1f3f451238)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         9296a282-c65c-42f9-ac44-4c6baa54f851)(label(5))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         80377118-6150-4dba-b54f-25346901389c)(content(Whitespace\"\\n\")))))))))(Tile((id \
         2a1553b7-d5ea-4393-8171-1103b0f4ac9b)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         146648a3-200b-4bfc-a716-02d0651c56db)(content(Whitespace\"\\n\"))))(Secondary((id \
         0fa3aaf4-f24e-4e5e-a2c3-e8956a5cbae4)(content(Whitespace\"\\n\"))))(Tile((id \
         302e2636-f64f-4506-b4a4-28f963b58efc)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         da026021-380e-467d-a4d0-96d247414f57)(content(Whitespace\"\\n\"))))(Tile((id \
         a877502e-5f87-4cce-877d-ff8743e66cfc)(label(running_sum))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         1a13d624-d00b-4268-92f3-4cb4db7f5fae)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         e2bef99d-6ddd-49a0-941b-7e0c039859fd)(label([]))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         a3a688bc-e6b5-435e-91cb-529137a89777)(content(Whitespace\"\\n\"))))(Tile((id \
         ad2c7d90-8264-4ffc-bdd1-da08a5f35247)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         15562202-5f15-4545-a008-a5218b9e4249)(content(Whitespace\" \
         \"))))(Tile((id \
         6334ca80-de32-42c1-aa2e-ce8f1dbf43ea)(label([]))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         0f69a958-7b02-41b6-b5a5-805c39e3aaec)(content(Whitespace\"\\n\")))))))))(Tile((id \
         96fce8b2-b005-4e57-bd31-96396eed1662)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         9a4a1153-dfea-48c1-a78c-532f2d5f474d)(content(Whitespace\"\\n\"))))(Secondary((id \
         b1bd6a6f-1678-45ba-a3b1-ea24689f248d)(content(Whitespace\"\\n\"))))(Tile((id \
         7b0dda89-7ead-42d4-a925-cb8f101d5128)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         16b12487-e9a6-4310-a387-5ba89d27387f)(content(Whitespace\"\\n\"))))(Tile((id \
         dc542001-1361-4d02-98a4-d8d758af0768)(label(running_sum))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         5fdb846f-ad31-4c2a-9b1b-3e62eb4abcf3)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         bf771a8e-1419-491d-8450-16d996b1dac7)(label([ ]))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         0b47eded-eb0a-4194-ae36-daa02fc7a81e)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         9ea9ae7c-0606-4467-bb92-3b843ebb7777)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         cec81f85-6203-41f7-9cc6-c687dd72d5b6)(content(Whitespace\" \
         \"))))(Tile((id \
         74338936-9f68-4d1e-974e-9355b7be6d0a)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         e9111525-6ed8-4c55-8e77-e0eefe5904db)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         aa54651d-2a61-42fb-851c-be1316c27c26)(content(Whitespace\" \
         \"))))(Tile((id \
         80f2f2f4-956b-4341-bcb8-f571aeeaf04a)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         e5d52e0e-22be-4499-9eb0-56563766562f)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         cd8e55d8-16b3-4525-a53b-0838ac1e7195)(content(Whitespace\" \
         \"))))(Tile((id \
         2840ffe0-177e-4039-9b9c-23149d1f0aab)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))))))))))))(Secondary((id \
         26a5b632-76c2-4568-bc71-8866628b28cf)(content(Whitespace\"\\n\"))))(Tile((id \
         908c94d3-7436-4085-9353-0a4773c1a556)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         06c32d0b-2617-48a9-b233-c74733b27f83)(content(Whitespace\" \
         \"))))(Tile((id 52b34d9a-dfb9-4cec-b59f-a528b489577f)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         5d639ad1-a118-49ee-97aa-4c9fad54cccc)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         87c9ed60-9c92-4b63-aa39-03ab350404cf)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         46786b0c-25fe-4b31-b80f-3e4d7f532f2a)(content(Whitespace\" \
         \"))))(Tile((id \
         28e95104-8e47-4bb0-9bc3-102af97387f6)(label(2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         c4406ca9-73f9-4ee6-8c46-eef2be247b4f)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         4fa7be64-e269-4b35-b475-530bac0cd701)(content(Whitespace\" \
         \"))))(Tile((id \
         a2220270-ec2c-403a-b2d9-366c75ba66a8)(label(3))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         088c3cfa-0ccd-44d0-94af-054852933a99)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         ea5e3443-2c3e-4f24-91dd-015179ce6bc2)(content(Whitespace\" \
         \"))))(Tile((id \
         26093e9b-98c1-4c35-a516-5a9795dad9e1)(label(4))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         daf2ae6b-eeab-4bd1-a402-c83d70b0fb8b)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         5567cc64-f6a6-4f34-b4a0-439adf68dee2)(content(Whitespace\"\\n\"))))(Secondary((id \
         80ff5907-534c-4fb5-a5f6-d33951918910)(content(Whitespace\"\\n\"))))(Secondary((id \
         cc09643c-fe63-4515-a319-94530c67836a)(content(Comment\"# END OF PART \
         9 - Select the next slide from the top menu       \
         #\"))))(Secondary((id \
         17e453a4-893c-48ba-9c98-7704b5ca1c75)(content(Whitespace\"\\n\")))))";
      backup_text =
        "# PROBES TUTORIAL - PART 9: WRITING EXERCISE (FOLD)                #\n\n\
         # Implement `running_sum`: compute a list where each element       #\n\
         # is the sum of all elements up to that position.                   #\n\
         #                                                                   #\n\
         #   running_sum([1, 2, 3]) == [1, 3, 6]                            #\n\
         #   running_sum([5]) == [5]                                         #\n\
         #   running_sum([]) == []                                           #\n\
         #                                                                   #\n\
         # Use fold_left to walk through the list:                           #\n\
         #   fold_left(list, fn, init) -> result                             #\n\
         #   fn takes (accumulator, element) and returns new accumulator     #\n\
         #                                                                   #\n\
         # Tip: You'll need to track both a running total and the result    #\n\
         # list. Use a tuple (total, result_list) as your accumulator.      #\n\
         #                                                                   #\n\
         # Other useful functions:                                           #\n\
         #   append(list1, list2) -> list                                    #\n\
         #   Tuple access via pattern: let (x, y) = t                       #\n\
         #                                                                   #\n\
         # Turn on auto-probe and click inside your fold callback.           #\n\
         # In Many mode you can see the accumulator at each step.            #\n\
         # =============================================================== #\n\n\
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
         end\n\n\
         # END OF PART 9 - Select the next slide from the top menu       #\n";
      refractors = "()";
    } )
