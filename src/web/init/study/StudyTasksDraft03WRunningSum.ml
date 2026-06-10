let out : string * Haz3lcore.PersistentSegment.t =
  ( "Study / tasks-draft / 03W-running-sum",
    {
      segment =
        "((Secondary((id \
         facb98a8-772a-4e7f-9fda-a46957ba511b)(content(Comment\"# RUNNING SUM \
         TASK                             #\"))))(Secondary((id \
         8f03467d-e1e8-471c-8010-448c8cc1e4bf)(content(Whitespace\"\\n\"))))(Secondary((id \
         6082c4ce-4bd2-48bc-a06a-a2efe1ecc0a9)(content(Comment\"#                                              \
         #\"))))(Secondary((id \
         54aa1526-5b45-45f3-ba06-09404d491b47)(content(Whitespace\"\\n\"))))(Secondary((id \
         317565ef-e3dd-4fda-bcf6-3559bb539d19)(content(Comment\"# Implement \
         running_sum: compute a list where  #\"))))(Secondary((id \
         8bc1c9fc-a9b5-43c1-8825-a3aa5be30e5d)(content(Whitespace\"\\n\"))))(Secondary((id \
         9de4e8c7-6029-45ab-ae09-d32cbd88c230)(content(Comment\"# each element \
         is the sum of all elements up   #\"))))(Secondary((id \
         61c692e3-8aee-40b3-8bab-b96e090b01c1)(content(Whitespace\"\\n\"))))(Secondary((id \
         a2d6d599-331d-4055-86d2-9b772de8c181)(content(Comment\"# to and \
         including that position.              #\"))))(Secondary((id \
         58e88071-22c3-4438-8d26-e1a23c8564d0)(content(Whitespace\"\\n\"))))(Secondary((id \
         e84055d2-dffe-4f49-945a-7f920752810a)(content(Comment\"#                                              \
         #\"))))(Secondary((id \
         fb7d634f-7a80-484a-bc89-d87f82fcc1e9)(content(Whitespace\"\\n\"))))(Secondary((id \
         2eab17e0-cbdf-4e1d-98f5-4528a66f8bd5)(content(Comment\"# \
         Examples:                                    #\"))))(Secondary((id \
         cdd30507-fbfc-4236-846d-59917fcc9f08)(content(Whitespace\"\\n\"))))(Secondary((id \
         16c87965-0513-4160-8df4-fbd8e46c527e)(content(Comment\"#   \
         running_sum([1, 2, 3]) == [1, 3, 6]        #\"))))(Secondary((id \
         c81f35b2-df50-4b78-ac35-84b0cc6f89ef)(content(Whitespace\"\\n\"))))(Secondary((id \
         6c2b407e-80d1-46ae-a35a-f22724ee611a)(content(Comment\"#   \
         running_sum([5]) == [5]                    #\"))))(Secondary((id \
         f5afdf2e-f6c9-46fd-bda2-3e967237b953)(content(Whitespace\"\\n\"))))(Secondary((id \
         61c80e19-708d-4249-a808-b022e85be920)(content(Comment\"#   \
         running_sum([]) == []                      #\"))))(Secondary((id \
         5a8ac775-8e69-4743-8956-c3a46f95b9cb)(content(Whitespace\"\\n\"))))(Secondary((id \
         ce0c5096-f2af-4413-b763-951834a30918)(content(Comment\"#                                              \
         #\"))))(Secondary((id \
         6cf6cab8-d2bf-44e3-a116-075122f5253b)(content(Whitespace\"\\n\"))))(Secondary((id \
         5d446065-391e-41f1-a6c4-69f507cf4ee9)(content(Comment\"# Some \
         standard library functions that may be  #\"))))(Secondary((id \
         4f4b1b4f-74bf-479e-bce0-d029de953b43)(content(Whitespace\"\\n\"))))(Secondary((id \
         87fb73f3-34f5-4124-baa9-84fdc4050a60)(content(Comment\"# useful (work \
         out the argument order with     #\"))))(Secondary((id \
         1548b541-fa04-4748-830a-969112a62461)(content(Whitespace\"\\n\"))))(Secondary((id \
         78ea49b1-5cce-4415-8b2d-119e7b92249f)(content(Comment\"# \
         probes):                                     #\"))))(Secondary((id \
         c9c95d12-e124-41e9-b214-1c70d1ee55ea)(content(Whitespace\"\\n\"))))(Secondary((id \
         0a4832c2-edff-433e-9513-f9014468de21)(content(Comment\"#   fold_left \
         : ([a], (acc, a) -> acc, acc) -> acc #\"))))(Secondary((id \
         365fda75-3ecb-4040-92cf-0cd5d6781bdb)(content(Whitespace\"\\n\"))))(Secondary((id \
         9dba0f56-90f6-424d-a4b3-36f7e15fcc00)(content(Comment\"#   append : \
         ([a], [a]) -> [a]                 #\"))))(Secondary((id \
         69c45bc5-58da-4c29-96e3-ca1b8d617141)(content(Whitespace\"\\n\"))))(Secondary((id \
         f8531911-f14c-42f3-8a05-82c140faf120)(content(Comment\"#   rev : [a] \
         -> [a]                           #\"))))(Secondary((id \
         6db0ebb3-2942-4b1d-bfbf-782e51e8ef0b)(content(Whitespace\"\\n\"))))(Secondary((id \
         4356e7b9-1e0f-47d4-83c3-adbf0730f4dd)(content(Comment\"#   length : \
         [a] -> Int                        #\"))))(Secondary((id \
         a4c2f4a0-98d5-4699-9512-b7a6191e8cec)(content(Whitespace\"\\n\"))))(Secondary((id \
         51847585-f914-4f5d-829f-875cec507eb1)(content(Comment\"#                                              \
         #\"))))(Secondary((id \
         da8539ab-4feb-4e2e-9fda-1c08ca05738c)(content(Whitespace\"\\n\"))))(Secondary((id \
         be711e27-a3b8-443b-a0fd-5dbac52d53da)(content(Comment\"# Tip: you may \
         need to track both the running  #\"))))(Secondary((id \
         df752d39-d3f4-4a5c-904f-0d5cbeeb7ea8)(content(Whitespace\"\\n\"))))(Secondary((id \
         5844c05d-2813-4948-abfd-6fe7e098067f)(content(Comment\"# total and \
         the result list in your fold.      #\"))))(Secondary((id \
         1b06367a-9ecc-4f4f-aeda-6a6d52dccfc0)(content(Whitespace\"\\n\"))))(Secondary((id \
         9cdbd9ff-55b1-400e-a16f-fad23515a583)(content(Whitespace\"\\n\"))))(Tile((id \
         bcb2af99-39d4-4c96-9ab6-1351a59ecf3f)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         4ab2d2ed-ad7e-420d-82b0-c335742a0f9e)(content(Whitespace\" \
         \"))))(Tile((id \
         75e17e4b-882b-4d10-b98a-f43cce4d449a)(label(running_sum))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         ce938b5c-7b6b-4150-8bcb-eb715fe77038)(content(Whitespace\" \
         \")))))((Secondary((id \
         6734417a-3b10-4449-8ce6-b3a0e7de8041)(content(Whitespace\" \
         \"))))(Tile((id 2c5984ad-4bff-4b3f-9429-75828a9823a7)(label(fun \
         ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 37))(sort Exp))))))(shards(0 \
         1))(children(((Secondary((id \
         dab3bcce-2eeb-4bc5-951e-67954defd6dc)(content(Whitespace\" \
         \"))))(Tile((id \
         d87144c6-4add-4558-9a90-f9c073a0908c)(label(nums))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         b2246948-94ad-4567-bab9-7dddc9efb370)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         3ed6f068-d6ee-40cc-8103-69b3f3a38172)(content(Whitespace\"\\n\"))))(Tile((id \
         47ab3599-06e1-4a8e-b435-45440736525d)(label(?))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         4526faad-ec9a-4f11-8bdd-6000d24b19a4)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         e49235db-6c1b-4248-a67d-5999e5a0ead6)(content(Whitespace\"\\n\"))))(Secondary((id \
         bea3c04f-e0c9-4d44-ad84-c12cae0700ed)(content(Whitespace\"\\n\"))))(Tile((id \
         e19a007a-9206-47b6-86de-c7ebc503a128)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         67a74fe8-fb9a-4d31-8d1f-5dfee34fc409)(content(Whitespace\"\\n\"))))(Tile((id \
         eb3fe870-eb1d-415a-86be-424468f71eae)(label(running_sum))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         66244673-4432-48a6-b3d5-5871d61ddd02)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         32a01e86-70eb-4332-b9af-1083ccb148bb)(label([ ]))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         1ee1cb8b-54fe-4725-a2b7-3fa4057805fc)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         d27e386c-8264-4e87-b33b-ca5cbb073bdf)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         cde950e0-1209-49ba-9359-b929eccd9c50)(content(Whitespace\" \
         \"))))(Tile((id \
         1b930333-a81d-4c72-9a34-be86ac093635)(label(2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         5a43fa9d-2d73-46f8-b798-fcf5515e04ff)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         04c02a0b-ef33-4eaa-bdbd-94373c7dcde2)(content(Whitespace\" \
         \"))))(Tile((id \
         c1641d4f-a128-449c-bb00-11a58b0a35a9)(label(3))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))))))))))))(Secondary((id \
         aaeea70c-2b98-4490-bec3-6dbc5659784e)(content(Whitespace\"\\n\"))))(Tile((id \
         7374e8f1-4b25-4407-a61d-aca42f5e2a75)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         0c3df174-5c85-43bc-9b15-427b46cc109a)(content(Whitespace\" \
         \"))))(Tile((id e6abf90a-a9a8-4db7-8ed6-f3eeb1ff34e4)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         553f46af-0ce0-453a-8e2a-11ba6c8ba87a)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         3e8e766a-8513-4237-8f20-a7ffc47a9285)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         bc043e64-a0de-41c3-8f35-abd26cb50e29)(content(Whitespace\" \
         \"))))(Tile((id \
         7e8f9298-89cd-4897-adee-9ba5e5cfc7a6)(label(3))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         a46c510b-b525-4224-9c25-7880b6deeb5f)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         21c4f57e-3e0d-49ec-8252-3b75391dbc12)(content(Whitespace\" \
         \"))))(Tile((id \
         c3dd316b-d7d5-4fd2-9857-89c6ba61465a)(label(6))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         1a33585b-02c8-45b5-acf6-0a134e98c856)(content(Whitespace\"\\n\")))))))))(Tile((id \
         dcb15d1c-d87e-4f4a-82f4-18953002288b)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         3afbd64e-8aed-4aee-8d0f-dcc9fa0db8c0)(content(Whitespace\"\\n\"))))(Secondary((id \
         4985e466-537c-4c0b-8558-66a7d1ca4d57)(content(Whitespace\"\\n\"))))(Tile((id \
         e71fe07b-073e-47d2-957a-2cf8c6378854)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         5286511b-7256-4d50-b227-0b514040f9fb)(content(Whitespace\"\\n\"))))(Tile((id \
         94b3e8ed-5f09-4ee0-aa64-3df6d1219d1d)(label(running_sum))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         1ad6aa30-2c50-484e-8736-3931b11d5116)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         70b47c83-45a8-413e-9fd4-fb9c8addaff1)(label([ ]))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         d9b8ae52-1e20-4f66-9364-b89841b9b2ba)(label(5))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))))))))))))(Secondary((id \
         e0800cae-feea-42c8-b7a7-a8743cce281c)(content(Whitespace\"\\n\"))))(Tile((id \
         cb6d2455-fba2-4980-91a5-4a641844257e)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         32a7ba33-f3f6-44b9-b554-f263317f2d0a)(content(Whitespace\" \
         \"))))(Tile((id 463d200f-a629-4af5-9cde-d0e1a22ae611)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         0a7be852-1312-487f-81ef-b34eec6c4ea2)(label(5))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         2517ae3f-f0d8-4a6b-b993-ea2e6070e3ce)(content(Whitespace\"\\n\")))))))))(Tile((id \
         d455c4d1-df5f-4535-9e68-b91c59cd54b9)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         79bb1dab-a2fe-4a0d-bd6e-f0a8df0c93ba)(content(Whitespace\"\\n\"))))(Secondary((id \
         f9ac86ce-03cc-4a44-a37d-bbffe97e8b1a)(content(Whitespace\"\\n\"))))(Tile((id \
         5b663674-65be-4ec2-89fc-87365ab7ce04)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         d6d4a116-8bc2-4cd2-bb4d-7edbfd6699a7)(content(Whitespace\"\\n\"))))(Tile((id \
         734d63f9-6aeb-48e9-9ec5-8ee5555ea946)(label(running_sum))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         e67a87f2-3a2d-414b-96ff-7b5bf9a60a6f)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         8bdd9d26-abf8-4b0b-8912-9bf982af9d7c)(label([]))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         01148b6b-5844-46c0-b98a-02fedbfc6be3)(content(Whitespace\"\\n\"))))(Tile((id \
         3cbe24a4-0c03-4023-bc94-ca7947cf33bd)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         b6d5fb35-fcaa-4baf-b1f2-296b9be8c32d)(content(Whitespace\" \
         \"))))(Tile((id \
         537b87b9-8d74-4313-877d-bff8e7d9cd0a)(label([]))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         a9177485-86ab-448c-9475-c3a1681457b0)(content(Whitespace\"\\n\")))))))))(Tile((id \
         d5891f7c-2b82-470e-9dba-39e8026de1ef)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         ddb95d04-236e-4ebb-b181-f6330a0ff2e1)(content(Whitespace\"\\n\"))))(Secondary((id \
         b7692a2b-09fc-4901-959b-e7d47a540bf6)(content(Whitespace\"\\n\"))))(Tile((id \
         022af85b-499c-49de-9155-03e217ade57a)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         e5cf28de-627b-47a6-ae53-96e89de67348)(content(Whitespace\"\\n\"))))(Tile((id \
         f9a26af4-777f-4294-90d9-6465c0d46c20)(label(running_sum))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         18c57447-5f83-4bc9-9cd6-fdcd2b3aaa13)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         1b9bb9df-27ca-4594-89aa-e329f6bd4588)(label([ ]))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         fe81a585-b1db-4570-b7a1-9c85016ca814)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         838efa8b-3f61-4173-bac6-b861d9bf8f06)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         8cd20e37-aedb-42ab-b325-99ca82405faf)(content(Whitespace\" \
         \"))))(Tile((id \
         14ff4d55-3f32-46e8-a2ea-d84c032cfcdd)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         b7ff03ed-fa8b-4430-b051-8602b8977a0c)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         5f861007-7691-4b7e-bf17-1aae758b4b3a)(content(Whitespace\" \
         \"))))(Tile((id \
         05cd1a3f-5dab-4c85-8112-5001453c81c4)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         80dd60ab-03e9-4e51-b687-a5386739ec43)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         00bcccf2-f247-40ca-8acb-30d89b3e2655)(content(Whitespace\" \
         \"))))(Tile((id \
         fd5d7723-0027-4341-9f83-259337b1fc1f)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))))))))))))(Secondary((id \
         3183a63e-5b73-4641-afff-906f874c5383)(content(Whitespace\"\\n\"))))(Tile((id \
         a967891d-af82-4b1a-b1dc-b5fa3d42c1fe)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         ed9ce0bf-d7bc-4fad-b5d4-9d19f04015c1)(content(Whitespace\" \
         \"))))(Tile((id 2b01e8d0-080b-49ca-bb63-0db07af79930)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         3e7b59d1-0f6b-459f-940c-51c0973b2e36)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         508e5504-c5e4-4bef-bc25-1c378ac6a8e6)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         5d8f174d-e844-48a2-8774-dd807cfff047)(content(Whitespace\" \
         \"))))(Tile((id \
         be066421-61f5-470b-9a8e-1c50a688ee7d)(label(2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         5d3f53bb-c032-47e9-9a72-1a6048cd0a42)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         9d83227b-0ccb-4643-a793-3a87434ac338)(content(Whitespace\" \
         \"))))(Tile((id \
         d98a4d81-c28d-4415-bd3d-18421c3ba769)(label(3))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         a64dd419-e8af-4a8a-ba72-0f4e29402ad5)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         ad24aab1-f7b4-47ca-9cd0-ebf20880a64c)(content(Whitespace\" \
         \"))))(Tile((id \
         6baed941-4c9c-4dea-a36a-e639d14dec15)(label(4))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         790db836-39c5-4bab-9dbf-667bc56f1a9f)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         9609a0b7-73d5-4ad7-840f-b29bcfd73c3b)(content(Whitespace\"\\n\")))))";
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
         # Some standard library functions that may be  #\n\
         # useful (work out the argument order with     #\n\
         # probes):                                     #\n\
         #   fold_left : ([a], (acc, a) -> acc, acc) -> acc #\n\
         #   append : ([a], [a]) -> [a]                 #\n\
         #   rev : [a] -> [a]                           #\n\
         #   length : [a] -> Int                        #\n\
         #                                              #\n\
         # Tip: you may need to track both the running  #\n\
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
