let out : string * Haz3lcore.PersistentSegment.t =
  ( "Study / tasks-draft / 03W-running-sum",
    {
      segment =
        "((Secondary((id \
         f3b840ed-5457-4bfa-9a24-447f5e4d971b)(content(Comment\"# RUNNING SUM \
         TASK                             #\"))))(Secondary((id \
         e149e856-ad5d-4360-8579-fa5e69500b75)(content(Whitespace\"\\n\"))))(Secondary((id \
         caca8b4c-dc36-4b8b-88da-db67206d47cc)(content(Comment\"#                                              \
         #\"))))(Secondary((id \
         ac170c47-5589-4a23-9081-4407db08afe6)(content(Whitespace\"\\n\"))))(Secondary((id \
         17cdd78a-3eb8-4f43-a823-79aba22c3876)(content(Comment\"# Implement \
         running_sum: compute a list where  #\"))))(Secondary((id \
         f8c13294-d2f4-4af3-bfc6-52487bf41649)(content(Whitespace\"\\n\"))))(Secondary((id \
         4f798b0b-1e11-4df2-9e26-2e5e1591e4c8)(content(Comment\"# each element \
         is the sum of all elements up   #\"))))(Secondary((id \
         e6ddd0a7-8638-47e5-8a70-be147d70f7c1)(content(Whitespace\"\\n\"))))(Secondary((id \
         4ac1888a-ee89-46af-b864-a60a3834d893)(content(Comment\"# to and \
         including that position.              #\"))))(Secondary((id \
         d4258db1-ff05-4eb8-84fb-2da7ed550213)(content(Whitespace\"\\n\"))))(Secondary((id \
         c5dcbf90-e6e8-407d-a0db-952fba4529b2)(content(Comment\"#                                              \
         #\"))))(Secondary((id \
         16aaf7d0-5abb-42e8-863e-35a755c5c299)(content(Whitespace\"\\n\"))))(Secondary((id \
         fcb3a6ad-90c4-43af-ab8b-a2f546e70e38)(content(Comment\"# \
         Examples:                                    #\"))))(Secondary((id \
         9c45573c-3aa8-42de-8378-d199a1d31fe9)(content(Whitespace\"\\n\"))))(Secondary((id \
         905a35ca-6dfc-4beb-922c-994ca8c96b31)(content(Comment\"#   \
         running_sum([1, 2, 3]) == [1, 3, 6]        #\"))))(Secondary((id \
         f96a4b53-fc59-4c05-a0ad-02f15f77dd0a)(content(Whitespace\"\\n\"))))(Secondary((id \
         7a92540d-2b3a-4008-ae3e-796e30f800cb)(content(Comment\"#   \
         running_sum([5]) == [5]                    #\"))))(Secondary((id \
         b8fdf8a5-9981-430b-8293-f69d87f9850b)(content(Whitespace\"\\n\"))))(Secondary((id \
         aaa32594-0ea2-4c6f-bb7a-80e4f5a5e6e7)(content(Comment\"#   \
         running_sum([]) == []                      #\"))))(Secondary((id \
         9ca7e44c-ba90-42ca-8a21-1d94395d7e74)(content(Whitespace\"\\n\"))))(Secondary((id \
         82048465-708d-4985-8d44-bef02ffb8cf9)(content(Comment\"#                                              \
         #\"))))(Secondary((id \
         fbdf5b21-454b-4064-9cbe-9b4984d975e6)(content(Whitespace\"\\n\"))))(Secondary((id \
         bf964893-2def-4583-adbf-fea4a093486d)(content(Comment\"# Available \
         functions:                         #\"))))(Secondary((id \
         813841a9-4f96-4c9b-9037-df779197ff23)(content(Whitespace\"\\n\"))))(Secondary((id \
         0a556471-859b-45db-8cd8-25e0e2ba2311)(content(Comment\"#   \
         fold_left(list, fn, init) -> result        #\"))))(Secondary((id \
         94b069d6-c9f5-4a06-92e2-2c94a6ee042b)(content(Whitespace\"\\n\"))))(Secondary((id \
         a4fc33a7-afcb-41e8-959b-a1e435591b8f)(content(Comment\"#     fn takes \
         (accumulator, element)          #\"))))(Secondary((id \
         77be4801-ee88-4ea0-9c28-2213a586bfa9)(content(Whitespace\"\\n\"))))(Secondary((id \
         517ba9cc-7b70-4a99-ac05-52ad981e4515)(content(Comment\"#   \
         append(list1, list2) -> list               #\"))))(Secondary((id \
         a1613638-fc4a-4901-a0c8-c4a9152ccd8e)(content(Whitespace\"\\n\"))))(Secondary((id \
         88b6264a-a4ad-490f-9bcd-94873febd4e1)(content(Comment\"#   rev(list) \
         -> list                          #\"))))(Secondary((id \
         ba837256-3523-4f73-b93e-86c214cc8eb7)(content(Whitespace\"\\n\"))))(Secondary((id \
         ad4a8a44-8da8-4976-8a54-8b8fa90ed235)(content(Comment\"#   map(list, \
         fn) -> list                      #\"))))(Secondary((id \
         4c55d6d1-4a79-4e10-87d1-ba22206d0191)(content(Whitespace\"\\n\"))))(Secondary((id \
         7f7b3fc3-8fbf-4ba8-9008-5d5afc025081)(content(Comment\"#   \
         length(list) -> Int                        #\"))))(Secondary((id \
         d3d40318-9fd2-4d7d-b8dd-932dc219e16f)(content(Whitespace\"\\n\"))))(Secondary((id \
         d3ce602a-7c63-49a6-b12e-deae2f2f16e9)(content(Comment\"#                                              \
         #\"))))(Secondary((id \
         abcdfee2-d6f1-49d3-b93e-ab23bb12f438)(content(Whitespace\"\\n\"))))(Secondary((id \
         fb5f2c9d-d71a-4601-b56d-1687b8b39b9f)(content(Comment\"# Syntax \
         reminders:                            #\"))))(Secondary((id \
         6c963a9c-080b-4371-9bcb-ef99c2326d0e)(content(Whitespace\"\\n\"))))(Secondary((id \
         9472f2a7-8374-4ca5-957c-6e978dab743d)(content(Comment\"#   Tuple: (a, \
         b) = ...                        #\"))))(Secondary((id \
         de817cd5-64c0-48a3-8d15-9e810b431f5a)(content(Whitespace\"\\n\"))))(Secondary((id \
         1dcf4abe-da0e-4691-8844-47595d697ce6)(content(Comment\"#   Tuple \
         access via pattern: let (x, y) = t   #\"))))(Secondary((id \
         9a674aa5-b897-4ecb-a6d6-1a95ac4f35c1)(content(Whitespace\"\\n\"))))(Secondary((id \
         5b1d125c-8078-401b-8129-8725e8a5d70d)(content(Comment\"#   List cons: \
         x::xs, List literal: [1, 2, 3]  #\"))))(Secondary((id \
         d68ee264-102e-4965-8e3c-4de5f1e07fbd)(content(Whitespace\"\\n\"))))(Secondary((id \
         9793c240-c360-416c-95d1-693dfaeae7fa)(content(Comment\"#                                              \
         #\"))))(Secondary((id \
         2967c56d-14c2-4ca3-a6e3-4e4d12bbfcf9)(content(Whitespace\"\\n\"))))(Secondary((id \
         87e96126-7ba7-4617-8526-43ad5d5aeae0)(content(Comment\"# Tip: You may \
         need to track both the running  #\"))))(Secondary((id \
         9ce35eb2-d714-42f9-8218-fd55b246a2fb)(content(Whitespace\"\\n\"))))(Secondary((id \
         624d10ed-b718-49ab-9559-48fb915a4d02)(content(Comment\"# total and \
         the result list in your fold.      #\"))))(Secondary((id \
         b21d44c2-6abd-41c8-8948-5e91183a6d0c)(content(Whitespace\"\\n\"))))(Secondary((id \
         fca1b68f-f0f6-41c5-94b6-c35cd2208978)(content(Whitespace\"\\n\"))))(Tile((id \
         e950edc5-9325-4bd8-8e53-b7a6714da2e6)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         e929aa1c-f704-4ad7-b5d8-c57c96060769)(content(Whitespace\" \
         \"))))(Tile((id \
         b17d859e-2cb7-4dba-96e9-be449e713e96)(label(running_sum))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         096ae3b1-92cf-44c6-baf0-1939b4e2f13d)(content(Whitespace\" \
         \")))))((Secondary((id \
         5d669c26-7795-4623-bb6e-f2fef70c4359)(content(Whitespace\" \
         \"))))(Tile((id 72800d6b-9c94-4590-80c7-4437c0714453)(label(fun \
         ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 37))(sort Exp))))))(shards(0 \
         1))(children(((Secondary((id \
         ad02c203-73f4-4f6c-a550-3c224f7e5a9f)(content(Whitespace\" \
         \"))))(Tile((id \
         11bd3929-f158-4876-bba9-e3c300c24afd)(label(nums))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         7b0943d4-da3a-4fbb-ba93-d16c3bfbf052)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         49e5ca59-2d5e-4c63-9beb-b93f5a1a24fb)(content(Whitespace\"\\n\"))))(Tile((id \
         b88e0e2d-40dd-488d-8ccf-cacf12534cd0)(label(?))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         e868a0c5-2674-4f36-8c9a-4600503711a5)(content(Whitespace\"\\n\"))))(Secondary((id \
         0b0f42bb-da65-4b59-bff5-e35d3c6e9b5b)(content(Whitespace\"\\n\"))))(Secondary((id \
         fc85d74e-be29-4115-9f9f-7cc63b430776)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         0cefb223-de9c-4042-8763-c921907b27b8)(content(Whitespace\"\\n\"))))(Secondary((id \
         535bdd32-95e5-45f8-8bee-1bce6f31877b)(content(Whitespace\"\\n\"))))(Tile((id \
         8c5f09a1-890c-46e8-b524-4068c3316d45)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         9b647d5e-a46b-4d0c-96ae-c9823a805edf)(content(Whitespace\"\\n\"))))(Tile((id \
         4a844e93-9c88-4f56-98ce-3f0ed4855641)(label(running_sum))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         539e1389-6310-4831-8eab-8592a834e938)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         d995df27-3e8f-4f2f-8b97-b836d8ddc63e)(label([ ]))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         70120c71-02e0-4501-bfb0-ed8b34b63bef)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         294eebcb-6bfe-41a1-8f8a-91aed1dec096)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         883d4d50-386a-444b-b0fc-469814f5ed2d)(content(Whitespace\" \
         \"))))(Tile((id \
         3b25ea97-d16c-4194-9924-0c5210db649d)(label(2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         99dfc6a3-daae-4d80-bf9f-7da8ffe8346d)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         f85ce1f3-82c7-4839-987b-50b4caa174a6)(content(Whitespace\" \
         \"))))(Tile((id \
         47e1b34a-8bde-4e94-a02e-ff5fe79b62c5)(label(3))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))))))))))))(Secondary((id \
         5ad5bef4-fae8-4c6f-96ee-b5d06f5b076d)(content(Whitespace\"\\n\"))))(Tile((id \
         ff4fbc83-4e36-43c4-bf15-cfa238fe8bef)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         5c59e217-eae5-4afc-94e9-735483c96e58)(content(Whitespace\" \
         \"))))(Tile((id 102db4a3-4da7-44c9-85e6-4e7f64b65753)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         ecd6b8f2-c831-41ea-b3ca-25e32e2a9d62)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         a22b43d4-6372-4873-9e62-bf81d2a453ea)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         c056784d-2312-428f-bf79-b2749dea7256)(content(Whitespace\" \
         \"))))(Tile((id \
         732d550e-e0b1-499e-94f3-c0c8c464fa5d)(label(3))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         e01ce7b8-d67b-414f-b6d7-d00a51e2ed86)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         1e4c02d6-966f-402f-b087-966c2d75f263)(content(Whitespace\" \
         \"))))(Tile((id \
         b4d7099a-d67d-4e1e-bfb5-762eae74a71d)(label(6))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         48f3b69e-036e-4a37-92b0-bf8e41b3b16b)(content(Whitespace\"\\n\")))))))))(Tile((id \
         6a13e594-ccba-455b-ac33-e0ab1a4ee7fd)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         73b92534-9213-4360-a8cf-8fe099aeb470)(content(Whitespace\"\\n\"))))(Secondary((id \
         541d1569-a7ae-44ea-be79-65fcc21010d2)(content(Whitespace\"\\n\"))))(Tile((id \
         23bae72e-8319-445a-bb31-9335e6dadee5)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         5db59df6-4161-4d1d-be8f-08f7e48e76c4)(content(Whitespace\"\\n\"))))(Tile((id \
         8b03dd13-af00-4ceb-8db8-95076c5f5c67)(label(running_sum))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         c1764cfd-385b-474e-b6c6-ff5a0ce6b153)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         66c36b33-d651-4ec8-981b-fb292dd67139)(label([ ]))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         304e3eff-ab56-4dec-b4ed-20dc558a1d9c)(label(5))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))))))))))))(Secondary((id \
         1320eaa8-39d1-4cef-9001-489fc9541619)(content(Whitespace\"\\n\"))))(Tile((id \
         487943ce-bf7d-4604-97bc-5ac059f20e5f)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         0ab35952-153f-464a-b4b4-e9229aa8cadd)(content(Whitespace\" \
         \"))))(Tile((id 6ea9ca02-179e-42d3-8721-7133f9c922a9)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         f2631c51-6305-4143-8173-feffdfc39f05)(label(5))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         f7663377-fba3-4d6f-93bc-77ae0694bce4)(content(Whitespace\"\\n\")))))))))(Tile((id \
         9dd2993d-70ef-444b-bb99-3373ea89ca83)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         b5e5a700-b611-4120-9e84-846428a69c47)(content(Whitespace\"\\n\"))))(Secondary((id \
         1bd18684-939d-4e94-abf4-36af4949415c)(content(Whitespace\"\\n\"))))(Tile((id \
         c29f33cd-03ee-4766-8d68-2866800ce33c)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         4a6f85aa-a236-4c88-8bf5-190704c8ec37)(content(Whitespace\"\\n\"))))(Tile((id \
         ea856d28-a3a4-4c4a-8665-8939a51f2ee8)(label(running_sum))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         a60c9ebf-92a8-4d01-8d00-26fa70864c80)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         8cfa35e3-8c4d-4035-8e83-ec59e8c14467)(label([]))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         5f0ebc67-7072-4ec7-8119-604d6d9acc3c)(content(Whitespace\"\\n\"))))(Tile((id \
         260f3488-4d9f-440b-814f-84c3b87989ec)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         706c59cf-521c-48f9-8886-f728034420e6)(content(Whitespace\" \
         \"))))(Tile((id \
         3faa312c-0451-4ec7-894f-98d72126ae67)(label([]))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         5c8c4aec-9cf3-43b1-ac4e-6663cb7a25f4)(content(Whitespace\"\\n\")))))))))(Tile((id \
         1166f3e8-07f0-40cb-8052-e4f0d40d3a74)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         1753072f-bea9-4eb3-b376-657103096348)(content(Whitespace\"\\n\"))))(Secondary((id \
         0bc13506-3890-46da-be53-a701f1f87140)(content(Whitespace\"\\n\"))))(Tile((id \
         1f317b47-790d-4067-a1e6-60aaaed0d527)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         176d49d1-25a7-493b-8a26-1eaa304381e0)(content(Whitespace\"\\n\"))))(Tile((id \
         55112876-afbe-4174-9c38-ccc9f8c3d2be)(label(running_sum))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         2b917c2f-8646-4e1b-b679-d00ac22c3d3d)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         ddccf9bd-5e22-4149-b084-0d59d4d4aa6a)(label([ ]))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         814fb18d-5568-4bf2-8795-89cad1ce4885)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         27a80b30-177f-4476-ab52-f336dfee4870)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         0bd2c61f-ed8c-45b8-8ddb-25e9ef0d87a1)(content(Whitespace\" \
         \"))))(Tile((id \
         eaf27cb5-1055-473f-a366-f25137d30996)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         00f9f322-104e-4a35-ad16-b9cd9e3ef91c)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         b0b28d55-8255-4833-812f-960ec01b22cd)(content(Whitespace\" \
         \"))))(Tile((id \
         0113ec5a-4fad-4721-8917-a4d982c87979)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         54650eeb-1ead-45df-b510-15bac27a04cc)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         02c54f2a-a0d0-4be1-ab86-7c2b1a634fcb)(content(Whitespace\" \
         \"))))(Tile((id \
         7f82ec8b-c553-45b9-95db-ba7184c93527)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))))))))))))(Secondary((id \
         794fddb6-80b9-4173-8889-ce1bf35ddaf5)(content(Whitespace\"\\n\"))))(Tile((id \
         890341b3-d9fa-4309-8b08-00436537c467)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         ca33a887-c8a8-46e6-86b8-6a1bcaa5bd35)(content(Whitespace\" \
         \"))))(Tile((id 8fd978b6-1a69-4e83-b755-9e24ea48b12b)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         56c98c20-c326-4e58-ae1c-b5d72e322e99)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         f2823662-52fc-4e63-b8b9-b15ba264c55b)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         0b623ff4-ddb8-47e0-8bb7-6a2667d004f3)(content(Whitespace\" \
         \"))))(Tile((id \
         23bdfb38-a8d5-432c-a4ff-709126ffe8c5)(label(2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         20b2dc38-5d4a-410e-96a9-8a872eaed116)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         521d78e8-783f-4be5-a05e-26b8cdeff9cc)(content(Whitespace\" \
         \"))))(Tile((id \
         24173765-6db4-4f60-b2fc-9ca3e9b4fc4e)(label(3))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         0d04e2a6-d2df-48a0-af3e-5c3e7a2a3bd9)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         6c9e0118-0c1e-4d25-810d-cc0b44ccd28c)(content(Whitespace\" \
         \"))))(Tile((id \
         3de08885-c7fc-49d2-a405-acd0cdaf5303)(label(4))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         00482309-fc5b-4bc9-8cca-682b7e52fcc8)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         e0aeb88f-b240-42e3-9539-8ba76ddce66c)(content(Whitespace\"\\n\")))))";
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
