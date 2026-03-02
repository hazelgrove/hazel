let out : string * Haz3lcore.PersistentSegment.t =
  ( "Study / writing / running-sum / running-sum-solution",
    {
      segment =
        "((Secondary((id \
         a5ff42ba-7754-4b44-b174-7090dfbf0e21)(content(Comment\"# RUNNING SUM \
         - SOLUTION #\"))))(Secondary((id \
         ee7331df-70da-4f96-ac33-0c3ba4ac114c)(content(Whitespace\"\\n\"))))(Secondary((id \
         b48a76bd-7d57-42bc-8680-ba34fe2a3d1a)(content(Whitespace\"\\n\"))))(Secondary((id \
         4447acac-c0e1-4c2b-bfe4-3a227aedbfec)(content(Comment\"# Uses \
         fold_left with a tuple accumulator:         #\"))))(Secondary((id \
         841b00c1-6cbb-4d8a-8e52-7ac0055af004)(content(Whitespace\"\\n\"))))(Secondary((id \
         763e4785-139f-49fd-8b99-45150b267890)(content(Comment\"# \
         (running_total, result_list_so_far)              \
         #\"))))(Secondary((id \
         6a535f1f-09e5-4df1-96c0-065eee848156)(content(Whitespace\"\\n\"))))(Secondary((id \
         244fc376-1400-4a22-86cd-b088313c1532)(content(Comment\"# On each \
         step, add current element to total,      #\"))))(Secondary((id \
         6d19f6b6-846d-4379-bcc9-cc8dc4bd707d)(content(Whitespace\"\\n\"))))(Secondary((id \
         f5ee6451-1e77-4d96-a9f8-b8b3e8338d85)(content(Comment\"# append new \
         total to result list.                 #\"))))(Secondary((id \
         1b9b64f9-ac37-4074-bcaa-f25afe3ad7e1)(content(Whitespace\"\\n\"))))(Secondary((id \
         592995bd-dc3f-4313-891b-86827dbe4384)(content(Whitespace\"\\n\"))))(Tile((id \
         a4e5fd12-6cba-4d03-b41c-78e3c4d9189d)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         db255fee-fe51-4ca9-b5f7-fc8f23cd26b5)(content(Whitespace\" \
         \"))))(Tile((id \
         e37f199e-9730-4efc-8ffb-bfebc9d265c7)(label(running_sum))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         52f45e40-1739-4f98-a25b-5c555570531f)(content(Whitespace\" \
         \")))))((Secondary((id \
         a17ba03f-48c0-4e84-989d-c853331c9921)(content(Whitespace\" \
         \"))))(Tile((id 6b4dd2ab-bdd4-4f75-b6ba-fc3be67dab21)(label(fun \
         ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 37))(sort Exp))))))(shards(0 \
         1))(children(((Secondary((id \
         4a2b8f63-2066-4404-8686-5ab889a6cce3)(content(Whitespace\" \
         \"))))(Tile((id \
         0a8d350a-4a08-4275-bc48-f93edf8e6f7f)(label(nums))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         492662f7-e44d-4f14-a4cd-79536c07d93d)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         950ef4eb-05c7-455e-aa28-f7642ca82bd1)(content(Whitespace\"\\n\"))))(Tile((id \
         1080a42c-27ab-4043-9bdb-58d6976d4088)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         f7e5f706-98ca-4a11-83ef-3439fac65a79)(content(Whitespace\" \
         \"))))(Tile((id \
         ed9a1398-111e-4031-aa40-b664718a3e01)(label(\"(\"\")\"))(mold((out \
         Pat)(in_(Pat))(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0 1))(children(((Tile((id \
         e536e461-a538-4ef7-8341-49820f3ca4bc)(label(_))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         cd3a7631-b2a4-4881-b19d-86164165fe82)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 44))(sort Pat))((shape(Concave \
         44))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         4c2a388e-7bb2-4759-af31-a4faf8c523e1)(content(Whitespace\" \
         \"))))(Tile((id \
         99d9000b-221a-4568-8667-11a03ff731f7)(label(result))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children()))))))))(Secondary((id \
         af696b52-82da-4dde-9acf-1e9e89ef2253)(content(Whitespace\" \
         \")))))((Secondary((id \
         8a813ccc-7bd1-4cbd-b4fb-e25b30a68454)(content(Whitespace\" \
         \"))))(Tile((id \
         989a4ada-5c37-4ab2-b4f1-e4c88fc0f92a)(label(fold_left))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         35adce3f-7368-4059-a151-7e062e629bfd)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         bac156fa-3e79-4a4e-a6c2-06a0bfea13bc)(label(nums))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         5170744c-7474-4146-b39f-4177ed492a36)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         9b924efb-2550-45f2-8347-1b548b389c3e)(content(Whitespace\"\\n\"))))(Tile((id \
         025e4389-c9b5-4a1f-b8eb-5369330eeb30)(label(fun ->))(mold((out \
         Exp)(in_(Pat))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         37))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         bad2527e-f016-48b8-8e15-3180333329cb)(content(Whitespace\" \
         \"))))(Tile((id \
         42122c90-abce-4fd6-a3fd-b0a8597e5cab)(label(\"(\"\")\"))(mold((out \
         Pat)(in_(Pat))(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0 1))(children(((Tile((id \
         56d43c29-7e40-4f91-b5d4-1239008046c9)(label(\"(\"\")\"))(mold((out \
         Pat)(in_(Pat))(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0 1))(children(((Tile((id \
         14d9ea1b-a805-4287-9c51-96935bbcff58)(label(total))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         9fc16c75-d155-4d89-ab80-52af20ab67c8)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 44))(sort Pat))((shape(Concave \
         44))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         546a4040-4ff0-4b77-9bbf-cb84d89c77e0)(content(Whitespace\" \
         \"))))(Tile((id \
         3aa2b51f-c5b9-4b2d-97d5-8ea0f628406e)(label(acc))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children()))))))))(Tile((id \
         c83fbebf-a260-4984-a83b-21fcf85f464f)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 44))(sort Pat))((shape(Concave \
         44))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         9f5c1365-c498-4583-b9db-2864af58b015)(content(Whitespace\" \
         \"))))(Tile((id \
         51b8ac02-9d3c-4146-8c86-39a3bf4ac22a)(label(x))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children()))))))))(Secondary((id \
         1e098756-4a73-4bf4-adce-34e9465c30af)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         a0121347-afb3-4b19-865e-e5a0baada824)(content(Whitespace\" \
         \"))))(Tile((id \
         9ceb3154-a783-4768-9cba-efee8db53e6b)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         b658e063-e7cd-4783-b85a-ba4b7f2cdd8c)(content(Whitespace\"\\n\"))))(Tile((id \
         f7ca4e3c-a15d-42b3-86af-ab13a642a6e5)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         3d7a81d2-871c-4106-a2c5-6f2b6f758d1e)(content(Whitespace\" \
         \"))))(Tile((id \
         3c4fe5dc-448d-4dd3-b0a3-d24bab5487e1)(label(new_total))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         7c4ade9f-4e81-4832-be1f-de8c043ce68f)(content(Whitespace\" \
         \")))))((Secondary((id \
         90a552fd-64d8-4957-8bfe-c5cc0ccbe97e)(content(Whitespace\" \
         \"))))(Tile((id \
         2a4814a9-061c-42fb-85ba-6cbc48fd02bf)(label(total))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         74c9007c-bce4-4dbd-b235-60efa42994fc)(content(Whitespace\" \
         \"))))(Tile((id \
         36c1e271-bc28-4ebb-8308-db44d4b54858)(label(+))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 28))(sort Exp))((shape(Concave \
         28))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         330ec9d8-2933-4ddb-86ec-588ea8a4673e)(content(Whitespace\" \
         \"))))(Tile((id \
         6440acf9-21f7-40a0-b59b-8102205c8a47)(label(x))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         e1cfbe59-addb-481d-a5fe-691769316ab9)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         da203746-d81f-4ff5-8b9c-8532fa2e4645)(content(Whitespace\"\\n\"))))(Tile((id \
         9864a815-a888-417f-9382-8239f90cd0c4)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         1cf3fecf-5f8d-4eff-8194-76bc85ca2a91)(label(new_total))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         82eeaa5a-207c-4c36-8886-960b417a4f24)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         016aacfd-116d-4700-b9f2-ca495680ceb6)(content(Whitespace\" \
         \"))))(Tile((id \
         d95ed804-d1ba-4fd3-89a3-14467c2f1f70)(label(append))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         1fb43fc0-18f1-4e59-9559-e2c49cde596c)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         f567e15e-6d79-491f-8193-23f09d3c6ff6)(label(acc))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         a05e3bfc-9d88-4c0d-a6c7-0945b25e0516)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         153f6921-cf34-4084-9975-c041fe535d8d)(content(Whitespace\" \
         \"))))(Tile((id d191408c-197e-4c31-9a0e-811213c10f4c)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         fad33255-daf4-4de0-82b3-b177a89a5dd4)(label(new_total))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))))))))))))))))))))))(Tile((id \
         636b5220-b5e9-4dcb-b66a-e99210326311)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         6f84b320-a7e2-40f6-b4d5-6bf461fe7ac9)(content(Whitespace\"\\n\"))))(Tile((id \
         c0c0e453-a5b9-481e-85d9-de137fbc77a6)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         931db899-cb56-400a-ae74-419fe7365631)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         61958137-0962-4d17-a9d4-efef3f588a76)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         2e8d7999-bb51-4f16-acbf-1fee5535c3d2)(content(Whitespace\" \
         \"))))(Tile((id \
         4d60d93a-780d-489e-9b26-f46063e6c313)(label([]))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         823bf7a1-e3f2-4338-9ec5-d42aea4d1b8e)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         b660c79b-c50b-4e04-b79e-86f4ccd78d0d)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         75bea0d9-4c23-47de-a504-b9cc5722c8dd)(content(Whitespace\"\\n\"))))(Tile((id \
         cabf1748-9ad1-4e1a-bc83-c199c1beb90a)(label(result))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         280d3600-546e-4309-ac26-a9878d984fdc)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         89b1d929-12db-48e4-b523-70b85742b302)(content(Whitespace\"\\n\"))))(Secondary((id \
         ea914161-3741-416b-b281-ec8074e947fc)(content(Whitespace\"\\n\"))))(Tile((id \
         436bb7ba-508e-4f1a-98a3-7b05cd5a397d)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         fd367f3c-c50b-4b5f-aadb-086a3661b561)(content(Whitespace\"\\n\"))))(Tile((id \
         72e00927-0921-4b60-92ae-73bfe01de3d4)(label(running_sum))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         44a9d148-345f-4cbc-9949-95f734d9f46c)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         9b005d1e-ec1c-4508-a373-d207d88e0a66)(label([ ]))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         f1e9e48a-6f58-4aeb-bdae-355f3639010d)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         f720ca85-7e77-4b37-b9a3-0936fc032ae5)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         125e7490-3995-4820-b544-09f9cddb586f)(content(Whitespace\" \
         \"))))(Tile((id \
         432a0d40-d8c2-4611-8c21-a5a39034090e)(label(2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         58b31403-cf00-4edd-94e4-be2e6c7effe7)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         5fbaff47-8cbb-4066-a230-7e283462022d)(content(Whitespace\" \
         \"))))(Tile((id \
         36292af4-7bae-4d2e-a0bf-7425746313b9)(label(3))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))))))))))))(Secondary((id \
         8ed1330b-77b3-4fc4-9480-24f0a3946eca)(content(Whitespace\"\\n\"))))(Tile((id \
         bfe62c88-f0bb-4886-b5c3-484e24b740e0)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         8ad696e6-ff0b-46bf-bfff-9a2359e9cfea)(content(Whitespace\" \
         \"))))(Tile((id e76b4713-9319-4abf-a5b1-92f9026386d9)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         af913673-78f9-44c6-b074-b95bc0953d2c)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         b3d72eeb-58d5-4f01-9814-2529c869308b)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         6376153e-f91b-4b41-adde-8fa6fc796bad)(content(Whitespace\" \
         \"))))(Tile((id \
         3865a2d3-21b9-4f76-b35e-e35fd9b44e01)(label(3))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         ca5e45ed-51bb-4312-b8a8-8ac1937d4b26)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         de08c421-ed6e-4c07-84b3-afdb40c6d94a)(content(Whitespace\" \
         \"))))(Tile((id \
         e9632430-d93f-490a-99f8-179a1b38650c)(label(6))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         b6a6dd22-8757-4f3e-b37c-1e66dd17db6f)(content(Whitespace\"\\n\")))))))))(Tile((id \
         7f96f997-59ce-4839-a390-b0d651c4a879)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         3ec5a6c2-1fc6-422e-8595-a3e466ce89fa)(content(Whitespace\"\\n\"))))(Secondary((id \
         ec22f2b4-0eb1-466d-b8dc-9fbb858244f1)(content(Whitespace\"\\n\"))))(Tile((id \
         3041d745-40c1-49cc-a19a-18064d595b8c)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         9e69cb93-97d0-4f0f-8d03-d7ad3bcad166)(content(Whitespace\"\\n\"))))(Tile((id \
         7b9da09a-4fcb-4bd2-8969-e00a8028cde2)(label(running_sum))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         4084e41c-853e-4087-ab45-f46f362c87bb)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         dcf378e6-2029-499d-ba72-aa88658e2fc4)(label([ ]))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         10e1ac61-d690-4027-b90f-d288d1f2fc0b)(label(5))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))))))))))))(Secondary((id \
         67ae6848-984c-42d6-8854-05b7d26ff015)(content(Whitespace\"\\n\"))))(Tile((id \
         d087c7d2-a46a-4769-8fa2-219885ced77e)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         3d861a8b-da15-4aaa-bfa5-bb7a191a9206)(content(Whitespace\" \
         \"))))(Tile((id 09e50e44-8e09-453f-83d4-4ad882c0d8f1)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         bdb73c58-bbb3-4855-ad0c-aa0851a55881)(label(5))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         77f9e6b2-3726-4fb7-88f6-5b6b1f95c53a)(content(Whitespace\"\\n\")))))))))(Tile((id \
         f63e77bc-37bf-4a5a-929d-b9d06fa83c49)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         8f3baa87-403f-41e6-9b1d-72c0d413edfa)(content(Whitespace\"\\n\"))))(Secondary((id \
         f9439aee-5c51-4970-bb4f-287debdc3b15)(content(Whitespace\"\\n\"))))(Tile((id \
         92a3cc13-9149-43a9-8179-535fce83f681)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         b2262fe9-7d1d-4630-b855-a48edeb60cc3)(content(Whitespace\"\\n\"))))(Tile((id \
         7bb5c88d-13d1-4db0-be22-ba091ccae175)(label(running_sum))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         19d1187a-ae9b-4109-94ca-a4bba9bcaaa3)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         2c2f319d-8a86-4bda-92a1-8c6328f57302)(label([]))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         608efedb-8e16-43f8-91c8-3354d3de8540)(content(Whitespace\"\\n\"))))(Tile((id \
         f42505e3-0421-41b8-ac7e-a92691ec3347)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         77ccc3c7-0ec3-4b5c-902c-1f66c84f80fe)(content(Whitespace\" \
         \"))))(Tile((id \
         01bcd065-881a-4b64-b5fe-ebf31cb60534)(label([]))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         518c1cff-9bd3-49fc-b89d-aff6c96c7de2)(content(Whitespace\"\\n\")))))))))(Tile((id \
         7e355cc2-97ba-40ee-9c93-c0f189ba4e28)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         71499da7-7c10-4b3b-947c-32db5db0172d)(content(Whitespace\"\\n\"))))(Secondary((id \
         7ecef74c-d480-4abb-84e1-68775a5b13bb)(content(Whitespace\"\\n\"))))(Tile((id \
         83e628f6-f74c-46e0-988c-09880e4dd54c)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         984f5fe7-3dad-44b0-9dfe-190ad911dce1)(content(Whitespace\"\\n\"))))(Tile((id \
         4a12b887-973e-4b95-88a3-7a468b6f78ce)(label(running_sum))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         39f9cb5b-a715-45df-8934-8b4a0eb3392a)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         d0b53d2d-f1ad-4c0f-837c-88134aac2e3d)(label([ ]))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         d600e292-f529-485d-b067-9ae85853ecc6)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         6245be09-0d3b-405b-8370-085d21bf44f9)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         2626ef7d-9bdb-4a49-bfd6-834806b8af69)(content(Whitespace\" \
         \"))))(Tile((id \
         6e7ccb29-1cd2-4bb7-abe1-29459884cec7)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         8dfba3da-eeca-474b-969b-f0eb5b99a9e0)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         bc5fccb8-daea-4cc9-91c6-ea87eb034686)(content(Whitespace\" \
         \"))))(Tile((id \
         59ffe92b-a7d6-4396-a382-4206d6260eb7)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         62b94302-41dc-4ff5-9112-037af5285cd2)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         de4aa163-cd0b-4748-98f6-348c9fc540b7)(content(Whitespace\" \
         \"))))(Tile((id \
         1eabf49c-250a-4d60-b4f1-ade66d1bd389)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))))))))))))(Secondary((id \
         0b12249a-1d75-44fd-9df2-7226c7c2523f)(content(Whitespace\"\\n\"))))(Tile((id \
         ed584dcc-daa4-4fb0-b9d3-e264fc35b82c)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         48263478-ec79-49b1-b5b7-13f015862d7e)(content(Whitespace\" \
         \"))))(Tile((id 11ec682e-b15e-41ea-9456-f5ab754a1b92)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         c24ae2bf-4d7d-4fff-a704-5256b65f0d80)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         cae926ea-c0b6-444d-8c2c-6d5e8c412729)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         6e411ce1-b128-4d5f-b51d-e575b30648cb)(content(Whitespace\" \
         \"))))(Tile((id \
         dc4f8c74-5a31-4f47-9ba1-6b2917bfd69d)(label(2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         571f813f-3160-44c5-92c3-5afd058d82bc)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         d7c307ff-5e53-411c-84fb-87c4fc62d1f7)(content(Whitespace\" \
         \"))))(Tile((id \
         ca80e3f6-5a16-417e-a303-54112b63ef33)(label(3))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         227f30b8-712e-40eb-b795-200d8e141b09)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         20969433-0323-4658-b925-bcd7ad5e4f35)(content(Whitespace\" \
         \"))))(Tile((id \
         11d1bcf3-4cb7-4e48-b340-456cda9b3a02)(label(4))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         38a1fd40-7b7a-4393-af67-b73141e58e9b)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         d35e89ba-a1ab-41fd-9262-c0f86c0ffb63)(content(Whitespace\"\\n\")))))";
      backup_text =
        "# RUNNING SUM - SOLUTION #\n\n\
         # Uses fold_left with a tuple accumulator:         #\n\
         # (running_total, result_list_so_far)              #\n\
         # On each step, add current element to total,      #\n\
         # append new total to result list.                 #\n\n\
         let running_sum = fun nums ->\n\
         let (_, result) = fold_left(nums,\n\
         fun ((total, acc), x) -> (\n\
         let new_total = total + x in\n\
         (new_total, append(acc, [new_total]))),\n\
         (0, [])\n\
         ) in\n\
         result\n\
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
