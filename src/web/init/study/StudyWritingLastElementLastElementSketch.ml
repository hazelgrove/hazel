let out : string * Haz3lcore.PersistentSegment.t =
  ( "Study / writing / last-element / last-element-sketch",
    {
      segment =
        "((Secondary((id \
         dede6717-5d39-4a0d-9b67-795f497d7d30)(content(Comment\"# LAST ELEMENT \
         TASK                            #\"))))(Secondary((id \
         64eb1bf7-80b3-468e-afdc-95451916eb19)(content(Whitespace\"\\n\"))))(Secondary((id \
         00d85ad2-6071-409e-bd51-19d0e72b7e8b)(content(Comment\"#                                              \
         #\"))))(Secondary((id \
         c6d2ea6a-7d97-4ac3-9b76-e68afe027427)(content(Whitespace\"\\n\"))))(Secondary((id \
         54f36a5d-a47a-445d-b23d-95c05ec2b1c5)(content(Comment\"# Implement \
         last: get the last element of a    #\"))))(Secondary((id \
         21553ccc-b37b-4644-a30d-386598def895)(content(Whitespace\"\\n\"))))(Secondary((id \
         83a601b2-6014-453a-befb-7fb64fcab48a)(content(Comment\"# list, or \
         return a default if empty.          #\"))))(Secondary((id \
         6f1a8081-71e5-447b-a614-6209178e149b)(content(Whitespace\"\\n\"))))(Secondary((id \
         1c2eea09-caee-4636-a5dc-48fb9c48fb9a)(content(Comment\"#                                              \
         #\"))))(Secondary((id \
         7f75a268-ff4f-4578-8b53-e73d69bf6a62)(content(Whitespace\"\\n\"))))(Secondary((id \
         cf979470-0fc8-4566-b4c2-53d4bb9e48e0)(content(Comment\"# \
         Examples:                                    #\"))))(Secondary((id \
         c48ff708-bf8b-4ac9-b500-ae2c760d8020)(content(Whitespace\"\\n\"))))(Secondary((id \
         dd166b75-fe3b-4be3-be3f-55f62d9f4c89)(content(Comment\"#   last([1, \
         2, 3], 0) == 3                    #\"))))(Secondary((id \
         2c6b743d-0097-4754-87ef-682f87d2924a)(content(Whitespace\"\\n\"))))(Secondary((id \
         6ec46cc6-4133-4e06-9821-b6ca0b96e69d)(content(Comment\"#   last([42], \
         0) == 42                        #\"))))(Secondary((id \
         2bd15df7-768d-455f-91f0-c0398ca991e8)(content(Whitespace\"\\n\"))))(Secondary((id \
         9c69f8de-967b-415d-b876-df74f2862404)(content(Comment\"#   last([], \
         99) == 99                         #\"))))(Secondary((id \
         7b5c7874-87eb-4c97-882f-9c7a8f44cfdc)(content(Whitespace\"\\n\"))))(Secondary((id \
         ea4668cf-73e6-4684-8c29-e3b7aa13b4f3)(content(Comment\"#                                              \
         #\"))))(Secondary((id \
         1eed3155-a5b7-4344-9c4b-e801bb24aa03)(content(Whitespace\"\\n\"))))(Secondary((id \
         623f6e51-d6fd-4f81-8829-ac57ee5e2640)(content(Comment\"# Available \
         functions:                         #\"))))(Secondary((id \
         78191489-8846-416c-a299-f08857e1cbbb)(content(Whitespace\"\\n\"))))(Secondary((id \
         8381a798-b9b7-4c77-b2b3-341f79c76139)(content(Comment\"#   \
         fold_left(list, fn, init) -> result        #\"))))(Secondary((id \
         5862f59f-ae9b-4429-9138-3afa150f3df6)(content(Whitespace\"\\n\"))))(Secondary((id \
         b0aac52d-c674-401d-947d-217631e53bb8)(content(Comment\"#     fn takes \
         (accumulator, element)          #\"))))(Secondary((id \
         a69caa5d-18c2-44b1-bb38-1c52925a0be7)(content(Whitespace\"\\n\"))))(Secondary((id \
         033ef581-7770-4af8-8688-c59ab27b6b47)(content(Comment\"#   \
         fold_right(list, fn, init) -> result       #\"))))(Secondary((id \
         8e30bfec-fda6-42ad-8040-daaf4ef36fa4)(content(Whitespace\"\\n\"))))(Secondary((id \
         930759bd-c594-40a6-a0e5-133ba689ea65)(content(Comment\"#     fn takes \
         (element, accumulator)          #\"))))(Secondary((id \
         c3d13ed5-0555-4c24-be82-ad773ac04a32)(content(Whitespace\"\\n\"))))(Secondary((id \
         29b7487a-5100-4ce7-be2c-92f06fba3ab8)(content(Comment\"#   rev(list) \
         -> list                          #\"))))(Secondary((id \
         ed341853-1810-405d-95a0-c314cd282b10)(content(Whitespace\"\\n\"))))(Secondary((id \
         79304728-1621-4c89-b152-503ee20ab0a6)(content(Comment\"#   \
         length(list) -> Int                        #\"))))(Secondary((id \
         24b9efcf-2124-489e-90b0-7e5e230fa568)(content(Whitespace\"\\n\"))))(Secondary((id \
         89728dd2-ce83-4a8d-9858-a7eb9b0f8cf3)(content(Comment\"#                                              \
         #\"))))(Secondary((id \
         df4c9afe-647a-4ee3-9541-466a269865c2)(content(Whitespace\"\\n\"))))(Secondary((id \
         7f81b57e-c5b9-4433-8ee7-eb688f1a2632)(content(Comment\"# Tip: Think \
         about what the fold should        #\"))))(Secondary((id \
         94fbda5a-1da4-40df-a1ac-c6dfc9826747)(content(Whitespace\"\\n\"))))(Secondary((id \
         4d8441d8-3b5c-4b44-b0e2-008f2558ef78)(content(Comment\"# \
         \\\"remember\\\" as it processes each element.     \
         #\"))))(Secondary((id \
         c1cef845-ebf8-4910-9e19-378da6994b20)(content(Whitespace\"\\n\"))))(Secondary((id \
         2929c2d2-7224-49f9-b699-9178f4ecb5fa)(content(Whitespace\"\\n\"))))(Tile((id \
         07fb48ba-ac8b-4c75-8988-6d9d5536171b)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         ae5c9117-e739-460d-afa5-1934f8f17bc4)(content(Whitespace\" \
         \"))))(Tile((id \
         87f69c8d-2c04-459d-8a32-2abfffbed92b)(label(last))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         db36fc8c-7798-4497-bc87-ea17eae146c5)(content(Whitespace\" \
         \")))))((Secondary((id \
         aeea5b69-1385-4450-ab32-782451ec36b8)(content(Whitespace\" \
         \"))))(Tile((id 32c3e777-ff81-4fff-9e18-b4454ce31f1c)(label(fun \
         ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 37))(sort Exp))))))(shards(0 \
         1))(children(((Secondary((id \
         7a9570be-d0a2-40dc-9769-1f48019b9cb6)(content(Whitespace\" \
         \"))))(Tile((id \
         0bb4a468-0459-4864-9140-fba0ba467109)(label(\"(\"\")\"))(mold((out \
         Pat)(in_(Pat))(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0 1))(children(((Tile((id \
         8be64f1e-6c06-41dc-9355-cd03bfcba593)(label(xs))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         07a11484-e040-4811-a4b8-48b3676db571)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 44))(sort Pat))((shape(Concave \
         44))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         34fdf388-6c51-4a9b-ad52-761d7fe9befd)(content(Whitespace\" \
         \"))))(Tile((id \
         d48d1873-1b9f-4f59-9f49-c788511a5efd)(label(default))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children()))))))))(Secondary((id \
         548f8e4c-08e7-45b7-9a48-32c7100cfe9b)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         e5372de3-e22f-4316-ab8d-78d01af262d1)(content(Whitespace\"\\n\"))))(Tile((id \
         06675f88-29a8-4781-8dc6-7aabc1aceee8)(label(?))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         6be7ccd4-0ebb-46c6-9266-2fbbab7d09c2)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         a544addb-449b-41bb-886f-a9aa09acb721)(content(Whitespace\"\\n\"))))(Secondary((id \
         caa11667-cc0b-43fe-8432-cc29d40e5a33)(content(Whitespace\"\\n\"))))(Tile((id \
         ae3023c9-d560-4ce3-8026-a58b80894cd7)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         453b281c-3491-4b06-a1eb-1598c526a850)(content(Whitespace\"\\n\"))))(Tile((id \
         57e64d68-e2c5-4d54-8751-1daf686dcbfa)(label(last))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         30f2de26-5a41-4725-9de3-0051fb1e7945)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         5c8cac4d-cf6d-4917-b01f-1fcca11b2561)(label([ ]))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         9211aefa-778f-4d5d-8205-817fc2e5f00d)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         68d7bb74-016f-42fc-af12-fe6901981c52)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         01ddf534-8f1d-4404-ab41-c8b6cc4e98f8)(content(Whitespace\" \
         \"))))(Tile((id \
         ad4f62cd-17b7-45ac-b2e4-317cfc5e6d6c)(label(2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         e341ba5a-e4b7-4429-8514-f1a450b526b0)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         a2686e2e-2238-448b-8787-bb01d32825cc)(content(Whitespace\" \
         \"))))(Tile((id \
         801738e7-b715-4c95-98d0-9146553c3afa)(label(3))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         a693b790-4a74-4684-bee3-57418589eada)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         0fbca5bb-cffa-4b28-b1fe-f5d9ca5289de)(content(Whitespace\" \
         \"))))(Tile((id \
         3f1ca251-b1bd-4d14-8978-25aac9bd3b73)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         a0f4a86a-ca50-4182-904e-086da5e8db20)(content(Whitespace\"\\n\"))))(Tile((id \
         384cfd0d-15c8-4c57-8f7e-bbc1fdeaa5ed)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         01254c87-0fb5-469c-b00c-3adc42fc0853)(content(Whitespace\" \
         \"))))(Tile((id \
         6522fc7c-12c1-4ab1-9d79-2f8e0b1ed5b6)(label(3))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         b6286455-ca04-4ccd-8f71-dc49c661c1fd)(content(Whitespace\"\\n\")))))))))(Tile((id \
         f130a641-f02a-469f-9f2c-815d95246f5b)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         7e633d72-198c-48c8-bf53-303c93d115b0)(content(Whitespace\"\\n\"))))(Secondary((id \
         20ee93af-70f9-49e0-9799-39d6888bdd20)(content(Whitespace\"\\n\"))))(Tile((id \
         d7e96d45-dc87-402d-a383-1d02c3169f2a)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         bf8a937e-8a60-4259-aa63-8e88db32f725)(content(Whitespace\"\\n\"))))(Tile((id \
         e86990cb-a1f6-4376-a8ef-1c58d99b560f)(label(last))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         0b9e92a0-4ad6-4289-bc24-78935dbb695f)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         de7dbc40-ea39-4381-9f25-15f64138ee1c)(label([ ]))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         3c5e6627-44ab-4a57-ac69-a4947c07a829)(label(42))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         a69fe64f-4681-4677-a274-5cf8a7c4300f)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         9293e476-2cd9-4aa8-9409-9ed93a2214b6)(content(Whitespace\" \
         \"))))(Tile((id \
         b93a7ecd-680a-46a1-9d45-fbb1cb32bbf8)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         7f5c54f3-c331-4dd7-87f4-fd11f9964a07)(content(Whitespace\"\\n\"))))(Tile((id \
         d927a0e4-ab97-4316-98d1-1033c8f8c17e)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         ceb0bae5-a8c3-47a9-a8d3-26e72314b04c)(content(Whitespace\" \
         \"))))(Tile((id \
         0154904e-36b6-455b-a692-fd3b021db384)(label(42))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         c8604104-be2e-4292-885e-797b239502e9)(content(Whitespace\"\\n\")))))))))(Tile((id \
         672025c8-4801-4d41-ae11-d714cb836638)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         3bc89829-b3f6-4f40-a14d-00b01d387402)(content(Whitespace\"\\n\"))))(Secondary((id \
         3d52cc36-7bfd-407c-806f-7503e4a85654)(content(Whitespace\"\\n\"))))(Tile((id \
         1a6826f4-055e-443e-939a-9f10f67f1723)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         82be391b-ece7-4f58-9f31-8833c69ee11a)(content(Whitespace\"\\n\"))))(Tile((id \
         1701b4ce-b732-4cc9-8a0c-70a21dc1ffaa)(label(last))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         9519497a-56e4-47a5-9681-5e7326c16a03)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         d160231a-0caf-4b45-88a8-c9ba7f6f67d6)(label([ ]))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         ab6fb3e0-d1f5-4e87-a76a-77d84ed7b263)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         0450b69d-7481-4bcb-81d0-bf5e7a8e92d1)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         fbc763fd-2136-4a81-8ce0-98e96d09cf6e)(content(Whitespace\" \
         \"))))(Tile((id \
         06b12303-9016-46f6-a0ef-5607bf07df2e)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         9ec9680d-bf62-40aa-b745-d867a3f4e373)(content(Whitespace\"\\n\"))))(Tile((id \
         69154f31-cf76-48f5-80f2-8f14a652b8be)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         4805d547-5d8a-40f3-a5e2-fb0204efa878)(content(Whitespace\" \
         \"))))(Tile((id \
         18124019-26df-4ab2-9370-818f3a81dbe0)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         4987f496-b908-4436-94d4-e64bee3017fb)(content(Whitespace\"\\n\")))))))))(Tile((id \
         8126a0e3-904b-4c55-9283-c9f410ab9f24)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         79da78d4-bec3-4447-8b79-e373839a93ac)(content(Whitespace\"\\n\"))))(Secondary((id \
         2e386b6f-5fbb-4ca1-8a29-0c1d251834d4)(content(Whitespace\"\\n\"))))(Tile((id \
         ea00778a-c9a7-4d34-92ff-a894ba1717f0)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         ed12ac3e-9e8e-4e62-81a7-e4db68faba4a)(content(Whitespace\"\\n\"))))(Tile((id \
         ae6b5224-7454-447d-ae5e-9bbd32b97dc1)(label(last))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         5197ee89-eb35-4a85-9bc6-92eee4dde072)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         d11e1a92-badb-402b-90c7-11434503681f)(label([]))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         c82fe761-640d-4bab-9933-f6af1fe16bcc)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         f45ff17c-dc17-4d90-9fcf-3e89bd837c62)(content(Whitespace\" \
         \"))))(Tile((id \
         cd85e0fa-418b-4c7a-8d9a-43789b2e7ddc)(label(99))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         1a90e772-cb23-4294-ba30-00528f8721ec)(content(Whitespace\"\\n\"))))(Tile((id \
         f7ebf14c-d004-4563-bdd7-512fcea7014e)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         e09e506d-e85e-48ed-8642-9a10eee8c3ad)(content(Whitespace\" \
         \"))))(Tile((id \
         00d70656-210a-479f-972a-ea54cb967634)(label(99))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         69d976a0-68c6-4f41-865d-74619cb9e357)(content(Whitespace\"\\n\")))))))))(Tile((id \
         df65795e-89e4-4535-acb2-a682bceffc2a)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         18facdd5-72ed-4176-9f67-2730ca8b70d0)(content(Whitespace\"\\n\"))))(Secondary((id \
         397a2bc2-a45c-4413-9d21-8ed0ab50c67d)(content(Whitespace\"\\n\"))))(Tile((id \
         787fe275-b418-4268-a155-db10cf9ceaad)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         909e6cb8-de5d-4520-b4e8-977731e75734)(content(Whitespace\"\\n\"))))(Tile((id \
         e15e6146-4e13-427d-b6f6-30bb3055ee7a)(label(last))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         ff8bbe3e-3f37-4e81-a8b6-47e7d092a96e)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         32e4c27f-7f29-48eb-a466-edbc5cf8f37c)(label([]))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         6291180c-351d-4c55-9954-5dcd7c7e6a79)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         f3a6b8f1-5fdb-4c7d-8485-47d0099a507f)(content(Whitespace\" \
         \"))))(Tile((id \
         c3cdd3af-23db-4296-94dc-85957651a097)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         d57a907b-d4fd-4991-ac7b-257fab07bd98)(content(Whitespace\"\\n\"))))(Tile((id \
         be2af4b4-5671-4709-aad6-639342e6c17e)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         9685b9df-8405-4c3e-855e-5dce15d67800)(content(Whitespace\" \
         \"))))(Tile((id \
         c06f80e9-3d71-46a5-9b89-c3157bd41c9d)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         d0768894-af99-4afc-b23f-41c20f186a00)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         8548c09d-cc12-4083-b4c9-e18f47358781)(content(Whitespace\"\\n\")))))";
      backup_text =
        "# LAST ELEMENT TASK                            #\n\
         #                                              #\n\
         # Implement last: get the last element of a    #\n\
         # list, or return a default if empty.          #\n\
         #                                              #\n\
         # Examples:                                    #\n\
         #   last([1, 2, 3], 0) == 3                    #\n\
         #   last([42], 0) == 42                        #\n\
         #   last([], 99) == 99                         #\n\
         #                                              #\n\
         # Available functions:                         #\n\
         #   fold_left(list, fn, init) -> result        #\n\
         #     fn takes (accumulator, element)          #\n\
         #   fold_right(list, fn, init) -> result       #\n\
         #     fn takes (element, accumulator)          #\n\
         #   rev(list) -> list                          #\n\
         #   length(list) -> Int                        #\n\
         #                                              #\n\
         # Tip: Think about what the fold should        #\n\
         # \"remember\" as it processes each element.     #\n\n\
         let last = fun (xs, default) ->\n\
         ?\n\
         in\n\n\
         test\n\
         last([1, 2, 3], 0)\n\
         == 3\n\
         end;\n\n\
         test\n\
         last([42], 0)\n\
         == 42\n\
         end;\n\n\
         test\n\
         last([1], 0)\n\
         == 1\n\
         end;\n\n\
         test\n\
         last([], 99)\n\
         == 99\n\
         end;\n\n\
         test\n\
         last([], 0)\n\
         == 0\n\
         end\n";
      refractors = "()";
    } )
