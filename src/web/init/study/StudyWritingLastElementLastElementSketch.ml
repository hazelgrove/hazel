let out : string * Haz3lcore.PersistentSegment.t =
  ( "Study / writing / last-element / last-element-sketch",
    {
      segment =
        "((Secondary((id \
         bd4e0898-b60a-436e-a908-e9b28387cd77)(content(Comment\"# LAST ELEMENT \
         TASK                            #\"))))(Secondary((id \
         3c9a5672-1fc7-4ed5-8587-58d59a9ff4ef)(content(Whitespace\"\\n\"))))(Secondary((id \
         13f87199-1abd-46cc-8fda-5d6efd5dd35b)(content(Comment\"#                                              \
         #\"))))(Secondary((id \
         b3cd895c-2197-4fe2-acab-5e7bfd3e1616)(content(Whitespace\"\\n\"))))(Secondary((id \
         1882b7fa-aa58-4ea3-be80-69e42b4a7980)(content(Comment\"# Implement \
         last: get the last element of a    #\"))))(Secondary((id \
         d5dc96e2-9f11-48b3-afab-45ead2db28d3)(content(Whitespace\"\\n\"))))(Secondary((id \
         695f0824-5ae3-4439-b9a8-c64f78f0edc3)(content(Comment\"# list, or \
         return a default if empty.          #\"))))(Secondary((id \
         062d656f-07e1-40f1-850a-5c80e5d1ef31)(content(Whitespace\"\\n\"))))(Secondary((id \
         27504308-5461-48c0-801e-5a50196d6779)(content(Comment\"#                                              \
         #\"))))(Secondary((id \
         af0de4d5-2981-4e3e-98ed-0b1c9e13a2c5)(content(Whitespace\"\\n\"))))(Secondary((id \
         89fa67bf-9196-4971-a590-86b7737d3aec)(content(Comment\"# \
         Examples:                                    #\"))))(Secondary((id \
         4ba44a97-a3ef-400c-9fea-472f446336cd)(content(Whitespace\"\\n\"))))(Secondary((id \
         9f0056eb-3855-4e88-a0c6-34e8662f7079)(content(Comment\"#   last([1, \
         2, 3], 0) == 3                    #\"))))(Secondary((id \
         64a94f6e-1dd7-49f8-a604-9c97275588c1)(content(Whitespace\"\\n\"))))(Secondary((id \
         cb4282cd-0c72-4f31-ac5d-55d3caaa11c9)(content(Comment\"#   last([42], \
         0) == 42                        #\"))))(Secondary((id \
         21f8ac5c-1abe-4079-93c2-e9147b7c5662)(content(Whitespace\"\\n\"))))(Secondary((id \
         7727467c-6efa-4830-b620-90ad684f31e4)(content(Comment\"#   last([], \
         99) == 99                         #\"))))(Secondary((id \
         2181f479-2a07-4d5d-b832-ef8f64561117)(content(Whitespace\"\\n\"))))(Secondary((id \
         c7414af7-d967-4b1b-b344-ced59953c59d)(content(Comment\"#                                              \
         #\"))))(Secondary((id \
         986c43f3-4483-485e-afb7-019574569e9f)(content(Whitespace\"\\n\"))))(Secondary((id \
         6aac256c-aec0-4515-b581-85a1a6d8c10d)(content(Comment\"# Available \
         functions:                         #\"))))(Secondary((id \
         b637a8ba-9165-4c70-bb20-a8c9b8b33338)(content(Whitespace\"\\n\"))))(Secondary((id \
         1fa1bc69-1ec4-4ef9-aed7-27498b502e0a)(content(Comment\"#   \
         fold_left(list, fn, init) -> result        #\"))))(Secondary((id \
         0a635ba0-04d0-4437-9cbf-c7854ce7bb15)(content(Whitespace\"\\n\"))))(Secondary((id \
         cd70da1d-42ed-4436-adf8-21a457d8c47a)(content(Comment\"#     fn takes \
         (accumulator, element)          #\"))))(Secondary((id \
         d96bd862-ae19-4818-a0a4-37ff82fcff32)(content(Whitespace\"\\n\"))))(Secondary((id \
         db0ea618-a867-4eec-9429-063865049e90)(content(Comment\"#   \
         fold_right(list, fn, init) -> result       #\"))))(Secondary((id \
         eec1c904-3e75-42e5-bac8-1edadce3666f)(content(Whitespace\"\\n\"))))(Secondary((id \
         d1f0185c-00e2-4b11-9d6c-0bf4b644746b)(content(Comment\"#     fn takes \
         (element, accumulator)          #\"))))(Secondary((id \
         57c71b97-bdf6-48ed-a0ec-f2cc7bb7a935)(content(Whitespace\"\\n\"))))(Secondary((id \
         93779135-8f9a-4b30-ade3-e34cce93dcd5)(content(Comment\"#   rev(list) \
         -> list                          #\"))))(Secondary((id \
         524542da-4a5b-4a99-9d64-40bcce02eac6)(content(Whitespace\"\\n\"))))(Secondary((id \
         6ff4b5a2-fcb4-4cc4-a72e-8988e5b857d0)(content(Comment\"#   \
         length(list) -> Int                        #\"))))(Secondary((id \
         4ef5ea0c-8530-43dd-8184-cb833f04bffc)(content(Whitespace\"\\n\"))))(Secondary((id \
         ed11c1ed-7f99-4a0a-aabc-c88eb0e7019a)(content(Comment\"#                                              \
         #\"))))(Secondary((id \
         2ce87c5b-12ff-451e-bee1-4849e1f1a737)(content(Whitespace\"\\n\"))))(Secondary((id \
         8e7b71a6-b359-4e30-a637-7911ea5de1db)(content(Comment\"# Tip: Think \
         about what the fold should        #\"))))(Secondary((id \
         b2e14072-d221-4fb2-8136-8644d9292f81)(content(Whitespace\"\\n\"))))(Secondary((id \
         0674bb16-2e19-4f1b-9ded-36426e371c32)(content(Comment\"# \
         \\\"remember\\\" as it processes each element.     \
         #\"))))(Secondary((id \
         0d3e2665-ebb8-4312-88e8-9eab196f0475)(content(Whitespace\"\\n\"))))(Secondary((id \
         c9d85428-cbd9-4b41-9de8-a36e533b7395)(content(Whitespace\"\\n\"))))(Tile((id \
         0f522479-97f8-4617-96ae-e457d1345adc)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         52e50afb-e391-45c7-ad2e-914989e1c28a)(content(Whitespace\" \
         \"))))(Tile((id \
         2efa9142-5fab-4f45-83ef-3d6da511a2f6)(label(last))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         c6d653a2-d461-4cb7-906f-27a939e29dc9)(content(Whitespace\" \
         \")))))((Secondary((id \
         4fe2b420-e6c4-4b71-acc1-fde934cf29ef)(content(Whitespace\" \
         \"))))(Tile((id 0566b070-901f-4c89-ac0a-edacaa9645a0)(label(fun \
         ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 37))(sort Exp))))))(shards(0 \
         1))(children(((Secondary((id \
         ee685f4d-3b8e-4fda-8889-51bc048b3420)(content(Whitespace\" \
         \"))))(Tile((id \
         09106476-fbc3-4d89-83cf-f9dcb89bd305)(label(\"(\"\")\"))(mold((out \
         Pat)(in_(Pat))(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0 1))(children(((Tile((id \
         371ee586-e5a6-4e0b-ba57-13de9c19bcb5)(label(xs))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         20471ee9-4ae0-45c1-91e8-63c267686bff)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 44))(sort Pat))((shape(Concave \
         44))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         59158d68-bc6f-4be4-89b9-86313fb7fa48)(content(Whitespace\" \
         \"))))(Tile((id \
         1200cb99-ed85-421b-90a0-9ae169a8cc08)(label(default))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children()))))))))(Secondary((id \
         32d3b665-00f5-4881-916c-c145a2e60e2d)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         8bdec161-2b9a-4652-8bc6-2c968ee34741)(content(Whitespace\"\\n\"))))(Tile((id \
         da4c0932-72c2-4291-9118-b6f252a7cb23)(label(?))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         829850a6-1f6d-4acc-871d-26746dacc6a7)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         ddcb3939-5ac1-4b25-849d-87fdfb014097)(content(Whitespace\"\\n\"))))(Secondary((id \
         ea8dbcc6-5b68-4156-af11-6d757a1314d1)(content(Whitespace\"\\n\"))))(Tile((id \
         495bec19-ebf1-4d50-9026-e98f68fcd5ed)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         695d7d1b-2122-4089-a455-f3db9edc1acc)(content(Whitespace\"\\n\"))))(Tile((id \
         9807c02f-a725-438c-9b20-1e88113f0b72)(label(last))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         ca464c3e-998c-4b8c-83f8-5542b5028120)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         2d4fa45b-178f-42e7-b789-fb023d2511c1)(label([ ]))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         ed7f67e5-30e0-4e84-96a7-0dffe4b11aec)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         24ce2143-fa57-4ca7-b97e-9b071f5ab78d)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         8fe50103-d311-45e4-bf69-8f086b97c6a7)(content(Whitespace\" \
         \"))))(Tile((id \
         086e8160-71b9-40e7-a590-4fa811488efa)(label(2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         9c2eb8a3-0710-454f-bac8-36281a7e5571)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         2c01f55f-3bfa-4c82-a210-ce0c4f7924e8)(content(Whitespace\" \
         \"))))(Tile((id \
         a6368203-a2e6-49b1-8303-de3a683e53e3)(label(3))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         60202922-1309-4bef-898e-dda2913f9839)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         02f7c685-18d8-4cc3-a8e1-04285766127a)(content(Whitespace\" \
         \"))))(Tile((id \
         6301aa1e-3fa1-4904-a8f2-7d605c327706)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         ed1ca89e-9e9a-47d6-aa12-ca79ce35182c)(content(Whitespace\"\\n\"))))(Tile((id \
         8755e8b9-4c05-4199-a5c6-370841372493)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         5bd0c6d3-3b0e-4722-baae-82faa6b66a34)(content(Whitespace\" \
         \"))))(Tile((id \
         786fa127-8c66-49d5-ac57-ac82da4e67d3)(label(3))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         7cbb92a9-f39e-4870-92d3-9f14533b1373)(content(Whitespace\"\\n\")))))))))(Tile((id \
         1c0387a1-848b-4508-85bc-53824fb0af0a)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         78a48c27-7fb5-4311-851b-8cbf1171a0f1)(content(Whitespace\"\\n\"))))(Secondary((id \
         eaa0f913-9d47-48a0-a4ce-934f30d18b9c)(content(Whitespace\"\\n\"))))(Tile((id \
         dd293d53-bfcd-4efc-9465-dd463a61236b)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         f0dcc8ac-5770-4fe9-9cb4-62d8cb176c16)(content(Whitespace\"\\n\"))))(Tile((id \
         089ccc5b-9e21-496a-aaef-db8185c070c2)(label(last))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         1d39dfbd-bf92-4c89-920b-024297d92cb8)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         1b377ea5-8ff1-4646-984d-8ea508ee8917)(label([ ]))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         535ef62d-856a-44f8-854c-892620ec1001)(label(42))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         83f3cae8-d1f7-4447-89bb-bac151e8146e)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         f8b1e7c2-98fd-44da-a517-1a88111c178e)(content(Whitespace\" \
         \"))))(Tile((id \
         b52f30ba-740e-474e-94ce-7cfeea1af523)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         34c48f95-32b8-4685-8066-84050ba1e8fc)(content(Whitespace\"\\n\"))))(Tile((id \
         a27feea2-58b9-481d-aac5-c12362490815)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         2ad8fe2a-ac24-4a13-bdcf-6b0ddc95efa2)(content(Whitespace\" \
         \"))))(Tile((id \
         a2262111-96cd-4a5a-96d3-57199fa36cdf)(label(42))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         d36d9dbe-3418-40d1-8006-46db8dc8844b)(content(Whitespace\"\\n\")))))))))(Tile((id \
         ea8b24d4-1446-4b16-a72b-ba4bec056591)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         3a6a7ade-757e-424e-9b2e-ebb1234124b9)(content(Whitespace\"\\n\"))))(Secondary((id \
         34003e04-66dc-404e-8991-ad4a765fd70c)(content(Whitespace\"\\n\"))))(Tile((id \
         c084f4af-8472-4639-8585-e896e0119614)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         c8249cf7-315a-4616-b6ac-1946eb1d811d)(content(Whitespace\"\\n\"))))(Tile((id \
         b641132c-261e-4e07-8c36-c8afb7d5c5e9)(label(last))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         9656ea02-cb98-49b3-9757-052e951b7d80)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         f7872065-f5a5-41cc-9d34-4422e876063f)(label([ ]))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         eddbe355-0079-462a-8100-a31c4f709751)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         2420f7c1-8a44-48d7-b9d1-59271d9687be)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         6473803a-dba7-4043-bec8-77f2c415ca31)(content(Whitespace\" \
         \"))))(Tile((id \
         9eff7eb3-2873-45b3-b3bc-943377ba660c)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         d4ea20bf-19aa-4d5d-85b0-b83fa8222279)(content(Whitespace\"\\n\"))))(Tile((id \
         e811a5d8-60dd-48e7-a65c-152614ae0e15)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         79d6e60d-31ad-4d9a-b3ea-4e6b77cfe841)(content(Whitespace\" \
         \"))))(Tile((id \
         6e82d34f-625e-49f5-96ce-7dcc3ea03e1f)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         95f13105-744b-4cc9-93f4-955f7ad703da)(content(Whitespace\"\\n\")))))))))(Tile((id \
         6e1d6847-8522-4817-9a4f-0dcda40d5673)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         fa5d0a08-13c6-4dd5-a7d2-cf5550df687e)(content(Whitespace\"\\n\"))))(Secondary((id \
         b8537d7b-ac47-4cf2-a32d-11582ff29b20)(content(Whitespace\"\\n\"))))(Tile((id \
         e9c1ca27-b055-4890-b61f-93d0444c8925)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         2b45e23a-a55b-4c60-89e0-ac66010c6e47)(content(Whitespace\"\\n\"))))(Tile((id \
         6437eb39-62f3-4542-b6bc-5eb46f7a8d52)(label(last))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         0e506318-c5cf-4f25-8ce1-696fdb4d0c1a)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         ea21890f-5bba-49a3-be61-d0dffa558bc9)(label([]))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         8d4f9776-db83-4b01-8da2-1aeb5703f78e)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         539472a5-a76e-46e9-a69c-4e305afae978)(content(Whitespace\" \
         \"))))(Tile((id \
         9062a692-b4c3-4fab-add9-f4f4082240ef)(label(99))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         1313a162-c1f9-43a1-94af-bfde85974f34)(content(Whitespace\"\\n\"))))(Tile((id \
         34176096-5286-4c5e-958c-274771177f2d)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         4f284476-e692-475e-b618-53349e014cfe)(content(Whitespace\" \
         \"))))(Tile((id \
         3f38de39-0960-4c04-ac0c-d137f6ecb0e3)(label(99))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         f96aab91-fe44-44cf-86c6-57085f826702)(content(Whitespace\"\\n\")))))))))(Tile((id \
         0589fbff-5eda-430b-9a8d-ce6a6778b89b)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         ac949eaa-ccc3-4915-a2a3-515620800e3d)(content(Whitespace\"\\n\"))))(Secondary((id \
         5dc0e2af-f384-4177-9897-3d6eb2f63f43)(content(Whitespace\"\\n\"))))(Tile((id \
         2c926787-77e6-48c9-a796-ee703efc27c8)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         aaafae7c-24bd-40c1-976f-896a79905fb6)(content(Whitespace\"\\n\"))))(Tile((id \
         e45fd253-ba24-4e1c-ac06-6ce253051b23)(label(last))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         546a2bda-58c8-4230-9bb8-519b85ab4743)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         0b550738-a215-431d-925e-804fe8fa5997)(label([]))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         6c02878f-99b8-4f4d-b5cc-de4f54d5399d)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         59dc4747-658d-4004-8b43-2cb88ed84715)(content(Whitespace\" \
         \"))))(Tile((id \
         a07b9623-0d4d-4e68-9edf-d63947ab7bc5)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         99b0c143-d08f-48b4-bcf8-afb1b2f71528)(content(Whitespace\"\\n\"))))(Tile((id \
         46e992e0-1723-42d2-843e-da4afc62bd98)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         fcd596df-059c-420f-8787-e99b333c85ab)(content(Whitespace\" \
         \"))))(Tile((id \
         12aa698d-698b-48c9-9e52-6d60b55df3f1)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         d42f8991-fe15-4f6a-9651-01253e2c9e53)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         9d9a558f-a920-4138-b8ab-eeb2b54f8a40)(content(Whitespace\"\\n\")))))";
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
