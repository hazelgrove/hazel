let out : string * Haz3lcore.PersistentSegment.t =
  ( "Study / writing / last-element / last-element-sketch",
    {
      segment =
        "((Secondary((id \
         541c7a0b-f1ed-4354-b679-d625bbd269d3)(content(Comment\"# LAST ELEMENT \
         TASK                            #\"))))(Secondary((id \
         3e9b0072-1af5-4ee0-8c94-8b7d390737e3)(content(Whitespace\"\\n\"))))(Secondary((id \
         71674228-8b2a-4a21-b49c-348040ad7471)(content(Comment\"#                                              \
         #\"))))(Secondary((id \
         ee242d6b-596a-4647-ba8e-940e8895a07b)(content(Whitespace\"\\n\"))))(Secondary((id \
         86a1b7d5-c25d-45e0-86f8-feb65dc38903)(content(Comment\"# Implement \
         last: get the last element of a    #\"))))(Secondary((id \
         22c2c75c-fc9e-415d-9000-16d2fa25ca01)(content(Whitespace\"\\n\"))))(Secondary((id \
         0844fbe3-0c1d-401e-8fd7-8c8e2f2becb8)(content(Comment\"# list, or \
         return a default if empty.          #\"))))(Secondary((id \
         ad21deaf-94e3-4ebb-bb88-962c831e0347)(content(Whitespace\"\\n\"))))(Secondary((id \
         6f886288-0fae-434c-b8bb-4c0be4a0af3d)(content(Comment\"#                                              \
         #\"))))(Secondary((id \
         52cac301-94ed-4061-91b2-73e957c28e66)(content(Whitespace\"\\n\"))))(Secondary((id \
         01ff22e6-4c46-4c16-bb54-54d6e5cd2ff6)(content(Comment\"# \
         Examples:                                    #\"))))(Secondary((id \
         89470ff0-b426-4942-89b3-3f76f5970322)(content(Whitespace\"\\n\"))))(Secondary((id \
         550184b3-9cdd-4da6-ac49-79ea91fe6235)(content(Comment\"#   last([1, \
         2, 3], 0) == 3                    #\"))))(Secondary((id \
         3f8cd881-c2a7-4ca9-b822-c88b596113a7)(content(Whitespace\"\\n\"))))(Secondary((id \
         686370d8-7791-4652-bcda-40d8dddef176)(content(Comment\"#   last([42], \
         0) == 42                        #\"))))(Secondary((id \
         b4ba3dad-8c6d-4862-97c1-4c2fab2d5c61)(content(Whitespace\"\\n\"))))(Secondary((id \
         3a34bd33-3179-4c22-98b4-96369a4ddd5f)(content(Comment\"#   last([], \
         99) == 99                         #\"))))(Secondary((id \
         b9bfbfbd-e9df-40d8-970a-db09b8b53d7b)(content(Whitespace\"\\n\"))))(Secondary((id \
         23ffba60-9ae6-407b-b69a-cc29b7cc269b)(content(Comment\"#                                              \
         #\"))))(Secondary((id \
         7fb60b51-7284-4cd7-8783-a4e320a1c11d)(content(Whitespace\"\\n\"))))(Secondary((id \
         aac90284-abf7-40cf-a765-bb9797242367)(content(Comment\"# Available \
         functions:                         #\"))))(Secondary((id \
         b128a126-7581-4efd-a660-e1989a6e78a0)(content(Whitespace\"\\n\"))))(Secondary((id \
         f27649c2-b241-40a6-859c-fd896ae8feac)(content(Comment\"#   \
         fold_left(list, fn, init) -> result        #\"))))(Secondary((id \
         8ae676eb-5b43-4ede-b043-989a8a2aca31)(content(Whitespace\"\\n\"))))(Secondary((id \
         8eededef-2e1f-4af0-b896-0bfb4825053f)(content(Comment\"#     fn takes \
         (accumulator, element)          #\"))))(Secondary((id \
         7f16feca-2387-45a0-9a34-3e39a2f1f071)(content(Whitespace\"\\n\"))))(Secondary((id \
         22a09a08-0d1d-403f-82eb-9ee101641291)(content(Comment\"#   \
         fold_right(list, fn, init) -> result       #\"))))(Secondary((id \
         bd6ef21e-b53b-477b-9a41-d2529465401f)(content(Whitespace\"\\n\"))))(Secondary((id \
         9256cbed-eecc-4784-8c32-1f1c2a54b758)(content(Comment\"#     fn takes \
         (element, accumulator)          #\"))))(Secondary((id \
         2b098c04-0fdb-433b-9d95-cfef65626c9c)(content(Whitespace\"\\n\"))))(Secondary((id \
         a4690dc2-032b-4678-ae77-547b8bd10e5b)(content(Comment\"#   rev(list) \
         -> list                          #\"))))(Secondary((id \
         b7de364b-9db7-4542-a6c0-585a6370f844)(content(Whitespace\"\\n\"))))(Secondary((id \
         c2c5b57d-0fb6-49de-bead-e2bc5a52a656)(content(Comment\"#   \
         length(list) -> Int                        #\"))))(Secondary((id \
         6834f205-9f81-4486-977d-e9445a94e748)(content(Whitespace\"\\n\"))))(Secondary((id \
         f4607da8-f0cd-4020-9693-4d9475d0e253)(content(Comment\"#                                              \
         #\"))))(Secondary((id \
         8a4de0a7-c74b-48a9-b0e4-7878995e0f04)(content(Whitespace\"\\n\"))))(Secondary((id \
         e5d8aa35-e212-4c47-a694-8ed9def3fc6b)(content(Comment\"# Tip: Think \
         about what the fold should        #\"))))(Secondary((id \
         1f336cc9-2997-49f2-9d35-48f7efd1c8bc)(content(Whitespace\"\\n\"))))(Secondary((id \
         1e81973f-be28-4691-bbbb-a3bf672aa876)(content(Comment\"# \
         \\\"remember\\\" as it processes each element.     \
         #\"))))(Secondary((id \
         c52b5d68-2f3f-4404-ab95-2c941138eec6)(content(Whitespace\"\\n\"))))(Secondary((id \
         54bcb98f-504a-40a8-9680-cdcc35a2d8fe)(content(Whitespace\"\\n\"))))(Tile((id \
         75eb8deb-637c-45da-98f5-4eb5571d2ef9)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         b51d2001-f17e-463b-bd08-e9188f7fd675)(content(Whitespace\" \
         \"))))(Tile((id \
         123627e4-2c3d-4ded-8ce6-f1cef1e57138)(label(last))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         2ed47a58-980e-47e5-80cc-1cc06784297f)(content(Whitespace\" \
         \")))))((Secondary((id \
         9ea64097-64df-4083-bdf4-d7bccc59f455)(content(Whitespace\" \
         \"))))(Tile((id aa8bff0c-a807-4bd3-9cc9-cfdcbce5cae5)(label(fun \
         ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 37))(sort Exp))))))(shards(0 \
         1))(children(((Secondary((id \
         13d00ef5-9ee8-4b53-ba93-c32a7c951c2c)(content(Whitespace\" \
         \"))))(Tile((id \
         5a5f3ceb-80ee-4ff8-ad53-e5c5d5074e6a)(label(\"(\"\")\"))(mold((out \
         Pat)(in_(Pat))(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0 1))(children(((Tile((id \
         10fd268e-134e-4b4e-a7fa-b615dcb44004)(label(xs))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         3c31a5fa-92ec-472c-92f9-3d66569fb24d)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 44))(sort Pat))((shape(Concave \
         44))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         f8ca9a9d-9289-4f0c-a295-834478057b40)(content(Whitespace\" \
         \"))))(Tile((id \
         8e87200e-4cfc-46cc-aea0-7abed755fecb)(label(default))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children()))))))))(Secondary((id \
         473f1025-c9d0-4bbd-a871-c0560af68bae)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         83ab0333-6e40-4a93-b341-fadfac4f4a8d)(content(Whitespace\"\\n\"))))(Tile((id \
         d4fc027a-6297-44e4-a0a8-b83fbb363505)(label(?))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         31ce9abd-3b15-4de9-86d2-df7aa6db889c)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         358dad4c-db67-4ef2-93bc-ded4d251c0d8)(content(Whitespace\"\\n\"))))(Secondary((id \
         aecb3bec-dfc7-475a-9a34-36dae957b8f9)(content(Whitespace\"\\n\"))))(Tile((id \
         6e0ce0cd-eb49-4f18-907a-f9163d59eb8b)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         b94fb0e3-9717-444a-a7fb-4e5801054a90)(content(Whitespace\"\\n\"))))(Tile((id \
         9ecad852-2dc6-4d6e-91e1-50822bb7df76)(label(last))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         646be124-c3e9-42ad-a0e3-78350e8a9579)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         1c840e3c-de63-4c7d-9ca5-7de4020636b1)(label([ ]))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         46a143aa-f521-4d68-bb95-1f4638384a4c)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         c8afd009-c623-4f41-a2b6-73c9000c77b4)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         cba42e82-b6e1-4424-8c27-5df6e9013f12)(content(Whitespace\" \
         \"))))(Tile((id \
         49cabbb2-7f9c-4bd6-a4b3-fc6764e55e35)(label(2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         b45e1273-873c-4830-8010-7d093e3ef2e8)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         4eb64b4f-de50-4aca-ab65-632d240adda5)(content(Whitespace\" \
         \"))))(Tile((id \
         5adf49e6-b51b-482a-a91c-e37513ae8eb5)(label(3))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         8b42e4ed-b1db-4d3c-b13a-4136ace22205)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         3037251f-bb7a-4efb-9fb4-a34ee9fc5295)(content(Whitespace\" \
         \"))))(Tile((id \
         8a275465-01c1-4f0c-b49d-1531f198ce70)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         7b0c875a-3bd2-457b-91b2-7e089921672b)(content(Whitespace\"\\n\"))))(Tile((id \
         2f2d9e0f-0166-4d04-ab3d-fe9bc6bba382)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         a45b82d0-e4ac-48a2-aabc-54719f495b90)(content(Whitespace\" \
         \"))))(Tile((id \
         1733c18b-8838-4a2e-bf50-aa89c74ea696)(label(3))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         0e77b692-5792-4159-9bdd-186220ebd8b5)(content(Whitespace\"\\n\")))))))))(Tile((id \
         882dd59c-45b9-4751-840d-da5780058051)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         6462a6a6-0d62-47dc-b439-e76dc4fe96c0)(content(Whitespace\"\\n\"))))(Secondary((id \
         9885e806-8a6b-4032-aa5b-9668c676840f)(content(Whitespace\"\\n\"))))(Tile((id \
         1f02f315-b509-4185-95c3-017e2a5ff18d)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         dcb102f8-f8f5-4f78-9aaa-0b9705c43ad6)(content(Whitespace\"\\n\"))))(Tile((id \
         2c6d5e55-74aa-4776-a377-0e3d9094a1f2)(label(last))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         614ccd67-1bc4-4247-ac36-1f389a411908)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         1e8bfaa8-438f-4004-b149-1bda4f1904e0)(label([ ]))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         1bcd546c-0605-49f8-900d-7893cca9cae5)(label(42))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         bc983f90-8e3f-49c0-a2af-96e3894c260f)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         aba8aa6a-30c1-4017-b835-233304347a9b)(content(Whitespace\" \
         \"))))(Tile((id \
         e9457bd6-15d9-485b-b0e2-9b4281ce60c0)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         8f0dc715-7fe4-4ad0-8eec-ab9c2908b966)(content(Whitespace\"\\n\"))))(Tile((id \
         c4b22435-7fe9-4a9a-81cd-40ed3aaa2bcb)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         544b9284-9aa0-408a-b80c-1875e8d5e546)(content(Whitespace\" \
         \"))))(Tile((id \
         baf0ae5e-caa8-45b8-9a3b-ce3e5b11ad8d)(label(42))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         d42cc1d4-4855-492a-b625-b2266a124d43)(content(Whitespace\"\\n\")))))))))(Tile((id \
         7ff4beee-eae6-4d02-a18d-c907c96c5ad3)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         ccfaa701-9bd3-4da5-900a-fe3118f44490)(content(Whitespace\"\\n\"))))(Secondary((id \
         855c6d28-3ffa-4a5a-b54f-60446989e7c8)(content(Whitespace\"\\n\"))))(Tile((id \
         75af09dd-4a59-4368-861b-0736099a0cad)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         7378a637-10f1-4cf0-abb5-73e06df2feea)(content(Whitespace\"\\n\"))))(Tile((id \
         85baba71-fe1c-4fb0-a9a7-d9a638076bac)(label(last))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         7ad78f02-1793-498d-81cb-c06e8154c6a0)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         1808121a-c8e2-4f36-896b-66fa33b73a51)(label([ ]))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         28ce7295-77c2-482a-9686-e0ca132bede2)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         2bb7baa0-a977-4a2c-bee0-d8980e2e4187)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         3212d9cb-e006-4abb-b5b2-ee67ccd1c3c2)(content(Whitespace\" \
         \"))))(Tile((id \
         62f6d5f6-6de2-4439-b245-3a0ce68b3b93)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         7a58dc2f-1074-4e70-9910-ee46648667a0)(content(Whitespace\"\\n\"))))(Tile((id \
         cf883dfc-2b9f-49c8-91a0-ecba0f806b33)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         e324be4f-0a32-491c-85bd-767cf3dc1f2c)(content(Whitespace\" \
         \"))))(Tile((id \
         3f8f21ce-772d-4e7f-ac14-8ac31b263cd6)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         4408d236-fdd2-4419-8740-26b6ea86cc33)(content(Whitespace\"\\n\")))))))))(Tile((id \
         e842f1f2-163a-4c44-8fd0-577818026a0d)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         5a438ac8-5df4-4e0c-8f68-bde9fff44b82)(content(Whitespace\"\\n\"))))(Secondary((id \
         7d129da0-2c3f-4abf-8c2f-053edd8e8586)(content(Whitespace\"\\n\"))))(Tile((id \
         a5c76023-d905-4e74-8abe-da856d7108b2)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         fddef9bb-8468-4960-a6b1-7e502ee5c1ef)(content(Whitespace\"\\n\"))))(Tile((id \
         f499643d-f2b9-403a-829e-b0cc877ce9f4)(label(last))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         a634a8e6-13f5-4026-90fe-a32865210670)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         7e18c5f7-40d4-4813-a715-7b13cd2930b4)(label([]))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         155336bf-63c6-419a-bc8e-e3424bcc47cc)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         974599fd-4fa1-4592-a9b7-d23c8c120986)(content(Whitespace\" \
         \"))))(Tile((id \
         64657f08-d919-41c3-87e2-3617989ebfdb)(label(99))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         9e4dc4fa-20c4-44d9-b5b3-13644c860f06)(content(Whitespace\"\\n\"))))(Tile((id \
         73c0982f-a360-4e13-bffd-e2108455ce06)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         50a93221-eb4f-48ef-96be-bc5d4e756f40)(content(Whitespace\" \
         \"))))(Tile((id \
         3b855b06-ec9d-4392-94d8-079b07b95fc6)(label(99))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         b849756d-1a04-48a6-8c28-520e85ac2290)(content(Whitespace\"\\n\")))))))))(Tile((id \
         53c15433-ac71-4191-a8cc-7a81037a34e0)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         d763a586-b3eb-4576-892e-1f623c052fea)(content(Whitespace\"\\n\"))))(Secondary((id \
         23144b6c-c70e-466f-a371-d98b171efb90)(content(Whitespace\"\\n\"))))(Tile((id \
         b1750a5e-5d2c-4e65-99db-cd7bd8a90e65)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         a8622bb6-7c84-4a16-ae15-7fbab38d2786)(content(Whitespace\"\\n\"))))(Tile((id \
         539c5a83-c005-423b-a1c8-1db11451e649)(label(last))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         04cabde9-cf3a-476e-83dd-541da682647a)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         c5dc75d8-3930-4740-ba58-00c800541604)(label([]))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         221dcd13-8245-4f57-9336-a6c8eaed8b12)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         482946d4-a071-45fd-8453-e87eeaefef32)(content(Whitespace\" \
         \"))))(Tile((id \
         a7b82f49-ca7d-4dfc-8a28-2c84c612eab1)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         2d47dff0-e9bd-42b0-ad77-b3c9a3eedba1)(content(Whitespace\"\\n\"))))(Tile((id \
         7d1c188a-ccfd-4cdc-95e4-70415b52711c)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         c3d6400b-f0c7-4a17-ae88-3e5f9b77c813)(content(Whitespace\" \
         \"))))(Tile((id \
         4998cbb6-45ea-410d-b650-c45f6123a5c3)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         fc8bbffb-6c91-4073-a8dd-718e2ed09ee8)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         f4e55022-26e7-4f2f-adb4-8dd951c90ac2)(content(Whitespace\"\\n\")))))";
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
