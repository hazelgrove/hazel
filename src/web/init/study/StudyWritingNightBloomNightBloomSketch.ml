let out : string * Haz3lcore.PersistentSegment.t =
  ( "Study / writing / night-bloom / night-bloom-sketch",
    {
      segment =
        "((Secondary((id \
         3987eb44-fd7e-43e4-bedc-a7af59ab9989)(content(Comment\"# NIGHT BLOOM \
         FILTER TASK                        #\"))))(Secondary((id \
         02856485-cdb2-49a0-9985-8c850e7b76c4)(content(Whitespace\"\\n\"))))(Secondary((id \
         15aebd70-359a-4dfe-bc2e-e59f60ebde7a)(content(Comment\"#                                                \
         #\"))))(Secondary((id \
         400433d9-1822-4edf-ba9e-e5648f4b7923)(content(Whitespace\"\\n\"))))(Secondary((id \
         e2724f62-3b5a-4845-98c3-e82fe9e6cc7c)(content(Comment\"# A plant \
         catalog has entries like:              #\"))))(Secondary((id \
         a1b9a928-8afb-4004-9b10-065f1edeecfe)(content(Whitespace\"\\n\"))))(Secondary((id \
         deee634b-1f41-43de-afec-10e2198bf1ce)(content(Comment\"#   \
         \\\"Moonbloom [night] 200ml\\\"                    \
         #\"))))(Secondary((id \
         c8323e7b-b3e3-4ecb-adfa-a3e33dd8ce74)(content(Whitespace\"\\n\"))))(Secondary((id \
         b7823a87-e147-4c13-93a3-6d336d9f32b6)(content(Comment\"#   \
         \\\"Duskrose [day] 150ml\\\"                        \
         #\"))))(Secondary((id \
         b8b846bd-379b-4f2c-a41e-cab7e6f6a715)(content(Whitespace\"\\n\"))))(Secondary((id \
         f116aa85-e56d-47a1-99c9-83cc26d17bc6)(content(Comment\"#                                                \
         #\"))))(Secondary((id \
         eb46310e-c5d0-4521-ab74-f5865e493df5)(content(Whitespace\"\\n\"))))(Secondary((id \
         02501f4b-a44d-4ace-a962-df86853be12f)(content(Comment\"# Filter to \
         night-blooming plants and extract    #\"))))(Secondary((id \
         7a091b14-2415-4f35-b603-3eeb7d5e27ce)(content(Whitespace\"\\n\"))))(Secondary((id \
         4ba32bd5-f943-4684-abb3-e099881384c4)(content(Comment\"# just their \
         names: [\\\"Moonbloom\\\", \\\"Starfern\\\"]    #\"))))(Secondary((id \
         5046def4-9f33-4c68-ba6a-7edb6712900b)(content(Whitespace\"\\n\"))))(Secondary((id \
         86b172a6-0655-4100-b180-347d98328775)(content(Comment\"#                                                \
         #\"))))(Secondary((id \
         55f408d9-bb35-485d-af35-9cdd2eada3c0)(content(Whitespace\"\\n\"))))(Secondary((id \
         ea508beb-d277-41a2-ab60-0d6c40b227b1)(content(Comment\"# \
         Steps:                                         #\"))))(Secondary((id \
         564a3b06-96b8-4f9c-a523-d55d76f4b2c1)(content(Whitespace\"\\n\"))))(Secondary((id \
         87d120a8-c7ed-47f5-8bc9-2bdf9f83565a)(content(Comment\"#   1. \
         is_night: check if entry contains \\\"night\\\" #\"))))(Secondary((id \
         7c2c079a-eb35-4218-b611-d94a1c80f30b)(content(Whitespace\"\\n\"))))(Secondary((id \
         034838e7-41d2-4b97-a51c-c1cd3cedfc25)(content(Comment\"#   2. \
         extract_name: get the first word          #\"))))(Secondary((id \
         4a6db1b1-87c8-4263-ad9f-829ef57a6903)(content(Whitespace\"\\n\"))))(Secondary((id \
         4073f7ab-e284-438f-9be8-60b513b18a30)(content(Comment\"#   3. Combine \
         with filter and map               #\"))))(Secondary((id \
         477b5359-51f2-44e4-8f00-8f758a423330)(content(Whitespace\"\\n\"))))(Secondary((id \
         61bd91e2-08af-4313-ae0d-e46291714f81)(content(Comment\"#                                                \
         #\"))))(Secondary((id \
         70ae03b9-ec4a-4bde-ade6-7902af509fe4)(content(Whitespace\"\\n\"))))(Secondary((id \
         df4b18aa-deda-41d3-8287-c70d9c8a2fd2)(content(Comment\"# Available \
         functions:                           #\"))))(Secondary((id \
         9be65ec3-2805-4779-9cdd-f6097d4595e3)(content(Whitespace\"\\n\"))))(Secondary((id \
         8f359c8a-fff3-43e0-a237-d09e0e5c8693)(content(Comment\"#   \
         string_match(pattern, str) -> Bool           #\"))))(Secondary((id \
         fde04913-7192-4f58-8daa-46d655692cd8)(content(Whitespace\"\\n\"))))(Secondary((id \
         f3f8c642-d52d-4e58-8a87-a4dc7dd6a462)(content(Comment\"#   \
         string_split(separator, str) -> [String]     #\"))))(Secondary((id \
         7e01820b-1d00-4fae-b475-50b728181197)(content(Whitespace\"\\n\"))))(Secondary((id \
         3861fb60-afd0-42ab-92f0-0119feb64333)(content(Comment\"#   nth(list, \
         index) -> element                  #\"))))(Secondary((id \
         12431d3e-5322-4858-86c6-367f5784dbaa)(content(Whitespace\"\\n\"))))(Secondary((id \
         eff2b445-6e7c-4ccf-afbb-0c33cb997140)(content(Comment\"#   \
         filter(list, predicate) -> list              #\"))))(Secondary((id \
         4e76417c-e9d2-488a-aa82-c99c5fdc5d4a)(content(Whitespace\"\\n\"))))(Secondary((id \
         01ae1739-8365-4230-8b40-d19b9f33899a)(content(Comment\"#   map(list, \
         fn) -> list                        #\"))))(Secondary((id \
         ab9f8bea-db89-4733-9cee-b4c39c97f298)(content(Whitespace\"\\n\"))))(Secondary((id \
         f9b74e9b-4121-462f-b025-3480843e66cd)(content(Comment\"#                                                \
         #\"))))(Secondary((id \
         018aabaf-cb7d-4c89-80a2-03a6d9fc65ba)(content(Whitespace\"\\n\"))))(Secondary((id \
         10380dbb-9ba0-4714-b500-bc2b351a46dd)(content(Comment\"# Note: \
         string_match uses regex patterns.        #\"))))(Secondary((id \
         19be1320-52b8-488d-9370-5ad2b8ffee36)(content(Whitespace\"\\n\"))))(Secondary((id \
         e784cbc2-a2a3-4abb-97bd-b366a611ed2b)(content(Comment\"# The pattern \
         \\\"[abc]\\\" matches any of a, b, c.   #\"))))(Secondary((id \
         bba25e1c-993e-45c9-9d30-f812302d8a05)(content(Whitespace\"\\n\"))))(Secondary((id \
         22504d4e-8c09-49e1-8a4f-ce5696135ecf)(content(Comment\"#                                                \
         #\"))))(Secondary((id \
         bcdcf03a-1731-414b-a586-0027e4ae4278)(content(Whitespace\"\\n\"))))(Secondary((id \
         8804e7b2-10cd-4e66-8ad7-269b86716a5e)(content(Comment\"# Tip: Use \
         probes to see what your pattern       #\"))))(Secondary((id \
         e8e1452e-dcca-438f-8488-36a9f1ab6d5d)(content(Whitespace\"\\n\"))))(Secondary((id \
         1a1cc2e4-36fd-4e5a-89c2-c5f5fee67738)(content(Comment\"# actually \
         matches -- regex can be surprising!   #\"))))(Secondary((id \
         15db0e04-4df4-45df-b665-8bf8bfff7ef7)(content(Whitespace\"\\n\"))))(Secondary((id \
         910fa13f-a324-49e7-93c5-f541a7bdf475)(content(Whitespace\"\\n\"))))(Tile((id \
         575c6e6c-e73f-4cb0-b18f-92b1e38bc3c7)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         61ebe7f2-28f6-4fcf-89ab-bf673dc70bdb)(content(Whitespace\" \
         \"))))(Tile((id \
         c2af925f-cc94-415e-9052-6eafbaa51198)(label(entries))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         183ba071-e2e6-42f0-a98d-8f61b11380ef)(content(Whitespace\" \
         \")))))((Secondary((id \
         d629f801-4f8a-4ecd-87ca-763386d46962)(content(Whitespace\" \
         \"))))(Tile((id 57649529-667e-4723-b2e9-bcd9303d4012)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         a5820245-48c9-4959-b128-c0f74a3d94ec)(content(Whitespace\"\\n\"))))(Tile((id \
         52e688f5-861e-4764-a3a3-13218ea3c68c)(label(\"\\\"Moonbloom [night] \
         200ml\\\"\"))(mold((out Exp)(in_())(nibs(((shape Convex)(sort \
         Exp))((shape Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
         d1f408f2-7030-4b4f-b7ab-dcd693ea489b)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         3d4b3f00-4589-428a-9cd7-ac9c13bfb543)(content(Whitespace\"\\n\"))))(Tile((id \
         c32db032-c2a4-4bd6-809c-fab60a4781b7)(label(\"\\\"Duskrose [day] \
         150ml\\\"\"))(mold((out Exp)(in_())(nibs(((shape Convex)(sort \
         Exp))((shape Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
         46c8556c-3b8b-4533-9ec7-e9db00f59bab)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         32b0e2e8-8e5b-4e62-afdf-71fa4d17a04f)(content(Whitespace\"\\n\"))))(Tile((id \
         0d9aa5bf-0fcd-45b4-b349-4022f1906c19)(label(\"\\\"Starfern [night] \
         175ml\\\"\"))(mold((out Exp)(in_())(nibs(((shape Convex)(sort \
         Exp))((shape Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
         55afc6c2-fd28-4be2-8514-e567a321487e)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         6f4c02f6-6a56-4896-9b88-69ed5f188e76)(content(Whitespace\"\\n\"))))(Tile((id \
         5f266eb6-4f7d-43c7-bf01-e7a142c8a467)(label(\"\\\"Ghostvine [day] \
         100ml\\\"\"))(mold((out Exp)(in_())(nibs(((shape Convex)(sort \
         Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         95986068-ec2b-404b-9c6f-aab7c2c26b59)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         1b3e5554-8c06-4722-a89f-a8ee88f7fc48)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         eceb5f30-7072-4c37-9373-c3abc28424ed)(content(Whitespace\"\\n\"))))(Secondary((id \
         18e6eec3-58e4-4a4c-802e-6bb84c260b76)(content(Whitespace\"\\n\"))))(Secondary((id \
         ff3c6e52-a104-4f91-9174-4c2a4b1274af)(content(Comment\"# Check if \
         entry is a night-blooming plant #\"))))(Secondary((id \
         8529ea73-c124-4dea-8ce9-323c011d5101)(content(Whitespace\"\\n\"))))(Tile((id \
         7312f530-50c4-4ecb-a33e-0f85de4a3e7c)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         8e19b0ba-27ae-4488-8841-8683947a1acf)(content(Whitespace\" \
         \"))))(Tile((id \
         8e95e0ae-aa39-4d36-a328-a58d0eb4eaff)(label(is_night))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         ef1e1673-472e-4559-b964-d20ef294832d)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 24))(sort Pat))((shape(Concave \
         24))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         ed981e74-712b-46f7-a99d-a07a5e8d03b6)(content(Whitespace\" \
         \"))))(Tile((id \
         14a69745-ffcc-4846-bb4b-830ce5d89098)(label(String))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         a3018bb1-df6e-4161-8ead-04f76e9c63e5)(content(Whitespace\" \
         \"))))(Tile((id \
         ebaf8cd0-a8ee-4268-adfe-6670d06ac6d3)(label(->))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 13))(sort Typ))((shape(Concave \
         13))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         556b89df-2d09-4f4c-ad92-e29fe77ff317)(content(Whitespace\" \
         \"))))(Tile((id \
         964e6187-b22f-45e4-accc-cf9e585fc235)(label(Bool))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         39d8462e-fb67-4c12-bfed-e00963956597)(content(Whitespace\" \
         \")))))((Secondary((id \
         7698800a-3abe-4645-bb8c-232a0bc93a33)(content(Whitespace\" \
         \"))))(Tile((id 6dc04ecd-b03c-4c2c-8df3-7ed588821380)(label(fun \
         ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 37))(sort Exp))))))(shards(0 \
         1))(children(((Secondary((id \
         e5dfd45a-eabd-4896-a114-742fac267dc6)(content(Whitespace\" \
         \"))))(Tile((id \
         4a05239d-6bfd-4ccd-b045-d68644606b9d)(label(entry))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         c2c1a7ad-6dcd-4f91-a7cf-173f19c4b968)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         f9e56ef4-6b45-4851-a61f-b5fd1ca6d71f)(content(Whitespace\"\\n\"))))(Tile((id \
         99e06af5-09aa-4358-aa17-c9abed16fbd6)(label(?))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         697ff444-e4c8-46aa-83a5-f05aa1b4d433)(content(Whitespace\"\\n\"))))(Secondary((id \
         47dd8630-4e80-4c9b-a9dc-c53b8725a2c2)(content(Whitespace\"\\n\"))))(Secondary((id \
         1685cc36-08a4-483c-8db2-fbfbd14f97b2)(content(Whitespace\"\\n\"))))(Secondary((id \
         2f2f75fb-ac76-44df-91bb-a28583bffa11)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         461b4dda-0c3d-4b4f-b341-c637e2235574)(content(Whitespace\"\\n\"))))(Secondary((id \
         fa039261-20a6-4d4b-80f2-ea71f5402612)(content(Whitespace\"\\n\"))))(Secondary((id \
         a89aaf79-78a9-4ef1-b734-cc57900916eb)(content(Comment\"# Extract just \
         the plant name from an entry #\"))))(Secondary((id \
         29f2cedd-9835-460d-b29e-b039b6f0d55c)(content(Whitespace\"\\n\"))))(Tile((id \
         804b65c0-6012-4e9a-8fa8-b52dd903374b)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         9fd08c0a-cd0f-4434-9da1-0e4ef852d57b)(content(Whitespace\" \
         \"))))(Tile((id \
         6fe3dd25-3b1a-4eb8-8075-00f19009938b)(label(extract_name))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         ea009ca0-bd48-4a89-8857-f72f7e66ad78)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 24))(sort Pat))((shape(Concave \
         24))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         906bf540-a8a0-47e3-b998-991e60bc32e7)(content(Whitespace\" \
         \"))))(Tile((id \
         d85188cc-1619-4401-b20e-293e65f7acc9)(label(String))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         ff3fa6a4-214d-436c-81ee-c814527596ed)(content(Whitespace\" \
         \"))))(Tile((id \
         fe320dbd-2f11-47cb-a7ca-269c379fe635)(label(->))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 13))(sort Typ))((shape(Concave \
         13))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         85769c90-88eb-4723-b437-41c53e179305)(content(Whitespace\" \
         \"))))(Tile((id \
         afef0349-2430-4a3e-a1c3-fd6018453a48)(label(String))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         5ba1a544-ccb1-4ad1-8a68-2dbf8d46272e)(content(Whitespace\" \
         \")))))((Secondary((id \
         57af83d0-3df9-45df-acb3-f1983fc33d1b)(content(Whitespace\" \
         \"))))(Tile((id 427ce943-35e1-4de5-bd84-714ea0fa29f1)(label(fun \
         ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 37))(sort Exp))))))(shards(0 \
         1))(children(((Secondary((id \
         754af4aa-4afa-4a12-b71d-8fb820c6271d)(content(Whitespace\" \
         \"))))(Tile((id \
         1befb7a7-fae7-4e3f-888d-74a29643299e)(label(entry))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         482aec86-2739-4525-864d-e805ef7c59d4)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         377939e9-30b0-4cc8-8526-f5b2048110e5)(content(Whitespace\"\\n\"))))(Tile((id \
         ba76040e-3aa7-4f9c-ba3c-2d757299a74c)(label(?))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         f098c206-683e-4831-99e8-5e7207dd8fa0)(content(Whitespace\"\\n\"))))(Secondary((id \
         ee7770c3-1451-4202-9bee-9999809b4ad6)(content(Whitespace\"\\n\"))))(Secondary((id \
         e0a73d23-be4c-433d-8528-b19b8ad80a82)(content(Whitespace\"\\n\"))))(Secondary((id \
         1fd7c70c-bf62-47ab-a8f7-78efc4564c86)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         063ed444-83f4-4e5d-9532-ba7ce1c3c7e7)(content(Whitespace\"\\n\"))))(Secondary((id \
         48e655d0-0655-4b0d-ab30-473fad3b54f3)(content(Whitespace\"\\n\"))))(Secondary((id \
         9e576f06-49ce-409a-8324-d09fcdffaa41)(content(Comment\"# Combine: \
         filter night entries, then extract names #\"))))(Secondary((id \
         3dd38e05-6aef-406e-acfc-1a722bf12c55)(content(Whitespace\"\\n\"))))(Tile((id \
         f7a9b5ff-290d-4c89-8f4e-c7eba6173be5)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         05a3420f-33e7-4f9e-a4ad-220e94fcbcc6)(content(Whitespace\" \
         \"))))(Tile((id \
         f6b1f6d4-fa62-442a-92a2-9f1ab6df74a5)(label(night_names))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         4e76770a-da32-488c-b716-74848bb168ae)(content(Whitespace\" \
         \")))))((Secondary((id \
         e45f97a8-e467-40c6-a256-d3b156614753)(content(Whitespace\"\\n\"))))(Tile((id \
         e241ff9c-6ee8-4eb5-9671-d4ad1b768aea)(label(?))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         e6220b26-77d2-4238-998c-5b4f153760f7)(content(Whitespace\"\\n\"))))(Secondary((id \
         a8623bb8-a597-49f6-9ce1-7b45bf660cdd)(content(Whitespace\"\\n\"))))(Secondary((id \
         770352e2-bffe-42ea-b2a6-e6cca169502e)(content(Whitespace\"\\n\"))))(Secondary((id \
         eb4e8e6f-22eb-4d9d-bb5f-a18e33d0415f)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         daf1d0f4-8296-4924-8eaf-b05ae251ed7e)(content(Whitespace\"\\n\"))))(Secondary((id \
         8be5f4f5-7a33-4bb2-944c-d5ba5ff41aaf)(content(Whitespace\"\\n\"))))(Tile((id \
         df1857a0-5789-418c-988d-d05719e9d783)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         ee3153dd-c3f0-4133-8ff1-4bdca4946379)(content(Whitespace\" \
         \"))))(Tile((id \
         00dea206-5325-4f6d-adce-6d918f6aa6b7)(label(night_names))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         5324f1fd-6a20-4dfc-8d1d-0aed16632cdf)(content(Whitespace\" \
         \"))))(Tile((id \
         a8af3494-9ea1-4547-9313-9e4a4a02d586)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         d637721a-84e6-4535-b10d-6068baeaf2e9)(content(Whitespace\" \
         \"))))(Tile((id 6bac31c9-d155-4014-a756-3e8f8a1fcf6c)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         5efb25a1-5507-445a-b0d5-9e6db3cb95c1)(label(\"\\\"Moonbloom\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         572529ad-682c-406d-8b6e-812e5e5e96bb)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         a809f165-519d-44d9-a046-6e7b7b5cfabe)(content(Whitespace\" \
         \"))))(Tile((id \
         4d8c5a5b-2e5d-4d6f-96d2-2509a0678883)(label(\"\\\"Starfern\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         b59425ab-c64c-4e24-b868-17bb0a6a1cdf)(content(Whitespace\" \
         \")))))))))(Tile((id \
         2dcb1c94-a051-4407-a34f-e247e5569890)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         1cedd19a-99d5-4c2f-9c05-9ed9bab2edcb)(content(Whitespace\"\\n\"))))(Secondary((id \
         71898df6-dace-4338-a9b9-828138bb60f3)(content(Whitespace\"\\n\"))))(Tile((id \
         33ed3c32-5506-4a2f-8102-6329ed3c4c08)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         3c049fab-b06d-4fe5-a27c-c9598fd8533b)(content(Whitespace\" \
         \"))))(Tile((id \
         672dce10-e5c9-4f2e-888b-22d6f793d6b1)(label(is_night))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         82ece40e-4f3f-4437-b78a-f1bcd14b6262)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         1f6fca00-cbd8-42b8-8188-ac11657ede7a)(label(\"\\\"Moonbloom [night] \
         200ml\\\"\"))(mold((out Exp)(in_())(nibs(((shape Convex)(sort \
         Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         02ec2257-35bc-4282-b5de-c73db8e4393d)(content(Whitespace\" \
         \"))))(Tile((id \
         b12e917d-082b-4ebc-8dd2-1910d5ca4336)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         b2fc2055-a1bb-4e23-9025-ffd9ce100069)(content(Whitespace\" \
         \"))))(Tile((id \
         99497fd8-14da-48bd-9dfb-21807c3c61ad)(label(true))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         99e993c7-e53d-4d59-83a7-19a170c2d290)(content(Whitespace\" \
         \")))))))))(Tile((id \
         4fd0f003-5a41-465f-9ba5-fa17a88e3a4a)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         226deb53-771f-4fd6-8809-6b8fb5697d2c)(content(Whitespace\"\\n\"))))(Secondary((id \
         a843dece-b349-4120-a34f-a245d6787f38)(content(Whitespace\"\\n\"))))(Tile((id \
         188fdf5a-ceea-4322-8f33-676ebc5959de)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         eaf36d13-b35a-4a8c-b026-135f4cc0a7ea)(content(Whitespace\" \
         \"))))(Tile((id \
         f27551e5-7955-4e7d-b6ab-2bf638fa6a7b)(label(is_night))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         9c73aa6b-79e4-463b-98c0-32c3baa85621)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         0ad7827f-b8d5-4184-81d1-3c4dbbeb0ec6)(label(\"\\\"Duskrose [day] \
         150ml\\\"\"))(mold((out Exp)(in_())(nibs(((shape Convex)(sort \
         Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         8d9da85c-4919-46b0-95c8-c6b3399a6046)(content(Whitespace\" \
         \"))))(Tile((id \
         00113e77-a71e-45c8-a29d-403181eed480)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         3f234ea4-f7e5-4852-bab7-86b888d015d2)(content(Whitespace\" \
         \"))))(Tile((id \
         2ecc2136-70f3-477a-a920-5e618b2a70b8)(label(false))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         ddd17b1d-1749-4928-b663-09aab15ba673)(content(Whitespace\" \
         \")))))))))(Tile((id \
         2466ee3e-fcf4-4eed-a677-39e9147ce875)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         3bb73212-5be8-4004-becc-3957606df481)(content(Whitespace\"\\n\"))))(Secondary((id \
         7b65f097-c692-462f-95d4-f3df0eae521f)(content(Whitespace\"\\n\"))))(Tile((id \
         1303a7a5-b064-4149-aad1-2b95c1ea879c)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         f2aa3616-4891-4ee9-87cf-cca6f334825a)(content(Whitespace\" \
         \"))))(Tile((id \
         6e620e2a-82e6-4a09-bc10-8d7dd71dcd40)(label(extract_name))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         08f3c3c8-5e4e-4d32-8a20-634c08d4326d)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         e09af34e-ecaf-4f1b-b96a-4265af3cdd01)(label(\"\\\"Starfern [night] \
         175ml\\\"\"))(mold((out Exp)(in_())(nibs(((shape Convex)(sort \
         Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         52937e52-38dd-4df8-bd37-c9e7d710dd0f)(content(Whitespace\" \
         \"))))(Tile((id \
         6774a0eb-23a2-4bce-ad4d-849316259995)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         37be8532-012b-48b7-a930-c17774119d2e)(content(Whitespace\" \
         \"))))(Tile((id \
         abc18a14-c93e-47d6-bef2-55683969771a)(label(\"\\\"Starfern\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         c97b5606-4dd2-4c12-b55a-13014a207499)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         5ec487bf-d49d-4b7a-aead-9919a0058758)(content(Whitespace\"\\n\")))))";
      backup_text =
        "# NIGHT BLOOM FILTER TASK                        #\n\
         #                                                #\n\
         # A plant catalog has entries like:              #\n\
         #   \"Moonbloom [night] 200ml\"                    #\n\
         #   \"Duskrose [day] 150ml\"                        #\n\
         #                                                #\n\
         # Filter to night-blooming plants and extract    #\n\
         # just their names: [\"Moonbloom\", \"Starfern\"]    #\n\
         #                                                #\n\
         # Steps:                                         #\n\
         #   1. is_night: check if entry contains \"night\" #\n\
         #   2. extract_name: get the first word          #\n\
         #   3. Combine with filter and map               #\n\
         #                                                #\n\
         # Available functions:                           #\n\
         #   string_match(pattern, str) -> Bool           #\n\
         #   string_split(separator, str) -> [String]     #\n\
         #   nth(list, index) -> element                  #\n\
         #   filter(list, predicate) -> list              #\n\
         #   map(list, fn) -> list                        #\n\
         #                                                #\n\
         # Note: string_match uses regex patterns.        #\n\
         # The pattern \"[abc]\" matches any of a, b, c.   #\n\
         #                                                #\n\
         # Tip: Use probes to see what your pattern       #\n\
         # actually matches -- regex can be surprising!   #\n\n\
         let entries = [\n\
         \"Moonbloom [night] 200ml\",\n\
         \"Duskrose [day] 150ml\",\n\
         \"Starfern [night] 175ml\",\n\
         \"Ghostvine [day] 100ml\"\n\
         ] in\n\n\
         # Check if entry is a night-blooming plant #\n\
         let is_night: String -> Bool = fun entry ->\n\
         ?\n\n\n\n\
         in\n\n\
         # Extract just the plant name from an entry #\n\
         let extract_name: String -> String = fun entry ->\n\
         ?\n\n\n\n\
         in\n\n\
         # Combine: filter night entries, then extract names #\n\
         let night_names =\n\
         ?\n\n\n\n\
         in\n\n\
         test night_names == [\"Moonbloom\", \"Starfern\"] end;\n\n\
         test is_night(\"Moonbloom [night] 200ml\") == true end;\n\n\
         test is_night(\"Duskrose [day] 150ml\") == false end;\n\n\
         test extract_name(\"Starfern [night] 175ml\") == \"Starfern\" end\n";
      refractors = "()";
    } )
