let out : string * Haz3lcore.PersistentSegment.t =
  ( "Study / writing / clamp / clamp-sketch",
    {
      segment =
        "((Secondary((id \
         9e129b08-2a42-4052-a184-c1dcf8eadb66)(content(Comment\"# CLAMP \
         TASK                                   #\"))))(Secondary((id \
         73af0de9-2ec7-43b1-9771-16539dabfbdc)(content(Whitespace\"\\n\"))))(Secondary((id \
         f004be73-b892-456a-ac08-0c7ed380599d)(content(Comment\"#                                              \
         #\"))))(Secondary((id \
         bb2f72cb-4f3b-46e5-84db-e2630933d736)(content(Whitespace\"\\n\"))))(Secondary((id \
         4c3cf838-ca34-4d5e-9dff-ae3a22f47c1d)(content(Comment\"# Implement \
         clamp: constrain a number to be    #\"))))(Secondary((id \
         60cf2868-5315-454b-b171-6dab2a9905bb)(content(Whitespace\"\\n\"))))(Secondary((id \
         9ad75eeb-17c2-4690-a906-7cf61da5067d)(content(Comment\"# within a \
         given range [lo, hi].               #\"))))(Secondary((id \
         7af3533f-d272-4efd-8903-83a768e893f8)(content(Whitespace\"\\n\"))))(Secondary((id \
         7c9216a1-1579-49eb-bb70-507dd92e786a)(content(Comment\"#                                              \
         #\"))))(Secondary((id \
         1866f776-e13f-4958-a7e3-c2ee13165d66)(content(Whitespace\"\\n\"))))(Secondary((id \
         6808a203-affb-4fdb-abd4-72056932971c)(content(Comment\"# If x < lo, \
         return lo                         #\"))))(Secondary((id \
         18a4a450-7618-4daf-be6e-a2c05cc6bc25)(content(Whitespace\"\\n\"))))(Secondary((id \
         01f01b14-9a35-4ad2-91b0-8cdc0ff6a1a4)(content(Comment\"# If x > hi, \
         return hi                         #\"))))(Secondary((id \
         001337e2-ab92-493d-8237-2610c45f3119)(content(Whitespace\"\\n\"))))(Secondary((id \
         f3cf0e28-b386-4e50-b33d-1ed2071d0d78)(content(Comment\"# Otherwise, \
         return x                          #\"))))(Secondary((id \
         eb6eced3-fd96-4738-963e-400694957f0d)(content(Whitespace\"\\n\"))))(Secondary((id \
         f0a2aa94-90cd-4525-a3a1-c1237d643583)(content(Comment\"#                                              \
         #\"))))(Secondary((id \
         8b815089-8e82-41c3-8742-2e49553a34b3)(content(Whitespace\"\\n\"))))(Secondary((id \
         23d03e92-ef3e-4aea-a9b5-b5e266a3a3ee)(content(Comment\"# \
         Examples:                                    #\"))))(Secondary((id \
         2a82e926-25df-4708-84b7-c229f5b61451)(content(Whitespace\"\\n\"))))(Secondary((id \
         15c25ffb-d40f-4b3e-8e2a-2b8d9abc3547)(content(Comment\"#   clamp(5, \
         0, 10) == 5    (in range)         #\"))))(Secondary((id \
         b0c87b5f-8756-4f1d-ad67-3e0d6a11a658)(content(Whitespace\"\\n\"))))(Secondary((id \
         97ea94c4-e5ca-4024-bfe4-89e2165eb9ce)(content(Comment\"#   clamp(-3, \
         0, 10) == 0   (below min)        #\"))))(Secondary((id \
         041c1d34-628d-4caf-bb62-c208827f7d48)(content(Whitespace\"\\n\"))))(Secondary((id \
         5b1f0b86-2347-4f65-8d13-262768d19850)(content(Comment\"#   clamp(15, \
         0, 10) == 10  (above max)        #\"))))(Secondary((id \
         465247d2-fc0c-4fd1-b00b-040234f75f76)(content(Whitespace\"\\n\"))))(Secondary((id \
         93db3e79-67cb-4661-903c-09cdfa2927b9)(content(Comment\"#                                              \
         #\"))))(Secondary((id \
         907d3bd5-8fc6-4cf3-8fe5-6d542dd8354d)(content(Whitespace\"\\n\"))))(Secondary((id \
         19b3e5cf-55ef-4c08-8728-7e1accb4e838)(content(Comment\"# Syntax \
         reminder:                             #\"))))(Secondary((id \
         de9686ec-0e7b-4050-8c61-368bb0351c42)(content(Whitespace\"\\n\"))))(Secondary((id \
         cfbc714e-2883-4f9c-9f0c-0ebbf08519e7)(content(Comment\"#   if cond \
         then expr1 else expr2              #\"))))(Secondary((id \
         fc2be0b3-1956-4598-b42f-61a1a9ca8e08)(content(Whitespace\"\\n\"))))(Secondary((id \
         440c3c88-49a0-4240-9c60-8bad61e67f63)(content(Comment\"#   \
         Comparisons: <, >, <=, >=, ==              #\"))))(Secondary((id \
         6fba13bd-2dbd-4890-a3b3-48ec1e70afb5)(content(Whitespace\"\\n\"))))(Secondary((id \
         9d224180-f99d-47a8-95f9-a11a33aa423d)(content(Comment\"#                                              \
         #\"))))(Secondary((id \
         05a1d919-c1d9-4753-9050-8e32a91522c8)(content(Whitespace\"\\n\"))))(Secondary((id \
         9c69496c-3356-4498-bed7-46f6619113f4)(content(Comment\"# Tip: Turn on \
         auto-probe (microscope toggle)  #\"))))(Secondary((id \
         73ae9139-7f11-473a-9e85-f4e1c353b851)(content(Whitespace\"\\n\"))))(Secondary((id \
         64b4f825-94df-48a9-813c-22de5759d57a)(content(Comment\"# to see which \
         branch is taken for each test.  #\"))))(Secondary((id \
         e290e88c-8400-463f-9cc2-97aa0f2b0549)(content(Whitespace\"\\n\"))))(Secondary((id \
         cd4c101f-9ca7-499e-9005-0cc7cc420221)(content(Whitespace\"\\n\"))))(Tile((id \
         6ed74f41-d7ea-4275-b2dc-e4f5ad7001f9)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         1037b65c-8ad5-4da9-ba84-8a53b62d42bc)(content(Whitespace\" \
         \"))))(Tile((id \
         e048a865-a43e-4d92-8593-7923687af0a1)(label(clamp))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         8d4c1588-e5b1-4db9-b968-2b5536f2c70f)(content(Whitespace\" \
         \")))))((Secondary((id \
         38b614ef-75db-4107-9edb-cb4ff23fd96c)(content(Whitespace\" \
         \"))))(Tile((id c2eac3ca-59d1-4e98-b2ae-4e2a1ed99f28)(label(fun \
         ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 37))(sort Exp))))))(shards(0 \
         1))(children(((Secondary((id \
         c90adc69-80df-44b2-b935-8331b41c0775)(content(Whitespace\" \
         \"))))(Tile((id \
         c4a4bea9-904c-4f54-8920-663c1b5a7fc1)(label(\"(\"\")\"))(mold((out \
         Pat)(in_(Pat))(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0 1))(children(((Tile((id \
         52bfc054-2341-4114-b1ce-6a69f2c9c400)(label(x))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         bfea0329-63bb-4c9f-a6ab-9d94629afce0)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 44))(sort Pat))((shape(Concave \
         44))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         146d58f1-0353-4ade-87f8-f972c0c6aaff)(content(Whitespace\" \
         \"))))(Tile((id \
         5cb7b0ae-6cce-48ac-b6f2-be6fb1948311)(label(lo))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         420ff481-4293-4308-850f-929bb40ee375)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 44))(sort Pat))((shape(Concave \
         44))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         d3479269-c12b-4073-b60a-4043e826917d)(content(Whitespace\" \
         \"))))(Tile((id \
         16e83675-b5d8-4918-928e-cb099ab1d2de)(label(hi))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children()))))))))(Secondary((id \
         93ceb538-6539-479e-97e5-8a2e7e95e2b5)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         192706ac-8778-424d-bc6b-57a384d44eb5)(content(Whitespace\"\\n\"))))(Tile((id \
         fb6e0c97-146f-4851-952d-d26fd028177d)(label(?))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         a7fc7d17-9223-4872-b4ed-9d55f500823c)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         c2ffb2e8-1eb3-43c6-9053-3aa543e3e21a)(content(Whitespace\"\\n\"))))(Secondary((id \
         0ceba4f4-ccfe-4d48-b265-cbf3caa8ea35)(content(Whitespace\"\\n\"))))(Tile((id \
         958f22d1-7649-48cd-a65c-32c1930321ba)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         26467c15-d37e-45a8-ad17-40bb2d654623)(content(Whitespace\"\\n\"))))(Tile((id \
         89d2de1e-60cf-4c49-a99e-acaf149d8c2f)(label(clamp))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         90cb8511-d292-4084-a3a2-fb0a42f3947e)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         3cc9bc12-ac04-4b64-85a4-b93cef77c556)(label(5))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         47a30e0f-92d7-4ca4-836f-169fa05c0128)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         bcf42926-ecba-49cc-ae6d-b8f88fdfece2)(content(Whitespace\" \
         \"))))(Tile((id \
         e93527df-bd41-40e8-ba87-c94cd800b4d6)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         ea1ca868-b836-4a4e-a1a0-2d3732744b4b)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         7b270f4e-2838-4225-8317-344dbf0e0269)(content(Whitespace\" \
         \"))))(Tile((id \
         eb9472c6-a028-4ebb-b25d-16b4bfe8b84e)(label(10))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         c7bd9fc9-6736-4159-8df0-3128d7f40858)(content(Whitespace\"\\n\"))))(Tile((id \
         bafd4089-ecbc-49bf-9250-b2bbcbe99ce3)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         9c395aab-5ceb-4e47-a54f-082d8e6bd339)(content(Whitespace\" \
         \"))))(Tile((id \
         20212e62-fa30-48c3-a084-443361736668)(label(5))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         5693af40-70c9-464f-a7a2-63851f4cf598)(content(Whitespace\"\\n\")))))))))(Tile((id \
         f60099af-2e1e-49e5-acfe-e60f93d89189)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         8472d9ec-70a4-4d87-bd86-6c83f6226600)(content(Whitespace\"\\n\"))))(Secondary((id \
         0f70a4c0-4a73-4d29-a116-46c764218d31)(content(Whitespace\"\\n\"))))(Tile((id \
         11eca179-5956-4d41-8c4c-2689271a81b8)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         85e469d6-103c-47e8-8b42-ca2305b11df3)(content(Whitespace\"\\n\"))))(Tile((id \
         fedfc10e-8f1c-4231-9eb2-59a5f49c3a9b)(label(clamp))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         7a49c859-bfab-4be6-bd73-f1c846303e28)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         e6ea7752-81d1-4985-9614-e2338471b087)(label(-))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape(Concave 25))(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         d26d7174-ec8b-4328-b1f6-8fd8458688d5)(label(3))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         88276cd8-a6a6-4ccc-bb2d-9364a0960a31)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         b3a9f5a3-f113-4a49-9b70-c5dea6d83b7d)(content(Whitespace\" \
         \"))))(Tile((id \
         b109f924-579e-48c3-a67d-47caa168270f)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         39d84c4a-8011-4e26-a371-c3317216db3b)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         4b2cb364-77e4-473f-b3e5-7a65bb9cae2d)(content(Whitespace\" \
         \"))))(Tile((id \
         1f7d7604-3f97-48a6-b6a6-d49c234c0ee7)(label(10))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         2f9ab5d0-1d2f-4722-bcc2-09d9fddb399b)(content(Whitespace\"\\n\"))))(Tile((id \
         7b1d7019-4cab-4ea8-a175-a1883d055442)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         a05196d9-95fe-49e8-bad9-3315c3cfeb65)(content(Whitespace\" \
         \"))))(Tile((id \
         c37f7349-313e-40e8-8a42-318ee1b4af5f)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         fea6db33-b602-46ff-8434-ffe6ffbc8914)(content(Whitespace\"\\n\")))))))))(Tile((id \
         1559402d-b732-46c4-bb24-be9074c66e37)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         295c5375-454c-45c1-8274-eb342d052313)(content(Whitespace\"\\n\"))))(Secondary((id \
         35b535b7-5ed5-48e4-9eb4-9da2f85a8974)(content(Whitespace\"\\n\"))))(Tile((id \
         f24bae05-46d5-4a5c-abb8-9be6109f826b)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         f6d0d015-11c2-4574-aa97-e059b0f63cc9)(content(Whitespace\"\\n\"))))(Tile((id \
         dec12223-d758-4e90-be12-e6c5d907bccb)(label(clamp))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         2da003eb-0e3b-465a-8193-91b7b5941178)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         5c25d273-bbe0-428f-94c1-ae8ea88c5794)(label(15))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         81fb860b-0458-4a5f-90bb-e6fa3f3aeb9f)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         6ff42e5f-74d9-4749-8662-58b0a850aaaa)(content(Whitespace\" \
         \"))))(Tile((id \
         10089333-15fc-44c4-b641-66a830c90e13)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         66c0da75-3a82-42ac-88b0-c208c4104b27)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         45cfe17e-28b6-48d3-a066-b142cc09f6f8)(content(Whitespace\" \
         \"))))(Tile((id \
         20f92b42-401d-475c-ae57-7be2adcedf25)(label(10))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         306bc632-d3e8-41dd-8fce-0ce8308ea26d)(content(Whitespace\"\\n\"))))(Tile((id \
         f75f3943-1cce-44ac-a1d6-9e8800a8eb8e)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         a894ec3a-5b43-4cae-bbed-b4d7882c4096)(content(Whitespace\" \
         \"))))(Tile((id \
         577af406-ca78-49ee-9c3c-2b3d87622645)(label(10))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         b7657f55-a05c-46a2-9066-84e94f8d7896)(content(Whitespace\"\\n\")))))))))(Tile((id \
         ab60b731-e892-4e6f-8d18-361cb0ffd179)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         6755be64-ad3a-464b-bd4d-ff1cef7d76fb)(content(Whitespace\"\\n\"))))(Secondary((id \
         92f82f0f-ddf2-42c4-8f48-21c89a07502b)(content(Whitespace\"\\n\"))))(Tile((id \
         50428d43-94cf-45cc-8e43-46640b307acc)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         f9f306bf-b183-4acb-b001-1bf099683558)(content(Whitespace\"\\n\"))))(Tile((id \
         0310169a-5aae-4f72-89a2-c908aa996f51)(label(clamp))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         e10beda4-0fac-4038-88b1-0512c70699d1)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         28f5dc4c-3d13-4d91-ab34-040de3ac64bd)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         8d7fda5b-46a6-4175-a0ce-74b0fae6662b)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         e70bd7b0-deeb-40e4-95df-6da1409655b4)(content(Whitespace\" \
         \"))))(Tile((id \
         a113759e-52a1-498a-833a-71deb03b1217)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         f12091cc-a558-41e2-8833-d29035cb9257)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         e3f2c29d-c46f-4826-819e-bdcdf12d4598)(content(Whitespace\" \
         \"))))(Tile((id \
         6a45ad27-1fda-4f5c-97fd-aae3fa3d33cc)(label(10))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         5bd6e083-338f-4085-abba-9fdf717d73b5)(content(Whitespace\"\\n\"))))(Tile((id \
         b3c127a6-b7a9-41d5-b1a6-6304950fac64)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         f0cdc58e-a2d4-4adc-9d5c-9d5f9881c86b)(content(Whitespace\" \
         \"))))(Tile((id \
         ee605280-c910-4611-869a-c746d33cd3a4)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         5bf22b85-c2f5-4897-89bf-e3ae07d0dd89)(content(Whitespace\"\\n\")))))))))(Tile((id \
         bb451acd-8450-40d2-93d4-6935238c6cc6)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         29afe4d0-66ee-4c94-8ebe-85bac3c47256)(content(Whitespace\"\\n\"))))(Secondary((id \
         78d7a37a-bacb-4d47-8c84-42ca73d82bcd)(content(Whitespace\"\\n\"))))(Tile((id \
         9848c34c-dd5c-4ddf-aad5-80faa88fde07)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         92812bdd-8305-41e3-b207-347b8cf461df)(content(Whitespace\"\\n\"))))(Tile((id \
         aeec0da5-c1bc-4185-83bf-d802275254d1)(label(clamp))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         e1c89df8-ab6a-4dbf-b968-15122298d778)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         c6c4c983-d9de-4f2f-878d-051cb53442b8)(label(10))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         01039877-8b4e-44ab-ae28-8bf8f3176d94)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         2a3fc723-792e-40e4-9a74-f4d7f9fbdcbf)(content(Whitespace\" \
         \"))))(Tile((id \
         045f2473-dca5-4ea3-a483-8cb421062156)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         000f4f92-0ab7-4719-807b-c511361ae28c)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         5eeb4a8e-e64a-45aa-9273-9bc4e3a0d6eb)(content(Whitespace\" \
         \"))))(Tile((id \
         f14d4b59-df0a-4296-a6d9-b34940686f4d)(label(10))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         705af7dc-0bdf-4bfa-bc3d-7af93c90e789)(content(Whitespace\"\\n\"))))(Tile((id \
         2567da28-5236-41b4-af4e-7d790e9be400)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         b40526df-59ca-4279-bde5-a169f33e789e)(content(Whitespace\" \
         \"))))(Tile((id \
         d1fa5e4e-b5be-40c4-88e5-67c05b2e9561)(label(10))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         9bb080d3-0703-4df4-a243-f82665d7c976)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         775c58c9-8c43-453d-bee2-05b4b98b2dcc)(content(Whitespace\"\\n\")))))";
      backup_text =
        "# CLAMP TASK                                   #\n\
         #                                              #\n\
         # Implement clamp: constrain a number to be    #\n\
         # within a given range [lo, hi].               #\n\
         #                                              #\n\
         # If x < lo, return lo                         #\n\
         # If x > hi, return hi                         #\n\
         # Otherwise, return x                          #\n\
         #                                              #\n\
         # Examples:                                    #\n\
         #   clamp(5, 0, 10) == 5    (in range)         #\n\
         #   clamp(-3, 0, 10) == 0   (below min)        #\n\
         #   clamp(15, 0, 10) == 10  (above max)        #\n\
         #                                              #\n\
         # Syntax reminder:                             #\n\
         #   if cond then expr1 else expr2              #\n\
         #   Comparisons: <, >, <=, >=, ==              #\n\
         #                                              #\n\
         # Tip: Turn on auto-probe (microscope toggle)  #\n\
         # to see which branch is taken for each test.  #\n\n\
         let clamp = fun (x, lo, hi) ->\n\
         ?\n\
         in\n\n\
         test\n\
         clamp(5, 0, 10)\n\
         == 5\n\
         end;\n\n\
         test\n\
         clamp(-3, 0, 10)\n\
         == 0\n\
         end;\n\n\
         test\n\
         clamp(15, 0, 10)\n\
         == 10\n\
         end;\n\n\
         test\n\
         clamp(0, 0, 10)\n\
         == 0\n\
         end;\n\n\
         test\n\
         clamp(10, 0, 10)\n\
         == 10\n\
         end\n";
      refractors = "()";
    } )
