let out : string * Haz3lcore.PersistentSegment.t =
  ( "Study / writing / clamp / clamp-sketch",
    {
      segment =
        "((Secondary((id \
         66aed4e8-4d0e-4072-9e4c-678fec8b98a0)(content(Comment\"# CLAMP \
         TASK                                   #\"))))(Secondary((id \
         0308f4aa-3e52-4fce-9e91-5f6338509154)(content(Whitespace\"\\n\"))))(Secondary((id \
         9680a2b4-3d1c-4449-8de3-c648df4a572f)(content(Comment\"#                                              \
         #\"))))(Secondary((id \
         613d08e0-682e-427e-9eba-ce722c3e4164)(content(Whitespace\"\\n\"))))(Secondary((id \
         bc14ca6d-9776-46da-938b-b5c7b3e31d95)(content(Comment\"# Implement \
         clamp: constrain a number to be    #\"))))(Secondary((id \
         950167dd-d6fc-4870-bd65-3e44f9fdf004)(content(Whitespace\"\\n\"))))(Secondary((id \
         b2201a9b-158a-42d5-839a-aec5d89c9b52)(content(Comment\"# within a \
         given range [lo, hi].               #\"))))(Secondary((id \
         d0e05ef4-534d-445f-9d2d-2a97b8606090)(content(Whitespace\"\\n\"))))(Secondary((id \
         93af79a0-f15e-4956-a85f-6c67e7241484)(content(Comment\"#                                              \
         #\"))))(Secondary((id \
         43f2224d-8191-4d9a-8326-107b768be4f7)(content(Whitespace\"\\n\"))))(Secondary((id \
         ff2a056a-9e9f-4613-92b0-a34aab22908d)(content(Comment\"# If x < lo, \
         return lo                         #\"))))(Secondary((id \
         5e1beb3a-29cf-461b-8cd3-3080445ff461)(content(Whitespace\"\\n\"))))(Secondary((id \
         231cd854-67b3-4a7e-b984-a7bbc8c61da7)(content(Comment\"# If x > hi, \
         return hi                         #\"))))(Secondary((id \
         65afc6ed-e549-4e9e-a7c6-fef333450e5b)(content(Whitespace\"\\n\"))))(Secondary((id \
         14cd8867-1a7f-44ef-b347-b016843472c9)(content(Comment\"# Otherwise, \
         return x                          #\"))))(Secondary((id \
         3b29ef78-7bb8-410d-b768-d82974d1aea4)(content(Whitespace\"\\n\"))))(Secondary((id \
         16514c8f-af69-4201-bc37-b8a0dd47101e)(content(Comment\"#                                              \
         #\"))))(Secondary((id \
         27fbb199-96f0-4f51-8ff4-639f99ed6b8a)(content(Whitespace\"\\n\"))))(Secondary((id \
         6b0c5c62-1ffc-4a3c-8012-9f99156e1aca)(content(Comment\"# \
         Examples:                                    #\"))))(Secondary((id \
         2cbcece7-b227-4639-9f7a-30d82849f869)(content(Whitespace\"\\n\"))))(Secondary((id \
         d79b09fd-d248-4c1f-9429-755944c2dee2)(content(Comment\"#   clamp(5, \
         0, 10) == 5    (in range)         #\"))))(Secondary((id \
         a235baf2-5383-4cc0-b298-71e31cfddf7c)(content(Whitespace\"\\n\"))))(Secondary((id \
         257f3db2-e822-4d58-a625-a97da8dc7b48)(content(Comment\"#   clamp(-3, \
         0, 10) == 0   (below min)        #\"))))(Secondary((id \
         bdeb2549-ff51-48ff-b7b8-ace7d6c430b7)(content(Whitespace\"\\n\"))))(Secondary((id \
         06d1c65c-df4b-4994-9dac-47d987c783a0)(content(Comment\"#   clamp(15, \
         0, 10) == 10  (above max)        #\"))))(Secondary((id \
         c32e0afd-8964-4bd1-9c68-2f925b53d581)(content(Whitespace\"\\n\"))))(Secondary((id \
         f414d606-9d25-4c10-ac6c-3c5b13b16c1a)(content(Comment\"#                                              \
         #\"))))(Secondary((id \
         94089563-5813-4096-a289-62e8e805cf85)(content(Whitespace\"\\n\"))))(Secondary((id \
         cf4b2578-23a3-46d7-9e20-d99fb668719b)(content(Comment\"# Syntax \
         reminder:                             #\"))))(Secondary((id \
         740d38a8-1e3d-4353-8c11-bcc2d1d9f04e)(content(Whitespace\"\\n\"))))(Secondary((id \
         15a2f43a-50bb-4479-afe8-769946758149)(content(Comment\"#   if cond \
         then expr1 else expr2              #\"))))(Secondary((id \
         929c279b-2553-40fa-92a8-8d8959913de8)(content(Whitespace\"\\n\"))))(Secondary((id \
         043803e8-1e04-48c4-a344-ec8cc8e775c6)(content(Comment\"#   \
         Comparisons: <, >, <=, >=, ==              #\"))))(Secondary((id \
         aca7811f-8654-43dc-945e-6da8303e5d5d)(content(Whitespace\"\\n\"))))(Secondary((id \
         47738f2d-d158-49bc-9deb-0b467363471d)(content(Comment\"#                                              \
         #\"))))(Secondary((id \
         7fde424d-9a95-4bca-b63b-cf6dbcb29536)(content(Whitespace\"\\n\"))))(Secondary((id \
         12056820-5712-447c-9def-48e5e81a47dc)(content(Comment\"# Tip: Turn on \
         auto-probe (microscope toggle)  #\"))))(Secondary((id \
         80c7a4bd-04b3-4b97-abeb-ac731aafce3f)(content(Whitespace\"\\n\"))))(Secondary((id \
         1c1a1f43-a48a-497f-9e2f-ceca34b571ac)(content(Comment\"# to see which \
         branch is taken for each test.  #\"))))(Secondary((id \
         52009de6-d3d3-41c0-bd57-61955c42a827)(content(Whitespace\"\\n\"))))(Secondary((id \
         a320daa2-ef65-4c71-926a-fa9093e93074)(content(Whitespace\"\\n\"))))(Tile((id \
         12d07c88-5e45-4160-ae2e-15066853936e)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         81229213-533b-4614-a62c-bfa5b8092cf6)(content(Whitespace\" \
         \"))))(Tile((id \
         98af44a1-17b4-4192-b40e-ed65e99baacd)(label(clamp))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         46b236fb-06e1-4246-ab9f-dc94e7a3f0d3)(content(Whitespace\" \
         \")))))((Secondary((id \
         8acfe3a0-dc63-45a2-aedd-d84ba3e3f24b)(content(Whitespace\" \
         \"))))(Tile((id d6b114ae-99ee-4d7e-8bc2-a934187bf767)(label(fun \
         ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 37))(sort Exp))))))(shards(0 \
         1))(children(((Secondary((id \
         81abcd85-72c3-4e45-a3d0-def2e91fcc48)(content(Whitespace\" \
         \"))))(Tile((id \
         5375f13d-d356-4c59-80b2-f41a5b4f50c9)(label(\"(\"\")\"))(mold((out \
         Pat)(in_(Pat))(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0 1))(children(((Tile((id \
         94aef36a-2810-4a42-8007-1ecb7bbd626f)(label(x))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         dff80c96-3130-4bb0-9f60-24ada990b2cb)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 44))(sort Pat))((shape(Concave \
         44))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         077de98d-0695-42d8-b97c-162ab5134e28)(content(Whitespace\" \
         \"))))(Tile((id \
         cf92ac09-71c9-4388-94b5-92a9bbb14080)(label(lo))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         35ddcab9-130c-4a03-b580-e856bf175da9)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 44))(sort Pat))((shape(Concave \
         44))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         e33c88a1-721d-42db-9891-a94e4594d1b0)(content(Whitespace\" \
         \"))))(Tile((id \
         0e156846-7c51-4d37-a0df-a53fb6916a14)(label(hi))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children()))))))))(Secondary((id \
         75572957-c3f8-4457-8148-1dbb9c851f6d)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         80fb3485-595e-40ec-beef-4359dd3a09a6)(content(Whitespace\"\\n\"))))(Tile((id \
         463c07f2-48d8-4ab7-9b81-51515347b71f)(label(?))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         adc5bc81-d62d-4cff-889c-0f5de9a662fc)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         cf93b1e7-199e-4bc1-ba27-6609f3095a06)(content(Whitespace\"\\n\"))))(Secondary((id \
         bd1f6bb4-f663-4f4c-887c-cd65281cd85a)(content(Whitespace\"\\n\"))))(Tile((id \
         0a177ea2-5a4f-496a-a504-426042e37a42)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         93fa82ff-9642-40c0-8d90-25f7318af241)(content(Whitespace\"\\n\"))))(Tile((id \
         73c46431-f15b-4f61-97b6-c282d14c35ba)(label(clamp))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         6d144988-4e1f-43a5-98de-5f2498cb6564)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         3bedfc2b-4518-4ec4-b736-58d04d1cdb02)(label(5))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         c2fef67a-bc97-4665-a961-af0387c4a8dd)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         9f5e6feb-0e6a-4ebc-93d2-d042fbec8f1f)(content(Whitespace\" \
         \"))))(Tile((id \
         5b2fa6e0-3df4-4837-8d0a-be6d73281b97)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         a33f1c50-8b9d-47e6-8ac7-3fcc256b356a)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         bc0bd317-4a7b-4595-b599-e66d714464a9)(content(Whitespace\" \
         \"))))(Tile((id \
         1569850f-4c63-4cf9-9e9b-19ffbcdf8a5f)(label(10))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         19e3098a-e982-4edf-a421-098e466add0f)(content(Whitespace\"\\n\"))))(Tile((id \
         bdbabd85-77c2-42c8-b891-ccd87e8d65a4)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         d3cc412d-4f81-4878-bf34-d2fcb92e1f54)(content(Whitespace\" \
         \"))))(Tile((id \
         c49a3dfd-d476-4c6d-a636-f019673170b8)(label(5))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         74693ec6-3989-47d5-95de-49b054252a23)(content(Whitespace\"\\n\")))))))))(Tile((id \
         3eb37fe1-c613-45fc-86ed-77224cb76077)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         7b0e2a63-4225-49cc-a46a-5ec10c5760d4)(content(Whitespace\"\\n\"))))(Secondary((id \
         d2d18f6f-55b4-46cb-80f5-2738069e3159)(content(Whitespace\"\\n\"))))(Tile((id \
         d9a06e0b-e070-46b9-bbfe-7c34e8254cce)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         3b4d3947-9f50-4410-b184-60e4fc9d64fa)(content(Whitespace\"\\n\"))))(Tile((id \
         0124d831-0a76-4fbe-ae0b-dbbca3bb0b53)(label(clamp))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         6ce3d63f-2fa2-4778-a8be-0a8a5a0ab63b)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         e022c1c2-7916-4b57-8ebd-14dd291b1cad)(label(-))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape(Concave 25))(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         1ceafcb0-e2c6-4f87-b0a1-fcc41fd433fd)(label(3))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         a9be2ad7-66f2-4d83-b74e-0b54d3e1f345)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         98124f6d-9ba4-44fa-9264-47708c020630)(content(Whitespace\" \
         \"))))(Tile((id \
         3e1afef4-ff0d-4b4e-ada1-73714576fead)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         89365d0d-4734-47fb-85a9-39dc7f74df03)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         5d156af1-9a36-49c8-af61-351f17bd8acd)(content(Whitespace\" \
         \"))))(Tile((id \
         4f7f614b-1687-4395-bb1e-6de0604f9dd5)(label(10))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         2ec9bae7-f09f-4f58-b6e7-a2dabc489b30)(content(Whitespace\"\\n\"))))(Tile((id \
         f10001a0-30c4-4b0e-b04a-e30e410e0186)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         fe95da87-4a38-4b17-8df8-7fe6e96c40f1)(content(Whitespace\" \
         \"))))(Tile((id \
         8c9a4f44-589e-4743-9926-c4fe47907569)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         ae1d0204-aab6-4a40-b539-41de481d10bd)(content(Whitespace\"\\n\")))))))))(Tile((id \
         ab4a796a-27d2-4ab1-b828-799e5c756faa)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         d2f7d622-6041-4f7c-bed7-6531472d86d7)(content(Whitespace\"\\n\"))))(Secondary((id \
         865eb44c-2b12-4bbc-894a-d4e87de24027)(content(Whitespace\"\\n\"))))(Tile((id \
         12b9b8e5-082e-482f-b537-fd11c9c803e9)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         60ac697c-6ff5-4758-8e61-46af44cc3557)(content(Whitespace\"\\n\"))))(Tile((id \
         a12ee300-feee-426f-a025-49e3a54f896c)(label(clamp))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         a31c87a2-13e3-4f09-beac-3e5dadbfb046)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         b4dfbf51-e5fb-472a-8f31-c8d20a5aa817)(label(15))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         e6b1bb5f-ecda-4ceb-8ee7-6ec71dd3d96e)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         b295b6d2-73ad-4fa2-9a15-c3fee409f94d)(content(Whitespace\" \
         \"))))(Tile((id \
         427ffdaa-aea7-4dd1-9e3a-7f7d8ba90aab)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         2967b655-f683-4d6a-9118-653eefc2a6fd)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         ef9f7c2d-87a5-47e2-878c-c4193eb31692)(content(Whitespace\" \
         \"))))(Tile((id \
         0504b2c0-5492-45bd-91b7-41a4f332207f)(label(10))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         f97f12dd-c5c3-4fbe-bb01-0e05912cd2c6)(content(Whitespace\"\\n\"))))(Tile((id \
         e6010a15-b4c9-4ab8-9035-5bddbffacdb4)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         b1952590-21b0-4ca7-95f7-63bd692d05f7)(content(Whitespace\" \
         \"))))(Tile((id \
         d3d2dc49-82ce-40d1-a6f9-f31b2e03f574)(label(10))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         ea04b110-45bb-4420-9076-780a5bc9c842)(content(Whitespace\"\\n\")))))))))(Tile((id \
         b26b5b79-7732-469f-9b4c-1d3ed976eb8d)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         776b911f-a55c-4a66-914a-439cce94d108)(content(Whitespace\"\\n\"))))(Secondary((id \
         f8235661-6b23-4bab-93e2-ccda09e3c3b6)(content(Whitespace\"\\n\"))))(Tile((id \
         424eb795-5c17-4975-9794-5386d8bc7205)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         91ac8eba-a869-48b7-be5c-b2873c6897f4)(content(Whitespace\"\\n\"))))(Tile((id \
         3072ea2d-2fed-4fd4-9046-e44fde6e1d89)(label(clamp))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         674c81f5-5423-4c5d-83e9-1e69c37ec8fd)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         a6dd0bd0-a58e-4d13-afd9-569c1c01ee5d)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         ca1c12dc-30a9-459e-990c-1fafda03df03)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         82bf1718-6dee-4159-b32c-dfc0e5f7bad8)(content(Whitespace\" \
         \"))))(Tile((id \
         be60a60d-3a78-4b66-86b4-9493734f4d53)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         7598b5a8-5f7d-43b8-a74e-93022234a6cb)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         2fe880c2-e71f-415c-9952-629fbeea4948)(content(Whitespace\" \
         \"))))(Tile((id \
         1d9d6781-4c8b-4cba-b9cc-4cb0abf517db)(label(10))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         bb4002a9-2ede-4d0b-baaa-133be78ca135)(content(Whitespace\"\\n\"))))(Tile((id \
         156a36be-382e-41a2-adea-9c34317d307f)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         014df5e5-e594-471f-85a1-7ca76ed4d56f)(content(Whitespace\" \
         \"))))(Tile((id \
         ee2d2510-0988-42bc-8cd4-a772971c2847)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         a3d63d87-0d9a-4432-9466-e5d1f6b222c7)(content(Whitespace\"\\n\")))))))))(Tile((id \
         999d9982-2ee4-4738-a4c4-de2386dba9a9)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         c68c0fc6-0d42-41c6-b9a9-35ba6848ec69)(content(Whitespace\"\\n\"))))(Secondary((id \
         0512d246-c5b7-41e6-b4cb-887e3825df64)(content(Whitespace\"\\n\"))))(Tile((id \
         ca5837d9-7b2d-45cf-8ba2-c6dfd8349f4e)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         07fb38ac-a838-4963-857f-35cec46b0617)(content(Whitespace\"\\n\"))))(Tile((id \
         f776236a-c6ac-4bc0-aa05-4f592368bb22)(label(clamp))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         31a13e02-a6fc-4e6f-8f66-9407ce098a79)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         c94b2bf6-343f-4113-bb81-8eef13477c0d)(label(10))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         25445ba7-cfc4-4e14-8e08-87ad13668114)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         354abe0d-ad24-4453-ac99-521149652a1e)(content(Whitespace\" \
         \"))))(Tile((id \
         a47d32e1-efc3-4572-8e9b-04e98fa0adf2)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         4103e323-98c3-4fd9-adf3-ad59bd3dd9f0)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         0284c14d-534b-402e-985c-cb9ace6f18b5)(content(Whitespace\" \
         \"))))(Tile((id \
         7e4d1991-40df-46dc-aeeb-84ef5dfdd09a)(label(10))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         c5d17086-5987-458c-9478-54f76429a6d1)(content(Whitespace\"\\n\"))))(Tile((id \
         7fbc7e53-a23b-4b4b-b92b-52fbdb73e521)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         1b2aa9e6-10ef-4606-8fc8-79a4fe4048e4)(content(Whitespace\" \
         \"))))(Tile((id \
         448fb12e-50f3-4853-9009-a56b6f18f455)(label(10))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         ff2eca71-8ffe-4cec-b06b-5895a4f12063)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         332702dd-d989-4f8f-881f-302fac294171)(content(Whitespace\"\\n\")))))";
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
