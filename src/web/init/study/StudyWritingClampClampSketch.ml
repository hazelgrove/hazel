let out : string * Haz3lcore.PersistentSegment.t =
  ( "Study / writing / clamp / clamp-sketch",
    {
      segment =
        "((Secondary((id \
         6ca1bdea-683e-4b08-ab49-e7353937d48e)(content(Comment\"# CLAMP \
         TASK                                   #\"))))(Secondary((id \
         12934359-4d25-4cbf-a9fa-f4bcb2265fb3)(content(Whitespace\"\\n\"))))(Secondary((id \
         483033f5-c0b9-4054-bdee-a93a35024d29)(content(Comment\"#                                              \
         #\"))))(Secondary((id \
         23e86564-8a38-41ce-b8df-15961c53cd61)(content(Whitespace\"\\n\"))))(Secondary((id \
         0584ebfb-cfe7-42c3-bbd5-9f039d47c0af)(content(Comment\"# Implement \
         clamp: constrain a number to be    #\"))))(Secondary((id \
         efe984cb-da3c-4c83-9638-2e5161c72a14)(content(Whitespace\"\\n\"))))(Secondary((id \
         2b7d84bd-81a6-4267-b64e-9dc3581690fa)(content(Comment\"# within a \
         given range [lo, hi].               #\"))))(Secondary((id \
         584ef9bb-8d8a-4851-8ea5-2c569bf26918)(content(Whitespace\"\\n\"))))(Secondary((id \
         10b3dc5e-6ae6-4d2b-a37c-154be805a7a4)(content(Comment\"#                                              \
         #\"))))(Secondary((id \
         90f01c08-5a01-48c2-a85e-8c3d80930f86)(content(Whitespace\"\\n\"))))(Secondary((id \
         9f80b958-be2f-45c5-b4cd-3dfa6f63a377)(content(Comment\"# If x < lo, \
         return lo                         #\"))))(Secondary((id \
         51bff7fc-b274-42e1-a0eb-7abfa8fa38fb)(content(Whitespace\"\\n\"))))(Secondary((id \
         33f2ad6a-dfaf-4f77-8b74-ddd8d7c36648)(content(Comment\"# If x > hi, \
         return hi                         #\"))))(Secondary((id \
         5ffafec2-6ef8-44df-9ec8-16fcb26f045f)(content(Whitespace\"\\n\"))))(Secondary((id \
         71d58064-26f7-4e54-8351-46c1618fe52e)(content(Comment\"# Otherwise, \
         return x                          #\"))))(Secondary((id \
         59836500-1996-49dd-8275-df86cce2f594)(content(Whitespace\"\\n\"))))(Secondary((id \
         dd250b23-b2b6-4710-8ebe-b7faa650ddfd)(content(Comment\"#                                              \
         #\"))))(Secondary((id \
         428ae385-8111-4b46-867c-4d53cc693452)(content(Whitespace\"\\n\"))))(Secondary((id \
         812daad5-ed1c-4515-98a7-ea7de4d0a6e7)(content(Comment\"# \
         Examples:                                    #\"))))(Secondary((id \
         b4113c66-5a38-44d0-a455-70f1b5a56626)(content(Whitespace\"\\n\"))))(Secondary((id \
         bc9369c7-4281-41ac-9010-831d848aed3b)(content(Comment\"#   clamp(5, \
         0, 10) == 5    (in range)         #\"))))(Secondary((id \
         3608e1e3-d469-4384-89ab-2cc5fdfe8946)(content(Whitespace\"\\n\"))))(Secondary((id \
         d1d01a48-8ee7-4aa9-92e9-6c622753c855)(content(Comment\"#   clamp(-3, \
         0, 10) == 0   (below min)        #\"))))(Secondary((id \
         24ee26e6-c7e0-4b67-a938-4df7c35dc35d)(content(Whitespace\"\\n\"))))(Secondary((id \
         49cf144d-d622-4ef3-a0ec-1f7a678c13ff)(content(Comment\"#   clamp(15, \
         0, 10) == 10  (above max)        #\"))))(Secondary((id \
         668dc14a-cc81-4a63-ad96-9501ee2d08d0)(content(Whitespace\"\\n\"))))(Secondary((id \
         fed71d25-88ef-4c89-be79-210a3fdd212f)(content(Comment\"#                                              \
         #\"))))(Secondary((id \
         d0c37d18-a406-42f6-a5dd-9c41189801fa)(content(Whitespace\"\\n\"))))(Secondary((id \
         efcedf1c-8035-4361-b8fb-4bf2307196dc)(content(Comment\"# Syntax \
         reminder:                             #\"))))(Secondary((id \
         a22ef6e2-ac19-4d21-8f0d-ecd4e0207930)(content(Whitespace\"\\n\"))))(Secondary((id \
         2bbe45bf-9101-464f-b232-00d20410c64b)(content(Comment\"#   if cond \
         then expr1 else expr2              #\"))))(Secondary((id \
         03138542-c1cf-435d-bbc4-ca9ffdbc82ad)(content(Whitespace\"\\n\"))))(Secondary((id \
         bb0e4c5c-37fb-4167-9862-e31692e56c51)(content(Comment\"#   \
         Comparisons: <, >, <=, >=, ==              #\"))))(Secondary((id \
         caad6ece-3120-47a5-b5e3-54d88cc6f90a)(content(Whitespace\"\\n\"))))(Secondary((id \
         80d8c8e3-8192-4435-958e-b5f664fc5317)(content(Comment\"#                                              \
         #\"))))(Secondary((id \
         440f9326-a287-4727-a751-70e100c82066)(content(Whitespace\"\\n\"))))(Secondary((id \
         5476e685-3740-4fc2-875e-3d7da6d6d262)(content(Whitespace\"\\n\"))))(Tile((id \
         24d75159-8eea-4717-b584-95c2d318eb1d)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         5985e865-43ea-4ddd-b6fb-66bf8a5a22c1)(content(Whitespace\" \
         \"))))(Tile((id \
         05879ec7-84c3-4f70-a54b-4a0e49fd3dde)(label(clamp))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         ad560b1a-9d6f-4c6e-90f0-955d26eca231)(content(Whitespace\" \
         \")))))((Secondary((id \
         d77d32d4-9168-4222-ba0c-fc5c9e813fd9)(content(Whitespace\" \
         \"))))(Tile((id 0981fd9a-2079-497c-9847-c068126a6738)(label(fun \
         ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 37))(sort Exp))))))(shards(0 \
         1))(children(((Secondary((id \
         45a095de-c7f8-4cf4-a9f9-f73498154fa8)(content(Whitespace\" \
         \"))))(Tile((id \
         942ef6f3-9501-4319-88c6-7acbf2288993)(label(\"(\"\")\"))(mold((out \
         Pat)(in_(Pat))(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0 1))(children(((Tile((id \
         d316565d-5abf-4346-b55d-3e362f6eee5c)(label(x))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         3d917d90-cf84-45d3-9eca-c43d80123a87)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 44))(sort Pat))((shape(Concave \
         44))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         84ae17f4-a433-4b95-8e0f-77f5813ca6c4)(content(Whitespace\" \
         \"))))(Tile((id \
         50983a67-b6bc-447f-8888-1eff1e87e411)(label(lo))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         41625e4a-2837-4fea-b2a1-418b546346dc)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 44))(sort Pat))((shape(Concave \
         44))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         5c939749-eae2-4fc2-8443-e6bb8e076bf9)(content(Whitespace\" \
         \"))))(Tile((id \
         42db448d-5ee0-4013-83d9-62b642574c1d)(label(hi))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children()))))))))(Secondary((id \
         86d16314-8496-462d-aba9-9da6013ae93c)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         0e96d582-4352-426f-8b13-caab0f15a17f)(content(Whitespace\"\\n\"))))(Tile((id \
         e77f99df-1740-4e33-b183-47bab6cf1d09)(label(?))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         4f799886-ec55-4fb0-9ff4-7a54c047215f)(content(Whitespace\" \
         \"))))(Secondary((id \
         856d413f-2a3a-4674-8a8c-ae4cb42c4b7f)(content(Whitespace\" \
         \"))))(Secondary((id \
         267b06d4-2e74-447b-9f9a-65dd6e687305)(content(Whitespace\"\\n\"))))(Secondary((id \
         f0ba5eb1-40f8-4f00-93ea-7cc11cfc59d3)(content(Whitespace\"\\n\"))))(Secondary((id \
         22373aa6-48f9-4758-8ee7-4dd1cfc9eef0)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         52bd027f-f1fc-4d8f-b6d6-68875d6c63ad)(content(Whitespace\"\\n\"))))(Secondary((id \
         8973cb3b-93f3-4af6-858c-67797f42bd76)(content(Whitespace\"\\n\"))))(Tile((id \
         3a28d0ed-1a7d-470c-87cf-962b4df21574)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         c0ba382c-034a-4024-ab11-2f14ab7050fe)(content(Whitespace\"\\n\"))))(Tile((id \
         d8561253-3ce2-447b-9ec1-048a52313023)(label(clamp))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         26ffda19-a939-49a0-a184-0cb83c8aed8a)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         90af7516-c484-47b5-917c-44a01bf94dc0)(label(5))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         6e5af40c-20bb-430e-8bab-5d4b9bef96a5)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         730147b5-9069-4d30-8349-8d69a59474a4)(content(Whitespace\" \
         \"))))(Tile((id \
         ffa9d0bc-b571-48c6-9ac3-db58d0c30491)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         7ad593cf-89f5-4df8-b5c9-8997dca8d351)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         8a994250-e29f-4b9f-ac02-10112e9fce8d)(content(Whitespace\" \
         \"))))(Tile((id \
         33a95718-e922-428b-9495-9e2f543eeb39)(label(10))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         e73865db-2662-49d5-8c3c-524431585e3b)(content(Whitespace\"\\n\"))))(Tile((id \
         cbe3ec7a-fc6d-4524-a7af-6f06a133e604)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         2a0d198b-061d-4c06-8502-50bd0cd8ac58)(content(Whitespace\" \
         \"))))(Tile((id \
         0889359f-ff6f-4a67-91c4-cef9c91dda72)(label(5))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         fb729bf5-be4e-4f6b-b917-fd2bded366fc)(content(Whitespace\"\\n\")))))))))(Tile((id \
         627e45c3-2fe6-4abd-8f49-aa3b57f2187e)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         1055abca-4cb6-40dc-abb4-f03addcad667)(content(Whitespace\"\\n\"))))(Secondary((id \
         6bf71e5a-9ccc-4359-bb30-de2579ca05fb)(content(Whitespace\"\\n\"))))(Tile((id \
         a04fa4be-b142-409d-b53c-2586d4aac2f4)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         3dd8cc74-df9c-4f32-8aa0-ea157bd4d685)(content(Whitespace\"\\n\"))))(Tile((id \
         e233bfaf-5d65-4944-96e9-23886ac0f5cf)(label(clamp))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         7a9b31e1-2fae-4a79-84e4-32b85b1d8334)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         6a7de619-12b8-4834-a2f7-e6d9a83be78c)(label(-))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape(Concave 25))(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         5be6fe87-8d1a-4899-9a90-b8aca66748cc)(label(3))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         05c2171c-ba45-44ba-bce3-3e9ae6942bdc)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         047e7f84-248c-4186-9c33-e9f1bd9eecd0)(content(Whitespace\" \
         \"))))(Tile((id \
         da0a6af5-cd6b-4c9f-9ed6-9ee98acd6535)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         7711c7e8-eaf4-42cb-a8f7-f13c5daff3ed)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         17a05dff-6f7c-4460-aba1-c29591ccc621)(content(Whitespace\" \
         \"))))(Tile((id \
         0cca9b2b-bd17-44a1-86d1-73291d731b72)(label(10))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         b9db8089-d669-4e0b-9b6d-478b542029f6)(content(Whitespace\"\\n\"))))(Tile((id \
         465e90c4-8e3f-43c1-b61e-bdae07c392b8)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         3e4b62f7-3576-4a5f-9c16-23fd6418f9d5)(content(Whitespace\" \
         \"))))(Tile((id \
         a2f92910-5c48-4633-8c43-f11bd7f22838)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         ffcee59c-a659-4b49-b038-006275b823ba)(content(Whitespace\"\\n\")))))))))(Tile((id \
         a61c214a-076d-42d9-87b5-dc23df6241f8)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         0e3e0ea7-c6aa-444c-aa13-f41cb9330040)(content(Whitespace\"\\n\"))))(Secondary((id \
         aa07af2b-b958-4145-a78d-308d9245ba15)(content(Whitespace\"\\n\"))))(Tile((id \
         79a8d287-becf-4412-b280-3239841d0340)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         50f9264e-5063-43f3-987a-dfa301fed984)(content(Whitespace\"\\n\"))))(Tile((id \
         cd7c6b34-6e1e-49f8-b051-d505b4fd26e6)(label(clamp))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         edc2c1ad-13cf-48dd-920b-0d952efb13a4)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         6695bd00-aab0-4cfd-a1bd-e97a214eca34)(label(15))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         25943a52-2912-4737-9190-a268ae4480dd)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         47e16287-3ea6-4204-a785-02af8ecc4ddd)(content(Whitespace\" \
         \"))))(Tile((id \
         4482b926-c74d-4379-8646-c49e61271a6b)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         2705bbde-7c5c-4643-874b-8eccfac831da)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         2250c657-fbcc-488a-b4fc-b8fb4e8b9f96)(content(Whitespace\" \
         \"))))(Tile((id \
         9f01ffe0-4e69-4d69-97c9-77b099984bfd)(label(10))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         2f292f2d-27c8-493e-bdc8-2a701db2f76b)(content(Whitespace\"\\n\"))))(Tile((id \
         da566790-7e85-47b5-a0fe-e186fd39286b)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         d492d5fa-5e98-4cbd-aac4-87353ddcc488)(content(Whitespace\" \
         \"))))(Tile((id \
         51670fd9-e155-45e6-aad4-b88d2479cd92)(label(10))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         7842255b-9a23-4e1a-842a-138da3ed60a9)(content(Whitespace\"\\n\")))))))))(Tile((id \
         9900f9ac-9dcf-478d-b067-493494037b42)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         91b66912-f50b-42bf-bf23-67be6dc930a2)(content(Whitespace\"\\n\"))))(Secondary((id \
         ea80e863-30d3-48ee-bf98-8ad2f383bfb1)(content(Whitespace\"\\n\"))))(Tile((id \
         a81192d7-3136-413d-8462-89d7a57259f5)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         f96a400b-1fcf-4ab0-91c4-192b6e5a6828)(content(Whitespace\"\\n\"))))(Tile((id \
         cef49c33-2e40-45b4-87d0-5248db47304a)(label(clamp))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         c73eedcc-6401-45fd-9b8c-02cb34b9b5da)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         5d16ecae-7524-4764-8839-6468d60a9401)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         2f3cdf51-b159-4eb9-a385-a454246adc37)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         5ae97384-85f7-4143-98df-482d35c54c67)(content(Whitespace\" \
         \"))))(Tile((id \
         9319a036-e40b-406e-bbda-f46e986759f8)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         f5798e69-8189-4dd6-9de3-f759bce1e815)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         b363699b-09d8-4791-a2a8-aad600924ff6)(content(Whitespace\" \
         \"))))(Tile((id \
         48b8d8a8-e3f1-4bcb-9431-656e3e6d48ad)(label(10))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         51fc8568-f30f-46f0-b6ea-3921e929f67f)(content(Whitespace\"\\n\"))))(Tile((id \
         28241953-d2e5-406f-aa62-aeb4bc878c4c)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         06940ec9-b721-42bd-9fc1-c95bceccda51)(content(Whitespace\" \
         \"))))(Tile((id \
         5ca671d3-a6fd-4ece-937b-2739106ef6ad)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         45f73a31-f76d-47f7-95e4-2036439d5870)(content(Whitespace\"\\n\")))))))))(Tile((id \
         c56670cd-f291-4070-bc10-1ed69c92b5ba)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         1db2a160-c4e5-44ed-b00f-6846fb6e0966)(content(Whitespace\"\\n\"))))(Secondary((id \
         3e0de77e-ee22-4a51-bdbe-3daca58d9fbb)(content(Whitespace\"\\n\"))))(Tile((id \
         065ed226-76f9-4c6e-b931-28082ee6ae75)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         fb1fd379-7ef6-413e-882e-9eb03bd3f552)(content(Whitespace\"\\n\"))))(Tile((id \
         c3fe3e6b-0504-4fe1-b8b1-cdf2d58b086e)(label(clamp))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         581d9fc3-0dcf-4198-9fda-ae6689d0f578)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         8efae036-09c6-4dd3-8076-bc6784ff337c)(label(10))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         5d345b7b-9c4e-4f22-95fb-6249c0d8c68d)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         cc223a46-22c0-43f9-b5d7-a6ef816a1c91)(content(Whitespace\" \
         \"))))(Tile((id \
         bfe46e60-cc83-4403-9fba-05d27038b273)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         e39096aa-c424-483e-8b18-ee39e5d1de19)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         7ab6cc87-743c-4183-ac6c-74e608750645)(content(Whitespace\" \
         \"))))(Tile((id \
         cf1dafa4-a809-4269-b9e3-c5e995daa938)(label(10))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         42c08b72-39d6-4c8b-9ea6-be39b8241bb5)(content(Whitespace\"\\n\"))))(Tile((id \
         6512c06e-7a72-4b24-8e58-0828ad3fc7ec)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         13815731-e6f7-47f2-8001-e1c72095d529)(content(Whitespace\" \
         \"))))(Tile((id \
         4fb24cbb-a452-4c79-9750-d2695df5afc1)(label(10))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         2bcc0082-a46d-4204-8a1e-b78907f5e29a)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         2691c84f-90fe-4602-866c-bacf486a5a7d)(content(Whitespace\"\\n\")))))";
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
         #                                              #\n\n\
         let clamp = fun (x, lo, hi) ->\n\
         ?  \n\n\n\
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
