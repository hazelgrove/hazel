let out : string * Haz3lcore.PersistentSegment.t =
  ( "Study / writing / clamp / clamp-sketch",
    {
      segment =
        "((Secondary((id \
         a0d18f4f-c64a-4304-b2e1-e5724c982532)(content(Comment\"# CLAMP \
         TASK                                   #\"))))(Secondary((id \
         9504f6df-7cc5-4a46-8dc6-dee39b53ecd5)(content(Whitespace\"\\n\"))))(Secondary((id \
         853784e6-33d9-4255-bf00-09e2cf6ba5ac)(content(Comment\"#                                              \
         #\"))))(Secondary((id \
         8c4ede63-e292-4b81-839c-b0f9986992a5)(content(Whitespace\"\\n\"))))(Secondary((id \
         1a299094-3a04-490b-b1f6-b50a85fb0980)(content(Comment\"# Implement \
         clamp: constrain a number to be    #\"))))(Secondary((id \
         0e80a387-0f65-4c40-9e5e-4fa79478c9ab)(content(Whitespace\"\\n\"))))(Secondary((id \
         c4b06d3e-b2d6-4e69-99d3-6c4c6996a522)(content(Comment\"# within a \
         given range [lo, hi].               #\"))))(Secondary((id \
         c2035c7a-85de-4265-a7d4-84e7df46e784)(content(Whitespace\"\\n\"))))(Secondary((id \
         a2dc0925-c306-48ee-a2ef-de854897e03f)(content(Comment\"#                                              \
         #\"))))(Secondary((id \
         8f4e8090-74d8-46dc-8aee-ef9dda99d0cb)(content(Whitespace\"\\n\"))))(Secondary((id \
         695c790a-e30f-4849-a5c1-43992084524e)(content(Comment\"# If x < lo, \
         return lo                         #\"))))(Secondary((id \
         b329ffcb-1a7a-410b-9c1a-6cd832112649)(content(Whitespace\"\\n\"))))(Secondary((id \
         e45010fc-2bc4-423c-95bc-6440174d6bcd)(content(Comment\"# If x > hi, \
         return hi                         #\"))))(Secondary((id \
         79dd0600-6a99-4507-94fe-3a08c308c6af)(content(Whitespace\"\\n\"))))(Secondary((id \
         4e075a2b-a87b-497f-84b3-f2256143ef43)(content(Comment\"# Otherwise, \
         return x                          #\"))))(Secondary((id \
         a34ab34b-4cfe-442c-bf10-7e95bbf6fe0f)(content(Whitespace\"\\n\"))))(Secondary((id \
         356460a9-4670-4af4-aa46-78b93bd788a4)(content(Comment\"#                                              \
         #\"))))(Secondary((id \
         ca234b09-d5cd-4bd5-a783-16e57be6ce37)(content(Whitespace\"\\n\"))))(Secondary((id \
         4202a193-0a19-4752-a1ce-3641faa660f9)(content(Comment\"# \
         Examples:                                    #\"))))(Secondary((id \
         e67781de-a262-4306-87a9-2d0ed1a742ff)(content(Whitespace\"\\n\"))))(Secondary((id \
         7a71754c-4784-4495-b720-5d8949b6de42)(content(Comment\"#   clamp(5, \
         0, 10) == 5    (in range)         #\"))))(Secondary((id \
         ed65fdde-e6ac-4579-85ef-66060d754545)(content(Whitespace\"\\n\"))))(Secondary((id \
         09410d23-2aed-4c9d-bccc-e06ae5391c0a)(content(Comment\"#   clamp(-3, \
         0, 10) == 0   (below min)        #\"))))(Secondary((id \
         368f8119-448b-45f3-a6d3-6295b399da19)(content(Whitespace\"\\n\"))))(Secondary((id \
         d4dfa2db-150f-4b9d-aa89-337b0c5d451d)(content(Comment\"#   clamp(15, \
         0, 10) == 10  (above max)        #\"))))(Secondary((id \
         cc0d4a9d-35aa-4b25-a2f6-a68a720b64cc)(content(Whitespace\"\\n\"))))(Secondary((id \
         c6be7e1e-c230-47ac-9ee7-c66a798d42ba)(content(Comment\"#                                              \
         #\"))))(Secondary((id \
         bedac59f-0f8c-4189-b074-e90c209c328a)(content(Whitespace\"\\n\"))))(Secondary((id \
         09e620a5-d7cd-4aad-966d-c8a1df1b6629)(content(Comment\"# Syntax \
         reminder:                             #\"))))(Secondary((id \
         d507c44c-7593-461d-86d6-64a9212be02b)(content(Whitespace\"\\n\"))))(Secondary((id \
         0424db7b-9cb6-42b6-9952-bd41f6b65614)(content(Comment\"#   if cond \
         then expr1 else expr2              #\"))))(Secondary((id \
         aa3e16f9-7cc4-456a-ab81-ad52ab548090)(content(Whitespace\"\\n\"))))(Secondary((id \
         263b970b-dbb6-4573-b546-d2e2673976c1)(content(Comment\"#   \
         Comparisons: <, >, <=, >=, ==              #\"))))(Secondary((id \
         f1e49a55-cced-4913-a736-572859573705)(content(Whitespace\"\\n\"))))(Secondary((id \
         3a4d336c-9521-4fd2-9271-fcd153e68f38)(content(Comment\"#                                              \
         #\"))))(Secondary((id \
         6c92c10c-8aac-4310-8dda-4dde5086e50e)(content(Whitespace\"\\n\"))))(Secondary((id \
         657593ae-24c4-4075-a0bc-91206bb65944)(content(Comment\"# Tip: Turn on \
         auto-probe (microscope toggle)  #\"))))(Secondary((id \
         bfc94a4a-371a-424e-bf9a-4d33934b351c)(content(Whitespace\"\\n\"))))(Secondary((id \
         dc86bdaa-4cc8-401d-a5d4-2af6a7439b7e)(content(Comment\"# to see which \
         branch is taken for each test.  #\"))))(Secondary((id \
         ff156b74-44c9-4a24-841c-195d9041d124)(content(Whitespace\"\\n\"))))(Secondary((id \
         3427a8c7-862c-4efa-a436-2e2de789d16f)(content(Whitespace\"\\n\"))))(Tile((id \
         076b77d5-8f71-41c7-a8f3-eaf22246071a)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         2461574d-c06b-4234-b747-785a686d9515)(content(Whitespace\" \
         \"))))(Tile((id \
         7dfe1d90-bf83-4f20-ba89-597b780bd238)(label(clamp))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         7bf0f28d-bada-45d8-b028-d5d9132e59bc)(content(Whitespace\" \
         \")))))((Secondary((id \
         ce4985eb-94ef-4f2a-bf35-167a3428a359)(content(Whitespace\" \
         \"))))(Tile((id a3aa716f-8583-435d-9fb9-246f0ae1edde)(label(fun \
         ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 37))(sort Exp))))))(shards(0 \
         1))(children(((Secondary((id \
         f592ce47-5b5e-4f37-a80b-7978fc317418)(content(Whitespace\" \
         \"))))(Tile((id \
         32a5e606-e6fc-4fe0-b3f7-af0e1d23cb37)(label(\"(\"\")\"))(mold((out \
         Pat)(in_(Pat))(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0 1))(children(((Tile((id \
         eaadb503-d01d-401e-8020-601a552c923c)(label(x))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         efa007bf-ec0e-46ca-b942-5a9b823c2be3)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 44))(sort Pat))((shape(Concave \
         44))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         c0ef938a-7471-4216-8f3f-cf14da4b3108)(content(Whitespace\" \
         \"))))(Tile((id \
         f29d455d-d289-4351-be76-7a07b6a056e1)(label(lo))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         64ca61bc-e324-462a-a653-ce3a9e9b84fe)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 44))(sort Pat))((shape(Concave \
         44))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         bce235b1-2565-46d5-8d63-12616ffc24d2)(content(Whitespace\" \
         \"))))(Tile((id \
         c788a2de-b2aa-4932-a0da-d8b96bdc3df0)(label(hi))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children()))))))))(Secondary((id \
         3dbd87e3-8087-41a6-b09e-e191f144a3bc)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         8f9a6aa6-be76-43d5-b650-296111be4f79)(content(Whitespace\"\\n\"))))(Tile((id \
         88047181-58bf-49d8-add3-4212bac87180)(label(?))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         1220fe8c-82b5-4986-acbe-3ff0ee8463ad)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         546303e5-91ad-436f-b3b6-e7cd50c78203)(content(Whitespace\"\\n\"))))(Secondary((id \
         6a229407-97fa-4eef-a7da-5e950e2ff59a)(content(Whitespace\"\\n\"))))(Tile((id \
         c9ad7a67-1df8-4f57-bfd2-40bf7ad4cbdf)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         595d4e6d-f92f-4621-9d24-d269293152c2)(content(Whitespace\"\\n\"))))(Tile((id \
         70452adf-6fcc-4577-8e88-f22c864344ae)(label(clamp))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         dc7dbb44-e162-46eb-9622-2c2acc902c51)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         4f7ff55f-63c8-42fd-9e4b-234abc60d6bb)(label(5))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         3d8337b2-f897-4150-9717-a30ab6905a30)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         73d4dac7-9354-401d-b6d5-3d600b973685)(content(Whitespace\" \
         \"))))(Tile((id \
         7bb4d628-8c2e-4a8e-9551-8bff91fc8287)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         4e1e6e78-1087-4884-9cc1-b4a0bf5964af)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         d88ad141-9f52-440a-89b1-b410b8509164)(content(Whitespace\" \
         \"))))(Tile((id \
         3f03a704-c6a7-4a3f-8294-777d81d59616)(label(10))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         574539ec-e39b-4be7-9aad-087485f03ccc)(content(Whitespace\"\\n\"))))(Tile((id \
         05080750-86c0-4da7-b8bc-97ac6c6019bb)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         5af823b2-ff20-44b9-ab17-830d7c27be27)(content(Whitespace\" \
         \"))))(Tile((id \
         0bad0874-d4b8-4b04-866f-f0267447ea7d)(label(5))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         69967cb0-7298-4dd8-afd8-cacebff59206)(content(Whitespace\"\\n\")))))))))(Tile((id \
         2f205636-ccce-4a68-a629-28562ad41a51)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         c25c5608-0db4-40c5-9bfa-b609c9565f1d)(content(Whitespace\"\\n\"))))(Secondary((id \
         058c05a7-9a2d-4fa2-8a34-624afc5f3431)(content(Whitespace\"\\n\"))))(Tile((id \
         2fc114ad-41bd-4203-8f3d-07156992ce62)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         8e6063ae-81b1-4834-867c-abfb0dea511f)(content(Whitespace\"\\n\"))))(Tile((id \
         3e71ab6e-ea9c-44be-929b-a7345ea67618)(label(clamp))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         5e23aa60-a5c4-4c6e-9f90-8f372f70e635)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         418e7c7a-ba0c-45d8-af40-8f4dea4d4010)(label(-))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape(Concave 25))(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         e139f644-f6c4-43a1-8c3d-bc3956dbd3a1)(label(3))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         b6fdb39d-5a88-4f74-b977-6987483efd13)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         10ceca33-d6f6-44ee-a367-3fafad77898b)(content(Whitespace\" \
         \"))))(Tile((id \
         91286e94-8949-414e-88ec-52817d6400e0)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         fa07a46c-2a39-4791-bc79-a4a38d0f5b58)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         423f40a2-4db2-4e0b-9e75-3ddcbbb5d1d8)(content(Whitespace\" \
         \"))))(Tile((id \
         d73946de-82c1-4123-a62a-522fa190751f)(label(10))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         01d13d76-da16-4309-b130-7a019a5e2e3f)(content(Whitespace\"\\n\"))))(Tile((id \
         ed10b62c-c140-4512-b375-87ce8a56669c)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         2bdf8d7e-c75d-4168-af72-4e61cf0589f0)(content(Whitespace\" \
         \"))))(Tile((id \
         e4aca87f-13f6-430b-a64d-9a0d4dc20177)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         7018f46a-509e-4273-b574-dc3c738b882a)(content(Whitespace\"\\n\")))))))))(Tile((id \
         344c5d03-faf4-4825-ab16-912f47ebd788)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         76d3c2aa-97be-4122-89bc-bc50f0ec631c)(content(Whitespace\"\\n\"))))(Secondary((id \
         468b155c-de9e-4ee9-bf7e-082b1c917071)(content(Whitespace\"\\n\"))))(Tile((id \
         eebef564-7ca1-42bf-903f-bc90b031b543)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         ca42e385-28ea-4410-9162-4469feafbaaa)(content(Whitespace\"\\n\"))))(Tile((id \
         ccc90842-6817-488c-aa19-8cc244896b60)(label(clamp))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         40f8ff1e-7af6-4ba0-aeed-79e81d1b2641)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         e4ac7480-a9f3-4eaa-834e-a5a6a3f9e793)(label(15))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         c61d3c5d-f51a-4215-b6e5-b1d77a4dad38)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         de5dfb42-a112-4fc0-aff4-60a0f2becf9f)(content(Whitespace\" \
         \"))))(Tile((id \
         36d40439-db31-4d0e-a349-986ef3122c08)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         b3de00ef-6947-4b4b-8664-d9f5187517e0)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         88e1c02e-e080-4327-9756-fbc54231385f)(content(Whitespace\" \
         \"))))(Tile((id \
         37ce26c8-df6c-42d8-9845-ecfa127578d9)(label(10))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         e5c22869-fcf9-495a-989a-9040da8d83c7)(content(Whitespace\"\\n\"))))(Tile((id \
         0fa41701-d463-4b7a-8fc0-e88067897b06)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         8404b81a-ac5b-4851-9685-0ffad8917202)(content(Whitespace\" \
         \"))))(Tile((id \
         ada1139b-4dd2-49cd-a01d-0194fefd7894)(label(10))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         3e47f69a-1a97-406f-84d6-71731a96e587)(content(Whitespace\"\\n\")))))))))(Tile((id \
         ee28b03e-78c3-491d-8514-23b4d5b879aa)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         7c5e6d0e-2860-489e-8c60-716c02196f8c)(content(Whitespace\"\\n\"))))(Secondary((id \
         350b8d8e-804c-4d86-bcc7-e0489235181f)(content(Whitespace\"\\n\"))))(Tile((id \
         d3b53d92-b8a8-47b3-9ed5-ac39990d34b9)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         15dc5e28-dac6-4857-8be6-a31763f235ad)(content(Whitespace\"\\n\"))))(Tile((id \
         29212631-4e78-4a5c-b3b5-6a6eb2775e6b)(label(clamp))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         0e21c6bc-2c16-4e53-953c-6cccbb8ac870)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         b442d826-6687-438a-8ea0-962d3e6187b6)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         8316d1f4-85bc-4343-9122-43e3af08be87)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         d8979d1e-63eb-4b2a-91d0-7054c5700bb7)(content(Whitespace\" \
         \"))))(Tile((id \
         7d2036ad-1580-4e6a-8b2c-efb429fad043)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         6de77484-52e5-4f6a-bab4-4551f5f32b20)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         04c3a2dd-b80a-471e-87b6-1566ae3a5acd)(content(Whitespace\" \
         \"))))(Tile((id \
         e120d75f-42e9-4672-b8d2-1527567784c2)(label(10))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         6e0f5fe4-393f-4358-8df6-a9f4023ccdab)(content(Whitespace\"\\n\"))))(Tile((id \
         dde9d997-2cdf-48f7-87f2-2d5647a4e2af)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         17ade520-c7a8-4f37-9f75-f9d221b2d0b0)(content(Whitespace\" \
         \"))))(Tile((id \
         ebf46c70-a7b8-4263-b286-3639b1107072)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         d5172d44-4a62-40b4-9dce-9c7cda840827)(content(Whitespace\"\\n\")))))))))(Tile((id \
         0bae4f56-ad34-4c86-a344-546324c4b7ce)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         2e725e42-6b89-406d-96b4-fb4c4187db8e)(content(Whitespace\"\\n\"))))(Secondary((id \
         11d04513-f261-49df-a210-1f888be287ef)(content(Whitespace\"\\n\"))))(Tile((id \
         4797ea69-8411-48ff-a526-5aaf098ca132)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         bd36ea94-5e05-46f2-9c01-d00fbb0dd6ec)(content(Whitespace\"\\n\"))))(Tile((id \
         627a089a-5c47-4017-99ba-15663725be78)(label(clamp))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         e91966f3-65c9-466d-97aa-66b6c3708950)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         219e9904-cc12-4ac3-bcf1-755e0ef7bab1)(label(10))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         4fc86de8-bb5e-4b40-bc72-2c3a6d74e5b9)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         a82d101a-f93d-482f-9688-2b16650a3595)(content(Whitespace\" \
         \"))))(Tile((id \
         abae3032-34c0-492a-8a04-6dc3522c3afe)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         c2fc530c-49c6-48af-b425-304c2152e136)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         e5e80b7e-0e22-4853-93a4-ffa33741e221)(content(Whitespace\" \
         \"))))(Tile((id \
         7edd26ae-267c-47f5-b726-dad7bf0633fa)(label(10))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         f78a9ec0-e993-43f6-b375-99c068cfb00f)(content(Whitespace\"\\n\"))))(Tile((id \
         a45c9772-05fb-4c22-a182-46a1f509765d)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         f547cbe1-078a-40c4-b273-12d4fc30f101)(content(Whitespace\" \
         \"))))(Tile((id \
         70dea645-6083-4651-ae87-02ac3fb4998f)(label(10))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         aec591eb-d435-4947-940e-29056ba36df1)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         2f48bb49-b6b3-4e4c-b67d-d213fc023bd0)(content(Whitespace\"\\n\")))))";
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
