let out : string * Haz3lcore.PersistentSegment.t =
  ( "Study / writing / clamp / clamp-sketch",
    {
      segment =
        "((Secondary((id \
         4ea9cc4e-3981-41e6-aea7-50d972e0f6e8)(content(Comment\"# CLAMP \
         TASK                                   #\"))))(Secondary((id \
         1bea3466-0a2b-4bcb-887e-724e5ab6a730)(content(Whitespace\"\\n\"))))(Secondary((id \
         ac635ca6-6e98-4ae9-931b-cfab2ad643fd)(content(Comment\"#                                              \
         #\"))))(Secondary((id \
         a9b9308a-fd71-4190-9f59-32dcf2d1d1b4)(content(Whitespace\"\\n\"))))(Secondary((id \
         14c04d70-2533-438d-8798-7331e48f5117)(content(Comment\"# Implement \
         clamp: constrain a number to be    #\"))))(Secondary((id \
         e2a45923-a4e3-43b0-84b2-5b0a9035321b)(content(Whitespace\"\\n\"))))(Secondary((id \
         5c82c5e4-224a-4235-8a59-6d04d9402f7c)(content(Comment\"# within a \
         given range [lo, hi].               #\"))))(Secondary((id \
         20cd1b7f-a846-4744-940a-3dacfae905f9)(content(Whitespace\"\\n\"))))(Secondary((id \
         5e13829d-66e3-4efc-87db-f5430c1c8c04)(content(Comment\"#                                              \
         #\"))))(Secondary((id \
         8f856b9a-22bb-402e-a144-a8f58f57f701)(content(Whitespace\"\\n\"))))(Secondary((id \
         d9030423-c703-4217-b62c-a6f0aa498e57)(content(Comment\"# If x < lo, \
         return lo                         #\"))))(Secondary((id \
         fac010c1-57a7-428d-b1f1-474eccda59ab)(content(Whitespace\"\\n\"))))(Secondary((id \
         310867d5-5e60-4531-bfdb-31fc21e0ef45)(content(Comment\"# If x > hi, \
         return hi                         #\"))))(Secondary((id \
         16596d25-a82e-4a8d-b24b-420e9573a9cb)(content(Whitespace\"\\n\"))))(Secondary((id \
         00c5556d-b505-46c2-bd66-dabbf0a7edb7)(content(Comment\"# Otherwise, \
         return x                          #\"))))(Secondary((id \
         b9c768bd-6814-4e58-a9af-86c23c7baf2e)(content(Whitespace\"\\n\"))))(Secondary((id \
         04dca744-41e7-46e8-abc6-d48237909cbc)(content(Comment\"#                                              \
         #\"))))(Secondary((id \
         5f69ba75-4496-42de-a2c8-bce6bfa3b123)(content(Whitespace\"\\n\"))))(Secondary((id \
         65f8ee83-830f-446a-abe4-65b7a71de09a)(content(Comment\"# \
         Examples:                                    #\"))))(Secondary((id \
         b9952e93-364f-46ec-833c-8757fec18462)(content(Whitespace\"\\n\"))))(Secondary((id \
         cfb90c34-1a48-4d6e-8665-61e9de2ebdaf)(content(Comment\"#   clamp(5, \
         0, 10) == 5    (in range)         #\"))))(Secondary((id \
         ac5d6cae-328e-4c5e-bdc0-2f4133344921)(content(Whitespace\"\\n\"))))(Secondary((id \
         85cbdd33-92d4-457f-8213-ef4615e8941b)(content(Comment\"#   clamp(-3, \
         0, 10) == 0   (below min)        #\"))))(Secondary((id \
         437c0545-c7c5-405b-bfe9-2f5b7a7a68b8)(content(Whitespace\"\\n\"))))(Secondary((id \
         1787c2b6-1dfa-4563-afdb-43f762d8a435)(content(Comment\"#   clamp(15, \
         0, 10) == 10  (above max)        #\"))))(Secondary((id \
         3234a547-03a7-40df-bfdd-b0edb603a6de)(content(Whitespace\"\\n\"))))(Secondary((id \
         868a472d-569c-4cdd-ab83-3abe6e324dcf)(content(Comment\"#                                              \
         #\"))))(Secondary((id \
         d6f23712-0a38-4dcf-aa4c-5d2bf8d1984b)(content(Whitespace\"\\n\"))))(Secondary((id \
         7127a216-9269-414f-b3eb-fbdd82bbaa04)(content(Comment\"# Syntax \
         reminder:                             #\"))))(Secondary((id \
         125a35d1-7985-46ee-8774-f2abe6d097a3)(content(Whitespace\"\\n\"))))(Secondary((id \
         93efdf47-d9a9-412b-9f35-56fc7b26165e)(content(Comment\"#   if cond \
         then expr1 else expr2              #\"))))(Secondary((id \
         ed7a89a2-a8e3-4126-af0a-3e550dc49865)(content(Whitespace\"\\n\"))))(Secondary((id \
         2af48d7f-9f69-4c31-a29f-f6b322141692)(content(Comment\"#   \
         Comparisons: <, >, <=, >=, ==              #\"))))(Secondary((id \
         3a8e62ba-9d5c-476a-9f5b-fc537023828a)(content(Whitespace\"\\n\"))))(Secondary((id \
         5253c83d-f3d9-409c-95f0-98710079aa73)(content(Comment\"#                                              \
         #\"))))(Secondary((id \
         61f3ee92-4fe9-4336-aa8a-ab767e7d2739)(content(Whitespace\"\\n\"))))(Secondary((id \
         fdac2226-0b80-4fe8-b283-4a035e9407c9)(content(Whitespace\"\\n\"))))(Tile((id \
         4388a4e0-b95d-4da5-88e7-42f1ecc013cd)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         7bf00102-7e97-491b-89d9-8776d186c7bd)(content(Whitespace\" \
         \"))))(Tile((id \
         d8df1440-c438-460f-b447-d8922ebb8c3e)(label(clamp))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         395e4e27-0ce8-4af8-b37d-49b526697bbe)(content(Whitespace\" \
         \")))))((Secondary((id \
         d01b6602-0ff7-484c-b3a4-9c581faa035a)(content(Whitespace\" \
         \"))))(Tile((id e02b28a5-8703-4a4c-b49e-da6af71313ba)(label(fun \
         ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 37))(sort Exp))))))(shards(0 \
         1))(children(((Secondary((id \
         eab3cded-9467-446d-80a8-5ef16ff984e3)(content(Whitespace\" \
         \"))))(Tile((id \
         753383ec-6dd8-40e5-8865-f8e72c5e9704)(label(\"(\"\")\"))(mold((out \
         Pat)(in_(Pat))(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0 1))(children(((Tile((id \
         f607e155-5590-4d76-b641-0b2ccd1dcceb)(label(x))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         85423687-2c1f-4f03-9321-441ca89bb42c)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 44))(sort Pat))((shape(Concave \
         44))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         101b2ee8-40fe-484c-988e-52662da47e9b)(content(Whitespace\" \
         \"))))(Tile((id \
         3f6a3fbf-179a-46eb-b77c-8903950d1bec)(label(lo))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         d92a60d8-4c97-48c7-b7cd-db040c336402)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 44))(sort Pat))((shape(Concave \
         44))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         a85ea817-0805-4b13-bd6e-328a07bc9ba6)(content(Whitespace\" \
         \"))))(Tile((id \
         c5b10af9-9b01-4727-bf96-0636b760f3e6)(label(hi))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children()))))))))(Secondary((id \
         2e4b1976-0d52-4164-afdf-e7baace9d115)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         c45d526a-94fe-4d8c-97df-e64574c58d4c)(content(Whitespace\"\\n\"))))(Tile((id \
         4702d290-df15-4a6b-826e-9ae2680adc95)(label(?))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         28a6e5eb-95ba-4bec-bb60-75832dffd4a0)(content(Whitespace\" \
         \"))))(Secondary((id \
         3733848b-1031-4bcc-9377-6f9a54e18833)(content(Whitespace\" \
         \"))))(Secondary((id \
         b9f2f3e5-0f8f-4f9b-8a1d-b51b02a2bdc1)(content(Whitespace\"\\n\"))))(Secondary((id \
         39c8f09f-0511-48f2-b763-bfec8dbc1ec9)(content(Whitespace\"\\n\"))))(Secondary((id \
         729580e5-c981-4c8b-b22f-fad62c7fbde1)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         1214b52e-250b-49cd-884e-175f3049bb45)(content(Whitespace\"\\n\"))))(Secondary((id \
         bd281325-1c49-4ccf-9b47-f9a163347ea1)(content(Whitespace\"\\n\"))))(Tile((id \
         385e72f9-0f0b-4ef8-a727-aa78c00a3c1c)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         d40aa983-ee01-4dd3-adbe-0bd612894b33)(content(Whitespace\"\\n\"))))(Tile((id \
         762f3241-45f5-4397-a8af-3fc62fcfd469)(label(clamp))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         3e2fe14d-679e-4d82-82f0-715233e43491)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         9cd53e09-2ac4-4a12-a4e3-5520193ebb10)(label(5))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         c12ee0e8-378a-40f5-882a-97447fd88fa3)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         1b514481-6262-437f-ac80-f1769e845999)(content(Whitespace\" \
         \"))))(Tile((id \
         5cbdb98b-34a6-4bb3-8c2e-6c1b79359dd8)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         3f180ed1-1ed8-409b-8f8d-bc407d2f0323)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         c5134b9a-3120-4b0f-9087-b3ebb7f91164)(content(Whitespace\" \
         \"))))(Tile((id \
         71d88548-666b-4935-b29d-faf219d0d2e3)(label(10))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         68f7fab3-c939-4319-9e46-386be7ea850a)(content(Whitespace\"\\n\"))))(Tile((id \
         56ecc21c-cdb8-45db-a471-c90a55a50a28)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         76d7a78e-c08e-4f87-8139-4d8ab95ad87d)(content(Whitespace\" \
         \"))))(Tile((id \
         25c5a2c2-f6cf-4a26-bffc-2b8d0c4e0643)(label(5))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         80a53804-cdd1-48fc-b279-0aa3f298e67a)(content(Whitespace\"\\n\")))))))))(Tile((id \
         c2bbd6df-d369-4913-9c20-c3bdb49d27b2)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         fb0f0023-6b31-4fe4-ac30-3a58225c445d)(content(Whitespace\"\\n\"))))(Secondary((id \
         27aac6b6-8950-4bd3-92ae-e8c3b31773ba)(content(Whitespace\"\\n\"))))(Tile((id \
         e2c7ae8c-7e37-4416-bec6-db04567a008d)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         f183aa6b-7f3d-4e0d-a95c-f5b9f52782bb)(content(Whitespace\"\\n\"))))(Tile((id \
         1f631670-de25-4b4b-961a-48eae770dd46)(label(clamp))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         28440dae-cb27-4bcb-858f-d3e9986026f7)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         26c090be-ef92-485a-9356-bb986ee46217)(label(-))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape(Concave 25))(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         87e7d248-0127-4319-8f3d-71d32cc5dc33)(label(3))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         a13e8b8b-0f8e-4adc-a697-ebf98385421d)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         cbb95d6a-762a-441b-98e1-a3cabc9acee4)(content(Whitespace\" \
         \"))))(Tile((id \
         a9b1469b-1047-4248-b891-b786730fbd22)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         31737a38-a5a5-4f77-8a1c-3471493f671b)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         a0055f3c-341f-460f-aa51-3638aa9d7b6c)(content(Whitespace\" \
         \"))))(Tile((id \
         644b0f4d-a7cf-4ba4-8124-a0fdfc4ea166)(label(10))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         0014e889-1273-4c71-a3a9-28356dc70944)(content(Whitespace\"\\n\"))))(Tile((id \
         002bc657-6b5e-4fc6-8e1f-99c20b2d6231)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         5dcc0dfb-fe7f-4d3d-ba68-67563c2eb060)(content(Whitespace\" \
         \"))))(Tile((id \
         d8e477e4-da4a-42d3-aa54-5565cecead9f)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         4e3f9f8b-1814-43b3-8216-af53fa5a3a5c)(content(Whitespace\"\\n\")))))))))(Tile((id \
         b0f98374-4c5b-4ffa-808b-00dd8e7ce0c8)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         dbf76a66-f980-46fe-aaa9-c9372f3a9705)(content(Whitespace\"\\n\"))))(Secondary((id \
         823cef03-38a4-4139-b0db-eed77f7df374)(content(Whitespace\"\\n\"))))(Tile((id \
         daf6c9e2-09a5-4192-9c7f-3fd3ca4491b4)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         5f263650-3a0a-4c3a-9957-272f316391be)(content(Whitespace\"\\n\"))))(Tile((id \
         3b39ea32-2a4f-4ade-9d78-232bd4ac44e8)(label(clamp))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         5f74b307-6c84-426b-b4e3-b58d82777fce)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         5e756e75-95cd-42f9-b949-ad0ec614e5cd)(label(15))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         d4642366-06b4-486f-ae57-b68c7c6c810b)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         3a813c22-6489-4902-89d7-f2c0036d28f0)(content(Whitespace\" \
         \"))))(Tile((id \
         2c99bcb3-d6e7-46b5-9ee7-746daefd6c51)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         e1d9283b-da6c-442b-9035-bbeb33b5d6ef)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         8f0ef299-b6c7-4953-a19c-015088a092bb)(content(Whitespace\" \
         \"))))(Tile((id \
         88414f1d-773e-4d0a-8cdf-998713d937bc)(label(10))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         991561be-185f-4ad3-abdb-f05326ac07a7)(content(Whitespace\"\\n\"))))(Tile((id \
         d44d4791-55d2-470a-af34-9806bef9066c)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         b8d417a3-67d7-485f-8af7-e6084fdce84a)(content(Whitespace\" \
         \"))))(Tile((id \
         b8908481-9a9f-4e6f-8309-72837982d480)(label(10))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         62527fd9-6f8e-41c9-8128-17ac928fd24c)(content(Whitespace\"\\n\")))))))))(Tile((id \
         064d07bb-25d4-4620-be5e-24d9a87593fc)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         f51e194d-2661-4f9a-aaff-e7c133f7f041)(content(Whitespace\"\\n\"))))(Secondary((id \
         b7c35185-5715-497f-b25b-64989743b408)(content(Whitespace\"\\n\"))))(Tile((id \
         3bc2935f-c8b4-4f11-929e-200bd0a19507)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         ba2da5df-ed14-4723-b2c9-91b0f99c182e)(content(Whitespace\"\\n\"))))(Tile((id \
         0dcc0f49-f2da-45c6-9d03-1d7c8b29ace3)(label(clamp))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         a2902031-a8a8-4b52-9668-afa9dac5d4a3)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         eb4df618-342b-4fca-ada2-a9638c0eb840)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         f95a947e-73b2-4c7b-8472-e9c711af43b9)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         2434a3c0-fcf4-4d4c-a3e3-41fc868ee9a4)(content(Whitespace\" \
         \"))))(Tile((id \
         5c51a008-173f-458a-8265-97715ff5fdc0)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         f22900f6-bff2-4e8f-afc1-bf5cbf2655e6)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         e1d4ecd8-9244-4542-8b0d-16385f7f3d0f)(content(Whitespace\" \
         \"))))(Tile((id \
         a1b172d2-fbfb-46cf-87b6-47877c9efed0)(label(10))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         c97da470-7425-4819-b76f-fc7bf84e89d1)(content(Whitespace\"\\n\"))))(Tile((id \
         104ec32c-faa9-47fe-80e1-1ae63572ac41)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         8d020ce9-8ed4-4c52-be48-9c96b103e90b)(content(Whitespace\" \
         \"))))(Tile((id \
         27229efc-d327-4b5f-80c7-9c8a7a8b6d5b)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         6b595bf6-cf6f-405a-9347-d77ad6045ff8)(content(Whitespace\"\\n\")))))))))(Tile((id \
         696a39ee-8cb3-4080-a0e1-f7967e0e264f)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         b5214e48-9ce8-4af7-b5e9-aa9657a757ae)(content(Whitespace\"\\n\"))))(Secondary((id \
         198d0cca-e0c9-47a1-acc4-a872be57f564)(content(Whitespace\"\\n\"))))(Tile((id \
         3485870c-7249-43e5-828a-cb9a9f4dbc88)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         9caf19ff-9fad-42fd-88f6-dce95416d744)(content(Whitespace\"\\n\"))))(Tile((id \
         03df6b1a-3dc6-4871-9604-995750d516e1)(label(clamp))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         3f5a8b59-e42c-4242-a2e6-db028a6bdc3c)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         7db3372b-76be-4bf0-9da0-14b8e7ad4fc0)(label(10))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         8a6e377a-027b-4e74-ad38-660fc40bdcc1)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         ee9e61aa-0f81-45f5-a526-d2c2a4e88db0)(content(Whitespace\" \
         \"))))(Tile((id \
         574cdfe2-f647-4b80-aac6-d9fc8b51f713)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         fd7ebbc5-6c0a-48b7-a781-e9c92961aa87)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         d146bfb4-b4ff-4167-adbd-369c7f2b3220)(content(Whitespace\" \
         \"))))(Tile((id \
         5121f767-720d-4341-a64d-a941fbefeba0)(label(10))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         37d75663-bf78-4170-be21-1810352936e5)(content(Whitespace\"\\n\"))))(Tile((id \
         6d5a06eb-c6d0-44cb-bc36-34e38d0ac7cb)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         d154012f-e54f-4aba-aa26-00bbd39dcb4c)(content(Whitespace\" \
         \"))))(Tile((id \
         9ee31b5e-6f01-49dc-980a-a06d11c2bd96)(label(10))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         8f95378a-9025-4f57-b6dc-9824f3b47456)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         79ef83d4-9e27-4ad0-9aa8-dbada9ad4590)(content(Whitespace\"\\n\")))))";
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
