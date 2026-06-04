let out : string * Haz3lcore.PersistentSegment.t =
  ( "Study / tutorial / 08-clamp",
    {
      segment =
        "((Secondary((id \
         0dd41622-bb9c-41e5-a6df-463cab698845)(content(Comment\"# PROBES \
         TUTORIAL - PART 8: WRITING EXERCISE (CLAMP)               \
         #\"))))(Secondary((id \
         36bfb3cf-5def-4a18-94d6-738e6a09600d)(content(Whitespace\"\\n\"))))(Secondary((id \
         537810fb-803b-480f-b069-5ddbbf060d60)(content(Whitespace\"\\n\"))))(Secondary((id \
         f49dc1bd-390b-42b6-bc0f-760f2e5f6454)(content(Comment\"# Now it's \
         your turn to write some Hazel code.                     \
         #\"))))(Secondary((id \
         90a3de6c-437a-4339-933d-2b8aff155e80)(content(Whitespace\"\\n\"))))(Secondary((id \
         09d61720-af61-42e1-b7ec-a11b657c1c78)(content(Comment\"# Implement \
         `clamp`: constrain a number to be within [lo, hi].     \
         #\"))))(Secondary((id \
         d1b19c3a-37d6-41b0-bb6c-4b1061b58618)(content(Whitespace\"\\n\"))))(Secondary((id \
         ca50b2a4-a9ca-4686-9814-9df1cccfa75a)(content(Comment\"#                                                                   \
         #\"))))(Secondary((id \
         fafe35a4-f7f7-4b38-ad38-a0e722d2d88c)(content(Whitespace\"\\n\"))))(Secondary((id \
         c1f5c916-ac7a-423a-8978-b8d7713015de)(content(Comment\"#   clamp(5, \
         0, 10) == 5    (in range \\226\\128\\148 return x)                  \
         #\"))))(Secondary((id \
         bc4d6dda-e7e5-4195-8174-b5ca42656325)(content(Whitespace\"\\n\"))))(Secondary((id \
         a7fac324-a9a5-4b8d-a6df-5f7dd90c3a62)(content(Comment\"#   clamp(-3, \
         0, 10) == 0   (below min \\226\\128\\148 return lo)                \
         #\"))))(Secondary((id \
         8cc972ce-6a89-40bb-85ff-bef1d6800ddf)(content(Whitespace\"\\n\"))))(Secondary((id \
         d26e3e28-a6fe-49c8-8468-ad9ec047bfef)(content(Comment\"#   clamp(15, \
         0, 10) == 10  (above max \\226\\128\\148 return hi)                \
         #\"))))(Secondary((id \
         b49c168a-449f-4dec-864b-e40fbbf05753)(content(Whitespace\"\\n\"))))(Secondary((id \
         5088a83b-5221-4b0f-8ca7-bd0f7ce24fca)(content(Comment\"#                                                                   \
         #\"))))(Secondary((id \
         5d45185e-a0ec-43da-8a31-e753128f226e)(content(Whitespace\"\\n\"))))(Secondary((id \
         5992248b-1449-4f91-b522-c565843ec466)(content(Comment\"# Replace the \
         ? with your implementation using if/else:             \
         #\"))))(Secondary((id \
         0535e814-815b-4bb6-86da-c091daf23ffe)(content(Whitespace\"\\n\"))))(Secondary((id \
         8ceba7b2-81bf-46a4-a6a6-70db2d22bb6d)(content(Comment\"#   if cond \
         then expr else expr                                     \
         #\"))))(Secondary((id \
         cc0c8eff-9c29-41ff-8973-e563659b3771)(content(Whitespace\"\\n\"))))(Secondary((id \
         6666408f-359c-40d0-a5eb-ad997f241969)(content(Comment\"#                                                                   \
         #\"))))(Secondary((id \
         4c772c8a-b78d-4cee-b99d-14efd474c46a)(content(Whitespace\"\\n\"))))(Secondary((id \
         99fe2a23-4229-4448-be80-2525153f3eea)(content(Comment\"# Turn on \
         auto-probe and click inside your function as you write   \
         #\"))))(Secondary((id \
         6f6b8895-915f-40bb-bf96-9249256779a6)(content(Whitespace\"\\n\"))))(Secondary((id \
         7f102b32-b02f-4f8f-9e60-b47ce9614e31)(content(Comment\"# to see \
         intermediate values update live.                           \
         #\"))))(Secondary((id \
         d0100e3c-4cf6-4340-8ad8-22014573dc5e)(content(Whitespace\"\\n\"))))(Secondary((id \
         b82368d0-821c-4ef6-b34c-64a86376185d)(content(Comment\"# \
         =============================================================== \
         #\"))))(Secondary((id \
         7c02479c-8a17-439d-977b-d3e1d6b405cc)(content(Whitespace\"\\n\"))))(Secondary((id \
         9cb89f98-b0fb-44d5-90a6-82d070f03686)(content(Whitespace\"\\n\"))))(Tile((id \
         f8e8e088-06c7-4212-a370-8cdce3b2119f)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         2733757c-221e-45e9-8a73-bcb945cc9660)(content(Whitespace\" \
         \"))))(Tile((id \
         6ff5c12f-36b1-4c4e-9996-33b51d2114e0)(label(clamp))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         adee10c5-3415-4596-b369-3b076c700977)(content(Whitespace\" \
         \")))))((Secondary((id \
         da71a493-3d65-45b8-b9e9-d0a32469b420)(content(Whitespace\" \
         \"))))(Tile((id 39025531-35b8-4426-8db5-6474a2a43cd0)(label(fun \
         ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 37))(sort Exp))))))(shards(0 \
         1))(children(((Secondary((id \
         394db5ab-a7a3-41b1-a8f2-1424c7a92df4)(content(Whitespace\" \
         \"))))(Tile((id \
         2b30cf63-671e-441b-b9dc-babab534f303)(label(\"(\"\")\"))(mold((out \
         Pat)(in_(Pat))(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0 1))(children(((Tile((id \
         e9a659fa-4f8a-49f5-aa6f-6795666e4d25)(label(x))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         b3c5b049-927b-4dbf-9d35-32ea2b4f62bf)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 44))(sort Pat))((shape(Concave \
         44))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         9cc4c8ed-d1c8-43de-8502-c299747c424f)(content(Whitespace\" \
         \"))))(Tile((id \
         801e3018-e8e1-43cc-b3ac-e128d37db407)(label(lo))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         824e8c6c-a85e-4a61-8de3-96cb0614727b)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 44))(sort Pat))((shape(Concave \
         44))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         bda26674-b4a6-4ea9-ab36-fedcec8f8874)(content(Whitespace\" \
         \"))))(Tile((id \
         16647c3c-2296-4fa9-a433-c5914b2da4a1)(label(hi))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children()))))))))(Secondary((id \
         04485876-79b9-4ad7-98b1-3d4f30a0aa42)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         79201930-0880-42bf-8fa1-7b260fc3c22a)(content(Whitespace\"\\n\"))))(Tile((id \
         70a71562-d804-4981-bb7b-f6ac16ef3e35)(label(?))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         84dcddc8-13bf-4401-9711-f8f582269a71)(content(Whitespace\"\\n\"))))(Secondary((id \
         d8fe830e-1b18-4267-919f-c3e69df72b7c)(content(Whitespace\"\\n\"))))(Secondary((id \
         865cb2bc-3aea-4dd7-8ae6-7387160e50b4)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         8aedeada-fa6d-4acb-8246-0b3cbe79ee96)(content(Whitespace\"\\n\"))))(Secondary((id \
         6e2687a1-e039-4dca-8652-da2dd9d17de5)(content(Whitespace\"\\n\"))))(Tile((id \
         73ced92d-5240-4b7e-9e9e-cbb387c0ec5e)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         c1a652ef-88f6-4500-9c46-9d12e8aa4a5e)(content(Whitespace\"\\n\"))))(Tile((id \
         b27d344f-5b10-4657-8f01-6933dda80e6b)(label(clamp))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         06a4cc86-f377-4549-a806-3e7e8e9de697)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         63fd3c63-c6cc-4883-890a-a0a45ec1893d)(label(5))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         c7f5ed20-5cd0-49f7-95d7-6cd265c87c2c)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         230f1a07-e546-4383-be2a-fab1b5683d3e)(content(Whitespace\" \
         \"))))(Tile((id \
         6b1e1568-ec0f-4cea-b56b-add1dd9a2678)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         131fcdd3-1fd4-4d86-ba30-ea4bf7aa7b39)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         454783d4-e6f1-475d-86f2-ab630cb2debf)(content(Whitespace\" \
         \"))))(Tile((id \
         497e815a-ea1b-47a3-9f9b-7c56fe2097aa)(label(10))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         93b8e259-cb0b-4d4b-8faf-9acea5d32490)(content(Whitespace\"\\n\"))))(Tile((id \
         3b0ea505-9e68-4f84-a026-be59168d0e45)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         49667898-84a6-4763-950e-15937de0788f)(content(Whitespace\" \
         \"))))(Tile((id \
         e4f1087a-9842-463f-98a2-8eb2a7eb79ca)(label(5))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         c7413b18-c946-442d-a2f9-21f2c27937b0)(content(Whitespace\"\\n\")))))))))(Tile((id \
         45c2c8df-60be-414c-a998-7f30442fdaff)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         ab3906a8-e29b-48cd-8178-76a4a3029f01)(content(Whitespace\"\\n\"))))(Secondary((id \
         f5ec2795-aa15-4e23-b757-4ff417bad2dd)(content(Whitespace\"\\n\"))))(Tile((id \
         9839aa74-705b-4710-8297-6d89ad82fb19)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         fb21672a-7b55-454c-9dec-c25f74a78dc9)(content(Whitespace\"\\n\"))))(Tile((id \
         dc8e99ae-8065-47f1-b61f-2357f3128b5c)(label(clamp))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         20f35327-51e4-4f93-9c79-e80288cb395c)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         2cd95d8d-2842-4538-9734-d5b85e5a7d87)(label(-))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape(Concave 25))(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         d9cb7dd0-18da-4eb3-9c08-ee77115f53a1)(label(3))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         31f24c7b-727e-41f4-b3f3-7d55fd7b69bc)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         bbc82d4b-2791-49ba-ab35-a8915d491662)(content(Whitespace\" \
         \"))))(Tile((id \
         5a8f975c-5489-4402-84f7-f59ec3f79e58)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         307767ca-df7a-4835-977a-96e06d3e5f82)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         12bdfef0-5f11-4e44-806a-95871e04065f)(content(Whitespace\" \
         \"))))(Tile((id \
         77bf8c24-a2fb-4ea5-b817-41df11a7db90)(label(10))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         26619f5b-c95a-49e1-b191-522ce67747cf)(content(Whitespace\"\\n\"))))(Tile((id \
         85cc0c0f-0aa1-4e86-af39-96724972fe71)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         b8caf7fb-87fb-48fa-a79e-9c6d1c389661)(content(Whitespace\" \
         \"))))(Tile((id \
         decfaa6c-d9c7-412a-8571-d3a61c70fa6b)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         ae36050b-f359-4e30-84ef-bf813a744aad)(content(Whitespace\"\\n\")))))))))(Tile((id \
         81c816eb-edf3-42df-86b4-65cc8808fb1e)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         49d0057e-3158-4a24-8f9f-546f3e5e2ec4)(content(Whitespace\"\\n\"))))(Secondary((id \
         9632dd07-0f65-4650-a0f0-24f1b5c5e13f)(content(Whitespace\"\\n\"))))(Tile((id \
         e2f67fac-a573-4fb6-9da8-53b071f28747)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         c2eff4f5-e857-41a1-bbbb-13df9441bf09)(content(Whitespace\"\\n\"))))(Tile((id \
         20a1cb60-1cfd-41b0-803e-abc05ca2b2b5)(label(clamp))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         c47b782c-1c10-40f7-aea8-a71c3b51ed2c)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         f8b69107-96bd-4957-a3bb-e560ec281e1f)(label(15))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         1fad9e69-5ae7-4b5a-a1da-9cf2ae26bdbe)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         407d83bd-49b1-452f-a811-ad32ab035287)(content(Whitespace\" \
         \"))))(Tile((id \
         36339e24-2bd2-421d-87f8-05d631db9bff)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         59a12010-42f7-40f1-85f6-ae41053d20cd)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         b1aad262-0aa0-4e95-b89d-d6ed424b78b4)(content(Whitespace\" \
         \"))))(Tile((id \
         125ea537-3799-48e2-ad96-f020e4fdad2c)(label(10))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         2bf7a227-4684-446b-b8e4-4f106f0851f6)(content(Whitespace\"\\n\"))))(Tile((id \
         70fe600b-1cf1-49ed-b60c-08cce0f796b8)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         284882cd-d3bd-4af9-9dbc-9704891303d6)(content(Whitespace\" \
         \"))))(Tile((id \
         0436e913-10c3-43bc-8e12-cc5aea71b6e8)(label(10))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         2a021252-f018-44a9-a5d0-70b487da4206)(content(Whitespace\"\\n\")))))))))(Tile((id \
         6aedb7cc-8c3d-46b0-95f8-c99ed988cd54)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         2d59689f-6e14-4ba0-8a35-81c2ab0b6ba2)(content(Whitespace\"\\n\"))))(Secondary((id \
         2de17253-f126-42db-824a-b345f4a04dc3)(content(Whitespace\"\\n\"))))(Tile((id \
         9c0baca0-a200-4b9d-bc8c-07e8278b6471)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         2f31634f-bc8c-454b-b214-c60f0f77486c)(content(Whitespace\"\\n\"))))(Tile((id \
         3a1638c5-161c-4992-957e-e24caa9dca14)(label(clamp))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         d04fca79-81c1-4359-b1eb-31b0901df641)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         a44bb62f-ed21-45db-8160-951c61e9df54)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         fb6c8e58-51b3-489a-94eb-532033d645c8)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         359798e0-621b-4ec2-84e0-896da4c43344)(content(Whitespace\" \
         \"))))(Tile((id \
         932bf498-5f43-44d5-92be-810c0324d494)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         dd225ae3-78a4-4477-96e6-e53d140dd86f)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         02c57c3c-e329-4cd7-a360-f3728f6a671b)(content(Whitespace\" \
         \"))))(Tile((id \
         6b0f80d2-1e6e-4faa-bb2d-6ddec301ae66)(label(10))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         35477876-fd9d-4ec9-b475-14f0d3778fca)(content(Whitespace\"\\n\"))))(Tile((id \
         df80dda0-f893-43ec-9037-8fef3d093be8)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         21bb11ab-de1b-41a4-9d5e-5c7a155e25f5)(content(Whitespace\" \
         \"))))(Tile((id \
         97f2d0e6-1cde-42a3-9600-3f160d75d46e)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         41a9c6fb-d4fb-4d52-9f8e-01264d465c7b)(content(Whitespace\"\\n\")))))))))(Tile((id \
         80c1dc07-2b27-4524-8164-a35fb2b44d8b)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         0e9c4045-2e96-4dfb-ae00-af5201192b4c)(content(Whitespace\"\\n\"))))(Secondary((id \
         841dd909-f90b-46b3-ba7d-c55ce103845f)(content(Whitespace\"\\n\"))))(Tile((id \
         ae51196f-e6f3-45e4-ab1d-9aaba2cb935c)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         b17cf7c5-7b38-41f8-a110-7f716ed9b26d)(content(Whitespace\"\\n\"))))(Tile((id \
         0f05fa26-e0f1-434a-8f49-7844611441a1)(label(clamp))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         2da5ea36-5fc8-43a1-9cdc-8a2d3b57c4a5)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         94b81456-2c29-40df-b9bf-4e13e4543771)(label(10))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         d224e667-094a-4663-b915-184659e9b9b6)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         f207e5a0-8d46-46d1-82e4-8c25ef2bd67b)(content(Whitespace\" \
         \"))))(Tile((id \
         d51feb72-22e2-4c09-aa06-5586d4351790)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         da7642d3-1c94-468f-8b43-2c70cbc60a0d)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         fc6cf8f4-8a0e-4a74-af0f-3a4c663651b5)(content(Whitespace\" \
         \"))))(Tile((id \
         145b0fb6-e45c-400f-9cfa-73fc54275116)(label(10))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         10fb1795-06b0-417a-950f-f45f11efb6b5)(content(Whitespace\"\\n\"))))(Tile((id \
         0219a1cf-3c81-42b8-97d0-327bb2497a38)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         f97ec065-583c-4316-872f-f4d48211798c)(content(Whitespace\" \
         \"))))(Tile((id \
         49c3bfa2-c468-4d07-86df-62639e7c1668)(label(10))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         3965f5e3-b268-48d4-ac24-ac41d823f0a0)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         f985a908-4e82-4a9f-8d28-079e97c98dfd)(content(Whitespace\"\\n\"))))(Secondary((id \
         d966889d-545a-4ed6-b840-d0b5a30410ea)(content(Whitespace\"\\n\"))))(Secondary((id \
         f8492b7f-9c68-4e0d-b3e0-55ecaa6152ff)(content(Comment\"# END OF PART \
         8 - Select the next slide from the top menu       \
         #\"))))(Secondary((id \
         de5e1a70-c309-4234-a745-0fd6940f9eff)(content(Whitespace\"\\n\")))))";
      backup_text =
        "# PROBES TUTORIAL - PART 8: WRITING EXERCISE (CLAMP)               #\n\n\
         # Now it's your turn to write some Hazel code.                     #\n\
         # Implement `clamp`: constrain a number to be within [lo, hi].     #\n\
         #                                                                   #\n\
         #   clamp(5, 0, 10) == 5    (in range \226\128\148 return \
         x)                  #\n\
         #   clamp(-3, 0, 10) == 0   (below min \226\128\148 return \
         lo)                #\n\
         #   clamp(15, 0, 10) == 10  (above max \226\128\148 return \
         hi)                #\n\
         #                                                                   #\n\
         # Replace the ? with your implementation using if/else:             #\n\
         #   if cond then expr else expr                                     #\n\
         #                                                                   #\n\
         # Turn on auto-probe and click inside your function as you write   #\n\
         # to see intermediate values update live.                           #\n\
         # =============================================================== #\n\n\
         let clamp = fun (x, lo, hi) ->\n\
         ?\n\n\n\
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
         end\n\n\
         # END OF PART 8 - Select the next slide from the top menu       #\n";
      refractors = "()";
    } )
