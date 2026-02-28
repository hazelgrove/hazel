let out : string * Haz3lcore.PersistentSegment.t =
  ( "Study / tutorial / 05-step-into",
    {
      segment =
        "((Secondary((id \
         4c695e79-5166-4b94-8bfb-0a650b5e9caf)(content(Comment\"# PROBES \
         TUTORIAL - PART 5: STEP INTO AND THE DYNAMIC CURSOR BAR \
         #\"))))(Secondary((id \
         665a473e-1ab6-49a1-bb11-03efe701de3c)(content(Whitespace\"\\n\"))))(Secondary((id \
         d3e379f6-183b-443c-a889-96e9b60178ea)(content(Comment\"# You've \
         pinned a call and can see values inside a function. \
         #\"))))(Secondary((id \
         0f38f092-67b0-4e31-b91d-6ecda0da9861)(content(Whitespace\"\\n\"))))(Secondary((id \
         d03a0528-9508-4b80-8f84-ac262ecabc6a)(content(Comment\"# But what if \
         the bug is deeper, inside a function that your #\"))))(Secondary((id \
         4449fa4e-b31e-44d8-a030-c9a96959714b)(content(Whitespace\"\\n\"))))(Secondary((id \
         34df08a8-8f3d-4721-969f-90e880a2246c)(content(Comment\"# function \
         calls? Step Into follows the call stack down. #\"))))(Secondary((id \
         0f9fc154-4a0a-464e-863d-90ee8767bdb9)(content(Whitespace\"\\n\"))))(Secondary((id \
         9524a7c2-b083-450b-b2cb-91ac9aa8d201)(content(Whitespace\"\\n\"))))(Secondary((id \
         cab4e0eb-8707-4dd4-9253-168048021bee)(content(Comment\"# TRY THIS: \
         #\"))))(Secondary((id \
         9ad17b63-b4f0-47c7-a9e1-d61fb4f5e0d5)(content(Whitespace\"\\n\"))))(Secondary((id \
         469a6de2-054b-4c64-aa95-38b44768cdaa)(content(Comment\"# 1. Turn on \
         auto-probe and click inside `daily_water` #\"))))(Secondary((id \
         9777c9f8-c5e2-42aa-a815-4ff8d6a26c6d)(content(Whitespace\"\\n\"))))(Secondary((id \
         39f79689-8bea-4d83-a6e8-7979ee36231e)(content(Comment\"# 2. Pin one \
         of the test calls (click a sample > Pin) #\"))))(Secondary((id \
         ac446f74-28b1-42f2-8c20-3c950c98fa85)(content(Whitespace\"\\n\"))))(Secondary((id \
         e20abe02-6cf9-4450-938b-436eec0e2e6f)(content(Comment\"# 3. Now add a \
         probe to the `phase_multiplier(phase)` call #\"))))(Secondary((id \
         191fce35-a779-454b-b36c-69179f7797f7)(content(Whitespace\"\\n\"))))(Secondary((id \
         60ccd0bb-25d8-4b61-aed1-d5b13a213331)(content(Comment\"#    inside \
         `daily_water` (click on `phase_multiplier`) #\"))))(Secondary((id \
         f26d9015-e10b-4c39-b229-d8ba6f9a56f0)(content(Whitespace\"\\n\"))))(Secondary((id \
         bc5bfbb0-237b-4985-a06c-5ba87e9d80ba)(content(Comment\"# 4. Click \
         that sample and choose \\\"Step Into\\\" from the \
         #\"))))(Secondary((id \
         3123c2dc-4d75-48dc-9e41-e21b7702dee0)(content(Whitespace\"\\n\"))))(Secondary((id \
         b828ec23-d940-4156-851e-4004df722b8e)(content(Comment\"#    dropdown \
         (or press Enter) #\"))))(Secondary((id \
         a23e27be-dae4-43f1-a3c0-28c32595fc9d)(content(Whitespace\"\\n\"))))(Secondary((id \
         276e1ad6-9be2-4b64-8f20-baf6c3c0d181)(content(Comment\"# 5. Your \
         cursor jumps into `phase_multiplier`! The probes \
         #\"))))(Secondary((id \
         605ac075-29f2-47c8-ba79-fbd3c51a90e8)(content(Whitespace\"\\n\"))))(Secondary((id \
         d1fc101d-4aae-410c-9b37-f1765c644c84)(content(Comment\"#    there \
         show only values from your pinned context. #\"))))(Secondary((id \
         08192726-2ad6-4359-8e6f-4ff29b1c742c)(content(Whitespace\"\\n\"))))(Secondary((id \
         4363176d-dda3-43b7-b6b6-32360253dcfd)(content(Whitespace\"\\n\"))))(Secondary((id \
         2aa58f4f-b875-4818-9ffa-b65072d0fa3d)(content(Comment\"# THE DYNAMIC \
         CURSOR BAR #\"))))(Secondary((id \
         56bdba18-2473-4cb7-b462-9c377762fd42)(content(Whitespace\"\\n\"))))(Secondary((id \
         e536552f-f0cd-46df-9c86-e20f4eefb68e)(content(Comment\"# Look at the \
         bar at the top of the editor after stepping in. #\"))))(Secondary((id \
         a0e65cb4-8846-4bd8-9f26-d2f8d67ea76b)(content(Whitespace\"\\n\"))))(Secondary((id \
         1eeab61a-69f4-42be-b314-acf573a4ef5b)(content(Comment\"# It shows \
         your position in the call stack as breadcrumbs: #\"))))(Secondary((id \
         94792b07-6477-4d08-8be9-d444136523c4)(content(Whitespace\"\\n\"))))(Secondary((id \
         bdd006f0-5c08-4c1d-a960-3401edc9d749)(content(Comment\"#   top-level \
         > daily_water > phase_multiplier #\"))))(Secondary((id \
         5a798595-067d-4911-b03d-a758d2e25301)(content(Whitespace\"\\n\"))))(Secondary((id \
         967b263d-8a65-4b38-b05c-84638d5a5522)(content(Comment\"# Click a \
         function name to jump to its definition. #\"))))(Secondary((id \
         9c10a1b3-feb7-4eb6-b56b-deee81ae9dc5)(content(Whitespace\"\\n\"))))(Secondary((id \
         e1799b3d-3e05-4274-8f88-150f80b7e534)(content(Comment\"# Click a \
         chevron (>) to jump to the call site. #\"))))(Secondary((id \
         483674ed-b1c5-4296-9699-2d590782476f)(content(Whitespace\"\\n\"))))(Secondary((id \
         27326529-62e8-4f95-8221-a21b8615dc0e)(content(Comment\"# This lets \
         you move up and down the call stack freely. #\"))))(Secondary((id \
         5795842f-8d0f-4fb5-b341-4c59e5346124)(content(Whitespace\"\\n\"))))(Secondary((id \
         f6912846-446f-4d76-a241-c49b6d762dfc)(content(Whitespace\"\\n\"))))(Tile((id \
         2f11caf7-c282-4b4f-9332-c2823dd6e816)(label(type = in))(mold((out \
         Exp)(in_(TPat Typ))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         4d10e6ec-47e5-455a-8772-ea02ee88621e)(content(Whitespace\" \
         \"))))(Tile((id \
         e4f2bf5f-dcaa-4442-83e0-dcbb31da482c)(label(MoonPhase))(mold((out \
         TPat)(in_())(nibs(((shape Convex)(sort TPat))((shape Convex)(sort \
         TPat))))))(shards(0))(children())))(Secondary((id \
         08fc6638-2872-435a-bfab-0c40d46b99a5)(content(Whitespace\" \
         \")))))((Secondary((id \
         39c7f5f5-cead-4cd2-a2bf-089cb4a4e004)(content(Whitespace\" \
         \"))))(Tile((id \
         07d2f6d6-c8b4-4976-99b0-959c21d06b7a)(label(+))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape(Concave 33))(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         7989052f-f108-4f17-a92a-acb0aa23c763)(content(Whitespace\" \
         \"))))(Tile((id \
         650c1e9f-a304-4b33-859d-481d7ac557e4)(label(New))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         4052a6d8-98d5-41c4-a2d8-d8ea1d1742cc)(content(Whitespace\" \
         \"))))(Tile((id \
         3b9bcdba-2181-440a-982e-512d0fdca780)(label(+))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 12))(sort Typ))((shape(Concave \
         12))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         f0443062-ba06-4d15-aaa6-21e8e47e8722)(content(Whitespace\" \
         \"))))(Tile((id \
         666f3989-7d48-45b7-b665-c921c0759bce)(label(Waxing))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         69733aef-b8e6-40f2-8f59-e88f21e9571f)(content(Whitespace\" \
         \"))))(Tile((id \
         496937cd-050e-4660-b282-8154117dd60c)(label(+))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 12))(sort Typ))((shape(Concave \
         12))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         f06a5d55-c06b-426d-99fb-2d7e2b6c11cb)(content(Whitespace\" \
         \"))))(Tile((id \
         cf2c169b-41b6-40fd-97f6-d65bcd3dc898)(label(Full))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         fdc25d91-9bdf-4e51-9cae-07397b213a41)(content(Whitespace\" \
         \"))))(Tile((id \
         69b333d2-d981-4195-b426-ad7793a166ac)(label(+))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 12))(sort Typ))((shape(Concave \
         12))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         66e9310a-c8ae-4cf5-83c3-fec29aa999a8)(content(Whitespace\" \
         \"))))(Tile((id \
         efb3e6c9-18a3-4f13-a4ad-0432dfe4fe62)(label(Waning))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         77a23520-a725-4bbe-939b-ef6a8207718d)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         5fc5bcc5-0277-40b7-be56-ba14333c0b68)(content(Whitespace\"\\n\"))))(Secondary((id \
         77a9892f-90d8-48b7-af80-e4d7e4b20966)(content(Whitespace\"\\n\"))))(Tile((id \
         c70c3635-d4c1-4a48-88e9-dd19714950c5)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         0e9f8735-db88-4c61-9ea2-1ccd85b5d577)(content(Whitespace\" \
         \"))))(Tile((id \
         b2c9d163-cb76-485e-84d3-5170e0e81198)(label(phase_multiplier))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         4f95a4d5-5be7-4ed7-ba2e-64f58cdc9b1c)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 24))(sort Pat))((shape(Concave \
         24))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         81fd0a04-3c53-4d81-8b69-1137507cb576)(content(Whitespace\" \
         \"))))(Tile((id \
         1f805310-4ac5-46dc-a4ee-e9c2853c071f)(label(MoonPhase))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         4052e142-9bcc-4453-b5ae-e8d0fac4533c)(content(Whitespace\" \
         \"))))(Tile((id \
         e9bdd0b5-faf0-4e13-8b63-c7aea0ee0e45)(label(->))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 13))(sort Typ))((shape(Concave \
         13))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         0c66fd8a-1ef5-484b-8317-f02cce4d2182)(content(Whitespace\" \
         \"))))(Tile((id \
         792a47b6-e28c-479f-b594-cf624403ea9f)(label(Float))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         ea9c7ec3-67e9-4e07-9f44-8b33babfbe52)(content(Whitespace\" \
         \")))))((Secondary((id \
         fe39738e-aed1-41ae-a9b2-77a28ae202e8)(content(Whitespace\"\\n\"))))(Tile((id \
         97efd050-6f8c-4c03-ac42-e3196e8e8cb0)(label(fun ->))(mold((out \
         Exp)(in_(Pat))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         37))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         3ef18346-d030-4385-8a73-e54a41741f72)(content(Whitespace\" \
         \"))))(Tile((id \
         03bab45d-8f70-4817-92a1-88b447b024e6)(label(phase))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         12913f1e-944a-4a16-8cca-82194b0c765d)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         ab548240-9d2e-4049-a0d6-3f8311a7d0d3)(content(Whitespace\" \
         \"))))(Tile((id 36d919be-fe9a-467b-a362-32784ccbb058)(label(case \
         end))(mold((out Exp)(in_(Rul))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         3d85fb7a-81f5-45f5-be84-980e48629066)(content(Whitespace\" \
         \"))))(Tile((id \
         a0026e0e-9259-43ab-bba3-919fc0d444fd)(label(phase))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         400eaf40-9ef4-4b44-9467-0bc299adee78)(content(Whitespace\"\\n\"))))(Tile((id \
         e76fcdb1-fb1d-4f8e-97f6-740712ccae33)(label(| =>))(mold((out \
         Rul)(in_(Pat))(nibs(((shape(Concave 46))(sort Exp))((shape(Concave \
         46))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         675e38f9-8da9-4fdd-b09e-57b1101c79d2)(content(Whitespace\" \
         \"))))(Tile((id \
         d91ec7db-ca07-4b69-8238-1728e9736fed)(label(New))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         bbe1f12b-720e-44a4-8c76-6bb58faf46cd)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         9ebbb27f-e897-4edc-b9b2-95996ebc717c)(content(Whitespace\" \
         \"))))(Tile((id \
         90dfcf5f-991a-4c21-b0ca-fb35c20019d7)(label(1.2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         82e77d0e-858a-4fd6-a6b3-1a63f5af889c)(content(Whitespace\"\\n\"))))(Tile((id \
         e9b5a4d2-5cd4-4f35-98a0-c82407b2d43c)(label(| =>))(mold((out \
         Rul)(in_(Pat))(nibs(((shape(Concave 46))(sort Exp))((shape(Concave \
         46))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         79df60a6-be3e-4843-800b-31973d88e0da)(content(Whitespace\" \
         \"))))(Tile((id \
         7dd3b919-2467-484f-a5ea-3d517892075e)(label(Waxing))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         aa5a9b83-1e94-41b0-adc8-c96dd008baff)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         af836179-0f7c-4477-ac7a-73d8289f039f)(content(Whitespace\" \
         \"))))(Tile((id \
         f5a6f50b-4fc8-41db-a3eb-0bfb09df57db)(label(1.1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         91e202cb-9f41-4303-8f26-b2033b2906a0)(content(Whitespace\"\\n\"))))(Tile((id \
         70f0e745-6fa5-4ed3-b4b4-80386e584f28)(label(| =>))(mold((out \
         Rul)(in_(Pat))(nibs(((shape(Concave 46))(sort Exp))((shape(Concave \
         46))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         29ebbcbd-9639-46a4-a6db-1e71faa4bb80)(content(Whitespace\" \
         \"))))(Tile((id \
         e3de08f3-b16b-46e8-87b8-715a182aba61)(label(Full))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         559076ec-d021-438f-a5c8-d9c910f62b3c)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         708b0d5a-7385-4ad1-aa21-146c64ff5e63)(content(Whitespace\" \
         \"))))(Tile((id \
         b41fabd2-b5bb-42a0-b4db-f4b17a700aaa)(label(0.88))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         be8a67ee-d304-4cae-8223-056f6021b852)(content(Whitespace\"\\n\"))))(Tile((id \
         15482d99-0d83-4ec4-bfe4-18daaaaa7721)(label(| =>))(mold((out \
         Rul)(in_(Pat))(nibs(((shape(Concave 46))(sort Exp))((shape(Concave \
         46))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         3a42f5df-8d33-45a4-b8f6-310bfeb6ad2f)(content(Whitespace\" \
         \"))))(Tile((id \
         fa57d6a6-6167-4c47-a2aa-39a378c0342a)(label(Waning))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         1576242e-f4d6-4714-846f-1f203df564aa)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         a7fc1298-94bd-4659-85b6-bc2bfdb89cc4)(content(Whitespace\" \
         \"))))(Tile((id \
         2f3242c9-65a4-4760-bad3-9e1282f1384a)(label(0.95))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         a3ca46df-9c5a-4ea2-b708-3cef5a3aad94)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         d05113a1-6936-46c8-87aa-a8c575d35215)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         e82563ca-c877-4e8d-8f10-787b3c1d1801)(content(Whitespace\"\\n\"))))(Secondary((id \
         ddb2baa2-7915-455c-a6dd-103dbd9a574f)(content(Whitespace\"\\n\"))))(Tile((id \
         534c89fa-9010-48f7-a115-7c594bc4837d)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         7de7788b-e2f1-4017-b1ce-eeebac62b7d0)(content(Whitespace\" \
         \"))))(Tile((id \
         dae6cb6e-1232-440b-b6f0-423029ffb41b)(label(shade_multiplier))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         ae6d781c-8db9-45d9-9e41-735614b3ade9)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 24))(sort Pat))((shape(Concave \
         24))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         48838bf7-42c4-4f6e-83a0-b92161aae6ed)(content(Whitespace\" \
         \"))))(Tile((id \
         ead64aee-bbdf-47d4-b0d0-5a82f6b8f310)(label(Int))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         2a329ea4-920c-4834-9d03-d59902dfb1ad)(content(Whitespace\" \
         \"))))(Tile((id \
         27667513-097f-4a5b-942c-d1d9c2111386)(label(->))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 13))(sort Typ))((shape(Concave \
         13))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         b13419be-9c42-442e-a2c2-bebb054bebe9)(content(Whitespace\" \
         \"))))(Tile((id \
         6a36435c-4560-44f0-b1bb-8abac64689a2)(label(Float))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         0a1dd744-2ad6-41ce-a2b3-c6d85255f582)(content(Whitespace\" \
         \")))))((Secondary((id \
         237c4170-45ba-4e6a-8f08-d7dd8f548303)(content(Whitespace\"\\n\"))))(Tile((id \
         1c400072-2cbc-4483-9e98-b35b69c0c383)(label(fun ->))(mold((out \
         Exp)(in_(Pat))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         37))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         94ebb812-ddef-46b6-a060-812420c458d1)(content(Whitespace\" \
         \"))))(Tile((id \
         abe3712a-da0d-4df0-ba8b-fa6cef564a7a)(label(shade))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         4b62cc1b-2cc8-4532-b5f8-64d4812f4d64)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         f56d5ecd-78a5-482f-bedc-a319baa823d0)(content(Whitespace\" \
         \"))))(Tile((id 19204f19-8bfc-45e4-952d-b55ba9a93485)(label(case \
         end))(mold((out Exp)(in_(Rul))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         beb0e511-b99b-45d6-a781-0d75aa01621f)(content(Whitespace\" \
         \"))))(Tile((id \
         4aa542ad-508f-464f-a6bc-6e9c8165b8ed)(label(shade))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         64dcea3d-e962-48ae-bccc-3ef021f5bc28)(content(Whitespace\"\\n\"))))(Tile((id \
         ade263a3-6ff1-458d-b1e4-216f6a3e4244)(label(| =>))(mold((out \
         Rul)(in_(Pat))(nibs(((shape(Concave 46))(sort Exp))((shape(Concave \
         46))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         4cab37f6-4be9-48b1-8a26-15ce151bd640)(content(Whitespace\" \
         \"))))(Tile((id \
         320a86ed-d80d-4464-9d86-e0afdc9ecd99)(label(0))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         4c2aa25a-7cf7-4664-aa22-97fce646d4d2)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         fde9302d-38fd-4b1f-b6b8-8af58eb46470)(content(Whitespace\" \
         \"))))(Tile((id \
         f71e4864-3ba8-49f6-941e-0d6c0efc2750)(label(1.0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         26cdb4e9-e113-4894-89f6-16281e7813bc)(content(Whitespace\"\\n\"))))(Tile((id \
         251fb2db-ba4b-4adf-b95c-55a63fb3e30c)(label(| =>))(mold((out \
         Rul)(in_(Pat))(nibs(((shape(Concave 46))(sort Exp))((shape(Concave \
         46))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         b3df0109-28ed-4ab2-b93f-6a6c688bf3b3)(content(Whitespace\" \
         \"))))(Tile((id \
         16805ae9-3aee-4ddc-b19b-f03c16a96bf4)(label(1))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         ce236e75-dd75-4dd9-8164-1acc93a0b928)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         823d27c2-e9b9-41c4-8b1a-a2b6d8dba76a)(content(Whitespace\" \
         \"))))(Tile((id \
         86043bde-8b05-429a-8931-7e7d996a0e3d)(label(0.9))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         e75a69f7-ade5-4660-aaf0-5c90dec35b68)(content(Whitespace\"\\n\"))))(Tile((id \
         c4df13b5-664b-4fe0-9375-9e73e4027a87)(label(| =>))(mold((out \
         Rul)(in_(Pat))(nibs(((shape(Concave 46))(sort Exp))((shape(Concave \
         46))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         9d7f1cf2-1722-4c59-96d9-854a73be03b5)(content(Whitespace\" \
         \"))))(Tile((id \
         6eae2226-718c-47e2-b55c-7b64f37d14ca)(label(2))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         2b6f53c9-e2ee-4486-9c6a-e8cae025a658)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         a1414487-e3c9-421a-96b8-6c0355f1023d)(content(Whitespace\" \
         \"))))(Tile((id \
         2795cb3c-2474-410c-aa9b-fa9781277199)(label(0.75))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         2ffea129-88e9-49d2-b4e7-4aa51640d344)(content(Whitespace\"\\n\"))))(Tile((id \
         1e8ca8f8-bd5a-4e46-932c-fb457bbca528)(label(| =>))(mold((out \
         Rul)(in_(Pat))(nibs(((shape(Concave 46))(sort Exp))((shape(Concave \
         46))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         8e47e9d0-06a9-446a-9522-773006af479d)(content(Whitespace\" \
         \"))))(Tile((id \
         dc5f54ac-9065-4176-abb5-9913ab287641)(label(_))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         532aa1be-e8d2-41de-abb3-1c95e8a6cc60)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         0fd97a33-7b44-415e-9674-d0f2a6f98736)(content(Whitespace\" \
         \"))))(Tile((id \
         0fa8e1f0-4eb4-4c4a-b464-a9008a96e12d)(label(0.6))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         2301e876-44bd-4dab-8971-517282c54995)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         85b21955-55a0-44ed-a383-8190e79d43ea)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         55b58469-f0f6-4d0c-956b-ee3283de5bac)(content(Whitespace\"\\n\"))))(Secondary((id \
         2a3ba6d1-1574-45b7-91c5-b3f1998ca2c9)(content(Whitespace\"\\n\"))))(Tile((id \
         380b5b3c-a735-442f-8528-702325bf23d2)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         88a21481-0c00-4e74-8ebe-128bbced70bf)(content(Whitespace\" \
         \"))))(Tile((id \
         b8c5e09f-6abd-4ec4-b4fc-99d71ee5fb56)(label(daily_water))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         c84d9376-e837-461b-859d-573b09158b68)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 24))(sort Pat))((shape(Concave \
         24))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         20eae83b-81d7-41a8-9b8c-009b4fa76e6b)(content(Whitespace\" \
         \"))))(Tile((id \
         cc945b21-2882-405d-8db4-c6bdf61ce945)(label(\"(\"\")\"))(mold((out \
         Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0 1))(children(((Tile((id \
         a96e8429-49bd-4698-b83e-151d0a065e7e)(label(Int))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         f6005984-6d69-4185-8317-c47325193263)(label(,))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 44))(sort Typ))((shape(Concave \
         44))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         7f8f07af-f6c8-49a5-8cbf-d8feb9b9ccde)(content(Whitespace\" \
         \"))))(Tile((id \
         245a0b43-c029-4a26-99be-ee9b1b193d12)(label(Int))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         a39b8782-cf0a-4db9-8d88-d189f832988c)(label(,))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 44))(sort Typ))((shape(Concave \
         44))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         26c64df7-ab91-4413-92a7-6a094027b978)(content(Whitespace\" \
         \"))))(Tile((id \
         e8b5c385-9812-4002-ad1d-b7b79450e48b)(label(MoonPhase))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children()))))))))(Secondary((id \
         bd0f3dd0-4c21-4bf9-a798-50a7c9b2e2c5)(content(Whitespace\" \
         \"))))(Tile((id \
         6899271f-5d15-4108-a2bf-ca24550dd4fa)(label(->))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 13))(sort Typ))((shape(Concave \
         13))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         68410471-3915-43c5-bc31-ab30407d0879)(content(Whitespace\" \
         \"))))(Tile((id \
         d78446a1-6024-4e75-a39d-184201b0c83a)(label(Int))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         4bc9d723-4e79-4289-898b-800c9299de80)(content(Whitespace\" \
         \")))))((Secondary((id \
         aab2eb70-14e8-44dc-9a42-0d09b7500bef)(content(Whitespace\"\\n\"))))(Tile((id \
         b28831b4-6886-49e7-991a-1ce0f0f828a4)(label(fun ->))(mold((out \
         Exp)(in_(Pat))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         37))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         54c392c4-b426-4a85-82f0-baf0c3d5582d)(content(Whitespace\" \
         \"))))(Tile((id \
         f4352ca3-20b4-4f3b-9c08-79bf43e73d50)(label(\"(\"\")\"))(mold((out \
         Pat)(in_(Pat))(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0 1))(children(((Tile((id \
         577c5823-6e90-4930-8c70-458327c0a5a7)(label(base))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         b93f4f47-2554-4502-aa51-e3b36421c94f)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 44))(sort Pat))((shape(Concave \
         44))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         478c02bb-b160-4cd7-bcdc-8784d746558d)(content(Whitespace\" \
         \"))))(Tile((id \
         8c1f9aac-5dd7-437a-bf91-995808843b45)(label(shade))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         134d6013-bf48-4cd3-8a1f-8ba9fa209f10)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 44))(sort Pat))((shape(Concave \
         44))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         286c64da-796c-4ed0-b9c8-8c53fe18752d)(content(Whitespace\" \
         \"))))(Tile((id \
         58264c2e-9a29-4b35-ba96-0b73ba4c160c)(label(phase))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children()))))))))(Secondary((id \
         3f1a62d3-19ae-47a1-a587-2c8ad6a3b404)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         e31fb5e3-043b-4824-87c8-6e5f7738279c)(content(Whitespace\"\\n\"))))(Tile((id \
         5d5507d0-94a2-4e9e-a370-38e675edb74f)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         b699a73c-6f39-413c-ae53-50237d342edd)(content(Whitespace\" \
         \"))))(Tile((id \
         b15da2f6-c4c0-4045-864c-8638786d7640)(label(base_f))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         7cd5848f-2b56-44ca-bc17-81c1d468e035)(content(Whitespace\" \
         \")))))((Secondary((id \
         deac20cb-1f99-4e01-b566-f9f226eae73f)(content(Whitespace\" \
         \"))))(Tile((id \
         949c654d-2e21-4900-ba29-51f94a70f5c1)(label(float_of_int))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         1fb51231-078e-4fc8-8574-5ee4df5e511d)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         ab4e3bc0-2b11-4137-a3db-1e02221ac240)(label(base))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         b7449cf5-6f52-48d2-a130-18e3478c3633)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         c755f972-1372-418e-94a2-a58c33092005)(content(Whitespace\"\\n\"))))(Tile((id \
         dcdb7347-ece6-4c44-9174-18a11e0b35ae)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         4caf4161-4183-45c3-a1a1-e92f93886c3f)(content(Whitespace\" \
         \"))))(Tile((id \
         1c2d2d78-882c-452b-a18b-6f9a2737246d)(label(phase_adj))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         21a80fed-59df-4559-9cfc-3720e042e9f2)(content(Whitespace\" \
         \")))))((Secondary((id \
         db14d5e3-4a5d-42ae-b26b-eddd2eae5a73)(content(Whitespace\" \
         \"))))(Tile((id \
         c04acda7-e6e4-43b0-98a0-edc7fb0b9aab)(label(base_f))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         daa841c4-b918-4447-b9a0-383a6c6b06c7)(content(Whitespace\" \
         \"))))(Tile((id \
         0575fc6d-dfe5-49a9-a13a-9e7990bece4f)(label(*.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 27))(sort Exp))((shape(Concave \
         27))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         848a99c7-3503-4833-8f1d-af893c176852)(content(Whitespace\" \
         \"))))(Tile((id \
         bb1547c3-1a6d-4825-9066-f57bd903633a)(label(phase_multiplier))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         49e84c21-278c-4501-8efc-faf02406e8cf)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         e70f13c6-ca22-4c67-8d77-9c48b9ae312c)(label(phase))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         09fc012b-ffe9-46de-ae82-c260a06627c8)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         dd759ca8-8b1e-40d8-b69d-a7ad705b5df2)(content(Whitespace\"\\n\"))))(Tile((id \
         726f35c8-acef-4bdf-8eb3-4366a8a6a539)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         bfd89db5-ac3b-4542-a13c-7f0c919241da)(content(Whitespace\" \
         \"))))(Tile((id \
         3ab643c7-8594-4a74-800c-99e5a62d66ba)(label(shade_adj))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         8f0bfc4d-2618-4748-8f2d-358f0783bff2)(content(Whitespace\" \
         \")))))((Secondary((id \
         08c45be9-ce8c-4233-8997-4b11be3efbc2)(content(Whitespace\" \
         \"))))(Tile((id \
         1c0dfe87-643a-46fa-8235-f9880137a9fc)(label(phase_adj))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         e874f837-a058-4df7-8431-8cb3f318bdf2)(content(Whitespace\" \
         \"))))(Tile((id \
         bcf8847a-0973-46af-b9f3-0bcfc99cdf81)(label(*.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 27))(sort Exp))((shape(Concave \
         27))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         76e1f138-b1aa-4253-b064-b2512a444caf)(content(Whitespace\" \
         \"))))(Tile((id \
         d8972feb-26ea-429d-9aad-4a2dee6b2cd7)(label(shade_multiplier))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         33587810-7c87-4594-9599-2521bc37003a)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         cf31e5f6-b1b4-4b00-99e3-9e9a5e3199d5)(label(shade))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         5f633692-5516-4f2d-8d84-af82e18e3d1d)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         e1e97540-1c13-4e7f-b47b-31c821b52269)(content(Whitespace\"\\n\"))))(Tile((id \
         722fc7f9-205f-4b98-9945-2eeb2c98a24b)(label(int_of_float))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         5912da59-be73-4ba1-ba09-23f758a76b35)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         fd6ff251-54fa-4f10-a0a2-75b87f1d1f8c)(label(shade_adj))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         e6d94acd-d07d-41aa-a799-f00c8cbb2652)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         ee16a1bf-33be-4268-998a-a116a7d97f4e)(content(Whitespace\"\\n\"))))(Secondary((id \
         97642e06-1a71-4d60-af7a-63845103f812)(content(Whitespace\"\\n\"))))(Tile((id \
         111234cd-9462-4316-94e9-c43b930b79a6)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         34caddf2-e800-4b4a-97e6-d86e687ab081)(content(Whitespace\" \
         \"))))(Tile((id \
         6179240c-cac6-477a-8421-218fc37447fa)(label(daily_water))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         9d48b549-e99a-4b5d-9f47-cbd8afee8d8a)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         ced3726b-eb58-4031-b69c-dc7ef3140ddf)(label(250))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         4faecfea-5837-4fab-bc2c-6c7e5065557c)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         9082b775-f560-4a00-9215-97fdf71fac78)(content(Whitespace\" \
         \"))))(Tile((id \
         f922cc9d-51f4-4d45-8c98-57c205b312ff)(label(2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         c6c576de-e7a0-4570-a76e-d33a408952f2)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         a1006d0f-3204-4386-a763-f4489bb0079a)(content(Whitespace\" \
         \"))))(Tile((id \
         edf11810-9e01-4aa0-8a3c-b9c727b01edf)(label(Full))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         031f5919-30df-49b5-a673-abfd79aeeeea)(content(Whitespace\" \
         \"))))(Tile((id \
         41aac8dc-c98d-4363-8054-ae4826507cbd)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         c8af12d0-6667-4388-8ff1-86eca19fbad9)(content(Whitespace\" \
         \"))))(Tile((id \
         4e240db8-ab5e-48ff-80a6-50441a4717c8)(label(165))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         497cd3fc-8e5b-41d1-a8d6-c7d229101830)(content(Whitespace\" \
         \")))))))))(Tile((id \
         28ab71e7-2d22-4b4d-b5da-dedeaf59498a)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         7f2cffe2-0758-41db-b0cc-9244e887502e)(content(Whitespace\"\\n\"))))(Tile((id \
         216412fb-3283-4650-9320-bb6398ba5d93)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         7e4d4952-60c7-46bc-9c8f-eeff789a3829)(content(Whitespace\" \
         \"))))(Tile((id \
         1ac31d16-1f78-4c8f-afce-6887fa681687)(label(daily_water))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         d6d4747d-5224-40f5-b356-ce86d2994cf2)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         f2db2d9d-c754-406e-bbc8-d59b34960439)(label(50))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         c470cc27-27a6-4463-90f8-f82b88170ac3)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         7112a85b-c3a5-4021-b9d9-2cf57b8a8078)(content(Whitespace\" \
         \"))))(Tile((id \
         c37e3d85-02a2-4e60-87e5-396b1cfba091)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         56929c33-e8b9-4cc6-9cd0-a721c086daf7)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         f6ebcbd8-62d4-4a0d-b1f1-79aeadfbbd8b)(content(Whitespace\" \
         \"))))(Tile((id \
         495545a8-b80f-42c8-be59-c4d2a2d93e67)(label(New))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         b95cfdbb-74ca-4548-82e2-1e662ea1358c)(content(Whitespace\" \
         \"))))(Tile((id \
         0b3ddb1d-35c7-4e71-b7af-406d90374169)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         0a89a968-fdd2-4069-8ec9-e5d7de92a694)(content(Whitespace\" \
         \"))))(Tile((id \
         77b88432-1a41-49a3-a96e-a3115fafe67c)(label(60))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         99be8afb-6984-4d7c-bfbe-2888f0ba823f)(content(Whitespace\" \
         \")))))))))(Tile((id \
         3b0d650a-5638-4c88-93d3-a7f0a0017c19)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         a7318749-e2cf-49e5-920f-b2cf8cad5650)(content(Whitespace\"\\n\"))))(Tile((id \
         ca601593-9aa3-4658-b223-9daf24509bed)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         1039d5db-cb29-43c6-9aa2-55f2c83031fc)(content(Whitespace\" \
         \"))))(Tile((id \
         819b1d69-fb0e-4579-9197-9499aaaefd5a)(label(daily_water))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         547bde04-59be-4fe0-b6bc-f8fae09ff797)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         9caeff49-a744-4765-8f10-5e5e10443972)(label(180))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         e8e919ab-0b7d-4e4a-bc78-c351982c1514)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         8380d06e-bc4b-47d8-bccf-ece5b0b529f6)(content(Whitespace\" \
         \"))))(Tile((id \
         25eeb748-c233-434f-b678-2d932ddd2875)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         face1e53-2119-404f-a066-d74d290fec1d)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         f36dc97f-6774-44be-a60c-b18aa347b2b2)(content(Whitespace\" \
         \"))))(Tile((id \
         8c1d7b9b-e3b2-4e33-baf2-afb44b7912b4)(label(Waning))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         15c48654-d42d-45f6-8f20-8321d2ca755e)(content(Whitespace\" \
         \"))))(Tile((id \
         70962d40-76ac-422a-b14d-c109bd711eb2)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         2c14aea4-b64e-4172-a36e-a33bf1567e80)(content(Whitespace\" \
         \"))))(Tile((id \
         75797e9a-ff34-4bc1-afcd-94d81cf1998b)(label(171))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         47a1abf3-0e12-4d83-960b-11e5057ea0cb)(content(Whitespace\" \
         \")))))))))(Tile((id \
         c98d41f6-661b-4072-956f-03cd9951c437)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         dec67505-19e4-4b8f-9def-1d9295208407)(content(Whitespace\"\\n\"))))(Tile((id \
         b3fe7313-f556-42ef-a5a8-de661e7aeb16)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         8edd1fd2-d3c1-4a07-83ab-36b49d47d762)(content(Whitespace\" \
         \"))))(Tile((id \
         64eb8d15-67f5-4298-a40f-caa66efb1e93)(label(daily_water))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         98b97194-59b0-42b4-a935-af103232b717)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         ab1d741b-fd12-4fd8-ab41-cae9be8d8bdd)(label(200))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         21fba9e7-de05-48cf-ad2d-9df1efa0e09f)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         67b47b46-d8df-4ecd-9555-3801d7d62467)(content(Whitespace\" \
         \"))))(Tile((id \
         d5364263-5eb1-4651-a23c-151376e150e7)(label(2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         7ad87944-595d-4e19-b678-215bef151128)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         08cf2ff6-3d51-4cad-8554-1211af0d11bd)(content(Whitespace\" \
         \"))))(Tile((id \
         c3a63e98-13b4-4fdb-b069-bf28daa2cfa7)(label(Waxing))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         9083e8aa-ff04-4c37-abb6-db65711060f2)(content(Whitespace\" \
         \"))))(Tile((id \
         33c7fd20-52cc-4224-8518-8737f15feb43)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         642a9638-9866-46eb-a879-a92bb2a57934)(content(Whitespace\" \
         \"))))(Tile((id \
         64eedcef-127b-4d3a-b42f-0a6864172bd1)(label(165))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         3a567a09-6dc1-476b-b781-6429bd5e8739)(content(Whitespace\" \
         \")))))))))(Tile((id \
         e85c5aa3-ebd7-4531-9d3c-405bc54cf56d)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         d8dbfc71-b25b-4170-ab2d-3b36312f9e29)(content(Whitespace\"\\n\"))))(Secondary((id \
         ab290c79-0f07-4102-a72e-5c29292b2d9c)(content(Whitespace\"\\n\"))))(Tile((id \
         0c857320-bbd2-4639-8bb7-627f6d0c5da4)(label(daily_water))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         b39b6a08-1731-47aa-a6cd-81747f66ea5c)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         9dd23c59-949c-438c-ba9c-7d1389c00640)(label(100))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         8a91c85d-0f3a-4dd3-9bd5-c78be1f897a7)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         c600f435-b768-4cc3-b56f-12acbc9fc2e6)(content(Whitespace\" \
         \"))))(Tile((id \
         6b173d13-a9c7-4e08-bdb7-2ef3fc9b996e)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         bb66aa54-1c2f-4719-9390-cb748003c1d7)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         b4c70fb8-d488-42eb-8b29-985568987373)(content(Whitespace\" \
         \"))))(Tile((id \
         5d733ff9-397e-4a9c-9dd3-ed69116ab9f8)(label(Full))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         19f584ad-91f3-4e56-9e68-6b640ceeae40)(content(Whitespace\"\\n\"))))(Secondary((id \
         5b9481f1-bfb9-4fad-b02c-24020c45e179)(content(Whitespace\"\\n\"))))(Secondary((id \
         249902d9-6552-4901-bf7b-f139a028db4a)(content(Comment\"# END OF PART \
         5 - Select the next slide from the top menu #\"))))(Secondary((id \
         f9386fb1-05c6-4d38-a9bb-ed68640bf3c5)(content(Whitespace\"\\n\")))))";
      backup_text =
        "# PROBES TUTORIAL - PART 5: STEP INTO AND THE DYNAMIC CURSOR BAR #\n\
         # You've pinned a call and can see values inside a function. #\n\
         # But what if the bug is deeper, inside a function that your #\n\
         # function calls? Step Into follows the call stack down. #\n\n\
         # TRY THIS: #\n\
         # 1. Turn on auto-probe and click inside `daily_water` #\n\
         # 2. Pin one of the test calls (click a sample > Pin) #\n\
         # 3. Now add a probe to the `phase_multiplier(phase)` call #\n\
         #    inside `daily_water` (click on `phase_multiplier`) #\n\
         # 4. Click that sample and choose \"Step Into\" from the #\n\
         #    dropdown (or press Enter) #\n\
         # 5. Your cursor jumps into `phase_multiplier`! The probes #\n\
         #    there show only values from your pinned context. #\n\n\
         # THE DYNAMIC CURSOR BAR #\n\
         # Look at the bar at the top of the editor after stepping in. #\n\
         # It shows your position in the call stack as breadcrumbs: #\n\
         #   top-level > daily_water > phase_multiplier #\n\
         # Click a function name to jump to its definition. #\n\
         # Click a chevron (>) to jump to the call site. #\n\
         # This lets you move up and down the call stack freely. #\n\n\
         type MoonPhase = + New + Waxing + Full + Waning in\n\n\
         let phase_multiplier: MoonPhase -> Float =\n\
         fun phase -> case phase\n\
         | New => 1.2\n\
         | Waxing => 1.1\n\
         | Full => 0.88\n\
         | Waning => 0.95\n\
         end\n\
         in\n\n\
         let shade_multiplier: Int -> Float =\n\
         fun shade -> case shade\n\
         | 0 => 1.0\n\
         | 1 => 0.9\n\
         | 2 => 0.75\n\
         | _ => 0.6\n\
         end\n\
         in\n\n\
         let daily_water: (Int, Int, MoonPhase) -> Int =\n\
         fun (base, shade, phase) ->\n\
         let base_f = float_of_int(base) in\n\
         let phase_adj = base_f *. phase_multiplier(phase) in\n\
         let shade_adj = phase_adj *. shade_multiplier(shade) in\n\
         int_of_float(shade_adj)\n\
         in\n\n\
         test daily_water(250, 2, Full) == 165 end;\n\
         test daily_water(50, 0, New) == 60 end;\n\
         test daily_water(180, 0, Waning) == 171 end;\n\
         test daily_water(200, 2, Waxing) == 165 end;\n\n\
         daily_water(100, 1, Full)\n\n\
         # END OF PART 5 - Select the next slide from the top menu #\n";
      refractors = "()";
    } )
