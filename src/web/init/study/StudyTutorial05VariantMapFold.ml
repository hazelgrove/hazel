let out : string * Haz3lcore.PersistentSegment.t =
  ( "Study / tutorial / 05-variant-map-fold",
    {
      segment =
        "((Secondary((id \
         1e6e1d2f-e654-43d6-b460-1bf1dd86e4b3)(content(Comment\"# PART 5 \
         VARIANT: STEP INTO WITH MAP + FOLD #\"))))(Secondary((id \
         d2acca1c-4554-42bd-ad52-6d8d85b0b386)(content(Whitespace\"\\n\"))))(Secondary((id \
         66411c91-ee87-4f8b-8a8d-16e69ad0849a)(content(Whitespace\"\\n\"))))(Secondary((id \
         f4a0d290-e242-4e85-8c63-66286209b2f2)(content(Comment\"# This \
         function has a two-stage pipeline: map transforms \
         #\"))))(Secondary((id \
         71a75193-e487-4c1c-8e38-094e126bca7f)(content(Whitespace\"\\n\"))))(Secondary((id \
         aa1397b2-23fa-49f2-ae0c-2de51ac83b07)(content(Comment\"# the data, \
         then fold aggregates it. From outside you see #\"))))(Secondary((id \
         dad19f1d-5021-4cd7-bd05-3febd451f9ef)(content(Whitespace\"\\n\"))))(Secondary((id \
         b452aaed-3395-49c5-99e5-7b660d527ef1)(content(Comment\"# one number. \
         Step Into reveals the whole pipeline. #\"))))(Secondary((id \
         d8fd4e2d-9a14-4498-8b9d-f9497dd54bbb)(content(Whitespace\"\\n\"))))(Secondary((id \
         100a838f-9b41-4b5f-a52e-89454aa001cd)(content(Whitespace\"\\n\"))))(Secondary((id \
         e125715e-08e5-46ab-a164-dfe2508e1f22)(content(Comment\"# \
         ============================================================ \
         #\"))))(Secondary((id \
         23fd4808-ed19-4102-bb49-8d62be764122)(content(Whitespace\"\\n\"))))(Secondary((id \
         62c8ce85-d040-4215-9efb-9ab1af2773f1)(content(Whitespace\"\\n\"))))(Tile((id \
         fbd59b27-5e5f-4720-9013-40ee9a0ad5a1)(label(type = in))(mold((out \
         Exp)(in_(TPat Typ))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         4ab2ca25-500d-4ec7-8877-0baaa07934a6)(content(Whitespace\" \
         \"))))(Tile((id \
         33fc5abe-350d-4e93-8dea-01acb9ceab59)(label(Plant))(mold((out \
         TPat)(in_())(nibs(((shape Convex)(sort TPat))((shape Convex)(sort \
         TPat))))))(shards(0))(children())))(Secondary((id \
         c7bccd30-ef32-4d48-b419-25ee3f5afc03)(content(Whitespace\" \
         \")))))((Secondary((id \
         2d9bd700-cc2a-43fc-8164-376f2394f8f9)(content(Whitespace\" \
         \"))))(Tile((id \
         b753a038-0d13-4c31-85b4-1510f0d97efe)(label(\"(\"\")\"))(mold((out \
         Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0 1))(children(((Secondary((id \
         67140736-4b2b-4c53-8a45-8adeb7dda43d)(content(Whitespace\"\\n\"))))(Tile((id \
         1cd9c7b7-e5b8-4ced-a0ad-ffac13756cb2)(label(name))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         ee66ce0d-933d-4054-8ffd-df711fa8178d)(content(Whitespace\" \
         \"))))(Tile((id \
         56727121-b3c8-4394-87e1-53a9e06d395f)(label(=))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 39))(sort Typ))((shape(Concave \
         39))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         a946ddf3-06cc-4280-a2b2-eb6d5d0c4483)(content(Whitespace\" \
         \"))))(Tile((id \
         f2b5c971-5389-48ec-ae1c-e390e6c8b26c)(label(String))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         4806d409-31d7-4839-8471-54233b9c8f93)(label(,))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 44))(sort Typ))((shape(Concave \
         44))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         a4bc6b80-b281-4313-9983-c6b00feba94d)(content(Whitespace\"\\n\"))))(Tile((id \
         c321edb5-cf6c-4a98-8a5e-c217a6d28232)(label(icon))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         4ad32e7c-7877-47eb-96b6-0165feea4fd7)(content(Whitespace\" \
         \"))))(Tile((id \
         1578e91f-81f7-4fc2-a39f-9b8f045128f8)(label(=))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 39))(sort Typ))((shape(Concave \
         39))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         49ae060c-7dc5-4d1c-9abb-216e4f37456f)(content(Whitespace\" \
         \"))))(Tile((id \
         335f73f3-7197-4572-8551-f0eafa936cc3)(label(String))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         567397a0-6493-4b75-afb9-5220555da996)(label(,))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 44))(sort Typ))((shape(Concave \
         44))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         0ccca285-21a0-4656-b89b-948d023c8ce9)(content(Whitespace\"\\n\"))))(Tile((id \
         5b9ec756-136a-4ad5-a0a1-6659c86f3371)(label(water))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         61dd288a-14c6-4b14-a6f6-2ad3253067fa)(content(Whitespace\" \
         \"))))(Tile((id \
         1cb024e8-f707-4664-883a-ac143b2607a6)(label(=))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 39))(sort Typ))((shape(Concave \
         39))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         f7c64cdf-b6aa-4353-9a19-0df5bd0db23b)(content(Whitespace\" \
         \"))))(Tile((id \
         53fc4c8e-232b-444c-8040-086f644af727)(label(Int))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         218baa82-8ea3-4b92-91ff-791245ca4e1c)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         9d778b09-4928-4c07-af3d-9295778346cb)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         b2e42a2e-5e55-4b4b-b166-16203beeac0f)(content(Whitespace\"\\n\"))))(Secondary((id \
         cabb8f90-c9b8-4a43-9d24-4c467f5f7f50)(content(Whitespace\"\\n\"))))(Tile((id \
         c666f769-ed75-4de8-bead-e73f82cf3977)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         98806c63-be83-40fb-80fb-98ce61e23a76)(content(Whitespace\" \
         \"))))(Tile((id \
         7e99b817-dd85-44d7-8200-5d16f79e9361)(label(fern))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         3eab8587-4bcf-427e-a903-c9423a4c2e86)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 24))(sort Pat))((shape(Concave \
         24))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         a44be980-dbdd-47bf-a841-2dcb1b1d21aa)(content(Whitespace\" \
         \"))))(Tile((id \
         0c8cab17-b325-4be3-b4c4-a4a94fd3c36d)(label(Plant))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         72852b0b-6cb8-4560-aa2b-b0b7f9b3e0a0)(content(Whitespace\" \
         \")))))((Secondary((id \
         def334bb-24a3-4833-806c-c4079ebb095c)(content(Whitespace\" \
         \"))))(Tile((id \
         1ade57ba-6521-458d-b2f7-8dd32779490d)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         cd55181a-f1b3-492f-a876-44baa9f19047)(label(name))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         40efdacf-af00-45c4-b718-6ab6d9ea59ca)(content(Whitespace\" \
         \"))))(Tile((id \
         bd57ffff-7daa-4b71-a80d-806615c72aa6)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         49de2405-63a5-47b8-bf6b-970c59e78bad)(content(Whitespace\" \
         \"))))(Tile((id \
         d6aa82b1-cf9b-4210-8426-452c10f62756)(label(\"\\\"Fern\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         21d686dc-c20c-4bb1-a333-e8ca56dfc024)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         25e40423-1180-431b-85b8-3e51f518694b)(content(Whitespace\" \
         \"))))(Tile((id \
         6d4a7125-f9bd-4320-a32a-af96f6d77540)(label(icon))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         b4476c71-c259-494f-8296-ac28eb35c098)(content(Whitespace\" \
         \"))))(Tile((id \
         5252a321-b9fc-435a-915d-396abc72b06e)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         2011443a-9ab4-40a1-b777-ae7f7d25cb50)(content(Whitespace\" \
         \"))))(Tile((id \
         59dad7d8-9848-4c00-b433-311f3f9cdc2c)(label(\"\\\"\\240\\159\\140\\191\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         f357f8d8-78e2-4d28-90fc-49d735d84abf)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         a471cd43-a287-40dd-a72c-abc1c8d57bdb)(content(Whitespace\" \
         \"))))(Tile((id \
         6a0f492f-6794-403e-97cb-02f3a1538704)(label(water))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         f7a514e1-4af9-44d9-9f6e-93169531b2bf)(content(Whitespace\" \
         \"))))(Tile((id \
         fa16ce24-cf1b-4554-8fc5-56e75e3186e9)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         b2320a1c-96c9-4fe7-b758-8a19837f8628)(content(Whitespace\" \
         \"))))(Tile((id \
         72a1299f-920f-4bc7-bbf8-c0ae89bf078d)(label(250))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         34e84b30-8243-45b7-a426-e3b2274b7bbf)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         dddc549d-bfac-4557-8fca-38f3448a8df0)(content(Whitespace\"\\n\"))))(Tile((id \
         cf1dd0ba-4155-4177-8557-b8cbd3b3b9bf)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         eddb8079-648f-44ab-aaf4-f5bb723ee675)(content(Whitespace\" \
         \"))))(Tile((id \
         353fd084-bacf-4d98-b14d-21f82b3113c0)(label(orchid))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         d2eb8fc6-b69f-4db1-b947-4627699dfcbd)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 24))(sort Pat))((shape(Concave \
         24))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         b197cf4d-2c9c-4b53-89f0-bfc5254be4cb)(content(Whitespace\" \
         \"))))(Tile((id \
         cc81b54e-9838-401c-89f5-95313099155e)(label(Plant))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         b94e1405-9f15-49d5-aebd-91bfa1f6efac)(content(Whitespace\" \
         \")))))((Secondary((id \
         fd4b6699-d1a3-4384-9efb-d75fbd438344)(content(Whitespace\" \
         \"))))(Tile((id \
         9bc0b108-b081-4ca4-ab7a-d1f715cc6500)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         c7b22055-aae4-4d8d-acf4-853a98c644e4)(label(name))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         917db97a-edb3-476f-9fa1-f7dd66b26de7)(content(Whitespace\" \
         \"))))(Tile((id \
         777cf64c-2f56-43cd-8878-0f26bc6afc12)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         aba57ef6-d06f-4a55-9dbb-6a5002ba998e)(content(Whitespace\" \
         \"))))(Tile((id \
         674bbf66-c268-439b-9f14-ee71d17a0d65)(label(\"\\\"Orchid\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         8b70ffb8-7d77-4732-b0e8-a0a79d5d560a)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         03e835d4-80d7-42e8-bf55-0308dd7b6f2f)(content(Whitespace\" \
         \"))))(Tile((id \
         672060f5-1dd4-462a-9ac2-e0f4fa3feff8)(label(icon))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         7bea06c0-a7ee-45c6-87c5-f767c8cfe7c1)(content(Whitespace\" \
         \"))))(Tile((id \
         57f14008-ebc4-463a-8bf3-29763333af05)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         99be933b-dde7-49d7-a386-3b4c8b3ad205)(content(Whitespace\" \
         \"))))(Tile((id \
         28cde412-4694-444b-b6bd-788dcff29b6d)(label(\"\\\"\\240\\159\\140\\184\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         4065c60c-5a49-45f9-8ebb-58a2d7a983d0)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         9c94bcc2-be76-47fc-8d3d-1894a31b1c40)(content(Whitespace\" \
         \"))))(Tile((id \
         bbcf805c-cf8e-4c35-973c-506ac66fb7fe)(label(water))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         f38aff2b-e553-429c-b4e7-460741a9bd8d)(content(Whitespace\" \
         \"))))(Tile((id \
         e7d42b18-23e2-4485-bb46-7ac16a3a765d)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         6d7ac1fe-4e7e-49c6-b9e8-35996b9231b6)(content(Whitespace\" \
         \"))))(Tile((id \
         b1be235a-b5c9-4550-a6d4-013d3bd18bc1)(label(180))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         aa490565-ee7c-4adf-b30c-c4a12d4c2cfc)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         dda36294-bb30-40f7-9f02-1378d64df660)(content(Whitespace\"\\n\"))))(Tile((id \
         62976309-ecbd-479f-9f69-3cb1133310e6)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         34e8a81e-1dac-4f09-87c8-16cba31fdbbf)(content(Whitespace\" \
         \"))))(Tile((id \
         dd10660a-5a57-4ad3-ab93-25bba6f4f1b6)(label(cactus))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         a0556c3b-54f2-46ec-911e-10d90d50eac3)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 24))(sort Pat))((shape(Concave \
         24))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         426a4af7-3292-4a2b-9c6b-00c22b9054d8)(content(Whitespace\" \
         \"))))(Tile((id \
         0b252d56-f82c-411f-bc15-d2c31c9324ed)(label(Plant))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         a60f6df2-0069-4e42-bbca-0794b8eef1ef)(content(Whitespace\" \
         \")))))((Secondary((id \
         4893ca8e-600c-4d9f-8e55-78d5f737c41a)(content(Whitespace\" \
         \"))))(Tile((id \
         772159bc-7e36-4b6e-ac53-cd5e5d4b559f)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         3689fb99-0594-46dc-b3de-099b6d077927)(label(name))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         f8798266-8236-4937-8beb-518791227270)(content(Whitespace\" \
         \"))))(Tile((id \
         12a3e7cc-baf7-4ef2-9c12-cdfd1b59ffa6)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         664b233e-57c2-47d9-8858-1364a143b544)(content(Whitespace\" \
         \"))))(Tile((id \
         9588af72-3838-4c2d-9d5a-593593f8f86f)(label(\"\\\"Cactus\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         82dab226-df14-481b-af53-9203f50245a2)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         cd40b4cc-14ba-4103-a2fb-289ee9a482ec)(content(Whitespace\" \
         \"))))(Tile((id \
         bfb126db-6907-445e-b2c5-5a034f95af52)(label(icon))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         938fc842-2a83-4267-9294-01be83308a30)(content(Whitespace\" \
         \"))))(Tile((id \
         88353583-bc8a-4b2e-89c5-47f31b7f940a)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         37d8634d-0cba-44c7-bdf8-6b147a47558c)(content(Whitespace\" \
         \"))))(Tile((id \
         67d18b47-9dc2-4747-98e3-efde3c9c858b)(label(\"\\\"\\240\\159\\141\\132\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         2989ab19-de12-4927-8062-6f8da48d15a4)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         391b265e-35b8-4f4c-92a5-b146f68d8e54)(content(Whitespace\" \
         \"))))(Tile((id \
         06bf2792-adcb-4ca4-bb6f-8a2d8f61398b)(label(water))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         9f79c9a9-cbe1-40de-8270-7cca3e8bb7a8)(content(Whitespace\" \
         \"))))(Tile((id \
         177e5099-dc7f-4427-815b-ea318c2b3e6c)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         550e931a-2c0b-45a5-a217-c00ef3a48eb5)(content(Whitespace\" \
         \"))))(Tile((id \
         c347b068-c780-4653-a430-8c399f0def06)(label(50))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         d2f4eaba-9d72-4fda-946e-291384f56956)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         b2402d0e-b894-4f25-b714-2b8d4e442882)(content(Whitespace\"\\n\"))))(Tile((id \
         0d82d611-72dd-437e-bd2b-d250c882a243)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         6f1c6f7e-c4c5-439b-84e4-dfae1a08805f)(content(Whitespace\" \
         \"))))(Tile((id \
         3c7184c5-814a-4476-9984-b47de5c8f96f)(label(lily))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         170cc158-1408-4b51-b3ea-8fcf55fd9b26)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 24))(sort Pat))((shape(Concave \
         24))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         f1f32c9c-e948-4448-a727-82917ebd3aaf)(content(Whitespace\" \
         \"))))(Tile((id \
         8adf9876-6786-4ced-8cd6-db2c0977e89f)(label(Plant))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         49e58484-f3ef-40e6-a6d9-2aad250b7ef2)(content(Whitespace\" \
         \")))))((Secondary((id \
         d5af06fc-b855-4f49-b725-549cbf853a41)(content(Whitespace\" \
         \"))))(Tile((id \
         0a1724c3-cd08-4acc-9d7f-df7929cabf34)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         ce53fb89-0ea0-49a4-b99a-e2d147632aee)(label(name))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         42537086-5434-4a92-85c9-94a27df6345a)(content(Whitespace\" \
         \"))))(Tile((id \
         05f3b155-4c06-4881-9e6f-6c20b165a341)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         b81c723f-7ee4-4e97-ae38-4c88d5188be6)(content(Whitespace\" \
         \"))))(Tile((id \
         cf01cbda-dfd2-41f4-9754-69704f082c05)(label(\"\\\"Lily\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         2b54a03f-7114-46df-b03d-f92a37c5f082)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         017b9da7-73f2-4a40-a3c7-ea892e4eb78d)(content(Whitespace\" \
         \"))))(Tile((id \
         e5c2be13-6c38-4fca-88d0-17afd99b1a52)(label(icon))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         740bff7c-5a70-4355-9832-d521cbcc1869)(content(Whitespace\" \
         \"))))(Tile((id \
         d975135a-b00f-4287-b6ca-9b4e330f0bcc)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         0f9a81d2-0712-4d7c-b8ac-6c7c26867ced)(content(Whitespace\" \
         \"))))(Tile((id \
         c8f182a6-7820-4729-adbb-05111ef2eb77)(label(\"\\\"\\226\\152\\152\\239\\184\\143\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         cbab3a1a-41d6-47d1-808d-d692d2006344)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         5476ca60-01a0-4f35-8f41-504ddff03b10)(content(Whitespace\" \
         \"))))(Tile((id \
         6fbe1b86-a6df-41af-b2c8-b69c78158859)(label(water))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         b1c7ef1e-1eaa-4a74-8121-254a15d83050)(content(Whitespace\" \
         \"))))(Tile((id \
         2c177d0f-7317-41b1-bb9f-c38a8efbda4c)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         09bb2ac9-b72f-4d4f-99cc-d32110284986)(content(Whitespace\" \
         \"))))(Tile((id \
         da0c1dc1-8ee6-4d95-a45f-644305c8a94e)(label(200))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         d50ce2b1-6aae-4fd2-85b9-06fc955b7898)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         08ef022f-6e1d-487c-9bea-2bd0bd763f0c)(content(Whitespace\"\\n\"))))(Tile((id \
         05595cfb-d91c-49a0-a2e1-43df82066479)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         51050a73-0d58-49a8-86f3-979c330c021a)(content(Whitespace\" \
         \"))))(Tile((id \
         c7c1c022-2499-4da8-a268-a15e7a5cc8ca)(label(daisy))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         1f847a75-e62c-4e03-bb60-4aa75af68bda)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 24))(sort Pat))((shape(Concave \
         24))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         0fdce821-4f19-49f8-a3ec-0d8aaffdc621)(content(Whitespace\" \
         \"))))(Tile((id \
         a3cf871e-457b-4922-ab37-70f5e9704334)(label(Plant))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         18fc73c2-a021-4f58-b6b6-856f4571bc51)(content(Whitespace\" \
         \")))))((Secondary((id \
         8514ae85-42e2-4b22-b696-c2246c7b0002)(content(Whitespace\" \
         \"))))(Tile((id \
         6c90a110-92ae-4823-b9af-f4a07fa5335f)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         d877a834-29b4-4967-97bf-992d261cdbf1)(label(name))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         f8d00396-d6a9-4865-bb39-ce0d38add7f3)(content(Whitespace\" \
         \"))))(Tile((id \
         9076fdd8-1da0-4b10-a848-3fff64ad9a89)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         a2f87a3d-bd81-41db-a4a7-f2166e16d4aa)(content(Whitespace\" \
         \"))))(Tile((id \
         f66820ee-9e3a-4bc2-9b4c-0694b92b0012)(label(\"\\\"Daisy\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         8f09e346-2476-4413-8f2b-b00262cdeb5b)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         514dd65c-7688-44de-ab31-7272d0e8127c)(content(Whitespace\" \
         \"))))(Tile((id \
         459716e4-f192-4789-9126-ae7868495b38)(label(icon))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         4098ce8e-8f9d-4ee8-b045-0a6799414a0e)(content(Whitespace\" \
         \"))))(Tile((id \
         597b9c90-1e6c-4a9b-ba08-84656b373c2b)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         a73369cb-6058-4c6e-b6a0-87b8619a6cf5)(content(Whitespace\" \
         \"))))(Tile((id \
         69028326-10d3-4338-93bb-86fa3ae30244)(label(\"\\\"\\240\\159\\140\\177\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         fe1774ef-bc97-4d59-a972-dbb7710bc004)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         5576e62a-d34c-4fc2-93a6-59163f7926c2)(content(Whitespace\" \
         \"))))(Tile((id \
         21564978-4c46-403b-a5e3-953b83b0cd23)(label(water))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         7f60007f-46e8-4bec-9583-547ace9b64d6)(content(Whitespace\" \
         \"))))(Tile((id \
         11572f07-5098-4872-855a-e80409106bbb)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         e87af4b5-809b-4959-a372-4fc94a2456cc)(content(Whitespace\" \
         \"))))(Tile((id \
         120865dc-ef65-4bdc-bfbe-f073d63d5ddc)(label(160))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         46d05952-3a7c-4f00-a78a-f164af4b4b9a)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         0452deb9-3b52-4a8f-84ca-33a8c549ecbb)(content(Whitespace\"\\n\"))))(Secondary((id \
         d907b892-f2d0-4cf6-9b0a-84614e170d30)(content(Whitespace\"\\n\"))))(Secondary((id \
         f91759df-0958-4a9e-9c70-a34b80edeaeb)(content(Comment\"# weekly_total \
         computes the total weekly water for a garden. #\"))))(Secondary((id \
         d9647e2a-5461-48ea-a28b-bcdb88b80a27)(content(Whitespace\"\\n\"))))(Secondary((id \
         b983ad35-a5e0-4c79-a8d2-700c0a2fab92)(content(Comment\"# First it \
         maps each plant's daily water to weekly (x7), #\"))))(Secondary((id \
         32fe695f-ade5-4d6e-9de8-7431e7cef2ec)(content(Whitespace\"\\n\"))))(Secondary((id \
         28f7e54d-4919-45c1-9330-77c28ea34695)(content(Comment\"# then folds \
         to sum everything up. #\"))))(Secondary((id \
         e1cf006c-d593-4aca-bb26-28b9bb487230)(content(Whitespace\"\\n\"))))(Secondary((id \
         4ccb92b2-4fe7-4462-8260-601f5a55e85a)(content(Whitespace\"\\n\"))))(Tile((id \
         6bd93982-7242-4e9c-9e9a-f048cbca99b7)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         7f68ca61-bf29-41ca-98db-26afc51c828c)(content(Whitespace\" \
         \"))))(Tile((id \
         d4754b71-ed51-4207-b0a6-960425532bc3)(label(weekly_total))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         8c5e2657-116f-4d6d-96ab-da5f8cd9a85e)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 24))(sort Pat))((shape(Concave \
         24))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         1104e909-22f3-46f6-abae-6367da671fde)(content(Whitespace\" \
         \"))))(Tile((id 56161081-f7e2-4680-8e9e-c780c350174d)(label([ \
         ]))(mold((out Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape \
         Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
         6b58244e-dc89-455d-889e-f69d08d0a97b)(label(Plant))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children()))))))))(Secondary((id \
         ab306294-0fb7-4bb3-be19-273bcedd9679)(content(Whitespace\" \
         \"))))(Tile((id \
         3712122b-9727-42c0-8954-c7e217f2c3bc)(label(->))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 13))(sort Typ))((shape(Concave \
         13))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         4a7d10a1-39e2-4464-939e-809b05567486)(content(Whitespace\" \
         \"))))(Tile((id \
         07156335-b552-4a69-a3da-e4637ec24068)(label(Int))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         61baa9b5-1065-44a1-baeb-f6a247bbbdb3)(content(Whitespace\" \
         \")))))((Secondary((id \
         49781dda-1fe4-4d5d-b9b9-1ec06a6755bb)(content(Whitespace\"\\n\"))))(Tile((id \
         d24273e6-aaf7-4a9a-bf11-e929d946ac9f)(label(fun ->))(mold((out \
         Exp)(in_(Pat))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         37))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         b4715df9-5296-4366-9e8f-fcda3c681272)(content(Whitespace\" \
         \"))))(Tile((id \
         22cc7194-4e85-4e34-8537-39975bfce8ad)(label(plants))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         91c92b0c-ceb4-4f60-bf19-5df35cd7d66e)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         9d7e5c35-ec4a-4d8c-9497-b38ee3afdf42)(content(Whitespace\"\\n\"))))(Tile((id \
         e3b00d16-edfd-478b-8c1d-69d7ed78135c)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         cdbb3099-18e2-4c27-9bf1-e003648d703e)(content(Whitespace\" \
         \"))))(Tile((id \
         058dec6a-a211-4d25-974d-7cddbf936129)(label(weekly_amounts))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         67463635-39aa-4c96-8aef-cba8812f179a)(content(Whitespace\" \
         \")))))((Secondary((id \
         a6e34f70-10f7-40d8-8646-9bce01361081)(content(Whitespace\" \
         \"))))(Tile((id \
         0f05a32b-72cc-4531-9bc0-9c9ecbb43231)(label(map))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         a94edaa7-f967-4d03-972f-cb7560c2381c)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         8671ab74-4da7-4d1b-bc6c-bcd269be700c)(label(plants))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         f541c942-f311-474e-ac07-4d5e13991a60)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         cae00eae-b8a3-41fc-9d30-74a79e94d436)(content(Whitespace\" \
         \"))))(Tile((id f2a3d0fd-137a-43bc-82ac-18dc4504bd40)(label(fun \
         ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 37))(sort Exp))))))(shards(0 \
         1))(children(((Secondary((id \
         6671281d-ab9d-4979-b7ed-3fed62b6800a)(content(Whitespace\" \
         \"))))(Tile((id \
         9885fdcc-7c12-45da-a87c-9eb2d8391a8f)(label(plant))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         fe4f4a7a-f24a-45d3-b74f-00da28c2a3df)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         56c0df7c-6727-47c2-baa9-7ee49f2277ed)(content(Whitespace\"\\n\"))))(Tile((id \
         9c388b5b-5df9-4d8f-a537-9c9bafc343f7)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         20ebb2b8-db33-4502-8fe2-f81800832eb0)(content(Whitespace\" \
         \"))))(Tile((id \
         c510a290-cfb3-4132-ac9d-68eef7c2fa3a)(label(daily))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         280c1ea5-ce02-49ad-8a82-5c352ab2c759)(content(Whitespace\" \
         \")))))((Secondary((id \
         c6b4b306-5ee4-48c4-80e4-5091d85d9423)(content(Whitespace\" \
         \"))))(Tile((id \
         5a7d949d-ab1f-4759-baa9-d02c702e8976)(label(plant))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         9a7e4e0b-6a62-4359-ad65-d8e09008f39f)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         fb46d650-5b1c-47d2-8cf2-335801b534d5)(label(water))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         866d0fc5-c411-4b50-b8dd-b29642c4c81f)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         252919cf-7b2e-410e-a1da-66aac4e6d022)(content(Whitespace\"\\n\"))))(Tile((id \
         f58da9c4-062c-459a-a603-5dd01a997d24)(label(daily))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         17101205-125b-40e5-968d-eb91f9812488)(content(Whitespace\" \
         \"))))(Tile((id \
         6344516c-f8f4-4fbd-bf00-fdf4c16146d6)(label(*))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 27))(sort Exp))((shape(Concave \
         27))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         e36cec86-0c11-426f-b336-eb217015b907)(content(Whitespace\" \
         \"))))(Tile((id \
         2cb9decf-9c8f-4ccb-9aa1-dae15a3581e9)(label(7))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         a0f49c60-24db-4bcf-81b0-37694cea5605)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         3cb6e3d5-1440-4344-848b-afb95eecadad)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         63c21ebb-dba3-41c5-b0d9-1fc0cd5d12c7)(content(Whitespace\"\\n\"))))(Tile((id \
         ca223fb1-e830-4be4-90d5-67de2e2cdf23)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         ea379630-a723-4c66-8ff7-a959fda8af67)(content(Whitespace\" \
         \"))))(Tile((id \
         db6139c9-0601-429b-9e12-3bc775714869)(label(sum))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         de790a84-36ef-4c8f-ada7-b388aeb8de32)(content(Whitespace\" \
         \")))))((Secondary((id \
         7f493772-562d-4985-b935-bd4ea7b2f590)(content(Whitespace\" \
         \"))))(Tile((id b103d295-f823-4beb-9af6-e13ed495acf9)(label(fun \
         ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 37))(sort Exp))))))(shards(0 \
         1))(children(((Secondary((id \
         7b772a09-1c0c-40aa-a203-abdfd24f9e07)(content(Whitespace\" \
         \"))))(Tile((id \
         ef6beefb-a02f-48cf-86ff-0bf644307eed)(label(\"(\"\")\"))(mold((out \
         Pat)(in_(Pat))(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0 1))(children(((Tile((id \
         234def32-3bf9-4c38-8626-a9d649c10a14)(label(acc))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         5d9fc30f-8c95-4543-98a9-be6d10a56d49)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 44))(sort Pat))((shape(Concave \
         44))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         618763a4-03f6-4b68-ab72-33d14f82704e)(content(Whitespace\" \
         \"))))(Tile((id \
         d7fa746b-cbae-4367-9371-05abce1b5e2f)(label(w))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children()))))))))(Secondary((id \
         0f899c3a-a89a-4a9a-8971-448c04c109c6)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         3b25f494-3f06-4ca9-8f95-103fe98f8354)(content(Whitespace\"\\n\"))))(Tile((id \
         7443a7da-6090-415d-a8d7-70d65efd517f)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         d12da3eb-265b-4d23-959a-87b2d4af9bbf)(content(Whitespace\" \
         \"))))(Tile((id \
         3e8b3dfd-ae85-410c-b7ce-027171b955ca)(label(running))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         17078742-f0b0-446e-8dfd-914491f3b444)(content(Whitespace\" \
         \")))))((Secondary((id \
         0869165f-1a76-4009-8d98-339ae2e174b3)(content(Whitespace\" \
         \"))))(Tile((id \
         670d94cc-21f4-4fad-8464-2df52d7bbb0c)(label(acc))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         73f1bcd8-f58a-44e9-a425-d689f40c9d61)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         b16e899f-4baf-4b73-b5f0-4c939b4334cb)(content(Whitespace\"\\n\"))))(Tile((id \
         8b61d4aa-07ea-4371-af01-235fec189190)(label(running))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         cfe55ab2-8515-4cf6-97b4-02ced48d9f54)(content(Whitespace\" \
         \"))))(Tile((id \
         d684db6a-1a77-438c-8361-d5f6d2bc2f2d)(label(+))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 28))(sort Exp))((shape(Concave \
         28))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         69b8d21b-c89e-4757-9963-9cc745081d8a)(content(Whitespace\" \
         \"))))(Tile((id \
         c5c601af-b6c9-4fcc-99f1-02aedfdb4ea3)(label(w))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         0eca6f0e-8bab-49dd-95b1-279f93563292)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         3457f6a9-5618-4d6d-94db-6d7e2dba5327)(content(Whitespace\"\\n\"))))(Tile((id \
         f69f947a-d64f-46e3-ae71-7dcb4415cf9e)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         0cfe9b8c-cc63-4cef-acb5-8ddde6011662)(content(Whitespace\" \
         \"))))(Tile((id \
         96ed4d59-552f-4b69-a269-6dd24977ac2b)(label(total))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         7097b69d-4c3b-4a81-9418-198d79e8a05f)(content(Whitespace\" \
         \")))))((Secondary((id \
         0a913de9-6a6d-4fc8-8d47-26a9aee51481)(content(Whitespace\" \
         \"))))(Tile((id \
         a6a57976-f738-4bb0-84fa-e353f16c6b24)(label(fold_left))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         e0bc65ca-4133-4831-b1e0-7dff221213a6)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         d210020f-a5cc-41d6-bc98-841427ecc9c8)(label(weekly_amounts))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         957f9e88-45a4-480b-9e93-64a93479c777)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         0d092484-a28c-4df6-9a93-07e7e1aa8537)(content(Whitespace\" \
         \"))))(Tile((id \
         74be4c49-c4b2-477f-bcf8-3eb4adaab7ba)(label(sum))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         efcd5c29-1a74-4ade-8007-8781420bced0)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         2bf95023-4e21-42fa-a548-da3827a5d8c6)(content(Whitespace\" \
         \"))))(Tile((id \
         a0abda5c-ddfe-48ca-b2a8-cf8556869416)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         3b495a9c-6f07-4c20-ae08-232c3c7fdc32)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         b99eedfa-3f3f-4b87-92f7-11ed34b9b888)(content(Whitespace\"\\n\"))))(Tile((id \
         7e7bbd93-b75e-4e62-ba4b-c541dc440e5c)(label(total))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         9362e879-a158-4227-a47c-418d38eb8680)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         5808e616-34b2-48a7-988c-4ea2b5641f48)(content(Whitespace\"\\n\"))))(Secondary((id \
         b6278e91-7695-40bc-9c4f-981826e122b6)(content(Whitespace\"\\n\"))))(Secondary((id \
         1e2e76d6-ec7b-4b33-ae2a-b4b3e734a995)(content(Comment\"# EXERCISE 1: \
         Step into the map #\"))))(Secondary((id \
         bd47f9fd-01e2-4422-bcce-e452d1b5719b)(content(Whitespace\"\\n\"))))(Secondary((id \
         e8ed13ba-d452-4f17-bc98-f5f11bb56cd7)(content(Comment\"# 1. Add a \
         probe to `weekly_total(shade)` below. #\"))))(Secondary((id \
         655e13b2-18a5-4c8f-b098-5ec8c4f323a9)(content(Whitespace\"\\n\"))))(Secondary((id \
         2a276d30-05d3-42a6-b617-acd7a5f73597)(content(Comment\"#    It \
         returns 4270. How does it get there? #\"))))(Secondary((id \
         d0ffd0ab-a58f-4558-b5c1-23e9911c347d)(content(Whitespace\"\\n\"))))(Secondary((id \
         1ea9e8c8-988e-4add-b4ed-e4ff2ce9c00a)(content(Comment\"# 2. Click the \
         sample and Step Into (Enter). #\"))))(Secondary((id \
         c5aed211-a649-478d-bb3f-8379b34ec616)(content(Whitespace\"\\n\"))))(Secondary((id \
         73bcb3e6-bb49-46bf-99a6-ea98cfcc288c)(content(Comment\"# 3. Turn on \
         auto-probe inside `weekly_total`. #\"))))(Secondary((id \
         984f32ff-9bad-4b1a-8336-70c1e99f05ea)(content(Whitespace\"\\n\"))))(Secondary((id \
         e0bf16fd-8ca9-48b7-b817-88cd275437ad)(content(Comment\"# 4. The map \
         callback shows each plant's `daily` water #\"))))(Secondary((id \
         1777fafb-1078-465d-8dce-aedf337f2198)(content(Whitespace\"\\n\"))))(Secondary((id \
         1eb854ca-3a31-411c-b81e-722a1fa837f3)(content(Comment\"#    and the \
         `daily * 7` result. In Many mode you see #\"))))(Secondary((id \
         9a41ce18-20c6-49c2-8823-5bc3db49ba65)(content(Whitespace\"\\n\"))))(Secondary((id \
         d7fd6a3e-359e-48a6-ab70-75e5973d4141)(content(Comment\"#    all 3 \
         transformations side by side: #\"))))(Secondary((id \
         64add9af-7cef-4099-b9bc-a9cebe030916)(content(Whitespace\"\\n\"))))(Secondary((id \
         1d8eaf98-3aa7-4350-b092-62fc9220b6e5)(content(Comment\"#    daily: \
         [250, 200, 160] and daily*7: [1750, 1400, 1120] #\"))))(Secondary((id \
         232dc570-b9a6-441e-a9dc-bf5ff46bf1d9)(content(Whitespace\"\\n\"))))(Secondary((id \
         c5a22f34-8587-41c2-8ec3-e114ec763735)(content(Whitespace\"\\n\"))))(Secondary((id \
         55dadaf9-6ae4-46f9-aa9b-f7346d26bb89)(content(Comment\"# EXERCISE 2: \
         Now look at the fold #\"))))(Secondary((id \
         c55f882d-311f-4c68-8517-8e016545b335)(content(Whitespace\"\\n\"))))(Secondary((id \
         52960525-fda0-4e7f-8ff1-d443f4d1fe9f)(content(Comment\"# 5. Still \
         inside `weekly_total`, look at the fold #\"))))(Secondary((id \
         050bb36c-150d-4970-b6b2-c93a260d5da2)(content(Whitespace\"\\n\"))))(Secondary((id \
         e6f67dd1-5d25-4a7d-9bca-50b4b3fedf35)(content(Comment\"#    \
         callback's samples. In Many mode, `running` shows \
         #\"))))(Secondary((id \
         a52396b0-47fb-476d-8892-0c2962868e05)(content(Whitespace\"\\n\"))))(Secondary((id \
         e5073d46-f4cd-46da-82a0-c7634cb1c80e)(content(Comment\"#    the \
         accumulator: [0, 1750, 3150] and `running + w` #\"))))(Secondary((id \
         2fe8766b-c33b-4b49-9439-5322b8049940)(content(Whitespace\"\\n\"))))(Secondary((id \
         b93a7edf-2b4a-478d-adbc-43e1ea4a36b8)(content(Comment\"#    shows it \
         growing: [1750, 3150, 4270]. #\"))))(Secondary((id \
         1a979114-8f9d-4db8-89bf-6f1ea1e887e1)(content(Whitespace\"\\n\"))))(Secondary((id \
         80acc3ee-1f91-4821-b708-6593994b99f8)(content(Comment\"# 6. Use the \
         dynamic cursor bar at the top to navigate #\"))))(Secondary((id \
         3e3dab7b-ef9e-427e-aacb-18337c09a507)(content(Whitespace\"\\n\"))))(Secondary((id \
         ef0b565f-271b-4896-8c30-1293ac08ce16)(content(Comment\"#    back out. \
         Try stepping into `weekly_total(all)` \\226\\128\\148 \
         #\"))))(Secondary((id \
         5bb0cb6a-dddd-423e-a2c9-12c725ff84b8)(content(Whitespace\"\\n\"))))(Secondary((id \
         ec5a9518-1839-43ac-b934-7d9e8a3e7b58)(content(Comment\"#    now there \
         are 5 iterations each. #\"))))(Secondary((id \
         3bbb59f6-233f-44c6-87f4-b8794971a147)(content(Whitespace\"\\n\"))))(Secondary((id \
         bd48f95b-02cd-4f64-a3e1-489c6599690d)(content(Whitespace\"\\n\"))))(Tile((id \
         525ac47a-444a-4ed7-a07c-8ac309b5f6f2)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         08b7c63f-8c42-4724-ba48-a50692e17975)(content(Whitespace\" \
         \"))))(Tile((id \
         a5578df8-4984-4bd0-8a2e-647ddb8e7e00)(label(shade))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         6bf77d66-5a59-4baf-8eb5-6b388fc22fde)(content(Whitespace\" \
         \")))))((Secondary((id \
         d0e654a5-66e4-499b-bffe-e2bfdb073fc0)(content(Whitespace\" \
         \"))))(Tile((id 81f6de89-0070-4b53-b371-392f3504b4d4)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         7ab463a7-72e2-4da8-a0b9-f7e81a9f703d)(label(fern))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         4f9ddb78-571d-4b69-a9dd-a91d763e39df)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         82e155b2-b12e-453a-aebd-24b89fbc44b5)(content(Whitespace\" \
         \"))))(Tile((id \
         06ff79e9-b767-4461-827b-d09eaf1f2d7f)(label(lily))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         f73c55fc-574f-4d88-a20c-4bba5da344f5)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         95040957-e1dd-455d-a46e-c8253da70a17)(content(Whitespace\" \
         \"))))(Tile((id \
         eb7622b1-fd16-498d-82c6-371fb3c9caad)(label(daisy))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         ed77b834-9354-4f0b-816d-0dc6fedc2683)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         609e2d93-7966-4d0a-a65a-e9f119bd2631)(content(Whitespace\"\\n\"))))(Tile((id \
         bfd30c03-6e2c-4f25-a5e3-885b75bf444a)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         4da6965d-5492-4564-8872-5ef5d9b0800a)(content(Whitespace\" \
         \"))))(Tile((id \
         163e01a2-9c0e-42d4-8245-d6d51d2aa498)(label(sun))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         a2dcc4ea-e389-4da3-ab3d-21e069ac918a)(content(Whitespace\" \
         \")))))((Secondary((id \
         dff764c0-bd91-4d06-8267-1065da20ed49)(content(Whitespace\" \
         \"))))(Tile((id 907d4cfd-799d-4aa4-a196-5751351dc21a)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         89fba42d-599f-49b9-87ab-5a19c27ba5d0)(label(orchid))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         e3896e63-f2cc-4b27-9e60-752484a47cd5)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         f9ea8546-5381-4f47-8442-8333c3779b2b)(content(Whitespace\" \
         \"))))(Tile((id \
         3477123e-f6cd-4b42-8ba0-a8105a7e61e9)(label(cactus))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         1b6e3fb7-b78d-4d85-a984-de5651be4b5b)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         982eb205-8ac1-4854-a512-032fd10abb4d)(content(Whitespace\"\\n\"))))(Tile((id \
         d0d98d91-f171-4c62-a8ae-200c5750bd86)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         50f1c8ba-fe1f-418d-9435-a57f60c91e8a)(content(Whitespace\" \
         \"))))(Tile((id \
         2d474893-348f-4134-9a92-16b0f9bfe8ba)(label(all))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         8d6c1e8e-9b4f-498c-9097-0a115499f0db)(content(Whitespace\" \
         \")))))((Secondary((id \
         b464e902-a4ec-4ff4-9f5f-9a8bd4b90c5b)(content(Whitespace\" \
         \"))))(Tile((id 14b16567-6a65-485a-953f-39d2a31fdc52)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         26964abd-dfbb-48ae-9c20-07e160fa923b)(label(fern))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         d6d3d57f-8c43-4f97-aa74-2ea220876a86)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         e720c7f0-345b-4035-b5ac-18f675f8de45)(content(Whitespace\" \
         \"))))(Tile((id \
         cbcf8bae-0c51-48b9-9c02-305ad18f7299)(label(orchid))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         cc8c6d1b-3841-433c-93a1-3911fcec35a9)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         a6a51246-a935-4281-81b1-99a9a27d77c5)(content(Whitespace\" \
         \"))))(Tile((id \
         eb702925-f40f-4d28-ac5b-10becb9acfd0)(label(cactus))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         cc0302f2-4b5c-4224-b328-1e32f543fbed)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         3c19227d-89dd-4816-8884-274fd6ad6d3b)(content(Whitespace\" \
         \"))))(Tile((id \
         528c88f6-78a8-41c5-b49a-5ad27bf2d3df)(label(lily))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         ce554a49-87e3-468a-a2fe-f4976ec2cb49)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         53fb051f-e24e-4914-8aa1-8edce2584dc8)(content(Whitespace\" \
         \"))))(Tile((id \
         ebd0aa0d-c4a3-478f-9a5a-d9492e0584cd)(label(daisy))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         fa4eab9f-ff15-4ccb-870a-7526ad989844)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         f1acad51-0ce8-413d-bae8-6540bdc3f2ba)(content(Whitespace\"\\n\"))))(Secondary((id \
         eef72df2-3573-43a1-80f5-988104b47e86)(content(Whitespace\"\\n\"))))(Tile((id \
         f97c9c0c-4b35-481b-af1b-968dc918ac7c)(label(weekly_total))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         641e086b-f203-4cfb-a523-205c5a812321)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         a427fae3-1893-4883-b94a-19f3bf4b42a2)(label(shade))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         87ad815b-43ff-4a72-a257-9f86bf2e7657)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         cdca7286-c86b-4c8c-a983-f3fd1161f625)(content(Whitespace\"\\n\"))))(Tile((id \
         535c7f31-691f-4d57-baa1-938b5da45dfd)(label(weekly_total))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         d7ca35dc-3f5d-456b-b310-61d4f426e948)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         3dfd8efc-75fc-490e-938d-741f5d9e222e)(label(sun))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         d57ad8cb-a3f6-43dc-aa32-e66d18dac0a6)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         2c8676b2-b5a1-467b-b416-a051ce017cb4)(content(Whitespace\"\\n\"))))(Tile((id \
         7063a888-3090-4fdb-b0bf-a2af19f2d012)(label(weekly_total))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         5cec6196-cf2e-459a-afa1-1a0e3ca6cd1c)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         b8c48f38-e133-43b9-8ea6-665afb198c52)(label(all))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         70c39022-f4b2-47fc-8856-8ff71b1070bc)(content(Whitespace\"\\n\"))))(Secondary((id \
         a1674098-3343-4a5a-ac2a-4cb051e95be6)(content(Whitespace\"\\n\"))))(Secondary((id \
         986a831d-99f9-496d-8448-f6ffce09ecda)(content(Comment\"# END \
         #\"))))(Secondary((id \
         d94421a3-4a7b-4c12-9764-56805eb25b32)(content(Whitespace\"\\n\")))))";
      backup_text =
        "# PART 5 VARIANT: STEP INTO WITH MAP + FOLD #\n\n\
         # This function has a two-stage pipeline: map transforms #\n\
         # the data, then fold aggregates it. From outside you see #\n\
         # one number. Step Into reveals the whole pipeline. #\n\n\
         # ============================================================ #\n\n\
         type Plant = (\n\
         name = String,\n\
         icon = String,\n\
         water = Int\n\
         ) in\n\n\
         let fern: Plant = (name = \"Fern\", icon = \"\240\159\140\191\", \
         water = 250) in\n\
         let orchid: Plant = (name = \"Orchid\", icon = \"\240\159\140\184\", \
         water = 180) in\n\
         let cactus: Plant = (name = \"Cactus\", icon = \"\240\159\141\132\", \
         water = 50) in\n\
         let lily: Plant = (name = \"Lily\", icon = \
         \"\226\152\152\239\184\143\", water = 200) in\n\
         let daisy: Plant = (name = \"Daisy\", icon = \"\240\159\140\177\", \
         water = 160) in\n\n\
         # weekly_total computes the total weekly water for a garden. #\n\
         # First it maps each plant's daily water to weekly (x7), #\n\
         # then folds to sum everything up. #\n\n\
         let weekly_total: [Plant] -> Int =\n\
         fun plants ->\n\
         let weekly_amounts = map(plants, fun plant ->\n\
         let daily = plant.water in\n\
         daily * 7\n\
         ) in\n\
         let sum = fun (acc, w) ->\n\
         let running = acc in\n\
         running + w\n\
         in\n\
         let total = fold_left(weekly_amounts, sum, 0) in\n\
         total\n\
         in\n\n\
         # EXERCISE 1: Step into the map #\n\
         # 1. Add a probe to `weekly_total(shade)` below. #\n\
         #    It returns 4270. How does it get there? #\n\
         # 2. Click the sample and Step Into (Enter). #\n\
         # 3. Turn on auto-probe inside `weekly_total`. #\n\
         # 4. The map callback shows each plant's `daily` water #\n\
         #    and the `daily * 7` result. In Many mode you see #\n\
         #    all 3 transformations side by side: #\n\
         #    daily: [250, 200, 160] and daily*7: [1750, 1400, 1120] #\n\n\
         # EXERCISE 2: Now look at the fold #\n\
         # 5. Still inside `weekly_total`, look at the fold #\n\
         #    callback's samples. In Many mode, `running` shows #\n\
         #    the accumulator: [0, 1750, 3150] and `running + w` #\n\
         #    shows it growing: [1750, 3150, 4270]. #\n\
         # 6. Use the dynamic cursor bar at the top to navigate #\n\
         #    back out. Try stepping into `weekly_total(all)` \226\128\148 #\n\
         #    now there are 5 iterations each. #\n\n\
         let shade = [fern, lily, daisy] in\n\
         let sun = [orchid, cactus] in\n\
         let all = [fern, orchid, cactus, lily, daisy] in\n\n\
         weekly_total(shade);\n\
         weekly_total(sun);\n\
         weekly_total(all)\n\n\
         # END #\n";
      refractors = "()";
    } )
