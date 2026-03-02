let out : string * Haz3lcore.PersistentSegment.t =
  ( "Study / debugging / soil-plotter / soil-plotter-bug-scaffold",
    {
      segment =
        "((Secondary((id \
         6be9e774-cb80-41a8-9fdb-24481d4a4c1b)(content(Comment\"# Crop Plotter \
         with Soil Types #\"))))(Secondary((id \
         bd3e5372-0acc-431a-9f1e-c9411ca29834)(content(Whitespace\"\\n\"))))(Secondary((id \
         d55fde8d-af49-42d9-8bdf-0bf2f623bd57)(content(Comment\"# Each cell \
         has a crop and a soil type #\"))))(Secondary((id \
         44dde16c-7888-4584-ad35-01fb6b29e1fa)(content(Whitespace\"\\n\"))))(Secondary((id \
         5370db96-3765-4de7-a1a5-3e3cd53070d4)(content(Whitespace\"\\n\"))))(Tile((id \
         918d642d-26ef-49a8-b918-fa43d493453b)(label(type = in))(mold((out \
         Exp)(in_(TPat Typ))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         68afe1aa-41a8-408a-9078-6e9b2e25f45c)(content(Whitespace\" \
         \"))))(Tile((id \
         1921e2c0-f656-40e4-988e-000492ef259f)(label(Crop))(mold((out \
         TPat)(in_())(nibs(((shape Convex)(sort TPat))((shape Convex)(sort \
         TPat))))))(shards(0))(children())))(Secondary((id \
         3becea9b-d24b-469c-829d-be46d0d9e17a)(content(Whitespace\" \
         \")))))((Secondary((id \
         698937fa-7797-4348-b23b-a6451379916e)(content(Whitespace\" \
         \"))))(Tile((id \
         c2bda5b1-fac3-482c-975f-38d7dd4bc051)(label(String))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         b5b65360-fe00-48ba-aa00-28c42b9a1ca3)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         f79efb49-c7d5-4419-bbf3-5075a2a41347)(content(Whitespace\" \
         \"))))(Secondary((id \
         a4e1a695-b1f7-4ca1-9641-e8976fab89f2)(content(Whitespace\" \
         \"))))(Secondary((id \
         062a3ae5-f32c-4b8d-8670-6ee36475f6f8)(content(Comment\"# Plant emoji \
         or \\\"\\\" for empty #\"))))(Secondary((id \
         4aa81b83-2c74-400e-b4ff-fd01a0889656)(content(Whitespace\"\\n\"))))(Tile((id \
         019ff09d-3733-47cb-870f-a6711bc1d9f0)(label(type = in))(mold((out \
         Exp)(in_(TPat Typ))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         bd572c58-ee9d-47c4-a22f-adb82cc35c5e)(content(Whitespace\" \
         \"))))(Tile((id \
         e3031ba6-f921-4d50-8b6a-9d1550c782f2)(label(Row))(mold((out \
         TPat)(in_())(nibs(((shape Convex)(sort TPat))((shape Convex)(sort \
         TPat))))))(shards(0))(children())))(Secondary((id \
         c2f0e787-b6d6-4caf-bdfd-525ce3343701)(content(Whitespace\" \
         \")))))((Secondary((id \
         996c644a-19eb-47df-89e4-cae726173882)(content(Whitespace\" \
         \"))))(Tile((id \
         94dc9caa-b627-4642-a22e-fdce59713a7a)(label(Int))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         5d80a58b-382e-4d04-a4cd-b091dc84e099)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         eb41c0c9-3385-4d88-ad74-903dd8af572c)(content(Whitespace\"\\n\"))))(Tile((id \
         e602c310-6752-4541-9c99-0deaab5c90c8)(label(type = in))(mold((out \
         Exp)(in_(TPat Typ))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         14c6fd29-81a7-404b-b367-dd241e7678fc)(content(Whitespace\" \
         \"))))(Tile((id \
         c4d0e728-0361-43ec-88b2-fa96245afddf)(label(Col))(mold((out \
         TPat)(in_())(nibs(((shape Convex)(sort TPat))((shape Convex)(sort \
         TPat))))))(shards(0))(children())))(Secondary((id \
         bcc25411-b5af-4452-9b3b-19e3919c2c64)(content(Whitespace\" \
         \")))))((Secondary((id \
         e13a6e3f-47ee-4713-8d2b-7e4adaf59eca)(content(Whitespace\" \
         \"))))(Tile((id \
         f0dbe7cb-b667-4e07-b84a-c6118c13a836)(label(Int))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         158e05d7-9603-4042-a7be-f6325c5964db)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         0864b761-f640-414a-8812-5f3b98a05a7a)(content(Whitespace\"\\n\"))))(Secondary((id \
         78c467ee-9671-49f1-b895-738ff74f416c)(content(Whitespace\"\\n\"))))(Secondary((id \
         fc9eebaa-49ff-48aa-840c-42172236627a)(content(Comment\"# Soil types \
         affect what grows best in each cell #\"))))(Secondary((id \
         23eb3c8a-33db-4725-a3b9-5d2e6617daa3)(content(Whitespace\"\\n\"))))(Tile((id \
         dbc2e3d3-82b6-4b31-bb84-e3b25eef8479)(label(type = in))(mold((out \
         Exp)(in_(TPat Typ))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         4bf74248-0248-490a-9078-5fd6ed428988)(content(Whitespace\" \
         \"))))(Tile((id \
         68828a16-062f-4b61-9164-2f72d6c05198)(label(SoilType))(mold((out \
         TPat)(in_())(nibs(((shape Convex)(sort TPat))((shape Convex)(sort \
         TPat))))))(shards(0))(children())))(Secondary((id \
         53aedb1a-e361-4ddf-a805-622f2d06b94c)(content(Whitespace\" \
         \")))))((Secondary((id \
         a137241f-a3d6-4d3e-8b9c-b17eb4838a7c)(content(Whitespace\"\\n\"))))(Tile((id \
         bbfdbf38-41f9-4fb7-bb55-c17854e9f8b2)(label(+))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape(Concave 33))(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         57e1e82c-fecc-412e-9fa3-f14d579eb294)(content(Whitespace\" \
         \"))))(Tile((id \
         7976fa2a-00e9-4767-9339-a623915cc2d2)(label(Loamy))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         cc8d80d8-21c1-4a62-a56f-db90ddfd1c42)(content(Whitespace\" \
         \"))))(Secondary((id \
         e6e8dcd5-29b5-4b57-962c-4ffcc4f06bc7)(content(Whitespace\" \
         \"))))(Secondary((id \
         45bc2a06-1189-443b-a342-a17822cd90a7)(content(Whitespace\" \
         \"))))(Secondary((id \
         477c8255-0f76-4df8-b087-1a6bb1444d6d)(content(Whitespace\" \
         \"))))(Secondary((id \
         8c33f9f7-6c9b-4bfb-845f-030d44c0e3a3)(content(Comment\"# Rich, \
         balanced soil #\"))))(Secondary((id \
         df1fe366-5e72-451b-9f86-9e4eb3e8c073)(content(Whitespace\"\\n\"))))(Tile((id \
         29fbedc1-148d-4c9c-8ec5-48517b05aa77)(label(+))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 12))(sort Typ))((shape(Concave \
         12))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         2158b7c1-6015-4c5b-a17e-9ae80fc33f13)(content(Whitespace\" \
         \"))))(Tile((id \
         aa4cfcfc-36a8-4ab0-a09e-f1bc84e5c301)(label(Sandy))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         18a00b61-5cb5-40b8-9547-e9d400f61dfe)(content(Whitespace\" \
         \"))))(Secondary((id \
         84625bf7-049b-4477-a99d-ee206099fe7b)(content(Whitespace\" \
         \"))))(Secondary((id \
         0f39fd00-c158-48cd-b33c-c8393a4e6182)(content(Whitespace\" \
         \"))))(Secondary((id \
         757a634d-cea6-4e9a-8b13-1d89e18a3c64)(content(Whitespace\" \
         \"))))(Secondary((id \
         276d496b-bd16-4830-870f-0f921906c7db)(content(Comment\"# Light, \
         drains quickly #\"))))(Secondary((id \
         b10a2ae9-0fb7-4e10-8923-5df51e0b6bea)(content(Whitespace\"\\n\"))))(Tile((id \
         ff392c39-df3e-4211-81b0-1c9f9038a709)(label(+))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 12))(sort Typ))((shape(Concave \
         12))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         22934c76-5268-4a09-bfa8-5dc51c196942)(content(Whitespace\" \
         \"))))(Tile((id \
         52eebecc-7e08-4cc1-bf0e-1e216907718b)(label(Clay))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         28d170a7-818e-40f1-9bf6-32d791da98cb)(content(Whitespace\" \
         \"))))(Secondary((id \
         9e7d6722-6e0b-4cf8-9afe-715da7396d82)(content(Whitespace\" \
         \"))))(Secondary((id \
         d9f7661a-62d8-4866-9e61-f41225dfe438)(content(Whitespace\" \
         \"))))(Secondary((id \
         ee47cea1-6212-4da5-a230-416301c4c61e)(content(Whitespace\" \
         \"))))(Secondary((id \
         5d4d873d-18d9-4605-b555-5c4467d4310e)(content(Whitespace\" \
         \"))))(Secondary((id \
         77f4b2be-3e51-46fe-9c17-405861dd7463)(content(Comment\"# Dense, holds \
         water #\"))))(Secondary((id \
         7a67a2b9-a80c-427a-8635-4a0039cb79f0)(content(Whitespace\"\\n\"))))(Tile((id \
         f137c23f-ca00-4628-9d1d-57009d6549f1)(label(+))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 12))(sort Typ))((shape(Concave \
         12))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         9763bf9e-80a2-42f1-9e5f-ef56cf6c4931)(content(Whitespace\" \
         \"))))(Tile((id \
         8db727a6-f32a-4950-90f7-ff645ca22021)(label(Rich))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         979ee962-1b0f-4a4a-90a0-7b916fcd5166)(content(Whitespace\" \
         \"))))(Secondary((id \
         cc944207-e694-456c-a3d1-2de410114c4e)(content(Whitespace\" \
         \"))))(Secondary((id \
         2326025d-3e0f-47e4-9d49-be7933d3c622)(content(Whitespace\" \
         \"))))(Secondary((id \
         3efd1b80-1cc6-4350-a1bb-a9ad693127ce)(content(Whitespace\" \
         \"))))(Secondary((id \
         e70bf984-95c5-422f-81a3-b12f264088a4)(content(Whitespace\" \
         \"))))(Secondary((id \
         50f094df-0642-4173-908f-ffe9e8738da0)(content(Comment\"# \
         Nutrient-dense, dark #\"))))(Secondary((id \
         52a6053f-6ea1-4973-8a01-15e65984506e)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         a8e323d7-ab9d-4f40-9ab3-6bb94e4fce29)(content(Whitespace\"\\n\"))))(Secondary((id \
         6f24cb77-d5e4-4f2d-a775-6ca1ef529c3c)(content(Whitespace\"\\n\"))))(Secondary((id \
         0d66a316-348b-485e-94ef-8f448d670291)(content(Comment\"# Each cell in \
         the field has two properties #\"))))(Secondary((id \
         fbe3a9e1-972e-4042-a775-e16e8a2f953e)(content(Whitespace\"\\n\"))))(Tile((id \
         d8ed1647-8a0f-4c56-8d54-b5e7c7242e9d)(label(type = in))(mold((out \
         Exp)(in_(TPat Typ))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         6a29344d-653c-416c-b6ae-bafb356c90f6)(content(Whitespace\" \
         \"))))(Tile((id \
         32e22929-e906-43e8-bd67-3e844f080fdc)(label(Cell))(mold((out \
         TPat)(in_())(nibs(((shape Convex)(sort TPat))((shape Convex)(sort \
         TPat))))))(shards(0))(children())))(Secondary((id \
         84fc20ee-003d-4fd0-b545-5882709ae1b9)(content(Whitespace\" \
         \")))))((Secondary((id \
         64f78a98-ba3a-4adc-b8fe-8b4474a15084)(content(Whitespace\" \
         \"))))(Tile((id \
         06698824-5bd4-4f64-b5c7-f213c4a8fbe7)(label(\"(\"\")\"))(mold((out \
         Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0 1))(children(((Secondary((id \
         bf49797b-566e-4159-9335-00f7a9fa1b0b)(content(Whitespace\"\\n\"))))(Tile((id \
         1a82a02e-791d-4b63-85a5-7cfa985e0a56)(label(crop))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         6eecc0e0-36bf-467b-b58a-a1548a5c4ac0)(content(Whitespace\" \
         \"))))(Tile((id \
         186eb6a6-9710-40b0-a164-4e51b2251261)(label(=))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 39))(sort Typ))((shape(Concave \
         39))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         f6377919-c200-4819-889b-f63a7eebf0e5)(content(Whitespace\" \
         \"))))(Tile((id \
         fc5c3303-1a20-4a78-8ed8-23d712e54d2f)(label(Crop))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         bfbd995e-c956-45e7-8875-752cd59bf0b8)(label(,))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 44))(sort Typ))((shape(Concave \
         44))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         f136c227-6aef-4fe5-ac93-1b7c48de442a)(content(Whitespace\" \
         \"))))(Secondary((id \
         8fbb5781-bd77-4665-b179-370a6398dabe)(content(Whitespace\" \
         \"))))(Secondary((id \
         dfaa4957-78a2-47b5-a250-34927432710a)(content(Whitespace\" \
         \"))))(Secondary((id \
         8038bc3d-c974-4333-821f-c24e6997a27b)(content(Whitespace\" \
         \"))))(Secondary((id \
         7212f98f-88f0-49ec-923f-6d3113bffdde)(content(Whitespace\" \
         \"))))(Secondary((id \
         f0fe9d05-6183-4b3a-9b07-7e26a60f09e8)(content(Whitespace\" \
         \"))))(Secondary((id \
         0e94bd4f-2165-4da5-8e9d-a6489ea2add1)(content(Comment\"# What's \
         planted here #\"))))(Secondary((id \
         1059cadf-d1f8-45b8-b6df-7adce0801939)(content(Whitespace\"\\n\"))))(Tile((id \
         fa0a2c7e-c829-4511-91d3-ee0e10b4191d)(label(soil))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         8a343c2d-5055-4a4d-84ee-e490da558a96)(content(Whitespace\" \
         \"))))(Tile((id \
         3f219ee7-0d7f-48c6-8d18-482139b8c922)(label(=))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 39))(sort Typ))((shape(Concave \
         39))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         619e8d53-f46e-47ca-a77b-c20aad24a69c)(content(Whitespace\" \
         \"))))(Tile((id \
         8884009e-5fce-460c-98f5-508e1a38460b)(label(SoilType))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         f58a40d1-acf1-4d35-82fe-9c406cb581bc)(content(Whitespace\" \
         \"))))(Secondary((id \
         5a077ebb-293c-4713-a193-e8978f391754)(content(Whitespace\" \
         \"))))(Secondary((id \
         f1e22776-e81f-4f5a-a2f5-240d8340ae99)(content(Whitespace\" \
         \"))))(Secondary((id \
         1852306d-4eed-4e15-8b1d-05b792948076)(content(Comment\"# What kind of \
         soil #\"))))(Secondary((id \
         a48de723-4265-44b0-9bdb-18d87ff48bc1)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         3ebd4d88-b4ea-4a6c-a2b9-43a87676ffd5)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         18a4e43d-55d3-4ca3-854f-d39a34f10ce4)(content(Whitespace\"\\n\"))))(Secondary((id \
         8e2063ed-4e89-4f15-a43e-95084d543999)(content(Whitespace\"\\n\"))))(Tile((id \
         46f1605b-21f7-423f-8a61-e89d0a4ee933)(label(type = in))(mold((out \
         Exp)(in_(TPat Typ))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         21d49853-73d9-49c9-82c5-7b88f50297b7)(content(Whitespace\" \
         \"))))(Tile((id \
         a30b9588-778f-4994-88b5-0a86883c1c4c)(label(Field))(mold((out \
         TPat)(in_())(nibs(((shape Convex)(sort TPat))((shape Convex)(sort \
         TPat))))))(shards(0))(children())))(Secondary((id \
         4e668b22-9bb8-4a02-a27a-4e4361827b71)(content(Whitespace\" \
         \")))))((Secondary((id \
         7deaf104-6ba6-4d57-9946-291c6363cc13)(content(Whitespace\" \
         \"))))(Tile((id 098d0a42-b534-4680-97ac-18e61553227a)(label([ \
         ]))(mold((out Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape \
         Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
         79f57aa6-1e1a-4559-89ab-72335f08a230)(label([ ]))(mold((out \
         Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0 1))(children(((Tile((id \
         dbbd0049-399e-46da-bfc8-5d0c472b062f)(label(Cell))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))))))))))))(Secondary((id \
         dfe351e3-2398-47b4-bdc8-ef9b6c965687)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         21c89e7e-25ad-485e-8e9e-9dffcf3008f1)(content(Whitespace\"\\n\"))))(Secondary((id \
         494731f5-da98-4740-9c14-498e141939d3)(content(Whitespace\"\\n\"))))(Tile((id \
         ab053abe-5341-491e-a52e-a97cbf1e1041)(label(type = in))(mold((out \
         Exp)(in_(TPat Typ))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         7116f4c4-741e-48cf-8cdb-40016201b65d)(content(Whitespace\" \
         \"))))(Tile((id \
         42d631ff-c548-48b4-9c68-e83d0d7a1f7c)(label(Model))(mold((out \
         TPat)(in_())(nibs(((shape Convex)(sort TPat))((shape Convex)(sort \
         TPat))))))(shards(0))(children())))(Secondary((id \
         b734420d-83cd-4193-982c-0dc86fb04468)(content(Whitespace\" \
         \")))))((Secondary((id \
         d7a87436-aa6f-4c6e-ab46-73bb85bb864f)(content(Whitespace\" \
         \"))))(Tile((id \
         91408615-3c77-4bcb-9cea-0be46e4e198b)(label(\"(\"\")\"))(mold((out \
         Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0 1))(children(((Secondary((id \
         746f409b-a55b-4d0a-86db-0480e01fd11d)(content(Whitespace\"\\n\"))))(Tile((id \
         407af351-b8ae-46c9-97fc-81304abe3ae1)(label(field))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         41a93c0c-d664-4832-94e7-992265224dda)(content(Whitespace\" \
         \"))))(Tile((id \
         2b8d944c-22dd-4a9b-a0bc-5388a7861145)(label(=))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 39))(sort Typ))((shape(Concave \
         39))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         4031ecc0-3bd2-4bb6-b8bc-aca6fbd2cb1c)(content(Whitespace\" \
         \"))))(Tile((id \
         5bd9e653-b922-48ff-b625-7e85bd6960aa)(label(Field))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         ccd048e0-08ac-4b5d-bd13-370bf7e8856f)(label(,))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 44))(sort Typ))((shape(Concave \
         44))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         4f1c22a4-2e8a-441e-b9ca-b9fbefe69b57)(content(Whitespace\"\\n\"))))(Tile((id \
         7bd4a289-3379-4e9d-8fb3-283fcdf27f72)(label(currentSeed))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         56c0d73f-8fd9-46fe-818b-fb0f7b5b935c)(content(Whitespace\" \
         \"))))(Tile((id \
         61f01a53-4115-4879-af60-bb03fa08904b)(label(=))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 39))(sort Typ))((shape(Concave \
         39))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         7a7238c8-11f4-4f5e-99cd-a722aef4f9a9)(content(Whitespace\" \
         \"))))(Tile((id \
         7dd937a3-8c65-4633-a8ed-657d72347bd4)(label(Crop))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         c0a1e7e7-1329-46c1-95d2-aba266f21131)(label(,))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 44))(sort Typ))((shape(Concave \
         44))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         8313ca5e-d752-4369-8548-4657741d658b)(content(Whitespace\"\\n\"))))(Tile((id \
         b756fee6-a363-4ba6-8dab-4f1c2cc258d4)(label(seedInventory))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         25a877fd-afb4-4f3c-a22e-e332024fca2e)(content(Whitespace\" \
         \"))))(Tile((id \
         9885dfc3-ff08-44d1-8040-a1166b779b41)(label(=))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 39))(sort Typ))((shape(Concave \
         39))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         18399361-95ea-44d4-b5bf-21327632c20f)(content(Whitespace\" \
         \"))))(Tile((id 519d0921-edc6-45c0-9a58-142730d6a9b0)(label([ \
         ]))(mold((out Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape \
         Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
         dc391adf-065a-4a07-9e71-ed21fb3201cb)(label(Crop))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children()))))))))(Secondary((id \
         45a8e7e2-c60e-4a2f-a00f-c0af5a67929a)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         dd7ee96b-dd05-47d1-8877-c546dd3ebf38)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         dcca8dce-af2c-44cd-b4c7-64f81fd08170)(content(Whitespace\"\\n\"))))(Secondary((id \
         4aba0bcc-739a-4c86-bbf6-88e361e30f00)(content(Whitespace\"\\n\"))))(Tile((id \
         98ecc587-3a9d-4131-9fca-d49732e6d6a6)(label(type = in))(mold((out \
         Exp)(in_(TPat Typ))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         00a2e203-0bf0-4bd7-b799-f26bf6f72e3a)(content(Whitespace\" \
         \"))))(Tile((id \
         3c256908-808a-4ba7-9684-21cc39bf76c8)(label(Action))(mold((out \
         TPat)(in_())(nibs(((shape Convex)(sort TPat))((shape Convex)(sort \
         TPat))))))(shards(0))(children())))(Secondary((id \
         e1fd9b00-d67e-4730-ab75-e17c651beee8)(content(Whitespace\" \
         \")))))((Secondary((id \
         3fcc5262-d703-49ff-bbc7-a0cce1eac4fc)(content(Whitespace\"\\n\"))))(Tile((id \
         332d5cd3-4f6c-441a-bbaf-5c6e8006be31)(label(+))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape(Concave 33))(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         328411d5-c02c-4194-bb40-290e179dcbda)(content(Whitespace\" \
         \"))))(Tile((id \
         da6f6bd2-97ad-440e-a54d-95285ca51020)(label(PlantCrop))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         5ea0b8b4-7e48-47fa-9a9c-e3f70bee880d)(label(\"(\"\")\"))(mold((out \
         Typ)(in_(Typ))(nibs(((shape(Concave 11))(sort Typ))((shape \
         Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
         79ea9bd8-d6f7-406d-b160-f52dbd087363)(label(Row))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         1299b8df-ff93-44c5-9628-959935e53852)(label(,))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 44))(sort Typ))((shape(Concave \
         44))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         fbf22c85-a86a-4b0a-9f14-809fe7231bfd)(content(Whitespace\" \
         \"))))(Tile((id \
         c3285520-699f-4699-b6a7-c212399ceae6)(label(Col))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children()))))))))(Secondary((id \
         f4050fda-ef4a-4871-b962-0d3210d30e15)(content(Whitespace\" \
         \"))))(Secondary((id \
         770da467-98fd-4022-92e3-16c849082100)(content(Whitespace\" \
         \"))))(Secondary((id \
         36ad7dfe-d254-4b82-a84b-d1e4eef3f5d9)(content(Whitespace\" \
         \"))))(Secondary((id \
         7a0b063d-ea03-4656-b217-7cd97717a55b)(content(Whitespace\" \
         \"))))(Secondary((id \
         47c54c26-e838-4d0c-8f66-e58d07d54bf4)(content(Whitespace\" \
         \"))))(Secondary((id \
         d5c153a9-1f80-41ef-922a-4091960e3349)(content(Whitespace\" \
         \"))))(Secondary((id \
         834a8a6f-2285-4b9a-9f9d-4e4c1ae6d075)(content(Whitespace\" \
         \"))))(Secondary((id \
         0317d650-831f-43e3-924b-a2ed4c12c15c)(content(Whitespace\" \
         \"))))(Secondary((id \
         d3c6dfc1-dcf0-4494-ae88-23338f9069ee)(content(Whitespace\" \
         \"))))(Secondary((id \
         71989367-b7d2-445a-86b5-2851bee116a0)(content(Whitespace\" \
         \"))))(Secondary((id \
         270efd82-86f6-4dd0-868e-3900bdb28ba6)(content(Whitespace\" \
         \"))))(Secondary((id \
         ed981cd2-cd40-4832-b5b3-d509c1f8314b)(content(Whitespace\" \
         \"))))(Secondary((id \
         5ef35e79-a7af-4374-888d-855d93f26132)(content(Comment\"# Plant \
         current seed at position #\"))))(Secondary((id \
         831405ae-551a-40c9-a89f-aa43d717cd01)(content(Whitespace\"\\n\"))))(Tile((id \
         6d6d3f43-4e6d-418b-b078-610dc964ea63)(label(+))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 12))(sort Typ))((shape(Concave \
         12))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         d80e2a73-8794-466e-9cbd-6fd921da27e9)(content(Whitespace\" \
         \"))))(Tile((id \
         b0ceb95c-ec6c-4c4e-8544-7331afca92ba)(label(HarvestCrop))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         19349283-791c-4ad1-97c7-982867cad842)(label(\"(\"\")\"))(mold((out \
         Typ)(in_(Typ))(nibs(((shape(Concave 11))(sort Typ))((shape \
         Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
         7088dfcd-e32a-4ac7-b08f-b4626f451dba)(label(Row))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         17f4de4b-54ea-408a-bfc3-4f3bb0be6a48)(label(,))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 44))(sort Typ))((shape(Concave \
         44))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         d0766c11-8b7c-4ffe-8d66-906b73c2ef8c)(content(Whitespace\" \
         \"))))(Tile((id \
         9e07d4ed-cc2a-46da-8c23-25361bd8b79c)(label(Col))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children()))))))))(Secondary((id \
         9378f7d3-0932-4b97-88c4-4b56cc0424ba)(content(Whitespace\" \
         \"))))(Secondary((id \
         cec32519-c603-4f99-9080-29d8d0b4fc2d)(content(Whitespace\" \
         \"))))(Secondary((id \
         bd07ae18-1241-492c-968d-7f7346e53580)(content(Whitespace\" \
         \"))))(Secondary((id \
         627df5fc-af04-47d9-a323-4e6cbf2f7d60)(content(Whitespace\" \
         \"))))(Secondary((id \
         59f0ee4b-d2dc-4a9b-8094-63d96b8bcc55)(content(Whitespace\" \
         \"))))(Secondary((id \
         6dcaea83-e865-47cc-b1fa-82097c69ed17)(content(Whitespace\" \
         \"))))(Secondary((id \
         e39ca3d0-1f7a-4cdc-b804-9d761f0cdbcb)(content(Whitespace\" \
         \"))))(Secondary((id \
         61cdc3af-01fb-4372-a11c-a0953fa14a22)(content(Whitespace\" \
         \"))))(Secondary((id \
         4c4a0450-6e34-490a-8c92-69892c012e9b)(content(Whitespace\" \
         \"))))(Secondary((id \
         7e7df165-f4df-492d-b09d-9deb78c9130d)(content(Whitespace\" \
         \"))))(Secondary((id \
         70d35c83-1b04-4257-9ab7-b74b7a25c8e6)(content(Comment\"# Remove crop \
         at position #\"))))(Secondary((id \
         f143f112-0731-484d-aee9-880b7f819f48)(content(Whitespace\"\\n\"))))(Tile((id \
         620acad4-d840-429f-9efd-ec43259da429)(label(+))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 12))(sort Typ))((shape(Concave \
         12))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         f72fc6d2-d234-437f-bdc3-9d3bf79fc7ae)(content(Whitespace\" \
         \"))))(Tile((id \
         1afcb6e7-2597-41b8-93e0-d446dea148b3)(label(TillSoil))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         8eab07c7-a6ab-4949-858a-2d99e01c8de2)(label(\"(\"\")\"))(mold((out \
         Typ)(in_(Typ))(nibs(((shape(Concave 11))(sort Typ))((shape \
         Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
         aa46de01-de0f-4753-a1fb-a274b8dc655c)(label(Row))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         5fb2d13c-c5ae-4599-b833-e191d11f83a8)(label(,))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 44))(sort Typ))((shape(Concave \
         44))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         1aff61c8-74f0-4bec-bc22-5e3c4e144afe)(content(Whitespace\" \
         \"))))(Tile((id \
         e29f215e-0c12-42fa-abfe-034952401919)(label(Col))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         81a034c5-e1c7-45d6-9d88-b4d97bb53492)(label(,))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 44))(sort Typ))((shape(Concave \
         44))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         3073f71d-c511-422b-af1a-c825e63a781d)(content(Whitespace\" \
         \"))))(Tile((id \
         b174fb79-4f57-43d7-b717-2eccdf8820d8)(label(SoilType))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children()))))))))(Secondary((id \
         80125c35-ae69-4586-9553-a4bded0dcdda)(content(Whitespace\" \
         \"))))(Secondary((id \
         4d606843-1d87-4bde-b1a0-79202bca1583)(content(Whitespace\" \
         \"))))(Secondary((id \
         b1a278a7-0cc1-4071-b6f1-4dac526354a8)(content(Whitespace\" \
         \"))))(Secondary((id \
         836824dd-de36-45ff-ba00-bae0e24895af)(content(Comment\"# Change soil \
         type at position #\"))))(Secondary((id \
         3e175c92-95e0-4a1f-a8f7-f0a8d832d448)(content(Whitespace\"\\n\"))))(Tile((id \
         ef12a5af-16b3-4f91-8bf0-3e2c68d95aea)(label(+))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 12))(sort Typ))((shape(Concave \
         12))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         fdb0dd28-deef-4756-9e12-8a93274e8bf3)(content(Whitespace\" \
         \"))))(Tile((id \
         23fc2d15-cd9a-4b14-97a0-171470aa57aa)(label(PlantRow))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         625a5523-a8cb-45fd-8222-03da221f1b5c)(label(\"(\"\")\"))(mold((out \
         Typ)(in_(Typ))(nibs(((shape(Concave 11))(sort Typ))((shape \
         Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
         3a25a688-2d8d-4b4e-ac0d-fb998d9bd927)(label(Row))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children()))))))))(Secondary((id \
         fc237e42-a099-4cfe-8377-cee5962f8f36)(content(Whitespace\" \
         \"))))(Secondary((id \
         be67e01c-973d-4824-81c8-acab3dc4f32c)(content(Whitespace\" \
         \"))))(Secondary((id \
         4d039c8b-49a3-460e-b88e-2d5b6dd0b73d)(content(Whitespace\" \
         \"))))(Secondary((id \
         346c74c0-fb0a-4a51-b47a-598a50e49272)(content(Whitespace\" \
         \"))))(Secondary((id \
         70975e78-09a0-442c-9718-b5f007e0b177)(content(Whitespace\" \
         \"))))(Secondary((id \
         bf47536a-0366-479e-a500-6dbbb9c5007b)(content(Whitespace\" \
         \"))))(Secondary((id \
         ee9fbf7b-29f1-40ad-8a9f-0f4891612b8e)(content(Whitespace\" \
         \"))))(Secondary((id \
         ea9ad430-8aff-40e0-b604-7c7e83723a30)(content(Whitespace\" \
         \"))))(Secondary((id \
         d32827b2-5b87-4d01-921e-97e26f7b91ee)(content(Whitespace\" \
         \"))))(Secondary((id \
         7b53f956-9f43-4f8d-9ef2-1d1d50e845b9)(content(Whitespace\" \
         \"))))(Secondary((id \
         c1881dc4-0d0e-41e2-b9b2-1a286658112a)(content(Whitespace\" \
         \"))))(Secondary((id \
         a69c45e1-295e-444d-92c0-4008011ce5b4)(content(Whitespace\" \
         \"))))(Secondary((id \
         b738d0a7-3c6d-4129-831c-f279ed1a6d24)(content(Whitespace\" \
         \"))))(Secondary((id \
         8208c170-0949-44c9-a811-1a77595711ee)(content(Whitespace\" \
         \"))))(Secondary((id \
         19525430-1c1d-4e70-b2bb-f8c462c0f6b9)(content(Whitespace\" \
         \"))))(Secondary((id \
         7b6c2411-4b57-448c-8d97-876dbc335f54)(content(Whitespace\" \
         \"))))(Secondary((id \
         119898ad-3c7f-4be6-b95a-a37e844c24b4)(content(Whitespace\" \
         \"))))(Secondary((id \
         0f6f129d-89b7-416f-a8d2-880145b43dcd)(content(Whitespace\" \
         \"))))(Secondary((id \
         491545bd-8a49-4607-98c9-f3f298db78a2)(content(Comment\"# Plant entire \
         row with current seed #\"))))(Secondary((id \
         b9dfb713-eb6b-49e2-be3a-f67d61b62eb0)(content(Whitespace\"\\n\"))))(Tile((id \
         211e2ecc-5e89-447c-8b29-079fb1700988)(label(+))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 12))(sort Typ))((shape(Concave \
         12))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         dad6ec14-afd9-4bff-ba48-00837becc2c5)(content(Whitespace\" \
         \"))))(Tile((id \
         c18b7829-2f71-42b2-952e-59b0c1ade85d)(label(ClearField))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         eff1817b-8a47-418f-bc87-f4689875baf4)(content(Whitespace\" \
         \"))))(Secondary((id \
         d879333e-44cb-48e9-9c72-5a17c9745b01)(content(Whitespace\" \
         \"))))(Secondary((id \
         64c5f4ef-5acc-4648-ae09-fdb2ad221e50)(content(Whitespace\" \
         \"))))(Secondary((id \
         01b2f08b-b924-4e9a-8446-25816ed199d9)(content(Whitespace\" \
         \"))))(Secondary((id \
         28f73957-6d55-442b-9c5e-1457cb886716)(content(Whitespace\" \
         \"))))(Secondary((id \
         ef935dea-8fe5-482f-a819-d7eef1a44fe4)(content(Whitespace\" \
         \"))))(Secondary((id \
         5a8b394b-16a7-4468-84d5-112a186c7b7e)(content(Whitespace\" \
         \"))))(Secondary((id \
         be5d5c7f-a01b-45f8-b739-c46a7687b9a4)(content(Whitespace\" \
         \"))))(Secondary((id \
         8f836e20-c160-4c4c-9a46-70aa16c4f96c)(content(Whitespace\" \
         \"))))(Secondary((id \
         148b977f-5e4e-4668-9958-ebad0df79636)(content(Whitespace\" \
         \"))))(Secondary((id \
         0755ef9a-318c-4da1-affa-4a9fc7faffb4)(content(Whitespace\" \
         \"))))(Secondary((id \
         6bdb545b-102b-4e5f-873a-e0dfc4ff5534)(content(Whitespace\" \
         \"))))(Secondary((id \
         124ac841-0a80-4498-942b-64258d5343bc)(content(Whitespace\" \
         \"))))(Secondary((id \
         84e15ac1-0304-4482-9eaa-bb88e4ceea3b)(content(Whitespace\" \
         \"))))(Secondary((id \
         e41c3c4d-b9ab-483e-b3cd-9c7a8e19858f)(content(Whitespace\" \
         \"))))(Secondary((id \
         ebef40f7-eaed-4837-94f2-407bdf67c964)(content(Whitespace\" \
         \"))))(Secondary((id \
         a6f2e1fc-c8e0-4488-a4db-d1b5538e534b)(content(Whitespace\" \
         \"))))(Secondary((id \
         e2d1731a-9d49-4ea1-859f-7092aa6338ef)(content(Whitespace\" \
         \"))))(Secondary((id \
         eefa5584-22f9-4188-92fc-fcf3456b54a7)(content(Whitespace\" \
         \"))))(Secondary((id \
         8c53803f-6690-4139-8472-b8d72f5a9c7b)(content(Whitespace\" \
         \"))))(Secondary((id \
         020d08d5-ba01-406e-afc3-44ae21dab468)(content(Whitespace\" \
         \"))))(Secondary((id \
         e76dfdd4-dafd-40eb-aea7-c681f2d41173)(content(Comment\"# Remove all \
         crops #\"))))(Secondary((id \
         99a8ac41-9510-4f12-94f4-0d92d058b90f)(content(Whitespace\"\\n\"))))(Tile((id \
         1da105a6-7f5f-4077-b595-0869627726b2)(label(+))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 12))(sort Typ))((shape(Concave \
         12))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         6c67aff8-82f9-426d-b96a-22e0407de5e4)(content(Whitespace\" \
         \"))))(Tile((id \
         b117868d-f79d-492e-9fd8-1f3dbf384ba9)(label(SelectSeed))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         04d2f482-d198-4251-a97f-ddde2005535e)(label(\"(\"\")\"))(mold((out \
         Typ)(in_(Typ))(nibs(((shape(Concave 11))(sort Typ))((shape \
         Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
         86bc4d90-e27d-4e19-881d-5d0d17e82001)(label(Int))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children()))))))))(Secondary((id \
         94ad11b8-25d1-4f76-9514-d6fa66b5a1a0)(content(Whitespace\" \
         \"))))(Secondary((id \
         5bf261f1-88b3-45fa-bd3f-a01bb827141c)(content(Whitespace\" \
         \"))))(Secondary((id \
         2b4a883f-8354-4a98-9561-0ede071de91d)(content(Whitespace\" \
         \"))))(Secondary((id \
         0febd562-27b7-4e97-bc03-79e06d99e3a9)(content(Whitespace\" \
         \"))))(Secondary((id \
         f367a75a-3aae-455a-bb3c-737d413f1d64)(content(Whitespace\" \
         \"))))(Secondary((id \
         9359e080-af6c-496f-b7e8-44d5295155b2)(content(Whitespace\" \
         \"))))(Secondary((id \
         a4dff3b5-80ba-4c17-bec8-324e1b149c67)(content(Whitespace\" \
         \"))))(Secondary((id \
         cb84e035-b24b-47a5-9f35-9bd8e29842bb)(content(Whitespace\" \
         \"))))(Secondary((id \
         fb3241c6-3014-4664-a8fe-c3caaf41245e)(content(Whitespace\" \
         \"))))(Secondary((id \
         a28356a2-f984-4809-9be8-8d448fbd8ce8)(content(Whitespace\" \
         \"))))(Secondary((id \
         956eef76-db76-40a1-9121-d0c69f1e99a6)(content(Whitespace\" \
         \"))))(Secondary((id \
         1729d468-452e-4ca3-9150-775256d8e8a4)(content(Whitespace\" \
         \"))))(Secondary((id \
         10700b6d-f5a1-4016-aa83-37276ed9e379)(content(Whitespace\" \
         \"))))(Secondary((id \
         f557f2bd-c6d0-4c81-99b3-afb27213327d)(content(Whitespace\" \
         \"))))(Secondary((id \
         270dd624-677e-4bc4-a6d1-5ba4639d111c)(content(Whitespace\" \
         \"))))(Secondary((id \
         69b1fb85-2461-4240-a858-a9f6b9adbc50)(content(Whitespace\" \
         \"))))(Secondary((id \
         366895be-91ba-4257-ad61-f1dc709e3173)(content(Comment\"# Select seed \
         from inventory #\"))))(Secondary((id \
         6d5f6935-c2b3-4624-9217-f7ebf89df731)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         b82aa452-bb14-423b-8474-bd074ef135ea)(content(Whitespace\"\\n\"))))(Secondary((id \
         8ac56427-5639-4c09-acc1-45084f6634c1)(content(Whitespace\"\\n\"))))(Secondary((id \
         1f537451-87dd-4001-b040-7234cfb2bbdb)(content(Comment\"# Initial \
         field with varied soil types #\"))))(Secondary((id \
         753ef57c-4e76-4c1a-9926-b3bdd67027ea)(content(Whitespace\"\\n\"))))(Tile((id \
         f10bed69-815a-47a5-8c42-2ddeee2ee236)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         3dedba54-6e37-4b02-8193-c942ddf51f23)(content(Whitespace\" \
         \"))))(Tile((id \
         957493cf-1dad-4c06-85b8-7095e2cda879)(label(init))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         937cb857-5c3c-4b6e-b256-2c7226378094)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 24))(sort Pat))((shape(Concave \
         24))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         4a0811e8-7d0f-4899-9958-ef166d217842)(content(Whitespace\" \
         \"))))(Tile((id \
         93fd4b60-b9c7-4839-9bcd-930249d58720)(label(Model))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         c91b566a-15ec-42a3-a07c-d757ae5f9557)(content(Whitespace\" \
         \")))))((Secondary((id \
         9328222a-622f-4df7-9e2b-94ea4e9dcfae)(content(Whitespace\" \
         \"))))(Tile((id \
         c0eaa6e8-2528-4038-aaa5-fa41fc365fc3)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         a498926f-5db1-4100-9504-52056d3de2dd)(content(Whitespace\"\\n\"))))(Tile((id \
         9f4dd005-3a49-4f85-97c9-f0723056840d)(label(field))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         2d2958c8-24b3-44b0-a629-fa087786bda8)(content(Whitespace\" \
         \"))))(Tile((id \
         d6f7105d-d7a7-47df-a8e6-d86cae0b79bb)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         79df7330-9d5b-4732-88c7-68e6d1b72be4)(content(Whitespace\" \
         \"))))(Tile((id fc847955-2f2d-49f4-876f-68e36e637410)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         6abf25bb-6f0d-4348-8bcb-7a60708170ff)(content(Whitespace\"\\n\"))))(Tile((id \
         b349e516-21b3-4124-bafe-437a29c4a00b)(label([ ]))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         9c3a6bd1-4c9c-4b49-af7b-fb790de49054)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         902cf46a-ee6d-4fa4-8990-e0d4b290ee58)(label(crop))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         83351967-d854-419c-af02-74d07bc8a8c6)(content(Whitespace\" \
         \"))))(Tile((id \
         e1834a6f-c683-495e-832d-34d8e0aaa0c7)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         c722eb35-260f-46b7-982b-fecf38b286ed)(content(Whitespace\" \
         \"))))(Tile((id \
         94769eb8-e411-4832-89e1-2217ca8126f9)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         5ab8e5de-1aea-4ffb-8b66-c6ffd56f5db8)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         4bb0765e-10d3-49b2-8636-900196784d8f)(content(Whitespace\" \
         \"))))(Tile((id \
         6903a511-d228-4a42-b443-d165f8e9f8ae)(label(soil))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         00222121-e8be-47b2-9d38-f864a2b52a16)(content(Whitespace\" \
         \"))))(Tile((id \
         a195f234-06eb-4c04-a8dc-1c0e4b209788)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         a688eebe-a645-4777-b54b-dc788bf19b43)(content(Whitespace\" \
         \"))))(Tile((id \
         272f1426-9a54-4c11-abdc-4ba25e21a522)(label(Loamy))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         5349dcc1-dc8f-426e-acaf-243013248ed0)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         29b842a6-aa39-48f7-8140-dfda8b139838)(content(Whitespace\" \
         \"))))(Tile((id \
         a15afea6-9b86-48ad-81b1-ea4dfbc5b281)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         ee01dc31-eabc-4125-aaca-fe8112262d5c)(label(crop))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         cd075f06-d2a7-4b00-91b7-d86f9e77cc03)(content(Whitespace\" \
         \"))))(Tile((id \
         f06602b7-642c-40ab-8e06-1bb221789ed6)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         2457ae09-a633-45ef-ba08-a6221d2f6dd7)(content(Whitespace\" \
         \"))))(Tile((id \
         daceeb63-3378-49ba-84bb-1a85a4984f8a)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         b3c7ff9e-34c1-45e3-9346-a5d0906019b2)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         8fc2b16b-913f-40dd-b8a5-2727b91e371d)(content(Whitespace\" \
         \"))))(Tile((id \
         11e3fe9a-b18f-4f48-ac86-ecccfe4e01e9)(label(soil))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         44342263-ea0b-45ab-ab91-e3da2f54cc80)(content(Whitespace\" \
         \"))))(Tile((id \
         7625107e-080b-428c-9f17-98557faba1a9)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         8b5ebee9-ac65-4399-823c-08dc4eca1511)(content(Whitespace\" \
         \"))))(Tile((id \
         7073c22d-0d65-4994-ba1b-5eb3cf1a0969)(label(Sandy))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         700faf7b-0b43-46ae-94ab-888d9086322c)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         48b0da66-85ca-433a-892b-67d709a2a015)(content(Whitespace\" \
         \"))))(Tile((id \
         e1ccc03c-0624-45f1-a4a2-7b868cccb70b)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         59d083b9-db1f-4dd5-9ce9-76dba13536f1)(label(crop))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         9d63b67d-e6a4-40ea-8fbd-26b837e72e99)(content(Whitespace\" \
         \"))))(Tile((id \
         25b77195-adf1-4cfd-98ca-a30953b381c1)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         2048f310-4353-45a8-8ab3-c6f631501f42)(content(Whitespace\" \
         \"))))(Tile((id \
         d1cfe0d2-6ce4-45f6-bc14-d5dbce05b856)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         5673eb1f-0990-4106-8640-5e83a16b1977)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         858d4077-8547-4151-92f2-4f28b645119b)(content(Whitespace\" \
         \"))))(Tile((id \
         c265694d-0f19-4058-8f53-d97b5afc5d33)(label(soil))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         7ce557f9-03a0-4d0b-86d3-27e429b1c007)(content(Whitespace\" \
         \"))))(Tile((id \
         76973d5a-7b56-4304-810a-b42d7f87cdb1)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         38dcc0fd-4695-426c-aa54-e6037c6e9690)(content(Whitespace\" \
         \"))))(Tile((id \
         39c2f620-bf76-41c6-8ec0-bd5220f8b309)(label(Clay))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))))))))))))(Tile((id \
         5bb5c653-0a46-4f19-8fae-b530d2e95c32)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         63d846e3-409b-4d97-bedc-f667131962c9)(content(Whitespace\"\\n\"))))(Tile((id \
         8eabcc7c-2d1d-41a8-9cfd-d87c5f88b6da)(label([ ]))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         1fa750fc-1c79-401b-909d-3134a08f750a)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         beb8dbfd-98ce-4185-b867-5ee5153f4e82)(label(crop))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         33aaad4f-d789-4376-98e4-259bf85c5254)(content(Whitespace\" \
         \"))))(Tile((id \
         28998e74-edc1-42bd-a000-e7447ae12d79)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         64678a3e-72e4-4893-bb62-0b3ae0a44d0b)(content(Whitespace\" \
         \"))))(Tile((id \
         d4dd3b2a-a098-4102-af93-c29b87f9fa0c)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         1120859d-060e-41f4-9e8a-77861021b3e6)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         22048f3c-42aa-4d63-835e-6235923f4ee3)(content(Whitespace\" \
         \"))))(Tile((id \
         a8c681c6-8ae6-497d-912a-8bd612de899f)(label(soil))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         7d2f76cd-675d-4f95-afaa-379393d4582f)(content(Whitespace\" \
         \"))))(Tile((id \
         abc31028-15bb-47f1-b46b-12b9f9ed5fa5)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         74c662cf-8fb0-40a6-9249-18aa0287ec7b)(content(Whitespace\" \
         \"))))(Tile((id \
         f5d52b2f-6cbb-45ed-9a0e-2bd617cd8a61)(label(Rich))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         aa5d3aa0-5400-45b1-82fa-c29c55b37bdf)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         d2726897-91a8-4081-97ce-bedd56002f12)(content(Whitespace\" \
         \"))))(Secondary((id \
         2d9f1d32-9a99-4055-8e6b-1300de0357f1)(content(Whitespace\" \
         \"))))(Tile((id \
         8fa47afa-bf60-4930-b5d4-87cfc7e7a24f)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         82016b31-1a99-42ef-a0b9-8dc30aadd8c2)(label(crop))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         e5c00ee9-8d74-4b58-b00a-084309cc614d)(content(Whitespace\" \
         \"))))(Tile((id \
         d16e0262-751f-4fab-b3e4-01495f0b8eae)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         2281549a-b8e1-486d-855e-9bd9b1ce028f)(content(Whitespace\" \
         \"))))(Tile((id \
         396ae1dc-d94f-4be3-8af8-74633fc62529)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         f86eeebf-a76d-4971-aba4-0ee4e4a5d77b)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         4f965835-a56c-4f18-83e8-293da36ae854)(content(Whitespace\" \
         \"))))(Tile((id \
         a5fadb93-8fc2-4274-b19d-136a84d4a290)(label(soil))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         b3ac5dd3-b5c7-49e1-a4e6-e938e9a7d3d7)(content(Whitespace\" \
         \"))))(Tile((id \
         d5bdae2a-d713-4779-96d5-6f1512f2d3a5)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         d75cc233-6b46-464a-8264-e8b1a35fc61d)(content(Whitespace\" \
         \"))))(Tile((id \
         20af4ce0-4429-4552-a077-aeee462f32b7)(label(Loamy))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         669e3f1d-b24e-4c74-987a-8af7d4c254bf)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         4d64dd0f-8f93-4aa6-a783-4d51d0b61160)(content(Whitespace\" \
         \"))))(Tile((id \
         4452142e-2e67-4925-ac58-73ee0b06c9e3)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         ce35ec82-acfb-42bc-89d9-457addc2d7aa)(label(crop))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         916fe0c3-adc1-4daf-9284-fa41918d7564)(content(Whitespace\" \
         \"))))(Tile((id \
         9364585d-9d35-4f67-b3d3-44417a99b1d1)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         645331e2-8d6c-4e45-b37f-319995f2a479)(content(Whitespace\" \
         \"))))(Tile((id \
         3f246d5d-f5d2-4859-a51e-48c85558575a)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         87e6ebe1-b2bb-48b0-b577-de9caae7035c)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         a6115d25-d39c-4991-90c6-004dd386defd)(content(Whitespace\" \
         \"))))(Tile((id \
         82f95761-304e-477b-a17c-1eebb21e0d5f)(label(soil))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         637aa856-d637-4fdf-aafb-19b0553a0161)(content(Whitespace\" \
         \"))))(Tile((id \
         076d8413-f67c-4d1e-8948-18b590d34c59)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         a9983172-eb5b-481d-b019-13d385ccad26)(content(Whitespace\" \
         \"))))(Tile((id \
         b0f41cff-b6a9-43e0-ad5b-c2feca93c325)(label(Sandy))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))))))))))))(Tile((id \
         5597ced1-f9d1-41f4-a72c-48b9352b0763)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         a757eed1-7e08-482d-a3eb-fe02adb6eb3e)(content(Whitespace\"\\n\"))))(Tile((id \
         3e2a7efc-9532-46f0-998e-4fd0cd744434)(label([ ]))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         517bec64-dd7a-4f25-bfa7-0cce7c2011e0)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         e14b77ff-aa47-4e74-8891-07f2a36daf6e)(label(crop))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         89a9c9f9-b30a-4474-a0bc-8610d1f440f7)(content(Whitespace\" \
         \"))))(Tile((id \
         ec268643-1530-4eb6-81a0-e0520f42b9ea)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         91841874-a2bc-43fd-b197-0edb7fc46328)(content(Whitespace\" \
         \"))))(Tile((id \
         3a936619-5434-4686-942f-991f3e5163c3)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         92e8e2c9-9c4a-4a54-92d1-ff70dcb8113f)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         51819e63-21d7-41f2-bce0-cde22caef0d4)(content(Whitespace\" \
         \"))))(Tile((id \
         e5095fda-ffcf-4040-8631-1d70bde32001)(label(soil))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         1644c6d6-d297-4bfd-8f57-a4444372ee86)(content(Whitespace\" \
         \"))))(Tile((id \
         a75fda0b-2a17-4fc4-ae55-bbd23d0ff24b)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         04e65ef2-5521-429c-acd5-0c70a7368a9e)(content(Whitespace\" \
         \"))))(Tile((id \
         705687bb-0624-4ccf-95b1-06a4c2ba8368)(label(Clay))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         00504bce-abe5-48f8-91cf-3175b244cafa)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         c0442193-bdac-4163-bca7-32205344e3bc)(content(Whitespace\" \
         \"))))(Secondary((id \
         b3fd4339-369e-4f1b-bcb9-0e1b60782aca)(content(Whitespace\" \
         \"))))(Tile((id \
         f4a42a00-0c99-4bcb-ab6e-e85bc2a71fc0)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         9d5e498b-55fe-4cd1-8d1f-47e8faf74556)(label(crop))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         64408118-5d4f-49ad-b8c6-e78c2be436c3)(content(Whitespace\" \
         \"))))(Tile((id \
         90aa33e0-47f5-4ba3-9073-ee736bbb4703)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         be5f9554-f7e1-4bcc-be10-76cec93b8187)(content(Whitespace\" \
         \"))))(Tile((id \
         4b800426-b1f7-4517-b9a7-9a1edb0b063d)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         078a9230-3f68-491b-a6a9-0770677619c4)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         ecd7c602-7325-45e1-a2c8-72c0cb45e70e)(content(Whitespace\" \
         \"))))(Tile((id \
         504ebdb2-d64a-4779-bb58-a38a30ed2478)(label(soil))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         c6b2bc7c-059f-4176-8862-a5b3cbd4d5cb)(content(Whitespace\" \
         \"))))(Tile((id \
         1cf667a2-23dd-4904-bb69-fcb41ab37b28)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         418712a1-cce6-4ecd-a65c-e91c7f4fdc06)(content(Whitespace\" \
         \"))))(Tile((id \
         b85ef241-89ec-406c-9ece-2522f3a93104)(label(Rich))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         4ff42226-affd-481b-8a9c-4da2b606975e)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         4be4a597-c15b-4926-86ec-3dbe0fb91f05)(content(Whitespace\" \
         \"))))(Secondary((id \
         77143121-1233-421d-bfae-adcc2bcf428f)(content(Whitespace\" \
         \"))))(Tile((id \
         3440f4b9-4293-4916-992c-0ac7ea7db9c1)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         21f3f4cd-1415-4c9d-813b-05db3c5fff48)(label(crop))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         c2165362-5b23-4814-9259-47e737ffcf4d)(content(Whitespace\" \
         \"))))(Tile((id \
         adda774c-6e45-40ab-817b-385899174aac)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         caa56f4f-1069-464a-8cff-796d7753bac9)(content(Whitespace\" \
         \"))))(Tile((id \
         0d01ecc3-c765-4163-afd7-c913e59d44ad)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         fd2367f2-910e-4098-9b30-d3ef518fedea)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         68310518-e79d-4f7f-b055-0283995d0c2e)(content(Whitespace\" \
         \"))))(Tile((id \
         e6e05832-d55b-4970-b810-77b3e9aa957c)(label(soil))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         a48590b8-b93a-4c25-8201-2db434a1bc3d)(content(Whitespace\" \
         \"))))(Tile((id \
         07faeb51-253a-4fce-b465-ce21bb4150e2)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         502e1aae-c5b9-4414-8505-688582cc4115)(content(Whitespace\" \
         \"))))(Tile((id \
         be0c826f-7d87-473a-9da2-746b7e975510)(label(Loamy))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))))))))))))(Secondary((id \
         21172f7b-a6cf-40fb-9e7d-5ef5db6037f2)(content(Whitespace\"\\n\")))))))))(Tile((id \
         db7a00d9-9259-4aa9-94c0-bf64f15bd315)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         570f7267-4d6d-4721-8b0c-66758367cbb4)(content(Whitespace\"\\n\"))))(Tile((id \
         f5748c0c-5a32-4b0b-b98b-aa29e5c9c308)(label(currentSeed))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         e4dd74e4-4ac8-42f9-9300-8c8a8ebb0ec6)(content(Whitespace\" \
         \"))))(Tile((id \
         cb6563f7-049f-47dc-b634-1cbb45cbd1e3)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         5d705a09-ce7f-4506-bf7b-653ddebc6989)(content(Whitespace\" \
         \"))))(Tile((id \
         3218fa29-5da3-46b2-a6e7-a3f5b6d7363e)(label(\"\\\"\\240\\159\\140\\177\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         8c61fa73-8a08-41e0-94d1-dccd07ec356b)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         eed15a76-257f-4068-a3df-ee5fb5613cad)(content(Whitespace\"\\n\"))))(Tile((id \
         d7004357-bbfe-4e9f-9590-3cbf67e91822)(label(seedInventory))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         3d8e8605-7ca2-4590-9e26-65319c8bd94c)(content(Whitespace\" \
         \"))))(Tile((id \
         62174f11-6c70-41b1-9a79-87aa7cc1ec8a)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         e686a78f-dcfc-4f4d-9c90-45f6f4303d42)(content(Whitespace\" \
         \"))))(Tile((id 71d86271-3ec2-4e4a-8369-9e3ca1bd4883)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         e22dd88e-0eb5-4822-9568-41966a6d7844)(label(\"\\\"\\240\\159\\140\\177\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         f80100fa-ebf3-46b9-ba5c-c743a6740f23)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         c9b412c5-1f26-4ce2-a138-18fb8fb4af2a)(content(Whitespace\" \
         \"))))(Tile((id \
         ba4a9ec7-fc9c-4080-8fc8-af1d60d2dab1)(label(\"\\\"\\240\\159\\140\\191\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         ffa8377a-2f67-4f61-a6c5-3708c05574b6)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         781a0346-3e6f-4c70-b5a2-8d1832494cd9)(content(Whitespace\" \
         \"))))(Tile((id \
         34eef8d5-ff3c-434c-bdbf-77a3f96e9ba1)(label(\"\\\"\\240\\159\\141\\132\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         7db67cd9-55c6-431f-8053-6971f88e6daa)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         c79895bd-ebf9-4763-a612-b3927c2cd858)(content(Whitespace\" \
         \"))))(Tile((id \
         54948d3f-2f2d-43e6-bc99-ca6a598c7ffc)(label(\"\\\"\\226\\152\\152\\239\\184\\143\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         5826a5ad-ae31-43c5-8c13-dfb0ca92e78c)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         19f5c57e-8194-49da-8975-76988693521d)(content(Whitespace\" \
         \"))))(Tile((id \
         b3b3da01-a4b0-4eff-b2e5-0b8c3881fc9d)(label(\"\\\"\\240\\159\\140\\184\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         6dd6ec53-f9d2-4122-b46c-088f32737590)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         faefc3e6-087b-4cb1-b08c-9c067470a8c3)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         1d31a21f-e11a-4db9-a778-bb549bff990f)(content(Whitespace\"\\n\"))))(Secondary((id \
         ec96ccdc-8bdb-4808-8efa-43eb6af56cc6)(content(Whitespace\"\\n\"))))(Secondary((id \
         2170b091-923f-4022-9aa2-0c768b7882b8)(content(Comment\"# Set a \
         specific cell in the field #\"))))(Secondary((id \
         67ea4b0f-b3fc-4819-9647-a0feb4c954bf)(content(Whitespace\"\\n\"))))(Secondary((id \
         0d9e0e55-3b62-4179-85a2-1bd5b1b0d9b7)(content(Comment\"# Uses nested \
         mapi: outer loop for rows (i), inner loop for columns (j) \
         #\"))))(Secondary((id \
         5880cebe-919f-43d0-9fd4-9ba11350d93f)(content(Whitespace\"\\n\"))))(Tile((id \
         97c85837-3d4e-4bee-a9fc-85cfe790efbc)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         c78b96b9-6d97-48f3-9cc9-36720baac20d)(content(Whitespace\" \
         \"))))(Tile((id \
         bdbb74d8-ed44-49cd-a563-2e30d0b8c548)(label(setCell))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         f79ad2b0-335b-41cd-904c-b1c0b6208144)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 24))(sort Pat))((shape(Concave \
         24))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         095ddd97-212a-4896-8c07-a41420716b4b)(content(Whitespace\" \
         \"))))(Tile((id \
         63e55b0a-c5fc-491b-bc4b-67a7bbdedeb5)(label(\"(\"\")\"))(mold((out \
         Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0 1))(children(((Tile((id \
         b1211493-7444-4bdc-a447-23c1b245da48)(label(Field))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         4980afe0-bb96-4ed5-bc2b-afb2141d6c39)(label(,))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 44))(sort Typ))((shape(Concave \
         44))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         eae2a6c5-e443-456d-826f-1f029b646c48)(content(Whitespace\" \
         \"))))(Tile((id \
         c3c66a7c-d260-4fba-9de4-00e908362189)(label(Row))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         3da30505-c916-45d9-9ced-3706b55a4aa1)(label(,))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 44))(sort Typ))((shape(Concave \
         44))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         9ab4ff53-3497-42ee-9658-8ae4da0b0def)(content(Whitespace\" \
         \"))))(Tile((id \
         8fcc0609-0fbf-4478-bac0-e2c08de35d2a)(label(Col))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         667283ee-166f-4108-b9f7-b2e2fb348257)(label(,))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 44))(sort Typ))((shape(Concave \
         44))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         c1764890-fc9f-4d9a-aae5-23395b35a4ee)(content(Whitespace\" \
         \"))))(Tile((id \
         276a7b5e-5617-4948-bee4-877c4db5dac5)(label(Cell))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children()))))))))(Secondary((id \
         4da720fb-eedb-4c67-b7c6-fb83984171d9)(content(Whitespace\" \
         \"))))(Tile((id \
         468a4a02-f105-4229-b90b-a86ceb5a35c4)(label(->))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 13))(sort Typ))((shape(Concave \
         13))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         df5f64dc-74bc-49cf-bad3-c5db0f3e6dc5)(content(Whitespace\" \
         \"))))(Tile((id \
         dbd5e7db-298d-40c3-a3ee-4dcb1212bc6f)(label(Field))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         9fb1f48c-7ea8-44b1-94e2-c7a3063cd26d)(content(Whitespace\" \
         \")))))((Secondary((id \
         c7a584af-4bbe-4931-979d-17c369de619e)(content(Whitespace\"\\n\"))))(Tile((id \
         cad61720-ec02-43a6-ace4-37629cde7a9e)(label(fun ->))(mold((out \
         Exp)(in_(Pat))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         37))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         d1854dfc-29c2-43fb-8978-6a082b713768)(content(Whitespace\" \
         \"))))(Tile((id \
         2661e030-afbf-44b6-9ff0-21136f902909)(label(field))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         6cb9bb34-0f81-4096-8048-814ec20599a0)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 44))(sort Pat))((shape(Concave \
         44))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         7fd9c05a-b6ce-4a21-9ce0-c220903958ce)(content(Whitespace\" \
         \"))))(Tile((id \
         7eece0c1-f2e2-4316-9bbc-afcec3cf2f7c)(label(row))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         1044ed1a-e8ba-44ab-b1f3-67c66c2d0e3f)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 44))(sort Pat))((shape(Concave \
         44))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         19a4bc7f-b7c7-44eb-ae5b-136b0effea0b)(content(Whitespace\" \
         \"))))(Tile((id \
         cffda8e8-2d39-4837-ba24-f6fd769619f3)(label(col))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         13f89214-36a1-4bb2-9065-ee8046e2f523)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 44))(sort Pat))((shape(Concave \
         44))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         5469b934-4e16-4b31-a59a-234223aa16bd)(content(Whitespace\" \
         \"))))(Tile((id \
         192b3174-a7d2-46bf-9d28-096258186d39)(label(newCell))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         73245484-414e-477b-a898-2e5ce32476a5)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         dc858dd4-b232-4163-aa4a-466e3cf95cfc)(content(Whitespace\"\\n\"))))(Tile((id \
         43af0a1b-c300-4421-b2ea-0eebe84ade23)(label(mapi))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         dd2fe453-8026-4790-b8d8-060b6b6da5c2)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         f23060b2-7dff-4a57-896c-10fa7d1e8b99)(label(field))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         6bd3902d-16ce-43b7-8619-54bf85eb2960)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         54e5b0eb-ba6c-411c-a11d-ff264662c47e)(content(Whitespace\" \
         \"))))(Tile((id b87e50e8-5a36-4166-a624-f2651445ca8a)(label(fun \
         ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 37))(sort Exp))))))(shards(0 \
         1))(children(((Secondary((id \
         83eb4677-1ae8-44f0-aa7f-7531f433838a)(content(Whitespace\" \
         \"))))(Tile((id \
         75c97afd-de25-4b5a-8433-d68753e8bf32)(label(\"(\"\")\"))(mold((out \
         Pat)(in_(Pat))(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0 1))(children(((Tile((id \
         5a8d67ea-4739-4c19-bad9-9f7401668c7a)(label(i))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         2f1d3da6-1ed5-4305-8ea9-f27366dec8fd)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 44))(sort Pat))((shape(Concave \
         44))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         059a4ac3-2008-42d5-bc0a-86a47d243f1f)(content(Whitespace\" \
         \"))))(Tile((id \
         57f96b1c-937a-4619-b7ee-b5a7e329ba80)(label(r))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children()))))))))(Secondary((id \
         63f081ef-c4d1-48f9-9920-6bc56f8c622e)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         05434c51-cbb1-4365-8f2e-e5e8131f8aca)(content(Whitespace\"\\n\"))))(Tile((id \
         6a2134f1-8b2c-4f95-a11b-b3fd34b2c54b)(label(if then else))(mold((out \
         Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         36))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         b98b09b0-85f4-40a5-a960-8a8d19e327a6)(content(Whitespace\" \
         \"))))(Tile((id \
         6a8d3d81-a791-4111-9531-67bff1adc6d0)(label(i))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         e0095213-7e9c-40ac-86bc-6cf067fb177f)(content(Whitespace\" \
         \"))))(Tile((id \
         4025f4f7-6743-4e28-8e4a-f31eb0b4ebcf)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         7fa91eed-6cc5-4239-a90f-d4a5236d3319)(content(Whitespace\" \
         \"))))(Tile((id \
         cf85a0ab-0f4c-4498-807d-42267b7e5ff2)(label(row))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         14f4f80d-7d4f-4177-8302-63391afa6cfb)(content(Whitespace\"\\n\")))))((Secondary((id \
         a8faf95b-b6cf-4b40-904a-f91060549680)(content(Whitespace\" \
         \"))))(Tile((id \
         11beecee-d7fd-4226-9bbc-be12bdfdfb97)(label(mapi))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         daaa968e-997f-486e-9390-41f8af20df26)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         382608fd-ded1-4ef5-8f63-cf696bc3fee5)(label(r))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         819a90b0-480c-4a21-906f-529c8e7a28b1)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         1da46b19-f61d-42b0-ae0e-4a06f94c1e4d)(content(Whitespace\" \
         \"))))(Tile((id d01d926d-d948-4474-a6d0-026b33766a12)(label(fun \
         ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 37))(sort Exp))))))(shards(0 \
         1))(children(((Secondary((id \
         163e905d-770b-4097-89be-8cbc6c68f41d)(content(Whitespace\" \
         \"))))(Tile((id \
         57b3e7c9-e12b-4c06-a659-9573d2994377)(label(\"(\"\")\"))(mold((out \
         Pat)(in_(Pat))(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0 1))(children(((Tile((id \
         84fec8de-5b9e-4def-874e-f8b14ff9f805)(label(j))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         d5a6ba64-5130-40fa-b5bf-6baa3cf6848f)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 44))(sort Pat))((shape(Concave \
         44))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         60afff6f-eae9-440c-b4bd-a4d7d75c1389)(content(Whitespace\" \
         \"))))(Tile((id \
         fba2cfca-9352-4d86-bcf0-1eb47fc5fddc)(label(cell))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children()))))))))(Secondary((id \
         d5948c86-86ac-466e-93a6-4ed443736f42)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         e0b60d7f-b919-4db8-a2ca-2b4e905fe94e)(content(Whitespace\"\\n\"))))(Tile((id \
         5fc2a9ff-0c4f-4b7a-a931-85aee910a866)(label(if then else))(mold((out \
         Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         36))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         723ab2aa-6f35-4065-8200-70c09273b41c)(content(Whitespace\" \
         \"))))(Tile((id \
         e0c16aa3-5e08-4e2b-b759-b210c82c12ae)(label(j))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         243f5c29-f262-49a3-8eaf-e90aee908823)(content(Whitespace\" \
         \"))))(Tile((id \
         8363f172-7e2e-47c9-ac7f-cb3d311cc586)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         db06b06a-9643-4330-8227-81b82d3d8fb7)(content(Whitespace\" \
         \"))))(Tile((id \
         66108ded-a622-4fb5-97d6-0b87d6197085)(label(col))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         a1c51d43-a870-48f8-8198-1d25fc032bbf)(content(Whitespace\"\\n\")))))((Secondary((id \
         020d5d07-d446-487b-b60c-22c74ebde963)(content(Whitespace\" \
         \"))))(Tile((id \
         12bd2f2d-93d5-4f8d-a033-d603414e8e8b)(label(newCell))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         0c40ce56-e525-4b85-bd50-e42d3db54485)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         bf95f6da-d46b-47bd-924b-c2aec2965545)(content(Whitespace\" \
         \"))))(Tile((id \
         98c26143-0b5e-43c8-bd0a-7457b684bb5d)(label(cell))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         53c003c1-c9cd-44b3-beec-7a265b389d01)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         451ff6dd-7808-4862-895d-c7cdab5aef70)(content(Whitespace\" \
         \"))))(Tile((id \
         fd6d67b9-975a-4230-9789-d791e6e7fd97)(label(r))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         fc620084-add2-4dea-9d65-de9f22b88c63)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         f828e56f-2bf3-48b7-9d40-36ccb5fc4281)(content(Whitespace\"\\n\"))))(Secondary((id \
         2e5be387-4743-4547-aead-b674eb03cd73)(content(Whitespace\"\\n\"))))(Secondary((id \
         08972732-943e-4498-a7ec-fcdc8b6957b0)(content(Comment\"# Plant crop \
         at position, preserving the existing soil type #\"))))(Secondary((id \
         972d82df-f98e-41cb-8e17-1adacd45746a)(content(Whitespace\"\\n\"))))(Secondary((id \
         d01e1a50-8181-4ad9-9159-4998ce55150e)(content(Comment\"# This uses \
         nested mapi to find the right cell #\"))))(Secondary((id \
         fef12129-6d47-4c60-8737-80bd0d4b6665)(content(Whitespace\"\\n\"))))(Secondary((id \
         70b8c586-1f33-4f41-8773-4f97cb2597de)(content(Comment\"# Outer mapi \
         iterates rows with index i #\"))))(Secondary((id \
         003cb54e-605a-4803-a414-d267ffca264c)(content(Whitespace\"\\n\"))))(Secondary((id \
         b60cdd41-904a-4ff1-ac12-be57c90e471e)(content(Comment\"# Inner mapi \
         iterates columns with index j #\"))))(Secondary((id \
         9df9c8b2-06b2-405a-bcb5-7d1413cff170)(content(Whitespace\"\\n\"))))(Tile((id \
         0107ea8c-5060-4c1c-88c6-5df1dc9001be)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         5046e311-3ef7-4e5e-b89b-d50749e53626)(content(Whitespace\" \
         \"))))(Tile((id \
         40b99c12-77ff-4f3c-a7ac-fc0b48d9f4f5)(label(plantCrop))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         fe72de29-ad64-43d9-b7ed-e0b5d869058b)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 24))(sort Pat))((shape(Concave \
         24))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         a88e9030-b43f-444e-98d9-60cf25827e7b)(content(Whitespace\" \
         \"))))(Tile((id \
         87984021-486b-4bd9-9bea-290e7a3ece4c)(label(\"(\"\")\"))(mold((out \
         Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0 1))(children(((Tile((id \
         0cc86329-ba5f-4b98-a59a-4ad69fef51e0)(label(Field))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         92ccd624-923a-4136-b675-d3afcf58b0d5)(label(,))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 44))(sort Typ))((shape(Concave \
         44))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         8871c9b3-7c4e-4455-bdd8-30555a682e8a)(content(Whitespace\" \
         \"))))(Tile((id \
         25282150-3296-4e97-9b11-f91e1c0ef262)(label(Row))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         81d99bac-9d4c-431c-97a2-9105eeef6107)(label(,))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 44))(sort Typ))((shape(Concave \
         44))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         5d04c360-dc0b-425b-99e4-9169094905f4)(content(Whitespace\" \
         \"))))(Tile((id \
         e04a791c-876c-41e6-82c5-ce3252278016)(label(Col))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         128c9b1a-3bf0-4312-a713-7ed67182be20)(label(,))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 44))(sort Typ))((shape(Concave \
         44))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         ff179d0c-7c0b-4761-af8e-f0522745fe1a)(content(Whitespace\" \
         \"))))(Tile((id \
         16821459-242a-451e-980c-a69dbced0de1)(label(Crop))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children()))))))))(Secondary((id \
         98f462ac-99c8-47eb-8f25-9f32fb01ad13)(content(Whitespace\" \
         \"))))(Tile((id \
         6a34388e-a291-42ec-931d-d21708a87775)(label(->))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 13))(sort Typ))((shape(Concave \
         13))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         3b312de1-d0a7-4d9b-8c80-5f7d132f6016)(content(Whitespace\" \
         \"))))(Tile((id \
         0451d577-9b04-4ab0-9072-376d99608a09)(label(Field))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         0a6e48a7-f240-4922-b410-fdf982462f74)(content(Whitespace\" \
         \")))))((Secondary((id \
         a4313c45-e978-4d73-ba83-afdeba8f4d3e)(content(Whitespace\"\\n\"))))(Tile((id \
         cdfb582e-29c0-44c8-a5f4-c2e4b1d546bc)(label(fun ->))(mold((out \
         Exp)(in_(Pat))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         37))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         5719b458-3db7-4cde-b8f4-c9094a962818)(content(Whitespace\" \
         \"))))(Tile((id \
         98fbd7dc-1733-4f76-99b9-a0809dde1ced)(label(field))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         c9a08b5f-035b-41d0-98db-779179a159ff)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 44))(sort Pat))((shape(Concave \
         44))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         908d5153-abd9-49e4-91cd-bfbea27b787f)(content(Whitespace\" \
         \"))))(Tile((id \
         10f9f512-9883-4eeb-aa9c-649959c445d8)(label(row))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         faa46566-5be9-4f47-a80c-a5eb019f848b)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 44))(sort Pat))((shape(Concave \
         44))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         6092b508-1d0d-48e5-87ee-3d179aa996f1)(content(Whitespace\" \
         \"))))(Tile((id \
         7ca91353-b7ab-4fd5-ab5f-83bc5e0dae55)(label(col))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         ae59b4cd-e3e5-434f-bcc4-2814d96d4ca4)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 44))(sort Pat))((shape(Concave \
         44))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         2eb29b79-a37a-4012-81cf-d5b08febf5df)(content(Whitespace\" \
         \"))))(Tile((id \
         84675752-72c8-4df2-bdcd-84f5508c4fe1)(label(seed))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         3c4c15ce-5754-4321-9a5d-bb8ef90de1b9)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         58e23256-4a40-4099-b463-0724701281b3)(content(Whitespace\"\\n\"))))(Tile((id \
         93f841c1-72be-4c6e-82ea-405020dbb423)(label(mapi))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         67d6af33-1611-47e1-9714-c592febcaaa7)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         bce1c168-e2e7-4652-b5a4-84d789ca5f5e)(label(field))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         a0e09271-d8cf-496a-a399-8cf0348891ee)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         2848f12d-b4e0-4d17-9b03-79ca76f11fa3)(content(Whitespace\" \
         \"))))(Tile((id 785819f6-d6c6-4717-9377-73f559865a99)(label(fun \
         ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 37))(sort Exp))))))(shards(0 \
         1))(children(((Secondary((id \
         725f6929-91bf-4003-9912-426d42eb8f69)(content(Whitespace\" \
         \"))))(Tile((id \
         3788e22a-3fe7-4c39-9025-5c50ba697194)(label(\"(\"\")\"))(mold((out \
         Pat)(in_(Pat))(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0 1))(children(((Tile((id \
         dfea66a5-d735-4f4d-a17a-cf0636b36d7a)(label(i))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         fa060090-d473-40a7-8fd6-752ca1370650)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 44))(sort Pat))((shape(Concave \
         44))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         fe8e787e-450c-4155-9c2e-0039fa586ebd)(content(Whitespace\" \
         \"))))(Tile((id \
         850eedcc-f7d1-46fd-a92e-67223eaa0cf5)(label(r))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children()))))))))(Secondary((id \
         5d1a5a92-d812-4778-91a0-31ca0ed9251c)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         e7593555-a95e-42ee-8190-ecac1cdc1a5c)(content(Whitespace\"\\n\"))))(Tile((id \
         7a354e74-6c19-4191-ba02-6de84fb59a7f)(label(if then else))(mold((out \
         Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         36))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         9ee43414-a48d-4f0d-9737-fbe4622eb711)(content(Whitespace\" \
         \"))))(Tile((id \
         0f5fa79d-cab4-4fdc-9393-640a42c689ed)(label(i))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         4a878b81-e836-4237-b9ae-2ba0ea1f2b00)(content(Whitespace\" \
         \"))))(Tile((id \
         e80042d5-bc84-4888-96df-d6556495eae3)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         792708b9-01ae-4c98-a767-41ac63fb1086)(content(Whitespace\" \
         \"))))(Tile((id \
         62e1087e-6431-4767-802b-c6dae41b1251)(label(row))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         1188a70d-18bc-425c-8338-69aad409480c)(content(Whitespace\"\\n\")))))((Secondary((id \
         0a644440-da03-4141-b900-03a9db1e5169)(content(Whitespace\" \
         \"))))(Tile((id \
         6ea557bd-12be-445e-8863-815a350d8abd)(label(mapi))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         84f27df3-ad44-4f8a-b8e9-c7078c5643a6)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         5c45c198-0f5f-4aca-87d0-9162852db7fe)(label(r))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         e8eaf2a4-4b1c-4602-8e2b-3c891931ba22)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         7ee667fa-023b-4959-899d-ea37418daae7)(content(Whitespace\" \
         \"))))(Tile((id 36a8a99d-1e58-488f-bdb4-42ef432a9650)(label(fun \
         ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 37))(sort Exp))))))(shards(0 \
         1))(children(((Secondary((id \
         7831dc34-ca25-47de-a7bc-282211d671c8)(content(Whitespace\" \
         \"))))(Tile((id \
         775126fd-e97f-4632-9fc8-07142de8e013)(label(\"(\"\")\"))(mold((out \
         Pat)(in_(Pat))(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0 1))(children(((Tile((id \
         4d14815c-577d-4fc6-901b-bc5adfddaa08)(label(j))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         1b392c2c-5989-4276-973d-c4552bedc55e)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 44))(sort Pat))((shape(Concave \
         44))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         b56ee8bf-03ba-4a0f-a980-0bce5e3677ee)(content(Whitespace\" \
         \"))))(Tile((id \
         9d4ab153-f539-497c-bda8-34caa9fce81e)(label(cell))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children()))))))))(Secondary((id \
         4f0e06f7-4cb1-4469-9018-03b2f5cb89df)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         fd361d45-ef83-46d2-bf01-497b09cbb2e2)(content(Whitespace\"\\n\"))))(Tile((id \
         d5d09bbe-972c-4c0d-8651-e10150734188)(label(if then else))(mold((out \
         Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         36))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         a51aa263-c3ad-4c06-80fb-af5c05775dfd)(content(Whitespace\" \
         \"))))(Tile((id \
         d1733956-0033-439a-9b02-fc8cf680a8a8)(label(i))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         2e7a3c77-1f11-47c6-abad-3ea1c8ab5d7a)(content(Whitespace\" \
         \"))))(Tile((id \
         4c4266a9-0bec-4f65-bf41-4c5205854ec3)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         a09f3fe8-1b48-49eb-ae65-5fe457598d13)(content(Whitespace\" \
         \"))))(Tile((id \
         02842658-d0ac-4f39-b9a4-8de348e480fd)(label(col))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         e72e122c-84ad-4338-b636-b5a36c68c924)(content(Whitespace\"\\n\")))))((Secondary((id \
         fc4f239e-d142-4bba-9a35-19bbb38e48b9)(content(Whitespace\" \
         \"))))(Tile((id \
         d036379e-1038-4ce8-a8b6-c071176c3fc5)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         4ca5a818-3074-4501-bdc5-2905fab8a101)(label(crop))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         3134604e-33f0-43cc-b99a-4f748beeed8e)(content(Whitespace\" \
         \"))))(Tile((id \
         46373530-a4da-4505-a159-a7251f26a52c)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         6896cbf6-99b1-48a0-afac-16065fd780f2)(content(Whitespace\" \
         \"))))(Tile((id \
         bfe5864b-17a5-4a4a-882f-0b0fb728c0e0)(label(seed))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         c78de3d9-352a-4d2e-b663-e8e1f05187d7)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         f048ab7c-0b68-4c30-8f7e-8e2c5e1117cd)(content(Whitespace\" \
         \"))))(Tile((id \
         e36326c3-3793-4b37-8570-fc2ddeb93ca5)(label(soil))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         40e594cc-7ff3-4120-95c9-f5dbcebc46ae)(content(Whitespace\" \
         \"))))(Tile((id \
         01029912-daf4-44ee-a176-3afd5a0ad6bb)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         0114379e-5b39-4f23-81bf-c8764426c118)(content(Whitespace\" \
         \"))))(Tile((id \
         85fab1ab-0a1b-4beb-884a-dc2e098dd450)(label(cell))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         d14a33ca-9ca7-4706-870f-5671a7261caa)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         0dddd098-bd9c-46b0-97e9-59b1c0026d44)(label(soil))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         f59fb156-d779-4067-beac-56636547cb9a)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         ad02551a-6b16-44cc-a63b-69832f71b5f4)(content(Whitespace\" \
         \"))))(Tile((id \
         67ab2b74-533b-4fc2-bb38-483d969538fa)(label(cell))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         4bff13a9-d2ad-4e49-a277-61cbe06d4593)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         954f3c6c-9efb-4f45-9084-b35d13479e2c)(content(Whitespace\" \
         \"))))(Tile((id \
         f89bab75-a256-49b2-95ef-238c38c6add8)(label(r))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         83fdff33-6b74-4a8b-9fdc-835c61a9cd3c)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         ae7e4083-8982-4a19-b50f-6c1cedd4f7b3)(content(Whitespace\"\\n\"))))(Secondary((id \
         ecec3270-aafa-479e-a37d-0b46efe77da6)(content(Whitespace\"\\n\"))))(Secondary((id \
         09d7438c-113f-4fec-9132-99417813e4ca)(content(Comment\"# Remove crop \
         at position, preserving soil type #\"))))(Secondary((id \
         222dabe4-444a-4a45-a98f-bc75b91a9374)(content(Whitespace\"\\n\"))))(Tile((id \
         35131366-c5b1-46e3-8094-9e881164eeab)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         5ab77868-1c25-4fc9-b5b6-0d0b98e298b3)(content(Whitespace\" \
         \"))))(Tile((id \
         5d2db3e8-ed76-4a22-9c2f-901d22766a24)(label(harvestCrop))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         0f930afb-d56e-4afd-bb02-3e105d21cb8d)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 24))(sort Pat))((shape(Concave \
         24))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         1c49c917-aebd-4f82-b12b-dc78a285cade)(content(Whitespace\" \
         \"))))(Tile((id \
         875758c2-4a9b-4161-b5f6-0851d0fe0861)(label(\"(\"\")\"))(mold((out \
         Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0 1))(children(((Tile((id \
         64682bf5-d505-478a-a48d-42ce26416b20)(label(Field))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         8bccd07c-cf9f-4f16-8afd-63864bb933ae)(label(,))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 44))(sort Typ))((shape(Concave \
         44))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         31bf4a58-4b3d-4dea-9cc0-6b1892d839a5)(content(Whitespace\" \
         \"))))(Tile((id \
         cb6fda6d-2a5b-48ce-b07a-9226de50ed26)(label(Row))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         70e89e4c-50d4-4484-8d73-4ba7d85fc61a)(label(,))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 44))(sort Typ))((shape(Concave \
         44))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         55d4bed7-b87b-4c4a-bc16-71a4493a442a)(content(Whitespace\" \
         \"))))(Tile((id \
         6e2a8b01-0b9f-49db-b257-35a1d606ccb8)(label(Col))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children()))))))))(Secondary((id \
         2b965be5-b55c-4b37-b2e6-f465e8ea5857)(content(Whitespace\" \
         \"))))(Tile((id \
         7d951c49-a177-4f7d-bb01-fabac8a7d945)(label(->))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 13))(sort Typ))((shape(Concave \
         13))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         0fba0491-d048-412b-bbbe-9ae6a7edfd17)(content(Whitespace\" \
         \"))))(Tile((id \
         71742456-af39-4934-8104-0183f90ec5d7)(label(Field))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         4999b22d-3d29-485b-a8ea-77a34a4ac084)(content(Whitespace\" \
         \")))))((Secondary((id \
         59caf796-1e96-404b-9230-5f2d73b1389f)(content(Whitespace\"\\n\"))))(Tile((id \
         0a0e53ef-2c76-467f-97a2-f92f52c5f383)(label(fun ->))(mold((out \
         Exp)(in_(Pat))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         37))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         a6702d93-ee64-4089-a2f8-480664554100)(content(Whitespace\" \
         \"))))(Tile((id \
         61db7e9b-d8b8-4e0e-bce9-4a67ffd9d100)(label(field))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         bfc19039-310e-43ff-ac9e-b19d4e2cbd9d)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 44))(sort Pat))((shape(Concave \
         44))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         4e4e9fc9-0f19-4c63-97fe-fdc8d6635c7e)(content(Whitespace\" \
         \"))))(Tile((id \
         5aab9ea9-5fbb-410d-97a0-ba29b474b582)(label(row))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         5156e572-ed2a-48a9-af1c-47cbf1c38e3b)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 44))(sort Pat))((shape(Concave \
         44))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         c50f4093-dba9-4e0b-8d48-edb8e421d5aa)(content(Whitespace\" \
         \"))))(Tile((id \
         632c010b-5121-46ef-8edd-8608f914dbf7)(label(col))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         c49b4113-3ef1-4c49-a0a2-40fa13bda644)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         7c14839a-0097-4635-a2b1-9f451d056895)(content(Whitespace\"\\n\"))))(Tile((id \
         d8660f92-32d6-43c2-9ca4-8e14cd6acb9f)(label(mapi))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         ec7a4bb1-7ab2-4a41-85d8-0d15189de5c2)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         001c68ac-6b4a-4445-abdc-377c590b7b2d)(label(field))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         2e27f626-0249-46cb-96d4-8a93af62a5aa)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         910f54d6-9767-4b72-b62e-c7bcb4116150)(content(Whitespace\" \
         \"))))(Tile((id a320ca94-b1d9-42e3-a152-0d5622778b23)(label(fun \
         ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 37))(sort Exp))))))(shards(0 \
         1))(children(((Secondary((id \
         938c0ba1-a14d-442d-85b7-9b4e1b3b822c)(content(Whitespace\" \
         \"))))(Tile((id \
         6203fbe3-6d6c-4df4-87a4-0fed25b341aa)(label(\"(\"\")\"))(mold((out \
         Pat)(in_(Pat))(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0 1))(children(((Tile((id \
         085db918-b6c2-4ea6-baff-36d49ea7afc1)(label(i))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         820ce6eb-05f3-4fba-b8e8-7d4f9b968745)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 44))(sort Pat))((shape(Concave \
         44))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         6a9440ec-2387-47a7-b855-1d8cba19fc54)(content(Whitespace\" \
         \"))))(Tile((id \
         484a6980-63b5-4d29-80ed-9071e4384db7)(label(r))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children()))))))))(Secondary((id \
         b1f868bc-c039-49be-9fbc-9a2490dc4a07)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         ade2437c-86d8-48b0-84e7-9b6dd5a268c5)(content(Whitespace\"\\n\"))))(Tile((id \
         09dece82-6353-41e5-82c2-7135b382e657)(label(if then else))(mold((out \
         Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         36))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         5a0a9b8d-9641-4f9f-be4f-ca737a0aae8e)(content(Whitespace\" \
         \"))))(Tile((id \
         104c5de6-bf25-4bf5-ac83-64a448f95e8d)(label(i))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         3ff4c8b8-5573-4390-97c8-0f139c590682)(content(Whitespace\" \
         \"))))(Tile((id \
         a3f1258a-f81f-446f-8cef-c6b7a56f9183)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         3dc6d8ff-b6d9-4aa9-b932-23f706c5195c)(content(Whitespace\" \
         \"))))(Tile((id \
         3feecf08-369d-4eba-99f7-ae12b9816f80)(label(row))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         95d6a6fa-cb83-4c04-9ada-8236a484106e)(content(Whitespace\"\\n\")))))((Secondary((id \
         42766450-6a97-4a1c-9556-dbd69d371e8e)(content(Whitespace\" \
         \"))))(Tile((id \
         70bf80e9-cd83-40f7-bac5-8b42472ef5f6)(label(mapi))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         aab78506-ab21-43ab-a6c5-42d42f42e57a)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         5a65a635-c454-41f9-b7c8-15869e4c8753)(label(r))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         59b7d62a-31e8-4e25-a4e3-1028039e544c)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         b37140d5-344b-4382-ba93-f964ba906ce1)(content(Whitespace\" \
         \"))))(Tile((id 59dfe099-97b4-48cc-a124-761203965a89)(label(fun \
         ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 37))(sort Exp))))))(shards(0 \
         1))(children(((Secondary((id \
         f002b29d-a0f2-4c1c-9d94-18778d0958e9)(content(Whitespace\" \
         \"))))(Tile((id \
         74701064-b484-4508-aa9b-afe4cf3d02fa)(label(\"(\"\")\"))(mold((out \
         Pat)(in_(Pat))(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0 1))(children(((Tile((id \
         b72a4245-34ed-4298-a9cb-a1aa29d26c1f)(label(j))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         dfab4193-b31d-4580-9246-9397ca99b6de)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 44))(sort Pat))((shape(Concave \
         44))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         f4e31f01-50dc-433d-af53-5116888a4087)(content(Whitespace\" \
         \"))))(Tile((id \
         2cb1014a-8baa-473e-9486-47e3855b220d)(label(cell))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children()))))))))(Secondary((id \
         b8c39966-e2ee-403d-8991-35c8f9df2379)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         b69df1cf-260a-45a1-8948-56b1547c4407)(content(Whitespace\"\\n\"))))(Tile((id \
         c9ddc0f2-7b19-4d47-8eb7-98dd85399fc1)(label(if then else))(mold((out \
         Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         36))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         b34e17ae-be81-4245-bc39-6952cdf1f81a)(content(Whitespace\" \
         \"))))(Tile((id \
         dbb56e53-6db9-410f-a436-974a505034a1)(label(j))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         8e20ee7a-740c-4f2a-96be-095a819d9297)(content(Whitespace\" \
         \"))))(Tile((id \
         d9b99735-6d66-49be-97af-d166a48640ef)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         fd479f6d-9a37-4866-b6e3-c65094b87f94)(content(Whitespace\" \
         \"))))(Tile((id \
         789d0bb8-ff6c-483a-a778-976da579d88a)(label(col))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         1c45faac-8e89-40ed-a035-14ab519aa8c4)(content(Whitespace\"\\n\")))))((Secondary((id \
         879d0e4d-d257-4acf-b527-35d68bfdb905)(content(Whitespace\" \
         \"))))(Tile((id \
         75e08a3f-68ea-4be9-b70f-9bc5f938d817)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         0fc687fa-ddec-452e-84ce-d033129e1bf0)(label(crop))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         d5c6fa59-480c-4576-bc12-02440d7fb902)(content(Whitespace\" \
         \"))))(Tile((id \
         056703a7-c96f-4bd7-92c7-f146a74e3878)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         52809172-3f7e-4a87-b77a-0f1ef08ba7db)(content(Whitespace\" \
         \"))))(Tile((id \
         f7722156-c4b3-4af1-aaf6-5fbad29b39d0)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         4086abdd-ab83-44bb-908f-00bf955b71b9)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         94f3ca74-9a38-464c-851b-d2c744a36ea9)(content(Whitespace\" \
         \"))))(Tile((id \
         c48cd620-f72b-4ba2-86c3-e5898817c3e3)(label(soil))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         76a1ada4-3a25-48e5-9cac-9ae1cbc7a729)(content(Whitespace\" \
         \"))))(Tile((id \
         ce0cc0d7-2958-4647-951d-9f109d2482ea)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         4016a74e-770c-4026-8126-eca0757a6a66)(content(Whitespace\" \
         \"))))(Tile((id \
         57cbc067-b70e-487a-8db7-7a1529ca0f49)(label(cell))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         ce21506a-4170-47e5-bdb0-ff55ec38f302)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         4552101a-a01f-4914-8965-ed787bba77fb)(label(soil))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         91e3273a-af8d-437e-9043-d1f77d7f10a8)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         ae815fc3-7c8b-4a5e-9a81-7b965cb9c060)(content(Whitespace\" \
         \"))))(Tile((id \
         d31982bd-271b-492e-92fa-df131d30494e)(label(cell))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         ca6bacda-640e-4af4-88c1-e3b3ed87516c)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         2eeedd55-f627-461d-93d5-df18454907cd)(content(Whitespace\" \
         \"))))(Tile((id \
         5a0b4909-4910-4ea1-98b7-e5b6ff7e0963)(label(r))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         71a4045d-228c-4de7-a99e-c9a4d4fd7467)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         9f6d14de-5f8e-46f3-bd2c-fe4406a524f6)(content(Whitespace\"\\n\"))))(Secondary((id \
         555c83dc-deae-47e6-891f-d382692472bb)(content(Whitespace\"\\n\"))))(Secondary((id \
         8cd462e5-b2fd-4702-8e6d-c195f760b8eb)(content(Comment\"# Change soil \
         type at position, preserving crop #\"))))(Secondary((id \
         7e92cf05-81e8-4a24-a718-4934c6cb98bc)(content(Whitespace\"\\n\"))))(Tile((id \
         35d145d1-7443-4da5-ba3c-80e8c7b3cd6c)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         ee4a2015-60d7-4d06-be8c-9d607a2ae475)(content(Whitespace\" \
         \"))))(Tile((id \
         69b36c7f-3162-44c0-8cb6-d10a0e59fb98)(label(tillSoil))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         ead50281-9bc3-4af0-8c82-904b970020a4)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 24))(sort Pat))((shape(Concave \
         24))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         86e7f5d0-11ca-4765-8e4e-2cb694789930)(content(Whitespace\" \
         \"))))(Tile((id \
         41e6f6b1-3635-4577-9f04-3b638275a749)(label(\"(\"\")\"))(mold((out \
         Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0 1))(children(((Tile((id \
         1ec45040-0467-43ac-9549-66c3bf3faf7b)(label(Field))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         cea10c7a-5444-4d60-b6df-43baa9cb15dc)(label(,))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 44))(sort Typ))((shape(Concave \
         44))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         ac42b28e-b8f9-4c9b-94fc-c9986788d62a)(content(Whitespace\" \
         \"))))(Tile((id \
         be2eb77f-66e2-43ee-ae7c-c0c4f1dcb122)(label(Row))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         e5b81925-9710-4a48-8210-d229369ac817)(label(,))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 44))(sort Typ))((shape(Concave \
         44))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         15f52702-c184-45a3-8b14-c831bb1ff5a6)(content(Whitespace\" \
         \"))))(Tile((id \
         18f41abd-add9-4d89-9d67-e19d10317255)(label(Col))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         0ef74f3d-1502-4009-b003-98d915fd886b)(label(,))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 44))(sort Typ))((shape(Concave \
         44))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         2b9fc748-e83c-4e64-bfb4-b10fd8466f78)(content(Whitespace\" \
         \"))))(Tile((id \
         42f874dd-cfbd-4cd8-8d11-82650d56f66b)(label(SoilType))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children()))))))))(Secondary((id \
         3c462b0b-dce2-4026-b0db-93cd685ec600)(content(Whitespace\" \
         \"))))(Tile((id \
         74405a08-66bc-4631-82b3-6f5b1dc41673)(label(->))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 13))(sort Typ))((shape(Concave \
         13))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         c541a6d4-7dbf-452d-b1ef-4fa68e33e1e6)(content(Whitespace\" \
         \"))))(Tile((id \
         d380a1c6-118c-46db-87d1-703297dddd05)(label(Field))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         9f83a0d8-c6c5-44b1-98fb-60a1239d2110)(content(Whitespace\" \
         \")))))((Secondary((id \
         1068d67d-165f-4fc1-a7e2-7f388727f437)(content(Whitespace\"\\n\"))))(Tile((id \
         c25d65d1-11e5-4c4f-902c-fa6a60562807)(label(fun ->))(mold((out \
         Exp)(in_(Pat))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         37))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         095cf16d-fc5d-4057-af3e-25f62d6cdd18)(content(Whitespace\" \
         \"))))(Tile((id \
         725f42ea-e29a-41be-bd52-ee7f5918c350)(label(field))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         640d1ae7-6a44-476d-8edf-25afce3d356d)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 44))(sort Pat))((shape(Concave \
         44))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         2ea26fce-8245-4492-b701-8e13cdbbf6fd)(content(Whitespace\" \
         \"))))(Tile((id \
         996e258c-3dee-49eb-959f-7f3ffc7a4b0e)(label(row))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         bf79090b-838f-4885-ab15-2fa9a51177ec)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 44))(sort Pat))((shape(Concave \
         44))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         3ddc5b2f-d19c-4a46-ae95-e860b535dee3)(content(Whitespace\" \
         \"))))(Tile((id \
         7ea43287-78d3-4355-ac44-8691d75a58ca)(label(col))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         258fe320-5318-45a3-9fb4-ba19abcf00bb)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 44))(sort Pat))((shape(Concave \
         44))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         c63ef05a-d5ea-4dc3-a4b9-fe3dfee9e516)(content(Whitespace\" \
         \"))))(Tile((id \
         22c49aa6-4c4e-476a-8c09-ae0336964076)(label(newSoil))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         0a6094c4-3d39-4b8b-83a0-5c3034417fde)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         9c183824-fbc4-478d-bd9e-42eeda6d8c26)(content(Whitespace\"\\n\"))))(Tile((id \
         0c85301a-0e99-431d-b12f-761cb489b3b9)(label(mapi))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         16b81ca2-ead7-439e-aea9-dc3d156e669d)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         82303d17-1c05-4918-8ef2-cd1899a77e89)(label(field))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         4161aa55-35f2-45ba-b978-c7c739a1b489)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         cfded7e4-2897-4541-8fd6-45c44ac86e68)(content(Whitespace\" \
         \"))))(Tile((id 0f6a3703-fd7d-490d-8674-a574565feb65)(label(fun \
         ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 37))(sort Exp))))))(shards(0 \
         1))(children(((Secondary((id \
         1946b441-5037-4da3-8e55-fe6e8fc17c95)(content(Whitespace\" \
         \"))))(Tile((id \
         b08f0ea1-244b-4f27-aa43-957b4127ceda)(label(\"(\"\")\"))(mold((out \
         Pat)(in_(Pat))(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0 1))(children(((Tile((id \
         12535aeb-2cc1-40fe-9725-abaab5758c15)(label(i))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         2b7e7ddf-c365-4eba-92fb-868a8fd4999f)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 44))(sort Pat))((shape(Concave \
         44))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         a9431504-b798-44b3-8e5a-2d072400e247)(content(Whitespace\" \
         \"))))(Tile((id \
         7b31f299-3039-4d0c-a7d4-e42d3b7f495c)(label(r))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children()))))))))(Secondary((id \
         a66e6de1-01a2-457d-807f-1d50880bfd40)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         151d4b22-3eea-4c68-b38b-63a9f0f09df7)(content(Whitespace\"\\n\"))))(Tile((id \
         f2f9999a-4b1a-45d0-acb8-611037d83820)(label(if then else))(mold((out \
         Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         36))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         541b3899-15e4-4c4f-b2ab-aef75fe3c414)(content(Whitespace\" \
         \"))))(Tile((id \
         e979a8e8-4afb-48d1-b5d8-1fbe7ecfb870)(label(i))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         c306aca6-d889-46ce-ba05-83e80e1dd846)(content(Whitespace\" \
         \"))))(Tile((id \
         c0f12325-1937-4f00-ba95-358c218749cb)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         d9d6db31-c4e3-47ae-b8e6-4335a622f8ca)(content(Whitespace\" \
         \"))))(Tile((id \
         e586004f-3b2a-4c25-b44a-15ede8176929)(label(row))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         fed8c7b5-3415-4711-ba82-196694032edd)(content(Whitespace\"\\n\")))))((Secondary((id \
         0bbb9e92-d3f1-475b-9865-b9540aa4ad32)(content(Whitespace\" \
         \"))))(Tile((id \
         88b93761-ff0a-4a38-8922-7ab7fba30f0a)(label(mapi))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         45f97e86-389f-4f42-acee-5825caef641b)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         eccb2c9f-6c63-40a4-bae5-85a7ab097605)(label(r))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         34aea337-88ea-418a-a1ce-2622a65fa99f)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         0dbc589a-2b37-499e-bfdb-ef21f042a599)(content(Whitespace\" \
         \"))))(Tile((id e3eb3069-adfe-4369-a3a0-6ef8d438a0d3)(label(fun \
         ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 37))(sort Exp))))))(shards(0 \
         1))(children(((Secondary((id \
         3ad4cd3e-71aa-4545-bda2-749c66555077)(content(Whitespace\" \
         \"))))(Tile((id \
         697802cc-2644-4880-be75-45297d3181d6)(label(\"(\"\")\"))(mold((out \
         Pat)(in_(Pat))(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0 1))(children(((Tile((id \
         f271daf5-bb7c-4600-a1ea-9dd3f7e22496)(label(j))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         938a4006-bd1c-4370-b83b-e9ae14aba75f)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 44))(sort Pat))((shape(Concave \
         44))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         039c89c7-98f4-483f-8c16-2cdb68936f97)(content(Whitespace\" \
         \"))))(Tile((id \
         d55448e8-742a-4aaa-80a3-604e5b99c99c)(label(cell))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children()))))))))(Secondary((id \
         05484dfd-a4a1-492d-83e6-b0b9fefb2aba)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         98a9728b-8d03-4ff5-ad5e-e77879cdd995)(content(Whitespace\"\\n\"))))(Tile((id \
         5cdc3aad-98bd-461d-9d79-dce76ca480f0)(label(if then else))(mold((out \
         Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         36))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         cc8de349-0ed8-46a5-8a95-225aa6a2f3b6)(content(Whitespace\" \
         \"))))(Tile((id \
         d2d471cb-b5c2-4dcd-b349-a4b035611b96)(label(j))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         b0cd3ba8-7c2e-402c-a498-4b1eae54b768)(content(Whitespace\" \
         \"))))(Tile((id \
         1849d49a-751c-4fd5-b97e-3b359ebf5e86)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         464d467b-523f-4d24-b449-da2ef9750817)(content(Whitespace\" \
         \"))))(Tile((id \
         95b0d663-4d67-450d-970c-2d1f631d9adc)(label(col))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         596212b7-2ab7-4f0e-88e5-fcb60050c24c)(content(Whitespace\"\\n\")))))((Secondary((id \
         b42b6965-a4f5-4b4d-9540-123d6302c1ac)(content(Whitespace\" \
         \"))))(Tile((id \
         05caae7b-f9dd-48b4-956b-121944af09cd)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         91b93922-2154-4bd6-9272-1417c6c14613)(label(crop))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         856d7ef2-88ba-440a-a443-36c4cf7512b1)(content(Whitespace\" \
         \"))))(Tile((id \
         6762fdc3-1399-43a5-91f5-ba908f3cbcc5)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         08476dd2-d3fa-4bd7-82fe-4626303608ef)(content(Whitespace\" \
         \"))))(Tile((id \
         83c497cb-1e00-470b-8feb-cca89f242917)(label(cell))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         129ff599-dffe-4ffa-8d2f-99928db416fd)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         81b700cf-282e-4aaa-8360-54cce2a73b3b)(label(crop))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         9c2f6104-87c1-4c62-9dea-4ace27af6204)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         f33b2206-7d18-4db6-8060-63550ca7f272)(content(Whitespace\" \
         \"))))(Tile((id \
         593b320c-8245-44ff-8832-443fbc870ed8)(label(soil))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         a9356b6d-1747-49d2-9193-5ba8b4b5fab7)(content(Whitespace\" \
         \"))))(Tile((id \
         8be2c95d-9650-4142-8664-2f34cc3f8e67)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         dd02c4a1-6cf6-4e0f-98e6-1fb7b470d3e6)(content(Whitespace\" \
         \"))))(Tile((id \
         6a7a4ab2-d7c9-4fd6-b2c5-bac5ddb15b87)(label(newSoil))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         866e4c5c-d700-41c9-b76b-787434ee4402)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         fbbb9dfe-f84c-4cdd-9638-bc6e0b72f55a)(content(Whitespace\" \
         \"))))(Tile((id \
         9d86bb69-587d-417c-810e-9e9634c77a43)(label(cell))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         bd6ddb82-5d52-4702-9094-5e8cf89a38a9)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         0cb47564-29e4-42ce-a883-19473317d8ff)(content(Whitespace\" \
         \"))))(Tile((id \
         d3dc2973-f831-4deb-8667-e6a5a6e4ddc3)(label(r))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         bc958ebe-0a90-4733-9a5d-722f53f54830)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         82a055fb-e865-40b9-9177-ba899a103964)(content(Whitespace\"\\n\"))))(Secondary((id \
         f970d718-9126-46e1-b798-4b0f91f80869)(content(Whitespace\"\\n\"))))(Secondary((id \
         39cc8113-0e79-4145-9f86-f47aadbf4b12)(content(Comment\"# Plant entire \
         row with seed #\"))))(Secondary((id \
         66e3f384-54a0-4af2-a2a5-3b03dc30eea6)(content(Whitespace\"\\n\"))))(Tile((id \
         aa9801b9-b3fd-4ef9-bbff-45e263488415)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         9604d397-e000-4158-bc93-d83f8a8cd7be)(content(Whitespace\" \
         \"))))(Tile((id \
         de2c508b-38ab-48ff-affe-c8248f59a8f4)(label(plantRow))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         b2ffdc24-4ae0-4130-be94-3e4a8dae04b0)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 24))(sort Pat))((shape(Concave \
         24))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         f0fd9069-35aa-496a-9553-4d3176cc67b4)(content(Whitespace\" \
         \"))))(Tile((id \
         7cabf473-f74b-408e-addf-d990211c6da5)(label(\"(\"\")\"))(mold((out \
         Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0 1))(children(((Tile((id \
         36278eca-fa5f-49c8-83aa-3bb59f038e97)(label(Field))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         5fb4bf0e-7d65-458d-b4e0-c90ae103024f)(label(,))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 44))(sort Typ))((shape(Concave \
         44))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         b3d193b5-1223-45c0-b15b-baf4ce02d2ed)(content(Whitespace\" \
         \"))))(Tile((id \
         b41edf57-b36a-432d-a635-82d95cb78030)(label(Row))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         31045006-f04b-47e6-ad56-bb1897fa950d)(label(,))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 44))(sort Typ))((shape(Concave \
         44))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         f3d368a6-e0ec-4b8c-8904-1e13236742c8)(content(Whitespace\" \
         \"))))(Tile((id \
         4b2bccf7-f566-4b68-a4b7-e3722b2c25e4)(label(Crop))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children()))))))))(Secondary((id \
         4c793e18-e9d6-434f-9bee-a055274a7615)(content(Whitespace\" \
         \"))))(Tile((id \
         9e51ef50-3970-47da-a9bd-ff6eb061222a)(label(->))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 13))(sort Typ))((shape(Concave \
         13))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         4dad75eb-574e-4df6-8a86-4b1b9701d807)(content(Whitespace\" \
         \"))))(Tile((id \
         26d29498-78e2-4b20-8d27-528449c6e01e)(label(Field))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         dd84e667-7dad-4bbf-8b4e-6555bf4694a8)(content(Whitespace\" \
         \")))))((Secondary((id \
         335923e1-f3a1-40ea-a5e6-b39e317ef103)(content(Whitespace\"\\n\"))))(Tile((id \
         9d9ee139-2df3-4344-9a13-05b93445c8be)(label(fun ->))(mold((out \
         Exp)(in_(Pat))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         37))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         900fbedd-bf24-467d-9c7e-47c73a836b0b)(content(Whitespace\" \
         \"))))(Tile((id \
         8644b539-fdc3-4af9-94de-42809dc0452a)(label(field))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         b0a75855-d563-41a5-8178-1da5be6c19f8)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 44))(sort Pat))((shape(Concave \
         44))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         31adbfb7-0606-4623-a841-bf0bce2b3cbd)(content(Whitespace\" \
         \"))))(Tile((id \
         d0bbafc7-9cd1-431e-ad91-f76016a15272)(label(targetRow))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         3fa27ec4-9dca-4398-94be-91ca35227e39)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 44))(sort Pat))((shape(Concave \
         44))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         588bde20-ca2f-4780-97d4-fe87bbd9dd6c)(content(Whitespace\" \
         \"))))(Tile((id \
         ed124058-f5b8-4989-a32c-fa9a27c2d06c)(label(seed))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         6d45d693-c79c-4dae-a3ee-91b6ad55a279)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         d95e1d09-e999-4338-99f9-f96d31b62969)(content(Whitespace\"\\n\"))))(Tile((id \
         73b785e7-800e-42ee-9e0d-52505ab2b4d8)(label(mapi))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         9068fa24-66d5-48c7-843f-3d055b472409)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         71bad997-9e1d-4269-9ca2-0311f2d3c268)(label(field))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         7eea56da-7a4c-4006-8e56-244bd7345ea4)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         90d87644-794e-4d7d-9a04-ea6e8ab155c0)(content(Whitespace\" \
         \"))))(Tile((id c8a58074-59d4-4d9c-b76e-5ed8f4839b36)(label(fun \
         ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 37))(sort Exp))))))(shards(0 \
         1))(children(((Secondary((id \
         d2f09372-4999-44a1-b3c0-dbda1408fe97)(content(Whitespace\" \
         \"))))(Tile((id \
         0dd2a6a6-a188-432a-814e-f5b033db0188)(label(\"(\"\")\"))(mold((out \
         Pat)(in_(Pat))(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0 1))(children(((Tile((id \
         4ba9483f-8dca-492c-b9a0-11d743127241)(label(i))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         be89bda8-72bd-43bb-827a-5e4251dc0455)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 44))(sort Pat))((shape(Concave \
         44))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         e59b0e6b-2877-407f-a776-fa38cdffe57c)(content(Whitespace\" \
         \"))))(Tile((id \
         7fa373e3-5fc4-4b44-99e3-685a57c2dacd)(label(row))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children()))))))))(Secondary((id \
         adcafe18-a2b0-4544-a9c4-83f149679467)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         840cfac4-fdb3-41e0-8867-2a206c662b06)(content(Whitespace\"\\n\"))))(Tile((id \
         50717700-3170-4acf-9268-3ea58949ed65)(label(if then else))(mold((out \
         Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         36))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         9c6034e9-67a1-4d26-9c6a-b724c568d039)(content(Whitespace\" \
         \"))))(Tile((id \
         0a0ac6ca-fb88-4390-970a-ec206f790607)(label(i))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         2eceae24-add6-48cb-b787-742fa5d52ca3)(content(Whitespace\" \
         \"))))(Tile((id \
         75b61dd2-1a99-4892-b499-20900e0e4dea)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         4cb3a44c-29c0-473a-83d4-87d90527f08c)(content(Whitespace\" \
         \"))))(Tile((id \
         15a350e4-a367-4ace-9e48-71c01e095456)(label(targetRow))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         c3364682-ad1c-472b-b503-39a9dbe68209)(content(Whitespace\"\\n\")))))((Secondary((id \
         f6e00180-f3a8-458e-8159-5455bf382342)(content(Whitespace\" \
         \"))))(Tile((id \
         3af8b129-b9b5-4452-a260-d947a7843423)(label(map))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         dcf33b4a-95b1-4f83-8196-d8ce6aa62568)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         5aa5a076-3f62-49ca-855f-2afaa0663d53)(label(row))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         d98ba7a9-58d7-4075-988a-7c26486856a7)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         e0d52e03-6786-493d-820f-927dc48c4b1f)(content(Whitespace\" \
         \"))))(Tile((id e50f3501-4360-4d49-8041-27e0d679afec)(label(fun \
         ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 37))(sort Exp))))))(shards(0 \
         1))(children(((Secondary((id \
         84cfd3b6-302e-48e0-81c7-4af1242905d6)(content(Whitespace\" \
         \"))))(Tile((id \
         b624d403-ec9c-4147-83a2-8edc34899865)(label(cell))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         0427d313-e1b5-44fa-8b6a-f5d2fa6a3e68)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         5b2b1b62-cd9a-44ae-bec0-18b76f4a3aef)(content(Whitespace\" \
         \"))))(Tile((id \
         bf5b6251-72e9-4ca6-8135-e0b0b06a1006)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         5596a0e2-bd07-422c-a041-52bcf4d13c51)(label(crop))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         91372409-ddb1-4315-aa0b-012f3f003f52)(content(Whitespace\" \
         \"))))(Tile((id \
         bf7e7360-eaa4-44a3-a60a-97a384089ce7)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         e1bf5dd2-d629-4b09-af57-60d5804a2de7)(content(Whitespace\" \
         \"))))(Tile((id \
         79b49775-99b0-45cb-9089-b9d02c9d1fa6)(label(seed))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         4b7beede-4998-4ee4-b421-ffed533a0316)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         271a7550-7400-4617-b058-e0adeccdd754)(content(Whitespace\" \
         \"))))(Tile((id \
         4da3e48d-9527-4e9b-ab4a-7ce21e7489c7)(label(soil))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         c3fd1b73-2ea8-422f-ac28-8e206ee6de83)(content(Whitespace\" \
         \"))))(Tile((id \
         84efcc08-b10e-4bdc-9b0a-9d6aec89b841)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         82d58c03-c80d-4784-940a-b9e61a4f83f9)(content(Whitespace\" \
         \"))))(Tile((id \
         c2635e93-b09c-4c0e-a98f-57c906ed3c84)(label(cell))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         35d7bc01-6983-4126-abb3-ae8c1a03456d)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         5d28f4ab-8eae-4a02-8881-9e262d689377)(label(soil))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))))))))))))(Secondary((id \
         6865ddf8-21aa-4e26-ba69-8cbd6a698a7e)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         600d52bb-dfa4-4335-9b0e-abe2ac691446)(content(Whitespace\" \
         \"))))(Tile((id \
         b570a66e-79dc-42a5-b8eb-c2dcde6fd5b6)(label(row))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         1beef86e-c92a-4b7c-8b40-208d4f348407)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         c7407ee1-f2a7-4147-98d3-bbe0ca3108ec)(content(Whitespace\"\\n\"))))(Secondary((id \
         3c2ef874-d3df-4aad-bca7-069deca36f81)(content(Whitespace\"\\n\"))))(Secondary((id \
         7db01896-90a9-444c-990d-65dcd09d9946)(content(Comment\"# Clear all \
         crops from field #\"))))(Secondary((id \
         dd8a9aad-51b7-44ab-a79e-6e420ff1ba16)(content(Whitespace\"\\n\"))))(Tile((id \
         2c122a8d-2f4b-4fa6-8092-2ddee48fd933)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         59128c56-70f2-452e-a7a1-a2462273a11b)(content(Whitespace\" \
         \"))))(Tile((id \
         75c26473-5892-4689-9f68-b4c317c318cf)(label(clearField))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         fecb77e6-94d7-4cea-85ef-a3f7f6dad097)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 24))(sort Pat))((shape(Concave \
         24))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         0bccf235-1787-48cc-a56c-1c30b086828e)(content(Whitespace\" \
         \"))))(Tile((id \
         b8e6e67a-b9e3-4afa-b747-641a58b2aaff)(label(Field))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         d43d034e-2412-4944-985a-7cb612455933)(content(Whitespace\" \
         \"))))(Tile((id \
         2261cb42-476e-4768-ba04-e6d9012bcb94)(label(->))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 13))(sort Typ))((shape(Concave \
         13))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         1e34e68d-f16d-4372-8aa9-f55423a74d76)(content(Whitespace\" \
         \"))))(Tile((id \
         167cce4f-a176-4c10-92aa-64ac16c67510)(label(Field))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         d34412e7-6c34-4493-a72f-1829d58a8f51)(content(Whitespace\" \
         \")))))((Secondary((id \
         fd889eed-7d94-4fcf-a946-9d5c1c30eae3)(content(Whitespace\"\\n\"))))(Tile((id \
         eddd4694-3e11-47bd-a4d4-e909fc412001)(label(fun ->))(mold((out \
         Exp)(in_(Pat))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         37))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         b6812e1e-95d6-471a-9f8d-28f63fbdbf08)(content(Whitespace\" \
         \"))))(Tile((id \
         05da02d1-7638-4c19-aafa-cf90f36254dc)(label(field))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         c42901ed-6470-412b-8c29-f53b6e70a857)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         851af571-ff08-4d29-a909-e3d23b03242e)(content(Whitespace\"\\n\"))))(Tile((id \
         6e097dbd-2fdf-4ee7-b885-5614cecb09b6)(label(map))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         f9c2ad97-9861-4556-bcd3-cbbaa26d8391)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         44a02955-9050-4786-b24a-6ef0208a53c7)(label(field))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         b04ea79e-3fd3-49ed-a096-aa1c69d51241)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         56592cfc-2c0c-4865-824d-9a739e33931a)(content(Whitespace\" \
         \"))))(Tile((id a494dcf7-169a-44b2-bfe4-c12bb4daf453)(label(fun \
         ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 37))(sort Exp))))))(shards(0 \
         1))(children(((Secondary((id \
         e5ac7f71-f32f-4cbc-b9f7-876a9b634384)(content(Whitespace\" \
         \"))))(Tile((id \
         88c81862-70d6-4fa2-9a95-7a1bd3cc9994)(label(row))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         fcbcb4c8-4a57-4ffc-aff1-c7b8f325c648)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         3d8d5c2d-5395-4b37-8981-cd2dbf0df4c4)(content(Whitespace\"\\n\"))))(Tile((id \
         2d720a70-7343-4b28-8d7d-55e00c267d9c)(label(map))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         0a698dd2-ddf9-434b-831c-08fd8208ce28)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         50c48fd1-9c82-4b12-8164-28e8c3ee21d4)(label(row))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         55573042-8e02-4e20-a98e-562140cbe531)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         83ba4273-2c6e-4d98-bc0e-49bdb25b7d7f)(content(Whitespace\" \
         \"))))(Tile((id 445de33c-e9f9-4044-bf00-03fa4f1b0f62)(label(fun \
         ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 37))(sort Exp))))))(shards(0 \
         1))(children(((Secondary((id \
         05635e60-3139-4c86-a0dd-a6e38ef520ba)(content(Whitespace\" \
         \"))))(Tile((id \
         26e146e9-9129-4fa6-b8fe-9dacb1b35f1d)(label(cell))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         822d61d1-a5c7-4bc4-a513-b3a24c2093c8)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         f0a6bd23-5046-4963-8d5f-3dbb47f5fcea)(content(Whitespace\" \
         \"))))(Tile((id \
         7dd00087-2341-474b-a584-35ca7798f962)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         6fd380b5-8819-4400-b8ea-41c8159bf62f)(label(crop))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         d99540c4-be3c-4f11-a7e7-1e86c9cbbbfc)(content(Whitespace\" \
         \"))))(Tile((id \
         23e9df69-e633-4e09-9f00-97cbd1121972)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         e39343d5-ee23-4e55-a799-71f5550e28cc)(content(Whitespace\" \
         \"))))(Tile((id \
         15ce8797-3162-4df9-92d2-b78c6ce88a1a)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         7fa338e9-7835-47f2-a2ce-75be402ec0ab)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         1de7a94e-0a87-40cb-af21-8db0cc00df54)(content(Whitespace\" \
         \"))))(Tile((id \
         444a509f-0933-4d04-aca5-28b09fa9b220)(label(soil))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         5baab571-4a80-4a3a-86eb-004ee82ca038)(content(Whitespace\" \
         \"))))(Tile((id \
         983e5085-36e5-42a7-871f-bd1118e04610)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         4e62a7a5-9761-4915-b86a-2ddaf943b118)(content(Whitespace\" \
         \"))))(Tile((id \
         c3d922f9-e825-4866-9a54-98154750429e)(label(cell))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         bcb2a348-c4fa-4b65-bdbd-a02ab41bbcdf)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         414e8e17-06fe-4938-8e68-883483ee6d7e)(label(soil))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))))))))))))(Secondary((id \
         080ff68c-9628-420a-9db5-4d9cfc95f5cc)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         1a9a9757-b3b5-43dc-815a-10a41d0ef1db)(content(Whitespace\"\\n\"))))(Secondary((id \
         98aa5913-7283-41e7-84a2-3f32b7ee6480)(content(Whitespace\"\\n\"))))(Secondary((id \
         d73624fa-e5c7-4a96-b2c9-ce58aa64779b)(content(Comment\"# Update the \
         model based on an action #\"))))(Secondary((id \
         773766c7-242f-4a89-a061-a0c591fdecda)(content(Whitespace\"\\n\"))))(Tile((id \
         f5bb97c7-0c89-4007-804f-f5111b4db121)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         53d263d5-1c3a-44a8-bcca-804ad5427693)(content(Whitespace\" \
         \"))))(Tile((id \
         41b9246f-e4ec-45a8-8ad8-2206e2892d4b)(label(update))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         f74b840b-0079-4b46-a833-c276a95b7311)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 24))(sort Pat))((shape(Concave \
         24))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         5ff08748-4b54-4a14-8ec7-9533c5b0692b)(content(Whitespace\" \
         \"))))(Tile((id \
         dd5555a3-df5c-4a79-8c68-fcd3d6751a66)(label(\"(\"\")\"))(mold((out \
         Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0 1))(children(((Tile((id \
         6d60cde5-b5e1-4189-a716-ce6e26cf73ae)(label(Model))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         3bc2e722-b921-4d02-9f35-50fff91713a0)(label(,))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 44))(sort Typ))((shape(Concave \
         44))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         91f7ca66-8f7f-4057-ac5e-f4c5ddb1e119)(content(Whitespace\" \
         \"))))(Tile((id \
         a3cb91a0-f682-4b59-875e-4fefd0a72465)(label(Action))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children()))))))))(Secondary((id \
         82e8e3b6-b62a-47c2-9230-f089f9edf982)(content(Whitespace\" \
         \"))))(Tile((id \
         58064a36-a1a9-4915-a4f3-fb59d8ccc7b4)(label(->))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 13))(sort Typ))((shape(Concave \
         13))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         b8ca3e2d-21a9-443b-a4f2-195d8a70fe17)(content(Whitespace\" \
         \"))))(Tile((id \
         802b0bda-7de6-42cb-9a85-af4f3987428b)(label(Model))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         df035a5d-729c-46a6-9c03-2f8896dc9924)(content(Whitespace\" \
         \")))))((Secondary((id \
         d00ae15e-a639-4496-a1b2-20b1ec7c4263)(content(Whitespace\"\\n\"))))(Tile((id \
         e290a47c-ee50-4fe5-99ce-7d25a862743a)(label(fun ->))(mold((out \
         Exp)(in_(Pat))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         37))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         3d80fe6c-a514-471b-83fe-b75919cab2a3)(content(Whitespace\" \
         \"))))(Tile((id \
         690694d6-e9e1-454d-a338-cca30d1f7236)(label(m))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         91f952f9-7e69-4cc6-b271-dd0bf008448c)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 44))(sort Pat))((shape(Concave \
         44))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         6039421e-9e9b-4ac2-b477-8a0cd3e03085)(content(Whitespace\" \
         \"))))(Tile((id \
         991694c7-67ca-4051-a1e7-88a7f16c50e9)(label(action))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         6bf3cf97-268c-4fd2-ac07-79e6059ce82b)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         16046b86-ff9c-4960-b950-7e0d1bce545b)(content(Whitespace\"\\n\"))))(Tile((id \
         65897b1f-837f-4fe2-99ff-195acd83aed3)(label(case end))(mold((out \
         Exp)(in_(Rul))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         e891e707-9f4a-4d69-b14f-bf7f6275293c)(content(Whitespace\" \
         \"))))(Tile((id \
         72143838-289c-4b6f-bc6c-941615bae738)(label(action))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         0da3b698-9bbd-4360-b8e1-8decefcbd480)(content(Whitespace\"\\n\"))))(Tile((id \
         214d113e-da19-423e-bfe8-ebe5ee436b9c)(label(| =>))(mold((out \
         Rul)(in_(Pat))(nibs(((shape(Concave 46))(sort Exp))((shape(Concave \
         46))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         7a697c26-cce0-4d26-b42b-cbd7b4f7a8e7)(content(Whitespace\" \
         \"))))(Tile((id \
         5d2981b8-b0bf-4b6b-b057-4c5f07ebb912)(label(PlantCrop))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         336bbf54-d128-4b8a-a164-dea199b8f7f1)(label(\"(\"\")\"))(mold((out \
         Pat)(in_(Pat))(nibs(((shape(Concave 23))(sort Pat))((shape \
         Convex)(sort Pat))))))(shards(0 1))(children(((Tile((id \
         c3d6cbc1-82c2-4f30-ad9c-5fc9fbbdf2f7)(label(row))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         17fd2a78-725a-41ba-82b0-5a2a6edeeaed)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 44))(sort Pat))((shape(Concave \
         44))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         83c8f838-9a33-4cf1-b4eb-912a4232bcb0)(content(Whitespace\" \
         \"))))(Tile((id \
         00cd4bea-e702-4434-b1d3-d01e3aef78c1)(label(col))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children()))))))))(Secondary((id \
         367bcb2c-2204-48f7-a763-e08b2c026ffa)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         699ec21a-13ca-4ec4-ab96-7fb734c6e1bd)(content(Whitespace\"\\n\"))))(Tile((id \
         07572570-f191-4e37-9629-dd1a78f174d2)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         40d93f61-c967-488e-be20-2055cc65298f)(content(Whitespace\"\\n\"))))(Tile((id \
         14a257ed-ccee-44aa-95d5-af6e86232dff)(label(field))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         f655ab83-ed5d-40b8-96c1-95a6db33bbf2)(content(Whitespace\" \
         \"))))(Tile((id \
         6c65c1ec-a694-4e00-b39b-93f444483a88)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         053feb4e-2a94-4e24-9b8c-2d03a40df240)(content(Whitespace\" \
         \"))))(Tile((id \
         b95860fa-0efe-4dcb-9747-fe2c727456bf)(label(plantCrop))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         ed81eb33-b9a0-4e45-93e7-f72d40fd63d5)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         b5ff12de-df21-461f-93bc-2d60239351e0)(label(m))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         2df3f50b-72fb-4282-a280-48517836568a)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         be1f11a3-6dc8-40d5-9c69-afb558872153)(label(field))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         d05818e8-c41c-4182-ae5e-0172fcfd10f8)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         f6d61315-0b8b-4a61-b527-2442df97f6db)(content(Whitespace\" \
         \"))))(Tile((id \
         e5453cbb-edd7-464b-b229-7bcc7776400c)(label(row))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         c2989472-c869-444d-92ed-67c7d8ec6abc)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         ff3e33cc-0b0d-47aa-bebc-9fda7ce68c77)(content(Whitespace\" \
         \"))))(Tile((id \
         6ed87dd8-ab12-4484-be4c-b213336c1e72)(label(col))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         6b9ee2ca-7777-4b03-af60-d8d5ba45d10e)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         57766811-a4f8-4b33-8b6c-185b5529f0bc)(content(Whitespace\" \
         \"))))(Tile((id \
         2662767f-35d4-484b-8494-c42b69efc4fe)(label(m))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         7f57af4a-4ce8-4f09-80ec-5ee14850160e)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         412ca8cb-2d87-4136-a80c-4b6598844315)(label(currentSeed))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         5b45042e-cdde-49e4-8137-332e0ae31cb4)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         cf7aa077-66ff-4eed-8a0c-a142269c894c)(content(Whitespace\"\\n\"))))(Tile((id \
         f39afe03-77f9-4609-9f54-e1f9f32b1a16)(label(currentSeed))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         b0d35efd-a4d4-49f2-bdcd-5089ef123abd)(content(Whitespace\" \
         \"))))(Tile((id \
         8bc64389-4153-481a-ab6b-156aa3aa6b82)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         8ac55d66-83fd-4809-9e37-d56360d7e77b)(content(Whitespace\" \
         \"))))(Tile((id \
         c897043b-dda6-40b3-9aa3-04da5989afb2)(label(m))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         ac4e1365-cc04-4448-951a-b49e610059b7)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         a6e15a36-169f-44da-96dc-bc771704d1af)(label(currentSeed))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         642a62ec-3697-4f59-ae55-e2ea34ae8504)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         f3f0c677-4db9-46c3-90e6-d6306bdfdec4)(content(Whitespace\"\\n\"))))(Tile((id \
         f14422bc-e8b1-4ccd-81e6-aba63fb40b5d)(label(seedInventory))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         fea91020-c286-4127-b778-6ff72ceaf505)(content(Whitespace\" \
         \"))))(Tile((id \
         1956d340-ca35-46a0-829c-6aedc100a4b2)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         8888ace8-5c16-41bd-8ade-c040716095be)(content(Whitespace\" \
         \"))))(Tile((id \
         53e3986b-300f-4fd4-bb5c-7488156da312)(label(m))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         ed74953b-cdc4-4ac5-9b90-3ba490620d46)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         6321c856-9aef-454b-b8a7-75eaf89cac10)(label(seedInventory))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         ad1b7d0f-22dd-48a1-8296-69122d7a6479)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         c70f5243-2b83-4574-9771-08db8d236d02)(content(Whitespace\"\\n\"))))(Tile((id \
         0a50a0fa-0fb6-4970-9833-0da78276f579)(label(| =>))(mold((out \
         Rul)(in_(Pat))(nibs(((shape(Concave 46))(sort Exp))((shape(Concave \
         46))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         3d6e9e66-5617-4e68-aeb2-9272d022413c)(content(Whitespace\" \
         \"))))(Tile((id \
         923ebf99-35b9-4a7a-9fba-61a1405e6520)(label(HarvestCrop))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         aa17b131-e994-41b9-af03-3cef4962d888)(label(\"(\"\")\"))(mold((out \
         Pat)(in_(Pat))(nibs(((shape(Concave 23))(sort Pat))((shape \
         Convex)(sort Pat))))))(shards(0 1))(children(((Tile((id \
         c4066cf9-1fe2-462b-81ea-63bed10db10e)(label(row))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         7d8e5d8e-322c-477d-8de5-16e2da0a393d)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 44))(sort Pat))((shape(Concave \
         44))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         0cc34f5c-ed41-4607-a37c-7c4ff67273a7)(content(Whitespace\" \
         \"))))(Tile((id \
         de6cce9e-8d82-4114-b1a9-c7436d09904f)(label(col))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children()))))))))(Secondary((id \
         e538027a-be49-4aef-a2e2-c3096fba530c)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         3d05e553-34d6-4099-86d0-ed9ebac4e9a1)(content(Whitespace\"\\n\"))))(Tile((id \
         76d2ebb8-6e83-45e5-b9c7-3bb8e622635e)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         23d5ef00-848d-455e-9eb3-5fe175dd4e1d)(content(Whitespace\"\\n\"))))(Tile((id \
         787ec5c8-fd83-45db-8f87-4c16ff81df47)(label(field))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         d8b95478-7d30-4ebf-88d3-620b1b784f9d)(content(Whitespace\" \
         \"))))(Tile((id \
         135af953-f365-4821-91ac-ca55d56bacad)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         d4b95475-2d9f-479e-abe7-48739d6dae84)(content(Whitespace\" \
         \"))))(Tile((id \
         e385e4c5-68f5-4ae6-8ae5-2c3953524858)(label(harvestCrop))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         c66cf041-7804-4507-b07c-9ca78085b0a7)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         4ea09321-85dc-4a38-8375-bacb3cd17f9b)(label(m))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         ad2b1d6d-a61a-449d-b93f-841719feb545)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         ca07b752-aefb-4a79-a4a4-fd6599eae59c)(label(field))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         015a0b12-f848-406e-a8fb-add4cc535e47)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         2fcc1292-eada-469d-9455-5859299e1e99)(content(Whitespace\" \
         \"))))(Tile((id \
         50d9f921-a119-4705-ba52-d8ddc5f61ffa)(label(row))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         8d6c28cd-5a6d-4133-999c-6dae2a9f4ded)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         977fa910-c519-422a-bedd-e43f87ba5e27)(content(Whitespace\" \
         \"))))(Tile((id \
         0c80ba0f-8191-447a-9be0-6dce1bfd49a4)(label(col))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         a4a97ca4-3a9e-4bd8-84e5-9bf2e3c16cdf)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         1c45fbb8-3b1d-45d8-ac22-b677331fae26)(content(Whitespace\"\\n\"))))(Tile((id \
         2c6b5500-c3d7-428f-910f-2270665c8d0e)(label(currentSeed))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         24da2eb2-fd54-4c3b-af1f-7ec1a93ee318)(content(Whitespace\" \
         \"))))(Tile((id \
         b819a663-5a22-4d59-88e5-bb57fbbada7f)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         8058fadc-0db9-433e-b58f-3547066b1e26)(content(Whitespace\" \
         \"))))(Tile((id \
         01451e31-4368-44a0-8c9a-f8f70e9e5988)(label(m))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         2a6ff12d-8a9f-4f8a-8b82-7892a0e7ae13)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         1691a66f-97d1-4d76-8100-85fb0426f05a)(label(currentSeed))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         5b3e0a6c-15ec-4882-91bb-ee333b5441e0)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         b8336ca3-d8b3-44c4-b2fe-bab28b380169)(content(Whitespace\"\\n\"))))(Tile((id \
         9308e419-81c1-496d-8d1b-b518463ddeeb)(label(seedInventory))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         265851ab-f2ad-4e26-a0f9-f73a7f3e9308)(content(Whitespace\" \
         \"))))(Tile((id \
         0508c624-56c4-46e3-9dfd-e19474391c26)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         3fba2da8-4209-4a29-b6ab-1b692ae305f4)(content(Whitespace\" \
         \"))))(Tile((id \
         00a515e4-663f-480a-9d5a-4478b6c30057)(label(m))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         5afb25e4-f374-43f0-83fa-5e4a722ca391)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         00095c02-7bf7-4215-9ef1-7f0b500987f6)(label(seedInventory))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         e51003a0-3daa-47fb-a53d-3721fc4f376e)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         756cc99f-8b25-412a-a253-4fbdb0996f93)(content(Whitespace\"\\n\"))))(Tile((id \
         9534e8f7-7cbc-41b4-89e4-b5f4e2f37a5e)(label(| =>))(mold((out \
         Rul)(in_(Pat))(nibs(((shape(Concave 46))(sort Exp))((shape(Concave \
         46))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         9823a972-0fee-4e0c-b931-19a16ff19640)(content(Whitespace\" \
         \"))))(Tile((id \
         0c96e69a-69c1-4b5c-9738-e8a54b5121ca)(label(TillSoil))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         cedf866e-86f5-4e59-9db2-cd47e1bc723a)(label(\"(\"\")\"))(mold((out \
         Pat)(in_(Pat))(nibs(((shape(Concave 23))(sort Pat))((shape \
         Convex)(sort Pat))))))(shards(0 1))(children(((Tile((id \
         4c37965b-ed02-4f82-b7fa-9a9ac602ad88)(label(row))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         eb3e3dd4-50d4-45d2-bc10-10d06a5dc7cc)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 44))(sort Pat))((shape(Concave \
         44))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         6db6f89c-dadb-49a9-ba5a-236291edd740)(content(Whitespace\" \
         \"))))(Tile((id \
         d1db62e4-6a70-439d-8e21-bddb3e49d4dd)(label(col))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         f4ac26f5-2efb-432a-9268-fa60215e09d6)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 44))(sort Pat))((shape(Concave \
         44))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         33cea68b-4609-4d9e-aac6-d33e19c090a7)(content(Whitespace\" \
         \"))))(Tile((id \
         56e8954d-4da9-4382-af91-3772e7cb0961)(label(soil))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children()))))))))(Secondary((id \
         860a95d4-8d1e-4efa-b7bb-fc4a0ab29e50)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         a2134500-8a04-42a6-9ddc-2bfe1a9bccac)(content(Whitespace\"\\n\"))))(Tile((id \
         c21fff1e-379c-433e-a8fe-e935316fdc93)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         1a9223f6-6c81-4c22-b0ac-87bf1e476e25)(content(Whitespace\"\\n\"))))(Tile((id \
         fab93ed1-8958-4962-81f2-ff14ccd6263a)(label(field))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         a245b096-2d56-40bc-9e92-7dbc5d102669)(content(Whitespace\" \
         \"))))(Tile((id \
         da3db5e1-019b-4f4a-b869-df77770a0d23)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         9f86826a-c1b9-41e2-877d-d0a5d3f63e13)(content(Whitespace\" \
         \"))))(Tile((id \
         f2270fe0-1816-478e-8bfc-e8e20e1fb3fa)(label(tillSoil))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         c4a269fb-ae1b-4c02-85b0-f80ab11046fe)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         6508613b-8d5f-4bb2-9c3d-55ace32a4c10)(label(m))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         4f27aa23-54a1-40bb-829f-889e0e1b286a)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         b90bf719-3ab0-4bcd-81cf-5afd662590fa)(label(field))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         3e9a2d81-f646-464d-b413-fb031a269602)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         b142257a-a02f-4ceb-adf8-5b9f05eaf991)(content(Whitespace\" \
         \"))))(Tile((id \
         2c9c0f9b-9ebf-40d4-a3fe-8cb863ebc69e)(label(row))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         a8995eae-2c7d-4491-aea2-9f64acfa1bb6)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         a3982a1e-1e78-42ad-aa3a-6123d4fcecb3)(content(Whitespace\" \
         \"))))(Tile((id \
         74625a4f-9b19-4f5f-bef3-45bb29cd47eb)(label(col))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         590b4494-6899-4548-9c6b-77893d627b72)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         367fd1c7-bc21-47b7-9a72-9fb766e080fa)(content(Whitespace\" \
         \"))))(Tile((id \
         083a82c1-c483-4286-b07e-9df7dfe7e343)(label(soil))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         f5b2b6ad-79cb-466e-8649-4aac057fed3e)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         ba8ee13b-4a5c-4e04-845e-a7ee6d5290e9)(content(Whitespace\"\\n\"))))(Tile((id \
         07a1d608-c769-415a-84e5-3fdadddb1ec2)(label(currentSeed))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         a3ebe673-b056-44e2-8d62-73482e718ffb)(content(Whitespace\" \
         \"))))(Tile((id \
         ddba0dbd-5265-4ce0-8294-a81f596ae806)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         7bb4e20b-9c0a-45b5-a552-490d12cbf966)(content(Whitespace\" \
         \"))))(Tile((id \
         e1bd4bfb-f181-4e2f-859f-e7cab07424ad)(label(m))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         f87a9dee-8379-48e4-ab89-99605aae0ffd)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         8e85e9b7-9f3b-4ed9-a394-eb76484977f9)(label(currentSeed))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         bbc0265b-bf49-48e8-8b86-fe02576e683f)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         83da5f0d-37e3-47ff-adbf-e5b293a0ad08)(content(Whitespace\"\\n\"))))(Tile((id \
         de53e9de-bd90-4c36-91bb-18814fe21d20)(label(seedInventory))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         e900117a-24ef-43d7-9248-2b38a716a084)(content(Whitespace\" \
         \"))))(Tile((id \
         5753b616-7061-4a8f-9d41-97d4b53ccdd9)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         13b6b6f7-a9f3-45b6-902a-59fc6e2665f6)(content(Whitespace\" \
         \"))))(Tile((id \
         755765b8-fa75-4037-b8a5-cbc179c271b1)(label(m))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         aba3c93c-254f-43bc-bbc2-cb404a2cf732)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         356d6f1d-a1cc-4c95-ba61-5a9b77e02d30)(label(seedInventory))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         c1dc84cc-cf3b-4ffa-9cbb-746b69ceda26)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         64012b85-7a28-4e53-8084-adbe14f02ef2)(content(Whitespace\"\\n\"))))(Tile((id \
         2e168878-3383-4512-bac1-4b42866b7578)(label(| =>))(mold((out \
         Rul)(in_(Pat))(nibs(((shape(Concave 46))(sort Exp))((shape(Concave \
         46))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         13345191-7033-45e8-bba7-2f401590ad70)(content(Whitespace\" \
         \"))))(Tile((id \
         ea266b99-06b1-4bbc-9a3d-f2d396a5cdd0)(label(PlantRow))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         0b9ceace-da1a-4ef1-8b2a-181125ae9012)(label(\"(\"\")\"))(mold((out \
         Pat)(in_(Pat))(nibs(((shape(Concave 23))(sort Pat))((shape \
         Convex)(sort Pat))))))(shards(0 1))(children(((Tile((id \
         fc4ea4b0-7454-4163-803a-634b57f6d434)(label(row))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children()))))))))(Secondary((id \
         aa0d20d7-6c56-4ecb-9f7e-40b7d4a712bf)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         d62655b4-baf3-40a1-9485-d484b3bc5eac)(content(Whitespace\"\\n\"))))(Tile((id \
         078d9d74-7483-48c4-9549-b7379f901d14)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         a27f90d1-2715-4712-8497-03bb60706f35)(content(Whitespace\"\\n\"))))(Tile((id \
         8356ba4c-94f0-4691-ad83-a5c8e0656ce0)(label(field))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         0bbb7018-b902-4583-a01f-8908289ce65c)(content(Whitespace\" \
         \"))))(Tile((id \
         bbfaf149-5667-4ff9-b7b9-72f9d2753cc5)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         6c87be50-7a1f-4606-b493-25c8a3c497e4)(content(Whitespace\" \
         \"))))(Tile((id \
         6bc08d0c-7dda-4307-b7f0-4b186206a880)(label(plantRow))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         b3a03771-2141-47b3-8dd5-c2449bccb95f)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         ea0e446b-3763-4b85-86d6-5db3d503ac9b)(label(m))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         430217f5-4e04-46df-b8f6-61cdd513cefb)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         eb980923-e249-4c3d-a796-dd32892e3a48)(label(field))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         24c0477d-a456-4cd8-885e-482acdb42de0)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         983e7427-2fbb-4b43-957d-4237586a9da3)(content(Whitespace\" \
         \"))))(Tile((id \
         79ad21b2-b458-4bbf-8ed6-a743c06b7218)(label(row))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         56785f88-379a-4101-a421-228dc79894ca)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         fca85690-1557-4a5d-bbe6-2faf8aa6713f)(content(Whitespace\" \
         \"))))(Tile((id \
         7e4e4f26-1b9d-4249-a9f2-4e189ed245f8)(label(m))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         6eff51f0-2b2b-41f1-b204-bec64c99a36c)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         fa5b17cd-2095-431d-81a3-ce059d24cefc)(label(currentSeed))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         5c7a55a0-a307-40ed-a7b5-1840a2d4118e)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         0a2d4eca-57d2-4ade-a959-95aa4f5c88bd)(content(Whitespace\"\\n\"))))(Tile((id \
         cc2bf5a6-c1cd-47d4-9ad0-fa859793232c)(label(currentSeed))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         a763b76d-31af-4a5b-a8c9-729df1d3fd4b)(content(Whitespace\" \
         \"))))(Tile((id \
         b3d7f388-cbe3-46aa-bf2d-1fb7bf092289)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         18007180-b5b9-41ac-853f-8564ba9b75c0)(content(Whitespace\" \
         \"))))(Tile((id \
         e42d04ef-1744-4859-9d2e-28ab14bf0992)(label(m))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         cebac27a-f1e9-4425-b203-11dfd0981560)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         b713d00e-0c17-4c0f-9ed7-2095e28500a2)(label(currentSeed))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         82e5aa7f-30d8-4b4c-95ac-39a057a9c7e7)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         93e09f93-e5b0-438c-9ee8-4958923dd81c)(content(Whitespace\"\\n\"))))(Tile((id \
         1529fdc9-1736-4956-b102-2edfecd077cb)(label(seedInventory))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         a2277376-8d41-4c06-970c-655e4ac54532)(content(Whitespace\" \
         \"))))(Tile((id \
         21c7f5f0-acd8-4b92-9112-4c654a7a82c5)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         50efa4a5-9908-4041-880c-0f604e415b3e)(content(Whitespace\" \
         \"))))(Tile((id \
         3ffe0b31-a982-4dda-aba4-5eaba97fb5f9)(label(m))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         b42707a7-e947-4f80-9940-0c562e1c98bd)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         8a9c9af0-c840-4621-af8b-c3008d09af1a)(label(seedInventory))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         ab189efd-bc51-46f1-9920-588acbe36dde)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         b3bf9939-d453-49cd-9d5d-13ee6615ea43)(content(Whitespace\"\\n\"))))(Tile((id \
         127e9b3e-242f-43f0-885a-9aaf4f29bcbb)(label(| =>))(mold((out \
         Rul)(in_(Pat))(nibs(((shape(Concave 46))(sort Exp))((shape(Concave \
         46))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         ae75fa9a-0ca9-417c-a15a-0391bf515f3a)(content(Whitespace\" \
         \"))))(Tile((id \
         cb5dd764-df8b-4f17-a67a-7771044ef369)(label(ClearField))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         f750062a-019f-4235-8633-b764d5bdc081)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         051ad490-ab01-43e6-87c0-a61c6df8ffee)(content(Whitespace\"\\n\"))))(Tile((id \
         97a88f68-550d-443e-b965-f164a74bdc35)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         5aac5a26-3ce1-458b-a7fd-9d0982319c93)(content(Whitespace\"\\n\"))))(Tile((id \
         efd63d70-095d-4bb1-b462-072d95f6219e)(label(field))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         cfdddd17-745d-4550-9522-d8f6e3a9f748)(content(Whitespace\" \
         \"))))(Tile((id \
         9f14a479-54fa-4e58-976f-5968d3b63092)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         62cec924-87f9-4ac2-8ff1-5a878ccd16ed)(content(Whitespace\" \
         \"))))(Tile((id \
         f0bcb0e9-9512-4719-842a-5abae5ede044)(label(clearField))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         d7cefedd-58d6-49e8-9100-5c9e2020762d)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         41407c34-0c55-4451-8370-c9d280514c62)(label(m))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         227256e7-910f-406b-948a-5b4e94f8678b)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         d5791812-9a33-41ea-bd07-5de4e1728489)(label(field))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         64be9763-3758-4436-8466-b81778172834)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         7de23d94-8e66-4cb9-a5e1-2c0e52d12558)(content(Whitespace\"\\n\"))))(Tile((id \
         bec5af54-32a7-4a18-b188-606a5e1c5bf6)(label(currentSeed))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         cbe3c1ce-febf-4650-a9f8-ec714f69aa72)(content(Whitespace\" \
         \"))))(Tile((id \
         a009aeeb-d2db-4066-a73a-3ad9b2d25c59)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         3419256f-2d39-4e5a-9fcd-0c45ed1d6d6c)(content(Whitespace\" \
         \"))))(Tile((id \
         ded44c24-7d49-426c-873b-7457bb2ec3bd)(label(m))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         f06bcad2-169e-4234-af21-1c93899e7f8a)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         ae3f25f9-384c-4d1c-9b0e-f01193832cb4)(label(currentSeed))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         94380be3-c73b-4a84-8b2c-66462c917bd6)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         7e3969ed-e4f8-4e37-af6f-501aa26ce192)(content(Whitespace\"\\n\"))))(Tile((id \
         9216f748-c0ce-4d21-92d5-2f2d8f2c7246)(label(seedInventory))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         1c01aba7-2021-4933-8124-f2c981d04d51)(content(Whitespace\" \
         \"))))(Tile((id \
         e097dc94-4e7a-4312-b65a-1becc1ed8ac3)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         8f65657c-8f59-41e5-a20f-1eb49fe30dca)(content(Whitespace\" \
         \"))))(Tile((id \
         75b69ad0-22f3-49be-834d-a9b780247165)(label(m))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         09720faf-8d56-4996-a227-5ce7e95cc404)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         3592e2a4-13ab-40b9-87e2-0ebbd5171036)(label(seedInventory))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         cf476aeb-51fd-4322-be50-e3246b46731e)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         428ae2ee-b844-423f-a15d-255a83649f7f)(content(Whitespace\"\\n\"))))(Tile((id \
         9f8b8213-f2e1-4329-966d-6fc98878f5be)(label(| =>))(mold((out \
         Rul)(in_(Pat))(nibs(((shape(Concave 46))(sort Exp))((shape(Concave \
         46))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         98e5a27e-5273-4617-9f33-436656ea7390)(content(Whitespace\" \
         \"))))(Tile((id \
         c451e459-966f-4d9d-88d7-96096b649f73)(label(SelectSeed))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         472c505d-648c-4cbb-a7e4-b90afab72e35)(label(\"(\"\")\"))(mold((out \
         Pat)(in_(Pat))(nibs(((shape(Concave 23))(sort Pat))((shape \
         Convex)(sort Pat))))))(shards(0 1))(children(((Tile((id \
         d3ffae0a-8bd1-481c-b138-e88bc1ea01e8)(label(idx))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children()))))))))(Secondary((id \
         cb65ef16-e88b-4175-ab7c-4af505bb59bd)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         50f954a1-13ae-4184-9229-14835d2d4ab6)(content(Whitespace\"\\n\"))))(Tile((id \
         39d087a9-c07d-4a03-913d-273a29cc965f)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         aeb9b591-87f5-4218-ba74-baabe65268b5)(content(Whitespace\"\\n\"))))(Tile((id \
         a9ebffe5-934e-40f7-bd1a-ebe25cd3a7f4)(label(field))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         9df870cb-dc8b-449d-a97a-cb6954829337)(content(Whitespace\" \
         \"))))(Tile((id \
         4a2e2c1a-2805-4916-a8dd-5d72fcf9f405)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         6abe3f18-1164-4ce6-bd91-24a80a0b733d)(content(Whitespace\" \
         \"))))(Tile((id \
         d07751de-b7ff-48f7-be3a-e0962b0b1f52)(label(m))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         f0e85f31-a201-4493-9a54-177ff638ffbe)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         f2a4c30c-0fec-478b-8265-58b76987129c)(label(field))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         ef957fe3-caab-460f-ad96-4f1b1c9eaaf7)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         ab377c44-6d71-4ecb-be66-7a58465c9a67)(content(Whitespace\"\\n\"))))(Tile((id \
         9199bac9-683c-464e-90bb-2766f4d0f93c)(label(currentSeed))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         e9479f4f-99e6-4ba9-bcf9-4844941a54fd)(content(Whitespace\" \
         \"))))(Tile((id \
         ec0f1b53-34d3-4f49-8ee5-8a5f797db06c)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         45bf2744-8c3f-490e-99d7-3a101ac46d05)(content(Whitespace\" \
         \"))))(Tile((id \
         4449fe26-5015-4058-b869-ff8b95d31bb2)(label(nth))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         bc5f7c14-c862-4ec5-9065-922a2fe82ca8)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         f31f8cec-8b89-4642-a729-1c98c35b0bf3)(label(m))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         1f56d63a-b939-46bc-8338-6737a8ec7340)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         6675e63a-c792-43da-8cf1-5d1da507a139)(label(seedInventory))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         5e4ee578-3403-4e35-98be-3866f8d9138d)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         f6feca4b-b734-4b93-90df-846a0c04ceee)(content(Whitespace\" \
         \"))))(Tile((id \
         f5083d78-5512-4a40-8734-eb569d6a2931)(label(idx))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         60e233dc-062c-46e4-8435-5a49d7753373)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         71127483-51ee-4863-aa24-e92eea9cfc40)(content(Whitespace\"\\n\"))))(Tile((id \
         130c3a22-ce55-4b33-8cbc-2f32a061c360)(label(seedInventory))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         08aa2343-c5df-48b1-ab09-402e82a5f07d)(content(Whitespace\" \
         \"))))(Tile((id \
         eabf0d61-b3fa-4c5b-b545-cb6f880c7289)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         ee6ffcda-d02d-41ea-8f38-bb8497a8022d)(content(Whitespace\" \
         \"))))(Tile((id \
         8cacd3f9-7337-48ab-a060-fbabe795d835)(label(m))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         3f8d4e43-2418-4adb-9622-16fbaf9dd062)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         c34442f6-6c4a-40e3-87bd-cef97643af36)(label(seedInventory))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         5a8f7268-e5b5-40ee-8119-e4616a2f26e8)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         8f131f32-d4a9-4ced-9577-c91e1a57610f)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         26fb04c9-2a83-43e6-b31f-c27cf48acb61)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         714acdcc-87da-47c9-90e7-0db7a8870204)(content(Whitespace\"\\n\"))))(Secondary((id \
         5d93df52-8273-449d-a628-d425f07f43b3)(content(Whitespace\"\\n\"))))(Secondary((id \
         50fb8a37-92d1-454c-90ab-4d213890d1fe)(content(Comment\"# Run multiple \
         actions in sequence #\"))))(Secondary((id \
         7bf80cea-61fb-4d0c-a5c7-56a042f20c8c)(content(Whitespace\"\\n\"))))(Tile((id \
         8c14c0e6-97e5-449e-89f5-b2c1d87dd77b)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         dd5192a1-440b-4c2d-a952-5211d0ffffe8)(content(Whitespace\" \
         \"))))(Tile((id \
         f9755195-5646-4fd2-b818-200a9ac3d349)(label(do))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         9118be27-27bb-4e66-9a6f-ea564f16aa3c)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 24))(sort Pat))((shape(Concave \
         24))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         60b98562-2174-498a-aa0d-be850b5cae6f)(content(Whitespace\" \
         \"))))(Tile((id \
         1bc166f6-b891-4517-9f02-cc8f322b0b21)(label(\"(\"\")\"))(mold((out \
         Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0 1))(children(((Tile((id \
         e7a8cb97-d2fe-4368-9fc0-1686622af10d)(label(Model))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         b097d6e7-a51b-48a4-a937-a4a7d6f674b9)(label(,))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 44))(sort Typ))((shape(Concave \
         44))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         23702693-8950-46a2-b48f-2879fb69dcee)(content(Whitespace\" \
         \"))))(Tile((id 690c5ba3-f92a-4e5f-9823-a8e9a030544a)(label([ \
         ]))(mold((out Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape \
         Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
         86123e78-387f-4c88-b07a-e7f326762f7b)(label(Action))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))))))))))))(Secondary((id \
         56de1cfc-d46d-47e3-b23f-e3ce98fca82f)(content(Whitespace\" \
         \"))))(Tile((id \
         569a903c-f919-4b0b-83d8-03dbd41fe992)(label(->))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 13))(sort Typ))((shape(Concave \
         13))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         a023b7fc-2de9-47df-bca7-6e76447ec80b)(content(Whitespace\" \
         \"))))(Tile((id \
         7b570c78-e7b5-4455-87ae-cb15944672dd)(label(Model))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         656a4f38-cac5-4d51-a87b-d599726c6aa4)(content(Whitespace\" \
         \")))))((Secondary((id \
         4a94de45-8664-4e12-b165-1ca16820ea84)(content(Whitespace\"\\n\"))))(Tile((id \
         e1206d57-5765-4fef-b090-4e3b52cbcf76)(label(fun ->))(mold((out \
         Exp)(in_(Pat))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         37))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         444c4bc2-3585-47e7-ad6d-cb450c388c09)(content(Whitespace\" \
         \"))))(Tile((id \
         d7de1215-1340-4c9f-ae32-e0137d66968e)(label(\"(\"\")\"))(mold((out \
         Pat)(in_(Pat))(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0 1))(children(((Tile((id \
         c3606c84-e111-4030-bd0b-067183c0a478)(label(model))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         b3de3eb6-ea9f-4b6a-b599-07bce0004c51)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 24))(sort Pat))((shape(Concave \
         24))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         7ef062eb-bda9-4cea-b8f7-cbdbc983873c)(content(Whitespace\" \
         \"))))(Tile((id \
         9ce06f52-d07a-411f-b559-116e800eb70b)(label(Model))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         8286f8d8-d1b7-4771-b8dc-b7b560cd5404)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 44))(sort Pat))((shape(Concave \
         44))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         46851f4d-3cff-4a32-87bb-1d00aab89682)(content(Whitespace\" \
         \"))))(Tile((id \
         2686c657-ba6c-4392-960b-e2e43c84b743)(label(actions))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         8367c850-d21c-4e76-9432-e26ca847c5a4)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 24))(sort Pat))((shape(Concave \
         24))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         d8f96238-6db1-488f-878a-f42f973c144a)(content(Whitespace\" \
         \"))))(Tile((id ed6efed9-0d88-4818-be7f-c6b9e72077dc)(label([ \
         ]))(mold((out Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape \
         Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
         e4b8ffdc-37f9-4bb0-850d-471e1213c1e4)(label(Action))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))))))))))))(Secondary((id \
         1dac836b-ced5-4051-9e05-322b030597c0)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         de143df4-168a-4931-a725-4b8d4bbc5385)(content(Whitespace\"\\n\"))))(Tile((id \
         b200b978-cd40-45fe-8a6e-b37c257e74d6)(label(fold_left))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         4e482e46-74f2-4099-8c0d-0d045950402e)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         5a5b5227-b452-4cf5-8fa8-791b9009012c)(label(actions))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         4075c101-1af4-4c89-9669-f28b2ab755ca)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         5f9db5ce-597f-46f1-9ed0-ab3f655736d5)(content(Whitespace\" \
         \"))))(Tile((id \
         60be2c9f-74e9-455b-9d9f-c29db3f1da3c)(label(update))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         e8a7af6b-8895-440c-8af8-597731181e73)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         8af618d8-9cdb-4c71-8dda-7675a606f188)(content(Whitespace\" \
         \"))))(Tile((id \
         c269b355-d2c2-43f8-bea7-7ce2acdc11bf)(label(model))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         000ff34a-3fe7-48da-90d9-14a02d0d5158)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         2b1c0317-eaea-432b-bb73-0c9e257e9d79)(content(Whitespace\"\\n\"))))(Secondary((id \
         36c7b208-ddbb-462e-9ef2-2f89b697b7d7)(content(Whitespace\"\\n\"))))(Secondary((id \
         9daf367b-8e26-4f83-aa02-76fd8e79fcd8)(content(Comment\"# Helper to \
         get cell at position #\"))))(Secondary((id \
         4bdf4461-bf68-4172-bd1e-a5d5367852c9)(content(Whitespace\"\\n\"))))(Tile((id \
         87a5aa5c-aabb-498e-bd60-ed278e8ceaab)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         df6106fa-4cab-4b2b-a5f5-53d829a3ade9)(content(Whitespace\" \
         \"))))(Tile((id \
         165412e3-ce15-47f6-b47b-05bb8db4688e)(label(getCell))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         9a938236-0be7-491d-a97c-126ff4788d00)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 24))(sort Pat))((shape(Concave \
         24))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         e96a92e0-e268-4f18-9542-19d7eb038552)(content(Whitespace\" \
         \"))))(Tile((id \
         499f2007-e804-4efe-92fa-93f3f8f654de)(label(\"(\"\")\"))(mold((out \
         Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0 1))(children(((Tile((id \
         4b9e512e-4114-42ae-a28d-28a9127ed1f0)(label(Field))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         2887f967-62f4-4730-be42-0c085d988624)(label(,))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 44))(sort Typ))((shape(Concave \
         44))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         89d63448-d7e7-4ac8-8b0e-975714427cde)(content(Whitespace\" \
         \"))))(Tile((id \
         9cd70074-5604-4319-a3bb-ae668d761974)(label(Row))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         2a5db72c-666e-4959-a585-62f0fc08e702)(label(,))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 44))(sort Typ))((shape(Concave \
         44))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         89cdafc8-0902-4ad8-89c2-c9ca4b1018fc)(content(Whitespace\" \
         \"))))(Tile((id \
         f6f4fb40-9ea9-42f2-82b5-9e898ea22a3f)(label(Col))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children()))))))))(Secondary((id \
         75dd1469-5d7f-43b1-aa51-fa183bffd838)(content(Whitespace\" \
         \"))))(Tile((id \
         e8c54bb6-5a8b-4100-84dd-098c1a4c7b52)(label(->))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 13))(sort Typ))((shape(Concave \
         13))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         ed5a068c-c3ae-45ad-adc5-dcdec1da1c6f)(content(Whitespace\" \
         \"))))(Tile((id \
         155c73c5-44a1-4a69-abd7-995380a4773f)(label(Cell))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         c286a47c-fab7-440f-bae2-a6c91965e895)(content(Whitespace\" \
         \")))))((Secondary((id \
         669f6c78-5804-4dd4-a016-ca3220d844dc)(content(Whitespace\"\\n\"))))(Tile((id \
         6a75fce0-4798-4b14-ae1e-fffd5529ae83)(label(fun ->))(mold((out \
         Exp)(in_(Pat))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         37))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         b9d5eba7-6677-47f4-90ac-f537ab2729ed)(content(Whitespace\" \
         \"))))(Tile((id \
         8ca5e384-bcc8-417d-b968-cdd285d2be12)(label(\"(\"\")\"))(mold((out \
         Pat)(in_(Pat))(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0 1))(children(((Tile((id \
         d9ea86be-fe72-4eef-9201-3093b4fc230c)(label(field))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         fbfa47e8-58eb-4793-9810-19845c80c1a6)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 44))(sort Pat))((shape(Concave \
         44))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         5125b69c-de50-4e48-bf10-146ca615ded0)(content(Whitespace\" \
         \"))))(Tile((id \
         6dd561b1-979f-438f-99f0-2d7ef9f74948)(label(row))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         f21c0d3a-3919-4efd-b61c-70cea09894a5)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 44))(sort Pat))((shape(Concave \
         44))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         b537ef5f-ea1f-4ffd-a959-b64159607a05)(content(Whitespace\" \
         \"))))(Tile((id \
         cea76c22-67e7-4a47-b466-c2e61f08cf76)(label(col))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children()))))))))(Secondary((id \
         d352fce5-a955-4478-bbf7-afd4b25c5ad7)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         6b911cca-bfa8-4ffc-bee3-1d7da61e8f77)(content(Whitespace\"\\n\"))))(Tile((id \
         b4b5752d-d727-47fd-b7c8-864694584939)(label(nth))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         5449544f-3237-4411-b25a-89d58d244ba2)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         87b582a2-3e3c-4418-89df-fe2061c3d9c6)(label(nth))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         76c29b88-7f63-426f-b015-ffd1296bbb33)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         3fd36a61-ad21-4960-a87d-d5d2ba72eb24)(label(field))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         8d532f81-cdb1-4f82-909f-e969d9a4d917)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         95490f33-a8c6-4def-8764-51a017a81768)(content(Whitespace\" \
         \"))))(Tile((id \
         a852efa6-4676-4cca-919c-09da9a7c9064)(label(row))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         0cf3c31c-419e-4cd5-b188-08bb08dfa5a2)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         48bafc01-1bce-4c5c-8e34-88a9528ae5a8)(content(Whitespace\" \
         \"))))(Tile((id \
         2c5fc430-d3a8-4347-8a5c-b74648c48491)(label(col))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         dfd8b3fc-de4a-4f81-86e4-ea23cd94a27e)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         0e919645-e2a4-4e32-a495-32b64032c080)(content(Whitespace\"\\n\"))))(Secondary((id \
         a9b0b0cc-4559-46e3-8e79-780e5a75fca2)(content(Whitespace\"\\n\"))))(Secondary((id \
         a09a7baa-9117-42ba-9be2-9bf32f2e496b)(content(Comment\"# ===== TESTS \
         ===== #\"))))(Secondary((id \
         c6ad6f99-dee7-4764-b935-ae28ccbeba84)(content(Whitespace\"\\n\"))))(Secondary((id \
         52f36b70-5ae6-4046-95b1-beb57210ab04)(content(Whitespace\"\\n\"))))(Tile((id \
         35384283-4169-4936-8fc7-c49e7d9a9020)(label(hint test end))(mold((out \
         Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         46f2c719-75c0-4f37-b7f8-d6c25be19840)(content(Whitespace\" \
         \"))))(Tile((id \
         48ddd048-a52a-485a-839f-d5a724d44018)(label(\"\\\"plant single crop \
         preserves soil type\\\"\"))(mold((out Exp)(in_())(nibs(((shape \
         Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         33480a4d-5534-42e0-a552-9e1b2244e85b)(content(Whitespace\"\\n\")))))((Secondary((id \
         9816d973-23e2-487a-b75f-5cff87a73ba2)(content(Whitespace\"\\n\"))))(Tile((id \
         b8b10e98-1a48-4e3f-bf8f-43178659e5ac)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         60d34d5e-bdf8-467d-8a7a-3dc2c669515f)(content(Whitespace\" \
         \"))))(Tile((id \
         e456d209-d39c-47b8-ad6d-4761721ede3c)(label(m))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         d17eb812-e872-4573-bcd4-176dd02246f6)(content(Whitespace\" \
         \")))))((Secondary((id \
         6ae517a7-2864-4e19-a785-88ac12904117)(content(Whitespace\" \
         \"))))(Tile((id \
         d2f4e44a-02c4-439f-b878-45cc4e5b029f)(label(update))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         b97305bf-f902-49c3-9fab-706449d8069d)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         052a829c-4f3d-4eb0-892e-a88219e028f2)(label(init))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         23e113d4-3f0c-4346-a616-61a07814baf2)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         c9045918-55da-43d7-9822-174251f841c8)(content(Whitespace\" \
         \"))))(Tile((id \
         5285c554-c680-49e0-b903-96ebc5fd700d)(label(PlantCrop))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         dd880f2b-b2b7-4dc6-8d59-780c879f1555)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         7fb9d92d-b099-4f8d-af20-2547a4fe8575)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         11f23301-fad9-481e-b9d2-a7162b158935)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         012f4cbd-647d-44e5-bbd1-848c4287edd8)(content(Whitespace\" \
         \"))))(Tile((id \
         47dcfff8-378b-405f-8396-25346159e2da)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))))))))))))(Secondary((id \
         6c7bc071-d4c9-4296-b97c-4d9e40e3c94b)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         4435198c-5081-4e22-9bea-6f278a530ce3)(content(Whitespace\"\\n\"))))(Tile((id \
         ec2e4f64-2a8d-43a2-bb00-3af0a5d40ba9)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         5a36e748-9b72-45f5-ae0e-dd1c91f4d667)(content(Whitespace\" \
         \"))))(Tile((id \
         db141957-d91b-4a86-8757-15e8cafe0e91)(label(cell))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         31568a35-1f42-436e-966f-5e7ee6275ddb)(content(Whitespace\" \
         \")))))((Secondary((id \
         7ba1ec40-5ede-4965-95b1-1faff8db07d8)(content(Whitespace\" \
         \"))))(Tile((id \
         4b30153f-0605-402f-b24f-50e07994ec87)(label(getCell))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         2db39635-e201-4442-b427-129f626ebe0f)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         3182fcb8-a234-4866-afc1-6f999635d1d4)(label(m))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         8eebb2b7-1a62-4f20-bce9-1be60b9d123f)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         c29ad251-be47-4850-93d8-638d3ce23c2c)(label(field))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         cdaa48b8-8bfe-48a5-a535-4cb2957b075a)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         8d11a5ab-a57e-4191-add2-e358dabb367e)(content(Whitespace\" \
         \"))))(Tile((id \
         fa53701d-7fe5-414b-bd8f-5b98b8e04470)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         86a91c5e-0e39-4113-9432-b1b6003d511c)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         5bf1215b-7a30-425a-b271-8842d39457fe)(content(Whitespace\" \
         \"))))(Tile((id \
         719ad2c4-952c-408f-a49f-6909d0fbe2f2)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         b3fafccb-ea0b-43b1-9181-67b8339f094d)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         5646ea3e-cb72-4b61-98bc-d55c4a163e72)(content(Whitespace\"\\n\"))))(Tile((id \
         2c41fdcf-3dd6-4161-aee8-6183a6d6ddb7)(label(cell))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         f3a3c363-0293-4a0b-b4a8-ef7a40ed8b02)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         a3816868-0f7f-44b9-b5d1-d47e98a6898b)(label(crop))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         6130e8ee-5e3b-4138-a287-dc4279d57e5e)(content(Whitespace\" \
         \"))))(Tile((id \
         4055ffb6-194d-4517-9977-f80b78ee68ff)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         c8e7e982-ec81-444d-a9cd-7210a6f823b7)(content(Whitespace\" \
         \"))))(Tile((id \
         0b7a411c-0186-4510-90c3-e50106aae44d)(label(\"\\\"\\240\\159\\140\\177\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         a876c395-5458-4364-9f08-a44a214f7d6d)(content(Whitespace\" \
         \"))))(Tile((id \
         bb030c5d-b5d5-4687-ad7d-d318db37c546)(label(&&))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 32))(sort Exp))((shape(Concave \
         32))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         0f495f44-039b-4833-8f1c-e7dfd9b744a1)(content(Whitespace\" \
         \"))))(Tile((id \
         2d6bc4d5-25a4-4fd2-9b22-12ab1ef42a70)(label(cell))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         7172cc99-39fc-4fd5-9cf0-39950c65a73b)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         3c1641da-e4ba-4bc3-a3b5-690d518206bf)(label(soil))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         881f1e4c-4f96-4d4e-a956-d057b9fbf426)(content(Whitespace\" \
         \"))))(Tile((id \
         4c903aed-629f-41ac-b9c5-f6095e0d4ac4)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         e6d21c5d-48fa-4a23-bc02-88c35cc92d8e)(content(Whitespace\" \
         \"))))(Tile((id \
         efa072c7-5917-45f7-ac98-021e50af0720)(label(Loamy))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         20d0a9cb-524d-473f-8c27-b254f9fc979a)(content(Whitespace\"\\n\")))))))))(Tile((id \
         8f11722c-9e68-4a2c-8cdb-b2ae0f502530)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         9fdba0e6-230b-48c4-b054-48c43d16144d)(content(Whitespace\"\\n\"))))(Secondary((id \
         e877fc8a-180a-4254-b268-854a9002b0a4)(content(Whitespace\"\\n\"))))(Tile((id \
         fe907d45-2ec4-400d-9e6b-3f1a8f169913)(label(hint test end))(mold((out \
         Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         faaeb348-f9dd-423b-b164-91abd4b8ba61)(content(Whitespace\" \
         \"))))(Tile((id \
         eb49e6ff-2e0e-4e19-8811-430190081cd5)(label(\"\\\"plant at corner \
         preserves soil\\\"\"))(mold((out Exp)(in_())(nibs(((shape \
         Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         af01695c-053e-42b1-b332-1c2e2fe2ff7c)(content(Whitespace\"\\n\")))))((Secondary((id \
         9c6ee2c0-e4f4-4cd1-9a12-d3ba5c8a6cf9)(content(Whitespace\"\\n\"))))(Tile((id \
         995946a9-a1fb-4898-a772-3443417c318c)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         7962d291-be8f-4e0b-b9cd-7d5cb78ba3ca)(content(Whitespace\" \
         \"))))(Tile((id \
         ed97da1b-037c-4733-8bad-fdd374b2b5b9)(label(m))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         ab7644ab-eb54-40ac-8117-a7b1f8a699cd)(content(Whitespace\" \
         \")))))((Secondary((id \
         7bdde084-34e0-424a-8c90-a590a0ccd6aa)(content(Whitespace\" \
         \"))))(Tile((id \
         ca666125-cdca-47b3-bd06-8f8d9a09a96a)(label(update))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         4088d55a-439a-4e7e-b379-11b8ee38c243)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         2675aa4b-b40b-4ff4-b4b9-c5c02e48207a)(label(init))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         0f2cb03d-5b4e-49ca-b470-a6eb869a39f4)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         11e25baa-575f-4782-8920-c9cecc43db7e)(content(Whitespace\" \
         \"))))(Tile((id \
         68ecc0b4-9ab8-4fa4-9fb7-f9ae9d5a8d67)(label(PlantCrop))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         22b0abef-3d3d-4a81-a2f4-bb7b5fcc4207)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         2d53ecbd-1f3c-44c0-9c31-6313745a98e3)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         ba8103e3-176b-4f35-b377-d8c33d6bf952)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         893f8013-3138-4a2d-8c3c-6e9b7936e2b5)(content(Whitespace\" \
         \"))))(Tile((id \
         2411204d-6c6e-4b0d-8509-96de5bc877c7)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))))))))))))(Secondary((id \
         e8eddd03-2e78-4404-bf8a-047552bb4a6d)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         540120a9-9a82-4c9b-9b6c-7bbe3be95db4)(content(Whitespace\"\\n\"))))(Tile((id \
         6fb3b21e-ca4f-409f-aa4c-7541c8bacbf1)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         eeb5362b-c88f-4b97-bddb-5de35ad127bd)(content(Whitespace\" \
         \"))))(Tile((id \
         ce4096fc-6014-4c0a-8826-3f71eb908299)(label(cell))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         9a5aac58-71cf-4eb7-b4c1-a6d0e2b1d12a)(content(Whitespace\" \
         \")))))((Secondary((id \
         9af1692c-6231-4b89-9d17-b9cf927464bf)(content(Whitespace\" \
         \"))))(Tile((id \
         b38c3305-334a-4d74-884d-cc36436ee3d1)(label(getCell))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         f876bfd7-2522-42e7-a845-3aa6b8f6085d)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         ddc1110e-1915-4372-b409-3a74cb4d831b)(label(m))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         7ba15d82-ce73-45e7-9c73-a5854d12797e)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         d5092542-9864-4e71-9ce7-c3db8bf7b9ce)(label(field))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         75410452-4e22-4f3c-8087-581992c53f24)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         560b2b43-f503-4aab-a1b8-827cd4c22de2)(content(Whitespace\" \
         \"))))(Tile((id \
         9f96c37d-193a-48c3-b62b-28deb261b38c)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         2946c747-d6da-4ffd-9474-b3816eb11e99)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         aa7ed337-823e-4ee2-b8a0-5d9a473dd259)(content(Whitespace\" \
         \"))))(Tile((id \
         53d5f2cc-b099-4774-b273-1ae0a7bd33b9)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         2f2c7015-0668-471c-a7b6-c1942b901775)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         06cff7da-8982-4674-8f27-6258e09d972d)(content(Whitespace\"\\n\"))))(Tile((id \
         e9c2cc1d-6ae5-4f08-8fa7-aca6a5683875)(label(cell))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         ecc560ff-96e9-4b76-8b02-5811217f9864)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         53626f84-6d9e-4b59-950f-05fa384033a0)(label(crop))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         c2961f50-5468-49c9-83cb-43f337389ec7)(content(Whitespace\" \
         \"))))(Tile((id \
         5de3bab4-1b08-4613-bb70-ff7fe4251fc7)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         f3c70a47-b9e8-4e59-99f6-a46cff2efd64)(content(Whitespace\" \
         \"))))(Tile((id \
         4c8bd772-58c4-4073-bb39-e360dbf57b33)(label(\"\\\"\\240\\159\\140\\177\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         4c7e31a5-c456-4fc1-9666-947a64b8821f)(content(Whitespace\" \
         \"))))(Tile((id \
         d32b2e08-4bce-44e7-9c08-3d0959e578f6)(label(&&))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 32))(sort Exp))((shape(Concave \
         32))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         4b121044-478a-4246-ae10-51711e80d7bc)(content(Whitespace\" \
         \"))))(Tile((id \
         7e49d5aa-c71a-491e-b6b2-6e69558ae3c6)(label(cell))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         faf8bdf1-3f5c-468e-9166-fc64b3f95c93)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         81abb1da-aff3-4634-adcd-7953e0e62801)(label(soil))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         d0fdeb42-0b06-413f-a80f-657d3f160011)(content(Whitespace\" \
         \"))))(Tile((id \
         30d6506c-7bce-4ff2-9c74-307b141165bb)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         11efde3f-04a9-48f7-8380-3fa81c4ce481)(content(Whitespace\" \
         \"))))(Tile((id \
         c6deea4a-659d-4a3e-9b1c-cdfe4ee28161)(label(Loamy))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         74705c9d-04e4-440f-99e0-5f08c7e4454f)(content(Whitespace\"\\n\")))))))))(Tile((id \
         bdd9afae-fb99-49ff-98ae-42bad9673d14)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         1cc6e2eb-f20a-47ae-90d2-b1c20fa183c0)(content(Whitespace\"\\n\"))))(Secondary((id \
         aa7803d0-2292-48b2-a881-d354cde86f01)(content(Whitespace\"\\n\"))))(Tile((id \
         a66eab0e-8ec5-43a0-85da-5288a1e18f31)(label(hint test end))(mold((out \
         Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         93c7aae0-099f-4ab9-a0fb-971ca40c083a)(content(Whitespace\" \
         \"))))(Tile((id \
         edb3b2c6-eae7-46bb-b33a-bb933e780601)(label(\"\\\"plant at different \
         position\\\"\"))(mold((out Exp)(in_())(nibs(((shape Convex)(sort \
         Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         82f68a9b-66da-424b-b75c-3e5f40bd36a2)(content(Whitespace\"\\n\")))))((Secondary((id \
         1d1190f1-bd65-42ec-93ba-b5cf1e1f0cef)(content(Whitespace\"\\n\"))))(Tile((id \
         3b232826-11e3-487b-bd39-e0563d9cab8c)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         34cedfdc-241b-4b49-bc07-4f5e1db5cde4)(content(Whitespace\" \
         \"))))(Tile((id \
         6a54bb64-3e09-49aa-9d2b-30f455fff48c)(label(m))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         a4d8db98-7c54-4bd8-b203-32108ab5913a)(content(Whitespace\" \
         \")))))((Secondary((id \
         b0ecaca3-e63a-4863-a729-1fecd8e46673)(content(Whitespace\" \
         \"))))(Tile((id \
         f17c600f-a8dd-4470-af44-5f6857ad9da4)(label(update))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         6742b525-0910-4bea-a86a-fe709d8263fb)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         326b9dd1-95be-4a9a-aa34-8a57a51b06cc)(label(init))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         6ff1fd5d-ed36-4179-aabe-f6ad26ce976f)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         cbaa8130-7b93-4653-a050-8feccfb4ed0e)(content(Whitespace\" \
         \"))))(Tile((id \
         056af2d5-1abd-41ae-aa38-46430d99758c)(label(PlantCrop))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         cc053dd0-328a-44a8-b392-923b10e1ef55)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         3dcd72d7-2a64-422e-b8a1-1e5835c242fb)(label(2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         3b114d66-ea28-4555-b583-44297ea81b1a)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         36ab08eb-55a1-4515-a021-d97605910c1f)(content(Whitespace\" \
         \"))))(Tile((id \
         09ab9152-a050-4d76-9e88-e5f42f601ba1)(label(2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))))))))))))(Secondary((id \
         63871eed-d710-4acf-bcaa-f576f135cbb5)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         fe3e4204-370f-4612-8d44-453606b7f76f)(content(Whitespace\"\\n\"))))(Tile((id \
         37a6347f-722c-4c45-8b6d-02009e70e654)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         53c0586d-2ae1-46a7-9b24-ddb158bcff9a)(content(Whitespace\" \
         \"))))(Tile((id \
         b53788ec-1948-400d-a9d0-6dbb668f6103)(label(cell))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         2fbf440b-f6d1-47c0-8f3c-67fc83e9e94a)(content(Whitespace\" \
         \")))))((Secondary((id \
         ca9a74da-2c4b-42c1-a7f4-ba2fa576c546)(content(Whitespace\" \
         \"))))(Tile((id \
         4a678b4c-94c9-4c23-b48f-768bbfdad423)(label(getCell))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         5232dc86-cccd-4e04-8854-7646d246453d)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         ae135d26-8bad-4366-8e7b-0d18977ac7ec)(label(m))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         6c6deeaa-bb6e-407f-b29d-29dc026c94af)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         c57b683b-6526-4b40-b3d2-d6c174f055c9)(label(field))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         503f16e5-b5ce-44f4-b6ca-97038d9254ce)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         0063f0f3-91fc-4450-97ad-a4de89778a16)(content(Whitespace\" \
         \"))))(Tile((id \
         4e14a68d-6f5c-47d4-a46f-22e5f1bf31b1)(label(2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         1192e172-c413-4ddc-9cbe-52acfa230676)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         88eb38e1-75d1-4f3c-bb57-3c4e7901ceb7)(content(Whitespace\" \
         \"))))(Tile((id \
         7e9a9659-0eb5-41b3-a54f-e52b23f79a13)(label(2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         f0e8b3f9-dc19-43ab-a97a-2029a7f497de)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         663f88ae-64ee-4d68-b3be-42ab757ec86d)(content(Whitespace\"\\n\"))))(Tile((id \
         b6fa8836-8054-4076-b575-532c2b01c211)(label(cell))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         45c14444-81d2-425f-9b82-4229b43388a0)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         ff6cff6f-6d4a-484e-a3b6-09131c2530a2)(label(crop))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         52e97b23-6130-4c93-a9b1-026a9c4293fa)(content(Whitespace\" \
         \"))))(Tile((id \
         aaf7cfab-5412-4655-a293-23834e81b1c5)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         1c322013-0122-4c33-b9a0-399df2a0f26a)(content(Whitespace\" \
         \"))))(Tile((id \
         a8d4bb57-6cc8-407e-81d8-67eb8e578427)(label(\"\\\"\\240\\159\\140\\177\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         8d4de477-57f0-4664-acbf-72b9b0f3f8d4)(content(Whitespace\" \
         \"))))(Tile((id \
         0e0442b5-9b85-4a33-869f-498aa067c7a4)(label(&&))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 32))(sort Exp))((shape(Concave \
         32))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         f8a6ff56-4f99-41d6-aef6-a3d07b719966)(content(Whitespace\" \
         \"))))(Tile((id \
         cbbb60a2-0541-41e3-9fba-990e3448098f)(label(cell))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         c7d21ca7-18bb-4d73-af2e-9f71bd7ab07c)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         aa7c2c46-3cbc-4495-bfdb-e099ae8c30a6)(label(soil))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         25bdcee7-2566-45eb-aa2f-0dfb5dd049a8)(content(Whitespace\" \
         \"))))(Tile((id \
         2dde1bac-6cfc-4e43-90d3-26135061ce81)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         9b8083a3-29f3-41e7-bd12-3c1f75eb6d9c)(content(Whitespace\" \
         \"))))(Tile((id \
         9f551103-31be-44f2-bc52-9fdfa0c44027)(label(Loamy))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         22f862b4-6cde-4dcf-b344-a759866733d3)(content(Whitespace\"\\n\")))))))))(Tile((id \
         acad6113-8eb4-4a9f-9ed3-0041942940a8)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         d0f2c72f-d29c-4fae-8f96-dff092cefe13)(content(Whitespace\"\\n\"))))(Secondary((id \
         7b362953-ab11-4c5d-adfc-f4de0c0281c8)(content(Whitespace\"\\n\"))))(Tile((id \
         aa054a7c-5d8a-4e1a-bc73-937f4609b8e0)(label(hint test end))(mold((out \
         Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         126cd38c-324b-4d26-b927-e2554f2c8a60)(content(Whitespace\" \
         \"))))(Tile((id \
         c08515e8-b1fb-4583-9df5-1488733e0997)(label(\"\\\"harvest removes \
         crop but keeps soil\\\"\"))(mold((out Exp)(in_())(nibs(((shape \
         Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         1446b68c-c353-4db5-ad4a-62bf074769c2)(content(Whitespace\"\\n\")))))((Secondary((id \
         38c78ee9-f2ed-4e4a-afc8-ab8a49680559)(content(Whitespace\"\\n\"))))(Tile((id \
         2b64788e-8938-440a-a178-d97aab02e90c)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         e2afab8e-029c-4dba-8250-a402b369944a)(content(Whitespace\" \
         \"))))(Tile((id \
         c8b9a832-e981-4878-b2d6-c2beca90b81d)(label(m))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         e71d675d-7a11-4e04-a8b0-232eabd3f82b)(content(Whitespace\" \
         \")))))((Secondary((id \
         5ef367db-4d8f-4fa6-bd69-3bd546e7837e)(content(Whitespace\" \
         \"))))(Tile((id \
         19ea5ad5-cf28-41d4-8f9b-eb2fb0098734)(label(do))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         8ed7c863-a804-4184-909a-c81bc78bc728)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         728a3474-8253-4d8e-a792-e00b3a6decd7)(label(init))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         c4349542-de03-433b-b570-2b0e53714a81)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         6efb85d1-d0bd-40f3-bc8a-ea1c4732ee1c)(content(Whitespace\" \
         \"))))(Tile((id 51b6b4d4-d7e3-48b2-a3b5-999f5f490fe9)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         3cd33894-c7d6-4958-b7ef-8a74265c982e)(label(PlantCrop))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         5145b8cb-3c23-4a26-943d-1fc871c387a4)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         102adf9f-40a9-4553-a47d-d610f53e30f9)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         48078a79-3c06-4eb3-9622-62ae420303a9)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         57a1cb56-951b-4c9a-8ac3-b263875cde38)(content(Whitespace\" \
         \"))))(Tile((id \
         d22dbe80-310c-47b8-a21a-1158c51ab01a)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         6fc34c8e-37a9-4c98-b0a7-7c807940798e)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         286388f6-c5e7-4d23-bd65-844e5ec94a33)(content(Whitespace\" \
         \"))))(Tile((id \
         2fda43eb-59f5-42f0-8bf7-db03a831906f)(label(HarvestCrop))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         af0c2d71-28ff-486e-a0ac-f3d6fa0f0de9)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         6c02e189-ce64-4aa7-aab1-3833df025377)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         8a99df89-2acf-46ea-9058-985fe5262094)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         aeb31b3b-4627-47ea-a1aa-19cb67e59a65)(content(Whitespace\" \
         \"))))(Tile((id \
         f718848f-34c0-4dbc-a4f1-ed236532b6be)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))))))))))))(Secondary((id \
         9796af7f-c9ea-4547-b706-77c049c58581)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         a87f5244-62b1-4428-9dc2-7ca4b6b185e2)(content(Whitespace\"\\n\"))))(Tile((id \
         8a3d8797-4cdf-4d94-834f-690fd7e01992)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         f9230c10-68b7-46c8-b953-c5e55079db16)(content(Whitespace\" \
         \"))))(Tile((id \
         ff9f7895-4a70-49ab-a22f-f6a88192f167)(label(cell))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         022aaa7a-a04a-4173-a39d-d59e8461dcac)(content(Whitespace\" \
         \")))))((Secondary((id \
         b9e2ee19-204a-4f13-9257-e834f75e370a)(content(Whitespace\" \
         \"))))(Tile((id \
         f3446400-eef9-45f1-aee3-d8d27b81c5ac)(label(getCell))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         260644a4-a363-480f-ace1-543a58b08c9b)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         78a8615e-2166-4f83-8dc6-6b8fe28cf796)(label(m))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         60ddfb2f-e7da-49b7-a819-4590be35385c)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         3c35af5a-1cc6-4203-8db8-8d375f0d2d2f)(label(field))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         936337db-9cf1-4feb-98d4-c22737ee1ce1)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         20754931-63ed-4d40-91dc-07c8c1518132)(content(Whitespace\" \
         \"))))(Tile((id \
         ddedf967-f88c-4a3f-b520-aac1d6c12802)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         ea9cbeec-4a5a-4e4b-b410-6a66c75287dc)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         16d92f30-5101-4c75-b7bd-e3f6022da181)(content(Whitespace\" \
         \"))))(Tile((id \
         14046a88-6a2e-40a6-b287-14218b7f9e5c)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         5f63806c-6c14-41d5-9c5e-a54f9d7fc3b7)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         ddbebb78-9db1-4fc6-adf4-42349c27ee4a)(content(Whitespace\"\\n\"))))(Tile((id \
         1e3d0c57-bf2c-44e8-ad18-bcc9dd967ebb)(label(cell))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         39c0b89c-27e8-44fa-9ba0-8441fe4a48dd)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         b8f4115d-44dd-4de9-aa4b-30cfd751cbb0)(label(crop))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         9b508c91-00b5-45c5-8c76-54360816aff3)(content(Whitespace\" \
         \"))))(Tile((id \
         c3ea5bfd-d14d-4a8b-a2b4-c8cf0a7b2283)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         e9470b3a-3ee6-4646-9207-4746a1420f50)(content(Whitespace\" \
         \"))))(Tile((id \
         a5881763-09e3-4f71-b5f3-075688ea52a1)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         6f00a3a5-1f37-4b0e-8e4b-ab2658734d9c)(content(Whitespace\" \
         \"))))(Tile((id \
         03052c1d-7abe-4a27-a2ae-1ac72a920ba3)(label(&&))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 32))(sort Exp))((shape(Concave \
         32))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         09dcfc00-0523-47a3-b274-58a97823cc2c)(content(Whitespace\" \
         \"))))(Tile((id \
         ce546b6b-3a9e-4390-ad23-ff9559c664fe)(label(cell))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         d6103b5b-a412-439a-9d22-568a4ba3dabb)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         1db6bd48-7326-4168-95c4-250c52d6d2b0)(label(soil))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         f3365303-82ae-43fc-96d7-4ae99b18d6e3)(content(Whitespace\" \
         \"))))(Tile((id \
         c852a8be-4c4f-4dff-9a51-c9f4dce58f17)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         9d40a5a8-c0d2-4c58-911c-37708b4aac4b)(content(Whitespace\" \
         \"))))(Tile((id \
         43708b50-e688-4fda-a8a2-6922bee5be8c)(label(Loamy))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         3486ffee-5c48-4d72-b3bf-20172af4b980)(content(Whitespace\"\\n\")))))))))(Tile((id \
         e9aab9d9-93e6-421d-82bb-712d1c454949)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         6546b0ba-2076-4622-b0fd-907db3576d38)(content(Whitespace\"\\n\"))))(Secondary((id \
         a988eb83-7626-4b1e-aae1-11a8d471a699)(content(Whitespace\"\\n\"))))(Tile((id \
         0198b81d-23b3-475c-8cb8-3cb9739ee039)(label(hint test end))(mold((out \
         Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         e7045fb7-3084-4e98-b631-7106bc9ea216)(content(Whitespace\" \
         \"))))(Tile((id \
         8ea9add0-b375-4dc7-aa54-a0911c334e66)(label(\"\\\"till soil changes \
         soil type but keeps crop\\\"\"))(mold((out Exp)(in_())(nibs(((shape \
         Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         8a96f305-98cc-4771-8f94-a68fbf785b15)(content(Whitespace\"\\n\")))))((Secondary((id \
         c97edf6c-e3a4-41f7-a7b8-0ce2653f4bc8)(content(Whitespace\"\\n\"))))(Tile((id \
         10afdfb9-556a-4ed6-aae4-e37a8b96cd81)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         935fc0c7-8f9a-400f-9ac6-e2e57ef4010b)(content(Whitespace\" \
         \"))))(Tile((id \
         ed6278b9-13d6-443e-ab79-6fd041571a77)(label(m))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         aaaf99cc-c6f3-43c7-b06f-40e29e24a559)(content(Whitespace\" \
         \")))))((Secondary((id \
         74827402-2639-49f9-9a87-e0eebc54ea8d)(content(Whitespace\" \
         \"))))(Tile((id \
         1d1f8499-f42b-4e3e-945d-7d93e1c85c36)(label(do))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         632c8bcc-5ffc-4d9b-b9cc-c42227446775)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         665c19d6-0753-46b5-9ffa-c5cd72cff085)(label(init))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         5c918b69-8123-401c-9122-b71fd4668d3c)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         0561a0e6-0925-4d06-ac77-cc8fe3883f59)(content(Whitespace\" \
         \"))))(Tile((id ea21d95f-65ad-46f6-8cde-341b3758677e)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         0daa5704-cb68-4b70-9fc6-0d3d81bfdd46)(label(PlantCrop))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         b2953603-1455-4646-9a8b-4cc6967779e3)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         ab85a1f4-6cd5-4802-99c7-add02eedec32)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         b329aa97-0d86-4492-91fd-2f9156f1d8db)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         dd879423-7b24-4f32-b50b-a2fa0a9c50b8)(content(Whitespace\" \
         \"))))(Tile((id \
         697384bc-20f9-46a4-8770-915e0658c2c0)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         a312a885-dc6c-4129-9a31-8a4cced0b9ff)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         7b278e4d-3a3d-47e3-9f57-e403f9352d13)(content(Whitespace\" \
         \"))))(Tile((id \
         eedff821-21e2-4c2c-a129-4ee3812a6e82)(label(TillSoil))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         ab7c03bb-fde9-45d1-8f8d-a20ec13695ae)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         a898ae6c-5e4b-4b54-acfb-e48772b8abb9)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         90121a94-c9d7-441a-a409-48fdad70a041)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         bd7607c6-f70b-482f-b507-9f243551320d)(content(Whitespace\" \
         \"))))(Tile((id \
         85708013-ac03-4377-b71b-957164eb9e84)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         8cb0392b-1f6d-4e24-8d7b-50b90d64864b)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         d6a6791a-af2a-4317-a2bc-bce9c926983f)(content(Whitespace\" \
         \"))))(Tile((id \
         a7265595-65f2-44b3-b19a-91709f3b578b)(label(Rich))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))))))))))))(Secondary((id \
         9e6e84be-1e22-4578-b586-606d520598e7)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         f48036ed-5c47-4546-83a3-c5dfb9eb233e)(content(Whitespace\"\\n\"))))(Tile((id \
         95a3e931-244a-4788-b4e7-1ee592b89dc4)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         5c1aeb80-2c03-4208-b0dd-f86d920d48b1)(content(Whitespace\" \
         \"))))(Tile((id \
         baa0de35-c28b-4d16-a9cc-98ab1e5385da)(label(cell))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         485c540c-da70-4070-ab21-e46fdc8d27e9)(content(Whitespace\" \
         \")))))((Secondary((id \
         3ae889a6-abbd-4184-a523-3e6dca036386)(content(Whitespace\" \
         \"))))(Tile((id \
         802b57e7-80e2-4742-97fe-84dffddedc01)(label(getCell))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         4913a946-9d86-4b13-9e43-a73bbad7e3ea)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         1bef4755-297f-45ab-9872-cda6b8633a4e)(label(m))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         c2e16c23-02d3-4f70-8a26-0f463f94d75b)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         1d7b6162-4446-452a-84ab-b9778bbcf4d9)(label(field))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         417cc399-4c1a-48cb-a1e4-85b464e840b5)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         530a40ca-1164-4167-a42e-a0128ad43327)(content(Whitespace\" \
         \"))))(Tile((id \
         941b9538-0fe9-4509-aa03-602a3e0bae50)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         6eb81dc8-8ded-4435-b66c-aa01d32f74f0)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         98ef77ad-acb1-464a-83d0-849bba467c9e)(content(Whitespace\" \
         \"))))(Tile((id \
         7d3bb917-b3e8-490c-83b6-fbf3a1b227c4)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         7335676b-2c2a-4345-9836-8a04320886ed)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         1fb16720-4282-44fe-8704-a4756845295e)(content(Whitespace\"\\n\"))))(Tile((id \
         a23e19ad-298b-4146-b958-b7961b3b4886)(label(cell))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         c47647c5-f077-40c7-bb58-7310aa5b1007)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         b4a80e5b-48a7-49c3-9e6d-3601afdd8892)(label(crop))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         50753173-39cb-42ad-b939-2a6e9489e890)(content(Whitespace\" \
         \"))))(Tile((id \
         c87b8ec2-2fe1-4280-860a-471aea221900)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         ba9e7479-dcc5-4fab-8cba-6efc4ea4e6db)(content(Whitespace\" \
         \"))))(Tile((id \
         488fbf50-5833-4ffd-b68c-4ae3ca316abc)(label(\"\\\"\\240\\159\\140\\177\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         abff17c3-8780-4d6d-b7d4-96250f2981d0)(content(Whitespace\" \
         \"))))(Tile((id \
         0762fbd8-aa48-41ce-8219-7c2e90b4382c)(label(&&))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 32))(sort Exp))((shape(Concave \
         32))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         80031ae4-367c-4321-974d-8fb9c30c8a46)(content(Whitespace\" \
         \"))))(Tile((id \
         965cdd62-b6d9-4aae-ae91-3e2f6d6aae25)(label(cell))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         3cfba3b2-e79d-4deb-b1df-8c2eab70d001)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         06778a7a-f02a-4546-89ef-080e734049c5)(label(soil))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         17fe844c-83b4-4886-9fe9-d8d291c7061f)(content(Whitespace\" \
         \"))))(Tile((id \
         292112b2-cd85-4514-841e-2358abec5936)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         6b1b3906-e454-4288-bc04-986fa627ed63)(content(Whitespace\" \
         \"))))(Tile((id \
         7801b0d4-4f5d-4e35-aa40-6a0f923694cd)(label(Rich))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         367216dd-2e79-474c-a609-3a911f511ed3)(content(Whitespace\"\\n\")))))))))(Tile((id \
         7dbf562e-2585-449c-b005-cf3847fbfd1c)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         e6b6cae5-0169-414a-bb59-c5ac36886eb7)(content(Whitespace\"\\n\"))))(Secondary((id \
         b4d30bab-ea3a-4387-99e2-9cb142053292)(content(Whitespace\"\\n\"))))(Tile((id \
         93884727-43ab-4f0c-ba8e-bab80e947724)(label(hint test end))(mold((out \
         Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         46d66871-69fb-4c02-a8c2-78c18e357495)(content(Whitespace\" \
         \"))))(Tile((id \
         33df363d-7c20-46e8-9a18-b313cc11aa13)(label(\"\\\"plant row fills \
         entire row\\\"\"))(mold((out Exp)(in_())(nibs(((shape Convex)(sort \
         Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         0944ba61-8358-4637-b0ca-8413318e9441)(content(Whitespace\"\\n\")))))((Secondary((id \
         338005be-d6c5-457b-afd1-6660082d1081)(content(Whitespace\"\\n\"))))(Tile((id \
         205a79a9-fbe5-4177-ae37-5333c6906533)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         17cb68fa-7120-4365-bd54-a54fe0413480)(content(Whitespace\" \
         \"))))(Tile((id \
         ed0f3ac5-cd56-4149-97de-e15afdbf7bb4)(label(m))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         bde0363f-7308-4831-88df-a6043d1bd771)(content(Whitespace\" \
         \")))))((Secondary((id \
         e986ec94-3483-43a5-950b-90373ea05ceb)(content(Whitespace\" \
         \"))))(Tile((id \
         30f0b214-176b-4f8a-8de4-56b241c052ce)(label(update))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         a147819f-b45e-4fee-a8c4-c50f14d39087)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         f3f1bcc7-3a25-4219-8ccd-17dc0de75159)(label(init))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         67b904dd-8ff0-4a24-83f8-f3dd41ef373b)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         93928d88-477d-4181-a961-670d41945b8a)(content(Whitespace\" \
         \"))))(Tile((id \
         494ea5dd-ae5f-4bae-a9aa-fdfadc72acdd)(label(PlantRow))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         71b259f2-a7a9-42fe-ba9f-c6ad3db96db7)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         54c7a39a-fbac-4d3f-86ea-a4faa67215c2)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))))))))))))(Secondary((id \
         d94caf25-f529-4acf-91b1-b2e7c313dc96)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         4a804d0f-0bb4-4bad-9bd6-c7aca680c761)(content(Whitespace\"\\n\"))))(Tile((id \
         6dca983c-1057-42da-948e-ffba4d109074)(label(m))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         ed0869ff-28e0-4c12-a144-bfb7f63cc0f3)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         b9bd34c3-fa05-4b08-85b7-b7cedc529d3a)(label(field))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         b1214097-0efb-46c5-aa65-d801db5c8809)(content(Whitespace\" \
         \"))))(Tile((id \
         00206cfc-cfbe-4639-8f25-2415bb54c083)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         4f1d7989-c883-4fa9-85dc-02208431c750)(content(Whitespace\" \
         \"))))(Tile((id b23fbcc8-e3ba-43de-b00f-7cfa40c81ac8)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         511c99ff-c808-4a06-a02d-00aaf8ef999d)(content(Whitespace\"\\n\"))))(Tile((id \
         3e5f1fc7-bf8b-4eae-b24f-65524e10b7ff)(label([ ]))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         60a60c33-2959-40d8-8e4f-f1c4045d4541)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         a823898a-22bb-4636-861c-927661be9d78)(label(crop))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         61e6cd27-c3d6-4edf-9ac8-042d9849b850)(content(Whitespace\" \
         \"))))(Tile((id \
         ef778f32-537f-4df8-bf15-711249504d29)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         bab37618-5052-4f47-ab56-294c7aa2e6a7)(content(Whitespace\" \
         \"))))(Tile((id \
         c64d7421-35cd-482e-9e38-edd71f2e6e32)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         894ce007-dca3-4bd6-a888-3b5d2bc08825)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         2ca6ede5-032a-409e-af8d-541ff33b52a3)(content(Whitespace\" \
         \"))))(Tile((id \
         cdbfb480-a287-4d94-8953-7fd0732fd515)(label(soil))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         367853d3-1c8f-4426-b772-ac3f354536b8)(content(Whitespace\" \
         \"))))(Tile((id \
         c7baa777-6a73-4f77-af32-2017711d8600)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         3e5e9c52-09aa-4781-b151-540dace3da5c)(content(Whitespace\" \
         \"))))(Tile((id \
         ae4032bf-b449-42a9-b1db-361c812ec5e5)(label(Loamy))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         5e7b51a1-c167-4feb-8851-2958646e6383)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         6e41d242-cabb-46be-b664-7196551e2480)(content(Whitespace\" \
         \"))))(Tile((id \
         de74a55c-0ec5-41f4-989b-1edb6912910e)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         72c74795-de0c-4af0-b03c-1e8856fd3b63)(label(crop))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         b8739755-ee56-4de4-82f7-6d807ae838bb)(content(Whitespace\" \
         \"))))(Tile((id \
         c7a6300e-098d-4147-a873-a396075a5424)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         e83e15af-fb78-4d85-b275-9e8521086a79)(content(Whitespace\" \
         \"))))(Tile((id \
         8429d21b-6bff-4f44-8cae-87ca1d51ff72)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         146c1fb1-c179-45b6-b5e3-daedbed4ce8a)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         eff94705-a23b-4cc5-aac3-181899d102fc)(content(Whitespace\" \
         \"))))(Tile((id \
         dd6c85e8-e5c5-4366-9eb4-f670fdd487c0)(label(soil))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         4ba82e9b-4f35-448d-a381-466fb22d7f1f)(content(Whitespace\" \
         \"))))(Tile((id \
         3d129533-bf33-447c-bd49-239fb0a6f851)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         edca3378-ed2e-4937-ac3a-40d7ff839433)(content(Whitespace\" \
         \"))))(Tile((id \
         33e0c0cb-a7c2-4825-b2ba-e344f8e3890c)(label(Sandy))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         de0b5e4c-ad23-4ebc-8a27-b307c8b9f9e3)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         5904d7f0-b0ff-4182-b8de-185435284b59)(content(Whitespace\" \
         \"))))(Tile((id \
         3a23f510-5a90-4737-b1f7-e20be50c3123)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         c71ec8c8-9ca4-4746-8bc2-e64b09df3bc3)(label(crop))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         ce137f6d-83c9-4d4a-b572-265523ce4e9f)(content(Whitespace\" \
         \"))))(Tile((id \
         fefa4d73-d406-4c94-bbdc-4bd300de52ce)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         3d51c6ee-46f5-453a-8e12-06cda5541e14)(content(Whitespace\" \
         \"))))(Tile((id \
         53502a41-9631-4d1a-a5e8-88900619007e)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         33c6b637-8942-4735-acbd-11e8af1c8db7)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         fdb449ff-982c-405b-90b7-8bf7f5cb4cd0)(content(Whitespace\" \
         \"))))(Tile((id \
         c8a41eda-ab7d-4a23-a1b8-61b4dff639bd)(label(soil))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         bf5e84ba-399a-406e-9331-ddc64c4c68af)(content(Whitespace\" \
         \"))))(Tile((id \
         f3c452ea-aadd-4fe6-a408-5aad48ade3f4)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         e64b2b89-2e42-43c6-ae1c-b920940b20d8)(content(Whitespace\" \
         \"))))(Tile((id \
         a8640c0a-7156-4059-bbae-18996d4b351a)(label(Clay))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))))))))))))(Tile((id \
         dda934e2-9202-42a3-a4a7-d6d7cb6c0df0)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         491993b5-5a45-41fd-b291-bb2ba712c740)(content(Whitespace\"\\n\"))))(Tile((id \
         8850aa1f-0834-4b6e-8195-e558d2f8cacd)(label([ ]))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         fbb1a3f3-4169-432d-9ede-27a60013a3e5)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         2b059875-35d7-44f6-aff3-c4ad85c4d311)(label(crop))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         05b0bd5b-836a-4330-acc3-fede1dba2905)(content(Whitespace\" \
         \"))))(Tile((id \
         f4e1f9db-9e3f-45f0-adb4-4f98496070d8)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         f7e13bdb-641e-4ef2-ba30-9b05d2d2d104)(content(Whitespace\" \
         \"))))(Tile((id \
         8ad6765f-e4b2-4ad2-9593-52b9f798dbef)(label(\"\\\"\\240\\159\\140\\177\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         e776a086-b423-421d-a0aa-7a4b7c7d6ae7)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         05cd85be-ccfe-4a2a-a88d-68edf357dd9e)(content(Whitespace\" \
         \"))))(Tile((id \
         d8c74609-ac21-44d3-b02d-d21677e4920b)(label(soil))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         b9620cd2-f2d9-4e96-9f12-8006cea8032a)(content(Whitespace\" \
         \"))))(Tile((id \
         86ad847f-ad4f-455f-86eb-6e00da6d4db2)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         a8717c26-20c5-4e8f-a4f5-bd81e44065a5)(content(Whitespace\" \
         \"))))(Tile((id \
         517b3e45-1565-46ec-8eba-3882239d09cc)(label(Rich))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         c639d652-33f8-4e04-ad6d-420e7f411362)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         6f67fa6b-2d4b-4ba8-bc67-e6cd9c0100f3)(content(Whitespace\" \
         \"))))(Tile((id \
         78f45245-b5c5-43f8-89fe-5c830de3e10d)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         719187ca-da1e-4319-8641-64ac0dbfd7bf)(label(crop))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         6c3e21c5-e363-4a2a-b11e-2b0afb68375b)(content(Whitespace\" \
         \"))))(Tile((id \
         e882a9d6-f955-4173-b5b4-810862f97f97)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         2f6ad08c-5ed2-46a2-85d1-a58bd1e77f69)(content(Whitespace\" \
         \"))))(Tile((id \
         c9758af3-f21c-4540-b2b5-d392c80ac83d)(label(\"\\\"\\240\\159\\140\\177\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         ef5bd56b-6f5d-48ce-9abd-b093ecffa5d0)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         12d4f1de-98e9-4c89-929b-f14fa06d32a6)(content(Whitespace\" \
         \"))))(Tile((id \
         5a8cb5d3-d3da-46a2-88b3-066e365433cf)(label(soil))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         561a8cdc-2c1e-430b-bb10-a03d6808e294)(content(Whitespace\" \
         \"))))(Tile((id \
         ef1dc647-4ddc-49f8-b5d3-8916f018b6f3)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         3164df9c-6cf7-4b3c-bdb7-2c61860445a1)(content(Whitespace\" \
         \"))))(Tile((id \
         b001efed-3a45-46da-81db-3c7b7b07b52b)(label(Loamy))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         fbb711ea-a016-4b46-aebb-1d25211b414b)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         1576a511-f457-459e-9c5b-67d578b0765a)(content(Whitespace\" \
         \"))))(Tile((id \
         8f3a620a-5814-494b-aa87-c71e5ce35319)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         97650a46-5307-4b41-a725-ad17d589f2bc)(label(crop))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         0ef979c9-a798-42c9-843e-5375825e4ee7)(content(Whitespace\" \
         \"))))(Tile((id \
         76dc2063-c61e-4cd3-8187-d0e021d7a370)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         81a6a889-e826-4ce2-b33e-310dfd1598f5)(content(Whitespace\" \
         \"))))(Tile((id \
         1b484cff-12de-4572-a59e-caea1ecf8352)(label(\"\\\"\\240\\159\\140\\177\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         c7d39068-93f7-424a-abd8-d38a3c889a87)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         192cbfde-cda2-4d46-a55d-10b78c69d5ce)(content(Whitespace\" \
         \"))))(Tile((id \
         d9728c88-6a3a-4946-822b-023042612e7a)(label(soil))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         a78f0457-4f0f-49bb-a6a4-4fc028e6ffa8)(content(Whitespace\" \
         \"))))(Tile((id \
         9b3efbd5-f7d3-4a2a-be19-f76a05d73e61)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         6448d032-dd1f-4ff0-9998-e7bf62ea8cc5)(content(Whitespace\" \
         \"))))(Tile((id \
         f028d52f-5295-428b-b1fc-79f787c0231e)(label(Sandy))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))))))))))))(Tile((id \
         4858cc68-5d29-494a-8b94-10856ae422c3)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         f2d94ffb-7554-4783-8cbf-a3ab5734e342)(content(Whitespace\"\\n\"))))(Tile((id \
         df946196-6b74-4b90-9bc0-d537ecd5a8db)(label([ ]))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         73e28797-2e9e-45af-89f0-8b09dbff592f)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         0669b9f9-1555-4afe-9627-3b105ec5295c)(label(crop))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         696c8d48-f971-446f-b085-48f54dab02e4)(content(Whitespace\" \
         \"))))(Tile((id \
         ddfbe698-a7ad-4637-8660-c138b7146dab)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         db469e47-a862-4cb1-b919-546232e9985b)(content(Whitespace\" \
         \"))))(Tile((id \
         7de8c0d6-6536-43ea-a46e-528181d4e007)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         dba726a9-b03b-47a7-8e85-3c8895958176)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         e90c7199-ed29-457e-ba9b-1b03064b8873)(content(Whitespace\" \
         \"))))(Tile((id \
         86e5adad-d050-49a0-b9ee-d15a7fcab2d1)(label(soil))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         962e94db-c0b5-44cb-a003-5e19829d2c84)(content(Whitespace\" \
         \"))))(Tile((id \
         5b628710-d1b2-40fa-b86d-1562f8b5a33d)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         d107f665-7fc2-41f6-90e2-f19485564cce)(content(Whitespace\" \
         \"))))(Tile((id \
         40fd3a9b-94be-4868-966d-866486b34b18)(label(Clay))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         175a81dc-1608-4edc-a9a2-630dde96aa67)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         058d322a-6bde-4dca-8d7a-d518b78c599a)(content(Whitespace\" \
         \"))))(Tile((id \
         d7ace351-cb6d-4870-a7a7-5ad6e4cfbe9e)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         f07bd2c5-943f-4cf6-903d-8f087d276816)(label(crop))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         8c040565-5849-45b8-b316-9f6fd819b8bc)(content(Whitespace\" \
         \"))))(Tile((id \
         169a8975-0302-463e-ba3d-63d29928869d)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         e91d6257-5439-4510-a9bb-554fb9495c51)(content(Whitespace\" \
         \"))))(Tile((id \
         cfd438bd-a3aa-45b8-8060-52e8431302ef)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         168ec45e-99c5-49e0-9f5c-e114d37ff2f8)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         0cb0bba7-1bdd-44f5-821f-697803d28391)(content(Whitespace\" \
         \"))))(Tile((id \
         578a1a7e-a132-4025-84a5-11a874ae30db)(label(soil))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         0e129b0c-2c6d-48cc-9edb-998003eaa447)(content(Whitespace\" \
         \"))))(Tile((id \
         429c7ac5-b520-4669-ac8b-fc7232c9414e)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         e8715aea-50c5-4b18-9741-cb744c1921df)(content(Whitespace\" \
         \"))))(Tile((id \
         cad2691a-b8e3-46bd-856a-c13a7779eb54)(label(Rich))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         1c91c596-8a7e-49c2-8d62-ad2a14cfabc8)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         bfee85e9-dba6-4719-8be8-2bf378c90759)(content(Whitespace\" \
         \"))))(Tile((id \
         eba5be85-f175-493c-87e2-dd5ec1ff2568)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         bd03d1cf-4860-4632-a73b-9200727b95f5)(label(crop))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         158f773f-dc22-48d5-8312-f5242c98875e)(content(Whitespace\" \
         \"))))(Tile((id \
         1ad7e59a-dca8-4bf0-bcfe-cbd3af3806d1)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         f796af53-65d1-40e0-9b8d-46832a9d58cd)(content(Whitespace\" \
         \"))))(Tile((id \
         6a466c74-8dcd-46f7-bf71-43ec2add4aec)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         76f6688e-5fd0-44d7-9968-a1a533fe0653)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         166c3eab-57f7-4d67-a9c1-6d4f8e84a2d8)(content(Whitespace\" \
         \"))))(Tile((id \
         7e16c645-f0b3-4d0c-a8e9-4594553aec85)(label(soil))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         cebb306b-3c2b-468d-b6ca-46fd13f179e7)(content(Whitespace\" \
         \"))))(Tile((id \
         311cca94-e00f-4d3d-912a-6412ea0ea0c0)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         1adedda9-f502-412e-a20f-4be48a745833)(content(Whitespace\" \
         \"))))(Tile((id \
         97b776fe-c434-44a9-aa91-0e85108979bc)(label(Loamy))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))))))))))))(Secondary((id \
         97edcff7-fd2b-4480-b8da-28fefe33cec9)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         18e3f2cc-fbd6-4e1c-879b-f539c959f122)(content(Whitespace\"\\n\")))))))))(Tile((id \
         db7f7e84-d6cd-4204-bfb6-7c0c43492532)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         9afd2ff2-0188-42db-b7be-c0ddb6dec613)(content(Whitespace\"\\n\"))))(Secondary((id \
         04ac0b0b-7658-42aa-b406-e59465021bc8)(content(Whitespace\"\\n\"))))(Tile((id \
         9300abf1-97f9-4887-a7f0-411df60868be)(label(hint test end))(mold((out \
         Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         fcbc91c3-e698-4527-92b8-4424510a9af3)(content(Whitespace\" \
         \"))))(Tile((id \
         99502058-0bcc-48c6-9816-c8a4087deab2)(label(\"\\\"clear field removes \
         all crops\\\"\"))(mold((out Exp)(in_())(nibs(((shape Convex)(sort \
         Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         8b4b17cd-c885-4984-a9d6-997b3e492d2d)(content(Whitespace\"\\n\")))))((Secondary((id \
         f6a0338b-7b17-4164-9a3d-bb3cde259f55)(content(Whitespace\"\\n\"))))(Tile((id \
         a6077c45-2546-4f91-adb3-228bb6941c04)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         d973ed3f-82cd-479a-8683-14d57564211a)(content(Whitespace\" \
         \"))))(Tile((id \
         a74febd6-1efd-4b1c-92c5-76f0991fe071)(label(m))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         50e5703f-734c-48e0-b6c9-ded0eaadc46f)(content(Whitespace\" \
         \")))))((Secondary((id \
         559b1697-e112-48a7-bb7b-abb19045c220)(content(Whitespace\" \
         \"))))(Tile((id \
         b708ae37-0379-4a03-a750-2bf53169f945)(label(do))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         99cf4e31-4117-4995-b4e1-e03c031767d8)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         cf5e3605-c25d-4d3d-b3eb-eda7887196b5)(label(init))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         f4ff6450-0315-4924-8135-7492707ec61a)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         66c4b865-9378-4c11-8010-78a814b1e4f0)(content(Whitespace\" \
         \"))))(Tile((id d0f6405f-38ee-474a-b6df-bb45e1f08486)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         036d0c50-94d8-4272-ad3d-1654339569f1)(label(PlantRow))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         4e0b100f-b8ee-47dc-aee0-1fc095537bcd)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         b3d0f821-a3ed-4462-a9cb-44291033b3b1)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         0b19a599-f41e-4bdc-91a3-0f474a72a88b)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         4e2464f6-915a-41dd-9cc9-526d6569d2b5)(content(Whitespace\" \
         \"))))(Tile((id \
         241ef698-f67e-4fbd-9f5a-04b2176db5c9)(label(PlantRow))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         59e6ee1b-1147-4d2e-8e79-7cb7133975ee)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         20218df6-4865-4086-9ecb-508aad7069b7)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         d93f76c6-36d2-4060-90c0-2ebaf07b066b)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         5fb3cb27-9cce-42fc-a3c0-e06afd32c45b)(content(Whitespace\" \
         \"))))(Tile((id \
         92505a78-8cf7-4dbf-81db-dcf842820cbb)(label(ClearField))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))))))))))))(Secondary((id \
         acd4391f-d3c3-45e6-a1ed-835089aef9a7)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         2e4a3f02-a611-46fc-a05e-9d6c1d37ab96)(content(Whitespace\"\\n\"))))(Tile((id \
         ea8fd00c-7bc5-4e05-b1c1-3a8c20a3a61d)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         37cd008e-1f85-4ccb-abb4-204490862a90)(content(Whitespace\" \
         \"))))(Tile((id \
         c8b94aca-54d9-4696-a7c4-ce2dfe076485)(label(emptyField))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         492966d4-3e52-4634-af20-a176a7a593ff)(content(Whitespace\" \
         \")))))((Secondary((id \
         f2e7b4ed-7eb9-43dd-84d9-804b7685c694)(content(Whitespace\" \
         \"))))(Tile((id 07ba0e5e-cf57-4a20-8a3d-5c14ab114ac7)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         b7fb23be-4983-4931-8988-6106b766986d)(content(Whitespace\"\\n\"))))(Tile((id \
         fd2fd8a0-6b36-4896-9464-3374c2f8c3b8)(label([ ]))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         b48179e1-4631-4176-b7fc-72a24d2329bb)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         ccf37523-51db-4f24-9674-206fd3d269bf)(label(crop))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         fd018690-c025-4181-8968-2e6b8e3bd2bc)(content(Whitespace\" \
         \"))))(Tile((id \
         0fe96e0c-26b6-4211-b9d2-82af71be17af)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         b50c9797-4f38-4aab-ba1a-b73d142fb67a)(content(Whitespace\" \
         \"))))(Tile((id \
         84a97286-a4c6-4cab-9274-ad80b783d182)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         bed2f70a-7791-4651-ba9b-7e1d7111a3de)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         40c731be-876d-4bf4-bd69-6ba36ca0581a)(content(Whitespace\" \
         \"))))(Tile((id \
         41b4a966-c2a1-4b52-a905-e05e6493da89)(label(soil))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         6aff0feb-8820-4b4a-be9f-d3aa8741945e)(content(Whitespace\" \
         \"))))(Tile((id \
         7b7bebd1-d0df-4ae7-9c00-824fe168825d)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         5d74ec1d-8a58-43e5-a794-908150e56acb)(content(Whitespace\" \
         \"))))(Tile((id \
         c35da472-d594-4f09-9613-ef952410d925)(label(Loamy))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         10e35750-7405-4efe-97b6-9fa99ad9f2e1)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         f8200472-5153-4806-b29c-2812c574706b)(content(Whitespace\" \
         \"))))(Tile((id \
         777166b0-6e58-4370-b64d-15be2da78907)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         a15faa3d-8853-4ad5-82b6-7fab3d583cd7)(label(crop))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         4b0d4b9e-8f66-429d-9d39-a8cf4ac6d5a5)(content(Whitespace\" \
         \"))))(Tile((id \
         f1b81255-d807-4b45-9c01-15deb136a3b4)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         b0561511-3971-40e6-8ffe-d58fd262e5d0)(content(Whitespace\" \
         \"))))(Tile((id \
         66725d11-804c-432c-8699-c3f1fe5bbb61)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         9413cbba-32d8-43b0-a4bf-80f3b939f40b)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         d4d8e4f8-9e60-45c3-8779-29e44d2cecca)(content(Whitespace\" \
         \"))))(Tile((id \
         46b38e70-6e20-487e-9b95-72517201306e)(label(soil))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         8ec52cd6-1672-4392-89ad-cf45c8104024)(content(Whitespace\" \
         \"))))(Tile((id \
         d34da664-986c-4d78-81f5-1464dec5ba17)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         93f0d679-3093-4d8f-84b9-03483ca8a86f)(content(Whitespace\" \
         \"))))(Tile((id \
         26597ba9-5c7f-4878-b5af-5f4aaf80fd90)(label(Sandy))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         07f478f9-c145-46df-a6c8-3ef4c8c78190)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         1205f2eb-cd70-47d1-a3ef-5b2db4dabf90)(content(Whitespace\" \
         \"))))(Tile((id \
         5c11b401-5140-467d-8842-992170a26e56)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         e03e0230-dfbe-4089-afe5-3004e242f6d4)(label(crop))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         f2e4d751-543c-4fd6-9694-7223a7692ae9)(content(Whitespace\" \
         \"))))(Tile((id \
         1e9328e2-12ff-4ae5-8763-f186fbd60e18)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         cad135eb-ee53-45b9-9367-7bc957df062c)(content(Whitespace\" \
         \"))))(Tile((id \
         c0c0e402-563b-4f01-ace0-6b2e21b55f29)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         a92ae99b-900c-41ef-9aed-c777c1724d61)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         9d500829-32e4-4cf9-a893-02348b8abea8)(content(Whitespace\" \
         \"))))(Tile((id \
         ff43a341-b8b7-4142-b6f2-4f1e7d08ed34)(label(soil))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         fb077192-42bc-4946-b4cd-d8153d448ed5)(content(Whitespace\" \
         \"))))(Tile((id \
         b080172e-ab40-440a-9260-69ce31a60fd6)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         f1ba5adc-aa44-45e1-ad89-b2dc14aff0d7)(content(Whitespace\" \
         \"))))(Tile((id \
         dd4004bd-42ca-4a1a-a49d-a3bbd14ca009)(label(Clay))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))))))))))))(Tile((id \
         03b82e9a-5997-4ad4-8d2a-8c05c7c7069b)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         5d30c9fe-2a1e-4415-bf43-fe7924387682)(content(Whitespace\"\\n\"))))(Tile((id \
         0d7a1c5b-c6d8-4117-b0a2-b8ebc1d2cd75)(label([ ]))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         12bd2842-b9b4-4d7e-a6cf-ac807a7fbe3e)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         98dc99ab-defd-4647-aa1a-a8ebfa14da79)(label(crop))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         372ba455-596f-478a-b26a-2da7308c7b5c)(content(Whitespace\" \
         \"))))(Tile((id \
         8867d899-d698-4f12-a6bc-019cef951317)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         d7e9944c-1f43-424c-bc48-ce107e1812fa)(content(Whitespace\" \
         \"))))(Tile((id \
         a06426d8-83d5-457a-8ee5-9a0854d717a1)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         9c63eb97-406c-46e8-8493-178cde32c407)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         a2adb372-d48c-4d06-8d42-a9691f01b69c)(content(Whitespace\" \
         \"))))(Tile((id \
         63a0e974-ba18-40e6-8c34-918ad94aebcd)(label(soil))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         f3f90983-2e47-4355-a8ae-942bc08f97a1)(content(Whitespace\" \
         \"))))(Tile((id \
         a15bf52e-863b-4f47-a0aa-4e8c2e2d7bad)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         c38c6ff3-28ea-47fa-85fb-2f1dabf9e8ac)(content(Whitespace\" \
         \"))))(Tile((id \
         5faca5c4-00dd-49db-877b-86eaa7a1f108)(label(Rich))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         c15b16e4-e49c-49c2-8f99-f9f7f9548655)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         687dfc1c-ee45-4061-8916-0330c4c5e93b)(content(Whitespace\" \
         \"))))(Tile((id \
         9ae73142-d69c-41cc-9493-8c797c872355)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         a4f0902e-3afd-43c6-bc93-7dec6e24f033)(label(crop))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         43a5cdbe-22fd-436a-b9e9-dcbd92b5f132)(content(Whitespace\" \
         \"))))(Tile((id \
         5e30a156-11ab-400c-a5df-23d63e9454bb)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         f56cb962-b590-4025-8e9c-913446d1a042)(content(Whitespace\" \
         \"))))(Tile((id \
         b49e4494-85ec-4502-b3de-1ca58f486256)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         a81c636c-f81d-4320-8fe1-12cdab843843)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         afec044c-aa53-4d54-973b-e26f695236b3)(content(Whitespace\" \
         \"))))(Tile((id \
         cd794932-5997-467f-9e0e-cd551c1a2bf1)(label(soil))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         cea1051e-0452-4ae5-9325-f9e1de6d1537)(content(Whitespace\" \
         \"))))(Tile((id \
         39739492-930e-4659-a15c-141ea79524a1)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         61723291-0696-44aa-80f1-2cc15c693868)(content(Whitespace\" \
         \"))))(Tile((id \
         6744533a-3b6e-462f-b69c-b9bb2fa018b1)(label(Loamy))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         7d904f81-b2c2-415e-99e8-938acbdfadd2)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         188be2b7-28ad-4173-896b-5309b1f833f0)(content(Whitespace\" \
         \"))))(Tile((id \
         bff80209-9391-41f2-bf10-150c9a523ee2)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         d1940409-66dc-4a9b-99fa-fc919439e184)(label(crop))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         54e229e7-5c74-4480-8e99-38660a4848ac)(content(Whitespace\" \
         \"))))(Tile((id \
         64c848b9-2796-4a6d-a55a-7db3ec3af305)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         0eacf3c4-d7f2-4b10-bcfe-8b2bb7329e44)(content(Whitespace\" \
         \"))))(Tile((id \
         0bb2cd8b-28dc-4f59-8161-3c7f4e589188)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         8a35a1b5-d367-44d3-a62d-49c7d732d4de)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         7df0d64f-dac2-4f68-8b45-660fca23dc09)(content(Whitespace\" \
         \"))))(Tile((id \
         56a96858-f8ff-4f1b-a898-1256257fb461)(label(soil))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         eb804a60-6b25-4681-a376-d7ebc878ff69)(content(Whitespace\" \
         \"))))(Tile((id \
         123c261e-40cd-4bfa-a55a-3d6f719d1cb8)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         1a864e6d-04e4-43db-822a-38c6525d813a)(content(Whitespace\" \
         \"))))(Tile((id \
         b6323108-22bd-4e45-929e-2197b8dde726)(label(Sandy))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))))))))))))(Tile((id \
         92275d72-029c-47d6-b942-e73306a3fa2e)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         ac371919-d15e-489f-b2c3-bf1246c21411)(content(Whitespace\"\\n\"))))(Tile((id \
         183f9b88-f551-4d2e-8f5c-42fe317ba104)(label([ ]))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         ee44b536-ed56-49f2-bffe-fa55dc71bbc8)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         58c8263a-c529-4915-957f-71217075c6a3)(label(crop))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         bd6388d7-6a81-4958-9f65-3a841c4d454e)(content(Whitespace\" \
         \"))))(Tile((id \
         1f45f03e-4e95-4064-a848-eb95cf4e9e2c)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         d77e87ed-0b35-4b8c-a55f-6b726f43225a)(content(Whitespace\" \
         \"))))(Tile((id \
         bb44e4bc-7776-4635-accc-dc04a37bd0a2)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         ff5f54ec-05e2-4a14-acba-8b7dc28cb01c)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         1cb908aa-5b35-4993-951b-445ac6e7db0c)(content(Whitespace\" \
         \"))))(Tile((id \
         32080033-b2dd-4284-9ac9-6c0327614399)(label(soil))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         afb08544-43a8-4155-83b8-6df578a16f12)(content(Whitespace\" \
         \"))))(Tile((id \
         f9063990-b038-4f83-b1fd-755617ca7543)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         b78806b1-c388-435c-bb9c-2bcf7cf022ae)(content(Whitespace\" \
         \"))))(Tile((id \
         03aea3cd-a42e-4f14-932b-2e065a8559df)(label(Clay))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         6fdb835e-4b55-413f-8f95-7d61e70bff48)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         7920a2cc-95b7-48ec-bd9e-f23c29f226ec)(content(Whitespace\" \
         \"))))(Tile((id \
         b1129f27-135e-4af3-98d5-e5f5eb10a7e9)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         95a1aa7e-8e29-4bd2-84cc-a97332cb50df)(label(crop))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         87142014-2d58-4400-a00b-e9643a854a71)(content(Whitespace\" \
         \"))))(Tile((id \
         7beb0481-9088-4473-89f0-97cee36ec6aa)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         1718f6ef-2c91-4502-b1a7-b5a353676aab)(content(Whitespace\" \
         \"))))(Tile((id \
         660a826d-31fc-4554-bf0d-35da3f38a903)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         ba921d45-a53f-4c1f-a0e9-7cc59badfb0a)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         35c1f793-becf-435b-b9ec-ece552655715)(content(Whitespace\" \
         \"))))(Tile((id \
         a8050b99-6c3d-4108-a03b-3bf4bcca6569)(label(soil))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         93946d72-fd9d-44c0-a75f-4ec0e7d0ed33)(content(Whitespace\" \
         \"))))(Tile((id \
         40eae21e-d599-4338-8b0b-20515b9f1235)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         f0859c1b-d13c-428e-8fdb-f4925060e135)(content(Whitespace\" \
         \"))))(Tile((id \
         ab3baccb-06a7-49a2-8679-e4c73b114707)(label(Rich))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         4bb605b6-6a13-467a-9243-56d32e6afd25)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         d0459ffa-dadd-4e02-8ccf-1972da4e6bce)(content(Whitespace\" \
         \"))))(Tile((id \
         a514009a-201a-4b95-8d4a-068029632bd1)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         c4d41ee2-c7b1-418f-b49a-b9969c46f67d)(label(crop))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         118cf2c8-a18f-43ee-9ff4-440553373bf4)(content(Whitespace\" \
         \"))))(Tile((id \
         def1d8f8-409a-41f3-83e2-62b0e9ab5a20)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         97472690-50d4-4887-aee9-ed753a727a9f)(content(Whitespace\" \
         \"))))(Tile((id \
         f88e2a54-a3e8-453c-bee8-fb7d81050121)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         733d4a19-2ab0-428a-8f43-958df679d137)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         1b9e4e85-fc5c-41bd-9af4-6b8e1703a6bb)(content(Whitespace\" \
         \"))))(Tile((id \
         1eb805de-d814-4457-8b19-8777dd6b58cc)(label(soil))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         f90a3d22-fc17-44b4-aee9-c566fe7676ec)(content(Whitespace\" \
         \"))))(Tile((id \
         385dc809-643a-47c2-bd18-c9bbc57480e4)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         1ffa1a68-7088-49a6-bef5-53963a505f69)(content(Whitespace\" \
         \"))))(Tile((id \
         d3259e5b-af0f-4e3c-8c01-dca098a62843)(label(Loamy))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))))))))))))(Secondary((id \
         86b8f96b-9081-4e5f-8279-232e060399bd)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         de5a8c04-937c-4683-8f63-db0b2f4a02d2)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         20f1f921-f3ce-4c38-991f-31ca458f714a)(content(Whitespace\"\\n\"))))(Tile((id \
         f34b4ec2-7fa3-48d7-a76e-73c3838d632b)(label(m))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         4c572e7e-f8d2-448e-b4f8-1d09edc0559f)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         7166d5ea-0bd2-4012-afa3-207432de6ce9)(label(field))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         7bf66928-4e0a-41d2-993a-f5aac725367b)(content(Whitespace\" \
         \"))))(Tile((id \
         735c8c1f-5790-4fe4-9384-0444d6b1b02b)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         8b402d9b-4b7c-4d6c-8e54-ccbae8c79409)(content(Whitespace\" \
         \"))))(Tile((id \
         9017328f-fca0-4547-84ac-b586d7059c03)(label(emptyField))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         968e7a51-7165-413c-a690-36e8bb31453d)(content(Whitespace\"\\n\")))))))))(Tile((id \
         5e3c0786-1884-40f2-ba93-11f61d3e64f6)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         b33837bb-6633-4ac8-a6db-97033ebcd819)(content(Whitespace\"\\n\"))))(Secondary((id \
         c772f043-d602-48a9-bcde-b0234055fe30)(content(Whitespace\"\\n\"))))(Tile((id \
         30f7a558-7b6d-4234-9b75-738db5e0312d)(label(hint test end))(mold((out \
         Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         305a73c5-5bdd-4bf0-bef3-1497b0a50696)(content(Whitespace\" \
         \"))))(Tile((id \
         c68dbf5f-40b1-46ae-a108-ce817af04f68)(label(\"\\\"select seed changes \
         current seed\\\"\"))(mold((out Exp)(in_())(nibs(((shape Convex)(sort \
         Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         7baa22e4-15fd-4a54-b715-027c706f9a16)(content(Whitespace\"\\n\")))))((Secondary((id \
         0ba3bba9-d6fc-4b63-a83f-79deba74733e)(content(Whitespace\"\\n\"))))(Tile((id \
         8a454475-1bc8-44aa-9f34-d2266e00bb95)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         df0a02f5-73e7-4a6c-b815-25f90476b501)(content(Whitespace\" \
         \"))))(Tile((id \
         5b04c4d2-6afb-4c82-878f-911152e303f8)(label(m))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         ee11f681-29a3-469f-a8dd-d22848988023)(content(Whitespace\" \
         \")))))((Secondary((id \
         b60206a5-03f0-4549-8585-a447491a70ac)(content(Whitespace\" \
         \"))))(Tile((id \
         822a0ec3-4a0f-4f97-be9a-1def88bb9356)(label(update))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         f387e66b-0605-4a0f-b6f6-132063c7551a)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         096173a6-aafc-4248-985a-035900cdabdc)(label(init))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         a8f4ec69-1263-48c3-b753-9ace85e38dde)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         20a76bb6-cbac-442c-9947-0f85b0c42cec)(content(Whitespace\" \
         \"))))(Tile((id \
         1e5be1e4-2c5d-482a-90e8-bcc0f405839d)(label(SelectSeed))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         316396c7-9020-4a97-9abf-3a3076be99ac)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         1d321199-5e03-420e-b3ee-871119fba3e6)(label(2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))))))))))))(Secondary((id \
         ce0b95cb-4f0c-473b-8601-4226642947e5)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         9aaf0e71-389b-4cba-ac9e-64fdab3f4b00)(content(Whitespace\"\\n\"))))(Tile((id \
         fc9c74d1-9668-4683-868d-3010ab89e453)(label(m))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         6815c9b1-e6ea-4d1c-aac1-fd1bf3590f8d)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         fcf091a8-c950-4702-98a0-94da1b34d3e8)(label(currentSeed))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         a32171da-33e1-47bb-b50c-86c4368bb8ed)(content(Whitespace\" \
         \"))))(Tile((id \
         c9bc5fd2-2d3d-4d71-86c5-2ea60b43c273)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         e11de9a6-79f3-4b8d-bd2f-e4903a700b46)(content(Whitespace\" \
         \"))))(Tile((id \
         7e274869-9952-46ed-94be-0af742789e20)(label(\"\\\"\\240\\159\\141\\132\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         3ec048fb-722a-4e71-a310-b416468a1bcf)(content(Whitespace\"\\n\")))))))))(Tile((id \
         737dabbd-3666-43c0-bdca-b5dc10a04bb2)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         4b5cb0c1-adf4-4252-9bd3-5406f8e060f5)(content(Whitespace\"\\n\"))))(Secondary((id \
         f58bdfb9-9c27-4d47-95a7-feefdedb6765)(content(Whitespace\"\\n\"))))(Tile((id \
         d1949523-9454-4e62-95b2-e7585967c79f)(label(hint test end))(mold((out \
         Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         e95374cd-b3ae-48c0-82fa-321a4d280882)(content(Whitespace\" \
         \"))))(Tile((id \
         efaf88db-fbd1-42f6-bd3b-e5d8cce19497)(label(\"\\\"plant with \
         different seed\\\"\"))(mold((out Exp)(in_())(nibs(((shape \
         Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         14ab60bf-df4f-49cd-aea6-7d53b52b9748)(content(Whitespace\"\\n\")))))((Secondary((id \
         5c994574-3344-4ff7-bb98-b585ad75553c)(content(Whitespace\"\\n\"))))(Tile((id \
         719b69c6-e299-4125-b4e7-75c9c7a2d28c)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         fb3fcefa-6c43-4b6c-b092-db579d9fae0c)(content(Whitespace\" \
         \"))))(Tile((id \
         b1900274-1cb6-4111-85a8-c6e6932cf459)(label(m))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         2deae4ec-17b9-487f-98b1-aa1477d741d6)(content(Whitespace\" \
         \")))))((Secondary((id \
         e6cfabc0-b23a-49e5-ac3c-708e44abca60)(content(Whitespace\" \
         \"))))(Tile((id \
         d8f2230d-f754-467d-9d5b-8d6233b4d8aa)(label(do))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         28141dd7-067a-421d-870d-f2d6e458d522)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         55f3e77b-5b53-43cc-ad08-7cd7ac470c33)(label(init))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         df6448f9-5cbc-496c-af81-57a2903329aa)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         676df324-67b6-416f-841c-88babeab43f8)(content(Whitespace\" \
         \"))))(Tile((id 6150ccb2-b217-42a8-b44e-9f7083246501)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         8a5e91d2-2d68-45cc-988a-316a62b1359a)(label(SelectSeed))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         916146cf-56d1-439e-90ee-2731a9070d97)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         4769e940-a902-44c5-818d-eaf68ebb7ee2)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         5d837688-c875-481a-bc84-c334f1d5deea)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         86f6eee6-66da-40dd-aee0-a033b0abe8a8)(content(Whitespace\" \
         \"))))(Tile((id \
         13cf5892-ac33-4ad1-a9c9-5293448f4b23)(label(PlantCrop))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         c9a0dd4b-be9f-4f28-a9a8-001565f75af5)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         89905058-b656-4d54-a1eb-c326e10e8305)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         7f76bc13-1000-49c3-a1c7-aba542e8f152)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         5b0ac8c7-d351-4a48-8bf3-239d6e7a2641)(content(Whitespace\" \
         \"))))(Tile((id \
         af88bce1-d369-4290-b3b6-055e30b75adf)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))))))))))))(Secondary((id \
         8c26358d-d72c-4f7c-b3d4-31b648fc66f2)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         f976d1af-ca32-453c-b49f-a79f08762604)(content(Whitespace\"\\n\"))))(Tile((id \
         ec97a413-6177-4b89-9072-86df18900ae9)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         44645d79-2fd8-431d-8cc0-917cbf531504)(content(Whitespace\" \
         \"))))(Tile((id \
         e6b1a3ff-4d41-43c7-aaa0-d52863ea0664)(label(cell))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         a8370d4a-cd5f-40fe-aa9f-7725cdc22d2d)(content(Whitespace\" \
         \")))))((Secondary((id \
         d2640e02-d92c-483d-9e28-f9b7c296e8f8)(content(Whitespace\" \
         \"))))(Tile((id \
         c15b4587-2263-4edb-ba83-f76113439cf7)(label(getCell))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         6df01964-df5e-4ca0-a854-8ac3dda0a61c)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         84ef7723-0686-4cb5-a547-6e580ec3841e)(label(m))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         6aba0995-bc8b-459b-bcf2-160b25c06fb3)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         fbd47cbc-7e44-4ec7-82dd-e67c14ffd04f)(label(field))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         d8bb9263-f913-4c63-9157-32860b196df7)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         4fcc0926-60ed-4f1f-a6b9-66ab7b016cb7)(content(Whitespace\" \
         \"))))(Tile((id \
         05a390d0-ac78-4654-8967-789d51971433)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         dbe875d7-71c4-4b2b-8220-9c354bd41e7a)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         d550bddf-10ad-48ba-9590-5aae368a5c5e)(content(Whitespace\" \
         \"))))(Tile((id \
         2e1c5005-6c57-4eda-ac84-6460694b7b50)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         bc11eb6b-6625-432f-9ae6-c06bc5542546)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         08635245-bad5-43b7-b188-7823e15485ec)(content(Whitespace\"\\n\"))))(Tile((id \
         822325c7-6a92-47e9-a4c3-05fae14e8b24)(label(cell))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         b91aec34-bfe8-4f81-b629-566b1429fc37)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         c8830635-a84b-45b7-80b7-d37134d72723)(label(crop))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         25a5b4ac-c29e-4934-8303-480ecb8607da)(content(Whitespace\" \
         \"))))(Tile((id \
         73eb0684-8fd4-46d7-86d4-9f69a711cf48)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         e0c6314a-5ba5-43fb-8da0-4c1a84aed9a2)(content(Whitespace\" \
         \"))))(Tile((id \
         ba500ba2-005d-48fb-95db-26585e87c847)(label(\"\\\"\\240\\159\\140\\191\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         7d24b873-84cc-4d7c-a5b8-6ca987074639)(content(Whitespace\" \
         \"))))(Tile((id \
         5dad7de0-10c6-4bfa-a956-5f0352ef9d23)(label(&&))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 32))(sort Exp))((shape(Concave \
         32))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         388842b6-af64-4501-a559-29780168e434)(content(Whitespace\" \
         \"))))(Tile((id \
         8a4f9cfd-b824-4de9-ae7a-2b43cbf3254e)(label(cell))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         dc9fbc9c-12d9-4994-ad81-2827dbcaca18)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         27f14422-2ec8-4665-989e-93d1976d2436)(label(soil))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         21e9ec81-02d4-473d-a3de-164a09242c18)(content(Whitespace\" \
         \"))))(Tile((id \
         9e1f826a-9c31-4999-9408-8e6eb09b5b75)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         91d95b45-b3bf-40d2-98db-ee6c38afde32)(content(Whitespace\" \
         \"))))(Tile((id \
         f0ccb0a0-5017-47db-8816-93c25ab6c17c)(label(Sandy))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         78a57e83-bf3c-43b0-bf04-9759b84d9c5d)(content(Whitespace\"\\n\")))))))))(Tile((id \
         8fe45b75-7938-4a4b-906f-9c8b56916bb3)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         9bda3a9f-f3ae-40b8-8774-83d1bd5a327b)(content(Whitespace\"\\n\"))))(Secondary((id \
         fd43fdf7-2ce6-4dec-948e-1385700d90ca)(content(Whitespace\"\\n\"))))(Tile((id \
         ad10feaf-e5e3-4c90-92af-92fbde2b0f02)(label(hint test end))(mold((out \
         Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         c7c89830-d2dc-4a7f-9fff-1ab7b165ab21)(content(Whitespace\" \
         \"))))(Tile((id \
         6657cab7-4eb0-4d97-afa9-7a5872d32b1c)(label(\"\\\"multiple plants \
         preserve their soils\\\"\"))(mold((out Exp)(in_())(nibs(((shape \
         Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         2084dc9c-c270-4578-ad14-ebdc4109aaea)(content(Whitespace\"\\n\")))))((Secondary((id \
         28d388c9-91da-4657-9c50-b0590867a706)(content(Whitespace\"\\n\"))))(Tile((id \
         aa9b792e-d6a5-438f-acc9-57cd7bbca070)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         f78d338c-5c3a-4dca-803f-853bf31065c0)(content(Whitespace\" \
         \"))))(Tile((id \
         a0c3fe2c-08d4-4520-8691-0d449cae1aa1)(label(m))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         c40b56a0-9b88-445f-a9c0-6441baf950cd)(content(Whitespace\" \
         \")))))((Secondary((id \
         b73102aa-24aa-4471-87e5-5dda4bc29190)(content(Whitespace\" \
         \"))))(Tile((id \
         3d7261ef-269d-4607-b471-523b20713dfa)(label(do))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         408cd3ee-51c0-4a99-89a0-9dea960dd120)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         6c77875c-d11f-4ddc-82db-5fdcb73ccf45)(label(init))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         69bfd67d-3a63-45c5-be23-bc7c62d5e35a)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         bb3c772f-c53b-48e5-975c-6f4a8b998d72)(content(Whitespace\" \
         \"))))(Tile((id 51914cba-be20-4f4d-8803-a28f47dd4dcc)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         9a1d26ca-d28d-4b89-bfd5-948beb29c5ef)(content(Whitespace\"\\n\"))))(Tile((id \
         f2786c16-74da-4e9a-a228-58a4b23e77ee)(label(PlantCrop))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         c197dfd0-b702-4ac9-b67a-b09fc4d20f84)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         2d448a63-3148-45d7-b0d6-c34022ed84ca)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         3fd3abd2-00ac-454b-85c7-cdd038a1c6f0)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         e2807569-7c38-43e6-b03e-f77170d2ed43)(content(Whitespace\" \
         \"))))(Tile((id \
         0dc79000-7520-4c69-9b30-2abc3a37c38b)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         9ea90584-1a18-4c02-a47a-385e43dd0d71)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         cba7cc41-b6e5-4e8f-a212-0d2df83f3ba0)(content(Whitespace\"\\n\"))))(Tile((id \
         8435030f-3579-418d-a158-1f429842e90b)(label(PlantCrop))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         387b2321-6349-4ba0-8cae-4dc8b58f6823)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         527b154a-1e8f-4db2-af9d-7c7ccfb78d79)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         75d8c38a-f3f9-42cf-92db-37bfc35e1546)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         14edaa76-b721-4fab-9077-76df83535921)(content(Whitespace\" \
         \"))))(Tile((id \
         d2353933-0cf5-46e5-8408-b1a0964b1fef)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         5c1b4e8f-0d46-4b0b-b7b0-488604fd6c9e)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         e2500c8b-2b4e-43c0-9b64-5eb9511499c2)(content(Whitespace\"\\n\"))))(Tile((id \
         2b28abbc-3a98-4fc2-b59f-9e27e00f6a57)(label(PlantCrop))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         42f66a0f-fe25-4887-a175-6a4d3a2d4273)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         9141592a-5165-4a74-9f5f-389b30c65f9c)(label(2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         94b5bde8-e5b9-47f4-b086-2e5fbdad1893)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         ba6fa22b-188b-4ef2-a53f-304b22e76763)(content(Whitespace\" \
         \"))))(Tile((id \
         20c42031-4839-4c3b-85b1-229091ff52f5)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         c82354c2-bec5-4d19-8927-c909bb4c17ca)(content(Whitespace\"\\n\"))))))))))))))(Secondary((id \
         81479099-aa98-413d-966f-bbda3a9c808f)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         96f41098-8453-489b-84b8-5687327e8ca2)(content(Whitespace\"\\n\"))))(Tile((id \
         f70918ac-03cd-4b11-94eb-99d6e53442c5)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         378e86c6-76ca-4a4a-ae8f-1068a628d7a2)(content(Whitespace\" \
         \"))))(Tile((id \
         bd842926-639f-4fe4-a6b7-b0cf8ecc26f5)(label(c00))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         4d75a2af-41ba-40db-82d0-613ffb108562)(content(Whitespace\" \
         \")))))((Secondary((id \
         037698c6-d45b-4b0c-8e88-c3696f7ae635)(content(Whitespace\" \
         \"))))(Tile((id \
         2fb6839f-ec5e-44ee-8a7d-51be21e37ab4)(label(getCell))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         a844b0fd-a406-4c25-8cd1-92218d9808a1)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         04d2747a-6a4d-4cf0-b4ca-343597ca41d5)(label(m))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         576d314e-e80b-416f-b753-112fa004820d)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         cb978d33-dcbd-4c0c-bfb8-cc276afb2ea1)(label(field))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         e9015bf9-2750-444f-86fa-bb125bec0740)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         661883fa-6ac0-405d-b86f-a4066fffa1cc)(content(Whitespace\" \
         \"))))(Tile((id \
         9a2239e0-4d5f-4897-afcf-80b9f8a8cd16)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         3da9dc63-9b22-4fd9-b035-d3c63f3a319b)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         ed4dbe56-b942-4ff8-81d1-b75bea5ec54f)(content(Whitespace\" \
         \"))))(Tile((id \
         9aee704e-51e3-4f02-abcf-66729a1440d6)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         1f192326-19b6-4cd9-8419-4eb6a70871a9)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         6d6add8d-e703-4eef-ade4-d296bdfda5bb)(content(Whitespace\"\\n\"))))(Tile((id \
         f2bdaaa8-65fe-40e5-ad24-d99fc3cd793f)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         6e49153d-eb18-41cd-9cb5-0c932aca9e4d)(content(Whitespace\" \
         \"))))(Tile((id \
         c50e7129-521d-46d4-996c-11cb63bb53f7)(label(c10))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         94df6966-4697-4709-90b3-85fe7d7ce286)(content(Whitespace\" \
         \")))))((Secondary((id \
         615874f7-fc77-4a26-ba29-f73702ca2c2d)(content(Whitespace\" \
         \"))))(Tile((id \
         3ed90d2d-da59-47a9-a547-9b07f8235d5c)(label(getCell))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         e9fcb7f9-e96e-4e83-98e2-fb22fa8217bb)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         e7d1cfdc-df08-4ed9-b2c4-55255e7467b5)(label(m))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         019962ec-5513-4c8e-af59-4f0ed88db469)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         35f013e4-b7e9-4ea5-89a6-751b9d3182a0)(label(field))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         0095027b-1b69-4ec3-ac71-f6e679a3d812)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         9f90d875-a460-4c10-8047-3fa09923fc57)(content(Whitespace\" \
         \"))))(Tile((id \
         ee7e33ea-b7d4-4491-b651-d11e97ddc4dc)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         f2428775-5776-48c5-8ac8-eef82346ee54)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         a2a4a92d-6732-41b3-881c-b5b5c471711d)(content(Whitespace\" \
         \"))))(Tile((id \
         514a7384-cd77-436c-9f85-d5b482e73616)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         4e41757b-5030-479a-b097-12905fbeb3f2)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         c8f34635-a469-4683-84d8-1d621be74e10)(content(Whitespace\"\\n\"))))(Tile((id \
         cab924d5-4281-4a82-a523-fc87fe9822d9)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         66daf3ce-7a26-471d-9484-b6f6ae12e63f)(content(Whitespace\" \
         \"))))(Tile((id \
         f6e94e17-fbdb-4548-b28f-c04d74774739)(label(c20))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         978949aa-f079-4302-b305-8a400e176846)(content(Whitespace\" \
         \")))))((Secondary((id \
         c0f5d259-e2e3-4a41-890a-b9ca9aab7a6a)(content(Whitespace\" \
         \"))))(Tile((id \
         d9bc2e40-1091-4d96-bd2e-7daa6b0047fd)(label(getCell))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         1164e600-5841-42a0-8ec6-5dd45df070c0)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         c5ca75cb-4fa6-4d6f-afc4-20ffe0dc417b)(label(m))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         261f2daf-3cfc-4332-8768-7d60202c1b22)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         c5c9736b-0e6a-46d3-96c2-03d2d3455f66)(label(field))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         e288f4c6-814f-421c-ada4-5b4d15195b63)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         c2a6a1e7-defc-48e4-be32-a9a21eee7287)(content(Whitespace\" \
         \"))))(Tile((id \
         e6bae362-a40b-41b7-af75-fd90e465e415)(label(2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         7c785c41-95dd-46c2-b42c-5fcf9b17c3ef)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         634d7eb0-7fb9-4ef8-a096-e6152d2bd07e)(content(Whitespace\" \
         \"))))(Tile((id \
         233b8095-c6bb-4dc9-a72c-44cea38f9f1f)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         c4279096-fb44-4d56-88cc-12fc4621bf8f)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         d82f66cc-a835-44fa-8245-70adf5c0db05)(content(Whitespace\"\\n\"))))(Tile((id \
         3695290b-9328-4e30-ae14-8aa3f7b28c68)(label(c00))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         fa72edb9-6ac1-4d3b-873d-df5d496e9538)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         3b0e3fb8-27b7-47de-8946-c31b1e2fb581)(label(soil))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         409bc65a-8437-41c1-b43e-600b222d2e35)(content(Whitespace\" \
         \"))))(Tile((id \
         ae32afb4-c4ff-4d61-bb80-93e5fb1259e6)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         dc35fc7e-2562-4dd3-a41c-12ffcaac06f6)(content(Whitespace\" \
         \"))))(Tile((id \
         a746a592-c40a-4f8e-9b6d-2559334c5142)(label(Loamy))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         2b16a33e-effe-4ae9-ac7a-cdde4c0db81d)(content(Whitespace\" \
         \"))))(Tile((id \
         3513414f-21cc-4c5a-af2b-54023ab21f17)(label(&&))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 32))(sort Exp))((shape(Concave \
         32))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         d7919abb-59bf-4b1b-af42-f43503debeaa)(content(Whitespace\" \
         \"))))(Tile((id \
         03bb8ff6-cab1-489e-a07f-cfa1a5dc2de8)(label(c10))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         f0303af3-4936-4264-bd12-ae1d9c3d6caf)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         0a90db18-8460-4098-bfcc-3e3995f44609)(label(soil))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         af6653a7-8d7e-4c0d-bbef-8afab26a014b)(content(Whitespace\" \
         \"))))(Tile((id \
         95ab4532-f3bc-4c4c-a33d-5af0cd15ef2e)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         da475d1f-05c2-4896-b7ef-aa8c8f9c169c)(content(Whitespace\" \
         \"))))(Tile((id \
         a98a9277-a886-41a2-945a-c5a4cb8a64cf)(label(Rich))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         74c2c05a-17d9-48a7-b5e7-3570ce49747c)(content(Whitespace\" \
         \"))))(Tile((id \
         316ca294-828b-4ecf-aba4-d8750e6ada4f)(label(&&))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 32))(sort Exp))((shape(Concave \
         32))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         6795820f-3d71-4692-8393-45e9a502291b)(content(Whitespace\" \
         \"))))(Tile((id \
         5a6932bb-a960-4198-9ff8-65c53b8ab402)(label(c20))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         8b475930-f7e2-44a9-a28d-23a08479f287)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         2df46e49-a7be-4ded-bcf7-06f950d14daf)(label(soil))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         338da068-1bf0-4fe2-8d67-1147ad3d2d0e)(content(Whitespace\" \
         \"))))(Tile((id \
         d0d22d9d-af18-41f2-abbf-fc88f30893c5)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         5951189b-c5be-45f3-bdc8-f8729345c671)(content(Whitespace\" \
         \"))))(Tile((id \
         2b86052f-0b6f-4064-aaef-1934682cd413)(label(Clay))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         33fd32c0-3d49-4aea-8d42-702804dbd4e9)(content(Whitespace\"\\n\")))))))))(Tile((id \
         178b9e02-d120-49b5-8a2f-37afd09d7218)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         51559e37-edd7-4b96-baf8-d68e069c2f6f)(content(Whitespace\"\\n\"))))(Secondary((id \
         13370c66-13ef-484d-9094-f554c856b29f)(content(Whitespace\"\\n\"))))(Tile((id \
         2ecead69-a2e3-4b3c-8c65-038633f31b8a)(label(hint test end))(mold((out \
         Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         43fc5cfb-cec8-452e-989c-f6c63a0bdf4e)(content(Whitespace\" \
         \"))))(Tile((id \
         09538037-3b6d-4e43-b6e9-f6fe07425ba2)(label(\"\\\"till then plant \
         sequence\\\"\"))(mold((out Exp)(in_())(nibs(((shape Convex)(sort \
         Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         cc10dfd6-b876-4c7f-b62f-1ab40352b54f)(content(Whitespace\"\\n\")))))((Secondary((id \
         862960fc-0ed2-492e-9ab5-5dfe7b86e5c6)(content(Whitespace\"\\n\"))))(Tile((id \
         35be0192-8fd7-4847-8102-34ebe980683b)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         8c12f658-c705-4995-9ec8-dd85fa42d4fc)(content(Whitespace\" \
         \"))))(Tile((id \
         b6db8f58-b3c5-4551-836b-40dc5333c656)(label(m))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         7bc09a94-2119-494b-98ec-e6d9a0fdeac3)(content(Whitespace\" \
         \")))))((Secondary((id \
         7f90dc30-d89d-4d46-b92d-9f249081f094)(content(Whitespace\" \
         \"))))(Tile((id \
         9ce2d108-8a0b-42be-9bea-878414531587)(label(do))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         27f52729-5734-4ae4-8252-7ccb5f3de922)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         08c0b3b8-4a73-4479-8240-845fd706b78e)(label(init))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         687b441b-f7bb-49e0-85d4-0f0c83d4aded)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         4930c66e-0bd1-48e2-b1ff-616c8d3f26a8)(content(Whitespace\" \
         \"))))(Tile((id b8c378ea-d713-49f0-861a-2787c0845182)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         f2ea638b-da2d-44cf-9dd2-b3069141d6b7)(content(Whitespace\"\\n\"))))(Tile((id \
         cb2b7955-2dcd-4f3e-bc46-9d87764cf276)(label(TillSoil))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         611957cd-d1b1-419a-9d17-126050375d1c)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         03f7f644-cd82-4dcd-903f-3b5d760af980)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         8abf4c36-48d6-440c-96e9-779e072d8299)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         de72918a-80f0-4f33-8ce1-c73932c776b7)(content(Whitespace\" \
         \"))))(Tile((id \
         446f03a5-c323-4d3c-9d95-dae8c432915b)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         b6eb0b6b-0dbc-40ed-9e14-ab950671ac61)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         7a1fe74f-0319-47db-a85e-5487fdcbae49)(content(Whitespace\" \
         \"))))(Tile((id \
         f4d68d1b-3c98-458a-8d09-2a8ce0f96942)(label(Clay))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         aeee806a-a5a6-4da4-a81e-f31a2b319b44)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         a5700812-3b8d-42bb-8074-e24327a9373d)(content(Whitespace\"\\n\"))))(Tile((id \
         ca46b78d-8b6f-4fc6-9388-16fcde2656a7)(label(PlantCrop))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         eb2af9af-f4a8-4740-b85c-774083019c34)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         95251548-8e65-4af1-b8ef-19cde191cb63)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         1effc728-9e17-4176-90ea-8970bea3b565)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         961f73b8-c5a3-4748-9e40-b3c3997be8c9)(content(Whitespace\" \
         \"))))(Tile((id \
         a61dd5b7-4a36-466d-8a48-6a0150d0895c)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         a1f9b350-01b5-43f1-a271-93eee9911e57)(content(Whitespace\"\\n\"))))))))))))))(Secondary((id \
         cca84280-87e8-4b5f-8010-6130f0d321ce)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         b62cb24e-da2d-4d67-b932-9b0e91ef14a6)(content(Whitespace\"\\n\"))))(Tile((id \
         089a041d-60d3-4cb6-a548-b3688a218476)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         27afebb8-5714-46fe-b041-b15617dde5a4)(content(Whitespace\" \
         \"))))(Tile((id \
         835acc0e-f1b2-4fe1-9edc-977a23bb9aeb)(label(cell))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         19b2482d-000c-4519-8f2f-54afd5e82e34)(content(Whitespace\" \
         \")))))((Secondary((id \
         1aa17b26-ae08-434a-9644-a3f98f56e7b1)(content(Whitespace\" \
         \"))))(Tile((id \
         bb382a70-04b5-4a84-a213-0df1914ad83c)(label(getCell))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         e24a9046-c3fc-43d5-828d-1c5d3b16218e)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         bf529d6b-2648-4430-b145-230b1b7aa3e5)(label(m))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         93a67bb8-394e-421a-aa21-293908545923)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         3eb49bb6-3206-43f4-9729-75e1e7f288d2)(label(field))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         56de565b-577b-43db-96f9-c668a048289f)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         a2e510c2-00be-415c-9d4b-e385f5327ee0)(content(Whitespace\" \
         \"))))(Tile((id \
         8ceda197-aa6b-4d76-9891-e4197c735870)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         3d7fba7f-f8ec-494f-ae06-87bcb64c63c7)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         69f67a53-be4a-492a-8ee7-0c1fd2f16013)(content(Whitespace\" \
         \"))))(Tile((id \
         5aa5ffac-7145-4983-bb1d-4e2000fd6d0d)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         39acb416-3bcf-4b6c-a3eb-ec61f9f8462c)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         56a53482-68bf-4b4f-9648-d8af7ff33ca0)(content(Whitespace\"\\n\"))))(Tile((id \
         083fdfb1-72db-4486-9053-682a649993ce)(label(cell))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         16728f74-e3a3-470e-adc2-1f3709b605cb)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         a924157a-62c3-41aa-a42d-044a5104ee4a)(label(crop))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         07bcc0e1-84d0-400d-b420-bc863f636781)(content(Whitespace\" \
         \"))))(Tile((id \
         3e93c59a-5bf8-4c48-89de-df764d462bf5)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         515c4e87-6596-4e9c-9235-f8241b285796)(content(Whitespace\" \
         \"))))(Tile((id \
         41a7a9b8-bebe-4f56-be97-345c4b39169b)(label(\"\\\"\\240\\159\\140\\177\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         431550d0-8e49-48ba-a1c5-e2e05d8c2d63)(content(Whitespace\" \
         \"))))(Tile((id \
         6b6ffe91-3cc9-434d-b6ae-ba158a2750ef)(label(&&))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 32))(sort Exp))((shape(Concave \
         32))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         060b916f-dcdd-4342-ba03-e29a10d665fa)(content(Whitespace\" \
         \"))))(Tile((id \
         87f70154-e533-4ae0-ace6-1db094cc9747)(label(cell))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         54c3cc7f-7f78-4a7e-a392-0804358cd263)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         c979b1c3-59e1-48b8-a034-c0742692c3f0)(label(soil))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         e8851a24-d0a8-4af1-963e-55b8def0bcc3)(content(Whitespace\" \
         \"))))(Tile((id \
         3d9808d5-0bc9-49c4-afdc-98c5ba82b66e)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         a97ab139-ecf9-4a0e-9d1a-b60b93622cba)(content(Whitespace\" \
         \"))))(Tile((id \
         0ce2a140-9174-407f-99e7-4ca77083a093)(label(Clay))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         dc7efea9-f66f-4404-b49e-e75ee3935e50)(content(Whitespace\"\\n\")))))))))(Tile((id \
         1e39a2c4-9799-408a-b0b4-912c3b63d0a8)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         6b90c63a-8834-4d6c-8a06-d04ceb6d348d)(content(Whitespace\"\\n\"))))(Secondary((id \
         886851ac-0a13-4f9d-b96c-c2ef6760757a)(content(Whitespace\"\\n\"))))(Tile((id \
         da44dae8-976e-499b-a95a-b31261dfb78c)(label(hint test end))(mold((out \
         Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         80c4f351-9213-4240-a381-5622c2cd497e)(content(Whitespace\" \
         \"))))(Tile((id \
         d61058ab-d9e6-486d-9f1b-1362912faef8)(label(\"\\\"plant row then \
         harvest one\\\"\"))(mold((out Exp)(in_())(nibs(((shape Convex)(sort \
         Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         2903db88-a602-435c-92a5-65f5d532a20c)(content(Whitespace\"\\n\")))))((Secondary((id \
         507ce670-2446-44f8-9486-5d6ae117b2b2)(content(Whitespace\"\\n\"))))(Tile((id \
         d55b535b-5281-4e68-afba-ca7d35802b1b)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         cb60a194-c44b-4213-b2b1-826eef1539f5)(content(Whitespace\" \
         \"))))(Tile((id \
         d9603905-ef4e-4ca7-bca7-aa83b80e45a0)(label(m))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         877ae45d-0016-4310-886b-01096c3b8bda)(content(Whitespace\" \
         \")))))((Secondary((id \
         c70f97c2-a908-420a-a1c4-c763b8956741)(content(Whitespace\" \
         \"))))(Tile((id \
         27cff0b5-611a-4705-bda8-8b7f3d65c01d)(label(do))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         d3605807-f964-486c-8ff9-cae01bb2a2b0)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         c034779d-edef-4e38-a963-75693336d54c)(label(init))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         2140c78b-3cf2-45bf-aea1-0dc43240dc37)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         4966701e-a05d-4af6-a045-4cffb7b68205)(content(Whitespace\" \
         \"))))(Tile((id 97e6ab2d-b941-44eb-831e-d872ad36f727)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         cb0e31f5-e9c0-4c80-8e6e-2f96460770e7)(content(Whitespace\"\\n\"))))(Tile((id \
         00c7d893-c19d-478b-8593-766230e8dcb4)(label(PlantRow))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         de62c777-c951-4362-9a9e-6d78cdb3ddc5)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         4fca4735-9c14-437c-8168-6fa9379b0f5d)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         e58ed625-af3d-4f03-915b-b61dc4b1b96f)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         568d8265-3fde-4dea-b3ef-5b7497cda24f)(content(Whitespace\"\\n\"))))(Tile((id \
         7f8a4540-143b-46c7-bee4-ced84fcd9fcb)(label(HarvestCrop))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         42346168-6733-4267-b19c-6530ecffe3f3)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         e2b53920-bbe2-4536-a054-0e306e8e5171)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         4cc952cd-eaa3-42fe-9ca4-f0ba8e27b448)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         69db6021-e4fc-4426-ab48-ff651e9ea225)(content(Whitespace\" \
         \"))))(Tile((id \
         4edf3f3d-b6ab-4d81-baf5-a3e71d5eec32)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         0cbda72b-3905-44f8-9ab8-aaaef90d5d50)(content(Whitespace\"\\n\"))))))))))))))(Secondary((id \
         d468470f-e0c5-4a1a-a03c-aa8d441c2bac)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         72e168d6-cf61-4d87-abb9-7e43f456cd18)(content(Whitespace\"\\n\"))))(Tile((id \
         5c0c1413-808e-4753-9aea-345040e1f550)(label(m))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         ef3c5764-4533-4a66-95a2-bbf2c856c288)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         3de27008-ffb8-47d1-98d8-8be35b394b5e)(label(field))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         495d364a-7e66-4542-a2b3-856535694452)(content(Whitespace\" \
         \"))))(Tile((id \
         8757d7f7-e9c4-4795-bd08-2e30d46dfb44)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         f76faf43-98e3-4946-8953-23ea8276ee4f)(content(Whitespace\" \
         \"))))(Tile((id db6fa966-9b81-43a5-88e3-c1afa1ab2af4)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         683a1100-0202-4bf5-bbfa-3ef97c7a5ecb)(content(Whitespace\"\\n\"))))(Tile((id \
         f5c09904-b32f-42d5-a699-91fe26255194)(label([ ]))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         bd6bab87-4362-41c5-9462-6e9d62c09ab4)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         838a2b5c-9d64-4ea0-971a-d4421de87aec)(label(crop))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         c3d31618-9e54-44e7-9d74-c23b1f37a51b)(content(Whitespace\" \
         \"))))(Tile((id \
         a002f41e-ef36-4de8-a3a5-c386d4e9ed2a)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         45173d41-5e15-4497-9ba8-36c86550e9e8)(content(Whitespace\" \
         \"))))(Tile((id \
         cc28b560-f1ea-4ae9-a501-c61d0f534b1e)(label(\"\\\"\\240\\159\\140\\177\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         2b9bec4d-571e-49ab-92c0-ba5bba2e3cd2)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         663d91d6-80da-4e1b-881a-7794fadec177)(content(Whitespace\" \
         \"))))(Tile((id \
         7581c929-f875-4216-9d8c-dfc97f37507c)(label(soil))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         6ebe94d9-6827-4512-ac73-95cb2869d14e)(content(Whitespace\" \
         \"))))(Tile((id \
         18489f5c-0fe5-4741-9d74-e1559f967b7e)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         7b43b089-6656-43ac-a010-c5a7e7141c35)(content(Whitespace\" \
         \"))))(Tile((id \
         2223435c-ebb4-4e12-823a-ca422a898f62)(label(Loamy))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         1ed5f5f2-2f2f-4c93-8380-655b72931332)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         b6ff76c3-b6e5-4ed0-9f13-befee78ff865)(content(Whitespace\" \
         \"))))(Tile((id \
         9530c57d-67c5-4b65-b08a-d828f05ae716)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         74dc4ce4-82b1-4c47-a564-e7b6d95de63b)(label(crop))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         bccb4390-4b58-42bb-82c7-1483643ad993)(content(Whitespace\" \
         \"))))(Tile((id \
         ec5e649c-a491-4d0e-9a16-a5e8378e81b4)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         4e0d4b70-fd27-42e2-a011-66cb18ae5438)(content(Whitespace\" \
         \"))))(Tile((id \
         644a67e7-f225-415c-b55e-f1d9c14c5534)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         558b8c3f-6925-4a53-8193-615d6617f179)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         59dfd05f-8516-4adb-882a-2fce94d570c4)(content(Whitespace\" \
         \"))))(Tile((id \
         3956c415-4418-43fd-93e9-2140cd4356b1)(label(soil))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         45eb095b-783a-4a28-acda-8713b7f69ea4)(content(Whitespace\" \
         \"))))(Tile((id \
         7f64970b-c5ea-469a-8e17-21b80e13f7c5)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         b3988efd-295e-4ca8-b099-99be8406708a)(content(Whitespace\" \
         \"))))(Tile((id \
         69b99cb1-da0d-4621-b238-f320abcdea20)(label(Sandy))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         6d848881-4a0c-4b2c-9732-ed0ecc552924)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         85a9548d-d43c-4ca9-8b5c-0c6d595a5d09)(content(Whitespace\" \
         \"))))(Tile((id \
         42c8f662-7cc1-44cb-8c04-1c118db55d4a)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         50718eff-5105-44f5-9e82-d9e3ffcaf6e8)(label(crop))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         35d8e737-d5c8-48ec-b459-da7370039719)(content(Whitespace\" \
         \"))))(Tile((id \
         5ef80b72-a118-455a-80c2-68d4e9712710)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         3e5f2640-e7fa-4d8a-9dce-0945edda1e49)(content(Whitespace\" \
         \"))))(Tile((id \
         f1e92b50-8ef5-4e98-97b3-27f8c15a093f)(label(\"\\\"\\240\\159\\140\\177\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         efff21cd-0fdd-421b-b807-24443e68a27a)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         ca30d727-8844-43ee-81fc-2700c9f9348d)(content(Whitespace\" \
         \"))))(Tile((id \
         3990f71f-2e24-4ef4-a56f-488325bc85db)(label(soil))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         3a4e6cd6-bbaa-4ec0-9b7c-2c301d718a4f)(content(Whitespace\" \
         \"))))(Tile((id \
         0218bdcb-acb7-4599-a043-8a24b05d0099)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         799c29cc-e7dd-4be6-9364-ec77b8b727b1)(content(Whitespace\" \
         \"))))(Tile((id \
         7ed8616b-c87f-470c-8173-6b003a0c1928)(label(Clay))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))))))))))))(Tile((id \
         779b2829-e1b4-48b6-ab51-8ea0129f7c08)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         1c65a9df-fcff-4212-8cd3-0733850a0b70)(content(Whitespace\"\\n\"))))(Tile((id \
         be03400e-4576-4231-b592-dd4d5569cbd1)(label([ ]))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         af7233a4-c7dd-49b6-8121-07d18d097a65)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         beef3eda-5d82-47fb-8e51-9791f6c82b85)(label(crop))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         eb3a029f-3646-4388-b449-7f11994f676e)(content(Whitespace\" \
         \"))))(Tile((id \
         459d6a0e-5df2-4bf6-8735-8852ca7196bd)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         af774a2b-1917-45a7-b103-2a1d5c921203)(content(Whitespace\" \
         \"))))(Tile((id \
         e3ac623f-6c29-4d6f-b9a4-2eeb76e5faa6)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         b9fcc65e-04af-4345-9bd5-1fbec3b345a0)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         e6672645-298d-4454-a489-95b89564895d)(content(Whitespace\" \
         \"))))(Tile((id \
         5ec170b0-289b-415e-b65d-f2535d0afa20)(label(soil))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         02a16d12-90fe-4be4-a7a7-f5a5c7fb25b2)(content(Whitespace\" \
         \"))))(Tile((id \
         893e3241-1dda-4b07-b913-808f97e31c1b)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         940bb309-bc25-450a-b41b-f2cd1ba920c1)(content(Whitespace\" \
         \"))))(Tile((id \
         cdb442eb-7d57-436c-9ef0-834b9ae40855)(label(Rich))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         f74f3af0-8f27-4659-a269-a0bab630cdc7)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         4bcdf96d-028f-4da7-910d-f6a0b8597db8)(content(Whitespace\" \
         \"))))(Tile((id \
         6fa2bee5-b2ac-4085-89db-2d6b9d3a9445)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         99d130fd-4e36-4a3d-8b8f-9abbcb7fed22)(label(crop))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         62c7bce7-f433-41bd-b10a-e4e4d6b3e708)(content(Whitespace\" \
         \"))))(Tile((id \
         d8f95320-cda8-4c52-8c93-a4ac045508fa)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         3837929e-1973-4109-b050-42df1fb1faf9)(content(Whitespace\" \
         \"))))(Tile((id \
         a431fce5-2580-4e80-a058-1ad9f733d38f)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         a408e477-7b4d-4ce0-9b56-858dfc5d9204)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         eaef38d3-1540-4038-bfa3-ca7f0b5b8e19)(content(Whitespace\" \
         \"))))(Tile((id \
         7fb6616b-526e-4d34-991d-2512ddd8ae3b)(label(soil))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         9d1ca492-6767-471d-b3e6-64e7047a0716)(content(Whitespace\" \
         \"))))(Tile((id \
         3e87ee92-bd05-42d4-bf02-dad6dd89c552)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         4416c0ed-79ef-4bc8-acf8-bf52979d6b2b)(content(Whitespace\" \
         \"))))(Tile((id \
         a0cc7a82-1ae1-4613-9446-4a2469f721f0)(label(Loamy))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         b34c9b58-17ca-44f4-b634-de95541c2800)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         4e98fb4a-6c40-420a-973c-04b89c598293)(content(Whitespace\" \
         \"))))(Tile((id \
         8c7e2593-c60f-4bbf-a2a9-22281d4d68f5)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         aa4810df-0d74-4059-b2f8-004cc1f494a0)(label(crop))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         ec576b16-ecba-4946-a77b-7df8fdba7ab0)(content(Whitespace\" \
         \"))))(Tile((id \
         85318293-1412-4b9c-8fb0-55074f93a95d)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         252c389b-ff56-4fb4-9f69-68a6cf234eee)(content(Whitespace\" \
         \"))))(Tile((id \
         7d44ada0-621f-4412-be25-b7caf0fb1b5d)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         cf4cfe9c-35ea-4116-a064-6be2cf5f4515)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         4793357b-eecd-4f94-adb7-41883909f45c)(content(Whitespace\" \
         \"))))(Tile((id \
         a18f4a80-50ae-4df3-911c-ecb0a9fc35df)(label(soil))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         9f37c7a6-4e53-4040-a076-c10dacfcd225)(content(Whitespace\" \
         \"))))(Tile((id \
         b5638cae-7340-4729-a6d3-a3d0e17d7fa4)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         51bc7088-b8b0-40b7-bfc6-e0cdcdab3332)(content(Whitespace\" \
         \"))))(Tile((id \
         400410b8-b429-454a-8315-4d4da2f6fadd)(label(Sandy))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))))))))))))(Tile((id \
         d80b5afc-291a-44e9-8cc8-208f8b6b5395)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         45d5ce23-f3c1-4744-87ab-3d24ec176403)(content(Whitespace\"\\n\"))))(Tile((id \
         6f274d80-1192-4440-aac6-2031e4498676)(label([ ]))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         897cf568-e077-4b18-8c40-2f86017adaa3)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         a4f1e112-321c-42a4-984d-0fd846d8da3b)(label(crop))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         e0a3fb4e-23f2-4a1a-87d1-59ca7c48a1ac)(content(Whitespace\" \
         \"))))(Tile((id \
         6342ab2c-f8bd-470c-8c95-88ec0fbb820c)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         0be3c961-c3bf-4f68-b535-fb09151a8d87)(content(Whitespace\" \
         \"))))(Tile((id \
         f54499b6-9c45-445c-b472-4670645d5f5d)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         dabdb21b-14e9-4fee-9756-a74646d96974)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         94714d09-f8ba-4676-8264-d216a5203d9e)(content(Whitespace\" \
         \"))))(Tile((id \
         594c88d5-cc8c-4cdd-bd60-5a15a87cdfb5)(label(soil))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         3342a5dc-b563-4798-8212-c30bb2e3ed27)(content(Whitespace\" \
         \"))))(Tile((id \
         19690465-56b8-426b-8d7b-8a38a05b43f6)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         e79abb7b-020d-4765-809a-e0fa2b8010cf)(content(Whitespace\" \
         \"))))(Tile((id \
         3743dd7e-6938-4706-9906-6c594c3358db)(label(Clay))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         95c0ded1-547c-4809-84c0-4c65404da501)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         d338b351-5225-46a9-aeef-fe06720fb092)(content(Whitespace\" \
         \"))))(Tile((id \
         5ab19e05-dbd2-4fb5-9455-23a4dc800c72)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         e0ee8f99-1ac9-40ac-8750-748405e17865)(label(crop))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         153d11c4-6772-4f84-be9a-6ee1d69aef64)(content(Whitespace\" \
         \"))))(Tile((id \
         2e650e7c-a370-4daf-9d55-022ffd4e71d6)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         a2fac5d2-7e96-4ce5-8d09-2b4c459dff93)(content(Whitespace\" \
         \"))))(Tile((id \
         e8e4a5db-3994-4a8f-b503-9471c13ab6cf)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         b461bffc-bfbd-4f78-8c4c-ac279a881580)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         94006b5f-0415-418f-a538-349e36f46c01)(content(Whitespace\" \
         \"))))(Tile((id \
         6cd61d0d-914f-4699-9e50-86d21b92c68f)(label(soil))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         49b83326-24c9-478a-a6db-9451322c3a28)(content(Whitespace\" \
         \"))))(Tile((id \
         6c57daa8-ebdd-4c97-8bfb-820db26f5cc3)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         189220bc-c9bb-4b65-bde9-d2f91b5ff7ff)(content(Whitespace\" \
         \"))))(Tile((id \
         3bb0a33b-4438-4b12-ae8f-cd6c25183e08)(label(Rich))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         90b53ce3-2f10-4fd7-87f7-2e93ebe9a885)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         88f99d41-0fa3-4276-a7c8-dcbbeb99a63a)(content(Whitespace\" \
         \"))))(Tile((id \
         af24af9c-5b50-4af4-a2b2-0983f6fc33a8)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         cf4266a8-1ac4-4be4-bb7d-ac8c816163f3)(label(crop))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         9c34104d-307a-4eac-886e-219e80bd73ac)(content(Whitespace\" \
         \"))))(Tile((id \
         70bf0493-ff54-4323-af68-92327145d2fc)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         a978e60c-4f47-4254-bc1d-a9c9381d3a13)(content(Whitespace\" \
         \"))))(Tile((id \
         29d152c7-6230-4329-bea6-e0eae0766b8a)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         c07f4fe5-45d5-4c20-b761-207033ceda77)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         66722255-d289-4976-819f-5b878be99c5c)(content(Whitespace\" \
         \"))))(Tile((id \
         41d5e1f0-6377-42c3-84e4-a2243925c5ef)(label(soil))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         d7550612-40ad-4ca5-b5f6-c605af9353a8)(content(Whitespace\" \
         \"))))(Tile((id \
         10677393-b1c4-42d8-94bb-bc7194962758)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         ca1c0a37-691b-4ba1-9694-14b87134e641)(content(Whitespace\" \
         \"))))(Tile((id \
         6a2630b2-0704-4b22-bd38-df7e123f533f)(label(Loamy))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))))))))))))(Secondary((id \
         cab7bb73-e8a8-4a7f-8b0d-10ed18182522)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         ed28827e-f76b-4072-adc6-026373c96c1a)(content(Whitespace\"\\n\")))))))))(Tile((id \
         6de0a824-b46d-4de6-ad55-6cf99b14caf0)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         0cc0976d-e346-45e5-a4c5-b68cff392c3e)(content(Whitespace\"\\n\"))))(Secondary((id \
         6769f376-9546-4266-9cc8-aa1e59f9a762)(content(Whitespace\"\\n\"))))(Tile((id \
         80f6f910-53b5-4664-a0c0-e9c0e5db5bb2)(label(hint test end))(mold((out \
         Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         f3499584-a809-41bc-9375-0e207444c506)(content(Whitespace\" \
         \"))))(Tile((id \
         1cc06698-9827-4b8f-b053-5233fc99a1f1)(label(\"\\\"complex planting \
         pattern\\\"\"))(mold((out Exp)(in_())(nibs(((shape Convex)(sort \
         Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         13ff38d3-79d1-45db-a490-6c6c325aafc9)(content(Whitespace\"\\n\")))))((Secondary((id \
         a683f515-8faf-4b57-b789-59bbcd5a859c)(content(Whitespace\"\\n\"))))(Tile((id \
         507e616e-a513-42d0-9701-43bdfe6da486)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         a1795704-b541-41fe-906e-e2136ae2674c)(content(Whitespace\" \
         \"))))(Tile((id \
         486b5fbe-0649-4f0d-9e59-3232d17c3e2b)(label(m))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         bf6d0bc2-354f-4936-8627-204e67535829)(content(Whitespace\" \
         \")))))((Secondary((id \
         64f6a574-d54e-4559-8d24-ade1c19f24f5)(content(Whitespace\" \
         \"))))(Tile((id \
         df9395b2-5dec-4429-909b-009cda9a2641)(label(do))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         e33db434-ad41-4c33-b52c-f14ba17597ab)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         67468150-4ac7-49af-8613-2fd28403cf1a)(label(init))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         228e4295-4e9b-404d-9931-6ff1248076be)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         66b3b579-6840-4c30-8047-e5773bcef8fd)(content(Whitespace\" \
         \"))))(Tile((id e26a4998-ceef-47f3-9ae9-82d40a27298f)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         3c8f0e4e-ffa7-4273-a4bd-60fbf1e32462)(content(Whitespace\"\\n\"))))(Tile((id \
         ced99626-3cc9-47b4-a6db-c06320f80769)(label(SelectSeed))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         38198a83-e994-4700-b591-22bec2da9ef2)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         85aad9b3-44b9-4a97-905c-7620830302be)(label(3))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         bbc33151-25cd-4487-8209-aa3e0f01afde)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         55b8e039-31e5-4429-8f11-ae50aceb4c06)(content(Whitespace\"\\n\"))))(Tile((id \
         3350c6b9-359e-4d09-899f-2341da8d9acf)(label(PlantCrop))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         46a5ac6a-c33c-4e0b-a013-5185acd943a8)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         31bbeaa2-dbf1-441c-b5b0-72ae00714ae2)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         56047ace-43c8-4468-9007-69f51e5ba51a)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         547fcea5-c086-451d-bfff-b77c75cda2f9)(content(Whitespace\" \
         \"))))(Tile((id \
         e85ff040-6fcb-4656-a8f1-ee0a3e98638d)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         7e855b49-0561-4dd0-ab24-6a238cecf217)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         c39047ef-81e6-4de4-b9ac-a58fcf287ed7)(content(Whitespace\"\\n\"))))(Tile((id \
         336a643d-f490-4176-9def-2487d337f241)(label(SelectSeed))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         f96fa75b-8b1b-4f8c-9a0c-4b4ccf7c4d21)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         ee00ab4b-c5ce-41f2-a59d-41ce2338dd92)(label(4))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         698641c8-7b6b-4fd1-908f-952bd1f481bf)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         609cbdfc-5623-4674-a8bb-785c0bf9484c)(content(Whitespace\"\\n\"))))(Tile((id \
         47800abe-d9c7-4c00-9010-671b2aef9906)(label(PlantCrop))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         c4319cda-234b-4041-b503-c8e8dd067db0)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         9166afce-7bc6-430b-b636-af3a3b46cbb4)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         56d7f27e-cd76-4491-b7da-cd115e16a947)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         f27a6ab8-b65e-47fb-a30b-ae530f954252)(content(Whitespace\" \
         \"))))(Tile((id \
         ce49554e-1454-4a08-918b-efbd1ed4262d)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         27add5ac-4cfc-4574-98b7-55be5c4175e0)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         8d74f8d1-a8ef-4eb8-9d59-ae532fa75afe)(content(Whitespace\"\\n\"))))(Tile((id \
         35853bc9-a0f9-4db8-a9c4-433a65de6867)(label(SelectSeed))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         ff4cb2b6-a60a-47df-8fc2-c590f5134d60)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         6bb4ec9c-0bb7-40e2-9777-04446beb2222)(label(2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         4084554c-8d72-4f25-ab09-a2559ef475a8)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         09dd1330-5460-47d7-a1a9-a33f189060ac)(content(Whitespace\"\\n\"))))(Tile((id \
         474b2b90-af85-4eab-b559-4ef3886d92d4)(label(PlantCrop))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         28deba63-8fa8-4277-88a5-f6ddfc324c09)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         f39b578f-08e9-428a-8043-8e43ebd3efd1)(label(2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         900daca5-5220-4da8-9dd2-5ba122b2fcf5)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         f647bd02-5a4b-44ae-a534-35ce2f8024a6)(content(Whitespace\" \
         \"))))(Tile((id \
         ef013acd-ac65-438a-8c9f-f7ced09ae961)(label(2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         8f42af1d-de80-4a10-8bb4-b04d3fcd954f)(content(Whitespace\"\\n\"))))))))))))))(Secondary((id \
         f6e15575-f998-4aeb-a1be-8b9d41c515d6)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         32748a91-c718-4ad2-84cc-fd04e3d2f56d)(content(Whitespace\"\\n\"))))(Tile((id \
         98e5140f-1148-44a1-8c8c-6db6b60265e1)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         37a7ee36-b050-4e89-a828-32e86d1dfbfd)(content(Whitespace\" \
         \"))))(Tile((id \
         8e6fbfc8-dae4-4afb-b566-c016803db182)(label(c00))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         865e2877-a48a-42a7-81ef-1ffdd7502e7c)(content(Whitespace\" \
         \")))))((Secondary((id \
         d07e8dd6-07c4-48cc-a22e-b0f9b1a07094)(content(Whitespace\" \
         \"))))(Tile((id \
         2c6f8606-064d-4370-9638-a7142a91b548)(label(getCell))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         d67cb394-ea5a-45a8-b5f5-6c0fa36b625d)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         6a2979ff-4e6b-414e-b239-6d6ddb7a5c02)(label(m))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         a6d9f5c7-609c-4163-a736-406fe191d036)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         efb3a086-794d-495f-a7f1-e354e202a7e5)(label(field))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         b43db18c-439e-479d-8b6d-9d7a426d6a98)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         39295cb0-d549-4089-97a8-23f577704a73)(content(Whitespace\" \
         \"))))(Tile((id \
         0ea0ef57-2c24-47b2-b8e2-b2d7976f45c6)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         9d5e7205-586f-44fc-b5d8-ab14c67f2d34)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         9a6e75d5-e7ac-4af1-9c89-356220ef28aa)(content(Whitespace\" \
         \"))))(Tile((id \
         11421fd5-d2aa-4b20-9343-22339a102312)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         2a847698-f70a-4293-975c-269e3e575c24)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         a5c79d10-5387-4deb-92e1-ccf9d50ddd15)(content(Whitespace\"\\n\"))))(Tile((id \
         3ae2e62c-c066-469f-b4eb-aa99db3b8353)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         4493c5a4-4fd9-4939-8770-a1bbb3fbb092)(content(Whitespace\" \
         \"))))(Tile((id \
         2cf66780-f458-4e5d-8acf-7ad5093d9e27)(label(c11))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         8ba602f5-00f6-4152-a91e-51be962c403d)(content(Whitespace\" \
         \")))))((Secondary((id \
         a8473a28-ee61-4740-9e6a-c180b408d1f2)(content(Whitespace\" \
         \"))))(Tile((id \
         34c44952-bce7-419e-b914-6525ab978648)(label(getCell))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         45439a39-662b-4958-8bb8-91c23cf89933)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         41b9a180-f593-471f-a1a0-5e512bd2210a)(label(m))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         5f8c54ac-35f3-4e2b-8405-d9507c9d804c)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         53d4ca1e-5154-4970-8ac2-f31570996477)(label(field))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         545264b2-8e37-411d-96c8-697a2025037a)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         8a43b0f0-3000-4b6b-9aa2-4dd915be9fd4)(content(Whitespace\" \
         \"))))(Tile((id \
         15a547bb-172b-4e86-b3da-c0b69842f8e6)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         8287225f-f3fd-4c37-9499-c3efd0b1aeec)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         cb192742-6c94-4e0a-aa65-5174cf8a3b9e)(content(Whitespace\" \
         \"))))(Tile((id \
         95fe75b2-f04d-464e-9bbc-08f3125ef16f)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         7a80bca7-33b8-4b90-9cfe-a749f92ae8b6)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         c74ffeca-da8c-4e80-88c1-a1f9318f80af)(content(Whitespace\"\\n\"))))(Tile((id \
         1db7f8b7-0961-494b-85dd-ec3403a7476a)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         baad2f44-8253-496a-88d7-4d43c69a2869)(content(Whitespace\" \
         \"))))(Tile((id \
         ed424449-163d-4b11-8961-a60a39961840)(label(c22))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         6ab47ec5-1e6c-4a5e-aa8c-19e8d88a6d8b)(content(Whitespace\" \
         \")))))((Secondary((id \
         d8bb0514-ebd5-4ebb-a5da-fa4f9c888608)(content(Whitespace\" \
         \"))))(Tile((id \
         2cd9a2a9-7df9-498f-aa08-05792ec575dd)(label(getCell))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         a23adc17-386d-4019-944d-c3609673b0a6)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         c5a7acfd-8c14-4de9-beed-6bf943e139ac)(label(m))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         34f3cadc-5e92-4198-b93e-fdcf6731e479)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         e5c3be72-af9e-4a04-b34c-fd07128912a1)(label(field))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         d157b90c-43e0-466b-a4c0-b3e257668485)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         401aaebd-cf51-4e64-aaaf-a50718c63085)(content(Whitespace\" \
         \"))))(Tile((id \
         30c48881-6b8b-42ee-91f5-25b773263c1c)(label(2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         5d8c7004-1711-4e79-a7b6-dadf7c12d9a7)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         742ffa27-8d59-4f87-85ba-ccfa6a5c370d)(content(Whitespace\" \
         \"))))(Tile((id \
         d29d13bf-7ffa-481f-802d-127648dbdcfd)(label(2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         1998422c-2d89-4366-b7e2-0ecd91ae56bf)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         77650838-8ae9-446e-832d-2ce936248973)(content(Whitespace\"\\n\"))))(Tile((id \
         f606dcfd-1aff-4107-aa54-ffab6bc10c52)(label(c00))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         91329433-6d13-4267-b136-46b0da443f74)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         5a45a34f-16bb-4e23-9306-7042575aba56)(label(crop))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         08605ad5-52ce-4c26-93fa-bfe7660b97c9)(content(Whitespace\" \
         \"))))(Tile((id \
         981d1fa0-7a2b-49e0-bd6a-9bb4c4ca0085)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         fa9524c6-2e8c-4ff2-8eb4-ab611e18a3ea)(content(Whitespace\" \
         \"))))(Tile((id \
         62adfc26-66ec-4ff7-bc5a-1a7c7293ed6a)(label(\"\\\"\\226\\152\\152\\239\\184\\143\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         d3a918b7-bf8b-4b7e-b5db-dc465a4a643c)(content(Whitespace\" \
         \"))))(Tile((id \
         7250d1ef-6f52-4a7e-8492-1cbfb2efe722)(label(&&))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 32))(sort Exp))((shape(Concave \
         32))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         d4ef5dfe-7af6-45bd-80f1-49e31a093a73)(content(Whitespace\" \
         \"))))(Tile((id \
         4d949fda-df52-4289-91cf-158999d543f2)(label(c11))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         c8741822-8edf-4fe6-8656-349a70aad36b)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         6c30b45c-46d9-4ba2-adc7-fa9a66d51baa)(label(crop))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         34fc950c-38a2-442a-aa70-e57f9be14911)(content(Whitespace\" \
         \"))))(Tile((id \
         7ca9dd32-e184-4d87-a419-aa8de3d9ea8e)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         16cc34bc-030d-4200-a5cb-15eb633746a8)(content(Whitespace\" \
         \"))))(Tile((id \
         b61cf03e-d6a3-466d-bc90-bf33533cc0d5)(label(\"\\\"\\240\\159\\140\\184\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         f0bed44f-d2e8-421b-bfd7-5e7a57bef605)(content(Whitespace\" \
         \"))))(Tile((id \
         672dfd41-571e-4c05-8153-2e11dded7570)(label(&&))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 32))(sort Exp))((shape(Concave \
         32))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         a1c09d3f-dace-4b84-9161-b0b134890ab8)(content(Whitespace\" \
         \"))))(Tile((id \
         18093772-c1ec-4731-afb2-6cff641bc453)(label(c22))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         b68b3728-4c23-4bb5-8473-62cdb3602292)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         dd48b1bc-b7d5-46dd-b0bd-3957da6b4354)(label(crop))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         e7a63589-e325-4bfa-a00a-2e55d7707a0a)(content(Whitespace\" \
         \"))))(Tile((id \
         2f514b52-f203-41a1-86ee-1e92a1c07dab)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         bb7bdee9-5af1-4a6d-8d40-ed48091929b6)(content(Whitespace\" \
         \"))))(Tile((id \
         bdbdd7fc-a3f0-4958-89fd-bdbee5f47aea)(label(\"\\\"\\240\\159\\141\\132\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         691c2a30-c558-433a-b027-db26b80b1eba)(content(Whitespace\"\\n\")))))))))(Tile((id \
         d4559c52-69e8-4eef-846c-77ebe5bd875b)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         0b958cfc-aaf3-4a92-8158-e2768e5849a3)(content(Whitespace\"\\n\"))))(Secondary((id \
         473510fa-06f5-41a9-b22b-56a2adfb3247)(content(Whitespace\"\\n\"))))(Tile((id \
         25505850-8c9f-4f01-9232-318ead437fcc)(label(hint test end))(mold((out \
         Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         cd1fafc5-bc64-4af5-bec9-4354d5a45901)(content(Whitespace\" \
         \"))))(Tile((id \
         ee82b3d9-3b77-4a33-939d-8f3a85db1260)(label(\"\\\"till multiple \
         cells\\\"\"))(mold((out Exp)(in_())(nibs(((shape Convex)(sort \
         Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         d5a6d1ba-8182-4099-b60a-15248d322292)(content(Whitespace\"\\n\")))))((Secondary((id \
         d44b86e5-2542-4075-be68-9506d123d3ed)(content(Whitespace\"\\n\"))))(Tile((id \
         7f9a6c81-de17-4eaf-a6cc-4a39775ad408)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         48a47b57-8682-4114-be8c-3246101f42fd)(content(Whitespace\" \
         \"))))(Tile((id \
         e9271be1-7428-428c-a568-f812f8e5b901)(label(m))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         9ea87aa6-36d0-47e6-9c70-d917dd3f781e)(content(Whitespace\" \
         \")))))((Secondary((id \
         969a008f-08a3-4096-873b-e4c8f95a7391)(content(Whitespace\" \
         \"))))(Tile((id \
         5843dfd9-8c11-4d11-8f2a-9f5d6f4a3de9)(label(do))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         019ffc13-22b0-4776-935e-44d8a10f0a20)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         aba7a81e-ec4b-4425-b1e4-fc3b009d8238)(label(init))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         2d018c3f-9a2c-487e-a1c7-dd4e2df48f73)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         9e98dea3-25c3-4df6-83a3-478ff797e81b)(content(Whitespace\" \
         \"))))(Tile((id ccf994d1-5d13-4e72-94f2-3355a4a1c945)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         bdb8b996-ebb3-4b98-b1c0-7690a2fc5e90)(content(Whitespace\"\\n\"))))(Tile((id \
         02b3262d-882b-4030-8119-47fa8f444e4e)(label(TillSoil))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         69874ab3-5093-4c96-b7e0-bf9264a3e00d)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         3d2dfa32-5de7-4526-b654-5f745aca91bd)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         58bb2364-d967-43dc-8251-d910438ea2b9)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         169dad51-79c7-4baf-bc18-4b11926db79e)(content(Whitespace\" \
         \"))))(Tile((id \
         87437050-5326-4d98-8640-469329b29fbe)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         a978b4ca-9cab-4812-9c0a-153f97042fdf)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         b03726d5-809c-4fd9-b545-d4ca6e351f11)(content(Whitespace\" \
         \"))))(Tile((id \
         736a9e12-e82d-440d-a711-c4a73e237f6c)(label(Sandy))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         13d554b0-b833-4554-8e27-85d53d0eb40b)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         e1439ec2-0968-49a1-b2e1-41ba8a73b406)(content(Whitespace\"\\n\"))))(Tile((id \
         36812bab-8351-4493-8aec-fb30266ddcaf)(label(TillSoil))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         ea8a43d1-f345-4dae-ab56-462d532d440d)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         b8ca491f-104b-4ff9-a800-9e8d9fe186d9)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         e2b63cd3-9267-4c42-ac4e-4068d6945647)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         bef157bc-161d-4abf-987e-3d4f84e50fb6)(content(Whitespace\" \
         \"))))(Tile((id \
         c7928049-e226-46b8-b6a9-a3fc71cd749d)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         1b71b4f8-1032-48a7-8e15-04c4cfa94dc4)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         038b6290-cc1c-4992-815b-a0c16409a9b1)(content(Whitespace\" \
         \"))))(Tile((id \
         c1deb12f-f752-4a54-acd3-f6a546f99d42)(label(Sandy))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         49c91e20-e77b-49c9-8d91-063351b91589)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         1a3b0ac9-d3c2-4182-ba16-2b63681d5c4d)(content(Whitespace\"\\n\"))))(Tile((id \
         4576b654-acfc-45a3-8372-f1a932436371)(label(TillSoil))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         a8bd7d16-f707-492e-832f-9f654ea4c090)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         08fd2f63-d26f-4adc-a3ac-63689023d863)(label(2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         183d558e-0722-4565-9fab-dfd3dc95abd6)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         b61ae15e-94a9-4a33-83d8-d8bb4d75478b)(content(Whitespace\" \
         \"))))(Tile((id \
         4c00ac45-da3c-4bc7-a37a-0013d6a4d8d8)(label(2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         8067d93a-9c34-467c-b3ae-1bb6ee3c88ca)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         bc8d6004-a1cd-4d57-8f32-30f5e72f5212)(content(Whitespace\" \
         \"))))(Tile((id \
         783c0a02-99d5-4ea6-b6d9-3901244c6407)(label(Sandy))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         c6123149-43c5-4d7f-91d8-bf413a54df0e)(content(Whitespace\"\\n\"))))))))))))))(Secondary((id \
         533cd1c7-20f7-4871-9216-760220bf6c8c)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         d447094d-aecd-44a5-a8c5-c3fa3c5a3377)(content(Whitespace\"\\n\"))))(Tile((id \
         c01bbe98-7c38-432c-b97a-8aa4e4d8ee91)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         edb000cd-b798-45ee-b9e6-10e536900dff)(content(Whitespace\" \
         \"))))(Tile((id \
         c22513d4-c907-48dc-bd51-3c8f25c5f87a)(label(c00))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         79a892ea-9e6d-45e9-a5bb-f44a7eec0790)(content(Whitespace\" \
         \")))))((Secondary((id \
         6d96af8b-1bc6-415b-bfe6-aeb0e1fbc2d8)(content(Whitespace\" \
         \"))))(Tile((id \
         42ce7cc0-f115-4f0f-8926-90d5d6a4cb92)(label(getCell))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         06c47b92-da2c-4d5b-8c9c-43d30bfac2a4)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         35608b3a-0dc6-4931-be6f-f348c32b5e5b)(label(m))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         8ec5a623-b369-4189-a13f-87802697502c)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         be692a24-11f2-417f-bb68-94c232e5a525)(label(field))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         42b2bd1f-9b62-421e-a1d2-d5695b8cb3ae)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         8db7fffe-2d2e-48b9-8904-959dc4e3bae0)(content(Whitespace\" \
         \"))))(Tile((id \
         106f2c7b-5f70-44cc-a64a-40a8a07d0958)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         165f7898-8104-4090-bb58-c29269f26890)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         54432d2a-6179-4dfa-9958-654937503818)(content(Whitespace\" \
         \"))))(Tile((id \
         adbf88e7-84e2-43c5-8b12-0ea30e39a2e0)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         df2fdb0a-3ebc-43f3-9d5e-48b1fad46d8c)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         acfa3c77-40f7-4d31-9448-aa42d423c81a)(content(Whitespace\"\\n\"))))(Tile((id \
         968a6592-de74-4d62-9083-d5fb5139d11c)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         428efd78-1402-4084-9aec-0126c7852487)(content(Whitespace\" \
         \"))))(Tile((id \
         472356d7-ecf8-46a3-881d-90c2ac062923)(label(c11))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         4173eea7-6a17-4fe7-b68a-3f7863372c49)(content(Whitespace\" \
         \")))))((Secondary((id \
         2f8b49c6-a594-4b97-89b5-b42695517780)(content(Whitespace\" \
         \"))))(Tile((id \
         ffc1ecff-d7b8-42ec-87eb-0bedd4448a72)(label(getCell))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         9c8da787-ee7a-4075-82ac-38c394c223e5)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         f702beb1-47f9-40dc-8c0b-901590231164)(label(m))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         6762881d-fff1-43a6-b7e6-c21a1cc11292)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         d2e289ce-21a6-45fb-a61d-9e83dc659e21)(label(field))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         da9ea91f-ced2-4ba0-99c0-f2886b8b5baf)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         eea3bb8e-872e-494d-8a50-7c2336cd1d8f)(content(Whitespace\" \
         \"))))(Tile((id \
         116167b7-e07e-43c6-8fa5-d66270fc0fba)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         3aa66b0b-40ae-4ec6-a150-c02423b13907)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         ca54c286-68f1-4a32-9f5a-14b086d1b627)(content(Whitespace\" \
         \"))))(Tile((id \
         d0238eed-f41e-47bb-bedf-64dddda5862e)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         8ad417c7-75ca-403e-b746-d02998446270)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         a7a18f85-d340-4552-8191-c423df317e85)(content(Whitespace\"\\n\"))))(Tile((id \
         ebdcd1d4-bc2d-49f9-9930-6ed2026a3c3c)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         e23f2f64-009e-4cff-9508-09530f61a8f1)(content(Whitespace\" \
         \"))))(Tile((id \
         c009e132-8c2e-4d2b-a362-3e5b92aa3d24)(label(c22))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         ea3bd00a-193d-4e7b-a81a-ca23145b2ef2)(content(Whitespace\" \
         \")))))((Secondary((id \
         ba8995da-6ac1-41b7-b35b-349b19bc1440)(content(Whitespace\" \
         \"))))(Tile((id \
         be0c8dea-69d9-44e3-9570-8cad1ab6ee72)(label(getCell))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         663c6bf2-13a5-4786-ac30-d7e1ff7a2dba)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         ede421d6-428a-42bb-ade6-e68cbb0cc94d)(label(m))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         a168265d-2512-439a-a143-ea3799c64f89)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         37c7491a-cad2-47d6-87c4-37fcae1f3964)(label(field))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         706aac70-2c6b-40b8-8389-d081ab856060)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         bcecadc1-60bc-4e8d-9352-674b80813a8c)(content(Whitespace\" \
         \"))))(Tile((id \
         6d465505-8d2e-4376-bcd6-63b517f3087b)(label(2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         4f4a9e18-8382-4209-9be1-6307878a8baa)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         3dcd59ec-53e9-44e5-b3f9-41a5410bf751)(content(Whitespace\" \
         \"))))(Tile((id \
         7b334558-d80e-494f-988a-397b491e7289)(label(2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         a1253048-412f-4ef1-8e02-ebb30bebdd3f)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         7779318a-4d67-432c-a471-dd99a7196d05)(content(Whitespace\"\\n\"))))(Tile((id \
         0adb6555-88ed-4591-b25b-ad6343a24541)(label(c00))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         8136c843-69d4-4ea2-be4b-24cd33b45439)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         9c69115c-2cfa-4875-8a2f-7094c7de0654)(label(soil))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         a3728b13-c9b7-45c4-9649-62f481e64ee5)(content(Whitespace\" \
         \"))))(Tile((id \
         ddae57b7-475c-4096-92dc-3060d1224c6d)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         c46c5970-c9d9-4960-b681-99d7ffc9fc41)(content(Whitespace\" \
         \"))))(Tile((id \
         5eb13450-7dc0-4f67-947a-e8125e6480b8)(label(Sandy))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         07d2d756-f9bd-470e-97fb-63efa4c593d8)(content(Whitespace\" \
         \"))))(Tile((id \
         3f70420d-5d08-4e71-8cbd-fa33048b65c7)(label(&&))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 32))(sort Exp))((shape(Concave \
         32))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         bf7972a9-31ed-45c9-b7c9-4eb62004c45e)(content(Whitespace\" \
         \"))))(Tile((id \
         6c485685-14c6-42a3-b05e-4014e239c346)(label(c11))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         7c7475cc-ed8c-4bba-ae7c-9c29b1c18b3d)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         006006db-17f9-44da-965d-00b33994e508)(label(soil))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         ccb2bbc7-8f9c-44d2-9abd-c0a72e9e9bf8)(content(Whitespace\" \
         \"))))(Tile((id \
         2e7e076d-48d2-4042-a9ae-cef916993704)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         42fd008b-da6f-4518-a185-3c1e38bf14a1)(content(Whitespace\" \
         \"))))(Tile((id \
         d77d2f7f-1710-4817-8570-0d63a2fb2f36)(label(Sandy))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         f1b71b2e-7141-4940-8d73-039e7522d46b)(content(Whitespace\" \
         \"))))(Tile((id \
         0067a1c4-fb55-4145-af33-b38e8a5ef2f6)(label(&&))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 32))(sort Exp))((shape(Concave \
         32))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         b6bd8d45-e11d-4518-a7b8-2f2826a95aeb)(content(Whitespace\" \
         \"))))(Tile((id \
         c495d629-b560-4b81-88dd-6bc1d8fb3450)(label(c22))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         5273722f-0023-4c20-bb5a-ecaf1df8f29c)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         5046c9b8-ada6-4142-a86d-5db0b041c43a)(label(soil))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         6d29e1fb-bbbf-4399-adb7-ef26bfcf7562)(content(Whitespace\" \
         \"))))(Tile((id \
         fc004976-ede3-4ac8-a10e-64cfdcf096ab)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         a9ecebd5-3d51-4bde-96e3-ac94742c10d2)(content(Whitespace\" \
         \"))))(Tile((id \
         afb10556-7752-41d7-8eb9-e6e6f55baf03)(label(Sandy))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         3fe1b20d-3cfd-42e3-88df-0f68c73229a2)(content(Whitespace\"\\n\")))))))))(Tile((id \
         768f78ca-3abf-4d05-9724-984ebbdc7bdf)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         82733402-24d8-49c6-b82d-2e6fe491ce49)(content(Whitespace\"\\n\"))))(Secondary((id \
         3fcf9e1e-28c2-4d0f-bfb9-6bb0e6dc697b)(content(Whitespace\"\\n\"))))(Tile((id \
         162e84d6-cce5-4640-b897-fdb1f8280c13)(label(hint test end))(mold((out \
         Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         1518a5cb-a4fc-43f3-8292-ed5cdc69b2c4)(content(Whitespace\" \
         \"))))(Tile((id \
         cea4af2e-992b-4e4c-87da-0e82894ef46f)(label(\"\\\"plant \
         off-diagonal\\\"\"))(mold((out Exp)(in_())(nibs(((shape Convex)(sort \
         Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         e88158b9-f696-4016-b55b-06f8e75ba1c8)(content(Whitespace\"\\n\")))))((Secondary((id \
         de2c45bb-9ff8-428f-b9a9-d098d0d47755)(content(Whitespace\"\\n\"))))(Tile((id \
         b565f80e-0339-4827-b83e-fc5e286553b6)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         9e397653-29c8-434d-a3a6-47fede0fd642)(content(Whitespace\" \
         \"))))(Tile((id \
         45dab3ad-c13e-439e-bbac-c054d667f98d)(label(m))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         7d225d61-2ce2-40bc-8671-8d661d257493)(content(Whitespace\" \
         \")))))((Secondary((id \
         c4bcdd28-c195-4d6d-9eb2-1c99fe9ffc50)(content(Whitespace\" \
         \"))))(Tile((id \
         5519f45f-6aa6-492a-94e8-a57af9517b94)(label(update))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         2b301f7a-9e5a-40cc-8b9d-f774a065942d)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         ad0d9fa1-112c-4670-85be-4078e34cebed)(label(init))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         c5385516-b2ea-48a8-a898-66095c7e413b)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         99367080-442f-4ace-9286-aaf323141b3b)(content(Whitespace\" \
         \"))))(Tile((id \
         f784c06b-7de1-4e48-ba0e-f830c3458a08)(label(PlantCrop))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         5940f9d0-e495-4bde-96c9-28fe1cec576d)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         63cd41a8-3b19-43d4-b723-3f92f1c933fb)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         3a66f96b-6b2d-4282-8a4c-c40f600fd0dc)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         171dbf9c-1dc8-4faa-b04c-934ad6d008e4)(content(Whitespace\" \
         \"))))(Tile((id \
         c4c128b1-f3de-4bdd-ab37-008818ea07ff)(label(2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))))))))))))(Secondary((id \
         466094ca-7f2b-40b1-841b-9ecc6bbfd9db)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         f759c93a-56e7-4a7c-8e89-67c0a0d4ce1c)(content(Whitespace\"\\n\"))))(Tile((id \
         3aeb1653-7d0a-4345-beab-0e3790f608da)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         374c41d2-7b25-42af-9484-bfb0c84654fc)(content(Whitespace\" \
         \"))))(Tile((id \
         3c0238de-034a-461f-9afe-f09928662c91)(label(cell))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         7a7aa044-5307-4237-810f-1113673cb720)(content(Whitespace\" \
         \")))))((Secondary((id \
         48ce64c7-2f23-456e-9f81-ae8cdd0f3c09)(content(Whitespace\" \
         \"))))(Tile((id \
         5f8b184e-23bf-461b-9485-1c8a1cb94919)(label(getCell))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         640d3de2-f397-4e6d-bd4d-f558fbe8fb3b)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         d14e69e6-796d-4213-a892-69632095aa1b)(label(m))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         e9f756ab-5746-400d-bc54-d58569d58f81)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         b39de5ce-6b34-41fa-a8b0-b9b0044230b1)(label(field))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         2826795b-3312-4cfc-be7c-e9d2b5900549)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         0f32d5bc-8d9b-4f97-bb7b-e3f55b348362)(content(Whitespace\" \
         \"))))(Tile((id \
         50ef59cc-1c72-4088-b85e-c132ec056bb5)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         d0f1317b-35c9-443a-97bd-6d598c30f34b)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         e03d1f55-c074-4552-a664-fe590de782b6)(content(Whitespace\" \
         \"))))(Tile((id \
         a99e41a7-4051-4ec2-90b4-ddc771b1e1f6)(label(2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         586af1b3-9688-4545-bdec-5175e10188e4)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         adab1212-b114-4327-b210-5ce827bd6841)(content(Whitespace\"\\n\"))))(Tile((id \
         201a3f84-fbb7-472d-bf61-bd69893b1512)(label(cell))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         d549aac1-1648-4be1-992f-2c35c8fadb72)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         0ef2b082-cbc8-4c6c-a661-b390a0ba8486)(label(crop))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         954f7896-2288-431b-8c47-d0b1342c1d97)(content(Whitespace\" \
         \"))))(Tile((id \
         936630ed-3a27-4076-95d6-85b75d986cba)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         03caa59c-9df9-45be-ae65-a48c5d00975b)(content(Whitespace\" \
         \"))))(Tile((id \
         3da745e5-334c-480b-9c80-fbb12f5ca1c5)(label(\"\\\"\\240\\159\\140\\177\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         1610c016-2b72-4fd2-9b08-d70cdabed6ad)(content(Whitespace\" \
         \"))))(Tile((id \
         b63f20e5-4bcf-4bfb-b21c-3fe367e8790a)(label(&&))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 32))(sort Exp))((shape(Concave \
         32))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         ff8e2f3b-c04c-40c6-b189-c881537f3715)(content(Whitespace\" \
         \"))))(Tile((id \
         2e602613-af73-4278-9840-a2f9dea3476e)(label(cell))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         57c8d16e-276f-405b-a8ad-4004d2916fd2)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         3d0b6f53-ea27-4125-a53c-ac324e761bae)(label(soil))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         0497c240-bbee-41da-af6c-03457a8478bc)(content(Whitespace\" \
         \"))))(Tile((id \
         5044f9f8-e580-40a1-958f-ac67c85a58b0)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         bc1d1c2e-a1c4-424f-abaa-3b5baf116535)(content(Whitespace\" \
         \"))))(Tile((id \
         7fadceff-ef45-4486-8dc6-acc663118e62)(label(Clay))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         7a2fff0f-c1b2-49ac-8cba-cc407a1a5464)(content(Whitespace\"\\n\")))))))))(Tile((id \
         b8940634-c1ee-4723-ad37-1e19d7849fb9)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         0c437645-e301-48de-98f5-fd1945a54c41)(content(Whitespace\"\\n\"))))(Secondary((id \
         4f4a8b16-9fb7-4582-90d5-1d91ee939d93)(content(Whitespace\"\\n\"))))(Secondary((id \
         da42be15-5390-48e0-9dd7-adcd9e1e8938)(content(Comment\"# Demo: Create \
         a small farm #\"))))(Secondary((id \
         90185983-eead-4714-b0aa-d21a8acb37c7)(content(Whitespace\"\\n\"))))(Tile((id \
         fa45dd63-1b76-4785-8441-d62eb292531c)(label(do))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         edf0e237-4a00-4c3c-afe5-229e781940aa)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         f6d8d82a-89b2-46e4-a379-dd0cc3e92507)(label(init))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         08c0aa39-b710-4464-8a38-567ffea6951a)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         c39192fb-2819-4cc2-915b-9753f43ac624)(content(Whitespace\" \
         \"))))(Tile((id 91416dd8-f312-4f40-a189-f80bfc31f31c)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         d077a427-639f-478b-bd3b-9ace7d5a3289)(content(Whitespace\"\\n\"))))(Tile((id \
         01c18a69-57aa-4bf8-bf00-f79221eabc3c)(label(SelectSeed))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         1c4a79e7-9253-4185-8ef0-a0e00026ff2f)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         763e80f8-e49e-47ad-9df4-e5622eb6b27b)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         4437e6a4-bfc9-404a-8aa7-b4e135f4e8b1)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         8592621b-8583-4302-9b9b-47b33b7e08a4)(content(Whitespace\"\\n\"))))(Tile((id \
         95be0c91-7579-40c8-9b23-bd0f3da79ac6)(label(PlantCrop))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         9fb7fe80-decb-4cf5-a5b7-ffb18a09de3e)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         81ab9803-5f98-4608-93a1-f73005babe33)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         0a30817c-0fc7-4a76-acd4-a3415ed8db81)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         81dcf21c-6720-4d20-9088-17251e1453c6)(content(Whitespace\" \
         \"))))(Tile((id \
         f821cbcb-7b2d-42cf-909d-f9868d42df27)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         123ee40a-5a6a-4284-84c4-135b8ba8692c)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         08075013-6839-4199-937c-5094b533fa5d)(content(Whitespace\"\\n\"))))(Tile((id \
         c5a23ac5-dc27-4691-9ec8-f34a899a5cec)(label(PlantCrop))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         72e1ec09-1af1-4ddf-9914-22dda87f82d6)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         275052e3-75bd-401a-a063-13b5ceadead4)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         2f5145f7-ce5a-4b48-9f7b-195c024346e4)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         3434dff4-e4bd-4072-a16a-e130a8865af2)(content(Whitespace\" \
         \"))))(Tile((id \
         52584375-60f8-46f4-bd86-e892b236a18c)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         27a3ff0e-fcca-4f6d-823f-eb21da87633f)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         72bf21e1-e3ac-48a3-b307-5f989fa4db87)(content(Whitespace\"\\n\"))))(Tile((id \
         00fb0c21-0c5c-4db0-8571-27077a0c83e8)(label(SelectSeed))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         9e9218d4-d268-4b85-96b5-e05b3c25dcf1)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         b080e5ea-38ee-46c7-977b-da6e0b846220)(label(3))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         20a6096f-d517-42a3-a92a-cf1bc4faa081)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         39427171-3649-421d-97f4-66ca18458520)(content(Whitespace\"\\n\"))))(Tile((id \
         73669115-45b0-4e2b-8266-ff00049e260f)(label(PlantCrop))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         45134405-1079-4e56-9985-c20f13dc6efe)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         52fd64cb-0e95-4a00-9e2b-7445b4dad702)(label(2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         03185c6b-af86-43e7-8ab0-f3e8063b9221)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         d9b16f1f-2d45-41a0-9656-d33a49d7568d)(content(Whitespace\" \
         \"))))(Tile((id \
         09ee7d10-c31f-4459-a372-7687d48688dd)(label(2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         a9bc1a64-9dda-4e35-8807-8747a449e2fa)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         db8a7faf-7708-4c71-ae84-a4b477f65fd8)(content(Whitespace\"\\n\"))))(Tile((id \
         171a529f-bd55-424e-9cb8-7ae5e99ddfb1)(label(TillSoil))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         12370612-5688-4b9c-82ca-bca354bb2e5e)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         95b4d46b-d70a-486d-93d2-6377946683bf)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         53121da0-e978-4c48-ac06-6c1726ffed38)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         79a7885d-f026-439d-b3b8-8a91fdd7f140)(content(Whitespace\" \
         \"))))(Tile((id \
         74c9e540-3c98-4499-b696-bb9183b8ef54)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         6fed2a7d-7055-4c10-bbc4-42c6ff79fb2e)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         1530ee43-68d0-42bc-b040-e7fc3932531f)(content(Whitespace\" \
         \"))))(Tile((id \
         28758265-47ab-455f-98f3-7fef549620ec)(label(Rich))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         5c3afe8c-e196-4646-95db-62394578670c)(content(Whitespace\"\\n\"))))))))))))))(Secondary((id \
         0a040559-582d-489d-b94a-bb516757c14b)(content(Whitespace\"\\n\")))))";
      backup_text =
        "# Crop Plotter with Soil Types #\n\
         # Each cell has a crop and a soil type #\n\n\
         type Crop = String in  # Plant emoji or \"\" for empty #\n\
         type Row = Int in\n\
         type Col = Int in\n\n\
         # Soil types affect what grows best in each cell #\n\
         type SoilType =\n\
         + Loamy    # Rich, balanced soil #\n\
         + Sandy    # Light, drains quickly #\n\
         + Clay     # Dense, holds water #\n\
         + Rich     # Nutrient-dense, dark #\n\
         in\n\n\
         # Each cell in the field has two properties #\n\
         type Cell = (\n\
         crop = Crop,      # What's planted here #\n\
         soil = SoilType   # What kind of soil #\n\
         ) in\n\n\
         type Field = [[Cell]] in\n\n\
         type Model = (\n\
         field = Field,\n\
         currentSeed = Crop,\n\
         seedInventory = [Crop]\n\
         ) in\n\n\
         type Action =\n\
         + PlantCrop(Row, Col)            # Plant current seed at position #\n\
         + HarvestCrop(Row, Col)          # Remove crop at position #\n\
         + TillSoil(Row, Col, SoilType)   # Change soil type at position #\n\
         + PlantRow(Row)                  # Plant entire row with current seed #\n\
         + ClearField                     # Remove all crops #\n\
         + SelectSeed(Int)                # Select seed from inventory #\n\
         in\n\n\
         # Initial field with varied soil types #\n\
         let init: Model = (\n\
         field = [\n\
         [(crop = \"\", soil = Loamy), (crop = \"\", soil = Sandy), (crop = \
         \"\", soil = Clay)],\n\
         [(crop = \"\", soil = Rich),  (crop = \"\", soil = Loamy), (crop = \
         \"\", soil = Sandy)],\n\
         [(crop = \"\", soil = Clay),  (crop = \"\", soil = Rich),  (crop = \
         \"\", soil = Loamy)]\n\
         ],\n\
         currentSeed = \"\240\159\140\177\",\n\
         seedInventory = [\"\240\159\140\177\", \"\240\159\140\191\", \
         \"\240\159\141\132\", \"\226\152\152\239\184\143\", \
         \"\240\159\140\184\"]\n\
         ) in\n\n\
         # Set a specific cell in the field #\n\
         # Uses nested mapi: outer loop for rows (i), inner loop for columns \
         (j) #\n\
         let setCell: (Field, Row, Col, Cell) -> Field =\n\
         fun field, row, col, newCell ->\n\
         mapi(field, fun (i, r) ->\n\
         if i == row\n\
         then mapi(r, fun (j, cell) ->\n\
         if j == col\n\
         then newCell\n\
         else cell)\n\
         else r)\n\
         in\n\n\
         # Plant crop at position, preserving the existing soil type #\n\
         # This uses nested mapi to find the right cell #\n\
         # Outer mapi iterates rows with index i #\n\
         # Inner mapi iterates columns with index j #\n\
         let plantCrop: (Field, Row, Col, Crop) -> Field =\n\
         fun field, row, col, seed ->\n\
         mapi(field, fun (i, r) ->\n\
         if i == row\n\
         then mapi(r, fun (j, cell) ->\n\
         if i == col\n\
         then (crop = seed, soil = cell.soil)\n\
         else cell)\n\
         else r)\n\
         in\n\n\
         # Remove crop at position, preserving soil type #\n\
         let harvestCrop: (Field, Row, Col) -> Field =\n\
         fun field, row, col ->\n\
         mapi(field, fun (i, r) ->\n\
         if i == row\n\
         then mapi(r, fun (j, cell) ->\n\
         if j == col\n\
         then (crop = \"\", soil = cell.soil)\n\
         else cell)\n\
         else r)\n\
         in\n\n\
         # Change soil type at position, preserving crop #\n\
         let tillSoil: (Field, Row, Col, SoilType) -> Field =\n\
         fun field, row, col, newSoil ->\n\
         mapi(field, fun (i, r) ->\n\
         if i == row\n\
         then mapi(r, fun (j, cell) ->\n\
         if j == col\n\
         then (crop = cell.crop, soil = newSoil)\n\
         else cell)\n\
         else r)\n\
         in\n\n\
         # Plant entire row with seed #\n\
         let plantRow: (Field, Row, Crop) -> Field =\n\
         fun field, targetRow, seed ->\n\
         mapi(field, fun (i, row) ->\n\
         if i == targetRow\n\
         then map(row, fun cell -> (crop = seed, soil = cell.soil))\n\
         else row)\n\
         in\n\n\
         # Clear all crops from field #\n\
         let clearField: Field -> Field =\n\
         fun field ->\n\
         map(field, fun row ->\n\
         map(row, fun cell -> (crop = \"\", soil = cell.soil)))\n\
         in\n\n\
         # Update the model based on an action #\n\
         let update: (Model, Action) -> Model =\n\
         fun m, action ->\n\
         case action\n\
         | PlantCrop(row, col) =>\n\
         (\n\
         field = plantCrop(m.field, row, col, m.currentSeed),\n\
         currentSeed = m.currentSeed,\n\
         seedInventory = m.seedInventory\n\
         )\n\
         | HarvestCrop(row, col) =>\n\
         (\n\
         field = harvestCrop(m.field, row, col),\n\
         currentSeed = m.currentSeed,\n\
         seedInventory = m.seedInventory\n\
         )\n\
         | TillSoil(row, col, soil) =>\n\
         (\n\
         field = tillSoil(m.field, row, col, soil),\n\
         currentSeed = m.currentSeed,\n\
         seedInventory = m.seedInventory\n\
         )\n\
         | PlantRow(row) =>\n\
         (\n\
         field = plantRow(m.field, row, m.currentSeed),\n\
         currentSeed = m.currentSeed,\n\
         seedInventory = m.seedInventory\n\
         )\n\
         | ClearField =>\n\
         (\n\
         field = clearField(m.field),\n\
         currentSeed = m.currentSeed,\n\
         seedInventory = m.seedInventory\n\
         )\n\
         | SelectSeed(idx) =>\n\
         (\n\
         field = m.field,\n\
         currentSeed = nth(m.seedInventory, idx),\n\
         seedInventory = m.seedInventory\n\
         )\n\
         end\n\
         in\n\n\
         # Run multiple actions in sequence #\n\
         let do: (Model, [Action]) -> Model =\n\
         fun (model: Model, actions: [Action]) ->\n\
         fold_left(actions, update, model)\n\
         in\n\n\
         # Helper to get cell at position #\n\
         let getCell: (Field, Row, Col) -> Cell =\n\
         fun (field, row, col) ->\n\
         nth(nth(field, row), col)\n\
         in\n\n\
         # ===== TESTS ===== #\n\n\
         hint \"plant single crop preserves soil type\"\n\
         test\n\
         let m = update(init, PlantCrop(1, 1)) in\n\
         let cell = getCell(m.field, 1, 1) in\n\
         cell.crop == \"\240\159\140\177\" && cell.soil == Loamy\n\
         end;\n\n\
         hint \"plant at corner preserves soil\"\n\
         test\n\
         let m = update(init, PlantCrop(0, 0)) in\n\
         let cell = getCell(m.field, 0, 0) in\n\
         cell.crop == \"\240\159\140\177\" && cell.soil == Loamy\n\
         end;\n\n\
         hint \"plant at different position\"\n\
         test\n\
         let m = update(init, PlantCrop(2, 2)) in\n\
         let cell = getCell(m.field, 2, 2) in\n\
         cell.crop == \"\240\159\140\177\" && cell.soil == Loamy\n\
         end;\n\n\
         hint \"harvest removes crop but keeps soil\"\n\
         test\n\
         let m = do(init, [PlantCrop(1, 1), HarvestCrop(1, 1)]) in\n\
         let cell = getCell(m.field, 1, 1) in\n\
         cell.crop == \"\" && cell.soil == Loamy\n\
         end;\n\n\
         hint \"till soil changes soil type but keeps crop\"\n\
         test\n\
         let m = do(init, [PlantCrop(0, 0), TillSoil(0, 0, Rich)]) in\n\
         let cell = getCell(m.field, 0, 0) in\n\
         cell.crop == \"\240\159\140\177\" && cell.soil == Rich\n\
         end;\n\n\
         hint \"plant row fills entire row\"\n\
         test\n\
         let m = update(init, PlantRow(1)) in\n\
         m.field == [\n\
         [(crop = \"\", soil = Loamy), (crop = \"\", soil = Sandy), (crop = \
         \"\", soil = Clay)],\n\
         [(crop = \"\240\159\140\177\", soil = Rich), (crop = \
         \"\240\159\140\177\", soil = Loamy), (crop = \"\240\159\140\177\", \
         soil = Sandy)],\n\
         [(crop = \"\", soil = Clay), (crop = \"\", soil = Rich), (crop = \
         \"\", soil = Loamy)]\n\
         ]\n\
         end;\n\n\
         hint \"clear field removes all crops\"\n\
         test\n\
         let m = do(init, [PlantRow(0), PlantRow(1), ClearField]) in\n\
         let emptyField = [\n\
         [(crop = \"\", soil = Loamy), (crop = \"\", soil = Sandy), (crop = \
         \"\", soil = Clay)],\n\
         [(crop = \"\", soil = Rich), (crop = \"\", soil = Loamy), (crop = \
         \"\", soil = Sandy)],\n\
         [(crop = \"\", soil = Clay), (crop = \"\", soil = Rich), (crop = \
         \"\", soil = Loamy)]\n\
         ] in\n\
         m.field == emptyField\n\
         end;\n\n\
         hint \"select seed changes current seed\"\n\
         test\n\
         let m = update(init, SelectSeed(2)) in\n\
         m.currentSeed == \"\240\159\141\132\"\n\
         end;\n\n\
         hint \"plant with different seed\"\n\
         test\n\
         let m = do(init, [SelectSeed(1), PlantCrop(0, 1)]) in\n\
         let cell = getCell(m.field, 0, 1) in\n\
         cell.crop == \"\240\159\140\191\" && cell.soil == Sandy\n\
         end;\n\n\
         hint \"multiple plants preserve their soils\"\n\
         test\n\
         let m = do(init, [\n\
         PlantCrop(0, 0),\n\
         PlantCrop(1, 0),\n\
         PlantCrop(2, 0)\n\
         ]) in\n\
         let c00 = getCell(m.field, 0, 0) in\n\
         let c10 = getCell(m.field, 1, 0) in\n\
         let c20 = getCell(m.field, 2, 0) in\n\
         c00.soil == Loamy && c10.soil == Rich && c20.soil == Clay\n\
         end;\n\n\
         hint \"till then plant sequence\"\n\
         test\n\
         let m = do(init, [\n\
         TillSoil(1, 1, Clay),\n\
         PlantCrop(1, 1)\n\
         ]) in\n\
         let cell = getCell(m.field, 1, 1) in\n\
         cell.crop == \"\240\159\140\177\" && cell.soil == Clay\n\
         end;\n\n\
         hint \"plant row then harvest one\"\n\
         test\n\
         let m = do(init, [\n\
         PlantRow(0),\n\
         HarvestCrop(0, 1)\n\
         ]) in\n\
         m.field == [\n\
         [(crop = \"\240\159\140\177\", soil = Loamy), (crop = \"\", soil = \
         Sandy), (crop = \"\240\159\140\177\", soil = Clay)],\n\
         [(crop = \"\", soil = Rich), (crop = \"\", soil = Loamy), (crop = \
         \"\", soil = Sandy)],\n\
         [(crop = \"\", soil = Clay), (crop = \"\", soil = Rich), (crop = \
         \"\", soil = Loamy)]\n\
         ]\n\
         end;\n\n\
         hint \"complex planting pattern\"\n\
         test\n\
         let m = do(init, [\n\
         SelectSeed(3),\n\
         PlantCrop(0, 0),\n\
         SelectSeed(4),\n\
         PlantCrop(1, 1),\n\
         SelectSeed(2),\n\
         PlantCrop(2, 2)\n\
         ]) in\n\
         let c00 = getCell(m.field, 0, 0) in\n\
         let c11 = getCell(m.field, 1, 1) in\n\
         let c22 = getCell(m.field, 2, 2) in\n\
         c00.crop == \"\226\152\152\239\184\143\" && c11.crop == \
         \"\240\159\140\184\" && c22.crop == \"\240\159\141\132\"\n\
         end;\n\n\
         hint \"till multiple cells\"\n\
         test\n\
         let m = do(init, [\n\
         TillSoil(0, 0, Sandy),\n\
         TillSoil(1, 1, Sandy),\n\
         TillSoil(2, 2, Sandy)\n\
         ]) in\n\
         let c00 = getCell(m.field, 0, 0) in\n\
         let c11 = getCell(m.field, 1, 1) in\n\
         let c22 = getCell(m.field, 2, 2) in\n\
         c00.soil == Sandy && c11.soil == Sandy && c22.soil == Sandy\n\
         end;\n\n\
         hint \"plant off-diagonal\"\n\
         test\n\
         let m = update(init, PlantCrop(0, 2)) in\n\
         let cell = getCell(m.field, 0, 2) in\n\
         cell.crop == \"\240\159\140\177\" && cell.soil == Clay\n\
         end;\n\n\
         # Demo: Create a small farm #\n\
         do(init, [\n\
         SelectSeed(1),\n\
         PlantCrop(0, 0),\n\
         PlantCrop(1, 1),\n\
         SelectSeed(3),\n\
         PlantCrop(2, 2),\n\
         TillSoil(1, 1, Rich)\n\
         ])\n";
      refractors = "()";
    } )
