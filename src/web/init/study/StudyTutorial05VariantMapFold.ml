let out : string * Haz3lcore.PersistentSegment.t =
  ( "Study / tutorial / 05-variant-map-fold",
    {
      segment =
        "((Secondary((id \
         49910e53-2230-4484-9b82-f5e59dfc52df)(content(Comment\"# PART 5 \
         VARIANT: STEP INTO WITH MAP + FOLD #\"))))(Secondary((id \
         9fc2db73-242b-4684-a316-b1ae746e9d47)(content(Whitespace\"\\n\"))))(Secondary((id \
         b24e6bad-1278-4bb3-8e94-ce96c79495f6)(content(Whitespace\"\\n\"))))(Secondary((id \
         b4e38e28-7242-45ef-9388-8beacd2ba3f1)(content(Comment\"# This \
         function has a two-stage pipeline: map transforms \
         #\"))))(Secondary((id \
         b3aeca0f-6324-4a16-ae52-1e6b3667cb61)(content(Whitespace\"\\n\"))))(Secondary((id \
         a2c80e9f-e82d-481d-8673-c29b04ab442e)(content(Comment\"# the data, \
         then fold aggregates it. From outside you see #\"))))(Secondary((id \
         2a7eb013-0971-4c1e-b1ca-48c48c1e3745)(content(Whitespace\"\\n\"))))(Secondary((id \
         91bc2a3d-11f5-49c8-a280-02968604e9a7)(content(Comment\"# one number. \
         Step Into reveals the whole pipeline. #\"))))(Secondary((id \
         ddf71115-6a21-4602-9220-df16f3a95183)(content(Whitespace\"\\n\"))))(Secondary((id \
         cb7bb352-20a3-457c-9d09-9eafe2d3bd65)(content(Whitespace\"\\n\"))))(Secondary((id \
         d3e747bd-1238-419f-a276-feff7ed2906a)(content(Comment\"# \
         ============================================================ \
         #\"))))(Secondary((id \
         aa5a7b60-6547-4d4e-9e76-fec32e65427f)(content(Whitespace\"\\n\"))))(Secondary((id \
         ad3e6406-21b2-4d6d-9174-fe8d4dcc86cd)(content(Whitespace\"\\n\"))))(Tile((id \
         a0806d12-2f97-4c72-aa96-e1caa7e65866)(label(type = in))(mold((out \
         Exp)(in_(TPat Typ))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         53761120-1f27-4ab7-a851-fe221836bb28)(content(Whitespace\" \
         \"))))(Tile((id \
         3c66dfd8-b687-488e-b91c-6283023e4f3e)(label(Plant))(mold((out \
         TPat)(in_())(nibs(((shape Convex)(sort TPat))((shape Convex)(sort \
         TPat))))))(shards(0))(children())))(Secondary((id \
         7e87054d-cef0-4baf-b01f-9c7a718b4082)(content(Whitespace\" \
         \")))))((Secondary((id \
         a84fe7a5-5b1c-4419-b61e-9f9be063612c)(content(Whitespace\" \
         \"))))(Tile((id \
         72cab9c6-f937-4e06-a8fd-5fc9de300b71)(label(\"(\"\")\"))(mold((out \
         Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0 1))(children(((Secondary((id \
         e286b40c-8493-4f93-be5a-142731b4745b)(content(Whitespace\"\\n\"))))(Tile((id \
         ea9ae186-a362-49f1-9ac9-ad0005ab787f)(label(name))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         d270bb9f-5aef-491a-bd1e-fe6e95d98e5f)(content(Whitespace\" \
         \"))))(Tile((id \
         be69a71e-a172-4c74-a26d-a1b58c3ff34f)(label(=))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 39))(sort Typ))((shape(Concave \
         39))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         13de1130-ef92-49f3-9d02-335a2fef6b19)(content(Whitespace\" \
         \"))))(Tile((id \
         75546c1d-06ae-4586-8563-fd06f32c21cc)(label(String))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         94e7c2ae-0308-41d5-b9d1-f9ff12b71bb7)(label(,))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 44))(sort Typ))((shape(Concave \
         44))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         0b589c90-abbb-431b-bfce-8b46f556d6a8)(content(Whitespace\"\\n\"))))(Tile((id \
         106f8013-1b9f-489f-84cd-66e78cf7d9c8)(label(icon))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         07b3b139-e039-49c5-abf6-7a7c534603c3)(content(Whitespace\" \
         \"))))(Tile((id \
         b11b47cb-9801-4cb6-8903-49e992f60142)(label(=))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 39))(sort Typ))((shape(Concave \
         39))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         b96a3496-b88d-4640-8e72-01b9adcc6203)(content(Whitespace\" \
         \"))))(Tile((id \
         7d5a548c-e7aa-488b-934f-315e2b80591a)(label(String))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         b6594208-3c27-4bc5-aeae-0cd54fba8a65)(label(,))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 44))(sort Typ))((shape(Concave \
         44))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         89121776-7cd8-4830-9ef5-d64689a1206a)(content(Whitespace\"\\n\"))))(Tile((id \
         7bc528b4-2a3e-47f2-8b05-fe6aad9edb9a)(label(water))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         a2ac36b3-e87e-4be1-ae27-22419d3baabf)(content(Whitespace\" \
         \"))))(Tile((id \
         cdd2a1c1-a1ca-4f60-b547-d06d22da360b)(label(=))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 39))(sort Typ))((shape(Concave \
         39))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         873e3182-58df-47f8-9622-cb372a2efbc6)(content(Whitespace\" \
         \"))))(Tile((id \
         b55bcf59-821a-442d-a0a2-bdea9913240b)(label(Int))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         194422be-d98d-421d-8997-9b57a3a88646)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         7db825ed-2758-4e6d-b2c7-b000f71eab01)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         93f88de6-4e3e-4fe2-a0f4-dc0e5bf7d7d6)(content(Whitespace\"\\n\"))))(Secondary((id \
         8b3b01c8-d16e-40ee-a835-9823b20a6a63)(content(Whitespace\"\\n\"))))(Tile((id \
         3d913499-c7e0-4d0e-9b8c-3e2c609e3c47)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         04ec040f-2220-41ad-ae3e-c977ee009107)(content(Whitespace\" \
         \"))))(Tile((id \
         4b3327f2-d011-4a1b-8785-208c7ed4d831)(label(fern))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         36ddb0ad-4be0-44a8-b745-a463df507fb8)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 24))(sort Pat))((shape(Concave \
         24))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         52da652a-5939-4053-85cf-276d383a0af3)(content(Whitespace\" \
         \"))))(Tile((id \
         d0b59cf8-e226-46c9-8a68-ab78718bb52d)(label(Plant))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         8c27e24c-88ef-42a3-bf51-637c20a9147f)(content(Whitespace\" \
         \")))))((Secondary((id \
         21d49684-5b28-416a-b43e-0939d8b88ce7)(content(Whitespace\" \
         \"))))(Tile((id \
         83337d89-8854-44fe-9ebd-7f3767d02b31)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         25d70256-fd55-4a3b-9579-5e2d5b19a054)(label(name))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         b81904d5-bc89-412c-81c1-eb122d492fdb)(content(Whitespace\" \
         \"))))(Tile((id \
         42535c1b-6a53-4492-a465-475e36be3dba)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         93af8770-0dc6-47ee-8a92-7e00c5fb4b64)(content(Whitespace\" \
         \"))))(Tile((id \
         24a2a3ba-3688-405e-867d-bdd4bccd2f2b)(label(\"\\\"Fern\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         6cbc0ae4-43d9-44bb-b0d2-bf201e9dcb91)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         f9b84984-2a53-44c3-9020-e54233dfeb8f)(content(Whitespace\" \
         \"))))(Tile((id \
         56014c99-9353-4e99-a253-e16eaf6d2e33)(label(icon))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         c61c66a1-0d7b-4abb-8eae-5202b5659ff0)(content(Whitespace\" \
         \"))))(Tile((id \
         19c6631b-fc53-4cea-a34f-0b74a2e12979)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         ef90ef38-eaac-4b59-91ca-66e7b0ccaadb)(content(Whitespace\" \
         \"))))(Tile((id \
         398d6ee9-0e3d-4f01-ab90-1a0a2be67e86)(label(\"\\\"\\240\\159\\140\\191\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         209796b6-7481-4e9a-9769-e8bcfd1894a6)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         d19307d1-96e1-4783-853e-fa684b5001ba)(content(Whitespace\" \
         \"))))(Tile((id \
         e5c09789-7862-45a6-abb4-11004f61af65)(label(water))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         30929b0d-76cb-4881-9246-871cb0ec83d7)(content(Whitespace\" \
         \"))))(Tile((id \
         0c3b87ea-abb8-4c0d-8bdd-42660913bd86)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         affcea7a-67bb-4e5f-8faf-5cb22aa465b2)(content(Whitespace\" \
         \"))))(Tile((id \
         31f89c32-c433-4920-aaf3-38a51b019e22)(label(250))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         e0a641d8-aa74-45d2-9a8c-ae03081ddc49)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         c896118f-134e-4fa7-90fc-563563cad7e9)(content(Whitespace\"\\n\"))))(Tile((id \
         e3b98991-8e7f-4314-9f14-b45fd07ff6aa)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         478ee8e7-ad9a-4535-b936-6aa39cd5eeb8)(content(Whitespace\" \
         \"))))(Tile((id \
         73ac44f9-aa0d-497d-89c5-a3e286606652)(label(orchid))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         c35e7d5c-c8ba-4012-bd90-f2e7321b527d)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 24))(sort Pat))((shape(Concave \
         24))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         6833c0df-130e-4ab2-8498-b88308ef1d88)(content(Whitespace\" \
         \"))))(Tile((id \
         e23aad70-4cd8-4151-9455-01b0d946f8b2)(label(Plant))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         7fd3eb14-adb5-4dbe-bca3-4a183bcb652e)(content(Whitespace\" \
         \")))))((Secondary((id \
         4d563ab2-d34c-4996-bc46-999fd611f492)(content(Whitespace\" \
         \"))))(Tile((id \
         ffa6f8ff-7512-4e3e-9226-aa966c9fbc03)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         28730536-46c4-4a54-b598-79b2a0897be0)(label(name))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         36e2fe39-c7ec-41a6-942a-feebd3da44e0)(content(Whitespace\" \
         \"))))(Tile((id \
         e365669a-f57d-4700-bdc3-4ed39c27d0ed)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         c50093c3-38e8-4d0c-b754-993f37c7736a)(content(Whitespace\" \
         \"))))(Tile((id \
         2966f2bc-d536-42dc-87fa-9ca7498c39d4)(label(\"\\\"Orchid\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         e6c94287-b9f0-47c4-9880-96a0063af402)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         19233404-f12d-4798-bbd8-065cfed44ac7)(content(Whitespace\" \
         \"))))(Tile((id \
         4e632b36-0b9c-4f5c-b5b2-488897140854)(label(icon))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         3e6023a4-e9e4-4363-adc0-e704f56dd021)(content(Whitespace\" \
         \"))))(Tile((id \
         93f53644-64ce-4e09-910c-200d0b8d49e1)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         813045a5-3647-4bc0-83fc-0bbe5897249b)(content(Whitespace\" \
         \"))))(Tile((id \
         4928c8db-2f47-42e6-9f5b-6ff56b9aace6)(label(\"\\\"\\240\\159\\140\\184\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         110b5dd0-c17d-4b1c-b8a5-55c0372ecc8e)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         defd4a6a-93d0-4db8-82d7-92919b2e0330)(content(Whitespace\" \
         \"))))(Tile((id \
         adb34539-f54a-4203-b97d-b96e991ed4ce)(label(water))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         526946d1-095a-4489-a7c9-72235d142909)(content(Whitespace\" \
         \"))))(Tile((id \
         1b72da4b-e157-498a-93ef-454afca1edf6)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         a854c638-2625-4160-953f-87bc2e636023)(content(Whitespace\" \
         \"))))(Tile((id \
         f6459c49-e608-4cea-ac5d-8faf803afedf)(label(180))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         9e15fb8e-2db7-4426-8399-9f9dc9ae2a9d)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         e6cbff76-7c66-4937-8dce-f5b2766eeb20)(content(Whitespace\"\\n\"))))(Tile((id \
         73569f49-399f-47d4-af1c-0d7af9173636)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         ae10ef75-73cd-4b4b-838e-46156735dc0e)(content(Whitespace\" \
         \"))))(Tile((id \
         e5f85b8e-3f58-483d-aa85-df4538a1f9b0)(label(cactus))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         f0f7fe1b-8394-4143-93b5-1a5321e67c43)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 24))(sort Pat))((shape(Concave \
         24))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         af26de51-fc57-4907-bc00-684b68b31164)(content(Whitespace\" \
         \"))))(Tile((id \
         01c64a82-b8d7-4ebd-986c-9e25a4bf4e69)(label(Plant))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         573c4d86-7864-4e6d-90fe-e2b073194b94)(content(Whitespace\" \
         \")))))((Secondary((id \
         abfc1fb9-d134-4587-8ae3-c687e8f175d0)(content(Whitespace\" \
         \"))))(Tile((id \
         3c22bd86-c825-4087-ac57-5eceaab4a2a2)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         dc10e05c-e1dd-4a1c-bfbd-6e2a203459f9)(label(name))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         ba77f7f1-a711-462c-8d9a-86f4723427ad)(content(Whitespace\" \
         \"))))(Tile((id \
         5a8aea06-78c4-4507-b3e9-c9613be88c86)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         f46bb93a-91e4-4765-ae99-0cd6697bfb34)(content(Whitespace\" \
         \"))))(Tile((id \
         6172627c-c441-47f1-9d0c-0e2b3f5c0b1c)(label(\"\\\"Cactus\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         73b9baa9-67e3-44e4-8676-a831871f759d)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         cedae25c-0aee-4b31-b340-da597676aba4)(content(Whitespace\" \
         \"))))(Tile((id \
         e50d0cc8-6a9e-40f7-b4a9-df4d15b0e7a3)(label(icon))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         068f7644-862d-4ca4-804c-e2168daed1ee)(content(Whitespace\" \
         \"))))(Tile((id \
         4f960635-a95f-4917-b1b0-2cb86fb1e2c9)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         ef5b9ba8-2ba5-42ea-8f0a-31995dc2bd60)(content(Whitespace\" \
         \"))))(Tile((id \
         f78a744c-f109-4a28-94d8-9bd8ed69b854)(label(\"\\\"\\240\\159\\141\\132\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         00bbb7c2-09de-4a13-bd64-76ae9ae303b6)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         29c07db5-cbf2-4ac3-b5c2-6c677cb24cd4)(content(Whitespace\" \
         \"))))(Tile((id \
         f31f594a-6272-4ac5-94e5-653c0cba6bee)(label(water))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         23353557-622b-4c92-a5d5-14154dcefe6c)(content(Whitespace\" \
         \"))))(Tile((id \
         4f6cbf88-d037-4bee-be96-0460a3f21278)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         cd2e02e4-9bb5-4612-ac49-25f3ab46cf0d)(content(Whitespace\" \
         \"))))(Tile((id \
         50d51345-4c2a-478c-815c-ec4035b20201)(label(50))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         1fd7b48f-a24f-4796-8fe7-9d427e15ccbe)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         c82dbc2c-8bde-48a4-9517-bc31cc506267)(content(Whitespace\"\\n\"))))(Tile((id \
         4db52adc-6b6f-4c7a-989e-00f355f8a229)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         a78b4829-e3cf-4957-81a9-fb2528df5a68)(content(Whitespace\" \
         \"))))(Tile((id \
         d086ca2a-a5a1-4d06-b866-3143dde55f1b)(label(lily))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         efd84aae-0e9f-41c4-91a2-e50dcdd970a8)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 24))(sort Pat))((shape(Concave \
         24))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         745f4b48-a2e6-4e19-a4da-1fe7c613434e)(content(Whitespace\" \
         \"))))(Tile((id \
         73f91a46-d340-4810-b3be-6b7cbe3dd8ad)(label(Plant))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         ba6274db-8ee2-45b1-b40c-695d76f6dbaf)(content(Whitespace\" \
         \")))))((Secondary((id \
         d3f7290c-5499-4444-ab63-a5c8be83d99b)(content(Whitespace\" \
         \"))))(Tile((id \
         43ee10ba-6f9c-48c7-b606-40a593465d72)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         dbd9733b-f1b2-4cd1-8b0a-6422c2dcefa5)(label(name))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         8b385c1b-fb8b-47ab-9b4b-8d0c67d1f33e)(content(Whitespace\" \
         \"))))(Tile((id \
         0e15ab9c-fe28-43d3-b628-56df3de08d76)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         d079afc0-7818-419c-a683-6611090ea4a4)(content(Whitespace\" \
         \"))))(Tile((id \
         41d9a665-e81a-49ba-b8a9-bb1cdefd3ccd)(label(\"\\\"Lily\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         61325ac5-cdd4-488c-825b-5e163796f1d9)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         a1643d74-388a-42a1-a2c0-3d704fbb35ad)(content(Whitespace\" \
         \"))))(Tile((id \
         d6d4c402-f370-42aa-82d3-290bca1c399e)(label(icon))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         af30646c-bec5-45fc-b913-f610fc6501a4)(content(Whitespace\" \
         \"))))(Tile((id \
         e82e3885-7f7d-4461-b7c9-dafab3b0cbf5)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         be241483-17c6-47db-b62e-76f31b2d1363)(content(Whitespace\" \
         \"))))(Tile((id \
         67d3df69-cec1-49ef-89fe-4669521a2d8c)(label(\"\\\"\\226\\152\\152\\239\\184\\143\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         41b3158c-a5f6-4d75-8e1f-bae72f802df7)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         e872d503-d0dc-436b-a444-b912dd0952b9)(content(Whitespace\" \
         \"))))(Tile((id \
         ca3a9b86-a4a4-4dbb-853f-f3eb15a9caa1)(label(water))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         495c3bbf-7e86-4129-98ec-7e0aede134eb)(content(Whitespace\" \
         \"))))(Tile((id \
         ec5b6293-7154-4f56-a84d-0e7318567648)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         a595f55f-2add-4324-a7b8-de523eda97cd)(content(Whitespace\" \
         \"))))(Tile((id \
         6d4e5054-fca4-4a73-8724-59467b8d9b44)(label(200))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         40ee8e6e-b869-4b92-81ef-8a778bd67293)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         e5884c03-9e1c-42c0-a901-8741ba31d397)(content(Whitespace\"\\n\"))))(Tile((id \
         a8e85ce7-bee8-4374-8598-deaa01456a41)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         41ac611a-5544-4b14-b603-68f68339dea4)(content(Whitespace\" \
         \"))))(Tile((id \
         6d141f26-b8f5-46d9-a67a-caf2027e7b3f)(label(daisy))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         63ff6e55-08ee-4bac-93ac-58fc441f8b16)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 24))(sort Pat))((shape(Concave \
         24))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         8a48f2a9-613a-41dd-8043-4603f4b78c76)(content(Whitespace\" \
         \"))))(Tile((id \
         b59f8f09-3084-4881-9441-44e117c81c28)(label(Plant))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         4dc1b641-0f0b-4ff0-ad76-f39f65dbf762)(content(Whitespace\" \
         \")))))((Secondary((id \
         da970bea-eb54-46ed-9275-1758b6ebaee9)(content(Whitespace\" \
         \"))))(Tile((id \
         0b3ae107-234b-48ca-b6c0-cb6f3f740738)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         301caeb5-670b-4b9e-896e-b75f096bfad2)(label(name))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         b67112d0-6a34-4529-a130-9b843e62727b)(content(Whitespace\" \
         \"))))(Tile((id \
         68ce512d-4d4c-47a7-a853-4b40df5a8f89)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         bf367564-79e1-43c8-8b41-a14cff71c8a8)(content(Whitespace\" \
         \"))))(Tile((id \
         b238cdaf-bc09-4cae-8e2c-258ed0d320e1)(label(\"\\\"Daisy\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         ed0e045f-c0fd-4f68-985b-eb93fd75ce8d)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         35d64590-5173-4ce6-a892-ef0187611d31)(content(Whitespace\" \
         \"))))(Tile((id \
         6857f19e-0283-41dc-8b80-b55f9fd61b87)(label(icon))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         985b1c54-988e-43ef-9402-24538e9479d9)(content(Whitespace\" \
         \"))))(Tile((id \
         202c0b10-2947-4314-8445-ce861006a995)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         b975fc9d-c007-4cb8-9fb2-c20142d23174)(content(Whitespace\" \
         \"))))(Tile((id \
         24240de0-a430-4e74-9e2f-54232da4de3b)(label(\"\\\"\\240\\159\\140\\177\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         35b3afb0-63b0-4384-a2ac-da33daa5d6aa)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         f654bd2d-c5f1-4708-b0b5-9d80de82fea4)(content(Whitespace\" \
         \"))))(Tile((id \
         b21e9f73-820d-4016-8efa-7748d778d986)(label(water))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         d386656c-35cc-43f7-8aea-e131c836f6c6)(content(Whitespace\" \
         \"))))(Tile((id \
         169e7474-c63f-4dee-b615-485d78aca2e4)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         a0b80695-d8ef-4783-9004-1b2285f10a00)(content(Whitespace\" \
         \"))))(Tile((id \
         210e3617-c3c1-41d6-b4b3-967615aea211)(label(160))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         c8d6d4e8-e16f-4b91-9e0a-ea2c49b9abe9)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         cde85b78-4be4-42ef-829d-3ad257b14640)(content(Whitespace\"\\n\"))))(Secondary((id \
         487fb409-d527-488e-b406-23748587f849)(content(Whitespace\"\\n\"))))(Secondary((id \
         88812677-a6bc-4cc3-9a0d-b8cc2a355d7c)(content(Comment\"# weekly_total \
         computes the total weekly water for a garden. #\"))))(Secondary((id \
         09a46b74-af92-43d1-9227-9f5516843512)(content(Whitespace\"\\n\"))))(Secondary((id \
         991e086f-3e0b-4463-b783-3da83bb50462)(content(Comment\"# First it \
         maps each plant's daily water to weekly (x7), #\"))))(Secondary((id \
         1c567484-2c7f-4bca-90a5-e40170a79b5f)(content(Whitespace\"\\n\"))))(Secondary((id \
         9f208a5c-0163-4358-8d3a-2705a3dff193)(content(Comment\"# then folds \
         to sum everything up. #\"))))(Secondary((id \
         7bbf4a14-5ede-4f81-9786-097e9d5c48ca)(content(Whitespace\"\\n\"))))(Secondary((id \
         edaf1360-30ff-454c-88c1-0249a0ce6d31)(content(Whitespace\"\\n\"))))(Tile((id \
         12dd1b4a-7f96-46f5-97a2-039347b95e9e)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         fa4203f9-1431-4ba9-ac4a-2a40659b9454)(content(Whitespace\" \
         \"))))(Tile((id \
         b6b2e6ac-4fe9-468c-88ff-648d95567f03)(label(weekly_total))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         6951aa4a-eddf-4231-9426-5bafcfe4d634)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 24))(sort Pat))((shape(Concave \
         24))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         8c4c2a5c-72c1-4159-a303-4b31a309d3cf)(content(Whitespace\" \
         \"))))(Tile((id 3d1886c0-fdc7-4cad-a7c0-346e6d1f53c4)(label([ \
         ]))(mold((out Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape \
         Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
         5d618c62-f3e4-43d4-9c5c-6117be8ee8c0)(label(Plant))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children()))))))))(Secondary((id \
         c06a06ff-984b-4927-a131-f06f881cf245)(content(Whitespace\" \
         \"))))(Tile((id \
         ac660224-8e48-4d1b-aea7-48733b5970c2)(label(->))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 13))(sort Typ))((shape(Concave \
         13))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         26ecd7d6-4b94-434d-b922-ee179bddb7de)(content(Whitespace\" \
         \"))))(Tile((id \
         990546b9-4e6e-4bdc-a1ae-a79675318be9)(label(Int))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         bcfc6359-4dde-4860-a7ce-51ae88d62d70)(content(Whitespace\" \
         \")))))((Secondary((id \
         f0345e9a-659d-4f31-a3a3-2898beb53d65)(content(Whitespace\"\\n\"))))(Tile((id \
         c9fb4665-f107-4a78-9751-d0c0b4cbd323)(label(fun ->))(mold((out \
         Exp)(in_(Pat))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         37))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         31e9345a-2400-425c-8104-1eb03b68497b)(content(Whitespace\" \
         \"))))(Tile((id \
         5d71d560-b14f-4d6a-8d90-0b1843eb6bf2)(label(plants))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         cf70f610-b8a0-4b42-be68-ab3523260cf3)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         1b18387f-2544-414b-bfce-8ff2d6240858)(content(Whitespace\"\\n\"))))(Tile((id \
         9bf4492d-fb08-4a96-beaf-ea5bd1f7989e)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         725730d4-b144-4676-ade3-27a96be02736)(content(Whitespace\" \
         \"))))(Tile((id \
         de13a223-e75a-4f94-a84e-602687933992)(label(weekly_amounts))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         92054a62-1470-4aea-ae9a-2a2c7c2cce5b)(content(Whitespace\" \
         \")))))((Secondary((id \
         5ac0897c-1672-422e-9064-b63511adf1d4)(content(Whitespace\" \
         \"))))(Tile((id \
         30baa4da-71e7-48a4-bfa2-e2516b851a9f)(label(map))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         007eee49-8b83-4847-b93a-50eeea143691)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         2c50c2db-15fb-4928-b97d-4b12f12a8dd0)(label(plants))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         a9aef701-089e-470a-98f9-fced4e7d2f65)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         5b58fb70-f2e0-4f19-9860-c87b91955092)(content(Whitespace\" \
         \"))))(Tile((id 6554a06a-eb5b-463a-95e2-1b72a050dded)(label(fun \
         ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 37))(sort Exp))))))(shards(0 \
         1))(children(((Secondary((id \
         35684c6f-aa0f-4cfd-b6d1-49688fb7f76c)(content(Whitespace\" \
         \"))))(Tile((id \
         0f5514c7-cdd0-497f-9ae9-8f4c05d6c819)(label(plant))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         356f43b6-bb0f-4868-bab7-b9d8bfeb1b5d)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         9964a6c2-4459-412a-8c89-6aee2ca8fecb)(content(Whitespace\"\\n\"))))(Tile((id \
         871a9cb7-1d21-4ef4-8c3d-fd014755fa83)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         b18570d9-68a5-4fb6-803d-b5b7b4c102af)(content(Whitespace\" \
         \"))))(Tile((id \
         0f0ccfbe-6192-4508-8902-a80fc88560eb)(label(daily))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         cad7374a-858e-409b-9202-74470fabd0b2)(content(Whitespace\" \
         \")))))((Secondary((id \
         e7ff6ab8-2b95-4db0-b903-a6d9667794e0)(content(Whitespace\" \
         \"))))(Tile((id \
         32349836-b609-4152-8028-eccbaa0cc947)(label(plant))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         5ae076f8-f5bb-4091-832a-e70ced9b162d)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         bbe5122f-17b2-45f2-b048-90f226c5e4cb)(label(water))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         a02cb370-a876-49aa-af4c-d0aaa58f800a)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         36dd2968-b527-411c-b6a9-fa5ed85f7a13)(content(Whitespace\"\\n\"))))(Tile((id \
         4cbf817e-599e-4272-920c-fe941f2839ce)(label(daily))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         f71e5af8-a280-4171-ba19-f8d3b8356c7d)(content(Whitespace\" \
         \"))))(Tile((id \
         f7d6bf54-41f6-45e4-bc13-ed3a5adc7640)(label(*))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 27))(sort Exp))((shape(Concave \
         27))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         a77f4fd4-c6f1-49ca-bbae-d226669b4f24)(content(Whitespace\" \
         \"))))(Tile((id \
         03fb1b1e-1a7c-4b77-8df9-654ec5ffb98b)(label(7))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         1f456704-b57a-4d5e-af40-e4ad187323be)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         18f9f320-2a90-4565-b304-d46b35ae1272)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         361bd702-8429-4777-90e8-d9163143c19d)(content(Whitespace\"\\n\"))))(Tile((id \
         98e85eee-b6e1-439d-bddf-183b1554d891)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         cf2c6f28-6aba-462c-8e50-fd1f2f61e4c8)(content(Whitespace\" \
         \"))))(Tile((id \
         fe31ba67-389b-4169-844f-fa7c955d5638)(label(sum))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         f6b56e46-53e7-4449-9981-99b489936643)(content(Whitespace\" \
         \")))))((Secondary((id \
         70d870b4-3c74-41fb-a971-25ef09088cb8)(content(Whitespace\" \
         \"))))(Tile((id 222bcc10-a723-47ca-8b61-578cf9f6a899)(label(fun \
         ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 37))(sort Exp))))))(shards(0 \
         1))(children(((Secondary((id \
         eb6ab226-a1af-459a-81bb-7373c87e2752)(content(Whitespace\" \
         \"))))(Tile((id \
         c137e003-c383-4dd5-a9fc-b2cdaee94321)(label(\"(\"\")\"))(mold((out \
         Pat)(in_(Pat))(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0 1))(children(((Tile((id \
         8b9c986c-707a-417c-bdd6-00411d01836e)(label(acc))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         851be568-09a6-4c0f-9688-fb4e0a96c90d)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 44))(sort Pat))((shape(Concave \
         44))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         713b7370-a448-4f5c-8a6a-0065ab4a06f9)(content(Whitespace\" \
         \"))))(Tile((id \
         e5214210-c48d-4ce2-bc8d-a15ef108d0c2)(label(w))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children()))))))))(Secondary((id \
         ae965b79-09fa-4c4b-9055-17acc5b815a5)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         70210ee8-98d9-4edb-9c5d-952ab2726d10)(content(Whitespace\"\\n\"))))(Tile((id \
         c15eabc5-83d1-45f8-86e7-c54c87da95f8)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         f5ca3f39-3ee1-4363-a57e-3183a586a4dd)(content(Whitespace\" \
         \"))))(Tile((id \
         16a6221e-a37f-4e36-936e-32970e70fbf7)(label(running))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         e547060d-119b-4864-9244-a28f24f01d3f)(content(Whitespace\" \
         \")))))((Secondary((id \
         6f50f663-c01c-4953-b4a9-c96bbd0967d1)(content(Whitespace\" \
         \"))))(Tile((id \
         8bf591b2-545d-462c-9091-0c939bade235)(label(acc))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         42eb8202-ac53-4a6a-8013-582770b22a65)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         f88d82b3-679a-40e7-931e-60bfd12b8ed0)(content(Whitespace\"\\n\"))))(Tile((id \
         3245e3f7-dc7b-46f0-af7d-8eda1a9e19db)(label(running))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         85cb970e-355c-46b4-9949-5e743f611ac6)(content(Whitespace\" \
         \"))))(Tile((id \
         e248c310-cb42-43ea-960d-8c133a38151f)(label(+))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 28))(sort Exp))((shape(Concave \
         28))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         6a9b68ac-d9b1-49ad-82af-d2723c71c7f2)(content(Whitespace\" \
         \"))))(Tile((id \
         b3f5efae-0be2-40e4-9420-5e0bddb542c3)(label(w))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         2ede2a26-9cfe-43b8-ba24-cb89e571c2bf)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         35a341cf-1727-453c-8c70-6d2f8af39c2c)(content(Whitespace\"\\n\"))))(Tile((id \
         fbcc0bff-295c-4ad8-a2ec-2caecff14043)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         1eac930e-4c11-4d1a-9c2e-de020f3097fc)(content(Whitespace\" \
         \"))))(Tile((id \
         1de0fbfd-8536-48de-aca8-fa3ff5b6102c)(label(total))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         162bcd74-cb6f-4549-bd1e-9a8c5c8a5989)(content(Whitespace\" \
         \")))))((Secondary((id \
         6147da21-4dc7-40b2-b1a0-cfd3ac2fa9b5)(content(Whitespace\" \
         \"))))(Tile((id \
         07419f1d-c8c6-4fb6-b69c-2e6377650568)(label(fold_left))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         eced07e3-e68d-446e-bfa8-924a0d0c0147)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         bab230fe-80b2-474a-ad2f-def5a6bbfda5)(label(weekly_amounts))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         14052e21-246a-46cc-a848-9f5326266989)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         a49e226e-a97c-4bbb-8356-ecd15377efde)(content(Whitespace\" \
         \"))))(Tile((id \
         5294881a-f740-43df-a034-3459912990e6)(label(sum))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         2dc07c3b-96aa-401b-bbbc-b1f4491eb5d2)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         ff3069a5-65a8-4d23-b217-0ba7a3488289)(content(Whitespace\" \
         \"))))(Tile((id \
         5109ee2e-c5eb-42da-a61a-2d95fc2bd727)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         fb8081a3-949d-49af-b8a5-7e5fb5a8672c)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         be1f3cee-30d4-4afe-be4f-6a70222225dd)(content(Whitespace\"\\n\"))))(Tile((id \
         2ef6f830-4aa3-4a9c-ba2d-f6513ac2f4d6)(label(total))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         6e7d1ce3-115e-48e3-acba-ecf5bad4730f)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         5ed3f37e-951e-4b7b-81bc-78f0e0f5dc03)(content(Whitespace\"\\n\"))))(Secondary((id \
         75b5c07f-8484-4551-80c9-eaf4607fc317)(content(Whitespace\"\\n\"))))(Secondary((id \
         952fa54b-cfd3-4bc0-b59d-fce59754a2d6)(content(Comment\"# EXERCISE 1: \
         Step into the map #\"))))(Secondary((id \
         02528693-b405-4d9e-9e1d-f8e18a08a620)(content(Whitespace\"\\n\"))))(Secondary((id \
         84380e0b-048b-49f0-beb1-c21ea67dd3ac)(content(Comment\"# 1. Add a \
         probe to `weekly_total(shade)` below. #\"))))(Secondary((id \
         73d7d169-eb56-4493-85af-3706ee14ff60)(content(Whitespace\"\\n\"))))(Secondary((id \
         e659165f-55a5-480e-b036-52426afd1d37)(content(Comment\"#    It \
         returns 4270. How does it get there? #\"))))(Secondary((id \
         f01c139a-d4c0-42b2-a43e-9e7a7de1f9c4)(content(Whitespace\"\\n\"))))(Secondary((id \
         e14f451d-8c36-4855-9474-723f739fab4f)(content(Comment\"# 2. Click the \
         sample and Step Into (Enter). #\"))))(Secondary((id \
         eae6439c-b211-4903-bb3a-781e234bb5cf)(content(Whitespace\"\\n\"))))(Secondary((id \
         c52ea42c-5705-42a2-a574-d8a779cb5bf6)(content(Comment\"# 3. Turn on \
         auto-probe inside `weekly_total`. #\"))))(Secondary((id \
         b50c04f4-a46a-43ac-ae54-be55cf50a3f8)(content(Whitespace\"\\n\"))))(Secondary((id \
         27149467-7e57-42fc-b1cf-f20e68d452ab)(content(Comment\"# 4. The map \
         callback shows each plant's `daily` water #\"))))(Secondary((id \
         9d98e380-e107-4498-a8c4-90b093b31057)(content(Whitespace\"\\n\"))))(Secondary((id \
         54d9a94a-9a49-40ca-b0b6-1bbe408f3503)(content(Comment\"#    and the \
         `daily * 7` result. In Many mode you see #\"))))(Secondary((id \
         e0e5945d-a35f-4c2f-90a8-d9640436c0e4)(content(Whitespace\"\\n\"))))(Secondary((id \
         5d2f0ce9-6fb9-453d-bb84-4965a68b81bb)(content(Comment\"#    all 3 \
         transformations side by side: #\"))))(Secondary((id \
         a6ac52fb-8d63-4eb8-ab04-a0fa0551943c)(content(Whitespace\"\\n\"))))(Secondary((id \
         33299b59-aee8-47a0-b8b8-9325f8203d79)(content(Comment\"#    daily: \
         [250, 200, 160] and daily*7: [1750, 1400, 1120] #\"))))(Secondary((id \
         74f72041-b824-426b-b26c-612f21b86d70)(content(Whitespace\"\\n\"))))(Secondary((id \
         5abd8c9f-50ec-4fcc-9878-46ec28e37b03)(content(Whitespace\"\\n\"))))(Secondary((id \
         7f70e318-a5ea-481d-84b1-3cf34340ee54)(content(Comment\"# EXERCISE 2: \
         Now look at the fold #\"))))(Secondary((id \
         29356356-f411-44b6-b332-d2d2e076755f)(content(Whitespace\"\\n\"))))(Secondary((id \
         dedd5afd-7b9e-41e9-8276-bc0b3c916a84)(content(Comment\"# 5. Still \
         inside `weekly_total`, look at the fold #\"))))(Secondary((id \
         1028197e-c6b5-4f25-b4e5-be6377fbbe97)(content(Whitespace\"\\n\"))))(Secondary((id \
         fd95ae2d-8787-40fa-a3a1-d746cd36b493)(content(Comment\"#    \
         callback's samples. In Many mode, `running` shows \
         #\"))))(Secondary((id \
         45c119a8-fa2e-4659-92f9-f0e4fb5b5504)(content(Whitespace\"\\n\"))))(Secondary((id \
         1fd5a8df-76ef-4fd6-a0a7-1b99358f2cf0)(content(Comment\"#    the \
         accumulator: [0, 1750, 3150] and `running + w` #\"))))(Secondary((id \
         85770655-1341-4cc8-8043-d25be0eca33d)(content(Whitespace\"\\n\"))))(Secondary((id \
         a3d6ed20-2b59-45df-9f69-746e70c6ae97)(content(Comment\"#    shows it \
         growing: [1750, 3150, 4270]. #\"))))(Secondary((id \
         5ea07a68-f0f0-4730-a76e-5aa0c3dad355)(content(Whitespace\"\\n\"))))(Secondary((id \
         46e986cf-75c3-41c7-bb53-9df1e77cf522)(content(Comment\"# 6. Use the \
         dynamic cursor bar at the top to navigate #\"))))(Secondary((id \
         c02fbe9c-cf0f-48a0-a7a0-4a5c48113492)(content(Whitespace\"\\n\"))))(Secondary((id \
         f2e39807-8512-495d-9997-67d1744d3e39)(content(Comment\"#    back out. \
         Try stepping into `weekly_total(all)` \\226\\128\\148 \
         #\"))))(Secondary((id \
         c4ed015b-da49-4b70-88d5-6cc44effc551)(content(Whitespace\"\\n\"))))(Secondary((id \
         66c19c8e-1c2b-4777-a0ea-b427832858e4)(content(Comment\"#    now there \
         are 5 iterations each. #\"))))(Secondary((id \
         0fdf0cb2-ef7f-4e27-9c36-b792ada16d81)(content(Whitespace\"\\n\"))))(Secondary((id \
         7500dc1f-30e3-4688-bdaa-4cfa428cd445)(content(Whitespace\"\\n\"))))(Tile((id \
         1731e684-eccf-457a-8384-733bc863c49d)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         3221ff76-cc59-412b-bee6-bd385f176ab0)(content(Whitespace\" \
         \"))))(Tile((id \
         9ed64389-9c5b-46d9-baa0-2534299f3db3)(label(shade))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         bbd5be91-8501-4163-8e68-8356a55fd931)(content(Whitespace\" \
         \")))))((Secondary((id \
         5e473d4a-0609-473f-8269-a410d7d9dfe2)(content(Whitespace\" \
         \"))))(Tile((id d746a84a-4bf6-4b17-a278-dc8a5ce01fd4)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         38b692cf-6009-4466-8724-f2611d8dd87a)(label(fern))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         5ac942f3-da3e-4aaf-97da-62c595747e82)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         6d1db4f1-fd4e-4579-ac2e-a9eeadb04424)(content(Whitespace\" \
         \"))))(Tile((id \
         2f4563af-3d3c-4dbd-b95f-faf50b89021f)(label(lily))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         6f041c65-98a8-41a8-84d3-56d406868e2f)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         8ef86d0b-6d18-48c9-9253-124f7884b11a)(content(Whitespace\" \
         \"))))(Tile((id \
         dfb0f3ab-67e3-45e6-9a71-07983e0d38da)(label(daisy))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         a84f6c17-6378-4ec0-bbe9-be2a8b299358)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         8c2e907e-a379-478a-a0f0-8149b4818320)(content(Whitespace\"\\n\"))))(Tile((id \
         293fd504-0fdc-43c8-a605-badf930240bd)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         918699ab-1701-4d94-b4ed-4aa6672ec2c3)(content(Whitespace\" \
         \"))))(Tile((id \
         6dbc1ce2-7f19-4403-a245-2b55c147a7d7)(label(sun))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         dede1ea3-df4b-4eaa-811a-dd39ce246561)(content(Whitespace\" \
         \")))))((Secondary((id \
         7a3dafac-cf3e-46a6-8646-377147314491)(content(Whitespace\" \
         \"))))(Tile((id 34266292-ea9d-472a-9e28-1da904690019)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         afd4942f-bd7b-44a5-8ce6-db93e1118171)(label(orchid))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         86f73b7e-1863-432e-ae3a-7c2901559045)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         a972d6d1-7599-4d1f-8702-54b401946276)(content(Whitespace\" \
         \"))))(Tile((id \
         6e7b9fbb-2b9f-4032-ae61-4cf3be1cfe5e)(label(cactus))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         2d853d2f-e493-496c-8531-0f5d35663330)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         3fb3acb5-411c-4f67-bc8f-3c74d62f7606)(content(Whitespace\"\\n\"))))(Tile((id \
         8be39c52-5575-4477-b7a0-528ef774f17f)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         5902ac3f-73e1-4292-832d-718553bb4d36)(content(Whitespace\" \
         \"))))(Tile((id \
         08ef2bf6-e831-466b-92b8-46ca1a4bdce6)(label(all))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         af2d9dc6-cd6c-4200-88b4-14079f84f9ee)(content(Whitespace\" \
         \")))))((Secondary((id \
         08030b9a-b2dd-49c5-af93-e4ac6f43e519)(content(Whitespace\" \
         \"))))(Tile((id 75f4806f-2b15-4c1c-99fc-a605379c02a1)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         9e765a9d-385b-4333-8908-97d2f6354b21)(label(fern))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         bb3731a1-bde5-4d69-abc0-d6cf36822734)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         21f60ad0-c8d7-4142-831c-2c02c69a007e)(content(Whitespace\" \
         \"))))(Tile((id \
         910e10e3-85e5-4769-bfc1-1eb60558a8ac)(label(orchid))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         81d608f7-00c9-47b7-8fe4-fd6eb473904b)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         0a376510-f173-4132-ab32-d582853bf597)(content(Whitespace\" \
         \"))))(Tile((id \
         7887143a-215d-40e8-858b-48f8d52b7184)(label(cactus))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         4d443422-89a4-4e83-a96d-6ca588a57225)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         ed4c2c49-937b-46c8-bbae-72df1de6f3e3)(content(Whitespace\" \
         \"))))(Tile((id \
         dcc1ca24-1824-4f77-85db-cd4c0a33559d)(label(lily))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         e37011a5-d28c-4a16-8bd8-c0ae581662d5)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         03063763-3902-46e8-91b5-aa29816eb717)(content(Whitespace\" \
         \"))))(Tile((id \
         fd403c43-aeab-406f-aff8-1d24bacaa61e)(label(daisy))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         ba07f915-6927-4226-a9d6-a5d06143ff69)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         0746552e-43c8-47da-bb8e-5b6bc1140faa)(content(Whitespace\"\\n\"))))(Secondary((id \
         b066c7bc-2a49-45c7-bb7b-6fa1a1fe0e3c)(content(Whitespace\"\\n\"))))(Tile((id \
         a5968c6b-a227-43cd-aef5-229d899a4ff9)(label(weekly_total))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         aed7077d-a0b2-43f2-af77-f8c50ad39396)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         18d7ec82-820a-451f-9f80-f09af18ebcff)(label(shade))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         f89cfcca-b9f7-4128-b8dc-f23cbed8e5e8)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         ed96d9da-d846-437f-9e6c-710febb4c45e)(content(Whitespace\"\\n\"))))(Tile((id \
         5f452673-0cf3-4146-8266-6f8c765d48cc)(label(weekly_total))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         157907a2-2313-44ee-9443-265dad9fe76d)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         9a8e07f7-cc6c-47ef-a06b-13e47d4c07fa)(label(sun))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         8a6c135f-2ef9-4dc6-b4ab-67fca8958ab2)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         e29c23d1-ea30-4d0c-acd4-c73a2011a2b6)(content(Whitespace\"\\n\"))))(Tile((id \
         a2e6c816-01f8-4a75-b865-b99e66c57f0c)(label(weekly_total))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         bd667e33-3f3b-4fad-b5cf-8ce7b0a40be1)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         af7349b9-4fa6-47bc-b5f5-99363cde897b)(label(all))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         7a1f5679-ae75-461e-a777-a0014dfaeca8)(content(Whitespace\"\\n\"))))(Secondary((id \
         2becb22d-a2e5-4cc6-a4ac-3a4b28cb9acd)(content(Whitespace\"\\n\"))))(Secondary((id \
         452aed76-ae4c-4074-a948-6182971668e0)(content(Comment\"# END \
         #\"))))(Secondary((id \
         37828135-e213-4c67-b051-bad1793023f0)(content(Whitespace\"\\n\")))))";
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
