let out : string * Haz3lcore.PersistentSegment.t =
  ( "Study / tutorial / 05-variant-map-fold",
    {
      segment =
        "((Secondary((id \
         78baf32f-9b4a-433d-adf8-9a6bfe94032a)(content(Comment\"# PART 5 \
         VARIANT: STEP INTO WITH MAP + FOLD #\"))))(Secondary((id \
         cd448204-ae66-43ef-8cff-ce2de9894a58)(content(Whitespace\"\\n\"))))(Secondary((id \
         eb6b8359-985f-4b31-a9aa-db0ad1b62a11)(content(Whitespace\"\\n\"))))(Secondary((id \
         7180ef47-ddfe-4973-84e3-c83271250bbe)(content(Comment\"# This \
         function has a two-stage pipeline: map transforms \
         #\"))))(Secondary((id \
         c574ee4d-86f5-47b7-9b51-11e65649cbf2)(content(Whitespace\"\\n\"))))(Secondary((id \
         8ac41fda-8d02-4fe1-b7ad-00f72fe7ca51)(content(Comment\"# the data, \
         then fold aggregates it. From outside you see #\"))))(Secondary((id \
         7941dc00-3390-40fe-8cff-8737ffbf65b6)(content(Whitespace\"\\n\"))))(Secondary((id \
         d95ba778-ecf3-443b-bd0e-9b9c862fbf90)(content(Comment\"# one number. \
         Step Into reveals the whole pipeline. #\"))))(Secondary((id \
         c37dfa75-554b-45c8-bc55-a03618f190e8)(content(Whitespace\"\\n\"))))(Secondary((id \
         9ac341d3-ebd5-4966-a7ff-4cc1f6acc621)(content(Whitespace\"\\n\"))))(Secondary((id \
         6878382c-9f66-44f6-b370-cd3ffe613aa3)(content(Comment\"# \
         ============================================================ \
         #\"))))(Secondary((id \
         d56af7e2-adfe-4170-8adf-7f46751ae9c1)(content(Whitespace\"\\n\"))))(Secondary((id \
         3448d5a0-a498-4845-a5f5-d778f0ec736f)(content(Whitespace\"\\n\"))))(Tile((id \
         62e4d1f9-17f7-4c44-9303-f91be4b4b388)(label(type = in))(mold((out \
         Exp)(in_(TPat Typ))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         a4e96710-d113-4d56-a13a-df5001e442fb)(content(Whitespace\" \
         \"))))(Tile((id \
         8a642a78-1c3b-4f1b-81aa-64259fb7aed9)(label(Plant))(mold((out \
         TPat)(in_())(nibs(((shape Convex)(sort TPat))((shape Convex)(sort \
         TPat))))))(shards(0))(children())))(Secondary((id \
         46edf821-8e9e-4d7a-81a5-20772adf3a67)(content(Whitespace\" \
         \")))))((Secondary((id \
         3f25d6cd-61f7-489c-9ae4-3b850c59ac75)(content(Whitespace\" \
         \"))))(Tile((id \
         a7ce4d28-bbe9-4c33-accb-cc4de0d250c7)(label(\"(\"\")\"))(mold((out \
         Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0 1))(children(((Secondary((id \
         48593ad8-a0e6-4382-be2e-d8b5c82bc520)(content(Whitespace\"\\n\"))))(Tile((id \
         5994ad68-3b3b-4391-bf15-a9a484f3a5f2)(label(name))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         b76cd83a-f3d0-4b4c-9263-193444739d17)(content(Whitespace\" \
         \"))))(Tile((id \
         bf4c4a18-a4b1-4b8a-b3df-f69b098110f0)(label(=))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 39))(sort Typ))((shape(Concave \
         39))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         41c4ac3d-39ea-4ea9-9ea6-83e53d77c979)(content(Whitespace\" \
         \"))))(Tile((id \
         07be2730-fbe5-46a0-9faa-92040d2d99f1)(label(String))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         63443178-8b3d-43a9-8d5d-6dc8a0b0b1ef)(label(,))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 44))(sort Typ))((shape(Concave \
         44))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         2cbe7dc3-98e4-4430-884b-61dc33a59f66)(content(Whitespace\"\\n\"))))(Tile((id \
         fc8147ca-24de-483b-846d-882d2df7fe29)(label(icon))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         06c3e13c-fa96-4d47-bc8f-d7a287a3838e)(content(Whitespace\" \
         \"))))(Tile((id \
         42dbd5d0-a42b-43c6-ad34-780b1fcbd536)(label(=))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 39))(sort Typ))((shape(Concave \
         39))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         bcc32696-5652-4702-a78a-833456eaadae)(content(Whitespace\" \
         \"))))(Tile((id \
         30902757-d9f3-4f5b-8023-85814219a759)(label(String))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         2781c6e4-daac-46a3-ae71-7871a97004c9)(label(,))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 44))(sort Typ))((shape(Concave \
         44))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         feb909e1-e3bc-425a-83f2-8b3a1fd6713d)(content(Whitespace\"\\n\"))))(Tile((id \
         f06afaf5-a9ce-456a-be01-ebcf090fa35e)(label(water))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         7b063a16-04bb-4f6d-90cc-2ad7139e7c7a)(content(Whitespace\" \
         \"))))(Tile((id \
         d25ee421-9c4e-4bc0-9bd9-625ebee56163)(label(=))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 39))(sort Typ))((shape(Concave \
         39))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         554e6803-ee8e-4f94-9733-b688ab4f37df)(content(Whitespace\" \
         \"))))(Tile((id \
         2ee2aca8-68ff-4871-b2a3-9cab6b8caef6)(label(Int))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         42bddd98-2c32-461b-872d-fdad4ee3c03a)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         cc4c4aa7-1d20-4a26-aefb-0b3c5cc5361c)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         e5227e6c-a344-4e99-93d1-78d259f909c2)(content(Whitespace\"\\n\"))))(Secondary((id \
         115969ac-345f-4d57-b7a8-9e2d16cd14f9)(content(Whitespace\"\\n\"))))(Tile((id \
         71aa0b5d-3097-4562-83d1-3bb55caf0628)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         9d8a7a0d-5843-479a-ab52-d86fabceed34)(content(Whitespace\" \
         \"))))(Tile((id \
         2e7a3baf-f855-4655-af18-a85d1c5ef390)(label(fern))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         3e2f460d-ceb5-4ea3-afc4-98a5b8f7ef69)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 24))(sort Pat))((shape(Concave \
         24))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         1772ec3e-39da-4412-9f2a-14dcf9e7acff)(content(Whitespace\" \
         \"))))(Tile((id \
         66086f0e-c970-4505-825e-f30c55dd4e93)(label(Plant))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         c9071732-7810-49ad-9fb4-fa746ed59d4c)(content(Whitespace\" \
         \")))))((Secondary((id \
         75704381-92ca-4e33-b356-88983f78a0c8)(content(Whitespace\" \
         \"))))(Tile((id \
         602ddb6f-feb5-4326-9790-3f89b3faa318)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         588e9c9c-c00e-4d79-9901-4a33a97f4170)(label(name))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         b231969a-add5-49d6-b669-a426c7bf37eb)(content(Whitespace\" \
         \"))))(Tile((id \
         e0e2f1de-9cc5-4a1e-be01-0d7424f8e786)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         7a6efa62-a278-4286-927a-54395b70972e)(content(Whitespace\" \
         \"))))(Tile((id \
         7afeca32-6b47-45f0-880e-1c7c880366a5)(label(\"\\\"Fern\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         248d3d14-afd6-4bb7-b14c-b02f232a8d7f)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         64a2be07-b53e-4124-9e7b-e4d3dfd3457c)(content(Whitespace\" \
         \"))))(Tile((id \
         1b230e8d-2a8f-4dd2-8bf6-91a7a29e5b32)(label(icon))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         3e48f14c-bba7-454d-8ac7-44f907d01e9f)(content(Whitespace\" \
         \"))))(Tile((id \
         0642c3e6-4a1a-4722-8685-6e7f6948cf32)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         c212b33a-434c-459b-9590-b18221402931)(content(Whitespace\" \
         \"))))(Tile((id \
         fba9c5a2-2852-48a9-80ad-7b022b909294)(label(\"\\\"\\240\\159\\140\\191\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         860cd29d-f57c-49e2-a64e-96f3843fd2ff)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         6cdebbc1-39e3-4742-9138-b6528e8106c5)(content(Whitespace\" \
         \"))))(Tile((id \
         3a5eda2d-0de2-4581-9ce9-91b75dd73a01)(label(water))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         c1b874d3-feee-4808-ac19-377e69fba043)(content(Whitespace\" \
         \"))))(Tile((id \
         a7fed597-4fd5-46b0-b124-2a05f89f3730)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         a5bbf0cd-2151-469f-b08a-f6b65653c485)(content(Whitespace\" \
         \"))))(Tile((id \
         4965adaf-d1b6-428e-9778-89856a38ee00)(label(250))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         d9f030da-df6b-4a18-98b5-9e9a2466b935)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         446ff685-5c75-4db5-a77b-d7ae01421d39)(content(Whitespace\"\\n\"))))(Tile((id \
         d7e342a1-140d-427b-b95d-66ea569fc1f3)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         c4a1971a-7729-4646-8c7c-8df6817b1223)(content(Whitespace\" \
         \"))))(Tile((id \
         14781167-e748-40c6-ab55-8589fdecb280)(label(orchid))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         6e8eb1f3-a3c9-4f0b-be88-f219b2c35c3e)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 24))(sort Pat))((shape(Concave \
         24))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         4679b11d-be4f-4928-bc1b-c28909bb80f0)(content(Whitespace\" \
         \"))))(Tile((id \
         0ef1f1e0-e36d-4240-974a-8349c617c711)(label(Plant))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         22d415e2-fad2-4e76-86db-a4cd2c1c4412)(content(Whitespace\" \
         \")))))((Secondary((id \
         7feffd18-7156-48b8-b7e1-6b4444dc4891)(content(Whitespace\" \
         \"))))(Tile((id \
         c8bfcabe-2f15-44e2-868a-2836515f7a04)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         31700039-eb5c-40c4-b93f-d1bcfba5ddc6)(label(name))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         39f0a111-878c-434a-bfaa-fb17d92d14d3)(content(Whitespace\" \
         \"))))(Tile((id \
         96947f96-04ed-4519-b9ba-f2b5afbb55df)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         f3bfd666-63f4-4ed7-abd1-d18f4468a4d5)(content(Whitespace\" \
         \"))))(Tile((id \
         6ce27c1c-39cd-4d5a-898a-df74aa842ec1)(label(\"\\\"Orchid\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         60e2e7f4-c77c-43b3-9316-479462e6785b)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         927ea38c-6b02-4bf8-9970-8e8872e60296)(content(Whitespace\" \
         \"))))(Tile((id \
         13e9956b-115d-4fe9-9849-7f4583083f63)(label(icon))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         4e66cb15-814c-40be-8efe-721bcf7621ca)(content(Whitespace\" \
         \"))))(Tile((id \
         e8754009-e832-4b1f-a70a-3e3121fe597f)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         4463a427-dac1-4d64-9584-1688ca206c5d)(content(Whitespace\" \
         \"))))(Tile((id \
         5fb76a58-c32b-4526-b332-d6c7d7a3266b)(label(\"\\\"\\240\\159\\140\\184\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         e5dd33e3-eeef-439e-84a9-8353bff67239)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         ed578f60-0563-4c8e-ad62-81377de60bbf)(content(Whitespace\" \
         \"))))(Tile((id \
         5a60c754-3549-4d4e-adbe-93f541e1dc05)(label(water))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         0f892675-756e-4746-b202-4fc3308668ed)(content(Whitespace\" \
         \"))))(Tile((id \
         0cd43c0a-7a58-4160-9970-70521424fe97)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         2370a7eb-de79-4e42-99be-e2129e8ad245)(content(Whitespace\" \
         \"))))(Tile((id \
         7d2e6073-fc3e-4532-b717-7ebeb09b4fca)(label(180))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         264c81f1-a29a-4aa8-bfce-ecb9587a3f25)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         9b64a46e-7dee-4c0a-9f9c-f81ead340b8c)(content(Whitespace\"\\n\"))))(Tile((id \
         f7c80944-ea36-4d9d-b5d8-cf78ce500ded)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         bd621827-443e-4b29-84a8-8f737e9bb171)(content(Whitespace\" \
         \"))))(Tile((id \
         397331a9-94af-401d-a62e-15a019e9b0f9)(label(cactus))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         7aec8ae9-9fba-4267-90d2-823bc7a50636)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 24))(sort Pat))((shape(Concave \
         24))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         c2d1e30e-cbd3-47d4-aa23-c58470a7b8f8)(content(Whitespace\" \
         \"))))(Tile((id \
         690cfef1-bf8e-4d52-b771-cc8b92c5189a)(label(Plant))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         57dff6d0-8da6-4c02-9adb-7619e392a6c6)(content(Whitespace\" \
         \")))))((Secondary((id \
         3a6ed73e-512a-4682-9c75-a1788f404b1f)(content(Whitespace\" \
         \"))))(Tile((id \
         a74bc7f4-85c0-445f-a5a0-829be7be666e)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         1a47b98c-9144-4426-9a38-20f558693b3f)(label(name))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         ed13e8ab-5195-4d3c-94a9-43125cfb7cff)(content(Whitespace\" \
         \"))))(Tile((id \
         8d17f5e8-44e6-43da-b20e-81043a5efa22)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         1298b20b-9207-4216-a511-24d3b6bfee3f)(content(Whitespace\" \
         \"))))(Tile((id \
         a8f6f2a5-a582-4bdf-8855-2975038d6f24)(label(\"\\\"Cactus\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         be4bd90b-cf55-4d72-8ac2-fbbc12d2b990)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         0f80caaa-00c9-432d-bd5b-30c55f8bba2d)(content(Whitespace\" \
         \"))))(Tile((id \
         95413a45-3ac1-461b-ba5f-2cf0601f32d1)(label(icon))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         e2bd5e61-77c4-42f8-a72a-a3b40bb35eac)(content(Whitespace\" \
         \"))))(Tile((id \
         70293179-6439-4951-b959-e7ff3d0e3942)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         4c2891d4-1a63-468d-8dd5-4c2f43d50d34)(content(Whitespace\" \
         \"))))(Tile((id \
         018b1830-ebd9-4f25-a983-6cecab628bb3)(label(\"\\\"\\240\\159\\141\\132\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         ae4848b4-8c2a-4a08-8938-71211bffb76f)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         269e7aa4-33c9-4154-9238-ed102199bc67)(content(Whitespace\" \
         \"))))(Tile((id \
         fe421519-7c6d-4668-8564-fbd631e13afb)(label(water))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         ee340f3f-767d-4053-be84-2e0c55afc14f)(content(Whitespace\" \
         \"))))(Tile((id \
         0258c9eb-f740-4ba5-9c5b-fb5abe548546)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         e6c5e914-0b6b-4983-9107-d4fc5f2184d3)(content(Whitespace\" \
         \"))))(Tile((id \
         ad20f5b1-390e-45bb-b292-332b2c3b31cf)(label(50))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         6c7cf1e4-50e6-43f6-8ce5-65b012e86491)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         a6e54b40-ab73-4382-9505-cadf3787db0c)(content(Whitespace\"\\n\"))))(Tile((id \
         cf5e0982-1b01-4f41-bb2f-af079e2c844e)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         fc99e378-e8fb-4783-881d-c9ac751263fb)(content(Whitespace\" \
         \"))))(Tile((id \
         538d42f3-50f4-474d-b922-2411a09a1ce1)(label(lily))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         dc8ec3e2-c9ab-415f-a1bb-aa0e7232caa7)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 24))(sort Pat))((shape(Concave \
         24))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         7879ccc3-3974-4eaf-bcc9-6f9214ddc394)(content(Whitespace\" \
         \"))))(Tile((id \
         e1732e87-f0f4-4117-8935-cf0d1d4c7291)(label(Plant))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         2a8aea73-e87b-46ad-a000-87157fd334ba)(content(Whitespace\" \
         \")))))((Secondary((id \
         14ef483a-63a2-4327-a124-688eb899a652)(content(Whitespace\" \
         \"))))(Tile((id \
         5eae1964-0de6-4b62-a0a1-7b64ca340cd5)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         b0771dca-81ac-4af7-8b33-24ac0e62a92b)(label(name))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         48244754-f71a-4835-9804-2dcb7ec4a587)(content(Whitespace\" \
         \"))))(Tile((id \
         cc9f0832-35e7-459e-8ff7-3cff6038e53b)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         4bd95714-b825-45cf-841d-2fa7838d644e)(content(Whitespace\" \
         \"))))(Tile((id \
         8221252e-56f4-4a48-bff9-1801239c0165)(label(\"\\\"Lily\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         613874e0-bb3c-4957-99d4-cef17f572200)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         3e4c40eb-f440-4e2f-a0ef-7ba2b2ae5dcb)(content(Whitespace\" \
         \"))))(Tile((id \
         9ac324d8-3605-41e1-8bdc-746062f8f3f7)(label(icon))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         2745b5b3-da68-4bd4-b697-d472532bd4de)(content(Whitespace\" \
         \"))))(Tile((id \
         0505f667-5f64-4577-acc0-58dda9fd3e7b)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         3da74e94-b08f-4952-af7f-ddb27397aa2d)(content(Whitespace\" \
         \"))))(Tile((id \
         9df91c4a-bcf1-45b9-aef1-d2b73be0f0a6)(label(\"\\\"\\226\\152\\152\\239\\184\\143\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         27833a0d-15fb-4b32-aeaf-c4c9ded0b0f9)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         efa27ccc-6cf8-44b1-bc20-5486dd2f97c4)(content(Whitespace\" \
         \"))))(Tile((id \
         7866bdf6-51c7-431c-8144-13d00c8966c9)(label(water))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         de0b7553-17f3-48c3-bdd5-da6ccae968e4)(content(Whitespace\" \
         \"))))(Tile((id \
         db21407b-ad60-4759-8044-134a9aa551dd)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         1f9190e4-1a7b-4681-be30-9f94d705cce8)(content(Whitespace\" \
         \"))))(Tile((id \
         c51264f1-0d93-4c0f-a089-c54db4004551)(label(200))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         8f0a7e7e-7d56-4155-a779-b7880f01a5ce)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         cee1aa9b-c8dd-4cb9-8910-b202e8c9454b)(content(Whitespace\"\\n\"))))(Tile((id \
         ce2db648-aa4f-4ac5-904c-3f9d4d747054)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         852b62f2-c6b3-443c-9b5f-78dce6e7164b)(content(Whitespace\" \
         \"))))(Tile((id \
         f1e8a274-51fa-40fd-9005-c6776c9b9164)(label(daisy))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         87419827-a470-418b-939c-7be369200de2)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 24))(sort Pat))((shape(Concave \
         24))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         ac54c8ef-6122-49d0-a763-6f717fbbdb29)(content(Whitespace\" \
         \"))))(Tile((id \
         6ae938c8-318e-480d-be97-163a3531e23f)(label(Plant))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         83f2b285-6536-4d12-97bf-2af4379c28ae)(content(Whitespace\" \
         \")))))((Secondary((id \
         a5788352-bbc1-482a-8bc2-f2946efc13bf)(content(Whitespace\" \
         \"))))(Tile((id \
         2921869b-d57d-4d96-af28-dd8bc7e6ccc4)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         e2bc31f0-d53f-482d-858f-7fdebcdc3697)(label(name))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         66197d81-57da-4609-8b46-ea8851376cf6)(content(Whitespace\" \
         \"))))(Tile((id \
         306e00c3-4f9b-420c-9a7e-a150c845b23a)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         286a04b0-afb2-4a08-ab85-3b5caa6c52c5)(content(Whitespace\" \
         \"))))(Tile((id \
         e19096b1-f03c-44d2-b953-b8b22bfa1158)(label(\"\\\"Daisy\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         e7fa098a-d3bb-4286-8a51-8a1a774bf407)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         7d7097e9-e987-474d-af96-286506b300c3)(content(Whitespace\" \
         \"))))(Tile((id \
         0eaac512-c904-4e74-a2f9-2a2101f2fb09)(label(icon))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         53e8eee8-4662-4eff-90e2-98c336cfcdc9)(content(Whitespace\" \
         \"))))(Tile((id \
         030b9a66-0637-42e5-8ab5-2b787e7656e4)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         78324f9f-62c5-43e3-acb8-ffde378b5a95)(content(Whitespace\" \
         \"))))(Tile((id \
         a3a0fb96-74b4-4a26-986f-79482a748d03)(label(\"\\\"\\240\\159\\140\\177\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         9215077b-37ba-46ba-bcca-ab307b2ebcb9)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         49f6eb9c-4810-416a-a8e2-6a690f23ccc9)(content(Whitespace\" \
         \"))))(Tile((id \
         d31e9eca-92d5-4d1f-b97b-ab1a99824fa2)(label(water))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         e550dba2-e084-4ccf-9482-040dbec29c0b)(content(Whitespace\" \
         \"))))(Tile((id \
         34af4dc3-1460-4070-ac28-03112db7e774)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         8c586d30-61e9-433e-833a-c9fab1d2b2be)(content(Whitespace\" \
         \"))))(Tile((id \
         bfc0c0c3-1ae9-446b-aa28-3ec4397c92d3)(label(160))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         9a2af662-2e43-48a0-a0af-210d33a53b43)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         1f58ae41-d756-42a6-a5a9-97df2423d639)(content(Whitespace\"\\n\"))))(Secondary((id \
         716cc36a-77d7-43b6-b7ce-a9c9349e21a3)(content(Whitespace\"\\n\"))))(Secondary((id \
         db08e2fc-9e03-4fd0-b77c-7010c71d9cc3)(content(Comment\"# weekly_total \
         computes the total weekly water for a garden. #\"))))(Secondary((id \
         27a9c69d-c470-469f-983f-050b95cf29ce)(content(Whitespace\"\\n\"))))(Secondary((id \
         e0877a58-e0ec-4d76-8375-9f36932a5678)(content(Comment\"# First it \
         maps each plant's daily water to weekly (x7), #\"))))(Secondary((id \
         164255c6-ae5a-4be2-924f-f0dbd46cef4b)(content(Whitespace\"\\n\"))))(Secondary((id \
         1cb8d855-1602-411e-bc74-d62f20bc4657)(content(Comment\"# then folds \
         to sum everything up. #\"))))(Secondary((id \
         0d4209d3-44c3-4bbd-b7bf-c12529dc89d4)(content(Whitespace\"\\n\"))))(Secondary((id \
         36cc1cee-f9db-4ecc-babe-a2c7806fe8a0)(content(Whitespace\"\\n\"))))(Tile((id \
         400375b5-5a3e-44d5-be8e-1efa67f0e610)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         ceee1eca-208b-4d5d-a428-364afe2e9f6a)(content(Whitespace\" \
         \"))))(Tile((id \
         4603b2cd-8c64-403b-8f43-4e524856099a)(label(weekly_total))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         a643f75f-79e0-4152-990c-76d2612a2773)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 24))(sort Pat))((shape(Concave \
         24))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         b4d61acd-ea86-4702-a4a4-d53b6ac9ef81)(content(Whitespace\" \
         \"))))(Tile((id cc438990-f054-4966-981c-72cba783b3c2)(label([ \
         ]))(mold((out Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape \
         Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
         63734279-82d8-4120-96bf-7b35c30d8164)(label(Plant))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children()))))))))(Secondary((id \
         c4fa1f45-1bae-4337-9338-f476f13eb5f4)(content(Whitespace\" \
         \"))))(Tile((id \
         6a62b527-5dcb-4660-95a8-26d00060a79a)(label(->))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 13))(sort Typ))((shape(Concave \
         13))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         b3777692-42bb-44eb-b176-cecd6d0b0546)(content(Whitespace\" \
         \"))))(Tile((id \
         ff276533-5970-4ea1-ac6c-13b11a1b0091)(label(Int))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         e0e7abdd-780f-4f45-8a1a-b8930fd5294e)(content(Whitespace\" \
         \")))))((Secondary((id \
         eb56f606-4873-437c-bb72-ea3c8050e670)(content(Whitespace\"\\n\"))))(Tile((id \
         1cb81ea0-f397-45fb-8749-edabef5ba19f)(label(fun ->))(mold((out \
         Exp)(in_(Pat))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         37))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         eda4ccb0-02c5-419d-9aff-b0dfecddc058)(content(Whitespace\" \
         \"))))(Tile((id \
         1aa3ad9f-6087-4f3a-acce-a0ea1fa9306f)(label(plants))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         3bca7c17-6854-4f6a-9805-aeb1e79bab63)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         cb534636-e9f7-486b-b6ba-bb3cdbfa909d)(content(Whitespace\"\\n\"))))(Tile((id \
         a1af72e6-85b9-4383-8829-85044b658932)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         28f033c8-4825-4429-a6de-360f44241986)(content(Whitespace\" \
         \"))))(Tile((id \
         0f14cdc8-fdce-4e72-b155-b7ff1c8184c2)(label(weekly_amounts))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         47c82d8b-1211-4970-8529-706d6fd094fb)(content(Whitespace\" \
         \")))))((Secondary((id \
         a6082176-bba9-438e-ae60-4180c9c0b82d)(content(Whitespace\" \
         \"))))(Tile((id \
         1c1160cb-11d2-4a0a-9cfd-d76bef8cd2f2)(label(map))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         c41da856-92e6-4b66-a630-c08376c55e7f)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         ee160fc5-66a8-4e33-b5c3-c64156dc7f45)(label(plants))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         00745832-fa06-43ec-9167-2f9e9440c84b)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         b4637c8f-edd5-4f77-aaf1-81e9ac6c1694)(content(Whitespace\" \
         \"))))(Tile((id 783ba253-9b04-43cd-ac40-8e77080db424)(label(fun \
         ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 37))(sort Exp))))))(shards(0 \
         1))(children(((Secondary((id \
         cfe97080-f1e1-48c1-b083-dbc2f37b3d6e)(content(Whitespace\" \
         \"))))(Tile((id \
         4b2fafd3-b231-4fe8-9528-2c2be5943af0)(label(plant))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         b350da16-81ab-4927-94a3-e41c52593ec9)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         069dc36f-aa10-4541-bbc2-b7ee8a6dcb4b)(content(Whitespace\"\\n\"))))(Tile((id \
         1135b1e7-96da-4a77-ad98-5fdf2083d1a6)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         59a9a0ab-c00f-41b7-ac53-115814ae8427)(content(Whitespace\" \
         \"))))(Tile((id \
         c1a823c6-52e2-4e16-b039-43117b1d24e1)(label(daily))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         d7581a90-5c54-4a42-b8dc-67a33b564ea7)(content(Whitespace\" \
         \")))))((Secondary((id \
         3b317606-2cc2-4b99-93a7-58d00d8463a4)(content(Whitespace\" \
         \"))))(Tile((id \
         78571173-5920-4dfa-99db-ec50f8ec3da0)(label(plant))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         573439b9-d4d8-4d01-9f83-d77454850608)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         90d01b26-0c7d-4a5c-98e8-ba050c74fce5)(label(water))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         88e876b8-b0cc-408c-9b9f-b02fbd908773)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         63da6d03-e273-4ac9-9bd5-c56c99a01b93)(content(Whitespace\"\\n\"))))(Tile((id \
         53ec4b17-ba12-4a35-ae98-7d31c01e2125)(label(daily))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         2b650ea3-dc7b-4a27-a17a-d7057f3abc48)(content(Whitespace\" \
         \"))))(Tile((id \
         10e445ae-5500-4040-856b-ed5d33fc5ac0)(label(*))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 27))(sort Exp))((shape(Concave \
         27))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         4c89e3dd-5942-4eb6-bfad-925d8c4f1cd1)(content(Whitespace\" \
         \"))))(Tile((id \
         b07efc65-2b86-4a70-a7fd-8d83784f55be)(label(7))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         f9823c7d-af25-4f4b-a5d0-cf8b67eb775e)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         2c94fc8f-fe87-417a-b2a4-ff04b5367ca9)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         8c09cfc1-3d88-486a-9359-79628097c907)(content(Whitespace\"\\n\"))))(Tile((id \
         cfc868d0-4bfb-4d22-b062-79334d8868d4)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         76242966-c4ff-4017-99b5-1eed9aad7ae8)(content(Whitespace\" \
         \"))))(Tile((id \
         08a8a580-d1c5-48d5-9c42-e659a2c7d940)(label(sum))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         a88e17e0-01fe-479e-a790-715316a20ebe)(content(Whitespace\" \
         \")))))((Secondary((id \
         ef2e36b6-40b6-43b1-bdfc-01cbf128600d)(content(Whitespace\" \
         \"))))(Tile((id 58c619f7-91eb-4544-9f07-456c1f7de280)(label(fun \
         ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 37))(sort Exp))))))(shards(0 \
         1))(children(((Secondary((id \
         2c0d16b7-6046-44b2-b2a0-47042b235a9e)(content(Whitespace\" \
         \"))))(Tile((id \
         6009904c-7221-44bb-b4d4-9ff4d96f8bb4)(label(\"(\"\")\"))(mold((out \
         Pat)(in_(Pat))(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0 1))(children(((Tile((id \
         2defb65f-1029-4faf-9743-d7e4e424533e)(label(acc))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         0c3074f8-ba03-4f4c-b29b-2153560d80b7)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 44))(sort Pat))((shape(Concave \
         44))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         b6ca5b88-1771-4777-943e-62d60b3cfebd)(content(Whitespace\" \
         \"))))(Tile((id \
         50ad1581-24e4-4928-9874-51f724c6d588)(label(w))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children()))))))))(Secondary((id \
         32ee4f44-01ea-4feb-ac29-27f8cd760bcf)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         60216398-56d0-45bb-a92a-714f6325a71d)(content(Whitespace\"\\n\"))))(Tile((id \
         f0eb0153-5896-44b0-be14-e74e926ab839)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         cb90621d-f8d0-45fd-bfdf-74a511ad009c)(content(Whitespace\" \
         \"))))(Tile((id \
         b05c60e0-280f-4137-bd8c-312ad9815438)(label(running))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         651ef5de-3855-43b5-a54e-cf4d9c23057d)(content(Whitespace\" \
         \")))))((Secondary((id \
         e12106c7-4395-412f-b039-3d83ed113111)(content(Whitespace\" \
         \"))))(Tile((id \
         59a32e34-9cad-4134-b3d1-94d77cf1e9d9)(label(acc))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         9b8d3751-d376-4f98-9411-750b0305e870)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         9d7564b1-e483-4c85-a363-72504e7c2ada)(content(Whitespace\"\\n\"))))(Tile((id \
         787fa748-d8c4-4be5-8003-60f33e142e20)(label(running))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         b7a4dff9-415e-41c0-b767-9603343e611b)(content(Whitespace\" \
         \"))))(Tile((id \
         c9d505ec-ba47-433a-9800-e253ed19282e)(label(+))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 28))(sort Exp))((shape(Concave \
         28))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         44dc7e85-5f0a-4488-8f82-276d6c1b484c)(content(Whitespace\" \
         \"))))(Tile((id \
         de11a697-5692-458a-a704-c8fa0a1f5fed)(label(w))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         9e02cc9e-ab1b-4aa3-91c0-22c8ee7715c3)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         2b747c9c-3470-4895-bcaf-0a6d9da09b27)(content(Whitespace\"\\n\"))))(Tile((id \
         c046bf1e-299d-43a5-819d-f3eb025bff24)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         0b2cc538-2050-4502-9f80-2182d31c5d0a)(content(Whitespace\" \
         \"))))(Tile((id \
         f1774f8e-675e-4cc9-bdc2-a627ec7645f0)(label(total))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         1c7584d8-4dbd-48bb-9365-e72ae84c1edf)(content(Whitespace\" \
         \")))))((Secondary((id \
         b59e77a1-bfb3-409a-9376-dfed7224e812)(content(Whitespace\" \
         \"))))(Tile((id \
         2edb9f6d-14d8-4d29-9a36-dd2e7af4726a)(label(fold_left))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         f413177b-fe84-4e60-afe4-79d8316d9f8f)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         32eda9e2-8efa-43b0-a3d9-000a3cf111cd)(label(weekly_amounts))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         e3838fd8-90fa-46a5-9f6d-a9ac51ac1f94)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         1578bc74-3bc5-41f7-af33-a90ae1360e9d)(content(Whitespace\" \
         \"))))(Tile((id \
         28002892-f938-4eae-899c-9c65c4eec43d)(label(sum))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         639e9fbc-8581-4db1-a633-78e146e68cba)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         aa715e58-0ba4-41df-97ec-dd60be467ca9)(content(Whitespace\" \
         \"))))(Tile((id \
         a2f00f62-8c57-405d-ba9f-4723fb1317e5)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         6fce3150-26f2-4717-aefb-6ee92967bfa8)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         0548d0b6-6864-4e32-8458-4f690401257d)(content(Whitespace\"\\n\"))))(Tile((id \
         c23da047-fcb5-476e-ac5a-d57db8cef9d3)(label(total))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         b3c17596-a995-4dd4-9896-dc6b6380e321)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         c552dfe5-fa5e-4573-a7ed-4ea34dd07438)(content(Whitespace\"\\n\"))))(Secondary((id \
         db26e8cd-3743-4744-a0ca-00b78eff054c)(content(Whitespace\"\\n\"))))(Secondary((id \
         bfb73e7b-8946-4e5c-b772-9cb6adba70a8)(content(Comment\"# EXERCISE 1: \
         Step into the map #\"))))(Secondary((id \
         2214f69e-3c65-4c2b-ab17-79219cfa3583)(content(Whitespace\"\\n\"))))(Secondary((id \
         fa113ba7-481f-4f3f-91d4-b574dcf7f047)(content(Comment\"# 1. Add a \
         probe to `weekly_total(shade)` below. #\"))))(Secondary((id \
         cf66da38-cde8-4d11-9d58-593e3820a705)(content(Whitespace\"\\n\"))))(Secondary((id \
         bad89c5b-7b64-4d05-8bf4-bdfba125bbe0)(content(Comment\"#    It \
         returns 4270. How does it get there? #\"))))(Secondary((id \
         79dff003-7f2c-4856-8dbb-f115bfbbf4e1)(content(Whitespace\"\\n\"))))(Secondary((id \
         215f4a6c-e144-477d-b7c1-23fdfb5c90f0)(content(Comment\"# 2. Click the \
         sample and Step Into (Enter). #\"))))(Secondary((id \
         7ffd48e9-00d6-458e-9af1-351e8095e622)(content(Whitespace\"\\n\"))))(Secondary((id \
         b98f09aa-6e3c-423d-9779-2259776476ad)(content(Comment\"# 3. Turn on \
         auto-probe inside `weekly_total`. #\"))))(Secondary((id \
         6579c47a-59f8-4791-ba56-a38753325f9d)(content(Whitespace\"\\n\"))))(Secondary((id \
         6570813f-f968-43b7-b264-28afbb559c9d)(content(Comment\"# 4. The map \
         callback shows each plant's `daily` water #\"))))(Secondary((id \
         a7a993a4-09f9-4e48-b9eb-82440d93224f)(content(Whitespace\"\\n\"))))(Secondary((id \
         7a6de838-f3cf-4389-a42e-4036cb43e10b)(content(Comment\"#    and the \
         `daily * 7` result. In Many mode you see #\"))))(Secondary((id \
         616be4ac-d45f-40c1-90c4-f50acf9bc116)(content(Whitespace\"\\n\"))))(Secondary((id \
         ef3aa60c-c57f-4aa8-a61f-576130048da2)(content(Comment\"#    all 3 \
         transformations side by side: #\"))))(Secondary((id \
         9da3b694-85da-4178-856d-6d22e990715a)(content(Whitespace\"\\n\"))))(Secondary((id \
         13c36a79-0104-4c18-90b0-c45b51b9a20a)(content(Comment\"#    daily: \
         [250, 200, 160] and daily*7: [1750, 1400, 1120] #\"))))(Secondary((id \
         3438d29f-650e-431a-a4c6-69373b9f93bb)(content(Whitespace\"\\n\"))))(Secondary((id \
         0774796c-3d6b-40ee-ba28-3cafc04aaf98)(content(Whitespace\"\\n\"))))(Secondary((id \
         f5a7dadd-8fd4-425c-98b0-a1198401abe0)(content(Comment\"# EXERCISE 2: \
         Now look at the fold #\"))))(Secondary((id \
         d88cc501-51ac-456f-a154-14670416b297)(content(Whitespace\"\\n\"))))(Secondary((id \
         1550f912-e7f1-4686-8c69-3b8e14c48296)(content(Comment\"# 5. Still \
         inside `weekly_total`, look at the fold #\"))))(Secondary((id \
         ddfae94b-8c1b-475e-b7ee-40579eefb29c)(content(Whitespace\"\\n\"))))(Secondary((id \
         c98bd428-bc0b-486f-935e-b86f8214ef76)(content(Comment\"#    \
         callback's samples. In Many mode, `running` shows \
         #\"))))(Secondary((id \
         e27e4b30-6a1d-45ce-8cd5-4b02d5d01848)(content(Whitespace\"\\n\"))))(Secondary((id \
         5f2c2095-ba02-41ff-b694-9b2a3172b43a)(content(Comment\"#    the \
         accumulator: [0, 1750, 3150] and `running + w` #\"))))(Secondary((id \
         147295d8-1368-4108-adb6-394cb83b11fe)(content(Whitespace\"\\n\"))))(Secondary((id \
         436f920b-a638-4cba-8315-a2bbb979e197)(content(Comment\"#    shows it \
         growing: [1750, 3150, 4270]. #\"))))(Secondary((id \
         8736971b-5f64-4114-9a38-9e2c8cdc93b5)(content(Whitespace\"\\n\"))))(Secondary((id \
         2f951a3f-985a-4f59-ad39-2479fa59546b)(content(Comment\"# 6. Use the \
         dynamic cursor bar at the top to navigate #\"))))(Secondary((id \
         0d605ab0-f3e9-45d1-9cb6-ca5ef3634e3b)(content(Whitespace\"\\n\"))))(Secondary((id \
         d71bdb88-86b7-4f6b-8b70-281b901a0834)(content(Comment\"#    back out. \
         Try stepping into `weekly_total(all)` \\226\\128\\148 \
         #\"))))(Secondary((id \
         1b58826c-a6f6-45f0-8b83-d52386685e1c)(content(Whitespace\"\\n\"))))(Secondary((id \
         bb1a898c-fd4f-4e27-9284-929ce7fde4c9)(content(Comment\"#    now there \
         are 5 iterations each. #\"))))(Secondary((id \
         32bcfb31-f466-495d-b37e-faa43031466e)(content(Whitespace\"\\n\"))))(Secondary((id \
         bf70cf38-cd8d-46a5-8f0d-f41eea0d6d5a)(content(Whitespace\"\\n\"))))(Tile((id \
         a722eeeb-038f-4a65-b5b9-e91c574b4108)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         eb3c0340-40d7-42f1-93ba-29ad44b4695f)(content(Whitespace\" \
         \"))))(Tile((id \
         aed91b53-8cfb-487c-9477-a9975cdaae7d)(label(shade))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         fff71cf4-8e5a-47f1-9d3a-f78a4ba9ac41)(content(Whitespace\" \
         \")))))((Secondary((id \
         74a05cf8-c4cd-4945-8a20-6a77cd26705a)(content(Whitespace\" \
         \"))))(Tile((id 0001c0d3-1ab9-458d-a001-479ccd7d06fc)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         d7fba508-43e8-40f3-a803-d415e3969217)(label(fern))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         e2e7862d-38e7-407a-9340-3618aad610af)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         0b50891f-b5b7-4f73-9d28-220a8d07d939)(content(Whitespace\" \
         \"))))(Tile((id \
         0a1664a2-07e9-4665-b8f0-fb0220f31365)(label(lily))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         3a0ff853-181c-4bab-9034-27e23a414764)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         b2cb1de5-0a6c-4187-84b4-b3601e3b2b36)(content(Whitespace\" \
         \"))))(Tile((id \
         6ee717cc-cacf-4107-a4b3-e018b7495beb)(label(daisy))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         1b97b7c9-2b2c-48c1-99a9-1a12e2ff3d43)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         8223d544-85f2-439d-9502-aea251d42312)(content(Whitespace\"\\n\"))))(Tile((id \
         51210f81-fa30-4bca-a6c7-6e2c4e36059a)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         3a3acd75-17c4-481a-97a3-430b963eac67)(content(Whitespace\" \
         \"))))(Tile((id \
         a1130dc2-4437-4b9d-a837-c280868654dd)(label(sun))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         c3b65750-09fa-439b-bd11-721c4fdda52d)(content(Whitespace\" \
         \")))))((Secondary((id \
         edd1ea4f-42f0-4dbd-ba2c-fd6ca30144a6)(content(Whitespace\" \
         \"))))(Tile((id e175c2b9-1f23-422e-8dc6-aadf380ee674)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         c42d6a3a-9cd4-42d0-a5f3-465869558094)(label(orchid))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         e0154d6c-be55-4fae-a6f6-da76c6cccc6c)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         6f092806-b8f7-4faa-8480-d62aee321bb0)(content(Whitespace\" \
         \"))))(Tile((id \
         522637bd-1e01-4c40-ad3b-24671d2877aa)(label(cactus))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         30f57a16-a422-41fd-896a-06eb882b1c87)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         c9673255-24f5-424a-b874-af188d994032)(content(Whitespace\"\\n\"))))(Tile((id \
         4547a4d5-5590-412f-be9b-73ccf29ff653)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         e876e7fa-30eb-4022-8f84-896fae7b0c84)(content(Whitespace\" \
         \"))))(Tile((id \
         c5c338b2-6784-4094-8380-18194e2c855d)(label(all))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         63f04f1c-8fa8-46d3-9e70-1344845c26e8)(content(Whitespace\" \
         \")))))((Secondary((id \
         140e6945-43de-4a3e-b10c-84de6fe6dc3a)(content(Whitespace\" \
         \"))))(Tile((id 1108e0a1-e729-4907-afd2-0f246a64ee8b)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         ce5c34f5-f92e-4f28-a2c4-0ded8ef120a9)(label(fern))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         e210974f-77d5-4c80-989a-08adb7508119)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         995be519-f61c-49ef-9b18-1a3be4a1bcc5)(content(Whitespace\" \
         \"))))(Tile((id \
         7bd7bc85-3de8-47c6-8a16-c7374127a0da)(label(orchid))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         df146c52-ff33-469f-a2d2-b77863d85b15)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         16ccea22-6003-4df6-bd4c-097b928ccf37)(content(Whitespace\" \
         \"))))(Tile((id \
         65d8eee3-5bbf-4a29-a17c-83f59bc36185)(label(cactus))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         756daa81-86b7-4bd0-ae95-95758f6bcbb2)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         acb812fe-4e6d-49be-bb92-862e7edfcfcf)(content(Whitespace\" \
         \"))))(Tile((id \
         86c8a03b-5139-4eb8-a3ad-5441d7b6c6c7)(label(lily))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         57943f77-1070-4b51-8122-cb5534603188)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         4f1284a3-99ee-407c-b41d-a498b8b70d3a)(content(Whitespace\" \
         \"))))(Tile((id \
         5e1dff14-fe07-4ce8-a4ab-36a068512bcd)(label(daisy))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         7d3d377f-7714-4b9d-9d6a-b9bb1906bb0f)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         804011a2-3b42-4b2f-ac7d-75883a9481b7)(content(Whitespace\"\\n\"))))(Secondary((id \
         22728b8e-d091-4547-adaf-79147d16f0f3)(content(Whitespace\"\\n\"))))(Tile((id \
         390a30c5-fdc6-42d0-817a-6cfd6339aa74)(label(weekly_total))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         ed09d8cc-e3a8-4e93-b79f-fa3d1ac7acd7)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         7815117b-d2d7-479d-86fc-d8de6e65b3db)(label(shade))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         8c13698c-fbd8-4fbf-aa7a-3a2faca01dc8)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         0be5b39c-dc4e-42d7-b54e-e3e097b83fa5)(content(Whitespace\"\\n\"))))(Tile((id \
         924d801f-318f-40e0-997c-37ed963e79c6)(label(weekly_total))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         20495ae0-1392-4518-8f5c-8d06f5bb6a4f)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         c0d5654f-de51-4ca8-9aac-cd5e703b0bca)(label(sun))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         9e61253f-75ae-4cdb-9dc3-2752d28ffc78)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         b7406aaf-c074-42f0-bc39-ab3083ad35eb)(content(Whitespace\"\\n\"))))(Tile((id \
         180f6a8c-bfd0-42e3-9519-dfe90b4d40e7)(label(weekly_total))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         d1ed830c-1257-4fb7-b0dc-74d3d147464e)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         87c0132c-be88-4897-9c04-97d9af4b3fa5)(label(all))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         bfd95482-d220-4470-a5db-fc9e254e918b)(content(Whitespace\"\\n\"))))(Secondary((id \
         8a293fb3-3550-41cb-8023-813f9164ac1a)(content(Whitespace\"\\n\"))))(Secondary((id \
         89a56ea4-92bb-47e8-b722-f7ad40185848)(content(Comment\"# END \
         #\"))))(Secondary((id \
         57846a93-0a53-4190-aa3c-78fe845a8651)(content(Whitespace\"\\n\")))))";
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
