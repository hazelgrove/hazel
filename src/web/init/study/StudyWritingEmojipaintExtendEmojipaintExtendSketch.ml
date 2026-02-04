let out : string * Haz3lcore.PersistentSegment.t =
  ( "Study / writing / emojipaint-extend / emojipaint-extend-sketch",
    {
      segment =
        "((Secondary((id \
         148b12fa-43bb-4b17-9d5c-7abd1932360e)(content(Comment\"# EMOJIPAINT \
         EXTENSION TASK                     #\"))))(Secondary((id \
         0082b79f-b46d-445b-a285-f3a3c77f4731)(content(Whitespace\"\\n\"))))(Secondary((id \
         d9ba6219-c484-4ad8-9afd-c3824fbca5d9)(content(Comment\"#                                               \
         #\"))))(Secondary((id \
         19ec009e-6224-46e8-b7c0-5996f20dace9)(content(Whitespace\"\\n\"))))(Secondary((id \
         726694fa-09c7-451a-b287-5c20d5b28d9b)(content(Comment\"# The \
         emojipaint app lets you paint emojis on   #\"))))(Secondary((id \
         b547ab42-65a5-4b00-ae68-d411441fcd8a)(content(Whitespace\"\\n\"))))(Secondary((id \
         9d6a9de9-687a-4053-a1d8-2c7e600d887d)(content(Comment\"# a grid. It \
         already supports painting rows.    #\"))))(Secondary((id \
         96253eac-308c-4d1c-af68-3b33c244d633)(content(Whitespace\"\\n\"))))(Secondary((id \
         f2ff4edf-2c69-4b7d-9485-3565d5e55938)(content(Comment\"#                                               \
         #\"))))(Secondary((id \
         5c74088f-bd4e-4a18-b8f4-c6c6026ce34b)(content(Whitespace\"\\n\"))))(Secondary((id \
         729c4782-2f79-4fd5-9720-046531432ae9)(content(Comment\"# YOUR TASK: \
         Add a PaintCol action that fills   #\"))))(Secondary((id \
         766fa572-e3a9-41e5-8f0a-726ecc6f359b)(content(Whitespace\"\\n\"))))(Secondary((id \
         87b1d87f-b471-491f-a2d3-ce7328826485)(content(Comment\"# an entire \
         column with the current brush.      #\"))))(Secondary((id \
         b0b7cd87-7e7d-4a85-8b53-04694fee2636)(content(Whitespace\"\\n\"))))(Secondary((id \
         bc8562d7-590b-4c08-abec-ac94ff44c16c)(content(Comment\"#                                               \
         #\"))))(Secondary((id \
         8cfa9982-f4e1-4b4a-ae53-3b41ace835ce)(content(Whitespace\"\\n\"))))(Secondary((id \
         a45e4a92-e629-4cc3-a51a-a96ee83cbed2)(content(Comment\"# You need \
         to:                                  #\"))))(Secondary((id \
         232162d8-9756-4477-9dbb-61ef6cc73d30)(content(Whitespace\"\\n\"))))(Secondary((id \
         013baa00-5b91-4525-8899-fd7019f32801)(content(Comment\"#   1. Add \
         PaintCol(Col) to the Action type     #\"))))(Secondary((id \
         de5d627d-6563-49db-8442-738013f51bbc)(content(Whitespace\"\\n\"))))(Secondary((id \
         f62c7fec-d9bc-48c1-8f7a-23c403924495)(content(Comment\"#   2. Add a \
         setCol helper function             #\"))))(Secondary((id \
         677340fa-14f0-4963-830e-a30925d57d56)(content(Whitespace\"\\n\"))))(Secondary((id \
         8b853a47-edf5-4932-b105-c04f49dfe119)(content(Comment\"#   3. Handle \
         PaintCol in the update function   #\"))))(Secondary((id \
         c00067a5-bd35-4506-ae80-8e9bd0fefe2f)(content(Whitespace\"\\n\"))))(Secondary((id \
         e025c264-3b36-4372-abde-a4e467acbd07)(content(Comment\"#                                               \
         #\"))))(Secondary((id \
         bddea4de-c559-4040-8b53-cc68ef227121)(content(Whitespace\"\\n\"))))(Secondary((id \
         f2ecb531-d3d5-42f9-bfd1-05a27c957fe3)(content(Comment\"# Look at how \
         PaintRow is implemented for       #\"))))(Secondary((id \
         acf28193-1d29-4d6e-8af0-08975b808f2a)(content(Whitespace\"\\n\"))))(Secondary((id \
         038d24d4-f056-4aa2-a2c3-63d103646dd5)(content(Comment\"# guidance - \
         PaintCol is similar but vertical.  #\"))))(Secondary((id \
         0c7386cc-3003-4465-879d-16a8767cbf3c)(content(Whitespace\"\\n\"))))(Secondary((id \
         3128ee36-54c0-447d-b17e-61e999d0f7b7)(content(Comment\"#                                               \
         #\"))))(Secondary((id \
         e852d4bd-1676-49fb-b637-3bcb518c2e09)(content(Whitespace\"\\n\"))))(Secondary((id \
         db24efcd-ec33-45d5-ae2d-4fe2cb1b66d2)(content(Comment\"# Tip: Use \
         auto-probe to see how the canvas     #\"))))(Secondary((id \
         e58bd63c-85f0-4015-8462-a7e4a5777e3c)(content(Whitespace\"\\n\"))))(Secondary((id \
         0f5f4ef1-9632-4645-8f43-d144ca92587b)(content(Comment\"# changes \
         after each action.                    #\"))))(Secondary((id \
         d6f1b672-ef55-4b9b-87a0-14f0c1204f3a)(content(Whitespace\"\\n\"))))(Secondary((id \
         c1ea7311-d305-417b-83a2-778bfa927fc7)(content(Whitespace\"\\n\"))))(Tile((id \
         c9a2215a-dbf3-44b2-bf54-279f0e93074a)(label(type = in))(mold((out \
         Exp)(in_(TPat Typ))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         1e63731e-830f-4375-baa2-caa56d6bdbfe)(content(Whitespace\" \
         \"))))(Tile((id \
         7adc77b4-9713-4cf3-8324-99c789edd95c)(label(Emoji))(mold((out \
         TPat)(in_())(nibs(((shape Convex)(sort TPat))((shape Convex)(sort \
         TPat))))))(shards(0))(children())))(Secondary((id \
         620c3d27-2c33-469a-b3a1-5f4343ec3232)(content(Whitespace\" \
         \")))))((Secondary((id \
         c8ae13c3-24ae-422a-9ed3-4b8c8e6a5c1b)(content(Whitespace\" \
         \"))))(Tile((id \
         46db29ee-4029-4d03-9e9d-dbeaae584642)(label(String))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         043167d3-3c3c-4a87-9455-263c17a59fdf)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         1c948eab-28db-46c5-aeee-cdd04e6c7575)(content(Whitespace\"\\n\"))))(Tile((id \
         3259b223-a800-4d31-a2a2-100507e04436)(label(type = in))(mold((out \
         Exp)(in_(TPat Typ))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         76a9d2aa-34af-4a01-84c8-c0b05e51bb8c)(content(Whitespace\" \
         \"))))(Tile((id \
         d9bee2ff-8367-4c50-89f0-9f1528d53490)(label(Canvas))(mold((out \
         TPat)(in_())(nibs(((shape Convex)(sort TPat))((shape Convex)(sort \
         TPat))))))(shards(0))(children())))(Secondary((id \
         80d161d8-433a-4c57-94f6-219edcb715a3)(content(Whitespace\" \
         \")))))((Secondary((id \
         18f4c756-314d-4b0f-bec1-22b9ee76274f)(content(Whitespace\" \
         \"))))(Tile((id fa2ad2ed-ec06-459b-86b3-37464ccfa327)(label([ \
         ]))(mold((out Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape \
         Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
         f4542d17-bf87-4c6e-adbc-183cc5738777)(label([ ]))(mold((out \
         Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0 1))(children(((Tile((id \
         9a8b94ce-8475-4299-9c6d-56d1b0c3ed34)(label(Emoji))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))))))))))))(Secondary((id \
         85fbf8d8-9d71-4685-a06d-2b5c7c4896b7)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         102a5dca-1df8-4d75-b399-b3e1eaa1112e)(content(Whitespace\"\\n\"))))(Tile((id \
         3b1191f4-bf56-449b-aad3-ca7bd4069aa7)(label(type = in))(mold((out \
         Exp)(in_(TPat Typ))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         81054b76-04c0-445f-877a-b5c23ca3c6bf)(content(Whitespace\" \
         \"))))(Tile((id \
         bb4dd889-0c60-4c87-9b07-c9af94296c24)(label(Row))(mold((out \
         TPat)(in_())(nibs(((shape Convex)(sort TPat))((shape Convex)(sort \
         TPat))))))(shards(0))(children())))(Secondary((id \
         984bc4e6-eb52-45f6-bacc-7384d8495553)(content(Whitespace\" \
         \")))))((Secondary((id \
         e68023c3-f0f6-4b74-83c5-36cfe53d1a0c)(content(Whitespace\" \
         \"))))(Tile((id \
         f53f6d98-c0ce-4c51-ab55-fe55eb17742a)(label(Int))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         1b887983-1bcd-45a8-a804-8c1206374f80)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         efac9504-c813-47c8-a7c5-4865d5c9e7ea)(content(Whitespace\"\\n\"))))(Tile((id \
         67985002-4865-43c7-b5ae-d34a4bd8b5bf)(label(type = in))(mold((out \
         Exp)(in_(TPat Typ))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         5a3b6c3f-fd75-4516-bff4-1c53c2ad71fb)(content(Whitespace\" \
         \"))))(Tile((id \
         2e477821-50cc-4b38-8954-9a3b296530bd)(label(Col))(mold((out \
         TPat)(in_())(nibs(((shape Convex)(sort TPat))((shape Convex)(sort \
         TPat))))))(shards(0))(children())))(Secondary((id \
         67498e32-1120-47d3-a00c-204cf1d0ec67)(content(Whitespace\" \
         \")))))((Secondary((id \
         f4ffee2a-70a2-4691-9f8b-4c4fbfc67f5e)(content(Whitespace\" \
         \"))))(Tile((id \
         efd37283-1141-455f-8d26-970c1f216146)(label(Int))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         7ad6b622-b5ab-47c8-ae9e-8b37701a35c8)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         4e28815b-eeec-4d21-ad77-3409c61c55d4)(content(Whitespace\"\\n\"))))(Secondary((id \
         375932dc-5f0f-4daa-abca-bd07087ddbc6)(content(Whitespace\"\\n\"))))(Tile((id \
         46433ad5-83ed-4fb3-abc7-519b6ba1afdd)(label(type = in))(mold((out \
         Exp)(in_(TPat Typ))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         4630ba82-349f-40bf-9582-bf9f35d77294)(content(Whitespace\" \
         \"))))(Tile((id \
         7e8c9815-1683-4085-b489-905e656c676c)(label(Model))(mold((out \
         TPat)(in_())(nibs(((shape Convex)(sort TPat))((shape Convex)(sort \
         TPat))))))(shards(0))(children())))(Secondary((id \
         5d6e8818-01c3-4bad-9187-ec7f6a7072fb)(content(Whitespace\" \
         \")))))((Secondary((id \
         77b4fb38-4020-4f1f-9047-0f2564a46435)(content(Whitespace\" \
         \"))))(Tile((id \
         1978a3ae-a52e-4644-a388-0821c0150dd9)(label(\"(\"\")\"))(mold((out \
         Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0 1))(children(((Secondary((id \
         f419f6e0-bb06-404d-8884-23e78fe0ee91)(content(Whitespace\"\\n\"))))(Tile((id \
         ec617c39-f084-46d3-93cf-9a620d4e0d7b)(label(canvas))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         e7ac47fb-0192-4835-8e08-22d1004c7981)(content(Whitespace\" \
         \"))))(Tile((id \
         f37cb19f-506e-4c3f-b785-516cda289f6a)(label(=))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 39))(sort Typ))((shape(Concave \
         39))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         c874162b-fb45-46e2-8de5-cfa7c448f532)(content(Whitespace\" \
         \"))))(Tile((id \
         59e0fb60-e65e-4748-b580-c32bee1cdc56)(label(Canvas))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         3b0a772f-4586-4fab-830b-3e316fe4fb4a)(label(,))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 44))(sort Typ))((shape(Concave \
         44))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         0154df4d-53b9-4d90-97c3-d571d3e29f34)(content(Whitespace\"\\n\"))))(Tile((id \
         44eb5c01-5959-4e8f-896e-4132bd386add)(label(brush))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         fcf6da07-c122-4d13-ae0c-ab133c31f8f2)(content(Whitespace\" \
         \"))))(Tile((id \
         793f79ea-2259-4216-b4cc-b476f06bbeb7)(label(=))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 39))(sort Typ))((shape(Concave \
         39))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         a038a5ea-a1b8-4a61-800d-b9bf30eb2b1f)(content(Whitespace\" \
         \"))))(Tile((id \
         1df23901-07a0-439e-822c-34e2d60f485c)(label(Emoji))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         4c01ce26-9e82-45b2-b37d-1fcc273b100e)(label(,))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 44))(sort Typ))((shape(Concave \
         44))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         c138b67c-fed6-4d9c-8b74-ccbd96d37823)(content(Whitespace\"\\n\"))))(Tile((id \
         d2c3512d-3493-436c-986d-5b810946392b)(label(palette))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         cd9339d7-2ca5-477a-80d7-37052e48184b)(content(Whitespace\" \
         \"))))(Tile((id \
         c53a4401-d324-4f20-94fd-fb515ddf6c91)(label(=))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 39))(sort Typ))((shape(Concave \
         39))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         beff2e48-f850-4969-87b3-c8aee840c19f)(content(Whitespace\" \
         \"))))(Tile((id f9d600a5-2e20-41cd-bd2a-4c1a349306b5)(label([ \
         ]))(mold((out Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape \
         Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
         c7147092-0672-4527-8018-5ac8f57f6069)(label(Emoji))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children()))))))))(Secondary((id \
         7d0f8705-9946-48e8-9f9b-b7fbd391ce73)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         a58dcbaf-1849-40f7-8d59-c28373cc37dd)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         560fdfba-0db3-4614-8b28-7987f10da3b5)(content(Whitespace\"\\n\"))))(Secondary((id \
         02a0bfd6-c2e5-425d-ad1f-c92dbcc7c4dc)(content(Whitespace\"\\n\"))))(Tile((id \
         7d4d996d-e4ce-46ed-9d95-ea39a25d0621)(label(type = in))(mold((out \
         Exp)(in_(TPat Typ))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         dddf6df8-54d4-46e5-a03c-572f79d5151d)(content(Whitespace\" \
         \"))))(Tile((id \
         ad0b1825-c262-4149-8c30-379add2837c3)(label(Action))(mold((out \
         TPat)(in_())(nibs(((shape Convex)(sort TPat))((shape Convex)(sort \
         TPat))))))(shards(0))(children())))(Secondary((id \
         1f1ed23c-721e-40c6-8e21-f46185573113)(content(Whitespace\" \
         \")))))((Secondary((id \
         0093118d-c767-4f3e-b1d5-94368efa1201)(content(Whitespace\"\\n\"))))(Tile((id \
         4ba8e99d-c304-4492-a051-aba7efb1f3e9)(label(+))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape(Concave 33))(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         79b9a9cd-e439-4b94-9a87-7115097fece1)(content(Whitespace\" \
         \"))))(Tile((id \
         eabd68b2-0531-4f5f-9461-25a6c9e652f0)(label(SetBrush))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         4877be1b-cf40-4fac-927f-fbf850f1d128)(label(\"(\"\")\"))(mold((out \
         Typ)(in_(Typ))(nibs(((shape(Concave 11))(sort Typ))((shape \
         Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
         17dcf412-6b73-4212-a0f4-3c88728f6c66)(label(Int))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children()))))))))(Secondary((id \
         e0d6c7f9-43c9-482e-b38d-dd3ac4b63759)(content(Whitespace\"\\n\"))))(Tile((id \
         074aecfd-9a80-4f4b-b63f-db5eac4d8319)(label(+))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 12))(sort Typ))((shape(Concave \
         12))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         8ee779b8-68c1-405d-878e-f727f0a57e69)(content(Whitespace\" \
         \"))))(Tile((id \
         fc66e023-ebb5-4995-ab57-b8f5f75716f0)(label(PaintCell))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         4653124c-3752-4237-8d19-c9689a5fc074)(label(\"(\"\")\"))(mold((out \
         Typ)(in_(Typ))(nibs(((shape(Concave 11))(sort Typ))((shape \
         Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
         e4ef175f-9495-4da1-9dd4-82c181f89a0f)(label(Row))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         3a7a9592-ca76-4af1-8d6b-b5a60c2125e9)(label(,))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 44))(sort Typ))((shape(Concave \
         44))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         022c3f25-36b8-4916-97e9-f0376cfaec42)(content(Whitespace\" \
         \"))))(Tile((id \
         d7232d57-7afd-4588-b196-5c1b0d4f5030)(label(Col))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children()))))))))(Secondary((id \
         432f4e75-6186-4715-a042-dba6318e0c14)(content(Whitespace\"\\n\"))))(Tile((id \
         a41a572a-4b23-44e7-86e0-ff3f3353f1ee)(label(+))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 12))(sort Typ))((shape(Concave \
         12))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         f5404da7-4c17-48bd-b93f-6c62cb6a80aa)(content(Whitespace\" \
         \"))))(Tile((id \
         ee41cb27-07c6-4af2-9e57-8940bfcaba53)(label(ClearCell))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         8eda91aa-fd8a-49b5-ac83-e8e98bce61c6)(label(\"(\"\")\"))(mold((out \
         Typ)(in_(Typ))(nibs(((shape(Concave 11))(sort Typ))((shape \
         Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
         83ab402d-0ec9-4627-8a0a-d80bd1d3deba)(label(Row))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         4d7e9032-c58b-4b12-8e50-b992106d4654)(label(,))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 44))(sort Typ))((shape(Concave \
         44))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         37479e2d-17d5-4630-a754-b6ffdee99f04)(content(Whitespace\" \
         \"))))(Tile((id \
         9a91f938-fae1-464d-8d21-59d40c2cbc07)(label(Col))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children()))))))))(Secondary((id \
         876a3096-f9ea-4b38-a630-46a9a5b64de3)(content(Whitespace\"\\n\"))))(Tile((id \
         36222e4e-95e6-4b44-be08-1eb2b47f9b30)(label(+))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 12))(sort Typ))((shape(Concave \
         12))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         daddad40-042a-45c0-8d32-138e3b7912a6)(content(Whitespace\" \
         \"))))(Tile((id \
         86609bea-e1f5-436c-88b2-842cca13fe1c)(label(ClearGrid))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         390fdc3d-393a-4337-a645-3ae55c925012)(content(Whitespace\"\\n\"))))(Tile((id \
         1405a91b-163d-4166-888d-438e3d2e08e9)(label(+))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 12))(sort Typ))((shape(Concave \
         12))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         1ab44dc1-bc95-4c41-93c3-133a6de2dcf0)(content(Whitespace\" \
         \"))))(Tile((id \
         80758e27-7ae9-45da-a894-199941196820)(label(PaintRow))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         b51e651a-a7a4-434c-95da-a49d6574a150)(label(\"(\"\")\"))(mold((out \
         Typ)(in_(Typ))(nibs(((shape(Concave 11))(sort Typ))((shape \
         Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
         da3dc1c6-b982-422e-a929-042eae1a928b)(label(Row))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children()))))))))(Secondary((id \
         66b9cd25-f000-4b6a-b4fa-c833c7337199)(content(Whitespace\"\\n\"))))(Secondary((id \
         3de759e2-2756-407a-8e5f-51a3e6efb65c)(content(Comment\"# TODO: Add \
         PaintCol(Col) here #\"))))(Secondary((id \
         7193381f-c3d6-46f5-a0c1-536604e3e8ac)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         07948c9c-ddb2-4638-9495-7726dd8ded5d)(content(Whitespace\"\\n\"))))(Secondary((id \
         725b6c17-33c7-4ff1-ab86-a8e799485ae9)(content(Whitespace\"\\n\"))))(Tile((id \
         b5f53129-58eb-4689-bf1c-e0bc179328c2)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         65a3bf8e-38cb-4300-959a-c47db8cb0eba)(content(Whitespace\" \
         \"))))(Tile((id \
         926f1880-74c5-42af-82fb-1323769a4ccb)(label(init))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         e0921e14-3990-4388-920a-fccaf6337356)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 24))(sort Pat))((shape(Concave \
         24))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         a49c04cf-c120-4b40-b59f-4e0d4ae7a61b)(content(Whitespace\" \
         \"))))(Tile((id \
         4fa92d34-59e5-4378-bab0-69ecf66e7fbb)(label(Model))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         445f9381-41ac-452c-9a45-ebce17c319b5)(content(Whitespace\" \
         \")))))((Secondary((id \
         76335fb6-294f-42f9-8143-0e92d0570dc9)(content(Whitespace\" \
         \"))))(Tile((id \
         c5d7cf18-c3f7-4428-99ef-b27ff80d5937)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         24963380-7b29-4dd2-be6c-c2beb06ba7fb)(content(Whitespace\"\\n\"))))(Tile((id \
         737097d0-5334-4665-8f75-f4ded8b6123c)(label(canvas))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         651f1686-5751-4b48-80e5-356dedecc15d)(content(Whitespace\" \
         \"))))(Tile((id \
         44a17f57-e2ee-4e1f-95dc-0f03c3f021e8)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         7e1a6060-e3be-4cc7-ac1b-00ed4545acc8)(content(Whitespace\" \
         \"))))(Tile((id 79d28c9f-fad6-4347-9cc5-83e7d0957dbc)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         ba3ca219-76bf-4e40-a87d-b580ef1867ef)(content(Whitespace\"\\n\"))))(Tile((id \
         87772cc5-8625-4945-a7c4-ebbfc24bb8cb)(label([ ]))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         2850f855-6421-4868-9f58-9685d3abe00b)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         57b5d7a0-2c51-448b-bc72-9bf57f9b3578)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         173ed6e0-46ac-4ecf-a8b7-24f4b7a6bbf9)(content(Whitespace\" \
         \"))))(Tile((id \
         d3962c5b-d054-430f-96ad-ddd39d07df09)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         3349e9f3-e5f5-4b71-b822-ffe6183ae4ea)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         99d6b0df-caef-4a84-9cbf-0c81960b044f)(content(Whitespace\" \
         \"))))(Tile((id \
         2192ced2-15be-40c3-bf66-abc8f119d9cf)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         cc8f561c-0e80-4c1f-a9be-e7f018c2cc6e)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         08274146-ae30-4bff-91a8-f79ff65050b4)(content(Whitespace\"\\n\"))))(Tile((id \
         988f1df3-8ac6-4f88-86cb-ff31ba13ec97)(label([ ]))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         050f0d31-9273-4165-b788-304e5d051c39)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         feabf37a-b124-40e7-9d1e-acafe159c029)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         d1e24763-2e91-4ae2-af10-50bd9b3c7d9d)(content(Whitespace\" \
         \"))))(Tile((id \
         241d228d-f2d5-4c0e-a665-ce206960bd93)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         d8fe03cc-f385-4090-8355-3a3d7b5cc61d)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         759368fb-934b-4e49-874c-544856be26fe)(content(Whitespace\" \
         \"))))(Tile((id \
         4c11a2a1-5a41-4b45-821c-d39a06c7fb1f)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         bb48b95a-15df-46c3-9320-ba26566fd578)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         22f5dd0f-0e2d-46f3-ac31-98343a2cc45b)(content(Whitespace\"\\n\"))))(Tile((id \
         1cdbcaf1-9297-4f19-85e3-3e28486b6cba)(label([ ]))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         17209693-4e9b-4c0c-be56-dc3a0adf190f)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         7f595156-fdfc-4ed8-a8c0-5761721aa473)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         64b26423-aa96-4b1c-95da-2f8fb3868576)(content(Whitespace\" \
         \"))))(Tile((id \
         31a082e2-c0da-4bff-8573-f589eed46362)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         3f300676-6d72-4d02-b6b6-2446ed20ef30)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         2ecc127a-ea2f-4de1-8db5-56d5ebec471a)(content(Whitespace\" \
         \"))))(Tile((id \
         41df45e0-0ae4-45e5-99ae-5704eb607621)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         3643d7b1-d9a5-412f-a95e-f86a9751f4cc)(content(Whitespace\"\\n\")))))))))(Tile((id \
         f72863c2-e915-40ac-ab5e-19dd81724231)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         035f622d-4775-4442-8c4c-9d4268a694d9)(content(Whitespace\"\\n\"))))(Tile((id \
         69b1bcea-6bbd-4bfb-bbcf-68a3a5aec870)(label(brush))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         5912569b-8939-4670-863d-6715682eb923)(content(Whitespace\" \
         \"))))(Tile((id \
         13162f95-955e-4e33-826a-c1d840cd73e3)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         8f61a5a5-844e-44ca-8cd2-b3c61f97fb2b)(content(Whitespace\" \
         \"))))(Tile((id \
         7727da18-4837-4e35-bd5a-b149f8a56505)(label(\"\\\"\\240\\159\\142\\168\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         1b38998b-de85-44d2-b0de-1035f0f17bcc)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         49692085-7cc3-4e75-a2a9-77ffada6d3b6)(content(Whitespace\"\\n\"))))(Tile((id \
         eaa53771-b37d-408d-b5a0-20833648d60f)(label(palette))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         2bcb502d-d6b3-4834-98e4-1fe89e160c2d)(content(Whitespace\" \
         \"))))(Tile((id \
         9a2f7fee-2040-420a-8d53-d15d1d6a9486)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         2a1aaa1c-1a7b-4679-a6a6-7df1a98a6e76)(content(Whitespace\" \
         \"))))(Tile((id 3201198d-7c49-49d4-af60-b1a36691b543)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         d21de7a2-e1d4-4e73-a342-de5d645ab732)(label(\"\\\"\\240\\159\\142\\168\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         bb0ea79d-e966-49fa-8d55-fe76b0f49ac0)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         431520fc-1982-4d8b-b2ab-f41628d2470e)(content(Whitespace\" \
         \"))))(Tile((id \
         015fe28b-0aa3-4181-ae0f-cf0ec604d013)(label(\"\\\"\\240\\159\\140\\159\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         1447e926-3bf0-4a03-bc5c-92d081465876)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         a3bdb5a9-ce46-4530-bd6f-4c03947442a1)(content(Whitespace\" \
         \"))))(Tile((id \
         46f14484-25cb-48f3-ba63-ee9292154b9b)(label(\"\\\"\\240\\159\\146\\156\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         f4a8da4c-f5c2-4605-9fe0-48a6f2458947)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         104be664-7c56-4c4d-8219-273926a7a2f5)(content(Whitespace\" \
         \"))))(Tile((id \
         d179d41a-6946-46a2-89d3-cabbf007ad0e)(label(\"\\\"\\240\\159\\148\\165\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         27793523-bcff-46e4-8cbd-0583320b9696)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         6a234a05-b7fa-458d-82cb-5e89ba3a9af8)(content(Whitespace\" \
         \"))))(Tile((id \
         dd0bb122-d32d-48ec-8053-00d10279b41c)(label(\"\\\"\\240\\159\\140\\138\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         0104cbba-3e3a-4f4e-87a1-ef2c6c8e7fd8)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         3cd26bf6-0d51-4322-a463-d206b03930a3)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         c204ae27-6935-49e1-8be3-256de912a850)(content(Whitespace\"\\n\"))))(Secondary((id \
         65721e25-653d-4e0e-907c-f7cb2445c475)(content(Whitespace\"\\n\"))))(Tile((id \
         d9fa5d22-c545-47ab-96fb-5d780013d45d)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         307c368a-b9a3-4dd5-8a39-65fc3f15e2d3)(content(Whitespace\" \
         \"))))(Tile((id \
         90b59318-7a56-4051-b07d-77445c948f45)(label(setCell))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         c6e66d78-e9d0-4f6c-870c-bd244ef71bd2)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 24))(sort Pat))((shape(Concave \
         24))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         2bd24976-e896-438f-83f8-24950f53f3d2)(content(Whitespace\" \
         \"))))(Tile((id \
         c325b2f1-a911-4dee-bae0-96a8bbc7385d)(label(\"(\"\")\"))(mold((out \
         Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0 1))(children(((Tile((id \
         89803c96-6195-4ef7-ad85-5dd6e9ba07c4)(label(Canvas))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         ea210cad-5f62-41cb-a2c6-909f2a45d391)(label(,))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 44))(sort Typ))((shape(Concave \
         44))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         6444729f-7eac-4eb2-a85b-2b135a05d939)(content(Whitespace\" \
         \"))))(Tile((id \
         f592885f-de1c-4ac5-bfb5-ff94fc4f33b9)(label(Row))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         1937004c-9253-44a7-b589-b536975fa32f)(label(,))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 44))(sort Typ))((shape(Concave \
         44))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         2176cd9c-c90c-4467-8b29-bedcee97dda8)(content(Whitespace\" \
         \"))))(Tile((id \
         df027a2c-86b1-43f5-803e-dd3cc984ba4d)(label(Col))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         2726209c-dcac-4a6c-8a94-fe1f0d582c56)(label(,))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 44))(sort Typ))((shape(Concave \
         44))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         ab04d848-f7a3-4199-9301-48d89f328037)(content(Whitespace\" \
         \"))))(Tile((id \
         f41912a5-5719-4701-93b9-51a71a37bb15)(label(Emoji))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children()))))))))(Secondary((id \
         0e3520ac-ee59-4424-ad3f-b1a3d98c2dd4)(content(Whitespace\" \
         \"))))(Tile((id \
         70ff67b0-2d8e-4b45-bdbc-8de3172cc94c)(label(->))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 13))(sort Typ))((shape(Concave \
         13))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         120cadbf-627a-4271-a611-7b02cfc9b53f)(content(Whitespace\" \
         \"))))(Tile((id \
         0fd64843-12c8-4544-8047-b794cb308f37)(label(Canvas))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         1bb7050a-f22b-4580-be6e-47d1534b677c)(content(Whitespace\" \
         \")))))((Secondary((id \
         fe7cbccd-e466-4531-8e82-e731ed3fff51)(content(Whitespace\"\\n\"))))(Tile((id \
         d746a118-69dd-426a-8cc7-d1d13bbd2fa1)(label(fun ->))(mold((out \
         Exp)(in_(Pat))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         37))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         e2ea29c9-6bb1-4a8c-8d79-d352d5907f09)(content(Whitespace\" \
         \"))))(Tile((id \
         4530e5d3-696e-4151-9d71-95cf573ee718)(label(canvas))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         4aa0934d-5d0b-4799-95a8-b8b989707f10)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 44))(sort Pat))((shape(Concave \
         44))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         b22734c2-ce87-4f06-a111-a191463f50db)(content(Whitespace\" \
         \"))))(Tile((id \
         01514b6f-802c-4f94-9083-21973d2e272f)(label(row))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         9f5e48c2-7ccb-4032-9f10-7f655b2138cf)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 44))(sort Pat))((shape(Concave \
         44))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         fd810a87-1f9d-4016-bb22-996335cb3f6d)(content(Whitespace\" \
         \"))))(Tile((id \
         aeb0d26b-d4c8-4248-b1e3-6419e043c190)(label(col))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         2dd0f88e-bc1b-41c1-bfbc-10d76dd3d99f)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 44))(sort Pat))((shape(Concave \
         44))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         65cdb138-a411-4fca-b7f8-c9df6e4bdb17)(content(Whitespace\" \
         \"))))(Tile((id \
         95cab531-4dfc-4984-857b-9213aaab9b23)(label(emoji))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         f04c7729-197d-4bb1-a96d-23bbc5fa8cf2)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         12d9384d-d71e-4b87-aa89-7cf64769525b)(content(Whitespace\"\\n\"))))(Tile((id \
         38d8c590-42a1-4357-853b-f304ae4d9ad7)(label(mapi))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         bc1552fa-c5b7-4065-ae55-509579d396df)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         04c7689b-269a-493b-8d3b-288b4870b48f)(label(canvas))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         15d9a5d6-85cc-4196-bb99-59b72e10fb55)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         804dd2a8-ae04-45aa-9214-3943dfc34f0f)(content(Whitespace\" \
         \"))))(Tile((id b4f4f16c-2917-4e0a-ba3f-8d8d12bd50cd)(label(fun \
         ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 37))(sort Exp))))))(shards(0 \
         1))(children(((Secondary((id \
         1a4c5618-ebf6-4e7e-86c2-1c70388bccab)(content(Whitespace\" \
         \"))))(Tile((id \
         36f6401a-d276-4c7d-8e0b-96740942d2a9)(label(\"(\"\")\"))(mold((out \
         Pat)(in_(Pat))(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0 1))(children(((Tile((id \
         5facc88b-1e07-469d-b56b-1b6fc8268edf)(label(i))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         f508c405-7dc0-4fb7-ac62-f54a1256b8f0)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 44))(sort Pat))((shape(Concave \
         44))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         d243ca65-4887-4862-9b76-eae10f533941)(content(Whitespace\" \
         \"))))(Tile((id \
         bcc750ea-04e7-4c70-9303-bb51380e9b9c)(label(r))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children()))))))))(Secondary((id \
         ef8f7feb-60b9-4c1b-ba96-4a976e71f5fe)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         cf93ada6-4fb9-450a-b52a-007d621c587e)(content(Whitespace\"\\n\"))))(Tile((id \
         1efba796-c0be-486c-a774-771efb6893eb)(label(if then else))(mold((out \
         Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         36))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         4527e324-a4d7-4364-96fb-e3ceb8553b81)(content(Whitespace\" \
         \"))))(Tile((id \
         850238b9-3f31-479f-9896-72ec759fd466)(label(i))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         f517e113-ee52-4465-b3b4-f3bb130f8d0e)(content(Whitespace\" \
         \"))))(Tile((id \
         7ecd6094-7e76-4bcc-9a13-18e3b818aadb)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         e2c9c8f1-ef69-41ff-9db4-b3d21a98c6a3)(content(Whitespace\" \
         \"))))(Tile((id \
         591b30c1-8a7e-4ac5-bd2e-83ddb36021db)(label(row))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         eadc8fec-893a-41c3-b358-585c5a19b0d4)(content(Whitespace\"\\n\")))))((Secondary((id \
         86b65ad7-eb3b-450e-bdf6-e95e7a4fac47)(content(Whitespace\" \
         \"))))(Tile((id \
         ee41f691-0ea7-4550-ba3b-41019375b04f)(label(mapi))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         e02c9538-612e-4c47-ba33-2da1a9964525)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         5e7cd5df-1ea7-4921-ac0e-b92da44d3f68)(label(r))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         1198939a-fdda-4561-ad9e-6b7bfb3f764a)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         5c482a51-0dd7-4abb-91fc-701ee6881a82)(content(Whitespace\" \
         \"))))(Tile((id a489b898-8d9e-4ca7-8bb7-b7194d8c5d45)(label(fun \
         ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 37))(sort Exp))))))(shards(0 \
         1))(children(((Secondary((id \
         b9affd7d-8419-457f-ba90-68a84b894211)(content(Whitespace\" \
         \"))))(Tile((id \
         0336d7a9-64dc-472b-acd8-dc55d8f5582c)(label(\"(\"\")\"))(mold((out \
         Pat)(in_(Pat))(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0 1))(children(((Tile((id \
         cefc6e16-1d65-4863-badf-c22ed8c283e0)(label(j))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         7e946c45-20c1-4936-85ca-a1b416711a9d)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 44))(sort Pat))((shape(Concave \
         44))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         57802e7c-da14-4da4-8c61-f6a70aac60e5)(content(Whitespace\" \
         \"))))(Tile((id \
         e8abf654-8980-452e-a9a7-3584d288efdd)(label(c))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children()))))))))(Secondary((id \
         f75791ec-3bf2-4bf8-ad39-ffbc52d0335c)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         bf72098b-7e07-47ef-8145-b6e977613771)(content(Whitespace\" \
         \"))))(Tile((id 492ede7c-7c8b-4dd6-8f40-1bd15195dd00)(label(if then \
         else))(mold((out Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 36))(sort Exp))))))(shards(0 1 \
         2))(children(((Secondary((id \
         fc759313-aeda-44b7-a1cb-38e64af006b4)(content(Whitespace\" \
         \"))))(Tile((id \
         65c4b900-453a-471e-b7ea-f0350d36438a)(label(j))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         26801b2b-d477-4079-b3b0-519df709855e)(content(Whitespace\" \
         \"))))(Tile((id \
         1947a36d-38a9-450c-85b0-582e67e5cb5f)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         ff0fa767-a91b-49ea-95bd-be2c0cf39d9f)(content(Whitespace\" \
         \"))))(Tile((id \
         1915f472-b975-4508-b86b-bfb7f1af8dc1)(label(col))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         38f9f5bc-1102-4fa1-b489-28230b38b310)(content(Whitespace\" \
         \")))))((Secondary((id \
         f6890a49-6257-4321-a0e2-b891e502338f)(content(Whitespace\" \
         \"))))(Tile((id \
         0878d2d7-c299-4ddc-8503-7712126b418d)(label(emoji))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         8c9f87fc-e087-497b-a4a7-fdb09a7ce4b4)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         5cf57a52-a838-495c-9edd-ea2cf5bbfe88)(content(Whitespace\" \
         \"))))(Tile((id \
         c9afcbb8-a27c-407e-9510-7eca331baae0)(label(c))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         1217855e-e047-403d-afd3-ee05602e1535)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         6ae8078e-ff52-41ee-a56e-6ead7054bf22)(content(Whitespace\" \
         \"))))(Tile((id \
         615e2ad8-6258-4db0-aafa-c29f1248320d)(label(r))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         16442b50-388c-4c17-9202-815a4cd16802)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         3fe3d589-9683-4dde-ab1c-b5d526267775)(content(Whitespace\"\\n\"))))(Secondary((id \
         f6be93cd-b92a-43e7-ba48-e2474aaeee98)(content(Whitespace\"\\n\"))))(Tile((id \
         8e4a3f45-a19a-40dd-8503-a914dec59634)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         21545e19-27d7-4216-a95a-20e173302239)(content(Whitespace\" \
         \"))))(Tile((id \
         35baffa9-2a8b-4bc4-8550-2b8d11da363f)(label(setRow))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         2648a72a-5bcc-4303-b81f-dda695a99d54)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 24))(sort Pat))((shape(Concave \
         24))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         0f730268-b706-4f8f-8584-85e66518d873)(content(Whitespace\" \
         \"))))(Tile((id \
         c7ef95e2-1dcc-47a4-bdf9-c153c52be2ff)(label(\"(\"\")\"))(mold((out \
         Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0 1))(children(((Tile((id \
         dfc39b51-cb9c-4cc4-b0c2-ab70e435f724)(label(Canvas))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         e068cdfe-5e13-4e05-bf1e-ba55598e1a67)(label(,))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 44))(sort Typ))((shape(Concave \
         44))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         825dee82-47ba-41d9-ac21-849bfffe8b99)(content(Whitespace\" \
         \"))))(Tile((id \
         86f7ffa6-25f5-4fc4-a8f2-f58b3ac9e1b1)(label(Row))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         89cd6bc9-6312-4acb-8d9f-41409a645117)(label(,))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 44))(sort Typ))((shape(Concave \
         44))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         28a2681c-2a36-4aa2-bb54-7abf1f90f21c)(content(Whitespace\" \
         \"))))(Tile((id \
         87231764-bd17-405c-9ba6-1b8ec89e02d4)(label(Emoji))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children()))))))))(Secondary((id \
         6d1bec98-1c73-4636-ba37-b4fb0ab759e0)(content(Whitespace\" \
         \"))))(Tile((id \
         4c8b0d79-c985-4379-bcd2-b2e452506190)(label(->))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 13))(sort Typ))((shape(Concave \
         13))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         dc678cc4-2150-4bc3-87d5-be076448cfe7)(content(Whitespace\" \
         \"))))(Tile((id \
         b1351c33-02a3-4fed-ab82-8491456038fc)(label(Canvas))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         268e3392-a810-41c2-b2b7-adf3588640ea)(content(Whitespace\" \
         \")))))((Secondary((id \
         70c481b1-f933-47c0-b6f4-5eecb3f54d27)(content(Whitespace\"\\n\"))))(Tile((id \
         bc714c84-1ab0-436c-a7ec-9d1514e1e96c)(label(fun ->))(mold((out \
         Exp)(in_(Pat))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         37))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         d39f3897-a368-4cd7-89d2-25a16283e78e)(content(Whitespace\" \
         \"))))(Tile((id \
         a1ca5a95-5ed9-4270-b465-d48c303c4fe2)(label(canvas))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         e5ce522f-e1c1-4e17-8ad5-c01dccb8e3a1)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 44))(sort Pat))((shape(Concave \
         44))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         20a0d043-c002-41c0-8f3c-25a25332fe9a)(content(Whitespace\" \
         \"))))(Tile((id \
         705dfa5a-20bb-443f-b925-2946ab4d43da)(label(targetRow))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         3d90e349-8b8d-4df1-9cc1-231ebe813431)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 44))(sort Pat))((shape(Concave \
         44))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         1a70b6d8-1b26-4c70-9dd4-5186e43875b7)(content(Whitespace\" \
         \"))))(Tile((id \
         66cd83ab-6e1a-4e57-b2ee-1b9d6e9e70bb)(label(emoji))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         30a6ea79-b0e8-46f8-8b39-52d2f8844647)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         83a12937-dadb-42f9-b219-fda2a9e805f0)(content(Whitespace\"\\n\"))))(Tile((id \
         55488cf7-84bf-4be1-a12d-427587c394e6)(label(mapi))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         ecbd2ee1-23dc-4b2f-9843-2cc2aa07d931)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         fc0a6eb4-5e58-4734-a3d3-9a07aef1238b)(label(canvas))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         62d3d283-413d-44ba-8b4f-c7841cfb3e3f)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         c6091c24-a69a-40cf-851b-d4f34b38fa76)(content(Whitespace\" \
         \"))))(Tile((id dee6c272-408e-4bb8-9734-b67f171b3394)(label(fun \
         ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 37))(sort Exp))))))(shards(0 \
         1))(children(((Secondary((id \
         0e3b94d0-db6b-48b6-929a-9292c66dc89b)(content(Whitespace\" \
         \"))))(Tile((id \
         0858e84d-52a0-49dd-910e-a488ed78c3a1)(label(\"(\"\")\"))(mold((out \
         Pat)(in_(Pat))(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0 1))(children(((Tile((id \
         5162fca0-cdfd-486b-992a-73d510d60add)(label(i))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         5b012723-4e9f-4f57-8981-078cb4369c98)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 44))(sort Pat))((shape(Concave \
         44))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         dbfba7ab-c95a-47a2-ac88-c551417f1413)(content(Whitespace\" \
         \"))))(Tile((id \
         27574824-d89b-4003-af7a-7a5afcb44df5)(label(row))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children()))))))))(Secondary((id \
         752f2b5d-1b42-4aa6-be7f-4ece01d29e67)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         a9f62348-e0b1-41fd-8408-6629cc7ad0b9)(content(Whitespace\"\\n\"))))(Tile((id \
         34be161a-dd93-44e5-b3d2-8fca67e70727)(label(if then else))(mold((out \
         Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         36))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         64d250bd-7414-4f67-abaf-5abc8c01365e)(content(Whitespace\" \
         \"))))(Tile((id \
         a4f6118a-758e-4147-9573-29c0208b1d61)(label(i))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         65dfd61e-15f6-4bb6-8f7e-2b27cc47be49)(content(Whitespace\" \
         \"))))(Tile((id \
         658c4b5d-4938-4678-a47a-0f3ac57073ef)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         c7a94656-97cb-4986-8c41-0340955baef5)(content(Whitespace\" \
         \"))))(Tile((id \
         47a54665-73a1-4309-9972-5eb3487809b7)(label(targetRow))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         cd9baa2b-c99d-493d-9cef-780fb24cfc81)(content(Whitespace\"\\n\")))))((Secondary((id \
         2718b2b3-70e7-4a74-a874-9c38a3f28a28)(content(Whitespace\" \
         \"))))(Tile((id \
         a95b37cd-be9e-4845-8613-ee9fc60f4d00)(label(map))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         ae7ebcf1-7cd6-49e3-b9be-7051852e73b3)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         f413458b-32a6-4ae9-a660-b93730aafbcc)(label(row))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         7e064a6d-8d45-4e56-835b-a5afcfe07604)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         0e258a46-9691-477e-8d9d-fdba05801c49)(content(Whitespace\" \
         \"))))(Tile((id c6ad91c0-6445-4224-b68d-fc476c23ac4f)(label(fun \
         ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 37))(sort Exp))))))(shards(0 \
         1))(children(((Secondary((id \
         08b29969-1943-4a08-81b7-756e5fa5cd76)(content(Whitespace\" \
         \"))))(Tile((id \
         24206232-b381-4a33-b4de-18e55f74725c)(label(_))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         a7995e09-b6ff-4192-855a-b7ad0a29bf41)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         6b9f72ce-918e-4501-99d8-e99a66d043a0)(content(Whitespace\" \
         \"))))(Tile((id \
         fc2e864f-3154-4090-9a7d-1344605aa176)(label(emoji))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         9ac8c0c4-b1f5-4f79-b6e2-bde872d74a4a)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         4f437823-7a25-4b17-8929-69b4a576e3f7)(content(Whitespace\" \
         \"))))(Tile((id \
         ba9197c7-3d1b-42f5-ae72-e2aa0c90ae15)(label(row))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         df0d958c-9e32-42fd-a186-0cd5f21e733f)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         9808c499-4f59-45c3-b372-023355cbec36)(content(Whitespace\"\\n\"))))(Secondary((id \
         6427625f-be75-4e53-97dd-0e03a4f5f7aa)(content(Whitespace\"\\n\"))))(Secondary((id \
         1d82bd9a-cb84-4601-999f-f66f7c1b538a)(content(Comment\"# TODO: Add \
         setCol helper here #\"))))(Secondary((id \
         934733f2-d2b6-4fc7-bf02-b92a76a80c63)(content(Whitespace\"\\n\"))))(Secondary((id \
         34dd14c5-992a-4e86-a9cf-95bb182cd219)(content(Comment\"# Hint: You \
         need to modify each row, changing #\"))))(Secondary((id \
         27662f3b-bf5e-41a7-9313-b370e1baac2b)(content(Whitespace\"\\n\"))))(Secondary((id \
         9390a627-1c56-4b17-9fa4-dba3ecd2ae6b)(content(Comment\"# only the \
         cell at the target column.         #\"))))(Secondary((id \
         5073c314-737d-4803-8935-f06f12faf471)(content(Whitespace\"\\n\"))))(Secondary((id \
         925c09c3-5bc7-41ec-a328-0f54e378163f)(content(Whitespace\"\\n\"))))(Tile((id \
         a8702f1c-663b-48ae-8edd-ffb83241898a)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         d9293f78-11de-4a17-8340-72e04b06e9d6)(content(Whitespace\" \
         \"))))(Tile((id \
         3a223690-9d1a-4f6f-8b23-2928ebc276f3)(label(setAll))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         05fc0469-cf41-4b41-be2a-d387a6d66def)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 24))(sort Pat))((shape(Concave \
         24))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         0f56ff8b-c2d2-4395-a046-187c90a7a010)(content(Whitespace\" \
         \"))))(Tile((id \
         e2def99f-73bc-4c36-b2d7-7b15f748422d)(label(\"(\"\")\"))(mold((out \
         Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0 1))(children(((Tile((id \
         c3cbaadb-d4a7-45f3-a912-b710ee0a1d3c)(label(Canvas))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         0c93d428-8475-4464-9d9f-f4e6501b7e23)(label(,))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 44))(sort Typ))((shape(Concave \
         44))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         70747180-ed0e-416e-9dca-71e6df03909b)(content(Whitespace\" \
         \"))))(Tile((id \
         0ba93328-3edf-4150-832e-d8f98f968f4c)(label(Emoji))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children()))))))))(Secondary((id \
         5b1c1db1-e1ac-4b42-9318-d94d94e8c43d)(content(Whitespace\" \
         \"))))(Tile((id \
         892ec647-1a97-4005-8e6f-07741dc65ff1)(label(->))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 13))(sort Typ))((shape(Concave \
         13))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         cabc8e00-34aa-4af5-a9e8-a7da49e3c133)(content(Whitespace\" \
         \"))))(Tile((id \
         de243cf9-e5da-4065-a64b-97a4e14bdb8d)(label(Canvas))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         cdb2c44b-8d1d-40a3-9c7c-d44c4773d377)(content(Whitespace\" \
         \")))))((Secondary((id \
         c63c58f9-6fac-49aa-a22f-abd321e02787)(content(Whitespace\"\\n\"))))(Tile((id \
         8003a6be-40eb-4adf-8e86-c39969cc6936)(label(fun ->))(mold((out \
         Exp)(in_(Pat))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         37))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         e5e26ee0-4824-474a-a919-5f0d67e8f4a6)(content(Whitespace\" \
         \"))))(Tile((id \
         5850eb7c-d4c3-4b84-b1ec-7e8f30bceaae)(label(\"(\"\")\"))(mold((out \
         Pat)(in_(Pat))(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0 1))(children(((Tile((id \
         cf2b60a2-5168-44ba-9ce5-5d16799b4f5e)(label(canvas))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         cd50a7d6-7ee4-4d2e-a40d-ca36b350ea37)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 44))(sort Pat))((shape(Concave \
         44))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         8eeda8ae-e869-4a1c-817b-05f8c875546f)(content(Whitespace\" \
         \"))))(Tile((id \
         d381b318-770c-4d3b-a434-d26342fbcad9)(label(emoji))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children()))))))))(Secondary((id \
         3aea7f18-856d-4b68-b270-011e3aebc4ef)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         afc35178-2f9f-446f-a1b2-41a918a2d6e3)(content(Whitespace\"\\n\"))))(Tile((id \
         2b5773bb-33c5-418e-8c8f-24d5d22e8600)(label(map))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         c843bf07-76d7-44cc-b5af-8fda177ea2aa)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         3acf4230-6193-4714-8418-7a7c8f5c15f4)(label(canvas))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         9298dda1-5abe-418f-be77-099a2f2c9287)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         51d97f68-89b3-41fb-b8af-e505ff2aaa1f)(content(Whitespace\" \
         \"))))(Tile((id 2b405ef4-ebbb-4bca-ba3b-96df0593b613)(label(fun \
         ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 37))(sort Exp))))))(shards(0 \
         1))(children(((Secondary((id \
         d35ec07f-0bb3-46ba-89b4-fa28f4ccc6af)(content(Whitespace\" \
         \"))))(Tile((id \
         472f12c1-955e-4238-a10e-892f8249bb03)(label(row))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         5f2883a1-ea7f-495d-9d4f-435b9cb039d4)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         aa445f74-c0da-4467-95ed-bd7e19b250a7)(content(Whitespace\" \
         \"))))(Tile((id \
         c6de7e65-c8d3-45be-b4b0-a870ac9b2c5a)(label(map))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         bc31d3dc-14ee-47e6-8f7a-aa45f0b9c41e)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         4609f8b6-2cc0-4ea0-872e-a07377519e19)(label(row))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         941335ac-1632-4f2b-b1da-156048723af6)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         e8729a8a-7439-4538-b374-920f08411c19)(content(Whitespace\" \
         \"))))(Tile((id 7a34d802-cc46-41d4-9fd1-af546acfe9c0)(label(fun \
         ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 37))(sort Exp))))))(shards(0 \
         1))(children(((Secondary((id \
         c5095b98-0658-4f3d-a973-44d3ed1f07e7)(content(Whitespace\" \
         \"))))(Tile((id \
         58f711de-278c-4f17-83f6-28a44a836d3a)(label(_))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         73e7d943-cd66-418c-a8fd-97d48e997cfd)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         4919f905-e2da-43f6-96b8-d15db5a817ad)(content(Whitespace\" \
         \"))))(Tile((id \
         febc39f9-1147-4d6b-8a57-e334d5244262)(label(emoji))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))))))))))))(Secondary((id \
         afdeaa37-9f5f-425d-9119-5e1d942b2cb5)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         b17d4cf6-a736-4e4f-a171-61423fc72e24)(content(Whitespace\"\\n\"))))(Secondary((id \
         2c6d6d90-d5fc-435c-9012-0f01c3bca05e)(content(Whitespace\"\\n\"))))(Tile((id \
         e121f163-bfa9-4403-86c9-ec0c3b9c56ae)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         81833345-bd5b-47d8-adbe-fc865e23542d)(content(Whitespace\" \
         \"))))(Tile((id \
         c5919ba8-5b5a-4bf2-906e-77f656c16380)(label(updateGrid))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         838a5306-d6b2-4159-af83-8472362bd7bc)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 24))(sort Pat))((shape(Concave \
         24))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         69bce243-8494-46c2-bc0d-c3d1e739d26f)(content(Whitespace\" \
         \"))))(Tile((id \
         fb686aa2-7088-4cb7-ae94-632e5bf185d7)(label(\"(\"\")\"))(mold((out \
         Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0 1))(children(((Tile((id \
         70eb5aa0-d5bd-4701-83ef-6601f262e5ec)(label(Model))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         39845901-f51a-410d-9720-95947a932405)(label(,))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 44))(sort Typ))((shape(Concave \
         44))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         5b4fb388-ed32-4d1f-ab13-40b7c8febd7a)(content(Whitespace\" \
         \"))))(Tile((id \
         f4c035f9-48e9-4be3-8c57-575577f42da8)(label(Canvas))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         0fbdc83d-6089-4c47-a9cc-93b1524e4f14)(content(Whitespace\" \
         \"))))(Tile((id \
         823ed762-a314-4485-8b78-de634b4deee7)(label(->))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 13))(sort Typ))((shape(Concave \
         13))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         f02aa5fb-4485-4741-90e5-20dfc8240777)(content(Whitespace\" \
         \"))))(Tile((id \
         03838c9c-7485-476f-a476-b0f8d7975327)(label(Canvas))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children()))))))))(Secondary((id \
         854c431c-222e-4d5d-a605-1fb3ac6b35fe)(content(Whitespace\" \
         \"))))(Tile((id \
         9fef3a7b-bed6-4415-a1f6-e0e7bb4fcda4)(label(->))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 13))(sort Typ))((shape(Concave \
         13))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         c8a1c12b-84ae-4509-b93d-295ee1edfcc6)(content(Whitespace\" \
         \"))))(Tile((id \
         5240ea07-c0d0-4350-b005-e9d45eb25dfa)(label(Model))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         0f070ef2-2d1b-498c-b323-ed92802b0e2d)(content(Whitespace\" \
         \")))))((Secondary((id \
         613c19f2-7142-4a1f-8fa8-ba1edd1d38aa)(content(Whitespace\"\\n\"))))(Tile((id \
         3c7395a0-3e87-445e-8e6f-75b9bf173db6)(label(fun ->))(mold((out \
         Exp)(in_(Pat))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         37))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         1bd233d5-87f0-4135-9e60-ca4a1c5ab84a)(content(Whitespace\" \
         \"))))(Tile((id \
         835c8f4b-d234-4053-ac6e-891fb8a70184)(label(\"(\"\")\"))(mold((out \
         Pat)(in_(Pat))(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0 1))(children(((Tile((id \
         8d9c681b-2976-4361-b0e4-e647ed35f680)(label(m))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         c62763a9-7031-419d-af39-555aaa349343)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 44))(sort Pat))((shape(Concave \
         44))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         f761c2cb-4004-4bb4-b7c5-1471f72112e9)(content(Whitespace\" \
         \"))))(Tile((id \
         410adb85-f451-41d0-b4b4-e961832d0f4c)(label(f))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children()))))))))(Secondary((id \
         1996592a-b463-4158-9d62-8ffc01eb6366)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         28a76a70-d1c0-4135-9747-932dde20617c)(content(Whitespace\" \
         \"))))(Tile((id \
         857f2804-c8fb-4e25-add6-ee93ce48c37d)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         0b8f18db-0b9d-4df6-afaf-af47099bd77d)(label(f))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         23a2b6a7-e082-4234-a32a-b1fbf1f5d1bb)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         79c5bcd5-a87a-44f5-9346-347c86703a5c)(label(m))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         d85dea67-c909-4f6f-afdc-8172e9bc52c8)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         e8ae920f-9a26-4c58-aa6c-ba0e9476777e)(label(canvas))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         f7f72c70-e25a-4ee9-b15f-99f76e0f3467)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         ef3e26ff-9819-4de1-add7-78668694d0b3)(content(Whitespace\" \
         \"))))(Tile((id \
         5d8d003d-db66-42dd-b580-bdeacc0000ce)(label(m))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         de7260cf-a745-4a41-a399-889d9373f539)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         d024efab-e970-412c-84e5-161faa8f88c2)(label(brush))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         14cc0aba-0ab7-4656-9e7e-ba1b113768ca)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         5d02c20a-0736-4461-815c-b5a5f3119f6e)(content(Whitespace\" \
         \"))))(Tile((id \
         6ab07ca3-a74b-4e4c-b16f-81b8bf74f7b6)(label(m))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         fd511f71-ecc1-483d-8b4b-c3547067b918)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         87912e5a-ad8b-4f55-8a64-8a20c061f8db)(label(palette))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         2e0b8998-3344-4192-a3e2-a509d56b02b5)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         b00143a9-34d9-4b83-82ad-8871c675cdef)(content(Whitespace\"\\n\"))))(Secondary((id \
         715a670e-650f-435d-84fe-941e96a380cc)(content(Whitespace\"\\n\"))))(Tile((id \
         e91a3489-2806-4ddb-a4ff-cfe038801b87)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         8d2ebd43-75d0-464e-ab1d-21a8974ee411)(content(Whitespace\" \
         \"))))(Tile((id \
         9b370b54-73dd-4ed6-9492-bd10d3ff824c)(label(update))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         847b4365-3e51-4281-8d3f-050769cd85f8)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 24))(sort Pat))((shape(Concave \
         24))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         92dbb67f-77eb-428d-b9ba-54fa3cc2a51f)(content(Whitespace\" \
         \"))))(Tile((id \
         4a5a7ea1-d3ce-460b-ab91-dd246e32683a)(label(\"(\"\")\"))(mold((out \
         Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0 1))(children(((Tile((id \
         6ecf4191-0bbc-471f-8675-45ae03178b20)(label(Model))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         9b224315-a908-4856-b8af-0b10f5ac6216)(label(,))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 44))(sort Typ))((shape(Concave \
         44))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         9d67c812-85f3-428f-84e3-7ca2db3873c0)(content(Whitespace\" \
         \"))))(Tile((id \
         7a9c7ccd-19a4-454f-9ee0-5a7d30e61cfe)(label(Action))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children()))))))))(Secondary((id \
         55efaa98-5af8-4deb-bf70-4de70a82f87f)(content(Whitespace\" \
         \"))))(Tile((id \
         23f1d129-1470-4c2e-a796-fdcb0f9bdb15)(label(->))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 13))(sort Typ))((shape(Concave \
         13))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         f8e94d42-b07d-4cec-88b9-4a553fb306aa)(content(Whitespace\" \
         \"))))(Tile((id \
         a57938dd-73e9-40fc-bf8b-e682023de698)(label(Model))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         41dfc85c-66d3-4f0b-ae00-aaeedd3e1308)(content(Whitespace\" \
         \")))))((Secondary((id \
         885819c5-db49-4d4b-864f-5601d655d2c9)(content(Whitespace\"\\n\"))))(Tile((id \
         f049c9dd-c9e1-4d0d-9fc8-0247ee103ac2)(label(fun ->))(mold((out \
         Exp)(in_(Pat))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         37))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         ce042d55-a65e-41f0-8409-11f969091649)(content(Whitespace\" \
         \"))))(Tile((id \
         35426fe3-f210-4b71-8e0b-66b1ff12aa5d)(label(m))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         5c93b862-22ef-4134-b7d8-0c086fc8d593)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 44))(sort Pat))((shape(Concave \
         44))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         c7c0036c-3f81-4d12-9916-411cf1f7f98e)(content(Whitespace\" \
         \"))))(Tile((id \
         b6319c7e-dc05-4208-abd4-521bf603f309)(label(action))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         cdd3c5ee-8b44-4d2a-8703-de7fa9e82a3f)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         46c3aea9-c922-4ac8-9865-99fe2e4feedb)(content(Whitespace\"\\n\"))))(Tile((id \
         a9768ba2-1304-4498-b53a-c3f4ac7922a0)(label(case end))(mold((out \
         Exp)(in_(Rul))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         8adf5c2d-2bd0-45f4-a10a-e780881e7178)(content(Whitespace\" \
         \"))))(Tile((id \
         cdeb4994-daa4-4165-9fc7-915146c31ccd)(label(action))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         f3402140-0438-4778-b233-79e3cf879ed3)(content(Whitespace\"\\n\"))))(Tile((id \
         47e27b4f-db26-4ba2-b79c-ae2d2ec979d3)(label(| =>))(mold((out \
         Rul)(in_(Pat))(nibs(((shape(Concave 46))(sort Exp))((shape(Concave \
         46))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         04b07472-26b6-4d1f-86b7-9d608adaf847)(content(Whitespace\" \
         \"))))(Tile((id \
         50baea6f-2884-4cea-8c2b-025c9755640c)(label(SetBrush))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         857b9f15-96a4-4160-9b32-75ef130b47cb)(label(\"(\"\")\"))(mold((out \
         Pat)(in_(Pat))(nibs(((shape(Concave 23))(sort Pat))((shape \
         Convex)(sort Pat))))))(shards(0 1))(children(((Tile((id \
         d45d4f5f-5a14-4a8a-adee-c0bcafe0969a)(label(idx))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children()))))))))(Secondary((id \
         09c94dd8-07eb-48c8-aa5d-c7607ef24897)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         60896d89-a72f-42c6-93d4-b960b021a3b2)(content(Whitespace\"\\n\"))))(Tile((id \
         08db3400-ac41-4398-8baf-1a9c330747bc)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         ba033744-7f04-4194-806a-f2d31aa3b310)(label(m))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         54cc4339-f841-47a5-83b8-bd866fed3810)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         c78f5ac8-d1c6-425f-93b0-4da21a563fab)(label(canvas))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         a3272307-c714-466f-b6df-f5c2dbdc3931)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         32fc6b99-b74c-415e-8669-a48f998239d4)(content(Whitespace\" \
         \"))))(Tile((id \
         243963fe-9fed-4df8-bd00-9658b83c0be0)(label(nth))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         df4939b0-d613-4005-bfeb-c5f6fcc2f660)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         b142a3d5-e6a6-4a88-bdc1-06c8d77abdfc)(label(m))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         0a41bc63-376e-48ea-8921-7a6a40c57776)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         3ece0d69-e514-4c39-87f1-bf000921a353)(label(palette))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         33a68389-ada1-418c-b6e5-664063fff2b8)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         323fc3b4-7abb-4b8e-b062-f3d0a86602d9)(content(Whitespace\" \
         \"))))(Tile((id \
         ec91acd2-5ad5-4cb7-9079-edf9007b6ce6)(label(idx))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         1bf8e0cc-9678-4d4c-b0d9-25566048da29)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         333681b9-17d6-45bb-bd5c-f0d10871f232)(content(Whitespace\" \
         \"))))(Tile((id \
         087ce7a4-24ac-4aa3-a22d-321a217e6b37)(label(m))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         c0ba6157-6283-4f06-87b6-dd7d9f5c1d07)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         d7a7f9d9-d964-4a3e-98c3-99f192e6990d)(label(palette))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         c159f505-000e-4d34-99d3-e7ead34f6619)(content(Whitespace\"\\n\"))))(Tile((id \
         7879103f-194a-432c-9467-3dccb6ce48f1)(label(| =>))(mold((out \
         Rul)(in_(Pat))(nibs(((shape(Concave 46))(sort Exp))((shape(Concave \
         46))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         6c35ce98-3c53-405c-9db9-7ab97bc84bf5)(content(Whitespace\" \
         \"))))(Tile((id \
         10644719-1087-4f7f-9787-b2572cb2e9c2)(label(PaintCell))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         0a809b6c-02a0-4a98-9dd3-c721ce38a983)(label(\"(\"\")\"))(mold((out \
         Pat)(in_(Pat))(nibs(((shape(Concave 23))(sort Pat))((shape \
         Convex)(sort Pat))))))(shards(0 1))(children(((Tile((id \
         e01dcfc5-a9d6-4e25-bab4-3ac31fd7d289)(label(row))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         f8f1c49e-89ad-4568-9f28-e0d108db0a33)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 44))(sort Pat))((shape(Concave \
         44))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         ce892a77-4687-4c2c-88c6-7bb8365462e7)(content(Whitespace\" \
         \"))))(Tile((id \
         132be69b-d432-471a-8885-b0cb5b158543)(label(col))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children()))))))))(Secondary((id \
         08212629-7dcd-46a8-bfd7-8e17657f7cde)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         99231823-e4d3-4020-9c5f-7dd01359d7b4)(content(Whitespace\"\\n\"))))(Tile((id \
         8e4f0c7d-a378-4e3a-96dc-84a4f6699b85)(label(updateGrid))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         1c6aedc3-c74d-441c-b83f-7c9e7ecb180a)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         8ea5b2eb-20ce-4c53-b965-ef11b5c4d025)(label(m))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         ad945a10-0b68-4b0a-a032-a40fa13c053e)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         9a95e2ec-30e6-4260-891c-fe718b1feda0)(content(Whitespace\" \
         \"))))(Tile((id 74f1df08-42c5-4f81-ac2c-583607815beb)(label(fun \
         ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 37))(sort Exp))))))(shards(0 \
         1))(children(((Secondary((id \
         bf0ed9bd-a936-4a14-a1bf-1cd3f0efc302)(content(Whitespace\" \
         \"))))(Tile((id \
         2fec10a0-eaa9-4c22-a96e-c8af5e76b71c)(label(c))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         25af66d2-6912-49b5-80f0-f95f4a5f235d)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         0b6ca013-47cc-43d2-89d6-301edc84c62f)(content(Whitespace\" \
         \"))))(Tile((id \
         928d9aaa-6009-480e-a0e8-ad686d18192d)(label(setCell))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         998ff898-fbee-4a75-8dab-00ed899fab38)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         af6caf23-24dc-423c-b210-1bd1cb6a433f)(label(c))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         4516c5ee-0759-4274-82ac-cc23d8c7ea70)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         f4a63b3a-efad-4db2-9d2e-1358ac84df2e)(content(Whitespace\" \
         \"))))(Tile((id \
         23958338-ba85-4fe1-ab0e-1f8b6a663a2f)(label(row))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         3ea4866f-697d-489b-aa49-a63250d0c2f9)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         4a4f7567-d38e-4daf-ac6c-2f78a0e07819)(content(Whitespace\" \
         \"))))(Tile((id \
         42702344-fd26-4c61-a174-1b49aa2ed41e)(label(col))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         e69e6aa1-c19a-4424-8715-ce71ed229ef7)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         640df77e-e4a2-43d9-b1fd-1e529ea4c880)(content(Whitespace\" \
         \"))))(Tile((id \
         3fb15971-fdd0-4210-8b28-27c2cce78fcb)(label(m))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         ce6c930b-cfbe-4c2a-bfdc-f4d511a7f430)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         e657576a-dcbf-41a2-90c6-8cdad4aa4aa1)(label(brush))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))))))))))))(Secondary((id \
         cef030c1-3441-4711-953b-5b366a268f04)(content(Whitespace\"\\n\"))))(Tile((id \
         640224db-36b1-4089-87fa-9f41ed6b336f)(label(| =>))(mold((out \
         Rul)(in_(Pat))(nibs(((shape(Concave 46))(sort Exp))((shape(Concave \
         46))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         38f04d4f-b575-48af-88e7-e3b861ede1c5)(content(Whitespace\" \
         \"))))(Tile((id \
         30980769-e119-4b9f-8f2a-624ffb0327a7)(label(ClearCell))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         0dacc345-690c-4a22-a2ca-a8d992337504)(label(\"(\"\")\"))(mold((out \
         Pat)(in_(Pat))(nibs(((shape(Concave 23))(sort Pat))((shape \
         Convex)(sort Pat))))))(shards(0 1))(children(((Tile((id \
         683545fd-5fe6-4c15-9afd-19d7e39b62fd)(label(row))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         7f6d8514-638b-424c-8fae-3ac0e7880f8c)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 44))(sort Pat))((shape(Concave \
         44))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         853bc550-bf93-47bd-a6ba-184bb71c4a16)(content(Whitespace\" \
         \"))))(Tile((id \
         679c625f-2294-4b14-becc-abacc21001fc)(label(col))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children()))))))))(Secondary((id \
         b56cd810-5dfe-4e54-b8a3-5e4482a55dc9)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         bbfd6842-10c1-46ff-9db2-6dd96b3bd5ee)(content(Whitespace\"\\n\"))))(Tile((id \
         3f7e569b-6be7-43a7-8d9e-bc449dcbfbdd)(label(updateGrid))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         984771af-9883-4364-b6b4-01371b4bf92e)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         0a47edf1-198e-4b44-b3d0-105502a7b0d0)(label(m))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         0d631c2b-58f2-427c-9d80-9a8485f843e1)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         a81449d4-4749-4f22-8c33-b6f2d7b3a016)(content(Whitespace\" \
         \"))))(Tile((id 8c9427d1-3ddc-4e24-883f-8d280b66b118)(label(fun \
         ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 37))(sort Exp))))))(shards(0 \
         1))(children(((Secondary((id \
         c86ee572-d1e0-48d6-a5b0-00d30a9c4023)(content(Whitespace\" \
         \"))))(Tile((id \
         42b2378e-b701-4480-96c6-3918936c1b15)(label(c))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         b81034d8-24a9-4a67-9814-3699b73dc379)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         1576a797-72c2-4378-b3f7-7abcaf723e36)(content(Whitespace\" \
         \"))))(Tile((id \
         88677d2a-8f20-4d11-bb5f-03e64ab4bfa3)(label(setCell))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         b1b2fffd-b4ac-455c-b588-26d1b7bd9956)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         d23ae5b0-66e1-46aa-b600-ca862b1a4cce)(label(c))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         dac096e1-ce6b-47e7-b30b-9899a590f1c2)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         5cbb1521-e442-47e2-8e27-75d312646121)(content(Whitespace\" \
         \"))))(Tile((id \
         001d7a01-4431-4ecb-80b7-7b72414f7349)(label(row))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         9320e831-00db-40e0-ab0e-16c5b6610c59)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         189eb91d-365a-44a5-aaa8-f9731960841e)(content(Whitespace\" \
         \"))))(Tile((id \
         3f4f0e29-73c8-47ea-8498-d53c33e52897)(label(col))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         749babf3-5956-42ec-94ca-747b47398130)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         68cef366-0f61-4c75-857e-020b99063497)(content(Whitespace\" \
         \"))))(Tile((id \
         610818b0-032d-4f5e-be1a-e68113dca1db)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))))))))))))(Secondary((id \
         1c3ae15e-117d-4115-9d64-84804220703a)(content(Whitespace\"\\n\"))))(Tile((id \
         f7201891-30ba-4bf4-8851-88397800e589)(label(| =>))(mold((out \
         Rul)(in_(Pat))(nibs(((shape(Concave 46))(sort Exp))((shape(Concave \
         46))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         23d3bed3-f209-4c2a-8f49-2cd2a9c377b7)(content(Whitespace\" \
         \"))))(Tile((id \
         5090a9ff-d4ba-423e-a81e-7415fb38f7e3)(label(ClearGrid))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         f50102c8-7620-4b7c-9e36-3c7aa9c11721)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         9a205558-37d8-4ef9-ad0c-e70f12973232)(content(Whitespace\"\\n\"))))(Tile((id \
         ba408dfe-f344-438e-8f7a-07149dd4aa37)(label(updateGrid))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         0156e3f5-9be6-450f-ac73-22092a31e084)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         6ecc7975-13f7-4322-a1b0-cbddbbebd1cd)(label(m))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         3196f0cb-8477-4f7a-a2de-8f3cb11762d1)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         23ddb556-874a-401b-af87-7253b462c24b)(content(Whitespace\" \
         \"))))(Tile((id f9e0b754-eabd-40a9-a71d-48f0d032a2d5)(label(fun \
         ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 37))(sort Exp))))))(shards(0 \
         1))(children(((Secondary((id \
         ea752560-89a7-460c-a1e9-6a6fac3871a3)(content(Whitespace\" \
         \"))))(Tile((id \
         938aaa54-de38-4bb6-a370-d604c5a27ab6)(label(c))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         6aac4234-c9c1-4d8b-92c4-8c432e42039b)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         a217ddf4-19a5-4bb0-91d5-e5a62f2189e7)(content(Whitespace\" \
         \"))))(Tile((id \
         2f9f094e-2230-4a9a-a36f-7f69f5c906f9)(label(setAll))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         4a906ce0-b8b4-4402-8683-c292c9b050cb)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         3ab74b7a-4224-4308-8c74-5ade9d0303a3)(label(c))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         2fdba528-6823-4bfd-b4f9-ed8a7acdcf62)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         52e0a535-f8c1-476b-a9e4-9704a3971e5a)(content(Whitespace\" \
         \"))))(Tile((id \
         20b0a289-1f2c-41f7-91d7-8936a645de1f)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))))))))))))(Secondary((id \
         151063a9-2926-48c8-ad76-fd63b4e47a12)(content(Whitespace\"\\n\"))))(Tile((id \
         c55eddf8-184c-49ca-9053-ccb93f604c9e)(label(| =>))(mold((out \
         Rul)(in_(Pat))(nibs(((shape(Concave 46))(sort Exp))((shape(Concave \
         46))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         726a5c93-3588-40ed-93d3-96b6622111ad)(content(Whitespace\" \
         \"))))(Tile((id \
         a2ab244c-78a6-4b14-8489-d6e81040c31d)(label(PaintRow))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         7eba0d5e-c810-4f64-9fbd-0263b751083b)(label(\"(\"\")\"))(mold((out \
         Pat)(in_(Pat))(nibs(((shape(Concave 23))(sort Pat))((shape \
         Convex)(sort Pat))))))(shards(0 1))(children(((Tile((id \
         b874741b-ab03-44a4-a332-dd1eeeae8c5c)(label(row))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children()))))))))(Secondary((id \
         ff025fdd-1848-4d1d-bc36-2558d2106250)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         767bb978-463d-485a-bf97-9e3b810f281a)(content(Whitespace\"\\n\"))))(Tile((id \
         1ea77f81-200e-4486-9997-d894cabaf983)(label(updateGrid))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         ebaec26b-d802-4c3a-8621-a50002f6c712)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         acd207f2-e097-489d-b780-4e65a5761314)(label(m))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         ae7d1a72-0e37-4b34-b345-b94079be2f76)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         f9c0d711-ec8b-4f1c-81df-0b4aedfa6f5b)(content(Whitespace\" \
         \"))))(Tile((id 34a5e426-bb20-47ed-b38d-14eaec073680)(label(fun \
         ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 37))(sort Exp))))))(shards(0 \
         1))(children(((Secondary((id \
         053820a2-f2d6-4e5e-bf02-495a53de6ffb)(content(Whitespace\" \
         \"))))(Tile((id \
         495b733e-9c50-4241-8cec-702f2b88c5e9)(label(c))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         fce37239-c960-4c78-bde7-7178560efc1c)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         ea697a53-e6e1-4cda-85ad-1975b927a93f)(content(Whitespace\" \
         \"))))(Tile((id \
         dd4b2152-26b8-4492-8434-796d77731515)(label(setRow))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         655bb322-491f-4b53-a3be-3f1a6fc9dbde)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         87f7848a-1316-4ad6-a8b9-a32975a969c6)(label(c))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         9b47d8a8-403c-4384-8a2d-7fc6351ca89f)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         1420142c-1878-4d8f-b553-391c3e724df7)(content(Whitespace\" \
         \"))))(Tile((id \
         2f2b9f38-3a6c-42b6-88b8-dd36fd849daa)(label(row))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         3e9db129-919e-47ed-a023-7a96694ac674)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         3fd3e17e-fcff-4122-9229-2f00c96ba416)(content(Whitespace\" \
         \"))))(Tile((id \
         5086d250-2922-47d1-bebc-16dd49bd9004)(label(m))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         78abdc04-1252-4486-8288-fcaff55cb316)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         f9e90c63-4ffd-418d-8f2c-9407dd197256)(label(brush))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))))))))))))(Secondary((id \
         d4dd325c-c93a-490d-95ef-4d0f554f9fc3)(content(Whitespace\"\\n\"))))(Secondary((id \
         9179fd7a-928f-40ab-a4d6-4b37b6cd8b28)(content(Comment\"# TODO: Add \
         PaintCol case here #\"))))(Secondary((id \
         0a5989f0-a294-444a-9789-e5dccdd57452)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         060c9bdc-4fe5-4fee-a60b-d2154a04a581)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         50b8fe9f-71cd-48f9-92e8-ef917a48a2aa)(content(Whitespace\"\\n\"))))(Secondary((id \
         6b38c100-98d2-4c3e-8fb5-110046dddc64)(content(Whitespace\"\\n\"))))(Tile((id \
         0ac71697-e1cc-472e-8c6f-6de0eb8931eb)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         aed6263c-f92a-43cb-81b3-a70f688ae2c5)(content(Whitespace\" \
         \"))))(Tile((id \
         a878542e-2922-438e-a0e8-425b16822b9e)(label(do))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         2ce782bb-2835-43c1-9e63-4cece2935516)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 24))(sort Pat))((shape(Concave \
         24))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         4898f0cf-7168-46ed-be61-c31657b41571)(content(Whitespace\" \
         \"))))(Tile((id \
         16c69a7a-bf8e-44c9-ab5f-98efd5bcad88)(label(\"(\"\")\"))(mold((out \
         Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0 1))(children(((Tile((id \
         48b7d9ce-f051-4c4d-8a63-4ec4811e13d3)(label(Model))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         f7f0aa5c-7f4f-4e96-94f6-14d6f8af5cf9)(label(,))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 44))(sort Typ))((shape(Concave \
         44))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         0d895973-5254-47d9-8c5d-998d1e62b097)(content(Whitespace\" \
         \"))))(Tile((id fc85e1c7-4481-4318-9dbd-168d3e63ffb4)(label([ \
         ]))(mold((out Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape \
         Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
         ed8563b9-f539-4299-b527-5d46edef6596)(label(Action))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))))))))))))(Secondary((id \
         f13c7230-6ec2-4ddd-9db9-ac9d3205b935)(content(Whitespace\" \
         \"))))(Tile((id \
         f7d27a8d-baa8-46ca-8ea3-0a5628b2f645)(label(->))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 13))(sort Typ))((shape(Concave \
         13))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         3fd01f89-c19d-4774-8995-c75c64f993a2)(content(Whitespace\" \
         \"))))(Tile((id \
         175ef032-0e45-4a7c-abc2-fa737e9edea9)(label(Model))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         bd01f3bd-9ab7-4f49-a8e1-ecee065c76ea)(content(Whitespace\" \
         \")))))((Secondary((id \
         8def2d7e-41a9-40e9-85dc-624ca375ecad)(content(Whitespace\"\\n\"))))(Tile((id \
         302fed3d-a170-4a34-9d40-3295e5049521)(label(fun ->))(mold((out \
         Exp)(in_(Pat))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         37))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         f9b88498-b7c5-4114-9be0-cc5aa59454e2)(content(Whitespace\" \
         \"))))(Tile((id \
         1c780244-d246-47c7-b4ad-62a23a9af6e5)(label(\"(\"\")\"))(mold((out \
         Pat)(in_(Pat))(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0 1))(children(((Tile((id \
         db958408-f942-4dce-9650-e964d24482ae)(label(init))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         6d811d60-84e2-4d70-801a-904321050c1f)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 24))(sort Pat))((shape(Concave \
         24))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         dda463a8-0f05-4344-b290-9dc017e68cad)(content(Whitespace\" \
         \"))))(Tile((id \
         f6809218-eced-4ddc-bd2c-2a5c164e6d8d)(label(Model))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         33349bc7-3da8-4db6-9b06-08c35c542226)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 44))(sort Pat))((shape(Concave \
         44))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         953aba56-7003-40f2-b5ed-34b747fff6b2)(content(Whitespace\" \
         \"))))(Tile((id \
         034d9025-5aca-46b4-b000-e12d345e9e9d)(label(actions))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         e4328ef3-9eb7-480e-bd06-089a8b28b9f4)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 24))(sort Pat))((shape(Concave \
         24))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         ea145710-3a76-47de-a44c-94aaeb8eeca2)(content(Whitespace\" \
         \"))))(Tile((id 9ed64072-da64-4b24-a467-cd68df44da48)(label([ \
         ]))(mold((out Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape \
         Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
         85608653-9b96-47e4-895e-20466937bd25)(label(Action))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))))))))))))(Secondary((id \
         1dd55f36-dec8-4c4b-8c16-8a458a9933ff)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         373cea9e-15bf-406a-9fd6-42a673ccc7f8)(content(Whitespace\"\\n\"))))(Tile((id \
         49781405-9dcf-476d-9c79-ba1542d560bb)(label(fold_left))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         d95e55ff-59be-4694-a222-f6522d736181)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         619655d3-ae15-4a97-8654-d5fe87b52934)(label(actions))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         42cf29a6-9880-41ce-8b22-5d6cca250d14)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         545ba713-54e6-41de-b033-f8e1eac710a9)(content(Whitespace\" \
         \"))))(Tile((id \
         deca71dc-f25c-451c-b8a0-3a4a2112f4b3)(label(update))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         9464493d-afe8-4582-9aa2-9191790ad702)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         b9cbc7ea-67da-43b4-9e95-e8d4ba897d80)(content(Whitespace\" \
         \"))))(Tile((id \
         3d5c808f-d0ec-4354-bc9a-9bbdd737457f)(label(init))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         d8c645e1-2c5a-4cb9-9d60-3335aba412ea)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         7687f837-59f9-41b5-ae87-ed396e42f8ad)(content(Whitespace\"\\n\"))))(Secondary((id \
         730c75df-a458-4515-ac1c-fcfd08cb5fb5)(content(Whitespace\"\\n\"))))(Secondary((id \
         0488ca80-590d-4dbe-b9cd-88508533ec7a)(content(Comment\"# Existing \
         tests #\"))))(Secondary((id \
         e90b4573-6b83-4fda-9213-fa83facc0bba)(content(Whitespace\"\\n\"))))(Tile((id \
         72042d42-345c-4ac5-a78b-fc80906e1019)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         8afd1494-fa5e-4a80-add2-0aaa27899ff3)(content(Whitespace\"\\n\"))))(Tile((id \
         cb4e36a4-ceae-407c-95f6-a71b38576a72)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         3b11ba99-2c25-44d8-9597-33e682981621)(content(Whitespace\" \
         \"))))(Tile((id \
         1f06500b-2c91-4d52-a6e6-82f1c99e79a5)(label(m))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         f0196759-dd75-48b7-a029-70c4aec56ea2)(content(Whitespace\" \
         \")))))((Secondary((id \
         03493432-368d-456d-9127-436998bef423)(content(Whitespace\" \
         \"))))(Tile((id \
         4a38424f-de22-49c1-8054-ac927ae77cfd)(label(update))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         62bc9e49-3833-43c6-9a90-6521068e7423)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         8c7ba61b-0b71-431e-8b91-7e2bf887e4d8)(label(init))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         d04c6cd4-4f76-4970-857d-f3405eb3e77c)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         82711f84-a3e7-44de-bc0e-2540fc07cb75)(content(Whitespace\" \
         \"))))(Tile((id \
         3328df4b-daef-454e-9867-a36b6ea4bed7)(label(PaintRow))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         1e81efd5-09c4-4646-b07b-d02ef04fab29)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         66fc1551-d8a8-4960-b445-068343f80424)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))))))))))))(Secondary((id \
         eded5f40-4aa4-48c5-beee-db63c494cc36)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         d2626597-5090-4cb0-812f-e3ac08f8332a)(content(Whitespace\"\\n\"))))(Tile((id \
         e14da935-b915-4175-bef1-70d8cdeb6e95)(label(m))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         5c54e5b4-1a8a-439c-a05b-8148dcb6d42d)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         a0e3fa07-3afc-4572-a33e-86e2f6608118)(label(canvas))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         05d4928c-e508-44a3-a6c2-3112599b6ce1)(content(Whitespace\" \
         \"))))(Tile((id \
         06240333-9bc3-4c51-a9b0-991c41a467bc)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         775cf663-dc79-4d09-bdf7-6d04a469eb3f)(content(Whitespace\" \
         \"))))(Tile((id afd7c845-0049-4878-9f82-489af921f155)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         570c1429-bf58-4243-89c3-35b29d32291a)(label([ ]))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         21210c7a-170e-41ca-af82-af5c2806610f)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         9059e2c9-9e7b-4423-9539-d484abc00c7d)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         9c6fbd7a-b724-4418-a1aa-a8b06ca76b90)(content(Whitespace\" \
         \"))))(Tile((id \
         af12844c-44ae-493d-9cb1-6b43e2e48956)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         6295cf9b-f3f6-40a6-8273-b45c7d654890)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         896af24c-aece-4b56-a4b4-5e4fffd500c5)(content(Whitespace\" \
         \"))))(Tile((id \
         530eb88a-d953-4916-9235-2f5129ac3036)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         71b69073-325c-4911-af5a-7c59cbb76fd7)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         b0fe816e-7efc-4d11-921e-d6bd624a9539)(content(Whitespace\" \
         \"))))(Tile((id 1da68dea-107c-40dd-a6ef-6688c4e3ecd9)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         50e299a1-aef0-4d32-ba97-d601d96591ee)(label(\"\\\"\\240\\159\\142\\168\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         183dd596-9653-4dfb-b277-941b0cc91e21)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         bfe4ed39-ccd9-4b37-9cff-9b004242e233)(content(Whitespace\" \
         \"))))(Tile((id \
         0fcfe585-2d94-489a-a35a-8e064792660c)(label(\"\\\"\\240\\159\\142\\168\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         c5585dd4-6d60-4008-9e41-d2243bfbd825)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         ce0d06cc-f80a-4c32-bccc-385d8b00ccff)(content(Whitespace\" \
         \"))))(Tile((id \
         6b7f50ff-2c5f-4094-bb07-b479c98779ef)(label(\"\\\"\\240\\159\\142\\168\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         90346427-55ea-47dd-8af3-a2d7788ecc27)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         5a3c21f6-b418-4e6d-9d03-de41fc5ba53f)(content(Whitespace\" \
         \"))))(Tile((id 18191e61-601e-4a81-b528-13231ff1653b)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         2ea45ae1-de12-46aa-98a0-5064a4120f4b)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         fc9a18b9-4b72-4b74-b50b-5222fade9518)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         df03f8cf-5a10-41f3-a0d2-883b144727d9)(content(Whitespace\" \
         \"))))(Tile((id \
         2b9077cc-162f-473e-98dc-e4f768f06daf)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         492c4d71-f891-4b83-a910-917738076e58)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         25904361-9853-4f0a-b81b-ee487d45025f)(content(Whitespace\" \
         \"))))(Tile((id \
         5c3e8284-e81d-41a1-b1ad-f63883a88a9e)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))))))))))))(Secondary((id \
         3ef1204a-9bf0-4df9-845d-d2995e69239a)(content(Whitespace\"\\n\")))))))))(Tile((id \
         e6d15935-5fd5-492e-8345-522b27cdd859)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         48cd22b2-0726-4852-864a-423effbc1e55)(content(Whitespace\"\\n\"))))(Secondary((id \
         d3a8c822-21ec-4a6d-96dd-4a5295854dca)(content(Whitespace\"\\n\"))))(Secondary((id \
         4552f5d5-1f81-4ca3-adf3-6de864555028)(content(Comment\"# New tests \
         for PaintCol #\"))))(Secondary((id \
         e1164f63-74ff-443b-9730-19e4df66b329)(content(Whitespace\"\\n\"))))(Tile((id \
         366e40ca-3d7b-4261-82f8-e56aaac06ebf)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         55d79355-b894-467f-bda9-08f68e571d1c)(content(Whitespace\"\\n\"))))(Tile((id \
         51cb9549-2cd0-42d8-93d2-5ad9743eb6a4)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         b0362592-858b-4264-ba77-3c746229da1b)(content(Whitespace\" \
         \"))))(Tile((id \
         1bda9cc1-fc51-4db9-ac36-06487ed393b9)(label(m))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         24bb416f-576b-4e49-b698-bcd870796a95)(content(Whitespace\" \
         \")))))((Secondary((id \
         cfc87e4f-ea70-43e0-90e3-f5fc9f589737)(content(Whitespace\" \
         \"))))(Tile((id \
         213e2551-c903-481e-93dc-6b904bbd36a3)(label(update))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         5bebf15d-de18-425c-8013-ed5fb7529e38)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         666e46a6-b46c-4117-8c71-69937470208e)(label(init))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         bb43d9fb-0da7-4da9-a022-1aa40cefbfa8)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         5e0832fa-2f76-4d60-96c6-86e1ab70ef82)(content(Whitespace\" \
         \"))))(Tile((id \
         ce81d93c-7e0b-468a-adab-d7ad865a6544)(label(PaintCol))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         7897d2c4-ef05-4c0f-8a0e-78a0be8aef59)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         8717dc70-de0e-4621-8d92-04eddb608d81)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))))))))))))(Secondary((id \
         8e9fde02-88a6-4ff2-bbe2-4671add2790e)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         a3560192-483c-4472-88d4-9d6755301535)(content(Whitespace\"\\n\"))))(Tile((id \
         e4345975-bed1-4dd2-84ac-55082e58061f)(label(m))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         be6d8db2-ae8f-42b7-867a-3ca77436a0c4)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         88da0c6f-043d-426d-a77f-ede9a19a499e)(label(canvas))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         3b5c775b-9f42-4bde-97f2-62e1b6cba845)(content(Whitespace\" \
         \"))))(Tile((id \
         5f3f25ab-1b8d-4a08-93a3-9abc676cfb91)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         92c715ef-d1b6-40a9-a20c-26cadd6e34e1)(content(Whitespace\" \
         \"))))(Tile((id a25056a0-3ed5-4541-94b0-d98e3d736654)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         fe04a021-26b3-4118-9932-77b4b7c5a625)(label([ ]))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         0033d97c-c237-447c-a559-d5bf1700d472)(label(\"\\\"\\240\\159\\142\\168\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         2755547d-38e6-4e36-98bd-d53d12f3a8d3)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         ad032b76-a785-442c-9036-e757d6fd4216)(content(Whitespace\" \
         \"))))(Tile((id \
         187aa568-503a-4035-8e46-2f289814f6fc)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         435926d9-af93-40db-99ab-434ea327a113)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         a4027ed2-b9fe-4fe7-aca5-81fa0e0fd8b1)(content(Whitespace\" \
         \"))))(Tile((id \
         ae98a256-6c58-4ee5-ae27-8d94b2175c55)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         4e423bca-c571-4fe5-896c-36e7d2b13337)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         f59da1a7-90e0-4f0b-b248-1850fc455f9c)(content(Whitespace\" \
         \"))))(Tile((id ee582fd0-96f5-46f2-9a24-3e227645c742)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         ef67ccc4-84f4-4aaa-a180-8fe67a729fee)(label(\"\\\"\\240\\159\\142\\168\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         c961ac44-e674-4580-bddf-a570ce6a74da)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         f7b84a3c-dc93-43b5-9d3a-0521e15a3205)(content(Whitespace\" \
         \"))))(Tile((id \
         e60a5c8a-67c0-4e55-96e1-a1fa4932babc)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         5a43bd23-c6ef-4d10-a208-7e3f20c9da71)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         f9a0d8c4-d577-4ddf-bedc-9a7816e0c463)(content(Whitespace\" \
         \"))))(Tile((id \
         486d2f74-0125-42f4-8acd-600bea1ce1e5)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         55aae324-32ee-4871-9628-9b6f1095ed4d)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         a83a65fe-32ca-4d63-980a-9ecbe33b2e52)(content(Whitespace\" \
         \"))))(Tile((id 0b16e876-3901-46a2-b7df-6eeaed8eac56)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         cbb14d98-8873-400a-a9d7-7d2fa3523335)(label(\"\\\"\\240\\159\\142\\168\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         68ba4e96-ef83-4668-865e-09ad9f165628)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         7ab2ccc8-3e39-4d3e-b441-c5b02b243220)(content(Whitespace\" \
         \"))))(Tile((id \
         2b62ee37-d800-43de-bccc-02b011be0016)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         4f6efb0a-d5a2-4f8e-b252-46bc896f3ac3)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         7eba496b-6080-49a6-b77f-1d0859917de2)(content(Whitespace\" \
         \"))))(Tile((id \
         01e73e75-ef3e-4a6a-91bc-80d1350211f6)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))))))))))))(Secondary((id \
         87cf54f4-130f-4523-8d13-3078466c0dcd)(content(Whitespace\"\\n\")))))))))(Tile((id \
         bfcbed70-807f-4898-8195-4e195c6c2d8e)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         c921a0fa-e3d5-4f40-a49d-6f5839eb06b7)(content(Whitespace\"\\n\"))))(Secondary((id \
         f1fabe1f-0260-4830-aad3-185f1b9bf5c2)(content(Whitespace\"\\n\"))))(Tile((id \
         6f6b1cf9-4de4-40a6-9a54-2e1499590e72)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         cfebabd3-86a9-42de-b576-cc2c8a49561a)(content(Whitespace\"\\n\"))))(Tile((id \
         3c74a004-6cd1-4f4b-87f9-7cb5ad8cc601)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         7147d079-8e76-4458-a4b3-4e95f8df9dd0)(content(Whitespace\" \
         \"))))(Tile((id \
         bade3d3b-004d-4ffc-a1f0-1051b4028a66)(label(m))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         52a83656-2298-499c-b058-e2b0d12a2f08)(content(Whitespace\" \
         \")))))((Secondary((id \
         5479e875-34a9-46b3-b061-5ce6c95532ba)(content(Whitespace\" \
         \"))))(Tile((id \
         8015dfa6-7725-4c32-a7fd-4266b0f2696d)(label(update))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         b072d478-aec7-428c-803e-3ccc8df73e33)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         d2c5682e-1b63-4870-8290-47b3b2a5ffe3)(label(init))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         be2dc1b9-d414-42f5-b06f-364f42e8ed93)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         876b3386-1f59-478b-b000-a813dba9afae)(content(Whitespace\" \
         \"))))(Tile((id \
         ef33b830-c0a3-4217-ae7d-9a9e8872aa54)(label(PaintCol))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         ebcb6f30-f387-4435-98d0-6599a8a0b5b0)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         44c1f427-2f18-4050-9be3-b7084a034e67)(label(2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))))))))))))(Secondary((id \
         e0a8d554-bb18-4eb9-9f25-dfbce985dd7a)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         17df3e0c-bc6f-4d0e-a430-757b1a140fcf)(content(Whitespace\"\\n\"))))(Tile((id \
         0191fa1a-c3df-4fd4-af33-0cdb600ad7f5)(label(m))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         125b8fce-ffdc-4f24-badc-055787fb2885)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         36ab553e-4044-4123-8a89-c23488e7c238)(label(canvas))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         7f4e6592-0a23-4185-9b70-1ace02275bd5)(content(Whitespace\" \
         \"))))(Tile((id \
         cfd7043f-fca4-476c-a09f-9a9e13baf029)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         e204d2b0-49ce-4c40-868b-b01b65a44caa)(content(Whitespace\" \
         \"))))(Tile((id 0bc04e0b-716f-4f04-b76a-26416b61d8ff)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         8445ed8e-588b-46a1-a444-e061192ff093)(label([ ]))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         36acc7d5-723e-4843-a707-13c68eec83d5)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         7ff3080b-37d2-466a-9d62-5fa152980089)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         268392f0-60b0-4c2d-88d0-be8bbd03329d)(content(Whitespace\" \
         \"))))(Tile((id \
         c5bca570-945f-40b6-8788-74a56d863589)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         d0f76569-6ebf-4241-9cc6-d38972a652ef)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         59a0526f-8307-4bf1-811e-5a6cf9ef7a12)(content(Whitespace\" \
         \"))))(Tile((id \
         996d0d34-dbdd-4188-9c0d-6b81a152b530)(label(\"\\\"\\240\\159\\142\\168\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         45ceb30f-dde7-4e38-932e-25c09a348d46)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         b5e8edc8-a9e4-411a-b007-3838ff8656b1)(content(Whitespace\" \
         \"))))(Tile((id 5faa9506-abd5-4727-98e8-e76947061d17)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         06c084dc-c21f-4014-a35c-e4212a2179de)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         5a4f1d18-792c-4eae-9611-c3b53ac8748d)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         645c6e2f-a14e-4931-a068-ef460d2cb710)(content(Whitespace\" \
         \"))))(Tile((id \
         9db9c2fe-8f34-46cf-8c80-fbcd33d3f0a6)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         b536df8d-ae9a-45dd-9bdb-ee6dde760cee)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         6b5a3e12-0d05-45b8-b3a8-44811a4f7abd)(content(Whitespace\" \
         \"))))(Tile((id \
         9091afbe-fcfa-47dc-9b5a-5e2d9356ea9d)(label(\"\\\"\\240\\159\\142\\168\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         2feb6683-77da-402a-9e2e-6f07763cbf25)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         3700b3a4-4282-4b77-a0df-fe9489b142a0)(content(Whitespace\" \
         \"))))(Tile((id 21c592c5-19ae-4abb-9744-1114e0b62d74)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         eaa774f5-3ded-482e-ad88-ca00a068271a)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         d2efc185-57ac-4b98-8b07-17185c5988fe)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         167c9687-c1bb-40ca-808b-67682e4e4e0c)(content(Whitespace\" \
         \"))))(Tile((id \
         4dca736f-8691-4883-a8d1-a9124ea60da4)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         3fbd89b0-a173-42ab-93b8-e7e793043371)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         795d782c-7d26-4da1-9894-26ba79a8126a)(content(Whitespace\" \
         \"))))(Tile((id \
         5088fe15-e6e8-49db-8b17-4e17ec689e59)(label(\"\\\"\\240\\159\\142\\168\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))))))))))))(Secondary((id \
         c5088edb-7e02-4a0f-b145-f854a09ff8c2)(content(Whitespace\"\\n\")))))))))(Tile((id \
         1c8511ba-d9a5-4ae5-9fd8-8e3bae52f42e)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         13d5e666-100f-46fa-b3ec-e177024e56cf)(content(Whitespace\"\\n\"))))(Secondary((id \
         3a6c4ba1-3f86-421f-96ba-64b970af1b9f)(content(Whitespace\"\\n\"))))(Tile((id \
         00e58105-bc95-48bd-bf6b-c376293fe6f6)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         c87675a2-fefa-41e3-91c0-7b142e8503dc)(content(Whitespace\"\\n\"))))(Tile((id \
         d11d2f2d-638c-4bff-821f-14e376c60e87)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         f6ffcfad-c135-4062-80d6-0e5c7b9b00a0)(content(Whitespace\" \
         \"))))(Tile((id \
         2b2adf75-257f-4173-8537-674230ba1ab3)(label(m))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         43b462a3-8cba-4ec0-be0b-2bace2223d45)(content(Whitespace\" \
         \")))))((Secondary((id \
         0b5a1550-7316-42ec-ad68-49733d36b079)(content(Whitespace\" \
         \"))))(Tile((id \
         5ce0ede0-c44d-4689-adc6-97ffd2d02a1e)(label(do))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         6434b34d-1116-4ccc-9b05-36c7cacc675c)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         e235a870-f741-4707-9630-f7e151de01ef)(label(init))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         8aee9498-527e-461b-a30f-8c1b08b5cc13)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         2395aa0f-b9c7-436c-b495-f3bbdd7821ea)(content(Whitespace\" \
         \"))))(Tile((id f55e21b5-db84-4d83-b047-c32afb12b0b6)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         6cab4bc8-f54c-4790-a603-e20cc29a26b1)(label(PaintRow))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         1a3c934b-2564-46b5-8d80-d60db159def7)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         125a292f-6f93-4de7-91ba-c9498d281bc9)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         257dbc03-41c3-4603-b722-b114e1430609)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         f173a128-008f-4588-b604-27c72350138f)(content(Whitespace\" \
         \"))))(Tile((id \
         c4d326f4-9bf1-4d1c-81d1-e3e845d49d3b)(label(PaintCol))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         bbed9d36-ca3a-47f6-b22c-ceca474e23f3)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         11402e0c-1caf-43b5-a112-a2b75e4f6fc5)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))))))))))))(Secondary((id \
         43e1cded-9b76-4d2f-a5a0-e4eeda8ec797)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         272f8475-25a4-4ae4-a5ac-9474c7d3f3f8)(content(Whitespace\"\\n\"))))(Tile((id \
         3f7425fe-2f00-498c-9bfa-183fcc8f8680)(label(m))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         e4c6786a-8130-4d0a-b851-f7ee8ac2efe3)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         8602aed9-2612-43fa-833a-a9f7c25fbab7)(label(canvas))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         d4f074ed-0849-4562-84fa-e36f2ab654be)(content(Whitespace\" \
         \"))))(Tile((id \
         1c902568-976e-4f68-95b4-634daa09a8eb)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         1c0ed4bd-d6c0-4e64-8eed-04202fbabd71)(content(Whitespace\" \
         \"))))(Tile((id 9aeb0bb1-351a-4164-aa8b-6f7304b2aa1a)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         48b97b9e-8941-40f4-a05d-ab08fa8cf43a)(label([ ]))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         dc41ca0f-0d62-45ab-841e-08a1aefa5aec)(label(\"\\\"\\240\\159\\142\\168\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         e98d11d9-50b6-4a27-b771-566f79a521f3)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         cfe4454f-d6a8-4219-b502-828fe279fce2)(content(Whitespace\" \
         \"))))(Tile((id \
         7f932063-4b09-454b-9dcd-f465fb2e478d)(label(\"\\\"\\240\\159\\142\\168\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         5fc5d3ba-874a-4e00-91c4-70944595adcc)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         b7638274-08d1-4a0a-8fd8-035d1f44228c)(content(Whitespace\" \
         \"))))(Tile((id \
         14428032-d9e2-4a87-a148-5b1409437405)(label(\"\\\"\\240\\159\\142\\168\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         233d7e1f-8055-4f3c-ab26-89b3941c14ac)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         55a751ef-5498-4a76-aeb2-3ed5b2ebcd18)(content(Whitespace\" \
         \"))))(Tile((id 500e6d03-875b-458a-8558-5d9b90565028)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         3b470465-bc24-474e-b898-8c7f7f531353)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         aa895d5b-e43d-4a54-8dcb-ee2302347f3b)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         a15debd8-7ef8-4c91-acc9-08208e37f288)(content(Whitespace\" \
         \"))))(Tile((id \
         cf11db52-4156-4ada-8612-35c20f5614b1)(label(\"\\\"\\240\\159\\142\\168\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         a9b683cc-597a-404b-8b46-9211d7307cdd)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         049f202a-ed42-4692-abdf-554346fd55b2)(content(Whitespace\" \
         \"))))(Tile((id \
         dccb99e9-8f27-4716-bbf2-8ec2a5c09256)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         856616ce-3ddb-4ff8-9588-a256df9653e1)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         8f1ec1ae-12f9-4123-9dde-668a5e29f67e)(content(Whitespace\" \
         \"))))(Tile((id 51669649-9d89-47b7-aeb2-7da0ef3610ef)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         d9d72499-7099-4cdf-96ad-952a29a46723)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         19932ebc-bcf6-4301-a3ac-b7e3af93d390)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         091c3032-120f-49b5-adab-b8f1b3c89764)(content(Whitespace\" \
         \"))))(Tile((id \
         18bc6321-5891-41c3-b3d3-1d6a69c81a3a)(label(\"\\\"\\240\\159\\142\\168\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         0e37ef80-9ddd-4118-a789-d45f74432c17)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         4e6e26ab-1860-4222-bf5f-41e0221cc1ab)(content(Whitespace\" \
         \"))))(Tile((id \
         6e939c77-a354-43fe-86bf-9f8a7cb710cb)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))))))))))))(Secondary((id \
         8c6c8182-27a5-4547-94d8-9771316b7783)(content(Whitespace\"\\n\")))))))))(Tile((id \
         68017fb2-5776-4c76-97a5-b578869c8dd5)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         d966e151-0b80-484b-96c5-3913f1ecffe8)(content(Whitespace\"\\n\"))))(Secondary((id \
         a2a78f02-afe1-4c62-b757-ee702b92509f)(content(Whitespace\"\\n\"))))(Tile((id \
         8e17c2e4-8665-4995-9d69-5242357c47f0)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         6f246b57-5381-486a-b2af-074bf5df434f)(content(Whitespace\"\\n\"))))(Tile((id \
         37bccad7-029d-49b3-b99b-3d0b4ab13a82)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         dbf6d34d-eb6b-498e-a5af-54fb433af8eb)(content(Whitespace\" \
         \"))))(Tile((id \
         0df4bef4-73c7-4f26-b346-cf9486b04b52)(label(m))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         db959e43-fd58-4d54-bdea-32397e1d03f2)(content(Whitespace\" \
         \")))))((Secondary((id \
         c626aa33-a4b4-44e8-9cb2-fd84924a96d7)(content(Whitespace\" \
         \"))))(Tile((id \
         bfd9c433-f89b-4f14-ac87-86775656ba30)(label(do))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         9dafb619-8ce9-4916-a11a-855f904af411)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         03d48587-65f7-4907-bfdd-2f0f54788c7e)(label(init))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         6013915f-0b0a-4a8c-a778-63e8e35a48b6)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         5f89faad-7687-45a2-950b-3f56534994d1)(content(Whitespace\" \
         \"))))(Tile((id 1fe38fcc-b51b-4f92-9a96-c482ba87c525)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         95c6adfd-75fa-468a-959f-b5529150e8c1)(label(SetBrush))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         3a45bb28-7010-448d-b98d-fc64f0b7807b)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         2a41d07b-92b3-46c1-9937-5e7551b255e1)(label(2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         124a8a18-4f6a-4e55-ae86-ade837435cf6)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         a47eae22-60fb-4d77-a7ba-abeb8c0da688)(content(Whitespace\" \
         \"))))(Tile((id \
         344b9422-050b-430d-93ab-f9c22ba7c6e0)(label(PaintCol))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         807bdec0-ac87-454a-abf4-60d83e3517d5)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         34a02e5d-7ec6-4b97-8747-c9c444ce9c54)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))))))))))))(Secondary((id \
         c5a04767-c0ff-4e17-8d46-0747af36676b)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         a817cedf-353b-4d9c-91d2-8101777087c8)(content(Whitespace\"\\n\"))))(Tile((id \
         4cbfb527-7a6d-49cb-9ac1-db214bde3385)(label(m))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         64d9af40-34a6-41f5-8db1-b6486898d3be)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         2f5f243a-ea5e-4a50-a8b9-3efe4bd64878)(label(canvas))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         17e83f4e-9efb-4772-ad53-b7cffd54239c)(content(Whitespace\" \
         \"))))(Tile((id \
         6f72cebf-b4c9-46f9-8ab1-58df78ef40eb)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         aca41a59-dde4-4d1e-9aff-50bc02fbec88)(content(Whitespace\" \
         \"))))(Tile((id 4a6b9d59-db32-45d7-ba5a-d512b6fbdc23)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         b4550b22-e5e4-4789-bab1-d20d346c82a6)(label([ ]))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         7a123027-cbcc-49ae-90e8-69d172227c30)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         9061ab6a-f054-4952-9726-98226be90345)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         2b310909-923b-4676-a961-264fe8052982)(content(Whitespace\" \
         \"))))(Tile((id \
         0132aff3-f90f-4125-a55e-4fcaba7ce40a)(label(\"\\\"\\240\\159\\146\\156\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         82cc3534-a22a-41f4-b82e-3aed3c866ad3)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         dfde7bd7-9eb0-4341-b0f7-788508939022)(content(Whitespace\" \
         \"))))(Tile((id \
         e9cc41f9-fab9-4628-b940-30c28def97f4)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         637eaba5-1d01-42af-9392-5c3f36403a5d)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         8dec196e-ddf2-43a6-b1c5-0936900d11fd)(content(Whitespace\" \
         \"))))(Tile((id 3fc785cc-7cb5-401a-b853-83368a5f6182)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         ea4ee6ae-a0ee-4ad9-9a62-bb646326c2a0)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         25a321c9-8e9e-4b13-88d3-e06b2c82f0cc)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         8969745c-326e-4c76-8ac9-7dc2fc47ad74)(content(Whitespace\" \
         \"))))(Tile((id \
         61a755f1-bbbd-4673-9ca0-d22ea998d12c)(label(\"\\\"\\240\\159\\146\\156\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         5f644d11-133f-42f3-8ae2-f0d07631c37b)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         126d0656-e6e1-4ded-aa6d-bc1fbe7e68f6)(content(Whitespace\" \
         \"))))(Tile((id \
         3245afa5-ed0d-4993-bea0-34634453fb0e)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         33938bad-d69f-4f56-a2e1-c940aa53d031)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         4df5b493-ae28-4878-8e5e-bd23b2c6f539)(content(Whitespace\" \
         \"))))(Tile((id c1e4aeed-9e39-43e3-9866-1941232aa3a2)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         8113271a-fea5-4cca-9789-cae381cb62d6)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         5b58c451-68f3-4a80-95de-99fab2df0ee3)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         9f822dbc-7251-487a-8789-6fdb9bfdeaad)(content(Whitespace\" \
         \"))))(Tile((id \
         725f4841-5a62-4a90-b615-49fb1bc953b5)(label(\"\\\"\\240\\159\\146\\156\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         64a6a9e7-3f8c-44bd-99d2-5761eb59cc38)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         04cf92c2-296d-421d-83ea-d59c729f92a4)(content(Whitespace\" \
         \"))))(Tile((id \
         7dfa988f-b3bb-4505-957b-1c785bd8cf74)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))))))))))))(Secondary((id \
         8795ae3c-e3bd-4882-af69-ad7ee04b7367)(content(Whitespace\"\\n\"))))(Tile((id \
         95e5b7b2-37c5-46ad-824a-6c5d85deb073)(label(&&))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 32))(sort Exp))((shape(Concave \
         32))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         7261b4e2-a863-4a1f-b76a-6fe74c49d288)(content(Whitespace\" \
         \"))))(Tile((id \
         aed1e108-7838-4957-a5b3-f190e8d1f1d4)(label(m))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         ffe1f836-27c5-4580-965e-d77262475680)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         79312fb0-a357-4bca-bf22-7d25f63986c5)(label(brush))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         fc577875-e68d-4339-a408-44583de3f380)(content(Whitespace\" \
         \"))))(Tile((id \
         0546a2d8-636d-46d1-8f15-52f842c6fd66)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         5bacfabc-ef4f-400b-a5ea-2d0dceb38bd2)(content(Whitespace\" \
         \"))))(Tile((id \
         7094bcbd-64fe-414e-af30-42acbcec52ef)(label(\"\\\"\\240\\159\\146\\156\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         c9772d82-8bf9-4bd5-b12f-f62e55ffce5d)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         a03a0adc-2def-4857-9eee-8bc6733c2ad0)(content(Whitespace\"\\n\")))))";
      backup_text =
        "# EMOJIPAINT EXTENSION TASK                     #\n\
         #                                               #\n\
         # The emojipaint app lets you paint emojis on   #\n\
         # a grid. It already supports painting rows.    #\n\
         #                                               #\n\
         # YOUR TASK: Add a PaintCol action that fills   #\n\
         # an entire column with the current brush.      #\n\
         #                                               #\n\
         # You need to:                                  #\n\
         #   1. Add PaintCol(Col) to the Action type     #\n\
         #   2. Add a setCol helper function             #\n\
         #   3. Handle PaintCol in the update function   #\n\
         #                                               #\n\
         # Look at how PaintRow is implemented for       #\n\
         # guidance - PaintCol is similar but vertical.  #\n\
         #                                               #\n\
         # Tip: Use auto-probe to see how the canvas     #\n\
         # changes after each action.                    #\n\n\
         type Emoji = String in\n\
         type Canvas = [[Emoji]] in\n\
         type Row = Int in\n\
         type Col = Int in\n\n\
         type Model = (\n\
         canvas = Canvas,\n\
         brush = Emoji,\n\
         palette = [Emoji]\n\
         ) in\n\n\
         type Action =\n\
         + SetBrush(Int)\n\
         + PaintCell(Row, Col)\n\
         + ClearCell(Row, Col)\n\
         + ClearGrid\n\
         + PaintRow(Row)\n\
         # TODO: Add PaintCol(Col) here #\n\
         in\n\n\
         let init: Model = (\n\
         canvas = [\n\
         [\"\", \"\", \"\"],\n\
         [\"\", \"\", \"\"],\n\
         [\"\", \"\", \"\"]\n\
         ],\n\
         brush = \"\240\159\142\168\",\n\
         palette = [\"\240\159\142\168\", \"\240\159\140\159\", \
         \"\240\159\146\156\", \"\240\159\148\165\", \"\240\159\140\138\"]\n\
         ) in\n\n\
         let setCell: (Canvas, Row, Col, Emoji) -> Canvas =\n\
         fun canvas, row, col, emoji ->\n\
         mapi(canvas, fun (i, r) ->\n\
         if i == row\n\
         then mapi(r, fun (j, c) -> if j == col then emoji else c)\n\
         else r)\n\
         in\n\n\
         let setRow: (Canvas, Row, Emoji) -> Canvas =\n\
         fun canvas, targetRow, emoji ->\n\
         mapi(canvas, fun (i, row) ->\n\
         if i == targetRow\n\
         then map(row, fun _ -> emoji)\n\
         else row)\n\
         in\n\n\
         # TODO: Add setCol helper here #\n\
         # Hint: You need to modify each row, changing #\n\
         # only the cell at the target column.         #\n\n\
         let setAll: (Canvas, Emoji) -> Canvas =\n\
         fun (canvas, emoji) ->\n\
         map(canvas, fun row -> map(row, fun _ -> emoji))\n\
         in\n\n\
         let updateGrid: (Model, Canvas -> Canvas) -> Model =\n\
         fun (m, f) -> (f(m.canvas), m.brush, m.palette)\n\
         in\n\n\
         let update: (Model, Action) -> Model =\n\
         fun m, action ->\n\
         case action\n\
         | SetBrush(idx) =>\n\
         (m.canvas, nth(m.palette, idx), m.palette)\n\
         | PaintCell(row, col) =>\n\
         updateGrid(m, fun c -> setCell(c, row, col, m.brush))\n\
         | ClearCell(row, col) =>\n\
         updateGrid(m, fun c -> setCell(c, row, col, \"\"))\n\
         | ClearGrid =>\n\
         updateGrid(m, fun c -> setAll(c, \"\"))\n\
         | PaintRow(row) =>\n\
         updateGrid(m, fun c -> setRow(c, row, m.brush))\n\
         # TODO: Add PaintCol case here #\n\
         end\n\
         in\n\n\
         let do: (Model, [Action]) -> Model =\n\
         fun (init: Model, actions: [Action]) ->\n\
         fold_left(actions, update, init)\n\
         in\n\n\
         # Existing tests #\n\
         test\n\
         let m = update(init, PaintRow(1)) in\n\
         m.canvas == [[\"\", \"\", \"\"], [\"\240\159\142\168\", \
         \"\240\159\142\168\", \"\240\159\142\168\"], [\"\", \"\", \"\"]]\n\
         end;\n\n\
         # New tests for PaintCol #\n\
         test\n\
         let m = update(init, PaintCol(0)) in\n\
         m.canvas == [[\"\240\159\142\168\", \"\", \"\"], \
         [\"\240\159\142\168\", \"\", \"\"], [\"\240\159\142\168\", \"\", \
         \"\"]]\n\
         end;\n\n\
         test\n\
         let m = update(init, PaintCol(2)) in\n\
         m.canvas == [[\"\", \"\", \"\240\159\142\168\"], [\"\", \"\", \
         \"\240\159\142\168\"], [\"\", \"\", \"\240\159\142\168\"]]\n\
         end;\n\n\
         test\n\
         let m = do(init, [PaintRow(0), PaintCol(1)]) in\n\
         m.canvas == [[\"\240\159\142\168\", \"\240\159\142\168\", \
         \"\240\159\142\168\"], [\"\", \"\240\159\142\168\", \"\"], [\"\", \
         \"\240\159\142\168\", \"\"]]\n\
         end;\n\n\
         test\n\
         let m = do(init, [SetBrush(2), PaintCol(1)]) in\n\
         m.canvas == [[\"\", \"\240\159\146\156\", \"\"], [\"\", \
         \"\240\159\146\156\", \"\"], [\"\", \"\240\159\146\156\", \"\"]]\n\
         && m.brush == \"\240\159\146\156\"\n\
         end\n";
      refractors = "()";
    } )
