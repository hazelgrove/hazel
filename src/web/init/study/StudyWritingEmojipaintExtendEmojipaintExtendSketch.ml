let out : string * Haz3lcore.PersistentSegment.t =
  ( "Study / writing / emojipaint-extend / emojipaint-extend-sketch",
    {
      segment =
        "((Secondary((id \
         405f5840-072c-4875-857f-a5d1c6873219)(content(Comment\"# EMOJIPAINT \
         EXTENSION TASK                     #\"))))(Secondary((id \
         c40de80d-ecc0-430a-99f5-b51f001c31e8)(content(Whitespace\"\\n\"))))(Secondary((id \
         5f84f822-69ec-4af8-b5b2-7a28960272dd)(content(Comment\"#                                               \
         #\"))))(Secondary((id \
         12aa118a-ac69-4615-9349-e7b512e3f395)(content(Whitespace\"\\n\"))))(Secondary((id \
         5f74f6d0-d5cf-4783-80b9-5c9f81809067)(content(Comment\"# The \
         emojipaint app lets you paint emojis on   #\"))))(Secondary((id \
         7550897c-49b7-4dc7-bf93-c55b0f786e84)(content(Whitespace\"\\n\"))))(Secondary((id \
         e8a41aa8-ffe8-4723-86fd-25f7b9f9c806)(content(Comment\"# a grid. It \
         already supports painting rows.    #\"))))(Secondary((id \
         f4f0ae3e-6808-4842-b0c9-b200245c243d)(content(Whitespace\"\\n\"))))(Secondary((id \
         98b27b05-b6ea-469c-a504-fae0ec6b52e5)(content(Comment\"#                                               \
         #\"))))(Secondary((id \
         8f08673c-f641-4e24-8184-0eb5831491f3)(content(Whitespace\"\\n\"))))(Secondary((id \
         a00eecf5-7464-4052-aaa5-f2f67eee1902)(content(Comment\"# YOUR TASK: \
         Add a PaintCol action that fills   #\"))))(Secondary((id \
         cbf14976-0f2c-4eed-b34e-22e3ea2c03e4)(content(Whitespace\"\\n\"))))(Secondary((id \
         3de9f6d1-ca13-4176-b853-523dbb21dfa5)(content(Comment\"# an entire \
         column with the current brush.      #\"))))(Secondary((id \
         0251412b-bf68-4ae7-9d4d-2f27163b0158)(content(Whitespace\"\\n\"))))(Secondary((id \
         3201231f-72e3-4d5b-83a6-7c7cff435382)(content(Comment\"#                                               \
         #\"))))(Secondary((id \
         1e089b39-701a-4c46-b536-d5338a536072)(content(Whitespace\"\\n\"))))(Secondary((id \
         e699e511-3fe0-4408-acd5-388b05dc881f)(content(Comment\"# You need \
         to:                                  #\"))))(Secondary((id \
         d358bf87-5fec-4297-b0ff-6e64196b2b5d)(content(Whitespace\"\\n\"))))(Secondary((id \
         d2bc9225-7512-4bb5-ac10-44d1d451ad51)(content(Comment\"#   1. Add \
         PaintCol(Col) to the Action type     #\"))))(Secondary((id \
         5516945a-2899-4709-952e-96bbcc4fc101)(content(Whitespace\"\\n\"))))(Secondary((id \
         f7a7dbf5-103d-484d-9aad-cc9726edce75)(content(Comment\"#   2. Add a \
         setCol helper function             #\"))))(Secondary((id \
         6500f62b-87b7-4a85-bbfd-d423bb5e55df)(content(Whitespace\"\\n\"))))(Secondary((id \
         eb595890-89cf-4034-ba8c-5207820d2365)(content(Comment\"#   3. Handle \
         PaintCol in the update function   #\"))))(Secondary((id \
         d748bb5d-23c5-42ab-a70e-41b92c5b15cc)(content(Whitespace\"\\n\"))))(Secondary((id \
         b60ac385-65ee-43bb-8dde-157b89e26bad)(content(Comment\"#                                               \
         #\"))))(Secondary((id \
         684aa964-2482-424a-8b7f-873638281885)(content(Whitespace\"\\n\"))))(Secondary((id \
         3dc3827f-f331-4312-a480-4cf56eb1fa33)(content(Comment\"# Look at how \
         PaintRow is implemented for       #\"))))(Secondary((id \
         088d5ea5-dfe0-4be6-b021-a7e35071ffc0)(content(Whitespace\"\\n\"))))(Secondary((id \
         6f7fabab-6a4b-4812-90b9-66166de2f7e7)(content(Comment\"# guidance - \
         PaintCol is similar but vertical.  #\"))))(Secondary((id \
         83cf2596-b591-48e6-8c93-4d585bee36ef)(content(Whitespace\"\\n\"))))(Secondary((id \
         320a0005-9a94-49a8-9371-36a4fa96cfbf)(content(Comment\"#                                               \
         #\"))))(Secondary((id \
         f2496ecf-5766-450b-928a-1f7c60f9461e)(content(Whitespace\"\\n\"))))(Secondary((id \
         91461f69-1a8f-4efd-b5d1-c9e698a0fed3)(content(Comment\"# Tip: Use \
         auto-probe to see how the canvas     #\"))))(Secondary((id \
         3e39315f-c83f-406a-b247-d7045a1e4649)(content(Whitespace\"\\n\"))))(Secondary((id \
         53957757-4dc2-48e7-a117-ecb7bc52fbf1)(content(Comment\"# changes \
         after each action.                    #\"))))(Secondary((id \
         df1934fd-e76c-475d-bed3-d5fad9cc4297)(content(Whitespace\"\\n\"))))(Secondary((id \
         a4025ffe-8017-45bb-9027-9f368c78f303)(content(Whitespace\"\\n\"))))(Tile((id \
         5b8a9539-8ee5-46d7-9922-995addb2ed08)(label(type = in))(mold((out \
         Exp)(in_(TPat Typ))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         10294dc1-8d6a-4153-87e4-2c852053764b)(content(Whitespace\" \
         \"))))(Tile((id \
         39a6d04f-807d-4f13-8879-8c92a0d0bc8c)(label(Emoji))(mold((out \
         TPat)(in_())(nibs(((shape Convex)(sort TPat))((shape Convex)(sort \
         TPat))))))(shards(0))(children())))(Secondary((id \
         8c5e8b74-fc89-4cf5-97c4-ce95bff4e2f6)(content(Whitespace\" \
         \")))))((Secondary((id \
         f04dd830-84d1-4334-a652-a98c009cd570)(content(Whitespace\" \
         \"))))(Tile((id \
         7031a8fb-348f-4d9b-8447-58a543076b1d)(label(String))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         3165a94c-f43c-4f41-9065-7fca132183e6)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         c351fd3b-2eba-40f5-a129-9f8442ff86f4)(content(Whitespace\"\\n\"))))(Tile((id \
         34077818-cea1-4e62-b21b-fd23a4a5095d)(label(type = in))(mold((out \
         Exp)(in_(TPat Typ))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         d90ecbb6-b175-4b39-b355-e7c43a546b91)(content(Whitespace\" \
         \"))))(Tile((id \
         97ec7bc4-b64f-41cd-bebd-b684d1946ae6)(label(Canvas))(mold((out \
         TPat)(in_())(nibs(((shape Convex)(sort TPat))((shape Convex)(sort \
         TPat))))))(shards(0))(children())))(Secondary((id \
         c6e5cbeb-1f24-46d4-b0bf-90f3e281f9a7)(content(Whitespace\" \
         \")))))((Secondary((id \
         17bfcc64-d953-4f9a-9bfa-07f9cf5773d5)(content(Whitespace\" \
         \"))))(Tile((id 7e0e50da-9ace-4644-9285-51861054ec9c)(label([ \
         ]))(mold((out Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape \
         Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
         54ea0a56-060d-43ed-9649-f902cadd052c)(label([ ]))(mold((out \
         Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0 1))(children(((Tile((id \
         c6dcc7b1-4115-4fe8-90ac-33a0ee053c87)(label(Emoji))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))))))))))))(Secondary((id \
         e4073f99-19f9-4fa8-aea5-1fd64a207f56)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         e2239601-7703-499c-916e-7c7149994129)(content(Whitespace\"\\n\"))))(Tile((id \
         d7524440-808a-4572-9966-ab62d0bc371c)(label(type = in))(mold((out \
         Exp)(in_(TPat Typ))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         a8861840-3e3b-4860-8e20-a1c753295cc9)(content(Whitespace\" \
         \"))))(Tile((id \
         34f0ba3d-7c05-491e-ba07-1a29ebb06462)(label(Row))(mold((out \
         TPat)(in_())(nibs(((shape Convex)(sort TPat))((shape Convex)(sort \
         TPat))))))(shards(0))(children())))(Secondary((id \
         dba0afc0-539f-4d87-b493-e9fd3297e0c2)(content(Whitespace\" \
         \")))))((Secondary((id \
         c767aa39-975e-4083-96f0-bafb162e4d79)(content(Whitespace\" \
         \"))))(Tile((id \
         adefd32b-a4f0-4cc7-9063-9a825ced6b1a)(label(Int))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         a2093704-1e7f-4e12-958e-cd15a66b18ea)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         11f43dbb-3891-49b8-a80c-052b0c1c087d)(content(Whitespace\"\\n\"))))(Tile((id \
         40876d61-6599-446f-b5fb-57f2d6c2b3b0)(label(type = in))(mold((out \
         Exp)(in_(TPat Typ))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         80f9bcfa-f915-4cee-b7ab-f47744283ff5)(content(Whitespace\" \
         \"))))(Tile((id \
         51c077d6-8854-4760-8dc0-59c6d3ea58ce)(label(Col))(mold((out \
         TPat)(in_())(nibs(((shape Convex)(sort TPat))((shape Convex)(sort \
         TPat))))))(shards(0))(children())))(Secondary((id \
         fbb8f104-6687-43e6-80c1-2ada42114771)(content(Whitespace\" \
         \")))))((Secondary((id \
         83b60b37-60a6-4d4d-8f18-3003401a9c5a)(content(Whitespace\" \
         \"))))(Tile((id \
         11ef7bcc-24cf-4a35-a7d3-a65e8b7f88d5)(label(Int))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         3eaca95a-57ed-475b-bc3a-c6c441dbb363)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         4b0d3237-1141-46d5-8432-8d3c9f4cea6b)(content(Whitespace\"\\n\"))))(Secondary((id \
         8fbaea36-b1ba-4e71-b76d-a459112cd9a9)(content(Whitespace\"\\n\"))))(Tile((id \
         f743ca96-2c3d-4309-b253-116a03e7fece)(label(type = in))(mold((out \
         Exp)(in_(TPat Typ))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         40753eb0-ab05-439f-afa6-96a8d59a1712)(content(Whitespace\" \
         \"))))(Tile((id \
         f327d6da-01e4-431f-ae67-52b0794dfa44)(label(Model))(mold((out \
         TPat)(in_())(nibs(((shape Convex)(sort TPat))((shape Convex)(sort \
         TPat))))))(shards(0))(children())))(Secondary((id \
         086096c4-3d4b-4e5a-8fd1-233c7e4dd251)(content(Whitespace\" \
         \")))))((Secondary((id \
         fc86c044-ac8e-4ce2-b04c-2e36aecb572b)(content(Whitespace\" \
         \"))))(Tile((id \
         1578621a-8fa1-4050-a281-ded1f5a6ce2f)(label(\"(\"\")\"))(mold((out \
         Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0 1))(children(((Secondary((id \
         758a1657-9afe-4bdd-aea5-6e69fe41e7eb)(content(Whitespace\"\\n\"))))(Tile((id \
         8e03a86f-3399-476b-a07a-674f047bb436)(label(canvas))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         8b26eeb2-bd4a-47be-b39a-2f467e558013)(content(Whitespace\" \
         \"))))(Tile((id \
         4dba4d25-0faa-443b-90ad-af09c0d592e9)(label(=))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 39))(sort Typ))((shape(Concave \
         39))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         c030e530-91e7-4408-a421-17fa268c3df7)(content(Whitespace\" \
         \"))))(Tile((id \
         0ffe029f-1f8a-4fd6-953c-f54615aec7d6)(label(Canvas))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         7808eafd-761d-4e8d-9b71-b7560fbfa499)(label(,))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 44))(sort Typ))((shape(Concave \
         44))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         ea578c83-cefe-4c38-94ec-36dee77f15fe)(content(Whitespace\"\\n\"))))(Tile((id \
         883044e8-4938-45eb-8443-1dc4cd906b37)(label(brush))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         65119e33-a371-400d-b669-79db2a10ccd6)(content(Whitespace\" \
         \"))))(Tile((id \
         a78800d3-f352-4555-82ab-4958a560b5d1)(label(=))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 39))(sort Typ))((shape(Concave \
         39))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         1780723b-a03a-4fae-8337-24c2186f5932)(content(Whitespace\" \
         \"))))(Tile((id \
         2c6fcec0-0a20-4bf6-9724-de2e6ed29eb1)(label(Emoji))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         d2dc7369-2c87-424a-b193-0b675103d1b9)(label(,))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 44))(sort Typ))((shape(Concave \
         44))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         5f950a24-1b9e-4bfd-b5ea-c7f29bb2fc51)(content(Whitespace\"\\n\"))))(Tile((id \
         a9c0e6e9-29ab-4f13-870f-6ce71b9482fd)(label(palette))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         121e02ef-57ff-4ab9-92c3-1a742b6eb035)(content(Whitespace\" \
         \"))))(Tile((id \
         13afb8f9-a1fc-48e4-9096-d0588512c943)(label(=))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 39))(sort Typ))((shape(Concave \
         39))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         bfc37c9a-846c-4d9e-8331-f90c58b3aa43)(content(Whitespace\" \
         \"))))(Tile((id 60036308-c9f9-4b07-965f-e43c27eda79e)(label([ \
         ]))(mold((out Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape \
         Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
         24671956-6a91-4627-bb35-2d3e31b13d40)(label(Emoji))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children()))))))))(Secondary((id \
         3e630ba2-6743-4d71-a692-f3ae8232be95)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         da2f839a-5156-4f4a-ae4d-797ce6920dbb)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         1383c6e8-c94a-4999-9d11-95392eac8dd0)(content(Whitespace\"\\n\"))))(Secondary((id \
         c98e098c-6aa5-43f5-b790-8d05bb325bb4)(content(Whitespace\"\\n\"))))(Tile((id \
         8609f48f-f614-452d-b0b5-7d27b26b9106)(label(type = in))(mold((out \
         Exp)(in_(TPat Typ))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         2ac55531-fb86-4b8e-bc84-e524d380d11e)(content(Whitespace\" \
         \"))))(Tile((id \
         6266303e-9810-482c-bdd5-05f86f4b2c6b)(label(Action))(mold((out \
         TPat)(in_())(nibs(((shape Convex)(sort TPat))((shape Convex)(sort \
         TPat))))))(shards(0))(children())))(Secondary((id \
         8403ab77-bfd7-4bd4-ba2b-4150d1af0917)(content(Whitespace\" \
         \")))))((Secondary((id \
         a9c8ce5d-1dd8-4517-9e63-955e6bfdc532)(content(Whitespace\"\\n\"))))(Tile((id \
         b8d2f1b4-767f-4b7b-8457-08f9a298c79d)(label(+))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape(Concave 33))(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         473039b5-1c20-4542-a663-05c96e2a8338)(content(Whitespace\" \
         \"))))(Tile((id \
         a0f3ee76-b673-41f3-8935-12d7f0b91a30)(label(SetBrush))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         133af803-0333-43c7-8411-00454b707c57)(label(\"(\"\")\"))(mold((out \
         Typ)(in_(Typ))(nibs(((shape(Concave 11))(sort Typ))((shape \
         Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
         39b6049f-4288-4390-8842-4d822acc868d)(label(Int))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children()))))))))(Secondary((id \
         c2e1e431-b143-4c7c-9888-6babd22c7645)(content(Whitespace\"\\n\"))))(Tile((id \
         c7eb8dbd-04f2-4b81-a15c-ae1ab0a369c9)(label(+))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 12))(sort Typ))((shape(Concave \
         12))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         15019874-841e-4890-ad17-9c6ab0ba6089)(content(Whitespace\" \
         \"))))(Tile((id \
         a810b248-9209-4501-96d2-598321cfca3c)(label(PaintCell))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         2ef04e21-f76c-40b3-b941-6982a9be4dee)(label(\"(\"\")\"))(mold((out \
         Typ)(in_(Typ))(nibs(((shape(Concave 11))(sort Typ))((shape \
         Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
         c5dffa97-85f7-4166-af8a-72fc55c601e8)(label(Row))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         baeea27f-d146-42f6-acd2-53888e206651)(label(,))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 44))(sort Typ))((shape(Concave \
         44))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         48188265-b46f-4fad-af3f-871c0bbbcaf8)(content(Whitespace\" \
         \"))))(Tile((id \
         106483a0-b247-4442-9051-53bea6ee5bb5)(label(Col))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children()))))))))(Secondary((id \
         817a2844-7aed-4cdf-9bcb-26ffeb4b4c3b)(content(Whitespace\"\\n\"))))(Tile((id \
         c299ce86-02f7-49a8-b7a2-07da39a73b56)(label(+))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 12))(sort Typ))((shape(Concave \
         12))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         f098235a-5283-4728-aaa4-38e921a863fa)(content(Whitespace\" \
         \"))))(Tile((id \
         f71682cf-6a81-4371-8913-15b30a50545c)(label(ClearCell))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         8f4bbc0b-5d59-43b1-96ad-b8cf6c096a57)(label(\"(\"\")\"))(mold((out \
         Typ)(in_(Typ))(nibs(((shape(Concave 11))(sort Typ))((shape \
         Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
         bc1381eb-c335-46d9-b8ba-b5699c873e1f)(label(Row))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         0101c14e-37f1-462f-bd76-904270529d51)(label(,))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 44))(sort Typ))((shape(Concave \
         44))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         8bb3b75a-6be8-4bc9-bfac-6f69c77cd7a9)(content(Whitespace\" \
         \"))))(Tile((id \
         f867920b-0dbf-485d-a801-d6cf2d08cc29)(label(Col))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children()))))))))(Secondary((id \
         f820718a-3d2c-4ebf-8866-ce946a01bdd4)(content(Whitespace\"\\n\"))))(Tile((id \
         92bdd75e-9ad4-441f-846d-45c7267ec40c)(label(+))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 12))(sort Typ))((shape(Concave \
         12))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         3a618304-d10f-4a60-b2c0-48cea0fe0184)(content(Whitespace\" \
         \"))))(Tile((id \
         01dac1f1-7579-4aee-a346-252d59ddd5e6)(label(ClearGrid))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         eac0daeb-54de-417f-87f2-6815d340ec30)(content(Whitespace\"\\n\"))))(Tile((id \
         8be4a975-7131-41dd-8c67-90751b7e95ab)(label(+))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 12))(sort Typ))((shape(Concave \
         12))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         7e0c38d1-b9d9-45ff-9413-abe9a41e8bf7)(content(Whitespace\" \
         \"))))(Tile((id \
         d1f5673c-5fed-4b49-bf55-12080b69f75f)(label(PaintRow))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         8d2d045f-08a7-4474-925d-a0d7307bbcc1)(label(\"(\"\")\"))(mold((out \
         Typ)(in_(Typ))(nibs(((shape(Concave 11))(sort Typ))((shape \
         Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
         672fd7a5-405d-4b6f-849c-b7f69dd823c0)(label(Row))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children()))))))))(Secondary((id \
         b90c733c-634d-476a-83dc-6b6ceb99e0e9)(content(Whitespace\"\\n\"))))(Secondary((id \
         204173a4-2b34-406b-ab72-70835534515e)(content(Comment\"# TODO: Add \
         PaintCol(Col) here #\"))))(Secondary((id \
         93378168-42c2-46c0-869a-efad5a866924)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         e17c1cf2-147b-44dc-a451-a19e234eead9)(content(Whitespace\"\\n\"))))(Secondary((id \
         3d03cab8-6f1e-4a1b-9e80-db0b2cef26f8)(content(Whitespace\"\\n\"))))(Tile((id \
         e6af72f2-f5e2-4461-b402-56001a15c6ab)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         740dbefd-a0af-4070-b6af-318feea9ce68)(content(Whitespace\" \
         \"))))(Tile((id \
         ff9dbfff-6d93-43a6-89c2-e5e26116f6c3)(label(init))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         236f8b56-e3c1-4634-9114-db13a9227e59)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 24))(sort Pat))((shape(Concave \
         24))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         b2732b38-7a0f-4800-a8ff-80040fb7b7f8)(content(Whitespace\" \
         \"))))(Tile((id \
         ecfdb134-46fe-452c-8fda-f90e243fcd23)(label(Model))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         d391f0c7-659d-4e70-ad1d-aef9b413fd34)(content(Whitespace\" \
         \")))))((Secondary((id \
         9da530be-179e-40eb-b46f-bfd2960c8c16)(content(Whitespace\" \
         \"))))(Tile((id \
         64b6f8ce-5871-4bc4-b056-2e55ee0e52ba)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         45f5930a-3af6-43bc-a3b1-0213d3362ecd)(content(Whitespace\"\\n\"))))(Tile((id \
         d1ab2e61-e480-4b25-a59b-652be5b2d1da)(label(canvas))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         f5eb5700-7b76-4ea7-bac2-f49827fe94d3)(content(Whitespace\" \
         \"))))(Tile((id \
         6aa5e285-54af-45b4-9cec-79e229ebf505)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         35bb128c-1fcc-40b9-aa90-a5e1c81e5091)(content(Whitespace\" \
         \"))))(Tile((id d260e98c-491f-41ac-a7d6-7149b2bb7945)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         4d811bd0-6802-433c-9d1e-5f27de52489f)(content(Whitespace\"\\n\"))))(Tile((id \
         e5e5b91a-dc84-4a0b-916a-0734451eb46b)(label([ ]))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         9d861dab-ef78-48bf-974b-e4f81fe2b987)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         e7cb25e4-9a4c-4180-9e4a-7b515e8703a7)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         f5f9826d-824d-49fb-b808-1e3b615bb8f8)(content(Whitespace\" \
         \"))))(Tile((id \
         faa1f0ab-63f0-4bf1-b7ca-d9e47f95dab1)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         a84bbdfa-94b2-4c52-afc1-bb2754de769e)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         7118dc03-73ae-4613-83b9-f785d9e0222a)(content(Whitespace\" \
         \"))))(Tile((id \
         30a2a4ac-2b42-4428-a553-6f0259c6d782)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         ed3e3b6a-e345-4546-b8ca-01bf26fa7562)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         1a527d69-107a-4023-b535-b916e04cd0af)(content(Whitespace\"\\n\"))))(Tile((id \
         a88f4c97-3522-4f6a-917d-c0cdc4a16f2e)(label([ ]))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         7ccfc85a-3435-4b3a-ad08-0c2d5b0d7550)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         96b2aa01-f38b-4d2d-9a2d-47f5e1f550f3)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         60b0c84e-44e9-42fe-8843-6c615b27d80e)(content(Whitespace\" \
         \"))))(Tile((id \
         53303085-ab4a-4107-81dc-18f6603f17b8)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         5b871b01-498e-43eb-b089-0a832e7414b2)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         9c0e9faf-030f-49e9-9bf5-a22b772ebb3b)(content(Whitespace\" \
         \"))))(Tile((id \
         d547d8ff-8100-4daf-8a33-8d717b3ac1ff)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         ac846639-7d3c-4dd7-9413-06d6d0edd311)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         9578afc4-1004-4d95-b4bc-cb3c442147ca)(content(Whitespace\"\\n\"))))(Tile((id \
         a6c2f826-3041-4e2b-b43c-67a82b44657d)(label([ ]))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         a6b0c4ca-371f-4a91-80d0-924fadd3a82a)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         de6907a0-85fb-48c9-b008-b046f23808fe)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         2c07a286-cdbe-4a3f-a1c2-7972025cbd24)(content(Whitespace\" \
         \"))))(Tile((id \
         257d67a2-e709-4c07-a963-d53118fe7fc9)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         5ad82f95-5a11-4a8d-b533-1f0a18f008b8)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         3416a2ae-9676-46c5-ad98-42ae60e2adeb)(content(Whitespace\" \
         \"))))(Tile((id \
         b21b5485-1fcb-4208-b019-90ec70f30edd)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         2c9b94f1-5f2f-4eeb-9e4b-4708426fa1a7)(content(Whitespace\"\\n\")))))))))(Tile((id \
         9cb723ff-3439-4c13-996e-53e0e492ce15)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         79ff200c-e624-43c4-a7be-940c4b2c153e)(content(Whitespace\"\\n\"))))(Tile((id \
         7774f8a1-39d0-42cc-8eb2-33abe382ec4e)(label(brush))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         235cdf53-b76e-4c4b-b0c7-5259176a1b13)(content(Whitespace\" \
         \"))))(Tile((id \
         fda56874-2dfe-48be-9496-9144ee7af3ad)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         aea2d357-e89d-4716-84b4-96938c54df7d)(content(Whitespace\" \
         \"))))(Tile((id \
         d8f60f8f-4eca-4928-8e21-688e179c7d74)(label(\"\\\"\\240\\159\\142\\168\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         e12a64c8-a1bd-4d19-a156-78ee871d0d54)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         4e31604a-110e-46a7-a54a-a0e76fb9b36f)(content(Whitespace\"\\n\"))))(Tile((id \
         59267bcc-1c78-4227-b6bb-af360b330c14)(label(palette))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         f46ed4c3-dd3c-40a4-92be-000d1a7003c7)(content(Whitespace\" \
         \"))))(Tile((id \
         1d07a22b-890e-43b9-bbd8-4b7851d2d82f)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         775768c4-2452-4c9c-b6e4-af07c78320ab)(content(Whitespace\" \
         \"))))(Tile((id cfa0f1ce-aa4a-4aa8-abb0-4f42f5d60ae4)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         060d4867-295c-4d3b-9aed-56f836ee10cc)(label(\"\\\"\\240\\159\\142\\168\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         2e177b65-ca7b-412b-afe9-5992eca23834)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         a25ea892-1fbf-42f0-9788-e95c51087158)(content(Whitespace\" \
         \"))))(Tile((id \
         6c1d89d3-ba87-4262-a9c9-21ecb43de90b)(label(\"\\\"\\240\\159\\140\\159\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         f085316b-2139-4c60-8a40-8b8b4abfc56c)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         af78ba75-ccdc-44d4-8cff-1a2844706c63)(content(Whitespace\" \
         \"))))(Tile((id \
         d99609a1-b11b-4edf-9a2c-a5f027ca4a44)(label(\"\\\"\\240\\159\\146\\156\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         7791d24e-4c66-4302-8137-3cf96c2bc7a4)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         213689ff-cb98-4e13-a4cc-2688734f89be)(content(Whitespace\" \
         \"))))(Tile((id \
         3116ce98-debe-42b3-833d-07594058c2ff)(label(\"\\\"\\240\\159\\148\\165\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         77589890-6f70-4bdc-ac13-413083709c98)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         42b174ba-ed70-4641-968e-02590d2baf81)(content(Whitespace\" \
         \"))))(Tile((id \
         1043b03a-eb7a-4896-8f4e-2a5ecf831b01)(label(\"\\\"\\240\\159\\140\\138\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         76e264fd-670e-422e-80b5-64ee0848e679)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         56d6b5d6-755d-4a03-8c2b-c65f1670fbb5)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         803ac199-c99c-4e9f-bfe9-db05fd0dd512)(content(Whitespace\"\\n\"))))(Secondary((id \
         db8e84d9-5b27-4b4d-b353-ccd26b5dae85)(content(Whitespace\"\\n\"))))(Tile((id \
         a286fb9c-6df8-4be4-86e0-7351b3a6a214)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         ebfaf9a2-411f-4afe-a5be-55a507e57be6)(content(Whitespace\" \
         \"))))(Tile((id \
         93f4a41e-bbde-4853-88d6-c2be15ab8fa3)(label(setCell))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         5bf8c143-5e48-4f7f-be0b-85300249276c)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 24))(sort Pat))((shape(Concave \
         24))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         41c9043f-0b51-4bc2-9279-a62855f275b8)(content(Whitespace\" \
         \"))))(Tile((id \
         150eaed7-ae36-43d7-9d8f-aac994fd2f96)(label(\"(\"\")\"))(mold((out \
         Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0 1))(children(((Tile((id \
         114ff4b5-44ca-4329-9504-2a22d0603fc0)(label(Canvas))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         86e5a44d-0ad0-498c-a4f1-46708f021bde)(label(,))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 44))(sort Typ))((shape(Concave \
         44))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         f7318674-dc87-4352-8e6e-9f2ac966a2bc)(content(Whitespace\" \
         \"))))(Tile((id \
         a1169864-6c39-4a15-8b7f-2a9fe71c611f)(label(Row))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         21bbeffd-76cb-4084-a22f-7497ef8269dd)(label(,))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 44))(sort Typ))((shape(Concave \
         44))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         493e4eaa-3484-490a-b3d5-21b6f1383107)(content(Whitespace\" \
         \"))))(Tile((id \
         f6729f54-3252-433f-bdbc-365c0ca99777)(label(Col))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         ce523a62-276a-4761-b5f0-5e611f26f2bb)(label(,))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 44))(sort Typ))((shape(Concave \
         44))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         55f8999b-802f-4776-be4f-28156a152f1f)(content(Whitespace\" \
         \"))))(Tile((id \
         4a80442d-5cde-412e-8b73-8b322b51387d)(label(Emoji))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children()))))))))(Secondary((id \
         0d0cc035-f7bb-4625-8798-49ed083194ae)(content(Whitespace\" \
         \"))))(Tile((id \
         3df971cd-8c8a-4d24-8df5-996725367842)(label(->))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 13))(sort Typ))((shape(Concave \
         13))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         90f85ed6-4df1-4f39-a90d-c990056c0133)(content(Whitespace\" \
         \"))))(Tile((id \
         3f6ba3ad-6339-4f25-a0df-bdf8547cc954)(label(Canvas))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         9aa342a6-8d2c-4b9c-825d-35e24a43127a)(content(Whitespace\" \
         \")))))((Secondary((id \
         7188b920-ced1-47de-9ddc-c516ebe93296)(content(Whitespace\"\\n\"))))(Tile((id \
         49673315-3e24-4a98-a6d0-2a6b6508a370)(label(fun ->))(mold((out \
         Exp)(in_(Pat))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         37))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         1132e909-8ae5-427d-bd08-572f8c407ba2)(content(Whitespace\" \
         \"))))(Tile((id \
         de1bfe4a-1182-4cdf-82d2-dd9a09414049)(label(canvas))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         4c4b4854-77c0-4046-b1b2-024a85bf7d86)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 44))(sort Pat))((shape(Concave \
         44))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         c1f9ac39-d14c-4825-abcf-5f8d55121e6e)(content(Whitespace\" \
         \"))))(Tile((id \
         995569a4-487b-49f8-80a0-166e91378778)(label(row))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         9d00fa6e-e132-461b-9a3b-5b85b2154789)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 44))(sort Pat))((shape(Concave \
         44))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         71919c65-975c-41b5-bd61-468e70f6b1f4)(content(Whitespace\" \
         \"))))(Tile((id \
         85641f16-52fb-4e90-ab25-77b280ac19d3)(label(col))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         4321409d-8080-43b7-ad04-d59a3f86a8df)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 44))(sort Pat))((shape(Concave \
         44))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         8c38704f-05a4-4bc3-a44b-f3c4a5be5e6e)(content(Whitespace\" \
         \"))))(Tile((id \
         5006f131-85a6-428f-8ee4-5aa422c49c26)(label(emoji))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         74bf9c77-d84a-4893-9c4d-39042a34cee5)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         4ec52246-5e4c-44cb-a8c5-e8918e4e8cbf)(content(Whitespace\"\\n\"))))(Tile((id \
         09f956e1-d083-4029-8cca-555df39f7e9c)(label(mapi))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         af94341b-462e-4ce7-8dab-55d427409ef5)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         f92f21ff-618f-4e87-8015-69be85be90be)(label(canvas))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         d9005398-f7f6-405c-a4eb-73e3f6b720ae)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         d8557fca-9893-4c3f-9b83-7e8325a9513b)(content(Whitespace\" \
         \"))))(Tile((id 2ed8a644-55f3-4dea-8b07-89c4287a33d5)(label(fun \
         ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 37))(sort Exp))))))(shards(0 \
         1))(children(((Secondary((id \
         51994ebc-9dc6-4cfe-8cbe-efdffd66c1d0)(content(Whitespace\" \
         \"))))(Tile((id \
         d53ea192-0a88-43b9-b523-69a47d2b24b3)(label(\"(\"\")\"))(mold((out \
         Pat)(in_(Pat))(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0 1))(children(((Tile((id \
         78182c9a-28b0-45a2-acb1-9be04dc3172a)(label(i))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         cd537c2b-61ef-44db-8862-8360b2f5042f)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 44))(sort Pat))((shape(Concave \
         44))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         7dd428bd-0646-4863-a023-24a7c1ac9bb8)(content(Whitespace\" \
         \"))))(Tile((id \
         5112e4fa-d22e-43ab-90d4-26cfd8580a9b)(label(r))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children()))))))))(Secondary((id \
         4266f1a4-f6ac-4cce-8228-bb4e892eb798)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         6f5d92e1-e536-4fd6-9181-fbd67e299a03)(content(Whitespace\"\\n\"))))(Tile((id \
         39307782-361c-4822-aada-03e3c88e1ff4)(label(if then else))(mold((out \
         Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         36))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         e3266793-8bd2-48b6-871f-d752b69f0acc)(content(Whitespace\" \
         \"))))(Tile((id \
         0eb3887b-189c-4433-a631-57aeaa0da8ca)(label(i))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         7ef6a15a-bd30-44cf-992d-5cd68d176614)(content(Whitespace\" \
         \"))))(Tile((id \
         ffa979c0-6a71-4c89-b7f4-4a5cd4a6d074)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         3d1ec682-7f54-4d66-959d-eb118e26500f)(content(Whitespace\" \
         \"))))(Tile((id \
         449c02b7-9b23-4694-8349-6ac7d8a3ac67)(label(row))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         97a07fb0-2737-4038-8a50-7eebbcdf9d4c)(content(Whitespace\"\\n\")))))((Secondary((id \
         73e08004-1ab6-4811-a280-83b29bca2808)(content(Whitespace\" \
         \"))))(Tile((id \
         8d789d21-12bc-4c0c-a45e-e7d40a7e2736)(label(mapi))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         01dc0c35-ad26-4371-b58d-4f69a4f05ec4)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         a48b031b-d81b-4696-946f-a92f193e750e)(label(r))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         2956fc44-6376-420e-be03-2c933a6a5dca)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         e9ea2a87-ecb6-477c-8a62-7e8a866f95d5)(content(Whitespace\" \
         \"))))(Tile((id 4c108e17-405a-4045-b8e8-a8923a547849)(label(fun \
         ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 37))(sort Exp))))))(shards(0 \
         1))(children(((Secondary((id \
         68533063-63e2-4cef-97ec-9c8089e741d6)(content(Whitespace\" \
         \"))))(Tile((id \
         aafa1a41-d343-4a8b-a708-78d87153247d)(label(\"(\"\")\"))(mold((out \
         Pat)(in_(Pat))(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0 1))(children(((Tile((id \
         9128fcf3-21f5-4b64-b02e-64df236c73e3)(label(j))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         e69cf5a9-7763-4322-8bc6-c060b474e17d)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 44))(sort Pat))((shape(Concave \
         44))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         78ad2c02-e78d-484f-856f-c3e66e336f23)(content(Whitespace\" \
         \"))))(Tile((id \
         c0b3feda-7218-4b50-8203-aa225a6bd8fe)(label(c))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children()))))))))(Secondary((id \
         f1e1df3b-65cd-45a0-9c33-f1c6c47a86e7)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         4f6f10b9-5bb4-4ac4-9262-9cae8ba4f86f)(content(Whitespace\" \
         \"))))(Tile((id f037c56d-53fb-4afe-a90e-9b1585408892)(label(if then \
         else))(mold((out Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 36))(sort Exp))))))(shards(0 1 \
         2))(children(((Secondary((id \
         575068f4-9911-40aa-af06-344c60caa71f)(content(Whitespace\" \
         \"))))(Tile((id \
         b8434f70-dcd9-43b6-8ecb-c9d25814f2ac)(label(j))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         162c3d12-f88f-4e0e-8d24-55545c7d8775)(content(Whitespace\" \
         \"))))(Tile((id \
         0e6e0e39-8317-4814-8855-dc561142db12)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         bbe62515-2fae-4c92-901a-e97052a2bd53)(content(Whitespace\" \
         \"))))(Tile((id \
         5d4a9162-a981-428a-bf04-3c5fff3b3b00)(label(col))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         03cca742-c5f9-4c57-8807-1a087aa45edf)(content(Whitespace\" \
         \")))))((Secondary((id \
         dedd4745-134d-41be-b8b6-b328207a9f56)(content(Whitespace\" \
         \"))))(Tile((id \
         987d0021-fd72-4dad-8904-44fca35c5c44)(label(emoji))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         60fb4234-4c13-4f6b-b235-46854bbf2faa)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         3f32641b-ace3-41a8-b39b-4b693695c8ac)(content(Whitespace\" \
         \"))))(Tile((id \
         0b17c6d1-b911-4799-9a87-bb3eb5f162b3)(label(c))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         6db44910-abf1-4897-b3a5-ba2e3f666ca5)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         4364c7d3-cdfd-4bb1-87ef-c5c8a493652a)(content(Whitespace\" \
         \"))))(Tile((id \
         42930b20-1f07-42fd-a082-b79be6f3683a)(label(r))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         25678a57-2ded-45a5-8b17-85005dc9a543)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         8252a6c4-14f5-4956-b266-2aabc9f1698f)(content(Whitespace\"\\n\"))))(Secondary((id \
         e16432b0-40ac-413f-9cbc-69f1e999f2f7)(content(Whitespace\"\\n\"))))(Tile((id \
         a3e4a28b-2685-45df-95db-aeffb1a1abfe)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         0818a294-91d0-4067-afe1-bdca97e89042)(content(Whitespace\" \
         \"))))(Tile((id \
         4290ff61-9ce5-49a9-b5fa-437376f22b50)(label(setRow))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         488ea67e-7aab-4840-bfbd-995983bb8756)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 24))(sort Pat))((shape(Concave \
         24))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         80e0c1b5-6ab8-4fa7-b688-83899d57401e)(content(Whitespace\" \
         \"))))(Tile((id \
         ca7e921a-87ca-40e1-be12-0571041847b1)(label(\"(\"\")\"))(mold((out \
         Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0 1))(children(((Tile((id \
         1b6a1783-32bd-4913-8322-7897d2ecb076)(label(Canvas))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         64fbc50f-d64a-487d-82c9-74655a47a069)(label(,))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 44))(sort Typ))((shape(Concave \
         44))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         c6f51b6b-0f00-4fdc-9f29-062a7ac70242)(content(Whitespace\" \
         \"))))(Tile((id \
         502a53ff-8389-410c-a840-7f7112271d05)(label(Row))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         79e56f19-98e3-4798-8fb0-59f10d885524)(label(,))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 44))(sort Typ))((shape(Concave \
         44))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         fa15690b-5ecc-4175-885a-6c79c44eb0ee)(content(Whitespace\" \
         \"))))(Tile((id \
         6091173c-dfc3-49bc-8910-63ea63076b12)(label(Emoji))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children()))))))))(Secondary((id \
         3748906a-db09-45ec-84da-19ccc19605ac)(content(Whitespace\" \
         \"))))(Tile((id \
         31a2d999-1587-43ae-82d5-421f6931134e)(label(->))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 13))(sort Typ))((shape(Concave \
         13))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         d6364f17-7f0e-4a21-bafd-f881b2b03623)(content(Whitespace\" \
         \"))))(Tile((id \
         4a39f2ef-8a9d-4505-be8d-4abc12ed01e3)(label(Canvas))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         bcbad1ad-f179-484a-9150-dbd85b23e33f)(content(Whitespace\" \
         \")))))((Secondary((id \
         764ce473-fa7e-4532-b91f-8890148ba64d)(content(Whitespace\"\\n\"))))(Tile((id \
         25c4ba57-91a8-4d1b-a4fd-38ad034aafc8)(label(fun ->))(mold((out \
         Exp)(in_(Pat))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         37))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         f1f1a375-48ef-497f-8b20-cab934109037)(content(Whitespace\" \
         \"))))(Tile((id \
         247abfc7-837f-4915-87b5-4507322fe9c9)(label(canvas))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         d20af2c4-a63c-44dd-b0ae-7305dab93aa2)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 44))(sort Pat))((shape(Concave \
         44))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         bfdc200a-2e39-4fcc-ba52-02e72d854519)(content(Whitespace\" \
         \"))))(Tile((id \
         027b22cf-ecc9-4b19-9cec-ca1b1a57bbfa)(label(targetRow))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         0b0837ce-8956-4f4c-a381-a447f0b8c3e0)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 44))(sort Pat))((shape(Concave \
         44))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         d9d1e6aa-077e-4f40-b59a-c415299aff82)(content(Whitespace\" \
         \"))))(Tile((id \
         60c8146e-074d-4a55-b00f-8b9b5f0a4b11)(label(emoji))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         a488d259-a8d4-4cb4-8526-4560fad72f7e)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         146bef99-29d0-4df8-bbbc-84e99419a895)(content(Whitespace\"\\n\"))))(Tile((id \
         c6226677-10bd-466d-82b3-92a5f05c7de8)(label(mapi))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         af5edab0-9f6d-489b-8bdd-b803d234231c)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         38619226-f425-4cf3-a097-5ffed8fe3296)(label(canvas))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         35d09dcf-be74-4523-aa10-38d08509a050)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         8fb987d7-9039-4c45-9362-3c5d5f8f0f47)(content(Whitespace\" \
         \"))))(Tile((id fd32129a-adcf-49de-88a2-ad2d834e75c7)(label(fun \
         ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 37))(sort Exp))))))(shards(0 \
         1))(children(((Secondary((id \
         34d05b06-6a88-4336-bb11-4aa96067a785)(content(Whitespace\" \
         \"))))(Tile((id \
         10ed63ca-5551-4efe-869a-6a57ed2e31fd)(label(\"(\"\")\"))(mold((out \
         Pat)(in_(Pat))(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0 1))(children(((Tile((id \
         2d602e71-4f6f-4aca-be5c-d218f747f204)(label(i))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         b914da9e-6d84-4a5e-a33e-dd62e3bdd988)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 44))(sort Pat))((shape(Concave \
         44))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         ea4cedd1-dd17-42c8-8bb2-26c19a746799)(content(Whitespace\" \
         \"))))(Tile((id \
         64d95502-a9fb-4979-8da5-702777a9c5f5)(label(row))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children()))))))))(Secondary((id \
         87a63758-530a-41a3-9085-cce9895603e5)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         72bd579d-4b26-4624-bb03-7ae427f6bfe4)(content(Whitespace\"\\n\"))))(Tile((id \
         5bb02ae5-d896-4c3c-8916-d84bb124c4a2)(label(if then else))(mold((out \
         Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         36))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         0149eaeb-907e-4668-809b-820c4569de0c)(content(Whitespace\" \
         \"))))(Tile((id \
         9cecb250-1e1c-426b-a51b-38a148de81b3)(label(i))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         11bfc6b1-9e62-43e3-9150-679dd7095af7)(content(Whitespace\" \
         \"))))(Tile((id \
         e16ad001-53b7-4171-92a8-e6d2d11f4c13)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         93fa31d5-765a-45c8-a4ae-1df654adbed0)(content(Whitespace\" \
         \"))))(Tile((id \
         9f879200-609f-43db-bc37-2771bea2a1c2)(label(targetRow))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         8f83377d-41ae-47e0-8494-e8cb90820173)(content(Whitespace\"\\n\")))))((Secondary((id \
         effb05ac-34b2-43f1-85bd-45d6e9289d80)(content(Whitespace\" \
         \"))))(Tile((id \
         2b4d5b36-534f-4b2d-84c0-5f28d2584d76)(label(map))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         abccd5d7-a3ce-406f-b0d3-4296a40aae2c)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         1d79cbcd-df8e-4de7-a668-53e88def95d9)(label(row))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         cc0568fe-a5fa-43de-8dae-f7337019817c)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         badac455-266e-4050-bfdb-34e7f98e8f61)(content(Whitespace\" \
         \"))))(Tile((id a9408880-2157-4742-889f-a46eaea12783)(label(fun \
         ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 37))(sort Exp))))))(shards(0 \
         1))(children(((Secondary((id \
         6d52df17-1ef0-49a2-8051-3118d2ab1f50)(content(Whitespace\" \
         \"))))(Tile((id \
         b93fe507-856c-4233-89f8-f80b960b1cc9)(label(_))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         2e57f01a-206f-41b9-85a1-1c2aef6bc2c2)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         dfe3f2d4-1043-470d-b0e8-b68c5516db70)(content(Whitespace\" \
         \"))))(Tile((id \
         07d2fc5f-1475-4585-b636-1c103180afa5)(label(emoji))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         484b721f-8949-49dc-924a-f1595bdea195)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         9b6fefcf-22c1-4e96-bcbc-4af1376f3bb5)(content(Whitespace\" \
         \"))))(Tile((id \
         6cd5a926-6fb5-4ab4-a1a9-fde2ef05e602)(label(row))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         41a0f664-81f6-4e59-8b14-e4cab23bc475)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         8e200662-c86c-40bb-b2e5-ef5bd5c614c3)(content(Whitespace\"\\n\"))))(Secondary((id \
         e7e5016f-039e-43aa-8c99-a6212be733b3)(content(Whitespace\"\\n\"))))(Secondary((id \
         18ce800d-d789-47bd-a0ed-b0a854b793b0)(content(Comment\"# TODO: Add \
         setCol helper here #\"))))(Secondary((id \
         0598eda2-7445-4097-982d-8149d1204ba0)(content(Whitespace\"\\n\"))))(Secondary((id \
         361e57fd-3d8d-4a95-a878-a96cf8958abd)(content(Comment\"# Hint: You \
         need to modify each row, changing #\"))))(Secondary((id \
         c5b10a04-8e5f-4269-91f6-fd083ae58cf3)(content(Whitespace\"\\n\"))))(Secondary((id \
         8a626460-be9d-45a3-a7af-ee025304fe44)(content(Comment\"# only the \
         cell at the target column.         #\"))))(Secondary((id \
         850b6dfb-2e6b-4e84-905b-ff28b6061fd8)(content(Whitespace\"\\n\"))))(Secondary((id \
         cc7c47eb-97b0-4898-bd91-2eef631053a8)(content(Whitespace\"\\n\"))))(Tile((id \
         8c468070-5890-45c2-9548-8abc3f149761)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         009b1725-1b31-4ef1-9f46-a379b80dcb06)(content(Whitespace\" \
         \"))))(Tile((id \
         fc3090a3-c28f-49a8-8915-30a2a7d0e2cf)(label(setAll))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         2ebbb875-4bc1-473e-ab54-d7f149f18aca)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 24))(sort Pat))((shape(Concave \
         24))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         d5dba656-53a0-4486-bbdd-e3e8a7e05ae1)(content(Whitespace\" \
         \"))))(Tile((id \
         e4fd4328-8a5e-46e3-aae6-a1959a39f7e7)(label(\"(\"\")\"))(mold((out \
         Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0 1))(children(((Tile((id \
         c1b00a26-18b6-4a1e-92fa-12eb228e3c29)(label(Canvas))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         dcc6c08b-5a0a-4536-a905-eea8f0222268)(label(,))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 44))(sort Typ))((shape(Concave \
         44))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         75af9f11-7367-4563-9c75-6b91a46c7d61)(content(Whitespace\" \
         \"))))(Tile((id \
         b0d7b9fe-e3ba-417e-b960-eac5cab43e6d)(label(Emoji))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children()))))))))(Secondary((id \
         460a7b97-d067-43fc-9e82-3dd81c87bc32)(content(Whitespace\" \
         \"))))(Tile((id \
         f3664df7-c2cd-4b70-8517-3e0450072acb)(label(->))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 13))(sort Typ))((shape(Concave \
         13))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         a62a25e5-b7e6-4f3c-bc56-c8644c2fe9cc)(content(Whitespace\" \
         \"))))(Tile((id \
         1ca25f8b-a28e-4958-9a87-752f370e0599)(label(Canvas))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         25e749e6-6b59-4f3a-a0be-3260fb085fa8)(content(Whitespace\" \
         \")))))((Secondary((id \
         c7782e86-9714-4753-9349-1d991830a6e5)(content(Whitespace\"\\n\"))))(Tile((id \
         0bcb514b-827e-40c3-a47d-747528518bd2)(label(fun ->))(mold((out \
         Exp)(in_(Pat))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         37))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         af113c6a-f822-4cda-a918-77696f91d4ed)(content(Whitespace\" \
         \"))))(Tile((id \
         efd2b148-8860-4b23-b938-e29d1223158e)(label(\"(\"\")\"))(mold((out \
         Pat)(in_(Pat))(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0 1))(children(((Tile((id \
         7d482068-c9e4-4158-9817-8cd344d059fd)(label(canvas))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         8bee1a92-4c13-4035-b43b-d0010084e0c3)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 44))(sort Pat))((shape(Concave \
         44))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         778b2b61-5000-415f-a68a-359b66940144)(content(Whitespace\" \
         \"))))(Tile((id \
         ae1e26a4-6ba2-44e8-a64a-3a553d7e8ef2)(label(emoji))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children()))))))))(Secondary((id \
         65856940-9aa8-450f-b296-79b804e15e60)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         b2cb9bce-4e2e-4540-a819-3072968c6882)(content(Whitespace\"\\n\"))))(Tile((id \
         d036b04a-0f0c-41a8-ab61-e52b4ed1f255)(label(map))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         68d8a3a1-c46a-4ba9-90a0-28202bddf3ce)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         353f8cf0-5f6b-43ed-8f5d-367aadd22424)(label(canvas))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         ec1112a0-fdb8-4f0b-8a7e-2d09120d0dd4)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         f62101ea-01f2-4ee2-a67a-b09805830c58)(content(Whitespace\" \
         \"))))(Tile((id 75c5d9f4-5f40-42ab-be1f-ebf8c2b1068b)(label(fun \
         ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 37))(sort Exp))))))(shards(0 \
         1))(children(((Secondary((id \
         d9257a54-c5a9-4b00-a3c6-dadb3fe631d3)(content(Whitespace\" \
         \"))))(Tile((id \
         5ec468a9-622d-4a11-b404-d0cca0a4ec07)(label(row))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         1a5cf98a-cc58-41ae-a4a9-3deebc2999d9)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         080630f0-70b2-4557-885f-fe41f661710a)(content(Whitespace\" \
         \"))))(Tile((id \
         270e0be2-4420-406b-af43-594ffc92b79f)(label(map))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         1fe7aa09-881b-4c55-b3b9-e0cfbbe749d2)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         a5397d5e-c703-460c-bc37-ca30214503ee)(label(row))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         537bf7f2-a403-41e4-96f1-aba6a9293c43)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         3ca0d5cb-ad0a-453b-be2f-21f924f2bf67)(content(Whitespace\" \
         \"))))(Tile((id dee39682-8f2e-491c-b097-b1da92e6b68a)(label(fun \
         ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 37))(sort Exp))))))(shards(0 \
         1))(children(((Secondary((id \
         d811c086-37b1-4a5c-bf88-1d94b4c5c9ec)(content(Whitespace\" \
         \"))))(Tile((id \
         30d5fb8e-6d9a-46f0-b1a5-f024ca69f9ab)(label(_))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         3341731f-fd5b-4b0c-8762-a56f72781e97)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         0942a839-16f5-47f1-b7d6-da3e4148a679)(content(Whitespace\" \
         \"))))(Tile((id \
         97f4c456-e82b-4975-a1f2-7aa96a4aa686)(label(emoji))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))))))))))))(Secondary((id \
         98076267-d210-49a4-a837-60104ab9cd7c)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         2259e030-c4e7-4dd4-9afd-64f9dfbf014d)(content(Whitespace\"\\n\"))))(Secondary((id \
         3e7467c7-a46b-4704-9d6a-a325c6afef25)(content(Whitespace\"\\n\"))))(Tile((id \
         984f1862-ef2f-429e-abe7-876e9087ad94)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         cb1b81b4-7b72-46fa-b6d9-30f9f8047384)(content(Whitespace\" \
         \"))))(Tile((id \
         0ffb38bd-331c-4a68-bdf9-3b3c8412bb79)(label(updateGrid))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         c88cc4e5-6a95-4948-a9bb-32286bebfc83)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 24))(sort Pat))((shape(Concave \
         24))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         d41470f5-fd57-4063-99d5-d9d2979f38c9)(content(Whitespace\" \
         \"))))(Tile((id \
         118dd5e9-af8c-4d8e-8780-a509063c61d3)(label(\"(\"\")\"))(mold((out \
         Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0 1))(children(((Tile((id \
         1f68693b-03a9-48f2-8b02-364c6ba4386a)(label(Model))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         3169c632-644e-4e1c-8f11-7c25cb50a6ee)(label(,))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 44))(sort Typ))((shape(Concave \
         44))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         fa2e7ea2-619a-4871-b785-86b74363793d)(content(Whitespace\" \
         \"))))(Tile((id \
         4205e1a1-7014-4fc7-829f-c49c8c203e6e)(label(Canvas))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         c424cb1f-aa7c-435b-b8ec-5c8c6259f1b1)(content(Whitespace\" \
         \"))))(Tile((id \
         9cf8cd7b-0334-4451-86f2-befb082bc82c)(label(->))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 13))(sort Typ))((shape(Concave \
         13))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         892667e3-3246-45f9-be93-da9c93b8e3b5)(content(Whitespace\" \
         \"))))(Tile((id \
         3a7372e7-0970-480d-a414-7da03a2ab34c)(label(Canvas))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children()))))))))(Secondary((id \
         ec0b4d63-6e87-4fa5-82a2-93204aa09092)(content(Whitespace\" \
         \"))))(Tile((id \
         aee4addb-228a-471a-8808-75ac3bfb11bc)(label(->))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 13))(sort Typ))((shape(Concave \
         13))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         aa087991-41ee-43c4-8b0e-a0f9a6fbd191)(content(Whitespace\" \
         \"))))(Tile((id \
         6ed7a7a1-a467-43d2-8262-127437a663cf)(label(Model))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         b296f245-0656-4fb4-9aa5-2596f4f46af6)(content(Whitespace\" \
         \")))))((Secondary((id \
         db5de06c-d78c-4756-bef2-5e5b87eb1505)(content(Whitespace\"\\n\"))))(Tile((id \
         869f1cd6-8987-4e11-a294-d6da5d1077ac)(label(fun ->))(mold((out \
         Exp)(in_(Pat))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         37))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         d06ca398-a412-473c-ad2f-31492995ce23)(content(Whitespace\" \
         \"))))(Tile((id \
         97d6fcbc-3bea-41e1-8249-8d91b06d3b41)(label(\"(\"\")\"))(mold((out \
         Pat)(in_(Pat))(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0 1))(children(((Tile((id \
         e2de8916-2f73-402b-ab41-af6162a15b66)(label(m))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         b9c12918-7610-45be-bd87-974d14174f97)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 44))(sort Pat))((shape(Concave \
         44))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         19ba5852-7f22-43c7-986c-dbee5096c4c7)(content(Whitespace\" \
         \"))))(Tile((id \
         3b47309f-2dc4-4a2b-aa09-a28bca7fcf46)(label(f))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children()))))))))(Secondary((id \
         999535c2-810c-4a65-aca0-e94ef2c5d31f)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         6cd2ab19-d46a-4741-b89c-4d22cde698f2)(content(Whitespace\" \
         \"))))(Tile((id \
         d8bc0946-4271-4a32-9980-7f466c743eeb)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         9dfc033f-7c05-419d-b208-84289c6bf615)(label(f))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         6624bc3e-e281-4f5b-ab77-11177bcde78e)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         cffe0901-9849-47ba-b02a-78834382f2ac)(label(m))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         50383869-a58a-472e-9432-af1e8c94a5f8)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         7f45c1a0-e373-4988-89d1-ad4f605cd580)(label(canvas))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         7ce292fd-5056-4bb3-ba63-842da8f7c9ef)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         927b077d-50c1-4ad5-af14-6db0a696d707)(content(Whitespace\" \
         \"))))(Tile((id \
         36850651-52a9-40cf-b64f-f10fa76d15ea)(label(m))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         819d4372-68c7-49e0-bcc2-4bd594b5936e)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         36ef2eda-32e1-494b-be13-182733581d05)(label(brush))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         50b96b45-b8b0-4cb3-986f-83b78928ef00)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         1f130ecf-2e15-477e-81da-74ecb047ae7c)(content(Whitespace\" \
         \"))))(Tile((id \
         0ad0a60e-0d8a-4020-b116-9a884cf9695a)(label(m))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         28c2bf2a-9958-464c-a496-8f0b6da89ac3)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         3f48edd0-fab0-4506-8fa2-b66c3f77d53a)(label(palette))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         e4987b30-bfb1-4d03-8ac8-4fa3c7b699ee)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         3dc20298-4739-4388-a223-8e53ac8faf89)(content(Whitespace\"\\n\"))))(Secondary((id \
         eab55fad-3f68-4ef0-95f5-cf526f4ab7dd)(content(Whitespace\"\\n\"))))(Tile((id \
         3317ac60-fcb3-4cc6-8973-7f53d9600066)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         68880b1d-b63b-417b-88c0-22282d96af2d)(content(Whitespace\" \
         \"))))(Tile((id \
         ce6f9a86-9790-4779-9241-803724d7492d)(label(update))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         791f1f22-e6ad-4845-a2d3-718f58e875f7)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 24))(sort Pat))((shape(Concave \
         24))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         ba35e2b2-9d3e-4459-95a6-5df0ca033d1f)(content(Whitespace\" \
         \"))))(Tile((id \
         9f5fa7b3-0d5e-4b70-9f98-5f9da2f20b45)(label(\"(\"\")\"))(mold((out \
         Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0 1))(children(((Tile((id \
         e351290a-1e3d-405e-8e67-b45d305f2c4b)(label(Model))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         577fd321-ec16-426e-8dd1-f2ed907cc61d)(label(,))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 44))(sort Typ))((shape(Concave \
         44))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         a219852f-ee63-490e-b0be-e221673ab3c8)(content(Whitespace\" \
         \"))))(Tile((id \
         5fae1629-0a37-40a7-9c13-ab7ca7c966f9)(label(Action))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children()))))))))(Secondary((id \
         7aece1a9-ca7e-4827-949a-6cc79ee0945d)(content(Whitespace\" \
         \"))))(Tile((id \
         86272199-71a8-4cb0-af4c-eb97fe305d5e)(label(->))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 13))(sort Typ))((shape(Concave \
         13))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         21b5c57d-691a-4e3a-8536-6b96a7ea5dd5)(content(Whitespace\" \
         \"))))(Tile((id \
         a4b0ebe9-e8ab-41dc-a3b8-7a3f55a56543)(label(Model))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         c793d087-7ad7-46bc-a635-865ba0491174)(content(Whitespace\" \
         \")))))((Secondary((id \
         1ba3ca62-3115-4a4f-b412-064e046a1347)(content(Whitespace\"\\n\"))))(Tile((id \
         5a0332af-b183-4e3b-bc5d-7b57aa0e5bfb)(label(fun ->))(mold((out \
         Exp)(in_(Pat))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         37))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         221dd86c-03d1-4d83-b5e6-ff539f8e223d)(content(Whitespace\" \
         \"))))(Tile((id \
         a6cfcc66-a040-4e56-a9ae-74832a50bf41)(label(m))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         1638863a-ccda-4c5c-9f37-67bbabc412f6)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 44))(sort Pat))((shape(Concave \
         44))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         367928a0-27dc-4199-ae56-f7f34152c3d0)(content(Whitespace\" \
         \"))))(Tile((id \
         7a0a6179-d2cf-47f7-97a5-8b1cb200cf52)(label(action))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         92a3270e-8944-4d43-b5e4-b5beae8e63fe)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         4bf91e6e-2e23-4a95-80a8-7d4463b78578)(content(Whitespace\"\\n\"))))(Tile((id \
         8851aa61-b7b8-4a46-8384-d085949958d5)(label(case end))(mold((out \
         Exp)(in_(Rul))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         a2272a8a-79e6-4e71-bf99-504d7f5e72d1)(content(Whitespace\" \
         \"))))(Tile((id \
         0c63d779-c904-4f82-878b-51775c2100a1)(label(action))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         78b93849-1631-4665-8f24-c22fe9edf5df)(content(Whitespace\"\\n\"))))(Tile((id \
         29195ca7-9bb3-4991-960c-c3fad156d4be)(label(| =>))(mold((out \
         Rul)(in_(Pat))(nibs(((shape(Concave 46))(sort Exp))((shape(Concave \
         46))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         1d0d6bbb-bae3-4970-a34c-63e8b6862bba)(content(Whitespace\" \
         \"))))(Tile((id \
         b03523dd-3afc-4f1e-9a97-4b113c53bf73)(label(SetBrush))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         217f1b4a-a28f-494b-8761-c19518a6ff35)(label(\"(\"\")\"))(mold((out \
         Pat)(in_(Pat))(nibs(((shape(Concave 23))(sort Pat))((shape \
         Convex)(sort Pat))))))(shards(0 1))(children(((Tile((id \
         b3756e62-a59c-4cc7-922c-20dc9ec12130)(label(idx))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children()))))))))(Secondary((id \
         1eeff1be-adf4-4dbb-8549-51d69101f8ce)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         0664b824-4cee-4932-a2da-21381e4eb3cd)(content(Whitespace\"\\n\"))))(Tile((id \
         b75b1508-7737-4e79-a51a-0b56d6283520)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         953be753-c02a-4677-b747-c3a59442f170)(label(m))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         74f9123f-3411-45be-80ee-4cb7d0c4f042)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         89febf37-62c7-45d9-8b00-317739357e4d)(label(canvas))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         5581f9d4-9ae2-4017-9453-ca6d0856e2ea)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         c0b9ee90-b5d2-4749-b6c9-158b0f12a9a2)(content(Whitespace\" \
         \"))))(Tile((id \
         b67522a9-6763-4c00-a116-08111a971a04)(label(nth))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         9c68db8c-6b9b-4771-b855-330400d21776)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         3092ac42-d89f-42c1-90f3-bf629eaa5998)(label(m))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         d4cc5cf3-fa62-411f-a3a1-02b79d09d493)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         3729e135-62e4-4345-b547-6e2b6263ecff)(label(palette))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         0a5ffe36-645d-4134-836f-3c88c4b9d0db)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         2ad77beb-e8d1-42bf-8df4-dac473352f70)(content(Whitespace\" \
         \"))))(Tile((id \
         5befa917-3618-4476-a55f-5a2098384005)(label(idx))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         65b8754b-ce94-47db-8e85-d49c8eba36e1)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         e5af8477-a491-4ab7-9512-74fef57daa95)(content(Whitespace\" \
         \"))))(Tile((id \
         ddcd36f1-28b0-4915-9c9b-7676f13526c9)(label(m))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         76284f2a-6598-4903-8224-dddfc2e12ad4)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         3b625f87-7c46-44f3-b21b-f207f3899df1)(label(palette))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         2ec7cf94-1f62-4424-a399-7fc9489395a5)(content(Whitespace\"\\n\"))))(Tile((id \
         0113a585-181f-486c-82c3-878ab5d63376)(label(| =>))(mold((out \
         Rul)(in_(Pat))(nibs(((shape(Concave 46))(sort Exp))((shape(Concave \
         46))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         ccba7136-8397-4d27-ad01-c73ba6ccfedd)(content(Whitespace\" \
         \"))))(Tile((id \
         54ea656d-172e-4e33-b0dc-c5f4117b9009)(label(PaintCell))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         51675993-23d8-437b-af1b-b8443e4cec6c)(label(\"(\"\")\"))(mold((out \
         Pat)(in_(Pat))(nibs(((shape(Concave 23))(sort Pat))((shape \
         Convex)(sort Pat))))))(shards(0 1))(children(((Tile((id \
         2700aba3-6bb7-4f46-a2e8-2db965efe18e)(label(row))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         bae6e488-bff0-499e-83f1-8ba44b272cb6)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 44))(sort Pat))((shape(Concave \
         44))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         2b304c15-74f2-4112-8ea3-426adbca5aae)(content(Whitespace\" \
         \"))))(Tile((id \
         c029c3c5-b0a0-4ad8-aa73-6f413d7c5bfb)(label(col))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children()))))))))(Secondary((id \
         fb9df211-05e3-4157-807a-90eb02534e22)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         f0354851-b641-4408-b6e7-022b03de9f15)(content(Whitespace\"\\n\"))))(Tile((id \
         1e3abc6c-83f4-45d8-bcb4-242d38bd73a7)(label(updateGrid))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         6c48be7f-a331-4c3e-9e30-68084428e061)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         770b22b6-9665-46bd-b4a8-a93a57b0f0bc)(label(m))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         c888c161-d27c-4f92-9ade-ad989cdd8bdc)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         cf48397c-7b8c-4e05-9e39-5d09173312ec)(content(Whitespace\" \
         \"))))(Tile((id 570e2115-495d-451b-95ff-2f1c54ec61f4)(label(fun \
         ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 37))(sort Exp))))))(shards(0 \
         1))(children(((Secondary((id \
         93080af0-9bf1-4c80-81d2-49ebb68ccf5f)(content(Whitespace\" \
         \"))))(Tile((id \
         ca444484-6561-456b-95c6-48291711b03b)(label(c))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         98f30545-d33d-4ced-8860-8492790d4d1d)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         75242b20-9054-4014-9275-d2675290afa8)(content(Whitespace\" \
         \"))))(Tile((id \
         44d025cb-62d3-424c-b12f-74f171a457f5)(label(setCell))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         5e01134e-ea37-434a-8413-f450013ded38)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         2633a563-c3be-4bcc-8f44-37773447e1ff)(label(c))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         11856f19-40e1-4cfa-b0bb-437b366713e4)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         d6810a74-8a58-472f-94ae-46a377f8e81c)(content(Whitespace\" \
         \"))))(Tile((id \
         1931aa9f-20bc-469b-841a-420bbdd7222c)(label(row))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         4132b18a-f86d-4868-a6d4-9e8c3c0226ff)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         73ab8a2d-9989-4b92-bfeb-d6fae62f6ef0)(content(Whitespace\" \
         \"))))(Tile((id \
         d45b95f1-d0ae-4bfa-8d53-6c8ebd023b15)(label(col))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         f3d90e72-e842-4272-b783-11ea16c30cad)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         41667567-bcf9-4d3e-b864-8a84e21f57b1)(content(Whitespace\" \
         \"))))(Tile((id \
         dfac80d6-5c14-4731-80ee-7e5534e23a9b)(label(m))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         06b6c28a-dde3-412e-9e7a-cc6c98baa8a4)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         88bf7f57-1b5b-449b-aa68-344ff03d0bef)(label(brush))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))))))))))))(Secondary((id \
         fb170b6c-5067-42a9-85de-db2e282d8122)(content(Whitespace\"\\n\"))))(Tile((id \
         f224aff0-7c37-434b-9f88-812adf8f59e4)(label(| =>))(mold((out \
         Rul)(in_(Pat))(nibs(((shape(Concave 46))(sort Exp))((shape(Concave \
         46))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         dc811851-03ad-4ce0-8a09-7f0ec73c9d50)(content(Whitespace\" \
         \"))))(Tile((id \
         b324b643-0ba6-4dff-8082-d3600ce80c46)(label(ClearCell))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         c1c49646-d026-4735-ab1c-79c894586311)(label(\"(\"\")\"))(mold((out \
         Pat)(in_(Pat))(nibs(((shape(Concave 23))(sort Pat))((shape \
         Convex)(sort Pat))))))(shards(0 1))(children(((Tile((id \
         34aa84e9-c277-44f7-8f40-9f4f4a732d5e)(label(row))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         aa7b0691-93fa-4a5b-92c0-539c7f94ede1)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 44))(sort Pat))((shape(Concave \
         44))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         21e69b42-b3d3-4a9d-81d2-978b82782ab3)(content(Whitespace\" \
         \"))))(Tile((id \
         c8069003-e921-4d37-b33e-356ed125b271)(label(col))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children()))))))))(Secondary((id \
         11db8f8c-8694-4c0b-8c22-211f6286b38f)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         35c4728b-bbcc-4433-911d-065da774f2d7)(content(Whitespace\"\\n\"))))(Tile((id \
         e1021ab9-0e39-441c-8cf6-1b6aeaefe69f)(label(updateGrid))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         68dd7b9d-8b4e-48c2-9349-d5f2a7f90b7e)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         58e09fbf-a924-4c31-b3a6-26d5ecd222eb)(label(m))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         3fceb831-13ac-483e-b89d-2374dd5bed15)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         c4018128-63cb-49ce-b840-9fd7ef0b1b2b)(content(Whitespace\" \
         \"))))(Tile((id de7f9e30-f8a1-414d-a30c-99dc73322286)(label(fun \
         ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 37))(sort Exp))))))(shards(0 \
         1))(children(((Secondary((id \
         7179b36d-7318-4efd-b7ec-f31e45389295)(content(Whitespace\" \
         \"))))(Tile((id \
         ee3e4bf1-2bd3-498e-92e2-9d5ecc1035ea)(label(c))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         a71ea0e0-f448-4cbf-ad5f-9c6b9be91830)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         66c39f2f-15c8-4862-b06a-62d244a69d8b)(content(Whitespace\" \
         \"))))(Tile((id \
         97ec285f-6901-468a-9a66-bb5593e2ea21)(label(setCell))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         b67d6895-a459-4d81-8231-0e088017dd5d)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         02f3cc0d-0508-4324-9507-9aea0fcaeefb)(label(c))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         b0047561-f33f-4543-b14e-2c954dc5777c)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         1a4db850-83b5-45da-9e42-d25e2f901fd7)(content(Whitespace\" \
         \"))))(Tile((id \
         0367c1f0-38f2-41fe-ba58-45b151632975)(label(row))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         4429c909-de18-4763-90cd-9416cf42eb46)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         d0d0f285-e90c-4eb9-a288-5f1a0a4ed52a)(content(Whitespace\" \
         \"))))(Tile((id \
         2feec5c0-a9ea-49a0-b921-8c4cffd63ad1)(label(col))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         61e010e7-3f39-46aa-948e-33ee27c6a81d)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         d461d26f-68e2-4940-96ee-e14a4ce069bb)(content(Whitespace\" \
         \"))))(Tile((id \
         ffd9a9ef-1acf-453c-8a05-4f1eca680bb0)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))))))))))))(Secondary((id \
         23bbfb7d-1b16-41bd-993b-6803a594b828)(content(Whitespace\"\\n\"))))(Tile((id \
         afee64b2-9787-4458-b864-c3417a22fcf2)(label(| =>))(mold((out \
         Rul)(in_(Pat))(nibs(((shape(Concave 46))(sort Exp))((shape(Concave \
         46))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         177ef350-141a-4dae-bcf5-e2ceb396c4be)(content(Whitespace\" \
         \"))))(Tile((id \
         920f4f1c-10c8-46d0-a88c-c67c7e3f7385)(label(ClearGrid))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         a7b25643-6d2a-437f-a002-6087cdbf7ef8)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         940780bf-2258-4782-aa7d-d0ccd610ecee)(content(Whitespace\"\\n\"))))(Tile((id \
         43dcebdf-3121-4b55-aa2e-1815778984f2)(label(updateGrid))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         17c6a8b6-dd41-49fc-8e12-cb8ce8060291)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         e56ff30d-9890-4fa6-84fc-f80f7273a698)(label(m))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         f6199ea2-aebf-4fc9-819a-041e89949ddf)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         0f35f5fd-30d1-401b-a42e-bea782fa96b2)(content(Whitespace\" \
         \"))))(Tile((id 62e3d50e-3cd7-49f9-a6da-d84273f89ce0)(label(fun \
         ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 37))(sort Exp))))))(shards(0 \
         1))(children(((Secondary((id \
         12407954-3488-4773-aaf0-d84699b79849)(content(Whitespace\" \
         \"))))(Tile((id \
         edc5fb93-491a-4bc5-b080-a34c998a49b4)(label(c))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         04272d23-1a7b-4bb9-a509-a597c29043e2)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         d01c9601-9109-489b-9715-1d6bcf6fe5e7)(content(Whitespace\" \
         \"))))(Tile((id \
         9bf7b3c7-9df5-49a1-a2d7-9977c6a137e1)(label(setAll))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         74d31593-5806-47cf-af05-96700738de0c)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         af6c1ea5-0567-443f-8019-c5ee9560403a)(label(c))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         e882d758-9137-4bba-b575-e348783e6891)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         cc0ad035-1b91-437f-97d4-d983807458a9)(content(Whitespace\" \
         \"))))(Tile((id \
         e1e2fa99-a763-49b8-803a-6a6de3a25ff5)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))))))))))))(Secondary((id \
         286506f8-6f40-4bbf-9568-c1ccc1b7b046)(content(Whitespace\"\\n\"))))(Tile((id \
         d01bdf60-a09e-4397-9238-8d0269115445)(label(| =>))(mold((out \
         Rul)(in_(Pat))(nibs(((shape(Concave 46))(sort Exp))((shape(Concave \
         46))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         ea75e9ca-ab45-4083-a538-525dc679de43)(content(Whitespace\" \
         \"))))(Tile((id \
         3e40cc2e-d399-488c-9a2e-5f1deab21086)(label(PaintRow))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         cef9c343-91a6-4861-831a-611a8b750980)(label(\"(\"\")\"))(mold((out \
         Pat)(in_(Pat))(nibs(((shape(Concave 23))(sort Pat))((shape \
         Convex)(sort Pat))))))(shards(0 1))(children(((Tile((id \
         fb192ffa-8b62-4bd1-9221-5411e9c219f0)(label(row))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children()))))))))(Secondary((id \
         0a247653-a80d-41e9-a3c3-d56f1d08574a)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         fa661f70-441c-49cb-96b8-c1dc79071f2f)(content(Whitespace\"\\n\"))))(Tile((id \
         f222c873-9086-4471-97e4-685c4dbfa550)(label(updateGrid))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         e88abcea-893a-442b-a362-7d27bb0f269a)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         bfe0a0ff-1a10-4da3-a52a-782b0dfcb203)(label(m))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         076fdf70-50ac-4561-b81d-d4576ee118e6)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         64887075-13d1-48ee-9dfa-c5c2667932d1)(content(Whitespace\" \
         \"))))(Tile((id e5e15eee-e0ec-400d-91a3-aee2ea361046)(label(fun \
         ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 37))(sort Exp))))))(shards(0 \
         1))(children(((Secondary((id \
         73337a28-8687-4bf4-a578-61ae6fd53786)(content(Whitespace\" \
         \"))))(Tile((id \
         7baa3a63-ea7b-4e50-9e8c-ef053ece8472)(label(c))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         9dbb9002-8e36-4a59-bd8f-f30154a918e4)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         d47e436f-e921-49bd-a0c1-e0848a905ef8)(content(Whitespace\" \
         \"))))(Tile((id \
         ebf6b7b0-99a7-4186-a230-2f7ee422c52a)(label(setRow))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         dd0123e2-9a64-4303-a74e-98d26daf57f8)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         0abc38e5-4ac9-4a27-a88d-b9dfd05d9776)(label(c))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         fbee7486-d5af-4c2b-8bd2-5f5e70b9074e)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         0c1ed9df-caf5-48c2-b3b6-1b92f39211df)(content(Whitespace\" \
         \"))))(Tile((id \
         bb1d4287-4b37-457c-9162-ff781a108890)(label(row))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         8c301e65-1043-4e91-9683-be82537b666c)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         d0176a53-321f-4de6-aafa-d295187ca52c)(content(Whitespace\" \
         \"))))(Tile((id \
         1fc5a1d1-0733-4d63-b031-b470334dcba3)(label(m))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         098b0185-50bd-4557-b53e-18d45a1bc93e)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         76c748d8-1719-42cd-9579-49a8dceba1b0)(label(brush))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))))))))))))(Secondary((id \
         35ce1cad-2d42-4ac7-a24c-d10ee737c62b)(content(Whitespace\"\\n\"))))(Secondary((id \
         f1998c73-0413-4ae4-9933-00a63aef880c)(content(Comment\"# TODO: Add \
         PaintCol case here #\"))))(Secondary((id \
         fecd19d3-301d-4774-b647-c429461bb2ce)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         3986ebc8-480f-48c0-a98c-163c73a8fa51)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         68da80c0-4422-4f98-b1be-5facada00760)(content(Whitespace\"\\n\"))))(Secondary((id \
         ccdd1371-4d84-49ff-9b8b-58675df29a74)(content(Whitespace\"\\n\"))))(Tile((id \
         0d0ad8fd-c543-473e-b8aa-93b1fe20d8e7)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         0dd0efc4-314f-42ee-b413-ab9ef8b2a4e8)(content(Whitespace\" \
         \"))))(Tile((id \
         a5c1519a-00b8-4bdb-b27c-435a566ca96f)(label(do))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         e8dabcc3-32ee-4383-9a7d-229894bec0ba)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 24))(sort Pat))((shape(Concave \
         24))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         3399a1b0-4df0-44b6-96f6-5fef4cb8f1b0)(content(Whitespace\" \
         \"))))(Tile((id \
         2a89f2f0-ed36-4a15-ae4f-80643981dd45)(label(\"(\"\")\"))(mold((out \
         Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0 1))(children(((Tile((id \
         7c63d36e-8f58-40aa-96e6-d23bb70763c3)(label(Model))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         3d3a22ba-08b8-44d2-ad1f-6854809084da)(label(,))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 44))(sort Typ))((shape(Concave \
         44))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         7f93b5b6-c4c8-4966-b74e-cdd6d1b498a1)(content(Whitespace\" \
         \"))))(Tile((id 4d5e8517-ef5e-41c9-a3c0-fadafce8c4ef)(label([ \
         ]))(mold((out Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape \
         Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
         ba982132-2367-43f8-a038-d2918e9ff847)(label(Action))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))))))))))))(Secondary((id \
         85a75a11-98a2-42bc-849a-271a9bd3c24c)(content(Whitespace\" \
         \"))))(Tile((id \
         a7425ad3-c3ae-48ba-a298-b9bbbc02c3bf)(label(->))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 13))(sort Typ))((shape(Concave \
         13))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         38c4f07e-e92d-4ca3-95d2-8d00c711cebf)(content(Whitespace\" \
         \"))))(Tile((id \
         2838d059-4df9-485e-9acd-7a851108ce82)(label(Model))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         85183d4b-4d8f-47c5-8277-3aaa3319ffda)(content(Whitespace\" \
         \")))))((Secondary((id \
         c5853270-e699-4f71-8f11-8eb47b3efa21)(content(Whitespace\"\\n\"))))(Tile((id \
         429b9343-8bbf-44ba-a2b8-1f489c5152e3)(label(fun ->))(mold((out \
         Exp)(in_(Pat))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         37))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         96812e91-275d-4b75-9b80-7e0d6ee95742)(content(Whitespace\" \
         \"))))(Tile((id \
         1b9a3d04-a254-43ec-80cf-ce447e47665e)(label(\"(\"\")\"))(mold((out \
         Pat)(in_(Pat))(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0 1))(children(((Tile((id \
         d5159bcc-7c02-4952-a37c-781eec455fb0)(label(init))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         3d8b7776-93e7-4739-846a-8b493d2cb120)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 24))(sort Pat))((shape(Concave \
         24))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         d7b6c237-0fd4-44d7-a2bf-c9f92645a518)(content(Whitespace\" \
         \"))))(Tile((id \
         af532352-2ae0-46b5-8609-5dba4c6b8b34)(label(Model))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         9ee3fde0-70d8-4145-969d-74b8e913c745)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 44))(sort Pat))((shape(Concave \
         44))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         08d05e5b-f48f-43f1-9908-4da50ae73f6e)(content(Whitespace\" \
         \"))))(Tile((id \
         e1ec1f0c-7495-48f6-8e6d-ad9dcb5db4f6)(label(actions))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         f7868138-48a2-4ca5-b7f2-ff7167bc5d87)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 24))(sort Pat))((shape(Concave \
         24))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         c6eb59cc-78f9-4c47-a201-8a72e84cebca)(content(Whitespace\" \
         \"))))(Tile((id c73905dc-d2cf-43f3-95a0-f04b98e33c34)(label([ \
         ]))(mold((out Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape \
         Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
         9a2355e5-71b2-4373-bfc4-11bd189f606b)(label(Action))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))))))))))))(Secondary((id \
         4991fae3-79b2-4e87-a246-c6a11e9b21c8)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         14c2645c-42ac-4f53-a3a4-0d5b015cc1f7)(content(Whitespace\"\\n\"))))(Tile((id \
         40cb3b7a-500b-46a9-9217-d78427ace5b2)(label(fold_left))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         38e545d1-1e9c-4155-8226-98096699bc7f)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         4efb907d-5d2b-4131-8807-2c706e6f1afb)(label(actions))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         3e17ce2d-d78c-41f1-96dd-00da97847b6a)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         3486555d-084d-4638-8fac-7680cb64f88d)(content(Whitespace\" \
         \"))))(Tile((id \
         dc8e1903-8c21-4111-bd8f-27ea273817c8)(label(update))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         14f272e8-cfef-4d50-9713-1baf76bf1ee3)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         83e5266f-479f-488c-ad26-521174a69196)(content(Whitespace\" \
         \"))))(Tile((id \
         2797b354-3159-4649-9e33-96d32356a8dc)(label(init))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         3ab21452-7548-4d65-8af8-54bf98aa8047)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         6840335c-d73e-459f-b2ab-52c75f15998c)(content(Whitespace\"\\n\"))))(Secondary((id \
         4192f7d7-1de8-4d63-9889-fe208fbd3ebe)(content(Whitespace\"\\n\"))))(Secondary((id \
         e526244e-1831-4e38-b22c-7cc1b863616b)(content(Comment\"# Existing \
         tests #\"))))(Secondary((id \
         249c0e7c-5e5d-4550-959b-579db11a46a8)(content(Whitespace\"\\n\"))))(Tile((id \
         c0b8aefc-39ef-4902-aa90-cae706930cfa)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         8a3b5461-c203-4129-8e36-4145ee326bea)(content(Whitespace\"\\n\"))))(Tile((id \
         8a4303c9-afae-46f3-8d8f-489694667222)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         841d6eae-0d46-4e8c-adde-b80f385fb595)(content(Whitespace\" \
         \"))))(Tile((id \
         c1c2ee1c-544c-4df1-b6cf-43a8f69a3e79)(label(m))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         2fdb2faf-861f-4c0a-b48d-abf1258977c2)(content(Whitespace\" \
         \")))))((Secondary((id \
         e792ef39-6d4c-4aab-a2a7-695fe1e83bad)(content(Whitespace\" \
         \"))))(Tile((id \
         42f62bba-f904-419d-90d4-d72996c207ac)(label(update))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         3afd8218-50ac-4edd-9bb3-3cd55b6ae131)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         a23ec216-d0a6-4053-99fa-d3284eacc335)(label(init))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         581fa8d0-e3f9-4464-8a91-f76e448a8136)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         8128fc1f-5013-45ca-9eda-a2fa50ed2fbc)(content(Whitespace\" \
         \"))))(Tile((id \
         8bab2ba2-10f8-4cd7-9bdc-726dbb099fd1)(label(PaintRow))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         28f856da-9eed-46e2-b50e-839e9385607e)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         f4fc82d3-8adf-40e4-866e-cf4cc0b8b520)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))))))))))))(Secondary((id \
         eccb852b-f28b-4112-8f4f-6cda41961768)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         1d1ac85f-ab45-41d3-8bcf-959d8afd4856)(content(Whitespace\"\\n\"))))(Tile((id \
         dcc2ebd9-ae78-4ea5-a13a-c50c2b2416e9)(label(m))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         135321fa-115d-4eed-b38e-071e83192727)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         10dc8418-62f4-451f-8b03-d29fc821c15d)(label(canvas))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         7aa18d10-59a7-473f-ba27-daeb35fa579f)(content(Whitespace\" \
         \"))))(Tile((id \
         55ecafb0-f84f-4983-9700-4f380e61276a)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         606ec5cc-382c-4f39-9be6-80b78f78ef9f)(content(Whitespace\" \
         \"))))(Tile((id ff0e474d-d28c-488a-85af-2b18e4a323fd)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         2ea2c687-6096-47ec-976a-2bc1b82c7dc8)(label([ ]))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         d78fa625-4b46-4d0b-ad15-56d22dcf53b4)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         936a21b1-fef9-4aa2-9286-8beca637b770)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         e1025264-ddb3-4bce-8344-402d98d9c565)(content(Whitespace\" \
         \"))))(Tile((id \
         7ed04d64-ff8a-44dd-babf-95324333d5cb)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         943f5eb5-18be-48f5-be85-114b794714c2)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         d836f175-eb42-4dd0-b63c-d57b359d4b20)(content(Whitespace\" \
         \"))))(Tile((id \
         9a4ea019-d24d-405b-94a9-f806561c82a6)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         0ea7b21f-965f-4d6d-994d-7bf5c0100c8c)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         03075fae-f480-4598-834e-0d52766d34fb)(content(Whitespace\" \
         \"))))(Tile((id e7e2cdab-4efd-462b-b394-841ace50dd15)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         140f6a19-1534-47f7-90de-39b22599c8a0)(label(\"\\\"\\240\\159\\142\\168\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         f73b43c6-a853-46b6-a89d-a604c84b54b7)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         6bab3f39-3b6a-4faa-8bec-5318a963548a)(content(Whitespace\" \
         \"))))(Tile((id \
         a0b059d1-a746-439c-9e4c-5a0e6a12ea5a)(label(\"\\\"\\240\\159\\142\\168\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         8f6efb8f-b515-4b2f-a7c9-59ba4b0cdabf)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         e2998e90-70ab-415c-b321-4e3012a1fb16)(content(Whitespace\" \
         \"))))(Tile((id \
         461d3051-666c-4e1c-89d7-1fc029ab3553)(label(\"\\\"\\240\\159\\142\\168\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         3e0ed1c4-e5d5-4d26-9682-ef9ef6923bf6)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         1973bf13-4277-4b2b-b7e2-e8aa2ae35c3a)(content(Whitespace\" \
         \"))))(Tile((id 3e12bdcf-7bec-40f9-8bc3-0b80317926a8)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         d63fe5f3-8133-46a7-b78a-f90938480924)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         95fc4e89-6497-4d4e-a6fa-2f1805fb4a43)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         23feb029-9fef-4c63-949e-bd2f5f4b708d)(content(Whitespace\" \
         \"))))(Tile((id \
         bef2ca24-4afe-4922-a200-1e6ac5c1ac58)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         7d854eb0-f73a-4923-a0a5-1ef0ccd7706f)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         790526f4-59dd-4755-b7b6-b57041f098d4)(content(Whitespace\" \
         \"))))(Tile((id \
         91be97e4-8a78-449f-beac-9dada573e4ea)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))))))))))))(Secondary((id \
         8be1fa09-b2bd-446b-b11b-69e6a4c96ec3)(content(Whitespace\"\\n\")))))))))(Tile((id \
         a501633e-9afe-459e-ba11-9c500ee348f7)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         e044f025-21b4-406a-82ef-5f404295c0ca)(content(Whitespace\"\\n\"))))(Secondary((id \
         f6d2d274-fa4c-48f5-958e-193784c9f00d)(content(Whitespace\"\\n\"))))(Secondary((id \
         fae3c046-b7b5-4fb6-a109-d3d2d2e3d93d)(content(Comment\"# New tests \
         for PaintCol #\"))))(Secondary((id \
         61d83c60-c580-485e-b2d1-67b102f297f3)(content(Whitespace\"\\n\"))))(Tile((id \
         e17d9c5b-7bbd-417f-8792-58e625ab3b90)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         440b451b-4b30-4a38-9586-05a434a28409)(content(Whitespace\"\\n\"))))(Tile((id \
         7804194a-e962-4c09-b8e8-08e88b034a1a)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         b24701db-2e9a-4cc9-947d-41ba06291652)(content(Whitespace\" \
         \"))))(Tile((id \
         7818feb3-08c0-4733-93bc-d3213eec37f1)(label(m))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         94aa1f59-adc6-45f7-b88a-4e0cf9d2b481)(content(Whitespace\" \
         \")))))((Secondary((id \
         1767b29f-e28a-40ec-9d5b-d6468e6fb446)(content(Whitespace\" \
         \"))))(Tile((id \
         66b01ffe-2132-482c-81cf-698cf583b0a8)(label(update))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         758091b3-7a1c-43a2-af51-c2deab3b3c66)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         6a75a888-ee1e-431c-962b-54fbcba03a92)(label(init))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         3742c525-ccc0-4de0-977e-27b7c1c5d8ab)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         722da1fb-0b8a-4a2d-8005-6ac2da7232ce)(content(Whitespace\" \
         \"))))(Tile((id \
         66da15cb-d0ee-442d-9f71-4b2b8371a343)(label(PaintCol))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         ce5e24a1-553a-43de-8e78-35ed46b79ca8)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         ce044860-c428-4442-8640-62ee208792d6)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))))))))))))(Secondary((id \
         6f1ef72e-c0f2-46fb-b077-426577504f75)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         ec8347aa-28b4-40ce-ac2d-1a97fa0122f9)(content(Whitespace\"\\n\"))))(Tile((id \
         e18ae93e-eb0f-4793-9dad-748f0eca284a)(label(m))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         0d2bc085-6696-437e-9141-a06ec5deefce)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         4f0bb64a-cdf0-43d4-bd03-eb6575e130e2)(label(canvas))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         18cf7bd0-d809-43b3-8e7d-5d026f314913)(content(Whitespace\" \
         \"))))(Tile((id \
         592b766f-11b5-4bd8-882f-f976f4b15ea4)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         d940cb4b-095d-4a12-9a97-f0041650cba6)(content(Whitespace\" \
         \"))))(Tile((id 08bb44b6-ac60-4aa1-baac-4d62a35c1baf)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         d28702eb-61e8-44ea-849d-06fc5bb43a25)(label([ ]))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         bfaf599f-4ba2-49d6-8e43-459401537cca)(label(\"\\\"\\240\\159\\142\\168\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         5abfa602-5e2c-4dcd-b31b-b01752f75d81)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         0d9493ee-cd7d-41ab-be1d-bdcee4645924)(content(Whitespace\" \
         \"))))(Tile((id \
         bd83e0b0-4e6f-4ea4-bdd6-11367fcd2e26)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         ec9c3ce5-25c6-4622-82f2-e4e2866ca2ba)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         27a789b5-df07-4d19-bc6a-43903e54618e)(content(Whitespace\" \
         \"))))(Tile((id \
         6c2aeaca-eac6-403e-a863-6a22209e94a8)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         9e86ed21-645f-47b2-9459-77e5e74173b9)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         7e0cf5c2-a652-47f1-807a-8ced652fb86f)(content(Whitespace\" \
         \"))))(Tile((id 42bf6f6d-d82f-46e7-998a-2138776cc024)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         9e50d327-f6fd-46cd-8380-4718c41821fd)(label(\"\\\"\\240\\159\\142\\168\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         d3cfdb35-dcf8-4034-9538-d63291616318)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         b4a4e4c0-a527-45bb-98aa-4da313d591b8)(content(Whitespace\" \
         \"))))(Tile((id \
         cab6de3a-1262-418f-9ae6-4f5190d937a1)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         ba3f6811-41ec-4bfc-97bf-f282a2c71b99)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         36e25575-8c55-495d-92db-623fc362cba1)(content(Whitespace\" \
         \"))))(Tile((id \
         e09677b6-f63c-43c8-ae3f-31e0423da678)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         7594e36e-b573-425d-8487-1035bc8a8181)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         b42b0533-cd43-4d3e-9826-0a3883540467)(content(Whitespace\" \
         \"))))(Tile((id 20c0bb95-884a-4d69-ab87-2adec28f9c9e)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         055403fc-6eb7-424b-bb6d-2467699ffbf4)(label(\"\\\"\\240\\159\\142\\168\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         8f170c8c-24c5-41f0-bd5e-eeca920bbb4e)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         1a582f80-5526-497e-9e40-5c36d299347c)(content(Whitespace\" \
         \"))))(Tile((id \
         e746f0b3-59f5-4fc3-a6d1-c06135a33af6)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         f4968ef1-14af-47b2-b6d9-3922fd60aab3)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         793e685a-8aa7-419a-b0ad-ea307f42033a)(content(Whitespace\" \
         \"))))(Tile((id \
         283cc674-0fcb-4d8f-840c-391636ee9fa7)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))))))))))))(Secondary((id \
         f17e39c7-0573-4344-b7a9-c69144756993)(content(Whitespace\"\\n\")))))))))(Tile((id \
         811c1ebf-4751-4098-a394-89b31b9fed70)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         8a05aec5-a95f-45bb-96e7-6b4b96c27a66)(content(Whitespace\"\\n\"))))(Secondary((id \
         f9537d0d-2aa7-4f2f-b1b6-e40ce8f5defd)(content(Whitespace\"\\n\"))))(Tile((id \
         251caea5-6ba3-48d9-9893-e4051fc47cd2)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         3691dd81-6fa8-44c7-bb17-a416724351a5)(content(Whitespace\"\\n\"))))(Tile((id \
         1f06b12d-f14b-4484-9d86-8b5a8f9b7dca)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         c0c2dc4e-b8c5-46f1-9d0d-6073deb607c9)(content(Whitespace\" \
         \"))))(Tile((id \
         12e26a20-27a2-4631-8de3-eacaf3687c20)(label(m))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         5f7c37d9-f686-4fa6-96f2-640190118ccf)(content(Whitespace\" \
         \")))))((Secondary((id \
         c0e1c4a0-37dc-4fb0-8bde-47f9a5815450)(content(Whitespace\" \
         \"))))(Tile((id \
         0ea4972b-1313-4da3-9503-d4293e1baf6b)(label(update))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         d577208e-cfa8-4e2d-96c1-5e2dc0e35953)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         e36156ad-06a4-47b7-8e92-357dea7e3aa5)(label(init))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         0f41c80e-c70d-4300-ae81-a4450f5f10da)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         f6be7880-39ab-40a6-808b-b159e85aed33)(content(Whitespace\" \
         \"))))(Tile((id \
         8ed01c35-022d-4aef-9d46-94f7688a7197)(label(PaintCol))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         fb064b0a-5356-4351-8f6f-dd2cfa84fdcf)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         df85d1bf-bbf2-4e45-a82b-df6e92d27c5a)(label(2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))))))))))))(Secondary((id \
         175c37ba-6818-4cdb-a440-f20da7069ae5)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         7e12bae6-0297-44dd-ac68-8af9df15011d)(content(Whitespace\"\\n\"))))(Tile((id \
         f4c483df-92dc-4f25-ac30-72ff952992ff)(label(m))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         9f0f3a25-ce3b-4995-adff-92ee973afa54)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         388d4884-ce9f-44f9-a09e-2f84b34f8bee)(label(canvas))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         ea3e1ac1-6efc-4e1d-8ed8-f6de6ece2d5b)(content(Whitespace\" \
         \"))))(Tile((id \
         f6f4557c-bbf3-4871-88d8-caaedcf8437f)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         8fde2353-df7b-4ec2-b0c0-db924163173b)(content(Whitespace\" \
         \"))))(Tile((id e0102c46-f22e-4cae-a13f-96f6d692de67)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         354a6616-1305-4018-888a-576c207efca8)(label([ ]))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         f84f2369-85e0-4155-978e-7294b31e150e)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         6215e082-f3e9-431b-8b9d-bdcb5650bbfb)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         a0e0f16a-df1a-4738-957a-c10e05fc3392)(content(Whitespace\" \
         \"))))(Tile((id \
         037679bb-3c8b-43a9-b96b-680280a66b07)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         8848679b-e749-4405-bd53-3172ead905a2)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         518bf078-f70a-4659-9a65-d07bc7f9e580)(content(Whitespace\" \
         \"))))(Tile((id \
         a38eb0c6-7a7c-4e0d-aecc-0f1a1baf5dbc)(label(\"\\\"\\240\\159\\142\\168\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         6b6efdf1-2baa-4ce0-b9af-33d3dfd6ff05)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         d89bff63-b241-4226-b69f-35785e62cce3)(content(Whitespace\" \
         \"))))(Tile((id d3e1aa4a-7a64-491a-b87f-dd212994882c)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         2491192f-966d-4ddc-8f26-c02e508321ec)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         f235e705-da18-4916-afd4-fd914e1b5860)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         f3ff46bb-b45e-45ef-8b27-2fbf27f18c4b)(content(Whitespace\" \
         \"))))(Tile((id \
         ec98e651-bdec-4bdd-9511-b6bdb3ef4797)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         e8a72b4c-cdf8-456c-b72c-119ebcd34b0c)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         589bfde3-b0e0-43fa-961f-7c01014f3bd0)(content(Whitespace\" \
         \"))))(Tile((id \
         eb97d2bf-0fb0-4779-9e76-34f43ff5d380)(label(\"\\\"\\240\\159\\142\\168\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         1082188d-3380-4a68-9513-0b9008983f68)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         add7bee1-0cf2-49a8-941f-6173d052c685)(content(Whitespace\" \
         \"))))(Tile((id 448c4f38-28c3-4a41-95bb-a1645d70a998)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         fa83ea55-9bb5-424c-ae6e-7f88d72926a8)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         d28844a2-c46e-4161-83ff-3823b8979425)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         60c05b64-eabd-4521-a67d-7b11d9c0e0c6)(content(Whitespace\" \
         \"))))(Tile((id \
         88c14e8b-255b-4f79-bc00-b4511451d8d1)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         d3840539-3d4d-49bc-a6b9-985a371a367a)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         3b6b7f7e-4717-45db-a847-1cce95e2d746)(content(Whitespace\" \
         \"))))(Tile((id \
         b96ae453-a05f-4f8b-aca9-b8fb6052b6cf)(label(\"\\\"\\240\\159\\142\\168\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))))))))))))(Secondary((id \
         569ef5c8-a520-452f-8bf8-ec430b244b95)(content(Whitespace\"\\n\")))))))))(Tile((id \
         2466a6c3-9ac6-4568-a034-2826a389b1cb)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         3760cb90-0dc8-45c4-91cf-1df6bafae343)(content(Whitespace\"\\n\"))))(Secondary((id \
         1723ce35-ef55-483d-b556-2e82294b2bd6)(content(Whitespace\"\\n\"))))(Tile((id \
         59a28046-68e8-42df-9fbd-c08a76221ddc)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         d759c167-1ba4-4b73-aef0-a89896ad293f)(content(Whitespace\"\\n\"))))(Tile((id \
         73bda26e-4a82-4f17-980a-64fde3ae3d37)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         3fe0ca07-38d7-4e9f-937c-20a8661f726d)(content(Whitespace\" \
         \"))))(Tile((id \
         d5e0fe50-2d77-450f-aec1-b1c5cd1a9575)(label(m))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         c0cb3ff4-4d25-4814-b222-b54bac5fdd87)(content(Whitespace\" \
         \")))))((Secondary((id \
         dc17ee45-17cd-48c1-83fa-73e0434680bf)(content(Whitespace\" \
         \"))))(Tile((id \
         bdccc7ec-414d-428e-b24b-b99245e145d2)(label(do))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         28f9f027-a5fa-4b4d-8496-cb53e5ce8494)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         7ce0c2b6-54d8-423f-8d83-4de01225852c)(label(init))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         9820414d-0bd9-47d6-bb4c-f96899e53a8c)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         6ab97826-55bb-4892-9b34-5be131fb81a4)(content(Whitespace\" \
         \"))))(Tile((id bfd3a062-42d8-4741-86cf-01cd252550fa)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         2c60abfb-82b2-4ee9-a69d-2a647d60018d)(label(PaintRow))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         363b6185-7339-4217-89a7-417bc2021ae0)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         7f999c48-fbb8-4b36-a1d2-5501fc7a699d)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         c3d6f066-0667-49a3-932e-db44c5cf909c)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         7e57b5af-58a6-4e81-b685-d8d03460160b)(content(Whitespace\" \
         \"))))(Tile((id \
         3e4bab91-968c-46ad-a6e5-f0bfab7cc8a1)(label(PaintCol))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         01f11a5d-cf1d-4162-bad1-12a9729cbaeb)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         97d0bce3-b7a0-4979-95e5-37a7027ab1eb)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))))))))))))(Secondary((id \
         2e0d589c-9a7d-4017-9a2b-dc07bc12960f)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         eeced871-9c47-4113-8299-8f09dedf6c00)(content(Whitespace\"\\n\"))))(Tile((id \
         9c8ae660-d2f0-4d7b-baee-a6c22a8a6222)(label(m))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         6258d80f-7b90-4601-8fc2-2ff400c37540)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         646a0e8d-ee1c-4679-bf58-48253b166419)(label(canvas))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         a5b569f7-3bc2-4a99-bcd7-82d559028ce9)(content(Whitespace\" \
         \"))))(Tile((id \
         24811ade-3f6b-4ab2-9705-3581b968da5d)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         d87a9449-1e00-4974-b13b-2b60cc83e50c)(content(Whitespace\" \
         \"))))(Tile((id b1741949-aff9-4560-99c9-1b131410c481)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         ffd2da89-f753-4b45-aa0a-62250d71542c)(label([ ]))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         8a38c14e-fb0c-48e1-8cfa-09ddafc4bf1e)(label(\"\\\"\\240\\159\\142\\168\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         48b5b360-c096-4d66-95d6-6bc9626086b7)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         55e8210b-a493-4f3f-b1ee-2670df004532)(content(Whitespace\" \
         \"))))(Tile((id \
         d94ca161-be3f-4683-98b7-0a7a837e85f1)(label(\"\\\"\\240\\159\\142\\168\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         98686500-aaf9-4cae-b2f2-728f04eca608)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         2351eb69-025b-442c-80dc-f58ec75cf431)(content(Whitespace\" \
         \"))))(Tile((id \
         0ac93870-0691-4846-b467-2e09c902243e)(label(\"\\\"\\240\\159\\142\\168\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         b095b9cf-ec6e-4b19-8134-2b3062ad8967)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         880002e6-72ae-4ca9-8bdc-341b3bb4fec4)(content(Whitespace\" \
         \"))))(Tile((id 8ce52a92-b6f3-45e4-8ea3-a7fd47ecadcf)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         c4741adc-2441-41c6-96ab-c42ebe750f77)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         14296cf0-1db6-4cda-9c01-c3ced27976d1)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         bf685ae2-3148-449c-8e37-24b074ac0168)(content(Whitespace\" \
         \"))))(Tile((id \
         4b2b4c1c-d5eb-4965-8931-ac781e8d3379)(label(\"\\\"\\240\\159\\142\\168\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         23d4e442-bbfb-47d1-8b57-6e69ab0c17c1)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         f4584b23-fcc8-499f-b1c0-015d0b2c986b)(content(Whitespace\" \
         \"))))(Tile((id \
         49ae5d9d-6c0f-4e11-ae37-993f9f06a132)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         e9cfa091-c77c-43e9-bab2-2eacd945a823)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         29896cf4-22de-438b-a428-a5553c4e2a40)(content(Whitespace\" \
         \"))))(Tile((id 040d1439-27d6-45e8-845a-31d5cff4c5f2)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         7c147ca2-da68-4039-96dd-ee5a65ffdbae)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         ecff2796-2633-4129-80c5-489d9d981b0a)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         0efcf204-1c6f-404b-97bf-f4511586efc1)(content(Whitespace\" \
         \"))))(Tile((id \
         114c9f84-7740-478a-8594-64d6362cb196)(label(\"\\\"\\240\\159\\142\\168\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         2c766899-6765-4005-b726-652672617b87)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         4e7ef317-c814-4a89-a3e9-c338ebadbdc8)(content(Whitespace\" \
         \"))))(Tile((id \
         90385206-b318-4295-b3ae-ea166b1c523e)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))))))))))))(Secondary((id \
         41cd9250-e260-4850-b0b6-645e7cc929a6)(content(Whitespace\"\\n\")))))))))(Tile((id \
         485372c8-529b-4da5-8157-4f2ce4abe925)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         fa221584-c7c7-4c19-bd56-bab4e2a7f81b)(content(Whitespace\"\\n\"))))(Secondary((id \
         1eed1acf-a2ab-4672-8678-6361eb08720a)(content(Whitespace\"\\n\"))))(Tile((id \
         b59754f7-19f2-4f6f-80e2-1ac9b582988d)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         a71b0a26-e989-40a1-b958-c25985a7019d)(content(Whitespace\"\\n\"))))(Tile((id \
         c33b0b9e-fc4f-4626-9113-ca5b84ddfd08)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         c86f45f5-9036-4d52-bb50-e25a536114d8)(content(Whitespace\" \
         \"))))(Tile((id \
         0f20b408-5cdd-4dd7-910c-43eff2f037ed)(label(m))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         8b1c9f9b-de49-4fb3-9b02-d41b8fc5bac1)(content(Whitespace\" \
         \")))))((Secondary((id \
         3c43535b-923a-4329-9ee5-d9f6a0d95b07)(content(Whitespace\" \
         \"))))(Tile((id \
         e9a4c711-871c-4d5a-86ef-0f721bd6ca56)(label(do))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         8d459d2c-c137-41af-b9c9-6b2d50ff7738)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         4861c068-c2f2-48a3-bc0e-8427424d03e0)(label(init))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         35bff127-c564-437c-b71f-1971a108fc31)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         f91a0114-7c03-4e12-81ce-4f39ef08c60b)(content(Whitespace\" \
         \"))))(Tile((id 6e13950d-aac1-44fe-bad1-eee7b537d18b)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         6c6692c3-ec77-495a-83e3-8d50c404c54b)(label(SetBrush))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         dd295b1f-2436-460f-90a9-7022d0c28ddc)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         4c7ed3cb-db29-443a-ad0b-655247e5d80f)(label(2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         901bbedb-82e0-4e63-a6e3-f813b78b8263)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         dd2ce153-59d3-4313-9de6-7edbaeee02f4)(content(Whitespace\" \
         \"))))(Tile((id \
         885c462a-e4f3-4cc0-b761-ede6600e613a)(label(PaintCol))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         7934f6b0-b6a7-422b-88c9-5fe7bf47584a)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         5d18bcae-c19b-4671-8a20-5e2fd90f99bd)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))))))))))))(Secondary((id \
         77945a37-a994-4478-9036-3a58cb34dfd0)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         a52c95aa-7534-46f7-b463-add3b78d1d32)(content(Whitespace\"\\n\"))))(Tile((id \
         c306efff-aa98-4e09-8468-64cc35695e92)(label(m))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         c35f24be-4c11-4cf4-bc06-1c0fd4840e31)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         2d38bae5-3fd3-4427-8cf5-5d1552767873)(label(canvas))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         36215cd6-c394-4f0e-98b0-ffdecdbf5982)(content(Whitespace\" \
         \"))))(Tile((id \
         3b04cb93-b8fd-4351-a50b-1dd9eaf66cdf)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         38ba9312-7013-4074-b5e8-61cf6a1d19d9)(content(Whitespace\" \
         \"))))(Tile((id c1a9222d-834b-4761-b693-16de26467640)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         a899c992-fe3f-486a-a0b8-e10862b738f8)(label([ ]))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         b91719ce-a06c-4cb2-8349-258488082caf)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         eee709bc-17bf-4485-a510-d738cbbd0422)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         4253d00a-d251-4498-ab22-84b71c747c2d)(content(Whitespace\" \
         \"))))(Tile((id \
         4c08f6b1-a5e8-412f-ba63-2a05618d50a7)(label(\"\\\"\\240\\159\\146\\156\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         8e7f3212-5dfc-4916-8b56-11394d22c042)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         b6a01bbc-db26-4111-bf39-fb8b8426d999)(content(Whitespace\" \
         \"))))(Tile((id \
         f2229d4a-e2e8-4b90-9cd5-acdeb1928c5e)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         d09d6258-4a08-412c-9edf-3f19e57a77ba)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         e6c35b93-bf88-452d-8428-9de869cc4b2e)(content(Whitespace\" \
         \"))))(Tile((id 57f574ac-7fbb-438c-9732-7d9e526bd66f)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         ba774bc5-df72-46c1-83f1-1788a4a709a2)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         7605abad-5f27-4b03-af84-60110fe79e86)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         3cb1489b-09d5-43f2-bb8b-fb4a917cc10e)(content(Whitespace\" \
         \"))))(Tile((id \
         3747f622-5671-4df7-9b1c-673eed8c81b0)(label(\"\\\"\\240\\159\\146\\156\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         11cf51f2-a209-4895-96ee-f522e146f4cd)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         8975ea7e-d515-4434-92c3-4b722e6f72be)(content(Whitespace\" \
         \"))))(Tile((id \
         af6fc520-82d4-4292-a4d2-ea3a305de6f9)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         74e02915-36c3-4e8d-9cc4-bc775813580e)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         285294c3-0b0f-438c-a79a-64af94db149b)(content(Whitespace\" \
         \"))))(Tile((id 67266660-eae3-4ca7-be04-2f13e6a8eeed)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         69f06d74-db5c-4ea6-815a-4814872175f1)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         a11aef41-ae04-498e-9227-44e64485a7f0)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         45adad30-e661-4a06-9280-01f5a72bde0c)(content(Whitespace\" \
         \"))))(Tile((id \
         a2d9c3f3-e2de-4ff5-9b31-58d97593eff9)(label(\"\\\"\\240\\159\\146\\156\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         91697961-e622-4292-97df-893ac6fe4cf9)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         f43e4d10-8d79-4df2-a73d-5072d7ab6d66)(content(Whitespace\" \
         \"))))(Tile((id \
         735b39c4-1eaa-41f2-8d0f-a7de0c712a7a)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))))))))))))(Secondary((id \
         bee2d6dc-a580-4c5d-a184-94e3b05da227)(content(Whitespace\"\\n\"))))(Tile((id \
         2869154d-c8fc-48fa-8fcc-cd2611a011bd)(label(&&))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 32))(sort Exp))((shape(Concave \
         32))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         37354e30-34f2-46bf-974f-361bb4021847)(content(Whitespace\" \
         \"))))(Tile((id \
         2479b9cc-21f3-4bcd-8b1a-9a5edb324fc0)(label(m))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         94b1e73d-bd2f-4ed3-bc5d-d2303fa886f5)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         e4490b7a-84aa-4766-8a6a-ac37492f7ae9)(label(brush))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         a2b31c00-5f53-4cc3-a7d9-537425de0ebb)(content(Whitespace\" \
         \"))))(Tile((id \
         10267f59-a631-46b5-a049-b9a3df472cc6)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         2439072d-2791-40de-95e6-18748e10e5fe)(content(Whitespace\" \
         \"))))(Tile((id \
         2a2fa6ec-7192-4743-b752-e315a614670a)(label(\"\\\"\\240\\159\\146\\156\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         035f8057-f6f6-4e88-a62f-7851c532949c)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         6bfe23a7-cf6f-4989-9116-48245ece4f23)(content(Whitespace\"\\n\")))))";
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
