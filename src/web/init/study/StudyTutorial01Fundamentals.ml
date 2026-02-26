let out : string * Haz3lcore.PersistentSegment.t =
  ( "Study / tutorial / 01-fundamentals",
    {
      segment =
        "((Secondary((id \
         f2e4776c-55de-40cc-92b9-56d607deb557)(content(Comment\"# PROBES \
         TUTORIAL - PART 1: FUNDAMENTALS #\"))))(Secondary((id \
         501695e6-aabd-4bd8-b6ce-21a45f3d157c)(content(Whitespace\"\\n\"))))(Secondary((id \
         4fcf2273-5fed-4989-b54b-9cb1b1c5cbf6)(content(Comment\"# Probes show \
         you runtime values of expressions, inline in the editor. \
         #\"))))(Secondary((id \
         3aab53f4-ee4a-4270-9a3f-7d58180f8557)(content(Whitespace\"\\n\"))))(Secondary((id \
         3ea11a2d-5b30-4e9b-8c52-e570772e9fb4)(content(Whitespace\"\\n\"))))(Secondary((id \
         26386d23-95a8-4cbe-a701-bb724b7bf49f)(content(Comment\"# TRY THIS: \
         Put your cursor on the `*` in `250 * 7` below and \
         #\"))))(Secondary((id \
         fd75301d-5324-4e1d-9c7a-f9f7163ba62f)(content(Whitespace\"\\n\"))))(Secondary((id \
         7d9b191b-75cd-4007-959d-da12c0cfe404)(content(Comment\"# right-click \
         to open the context menu and select \\\"Add probe\\\". \
         #\"))))(Secondary((id \
         d9069a58-5a3e-475e-a395-a3e0608377b7)(content(Whitespace\"\\n\"))))(Secondary((id \
         5de0c413-a6cc-4976-9fe5-edfa9c1e50e6)(content(Comment\"# You should \
         see the sample value 1750 to the right. #\"))))(Secondary((id \
         425fad2b-76cb-4f98-9af6-238454db80bb)(content(Whitespace\"\\n\"))))(Secondary((id \
         552118af-829f-406c-9a6a-91a6e50cb17c)(content(Whitespace\"\\n\"))))(Secondary((id \
         1daa13b6-0df7-49f7-a4c3-fee25b32bad0)(content(Comment\"# Hazel uses \
         `let x = expr in body` to define values. #\"))))(Secondary((id \
         c3c988c0-9585-4dc7-a338-b8db907a8744)(content(Whitespace\"\\n\"))))(Tile((id \
         1a3e7a4c-8961-43eb-bfa1-1092a6180338)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         4e898d85-e6da-4863-833f-4eb4d3c348bb)(content(Whitespace\" \
         \"))))(Tile((id \
         65b28dc9-774f-4a20-8f64-42f34f12db19)(label(weekly_water))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         e6084860-ff71-43e3-bc99-db6f400e9fd3)(content(Whitespace\" \
         \")))))((Secondary((id \
         2d9d464c-58e4-414a-b234-3c41d13c3081)(content(Whitespace\" \
         \"))))(Tile((id \
         128e097d-e531-4b58-8ae0-480a7d39ad96)(label(250))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         ab730654-96f6-4099-afb4-7b4691223fc2)(content(Whitespace\" \
         \"))))(Tile((id \
         d4e413b4-736d-45aa-bfcb-79e48f7c7cb5)(label(*))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 27))(sort Exp))((shape(Concave \
         27))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         8dadb2a2-2d61-4ec1-9a96-ee475675e9bf)(content(Whitespace\" \
         \"))))(Tile((id \
         6148fb78-39de-4013-abfc-b8582fe69a3d)(label(7))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         da4793f1-916e-4a13-9303-15d17f210f0c)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         14cff3b2-b752-410c-b990-0631cf5ebb27)(content(Whitespace\"\\n\"))))(Secondary((id \
         f2ccec03-b610-4551-a81c-639ff76dc2e9)(content(Whitespace\"\\n\"))))(Secondary((id \
         e0d022d4-6216-4eca-a029-ad7353f284bb)(content(Comment\"# TRY THIS: \
         You can also press Cmd/Ctrl+E to toggle that probe. \
         #\"))))(Secondary((id \
         c425d585-d6f3-49b0-be34-9977d683d60a)(content(Whitespace\"\\n\"))))(Secondary((id \
         79f89d4c-ec43-4a3a-a26f-ffc994adf1b4)(content(Whitespace\"\\n\"))))(Secondary((id \
         4982038e-095c-4172-aa96-0556a38581e1)(content(Comment\"# You can also \
         probe variable names on the left side of a let binding. \
         #\"))))(Secondary((id \
         06df5141-ece4-4316-a9bb-5820c005c49b)(content(Whitespace\"\\n\"))))(Secondary((id \
         4a023553-bd5e-45aa-a88d-dc3dd6a7e7ad)(content(Comment\"# Try adding a \
         probe to `daily_ml` on the left of the `=` sign below. \
         #\"))))(Secondary((id \
         da4768f7-61fc-439c-9166-3332160cac98)(content(Whitespace\"\\n\"))))(Secondary((id \
         7e42e7e7-3fa5-488e-8679-5810731ac788)(content(Whitespace\"\\n\"))))(Tile((id \
         d5f5d344-e80b-4a78-a4cb-ec75c9b53900)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         76fdc748-229c-4287-abbe-7bd8b6285c6b)(content(Whitespace\" \
         \"))))(Tile((id \
         54838488-ea70-4489-af10-5fb4d99d7c2e)(label(daily_ml))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         d82f0860-f6dd-4650-a09b-eca8dbe33937)(content(Whitespace\" \
         \")))))((Secondary((id \
         4b431c11-7388-4b7f-934b-e8061afb7720)(content(Whitespace\" \
         \"))))(Tile((id \
         94bf0b64-63da-4fe8-9ed5-2d9e2c3f866d)(label(250))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         4c0642b1-cc7f-406f-afd1-8d31d36247c9)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         f9492f02-7d8a-4d4c-ac8e-431b5d171d8f)(content(Whitespace\"\\n\"))))(Tile((id \
         bcd4ddfb-dae6-4d66-b07f-904ff9802114)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         919e6615-33f5-43e3-af5b-dc3c659ad053)(content(Whitespace\" \
         \"))))(Tile((id \
         48b3452d-5f93-44a8-8d08-6f43d3cff8d3)(label(days))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         46b01458-ffcc-4b43-b0d2-43088eff5f8a)(content(Whitespace\" \
         \")))))((Secondary((id \
         89e7d620-b8d4-49e4-9715-e8d8c716e4b5)(content(Whitespace\" \
         \"))))(Tile((id \
         02639283-4d7d-49a9-99d3-6c84d5a91bb2)(label(7))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         1709f798-28aa-4288-8064-f96949042e64)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         cb379bc0-8f7a-4220-999a-8dcb5192ed0c)(content(Whitespace\"\\n\"))))(Tile((id \
         ddf89dac-26da-48de-b5b9-de18a586ba77)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         17c783a5-2fe7-478b-85ad-0f3b9abfbed6)(content(Whitespace\" \
         \"))))(Tile((id \
         aed29c8e-9490-4abd-8b0f-686601542004)(label(total))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         9166dfb1-6454-4113-bb4b-c173bde8e103)(content(Whitespace\" \
         \")))))((Secondary((id \
         05a52837-4256-483d-a0d2-0a4db500a842)(content(Whitespace\" \
         \"))))(Tile((id \
         033e1a24-11c1-4ef3-a0db-ec147d35fb2c)(label(daily_ml))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         8b993fb1-5b42-4a43-ae54-c10b4b3ef358)(content(Whitespace\" \
         \"))))(Tile((id \
         67fef9b4-cf10-4762-b44b-9f24e386f5e6)(label(*))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 27))(sort Exp))((shape(Concave \
         27))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         37a8917f-817d-4f1b-950c-1e95735f01e5)(content(Whitespace\" \
         \"))))(Tile((id \
         f58f34ef-b9da-4a8a-8be3-e782daa62c03)(label(days))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         1526b9f2-3f0b-4734-9395-c9ed8cfd7944)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         acbb9e01-b4e2-4d01-bd8f-1254888b0f5a)(content(Whitespace\"\\n\"))))(Secondary((id \
         095967d6-03b5-4b0c-93fa-be875488ea7c)(content(Whitespace\"\\n\"))))(Secondary((id \
         bdbfda64-7a4f-49be-8cb9-a10c1e7d8c1e)(content(Comment\"# Now click on \
         the sample value (1750). A dropdown shows the #\"))))(Secondary((id \
         aa64dd2a-e48b-4edc-a1c9-bbb12e6abb7d)(content(Whitespace\"\\n\"))))(Secondary((id \
         72949488-9bde-4a7f-9967-cd59eecc716e)(content(Comment\"# ENVIRONMENT: \
         the variables in the expression and their values. \
         #\"))))(Secondary((id \
         75edb07a-b77c-4b2e-b21f-827a9ec1d11c)(content(Whitespace\"\\n\"))))(Secondary((id \
         656c5229-d05a-46a7-b8e9-c908f790bc78)(content(Whitespace\"\\n\"))))(Secondary((id \
         604fd18b-bcc2-4e7c-832f-fee607703b02)(content(Comment\"# `type` \
         defines type aliases. Sum types list options with `+`: \
         #\"))))(Secondary((id \
         bd4222b8-b75e-4ffb-b095-af2d71b86fd4)(content(Whitespace\"\\n\"))))(Tile((id \
         b4f0a1f5-8da5-4ef1-b79f-915b51911b26)(label(type = in))(mold((out \
         Exp)(in_(TPat Typ))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         bf0efe67-379a-4b7f-8d68-90a825d3fd01)(content(Whitespace\" \
         \"))))(Tile((id \
         fe01c3cd-322c-4118-b882-7b35657e2e9d)(label(MoonPhase))(mold((out \
         TPat)(in_())(nibs(((shape Convex)(sort TPat))((shape Convex)(sort \
         TPat))))))(shards(0))(children())))(Secondary((id \
         6ef0d18b-880a-4312-9e19-2c33c586fc30)(content(Whitespace\" \
         \")))))((Secondary((id \
         2d62dd47-bd86-4e85-87c8-e1feedf4ec1a)(content(Whitespace\" \
         \"))))(Tile((id \
         b0cfdd14-02e9-481e-9aa5-3951a25e88aa)(label(New))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         c516ac71-0478-439f-9ef4-c3fd21def6fe)(content(Whitespace\" \
         \"))))(Tile((id \
         b649912c-198f-403a-b464-0c7d3f3a6507)(label(+))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 12))(sort Typ))((shape(Concave \
         12))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         5f12b8a1-fccd-408c-9fd3-67f2b9546fc6)(content(Whitespace\" \
         \"))))(Tile((id \
         9f88ba08-c0d1-4b54-845e-d22795d77c1e)(label(Waxing))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         9b641d81-5f64-482f-96c2-ccf57f0df8c1)(content(Whitespace\" \
         \"))))(Tile((id \
         ea10ff07-64dc-4f11-b20f-9ea5b93ada68)(label(+))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 12))(sort Typ))((shape(Concave \
         12))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         d8517daa-bdf2-466a-9587-6f82ade50d39)(content(Whitespace\" \
         \"))))(Tile((id \
         56b14304-f3e2-4f50-b1c7-84b58e7f84bc)(label(Full))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         d8f7395b-f891-46e7-91b8-7481a298ac1e)(content(Whitespace\" \
         \"))))(Tile((id \
         463b6635-0aa9-4d68-93f0-42db29798979)(label(+))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 12))(sort Typ))((shape(Concave \
         12))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         23507210-df5a-40b1-9495-c38156a15646)(content(Whitespace\" \
         \"))))(Tile((id \
         8c35fac6-6969-405a-9eba-2ddce5a7c701)(label(Waning))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         25ec4f96-24da-407c-a550-3713611342fa)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         c5ae82f8-90dd-4c23-88d0-a3b819828c81)(content(Whitespace\"\\n\"))))(Tile((id \
         5b4475ec-fc21-4350-ad11-6ca109876182)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         5478181e-691f-489b-b542-1ae07cea56ae)(content(Whitespace\" \
         \"))))(Tile((id \
         0a35ba57-e7c2-44da-87ca-64636e5fb599)(label(base_water))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         7d177187-9fe6-474e-85a5-7583a86c7383)(content(Whitespace\" \
         \")))))((Secondary((id \
         8bace817-4ca3-49f7-ac6c-bce9cacb73db)(content(Whitespace\" \
         \"))))(Tile((id \
         9606aa70-5031-4d87-a0f4-1f60ff10f228)(label(250))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         87e2f0e0-275c-4111-a0b3-6c90b49de38f)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         acbb8c58-2f48-4d09-99cc-2770123174a8)(content(Whitespace\"\\n\"))))(Secondary((id \
         4c570055-49c7-495c-80e8-0000f1f186bb)(content(Whitespace\"\\n\"))))(Secondary((id \
         1f06af33-dac6-4156-8ea2-4a55a1608d31)(content(Comment\"# Note that \
         not every expression gets evaluated! #\"))))(Secondary((id \
         e4994e94-94d7-4a31-938e-e4746c4b9d01)(content(Whitespace\"\\n\"))))(Secondary((id \
         d33c5b52-0254-40c2-930b-f99270049892)(content(Comment\"# `case` \
         pattern-matches a value against branches: #\"))))(Secondary((id \
         d88768ec-fb6f-4a67-9452-ffd32c74f698)(content(Whitespace\"\\n\"))))(Secondary((id \
         8df0fb18-9c3c-4053-ac7a-e75946bbde55)(content(Comment\"# Add probes \
         to each branch of the case below. #\"))))(Secondary((id \
         e6540a65-1429-4e53-9dff-379124413403)(content(Whitespace\"\\n\"))))(Secondary((id \
         8c6526f5-53ed-425c-bc84-d4a681ca5070)(content(Comment\"# (Add 3 \
         probes, on `+`, `-`, and `base_water`) #\"))))(Secondary((id \
         ed39350b-944c-4e4f-8f15-27c266878387)(content(Whitespace\"\\n\"))))(Secondary((id \
         6e8b8933-8902-4796-9331-ed79366c6058)(content(Whitespace\"\\n\"))))(Tile((id \
         1afa0056-0cec-4b0b-bf6c-e29970ff84e3)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         f3d48fde-c01d-4f8d-b598-9c101b2bc08e)(content(Whitespace\" \
         \"))))(Tile((id \
         7da3d27a-62b4-422c-b45d-cd12c590031f)(label(current_phase))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         339c7b9c-5674-42b7-8d83-d4ff05a56451)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 24))(sort Pat))((shape(Concave \
         24))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         695600f6-c27f-4216-b25c-7742d2ee8014)(content(Whitespace\" \
         \"))))(Tile((id \
         4efba199-b395-489d-a75c-5af531b52ab8)(label(MoonPhase))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         ab8d432b-2c5b-4c46-99a9-81f8481b7de0)(content(Whitespace\" \
         \")))))((Secondary((id \
         456d8366-f08b-4085-979f-5c65b46f75e4)(content(Whitespace\" \
         \"))))(Tile((id \
         a188c59f-0b71-437d-9e36-4cba182d1553)(label(Full))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         29fcef05-2259-4ed9-9b6c-ddb166885a88)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         c531fed2-fa1c-4e9c-9bcd-ad5844c30aa0)(content(Whitespace\"\\n\"))))(Secondary((id \
         af2711e6-fa71-4024-9b1d-41041ca22f0e)(content(Whitespace\"\\n\"))))(Tile((id \
         5c039671-26d5-4104-9e1c-1f7604e3580f)(label(case end))(mold((out \
         Exp)(in_(Rul))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         68d99990-7e09-4dee-8890-1232542b8a30)(content(Whitespace\" \
         \"))))(Tile((id \
         c3fd5d95-6ed4-47b2-b6c5-de338c7c7ca2)(label(current_phase))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         5b0faaaf-1ba1-4788-ba95-37eb0bb027ae)(content(Whitespace\"\\n\"))))(Tile((id \
         4a7071aa-2bae-4b52-be87-0b7aaf5b73bc)(label(| =>))(mold((out \
         Rul)(in_(Pat))(nibs(((shape(Concave 46))(sort Exp))((shape(Concave \
         46))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         42bf163b-6489-44a8-9170-f9949e4ddf6f)(content(Whitespace\" \
         \"))))(Tile((id \
         fdb8f87d-d53a-4d1f-8452-808d1d17ae03)(label(New))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         06e902a0-e21f-4bee-87f9-61fa2006f18c)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         8ec09a09-7513-4cc4-aac7-a7522afea7fc)(content(Whitespace\" \
         \"))))(Tile((id \
         4f7951c2-5778-4466-a7dd-9632554f557c)(label(base_water))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         bd673fe5-43b7-4654-9a61-c59c4b31b20b)(content(Whitespace\" \
         \"))))(Tile((id \
         939e5309-5117-4234-8b1b-57de94d9f674)(label(+))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 28))(sort Exp))((shape(Concave \
         28))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         26ec1b6d-cd37-4a2e-83d5-48ca30f0c880)(content(Whitespace\" \
         \"))))(Tile((id \
         2d4fb0dc-6716-4905-95b1-9ab070a0edca)(label(50))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         478008a9-334a-4f97-bc46-5caef6baa188)(content(Whitespace\"\\n\"))))(Tile((id \
         3c6081b2-76e5-4801-96d4-7bfba9229958)(label(| =>))(mold((out \
         Rul)(in_(Pat))(nibs(((shape(Concave 46))(sort Exp))((shape(Concave \
         46))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         b76c87a9-28cc-4554-9f8f-5983527dd34f)(content(Whitespace\" \
         \"))))(Tile((id \
         762eceb3-a276-4223-be5e-39cc22efbcfa)(label(Full))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         4a306a65-c480-4d0a-a91d-13785fd4408e)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         4238c48b-9c6e-4c9b-8432-2127a95bf7ba)(content(Whitespace\" \
         \"))))(Tile((id \
         8d985d29-a5f7-42fa-954d-c359c66fcb0a)(label(base_water))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         4b9e31ee-5e4a-4b84-87dd-7b2fef010429)(content(Whitespace\" \
         \"))))(Tile((id \
         5bd3b17b-c7fb-427b-ae3a-d75b314f01de)(label(-))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 28))(sort Exp))((shape(Concave \
         28))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         32176b9b-3c70-4f51-9cd8-0a09c423f32f)(content(Whitespace\" \
         \"))))(Tile((id \
         53857d74-590b-4534-a1dc-0edc386b1089)(label(30))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         cad02318-7bf2-4992-b4f7-66521c54f144)(content(Whitespace\"\\n\"))))(Tile((id \
         43bdbd6f-03a9-4ecf-93d9-9d91918e3148)(label(| =>))(mold((out \
         Rul)(in_(Pat))(nibs(((shape(Concave 46))(sort Exp))((shape(Concave \
         46))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         91c466e6-d6d2-48c7-91bf-cc0140372857)(content(Whitespace\" \
         \"))))(Tile((id \
         138ef4a2-28c8-4a5a-8899-149a902d0dac)(label(_))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         7d316dab-9ca1-4500-bf25-ff730216b959)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         80321c27-47e0-4445-9b06-4486f952bfe0)(content(Whitespace\" \
         \"))))(Tile((id \
         6b1381b5-684e-418c-828a-e43f84ca683c)(label(base_water))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         d7ab243d-bd39-4d27-b55f-6f8af7e85178)(content(Whitespace\" \
         \"))))(Secondary((id \
         2e68de9c-80cd-4fc4-8bfb-aae3d4e68ec9)(content(Whitespace\" \
         \"))))(Secondary((id \
         cd653967-7da7-49c3-8327-0d588e230e93)(content(Comment\"# _ is a \
         wildcard, matches anything #\"))))(Secondary((id \
         ee452749-150f-436a-8fe8-bd46e2e10819)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         5db816f8-770f-4090-9d3a-9e20ebdd0a2c)(content(Whitespace\"\\n\"))))(Secondary((id \
         25beebe9-18fe-46bc-bf96-a5fb799f1953)(content(Whitespace\"\\n\"))))(Secondary((id \
         57d0ad24-601d-43f5-b4ec-00df105bda0d)(content(Comment\"# Note that \
         only the matching branch shows a sample. #\"))))(Secondary((id \
         609e657a-48da-4ad6-a820-30e8d8bb6843)(content(Whitespace\"\\n\"))))(Secondary((id \
         3ec73c57-81d5-4100-bd28-e483304a7f6b)(content(Comment\"# The others \
         show a null-set icon, meaning there were #\"))))(Secondary((id \
         b9294640-c6b4-404b-887a-29d03d7419e3)(content(Whitespace\" \
         \"))))(Secondary((id \
         83da1269-ed88-4f73-89e7-253db8b40619)(content(Whitespace\"\\n\"))))(Secondary((id \
         7daaf1ab-6d79-4188-886f-70f2e42832e0)(content(Comment\"# no samples \
         collected since that code path was not taken! #\"))))(Secondary((id \
         c47bb27b-fe14-499c-9f7d-11e77fb1ab34)(content(Whitespace\"\\n\"))))(Secondary((id \
         67630284-3b15-4889-ab6d-4c4caf8dea86)(content(Whitespace\"\\n\"))))(Secondary((id \
         bd8f3cca-a32f-4629-a459-251bf6ef8c70)(content(Comment\"# Go back up \
         and try changing `Full` below to `New`. #\"))))(Secondary((id \
         158fc548-3671-4d28-a2ab-9b05ce574dc6)(content(Whitespace\"\\n\"))))(Secondary((id \
         ca50b522-c24a-4c2e-a2b8-ecf862907f8e)(content(Comment\"# Which branch \
         will light up this time? #\"))))(Secondary((id \
         65c3effb-0424-46db-b8dd-f943b4590a0d)(content(Whitespace\"\\n\"))))(Secondary((id \
         8145b338-127d-44db-873d-40ee75276374)(content(Whitespace\"\\n\"))))(Secondary((id \
         91b4638e-1c89-4491-b399-446a66c12545)(content(Comment\"# END OF PART \
         1 - Select the next slide from the top menu #\")))))";
      backup_text =
        "# PROBES TUTORIAL - PART 1: FUNDAMENTALS #\n\
         # Probes show you runtime values of expressions, inline in the \
         editor. #\n\n\
         # TRY THIS: Put your cursor on the `*` in `250 * 7` below and #\n\
         # right-click to open the context menu and select \"Add probe\". #\n\
         # You should see the sample value 1750 to the right. #\n\n\
         # Hazel uses `let x = expr in body` to define values. #\n\
         let weekly_water = 250 * 7 in\n\n\
         # TRY THIS: You can also press Cmd/Ctrl+E to toggle that probe. #\n\n\
         # You can also probe variable names on the left side of a let \
         binding. #\n\
         # Try adding a probe to `daily_ml` on the left of the `=` sign below. \
         #\n\n\
         let daily_ml = 250 in\n\
         let days = 7 in\n\
         let total = daily_ml * days in\n\n\
         # Now click on the sample value (1750). A dropdown shows the #\n\
         # ENVIRONMENT: the variables in the expression and their values. #\n\n\
         # `type` defines type aliases. Sum types list options with `+`: #\n\
         type MoonPhase = New + Waxing + Full + Waning in\n\
         let base_water = 250 in\n\n\
         # Note that not every expression gets evaluated! #\n\
         # `case` pattern-matches a value against branches: #\n\
         # Add probes to each branch of the case below. #\n\
         # (Add 3 probes, on `+`, `-`, and `base_water`) #\n\n\
         let current_phase: MoonPhase = Full in\n\n\
         case current_phase\n\
         | New => base_water + 50\n\
         | Full => base_water - 30\n\
         | _ => base_water  # _ is a wildcard, matches anything #\n\
         end\n\n\
         # Note that only the matching branch shows a sample. #\n\
         # The others show a null-set icon, meaning there were # \n\
         # no samples collected since that code path was not taken! #\n\n\
         # Go back up and try changing `Full` below to `New`. #\n\
         # Which branch will light up this time? #\n\n\
         # END OF PART 1 - Select the next slide from the top menu #";
      refractors = "()";
    } )
