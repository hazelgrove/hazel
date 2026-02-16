let out : string * Haz3lcore.PersistentSegment.t =
  ( "Study / writing / mentions / mentions-solution",
    {
      segment =
        "((Secondary((id \
         e989d7df-f8fa-4229-b0a4-05abea701782)(content(Comment\"# MENTION \
         EXTRACTOR - SOLUTION #\"))))(Secondary((id \
         acf05ebf-d38b-4a1f-a987-23d675a8350b)(content(Whitespace\"\\n\"))))(Secondary((id \
         4a420eb0-ca69-4c8a-99d4-6f9ed5df2399)(content(Whitespace\"\\n\"))))(Secondary((id \
         29ea35f7-40f8-4e34-bf67-fe676d4d32c0)(content(Comment\"# Check if a \
         word starts with @ #\"))))(Secondary((id \
         2a822259-0ae6-4b92-a319-2ddcae42261f)(content(Whitespace\"\\n\"))))(Tile((id \
         76a37ad9-e9ef-4734-8bae-1ea7ed241aac)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         7bcdbf93-c18a-45a7-ae46-f6cfb12c4425)(content(Whitespace\" \
         \"))))(Tile((id \
         f9052781-560a-4e21-9f7b-c96774c38904)(label(starts_with_at))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         933dd4c2-84e9-4045-abd0-c3177728a7bb)(content(Whitespace\" \
         \")))))((Secondary((id \
         cd096255-d75c-43f7-8d8b-aab2ec02a093)(content(Whitespace\" \
         \"))))(Tile((id 8f4b714d-0b92-4059-8f10-915827143c24)(label(fun \
         ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 37))(sort Exp))))))(shards(0 \
         1))(children(((Secondary((id \
         fd4e224a-955a-4288-b19d-d1f42c119ac8)(content(Whitespace\" \
         \"))))(Tile((id \
         e6f0f053-e46a-49af-82ed-d9ae6d09d652)(label(word))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         5e9224b8-38cf-41ec-b656-048fac1ec109)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         68265c27-6c46-4ec2-979a-e7bb747e3eef)(content(Whitespace\"\\n\"))))(Tile((id \
         ecd1b4ae-16b2-4bed-8ae5-57d989051836)(label(string_sub))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         2a294209-324a-44c0-9f86-d589d0770d7d)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         caa20f38-767f-4bb6-8d27-fde2ee78a9fb)(label(word))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         2dc293df-7db7-43b8-81db-69c338b6a252)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         9fe23c0e-179b-4770-b7f1-07393def65ee)(content(Whitespace\" \
         \"))))(Tile((id \
         85b9c9af-e065-4deb-b801-0da5b43fa294)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         71859da1-ab0f-41e3-8e51-53d6e348c2b4)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         6b896e78-ed0c-4543-8c8c-88d2b213e31e)(content(Whitespace\" \
         \"))))(Tile((id \
         303ce1ed-cefd-4928-94c8-da3caf3f8fba)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         ead17148-37d6-482b-bc3d-e94aef9e524d)(content(Whitespace\" \
         \"))))(Tile((id \
         72a2a493-5081-4a2b-a80f-7d98dcd04b5d)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         964eeedd-5571-4d30-bdfb-8698e37cc72a)(content(Whitespace\" \
         \"))))(Tile((id \
         ee1ca2b5-a555-44d7-946f-01c362cf20e9)(label(\"\\\"@\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         89df68d5-45b8-415d-94eb-90ceaf290823)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         776ad8fb-36c8-438f-95e6-9bc651388fd2)(content(Whitespace\"\\n\"))))(Secondary((id \
         a867c5b1-7c67-41bd-8caa-63b5b1b03f19)(content(Whitespace\"\\n\"))))(Secondary((id \
         c03d7957-0db1-41a8-8042-75503706f64f)(content(Comment\"# Remove the @ \
         prefix (take everything after index 0) #\"))))(Secondary((id \
         a34fc129-2efe-4898-8eae-15bd0de5beed)(content(Whitespace\"\\n\"))))(Tile((id \
         95a74664-9dc2-432c-9194-06b7bd718d71)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         c95e7a9a-f205-4b36-84ab-b1aec4393fc0)(content(Whitespace\" \
         \"))))(Tile((id \
         c5fd7334-597b-488a-be35-143e5df0ad04)(label(strip_at))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         839c263c-7b37-4643-93d8-718bb2b6194e)(content(Whitespace\" \
         \")))))((Secondary((id \
         d650a872-9011-4fb9-8ee1-145865686c29)(content(Whitespace\" \
         \"))))(Tile((id 4fca6051-789d-4a6d-805f-1c9fe91dc5e0)(label(fun \
         ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 37))(sort Exp))))))(shards(0 \
         1))(children(((Secondary((id \
         de6ca70c-87c2-489c-af7c-3b51c4de9ce3)(content(Whitespace\" \
         \"))))(Tile((id \
         4cec5bc3-dc16-48d2-955b-50017bf6da20)(label(word))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         a2e68ea0-bd10-415b-bbe1-d2625c228c65)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         1069713d-6c53-46a6-842e-26fca6251596)(content(Whitespace\"\\n\"))))(Tile((id \
         3a112163-b70f-463f-b508-fa581bcb7c9b)(label(string_sub))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         06071839-5e76-4ed5-9a2b-344a02f6904a)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         273b6479-14fe-480a-99bb-e4fd1f8d1b43)(label(word))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         d23ca285-2cb1-4455-be9f-58f9df57afd4)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         eb3a0ec1-20aa-4de4-8c99-149232a22341)(content(Whitespace\" \
         \"))))(Tile((id \
         ff8472bd-8bf6-4678-b1cd-61b82b734cee)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         75813324-ad59-4849-8557-521cc3cd90c9)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         e1b9ca4b-9c56-4932-a1d3-d4ec734cd96e)(content(Whitespace\" \
         \"))))(Tile((id \
         892fb284-d815-494f-b024-892f892e3b15)(label(string_length))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         7af5e723-e24d-4e25-8aac-f8dec1630065)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         42ccfb35-f0ec-477c-ab5f-ca7b3e4b364b)(label(word))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         a04979af-da8b-4f1d-b5c4-f027b97ddf29)(content(Whitespace\" \
         \"))))(Tile((id \
         40bd16d3-d477-4074-af3e-72112845a491)(label(-))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 28))(sort Exp))((shape(Concave \
         28))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         6f53cd12-8c0e-4bc0-aba5-2b40018f61f6)(content(Whitespace\" \
         \"))))(Tile((id \
         e87182f3-c816-446d-8d9a-7d6321a9743d)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         aa06a895-b944-48ca-9817-8ad7c479228e)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         bfeb78c4-3198-4270-9de0-dd9e54fae717)(content(Whitespace\"\\n\"))))(Secondary((id \
         00fe717f-4ccb-44e9-84ee-f76dc2af496b)(content(Whitespace\"\\n\"))))(Secondary((id \
         cc6c6822-86c1-470c-9796-a9ec5fb4c07c)(content(Comment\"# Extract \
         usernames: split -> filter -> map #\"))))(Secondary((id \
         ddf3cdfe-baf6-44a1-961c-69fcdabe03d7)(content(Whitespace\"\\n\"))))(Tile((id \
         1df4b6fa-c262-4aab-945f-6b1a8415ba4f)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         1d94290d-9fe3-4e81-ad82-a4dc8726215d)(content(Whitespace\" \
         \"))))(Tile((id \
         c1746662-6564-40dc-bde1-e32104886788)(label(extract_mentions))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         581eb7c2-6ac2-4aa6-8d69-474d0c59edda)(content(Whitespace\" \
         \")))))((Secondary((id \
         55b4d1d0-79d0-468b-9cf8-fed7e1db59a0)(content(Whitespace\" \
         \"))))(Tile((id d627c540-ebed-4392-b087-592013445b71)(label(fun \
         ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 37))(sort Exp))))))(shards(0 \
         1))(children(((Secondary((id \
         bf872bc2-36cd-4245-aa4c-8139455c72eb)(content(Whitespace\" \
         \"))))(Tile((id \
         31933692-bacd-4872-a521-d9e2a8776582)(label(message))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         add0cd2a-98cc-43c1-bf76-9c2223f2729e)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         54dc733d-1560-46d8-816b-1c225e024d04)(content(Whitespace\"\\n\"))))(Tile((id \
         af1aa66b-70fc-4ffe-9c92-a8cfcc2c627b)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         987f2469-4b73-417a-96dd-6188604753ba)(content(Whitespace\" \
         \"))))(Tile((id \
         6241cc84-59fb-48c4-8326-c9147c72d2df)(label(words))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         6281c1bb-dbca-49d9-b1c6-27d3937759ef)(content(Whitespace\" \
         \")))))((Secondary((id \
         2ad3e39a-c031-4df2-ac8f-63117b1d5a5b)(content(Whitespace\" \
         \"))))(Tile((id \
         146c73c3-bb22-41aa-a018-7f65ce73a40c)(label(string_split))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         f31ebbdb-854d-492c-8620-b5e7e6a59959)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         4207a980-6734-4b21-a161-80ce3cde8ac9)(label(\"\\\" \\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         bef2b607-2a78-4821-8e62-1360a8fedbcd)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         58224cf8-bdd3-4b2e-be15-5c9f29b60755)(content(Whitespace\" \
         \"))))(Tile((id \
         dc95848e-5278-4c96-861c-b14255379123)(label(message))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         465a0398-9e9e-40e0-84de-e7dc01d4c983)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         7026ed80-4fe1-4b29-9d66-0e44d81ce7dd)(content(Whitespace\"\\n\"))))(Tile((id \
         84db6e44-7b52-4bd8-8478-d44f98ed88d9)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         e1834627-b722-4b87-a6bf-1f2f2db8bdff)(content(Whitespace\" \
         \"))))(Tile((id \
         3c663ac2-ba89-4949-be30-f2a644d1f62f)(label(mentions))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         998a272c-d6e6-41d1-a119-b12bbc960b38)(content(Whitespace\" \
         \")))))((Secondary((id \
         42c42b2b-fdb0-47b3-8c3f-65d76d295672)(content(Whitespace\" \
         \"))))(Tile((id \
         ce9c63d0-119b-4614-b7b3-d8ccb725e6b6)(label(filter))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         740290f1-44ab-4e0e-a931-756b3f4eb096)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         5bbfb3ee-d065-4621-b758-26c2ca6f1a9f)(label(words))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         8527cf38-2f31-4a52-b8f0-add3c07d3a9e)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         43541d0c-e8f6-4273-901e-7b79b220ad41)(content(Whitespace\" \
         \"))))(Tile((id \
         80577ba3-83f5-4266-af1b-043cbeeb2cdc)(label(starts_with_at))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         4184fa85-532e-4a28-92a6-5606c01d388a)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         4edd06e6-a388-4453-97b1-e59b3ea263f4)(content(Whitespace\"\\n\"))))(Tile((id \
         5812f460-47a1-4668-9807-c4ab10ea0665)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         1fcb6488-b0b4-498f-b5e0-a1bdb320c303)(content(Whitespace\" \
         \"))))(Tile((id \
         8f365a1d-2f67-478c-9e0b-a33a85890d68)(label(usernames))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         544675e9-0d00-49b3-8645-67bd9dbe6691)(content(Whitespace\" \
         \")))))((Secondary((id \
         1b9ed7b8-727d-4843-9ef3-8c3b9c6654ef)(content(Whitespace\" \
         \"))))(Tile((id \
         4630903c-c7f1-4676-a8c7-c3f91786ba11)(label(map))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         5e2694aa-4da8-4da7-aa78-5f23273f6e53)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         6f279ac9-83d4-4e2d-8112-643b19429050)(label(mentions))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         4639047f-50b7-4451-893c-065d9d776312)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         98e414f9-99bf-4def-9aff-2b38216aca2f)(content(Whitespace\" \
         \"))))(Tile((id \
         563fbd82-a1ff-4716-b7fa-305d7b3a9341)(label(strip_at))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         c593157d-775b-4806-b45a-98f255edf101)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         d9f01042-12fe-47b3-a323-9f01eb75e85b)(content(Whitespace\"\\n\"))))(Tile((id \
         cecfcd25-54d6-41c1-af4a-de903cc22958)(label(usernames))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         1792d9e2-5c73-48f8-912a-6e15fea8a153)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         c6e489c8-2b69-423a-b124-d19731c37b3c)(content(Whitespace\"\\n\"))))(Secondary((id \
         30d3059e-dcde-4db1-8045-bcb366696d21)(content(Whitespace\"\\n\"))))(Tile((id \
         6ba0d840-60a9-40d4-8ccf-2e3a956c9a21)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         1de92d71-87fa-4238-bf2e-537404c95ea4)(content(Whitespace\"\\n\"))))(Tile((id \
         ca3f971e-6e3a-4e10-a772-d58d7cd9041a)(label(extract_mentions))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         8bc0ba1c-83c2-4624-a6c0-57aa8d8d6b50)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         9284db4c-1cb6-431d-885f-9fa5bb18ae13)(label(\"\\\"Hey \
         @alice\\\"\"))(mold((out Exp)(in_())(nibs(((shape Convex)(sort \
         Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         4fd0cf0d-7912-498e-97ab-50589b6ebc82)(content(Whitespace\"\\n\"))))(Tile((id \
         cf563eff-48c9-41a5-8294-2dc8afdf1606)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         da9ac6b4-5117-484c-ae6f-ad8e8461a5aa)(content(Whitespace\" \
         \"))))(Tile((id ef0a6b82-d5dc-403f-a5ed-fc62e8e7111b)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         675d0f15-1d57-4d19-9ca2-2cc9c70bd731)(label(\"\\\"alice\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         98dd6094-e731-43ac-b3dd-20bad21ee490)(content(Whitespace\"\\n\")))))))))(Tile((id \
         2af3ac87-2a72-466d-ba14-15d1e4ea7f23)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         b0e0c795-c9d4-43a4-aca8-769e09d56aaf)(content(Whitespace\"\\n\"))))(Secondary((id \
         9d5d9fda-1cc1-4868-9818-fce37a62a795)(content(Whitespace\"\\n\"))))(Tile((id \
         6f66556d-e6e0-4ed7-a9b7-bf4c18dc3c9e)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         20e3acde-c717-4576-9433-5c96dd62a5c5)(content(Whitespace\"\\n\"))))(Tile((id \
         29258245-1dfd-499e-907a-82604fc7dc11)(label(extract_mentions))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         8a0eebf9-6c6d-40c1-96f7-e3b36e7836e5)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         108458dd-24d8-4a04-b6cc-1b00faefecd7)(label(\"\\\"@bob @carol \
         hello\\\"\"))(mold((out Exp)(in_())(nibs(((shape Convex)(sort \
         Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         72f7df10-16a0-404a-bb3c-9a5ab3c0aa49)(content(Whitespace\"\\n\"))))(Tile((id \
         9adaa6b7-289c-4bb2-bbb2-5a266f699d0c)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         18803d5d-1436-4d00-af40-634b30ed7fb1)(content(Whitespace\" \
         \"))))(Tile((id fda190d6-cf1e-46f3-8921-f0dea8ed7e99)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         61c7cf4a-064c-490e-bf06-334794c22324)(label(\"\\\"bob\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         22825d12-7ff3-41d9-af30-a0dcf9c5e539)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         02796f44-cf03-43a2-8539-8d64baf1d64b)(content(Whitespace\" \
         \"))))(Tile((id \
         f6761557-5fc2-4b63-a0ec-2c278547bb02)(label(\"\\\"carol\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         9ef48ef9-5e5c-41d0-ae16-9f0c5f682616)(content(Whitespace\"\\n\")))))))))(Tile((id \
         d8d46253-0fc5-4be9-aee4-b55874e58841)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         4df77402-26e1-4e98-ba8d-dc9d2ac2d26b)(content(Whitespace\"\\n\"))))(Secondary((id \
         1fd26963-f6da-4b71-b1c4-0aef67740273)(content(Whitespace\"\\n\"))))(Tile((id \
         2f99c113-1fa4-45fd-95fe-c5431c554f6d)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         03a8b53b-61a8-4e79-8e99-104e176a3b0b)(content(Whitespace\"\\n\"))))(Tile((id \
         1f9b368c-e05a-45a2-88a5-1ede1dcad45f)(label(extract_mentions))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         ca9dd18d-da00-4c35-a861-82f21d8d1039)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         31b72cac-6fb6-4dd7-b4fd-9701c3534156)(label(\"\\\"no mentions \
         here\\\"\"))(mold((out Exp)(in_())(nibs(((shape Convex)(sort \
         Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         6a4fc0ca-5d61-4574-b0db-0a4ee64eb643)(content(Whitespace\"\\n\"))))(Tile((id \
         bb75f385-4906-4018-9b36-53cb3a888ed6)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         1881808f-bd45-43bc-b0b8-e82dc7891e7f)(content(Whitespace\" \
         \"))))(Tile((id \
         6e617584-1b43-4f24-aac5-da7ab4fd8b6b)(label([]))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         d1502e6f-83eb-421f-a815-2f3301483d5c)(content(Whitespace\"\\n\")))))))))(Tile((id \
         c7412ba4-edaa-4e6a-ae54-47143d4f60bb)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         3f7d3d80-0f16-46d1-b257-a4333189d58a)(content(Whitespace\"\\n\"))))(Secondary((id \
         08b3bf36-4a2a-4286-9ccd-451f0d5374d4)(content(Whitespace\"\\n\"))))(Tile((id \
         31da9b16-768e-490d-a1ba-4dac27e053a8)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         9fd307f3-f918-4b90-9803-4c319f5f24c3)(content(Whitespace\"\\n\"))))(Tile((id \
         3d7293ea-49dd-4c96-bc96-4c76717e8cfd)(label(extract_mentions))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         544664bc-e1e9-4c53-9350-782a8f05bb6d)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         794ab0ea-200a-4479-b0ed-c180b2e523d7)(label(\"\\\"@dan\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         7106001e-1930-43eb-8864-ea34a46a266e)(content(Whitespace\"\\n\"))))(Tile((id \
         2624d256-5975-4445-aea1-7cb5051ed6ca)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         916b2c4c-1b48-46fc-9aaa-43fc4e6d647d)(content(Whitespace\" \
         \"))))(Tile((id 05b4a5f1-3f66-45ad-aed0-e3ab0f7e3b56)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         2f0c54a5-e585-4745-a750-0054c4199ab7)(label(\"\\\"dan\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         3247872e-2b16-4b0e-b4a6-c33b20f8811e)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         4042e3a2-ca01-489e-8345-b8877ded3f56)(content(Whitespace\"\\n\")))))";
      backup_text =
        "# MENTION EXTRACTOR - SOLUTION #\n\n\
         # Check if a word starts with @ #\n\
         let starts_with_at = fun word ->\n\
         string_sub(word, 0, 1) == \"@\"\n\
         in\n\n\
         # Remove the @ prefix (take everything after index 0) #\n\
         let strip_at = fun word ->\n\
         string_sub(word, 1, string_length(word) - 1)\n\
         in\n\n\
         # Extract usernames: split -> filter -> map #\n\
         let extract_mentions = fun message ->\n\
         let words = string_split(\" \", message) in\n\
         let mentions = filter(words, starts_with_at) in\n\
         let usernames = map(mentions, strip_at) in\n\
         usernames\n\
         in\n\n\
         test\n\
         extract_mentions(\"Hey @alice\")\n\
         == [\"alice\"]\n\
         end;\n\n\
         test\n\
         extract_mentions(\"@bob @carol hello\")\n\
         == [\"bob\", \"carol\"]\n\
         end;\n\n\
         test\n\
         extract_mentions(\"no mentions here\")\n\
         == []\n\
         end;\n\n\
         test\n\
         extract_mentions(\"@dan\")\n\
         == [\"dan\"]\n\
         end\n";
      refractors = "()";
    } )
