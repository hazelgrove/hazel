let out : string * Haz3lcore.PersistentSegment.t =
  ( "Study / tutorial / 08-clamp",
    {
      segment =
        "((Secondary((id \
         4e0f08bd-8959-41a9-a6c1-4c756d663a3b)(content(Comment\"# PROBES \
         TUTORIAL - PART 8: WRITING EXERCISE (CLAMP)               \
         #\"))))(Secondary((id \
         2aa32bb5-bf37-4a38-bb5c-2acf2a7e6b67)(content(Whitespace\"\\n\"))))(Secondary((id \
         90746ca4-2367-48be-91c8-81c420abceaf)(content(Whitespace\"\\n\"))))(Secondary((id \
         32890a34-791b-4c15-80e5-e8335e0b5045)(content(Comment\"# Now it's \
         your turn to write some Hazel code.                     \
         #\"))))(Secondary((id \
         17950c39-2c55-4ecf-9d36-86da48bdd84b)(content(Whitespace\"\\n\"))))(Secondary((id \
         bcde00fb-c6a4-45c4-8072-b0a91d4fc03c)(content(Comment\"# Implement \
         `clamp`: constrain a number to be within [lo, hi].     \
         #\"))))(Secondary((id \
         cd209c4e-7b3d-4e3c-a56e-7e7dd430c779)(content(Whitespace\"\\n\"))))(Secondary((id \
         376aa7b8-fd96-4206-af19-acf651ff743f)(content(Comment\"#                                                                   \
         #\"))))(Secondary((id \
         4dbc5ca6-52e3-4047-b53e-de84db2745dc)(content(Whitespace\"\\n\"))))(Secondary((id \
         a3fd4aba-fd60-42ce-9526-8f09b290f61e)(content(Comment\"#   clamp(5, \
         0, 10) == 5    (in range \\226\\128\\148 return x)                  \
         #\"))))(Secondary((id \
         dde02367-2497-4f2d-a500-2ff67ae34f17)(content(Whitespace\"\\n\"))))(Secondary((id \
         751b0905-3ff4-4fbb-8fa3-38f4ec64c68e)(content(Comment\"#   clamp(-3, \
         0, 10) == 0   (below min \\226\\128\\148 return lo)                \
         #\"))))(Secondary((id \
         42cf30da-5fa3-4595-b280-422e1be7f794)(content(Whitespace\"\\n\"))))(Secondary((id \
         a3c9d3ec-6981-4bb1-9f84-af5f9e6be69f)(content(Comment\"#   clamp(15, \
         0, 10) == 10  (above max \\226\\128\\148 return hi)                \
         #\"))))(Secondary((id \
         f79a0860-9719-4954-a7e0-a7c01ddf9693)(content(Whitespace\"\\n\"))))(Secondary((id \
         8b85aa24-dc31-4965-85a5-e26e8d01d27f)(content(Comment\"#                                                                   \
         #\"))))(Secondary((id \
         334d36be-02ad-4ae0-b4ce-8ab945dcd44a)(content(Whitespace\"\\n\"))))(Secondary((id \
         b52ce78e-61cd-49fc-9fc6-4b7e0a03a3ff)(content(Comment\"# Replace the \
         ? with your implementation using if/else:             \
         #\"))))(Secondary((id \
         34bc8e5b-765e-475d-afcc-2b9697e70fa2)(content(Whitespace\"\\n\"))))(Secondary((id \
         9c7f0c72-d475-4e7f-a87a-edfd7fe4a97d)(content(Comment\"#   if cond \
         then expr else expr                                     \
         #\"))))(Secondary((id \
         4b3e877c-e3ec-407c-83bd-df60b2129c62)(content(Whitespace\"\\n\"))))(Secondary((id \
         01eefab6-ebea-4094-85b6-7c1c25e859b9)(content(Comment\"#                                                                   \
         #\"))))(Secondary((id \
         9e9bac07-88c3-479e-ab0c-e14a6f0b7b20)(content(Whitespace\"\\n\"))))(Secondary((id \
         30b0b6d2-7b7e-4077-8804-88c54a35b36f)(content(Comment\"# Turn on \
         auto-probe and click inside your function as you write   \
         #\"))))(Secondary((id \
         d7a66479-f887-4e7b-8250-4d3be31282a9)(content(Whitespace\"\\n\"))))(Secondary((id \
         88c1af95-f7bb-4402-97cb-4a83034943bd)(content(Comment\"# to see \
         intermediate values update live.                           \
         #\"))))(Secondary((id \
         c8a46fc0-e38f-4bf8-8c1e-9ab3dbc6e875)(content(Whitespace\"\\n\"))))(Secondary((id \
         ba806e6f-6e6a-4337-ace2-3ce0066845fc)(content(Comment\"# \
         =============================================================== \
         #\"))))(Secondary((id \
         bda3c860-a099-40bb-a716-31b2b8b9a22b)(content(Whitespace\"\\n\"))))(Secondary((id \
         7d30d792-c1a3-4d56-811a-d9a53076b7cf)(content(Whitespace\"\\n\"))))(Tile((id \
         bd0e6950-a94d-4357-b5f3-c6042862061e)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         2ea65794-726c-48a3-bee5-9634e160dfc7)(content(Whitespace\" \
         \"))))(Tile((id \
         09bdd8c2-3e99-4bc1-8b7e-4f4421d59603)(label(clamp))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         145fb518-0c68-4fbc-a881-65937cad8c99)(content(Whitespace\" \
         \")))))((Secondary((id \
         4a5ae1fa-5b78-404c-9139-241b6c665321)(content(Whitespace\" \
         \"))))(Tile((id 791ba0e9-d24b-46bc-9b9d-0b46832beafd)(label(fun \
         ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 37))(sort Exp))))))(shards(0 \
         1))(children(((Secondary((id \
         ee8f0ddf-95d2-49de-97e4-513d27a3b579)(content(Whitespace\" \
         \"))))(Tile((id \
         ba8579dd-428a-44f5-933a-4e66c8a04f10)(label(\"(\"\")\"))(mold((out \
         Pat)(in_(Pat))(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0 1))(children(((Tile((id \
         24cb62b2-f465-4a4e-8fca-82d2f8877760)(label(x))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         9a12188e-8c6e-4f00-9bb1-d4057e82721c)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 44))(sort Pat))((shape(Concave \
         44))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         13ece7ee-759a-498e-aa2a-6241d9d73cfb)(content(Whitespace\" \
         \"))))(Tile((id \
         dc9a62b3-6194-495c-a187-7ddc7f3e023c)(label(lo))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         a84cf1b6-3be9-4527-97c0-0141c87a6f37)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 44))(sort Pat))((shape(Concave \
         44))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         bc7bd888-486e-4bb9-b724-ce550abfe1c5)(content(Whitespace\" \
         \"))))(Tile((id \
         d717f9ba-314f-4ac6-aa91-0b22409ac9b0)(label(hi))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children()))))))))(Secondary((id \
         5a0c1757-2886-4604-8551-5f34f73869d3)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         7b7f295e-c9f4-43c0-9e73-a3265f14a94b)(content(Whitespace\"\\n\"))))(Tile((id \
         1a672a0c-7546-498b-8cd0-e2a7040755ee)(label(?))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         43e6711f-d3ec-4822-8e90-02ab7b7094da)(content(Whitespace\"\\n\"))))(Secondary((id \
         4dfc587b-a9cd-4e65-83d6-ffd95a355634)(content(Whitespace\"\\n\"))))(Secondary((id \
         05be2a31-0888-40c6-8a55-c14737854dc0)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         a76b8b98-8c10-46ee-b77a-d11156f51810)(content(Whitespace\"\\n\"))))(Secondary((id \
         8d962070-4061-41f7-a2d7-30292d1da631)(content(Whitespace\"\\n\"))))(Tile((id \
         92a38bf5-0e6e-4c62-9240-5e87e23ad6bd)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         4d523039-1283-4eb3-84f6-ac6a76d47ce7)(content(Whitespace\"\\n\"))))(Tile((id \
         32d0cd09-bdf6-4834-a366-8ac9a7a3b857)(label(clamp))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         f329688d-759c-4dd8-9273-15b7a8cd1848)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         e149950a-cf0d-4e03-8e90-b07a2d5219b8)(label(5))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         f9f43a57-f2bf-47f5-b6cd-a47db2a5a75e)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         ac5f2872-1f57-4d40-aa8f-c4f7d2d0a319)(content(Whitespace\" \
         \"))))(Tile((id \
         cd93f9ae-e98f-4b2a-8d7e-84fa58c107da)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         633fbb63-90ec-4d48-b480-d6645221d910)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         bdb1318b-cc0c-4f36-b0a2-14301563613a)(content(Whitespace\" \
         \"))))(Tile((id \
         739db769-62e7-4a7d-ab85-49f7ac9183b1)(label(10))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         35613711-0d95-4e35-8b63-32bfe9833836)(content(Whitespace\"\\n\"))))(Tile((id \
         484bfe62-7dfd-4218-9d42-f6e2c352048f)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         162eb3a6-c51c-41c9-a102-1a09e325ae15)(content(Whitespace\" \
         \"))))(Tile((id \
         c2a5d575-cc58-48f2-aa4d-c729b1f3d7f6)(label(5))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         00dbd02b-8d2d-4a84-82c8-d09de9f6c443)(content(Whitespace\"\\n\")))))))))(Tile((id \
         864078d6-e35b-46d5-8f96-af8b121e4779)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         3657cd66-59ff-4601-a4a5-082f29548d0d)(content(Whitespace\"\\n\"))))(Secondary((id \
         22b9cd2f-e1d5-4afd-bda0-339053551a5c)(content(Whitespace\"\\n\"))))(Tile((id \
         c7559120-bf36-42a5-a7cb-161164fadc0b)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         be51fcd6-0592-4576-bf2c-64a008c0182a)(content(Whitespace\"\\n\"))))(Tile((id \
         bb5f59c9-ae7b-4503-8434-892f75b35dea)(label(clamp))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         1580acb8-8f40-49e8-bd10-ff3a95688b0c)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         fcc9531a-02ff-401a-b0b8-5bd6cf40447b)(label(-))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape(Concave 25))(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         df4de728-1294-44c8-a540-edef03836578)(label(3))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         6050b763-bab9-42fd-9853-00dcc0b0350c)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         6701900c-b267-4eb5-95ea-caae50d86f17)(content(Whitespace\" \
         \"))))(Tile((id \
         fef372c8-a00e-41b4-bdd6-2a3aaa6d7cdc)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         bb92a358-ff67-448b-b9f7-b8c9a62e3788)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         94a103d8-2e04-477a-9d2e-2bc3d236395a)(content(Whitespace\" \
         \"))))(Tile((id \
         2cdfcb1f-00ae-40d3-a32c-ddcaecd5b93d)(label(10))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         fce619f9-9da6-4504-8588-349662221d95)(content(Whitespace\"\\n\"))))(Tile((id \
         ac8b6bca-20b1-473a-9b41-a56a0182dafb)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         7d25a2ca-be79-4ff7-bd80-13d666c8cbb0)(content(Whitespace\" \
         \"))))(Tile((id \
         cebe4ccd-c13c-4477-9f65-60e286b59864)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         dc4d593d-e6c1-419b-9449-0bea57421252)(content(Whitespace\"\\n\")))))))))(Tile((id \
         0d6d53f4-55f8-4240-8910-9e2556b0cba8)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         89886f91-62e0-43c3-b97e-ab31fdfdb89a)(content(Whitespace\"\\n\"))))(Secondary((id \
         465b352b-2142-4d0e-8e3a-87837806a975)(content(Whitespace\"\\n\"))))(Tile((id \
         b647c2bd-fc80-47cc-8c61-05465d2f1a80)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         94f92c62-135c-4560-86ea-6155312b5770)(content(Whitespace\"\\n\"))))(Tile((id \
         7fb914b2-5599-4c45-a640-d328048b3603)(label(clamp))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         92717865-637a-4cbf-b71b-014a16853665)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         c1abc025-21cd-4395-b456-ea04326859e2)(label(15))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         fee6fc42-9380-435e-96ad-471d9988197d)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         0901d351-0ed3-4d5d-8e46-7fa96190c43c)(content(Whitespace\" \
         \"))))(Tile((id \
         24bbe5a7-c76b-4fa4-9c86-1d9399a7228d)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         44698efe-3249-4ad8-b70a-6580c604c5b2)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         497ff0e2-b654-4be1-a706-06d13c66fb8f)(content(Whitespace\" \
         \"))))(Tile((id \
         c33f8dab-d834-4ac8-bf66-9370331407db)(label(10))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         49f38486-041a-48e5-b4f3-f7132f0d9366)(content(Whitespace\"\\n\"))))(Tile((id \
         37277d6c-8c78-40d6-a9ed-f249842763a2)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         e34823d1-f907-43e7-960b-f069712c2188)(content(Whitespace\" \
         \"))))(Tile((id \
         e4967d35-185f-471d-b5d1-1e0039d5d516)(label(10))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         217f746d-0dcd-4cbb-bd00-eab94636f760)(content(Whitespace\"\\n\")))))))))(Tile((id \
         6842fcc8-17d0-4725-bcd2-9da86664098d)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         b91bbad0-a429-4ee2-b2b2-e69fb23f9d02)(content(Whitespace\"\\n\"))))(Secondary((id \
         47ba7ab2-c049-4c10-a546-1732c5a8a3a4)(content(Whitespace\"\\n\"))))(Tile((id \
         e209b84e-fff8-46a3-9556-63b7b5877eb4)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         070e7293-7052-4072-af38-478533c02a49)(content(Whitespace\"\\n\"))))(Tile((id \
         13542017-c2d9-4f4c-974a-02e84ac56ffa)(label(clamp))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         9e6b7e99-bf8b-4638-80f6-a36ac92924ce)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         b35734ec-14cb-4d72-af01-bfb185b01dd5)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         bf8a2ad1-b89a-405e-bf2f-c08605a96f77)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         5e3c0f3c-7c21-41b3-9e73-650d8c9007ab)(content(Whitespace\" \
         \"))))(Tile((id \
         30ce2eff-33d6-493b-99d6-9462541ce326)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         6116af5e-af62-4e0f-b25b-dd437aae3739)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         c79c0c65-427f-4d0d-b3a7-df4106944929)(content(Whitespace\" \
         \"))))(Tile((id \
         e39f5cf3-000e-414e-b6da-5d8c5eb56869)(label(10))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         81b0cf26-f082-43ce-b96c-e0315c1b7486)(content(Whitespace\"\\n\"))))(Tile((id \
         54403da4-c0b6-49f1-ad3c-46a213594282)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         3f773954-4aa9-476f-89ad-1f34dc4cbea0)(content(Whitespace\" \
         \"))))(Tile((id \
         2fef1060-b62d-4582-9f84-402300b57908)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         be28c720-e93c-46c2-a123-ddcf6c6fb3e0)(content(Whitespace\"\\n\")))))))))(Tile((id \
         032a12b2-d768-481a-893a-14acbb07b495)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         0e369d14-d809-4b23-bcd0-0226ec60a2d9)(content(Whitespace\"\\n\"))))(Secondary((id \
         1eb76ead-c405-4a28-a109-2c4bb1ae68ef)(content(Whitespace\"\\n\"))))(Tile((id \
         77c90baa-7fe5-43d4-a3b4-2a4936cea730)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         6f6bb295-b49f-47c2-a3a1-d0f69686b52e)(content(Whitespace\"\\n\"))))(Tile((id \
         c7656216-565e-4bfd-9f4f-5d98aad2f3b0)(label(clamp))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         42888758-e95f-4e2f-8f89-920d966b795d)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         997cf27e-666a-40d8-a384-883323139491)(label(10))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         7cc1ca3c-cff9-4bad-b86f-8067cd5176f1)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         a8ad92e2-3527-47e1-bef7-3e4f0247f699)(content(Whitespace\" \
         \"))))(Tile((id \
         3a212e97-d026-4810-9e36-0a818c0c6f6f)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         3d003f2b-0799-463d-8eae-f76218cff46c)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         ae2f912b-d59c-4e01-a96b-f3c77386b720)(content(Whitespace\" \
         \"))))(Tile((id \
         e7f6e658-de68-474d-8248-e1d70654206e)(label(10))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         47b07738-024e-4310-bed8-4b302f827849)(content(Whitespace\"\\n\"))))(Tile((id \
         9820f499-dd0f-4950-9583-aac8f4e99a61)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         c6247c3b-1ac1-4562-a752-611466adc947)(content(Whitespace\" \
         \"))))(Tile((id \
         b00658a1-ee28-4713-91c2-432f38673db9)(label(10))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         715aabac-b58e-4464-ab31-6ff438cca6b9)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         fd46ea7a-1dbd-4517-b5f9-1cf0583ac852)(content(Whitespace\"\\n\"))))(Secondary((id \
         8e517173-cdda-4c36-a303-49d7c4c8af07)(content(Whitespace\"\\n\"))))(Secondary((id \
         6c6c45ab-ed4c-41c0-8a63-c4e38d7586ee)(content(Comment\"# END OF PART \
         8 - Select the next slide from the top menu       \
         #\"))))(Secondary((id \
         2aa295e2-6f02-4499-8ab0-738e25bb21e4)(content(Whitespace\"\\n\")))))";
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
