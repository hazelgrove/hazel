let out : string * Haz3lcore.PersistentZipper.t =
  ( "[TESTS] Types and Static Errors",
    {
      zipper =
        "((selection((focus Left)(content())(mode \
         Normal)))(backpack())(relatives((siblings(((Secondary((id \
         5da92fc0-10cd-4354-bf0b-1a22accca803)(content(Comment\"# Internal \
         Regression Tests: Type errors #\"))))(Secondary((id \
         42a2f89c-4a95-4199-8800-f53809f593ba)(content(Whitespace\"\\n\"))))(Secondary((id \
         e86dd07c-7157-40bc-bdaf-59d06e0034c9)(content(Comment\"# Each line \
         should show errors or not as indicated #\"))))(Secondary((id \
         1d55398b-0045-41ec-8690-4a4664980596)(content(Whitespace\"\\n\"))))(Secondary((id \
         5294d45d-964b-45c9-a85c-c0c11464bd39)(content(Whitespace\"\\n\"))))(Tile((id \
         758d9a9b-d1f3-4df6-9100-9f81566a6a37)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         17))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         ecc545ce-876a-4a70-ab47-8bfe9a428248)(content(Whitespace\" \
         \"))))(Tile((id \
         e16f00db-fd89-4065-b35a-64904016ae29)(label(_))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         5518e231-da7f-4a7d-8d45-05d910e7f0cf)(content(Whitespace\" \
         \")))))((Secondary((id \
         a29c4eb6-de40-4bb4-aaac-10f598b07123)(content(Whitespace\" \
         \"))))(Tile((id \
         e7743ed3-3268-45dd-be07-e34b23633d36)(label(unbound))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         651970c7-502d-48de-b4d4-870b276c3233)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         e52d4513-49e6-4f2f-8fd1-939e723da309)(content(Whitespace\" \
         \"))))(Secondary((id \
         d21dcb5b-762f-4047-9a8b-51cff4e4d5d8)(content(Comment \
         #err#)))))((Secondary((id \
         ab1dff56-08b2-4c50-a2b5-fa5a36fbcdab)(content(Whitespace\"\\n\"))))(Tile((id \
         4d41990a-b92b-444e-a1a3-bb7844e88871)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         17))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         b0ca9c49-112c-47a8-950b-8d0d7bc59a23)(content(Whitespace\" \
         \"))))(Tile((id \
         33aec8d3-3eb0-4679-a386-2dac5a304783)(label(Undefined))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         e5497533-ea66-474d-bd69-cc76b4a5653d)(content(Whitespace\" \
         \")))))((Secondary((id \
         51530a4b-7d00-47bd-b5ec-10682b5207d9)(content(Whitespace\" \
         \"))))(Tile((id \
         442205d8-cbd4-4bdc-9066-f486206b94fa)(label(Undefined))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         527c9d07-1ec6-4aec-bca1-b1a50cd3af43)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         3c09f1b8-0796-4cf6-a79c-0bcc0832d91c)(content(Whitespace\" \
         \"))))(Secondary((id \
         25d84317-ce77-4faf-9025-10a68eed8e6a)(content(Comment\"# 2x \
         err#\"))))(Secondary((id \
         97db46c7-ce76-4cee-9703-f04949d45340)(content(Whitespace\" \
         \"))))(Secondary((id \
         924f65ec-e7e5-46e4-ab2b-4e652571f60e)(content(Whitespace\"\\n\"))))(Tile((id \
         55a5b34b-daf5-48cb-b7de-4b76f16a960f)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         17))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         7c0fb53d-c7e1-4548-acb2-8faa621271ce)(content(Whitespace\" \
         \"))))(Tile((id \
         21d17081-0c7d-4a64-aead-e9ec099e9eb1)(label(true))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         ed0e1fc1-a44d-44a8-b50e-91e38cf14712)(content(Whitespace\" \
         \")))))((Secondary((id \
         a297d308-da82-4733-81ba-6c15026cac9d)(content(Whitespace\" \
         \"))))(Tile((id \
         1abd1a01-f0b5-498f-9446-4b285f25055b)(label(2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         862e2271-7b23-48e6-a013-c50d7e8240eb)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         cd54c7ae-3f6a-4aed-bb62-39cab8c5166a)(content(Whitespace\" \
         \"))))(Secondary((id \
         526f32dd-17b9-444d-b697-3a49874d86ab)(content(Comment\"#2x \
         err#\"))))(Secondary((id \
         3fa82081-4e36-4899-83eb-0194fbb8a152)(content(Whitespace\" \
         \"))))(Secondary((id \
         22947c17-9809-4e12-b52b-fd9b018be69b)(content(Whitespace\"\\n\"))))(Secondary((id \
         1a9bbbc8-33ba-42ef-a9a8-96b4936f83d2)(content(Whitespace\"\\n\"))))(Tile((id \
         d46a196d-925c-4f7f-9d9b-f73ef61410a5)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         17))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         ae1c0f3c-1692-4f10-9076-bcda8da909fd)(content(Whitespace\" \
         \"))))(Tile((id \
         685cd41c-494d-42eb-803e-262e8ebeb39a)(label(?))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         cabf0162-6154-4020-9ff0-f16aa1e5e9a8)(content(Whitespace\" \
         \")))))((Secondary((id \
         31e7b59a-ecef-4a68-907b-dd2c315acd63)(content(Whitespace\" \
         \"))))(Tile((id 1fc8b7c5-f735-4e45-9969-d03b430a1f0f)(label(if then \
         else))(mold((out Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 13))(sort Exp))))))(shards(0 1 \
         2))(children(((Secondary((id \
         4c64ce16-b737-4ab9-aac5-2c47d8f59c38)(content(Whitespace\" \
         \"))))(Tile((id \
         ce6095e0-6573-4575-915d-77625cad241b)(label(true))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         360306a1-d0c2-4b1b-8b91-98050159c120)(content(Whitespace\" \
         \")))))((Secondary((id \
         9076b31c-ca93-4931-a1b1-a3c6978acfb0)(content(Whitespace\" \
         \"))))(Tile((id \
         f3d56cf8-047d-4491-990e-2faa2345ed52)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         163e8baf-71e8-4c62-9bb5-a9789b912f9e)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         35b94740-b1b8-46d1-8c65-a2d9699cf5d7)(content(Whitespace\" \
         \"))))(Tile((id \
         32fd3308-b7d7-46ba-b40b-8c4739821951)(label(1.))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         359922bf-8f7c-4756-bce7-dfbdab6768eb)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         0aef1054-7de3-46de-b629-1712252e36c5)(content(Whitespace\" \
         \"))))(Secondary((id \
         bc921273-eecc-40dc-b037-bd352a9ba17e)(content(Comment \
         #err#))))(Secondary((id \
         27b24844-827f-4aaa-ac1f-22be23ed9e04)(content(Whitespace\" \
         \"))))(Secondary((id \
         7f2c237f-d86d-4e3f-afdb-498d07823331)(content(Whitespace\"\\n\"))))(Tile((id \
         1c454cf8-ecd5-4033-bbf8-19b68014191e)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         17))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         c2387c61-33ff-4a77-a91d-9268e9fbac09)(content(Whitespace\" \
         \"))))(Tile((id \
         f04265e1-9788-4653-a1d6-e266c5a59d66)(label(_))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         3c92d98f-a523-4344-903f-fcd44b7b8d13)(content(Whitespace\" \
         \")))))((Secondary((id \
         1f170ec6-a88f-4e37-98ff-43df1627bb74)(content(Whitespace\" \
         \"))))(Tile((id 3b52bb5e-ec3b-484c-ba60-48fb0c1db7d6)(label(if then \
         else))(mold((out Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 13))(sort Exp))))))(shards(0 1 \
         2))(children(((Secondary((id \
         fb073b60-2128-4c75-b741-84dd5eec14b9)(content(Whitespace\" \
         \"))))(Tile((id \
         0b5b8922-658a-4a9f-80bd-da3794c121d3)(label(true))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         949b9c01-395e-47f6-8cce-b5e6b6425c8c)(content(Whitespace\" \
         \")))))((Secondary((id \
         2cd662be-b43c-439a-a48d-51f072e4b301)(content(Whitespace\" \
         \"))))(Tile((id \
         69458974-8dd8-428a-8cee-887b4d3e1011)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         781060e1-c57d-4d6e-882a-8ad6560f230c)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         1b9ba2c3-4ca1-48d8-90b9-63e250bd1136)(content(Whitespace\" \
         \"))))(Tile((id \
         74cc524f-7218-4dba-a0de-0a18046194e9)(label(1.))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         635bcde5-e132-4058-978c-61533458ed13)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         ddc7f71e-ca6a-465b-8c87-5462853072a6)(content(Whitespace\" \
         \"))))(Secondary((id \
         923041c3-09ef-40f0-a53f-de812afcebf8)(content(Comment \
         #err#))))(Secondary((id \
         0bb4658e-b3bd-4d03-bd98-e4d3eb402e32)(content(Whitespace\"\\n\"))))(Tile((id \
         2c1d0ea8-7cb7-4202-af0f-9911fbd1ec26)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         17))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         5b4eeb14-e111-401c-8248-3aaf6b1c554e)(content(Whitespace\" \
         \"))))(Tile((id \
         9e766167-bcea-4bd6-9217-17aa21f80cb8)(label(_))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         8ef5ab6d-6cc4-4081-b092-6a7721669dda)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 12))(sort Pat))((shape(Concave \
         12))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         34d1a24a-41ae-4d51-8f5e-2a1d0570a6ef)(content(Whitespace\" \
         \"))))(Tile((id \
         5e923e13-50b3-40a1-8a29-2f66a2693330)(label(?))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         ada81658-b24c-447e-8e91-142f404fe596)(content(Whitespace\" \
         \")))))((Secondary((id \
         ff02d723-d855-4161-8cac-3aa34b6249d9)(content(Whitespace\" \
         \"))))(Tile((id 0fdedef1-300c-4640-9f6b-91b8c72008cb)(label(if then \
         else))(mold((out Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 13))(sort Exp))))))(shards(0 1 \
         2))(children(((Secondary((id \
         9b034a88-063b-4b56-805a-c33455450427)(content(Whitespace\" \
         \"))))(Tile((id \
         668e5861-da35-478b-9c16-76563698bea5)(label(true))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         000b9574-ea77-49fe-be7d-4c90e2805ab9)(content(Whitespace\" \
         \")))))((Secondary((id \
         87175621-3ea5-46f2-a49a-b5e5c1cabfcb)(content(Whitespace\" \
         \"))))(Tile((id \
         9d8b813e-c6f9-4f92-ba80-5f0ff87be342)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         8da20320-b9f3-4174-81d7-125940998614)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         7c2f93a8-d19a-45f5-a0e3-e927e22231ac)(content(Whitespace\" \
         \"))))(Tile((id \
         0b07d2df-14f0-4363-bd27-b9869c80d813)(label(1.))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         81cfd0cc-d03a-4fc8-8fa6-b3a95c031543)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         be5eb0b5-a089-4a75-90c2-0762c9803900)(content(Whitespace\"\\n\"))))(Tile((id \
         1339c79c-f1cb-4065-a6e6-6d93094c6e88)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         17))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         3c521c1e-0569-43e8-98eb-13a6e83c0360)(content(Whitespace\" \
         \"))))(Tile((id \
         171b8fab-f5da-457b-8bfb-0c0ea484b619)(label(_))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         af9669f3-ac9d-4d4e-8178-118a658c1acc)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 12))(sort Pat))((shape(Concave \
         12))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         8c96f25b-ee85-4207-bcfc-92b8c44ad284)(content(Whitespace\" \
         \"))))(Tile((id \
         95e80aa6-dfe1-449a-bdf4-d98b80a465b6)(label(Int))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         e07a80d8-0c00-45d1-8309-5dd1212fb23d)(content(Whitespace\" \
         \")))))((Secondary((id \
         761c39c7-7f56-4c6e-8301-77177c9271b7)(content(Whitespace\" \
         \"))))(Tile((id 919ca3cd-b67d-4eb8-9a75-712c864a22a5)(label(if then \
         else))(mold((out Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 13))(sort Exp))))))(shards(0 1 \
         2))(children(((Secondary((id \
         273a02dd-9e8e-49bf-8bff-f682950cb549)(content(Whitespace\" \
         \"))))(Tile((id \
         baeddf76-ce9f-493e-aa38-54b5d2ee57d6)(label(true))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         0049890b-ab90-4167-b71c-7703d5f2966a)(content(Whitespace\" \
         \")))))((Secondary((id \
         9dbdd12e-99d0-4f1a-b122-44e155bd502b)(content(Whitespace\" \
         \"))))(Tile((id \
         53f267e6-4ae2-4ec2-9da8-0a50a319eaa2)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         c9cebc13-1f7f-4352-9f4a-4893ea99c2bf)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         6ebe2987-1ed7-458f-a412-3f95909fbef6)(content(Whitespace\" \
         \"))))(Tile((id \
         8dbb7302-d5b4-4ee8-ba5e-50f0b47ba53d)(label(1.))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         ec52f0b3-744f-4efb-9487-cedac5fa1216)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         d18b91a6-dc68-4e1b-b84a-02573a655b65)(content(Whitespace\" \
         \"))))(Secondary((id \
         4179bbc1-2373-4a36-9a36-ae430f8f4e69)(content(Comment \
         #err#))))(Secondary((id \
         2d009bf6-b3d3-4a73-9a88-3c16ce0c0a22)(content(Whitespace\"\\n\"))))(Tile((id \
         18731ed3-13d7-4121-83cc-dc8da7d518fd)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         17))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         d1ec2c86-e9da-4e1d-9e96-cd7c4817bc01)(content(Whitespace\" \
         \"))))(Tile((id \
         0dc32826-da12-41a5-a8d9-bf97a424af3c)(label(_))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         bc032bcb-92a5-4c8a-a5b7-e1100f6c16cb)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 12))(sort Pat))((shape(Concave \
         12))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         847833a9-9721-4f0c-842e-e7fe5ecb85f5)(content(Whitespace\" \
         \"))))(Tile((id \
         b111eada-6fdf-4ff5-935d-a4792d1b1e6a)(label(Fake))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         b0409219-b018-4004-ac62-49b4fe302b3f)(content(Whitespace\" \
         \")))))((Secondary((id \
         1a525c37-234d-425d-9ee5-8ae791190501)(content(Whitespace\" \
         \"))))(Tile((id e8188ebc-d8b3-4345-b1c5-70e67dee6527)(label(if then \
         else))(mold((out Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 13))(sort Exp))))))(shards(0 1 \
         2))(children(((Secondary((id \
         17677456-b3ca-4d04-9dbc-a7ac432d1fda)(content(Whitespace\" \
         \"))))(Tile((id \
         fd34bee1-1b68-4096-80e9-9cd3ae539e12)(label(true))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         7e3d2df2-cdd1-4cd9-8398-57f1d67a70d6)(content(Whitespace\" \
         \")))))((Secondary((id \
         aa6e7a7a-cf03-4a05-a0cd-5fa657d6d55a)(content(Whitespace\" \
         \"))))(Tile((id \
         4bdf9d6a-924c-4f89-8314-7a95270142f2)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         2211a089-86da-4fd4-8474-fa51c99dcff7)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         1b1e7516-cf21-4360-94ba-5d30aa572f65)(content(Whitespace\" \
         \"))))(Tile((id \
         917ee296-4347-414a-aaae-e2f5a97a87a8)(label(true))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         ba6ffa57-1c5d-4b9d-a1b5-3b1328849511)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         5be2c2bd-da80-4512-bd2a-0a30f87d9958)(content(Whitespace\" \
         \"))))(Secondary((id \
         21bd2e10-a4d3-4951-a573-87b2c8f4eb1a)(content(Comment \
         #err#))))(Secondary((id \
         f131d865-8f16-4925-8f5b-6c2a42c446ce)(content(Whitespace\"\\n\"))))(Tile((id \
         f4c75534-a83a-4d04-a4fb-7714e1a2be7b)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         17))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         13883334-6745-4000-8735-9cd577a74d56)(content(Whitespace\" \
         \"))))(Tile((id \
         d7da986a-907c-4d43-ba60-f2868db14ab9)(label(_))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         00317a8c-b6b0-41e7-8284-f69f9970dc32)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 15))(sort Pat))((shape(Concave \
         15))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         668a2995-d1b6-48a2-8fd2-8662d288bcd1)(content(Whitespace\" \
         \"))))(Tile((id \
         46c49881-a9b2-4dc8-9cf2-c8231ba08662)(label(_))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         af269db2-934e-4855-b194-d5fc2381b7d2)(content(Whitespace\" \
         \")))))((Secondary((id \
         19bdbfd5-876f-4ac5-9b43-86c6d99586e0)(content(Whitespace\" \
         \"))))(Tile((id 5d8d2765-7c51-41ed-8af9-8f56488444e2)(label(if then \
         else))(mold((out Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 13))(sort Exp))))))(shards(0 1 \
         2))(children(((Secondary((id \
         9f5c3366-b61c-4de3-a8b1-9b9513755bd4)(content(Whitespace\" \
         \"))))(Tile((id \
         a1c41de4-fce7-449d-a78d-845ed7b83fc9)(label(true))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         95f34ebc-0dbe-439d-95dd-294cee252ec3)(content(Whitespace\" \
         \")))))((Secondary((id \
         a567c3ac-db02-4442-ae43-87ea60064fc6)(content(Whitespace\" \
         \"))))(Tile((id \
         f2c2d679-cf1a-45eb-8e5b-a3fbf5f0d7e7)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         82fd11dc-9d43-47bb-bba1-107c55f26962)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         d355dfc2-04e3-4e0e-a8e7-768021d31816)(content(Whitespace\" \
         \"))))(Tile((id \
         4c708eaf-93c2-4d51-81de-2fd3eb5d8d11)(label(1.))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         dacb6033-fcc9-4b12-9aab-49ea9fc94687)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         8fc791a7-579d-45c6-bd6e-9bb72239af77)(content(Whitespace\" \
         \"))))(Secondary((id \
         f0ac9f87-1bf4-4061-90c3-7ed6ff0c9676)(content(Comment\"#2x \
         err#\"))))(Secondary((id \
         62d1b5fe-cc31-4090-9d73-9eb0d2f3f492)(content(Whitespace\"\\n\"))))(Tile((id \
         4d1bde0b-6567-41e5-ae93-f10d8db5a6f8)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         17))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         d7c01def-cc22-4d2a-ab20-4592a0774cfc)(content(Whitespace\" \
         \"))))(Tile((id \
         16ae4dec-6c75-4fc9-8f43-1846e10b4f4f)(label(_))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         2d5b145d-9d10-42d9-a2c3-ab290a406e98)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 15))(sort Pat))((shape(Concave \
         15))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         ddcfb4d1-ad7e-4511-99dc-d3bc7e287e2e)(content(Whitespace\" \
         \"))))(Tile((id \
         72098cb3-4e14-4a8d-aa8f-f549c9ff9c31)(label(_))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         14e3f792-983c-41b7-9613-c5cf7ba10fb9)(content(Whitespace\" \
         \")))))((Secondary((id \
         a21af13a-3af9-49c7-a039-10fa5ba0c227)(content(Whitespace\" \
         \"))))(Tile((id \
         ece3978e-1cd4-451b-bf45-7d6afcc0d8e0)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         e9ad479b-4b32-4aa4-95c5-8f2331660129)(label(if then else))(mold((out \
         Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         13))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         ee8bde47-cc2b-400c-8842-875321687b3f)(content(Whitespace\" \
         \"))))(Tile((id \
         3abe37ed-1dc4-403f-9333-13f50ab3c89a)(label(true))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         46d7ce17-b064-4267-9d04-d43ff1a44b37)(content(Whitespace\" \
         \")))))((Secondary((id \
         8e44d5ee-ce2a-4383-9b6a-927091bb8bad)(content(Whitespace\" \
         \"))))(Tile((id \
         877923e2-726b-4015-bfb7-5f086e094f31)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         8bacec5f-e164-4eb4-b98e-8cff5fd06fae)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         13781ee0-dd5a-4c44-a4c6-e6b92703a646)(content(Whitespace\" \
         \"))))(Tile((id \
         67fb18f1-dcf0-434d-90d7-12148e6309bd)(label(1.))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         afccfd4d-9f21-4236-bf03-6b343c979790)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 15))(sort Exp))((shape(Concave \
         15))(sort Exp))))))(shards(0))(children())))(Grout((id \
         fd18e452-a794-4379-86b5-8dc753604a8d)(shape Convex)))(Secondary((id \
         5c9c94b5-7024-4306-bc51-615d6fc286b8)(content(Whitespace\" \
         \"))))(Secondary((id \
         90824032-b495-4713-aca0-0ddad77318e1)(content(Whitespace\" \
         \"))))(Secondary((id \
         a4e74869-0e3e-4d7a-bf27-333cde7cadc3)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         e627a0a2-dee6-4187-9293-25c03387df95)(content(Whitespace\" \
         \"))))(Secondary((id \
         766bc24d-9b24-4e05-975e-dd9e2d97b7e6)(content(Comment \
         #err#))))(Secondary((id \
         78f71957-8efa-4d59-a575-caa4cf32bfc1)(content(Whitespace\"\\n\"))))(Tile((id \
         2d69dac9-2ccb-4011-9aab-4eca07c7c92c)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         17))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         23b22068-b02d-4ab8-89a9-4540fef47d7c)(content(Whitespace\" \
         \"))))(Tile((id \
         5365281a-a023-4791-9839-87684bdf31e8)(label(_))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         9e80647c-ac36-45a5-b0c0-5fc8961430cf)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 12))(sort Pat))((shape(Concave \
         12))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         06ba9142-925a-46df-964e-9df82c1833c1)(content(Whitespace\" \
         \"))))(Tile((id \
         02a74c5f-e222-4858-a38f-42908b41c0d3)(label(?))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         90156175-90f3-4dfa-a9f0-56d255aa26c1)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 15))(sort Pat))((shape(Concave \
         15))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         49903fe9-0d39-42b7-9c7a-0f14e4a9fd21)(content(Whitespace\" \
         \"))))(Tile((id \
         7dec882a-5c45-424e-8102-6436a5c671f3)(label(_))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         529eae4c-f6cd-4acd-ae7c-d5b58803bd56)(content(Whitespace\" \
         \")))))((Secondary((id \
         c765dacb-776a-45b9-a193-78e5a748c823)(content(Whitespace\" \
         \"))))(Tile((id \
         3e445b08-a4f3-4933-9996-7830a0748b46)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         c9aaa257-c542-4379-ba42-6abf5bf2006e)(label(if then else))(mold((out \
         Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         13))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         d637ba05-504e-4018-88eb-c977bd5560dc)(content(Whitespace\" \
         \"))))(Tile((id \
         a5f17386-4e7a-4e30-86fb-2abf74b4fc06)(label(true))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         2c0726e0-3036-400a-81c6-70bdc8621e82)(content(Whitespace\" \
         \")))))((Secondary((id \
         9132c14f-825c-44da-935b-1658fd9cea66)(content(Whitespace\" \
         \"))))(Tile((id \
         7f3f8048-bd87-46f0-846b-58d34d6c5f62)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         2e982fe5-0106-43be-938e-ac667a719c41)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         be5ed0e3-dc75-4069-bdae-ed34c43bd28e)(content(Whitespace\" \
         \"))))(Tile((id \
         a79ea06c-1476-4c69-8beb-4145646d4894)(label(1.))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         e26977b4-dd03-4e05-8265-c3646e724205)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 15))(sort Exp))((shape(Concave \
         15))(sort Exp))))))(shards(0))(children())))(Grout((id \
         29b37a58-9bb0-4715-948c-b7c133046248)(shape Convex)))(Secondary((id \
         2cefdb81-8d32-44cf-a3a1-21c297ad2aa1)(content(Whitespace\" \
         \"))))(Secondary((id \
         f3715eff-bae1-4485-b3a7-2e4468b968b6)(content(Whitespace\" \
         \"))))(Secondary((id \
         d76e0966-3276-4ecc-bca4-a948545fe45d)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         3b8b0cca-8049-4d93-9103-87ae36c44270)(content(Whitespace\" \
         \"))))(Secondary((id \
         0ffc8e0c-89e2-41a1-848b-552c97f843fd)(content(Whitespace\"\\n\"))))(Tile((id \
         2f891405-da40-48a2-b79a-5f3b55251a76)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         17))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         d4fbe7f2-6e55-4160-a618-a1d1a09eb6e9)(content(Whitespace\" \
         \"))))(Tile((id 8dbef8da-e68a-4ec8-b75e-3a29fe9c8502)(label([ \
         ]))(mold((out Pat)(in_(Pat))(nibs(((shape Convex)(sort Pat))((shape \
         Convex)(sort Pat))))))(shards(0 1))(children(((Tile((id \
         a49b7ef5-f798-4643-9265-f8cf8c42b783)(label(_))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children()))))))))(Secondary((id \
         2a4ac6a7-0d55-4174-ab52-697d276c6fa9)(content(Whitespace\" \
         \")))))((Secondary((id \
         61d291b8-7b27-4fa0-ab09-ff412a99362a)(content(Whitespace\" \
         \"))))(Tile((id 6b1de325-1191-4962-8899-bf6988fe56f1)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         7b855072-62f4-415e-8452-952458cffca7)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         c790af14-829d-48fa-bd81-ee93a0f1b4dd)(label(if then else))(mold((out \
         Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         13))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         1ff71883-a48a-4ea0-b098-8d76e476a935)(content(Whitespace\" \
         \"))))(Tile((id \
         c02edbab-daee-4395-98fe-757cd6fee237)(label(true))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         8ce9a5b4-babe-4bf5-b061-7f45c282dadf)(content(Whitespace\" \
         \")))))((Secondary((id \
         d401bb56-0198-4147-af77-8a5aee9d583a)(content(Whitespace\" \
         \"))))(Tile((id \
         4f91be41-e3df-44a6-8a12-a15454048adb)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         b5540fb4-c645-42a2-ada2-3b805f1984a5)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         55622bb1-19e8-46d3-bf1d-e34363fc9e0f)(content(Whitespace\" \
         \"))))(Tile((id \
         518b51cd-a4c8-404d-a231-6f429a6b644c)(label(1.))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))))))))))))(Secondary((id \
         744414ae-d71e-48ae-bb9d-0e578c0459b4)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         e668a5ad-b385-40ec-b5b3-d04df38d66be)(content(Whitespace\" \
         \"))))(Secondary((id \
         18cc6604-fc46-4227-b871-d8c7df7b8bc1)(content(Comment\"#2x \
         err#\"))))(Secondary((id \
         ef4a4191-136e-4ace-976d-39d8e03247b0)(content(Whitespace\"\\n\"))))(Tile((id \
         9d99af65-a5af-4d6e-af4d-6b454b9bc95a)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         17))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         1552bacb-6c53-4dfa-aa28-ce61edcc10f2)(content(Whitespace\" \
         \"))))(Tile((id da3f7767-bd3d-404f-b0df-1a0dcd052f31)(label([ \
         ]))(mold((out Pat)(in_(Pat))(nibs(((shape Convex)(sort Pat))((shape \
         Convex)(sort Pat))))))(shards(0 1))(children(((Tile((id \
         f157a3c6-62bf-4d01-9763-a3a6153af589)(label(_))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children()))))))))(Secondary((id \
         113a5483-c7a2-4695-b09b-b299cd18b4ac)(content(Whitespace\" \
         \")))))((Secondary((id \
         c9129d4e-3c02-408f-8935-4e72d7010c1f)(content(Whitespace\" \
         \"))))(Tile((id \
         c8e00484-a21c-43e0-8aa5-2b78135f4fa9)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         217d5fd6-b91e-4473-98bb-42451ad1065c)(label(if then else))(mold((out \
         Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         13))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         01dff95a-d8a2-4b55-bece-4002bd9c2761)(content(Whitespace\" \
         \"))))(Tile((id \
         b41c3a2c-0ba3-4514-8dd6-8e24b395e3e3)(label(true))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         aad648c6-0b64-42b4-aaa0-7be5f9a8d682)(content(Whitespace\" \
         \")))))((Secondary((id \
         f564477d-3cfa-407d-bc31-1e49f12bc3e8)(content(Whitespace\" \
         \"))))(Tile((id \
         2b524b9c-6e74-4719-ab54-061b56ec717b)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         98f6ac6e-62bd-4748-b512-14bf23df07ea)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         fa485591-489a-43f2-bfaf-6640f7a71d32)(content(Whitespace\" \
         \"))))(Tile((id \
         ea5bf365-68b9-4850-a07a-4bc0adce1225)(label(1.))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         415357bc-ea8e-48b8-91cc-fa0aa13f1633)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         8bb6cc99-e72c-47a4-9ff8-6e2286b37a48)(content(Whitespace\" \
         \"))))(Secondary((id \
         80840a69-ffa9-4e74-9d19-09edec73c165)(content(Comment\"#3x \
         err#\"))))(Secondary((id \
         44c39228-da1a-433b-944a-a0c58b70039c)(content(Whitespace\" \
         \"))))(Secondary((id \
         2c374313-7ea3-4e1f-872b-c4c2af86cdc9)(content(Whitespace\"\\n\"))))(Secondary((id \
         17ab14c0-8f4b-44cd-a532-9a761a833be0)(content(Whitespace\"\\n\"))))(Tile((id \
         6a4e05e9-217c-4e45-8de1-c9e392ccb042)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         062c107b-c14c-4c45-aa0c-c9ffa4d5dbb1)(label(?))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         869737d1-c23a-476c-ab5e-8aa7861d964b)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 2))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         4eace465-2bc1-485e-8071-10d3df39a94b)(label(if then else))(mold((out \
         Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         13))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         31e06065-f79b-462c-abaa-97f39a7acb2d)(content(Whitespace\" \
         \"))))(Tile((id \
         e7b073be-fd7f-4671-a3f2-2b6d25f21d58)(label(true))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         ab43243b-e396-418f-b733-4bd210aaae93)(content(Whitespace\" \
         \")))))((Secondary((id \
         fddf6e72-eedf-43d2-bc92-49aa0a7e9da1)(content(Whitespace\" \
         \"))))(Tile((id \
         d69b9e82-73ed-4431-bd24-53dc723864a7)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         0c760bc0-d087-420d-909e-0b87ab92629a)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         cd6329dd-1917-4e68-83b6-407560872ef5)(content(Whitespace\" \
         \"))))(Tile((id \
         8c4024e7-bf7c-4da4-a8ed-043381e009b9)(label(1.))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         cdf6f11c-509f-4bc7-b86e-a0d84b87b124)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 16))(sort Exp))((shape(Concave \
         16))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         8d5c4873-344e-4b6f-9252-8d16ecea9c89)(content(Whitespace\"\\n\"))))(Tile((id \
         a7151210-7df0-43fa-b6e8-1522bd2a8d39)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         1e2df0fd-2270-4a1f-8fdc-e0a48ecd283a)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 2))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         4ebe65f5-1866-4b9c-be3f-66ba8b0550f5)(label(if then else))(mold((out \
         Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         13))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         3772625c-8d92-4280-b22c-2178e797e62c)(content(Whitespace\" \
         \"))))(Tile((id \
         bcc79751-6308-493e-887d-561862ee221b)(label(true))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         a51955ae-6487-446c-9257-0716097a9f33)(content(Whitespace\" \
         \")))))((Secondary((id \
         b375eef3-cec3-485b-a38e-1c67e897319b)(content(Whitespace\" \
         \"))))(Tile((id \
         a834af07-de84-456c-bd73-62c020367235)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         0d719393-3ee1-4354-b2ef-4237e6792a49)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         08b3171d-c456-4c9d-90a2-15f7be5c40f1)(content(Whitespace\" \
         \"))))(Tile((id \
         d7f6388d-b809-466e-83de-585d71ffee7c)(label(1.))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         9feb45b2-455a-4c65-867c-b52c870a7bfc)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 16))(sort Exp))((shape(Concave \
         16))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         f45233ef-9aa6-4b3b-a823-c6e50d713899)(content(Whitespace\" \
         \"))))(Secondary((id \
         17715bae-682d-4f0a-a823-afeba96c4317)(content(Comment \
         #err#))))(Secondary((id \
         713db1b1-9bd3-47ff-8f5f-e0430b1e5c69)(content(Whitespace\"\\n\"))))(Tile((id \
         e81b155e-d3be-44a5-a6ea-77de93eb55cf)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         056c50d0-5bbe-4d9c-aa02-82ef438b87c4)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         043b9ece-ebc9-451a-b290-7172b63c9d07)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 2))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         69ab3033-df77-4236-8146-8a44543dd271)(label(if then else))(mold((out \
         Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         13))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         80013202-921e-4217-839a-f8529889afaa)(content(Whitespace\" \
         \"))))(Tile((id \
         ad93347c-62c0-4b3f-89f3-b21c87f13fee)(label(true))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         37683a2a-fd0e-45ac-a70c-ac005275759d)(content(Whitespace\" \
         \")))))((Secondary((id \
         81c5a39d-cb24-4e1a-ab64-3ceadf99c677)(content(Whitespace\" \
         \"))))(Tile((id \
         0d7fd7b9-51e1-4d71-8726-cc48db06fb4e)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         92ffa4ac-0a8d-4a90-9fd8-52eaae6ec3ab)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         98c632bc-bd76-4579-afea-b16084f6d9ee)(content(Whitespace\" \
         \"))))(Tile((id \
         5a649e1e-8a58-403b-8ae6-f223d71f2969)(label(1.))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         a41ef3b0-6648-4e1f-b11a-d1389eaeb977)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 16))(sort Exp))((shape(Concave \
         16))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         6330fc76-d937-4dec-95de-19a3b41a37f9)(content(Whitespace\" \
         \"))))(Secondary((id \
         784084cc-19ef-4bf4-ae69-a058ee88bea2)(content(Comment \
         #err#))))(Secondary((id \
         6dd9573c-3237-429d-9f67-fa0d3a527ed3)(content(Whitespace\"\\n\"))))(Tile((id \
         e519a01d-cd0c-4303-8786-16c74b0e28e2)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         b7d43d16-9dd4-4778-95c9-bd6072924dfb)(label(fun ->))(mold((out \
         Exp)(in_(Pat))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         14))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         d529608c-40dc-4a8c-b119-bee30c8b71f8)(content(Whitespace\" \
         \"))))(Tile((id \
         11558ad7-43da-468c-8b47-5f9cb488f89e)(label(?))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         f5c16f08-3051-4023-aabf-3e1bb1df78e1)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         c6b8f9e4-6282-4559-9e72-9d225d03b1d3)(content(Whitespace\" \
         \"))))(Tile((id \
         f7c8f41a-07fb-4e4a-b5ea-629ae0f2ccd4)(label(?))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         e5bd26eb-70de-4d49-9c56-409b67357cc7)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 2))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         74ef60a5-13d3-4e2f-a547-f3989ce94472)(label(if then else))(mold((out \
         Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         13))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         92eac166-584e-479b-b29b-9f99d8b249b1)(content(Whitespace\" \
         \"))))(Tile((id \
         371202af-d5bb-4cdf-bd65-27c8b52f92e4)(label(true))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         cb068475-531a-4927-b6a1-22e4e9979b50)(content(Whitespace\" \
         \")))))((Secondary((id \
         ee59bc84-5e63-4ae8-a3c6-737e08a12db8)(content(Whitespace\" \
         \"))))(Tile((id \
         725e1f9c-e3d8-4e53-9a89-ae1a18604e66)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         635f53a8-1cbd-4708-8e50-9d63dca1547c)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         22dc26ab-904f-40c4-8dee-00f3305b3ae1)(content(Whitespace\" \
         \"))))(Tile((id \
         887151b3-cf70-431c-8d2b-3a27fc023f83)(label(1.))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         1e9cfcc6-d0cb-4b57-82a8-e7ea35bc3205)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 16))(sort Exp))((shape(Concave \
         16))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         09735344-80ff-4b00-a349-0a6b15010e36)(content(Whitespace\"\\n\"))))(Tile((id \
         5115c75e-67dc-46f3-a332-c00552804ebb)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         931e6408-0169-484d-971e-771107a34253)(label(fun ->))(mold((out \
         Exp)(in_(Pat))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         14))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         f045fc24-9608-4059-a6ba-21e53ed08f31)(content(Whitespace\" \
         \"))))(Tile((id \
         6eb2dfc0-ff7d-414d-83a4-9f931d62c219)(label(_))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         bb26f97d-b97b-4bb7-8bdb-fddecaa2694e)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         9136d4b6-ce1e-4392-9601-3d0c035a14ca)(content(Whitespace\" \
         \"))))(Tile((id \
         cfa13869-9098-440c-9570-f22d5a3f7add)(label(?))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         936129f1-6df6-49ee-b559-3bdbc23aae6f)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 2))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         1509b818-82f0-459a-92a2-20e0da30ad57)(label(if then else))(mold((out \
         Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         13))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         8dcb0e86-df5c-4570-a764-84e0884e146d)(content(Whitespace\" \
         \"))))(Tile((id \
         6602d0f7-cd0a-47f8-b6ad-bcea5de32084)(label(true))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         8a7041a4-636f-45e2-9cd6-f662e6fd6e8d)(content(Whitespace\" \
         \")))))((Secondary((id \
         4110f679-2c69-4fc3-abec-a40becaacd07)(content(Whitespace\" \
         \"))))(Tile((id \
         3bdebb97-27d7-46a0-9d87-91c04c4dffae)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         f3e7fd6d-42f7-4d19-8e3f-2b2db52cb74e)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         f6db3b30-d35e-4b07-aa59-45cf8062b207)(content(Whitespace\" \
         \"))))(Tile((id \
         28941dd4-506f-4e80-a8f4-fc2bb43031a8)(label(1.))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         76a1e448-41f0-400c-b7ea-a399a8f5eff7)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 16))(sort Exp))((shape(Concave \
         16))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         c6b13e8b-ef44-42eb-a13a-b117143507c3)(content(Whitespace\"\\n\"))))(Tile((id \
         b835c649-13a0-4402-9809-71a72688b29c)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         c9b83db9-29c1-4eff-b5cd-d46a43a263fc)(label(fun ->))(mold((out \
         Exp)(in_(Pat))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         14))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         9ed6e16a-06d8-47fb-a78d-edf20205444a)(content(Whitespace\" \
         \"))))(Tile((id \
         ae369f54-62ef-4e0b-b0ed-972da618fa40)(label(_))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         586adcad-0933-4a26-8699-916831c7baca)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 12))(sort Pat))((shape(Concave \
         12))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         58f64c30-9c76-4f77-a0c0-9ffc572ad5c5)(content(Whitespace\" \
         \"))))(Tile((id \
         1bc83856-75a5-47dc-b03f-aeaf9e5fd6e3)(label(?))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         9ec96788-240f-4611-bb39-c439232ca9b4)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         9e8fe634-54dd-443d-ab4a-521abd8964c2)(content(Whitespace\" \
         \"))))(Tile((id \
         3e7fc2eb-06bd-4be2-abaf-584b32e12d45)(label(?))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         ba595eed-171d-420a-b857-5de42ad13c8d)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 2))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         b65bd133-da62-44f4-9704-ca8bc1e0ad62)(label(if then else))(mold((out \
         Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         13))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         6e1dc7f8-c2b3-40a6-b611-1b7f180439d8)(content(Whitespace\" \
         \"))))(Tile((id \
         c1e9a35d-4c3b-4fae-964e-9410da8277c6)(label(true))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         75e4bbe2-89a0-4793-81ab-a7834cec12dc)(content(Whitespace\" \
         \")))))((Secondary((id \
         f76786ab-d167-41ff-8e29-62be967bd46d)(content(Whitespace\" \
         \"))))(Tile((id \
         d75262a6-3d43-4a3c-bc32-2bd23385a892)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         623f7820-12b8-47a4-8822-e05fa8adda06)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         e62d00c1-aff2-4b59-8b21-b1bc3862a939)(content(Whitespace\" \
         \"))))(Tile((id \
         324bcc52-4e3a-4618-bfde-19b16def2eb0)(label(1.))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         0ffbf675-d30a-44a6-a433-d5a0468a4a23)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 16))(sort Exp))((shape(Concave \
         16))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         160ffa92-f86d-4ad3-9acb-d07fee5e3cc1)(content(Whitespace\"\\n\"))))(Tile((id \
         46b256a9-ab08-4cb7-ab0e-5041143d5a2a)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         eeae076c-8bbe-4278-84fb-bc2ba52e1f65)(label(fun ->))(mold((out \
         Exp)(in_(Pat))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         14))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         8fa87a25-e4ed-41fe-955e-46434c5eac42)(content(Whitespace\" \
         \"))))(Tile((id \
         b786ec13-021e-4dd3-a6d1-4a91b20ff7a3)(label(_))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         78bf8675-87f4-4243-abd1-ce3715b2113a)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 12))(sort Pat))((shape(Concave \
         12))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         57bb5cca-14a0-47ec-9503-cf45558ffe98)(content(Whitespace\" \
         \"))))(Tile((id \
         f3a6ccc8-137d-4842-a5af-393bb13039ee)(label(Int))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         4eae28bb-5202-482e-a426-8ea5de90686e)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         8ca9da11-11a3-4737-8491-33d1cdc88f14)(content(Whitespace\" \
         \"))))(Tile((id \
         ac57a649-a903-4e08-8fda-7c24db523cf4)(label(?))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         6b8568ad-1c91-4af8-b040-d60b863dfabc)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 2))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         44e841ae-1a43-422e-b403-5ddac077aa63)(label(if then else))(mold((out \
         Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         13))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         643d2bde-bf48-49dd-ada0-17db9156819b)(content(Whitespace\" \
         \"))))(Tile((id \
         a73bd640-e074-4b47-93f1-7427b61a41f8)(label(true))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         015da2cc-d1ef-4475-be3a-cdd796acf727)(content(Whitespace\" \
         \")))))((Secondary((id \
         9acb2c30-cf6a-4c24-87df-62bacf1bcfd2)(content(Whitespace\" \
         \"))))(Tile((id \
         be1dca30-6ba9-4c5b-b436-2e3a6eb7487d)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         5379412a-df8b-41d6-9f7c-6c8094166b3f)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         5cfc16ae-ad03-47fe-91b1-4ad2d5d44dd2)(content(Whitespace\" \
         \"))))(Tile((id \
         8d2eea1a-bade-4543-8363-3081f99bd97c)(label(1.))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         033e58ff-4a73-4b04-8762-634a32b11c52)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 16))(sort Exp))((shape(Concave \
         16))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         7e626298-c9e0-40b4-ab5b-40ebeab4d0bb)(content(Whitespace\" \
         \"))))(Secondary((id \
         406feb6e-d83b-415b-a521-d415c5fbe936)(content(Comment \
         #err#))))(Secondary((id \
         96e2f8b6-8d35-45b2-b13e-75766b304fea)(content(Whitespace\"\\n\"))))(Secondary((id \
         f7cc02ea-9b90-4eee-aae8-3c74a49e26c8)(content(Whitespace\"\\n\"))))(Tile((id \
         b422ffc8-ca0c-4d6c-bfaa-8571225406a5)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         17))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         da974ecf-0447-4ce1-82a7-74d6055a2ba0)(content(Whitespace\" \
         \"))))(Tile((id \
         45d6fa4e-07bd-44bf-8a40-7c2265465b4f)(label(_))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         37cde378-ebb2-4488-8317-37496fe3d043)(content(Whitespace\" \
         \")))))((Secondary((id \
         82e0f7e1-1031-4fab-9e21-20e32a175135)(content(Whitespace\" \
         \"))))(Tile((id 76114edd-a5ab-4b9e-8603-1e9407eadd56)(label(fun \
         ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 14))(sort Exp))))))(shards(0 \
         1))(children(((Secondary((id \
         714ff5e2-8029-414a-afaa-abfdbd0695f7)(content(Whitespace\" \
         \"))))(Tile((id \
         587277fd-943e-4244-82ce-68a0e05cbacd)(label(x))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         f118590e-ab4f-4ae2-9a6e-552a0f9ab0e5)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         5e62d229-6ea4-4ff5-9fa5-bcbf1c2f7e13)(content(Whitespace\" \
         \"))))(Tile((id fd45d53e-f75e-4dc3-baae-4ccbeb673351)(label(if then \
         else))(mold((out Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 13))(sort Exp))))))(shards(0 1 \
         2))(children(((Secondary((id \
         7837008b-e3bb-42f8-8867-a9127126e4fa)(content(Whitespace\" \
         \"))))(Tile((id \
         49522309-0a28-4550-bd68-6f4af81b20f0)(label(true))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         deb3985a-b3eb-4209-81cb-51edfd8e9b11)(content(Whitespace\" \
         \")))))((Secondary((id \
         cfb4ee9d-b12f-41d3-b7b4-41b57bb76396)(content(Whitespace\" \
         \"))))(Tile((id \
         759039b7-c39e-4917-ad26-7768c2441a1e)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         96676838-abac-4fb0-b053-f3c5a31d4c00)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         e426c696-2fda-4120-8fef-c8ff8da2e6dc)(content(Whitespace\" \
         \"))))(Tile((id \
         e1cc2fdb-246d-4f22-8af0-bccbdb56f732)(label(1.))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         cc99b5f0-a62b-46df-b757-e5a8675aaa6e)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         4ac6d11a-9dd9-4e16-9a51-21e1e8b031ec)(content(Whitespace\" \
         \"))))(Secondary((id \
         fdcc919b-9f31-4ef2-abc4-5dea77326375)(content(Comment \
         #err#))))(Secondary((id \
         c4f89285-e554-459e-9bec-d31e32440b0b)(content(Whitespace\"\\n\"))))(Tile((id \
         329514a6-42d8-4d4e-bcb2-bfb8d7a8cb25)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         17))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         50c4e531-b9e0-4f96-a84d-0c99b30659e0)(content(Whitespace\" \
         \"))))(Tile((id \
         7f89c831-f210-451f-b524-fa4aa79d0f18)(label(_))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         9ced8341-5264-41ef-b296-25bf935fa376)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 12))(sort Pat))((shape(Concave \
         12))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         8dac34f3-bed8-4320-aeee-0c44f64ce226)(content(Whitespace\" \
         \"))))(Tile((id \
         f9d98bae-249a-4c23-831c-623bcfc26fd6)(label(?))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         5c4eb7df-2a0f-4d79-989d-4b0517785895)(content(Whitespace\" \
         \")))))((Secondary((id \
         89593eff-af8d-4a21-9229-004daae8c739)(content(Whitespace\" \
         \"))))(Tile((id eb933724-46a8-4882-9f06-364158da5956)(label(fun \
         ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 14))(sort Exp))))))(shards(0 \
         1))(children(((Secondary((id \
         807a029f-2ebb-4f26-88cb-30137df3bc63)(content(Whitespace\" \
         \"))))(Tile((id \
         cad8bfd8-9f38-42ff-845e-29709821f378)(label(x))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         9ee80e5c-817e-48dd-8512-1e344c939a8e)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         cad256b6-05a8-4712-aa63-8507bd913e6f)(content(Whitespace\" \
         \"))))(Tile((id ccfb67ea-c47d-4339-a212-5c08a3ccb2fb)(label(if then \
         else))(mold((out Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 13))(sort Exp))))))(shards(0 1 \
         2))(children(((Secondary((id \
         cae5d396-d39f-491c-b923-f0010827f252)(content(Whitespace\" \
         \"))))(Tile((id \
         e23b54ae-2077-47ac-bc29-2832e0f2ef68)(label(true))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         cb855eeb-2e65-41c5-b458-953806b60c33)(content(Whitespace\" \
         \")))))((Secondary((id \
         71b12abe-3a00-4cf5-a7e1-f7c0523c4581)(content(Whitespace\" \
         \"))))(Tile((id \
         4f36fea3-e604-4f77-a0dc-26e4563d3a04)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         b2ac5c70-f396-480d-95a6-e40ecd776ecd)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         e52c206b-91f1-4d12-9c38-39dfa2ce4fba)(content(Whitespace\" \
         \"))))(Tile((id \
         479a7dff-73f8-4f19-a00e-975f0647dc77)(label(1.))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         46812a90-5acc-4c22-a5ce-92233646948b)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         5ca0db43-c425-4617-ad97-a87fe9306f8a)(content(Whitespace\"\\n\"))))(Tile((id \
         4d7fe960-a87f-4ce5-ad32-aa8581035007)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         17))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         73b23123-29c3-4fb7-9b8a-05693f256733)(content(Whitespace\" \
         \"))))(Tile((id \
         13a3d926-b70d-41ff-8566-3c4c9b2bf99c)(label(_))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         423e0344-be2f-41a9-a456-ce1049a8b0b1)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 12))(sort Pat))((shape(Concave \
         12))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         5d7a831b-b8fd-4762-960d-0fba8f5fde2f)(content(Whitespace\" \
         \"))))(Tile((id \
         28842be8-0534-4ab1-bf86-12781a28769b)(label(?))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         36039aad-4cd9-48c2-a1bf-abfb4dc37cb3)(content(Whitespace\" \
         \"))))(Tile((id \
         0b7ad8bb-266d-46bf-8134-d1bc1ddc8e2d)(label(->))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 5))(sort Typ))((shape(Concave \
         5))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         08aba5c3-d8c5-4297-b74a-a4d302a166d9)(content(Whitespace\" \
         \"))))(Tile((id \
         62589bdb-8179-451e-8780-c8e4bf33b206)(label(?))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         6abab41e-89c9-43cd-b7f1-457d5df8684f)(content(Whitespace\" \
         \"))))(Secondary((id \
         60ba755b-6f24-4592-8b65-45adcfae5b02)(content(Whitespace\" \
         \")))))((Secondary((id \
         cdcbdfd1-d436-4c98-8e0c-327157bb3451)(content(Whitespace\" \
         \"))))(Tile((id a705045f-412e-4dad-a2be-d56a1d362101)(label(fun \
         ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 14))(sort Exp))))))(shards(0 \
         1))(children(((Secondary((id \
         f65af529-4319-4fe6-a9c8-1b716012603d)(content(Whitespace\" \
         \"))))(Tile((id \
         7cea72cc-149b-4e79-a93b-1b192f0668b8)(label(x))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         154047bc-4f40-4d89-90e7-fe3a0d15b2ae)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         70ab61a2-1bf1-470e-9950-a01f76d181d3)(content(Whitespace\" \
         \"))))(Tile((id 4c2b76d8-f6cf-4832-946b-505151bfe46a)(label(if then \
         else))(mold((out Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 13))(sort Exp))))))(shards(0 1 \
         2))(children(((Secondary((id \
         c36fdf84-9ff3-4eab-8ef7-973b0ba6a57f)(content(Whitespace\" \
         \"))))(Tile((id \
         8d5079eb-a0e3-40ef-abc3-b075fc44edee)(label(true))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         02f37a6c-8ff6-4798-ab58-7b69b6c17f52)(content(Whitespace\" \
         \")))))((Secondary((id \
         0dbdda0c-6ef3-43bd-af0e-da537f0d2318)(content(Whitespace\" \
         \"))))(Tile((id \
         48fbe76e-cf73-4bf3-9b0a-8e0c199f3303)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         92b2cd79-7544-4351-9f37-c05ef9093507)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         20d3283e-389a-4641-ad6b-5e1484f4d9b5)(content(Whitespace\" \
         \"))))(Tile((id \
         6f4418d6-ae55-4493-98ed-0497267a72de)(label(1.))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         78d8f6db-ed2e-4899-8433-68b9625994a6)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         7c24fdf1-2e36-41bf-bae7-c09f116607b7)(content(Whitespace\"\\n\"))))(Tile((id \
         e2cd3fad-540f-415d-8043-63fc31f3b221)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         17))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         89b58c46-a94f-453a-965e-a6ba9be57212)(content(Whitespace\" \
         \"))))(Tile((id \
         c76e67a6-df03-44f2-8536-e7936f3fed5d)(label(_))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         ff253a88-bd76-45b4-a989-9d9d343aa32d)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 12))(sort Pat))((shape(Concave \
         12))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         616d4a4d-a668-4e7a-9da3-340b7ce802f1)(content(Whitespace\" \
         \"))))(Tile((id \
         0458726d-4ccf-483c-be1d-547c65ad6b0a)(label(?))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         0006a920-97ec-4970-8b1f-cf1a597d3db5)(content(Whitespace\" \
         \"))))(Tile((id \
         6f5f976f-30ca-4e69-8579-f7ae2a108465)(label(->))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 5))(sort Typ))((shape(Concave \
         5))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         fd18ef4f-b609-4bfe-94e0-d296847bc935)(content(Whitespace\" \
         \"))))(Tile((id \
         24797b99-a831-4b3c-9560-52080fa62216)(label(Int))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         0f23f05b-a326-45bb-822d-1b8e51cb2306)(content(Whitespace\" \
         \")))))((Secondary((id \
         98d58acd-37ec-406b-8465-291fed880866)(content(Whitespace\" \
         \"))))(Tile((id bd301626-525b-43fe-9cda-be70912b2f7d)(label(fun \
         ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 14))(sort Exp))))))(shards(0 \
         1))(children(((Secondary((id \
         3d82ed17-21a6-4257-abe3-805513615e48)(content(Whitespace\" \
         \"))))(Tile((id \
         7931c017-d228-47da-a700-c35ee6b0eeb7)(label(x))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         4959e868-d1af-4cd0-a0fd-5dd704b3858a)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         68aea425-3c57-4a6f-a7d2-2c6cc0a87600)(content(Whitespace\" \
         \"))))(Tile((id 1de74948-95d6-4086-9e54-46ddefc6bdb0)(label(if then \
         else))(mold((out Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 13))(sort Exp))))))(shards(0 1 \
         2))(children(((Secondary((id \
         77837ee0-6d6f-4bd9-8ba8-3156b0b4db67)(content(Whitespace\" \
         \"))))(Tile((id \
         80e6a753-5adb-4f2b-984a-50a51e9fed27)(label(true))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         3183cb38-3f1f-460e-a208-2a0f22b10f22)(content(Whitespace\" \
         \")))))((Secondary((id \
         7bdded42-9f8d-4680-9293-122093727b04)(content(Whitespace\" \
         \"))))(Tile((id \
         e123ec12-e7cc-4aa7-9154-79c244bd5228)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         158e823c-6574-41b9-9424-b9126a6205d9)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         87c83d9d-6bc3-4197-bcbe-c8e906b497c2)(content(Whitespace\" \
         \"))))(Tile((id \
         8961a915-6957-406a-ab70-0da17acdafc8)(label(1.))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         e78aabd6-b2ad-4342-87ac-6740ea32c8ed)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         7d87fae4-a792-4377-92c6-14316ea90783)(content(Whitespace\" \
         \"))))(Secondary((id \
         b7bd0e63-0e4d-470e-83e8-e721992d87eb)(content(Comment \
         #err#))))(Secondary((id \
         177e50f2-8b1b-420b-b35f-fd34fee54ea6)(content(Whitespace\"\\n\"))))(Tile((id \
         20b66c80-1b9f-40ee-8eed-868afc146b88)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         17))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         28692c79-024d-45a8-8b4a-5654d5df86d7)(content(Whitespace\" \
         \"))))(Tile((id \
         179613fb-c76a-4a04-bb32-b3cbfc6f9c3e)(label(_))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         b23d5491-2fc4-4941-baa1-52a2e48b2ebe)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 12))(sort Pat))((shape(Concave \
         12))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         85989bf6-e65b-4e0d-966f-c79e1a6e2ccd)(content(Whitespace\" \
         \"))))(Tile((id \
         d4fbd548-6f00-4b4c-8ce6-f914b2f927ec)(label(?))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         fce4ca15-e39e-4de1-9f25-c9a05a197444)(content(Whitespace\" \
         \"))))(Tile((id \
         57be36ff-3c4d-4d97-a859-7cf78407f249)(label(->))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 5))(sort Typ))((shape(Concave \
         5))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         c0aec7b6-dce2-4e31-8a95-afe85d6ad196)(content(Whitespace\" \
         \"))))(Tile((id aeaa175c-40d2-4078-a79d-45a97002ae1e)(label([ \
         ]))(mold((out Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape \
         Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
         da4b5c25-5b93-47b9-a4e5-792658f4d1d7)(label(?))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children()))))))))(Secondary((id \
         1e6a7d87-0b88-4478-ad9d-8076310a0c65)(content(Whitespace\" \
         \")))))((Secondary((id \
         cc0cb60b-75fc-4450-ac42-3cafdb5cfeba)(content(Whitespace\" \
         \"))))(Tile((id ae53d282-5098-461b-ab95-5f058bc2e7a5)(label(fun \
         ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 14))(sort Exp))))))(shards(0 \
         1))(children(((Secondary((id \
         1bd72cb0-1037-4909-b674-45985ac7c1d5)(content(Whitespace\" \
         \"))))(Tile((id \
         0d421c83-c42c-4774-9249-79434643b47d)(label(x))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         adc3d9fe-730b-4f28-b367-5decaeaed27d)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         71b89864-aedf-4a6f-b7d8-a16daa20d32c)(content(Whitespace\" \
         \"))))(Tile((id bd635118-71f8-492d-9f8d-faa475c8d7e8)(label(if then \
         else))(mold((out Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 13))(sort Exp))))))(shards(0 1 \
         2))(children(((Secondary((id \
         21dd2afd-0e64-41be-aef1-b8b1b331715a)(content(Whitespace\" \
         \"))))(Tile((id \
         595c431d-1852-49fd-92b7-2aec39c6f666)(label(true))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         255e2326-12f6-411c-a6ac-ee4531c8ca08)(content(Whitespace\" \
         \")))))((Secondary((id \
         a0b9b432-9747-4c8e-a7ae-e7419db9c4de)(content(Whitespace\" \
         \"))))(Tile((id \
         a8973628-7c1f-4ed5-8827-3d539d535199)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         79a281fd-0316-4d3a-a0e1-c3f1f814246e)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         bbfb1efc-a7eb-4696-8cf7-e8f9f3c6ad72)(content(Whitespace\" \
         \"))))(Tile((id \
         00309486-c9af-4d2d-90c8-bc66a511c06a)(label(1.))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         2379e9e8-6f16-4225-9a6e-f95440f8c62b)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         a2eef8d8-db3a-4635-a4b5-404c4c0921f2)(content(Whitespace\" \
         \"))))(Secondary((id \
         9612e37e-8400-48ce-948e-d2cafd3095aa)(content(Comment\"#2x \
         err#\"))))(Secondary((id \
         832c68fe-94db-4f81-aaf6-94d9c7c77863)(content(Whitespace\"\\n\"))))(Secondary((id \
         a5a3b845-0842-4d91-8194-0b98b729c1d5)(content(Whitespace\"\\n\"))))(Tile((id \
         16a37cbe-dc5b-4227-b7bc-7f391876ab5f)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         e22a7750-a4f2-4ebd-93c0-175b6c723266)(label(?))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         91727912-05e6-454e-9782-5e618fe6e70a)(label(::))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 7))(sort Exp))((shape(Concave \
         7))(sort Exp))))))(shards(0))(children())))(Tile((id \
         8a13034f-7c7e-49fc-af4a-44261c19e927)(label([ ]))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         d9ddffb5-5c0f-41ac-8d9f-7631ca1d2ada)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         f10d6e13-da55-479f-abf2-12475f349978)(label(if then else))(mold((out \
         Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         13))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         fbfc5089-c87a-492b-9743-1636d471050e)(content(Whitespace\" \
         \"))))(Tile((id \
         9fd0c3db-965c-4c6f-a53f-1ea2a56b6b2c)(label(true))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         34fb2388-8f32-4f6b-8848-4c0315ccc6e8)(content(Whitespace\" \
         \")))))((Secondary((id \
         6a36d538-f325-4018-9415-eaf91fce7bca)(content(Whitespace\" \
         \"))))(Tile((id \
         cf3818bc-d193-4ad1-916b-f59ce6f9734e)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         af8e1c2b-974d-40dd-85e3-17be1e2eb285)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         b2fde3fe-6850-4aa1-9479-e300e01f0935)(content(Whitespace\" \
         \"))))(Tile((id \
         f730e703-9baa-4434-88c3-4d954f0bddca)(label(1.))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))))))))))))(Tile((id \
         e78aaab0-80e3-4256-ada1-49c56d6a86ce)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 16))(sort Exp))((shape(Concave \
         16))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         4452164f-3c96-43fd-b93b-fea5fd8118d1)(content(Whitespace\"\\n\"))))(Tile((id \
         b4e13b72-e4fd-4f1e-8453-c0dcbb78a057)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         3030b792-98fc-486e-b2a4-98094744700e)(label(::))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 7))(sort Exp))((shape(Concave \
         7))(sort Exp))))))(shards(0))(children())))(Tile((id \
         92d82649-57d6-4704-8d92-0cf013e1ebd6)(label([ ]))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         778d103f-be70-48ab-87e6-200b7accbed0)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         a0fdeb11-83f6-4283-a2bd-02a8680c87a5)(label(if then else))(mold((out \
         Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         13))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         bb78e77d-600d-487e-80f0-9fb38fd7a115)(content(Whitespace\" \
         \"))))(Tile((id \
         053aded7-442e-46c2-9f5a-7039c194a87e)(label(true))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         8f55ad96-a029-4f0c-bdab-e870748666d4)(content(Whitespace\" \
         \")))))((Secondary((id \
         1b67207d-7319-426b-90de-7732f2a13e87)(content(Whitespace\" \
         \"))))(Tile((id \
         744bec07-0822-4c77-b708-c0927855d960)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         12016f4c-9951-450c-b498-eee8dc7ae769)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         3c3d18a0-0022-4ab9-9e40-02e6eaa6ee34)(content(Whitespace\" \
         \"))))(Tile((id \
         297dbdb6-6b1d-431d-9a23-3eb22e184cc1)(label(1.))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))))))))))))(Tile((id \
         bcf15c13-6bd4-455d-815c-b295ff1739af)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 16))(sort Exp))((shape(Concave \
         16))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         36542426-c71a-437e-858b-bd3cc5922797)(content(Whitespace\" \
         \"))))(Secondary((id \
         ff23ec41-8dd7-4d7c-91a2-339224f8fb09)(content(Comment \
         #err#))))(Secondary((id \
         8cae60cd-016a-4424-9157-716529f04a4d)(content(Whitespace\"\\n\"))))(Tile((id \
         32e2f80b-d83a-4fe6-8ee8-ec4713973ee9)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         ff11824e-80e8-4f41-9f47-52c3f22de996)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         82717f81-0aa9-46ab-af23-5f4f5bab270b)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 15))(sort Exp))((shape(Concave \
         15))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         62191de8-30df-4984-aef7-e5813cafa6ed)(content(Whitespace\" \
         \"))))(Tile((id \
         9440e8cf-5a51-47d9-a888-6ed38bcd6309)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         94e4713b-16a1-463e-ae3a-15f3e4ca475d)(label(::))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 7))(sort Exp))((shape(Concave \
         7))(sort Exp))))))(shards(0))(children())))(Tile((id \
         6b5bca5d-99b3-426d-9151-191bb81b4016)(label([ ]))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         1917600e-8146-4ae0-921d-a38961fde2c7)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         5b77bc26-647d-49d6-b857-66949d31fad5)(label(if then else))(mold((out \
         Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         13))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         2d644fdf-b4d6-4f76-90de-b093805bffe2)(content(Whitespace\" \
         \"))))(Tile((id \
         c35f9fdc-6716-401a-aeb2-1ca1fdb8e2b7)(label(true))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         2e6140a9-617b-4c61-86c2-d5a6efbef23f)(content(Whitespace\" \
         \")))))((Secondary((id \
         11450632-e5fe-4edb-93b3-0c033bd175f8)(content(Whitespace\" \
         \"))))(Tile((id \
         9d630777-ffe3-4662-aebe-227954581acc)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         8f9ac4d3-711f-44e2-bb70-41cf3a5d7ced)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         e0b40035-4d13-47ab-a4f3-c5f97c8e2923)(content(Whitespace\" \
         \"))))(Tile((id \
         3010a196-1dba-4c06-8d15-b416e805b961)(label(1.))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))))))))))))(Tile((id \
         ee6da9f8-bc92-4b2d-8a86-46007a1bb18a)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 16))(sort Exp))((shape(Concave \
         16))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         1c2b19b1-999e-4f1d-907a-324df3549257)(content(Whitespace\" \
         \"))))(Secondary((id \
         f9ae0f2b-9d16-4ab2-94eb-a1df830bba5c)(content(Comment\"#2x \
         err#\"))))(Secondary((id \
         715595f7-c907-4139-b8eb-e9d61e3dd88e)(content(Whitespace\"\\n\"))))(Secondary((id \
         bf573312-7a77-4788-b725-962b09bb83d7)(content(Whitespace\"\\n\"))))(Tile((id \
         eaed346f-f9a1-4571-a6b0-2408f3b5aa35)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         17))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         24ba7e71-c265-47d8-b043-fb9571296c81)(content(Whitespace\" \
         \"))))(Tile((id \
         a721ed93-334e-4a5e-b279-54818064df5d)(label(?))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         9dd3b184-e240-418f-8237-48eeb811a098)(content(Whitespace\" \
         \")))))((Secondary((id \
         34d97d72-dbee-4cda-92ca-02e3b0813434)(content(Whitespace\" \
         \"))))(Tile((id fe7ce7dc-ccf0-41cf-b61e-58713886b996)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         b10593ba-bb07-4ff5-9bd1-f22ed0783aa9)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         e85e28c3-991c-46e9-9598-42b2477c865e)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 15))(sort Exp))((shape(Concave \
         15))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         7dc5f42b-8d36-4373-9d0f-8843fea91a1b)(content(Whitespace\" \
         \"))))(Tile((id \
         7ddf8712-61ea-4905-b497-add09996d1c4)(label(1.))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         eaf9b2dd-0d47-4af1-a5c8-061891ac8f7d)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 15))(sort Exp))((shape(Concave \
         15))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         08002c61-30fe-4b28-977a-6e168f820884)(content(Whitespace\" \
         \"))))(Tile((id \
         9f837009-fd6a-42e2-9e5e-3b311d3134cf)(label(true))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         91bbf839-f2fc-4312-af44-e8fe70a58b0e)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         34cb07ec-c128-4bae-b22b-603778514237)(content(Whitespace\" \
         \"))))(Secondary((id \
         edf1f302-ba8d-4a9f-b713-fc18c8beb475)(content(Comment\"#err: \
         inconsistent#\"))))(Secondary((id \
         1d21fb06-5b8c-403d-9347-ccf6b2fb7427)(content(Whitespace\"\\n\"))))(Tile((id \
         8dffbed9-b88c-47d9-a1c0-5b831c9a1e72)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         17))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         821485af-473b-4a90-bd5b-7589ed9dab33)(content(Whitespace\" \
         \"))))(Tile((id \
         eef784bc-4e2e-4903-9016-cf8fc863ca19)(label(_))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         10b05fdd-ddae-4124-b9b1-21decc4828d0)(content(Whitespace\" \
         \")))))((Secondary((id \
         aaf6379a-c330-4db6-af7b-4dddf9f0b575)(content(Whitespace\" \
         \"))))(Tile((id d8e9da41-110d-4de9-a7de-b44ff0038f24)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         8b2911fa-2a94-48f5-a573-d17e83dec5b3)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         c1b21d5f-f1cf-4268-8f26-83b92873027a)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 15))(sort Exp))((shape(Concave \
         15))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         26a10e17-9393-4593-8a58-59124dc53d1d)(content(Whitespace\" \
         \"))))(Tile((id \
         66740b97-c103-4946-a427-e7b398cbb185)(label(1.))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         a791cfe5-5851-430a-ac3d-e0d46e883523)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 15))(sort Exp))((shape(Concave \
         15))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         aa9f3248-2856-412e-82a8-dd48d95a1c4c)(content(Whitespace\" \
         \"))))(Tile((id \
         7073b5c4-0f33-40ed-9055-9767f0a66124)(label(true))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         7bfde104-a9c0-4f94-9d06-22beeee37d51)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         45117c72-352b-4c5f-af83-8e36735e7e1a)(content(Whitespace\" \
         \"))))(Secondary((id \
         f2b7d7e5-72d4-4d1f-b625-e9c3898e5468)(content(Comment\"#err: \
         inconsistent#\"))))(Secondary((id \
         ab630234-78a5-4b9a-8529-f99de22909a2)(content(Whitespace\"\\n\"))))(Tile((id \
         65f68252-d38e-4ca1-a53b-f5e5695ec473)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         17))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         a6da039f-4d44-4717-b047-1d45349c39ed)(content(Whitespace\" \
         \"))))(Tile((id \
         ddb68bc9-8439-4ce5-8fd0-832fa669dcd4)(label(_))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         52625e6f-568c-4303-adbc-109bccc6e8f3)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 12))(sort Pat))((shape(Concave \
         12))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         e99eb9ec-1890-4588-bc89-81b5d2c7cc89)(content(Whitespace\" \
         \"))))(Tile((id \
         feb58019-e9b3-4ab8-aac6-50469697595e)(label(?))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         f3af026d-83c5-47e3-9808-4da209e81c33)(content(Whitespace\" \
         \")))))((Secondary((id \
         9d41ec99-7d66-4e27-8410-edea51fb1893)(content(Whitespace\" \
         \"))))(Tile((id c8cf18cd-3cce-4141-93e0-262c397f7378)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         7e5af338-1e12-4b77-9ef0-b9435cecbe8e)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         ce3cb6e9-ae9b-426e-8ea1-7d34de49cefe)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 15))(sort Exp))((shape(Concave \
         15))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         eaf9f29f-6f60-4bf4-af45-656bb4cc205c)(content(Whitespace\" \
         \"))))(Tile((id \
         750c9a5d-7491-492b-833c-7ed0f5fac511)(label(1.))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         31ffd780-7310-4df1-8a92-a89e7cff64b5)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 15))(sort Exp))((shape(Concave \
         15))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         d07f6877-43fd-42d8-86b5-d08dba26aa21)(content(Whitespace\" \
         \"))))(Tile((id \
         dfa5256a-64f0-410e-b802-6ba2406cdde5)(label(true))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         54dad9f1-53be-4a73-bd83-b415d7cd8290)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         91d07bd1-6bd3-4208-b25f-6eb9a8d4b664)(content(Whitespace\" \
         \"))))(Secondary((id \
         345852c2-45de-4087-8256-52e284d261d1)(content(Whitespace\"\\n\"))))(Tile((id \
         0a12dd2c-678e-4455-bd6d-5bc663e5dc56)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         17))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         940e0344-3330-4333-831a-4aea460376bc)(content(Whitespace\" \
         \"))))(Tile((id \
         339ffff7-2b83-463e-a854-3c3437ab5d28)(label(_))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         f9197e5c-570e-4ed8-9a4f-29cc5f82adcb)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 12))(sort Pat))((shape(Concave \
         12))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         e48bb0db-e22f-4d30-8935-f1995d203638)(content(Whitespace\" \
         \"))))(Tile((id e17ae24a-882d-4d6a-9868-25a53e79b1f4)(label([ \
         ]))(mold((out Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape \
         Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
         af0fc4b3-ab89-401c-ae3c-0b81a39ec383)(label(?))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children()))))))))(Secondary((id \
         0e9b0fa8-2a42-40ee-9b1f-3df9310411ce)(content(Whitespace\" \
         \")))))((Secondary((id \
         05e3daf7-8ef4-472f-a181-3f8844bbd44d)(content(Whitespace\" \
         \"))))(Tile((id 381b65da-24d4-4d0a-a530-74f29497b0da)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         ab7bdb41-74f4-422c-8a53-61c996a9fe5e)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         86ea2bc8-08b6-4689-a027-a44ba1212a79)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 15))(sort Exp))((shape(Concave \
         15))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         2dff3c19-e5d1-4326-8692-b36e0db3a409)(content(Whitespace\" \
         \"))))(Tile((id \
         a0bc5a40-469c-46f1-8955-5319a19422fd)(label(1.))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         e75a574c-d344-44d5-8d7e-20ae67e3e853)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 15))(sort Exp))((shape(Concave \
         15))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         feee9ac1-d1ab-4645-b57c-7a03164d1678)(content(Whitespace\" \
         \"))))(Tile((id \
         a35b9d67-d86d-4b06-9dd0-4d8be5399211)(label(true))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         6497b3cb-a95d-4442-bcc3-d8090e2e5a22)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         f8e9353a-b5b5-4fbd-8032-b39888fb9411)(content(Whitespace\"\\n\"))))(Tile((id \
         7fd18fd5-b4a4-43eb-8749-0e5931a2c580)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         17))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         4782f42e-6a21-48eb-bca8-a256a683ff8d)(content(Whitespace\" \
         \"))))(Tile((id \
         4f66a6ba-5b88-484a-9a2f-263ac3c401d6)(label(_))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         df6fc56f-db6e-4c65-a21e-d6449d973b24)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 12))(sort Pat))((shape(Concave \
         12))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         f57a9db2-4b27-4d4e-9011-a65d52f06c8d)(content(Whitespace\" \
         \"))))(Tile((id a5fc12b1-b444-47b3-be8f-64b0cf7c3db4)(label([ \
         ]))(mold((out Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape \
         Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
         89de9b90-f6f6-4cdb-887e-9ac4425f51df)(label(Int))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children()))))))))(Secondary((id \
         10368c5e-2795-4710-9718-7eecfcc31f46)(content(Whitespace\" \
         \")))))((Secondary((id \
         374708fd-3c1a-4e62-a8c5-211cee8a2c24)(content(Whitespace\" \
         \"))))(Tile((id 0bc48421-27a1-4721-be67-208b92fd3629)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         3ec40dab-a842-48f6-a98f-58c6c4ffc14d)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         f52a6f4b-48e7-4790-b129-f7dfe605db12)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 15))(sort Exp))((shape(Concave \
         15))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         018e1211-5282-465d-aa1e-c01c5a5dd204)(content(Whitespace\" \
         \"))))(Tile((id \
         90aa3873-9c7d-42ae-9212-56d5ddb7be25)(label(1.))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         2de8f116-03fa-4345-b8fb-409a2713a817)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 15))(sort Exp))((shape(Concave \
         15))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         a606263a-dd65-4228-9392-7320e56a049d)(content(Whitespace\" \
         \"))))(Tile((id \
         5de1c5ad-a26e-4abb-84e2-9d8885d9c503)(label(true))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         e11206dc-f3ba-4c8d-b9d7-3729a189997a)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         59804776-fbab-4e7f-b4c9-5170c38856cc)(content(Whitespace\" \
         \"))))(Secondary((id \
         49a27604-e1ec-4dd3-9ae0-508c79589a59)(content(Comment\"#2x \
         err#\"))))(Secondary((id \
         bd8644c6-63d6-4e85-b190-e9e79a73221a)(content(Whitespace\"\\n\"))))(Secondary((id \
         508de859-8307-4c8c-adec-531192ad39aa)(content(Whitespace\"\\n\"))))(Tile((id \
         09facb73-3cc6-4f2f-a2cb-849b8dfb8905)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         17))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         075b678a-2685-4cb7-b20a-30ec25a8b215)(content(Whitespace\" \
         \"))))(Tile((id \
         f1fd4bc0-60a6-449a-9520-06e22dd15aae)(label(_))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         3ca23a48-76b9-4bb1-b28e-63c4ba2ad8fe)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 12))(sort Pat))((shape(Concave \
         12))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         f2b2ea56-5eca-4416-8fbf-3d5af26920e9)(content(Whitespace\" \
         \"))))(Tile((id 96f67279-339b-424f-a479-60000f7ed062)(label([ \
         ]))(mold((out Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape \
         Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
         f3359e4c-cec1-4f38-8557-14bcf32567ad)(label(Int))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children()))))))))(Secondary((id \
         9daeb52d-95b6-48dc-a7aa-86925d532ead)(content(Whitespace\" \
         \")))))((Secondary((id \
         4b957fd9-eb4b-4937-8d0d-af987a62a66e)(content(Whitespace\" \
         \"))))(Tile((id \
         f09855d7-cd50-4a1f-9ed7-85bc7a93832f)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         1fe29849-3433-4740-997d-668eef8d71bc)(label(::))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 7))(sort Exp))((shape(Concave \
         7))(sort Exp))))))(shards(0))(children())))(Tile((id \
         fddd3561-51a2-491a-97e0-6893154e3b03)(label([ ]))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         7a30df1d-81b1-46fd-8c4a-bfdfaf7539fc)(label(2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         d9a4e603-08c1-48ed-b113-0009ac910bfd)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         a4f64779-84df-4eff-ad25-7229fc9add4e)(content(Whitespace\"\\n\"))))(Tile((id \
         2eaa6225-8b50-4fac-af7c-cdf6ac0f02a6)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         17))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         5f0766be-60e9-482f-8a6e-6a64ce842de5)(content(Whitespace\" \
         \"))))(Tile((id \
         6c0df814-7020-470c-97cc-0f341aca78f3)(label(_))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         fad6a5cb-f485-4b9f-82a3-b8320e4fa334)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 12))(sort Pat))((shape(Concave \
         12))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         492524db-e03a-4c8d-9a86-b26f60766009)(content(Whitespace\" \
         \"))))(Tile((id 115fad2d-7365-4eb3-a443-de1405399eb7)(label([ \
         ]))(mold((out Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape \
         Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
         6f8c6706-b54f-4238-a10a-ff9cf1665bef)(label(Int))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children()))))))))(Secondary((id \
         a3a40f71-1c95-4f85-96a2-fe2a8f17114f)(content(Whitespace\" \
         \")))))((Secondary((id \
         b3ee84c6-5986-4301-8114-32bf685a1d09)(content(Whitespace\" \
         \"))))(Tile((id \
         474aa01d-ec29-4809-9449-d6bdcf751a01)(label(1.0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         bf89fc82-3456-464c-8b15-538542e3e74d)(label(::))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 7))(sort Exp))((shape(Concave \
         7))(sort Exp))))))(shards(0))(children())))(Tile((id \
         9af9f47e-fb39-487c-b81b-d7595b7ed4ec)(label([ ]))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         d174f6ac-82a8-4e02-a620-0c5f4d69469f)(label(2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         d077972b-d5d8-4f44-9782-2eadffb6362f)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         8b3b7181-bdce-48f7-a421-444869d6724f)(content(Whitespace\" \
         \"))))(Secondary((id \
         7fe3f81a-c96a-4136-930e-308dbe1b388a)(content(Comment \
         #err#))))(Secondary((id \
         843fd20e-f889-44d5-9b72-a22c3ff45ac3)(content(Whitespace\"\\n\"))))(Tile((id \
         c332994a-4006-41ad-9b56-aa50ab90e60d)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         17))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         be403d06-669b-481c-9837-90f26e2f9277)(content(Whitespace\" \
         \"))))(Tile((id \
         58666b1c-385c-4fcf-bace-08d280664e93)(label(_))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         0b67de8b-c914-41d4-aded-0de955e9e96f)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 12))(sort Pat))((shape(Concave \
         12))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         383def64-d2d5-49e7-b908-233f1356abd0)(content(Whitespace\" \
         \"))))(Tile((id 959d1342-a60f-467c-9e6b-736a06e3b167)(label([ \
         ]))(mold((out Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape \
         Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
         ed75b169-9e3e-4614-b0ab-bd533e3cb14e)(label(Int))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children()))))))))(Secondary((id \
         c081f642-8c3e-44c5-ac73-7096709f2140)(content(Whitespace\" \
         \")))))((Secondary((id \
         7a586f57-38eb-45d4-b816-acb66c51c1b4)(content(Whitespace\" \
         \"))))(Tile((id \
         9763aeaf-c06f-4b68-a254-a74077d5e931)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         c6fb1be3-3c11-4f07-9fde-7dee7f515a86)(label(::))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 7))(sort Exp))((shape(Concave \
         7))(sort Exp))))))(shards(0))(children())))(Tile((id \
         4560f41b-0387-4d28-925d-c84471ac0707)(label([ ]))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         686f2d71-762a-4ab2-aa74-49b68f2c06bf)(label(2.0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         0be59aeb-1745-4b49-bbb6-8fde7dc8189e)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         863e7f16-0102-446b-b2bf-8b0a77ba6cde)(content(Whitespace\" \
         \"))))(Secondary((id \
         ae46b032-fb59-479e-84bb-18846b40ac4a)(content(Comment \
         #err#))))(Secondary((id \
         a8f1d162-0617-4555-b6cb-a3921504c7fc)(content(Whitespace\"\\n\"))))(Tile((id \
         c50d9505-c1c8-4d29-84d6-fa5b82723196)(label(\"\\\"BYE\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))(ancestors())))(caret Outer))";
      backup_text =
        "# Internal Regression Tests: Type errors #\n\
         # Each line should show errors or not as indicated #\n\n\
         let _ = unbound in #err#\n\
         let Undefined = Undefined in # 2x err# \n\
         let true = 2 in #2x err# \n\n\
         let ? = if true then 1 else 1. in #err# \n\
         let _ = if true then 1 else 1. in #err#\n\
         let _: ? = if true then 1 else 1. in\n\
         let _: Int = if true then 1 else 1. in #err#\n\
         let _: Fake = if true then 1 else true in #err#\n\
         let _, _ = if true then 1 else 1. in #2x err#\n\
         let _, _ = (if true then 1 else 1.),    in #err#\n\
         let _: ?, _ = (if true then 1 else 1.),    in \n\
         let [_] = [(if true then 1 else 1.)] in #2x err#\n\
         let [_] = (if true then 1 else 1.) in #3x err# \n\n\
         (?)(if true then 1 else 1.);\n\
         1(if true then 1 else 1.); #err#\n\
         (1)(if true then 1 else 1.); #err#\n\
         (fun ? -> ?)(if true then 1 else 1.);\n\
         (fun _ -> ?)(if true then 1 else 1.);\n\
         (fun _: ? -> ?)(if true then 1 else 1.);\n\
         (fun _: Int -> ?)(if true then 1 else 1.); #err#\n\n\
         let _ = fun x -> if true then 1 else 1. in #err#\n\
         let _: ? = fun x -> if true then 1 else 1. in\n\
         let _: ? -> ?  = fun x -> if true then 1 else 1. in\n\
         let _: ? -> Int = fun x -> if true then 1 else 1. in #err#\n\
         let _: ? -> [?] = fun x -> if true then 1 else 1. in #2x err#\n\n\
         (?)::[(if true then 1 else 1.)];\n\
         1::[(if true then 1 else 1.)]; #err#\n\
         (1, 1)::[(if true then 1 else 1.)]; #2x err#\n\n\
         let ? = [1, 1., true] in #err: inconsistent#\n\
         let _ = [1, 1., true] in #err: inconsistent#\n\
         let _: ? = [1, 1., true] in \n\
         let _: [?] = [1, 1., true] in\n\
         let _: [Int] = [1, 1., true] in #2x err#\n\n\
         let _: [Int] = 1::[2] in\n\
         let _: [Int] = 1.0::[2] in #err#\n\
         let _: [Int] = 1::[2.0] in #err#\n\
         \"BYE\"";
    } )
