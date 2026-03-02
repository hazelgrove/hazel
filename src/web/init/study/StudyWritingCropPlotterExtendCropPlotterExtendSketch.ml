let out : string * Haz3lcore.PersistentSegment.t =
  ( "Study / writing / crop-plotter-extend / crop-plotter-extend-sketch",
    {
      segment =
        "((Secondary((id \
         de17951d-fc13-44b1-a2e9-92581f38bca0)(content(Comment\"# CROP PLOTTER \
         EXTENSION TASK                     #\"))))(Secondary((id \
         f59ed9c0-fc76-44c2-9de2-382c86be52db)(content(Whitespace\"\\n\"))))(Secondary((id \
         b2cf8d90-9a79-4a72-96b7-db50d6e2cf3d)(content(Comment\"#                                                 \
         #\"))))(Secondary((id \
         462adcb3-8a5b-4bf2-904e-49bc8002ec45)(content(Whitespace\"\\n\"))))(Secondary((id \
         2080a1fa-e948-425e-9e06-a58529be80da)(content(Comment\"# The crop \
         plotter app lets you plant seeds on    #\"))))(Secondary((id \
         3819c46d-7956-4d94-a0ea-e07a85acb7dc)(content(Whitespace\"\\n\"))))(Secondary((id \
         b91e7457-c16b-409e-a678-b6a384a9a9a0)(content(Comment\"# a grid. It \
         already supports planting rows.      #\"))))(Secondary((id \
         9696ded9-5937-4b9b-b4fb-f5db42760650)(content(Whitespace\"\\n\"))))(Secondary((id \
         d11b33cf-5c2c-4aa7-9efd-0cd2b4b41764)(content(Comment\"#                                                 \
         #\"))))(Secondary((id \
         4d574d10-0734-41ad-a31a-4951922f49af)(content(Whitespace\"\\n\"))))(Secondary((id \
         89bc3ed1-2f89-4904-8709-e510b7b1cedd)(content(Comment\"# YOUR TASK: \
         Add a PlantCol action that fills     #\"))))(Secondary((id \
         e0e3283b-3056-4c9a-bd9e-87071d174eff)(content(Whitespace\"\\n\"))))(Secondary((id \
         153d56f7-0d42-4d56-add9-889955c953cc)(content(Comment\"# an entire \
         column with the current seed.         #\"))))(Secondary((id \
         be205fe2-9628-45d9-b324-930172955222)(content(Whitespace\"\\n\"))))(Secondary((id \
         9567941b-bfca-44bf-a04b-f8f6df877854)(content(Comment\"#                                                 \
         #\"))))(Secondary((id \
         55f5bb22-1194-47f4-9708-cb3fad5c3eb5)(content(Whitespace\"\\n\"))))(Secondary((id \
         23f2ce50-f682-42f1-8922-4da962388788)(content(Comment\"# You need \
         to:                                    #\"))))(Secondary((id \
         947ae3df-f8bb-4690-ae11-d06e64ce997e)(content(Whitespace\"\\n\"))))(Secondary((id \
         caf7422f-4dc3-4474-8cac-bd5569e43acd)(content(Comment\"#   1. Add \
         PlantCol(Col) to the Action type       #\"))))(Secondary((id \
         eaa014b6-3083-4e46-ab2a-a1b0136b63b8)(content(Whitespace\"\\n\"))))(Secondary((id \
         64e5e792-1772-4f10-96a7-e07d763d62b8)(content(Comment\"#   2. Add a \
         setCol helper function               #\"))))(Secondary((id \
         f13cbac1-0361-4a17-8f42-73ca11ce3e83)(content(Whitespace\"\\n\"))))(Secondary((id \
         2982b4bf-53b2-46db-a5d1-88951f79c135)(content(Comment\"#   3. Handle \
         PlantCol in the update function     #\"))))(Secondary((id \
         18a250c6-e6f6-4651-b4c3-c821e134e543)(content(Whitespace\"\\n\"))))(Secondary((id \
         70e925b0-f6a7-487d-bffa-5d160ca70e3e)(content(Comment\"#                                                 \
         #\"))))(Secondary((id \
         8659e7ac-f695-4493-b777-720f190fb5a3)(content(Whitespace\"\\n\"))))(Secondary((id \
         2aff5811-0122-42c5-bd1f-a5a5a3aa38e4)(content(Comment\"# Look at how \
         PlantRow is implemented for         #\"))))(Secondary((id \
         02f587ac-5d28-40ba-8dd5-ebbef5fe88d6)(content(Whitespace\"\\n\"))))(Secondary((id \
         d1562122-ed7f-4334-a38a-9c122f3eb912)(content(Comment\"# guidance - \
         PlantCol is similar but vertical.    #\"))))(Secondary((id \
         2b455a06-92a8-4c96-8088-9e7430e9c494)(content(Whitespace\"\\n\"))))(Secondary((id \
         3e96f405-ca5b-4af8-a04f-7214a2d128ff)(content(Comment\"#                                                 \
         #\"))))(Secondary((id \
         04b10ed2-9912-415e-bd2d-ba30a2d48b3a)(content(Whitespace\"\\n\"))))(Secondary((id \
         1b64c678-439f-477a-8765-3cebc5c4f672)(content(Comment\"# Tip: Use \
         auto-probe to see how the grove        #\"))))(Secondary((id \
         dbf550ff-fced-474e-8077-b26a216ea19b)(content(Whitespace\"\\n\"))))(Secondary((id \
         acfdeba7-dcdc-49ee-9c3c-4f216647fed2)(content(Comment\"# changes \
         after each action.                      #\"))))(Secondary((id \
         dd019668-385b-40e1-859a-d84ddf978765)(content(Whitespace\"\\n\"))))(Secondary((id \
         1a40c8bd-af99-4f85-af93-b1deb5fa2a9f)(content(Whitespace\"\\n\"))))(Tile((id \
         4ded2269-bace-4384-88f9-26895dd90384)(label(type = in))(mold((out \
         Exp)(in_(TPat Typ))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         ddb79e04-a57b-4e49-a74a-dc7057fb1302)(content(Whitespace\" \
         \"))))(Tile((id \
         186637a6-75a0-4d02-9f36-4c30a2cee3e2)(label(Plant))(mold((out \
         TPat)(in_())(nibs(((shape Convex)(sort TPat))((shape Convex)(sort \
         TPat))))))(shards(0))(children())))(Secondary((id \
         92182ba7-5c7f-4017-9b54-702ec19b0372)(content(Whitespace\" \
         \")))))((Secondary((id \
         eb3bfb05-c012-45ac-9a9b-a2cc79784890)(content(Whitespace\" \
         \"))))(Tile((id \
         3c4d81df-33b0-4b56-b1a3-53016ba11f1b)(label(String))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         1d2ae039-ae43-4c58-9a40-84dfddc5a620)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         6ae1ae4c-2c25-4a6e-b0eb-6145f9948768)(content(Whitespace\"\\n\"))))(Tile((id \
         0129ee23-06f5-4872-ba44-cf7cd0cb4338)(label(type = in))(mold((out \
         Exp)(in_(TPat Typ))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         164f1087-c888-40b8-81c7-3ab651a13bd7)(content(Whitespace\" \
         \"))))(Tile((id \
         c359dd0b-fc09-46d2-856b-8e710af24bb6)(label(Grove))(mold((out \
         TPat)(in_())(nibs(((shape Convex)(sort TPat))((shape Convex)(sort \
         TPat))))))(shards(0))(children())))(Secondary((id \
         88342e30-5114-4496-aa7f-396ce8141fc4)(content(Whitespace\" \
         \")))))((Secondary((id \
         b6b823af-a6f1-4fdc-9e37-0b29bd05c693)(content(Whitespace\" \
         \"))))(Tile((id 9b5da971-6dd7-4f4e-bc31-25dff4de7557)(label([ \
         ]))(mold((out Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape \
         Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
         1d45f9a8-6baa-468e-8c89-b59afac52c4d)(label([ ]))(mold((out \
         Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0 1))(children(((Tile((id \
         7b62a96e-925e-491b-8344-677140f82dd6)(label(Plant))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))))))))))))(Secondary((id \
         77731b41-fd05-4a34-8c78-6a2f1014016b)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         75eb29d3-7ac7-4b1a-b402-5ebebd693f8a)(content(Whitespace\"\\n\"))))(Tile((id \
         9895f3e0-04c9-432f-82ae-92ea010ffbac)(label(type = in))(mold((out \
         Exp)(in_(TPat Typ))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         2007cf14-ce60-44a7-9c7d-181a7687e558)(content(Whitespace\" \
         \"))))(Tile((id \
         aa93dca0-6861-4148-afa1-4c8e6a49d004)(label(Row))(mold((out \
         TPat)(in_())(nibs(((shape Convex)(sort TPat))((shape Convex)(sort \
         TPat))))))(shards(0))(children())))(Secondary((id \
         415d7ca7-587a-46e0-8195-0e9d2f0d0814)(content(Whitespace\" \
         \")))))((Secondary((id \
         257f7ee8-d60d-40ce-9e95-e2391d9fee59)(content(Whitespace\" \
         \"))))(Tile((id \
         35bd5e09-691d-4931-a267-987f55ad4b70)(label(Int))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         d8085a05-a6bf-4843-b9fe-2cf3309a909a)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         1aff0bf9-9293-48b0-9562-dec5e112405a)(content(Whitespace\"\\n\"))))(Tile((id \
         2f03a5a2-8019-44ef-88b7-db76ecb0008a)(label(type = in))(mold((out \
         Exp)(in_(TPat Typ))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         f23b2b95-9543-4371-a8dd-8afe73eca5b5)(content(Whitespace\" \
         \"))))(Tile((id \
         78b5fe02-c7eb-47fa-983f-b2dbede012a2)(label(Col))(mold((out \
         TPat)(in_())(nibs(((shape Convex)(sort TPat))((shape Convex)(sort \
         TPat))))))(shards(0))(children())))(Secondary((id \
         8c1948ee-3003-4f9b-b8ee-35fda9930ce2)(content(Whitespace\" \
         \")))))((Secondary((id \
         49982734-ab0d-460f-9a44-e7c9afdd05db)(content(Whitespace\" \
         \"))))(Tile((id \
         c8b07e08-2fd4-4b6b-b79c-9b8d56bbcdfc)(label(Int))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         0f14f9e3-12cf-41f6-a6f4-261e0332a521)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         344d2021-2005-4afc-b760-43835ecd4b36)(content(Whitespace\"\\n\"))))(Secondary((id \
         3bfbab1a-6982-43f8-8889-1bef21d54b49)(content(Whitespace\"\\n\"))))(Tile((id \
         625ffa68-52bf-47dc-88d6-6c0ce9b0fed3)(label(type = in))(mold((out \
         Exp)(in_(TPat Typ))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         4f046786-e342-4f69-b084-56411c8d4415)(content(Whitespace\" \
         \"))))(Tile((id \
         c5f07edc-f05e-49fc-9c1b-1ed9b95b6558)(label(Model))(mold((out \
         TPat)(in_())(nibs(((shape Convex)(sort TPat))((shape Convex)(sort \
         TPat))))))(shards(0))(children())))(Secondary((id \
         edf3dab7-a10c-4578-a70d-52d1def8a6ba)(content(Whitespace\" \
         \")))))((Secondary((id \
         22c45931-f00f-42b2-9620-0c0fda09c3df)(content(Whitespace\" \
         \"))))(Tile((id \
         f997b838-36fc-4b55-8e2c-7e896bd68dad)(label(\"(\"\")\"))(mold((out \
         Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0 1))(children(((Secondary((id \
         aef67bb6-a218-47cf-9bfb-8c8f55aa6c65)(content(Whitespace\"\\n\"))))(Tile((id \
         9f69b8fa-0066-4a09-ad88-be0533fbc2b8)(label(grove))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         cf06730c-eb53-4166-ab8e-a088ecc66a6f)(content(Whitespace\" \
         \"))))(Tile((id \
         fc71b5a4-38b3-4ddc-b7a2-f6459669cd23)(label(=))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 39))(sort Typ))((shape(Concave \
         39))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         ab78b69d-347c-4dd4-9353-e1ceeaf87f42)(content(Whitespace\" \
         \"))))(Tile((id \
         df605077-038a-41cb-8028-e6973d698259)(label(Grove))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         e5326b32-5178-424d-824d-10307dcc2922)(label(,))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 44))(sort Typ))((shape(Concave \
         44))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         f03d1e3e-2fcf-4b38-9692-efb34570f406)(content(Whitespace\"\\n\"))))(Tile((id \
         432d210d-0252-4bf4-a302-a16d832f5af1)(label(currentSeed))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         38e642c9-c1af-408d-8b82-933b64594645)(content(Whitespace\" \
         \"))))(Tile((id \
         c543dd2c-acd9-42bf-a36c-a341310c1dd3)(label(=))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 39))(sort Typ))((shape(Concave \
         39))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         de60302e-1861-4056-84eb-9b788b72a68b)(content(Whitespace\" \
         \"))))(Tile((id \
         5e398e8a-f540-4eba-89ac-fa83252109e9)(label(Plant))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         95d8efbe-1ec0-4279-a2a0-a5a647e3ea5b)(label(,))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 44))(sort Typ))((shape(Concave \
         44))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         3571eda3-4437-4124-ae3e-33fc585bec7d)(content(Whitespace\"\\n\"))))(Tile((id \
         cc7e9b91-16a9-430e-8958-1579b114b97b)(label(seedInventory))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         b4698f08-8c29-4389-82cd-f3f117987824)(content(Whitespace\" \
         \"))))(Tile((id \
         79a2f525-4102-47fc-9010-7956a783d98d)(label(=))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 39))(sort Typ))((shape(Concave \
         39))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         566c7e98-df9d-41d0-b7a4-82a733d3a49e)(content(Whitespace\" \
         \"))))(Tile((id b58ed131-666f-4953-9a2b-5df80ca588e2)(label([ \
         ]))(mold((out Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape \
         Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
         b063a8d6-8ec4-43c3-9a83-d4434108398e)(label(Plant))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children()))))))))(Secondary((id \
         814cd88d-4228-4a6a-a661-a41361b0ce12)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         30988e30-9b09-4cfa-ad56-90df0f0ce07e)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         516304bc-7bfb-4a9a-9579-af49ea020ff9)(content(Whitespace\"\\n\"))))(Secondary((id \
         94cb49bd-505e-438a-ba54-7d926957107e)(content(Whitespace\"\\n\"))))(Tile((id \
         8829e00e-4fe9-4158-bc72-f9ae0844e32f)(label(type = in))(mold((out \
         Exp)(in_(TPat Typ))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         d225e960-61c1-4157-9467-e85560045547)(content(Whitespace\" \
         \"))))(Tile((id \
         2bbd0177-d998-40a9-b69f-9bd3fdf9dbb5)(label(Action))(mold((out \
         TPat)(in_())(nibs(((shape Convex)(sort TPat))((shape Convex)(sort \
         TPat))))))(shards(0))(children())))(Secondary((id \
         cc9f9760-fd6f-4506-9588-d92b56a667f3)(content(Whitespace\" \
         \")))))((Secondary((id \
         b8a418a5-f39a-4a76-920f-162559e6ad08)(content(Whitespace\"\\n\"))))(Tile((id \
         c45c2758-b49b-4789-af11-c02dcc95d986)(label(+))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape(Concave 33))(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         75d91de8-338e-4e9e-bc90-ba0d5dd960c7)(content(Whitespace\" \
         \"))))(Tile((id \
         dd706337-339c-4ab0-a1e4-f49a6393220b)(label(SelectSeed))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         ccbfc9d6-bf1c-4b96-a6bb-d566c6caddbc)(label(\"(\"\")\"))(mold((out \
         Typ)(in_(Typ))(nibs(((shape(Concave 11))(sort Typ))((shape \
         Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
         d76b11f2-3832-4778-b128-d3edc2732268)(label(Int))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children()))))))))(Secondary((id \
         ce349ee5-9753-4a14-b9e6-317e5679cefc)(content(Whitespace\"\\n\"))))(Tile((id \
         8de1e1a6-dd01-4200-adb9-d0680ffe90c1)(label(+))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 12))(sort Typ))((shape(Concave \
         12))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         7e6c5e86-0670-45d5-adf1-d4fa9c2e6405)(content(Whitespace\" \
         \"))))(Tile((id \
         457f5b9a-af41-46ca-8aa5-9dab1bcb3e06)(label(PlantSeed))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         7df71db9-a5e1-4dc2-aca8-ee3b923a4552)(label(\"(\"\")\"))(mold((out \
         Typ)(in_(Typ))(nibs(((shape(Concave 11))(sort Typ))((shape \
         Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
         34f3ada1-2ab8-4b5a-8592-eb7ce1c42d40)(label(Row))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         26bc96df-9fbe-45ba-a4f4-13032ea0d6b5)(label(,))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 44))(sort Typ))((shape(Concave \
         44))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         a6f63fb6-f1ff-447f-b85e-a9ed99c7d636)(content(Whitespace\" \
         \"))))(Tile((id \
         b24f3dda-ef08-438f-ac89-21d0863716f0)(label(Col))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children()))))))))(Secondary((id \
         a010262f-221a-46f7-9367-d84ebee34304)(content(Whitespace\"\\n\"))))(Tile((id \
         e317a914-c9b4-4bde-9e0a-00692298bc81)(label(+))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 12))(sort Typ))((shape(Concave \
         12))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         51f50192-0056-445e-b2a6-2d9487a88223)(content(Whitespace\" \
         \"))))(Tile((id \
         c45bb141-6ef2-41ca-ae1b-8328c22237ef)(label(Uproot))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         cfe62473-1bbb-4405-a461-deb734056c2d)(label(\"(\"\")\"))(mold((out \
         Typ)(in_(Typ))(nibs(((shape(Concave 11))(sort Typ))((shape \
         Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
         60df30e9-232f-47cc-8f5f-555f7b669920)(label(Row))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         6e320561-cca4-4107-b16d-50c38ed44f50)(label(,))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 44))(sort Typ))((shape(Concave \
         44))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         5ad4b72d-d97e-4f25-884f-ccebd6397fd3)(content(Whitespace\" \
         \"))))(Tile((id \
         30f8be2d-6ded-474b-a627-516f510fc1ef)(label(Col))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children()))))))))(Secondary((id \
         3cc4a9ea-7ae2-4618-9acd-334052a9a9ff)(content(Whitespace\"\\n\"))))(Tile((id \
         9a7a165f-0c60-4d2b-9668-39354f37c504)(label(+))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 12))(sort Typ))((shape(Concave \
         12))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         07755ae5-c3ad-488d-bb52-d8c3537cd86f)(content(Whitespace\" \
         \"))))(Tile((id \
         9aab5795-2955-4452-8313-18880aaf9243)(label(ClearGrove))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         bf77b1ce-e8d9-4475-b2bd-a81c0e910f2d)(content(Whitespace\"\\n\"))))(Tile((id \
         ecbbd006-2e54-4d75-8f18-3ef8a3b728a6)(label(+))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 12))(sort Typ))((shape(Concave \
         12))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         b8d6f061-8596-44de-8d6d-617e1a9220a0)(content(Whitespace\" \
         \"))))(Tile((id \
         5dd80557-8872-4e01-88d5-6cf9d55b58da)(label(PlantRow))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         dec87a83-2feb-4c33-999b-fffa90568a21)(label(\"(\"\")\"))(mold((out \
         Typ)(in_(Typ))(nibs(((shape(Concave 11))(sort Typ))((shape \
         Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
         ad16f085-2856-423d-a667-ab07d09da25b)(label(Row))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children()))))))))(Secondary((id \
         b5aad199-e441-4e32-83a5-dff8e7146455)(content(Whitespace\"\\n\"))))(Secondary((id \
         f91db691-634a-416d-b7aa-49f8b7d82ed3)(content(Comment\"# TODO: Add \
         PlantCol(Col) here #\"))))(Secondary((id \
         aa32a0bd-2665-4c46-83bd-69c3fd27220a)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         bd833479-264a-4cc8-b285-ba9668fd8a6b)(content(Whitespace\"\\n\"))))(Secondary((id \
         fe956322-01c8-4021-9f71-aa9769bdd6e9)(content(Whitespace\"\\n\"))))(Tile((id \
         040ca46f-9291-44e8-9839-3bf0907cf0af)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         fe6b72eb-e587-496b-a3c2-4e88bc9ad190)(content(Whitespace\" \
         \"))))(Tile((id \
         62e9fa49-58a4-48f7-8833-7080a3b766a4)(label(init))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         5e9240cb-2485-4d7a-bb19-108a459a95b2)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 24))(sort Pat))((shape(Concave \
         24))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         78992abd-fe28-46ff-b5e0-a25cb5b2e352)(content(Whitespace\" \
         \"))))(Tile((id \
         cddfe7a1-8418-41f5-a175-3bc2ec069743)(label(Model))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         cc52c72d-5492-4ff9-a001-5cae72d48413)(content(Whitespace\" \
         \")))))((Secondary((id \
         bd1b3dc0-a92a-44fc-ae5a-5a63c1a3cb17)(content(Whitespace\" \
         \"))))(Tile((id \
         ba15e00a-344d-4ebb-a2fa-e89892d50e38)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         0d7036a5-ae56-4c41-b5c0-e9d4d7b615e4)(content(Whitespace\"\\n\"))))(Tile((id \
         440a282e-41ef-4d52-8fe0-1a0bf3fc6842)(label(grove))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         a891ff06-ce22-47a0-b156-8e44fc5a1e95)(content(Whitespace\" \
         \"))))(Tile((id \
         f888dccc-5be1-49ec-a96c-7a332c874862)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         aa25b701-7c35-4450-b5b4-0a08428917e4)(content(Whitespace\" \
         \"))))(Tile((id ed703a26-8bd0-4cec-ba06-abd975c4276b)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         c359ae14-9f73-419d-92bb-bcb17a61f314)(content(Whitespace\"\\n\"))))(Tile((id \
         ec5bdbbe-02be-496f-a853-10dab1316a99)(label([ ]))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         8ac3c99f-ad25-405b-ab54-1a8f2591418e)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         15987f0f-8685-40fa-8c7f-b3c98e8308dc)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         721e257b-9974-42f4-b829-cccb809c9b37)(content(Whitespace\" \
         \"))))(Tile((id \
         81fa92ad-a2eb-4910-9b19-cd1b1e1189c6)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         f1a5d9de-7463-42b3-9d3a-897e7c5080b0)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         508c7b93-de46-4237-83c4-7e838e998f79)(content(Whitespace\" \
         \"))))(Tile((id \
         9065ca17-df7f-48a5-aea6-5721cddc6b7c)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         a65d52d0-abbe-4665-843b-c9058d9e0390)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         ad79ae29-575e-4ec3-9e68-dd21296d2c21)(content(Whitespace\"\\n\"))))(Tile((id \
         c67786ee-93b9-4fa2-a15f-e22c95ea36cf)(label([ ]))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         b478f8f9-cf3a-47e0-8a47-5b4656878602)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         71194d9e-944b-4fb7-a463-26d6f5d599d0)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         6a3b6864-a317-4d27-bc8a-91eb3b402e4b)(content(Whitespace\" \
         \"))))(Tile((id \
         ad97dfbc-f7ef-4419-9ed4-5f64a83c3276)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         8d79f25d-3a74-46cc-9db0-9a6c854ecca2)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         98407153-2b64-4ef5-ac85-90c626e6ebd1)(content(Whitespace\" \
         \"))))(Tile((id \
         0a30852c-53a2-4dd6-820f-c7d1b1e300df)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         3739a0d4-8ca2-4f9b-bcab-d4a25cdf0612)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         872d82a2-d3d3-46bc-983a-1602dca95140)(content(Whitespace\"\\n\"))))(Tile((id \
         d9a4863c-b91c-495f-9b24-f79fc5b0a637)(label([ ]))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         57ffcac8-0277-4485-9cd5-163e344b62c9)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         0d03d177-860d-4544-82c3-72e2a870d140)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         f30a8b97-ad78-4907-a2f7-a41ad5a2d81a)(content(Whitespace\" \
         \"))))(Tile((id \
         71b1726e-5af7-442b-96eb-d97204b061c1)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         af0e58dc-fbab-41fe-986a-7f31767cc5bb)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         4dc94574-df59-4a29-818c-e7b7f857f3e8)(content(Whitespace\" \
         \"))))(Tile((id \
         4630de79-8681-4706-a51c-d89e90559409)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         ba874f04-530d-48b9-8e83-81b25d95437a)(content(Whitespace\"\\n\")))))))))(Tile((id \
         741cce89-ee28-4a54-bf3d-4e9f3f7830d4)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         0493c56e-528d-4a7e-aa3a-f33c78eddd77)(content(Whitespace\"\\n\"))))(Tile((id \
         788e90b3-878f-4b7e-816a-e4ea3bd446b2)(label(currentSeed))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         9185eb21-5a8e-4be1-b7b9-07c121e8ff1b)(content(Whitespace\" \
         \"))))(Tile((id \
         ffe13730-0f75-40d1-aed3-89fb28d7c426)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         eb577ddc-ad78-4052-855e-fb20e76739d9)(content(Whitespace\" \
         \"))))(Tile((id \
         9ac0653e-4ea0-49a5-97c7-93a54b59e43a)(label(\"\\\"\\240\\159\\140\\177\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         3a6febf3-6253-465d-ac89-ad93db1c3cb3)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         199a7ae5-3da6-4b1a-83a9-7482cb7cd226)(content(Whitespace\"\\n\"))))(Tile((id \
         94a28836-e89c-455b-be0c-e38d6cb7b4d6)(label(seedInventory))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         87cbf32e-ca26-4422-9c7d-1a60edd04bbe)(content(Whitespace\" \
         \"))))(Tile((id \
         0c7d059b-894a-41ae-a6c0-7df138d17145)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         97824e2e-bc93-440e-96bb-42a4fd7bd4f4)(content(Whitespace\" \
         \"))))(Tile((id 6f0ca6ee-3bc4-46d9-a12a-3d8ab7942093)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         db9375e2-1d9a-41e9-aab1-6a9af0d29713)(label(\"\\\"\\240\\159\\140\\177\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         2a844c3c-8b02-47e4-92eb-f5322cea0d81)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         60f43669-479b-4fb2-8e74-c07e26b16e6c)(content(Whitespace\" \
         \"))))(Tile((id \
         1f34c957-f977-4f96-94f0-1ef8279b816e)(label(\"\\\"\\240\\159\\140\\191\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         5a54382d-5391-4960-ba0c-9f99fb9e5082)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         ff1d2e20-9acc-4d19-943b-ef70deea85c2)(content(Whitespace\" \
         \"))))(Tile((id \
         83de66da-bb5d-4367-a6a1-59349ad87fac)(label(\"\\\"\\240\\159\\141\\132\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         6450eb23-1138-49fd-b3e5-f4502a04c9b0)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         0f116916-c60d-40bc-b84f-e1587b8b7f51)(content(Whitespace\" \
         \"))))(Tile((id \
         5c923d1e-7c63-4fa4-8fab-139defa7f9cd)(label(\"\\\"\\226\\152\\152\\239\\184\\143\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         8fef782d-798b-4d62-901a-147441c2d66b)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         5b6ac7b3-4f5c-44a7-b7fd-0c65e3737f75)(content(Whitespace\" \
         \"))))(Tile((id \
         a14df788-95aa-4ddb-ae23-7e3786098064)(label(\"\\\"\\240\\159\\140\\184\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         647e1ec5-71c4-4d97-90da-6cf427812c3f)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         206a98a3-dbac-47ea-94ba-467343d6ca62)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         f981dd2f-a3d5-4c16-86bc-75aecf9cc6f4)(content(Whitespace\"\\n\"))))(Secondary((id \
         090a3dff-ea54-48fe-b5ee-33f5a05f72af)(content(Whitespace\"\\n\"))))(Tile((id \
         ba7e66da-cefd-4533-a540-71fa84d8bf9c)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         baf48002-1ccb-41ee-8d70-e7451cbd67b9)(content(Whitespace\" \
         \"))))(Tile((id \
         248cfbc9-3d6c-4fd7-bd79-d3673c72efa1)(label(setCell))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         44e358c1-698a-4fee-bc6b-9311137a1ab8)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 24))(sort Pat))((shape(Concave \
         24))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         7dd69821-5eb7-4489-a5de-3ff9008096be)(content(Whitespace\" \
         \"))))(Tile((id \
         f53910ed-5948-449b-b051-de0a694be7a7)(label(\"(\"\")\"))(mold((out \
         Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0 1))(children(((Tile((id \
         34e22a58-3b93-41bc-b8e9-f7feb640d1d4)(label(Grove))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         a120491b-1939-4d43-811a-6f761dffd034)(label(,))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 44))(sort Typ))((shape(Concave \
         44))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         f817ab63-70a7-4d94-87dd-b50ee42cb692)(content(Whitespace\" \
         \"))))(Tile((id \
         3011e101-155c-444e-9768-d2031f6846ff)(label(Row))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         cf4ae101-3aee-4b09-87d2-2e95440d9a36)(label(,))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 44))(sort Typ))((shape(Concave \
         44))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         24cefa28-3b23-4e3f-bb3f-76e0cf82cf89)(content(Whitespace\" \
         \"))))(Tile((id \
         db3ea090-9599-4adc-850c-e1bde1b8f550)(label(Col))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         29cc3158-aab5-48a4-b8d1-1dc4556ca038)(label(,))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 44))(sort Typ))((shape(Concave \
         44))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         e8e2ccdc-68af-4e0a-b499-fab749168892)(content(Whitespace\" \
         \"))))(Tile((id \
         fe446f8b-eadc-43f2-a967-00a4e1c33307)(label(Plant))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children()))))))))(Secondary((id \
         a9baf539-e901-432b-9499-3091f9e4996e)(content(Whitespace\" \
         \"))))(Tile((id \
         3cb77f99-631e-4995-8591-494278713dc9)(label(->))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 13))(sort Typ))((shape(Concave \
         13))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         5ef6c40b-32ee-49c8-9b30-05586d275b1e)(content(Whitespace\" \
         \"))))(Tile((id \
         a7315e4b-778f-4f67-b5fc-90cbca9f48ef)(label(Grove))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         d9376229-0b6a-44f9-abef-217b5bfd0a73)(content(Whitespace\" \
         \")))))((Secondary((id \
         c07437b6-bf6f-48d0-9a85-85ceae102414)(content(Whitespace\"\\n\"))))(Tile((id \
         4c2e0969-3d5f-434c-b346-3589fe879e99)(label(fun ->))(mold((out \
         Exp)(in_(Pat))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         37))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         2b31199a-3689-4103-89dd-e200f38a54a5)(content(Whitespace\" \
         \"))))(Tile((id \
         e1f1a3e1-57d3-45fa-8c76-60c624e4d95e)(label(grove))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         7f6023f9-ee28-40b4-935c-d016432426ae)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 44))(sort Pat))((shape(Concave \
         44))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         8ea2fed3-49c3-4e8a-b897-0ae7a76925cf)(content(Whitespace\" \
         \"))))(Tile((id \
         8700b078-6041-4aee-acdd-2911b10f61ae)(label(row))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         c0c5c783-92cd-42d2-a243-349ffae9ea08)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 44))(sort Pat))((shape(Concave \
         44))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         70d63bb6-f39f-4bfe-8e23-49d28a816841)(content(Whitespace\" \
         \"))))(Tile((id \
         5e71bbb7-f3c2-4843-9006-8fda3e4a6bf0)(label(col))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         ae24842a-c133-4662-8415-e4907feeb2aa)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 44))(sort Pat))((shape(Concave \
         44))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         3b7fb7ba-dbb7-4bea-b860-bd855ef27d5b)(content(Whitespace\" \
         \"))))(Tile((id \
         89a5bd99-3517-43de-96a2-e0c653f2baf7)(label(plant))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         7259917c-6c5b-48b8-b1df-e61f30b64ae1)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         bacd7d47-8f04-4893-9f14-514a8396bc0d)(content(Whitespace\"\\n\"))))(Tile((id \
         722d8b4a-c7e2-4a9b-96f3-22b7f2cfda40)(label(mapi))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         2787a45a-8cde-49e3-b2f6-377c48160a23)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         ad867f79-f432-47c8-a9b9-826633f56387)(label(grove))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         61253410-5ddf-4e53-9296-200072525649)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         c38a08fe-4d75-423e-bb90-de9bd4699be8)(content(Whitespace\" \
         \"))))(Tile((id 180b28c6-20a3-4a50-84e1-e56eb5746f67)(label(fun \
         ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 37))(sort Exp))))))(shards(0 \
         1))(children(((Secondary((id \
         9a7baeec-ddea-4d97-86d4-a9cb81f2ed23)(content(Whitespace\" \
         \"))))(Tile((id \
         cac51abf-23ad-436c-a511-6a447aff2afd)(label(\"(\"\")\"))(mold((out \
         Pat)(in_(Pat))(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0 1))(children(((Tile((id \
         5103ecdd-e737-4615-b844-840e722ba33b)(label(i))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         d7ec2b1f-acbc-4495-8e36-2abddab715ab)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 44))(sort Pat))((shape(Concave \
         44))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         f5951ee6-7c06-40fd-9229-b68c5b773075)(content(Whitespace\" \
         \"))))(Tile((id \
         2243cb9c-e794-4da9-a74e-19fd979f6166)(label(r))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children()))))))))(Secondary((id \
         d5b5102e-960f-4ad6-bfef-e15af7794892)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         480c9c79-ce7f-4c09-ad57-d30d33181150)(content(Whitespace\"\\n\"))))(Tile((id \
         c64c76e9-8846-425e-b2dc-bb220bc3060d)(label(if then else))(mold((out \
         Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         36))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         6f0a5618-c840-40c3-8af2-6d6d8c9feee8)(content(Whitespace\" \
         \"))))(Tile((id \
         4451ffd4-9b76-44f8-98e2-1d94f7f6f714)(label(i))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         6ef195c5-93f2-4c61-bb1c-409db6ae8e00)(content(Whitespace\" \
         \"))))(Tile((id \
         ffcf210a-01a8-4903-860a-65afcb764003)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         061abcc5-05b5-4354-8c66-cbe8de4186ba)(content(Whitespace\" \
         \"))))(Tile((id \
         5f342fae-6d51-4f6b-b874-5be1d5e9810e)(label(row))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         d01a8edc-0a19-4477-8cd6-9a6d3ce44734)(content(Whitespace\"\\n\")))))((Secondary((id \
         3d8cbff4-d8d1-412d-a4ab-4d7c0161197a)(content(Whitespace\" \
         \"))))(Tile((id \
         e81ed1ed-892e-4cd8-9f4c-755bd7fc4a0e)(label(mapi))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         6f40011c-2b8a-438e-841d-dcb4fc22c27c)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         2f8fbcee-3cdc-44ac-94b4-1d0115731960)(label(r))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         4b326f1b-273f-453e-9711-25ca929116d2)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         d76d588f-7774-465b-974d-fa6f90af064b)(content(Whitespace\" \
         \"))))(Tile((id 6ec94623-4c92-4067-a060-c0b481e11ff0)(label(fun \
         ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 37))(sort Exp))))))(shards(0 \
         1))(children(((Secondary((id \
         05b4a26b-39fd-4b9f-bf91-2707510c5f2a)(content(Whitespace\" \
         \"))))(Tile((id \
         c6a45b89-1580-4208-831d-cccbd5dda386)(label(\"(\"\")\"))(mold((out \
         Pat)(in_(Pat))(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0 1))(children(((Tile((id \
         4ab77419-9c4a-4ffb-9b10-8512f092b272)(label(j))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         1caaa7ad-d43c-451b-93a0-a971bd7d3290)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 44))(sort Pat))((shape(Concave \
         44))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         a1e9ab6b-bddd-4229-a66c-67a94b82bef7)(content(Whitespace\" \
         \"))))(Tile((id \
         9a07b240-7b37-4719-a6d5-619338a954ba)(label(c))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children()))))))))(Secondary((id \
         a0776dc5-c16b-425a-95f2-9c80c697269c)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         d276ad97-6659-46e0-bcc3-293085c67ada)(content(Whitespace\" \
         \"))))(Tile((id ec1e273a-2e03-45ea-af16-ccf62a7a57c6)(label(if then \
         else))(mold((out Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 36))(sort Exp))))))(shards(0 1 \
         2))(children(((Secondary((id \
         3f90d491-31ff-4f9c-8787-3667223399dc)(content(Whitespace\" \
         \"))))(Tile((id \
         7691303e-af3b-42de-89d3-af74183daa12)(label(j))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         c10afd4f-4ee8-4b07-a055-a39895961e67)(content(Whitespace\" \
         \"))))(Tile((id \
         16b0cad5-e4d3-4313-9f90-ed59d3aaadc0)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         3ad5307f-6ef8-43b8-848f-fbe6b9472822)(content(Whitespace\" \
         \"))))(Tile((id \
         33667b49-dbb7-4ae6-ad47-84080f203912)(label(col))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         19489e07-9348-411a-ae6b-b2daacc8e2ca)(content(Whitespace\" \
         \")))))((Secondary((id \
         6db633b9-e92b-43f4-8d33-851ad89ae550)(content(Whitespace\" \
         \"))))(Tile((id \
         f14ac856-2e5b-4b89-b3c7-0c1ae2289b59)(label(plant))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         a726767d-cc84-43d0-9a48-d169b7e88606)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         9551a56d-902b-4004-86da-5767ac8cb102)(content(Whitespace\" \
         \"))))(Tile((id \
         4b1b2d3b-4f5d-4f34-9eb4-5c3e79428078)(label(c))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         63cf2e4f-c23c-4c64-8792-254fab25d92c)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         ca516ea1-f5f1-49f9-a5f7-9d9504e11fbc)(content(Whitespace\" \
         \"))))(Tile((id \
         25d5a59e-c24a-4093-8fb3-892e55a67b69)(label(r))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         e0a3801e-1500-4e38-a892-563fc737daf6)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         8d5f354c-f009-4640-a719-30d365275327)(content(Whitespace\"\\n\"))))(Secondary((id \
         1b5b1fe9-d5cd-4371-8f81-e9addadb434b)(content(Whitespace\"\\n\"))))(Tile((id \
         b17ed6ab-1d43-4929-bfac-946ac1ed9483)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         989aeb02-d53b-46e7-b62f-a4dad9cfce10)(content(Whitespace\" \
         \"))))(Tile((id \
         aec53cf4-1766-431d-ab88-17635781bc31)(label(setRow))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         21e8d417-15d7-451d-a062-33003b0d50d2)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 24))(sort Pat))((shape(Concave \
         24))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         15e30d1d-4bd8-4869-81dc-20cb1940f94f)(content(Whitespace\" \
         \"))))(Tile((id \
         faa111ee-5852-42ff-b536-2b4de677ec16)(label(\"(\"\")\"))(mold((out \
         Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0 1))(children(((Tile((id \
         ce5c69eb-5b16-4831-9b83-294b0cfa2ca9)(label(Grove))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         ceed7004-cbd8-4791-a53d-e94f9c82f3ff)(label(,))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 44))(sort Typ))((shape(Concave \
         44))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         0074781b-7cca-40e3-8ef4-1c37121b5758)(content(Whitespace\" \
         \"))))(Tile((id \
         54bf03b6-5d90-4d29-a9e0-b48ea6218d6b)(label(Row))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         0f0c7a3c-9d1b-4e1c-bdb3-1af9156c4eff)(label(,))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 44))(sort Typ))((shape(Concave \
         44))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         dc4dd884-f2f1-4d8f-8ce0-ff06ee8cc60d)(content(Whitespace\" \
         \"))))(Tile((id \
         d6f99d39-ad63-4082-8d36-fff98536d341)(label(Plant))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children()))))))))(Secondary((id \
         8abb8963-9ec2-4261-8a90-a27635ccdbfa)(content(Whitespace\" \
         \"))))(Tile((id \
         a8514646-efd3-4e6d-b466-552c4c1ec6c2)(label(->))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 13))(sort Typ))((shape(Concave \
         13))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         260be25a-f188-4f8a-87c5-03c9d955edaf)(content(Whitespace\" \
         \"))))(Tile((id \
         43a83b51-d240-4109-af94-e73bccee346c)(label(Grove))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         d7ef622a-8e80-4a54-8b0b-7f74891f3655)(content(Whitespace\" \
         \")))))((Secondary((id \
         c90112eb-783b-4ad7-ae93-67fa06fd8cbb)(content(Whitespace\"\\n\"))))(Tile((id \
         c55c5dd9-34fe-4d2c-83c3-23a9bfba4956)(label(fun ->))(mold((out \
         Exp)(in_(Pat))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         37))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         76b05081-99dd-4212-b6a1-78ac2c86f599)(content(Whitespace\" \
         \"))))(Tile((id \
         3330413e-f7bf-4c66-85d1-ff1f86f9d782)(label(grove))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         b3b97c81-81a2-484e-a808-6453a3413db8)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 44))(sort Pat))((shape(Concave \
         44))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         00654526-f98e-4b17-9b83-1a3a3f52ee5a)(content(Whitespace\" \
         \"))))(Tile((id \
         8649923d-5738-4cbb-bf04-1bba8c5b3ca4)(label(targetRow))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         0544204b-2c55-44eb-b49d-b43c191ee37d)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 44))(sort Pat))((shape(Concave \
         44))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         9ee7ccbc-49eb-467b-9f4a-e61392c50375)(content(Whitespace\" \
         \"))))(Tile((id \
         45189cd5-7fa7-44f7-a248-22787cc187f6)(label(plant))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         7407b551-054b-4425-aec1-6002600e3ab0)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         a353359a-e70e-45af-a29e-aed13d9bf322)(content(Whitespace\"\\n\"))))(Tile((id \
         2799a6e6-45e0-48fd-bc4c-0cad92d08094)(label(mapi))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         4325fd2f-09a1-4d9a-acb7-5adb8e04b11a)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         2fbf8261-a602-4ac4-a786-d580fe50bdc8)(label(grove))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         ea3efc9c-0635-4529-a5bc-5eb9406845ad)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         cbe00b4e-41b1-4854-bbb6-43eaecf0e8ef)(content(Whitespace\" \
         \"))))(Tile((id b57b77ee-2b69-418a-b323-10354fbdf806)(label(fun \
         ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 37))(sort Exp))))))(shards(0 \
         1))(children(((Secondary((id \
         e6f4d47a-cda3-4a4f-880d-44fae95845be)(content(Whitespace\" \
         \"))))(Tile((id \
         53fde4f3-dec7-411d-9f8b-6026384fe9c6)(label(\"(\"\")\"))(mold((out \
         Pat)(in_(Pat))(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0 1))(children(((Tile((id \
         64d3e144-b265-4b5f-aa86-e1d86f8aa5c9)(label(i))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         6afd3339-14ed-4261-a58d-f773ab96816a)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 44))(sort Pat))((shape(Concave \
         44))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         ca6051d3-6f54-4f0f-aff0-6da4ac9e9495)(content(Whitespace\" \
         \"))))(Tile((id \
         85ff5c3f-4005-43d2-84e4-b405bf5d6d87)(label(row))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children()))))))))(Secondary((id \
         c772dba4-590c-45a7-a6d5-d9a2a1bc0313)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         97e7799d-ee76-41e8-be58-dbee6f51fba4)(content(Whitespace\"\\n\"))))(Tile((id \
         09bb2df1-98db-41df-a5c3-c57ea5518e8e)(label(if then else))(mold((out \
         Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         36))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         a843f4c3-ed01-48e3-8468-8bcecbd831b9)(content(Whitespace\" \
         \"))))(Tile((id \
         ce40fd77-c680-4b66-ad4e-e85313d4f18e)(label(i))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         17ce518d-1ae6-47db-ad51-b611cb0c6ee8)(content(Whitespace\" \
         \"))))(Tile((id \
         9e199000-f146-4150-9945-c3fb8bd34f02)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         27f35e0d-068e-4f82-9baa-367b54465c48)(content(Whitespace\" \
         \"))))(Tile((id \
         97753226-2a3d-4170-9cac-75834d4a7b1f)(label(targetRow))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         07da6483-8212-47e9-ae6e-c07e715d8990)(content(Whitespace\"\\n\")))))((Secondary((id \
         1a64fd73-13d9-4a2b-b46e-9f06fdc2c5f0)(content(Whitespace\" \
         \"))))(Tile((id \
         ffabf612-e995-48b9-946e-df2ecb8abffe)(label(map))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         00ea3001-c62f-4684-b22c-57037b531f63)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         8e69640e-55f2-45a9-bbd4-588cd78ab1b5)(label(row))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         8a4eab81-fa6a-47e3-be0d-9384484ba2ac)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         0daa520a-ea45-4464-9150-71566ed488f5)(content(Whitespace\" \
         \"))))(Tile((id b40c627a-2270-4ad8-a5e7-bdd44f6b5fe7)(label(fun \
         ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 37))(sort Exp))))))(shards(0 \
         1))(children(((Secondary((id \
         8c61bb7a-0c62-4f56-a1de-ab2d7bfe5b16)(content(Whitespace\" \
         \"))))(Tile((id \
         10c9bdfd-2c90-4756-95f8-1ab56fa69204)(label(_))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         02c04ebf-cbab-4842-bc31-74a13cf471c6)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         750bd68f-24f1-445e-b2f5-240a07691dcf)(content(Whitespace\" \
         \"))))(Tile((id \
         4baaa94d-21d4-4475-9d37-c142c514ec68)(label(plant))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         f7e576d0-e28a-425c-ade1-cb317683ed23)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         7fbb8a1e-5f0e-4ca5-8e0f-9e5531840fcb)(content(Whitespace\" \
         \"))))(Tile((id \
         ec65a436-4ac7-4733-904d-bf67de80519a)(label(row))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         7eca74f8-3540-45ae-8dd5-f8c61ff5c585)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         cafbd233-3d85-45f3-a4ce-935950e27d68)(content(Whitespace\"\\n\"))))(Secondary((id \
         657ebd6d-ac0f-4b96-b49a-8da87eb2b5d6)(content(Whitespace\"\\n\"))))(Secondary((id \
         1b46892b-a157-4006-ae89-859c5e10d046)(content(Comment\"# TODO: Add \
         setCol helper here #\"))))(Secondary((id \
         1fd49cec-3951-43f8-a4a7-3bcdfb08677a)(content(Whitespace\"\\n\"))))(Secondary((id \
         544f4968-1da4-4c6f-a5c2-66598041ad98)(content(Comment\"# Hint: You \
         need to modify each row, changing #\"))))(Secondary((id \
         c2352e90-55d3-4cb5-baed-3fd20794aeae)(content(Whitespace\"\\n\"))))(Secondary((id \
         2297a9d6-3d21-437b-af92-1592937dc774)(content(Comment\"# only the \
         cell at the target column.         #\"))))(Secondary((id \
         48919cd2-7504-4307-b2e7-65fc7fcbe269)(content(Whitespace\"\\n\"))))(Secondary((id \
         2b934c6e-31b1-4147-b863-37076f5b45c4)(content(Whitespace\"\\n\"))))(Tile((id \
         30f5d053-6bfd-4528-9d03-9df2e9752f95)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         16a0418c-41e1-4be3-b58e-3b1d67462cb2)(content(Whitespace\" \
         \"))))(Tile((id \
         c63667e0-618d-426d-8088-d8a5e9360b3b)(label(setAll))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         7a8b49c6-60b2-4c4f-874b-41afe5c2e5fe)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 24))(sort Pat))((shape(Concave \
         24))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         6d376ab1-5b22-4d96-8351-c05066965b9a)(content(Whitespace\" \
         \"))))(Tile((id \
         63699599-89e2-49cb-804a-c610470b982f)(label(\"(\"\")\"))(mold((out \
         Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0 1))(children(((Tile((id \
         966ba97b-6be4-448b-ad8f-476d8acbe6c9)(label(Grove))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         5e647977-d09a-4c85-8dc1-43f998608cf9)(label(,))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 44))(sort Typ))((shape(Concave \
         44))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         49c81f0e-8521-4a23-8dab-4ad2733daaed)(content(Whitespace\" \
         \"))))(Tile((id \
         101199dd-a3a5-490d-b12c-d4f46fe9a91c)(label(Plant))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children()))))))))(Secondary((id \
         1770e052-ed62-4b6f-82bf-2f8c67dd0c88)(content(Whitespace\" \
         \"))))(Tile((id \
         bb5053a2-fc47-45bd-b490-275da5fb5ee6)(label(->))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 13))(sort Typ))((shape(Concave \
         13))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         75adf1f6-f2d9-4a9f-a14c-9b7db8b25161)(content(Whitespace\" \
         \"))))(Tile((id \
         fa5b9ca8-dab5-4e33-8906-ec7a497d24f5)(label(Grove))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         6a62ab54-c8bd-4dd5-b535-5e57de48fd4b)(content(Whitespace\" \
         \")))))((Secondary((id \
         c9eb1997-151b-4997-ac88-66aa38c894fe)(content(Whitespace\"\\n\"))))(Tile((id \
         79fbad56-8271-470a-9c80-406db5162039)(label(fun ->))(mold((out \
         Exp)(in_(Pat))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         37))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         de082557-1960-41ba-9e18-deb8fb6eb313)(content(Whitespace\" \
         \"))))(Tile((id \
         aa3c5456-d877-405c-88cb-0b369ae26d71)(label(\"(\"\")\"))(mold((out \
         Pat)(in_(Pat))(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0 1))(children(((Tile((id \
         965bd4a3-5d8e-4ac3-96c3-f436aec1fe9a)(label(grove))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         8e81f7de-2911-4c6f-bf58-27b39796342e)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 44))(sort Pat))((shape(Concave \
         44))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         cc5f18c8-3a8c-4cbf-a3a1-b33a4d67fa57)(content(Whitespace\" \
         \"))))(Tile((id \
         27f7588c-8c0c-4eb3-a19e-f73667d0534a)(label(plant))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children()))))))))(Secondary((id \
         155989c6-7aa0-4f76-b456-22a62aeac2e9)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         c3eb0875-4c0b-48fb-b93c-374f0d65f3ce)(content(Whitespace\"\\n\"))))(Tile((id \
         50877dc9-4f99-4e75-b27a-4fcd85f6a3c1)(label(map))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         b7216c44-16d6-4791-8e70-b96180510a5b)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         95896e3f-b36c-4c4b-8e05-1aa5605a7006)(label(grove))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         7155f101-598e-4676-9a90-bb8bfa103644)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         31456a18-f297-4d8d-995a-7a7e98d2341d)(content(Whitespace\" \
         \"))))(Tile((id d27daa82-a307-4f1c-a6cd-d5cc63855040)(label(fun \
         ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 37))(sort Exp))))))(shards(0 \
         1))(children(((Secondary((id \
         6cb26707-3836-479b-9a66-110a99f41e63)(content(Whitespace\" \
         \"))))(Tile((id \
         23a6798a-3a81-42ca-8a14-ddfe8c20bc04)(label(row))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         895e45bd-dbc9-4e56-b0dc-1506ae78117f)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         dd3919a1-e04e-43dc-88d7-1f4b3769dff5)(content(Whitespace\" \
         \"))))(Tile((id \
         ad0e86f5-0c48-4ef5-b2f5-9de44ef5e5bf)(label(map))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         a7edd519-6e99-4e99-af2a-0d28b6ca8138)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         51853295-3673-42d6-8b58-c0dea6237ad6)(label(row))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         8b0b12fb-09e9-43dd-84b5-3855e4624d8f)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         13ce713c-83c5-4249-b50b-e127fca29465)(content(Whitespace\" \
         \"))))(Tile((id 393038f2-8018-4363-b50e-7f2000e3a614)(label(fun \
         ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 37))(sort Exp))))))(shards(0 \
         1))(children(((Secondary((id \
         65934668-4685-49c5-80a7-e98d89600164)(content(Whitespace\" \
         \"))))(Tile((id \
         d486bded-5bb5-4f84-9b62-6c5fde580533)(label(_))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         07d3b55a-1127-4129-a35d-9df6c4ea22e8)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         5bfccc0a-a4fe-4f26-a1d0-0c813c0ece52)(content(Whitespace\" \
         \"))))(Tile((id \
         db0c25e9-951c-4442-a3e3-59ea22c22029)(label(plant))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))))))))))))(Secondary((id \
         03ad4b98-7113-469a-a90e-fca381acf821)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         dd0d90b2-e878-47ae-89ca-21fb7e717a3a)(content(Whitespace\"\\n\"))))(Secondary((id \
         a600f417-30ad-4bb6-b0e3-4db7442a947a)(content(Whitespace\"\\n\"))))(Tile((id \
         9dfbaddc-8a85-4ccc-9ddb-70687697ae1f)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         d04255c7-accc-49c1-98c1-9040fba088ee)(content(Whitespace\" \
         \"))))(Tile((id \
         478c3774-0037-4264-87e5-b479d4d78442)(label(updateGrove))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         bfb7819a-add6-4d28-9d17-8da7ad538ccd)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 24))(sort Pat))((shape(Concave \
         24))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         1922ed5e-31ef-46a4-96de-60a0f2a10447)(content(Whitespace\" \
         \"))))(Tile((id \
         e491863a-89bd-4afb-add4-d47c3d0a228d)(label(\"(\"\")\"))(mold((out \
         Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0 1))(children(((Tile((id \
         091104a5-3d09-4afa-9d15-0d9fa07dcbcc)(label(Model))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         bf95e01c-d5dc-48c3-8e9d-97537d6ea0d1)(label(,))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 44))(sort Typ))((shape(Concave \
         44))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         73a305a2-5ecd-42f7-a33e-28158210c7df)(content(Whitespace\" \
         \"))))(Tile((id \
         acd2271d-8a87-41de-ac4c-26f54169d035)(label(Grove))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         2c13e471-2036-4e33-a2fe-ac2007698e05)(content(Whitespace\" \
         \"))))(Tile((id \
         653ee0cf-8010-4a8d-9162-53ee02c85041)(label(->))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 13))(sort Typ))((shape(Concave \
         13))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         f5124c14-0a7d-4409-8bba-ffc5742944f0)(content(Whitespace\" \
         \"))))(Tile((id \
         b3ce25f3-10e7-4c72-b76b-d445371c1ae4)(label(Grove))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children()))))))))(Secondary((id \
         cfb95d18-c86e-4204-abb2-0aeb299f3654)(content(Whitespace\" \
         \"))))(Tile((id \
         a1a138a5-4156-4dd9-b812-313852711ed9)(label(->))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 13))(sort Typ))((shape(Concave \
         13))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         5c878829-fc10-488f-8f3e-21535787dd14)(content(Whitespace\" \
         \"))))(Tile((id \
         3aef8610-22c8-4bf0-b763-257d03c4eb65)(label(Model))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         cecaa750-2907-4a24-bc56-86689dc33c52)(content(Whitespace\" \
         \")))))((Secondary((id \
         4598a768-ee8b-4374-94c5-6f6c3894ee28)(content(Whitespace\"\\n\"))))(Tile((id \
         61aaad75-f705-4be2-aed0-a91d2eabcca3)(label(fun ->))(mold((out \
         Exp)(in_(Pat))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         37))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         85b0cd28-3cd5-41f8-820f-3427fe05856e)(content(Whitespace\" \
         \"))))(Tile((id \
         8a0ed1a6-3a2b-4a6e-962f-913bc2e4854a)(label(\"(\"\")\"))(mold((out \
         Pat)(in_(Pat))(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0 1))(children(((Tile((id \
         f61d3f73-a226-465c-bc45-e70bd8e747c2)(label(m))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         7adcb970-8adc-4d6b-8dd5-0aa018c1d88f)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 44))(sort Pat))((shape(Concave \
         44))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         7e1615eb-9573-4a85-86e7-9d514e15dac5)(content(Whitespace\" \
         \"))))(Tile((id \
         7bef207e-1125-4e57-83f6-5c7072383f11)(label(f))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children()))))))))(Secondary((id \
         a9d029a5-254b-41f9-a28a-64afe7f72bc4)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         07a0b022-94de-4fea-af62-e89fe1ab1d75)(content(Whitespace\" \
         \"))))(Tile((id \
         da2d17f1-28aa-48f5-ac37-e376056dd805)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         0943644a-e84b-40d8-b4b2-23b52d405052)(label(f))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         c10b15cd-b34d-4c30-9873-07bf52f4caf6)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         d597675e-8041-4209-8946-ab00d79e7c08)(label(m))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         3c2bcd37-498b-4a8a-9d81-44815ce4e6ff)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         b1b6ce25-3184-41cc-bcd6-05a4b6c5ba1f)(label(grove))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         a7fd8cf7-6772-468e-b218-a04d9f793e85)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         50c75e1f-4a32-4763-8db6-1ed72d1d6d31)(content(Whitespace\" \
         \"))))(Tile((id \
         cf7d1914-a985-483f-9854-292d34fb696d)(label(m))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         64800bf1-a1aa-47e0-88c8-fa38e6e3cfcb)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         f2bbf94b-a10e-413f-8ba5-f81d06f333ad)(label(currentSeed))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         cef591c7-cdeb-4a84-ba38-4f1e6c7e655e)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         cd95be0f-b5e0-41db-90b6-e6b68856b0f3)(content(Whitespace\" \
         \"))))(Tile((id \
         6b85f951-d5ed-47ad-a14e-79f723808fac)(label(m))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         1554d7c3-682e-4235-be21-2577298d137a)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         23ff2134-ff72-4188-9b5a-f2e2c3bfa712)(label(seedInventory))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         403fe02d-707b-4405-b51a-b40f9f417f16)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         044c6bf0-156a-487a-979d-ac58257dea52)(content(Whitespace\"\\n\"))))(Secondary((id \
         9ee0979d-41bc-482e-bbe0-26b516da44ce)(content(Whitespace\"\\n\"))))(Tile((id \
         e0501887-e0c6-4820-8ef1-2117a220f23f)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         df712f1a-9773-4a39-b806-00412fff586d)(content(Whitespace\" \
         \"))))(Tile((id \
         eb6fa733-d7f2-4a61-9a42-b66c807da332)(label(update))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         21ac5bb0-c73e-4de7-ab29-cd2c0eef6741)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 24))(sort Pat))((shape(Concave \
         24))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         f1bc5a55-7fe3-40b5-aa49-516638e39771)(content(Whitespace\" \
         \"))))(Tile((id \
         e2f25731-a384-41e1-951f-3ac3d16e51a1)(label(\"(\"\")\"))(mold((out \
         Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0 1))(children(((Tile((id \
         32a7f7ae-ee98-4c93-96ba-3d8f867db125)(label(Model))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         ea85b708-d061-4541-a946-492a29d14efd)(label(,))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 44))(sort Typ))((shape(Concave \
         44))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         ea37aaeb-3269-4629-bbec-d132eb779583)(content(Whitespace\" \
         \"))))(Tile((id \
         ffbb0b61-4f51-4308-b769-14763df44926)(label(Action))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children()))))))))(Secondary((id \
         18328e34-90e2-466d-b399-632be4b204da)(content(Whitespace\" \
         \"))))(Tile((id \
         a7f76b6d-0f3e-42b5-8bc5-1d21816483c6)(label(->))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 13))(sort Typ))((shape(Concave \
         13))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         2d61fcd1-ad1c-4d40-b144-915e0a4615c6)(content(Whitespace\" \
         \"))))(Tile((id \
         bd1072f0-ae56-4081-a8ad-38712a7a5396)(label(Model))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         5b58f0ae-37a8-4f84-bf5c-8e3be686987d)(content(Whitespace\" \
         \")))))((Secondary((id \
         97e4bd09-ed65-46cb-9e6c-15bf39d09cfe)(content(Whitespace\"\\n\"))))(Tile((id \
         bf1bb30f-5633-4b50-bbd5-484ceffd4149)(label(fun ->))(mold((out \
         Exp)(in_(Pat))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         37))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         26f423fb-f3d1-41bb-ad94-d1d9aa46b054)(content(Whitespace\" \
         \"))))(Tile((id \
         ebd1c10c-efd7-44f7-bac2-52dfe47d1e09)(label(m))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         840b1306-5690-4a4b-b294-c132a02c0be0)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 44))(sort Pat))((shape(Concave \
         44))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         3f6d80c6-0206-4fdf-b929-362aa1f25aca)(content(Whitespace\" \
         \"))))(Tile((id \
         390f2ae6-9215-409d-be21-2962ba0e6f8f)(label(action))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         ce564ae5-5654-4a3f-b566-e37e0056d574)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         041040b4-b5ba-45e4-b5bc-bc7c330dab7b)(content(Whitespace\"\\n\"))))(Tile((id \
         feea4f89-5248-46d0-9724-56396d66020e)(label(case end))(mold((out \
         Exp)(in_(Rul))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         8ca8be0f-1bc8-44da-9e9a-5a8a8170c66c)(content(Whitespace\" \
         \"))))(Tile((id \
         0464b625-4a0b-4cb3-a121-3c8760bebd89)(label(action))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         5e0c8eac-062e-49a8-b5e6-e36ffe70435c)(content(Whitespace\"\\n\"))))(Tile((id \
         b509f80f-8ad8-4c93-886f-b9d7be6f3020)(label(| =>))(mold((out \
         Rul)(in_(Pat))(nibs(((shape(Concave 46))(sort Exp))((shape(Concave \
         46))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         dd03d4e7-e120-4f44-9694-8939cb0254f7)(content(Whitespace\" \
         \"))))(Tile((id \
         a06f35da-6f86-4df6-bc43-5a8c4122ccc2)(label(SelectSeed))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         7f99913a-194a-4a10-8126-db2c807a02b0)(label(\"(\"\")\"))(mold((out \
         Pat)(in_(Pat))(nibs(((shape(Concave 23))(sort Pat))((shape \
         Convex)(sort Pat))))))(shards(0 1))(children(((Tile((id \
         e713ba85-a888-4f4e-b5c6-0738d52b03f1)(label(idx))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children()))))))))(Secondary((id \
         db98fad9-190b-4c9e-b5f1-e25d6dbdbd6e)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         dfc773e6-07e5-4b74-b0f1-79668a1221a4)(content(Whitespace\"\\n\"))))(Tile((id \
         803657de-e22a-4461-b643-cea34472407b)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         c7351c22-02db-4866-803d-b68d13d99a04)(label(m))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         00f6e447-7290-4338-b66a-2032331dbc9e)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         901df55e-d604-45b3-870c-a7658b1a6bee)(label(grove))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         48b26dad-84d1-4bbf-bff6-4c8ec4f81696)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         faa3abac-2565-47e5-8ac7-34c5a0c0f073)(content(Whitespace\" \
         \"))))(Tile((id \
         a7f8319f-7d05-4b1f-a446-23c424fb2634)(label(nth))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         0827c659-ee48-45e5-a9ea-7ff79703e48e)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         4c60ffd9-1308-47b4-a688-f8482c2c3f99)(label(m))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         9a1ae6ee-be0d-4b0e-a819-df5d1b4ca846)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         a6677b03-32d8-4424-a3b6-e7e537adfe1e)(label(seedInventory))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         1b8e5d03-75e5-42c6-9a82-f34c6b7801f0)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         a28c7725-08cc-4f36-83d0-8f67803accdb)(content(Whitespace\" \
         \"))))(Tile((id \
         1a1fdc52-07e2-4cc1-b5ff-ec44833339ef)(label(idx))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         1a5cf60e-db7a-4e52-bc0e-566bf9282e8d)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         2b6c8847-22de-4f64-95ef-e6aa438c8790)(content(Whitespace\" \
         \"))))(Tile((id \
         bcc0a790-dacf-4092-87ac-f52f692487c8)(label(m))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         ae85b784-85f8-4278-bc3c-fc6ee3cfb2d1)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         bc1aadba-9f7a-4d79-97e7-373618282a47)(label(seedInventory))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         27959af9-d2e9-4d24-9690-52998d1eceb4)(content(Whitespace\"\\n\"))))(Tile((id \
         df53cbe7-aaab-46a2-b0da-0a6852b43413)(label(| =>))(mold((out \
         Rul)(in_(Pat))(nibs(((shape(Concave 46))(sort Exp))((shape(Concave \
         46))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         b8279e6a-2fab-4f8a-85e6-b76028287185)(content(Whitespace\" \
         \"))))(Tile((id \
         40af9f68-f9d0-498e-a0b6-6b3362b4ef99)(label(PlantSeed))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         9101032f-f1ab-49a2-8fd8-2c244370acec)(label(\"(\"\")\"))(mold((out \
         Pat)(in_(Pat))(nibs(((shape(Concave 23))(sort Pat))((shape \
         Convex)(sort Pat))))))(shards(0 1))(children(((Tile((id \
         59f7d525-5474-4c23-9696-1f57718c9a41)(label(row))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         ec449d42-49ed-4cb2-ac52-927988adf4b4)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 44))(sort Pat))((shape(Concave \
         44))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         b2454426-ac58-4430-99c8-5dfb500c7a24)(content(Whitespace\" \
         \"))))(Tile((id \
         cc9936f9-3d72-4023-b450-42c06244002d)(label(col))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children()))))))))(Secondary((id \
         ad78d888-050d-4526-b69e-e255decddb94)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         916757cc-b52e-4bb7-82d8-7a0b38b94d79)(content(Whitespace\"\\n\"))))(Tile((id \
         ae4343f3-384f-473b-9001-9bfacb646c46)(label(updateGrove))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         608fde76-e5f7-48a8-b5bc-829177f30c2f)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         b14b0efc-e534-4c71-a201-27327747329c)(label(m))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         65e1746f-b7c3-4c92-8981-41af8237c3e6)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         584e19c2-67d9-496a-9b78-2d27ed4fd0d9)(content(Whitespace\" \
         \"))))(Tile((id 547c3050-ca35-42ee-ae35-1f31d1b900c0)(label(fun \
         ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 37))(sort Exp))))))(shards(0 \
         1))(children(((Secondary((id \
         c95720f9-a0c6-4b02-8b40-11a9347aaa77)(content(Whitespace\" \
         \"))))(Tile((id \
         4c98a1c7-1885-4763-b2b6-406c42f72712)(label(g))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         08e92fca-1e23-4a65-b4cb-80eb0ef92c2f)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         1c6b6729-f220-494e-9b1c-900087681930)(content(Whitespace\" \
         \"))))(Tile((id \
         cc13e333-994f-456b-9bfe-68e476d494d8)(label(setCell))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         046b90b6-acf6-43f5-b562-5f2965f1babb)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         22b466ec-4298-4bfd-80a0-c624dfcff9e0)(label(g))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         410aba00-9204-4e5a-87e2-46de21795645)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         626ea4c5-f68d-4f6c-b7b3-373430037e51)(content(Whitespace\" \
         \"))))(Tile((id \
         cd31a625-77fc-41bf-b46f-ffc1c6f76d04)(label(row))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         c90b7bfe-3bf7-43ed-b90d-e9ba3b02ddce)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         ab6c020e-1d2d-4281-b6f8-200622ab38fb)(content(Whitespace\" \
         \"))))(Tile((id \
         28dd8727-7c07-4d1d-bb0c-11fcf9a60f11)(label(col))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         92c307ee-64b4-443d-beb0-bfec1c648b91)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         dfb21e07-5d3e-4a42-a50f-6c0ef7071032)(content(Whitespace\" \
         \"))))(Tile((id \
         58adf022-4518-466c-91ad-2833bbfa4739)(label(m))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         68e81075-aafd-4cc8-b0e9-86b5edd75236)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         fe9419d9-bbf3-4553-a7d9-3ceb59be5589)(label(currentSeed))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))))))))))))(Secondary((id \
         645b6481-dde7-404a-8238-67cdfde320ca)(content(Whitespace\"\\n\"))))(Tile((id \
         3a5d71a4-921c-4de7-98d0-8b0b128d7c14)(label(| =>))(mold((out \
         Rul)(in_(Pat))(nibs(((shape(Concave 46))(sort Exp))((shape(Concave \
         46))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         fd71f09d-de52-400a-aefa-41fb3393aff0)(content(Whitespace\" \
         \"))))(Tile((id \
         c5a7cce8-e6fc-4491-aee9-7685cfbc2c4f)(label(Uproot))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         a5fb33d9-a7f8-4ed6-9c58-7d2a4a417964)(label(\"(\"\")\"))(mold((out \
         Pat)(in_(Pat))(nibs(((shape(Concave 23))(sort Pat))((shape \
         Convex)(sort Pat))))))(shards(0 1))(children(((Tile((id \
         e653a913-4fbb-4acf-b3b8-d95b68f04572)(label(row))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         487cae3a-cee2-4882-8e6a-156731899412)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 44))(sort Pat))((shape(Concave \
         44))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         778f5d23-c89c-4fda-9037-3363527b11ca)(content(Whitespace\" \
         \"))))(Tile((id \
         7d733428-b2bf-483a-bd6c-21a83e4ed959)(label(col))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children()))))))))(Secondary((id \
         edf56993-77d9-4590-bb7a-8086a1e64cb9)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         aada65ec-7486-4dce-8c53-8aeb3f01e0da)(content(Whitespace\"\\n\"))))(Tile((id \
         6cf5f004-137d-420e-b22e-6b278d09259a)(label(updateGrove))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         7afd2025-6137-47c3-9c6e-c976c6462d55)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         4ceba2d6-6425-41e7-af24-3be6a1ce9ca4)(label(m))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         706caa9b-f84e-4bd1-b748-28ff77a47a66)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         5d79d2de-a4e8-4ad2-876d-bd2d36e8e421)(content(Whitespace\" \
         \"))))(Tile((id 7413debd-f332-4c55-a50d-fa08d7110561)(label(fun \
         ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 37))(sort Exp))))))(shards(0 \
         1))(children(((Secondary((id \
         653f05a9-b640-400c-8468-0a4a35655d04)(content(Whitespace\" \
         \"))))(Tile((id \
         deee0a2b-6f3a-4981-b783-4d199b614bba)(label(g))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         f8175502-0d59-4fdc-a041-1350a2cce6c7)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         ac71fe48-cbbd-4f20-836e-cd664e6d2254)(content(Whitespace\" \
         \"))))(Tile((id \
         0716cb16-f9db-432a-aa6c-7b6e9e5d42c0)(label(setCell))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         44c01776-5bb7-49e5-97fa-f4f47c83893e)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         34112ad7-df74-4d91-a6da-6e8af288d58d)(label(g))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         a52ad408-1b48-4abc-8cca-2de53d2d9ec6)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         39bd9b9f-eef7-425b-859e-787cc6856c27)(content(Whitespace\" \
         \"))))(Tile((id \
         976b18f0-7b9e-48e4-89ee-40a26abd3e86)(label(row))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         b3b6b65f-653f-4632-af55-a30f14cc796d)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         2eee45a4-1652-4d26-941e-fc2c59a82b59)(content(Whitespace\" \
         \"))))(Tile((id \
         576c3ee0-5798-4cb3-a685-0b399c408c38)(label(col))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         fcab97bb-ce09-49f8-a8e3-200ce7d879e2)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         862d705b-b151-41d0-a573-fbe56e093ce0)(content(Whitespace\" \
         \"))))(Tile((id \
         55d39f29-752a-484d-ba4c-351f3469d2ae)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))))))))))))(Secondary((id \
         fa8c75eb-9395-4d2b-928a-6eb189f4666a)(content(Whitespace\"\\n\"))))(Tile((id \
         99ff884f-73d3-4dcb-9dca-da9181277ff1)(label(| =>))(mold((out \
         Rul)(in_(Pat))(nibs(((shape(Concave 46))(sort Exp))((shape(Concave \
         46))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         0cb1f1a4-3348-4fb4-9111-2139dab5dccf)(content(Whitespace\" \
         \"))))(Tile((id \
         7246431b-dbeb-4d87-b686-caeb904ceece)(label(ClearGrove))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         a0ab6928-249b-49d9-84fa-4ca494778849)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         1a3278ee-cc30-4282-b0d6-80ec48643cd4)(content(Whitespace\"\\n\"))))(Tile((id \
         87cc0ce8-b7b8-4e39-8541-b6ed3e59fd13)(label(updateGrove))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         98fbf97b-0703-4000-9b0a-8d545758facc)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         663416c1-23bd-4520-b067-273c7f182768)(label(m))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         7c094591-0407-4c0d-9c82-c6d207e49391)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         8f510e7f-1f67-459d-8f37-1ee3386cb234)(content(Whitespace\" \
         \"))))(Tile((id ed38e8d8-3f75-4b0c-b875-8731c69fa914)(label(fun \
         ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 37))(sort Exp))))))(shards(0 \
         1))(children(((Secondary((id \
         11d76cc8-6f6a-4ccf-b31f-901ca5c0a410)(content(Whitespace\" \
         \"))))(Tile((id \
         6aaad91c-c03f-4824-839c-8e93621e6feb)(label(g))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         5c04c860-ab4f-4239-89e5-0544e63d6000)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         85174550-1a85-44cc-bcfd-4ab54a90e9f4)(content(Whitespace\" \
         \"))))(Tile((id \
         e3ffd77d-8699-434b-abf6-47234ba83011)(label(setAll))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         4de2f6f4-44e5-41a4-bcdc-82904e312680)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         4e767701-ecad-4fa3-ba73-dcad02e02978)(label(g))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         a4d7a461-b9c2-4b83-972c-f85473a73a81)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         4df06cfb-7d58-412e-9efd-e6d156142b48)(content(Whitespace\" \
         \"))))(Tile((id \
         cf5400be-7412-445a-aa26-14a038ee811f)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))))))))))))(Secondary((id \
         1ed9467b-f9cc-4abd-9791-c39a9704c6f4)(content(Whitespace\"\\n\"))))(Tile((id \
         7854c628-caf3-4e1f-99d0-07ad15063121)(label(| =>))(mold((out \
         Rul)(in_(Pat))(nibs(((shape(Concave 46))(sort Exp))((shape(Concave \
         46))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         2cc0fbaa-9b95-48dd-9e70-2c7d6ef92a53)(content(Whitespace\" \
         \"))))(Tile((id \
         5047164f-55fc-4a7d-8131-778cf3a6ab01)(label(PlantRow))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         4dc4f90f-d5ac-40e1-beca-67db74462e73)(label(\"(\"\")\"))(mold((out \
         Pat)(in_(Pat))(nibs(((shape(Concave 23))(sort Pat))((shape \
         Convex)(sort Pat))))))(shards(0 1))(children(((Tile((id \
         b55873c6-c820-4cea-af9c-e312edbea45c)(label(row))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children()))))))))(Secondary((id \
         7e48f22a-5b23-46fb-a379-8565e74daee2)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         316a0755-e363-4ab4-bfc6-0d557ff499b1)(content(Whitespace\"\\n\"))))(Tile((id \
         7eb259cb-2842-46e5-a0c0-a572853fa3c7)(label(updateGrove))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         aa29fdc9-059e-4beb-bdc2-51acb32be21c)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         bf9ac437-137d-4d2f-aae4-a35bb703c10d)(label(m))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         98403f30-78ca-4263-8799-6e93db8700ca)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         689dc385-a7d6-43eb-ac28-dc417b35a98a)(content(Whitespace\" \
         \"))))(Tile((id 02cdab9f-e18d-45e0-8c96-a1f059ba30f1)(label(fun \
         ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 37))(sort Exp))))))(shards(0 \
         1))(children(((Secondary((id \
         d77c5860-bc22-477e-85e0-727b894cded8)(content(Whitespace\" \
         \"))))(Tile((id \
         d7b98cb2-e495-48eb-b740-bf9faef9e60e)(label(g))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         bd475a35-47ff-4f84-9862-210ed4976c02)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         8aa5c4d7-58fe-49d3-ae51-4159523d04ca)(content(Whitespace\" \
         \"))))(Tile((id \
         34d68563-2e19-43d6-a1cb-8bbac685e398)(label(setRow))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         8601af37-83da-4294-a500-cf961da6ee75)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         6be493d4-b8d3-4c15-a84d-6f26aff77bfa)(label(g))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         738d03eb-c533-4cc4-8c82-371a5e0e645d)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         915f8231-0fbd-40dd-91cc-264662a35cea)(content(Whitespace\" \
         \"))))(Tile((id \
         53c1d89f-66d0-4a2c-a533-69106dc2d7dc)(label(row))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         3dc56ca1-d6ba-46cc-b86d-4416687e9c8d)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         f3e64eab-4825-4978-a847-59672e5ed317)(content(Whitespace\" \
         \"))))(Tile((id \
         a2562f30-8679-4870-8630-9c3bbcbae6bd)(label(m))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         6a698c48-5670-4ba0-bfd2-7df5b3df908e)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         04db5689-9533-4c16-9a53-cc7da7bff165)(label(currentSeed))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))))))))))))(Secondary((id \
         d5eca1b1-635e-45fc-b4da-6953a4180b89)(content(Whitespace\"\\n\"))))(Secondary((id \
         d7d6ab0b-b602-4c94-80dd-54966775cae4)(content(Comment\"# TODO: Add \
         PlantCol case here #\"))))(Secondary((id \
         a99b3663-d89c-43d1-aec6-fe13fc25cea0)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         bbc467b4-016d-45d3-bc49-31eade979ab9)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         7887bd80-2823-4e43-89ef-e530b4dd64bc)(content(Whitespace\"\\n\"))))(Secondary((id \
         8b31e48f-11d9-42fa-bf60-3fbc4ac2ad13)(content(Whitespace\"\\n\"))))(Tile((id \
         c9429b4b-9709-456c-88b9-15b5c519e041)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         a8aa1bde-b419-4737-96e5-46ad1b4ae6f2)(content(Whitespace\" \
         \"))))(Tile((id \
         21ec844d-bdab-434c-a0f6-4cbe2dc78320)(label(do))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         e12acbe6-09a5-4b2c-a5dc-ada8464b37a6)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 24))(sort Pat))((shape(Concave \
         24))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         4b2eb540-9806-4bb5-89db-6968ed7dc5da)(content(Whitespace\" \
         \"))))(Tile((id \
         3862016f-1c0d-4ffe-9785-1cf51087e87f)(label(\"(\"\")\"))(mold((out \
         Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0 1))(children(((Tile((id \
         759a3da6-47b9-4243-9f5a-18d21e5be757)(label(Model))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         d96470ea-3094-4f2d-8625-563a45aa6fb2)(label(,))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 44))(sort Typ))((shape(Concave \
         44))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         e6032919-3ce8-4aba-ab46-be0de20132ad)(content(Whitespace\" \
         \"))))(Tile((id b5613edd-48f5-41de-bcf5-3eab9cf637d5)(label([ \
         ]))(mold((out Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape \
         Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
         07173847-58e6-4079-abbb-293ce6c1ab45)(label(Action))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))))))))))))(Secondary((id \
         26ae5f62-b4c0-4869-b0e0-10590840b7fd)(content(Whitespace\" \
         \"))))(Tile((id \
         f7cffe14-56f8-4b57-be35-06bc592bdd78)(label(->))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 13))(sort Typ))((shape(Concave \
         13))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         202ef724-aec7-4336-ae69-4ad272cbbc00)(content(Whitespace\" \
         \"))))(Tile((id \
         a915af07-9a8b-4727-aba2-e2d4b39d0e8c)(label(Model))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         f4ead50d-1b8a-45d3-9ce9-4735964025c2)(content(Whitespace\" \
         \")))))((Secondary((id \
         91f10f1f-15b8-4c70-abd6-3580ed528374)(content(Whitespace\"\\n\"))))(Tile((id \
         ebc49161-358d-431d-8df6-6123a7ee255e)(label(fun ->))(mold((out \
         Exp)(in_(Pat))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         37))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         9c8ab263-dd89-4c1b-9438-55864fb5462e)(content(Whitespace\" \
         \"))))(Tile((id \
         dff9aabf-1819-4064-a832-be5b6e5ac02d)(label(\"(\"\")\"))(mold((out \
         Pat)(in_(Pat))(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0 1))(children(((Tile((id \
         3ef16e76-3b34-4d7d-94dc-237244bc609c)(label(init))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         c40b60b2-8862-42ff-a149-3b22b62adb7e)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 24))(sort Pat))((shape(Concave \
         24))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         3b97c62e-7f35-483b-be0c-9f906ac3a008)(content(Whitespace\" \
         \"))))(Tile((id \
         1c423d32-a7ad-4ad4-91c3-d1db5f437f10)(label(Model))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         efacd7a2-ec0f-458d-ad72-ee75fbebf5f0)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 44))(sort Pat))((shape(Concave \
         44))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         233cb93e-6ecf-4ebe-9aba-773b4c40f0f1)(content(Whitespace\" \
         \"))))(Tile((id \
         a5bb39c3-89b9-4f53-af6c-2df7301f9ccb)(label(actions))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         57ef8692-fe5d-4643-a346-e36211efd905)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 24))(sort Pat))((shape(Concave \
         24))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         0693a2b0-e29b-4363-a15b-05d1151453a4)(content(Whitespace\" \
         \"))))(Tile((id 291f5e55-2dea-4555-9f56-f46bad57d152)(label([ \
         ]))(mold((out Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape \
         Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
         9abe35b1-691b-4e9e-b18f-3a205173ac0c)(label(Action))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))))))))))))(Secondary((id \
         355ec664-b2fc-46fa-8f7c-300048e83e13)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         92df7424-439c-4c91-99a5-9e6df315e195)(content(Whitespace\"\\n\"))))(Tile((id \
         cedce528-ac22-4572-89b0-db06e1a9e348)(label(fold_left))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         bfbdc6aa-ed3d-43e6-9f74-74c732831afd)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         aeef2e7c-57f6-4448-ace7-dfd73f668f07)(label(actions))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         4eb2773f-5b4f-405f-b343-eab38905ba48)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         24b6fc2a-fc6f-4a2d-aea8-731c358e910e)(content(Whitespace\" \
         \"))))(Tile((id \
         a2a5da23-17e7-4e3c-a895-9c183c24bd3f)(label(update))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         7f368a1a-d59a-4ec0-a959-1d28baf901c5)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         bd595f83-2255-434f-aad6-7e2be52a99db)(content(Whitespace\" \
         \"))))(Tile((id \
         a9845a65-24ad-49df-92da-ce545723ef3f)(label(init))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         e3f675b9-eb3e-4899-90af-f1ffb5610834)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         3c8c86f4-e2dd-436f-9fa3-1cdc9e8e782d)(content(Whitespace\"\\n\"))))(Secondary((id \
         61774ef4-ad01-4849-b82e-8f27dc7326f1)(content(Whitespace\"\\n\"))))(Secondary((id \
         21cce60f-180d-4504-9162-fc83bdc2f524)(content(Comment\"# Existing \
         tests #\"))))(Secondary((id \
         b2f14412-f9e6-4ff9-99c0-0680aeead06d)(content(Whitespace\"\\n\"))))(Tile((id \
         57466908-3320-466e-a5c2-13c0c1278c13)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         0a26f4db-8073-45d1-9a89-5fa8de49ef62)(content(Whitespace\"\\n\"))))(Tile((id \
         b9371414-0a5b-4b29-8151-d895941ac109)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         63c985bf-6275-40c4-8318-f7b79fd28952)(content(Whitespace\" \
         \"))))(Tile((id \
         b58f63bd-fea4-40e6-a9d8-7dada3e99e19)(label(m))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         9f38a128-527a-4b4e-9064-cf55e48e1a6e)(content(Whitespace\" \
         \")))))((Secondary((id \
         26c07585-1588-4133-b47b-d3e926eb9e42)(content(Whitespace\" \
         \"))))(Tile((id \
         6951bf85-b2e0-4e98-af67-4ec03f39cd08)(label(update))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         f471d04c-06cf-49fa-b398-20c7421ec0bb)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         6b32ad0a-ab73-449a-bbf7-504c290188a2)(label(init))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         de4d3032-fed4-4d7c-9d73-c45bad673ada)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         7b865068-0213-4c19-8fd3-f27bc96cfaf5)(content(Whitespace\" \
         \"))))(Tile((id \
         6eef60cf-4f19-4a1f-9cdb-7252af05dd8e)(label(PlantRow))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         f22915e0-6460-4485-a4fb-3ba9285fba84)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         3a875f65-21fc-4c0b-8ea9-10e938c2452d)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))))))))))))(Secondary((id \
         24b57af9-2b4c-4067-abf4-69c0447ce507)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         3fdd0dd5-db22-440e-82e4-ce4032a01f79)(content(Whitespace\"\\n\"))))(Tile((id \
         56f22d48-b06a-4386-9a58-52c1c61de3e6)(label(m))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         7b3629a1-3c82-4aff-9613-d426b85f71ec)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         3a3d51b6-8947-4e14-9b64-42eff48d7954)(label(grove))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         cc7e4bc4-7286-4df5-8a7f-268ffaaf4328)(content(Whitespace\" \
         \"))))(Tile((id \
         8eba1a3c-be20-453d-96eb-7b5dbe5dd1fa)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         8d499453-d918-45e1-9f02-9af1cbaa105a)(content(Whitespace\" \
         \"))))(Tile((id cf03ec77-99cb-43d8-a519-eef124e7f27b)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         db9961f8-a126-4c53-8a07-8318e1fbf983)(label([ ]))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         698821d1-0575-4bf2-8b1c-c3dc4c61b906)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         c6f6a87d-ad72-46c9-a0fc-05c191057f14)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         5cc74af3-91cc-48a9-b3ab-c770159a8b8e)(content(Whitespace\" \
         \"))))(Tile((id \
         32fd196a-b25b-477d-a5a4-45f964b74b75)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         58173c5e-ad78-480f-b60a-1a019570de32)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         8bc1b9b3-c885-46dd-aded-3922671525e3)(content(Whitespace\" \
         \"))))(Tile((id \
         d8a67619-0193-46c9-b5db-e17276cb9964)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         364b0c58-d2b7-4c5d-8576-f0f1237098a5)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         44bddcd0-f9dd-4c2b-b118-affd6ef6549f)(content(Whitespace\" \
         \"))))(Tile((id be9ace97-8dd2-4913-a64a-3ae3afe30890)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         8eac2ee7-d8ce-4543-bd6b-669c18e69ebc)(label(\"\\\"\\240\\159\\140\\177\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         61c2acc0-f82d-4c9b-83e0-4c5b37d149c9)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         d402af14-3b42-4a40-919d-cf51b54378dd)(content(Whitespace\" \
         \"))))(Tile((id \
         643f98ab-5861-4b8f-92ea-80217e52b711)(label(\"\\\"\\240\\159\\140\\177\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         558b6f4a-e049-4cc0-86c1-b9955f5111a2)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         5ed3c67a-47d5-461d-8238-69761bee43c1)(content(Whitespace\" \
         \"))))(Tile((id \
         41e81560-d5a6-4d50-bc1b-f76c2a533631)(label(\"\\\"\\240\\159\\140\\177\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         957ec0e5-932b-4432-a0ca-162813ff13d2)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         0f29c37b-2e21-4e31-be70-7c407a4dc2a0)(content(Whitespace\" \
         \"))))(Tile((id 555934bb-7d0c-42f9-9acc-59a1a061f3c4)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         32f9a166-2624-46d5-bf5b-c80b857359c8)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         22342b52-ddde-45d9-b276-6395bdf6fcc5)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         4c96a039-bc34-40f3-a358-d24152454c09)(content(Whitespace\" \
         \"))))(Tile((id \
         a661d3e6-87d9-4cc7-89a5-aefa0512bba6)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         bceb97f0-ac0f-4f71-9a9f-fd2bcc7eef0d)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         54fbf3b2-ff71-40fa-b7f9-b99e28f71b8d)(content(Whitespace\" \
         \"))))(Tile((id \
         c985f1b6-d366-49ab-8547-d7af4c4f6ab2)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))))))))))))(Secondary((id \
         70890295-1940-40f6-b8eb-f43e5103fff2)(content(Whitespace\"\\n\")))))))))(Tile((id \
         eb0db6db-5c08-4a54-8480-fec7a238db8a)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         bfd62c1c-46fc-416c-ae1a-259a3623c4c6)(content(Whitespace\"\\n\"))))(Secondary((id \
         1bdebca3-87c6-4fec-b51f-3cd9b49af3e9)(content(Whitespace\"\\n\"))))(Secondary((id \
         c87d1693-81d0-4343-9784-0c149d695ca9)(content(Comment\"# New tests \
         for PlantCol #\"))))(Secondary((id \
         7afcf165-979f-4eeb-a650-b8411feeb5b0)(content(Whitespace\"\\n\"))))(Tile((id \
         db961d17-ad45-4a46-b156-7da95435407e)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         0fe11d4b-de0f-45c1-9eca-8aae5d40d786)(content(Whitespace\"\\n\"))))(Tile((id \
         02e18777-176b-456e-afaa-9452d43e4299)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         0dd7ed30-39e1-41b4-97bc-43aed000130a)(content(Whitespace\" \
         \"))))(Tile((id \
         868762a1-427f-4720-880c-1f6a532ccf42)(label(m))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         302a1c50-bea1-4df1-ac72-cd80a1b186fa)(content(Whitespace\" \
         \")))))((Secondary((id \
         312e733b-1acf-4d98-b637-fbc9587455b8)(content(Whitespace\" \
         \"))))(Tile((id \
         0b6e0861-dfd3-4460-a339-ebcbf7d3deff)(label(update))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         e837be52-5b5d-4e67-94db-b6edcf6055ba)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         7d79ba59-a3f4-4bf3-962b-9bf9501ca403)(label(init))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         670ff837-f24b-44ba-a188-1681c17ca48c)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         828f407c-84e1-4684-8506-bb9fcfaad891)(content(Whitespace\" \
         \"))))(Tile((id \
         59350b7a-051e-4262-ad47-4c180f753e16)(label(PlantCol))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         e9f935b8-d385-4d55-b0fc-b8bd9d6c50ff)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         d45fe6a9-603f-46b0-acb0-776425b43fa3)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))))))))))))(Secondary((id \
         93e03e2d-5a4a-4516-8309-650058f5bfbc)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         57485efa-fe37-44a8-b633-a1344b9401d9)(content(Whitespace\"\\n\"))))(Tile((id \
         b85bd9a4-8dcf-4607-8788-a3860d7d05de)(label(m))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         04150174-52db-46c4-aa7f-846a72d0cfca)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         059702e5-b8db-48bd-a6c3-e38afe9ad4df)(label(grove))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         e2034707-1d95-4464-8690-fcd75cfbb3b0)(content(Whitespace\" \
         \"))))(Tile((id \
         f23ae3ad-4035-4324-803c-f765d1ab3c91)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         0ce04031-018d-4ab2-be76-d9277dc06dad)(content(Whitespace\" \
         \"))))(Tile((id c169bba5-551e-4a8a-b16d-d4ed1f8dcb23)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         60dff0bc-0e0e-4e41-a4d4-9d52cb9c9b8f)(label([ ]))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         7296accb-fb4f-43f9-94b1-74bd51c0a521)(label(\"\\\"\\240\\159\\140\\177\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         6b77aeaf-9853-4db6-9274-6617cc107494)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         ae7016b6-07e7-4b5d-a3bc-2fed56ee6e03)(content(Whitespace\" \
         \"))))(Tile((id \
         58996b79-046b-409a-a1e4-eea0687f250d)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         cb6cef6b-1098-42c1-8216-ee7b83649fea)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         1f71f8a0-41e7-4a71-a50e-b88c1550a424)(content(Whitespace\" \
         \"))))(Tile((id \
         30ca4042-4a38-4f53-a033-345c97b28a7f)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         ddb90c42-35e7-4f66-98a9-006c0e1d9bf4)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         c67baeac-cf32-4c64-b163-22065e62927e)(content(Whitespace\" \
         \"))))(Tile((id 2263c1dc-dce8-4097-ad95-65054e14ffed)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         00c6ceb7-4ecb-47ef-b370-7bf789a7bcac)(label(\"\\\"\\240\\159\\140\\177\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         2ad0e915-2901-42df-a6b0-a3bbd7fbc8fb)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         8394811f-e891-4f37-b108-db22327879cf)(content(Whitespace\" \
         \"))))(Tile((id \
         a6657f82-73b1-4880-b294-2b83df30d82e)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         f44638a3-b8a3-4454-9bbb-d8f076c093f8)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         c12f4edc-8599-40ca-8d4e-a50009be8334)(content(Whitespace\" \
         \"))))(Tile((id \
         dec06b1a-b85f-4b34-81d6-7a68daa36950)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         105c28dc-bb89-43a1-9d10-365b2dc1ff21)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         ab8125fc-a727-477a-9a97-eee0bbd651be)(content(Whitespace\" \
         \"))))(Tile((id 84300c4c-5ab2-4018-8e58-97da3dade925)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         17457946-1be2-4d77-8aa4-d9a65453fe97)(label(\"\\\"\\240\\159\\140\\177\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         bffa7e28-3578-4efa-808f-0c7dc2dbca69)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         4fdfdc1d-69dc-42a3-82f9-6ac7a345c94e)(content(Whitespace\" \
         \"))))(Tile((id \
         ca548178-20f2-490a-961b-08da0785169e)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         e5d4d56b-8a38-40d4-8f71-aadfe0404b7c)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         14ecba8b-6613-4fb8-9b30-4b68d667fb99)(content(Whitespace\" \
         \"))))(Tile((id \
         1ddfc1a2-11ab-43de-aa00-1bb1b059a60e)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))))))))))))(Secondary((id \
         1559e84b-9484-4e95-ba9e-592d3f446321)(content(Whitespace\"\\n\")))))))))(Tile((id \
         4835666b-eb61-4df4-a84a-a07a18132355)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         39bb76d4-28f1-4d72-a5a2-5f40a58fd803)(content(Whitespace\"\\n\"))))(Secondary((id \
         fc5967fa-6568-4560-bbb8-069f3cf0ef16)(content(Whitespace\"\\n\"))))(Tile((id \
         29e3fa77-9ce3-42c6-866b-97da22c33816)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         3a4c72e9-d2c4-4b49-8b20-fa47fa2ca1ed)(content(Whitespace\"\\n\"))))(Tile((id \
         5d7220ba-426a-40a7-837f-3de9ac4f16f5)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         d8f600f1-f537-4290-833c-196e78d72d78)(content(Whitespace\" \
         \"))))(Tile((id \
         130a6989-fff6-4d2f-b5ee-55f5074586cd)(label(m))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         44bebda4-cb19-4e15-99fa-9969d120d61e)(content(Whitespace\" \
         \")))))((Secondary((id \
         a453fcce-a029-4c51-82db-908eceab5706)(content(Whitespace\" \
         \"))))(Tile((id \
         6d38bac9-9f32-4d35-9465-40683708e230)(label(update))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         4d8c649b-0907-471d-a4f7-66aadffe505e)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         84093f67-6fd7-4a66-90de-8f13eb80da48)(label(init))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         e37e4297-429c-4444-a008-8e01d22bc6a3)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         9da33643-75c3-446f-a880-380ec8688f0b)(content(Whitespace\" \
         \"))))(Tile((id \
         52de5877-7646-4e74-89bd-3bbf22c0c05e)(label(PlantCol))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         4a4229ac-7df2-479d-809f-33089f858fe6)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         6e81922a-bb83-4b9e-b47c-f9b59f72fef9)(label(2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))))))))))))(Secondary((id \
         0dc3e8a9-94ba-411f-bacd-b6241ce144b3)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         982af977-1ce0-4ed1-8df2-bf54b5f51085)(content(Whitespace\"\\n\"))))(Tile((id \
         d31f4190-b74e-465b-9016-a7355e259bd5)(label(m))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         ef993a5f-52f8-4c85-bd0f-526eb05df83b)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         2ba31c82-420f-472d-a700-72a172729b97)(label(grove))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         6e402015-d066-4566-b0c7-463b23ed101e)(content(Whitespace\" \
         \"))))(Tile((id \
         d8caa419-44b8-4358-9499-d8edcf81aec0)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         2399ab31-0d84-432f-b88c-a442b20b8ebd)(content(Whitespace\" \
         \"))))(Tile((id 01a50e21-a69d-4805-b920-d537a27e8bf3)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         08e507ab-1f34-4df4-b7ad-c8cbd50d0431)(label([ ]))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         9fa40685-ab66-4ff7-8711-60a22eb9e2b3)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         3c2958c7-9373-4a69-9887-da78287e1bad)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         1e2412ac-48e0-4dff-aa9c-6f8fb3a81312)(content(Whitespace\" \
         \"))))(Tile((id \
         3a7b874e-9620-4c04-bd6f-453e065ffc8d)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         f64ec884-b1f9-4fa7-ac35-092b9590e01e)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         a78478b2-37ad-42e7-973b-0c994377fa7f)(content(Whitespace\" \
         \"))))(Tile((id \
         72eb328b-2937-44b5-8a5e-c24ae9bf87e3)(label(\"\\\"\\240\\159\\140\\177\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         a746d0be-28cd-467a-9373-045fe29e766d)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         0b8f0b12-0351-486a-9423-9f033138eebf)(content(Whitespace\" \
         \"))))(Tile((id 35fc475f-3555-430c-89ff-9511a247f8ff)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         4d880e2a-bd31-4bed-99f2-44d4ef002cda)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         14f7e870-7e78-4fc0-9e08-91f6b25f1be5)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         4e909109-bf0a-4938-b7b8-3f5a2cac7d54)(content(Whitespace\" \
         \"))))(Tile((id \
         b53da898-6527-4175-a19d-16f24ba78fbc)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         ac0d0a8e-5542-401c-8fd7-ece652822723)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         27fb02a5-aff7-47e8-a22b-36ea817428d3)(content(Whitespace\" \
         \"))))(Tile((id \
         57a6457c-b9f4-4538-a3c5-8fb020865d28)(label(\"\\\"\\240\\159\\140\\177\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         14ded36d-27bc-433a-9c01-c6b657bb1a3b)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         6c8dc98a-aafd-4622-80e6-bb2bc3d3b6a7)(content(Whitespace\" \
         \"))))(Tile((id 5a012146-0e46-4a49-bb5f-cc80162be17f)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         852b8ff5-1984-4211-9a1b-c42b43b08836)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         6cc5c93f-4fe3-46f0-9a93-420fa2d1dcc3)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         3bdb3ab6-9f51-472c-a5cc-49ea165e5609)(content(Whitespace\" \
         \"))))(Tile((id \
         998f1fb1-ef19-49cd-ac36-8f2414a5322a)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         9186374d-081b-455b-96db-5d6d02209529)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         440062e4-cab2-44fa-a2f3-f5407251a58c)(content(Whitespace\" \
         \"))))(Tile((id \
         bd49d72c-33df-4fb6-b92f-a9e84d8d5c86)(label(\"\\\"\\240\\159\\140\\177\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))))))))))))(Secondary((id \
         82e27d03-3817-402b-9be4-027173abf3b5)(content(Whitespace\"\\n\")))))))))(Tile((id \
         694d23f7-c083-4463-9a76-5335b77464f9)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         25e36c17-ccac-416f-9ff7-336a6843a5e1)(content(Whitespace\"\\n\"))))(Secondary((id \
         333e5521-7829-4c76-a46d-a3d006e11619)(content(Whitespace\"\\n\"))))(Tile((id \
         51811fb0-3e1d-47c5-bc9a-492bc20192b5)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         dc24322d-1e3e-4c75-b2f6-f2e9615c09a2)(content(Whitespace\"\\n\"))))(Tile((id \
         93adc3fe-26cf-43b9-8a5c-07d522fa5674)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         4f6fcd6a-9ff3-4630-9d71-0a308bdcccc8)(content(Whitespace\" \
         \"))))(Tile((id \
         26069d15-2de5-4f8a-819a-7db977933d22)(label(m))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         2ac05152-bf4a-42e2-a2ae-66631ca9104c)(content(Whitespace\" \
         \")))))((Secondary((id \
         d1421156-0d35-428b-b898-c20e1fca5f48)(content(Whitespace\" \
         \"))))(Tile((id \
         ce1d6e39-8d88-44d1-8bcf-7b9abf605ae8)(label(do))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         8f3a11de-44bb-4e28-bc43-89293018ea37)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         4ba33413-08df-4e76-8ea7-27769b31e38f)(label(init))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         de3f11a4-545b-4d82-817c-a3322e2d89d1)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         70296aee-9d77-4397-95cf-254e324f4970)(content(Whitespace\" \
         \"))))(Tile((id ada150bf-7dc7-44ce-a13b-3e56a49317f9)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         e30988f1-1c25-4e0d-8340-6864b25e2eae)(label(PlantRow))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         60c70947-0c46-4c67-b2dd-1064397f89e6)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         59499a42-563e-4ff1-9365-80bdd5da0365)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         c0abed7f-e67d-4eee-b63c-45b8c3ba5118)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         926848f6-7cbd-436c-9b5f-81c7df3353bc)(content(Whitespace\" \
         \"))))(Tile((id \
         bb003768-2eb9-4f6d-9735-060d05bcaccf)(label(PlantCol))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         ffc4cfa6-30e8-432d-9680-6d31b48f49a5)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         ca8f1273-24a4-413d-8856-cf041598b19c)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))))))))))))(Secondary((id \
         088c6ac2-be2c-42d7-9e9c-46c443611371)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         f3f5901a-cd39-4a4e-9a14-bcf619e1a8a5)(content(Whitespace\"\\n\"))))(Tile((id \
         3fe20dd6-8498-43b4-95cd-e1b6bee8eb79)(label(m))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         efd69cb8-af88-45af-af3f-4511b1ff8bb2)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         dccb533c-e41c-4a08-be84-9e65b28102f9)(label(grove))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         b62c26a3-d490-4a3b-b93f-a27d2c9c4fec)(content(Whitespace\" \
         \"))))(Tile((id \
         05c88c93-8eed-4703-9e02-181e05475f3e)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         b35e5b6c-3597-4da1-8c7d-1e9afa9e03a0)(content(Whitespace\" \
         \"))))(Tile((id 6419cf49-07ae-458c-9647-26b30a9c8ca3)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         94f97dc3-0bd9-41d6-bcb1-34aa80edaba9)(label([ ]))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         d370db4a-b3b8-4023-bb4d-7f90c1e9bfe6)(label(\"\\\"\\240\\159\\140\\177\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         19e3417a-6d96-4fa3-84a8-b6862b869143)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         b6b22cb3-c56a-4e94-9831-491261d82808)(content(Whitespace\" \
         \"))))(Tile((id \
         a4c11bf2-92a3-4c74-a915-b7ee31408483)(label(\"\\\"\\240\\159\\140\\177\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         b9417e13-945f-4acf-b369-8549ade6e295)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         3bea1746-412c-4844-93c0-0b081f2e96a9)(content(Whitespace\" \
         \"))))(Tile((id \
         794082d8-2f0c-422b-a5cc-1a156b9e2083)(label(\"\\\"\\240\\159\\140\\177\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         f52378c6-abe6-420f-96a3-4aa3c11eb206)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         43cf306e-12ee-43ef-bc28-89d2266d854c)(content(Whitespace\" \
         \"))))(Tile((id 1c400d63-3635-4e01-946b-bbd1785c12c3)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         3d726311-bf03-43c5-b645-e8927a02f893)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         e09cefc4-ae16-4286-b805-861e842f2836)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         dac2e8dd-019e-4ce1-ae01-64ff55bab562)(content(Whitespace\" \
         \"))))(Tile((id \
         20029471-6694-4d52-8f6d-8fe631f2367b)(label(\"\\\"\\240\\159\\140\\177\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         86416861-fec2-49e9-9790-440c8c3704d1)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         cc9f89a0-a2ba-47dd-85e5-afd8309e4695)(content(Whitespace\" \
         \"))))(Tile((id \
         1165b0f6-dddb-42da-9e0b-a916c5b25e79)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         4cb7ca27-fd51-436c-962b-6330a1ae3f5b)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         e81acda6-c704-4b32-90bc-fa3dc8428709)(content(Whitespace\" \
         \"))))(Tile((id 3147ee74-ef41-4b3d-b9a5-346d77853d1f)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         e27e7903-8f34-4367-8fbe-e245fe518f34)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         e72a65c9-1746-40a0-907a-534efa1c26f4)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         7a8388a8-36ec-40aa-a284-d6e9a3f5821e)(content(Whitespace\" \
         \"))))(Tile((id \
         6df33955-b448-41c9-a473-725e1935071a)(label(\"\\\"\\240\\159\\140\\177\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         b8faa382-cda0-48b2-bb1b-8193d18903a1)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         a1c4ce21-7a47-4a30-a737-98bcc22d415e)(content(Whitespace\" \
         \"))))(Tile((id \
         366d0258-1acb-42ad-8e45-53625d0466a2)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))))))))))))(Secondary((id \
         7f02a788-4a4e-4825-9bfd-c96813f3386a)(content(Whitespace\"\\n\")))))))))(Tile((id \
         21277de1-c545-42d3-a271-fd5060f45a71)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         b7db287b-9f0b-4643-9b15-301dc7adc5c5)(content(Whitespace\"\\n\"))))(Secondary((id \
         1f1d516a-748e-4a26-9679-168b621647d3)(content(Whitespace\"\\n\"))))(Tile((id \
         ef0940d0-585e-44bc-a3c5-37101be8ffab)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         d062f486-ca3d-4c94-9792-e58b9471c55b)(content(Whitespace\"\\n\"))))(Tile((id \
         c1e9980c-c614-4057-b02f-39d6efb9aa53)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         857747f9-b9b1-4620-ad73-e137219c54ac)(content(Whitespace\" \
         \"))))(Tile((id \
         c2aafd68-1239-4265-9d36-d3691f1eef62)(label(m))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         dc1a4842-864f-480d-82dc-6e24caad05ea)(content(Whitespace\" \
         \")))))((Secondary((id \
         21427f9b-9e28-48fd-92fe-a8717eba2355)(content(Whitespace\" \
         \"))))(Tile((id \
         11449e0f-9f08-4865-9622-c67343d9f0d2)(label(do))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         7a1419a3-9029-45e4-9a1c-a1387991aaf7)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         bb0edb1e-cfef-491c-90fc-426bc9fb0096)(label(init))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         94d6424a-13f0-4229-baea-75172f0b6c8c)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         c22f8e63-dd0d-46b2-bff9-276c5d1f6c25)(content(Whitespace\" \
         \"))))(Tile((id 082dea1b-7d6b-4c07-904c-79b6f2be26d3)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         310efcc5-025c-4fd4-847a-61106744e6a2)(label(SelectSeed))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         823f604b-6cb9-429b-b48d-103d6d056eca)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         5ccf1f73-07e7-4df9-923b-69bf4727e528)(label(2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         42599020-fa91-405b-ad60-8f635155cee9)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         a2ee16c0-e7ee-4c4b-8309-a2617a99c685)(content(Whitespace\" \
         \"))))(Tile((id \
         cb768e84-dd7b-4ad6-8a8a-af0cdec86211)(label(PlantCol))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         d0e8961e-9202-4136-ab9c-f1ff9a05b38a)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         f26581aa-d34a-43a2-be26-01161ced1102)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))))))))))))(Secondary((id \
         fd38270c-5e04-4127-949a-cee9ea609f95)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         48006af1-f674-4a9f-9b67-ffbb18cbbaec)(content(Whitespace\"\\n\"))))(Tile((id \
         7223bac1-f56c-4185-abe9-6bd126e973f4)(label(m))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         875824b5-608e-409a-afc5-92c7bfb61524)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         de03f984-7876-4b5a-9465-0f3af8df58c6)(label(grove))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         e08f42e8-990a-40d4-a3ad-50e64d822eef)(content(Whitespace\" \
         \"))))(Tile((id \
         7cb0ae20-e000-4013-8b8f-f739ebf24608)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         b56c737c-a961-4c3f-b4a9-0b3837ca706d)(content(Whitespace\" \
         \"))))(Tile((id 0f618480-a55e-442d-ad06-0223afd095b7)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         cbf4eeee-58fb-4cb0-a4be-f4a5b364e214)(label([ ]))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         91e8ea3f-5657-451e-b9be-63cf0601fc0b)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         b0ef2007-8e0a-4d11-b819-e88b290e2b56)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         76e91f95-257b-4c43-af5d-13441ede7b0a)(content(Whitespace\" \
         \"))))(Tile((id \
         b2bdad6e-c250-406f-ad3b-a350f2902b2b)(label(\"\\\"\\240\\159\\141\\132\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         e7676042-6a49-453f-9c9e-6dd3577f003a)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         e6fddfd8-d05c-4e6d-a038-2a34178a40c5)(content(Whitespace\" \
         \"))))(Tile((id \
         95831cf4-9b9d-41ac-b36e-eb1e5e32a4ae)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         02f9bda5-87d9-4c62-a408-44678fbfb143)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         0d76e467-9b1f-4e4d-a656-83d30d57e549)(content(Whitespace\" \
         \"))))(Tile((id 68b55a83-43f7-420b-8b87-79ba213ac418)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         43d482a3-839b-43c8-9d08-7a5721e61d4d)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         78754abc-f047-4935-90bb-5a0ad07c1292)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         d7a965bb-35bd-4f85-b508-f35e954bf421)(content(Whitespace\" \
         \"))))(Tile((id \
         ed7ac0d6-d311-4c21-98f4-d33092b28151)(label(\"\\\"\\240\\159\\141\\132\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         3245970c-7819-4fdc-8ecb-fe42abacb09e)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         fc9e8f24-b8ac-43ef-bec2-a46832c3dc39)(content(Whitespace\" \
         \"))))(Tile((id \
         bd5f6be4-33cd-44ff-b8e2-c7743a1830af)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         653ecbd0-748c-4ac3-9ba1-0b2dfcc72f62)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         32735aeb-0c4e-4bcd-bbee-d373384f57c0)(content(Whitespace\" \
         \"))))(Tile((id fd09a918-6b58-4041-9038-0b394e551b4b)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         eab99a34-488d-4620-82eb-8625b7b3e405)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         40c95c6e-762d-43e4-961e-334ec8327d79)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         d95e5790-ff69-43e9-b851-a92e5f9e3a7b)(content(Whitespace\" \
         \"))))(Tile((id \
         1dc075ed-1f4d-4354-b11e-c92b29722de7)(label(\"\\\"\\240\\159\\141\\132\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         881d518f-2f17-46c5-a624-cb55633a43fe)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         4f8d133c-6236-42a4-955a-22bcce7f782b)(content(Whitespace\" \
         \"))))(Tile((id \
         b1526fb8-8192-4df1-8038-29b908556f9c)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))))))))))))(Secondary((id \
         9c6b9201-7a25-4128-87ab-c15f7d5d8a3a)(content(Whitespace\"\\n\"))))(Tile((id \
         5bb838b8-9c25-4178-a601-6a6b31d022ee)(label(&&))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 32))(sort Exp))((shape(Concave \
         32))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         ca1a3e1b-e38e-488b-8246-7635845ee8fa)(content(Whitespace\" \
         \"))))(Tile((id \
         c358a7f6-e6dd-4306-b2c5-f3e3e4019d9f)(label(m))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         8c6772b1-9ea1-478f-bd69-66ee54ad5401)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         7ceddb83-8830-4bca-b9d7-1f0f5863c838)(label(currentSeed))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         0e4bb47f-5a66-4ffa-ae6e-586fd90cc943)(content(Whitespace\" \
         \"))))(Tile((id \
         805a1b14-8e2e-4c98-aa28-5a8b89f433d3)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         3bcdbe10-f3f8-4f6c-a29a-655e3ef4977e)(content(Whitespace\" \
         \"))))(Tile((id \
         f6aa6f96-fae9-4397-af02-6c4370327e0d)(label(\"\\\"\\240\\159\\141\\132\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         0ee9af8a-f953-48e1-90ef-4a93135d3593)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         b3f4b668-e34e-42d7-b8de-ed0b1b3b6f67)(content(Whitespace\"\\n\")))))";
      backup_text =
        "# CROP PLOTTER EXTENSION TASK                     #\n\
         #                                                 #\n\
         # The crop plotter app lets you plant seeds on    #\n\
         # a grid. It already supports planting rows.      #\n\
         #                                                 #\n\
         # YOUR TASK: Add a PlantCol action that fills     #\n\
         # an entire column with the current seed.         #\n\
         #                                                 #\n\
         # You need to:                                    #\n\
         #   1. Add PlantCol(Col) to the Action type       #\n\
         #   2. Add a setCol helper function               #\n\
         #   3. Handle PlantCol in the update function     #\n\
         #                                                 #\n\
         # Look at how PlantRow is implemented for         #\n\
         # guidance - PlantCol is similar but vertical.    #\n\
         #                                                 #\n\
         # Tip: Use auto-probe to see how the grove        #\n\
         # changes after each action.                      #\n\n\
         type Plant = String in\n\
         type Grove = [[Plant]] in\n\
         type Row = Int in\n\
         type Col = Int in\n\n\
         type Model = (\n\
         grove = Grove,\n\
         currentSeed = Plant,\n\
         seedInventory = [Plant]\n\
         ) in\n\n\
         type Action =\n\
         + SelectSeed(Int)\n\
         + PlantSeed(Row, Col)\n\
         + Uproot(Row, Col)\n\
         + ClearGrove\n\
         + PlantRow(Row)\n\
         # TODO: Add PlantCol(Col) here #\n\
         in\n\n\
         let init: Model = (\n\
         grove = [\n\
         [\"\", \"\", \"\"],\n\
         [\"\", \"\", \"\"],\n\
         [\"\", \"\", \"\"]\n\
         ],\n\
         currentSeed = \"\240\159\140\177\",\n\
         seedInventory = [\"\240\159\140\177\", \"\240\159\140\191\", \
         \"\240\159\141\132\", \"\226\152\152\239\184\143\", \
         \"\240\159\140\184\"]\n\
         ) in\n\n\
         let setCell: (Grove, Row, Col, Plant) -> Grove =\n\
         fun grove, row, col, plant ->\n\
         mapi(grove, fun (i, r) ->\n\
         if i == row\n\
         then mapi(r, fun (j, c) -> if j == col then plant else c)\n\
         else r)\n\
         in\n\n\
         let setRow: (Grove, Row, Plant) -> Grove =\n\
         fun grove, targetRow, plant ->\n\
         mapi(grove, fun (i, row) ->\n\
         if i == targetRow\n\
         then map(row, fun _ -> plant)\n\
         else row)\n\
         in\n\n\
         # TODO: Add setCol helper here #\n\
         # Hint: You need to modify each row, changing #\n\
         # only the cell at the target column.         #\n\n\
         let setAll: (Grove, Plant) -> Grove =\n\
         fun (grove, plant) ->\n\
         map(grove, fun row -> map(row, fun _ -> plant))\n\
         in\n\n\
         let updateGrove: (Model, Grove -> Grove) -> Model =\n\
         fun (m, f) -> (f(m.grove), m.currentSeed, m.seedInventory)\n\
         in\n\n\
         let update: (Model, Action) -> Model =\n\
         fun m, action ->\n\
         case action\n\
         | SelectSeed(idx) =>\n\
         (m.grove, nth(m.seedInventory, idx), m.seedInventory)\n\
         | PlantSeed(row, col) =>\n\
         updateGrove(m, fun g -> setCell(g, row, col, m.currentSeed))\n\
         | Uproot(row, col) =>\n\
         updateGrove(m, fun g -> setCell(g, row, col, \"\"))\n\
         | ClearGrove =>\n\
         updateGrove(m, fun g -> setAll(g, \"\"))\n\
         | PlantRow(row) =>\n\
         updateGrove(m, fun g -> setRow(g, row, m.currentSeed))\n\
         # TODO: Add PlantCol case here #\n\
         end\n\
         in\n\n\
         let do: (Model, [Action]) -> Model =\n\
         fun (init: Model, actions: [Action]) ->\n\
         fold_left(actions, update, init)\n\
         in\n\n\
         # Existing tests #\n\
         test\n\
         let m = update(init, PlantRow(1)) in\n\
         m.grove == [[\"\", \"\", \"\"], [\"\240\159\140\177\", \
         \"\240\159\140\177\", \"\240\159\140\177\"], [\"\", \"\", \"\"]]\n\
         end;\n\n\
         # New tests for PlantCol #\n\
         test\n\
         let m = update(init, PlantCol(0)) in\n\
         m.grove == [[\"\240\159\140\177\", \"\", \"\"], \
         [\"\240\159\140\177\", \"\", \"\"], [\"\240\159\140\177\", \"\", \
         \"\"]]\n\
         end;\n\n\
         test\n\
         let m = update(init, PlantCol(2)) in\n\
         m.grove == [[\"\", \"\", \"\240\159\140\177\"], [\"\", \"\", \
         \"\240\159\140\177\"], [\"\", \"\", \"\240\159\140\177\"]]\n\
         end;\n\n\
         test\n\
         let m = do(init, [PlantRow(0), PlantCol(1)]) in\n\
         m.grove == [[\"\240\159\140\177\", \"\240\159\140\177\", \
         \"\240\159\140\177\"], [\"\", \"\240\159\140\177\", \"\"], [\"\", \
         \"\240\159\140\177\", \"\"]]\n\
         end;\n\n\
         test\n\
         let m = do(init, [SelectSeed(2), PlantCol(1)]) in\n\
         m.grove == [[\"\", \"\240\159\141\132\", \"\"], [\"\", \
         \"\240\159\141\132\", \"\"], [\"\", \"\240\159\141\132\", \"\"]]\n\
         && m.currentSeed == \"\240\159\141\132\"\n\
         end\n";
      refractors = "()";
    } )
