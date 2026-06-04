let out : string * Haz3lcore.PersistentSegment.t =
  ( "Study / writing / harvest-streak-extend / harvest-streak-extend-sketch",
    {
      segment =
        "((Secondary((id \
         a7442f4a-3685-4938-8a81-5b5de3baefe3)(content(Comment\"# HARVEST \
         STREAK EXTENSION TASK                   #\"))))(Secondary((id \
         ab510867-f05b-4dfc-881b-47099978dc7d)(content(Whitespace\"\\n\"))))(Secondary((id \
         c93f5495-b2d9-4bd4-9a76-aebac30b9842)(content(Comment\"#                                                  \
         #\"))))(Secondary((id \
         c8eeb58c-416f-4b08-89d1-6ce9c95186e2)(content(Whitespace\"\\n\"))))(Secondary((id \
         ff350a79-eb9c-4aa7-9c96-bb008962745c)(content(Comment\"# The harvest \
         ledger app tracks harvests and       #\"))))(Secondary((id \
         845a68eb-bc70-43a2-9000-7caa965dc80f)(content(Whitespace\"\\n\"))))(Secondary((id \
         f9b0e5a7-c9d6-4cd6-8eff-f78111b1bbba)(content(Comment\"# builds \
         streak bonuses for consecutive same-      #\"))))(Secondary((id \
         6640188d-2f89-4957-8cb2-ce018759136a)(content(Whitespace\"\\n\"))))(Secondary((id \
         7efb1881-aa44-4d37-b1d2-4ff7d8ca3f2a)(content(Comment\"# quality \
         harvests.                                #\"))))(Secondary((id \
         17e0466a-d914-4d1a-bdc7-222afaa37376)(content(Whitespace\"\\n\"))))(Secondary((id \
         d9d4e914-3e39-40a2-bd89-73284a70fa7a)(content(Comment\"#                                                  \
         #\"))))(Secondary((id \
         9a26f876-5783-4662-8e21-790ffd53316b)(content(Whitespace\"\\n\"))))(Secondary((id \
         56e242f7-654a-4a6b-a112-f5b173dade05)(content(Comment\"# YOUR TASK: \
         Add a PremiumSale action that lets    #\"))))(Secondary((id \
         7bdd4384-1251-474a-9658-5f58f7bcf123)(content(Whitespace\"\\n\"))))(Secondary((id \
         61d11852-79a2-4c00-a0ea-21e82743acdd)(content(Comment\"# the farmer \
         claim their streak bonus with a       #\"))))(Secondary((id \
         3fc3d1c8-5092-4db8-88cd-354f01400c00)(content(Whitespace\"\\n\"))))(Secondary((id \
         476c6047-1b44-4f53-b6f9-1151d4876619)(content(Comment\"# premium \
         multiplier when the streak is strong.    #\"))))(Secondary((id \
         e8bbf9c4-5aa6-4cdc-98d0-cacacd00a4b9)(content(Whitespace\"\\n\"))))(Secondary((id \
         1f5ccdcc-ba85-4c3e-84dd-f1e541a51d51)(content(Comment\"#                                                  \
         #\"))))(Secondary((id \
         2ba4d391-4f1b-4ab0-8bc6-46640e37c5e9)(content(Whitespace\"\\n\"))))(Secondary((id \
         8b285ffa-c045-4a09-b787-0faa0da2c999)(content(Comment\"# You need \
         to:                                     #\"))))(Secondary((id \
         41ac9a30-2dcd-4f10-8115-24f6fcefade1)(content(Whitespace\"\\n\"))))(Secondary((id \
         5ff599d2-01dd-4dae-83ad-c93bf21b3284)(content(Comment\"#   1. Add \
         PremiumSale to the Action type          #\"))))(Secondary((id \
         5fc96f63-ea09-4f09-8762-f5d0ffc1ab8e)(content(Whitespace\"\\n\"))))(Secondary((id \
         fddb62c5-679d-4283-9198-c2e7a8b0fb3d)(content(Comment\"#   2. Write a \
         premiumMultiplier helper function   #\"))))(Secondary((id \
         8a719045-34c1-4dd0-aef7-adde601f6b42)(content(Whitespace\"\\n\"))))(Secondary((id \
         506fd789-2dd1-4a51-97b2-0247515d55a2)(content(Comment\"#   3. Handle \
         PremiumSale in the update function   #\"))))(Secondary((id \
         4ec95e88-033e-4ba7-8f4f-c1f2939310d9)(content(Whitespace\"\\n\"))))(Secondary((id \
         954673d6-c8ea-486c-a7d8-d11334e49e03)(content(Comment\"#                                                  \
         #\"))))(Secondary((id \
         f048b6ff-93dd-44a0-b410-f8f15b90a623)(content(Whitespace\"\\n\"))))(Secondary((id \
         bd906994-cbd3-4b34-9f51-9035cb91369f)(content(Comment\"# Look at how \
         ClaimBonus is implemented for        #\"))))(Secondary((id \
         fb845f55-c290-4e9a-9339-57e125223a41)(content(Whitespace\"\\n\"))))(Secondary((id \
         fb52f10c-0f0e-4c48-bfb1-cf74b59d595a)(content(Comment\"# guidance - \
         PremiumSale is similar but applies    #\"))))(Secondary((id \
         44828411-2563-4065-9e9c-e8260a4d7fc1)(content(Whitespace\"\\n\"))))(Secondary((id \
         5b75a896-811f-4df0-8cef-de87264a5463)(content(Comment\"# a multiplier \
         to the payout.                      #\"))))(Secondary((id \
         2691ac01-4d0e-449d-b459-fa99829791d2)(content(Whitespace\"\\n\"))))(Secondary((id \
         270049f4-8db8-4f5c-abbc-842a715d129d)(content(Comment\"#                                                  \
         #\"))))(Secondary((id \
         d6ecf1d4-4135-4685-a8de-cc4c0a4aa2b0)(content(Whitespace\"\\n\"))))(Secondary((id \
         db4a6e6c-9fa1-46f1-b614-dcf867cd9270)(content(Comment\"# Tip: Use \
         auto-probe on premiumMultiplier to see  #\"))))(Secondary((id \
         d361e5f0-bd37-47b7-a8be-209cb3897889)(content(Whitespace\"\\n\"))))(Secondary((id \
         8ee19247-f95b-44f7-9f58-a72d320b213d)(content(Comment\"# when the \
         threshold fires.                        #\"))))(Secondary((id \
         4514bf13-19ad-4ff0-aa2e-1fdc4416b532)(content(Whitespace\"\\n\"))))(Secondary((id \
         0c18aa71-9e12-4f1d-97fe-731546cadb0b)(content(Whitespace\"\\n\"))))(Secondary((id \
         eebbd2d5-2bba-4655-b5a7-7d995cea812c)(content(Comment\"# Quality \
         tiers from the moonlit fields #\"))))(Secondary((id \
         4d61d6bf-818a-4e10-844e-9735211664cb)(content(Whitespace\"\\n\"))))(Tile((id \
         2f615d80-e906-4fce-b4fb-877f031e57cc)(label(type = in))(mold((out \
         Exp)(in_(TPat Typ))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         18cdbdd5-57f4-4a54-a697-a9600e5b49eb)(content(Whitespace\" \
         \"))))(Tile((id \
         fc3159c7-df21-4baa-8aff-756e62c147b5)(label(Quality))(mold((out \
         TPat)(in_())(nibs(((shape Convex)(sort TPat))((shape Convex)(sort \
         TPat))))))(shards(0))(children())))(Secondary((id \
         ab55610c-8904-4a8a-a2cf-2ee4cb43d2e6)(content(Whitespace\" \
         \")))))((Secondary((id \
         ed16dc3a-3c26-4ab9-8711-aa949eee9e11)(content(Whitespace\"\\n\"))))(Tile((id \
         a5f07a46-d740-4472-b082-416ea2286df9)(label(+))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape(Concave 33))(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         5a54a5a5-911d-4ee6-b01e-f365468f9480)(content(Whitespace\" \
         \"))))(Tile((id \
         0de1d9f9-01fb-4793-a071-b98e7f6cf293)(label(Bronze))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         dd0dc5de-a136-4282-a83f-060221f26cf1)(content(Whitespace\" \
         \"))))(Secondary((id \
         266c1a01-2075-47eb-acc1-388f50843c8a)(content(Whitespace\" \
         \"))))(Secondary((id \
         51adfb81-8ce5-4c53-a04b-e978f3a4b2d2)(content(Whitespace\" \
         \"))))(Secondary((id \
         6cb96f9e-78b6-44ac-8038-4170c06c0884)(content(Whitespace\" \
         \"))))(Secondary((id \
         3354bca3-c379-481f-863e-3343e34fb211)(content(Whitespace\" \
         \"))))(Secondary((id \
         64a6d11b-fe0e-4f07-871f-13c68fb9620d)(content(Whitespace\" \
         \"))))(Secondary((id \
         916b0b51-d770-41eb-a2e1-7202464575bb)(content(Comment\"# Common \
         harvest, basic value #\"))))(Secondary((id \
         b441f9a4-f88f-4049-a240-0282513a6ac6)(content(Whitespace\"\\n\"))))(Tile((id \
         a679050a-7a3a-434f-9513-55cebe28a357)(label(+))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 12))(sort Typ))((shape(Concave \
         12))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         5c3852b9-349b-4c5d-952f-3e1a288f593c)(content(Whitespace\" \
         \"))))(Tile((id \
         54c8b24e-fc2a-4814-b584-45a09c31a913)(label(Silver))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         4cad4ec0-48b9-47b8-89ae-ba72b9bb31c7)(content(Whitespace\" \
         \"))))(Secondary((id \
         f4dbe0e1-aa1f-40fd-9d0a-086d771573ca)(content(Whitespace\" \
         \"))))(Secondary((id \
         ffb42d29-3dfb-4b9a-be45-b03168802693)(content(Whitespace\" \
         \"))))(Secondary((id \
         a6da5e54-6d3b-40e9-9e03-0e08a6208c90)(content(Whitespace\" \
         \"))))(Secondary((id \
         90fa27c6-3159-47c5-a2bc-6e814d3c932a)(content(Whitespace\" \
         \"))))(Secondary((id \
         7a42f796-2e0d-4c3a-80ef-4439d0d309e6)(content(Whitespace\" \
         \"))))(Secondary((id \
         575b6a1c-a7d8-446e-a25a-721eca65c3af)(content(Comment\"# Good \
         quality, moderate bonus #\"))))(Secondary((id \
         270103aa-7f9b-408b-a36b-f6712621eeee)(content(Whitespace\"\\n\"))))(Tile((id \
         efe2d319-ebfc-4bf5-81f7-9d0bbf4be969)(label(+))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 12))(sort Typ))((shape(Concave \
         12))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         ab69da8f-20d0-4c28-939c-e4faf89503e7)(content(Whitespace\" \
         \"))))(Tile((id \
         ee1c0136-7706-4348-abee-5fe6b11fcf1f)(label(Gold))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         ccbf8d47-5bcc-45e5-a199-0578dfe2a9cd)(content(Whitespace\" \
         \"))))(Secondary((id \
         4d4ccd43-8f1f-41d1-abed-2616ead9025b)(content(Whitespace\" \
         \"))))(Secondary((id \
         2b400d32-4455-4d3c-a352-8f2f3bcbbfcb)(content(Whitespace\" \
         \"))))(Secondary((id \
         fc8f7ff0-320a-4390-ac0c-079d925bf151)(content(Whitespace\" \
         \"))))(Secondary((id \
         a62c9531-4c64-4681-a137-82aa2af51a40)(content(Whitespace\" \
         \"))))(Secondary((id \
         9e373f89-bd4d-40de-87b1-c341b2ddf131)(content(Whitespace\" \
         \"))))(Secondary((id \
         0ca26e66-6e60-45ce-ad80-8de808f38469)(content(Whitespace\" \
         \"))))(Secondary((id \
         21addaae-40ec-4fcb-a215-e91e0d922047)(content(Whitespace\" \
         \"))))(Secondary((id \
         7797bd52-1971-4d32-ba71-06b54c0bd823)(content(Comment\"# Excellent \
         harvest, high value #\"))))(Secondary((id \
         2c6a5b81-7ce3-4a13-aea8-75e06aa1482a)(content(Whitespace\"\\n\"))))(Tile((id \
         3a77aa97-eed6-4593-ad84-2f5208fa7ed7)(label(+))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 12))(sort Typ))((shape(Concave \
         12))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         16dd889e-51a4-48ca-8e9e-d64dd41f3e71)(content(Whitespace\" \
         \"))))(Tile((id \
         795ffd2b-777c-4bb7-8614-293312b8c0f3)(label(Starlight))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         56c70883-e410-4015-ac39-ef6b2d7bf577)(content(Whitespace\" \
         \"))))(Secondary((id \
         6ae36507-c648-4ba2-b2cb-0327dc5c3ff0)(content(Whitespace\" \
         \"))))(Secondary((id \
         3aea2ffb-e41c-4a45-b1ba-1829051fd4d0)(content(Whitespace\" \
         \"))))(Secondary((id \
         cb7de997-6165-4e44-b684-9ca13f7e81e2)(content(Comment\"# Legendary, \
         blessed by the moon #\"))))(Secondary((id \
         737fcc9f-1088-4e07-9c4e-5ce537e24bf1)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         64c32209-4416-48a1-967c-1b9b8923ebfb)(content(Whitespace\"\\n\"))))(Secondary((id \
         db1a960d-d85f-4253-a0df-9711794d6614)(content(Whitespace\"\\n\"))))(Secondary((id \
         17a60be8-6de9-4d78-9f96-ce9362b119b6)(content(Comment\"# Crops that \
         grow under the night sky #\"))))(Secondary((id \
         1b101ed3-5cda-46ba-9734-9271e4ca227e)(content(Whitespace\"\\n\"))))(Tile((id \
         a9824e2e-f09d-4a23-87f6-d544587ffafc)(label(type = in))(mold((out \
         Exp)(in_(TPat Typ))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         9651f365-6a02-426e-bd28-ffd74f24b55a)(content(Whitespace\" \
         \"))))(Tile((id \
         b82926e2-a66f-4af4-8fea-bc5423a10db3)(label(Crop))(mold((out \
         TPat)(in_())(nibs(((shape Convex)(sort TPat))((shape Convex)(sort \
         TPat))))))(shards(0))(children())))(Secondary((id \
         34f8c357-32e1-47e9-8a8a-20ddf77ce55e)(content(Whitespace\" \
         \")))))((Secondary((id \
         4acda8f5-860f-4871-8767-97c7bdab1583)(content(Whitespace\"\\n\"))))(Tile((id \
         812f543f-85fb-45a9-a734-e0eed13b70c8)(label(+))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape(Concave 33))(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         0866c1ef-77bd-4f4c-834f-2c04758ca6c0)(content(Whitespace\" \
         \"))))(Tile((id \
         2f07c274-2c58-482b-b4e7-21a4f0ef3dbc)(label(Moonmelon))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         1c406f1b-87bf-4a87-b8ca-f41808e86949)(content(Whitespace\" \
         \"))))(Secondary((id \
         7dee40bc-b975-4b0e-987d-6bd3e3b7689a)(content(Whitespace\" \
         \"))))(Secondary((id \
         db1651e3-56dc-4e66-9206-18bbfaa9199c)(content(Whitespace\" \
         \"))))(Secondary((id \
         9a691fb4-ab3b-4574-8193-ea2c4d9b3779)(content(Comment\"# Glows \
         faintly, sweet taste #\"))))(Secondary((id \
         713b4ec5-1caf-41fa-a99c-6c00f1ec2a88)(content(Whitespace\"\\n\"))))(Tile((id \
         e41544e5-1cbd-488c-b8b7-1390bbdb472b)(label(+))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 12))(sort Typ))((shape(Concave \
         12))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         246d96ff-9946-47c2-bae8-896610398bda)(content(Whitespace\" \
         \"))))(Tile((id \
         330775b8-f088-4dd2-b377-17927cd5c72c)(label(Starfruit))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         56e5951d-4c46-406a-aba8-00c2c6cbc0f1)(content(Whitespace\" \
         \"))))(Secondary((id \
         4a6af913-9228-4b0a-8c7e-89f977fa5352)(content(Whitespace\" \
         \"))))(Secondary((id \
         134bea10-35a9-4333-a50e-79983c59105e)(content(Whitespace\" \
         \"))))(Secondary((id \
         e8dd8a32-3656-45c4-8ec8-7165b8631799)(content(Comment\"# Shaped like \
         stars, tangy #\"))))(Secondary((id \
         cdc713c2-855d-4b32-986c-9bbf6fffe99c)(content(Whitespace\"\\n\"))))(Tile((id \
         3de9832b-f471-4cec-a205-4b0537f26728)(label(+))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 12))(sort Typ))((shape(Concave \
         12))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         5c4097fe-f3e8-4e23-9b98-096416b7f594)(content(Whitespace\" \
         \"))))(Tile((id \
         18db094c-0eda-43a5-93b0-ee06c3b588a6)(label(Nightshade))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         68682fc6-ccaf-4354-8d68-337394fe9af9)(content(Whitespace\" \
         \"))))(Secondary((id \
         b369fd3b-7f44-45ab-9dea-c5b9639342c0)(content(Whitespace\" \
         \"))))(Secondary((id \
         8b7834cb-a6e0-46cb-a78d-6829475cb763)(content(Comment\"# Purple \
         bloom, magical properties #\"))))(Secondary((id \
         fb58de4d-3ac8-4034-9481-586aedd9e71c)(content(Whitespace\"\\n\"))))(Tile((id \
         2f551467-8ec9-49ec-ac84-548f8067fdf3)(label(+))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 12))(sort Typ))((shape(Concave \
         12))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         4356b590-fd66-4d2b-be47-01b09983a635)(content(Whitespace\" \
         \"))))(Tile((id \
         f228c4a6-d17d-4a7f-b5e3-1325dc367800)(label(Duskwheat))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         bbd950df-f51f-43a4-8c58-4a08bfc5e217)(content(Whitespace\" \
         \"))))(Secondary((id \
         1515338a-7eb9-4a71-a917-67cb0d3ad47c)(content(Whitespace\" \
         \"))))(Secondary((id \
         5ed65a67-95b3-4d96-8f48-9381ef06d423)(content(Whitespace\" \
         \"))))(Secondary((id \
         9457c8e9-a864-4191-b600-89529d163950)(content(Comment\"# Golden \
         stalks, hearty grain #\"))))(Secondary((id \
         637a8ddf-92a6-45e0-884f-37d12f35bbcb)(content(Whitespace\"\\n\"))))(Tile((id \
         398dd329-6a33-42d8-8633-8e03436ea2fa)(label(+))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 12))(sort Typ))((shape(Concave \
         12))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         2bd1121e-ff1a-425d-b105-651654a08215)(content(Whitespace\" \
         \"))))(Tile((id \
         40a3e5eb-3226-4466-96ef-ea5c2395a326)(label(Glowpumpkin))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         b9b6a76d-bad9-49c2-a95a-bedafcd3cff8)(content(Whitespace\" \
         \"))))(Secondary((id \
         762a96e4-32db-410f-a62d-d076220c985b)(content(Comment\"# Orange and \
         luminescent #\"))))(Secondary((id \
         321fc4f4-2da9-4f6e-ae3d-46db6c06025c)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         23095c2b-55d4-4219-af30-95f34c1a98a8)(content(Whitespace\"\\n\"))))(Secondary((id \
         b1501673-1155-46cb-97c4-f89f3aad6179)(content(Whitespace\"\\n\"))))(Secondary((id \
         66ce1f21-fa4a-450b-a6a6-513ab049b837)(content(Comment\"# A single \
         harvest from the garden #\"))))(Secondary((id \
         e5d14c38-5567-4735-8d7b-6933fa85d124)(content(Whitespace\"\\n\"))))(Tile((id \
         fdb49830-0c79-42be-910b-ff18c397ef14)(label(type = in))(mold((out \
         Exp)(in_(TPat Typ))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         0cea42e7-246e-48d6-95c7-a80e95a74dee)(content(Whitespace\" \
         \"))))(Tile((id \
         622f04b7-282a-45eb-8755-7920c07c1ac1)(label(Harvest))(mold((out \
         TPat)(in_())(nibs(((shape Convex)(sort TPat))((shape Convex)(sort \
         TPat))))))(shards(0))(children())))(Secondary((id \
         9190c32f-1938-4a3b-a608-c81b5a56f7a4)(content(Whitespace\" \
         \")))))((Secondary((id \
         f8b14ffa-cc52-4391-bf0c-db34b2616a3d)(content(Whitespace\" \
         \"))))(Tile((id \
         3d751aa0-9da9-45e4-80f5-001eab99a29b)(label(\"(\"\")\"))(mold((out \
         Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0 1))(children(((Secondary((id \
         e168f651-88fe-462c-b622-8e07e0e027a8)(content(Whitespace\"\\n\"))))(Tile((id \
         f6b46a63-77ed-439a-a005-faf7473f13e9)(label(crop))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         6919d1ac-e588-4fcf-9425-9b595ada4128)(content(Whitespace\" \
         \"))))(Tile((id \
         32679a19-aa50-42bf-aae9-7a206c09a5fc)(label(=))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 39))(sort Typ))((shape(Concave \
         39))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         6cb88c5a-792d-4364-ae46-4e0fbb97a998)(content(Whitespace\" \
         \"))))(Tile((id \
         7c6bc71d-455d-41ac-9d44-51336c2ff41f)(label(Crop))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         f1252762-132b-4924-a4f4-926c380692cc)(label(,))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 44))(sort Typ))((shape(Concave \
         44))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         e4b724e4-9769-45e6-8172-64b4548eaaf9)(content(Whitespace\"\\n\"))))(Tile((id \
         809f4faa-2d68-4797-8e35-db2367b6aae9)(label(quality))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         3f201f60-7316-44d0-a82f-d2b1b1b555c5)(content(Whitespace\" \
         \"))))(Tile((id \
         21ed799b-ac09-42cb-939d-e53df9e70b91)(label(=))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 39))(sort Typ))((shape(Concave \
         39))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         ce112f25-870d-4ebb-a368-86f0bd883ccf)(content(Whitespace\" \
         \"))))(Tile((id \
         6a436e1f-5c6e-4ab7-aad0-aa9ad9efe39f)(label(Quality))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         9ec2e81b-7b85-4e2a-9738-6f6aa547246b)(label(,))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 44))(sort Typ))((shape(Concave \
         44))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         b1e022cf-5f28-4ddf-bcee-032ca724c262)(content(Whitespace\"\\n\"))))(Tile((id \
         6c0f16f6-547d-4b9e-8679-f54b0c6e4915)(label(quantity))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         6d307fdc-12c0-4c45-975c-0a45ec2aa510)(content(Whitespace\" \
         \"))))(Tile((id \
         c5e46d5c-732a-4ffe-b879-92500ebfddbb)(label(=))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 39))(sort Typ))((shape(Concave \
         39))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         0bf84aa6-593b-4406-a2dd-ed87e71dca38)(content(Whitespace\" \
         \"))))(Tile((id \
         b7f4d235-de69-47ec-a3fe-4bb7b353b25b)(label(Int))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         dc657e99-05bb-4195-a89f-5dbc6a994c73)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         b960fbc7-3221-44f8-ac5a-733e7627c421)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         abb361d9-8714-49fc-baf2-60d3591d7cb1)(content(Whitespace\"\\n\"))))(Secondary((id \
         093ffdf0-aa57-40b0-9e1f-8f9e12cbdd5a)(content(Whitespace\"\\n\"))))(Secondary((id \
         b77aa59f-a4a1-435b-b570-5194d913b79f)(content(Comment\"# The harvest \
         ledger tracks all harvests and bonuses #\"))))(Secondary((id \
         f99065a2-df25-4f47-9f79-bc07d5edc5e9)(content(Whitespace\"\\n\"))))(Tile((id \
         3e617ad5-9b56-4185-9f73-f55f61459d6c)(label(type = in))(mold((out \
         Exp)(in_(TPat Typ))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         d55d1f63-2a9f-4a41-bf4a-51247c8fc3ab)(content(Whitespace\" \
         \"))))(Tile((id \
         ba41e9d0-9fa2-4a18-a3a6-dfa63a5ce6b0)(label(Model))(mold((out \
         TPat)(in_())(nibs(((shape Convex)(sort TPat))((shape Convex)(sort \
         TPat))))))(shards(0))(children())))(Secondary((id \
         48f8c954-1a5e-48d8-a303-d385cfedc3b3)(content(Whitespace\" \
         \")))))((Secondary((id \
         89ca02d7-ff7a-41bf-9ac4-dd1f3e7aa4be)(content(Whitespace\" \
         \"))))(Tile((id \
         ac117f18-ba1a-4651-b0f2-f90c2e6dd172)(label(\"(\"\")\"))(mold((out \
         Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0 1))(children(((Secondary((id \
         bd9764d0-b82f-49d9-baba-6b93d63b8941)(content(Whitespace\"\\n\"))))(Tile((id \
         f9583c52-f00e-4cae-918c-8c5d0026046d)(label(harvests))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         e57e0b86-94b5-4d92-acbb-23ab05809418)(content(Whitespace\" \
         \"))))(Tile((id \
         6ff5b408-61b4-4fda-9432-609e372eb2ff)(label(=))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 39))(sort Typ))((shape(Concave \
         39))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         1f844e17-c181-49d7-8779-29d778bce0ed)(content(Whitespace\" \
         \"))))(Tile((id c735af85-0eec-4142-913b-72ec4a757720)(label([ \
         ]))(mold((out Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape \
         Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
         f3baa424-a0cd-4171-8e96-a6e4f8a73009)(label(Harvest))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children()))))))))(Tile((id \
         7c1ab5fb-3f62-408e-809c-a5c2b823be35)(label(,))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 44))(sort Typ))((shape(Concave \
         44))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         6771f459-d288-45c9-8f3e-c70aba88a956)(content(Whitespace\"\\n\"))))(Tile((id \
         e545cb4d-fde4-4d61-82da-ddaeadb4608a)(label(totalValue))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         87ebc1cc-839d-483d-9e75-e5c6d5a823ad)(content(Whitespace\" \
         \"))))(Tile((id \
         9a992b3b-30a5-42a5-bcc0-2daf6356545b)(label(=))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 39))(sort Typ))((shape(Concave \
         39))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         a7cc6450-5d36-4b72-b7a1-02adf333de78)(content(Whitespace\" \
         \"))))(Tile((id \
         b94f5717-c6f8-4b87-942d-945bf33b122c)(label(Int))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         6d048a6b-ca8b-4043-96fc-544e4b237f3c)(label(,))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 44))(sort Typ))((shape(Concave \
         44))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         e31d7812-f810-4961-b0b8-5e6098e55171)(content(Whitespace\"\\n\"))))(Tile((id \
         1a017ab5-7491-40f5-8279-ee5e92d2e7bb)(label(streakBonus))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         fd50a901-a642-46bb-897e-716cb20d734c)(content(Whitespace\" \
         \"))))(Tile((id \
         27fa624e-ca08-4590-811c-d076730ab938)(label(=))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 39))(sort Typ))((shape(Concave \
         39))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         c9b56d80-256a-4080-a78e-650c880bee7a)(content(Whitespace\" \
         \"))))(Tile((id \
         aba41824-98c0-4a78-a131-445bcf0b2917)(label(Int))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         48a8a309-5ddf-42dd-b78f-5ba77495d7db)(label(,))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 44))(sort Typ))((shape(Concave \
         44))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         cc8d7adc-3e0d-46f4-974e-2fe9caba8dca)(content(Whitespace\"\\n\"))))(Tile((id \
         aa00bc0f-c892-4843-9cd3-e780b3dfdb2e)(label(lastQuality))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         cb9bb53a-748d-4380-8680-cfeaddbb105a)(content(Whitespace\" \
         \"))))(Tile((id \
         8488b5f1-f628-4906-9e22-cebc56dff07b)(label(=))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 39))(sort Typ))((shape(Concave \
         39))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         7c0d5c9a-0139-46c5-bacd-16fc1634d34d)(content(Whitespace\" \
         \"))))(Tile((id \
         0466f2cc-3bda-43e6-b128-9d59e9a303ce)(label(Quality))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         0544f178-7585-40dd-91d6-d7ccb4d90b3e)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         18585e7e-0690-45c9-8281-b3482de2c233)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         c86fdc04-e639-4406-b89a-3ca8102c6a1e)(content(Whitespace\"\\n\"))))(Secondary((id \
         21073a87-af05-4481-a529-5865102b85f2)(content(Whitespace\"\\n\"))))(Secondary((id \
         f8909a49-0991-4470-a2dc-8feeaf181a4c)(content(Comment\"# Actions the \
         farmer can take #\"))))(Secondary((id \
         ff30d3f6-f24e-4777-bcb3-02d00c74b59a)(content(Whitespace\"\\n\"))))(Tile((id \
         3d6bb821-40bf-4bb5-a1a3-d60468ef58c8)(label(type = in))(mold((out \
         Exp)(in_(TPat Typ))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         a5de57a3-ede7-4afa-a902-37c473992d10)(content(Whitespace\" \
         \"))))(Tile((id \
         7081af0d-1d2b-41f2-bdd2-305904405519)(label(Action))(mold((out \
         TPat)(in_())(nibs(((shape Convex)(sort TPat))((shape Convex)(sort \
         TPat))))))(shards(0))(children())))(Secondary((id \
         3181d267-ab9b-4b40-91b3-828032b2874a)(content(Whitespace\" \
         \")))))((Secondary((id \
         8046c3ea-9f9b-4b78-aa40-c6bd77a01451)(content(Whitespace\"\\n\"))))(Tile((id \
         524c25f5-c27b-4d3f-9b8e-02107a471dc8)(label(+))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape(Concave 33))(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         16ce4f68-0c12-4b9a-af2e-a54e5807c468)(content(Whitespace\" \
         \"))))(Tile((id \
         8eb9a7d6-59fb-49c2-a5d2-d57e6778bf61)(label(RecordHarvest))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         14f040ff-dc59-47c2-9f17-ab8278233030)(label(\"(\"\")\"))(mold((out \
         Typ)(in_(Typ))(nibs(((shape(Concave 11))(sort Typ))((shape \
         Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
         db516a16-d707-477f-a9de-3c0826b1f77d)(label(Harvest))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children()))))))))(Secondary((id \
         2cf2c83b-8341-4f0b-84c1-2d2411a87d77)(content(Whitespace\" \
         \"))))(Secondary((id \
         a40d6006-734e-469d-8f82-554de8b1bc52)(content(Whitespace\" \
         \"))))(Secondary((id \
         44f231f3-f009-4560-85a4-92d66680b7cc)(content(Comment\"# Log a new \
         harvest #\"))))(Secondary((id \
         e74b93b3-0af7-49c6-9ed5-98e7f26d7cb6)(content(Whitespace\"\\n\"))))(Tile((id \
         03ecb4fc-55f5-47c5-b9fd-6b9f05c55f9e)(label(+))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 12))(sort Typ))((shape(Concave \
         12))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         4b5b80cd-5048-4345-9044-7b93d1fffc10)(content(Whitespace\" \
         \"))))(Tile((id \
         4b1da054-1011-4e8a-b238-c3f7e42ce567)(label(ClaimBonus))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         13568209-e7d1-420a-8a41-dec160a69ee8)(content(Whitespace\" \
         \"))))(Secondary((id \
         77eee2e3-13e8-4efc-a163-da849d24b3e6)(content(Whitespace\" \
         \"))))(Secondary((id \
         74cb1553-00f1-4ec7-b166-c6394a8d2186)(content(Whitespace\" \
         \"))))(Secondary((id \
         5ba30e18-0566-4812-8334-6b6e12524b9a)(content(Whitespace\" \
         \"))))(Secondary((id \
         64e74237-8dec-4fb6-b39c-d96106cf330f)(content(Whitespace\" \
         \"))))(Secondary((id \
         84e23317-d272-485a-9e02-b303eabe368a)(content(Whitespace\" \
         \"))))(Secondary((id \
         e5d95b23-4a9b-4081-9614-387e26354e6b)(content(Whitespace\" \
         \"))))(Secondary((id \
         9a508355-68dd-4e4e-bd6e-267cfb5474fc)(content(Whitespace\" \
         \"))))(Secondary((id \
         0457d76c-706c-471e-ad8c-acf5761931d5)(content(Whitespace\" \
         \"))))(Secondary((id \
         1ff628a0-4d04-4fbf-8682-ad419ea15de4)(content(Whitespace\" \
         \"))))(Secondary((id \
         dcd31a02-41bd-4a1f-b10c-49cefcc71b0d)(content(Whitespace\" \
         \"))))(Secondary((id \
         5a8e3f4c-07ee-4963-890e-64d1cd2f976d)(content(Whitespace\" \
         \"))))(Secondary((id \
         84e78d12-75fd-4f7f-a268-f678b3774109)(content(Whitespace\" \
         \"))))(Secondary((id \
         46a46cde-17e4-47d1-bf3e-b837c9158da0)(content(Whitespace\" \
         \"))))(Secondary((id \
         4284459a-4116-489d-9070-38e4337dfa36)(content(Comment\"# Collect \
         accumulated streak bonus #\"))))(Secondary((id \
         04a79a75-14d8-4716-b172-7cd097aecd08)(content(Whitespace\"\\n\"))))(Tile((id \
         b43af4a9-1dcb-4952-af67-f198e4155990)(label(+))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 12))(sort Typ))((shape(Concave \
         12))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         80d184b6-2877-4055-9c52-1249eb973389)(content(Whitespace\" \
         \"))))(Tile((id \
         8a765b3b-b9a8-4776-84f3-241e7c8568e2)(label(CloseDay))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         cc5d14a6-bba4-458d-95f7-9a0b5c8d3fa0)(content(Whitespace\" \
         \"))))(Secondary((id \
         bbc50a28-bcb4-4723-b2d0-085dc05fe6ff)(content(Whitespace\" \
         \"))))(Secondary((id \
         2b83b5b7-b803-424f-b102-c407ffb07355)(content(Whitespace\" \
         \"))))(Secondary((id \
         72748292-42e3-4bdc-91af-96a27aa97bc8)(content(Whitespace\" \
         \"))))(Secondary((id \
         b61d5ec7-aead-469a-af46-e68882c30481)(content(Whitespace\" \
         \"))))(Secondary((id \
         6a1a71ce-7161-4435-9a01-d67687184275)(content(Whitespace\" \
         \"))))(Secondary((id \
         36dc4bcb-138c-44d5-b897-df4aa7280673)(content(Whitespace\" \
         \"))))(Secondary((id \
         47a645d9-65ba-45d0-a103-9f169be00f02)(content(Whitespace\" \
         \"))))(Secondary((id \
         5be57fcd-2e84-4e60-a22d-ffd29d778c8b)(content(Whitespace\" \
         \"))))(Secondary((id \
         e946c574-7596-47e3-84bf-577e0125ff23)(content(Whitespace\" \
         \"))))(Secondary((id \
         235befe5-bef4-4ea6-a5ef-d9bab2e90724)(content(Whitespace\" \
         \"))))(Secondary((id \
         c00a0b6a-854b-41d8-be4e-1b659a79a19a)(content(Whitespace\" \
         \"))))(Secondary((id \
         a40ee25a-9f6f-4fb6-a387-33943fcdc669)(content(Whitespace\" \
         \"))))(Secondary((id \
         fe8cf0e7-c74a-4308-8706-2c8b6f5dcae2)(content(Whitespace\" \
         \"))))(Secondary((id \
         48e273dc-74f2-424e-9942-1a9707e47bbb)(content(Whitespace\" \
         \"))))(Secondary((id \
         f95a779d-c9c1-4cf8-a0db-aed068c48f92)(content(Whitespace\" \
         \"))))(Secondary((id \
         87bdda28-6e45-4af7-aaae-394e944ee532)(content(Comment\"# End the \
         harvest day, reset streaks #\"))))(Secondary((id \
         a3619d29-b7e2-4fcc-b2e7-5a7d3cb4b336)(content(Whitespace\"\\n\"))))(Secondary((id \
         c034d4c7-8a9f-49af-aeba-778a743477d7)(content(Comment\"# TODO: Add \
         PremiumSale here #\"))))(Secondary((id \
         e0cdb7a0-04c0-44e3-be15-555e21816ca4)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         53083084-467d-4cfc-913c-52d0b987df38)(content(Whitespace\"\\n\"))))(Secondary((id \
         bbd34f16-7ad3-42cb-8e45-251b00dccc07)(content(Whitespace\"\\n\"))))(Secondary((id \
         a3f05773-a70f-46df-8bcc-13e3158dfb31)(content(Comment\"# Calculate \
         base value of a crop #\"))))(Secondary((id \
         7cd557f7-e59c-48e7-a07f-2ca857141f49)(content(Whitespace\"\\n\"))))(Tile((id \
         ee46853b-7945-4660-a2f8-764b826cd2e1)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         f7b475df-c26f-4989-8b74-a8fb444021a4)(content(Whitespace\" \
         \"))))(Tile((id \
         c9677032-06f0-40b3-90dc-10555e729886)(label(cropValue))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         c787c680-5dec-4726-96a3-7999be16dfb2)(content(Whitespace\" \
         \"))))(Tile((id \
         125993fe-9e0e-4f97-984e-810d64d88e96)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 24))(sort Pat))((shape(Concave \
         24))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         fd39f5ca-4484-4127-a6f1-084eecef3201)(content(Whitespace\" \
         \"))))(Tile((id \
         7fec7cc5-537d-4b79-a355-b4e738ecfcab)(label(Crop))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         bd050583-4d0f-4b88-9d10-cda5ffb122f8)(content(Whitespace\" \
         \"))))(Tile((id \
         b1a91a21-3c10-4740-a208-d89be6c4c013)(label(->))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 13))(sort Typ))((shape(Concave \
         13))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         c70b4049-25a0-4122-9625-e8ab1a22d8ef)(content(Whitespace\" \
         \"))))(Tile((id \
         97b9042f-4db9-426b-ab7f-1a309bad4fd2)(label(Int))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         a28ea153-a63f-4347-9ec9-804c42ecc6ee)(content(Whitespace\" \
         \")))))((Secondary((id \
         0bfadaff-3e34-43e7-8514-da7781ca0d7d)(content(Whitespace\"\\n\"))))(Tile((id \
         9b4cf052-fc24-44c4-82d0-13af8152c5d6)(label(fun ->))(mold((out \
         Exp)(in_(Pat))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         37))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         2d46d4f8-c7a5-4182-a7d6-1554a5369579)(content(Whitespace\" \
         \"))))(Tile((id \
         da2fa9bc-0850-421e-a180-69f420336b57)(label(c))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         13716ae7-9712-448b-8e42-86555bef9196)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         86e4bcca-d1a7-4ec5-bf40-54041fb23ba5)(content(Whitespace\"\\n\"))))(Tile((id \
         fa4dc1fe-e609-4597-86a5-c981bb63a91e)(label(case end))(mold((out \
         Exp)(in_(Rul))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         3ccee141-65d0-4418-9d16-0206544b920e)(content(Whitespace\" \
         \"))))(Tile((id \
         2ac76417-376e-4ebb-9890-d0acfd555e15)(label(c))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         d4c7eb76-1a97-47af-9ebb-8415b8e6d06b)(content(Whitespace\"\\n\"))))(Tile((id \
         bcbd0111-4004-4df7-b06a-bf1d25638396)(label(| =>))(mold((out \
         Rul)(in_(Pat))(nibs(((shape(Concave 46))(sort Exp))((shape(Concave \
         46))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         c9e2024b-3c8b-468b-bc18-10fc00dd6427)(content(Whitespace\" \
         \"))))(Tile((id \
         2b8d1fd5-180b-4e1c-9261-ce9ec7055495)(label(Moonmelon))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         3d45913a-2b28-428e-a224-fd95eba7beca)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         ab72fd65-2a98-4ac5-b83c-a59210febe9e)(content(Whitespace\" \
         \"))))(Tile((id \
         cd3eb2b5-1ae8-4b0f-b0d7-ab6719a11895)(label(15))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         97001f54-d966-435c-a3d6-629fcd4aeb00)(content(Whitespace\"\\n\"))))(Tile((id \
         50adae07-3ab8-4d14-8c8e-3e45b264141d)(label(| =>))(mold((out \
         Rul)(in_(Pat))(nibs(((shape(Concave 46))(sort Exp))((shape(Concave \
         46))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         a286e2bd-7e11-4112-bef5-fc8931ccbbf6)(content(Whitespace\" \
         \"))))(Tile((id \
         e84cc276-9e22-4875-b299-47ade0113463)(label(Starfruit))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         81ae47ce-163a-404f-8985-e10274141a4c)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         7bed9474-c923-4c8c-bb29-6793351cc7e6)(content(Whitespace\" \
         \"))))(Tile((id \
         69a33308-6e39-4b12-998c-3e8c578af841)(label(20))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         219032c0-98bf-40f1-aaff-76ee8bc86eea)(content(Whitespace\"\\n\"))))(Tile((id \
         378ff770-f39f-43ed-94d3-ccf4ab1d8854)(label(| =>))(mold((out \
         Rul)(in_(Pat))(nibs(((shape(Concave 46))(sort Exp))((shape(Concave \
         46))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         fcf1187a-eafa-4df0-97c1-5998d446518f)(content(Whitespace\" \
         \"))))(Tile((id \
         c946a94f-fd8c-4ea7-9424-1d112bdfb4c5)(label(Nightshade))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         2dedb618-4462-49cd-a8f6-3b7395456639)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         356a73ec-3289-40a2-ab25-c20f3894906a)(content(Whitespace\" \
         \"))))(Tile((id \
         77288a20-ac91-4fae-82c6-d3e824b4f477)(label(20))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         10889d6b-2ad6-4299-9ae6-a1f8937b5de9)(content(Whitespace\"\\n\"))))(Tile((id \
         f5ef07fc-a696-4262-b715-c269835f1f04)(label(| =>))(mold((out \
         Rul)(in_(Pat))(nibs(((shape(Concave 46))(sort Exp))((shape(Concave \
         46))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         5304a5c6-c6a5-4de7-ac91-3b03f9b8c01e)(content(Whitespace\" \
         \"))))(Tile((id \
         a9519078-7295-45f7-b2e0-1a718929cf16)(label(Duskwheat))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         4a6e6d3b-eb3b-4502-a788-8da45b22d4c5)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         6481afb3-337c-4113-bbb6-4466a2012339)(content(Whitespace\" \
         \"))))(Tile((id \
         452364a1-bd1f-42b9-b7fe-e10747c62757)(label(10))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         9f6fca1e-7530-42a5-b337-2db26bb13a69)(content(Whitespace\"\\n\"))))(Tile((id \
         91dfb779-d200-40d0-9384-d14317f6a0fe)(label(| =>))(mold((out \
         Rul)(in_(Pat))(nibs(((shape(Concave 46))(sort Exp))((shape(Concave \
         46))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         cc95dae5-78e4-424a-8eda-a8a1f3f59757)(content(Whitespace\" \
         \"))))(Tile((id \
         a46f45ae-4bba-4664-ab2d-c4bc5a6ddc98)(label(Glowpumpkin))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         5a5e8d8c-a178-46ab-81af-48fda6a29fc8)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         fc22be5a-9a18-4615-b3dd-e8a3230a7a84)(content(Whitespace\" \
         \"))))(Tile((id \
         572d9494-eaae-4b07-977a-2c4af39385cb)(label(12))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         363ad671-6312-416f-88aa-031956eb7dab)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         5359d82b-11ad-43d4-9652-c887313b82d9)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         4af1a662-66c5-40d8-99dd-eac59039969f)(content(Whitespace\"\\n\"))))(Secondary((id \
         d90c85e2-0a20-4ba9-9b8e-b3ad1a8816d8)(content(Whitespace\"\\n\"))))(Secondary((id \
         83f8ba8b-6b41-4d7b-9ceb-0357fe6379ba)(content(Comment\"# Quality \
         multiplier for harvest value #\"))))(Secondary((id \
         2616f5d9-fe98-4a96-a8f3-580e84adb9fa)(content(Whitespace\"\\n\"))))(Tile((id \
         d156164f-3c2d-4493-a0d7-05792868fe15)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         cc6f58be-fce5-42e8-9bf2-d2bfdad40f7e)(content(Whitespace\" \
         \"))))(Tile((id \
         0afdbd28-7889-467a-8b75-3108ebb8d1d8)(label(qualityMultiplier))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         742d7ee2-771d-4197-a52e-6a56d3e98a75)(content(Whitespace\" \
         \"))))(Tile((id \
         cc2415be-aea5-4054-a533-087488cae2e3)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 24))(sort Pat))((shape(Concave \
         24))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         2c6ecdd2-4a84-4385-83fe-6738d6431871)(content(Whitespace\" \
         \"))))(Tile((id \
         342bdda7-db08-46c5-a38b-5a2ba5d2192e)(label(Quality))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         33b39597-5291-4900-971b-94090e52b148)(content(Whitespace\" \
         \"))))(Tile((id \
         d331bc2c-694f-4a87-a01e-01934f4a01be)(label(->))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 13))(sort Typ))((shape(Concave \
         13))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         a0627a2d-9d55-450e-9b8e-192ee4a209f6)(content(Whitespace\" \
         \"))))(Tile((id \
         57239070-899a-418e-95b2-facd06124ba2)(label(Int))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         dfc12ce4-dbd9-4d0b-9bbe-9eba2385e7e3)(content(Whitespace\" \
         \")))))((Secondary((id \
         5d34f384-b431-49bb-a78f-1f7072571a24)(content(Whitespace\"\\n\"))))(Tile((id \
         e7a83b33-e2c8-42a7-a522-718bd2776d05)(label(fun ->))(mold((out \
         Exp)(in_(Pat))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         37))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         834aeda3-b0c9-4a39-8160-ea7f19c9f477)(content(Whitespace\" \
         \"))))(Tile((id \
         2f159aa7-0af5-4118-87f4-c809d782465d)(label(q))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         1165249c-b39e-4a1b-8d66-4a58540f5227)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         5514d675-db97-47d0-86b6-f4854bb70774)(content(Whitespace\"\\n\"))))(Tile((id \
         e298bdb6-cf9c-45f0-b6c3-69eb8ed5c52c)(label(case end))(mold((out \
         Exp)(in_(Rul))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         303cd426-3784-4af7-a420-5340adaceff7)(content(Whitespace\" \
         \"))))(Tile((id \
         fd758f3a-0581-4f43-a7b2-6a458bd30e09)(label(q))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         4c0e96d1-1950-47d8-95af-ba727e3fcfaa)(content(Whitespace\"\\n\"))))(Tile((id \
         d161e428-bba4-4e87-a017-9a3622feb03c)(label(| =>))(mold((out \
         Rul)(in_(Pat))(nibs(((shape(Concave 46))(sort Exp))((shape(Concave \
         46))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         a8f6bc7e-fd7b-445a-821d-52c42ea9756f)(content(Whitespace\" \
         \"))))(Tile((id \
         871af404-f94b-4d60-bfa5-f74326a198d2)(label(Bronze))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         0dadb425-5c29-449b-a53a-9b58ca1fb09c)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         ac04b1de-0eef-404e-87fb-f996854ba633)(content(Whitespace\" \
         \"))))(Tile((id \
         169578a1-b7d3-409b-87be-4932cf0e9fd0)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         0acf5ec9-cf86-49a9-a058-af9d250463b3)(content(Whitespace\"\\n\"))))(Tile((id \
         c97b3d39-23c4-435b-a7b3-08af3830ba77)(label(| =>))(mold((out \
         Rul)(in_(Pat))(nibs(((shape(Concave 46))(sort Exp))((shape(Concave \
         46))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         d6f7f027-eb6f-4737-99a9-8bc88d8a28e2)(content(Whitespace\" \
         \"))))(Tile((id \
         4e640428-8fa9-4b67-b31b-981cf440f387)(label(Silver))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         3ceb0a40-06a7-4264-99c5-4220a4cd6a61)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         47172491-2083-47c5-bdf6-fa5125e161db)(content(Whitespace\" \
         \"))))(Tile((id \
         9cec0745-fcd6-4627-960f-50a52f4783cc)(label(2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         8c320880-f9a8-43f2-b3fa-d552bf321619)(content(Whitespace\"\\n\"))))(Tile((id \
         b7baca32-ee3a-495f-a6ce-a891279b9c39)(label(| =>))(mold((out \
         Rul)(in_(Pat))(nibs(((shape(Concave 46))(sort Exp))((shape(Concave \
         46))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         29d8dc62-1b99-4812-b954-a428cce82db1)(content(Whitespace\" \
         \"))))(Tile((id \
         df0bdb53-4acd-4e8a-98cd-04b6b28e6205)(label(Gold))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         af025c99-433d-4944-af40-ba24e154d8a9)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         d225eb54-901b-486a-a404-79d75bb91aa3)(content(Whitespace\" \
         \"))))(Tile((id \
         83a0b5d3-5b85-4413-a9be-193a13fa28f4)(label(3))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         683c728b-d478-4487-a643-9b0ebcfe991b)(content(Whitespace\"\\n\"))))(Tile((id \
         b05228e4-9fd1-4aa9-95bd-f3a36460f1fd)(label(| =>))(mold((out \
         Rul)(in_(Pat))(nibs(((shape(Concave 46))(sort Exp))((shape(Concave \
         46))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         debb2c23-a4ac-416b-b8df-ced946ed4a5d)(content(Whitespace\" \
         \"))))(Tile((id \
         b8a0ac4c-3deb-4ee6-9e8b-77776f2562cb)(label(Starlight))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         e948faca-db1d-40a7-827c-78700d1f8fcc)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         322923d1-0baa-4e64-a290-9566ea8dfd49)(content(Whitespace\" \
         \"))))(Tile((id \
         b5d45251-b08d-4d1b-9c03-9d7676045ca7)(label(5))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         d6e931b3-9a3b-4f15-b745-9b0312abbca6)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         f3ce1c56-bd91-4917-82cb-fb867ac8b40d)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         03efa383-f70e-4961-b338-6892891d37a7)(content(Whitespace\"\\n\"))))(Secondary((id \
         68499eb2-665f-463c-852b-cd930941a263)(content(Whitespace\"\\n\"))))(Secondary((id \
         e115d83d-f23e-45bf-96d7-ce16e8e94779)(content(Comment\"# Calculate \
         the value of a single harvest #\"))))(Secondary((id \
         bafa4160-15cc-4b85-8b93-82f66e813b1f)(content(Whitespace\"\\n\"))))(Tile((id \
         bbe48fc0-e701-4494-b5e8-fc001c88e8d9)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         cdbf1048-3618-4c94-b2b4-ce3c7aaa59c9)(content(Whitespace\" \
         \"))))(Tile((id \
         8f756586-b9ee-4c04-9da4-8379ef1f5bf6)(label(harvestValue))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         4775d4f6-6a2e-4686-a63a-1dc3d7a43f1b)(content(Whitespace\" \
         \"))))(Tile((id \
         84e4d03e-95cc-481d-a307-87b0f760cedd)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 24))(sort Pat))((shape(Concave \
         24))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         e5bbf2cd-4c78-411a-9807-806e5e04aaed)(content(Whitespace\" \
         \"))))(Tile((id \
         daa7cdac-5a62-43b4-aac5-293c6833be26)(label(Harvest))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         7fcd73f1-bcf5-4e80-8ab5-b87c31c0b164)(content(Whitespace\" \
         \"))))(Tile((id \
         ee65b16e-93aa-4b02-8e58-db05140bc134)(label(->))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 13))(sort Typ))((shape(Concave \
         13))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         9bcbb07a-bee6-4f2c-911e-63576933e372)(content(Whitespace\" \
         \"))))(Tile((id \
         d4dd8ee9-908e-4187-b63f-b06c5945b18b)(label(Int))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         1921b9af-77ec-405a-8dce-459e6fec098a)(content(Whitespace\" \
         \")))))((Secondary((id \
         e9ce8580-aa64-4e6a-89bc-eaec516a3c84)(content(Whitespace\"\\n\"))))(Tile((id \
         06074871-0748-4d45-85dc-9a8b7d3b82fc)(label(fun ->))(mold((out \
         Exp)(in_(Pat))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         37))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         2200cf32-9654-40ea-9143-fb0b32d954dc)(content(Whitespace\" \
         \"))))(Tile((id \
         f3d8e469-25a0-4721-ad18-c938ca97773c)(label(h))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         e36ed29c-d68b-45b0-ba49-6d9cf03cfc15)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         05076e43-3f19-4d30-8ae6-230bb10d4aa3)(content(Whitespace\"\\n\"))))(Tile((id \
         feb79f1e-8f3e-4f0c-be02-4e8dc4f312b3)(label(cropValue))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         5efc0795-0f26-41a9-97c8-cc9e72079723)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         fac93ced-0820-4aae-93bb-088e8db41da5)(label(h))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         131dd60c-fe74-4ab8-bdc8-692c1a2a1efa)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         3f3ec49c-e3f5-45e0-8bd8-2c17b3a9bc84)(label(crop))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         674d9c8b-e091-47c5-98e3-91a255e2865b)(content(Whitespace\" \
         \"))))(Tile((id \
         4ee4fed0-be54-4489-bbf2-d825b23b094f)(label(*))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 27))(sort Exp))((shape(Concave \
         27))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         b894460c-6918-44ba-87d7-ad0d3f41c7cd)(content(Whitespace\" \
         \"))))(Tile((id \
         8434b8d9-5f12-4345-a050-26f12abc8587)(label(qualityMultiplier))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         a3e8b434-c315-4692-9daf-aebc0e31c60d)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         119378c8-c84f-44ec-b676-708d8fae9e98)(label(h))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         e0ae79b0-9cf7-4819-91e9-7852f1afd07f)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         3da97e86-f3fe-47fe-aa14-31419bf49af8)(label(quality))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         a4166089-2cae-40c5-af83-abd5e09d923c)(content(Whitespace\" \
         \"))))(Tile((id \
         a71cb91f-170c-475b-81d6-26611f8f0842)(label(*))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 27))(sort Exp))((shape(Concave \
         27))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         ddbf4a02-be9b-47aa-bece-b71f05c62326)(content(Whitespace\" \
         \"))))(Tile((id \
         57f69184-0977-47fa-abd6-0539fbaa0e69)(label(h))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         0447efcd-1ed7-4cf5-bb3d-32275ae8e5ac)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         7be86877-bce0-455f-b8cc-3790aacd5adb)(label(quantity))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         3ca30138-984a-47ca-b1b9-a9a0522d2351)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         62a1c2b0-0357-430c-91af-46a441ad1b97)(content(Whitespace\"\\n\"))))(Secondary((id \
         3e4c1b40-a162-420c-8345-a15c96e047b1)(content(Whitespace\"\\n\"))))(Secondary((id \
         01b5bdfc-de5b-443c-970f-6ac724dae06c)(content(Comment\"# Initial \
         empty ledger #\"))))(Secondary((id \
         00ab5e0f-3031-45fb-a5ca-2928b3786d5b)(content(Whitespace\"\\n\"))))(Tile((id \
         0e8e3b39-51d4-48d6-8696-83379f124fab)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         9ef8d877-7d97-4767-8c05-b3073e1aa79b)(content(Whitespace\" \
         \"))))(Tile((id \
         e9b7d650-d107-4ec7-8d5b-723c24aeac83)(label(initModel))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         2d24ab52-9a78-42cb-9f90-c8b1cde673fd)(content(Whitespace\" \
         \"))))(Tile((id \
         a463f9bf-760d-48fd-adac-294df3f7f8b3)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 24))(sort Pat))((shape(Concave \
         24))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         8418499c-7d05-44d4-9268-9685f0762285)(content(Whitespace\" \
         \"))))(Tile((id \
         4b53ab85-17de-4a4e-920c-5936769bf8d6)(label(Model))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         4e737496-ba43-4a23-a4af-5e6ea32a103a)(content(Whitespace\" \
         \")))))((Secondary((id \
         d0173279-e6e7-4448-8edd-059fa90ff301)(content(Whitespace\" \
         \"))))(Tile((id \
         8c245928-67a3-417f-851a-d99261157026)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         99d4b7de-ee5f-47c9-81f7-70c82d3391eb)(content(Whitespace\"\\n\"))))(Tile((id \
         66ec403b-6c92-403c-8923-57beb9a8e818)(label(harvests))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         694b5eb1-66f5-46a8-9df6-51f110f3318e)(content(Whitespace\" \
         \"))))(Tile((id \
         16b5af50-7d2f-4732-8e7e-f869f9b1dc63)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         b6a5a5e6-76ae-401a-b83e-4ee703bf8651)(content(Whitespace\" \
         \"))))(Tile((id \
         1393ea38-93cf-48eb-adac-7fe22d6d1a57)(label([]))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         d31d9717-782b-4c67-8eeb-8b6600ca57ef)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         340f526c-53f7-4e0c-adc6-68531e308366)(content(Whitespace\"\\n\"))))(Tile((id \
         a976dcd8-0ec9-4dec-9b30-1c2cbafc923e)(label(totalValue))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         6c8ff779-2f45-45df-a7bf-e35883b52cab)(content(Whitespace\" \
         \"))))(Tile((id \
         18c8297b-838f-4c92-90a2-6ec0347318bc)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         c799e25b-8c01-406b-b979-b317732200d9)(content(Whitespace\" \
         \"))))(Tile((id \
         5f2ff154-7af1-462e-85d6-67e55e210485)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         d3b59279-8545-4540-a748-09918f4e3a89)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         23061dc7-89f8-4c22-b29a-c55a6ae01f80)(content(Whitespace\"\\n\"))))(Tile((id \
         a27a2532-e92b-400a-8b9a-b64fb2463aa5)(label(streakBonus))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         77262429-59b0-4c14-b9d5-48058cfcc71a)(content(Whitespace\" \
         \"))))(Tile((id \
         a33b2134-5858-4557-9964-129c064ca611)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         d055e265-e3c7-4056-995d-dd226d4e6740)(content(Whitespace\" \
         \"))))(Tile((id \
         f8dc2f3a-c8b4-4dba-aefb-db52d72b1022)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         6ec15f86-d613-4494-833f-be4d6843163f)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         6d4b0104-9177-448b-9f4e-c1fa21ded34a)(content(Whitespace\"\\n\"))))(Tile((id \
         c2e461f7-dedf-462f-84ca-fec9c1daef73)(label(lastQuality))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         38b63fc8-263d-4739-837a-2ced8e4dd4e2)(content(Whitespace\" \
         \"))))(Tile((id \
         f98bc5d3-d815-43dd-b433-441a6331e0f7)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         5db4eb15-1d49-4d66-856c-6f3e934ba2a6)(content(Whitespace\" \
         \"))))(Tile((id \
         4a10f83f-5bab-444b-8c10-11314d9a4615)(label(Bronze))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         611f878d-b062-45e9-b384-c1412844e405)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         f38f1161-6df4-4219-85c1-67826d6409d6)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         42ffef93-6c10-4728-8530-fd6d7ae9c53e)(content(Whitespace\"\\n\"))))(Secondary((id \
         2d85a242-8be9-4c9a-94d7-1892fe74df05)(content(Whitespace\"\\n\"))))(Secondary((id \
         21e80e8e-8017-44d7-a6b5-db176c70d38d)(content(Comment\"# Process a \
         harvest action and update the ledger #\"))))(Secondary((id \
         103215a1-fdf8-4de4-9ab8-aff99edbc82f)(content(Whitespace\"\\n\"))))(Tile((id \
         935c7687-b70c-4c3c-b226-082ea2ad1bd1)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         c715ef98-9045-480b-bd7c-c121d9a64038)(content(Whitespace\" \
         \"))))(Tile((id \
         9eb64c4e-92b9-48a6-ad3d-248d44ee56ec)(label(processHarvest))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         1be5870d-7dd7-400d-89d1-0b4d8f0d3a2f)(content(Whitespace\" \
         \"))))(Tile((id \
         9484710f-0d9f-49f4-abb3-24b437afa849)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 24))(sort Pat))((shape(Concave \
         24))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         a093d1ba-6d46-4104-a842-22d8ba5bcf85)(content(Whitespace\" \
         \"))))(Tile((id \
         a4e564a0-0efb-47ec-8077-d6dfc9e019cd)(label(\"(\"\")\"))(mold((out \
         Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0 1))(children(((Tile((id \
         98650a81-5364-4f53-9abb-13ff0ae6d9d4)(label(Model))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         bf3bb168-35b8-457a-b4a4-56101475e1e2)(label(,))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 44))(sort Typ))((shape(Concave \
         44))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         575feb2a-2733-48eb-9f00-2488d7c99663)(content(Whitespace\" \
         \"))))(Tile((id \
         e6f47ae5-7a54-482c-95ec-dd3248566c87)(label(Harvest))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children()))))))))(Secondary((id \
         33ae296e-8de9-4b0a-8542-f9daac95a1b5)(content(Whitespace\" \
         \"))))(Tile((id \
         51b52cf6-7515-428f-94d5-9389b94a05ed)(label(->))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 13))(sort Typ))((shape(Concave \
         13))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         fa326541-cb61-4703-9be3-0a7bcdf8be10)(content(Whitespace\" \
         \"))))(Tile((id \
         820e0e9e-eae4-450c-aa06-f48c413a9cf6)(label(Model))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         6f7f43aa-b1c0-4129-99c3-fcc401cd9874)(content(Whitespace\" \
         \")))))((Secondary((id \
         bbdf115f-ccd3-45b8-a265-31f2fad16fda)(content(Whitespace\"\\n\"))))(Tile((id \
         84ee80f6-aaa6-4722-8f2c-b72a21cacf80)(label(fun ->))(mold((out \
         Exp)(in_(Pat))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         37))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         fd9a2440-96fd-4efc-97ab-d144ccf115f8)(content(Whitespace\" \
         \"))))(Tile((id \
         614b081b-57ca-4649-a0a3-0cbbd78b6947)(label(\"(\"\")\"))(mold((out \
         Pat)(in_(Pat))(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0 1))(children(((Tile((id \
         80652857-5f94-493b-9e7b-fe11eac34ec1)(label(ledger))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         b8c4811a-7a64-4815-868a-3aff61f02968)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 44))(sort Pat))((shape(Concave \
         44))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         a3fbc9b5-cf50-426f-a95c-424322be011d)(content(Whitespace\" \
         \"))))(Tile((id \
         30ec8587-e808-4e04-b40b-0a9d8be2acf1)(label(h))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children()))))))))(Secondary((id \
         831f3ff1-5e2a-4fd6-b141-2d998a4a7c8d)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         8d4f0119-300d-480c-a043-0a20b2529ce9)(content(Whitespace\"\\n\"))))(Tile((id \
         1bffe8ae-1e01-48f7-9841-a1714e16e4e9)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         b081d3d6-7af0-4f38-87e8-2b7ce87cf092)(content(Whitespace\" \
         \"))))(Tile((id \
         4af3be37-d9ca-4bbe-b83d-c1b5965218ef)(label(value))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         59f12db6-f9cc-4c30-8858-d96760d9296a)(content(Whitespace\" \
         \")))))((Secondary((id \
         00b3cdb9-d1a7-4c4d-96f8-8ec07c93e535)(content(Whitespace\" \
         \"))))(Tile((id \
         20a03e8d-f438-41f3-8176-ecea03b9d60c)(label(harvestValue))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         b05535e8-4660-411b-9db4-4ef548469103)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         5973f2b3-f1ae-4720-a297-8ba69b46bf70)(label(h))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         ecb26b15-f29a-4548-98a4-1943ab0b8d8b)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         46359aa8-7e5a-4969-b89b-639b121fdd0b)(content(Whitespace\"\\n\"))))(Secondary((id \
         22726d5f-234c-4d4a-903a-627b123a793a)(content(Comment\"# Check if \
         this harvest continues the quality streak #\"))))(Secondary((id \
         fd305310-2c44-4518-945f-4486875c1b85)(content(Whitespace\"\\n\"))))(Secondary((id \
         c14d01a2-9ceb-4d79-983f-fb5a20899963)(content(Comment\"# First \
         harvest never continues a streak (no previous harvest) \
         #\"))))(Secondary((id \
         d060de72-76c3-47d5-a4e4-070d026fecce)(content(Whitespace\"\\n\"))))(Secondary((id \
         2fb91a51-36df-488a-82af-d48b14accdae)(content(Comment\"# Compare \
         current quality with the PREVIOUS lastQuality #\"))))(Secondary((id \
         9a848d41-d8e6-473b-887c-24b17e29a6da)(content(Whitespace\"\\n\"))))(Tile((id \
         bae4cc22-dbb5-47ef-8250-300a5cf29c46)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         bcaf39bb-b9e2-42d4-ab8c-89a8d07b1569)(content(Whitespace\" \
         \"))))(Tile((id \
         493ab058-e4fb-4f38-85a1-c62997faf635)(label(isFirst))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         291bceb0-a298-49c9-9b5d-73ec1c73f5fa)(content(Whitespace\" \
         \")))))((Secondary((id \
         729a1aa8-d66e-455e-89ce-991f67710d73)(content(Whitespace\" \
         \"))))(Tile((id \
         2f73ef68-c533-4030-a1d4-72f4ee2eaa7d)(label(length))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         49533989-4d34-42cc-bb96-d137eb15fdd2)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         82db502a-c734-45fa-b7d4-c5d6fc87250d)(label(ledger))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         05794c18-7c3d-440e-a800-e7bcb681c41d)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         711598f5-9e22-4d7f-8fce-fe407b49c2e3)(label(harvests))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         09f62748-e644-405d-a183-bff19ac8d693)(content(Whitespace\" \
         \"))))(Tile((id \
         e140d820-b7d6-4586-b629-90aa26020300)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         aab668d7-84b1-460f-bacf-6897f6b1e782)(content(Whitespace\" \
         \"))))(Tile((id \
         1f7a8d58-e78f-4f48-ae21-d817d513bb4f)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         41608722-cbf6-4954-9552-ea03c70465bf)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         f6f44783-1d8e-4f8b-a69f-275f23a399ad)(content(Whitespace\"\\n\"))))(Tile((id \
         91143814-3c66-4ab5-bcd2-f1efa9b6f3c5)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         271ad709-4f43-4327-b6e5-e5bda3718079)(content(Whitespace\" \
         \"))))(Tile((id \
         1f7154db-fa36-4238-9591-58a20a52150b)(label(continues))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         ea514e20-a51e-4cbb-b2b2-a645d753a646)(content(Whitespace\" \
         \")))))((Secondary((id \
         bca9e406-0108-4280-a8f1-cedf00a60ef2)(content(Whitespace\" \
         \"))))(Tile((id \
         0e8114cd-f572-4a2e-b37b-58d4f7fa16f0)(label(!))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape(Concave 27))(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         d067400b-efa3-42bd-89c4-1f01ab7e7964)(label(isFirst))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         e54f28e0-ee47-4362-89aa-f1d4a7a8a3f8)(content(Whitespace\" \
         \"))))(Tile((id \
         3caa5908-fcf6-4232-9992-79566f299616)(label(&&))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 32))(sort Exp))((shape(Concave \
         32))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         d50399ae-19f3-406c-b66d-a0626b35cce5)(content(Whitespace\" \
         \"))))(Tile((id \
         cbc60cdd-c426-4314-88a2-ffab96785a81)(label(h))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         4a212697-40c3-4f8e-a5d5-3971fcfaf255)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         50919da7-a0f4-46cf-be75-54d6a10641b0)(label(quality))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         cd32494d-2813-44f6-bae5-0b264800540d)(content(Whitespace\" \
         \"))))(Tile((id \
         143cace9-6ddc-456e-8e2f-3d3c30a88094)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         318d8d18-fafd-45a1-aa4a-65751b953b1e)(content(Whitespace\" \
         \"))))(Tile((id \
         cece24f5-34c5-4a1c-9849-b8d3d95dead1)(label(ledger))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         f331cdbd-5f29-4de5-a3af-33a8996c9756)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         892f4f12-f0cc-4e63-8b33-fe12709f19b9)(label(lastQuality))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         a5d5758b-1754-46d2-a742-bab1932e905c)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         888447ef-a587-4934-9b50-f647dafea315)(content(Whitespace\"\\n\"))))(Secondary((id \
         4a338a8e-7095-410e-bc61-5425618c9dcb)(content(Comment\"# Now update \
         lastQuality to current harvest #\"))))(Secondary((id \
         01a70267-0fff-459c-b512-e7fe24dd595a)(content(Whitespace\"\\n\"))))(Tile((id \
         03be4646-1bac-450a-870b-22fdc3a912e5)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         b879b47a-f556-47f1-af30-050c0d77324b)(content(Whitespace\" \
         \"))))(Tile((id \
         c4cee4a0-c74f-4c80-bb76-06c5136fed21)(label(newLast))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         75c79cf7-90b9-46cf-86fb-95d13cddf4e8)(content(Whitespace\" \
         \")))))((Secondary((id \
         9fae186c-4ca5-4e91-b172-0d779f88d045)(content(Whitespace\" \
         \"))))(Tile((id \
         87071978-b416-43ef-aa71-24821161dc0c)(label(h))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         d93cdb75-f878-4371-bd19-25ecb2100eb5)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         6dca8b5a-d282-4bca-a41d-68bba0440fc8)(label(quality))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         e13ed2fb-27f4-4704-adbf-610c9503b451)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         98dc7ef9-d155-417a-b850-827159582b34)(content(Whitespace\"\\n\"))))(Tile((id \
         9d063832-bdcd-4399-a24a-3ba7a320356f)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         bbe33cf4-58d8-4f1a-80fc-79ee4e5818dd)(content(Whitespace\" \
         \"))))(Tile((id \
         b4ba87ee-e62d-462b-ac82-168dd496f1b0)(label(newStreak))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         d1b6a517-1698-4596-90f5-8c424302046f)(content(Whitespace\" \
         \")))))((Secondary((id \
         adaf7fae-5152-4b5d-aaf0-6cbc1f3f28e0)(content(Whitespace\"\\n\"))))(Tile((id \
         d66dcb23-25d2-4c14-916e-cf165cb1a9d0)(label(if then else))(mold((out \
         Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         36))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         cd2e2f77-28a1-40f2-8432-cccfee717605)(content(Whitespace\" \
         \"))))(Tile((id \
         0b0b2020-0f09-46d8-9c7c-9c132ca2c200)(label(continues))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         e9d0e696-012c-4e00-85d1-0555776f9744)(content(Whitespace\"\\n\")))))((Secondary((id \
         01e428d3-42b3-4b82-b83e-5ce254952a68)(content(Whitespace\" \
         \"))))(Tile((id \
         beea5904-8917-440a-befa-9e33bdec3344)(label(ledger))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         390d27ea-3190-41c1-9d1a-985be2a33291)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         ab4520de-73c4-41f4-b89c-412c30cd3f2a)(label(streakBonus))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         74ee55b8-5fc9-4e39-9c46-d97c4d3647fc)(content(Whitespace\" \
         \"))))(Tile((id \
         7b637c3b-a409-46d4-b76d-6921d5ab2108)(label(+))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 28))(sort Exp))((shape(Concave \
         28))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         2852a0f2-1711-48ca-9a95-1e783728e1ac)(content(Whitespace\" \
         \"))))(Tile((id \
         6e17c3db-38c7-4b0a-9b92-82e6922927cf)(label(5))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         140dd78f-88b8-4336-978b-f5546712b857)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         a0753d5f-8d1b-469c-92ca-a0892ce23e19)(content(Whitespace\" \
         \"))))(Tile((id \
         09561eea-c602-4f9d-95d2-722e1806ad19)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         0256c816-20c8-4d02-9f48-bb488a0957c1)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         f2b5de72-2413-441d-a9dd-2c34246021ae)(content(Whitespace\"\\n\"))))(Tile((id \
         96d4b264-94cd-4f32-831d-c1d34e750f0d)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         40bfc89b-e868-402f-a822-4d348e62f664)(content(Whitespace\"\\n\"))))(Tile((id \
         aa2eb975-5a98-46f8-840e-6b477e638277)(label(harvests))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         dee3e5a3-a513-4b4a-a343-92d9024f571d)(content(Whitespace\" \
         \"))))(Tile((id \
         bf3cec9e-60c6-4142-9588-19329c320c7e)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         86a8a996-ff03-428f-b258-5d1cbcc036c5)(content(Whitespace\" \
         \"))))(Tile((id \
         a6ae61ce-27ea-4037-bf77-27fa303bbcef)(label(h))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         51451112-e1bd-42e6-9e1c-a35afa29d0ca)(content(Whitespace\" \
         \"))))(Tile((id \
         cd5c574d-364c-46a1-b6a0-1aaec5ef2916)(label(::))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 29))(sort Exp))((shape(Concave \
         29))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         c3f7f674-3d97-47d3-b4e2-8b45696136a7)(content(Whitespace\" \
         \"))))(Tile((id \
         72a3f889-3d25-4f3a-a673-62990096d480)(label(ledger))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         503d28a2-ff7d-48df-86bb-d8280b47aeab)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         0751bff3-e503-480a-bd68-910229f4918d)(label(harvests))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         e1c6741e-2b8e-4539-a600-172454ea027b)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         1b13dae2-be4d-462b-82d2-2dfaf1226a53)(content(Whitespace\"\\n\"))))(Tile((id \
         7fa38f0d-5210-425c-9273-b1c43645af15)(label(totalValue))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         f04e6739-d85a-49fe-8080-30ca2fe386a7)(content(Whitespace\" \
         \"))))(Tile((id \
         ef3940f9-1efe-46d8-be6e-76900634f126)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         67e16c9f-6e35-48e5-9c18-64b590f058be)(content(Whitespace\" \
         \"))))(Tile((id \
         70e68cc6-5a24-4260-b288-64e13c12ad47)(label(ledger))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         cf760c98-9651-4b7b-88ae-0c453f753642)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         74ef70de-3aa6-4965-8449-ebe025b8490a)(label(totalValue))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         9e284fae-1f25-4489-afa8-f4f2e98941b4)(content(Whitespace\" \
         \"))))(Tile((id \
         e282551b-2caa-4833-ba6b-19ba265ee705)(label(+))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 28))(sort Exp))((shape(Concave \
         28))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         7193860d-bc44-4d47-865c-54bb0f886d13)(content(Whitespace\" \
         \"))))(Tile((id \
         bf5619eb-8ff6-427f-8b5e-14dd345be4e6)(label(value))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         b95c021f-5275-4645-8c87-e29527191a0a)(content(Whitespace\" \
         \"))))(Tile((id \
         f31a3aa1-94ac-494f-a8a2-04907c9768f1)(label(+))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 28))(sort Exp))((shape(Concave \
         28))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         362da4ec-5113-45a3-950e-6a6d90060685)(content(Whitespace\" \
         \"))))(Tile((id \
         fe38d314-ea6c-4a14-837f-9ba65ba18cb2)(label(newStreak))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         0f677599-a149-4bf3-8c22-1c96ba021f39)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         25c77070-afb6-4bf5-a4b5-7ba76090cbcf)(content(Whitespace\"\\n\"))))(Tile((id \
         ca1d381e-c1cc-413a-b319-f5b3cd14ce00)(label(streakBonus))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         d4689115-8a64-4801-847e-33e3aea0e5ea)(content(Whitespace\" \
         \"))))(Tile((id \
         810d8c1f-7d0c-4637-ac61-05ac950ed725)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         0225396b-2d75-4582-971e-635497186c62)(content(Whitespace\" \
         \"))))(Tile((id \
         15909267-133d-4a8d-8793-e7bc4469f537)(label(newStreak))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         a29062f2-8230-4543-b45f-a3fca920b407)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         70042a1d-5a69-4f29-8b04-f04397169a7e)(content(Whitespace\"\\n\"))))(Tile((id \
         e0f309cf-cae8-48d8-ad92-06c964fc58a8)(label(lastQuality))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         d55c86ca-7f4e-4339-8e91-381988317153)(content(Whitespace\" \
         \"))))(Tile((id \
         4e61c76a-eaa9-4127-b76f-89fb011666b7)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         6abc67f3-4b96-4282-9f95-875d1280cb10)(content(Whitespace\" \
         \"))))(Tile((id \
         df9fab34-f53f-4527-9b6d-8727487f284c)(label(newLast))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         483a9506-8dfa-474f-b857-948387af7a38)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         88140546-0306-4a11-9111-cb38ab17efd7)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         71b8990e-2598-488d-90a7-f02ac5ef7efa)(content(Whitespace\"\\n\"))))(Secondary((id \
         a2cd864b-0025-426b-949b-e46acbd297c1)(content(Whitespace\"\\n\"))))(Secondary((id \
         35525df6-4c6e-47a9-8fdd-20ba868a4c6b)(content(Comment\"# Claim the \
         streak bonus and reset it #\"))))(Secondary((id \
         96a4853f-86fb-44bb-a637-6371a1848da8)(content(Whitespace\"\\n\"))))(Tile((id \
         67cb10c8-5b89-43c5-b694-c8ce86335d84)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         e23c752e-c293-4683-96c2-d380fa670fe0)(content(Whitespace\" \
         \"))))(Tile((id \
         8e0cbd70-a967-4165-8227-fb754cf0ff90)(label(claimBonus))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         ddb12e70-56b8-4288-977c-2140147ee3b4)(content(Whitespace\" \
         \"))))(Tile((id \
         cd2acde7-f11e-455c-9187-5a3c4baa892c)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 24))(sort Pat))((shape(Concave \
         24))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         d4924364-1de7-4a05-8171-bb4ba4464501)(content(Whitespace\" \
         \"))))(Tile((id \
         2fba1b92-1b20-41f4-8eb3-ae3897985507)(label(Model))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         08910a34-7b9f-4867-850e-7b327b7433ce)(content(Whitespace\" \
         \"))))(Tile((id \
         30a165cd-f082-4c72-ba29-ed74fc2fa3f3)(label(->))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 13))(sort Typ))((shape(Concave \
         13))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         2c5fd662-9911-4799-934e-37e867a5a1c0)(content(Whitespace\" \
         \"))))(Tile((id \
         5611cbd8-a9b9-4247-8dff-109804a32380)(label(Model))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         338395f7-0cad-49d4-88e0-a4304ca21de6)(content(Whitespace\" \
         \")))))((Secondary((id \
         41379e5c-0f46-4158-a84c-c3d6c2c46c2a)(content(Whitespace\"\\n\"))))(Tile((id \
         35a6bae1-8158-4714-88bd-62afeb0ddb7c)(label(fun ->))(mold((out \
         Exp)(in_(Pat))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         37))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         19dcf48f-8586-4b4c-91a5-62b86ab968ff)(content(Whitespace\" \
         \"))))(Tile((id \
         5b49eb47-9168-486b-b96d-a5448b2ade07)(label(ledger))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         d20cc7d7-38fa-4edc-88db-cd4db1abbb34)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         270783bd-b76f-46cc-960e-3144cb97ea5c)(content(Whitespace\"\\n\"))))(Tile((id \
         e0ee416c-2b94-44ee-a424-4e8bf2874f96)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         a039b359-083b-4417-871c-4b87cfe8d43d)(content(Whitespace\"\\n\"))))(Tile((id \
         d653848c-c774-4728-a270-9d63e67dfbe7)(label(harvests))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         802eeaa6-9112-44bd-9566-1a07c41d123b)(content(Whitespace\" \
         \"))))(Tile((id \
         c92d0899-eb37-4f71-891b-37b4b3ef5497)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         3fcf06d3-98fe-4313-94a9-bff38934f3e9)(content(Whitespace\" \
         \"))))(Tile((id \
         edac4590-cefa-4c40-9bd6-951b7cfbe248)(label(ledger))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         8874e77f-6fb5-45f5-b7f3-8e7af42231b0)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         7f142acd-379f-4258-bdcd-025301834e0b)(label(harvests))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         0cdde3aa-6f17-408b-b192-24d2d05079b3)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         230e09f3-35ce-4b0e-b25f-3ec1031c4141)(content(Whitespace\"\\n\"))))(Tile((id \
         07df8843-3521-4c85-8b71-527f36597c5e)(label(totalValue))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         cd0181ad-777e-450d-a908-a8b25de74d6d)(content(Whitespace\" \
         \"))))(Tile((id \
         5b3838ec-ff79-4a20-ace7-e058954ca3d3)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         4b2320a1-5656-4611-90b0-3e881dfa18e2)(content(Whitespace\" \
         \"))))(Tile((id \
         3bfdbd9a-4e77-41d6-b75e-5458576214b1)(label(ledger))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         f47a86d7-d71f-4e37-8b19-f96c96266f6f)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         609c1376-41e9-4d59-895c-68e95ecfc1dd)(label(totalValue))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         97f2cf11-2bbb-4e60-84a3-fe6419b66f19)(content(Whitespace\" \
         \"))))(Tile((id \
         d424af87-f85a-40eb-8bed-d6a59947473c)(label(+))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 28))(sort Exp))((shape(Concave \
         28))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         a4452b7a-9c0f-4229-a4d9-6cfdec34284f)(content(Whitespace\" \
         \"))))(Tile((id \
         887c99bb-0849-484f-a18b-bdcbb7481b20)(label(ledger))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         6bf6b7da-ed2d-4d90-9436-983582ec05a2)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         1801d589-a234-4600-a09a-3980789eb1dd)(label(streakBonus))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         60dec71e-ff34-46e0-a618-b5eb9231bc1a)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         6d1806bb-80f2-4fb1-acf8-72e2fc2e61c1)(content(Whitespace\"\\n\"))))(Tile((id \
         efd3de64-9653-4f85-bca1-233d609eb144)(label(streakBonus))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         1dbd8f71-8aaa-4da4-92ff-b33387b25944)(content(Whitespace\" \
         \"))))(Tile((id \
         8dc6874c-19a6-41ee-8a5a-28128e73fc5f)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         abeb8807-40cb-4b5d-b784-abe3222b39da)(content(Whitespace\" \
         \"))))(Tile((id \
         997e7ffa-a770-42d6-bdac-bfedf63131e7)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         2e273a79-6051-4398-924b-bd8f9a53702c)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         3080c00e-f215-4587-996f-f13f43e385e8)(content(Whitespace\"\\n\"))))(Tile((id \
         55c09166-cefc-4c18-b8e4-7c6738fab227)(label(lastQuality))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         04a90c1d-6f7a-4420-9a3c-37ba62adb5ab)(content(Whitespace\" \
         \"))))(Tile((id \
         060cd16b-be36-458d-81ed-92f597940730)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         b35f1fb2-7832-48ec-a140-a8a3dc8bbe37)(content(Whitespace\" \
         \"))))(Tile((id \
         b4d5b6c1-d9de-4efc-95c9-27cefe93f475)(label(ledger))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         542b6d94-2ec2-4934-82e4-1c41165167d9)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         1814a61c-5282-432b-b26d-55d6e0ec99d6)(label(lastQuality))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         bd7d2370-6391-4361-8908-5f2331059542)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         98425122-0a0e-4125-ad0e-d7b3307c711c)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         48ec4f4d-ef33-485c-ae75-e4b0befdfe90)(content(Whitespace\"\\n\"))))(Secondary((id \
         b167424d-0369-477f-bc93-e90f8e92c40d)(content(Whitespace\"\\n\"))))(Secondary((id \
         5c75653a-0774-4df0-ba56-005d08e4e639)(content(Comment\"# Close the \
         harvest day - reset streak tracking #\"))))(Secondary((id \
         d8366439-cdd7-4975-9e7e-c2b08d3757f9)(content(Whitespace\"\\n\"))))(Tile((id \
         47f36e9d-5306-4e6f-b6c7-a8a004ac670d)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         ade96046-5e69-489c-972f-bd0f8dcffdfd)(content(Whitespace\" \
         \"))))(Tile((id \
         2340c573-85f2-46bd-b928-0012ae26c54a)(label(closeDay))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         865920d4-77c7-411f-a3a6-90874b251e7a)(content(Whitespace\" \
         \"))))(Tile((id \
         b4a0e094-67b8-4fe6-8a7b-db34f156836a)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 24))(sort Pat))((shape(Concave \
         24))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         5b0304fd-bb3a-48c7-8455-f8b64cc6d6fc)(content(Whitespace\" \
         \"))))(Tile((id \
         0e5eea70-da48-42ec-a0e5-f8f659e140d2)(label(Model))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         9c258276-2a7a-44e3-8431-decf0ee6c5c1)(content(Whitespace\" \
         \"))))(Tile((id \
         54b45f78-66db-4b30-8c58-3dce6783747f)(label(->))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 13))(sort Typ))((shape(Concave \
         13))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         69056493-dc27-47ab-b90a-2da292bb5d15)(content(Whitespace\" \
         \"))))(Tile((id \
         79559342-4c02-4793-9acc-a05b358bcd83)(label(Model))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         5d08d976-e4ec-4858-a178-23d81d1eb4f7)(content(Whitespace\" \
         \")))))((Secondary((id \
         a6b89fbb-a970-4d54-9f74-3a745920d35b)(content(Whitespace\"\\n\"))))(Tile((id \
         7b4dca84-c14c-4987-8f86-f678679845d1)(label(fun ->))(mold((out \
         Exp)(in_(Pat))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         37))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         5b7cbc22-6821-4c02-940b-ed4f3125ce2c)(content(Whitespace\" \
         \"))))(Tile((id \
         040efd74-7fd5-4628-be32-921a122af06f)(label(ledger))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         7b5e7fbd-4383-4076-a891-c4830e3cf9d3)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         e60ef0b7-a6ba-4cd5-a6bc-d68c49806479)(content(Whitespace\"\\n\"))))(Tile((id \
         39398f3e-4356-49db-ba6a-61da4623dbff)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         5ef03d4b-0289-403f-8e5c-5a4af8bd0d6d)(content(Whitespace\"\\n\"))))(Tile((id \
         fb0ca176-65ff-4f8a-8ab1-f937b54cf736)(label(harvests))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         dd7a423c-c7bc-4e69-96f2-df32e68b1b4e)(content(Whitespace\" \
         \"))))(Tile((id \
         8d087567-4138-4c84-a688-ee4418aac943)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         7bc74bc1-9103-4cab-b8be-550572ee6824)(content(Whitespace\" \
         \"))))(Tile((id \
         9f10b338-cffa-4ee8-a464-7fc27e162c61)(label(ledger))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         e8a4854b-0714-4c75-8ec3-efcca0569d5a)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         7418109f-c092-4875-8731-f3e7248ddf91)(label(harvests))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         d41e2c74-a67a-472a-bc5d-60fb72d81cba)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         8cfeaf09-d6bc-4e31-a69e-c0866edbaf7b)(content(Whitespace\"\\n\"))))(Tile((id \
         de6027e2-3843-465c-a9d1-0f7c7983c682)(label(totalValue))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         a4fe2486-be0e-4cba-a78e-c3b2b66fa3a0)(content(Whitespace\" \
         \"))))(Tile((id \
         0bd56876-a04c-4600-87fc-ccbf7f11c828)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         830c5102-f795-4990-bd03-b4789f67702c)(content(Whitespace\" \
         \"))))(Tile((id \
         1578a9c9-3516-4260-901f-add668dced98)(label(ledger))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         7f16b26d-df7b-456d-b182-f15f657407f8)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         8d2ba887-5479-48b0-a3b1-f418dfe9c50e)(label(totalValue))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         17ad25ea-abd0-4220-a279-2a8aa3f3365c)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         00c7af24-d62d-4762-8cfc-75d810fc1943)(content(Whitespace\"\\n\"))))(Tile((id \
         4ef4d735-6e94-4d61-acc4-c9942c56a021)(label(streakBonus))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         3a733f7f-ca13-4e06-988b-301ac1f71478)(content(Whitespace\" \
         \"))))(Tile((id \
         86c6c461-e155-49f9-b4ff-e159e9654ccd)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         3cde3352-919e-402c-acac-39de0a20c6b7)(content(Whitespace\" \
         \"))))(Tile((id \
         aafc3235-c99e-4e38-9a40-dc3baf01ed52)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         7dc17fd2-19a1-4207-98a5-c252f0cd4ffd)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         b6d6eb65-713d-44cd-865e-a8e9df7299c3)(content(Whitespace\"\\n\"))))(Tile((id \
         d872097d-a9d0-428d-8112-e1ba5febf8e6)(label(lastQuality))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         dd9b773f-e972-4c09-9b21-5c3e42208c88)(content(Whitespace\" \
         \"))))(Tile((id \
         ad318153-2b19-4e10-81cc-35a2d9650bd9)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         5a84256f-e24e-45f3-a6c8-77ea01e61449)(content(Whitespace\" \
         \"))))(Tile((id \
         37f79185-8c02-4a87-afdc-41991f7ceac3)(label(Bronze))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         b6dd5ff9-3e7f-400d-9e4d-0baa2682e6bf)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         38e0c5e8-8496-4689-8bdc-f277d8758651)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         618e51e6-f0e8-45a2-b36b-f620c282992a)(content(Whitespace\"\\n\"))))(Secondary((id \
         8b7e3567-5460-41e0-974a-5a48ffdaa74c)(content(Whitespace\"\\n\"))))(Secondary((id \
         712449da-120f-4cb8-a9ee-2f81afa832b0)(content(Comment\"# TODO: Add \
         premiumMultiplier helper here                      \
         #\"))))(Secondary((id \
         bc617b99-ff5f-4cab-95a6-4f8271bcc3ae)(content(Whitespace\"\\n\"))))(Secondary((id \
         e588017f-64c3-4f37-a3dd-9a52ddd64923)(content(Comment\"# It takes a \
         streakBonus (Int) and returns the multiplier:     \
         #\"))))(Secondary((id \
         7b943ad1-ccb1-4002-a261-974b2c4ad914)(content(Whitespace\"\\n\"))))(Secondary((id \
         cfab8fa3-fa82-4eb5-a19d-6a52b11ef83d)(content(Comment\"#   - Return 2 \
         if the streak bonus is >= 10 (strong streak)    #\"))))(Secondary((id \
         19bf69af-a370-40ea-9a44-271fb36751ce)(content(Whitespace\"\\n\"))))(Secondary((id \
         087a00aa-7d40-4a4b-9b5b-c8f5b40303c0)(content(Comment\"#   - Return 1 \
         otherwise                                      #\"))))(Secondary((id \
         48f50de5-d825-4ef6-93c5-9313125f8039)(content(Whitespace\"\\n\"))))(Secondary((id \
         c540ed0b-8a98-454c-958c-b0f070a29994)(content(Comment\"# Hint: This \
         is a simple if/then/else on the streakBonus.     \
         #\"))))(Secondary((id \
         3597c6db-86da-4553-9b94-88343c4b4319)(content(Whitespace\"\\n\"))))(Secondary((id \
         ffe28952-f48f-4766-be8b-e60cc3b31afc)(content(Whitespace\"\\n\"))))(Tile((id \
         67e20ac9-b145-4ce4-ac8b-b620d516494f)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         26a97ac7-b2f6-4488-8451-e388bdb02ef2)(content(Whitespace\" \
         \"))))(Tile((id \
         457194ae-d0ff-49c5-942c-12d667ae10dc)(label(premiumMultiplier))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         4dbe3745-2a26-45c3-910e-672d45260407)(content(Whitespace\" \
         \"))))(Tile((id \
         8bd8c863-df00-4a4e-ad15-598cee0b9fb8)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 24))(sort Pat))((shape(Concave \
         24))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         c2310768-f1ea-4d4a-b9f1-6eaf646e5856)(content(Whitespace\" \
         \"))))(Tile((id \
         85deed24-5aa0-4957-9f42-b61b6f1a3233)(label(Int))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         d8652041-0e19-4ab5-bc69-5fa586f9351d)(content(Whitespace\" \
         \"))))(Tile((id \
         0d8c6928-0684-408d-a4c3-e0d335b88a77)(label(->))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 13))(sort Typ))((shape(Concave \
         13))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         934a0d71-5acd-46ae-abed-44862e00b2db)(content(Whitespace\" \
         \"))))(Tile((id \
         6cd86c6e-dd3d-48ee-bd8e-22bdcd1bbbe7)(label(Int))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         44d7cbf6-ed18-4cf0-a2b1-d1d45031d45e)(content(Whitespace\" \
         \")))))((Secondary((id \
         4cfcdc12-7668-446c-91b1-a1483ad55186)(content(Whitespace\"\\n\"))))(Tile((id \
         f1fefe46-2bf2-453b-b85a-8c462dcd0633)(label(fun ->))(mold((out \
         Exp)(in_(Pat))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         37))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         b5234070-750f-4b45-aaa5-2a35fb15412a)(content(Whitespace\" \
         \"))))(Tile((id \
         1d0e88de-c86b-4288-8804-74e51e76e526)(label(streakBonus))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         5d4eb79a-06d6-4fba-a9eb-9c2cb0aae6ff)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         ae05d7a1-8aed-451a-b991-a6b2316763b9)(content(Whitespace\"\\n\"))))(Tile((id \
         d8472e99-477c-4344-8c36-db258bb44b32)(label(?))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         b14168df-909b-48d3-995e-8ae6ef93000d)(content(Whitespace\"\\n\"))))(Secondary((id \
         4e8ff035-0583-4845-8e04-716b0e41cb5d)(content(Whitespace\"\\n\"))))(Secondary((id \
         0a55103c-0742-46c4-9f47-a19b436cd412)(content(Whitespace\"\\n\"))))(Secondary((id \
         c8c6a1c7-2875-4de3-971f-2323bdc5ecf6)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         98d2e2cf-95dc-4ffd-b572-ea23ec40f237)(content(Whitespace\"\\n\"))))(Secondary((id \
         3fc2b0cc-5473-47c2-8834-c34ce7660c13)(content(Whitespace\"\\n\"))))(Secondary((id \
         0a38177b-937d-4ae5-9a34-6010301b44fb)(content(Comment\"# Main update \
         function - dispatch actions #\"))))(Secondary((id \
         66a652ff-ff52-4af8-bec5-9d486bb3a904)(content(Whitespace\"\\n\"))))(Tile((id \
         58324458-0682-4426-9b12-2bf1d25f63a6)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         2dcce6b8-72f2-4ddc-a23b-aa89d50a5c4d)(content(Whitespace\" \
         \"))))(Tile((id \
         744802d0-2ec5-4f35-adbc-c8ccda66eff7)(label(update))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         534d71c2-1d20-4f13-b0d4-384a7bd658c7)(content(Whitespace\" \
         \"))))(Tile((id \
         5d244fa7-33ba-4868-b68d-fef08542db48)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 24))(sort Pat))((shape(Concave \
         24))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         c53e1e83-36d3-4b71-b3d8-67825772e9a1)(content(Whitespace\" \
         \"))))(Tile((id \
         81cf6c4f-acb9-4488-a362-b62cee68c2e3)(label(\"(\"\")\"))(mold((out \
         Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0 1))(children(((Tile((id \
         7ff6126b-7b24-4287-8ea7-190902248ea7)(label(Model))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         ce1f17b9-6e7d-4cd6-a8dd-9115467360d5)(label(,))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 44))(sort Typ))((shape(Concave \
         44))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         a073f1d2-3b5f-408d-ab1c-10a25b6e05f7)(content(Whitespace\" \
         \"))))(Tile((id \
         a74a972e-8d21-4ce0-afbc-fbdb4efcb931)(label(Action))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children()))))))))(Secondary((id \
         6fa3f17e-d217-4fb6-977c-c2985ffcf2bd)(content(Whitespace\" \
         \"))))(Tile((id \
         b4cba769-01f0-4149-b98b-e1919634657f)(label(->))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 13))(sort Typ))((shape(Concave \
         13))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         0ffacae1-879f-49e3-a888-8329fb4e04b0)(content(Whitespace\" \
         \"))))(Tile((id \
         0c190ae5-9884-4fc2-b066-2331bef51de1)(label(Model))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         a26638d8-936d-4000-9a03-e05e832fded9)(content(Whitespace\" \
         \")))))((Secondary((id \
         367c611f-d692-4a67-be40-dab2ee0ed62d)(content(Whitespace\"\\n\"))))(Tile((id \
         b546f4b8-9ba3-4837-be17-abf15542cb7f)(label(fun ->))(mold((out \
         Exp)(in_(Pat))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         37))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         56f30592-0650-4a2e-970f-477aa8ce6373)(content(Whitespace\" \
         \"))))(Tile((id \
         7751d3dd-4050-4164-ba18-ed320a6ca425)(label(\"(\"\")\"))(mold((out \
         Pat)(in_(Pat))(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0 1))(children(((Tile((id \
         404774f9-783d-4205-8f25-905bf7332dae)(label(ledger))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         10485185-939f-41f2-8a65-ad04752de4ae)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 44))(sort Pat))((shape(Concave \
         44))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         5e563ed4-7fab-43db-ae47-6151c93ef690)(content(Whitespace\" \
         \"))))(Tile((id \
         8c678980-e248-499b-b569-c256da35b626)(label(action))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children()))))))))(Secondary((id \
         008b8155-406a-4225-995b-fdd1a79dc5f6)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         6852fbf5-2089-4e1a-a27d-8c9419363c41)(content(Whitespace\"\\n\"))))(Tile((id \
         ace54bac-0915-4a86-ba33-58889e62e0a9)(label(case end))(mold((out \
         Exp)(in_(Rul))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         5f0bf693-734d-44b4-9014-d3b1d58ae979)(content(Whitespace\" \
         \"))))(Tile((id \
         5e414dcc-02f1-4846-b7bb-fe2e48d9b574)(label(action))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         391c86c3-d5a4-43df-87b2-38de9cc6995d)(content(Whitespace\"\\n\"))))(Tile((id \
         f5008534-c353-4a8a-a6d3-de06443c8ad3)(label(| =>))(mold((out \
         Rul)(in_(Pat))(nibs(((shape(Concave 46))(sort Exp))((shape(Concave \
         46))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         e126fa4b-7b0d-4f08-93cf-ea364cade247)(content(Whitespace\" \
         \"))))(Tile((id \
         8bff111e-9029-4be8-818a-bba564e2b7a5)(label(RecordHarvest))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         9a3e78a4-3aa5-4b00-a230-3e5a78ae4be6)(label(\"(\"\")\"))(mold((out \
         Pat)(in_(Pat))(nibs(((shape(Concave 23))(sort Pat))((shape \
         Convex)(sort Pat))))))(shards(0 1))(children(((Tile((id \
         be5ca62c-acbb-4dbb-ab57-812373b0c8f7)(label(h))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children()))))))))(Secondary((id \
         9e53569c-ad4c-425a-9028-e53399dfbc33)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         9f74b6c0-bc97-4770-a634-09794951b73a)(content(Whitespace\" \
         \"))))(Tile((id \
         b01148fe-1e4b-42db-9e6d-045efcdebfc1)(label(processHarvest))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         3360dd99-97c8-4532-99bf-b603f0e06c4c)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         d044c371-829f-48e3-9f26-69d97fd5ac33)(label(ledger))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         6c16411e-f0b0-4beb-a552-a49e49fb341c)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         9ffedb90-5db6-493d-8e7e-d58770888796)(content(Whitespace\" \
         \"))))(Tile((id \
         8075113e-3fb6-4272-8d5c-eaaef4b884d6)(label(h))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         79d458b2-78b6-45f3-87da-251659a9a2ed)(content(Whitespace\"\\n\"))))(Tile((id \
         082b129b-3dcf-49af-946c-29cf69f54917)(label(| =>))(mold((out \
         Rul)(in_(Pat))(nibs(((shape(Concave 46))(sort Exp))((shape(Concave \
         46))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         2a61b35a-69b4-4c30-9ec6-49eeac2f2ca7)(content(Whitespace\" \
         \"))))(Tile((id \
         57b91053-c853-44fd-b331-1a6d9615aef0)(label(ClaimBonus))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         6a51a5af-75b6-457a-99e9-898a94791052)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         92c3d007-dd40-4a88-8214-849612d6035e)(content(Whitespace\" \
         \"))))(Tile((id \
         27c0a218-e94f-4629-a90c-8d5e11a64a1b)(label(claimBonus))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         33ba3b3b-32f7-4df7-9b20-269fbac25b25)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         8e5e1d2a-2fad-4432-9b1e-f269620a2a0f)(label(ledger))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         a47962ec-a8cb-4ab1-87f2-15585c392392)(content(Whitespace\"\\n\"))))(Tile((id \
         b911f3e3-51a7-4926-b6a5-d7e59550a78d)(label(| =>))(mold((out \
         Rul)(in_(Pat))(nibs(((shape(Concave 46))(sort Exp))((shape(Concave \
         46))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         90ab113d-97d4-46c8-844c-d0a6975f0345)(content(Whitespace\" \
         \"))))(Tile((id \
         0ff100cf-b748-4b43-aabe-6e4192fce764)(label(CloseDay))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         08c01456-89a3-4449-a02c-ca8900576dd1)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         d4f6cd9c-7d63-44f3-8b04-56762787447a)(content(Whitespace\" \
         \"))))(Tile((id \
         7d5f4ff3-fc76-4b6b-b88f-e9c2e022c77c)(label(closeDay))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         0fa98e99-b6d5-4d51-a662-eeb53d0e8036)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         d372c145-2e5b-4352-b364-b9fb02f7d6f1)(label(ledger))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         4d893781-283b-44e8-941c-f8bfd85ac671)(content(Whitespace\"\\n\"))))(Secondary((id \
         c981b806-35e8-4ba3-bcb1-1f461cdc59d4)(content(Comment\"# TODO: Add \
         PremiumSale case here                          #\"))))(Secondary((id \
         ccd75127-cb79-4c6c-9720-3f87eea21f0d)(content(Whitespace\"\\n\"))))(Secondary((id \
         60116938-0c7d-4143-b761-deaa8a6cebe9)(content(Comment\"# Hint: \
         Compute payout = streakBonus * premiumMultiplier,  \
         #\"))))(Secondary((id \
         48cfae7e-91e1-491e-965e-60b2abc81ac4)(content(Whitespace\"\\n\"))))(Secondary((id \
         7def496c-520d-4bf0-8c6f-2dece599ece6)(content(Comment\"# add payout \
         to totalValue, and reset streakBonus to 0.    #\"))))(Secondary((id \
         45d32c42-8f01-40b9-b94d-44490b201ed7)(content(Whitespace\"\\n\"))))(Secondary((id \
         448c3273-6c4c-4450-beb4-6eca3d95d235)(content(Comment\"# Keep \
         harvests and lastQuality unchanged.                 \
         #\"))))(Secondary((id \
         7036c4fe-2572-4e90-85c0-a014e37c29ee)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         a626d436-14fb-4b05-84ca-e02f8cbeed0d)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         03dc34d3-4c0e-474b-bf6d-510e5fc91f81)(content(Whitespace\"\\n\"))))(Secondary((id \
         06d36bf8-fbd4-472d-8f0e-807219fc8931)(content(Whitespace\"\\n\"))))(Secondary((id \
         07d7e936-8b17-479e-895b-690d29c13ce1)(content(Comment\"# Run multiple \
         actions in sequence #\"))))(Secondary((id \
         6390e7a6-bb30-4d2a-8315-38dcad2105b9)(content(Whitespace\"\\n\"))))(Tile((id \
         d99d443b-756f-4b06-8e7d-baf98fa4e0ae)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         1f5c44a2-de93-4ea9-8f7e-df32d39b9121)(content(Whitespace\" \
         \"))))(Tile((id \
         dd10beb6-d039-4ad9-a1a4-9647cde7d8a5)(label(run))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         ea4640c3-2419-4641-b497-0020c8bf198a)(content(Whitespace\" \
         \"))))(Tile((id \
         0dbb9724-f1f3-4e3b-a4e3-0d159e1794fc)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 24))(sort Pat))((shape(Concave \
         24))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         2cd39266-285a-4f39-b5ad-38c060be69da)(content(Whitespace\" \
         \"))))(Tile((id \
         76239a22-de6b-4dd8-a4c2-67ed8ae7efff)(label(\"(\"\")\"))(mold((out \
         Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0 1))(children(((Tile((id \
         b0ec8bc3-9e1f-4ba7-82ff-2717c1834980)(label(Model))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         51d92053-f723-4ad9-8707-976d22574bb1)(label(,))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 44))(sort Typ))((shape(Concave \
         44))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         a428f83b-a366-414a-b95e-b869b1c8efdf)(content(Whitespace\" \
         \"))))(Tile((id 42181f3f-0cfc-4c6d-8770-ea722ccf8cf7)(label([ \
         ]))(mold((out Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape \
         Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
         98031823-0780-4eb9-89bf-179409053c68)(label(Action))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))))))))))))(Secondary((id \
         12a03e19-e8bd-4163-b3ca-ef2ecddb87c4)(content(Whitespace\" \
         \"))))(Tile((id \
         0f64714f-6306-4f5b-acfe-66cb78bf98f1)(label(->))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 13))(sort Typ))((shape(Concave \
         13))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         b51acdc9-dfaa-4da6-9e92-088c679bfe58)(content(Whitespace\" \
         \"))))(Tile((id \
         361a039b-8cd9-405c-aaea-51ce57b26cf1)(label(Model))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         5810ff35-67af-44b8-ad55-5344e4bf3db6)(content(Whitespace\" \
         \")))))((Secondary((id \
         9d619654-700c-46fb-a1bf-135322f787f1)(content(Whitespace\"\\n\"))))(Tile((id \
         6c52dc23-c02b-4188-9bdc-7fb551894b72)(label(fun ->))(mold((out \
         Exp)(in_(Pat))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         37))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         4b2a0d3d-2931-4233-b45b-3be1ba497d3f)(content(Whitespace\" \
         \"))))(Tile((id \
         0283a082-28a6-46f3-add1-c23dc571a0c1)(label(\"(\"\")\"))(mold((out \
         Pat)(in_(Pat))(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0 1))(children(((Tile((id \
         b84a0413-e52a-44af-be5e-9a5e518d8ddd)(label(ledger))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         a67c0409-afd2-47a7-8024-275e4f1c3533)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 44))(sort Pat))((shape(Concave \
         44))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         eef47943-bdac-41c8-bcc4-b1d26c2e7ef6)(content(Whitespace\" \
         \"))))(Tile((id \
         dcd437f6-ace0-4b18-9fb5-45a9213e63e8)(label(actions))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children()))))))))(Secondary((id \
         7b6cb4a3-0396-4d3e-958b-644568f06f2a)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         6cd552a8-9caf-438c-9236-62722a483858)(content(Whitespace\"\\n\"))))(Tile((id \
         9423adc7-8087-4ecf-8fb1-905055ae2bc0)(label(fold_left))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         fb14c2ae-3910-42a6-82fc-b80fc27861f6)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         244936cf-2856-4635-9bc4-d68f0e200a99)(label(actions))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         bc6b3eb9-e6c0-4669-af73-ac9b8466be00)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         093207ff-f2a6-42fc-a663-39bffb18d56c)(content(Whitespace\" \
         \"))))(Tile((id \
         b4f756b6-85be-44d3-84c0-87ef352f6fd1)(label(update))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         c31ed6f8-7c32-44e8-a6ce-0ddf89557756)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         8252a10d-f4b1-40fe-85e5-95bfd232de20)(content(Whitespace\" \
         \"))))(Tile((id \
         206a8c6f-e862-480b-b1ed-042a37f0fabb)(label(ledger))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         4cd91a2d-d256-4067-9331-5f1559534a70)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         1ad276e0-0fd5-42bc-afd1-d90b53858615)(content(Whitespace\"\\n\"))))(Secondary((id \
         29643ba5-910c-4f3e-ba50-29502d976c07)(content(Whitespace\"\\n\"))))(Secondary((id \
         d950c12a-dca5-4956-83eb-80941a3b2205)(content(Comment\"# ===== TESTS \
         ===== #\"))))(Secondary((id \
         b277f5d0-4dba-4097-8705-216140029a57)(content(Whitespace\"\\n\"))))(Secondary((id \
         0921d87c-eabb-48f2-bf4f-4fd675e01c96)(content(Whitespace\"\\n\"))))(Secondary((id \
         9d4055d8-23ef-4f6c-a84a-11e8df7fc808)(content(Comment\"# Regression: \
         basic harvest recording still works #\"))))(Secondary((id \
         0ad9acc3-0e59-441c-96ec-d83514eb6db7)(content(Whitespace\"\\n\"))))(Tile((id \
         614e43e0-cf33-49e2-8814-9ec8fa2f28fc)(label(hint test end))(mold((out \
         Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         4c094c61-3527-40ce-b766-9b7f87adf5d2)(content(Whitespace\" \
         \"))))(Tile((id \
         bb272b3a-131e-44a9-a5a6-065415afd209)(label(\"\\\"recording harvest \
         adds to total value\\\"\"))(mold((out Exp)(in_())(nibs(((shape \
         Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         7e58ed38-0927-4886-bbd7-81dab0712560)(content(Whitespace\"\\n\")))))((Secondary((id \
         4e697e57-40c4-4d2a-b5db-1ed0a0dd50fa)(content(Whitespace\"\\n\"))))(Tile((id \
         b4aecb8b-3ab4-4c12-a671-9ea2bfe225bd)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         7460aa96-6aac-43d4-ab8c-8d8e59c707bf)(content(Whitespace\" \
         \"))))(Tile((id \
         0212a4f8-5b8b-4be3-a72a-464f3d4af7d7)(label(h))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         bc942535-ca73-44b9-a2ee-289702db0d6e)(content(Whitespace\" \
         \")))))((Secondary((id \
         96fd2c54-42a5-4093-8517-66f62e827f64)(content(Whitespace\" \
         \"))))(Tile((id \
         b271ed05-1368-4514-b293-19d68fa30be9)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         00078004-748e-4efb-b6b5-11feb3614d06)(label(crop))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         15bc1a3f-4c84-479f-8821-813501b7604e)(content(Whitespace\" \
         \"))))(Tile((id \
         3d1e6b6d-98b5-47c9-be37-ade944e24c0e)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         bdff135e-e67e-4c12-9e61-14183a4a3dc2)(content(Whitespace\" \
         \"))))(Tile((id \
         1832c0d5-99d9-4836-b758-3a9dfab953b0)(label(Moonmelon))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         4fb139c8-6a99-4e6f-b694-ef8a78836f54)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         90d81e16-e9ea-44ec-b77d-dc0db9677c47)(content(Whitespace\" \
         \"))))(Tile((id \
         3b38d09b-bf10-4d83-ba3a-17592517dfb4)(label(quality))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         36b6d69c-c555-4fd2-a5db-e4b230363d52)(content(Whitespace\" \
         \"))))(Tile((id \
         d2267fca-7ba1-4c2e-b36b-b6190b21f1f8)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         cb1067a1-9751-4c03-8c06-a1860dd05392)(content(Whitespace\" \
         \"))))(Tile((id \
         8ad842b1-2632-4cd5-a69d-755646dfaac5)(label(Bronze))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         53c8956c-afad-4032-a0a2-e47996af5a6e)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         846499fb-f190-4b75-8fa6-3cb6243ca8a3)(content(Whitespace\" \
         \"))))(Tile((id \
         ef474d78-1ae4-4b6d-b29d-23eb2a72cc3a)(label(quantity))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         5d1d60ca-7ca9-4078-9656-ca8ec45061a6)(content(Whitespace\" \
         \"))))(Tile((id \
         a18b6bc4-b215-4e81-90c1-f708ae4d483c)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         90310c0a-d8e0-4db9-b428-ee3b5a71a17e)(content(Whitespace\" \
         \"))))(Tile((id \
         99115bf4-99de-4f42-a58f-0e1f7adbe1b6)(label(2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         d578a9c0-29eb-4927-8422-134547287487)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         ac95b747-5ca4-4532-b703-009e5fd1d8a5)(content(Whitespace\"\\n\"))))(Tile((id \
         3784d4d0-0065-4fef-98cf-ea76a238f597)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         c0edb6f2-1b35-466b-8d22-e003bb55e18e)(content(Whitespace\" \
         \"))))(Tile((id \
         13ddffb8-a64c-4e44-98d7-4f3c35239a2f)(label(ledger))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         a9922b84-4193-43ba-9d9e-5eb2481fe664)(content(Whitespace\" \
         \")))))((Secondary((id \
         0225a226-351b-4b6f-8e66-b98f8cd9606c)(content(Whitespace\" \
         \"))))(Tile((id \
         2e76b0a1-f619-4586-ae2f-d0d7f77c17c9)(label(update))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         9a4f2e1b-a664-4b24-9b77-2454b7188316)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         7173150b-ebae-4dab-a456-bea4d9958ea8)(label(initModel))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         dd267934-4502-44fa-aa81-df468182e801)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         c5b2b615-b1be-4d42-88c9-e116279b8e5c)(content(Whitespace\" \
         \"))))(Tile((id \
         f923b733-de0e-41cc-a419-7b3e500cf090)(label(RecordHarvest))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         0c10b8cb-636a-49e4-9846-9970e348abf7)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         d624fae1-848e-418a-a498-6e4041c44898)(label(h))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))))))))))))(Secondary((id \
         00876b77-fcc4-4439-b4d1-5be975502cb3)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         f786420c-ee19-490c-9b29-65397a8f1c87)(content(Whitespace\"\\n\"))))(Secondary((id \
         2e31a05d-ccfb-445f-a56a-44eb41b17577)(content(Comment\"# \
         Moonmelon(15) * Bronze(1) * 2 = 30, no streak bonus on first \
         #\"))))(Secondary((id \
         2b0da18f-c3b9-4ff8-8137-d96889275d48)(content(Whitespace\"\\n\"))))(Tile((id \
         0d62c3f0-ea72-44e1-8607-0a7040ce0633)(label(ledger))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         1005423e-5e9e-4e90-8283-71f5283c6ae8)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         2f40ee3c-63b5-429e-a065-29e842e3fe2a)(label(totalValue))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         d6c99850-6252-4bcc-bf2d-8687aa3eb1f7)(content(Whitespace\" \
         \"))))(Tile((id \
         d47b9ba4-4de3-4a65-99ad-5abf2902ade5)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         e2fd1a59-37ac-4b28-809e-97496f1123ab)(content(Whitespace\" \
         \"))))(Tile((id \
         5fdb6c06-9930-4d31-af77-e5a8b2f25e9b)(label(30))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         c4af4c14-969f-4637-86b9-38c0fd867f65)(content(Whitespace\"\\n\")))))))))(Tile((id \
         71b9f5e5-b9fc-46fa-841b-990fc873fbf9)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         ec5b867f-162f-43e5-bcab-1ce58a17c528)(content(Whitespace\"\\n\"))))(Secondary((id \
         d7de06d2-176a-4788-aeab-7bce0a8e3c6b)(content(Whitespace\"\\n\"))))(Secondary((id \
         f5e17c03-6d49-4786-85a4-629408be24f2)(content(Comment\"# Regression: \
         streak bonus still works #\"))))(Secondary((id \
         15e11cd4-6257-4bec-8673-28ae1fac58e4)(content(Whitespace\"\\n\"))))(Tile((id \
         7dda3c9c-d9b9-400f-a1c1-69ba07baf7e1)(label(hint test end))(mold((out \
         Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         febd51dd-7bc6-4198-ada0-14aaa4a28820)(content(Whitespace\" \
         \"))))(Tile((id \
         7275343e-e7dd-4114-9800-79a42bfb0d88)(label(\"\\\"same quality builds \
         streak\\\"\"))(mold((out Exp)(in_())(nibs(((shape Convex)(sort \
         Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         7bca719d-cfc7-439b-baea-d9f47cf1575e)(content(Whitespace\"\\n\")))))((Secondary((id \
         dd3d9dcd-2f5e-408d-8a82-41e47e139578)(content(Whitespace\"\\n\"))))(Tile((id \
         6578c2a4-cd1f-4ba6-8f88-1232eaa10692)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         927555a0-b6a7-4ea9-84c7-c254dc8bc42e)(content(Whitespace\" \
         \"))))(Tile((id \
         818cf7e3-bab4-41c1-8da7-c6a73be30f61)(label(h1))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         3703d0ad-79b3-4d79-8718-84dea7a80f07)(content(Whitespace\" \
         \")))))((Secondary((id \
         f780d591-6fe6-4609-8853-8ddb7765bb6d)(content(Whitespace\" \
         \"))))(Tile((id \
         893bad91-1251-41e1-9e51-52c2341fe9a9)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         6768b392-4990-4839-af72-141361c7a261)(label(crop))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         bd54b2f8-d21b-42b0-91ef-4eae99811dab)(content(Whitespace\" \
         \"))))(Tile((id \
         32cee0bb-772d-4237-821a-f5ef3b95924f)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         2632c549-ebf1-4b65-a075-bd0e9bdbefe5)(content(Whitespace\" \
         \"))))(Tile((id \
         268c80ca-26f3-4e62-84c0-27d2c52fa486)(label(Moonmelon))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         8ea9d338-12ab-4483-b60e-a57fbec6b28a)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         c8e60ed2-4a67-438e-b825-3fe085bfffe2)(content(Whitespace\" \
         \"))))(Tile((id \
         e6bbb305-8b9d-4de9-b94a-801a0bc2c707)(label(quality))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         c9abe359-30f3-495f-a745-0bbfafdc8bf9)(content(Whitespace\" \
         \"))))(Tile((id \
         3de9550e-0da2-4b7b-8692-328bb1c9ea49)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         7d020db0-89f6-42a2-84ed-6c78559b0033)(content(Whitespace\" \
         \"))))(Tile((id \
         8822d16e-032a-448e-af81-1ed5ff5795c8)(label(Gold))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         d955eafa-7941-46ba-87dd-1b038b5dbcf9)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         d0f71770-e6a1-4c7b-9bd2-d5e3f9e2640d)(content(Whitespace\" \
         \"))))(Tile((id \
         85d329b1-5924-458b-a347-77ec6a3d5952)(label(quantity))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         2a92f4d0-bc0d-4f87-8c62-e3ed8eadc28c)(content(Whitespace\" \
         \"))))(Tile((id \
         333ea245-d483-447f-b0c3-9c1c249f0286)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         976cc507-f635-482e-95e4-f8218afc2b9d)(content(Whitespace\" \
         \"))))(Tile((id \
         001c1cf9-f0e1-453c-ade6-1b96540ef335)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         7e6e7718-76b8-4fe1-99e9-5c922791d77b)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         4bb3e5f8-879e-4a9e-b114-faf7fb8450b7)(content(Whitespace\"\\n\"))))(Tile((id \
         af58e348-52f2-4b08-aa50-ba2e4e9808bc)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         629c25cc-9279-4083-ad81-862a7bc0450a)(content(Whitespace\" \
         \"))))(Tile((id \
         07125e20-f86e-4f5b-934a-f4dccee475fc)(label(h2))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         ec915a87-3207-4742-8228-8dd9fb7d04a7)(content(Whitespace\" \
         \")))))((Secondary((id \
         5683955a-9be5-4da9-8ed4-0a6bada77c91)(content(Whitespace\" \
         \"))))(Tile((id \
         b04c36d5-1ee5-4e5e-95bb-4e0951970c8e)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         35339c0b-d6b1-4b89-b786-a5bdd70918f4)(label(crop))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         9a61823d-848b-426d-9619-058ee56e3e5c)(content(Whitespace\" \
         \"))))(Tile((id \
         5460a00a-d83a-44c2-85fc-f2e96a0fcaee)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         04c619e9-d6dd-4e88-bd62-3a88056d2780)(content(Whitespace\" \
         \"))))(Tile((id \
         7a1d9b9c-02b7-4f89-bad7-618f3a2eed6f)(label(Starfruit))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         ac1ef567-b482-47dd-b6e6-3c8636642687)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         aa3e4f15-942d-4949-b067-6b56ea5b8624)(content(Whitespace\" \
         \"))))(Tile((id \
         09efb55d-12ba-4f5c-9ff1-7fddf79ab6c2)(label(quality))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         ea62bb77-8c97-4ddd-a23b-daabd353d743)(content(Whitespace\" \
         \"))))(Tile((id \
         52f863db-72e5-4f45-ba17-0e414d62f375)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         c7714cd1-07fb-47c9-870c-9dbae85503ce)(content(Whitespace\" \
         \"))))(Tile((id \
         030c289a-84ef-4573-b2f8-19153a213aea)(label(Gold))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         7a519e7b-0601-4aa4-8586-948a98389423)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         6709dd6b-d53d-4c36-a97f-4931bbc52cb1)(content(Whitespace\" \
         \"))))(Tile((id \
         93c46126-6dfb-4327-a9af-8da3c55d89a4)(label(quantity))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         f9c13cbf-caa0-4be2-b204-794f0ee1c2be)(content(Whitespace\" \
         \"))))(Tile((id \
         8647851e-0197-44bc-8b19-6deff2474c47)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         07c10e9f-e014-49dc-b3fc-a71e26890758)(content(Whitespace\" \
         \"))))(Tile((id \
         36c19183-ff28-4823-b0ce-e9970c982fcf)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         2b3a314a-18f2-430a-94ff-632be588a88e)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         5ddc7188-4077-40fc-aa56-0633f99c05d6)(content(Whitespace\"\\n\"))))(Tile((id \
         b7fadc93-f96e-447c-bdc3-8939ce57950b)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         cfd681e9-0627-4963-9645-bd4b813a4580)(content(Whitespace\" \
         \"))))(Tile((id \
         e18e825a-050d-4a41-a6d5-9697569be4b6)(label(ledger))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         f940d1af-d0d6-4637-b64a-ec4fae16ea35)(content(Whitespace\" \
         \")))))((Secondary((id \
         5e5fccd5-2cc8-49b0-941c-970d685bfe5f)(content(Whitespace\" \
         \"))))(Tile((id \
         63bb5dde-6ed9-4684-9446-8da9d0ff196d)(label(run))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         0447685f-8ee9-4d6e-9246-f28a6d447a40)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         fb60c480-fb02-44a6-8d1c-288b4c6a8125)(label(initModel))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         d2714da2-110d-4431-8523-29e97d6563b5)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         67f04393-3073-41c8-a453-effcc972ecda)(content(Whitespace\" \
         \"))))(Tile((id c98914d1-3357-4650-96b8-5ea0b542d7ef)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         3e031cbb-aa36-47ba-8230-904163394b44)(label(RecordHarvest))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         bea0cff6-3f5a-47e9-a98d-6d266f610a58)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         cb3597ff-8686-49cd-a9e9-052ff5416c41)(label(h1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         8814b597-99c5-436f-94b7-f8ce8e05116b)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         def9efa4-928a-47f4-bc3e-b44afd925746)(content(Whitespace\" \
         \"))))(Tile((id \
         0137ab0e-e73b-4141-b4de-f2d48d5159f8)(label(RecordHarvest))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         c3f4a2a0-bbe6-4e67-8372-19a3e867ee8d)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         1365e7cf-8f1e-49a5-a40d-636c77d8fc54)(label(h2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))))))))))))(Secondary((id \
         65628a7f-f750-4060-a7a3-9f3f97d888d2)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         baf8dc5a-4d19-40e4-9799-847e7f7174a2)(content(Whitespace\"\\n\"))))(Tile((id \
         c288322b-7764-476a-9c56-f089de9693f1)(label(ledger))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         9f0fc01c-1d70-49b6-8318-d813a3f988df)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         de9ea94d-2212-463e-b6ef-5f4c77d9f91d)(label(streakBonus))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         2ab73e9a-b564-4b60-b990-ff0926d7a76c)(content(Whitespace\" \
         \"))))(Tile((id \
         cb1bca7b-855a-4309-86a7-77885cb04411)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         e731c559-4aee-4e6e-8a83-b8aed2a87839)(content(Whitespace\" \
         \"))))(Tile((id \
         d762cb24-50fa-409c-b13a-43e71ced0ac1)(label(5))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         993d7884-ce52-42e8-bdb5-d6179d280d23)(content(Whitespace\"\\n\")))))))))(Tile((id \
         1fbdc4fd-3065-4c45-93b5-01965c5ee0ab)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         2c365780-420e-401c-aae0-8855dabf4444)(content(Whitespace\"\\n\"))))(Secondary((id \
         b5e650a5-d5ed-48b3-860e-bc00478c882a)(content(Whitespace\"\\n\"))))(Secondary((id \
         c7c58e3a-1eb6-4b0a-9424-41fe6d9cf2a4)(content(Comment\"# Regression: \
         claim bonus still works #\"))))(Secondary((id \
         d367aa57-ccad-4e53-a4d0-555e561030d7)(content(Whitespace\"\\n\"))))(Tile((id \
         20a2c828-ba10-4939-9d11-41967f7dafbf)(label(hint test end))(mold((out \
         Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         0e2ed9a0-7a87-41c2-9988-73f87bb8f80d)(content(Whitespace\" \
         \"))))(Tile((id \
         86023bfd-b6be-4ebe-9cae-2d82e1929df6)(label(\"\\\"claiming bonus adds \
         to total and resets streak\\\"\"))(mold((out Exp)(in_())(nibs(((shape \
         Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         eb9f5594-57de-4672-a5fe-706ee57aed22)(content(Whitespace\"\\n\")))))((Secondary((id \
         e83d8500-534c-40aa-8776-83d1b9898387)(content(Whitespace\"\\n\"))))(Tile((id \
         a25a1f62-7d55-4ee8-88ad-07b0f032e479)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         6162ed47-0a46-4031-b0c2-c02e5a4af13c)(content(Whitespace\" \
         \"))))(Tile((id \
         b989551d-cb98-446e-b151-d014ef3b67b8)(label(h1))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         0489ff1a-dc78-4e25-9828-3fe78bb10227)(content(Whitespace\" \
         \")))))((Secondary((id \
         556cb32a-25cd-4978-ac7e-93d068a9300f)(content(Whitespace\" \
         \"))))(Tile((id \
         fe7ecb30-3dea-4749-8313-352f86c24b21)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         fd0d6d58-de1c-40c6-a0cc-0c55f269d438)(label(crop))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         bfc9241c-ea96-4442-8561-eec24c61ccff)(content(Whitespace\" \
         \"))))(Tile((id \
         8b6f9a11-08e4-4eec-9d63-8a2c3e705924)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         ab628196-d298-44c8-a235-14b01e867710)(content(Whitespace\" \
         \"))))(Tile((id \
         bc62319f-f246-4bb0-b649-402fdb4f266e)(label(Moonmelon))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         3623ed28-1607-4052-b07a-aac7c3c49d2d)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         996ff2f0-f4b3-4181-beb9-4e9b1b1f33cb)(content(Whitespace\" \
         \"))))(Tile((id \
         b86e1385-d390-47dd-957f-da10ea6e388a)(label(quality))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         220e39f0-2cdd-43c5-a969-fbd253a2ef5e)(content(Whitespace\" \
         \"))))(Tile((id \
         edb4b844-8f32-43ba-b1c1-816e81367c56)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         05b83277-fbc2-4e6f-9e7b-e9d2670cca58)(content(Whitespace\" \
         \"))))(Tile((id \
         5303f397-a4e6-49c9-81d3-29a1bb83887c)(label(Gold))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         4abd00f0-a522-4221-b374-a039e2ba6ac1)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         61490227-7ecf-4478-834f-b30b57f596d1)(content(Whitespace\" \
         \"))))(Tile((id \
         f10d34f4-b167-4f04-aeed-9be7b57be48c)(label(quantity))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         d823dcb0-ff85-4a89-bcf8-e19d2304e337)(content(Whitespace\" \
         \"))))(Tile((id \
         46f81231-366e-486a-b8c0-f66050272c6a)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         4c2ace87-6c81-4920-b410-f6d2e8374f38)(content(Whitespace\" \
         \"))))(Tile((id \
         be47d553-5412-4c72-91d5-e78dbd9d6f66)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         19a4f7fe-25e7-4c36-bc0d-00424cbf9448)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         e07ffccd-9981-43d8-aea9-594ca183cd8a)(content(Whitespace\"\\n\"))))(Tile((id \
         97964825-1e73-4f43-8d76-52d4d99b3f9d)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         bcfdafcb-8cb1-40ff-b75c-faa8459380df)(content(Whitespace\" \
         \"))))(Tile((id \
         0e8da458-833d-44aa-8505-6e5f35212281)(label(h2))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         766f2448-b3b7-490e-b71a-3ec918ed3078)(content(Whitespace\" \
         \")))))((Secondary((id \
         6b792f43-7449-45aa-a6fa-99b4b2a968c5)(content(Whitespace\" \
         \"))))(Tile((id \
         c80c36a9-7ffb-478e-8c95-75e40a45d69b)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         0e1e6aee-1ac9-417a-b09f-0eccf89e3df2)(label(crop))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         bce9462c-ce16-4bbe-af6e-59e93d00994e)(content(Whitespace\" \
         \"))))(Tile((id \
         da75e992-479b-4f6c-b4d3-214c8f98d717)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         409d4b67-48b0-4cef-a6c0-d6fc5694b89b)(content(Whitespace\" \
         \"))))(Tile((id \
         77e57978-c13d-4588-9f66-f80f16549e6f)(label(Starfruit))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         1e7bd40d-a1c6-4cc3-9adb-2ee82ea5966c)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         c87d911e-0d14-40fc-a808-e75bb9a75863)(content(Whitespace\" \
         \"))))(Tile((id \
         819b5909-517e-4f37-968f-06e1c3c66261)(label(quality))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         de0f2cef-74ac-4624-97fd-eac814214f5e)(content(Whitespace\" \
         \"))))(Tile((id \
         443893c6-bb62-42c7-b960-dab645353e55)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         60f59061-4661-4975-8b8e-21e158945237)(content(Whitespace\" \
         \"))))(Tile((id \
         5109cadf-5bc3-412e-8a8c-b950a3613f84)(label(Gold))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         542524d0-08fd-4477-af4c-80106639b3b2)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         0435b964-3baa-400c-9afc-e03558a2107b)(content(Whitespace\" \
         \"))))(Tile((id \
         b09986b2-91ff-40e8-b60f-b69bac9ce756)(label(quantity))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         285ee10d-1cab-4b72-985c-edb2e1c80693)(content(Whitespace\" \
         \"))))(Tile((id \
         27a3fcd5-5d32-407f-89e3-25745ab11388)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         fcd30143-d4ed-4194-8e9f-110d959b9a15)(content(Whitespace\" \
         \"))))(Tile((id \
         986035df-d0ff-4f6f-8d85-2bc071b3b277)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         91ac0a27-95bd-40a1-afbb-312f3917ddc0)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         dd0305ac-11f7-45e6-8a80-dadca9f073f0)(content(Whitespace\"\\n\"))))(Tile((id \
         22c502ef-358b-4d38-8d27-507b0f3a3ca7)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         f1169ac4-7c98-48b7-b51f-323f68dde1c6)(content(Whitespace\" \
         \"))))(Tile((id \
         8fbbaf06-a5f4-44de-b3b8-8fe2fd7be4e8)(label(ledger))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         d9b4f1e3-355e-402e-9e02-d2c5d9859e06)(content(Whitespace\" \
         \")))))((Secondary((id \
         9d0be3fe-2bf0-4aaf-80c3-876ce9457d24)(content(Whitespace\" \
         \"))))(Tile((id \
         565cfdcc-2249-4c68-8382-573e030ca174)(label(run))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         4927fd54-f94c-47be-b39a-403f861fd7dd)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         5ee8dc34-f54b-4a55-b3c4-74075a5539e8)(label(initModel))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         b2e08551-dede-4a82-b357-8c499b496632)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         92ee152a-dff0-4e80-b67f-ab7dd6600f19)(content(Whitespace\" \
         \"))))(Tile((id 9aa40160-6d2e-4cb5-8264-81a60d852bb5)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         e7c46a60-83d8-497f-a980-fb3749f01723)(content(Whitespace\"\\n\"))))(Tile((id \
         af285aae-6fb0-4312-a611-93f5f6a0021d)(label(RecordHarvest))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         ceace30e-eda6-42e4-a958-bcbd7dc0c718)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         db26aa71-06f7-4ee5-8724-a807642c1f15)(label(h1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         b73d9ace-9e03-4d12-8bef-aebf5c98e639)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         a4524a43-08ab-4936-b032-4fd74c559d5c)(content(Whitespace\"\\n\"))))(Tile((id \
         bfdd779f-61bb-47a7-9366-375cebc2099a)(label(RecordHarvest))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         494e5e66-dd93-43cb-8c15-b0dbd639448c)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         1575f278-3720-4da0-96f7-7ddf5859b720)(label(h2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         246f2e64-1e48-4071-a932-6ef88260d130)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         9db63a88-e9ab-471b-828b-182530bc6c5a)(content(Whitespace\"\\n\"))))(Tile((id \
         26ec9e25-dfa8-41c9-a248-2d7d4e6611a3)(label(ClaimBonus))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         5fd8940c-d517-4be9-be20-d5733bbe2a4f)(content(Whitespace\"\\n\"))))))))))))))(Secondary((id \
         f7d948d6-a084-4e45-ad17-33316abe8012)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         3c5dc123-8ffe-4624-b03c-2d1920dee9e2)(content(Whitespace\"\\n\"))))(Tile((id \
         3bff2f82-b38b-4d68-a859-0b3daebaade4)(label(ledger))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         ecd3becc-c426-45ec-987e-a04341717eda)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         dae6034d-36aa-4cf7-bce1-2a1f4fe08ce9)(label(streakBonus))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         58657d5e-9eaa-437c-b9b3-e72c2fe7726e)(content(Whitespace\" \
         \"))))(Tile((id \
         7808f1e0-fefb-4666-9083-b7fd4f0c5b80)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         91ffba7d-35eb-489a-bcde-b112ff8478f3)(content(Whitespace\" \
         \"))))(Tile((id \
         9a465933-05a0-4e31-828a-acf8d9153663)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         bc5ef963-197a-4593-85e8-c01ab87d07d6)(content(Whitespace\"\\n\")))))))))(Tile((id \
         2ed83dda-338e-4afe-838f-9e209b1e8668)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         5ee057c4-aa7d-46c7-8bb3-4e54d1b7a409)(content(Whitespace\"\\n\"))))(Secondary((id \
         4aa6a2f8-ba96-41eb-91cf-8c40c1c15e8c)(content(Whitespace\"\\n\"))))(Secondary((id \
         3975a68a-ddb1-462c-af46-4c9b29fff5b0)(content(Comment\"# PremiumSale: \
         low streak gives 1x multiplier #\"))))(Secondary((id \
         91257cee-3844-4610-b136-63ddd56be182)(content(Whitespace\"\\n\"))))(Tile((id \
         8a35c036-2857-4099-8257-77ac2de9ebe4)(label(hint test end))(mold((out \
         Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         5e77a740-bfd1-4f99-a92d-15df9863550c)(content(Whitespace\" \
         \"))))(Tile((id \
         20082379-8f3f-4b73-8614-8ae04b2e9730)(label(\"\\\"PremiumSale with \
         low streak uses 1x multiplier\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         b091f1fc-ce10-4dc8-aefb-729a31134fcb)(content(Whitespace\"\\n\")))))((Secondary((id \
         d80880ec-71d2-4ee3-8d0a-eec41f003fb8)(content(Whitespace\"\\n\"))))(Secondary((id \
         2f4a8fa4-a0b5-4a62-adad-d11c500bba03)(content(Comment\"# Two \
         same-quality harvests build streakBonus to 5 #\"))))(Secondary((id \
         3aa34b6d-c49e-4d4a-b9aa-06e780c0a79a)(content(Whitespace\"\\n\"))))(Tile((id \
         38d6f668-94fd-40af-8d40-db716f6aee00)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         ee0edee3-3e51-49b6-9d2d-a848a9c81ce6)(content(Whitespace\" \
         \"))))(Tile((id \
         0c2419d2-6742-4d79-bd7b-cbcb7dd3b8a1)(label(h1))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         62ca270c-4bb7-41a7-ba1a-467661d28b70)(content(Whitespace\" \
         \")))))((Secondary((id \
         09e9d9eb-6959-4e4a-befb-4fac0237bd92)(content(Whitespace\" \
         \"))))(Tile((id \
         bc9c725a-6239-45a4-aedb-09a57bc96e21)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         904af09b-f879-4eaa-9192-d3e76ad18615)(label(crop))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         28d1387b-cbf6-420a-976f-c7220760d4f1)(content(Whitespace\" \
         \"))))(Tile((id \
         9858b227-6b27-44b9-aad9-27fb75c8cbc0)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         0c21eb61-8217-4172-9563-30b840a77d3f)(content(Whitespace\" \
         \"))))(Tile((id \
         9e41175e-59db-4a13-ba2f-93f652c23341)(label(Moonmelon))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         b2a8bebe-5619-48df-9561-ef11ae945e0b)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         19e75e2a-1689-4318-910a-0b32843d067a)(content(Whitespace\" \
         \"))))(Tile((id \
         5afe7454-1957-47f2-a134-a9e04d3ac350)(label(quality))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         687abbd1-a3a8-4aa5-8a04-5b6d75a15cc7)(content(Whitespace\" \
         \"))))(Tile((id \
         33d504be-d0e6-484a-a350-ca0bd9f24fd3)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         bcc103a3-2d16-491d-86c2-aed70be54ce8)(content(Whitespace\" \
         \"))))(Tile((id \
         e10bb234-9d61-4b6c-b0a8-eab8a752437b)(label(Silver))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         6927d27b-b515-4623-9fff-6390a2bf4610)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         80645840-781a-4ec6-a18e-a1dbd39693be)(content(Whitespace\" \
         \"))))(Tile((id \
         4c828243-3849-44c9-bd9a-eb4940b25d4f)(label(quantity))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         f663ec7f-9705-4252-bdf8-0b12913e9c3a)(content(Whitespace\" \
         \"))))(Tile((id \
         46ab7201-3eaa-427a-adfa-de97f907cd54)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         c660daf1-e12a-46dd-b3ea-c16b561d828b)(content(Whitespace\" \
         \"))))(Tile((id \
         e03d6c0c-ae7f-48fd-a260-d59c3b795f86)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         4c45fd2f-6cba-4479-8f92-f15b3b9382aa)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         79acb81e-fed7-46a0-8bb9-4854baa1968b)(content(Whitespace\"\\n\"))))(Tile((id \
         0df60eb5-35e3-4aec-b7f3-25427cf0621a)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         aed1698f-618f-41cd-a7dd-f79b23b85425)(content(Whitespace\" \
         \"))))(Tile((id \
         4286bb75-0d98-4484-82ff-115503fd4577)(label(h2))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         0c1db74b-f1fe-483e-be9b-7248f8e41887)(content(Whitespace\" \
         \")))))((Secondary((id \
         13ce16b3-4413-4f47-bc27-716bdd49f80b)(content(Whitespace\" \
         \"))))(Tile((id \
         0832fd0c-6055-40ad-b186-95c509395577)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         d42d20b4-d795-4eb5-941c-fd1a96b7c72b)(label(crop))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         90c6261b-92c8-4177-8216-b2d52e356e56)(content(Whitespace\" \
         \"))))(Tile((id \
         e44f9340-22c6-428a-8869-5c4a7a82bf26)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         3c3423f2-7722-4185-a4be-99ec48c240c7)(content(Whitespace\" \
         \"))))(Tile((id \
         7e05cf9b-ebf1-497d-a77b-117678bac1ef)(label(Starfruit))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         022290c9-0870-41b2-8861-0b94e90671fb)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         c2a8e003-d0b0-4305-a9c0-5cb166f908c7)(content(Whitespace\" \
         \"))))(Tile((id \
         85648bb5-3e10-4010-af28-741c1109a655)(label(quality))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         83df96ee-68c2-4af2-96de-13cb8f7432fd)(content(Whitespace\" \
         \"))))(Tile((id \
         52cb1695-7e22-4bba-b386-e43510bd5b2e)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         5ca14062-4a81-42e3-91e7-75a83087f126)(content(Whitespace\" \
         \"))))(Tile((id \
         e6f45880-841f-4730-a261-f9c13d755cec)(label(Silver))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         cb1d7064-3142-4f60-995f-46de6d1e6635)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         bfe5ade1-2eda-48b1-bcf3-4a94205f79f2)(content(Whitespace\" \
         \"))))(Tile((id \
         2d24e17f-6a09-4a30-abf8-5f0cde0a2401)(label(quantity))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         320602d7-9a10-4de2-838a-f4bcfb9c3bf5)(content(Whitespace\" \
         \"))))(Tile((id \
         596a5471-3b21-464e-b684-824cabbf9a06)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         b9fa5b55-a035-4bb7-8e29-1e213e6fc57e)(content(Whitespace\" \
         \"))))(Tile((id \
         9d499597-a13b-45c4-97bd-18197e25a8cd)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         1d151acc-4359-4ca5-b485-b12b554c2c88)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         0df062a0-98c3-4f20-ad6f-6df906668c3a)(content(Whitespace\"\\n\"))))(Tile((id \
         4d5646dc-782f-4026-95a9-7e21725fd1f7)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         f4674cbb-12da-4d64-b161-9682eacc9db0)(content(Whitespace\" \
         \"))))(Tile((id \
         a503c3b5-3e08-4afd-81bc-3451bc9b914c)(label(ledger))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         a91d155d-34ac-4d6b-8c18-f656d5276418)(content(Whitespace\" \
         \")))))((Secondary((id \
         2d1b0881-3c2e-4d44-b08b-23a0bca7223b)(content(Whitespace\" \
         \"))))(Tile((id \
         2074f7a0-c879-4f45-8f4b-b25a88c7ee9b)(label(run))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         76163300-5bb1-47da-b0df-3ba9a820cb04)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         009e4000-67d2-480d-a304-76bf24cced0a)(label(initModel))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         0fd1ac08-043f-40a6-a15d-625dee86bc5d)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         846f3a3a-83b6-4fa6-8a39-ca707b620b6a)(content(Whitespace\" \
         \"))))(Tile((id 646e9698-917b-42c6-a6ec-1ad09e4ce6f4)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         6591f027-043a-4bf6-9d44-ad1715896f80)(content(Whitespace\"\\n\"))))(Tile((id \
         1da8930e-bffc-43d0-8634-424a80877c1f)(label(RecordHarvest))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         a8ef2647-7962-41a2-9e81-41551834f035)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         d4516d2a-50fe-4dee-ad56-5fbcc4592309)(label(h1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         3e7e3949-9be7-4fa7-892b-3a0d08da5be0)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         2c77783a-34da-4628-9b83-d74eb77baeb8)(content(Whitespace\"\\n\"))))(Tile((id \
         98e1fb66-9be9-4dbb-b8ab-b1895bc13194)(label(RecordHarvest))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         a0486313-8e35-4f04-9f8a-824fad98c90d)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         f1418d7f-7c41-42cc-b52a-412bd57e7e80)(label(h2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         8ebf363f-68a4-4df5-917b-8df0ea08aba3)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         2cb17745-d01d-4072-8a08-637b2f545555)(content(Whitespace\"\\n\"))))(Tile((id \
         54acf642-d4ea-4067-9721-5fe148d4b069)(label(PremiumSale))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         3402415b-06ed-47c4-a4d5-c04b4b1704fd)(content(Whitespace\"\\n\"))))))))))))))(Secondary((id \
         700714a5-eef6-47bf-b213-3da14f37031d)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         72af3199-4651-41a9-b5b7-4afd5b49bbef)(content(Whitespace\"\\n\"))))(Secondary((id \
         cb3b8135-22c1-43a1-86a0-dfe990b0d221)(content(Comment\"# streakBonus \
         was 5, multiplier = 1, payout = 5 #\"))))(Secondary((id \
         81293f41-fc16-4e4c-8302-012803247202)(content(Whitespace\"\\n\"))))(Secondary((id \
         9a1539ab-a156-4012-8eda-ab9cf7fbaf2c)(content(Comment\"# h1: 15*2*1 = \
         30, h2: 20*2*1 + 5 = 45, PremiumSale: +5 #\"))))(Secondary((id \
         e5fbe5d2-f806-4f0f-8857-125875b72d6d)(content(Whitespace\"\\n\"))))(Tile((id \
         b3236ab2-f36c-4cef-b57a-243bec960125)(label(ledger))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         8efc316b-166b-41d8-823c-6e5d8a144919)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         bc36daec-26ca-49f4-bd6a-55b5c2bfb821)(label(totalValue))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         6289fccf-d881-41ef-b085-e175a649de2f)(content(Whitespace\" \
         \"))))(Tile((id \
         63f94ddc-a0d3-457f-9dae-45c3320332fb)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         219e1aac-102e-4f57-976b-e5cd6d910e86)(content(Whitespace\" \
         \"))))(Tile((id \
         a1759785-bc49-4b31-bc63-31e581e33cf9)(label(30))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         2a66d5c6-7c54-476a-b5d2-83d7d2416080)(content(Whitespace\" \
         \"))))(Tile((id \
         5ae5b4ff-83da-4c81-b16b-c16a76b9e155)(label(+))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 28))(sort Exp))((shape(Concave \
         28))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         88bf8310-7e41-4c5d-adc9-6b79845a0da3)(content(Whitespace\" \
         \"))))(Tile((id \
         cb7d8553-1fa0-4b98-9ac9-360e11fe0094)(label(45))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         76f98544-e5bc-4d57-aa80-8644c8818d16)(content(Whitespace\" \
         \"))))(Tile((id \
         390119d1-1a7a-4911-ae6b-9ad1512de60c)(label(+))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 28))(sort Exp))((shape(Concave \
         28))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         00f179c8-d2b5-438d-8f78-5db7896eb1e6)(content(Whitespace\" \
         \"))))(Tile((id \
         1cc4bbfc-94a7-4a56-8799-222e339da49e)(label(5))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         3e8c75bd-57c1-4da8-a7ee-7d8d2fe5ab53)(content(Whitespace\"\\n\")))))))))(Tile((id \
         b8011795-bcd6-47ae-a595-cc0e871d3ee0)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         14c6dd43-e789-4eba-b6e2-57d03dea0ea6)(content(Whitespace\"\\n\"))))(Secondary((id \
         bc62ece7-3f05-4608-97c8-0686e5fb981e)(content(Whitespace\"\\n\"))))(Secondary((id \
         13562f51-c9ff-4ea2-870f-1ab18cf908b3)(content(Comment\"# PremiumSale: \
         high streak gives 2x multiplier #\"))))(Secondary((id \
         5dbe6e24-66f8-4adb-b35f-232c641ef45e)(content(Whitespace\"\\n\"))))(Tile((id \
         8f441876-e1f6-43f2-be7d-a6423cfbe5cc)(label(hint test end))(mold((out \
         Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         9eccea41-3a20-4edd-bef3-85b8ba5cf1f3)(content(Whitespace\" \
         \"))))(Tile((id \
         1fc5ad1d-7893-48c2-be36-afe5e40d8752)(label(\"\\\"PremiumSale with \
         high streak uses 2x multiplier\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         5c95b91b-a327-4568-8524-580d3d5248a9)(content(Whitespace\"\\n\")))))((Secondary((id \
         6e092c75-a706-4a75-9704-d77db21a0186)(content(Whitespace\"\\n\"))))(Secondary((id \
         8c729ab3-a3bd-4186-86bc-52fbfac0e0e4)(content(Comment\"# Three \
         same-quality harvests build streakBonus to 10 #\"))))(Secondary((id \
         4a29af19-4700-497e-8fff-c536e9c00ef7)(content(Whitespace\"\\n\"))))(Tile((id \
         a311d189-a34c-4d68-823c-c75ee02d94f7)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         9fa898a3-43f5-4440-a07f-224c92db45aa)(content(Whitespace\" \
         \"))))(Tile((id \
         aebb4e64-437e-49c6-8633-83ab7571b477)(label(h1))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         c8b52884-ccc2-4a10-a25d-014d4e8ca895)(content(Whitespace\" \
         \")))))((Secondary((id \
         adc03e20-ec92-4eb8-a1fa-f7778888737a)(content(Whitespace\" \
         \"))))(Tile((id \
         6627c77f-6f1b-4cfa-a71c-5e7942f749ee)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         06622a2d-c9f2-4e8b-a38f-dd8f81e92b93)(label(crop))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         8dba2786-9602-4052-87dc-ae7e7e7aa074)(content(Whitespace\" \
         \"))))(Tile((id \
         82ad118e-605b-455b-bb5d-9cd0696b5961)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         d2f1eab0-dbc9-4d30-a0ec-51e326fa0aba)(content(Whitespace\" \
         \"))))(Tile((id \
         f860dfe8-f7db-4ec3-9ac8-48418654aeb6)(label(Moonmelon))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         e365802b-8306-4f46-8f5a-460284808cd9)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         9e1f7621-086b-4f41-93a8-2cb1db36d6e5)(content(Whitespace\" \
         \"))))(Tile((id \
         9cfca1e7-7090-416a-92c9-0028ae9db9b9)(label(quality))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         3e035afe-d5a8-419a-8ae7-72d233a5650b)(content(Whitespace\" \
         \"))))(Tile((id \
         f438f8c6-88b0-4f15-ba92-33ae4a558174)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         66ec68c8-0bc4-4915-8cbf-f2bce0e51ceb)(content(Whitespace\" \
         \"))))(Tile((id \
         c3185a4b-3e2e-4783-870a-44bee9a595f4)(label(Gold))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         2b8af904-040f-4994-a1d8-e9db5410cdb5)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         f7670880-496f-44cd-a09c-b14bbd35f1d2)(content(Whitespace\" \
         \"))))(Tile((id \
         d7f53c1a-3e00-47ae-91f8-7781d7042799)(label(quantity))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         508e5934-647c-4845-aa1c-fecf418a1b70)(content(Whitespace\" \
         \"))))(Tile((id \
         100bcbf0-8857-4318-aa8c-66d69ebb12f0)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         53be62d9-439e-45bd-aa89-37a1b020e991)(content(Whitespace\" \
         \"))))(Tile((id \
         8e74a508-9f7e-444c-a35f-4dd759a0b058)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         6bc4ad88-16e6-4d73-80e6-60d9e9dd5ab7)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         0c207a4d-c3cd-40f4-930c-a89699dcd690)(content(Whitespace\"\\n\"))))(Tile((id \
         416c6aa5-a5f2-4f66-9ee5-249d9aed515c)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         68bb16ac-712d-42ba-a982-ba60541562f4)(content(Whitespace\" \
         \"))))(Tile((id \
         21207b02-e85f-4a0f-bdc5-74372da07b3e)(label(h2))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         dbde49c0-0f98-4d77-b611-9ba424787694)(content(Whitespace\" \
         \")))))((Secondary((id \
         f3770e95-fb74-47aa-ba72-262a77f35313)(content(Whitespace\" \
         \"))))(Tile((id \
         4f7413ac-e850-400c-ac1f-5ccdb85281ac)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         0c070cef-802f-4662-9d10-f9c7df89f892)(label(crop))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         71fa4660-2c04-4d54-900e-de4b047f721c)(content(Whitespace\" \
         \"))))(Tile((id \
         bd80511f-8ae5-4ddd-82a3-066871b17e3a)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         11e95914-2e0f-4147-a26c-6ab4af32d438)(content(Whitespace\" \
         \"))))(Tile((id \
         42a044a9-5256-4dce-be66-4fcdf4e97167)(label(Starfruit))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         afe7021f-3131-40cf-8ba3-3f268b6a6ce1)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         2e54e505-c7dc-4122-a873-a8783b463419)(content(Whitespace\" \
         \"))))(Tile((id \
         bd8a2432-3993-4124-b167-cf0e1888b457)(label(quality))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         ccc9a3ca-05ff-494e-bf25-b31e1319b4ad)(content(Whitespace\" \
         \"))))(Tile((id \
         6c502c97-f495-40f6-bdb7-cf7ca1a93bff)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         02541b12-32aa-458d-afd0-1628f4d7fe5a)(content(Whitespace\" \
         \"))))(Tile((id \
         a9b167b8-cefc-437d-aedc-b84a46ff4482)(label(Gold))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         df928ed0-30c9-4bc1-8749-01773ba12247)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         9562530c-4f24-4fdf-a2ea-11d2b1857c69)(content(Whitespace\" \
         \"))))(Tile((id \
         18f37f1b-07d4-47b4-8750-7e34752a5b20)(label(quantity))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         410f5292-3612-454e-8556-2280be71ef95)(content(Whitespace\" \
         \"))))(Tile((id \
         78536321-1251-4712-b147-bf862224f515)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         17bb3f5f-01d8-4976-92ac-aeeebdb6f303)(content(Whitespace\" \
         \"))))(Tile((id \
         09c0f77f-4892-4515-9677-5724dd6ebf7f)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         449271d9-91ba-4163-bec7-10addf873813)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         06e5a754-3db6-4ee6-b1bd-ee4034020e04)(content(Whitespace\"\\n\"))))(Tile((id \
         070db9a4-b537-41f5-9e65-eb36cd31b11d)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         ea5e3fe2-443d-4d4e-8e2d-cc6c260c18a6)(content(Whitespace\" \
         \"))))(Tile((id \
         9b6236fb-ba13-4f26-bfe1-97b81bf4d531)(label(h3))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         ecfecbdb-06e0-45bf-b4c5-639fe77c9cbb)(content(Whitespace\" \
         \")))))((Secondary((id \
         5b613b9f-833b-4082-a690-99c8a62253c3)(content(Whitespace\" \
         \"))))(Tile((id \
         e24499ba-4d69-4fc8-bbff-648c03ccd225)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         7446dee0-4b72-459f-856a-f3225c72a138)(label(crop))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         0b8b0f07-d42e-46e6-9bc4-130ad6502dfc)(content(Whitespace\" \
         \"))))(Tile((id \
         be720f6e-0cc6-4ee5-b35f-d5971dd1987d)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         fec40920-01c6-4651-b2d5-6b8855a0107f)(content(Whitespace\" \
         \"))))(Tile((id \
         b2e96e03-134c-483a-aacd-903f9931c731)(label(Nightshade))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         42d8c18b-46f1-43e5-86db-7be3b68501d7)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         7d43a057-8aeb-442e-af55-0d59fa6b9f6b)(content(Whitespace\" \
         \"))))(Tile((id \
         5c5c67d8-1d64-48da-8b94-e78763568e21)(label(quality))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         d6db426d-08e6-49cf-b87b-3d144e994559)(content(Whitespace\" \
         \"))))(Tile((id \
         f54ea066-4335-48a0-8ff6-dbdb60463eb7)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         388eea1e-49d4-42c4-b4b4-4f0beb57e707)(content(Whitespace\" \
         \"))))(Tile((id \
         2bce7cc9-2e0d-4e01-88e7-a6ce3cff015c)(label(Gold))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         4e745bd2-5c6b-4cc5-84ab-aaf7e7767935)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         e91330e3-62f5-4858-ac6f-43b7026ec3c1)(content(Whitespace\" \
         \"))))(Tile((id \
         26b283eb-38a4-4c5c-b1f7-026ab93fb3e1)(label(quantity))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         56545267-9b4f-461d-a128-81173144ec7c)(content(Whitespace\" \
         \"))))(Tile((id \
         9e656950-12d4-4250-8400-84ad99246311)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         5f269205-8114-4048-828f-39680edb9095)(content(Whitespace\" \
         \"))))(Tile((id \
         6580cba9-bf11-412e-bdc6-e38532321768)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         0729fc47-b32e-4c06-ae27-9a30771cd827)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         f65d8ee9-4449-410a-a038-8efcd03d87e2)(content(Whitespace\"\\n\"))))(Tile((id \
         a4580ed7-fe6a-45cf-ab73-2027f0f632a4)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         8153549d-aec5-497d-992e-112182b4db48)(content(Whitespace\" \
         \"))))(Tile((id \
         cc66eb20-871c-4828-bc58-57490f330435)(label(ledger))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         8f7fa221-33d6-4278-974d-1bbfb50d2340)(content(Whitespace\" \
         \")))))((Secondary((id \
         ea67b411-423e-4e5d-816a-c613511d9d02)(content(Whitespace\" \
         \"))))(Tile((id \
         bc066a5f-3d49-48bb-9e09-b8409d1714ef)(label(run))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         4cdee418-52f9-462b-a348-204015aa25a0)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         400425a5-0047-46f4-afd3-eb1dbb04c3b1)(label(initModel))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         5f67bdd5-ca64-4059-8926-0120afc0ea03)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         5a7bb79c-08fa-4b00-87f1-9920088c84f2)(content(Whitespace\" \
         \"))))(Tile((id e40e1203-ef9d-47f3-b522-52b8a22eb185)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         0ace9625-dd36-4f08-a0bc-77cd777617ae)(content(Whitespace\"\\n\"))))(Tile((id \
         d2bffb81-e969-488a-a8d0-73707ccc7701)(label(RecordHarvest))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         c094768d-1e3d-48e9-bcbc-c2e187bb5f0a)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         6873a7e2-3beb-4505-b25a-8173c634ffeb)(label(h1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         0b2e6628-168c-476e-bab8-6abc2e560489)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         bb7708f8-0fb6-44ee-9321-212ae88081da)(content(Whitespace\"\\n\"))))(Tile((id \
         0fbd7852-6764-4db4-9ff6-af4def66f805)(label(RecordHarvest))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         b04e1f09-e637-4713-a69d-8cf81b8c260f)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         7efe5c1d-82d7-4009-af22-88aaec82246d)(label(h2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         c4221697-8bf0-4184-90c0-e181d87d0d23)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         02b47ff4-d249-433c-b9ce-a05d00f60d3d)(content(Whitespace\"\\n\"))))(Tile((id \
         2657d414-8b25-4dd9-983c-d6934830acbc)(label(RecordHarvest))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         98a205ce-ec69-4baa-9155-626cc457d653)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         5099dbeb-673b-4bd8-a858-2ebf27c6e5b0)(label(h3))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         654304e3-2721-4a0c-b4f5-0ecea85128c7)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         e5675c64-7106-4493-b2ea-e2bd6335d6c1)(content(Whitespace\"\\n\"))))(Tile((id \
         7e2344d9-9d90-4458-ba08-958837fbd5e1)(label(PremiumSale))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         aeeb6334-50d1-4509-a420-17d80742c81d)(content(Whitespace\"\\n\"))))))))))))))(Secondary((id \
         e787ca07-c5f5-404d-bae8-61aff344d5ec)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         7d131547-56c5-4175-9995-37eab7769809)(content(Whitespace\"\\n\"))))(Secondary((id \
         72e9b921-2c4a-4ba1-a47d-10852ad4682f)(content(Comment\"# streakBonus \
         was 10, multiplier = 2, payout = 20 #\"))))(Secondary((id \
         822012a5-8a73-4314-b513-1301f67f8d40)(content(Whitespace\"\\n\"))))(Secondary((id \
         ba5cb5f1-4c7e-4bba-9903-52f61a9de7cc)(content(Comment\"# h1: 15*3=45, \
         h2: 20*3+5=65, h3: 20*3+10=70, PremiumSale: +20 #\"))))(Secondary((id \
         8b4c8168-bd5f-4dec-b9d2-7cd1aba0c356)(content(Whitespace\"\\n\"))))(Tile((id \
         1786af96-18c9-4e3a-860a-09e1037d57ff)(label(ledger))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         fa8ce4fc-16fa-45ef-9a2f-da2932014033)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         9d188155-55f7-4da0-919a-2a9ae0c1f623)(label(totalValue))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         abf6396f-e8a2-442e-9cbe-e4db7d05d9a5)(content(Whitespace\" \
         \"))))(Tile((id \
         110c422e-948c-4d77-8ba2-0a1bfc532129)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         0a0348ff-fc98-4456-a42c-e8afdea50f76)(content(Whitespace\" \
         \"))))(Tile((id \
         099182dd-56be-4747-bc0f-018f2e7dcfd9)(label(45))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         c3de6d3e-42f0-4488-a283-1f2e78a365f1)(content(Whitespace\" \
         \"))))(Tile((id \
         c238e89e-00ca-451b-a380-ec9e856efd23)(label(+))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 28))(sort Exp))((shape(Concave \
         28))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         5e609c9c-e5d1-4b5e-beed-70a88622c881)(content(Whitespace\" \
         \"))))(Tile((id \
         93ca4669-888d-4b9a-98eb-c2102ce8ffb9)(label(65))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         420a7249-7138-44c9-ac8c-99c84adbf80a)(content(Whitespace\" \
         \"))))(Tile((id \
         876491d4-b065-4aee-831a-bad36656f667)(label(+))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 28))(sort Exp))((shape(Concave \
         28))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         ca61d8cc-8293-4b88-81d8-1d0c83ec7fd8)(content(Whitespace\" \
         \"))))(Tile((id \
         5059e622-5493-47a3-8cb8-d2fad9797233)(label(70))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         7f6b8ae8-9fb7-46e3-bf87-6648828dc7e5)(content(Whitespace\" \
         \"))))(Tile((id \
         0fbafb4a-f0da-4bfb-9e03-e45e316980a5)(label(+))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 28))(sort Exp))((shape(Concave \
         28))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         633d32b2-fccc-49d9-8cd9-894efd770320)(content(Whitespace\" \
         \"))))(Tile((id \
         6e54769f-a614-404b-9249-956804b31c39)(label(20))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         5a101410-8594-40ec-829d-48bb27ce2c1c)(content(Whitespace\"\\n\")))))))))(Tile((id \
         f32a7c0c-fa59-4aea-8219-f19c05938a7a)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         eb2bf1c7-663e-44e4-98cd-975d249d835a)(content(Whitespace\"\\n\"))))(Secondary((id \
         90708fd3-5ced-45ca-84f6-a2830b26e2be)(content(Whitespace\"\\n\"))))(Secondary((id \
         e72179c3-1709-43d0-adce-1e82fd59218b)(content(Comment\"# PremiumSale \
         resets streak after claiming #\"))))(Secondary((id \
         ba575936-5e2f-4086-9ca4-56009bdd0920)(content(Whitespace\"\\n\"))))(Tile((id \
         bfcef0e5-7670-4825-8b37-3cb7d62752a4)(label(hint test end))(mold((out \
         Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         aad738c8-b406-4c34-adf7-42819744e4f4)(content(Whitespace\" \
         \"))))(Tile((id \
         569b7b52-741a-4158-8f3a-2f9b5cbdda2c)(label(\"\\\"PremiumSale resets \
         streak to zero\\\"\"))(mold((out Exp)(in_())(nibs(((shape \
         Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         29b78ae4-3644-49ce-b2f9-c07f19bc61d2)(content(Whitespace\"\\n\")))))((Secondary((id \
         859b4826-5cbe-4c43-96eb-553c8f50eb83)(content(Whitespace\"\\n\"))))(Tile((id \
         24028c25-adcc-4cc1-ac14-d1d4bfe0e94c)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         565f6b13-2d05-4b54-9c10-e9f5caecf8d4)(content(Whitespace\" \
         \"))))(Tile((id \
         d554c4b7-0a6d-4579-9f62-0323b3746282)(label(h1))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         f41b505d-1bb7-4f6a-be0f-4a53f5267542)(content(Whitespace\" \
         \")))))((Secondary((id \
         1f71fa8d-56e3-4227-934d-97f3d82b56e4)(content(Whitespace\" \
         \"))))(Tile((id \
         474ab526-7de1-4b4b-89d4-56b576f17eb4)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         cde8dda1-f5db-4f9b-a7ab-688350dfb4ab)(label(crop))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         16ec5545-701a-456b-866d-8fd768e79cbf)(content(Whitespace\" \
         \"))))(Tile((id \
         49ba59e7-5cc9-46a4-8697-ece6517d3f43)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         65153a47-d831-425b-a7f4-e4f8d33ba734)(content(Whitespace\" \
         \"))))(Tile((id \
         144630d8-0d99-486a-b723-86a687795c44)(label(Moonmelon))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         949eaa06-c77c-48ff-9b02-63570e8b8f26)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         65424792-a53d-4cc4-9ec2-6654326f31f5)(content(Whitespace\" \
         \"))))(Tile((id \
         109570f1-1598-4800-8174-c051e8de2f23)(label(quality))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         cf7e80ab-1066-4c63-ad84-4ba2ac20994b)(content(Whitespace\" \
         \"))))(Tile((id \
         cc8c3bdd-9009-4506-8d65-9259a5eb0ccc)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         1957fee6-d1f3-415c-9dac-2ca019a1f910)(content(Whitespace\" \
         \"))))(Tile((id \
         d87c7290-4718-404b-ba64-92e7e5700446)(label(Gold))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         e03af605-4853-4323-a01d-4913dfb84667)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         e1d4e988-da42-4068-92e9-a1eae5faa8d4)(content(Whitespace\" \
         \"))))(Tile((id \
         872c6ad3-bff3-4b91-acde-a0103bed80e9)(label(quantity))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         dc775280-03eb-4b20-8d99-b9e6b2be23c8)(content(Whitespace\" \
         \"))))(Tile((id \
         365a46fa-6eb3-4410-b1d6-16acf4afe23e)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         84a03a37-2a08-43aa-beba-05c828c9801f)(content(Whitespace\" \
         \"))))(Tile((id \
         90c0ac46-0120-49a0-bdcc-d0c54d58bfb0)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         866a021b-84bf-4458-a897-09a893d817cf)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         3c2ee18d-96c7-4085-9cb3-6ab00829503d)(content(Whitespace\"\\n\"))))(Tile((id \
         27a6aca7-4131-4215-8029-10402dc32812)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         fd469480-de37-4542-960d-9df7fbc0b0c8)(content(Whitespace\" \
         \"))))(Tile((id \
         cc212ff8-e893-44f8-9a56-09a2e895f749)(label(h2))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         6015b375-ca99-4220-b0b8-1a09b8a8e338)(content(Whitespace\" \
         \")))))((Secondary((id \
         a910f68c-0c05-4ea5-bb6f-f65b72d0a28b)(content(Whitespace\" \
         \"))))(Tile((id \
         a1a659d3-2bc5-4168-b9f5-3f294f64c11b)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         7e547035-a5ac-4bad-92cb-c62b7c4fa4fe)(label(crop))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         ce97e61f-5a6f-4d6b-a47e-663a58cdbffb)(content(Whitespace\" \
         \"))))(Tile((id \
         6038b460-ef5a-4cb4-a492-d8b4b29f8821)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         dfa32b44-4351-4604-bbea-a75c5a0a9a3e)(content(Whitespace\" \
         \"))))(Tile((id \
         eec019d3-3ab7-4d5a-828a-fc10fdafff6d)(label(Starfruit))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         2aad6640-7ab2-428f-ac9b-b38480aca9a2)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         091ca4f3-ac3c-4565-a7d9-71063382e3b0)(content(Whitespace\" \
         \"))))(Tile((id \
         0b82534c-62e0-48eb-a1f0-17162d3c6515)(label(quality))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         6c4e1530-44ad-49a5-a939-d62673953a3a)(content(Whitespace\" \
         \"))))(Tile((id \
         808be878-2a3e-45a6-9856-046769371cf1)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         3659b21e-03ce-4a08-acf4-f79da37d893e)(content(Whitespace\" \
         \"))))(Tile((id \
         b9676495-ccda-4f83-96fc-739cb8d39cc1)(label(Gold))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         6377adf6-3e66-437e-b89a-dbcec6a0be5d)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         f7c67006-6baf-47c0-bb06-2db5f405208b)(content(Whitespace\" \
         \"))))(Tile((id \
         c38b070b-4384-4776-a6a5-80a2c2ea0ec5)(label(quantity))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         a6d8cac7-897e-4d72-b2dc-3c26eb164fa2)(content(Whitespace\" \
         \"))))(Tile((id \
         9d65f608-27f4-4eca-96dc-a12ce867c9ca)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         e614c370-1c01-4968-bf6c-5634f0e074fa)(content(Whitespace\" \
         \"))))(Tile((id \
         b800f4c8-4a09-40de-bcb4-0cdb7dd82ff9)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         88ebdd75-055e-436a-8c01-3032ef0105a2)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         a82dfb74-b07a-45d5-be34-918648e93675)(content(Whitespace\"\\n\"))))(Tile((id \
         2e1c3e4d-e309-4364-9ab5-73609865ecdf)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         a6e0a15d-27e9-4fc6-96fe-09873b5b9bfc)(content(Whitespace\" \
         \"))))(Tile((id \
         e66ffd0e-52bd-4ae6-90cf-ba005d3a5066)(label(h3))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         57697c7e-1586-4c13-9b8a-aee377ac1a90)(content(Whitespace\" \
         \")))))((Secondary((id \
         7cd6b747-763d-4cc2-b7d7-94168e6204d2)(content(Whitespace\" \
         \"))))(Tile((id \
         48b4bbeb-5ccc-402a-8d30-e50da5a24036)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         5624b889-1a54-44b6-8817-5defa42882d3)(label(crop))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         ac1deb33-4c14-4c87-8cbb-0182e784307f)(content(Whitespace\" \
         \"))))(Tile((id \
         817f3783-e914-440f-b4c7-9455c17d43f1)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         b009087c-01bb-4454-91e2-d868fdc02356)(content(Whitespace\" \
         \"))))(Tile((id \
         63396001-8cf5-4bca-84a1-248b156faf69)(label(Nightshade))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         1789aed7-7d2d-496d-b4f9-f793a022b21f)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         ffebcb62-9a31-4c41-9dfd-f6629490c269)(content(Whitespace\" \
         \"))))(Tile((id \
         4b6f5179-0472-4a90-8131-c99e1e90efbe)(label(quality))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         b0386a85-5842-4a44-8770-171513d51fae)(content(Whitespace\" \
         \"))))(Tile((id \
         5944d25e-9dbc-4e3b-ace3-a17d422af37e)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         7d24d6d1-1187-430a-a6da-f8002dc1b286)(content(Whitespace\" \
         \"))))(Tile((id \
         cf6292b7-f590-41a0-905d-fdea247dcd4d)(label(Gold))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         0df2910a-400b-47ee-b3b1-ced51bd04a87)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         99020a1f-7c77-4a22-97c9-f307901c542c)(content(Whitespace\" \
         \"))))(Tile((id \
         0542f3f5-2872-4f9b-9e27-f6e24c83c97a)(label(quantity))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         d067abdf-5631-4f10-9496-71541836a6cc)(content(Whitespace\" \
         \"))))(Tile((id \
         532ddd7d-adf7-4a7f-b2ba-c7b18c3a531a)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         0ae10da1-1700-4eb0-9cd8-0a82ecf55a82)(content(Whitespace\" \
         \"))))(Tile((id \
         de18f916-9f93-47b4-9a1b-c5934b35870f)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         2237bdef-c1e7-4931-843b-2e0406c6d53a)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         716319f8-319e-42a3-8008-065958e75195)(content(Whitespace\"\\n\"))))(Tile((id \
         4bb5f715-a7ad-4ad4-bae1-1750d109f76c)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         07f387fa-0f52-4abe-8e94-dc0744c57735)(content(Whitespace\" \
         \"))))(Tile((id \
         25d39773-e996-4090-b2f8-2072120e5db6)(label(ledger))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         1abf5b24-2e87-4965-a460-8f6d0d3fdd05)(content(Whitespace\" \
         \")))))((Secondary((id \
         e8d13ba9-7920-43e0-9137-f53a9a5e9f5d)(content(Whitespace\" \
         \"))))(Tile((id \
         94c621e1-7f7e-4369-9873-443153d7161c)(label(run))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         78841b93-9e29-41ce-bba7-9aebcf2a3b69)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         1863e424-7f90-4e6e-883c-cf0db584c6f8)(label(initModel))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         6a917212-200a-495a-9160-0072841d67bb)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         af0cc549-1651-454d-a007-cfb0da093dd3)(content(Whitespace\" \
         \"))))(Tile((id 0d799d4e-ec9b-4e8f-bd8c-dd64b11f6b3f)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         c8f9be3c-5289-48dc-ac78-3000b1510f76)(content(Whitespace\"\\n\"))))(Tile((id \
         77b832a2-e6af-48f0-b7b9-a3ec058cd081)(label(RecordHarvest))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         63cd1c33-62e8-49b8-9b8e-a33cf07b7a51)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         1aa3a5dc-b8de-4403-9694-569e10cd2847)(label(h1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         8e58717d-e47b-4f57-b25b-6a8dc0bf49ba)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         bded1b28-55e5-4a5f-ab01-29ee8d7c7833)(content(Whitespace\"\\n\"))))(Tile((id \
         0b376425-c827-4a93-b2be-e9ff56bca22f)(label(RecordHarvest))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         feed92d4-8865-49b2-a822-92fd6690f12d)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         cbc88e54-0230-445d-aab5-df9782e4620f)(label(h2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         17976bdc-a1e9-47ff-b0f4-bdff055edcb8)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         c87876b1-88c4-414d-a006-4768f0141e17)(content(Whitespace\"\\n\"))))(Tile((id \
         3251adb2-8df7-44ae-b6ce-5c91ac308d76)(label(RecordHarvest))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         eae1dd4f-97cb-4136-8a7f-2291798bc30e)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         48d16220-cb07-458d-b727-6497272447b1)(label(h3))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         cf31d87d-bfb4-4c89-83b0-0f74585e4b10)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         c6246932-f703-44e8-8e04-25859f8d42ad)(content(Whitespace\"\\n\"))))(Tile((id \
         ca033cb2-5cd5-4c83-a538-7f3dd35182fd)(label(PremiumSale))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         d628b7d6-35fa-4b47-9678-6b46cb917edd)(content(Whitespace\"\\n\"))))))))))))))(Secondary((id \
         d75a7e4c-6803-4ce1-b751-3f36bcdaab3e)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         b667f501-a45d-4536-bee8-373bc381a801)(content(Whitespace\"\\n\"))))(Tile((id \
         a3fbf5cf-882c-4e91-a56c-9ff5d19a0dce)(label(ledger))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         2a32a99d-4f4a-49f2-97d7-5f8296201968)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         0f2b5e07-fe06-4b29-ad6b-5ed0f411c1ed)(label(streakBonus))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         ab8e5d84-2be0-42d7-af8d-b6f8cc95e36d)(content(Whitespace\" \
         \"))))(Tile((id \
         f4888b62-ce41-4eac-8330-5a9899d08650)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         4353c938-b699-4c67-8872-494df4a19af7)(content(Whitespace\" \
         \"))))(Tile((id \
         c96a743a-a73c-4fed-9438-ef8fa95a77da)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         b694412d-272a-4285-9d94-7ec7b029fe23)(content(Whitespace\"\\n\")))))))))(Tile((id \
         7c01dae6-b1bd-49d6-87b1-22b1d33e67d8)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         6f65bb86-56eb-43e8-8b14-36d7bb7daea4)(content(Whitespace\"\\n\"))))(Secondary((id \
         bde4dfd1-8113-4ebc-8f6d-1846a27a72d2)(content(Whitespace\"\\n\"))))(Secondary((id \
         afd628cd-ff0e-4542-abee-e73d6d9724b6)(content(Comment\"# PremiumSale \
         with no streak gives zero payout #\"))))(Secondary((id \
         8cb48e16-f9a7-4f3f-b65e-65294241cc8d)(content(Whitespace\"\\n\"))))(Tile((id \
         b702218e-4510-4724-b56f-e1a0578bebee)(label(hint test end))(mold((out \
         Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         09c43cdf-da41-4404-91cf-e405331f6136)(content(Whitespace\" \
         \"))))(Tile((id \
         60e0c115-118a-4d29-bbf6-e2a2887fa1b7)(label(\"\\\"PremiumSale with \
         zero streak adds nothing\\\"\"))(mold((out Exp)(in_())(nibs(((shape \
         Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         53e8e04c-9951-4418-ae45-7a6c9e83551a)(content(Whitespace\"\\n\")))))((Secondary((id \
         c681cffd-2dcc-44c6-8c5c-52589295e5ef)(content(Whitespace\"\\n\"))))(Tile((id \
         479f064e-5f2f-45f8-98d2-f167f3586dcd)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         83e27da2-3212-40aa-bf25-64452f658510)(content(Whitespace\" \
         \"))))(Tile((id \
         ceb071fc-34b6-4118-b5b8-3c7d5e41bc2f)(label(h))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         5405d6c5-3d80-4db9-ac5d-9f069d0f0fbb)(content(Whitespace\" \
         \")))))((Secondary((id \
         a4663a5c-40ed-451a-bd58-38dd89af39f8)(content(Whitespace\" \
         \"))))(Tile((id \
         e1249559-6db4-4fee-9594-774b8aefa8ae)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         5ab3e810-30cc-4a0d-b230-3aca5c410dd5)(label(crop))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         29068318-54ae-40ec-98eb-37210202d4ba)(content(Whitespace\" \
         \"))))(Tile((id \
         d94b2a90-beb2-4c53-ae93-e203c145163d)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         41e051bb-de86-40bc-b4f1-989319577a66)(content(Whitespace\" \
         \"))))(Tile((id \
         7cc02afc-c2b1-4703-8b9a-1a271967592e)(label(Moonmelon))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         54f220e5-e92e-49f0-a4e7-48279c63aa85)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         a38c516e-836d-49bb-a328-6061d310a5fc)(content(Whitespace\" \
         \"))))(Tile((id \
         59405a7c-c6c1-48a9-a41d-c4a25bbe6abb)(label(quality))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         fd850483-b1ed-496a-8ab7-f28f83e9508e)(content(Whitespace\" \
         \"))))(Tile((id \
         96481e76-79dc-407c-89bc-c9838953f6ab)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         908474a4-82cb-4660-b07a-bc364b1acb92)(content(Whitespace\" \
         \"))))(Tile((id \
         fb89f6d8-c407-42f4-a996-1aef18911275)(label(Bronze))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         a880d129-315c-436f-8c28-ac286c3d7735)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         21032bc3-6c18-48bf-b9ce-117b79150d89)(content(Whitespace\" \
         \"))))(Tile((id \
         60359540-59fe-4e7d-bc84-8b60ddd067d8)(label(quantity))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         dad8294c-4c3b-4c68-9b85-2f44fab1f94f)(content(Whitespace\" \
         \"))))(Tile((id \
         9b58330a-19e8-4aa3-b1ba-c625864fa40a)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         3ecf9814-7f80-45b4-aa38-974c7abff8a8)(content(Whitespace\" \
         \"))))(Tile((id \
         87dc37a9-e9aa-4a2c-88a5-b939d514b3d0)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         65e93f69-a585-4821-99da-ccf576527ffe)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         3cab123a-64bc-432c-a399-b798d6a0a776)(content(Whitespace\"\\n\"))))(Tile((id \
         60f91ace-6dae-4e15-81c6-0acc4c573a17)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         7d856e55-dac5-4d75-aef8-5df793a28c08)(content(Whitespace\" \
         \"))))(Tile((id \
         09dc58fe-2b65-4974-918b-9d2af666f1c8)(label(ledger))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         ba2ff6e0-d41d-41de-9dba-ea84364e972d)(content(Whitespace\" \
         \")))))((Secondary((id \
         5c832130-3817-4cd4-a66d-1a422cb71a87)(content(Whitespace\" \
         \"))))(Tile((id \
         e3cb776f-d004-47f9-95fa-ce22377e0f73)(label(run))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         77698791-94d0-44ae-9e10-d743419725af)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         039295a7-2285-4c8f-9875-4b2c02a91156)(label(initModel))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         cd97ed22-073d-49a4-b482-5adc1c7ed7cf)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         e2e1cb21-78ce-4bf8-ba2c-04aba6651d9a)(content(Whitespace\" \
         \"))))(Tile((id e0defc16-1aaa-4223-a527-2cbeab1cc96a)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         b8555c95-9ba1-4142-97df-e8614e02eecb)(content(Whitespace\"\\n\"))))(Tile((id \
         b64e3683-7859-4b6f-ae51-101b557e9cf6)(label(RecordHarvest))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         e7f6248d-a38b-4368-96b0-590e92bf68b7)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         7471ddb4-6f63-4c53-b9ee-4f55a825edce)(label(h))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         c17e14a0-9529-4761-b549-05bce2cbf271)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         0b7cd9e4-7a08-4287-b0d4-bce022904269)(content(Whitespace\"\\n\"))))(Tile((id \
         fb807a06-11c7-4044-b7eb-3c368b5ae1f9)(label(PremiumSale))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         14dc4a6f-46a3-43ae-a8b9-2dba6f3c33d6)(content(Whitespace\"\\n\"))))))))))))))(Secondary((id \
         f3c446c5-7ca6-4765-aabb-9d7805ec0751)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         f40e4771-aa8b-4034-954a-c29e6564bf02)(content(Whitespace\"\\n\"))))(Secondary((id \
         edc74c53-b9f7-4873-9167-1afd59bdca99)(content(Comment\"# streakBonus \
         was 0, payout = 0 * 1 = 0 #\"))))(Secondary((id \
         2a989d59-e0d1-4f98-8f3c-f3c39fd4babe)(content(Whitespace\"\\n\"))))(Tile((id \
         6009fb63-751a-4819-a421-138f56907f30)(label(ledger))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         d8072f25-3440-439c-bded-140419614647)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         9dc62a35-ef28-4ce8-af5b-92fc4f0d796d)(label(totalValue))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         e09a1a31-ad66-4094-bfaf-cbc0f15a7412)(content(Whitespace\" \
         \"))))(Tile((id \
         f8aa2fee-1909-4df2-9743-2acced1c5029)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         e00b5b6d-866a-44bc-8bd8-d787c27537e9)(content(Whitespace\" \
         \"))))(Tile((id \
         8c37456e-319a-4b80-98d2-51fa1c98deb9)(label(15))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         0ac1d52d-dfed-405d-b1e4-ec922a27cb52)(content(Whitespace\"\\n\")))))))))(Tile((id \
         5c6261fa-b4d6-4a93-a24e-f431906bdc72)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         920b5fc5-c948-44b1-91fa-2db3b74d0d45)(content(Whitespace\"\\n\"))))(Secondary((id \
         c642aa33-b08f-47f7-b7b1-3f3d02345130)(content(Whitespace\"\\n\"))))(Secondary((id \
         eaa23164-aeb1-48d9-9ce1-d3f5b06b038c)(content(Comment\"# Demo: \
         Premium sale harvest day #\"))))(Secondary((id \
         bffa7d4c-39c0-498b-aaa7-d40e2204fd8b)(content(Whitespace\"\\n\"))))(Tile((id \
         f1851b2c-46f2-446a-8ab1-532fc57b10af)(label(run))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         0fcd37a7-b376-4150-8896-ed09a34b7e20)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         a7872f62-ed9c-4492-a360-861921ba448d)(label(initModel))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         ce45ba9a-82a4-474b-bb86-5f21841133cd)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         7827773a-1a02-4920-9d42-203c9b37c96d)(content(Whitespace\" \
         \"))))(Tile((id 4c30a8d2-978c-49ea-906d-d07759f5d73c)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         b3a10c44-63a4-43a8-b6e6-eb17b5e51e26)(content(Whitespace\"\\n\"))))(Tile((id \
         62a4f008-94e4-4fc1-99ce-ba06cee28b5f)(label(RecordHarvest))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         f281c58f-5eee-4e55-b4cf-9d9b5f7937f1)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         ab8038ec-0df4-422a-a54c-806d951251ca)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         713ca790-be84-455d-b40c-9c37f86bf736)(label(crop))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         88b05ba2-9459-4a55-917a-b273d31e689f)(content(Whitespace\" \
         \"))))(Tile((id \
         44863415-67c4-46d2-b0cb-70a9e1b75fdc)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         69de25b7-864b-428c-b6a9-ebe8530b7632)(content(Whitespace\" \
         \"))))(Tile((id \
         eb45dd58-b3ba-4053-bf0b-d610c8566229)(label(Nightshade))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         ff71e328-4e50-409b-b94b-b06502e150c8)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         5e475a0f-9508-4624-9a65-ccfd25c5289b)(content(Whitespace\" \
         \"))))(Tile((id \
         4dbb6f13-8291-4546-baee-099f4f47a568)(label(quality))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         79b2c273-3e86-4726-9f21-51a6d490403d)(content(Whitespace\" \
         \"))))(Tile((id \
         eb498ed4-0569-4bf4-8390-db450f2197b5)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         4b7153a9-79f6-44e6-9a4f-1ad10aedae8d)(content(Whitespace\" \
         \"))))(Tile((id \
         dda8dd7a-b081-49fc-a50d-d6d334f8c984)(label(Gold))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         9750b356-0b7c-4121-b394-11b5cbd22357)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         78579788-aa51-4243-9475-bf712c82cf9b)(content(Whitespace\" \
         \"))))(Tile((id \
         b8cad294-ecff-4e1c-a95b-f42734733730)(label(quantity))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         33896c91-08bf-4a7f-918b-42d2eaa3fe2c)(content(Whitespace\" \
         \"))))(Tile((id \
         3fffbade-f516-414e-95bd-77a3aa0ee742)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         e19d8a8a-b7ca-4ab4-8f2e-dc34e142e071)(content(Whitespace\" \
         \"))))(Tile((id \
         7a00f760-e119-49bf-b887-12fb16c69a2a)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))))))))))))(Tile((id \
         d0054f18-fdca-4513-aa43-d5e177bc9069)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         dac659e4-ac0e-46d9-91c7-76e25f05e1be)(content(Whitespace\"\\n\"))))(Tile((id \
         6fb4e021-ad99-456e-bee0-46c83da85a94)(label(RecordHarvest))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         ef828268-745f-4446-980c-8697f56b37de)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         b7e54346-5f65-4305-930a-052668740d8d)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         f9a73e03-084d-401d-a521-cdf39a8af63c)(label(crop))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         8624b428-a629-4214-939a-80f41ea88f70)(content(Whitespace\" \
         \"))))(Tile((id \
         36d5c4fd-a28a-49a1-abd2-abe77f64e13b)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         310b7ff1-6af4-49f3-800b-2e560a699154)(content(Whitespace\" \
         \"))))(Tile((id \
         e854570b-7e55-48f7-8948-98f7242013a7)(label(Starfruit))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         520e70e8-7157-483b-adbf-d97630d1b543)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         ce95cb98-15d6-43f6-a233-2aecc756cdfc)(content(Whitespace\" \
         \"))))(Tile((id \
         c8226aee-fba9-4c02-a602-3402aa1f8861)(label(quality))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         2f5be6aa-3677-4969-ad3f-cde44b0b6971)(content(Whitespace\" \
         \"))))(Tile((id \
         8dfe49d9-6736-481a-b6f7-7e0a79dccf5e)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         93b0c504-d355-4e04-9184-b8ff0a26d825)(content(Whitespace\" \
         \"))))(Tile((id \
         cc7c6229-7a37-4dcc-ad05-4706151c9a56)(label(Gold))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         ecd375d2-d7e0-4a6c-b564-df4551a2debe)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         3e2fd6ba-b3d7-4015-a852-73db45dde9c8)(content(Whitespace\" \
         \"))))(Tile((id \
         c798b985-0878-4410-b355-7ca3868198be)(label(quantity))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         e1f218f9-7fb5-4d44-ad86-719f40e10f86)(content(Whitespace\" \
         \"))))(Tile((id \
         abc1c677-ddb7-4d8a-a1db-b45763470c4a)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         d682b18a-0694-4ae3-8c40-50b49d4308d4)(content(Whitespace\" \
         \"))))(Tile((id \
         b56727a1-2944-4f5f-84f2-77bf2666f232)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))))))))))))(Tile((id \
         8db4b6e7-9580-47c1-a09f-f3802224982b)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         5e0383ca-1b84-46f3-b5c5-4207b593099b)(content(Whitespace\"\\n\"))))(Tile((id \
         cb7f9955-c284-4a48-82b3-f2aa8e27ea9e)(label(RecordHarvest))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         6e2fe7ac-fd5f-4fcf-8fbf-442a4f06c021)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         12798127-e1c5-4831-ac03-2ebcef157cd5)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         a69767db-d126-4b2a-8445-2757e844adff)(label(crop))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         9a09f0e4-87d8-41b1-8ced-2aab8fe74a64)(content(Whitespace\" \
         \"))))(Tile((id \
         77ee3f09-4eb4-4410-a5af-9bbdc807fb61)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         4bb69526-eaee-40a0-875f-e1041cbe712f)(content(Whitespace\" \
         \"))))(Tile((id \
         138c7b2d-ac5b-4084-9b2c-c50a109298b8)(label(Moonmelon))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         640a9679-d916-4305-95c7-dade5dba8066)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         e2995257-39d1-4507-98f7-f50563416771)(content(Whitespace\" \
         \"))))(Tile((id \
         7e96fc48-03c0-41c4-a285-332865c577fd)(label(quality))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         7ef85890-c988-40c5-8c6f-8386dd2d7c3a)(content(Whitespace\" \
         \"))))(Tile((id \
         1835b8f2-4cf7-4e7d-900a-9ed754a98868)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         892d16fe-d243-4321-b809-c7726231fc5d)(content(Whitespace\" \
         \"))))(Tile((id \
         2b67236c-4d72-4ee4-9d6f-834aaa2e5f37)(label(Gold))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         8440e117-bba9-478a-92e3-ab6afdf66bc0)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         f12a0993-e540-4fdc-b27f-5e003df5fa63)(content(Whitespace\" \
         \"))))(Tile((id \
         64517561-590a-4bc4-89ea-a0ed6a3a6656)(label(quantity))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         4c567421-0b4d-4e83-9ebf-ed97c229dc68)(content(Whitespace\" \
         \"))))(Tile((id \
         c96703db-dc4b-49a0-a22a-f1c7f874531e)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         31a92129-cdb7-4e47-95a9-87769fb709da)(content(Whitespace\" \
         \"))))(Tile((id \
         600c05ad-f521-4984-a610-6820a58dc9eb)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))))))))))))(Tile((id \
         b8c0eccb-a3bb-478d-ac68-93d0bb9121cf)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         04eb513d-f993-46f3-bfe3-cb2d1892c2d6)(content(Whitespace\"\\n\"))))(Tile((id \
         9dfb7ec8-02d5-43fe-8a70-a1c8119d3f7f)(label(PremiumSale))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         ae4bf6bc-8c42-4706-8463-ce4e8b6be407)(content(Whitespace\"\\n\"))))))))))))))(Secondary((id \
         f3e98d68-eeaf-4ef9-b530-5c1aefbd772f)(content(Whitespace\"\\n\")))))";
      backup_text =
        "# HARVEST STREAK EXTENSION TASK                   #\n\
         #                                                  #\n\
         # The harvest ledger app tracks harvests and       #\n\
         # builds streak bonuses for consecutive same-      #\n\
         # quality harvests.                                #\n\
         #                                                  #\n\
         # YOUR TASK: Add a PremiumSale action that lets    #\n\
         # the farmer claim their streak bonus with a       #\n\
         # premium multiplier when the streak is strong.    #\n\
         #                                                  #\n\
         # You need to:                                     #\n\
         #   1. Add PremiumSale to the Action type          #\n\
         #   2. Write a premiumMultiplier helper function   #\n\
         #   3. Handle PremiumSale in the update function   #\n\
         #                                                  #\n\
         # Look at how ClaimBonus is implemented for        #\n\
         # guidance - PremiumSale is similar but applies    #\n\
         # a multiplier to the payout.                      #\n\
         #                                                  #\n\
         # Tip: Use auto-probe on premiumMultiplier to see  #\n\
         # when the threshold fires.                        #\n\n\
         # Quality tiers from the moonlit fields #\n\
         type Quality =\n\
         + Bronze      # Common harvest, basic value #\n\
         + Silver      # Good quality, moderate bonus #\n\
         + Gold        # Excellent harvest, high value #\n\
         + Starlight   # Legendary, blessed by the moon #\n\
         in\n\n\
         # Crops that grow under the night sky #\n\
         type Crop =\n\
         + Moonmelon   # Glows faintly, sweet taste #\n\
         + Starfruit   # Shaped like stars, tangy #\n\
         + Nightshade  # Purple bloom, magical properties #\n\
         + Duskwheat   # Golden stalks, hearty grain #\n\
         + Glowpumpkin # Orange and luminescent #\n\
         in\n\n\
         # A single harvest from the garden #\n\
         type Harvest = (\n\
         crop = Crop,\n\
         quality = Quality,\n\
         quantity = Int\n\
         ) in\n\n\
         # The harvest ledger tracks all harvests and bonuses #\n\
         type Model = (\n\
         harvests = [Harvest],\n\
         totalValue = Int,\n\
         streakBonus = Int,\n\
         lastQuality = Quality\n\
         ) in\n\n\
         # Actions the farmer can take #\n\
         type Action =\n\
         + RecordHarvest(Harvest)  # Log a new harvest #\n\
         + ClaimBonus              # Collect accumulated streak bonus #\n\
         + CloseDay                # End the harvest day, reset streaks #\n\
         # TODO: Add PremiumSale here #\n\
         in\n\n\
         # Calculate base value of a crop #\n\
         let cropValue : Crop -> Int =\n\
         fun c ->\n\
         case c\n\
         | Moonmelon => 15\n\
         | Starfruit => 20\n\
         | Nightshade => 20\n\
         | Duskwheat => 10\n\
         | Glowpumpkin => 12\n\
         end\n\
         in\n\n\
         # Quality multiplier for harvest value #\n\
         let qualityMultiplier : Quality -> Int =\n\
         fun q ->\n\
         case q\n\
         | Bronze => 1\n\
         | Silver => 2\n\
         | Gold => 3\n\
         | Starlight => 5\n\
         end\n\
         in\n\n\
         # Calculate the value of a single harvest #\n\
         let harvestValue : Harvest -> Int =\n\
         fun h ->\n\
         cropValue(h.crop) * qualityMultiplier(h.quality) * h.quantity\n\
         in\n\n\
         # Initial empty ledger #\n\
         let initModel : Model = (\n\
         harvests = [],\n\
         totalValue = 0,\n\
         streakBonus = 0,\n\
         lastQuality = Bronze\n\
         ) in\n\n\
         # Process a harvest action and update the ledger #\n\
         let processHarvest : (Model, Harvest) -> Model =\n\
         fun (ledger, h) ->\n\
         let value = harvestValue(h) in\n\
         # Check if this harvest continues the quality streak #\n\
         # First harvest never continues a streak (no previous harvest) #\n\
         # Compare current quality with the PREVIOUS lastQuality #\n\
         let isFirst = length(ledger.harvests) == 0 in\n\
         let continues = !isFirst && h.quality == ledger.lastQuality in\n\
         # Now update lastQuality to current harvest #\n\
         let newLast = h.quality in\n\
         let newStreak =\n\
         if continues\n\
         then ledger.streakBonus + 5\n\
         else 0\n\
         in\n\
         (\n\
         harvests = h :: ledger.harvests,\n\
         totalValue = ledger.totalValue + value + newStreak,\n\
         streakBonus = newStreak,\n\
         lastQuality = newLast\n\
         )\n\
         in\n\n\
         # Claim the streak bonus and reset it #\n\
         let claimBonus : Model -> Model =\n\
         fun ledger ->\n\
         (\n\
         harvests = ledger.harvests,\n\
         totalValue = ledger.totalValue + ledger.streakBonus,\n\
         streakBonus = 0,\n\
         lastQuality = ledger.lastQuality\n\
         )\n\
         in\n\n\
         # Close the harvest day - reset streak tracking #\n\
         let closeDay : Model -> Model =\n\
         fun ledger ->\n\
         (\n\
         harvests = ledger.harvests,\n\
         totalValue = ledger.totalValue,\n\
         streakBonus = 0,\n\
         lastQuality = Bronze\n\
         )\n\
         in\n\n\
         # TODO: Add premiumMultiplier helper here                      #\n\
         # It takes a streakBonus (Int) and returns the multiplier:     #\n\
         #   - Return 2 if the streak bonus is >= 10 (strong streak)    #\n\
         #   - Return 1 otherwise                                      #\n\
         # Hint: This is a simple if/then/else on the streakBonus.     #\n\n\
         let premiumMultiplier : Int -> Int =\n\
         fun streakBonus ->\n\
         ?\n\n\n\n\
         in\n\n\
         # Main update function - dispatch actions #\n\
         let update : (Model, Action) -> Model =\n\
         fun (ledger, action) ->\n\
         case action\n\
         | RecordHarvest(h) => processHarvest(ledger, h)\n\
         | ClaimBonus => claimBonus(ledger)\n\
         | CloseDay => closeDay(ledger)\n\
         # TODO: Add PremiumSale case here                          #\n\
         # Hint: Compute payout = streakBonus * premiumMultiplier,  #\n\
         # add payout to totalValue, and reset streakBonus to 0.    #\n\
         # Keep harvests and lastQuality unchanged.                 #\n\
         end\n\
         in\n\n\
         # Run multiple actions in sequence #\n\
         let run : (Model, [Action]) -> Model =\n\
         fun (ledger, actions) ->\n\
         fold_left(actions, update, ledger)\n\
         in\n\n\
         # ===== TESTS ===== #\n\n\
         # Regression: basic harvest recording still works #\n\
         hint \"recording harvest adds to total value\"\n\
         test\n\
         let h = (crop = Moonmelon, quality = Bronze, quantity = 2) in\n\
         let ledger = update(initModel, RecordHarvest(h)) in\n\
         # Moonmelon(15) * Bronze(1) * 2 = 30, no streak bonus on first #\n\
         ledger.totalValue == 30\n\
         end;\n\n\
         # Regression: streak bonus still works #\n\
         hint \"same quality builds streak\"\n\
         test\n\
         let h1 = (crop = Moonmelon, quality = Gold, quantity = 1) in\n\
         let h2 = (crop = Starfruit, quality = Gold, quantity = 1) in\n\
         let ledger = run(initModel, [RecordHarvest(h1), RecordHarvest(h2)]) in\n\
         ledger.streakBonus == 5\n\
         end;\n\n\
         # Regression: claim bonus still works #\n\
         hint \"claiming bonus adds to total and resets streak\"\n\
         test\n\
         let h1 = (crop = Moonmelon, quality = Gold, quantity = 1) in\n\
         let h2 = (crop = Starfruit, quality = Gold, quantity = 1) in\n\
         let ledger = run(initModel, [\n\
         RecordHarvest(h1),\n\
         RecordHarvest(h2),\n\
         ClaimBonus\n\
         ]) in\n\
         ledger.streakBonus == 0\n\
         end;\n\n\
         # PremiumSale: low streak gives 1x multiplier #\n\
         hint \"PremiumSale with low streak uses 1x multiplier\"\n\
         test\n\
         # Two same-quality harvests build streakBonus to 5 #\n\
         let h1 = (crop = Moonmelon, quality = Silver, quantity = 1) in\n\
         let h2 = (crop = Starfruit, quality = Silver, quantity = 1) in\n\
         let ledger = run(initModel, [\n\
         RecordHarvest(h1),\n\
         RecordHarvest(h2),\n\
         PremiumSale\n\
         ]) in\n\
         # streakBonus was 5, multiplier = 1, payout = 5 #\n\
         # h1: 15*2*1 = 30, h2: 20*2*1 + 5 = 45, PremiumSale: +5 #\n\
         ledger.totalValue == 30 + 45 + 5\n\
         end;\n\n\
         # PremiumSale: high streak gives 2x multiplier #\n\
         hint \"PremiumSale with high streak uses 2x multiplier\"\n\
         test\n\
         # Three same-quality harvests build streakBonus to 10 #\n\
         let h1 = (crop = Moonmelon, quality = Gold, quantity = 1) in\n\
         let h2 = (crop = Starfruit, quality = Gold, quantity = 1) in\n\
         let h3 = (crop = Nightshade, quality = Gold, quantity = 1) in\n\
         let ledger = run(initModel, [\n\
         RecordHarvest(h1),\n\
         RecordHarvest(h2),\n\
         RecordHarvest(h3),\n\
         PremiumSale\n\
         ]) in\n\
         # streakBonus was 10, multiplier = 2, payout = 20 #\n\
         # h1: 15*3=45, h2: 20*3+5=65, h3: 20*3+10=70, PremiumSale: +20 #\n\
         ledger.totalValue == 45 + 65 + 70 + 20\n\
         end;\n\n\
         # PremiumSale resets streak after claiming #\n\
         hint \"PremiumSale resets streak to zero\"\n\
         test\n\
         let h1 = (crop = Moonmelon, quality = Gold, quantity = 1) in\n\
         let h2 = (crop = Starfruit, quality = Gold, quantity = 1) in\n\
         let h3 = (crop = Nightshade, quality = Gold, quantity = 1) in\n\
         let ledger = run(initModel, [\n\
         RecordHarvest(h1),\n\
         RecordHarvest(h2),\n\
         RecordHarvest(h3),\n\
         PremiumSale\n\
         ]) in\n\
         ledger.streakBonus == 0\n\
         end;\n\n\
         # PremiumSale with no streak gives zero payout #\n\
         hint \"PremiumSale with zero streak adds nothing\"\n\
         test\n\
         let h = (crop = Moonmelon, quality = Bronze, quantity = 1) in\n\
         let ledger = run(initModel, [\n\
         RecordHarvest(h),\n\
         PremiumSale\n\
         ]) in\n\
         # streakBonus was 0, payout = 0 * 1 = 0 #\n\
         ledger.totalValue == 15\n\
         end;\n\n\
         # Demo: Premium sale harvest day #\n\
         run(initModel, [\n\
         RecordHarvest((crop = Nightshade, quality = Gold, quantity = 1)),\n\
         RecordHarvest((crop = Starfruit, quality = Gold, quantity = 1)),\n\
         RecordHarvest((crop = Moonmelon, quality = Gold, quantity = 1)),\n\
         PremiumSale\n\
         ])\n";
      refractors = "()";
    } )
