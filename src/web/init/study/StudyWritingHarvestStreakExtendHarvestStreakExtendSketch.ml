let out : string * Haz3lcore.PersistentSegment.t =
  ( "Study / writing / harvest-streak-extend / harvest-streak-extend-sketch",
    {
      segment =
        "((Secondary((id \
         31aa4204-1060-4f1a-a8c9-32cacf367709)(content(Comment\"# HARVEST \
         STREAK EXTENSION TASK                   #\"))))(Secondary((id \
         c56002e6-f063-486f-a195-8cdcc9c0bfea)(content(Whitespace\"\\n\"))))(Secondary((id \
         cb8f03b4-2b43-492c-84cc-b171c3d96f3b)(content(Comment\"#                                                  \
         #\"))))(Secondary((id \
         23e9b909-c1a6-4099-b9ad-90dfe8abca4d)(content(Whitespace\"\\n\"))))(Secondary((id \
         16fbaff6-cd56-4791-bd4f-356067d80165)(content(Comment\"# The harvest \
         ledger app tracks harvests and       #\"))))(Secondary((id \
         c54c9cec-319e-44a2-bc39-a0488bf70797)(content(Whitespace\"\\n\"))))(Secondary((id \
         d5497e2a-4e51-4c2d-ba6f-fe8cd8ac24c2)(content(Comment\"# builds \
         streak bonuses for consecutive same-      #\"))))(Secondary((id \
         39189d21-c758-4c1c-945e-48f3f2644d14)(content(Whitespace\"\\n\"))))(Secondary((id \
         7708f864-39c6-40c7-8c2e-50188c323d94)(content(Comment\"# quality \
         harvests.                                #\"))))(Secondary((id \
         45c977b0-1402-4d47-9502-a8b5ab508c2b)(content(Whitespace\"\\n\"))))(Secondary((id \
         1571e4ac-c5d9-4584-a25e-3f1e9b4489d5)(content(Comment\"#                                                  \
         #\"))))(Secondary((id \
         16497a50-f3b1-4ad5-b0d7-009f99cce502)(content(Whitespace\"\\n\"))))(Secondary((id \
         f818e3eb-67a6-46fb-a21e-c81942fdcb9e)(content(Comment\"# YOUR TASK: \
         Add a PremiumSale action that lets    #\"))))(Secondary((id \
         d721bc1b-c09d-43a9-9893-8133364b805e)(content(Whitespace\"\\n\"))))(Secondary((id \
         7755f3ad-e9f4-4f29-84c8-7f25ae690671)(content(Comment\"# the farmer \
         claim their streak bonus with a       #\"))))(Secondary((id \
         06171082-bfca-4bb8-aa1b-18219f9f6d9e)(content(Whitespace\"\\n\"))))(Secondary((id \
         090a3f35-3ca0-41b7-be7c-7249a17382a0)(content(Comment\"# premium \
         multiplier when the streak is strong.    #\"))))(Secondary((id \
         2d8c1e29-b9dc-41cf-a216-ee05a6da35f6)(content(Whitespace\"\\n\"))))(Secondary((id \
         1487f597-3cfb-49a1-8e36-1076e6b8332c)(content(Comment\"#                                                  \
         #\"))))(Secondary((id \
         23593810-58b5-4de8-b24d-2e4e4d849227)(content(Whitespace\"\\n\"))))(Secondary((id \
         8b6575b6-3cdb-4e7a-8437-b8df4b24f1d1)(content(Comment\"# You need \
         to:                                     #\"))))(Secondary((id \
         7a0ea5f5-e706-46d8-a247-1c532a3aadad)(content(Whitespace\"\\n\"))))(Secondary((id \
         a5da932a-decc-4fae-897c-b6a0602e8b30)(content(Comment\"#   1. Add \
         PremiumSale to the Action type          #\"))))(Secondary((id \
         0df05a8b-7c1e-45a8-979f-b506d28058aa)(content(Whitespace\"\\n\"))))(Secondary((id \
         d61c8054-4d4b-41cb-ad92-2d40a50ab365)(content(Comment\"#   2. Write a \
         premiumMultiplier helper function   #\"))))(Secondary((id \
         5b81c651-e728-46d4-b16c-e8a0473efe62)(content(Whitespace\"\\n\"))))(Secondary((id \
         bd6f303f-ee8f-44d7-9f52-ba343f5eac85)(content(Comment\"#   3. Handle \
         PremiumSale in the update function   #\"))))(Secondary((id \
         b7b71466-542c-4094-80ac-f01f53fde66b)(content(Whitespace\"\\n\"))))(Secondary((id \
         d17a76a9-cdb0-4aba-b5b4-ce6eae5609cb)(content(Comment\"#                                                  \
         #\"))))(Secondary((id \
         541551e1-aeac-4ca4-a9e5-c5b100e2e6fb)(content(Whitespace\"\\n\"))))(Secondary((id \
         41504198-6183-4a21-8f09-4a7c45b1efc3)(content(Comment\"# Look at how \
         ClaimBonus is implemented for        #\"))))(Secondary((id \
         56328c05-e4fe-495a-a76b-cf1546d625d8)(content(Whitespace\"\\n\"))))(Secondary((id \
         861f4cc6-b305-4198-bf52-d9cc523ac498)(content(Comment\"# guidance - \
         PremiumSale is similar but applies    #\"))))(Secondary((id \
         80e4ec42-9991-47f0-8a48-680b32cb6dd9)(content(Whitespace\"\\n\"))))(Secondary((id \
         bb51700d-5be7-40f3-a206-3e1aceea881d)(content(Comment\"# a multiplier \
         to the payout.                      #\"))))(Secondary((id \
         c13d48b9-e435-4a7a-9b1c-f13845d969d3)(content(Whitespace\"\\n\"))))(Secondary((id \
         26e6798b-76de-4c19-bcbf-cdc4455e772b)(content(Comment\"#                                                  \
         #\"))))(Secondary((id \
         2fb51683-f9f3-4a45-a6e8-fdf72531bcf4)(content(Whitespace\"\\n\"))))(Secondary((id \
         18a79d9e-cda6-4fe5-b3f7-eaaae62204a4)(content(Comment\"# Tip: Use \
         auto-probe on premiumMultiplier to see  #\"))))(Secondary((id \
         f567f3de-a12e-4177-9c01-2ec203649f41)(content(Whitespace\"\\n\"))))(Secondary((id \
         2a5c2dac-8004-4e53-9f77-f308fd0357d4)(content(Comment\"# when the \
         threshold fires.                        #\"))))(Secondary((id \
         284914ca-8ec7-4c10-8729-07bfe89b8658)(content(Whitespace\"\\n\"))))(Secondary((id \
         1863c49c-ba0a-457e-9ff1-da2c420c5841)(content(Whitespace\"\\n\"))))(Secondary((id \
         41280b80-05e4-4b09-8df9-cfc168b4ae0c)(content(Comment\"# Quality \
         tiers from the moonlit fields #\"))))(Secondary((id \
         ea771c61-1573-410e-b805-c4606048fb05)(content(Whitespace\"\\n\"))))(Tile((id \
         1f3f4048-2ab3-4c55-87fc-504a08f86cbc)(label(type = in))(mold((out \
         Exp)(in_(TPat Typ))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         9df67f33-7f81-423b-b7c8-ea9a4f3e7c83)(content(Whitespace\" \
         \"))))(Tile((id \
         fa1e5a46-be1f-4e53-b863-02bdaefade1a)(label(Quality))(mold((out \
         TPat)(in_())(nibs(((shape Convex)(sort TPat))((shape Convex)(sort \
         TPat))))))(shards(0))(children())))(Secondary((id \
         b7f32b7d-a418-40a7-bd85-98058e204b2c)(content(Whitespace\" \
         \")))))((Secondary((id \
         f8b5338e-c032-42ab-aca6-1550dae5953e)(content(Whitespace\"\\n\"))))(Tile((id \
         e1997023-c3aa-474d-8a75-73787f087b18)(label(+))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape(Concave 33))(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         d22a70ae-f543-4323-914b-24d48a189047)(content(Whitespace\" \
         \"))))(Tile((id \
         5c2ae508-6a5b-480b-9c5d-76fe245239b4)(label(Bronze))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         c9a3dcb1-d74b-4bad-bef3-1b8d933b7d93)(content(Whitespace\" \
         \"))))(Secondary((id \
         29a19704-9a05-4032-abb8-f2c9083ab331)(content(Whitespace\" \
         \"))))(Secondary((id \
         aa744d26-c23a-439f-8943-38972db62409)(content(Whitespace\" \
         \"))))(Secondary((id \
         e5d5bc8b-a772-43f1-9ac7-b9b2296ec37d)(content(Whitespace\" \
         \"))))(Secondary((id \
         93a0a6da-d58a-4367-b5e6-5edf0b7a6f36)(content(Whitespace\" \
         \"))))(Secondary((id \
         84e05dcf-d4c1-433c-877d-7e8879dc678b)(content(Whitespace\" \
         \"))))(Secondary((id \
         883dd00e-8568-4304-b359-fc4e082a21f1)(content(Comment\"# Common \
         harvest, basic value #\"))))(Secondary((id \
         ba0ecf12-fcf5-4dca-8dc8-5b9c01fded55)(content(Whitespace\"\\n\"))))(Tile((id \
         3b1f9f4b-fa3b-4cfd-be69-921de52fc074)(label(+))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 12))(sort Typ))((shape(Concave \
         12))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         2c01ff1c-7800-4d66-b311-b17c5d6122c1)(content(Whitespace\" \
         \"))))(Tile((id \
         16b7e71b-d0fd-47c2-a6a4-8caf0eb382ac)(label(Silver))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         c8240178-fba2-4f7f-a832-c5a52108cb46)(content(Whitespace\" \
         \"))))(Secondary((id \
         59874ffc-7cb6-40b6-876c-f7b0a6d3b7c7)(content(Whitespace\" \
         \"))))(Secondary((id \
         e3a9ee1b-6803-4e4f-9e4b-996a652abf1e)(content(Whitespace\" \
         \"))))(Secondary((id \
         fa7dfb8b-b77f-4154-b4b7-1537cd3c410a)(content(Whitespace\" \
         \"))))(Secondary((id \
         4a294878-2408-4d41-a57b-b111dc8345f2)(content(Whitespace\" \
         \"))))(Secondary((id \
         fb237015-5999-46e1-b62e-9cf1f411a85f)(content(Whitespace\" \
         \"))))(Secondary((id \
         386dbf56-d1dd-4134-87c2-a547bb5810f6)(content(Comment\"# Good \
         quality, moderate bonus #\"))))(Secondary((id \
         88b71ab3-7b89-4ca1-91e3-70eebcd484e2)(content(Whitespace\"\\n\"))))(Tile((id \
         b11def93-4cde-40ff-af7b-69dfd5d0ea1a)(label(+))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 12))(sort Typ))((shape(Concave \
         12))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         935fa243-e5e1-454c-95c2-3caef981a53e)(content(Whitespace\" \
         \"))))(Tile((id \
         c40b455c-0fc6-44ba-898e-e77a9411904a)(label(Gold))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         a5f636e8-a046-4a85-936e-46a12e8dc162)(content(Whitespace\" \
         \"))))(Secondary((id \
         3aaa6c52-de8e-4954-8c86-6252386b8154)(content(Whitespace\" \
         \"))))(Secondary((id \
         538eb84d-7d86-4cf4-9b84-eca36f2f90d0)(content(Whitespace\" \
         \"))))(Secondary((id \
         bd2a13d2-5ea5-4a33-80d1-86e4ee91f6c1)(content(Whitespace\" \
         \"))))(Secondary((id \
         30ab312f-42d9-461b-9363-29855931b114)(content(Whitespace\" \
         \"))))(Secondary((id \
         35a9b5f5-cc81-47a1-a4d3-7f8bda377063)(content(Whitespace\" \
         \"))))(Secondary((id \
         ac645e26-875f-4fc7-8527-408b7688b2b1)(content(Whitespace\" \
         \"))))(Secondary((id \
         b65409c1-b71b-4216-b366-877c8c90ff3b)(content(Whitespace\" \
         \"))))(Secondary((id \
         132d7442-22ed-40bd-83c7-ac335998b4a7)(content(Comment\"# Excellent \
         harvest, high value #\"))))(Secondary((id \
         93209d94-57e3-4559-ad35-c9097465ae0c)(content(Whitespace\"\\n\"))))(Tile((id \
         73fd5542-0753-4c98-a41a-7768188221f5)(label(+))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 12))(sort Typ))((shape(Concave \
         12))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         a42f782c-7e7b-450c-9f45-eb3b44354746)(content(Whitespace\" \
         \"))))(Tile((id \
         9ff314bd-31f5-49c8-b6c8-37606c4af1eb)(label(Starlight))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         89b11d31-e945-4c6c-89c6-abc8e6d97c88)(content(Whitespace\" \
         \"))))(Secondary((id \
         60218dc1-82c7-4858-b6c5-5ae1ac096376)(content(Whitespace\" \
         \"))))(Secondary((id \
         278f79e6-56d5-4a4f-bca6-4c5b21ce0030)(content(Whitespace\" \
         \"))))(Secondary((id \
         b436fcab-ba15-41a0-9b3c-98adfb0658fc)(content(Comment\"# Legendary, \
         blessed by the moon #\"))))(Secondary((id \
         f040139e-c747-4b32-88ac-8d87cd9d83c9)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         6f34cc5b-557f-4755-b5c6-1259fceb0955)(content(Whitespace\"\\n\"))))(Secondary((id \
         469f8e80-82f9-45cb-85df-ac5d5810e6d4)(content(Whitespace\"\\n\"))))(Secondary((id \
         4d6ed18e-d4dd-4c4a-8b75-d324a9708072)(content(Comment\"# Crops that \
         grow under the night sky #\"))))(Secondary((id \
         54b75570-acdf-44e3-abb3-64734c5e6dc5)(content(Whitespace\"\\n\"))))(Tile((id \
         dd91091f-8fe6-4854-a5c5-73ee5d78cc37)(label(type = in))(mold((out \
         Exp)(in_(TPat Typ))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         16b6309e-1d17-40e8-99c4-b587f2181143)(content(Whitespace\" \
         \"))))(Tile((id \
         9927e423-eb76-4e42-a2b7-e358d7ec737d)(label(Crop))(mold((out \
         TPat)(in_())(nibs(((shape Convex)(sort TPat))((shape Convex)(sort \
         TPat))))))(shards(0))(children())))(Secondary((id \
         4c483fc1-5af7-4ad8-a9e8-ad8d71846499)(content(Whitespace\" \
         \")))))((Secondary((id \
         01f59700-7d3f-481c-b86d-fc7b19d7eb51)(content(Whitespace\"\\n\"))))(Tile((id \
         4baff569-50b4-4dda-8f38-85698990efc5)(label(+))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape(Concave 33))(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         e371207c-2166-4964-8ce8-8a54c6280654)(content(Whitespace\" \
         \"))))(Tile((id \
         b487f82c-de8d-4ddc-802b-43e3a845ea68)(label(Moonmelon))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         91390368-dcd2-488a-a02f-7072ddea3f81)(content(Whitespace\" \
         \"))))(Secondary((id \
         95293ed0-66d6-4053-bc0e-2f689baf878d)(content(Whitespace\" \
         \"))))(Secondary((id \
         c3820411-e364-4383-a1b8-1b306b7373c6)(content(Whitespace\" \
         \"))))(Secondary((id \
         3531b4cd-f9ec-43b2-a601-4879207a9c00)(content(Comment\"# Glows \
         faintly, sweet taste #\"))))(Secondary((id \
         d708367c-c18f-4746-9646-32da4b335b0a)(content(Whitespace\"\\n\"))))(Tile((id \
         39a3838d-6590-4c90-a187-49eb0ead8cf0)(label(+))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 12))(sort Typ))((shape(Concave \
         12))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         fb1e7702-437f-403b-a832-09ca9ffd645d)(content(Whitespace\" \
         \"))))(Tile((id \
         82f498ec-5e74-4dfd-83e0-6d1d37333536)(label(Starfruit))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         6ef86bdd-90ca-4bd6-9a9f-b2ea7b0535bc)(content(Whitespace\" \
         \"))))(Secondary((id \
         832379cb-a608-4dd2-b06c-0a32a05fc298)(content(Whitespace\" \
         \"))))(Secondary((id \
         603b0146-424e-4075-b4a3-3f7e615cc1b6)(content(Whitespace\" \
         \"))))(Secondary((id \
         73208033-c0b7-4f44-a097-91ed59bb7034)(content(Comment\"# Shaped like \
         stars, tangy #\"))))(Secondary((id \
         1538273e-2207-4276-9205-8a4b74d808ef)(content(Whitespace\"\\n\"))))(Tile((id \
         8bc4ff67-3cd9-4f66-8cdf-7344773cb0f8)(label(+))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 12))(sort Typ))((shape(Concave \
         12))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         64d838cb-3876-46a5-9a02-ed2c85cf31c4)(content(Whitespace\" \
         \"))))(Tile((id \
         eb052c39-be60-49b8-9d48-decd0ba74889)(label(Nightberry))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         18a49c28-6b32-4d78-a4f8-810ccabd817d)(content(Whitespace\" \
         \"))))(Secondary((id \
         dea9e210-5eb8-4b96-952d-edfb63cf9347)(content(Whitespace\" \
         \"))))(Secondary((id \
         b22843af-3ce0-4dc8-b597-19929623a4c3)(content(Comment\"# Deep purple, \
         magical properties #\"))))(Secondary((id \
         ec0475f0-f696-43f8-a4e7-d696b997de0b)(content(Whitespace\"\\n\"))))(Tile((id \
         16cd681e-1857-4882-9ea7-310eb07a587d)(label(+))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 12))(sort Typ))((shape(Concave \
         12))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         fb6c5cf4-0871-4697-bf46-374ecee2d919)(content(Whitespace\" \
         \"))))(Tile((id \
         5b8ca88f-0024-4d42-8505-a749399c5825)(label(Duskwheat))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         bfb471d1-1da6-4507-811c-5ebe5e1ab2c0)(content(Whitespace\" \
         \"))))(Secondary((id \
         e64819ce-223c-4945-86c7-6ac43e1f147a)(content(Whitespace\" \
         \"))))(Secondary((id \
         dc588d1a-3c53-45c7-a653-dc7dadcfed65)(content(Whitespace\" \
         \"))))(Secondary((id \
         2c8baba2-9cda-4a48-a11e-68cbbca571cc)(content(Comment\"# Golden \
         stalks, hearty grain #\"))))(Secondary((id \
         8180e502-63de-4245-b223-2f54b9087ca8)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         20e00805-e88b-4729-8a4f-f7fded1eb193)(content(Whitespace\"\\n\"))))(Secondary((id \
         578b18cb-7857-480c-81f7-abd1fa1c5d02)(content(Whitespace\"\\n\"))))(Secondary((id \
         7cda1d79-c025-4068-b4a7-20f204cd0c20)(content(Comment\"# A single \
         harvest from the garden #\"))))(Secondary((id \
         fea1e8ac-9062-461c-82e4-f73d9e45d83d)(content(Whitespace\"\\n\"))))(Tile((id \
         79ed3769-a7f5-4f8e-9d2d-bf6211a80d42)(label(type = in))(mold((out \
         Exp)(in_(TPat Typ))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         33998771-408b-45b0-b4bd-10481fae5d78)(content(Whitespace\" \
         \"))))(Tile((id \
         2e55178c-3c17-4e35-8ac8-19eccabf6e06)(label(Harvest))(mold((out \
         TPat)(in_())(nibs(((shape Convex)(sort TPat))((shape Convex)(sort \
         TPat))))))(shards(0))(children())))(Secondary((id \
         b2e4fff2-0c0b-450b-8d7a-1851ffc1fb35)(content(Whitespace\" \
         \")))))((Secondary((id \
         df947653-4f87-46c6-9deb-e5c9e00cfbac)(content(Whitespace\" \
         \"))))(Tile((id \
         103b6f2a-9f55-4f45-84d9-1d83f4193d19)(label(\"(\"\")\"))(mold((out \
         Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0 1))(children(((Secondary((id \
         0e3401be-6659-481d-9db4-e2b6004aefb9)(content(Whitespace\"\\n\"))))(Tile((id \
         18a65d4d-5385-49f4-895a-8fab289737b8)(label(crop))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         ddebdd98-a7f0-4da2-b33e-ead4250bc1dd)(content(Whitespace\" \
         \"))))(Tile((id \
         48a713ca-ba73-41a8-8d49-750eb8c2839f)(label(=))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 39))(sort Typ))((shape(Concave \
         39))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         d3f3b562-3456-4faa-8c2b-1d5c649a04f7)(content(Whitespace\" \
         \"))))(Tile((id \
         6e969e49-c219-4b7e-9a3b-53cbf5108051)(label(Crop))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         652b7547-f063-4333-b680-8d8ec80f2b7f)(label(,))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 44))(sort Typ))((shape(Concave \
         44))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         d8e02f2f-6c3f-45d8-895d-9c62a82bdaa9)(content(Whitespace\"\\n\"))))(Tile((id \
         d1b47592-25db-4cb5-9d27-120fda525e81)(label(quality))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         12690532-677e-4097-9917-b16497aad3bb)(content(Whitespace\" \
         \"))))(Tile((id \
         21a6920a-bf03-41de-98cd-4651ca7af150)(label(=))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 39))(sort Typ))((shape(Concave \
         39))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         58f9309d-8854-4d90-9a4f-983bf87f00e2)(content(Whitespace\" \
         \"))))(Tile((id \
         0b91e253-d6a8-4f6b-9a34-4f257d7320d5)(label(Quality))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         1e369f8e-c0cd-4934-8cf2-86452fb200e7)(label(,))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 44))(sort Typ))((shape(Concave \
         44))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         a070658f-9a70-4692-9760-0a55912f95d7)(content(Whitespace\"\\n\"))))(Tile((id \
         cfde9e5a-67ac-4f0b-a14d-ba138ae3b647)(label(quantity))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         7f3209bc-6aa3-45ab-bed7-52145d8fcf57)(content(Whitespace\" \
         \"))))(Tile((id \
         21318b17-9e6c-4d9b-bac9-67f82ac6a50c)(label(=))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 39))(sort Typ))((shape(Concave \
         39))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         2af5abe6-7243-481a-ab84-55812d4ab5a0)(content(Whitespace\" \
         \"))))(Tile((id \
         4d7afb85-8efe-4278-9b69-9d39b396c793)(label(Int))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         9ab69584-2d40-456c-acad-2f0f21e42e40)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         4e36849f-56f7-4bd6-a684-bee3bae6a0c6)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         5407b9f4-6341-47db-8d17-c761848b5a8e)(content(Whitespace\"\\n\"))))(Secondary((id \
         25ecbf1c-26b1-4520-aa2e-f63d0281c3a9)(content(Whitespace\"\\n\"))))(Secondary((id \
         b2a44e7f-873c-43ef-9db3-47c6243184d1)(content(Comment\"# The harvest \
         ledger tracks all harvests and bonuses #\"))))(Secondary((id \
         63c1ef23-7d51-4ca1-9933-4d75b5159cf6)(content(Whitespace\"\\n\"))))(Tile((id \
         1cbd8077-a108-4264-a2b6-6bcdd5116ba4)(label(type = in))(mold((out \
         Exp)(in_(TPat Typ))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         bd6a43b7-b731-4d27-9673-3ad054dcc2c5)(content(Whitespace\" \
         \"))))(Tile((id \
         683e35f7-857f-4b3d-9ad5-38914e5aecbe)(label(Model))(mold((out \
         TPat)(in_())(nibs(((shape Convex)(sort TPat))((shape Convex)(sort \
         TPat))))))(shards(0))(children())))(Secondary((id \
         e3749908-5d0a-4611-991b-cfc5d8828c83)(content(Whitespace\" \
         \")))))((Secondary((id \
         1117deea-4239-4d98-ae8d-165453ff2ac3)(content(Whitespace\" \
         \"))))(Tile((id \
         cbcc627d-1759-4cc4-9ef3-22ea0286bdd4)(label(\"(\"\")\"))(mold((out \
         Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0 1))(children(((Secondary((id \
         27a78cc3-4784-4f8b-bf71-e7d1a3102670)(content(Whitespace\"\\n\"))))(Tile((id \
         741866cf-cad0-40fd-b420-efbb4e5c329c)(label(harvests))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         6ba479a8-47ee-4d65-ad60-83b1d386e97a)(content(Whitespace\" \
         \"))))(Tile((id \
         c0f19598-01dc-4e2e-a6c6-1b100d8039ed)(label(=))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 39))(sort Typ))((shape(Concave \
         39))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         c904635a-2562-4911-b5ce-c545a00dde36)(content(Whitespace\" \
         \"))))(Tile((id 6dc028e0-3426-4f0d-be00-8c8bc3a5a6d7)(label([ \
         ]))(mold((out Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape \
         Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
         b0ed4892-cc1b-4fcc-9d0e-4d324312b484)(label(Harvest))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children()))))))))(Tile((id \
         f1a393fe-c982-45ef-bdd6-a20f46f06805)(label(,))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 44))(sort Typ))((shape(Concave \
         44))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         4a40d42b-bdb6-43cc-9eb5-1a0996ea361a)(content(Whitespace\"\\n\"))))(Tile((id \
         2306e2fa-1052-4d74-ad9d-9ac807a46477)(label(totalValue))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         140dbf69-08fd-4866-820f-9f4980e34822)(content(Whitespace\" \
         \"))))(Tile((id \
         9d4ef415-9f52-4a02-9d58-af969d79fb3a)(label(=))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 39))(sort Typ))((shape(Concave \
         39))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         ba34c9e6-93a4-44a6-8006-8f7245bae490)(content(Whitespace\" \
         \"))))(Tile((id \
         99720af5-d1f1-4a93-a314-f1a02d024c8c)(label(Int))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         e3610f5f-5d04-4dde-9bbe-76716cd4988d)(label(,))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 44))(sort Typ))((shape(Concave \
         44))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         e7108539-77cf-44fc-95bd-2ae79821db58)(content(Whitespace\"\\n\"))))(Tile((id \
         29ad9f33-837c-4794-b720-7be6f8ddaa51)(label(streakBonus))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         7948cdf9-fbae-4a5e-a9b2-815e5f2ee505)(content(Whitespace\" \
         \"))))(Tile((id \
         5d8d0d4e-2988-41c7-8c27-2fe52305d10a)(label(=))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 39))(sort Typ))((shape(Concave \
         39))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         d7dac6b8-d159-4c79-83c1-31dc65f2bc29)(content(Whitespace\" \
         \"))))(Tile((id \
         eb9dace1-e35a-452c-b5f4-bc913b9db336)(label(Int))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         7d79391f-e80f-45bc-9dab-d6d4dd2da61c)(label(,))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 44))(sort Typ))((shape(Concave \
         44))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         d2b9b0be-0289-4c30-a545-23d7c14cf507)(content(Whitespace\"\\n\"))))(Tile((id \
         f17d22d4-d855-4934-93f4-57c0f9e6671b)(label(lastQuality))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         9f9e8115-1fd1-4dc0-94fa-b3fd2865fe7e)(content(Whitespace\" \
         \"))))(Tile((id \
         54f2fbaf-61a5-4059-8428-cc9e3472a582)(label(=))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 39))(sort Typ))((shape(Concave \
         39))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         63765f94-8448-4acb-a46d-aa1abef9d844)(content(Whitespace\" \
         \"))))(Tile((id \
         bd5bef9a-0486-456d-8d71-47db11d8ed5d)(label(Quality))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         e58bcc0f-86d9-41f3-acbd-dfc4a16992fd)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         0ad5ce72-f73d-4c11-833c-1b878ce3e3ef)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         41304859-6d6e-4606-b189-56d872ab23c9)(content(Whitespace\"\\n\"))))(Secondary((id \
         3aaa92de-2c0d-429a-b57e-c325c44cd82b)(content(Whitespace\"\\n\"))))(Secondary((id \
         b45d191a-00fa-41fa-9a88-17955652fec7)(content(Comment\"# Actions the \
         farmer can take #\"))))(Secondary((id \
         753eba69-1a21-4715-9aa7-10375472ccfa)(content(Whitespace\"\\n\"))))(Tile((id \
         01964322-a271-45fe-9e76-2ffad6969b12)(label(type = in))(mold((out \
         Exp)(in_(TPat Typ))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         7a59e24b-bd09-46fb-ae47-2dc88dfbe239)(content(Whitespace\" \
         \"))))(Tile((id \
         438907be-bbe1-4ee1-9cc8-184a3599fb9a)(label(Action))(mold((out \
         TPat)(in_())(nibs(((shape Convex)(sort TPat))((shape Convex)(sort \
         TPat))))))(shards(0))(children())))(Secondary((id \
         25775287-0e5f-40b0-9a4a-d4f8972e8552)(content(Whitespace\" \
         \")))))((Secondary((id \
         23eae999-a028-4d0c-a9be-e529a0bebd49)(content(Whitespace\"\\n\"))))(Tile((id \
         d3b76759-355d-4c62-bad9-bd4c5216c6d0)(label(+))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape(Concave 33))(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         795374ce-c5e9-496e-97fc-df48ed2f9f70)(content(Whitespace\" \
         \"))))(Tile((id \
         89b81b59-2114-498c-8363-1aece7e8dfe5)(label(RecordHarvest))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         4a786f8a-a415-407b-9222-803088e7b028)(label(\"(\"\")\"))(mold((out \
         Typ)(in_(Typ))(nibs(((shape(Concave 11))(sort Typ))((shape \
         Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
         d3fcc391-25d9-417b-a328-6378b8af5822)(label(Harvest))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children()))))))))(Secondary((id \
         43d8612d-3d15-4e46-a64e-29e437a74df9)(content(Whitespace\" \
         \"))))(Secondary((id \
         ebd565fc-4da0-4823-9562-992ddb7820c7)(content(Whitespace\" \
         \"))))(Secondary((id \
         300898c4-fb89-4682-a222-7ad48a9977c0)(content(Comment\"# Log a new \
         harvest #\"))))(Secondary((id \
         e3d7e958-33e6-41c1-909a-11b74f11f868)(content(Whitespace\"\\n\"))))(Tile((id \
         f1906e98-6f6e-4ab0-972f-7a4649fea881)(label(+))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 12))(sort Typ))((shape(Concave \
         12))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         5cd51416-778f-4050-bf2a-44b55af939a3)(content(Whitespace\" \
         \"))))(Tile((id \
         a427052e-89b0-4a81-84e4-bf16402b1810)(label(ClaimBonus))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         64cd5f7e-9886-48fd-8bd4-79ec67fbedf6)(content(Whitespace\" \
         \"))))(Secondary((id \
         337d7a62-979e-4685-8b24-f50a5f663418)(content(Whitespace\" \
         \"))))(Secondary((id \
         dec56ee4-71d2-490a-81f9-e34206d80ed6)(content(Whitespace\" \
         \"))))(Secondary((id \
         07c49abd-15ee-45b4-8e3c-873fcf3520e8)(content(Whitespace\" \
         \"))))(Secondary((id \
         cfc36585-873d-4469-8892-208f0c83cdbf)(content(Whitespace\" \
         \"))))(Secondary((id \
         5db1a547-4ce6-44f5-8764-1d5d9ba5108d)(content(Whitespace\" \
         \"))))(Secondary((id \
         bc7d5a45-b651-414f-b650-14f21d1638c4)(content(Whitespace\" \
         \"))))(Secondary((id \
         6b52f19e-21ca-40bb-9d2e-18e50f3e5fce)(content(Whitespace\" \
         \"))))(Secondary((id \
         9e6ed1ea-d21f-41a3-95bf-d439f9a92b04)(content(Whitespace\" \
         \"))))(Secondary((id \
         7877d1ea-7ea2-4d01-b44a-a7b5b334755a)(content(Whitespace\" \
         \"))))(Secondary((id \
         e1dce80c-37a1-4c74-ac48-df0bcd44e131)(content(Whitespace\" \
         \"))))(Secondary((id \
         c96bd0ad-7f79-4524-b327-2e712a72fbc8)(content(Whitespace\" \
         \"))))(Secondary((id \
         370262ea-35c5-457c-8695-d06caf7cf523)(content(Whitespace\" \
         \"))))(Secondary((id \
         1745ab5a-a08f-48fb-ad3b-2cc1a06e3fb3)(content(Whitespace\" \
         \"))))(Secondary((id \
         bbe9c1bc-079e-423a-bcd9-8a6b5f7f0b7a)(content(Comment\"# Collect \
         accumulated streak bonus #\"))))(Secondary((id \
         650bb65e-ebab-4a50-be33-fc5f63eecea2)(content(Whitespace\"\\n\"))))(Tile((id \
         3e706bb0-1781-4038-a5fb-8f87df0cc2b1)(label(+))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 12))(sort Typ))((shape(Concave \
         12))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         c2306784-d68e-457d-a37c-8a71cd96c460)(content(Whitespace\" \
         \"))))(Tile((id \
         c78d9467-2f78-44e4-8a62-111f84d4c1de)(label(CloseDay))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         4efe6605-8c0f-4c0f-8460-517a0fc0e5f5)(content(Whitespace\" \
         \"))))(Secondary((id \
         6aa561a1-984c-4d4c-9029-429f7913a8f0)(content(Whitespace\" \
         \"))))(Secondary((id \
         1885b8a3-ebeb-4578-8759-cc5ef390cd30)(content(Whitespace\" \
         \"))))(Secondary((id \
         32895c42-8ec6-42e8-9871-5bc79b3b7e63)(content(Whitespace\" \
         \"))))(Secondary((id \
         2983be4f-fc3d-41c0-acbb-b56693de699d)(content(Whitespace\" \
         \"))))(Secondary((id \
         c76cef36-21b7-481b-81b7-fb6bb162308e)(content(Whitespace\" \
         \"))))(Secondary((id \
         bcf5d8f7-074b-41b3-8057-55bc2a21bfe5)(content(Whitespace\" \
         \"))))(Secondary((id \
         6f88a524-1954-4b42-b5fa-fc5c968e5504)(content(Whitespace\" \
         \"))))(Secondary((id \
         4aa4b67a-238d-41ae-a2e7-0a61b37f987c)(content(Whitespace\" \
         \"))))(Secondary((id \
         dac732aa-1a0f-4b0f-ad55-29306aff342c)(content(Whitespace\" \
         \"))))(Secondary((id \
         8e8cda0b-a1a6-48fd-a6cf-a024b5e9868a)(content(Whitespace\" \
         \"))))(Secondary((id \
         e7228be5-5f9e-414e-8c19-81715b7fcf18)(content(Whitespace\" \
         \"))))(Secondary((id \
         83e3bb85-7379-4c32-b9be-c3088c44b482)(content(Whitespace\" \
         \"))))(Secondary((id \
         bdb54ab2-aa99-419b-97eb-4ddafb3c786c)(content(Whitespace\" \
         \"))))(Secondary((id \
         394e9c44-6ba4-4183-9f29-6ba613e632fd)(content(Whitespace\" \
         \"))))(Secondary((id \
         a30e32e8-11f3-402c-849c-6375300bb068)(content(Whitespace\" \
         \"))))(Secondary((id \
         cf01d4e6-4d19-4564-a140-4a2d7925291a)(content(Comment\"# End the \
         harvest day, reset streaks #\"))))(Secondary((id \
         5ac6b0d1-9f77-4ebd-a572-a4ea088b8ff7)(content(Whitespace\"\\n\"))))(Secondary((id \
         77917fb5-d03c-43e0-a6e3-cb317b75d26d)(content(Comment\"# TODO: Add \
         PremiumSale here #\"))))(Secondary((id \
         8c7aaa4c-5361-4566-b2a4-7b96f641e454)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         86ef421d-0380-4e87-937a-95582d886370)(content(Whitespace\"\\n\"))))(Secondary((id \
         5cfe30ca-14bc-44da-8160-5400a8b21461)(content(Whitespace\"\\n\"))))(Secondary((id \
         f72bdf4a-0465-4e5e-8f0a-c6af7d9ced29)(content(Comment\"# Calculate \
         base value of a crop #\"))))(Secondary((id \
         b9c13b85-8b97-4674-bc93-4c76e01c4556)(content(Whitespace\"\\n\"))))(Tile((id \
         bce96391-7c99-4fbe-8a45-62437c421215)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         4ae11f5b-4d54-49d1-9ee6-dc42240633b0)(content(Whitespace\" \
         \"))))(Tile((id \
         58cf2423-17ad-4940-beb9-b79b253517fb)(label(cropValue))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         1f40208f-45e5-4dd5-9266-e53922c7a3c1)(content(Whitespace\" \
         \"))))(Tile((id \
         47ea21ba-e4f5-470a-8362-df1c29a99d21)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 24))(sort Pat))((shape(Concave \
         24))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         a5ea6b70-54dd-4e92-8026-3a0a9e81147c)(content(Whitespace\" \
         \"))))(Tile((id \
         584e4dcc-d93a-416a-9800-fc39b0ab4381)(label(Crop))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         9601b851-6283-4031-8790-f61f4fdfc2bb)(content(Whitespace\" \
         \"))))(Tile((id \
         3588c579-c850-4b04-a9fb-728fd5cf1a3f)(label(->))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 13))(sort Typ))((shape(Concave \
         13))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         6f72ffe7-41be-42a0-8810-5f6656cfef9f)(content(Whitespace\" \
         \"))))(Tile((id \
         c9478bdd-c93d-48d0-a831-6b24ef4bb573)(label(Int))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         a229e971-e8b9-4c65-9ca3-968c57ddbd73)(content(Whitespace\" \
         \")))))((Secondary((id \
         292e90d2-9e9b-41e8-aa50-4d89c7d05230)(content(Whitespace\"\\n\"))))(Tile((id \
         ef369eb9-c139-4256-975c-09cbf009fc36)(label(fun ->))(mold((out \
         Exp)(in_(Pat))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         37))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         98bf2233-6fec-4de6-9678-aaa98f1546bd)(content(Whitespace\" \
         \"))))(Tile((id \
         955a9a35-7e00-43f9-a07e-86378f2fba71)(label(c))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         b9bc2730-ed04-4709-ae38-5ad68b632efa)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         49ea3009-cb86-4221-9714-afdb1fcaa1f7)(content(Whitespace\"\\n\"))))(Tile((id \
         1a9c141f-1afd-4a0a-a8dd-ba684278ed07)(label(case end))(mold((out \
         Exp)(in_(Rul))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         d2df2c90-3375-4877-a863-f092e9ed0363)(content(Whitespace\" \
         \"))))(Tile((id \
         3d81d3c5-931c-40d5-9080-a310d94050b9)(label(c))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         eb92b16c-e8a9-4148-a209-f6a85571cb90)(content(Whitespace\"\\n\"))))(Tile((id \
         7fb324a1-ac85-49fd-a72f-e646c5b5deb6)(label(| =>))(mold((out \
         Rul)(in_(Pat))(nibs(((shape(Concave 46))(sort Exp))((shape(Concave \
         46))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         a672374a-d5cf-4ec7-abfe-7c4c29471c92)(content(Whitespace\" \
         \"))))(Tile((id \
         25f9c7d0-4c07-442b-a9eb-8537f134fbd5)(label(Moonmelon))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         38bfd7df-1a34-4823-bdc0-83315ae1072e)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         8e266d03-2af8-4595-9679-9d4fcb426f4e)(content(Whitespace\" \
         \"))))(Tile((id \
         2908a2e1-31de-4dcc-b041-1cce18d72fea)(label(15))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         659cd8e3-0bff-4622-8621-3bea3cf1034c)(content(Whitespace\"\\n\"))))(Tile((id \
         6e79b631-3c26-4245-ac80-f5aae61070da)(label(| =>))(mold((out \
         Rul)(in_(Pat))(nibs(((shape(Concave 46))(sort Exp))((shape(Concave \
         46))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         bef69c61-dd7f-4755-90f2-af7621691715)(content(Whitespace\" \
         \"))))(Tile((id \
         8ddfa6bc-b452-4e03-894c-8cc65f76a076)(label(Starfruit))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         0919c9c2-9d36-4b2d-a3b1-505d3bf08471)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         97c61301-de7b-4128-b8de-79241a6ea92e)(content(Whitespace\" \
         \"))))(Tile((id \
         03363956-6e66-4285-9cd7-9488e950528d)(label(20))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         a1d1ae44-dcee-41ee-b853-955aeae92f60)(content(Whitespace\"\\n\"))))(Tile((id \
         6d05cd70-5da3-4467-a0f0-6532376d956d)(label(| =>))(mold((out \
         Rul)(in_(Pat))(nibs(((shape(Concave 46))(sort Exp))((shape(Concave \
         46))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         e8390966-9e25-49d5-9122-0536636275d0)(content(Whitespace\" \
         \"))))(Tile((id \
         6921cbf2-b675-4b65-acec-226183c8e00f)(label(Nightberry))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         0f5e9323-7770-4843-9d95-fba1e4af6805)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         6fdf6c2e-d4c1-4d2a-91be-1dc95f68c12b)(content(Whitespace\" \
         \"))))(Tile((id \
         3bc0ec39-1852-430b-8638-eeced9d6d378)(label(25))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         9affbe25-848b-43a6-bc5f-f3d37a8afbe4)(content(Whitespace\"\\n\"))))(Tile((id \
         05e6cd66-7881-4889-af71-c09250ad7d74)(label(| =>))(mold((out \
         Rul)(in_(Pat))(nibs(((shape(Concave 46))(sort Exp))((shape(Concave \
         46))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         e524b1b3-d8c4-4335-a2cc-30f5a73921db)(content(Whitespace\" \
         \"))))(Tile((id \
         e5e7fe03-57ed-4ac5-bcb0-6d63af1eec45)(label(Duskwheat))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         f15f5bdd-d4d6-4722-99e5-fcca9cffe359)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         98b5aad8-7143-4043-935e-2e34fd81f285)(content(Whitespace\" \
         \"))))(Tile((id \
         4ad03ef8-e6a7-43f1-9a8d-a6fbda8f45bf)(label(10))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         777ddacc-ce1a-4959-b9d1-e8b32e29af85)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         ba260099-559f-4e80-8f3e-e78c70893259)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         b4748573-28dc-4cee-a9bc-bceae10e66c7)(content(Whitespace\"\\n\"))))(Secondary((id \
         60268880-9681-4b9d-a36f-13a608dcf8d7)(content(Whitespace\"\\n\"))))(Secondary((id \
         592d5b52-97d6-498e-9359-8bd1191f4e15)(content(Comment\"# Quality \
         multiplier for harvest value #\"))))(Secondary((id \
         d96f6eba-991c-4254-9090-83b8b1cd5282)(content(Whitespace\"\\n\"))))(Tile((id \
         c9fb72aa-7a14-4e29-b6b8-cbfd2ac9fedf)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         34190926-7024-4f28-91a2-3f412d7e2c36)(content(Whitespace\" \
         \"))))(Tile((id \
         8bc97efd-101b-4f95-bb17-b8fea7c4200c)(label(qualityMultiplier))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         18d57e7e-fbdc-4c0a-b1f3-db31dfcb425c)(content(Whitespace\" \
         \"))))(Tile((id \
         0d2d5e64-a79d-4429-85d4-663ba414eeec)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 24))(sort Pat))((shape(Concave \
         24))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         239179a3-5383-430a-bdeb-731b00bd7040)(content(Whitespace\" \
         \"))))(Tile((id \
         e9d6d173-a193-4116-8a2b-8e86b9e2e4e5)(label(Quality))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         2e5bd5d7-47ac-4879-8b39-0472670100d6)(content(Whitespace\" \
         \"))))(Tile((id \
         ab1adf9e-4c99-4b0a-a504-6ce80173353d)(label(->))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 13))(sort Typ))((shape(Concave \
         13))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         05887520-f0ce-4f37-b7d5-466eed630d91)(content(Whitespace\" \
         \"))))(Tile((id \
         e37aad6c-a634-4b29-a25e-5e4ad86ba6b8)(label(Int))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         437fbbe5-0b29-4870-a2df-352222ccb20e)(content(Whitespace\" \
         \")))))((Secondary((id \
         5911084b-4970-49fa-ab09-2e4c7d14b158)(content(Whitespace\"\\n\"))))(Tile((id \
         866188b0-b51d-4109-8046-380503fada92)(label(fun ->))(mold((out \
         Exp)(in_(Pat))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         37))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         44669583-43e3-4538-965b-7b6deda50429)(content(Whitespace\" \
         \"))))(Tile((id \
         03493857-3861-4b05-973a-2261e77801fe)(label(q))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         2f06fda9-f260-4f92-b678-e38059bdb7f4)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         885ece06-b36c-4687-8c41-8744d70f1a35)(content(Whitespace\"\\n\"))))(Tile((id \
         be20e6fc-7490-416e-9c2d-cc71c87e6a9a)(label(case end))(mold((out \
         Exp)(in_(Rul))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         b2fd7415-f56d-46c2-9a39-c5feb8fd5170)(content(Whitespace\" \
         \"))))(Tile((id \
         f025cd95-9ee1-4843-ad2a-132e9c69fa03)(label(q))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         51a72a81-bd63-4b9f-9ac8-3ba010593532)(content(Whitespace\"\\n\"))))(Tile((id \
         e2fd45a3-fb80-4650-b329-fb80911a6bae)(label(| =>))(mold((out \
         Rul)(in_(Pat))(nibs(((shape(Concave 46))(sort Exp))((shape(Concave \
         46))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         bb46d923-e870-4a10-b1aa-4c8ae530a369)(content(Whitespace\" \
         \"))))(Tile((id \
         ab375d61-5abd-4aa9-911a-bc3a3974a330)(label(Bronze))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         c2a7c58f-f806-4e11-9121-8deacd53c3b7)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         404a4cef-f22c-4084-822a-ce23dd67399d)(content(Whitespace\" \
         \"))))(Tile((id \
         04b190fd-ff64-4fa5-a3d0-c898069f3d05)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         05b316d5-b767-4453-a7f2-19e83a76622a)(content(Whitespace\"\\n\"))))(Tile((id \
         3173e291-7a79-42a1-ae20-52791e185404)(label(| =>))(mold((out \
         Rul)(in_(Pat))(nibs(((shape(Concave 46))(sort Exp))((shape(Concave \
         46))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         2e100bec-8f11-4c57-8f56-b4918a6f3ff9)(content(Whitespace\" \
         \"))))(Tile((id \
         1ddd5ef0-1e4f-4b38-a0a7-829a9e2d8dc8)(label(Silver))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         cfb09db4-4ea2-42ab-bd45-33c1f721f044)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         b75330ac-0ca2-4147-b01a-b40f8f5e362d)(content(Whitespace\" \
         \"))))(Tile((id \
         bc8d408d-0990-481a-9c17-2c4728a9dd12)(label(2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         4b63de62-fb24-46d1-952e-ca841e7828e5)(content(Whitespace\"\\n\"))))(Tile((id \
         6b7a4f25-e8b4-4d4d-8c22-5a9b9dcb7ff5)(label(| =>))(mold((out \
         Rul)(in_(Pat))(nibs(((shape(Concave 46))(sort Exp))((shape(Concave \
         46))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         01a2d2a2-92af-4f29-a463-ba737d156fb0)(content(Whitespace\" \
         \"))))(Tile((id \
         a7e79016-c366-446b-a7a5-5c355083dc47)(label(Gold))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         70f33074-ba3f-4096-86b3-6852a670f504)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         615b6a37-b69e-45c4-b354-09d97706ea82)(content(Whitespace\" \
         \"))))(Tile((id \
         8b3ee17d-12c3-4668-8a6d-deeb9b53fb65)(label(3))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         895622f6-3c89-4ec5-bb94-96cfd5b571d9)(content(Whitespace\"\\n\"))))(Tile((id \
         296c44be-f2b7-4a2f-92e3-40911c433eb4)(label(| =>))(mold((out \
         Rul)(in_(Pat))(nibs(((shape(Concave 46))(sort Exp))((shape(Concave \
         46))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         92e2b78d-9ca3-4aed-8eb7-4e6b5890f082)(content(Whitespace\" \
         \"))))(Tile((id \
         7ff518e2-3a69-49a8-b3ba-c8487d776f00)(label(Starlight))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         b48a7e95-1a13-42ee-a9d0-bc5330599bef)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         a513e006-a2c8-4b62-a5dc-58c2bb084a2e)(content(Whitespace\" \
         \"))))(Tile((id \
         27297393-af29-406c-969e-0b26f7690131)(label(5))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         45235f2b-1e2c-4e9e-9a17-db3d9f51836f)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         e6ad8643-5aae-422c-a801-e1d9ed90200d)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         38927bd4-c11a-443f-aa91-ca11d9e028af)(content(Whitespace\"\\n\"))))(Secondary((id \
         3c0f0404-2b4c-4857-8c05-5fe4000fdd74)(content(Whitespace\"\\n\"))))(Secondary((id \
         4f0eaa0e-0613-4696-88b0-dfa095dd988d)(content(Comment\"# Calculate \
         the value of a single harvest #\"))))(Secondary((id \
         d49486b3-e89a-4a9e-a1af-4b14b391e72e)(content(Whitespace\"\\n\"))))(Tile((id \
         6fecab8c-61a9-4c97-9a91-0f0a649f4071)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         93911d12-3115-46a5-bf1b-cdaa6b910d63)(content(Whitespace\" \
         \"))))(Tile((id \
         bc6547b9-c60e-46a1-a5e8-976d6c9802fb)(label(harvestValue))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         69f66920-5f25-4e87-80cd-1d16d0112b8a)(content(Whitespace\" \
         \"))))(Tile((id \
         cfd432d0-5eb4-4d39-a998-02eb90aab9c1)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 24))(sort Pat))((shape(Concave \
         24))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         ea81c59c-e6d5-434d-a10a-3afc8b94aaf0)(content(Whitespace\" \
         \"))))(Tile((id \
         380d819e-74d5-44bb-afdc-ae0d28671016)(label(Harvest))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         92e6b89b-daa9-4d1d-bc44-79fd01c7dfcb)(content(Whitespace\" \
         \"))))(Tile((id \
         0b0b345a-9ac4-4717-87f3-c6fc223d2379)(label(->))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 13))(sort Typ))((shape(Concave \
         13))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         86cf02e9-3224-489c-b091-e8c46e0b45ef)(content(Whitespace\" \
         \"))))(Tile((id \
         a5164a84-143b-4725-8463-eb105987c1ca)(label(Int))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         dc87d01f-3218-468a-b130-85b46d963022)(content(Whitespace\" \
         \")))))((Secondary((id \
         dbc6c249-36ca-4542-bf24-dc6169f85f33)(content(Whitespace\"\\n\"))))(Tile((id \
         736f9946-ad97-4aa9-a9d5-c223a3053c87)(label(fun ->))(mold((out \
         Exp)(in_(Pat))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         37))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         7efb5366-0aa3-47ba-bc0b-8beaabf570df)(content(Whitespace\" \
         \"))))(Tile((id \
         7da15190-fa23-4013-8774-a7ea8d66afdc)(label(h))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         8c819c67-6075-4f03-99f0-44ffab2aaafd)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         fb63cdf4-1a22-454d-8f38-1928d21024ae)(content(Whitespace\"\\n\"))))(Tile((id \
         c151c265-7fbd-4d5a-a98c-55f92202c321)(label(cropValue))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         5e176a40-3c11-443b-ab45-0ceb0d8b07c8)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         58856a18-3ae2-453b-acdd-a6e454ff3bf7)(label(h))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         53dd6d5e-7299-4a8b-8b5e-92859952e52f)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         0203592d-8123-44f5-8d05-a7ab0e64476e)(label(crop))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         59a63bdc-311e-48f8-a08f-c02bff132534)(content(Whitespace\" \
         \"))))(Tile((id \
         754ee913-7794-4791-a4ac-22f16711a4bf)(label(*))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 27))(sort Exp))((shape(Concave \
         27))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         32980c90-5fac-49f4-b739-fcd0762c4e53)(content(Whitespace\" \
         \"))))(Tile((id \
         3806b33f-56de-4f69-a4d2-443a9883524b)(label(qualityMultiplier))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         a747918c-cf97-4c66-946e-36af1156b056)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         e43e5d55-0451-4f49-a674-9af7bc6abe59)(label(h))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         7b656f07-ec81-4769-97b5-ebed91f41586)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         6a13b8fa-ee3b-430a-8eac-7e9ff8ce3aa6)(label(quality))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         087c1bbb-17d1-48d7-bd26-58a019f95265)(content(Whitespace\" \
         \"))))(Tile((id \
         3ea32a58-2b6e-40cc-a223-9e15906ec7dd)(label(*))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 27))(sort Exp))((shape(Concave \
         27))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         f80af781-ca39-4a70-8249-5d298e373bda)(content(Whitespace\" \
         \"))))(Tile((id \
         f3809ecf-c86c-420c-b50e-8ea28c20d1d9)(label(h))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         1588330a-1241-4703-89ea-28a2d843c8df)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         5042ac4c-435b-45b8-ba86-91f9f3a11abb)(label(quantity))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         ff8382eb-e4c8-4ba0-b289-86d970a5dff5)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         5355bd45-4bf6-49f9-80bd-06c7f651d940)(content(Whitespace\"\\n\"))))(Secondary((id \
         36e9c9d7-22df-44e9-9acb-78cab00ae67d)(content(Whitespace\"\\n\"))))(Secondary((id \
         1a357ba2-8cef-40b8-b5dc-8cfff877b6a8)(content(Comment\"# Initial \
         empty ledger #\"))))(Secondary((id \
         911814f9-8dda-44d4-b816-3a4df8e7993d)(content(Whitespace\"\\n\"))))(Tile((id \
         bf7bf340-a57c-409b-9acf-ebf2721530a4)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         e3ed2d55-8f27-4694-b7b9-5bcfe524b3c6)(content(Whitespace\" \
         \"))))(Tile((id \
         7467bbb9-3cfd-4766-93c9-e7d3f0c8b33c)(label(initModel))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         68265dfd-7190-4de3-ad15-0811c9d144f7)(content(Whitespace\" \
         \"))))(Tile((id \
         35e70967-6918-4e63-9be1-3d0f9c6be2d2)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 24))(sort Pat))((shape(Concave \
         24))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         793a111b-0e41-4247-a94d-b716c0d998e7)(content(Whitespace\" \
         \"))))(Tile((id \
         7c35f88d-267a-4419-81fa-44de119175d5)(label(Model))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         9adeecd6-8974-40a6-87d1-3a13834bf136)(content(Whitespace\" \
         \")))))((Secondary((id \
         4d27e2a0-0aed-46c9-85f1-05f1e034bdc5)(content(Whitespace\" \
         \"))))(Tile((id \
         2672c441-e42f-44e9-a476-33e35060c39e)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         a322b4e1-4ab4-4fd9-b524-5e600a7b79c3)(content(Whitespace\"\\n\"))))(Tile((id \
         654b5fba-b487-4371-9dcb-0c3befd17850)(label(harvests))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         aabaf0c6-5566-49c6-994d-fa6359b9e02f)(content(Whitespace\" \
         \"))))(Tile((id \
         dc2f8286-d18c-4859-bcb2-005fe1e6c1fb)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         654e71a8-a71d-4ee6-8852-e67801b7ea5a)(content(Whitespace\" \
         \"))))(Tile((id \
         551c56f2-be1d-44a5-97b6-c2efc51a2a97)(label([]))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         5e865c2c-3a1c-4073-b597-0c5a2074e8a8)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         4185e8ce-6efc-40a5-922b-87ebcb2b0875)(content(Whitespace\"\\n\"))))(Tile((id \
         5023dbe9-b683-4398-8aff-2ee32a3a0141)(label(totalValue))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         a8f63705-64e4-4460-af30-1d083c2a2958)(content(Whitespace\" \
         \"))))(Tile((id \
         023a5a88-7cb2-4300-b176-60eeadf1627c)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         c6cb8f4d-a441-41cb-a5d8-07f15f212fb3)(content(Whitespace\" \
         \"))))(Tile((id \
         29e2cd41-c66a-43c2-ba7b-d79fead92e0b)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         e5b20e39-3e20-488f-b107-c1f245dd4e1c)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         9542d909-8857-4a55-8962-d70bb0287ed6)(content(Whitespace\"\\n\"))))(Tile((id \
         221b6c39-6a02-47c7-a72a-050ca8bb7d3e)(label(streakBonus))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         c9210ad5-44f1-4858-a279-1c2dae730097)(content(Whitespace\" \
         \"))))(Tile((id \
         af27a075-9e90-4598-aada-e3245e5ec517)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         2ce93c6f-ee03-4a8a-9bdf-ff2ee495c2b4)(content(Whitespace\" \
         \"))))(Tile((id \
         708686c8-70f2-408d-9a27-bc4d3e2c66d3)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         261610d3-a5fe-4a2a-8749-f480e86f23c9)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         57ae6672-86d4-4e61-addb-bb8df5688dbb)(content(Whitespace\"\\n\"))))(Tile((id \
         d79312ac-5cae-4e42-a528-25d637b8ccfa)(label(lastQuality))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         a7163d2d-bace-4a26-8406-6f17a3509eac)(content(Whitespace\" \
         \"))))(Tile((id \
         d02790e7-c041-40d6-97e3-28170e6f6f46)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         00ad3e0d-b615-4461-af1f-aeae9ae87b6e)(content(Whitespace\" \
         \"))))(Tile((id \
         9b6fcfd5-56db-42a5-a925-6c6b4fb7bfb1)(label(Bronze))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         764005a6-dbe1-43dd-a647-aaac20a02f9f)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         0d07ae6b-4007-4ba3-a4eb-fa9a4e761077)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         b942b365-6fa5-4fd8-85c4-f44cc4253c3d)(content(Whitespace\"\\n\"))))(Secondary((id \
         2efd032a-50fd-49af-b85a-517cd62a1356)(content(Whitespace\"\\n\"))))(Secondary((id \
         b4e7e6fb-013c-49dc-bdab-b63a9fe77f46)(content(Comment\"# Process a \
         harvest action and update the ledger #\"))))(Secondary((id \
         c851349b-993c-41f9-8ee2-e5393113109b)(content(Whitespace\"\\n\"))))(Tile((id \
         c02840ad-92bf-46d8-b888-3e2fdce00fb5)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         c17c5c2c-5107-4148-96d0-24a444cf3eee)(content(Whitespace\" \
         \"))))(Tile((id \
         64f59dcd-ef6b-4e6a-9ffd-a855ba62540c)(label(processHarvest))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         00d4f1f5-83a5-4abd-894b-012a2f851ae6)(content(Whitespace\" \
         \"))))(Tile((id \
         01125d18-4e4d-41b2-96c6-688f75cb1a5d)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 24))(sort Pat))((shape(Concave \
         24))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         35b72ff7-c551-4f25-a74d-a9ecde360f42)(content(Whitespace\" \
         \"))))(Tile((id \
         45aecccd-9d1e-4b33-a791-26304976de61)(label(\"(\"\")\"))(mold((out \
         Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0 1))(children(((Tile((id \
         1ae122ba-d00e-4020-9694-6a151f4065ce)(label(Model))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         4b57e43e-c8de-4d47-9505-6c19ccfbdb1c)(label(,))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 44))(sort Typ))((shape(Concave \
         44))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         115ab7a9-b54b-4805-82d0-d58c7e41db87)(content(Whitespace\" \
         \"))))(Tile((id \
         2aaf6f80-f7a5-465d-b3ac-6d1e675fc2f7)(label(Harvest))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children()))))))))(Secondary((id \
         b5656bef-811e-481e-8879-1e7ed588a9cf)(content(Whitespace\" \
         \"))))(Tile((id \
         92c6f33b-3a2c-4bc0-9573-b370c7b09a3c)(label(->))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 13))(sort Typ))((shape(Concave \
         13))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         5d7e62db-34d9-48db-ab4e-1271e571ff25)(content(Whitespace\" \
         \"))))(Tile((id \
         e3c64d88-9b64-4464-aa2e-826b6c3fa086)(label(Model))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         7d4d3433-fc46-4167-bf58-f9d535dc3679)(content(Whitespace\" \
         \")))))((Secondary((id \
         88a5baab-f92d-40a2-85bf-c65923aba2b1)(content(Whitespace\"\\n\"))))(Tile((id \
         f4f2a945-e175-4542-9945-0e3056282ca9)(label(fun ->))(mold((out \
         Exp)(in_(Pat))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         37))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         4af1482f-d3de-4062-bc69-7c8452859d6a)(content(Whitespace\" \
         \"))))(Tile((id \
         c461baba-b639-4f63-b128-f99979e0e5ee)(label(\"(\"\")\"))(mold((out \
         Pat)(in_(Pat))(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0 1))(children(((Tile((id \
         5aafdbf6-9395-4e1b-bfa6-1d5bb12e31a2)(label(ledger))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         7e7ac2af-ee18-4806-9def-76f3d9339e14)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 44))(sort Pat))((shape(Concave \
         44))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         43d7c3cd-f93b-41d9-bf2a-adc8d1d8f5d3)(content(Whitespace\" \
         \"))))(Tile((id \
         b7b3c576-9129-49f0-afad-563e13bda7f4)(label(h))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children()))))))))(Secondary((id \
         f75a4257-6cc0-4e4b-82f2-827ddcdce64d)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         6bce7265-024a-449e-9610-018f764e14cb)(content(Whitespace\"\\n\"))))(Tile((id \
         061d8b3b-0048-4ca8-a623-c76403d7d630)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         b38980fd-46e3-4944-8d21-887df18301c0)(content(Whitespace\" \
         \"))))(Tile((id \
         cfa69355-ff0f-4781-9b58-fc7614e1e908)(label(value))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         7d2c1718-1bff-4168-a918-779fbd1bb546)(content(Whitespace\" \
         \")))))((Secondary((id \
         63c060f7-8070-4247-b92a-ca6faa2e190a)(content(Whitespace\" \
         \"))))(Tile((id \
         5fcdaa42-7cbb-4e02-84bf-a965ef0f8ff6)(label(harvestValue))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         eb2d6c5b-f2b2-4476-bd7a-26ee6e867b44)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         360e7336-6e64-43d2-8482-e004674b3f4e)(label(h))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         b9919c77-27c3-4b7a-8c8b-85a48d3e736a)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         729e214f-e68a-4317-b548-f9ed6c4d8757)(content(Whitespace\"\\n\"))))(Secondary((id \
         66812a27-cffb-4bc8-9829-dca3ca93ee46)(content(Comment\"# Check if \
         this harvest continues the quality streak #\"))))(Secondary((id \
         fa18387a-5e2e-4d4a-964c-dbf8d62a3940)(content(Whitespace\"\\n\"))))(Secondary((id \
         fbf1a7fa-3fcd-4474-a543-1b2c3725e3d3)(content(Comment\"# First \
         harvest never continues a streak (no previous harvest) \
         #\"))))(Secondary((id \
         ace7877a-c76b-405b-8d54-a4c1e1cde462)(content(Whitespace\"\\n\"))))(Secondary((id \
         10e7158b-a377-40bc-8383-9366e2f30f91)(content(Comment\"# Compare \
         current quality with the PREVIOUS lastQuality #\"))))(Secondary((id \
         52f29cb7-99d3-4cfe-8993-abe835104fd8)(content(Whitespace\"\\n\"))))(Tile((id \
         5f7a73e0-da27-49a3-9ac2-a56fd0e3022b)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         ed82b37b-bf4a-4904-8a55-b295cc777ace)(content(Whitespace\" \
         \"))))(Tile((id \
         bad9a243-fae2-44b5-b6d7-102f9f9998ff)(label(isFirst))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         77bad015-53fc-4dfa-929f-b4d14744acf3)(content(Whitespace\" \
         \")))))((Secondary((id \
         5a2ea6af-d528-4f7f-85f1-2c24930d7d14)(content(Whitespace\" \
         \"))))(Tile((id \
         76e41672-4490-4c26-9e92-806b9d43ece9)(label(length))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         3d5ef40a-055e-46ef-bbb3-a9a3c20138a1)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         8cf39be1-52ec-4574-9f30-23db2423878a)(label(ledger))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         a4157567-502a-4379-930d-f69c0da5e790)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         06cb866e-4faf-468c-bed5-70ccc5f5f9bf)(label(harvests))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         cfa9e6db-b275-4701-95d8-3057462b1c05)(content(Whitespace\" \
         \"))))(Tile((id \
         9c8cd374-0a09-4047-8642-ff293c25d944)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         e825daa7-89a5-4ccf-aa9a-f795a1c28943)(content(Whitespace\" \
         \"))))(Tile((id \
         2ca9bae7-3430-4ace-a9ff-9578a51cbba9)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         185b0446-8904-4c55-8c81-9c315c3fa68b)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         c1583336-6c5a-4024-a3e2-dd6cd12eb993)(content(Whitespace\"\\n\"))))(Tile((id \
         7133255e-eb5b-4cdc-bccc-b6fddb1b59d0)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         8682537f-ffd4-48a0-b67f-ddfdf93c4d66)(content(Whitespace\" \
         \"))))(Tile((id \
         6805f6a9-082e-40fb-abb6-a4da2ab82c99)(label(continues))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         cfe5218b-e10e-4778-a845-c9660a53b7af)(content(Whitespace\" \
         \")))))((Secondary((id \
         3fe6eaca-2fb6-4405-8a0f-aea2ff232850)(content(Whitespace\" \
         \"))))(Tile((id \
         dcc9a753-648b-4f83-99c4-2713c7be2db1)(label(!))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape(Concave 27))(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         57f58c29-8372-45f8-b9a3-7c2cadead313)(label(isFirst))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         292521c7-2062-46a0-9c55-3696094abbc6)(content(Whitespace\" \
         \"))))(Tile((id \
         fd4db371-41a7-4f2a-9312-5360ad2ee0e7)(label(&&))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 32))(sort Exp))((shape(Concave \
         32))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         1eac0f25-738a-4161-ac47-6f1760ca7fd1)(content(Whitespace\" \
         \"))))(Tile((id \
         86351c30-4194-45a9-bce8-3a6765ef937f)(label(h))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         1673d86b-4b9e-4fbd-9176-5cf21249c7e3)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         71418b9c-8e59-4094-bf50-cca11f977de3)(label(quality))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         99ec9bd0-9abc-4b4f-b609-256162a36ae1)(content(Whitespace\" \
         \"))))(Tile((id \
         3dd33f35-06b6-4101-8327-02e11a435515)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         4a51a42d-e464-4113-a1f3-ba2f15a19608)(content(Whitespace\" \
         \"))))(Tile((id \
         ea011905-7d25-4f5f-82ed-2ad32aadfa9b)(label(ledger))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         6faefe2c-c3f3-450d-9321-da903c87d4c1)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         88b424a5-d895-498a-8326-a648a9978d84)(label(lastQuality))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         74ea52e4-742b-4fdf-8d6e-badf4d9343cc)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         540d7209-8b79-48b2-a0fe-f548d6fa0953)(content(Whitespace\"\\n\"))))(Secondary((id \
         d33cd731-b21a-47bd-aed8-50b2f26057cc)(content(Comment\"# Now update \
         lastQuality to current harvest #\"))))(Secondary((id \
         3751e861-fe78-463c-95e4-7ab34cf967fd)(content(Whitespace\"\\n\"))))(Tile((id \
         67af7870-6a34-48da-a418-22fe0bcd30b7)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         81b8f906-fdbd-48d7-bb5a-199218d85e6b)(content(Whitespace\" \
         \"))))(Tile((id \
         5927e697-6b28-4d32-af88-1e5345900930)(label(newLast))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         c4936826-eac4-42ec-84be-d51819e0ecd6)(content(Whitespace\" \
         \")))))((Secondary((id \
         5865312b-09c0-49ef-b887-e379d91a2505)(content(Whitespace\" \
         \"))))(Tile((id \
         79c994f3-554c-498e-bbc6-015d5d425016)(label(h))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         ac40996d-d010-455a-a0e0-aff2efdd2c74)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         8c247c33-f498-410a-af74-b25b35d10985)(label(quality))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         e628afa2-fbca-4344-b0ae-6272f23bd8c8)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         22451857-f895-44e7-9adf-8959e0a95f7e)(content(Whitespace\"\\n\"))))(Tile((id \
         2c4bbaf7-01e4-4bf5-b1be-dd23ae102ec9)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         f9955495-9d80-41e1-9ce8-368728f3d7c5)(content(Whitespace\" \
         \"))))(Tile((id \
         f0d49394-b25c-46af-b538-24ba8bbb7a7b)(label(newStreak))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         e6f5cb7e-4a24-4d7a-b7ff-ca7814405ccf)(content(Whitespace\" \
         \")))))((Secondary((id \
         db1ba432-c209-44b5-9879-1f53cc3bc28b)(content(Whitespace\"\\n\"))))(Tile((id \
         12c3137f-7ec4-4fe7-b47f-69365a8e6734)(label(if then else))(mold((out \
         Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         36))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         8af261a2-de09-4d4c-87cc-d06f86c5ee54)(content(Whitespace\" \
         \"))))(Tile((id \
         87f91bab-e544-4817-9b03-465cec816aee)(label(continues))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         2ce26920-eb2c-4c28-8ebb-49c7e5e58710)(content(Whitespace\"\\n\")))))((Secondary((id \
         e991b4ed-7042-43ea-b6c7-78b4cb0f185e)(content(Whitespace\" \
         \"))))(Tile((id \
         7e421f1b-a396-4102-ab60-9dc9629a7232)(label(ledger))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         4dda7df3-3a35-4623-b1c5-508732a458c9)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         558baae9-eab5-45fd-8c08-3a9f2d4d1d05)(label(streakBonus))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         dbde7f88-e29c-449c-a172-c260d8bae048)(content(Whitespace\" \
         \"))))(Tile((id \
         de3c3786-e62f-459d-a00d-a1c9d4df5c53)(label(+))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 28))(sort Exp))((shape(Concave \
         28))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         60f116d7-c70a-4bb4-a928-beceab13a0e9)(content(Whitespace\" \
         \"))))(Tile((id \
         d079e36c-32e3-47dd-97de-97c8fb253722)(label(5))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         8aa179ff-417b-45da-ae7e-4aa4f80f6ab8)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         8b583e27-1496-45f5-96a6-9ea7bffcf4aa)(content(Whitespace\" \
         \"))))(Tile((id \
         5e37c366-1a49-4905-a714-c29fced0e1ff)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         eddd4ad7-7589-4792-a63f-13e0198598d1)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         7b61a35d-bbab-4ac4-b2e2-bdd54cf47115)(content(Whitespace\"\\n\"))))(Tile((id \
         e53c6c10-5310-4894-9271-4d5b7342f715)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         221c8977-2856-4fef-b8ca-5ed8adb43979)(content(Whitespace\"\\n\"))))(Tile((id \
         2a0732ec-2dc3-453c-bf03-3546d100efe1)(label(harvests))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         d6429554-bf98-4ed3-85d7-b81d0dafbb22)(content(Whitespace\" \
         \"))))(Tile((id \
         e9268870-01a4-40dc-86d3-e57e40618ad9)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         577496c7-1066-44c9-9fb2-a80e91fccf8d)(content(Whitespace\" \
         \"))))(Tile((id \
         eff33e0f-c231-4b22-936c-781ab8438df5)(label(h))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         16a41606-a5f6-4934-862e-0cbb1ce63aa8)(content(Whitespace\" \
         \"))))(Tile((id \
         9f6f3de3-b4ae-448a-a147-6d444542a43a)(label(::))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 29))(sort Exp))((shape(Concave \
         29))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         9e66aae3-e72c-4cf6-874d-996a9de6dc3c)(content(Whitespace\" \
         \"))))(Tile((id \
         4812bc6d-7104-46f8-b63b-927620c5ac04)(label(ledger))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         4ad5a00f-9c7d-4901-81e7-141cd3bd3ccb)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         e9cf78c6-7f80-47a8-9edc-9e6fd7d42825)(label(harvests))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         775288ee-7297-43c5-82fb-199d2eb0ec0d)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         7435e395-b5b0-4d0b-b528-049b871edc98)(content(Whitespace\"\\n\"))))(Tile((id \
         47f7f5b2-408e-45fd-9a87-6e4319f7f83c)(label(totalValue))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         8a1288ca-ebb9-4877-b855-5f725e5eab50)(content(Whitespace\" \
         \"))))(Tile((id \
         7256c1cf-902a-407f-b09a-af85a63d6987)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         4fdf5b70-b001-4f57-b81c-fb299c8a6cf9)(content(Whitespace\" \
         \"))))(Tile((id \
         c1174ad4-81c8-4b1a-9d9e-c0124f605517)(label(ledger))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         fd096d39-4761-43ea-96f0-46644e606601)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         b4bcefb6-5af8-4cdb-ab10-ab77914653c8)(label(totalValue))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         4550c70a-e8e7-4c19-9642-a42ef46bf2fc)(content(Whitespace\" \
         \"))))(Tile((id \
         703d3948-3afd-44d1-9343-de5352c3ba11)(label(+))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 28))(sort Exp))((shape(Concave \
         28))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         c1952527-1dd1-4654-9cd4-686e952422de)(content(Whitespace\" \
         \"))))(Tile((id \
         3e78e035-39e3-4563-a4f0-e49c01c4e48f)(label(value))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         0f4cf91b-45cd-4fa0-ba7c-1df930ea45cc)(content(Whitespace\" \
         \"))))(Tile((id \
         56b833c3-ad40-4f4b-8ba2-dbb7ef0df8cf)(label(+))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 28))(sort Exp))((shape(Concave \
         28))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         30c26907-7587-43e5-943c-374840f2796e)(content(Whitespace\" \
         \"))))(Tile((id \
         787ec88d-880c-45c7-bfa8-a85bac1af135)(label(newStreak))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         ddb268cb-63a4-4b4e-8c13-fd6a317a7d43)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         0894958e-1a1d-441d-ad19-46a362d76d2a)(content(Whitespace\"\\n\"))))(Tile((id \
         904d82e9-5c72-4dac-bf0c-c19a78908770)(label(streakBonus))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         19fbb6f6-c6aa-4c47-bdd3-e5d04e9f3e1a)(content(Whitespace\" \
         \"))))(Tile((id \
         d89578d2-c3a3-472c-9245-3cd7e4a52817)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         77eadbc7-3ce2-4013-b726-fa0ad8321dcd)(content(Whitespace\" \
         \"))))(Tile((id \
         663b7528-6f76-475b-8376-056cc19a7594)(label(newStreak))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         ceb1b85d-b0db-4f91-a8aa-09a8f6245adb)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         9297a6f5-6cd5-49e0-8549-587436d7543c)(content(Whitespace\"\\n\"))))(Tile((id \
         f037ccac-c77c-4433-b963-bc68689f425b)(label(lastQuality))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         36204054-f84e-4440-92c8-343c1fceaada)(content(Whitespace\" \
         \"))))(Tile((id \
         caba7d4a-15ba-46a1-96e2-f4f11327f0d0)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         5c4bcf58-edf1-4ea1-89c8-215c0ab5197a)(content(Whitespace\" \
         \"))))(Tile((id \
         aaeda7c5-d881-42b9-962d-83bd2850c68f)(label(newLast))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         460fc56c-2ada-45a3-b0e4-9702b94ba3ea)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         a048b590-7bc3-4767-a7ac-1850f75f4047)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         d5576786-0a15-406d-a7e5-ce97dad3dccd)(content(Whitespace\"\\n\"))))(Secondary((id \
         e96ac512-c06a-4b13-b51c-d8e76421fd5d)(content(Whitespace\"\\n\"))))(Secondary((id \
         293cdfe4-91c6-4af4-a295-5cad998e11e2)(content(Comment\"# Claim the \
         streak bonus and reset it #\"))))(Secondary((id \
         6c8b9b4d-e83c-4986-9746-dbc620c3ca7e)(content(Whitespace\"\\n\"))))(Tile((id \
         aa207fb0-342f-45f6-ae8a-58fd3524dccc)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         d24b0785-9f10-4b09-b773-0437a0ed806a)(content(Whitespace\" \
         \"))))(Tile((id \
         e9b36ba1-2223-4a0b-bd37-bb4020620c58)(label(claimBonus))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         7c2602a7-3746-48a5-a785-60593feabf43)(content(Whitespace\" \
         \"))))(Tile((id \
         3f82ace2-8590-43e1-822e-bb07bd3a0a5b)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 24))(sort Pat))((shape(Concave \
         24))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         a7842caf-60f5-42e0-b555-18ce07157052)(content(Whitespace\" \
         \"))))(Tile((id \
         742bd3bf-7d7a-4e03-a981-efc580c16d57)(label(Model))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         e9ee5491-4235-426e-9546-73896d7e2140)(content(Whitespace\" \
         \"))))(Tile((id \
         1e9eb5bf-9308-4631-8429-94c67b32dda8)(label(->))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 13))(sort Typ))((shape(Concave \
         13))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         4e8b6c36-740f-4583-aa5f-7da93a8506f1)(content(Whitespace\" \
         \"))))(Tile((id \
         82aa543b-4902-47ff-a98a-54dd1d51e682)(label(Model))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         de5839d6-1a94-490f-8287-2bc53fadeb8d)(content(Whitespace\" \
         \")))))((Secondary((id \
         c6a75fb0-dbd0-4151-bdea-8ed90a6f5147)(content(Whitespace\"\\n\"))))(Tile((id \
         9eb2715b-1549-4106-8e22-e4a6df41aa66)(label(fun ->))(mold((out \
         Exp)(in_(Pat))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         37))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         b57521a4-a19b-4eaf-969f-ac14fbdb5a59)(content(Whitespace\" \
         \"))))(Tile((id \
         b4efa6d9-5cec-4b21-8c33-c67c715b851a)(label(ledger))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         d22dc777-7a86-4421-a431-9a736d066b0c)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         4088f368-1d79-4a25-89cc-91b11670b2b4)(content(Whitespace\"\\n\"))))(Tile((id \
         99a7edd2-0179-4296-86f3-eef7be322c78)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         4a9f8ec3-3446-4f3c-b9a0-5a77d7151fde)(content(Whitespace\"\\n\"))))(Tile((id \
         d1822a15-f61a-4fbb-9dbb-43f1177b477d)(label(harvests))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         9d831f56-0b98-4c66-b26c-8ff067431cc2)(content(Whitespace\" \
         \"))))(Tile((id \
         945314f3-5a95-4138-8914-fd95c5e13c53)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         bc710be6-5726-4585-8802-8f58a6dfeb94)(content(Whitespace\" \
         \"))))(Tile((id \
         e5d93300-35d7-43c1-b80d-c9e2b033eb5a)(label(ledger))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         5103db73-1b7c-4e8a-a606-3994561b0a06)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         2cfc7f78-fc6a-463a-b296-c3337207d8bb)(label(harvests))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         93b92ae9-63ca-4b04-84d9-8d8abfa1dc6a)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         59cbb2e0-2db9-466f-8555-89152e9f0551)(content(Whitespace\"\\n\"))))(Tile((id \
         7cee68b1-dc55-4787-aee9-0d4c8dc4a7b6)(label(totalValue))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         47e94c57-b6ee-42ab-a3d1-49859c48b1a6)(content(Whitespace\" \
         \"))))(Tile((id \
         bede53e5-992d-448a-b41a-f28c8afd33d3)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         8fcafd69-8e98-413f-b976-c8a0f240adc3)(content(Whitespace\" \
         \"))))(Tile((id \
         7e821e13-e0b8-44d8-8546-5d6569280608)(label(ledger))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         9795852e-aae1-4dc2-80d0-8b9dddf56183)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         736e8faf-9eb6-4cdc-bbb1-fded2b8282fd)(label(totalValue))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         4d493d00-a93d-4530-8f55-d9a34a1a49e3)(content(Whitespace\" \
         \"))))(Tile((id \
         072f470f-6686-4e0c-93c1-54f5f4c94e05)(label(+))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 28))(sort Exp))((shape(Concave \
         28))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         65a54003-3b41-420c-83ad-6e882cca1db0)(content(Whitespace\" \
         \"))))(Tile((id \
         bbc67c69-ba43-4752-a382-f431371349b5)(label(ledger))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         b7d4e7d9-cbd7-4ee9-8b3b-dd4d13e81dca)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         67461b38-6170-4d40-9f6f-ba2aca6947e0)(label(streakBonus))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         56cead34-ebcd-4f11-a21a-9fba74cf161a)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         f53ad17b-c73d-4075-a761-410cf8e4fe2f)(content(Whitespace\"\\n\"))))(Tile((id \
         eb1a7943-7130-4771-8fc2-e233795fc57d)(label(streakBonus))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         447067b3-41f7-4113-8251-41353b6161d3)(content(Whitespace\" \
         \"))))(Tile((id \
         283ad4e2-e512-47d8-b240-6b16a017b8b3)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         8bbd720e-a83f-4876-9eb1-9694fb12fe32)(content(Whitespace\" \
         \"))))(Tile((id \
         ac22294c-caf6-4def-a7eb-ae3c91ff431c)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         d94bd67c-e109-424d-8562-8e757206a4d9)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         b5504255-6270-4ece-b041-fb7323f55213)(content(Whitespace\"\\n\"))))(Tile((id \
         10cdf1a2-8a94-42bc-a85b-f80df969add0)(label(lastQuality))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         c6317318-3554-490a-9e98-4e875f596f74)(content(Whitespace\" \
         \"))))(Tile((id \
         322394f3-4248-4c00-9031-619503d4031e)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         08bd9291-8681-4fd9-8df7-e976a0d63256)(content(Whitespace\" \
         \"))))(Tile((id \
         d29430f5-fc6b-4540-923a-6fce481f8b88)(label(ledger))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         aab02b3e-7e47-4089-bbcf-8e09f3fbfb84)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         217ab7c8-6019-4ee3-8f14-866e4f58f424)(label(lastQuality))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         2348ceda-4d68-4f54-86ef-6cb6349eaf85)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         b91d6d28-aadb-4f0b-b715-213dab8f35c2)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         9220197d-3490-4c0f-a013-628d7352c22d)(content(Whitespace\"\\n\"))))(Secondary((id \
         e6425db2-a9df-4332-bbce-e11a7e26eabc)(content(Whitespace\"\\n\"))))(Secondary((id \
         f08b7e54-19aa-44fc-8126-3a84da333cee)(content(Comment\"# Close the \
         harvest day - reset streak tracking #\"))))(Secondary((id \
         e33324dd-94c4-486d-a863-e95492e07089)(content(Whitespace\"\\n\"))))(Tile((id \
         10781f6d-a240-431b-a00d-0d098c39fbd1)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         7221973b-8a26-4e4d-94a2-725b5ca7fda8)(content(Whitespace\" \
         \"))))(Tile((id \
         d22ed8ec-072f-4bb3-8e9b-f92cf471aee1)(label(closeDay))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         d3ebe636-51de-4b2a-abef-445ff23d56a4)(content(Whitespace\" \
         \"))))(Tile((id \
         8a439f41-698c-46bf-8d91-5d618a39521b)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 24))(sort Pat))((shape(Concave \
         24))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         38e3df20-6c8c-4aa7-a382-14ddd8f25820)(content(Whitespace\" \
         \"))))(Tile((id \
         c0461f34-59ec-4bd8-9622-fd1bdef46156)(label(Model))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         bc880d96-846f-4e7a-8868-89906cdf3dfd)(content(Whitespace\" \
         \"))))(Tile((id \
         3f9cefde-10f8-4137-85f3-fb2906e371ed)(label(->))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 13))(sort Typ))((shape(Concave \
         13))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         ca5ef7fb-f29a-4e50-ae01-110d8ecc281c)(content(Whitespace\" \
         \"))))(Tile((id \
         2720ce12-5bf0-409a-a6e6-669fc1cf4ab6)(label(Model))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         637c5934-0b7d-4970-83ae-45e0d78fa807)(content(Whitespace\" \
         \")))))((Secondary((id \
         acd314d6-0f4f-4aa7-8b7f-0d8be5e2f245)(content(Whitespace\"\\n\"))))(Tile((id \
         4e047b20-ec53-43bf-a6a9-06c02acf6dbf)(label(fun ->))(mold((out \
         Exp)(in_(Pat))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         37))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         8d6285e3-89ae-475e-8c9e-a10ae7f3e7fc)(content(Whitespace\" \
         \"))))(Tile((id \
         0e5c4eea-ad20-4d52-b966-cdf668cc1dcd)(label(ledger))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         c63cd2e6-650e-40eb-95b0-a43db991ace1)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         e9169381-606c-4344-8c49-115647b10887)(content(Whitespace\"\\n\"))))(Tile((id \
         c904bd2b-8555-4bb3-b4fb-6b7922ecc84f)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         c70539be-782e-462b-8fe8-e0b8e38ed9e5)(content(Whitespace\"\\n\"))))(Tile((id \
         5b44addd-35ee-459c-9739-23f7ff5a8e11)(label(harvests))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         bb27795f-6534-4660-ad1a-f5f272c536a8)(content(Whitespace\" \
         \"))))(Tile((id \
         c0304ca7-641d-47de-b8c9-1ef568a0f813)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         3d66b299-2b85-42b4-9863-2b1981cffb2e)(content(Whitespace\" \
         \"))))(Tile((id \
         c83c0fdd-6639-4689-9d7f-e5c0fe5a5510)(label(ledger))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         d582dec0-0533-4566-b445-d56bda049d96)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         8947ba4e-75e1-473b-ba65-8f59221bdaa0)(label(harvests))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         519c6c75-3983-4f06-9b25-4d9ecc826efa)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         75b29243-d5d9-444c-8c5e-ebc53ff0e26d)(content(Whitespace\"\\n\"))))(Tile((id \
         b009e53d-53f7-4542-955b-9f58ac78cff1)(label(totalValue))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         2913bde1-dbe2-456f-8f98-7170e5d134c0)(content(Whitespace\" \
         \"))))(Tile((id \
         652120b6-5e6c-4832-8cf9-000ee156ed41)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         6a055f30-305e-4c30-94be-fd10dc438847)(content(Whitespace\" \
         \"))))(Tile((id \
         cf61c729-2bac-4e4a-9eec-1d51ac453ce9)(label(ledger))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         4caf899d-42c2-460c-8171-95d5d9d9512a)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         f9600871-1eaa-4681-8ca6-ef902b5625a4)(label(totalValue))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         f261a942-24e0-4f25-89b5-7648c1d6c10a)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         354e9fa7-6046-482c-abc1-f05140a40c8f)(content(Whitespace\"\\n\"))))(Tile((id \
         23e33eb3-4031-4b61-beda-da043c3d41de)(label(streakBonus))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         59b7abe0-0d4e-493c-be2c-84a68f222b60)(content(Whitespace\" \
         \"))))(Tile((id \
         dc01540b-83b5-44e8-b514-1e68d62e72ad)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         9fd586e5-ac11-46d5-b541-7370cc900a6b)(content(Whitespace\" \
         \"))))(Tile((id \
         4e84e13d-5e2d-448d-ba83-3523cd4501de)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         a32c7722-6f79-440f-80b4-3e722af56800)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         82eb63ec-09f5-4713-b840-c85bda0b8f96)(content(Whitespace\"\\n\"))))(Tile((id \
         9380efff-3d74-497b-8de7-bb2ef19ad678)(label(lastQuality))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         fd7c3701-29b7-4545-b9b3-7b6f61a1842b)(content(Whitespace\" \
         \"))))(Tile((id \
         fdfd8641-b676-49ba-8c53-868e46a91020)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         4af52f26-d332-43d9-9757-19ce397f544e)(content(Whitespace\" \
         \"))))(Tile((id \
         0f99f335-9cc7-4d02-8a71-b92297639fcf)(label(Bronze))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         e0a3f5d0-8fa5-4c96-8e0e-b760c78ce6a5)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         9ae55754-4cbc-4854-8337-74859b1db439)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         e8411fc9-d2e5-42a7-9853-b6d8cf7ab1ee)(content(Whitespace\"\\n\"))))(Secondary((id \
         dbf16c46-d7b7-42d7-ac03-6e839a5a1e03)(content(Whitespace\"\\n\"))))(Secondary((id \
         0c27fd71-509e-4619-99fd-7e815d154f9a)(content(Comment\"# TODO: Add \
         premiumMultiplier helper here                      \
         #\"))))(Secondary((id \
         4bf76bd2-ca91-4c8f-8cd5-c1cc2ac39b97)(content(Whitespace\"\\n\"))))(Secondary((id \
         382d4706-5fc2-4de5-97f9-c64f9bf6e13d)(content(Comment\"# It takes a \
         streakBonus (Int) and returns the multiplier:     \
         #\"))))(Secondary((id \
         504581e8-5251-47e9-a487-ca47a9f420c7)(content(Whitespace\"\\n\"))))(Secondary((id \
         33918d49-0812-41bd-b76a-86c8d17a3ad8)(content(Comment\"#   - Return 2 \
         if the streak bonus is >= 10 (strong streak)    #\"))))(Secondary((id \
         92d727f6-6274-46df-9032-0c465ad7a807)(content(Whitespace\"\\n\"))))(Secondary((id \
         c1822df6-58d1-42ca-ad94-8436905d5808)(content(Comment\"#   - Return 1 \
         otherwise                                      #\"))))(Secondary((id \
         3a66ed32-b515-4c20-b078-85a75671acf1)(content(Whitespace\"\\n\"))))(Secondary((id \
         bbccb10f-dd1c-4834-98a2-26c4bc01698c)(content(Comment\"# Hint: This \
         is a simple if/then/else on the streakBonus.     \
         #\"))))(Secondary((id \
         66757716-1d86-413e-ad2a-0376fdad84af)(content(Whitespace\"\\n\"))))(Secondary((id \
         a4aa5fed-de1b-419c-b184-afd2efbc087f)(content(Whitespace\"\\n\"))))(Tile((id \
         92b1567a-3f4d-4989-a317-d588d2a86fc8)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         466c18ca-0ef9-40a4-ada3-1c9100c974c0)(content(Whitespace\" \
         \"))))(Tile((id \
         df101f48-ceca-40c0-83bb-c394172f2b6a)(label(premiumMultiplier))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         256a4c31-0255-432d-a95d-c39acb911a0f)(content(Whitespace\" \
         \"))))(Tile((id \
         31fcfbba-c2d0-4f8b-ab26-fa4bd92676df)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 24))(sort Pat))((shape(Concave \
         24))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         a4637142-9cda-4c83-9381-c3b67412c5b1)(content(Whitespace\" \
         \"))))(Tile((id \
         25cedd98-4c77-477a-8efd-bfad6b076e95)(label(Int))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         4a61dd3d-e424-4443-a288-d2599135add0)(content(Whitespace\" \
         \"))))(Tile((id \
         6ee1c1ee-b4a3-4b4b-aab9-669f857516f5)(label(->))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 13))(sort Typ))((shape(Concave \
         13))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         6db01adb-2ad6-4618-9a97-b869ece5daa0)(content(Whitespace\" \
         \"))))(Tile((id \
         5adc431e-21fa-4c86-8acf-8c004470f401)(label(Int))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         a84d6fb3-fc30-4e36-b51b-08cb9836d526)(content(Whitespace\" \
         \")))))((Secondary((id \
         195b7b1d-96f9-4441-8138-3df1e8e01bd4)(content(Whitespace\"\\n\"))))(Tile((id \
         2c38ae8c-dfde-483c-a401-e4e49f927f2a)(label(fun ->))(mold((out \
         Exp)(in_(Pat))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         37))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         a8eba832-09bd-42b6-aa47-8848df5c8e52)(content(Whitespace\" \
         \"))))(Tile((id \
         20ccff5b-676e-4230-b7ee-bd8eaa020bec)(label(streakBonus))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         2d73a07a-ea20-45b4-8c7e-60672afcd199)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         5c7ca71d-d075-49aa-b5e3-70f38387345c)(content(Whitespace\"\\n\"))))(Tile((id \
         a81d24a3-60e6-4fad-989a-45e6dc98345a)(label(?))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         b18f8cde-87d1-4f3a-b651-47a53bcf29cd)(content(Whitespace\"\\n\"))))(Secondary((id \
         51642c3f-7ff3-412f-9197-3e6c5adb6ef1)(content(Whitespace\"\\n\"))))(Secondary((id \
         802de9aa-560a-415a-b83b-47a7e374c7d2)(content(Whitespace\"\\n\"))))(Secondary((id \
         c612003b-9a75-4df3-94dd-52478409fce7)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         8253c472-d048-47a2-ad88-4d7a3dc6c0d8)(content(Whitespace\"\\n\"))))(Secondary((id \
         799f87d7-2585-44bf-8376-ff4098bb43df)(content(Whitespace\"\\n\"))))(Secondary((id \
         02d44187-d4c2-4665-b1d8-d5e4e58c2e9a)(content(Comment\"# Main update \
         function - dispatch actions #\"))))(Secondary((id \
         97b2c146-a561-41ff-9097-c77ab79d3ecd)(content(Whitespace\"\\n\"))))(Tile((id \
         bb207f6a-4d29-41c0-8741-9cbc762b1321)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         1545d84e-7bbb-40e9-80dd-b9002bfad809)(content(Whitespace\" \
         \"))))(Tile((id \
         162b1d3c-6c57-478c-8e76-27447798f473)(label(update))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         e283884d-fc4d-4199-98f9-a182c1d9e8f1)(content(Whitespace\" \
         \"))))(Tile((id \
         989b5a96-94f2-4479-bfc2-0a69d79a3fcf)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 24))(sort Pat))((shape(Concave \
         24))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         26c7f920-6dd7-4c70-8bbf-72c3c9163249)(content(Whitespace\" \
         \"))))(Tile((id \
         df3bea6a-bd89-40e1-8886-734ca7549ff4)(label(\"(\"\")\"))(mold((out \
         Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0 1))(children(((Tile((id \
         4686c21e-0b3e-4c45-986c-feaa48213598)(label(Model))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         86003a66-5565-4cf6-b691-59549215b9e9)(label(,))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 44))(sort Typ))((shape(Concave \
         44))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         3de42b5d-af2e-49ba-b823-d006285ba667)(content(Whitespace\" \
         \"))))(Tile((id \
         2927000b-b2e9-496a-81c6-822b36a14f9f)(label(Action))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children()))))))))(Secondary((id \
         3463a3e1-a950-4b00-bf40-1b0fd4116210)(content(Whitespace\" \
         \"))))(Tile((id \
         d5b01fa4-5951-418e-b684-861f330d97cb)(label(->))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 13))(sort Typ))((shape(Concave \
         13))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         db5dc113-f7ff-4643-a7ce-479168bd2705)(content(Whitespace\" \
         \"))))(Tile((id \
         d3b862bf-b650-4d4f-8ade-bd15c46394e7)(label(Model))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         cdf5203b-dc61-4f46-95cc-9474e31e42cb)(content(Whitespace\" \
         \")))))((Secondary((id \
         59cf5373-0f07-4884-af22-c7c4bfa96d38)(content(Whitespace\"\\n\"))))(Tile((id \
         a3e03783-d5ac-4382-a5c5-793aed0a7a4b)(label(fun ->))(mold((out \
         Exp)(in_(Pat))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         37))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         03cdb161-122e-491d-b52f-edfa4e2c1b02)(content(Whitespace\" \
         \"))))(Tile((id \
         7d4b2e16-7eec-4a7c-ad2d-cad5f3d61c21)(label(\"(\"\")\"))(mold((out \
         Pat)(in_(Pat))(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0 1))(children(((Tile((id \
         ad115d92-94c1-4bfe-81e3-624819360406)(label(ledger))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         4cbf3971-12c4-4e89-bff8-fbda186f0156)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 44))(sort Pat))((shape(Concave \
         44))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         780eb5d3-80a9-425e-a411-57cf2843086a)(content(Whitespace\" \
         \"))))(Tile((id \
         6cb24f15-1588-41ee-acc3-e69692012822)(label(action))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children()))))))))(Secondary((id \
         144a8053-1257-45a5-a66a-975918e01052)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         6d067d67-9754-4f23-ad79-f20ec9bf2be1)(content(Whitespace\"\\n\"))))(Tile((id \
         2cf88cb3-ab1b-4ac2-8ea4-8f95331321f0)(label(case end))(mold((out \
         Exp)(in_(Rul))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         31b531e2-896b-4b5b-b5a2-5d6768b77f78)(content(Whitespace\" \
         \"))))(Tile((id \
         6870a5da-0525-4be3-a433-cd52d3c6f8d3)(label(action))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         f891eefc-b6f7-46ea-996d-980af5f822de)(content(Whitespace\"\\n\"))))(Tile((id \
         0bee06df-e64b-44a8-b06d-edf78e355d98)(label(| =>))(mold((out \
         Rul)(in_(Pat))(nibs(((shape(Concave 46))(sort Exp))((shape(Concave \
         46))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         3712d64d-2931-441c-8d1f-f5484a7ddb37)(content(Whitespace\" \
         \"))))(Tile((id \
         bfd6bd2a-1475-4b1d-b3d3-579e2c4e1126)(label(RecordHarvest))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         cc5e2bc9-629d-4509-a9d8-3459cd5961b7)(label(\"(\"\")\"))(mold((out \
         Pat)(in_(Pat))(nibs(((shape(Concave 23))(sort Pat))((shape \
         Convex)(sort Pat))))))(shards(0 1))(children(((Tile((id \
         9558ec06-a30b-4b04-b031-069497860355)(label(h))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children()))))))))(Secondary((id \
         67ff72a1-dad6-4f80-9b74-b954334ce779)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         833e7935-910e-4701-96d0-59bf3f88227d)(content(Whitespace\" \
         \"))))(Tile((id \
         fccfe480-3719-4b95-a1b0-61ebbb5392d7)(label(processHarvest))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         3f2b7627-b86b-4009-b357-f692fd5b24c3)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         b6106b11-f851-4786-85ea-764b97dfa3f2)(label(ledger))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         b27dffe8-f998-4b4f-bb8c-a1450b0a8489)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         63396933-04a7-4bb6-b5df-28f4a0d5e81b)(content(Whitespace\" \
         \"))))(Tile((id \
         db1148bf-0e1a-4ddc-ab05-6ff7cf88d6f2)(label(h))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         46e026e8-268b-4212-897b-8da7b32e15a4)(content(Whitespace\"\\n\"))))(Tile((id \
         882ac8aa-f8c0-49b2-b338-f05ee26b8e87)(label(| =>))(mold((out \
         Rul)(in_(Pat))(nibs(((shape(Concave 46))(sort Exp))((shape(Concave \
         46))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         9fcd4ecc-7eea-495b-8ca1-4257343f5255)(content(Whitespace\" \
         \"))))(Tile((id \
         d3df6ad9-5372-4d76-a382-79e0adc7c0ef)(label(ClaimBonus))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         5c30c1e7-90e9-44a9-8d70-ae8343f46c36)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         2c9b1dc2-76dd-47b7-b195-52fc1f4197c6)(content(Whitespace\" \
         \"))))(Tile((id \
         847d9d01-c63b-435f-8aee-87999570e00c)(label(claimBonus))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         8d1e08ff-b43c-42aa-af4f-9d98eb3ce800)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         902c5cef-40ef-4e20-a815-2207b560a8a5)(label(ledger))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         63f34f79-ee0d-4be4-8694-dbea45d19ef6)(content(Whitespace\"\\n\"))))(Tile((id \
         5af09f3c-928c-4e49-8893-93d27507d821)(label(| =>))(mold((out \
         Rul)(in_(Pat))(nibs(((shape(Concave 46))(sort Exp))((shape(Concave \
         46))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         abb793c0-2fa3-4c46-83e8-35852cfc5c46)(content(Whitespace\" \
         \"))))(Tile((id \
         73ae1f41-0795-402e-a143-00c130ebc867)(label(CloseDay))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         74eb6f62-c543-4049-bbcd-1e290851383d)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         5731fb42-ba54-45d1-8031-28f62eccd75b)(content(Whitespace\" \
         \"))))(Tile((id \
         bec042f9-d7ea-4d3d-89a7-066c653c540e)(label(closeDay))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         7897013a-e034-4e16-8a67-9eac699d63af)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         df11a5ee-1410-45ee-aff8-188d6972525d)(label(ledger))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         916c34c9-a9d3-4cef-bb0e-17965fd39908)(content(Whitespace\"\\n\"))))(Secondary((id \
         d9abfe27-5baa-41d2-a48c-3c7cf61a6e9b)(content(Comment\"# TODO: Add \
         PremiumSale case here                          #\"))))(Secondary((id \
         d8173a62-ecec-4953-8b65-3abe98b59145)(content(Whitespace\"\\n\"))))(Secondary((id \
         560b68e0-662c-4d29-b63b-635c932c447b)(content(Comment\"# Hint: \
         Compute payout = streakBonus * premiumMultiplier,  \
         #\"))))(Secondary((id \
         616ef2a3-3165-4d42-8144-536ee7405511)(content(Whitespace\"\\n\"))))(Secondary((id \
         e23c0c2e-225e-4e50-ac82-dbd2398127be)(content(Comment\"# add payout \
         to totalValue, and reset streakBonus to 0.    #\"))))(Secondary((id \
         e1bc2a36-e758-415c-b2cd-da3d4d3112e0)(content(Whitespace\"\\n\"))))(Secondary((id \
         d08d9e99-546d-4001-8ad1-427924e7e16c)(content(Comment\"# Keep \
         harvests and lastQuality unchanged.                 \
         #\"))))(Secondary((id \
         9463313e-f5ba-4291-a39b-802beb48a093)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         88440f1e-8003-49be-a95c-894533c7cf34)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         bf5294ac-e9e0-4131-880c-3019573ca403)(content(Whitespace\"\\n\"))))(Secondary((id \
         5292fa55-62d3-42ec-b79f-5db077b4e524)(content(Whitespace\"\\n\"))))(Secondary((id \
         8ca549e7-0a80-447e-80ba-2f2cccc4c5cc)(content(Comment\"# Run multiple \
         actions in sequence #\"))))(Secondary((id \
         9067decd-59cf-4605-9ed3-a0162d422e1f)(content(Whitespace\"\\n\"))))(Tile((id \
         f116aa52-aacb-4b6f-b63e-83c653d53e0b)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         f15137b5-14c1-4d4f-ae90-74e316ae4c63)(content(Whitespace\" \
         \"))))(Tile((id \
         71f089c3-5bf6-4054-8f98-59f80a84f815)(label(run))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         390c74f3-f70c-4cb7-942b-7be9e9e39c95)(content(Whitespace\" \
         \"))))(Tile((id \
         0f0ff7f6-99cc-47b7-bd2a-b66b07241d55)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 24))(sort Pat))((shape(Concave \
         24))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         cce34178-d51a-4211-b6a6-041c7fa13e57)(content(Whitespace\" \
         \"))))(Tile((id \
         a3e265e2-a03b-4091-96ca-91560a4688f7)(label(\"(\"\")\"))(mold((out \
         Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0 1))(children(((Tile((id \
         8eb9808d-038e-4421-a575-ab69307f9f14)(label(Model))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         8433a72a-c6fd-457b-ac48-792754262f55)(label(,))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 44))(sort Typ))((shape(Concave \
         44))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         2525778b-0f85-484a-84e6-9611f568b710)(content(Whitespace\" \
         \"))))(Tile((id ef79095d-ea9b-4d16-8af0-6826fe8f958f)(label([ \
         ]))(mold((out Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape \
         Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
         5063c9b7-b868-4abb-9375-cc61f695266b)(label(Action))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))))))))))))(Secondary((id \
         5e637306-a6b7-4647-93b8-8cd53e815237)(content(Whitespace\" \
         \"))))(Tile((id \
         aad7595b-45be-45bd-8e08-f29ced0b74bb)(label(->))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 13))(sort Typ))((shape(Concave \
         13))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         77984e8f-4385-4ce3-a554-fac0949c1459)(content(Whitespace\" \
         \"))))(Tile((id \
         434bcb14-5f7e-4b48-8713-da9076c1187d)(label(Model))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         c5015048-cabd-4f81-b270-07f2a304236c)(content(Whitespace\" \
         \")))))((Secondary((id \
         c5002ccc-5cad-4afc-8913-4039a536236f)(content(Whitespace\"\\n\"))))(Tile((id \
         999daed1-af62-4372-98da-3f5dde22f882)(label(fun ->))(mold((out \
         Exp)(in_(Pat))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         37))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         e48b40ca-3ed6-46ac-9403-77e9fce54d76)(content(Whitespace\" \
         \"))))(Tile((id \
         02a1e2c7-e726-4d2f-8fff-ea6fa6f5ceca)(label(\"(\"\")\"))(mold((out \
         Pat)(in_(Pat))(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0 1))(children(((Tile((id \
         3d79cdb1-2933-402e-a6ea-586df5d3eee8)(label(ledger))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         1b543fb0-c7b9-44b5-a8c5-5b1e428eb45b)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 44))(sort Pat))((shape(Concave \
         44))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         cf2dcbb1-549b-45c2-9277-c6fed3370826)(content(Whitespace\" \
         \"))))(Tile((id \
         ddf371c7-53e2-4b39-a9de-e7f23d443f06)(label(actions))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children()))))))))(Secondary((id \
         c7cf1aa1-a645-469b-8346-5802eb751b69)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         1a70e07d-aa24-491b-9739-2039b6678a3a)(content(Whitespace\"\\n\"))))(Tile((id \
         039b9e7e-b289-4214-9b28-a265379d195c)(label(fold_left))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         ce3574eb-ddd5-469b-9816-8f151e6217b8)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         f57eaa07-e3b6-48eb-97e8-95d231b4c617)(label(actions))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         a8471a67-bd65-4523-839d-5b0fa96917ae)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         963c7671-e391-4fb0-aa71-f9e0066d9198)(content(Whitespace\" \
         \"))))(Tile((id \
         7b3a8aa4-a2e2-462d-87a2-b4c65e098290)(label(update))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         d51e026b-457c-47e1-b10a-060a8b8fd68b)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         c43bc92b-2df2-40a1-b969-7a0e6e6a3529)(content(Whitespace\" \
         \"))))(Tile((id \
         dc271996-5c42-472f-9fe6-be2a629b7d3f)(label(ledger))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         0aef5c80-a76f-4c27-9cdc-4514d4ad3b2b)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         583e42ce-0710-4de4-8bec-0f8c45f790e1)(content(Whitespace\"\\n\"))))(Secondary((id \
         faa5bc9e-69f6-40f9-93fd-173cd53a708b)(content(Whitespace\"\\n\"))))(Secondary((id \
         3071dd72-3824-4a0e-8ce2-9e35556df8dc)(content(Comment\"# ===== TESTS \
         ===== #\"))))(Secondary((id \
         3c3ed6f6-226a-4e42-8fd2-73d542331013)(content(Whitespace\"\\n\"))))(Secondary((id \
         0955d35a-0e13-48ad-b5cc-2094d258d375)(content(Whitespace\"\\n\"))))(Secondary((id \
         2be2bb05-38fa-41b3-9de2-4ad0849ab96e)(content(Comment\"# Regression: \
         basic harvest recording still works #\"))))(Secondary((id \
         de92f452-69fb-4d78-8ae9-e912a515912d)(content(Whitespace\"\\n\"))))(Tile((id \
         62a80ee4-5445-4c22-974f-a875382de43e)(label(hint test end))(mold((out \
         Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         3e36d574-4686-40c3-a00a-2bbca9eb6930)(content(Whitespace\" \
         \"))))(Tile((id \
         ca5667c2-5c0a-4a07-8e2d-b0e02249a642)(label(\"\\\"recording harvest \
         adds to total value\\\"\"))(mold((out Exp)(in_())(nibs(((shape \
         Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         96bcf414-b937-4bd3-8083-2357ccf4b67d)(content(Whitespace\"\\n\")))))((Secondary((id \
         d5277f2c-3479-4a67-88e5-9e7607f885fe)(content(Whitespace\"\\n\"))))(Tile((id \
         9019c17d-9312-4309-a486-6a8333592e89)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         bbd49f1d-0464-4c89-bfd7-c900bf67d68c)(content(Whitespace\" \
         \"))))(Tile((id \
         1cdd8e59-c384-4fbb-82ab-aaf7d27d7a8a)(label(h))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         bab46012-f6ee-4ffc-b014-0f71ae572937)(content(Whitespace\" \
         \")))))((Secondary((id \
         c55ab176-8e61-4f0d-9c4a-db14e1987860)(content(Whitespace\" \
         \"))))(Tile((id \
         f2cb791d-075c-443d-9094-ba1a036db237)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         6b5c0f09-068f-4415-8f87-e172b079121a)(label(crop))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         d7775665-41ae-4470-8487-28d876de3215)(content(Whitespace\" \
         \"))))(Tile((id \
         f303ae7d-25b5-437b-a4c8-3597efe0b1aa)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         1a18074c-df4e-4d0f-9eca-ca87381bbece)(content(Whitespace\" \
         \"))))(Tile((id \
         891c9a42-36ff-42fe-a656-06d500d5a49b)(label(Moonmelon))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         8146f803-a8e3-4101-8b77-356e1e6f3a40)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         a1a2b43c-96fd-4565-a315-3774c809b9f0)(content(Whitespace\" \
         \"))))(Tile((id \
         dfe08774-8d4e-4fb3-b477-b933413dd81d)(label(quality))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         5c37cd67-fa70-4c39-87d5-afe5d2039101)(content(Whitespace\" \
         \"))))(Tile((id \
         d0a74402-c9de-488d-9652-f20bbab1f8c7)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         e2ee26ec-174f-41b0-9f68-55ad3ed3d3e2)(content(Whitespace\" \
         \"))))(Tile((id \
         091d0b6f-1681-4f98-be54-8ea35b96e57d)(label(Bronze))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         2502712f-3ee8-45ac-bd3e-f4cfbdb5c3a2)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         f847b9bc-66c4-43c3-87f8-6f9910988429)(content(Whitespace\" \
         \"))))(Tile((id \
         a9b313ef-61e5-48f8-b78f-b7a975fe11e2)(label(quantity))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         f9b3a385-62e4-41a7-9d0e-194181d99afa)(content(Whitespace\" \
         \"))))(Tile((id \
         c873d8e5-de68-4395-b767-40e37a1156d2)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         aee9c16f-9c74-480c-be37-7ef3134005e0)(content(Whitespace\" \
         \"))))(Tile((id \
         899147ca-be6e-4eff-801d-a15414c3e6ae)(label(2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         9acd6e5f-88ae-425c-ae2a-2cd040c87bfb)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         88ce6a41-5603-459d-8901-d416954103a6)(content(Whitespace\"\\n\"))))(Tile((id \
         32af3442-1c44-40c8-ad2d-ad242334e251)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         4123ded1-b58c-43e7-ba05-f22198c1bb7f)(content(Whitespace\" \
         \"))))(Tile((id \
         4d8bb029-264f-4a13-ae29-418182d085bd)(label(ledger))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         8369be92-02b4-42a1-990a-a58b6d00031a)(content(Whitespace\" \
         \")))))((Secondary((id \
         bd6a82d3-cfed-47ad-a49e-0868fac0bb48)(content(Whitespace\" \
         \"))))(Tile((id \
         6f2bd0d9-822a-44f5-9f7d-946b31a10a83)(label(update))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         7e0adbef-1b3a-4ef4-940e-e50e0e7d4ee9)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         d4e5877f-5070-4f81-99bc-9a12d0c3b637)(label(initModel))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         1e73e0b0-6041-42c3-b6f1-5b41424b0ad2)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         d1d6e05d-aff3-4bef-9717-8c38223267ad)(content(Whitespace\" \
         \"))))(Tile((id \
         c4896a2f-f943-417d-a122-9ebd42843c4f)(label(RecordHarvest))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         be5d6602-29ec-4a7f-b1e8-95ce97fd42b9)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         375a845e-cae4-4f61-b13a-c529f888c059)(label(h))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))))))))))))(Secondary((id \
         d290ad4f-bf0c-46b2-a516-9a5d614e1074)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         fa4d0339-bc49-4eeb-b1ca-81810f2de91c)(content(Whitespace\"\\n\"))))(Secondary((id \
         c7460201-b27d-49da-8bd7-f9e522fe85df)(content(Comment\"# \
         Moonmelon(15) * Bronze(1) * 2 = 30, no streak bonus on first \
         #\"))))(Secondary((id \
         dd12ee58-e880-4e05-9385-cc85a73242fd)(content(Whitespace\"\\n\"))))(Tile((id \
         edd7130f-d4a3-4b22-9495-367ab1c7cba7)(label(ledger))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         f0625c6f-c583-402e-a8b9-7a0f3961cd2a)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         dcb5cb08-3782-4b93-a600-b947b5e2cd89)(label(totalValue))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         c6337acc-df20-4407-894b-34faaede9bf2)(content(Whitespace\" \
         \"))))(Tile((id \
         ddc65b70-0f87-4dbe-a02c-59866fd8f323)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         e851dfff-6ded-4bbf-8c23-a2c00f2f669a)(content(Whitespace\" \
         \"))))(Tile((id \
         828560aa-0160-44db-8bc1-e76e89af6b10)(label(30))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         589ca3ab-68c4-4424-af4b-84e9bdce9f43)(content(Whitespace\"\\n\")))))))))(Tile((id \
         f9c492d3-000a-4715-92b4-649d45583592)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         590dca72-f85f-49d9-b9ce-ab174da17383)(content(Whitespace\"\\n\"))))(Secondary((id \
         9250b4e6-28bc-4920-81ca-c9748f747fa2)(content(Whitespace\"\\n\"))))(Secondary((id \
         be5cce17-7981-4141-bcf3-dcc8b54de3c4)(content(Comment\"# Regression: \
         streak bonus still works #\"))))(Secondary((id \
         110726ba-9130-4069-8391-805275152d6d)(content(Whitespace\"\\n\"))))(Tile((id \
         481d4156-b7d0-40b4-9be6-d92e84004e59)(label(hint test end))(mold((out \
         Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         6d65c1e3-2313-4518-8546-f347d481479c)(content(Whitespace\" \
         \"))))(Tile((id \
         4b1eb058-2e01-4e76-9d4f-d5e3278753c7)(label(\"\\\"same quality builds \
         streak\\\"\"))(mold((out Exp)(in_())(nibs(((shape Convex)(sort \
         Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         5c8a2efc-e796-4648-a152-9904b4339a11)(content(Whitespace\"\\n\")))))((Secondary((id \
         9dcb488e-b074-44f0-85df-32c8a9d992e2)(content(Whitespace\"\\n\"))))(Tile((id \
         35049891-0e11-488c-a407-19f55d30fc40)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         e7280422-b8f8-4e91-b05b-c3f6e9a3577c)(content(Whitespace\" \
         \"))))(Tile((id \
         f83cd217-5a7f-4990-a336-e84291862053)(label(h1))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         43d6b137-2b71-4617-b813-70c49f6e7c25)(content(Whitespace\" \
         \")))))((Secondary((id \
         1357ea53-8363-41c9-8ff7-1ae7d202a166)(content(Whitespace\" \
         \"))))(Tile((id \
         bf7e759b-03b7-440b-9495-4b8e8f1d710e)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         52860532-c1b1-4977-8462-026ad75a925e)(label(crop))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         a1cb390d-4e03-49bf-bde0-2f560b5faa10)(content(Whitespace\" \
         \"))))(Tile((id \
         b2e1557a-7919-4744-887a-74aab9c81b75)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         54fd5b65-c63b-4671-8212-266b50d0f37b)(content(Whitespace\" \
         \"))))(Tile((id \
         dcd400a8-4d36-4bea-a652-c0efa78bf75b)(label(Moonmelon))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         58707a2b-7821-4a62-b2e9-cdfd0149f001)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         5e84be9a-932f-49f8-99a4-c982d18816ca)(content(Whitespace\" \
         \"))))(Tile((id \
         2db50ce7-f83c-4725-8e8c-6502c6afdeba)(label(quality))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         dbf78ce3-9a5e-4a16-b8c4-3e087ef28432)(content(Whitespace\" \
         \"))))(Tile((id \
         bc8ed875-dbdb-41e4-83cb-95db6c1ed4bb)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         1e6b436d-4f8c-42d8-916f-4fb5a933fe0f)(content(Whitespace\" \
         \"))))(Tile((id \
         9589856a-6d2d-4415-9367-f2ff8d89c298)(label(Gold))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         e9e341d7-f212-4263-92df-c2059e547044)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         f2771403-046b-4059-bc97-ed5e831f4999)(content(Whitespace\" \
         \"))))(Tile((id \
         2fe23510-54b3-4411-9e45-70e5cf0510c0)(label(quantity))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         d8bfb2e2-73e8-43fc-b4db-fc17eb122438)(content(Whitespace\" \
         \"))))(Tile((id \
         c877af71-6ca0-469c-aa37-77ff0375ee0e)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         bf4407a6-72ef-4e63-a628-2bb9e696bba7)(content(Whitespace\" \
         \"))))(Tile((id \
         a44f4e5f-b1ab-404d-8aa9-468ea60171c2)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         e501637d-e652-4798-97fb-d62048e3c4b8)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         0d2c0105-37bd-43a0-8926-7dddfbbb5cb2)(content(Whitespace\"\\n\"))))(Tile((id \
         aaddf787-65c9-48bd-8980-2f9409a35a4e)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         bef8aa87-cc4d-4bdc-abce-b7f02e92ec16)(content(Whitespace\" \
         \"))))(Tile((id \
         94889694-0548-4625-8aed-c4778b85afd3)(label(h2))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         bfbd3a23-8bef-429b-a74c-28ece31a4482)(content(Whitespace\" \
         \")))))((Secondary((id \
         4c553037-cca2-4a8e-9d44-d92592aa20af)(content(Whitespace\" \
         \"))))(Tile((id \
         be94b723-aa45-4563-a1bf-789a27c5b751)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         7427252d-3635-4fa7-8298-c83a04ef4d7d)(label(crop))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         489e2f49-a1e7-44d5-bd39-d8ef031dbf26)(content(Whitespace\" \
         \"))))(Tile((id \
         22931383-a1c1-478a-817e-d22bd472d9c9)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         f709b146-0d59-48b6-b4da-bcd5d8f3158b)(content(Whitespace\" \
         \"))))(Tile((id \
         4cc63680-82b7-4559-9896-2896bf17f8ec)(label(Starfruit))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         53163b4f-7ad2-4df5-8778-17aaa35363ce)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         fc452a2a-05c2-4eb2-af61-095477e0b99e)(content(Whitespace\" \
         \"))))(Tile((id \
         9b2d9ed2-5455-445c-805f-964e15017876)(label(quality))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         2db7a7fe-320d-42bc-a32b-74e6d2f2d50b)(content(Whitespace\" \
         \"))))(Tile((id \
         78213c15-eea1-4e0b-87c7-da82bfe4854a)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         943a9bb2-d282-4be4-8ef8-6ef77424cdab)(content(Whitespace\" \
         \"))))(Tile((id \
         e3d917e5-cfd8-431b-9dc7-4da3c32182e9)(label(Gold))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         b105b2fe-4c4b-42de-a60c-24d8402e60ad)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         d08a6c92-123f-4649-91d0-3b45bc37daa7)(content(Whitespace\" \
         \"))))(Tile((id \
         5b5171dc-0043-481a-a99a-da6828d442c5)(label(quantity))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         a0246a7f-01a2-47e6-915b-8b2c4d38730d)(content(Whitespace\" \
         \"))))(Tile((id \
         5be8f8db-ff31-4663-a745-6c8b296934f1)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         d7d488e3-0351-4269-9eba-d31e51421beb)(content(Whitespace\" \
         \"))))(Tile((id \
         ffbe48a0-e973-405d-ad81-3d8fab9a8f13)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         539eb12e-fc44-423c-9deb-b954ce827da9)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         56f6ddb3-4590-474c-ae90-c8cdf7757e65)(content(Whitespace\"\\n\"))))(Tile((id \
         46262cab-5680-4b5f-9682-49aa28f0d42c)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         950e3457-1738-4f54-bd2a-821056e326ac)(content(Whitespace\" \
         \"))))(Tile((id \
         03bc2538-2a3b-4bbd-8234-da0cd26d8b65)(label(ledger))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         a8594011-2339-48ec-a35b-9de4ce83eed7)(content(Whitespace\" \
         \")))))((Secondary((id \
         3d463000-0c8e-4045-a3e7-4476c87ed781)(content(Whitespace\" \
         \"))))(Tile((id \
         6a9a5dcb-b9ce-4223-80dd-07806e183616)(label(run))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         74367566-9a20-469e-8aa0-ab187a3a9a91)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         a65bec94-de0c-4eec-b8d4-71bbb76f6054)(label(initModel))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         7be232c1-c0b7-4d66-8680-72897e5b97f7)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         6deb40ad-b3c0-4a9b-8b84-ab4606d9635f)(content(Whitespace\" \
         \"))))(Tile((id 3df47dc0-7f85-42c2-bab3-8dd7be85e88a)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         b02bb30b-d741-4005-bd47-c20f51c1b960)(label(RecordHarvest))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         41e4fc6f-3cd9-489a-970b-3b6466960da0)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         a615be3c-e819-4a92-8811-27d1c2cf9368)(label(h1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         61ca02b9-1cc0-4fec-8f04-04596cd5af71)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         7e3fc267-7f5f-4a1d-b400-b4824b9a17f4)(content(Whitespace\" \
         \"))))(Tile((id \
         712b1fab-a6c7-4ae5-81c7-f16d57e36291)(label(RecordHarvest))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         e10fcf2e-d0af-4882-bacf-ffc30697ae7f)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         c4e62384-55a7-4dbb-b5c3-08c04d1aa6b4)(label(h2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))))))))))))(Secondary((id \
         4fbfa1d6-6177-4add-89be-24e3a464ccf8)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         c1a43a7a-61d8-44ca-8ad0-a033a7f8db10)(content(Whitespace\"\\n\"))))(Tile((id \
         dd857837-f8bb-413d-ace0-db969242ecfa)(label(ledger))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         61bf106a-f931-4401-aafb-811c62895653)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         0a88a097-8824-4c1d-93d6-055cc0b87666)(label(streakBonus))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         d3287032-191f-4010-a0fb-c28179c32323)(content(Whitespace\" \
         \"))))(Tile((id \
         9f7557c7-79e6-472d-b8f5-fbea284b8519)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         9db4ad34-841b-4321-aca4-3f64017984ae)(content(Whitespace\" \
         \"))))(Tile((id \
         16d60786-eeb5-4685-9726-4083e1b55b06)(label(5))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         de266145-e5ba-453f-8150-c159eb32612f)(content(Whitespace\"\\n\")))))))))(Tile((id \
         9763ba1a-d316-43c3-bfe6-92a433d00076)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         26cf5256-644b-4233-b74a-f28b74deabe7)(content(Whitespace\"\\n\"))))(Secondary((id \
         7115170a-2ad3-413d-9923-e76e5d6b692c)(content(Whitespace\"\\n\"))))(Secondary((id \
         eac5dda6-e1cb-441e-a8bf-87aa5466a0ea)(content(Comment\"# Regression: \
         claim bonus still works #\"))))(Secondary((id \
         d7c84a56-ba40-43e1-aa16-734740ef31b4)(content(Whitespace\"\\n\"))))(Tile((id \
         bba5ed74-9888-4563-8352-3ede3ca80228)(label(hint test end))(mold((out \
         Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         ced0b8ff-5cb8-4d12-ad8f-22747a5843d8)(content(Whitespace\" \
         \"))))(Tile((id \
         24f9a3d1-e564-4406-965c-badde70d74d9)(label(\"\\\"claiming bonus adds \
         to total and resets streak\\\"\"))(mold((out Exp)(in_())(nibs(((shape \
         Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         f6bdce2e-06ef-43bf-93a2-d04839360ae7)(content(Whitespace\"\\n\")))))((Secondary((id \
         7f0b5129-1753-49e7-b512-eb06020dd329)(content(Whitespace\"\\n\"))))(Tile((id \
         37d90334-fece-463e-864c-1ac4164cc265)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         2c4cb1ba-fc45-4731-8dbe-2eee3c4fb8cd)(content(Whitespace\" \
         \"))))(Tile((id \
         de65656f-c814-463c-830a-ecfb39df35b6)(label(h1))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         e05903f8-ce5f-4262-bbe0-ad638d7cddd9)(content(Whitespace\" \
         \")))))((Secondary((id \
         cd5aceb6-52ca-4bdd-8b90-ba8016c90cf5)(content(Whitespace\" \
         \"))))(Tile((id \
         ebe599a1-b634-4d97-afe6-ebf7242036bf)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         fd013063-0fdd-4531-8249-abacebbb6ab9)(label(crop))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         7e332add-d030-4747-8c01-af6262f643ad)(content(Whitespace\" \
         \"))))(Tile((id \
         9ef8b865-d660-4911-a6c9-e2f258841ba9)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         682ad859-0df4-42b1-90b4-9c6677b43f8d)(content(Whitespace\" \
         \"))))(Tile((id \
         97ccf760-a2b0-4d48-97d7-ecbffb639dd7)(label(Moonmelon))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         093c086b-76b6-4101-9ea3-da808b0cc5c7)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         64eabaa3-1417-4cbd-9c01-9e57487657f4)(content(Whitespace\" \
         \"))))(Tile((id \
         7466ffc5-6288-4618-90c1-c944c67d5b12)(label(quality))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         725b44f4-9745-4166-ad9a-07699ff33f58)(content(Whitespace\" \
         \"))))(Tile((id \
         b2839621-baaf-4b16-a9e5-6e852c90a1bb)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         0177b09f-98cc-4936-8fb5-d054fa0efae2)(content(Whitespace\" \
         \"))))(Tile((id \
         ba83266a-a461-4ecd-8f2f-a3304b6fc931)(label(Gold))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         f164d7e4-c34a-4488-ac48-d4a124675442)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         803c5586-b74e-4ee4-b595-7b51b4d803f2)(content(Whitespace\" \
         \"))))(Tile((id \
         be07510a-2beb-48c8-ab2c-8d193e5d0c42)(label(quantity))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         2d7fa9c5-8a36-4f03-9e18-0a37718dc259)(content(Whitespace\" \
         \"))))(Tile((id \
         c8139976-8219-46ef-ad99-c5bb7ec722dd)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         e190b769-7cc7-4fdf-a3fd-7735eab52fd1)(content(Whitespace\" \
         \"))))(Tile((id \
         d54d58a7-991a-47ef-a44c-ae672c6e4492)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         376c01bc-413e-4892-bb7c-aefbf451c09b)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         7ec9aa61-5149-4a7a-98fa-0a7249b09084)(content(Whitespace\"\\n\"))))(Tile((id \
         5cc21537-2990-45fa-a1ff-ca17e776b4bc)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         7a0c600d-ba71-40eb-a218-59fd94325c03)(content(Whitespace\" \
         \"))))(Tile((id \
         bda576d3-9fa6-4908-bb57-4cfb7e83f5e4)(label(h2))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         09d4e02c-1659-4f30-858d-e0fb6d320682)(content(Whitespace\" \
         \")))))((Secondary((id \
         8fa9484f-0506-49fb-a0ff-0844a930e264)(content(Whitespace\" \
         \"))))(Tile((id \
         135eeb43-8f51-4389-a58b-39b6b0dcd68d)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         3504f5f1-d719-468b-be7a-65f81c98e53f)(label(crop))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         94d50032-9dfd-4f80-878f-b402cbfee476)(content(Whitespace\" \
         \"))))(Tile((id \
         f5cd2ef7-495f-459c-b791-06a26faa0da3)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         145cd9ca-e411-4992-b815-617fd829aa22)(content(Whitespace\" \
         \"))))(Tile((id \
         2eb5c1bb-1cfb-4a96-be0f-a0fbb310c7b4)(label(Starfruit))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         b0b5534f-1534-4d5c-bd21-626caf36e81c)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         2dc14623-1f0d-434d-9747-f860a1d70473)(content(Whitespace\" \
         \"))))(Tile((id \
         e24dcfde-95a2-44b8-b51a-0431c1a3d1f5)(label(quality))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         bbf02ba7-bf02-4d6e-8d90-c6b394888780)(content(Whitespace\" \
         \"))))(Tile((id \
         9010f564-938b-449e-bb8c-d566654f0a13)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         7ee0054a-e50f-42fc-9d75-051a7593efa8)(content(Whitespace\" \
         \"))))(Tile((id \
         9bc66154-217f-44fc-b95f-710c7ea46bae)(label(Gold))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         80aac079-3fa2-438d-ba01-ceba1f426841)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         ffcd5bda-b9dd-4f19-976d-de19de312d86)(content(Whitespace\" \
         \"))))(Tile((id \
         660afb1a-0294-4459-b440-5e5c4d157fc4)(label(quantity))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         efa83c75-db65-4e4d-b3ed-9cf4b91cf085)(content(Whitespace\" \
         \"))))(Tile((id \
         db4287eb-6e1f-4258-8f42-56ada8fb4fb4)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         7e9905e4-ba6e-4ae1-8c24-bc7c9d82803a)(content(Whitespace\" \
         \"))))(Tile((id \
         5382dbba-96e3-473a-b874-7009d288108f)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         bb26c901-949e-4cda-b23d-4f414ebd8a9a)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         93286449-3e5a-4e0e-aee9-e557bc955670)(content(Whitespace\"\\n\"))))(Tile((id \
         cf8dbafa-6241-42f1-b4d8-8a99ca9803de)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         5b941bd2-ff58-4d1d-883f-45b8aa591ea9)(content(Whitespace\" \
         \"))))(Tile((id \
         fcc52737-6f90-4be7-ae19-ba8108957f96)(label(ledger))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         ef7f8f98-470b-4839-9f45-cac54badce24)(content(Whitespace\" \
         \")))))((Secondary((id \
         4de21352-91f4-411b-be5b-dc528c1a8c4d)(content(Whitespace\" \
         \"))))(Tile((id \
         c12071f3-c6cc-4d01-9ebc-a0d4b22835f7)(label(run))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         6a94e85f-a52a-4286-9ffa-118c25e218f5)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         3c42329f-fa70-494c-b8ad-74891c2b5aa1)(label(initModel))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         a4fbaf75-eaf9-4cc4-b729-ee5cb4039d65)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         10762c3e-5c7f-4528-85eb-25748667f2e9)(content(Whitespace\" \
         \"))))(Tile((id f04ec803-4db4-4e9c-aabe-039556857d50)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         98ab6298-87fb-4481-a6a2-331f26d38565)(content(Whitespace\"\\n\"))))(Tile((id \
         585a4db2-9ce3-4fef-b0e8-aaed43a39a56)(label(RecordHarvest))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         7a01dfec-e5c7-4407-9bc8-4714c15c4766)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         48510a1c-f43b-408c-bdca-b861ff10c218)(label(h1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         26112861-eba9-431b-a2eb-4223bd1bee19)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         d89ee634-2128-49f5-ba9c-dc0650bdb879)(content(Whitespace\"\\n\"))))(Tile((id \
         459bfa8d-9a66-41c9-893b-fe562f3a96c9)(label(RecordHarvest))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         129467b3-7321-40ad-8cde-dca2c646d885)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         ef4b9c84-b31e-4262-8d95-d0404c2aa7ac)(label(h2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         5d48647f-99bd-4a7a-98c1-5ed0475d3c4e)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         1bb0b027-4d45-4113-a461-9812678cb86b)(content(Whitespace\"\\n\"))))(Tile((id \
         c89dc06f-4315-454a-9128-826aef6d978c)(label(ClaimBonus))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         862afb80-a597-4fe3-8f41-00d9047ef2f6)(content(Whitespace\"\\n\"))))))))))))))(Secondary((id \
         ab3d5cbd-6fde-4afe-8df7-3b251d3493ea)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         f6ef0c4d-9dd5-47e3-ad77-b5360e309d08)(content(Whitespace\"\\n\"))))(Tile((id \
         d0fcc4f9-2392-4924-ae47-13525807b760)(label(ledger))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         bce74e69-2167-4014-b1b9-559a33209a95)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         ed93486f-663a-4a7e-ac4c-ede1585d4819)(label(streakBonus))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         55b040cd-2a21-4da8-8233-1684c1f13a11)(content(Whitespace\" \
         \"))))(Tile((id \
         9a039078-7184-4097-bd72-a5a1eecf550a)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         a8144648-3e83-48ed-9a5c-6aef7ce9f055)(content(Whitespace\" \
         \"))))(Tile((id \
         97f24aa7-7a8e-4bbb-9b22-4bc322c57f42)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         e4cf7321-9abd-469b-a2a7-56e77ad64ac6)(content(Whitespace\"\\n\")))))))))(Tile((id \
         85150307-1e35-4f8e-a0ad-61d32b3d9855)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         3dcc055f-3c1a-4cd0-a8a5-c30e6a9bceea)(content(Whitespace\"\\n\"))))(Secondary((id \
         dd5512be-db49-4e24-b498-9390d5c291bd)(content(Whitespace\"\\n\"))))(Secondary((id \
         5d7582c0-5140-411c-8ccb-75583e7c425c)(content(Comment\"# PremiumSale: \
         low streak gives 1x multiplier #\"))))(Secondary((id \
         2e30dff6-6903-4916-8192-bdf8bab1e0da)(content(Whitespace\"\\n\"))))(Tile((id \
         71b7560c-2552-4328-9e91-db74ddefee06)(label(hint test end))(mold((out \
         Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         b5637466-6a34-4f39-87f3-acb402e5c2ef)(content(Whitespace\" \
         \"))))(Tile((id \
         a144dd77-ca86-406e-8335-ef772839ab8d)(label(\"\\\"PremiumSale with \
         low streak uses 1x multiplier\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         ad5fff55-d378-492d-ab02-5ee7c0ad7551)(content(Whitespace\"\\n\")))))((Secondary((id \
         bb7ae738-e23c-49ce-b09f-ecae486288da)(content(Whitespace\"\\n\"))))(Secondary((id \
         0a7de90b-b53f-473a-a710-6a96953dd36a)(content(Comment\"# Two \
         same-quality harvests build streakBonus to 5 #\"))))(Secondary((id \
         eca38e38-df3b-430a-a96c-99ca77a9d43b)(content(Whitespace\"\\n\"))))(Tile((id \
         39607283-bb18-459b-9129-96ce69db3875)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         bc9d9f69-0bb4-4e5a-946f-d82c715949f8)(content(Whitespace\" \
         \"))))(Tile((id \
         ac36c128-d064-4c5e-aff7-9e8d34c13fd9)(label(h1))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         4f1d3bb0-af94-4d88-b5f1-30d6d3eba3a4)(content(Whitespace\" \
         \")))))((Secondary((id \
         7ae3fd6e-0f7f-4199-a4d1-504239c9112e)(content(Whitespace\" \
         \"))))(Tile((id \
         3509161c-a4ea-4430-a2f1-8c9d858c42b2)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         1fbed233-a794-4092-9df6-b439bdb86383)(label(crop))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         1cbb297c-3e73-4f50-8bb8-7e4d72cc6dbe)(content(Whitespace\" \
         \"))))(Tile((id \
         3927f4d2-8ddb-4115-85c0-a81671f8df8c)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         e249ff05-e0f5-4f4e-b73a-1460e525139f)(content(Whitespace\" \
         \"))))(Tile((id \
         b69f9f8e-6404-477a-9b53-3ea9b16a02a0)(label(Moonmelon))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         6e4020de-d672-49b8-baec-2b67464bbd14)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         dd802fc6-2fee-441f-a74f-6e076f73d30c)(content(Whitespace\" \
         \"))))(Tile((id \
         554f75ca-5937-48bb-8392-9fca8bf697b6)(label(quality))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         f9189d43-c68b-4514-af51-066df5aa268d)(content(Whitespace\" \
         \"))))(Tile((id \
         5c2618d2-7089-4d3c-93f8-075cd400f488)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         9d84d02b-016c-459d-a6c3-3d637c858404)(content(Whitespace\" \
         \"))))(Tile((id \
         8f4e8bcf-9ebb-45d9-a411-3fe04d4a912a)(label(Silver))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         bef6503a-972e-4e0d-91ea-38153891fd77)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         96219b62-d118-47ab-95da-4b2e6bcc8909)(content(Whitespace\" \
         \"))))(Tile((id \
         7a1155b7-ee33-4e19-bb49-6271c9e0a40d)(label(quantity))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         d05f612d-f62d-4e7e-a0b8-456aacc390aa)(content(Whitespace\" \
         \"))))(Tile((id \
         bad3e3b5-1f9c-4878-a7b9-f79724e8a3bd)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         67a67d7c-3871-4e6d-92f6-866ca6528945)(content(Whitespace\" \
         \"))))(Tile((id \
         2a99ba49-54e5-4f6f-a4cd-3fe95f53b878)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         071924de-e3aa-4386-af12-812f28223ec0)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         2a8095ab-9938-4ba5-ad49-4ccc72eb4258)(content(Whitespace\"\\n\"))))(Tile((id \
         3f37da06-a558-4f19-911d-bc80570660ae)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         ba35c45e-84a5-42b7-999e-92304d75da50)(content(Whitespace\" \
         \"))))(Tile((id \
         f8ff6cdb-ba82-4580-a5b6-32dbd05a82d4)(label(h2))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         865a9fb9-d4a1-4918-8952-763f74479de8)(content(Whitespace\" \
         \")))))((Secondary((id \
         d30d6abb-2428-4602-ac25-3209ed351a43)(content(Whitespace\" \
         \"))))(Tile((id \
         af808f0b-41e0-49c0-96f8-46690062e5f0)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         7330d6c3-2db7-444f-a9fa-525625177c1e)(label(crop))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         00fae48d-6ac5-42ba-99fe-154bd2837488)(content(Whitespace\" \
         \"))))(Tile((id \
         8eb8d65e-bd35-46f3-9c05-6055046f5b32)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         235fb4fa-2ad0-4bf4-9cd1-8274a7685854)(content(Whitespace\" \
         \"))))(Tile((id \
         8b6c15db-f08f-4058-921c-7e607909aadb)(label(Starfruit))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         b2803578-8f7f-403c-96b7-af88651ff362)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         e28977de-2f1f-4da3-8d5c-e3a4235a838b)(content(Whitespace\" \
         \"))))(Tile((id \
         f846a049-4a72-4682-a367-a6e3be13c22c)(label(quality))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         8287848d-1581-43f1-af85-ea8ae001cd3a)(content(Whitespace\" \
         \"))))(Tile((id \
         f254fef6-9a79-4b61-9829-269ef1c5be4b)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         d17d0d68-7c76-4485-a676-51a666bc4dca)(content(Whitespace\" \
         \"))))(Tile((id \
         8d80caa8-bc2f-43d2-abce-ff7c2012bef1)(label(Silver))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         c0d41ef7-9a24-4552-8acf-3efbe7f1d4c4)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         81758e5b-89b0-4e3d-b8e7-fe52a891214e)(content(Whitespace\" \
         \"))))(Tile((id \
         0e61d1a2-7f39-4f6a-8b7d-c9af9389b46b)(label(quantity))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         fe3f65bd-fc2f-4585-a746-1b0c4e407f73)(content(Whitespace\" \
         \"))))(Tile((id \
         020adce0-f571-4266-a44b-1f454920c342)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         e177c986-8587-483d-b345-14024c20901e)(content(Whitespace\" \
         \"))))(Tile((id \
         f7432492-b382-42e0-a52e-cc828f94ed12)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         3f74a49d-05fd-44c2-8cb9-a2369e4024ad)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         1114a8e5-614f-4ed9-a444-34589462a967)(content(Whitespace\"\\n\"))))(Tile((id \
         b0761fe2-a10d-489c-b41b-d4ea0f0e1d64)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         09fc855c-e2d0-425e-a84a-07cead56cfae)(content(Whitespace\" \
         \"))))(Tile((id \
         f3baa5ea-ca5b-44fe-a200-ffd851e53bff)(label(ledger))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         4c1ff69c-e692-47ad-b2fe-6781fa1fec00)(content(Whitespace\" \
         \")))))((Secondary((id \
         2d8d5906-61cb-4433-9cd6-a9e296e44995)(content(Whitespace\" \
         \"))))(Tile((id \
         ae6abd00-8623-480c-9a4e-a1f189115314)(label(run))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         ca943416-b40a-4612-b3ba-8c026868ac75)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         e897d912-2272-431e-8b86-0597fc0b45b4)(label(initModel))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         cb82948c-747c-4ca8-b480-3854c52859ca)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         ced18e7e-2764-4a65-890e-fe99c271284e)(content(Whitespace\" \
         \"))))(Tile((id ac524faa-374b-46b0-bee8-06aa8bfc6983)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         1df34bd7-1733-4e61-9cef-4599d8e81a75)(content(Whitespace\"\\n\"))))(Tile((id \
         3a213e60-4440-4252-b75e-ab1fe3e5673d)(label(RecordHarvest))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         131e9533-365f-42ed-82e5-1c8a32a0290c)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         49d647e4-5983-4c10-b5d6-073343ba13a0)(label(h1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         4ad54ac8-5f0a-4aae-9445-6584837daf9b)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         3ec9da76-0abc-4dee-a287-edefe1969cf4)(content(Whitespace\"\\n\"))))(Tile((id \
         7941ae74-926e-4a3a-986e-4a30dbaee5ff)(label(RecordHarvest))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         40b39edb-bdf1-4801-adb2-b1be11a0b94a)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         20c78af6-ae09-4ea5-a4b7-8716b0862c5d)(label(h2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         5a12ab13-33a9-470f-8a90-addc5b717044)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         7c20827c-8131-41f1-a68b-0519d90083b2)(content(Whitespace\"\\n\"))))(Tile((id \
         7685473f-2fe4-4776-b3c6-63ab0582040e)(label(PremiumSale))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         8afbd106-28cb-4f17-8d19-88289ce9d130)(content(Whitespace\"\\n\"))))))))))))))(Secondary((id \
         6162e11a-dc2b-4bb8-b7a8-3fc3c89fc6ad)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         791191c2-a4a2-48e8-8086-622c820200d7)(content(Whitespace\"\\n\"))))(Secondary((id \
         c7dd275d-6709-488c-922f-028cb60c5f63)(content(Comment\"# streakBonus \
         was 5, multiplier = 1, payout = 5 #\"))))(Secondary((id \
         d9c8d88b-8b45-4c8c-b04a-40288bd71dce)(content(Whitespace\"\\n\"))))(Secondary((id \
         6a2f4d6d-b684-498e-b895-1c8b25d43541)(content(Comment\"# h1: 15*2*1 = \
         30, h2: 20*2*1 + 5 = 45, PremiumSale: +5 #\"))))(Secondary((id \
         a0237c81-1097-4b15-a780-de5cbfef9db8)(content(Whitespace\"\\n\"))))(Tile((id \
         1c84b1df-610b-4018-a61c-ec7ec233c528)(label(ledger))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         031368da-5ed0-4299-8fcf-1703c0a41685)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         3e9e8a29-4681-4797-b8b6-192f3322536a)(label(totalValue))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         2b5f995c-a71b-4444-8626-0c0b26554d55)(content(Whitespace\" \
         \"))))(Tile((id \
         2b897cc8-e443-4768-a631-7aa18eb94870)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         c3f3b220-f93e-48d0-b35e-cb70ff70cbab)(content(Whitespace\" \
         \"))))(Tile((id \
         678e4a59-76f2-4212-af9d-fb17eeba1448)(label(30))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         ee46b0c6-6603-4883-98ee-ff02b250b92d)(content(Whitespace\" \
         \"))))(Tile((id \
         4c43bbd2-cd51-468e-924b-c15a9422df62)(label(+))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 28))(sort Exp))((shape(Concave \
         28))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         a4ee75af-7ece-474c-ac0f-edd4358d21a0)(content(Whitespace\" \
         \"))))(Tile((id \
         355c724d-56c3-4b0b-aabb-c17d1b8f28d4)(label(45))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         68c12fa9-bd79-4549-a11c-3b69d41dbb3d)(content(Whitespace\" \
         \"))))(Tile((id \
         14aa4259-90bc-4b63-bb74-9ce4e8e5e971)(label(+))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 28))(sort Exp))((shape(Concave \
         28))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         2b6b093b-f349-4891-8acd-8945f17edd51)(content(Whitespace\" \
         \"))))(Tile((id \
         f61ed4b6-e8f9-41b6-9883-3b60e969ff9a)(label(5))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         65e49f95-9956-4ffd-92c0-04907eac5bee)(content(Whitespace\"\\n\")))))))))(Tile((id \
         526c62d6-cb83-4259-a238-22c7122d1969)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         0a527478-e0dd-4381-884b-68550d8bdf7c)(content(Whitespace\"\\n\"))))(Secondary((id \
         2ecdaa0f-9570-4314-ae12-0cc3ae75c874)(content(Whitespace\"\\n\"))))(Secondary((id \
         e36afbdf-3fb9-4fd5-b2f8-65138ddffa2c)(content(Comment\"# PremiumSale: \
         high streak gives 2x multiplier #\"))))(Secondary((id \
         360721eb-3862-4459-b176-fe25c5d7e1a4)(content(Whitespace\"\\n\"))))(Tile((id \
         1a218c0f-ca01-4652-8304-2fcfc958ceeb)(label(hint test end))(mold((out \
         Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         967e306d-cbfb-499b-a8be-962e671340d0)(content(Whitespace\" \
         \"))))(Tile((id \
         37a2ffb5-3962-4f2e-a7b4-61f30a4aa97b)(label(\"\\\"PremiumSale with \
         high streak uses 2x multiplier\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         9e9f70fc-668d-454e-b23e-99671a34e8f4)(content(Whitespace\"\\n\")))))((Secondary((id \
         c3a3ec5c-b2ae-4744-88d8-528dc051af4c)(content(Whitespace\"\\n\"))))(Secondary((id \
         73063d81-d9ec-4d9a-8913-3a57a2893a73)(content(Comment\"# Three \
         same-quality harvests build streakBonus to 10 #\"))))(Secondary((id \
         e7022d2c-112b-41c6-9550-340926eac30b)(content(Whitespace\"\\n\"))))(Tile((id \
         a044895c-5799-4834-9816-b60b6a9fba7d)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         c88e8c5c-def0-4952-ac95-8d7ae6159b64)(content(Whitespace\" \
         \"))))(Tile((id \
         6dac05dd-a244-44a7-9e14-de2c4eb7db2d)(label(h1))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         0e38678b-79d9-43c4-a029-23246cacbc28)(content(Whitespace\" \
         \")))))((Secondary((id \
         a2230543-807e-4538-9d31-c7f58091261a)(content(Whitespace\" \
         \"))))(Tile((id \
         e7818377-fb1a-46d5-951a-c832c15d6d8c)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         887e1814-f285-434c-933b-ec5345714e2d)(label(crop))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         eb4c4d23-c489-4595-8880-194c1e253cf1)(content(Whitespace\" \
         \"))))(Tile((id \
         a3e10285-5314-4104-a6a2-643211e1cd1a)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         745d5fc4-0e0d-48d1-82a4-b9fa476e7143)(content(Whitespace\" \
         \"))))(Tile((id \
         e98ca3cc-c504-4aa2-a142-8524515fb64c)(label(Moonmelon))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         5c6907e2-54c7-431e-ba6a-8c8c9f51680d)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         01e4e3af-e173-491e-bbe4-9aa994d546fe)(content(Whitespace\" \
         \"))))(Tile((id \
         372da274-5fd1-4182-86d5-b6a0eb910ef8)(label(quality))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         1297074a-3431-402a-b8eb-2bd0aeaf36cf)(content(Whitespace\" \
         \"))))(Tile((id \
         279b142a-5b4d-4d0c-84b4-926347fb1fa5)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         55421156-b453-4475-91b3-62f368473889)(content(Whitespace\" \
         \"))))(Tile((id \
         b1199045-5fc2-4fd9-9ed6-5377953ce6aa)(label(Gold))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         0c78184a-0327-428e-a4af-bea4117d7cf5)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         2d95bcc2-013d-455d-b17f-c967baa5fd0d)(content(Whitespace\" \
         \"))))(Tile((id \
         93d0d7c5-8017-4ebc-8601-bb884714eccd)(label(quantity))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         cdbb9e7d-d8e5-4ad9-8b84-b3e342e2654c)(content(Whitespace\" \
         \"))))(Tile((id \
         27fbb179-ced0-405f-a95d-bc6b70c2823d)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         9d6e724c-3c01-4c4a-928f-240dbafba68c)(content(Whitespace\" \
         \"))))(Tile((id \
         3f0b9bad-6378-4cc9-89c1-aacc1f76face)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         3f0a8eb8-2a2f-44c9-a2f6-bd047034d687)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         8af35a88-170d-4820-bdd1-d766b5d8429b)(content(Whitespace\"\\n\"))))(Tile((id \
         4b85f2a9-d659-4f0f-887a-7c3cc6b44674)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         8c73a4a6-03b9-4f21-9659-dc6dc471e9a4)(content(Whitespace\" \
         \"))))(Tile((id \
         90501dff-765a-461d-9894-6a7258e72cc1)(label(h2))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         65f2c9a3-cc13-4150-a0c8-2cf872c07887)(content(Whitespace\" \
         \")))))((Secondary((id \
         fef8bbe3-8997-401d-90fe-6b0182d4854a)(content(Whitespace\" \
         \"))))(Tile((id \
         10491856-3d58-4de6-9a96-cae0852ab016)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         eb76fb6d-8d1f-4ca1-998d-123617afe03f)(label(crop))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         c02c0b5c-7dfd-45db-87aa-70ce3310babb)(content(Whitespace\" \
         \"))))(Tile((id \
         f2dc7fc8-a5ab-4d61-b657-0987da23099d)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         7b6341ab-4bd3-45d7-b648-460fd4a040bf)(content(Whitespace\" \
         \"))))(Tile((id \
         caa6c5bd-aebd-4aaa-b75b-5fb512e02a39)(label(Starfruit))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         d871a2b9-e6f6-440d-b9c2-2d65842441a0)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         54d0a941-4cdf-455f-8f7b-c48aa9cdce01)(content(Whitespace\" \
         \"))))(Tile((id \
         62f43314-d9ee-46ad-a38f-7d51a7bf892f)(label(quality))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         80980ff9-3fa0-4817-a6ba-2d4595e0adec)(content(Whitespace\" \
         \"))))(Tile((id \
         00447413-6c4d-4b47-8588-c2ca6ae777b5)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         2e33bb8f-f2e1-4298-b0cc-33849288433f)(content(Whitespace\" \
         \"))))(Tile((id \
         dce2c08f-0afb-4ee7-9270-01969f3bcfe0)(label(Gold))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         f5e9ff7c-2a55-454a-bad9-2d6a3a0a4ce1)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         443938ce-2a76-4fa5-9481-c46229fa1f71)(content(Whitespace\" \
         \"))))(Tile((id \
         6cdd9732-1fe4-4ad3-8e9e-e048a7f73e7e)(label(quantity))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         3a47d82f-a84f-42cd-96b6-babb598fb593)(content(Whitespace\" \
         \"))))(Tile((id \
         a8edecc5-9f00-451f-9c08-c96690cda815)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         7272cabb-34c0-43aa-837c-6aeee57a5ee4)(content(Whitespace\" \
         \"))))(Tile((id \
         65e53af3-7bc0-4de9-bd9e-62f668c38798)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         8c2efdfa-d1d7-4e17-a83c-83cf86ff6a3f)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         9c6747de-47fc-44b7-afde-49f9eab749c4)(content(Whitespace\"\\n\"))))(Tile((id \
         2c15898a-edfd-443b-8350-752c0e83dc55)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         e47803e4-1df3-49e6-be0a-a75e056ff467)(content(Whitespace\" \
         \"))))(Tile((id \
         bf6b0748-372e-47c1-9361-65aed63fe9a2)(label(h3))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         64b62a8b-a807-46da-aebd-00507664e652)(content(Whitespace\" \
         \")))))((Secondary((id \
         6255f467-c8be-464b-81c0-35b5671e29a5)(content(Whitespace\" \
         \"))))(Tile((id \
         3baf374d-59e4-4ff7-9bae-735bca4a5f02)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         3c437526-cebd-4f5a-8562-47fcb240491c)(label(crop))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         4f4c9905-4241-485f-8703-42432f9ff721)(content(Whitespace\" \
         \"))))(Tile((id \
         29d8deb8-fada-4bc5-995d-68fecc091b32)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         5570ac47-1afb-4ae4-a6ad-46ecdcc1f08d)(content(Whitespace\" \
         \"))))(Tile((id \
         1bf7efb6-65de-47f2-8a25-2a6af4d281fb)(label(Nightberry))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         85c9b106-e34b-4e9f-b58a-5ecb53b196e8)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         3089df29-6597-4c1e-afba-485fc613dabd)(content(Whitespace\" \
         \"))))(Tile((id \
         e4337008-28b3-48cf-bc50-66d8124d859e)(label(quality))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         c8adf921-1c1c-4251-a763-f3bc72469d63)(content(Whitespace\" \
         \"))))(Tile((id \
         f59c35dd-6c93-4627-b707-5532a2de0caa)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         9b3f4f1b-872f-4c93-989c-087bd26ce7b0)(content(Whitespace\" \
         \"))))(Tile((id \
         650f1fee-6a45-480b-bf69-98a9e3cc40e9)(label(Gold))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         bca57068-f961-4cc5-9174-df99b0e13cff)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         8a62c853-3a76-4ab2-aefd-8692e4a8ef53)(content(Whitespace\" \
         \"))))(Tile((id \
         b61d3866-3d04-4645-8c3b-da261ddcfbf0)(label(quantity))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         bb73264c-30b6-47bc-b05c-673c19ff7d59)(content(Whitespace\" \
         \"))))(Tile((id \
         fc8430fe-c400-4fe3-b522-025da9cc0368)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         b5aa5731-13ec-41ed-b64a-7332f93b1f24)(content(Whitespace\" \
         \"))))(Tile((id \
         237a0c77-783c-47a9-95b6-bada76e7379e)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         47a52b9b-d15d-4d48-a810-cf758af0c410)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         febb49d0-417d-4a13-bf59-38b327aa689a)(content(Whitespace\"\\n\"))))(Tile((id \
         9017e543-65a9-42b3-bd21-a50e6b9512aa)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         a4321e16-a71b-403d-883a-8ba6d01bb42f)(content(Whitespace\" \
         \"))))(Tile((id \
         daca930e-4e88-438d-8157-be5e41280d2d)(label(ledger))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         4d4cc4d4-767b-4f06-838f-7eb44231613a)(content(Whitespace\" \
         \")))))((Secondary((id \
         e09e0d6b-ba7c-4294-98e8-d525a06a8bb8)(content(Whitespace\" \
         \"))))(Tile((id \
         3dcbe622-974d-4b1c-9fce-07402694dc77)(label(run))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         f836965d-28ef-431f-ae30-c193110b1501)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         ffbdb56a-bc8d-41ee-b350-05bc54b6ba2e)(label(initModel))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         450d1f63-e820-4223-acad-67762b038039)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         7ef149a7-82fe-4f3f-8537-f3f99f0adf84)(content(Whitespace\" \
         \"))))(Tile((id 728c6576-1798-4318-b434-ef2ebe88d2b0)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         0d25b9f1-0084-4f01-af0c-e74b1989316b)(content(Whitespace\"\\n\"))))(Tile((id \
         08d71bf0-5f45-405c-8fdc-300e0e2eb925)(label(RecordHarvest))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         8d0be2e0-8728-426f-8588-1d028317ddf8)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         3a46f1bb-7b1b-41f0-86a6-d64315784e5b)(label(h1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         0b2a69be-9812-4b48-8211-6389316bc070)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         935fbf30-4447-4776-8293-9cc1702ed5a2)(content(Whitespace\"\\n\"))))(Tile((id \
         9a8e6e1d-aa4c-4a94-9d3d-36df9c13e427)(label(RecordHarvest))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         6babe6c3-c759-4c12-8e6d-c47721c8f67c)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         e3db0b9b-39c3-4fc3-be98-eb45aee7fa06)(label(h2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         a87f4f5d-56f8-42ee-ba88-6c1095353305)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         786ce778-7660-4feb-9553-03d5de4cee78)(content(Whitespace\"\\n\"))))(Tile((id \
         fcc13da1-8681-4d72-b2e5-3337db9b1a00)(label(RecordHarvest))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         e41e7e5a-1dce-4604-9f8d-6c2ee34c0304)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         854d4e2d-8d16-4c03-aeef-9212c56cdc24)(label(h3))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         65658681-e432-4cad-b1ff-1ed1ca77b6e9)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         18e793bb-7d45-46ce-9527-c69550763344)(content(Whitespace\"\\n\"))))(Tile((id \
         2e455e62-69ef-4544-a90d-c82f84f15e94)(label(PremiumSale))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         be1e0670-2ff8-44b1-89d1-01069498cfb1)(content(Whitespace\"\\n\"))))))))))))))(Secondary((id \
         df0cfada-4ae5-4375-929f-b881fe63f087)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         3cd72e3a-83ba-46ec-abf7-5656956d6e2c)(content(Whitespace\"\\n\"))))(Secondary((id \
         222e880c-bce6-4364-b548-af7a368477df)(content(Comment\"# streakBonus \
         was 10, multiplier = 2, payout = 20 #\"))))(Secondary((id \
         87b893d3-0f6f-42c8-9001-7dcc63673d55)(content(Whitespace\"\\n\"))))(Secondary((id \
         9226d046-ccc7-400c-aada-55f9f0dbfc71)(content(Comment\"# h1: 15*3=45, \
         h2: 20*3+5=65, h3: 25*3+10=85, PremiumSale: +20 #\"))))(Secondary((id \
         88869bb8-c86b-44a1-94d2-8c369f62b8e6)(content(Whitespace\"\\n\"))))(Tile((id \
         c6d3affc-8aa7-44ad-99db-6220d7ab7de5)(label(ledger))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         432a9fea-3a6b-4a04-90ca-af23a1cc78d3)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         c13bcd99-1c3d-4343-9858-52674135b04c)(label(totalValue))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         33239f53-5721-4325-ba2c-83f86f164933)(content(Whitespace\" \
         \"))))(Tile((id \
         2b888350-d986-4ba9-a398-e4c1ebb43bd1)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         1f2e5b95-9b02-41de-8f3d-6ded61297667)(content(Whitespace\" \
         \"))))(Tile((id \
         d7ceadd2-b055-4dc5-b7ac-5b6254ecdc74)(label(45))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         e6d51bb3-6dd2-4cdf-90a1-9725e8294695)(content(Whitespace\" \
         \"))))(Tile((id \
         11545a8f-f1f9-4ea4-814a-6e2d3d2a9501)(label(+))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 28))(sort Exp))((shape(Concave \
         28))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         74f5b14c-7d8a-41c7-9898-422112e847f3)(content(Whitespace\" \
         \"))))(Tile((id \
         6ed6c40f-0093-4eb3-8997-6220da7f67b2)(label(65))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         ae06b756-3a5d-4b86-b597-5d40436b4a02)(content(Whitespace\" \
         \"))))(Tile((id \
         83ca627a-4425-4ea2-9f3b-00e4daea65f2)(label(+))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 28))(sort Exp))((shape(Concave \
         28))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         b980fe90-5414-4489-86b3-4a3b69e553c2)(content(Whitespace\" \
         \"))))(Tile((id \
         a73e99c7-1326-47f2-af83-e17a45b7c898)(label(85))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         6ffbc72c-03d8-4f6f-995e-a460350270ff)(content(Whitespace\" \
         \"))))(Tile((id \
         311ae5cc-642b-4611-b8ac-d98981a61cb7)(label(+))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 28))(sort Exp))((shape(Concave \
         28))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         c6ae6f9b-3274-4868-97c2-9a888a95e624)(content(Whitespace\" \
         \"))))(Tile((id \
         16352409-1f99-4132-8fea-1cf1ebd07e05)(label(20))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         07ac9622-637f-4847-8756-d03b5f226dc6)(content(Whitespace\"\\n\")))))))))(Tile((id \
         502cc382-3b6b-42ca-a9e0-a6b08ba545b5)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         68e3adf5-b5eb-4dbe-b133-2e28aa139f2d)(content(Whitespace\"\\n\"))))(Secondary((id \
         0d833a7d-e20e-4925-97ce-ff8e7b715973)(content(Whitespace\"\\n\"))))(Secondary((id \
         1ea9f90e-9b00-4310-bac9-8518ff6fa356)(content(Comment\"# PremiumSale \
         resets streak after claiming #\"))))(Secondary((id \
         ab9c1392-60db-4e19-8d57-55e8f1ccaa4b)(content(Whitespace\"\\n\"))))(Tile((id \
         947d62af-963d-4a87-aa56-3d349ab929d3)(label(hint test end))(mold((out \
         Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         ad72ed6f-3d81-4c47-a95a-53d8172cb617)(content(Whitespace\" \
         \"))))(Tile((id \
         de4913bc-17af-406f-a077-294ec2f31a8c)(label(\"\\\"PremiumSale resets \
         streak to zero\\\"\"))(mold((out Exp)(in_())(nibs(((shape \
         Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         d85f5cc4-cdd9-46e3-b477-dc6e2acf2491)(content(Whitespace\"\\n\")))))((Secondary((id \
         3efea212-0379-47b1-9dda-a5f623faa869)(content(Whitespace\"\\n\"))))(Tile((id \
         1094cf82-950e-494b-8c77-bde2f8d46715)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         7378ea3a-0d61-4db0-9ab8-ef1a695791c9)(content(Whitespace\" \
         \"))))(Tile((id \
         e6962739-d26e-4a79-8f46-7e4dcb99c41e)(label(h1))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         8c850ff6-ed7e-4567-8d2e-ba6678915011)(content(Whitespace\" \
         \")))))((Secondary((id \
         99036a1d-7d6a-4ead-a365-2509c41a2baa)(content(Whitespace\" \
         \"))))(Tile((id \
         8edbcf7a-ec43-4e8d-9045-a446a8150874)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         4c742804-5ca0-4b49-bd49-ee67fcf69990)(label(crop))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         8f2803f7-b683-442b-8d25-aa72f18d9f97)(content(Whitespace\" \
         \"))))(Tile((id \
         f1b709ba-6c01-40d9-a49f-3405576c8ea8)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         25174e89-1602-41c7-a9ab-8908ddcee3f7)(content(Whitespace\" \
         \"))))(Tile((id \
         02eee7f1-e010-49ac-b912-c5e7c5d0ac09)(label(Moonmelon))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         4beb370b-60f5-452e-8411-ce097d023fab)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         3915dfa7-58a6-46d9-b3b0-3699482c449c)(content(Whitespace\" \
         \"))))(Tile((id \
         03ad816a-5d1d-4534-8293-811f5d981390)(label(quality))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         3b38b5e0-e5e7-4a93-83d9-06b33d95eda5)(content(Whitespace\" \
         \"))))(Tile((id \
         08f1e598-3f08-43d6-a137-0dd4cd2bc008)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         9916465e-06fa-4daa-802d-f588e55d238d)(content(Whitespace\" \
         \"))))(Tile((id \
         f2ebbcf6-77c1-4860-904c-9519e6cb7d18)(label(Gold))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         39edf920-186a-49c9-a897-0549ddd0b2e3)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         57771668-3e34-41f7-9c2d-8b3edbdf70b8)(content(Whitespace\" \
         \"))))(Tile((id \
         5a3e78d7-d7fa-4542-a324-16a2fed3a58d)(label(quantity))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         be7b0ecd-6836-493b-b46e-42e87ac0c841)(content(Whitespace\" \
         \"))))(Tile((id \
         f9e4535d-8524-451b-bf6e-86850cc54d88)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         d5d1332b-8982-457d-87f0-2b1882772494)(content(Whitespace\" \
         \"))))(Tile((id \
         6774785e-f0df-48db-a015-ff4eab5e9ade)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         008db3d0-9e6a-41b9-b282-3ab7fb33ef3e)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         94f7482c-1679-4157-9999-284cb63a6ae8)(content(Whitespace\"\\n\"))))(Tile((id \
         8d39221a-9190-4b18-9887-9a6b8dee62ac)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         52fe5633-1a43-444f-bfa9-7baba555260e)(content(Whitespace\" \
         \"))))(Tile((id \
         60a76073-80ee-4d91-a603-f43e0e66a323)(label(h2))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         ac70b36f-1f59-4f65-9b86-2f2c3949928e)(content(Whitespace\" \
         \")))))((Secondary((id \
         1a3094ee-2783-4667-907c-0be6a29365cb)(content(Whitespace\" \
         \"))))(Tile((id \
         ed8e8d8f-fe67-49ec-8c75-43ef25f425f9)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         30bd0984-c4c4-4cbb-9c11-c4b53940b8a6)(label(crop))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         921140cb-7e40-47a2-875a-4b15de6fd6b2)(content(Whitespace\" \
         \"))))(Tile((id \
         34149f2c-a0ca-4a02-912b-4916ac97d41f)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         22fba13e-a0d9-4aa2-82dd-01adce6ae06d)(content(Whitespace\" \
         \"))))(Tile((id \
         bc1ba56c-08f4-4cb0-8a5f-57080098f500)(label(Starfruit))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         3c704215-4773-430c-a1b0-4f95374a523d)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         7d81d61e-7693-4a46-99e6-ef8b9b4d96ed)(content(Whitespace\" \
         \"))))(Tile((id \
         3ba34ee1-9c0f-4e05-82e0-36339d924b6e)(label(quality))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         e50c7929-7aed-4f4f-956e-53c698aaabb6)(content(Whitespace\" \
         \"))))(Tile((id \
         02e0e77d-e1fd-428a-8116-eaa981d62b81)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         30d487c1-e623-4a3d-ab05-b59fefa1439b)(content(Whitespace\" \
         \"))))(Tile((id \
         a6634d81-f718-4949-a4e8-a3c8ec166c0e)(label(Gold))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         8ae40e78-9f99-4fea-8876-3a593a98fe5e)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         360f209d-c061-4415-80f7-02e033916346)(content(Whitespace\" \
         \"))))(Tile((id \
         9f454dbc-e1f4-4d10-bb76-5347bc086c58)(label(quantity))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         939df1f4-bef2-4479-a1a0-5f1c974a66f5)(content(Whitespace\" \
         \"))))(Tile((id \
         9c917398-1a42-43c0-aba3-a6e1fdcf5e4a)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         f2165fe0-1850-4211-aada-e8177d5c096c)(content(Whitespace\" \
         \"))))(Tile((id \
         5dc28479-914b-4b68-a357-db64b29ba0d4)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         1c2370ea-ed58-43f7-8f32-3b65209c39d9)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         4de57361-6f63-4133-a7c1-cc8eb202f586)(content(Whitespace\"\\n\"))))(Tile((id \
         71e0271b-1efd-404a-a488-5750c694dad9)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         84a7f864-38dc-40a8-8151-b40c6c7e82e5)(content(Whitespace\" \
         \"))))(Tile((id \
         5039437f-a310-4f0c-a496-901642c842f8)(label(h3))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         23fe01ba-2871-47e3-a3fd-f3c7b44ce719)(content(Whitespace\" \
         \")))))((Secondary((id \
         987014dd-a3c6-4e96-9479-cf7f2d6d9498)(content(Whitespace\" \
         \"))))(Tile((id \
         63fad58f-efaa-4242-8962-fcd3e36e0ea3)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         9c98fd30-79a4-4715-a310-08e42b5303bd)(label(crop))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         c8fd6512-cabf-4a62-8a4d-8aec1d39e899)(content(Whitespace\" \
         \"))))(Tile((id \
         27eaa8dd-e262-42b7-8950-eeae81f45790)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         4a48bafe-27ad-44a9-b653-cd1f67198c77)(content(Whitespace\" \
         \"))))(Tile((id \
         308e4b8b-6231-4928-aa6a-d33e1b4caf3a)(label(Nightberry))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         33a62502-9a3d-4c7e-b6aa-23bebbf3d470)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         416c5d91-a7f4-4f12-b4b5-1dbc3736251a)(content(Whitespace\" \
         \"))))(Tile((id \
         9e3b45ec-f60d-4122-b900-6c7966d070a5)(label(quality))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         d64934ea-9c53-4bb2-92f8-08df4d704144)(content(Whitespace\" \
         \"))))(Tile((id \
         559ee071-dea9-4243-9484-9e6288769242)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         d81f02a6-242a-42ec-a913-30329b65a5d1)(content(Whitespace\" \
         \"))))(Tile((id \
         76d76ea8-16dd-4237-ad5d-b4a76c059109)(label(Gold))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         aebcff52-9a75-490c-a7e2-efe1d434d012)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         f38ab5e2-40a6-4ddc-9482-1c2db556669b)(content(Whitespace\" \
         \"))))(Tile((id \
         4aecf30f-775b-4100-a7a6-0dae3e3c8ff5)(label(quantity))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         7d647160-4b2d-4d40-9c48-65b1940bdf62)(content(Whitespace\" \
         \"))))(Tile((id \
         c4295671-9a04-4105-b569-3d660f97b3ad)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         24121fad-f759-4cb8-9ca0-0a3973308001)(content(Whitespace\" \
         \"))))(Tile((id \
         63526238-e6b2-4805-b747-22ea77795527)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         1e0338f8-8a4a-453f-9ed4-63c1e76723ab)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         832e020e-f0e3-4611-8038-3e9911bd5021)(content(Whitespace\"\\n\"))))(Tile((id \
         9f2852db-6b85-403c-89e7-50f0085336eb)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         5dede302-cbff-4424-ac83-9b6a6c2144f9)(content(Whitespace\" \
         \"))))(Tile((id \
         a40c0ba8-0b4e-4758-b541-4653b614cea5)(label(ledger))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         c25a8f41-a1aa-4473-95bd-1286e541d187)(content(Whitespace\" \
         \")))))((Secondary((id \
         f6d2b20d-fe23-4f08-8181-67166e79b993)(content(Whitespace\" \
         \"))))(Tile((id \
         f0eb8851-3c3c-417e-8bbb-063a8c4b2754)(label(run))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         d4aeefe7-e99f-4ed8-a1b6-658e9a74fb22)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         72e5ebe6-ba12-4292-9d4e-0bf7623273f3)(label(initModel))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         c8998dd7-c040-4be6-a5e3-0564afcdeb34)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         470d5193-c435-4ae0-a793-0560f732f00b)(content(Whitespace\" \
         \"))))(Tile((id 797a2f2f-5276-4741-a1cb-0b08da852e7b)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         e2aa7373-a374-4df8-9920-1cbc543ae640)(content(Whitespace\"\\n\"))))(Tile((id \
         8a8257b1-b464-4fe6-901e-73683d8f899a)(label(RecordHarvest))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         9effd885-5489-430d-8810-5b379b3a96e6)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         c0231f05-6631-4e62-a85a-aca9e4dc4588)(label(h1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         7920fc8e-915c-499c-8282-6abf05f34640)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         340901c0-6914-4a4c-9c75-f994c2cd2409)(content(Whitespace\"\\n\"))))(Tile((id \
         8ca47670-8c47-4950-bab5-0dc495e10b4b)(label(RecordHarvest))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         4840ba79-df35-4ee8-bab7-7ee2a4b0c514)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         0f41129c-62b8-4954-9676-c64feda98c84)(label(h2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         7014bf29-dc38-4109-9263-f52116a32b11)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         36a808ba-64dc-4196-86eb-d32b54df8a18)(content(Whitespace\"\\n\"))))(Tile((id \
         99f6a4ab-b7ae-4010-a454-b8942d1b937f)(label(RecordHarvest))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         3dc3b314-696e-46b9-9f6f-f10d2faf71bc)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         1dfa4e05-4106-419e-a3db-781d92105dbf)(label(h3))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         e729a94d-025f-44e8-b645-bc2a74c06a96)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         8fd87a1d-90e9-4e50-b920-2497d2331822)(content(Whitespace\"\\n\"))))(Tile((id \
         08cad4ef-f1d8-4a7d-ae0d-312e53e63ccf)(label(PremiumSale))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         bbf65717-33be-42ad-99e4-bd33942826fd)(content(Whitespace\"\\n\"))))))))))))))(Secondary((id \
         f2651d86-9372-4aa0-8f0d-50959137579d)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         da9b1627-041f-49b3-8024-ab3bed14a14d)(content(Whitespace\"\\n\"))))(Tile((id \
         01a48cbd-6b69-4b81-b1e4-cdf9df87a1aa)(label(ledger))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         16cfd11f-714d-4763-9c45-9425c5b0e670)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         07c214a7-02ce-43cc-890f-8d40a7238844)(label(streakBonus))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         28092e10-e5d3-43aa-8723-c7fe7ee38b68)(content(Whitespace\" \
         \"))))(Tile((id \
         23bc5c52-4165-4434-a3ab-69dc04b41ec0)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         93695ef9-d605-4193-9617-fe82d9cf65cb)(content(Whitespace\" \
         \"))))(Tile((id \
         b93904ee-adc1-4b54-ab19-48df2287d643)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         8c52df88-2d03-49d9-bbea-30804c57effa)(content(Whitespace\"\\n\")))))))))(Tile((id \
         a87e1885-ada6-49ec-a549-760471aa3907)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         c4ca9c9a-8543-4952-8bc6-1a9e918b55e0)(content(Whitespace\"\\n\"))))(Secondary((id \
         f1f50bbb-210e-4d83-b4be-81296781f274)(content(Whitespace\"\\n\"))))(Secondary((id \
         8db92e54-8189-431e-ad96-47f7562e935c)(content(Comment\"# PremiumSale \
         with no streak gives zero payout #\"))))(Secondary((id \
         9b2f4ab3-5601-46c7-831b-af771f2f3c0b)(content(Whitespace\"\\n\"))))(Tile((id \
         40d0d086-5555-4f67-9584-8c32528c13cd)(label(hint test end))(mold((out \
         Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         86efa204-dc68-49ff-948c-7d87fe4fcf01)(content(Whitespace\" \
         \"))))(Tile((id \
         67c93f60-a492-4051-8cc7-369ec2169088)(label(\"\\\"PremiumSale with \
         zero streak adds nothing\\\"\"))(mold((out Exp)(in_())(nibs(((shape \
         Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         6488d480-e935-41fe-8e69-dade32a1cf5f)(content(Whitespace\"\\n\")))))((Secondary((id \
         20128845-f141-49cc-b207-de926b6bf210)(content(Whitespace\"\\n\"))))(Tile((id \
         6c6ae7c7-4680-4f2d-bf1e-24fea4278eab)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         2a3f2ba5-f5c6-43d4-ae1e-6539b8cd7d3e)(content(Whitespace\" \
         \"))))(Tile((id \
         f12e5eb0-7f5e-4f32-84c6-3acd70c7d6b1)(label(h))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         40a32454-e4d0-47e0-b425-f78789e21cbe)(content(Whitespace\" \
         \")))))((Secondary((id \
         16126f50-45cb-4b6c-93fe-dcff7b398fda)(content(Whitespace\" \
         \"))))(Tile((id \
         fbff0e54-b149-4a85-947d-0a7bd8b8503a)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         c7f750f4-559e-41bf-9061-d38c50acbfee)(label(crop))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         1a4a018c-78f5-488a-b63d-3a2102ac50e9)(content(Whitespace\" \
         \"))))(Tile((id \
         a0c46f1f-6aa1-42a4-be60-a7540eccb7e6)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         5b4987d5-271b-430e-84ba-8cfb15313b1a)(content(Whitespace\" \
         \"))))(Tile((id \
         de2382b5-95ca-4dff-adf0-fe2ef5bca3f5)(label(Moonmelon))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         b7816c79-dc0a-42b8-95be-abf47e1094ce)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         c9dcf514-c70d-475b-9d3c-d68a92ad5f48)(content(Whitespace\" \
         \"))))(Tile((id \
         a6b077b0-6a2c-4176-ab98-3512e285bcd8)(label(quality))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         cc799652-e002-479d-8939-9c9e383a9875)(content(Whitespace\" \
         \"))))(Tile((id \
         1516f855-d0d5-469a-bf17-385bdcd38c2e)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         9f0d4864-2425-4944-9af9-75d7d12aa3ef)(content(Whitespace\" \
         \"))))(Tile((id \
         0c9a92d2-a110-4e6d-b2ed-41fdca651682)(label(Bronze))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         4cc67b8d-0d2c-4489-af24-8787b98acf76)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         464098a2-b5d3-4950-bae9-54419a886056)(content(Whitespace\" \
         \"))))(Tile((id \
         de3845e6-3a1f-4f4c-9267-938ca32febeb)(label(quantity))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         50053276-d6ec-4844-8ce5-c02c5fa56778)(content(Whitespace\" \
         \"))))(Tile((id \
         5a7ea0fc-f615-4a64-9e33-a9a2c77ec0df)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         2df6f861-f9c2-4171-a9f7-ec6b3bdbbcfd)(content(Whitespace\" \
         \"))))(Tile((id \
         61584e9d-e1a8-49d1-be45-f0cb1354d924)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         f5aabd47-0231-41f2-8b4d-ffe4d862ade3)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         2950b52b-59c4-4c0e-9341-b833e92f4c8b)(content(Whitespace\"\\n\"))))(Tile((id \
         b7ccabd1-ffa2-4a22-be63-8f8a868df0bc)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         77875ec2-710d-46bf-a04c-7e839f32b9e0)(content(Whitespace\" \
         \"))))(Tile((id \
         a8aa9804-bd1c-4653-ad30-12a6a4bbba39)(label(ledger))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         e9b7a4c4-44f3-41a8-8199-acfc3cf8ee59)(content(Whitespace\" \
         \")))))((Secondary((id \
         c46f4848-2426-4bef-8301-bae20cba86c1)(content(Whitespace\" \
         \"))))(Tile((id \
         d1ce7053-ea8d-4805-bc3b-b60428af9599)(label(run))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         5c0a1ab5-a838-42b4-a9fd-173303221457)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         5108851e-60ec-40a4-9165-d54a935ddb35)(label(initModel))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         1730a684-c78c-4614-adbb-096e3cd84262)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         b54949e0-ffa1-4806-b47e-0a8a08e64c33)(content(Whitespace\" \
         \"))))(Tile((id 75a9ecb6-c2b4-4f97-a828-b27fdddc5cd7)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         901a7cd0-5be6-4008-92bf-20b2def84375)(content(Whitespace\"\\n\"))))(Tile((id \
         9d1814f4-fd4a-4e8f-b836-fe6d76970262)(label(RecordHarvest))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         91d4b74d-d989-440d-9f53-caf371fe27f5)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         cc85c3f1-dbdc-468a-ab9b-34f8ebac2026)(label(h))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         5ed5d6ae-09a7-4560-b1a9-fdd3964976f1)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         05e74db3-31ee-480a-afe3-ab31a6290f39)(content(Whitespace\"\\n\"))))(Tile((id \
         88b17571-0645-418f-a51d-4910bac7de4e)(label(PremiumSale))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         bad9a125-6932-4e68-8e47-7dc55999c39d)(content(Whitespace\"\\n\"))))))))))))))(Secondary((id \
         fb5839a6-f94f-4333-a86d-1d1d14b092ec)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         91f180f1-7fd7-4146-b21e-6824f7940566)(content(Whitespace\"\\n\"))))(Secondary((id \
         5ca7605c-c483-49af-a08f-971d625fb946)(content(Comment\"# streakBonus \
         was 0, payout = 0 * 1 = 0 #\"))))(Secondary((id \
         19b7e929-5fa6-4d25-bd6d-33409e6c5142)(content(Whitespace\"\\n\"))))(Tile((id \
         0d70a15e-14d1-4869-a0cd-a2e31430b44b)(label(ledger))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         16cfa983-2158-4b1c-a270-1dfe461e297a)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         c01f13e8-0e8f-41bb-9d43-9c1f53cfce89)(label(totalValue))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         013f419f-3082-4d6b-8e1b-28b57df0a96c)(content(Whitespace\" \
         \"))))(Tile((id \
         4db38456-edf6-4b9f-ad57-0822ce4bb0ef)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         de8351e8-708e-4750-8213-2c418285978a)(content(Whitespace\" \
         \"))))(Tile((id \
         c6af9d45-f456-46b5-a82c-b90141a3b98c)(label(15))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         c0d637bc-1bf1-4d78-93dc-5c46e682be36)(content(Whitespace\"\\n\")))))))))(Tile((id \
         e1e8084b-de8e-4a89-a1ff-53526af203b5)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         b3745281-ada0-4dea-a864-6c697b1ee499)(content(Whitespace\"\\n\"))))(Secondary((id \
         67f38b0d-781f-48b3-927c-e7cc6291c895)(content(Whitespace\"\\n\"))))(Secondary((id \
         2ea0d206-6371-40aa-8865-56974cfb9cd8)(content(Comment\"# Demo: \
         Premium sale harvest day #\"))))(Secondary((id \
         d8105b24-c435-4c46-bcbf-3a92076cc212)(content(Whitespace\"\\n\"))))(Tile((id \
         39672cab-4388-4714-b4a3-180726cc5b15)(label(run))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         1a1ae1bb-e274-4e6e-bb33-841b6fe3a6d3)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         242dacd7-6dc5-41df-ab28-91722c7b2e53)(label(initModel))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         cbe39f3c-c08e-4dd5-b20c-5bf201c48235)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         3149cce8-b8d3-42dc-9b85-35bc9d3ca58d)(content(Whitespace\" \
         \"))))(Tile((id 1120c588-78f0-463e-aa92-28494e1e6a40)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         6a950672-8755-401c-b338-b34f3a7e984c)(content(Whitespace\"\\n\"))))(Tile((id \
         25ff4811-da8e-4d4a-9931-eade488f8886)(label(RecordHarvest))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         7c0137b1-2299-45ed-ab84-29a3e1e5cb99)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         8ce27731-f7ee-4697-9908-27eed373cdfd)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         590ea558-84c6-4f9f-89b9-d937c8463c7d)(label(crop))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         b59f525d-e3eb-4932-b1e2-ae7f773cfeb8)(content(Whitespace\" \
         \"))))(Tile((id \
         b04fc247-db60-498c-9137-315349688f44)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         e53452b4-07e3-469b-a149-4bf3946a0193)(content(Whitespace\" \
         \"))))(Tile((id \
         b93a51c0-7131-494e-a6dc-5c7ed5198156)(label(Nightberry))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         b0f1c93e-ed67-4b9b-b830-c016aa5f1c60)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         815e4be1-8c22-42c6-8ec6-b4fbe2322a6a)(content(Whitespace\" \
         \"))))(Tile((id \
         a0500105-5258-4cd6-8bb3-0db2b8b8743d)(label(quality))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         42af059b-7d4b-4163-8d0b-7b5f157b9899)(content(Whitespace\" \
         \"))))(Tile((id \
         7215e810-2965-4498-b63e-3767041a9bd1)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         8bf5cb06-e1ed-4670-ab29-e13bfdada449)(content(Whitespace\" \
         \"))))(Tile((id \
         0d079967-f101-4a5e-bef7-66904988da66)(label(Gold))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         e53f2505-224a-484b-b2d1-5b3fa010aba3)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         8b601fd2-3a0d-4fd9-b2b8-ed3244de33dc)(content(Whitespace\" \
         \"))))(Tile((id \
         bb82c8e3-a0c6-449f-8043-5abbf5f5a7c9)(label(quantity))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         8fb0162a-8b95-4af0-98e0-071341dd308e)(content(Whitespace\" \
         \"))))(Tile((id \
         6be3b12a-b53b-4d47-b5b1-57441c37d27b)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         595abca0-5ccd-42ad-925d-be7105632bca)(content(Whitespace\" \
         \"))))(Tile((id \
         5b100a03-eb87-4ded-bf8b-912244b3bfb4)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))))))))))))(Tile((id \
         9eae60e7-74f8-46ab-b218-435f23395227)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         2c3d1f10-d87d-42ca-b338-3af38d39e005)(content(Whitespace\"\\n\"))))(Tile((id \
         e0a43212-842c-4764-a844-7a0140873707)(label(RecordHarvest))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         14a7a1e3-5692-404d-b905-fd519e4487e0)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         ad6210bf-af38-4c7c-86d6-62ee9e2faaf1)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         895d5174-34f3-4c36-9613-d8ea46f40c8c)(label(crop))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         790abb93-1da6-4fba-b25d-ad7789e6e01d)(content(Whitespace\" \
         \"))))(Tile((id \
         921154b4-c2bb-4286-9307-002a117e66d7)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         bc825027-09b9-411e-aa14-e8c3fe7f89ac)(content(Whitespace\" \
         \"))))(Tile((id \
         757323dd-7f66-46a5-8322-9879de91ebf8)(label(Starfruit))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         962d0f33-a368-455e-87ab-647cae1aa0ab)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         a43a33b1-1c97-4dc4-b1fd-f9b01f3b33ca)(content(Whitespace\" \
         \"))))(Tile((id \
         5bbbd4a3-d262-43df-86f4-2ba321f90470)(label(quality))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         1834279d-75f7-4cfc-9c1a-ea631d71e29d)(content(Whitespace\" \
         \"))))(Tile((id \
         5d32caee-f78d-4daf-8c24-d50ace8fc97c)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         4585c5a9-65d3-400f-bcdc-de7bc9f8d3b5)(content(Whitespace\" \
         \"))))(Tile((id \
         2ae124f2-1b93-4745-a152-2ef4cc068a7a)(label(Gold))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         14061efc-adf2-4877-98b7-3d4c6446287d)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         fdd3831f-51d7-437f-99b0-4d8870b165cf)(content(Whitespace\" \
         \"))))(Tile((id \
         4a5da3c3-124e-47e3-baae-ccdd13652b2f)(label(quantity))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         6d5f1945-1636-454f-a45b-e23519955cb7)(content(Whitespace\" \
         \"))))(Tile((id \
         e01a720e-9f14-43c8-88e5-12a32b8c7a7d)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         8acbbfeb-2f05-4eed-a4fa-3b3924184159)(content(Whitespace\" \
         \"))))(Tile((id \
         499f9943-03cc-44ac-912d-63dcbe329bfb)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))))))))))))(Tile((id \
         0bbba94e-513b-4f7a-8237-95e3f13f0565)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         f119d9b8-ba95-426e-a8ab-54bba6dcb8ab)(content(Whitespace\"\\n\"))))(Tile((id \
         00258919-9541-48d3-a6db-50f779a20306)(label(RecordHarvest))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         851b4492-ee20-4994-a6d9-0f8fa2458317)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         f2e0ee64-48b9-4980-aba9-be0d6828f487)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         4618574f-988d-40a2-8dc8-847ba087d71f)(label(crop))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         e9b78eb6-0c0f-4a6b-a0ec-9692db3222e5)(content(Whitespace\" \
         \"))))(Tile((id \
         a1145e14-1ae3-46ee-83f7-a8b9c85bf74f)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         928a7384-1b21-4b41-851c-599a72bede92)(content(Whitespace\" \
         \"))))(Tile((id \
         d3b6ffaf-ab82-4b35-93e0-ef36786b3aee)(label(Moonmelon))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         07fb2197-b54a-4145-bd4c-41224458a405)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         b6d4342f-842c-4831-9bbd-e75e6c03f1b4)(content(Whitespace\" \
         \"))))(Tile((id \
         51ee0fea-a9f5-4d84-96bb-8ec1cd023600)(label(quality))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         a6324840-8dc5-4e8e-9fa5-8777589d3a81)(content(Whitespace\" \
         \"))))(Tile((id \
         d604868f-263a-4cab-95da-ee340bc1fe96)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         537b5046-cca0-423f-9225-3ec21312dcd2)(content(Whitespace\" \
         \"))))(Tile((id \
         1ac0e857-9fc5-4f04-964f-004428056dd7)(label(Gold))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         fcb90471-95d4-48fc-96ec-908b3ec6c2f9)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         1946a36a-e3d4-47c1-af6f-194379c9a5e8)(content(Whitespace\" \
         \"))))(Tile((id \
         5069dda1-41e9-4ed6-b81d-8fe2ec5e68ca)(label(quantity))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         32582616-4b3e-4edb-88cd-230bf9631fd1)(content(Whitespace\" \
         \"))))(Tile((id \
         79cf99dd-96e6-43e0-b5e5-e4d1e9facf8a)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         ed8491cf-0141-45c5-b236-72ffdc03a956)(content(Whitespace\" \
         \"))))(Tile((id \
         96275d89-abe4-4318-9434-9d167ca94ab0)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))))))))))))(Tile((id \
         79a6a156-69ce-41a2-96ef-35a7dda78d11)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         cd07c805-7d44-4de5-b634-bca5c1db3cae)(content(Whitespace\"\\n\"))))(Tile((id \
         bdf4d5b1-34e2-43fe-a472-84e891585211)(label(PremiumSale))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         5551ecf4-c13c-471c-b4b1-c78ff871e272)(content(Whitespace\"\\n\"))))))))))))))(Secondary((id \
         e6ff8d17-38ba-4714-81db-ecd8175b6e25)(content(Whitespace\"\\n\")))))";
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
         + Nightberry  # Deep purple, magical properties #\n\
         + Duskwheat   # Golden stalks, hearty grain #\n\
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
         | Nightberry => 25\n\
         | Duskwheat => 10\n\
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
         let h3 = (crop = Nightberry, quality = Gold, quantity = 1) in\n\
         let ledger = run(initModel, [\n\
         RecordHarvest(h1),\n\
         RecordHarvest(h2),\n\
         RecordHarvest(h3),\n\
         PremiumSale\n\
         ]) in\n\
         # streakBonus was 10, multiplier = 2, payout = 20 #\n\
         # h1: 15*3=45, h2: 20*3+5=65, h3: 25*3+10=85, PremiumSale: +20 #\n\
         ledger.totalValue == 45 + 65 + 85 + 20\n\
         end;\n\n\
         # PremiumSale resets streak after claiming #\n\
         hint \"PremiumSale resets streak to zero\"\n\
         test\n\
         let h1 = (crop = Moonmelon, quality = Gold, quantity = 1) in\n\
         let h2 = (crop = Starfruit, quality = Gold, quantity = 1) in\n\
         let h3 = (crop = Nightberry, quality = Gold, quantity = 1) in\n\
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
         RecordHarvest((crop = Nightberry, quality = Gold, quantity = 1)),\n\
         RecordHarvest((crop = Starfruit, quality = Gold, quantity = 1)),\n\
         RecordHarvest((crop = Moonmelon, quality = Gold, quantity = 1)),\n\
         PremiumSale\n\
         ])\n";
      refractors = "()";
    } )
