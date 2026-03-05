let out : string * Haz3lcore.PersistentSegment.t =
  ( "Study / writing / harvest-streak-extend / harvest-streak-extend-sketch",
    {
      segment =
        "((Secondary((id \
         1ba5f79e-b580-4681-a864-a19e94274f83)(content(Comment\"# HARVEST \
         STREAK EXTENSION TASK                   #\"))))(Secondary((id \
         0d482bf1-794b-42fe-bc74-db1100180a79)(content(Whitespace\"\\n\"))))(Secondary((id \
         81721471-7e86-42bd-b65f-a67db1bff71c)(content(Comment\"#                                                  \
         #\"))))(Secondary((id \
         9fb29ed4-49fe-470b-9d25-e2f130cbcdd1)(content(Whitespace\"\\n\"))))(Secondary((id \
         9e194650-9ea3-49a3-9473-9c354934c55d)(content(Comment\"# The harvest \
         ledger app tracks harvests and       #\"))))(Secondary((id \
         6ed77446-be77-44a7-a7e2-432881bda466)(content(Whitespace\"\\n\"))))(Secondary((id \
         0be1b561-6d15-4fb4-930b-47e294b779df)(content(Comment\"# builds \
         streak bonuses for consecutive same-      #\"))))(Secondary((id \
         dc570d56-a8b9-4772-a0da-f82f90fa0b97)(content(Whitespace\"\\n\"))))(Secondary((id \
         75b2c398-967a-4652-a9a1-caa905b91150)(content(Comment\"# quality \
         harvests.                                #\"))))(Secondary((id \
         8a2c52f6-2750-4470-afee-cdcfc39901c9)(content(Whitespace\"\\n\"))))(Secondary((id \
         5c0e1933-bc72-44e0-99c1-5335a3b5c47a)(content(Comment\"#                                                  \
         #\"))))(Secondary((id \
         7bad6ad0-07d8-40d9-8e94-f6b44fd97f91)(content(Whitespace\"\\n\"))))(Secondary((id \
         cf0d7135-833c-4405-9d9c-eb7ee8d5debe)(content(Comment\"# YOUR TASK: \
         Add a PremiumSale action that lets    #\"))))(Secondary((id \
         4e400023-5d19-4471-a3da-ea036f4227b5)(content(Whitespace\"\\n\"))))(Secondary((id \
         1170076f-124c-4acc-91f3-b390525e3d38)(content(Comment\"# the farmer \
         claim their streak bonus with a       #\"))))(Secondary((id \
         18b6ddaa-4412-4bc0-8996-1b0a90153ef4)(content(Whitespace\"\\n\"))))(Secondary((id \
         cdf1adf6-68c9-49e8-863f-dee59df07377)(content(Comment\"# premium \
         multiplier when the streak is strong.    #\"))))(Secondary((id \
         f51a142e-7071-4af0-89f2-52fecfa704be)(content(Whitespace\"\\n\"))))(Secondary((id \
         7bedab0c-8c3a-4054-ad6a-145a8119b6c7)(content(Comment\"#                                                  \
         #\"))))(Secondary((id \
         e745c7be-46d5-4cb9-840b-44fbf0819275)(content(Whitespace\"\\n\"))))(Secondary((id \
         17c03de8-f41a-4b37-84ad-8ca2b0cd74dd)(content(Comment\"# You need \
         to:                                     #\"))))(Secondary((id \
         c677ec4a-e15d-411c-afb2-9afcea357596)(content(Whitespace\"\\n\"))))(Secondary((id \
         e7ccb883-d651-4f8b-88be-e5e35d32221b)(content(Comment\"#   1. Add \
         PremiumSale to the Action type          #\"))))(Secondary((id \
         6d3beb1c-4e28-4f9a-b6ea-cc77e06719d4)(content(Whitespace\"\\n\"))))(Secondary((id \
         0588e8a2-f110-4f6e-9a1f-25126b493156)(content(Comment\"#   2. Write a \
         premiumMultiplier helper function   #\"))))(Secondary((id \
         84452764-b0ae-4636-943c-b6830870f74c)(content(Whitespace\"\\n\"))))(Secondary((id \
         5db336bf-8966-495a-993e-299ffe31ce71)(content(Comment\"#   3. Handle \
         PremiumSale in the update function   #\"))))(Secondary((id \
         df07cec6-3f17-47a1-adf2-db0a1942706e)(content(Whitespace\"\\n\"))))(Secondary((id \
         b6944f65-a6d9-42db-8ba5-01bc38169bcc)(content(Comment\"#                                                  \
         #\"))))(Secondary((id \
         53e877c4-aa8b-4344-af87-ae7aa6c900ee)(content(Whitespace\"\\n\"))))(Secondary((id \
         5c48e49b-4ac9-4a1c-93d9-33a1e50f01b2)(content(Comment\"# Look at how \
         ClaimBonus is implemented for        #\"))))(Secondary((id \
         811a6e12-22cd-4b66-88dc-6684d3ee9834)(content(Whitespace\"\\n\"))))(Secondary((id \
         aa7e0e9d-eb24-477a-8cd4-74968c3ca8f4)(content(Comment\"# guidance - \
         PremiumSale is similar but applies    #\"))))(Secondary((id \
         a054dabc-228a-4079-87e5-47650b903d85)(content(Whitespace\"\\n\"))))(Secondary((id \
         614c75f1-1a5c-400c-8b8d-907a92d4639b)(content(Comment\"# a multiplier \
         to the payout.                      #\"))))(Secondary((id \
         6ee6ef15-d34e-4bde-91f0-cf38ae4805af)(content(Whitespace\"\\n\"))))(Secondary((id \
         f0e0f1b3-97d4-4186-ab3b-01145ed28743)(content(Comment\"#                                                  \
         #\"))))(Secondary((id \
         0e0aac92-1ebc-4d3b-929e-4a7d5dd4849f)(content(Whitespace\"\\n\"))))(Secondary((id \
         947a6ebc-6533-4ba9-9509-ee01e702df69)(content(Comment\"# Tip: Use \
         auto-probe on premiumMultiplier to see  #\"))))(Secondary((id \
         ab9fc65d-59a6-4ed1-8c5b-645ae5ecaa80)(content(Whitespace\"\\n\"))))(Secondary((id \
         c21cbded-c106-40bc-b264-fa7e4b6d805e)(content(Comment\"# when the \
         threshold fires.                        #\"))))(Secondary((id \
         1bf5c532-3c77-45e1-8325-80c3395701b4)(content(Whitespace\"\\n\"))))(Secondary((id \
         82316a97-d1b8-49f5-96a8-438e6156777f)(content(Whitespace\"\\n\"))))(Secondary((id \
         35c39213-239a-4149-9453-9cffddb45489)(content(Comment\"# Quality \
         tiers from the moonlit fields #\"))))(Secondary((id \
         9d7efc5a-a709-4934-a74d-480d1d32f526)(content(Whitespace\"\\n\"))))(Tile((id \
         f5a37026-fc96-46ff-a006-05157920a274)(label(type = in))(mold((out \
         Exp)(in_(TPat Typ))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         3c6ed622-c652-48f5-ab5b-6f6ccf9db85b)(content(Whitespace\" \
         \"))))(Tile((id \
         f0b8e667-40f9-4adc-bdc6-c9e51bedfe7c)(label(Quality))(mold((out \
         TPat)(in_())(nibs(((shape Convex)(sort TPat))((shape Convex)(sort \
         TPat))))))(shards(0))(children())))(Secondary((id \
         22dc241e-916c-40b8-b04e-e72ed38b3893)(content(Whitespace\" \
         \")))))((Secondary((id \
         dadf532b-812e-4bd4-a18b-acdb7ddd3d0c)(content(Whitespace\"\\n\"))))(Tile((id \
         b63654ed-a654-42d2-a5ea-2c8e63a351df)(label(+))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape(Concave 33))(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         5c9c9431-be7c-4fd4-a02a-b8d765c93809)(content(Whitespace\" \
         \"))))(Tile((id \
         7e73f9c7-fce1-4d15-8ed1-661a9509f85f)(label(Bronze))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         1b91fcc3-2573-4432-8100-774751ff7917)(content(Whitespace\" \
         \"))))(Secondary((id \
         9884016f-bab5-49b9-b970-76ba41832f01)(content(Whitespace\" \
         \"))))(Secondary((id \
         4c8191d2-2f5e-4f21-bb81-09edbeb6f57d)(content(Whitespace\" \
         \"))))(Secondary((id \
         1862faca-ecec-420a-80e2-b3861bfa9359)(content(Whitespace\" \
         \"))))(Secondary((id \
         ab5b459d-6a11-4581-966c-d6036b7e4c20)(content(Whitespace\" \
         \"))))(Secondary((id \
         cc3e3056-9fc7-416a-8943-94e6509c98b6)(content(Whitespace\" \
         \"))))(Secondary((id \
         a3f52c0d-c838-4558-ab36-ef2280696f0a)(content(Comment\"# Common \
         harvest, basic value #\"))))(Secondary((id \
         8ee6ca0e-ced2-4dc1-b233-5eb12564d6f6)(content(Whitespace\"\\n\"))))(Tile((id \
         a07b88ea-88d1-4043-8b48-d961a84f7381)(label(+))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 12))(sort Typ))((shape(Concave \
         12))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         0bf87a81-0690-4eed-a408-20c6430ea8de)(content(Whitespace\" \
         \"))))(Tile((id \
         96cbbca0-8d14-47bd-a763-2d86c9678b51)(label(Silver))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         e5059193-59d1-4e13-885f-aeaf8a008678)(content(Whitespace\" \
         \"))))(Secondary((id \
         f9869403-2d66-4bbd-88ae-819ceb4f5393)(content(Whitespace\" \
         \"))))(Secondary((id \
         a95abf0c-d2d5-49ca-ad8c-5caa4f2c45e0)(content(Whitespace\" \
         \"))))(Secondary((id \
         ed3ba2be-adb3-446f-b155-cbf13ab608c8)(content(Whitespace\" \
         \"))))(Secondary((id \
         55e34ab7-2133-44af-a63f-48bef3b7144c)(content(Whitespace\" \
         \"))))(Secondary((id \
         fc007c7b-f951-4f58-a454-233ada8d014b)(content(Whitespace\" \
         \"))))(Secondary((id \
         b57517fa-d55c-4a9a-bca9-20cb4cde96de)(content(Comment\"# Good \
         quality, moderate bonus #\"))))(Secondary((id \
         ca16e05d-7dad-45ea-9d5a-734e973b08cc)(content(Whitespace\"\\n\"))))(Tile((id \
         6ebeb762-ba36-4289-9aea-fc9060b2b563)(label(+))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 12))(sort Typ))((shape(Concave \
         12))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         8034696c-d948-42c9-8240-6b53c07c02a7)(content(Whitespace\" \
         \"))))(Tile((id \
         7516ff26-b140-4aea-a9ed-de0ff8f76fc5)(label(Gold))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         ba5320b8-3b66-4810-8085-d35c5b1c3028)(content(Whitespace\" \
         \"))))(Secondary((id \
         3851b71c-bcfb-4884-93d8-fd1ebcf8e4e6)(content(Whitespace\" \
         \"))))(Secondary((id \
         f3650414-b0ca-4975-af01-0566fddb8d37)(content(Whitespace\" \
         \"))))(Secondary((id \
         96bb8100-00a5-47c5-a640-f92280b8d45b)(content(Whitespace\" \
         \"))))(Secondary((id \
         9120d364-6634-410d-8249-d9a1156a995c)(content(Whitespace\" \
         \"))))(Secondary((id \
         ffcec455-5239-494d-8342-29b31a69187c)(content(Whitespace\" \
         \"))))(Secondary((id \
         69f2dbbc-b76d-4c56-ace6-ae5ffac4311b)(content(Whitespace\" \
         \"))))(Secondary((id \
         281095f3-1574-4d8b-8424-82800ced5ce3)(content(Whitespace\" \
         \"))))(Secondary((id \
         bb74c680-516f-42d1-a58c-bb3cdf00b9df)(content(Comment\"# Excellent \
         harvest, high value #\"))))(Secondary((id \
         651f67da-518f-4f25-a517-4005e30c5494)(content(Whitespace\"\\n\"))))(Tile((id \
         2d54736a-517c-41b7-b8ba-c7cf26923177)(label(+))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 12))(sort Typ))((shape(Concave \
         12))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         33d2e95a-1815-4b92-a4b1-ccea30557cf9)(content(Whitespace\" \
         \"))))(Tile((id \
         733fc1a8-57ab-4ddf-8dd0-95a977f876e5)(label(Starlight))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         8677dc46-2895-46d3-a900-e389a354da2a)(content(Whitespace\" \
         \"))))(Secondary((id \
         ed537963-b17a-4ba9-9e53-b55193910cdf)(content(Whitespace\" \
         \"))))(Secondary((id \
         b6b06061-b800-48af-b951-92b10d929ad6)(content(Whitespace\" \
         \"))))(Secondary((id \
         fc0bf523-09ec-4d4c-aa3f-60b6a5182f25)(content(Comment\"# Legendary, \
         blessed by the moon #\"))))(Secondary((id \
         07e939ed-a068-44d6-9ce2-8e006652d9ae)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         77e28e3b-f3b0-4303-84b0-e9c0e13404eb)(content(Whitespace\"\\n\"))))(Secondary((id \
         6e3c19ed-aca6-489d-9134-41ea0244ca6d)(content(Whitespace\"\\n\"))))(Secondary((id \
         dfdf547e-aaaa-4a82-ad58-32449d5b88af)(content(Comment\"# Crops that \
         grow under the night sky #\"))))(Secondary((id \
         5902ac39-787b-4bbf-a6ab-06d02f24c528)(content(Whitespace\"\\n\"))))(Tile((id \
         f3f2f867-f1c5-4555-b16f-ae9cd637faf3)(label(type = in))(mold((out \
         Exp)(in_(TPat Typ))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         45fa44f1-4ff3-48bc-8930-3053293a45db)(content(Whitespace\" \
         \"))))(Tile((id \
         745d38e0-a074-42dc-9152-e62ac057bdbc)(label(Crop))(mold((out \
         TPat)(in_())(nibs(((shape Convex)(sort TPat))((shape Convex)(sort \
         TPat))))))(shards(0))(children())))(Secondary((id \
         cb29e60b-0ebd-4751-b506-f348cda81fd5)(content(Whitespace\" \
         \")))))((Secondary((id \
         2545e1c6-c61b-4dba-a168-c3acf8614070)(content(Whitespace\"\\n\"))))(Tile((id \
         15ecd43c-616f-4706-a673-73c40b2af0a1)(label(+))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape(Concave 33))(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         f87872e8-5e8e-4d4c-bacb-98ddd83550a6)(content(Whitespace\" \
         \"))))(Tile((id \
         4a176a65-9da9-4a8e-a17f-2409aff0ed1c)(label(Moonmelon))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         310e81a4-c4cd-45c6-96e7-58eef1b22b96)(content(Whitespace\" \
         \"))))(Secondary((id \
         c9b73125-5375-4980-b7c0-ace313e2d7a1)(content(Whitespace\" \
         \"))))(Secondary((id \
         f921fff5-b90e-4fcb-b039-8d9941f7cf25)(content(Whitespace\" \
         \"))))(Secondary((id \
         884b37f5-d852-40f3-800c-d67446f48a8c)(content(Comment\"# Glows \
         faintly, sweet taste #\"))))(Secondary((id \
         cbad8e10-da18-4fa2-ab8c-699f02710129)(content(Whitespace\"\\n\"))))(Tile((id \
         77d48357-79a0-460b-ad05-03042bb529f3)(label(+))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 12))(sort Typ))((shape(Concave \
         12))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         91d1164a-175e-499a-ac62-8c92f4976a90)(content(Whitespace\" \
         \"))))(Tile((id \
         289d0830-5ec3-4e32-9883-14a741865e90)(label(Starfruit))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         f31d88d5-a586-4482-9818-45d736610d06)(content(Whitespace\" \
         \"))))(Secondary((id \
         b2635da7-fbec-4318-a3b8-2b26d1d15232)(content(Whitespace\" \
         \"))))(Secondary((id \
         a1a126e3-5b6d-4b1a-af25-74010464e683)(content(Whitespace\" \
         \"))))(Secondary((id \
         75f69936-1443-4240-880b-773fc8c14cad)(content(Comment\"# Shaped like \
         stars, tangy #\"))))(Secondary((id \
         e7d3134a-c3d9-48a7-b89b-3af8ea936a37)(content(Whitespace\"\\n\"))))(Tile((id \
         ddb5581d-af22-456a-b1b9-ff8296484aae)(label(+))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 12))(sort Typ))((shape(Concave \
         12))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         7d9361c7-3cb5-488d-a570-2bf88454590f)(content(Whitespace\" \
         \"))))(Tile((id \
         93c3fd32-7cd4-463a-b1b2-02992b94cc01)(label(Nightshade))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         e60db7dd-6fd6-4569-b7a5-00d4f6afb9f1)(content(Whitespace\" \
         \"))))(Secondary((id \
         5878493c-b1ba-4fc6-a44f-d7f8eacaea6e)(content(Whitespace\" \
         \"))))(Secondary((id \
         1264ddf9-d2a3-4260-a7b3-3ceba27381a7)(content(Comment\"# Purple \
         bloom, magical properties #\"))))(Secondary((id \
         5edf2489-623c-4e7d-b8aa-22dff93fe8f4)(content(Whitespace\"\\n\"))))(Tile((id \
         8c93bf00-4b16-4893-a237-997ca0efb3d6)(label(+))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 12))(sort Typ))((shape(Concave \
         12))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         59746e99-1668-429f-a593-9751827bbc01)(content(Whitespace\" \
         \"))))(Tile((id \
         f69967e6-0d94-482b-8f89-33e79e699b2d)(label(Duskwheat))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         11484307-a7e0-46b0-a263-2c0805c186bd)(content(Whitespace\" \
         \"))))(Secondary((id \
         a3356000-d00a-4fed-94c5-379703cab580)(content(Whitespace\" \
         \"))))(Secondary((id \
         7b173826-a123-4d54-8f02-e90c25aa67da)(content(Whitespace\" \
         \"))))(Secondary((id \
         8b1ba3f8-e90f-4828-bfc8-72a8da9c09d0)(content(Comment\"# Golden \
         stalks, hearty grain #\"))))(Secondary((id \
         3ad4de9b-c7f1-4a84-9aa3-a82c646b1b54)(content(Whitespace\"\\n\"))))(Tile((id \
         772108d8-e9d6-4008-9b36-e2882ca5bcd3)(label(+))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 12))(sort Typ))((shape(Concave \
         12))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         08827d04-8aef-4071-8ad0-696711559259)(content(Whitespace\" \
         \"))))(Tile((id \
         c9b6b874-bae2-4b1a-a28f-b9dc3ad95f48)(label(Glowpumpkin))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         baabeade-2325-410b-9934-efbba4988b3b)(content(Whitespace\" \
         \"))))(Secondary((id \
         bae7cdc7-baa6-42bd-b627-58fd0705863a)(content(Comment\"# Orange and \
         luminescent #\"))))(Secondary((id \
         99a031a3-f4b0-4876-acf6-36b97054be8c)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         db97ef78-c9e1-4cba-8fd3-d36369b36b7e)(content(Whitespace\"\\n\"))))(Secondary((id \
         b56821a8-e6bd-4181-be47-e7ecc7444f52)(content(Whitespace\"\\n\"))))(Secondary((id \
         fe881c56-4692-4277-b803-2531ebad0694)(content(Comment\"# A single \
         harvest from the garden #\"))))(Secondary((id \
         b4593baa-170c-4d8d-94ca-ffada321ab27)(content(Whitespace\"\\n\"))))(Tile((id \
         26136bcd-7645-49c0-b22f-e857a9c21a65)(label(type = in))(mold((out \
         Exp)(in_(TPat Typ))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         1897d734-049a-409c-bbf0-87d0e4eef4c2)(content(Whitespace\" \
         \"))))(Tile((id \
         e5369a7e-e68a-4ec6-ba5b-73f561cc0a1d)(label(Harvest))(mold((out \
         TPat)(in_())(nibs(((shape Convex)(sort TPat))((shape Convex)(sort \
         TPat))))))(shards(0))(children())))(Secondary((id \
         450d0c2f-2613-4637-b0e6-fd86683eeb6d)(content(Whitespace\" \
         \")))))((Secondary((id \
         e75e2a20-1bbc-4c33-a152-de5041dff3c4)(content(Whitespace\" \
         \"))))(Tile((id \
         2181e6b7-4545-4b63-95f4-1d590df14442)(label(\"(\"\")\"))(mold((out \
         Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0 1))(children(((Secondary((id \
         5a31b1d2-3e2d-40d9-a8d7-3f0f8de574e7)(content(Whitespace\"\\n\"))))(Tile((id \
         ffb8870d-a9d7-4dfd-9c8d-5d543a0dbcc6)(label(crop))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         5e2cffe2-a472-4e3b-962e-9949850f137e)(content(Whitespace\" \
         \"))))(Tile((id \
         017909e8-530a-4c07-a079-d89386566b56)(label(=))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 39))(sort Typ))((shape(Concave \
         39))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         d35f42c1-194c-4067-b6f5-dddd21b2f69f)(content(Whitespace\" \
         \"))))(Tile((id \
         835e87fd-a361-4e3e-b053-7f88589093ae)(label(Crop))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         ccfdc837-7beb-44a3-ac48-9ecb4b9d74a3)(label(,))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 44))(sort Typ))((shape(Concave \
         44))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         6a3c28bc-0d4f-49bc-9d5b-e40720bd9ca3)(content(Whitespace\"\\n\"))))(Tile((id \
         771c5d27-c241-4b02-8ff9-c54964afcbc0)(label(quality))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         e2061be0-1da1-4274-a62c-2928d228b992)(content(Whitespace\" \
         \"))))(Tile((id \
         9bf311f2-2c39-4233-a2f3-8eaf6642ac9d)(label(=))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 39))(sort Typ))((shape(Concave \
         39))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         78f995ff-b1b7-475c-a6b3-3748439fb4bc)(content(Whitespace\" \
         \"))))(Tile((id \
         d0aa4d72-71dd-4409-9afe-e906ff7bdd41)(label(Quality))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         2977eeb0-13d9-4477-a282-a6b685e054c9)(label(,))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 44))(sort Typ))((shape(Concave \
         44))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         1d0df260-545b-4d73-9a7d-1c15d42f9aed)(content(Whitespace\"\\n\"))))(Tile((id \
         97123be1-b7fe-4ca4-8366-7fc6b427dc5c)(label(quantity))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         0eaf6e4a-e78c-464a-80cf-98d7342870b4)(content(Whitespace\" \
         \"))))(Tile((id \
         a8ae2eb4-cb09-4564-bc13-2eb715c98a1b)(label(=))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 39))(sort Typ))((shape(Concave \
         39))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         7317cb21-b0c5-4f58-b884-284676990b94)(content(Whitespace\" \
         \"))))(Tile((id \
         41483568-344a-468b-8f69-b62626d557b1)(label(Int))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         b2aa65a8-765b-4c3a-98be-053279d2ed32)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         12615c73-96db-447b-8b3e-bcde951ef401)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         db65edfb-ddca-4b04-91ac-3dc5fa6344ca)(content(Whitespace\"\\n\"))))(Secondary((id \
         44c49bd4-6cab-47d9-a2e5-c133b9c31ec0)(content(Whitespace\"\\n\"))))(Secondary((id \
         41588b2a-6adb-4710-be1c-d5451bfa7be8)(content(Comment\"# The harvest \
         ledger tracks all harvests and bonuses #\"))))(Secondary((id \
         d639a166-9a95-4c7e-9600-69dfa83c904e)(content(Whitespace\"\\n\"))))(Tile((id \
         1230f7f6-395f-4597-a4b3-315b76bdb877)(label(type = in))(mold((out \
         Exp)(in_(TPat Typ))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         a8fba79b-13fb-4876-8b98-dd1b0239e2c3)(content(Whitespace\" \
         \"))))(Tile((id \
         2e5c89eb-5d2e-472a-85cc-ec2bf20ae3ac)(label(Model))(mold((out \
         TPat)(in_())(nibs(((shape Convex)(sort TPat))((shape Convex)(sort \
         TPat))))))(shards(0))(children())))(Secondary((id \
         a824aeaf-de27-4d8d-8334-cef8a7d13e72)(content(Whitespace\" \
         \")))))((Secondary((id \
         599c67cc-0411-4b47-8d2d-33b1e4c29158)(content(Whitespace\" \
         \"))))(Tile((id \
         d9717569-a978-43b8-88ac-aaa14390feed)(label(\"(\"\")\"))(mold((out \
         Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0 1))(children(((Secondary((id \
         6da1dc85-de7e-4215-9c8d-3074f1e4c101)(content(Whitespace\"\\n\"))))(Tile((id \
         6891a886-9553-468a-ad91-29f03520b251)(label(harvests))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         30c1dc96-f118-4a95-8668-34830539acbe)(content(Whitespace\" \
         \"))))(Tile((id \
         1c2361ab-8f6f-45ea-b344-6127b3b738e8)(label(=))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 39))(sort Typ))((shape(Concave \
         39))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         374b544b-4839-4352-bca2-9ba556803f64)(content(Whitespace\" \
         \"))))(Tile((id 823c122a-e0fd-41ac-b949-839321a985ea)(label([ \
         ]))(mold((out Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape \
         Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
         99c82ffd-2d7c-436c-9550-38ef4a03a477)(label(Harvest))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children()))))))))(Tile((id \
         faae1212-b27d-430d-a902-f6f0feafa5b1)(label(,))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 44))(sort Typ))((shape(Concave \
         44))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         2f8bb456-c31e-403e-9f89-22f1088c54ae)(content(Whitespace\"\\n\"))))(Tile((id \
         a1f0b42a-bfee-4122-8f6d-db7ac3be5937)(label(totalValue))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         0ace861b-e260-497b-82bc-ba9a3a2da0e3)(content(Whitespace\" \
         \"))))(Tile((id \
         01b41777-598a-44b5-835d-67756cb92429)(label(=))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 39))(sort Typ))((shape(Concave \
         39))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         5412f6ba-eb37-4799-aa12-45eb35d4a1ca)(content(Whitespace\" \
         \"))))(Tile((id \
         213451cd-f709-4f03-ae32-8594208243b6)(label(Int))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         c536cd14-44e3-4e92-a4da-f2d00985114e)(label(,))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 44))(sort Typ))((shape(Concave \
         44))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         5bd4e708-7b99-408c-828c-f163b0088e05)(content(Whitespace\"\\n\"))))(Tile((id \
         b9cf7e1c-147f-4f64-a297-2df25702e64e)(label(streakBonus))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         118cce2a-3f98-4a7b-a73a-61c1825392ce)(content(Whitespace\" \
         \"))))(Tile((id \
         0565d65b-c1ef-4aa5-9fe1-410f41afb880)(label(=))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 39))(sort Typ))((shape(Concave \
         39))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         23fd5c11-fc8d-4157-a84f-6ef4288ed1ee)(content(Whitespace\" \
         \"))))(Tile((id \
         6eec4407-6519-4cad-a20e-e84dfbb62640)(label(Int))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         09d6588d-0ba7-428f-8f06-037e840aaf79)(label(,))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 44))(sort Typ))((shape(Concave \
         44))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         809a3395-9278-439c-9ed1-195f4087a666)(content(Whitespace\"\\n\"))))(Tile((id \
         3a118da2-e78c-4eb7-b9c6-b6a57f9264ef)(label(lastQuality))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         93396cd6-c0f4-4992-873b-99626e705a6d)(content(Whitespace\" \
         \"))))(Tile((id \
         58ea5142-e0be-4e04-a513-889419e33bd2)(label(=))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 39))(sort Typ))((shape(Concave \
         39))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         b9bdbda0-56fd-4ea8-8d52-5cf83fa9c62e)(content(Whitespace\" \
         \"))))(Tile((id \
         c92b6e20-10a7-4115-a3ec-e2917f108231)(label(Quality))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         4df66bd8-6151-4c15-b097-5c17ce6d861b)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         c45c19b5-8975-4f59-87d1-9dd177a0dea4)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         bc8cdf1d-05e9-4829-9730-410558162dbb)(content(Whitespace\"\\n\"))))(Secondary((id \
         29db040e-98ae-419f-b400-7d456c3f9a43)(content(Whitespace\"\\n\"))))(Secondary((id \
         e266bdbf-acf4-47c0-953a-dae5786d5b62)(content(Comment\"# Actions the \
         farmer can take #\"))))(Secondary((id \
         31829a6d-7c56-4b9f-bbf5-99a26017cfae)(content(Whitespace\"\\n\"))))(Tile((id \
         09278125-e0cf-4b2a-ac4d-f399815de2d2)(label(type = in))(mold((out \
         Exp)(in_(TPat Typ))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         888f4bcd-87fd-437e-b3dc-f501e405acad)(content(Whitespace\" \
         \"))))(Tile((id \
         67fead44-be8b-4edb-a694-12eba977f54b)(label(Action))(mold((out \
         TPat)(in_())(nibs(((shape Convex)(sort TPat))((shape Convex)(sort \
         TPat))))))(shards(0))(children())))(Secondary((id \
         f49e7b09-4099-4d2e-9bab-2032d7056788)(content(Whitespace\" \
         \")))))((Secondary((id \
         ed41d755-d6e5-4178-b058-2e1e919a8292)(content(Whitespace\"\\n\"))))(Tile((id \
         0cd83747-25dd-403a-9ed1-77265b531be4)(label(+))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape(Concave 33))(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         ac2f20bb-c7ae-4790-9359-54f7c766a79e)(content(Whitespace\" \
         \"))))(Tile((id \
         7aeed6a4-0b67-431f-8653-cc06c380f57e)(label(RecordHarvest))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         eb871f5b-4634-45c6-aab8-a5b277970dbf)(label(\"(\"\")\"))(mold((out \
         Typ)(in_(Typ))(nibs(((shape(Concave 11))(sort Typ))((shape \
         Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
         35d1a590-a018-4d09-bd0f-c7e6ad5146e6)(label(Harvest))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children()))))))))(Secondary((id \
         52be25dc-56b8-4cc2-b248-fad0c63a1005)(content(Whitespace\" \
         \"))))(Secondary((id \
         478c7e21-f014-4279-8011-64cc0dc26be8)(content(Whitespace\" \
         \"))))(Secondary((id \
         6a9556c1-270f-423d-83f4-a8bad70975cb)(content(Comment\"# Log a new \
         harvest #\"))))(Secondary((id \
         7a0ee913-6398-4fb5-b60d-876c4b4469e2)(content(Whitespace\"\\n\"))))(Tile((id \
         ee215d25-91be-468c-b06a-76e706f3f427)(label(+))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 12))(sort Typ))((shape(Concave \
         12))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         762d93a7-f199-4c0e-ae67-609969ebb0f8)(content(Whitespace\" \
         \"))))(Tile((id \
         2d0f7a34-c940-453a-9d53-f67d1f60d32e)(label(ClaimBonus))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         f1fba4e3-cc7c-47d5-9cc5-8313bf137d85)(content(Whitespace\" \
         \"))))(Secondary((id \
         62e3ebe9-4bee-4bf6-b5dc-bfb59edc302b)(content(Whitespace\" \
         \"))))(Secondary((id \
         dc795e33-587a-42bf-a902-c0fc7e9786e4)(content(Whitespace\" \
         \"))))(Secondary((id \
         27eaafc6-e395-4a1a-89df-0ac7fc80fd10)(content(Whitespace\" \
         \"))))(Secondary((id \
         06b97710-6e09-4d85-802f-75d19cec4a21)(content(Whitespace\" \
         \"))))(Secondary((id \
         892bf62d-f242-41a8-8485-bac8244a3c25)(content(Whitespace\" \
         \"))))(Secondary((id \
         813e86c6-c13e-4f56-ac9f-fb281c874b7e)(content(Whitespace\" \
         \"))))(Secondary((id \
         cca286a6-6027-4065-b096-546dd5aa6787)(content(Whitespace\" \
         \"))))(Secondary((id \
         2dfae679-d0fb-4638-8876-2c41fecb850b)(content(Whitespace\" \
         \"))))(Secondary((id \
         54542825-f568-430c-bd07-0e87dbac7746)(content(Whitespace\" \
         \"))))(Secondary((id \
         1340d48c-f95a-4e9a-9466-a876f7fc6850)(content(Whitespace\" \
         \"))))(Secondary((id \
         a42347dc-51c6-416d-9680-249dfdd1c532)(content(Whitespace\" \
         \"))))(Secondary((id \
         00e08c92-0665-480a-9a60-4daa430530df)(content(Whitespace\" \
         \"))))(Secondary((id \
         a75627f6-79ce-4397-ace0-4d383c2d18dd)(content(Whitespace\" \
         \"))))(Secondary((id \
         7595cbb9-c717-495e-9a8a-4641916d6823)(content(Comment\"# Collect \
         accumulated streak bonus #\"))))(Secondary((id \
         5543a01e-1ea2-4e87-8767-6039817ce7c0)(content(Whitespace\"\\n\"))))(Tile((id \
         bccbd28d-3aa7-44fb-be1c-2d0f663f8e9c)(label(+))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 12))(sort Typ))((shape(Concave \
         12))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         a5d17f4d-6516-45ec-9601-c9c4e0778a34)(content(Whitespace\" \
         \"))))(Tile((id \
         9b7a9f32-9f42-42c2-90b9-6ede7f921234)(label(CloseDay))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         449ac301-b739-4062-818f-55b78f411f4a)(content(Whitespace\" \
         \"))))(Secondary((id \
         c85f90ed-9505-4722-80ae-c2107946c82a)(content(Whitespace\" \
         \"))))(Secondary((id \
         272a95bc-3ed5-44fe-a36a-7fa1473cd2b1)(content(Whitespace\" \
         \"))))(Secondary((id \
         c3722009-a894-4d6e-8cd8-79c4e2920c94)(content(Whitespace\" \
         \"))))(Secondary((id \
         289e3179-c9ba-48d5-a902-7800558551b1)(content(Whitespace\" \
         \"))))(Secondary((id \
         22a0da29-bd05-48ee-aa91-779e52e568d1)(content(Whitespace\" \
         \"))))(Secondary((id \
         dcd8b0ae-43cb-4717-8762-627f609038ad)(content(Whitespace\" \
         \"))))(Secondary((id \
         ff33435c-9c98-45f0-b5d7-b606f6c97358)(content(Whitespace\" \
         \"))))(Secondary((id \
         d2453de4-c600-432d-bd17-b92028bb8a5f)(content(Whitespace\" \
         \"))))(Secondary((id \
         4364d7cb-9be4-45b6-9cc5-1ec128884500)(content(Whitespace\" \
         \"))))(Secondary((id \
         95ec4fb1-bdc6-420c-b909-8e5161c7043c)(content(Whitespace\" \
         \"))))(Secondary((id \
         4949f844-f9b8-44c9-a6ce-27b1b2f94121)(content(Whitespace\" \
         \"))))(Secondary((id \
         95db7bd2-8be7-45d3-81b0-eb3396bafdcb)(content(Whitespace\" \
         \"))))(Secondary((id \
         08f3d171-d17d-4322-94ef-be9430f7e905)(content(Whitespace\" \
         \"))))(Secondary((id \
         19c76cb0-e205-4e33-a68b-afa79fa71789)(content(Whitespace\" \
         \"))))(Secondary((id \
         04d80ce3-e60c-4578-82b7-10bb43f24e41)(content(Whitespace\" \
         \"))))(Secondary((id \
         602ba515-912b-470e-94ec-72b375fe1f28)(content(Comment\"# End the \
         harvest day, reset streaks #\"))))(Secondary((id \
         69b69987-084b-44a5-8b8b-42eff26a567a)(content(Whitespace\"\\n\"))))(Secondary((id \
         50bc1df6-b425-49c2-9717-344fcf93d7a3)(content(Comment\"# TODO: Add \
         PremiumSale here #\"))))(Secondary((id \
         a8868407-e978-4743-b43d-b0ad68c0b376)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         b40d7502-4a49-4c94-be20-cca5a8525e08)(content(Whitespace\"\\n\"))))(Secondary((id \
         52262ba6-8dfc-40fb-90e7-272d25499c6e)(content(Whitespace\"\\n\"))))(Secondary((id \
         6afc5701-5ca4-487c-bc8f-443b80394448)(content(Comment\"# Calculate \
         base value of a crop #\"))))(Secondary((id \
         93242a8b-c1da-4319-9c14-d356011bcc80)(content(Whitespace\"\\n\"))))(Tile((id \
         53e13356-5fdf-4979-8932-4886da23b1dc)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         702b37fa-60d3-4811-985c-6a67fe18cbe1)(content(Whitespace\" \
         \"))))(Tile((id \
         83568ab2-3721-4ce9-b5df-3b2e1288123d)(label(cropValue))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         9669041a-0bc0-4790-a3ef-0dce6c5ee4f0)(content(Whitespace\" \
         \"))))(Tile((id \
         a08fd0ca-54bb-48ee-90a9-654fca34344d)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 24))(sort Pat))((shape(Concave \
         24))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         343f22ce-849f-4b79-92c6-b0d47649f6cf)(content(Whitespace\" \
         \"))))(Tile((id \
         77b6aaeb-0299-4e79-8d3e-f211725241dd)(label(Crop))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         acc54567-1709-4e86-b698-a64939126f0a)(content(Whitespace\" \
         \"))))(Tile((id \
         7ce16831-a68b-4dc0-97ca-0fd1cfed2fdf)(label(->))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 13))(sort Typ))((shape(Concave \
         13))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         780133ca-11c6-4ebc-a3f9-7e2fe4089e67)(content(Whitespace\" \
         \"))))(Tile((id \
         02dcadab-fb75-477c-b2d2-6f9b9ec482da)(label(Int))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         bbc1d797-7e98-47c4-b772-6bbd6446bd85)(content(Whitespace\" \
         \")))))((Secondary((id \
         6a11b057-eca7-4e38-b726-d64ad8c5a5c6)(content(Whitespace\"\\n\"))))(Tile((id \
         c0f97921-7e7f-4cdb-bf3d-83c05afb5f6b)(label(fun ->))(mold((out \
         Exp)(in_(Pat))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         37))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         fdde3256-8f54-49ef-a441-bff9ab97c83a)(content(Whitespace\" \
         \"))))(Tile((id \
         258e9c58-8736-48c1-8b0f-3fba58282efe)(label(c))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         9b59640c-be93-49e2-bff2-452927fb6bd2)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         e1def5dc-1a85-4066-9c70-688a6164d152)(content(Whitespace\"\\n\"))))(Tile((id \
         4e236af3-441a-488d-b613-a0f7927ef254)(label(case end))(mold((out \
         Exp)(in_(Rul))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         56b0ea9d-1546-46ef-b026-73cc7c3d1950)(content(Whitespace\" \
         \"))))(Tile((id \
         5b5f8211-b581-4d92-8bef-b16d65d913bd)(label(c))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         20a353e6-1626-4b40-bdaa-a5a53ec53551)(content(Whitespace\"\\n\"))))(Tile((id \
         e08f3425-37c6-43aa-9952-f4f2249e538e)(label(| =>))(mold((out \
         Rul)(in_(Pat))(nibs(((shape(Concave 46))(sort Exp))((shape(Concave \
         46))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         e3570fce-67e9-435f-8a17-a3cff82533e1)(content(Whitespace\" \
         \"))))(Tile((id \
         f36a0361-f522-4b5a-bd7d-797ccac55eb2)(label(Moonmelon))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         f7bfd4d9-00e3-45f1-8deb-168adb6db235)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         3211cd70-cb94-4d18-9a9f-54125613028a)(content(Whitespace\" \
         \"))))(Tile((id \
         6026cb2b-ebc8-43a4-9b4b-723b505a8b14)(label(15))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         1217122f-cd44-46f5-b81e-89e034b2285e)(content(Whitespace\"\\n\"))))(Tile((id \
         82fda499-b2ef-44e6-bf81-79f375c9033f)(label(| =>))(mold((out \
         Rul)(in_(Pat))(nibs(((shape(Concave 46))(sort Exp))((shape(Concave \
         46))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         28ddb57d-1e93-4fe9-a3be-47d8e5b7f795)(content(Whitespace\" \
         \"))))(Tile((id \
         27036f21-1f3a-4f6b-ad9a-3bfb07da196b)(label(Starfruit))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         231c41ab-845e-422b-83d6-065a1c438081)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         e320afba-50f3-41df-9673-a52690fd600d)(content(Whitespace\" \
         \"))))(Tile((id \
         d8c54065-10dd-4d25-a5e3-9f8b88c5d3b9)(label(20))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         31bdd427-1bc3-4bf3-96b3-92718875b034)(content(Whitespace\"\\n\"))))(Tile((id \
         0e660184-4dd7-4be5-aa1f-cd9e35077fb3)(label(| =>))(mold((out \
         Rul)(in_(Pat))(nibs(((shape(Concave 46))(sort Exp))((shape(Concave \
         46))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         959284f7-6ebb-41cf-a026-bd0614a038fc)(content(Whitespace\" \
         \"))))(Tile((id \
         10647061-f14b-4a36-81dd-f21a5f3fecbb)(label(Nightshade))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         4864c9db-9246-480a-ab4a-c98c4a78f16c)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         4b7268aa-f06a-44b4-8f07-78abbf3e173e)(content(Whitespace\" \
         \"))))(Tile((id \
         1d03102f-f722-4c90-bac0-551be3180694)(label(20))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         aac5eb54-1aa2-4156-a8d4-37845e97d49f)(content(Whitespace\"\\n\"))))(Tile((id \
         206d1eab-f419-4ba8-a5c3-78ffd103a0a1)(label(| =>))(mold((out \
         Rul)(in_(Pat))(nibs(((shape(Concave 46))(sort Exp))((shape(Concave \
         46))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         1bb5a8ce-0d15-4e46-803c-b774b546c4d2)(content(Whitespace\" \
         \"))))(Tile((id \
         524f68be-02cf-4f3e-82c9-6d46b2ef0463)(label(Duskwheat))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         77609c15-ab6d-456c-98b5-64bc0e886611)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         d72e1bc9-6d7d-4395-ba48-fa6541d3f610)(content(Whitespace\" \
         \"))))(Tile((id \
         9e71b5c1-ee69-402f-8bcf-9095a35cac7c)(label(10))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         809eee6d-1d7d-4fa7-a7a1-b1f682f66fbd)(content(Whitespace\"\\n\"))))(Tile((id \
         715af849-c742-4885-9bf9-86b708be3e27)(label(| =>))(mold((out \
         Rul)(in_(Pat))(nibs(((shape(Concave 46))(sort Exp))((shape(Concave \
         46))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         f4f99eb1-881a-468e-b5a8-4c9f4f8d542f)(content(Whitespace\" \
         \"))))(Tile((id \
         e879d774-dd58-4afd-a589-31e731367445)(label(Glowpumpkin))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         38845606-7561-4f2c-97a0-9029e44833f3)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         098326ba-8b4b-4315-86f2-f859c9b0b0ea)(content(Whitespace\" \
         \"))))(Tile((id \
         d35ec591-d732-491f-bd44-a944335e6236)(label(12))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         ff45571e-c7b8-4cae-8d51-b1bf56443d55)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         bed7b35e-b54e-4c8c-98b0-54cd5f591ecc)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         c7fb7f27-644e-42a8-8d1c-f3012c44ef7d)(content(Whitespace\"\\n\"))))(Secondary((id \
         9ddaf055-7946-451f-860d-974fe5d68309)(content(Whitespace\"\\n\"))))(Secondary((id \
         30adf60d-164a-4cb2-a1ff-0e937f6d7d8d)(content(Comment\"# Quality \
         multiplier for harvest value #\"))))(Secondary((id \
         37736796-8ca6-4443-a099-81889d3f5c8b)(content(Whitespace\"\\n\"))))(Tile((id \
         6e0b22ae-2185-47d6-8925-a6fec8e7f9e8)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         cb6aa1ce-6a6f-4626-8c25-857f241a5462)(content(Whitespace\" \
         \"))))(Tile((id \
         18917a94-25db-494e-b9d7-23734b05c8ea)(label(qualityMultiplier))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         6a167c4b-1446-4e31-bc7b-bf5f407f4bad)(content(Whitespace\" \
         \"))))(Tile((id \
         475b6941-b302-4478-b9ca-902d351f63f8)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 24))(sort Pat))((shape(Concave \
         24))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         705a4f45-efcd-4647-9294-106469571620)(content(Whitespace\" \
         \"))))(Tile((id \
         80e1f65d-7f4a-4164-a803-c4a5b5cafca7)(label(Quality))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         4582d356-2b58-4bdb-8a6f-88d4c5f80c9f)(content(Whitespace\" \
         \"))))(Tile((id \
         57a81250-66e2-403b-8152-24a2e1edbb23)(label(->))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 13))(sort Typ))((shape(Concave \
         13))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         acc8969f-2ea5-45ae-bb79-f11b8cc93d90)(content(Whitespace\" \
         \"))))(Tile((id \
         10fffa3c-0012-43c5-a0ab-437239a35fa9)(label(Int))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         76293215-3acf-4127-b779-627797963d06)(content(Whitespace\" \
         \")))))((Secondary((id \
         3b94ad2c-994e-4ed2-a0f7-67d660ae8546)(content(Whitespace\"\\n\"))))(Tile((id \
         568c6813-f7eb-4552-aed5-8860119fea8e)(label(fun ->))(mold((out \
         Exp)(in_(Pat))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         37))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         ebf98075-0864-4435-bdef-9ff9858523d2)(content(Whitespace\" \
         \"))))(Tile((id \
         6768ce34-d923-4b47-aa6d-2a5fc8ec350c)(label(q))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         5d405b19-16d2-4fc3-b116-ef48479d03f2)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         31fe3552-0158-490e-a3b3-a196b8d9690c)(content(Whitespace\"\\n\"))))(Tile((id \
         e36485f5-dfdc-416d-a7e6-544823218bb7)(label(case end))(mold((out \
         Exp)(in_(Rul))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         d0940593-7c78-468f-8db8-5fff6d66b290)(content(Whitespace\" \
         \"))))(Tile((id \
         1b2d0d27-adbe-4c08-b319-40f164462b04)(label(q))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         a40b0ccf-25c8-46e7-af64-0614e1c145e3)(content(Whitespace\"\\n\"))))(Tile((id \
         7cee4b78-4102-4b56-9e1d-001f4d318048)(label(| =>))(mold((out \
         Rul)(in_(Pat))(nibs(((shape(Concave 46))(sort Exp))((shape(Concave \
         46))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         85e1068a-ffbf-417e-9b49-5cc88db8696a)(content(Whitespace\" \
         \"))))(Tile((id \
         1249e459-57de-4479-a5a9-6d519843dc49)(label(Bronze))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         bd39af06-bebc-4120-8573-bbb691a53d7c)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         6200017d-9e81-48e4-9e01-2e69436d18a2)(content(Whitespace\" \
         \"))))(Tile((id \
         5b463e29-3d6e-4d41-a61c-26fa2356d7b7)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         c416d452-e226-4dbb-9af6-ff7c16f68e44)(content(Whitespace\"\\n\"))))(Tile((id \
         291265ed-0f72-464e-9e87-8df10a3ba826)(label(| =>))(mold((out \
         Rul)(in_(Pat))(nibs(((shape(Concave 46))(sort Exp))((shape(Concave \
         46))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         b6a0a029-2d9e-4ee0-b4c2-9cdf43321d7b)(content(Whitespace\" \
         \"))))(Tile((id \
         e0ae4216-b39d-45ec-8be7-d78df3177d5e)(label(Silver))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         4be7a790-621b-485d-9bb2-1342d0d60747)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         b1fc032d-a906-4ea8-9f44-2c4606ca3d59)(content(Whitespace\" \
         \"))))(Tile((id \
         8e75e428-2bba-4519-a035-d8ef666b494d)(label(2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         0e341aa0-5430-4a5b-9b76-af5485d52801)(content(Whitespace\"\\n\"))))(Tile((id \
         821068f4-3ad5-40ba-9faf-2449a37c2b85)(label(| =>))(mold((out \
         Rul)(in_(Pat))(nibs(((shape(Concave 46))(sort Exp))((shape(Concave \
         46))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         b3051c88-4a58-4b63-997f-98b408311ef0)(content(Whitespace\" \
         \"))))(Tile((id \
         9d8d70f9-522d-45b8-a63b-f35bfed65109)(label(Gold))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         9134214c-7885-4773-9431-925cc8040b68)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         a94a6aa2-c164-45fe-993c-c6dd04cceb84)(content(Whitespace\" \
         \"))))(Tile((id \
         0d7e99b8-c625-49db-ac76-56b38c701f5d)(label(3))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         f3d3f6b0-c827-4b0f-a598-949a08aa1820)(content(Whitespace\"\\n\"))))(Tile((id \
         6bc65d2d-5f98-417a-8836-c8172873afcf)(label(| =>))(mold((out \
         Rul)(in_(Pat))(nibs(((shape(Concave 46))(sort Exp))((shape(Concave \
         46))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         958fe50a-6225-4d11-8851-31e8fedd31ac)(content(Whitespace\" \
         \"))))(Tile((id \
         ce9e2da2-71d9-44b2-b98a-6d35947cdbef)(label(Starlight))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         a52dbcae-1680-462e-a477-93eeb634f7da)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         6dedd6b7-285d-4c2a-a99d-d53904577b35)(content(Whitespace\" \
         \"))))(Tile((id \
         449d59a0-c98e-4aa2-9a00-71315ad89450)(label(5))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         663282ce-34a8-4a30-b2ff-d2a6380dcfe3)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         515daf16-c717-411e-a44d-0fc07809f742)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         00c14866-d5fb-4440-a657-1d3e76e58b9c)(content(Whitespace\"\\n\"))))(Secondary((id \
         1a330669-5737-4b7b-a17d-68988a4c99ad)(content(Whitespace\"\\n\"))))(Secondary((id \
         36fa7f52-7b37-4c40-807d-516cc84726ec)(content(Comment\"# Calculate \
         the value of a single harvest #\"))))(Secondary((id \
         a82e4fa8-bd30-4f73-8657-cac1752a1b25)(content(Whitespace\"\\n\"))))(Tile((id \
         56fd0b94-c695-45b0-a400-9941f50faa4f)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         23e215bd-e5d0-4235-8399-9c213656f8a3)(content(Whitespace\" \
         \"))))(Tile((id \
         afdda841-f24a-44d7-a011-3cb37226dcac)(label(harvestValue))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         abfc1e9c-ce01-420e-9255-039a4c96c0a9)(content(Whitespace\" \
         \"))))(Tile((id \
         4d0e77b3-5027-4eb6-b014-96849ecd1b37)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 24))(sort Pat))((shape(Concave \
         24))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         6f118f7d-1406-40b0-a612-e23fa072a977)(content(Whitespace\" \
         \"))))(Tile((id \
         af0e27f0-ff95-413e-a9fb-a345c0a4c4cc)(label(Harvest))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         94e00ff5-7df3-403a-b4e3-f2df5f2bd88c)(content(Whitespace\" \
         \"))))(Tile((id \
         6d7c03be-8c67-40d4-827e-8df4ff90dfc6)(label(->))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 13))(sort Typ))((shape(Concave \
         13))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         5ce59b80-b235-4228-8a12-77a46f3bfe49)(content(Whitespace\" \
         \"))))(Tile((id \
         114147f5-f1af-4a61-be76-436740ade6f0)(label(Int))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         40bfc1de-6780-45d9-892f-d98d70a925a6)(content(Whitespace\" \
         \")))))((Secondary((id \
         36cef707-8021-412d-a288-a7c0b7482e50)(content(Whitespace\"\\n\"))))(Tile((id \
         18edd223-232b-4902-b7c9-b6a229c257a9)(label(fun ->))(mold((out \
         Exp)(in_(Pat))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         37))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         3cc36a3b-f7b7-42ff-b39a-bfa489e8a7a9)(content(Whitespace\" \
         \"))))(Tile((id \
         b6310c90-5907-463b-9a14-b56fa3e96878)(label(h))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         8aae17a7-96ee-4cbc-b460-24f4e9c2c64d)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         3204eadb-8f50-45c8-a420-8f6e1385fe46)(content(Whitespace\"\\n\"))))(Tile((id \
         5830acca-c325-4f39-9f7e-c76752a772e8)(label(cropValue))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         5220557f-f430-46b5-9abb-b655693be2af)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         90057964-604d-4989-a754-033e2efc7b6f)(label(h))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         ab845549-560a-44b7-ba58-45569d03be11)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         683230c9-c2d3-4396-b7e7-1e99375c8365)(label(crop))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         0b761011-67ae-4b97-b3b7-76e308308654)(content(Whitespace\" \
         \"))))(Tile((id \
         6d8b77e1-f332-4488-9198-6c2fd26e63df)(label(*))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 27))(sort Exp))((shape(Concave \
         27))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         249f24bb-ea61-4fe7-873e-0d6b5447ae5a)(content(Whitespace\" \
         \"))))(Tile((id \
         8f4a0260-583c-4b27-9c53-fb8aa6fabbaa)(label(qualityMultiplier))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         22181226-a609-4a51-a657-41d8096daeb5)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         974540d7-b128-4c9b-8790-2628d3bbed55)(label(h))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         8bfa6015-8749-4e84-94c2-4b805353b316)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         e15d12a9-ad93-408c-baed-cb690a03b2ec)(label(quality))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         dcda3898-f1a7-4096-93e4-58ab1262f617)(content(Whitespace\" \
         \"))))(Tile((id \
         31072dbd-0064-4476-9102-ca5a23533936)(label(*))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 27))(sort Exp))((shape(Concave \
         27))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         ed094d59-0d13-4c01-990b-59315a4b8c3a)(content(Whitespace\" \
         \"))))(Tile((id \
         84c42351-8adf-4c64-992b-2b43810a64f9)(label(h))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         6b951d4f-3c50-4ea5-8f21-968506f2b0e5)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         9aeac2de-8a46-4704-9250-1f7e2bcbdad5)(label(quantity))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         7818e05f-611d-461f-9f4f-7384c84cbbf2)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         86bed83f-5ba2-431a-8d7c-fa2235f4d386)(content(Whitespace\"\\n\"))))(Secondary((id \
         e9490510-94bf-453a-bb2c-9a9caf650526)(content(Whitespace\"\\n\"))))(Secondary((id \
         33a76588-774a-40e6-beaa-7cb82b9ce012)(content(Comment\"# Initial \
         empty ledger #\"))))(Secondary((id \
         deff9ebe-4216-4bf3-86b0-f652f743699e)(content(Whitespace\"\\n\"))))(Tile((id \
         a4ee745b-a258-4f3c-911d-6afaa11d1b56)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         3bec8ac9-ab3c-4b78-8e7a-c9239aa9e84a)(content(Whitespace\" \
         \"))))(Tile((id \
         e5e25de5-de09-44c9-b5f1-d36bb0793873)(label(initModel))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         c2526044-75f0-4587-b459-2fe979280041)(content(Whitespace\" \
         \"))))(Tile((id \
         c25bbf65-4252-4010-bc62-f7d9419544ab)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 24))(sort Pat))((shape(Concave \
         24))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         6c7db20e-015d-4357-a307-826bdb931924)(content(Whitespace\" \
         \"))))(Tile((id \
         275ab444-969f-4714-99f5-9c2f63d317a2)(label(Model))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         e0a5a7d6-756d-448f-9971-47cc598bb8c7)(content(Whitespace\" \
         \")))))((Secondary((id \
         6d3add6c-fa44-4996-b836-711dcf83d0ef)(content(Whitespace\" \
         \"))))(Tile((id \
         c6fac2f9-799a-4dc7-b05c-2f098783649e)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         301b5579-f45b-460c-b224-7c06f73d39e8)(content(Whitespace\"\\n\"))))(Tile((id \
         cda5a5c8-c6e5-4aff-bac2-8436da98b33b)(label(harvests))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         91dbd4d0-86e2-4bd4-b5bb-312fd2fbdcc7)(content(Whitespace\" \
         \"))))(Tile((id \
         a854c28a-0381-422e-a407-3fdc2ea6c4fd)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         72c94c24-b30b-4cc1-941c-9b73015758fe)(content(Whitespace\" \
         \"))))(Tile((id \
         2eceef10-03dd-4e91-8f49-47cc4da2414b)(label([]))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         e72d84b8-98fe-4663-92a0-0ab584fe214f)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         36dee17a-e2fb-443a-b8df-933af09afde5)(content(Whitespace\"\\n\"))))(Tile((id \
         cafd0462-57b7-45c1-8a8d-bb5976dd08e2)(label(totalValue))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         117678a2-7cad-4358-9211-7ac1ec24f9dc)(content(Whitespace\" \
         \"))))(Tile((id \
         20890410-ad59-424e-a78d-e9aef968bccd)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         ed1854e9-aad8-4b82-85fd-7170b345f506)(content(Whitespace\" \
         \"))))(Tile((id \
         a9d1585e-a9df-4844-b8ef-8df3fc607a30)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         e97d0593-fd09-4c26-8bb0-a617c01095c3)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         c1268e36-13fa-492e-9ec4-030b0c01e5e3)(content(Whitespace\"\\n\"))))(Tile((id \
         47cd6aed-6661-4d30-96bf-cff35349da58)(label(streakBonus))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         3f84fef3-3900-4ff2-930f-3ee449c40618)(content(Whitespace\" \
         \"))))(Tile((id \
         21118f68-c529-468f-b295-10ad4ecdc4f0)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         0bbe8db2-f499-4ab6-85cd-0d2329f5ec09)(content(Whitespace\" \
         \"))))(Tile((id \
         6947b304-d87a-4076-a82f-4286383da3d0)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         b2f750de-02c6-4623-8616-6e9e8e45c7c9)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         d1c78425-6756-44d1-8ec1-cf9800e69c34)(content(Whitespace\"\\n\"))))(Tile((id \
         5c2cf647-5bdd-4b16-bab9-07cbbaba3078)(label(lastQuality))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         82c9d96d-1ae7-468a-819b-5c7bd90d0506)(content(Whitespace\" \
         \"))))(Tile((id \
         670f3152-5986-45d0-9481-ae5fb2792153)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         510ebd50-4ef9-4afc-ac1b-0f249de62139)(content(Whitespace\" \
         \"))))(Tile((id \
         9d67fd02-8c2c-4058-8b3b-636f11e81620)(label(Bronze))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         5596f6f7-585c-4570-8a19-dc0c410c2de2)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         5a87d344-162e-4f0f-a41d-0d1d449f82b4)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         c3294fb4-519c-407c-bc92-1c8f1e6d67b3)(content(Whitespace\"\\n\"))))(Secondary((id \
         275d661a-6f48-4a00-bd7a-9ab9cc20d61d)(content(Whitespace\"\\n\"))))(Secondary((id \
         8c0b81ac-38de-471a-8cba-0f59c975ead3)(content(Comment\"# Process a \
         harvest action and update the ledger #\"))))(Secondary((id \
         1457f30c-8ab5-4bff-afd6-26032ff8f5f5)(content(Whitespace\"\\n\"))))(Tile((id \
         3637a9b8-f115-4171-8073-606976194071)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         23028347-b402-45aa-bb00-93c24ada9e96)(content(Whitespace\" \
         \"))))(Tile((id \
         6ee3548b-5877-45d8-9007-246720b223fe)(label(processHarvest))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         fcbe6d3d-4185-45d6-8bf0-c6997dabbf9f)(content(Whitespace\" \
         \"))))(Tile((id \
         da2cd698-22ff-4b74-b911-9ef7e5604f61)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 24))(sort Pat))((shape(Concave \
         24))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         cc630d51-adea-4104-8920-f502d2db5ae2)(content(Whitespace\" \
         \"))))(Tile((id \
         09f73f3d-1968-4724-8b1f-2fe14b7779ea)(label(\"(\"\")\"))(mold((out \
         Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0 1))(children(((Tile((id \
         edd68ee0-ff5a-48df-8317-446d23bba0f4)(label(Model))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         97239511-dd95-4521-b239-64347d6d55e0)(label(,))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 44))(sort Typ))((shape(Concave \
         44))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         efc0faf8-8b58-477d-afa2-12ebbf512e9b)(content(Whitespace\" \
         \"))))(Tile((id \
         81275e4b-0f0e-4598-b1ba-884f154c0afd)(label(Harvest))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children()))))))))(Secondary((id \
         e1190f38-4786-4ad3-b0f3-e67e7b679474)(content(Whitespace\" \
         \"))))(Tile((id \
         80840bf2-3c2c-4d3e-a05c-59074fd14b48)(label(->))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 13))(sort Typ))((shape(Concave \
         13))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         0651e4f2-17f1-40a0-926e-5fd4609ff7ea)(content(Whitespace\" \
         \"))))(Tile((id \
         9581ccf2-e4f6-4bf5-bdda-c01a6b7c2678)(label(Model))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         e46124d8-607b-417f-be6c-c34cf2d2c04b)(content(Whitespace\" \
         \")))))((Secondary((id \
         d501a36c-9d66-47d0-bbd4-aaf9aca618b5)(content(Whitespace\"\\n\"))))(Tile((id \
         5fff98b8-b9c0-46fa-84ee-8b03babe309f)(label(fun ->))(mold((out \
         Exp)(in_(Pat))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         37))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         0497f2fc-fad1-4eae-9ebd-deca63efca21)(content(Whitespace\" \
         \"))))(Tile((id \
         63d043da-9ba9-47ea-800b-63c5588d1edb)(label(\"(\"\")\"))(mold((out \
         Pat)(in_(Pat))(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0 1))(children(((Tile((id \
         447a1f7f-a407-4ea7-9e79-720b67bfc850)(label(ledger))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         f13469b4-54e7-44c8-851c-931c1f42b799)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 44))(sort Pat))((shape(Concave \
         44))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         6dc81b89-a197-4db0-9b2c-f860c603aeb8)(content(Whitespace\" \
         \"))))(Tile((id \
         9b764642-2894-41bf-901d-6a8d7af8cf8b)(label(h))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children()))))))))(Secondary((id \
         84bd9455-f763-4b07-98dc-c5f1a6064e4c)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         9f2137bd-0016-4d55-8a81-3a8e8f2cb3ec)(content(Whitespace\"\\n\"))))(Tile((id \
         76e5e986-9625-43c2-8148-2bddbb89d271)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         c1fd44eb-b8a8-41db-949f-c39df5a7afb3)(content(Whitespace\" \
         \"))))(Tile((id \
         ffd5954e-e0f0-441c-982c-372dd73a34de)(label(value))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         d50cc57a-d7f9-46b1-8f2e-7a595d360459)(content(Whitespace\" \
         \")))))((Secondary((id \
         8bb8651c-5afc-4c4f-ade5-82d065ab3b4c)(content(Whitespace\" \
         \"))))(Tile((id \
         4f5082ce-bbbb-44a6-91bb-ff7577a50c30)(label(harvestValue))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         08fcde6c-ce54-4a64-999a-d81195a5fa91)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         4880314b-6ef1-43af-850a-f15d9f82d6a1)(label(h))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         352d92ea-52cc-4645-bb11-ad17e873bc86)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         ce7ab7fc-8e27-43bc-9b15-bb6fcd28e1e0)(content(Whitespace\"\\n\"))))(Secondary((id \
         7b89c81f-0526-45d2-907b-e14fef322ec2)(content(Comment\"# Check if \
         this harvest continues the quality streak #\"))))(Secondary((id \
         c180b0ef-3fc3-45ca-8300-2028048abb31)(content(Whitespace\"\\n\"))))(Secondary((id \
         f88dbec0-69be-4845-9c7c-ba8d06a8bcf0)(content(Comment\"# First \
         harvest never continues a streak (no previous harvest) \
         #\"))))(Secondary((id \
         cd3105b1-9a4d-4535-9e4a-ad8de703de77)(content(Whitespace\"\\n\"))))(Secondary((id \
         afb57420-4de0-48b2-be8c-873fae3f8e72)(content(Comment\"# Compare \
         current quality with the PREVIOUS lastQuality #\"))))(Secondary((id \
         39e96128-460b-4442-b3c4-bd2b9ca70b44)(content(Whitespace\"\\n\"))))(Tile((id \
         51676401-e70a-4be0-b6b1-6e3e1e4a580f)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         9042f5ec-97b5-4d72-96f4-c3b6afe541ff)(content(Whitespace\" \
         \"))))(Tile((id \
         dcdd7e8d-a201-4a0a-b4d7-bc7749781289)(label(isFirst))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         93241dea-1f42-41b0-82f6-852643812257)(content(Whitespace\" \
         \")))))((Secondary((id \
         9d46f802-6e4c-40ba-b2aa-066f509485f2)(content(Whitespace\" \
         \"))))(Tile((id \
         d0125c50-c52a-4827-940e-ee4b0263fab0)(label(length))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         17280444-291f-4b62-a9a2-59b024242353)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         8a51d934-4cd7-4d72-9400-11c35d144b4f)(label(ledger))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         02dbd96c-aa3d-4920-905d-203a916e5a34)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         e6dab292-137e-4fa0-a8e8-cc1da16aab43)(label(harvests))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         beea654f-3e2c-42fd-bdc0-98e2172d405c)(content(Whitespace\" \
         \"))))(Tile((id \
         3cc235e4-995b-4d45-bf0e-6462fc84eb96)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         302a2fa2-02fe-4866-b915-5b5b2e92d204)(content(Whitespace\" \
         \"))))(Tile((id \
         59c9c247-2e8b-4b3d-ad16-6eff9ac0ed1b)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         cfa82a4e-2a5f-43a3-9fb4-b550e5d9e59e)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         699fd926-864c-4275-b3cc-1cb7424d176d)(content(Whitespace\"\\n\"))))(Tile((id \
         51ba2749-ee7d-40e4-8bc8-a521efc193e1)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         d8ed3ecd-a79e-4830-b841-f43980b728bf)(content(Whitespace\" \
         \"))))(Tile((id \
         5364c62e-7638-46d7-83f1-bcf18ecefc74)(label(continues))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         f64a1c54-4dec-4e92-895a-c0e3fb239525)(content(Whitespace\" \
         \")))))((Secondary((id \
         b3aaba22-2700-43f1-bb20-74e1f0b6eed9)(content(Whitespace\" \
         \"))))(Tile((id \
         fef86709-e7c3-4250-9199-8b3902713ee6)(label(!))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape(Concave 27))(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         cbb3da99-7175-45ee-afc1-806738188e7f)(label(isFirst))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         cf42becd-2f18-4045-b5f9-3602a36ef732)(content(Whitespace\" \
         \"))))(Tile((id \
         d659583f-9662-4f29-85cb-92af4fc0b606)(label(&&))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 32))(sort Exp))((shape(Concave \
         32))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         a06a77bd-3963-4aa7-af77-8bcf4588538f)(content(Whitespace\" \
         \"))))(Tile((id \
         0dfe79c2-48f5-4eb3-903e-be9b2ed69851)(label(h))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         2dc57019-4ef7-4a44-a7d6-eaa200409149)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         a16fcfe8-77eb-493f-863c-9a78fe2d1e3c)(label(quality))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         0f2c5357-0b50-48db-b081-5072f0e5fdb9)(content(Whitespace\" \
         \"))))(Tile((id \
         ed4965a2-601a-4eaf-95bf-2479833a9ec3)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         28411851-8968-458c-afbe-9c7eead83272)(content(Whitespace\" \
         \"))))(Tile((id \
         e532e190-1d1d-453b-bddd-95bf104b9393)(label(ledger))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         ce6b8cdf-cb52-4b54-9d16-c9770c2b4dd5)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         9420a33b-c972-497a-845b-04721a8b05ee)(label(lastQuality))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         39f134a7-5229-4f7d-a7a3-ceaa7b14d708)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         42c7bf34-85f7-48d3-a0f5-0cd5a6a20395)(content(Whitespace\"\\n\"))))(Secondary((id \
         12756a21-bb94-4da8-9ff9-a71217d2fb49)(content(Comment\"# Now update \
         lastQuality to current harvest #\"))))(Secondary((id \
         97fa51e3-294f-469d-9fe1-0d4b640f83f0)(content(Whitespace\"\\n\"))))(Tile((id \
         e993fe95-d50a-4e49-8686-9971ab68a476)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         c025d8c2-4446-4e82-b03b-7f925bf82fef)(content(Whitespace\" \
         \"))))(Tile((id \
         5e05b5c6-6d28-4499-9e62-dacde5d8472f)(label(newLast))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         b3964d8f-6256-404d-b3c7-d68d3c9a3e74)(content(Whitespace\" \
         \")))))((Secondary((id \
         f206b0aa-b7b0-400e-b8bc-dea66f9c247c)(content(Whitespace\" \
         \"))))(Tile((id \
         e620a1f1-63e9-4f78-b7bf-aff6804b294e)(label(h))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         7e5d4275-db3d-46b2-b2b8-12a2d522319f)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         afc6e84c-37de-40bf-90c6-722d08e5eca3)(label(quality))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         df49fe15-d116-4ac8-b221-8daa2e03b7b8)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         156fb432-98c7-429d-b42f-15e5a45d5f1b)(content(Whitespace\"\\n\"))))(Tile((id \
         f004bf0e-83d2-4a3b-a96d-1904d3895481)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         a845350a-6888-4c71-9c1e-f6cd008019f3)(content(Whitespace\" \
         \"))))(Tile((id \
         cab87e82-824e-41c2-85e3-06c816196d22)(label(newStreak))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         0bbe12c8-2e09-4dae-9d4b-d44ad0348bb9)(content(Whitespace\" \
         \")))))((Secondary((id \
         c3767920-d1d0-4de3-a625-6038e5fa01b6)(content(Whitespace\"\\n\"))))(Tile((id \
         8a47d716-e436-43d3-8185-da8a70e68622)(label(if then else))(mold((out \
         Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         36))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         2a691716-e904-412e-a36d-ff92f9e1b421)(content(Whitespace\" \
         \"))))(Tile((id \
         27344818-39f6-4e3d-911a-ec884d99b78e)(label(continues))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         65b8fbbd-616a-459e-aaac-12e26555f72c)(content(Whitespace\"\\n\")))))((Secondary((id \
         d6a916de-d5c9-4ee6-92fa-a5a79cb52eb2)(content(Whitespace\" \
         \"))))(Tile((id \
         a774df1c-f7a2-44b1-9880-4f62ae1d7ec6)(label(ledger))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         1393779a-cf0c-4358-857d-2cbe53cea9e0)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         2997b59f-b728-44e6-b55f-d05e0fd3ef4e)(label(streakBonus))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         0de51083-5cce-4264-b2f5-76a40559d88b)(content(Whitespace\" \
         \"))))(Tile((id \
         4d57cdb8-a789-4902-ad49-0de25460f02e)(label(+))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 28))(sort Exp))((shape(Concave \
         28))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         61c25cba-a293-42af-9941-1ca837d407f1)(content(Whitespace\" \
         \"))))(Tile((id \
         d31be5da-bd97-47bb-b14c-991e25d8f53a)(label(5))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         c8cdf678-7f27-4fa5-89a7-e8c94410975c)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         cf969b4b-9e18-4ce5-b3d9-e119c2535a7a)(content(Whitespace\" \
         \"))))(Tile((id \
         f1f5983f-1817-4c2a-bf21-19bf5fb7b8d8)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         fbc2a880-7724-4347-92fa-f2fdb2beeaa1)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         c62cc4d9-a185-4446-a7f0-0a516ccc414c)(content(Whitespace\"\\n\"))))(Tile((id \
         4ce1ce09-3f1e-490e-8b8f-0d3de79987f9)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         d7926e68-a0b5-4661-9dfb-b2017ffe88a4)(content(Whitespace\"\\n\"))))(Tile((id \
         e09316e0-b477-448b-ae1c-a14d7099dded)(label(harvests))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         4c1c1464-95cb-4874-b1a4-24f72ffb5aad)(content(Whitespace\" \
         \"))))(Tile((id \
         7a37ea95-ac92-456a-aebc-12fbcb0fa79a)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         5b0a7853-a2ba-4171-8321-315acf871329)(content(Whitespace\" \
         \"))))(Tile((id \
         bb4cc741-8db6-4cbe-ad4e-85a8382d02d1)(label(h))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         e6618938-2163-4c3c-b0d7-1676737fb053)(content(Whitespace\" \
         \"))))(Tile((id \
         da4e0c35-b6a6-4978-b494-83451c87ba60)(label(::))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 29))(sort Exp))((shape(Concave \
         29))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         c7ce16f1-6e46-4990-b770-57d8dba07850)(content(Whitespace\" \
         \"))))(Tile((id \
         43b8cbe3-6ec0-4a9e-940e-94aa8bf5149e)(label(ledger))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         ccc814b5-3538-4115-9492-cf4424942915)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         0d055350-e288-4e2d-b78b-20b03f8abac6)(label(harvests))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         6785e749-d69c-404a-bd91-fa0118f14654)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         b1511409-ae1d-4e85-8648-96dad0b08dfb)(content(Whitespace\"\\n\"))))(Tile((id \
         c3470fee-b6eb-4725-9a95-9da864964bde)(label(totalValue))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         446843a1-fbc6-4506-8b54-e4cb269f5fb5)(content(Whitespace\" \
         \"))))(Tile((id \
         7218fde0-507a-4192-934f-90f77309adb3)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         a9b6414a-c4fb-4b60-8d20-c4295bc4ff22)(content(Whitespace\" \
         \"))))(Tile((id \
         b95cbe4b-1e68-4bb9-8689-9e7ce1a50e36)(label(ledger))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         871e38fa-a8d1-42bd-87c8-92598a43ffec)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         1f489326-7b89-482c-b1cd-1587e194c363)(label(totalValue))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         8dcd6f07-c587-41fb-b898-1bbab475fc68)(content(Whitespace\" \
         \"))))(Tile((id \
         66ea25d5-955b-45bf-80ca-203e30dfa0aa)(label(+))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 28))(sort Exp))((shape(Concave \
         28))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         78e541f9-efd2-46db-9944-5ea7142183f2)(content(Whitespace\" \
         \"))))(Tile((id \
         481e972b-f4c8-47e8-8f82-e1970a7f0af0)(label(value))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         26b1d4f1-f807-45b2-b2fa-1462c64ed029)(content(Whitespace\" \
         \"))))(Tile((id \
         a8fe8930-74a7-48dd-a8eb-86c4cc9575f1)(label(+))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 28))(sort Exp))((shape(Concave \
         28))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         a5bf7710-098f-48d8-941e-9457900a6dda)(content(Whitespace\" \
         \"))))(Tile((id \
         c6a041e1-3d4d-4aed-b4da-7dddbe660dcf)(label(newStreak))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         618717d4-33af-4db2-b017-e31afd1cfaec)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         0385ba5e-12ab-4abb-9f99-c4559bbac133)(content(Whitespace\"\\n\"))))(Tile((id \
         bed4a5a0-0edf-49d3-a518-f019fa6e1a27)(label(streakBonus))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         a20e4a8f-b3d4-44c8-87a9-45024d663590)(content(Whitespace\" \
         \"))))(Tile((id \
         0b838f38-2c65-4c89-91e3-4f690f12b8fb)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         93adf787-ddc8-4d59-bc63-be1212c9a63c)(content(Whitespace\" \
         \"))))(Tile((id \
         13536804-057f-45e0-b14c-1dbf20290f91)(label(newStreak))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         f497af41-80a0-45ad-9ef4-dfca67081068)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         1b67b329-13c7-4088-8f7c-e72abcd8a1bb)(content(Whitespace\"\\n\"))))(Tile((id \
         cb0633af-e882-43c7-b942-e16701cb2faa)(label(lastQuality))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         1a6309e2-397a-4a40-a04b-925db449b994)(content(Whitespace\" \
         \"))))(Tile((id \
         598056dd-d061-45e6-9c67-237c9fe074bb)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         a8886b6d-ad50-4e3f-bbb7-d16b345b72f7)(content(Whitespace\" \
         \"))))(Tile((id \
         8aa177c1-5ffb-4e09-9328-bd119699a11c)(label(newLast))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         1b0fb703-0daa-4877-83be-355640289338)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         d860ac32-9499-4217-9eb8-81a11de5cb95)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         bcca35ec-e130-493e-9880-4117c26d0ca4)(content(Whitespace\"\\n\"))))(Secondary((id \
         aa214417-1df2-44dc-b70f-d39353ff40fb)(content(Whitespace\"\\n\"))))(Secondary((id \
         eba0ccc8-0acb-4218-b8a5-002f37e6350d)(content(Comment\"# Claim the \
         streak bonus and reset it #\"))))(Secondary((id \
         98addafa-6311-4224-a3ec-9fb4423d963a)(content(Whitespace\"\\n\"))))(Tile((id \
         48ba11c7-22f9-4cf8-92f4-1fd6a1bc5765)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         1cfc84cb-d448-4edf-b6b2-c54f64685c96)(content(Whitespace\" \
         \"))))(Tile((id \
         aa7f0e09-8e8a-4545-8d98-5064b927b46c)(label(claimBonus))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         45d7f530-f79f-479d-b2a5-74b710fc6ab7)(content(Whitespace\" \
         \"))))(Tile((id \
         18dbaf88-8290-43a8-8765-d146501f5b38)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 24))(sort Pat))((shape(Concave \
         24))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         35fd9d8e-5a3d-40b4-988b-1ab4d7444d1a)(content(Whitespace\" \
         \"))))(Tile((id \
         4b20fabc-0f16-4e6d-a3f4-4da76c83d1b6)(label(Model))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         3db0e250-9b23-4b26-82f7-4f731b727d50)(content(Whitespace\" \
         \"))))(Tile((id \
         3739223a-d8fe-439d-9259-c2d1b3d45cd0)(label(->))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 13))(sort Typ))((shape(Concave \
         13))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         1e337467-1757-489b-857f-62e2d2883abd)(content(Whitespace\" \
         \"))))(Tile((id \
         41fb4d83-e74e-42d2-902a-6badeb2381c7)(label(Model))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         f57bcbe3-c457-4770-a079-5ec55b65ebdd)(content(Whitespace\" \
         \")))))((Secondary((id \
         a886c33a-8db1-43c7-aa8e-3759b6a4f990)(content(Whitespace\"\\n\"))))(Tile((id \
         d08e1c75-d98b-42db-9a37-e03d2dee72bb)(label(fun ->))(mold((out \
         Exp)(in_(Pat))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         37))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         6cda1bec-768a-4154-a8b1-ea65e2745d90)(content(Whitespace\" \
         \"))))(Tile((id \
         ff52f422-1a8a-4bc5-9807-ee7f3c6ffb74)(label(ledger))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         73d67c09-03d3-4ee3-8327-8fc401f159dc)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         9d9dd582-7658-4bae-bf72-32aa5c9eeb3e)(content(Whitespace\"\\n\"))))(Tile((id \
         8c079633-91c3-47cb-84ac-2e1acd3f45a7)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         d04b8d00-235b-4316-8934-6ea65d14000e)(content(Whitespace\"\\n\"))))(Tile((id \
         2394e33a-5502-475a-991a-cc8d0b9d038b)(label(harvests))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         7d333807-4b32-48fd-b6a7-7a557a9dd157)(content(Whitespace\" \
         \"))))(Tile((id \
         710318a0-264a-4419-a0ff-c2281aa48100)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         338420d1-3d9a-4ae8-9890-99388c8b219d)(content(Whitespace\" \
         \"))))(Tile((id \
         a9dd0b51-5232-4b33-aff5-9497a5b54a29)(label(ledger))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         817427d6-5b9b-4bd7-904a-9a562567a092)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         58f4f7cd-4097-49eb-946e-b63c8e8e0c48)(label(harvests))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         5e7fb598-9b16-46b6-a3cd-f6a9e098d0a3)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         2cc84291-3c74-4388-8fef-f14a76aaaa99)(content(Whitespace\"\\n\"))))(Tile((id \
         0f433204-af9a-480d-934a-b89c44e3430e)(label(totalValue))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         9894dce8-01a0-4b6c-b135-89b30565f78d)(content(Whitespace\" \
         \"))))(Tile((id \
         8cd738be-825b-4be7-bffd-aeae0d8cac59)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         54110395-3522-46e0-95f7-1c22e3b74970)(content(Whitespace\" \
         \"))))(Tile((id \
         16699bc7-7ae4-43ba-ac08-2c0deb5130da)(label(ledger))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         7f370de5-9a21-4650-a2e2-65ca5633474a)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         1b1df995-9fab-4648-89a3-491de35fd2bc)(label(totalValue))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         c5d5c9c0-794e-41cc-8c1d-b481218c95e6)(content(Whitespace\" \
         \"))))(Tile((id \
         cd3d8b26-cc52-4885-b38d-751d37103578)(label(+))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 28))(sort Exp))((shape(Concave \
         28))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         347dad77-2318-4ab6-80d1-165a415568a1)(content(Whitespace\" \
         \"))))(Tile((id \
         cc3ed24a-1b9b-4a60-b323-700f75c85882)(label(ledger))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         8e5fdd4e-8299-4cf0-b06d-4c85e4c3e500)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         46c44a38-5840-4e11-aad3-8e20c429cb87)(label(streakBonus))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         e7cd92a9-0c03-45aa-b98f-fe09856276e5)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         efaa01ec-5f89-4431-8e3a-00bb9fe793b7)(content(Whitespace\"\\n\"))))(Tile((id \
         ff8a1e54-ab19-4363-bd06-b39f06861e5c)(label(streakBonus))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         b1af1fab-f7bc-4ebf-a115-df1241bec329)(content(Whitespace\" \
         \"))))(Tile((id \
         73de0ec4-5333-433f-a6f2-4c5941b50d0f)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         10b5dbe0-3dba-4e6d-9c72-63d6cc9a85c8)(content(Whitespace\" \
         \"))))(Tile((id \
         a83923a6-116d-4e40-b5e7-ca864df181ab)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         b2332693-743f-4801-ac73-e4a907c0dc39)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         edd017fd-5682-4c27-abcf-e11884b77b12)(content(Whitespace\"\\n\"))))(Tile((id \
         583ae1b7-0e38-4384-b6e6-db325446415f)(label(lastQuality))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         71c1561c-7c45-4c6b-b93e-5d32b94ead5e)(content(Whitespace\" \
         \"))))(Tile((id \
         1d9e8b5a-951f-4fd2-b566-a5f32e48113f)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         74f9e48b-56ea-4f02-8cce-5cdcbab8b27f)(content(Whitespace\" \
         \"))))(Tile((id \
         b1537cab-8a20-49b4-b9fc-acfc7cbed4ff)(label(ledger))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         e7f7a572-ee1b-4abc-8c3c-cd434089a5db)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         3caa7259-bbf4-40ce-a5c2-9b0f71fbb68c)(label(lastQuality))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         be57d2f0-0ef1-4d19-ab90-f6192410dacf)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         9dbdd626-6880-4591-9b83-c5dc2d9bc579)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         be800600-3b78-4069-a6a0-6aa1c9c6b1ad)(content(Whitespace\"\\n\"))))(Secondary((id \
         84ea3a02-16de-4ecb-900c-201a1730580f)(content(Whitespace\"\\n\"))))(Secondary((id \
         74a2de5b-5c4c-4021-86f5-34223acd7414)(content(Comment\"# Close the \
         harvest day - reset streak tracking #\"))))(Secondary((id \
         8799024a-416d-400a-9ff0-31ec43e0c851)(content(Whitespace\"\\n\"))))(Tile((id \
         f8fb1dc5-7c8a-4064-8d5e-08e3d3cfac58)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         f911a391-cb96-4c9c-948f-451facff8551)(content(Whitespace\" \
         \"))))(Tile((id \
         7a46f84c-ddf0-4a32-8cc6-cd0b885c474a)(label(closeDay))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         565e7980-0d7f-4cad-a472-068670dd3097)(content(Whitespace\" \
         \"))))(Tile((id \
         df7a1314-4d7c-4762-bab9-da73b6a6a0a3)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 24))(sort Pat))((shape(Concave \
         24))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         eda1f8d3-36df-4dca-8c0b-ccb5473805a2)(content(Whitespace\" \
         \"))))(Tile((id \
         f4468732-20f8-4ad9-8506-3756659c5d5b)(label(Model))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         31d1fa5c-f28a-48f4-a212-c48403e80baf)(content(Whitespace\" \
         \"))))(Tile((id \
         411aa080-9c02-495d-aac5-bf974a7cda7c)(label(->))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 13))(sort Typ))((shape(Concave \
         13))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         6163b2a7-e191-441d-a0a8-342e2d67e864)(content(Whitespace\" \
         \"))))(Tile((id \
         d8de1958-ae75-4d4d-aa1e-759f7aeac630)(label(Model))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         c071073a-8fb6-4892-9e20-a85149a8f67c)(content(Whitespace\" \
         \")))))((Secondary((id \
         8bf26374-62f0-4b23-9131-8bd0866ea2a0)(content(Whitespace\"\\n\"))))(Tile((id \
         fe587abc-f465-4a47-aad7-09a06990f58c)(label(fun ->))(mold((out \
         Exp)(in_(Pat))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         37))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         629e9163-9476-4ff1-a6a3-27aae5b0f3eb)(content(Whitespace\" \
         \"))))(Tile((id \
         f602a82b-aa85-4a87-8374-2897efb2972a)(label(ledger))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         d9dbe2b1-05af-41cd-82e8-934bf52ec868)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         14c66cbc-7e31-4933-86f0-a6600d7c49c7)(content(Whitespace\"\\n\"))))(Tile((id \
         bcd22959-37f9-4cd1-9610-e3881e7b1022)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         306b2680-676b-41f8-ae69-2f132c10615c)(content(Whitespace\"\\n\"))))(Tile((id \
         e6059d29-73ad-49bd-a9bf-97bc632a4a6f)(label(harvests))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         d8df629b-c703-4a21-bdb9-8f2601d6ee2f)(content(Whitespace\" \
         \"))))(Tile((id \
         291c9ce4-2742-4641-8adb-c9832c3655ee)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         536a695c-4ddb-4829-b532-70ac0c2d90a6)(content(Whitespace\" \
         \"))))(Tile((id \
         df390d6c-f927-462f-a959-98f2a9319cb8)(label(ledger))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         f08d7a59-4e7b-43ab-b992-eb8c153d38e4)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         d4d5f33e-fd13-4aff-a8bd-006740f86ad8)(label(harvests))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         d7b5d74a-a9e0-4111-9f71-e1cddaea2943)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         2c635e86-e976-4b35-8983-b8e10cebce53)(content(Whitespace\"\\n\"))))(Tile((id \
         de1f0eb0-c91d-4243-8904-0f5738b274a1)(label(totalValue))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         690077fb-7101-4241-98dd-196d2a727ac8)(content(Whitespace\" \
         \"))))(Tile((id \
         3805d665-ee24-4cb0-9907-a4ddea8e9953)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         f8ad9da5-a71e-4710-8734-dbc03ede8f96)(content(Whitespace\" \
         \"))))(Tile((id \
         2e049bd6-f297-4241-aea4-98046a9e49bf)(label(ledger))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         8a7286b1-8b27-495d-81c3-54f44f51a80d)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         d0c23bb8-90a9-472b-9a7b-223288dd157d)(label(totalValue))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         4b3ebd56-33c6-4d5a-bf27-02bc0f7ef09e)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         98bc5b23-483b-45d6-8b58-d57ee9a5644b)(content(Whitespace\"\\n\"))))(Tile((id \
         3522beb5-8a4c-44a6-bc8a-37e4ec6bc70a)(label(streakBonus))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         25c591fd-f1dd-4cc0-a7be-a3c12d169ff0)(content(Whitespace\" \
         \"))))(Tile((id \
         3b71e4eb-af87-4685-9c2d-e341687ed253)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         5592a91e-95f7-4e60-9aa0-0e978ea663bd)(content(Whitespace\" \
         \"))))(Tile((id \
         c6dd388e-736c-451c-90ac-1224535a27d6)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         5c18134d-cc91-4097-b956-e9aefd3715ea)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         b1390da1-aaf4-4f6a-9ee1-0762aad836bb)(content(Whitespace\"\\n\"))))(Tile((id \
         acf3ee4f-94b7-481b-97da-3094d565d7e4)(label(lastQuality))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         6dc9efea-1ebd-4c4d-9831-51e5e062e486)(content(Whitespace\" \
         \"))))(Tile((id \
         6481ec25-90a4-4483-9928-034fe27608b4)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         a016f463-95db-46fe-b655-4d35bed02661)(content(Whitespace\" \
         \"))))(Tile((id \
         66231c5b-834b-4843-9e45-cb7ffe550edb)(label(Bronze))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         d0e53b52-0789-4cb2-8f5f-d1f6d37af5d8)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         e2024060-9895-4dcc-8110-0a5230677c51)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         00cafadd-066e-4057-8902-26c48c7935e5)(content(Whitespace\"\\n\"))))(Secondary((id \
         813b0202-7e59-4c2b-8be9-333d3cba27e9)(content(Whitespace\"\\n\"))))(Secondary((id \
         b2d86059-bed6-4918-9cdd-b1a921a04f00)(content(Comment\"# TODO: Add \
         premiumMultiplier helper here                      \
         #\"))))(Secondary((id \
         2604fc99-6bcb-4056-829e-6f9d3e45745c)(content(Whitespace\"\\n\"))))(Secondary((id \
         56378e0c-ca7b-4f27-948d-1d691445683c)(content(Comment\"# It takes a \
         streakBonus (Int) and returns the multiplier:     \
         #\"))))(Secondary((id \
         69c98d80-da61-47dd-94b9-4c8a705d990e)(content(Whitespace\"\\n\"))))(Secondary((id \
         a72cad92-9763-4e1a-9cef-053b9bb806be)(content(Comment\"#   - Return 2 \
         if the streak bonus is >= 10 (strong streak)    #\"))))(Secondary((id \
         8779ddf6-0c89-41f7-9890-b1413cdf78df)(content(Whitespace\"\\n\"))))(Secondary((id \
         52242110-9513-407f-a342-6f28e358bc9a)(content(Comment\"#   - Return 1 \
         otherwise                                      #\"))))(Secondary((id \
         7526e926-18dc-4329-8f7a-aa2c00138c14)(content(Whitespace\"\\n\"))))(Secondary((id \
         f85b2830-7d87-4af8-83f8-623001ee32a8)(content(Comment\"# Hint: This \
         is a simple if/then/else on the streakBonus.     \
         #\"))))(Secondary((id \
         5545dcb5-d98a-4858-8b3e-680fa28fffc8)(content(Whitespace\"\\n\"))))(Secondary((id \
         aafe6f36-b80b-470b-99af-1d532385225e)(content(Whitespace\"\\n\"))))(Tile((id \
         1b3ea9ca-7ef7-4c29-9c21-851fa5dd102f)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         37171ef4-d28a-4cfd-9dce-2466fb768e8a)(content(Whitespace\" \
         \"))))(Tile((id \
         e5dd2a5b-ab1e-4b53-a0ed-31d9e06cfc20)(label(premiumMultiplier))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         da13d08b-ed00-439e-a021-1b357fca2dd7)(content(Whitespace\" \
         \"))))(Tile((id \
         60bad74c-bc90-434b-babf-79ec0dc18902)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 24))(sort Pat))((shape(Concave \
         24))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         6228950a-56d6-4d6c-acad-ace1e5cf544a)(content(Whitespace\" \
         \"))))(Tile((id \
         bf0cd860-4a29-4731-a06b-ce03e176b413)(label(Int))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         31b3e951-031a-4ae3-bb02-8a1a730c2ec3)(content(Whitespace\" \
         \"))))(Tile((id \
         4dc8eb9f-a176-423e-891e-e87ca1eb5d21)(label(->))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 13))(sort Typ))((shape(Concave \
         13))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         2197fca2-9222-40ef-bf8d-38db6537b722)(content(Whitespace\" \
         \"))))(Tile((id \
         93f0949d-8ba9-4f30-b00f-8eeec54f6c41)(label(Int))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         b7d7182a-9cc4-4bf8-8ad0-f53500cf6710)(content(Whitespace\" \
         \")))))((Secondary((id \
         f68db676-4a22-4e19-8ba1-99d2467d1757)(content(Whitespace\"\\n\"))))(Tile((id \
         286aced1-9680-48f9-b8bc-050b03d82856)(label(fun ->))(mold((out \
         Exp)(in_(Pat))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         37))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         80a7262d-5b13-402a-ac8e-a0ea3865a57d)(content(Whitespace\" \
         \"))))(Tile((id \
         9fd1a36b-d16e-42a7-9985-c8dcc61afde1)(label(streakBonus))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         698be174-a520-4c15-aef3-0e76c9bd13bd)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         44d830c9-7f19-4004-85b5-e750f55546ca)(content(Whitespace\"\\n\"))))(Tile((id \
         34910791-23d9-4e1b-8a5b-467bba1d9786)(label(?))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         007fe96e-4734-44a6-ae40-e185e53981e5)(content(Whitespace\"\\n\"))))(Secondary((id \
         1e3fd8d3-6660-4b85-a784-1277da4cd208)(content(Whitespace\"\\n\"))))(Secondary((id \
         e52d8f48-630c-4a03-acf0-6de0be4b4e8c)(content(Whitespace\"\\n\"))))(Secondary((id \
         50a762fd-4000-4e25-a179-6c92d7d019ef)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         f820e010-27dc-4e85-bc68-48f3844e724c)(content(Whitespace\"\\n\"))))(Secondary((id \
         dceac99b-f4ae-4bf5-bd35-fd507076c037)(content(Whitespace\"\\n\"))))(Secondary((id \
         67174e87-39e4-4fcd-8c01-a3c226d35ae0)(content(Comment\"# Main update \
         function - dispatch actions #\"))))(Secondary((id \
         0a4d47e6-77ca-42ed-8ae4-e9d27fb8aa5a)(content(Whitespace\"\\n\"))))(Tile((id \
         2b5fe1d8-c121-4cba-b85f-7164f298cffb)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         9d09728a-56e1-4d79-968f-bd8b3a18bcbf)(content(Whitespace\" \
         \"))))(Tile((id \
         f62f53c6-1b19-47d6-b207-dcbeb3e88c16)(label(update))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         10a243cd-865d-44de-8231-10727f4e47d0)(content(Whitespace\" \
         \"))))(Tile((id \
         f7ca3e98-05ee-448b-9321-cf266e93b617)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 24))(sort Pat))((shape(Concave \
         24))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         91fbfa4f-03bf-4344-b1e2-8325651dc7b9)(content(Whitespace\" \
         \"))))(Tile((id \
         44ce328e-2cdc-43d5-992f-98b9d3b7eb3f)(label(\"(\"\")\"))(mold((out \
         Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0 1))(children(((Tile((id \
         12f390ef-739f-4c54-b5b6-d7a1fdb20f2a)(label(Model))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         c40bbc35-c486-4d6d-802d-fcdae3b549cf)(label(,))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 44))(sort Typ))((shape(Concave \
         44))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         8da7ebbe-6d0f-4317-9dc8-768fa5747583)(content(Whitespace\" \
         \"))))(Tile((id \
         f072c280-dd70-4ca7-917e-f5725cf16b25)(label(Action))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children()))))))))(Secondary((id \
         d208e685-a7f5-4894-a208-a05913a94ef2)(content(Whitespace\" \
         \"))))(Tile((id \
         96a98fc1-c0e4-491a-b7ec-5a08b0e1ee06)(label(->))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 13))(sort Typ))((shape(Concave \
         13))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         26cc5faa-8e77-4a09-a8ab-94aa6fdf3520)(content(Whitespace\" \
         \"))))(Tile((id \
         e4177f0b-4823-4bba-a3cd-a4d2adcb4e27)(label(Model))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         a7849462-29c9-407d-b309-ec5f10fefbe4)(content(Whitespace\" \
         \")))))((Secondary((id \
         42da6144-353f-4305-a7bd-851d2d5ca0b6)(content(Whitespace\"\\n\"))))(Tile((id \
         875ab554-4b52-44d0-bfe9-2076b88ac7b7)(label(fun ->))(mold((out \
         Exp)(in_(Pat))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         37))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         c80735bc-7beb-4844-a7a1-802b09e3a3c7)(content(Whitespace\" \
         \"))))(Tile((id \
         506ab5dd-a428-4f41-9b0b-d04c463a0166)(label(\"(\"\")\"))(mold((out \
         Pat)(in_(Pat))(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0 1))(children(((Tile((id \
         e4b71de8-2d89-4162-b823-fecd098eacc5)(label(ledger))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         90d768d8-b0df-4c4f-9929-001cfa5a7e1e)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 44))(sort Pat))((shape(Concave \
         44))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         a1d9d9bf-40fd-485d-9da1-e2a69dacb1ef)(content(Whitespace\" \
         \"))))(Tile((id \
         1103aba9-62f7-422d-8b22-c74d8cbaf6f8)(label(action))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children()))))))))(Secondary((id \
         68aa0543-f3d4-42db-bc3a-e614fb203a31)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         ccae5058-7d53-42bc-a332-12e8c164e83e)(content(Whitespace\"\\n\"))))(Tile((id \
         64a9466f-4060-4115-85de-e5a7fd495762)(label(case end))(mold((out \
         Exp)(in_(Rul))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         2556f3f9-c273-4207-a183-f6ef780523f2)(content(Whitespace\" \
         \"))))(Tile((id \
         c0a268dc-a62b-4a6c-9873-af5529d4232a)(label(action))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         1c6c0c8d-4a23-46a0-b504-53fbc830a657)(content(Whitespace\"\\n\"))))(Tile((id \
         7978471e-f6ff-4e6d-aed7-842f0957d862)(label(| =>))(mold((out \
         Rul)(in_(Pat))(nibs(((shape(Concave 46))(sort Exp))((shape(Concave \
         46))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         e5ac26ba-bbde-4964-8496-d561fc45105f)(content(Whitespace\" \
         \"))))(Tile((id \
         3ff6fe9c-eed1-4c6e-a7b4-7fdc7a485017)(label(RecordHarvest))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         d5ff6cec-0b77-4bf5-a039-04c4c924eb70)(label(\"(\"\")\"))(mold((out \
         Pat)(in_(Pat))(nibs(((shape(Concave 23))(sort Pat))((shape \
         Convex)(sort Pat))))))(shards(0 1))(children(((Tile((id \
         ff5a7c93-22dd-4c1f-afb6-080988abc6f9)(label(h))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children()))))))))(Secondary((id \
         ecd7c578-7753-4a57-95b5-c790a57c5e71)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         15128ab0-433b-4170-acd0-3f13286d343b)(content(Whitespace\" \
         \"))))(Tile((id \
         aa41569b-59a9-4e05-a938-d87958b752df)(label(processHarvest))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         3d0b2f4a-a7f7-4277-9bbc-a63da35342f9)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         57b2dff4-8afb-4b88-b3d0-d8fee386cf9a)(label(ledger))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         b1456e75-999c-4716-ba02-ebc6c5210706)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         b7587915-0512-42dd-bb8c-a0e3c79add3f)(content(Whitespace\" \
         \"))))(Tile((id \
         1c0ee1e6-24d2-4697-9f55-3b57a7f34d41)(label(h))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         908e26b8-67ce-4e05-8b1b-61b7ca66c75e)(content(Whitespace\"\\n\"))))(Tile((id \
         51f0b8f3-59e6-4580-82f6-7009675b1dac)(label(| =>))(mold((out \
         Rul)(in_(Pat))(nibs(((shape(Concave 46))(sort Exp))((shape(Concave \
         46))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         c285f3bf-63bb-44ee-a784-4351c7e8b102)(content(Whitespace\" \
         \"))))(Tile((id \
         480fa9a0-a18d-4624-9a86-2ff837c4e575)(label(ClaimBonus))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         17303e3f-e8a9-421d-9627-0c5f65e91311)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         ba9990a1-cb68-4006-aa0f-5d832a96b8d9)(content(Whitespace\" \
         \"))))(Tile((id \
         8af8a227-c782-47e6-8e9a-1779ef3cf061)(label(claimBonus))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         b6bf2cc3-0a8f-4862-8556-ef7b0dc30d6e)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         b71758c4-4bfb-4526-8942-69a702a240df)(label(ledger))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         58706509-64e1-4b40-89a4-a4bbe53cd742)(content(Whitespace\"\\n\"))))(Tile((id \
         ce49d9d9-ba83-4d3b-877a-c30ed95ac5ca)(label(| =>))(mold((out \
         Rul)(in_(Pat))(nibs(((shape(Concave 46))(sort Exp))((shape(Concave \
         46))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         3de39fe6-3dce-4a88-8f9c-607cd9d4a3e8)(content(Whitespace\" \
         \"))))(Tile((id \
         8f6ccb48-87c5-428e-a0af-28eba5af47a7)(label(CloseDay))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         11d96cca-c75f-4317-9958-d94d97ddefd3)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         fde98343-f4cc-4e35-98fa-c1dcb96f52f5)(content(Whitespace\" \
         \"))))(Tile((id \
         21597bf4-0b5e-403d-a29b-c7cd7b89c07b)(label(closeDay))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         242a5ac7-e7ed-4417-adec-8c4f37abd34b)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         b140e0cf-3f08-4aac-96ea-c3d096d0f80b)(label(ledger))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         2d74b2d0-e604-4b18-b1cd-cc7e0521b6eb)(content(Whitespace\"\\n\"))))(Secondary((id \
         16185a32-fe27-4ccf-a18b-9c625fcad2ae)(content(Comment\"# TODO: Add \
         PremiumSale case here                          #\"))))(Secondary((id \
         411991a9-3a27-4b6e-aa05-463baecef49a)(content(Whitespace\"\\n\"))))(Secondary((id \
         bd028108-5a6b-414a-ad60-3fd6075a7cd0)(content(Comment\"# Hint: \
         Compute payout = streakBonus * premiumMultiplier,  \
         #\"))))(Secondary((id \
         696f95ed-9f7f-4efc-a56b-96a3f11f2679)(content(Whitespace\"\\n\"))))(Secondary((id \
         05ab1c64-0b6d-4169-a22d-18cbf4431ee1)(content(Comment\"# add payout \
         to totalValue, and reset streakBonus to 0.    #\"))))(Secondary((id \
         f047b480-f22d-46a4-9639-7fb2714a5cd3)(content(Whitespace\"\\n\"))))(Secondary((id \
         bf45a078-457b-4573-aad8-fec68e570802)(content(Comment\"# Keep \
         harvests and lastQuality unchanged.                 \
         #\"))))(Secondary((id \
         4ea0389e-22d1-4a89-b75e-334d98d970ee)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         006216c0-2295-420d-aacd-442d7803e08e)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         f68ceb97-ec07-431a-bf05-266c8c41dfee)(content(Whitespace\"\\n\"))))(Secondary((id \
         ff2bcb65-b552-424e-9549-5f9e4b8d2ee3)(content(Whitespace\"\\n\"))))(Secondary((id \
         33edfa77-dcae-4ba7-be15-d4a76f61ac3b)(content(Comment\"# Run multiple \
         actions in sequence #\"))))(Secondary((id \
         f6c004d0-1837-4a45-8ef2-ba68dc16ab72)(content(Whitespace\"\\n\"))))(Tile((id \
         ce2f7334-a018-4b89-801c-cf0960447161)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         dbceef75-c137-4dc4-877c-202e29aeac02)(content(Whitespace\" \
         \"))))(Tile((id \
         214d58e4-4d1b-4da0-9780-f851e0a0ba5d)(label(run))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         bc54aaf7-9b07-4a5f-bf52-07dd505a4f2e)(content(Whitespace\" \
         \"))))(Tile((id \
         00852f80-801d-417d-b00c-097020508111)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 24))(sort Pat))((shape(Concave \
         24))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         c4129091-a285-4ef2-9175-5c7d925e7f04)(content(Whitespace\" \
         \"))))(Tile((id \
         d46db427-0f58-4ee0-b86c-4659b90c3519)(label(\"(\"\")\"))(mold((out \
         Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0 1))(children(((Tile((id \
         7c5d4fe5-ca6c-4990-9627-4b4dea3f0a0a)(label(Model))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         c3d6271b-0303-4d7e-997a-22ea90bb2cd4)(label(,))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 44))(sort Typ))((shape(Concave \
         44))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         2e7775bd-c7eb-4c07-b98c-8ba74ad37505)(content(Whitespace\" \
         \"))))(Tile((id 10656735-d461-4a11-b523-69d1b6915aad)(label([ \
         ]))(mold((out Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape \
         Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
         14ab30e9-d313-4498-973a-30d681000aef)(label(Action))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))))))))))))(Secondary((id \
         7221be3d-b6ed-4453-a5ac-cd3d0b7122e1)(content(Whitespace\" \
         \"))))(Tile((id \
         bb42bea7-d50e-45e3-bd5c-a7e1171e2696)(label(->))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 13))(sort Typ))((shape(Concave \
         13))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         c195074d-97a1-4b3d-83b8-af7bd7c13d02)(content(Whitespace\" \
         \"))))(Tile((id \
         a677ee28-beda-41f5-b284-b50379c8a5f4)(label(Model))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         747e5df5-ee5c-4d07-9722-338094cb6885)(content(Whitespace\" \
         \")))))((Secondary((id \
         5e9ba3fb-f05f-4900-9331-cbe57381da0d)(content(Whitespace\"\\n\"))))(Tile((id \
         281bb95e-f392-482f-b6b0-ef10bda566fd)(label(fun ->))(mold((out \
         Exp)(in_(Pat))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         37))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         0d7e65e2-b7b1-4c20-a1bc-72cd934807c4)(content(Whitespace\" \
         \"))))(Tile((id \
         f5cbbe24-fc18-43eb-9515-b1782bb39518)(label(\"(\"\")\"))(mold((out \
         Pat)(in_(Pat))(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0 1))(children(((Tile((id \
         de3a0926-d632-4acc-ad3c-fe3b8dc41683)(label(ledger))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         05173798-3694-470f-ba03-56162296e96d)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 44))(sort Pat))((shape(Concave \
         44))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         7df0de91-a1b8-4f22-b3fb-bdced9a3d832)(content(Whitespace\" \
         \"))))(Tile((id \
         900d8d4a-ff32-4c7b-86d0-ce661096ad12)(label(actions))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children()))))))))(Secondary((id \
         32b6f815-ad43-46ce-a395-caac3060892b)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         ca2d61ae-90e7-43af-a295-0fa9b40cf471)(content(Whitespace\"\\n\"))))(Tile((id \
         43fe0f8d-2974-4701-83e7-b7b98934fe36)(label(fold_left))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         88b39864-a4c4-4597-810a-76836a9ac33c)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         1008ddb4-2ba4-41f5-ae78-9af7df7cccf0)(label(actions))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         6bfc0f3b-8eb3-44ff-ba18-fb45604e55a1)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         fda886b9-61a2-4547-bef6-cda5232a79be)(content(Whitespace\" \
         \"))))(Tile((id \
         d71a8e9c-8142-4ef3-bda7-83d650f6d327)(label(update))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         34a94ed2-0443-46c3-8ab5-1f3baac46385)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         9a665503-e116-4286-b556-edb59f0357af)(content(Whitespace\" \
         \"))))(Tile((id \
         c61e1a30-fda1-4a39-a693-fa955eb99f6e)(label(ledger))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         ac2e0a0d-4fff-488b-9aa8-f6365f3bf276)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         42177690-1ab9-468f-b28b-af8c6ce6cba0)(content(Whitespace\"\\n\"))))(Secondary((id \
         72725f44-2f8f-4cfa-adb2-8a9b5cbe7ea6)(content(Whitespace\"\\n\"))))(Secondary((id \
         617c2689-dbde-4d12-b544-bbd86d09647c)(content(Comment\"# ===== TESTS \
         ===== #\"))))(Secondary((id \
         92182310-d644-4799-ad83-fe95ee5bbfa1)(content(Whitespace\"\\n\"))))(Secondary((id \
         ef3aa53b-a887-4c09-b0cb-de32411fd6de)(content(Whitespace\"\\n\"))))(Secondary((id \
         99c0976a-8cd9-4f38-8b83-ea9474309fad)(content(Comment\"# Regression: \
         basic harvest recording still works #\"))))(Secondary((id \
         e4d8af53-9f38-42e1-8540-1594bf96ad91)(content(Whitespace\"\\n\"))))(Tile((id \
         d86930cc-228b-44bb-826b-46dbc2329767)(label(hint test end))(mold((out \
         Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         d5a38011-8859-4b06-82a6-78ae0d9dfa1a)(content(Whitespace\" \
         \"))))(Tile((id \
         3a462c93-d21d-438a-a124-38e2915b4602)(label(\"\\\"recording harvest \
         adds to total value\\\"\"))(mold((out Exp)(in_())(nibs(((shape \
         Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         91f9da33-12aa-4d33-96ef-ff965604c916)(content(Whitespace\"\\n\")))))((Secondary((id \
         f910bf41-91bd-43e4-86ef-25c82840d47f)(content(Whitespace\"\\n\"))))(Tile((id \
         f846ca1d-448b-4e96-a9a3-c4bc5048da59)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         b8aeb328-8ea3-4ee0-862e-156914955b43)(content(Whitespace\" \
         \"))))(Tile((id \
         586ae9ae-36c9-4de0-9806-e65737ee9dc6)(label(h))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         651ecd35-4dfe-431c-8aed-20d16999f915)(content(Whitespace\" \
         \")))))((Secondary((id \
         f50b03ec-d7cd-446e-b687-22cc0959d433)(content(Whitespace\" \
         \"))))(Tile((id \
         7e1801cd-d4d5-4d5f-8c67-4796a54e9ff5)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         ac8dbb22-fb2f-4e01-852e-3627a98b30bb)(label(crop))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         3e243e7e-53a8-445b-9a8b-f2b2868a4990)(content(Whitespace\" \
         \"))))(Tile((id \
         6f477394-6381-4204-ba07-873156bf0796)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         dff85980-6037-4a13-9a54-a20a40077cf9)(content(Whitespace\" \
         \"))))(Tile((id \
         d6d91275-5956-43a1-83ab-6df0e5fae20e)(label(Moonmelon))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         c9063faf-307f-4723-8f5f-b8cc8fd7831b)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         199bd228-4f1e-47fd-bf2a-814421f24e77)(content(Whitespace\" \
         \"))))(Tile((id \
         66896407-8a04-44df-b138-56062a262c88)(label(quality))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         d8ca982e-c42c-4206-88fe-5bd107b625f3)(content(Whitespace\" \
         \"))))(Tile((id \
         1929dbcf-73b4-4936-8b7f-f800cdcb2bfa)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         6450d62c-f998-472a-bff6-bdcfe22a07b9)(content(Whitespace\" \
         \"))))(Tile((id \
         fd3eeaa2-f598-40c6-8f5d-57fdf8df4d24)(label(Bronze))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         cb17e98a-1b34-4f86-91f8-7dd1cc74b43a)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         d5e981f7-9cb8-4483-a126-382c4a8683e8)(content(Whitespace\" \
         \"))))(Tile((id \
         77aba276-3ec5-4d6f-9106-52a352d7831c)(label(quantity))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         6aa8d0f7-0fb3-44f4-9732-7b16ff866062)(content(Whitespace\" \
         \"))))(Tile((id \
         830abcd5-aac8-41cb-8c63-a59d81be44e0)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         ac3a365f-42c8-4889-b499-3364ae360ef0)(content(Whitespace\" \
         \"))))(Tile((id \
         8eaa361c-3c04-4aa4-8b9e-dec9f95cae6d)(label(2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         cf8efba4-7d41-4e30-9807-fcd47df381ac)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         96bb0636-833d-4c5f-b6c1-1d7336a47b46)(content(Whitespace\"\\n\"))))(Tile((id \
         a1b0aa73-f0fb-4d64-9884-89919f925210)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         e8582002-b1af-41a1-9105-4de49bcb2d1a)(content(Whitespace\" \
         \"))))(Tile((id \
         3ae45219-1bb8-4c24-8c17-a2e62a93f893)(label(ledger))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         375575d1-dae1-48c4-a4bf-c2a3424a4471)(content(Whitespace\" \
         \")))))((Secondary((id \
         1bd0591f-f08a-414d-bc29-c9a1492869e0)(content(Whitespace\" \
         \"))))(Tile((id \
         ee77a796-429a-42f8-9a27-4479e18d7c8d)(label(update))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         9d7cdbf6-d0d2-4514-a7ab-9e7c3aa55e3c)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         d1b07c20-4956-4d7a-90fc-e2069e0108ea)(label(initModel))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         41e91924-8628-4be6-9cc9-4591fdf503b7)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         93c1bbc8-37cb-48a5-9ab0-67f031373bbd)(content(Whitespace\" \
         \"))))(Tile((id \
         23116810-c3c3-4f90-b7c6-c6809e1d49bd)(label(RecordHarvest))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         425c9f64-f1ff-4be1-b280-2abcad74976e)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         6d6e31c0-114f-4881-a0de-edc7d89ea7e5)(label(h))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))))))))))))(Secondary((id \
         67843c07-89b7-403f-b1a8-217c861714a8)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         ee2c23b2-4bad-45c4-9340-2df880b34b4d)(content(Whitespace\"\\n\"))))(Secondary((id \
         f04f8159-9a9c-4e35-9f2b-517f384f4a14)(content(Comment\"# \
         Moonmelon(15) * Bronze(1) * 2 = 30, no streak bonus on first \
         #\"))))(Secondary((id \
         d89b7c15-d280-4a4d-9245-b2071fa4e046)(content(Whitespace\"\\n\"))))(Tile((id \
         06574443-23d2-4360-ac58-e85be8125b35)(label(ledger))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         80f8b775-7bd5-4d1b-9f3f-0542049f22e0)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         eb7a1bb9-0098-4953-81d7-bcd404c76f4f)(label(totalValue))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         e6b555ab-a489-4f4b-8fea-4098c4c1f8c4)(content(Whitespace\" \
         \"))))(Tile((id \
         7d0ef471-a17e-4a0a-91d3-2b37ec086513)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         47acdd46-e35f-4530-984b-326cb2429ff6)(content(Whitespace\" \
         \"))))(Tile((id \
         82adf366-70d9-4557-816e-62d4e1a91aa8)(label(30))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         9f975e41-17e2-4afc-bb54-d21933397aea)(content(Whitespace\"\\n\")))))))))(Tile((id \
         eaf3390a-d760-4abe-b16d-fb237b460b61)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         45157116-667e-4409-862f-19685b2aca45)(content(Whitespace\"\\n\"))))(Secondary((id \
         eb5e2b07-2092-4f33-b400-41926a20a48c)(content(Whitespace\"\\n\"))))(Secondary((id \
         b6469696-9998-4430-964b-8f8631fa0a0d)(content(Comment\"# Regression: \
         streak bonus still works #\"))))(Secondary((id \
         51340bf3-5f39-4fc6-9c9a-8a7d076b9672)(content(Whitespace\"\\n\"))))(Tile((id \
         d8bfa6be-9383-463b-bfe6-b902d90cab7e)(label(hint test end))(mold((out \
         Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         e0b14795-791e-4f17-9bd7-0108d5095bfa)(content(Whitespace\" \
         \"))))(Tile((id \
         4d74ad31-1645-43b4-8a99-ec44cc9f5660)(label(\"\\\"same quality builds \
         streak\\\"\"))(mold((out Exp)(in_())(nibs(((shape Convex)(sort \
         Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         31839a67-c742-41f9-919a-fb358ea02a5f)(content(Whitespace\"\\n\")))))((Secondary((id \
         61612e1e-2b27-4695-b487-ff7a1b3e6d1b)(content(Whitespace\"\\n\"))))(Tile((id \
         5622f3f9-0213-4822-b521-9f0d703d7057)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         b39fd50e-683f-45be-b289-d4c76d075e4a)(content(Whitespace\" \
         \"))))(Tile((id \
         8be7bab4-adae-46b2-88e4-97de877d837f)(label(h1))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         a7c516ee-6354-4e3e-ba2a-0cc2e65c7222)(content(Whitespace\" \
         \")))))((Secondary((id \
         c6c7b527-1efa-463b-afce-db8e9bd1f9c8)(content(Whitespace\" \
         \"))))(Tile((id \
         2e891373-fb3c-46ea-9ae9-dbfd25dcd279)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         f9eceb41-ded7-41b7-9ad6-abd5561fc581)(label(crop))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         d0b0fb1f-89eb-4103-98ea-1e8baac905d1)(content(Whitespace\" \
         \"))))(Tile((id \
         d895a744-3c38-4488-92c2-c3d5b64a8503)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         3ec1c25a-970c-4f1a-abf2-a36c986572cb)(content(Whitespace\" \
         \"))))(Tile((id \
         9f80fd9c-8ba4-40ab-851a-996bf18c67bb)(label(Moonmelon))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         cd49fb9b-38bf-4e60-a1f6-43c1f3459bd9)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         2a787fd0-d14b-4914-870c-355d611aabd9)(content(Whitespace\" \
         \"))))(Tile((id \
         4adefeec-da03-4ede-b897-8c6e07c2d3eb)(label(quality))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         6c22b555-4df4-4070-99af-32c3b1f8e228)(content(Whitespace\" \
         \"))))(Tile((id \
         c4d0db6a-73f7-4caa-b548-3b8720ccbf99)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         9861d243-d524-462d-8428-847b1a30f0cf)(content(Whitespace\" \
         \"))))(Tile((id \
         09619f46-1a07-4c08-aebb-a7d9130f7167)(label(Gold))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         dba6f01a-136b-4bc8-8606-fc3da1834711)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         c5c78f57-df75-4672-9ee3-4aeec923db4d)(content(Whitespace\" \
         \"))))(Tile((id \
         eb8516fc-01bd-4d29-9649-cd2d04c9bf22)(label(quantity))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         d11a83ec-32c8-44cc-a754-1329c1c2ae8c)(content(Whitespace\" \
         \"))))(Tile((id \
         a73d8218-d395-4267-be32-715cb73de793)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         08b30d02-6ed3-4b90-b4a3-f7c7eac4edfe)(content(Whitespace\" \
         \"))))(Tile((id \
         1caa94d8-ff73-43fc-8738-2f594a07ca5d)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         606076f6-9dba-4f40-be8a-dacbe37e14ea)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         b248ed17-565f-4515-96cd-f9e212436fbb)(content(Whitespace\"\\n\"))))(Tile((id \
         0e60d7cf-b3c7-4d73-9369-e520722cfc57)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         59439a47-a98b-4ea1-a61e-6be45b9cd226)(content(Whitespace\" \
         \"))))(Tile((id \
         a96af0ad-99ad-466f-a338-86ff2890af22)(label(h2))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         290cc28c-b095-49e8-a98d-11bf983e7d7d)(content(Whitespace\" \
         \")))))((Secondary((id \
         030589b0-eec4-43dd-837c-be23ecc1dd08)(content(Whitespace\" \
         \"))))(Tile((id \
         8150f6d7-ba8e-4076-bdf1-c389b6549c65)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         ffa9fa74-2411-4374-9ac4-c1d6173eeb22)(label(crop))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         1051ce0b-8fcf-4978-8004-74bc92c2f855)(content(Whitespace\" \
         \"))))(Tile((id \
         2c1b19e4-c24f-4ee2-b857-d06240e0c7f1)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         94659fea-3420-48d2-bd00-5326b0de6576)(content(Whitespace\" \
         \"))))(Tile((id \
         d1c27e8f-e219-4990-a4e5-14d38d4a32fa)(label(Starfruit))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         b6d1ca3c-cc69-454f-8eeb-7fd9123b25d3)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         876bddb2-954d-430e-b3c6-2301aa9d1d3a)(content(Whitespace\" \
         \"))))(Tile((id \
         e072904d-d679-43f3-b9bc-192dcdce6bbf)(label(quality))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         2f735a7d-e1a1-41ff-b999-328bff5b5a24)(content(Whitespace\" \
         \"))))(Tile((id \
         20eb5649-7dc6-445d-a45e-20390587b994)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         319628c9-07c3-4689-817c-cdba74421098)(content(Whitespace\" \
         \"))))(Tile((id \
         c8c1fb52-68b2-4f14-b02f-5c94657bfd1b)(label(Gold))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         9b1a9cb3-8a9d-4f9e-8ab4-7f61c3f895ee)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         244ede5b-7662-43f9-a968-ad971a0508ed)(content(Whitespace\" \
         \"))))(Tile((id \
         e0cb8d94-a523-413c-8740-c6a1f15584ef)(label(quantity))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         77ef3eac-e6f8-47bb-a8f8-538e0821c642)(content(Whitespace\" \
         \"))))(Tile((id \
         84f185ae-de59-4748-bd12-33adec85a83d)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         03d8996a-2586-4db8-ac8b-1e02713f89ec)(content(Whitespace\" \
         \"))))(Tile((id \
         b064dc3a-6b52-4c50-8e5e-2eeed49d2d48)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         8606e65b-d828-444d-b2bc-26211a9dbced)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         2336be71-0b87-420a-9321-b88dfb7ecfd9)(content(Whitespace\"\\n\"))))(Tile((id \
         aef67a5d-29f8-4f93-8e0d-e6496885e042)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         5c104039-4bdd-49d4-be26-4b8a308af282)(content(Whitespace\" \
         \"))))(Tile((id \
         fd9ea9b5-ac06-42b1-8fb1-794883bc12e9)(label(ledger))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         d911db87-32ac-40fc-89cd-7cb7a508e4f2)(content(Whitespace\" \
         \")))))((Secondary((id \
         9cbc53ba-4174-4371-bbd6-de752ef94039)(content(Whitespace\" \
         \"))))(Tile((id \
         dea2030a-08d6-4d73-8eb2-0881ff6e173b)(label(run))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         e36e4ab3-4814-427f-90be-87d4bb05a3f8)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         039ac46a-ea5c-454c-9ea0-4ba9c28806e9)(label(initModel))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         df1f5dd7-9f09-4847-b1bc-cb7bba8ea5b9)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         4fc825de-4741-4535-995d-7a13cac66240)(content(Whitespace\" \
         \"))))(Tile((id 93bcad9f-4ecf-42e7-9fff-f430e3bc8aa8)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         8b14a11b-012c-4020-b7cc-1dbe4858ea77)(label(RecordHarvest))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         a586128e-0b1a-42fc-970c-613a530244ea)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         399ba630-6e15-4353-a3d6-b6947e1b930f)(label(h1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         b59fd450-0f7c-49c2-a03f-b114226359a2)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         0e7bd1e8-d679-41bf-8277-289787abe24b)(content(Whitespace\" \
         \"))))(Tile((id \
         b0c9e2e7-6cc8-4b49-8b84-d0e71aa77217)(label(RecordHarvest))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         b51026ea-dd27-420f-9d16-323ae56697f3)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         4b855a07-30c0-4d1c-9ade-23f01db6fce8)(label(h2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))))))))))))(Secondary((id \
         74e1d708-3b49-4eb6-9244-6d9363d4e7ff)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         390692da-5587-41fd-aa00-1658dc299c59)(content(Whitespace\"\\n\"))))(Tile((id \
         4c72cc02-9c4d-41eb-8550-82af5ae5dbb1)(label(ledger))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         df0cde17-e1eb-49b6-a643-f55ec7424483)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         a4d35d4a-dc29-4bc9-8c07-2f87024c6db3)(label(streakBonus))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         75714106-6b33-4695-9075-e33ca7d55c55)(content(Whitespace\" \
         \"))))(Tile((id \
         a9c4de9c-176e-4e4a-8a73-38c39cc3c738)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         fceaa79b-af87-4cb0-bbda-5cbec0568415)(content(Whitespace\" \
         \"))))(Tile((id \
         d2239375-fccc-4282-be0a-20ba838c5926)(label(5))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         959239a3-2715-4e4d-a5a0-030183410f6f)(content(Whitespace\"\\n\")))))))))(Tile((id \
         50fedeae-a082-4dc8-ad11-d39541831148)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         0f663547-5b4f-49ed-925b-dbb9a72fb2ba)(content(Whitespace\"\\n\"))))(Secondary((id \
         f3fbf133-0997-4be3-bc1d-7ad57f8846cb)(content(Whitespace\"\\n\"))))(Secondary((id \
         4abed2cc-d07b-4166-b8ab-ad7b36ab59e9)(content(Comment\"# Regression: \
         claim bonus still works #\"))))(Secondary((id \
         4695d71a-2eef-47ce-8c11-3b0b07368e0e)(content(Whitespace\"\\n\"))))(Tile((id \
         761214db-a3ba-469b-ae07-0715b68d6a97)(label(hint test end))(mold((out \
         Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         b4938d0f-ed1c-4a15-b1a5-5eedca90ab20)(content(Whitespace\" \
         \"))))(Tile((id \
         93f8c113-ee5a-4682-806a-29cbf332ca3b)(label(\"\\\"claiming bonus adds \
         to total and resets streak\\\"\"))(mold((out Exp)(in_())(nibs(((shape \
         Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         095ab44a-a253-49e8-a90e-6a3dd10e74f9)(content(Whitespace\"\\n\")))))((Secondary((id \
         31f4cabb-0a53-4e3e-a6ec-377ce2e0ea47)(content(Whitespace\"\\n\"))))(Tile((id \
         6e1ccfa7-75e1-4c18-9120-b67a7c442489)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         fd465160-da29-4ea7-bb87-2d9e4fb487c1)(content(Whitespace\" \
         \"))))(Tile((id \
         5a938746-7488-4fd0-9cfb-7b7e88da4546)(label(h1))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         da65e1d9-0c9f-4dac-a1ba-21035e27e840)(content(Whitespace\" \
         \")))))((Secondary((id \
         399857fa-9a54-46de-9b07-9465245c4f1a)(content(Whitespace\" \
         \"))))(Tile((id \
         d3af827b-1bb3-417b-a27a-3e8d905a4b6b)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         e4b3d79c-ede1-4220-87ca-f9fa5a5611fc)(label(crop))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         50dc58b5-3056-4655-a326-71a152712da8)(content(Whitespace\" \
         \"))))(Tile((id \
         2a67e11f-9d30-4ab6-bffe-6cce7af32387)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         44dd5972-1adf-4795-bbf0-fc6a4d5a975c)(content(Whitespace\" \
         \"))))(Tile((id \
         a3144832-735d-4336-b93e-dea97ffc66f0)(label(Moonmelon))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         0a974cb3-b9a5-4899-86f8-940d05b0aa73)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         0d565548-b9e9-4451-ab3d-202c73725c9d)(content(Whitespace\" \
         \"))))(Tile((id \
         9fc8f327-e6bb-4bdc-bce0-7442fafbb302)(label(quality))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         22c6367d-85e6-4144-8919-f3735518b102)(content(Whitespace\" \
         \"))))(Tile((id \
         4f467794-679e-4ae1-9c92-a0478c7785c8)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         ee78b64c-af5e-4541-82de-ae9ff4a9f74f)(content(Whitespace\" \
         \"))))(Tile((id \
         c42ae373-658d-45df-885a-ab7bfbde3aa6)(label(Gold))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         3a88c471-7cbf-4808-974d-fdc15ea96f35)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         583c6506-dffc-4b77-bb5a-88ab8fff5278)(content(Whitespace\" \
         \"))))(Tile((id \
         47555963-88fd-483c-9e07-7946f19ead8c)(label(quantity))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         4f895d7d-bdf7-4981-9aab-079dc70b90a6)(content(Whitespace\" \
         \"))))(Tile((id \
         d56bc663-c1b4-4c54-9dc6-3cbe4ae52ce8)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         518a9d50-5917-4c4c-9dff-1cd5954bcc78)(content(Whitespace\" \
         \"))))(Tile((id \
         cd4bb3e7-dbed-40a1-bb27-734060be85e8)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         591c8e08-bd25-432e-a615-f615112e743a)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         ca9bd86b-5e66-4960-a8e5-72c11b2acd1e)(content(Whitespace\"\\n\"))))(Tile((id \
         4cbe25b5-4f62-4cf5-865f-121845008128)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         4a2fb14f-96d2-4b73-b12a-8747fb7f9494)(content(Whitespace\" \
         \"))))(Tile((id \
         6e3db86e-371d-4371-a1f1-190e22f4dd90)(label(h2))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         f02cd806-5974-4cbd-95b5-3fcda929985f)(content(Whitespace\" \
         \")))))((Secondary((id \
         0f877f29-4f7c-422d-b804-313d78812ce4)(content(Whitespace\" \
         \"))))(Tile((id \
         1b60bff7-a792-4bfb-883b-8d22c901d25a)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         15650a2d-6601-4917-bf37-6bb93c3991c3)(label(crop))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         967910b2-e8cf-4f8a-bace-19bec5698aa8)(content(Whitespace\" \
         \"))))(Tile((id \
         17212f88-c74c-479b-96c4-14420b1db554)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         4247ddc6-df18-456a-9dde-154adaa249c7)(content(Whitespace\" \
         \"))))(Tile((id \
         cb147dc3-a19f-451a-9264-5dc53153b631)(label(Starfruit))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         7740fe08-9d4b-4bcb-a603-564f11d211b0)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         064792af-cf3c-47de-8d5a-8add68c2b501)(content(Whitespace\" \
         \"))))(Tile((id \
         334db1ae-eb0e-4090-8f73-fffab47e68bd)(label(quality))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         f37b31d7-4b10-45e9-92d2-82d68686c26e)(content(Whitespace\" \
         \"))))(Tile((id \
         61f5c463-f122-4ac2-bea8-f93c41c41391)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         09b07041-3345-4aa2-a26a-8f0a760b389c)(content(Whitespace\" \
         \"))))(Tile((id \
         6e349a6e-62a8-48d9-9267-7aadbf9443c0)(label(Gold))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         0f276b46-cd72-4aa1-9505-8cbd74e845d9)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         11d62b10-bae7-408a-819f-7112de291ae6)(content(Whitespace\" \
         \"))))(Tile((id \
         b9a21e02-6d2e-46c1-99ce-ddaea595467f)(label(quantity))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         164a266d-e9a7-49db-be3b-59d37449c242)(content(Whitespace\" \
         \"))))(Tile((id \
         93a2df80-dce2-40a4-ae4c-f216782e1e95)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         40211721-93ca-4c90-a433-0e4f2ddace8c)(content(Whitespace\" \
         \"))))(Tile((id \
         99570d53-f5a3-42b2-a15b-15cad0420cb8)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         33a62826-b798-4ed3-805c-f48d72df1d1e)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         1c3296f4-40ba-42e8-95d7-ffd00d5a9ff4)(content(Whitespace\"\\n\"))))(Tile((id \
         9fd2b8e6-41d6-41d6-bb19-64668785e111)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         1a781be7-af3e-4f4e-b324-b7c6e045e8e4)(content(Whitespace\" \
         \"))))(Tile((id \
         6d73999b-1e4b-43d9-a5ab-6c1232b78f92)(label(ledger))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         1083f0c8-a57b-4336-aed8-7ec708a25691)(content(Whitespace\" \
         \")))))((Secondary((id \
         045297a1-68df-4444-a0d0-0a1a8dc6fb7a)(content(Whitespace\" \
         \"))))(Tile((id \
         f0f2c7fd-b723-4a8a-979c-ad2e88b06d62)(label(run))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         2a068cda-5836-4429-a24f-9532dfe3b7bc)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         e6735f66-9038-4b72-93e6-5c3cd387c8e2)(label(initModel))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         83e4f1bb-c322-4811-b292-60e33720f53d)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         22d4cfe2-9b64-486f-b212-9bef0e797919)(content(Whitespace\" \
         \"))))(Tile((id 034d5486-9885-45fe-b299-99bd1ba96f9f)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         87f0d57a-fb4e-4925-9a4a-a2fd2018615f)(content(Whitespace\"\\n\"))))(Tile((id \
         7b999e45-5fa6-416a-b141-1afc6d55c0ea)(label(RecordHarvest))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         ad45e277-9d37-4ecc-b47d-6adf16feb308)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         eef62db8-43fe-4a81-9448-c312507c7aa3)(label(h1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         354a8327-6fc0-4e90-af33-c01a8b0f463b)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         12d1a3e0-e278-4db5-b183-3503639e997c)(content(Whitespace\"\\n\"))))(Tile((id \
         43ca85d0-4842-4904-adb1-6b2e2589b176)(label(RecordHarvest))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         86cd9ec4-71ae-413f-82b4-9f762e46ccff)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         700d5e8b-7aed-41da-b17e-6dd0721dcfeb)(label(h2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         b3e18dbf-f334-4a21-8966-2630663dcac6)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         5340c87e-a6d2-4a77-9f48-b1f50b7805d7)(content(Whitespace\"\\n\"))))(Tile((id \
         3483421d-fdb6-416b-84de-84ad04b2e46d)(label(ClaimBonus))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         f29a592c-1947-4696-9623-52b713d858ce)(content(Whitespace\"\\n\"))))))))))))))(Secondary((id \
         6f216968-7c13-4303-8f46-aa30ebe928c0)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         1b109857-424f-453e-90d8-2eb850497a1c)(content(Whitespace\"\\n\"))))(Tile((id \
         22e24240-ce17-47ae-8afc-898004129095)(label(ledger))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         2b04f0f8-d0b7-40a0-9c9a-2c558b60fbd5)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         464bd83e-4c5a-4bab-96ef-27dae4703288)(label(streakBonus))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         fc6f06ac-5f0d-41d1-a806-e46fff70d623)(content(Whitespace\" \
         \"))))(Tile((id \
         5c5c99d2-3d36-43ed-ad85-38036cb75695)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         4b20c9e3-631b-4f27-b1c6-9ae45ad9bf23)(content(Whitespace\" \
         \"))))(Tile((id \
         430bfd72-ae66-4548-8fd9-2404e38dc960)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         97d2042a-ee15-4c28-bcc3-54d047d2d926)(content(Whitespace\"\\n\")))))))))(Tile((id \
         639c8f21-2ef4-4364-bf3e-9df0cf6cbe91)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         46270748-52c2-4d10-a098-78443b237807)(content(Whitespace\"\\n\"))))(Secondary((id \
         397c2507-1670-4e61-8107-13280c950ca9)(content(Whitespace\"\\n\"))))(Secondary((id \
         a58f300c-6aab-4a77-8393-40ee0ef3b10a)(content(Comment\"# PremiumSale: \
         low streak gives 1x multiplier #\"))))(Secondary((id \
         90ffa561-2a12-4d61-be84-6228f9ee43a2)(content(Whitespace\"\\n\"))))(Tile((id \
         f36db956-8070-4cf6-8156-5b2c03f82cd3)(label(hint test end))(mold((out \
         Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         740d6543-2d74-4677-9060-04d410560c44)(content(Whitespace\" \
         \"))))(Tile((id \
         b7435429-e758-4003-b329-7acc06383a88)(label(\"\\\"PremiumSale with \
         low streak uses 1x multiplier\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         8dbf3bef-4c0a-4521-8c18-9e2bdd2f5a16)(content(Whitespace\"\\n\")))))((Secondary((id \
         cd46cade-c6f3-47fd-b64b-60b79606bdd4)(content(Whitespace\"\\n\"))))(Secondary((id \
         b3f55382-9c3a-4032-997e-6f374cd8fd99)(content(Comment\"# Two \
         same-quality harvests build streakBonus to 5 #\"))))(Secondary((id \
         26ed87b2-e9c3-4420-87cd-4fd4d889ea95)(content(Whitespace\"\\n\"))))(Tile((id \
         6469ccdd-2baa-4534-87d6-dcec29d11f9b)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         cdef31b8-f56e-4458-868b-db406514a382)(content(Whitespace\" \
         \"))))(Tile((id \
         acbcc312-26a7-4fe7-a2e9-b8411582b7a7)(label(h1))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         fd41a87b-4200-4e9e-96cf-08e11c377389)(content(Whitespace\" \
         \")))))((Secondary((id \
         84384408-4183-4555-9d81-780253246a09)(content(Whitespace\" \
         \"))))(Tile((id \
         da078a24-c638-41f5-9770-ab8b298cd107)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         6601f675-84ee-4b08-8c9b-d6ceadb22ac9)(label(crop))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         743ea2eb-5076-4370-94df-fa4e0e74d53c)(content(Whitespace\" \
         \"))))(Tile((id \
         82f3925c-2f04-4ab1-a2f3-68cb8236e83c)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         6aa19e9c-3347-4870-8e35-80edc2c4a7bc)(content(Whitespace\" \
         \"))))(Tile((id \
         7243aa27-ca1a-425e-913c-64b5621f9a58)(label(Moonmelon))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         2508c47e-14a7-4263-9ac8-ee5c309e6290)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         48ae5ccd-05ee-4e88-a82d-86061ae1bb45)(content(Whitespace\" \
         \"))))(Tile((id \
         ce762433-7d8c-42a2-8ed1-2e80b5d12a03)(label(quality))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         327c3b0e-8f64-4dc7-afc0-b52132511b8e)(content(Whitespace\" \
         \"))))(Tile((id \
         f2f12336-f0de-45d1-bd27-b70223ea2b8d)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         ae41c941-0239-4c93-80d5-805a141aff99)(content(Whitespace\" \
         \"))))(Tile((id \
         cd40578f-03f8-4899-979f-1ac633161eb8)(label(Silver))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         2fbb79ea-9f66-48ce-8d89-8ad9d3a4828d)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         23fa8907-2a35-445c-aa15-cff43c017bb3)(content(Whitespace\" \
         \"))))(Tile((id \
         85941c77-9f4e-411b-b1c1-5feb6b77372b)(label(quantity))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         3da0c470-b46c-413d-b1d0-f00069b72df8)(content(Whitespace\" \
         \"))))(Tile((id \
         e0bf32de-cc92-4d46-89ef-2d0f2850559b)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         ee1c8102-4d94-4fbc-b0a8-4fa227d5c50a)(content(Whitespace\" \
         \"))))(Tile((id \
         87f722a0-883c-4901-824c-fab0615d111b)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         9d00a82c-8b21-4787-91f0-5bd09ea2e87e)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         b33ab58e-e0ed-4184-8792-f4d8f6132aa0)(content(Whitespace\"\\n\"))))(Tile((id \
         b5b519f7-8746-44a9-8e98-8fc7ec79e0a9)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         b6f835c0-347f-41e7-b6f4-917e80222c7d)(content(Whitespace\" \
         \"))))(Tile((id \
         58a430db-dce5-4b41-8bd3-e5306e2dcb12)(label(h2))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         dbe47124-4411-4a86-a6a4-451dfa8add3e)(content(Whitespace\" \
         \")))))((Secondary((id \
         64bf3ce0-4876-493a-8531-14730dbed703)(content(Whitespace\" \
         \"))))(Tile((id \
         1b0c837b-7e4c-40ac-a936-c76382995370)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         986e4951-a910-4e34-8bfa-3e95c3ec502d)(label(crop))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         382002e7-312b-4831-9217-f1660997a159)(content(Whitespace\" \
         \"))))(Tile((id \
         68b80207-14a5-4a12-ae84-307514a27cab)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         b5cf4b55-4b3b-478f-b7cd-339b20c4a080)(content(Whitespace\" \
         \"))))(Tile((id \
         2122db7e-de4b-4742-819c-0bb12c25dc4b)(label(Starfruit))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         e9571375-f929-484d-b4b9-cf7326b30d52)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         6df4b4fe-c175-4ef4-ba88-fea87586a5d1)(content(Whitespace\" \
         \"))))(Tile((id \
         00b1081f-acbe-4aec-b816-b721b3934589)(label(quality))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         4ab61c90-0b6e-48da-85d0-041452abd5c8)(content(Whitespace\" \
         \"))))(Tile((id \
         5b7776bd-482d-4ac1-991f-5cabed189360)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         e9082897-670e-45b1-8420-0d944ca4d8d7)(content(Whitespace\" \
         \"))))(Tile((id \
         fd84d6c5-f45e-4e40-9788-50cdced0eabf)(label(Silver))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         82765f59-752a-4280-926b-9641b6462a81)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         ab552b56-9740-4aba-8783-d7f1e698b829)(content(Whitespace\" \
         \"))))(Tile((id \
         46ae403b-3d98-461a-9e90-db88141825ba)(label(quantity))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         4224852c-0a3d-4248-b43b-6be07d67a8c5)(content(Whitespace\" \
         \"))))(Tile((id \
         0efce3b4-3b98-4290-8bed-3a149b1bbcb0)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         6d031da1-6203-4b31-96c8-28f2b4a3f5e1)(content(Whitespace\" \
         \"))))(Tile((id \
         e91342b5-0fd1-45f2-89d9-3560d43a99e2)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         1ca840bb-bfd8-43c7-9ecf-8ee629f76bb9)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         de080aa9-be8b-47ab-9229-e251c21e6e3f)(content(Whitespace\"\\n\"))))(Tile((id \
         f0ccb61b-8057-499c-afe5-472f13c2b60b)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         624190bd-a513-4771-a912-7b07deadc636)(content(Whitespace\" \
         \"))))(Tile((id \
         195579f3-f217-408b-b657-c654feca649d)(label(ledger))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         c76e368e-02e2-47b5-a9c9-b99c48c87531)(content(Whitespace\" \
         \")))))((Secondary((id \
         0bca530d-88cf-4b7f-9b91-6947d4503cfb)(content(Whitespace\" \
         \"))))(Tile((id \
         0458111a-1141-4a05-9fb2-6d90b8ef8ca0)(label(run))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         ee438d18-cb4c-4cf8-9a65-bc318bbbe610)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         07b3e191-28d1-43e7-84dd-b68c17877b59)(label(initModel))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         beb2be5d-58fd-4777-9e92-290351e9ed35)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         e512032d-d940-4fda-89cf-4908094b7727)(content(Whitespace\" \
         \"))))(Tile((id 748eb9df-c798-4041-8c63-f5f5b429fb85)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         99eca50a-2305-4dc6-88ef-74edf6f3e4d9)(content(Whitespace\"\\n\"))))(Tile((id \
         3f06f3b6-adf4-4bd8-b6fa-f07293d973c6)(label(RecordHarvest))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         8967fef6-f484-467d-986e-bbe19de335e0)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         bcd9c616-6a0c-4f41-a7a1-783f7a5ecea8)(label(h1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         345da437-f519-43f1-bec0-1fd698c25f9f)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         1d2d9c1f-1bf9-427d-acab-6027d55b6230)(content(Whitespace\"\\n\"))))(Tile((id \
         44d48808-f341-440f-b5c5-e0b36407e5e3)(label(RecordHarvest))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         eaec240c-fd60-4fcf-b85f-2a699bb53278)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         aa50defe-ac27-4e3b-a29b-3becfd8c9e34)(label(h2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         a4395f95-ece1-4bdc-a97e-efbc1cbdf4f0)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         caea20f1-4f56-4a93-9dd4-068acbd78fed)(content(Whitespace\"\\n\"))))(Tile((id \
         f9ab1657-259a-41c9-8dd5-ed00843142c3)(label(PremiumSale))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         61cf020d-e7db-4e63-9809-a0824b311621)(content(Whitespace\"\\n\"))))))))))))))(Secondary((id \
         9b973e6c-4b07-48fe-9889-a11aca048115)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         655abd2c-5954-4e07-8b00-14d966be700e)(content(Whitespace\"\\n\"))))(Secondary((id \
         d986860b-51cd-47c2-8eac-d1d891f0159b)(content(Comment\"# streakBonus \
         was 5, multiplier = 1, payout = 5 #\"))))(Secondary((id \
         624d9873-6812-4ae6-8093-58a8889820d8)(content(Whitespace\"\\n\"))))(Secondary((id \
         7a382c71-b069-43b8-91a9-de22e1d6c37b)(content(Comment\"# h1: 15*2*1 = \
         30, h2: 20*2*1 + 5 = 45, PremiumSale: +5 #\"))))(Secondary((id \
         64b52e27-4510-4687-9799-fb891070a44c)(content(Whitespace\"\\n\"))))(Tile((id \
         2e042d1c-127c-43a2-bbf6-bb1d01ffa51c)(label(ledger))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         c992e278-4b13-4e2b-91d2-a34e766780f6)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         2f0d5d0b-a6e6-4c94-b5c3-23019ad662e4)(label(totalValue))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         d9d992ba-a242-428f-9b4d-3e5356efd157)(content(Whitespace\" \
         \"))))(Tile((id \
         802760f5-9973-44f9-b608-7f0709f60ceb)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         16bd17c8-22b3-4aae-ad90-e12b72f7ed78)(content(Whitespace\" \
         \"))))(Tile((id \
         7e06f13f-1d5d-4e91-b23d-a15735296470)(label(30))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         45d338e0-6951-4066-9015-aa608f625fd7)(content(Whitespace\" \
         \"))))(Tile((id \
         efd26527-3e1a-44fc-828f-c56129600d53)(label(+))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 28))(sort Exp))((shape(Concave \
         28))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         1ded2586-a811-4610-80c9-99f0d4211048)(content(Whitespace\" \
         \"))))(Tile((id \
         559d038c-c66a-4569-9102-6787e7d91d3f)(label(45))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         c68c04c6-2bfa-477d-9d84-807c82a5301f)(content(Whitespace\" \
         \"))))(Tile((id \
         35f63690-14b9-4ba7-8afc-4743628a3fc2)(label(+))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 28))(sort Exp))((shape(Concave \
         28))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         4394a6ac-e9fe-4cf8-aba4-ac0418ffdc8e)(content(Whitespace\" \
         \"))))(Tile((id \
         f30fdf78-69bd-4e33-ba0c-c9e2444c42ba)(label(5))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         918f237f-5ca5-4301-a5c5-d2b32705b023)(content(Whitespace\"\\n\")))))))))(Tile((id \
         e8930ca0-f143-40ff-92da-de1f13418a23)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         f5eb4134-113e-46b2-930f-3e951e8c736d)(content(Whitespace\"\\n\"))))(Secondary((id \
         fe0a40b2-4939-4e98-85d1-09c298873f3b)(content(Whitespace\"\\n\"))))(Secondary((id \
         f2c31bd2-8473-4932-806a-f8feeda8d66a)(content(Comment\"# PremiumSale: \
         high streak gives 2x multiplier #\"))))(Secondary((id \
         d79e5d53-567f-495c-8dc8-ad37ad8ebd21)(content(Whitespace\"\\n\"))))(Tile((id \
         108ecb70-ace9-419f-900d-1333865309f3)(label(hint test end))(mold((out \
         Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         52679bb4-1db1-47c6-9686-cbc6f6314fe5)(content(Whitespace\" \
         \"))))(Tile((id \
         fcd8098f-5560-4e3e-a3e0-92686693a162)(label(\"\\\"PremiumSale with \
         high streak uses 2x multiplier\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         12069823-ccaa-44e3-bb8e-f715117f3829)(content(Whitespace\"\\n\")))))((Secondary((id \
         3d004f7a-c49a-4156-8dce-8b9d9f03cba1)(content(Whitespace\"\\n\"))))(Secondary((id \
         ec6af318-4cef-4945-8d51-9067a74e59e6)(content(Comment\"# Three \
         same-quality harvests build streakBonus to 10 #\"))))(Secondary((id \
         6677d911-c208-48c4-8877-7bc486e164c7)(content(Whitespace\"\\n\"))))(Tile((id \
         f175a146-a9db-443b-a250-692dabbf4a07)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         29e1de8b-352c-439b-bc8a-163b7b52a568)(content(Whitespace\" \
         \"))))(Tile((id \
         90b839f5-6b14-4543-9d1e-c19f3a3cda1b)(label(h1))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         4d2462c8-2680-41db-bc9e-e202fd390a8b)(content(Whitespace\" \
         \")))))((Secondary((id \
         2f2ad6c7-7146-45cb-a360-537dde7aef3c)(content(Whitespace\" \
         \"))))(Tile((id \
         12d71e66-1e4b-4ff5-a5e9-0b3689031af0)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         ad0ffb30-5748-40a6-9679-ba11a5db2a35)(label(crop))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         494352d7-4f47-47ef-bba4-e4890c856b77)(content(Whitespace\" \
         \"))))(Tile((id \
         42fa3d11-81cd-456b-b72a-c9e7c550cd6d)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         57527640-6751-43d8-afd5-fe135421e777)(content(Whitespace\" \
         \"))))(Tile((id \
         f8156be6-01c0-4059-9dc9-90af831d363a)(label(Moonmelon))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         b82a8909-7fc4-4db5-a25d-f7e0974cd73f)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         ffe8dde1-1d9a-4047-b62e-6648547fb29d)(content(Whitespace\" \
         \"))))(Tile((id \
         da318cdb-d018-4109-b8a9-41b8b93fbc7c)(label(quality))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         34151d60-3cb3-421e-9f71-0c96b8f4ab0e)(content(Whitespace\" \
         \"))))(Tile((id \
         d5fe3b0e-53e8-4cd4-9184-f344fd5f5d9a)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         34f76e6e-1574-4bbb-8494-7762f686fde5)(content(Whitespace\" \
         \"))))(Tile((id \
         5e7575a9-4a57-4909-a0ee-7978d11f68ce)(label(Gold))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         ac1f6da4-b925-4272-ab38-0dae209454cc)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         ed71dfe0-1920-4035-b7ee-910e925f1104)(content(Whitespace\" \
         \"))))(Tile((id \
         8c9bd4e5-f36c-4eae-9073-872160a9a110)(label(quantity))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         10e4d27f-0fc8-4971-9ece-94bfa0dabf33)(content(Whitespace\" \
         \"))))(Tile((id \
         68e6058e-d1c4-4256-a8bf-1cdf0495dbfa)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         cf153de2-1f4b-4ef8-a8b0-d1b32c35a482)(content(Whitespace\" \
         \"))))(Tile((id \
         d1121564-13dd-4485-b00f-c4f9be37f3fb)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         0cb73517-e6de-49d8-bfb9-7b27820b8129)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         37cbc953-0205-4cf0-bea1-91efdf62f147)(content(Whitespace\"\\n\"))))(Tile((id \
         92313237-933b-4e2d-b089-b27fbc88be7b)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         21a8cde3-dbf2-4d5f-a9b4-76853581868a)(content(Whitespace\" \
         \"))))(Tile((id \
         a500bf4f-3067-46e5-833c-b5b4f4437de4)(label(h2))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         f27b1100-6d28-4259-98df-56529a912028)(content(Whitespace\" \
         \")))))((Secondary((id \
         4fe9e9b6-bc32-44ce-ba1f-eeb05f599f6f)(content(Whitespace\" \
         \"))))(Tile((id \
         8b7dd082-a1ca-4d65-aa5e-e08235ecdb97)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         d5afdd67-d5fe-45d8-b2a8-66e38a8670ec)(label(crop))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         1ee5bf44-3b1a-4693-b080-af6baa92ab86)(content(Whitespace\" \
         \"))))(Tile((id \
         81a4264e-1841-4471-80e7-367309505075)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         d0d09c0c-0e66-4c43-be71-aafb5e5d6076)(content(Whitespace\" \
         \"))))(Tile((id \
         2299af2e-998f-4fb8-b362-c988ec5d969f)(label(Starfruit))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         9a1837f5-cee0-412d-b795-9f221bf19656)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         e698ed05-23ac-4c8c-97fa-c3be26b039b9)(content(Whitespace\" \
         \"))))(Tile((id \
         d03955a4-ce77-4c91-b910-8e4c4dfd4474)(label(quality))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         6ffbcdf1-bc8f-45da-9835-9d57d3952bc3)(content(Whitespace\" \
         \"))))(Tile((id \
         44fa01d8-70a7-47e9-9bbc-8ebd4503cf20)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         3f136285-483c-46f5-aab7-678b2b24676a)(content(Whitespace\" \
         \"))))(Tile((id \
         e4075383-bcd1-4851-814b-b4af5690eaa4)(label(Gold))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         11c3e3bc-8e26-4d43-8b6d-4ecc584a8251)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         67b2378b-3087-40a3-a4bd-f8bb9f903cc9)(content(Whitespace\" \
         \"))))(Tile((id \
         c2b74aac-bf6f-4c15-9de4-643996db1106)(label(quantity))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         89c96041-cf22-4e41-ad37-23adea6cd0b5)(content(Whitespace\" \
         \"))))(Tile((id \
         745255f9-ad57-4744-b437-7cb76938cf15)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         2555e389-fa8b-4cd6-ad36-4815a5063a10)(content(Whitespace\" \
         \"))))(Tile((id \
         79febdb2-0967-49b6-b33a-b33a422ce0fe)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         c6ea82ec-5cdb-4085-8cd6-fb9260c5a330)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         54092b84-3bbf-4862-a22e-8d57a7fe3610)(content(Whitespace\"\\n\"))))(Tile((id \
         aaa8d817-6ed8-4a02-a07f-4678a86df581)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         b4ea9b72-26ae-42ec-9ccc-a8deea511f15)(content(Whitespace\" \
         \"))))(Tile((id \
         b51237e9-9943-4df6-be7b-d949dcf8ac8a)(label(h3))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         736d0049-9a11-4c58-a95c-8e7acce22764)(content(Whitespace\" \
         \")))))((Secondary((id \
         d15def02-6fc1-4ef2-85a2-17da218029e4)(content(Whitespace\" \
         \"))))(Tile((id \
         17173a31-bfc6-42c9-bf72-7be4d74cc5be)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         3af776d4-57db-4c46-87e8-0bbae518aaae)(label(crop))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         e9684b2f-0936-44c4-8d4a-25b39b0eaa12)(content(Whitespace\" \
         \"))))(Tile((id \
         b2d43c98-8297-4dc6-af86-ef593e26dbdb)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         8e86b96a-e119-4b2e-953d-5d91e377c06a)(content(Whitespace\" \
         \"))))(Tile((id \
         569c90f7-95c6-472f-a975-e8d9e2dbaa7c)(label(Nightshade))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         a418485b-4729-4f36-bc8d-22d50ff56906)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         11f39b49-02b6-4a13-bbda-57efb453faee)(content(Whitespace\" \
         \"))))(Tile((id \
         e98f878d-99bc-47e2-90aa-fae51dddcd8d)(label(quality))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         be6f692c-4a10-45d7-b08b-31bdca30c846)(content(Whitespace\" \
         \"))))(Tile((id \
         3af94134-7527-41bb-bb00-e21c97d4e932)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         9f885f42-6a3b-4c1d-93cf-49e503f08048)(content(Whitespace\" \
         \"))))(Tile((id \
         657970d5-35bf-43ec-bed3-223a9ed116ca)(label(Gold))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         8561c3ff-69dc-4266-b6fb-310024af6e8a)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         f88e5cb0-0073-4c7e-b029-3554857bb3fa)(content(Whitespace\" \
         \"))))(Tile((id \
         44ea14a0-2f0d-4bf6-9d4e-9648d2ddcfaa)(label(quantity))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         9ba4c3d1-c395-46eb-b506-00013abe9b8f)(content(Whitespace\" \
         \"))))(Tile((id \
         fd75dd28-6b5d-4824-adbc-fd3273559227)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         7dbedb68-ce7b-4d48-b019-b8d76d283559)(content(Whitespace\" \
         \"))))(Tile((id \
         14471eb7-ae4c-4ddc-ba3d-7d3d5d6333d9)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         ea6d9152-adc4-42e5-bbbf-44aba9b5526d)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         8d21b070-d52a-4cc3-b73e-6cd5a8f09fba)(content(Whitespace\"\\n\"))))(Tile((id \
         76ba9e73-d939-45ef-b9b1-de26d0d55add)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         fd61b861-ae87-4ad7-a6e8-1f2be3c2b226)(content(Whitespace\" \
         \"))))(Tile((id \
         7c82b9bb-5157-4b10-801c-60e5f16c411c)(label(ledger))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         86354d6e-80d4-4881-a246-ed0a1be37c08)(content(Whitespace\" \
         \")))))((Secondary((id \
         ffeafb14-be60-4aef-b908-046d4132c163)(content(Whitespace\" \
         \"))))(Tile((id \
         d9cd805a-212b-4011-a1f8-2b0ff975a03d)(label(run))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         d71695f6-bba3-4e96-a02e-9a25c7a7911a)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         6731666a-ab74-41ac-bc87-0ac1ef3e451a)(label(initModel))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         02f47ee3-765f-4681-8037-f91df7afa473)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         e1bc3d4b-71f5-4c5b-bd0b-27ba370c73fe)(content(Whitespace\" \
         \"))))(Tile((id 1dd0a641-c08d-4ee5-ada1-3e226848a664)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         115f6c5f-a50a-4a5c-9ed6-05f875a3aed7)(content(Whitespace\"\\n\"))))(Tile((id \
         9c99dfcc-3387-49b6-8605-58c8ceddbf37)(label(RecordHarvest))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         aa3dc507-6de1-45b0-9814-4216cd86b1f2)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         f792c430-d316-4544-a5b4-fff0772f60ca)(label(h1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         7912e002-65d6-44c1-870c-981b8618b5c3)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         9c4823de-a602-4ec1-b472-e427c7b07413)(content(Whitespace\"\\n\"))))(Tile((id \
         93fe02b2-7604-4ecb-b6de-2567cea8623a)(label(RecordHarvest))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         07db795b-5f0d-4db0-939b-3004a8e8446f)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         1f9a6be6-4726-4a8a-a730-618aae6fe9d5)(label(h2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         93a5d6bb-0b56-42bb-939c-e42fde91bcb1)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         4ccd1cc2-c10b-49d1-ae2f-ee0291134582)(content(Whitespace\"\\n\"))))(Tile((id \
         d5b15362-1695-4e5c-b2a8-b8236c7f1c1c)(label(RecordHarvest))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         d775977c-3dac-4e21-a8c0-8ed5fd69c8e0)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         399cb913-6c48-472a-b6d8-6093b224566e)(label(h3))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         0a2a2b5d-d8fb-43ba-bfa0-0bfa80db1e8c)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         9b0181d5-cadb-4917-b65f-2a3b0fe9c5ab)(content(Whitespace\"\\n\"))))(Tile((id \
         ccde7fff-f941-48ba-b044-d0f9b80da617)(label(PremiumSale))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         22917250-600c-4951-a43e-5decc7d47653)(content(Whitespace\"\\n\"))))))))))))))(Secondary((id \
         403bd340-5ec1-4b63-9f87-04b0b1ec4534)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         ca6692e3-2e3e-49ed-b583-0ff7f3608adb)(content(Whitespace\"\\n\"))))(Secondary((id \
         0602a813-c1e0-4af4-b913-599a7568fd4d)(content(Comment\"# streakBonus \
         was 10, multiplier = 2, payout = 20 #\"))))(Secondary((id \
         f22b66ff-3f5c-48b5-b226-081ccc0fea12)(content(Whitespace\"\\n\"))))(Secondary((id \
         fa355a6c-0c01-497b-935c-4810485adf4f)(content(Comment\"# h1: 15*3=45, \
         h2: 20*3+5=65, h3: 20*3+10=70, PremiumSale: +20 #\"))))(Secondary((id \
         bb9ec700-64e9-446e-b054-dd521d7c9697)(content(Whitespace\"\\n\"))))(Tile((id \
         7663dfc5-9dd9-4e51-943e-f33e5500dfbb)(label(ledger))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         9f49e2a1-67dd-4a2e-a3d7-a9b71214d3a4)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         cf1d52f3-6c44-4eae-8c06-c4e6afeef7b7)(label(totalValue))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         85913133-ff1f-4bdc-9b4a-119977f710a2)(content(Whitespace\" \
         \"))))(Tile((id \
         ade977ef-4274-442d-8794-bd7e0a49501f)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         d7c65dc2-3236-412d-bb9d-4eb1e7037890)(content(Whitespace\" \
         \"))))(Tile((id \
         d0cd48ea-1069-4fcb-aa0f-9780cce52068)(label(45))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         46725839-38df-4968-9312-f15cd00638f4)(content(Whitespace\" \
         \"))))(Tile((id \
         fab05d2e-f299-4ef2-b30c-a55467e48d0d)(label(+))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 28))(sort Exp))((shape(Concave \
         28))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         c0334a4f-31b5-4b8f-b4c9-55ced3f4124f)(content(Whitespace\" \
         \"))))(Tile((id \
         8c478c82-1e99-48eb-aa0f-1c238eed0258)(label(65))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         9c5f979e-bd34-4b1b-af99-d98d026a3d1a)(content(Whitespace\" \
         \"))))(Tile((id \
         287112fa-2f42-4fed-b61b-8f975766e1c4)(label(+))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 28))(sort Exp))((shape(Concave \
         28))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         fc23dfc9-cb1b-4ed1-abc6-17ce6345a885)(content(Whitespace\" \
         \"))))(Tile((id \
         e3ed96be-4de3-4683-9e19-d0f9964d5882)(label(70))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         e03427f5-6ac6-4466-87c7-efa69fad5f14)(content(Whitespace\" \
         \"))))(Tile((id \
         657f3f6c-2aa8-4b09-96d7-ca66d025f9ca)(label(+))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 28))(sort Exp))((shape(Concave \
         28))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         ac354997-5ae8-4c29-87af-58be4268a0df)(content(Whitespace\" \
         \"))))(Tile((id \
         daadeb39-6f63-4fab-b69d-7efd906b8f62)(label(20))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         4fa7e4f8-dc8d-4dc7-b8fe-67f8c3efe9c4)(content(Whitespace\"\\n\")))))))))(Tile((id \
         6d4e314c-fc9b-4d3b-9569-2adc07a60f15)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         44f9f4c3-fe29-4312-948f-5c8008d0ad4b)(content(Whitespace\"\\n\"))))(Secondary((id \
         aae58cfd-e255-4b06-bc73-21f21f83173b)(content(Whitespace\"\\n\"))))(Secondary((id \
         6a1ea244-3cd6-44b6-b75d-f49288c54d95)(content(Comment\"# PremiumSale \
         resets streak after claiming #\"))))(Secondary((id \
         ffc61383-d495-4251-ae01-d0ae1c8c2694)(content(Whitespace\"\\n\"))))(Tile((id \
         053704e0-54a0-4532-a04e-7e45d0a9b375)(label(hint test end))(mold((out \
         Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         62086c96-6080-49e0-ba92-5b560fe2561a)(content(Whitespace\" \
         \"))))(Tile((id \
         a9dad177-6f89-46b3-a56d-3d4582cb53b9)(label(\"\\\"PremiumSale resets \
         streak to zero\\\"\"))(mold((out Exp)(in_())(nibs(((shape \
         Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         854ab673-174a-4c75-af77-87199b812570)(content(Whitespace\"\\n\")))))((Secondary((id \
         a84e1ca3-bed5-4b2e-b9f6-f8b192f656ca)(content(Whitespace\"\\n\"))))(Tile((id \
         c872cdb2-f16e-4d15-9bae-da765dd4b070)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         5e7d6631-077c-423d-9bef-b1deb8b68441)(content(Whitespace\" \
         \"))))(Tile((id \
         a2ddfe69-0790-44d3-881c-7f2b542f400b)(label(h1))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         7cbb8709-0e0a-4205-a6ca-c046c3211234)(content(Whitespace\" \
         \")))))((Secondary((id \
         c35172b2-4799-4988-bc72-1ef6227972e6)(content(Whitespace\" \
         \"))))(Tile((id \
         35fdb223-2091-4297-b45e-7aa9d5467570)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         d8e2d3ac-c31e-43a0-bc54-b384608afefa)(label(crop))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         9664f96b-87be-4585-9a39-5588f0a2dc81)(content(Whitespace\" \
         \"))))(Tile((id \
         2fec6c34-555c-4ff5-81c9-d1a48c7b428e)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         a2d47824-aa87-4713-9952-29c5396ccf07)(content(Whitespace\" \
         \"))))(Tile((id \
         0a8fce23-e2ab-4f2f-b07b-e6acda9c5202)(label(Moonmelon))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         b9a7e5c6-c21b-46c9-9554-744f794304dc)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         73b3a681-9917-4930-99cf-2bfcca91d339)(content(Whitespace\" \
         \"))))(Tile((id \
         67ffd463-a462-485e-a6f7-f89657af5adf)(label(quality))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         91709290-146d-4abf-a276-39d197bc4d9a)(content(Whitespace\" \
         \"))))(Tile((id \
         d1f221f1-4e62-4488-93ae-8eea9451bb31)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         9b2db34f-5769-4dae-a7e1-1ba6ffc79f86)(content(Whitespace\" \
         \"))))(Tile((id \
         d53888fc-af23-4c34-8eec-3fda1443f375)(label(Gold))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         d631e3ab-d4c4-453a-9fcf-191e53be1d51)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         f546ae47-3fb5-455a-ab41-0cc71c96b665)(content(Whitespace\" \
         \"))))(Tile((id \
         3268cb0a-8b75-46c5-9771-5af76a9a6d64)(label(quantity))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         76628018-9b3f-4e43-96c5-426337a67bdd)(content(Whitespace\" \
         \"))))(Tile((id \
         4dc2debf-d919-4426-9cc0-47beb283cb85)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         f91ca777-f9e9-47d6-982d-3a6d93408a4a)(content(Whitespace\" \
         \"))))(Tile((id \
         951007a5-87d0-4a3a-84c3-8dc6cdd1d4f7)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         a5aefd6b-dbb2-438d-b9f8-90dae9fedebc)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         8ad98eaf-36c0-49df-ac31-11c1d0f8f542)(content(Whitespace\"\\n\"))))(Tile((id \
         60d7383f-e356-4a15-a4fb-b2a3c27a59d5)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         8cf730cf-2868-4a8d-86c1-80ab11c5ae49)(content(Whitespace\" \
         \"))))(Tile((id \
         2105175c-4cc3-4cc8-bdb5-b5828f0be2e7)(label(h2))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         98e27747-0b22-4592-997f-4ee5e749134b)(content(Whitespace\" \
         \")))))((Secondary((id \
         b8f6bae4-331f-4c80-9458-29389e942799)(content(Whitespace\" \
         \"))))(Tile((id \
         97777ceb-5823-4db4-8788-433603e09682)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         f2d95f15-03dc-4f82-9bf7-664435f4ac3e)(label(crop))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         7b475b2a-479d-4b13-8288-d9e2a6b9a525)(content(Whitespace\" \
         \"))))(Tile((id \
         25f774fd-1fe1-4c90-9395-c5607acc479f)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         92a422d7-61a0-4223-a4ec-cfd33dafc464)(content(Whitespace\" \
         \"))))(Tile((id \
         b2d94a8a-91df-4034-b54f-bdf57bd7b2f0)(label(Starfruit))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         bd62facd-4a8b-4582-9626-85ca91638288)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         3ac23786-334e-4ace-9ab4-7f303a94a8f5)(content(Whitespace\" \
         \"))))(Tile((id \
         bfb014b4-c0d1-4e4f-966d-7ff18bb94813)(label(quality))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         11d22437-1139-4c5e-9841-0097b13f8656)(content(Whitespace\" \
         \"))))(Tile((id \
         faeeb2f7-820b-4f1d-9622-b079776dd983)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         ddb12158-2bc0-47bd-85d1-4ed03aec05a1)(content(Whitespace\" \
         \"))))(Tile((id \
         508fa46c-ac67-4000-a5ca-2cb428ccc27e)(label(Gold))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         6ab4889f-f0a6-4314-bf9d-c0201f9f3c73)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         7f7ce2bb-a0bc-4a81-ad26-a2d74b5b64f2)(content(Whitespace\" \
         \"))))(Tile((id \
         d17d84dc-18c1-41d8-8dcd-b10a336fabf6)(label(quantity))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         1f88be93-01ae-4d94-b1a6-583341a675a2)(content(Whitespace\" \
         \"))))(Tile((id \
         b31d8f3f-0e94-47bf-be83-a3741168a1da)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         e2b8bf23-1fce-40ff-be56-d122733b7979)(content(Whitespace\" \
         \"))))(Tile((id \
         5af07ed4-5f6b-4d56-92cb-e2fe11a75072)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         01b976c0-1436-4e1d-8527-30485fc0f91d)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         ee8730b3-ff5f-4ba9-a69c-e22c979ed839)(content(Whitespace\"\\n\"))))(Tile((id \
         5b497979-0a4a-48fb-bb74-822be53fd0f2)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         9d242f4a-fd84-4b98-b0db-3b89c688a4fb)(content(Whitespace\" \
         \"))))(Tile((id \
         45367e53-c793-4876-91dd-eaee701e9859)(label(h3))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         f7750bed-0394-4ce3-8d4a-2141a2d5716a)(content(Whitespace\" \
         \")))))((Secondary((id \
         6f3f829b-3a2e-42f2-9f93-33ac356a55ee)(content(Whitespace\" \
         \"))))(Tile((id \
         9bac64c0-18a6-40e6-99b6-47b1983c9ce9)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         685aba69-732d-41e0-8649-f9552f3d2227)(label(crop))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         b046b0a6-931b-475d-8861-048485ea4db7)(content(Whitespace\" \
         \"))))(Tile((id \
         5b6715d8-4ad2-4a40-88ab-8622c73654cb)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         e4e82bc8-1555-453b-a7bf-7745ccd4c6f0)(content(Whitespace\" \
         \"))))(Tile((id \
         7af5e397-4c6d-4748-bb4a-02dfe5ded147)(label(Nightshade))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         829668b5-c438-4ff5-9881-693f01c9b4df)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         5e282689-a5cf-4753-83bc-b0c292b9e2e3)(content(Whitespace\" \
         \"))))(Tile((id \
         534ccee5-04d0-41a7-9728-9d3c50850d5e)(label(quality))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         31c8b5f9-921b-42b7-875e-a25b6c425b14)(content(Whitespace\" \
         \"))))(Tile((id \
         1f914c8c-738a-4741-a3b5-5aa335ff4caf)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         e14bace7-dbe2-406d-a48f-b07dc9a3c718)(content(Whitespace\" \
         \"))))(Tile((id \
         c0373fbf-53a6-4d3a-809a-3991a99eb244)(label(Gold))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         89379b07-7127-47ea-b75c-0ffd0020696a)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         e08a76da-42c9-42bb-9cb6-08e21468a862)(content(Whitespace\" \
         \"))))(Tile((id \
         ac399040-6534-4bbd-b872-2df44bfec8f9)(label(quantity))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         252017db-3070-4258-86c1-f1120b94a00a)(content(Whitespace\" \
         \"))))(Tile((id \
         5e52bf97-fab2-4efe-b979-13c88ce0a821)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         94c340ac-bc4a-4d76-a6b6-653d97a78ec2)(content(Whitespace\" \
         \"))))(Tile((id \
         22f3dfea-182f-4e25-8006-cf4ff216b2cc)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         dfe20947-f449-424f-9411-ffe889890e66)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         51a9a74f-76be-46a8-844f-243dfea15f33)(content(Whitespace\"\\n\"))))(Tile((id \
         4d029dc5-b10b-48de-aca1-786f05e3d372)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         713c8ea5-aa6d-45f4-8766-6052ce9ba6ab)(content(Whitespace\" \
         \"))))(Tile((id \
         effd1714-419e-4c48-bdcf-73647595317e)(label(ledger))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         2847f3cf-431a-449d-9f56-8e2ff60ccf83)(content(Whitespace\" \
         \")))))((Secondary((id \
         c99153ae-e372-4cfa-98b9-b53cd3075bd4)(content(Whitespace\" \
         \"))))(Tile((id \
         c94446a7-9af4-46fe-9a04-6de812315d16)(label(run))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         a2545a45-c025-4d9f-b128-6683f24f0a44)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         a2e24be1-cce9-4b0c-8dd9-c9bc49054073)(label(initModel))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         a7f48665-223b-417a-945b-b168173f044d)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         543372f5-cfe3-4b20-8082-660e2687913b)(content(Whitespace\" \
         \"))))(Tile((id 0f5d95e7-dd65-4e86-8b00-d5e060d34dff)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         0a4953a1-bc17-42ae-88c8-d7c76db7b115)(content(Whitespace\"\\n\"))))(Tile((id \
         78c5c28e-4127-4cc0-9ac7-9798f752c762)(label(RecordHarvest))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         d467f548-9d2b-4d48-8975-121312bbc527)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         a47ba63f-2449-42f1-9217-822c3cd78837)(label(h1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         1a4b9770-a97a-4dad-8a19-25a15d29c106)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         997fcebd-b4f1-4d99-aabc-9bb8072bf3d5)(content(Whitespace\"\\n\"))))(Tile((id \
         6803a7d1-03dc-442e-bb42-e47b61f5056f)(label(RecordHarvest))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         0b209255-2736-4ac5-96a4-487f9f652b37)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         4954d940-1b64-4817-a008-5caf602954ea)(label(h2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         a43395da-569e-4c0c-8862-583a436d06e0)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         b3ebd53c-1c77-42b7-8dfd-6a79853d009d)(content(Whitespace\"\\n\"))))(Tile((id \
         7ba7aa08-f7a6-4445-a3da-1ba84160650e)(label(RecordHarvest))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         fef4524d-fc08-4312-8d98-77d89d16494f)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         311bd8a3-d635-4245-9997-ef7ac894ada9)(label(h3))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         f0121826-9fa0-487f-80dd-d3f3050f038c)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         58b4f40c-e9ff-4122-aeee-c4800a34d6a2)(content(Whitespace\"\\n\"))))(Tile((id \
         916e9c45-3467-443f-96a9-5f05e6d296e5)(label(PremiumSale))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         8004a82e-bad3-4bd3-ba38-af971ba84f07)(content(Whitespace\"\\n\"))))))))))))))(Secondary((id \
         7e835b69-f5e3-44b2-8cde-a2810d07d60a)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         d7e1c9aa-b41d-481a-929e-d1702ef59b5e)(content(Whitespace\"\\n\"))))(Tile((id \
         83620d21-694a-42c6-b6b0-0d69f7ebbd2a)(label(ledger))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         90f3a401-b377-41bc-aaec-335f4c60428f)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         3eb60308-21c5-4206-9f50-2ec700fc8855)(label(streakBonus))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         d2fa0346-09cd-420b-bcc5-b06970e2dd30)(content(Whitespace\" \
         \"))))(Tile((id \
         2dff565d-fedd-411f-b8be-b42a2cd9f6a8)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         dd4e5f03-0808-46f0-af38-2ff8be224f02)(content(Whitespace\" \
         \"))))(Tile((id \
         a482c40b-2d4c-4cb0-9d37-afe3ef1f6c99)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         9695935f-0eec-4299-95b5-827654a89431)(content(Whitespace\"\\n\")))))))))(Tile((id \
         0fe3c893-ef2e-4188-832c-17b2b8c57968)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         f452b152-81ee-4a5a-84aa-ce432f8c9a06)(content(Whitespace\"\\n\"))))(Secondary((id \
         d88c0619-07b1-4203-afac-3afeb0ee4426)(content(Whitespace\"\\n\"))))(Secondary((id \
         f1521be6-ed95-42b4-9cb4-e154f12f0f58)(content(Comment\"# PremiumSale \
         with no streak gives zero payout #\"))))(Secondary((id \
         d1e6828a-d07b-40e4-b1fd-608876c90849)(content(Whitespace\"\\n\"))))(Tile((id \
         458b8b00-eed8-4624-8231-9f5137c84235)(label(hint test end))(mold((out \
         Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         89da730a-3c2e-4b63-85aa-30d4df6fbaff)(content(Whitespace\" \
         \"))))(Tile((id \
         6f812331-0ec7-4c51-a230-c2112edaf5aa)(label(\"\\\"PremiumSale with \
         zero streak adds nothing\\\"\"))(mold((out Exp)(in_())(nibs(((shape \
         Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         6a00d886-e809-408f-ba31-4cb6b1f160ea)(content(Whitespace\"\\n\")))))((Secondary((id \
         8e649eaa-00fc-4b11-b915-ffe3077c9f8f)(content(Whitespace\"\\n\"))))(Tile((id \
         0c66e9e3-50b6-453b-b354-cc847b867ebb)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         f26952fb-c099-41b4-918a-0e9c5bf4e056)(content(Whitespace\" \
         \"))))(Tile((id \
         fc7aeb7c-1534-43c8-9b7a-80b4fa66ef15)(label(h))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         4e43774d-feec-485c-867b-5655a6d28634)(content(Whitespace\" \
         \")))))((Secondary((id \
         b1eec68b-49f2-45dd-b230-5233451943c9)(content(Whitespace\" \
         \"))))(Tile((id \
         af363108-9fd7-4b7f-85ed-06c09bc8725d)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         cc1f12b6-3192-4e40-b188-2908635a65b6)(label(crop))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         cb0efb4d-6e21-4ee4-b211-5e622cb0272a)(content(Whitespace\" \
         \"))))(Tile((id \
         5ff97942-b668-4300-8441-572048a516bf)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         f1312f83-eb04-4068-bb0a-027f31971b75)(content(Whitespace\" \
         \"))))(Tile((id \
         42f4a606-0c67-4ec5-8f5e-20e38f276129)(label(Moonmelon))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         06059368-4bb2-430c-93a7-54f13638a3a4)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         dae9b316-f272-45c2-bba4-dad3323de875)(content(Whitespace\" \
         \"))))(Tile((id \
         dfa133f1-d9d2-4e03-99d5-43d8f56f262e)(label(quality))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         d81550f4-e48e-43b3-8855-74f58420e7be)(content(Whitespace\" \
         \"))))(Tile((id \
         33e239f2-b550-411f-b158-244a3684fb11)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         5357fe21-1822-4be4-925c-d5c66d37e574)(content(Whitespace\" \
         \"))))(Tile((id \
         63688f1c-0ad2-474c-9f86-0a40d0092481)(label(Bronze))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         79a5813c-2430-4426-921a-87dcc455140f)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         f7f0ce07-765f-4b65-a4c9-1fdd0d98ccc2)(content(Whitespace\" \
         \"))))(Tile((id \
         ccb2f49f-4a88-4d8c-a46e-b858870eb69e)(label(quantity))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         8f12191c-a9d6-4472-8780-38c7002057b4)(content(Whitespace\" \
         \"))))(Tile((id \
         dfac38db-174e-4246-828a-fdda0be3e5df)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         399a3c46-4c7c-4c00-8438-089603a6f0e8)(content(Whitespace\" \
         \"))))(Tile((id \
         1cb935a1-b7bf-4af3-a636-1913248b0b46)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         08ed744b-f10b-4a1c-a1cf-b1c0fb067db1)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         e85e0b85-f5ff-46eb-9d04-f7f3104bfcaf)(content(Whitespace\"\\n\"))))(Tile((id \
         abb3a40d-51aa-477c-a98f-513e70229775)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         858457a3-dc5b-43bc-af50-3ed1add8d90f)(content(Whitespace\" \
         \"))))(Tile((id \
         dc4e1e06-0b5e-4091-86bb-971f1fd673d4)(label(ledger))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         e0e52dbc-8f12-4a8b-bfab-abb9171cc26d)(content(Whitespace\" \
         \")))))((Secondary((id \
         724d25e0-c6da-4c3b-b14f-0813a0eb1dc1)(content(Whitespace\" \
         \"))))(Tile((id \
         12b956a3-cef2-498b-968f-421db3ff0430)(label(run))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         6bbfa772-151a-4adc-8b26-8ce291890363)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         cd526ace-51ed-435a-9849-ed05032ef5eb)(label(initModel))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         7df098a7-1eaf-4454-9aa3-20750ed106e7)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         396d865d-d979-4a21-bd0d-8b362e31d482)(content(Whitespace\" \
         \"))))(Tile((id d32a027c-b6b3-454d-a16b-f6d545aaa96f)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         51d6d7ef-5942-4ae5-82eb-6503d2b76e9f)(content(Whitespace\"\\n\"))))(Tile((id \
         84ffe6c0-786d-4e18-8a87-bec0a49468bd)(label(RecordHarvest))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         07f36357-debc-4729-9a0c-86503ec2b336)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         e5679e61-5079-4171-9fcb-7b41029eeae5)(label(h))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         3d69472d-d232-43d3-bd7e-8602268389bd)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         a794180d-ec53-448e-bd98-c528dc6ac7ed)(content(Whitespace\"\\n\"))))(Tile((id \
         4a6b5ea3-00d3-47c5-b811-7e0ddec06311)(label(PremiumSale))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         3f5e26fd-5bc8-4626-a967-79dd779eded3)(content(Whitespace\"\\n\"))))))))))))))(Secondary((id \
         97a0c42f-5f0f-4125-b878-b9b37f5da451)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         eb241c55-108e-4bcd-88b9-8a7a9de1ba4a)(content(Whitespace\"\\n\"))))(Secondary((id \
         11da2d86-f0f8-4199-9e5f-c463e9185e4a)(content(Comment\"# streakBonus \
         was 0, payout = 0 * 1 = 0 #\"))))(Secondary((id \
         630d0bd2-0d2e-4770-a167-bf26702ec1fd)(content(Whitespace\"\\n\"))))(Tile((id \
         52809aee-98a6-4618-970b-15c49ff850d1)(label(ledger))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         40912bff-32fd-4f71-8c83-dedb6c8c74d8)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         f095382a-5f86-40fb-9b05-2ccec646c39f)(label(totalValue))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         fbfbdb74-7bfe-4ffe-b6d1-11e41cd02794)(content(Whitespace\" \
         \"))))(Tile((id \
         02246ba5-2265-4322-9114-7f3bb633dc0f)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         b9a35683-4bc8-4841-97ca-004d6ff700d6)(content(Whitespace\" \
         \"))))(Tile((id \
         6022560a-82a5-4d1b-b5f7-2daf0474af78)(label(15))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         3e78bb25-ee5c-4ffc-a77b-49baf11c0d56)(content(Whitespace\"\\n\")))))))))(Tile((id \
         85cb7f8b-4356-4d60-9bb2-5b0ce9bc0d46)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         cd21e83d-43f3-4ff7-8874-946f7edadd24)(content(Whitespace\"\\n\"))))(Secondary((id \
         d9e682a2-9311-4d02-be5e-4c3e9fcd93e2)(content(Whitespace\"\\n\"))))(Secondary((id \
         56a69158-92f7-4e0a-8287-0c7bdbb8cd15)(content(Comment\"# Demo: \
         Premium sale harvest day #\"))))(Secondary((id \
         1e738b90-b0e3-4553-9450-eb63dd33487c)(content(Whitespace\"\\n\"))))(Tile((id \
         770c5f8f-3d61-49c1-9c32-fd1adae3212a)(label(run))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         f3505135-4b7f-42a5-8847-c146a087b1e0)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         9c7cf45a-b4b8-4c88-be6d-721c14fcacee)(label(initModel))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         eb8cefeb-77f7-4a92-aafe-6b30ed750a28)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         c09b699f-7143-43ff-b6ff-550c2ffec38f)(content(Whitespace\" \
         \"))))(Tile((id ccde1381-44f6-4ea0-92b5-66b6d0c32194)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         8774c98a-10c9-4a48-a1c1-e80dc84595dd)(content(Whitespace\"\\n\"))))(Tile((id \
         0efba890-a04f-46f8-96ed-4fa3244b3e33)(label(RecordHarvest))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         965e886c-4b51-49a0-96d0-079e7eac911e)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         9833a2f5-b5a7-465e-b3d1-516ee4f2f101)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         56ede180-cdb9-4833-bab4-2d1d5ac21e27)(label(crop))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         d5384663-42a6-4227-93fe-97323d11863f)(content(Whitespace\" \
         \"))))(Tile((id \
         8ef67d07-e282-466d-a19c-deb10727af10)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         fa544053-9514-4b45-8da4-aa917436f5c1)(content(Whitespace\" \
         \"))))(Tile((id \
         a1a4cbd1-e179-4f48-984d-2310cded2016)(label(Nightshade))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         d04e7bdb-2a19-4022-a7d0-706cd400d041)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         ecb429a7-ded2-42c2-b26d-9b2ea2975398)(content(Whitespace\" \
         \"))))(Tile((id \
         61d105a2-bdf1-4835-b777-4475071b997a)(label(quality))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         86ad4ff3-b123-493b-8198-75c801684cf5)(content(Whitespace\" \
         \"))))(Tile((id \
         ea914c38-9fbe-4605-b12a-d9d3c824d07d)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         47bd0abd-5c33-41ee-9e53-a84a8f790fd0)(content(Whitespace\" \
         \"))))(Tile((id \
         9db47044-07e3-4cd6-9379-9bd4c2a34859)(label(Gold))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         c0e4b105-719d-4786-b02a-bb90f6ea6c15)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         23b0fdf0-f4f4-40ba-910a-dc04d5289e38)(content(Whitespace\" \
         \"))))(Tile((id \
         494d2c35-952a-4258-b163-6a69131be1ea)(label(quantity))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         d7a38ceb-79cc-488d-b548-64041e20bd08)(content(Whitespace\" \
         \"))))(Tile((id \
         c508b622-f1c3-42de-bce0-8582acf4b955)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         6eec6cb1-c728-4ca3-a50a-becb1f10fcaa)(content(Whitespace\" \
         \"))))(Tile((id \
         529aca98-493a-43a1-bed1-045938c34ef1)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))))))))))))(Tile((id \
         2c2c9833-2eab-4e19-80a5-d60b1bd44929)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         8b7a9e89-0a07-4813-9f4e-82292e070bb0)(content(Whitespace\"\\n\"))))(Tile((id \
         d97b194c-26d9-43b1-94d6-3a17c2f3895f)(label(RecordHarvest))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         5141b044-1703-4a05-9cd8-fff4db057296)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         8d10aae2-9e3d-4d9c-ac1c-ae7668d84c44)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         78a12e42-293a-48c3-b8f5-78595e3f1445)(label(crop))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         91aa246e-1528-467a-b345-d9d20203bae8)(content(Whitespace\" \
         \"))))(Tile((id \
         e435bb64-9019-40c2-b73a-78d574b77cf9)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         564c7845-1c52-4721-9022-007dae85a689)(content(Whitespace\" \
         \"))))(Tile((id \
         4fed35a1-7ccc-48ad-bd86-6b0cf0295aa4)(label(Starfruit))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         b90729f6-904f-4015-91c2-3b2fc63dee89)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         254e6a58-0727-450f-b1fd-2fb391bdebfa)(content(Whitespace\" \
         \"))))(Tile((id \
         122657d2-0491-43bc-a84d-f5fd1d51da64)(label(quality))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         d406cd7e-36e0-48b8-8199-c714b17ad003)(content(Whitespace\" \
         \"))))(Tile((id \
         c12cf937-eb3b-4749-a798-c0a79b6aacab)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         8cb22d28-16f6-4b5a-8595-a41bd995743c)(content(Whitespace\" \
         \"))))(Tile((id \
         bd4cb6e5-2bb7-4ff2-af04-68f63fc0325e)(label(Gold))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         35006fb0-cbe7-483b-8dc3-6138b29d4167)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         8121c9c2-d173-4378-9802-dc837aa34412)(content(Whitespace\" \
         \"))))(Tile((id \
         931da1a3-65fd-45c2-9b33-13e3c3003650)(label(quantity))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         4643e455-a7de-46fd-83a5-c5432daa2a93)(content(Whitespace\" \
         \"))))(Tile((id \
         4f8c16aa-01e9-436d-b6f8-d23495b8970a)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         6c6ff799-fc19-45d1-bbca-0b373a9d51ad)(content(Whitespace\" \
         \"))))(Tile((id \
         c39b27f9-4566-490a-b55d-128b22a72804)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))))))))))))(Tile((id \
         87c1d769-c786-4894-b8ee-ac7e19ab99ed)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         0c84c09a-750c-4765-8a25-4231f00cee8d)(content(Whitespace\"\\n\"))))(Tile((id \
         26fa8c06-618f-480d-8d99-cb22c9a2575e)(label(RecordHarvest))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         efad0d0c-28dc-4056-ad5f-6dc5c0604fe1)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         dacec53f-a158-4ce1-849b-2e9f97cc5258)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         5926a4e5-d943-45a7-b4d8-b3db59a01534)(label(crop))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         acec8d42-4fe1-475c-9a11-93bf375272fa)(content(Whitespace\" \
         \"))))(Tile((id \
         973ef83f-498e-4b4b-a29f-57754184744b)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         e88d2a17-31a8-435f-97d4-9498ac503094)(content(Whitespace\" \
         \"))))(Tile((id \
         c2895514-d355-4858-b321-0991cd9f39d4)(label(Moonmelon))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         2404d2cd-affc-4a17-9cd6-17a5b80ae632)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         bab63ae9-db1a-40cc-8951-1bcf4261148e)(content(Whitespace\" \
         \"))))(Tile((id \
         e87773af-dd25-454f-bace-1d6914afa0f7)(label(quality))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         7653f993-73b6-4807-b51c-6f422a488bb6)(content(Whitespace\" \
         \"))))(Tile((id \
         d96e435f-dcc2-421c-8033-31fcbfe4b118)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         845df0b0-bc4e-41f2-a8e7-db2e455643af)(content(Whitespace\" \
         \"))))(Tile((id \
         d40305c4-4dd8-4123-a2d2-e53d7c8cf3db)(label(Gold))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         deadbe3b-50fb-4c74-a27d-220b1143ca4d)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         c1781948-2ed8-4c72-9b32-be4da16ec29f)(content(Whitespace\" \
         \"))))(Tile((id \
         9468cbdf-d309-49f9-ba51-e1abca2863d7)(label(quantity))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         0017df40-c60c-45a3-8ff9-61217a68c72e)(content(Whitespace\" \
         \"))))(Tile((id \
         e205247a-a692-45c1-9faa-6479d541eea5)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         726be26d-a7cd-4ec8-a531-72d8df885734)(content(Whitespace\" \
         \"))))(Tile((id \
         79e57470-9d72-4ee1-b622-08b3b948b075)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))))))))))))(Tile((id \
         61934794-ee1a-4653-8068-f2c02af739e6)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         82178fa5-c817-4ee8-be17-592560472b57)(content(Whitespace\"\\n\"))))(Tile((id \
         70648ad8-4644-4cb4-bd87-2f3adee9ba24)(label(PremiumSale))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         3a14fd3c-d4e7-4343-9638-4c31d1686310)(content(Whitespace\"\\n\"))))))))))))))(Secondary((id \
         55369a07-3871-4af8-ac86-f2e4c076b851)(content(Whitespace\"\\n\")))))";
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
