let out : string * Haz3lcore.PersistentSegment.t =
  ( "Study / tutorial / 02-functions-and-cursor",
    {
      segment =
        "((Secondary((id \
         9ad701c1-7427-414e-9d4c-9dd23e6ca040)(content(Comment\"# PROBES \
         TUTORIAL - PART 2: FUNCTIONS AND THE DYNAMIC CURSOR \
         #\"))))(Secondary((id \
         1e2ef2cc-ebe1-4d53-87ad-673756632582)(content(Whitespace\"\\n\"))))(Secondary((id \
         edc7cfaa-ed5d-4970-84cb-d80e84b01eef)(content(Whitespace\"\\n\"))))(Secondary((id \
         430b2fe3-28d9-469f-aff2-5295194f7af7)(content(Comment\"# When a \
         function is called multiple times, each call #\"))))(Secondary((id \
         13d7fdcf-3390-49bb-8ff9-969a160dd322)(content(Whitespace\"\\n\"))))(Secondary((id \
         16d5a52f-f0d5-4f04-9930-fdb926dbe2cf)(content(Comment\"# generates \
         its own sample. Let's see what that looks like! #\"))))(Secondary((id \
         fdb57b24-02c6-47c9-8023-b466b16ae48c)(content(Whitespace\"\\n\"))))(Secondary((id \
         2b829fba-2686-444e-8abb-8992629292ab)(content(Whitespace\"\\n\"))))(Tile((id \
         0ac24b37-2f77-46c4-9711-4259487e740a)(label(type = in))(mold((out \
         Exp)(in_(TPat Typ))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         32e5ff51-fe14-4417-88c4-8050aba4db59)(content(Whitespace\" \
         \"))))(Tile((id \
         86bfcee5-affb-4576-967c-4b40dcdbad44)(label(MoonPhase))(mold((out \
         TPat)(in_())(nibs(((shape Convex)(sort TPat))((shape Convex)(sort \
         TPat))))))(shards(0))(children())))(Secondary((id \
         c5161c4e-07f5-4263-93eb-85eef5bfbf8a)(content(Whitespace\" \
         \")))))((Secondary((id \
         b8389619-ba0e-4a9b-bc80-f8b14be05928)(content(Whitespace\" \
         \"))))(Tile((id \
         475e1558-9730-4248-8ea3-d6c7edc78eb7)(label(New))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         33d7ffe1-4bb1-4804-b54a-43c55c8c6104)(content(Whitespace\" \
         \"))))(Tile((id \
         b754dca9-2d38-4418-97bf-2fbaa1c52e16)(label(+))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 12))(sort Typ))((shape(Concave \
         12))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         2474957d-20d0-451d-a063-b5adcd725709)(content(Whitespace\" \
         \"))))(Tile((id \
         04ad3528-abea-49de-849e-b669427700eb)(label(Waxing))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         712741d8-6802-4457-beda-e39db09e2a51)(content(Whitespace\" \
         \"))))(Tile((id \
         daf74c77-73d0-46ac-8f09-6c17333928f2)(label(+))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 12))(sort Typ))((shape(Concave \
         12))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         cee9db43-8186-4ded-b7b6-d8858bac7b8f)(content(Whitespace\" \
         \"))))(Tile((id \
         e4d3a035-3118-4941-8685-913530d6b921)(label(Full))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         879bfa13-a507-4e03-b9d0-9a7435270b42)(content(Whitespace\" \
         \"))))(Tile((id \
         2270d6cb-54db-4c90-8d04-37db693f76f6)(label(+))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 12))(sort Typ))((shape(Concave \
         12))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         1fcf8665-ff4a-40a6-b28a-85500567609c)(content(Whitespace\" \
         \"))))(Tile((id \
         73d43faf-18ca-497c-b7d0-fe6f01dbb164)(label(Waning))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         d3955866-78b1-4e09-87ae-0c405390374f)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         a2882a86-5975-4e65-880c-06027d4ce7e3)(content(Whitespace\"\\n\"))))(Secondary((id \
         2e20e81f-3500-4cae-af18-d5b989fae001)(content(Whitespace\"\\n\"))))(Secondary((id \
         2fcf2474-6141-4ee7-a028-d934874e7a2a)(content(Comment\"# Hazel has no \
         special function definition syntax. #\"))))(Secondary((id \
         27bcfe5c-e5db-42c6-a117-a5be1957f0d2)(content(Whitespace\"\\n\"))))(Secondary((id \
         c10d1bf1-d893-4f64-bf0f-744d25a53ad9)(content(Comment\"# We use \
         regular let definitions to define function literals, \
         #\"))))(Secondary((id \
         21462932-6bfc-4bdf-bfcf-7b817b1d68fc)(content(Whitespace\"\\n\"))))(Secondary((id \
         23b90331-f452-427d-9d9b-181a7dbd0ebd)(content(Comment\"# using the \
         syntax `fun <pattern> -> <body>`. #\"))))(Secondary((id \
         d85c7365-d9d8-417a-b47b-d76f2c653197)(content(Whitespace\"\\n\"))))(Secondary((id \
         348d1d28-87c2-488e-9b37-4163966e619f)(content(Whitespace\"\\n\"))))(Secondary((id \
         502cd434-137b-447f-88d3-aebfda10e096)(content(Comment\"# TRY THIS: \
         Add a probe to the `multiplier` variable inside #\"))))(Secondary((id \
         e1b93b1c-33de-4c58-a2eb-a9b43858adc6)(content(Whitespace\"\\n\"))))(Secondary((id \
         e7d768bc-39e0-485f-a6bc-0b210c538fdd)(content(Comment\"# the function \
         `watering_amount` below. When you click on the #\"))))(Secondary((id \
         4805121e-abf5-43cb-a453-5c937aa3ac0e)(content(Whitespace\"\\n\"))))(Secondary((id \
         ddb8afb2-2288-4bf6-b837-262ae5e6edc8)(content(Comment\"# sample, \
         notice the arrows that appear to the left. Click on \
         #\"))))(Secondary((id \
         cb86a773-0df5-4dc3-a456-ccde979c9cb7)(content(Whitespace\"\\n\"))))(Secondary((id \
         ff803552-a7a6-4898-8a75-863fc6d1efba)(content(Comment\"# these \
         arrows, or use the left/right arrow keys, to navigate \
         #\"))))(Secondary((id \
         c4fca207-ae54-4f67-a99e-2a1d610b8ae6)(content(Whitespace\"\\n\"))))(Secondary((id \
         e9dc3855-e337-48d5-93d7-cd085104884d)(content(Comment\"# between the \
         three different samples collected. #\"))))(Secondary((id \
         4c7ec0b0-ebd8-4ef2-a47c-d0c978688556)(content(Whitespace\"\\n\"))))(Secondary((id \
         14dfcfef-1198-47a9-9b12-f8d598714ef4)(content(Whitespace\"\\n\"))))(Tile((id \
         f1fab747-7071-4634-aa65-784bf16d1852)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         33ccb0b1-c4d0-466a-86b1-881fc1d6ee0a)(content(Whitespace\" \
         \"))))(Tile((id \
         e8bd35c3-0a6a-4ddb-931c-6bbc42bce417)(label(watering_amount))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         95abf954-5247-4494-a6be-c4b949fdba03)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 24))(sort Pat))((shape(Concave \
         24))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         73090613-cbf5-4c9c-a0d8-8210b4b9ea2c)(content(Whitespace\" \
         \"))))(Tile((id \
         ff266fdf-ac99-4141-980c-95a1e32c3feb)(label(\"(\"\")\"))(mold((out \
         Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0 1))(children(((Tile((id \
         b829554b-e9e0-41ba-997d-19ff13d4d1c4)(label(Int))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         8869e5c6-6b69-4500-a384-b6cb57c493bc)(label(,))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 44))(sort Typ))((shape(Concave \
         44))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         a06f883b-5d7f-4ea9-8051-487ad937aef0)(content(Whitespace\" \
         \"))))(Tile((id \
         2dd4c524-7f46-44a3-8823-fe43f9bcdf3a)(label(MoonPhase))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children()))))))))(Secondary((id \
         f0505768-816a-4d84-99d0-9f9d915766cb)(content(Whitespace\" \
         \"))))(Tile((id \
         7695d7c4-790b-492d-ac67-6a63748df99d)(label(->))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 13))(sort Typ))((shape(Concave \
         13))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         1c6ca346-54e5-4031-b603-f27729a37bb1)(content(Whitespace\" \
         \"))))(Tile((id \
         a5038b2c-60a4-428b-873d-5a0b3174109a)(label(Int))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         ad8b02a6-edf3-478b-91ce-8fa82d2e0668)(content(Whitespace\" \
         \")))))((Secondary((id \
         8bca0c13-1517-4d70-8908-ac3580a1b26f)(content(Whitespace\"\\n\"))))(Tile((id \
         5a12ba33-61c5-434b-9a44-ea3dcb8b10a5)(label(fun ->))(mold((out \
         Exp)(in_(Pat))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         37))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         98b1764e-c824-466c-a59f-bb0cf8a28784)(content(Whitespace\" \
         \"))))(Tile((id \
         05ee85c8-3171-43ca-936a-6f66ff5b86e7)(label(\"(\"\")\"))(mold((out \
         Pat)(in_(Pat))(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0 1))(children(((Tile((id \
         78b99d0d-505f-4522-a4d9-6dd6f2ef0b65)(label(base_ml))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         c51bb0b0-430d-473f-8968-d6beebd6c5c6)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 44))(sort Pat))((shape(Concave \
         44))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         e322be38-34ea-456d-84fd-cac0ef8aa939)(content(Whitespace\" \
         \"))))(Tile((id \
         fd7d985f-ceff-44a4-a177-8be31a02c7ce)(label(phase))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children()))))))))(Secondary((id \
         d3b66abf-f6d0-4ad0-831c-decc50b7f26b)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         6617037d-9134-4078-85cd-ad03dcde1a54)(content(Whitespace\"\\n\"))))(Tile((id \
         252423ec-e2af-4144-8996-9c8bf58cf1a9)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         27493f8c-457b-41b4-b9f4-a9739e1c6268)(content(Whitespace\" \
         \"))))(Tile((id \
         4bda0f80-bbe5-4c65-bbfb-fe9173ee3f52)(label(multiplier))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         7f4f483e-8075-4479-9b47-9a1adabcd955)(content(Whitespace\" \
         \")))))((Secondary((id \
         ca2b0749-7662-4046-92c0-cda462ebb09d)(content(Whitespace\"\\n\"))))(Tile((id \
         6c6f0305-70fd-48bb-a352-b0fb4f9b4eab)(label(case end))(mold((out \
         Exp)(in_(Rul))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         71a30f40-28fb-44a8-bfb8-c082440aff6d)(content(Whitespace\" \
         \"))))(Tile((id \
         7cd54ee2-933d-4f69-8eea-dff89780f4b5)(label(phase))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         380471d3-6f9d-400a-8dc7-b6e895d6bac9)(content(Whitespace\"\\n\"))))(Tile((id \
         3c6c053b-6dd4-4a56-b8f1-2f7dc6f1b589)(label(| =>))(mold((out \
         Rul)(in_(Pat))(nibs(((shape(Concave 46))(sort Exp))((shape(Concave \
         46))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         6ae599ab-d9bc-4ff2-a304-1f35e86c2075)(content(Whitespace\" \
         \"))))(Tile((id \
         7b28b6e7-0dac-404b-9693-730b4e223b74)(label(New))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         47e6cc22-3e49-4261-9af8-faff9a65a902)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         3e8e31f5-11fe-49dc-9f09-8f98cf1b6c2c)(content(Whitespace\" \
         \"))))(Tile((id \
         8ed734b2-dec0-491c-970d-d21adc48e816)(label(1.2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         cc9b6f17-ab81-495e-8d91-5cf36c3bcb07)(content(Whitespace\"\\n\"))))(Tile((id \
         85985f7f-4a7e-4ca3-9ec4-01417ff9a4ca)(label(| =>))(mold((out \
         Rul)(in_(Pat))(nibs(((shape(Concave 46))(sort Exp))((shape(Concave \
         46))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         bf62ee69-4f97-40bd-945c-56aac3fe048b)(content(Whitespace\" \
         \"))))(Tile((id \
         948ed3a8-23d2-4026-8f33-d61e54c2b6e4)(label(Full))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         459201f4-b83e-40b1-96ff-19983c7f1850)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         0e33fff9-34e1-4a30-a0bf-68b032f3a30f)(content(Whitespace\" \
         \"))))(Tile((id \
         dfbf7aa0-da68-485e-a1fd-a54f3b1c2a09)(label(0.88))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         342cd68b-a6cb-4f20-a58e-a6cf81da7983)(content(Whitespace\"\\n\"))))(Tile((id \
         707bf2a5-1309-47b9-b71f-43875339fcda)(label(| =>))(mold((out \
         Rul)(in_(Pat))(nibs(((shape(Concave 46))(sort Exp))((shape(Concave \
         46))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         680b194a-b68b-46a1-a052-714c3d8335a0)(content(Whitespace\" \
         \"))))(Tile((id \
         f47ccbc4-9efe-4a7d-a906-d5a58ccfef3e)(label(Waxing))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         ff8256f5-fc45-4dc5-8512-b1b61defb915)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         6bfe3b2b-3f94-4db8-b0b8-a9adcd69e0cd)(content(Whitespace\" \
         \"))))(Tile((id \
         a60dbfff-e83b-42a4-8530-885c73a4c941)(label(1.1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         c45cb99c-92ad-4004-87c1-5fcfad7d882c)(content(Whitespace\"\\n\"))))(Tile((id \
         26ae8afb-43b9-4411-96f9-a0192652abd8)(label(| =>))(mold((out \
         Rul)(in_(Pat))(nibs(((shape(Concave 46))(sort Exp))((shape(Concave \
         46))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         7d209295-730b-49d2-b5e0-608783b98987)(content(Whitespace\" \
         \"))))(Tile((id \
         2de37037-56da-4a42-960f-0af10a44e002)(label(Waning))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         0d038b03-8c8a-4115-838b-00d9c8ec70dc)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         67a52da9-1976-41c7-be75-d70fe0213d09)(content(Whitespace\" \
         \"))))(Tile((id \
         1b5d1ba9-15ca-4b6d-93a9-72b68c977c33)(label(0.95))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         4d6c5c36-7d63-4028-af36-b7e50e1964bf)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         a6f6343e-5f1b-44e4-8c22-38b199c82d33)(content(Whitespace\" \
         \"))))(Secondary((id \
         600ebd88-d9cd-4148-ad8c-9b36629e2d7a)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         d3f24dce-33c5-47a6-841d-c132af679849)(content(Whitespace\" \
         \"))))(Tile((id \
         a6c0a0bd-bbe6-48a2-9a40-0e50d4feccef)(label(int_of_float))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         3605e7c1-9535-43b0-9c9d-d0e755923d40)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         22f3e353-e5d6-45db-9e78-037ec30fd723)(label(float_of_int))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         e7a7b1a4-61fc-490f-994d-f1a77864a74e)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         4802dffc-2782-4504-81de-25895e709e87)(label(base_ml))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         18776e72-35eb-412d-a3da-ad2836536c19)(content(Whitespace\" \
         \"))))(Tile((id \
         4b04396b-0088-4dac-af15-6d4feaf66ff5)(label(*.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 27))(sort Exp))((shape(Concave \
         27))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         fe1f4aaf-9141-43d1-97a2-93cd62679fc8)(content(Whitespace\" \
         \"))))(Tile((id \
         722b6adc-66d0-48c3-abc2-84d522276683)(label(multiplier))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         063e5886-d31e-4f98-a572-ed132d4ff43b)(content(Whitespace\"\\n\"))))(Secondary((id \
         a944e044-7d85-4b7b-8517-dee577373183)(content(Comment\"# Above: Hazel \
         uses C-style Function application syntax #\"))))(Secondary((id \
         5694b5eb-ffbd-4342-acbe-ad7c7139241c)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         a4e2952c-fc3a-42df-98a5-1f753b9aeeb8)(content(Whitespace\"\\n\"))))(Secondary((id \
         d9d98c3b-29fe-4ac6-85a7-ea5a682ad3cd)(content(Whitespace\"\\n\"))))(Secondary((id \
         4b36fc36-7c26-43cb-ad36-922823805bd8)(content(Comment\"# Now click \
         the samples for the 3 calls to `watering_amount` below. \
         #\"))))(Secondary((id \
         300a9235-42d7-4af4-b6f1-8388fd587e13)(content(Whitespace\"\\n\"))))(Secondary((id \
         e6612ae9-2304-4476-aa34-34f37dc60f57)(content(Comment\"# Notice the \
         sample for 'multiplier' above changes to /align/ with \
         #\"))))(Secondary((id \
         e685dc85-37c0-4f6f-b9ae-25357e86fde6)(content(Whitespace\"\\n\"))))(Secondary((id \
         22ee2efd-798e-44f8-9be9-dd0bf3b014ff)(content(Comment\"# the selected \
         call! We call this behavior the 'dynamic cursor', \
         #\"))))(Secondary((id \
         51e9f208-c99e-45ae-a02b-2b7c91b9e406)(content(Whitespace\"\\n\"))))(Secondary((id \
         07991fc0-b462-49b3-ae97-14353d0a2efb)(content(Comment\"# which aligns \
         probe samples to a particular step in an execution. \
         #\"))))(Secondary((id \
         25362849-f7e4-4b3e-bf9e-d3fbfa045004)(content(Whitespace\"\\n\"))))(Secondary((id \
         c6c8a5fd-c67d-4625-94fa-a07b6ed16263)(content(Whitespace\"\\n\"))))(Tile((id \
         d2e7bbe8-c0fb-4c77-90bc-c6820e2c5c1f)(label(watering_amount))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         482290cd-dbc9-43aa-acca-f09518f3837a)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         a6a96e2c-7121-42a6-b9e7-113ff7d37cca)(label(250))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         9a101680-c250-4bd1-9bca-9e140ddc3c7e)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         14fb8038-79c3-423b-a9f3-e35c5867a348)(content(Whitespace\" \
         \"))))(Tile((id \
         685c2acf-71a5-41c2-a775-6894be2d5622)(label(Full))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         909996c2-cf4f-4ac1-a709-db4f3aef2850)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         59c9f7d0-6112-4b12-83ee-051482001c70)(content(Whitespace\"\\n\"))))(Tile((id \
         48af81a0-f3c8-40d9-a69b-e8b181b917ce)(label(watering_amount))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         936a3ddb-44e8-4ed7-8e34-043ba2d5502f)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         67efc5db-452a-4852-8d5e-7ed0facd3b6c)(label(50))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         f07bfd7d-1f3d-4e59-bd89-e85c32c8f5d4)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         b357877c-1772-4c5b-86a7-c0cdc1c9c428)(content(Whitespace\" \
         \"))))(Tile((id \
         2ce3b0c5-56ab-410f-befe-a0114f109a69)(label(New))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         c2df6bd3-8635-4d52-8531-5bdb118f0a1e)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         0ebe599d-7f46-4275-a567-be5adb92b200)(content(Whitespace\"\\n\"))))(Tile((id \
         e6f01869-8103-4e5a-90f7-b38a1e71b768)(label(watering_amount))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         cb5fe821-8272-4816-8057-bae1037a6de6)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         8ae4d886-87b4-42bf-b58f-af915863c1be)(label(180))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         448effef-05b1-48e0-b133-31f5df5fb746)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         156fae10-f608-48fc-853c-f23a5b15bef0)(content(Whitespace\" \
         \"))))(Tile((id \
         c5f966f7-bf5c-4063-a0df-35c659f93791)(label(Waning))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         76b2e95b-ede7-494a-8bd7-18016474e1e1)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         cde99c76-927d-43f1-8c26-d67f48fdc664)(content(Whitespace\"\\n\"))))(Secondary((id \
         cbaba9ef-06f8-48d0-a725-a70d1be987d9)(content(Whitespace\"\\n\"))))(Secondary((id \
         c4ac8529-6719-4788-9f26-e39f8848b677)(content(Comment\"# Below is the \
         same function as above, this time with many probes. \
         #\"))))(Secondary((id \
         079ea221-3c1b-4bba-91dd-258e6e96baf5)(content(Whitespace\"\\n\"))))(Secondary((id \
         2b87053c-6322-4a29-8a35-402698402eab)(content(Comment\"# Select the \
         `multiplier` sample and use the arrow keys to move \
         #\"))))(Secondary((id \
         59677a3f-42b3-41f0-a154-776940f2d0d1)(content(Whitespace\"\\n\"))))(Secondary((id \
         6345111c-129a-4d7b-b82f-558368bc99ef)(content(Comment\"# through the \
         different values. Notice how this time, there are two \
         #\"))))(Secondary((id \
         8ffbaf0b-2409-43bb-bd49-b58968231b2f)(content(Whitespace\"\\n\"))))(Secondary((id \
         d68ac8d6-608b-4ec8-9ee0-a68ef221512f)(content(Comment\"# different \
         symbols next to the branches with no samples; \\226\\136\\133 from \
         #\"))))(Secondary((id \
         0cf6b70d-8acc-4260-a030-f7076d89f0f9)(content(Whitespace\"\\n\"))))(Secondary((id \
         9bfb4ea7-174c-440e-8eaa-8f24412e72ce)(content(Comment\"# before on \
         `Waxing`, which means never evaluated, and a new symbol \
         #\"))))(Secondary((id \
         742b7a3d-b793-40ac-aa97-028e775a2896)(content(Whitespace\"\\n\"))))(Secondary((id \
         b46a8519-a27f-400e-bf29-b418d2b11e72)(content(Comment\"# \
         \\226\\138\\150, which means there are samples, but they are not \
         aligned to the #\"))))(Secondary((id \
         0f77ee7f-2454-475d-8e2f-b4e71cac12a3)(content(Whitespace\"\\n\"))))(Secondary((id \
         701847c8-5c4f-4f6b-8d5c-ecdea649d57c)(content(Comment\"# dynamic \
         cursor (because of the `multiplier` sample you selected). \
         #\"))))(Secondary((id \
         2bb5b576-d45e-4872-9209-8a22671c7847)(content(Whitespace\"\\n\"))))(Secondary((id \
         fabc6b9e-b731-4550-8d4a-d75332249ddc)(content(Comment\"# Click on any \
         \\226\\138\\150 to align the dynamic cursor to that branch. \
         #\"))))(Secondary((id \
         e511c736-6f1f-4019-8652-5bd2acaeab35)(content(Whitespace\"\\n\"))))(Secondary((id \
         18e31f21-caf3-471c-8f39-a5051b4f5fa5)(content(Whitespace\"\\n\"))))(Tile((id \
         5c432bb6-c4e2-4871-b2ec-9710a98e7803)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         e5b73638-23c4-4879-8a7d-beef493bda60)(content(Whitespace\" \
         \"))))(Tile((id \
         7895acef-d3d3-47da-8e9a-33201bb2f87d)(label(watering_amount))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         2ae73f36-5b99-477d-806d-393b79f92311)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 24))(sort Pat))((shape(Concave \
         24))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         192c0f1c-b42c-4a63-af10-8bb062802fc3)(content(Whitespace\" \
         \"))))(Tile((id \
         c5ceb994-c738-46e1-989a-03f85f7336cd)(label(\"(\"\")\"))(mold((out \
         Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0 1))(children(((Tile((id \
         7499807c-ce18-40ca-80bb-938a5ad76717)(label(Int))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         3d24d1b3-fa54-4227-8310-eba4d09db984)(label(,))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 44))(sort Typ))((shape(Concave \
         44))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         58120796-4ff8-44b4-bfbf-5c1e045a4d20)(content(Whitespace\" \
         \"))))(Tile((id \
         9f6aca3d-860f-485c-b8b8-c88ffed0188e)(label(MoonPhase))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children()))))))))(Secondary((id \
         6e85f53e-17ae-48f5-a982-7b323210abcc)(content(Whitespace\" \
         \"))))(Tile((id \
         ac98a077-a240-4fdc-8fb5-c08a9769d0ab)(label(->))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 13))(sort Typ))((shape(Concave \
         13))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         e701c7c5-0155-46c4-b25f-7a7cd26b876f)(content(Whitespace\" \
         \"))))(Tile((id \
         6a83caf1-c005-4af4-8e3e-c212527db7ab)(label(Int))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         aeeba3fa-8762-491c-8c5e-c0b19faaae4d)(content(Whitespace\" \
         \")))))((Secondary((id \
         0ac6864c-1055-4911-a1aa-627ac048868f)(content(Whitespace\"\\n\"))))(Tile((id \
         02efae64-52ee-4c47-b7a2-a7dfb1f1f0a1)(label(fun ->))(mold((out \
         Exp)(in_(Pat))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         37))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         91b760de-1a27-49e2-a6cc-272288226662)(content(Whitespace\" \
         \"))))(Tile((id \
         8455fbd3-c06a-4b75-8168-ef6619979ead)(label(\"(\"\")\"))(mold((out \
         Pat)(in_(Pat))(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0 1))(children(((Tile((id \
         18529367-14e5-4db3-9b25-c6be57329bc4)(label(base_ml))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         8158e103-2e59-4111-b1bc-7cb7034b27f3)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 44))(sort Pat))((shape(Concave \
         44))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         e5e962ef-af50-4cb1-a62f-4a65f2054e21)(content(Whitespace\" \
         \"))))(Tile((id \
         d5bab42a-e865-4d8e-879b-90f8821f751f)(label(phase))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children()))))))))(Secondary((id \
         ed913f10-2a0e-4ce9-b39e-3679753d71a0)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         5f2153db-6905-4350-9391-45987ed7f3b8)(content(Whitespace\"\\n\"))))(Tile((id \
         f6d0555f-86c0-4cd1-8380-51d30432bc41)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         9e74cc73-7045-4211-ab43-719a1aa010b8)(content(Whitespace\" \
         \"))))(Tile((id \
         97f548d1-c6ba-4f84-a1c6-b65c73d805fc)(label(multiplier))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         f6dcec5f-ea34-44e1-84bb-429e103a10aa)(content(Whitespace\" \
         \")))))((Secondary((id \
         faef55fa-6f28-4a6c-afec-b531a21176d4)(content(Whitespace\"\\n\"))))(Tile((id \
         e15131ff-1c7a-4660-a198-1221299f25d4)(label(case end))(mold((out \
         Exp)(in_(Rul))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         3bdf7fa2-2994-4cb4-8dd7-4333ac15c27d)(content(Whitespace\" \
         \"))))(Tile((id \
         6de2113c-cba4-49aa-8408-47670511efbf)(label(phase))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         242cfa97-4d4e-4496-af7a-83e9415a39a0)(content(Whitespace\"\\n\"))))(Tile((id \
         304f5a6b-ae34-49e4-bf15-bc9dbfc9db01)(label(| =>))(mold((out \
         Rul)(in_(Pat))(nibs(((shape(Concave 46))(sort Exp))((shape(Concave \
         46))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         84faab97-7f84-42f8-8501-ebba3fb4637b)(content(Whitespace\" \
         \"))))(Tile((id \
         84624bc7-078a-45a8-b4a9-32e4626b7ede)(label(New))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         c282344a-022d-4f7a-8228-3f04102b7190)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         d990b6cc-f57f-4034-9321-2170a5986634)(content(Whitespace\" \
         \"))))(Tile((id \
         05300655-9d7b-4217-8e21-04b72b6123b4)(label(1.2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         b526a844-3895-4ec4-b759-9ffbc3e0993e)(content(Whitespace\"\\n\"))))(Tile((id \
         c11eca4a-fff2-4ec3-9049-caf98775b226)(label(| =>))(mold((out \
         Rul)(in_(Pat))(nibs(((shape(Concave 46))(sort Exp))((shape(Concave \
         46))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         35e82e42-2f26-4c95-8c7c-917385f543e4)(content(Whitespace\" \
         \"))))(Tile((id \
         eb044fe0-f0b6-428a-b86d-05dcc5b9b12a)(label(Full))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         8b1198f9-162f-47ba-b18a-a6ea6cc653e3)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         7511176f-6709-4084-9602-3753e850b3f8)(content(Whitespace\" \
         \"))))(Tile((id \
         f27ec8e3-d53c-42d8-9eef-b33733e43fa7)(label(0.88))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         3249acda-96e5-4704-b51d-7a00c8729624)(content(Whitespace\"\\n\"))))(Tile((id \
         5d818ee3-505c-4f6f-9476-605c6a8d4bd9)(label(| =>))(mold((out \
         Rul)(in_(Pat))(nibs(((shape(Concave 46))(sort Exp))((shape(Concave \
         46))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         775b327a-077b-45df-bcd6-8f683623fa13)(content(Whitespace\" \
         \"))))(Tile((id \
         7824a905-ac9e-495e-98a7-0605e398a868)(label(Waxing))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         9007f368-87fd-42b9-9a49-6bd495f1f803)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         0f12219b-7c4c-4993-8d4c-b117fa05f2ac)(content(Whitespace\" \
         \"))))(Tile((id \
         e43fcde9-097a-48ca-925b-949a41f7c67d)(label(1.1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         b44d8fad-86e5-4768-b576-33dab9ab8907)(content(Whitespace\"\\n\"))))(Tile((id \
         1d1f7907-2125-4252-9ef1-a6aa5003dfdb)(label(| =>))(mold((out \
         Rul)(in_(Pat))(nibs(((shape(Concave 46))(sort Exp))((shape(Concave \
         46))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         6f409671-a8b9-4df0-b7ff-7186dda091ba)(content(Whitespace\" \
         \"))))(Tile((id \
         fadaaa72-6a7f-4599-b564-af16add414f6)(label(Waning))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         702af54b-f7eb-42a6-bb85-c9b913e86a50)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         f731a12a-2efa-4973-996f-8426ee08ec0a)(content(Whitespace\" \
         \"))))(Tile((id \
         4bd3aedc-fff9-47a9-b429-e3dffe8aa6ac)(label(0.95))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         fd26fe83-8b95-4f29-ab97-22d0dc7095bd)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         6b6c60d0-462c-409f-b719-272c8a2aaa4d)(content(Whitespace\" \
         \"))))(Secondary((id \
         96a1f246-6b20-41a6-bdf7-760bfe584a3c)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         6edcd6d0-ca09-43bb-b3e2-7993defd9561)(content(Whitespace\" \
         \"))))(Tile((id \
         7cbb9580-e84c-4578-bc31-a4efa7a9c239)(label(int_of_float))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         7c85c93b-0c31-426b-8330-61ba8f3a46bc)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         9e9b24d2-30f4-4e33-adf5-7d08a6344500)(label(float_of_int))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         80992534-6536-4012-b41a-068cc33e5867)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         987e63c3-21a6-4197-ad51-75081a5c1a50)(label(base_ml))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         64d8ea6e-caf3-4489-a492-c67168226025)(content(Whitespace\" \
         \"))))(Tile((id \
         d4373889-3f78-42c3-9271-6cec93294e7b)(label(*.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 27))(sort Exp))((shape(Concave \
         27))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         19692276-318c-4ad7-bc5e-175ee9cdeaae)(content(Whitespace\" \
         \"))))(Tile((id \
         c9dede40-91f5-4f2b-af05-d7007099aee1)(label(multiplier))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         da228afb-afe2-4254-bc59-8bbd525a16e5)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         abed788f-68a9-420f-b48b-61fd007a75c8)(content(Whitespace\"\\n\"))))(Secondary((id \
         83ba8f38-8794-4fd9-bd75-8be7d761c11b)(content(Whitespace\"\\n\"))))(Secondary((id \
         cbee4ebb-23a3-4f90-8d2d-0a537868522d)(content(Comment\"# TAKEAWAY: \
         The dynamic cursor is an internal mechanism which \
         #\"))))(Secondary((id \
         58807745-6161-4c44-81ec-0a6ed9d17144)(content(Whitespace\"\\n\"))))(Secondary((id \
         232d0aad-d9b6-4470-8b0f-2f5b9bb8cea4)(content(Comment\"# tries to \
         keep the probe samples shown aligned to the same \
         #\"))))(Secondary((id \
         72898df1-2521-4354-a8f3-8908821d199f)(content(Whitespace\"\\n\"))))(Secondary((id \
         de792bbd-e4fe-4df6-aa8a-a502ae9f1032)(content(Comment\"# execution, \
         in particular the same call to a function. #\"))))(Secondary((id \
         1ba32dbe-d345-4331-b0cf-a52c9de64d43)(content(Whitespace\"\\n\"))))(Secondary((id \
         2b7413ce-6d37-438b-8066-e67d0c0077e1)(content(Whitespace\"\\n\"))))(Tile((id \
         4f1abc4f-1643-4127-b231-d38594232ab2)(label(watering_amount))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         5979b0fe-90d6-4271-a55d-ff97c1069cff)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         f1f5fea0-be0c-4e1a-91f7-b09db23ea9ed)(label(250))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         190687b9-04e3-452e-a97e-d35eaebb60bb)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         6a468cc3-d32a-4853-8c16-68f2726dc0c6)(content(Whitespace\" \
         \"))))(Tile((id \
         0ea92061-f24f-4a8e-82e9-3f476891e2e4)(label(Full))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         f98b82e4-7760-4070-8a87-3c43fedeb822)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         2ca19426-be4d-4431-b343-bae7d4b29632)(content(Whitespace\"\\n\"))))(Tile((id \
         0ca322d3-bc3c-4083-bcbb-6a58d48bbf92)(label(watering_amount))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         005f3d69-c66d-4a45-bddf-a18190262278)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         a2cd3eb2-aaf0-4da5-a30c-0899ea0cc6e9)(label(50))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         3379cd0d-648c-42dd-b544-bf5e3ea29dad)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         0a8b9c0f-81ac-4f4a-975a-b4d7bdb83911)(content(Whitespace\" \
         \"))))(Tile((id \
         31ec846c-7bc8-4b03-84e9-418a92319cd4)(label(New))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         bfdd7406-3797-424b-809b-b98ff326332d)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         e4d1e7c9-7437-4257-9762-b9305c851bdc)(content(Whitespace\"\\n\"))))(Tile((id \
         2f06ce35-569c-4825-97d3-f90f3665cc65)(label(watering_amount))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         202121e3-0fa4-4d86-86cf-095b34951b8a)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         e960b001-2beb-430b-a831-b28cae70c321)(label(180))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         5127e059-d088-497a-8c16-d88d3b7dc8b5)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         183f2bad-ad3d-4799-8d6e-b6f6080c1eb6)(content(Whitespace\" \
         \"))))(Tile((id \
         10eb6ea2-aff1-4d9c-a4cf-84b2a606e918)(label(Waning))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         6a7f740b-972c-4cd5-a91a-5bb4f04db0ae)(content(Whitespace\"\\n\"))))(Secondary((id \
         1351a62c-2ea7-4320-8712-a36e38400d34)(content(Whitespace\"\\n\"))))(Secondary((id \
         19c56a16-fda6-4c70-9b4d-f33b0006ff01)(content(Comment\"# One last \
         thing: SINGLE MODE (default) vs MANY MODE #\"))))(Secondary((id \
         199d0276-6e07-4865-9ccd-8c1a6fcf968f)(content(Whitespace\"\\n\"))))(Secondary((id \
         ed25a18c-622f-4b01-ac23-e84832a8498e)(content(Comment\"# Double-click \
         any above sample, or press Space when a sample #\"))))(Secondary((id \
         ead64ffc-2a48-479b-9126-b7cbbbb7b9c2)(content(Whitespace\"\\n\"))))(Secondary((id \
         a403ddc4-5768-4ab4-a5f8-d99cad036699)(content(Comment\"# is selected \
         to toggle Many mode: all samples are shown at once! \
         #\"))))(Secondary((id \
         f38a8827-bb34-4106-9a8c-aa3f6486f30e)(content(Whitespace\"\\n\"))))(Secondary((id \
         1e767f98-ba52-48f6-a9d2-5528991f4884)(content(Comment\"# Similarly to \
         single mode, left/right arrow keys move samples. \
         #\"))))(Secondary((id \
         cf8e5561-7442-42b4-9cb2-94b8d4df7b4a)(content(Whitespace\"\\n\"))))(Secondary((id \
         2ad1cba2-5850-4530-97d6-20240e165e60)(content(Comment\"# Double-click \
         again (or Space) to go back to Single mode. #\"))))(Secondary((id \
         deda634e-3bb1-467c-b25f-dbe100a4ef46)(content(Whitespace\"\\n\"))))(Secondary((id \
         0ef28b3e-c3bc-479b-a683-c3f8da7e6685)(content(Whitespace\"\\n\"))))(Secondary((id \
         6b60c0e1-a90d-459e-b8a4-00a406b97363)(content(Comment\"# END OF PART \
         2 - Select the next slide from the top menu #\"))))(Secondary((id \
         2662682c-7285-4cac-a6d5-5c378f01514b)(content(Whitespace\"\\n\")))))";
      backup_text =
        "# PROBES TUTORIAL - PART 2: FUNCTIONS AND THE DYNAMIC CURSOR #\n\n\
         # When a function is called multiple times, each call #\n\
         # generates its own sample. Let's see what that looks like! #\n\n\
         type MoonPhase = New + Waxing + Full + Waning in\n\n\
         # Hazel has no special function definition syntax. #\n\
         # We use regular let definitions to define function literals, #\n\
         # using the syntax `fun <pattern> -> <body>`. #\n\n\
         # TRY THIS: Add a probe to the `multiplier` variable inside #\n\
         # the function `watering_amount` below. When you click on the #\n\
         # sample, notice the arrows that appear to the left. Click on #\n\
         # these arrows, or use the left/right arrow keys, to navigate #\n\
         # between the three different samples collected. #\n\n\
         let watering_amount: (Int, MoonPhase) -> Int =\n\
         fun (base_ml, phase) ->\n\
         let multiplier =\n\
         case phase\n\
         | New => 1.2\n\
         | Full => 0.88\n\
         | Waxing => 1.1\n\
         | Waning => 0.95\n\
         end \n\
         in int_of_float(float_of_int(base_ml) *. multiplier)\n\
         # Above: Hazel uses C-style Function application syntax #\n\
         in\n\n\
         # Now click the samples for the 3 calls to `watering_amount` below. #\n\
         # Notice the sample for 'multiplier' above changes to /align/ with #\n\
         # the selected call! We call this behavior the 'dynamic cursor', #\n\
         # which aligns probe samples to a particular step in an execution. #\n\n\
         ^^probe(watering_amount(250, Full));\n\
         ^^probe(watering_amount(50, New));\n\
         ^^probe(watering_amount(180, Waning));\n\n\
         # Below is the same function as above, this time with many probes. #\n\
         # Select the `multiplier` sample and use the arrow keys to move #\n\
         # through the different values. Notice how this time, there are two #\n\
         # different symbols next to the branches with no samples; \
         \226\136\133 from #\n\
         # before on `Waxing`, which means never evaluated, and a new symbol #\n\
         # \226\138\150, which means there are samples, but they are not \
         aligned to the #\n\
         # dynamic cursor (because of the `multiplier` sample you selected). #\n\
         # Click on any \226\138\150 to align the dynamic cursor to that \
         branch. #\n\n\
         let watering_amount: (Int, MoonPhase) -> Int =\n\
         fun (base_ml, phase) ->\n\
         let ^^probe(multiplier) =\n\
         case ^^probe(phase)\n\
         | New => ^^probe(1.2)\n\
         | Full => ^^probe(0.88)\n\
         | Waxing => ^^probe(1.1)\n\
         | Waning => ^^probe(0.95)\n\
         end \n\
         in ^^probe(int_of_float(float_of_int(base_ml) *. multiplier))\n\
         in\n\n\
         # TAKEAWAY: The dynamic cursor is an internal mechanism which #\n\
         # tries to keep the probe samples shown aligned to the same #\n\
         # execution, in particular the same call to a function. #\n\n\
         watering_amount(250, Full);\n\
         watering_amount(50, New);\n\
         watering_amount(180, Waning)\n\n\
         # One last thing: SINGLE MODE (default) vs MANY MODE #\n\
         # Double-click any above sample, or press Space when a sample #\n\
         # is selected to toggle Many mode: all samples are shown at once! #\n\
         # Similarly to single mode, left/right arrow keys move samples. #\n\
         # Double-click again (or Space) to go back to Single mode. #\n\n\
         # END OF PART 2 - Select the next slide from the top menu #\n";
      refractors =
        "((7c85c93b-0c31-426b-8330-61ba8f3a46bc((kind \
         Probe)(model\"()\")))(4bd3aedc-fff9-47a9-b429-e3dffe8aa6ac((kind \
         Probe)(model\"()\")))(e43fcde9-097a-48ca-925b-949a41f7c67d((kind \
         Probe)(model\"()\")))(f27ec8e3-d53c-42d8-9eef-b33733e43fa7((kind \
         Probe)(model\"()\")))(05300655-9d7b-4217-8e21-04b72b6123b4((kind \
         Probe)(model\"()\")))(6de2113c-cba4-49aa-8408-47670511efbf((kind \
         Probe)(model\"()\")))(97f548d1-c6ba-4f84-a1c6-b65c73d805fc((kind \
         Probe)(model\"()\")))(cb5fe821-8272-4816-8057-bae1037a6de6((kind \
         Probe)(model\"()\")))(936a3ddb-44e8-4ed7-8e34-043ba2d5502f((kind \
         Probe)(model\"()\")))(482290cd-dbc9-43aa-acca-f09518f3837a((kind \
         Probe)(model\"()\"))))";
    } )
