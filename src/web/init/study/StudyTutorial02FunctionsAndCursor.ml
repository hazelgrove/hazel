let out : string * Haz3lcore.PersistentSegment.t =
  ( "Study / tutorial / 02-functions-and-cursor",
    {
      segment =
        "((Secondary((id \
         14fe390b-7b94-4723-8d75-74a191173b68)(content(Comment\"# PROBES \
         TUTORIAL - PART 2: FUNCTIONS AND THE DYNAMIC CURSOR \
         #\"))))(Secondary((id \
         0f4580c1-5d6f-41bf-bf8f-3217b09e5fac)(content(Whitespace\"\\n\"))))(Secondary((id \
         da9ec50d-c90a-4a20-add7-1a38a96a56a5)(content(Whitespace\"\\n\"))))(Secondary((id \
         531f44b0-a33c-4390-8bdd-140560d62e28)(content(Comment\"# When a \
         function is called multiple times, each call #\"))))(Secondary((id \
         efcb721d-f2ea-46c1-999b-29aaf66433ff)(content(Whitespace\"\\n\"))))(Secondary((id \
         bcc5f34f-0696-4f0a-861a-4f2cc932e6fc)(content(Comment\"# generates \
         its own sample. Let's see what that looks like! #\"))))(Secondary((id \
         aa6ccb3b-a651-4409-8c9c-23391f9070d5)(content(Whitespace\"\\n\"))))(Secondary((id \
         6c744aa1-69f4-40a6-87c8-b9b314266f82)(content(Whitespace\"\\n\"))))(Tile((id \
         53d10dd8-735d-48d4-912f-c1bae766b4fb)(label(type = in))(mold((out \
         Exp)(in_(TPat Typ))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         f1a10c53-3a86-43aa-8b33-eb959443f20d)(content(Whitespace\" \
         \"))))(Tile((id \
         143a883f-1c24-4fd0-bdc6-9853fdd6dc41)(label(MoonPhase))(mold((out \
         TPat)(in_())(nibs(((shape Convex)(sort TPat))((shape Convex)(sort \
         TPat))))))(shards(0))(children())))(Secondary((id \
         6baad5c4-b48f-4584-9f46-a8e214eb9b5e)(content(Whitespace\" \
         \")))))((Secondary((id \
         e2d31a09-35eb-477b-8ed2-b6f4d366ab43)(content(Whitespace\" \
         \"))))(Tile((id \
         81e431b2-e130-472e-825a-e1356d7b478d)(label(New))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         1e016b62-01d1-4ffe-9d95-ee777f210871)(content(Whitespace\" \
         \"))))(Tile((id \
         5427ba72-6527-420a-9a7a-d9cc38776746)(label(+))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 12))(sort Typ))((shape(Concave \
         12))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         f1c17dad-d0df-468c-93c5-6c6552b79410)(content(Whitespace\" \
         \"))))(Tile((id \
         b2808ac3-c117-4060-86db-d5f7a8c6cd0b)(label(Waxing))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         54717191-73cf-4cbf-bded-efdd50b4f041)(content(Whitespace\" \
         \"))))(Tile((id \
         8de47ac4-072d-4bae-9cd1-dce777b1fd5e)(label(+))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 12))(sort Typ))((shape(Concave \
         12))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         3e20b5b2-a1eb-423d-94ad-787a53b9e0da)(content(Whitespace\" \
         \"))))(Tile((id \
         2b83792b-772b-4c14-aa11-4fcacf7eb030)(label(Full))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         85d8a5a3-9b49-4e90-9f47-18b0a8899a69)(content(Whitespace\" \
         \"))))(Tile((id \
         5c3392f3-8f62-4b6d-a128-eecb35057c18)(label(+))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 12))(sort Typ))((shape(Concave \
         12))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         182b97e9-058f-450d-ae5c-f572c5ac73c6)(content(Whitespace\" \
         \"))))(Tile((id \
         ee23affc-834c-4b09-8518-7d16de3ce733)(label(Waning))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         4115ace8-17a6-49cf-8961-1bf1ef4e2cef)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         312db22f-ec88-499a-b98d-a44939d87d14)(content(Whitespace\"\\n\"))))(Secondary((id \
         e189cb73-997f-4e46-a276-f1d536ec47c4)(content(Whitespace\"\\n\"))))(Secondary((id \
         67ed1d36-bd00-4c2f-89a6-d7147cebcb33)(content(Comment\"# Hazel has no \
         special function definition syntax. #\"))))(Secondary((id \
         28c34f9f-581d-47a2-b0c2-38c9463198f1)(content(Whitespace\"\\n\"))))(Secondary((id \
         60f36267-e8d8-4701-abb3-9c2bf5f8f783)(content(Comment\"# We use \
         regular let definitions to define function literals, \
         #\"))))(Secondary((id \
         f7ab6335-6bfa-4ef6-b947-20d5c5845f5e)(content(Whitespace\"\\n\"))))(Secondary((id \
         200a98a3-e9bc-4993-9ab1-1ad477093b40)(content(Comment\"# using the \
         syntax `fun <pattern> -> <body>`. #\"))))(Secondary((id \
         dfec60be-eef9-41c4-a42e-4fe7419a7e2a)(content(Whitespace\"\\n\"))))(Secondary((id \
         75013143-1ce2-46e9-9a5a-64ba747e0aaa)(content(Whitespace\"\\n\"))))(Secondary((id \
         6da4dc9d-cdb8-4c92-bdca-3f65d2d0b02c)(content(Comment\"# TRY THIS: \
         Add a probe to the `multiplier` variable inside #\"))))(Secondary((id \
         402db70c-f54c-4176-b2c7-7c2fbc79e6f7)(content(Whitespace\"\\n\"))))(Secondary((id \
         eff1f5fb-c1b4-4606-9f7b-bae7f2d68320)(content(Comment\"# the function \
         `watering_amount` below. When you click on the #\"))))(Secondary((id \
         995f1eb3-4226-4eab-93b1-870fda9e12ac)(content(Whitespace\"\\n\"))))(Secondary((id \
         cc1ab7f5-85f1-4c60-bb53-4d6e151a66f3)(content(Comment\"# sample, \
         notice the arrows that appear to the left. Click on \
         #\"))))(Secondary((id \
         99c6795d-40ec-4966-90e3-c31a988b5d14)(content(Whitespace\"\\n\"))))(Secondary((id \
         15d33bf4-53a2-4472-aa73-8b5be875ac61)(content(Comment\"# these \
         arrows, or use the left/right arrow keys, to navigate \
         #\"))))(Secondary((id \
         068e41d4-a7bb-4fd5-85cb-44af02d82ad3)(content(Whitespace\"\\n\"))))(Secondary((id \
         da74be35-5462-428a-941f-0cdef163f0f3)(content(Comment\"# between the \
         three different samples collected. #\"))))(Secondary((id \
         38914c6d-4c16-4352-8b49-ea4ca4595ec6)(content(Whitespace\"\\n\"))))(Secondary((id \
         d7900d19-9c83-4fc1-b839-36d2673fcc97)(content(Whitespace\"\\n\"))))(Tile((id \
         9139081d-9639-481c-b959-64f827201b18)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         247c44c8-15fe-4c4d-abe0-47c89ecdd38d)(content(Whitespace\" \
         \"))))(Tile((id \
         8c386cb9-c804-4fbe-8ae8-579887cb36e4)(label(watering_amount))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         5004d18f-a82f-464f-94f1-9073f71b40c0)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 24))(sort Pat))((shape(Concave \
         24))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         37794e96-30ae-4a0f-bcf2-e74af184a955)(content(Whitespace\" \
         \"))))(Tile((id \
         287bcaf8-6f48-4b26-8ee0-d5b387e09871)(label(\"(\"\")\"))(mold((out \
         Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0 1))(children(((Tile((id \
         17955a9c-826a-40c8-992a-a24db7f5fca2)(label(Int))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         ca100cbc-b0a1-43d2-a3cc-9398e65c2613)(label(,))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 44))(sort Typ))((shape(Concave \
         44))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         2118fa95-bc53-4f5b-9cd7-0ba8b6043060)(content(Whitespace\" \
         \"))))(Tile((id \
         d7edac10-7d54-4fe1-950c-6db523f9c095)(label(MoonPhase))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children()))))))))(Secondary((id \
         1f99f3d3-4436-46b6-8d47-f2d9eb394b80)(content(Whitespace\" \
         \"))))(Tile((id \
         2ade7505-23de-489e-991f-b376d8eb9349)(label(->))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 13))(sort Typ))((shape(Concave \
         13))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         4e9f599d-e61b-40c0-b815-35a32ff95560)(content(Whitespace\" \
         \"))))(Tile((id \
         66b0f675-2311-43c5-a720-a5aa917f4117)(label(Int))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         3c6592e2-d725-4157-a23f-f5a1c3276355)(content(Whitespace\" \
         \")))))((Secondary((id \
         bc53c78f-42de-405a-abf9-0c35e272af6a)(content(Whitespace\"\\n\"))))(Tile((id \
         4d941bf5-fe5f-4f72-9087-c9b9611c1444)(label(fun ->))(mold((out \
         Exp)(in_(Pat))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         37))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         0102183c-afec-455b-ae8e-9665a5009617)(content(Whitespace\" \
         \"))))(Tile((id \
         6d77cb2a-0f54-4ec8-8538-dfff74a9f2d7)(label(\"(\"\")\"))(mold((out \
         Pat)(in_(Pat))(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0 1))(children(((Tile((id \
         2cfd0347-6179-4dbf-93fc-45701915a760)(label(base_ml))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         bfa86fdc-5154-42c3-999d-a22873113cf3)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 44))(sort Pat))((shape(Concave \
         44))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         d61a2fc3-f154-4353-bcba-87d65d41cf7d)(content(Whitespace\" \
         \"))))(Tile((id \
         fadc94d0-683d-4e2a-9982-d6ba4e171ce0)(label(phase))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children()))))))))(Secondary((id \
         343b24cf-c8e8-4a53-9000-56c782faed31)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         9fa9fb24-09ca-4a54-963a-b7f9a117e476)(content(Whitespace\"\\n\"))))(Tile((id \
         900274b1-159f-408e-877e-7ecc1e2e19ef)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         fdd2b4b3-3150-4d4c-8367-07ab1ed2f3d4)(content(Whitespace\" \
         \"))))(Tile((id \
         71bc660f-8611-4c75-a2fe-8ccadc995a21)(label(multiplier))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         a87db036-8f71-45b6-bc59-9a2559841d40)(content(Whitespace\" \
         \")))))((Secondary((id \
         1215526e-fd0b-4a5d-8f39-fd8861b07f17)(content(Whitespace\"\\n\"))))(Tile((id \
         1d093906-1e86-40db-9b8e-d7ae607cb098)(label(case end))(mold((out \
         Exp)(in_(Rul))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         36a550c3-37c4-46d5-a77e-2e280263127b)(content(Whitespace\" \
         \"))))(Tile((id \
         effa9278-7be9-4d75-8167-046bb0a4bba3)(label(phase))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         ed23ebf7-5f86-4305-993a-6ef51c15f292)(content(Whitespace\"\\n\"))))(Tile((id \
         f786951c-5779-417d-aede-99cbcef5764e)(label(| =>))(mold((out \
         Rul)(in_(Pat))(nibs(((shape(Concave 46))(sort Exp))((shape(Concave \
         46))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         389de3be-958a-4465-89d7-54d379d51ba1)(content(Whitespace\" \
         \"))))(Tile((id \
         9c4132c9-5433-4345-b9d4-179884450248)(label(New))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         59ba754c-e15c-480a-9381-d12f0e63005a)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         468f58a0-2e0b-4153-ae3f-de20da3b01ef)(content(Whitespace\" \
         \"))))(Tile((id \
         0dd7ab7c-9547-4e59-a858-d909ca56fe61)(label(1.2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         d010adac-1983-4c87-80e8-6cee71c75a5d)(content(Whitespace\"\\n\"))))(Tile((id \
         929379c3-441e-4653-83ed-d21cd893a67c)(label(| =>))(mold((out \
         Rul)(in_(Pat))(nibs(((shape(Concave 46))(sort Exp))((shape(Concave \
         46))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         1841f3e6-0c18-43c9-9ad3-67bda50dec31)(content(Whitespace\" \
         \"))))(Tile((id \
         7158facc-5ee1-45a2-a38e-86d8a224c923)(label(Full))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         99aa1844-d8e5-4c4b-abb5-b2cebda617a1)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         6bbe630d-48e0-47b6-a28d-82eaf78c499e)(content(Whitespace\" \
         \"))))(Tile((id \
         fc0a30c1-154f-4bab-b0e8-ca1164037da2)(label(0.88))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         a660f2c0-32e8-4e34-ba9b-d66ffe93d592)(content(Whitespace\"\\n\"))))(Tile((id \
         c3c557b4-2860-4dea-81b0-423a849a61c1)(label(| =>))(mold((out \
         Rul)(in_(Pat))(nibs(((shape(Concave 46))(sort Exp))((shape(Concave \
         46))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         ceb40775-cb10-4b06-ad14-209d00692a23)(content(Whitespace\" \
         \"))))(Tile((id \
         fca312a4-f92b-4fb1-ba0a-e407f4f72f64)(label(Waxing))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         ff6d7178-648b-418c-baac-93915e0b2741)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         d5b1f38c-dec3-453f-9c1b-27f137076ff9)(content(Whitespace\" \
         \"))))(Tile((id \
         d9ab4a04-bede-4288-9728-074495b84dc6)(label(1.1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         0bbeace7-98cc-4899-90dd-eef76f6bfd59)(content(Whitespace\"\\n\"))))(Tile((id \
         5e03d522-8116-452e-ab7d-8620e415f549)(label(| =>))(mold((out \
         Rul)(in_(Pat))(nibs(((shape(Concave 46))(sort Exp))((shape(Concave \
         46))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         c5ee6a3e-eb66-4627-8b86-76f08d4065bd)(content(Whitespace\" \
         \"))))(Tile((id \
         1eb4581a-3611-4230-a5da-c0a7af04ee90)(label(Waning))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         e41bd685-2984-47a7-bb5a-c4d57835c525)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         de03294e-a194-4408-91a4-87aabbf40ec0)(content(Whitespace\" \
         \"))))(Tile((id \
         9a61c61c-53aa-4226-a6c8-6d0ec9c59f31)(label(0.95))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         5b7018e6-f238-430b-89a9-045848560a47)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         3fcffa19-7395-4354-9b25-c2ea28fdcae7)(content(Whitespace\" \
         \"))))(Secondary((id \
         e3c5106e-0778-4b7b-9b94-e4c4e39e92e7)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         e7c379f8-9887-4cef-8416-8f5d7594a30a)(content(Whitespace\" \
         \"))))(Tile((id \
         fd68d742-7a00-4cd7-a6b0-327e48988c25)(label(int_of_float))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         f98096a6-f216-4367-b9dc-4334490ff800)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         8ec31d8f-ed90-4092-9b0d-d836e3badea1)(label(float_of_int))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         630413a2-c7a7-49d6-a687-be8deb2401d1)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         e7402882-e7f5-4ac5-88de-d31c2f136a51)(label(base_ml))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         cb76324e-20b5-4ede-84fc-db9e831489cd)(content(Whitespace\" \
         \"))))(Tile((id \
         badc8606-077e-4707-a8f3-bc191b6f6b44)(label(*.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 27))(sort Exp))((shape(Concave \
         27))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         6f41c1bb-47ff-4cee-8d0f-dd76ddb71ee5)(content(Whitespace\" \
         \"))))(Tile((id \
         663bc90a-4df0-48fb-979a-879fff466bbc)(label(multiplier))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         6ff3e001-7eb8-422c-86c2-6850fa366782)(content(Whitespace\"\\n\"))))(Secondary((id \
         edcc7097-e194-4c12-8baf-4ea54e4747e6)(content(Comment\"# Above: Hazel \
         uses C-style Function application syntax #\"))))(Secondary((id \
         5379e9a9-ab57-430a-989c-c5800715b908)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         2cf9eab7-7f94-40b2-b450-f24a39e8055b)(content(Whitespace\"\\n\"))))(Secondary((id \
         81112d84-de20-4409-88d7-d902a480f600)(content(Whitespace\"\\n\"))))(Secondary((id \
         104b1306-0900-4472-8f4d-45304a95348c)(content(Comment\"# Now click \
         the samples for the 3 calls to `watering_amount` below. \
         #\"))))(Secondary((id \
         22c2fba8-5ac2-4b80-81e3-e33a176a9592)(content(Whitespace\"\\n\"))))(Secondary((id \
         704a4b4e-5837-4a90-b680-bf033e9f6a6b)(content(Comment\"# Notice the \
         sample for 'multiplier' above changes to /align/ with \
         #\"))))(Secondary((id \
         77e33536-5c35-43a9-b999-1a1748061470)(content(Whitespace\"\\n\"))))(Secondary((id \
         8017507b-efcc-4367-8c41-3561e5ee22c9)(content(Comment\"# the selected \
         call! We call this behavior the 'dynamic cursor', \
         #\"))))(Secondary((id \
         acbce83e-686c-4497-a097-171d6f838ffe)(content(Whitespace\"\\n\"))))(Secondary((id \
         2d580ed5-6cd1-4aa8-86ee-5c5238926983)(content(Comment\"# which aligns \
         probe samples to a particular step in an execution. \
         #\"))))(Secondary((id \
         6cae8612-194f-4373-85ee-2ed9d9e9a494)(content(Whitespace\"\\n\"))))(Secondary((id \
         1a6f1dae-ca51-4b79-83d1-337be193b426)(content(Whitespace\"\\n\"))))(Tile((id \
         fc889a09-b064-4499-aac5-6c17062d8080)(label(watering_amount))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         379cb93d-72d6-4379-8bd6-8b5adabca51e)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         8981dc33-2abc-4f18-816b-da352715886e)(label(250))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         9a767b74-6733-4752-947b-79d119a963ce)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         587d4d8e-64fc-49dd-a545-8b7049be9ec0)(content(Whitespace\" \
         \"))))(Tile((id \
         5140f1d3-a3ae-49d3-8c34-bfdcf418c617)(label(Full))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         449a8667-3f3c-41db-b743-61bb2dc3c602)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         222b331a-3434-4f85-8ef0-f7a3e297698d)(content(Whitespace\"\\n\"))))(Tile((id \
         46cc2e14-1786-4d78-a846-e4264e062287)(label(watering_amount))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         17806ec3-f6fa-434c-9634-f1a6f20975ea)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         67c6aa60-4443-4326-8478-29f981749d03)(label(50))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         a41fbb04-6f5e-44a4-aa0e-5104933b74f9)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         2836eb5d-d184-4939-a1c0-d17c5591315f)(content(Whitespace\" \
         \"))))(Tile((id \
         6c8a53eb-44fb-438b-b2d5-1e282597cc96)(label(New))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         958ac43e-79f5-4877-8444-248400883efb)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         bf54d7e2-deec-4499-92cb-aa3ed4deefb5)(content(Whitespace\"\\n\"))))(Tile((id \
         d2010a0b-a21a-4308-bed8-9d36c4a2e456)(label(watering_amount))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         610cf22e-4c82-44f4-8f34-63996795aad8)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         36452cff-98d1-42e6-8976-e2517debbccb)(label(180))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         02bc8c01-fcbc-422a-bc05-f3d929f9ead8)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         7abdf9e2-dac3-45a4-a577-5b0380672b8a)(content(Whitespace\" \
         \"))))(Tile((id \
         022058b1-643a-4df7-a51e-1d5ab1cdf551)(label(Waning))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         7cf79a1e-e515-4108-bcb7-d7825eaa26ae)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         298cb74e-da1e-45ab-bbea-8a2114e8405b)(content(Whitespace\"\\n\"))))(Secondary((id \
         4a5056ae-9ef4-4a4a-bf28-3b332cf453a8)(content(Whitespace\"\\n\"))))(Secondary((id \
         3a9e648d-db47-4ccb-9eeb-fe693edbfef0)(content(Comment\"# Below is the \
         same function as above, this time with many probes. \
         #\"))))(Secondary((id \
         0ac18721-c7ef-4bd4-9c0c-e81199ba1901)(content(Whitespace\"\\n\"))))(Secondary((id \
         b33ccc85-e910-4347-a6f9-ebc6f5be68e8)(content(Comment\"# Select the \
         `multiplier` sample and use the arrow keys to move \
         #\"))))(Secondary((id \
         1f34445c-444f-4457-9c49-3281261f2767)(content(Whitespace\"\\n\"))))(Secondary((id \
         d1a51fef-89e2-4370-8d78-d345eeb9a728)(content(Comment\"# through the \
         different values. Notice how this time, there are two \
         #\"))))(Secondary((id \
         1ad8b56d-30d5-46e0-9ea5-1134d33d1f80)(content(Whitespace\"\\n\"))))(Secondary((id \
         8c0c7bdf-b5a9-4050-b403-dff69d1110ee)(content(Comment\"# different \
         symbols next to the branches with no samples; \\226\\136\\133 from \
         #\"))))(Secondary((id \
         b4a44e4b-8705-4cb6-a022-35ec47e6cda0)(content(Whitespace\"\\n\"))))(Secondary((id \
         5276d994-490a-4610-8098-b95d4e789a42)(content(Comment\"# before on \
         `Waxing`, which means never evaluated, and a new symbol \
         #\"))))(Secondary((id \
         49c1d51e-eee5-436d-893e-036241d783aa)(content(Whitespace\"\\n\"))))(Secondary((id \
         3d908004-9cc6-4ba1-beb8-482dfd9295fe)(content(Comment\"# \
         \\226\\138\\150, which means there are samples, but they are not \
         aligned to the #\"))))(Secondary((id \
         dd60a512-9c1d-4b0a-91c7-b904d774de99)(content(Whitespace\"\\n\"))))(Secondary((id \
         a27409c6-8ac7-4552-a0b4-00cc5853a7d0)(content(Comment\"# dynamic \
         cursor (because of the `multiplier` sample you selected). \
         #\"))))(Secondary((id \
         0eeff953-db6e-433f-a6b9-557838fb9d27)(content(Whitespace\"\\n\"))))(Secondary((id \
         4bce23ba-2d67-40d5-9c70-f963184c101e)(content(Comment\"# Click on any \
         \\226\\138\\150 to align the dynamic cursor to that branch. \
         #\"))))(Secondary((id \
         bced8f31-676c-4b3b-9f41-25eb31de8167)(content(Whitespace\"\\n\"))))(Secondary((id \
         425e397e-44b8-47fe-aa73-cfb76234b8fb)(content(Whitespace\"\\n\"))))(Tile((id \
         37d708c3-734a-4849-9d4e-2f87f1d1b45d)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         4393a725-1f09-49b2-b901-410aad01fed9)(content(Whitespace\" \
         \"))))(Tile((id \
         6ca794cd-5edd-4115-b3b4-a6ac5ff7a384)(label(watering_amount))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         7091a4fe-3741-4779-adae-5350d584c2ad)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 24))(sort Pat))((shape(Concave \
         24))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         bd3d51b6-9bca-4fe5-bf7b-e0cfad86464b)(content(Whitespace\" \
         \"))))(Tile((id \
         feba6e6d-974f-478f-bbe4-f00a6b2cecae)(label(\"(\"\")\"))(mold((out \
         Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0 1))(children(((Tile((id \
         618a6214-1458-4165-a6ed-0575e50504c5)(label(Int))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         ed389039-e1d7-4bb8-aa04-4830d83a7fc3)(label(,))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 44))(sort Typ))((shape(Concave \
         44))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         09e28740-409a-46e2-b227-2ab7f5648086)(content(Whitespace\" \
         \"))))(Tile((id \
         57203e15-5f10-4a59-93bd-cba44245afb9)(label(MoonPhase))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children()))))))))(Secondary((id \
         4d6bcc10-1c03-407a-ace5-3886ed0682eb)(content(Whitespace\" \
         \"))))(Tile((id \
         f2fa02d6-a37f-4407-be66-71f2a6acc1c7)(label(->))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 13))(sort Typ))((shape(Concave \
         13))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         666a6d69-e074-4f90-87aa-3308bd688863)(content(Whitespace\" \
         \"))))(Tile((id \
         ebf92e19-cbf5-4cf1-b8c8-18bfef7d518b)(label(Int))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         e364325f-b6a1-41c6-9064-ebc988b36256)(content(Whitespace\" \
         \")))))((Secondary((id \
         fa3ac05c-969c-40a5-bcd5-9ef97c455ce7)(content(Whitespace\"\\n\"))))(Tile((id \
         008d782a-7525-49d2-951d-edac69328a3c)(label(fun ->))(mold((out \
         Exp)(in_(Pat))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         37))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         28728ae2-c177-496a-8f53-6cbb9daf4b9c)(content(Whitespace\" \
         \"))))(Tile((id \
         ff873595-be33-4111-96a5-f21bec622db0)(label(\"(\"\")\"))(mold((out \
         Pat)(in_(Pat))(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0 1))(children(((Tile((id \
         7d95b372-3de5-4e9d-8f41-4172e3d47996)(label(base_ml))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         0c5731fe-0797-4a3f-aff1-0415d4126d80)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 44))(sort Pat))((shape(Concave \
         44))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         2bed16de-2076-43d3-96dd-d7b85e3303b1)(content(Whitespace\" \
         \"))))(Tile((id \
         561a8334-5b54-44f9-9deb-27e7f27f4cbc)(label(phase))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children()))))))))(Secondary((id \
         ac06b46e-afb7-4667-8ce1-002d0c8829a7)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         aadbfd54-77b7-4c77-bec4-920a7f90ba7a)(content(Whitespace\"\\n\"))))(Tile((id \
         324ca259-412e-4b16-99f9-c8fc093109a2)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         130ded36-e2af-45b5-a1f3-82ebafb47d96)(content(Whitespace\" \
         \"))))(Tile((id \
         2f966b99-22e6-4acd-a1f0-4bf5981ff784)(label(multiplier))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         49702fd2-f20b-4322-988a-0b680fae6b32)(content(Whitespace\" \
         \")))))((Secondary((id \
         50e23145-687b-4e52-a615-4e1a1ab255af)(content(Whitespace\"\\n\"))))(Tile((id \
         94249a8d-51c8-4b55-85d2-161a9fbd168b)(label(case end))(mold((out \
         Exp)(in_(Rul))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         0f220ec7-32a4-494b-96ff-5019fb71489c)(content(Whitespace\" \
         \"))))(Tile((id \
         285571c3-24c7-422b-b9a8-17be48e7dabc)(label(phase))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         8c921707-3a36-4e72-80a9-636bd1e21ca2)(content(Whitespace\"\\n\"))))(Tile((id \
         98f68ced-f87b-4118-984c-0be7f8a3e9f2)(label(| =>))(mold((out \
         Rul)(in_(Pat))(nibs(((shape(Concave 46))(sort Exp))((shape(Concave \
         46))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         2ae6f934-e5f5-4a91-a73a-1c094c4948f5)(content(Whitespace\" \
         \"))))(Tile((id \
         d87b3388-e6c5-495a-88b2-d90975c0003a)(label(New))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         838bdf5a-922b-436d-b47e-c067eea93174)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         885a3d5c-067c-4674-9ceb-dec6b76ad633)(content(Whitespace\" \
         \"))))(Tile((id \
         821e0757-94ad-4dae-8a43-269662e38dd1)(label(1.2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         829b265d-70a9-4f44-bfaf-d38cebf4d2ee)(content(Whitespace\"\\n\"))))(Tile((id \
         be1bf327-5ddd-47ae-a8ae-ccc997969a3c)(label(| =>))(mold((out \
         Rul)(in_(Pat))(nibs(((shape(Concave 46))(sort Exp))((shape(Concave \
         46))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         67a71883-68f0-4bca-8b66-e25a1efd474c)(content(Whitespace\" \
         \"))))(Tile((id \
         feb3d94f-c9b3-4c60-b36e-538d8f714b1d)(label(Full))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         416f1d2d-06ca-4ec4-ac67-d4cc4ad54fc2)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         05d8875b-7870-4e43-a999-b8581828dcfc)(content(Whitespace\" \
         \"))))(Tile((id \
         d1113962-b463-4163-b3c5-901d4bf10729)(label(0.88))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         71692612-ddf8-40c8-915f-6ae409a393fc)(content(Whitespace\"\\n\"))))(Tile((id \
         334328b1-2b62-4bc9-b484-3dd73cf7e1d2)(label(| =>))(mold((out \
         Rul)(in_(Pat))(nibs(((shape(Concave 46))(sort Exp))((shape(Concave \
         46))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         27cf3657-6f38-4971-8425-0580d5b3e96e)(content(Whitespace\" \
         \"))))(Tile((id \
         16ee45b1-515d-4c1f-87a8-6e74ea55e394)(label(Waxing))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         500a080d-f177-4def-98bb-9f33dd6410ab)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         5384b280-ff15-4d34-b7ec-2e608574e9ba)(content(Whitespace\" \
         \"))))(Tile((id \
         579f04d1-9b29-465e-8199-26c8ff1e3c40)(label(1.1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         f811ef62-921f-4722-b6ef-c697e6be32ca)(content(Whitespace\"\\n\"))))(Tile((id \
         3eb1b9c7-136a-4d30-85fd-80224213754a)(label(| =>))(mold((out \
         Rul)(in_(Pat))(nibs(((shape(Concave 46))(sort Exp))((shape(Concave \
         46))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         97558a0a-9dcf-4d0f-82a7-0b28891412b0)(content(Whitespace\" \
         \"))))(Tile((id \
         eca22a85-acf9-4bde-a5a2-552d750fa376)(label(Waning))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         b2e3a860-7fb7-4b5d-9387-a67daf29427a)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         c13e38f2-eab4-4f9b-aadc-2c00d871def1)(content(Whitespace\" \
         \"))))(Tile((id \
         c99bf720-c6bc-49ab-a2e9-cb1f1edd8c8a)(label(0.95))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         d694b54a-8719-4b56-9f28-8412fd6aff84)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         90493f21-5cdc-4417-860e-b87b9abac43f)(content(Whitespace\" \
         \"))))(Secondary((id \
         0eea3707-f29b-4161-9276-b6cd20b1ab80)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         af5b6f58-20b1-45e3-b367-f3b828395753)(content(Whitespace\" \
         \"))))(Tile((id \
         daae7a31-d56c-40a3-afb1-4794ab87c014)(label(int_of_float))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         87da092f-af9b-4cb0-a6b1-b668569a149e)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         00cfbe38-68a7-452a-a40a-ef950f70c331)(label(float_of_int))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         1ff145d6-c1e0-4559-a0d0-2f11c9e9fba8)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         a499d725-5a23-4985-8925-de6a1b92196e)(label(base_ml))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         27402bd1-eed5-4e2d-82a8-dac7c563f9b1)(content(Whitespace\" \
         \"))))(Tile((id \
         dda80740-73c2-4cb6-838f-a9cf50fe9757)(label(*.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 27))(sort Exp))((shape(Concave \
         27))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         da49cac2-98fc-44ec-aa66-35898e5ebd28)(content(Whitespace\" \
         \"))))(Tile((id \
         1dd0d8b9-ef8d-4c54-8bd6-662e24d4db89)(label(multiplier))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         8071f72d-0aca-44cd-aeda-5471f1be4e3a)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         987774ee-a2fc-4c41-bb9b-b351c75299b4)(content(Whitespace\"\\n\"))))(Secondary((id \
         cc2383ce-1aa1-4dee-aee6-f3ec95b31afb)(content(Whitespace\"\\n\"))))(Secondary((id \
         1c1b4d9b-e1aa-4b15-b071-8bbf2096c470)(content(Comment\"# TAKEAWAY: \
         The dynamic cursor is an internal mechanism which \
         #\"))))(Secondary((id \
         717d1b7d-3790-4857-924d-825be5c36eac)(content(Whitespace\"\\n\"))))(Secondary((id \
         0fe74acb-0a87-4eee-b31c-a5a738b48777)(content(Comment\"# tries to \
         keep the probe samples shown aligned to the same \
         #\"))))(Secondary((id \
         71021e9e-a2e6-495e-9417-bfdd3a95e62e)(content(Whitespace\"\\n\"))))(Secondary((id \
         ab7a21f3-c882-42eb-8f0b-3d0b11362cd4)(content(Comment\"# execution, \
         in particular the same call to a function. #\"))))(Secondary((id \
         c48920bf-a812-4b7b-84ca-86f73811debf)(content(Whitespace\"\\n\"))))(Secondary((id \
         6cc33d4c-9332-4577-a127-11ea87b22f7b)(content(Whitespace\"\\n\"))))(Tile((id \
         0f9d1549-369c-4785-9033-27c6a3e14590)(label(watering_amount))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         de5fcce3-b544-4fe3-965a-874d78372642)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         b5a14338-fd27-426e-ac63-e5d4eb8ed20e)(label(250))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         2b5a0276-0ad4-4780-a78e-69f5e04a7eec)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         7fb93dd7-4f68-460b-bc0b-59dd1fa7fccf)(content(Whitespace\" \
         \"))))(Tile((id \
         601d907a-f6da-460b-b3a2-9616adb607d4)(label(Full))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         e8e5a979-bbb0-4ba5-8642-2287c3074a3e)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         5aba9349-f5f5-4f4c-8551-e65cfd22d19b)(content(Whitespace\"\\n\"))))(Tile((id \
         ff4ad33b-3cb1-4df9-9f02-ad0cb187f9a2)(label(watering_amount))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         2e64d6bb-ccc7-4457-9282-c83d52cde2c5)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         005e8b9f-d652-4a4d-b913-c133a95cb341)(label(50))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         0c0a3d40-ecbf-4302-81a6-01c800a44c5e)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         ff425f4f-9ad0-43ae-9e64-e1fb53ca5d7f)(content(Whitespace\" \
         \"))))(Tile((id \
         46684b2f-e528-4abf-98c4-e109392e850e)(label(New))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         c85d52cc-f1a2-4d59-83e8-23f96da930a2)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         b9dd5c33-9e4b-4298-8179-924bb885793a)(content(Whitespace\"\\n\"))))(Tile((id \
         0e8930c7-2cff-4b8e-bce0-acaa2a135f8c)(label(watering_amount))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         f83d709a-bc29-470b-a7ea-a333f306b6b5)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         481a7e3e-dff8-43d3-8727-c5bad3e83961)(label(180))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         4c4f82a6-5bfe-4189-ad47-53c10159a15e)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         e409affd-0b72-437a-8096-57cce74a6975)(content(Whitespace\" \
         \"))))(Tile((id \
         e69d2136-e374-4833-a48d-b5eb7b9f8ae7)(label(Waning))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         2d41bb04-94e2-4597-8cb1-99575741ad6e)(content(Whitespace\"\\n\"))))(Secondary((id \
         551d3d60-5716-4c55-a81d-e4c0877ecdd2)(content(Whitespace\"\\n\"))))(Secondary((id \
         f132bb34-43e1-40a4-ad05-25b846575e77)(content(Comment\"# One last \
         thing: SINGLE MODE (default) vs MANY MODE #\"))))(Secondary((id \
         700c2f7f-4f75-4723-aefd-a2e15c13ec4d)(content(Whitespace\"\\n\"))))(Secondary((id \
         a7310911-6fe9-4b14-a7b4-585ff5dd3f6a)(content(Comment\"# Double-click \
         any above sample, or press Space when a sample #\"))))(Secondary((id \
         15d948db-65bc-40ad-a627-288c627bf59a)(content(Whitespace\"\\n\"))))(Secondary((id \
         dffeb72b-c91c-427e-ba96-a0eddb594adf)(content(Comment\"# is selected \
         to toggle Many mode: all samples are shown at once! \
         #\"))))(Secondary((id \
         07493949-3211-4ab8-9055-32fdcd901d47)(content(Whitespace\"\\n\"))))(Secondary((id \
         4360a4b5-a80b-425c-9e79-c1a42074b481)(content(Comment\"# Similarly to \
         single mode, left/right arrow keys move samples. \
         #\"))))(Secondary((id \
         0d23e3b0-8f5c-474b-88ba-62e64303fb6a)(content(Whitespace\"\\n\"))))(Secondary((id \
         2a32b9d0-f1bb-4a12-95bd-4834dbe750fb)(content(Comment\"# Double-click \
         again (or Space) to go back to Single mode. #\"))))(Secondary((id \
         49904e52-6dc0-48b0-aeb7-9d76daf045ce)(content(Whitespace\"\\n\"))))(Secondary((id \
         71b370d1-a649-4481-902b-de760f154835)(content(Whitespace\"\\n\"))))(Secondary((id \
         5406e2c9-26ee-4255-9550-88c1887b6749)(content(Comment\"# END OF PART \
         2 - Select the next slide from the top menu #\"))))(Secondary((id \
         d30a4ac2-e4d7-4201-8395-51615772d5cc)(content(Whitespace\"\\n\")))))";
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
        "((87da092f-af9b-4cb0-a6b1-b668569a149e((kind \
         Probe)(model\"()\")))(c99bf720-c6bc-49ab-a2e9-cb1f1edd8c8a((kind \
         Probe)(model\"()\")))(579f04d1-9b29-465e-8199-26c8ff1e3c40((kind \
         Probe)(model\"()\")))(d1113962-b463-4163-b3c5-901d4bf10729((kind \
         Probe)(model\"()\")))(821e0757-94ad-4dae-8a43-269662e38dd1((kind \
         Probe)(model\"()\")))(285571c3-24c7-422b-b9a8-17be48e7dabc((kind \
         Probe)(model\"()\")))(2f966b99-22e6-4acd-a1f0-4bf5981ff784((kind \
         Probe)(model\"()\")))(610cf22e-4c82-44f4-8f34-63996795aad8((kind \
         Probe)(model\"()\")))(17806ec3-f6fa-434c-9634-f1a6f20975ea((kind \
         Probe)(model\"()\")))(379cb93d-72d6-4379-8bd6-8b5adabca51e((kind \
         Probe)(model\"()\"))))";
    } )
