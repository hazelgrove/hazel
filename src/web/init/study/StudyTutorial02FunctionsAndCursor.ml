let out : string * Haz3lcore.PersistentSegment.t =
  ( "Study / tutorial / 02-functions-and-cursor",
    {
      segment =
        "((Secondary((id \
         d36f0ae9-7c5e-410e-87f8-bc0d782b8a67)(content(Comment\"# PROBES \
         TUTORIAL - PART 2: FUNCTIONS AND THE DYNAMIC CURSOR \
         #\"))))(Secondary((id \
         01209146-3eb1-432a-883f-4280e131ff71)(content(Whitespace\"\\n\"))))(Secondary((id \
         86529b31-c0e1-4a8a-bb85-c6ff421eecec)(content(Whitespace\"\\n\"))))(Secondary((id \
         a64921d7-abd1-4409-9166-8d7d873a3830)(content(Comment\"# When a \
         function is called multiple times, each call #\"))))(Secondary((id \
         8e40cd19-bb1b-4d52-9ac4-484b984af539)(content(Whitespace\"\\n\"))))(Secondary((id \
         c9612553-e8fb-4861-8050-d33b384cd6e3)(content(Comment\"# generates \
         its own sample. Let's see what that looks like! #\"))))(Secondary((id \
         4f52b094-f978-4dba-b01f-cd98f07302de)(content(Whitespace\"\\n\"))))(Secondary((id \
         37839a05-9772-472d-a83b-2ff623ff1898)(content(Whitespace\"\\n\"))))(Tile((id \
         5b0bcaa5-feef-4bd0-b01e-6d498b46f075)(label(type = in))(mold((out \
         Exp)(in_(TPat Typ))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         cffd2542-c04f-4373-bc09-bd71ce42d170)(content(Whitespace\" \
         \"))))(Tile((id \
         fbbf4b13-0201-4fcb-9ad0-0407e64df691)(label(MoonPhase))(mold((out \
         TPat)(in_())(nibs(((shape Convex)(sort TPat))((shape Convex)(sort \
         TPat))))))(shards(0))(children())))(Secondary((id \
         092da41c-2752-4037-aa48-57437bcd3238)(content(Whitespace\" \
         \")))))((Secondary((id \
         13d0e7f8-7f28-42a4-8026-893dba6f7155)(content(Whitespace\" \
         \"))))(Tile((id \
         d738b58d-39d7-4da1-952b-43102e14dc4f)(label(New))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         5ea7e81c-6f1f-44fd-91ec-eeffe316e60d)(content(Whitespace\" \
         \"))))(Tile((id \
         3886c10b-3a79-482e-ad2d-6242dda4808b)(label(+))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 12))(sort Typ))((shape(Concave \
         12))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         38dff8ed-a432-4c28-b966-750fd62f2be6)(content(Whitespace\" \
         \"))))(Tile((id \
         fd5410a5-e843-4906-a3ab-923f5b3c84d3)(label(Waxing))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         35ab4764-3f5f-43a9-8ea0-e41788ed49e0)(content(Whitespace\" \
         \"))))(Tile((id \
         3f00302d-3c55-45a6-866b-7b8b3d54fbbf)(label(+))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 12))(sort Typ))((shape(Concave \
         12))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         aa27f8c5-536d-48fb-8405-965198641b34)(content(Whitespace\" \
         \"))))(Tile((id \
         f9c2fa98-e20b-4362-8a5b-20e61fddfd7c)(label(Full))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         988c0035-dfd2-4c61-a9f5-bd6042c5e7eb)(content(Whitespace\" \
         \"))))(Tile((id \
         a44a753f-db85-4542-a033-75ca481e0d46)(label(+))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 12))(sort Typ))((shape(Concave \
         12))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         8146a4c3-94eb-4ab2-8fdc-ef076a021d93)(content(Whitespace\" \
         \"))))(Tile((id \
         00c038cf-03c0-489e-aab4-526316066ca5)(label(Waning))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         7aaf4b32-457c-4918-993c-0d73325b2be6)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         3d6eaaf1-eae7-4ea5-8f73-5d9ba6b86e1f)(content(Whitespace\"\\n\"))))(Secondary((id \
         ff66dca8-a3f8-4741-ae95-e5e7558ea227)(content(Whitespace\"\\n\"))))(Secondary((id \
         d04d9d6f-a71e-40a5-8547-ae6d791ea75c)(content(Comment\"# Hazel has no \
         special function definition syntax. #\"))))(Secondary((id \
         4b258923-60c7-4126-a127-852eb1e36ded)(content(Whitespace\"\\n\"))))(Secondary((id \
         204b0779-d623-4981-ae3f-dce3401ba4f2)(content(Comment\"# We use \
         regular let definitions to define function literals, \
         #\"))))(Secondary((id \
         11e23166-1abb-4169-9474-b958e230e905)(content(Whitespace\"\\n\"))))(Secondary((id \
         665c45b0-b6d2-4a57-9699-009bfea03b49)(content(Comment\"# using the \
         syntax `fun <pattern> -> <body>`. #\"))))(Secondary((id \
         5aac24ef-f9a0-45ef-a293-164de0d4b92b)(content(Whitespace\"\\n\"))))(Secondary((id \
         310110bf-4933-4045-9299-5023eaee3877)(content(Whitespace\"\\n\"))))(Secondary((id \
         70480bf8-b079-4a1a-8b09-cf05446931ba)(content(Comment\"# TRY THIS: \
         Add a probe to the `multiplier` variable inside #\"))))(Secondary((id \
         4013473a-ed7a-43bd-aff2-75436cc79a1d)(content(Whitespace\"\\n\"))))(Secondary((id \
         5ffecadb-3582-453d-9d8a-437a7e4323df)(content(Comment\"# the function \
         `watering_amount` below. When you click on the #\"))))(Secondary((id \
         d55ee6d4-0b98-41b8-8b5d-286a04ad4329)(content(Whitespace\"\\n\"))))(Secondary((id \
         b88793ff-f6e5-4407-a724-154d9ba396fb)(content(Comment\"# sample, \
         notice the arrows that appear to the left. Click on \
         #\"))))(Secondary((id \
         5b79a1ae-9549-4b61-a35a-19d0643b7950)(content(Whitespace\"\\n\"))))(Secondary((id \
         2fafc94c-404d-4c5b-8cc7-3f918f5dc439)(content(Comment\"# these \
         arrows, or use the left/right arrow keys, to navigate \
         #\"))))(Secondary((id \
         16e38588-49be-4646-8799-e236c8dc6d01)(content(Whitespace\"\\n\"))))(Secondary((id \
         459b3f65-8a1d-4bb7-b05b-abcaee40315b)(content(Comment\"# between the \
         three different samples collected. #\"))))(Secondary((id \
         72e99be3-0ddf-4ba3-9177-e7f589cf87e0)(content(Whitespace\"\\n\"))))(Secondary((id \
         71053925-f4cf-49eb-bdaa-7d2a34a8dc8d)(content(Whitespace\"\\n\"))))(Tile((id \
         86de6018-7cf4-4436-9b6b-2d349c628b18)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         83ca17e8-a121-419a-9d94-045be3b610b2)(content(Whitespace\" \
         \"))))(Tile((id \
         45d339dc-a2a2-4594-90d6-b3f3d38a15a6)(label(watering_amount))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         bb7ad76e-b22a-4bb8-8bc0-29923a78db6a)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 24))(sort Pat))((shape(Concave \
         24))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         238eb745-9053-46ba-b3de-8157db4e228a)(content(Whitespace\" \
         \"))))(Tile((id \
         eb9426bb-09da-4bc7-a853-ab7938faf19e)(label(\"(\"\")\"))(mold((out \
         Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0 1))(children(((Tile((id \
         aa90e98d-1f87-4074-b814-9242565ef5e8)(label(Int))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         1e81444f-6848-47c2-bd70-5d2ee0490042)(label(,))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 44))(sort Typ))((shape(Concave \
         44))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         4e9cbfb6-f229-4055-b965-d1123eb1d31b)(content(Whitespace\" \
         \"))))(Tile((id \
         4964fa1a-2b80-4586-8fed-ce66b11cf80b)(label(MoonPhase))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children()))))))))(Secondary((id \
         781270ec-c45c-457c-a1ff-a891c9ce6081)(content(Whitespace\" \
         \"))))(Tile((id \
         f3c6a2c7-3363-4e40-b442-dc500aa28a66)(label(->))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 13))(sort Typ))((shape(Concave \
         13))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         5ed8977a-b5ab-4883-ac2d-eee17b0ac26d)(content(Whitespace\" \
         \"))))(Tile((id \
         77122e02-bbc3-48a3-985d-1fe4be794e59)(label(Int))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         230616f5-8e02-4108-89e2-c1e7858d76b7)(content(Whitespace\" \
         \")))))((Secondary((id \
         85a7549f-0579-4124-a976-bd2e231ff81b)(content(Whitespace\"\\n\"))))(Tile((id \
         fbde92d5-76bc-4d90-aaca-72992af7c0b5)(label(fun ->))(mold((out \
         Exp)(in_(Pat))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         37))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         6f30f92b-cd8d-49ab-8b82-d368945abc7e)(content(Whitespace\" \
         \"))))(Tile((id \
         10f2cd21-d1c3-493e-9eb2-8bcf16cc11ae)(label(\"(\"\")\"))(mold((out \
         Pat)(in_(Pat))(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0 1))(children(((Tile((id \
         6a7cc7dc-bdd8-4807-b997-17becf1cf137)(label(base_ml))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         8f4b560b-96f4-4444-992e-6c764b01951e)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 44))(sort Pat))((shape(Concave \
         44))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         01479d5e-3ce8-4467-b44c-78ef61f6d250)(content(Whitespace\" \
         \"))))(Tile((id \
         7e540900-fe64-4c4a-913b-964640f37339)(label(phase))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children()))))))))(Secondary((id \
         3c37455f-28b9-43c1-b0c2-eebab6b806ee)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         3d328324-1d71-4f9d-bbaf-97fa91a1260c)(content(Whitespace\"\\n\"))))(Tile((id \
         1541a854-0817-4448-be52-d11e5aa7873c)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         a61df700-7643-4376-9646-1f9812f286bb)(content(Whitespace\" \
         \"))))(Tile((id \
         f91fa797-78fa-47be-a56d-a0666b7c7645)(label(multiplier))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         5f5bcc5b-ab8d-4c4d-98cb-e9ebd03674cd)(content(Whitespace\" \
         \")))))((Secondary((id \
         71aa8a9f-fa9c-410d-9e05-c7004f946561)(content(Whitespace\"\\n\"))))(Tile((id \
         9cc891c9-1ca3-4aaa-b8a6-bc6d7976350d)(label(case end))(mold((out \
         Exp)(in_(Rul))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         497c1c39-0641-4b34-ace4-c20bee123d8c)(content(Whitespace\" \
         \"))))(Tile((id \
         8f2687e4-abff-49bb-bd6f-1f8a022ba567)(label(phase))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         c36a8b2a-46c7-4db7-b6ca-346767bc3da7)(content(Whitespace\"\\n\"))))(Tile((id \
         bfcadfa2-3982-4267-a12b-f722144075bd)(label(| =>))(mold((out \
         Rul)(in_(Pat))(nibs(((shape(Concave 46))(sort Exp))((shape(Concave \
         46))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         a8feaccc-3c5a-48ab-88b5-05d7cd5aec96)(content(Whitespace\" \
         \"))))(Tile((id \
         099bbfe1-0d3f-4374-bc65-a074bc74c185)(label(New))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         a3751f9a-dd7e-4e2e-959b-f5d7a4b1d50c)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         0cde5495-ab07-4bdb-ae36-7f4fc767e569)(content(Whitespace\" \
         \"))))(Tile((id \
         263c57ff-d7f4-4ae9-948a-f8bc5b80a516)(label(1.2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         e7e997de-4897-484b-971f-9a47c55364ec)(content(Whitespace\"\\n\"))))(Tile((id \
         1aa384e7-07a1-4d91-b548-8387ada3e082)(label(| =>))(mold((out \
         Rul)(in_(Pat))(nibs(((shape(Concave 46))(sort Exp))((shape(Concave \
         46))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         b1554065-d12c-4521-9079-195068d252ff)(content(Whitespace\" \
         \"))))(Tile((id \
         de1e747c-fc8f-48e3-be4a-081e5f299ad8)(label(Full))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         5cc3d944-6698-44d4-95cd-17738581ed4d)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         cb0dc34a-636d-454b-be29-710515575596)(content(Whitespace\" \
         \"))))(Tile((id \
         c1f2a17d-1b8a-48b0-a0a9-93910c74cc84)(label(0.88))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         537bf022-979e-4333-8921-d56a458d3f31)(content(Whitespace\"\\n\"))))(Tile((id \
         b82ca280-a443-461d-8433-bfc20ed4086a)(label(| =>))(mold((out \
         Rul)(in_(Pat))(nibs(((shape(Concave 46))(sort Exp))((shape(Concave \
         46))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         2b5739de-0622-4ab4-af20-a83bd5c3e303)(content(Whitespace\" \
         \"))))(Tile((id \
         0f434e69-0d73-4448-abf5-d8a1d80e2ffd)(label(Waxing))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         b4c5d2cf-1816-493b-ae6c-b769de89bfe0)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         bc317a41-c49f-4b5c-bb50-5cd0af2da5b2)(content(Whitespace\" \
         \"))))(Tile((id \
         43267ca0-ef4a-4cd0-8906-78d7614d6891)(label(1.1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         43bd9dda-4d7a-4f31-90e2-d20571d00a03)(content(Whitespace\"\\n\"))))(Tile((id \
         1f94e3ab-960b-48c3-8e05-266ece38f9ef)(label(| =>))(mold((out \
         Rul)(in_(Pat))(nibs(((shape(Concave 46))(sort Exp))((shape(Concave \
         46))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         bd091ac9-9cee-4586-a9d4-8bbdf79a35af)(content(Whitespace\" \
         \"))))(Tile((id \
         9109e81f-977d-422c-9617-683c22245291)(label(Waning))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         84187935-be71-48f6-9938-74d696a2ff10)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         84015e0b-d7f8-4b62-96b9-407bb7764a3e)(content(Whitespace\" \
         \"))))(Tile((id \
         ba7e30ff-3399-43c4-9c3f-e4f2a4e62a4c)(label(0.95))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         e19b0ae8-3728-4e45-91fb-695e93bb4c0a)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         924bd3c5-dbdf-4869-a2a4-0603075ccd2f)(content(Whitespace\" \
         \"))))(Secondary((id \
         26c79d34-1ecd-410b-8850-79635cd36e5b)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         aa6b9cc0-c655-4dad-aa9b-be319117f9dc)(content(Whitespace\" \
         \"))))(Tile((id \
         cd2ce8ad-70ac-4df6-a618-00f5a5a421ee)(label(int_of_float))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         f325403d-1b41-4c06-ae35-ad42d1b4ad54)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         2215a870-b83f-45bc-bbe2-fac801fce594)(label(float_of_int))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         2989206e-63f3-4aaa-8219-60b39f4b2bf0)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         f52f0019-3926-4b00-b2c6-47a0a6f1651a)(label(base_ml))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         3a34581d-0b22-44c7-9166-bb74f1097359)(content(Whitespace\" \
         \"))))(Tile((id \
         134df8bd-7815-4aa7-b286-86ffc4ccfac7)(label(*.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 27))(sort Exp))((shape(Concave \
         27))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         75808f0c-3727-498f-a1a2-a203b4cc3c0c)(content(Whitespace\" \
         \"))))(Tile((id \
         348eb6a5-2582-44a6-8824-5aebdf511264)(label(multiplier))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         8db720b4-9f69-451f-9f17-3acab04eeaae)(content(Whitespace\"\\n\"))))(Secondary((id \
         378158a7-f366-4fa2-8caf-f67c9734af4c)(content(Comment\"# Above: Hazel \
         uses C-style Function application syntax #\"))))(Secondary((id \
         2baea87f-f036-46e1-8d79-15f8cd97e05d)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         08cac2dc-c494-45bb-865f-a322ac88d093)(content(Whitespace\"\\n\"))))(Secondary((id \
         b6d53d53-fd22-455e-881c-a81a0afcb539)(content(Whitespace\"\\n\"))))(Secondary((id \
         f64f2039-d764-4689-8f27-83888d1e7b75)(content(Comment\"# Now click \
         the samples for the 3 calls to `watering_amount` below. \
         #\"))))(Secondary((id \
         5d4c771a-856e-4e9c-bf2d-2dad8267b2e8)(content(Whitespace\"\\n\"))))(Secondary((id \
         f585c268-b7fa-40e4-a617-511b2a869b8b)(content(Comment\"# Notice the \
         sample for 'multiplier' above changes to /align/ with \
         #\"))))(Secondary((id \
         42292f22-cfc1-409d-a057-807e7d3ab1d5)(content(Whitespace\"\\n\"))))(Secondary((id \
         b293bf1f-fc3d-4000-8044-731921888805)(content(Comment\"# the selected \
         call! We call this behavior the 'dynamic cursor', \
         #\"))))(Secondary((id \
         dfee9e10-ee52-4548-b457-41a4db95876b)(content(Whitespace\"\\n\"))))(Secondary((id \
         217f2740-ab93-4301-95dd-5296ad9a586e)(content(Comment\"# which aligns \
         probe samples to a particular step in an execution. \
         #\"))))(Secondary((id \
         4359d71b-11e4-4a3d-8366-e6a252371953)(content(Whitespace\"\\n\"))))(Secondary((id \
         95fd6786-ccc5-4098-aa21-0c276e6f0b6d)(content(Whitespace\"\\n\"))))(Tile((id \
         42ec21ae-369a-4659-86bc-24b6b1c49031)(label(watering_amount))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         40534131-1834-4475-bbbd-475c5d0bcc24)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         3e07529c-247d-4413-ac8e-043c87e96452)(label(250))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         6f9a1f13-0088-4969-a57e-0f6abe1e007c)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         4a051e76-bd2c-412b-b1ec-ca87be973215)(content(Whitespace\" \
         \"))))(Tile((id \
         5627d27b-6537-4bd9-9782-524cfbc280b9)(label(Full))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         8201ec0c-9114-4fbe-b8d4-2495da6fc9be)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         cd8b287f-3e66-4308-a29a-079f710259e6)(content(Whitespace\"\\n\"))))(Tile((id \
         ed5df09c-7d77-4958-84ee-159e8ce9c6d1)(label(watering_amount))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         671aed96-2238-4215-beaf-4a5cdc9938dc)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         bacbff21-1d64-46db-8ab8-5ac5ad67f67b)(label(50))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         4193aeac-db5b-4751-a59f-b128a5098840)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         8ceb6ec7-b63e-4db7-9e9d-03352615645d)(content(Whitespace\" \
         \"))))(Tile((id \
         a31a9a22-bd6b-48fc-ab6b-1733f132520b)(label(New))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         eb607106-63f5-48e7-af11-b42c87b0faa0)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         7e1fa246-f20f-4fea-9498-8a6657d50204)(content(Whitespace\"\\n\"))))(Tile((id \
         98a0acb1-4b18-4720-8519-543d96cd4c83)(label(watering_amount))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         a77f813c-63e8-44ab-a730-5509acc4bc72)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         159f05df-c583-46d8-9e62-e70a209cf0d6)(label(180))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         6917c41c-3c11-422a-ac4e-2c74c365db39)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         213692da-ab59-4273-b9c0-387b6d583924)(content(Whitespace\" \
         \"))))(Tile((id \
         c7526d66-65a4-4fff-b64d-02ec1d91a39f)(label(Waning))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         576c8d3a-0ae5-43a3-a5f1-d1c981c8c704)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         e0037ab1-fc96-4284-86f1-d072dcf3ded6)(content(Whitespace\"\\n\"))))(Secondary((id \
         751b0496-3e8d-4ad0-8e98-8ce20f4de8f7)(content(Whitespace\"\\n\"))))(Secondary((id \
         edb9ceae-034b-4582-b5eb-6b664ad3c344)(content(Comment\"# Below is the \
         same function as above, this time with many probes. \
         #\"))))(Secondary((id \
         404a233f-5801-479d-a604-5955c3b36af2)(content(Whitespace\"\\n\"))))(Secondary((id \
         1718a967-6ac8-45a2-b6ce-b93a78f94fbd)(content(Comment\"# Select the \
         `multiplier` sample and use the arrow keys to move \
         #\"))))(Secondary((id \
         360423ce-9c74-411f-9b49-ea7d0ae1894b)(content(Whitespace\"\\n\"))))(Secondary((id \
         c2ead8ed-216a-4d9e-8ec0-75867c2a59f8)(content(Comment\"# through the \
         different values. Notice how this time, there are two \
         #\"))))(Secondary((id \
         afc44793-24ac-4a48-8d57-ff90d0b01a23)(content(Whitespace\"\\n\"))))(Secondary((id \
         0b88a2e7-d004-4dc9-acb8-be282a5909e0)(content(Comment\"# different \
         symbols next to the branches with no samples; \\226\\136\\133 from \
         #\"))))(Secondary((id \
         44ddf990-bc1c-4826-919c-300f3d39104d)(content(Whitespace\"\\n\"))))(Secondary((id \
         ddc65924-a984-4512-b938-331c68cf22ad)(content(Comment\"# before on \
         `Waxing`, which means never evaluated, and a new symbol \
         #\"))))(Secondary((id \
         7793c3c2-9245-4bae-8c2b-3a73c9a8cdc4)(content(Whitespace\"\\n\"))))(Secondary((id \
         940de73b-ecec-4a31-8094-fac5920c2ffa)(content(Comment\"# \
         \\226\\138\\150, which means there are samples, but they are not \
         aligned to the #\"))))(Secondary((id \
         38276caa-ffa8-4cd3-9872-cec249f541f3)(content(Whitespace\"\\n\"))))(Secondary((id \
         ecd31918-3ff1-4062-9409-f10b18a256d4)(content(Comment\"# dynamic \
         cursor (because of the `multiplier` sample you selected). \
         #\"))))(Secondary((id \
         49d728a2-c7e8-4185-886d-c4e751a49886)(content(Whitespace\"\\n\"))))(Secondary((id \
         24a27a29-6a67-49a9-bc1b-6079e711e322)(content(Comment\"# Click on any \
         \\226\\138\\150 to align the dynamic cursor to that branch. \
         #\"))))(Secondary((id \
         8fdf68af-d7e4-4984-b08f-05b15a1d4f96)(content(Whitespace\"\\n\"))))(Secondary((id \
         95c823ae-cb5f-4c18-8932-d15b9222330e)(content(Whitespace\"\\n\"))))(Tile((id \
         9f465508-d4e9-4961-aa05-c5dd3002ea1f)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         b96cd1d2-bd8b-4326-a09d-3e6bb46f1fad)(content(Whitespace\" \
         \"))))(Tile((id \
         61ee67a9-ac94-4aae-89fe-19345d47cfd4)(label(watering_amount))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         8eaad03e-ed9b-4375-a3f5-572ef1494e49)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 24))(sort Pat))((shape(Concave \
         24))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         9911c935-675f-471e-9524-468b9506730e)(content(Whitespace\" \
         \"))))(Tile((id \
         b457bcf6-6380-44b3-a969-26d3694666cc)(label(\"(\"\")\"))(mold((out \
         Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0 1))(children(((Tile((id \
         0c0c5ae9-eb33-48b1-b194-2c1c2848ecea)(label(Int))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         f3b56d59-b59a-4ada-a689-8939d2a8be4c)(label(,))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 44))(sort Typ))((shape(Concave \
         44))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         88737b4f-cdc9-4f32-af84-d2f9b0698945)(content(Whitespace\" \
         \"))))(Tile((id \
         5434a4e8-b06b-4663-8b59-208c9a873154)(label(MoonPhase))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children()))))))))(Secondary((id \
         cceda97c-3416-40c9-86fc-96906d2d1728)(content(Whitespace\" \
         \"))))(Tile((id \
         b9c27cdf-7660-45ca-8aa0-4138ac023277)(label(->))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 13))(sort Typ))((shape(Concave \
         13))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         87adcfff-fc38-45bf-abae-9ee5383e1aa4)(content(Whitespace\" \
         \"))))(Tile((id \
         c8bfd1b2-163f-4be9-b12e-3892593dd905)(label(Int))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         99653412-a626-41ed-aaa3-d7c5efca9532)(content(Whitespace\" \
         \")))))((Secondary((id \
         9c1355a4-e427-4902-836f-71b57afe9be2)(content(Whitespace\"\\n\"))))(Tile((id \
         cefe20f1-61f0-42b6-9724-8ecf5e68c897)(label(fun ->))(mold((out \
         Exp)(in_(Pat))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         37))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         ba27baa0-6712-468a-9b6c-527bda2faa29)(content(Whitespace\" \
         \"))))(Tile((id \
         46d998c4-a361-4456-a0b1-29ca9c2928f6)(label(\"(\"\")\"))(mold((out \
         Pat)(in_(Pat))(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0 1))(children(((Tile((id \
         d55d34c7-c9f3-4afe-8f90-1ee20e4a2f5f)(label(base_ml))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         84eb9565-7a71-4b04-96c2-70e79fa66c94)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 44))(sort Pat))((shape(Concave \
         44))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         9b63d48a-3f14-4ce5-9655-63fe0299e542)(content(Whitespace\" \
         \"))))(Tile((id \
         53494ee1-d8a3-4602-926c-b36fa6e2c772)(label(phase))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children()))))))))(Secondary((id \
         f72bbf0c-c929-40f8-8575-f8008bebfd62)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         e51e2663-aab3-4e02-b5fe-7259c0d2c14c)(content(Whitespace\"\\n\"))))(Tile((id \
         f296e0a0-c009-4d79-b037-dc31aa0bacbc)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         c7be063c-98de-4779-90bf-510c61072bf5)(content(Whitespace\" \
         \"))))(Tile((id \
         e4b9ba23-e935-4071-8d21-32600845fd50)(label(multiplier))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         cefa9cd4-98d8-4a19-9c9f-eaeba29eb31c)(content(Whitespace\" \
         \")))))((Secondary((id \
         9c4b217e-f177-4cf9-9859-0f1a9515241b)(content(Whitespace\"\\n\"))))(Tile((id \
         2c0d63d6-4a50-463e-ad63-9e60b27b7e71)(label(case end))(mold((out \
         Exp)(in_(Rul))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         b1207ded-c20f-4c20-9ea8-8c2f79158560)(content(Whitespace\" \
         \"))))(Tile((id \
         1660a4a4-d8e4-4cbd-aa67-a826659c13f8)(label(phase))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         9a7a5ddd-baf4-4dbd-adfd-7e9d5a797334)(content(Whitespace\"\\n\"))))(Tile((id \
         415953c9-b92c-4fda-8a5c-7ad54c00dc4c)(label(| =>))(mold((out \
         Rul)(in_(Pat))(nibs(((shape(Concave 46))(sort Exp))((shape(Concave \
         46))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         8c5e7247-c3a8-4037-8a13-0602799a9a24)(content(Whitespace\" \
         \"))))(Tile((id \
         fd6fc7ad-c58b-48a8-ba5f-4cbfab345a01)(label(New))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         2415ac31-76d5-4460-908a-27e9ee2f13a7)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         2777741a-b4a0-4d90-bbdc-34eb4a2d43fb)(content(Whitespace\" \
         \"))))(Tile((id \
         74b1a7eb-6f53-4b9d-baad-2712143046b6)(label(1.2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         1c0a023f-d0e5-4ccc-ab00-5943cf5c4e95)(content(Whitespace\"\\n\"))))(Tile((id \
         166b07af-0e96-4da0-9386-8c62b104939c)(label(| =>))(mold((out \
         Rul)(in_(Pat))(nibs(((shape(Concave 46))(sort Exp))((shape(Concave \
         46))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         9d1c4ccd-b137-4e0f-a6d5-16f4ed655956)(content(Whitespace\" \
         \"))))(Tile((id \
         0ea05522-2492-4d9e-b309-8a5fe9b60232)(label(Full))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         2725a2a6-798e-46b4-8fee-0ec08a599460)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         db870be0-f834-4f40-b888-909fba99617f)(content(Whitespace\" \
         \"))))(Tile((id \
         d1348055-444b-4551-973c-389d3e8cbdfb)(label(0.88))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         c7146618-1444-4019-b70d-0cbf568c8979)(content(Whitespace\"\\n\"))))(Tile((id \
         ff296194-61b0-442c-9d9e-b8f1e346c917)(label(| =>))(mold((out \
         Rul)(in_(Pat))(nibs(((shape(Concave 46))(sort Exp))((shape(Concave \
         46))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         2482be05-5894-4cb4-b8b1-41ad698d54c8)(content(Whitespace\" \
         \"))))(Tile((id \
         8d58c814-0117-4030-aaa6-f8794e3efb3d)(label(Waxing))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         bab5092d-7c94-4788-8001-520452388802)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         5b16dded-21bf-4311-914b-22dc6729dc37)(content(Whitespace\" \
         \"))))(Tile((id \
         69689c61-9b74-41d9-9df5-5377ac9e3717)(label(1.1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         6b065a9e-fdd4-4154-aae5-71197b68c1e1)(content(Whitespace\"\\n\"))))(Tile((id \
         2571ebdd-ecc1-45e4-b0db-2ab6a05a5919)(label(| =>))(mold((out \
         Rul)(in_(Pat))(nibs(((shape(Concave 46))(sort Exp))((shape(Concave \
         46))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         adc3b69a-a8ec-4049-aa75-991169cd20e6)(content(Whitespace\" \
         \"))))(Tile((id \
         bf6213eb-80a4-4c24-b2fe-169bc5e65a6b)(label(Waning))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         46fc8fd8-0d75-480f-b7fc-518378f52dc7)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         70e35557-6057-4cab-a224-4b6fc84ffacb)(content(Whitespace\" \
         \"))))(Tile((id \
         14a08f46-a270-4980-b81f-476553eda22b)(label(0.95))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         27d95311-c8cc-4981-9d39-c6bf10fe6a82)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         63800815-50d5-4711-a566-68f3191bd55b)(content(Whitespace\" \
         \"))))(Secondary((id \
         21545b98-3987-4ad3-8cf2-9472f0dfd24b)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         f65276f3-bfaa-40bb-91fd-92cf280c6153)(content(Whitespace\" \
         \"))))(Tile((id \
         a44e4e11-9909-4e54-8470-2066c9b317fc)(label(int_of_float))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         a16123d2-4d6e-47b5-bf1b-f724e1d6ab0d)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         1641b4aa-d566-4a67-b847-a968aececd6a)(label(float_of_int))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         d82c9f10-7760-4c6b-93f8-e6b30776bf4a)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         4c712ec2-5757-4590-b091-083b21f2774f)(label(base_ml))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         190e7b44-5b85-429f-ab9d-51a45c608bd3)(content(Whitespace\" \
         \"))))(Tile((id \
         2e3ab53a-ab37-45f4-af16-57b5e32b607c)(label(*.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 27))(sort Exp))((shape(Concave \
         27))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         6cee0cc9-f2d0-45ae-b531-3cacefb4c1db)(content(Whitespace\" \
         \"))))(Tile((id \
         e637693a-d4e5-416c-9fb7-1874501de8b5)(label(multiplier))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         7685073c-70ac-480b-aabe-3b4dc12c261d)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         b06d5171-ba12-4f35-b46b-9782c127f095)(content(Whitespace\"\\n\"))))(Secondary((id \
         c49af284-e114-439c-b63e-d08504fc8ab9)(content(Whitespace\"\\n\"))))(Secondary((id \
         e3e3a5b1-f7ac-422a-ac4d-efa6066843a1)(content(Comment\"# TAKEAWAY: \
         The dynamic cursor is an internal mechanism which \
         #\"))))(Secondary((id \
         86a2b618-fb0d-47e5-b43b-be2ffbe8c7f0)(content(Whitespace\"\\n\"))))(Secondary((id \
         74c4a39f-eeef-4513-b7e0-878813c08675)(content(Comment\"# tries to \
         keep the probe samples shown aligned to the same \
         #\"))))(Secondary((id \
         4674ca8d-c65d-495f-bb2d-e3aec8e983c2)(content(Whitespace\"\\n\"))))(Secondary((id \
         9aa3232c-4bee-411c-993d-e6556fea0ed8)(content(Comment\"# execution, \
         in particular the same call to a function. #\"))))(Secondary((id \
         4be718f0-d749-47a2-982d-60743a2f722f)(content(Whitespace\"\\n\"))))(Secondary((id \
         3c4f8d1f-3bd9-4501-9e45-70cf4d7630f1)(content(Whitespace\"\\n\"))))(Tile((id \
         ffc5cc74-8959-4650-9bed-e612dea3a18d)(label(watering_amount))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         e9d6111a-c692-4f05-b405-eb38dbc6454a)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         55014cfd-70db-4543-9438-f497256337c8)(label(250))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         98a540e4-6ac9-4d8d-83d4-cc5cefd0bb59)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         45a780bf-158d-42d7-a7ff-4b2fcc9daeb1)(content(Whitespace\" \
         \"))))(Tile((id \
         fa16ff8d-b4a7-4e2b-967c-00c0e1e78fd7)(label(Full))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         4432a156-7a54-4349-95e4-2aaf433444db)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         f0cae096-4b63-4eca-9e05-a92d9ff07029)(content(Whitespace\"\\n\"))))(Tile((id \
         2f03e284-8163-44f0-bb4e-87e3fec1dfa4)(label(watering_amount))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         4a5ba466-73b0-462f-9671-b96f51f69b47)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         99213d8e-6fe1-4bd9-a5d7-679ebf6a5fa8)(label(50))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         e1da9192-cd6c-416c-bcd2-78294f34cc43)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         d8f6d464-8fd5-4abe-89fa-e7a4fead10a4)(content(Whitespace\" \
         \"))))(Tile((id \
         d9521a0e-d10e-4707-8d86-f672eacd28ec)(label(New))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         3d1bafda-e584-4262-8e69-e0ff6ac61f2a)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         03b245d3-6022-4b9d-9035-c0e667b17e98)(content(Whitespace\"\\n\"))))(Tile((id \
         abda25dc-47a9-45a4-816e-8fc78dd2caee)(label(watering_amount))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         1da3d548-5d59-475c-9616-69fd10a5293d)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         a5a230fc-16e7-4d23-8b67-ea4f3f36c041)(label(180))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         5d683ce3-81bb-4e86-a780-417210d7acc9)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         f8a80daf-97a0-43b9-990d-97714367fc93)(content(Whitespace\" \
         \"))))(Tile((id \
         0681d83a-69c5-49a1-afda-2802a719a963)(label(Waning))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         1e7878cb-4153-4ece-a994-5e24beb63c00)(content(Whitespace\"\\n\"))))(Secondary((id \
         fdcdc25d-2817-4e2d-9011-172b8b60c80e)(content(Whitespace\"\\n\"))))(Secondary((id \
         149e27a0-626b-4b73-a734-daae5fe7ad26)(content(Comment\"# One last \
         thing: SINGLE MODE (default) vs MANY MODE #\"))))(Secondary((id \
         2a423644-a6d4-42e9-8bdc-b08fd0761baa)(content(Whitespace\"\\n\"))))(Secondary((id \
         3a8b7de3-acff-412e-8e94-cc7727cf4106)(content(Comment\"# Double-click \
         any above sample, or press Space when a sample #\"))))(Secondary((id \
         e3c35f4d-4406-4fc0-ba2b-f308fca1da62)(content(Whitespace\"\\n\"))))(Secondary((id \
         b2a61ad5-54c0-42ea-acf5-b4dff20e7a9a)(content(Comment\"# is selected \
         to toggle Many mode: all samples are shown at once! \
         #\"))))(Secondary((id \
         2d9b5306-429f-4b38-9168-57f311ac1986)(content(Whitespace\"\\n\"))))(Secondary((id \
         a681be5e-cc1b-4eeb-bf04-d2282e6762f2)(content(Comment\"# Similarly to \
         single mode, left/right arrow keys move samples. \
         #\"))))(Secondary((id \
         811e47e4-a3e6-4e02-8016-21eeed09fa5e)(content(Whitespace\"\\n\"))))(Secondary((id \
         df7182fc-4ef4-418c-9c81-8a600d8dd54b)(content(Comment\"# Double-click \
         again (or Space) to go back to Single mode. #\"))))(Secondary((id \
         b8695b1c-32dc-4528-a946-0687996cd1ff)(content(Whitespace\"\\n\"))))(Secondary((id \
         5b0d372c-3dae-4ae5-9540-2fcc846d0e44)(content(Whitespace\"\\n\"))))(Secondary((id \
         90ae6302-3a2c-4f5b-8892-7622ddc53a1e)(content(Comment\"# END OF PART \
         2 - Select the next slide from the top menu #\"))))(Secondary((id \
         270bc8e0-13e4-441c-bc84-5bd6675a9004)(content(Whitespace\"\\n\")))))";
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
        "((a16123d2-4d6e-47b5-bf1b-f724e1d6ab0d((kind \
         Probe)(model\"()\")))(14a08f46-a270-4980-b81f-476553eda22b((kind \
         Probe)(model\"()\")))(69689c61-9b74-41d9-9df5-5377ac9e3717((kind \
         Probe)(model\"()\")))(d1348055-444b-4551-973c-389d3e8cbdfb((kind \
         Probe)(model\"()\")))(74b1a7eb-6f53-4b9d-baad-2712143046b6((kind \
         Probe)(model\"()\")))(1660a4a4-d8e4-4cbd-aa67-a826659c13f8((kind \
         Probe)(model\"()\")))(e4b9ba23-e935-4071-8d21-32600845fd50((kind \
         Probe)(model\"()\")))(a77f813c-63e8-44ab-a730-5509acc4bc72((kind \
         Probe)(model\"()\")))(671aed96-2238-4215-beaf-4a5cdc9938dc((kind \
         Probe)(model\"()\")))(40534131-1834-4475-bbbd-475c5d0bcc24((kind \
         Probe)(model\"()\"))))";
    } )
