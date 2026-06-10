let out : string * Haz3lcore.PersistentSegment.t =
  ( "Study / tasks-draft / 05W-log-cleaner",
    {
      segment =
        "((Secondary((id \
         ada8d125-c86d-4484-b083-0ca059c9722b)(content(Comment\"# Moonphase \
         Log Cleaner v2                             #\"))))(Secondary((id \
         1acb3e6e-9294-4e6f-b41d-9aff6c0633c3)(content(Whitespace\"\\n\"))))(Secondary((id \
         e44cf56c-7787-4521-b3cf-24ef9b2ab964)(content(Comment\"#                                                      \
         #\"))))(Secondary((id \
         6de37989-e94c-48bb-90ae-1804dd9a4c9b)(content(Whitespace\"\\n\"))))(Secondary((id \
         e7ff03d0-bd5e-46c3-ba7a-b2c4c26eefad)(content(Comment\"# Garden \
         keepers record observations in a messy        #\"))))(Secondary((id \
         e8d61786-60e2-46b2-9f78-945d2aad1f7a)(content(Whitespace\"\\n\"))))(Secondary((id \
         370130f8-e17a-4ae9-87f6-525459099b03)(content(Comment\"# log with \
         entry numbers, emoji markers, inconsistent  #\"))))(Secondary((id \
         d503c981-4f29-41eb-beda-121a563ddae4)(content(Whitespace\"\\n\"))))(Secondary((id \
         bb183c2e-80f6-4c4c-9182-638f48ec7859)(content(Comment\"# dashes, and \
         extra whitespace. Implement clean_entry  #\"))))(Secondary((id \
         26452de1-24a5-46ff-bd74-ab214c09ab57)(content(Whitespace\"\\n\"))))(Secondary((id \
         10ffadb8-0a69-445a-aff5-3d598f16c537)(content(Comment\"# to \
         standardize each log entry.                       \
         #\"))))(Secondary((id \
         64c28626-94ac-4101-887a-68a07f884370)(content(Whitespace\"\\n\"))))(Secondary((id \
         cb61e284-1635-4989-a6c0-989b72fd3496)(content(Comment\"#                                                      \
         #\"))))(Secondary((id \
         3785e070-773a-47a7-9907-09c381d9df1c)(content(Whitespace\"\\n\"))))(Secondary((id \
         acd783ed-0ecf-4b5e-bfec-f54005343943)(content(Comment\"# Each raw \
         entry has a number marker (a hash sign      #\"))))(Secondary((id \
         8215ae8f-bd46-4dea-9f2f-8c8dbf013246)(content(Whitespace\"\\n\"))))(Secondary((id \
         e68e60c1-75b8-4519-84be-faeeda26dc72)(content(Comment\"# then \
         digits), a moon emoji, a phase name, and        #\"))))(Secondary((id \
         2ddb8226-0bec-4b44-8498-0e32bf643955)(content(Whitespace\"\\n\"))))(Secondary((id \
         4167daf8-9515-4534-b548-92ee1fa28aad)(content(Comment\"# notes after \
         dashes. See the test cases below         #\"))))(Secondary((id \
         6bf0ae35-4f8c-4bbc-8a1a-0d3a80f942c0)(content(Whitespace\"\\n\"))))(Secondary((id \
         e684e421-a082-4bc0-b7b0-c0b633118cc9)(content(Comment\"# for \
         examples.                                        \
         #\"))))(Secondary((id \
         a0a6d9c7-13d8-4f3d-b8e7-d176f045fe53)(content(Whitespace\"\\n\"))))(Secondary((id \
         26f5fbaf-d354-42c7-8ce2-de85b07ed7f5)(content(Comment\"#                                                      \
         #\"))))(Secondary((id \
         1cd28a60-2496-46d2-97be-03596c6217b6)(content(Whitespace\"\\n\"))))(Secondary((id \
         e2c3c82d-4c88-48f2-8405-63106637d4bf)(content(Comment\"# Cleaned \
         entries should look like:                    #\"))))(Secondary((id \
         288ad851-68d8-436a-bee8-c41257413fa7)(content(Whitespace\"\\n\"))))(Secondary((id \
         5064d18d-1063-4443-91a5-1a43883b2b56)(content(Comment\"#   \
         \\\"\\240\\159\\140\\149 Full Moon: clear skies, planted \
         moonbloom\\\"     #\"))))(Secondary((id \
         43b4e2b5-e373-4078-9ea3-b1f326f1e772)(content(Whitespace\"\\n\"))))(Secondary((id \
         d9a6ba30-f885-433c-8f87-0774d098eac7)(content(Comment\"#   \
         \\\"\\240\\159\\140\\145 New Moon: cloudy, harvested \
         starfern\\\"          #\"))))(Secondary((id \
         40d66165-ce54-43bd-8310-ecf478372bff)(content(Whitespace\"\\n\"))))(Secondary((id \
         3866ebe7-f1e0-4633-af49-87989638a4a1)(content(Comment\"#                                                      \
         #\"))))(Secondary((id \
         93156f98-e289-4bc3-bc0d-5fc95885ad90)(content(Whitespace\"\\n\"))))(Secondary((id \
         afc64b68-6500-42cd-8755-d7ea55eef47e)(content(Comment\"# The moon \
         emojis stay! Only the entry numbers         #\"))))(Secondary((id \
         e9c24dae-b113-4a42-8b77-e4df71eab746)(content(Whitespace\"\\n\"))))(Secondary((id \
         941ee5c7-0c8d-4ccd-a9db-b4bc7f2402c9)(content(Comment\"# should be \
         removed.                                   #\"))))(Secondary((id \
         4d609fd2-975d-491e-a9a2-0f43074e9b5b)(content(Whitespace\"\\n\"))))(Secondary((id \
         a04b12b6-2ec0-41aa-9098-5038f0936c36)(content(Comment\"#                                                      \
         #\"))))(Secondary((id \
         2a113215-b0f8-4dfe-97e1-b65356d65d8b)(content(Whitespace\"\\n\"))))(Secondary((id \
         065f9011-7714-45d8-ad04-d3dd255082a2)(content(Comment\"# \
         Steps:                                               \
         #\"))))(Secondary((id \
         6df93e56-6370-4914-88de-c32b3d115e72)(content(Whitespace\"\\n\"))))(Secondary((id \
         f0d35a76-31ec-4fb4-ad6e-8e1dc9dd413e)(content(Comment\"#   1. Trim \
         leading/trailing whitespace                #\"))))(Secondary((id \
         f6e29984-dbba-4508-a764-7df4b05eae2f)(content(Whitespace\"\\n\"))))(Secondary((id \
         cf32e93b-5763-4600-ab9d-d21b23b5b6d2)(content(Comment\"#   2. Remove \
         entry numbers (hash followed by digits)  #\"))))(Secondary((id \
         8c01ebfa-4776-4930-b35a-dd23d4f7db7d)(content(Whitespace\"\\n\"))))(Secondary((id \
         3c16b83f-16d9-4dcf-ab38-4d497d310334)(content(Comment\"#   3. \
         Normalize \\\" -- \\\" or \\\"--\\\" into \\\": \\\"              \
         #\"))))(Secondary((id \
         5a59cf84-a7a0-42b7-8ef2-586c68f294ff)(content(Whitespace\"\\n\"))))(Secondary((id \
         25dbda09-c424-4dba-8958-f06f4e18456d)(content(Comment\"#   4. \
         Collapse multiple spaces into one               #\"))))(Secondary((id \
         ee2754cb-1b05-4a6c-b6ad-b93334366294)(content(Whitespace\"\\n\"))))(Secondary((id \
         9b79198e-cf28-41a4-8b0f-880689bd6996)(content(Comment\"#   5. Final \
         trim for any leftover edge spaces         #\"))))(Secondary((id \
         193ad2fe-9ff6-4c31-8b53-96e790d06ea4)(content(Whitespace\"\\n\"))))(Secondary((id \
         97738da5-fd9b-43b4-8e5a-8f1b45747ea8)(content(Comment\"#                                                      \
         #\"))))(Secondary((id \
         17366258-756e-4210-99a5-01b445c8224f)(content(Whitespace\"\\n\"))))(Secondary((id \
         f70663c2-72ee-4eeb-b2bc-e12da55ff3f9)(content(Comment\"# Some \
         standard library functions that may be \
         useful:                                 #\"))))(Secondary((id \
         5b7b40b3-2ce8-4beb-b1ea-3271c1d0f579)(content(Whitespace\"\\n\"))))(Secondary((id \
         39904d5c-3491-4ce0-a068-c6790753c2a4)(content(Comment\"#   \
         string_trim: String -> String                      \
         #\"))))(Secondary((id \
         e8620190-4e59-4cbe-9539-a269da264a34)(content(Whitespace\"\\n\"))))(Secondary((id \
         0a056a2d-f201-4a74-b08d-25067d91ae06)(content(Comment\"#   \
         string_match: (String, String) -> Bool             \
         #\"))))(Secondary((id \
         899b697a-c917-4400-838c-e60a279d0c8a)(content(Whitespace\"\\n\"))))(Secondary((id \
         ee36d185-da99-4db6-b920-4a60b04c7dd7)(content(Comment\"#   \
         string_replace: (String, String, String) -> String \
         #\"))))(Secondary((id \
         e67075f8-3f75-41aa-add9-0096df58aa0e)(content(Whitespace\"\\n\"))))(Secondary((id \
         4b3d5ae7-6e40-41ab-8c82-56ae077f3313)(content(Comment\"#                                                      \
         #\"))))(Secondary((id \
         10f5a24b-8fd4-4b13-b8fb-95e49d890d00)(content(Whitespace\"\\n\"))))(Secondary((id \
         6dcea7dd-f44a-4f96-bf4c-eedd97334f0c)(content(Comment\"# These \
         functions are tragically underdocumented!      #\"))))(Secondary((id \
         efb07237-eae3-415d-bb05-dcd24d381ac4)(content(Whitespace\"\\n\"))))(Secondary((id \
         18125b62-0db4-4d9d-80bb-9f3e7a8125e4)(content(Comment\"# You will \
         have to figure out what those String        #\"))))(Secondary((id \
         391c6486-9a96-444b-b3ac-27a0d04464d4)(content(Whitespace\"\\n\"))))(Secondary((id \
         4ce7c0a6-8b5a-43bb-9d7e-f998dd6cb1eb)(content(Comment\"# arguments \
         mean by experimenting with probes.         #\"))))(Secondary((id \
         774aa63c-c2ca-4296-aa15-b92e4e066679)(content(Whitespace\"\\n\"))))(Secondary((id \
         5241970a-c2ef-4a54-b0e9-82c21a19d120)(content(Comment\"#                                                      \
         #\"))))(Secondary((id \
         a33ea64d-9598-447d-865a-8aaeba29b60a)(content(Whitespace\"\\n\"))))(Secondary((id \
         767ec326-883c-41a8-8822-8be25ef5d518)(content(Comment\"# One of the \
         String arguments is a regex pattern.      #\"))))(Secondary((id \
         f0d556a0-8e0d-4493-81cd-f4802171b6c7)(content(Whitespace\"\\n\"))))(Secondary((id \
         6d443ff1-25d7-440b-afab-a47c14230d4d)(content(Comment\"# Some useful \
         regex building blocks:                   #\"))))(Secondary((id \
         11dd3f3f-aa50-46c2-9177-359765f814fd)(content(Whitespace\"\\n\"))))(Secondary((id \
         ab94ccd0-fdda-463f-a728-084c26123a90)(content(Comment\"#   + means \
         \\\"one or more of the preceding\\\"             \
         #\"))))(Secondary((id \
         0e19d447-ea31-4c79-9b02-d50dc4d610b7)(content(Whitespace\"\\n\"))))(Secondary((id \
         2a9df368-d14b-4d3a-9489-2cd68fa0692d)(content(Comment\"#   * means \
         \\\"zero or more of the preceding\\\"            \
         #\"))))(Secondary((id \
         15e58065-b752-4380-8c53-a309b9232a91)(content(Whitespace\"\\n\"))))(Secondary((id \
         928f555f-0b13-4351-9c9a-d2592c691cc7)(content(Comment\"#   [abc] \
         matches any one character from the set       #\"))))(Secondary((id \
         06c2e839-bb7d-4ef0-abc2-6e64b0037b7e)(content(Whitespace\"\\n\"))))(Secondary((id \
         154ab08e-263f-4d72-befa-8aca119ac1d7)(content(Comment\"#   [0-9] \
         matches any digit                            #\"))))(Secondary((id \
         0a0314a4-8e8b-405c-b5ee-40491f9f54b9)(content(Whitespace\"\\n\"))))(Secondary((id \
         70db8382-2d29-4927-9b43-988ac569fc98)(content(Comment\"#   A space in \
         a pattern matches a literal space       #\"))))(Secondary((id \
         3dee538c-1f33-4427-b134-57ad30e9ac1a)(content(Whitespace\"\\n\"))))(Secondary((id \
         51e3457a-8502-4fe3-b49f-5b70359dd9eb)(content(Comment\"#                                                      \
         #\"))))(Secondary((id \
         fc0d83d4-8ec8-4943-9208-57e11f8855ce)(content(Whitespace\"\\n\"))))(Secondary((id \
         f97d5b5e-da89-4422-8aa8-473e474cfd57)(content(Comment\"# Tip: Build \
         one step at a time! After each line,      #\"))))(Secondary((id \
         6121c220-0479-4294-999a-d5cc185a2eca)(content(Whitespace\"\\n\"))))(Secondary((id \
         70cab5e0-458e-480c-a362-2e7c5ea09743)(content(Comment\"# check the \
         probe to see what your pattern did.        #\"))))(Secondary((id \
         f7295ae6-011d-47af-b0ca-183aa8aa88ad)(content(Whitespace\"\\n\"))))(Secondary((id \
         8e589d96-e8be-400f-ae06-eb4e657140f6)(content(Whitespace\"\\n\"))))(Tile((id \
         2c61eb48-f228-4e87-83a6-0bcd81f7c996)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         0060b852-41c6-4ff3-a6fd-6cad3944cab5)(content(Whitespace\" \
         \"))))(Tile((id \
         80a4d2f1-01bc-44f8-9b3e-5be4ce69a017)(label(clean_entry))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         5f920a6d-b97f-40f5-a3d3-73da214c48e1)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 24))(sort Pat))((shape(Concave \
         24))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         2c4cb64c-ab4f-47ca-9bc3-335147fd0134)(content(Whitespace\" \
         \"))))(Tile((id \
         ab16c6d5-b524-4126-a200-d36ebbcd12b4)(label(String))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         4b9a13bd-88ca-4d4f-ac26-8afa463d4544)(content(Whitespace\" \
         \"))))(Tile((id \
         eddbe2f6-693e-48e1-8f4a-b77cc3e3b6bc)(label(->))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 13))(sort Typ))((shape(Concave \
         13))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         4eedefb5-74b0-45fb-b1a5-527e3f57580e)(content(Whitespace\" \
         \"))))(Tile((id \
         ad96df6c-138a-4eb3-a6e0-d6e98aa7631b)(label(String))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         b3d8bd92-9da6-4200-87a5-4206d39d232c)(content(Whitespace\" \
         \")))))((Secondary((id \
         80d6dabc-e3eb-498e-aacc-9e4652c227a3)(content(Whitespace\" \
         \"))))(Tile((id b71648df-4eff-4e04-8498-285d3edecb54)(label(fun \
         ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 37))(sort Exp))))))(shards(0 \
         1))(children(((Secondary((id \
         9f15ca7f-05e1-4f50-a79d-e2be1c3317c6)(content(Whitespace\" \
         \"))))(Tile((id \
         f3a96305-ab7f-478f-91c0-9ca1be47772b)(label(entry))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         9256f698-871f-46b1-9bbf-7e2ff2f60cf8)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         c57c9932-e501-4737-9175-4772ce068c23)(content(Whitespace\"\\n\"))))(Tile((id \
         10842faf-43c3-4b33-a778-5fbf90c71a5a)(label(?))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         22836163-fcc1-4fd8-bf3e-3c16390b42d4)(content(Whitespace\"\\n\"))))(Secondary((id \
         eec30830-bd38-481f-a37f-9b3461bf7f67)(content(Whitespace\"\\n\"))))(Secondary((id \
         fd6f3d0f-0035-46ec-a7be-42eb0ffddef3)(content(Whitespace\"\\n\"))))(Secondary((id \
         aac6b2f2-29a3-4f58-a2d0-f867fe454464)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         bd31fd5e-c0a7-4658-899b-eb8db2ba7540)(content(Whitespace\"\\n\"))))(Secondary((id \
         37ae2f53-a0c4-45ee-9465-7af4a046305a)(content(Whitespace\"\\n\"))))(Tile((id \
         fc18614b-3392-40a3-98cb-5f45625dc42b)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         6473958e-186e-4278-a8b0-84c0e44575f3)(content(Whitespace\"\\n\"))))(Tile((id \
         d689dc64-5ab3-4152-9068-97c45b161b2d)(label(clean_entry))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         652313fb-c3e6-48a7-b7b5-0c290f5cb9d5)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         43ebd412-aea1-45f4-945b-dc60b2378b4b)(label(\"\\\"  #42 \
         \\240\\159\\140\\149 Full Moon -- clear skies, planted moonbloom  \
         \\\"\"))(mold((out Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0))(children()))))))))(Secondary((id \
         f06ee310-7453-4687-9fde-6ba10f4831e7)(content(Whitespace\" \
         \"))))(Tile((id \
         7f94d33b-dc5f-4d90-a7a7-0ea23040b6e9)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         ccda6065-b728-4744-a783-8053c1438d98)(content(Whitespace\"\\n\"))))(Tile((id \
         e7a2a8a4-7a9c-49b5-a349-d2b4edfb6071)(label(\"\\\"\\240\\159\\140\\149 \
         Full Moon: clear skies, planted moonbloom\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         1c4f8a88-b66d-4a1a-9e85-cf0f6bb8ce34)(content(Whitespace\"\\n\")))))))))(Tile((id \
         27284954-5212-4ce6-9cb3-327ba8252213)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         68f3910a-8146-432e-89b3-3e25de18bc9c)(content(Whitespace\"\\n\"))))(Secondary((id \
         e688edd8-5d77-4776-be3f-f5a92dd6797d)(content(Whitespace\"\\n\"))))(Tile((id \
         52073f11-7544-469c-9d43-24e146a4b747)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         5150a52d-7f18-4428-8780-7b0c66676c52)(content(Whitespace\"\\n\"))))(Tile((id \
         d484a5b8-aaef-498d-88de-03433be6a23a)(label(clean_entry))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         5acf623d-2028-4df7-b71c-5b91f6fd43b0)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         650b43cd-58a9-4a0f-a19a-4f32b7f1ea77)(label(\"\\\"#7 \
         \\240\\159\\140\\145  New Moon--cloudy,   harvested \
         starfern\\\"\"))(mold((out Exp)(in_())(nibs(((shape Convex)(sort \
         Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         2e00e999-fc98-4b5e-906e-9b1570ac4d41)(content(Whitespace\" \
         \"))))(Tile((id \
         a5f30d2a-4907-464c-9034-6a4559ed866d)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         b4864c26-9ec9-460a-bb9b-11f349a7dab1)(content(Whitespace\"\\n\"))))(Tile((id \
         ef6da8f0-850d-4ca4-97e4-bd94d5deb483)(label(\"\\\"\\240\\159\\140\\145 \
         New Moon: cloudy, harvested starfern\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         18efa820-bf06-4fb7-b7aa-ceeb12c42478)(content(Whitespace\"\\n\")))))))))(Tile((id \
         17895896-8fb4-469c-8546-64de640e8428)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         97581399-7582-4d66-8ed8-4a39294fc03a)(content(Whitespace\"\\n\"))))(Secondary((id \
         87483747-695d-4bad-bf07-dee7c00b13e2)(content(Whitespace\"\\n\"))))(Tile((id \
         fc13b1aa-4020-4711-a127-c8ab9ea4eec7)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         e9284bde-33e5-42b9-874c-57a1ab5025a7)(content(Whitespace\"\\n\"))))(Tile((id \
         204d43ce-7d3b-441d-85e6-cf47cd56e19f)(label(clean_entry))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         2b5104a7-dbb8-4c26-adfc-4a21f84673e1)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         30d43452-740b-418c-87bc-d646afce6273)(label(\"\\\"  #103 \
         \\240\\159\\140\\147 Half Moon -- light rain, pruned duskrose  \
         \\\"\"))(mold((out Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0))(children()))))))))(Secondary((id \
         1028e11c-cce9-466d-a1db-50a6dca515be)(content(Whitespace\" \
         \"))))(Tile((id \
         76a6a6fa-df52-4b47-8544-9a056c6388c7)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         d5b1983e-8325-43fe-ba20-a2186f5dca9b)(content(Whitespace\"\\n\"))))(Tile((id \
         1abe12cb-047f-4874-b110-9ac76c52cec4)(label(\"\\\"\\240\\159\\140\\147 \
         Half Moon: light rain, pruned duskrose\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         baca7d8b-e52f-42dd-867f-5bfb3bf9295f)(content(Whitespace\"\\n\")))))))))(Tile((id \
         eb3d204f-3cdb-4dcf-b722-27baca66c420)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         956e9e51-c815-43aa-8c47-b53bfa8b7a9a)(content(Whitespace\"\\n\"))))(Secondary((id \
         c698fdd1-ca3f-48bc-a955-f9d6d227e954)(content(Whitespace\"\\n\"))))(Tile((id \
         690cd136-9592-40f6-bab5-025ef2b5bcfc)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         9b3faa86-55dc-4259-a42a-1c57de3b76b0)(content(Whitespace\"\\n\"))))(Tile((id \
         74ca1c71-3eb9-4150-99a3-01e58023c258)(label(clean_entry))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         9f82fd3c-54cf-40c7-b17a-55ac9f78494b)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         613a87c4-b57f-464a-9d1a-00943366f334)(label(\"\\\"#15 \
         \\240\\159\\140\\151 Crescent--foggy,  checked   moth \
         traps\\\"\"))(mold((out Exp)(in_())(nibs(((shape Convex)(sort \
         Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         948b302c-3168-4105-bb93-b28d9b49b3d4)(content(Whitespace\" \
         \"))))(Tile((id \
         7501c1cb-4963-413d-b1ef-e56e21f3afd6)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         35833bfd-d67d-4193-94a8-7bf62831d41e)(content(Whitespace\"\\n\"))))(Tile((id \
         63095468-c5ce-4e25-9e97-f41bbed6fa04)(label(\"\\\"\\240\\159\\140\\151 \
         Crescent: foggy, checked moth traps\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         2fe37332-96df-4da9-a269-c33618d0ce00)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         83341497-0ac6-4444-94e1-49023056c6c7)(content(Whitespace\"\\n\")))))";
      backup_text =
        "# Moonphase Log Cleaner v2                             #\n\
         #                                                      #\n\
         # Garden keepers record observations in a messy        #\n\
         # log with entry numbers, emoji markers, inconsistent  #\n\
         # dashes, and extra whitespace. Implement clean_entry  #\n\
         # to standardize each log entry.                       #\n\
         #                                                      #\n\
         # Each raw entry has a number marker (a hash sign      #\n\
         # then digits), a moon emoji, a phase name, and        #\n\
         # notes after dashes. See the test cases below         #\n\
         # for examples.                                        #\n\
         #                                                      #\n\
         # Cleaned entries should look like:                    #\n\
         #   \"\240\159\140\149 Full Moon: clear skies, planted \
         moonbloom\"     #\n\
         #   \"\240\159\140\145 New Moon: cloudy, harvested \
         starfern\"          #\n\
         #                                                      #\n\
         # The moon emojis stay! Only the entry numbers         #\n\
         # should be removed.                                   #\n\
         #                                                      #\n\
         # Steps:                                               #\n\
         #   1. Trim leading/trailing whitespace                #\n\
         #   2. Remove entry numbers (hash followed by digits)  #\n\
         #   3. Normalize \" -- \" or \"--\" into \": \"              #\n\
         #   4. Collapse multiple spaces into one               #\n\
         #   5. Final trim for any leftover edge spaces         #\n\
         #                                                      #\n\
         # Some standard library functions that may be \
         useful:                                 #\n\
         #   string_trim: String -> String                      #\n\
         #   string_match: (String, String) -> Bool             #\n\
         #   string_replace: (String, String, String) -> String #\n\
         #                                                      #\n\
         # These functions are tragically underdocumented!      #\n\
         # You will have to figure out what those String        #\n\
         # arguments mean by experimenting with probes.         #\n\
         #                                                      #\n\
         # One of the String arguments is a regex pattern.      #\n\
         # Some useful regex building blocks:                   #\n\
         #   + means \"one or more of the preceding\"             #\n\
         #   * means \"zero or more of the preceding\"            #\n\
         #   [abc] matches any one character from the set       #\n\
         #   [0-9] matches any digit                            #\n\
         #   A space in a pattern matches a literal space       #\n\
         #                                                      #\n\
         # Tip: Build one step at a time! After each line,      #\n\
         # check the probe to see what your pattern did.        #\n\n\
         let clean_entry: String -> String = fun entry ->\n\
         ?\n\n\n\n\
         in\n\n\
         test\n\
         clean_entry(\"  #42 \240\159\140\149 Full Moon -- clear skies, \
         planted moonbloom  \") ==\n\
         \"\240\159\140\149 Full Moon: clear skies, planted moonbloom\"\n\
         end;\n\n\
         test\n\
         clean_entry(\"#7 \240\159\140\145  New Moon--cloudy,   harvested \
         starfern\") ==\n\
         \"\240\159\140\145 New Moon: cloudy, harvested starfern\"\n\
         end;\n\n\
         test\n\
         clean_entry(\"  #103 \240\159\140\147 Half Moon -- light rain, pruned \
         duskrose  \") ==\n\
         \"\240\159\140\147 Half Moon: light rain, pruned duskrose\"\n\
         end;\n\n\
         test\n\
         clean_entry(\"#15 \240\159\140\151 Crescent--foggy,  checked   moth \
         traps\") ==\n\
         \"\240\159\140\151 Crescent: foggy, checked moth traps\"\n\
         end\n";
      refractors = "()";
    } )
