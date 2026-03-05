let out : string * Haz3lcore.PersistentSegment.t =
  ( "Study / writing / log-cleaner / log-cleaner-v2-sketch",
    {
      segment =
        "((Secondary((id \
         6ad84539-b20c-4cb2-9c8a-017a3a464204)(content(Comment\"# Moonphase \
         Log Cleaner v2                             #\"))))(Secondary((id \
         30e44aec-8de3-4937-8d47-c1de0b48a4d1)(content(Whitespace\"\\n\"))))(Secondary((id \
         2cd8e843-ca3c-4245-bf02-b368a811ab10)(content(Comment\"#                                                      \
         #\"))))(Secondary((id \
         6efef985-f39f-4cbe-a3c7-01453aa6d521)(content(Whitespace\"\\n\"))))(Secondary((id \
         b8337f00-7cd2-415f-be99-9ae485114f3a)(content(Comment\"# Garden \
         keepers record observations in a messy        #\"))))(Secondary((id \
         de0fcff9-c009-4b11-a948-6b1524884df7)(content(Whitespace\"\\n\"))))(Secondary((id \
         fa50e5ec-26fb-412b-88d1-af5f57feb607)(content(Comment\"# log with \
         entry numbers, emoji markers, inconsistent  #\"))))(Secondary((id \
         5645cb92-8852-4cd9-a39b-943509864fe5)(content(Whitespace\"\\n\"))))(Secondary((id \
         73c64d8e-ffa2-438d-94c4-1367205faeff)(content(Comment\"# dashes, and \
         extra whitespace. Implement clean_entry  #\"))))(Secondary((id \
         ab18a340-b698-4e0c-9b07-de9141f23cb9)(content(Whitespace\"\\n\"))))(Secondary((id \
         a8d1050f-0cb6-4f74-91d5-bd23bcc6fd6b)(content(Comment\"# to \
         standardize each log entry.                       \
         #\"))))(Secondary((id \
         28fc8e1d-2345-4e0a-a75e-d8e2334f53df)(content(Whitespace\"\\n\"))))(Secondary((id \
         782b4c26-5b9b-4b48-b097-d29a27a0871c)(content(Comment\"#                                                      \
         #\"))))(Secondary((id \
         dc49b7d7-d89c-4eaa-a494-7a21ed941eb7)(content(Whitespace\"\\n\"))))(Secondary((id \
         48812edb-35d8-4c58-b408-75ab31e65d33)(content(Comment\"# Each raw \
         entry has a number marker (a hash sign      #\"))))(Secondary((id \
         6adc5364-813a-4190-b8cd-6896c8454de9)(content(Whitespace\"\\n\"))))(Secondary((id \
         72e819bf-bb11-49cd-846a-fa49ab5a43c2)(content(Comment\"# then \
         digits), a moon emoji, a phase name, and        #\"))))(Secondary((id \
         8b13150f-f551-4a12-9371-224006b3da95)(content(Whitespace\"\\n\"))))(Secondary((id \
         2d6241c3-54d6-48bc-9f1e-3cc19f551b49)(content(Comment\"# notes after \
         dashes. See the test cases below         #\"))))(Secondary((id \
         0b6c67e5-dbbe-44c8-88e6-823d14166744)(content(Whitespace\"\\n\"))))(Secondary((id \
         5719b46b-d840-41e0-8551-73941ffcadbe)(content(Comment\"# for \
         examples.                                        \
         #\"))))(Secondary((id \
         3b52ce66-ffcf-4b92-8f57-3b65445c6217)(content(Whitespace\"\\n\"))))(Secondary((id \
         489c21ee-5c7b-4d48-afb8-95b073d14796)(content(Comment\"#                                                      \
         #\"))))(Secondary((id \
         795f4b1f-0f43-4ce3-b3c1-72e36990391c)(content(Whitespace\"\\n\"))))(Secondary((id \
         dea371ce-7100-4d16-9835-7d24011331fb)(content(Comment\"# Cleaned \
         entries should look like:                    #\"))))(Secondary((id \
         b7c19722-7815-4587-a8a8-08f74e1d1442)(content(Whitespace\"\\n\"))))(Secondary((id \
         b7387e18-6ad6-4d2a-87cf-98a9fae16253)(content(Comment\"#   \
         \\\"\\240\\159\\140\\149 Full Moon: clear skies, planted \
         moonbloom\\\"     #\"))))(Secondary((id \
         b6bbd708-ee19-4438-846c-644af8b2e456)(content(Whitespace\"\\n\"))))(Secondary((id \
         142f8cad-4d34-4174-a838-02ad562bf69a)(content(Comment\"#   \
         \\\"\\240\\159\\140\\145 New Moon: cloudy, harvested \
         starfern\\\"          #\"))))(Secondary((id \
         b28895a1-985c-43e2-add0-44dcdce0dae5)(content(Whitespace\"\\n\"))))(Secondary((id \
         853f37f3-4939-4df5-9e11-f7a84f3bda2d)(content(Comment\"#                                                      \
         #\"))))(Secondary((id \
         ae55d28f-1df4-44a1-8978-9f6cb5717317)(content(Whitespace\"\\n\"))))(Secondary((id \
         fa2f9052-18d4-47dc-8c00-2e38ea598ae3)(content(Comment\"# The moon \
         emojis stay! Only the entry numbers         #\"))))(Secondary((id \
         66218dbd-6c15-43fe-98f0-eed39352940b)(content(Whitespace\"\\n\"))))(Secondary((id \
         23141f04-713c-480e-9b11-6d4aa8b3f429)(content(Comment\"# should be \
         removed.                                   #\"))))(Secondary((id \
         6034b79e-1689-41e4-99ca-b0f9383a0df1)(content(Whitespace\"\\n\"))))(Secondary((id \
         762e32fe-1a8c-47c8-8b03-baa86196595a)(content(Comment\"#                                                      \
         #\"))))(Secondary((id \
         b0a52f5e-ef9b-4652-9751-227b4ec10b42)(content(Whitespace\"\\n\"))))(Secondary((id \
         054883a4-b733-434c-a005-0e3b571af32a)(content(Comment\"# \
         Steps:                                               \
         #\"))))(Secondary((id \
         eccbbb4b-c926-4160-8d8d-ecafffd4065b)(content(Whitespace\"\\n\"))))(Secondary((id \
         b999143b-c77e-4d43-bb2a-f2f22a58dd43)(content(Comment\"#   1. Trim \
         leading/trailing whitespace                #\"))))(Secondary((id \
         d700db04-cc91-4101-acab-95c79b86ced4)(content(Whitespace\"\\n\"))))(Secondary((id \
         554be622-ed0e-4409-b74e-7068e16fdb23)(content(Comment\"#   2. Remove \
         entry numbers (hash followed by digits)  #\"))))(Secondary((id \
         6b5bdfa5-47ae-424a-9341-b186a8dfbde0)(content(Whitespace\"\\n\"))))(Secondary((id \
         7f061fb0-0a8f-4095-809f-baf6cdb7bd02)(content(Comment\"#   3. \
         Normalize \\\" -- \\\" or \\\"--\\\" into \\\": \\\"              \
         #\"))))(Secondary((id \
         8320f7df-161f-4545-a4ef-af1fff4e50d0)(content(Whitespace\"\\n\"))))(Secondary((id \
         e00dbaee-24d2-4d6e-9924-ca343029d803)(content(Comment\"#   4. \
         Collapse multiple spaces into one               #\"))))(Secondary((id \
         1c2f5316-2537-48eb-b3e2-62d63b2cde3a)(content(Whitespace\"\\n\"))))(Secondary((id \
         0c8bcd48-2a5f-4902-88fc-1167ec249d13)(content(Comment\"#   5. Final \
         trim for any leftover edge spaces         #\"))))(Secondary((id \
         cdd018d9-623c-4c4c-877c-32330169c260)(content(Whitespace\"\\n\"))))(Secondary((id \
         83e23c5c-262d-476d-ae76-2fb00a38364e)(content(Comment\"#                                                      \
         #\"))))(Secondary((id \
         34f9207e-ddaf-4c3b-b757-3afdea2c88ad)(content(Whitespace\"\\n\"))))(Secondary((id \
         61be1d1a-9a3f-472c-9bad-cd4ada75357f)(content(Comment\"# Available \
         functions:                                 #\"))))(Secondary((id \
         13d15e38-b8eb-4100-898d-7920b86e3fa6)(content(Whitespace\"\\n\"))))(Secondary((id \
         b8fc7d98-78c8-4551-a22b-84f2fddd1af4)(content(Comment\"#   \
         string_trim: String -> String                      \
         #\"))))(Secondary((id \
         2e9e4b28-578c-4804-aa03-df47ac6ff70e)(content(Whitespace\"\\n\"))))(Secondary((id \
         35c0b6af-86b4-4b5a-b036-e193a896e7a9)(content(Comment\"#   \
         string_replace: (String, String, String) -> String \
         #\"))))(Secondary((id \
         1c174eac-d79b-4112-a393-e8de124e0f8e)(content(Whitespace\"\\n\"))))(Secondary((id \
         a205416b-666f-4fb9-ba9d-641c73cba698)(content(Comment\"#                                                      \
         #\"))))(Secondary((id \
         f3e95ede-7216-476e-9901-bb71868876f2)(content(Whitespace\"\\n\"))))(Secondary((id \
         f6173b81-182f-42ee-9b2f-64b876f7b9d2)(content(Comment\"# These \
         functions are tragically underdocumented!       #\"))))(Secondary((id \
         7e5c113d-81c3-4c3b-bc20-7bd52724f125)(content(Whitespace\"\\n\"))))(Secondary((id \
         8c3c1d11-5cc0-4c38-b22d-c4ff28205b92)(content(Comment\"# You will \
         have to figure out what those String        #\"))))(Secondary((id \
         b54d9a1c-223f-4cbc-9471-93f63bd4e1fe)(content(Whitespace\"\\n\"))))(Secondary((id \
         97bd753d-d974-4335-acc1-160fd46b6924)(content(Comment\"# arguments \
         mean by experimenting with probes.         #\"))))(Secondary((id \
         b97b5061-2727-4270-ae30-689a9e44ae04)(content(Whitespace\"\\n\"))))(Secondary((id \
         42e97140-4790-45f7-bd85-dd0e97092c52)(content(Comment\"#                                                      \
         #\"))))(Secondary((id \
         9b3ed023-b7eb-4103-86e8-10278b6cf172)(content(Whitespace\"\\n\"))))(Secondary((id \
         0e471830-373f-4c1d-93c5-ba8ba6715f7f)(content(Comment\"# One of the \
         String arguments is a regex pattern.      #\"))))(Secondary((id \
         590bd635-d1d6-4afa-afa3-5bec9cfa0791)(content(Whitespace\"\\n\"))))(Secondary((id \
         57591b3b-4bac-44ae-a71c-397374f2a852)(content(Comment\"# Some useful \
         regex building blocks:                   #\"))))(Secondary((id \
         715dfe6b-3ef3-4f97-be1b-1ca11b4c261a)(content(Whitespace\"\\n\"))))(Secondary((id \
         694f5455-2ed2-4485-9ff7-892d4e966a36)(content(Comment\"#   + means \
         \\\"one or more of the preceding\\\"             \
         #\"))))(Secondary((id \
         62fe9e9f-a035-4128-82ce-0b71d7cddab0)(content(Whitespace\"\\n\"))))(Secondary((id \
         faa60a4e-7798-4a47-afd6-d7b7ecf06c75)(content(Comment\"#   * means \
         \\\"zero or more of the preceding\\\"            \
         #\"))))(Secondary((id \
         87e21819-ed42-4eb9-a388-13e57d35568a)(content(Whitespace\"\\n\"))))(Secondary((id \
         b837e139-3f20-4456-9a42-9b5976b5bc7b)(content(Comment\"#   [abc] \
         matches any one character from the set       #\"))))(Secondary((id \
         5e5bc2f1-0801-4dc5-8e5d-32761eff8cb7)(content(Whitespace\"\\n\"))))(Secondary((id \
         2cc6152b-b8f7-4765-862d-6a9a7046ed9c)(content(Comment\"#   [0-9] \
         matches any digit                            #\"))))(Secondary((id \
         eafc8b91-3fe4-4741-ad67-c4d086dc77e5)(content(Whitespace\"\\n\"))))(Secondary((id \
         f21fb242-4c9d-4a57-9942-3faef4af712a)(content(Comment\"#   A space in \
         a pattern matches a literal space       #\"))))(Secondary((id \
         f3291f85-51c2-4089-b9c8-0334dabbab0e)(content(Whitespace\"\\n\"))))(Secondary((id \
         79a3f43b-172d-4e7b-9254-0b2fcce04a82)(content(Comment\"#                                                      \
         #\"))))(Secondary((id \
         b3f74b1e-60e2-4aea-b285-746969405b43)(content(Whitespace\"\\n\"))))(Secondary((id \
         c323f5cf-fb3c-4891-af5f-e4a4dd1fb190)(content(Comment\"# Tip: Build \
         one step at a time! After each line,      #\"))))(Secondary((id \
         7938c34f-7a1e-4987-9551-9b7e999d3778)(content(Whitespace\"\\n\"))))(Secondary((id \
         41c84063-27c0-4a03-8bd6-ac5daf321b08)(content(Comment\"# check the \
         probe to see what your pattern did.        #\"))))(Secondary((id \
         ec98154a-d74d-4b3b-b889-02eb5d0b5f08)(content(Whitespace\"\\n\"))))(Secondary((id \
         e183f294-9fc9-441a-b276-e0811e0fc86a)(content(Whitespace\"\\n\"))))(Tile((id \
         bb0af41b-3ee5-4f36-b8e7-48e251ad379a)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         4f63d493-f203-4f9a-ac42-97a34d87426b)(content(Whitespace\" \
         \"))))(Tile((id \
         3e325e51-ad6f-4257-b21e-d23481a7c7aa)(label(clean_entry))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         83bec210-7dff-4a33-83f9-09b2300385f6)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 24))(sort Pat))((shape(Concave \
         24))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         35cd49fd-f030-49be-997f-8e741b24cd4a)(content(Whitespace\" \
         \"))))(Tile((id \
         dd2f59a1-49b6-43e7-bef6-1f0ac2716221)(label(String))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         85aa0aa7-bf68-4f8c-93ad-03ad7775cd81)(content(Whitespace\" \
         \"))))(Tile((id \
         b6fef30a-7a6b-4103-a0db-206c7c31f15e)(label(->))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 13))(sort Typ))((shape(Concave \
         13))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         972fd6d9-cf00-413a-b92a-8c228cfbb85f)(content(Whitespace\" \
         \"))))(Tile((id \
         d6779516-1d44-4701-83e5-f45dc5ed3b8b)(label(String))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         6a3db1c4-4788-4ca5-8478-562875605d1e)(content(Whitespace\" \
         \")))))((Secondary((id \
         6e89c628-df9b-4c9f-b48d-aaa0784a3d52)(content(Whitespace\" \
         \"))))(Tile((id f4eb790d-3a2f-4708-96e4-549846cf45bb)(label(fun \
         ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 37))(sort Exp))))))(shards(0 \
         1))(children(((Secondary((id \
         90c11660-0b0e-46ca-842b-5b0a17ea61fe)(content(Whitespace\" \
         \"))))(Tile((id \
         397ea8ac-c057-48a6-9e3e-c98238fc8736)(label(entry))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         a538a717-aa40-4a22-b353-2c5046dc4f35)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         7075400a-45e8-424f-9413-b9cd7fb5a996)(content(Whitespace\"\\n\"))))(Tile((id \
         6000be03-9012-4936-853d-526fbb46dc86)(label(?))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         9aea425a-cf47-4c21-8c40-1d132762ee05)(content(Whitespace\"\\n\"))))(Secondary((id \
         07680da4-6e6b-46e0-91b2-96ea137bf44e)(content(Whitespace\"\\n\"))))(Secondary((id \
         6838b8e6-e6c3-4f39-ba05-56c0f8464ab3)(content(Whitespace\"\\n\"))))(Secondary((id \
         044f2d61-9be2-4aa0-999d-fb3691d55d84)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         df440f33-83c5-42ae-89db-8cd9e44d5416)(content(Whitespace\"\\n\"))))(Secondary((id \
         cef9a66a-5df6-4637-9488-b9091cc5d27a)(content(Whitespace\"\\n\"))))(Tile((id \
         27f13699-4fb8-4207-9103-0922ab665cdc)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         6778289c-6f17-48c6-a103-185ef087f6c7)(content(Whitespace\"\\n\"))))(Tile((id \
         b2c7e200-51f6-483f-b86b-50e6d760f859)(label(clean_entry))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         bef43388-731e-448c-93ef-19b7849f8a56)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         9b707dd0-59ba-47c9-ad11-691491c13b11)(label(\"\\\"  #42 \
         \\240\\159\\140\\149 Full Moon -- clear skies, planted moonbloom  \
         \\\"\"))(mold((out Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0))(children()))))))))(Secondary((id \
         b6aff927-825c-4960-b506-55a30c5159f6)(content(Whitespace\" \
         \"))))(Tile((id \
         dfac6a56-1496-4fff-a367-bdce4c7aeba5)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         300fb416-bce6-4f15-9e55-f2c336111a45)(content(Whitespace\"\\n\"))))(Tile((id \
         0b89e775-439c-455f-912d-4f891b1a2d00)(label(\"\\\"\\240\\159\\140\\149 \
         Full Moon: clear skies, planted moonbloom\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         eef977d1-b228-4e91-8104-ee4403420e1e)(content(Whitespace\"\\n\")))))))))(Tile((id \
         cfc1f003-7a87-4b74-b52e-3eab5fac37da)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         3f32b6cf-470e-4310-872b-e0f281873342)(content(Whitespace\"\\n\"))))(Secondary((id \
         6c046b2d-eac1-4023-a4a7-31ca323fe09d)(content(Whitespace\"\\n\"))))(Tile((id \
         dd4a1737-da54-489d-aea4-1ece8d97d07a)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         23e7477c-463c-41c9-8a16-f21601a48598)(content(Whitespace\"\\n\"))))(Tile((id \
         eb2936f3-d8e1-4ce0-93a4-7d1b9baf6180)(label(clean_entry))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         ee185688-2386-4394-b9bc-317785699d24)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         cd458324-9ddc-4fff-a21d-51331d6bc246)(label(\"\\\"#7 \
         \\240\\159\\140\\145  New Moon--cloudy,   harvested \
         starfern\\\"\"))(mold((out Exp)(in_())(nibs(((shape Convex)(sort \
         Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         38c256cc-d392-47e0-bd0b-45fc824fc6a8)(content(Whitespace\" \
         \"))))(Tile((id \
         0552ea25-2bbb-4e63-9083-72fb1ccf4d16)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         3f9aad54-4e25-4684-af50-e674f9f17983)(content(Whitespace\"\\n\"))))(Tile((id \
         64c3ea2e-7a00-4825-bbb7-1fa232dd22c3)(label(\"\\\"\\240\\159\\140\\145 \
         New Moon: cloudy, harvested starfern\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         031551f9-4fa1-444e-9b80-cffc06990cb2)(content(Whitespace\"\\n\")))))))))(Tile((id \
         2c612624-5a24-432e-a868-06f4cca2b452)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         6c2b5983-0c88-4a1d-a925-3f5f5bb8e205)(content(Whitespace\"\\n\"))))(Secondary((id \
         6f3c9c3b-de4d-4d81-ac90-d94af37f591d)(content(Whitespace\"\\n\"))))(Tile((id \
         08452776-df93-4e2b-9e56-5f9d9ee2fc4a)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         4e1ddad7-9a6b-4ff4-965f-74cc6210483d)(content(Whitespace\"\\n\"))))(Tile((id \
         31b19edb-99b2-4b3b-95b0-6aadecf9c0d6)(label(clean_entry))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         1e1cc468-7415-4176-9230-dc24588251ab)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         dbe042c5-24b3-4df1-96d0-54b264a4d367)(label(\"\\\"  #103 \
         \\240\\159\\140\\147 Half Moon -- light rain, pruned duskrose  \
         \\\"\"))(mold((out Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0))(children()))))))))(Secondary((id \
         bd8ddafb-98df-40db-ae0e-ca9d7ce025a2)(content(Whitespace\" \
         \"))))(Tile((id \
         b4bd5cc8-00d5-4fd6-9caf-4939a138ae98)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         610321a2-d9e7-4559-af79-d5a8956ca7da)(content(Whitespace\"\\n\"))))(Tile((id \
         f0415cc4-94a3-444c-ba5f-7b59907b712b)(label(\"\\\"\\240\\159\\140\\147 \
         Half Moon: light rain, pruned duskrose\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         0293ad45-743a-470d-a9f6-bc3438f40008)(content(Whitespace\"\\n\")))))))))(Tile((id \
         7ae0de3f-fa4c-41dc-b197-34db45755143)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         b9695403-3c31-4f21-b40f-b34e25489dae)(content(Whitespace\"\\n\"))))(Secondary((id \
         c791300b-4957-4918-a447-35c94a5b50bd)(content(Whitespace\"\\n\"))))(Tile((id \
         5662c153-6735-4ed0-ae41-b8c70c6fa112)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         f7b8066e-a880-4c68-b44c-1a7ccb8aad45)(content(Whitespace\"\\n\"))))(Tile((id \
         dbf5ad22-a1a9-4b43-86fa-5f4f949e1708)(label(clean_entry))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         6288b70b-b1d8-46a7-bda9-fcf9d9e5ce3b)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         72fd8f3e-14e8-4fc0-9bba-2ef4f735182c)(label(\"\\\"#15 \
         \\240\\159\\140\\151 Crescent--foggy,  checked   moth \
         traps\\\"\"))(mold((out Exp)(in_())(nibs(((shape Convex)(sort \
         Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         087905b2-b7f2-40c8-88f9-91a411256a90)(content(Whitespace\" \
         \"))))(Tile((id \
         e1bc1d97-23d6-4b0e-8385-07280f3d4c4d)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         e63cd321-796d-45d0-a605-0c08f405ad61)(content(Whitespace\"\\n\"))))(Tile((id \
         e8d99b3d-f685-444f-80ac-08eb48ea33f1)(label(\"\\\"\\240\\159\\140\\151 \
         Crescent: foggy, checked moth traps\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         ac112bf5-4a40-422a-8b9c-36a90423bb97)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         b7a53c5a-e826-4d26-8d32-bc3cc1e60279)(content(Whitespace\"\\n\")))))";
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
         # Available functions:                                 #\n\
         #   string_trim: String -> String                      #\n\
         #   string_replace: (String, String, String) -> String #\n\
         #                                                      #\n\
         # These functions are tragically underdocumented!       #\n\
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
