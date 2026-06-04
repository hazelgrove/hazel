let out : string * Haz3lcore.PersistentSegment.t =
  ( "Study / writing / log-cleaner / log-cleaner-sketch",
    {
      segment =
        "((Secondary((id \
         b703b2d3-45d1-49c5-acef-ee21a946c25b)(content(Comment\"# Moonphase \
         Log Cleaner                            #\"))))(Secondary((id \
         72711d5b-c530-4664-b347-e782a5c17367)(content(Whitespace\"\\n\"))))(Secondary((id \
         49805a27-1cec-41af-9c86-5364fdb4e056)(content(Comment\"#                                                  \
         #\"))))(Secondary((id \
         3e0f8e03-df2f-4104-a527-8475783d7032)(content(Whitespace\"\\n\"))))(Secondary((id \
         0d182b7f-1fcc-48f3-a8c8-7d3c783666ae)(content(Comment\"# Garden \
         keepers record observations in a messy    #\"))))(Secondary((id \
         6c22fd5b-dcd9-4e18-bd71-cb92b053b491)(content(Whitespace\"\\n\"))))(Secondary((id \
         4e0020d0-c2c2-48bb-9dd5-0e25a1face95)(content(Comment\"# log with \
         emoji markers, inconsistent dashes,     #\"))))(Secondary((id \
         48e590e9-6ad4-42c2-a31a-091d699bb486)(content(Whitespace\"\\n\"))))(Secondary((id \
         ac7fb233-ba78-4be6-80bc-adfdb8ab6982)(content(Comment\"# and extra \
         whitespace. Implement clean_entry to   #\"))))(Secondary((id \
         c44cac30-3f37-42d3-ae11-75a2a86c04ba)(content(Whitespace\"\\n\"))))(Secondary((id \
         2808fb6f-fe9d-4d8f-8ecc-ab54f6e83b6e)(content(Comment\"# standardize \
         each log entry.                      #\"))))(Secondary((id \
         a036a5da-e1db-4fa9-b8fe-5b9b91b20b02)(content(Whitespace\"\\n\"))))(Secondary((id \
         cc374293-e024-4907-b7c0-ebb582af06ec)(content(Comment\"#                                                  \
         #\"))))(Secondary((id \
         d42dfb5b-4729-4e65-b559-bb88adfb6158)(content(Whitespace\"\\n\"))))(Secondary((id \
         d2c2a710-fb73-40cf-bccd-a462d660724a)(content(Comment\"# Raw entries \
         look like:                           #\"))))(Secondary((id \
         788c4be6-804f-42fa-965c-2ae70d90c0a5)(content(Whitespace\"\\n\"))))(Secondary((id \
         93b37c16-6db9-457a-a0fb-0604896aa3e6)(content(Comment\"#   \\\"  \
         \\240\\159\\140\\149 Full Moon -- clear skies, planted moonbloom  \
         \\\"  #\"))))(Secondary((id \
         8081275e-a481-4872-964a-d11091715310)(content(Whitespace\"\\n\"))))(Secondary((id \
         bd4648ad-3d6c-49c0-924b-6535f622f9d1)(content(Comment\"#   \
         \\\"\\240\\159\\140\\145  New Moon--cloudy,   harvested starfern\\\"  \
         #\"))))(Secondary((id \
         746a11c3-be35-4dfc-a96d-902c4338cd13)(content(Whitespace\"\\n\"))))(Secondary((id \
         3f3c5301-7cc3-4008-980e-ae8934ff5061)(content(Comment\"#                                                  \
         #\"))))(Secondary((id \
         56156d57-f75b-4b67-9128-26bdc6ea5299)(content(Whitespace\"\\n\"))))(Secondary((id \
         eabcb1f8-f402-4f8a-8762-6a3f3e6c7eaf)(content(Comment\"# Cleaned \
         entries should look like:                #\"))))(Secondary((id \
         37840fd1-4014-4834-a95e-cadfcb893385)(content(Whitespace\"\\n\"))))(Secondary((id \
         f4053f72-f299-4192-9ae2-69bb535c2ecb)(content(Comment\"#   \\\"Full \
         Moon: clear skies, planted moonbloom\\\"     #\"))))(Secondary((id \
         b8573e96-af84-4d51-9ec1-aac5478c76fb)(content(Whitespace\"\\n\"))))(Secondary((id \
         bdae0a13-4153-4e55-9edf-1af594df1ed6)(content(Comment\"#   \\\"New \
         Moon: cloudy, harvested starfern\\\"         #\"))))(Secondary((id \
         c7fdd3bd-7050-4197-b6bc-e63222843058)(content(Whitespace\"\\n\"))))(Secondary((id \
         ca5fbc82-7554-42bf-bbb3-4aa27ab69271)(content(Comment\"#                                                  \
         #\"))))(Secondary((id \
         58d211fb-e91c-4b62-b3f2-b1f79c3458f6)(content(Whitespace\"\\n\"))))(Secondary((id \
         7b0aed9e-d6cd-4326-8265-077794a32034)(content(Comment\"# \
         Steps:                                           \
         #\"))))(Secondary((id \
         02b36cbc-ebf1-4ce9-8fb4-0dcf67037681)(content(Whitespace\"\\n\"))))(Secondary((id \
         0e4712a4-4899-4c14-b846-036381479723)(content(Comment\"#   1. Trim \
         leading/trailing whitespace            #\"))))(Secondary((id \
         7db3c90d-8efd-49d6-ace9-98dfd61df208)(content(Whitespace\"\\n\"))))(Secondary((id \
         096c03fe-7fcb-4aa5-b5e3-a079ba2215ef)(content(Comment\"#   2. Remove \
         moon emoji symbols                   #\"))))(Secondary((id \
         3192f87b-3b7e-4526-9fb1-70539ebb6f25)(content(Whitespace\"\\n\"))))(Secondary((id \
         f1303c53-aa69-41ee-b80b-68ebc80b1cc8)(content(Comment\"#   3. \
         Normalize \\\" -- \\\" or \\\"--\\\" into \\\": \\\"          \
         #\"))))(Secondary((id \
         639aa42d-1b36-44bb-b18b-bad32f9c8746)(content(Whitespace\"\\n\"))))(Secondary((id \
         c01a7b65-b811-479c-ac73-0c2479308d7b)(content(Comment\"#   4. \
         Collapse multiple spaces into one           #\"))))(Secondary((id \
         334332d1-fb6d-41bb-97f7-35be8f6f3732)(content(Whitespace\"\\n\"))))(Secondary((id \
         c901190a-cf1d-4bbd-bb3a-e4250cfe3287)(content(Comment\"#   5. Final \
         trim for any leftover edge spaces     #\"))))(Secondary((id \
         4afc9a7e-fb53-400f-9845-7d070503391b)(content(Whitespace\"\\n\"))))(Secondary((id \
         08326492-913e-470e-b777-138b79e38109)(content(Comment\"#                                                  \
         #\"))))(Secondary((id \
         fb59cb30-c68a-4795-bd7f-75291f0a3d12)(content(Whitespace\"\\n\"))))(Secondary((id \
         18cf505c-d025-4da4-91f7-8cf2c734bb3e)(content(Comment\"# Available \
         functions:                             #\"))))(Secondary((id \
         8b16b676-f782-4e4e-8485-9a67b162c31b)(content(Whitespace\"\\n\"))))(Secondary((id \
         7f7cadb7-363f-4483-85a7-da9b1c42eb33)(content(Comment\"#   \
         string_trim(str) -> String                     #\"))))(Secondary((id \
         65dcbe00-e980-42a6-8993-58a832070861)(content(Whitespace\"\\n\"))))(Secondary((id \
         64157780-67a0-4827-b52d-06a07c526085)(content(Comment\"#   \
         string_replace(pattern, str, replacement)      #\"))))(Secondary((id \
         8c016a81-ba33-403e-a1c4-de7b2817b4a7)(content(Whitespace\"\\n\"))))(Secondary((id \
         9241d498-06ed-466e-8f3f-135a02c265b7)(content(Comment\"#     -> \
         String (replaces ALL matches)             #\"))))(Secondary((id \
         87f97e15-14c2-4144-9f13-1dab1968132a)(content(Whitespace\"\\n\"))))(Secondary((id \
         485d5cf1-5d99-48ac-b4a9-226120a73382)(content(Comment\"#   \
         string_match(pattern, str) -> Bool             #\"))))(Secondary((id \
         8fab6616-36e7-4372-a1d9-ca95c57d50f9)(content(Whitespace\"\\n\"))))(Secondary((id \
         4a6b2fc3-1653-4dd3-9529-df379e8c0608)(content(Comment\"#                                                  \
         #\"))))(Secondary((id \
         15edd082-5879-438a-949f-c211ce2190ce)(content(Whitespace\"\\n\"))))(Secondary((id \
         0624c2fc-2216-4d10-8bdd-f95c5c80f83f)(content(Comment\"# Patterns are \
         regex:                              #\"))))(Secondary((id \
         f2612cad-b01a-4b56-8b5e-2360717fcc1d)(content(Whitespace\"\\n\"))))(Secondary((id \
         ff742744-5fed-49c4-b1e0-81c8cf2a5604)(content(Comment\"#   \\\" +\\\" \
         matches one or more spaces                #\"))))(Secondary((id \
         ecb6dce1-47cc-4900-a497-09ad6f4f5e57)(content(Whitespace\"\\n\"))))(Secondary((id \
         ddbc7ad0-408b-4ec9-bba3-8051d3a2bc65)(content(Comment\"#   \\\" *-- \
         *\\\" matches -- with optional spaces       #\"))))(Secondary((id \
         0f3c8edf-cc7e-45e6-bd10-c6e0d9a835af)(content(Whitespace\"\\n\"))))(Secondary((id \
         4759f62f-248a-4690-8bc5-1e4b61073a65)(content(Comment\"#   \
         \\\"[abc]\\\" matches any character in the set       \
         #\"))))(Secondary((id \
         7b012b0a-9f97-46d9-918d-2d638e61ecfc)(content(Whitespace\"\\n\"))))(Secondary((id \
         095b8482-d671-4f18-8c1a-38a1480bf782)(content(Comment\"#                                                  \
         #\"))))(Secondary((id \
         516ffdf3-e10a-4786-9453-fb3e97214fa4)(content(Whitespace\"\\n\"))))(Secondary((id \
         2c6ca835-86be-40bd-bff5-fc4794bfc2fe)(content(Comment\"# Tip: Build \
         one step at a time! After each line,  #\"))))(Secondary((id \
         f689a797-0cde-46a9-a6bd-c87e22967b26)(content(Whitespace\"\\n\"))))(Secondary((id \
         5636de62-9a06-4867-96f9-204335ffc2b8)(content(Comment\"# check the \
         probe to see what your pattern did.    #\"))))(Secondary((id \
         05189376-a5ef-452f-a432-994a94cc98b7)(content(Whitespace\"\\n\"))))(Secondary((id \
         5e6d1ecd-5dde-4eaa-a35f-f837bcd0090c)(content(Whitespace\"\\n\"))))(Tile((id \
         b7cd26a3-ab1a-4e06-b1d6-dfa748c82372)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         6ec7161e-1c46-4942-9089-c9549d41859c)(content(Whitespace\" \
         \"))))(Tile((id \
         7828312a-4ea9-457b-b821-9bb54cbba014)(label(clean_entry))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         79ff6d78-ce2f-47dc-a664-5eff0849971d)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 24))(sort Pat))((shape(Concave \
         24))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         54713462-aae6-4352-bbec-cbd7564131a4)(content(Whitespace\" \
         \"))))(Tile((id \
         9ac3e9f2-a0a4-48f7-801b-b46b2d6be1ae)(label(String))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         1b7b6248-f05f-41a8-bd12-f3ac3a49e1f7)(content(Whitespace\" \
         \"))))(Tile((id \
         0e83b0bc-664e-421e-ab10-93883f360ad6)(label(->))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 13))(sort Typ))((shape(Concave \
         13))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         5a2fb484-232e-48b8-b11f-460990eaae7a)(content(Whitespace\" \
         \"))))(Tile((id \
         497d1252-4468-4f08-9c77-25570adfb84d)(label(String))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         dc9ffe8d-ea53-4350-9e7f-50f0f5135e0d)(content(Whitespace\" \
         \")))))((Secondary((id \
         79f94200-e6ee-42a5-a385-fa84c4afe61b)(content(Whitespace\" \
         \"))))(Tile((id 3ff968a3-0fe9-49fb-a492-e176bba7ab75)(label(fun \
         ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 37))(sort Exp))))))(shards(0 \
         1))(children(((Secondary((id \
         45ed2623-437f-4b64-bc82-31783f0d14af)(content(Whitespace\" \
         \"))))(Tile((id \
         7d632359-4e4e-4ac4-871e-b9bc91540cf9)(label(entry))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         9f24c3c9-adb3-4d89-963d-a6d4ba6c8c6b)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         eaf62ad7-2320-43c4-a2f0-ec6f864a24a3)(content(Whitespace\"\\n\"))))(Tile((id \
         b86de44a-7e6a-4f03-846b-b30f323e2aea)(label(?))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         d973e85e-5252-472d-9b55-7524cd99fa92)(content(Whitespace\"\\n\"))))(Secondary((id \
         9f4d7d60-f826-4f43-8d29-7ec56923db11)(content(Whitespace\"\\n\"))))(Secondary((id \
         e8b75101-0aa1-42d7-ab25-2cfb933f528b)(content(Whitespace\"\\n\"))))(Secondary((id \
         0759370a-6e09-41e9-bd24-939eb19783be)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         aec38f2b-f393-401b-b11c-8f4c514fc7de)(content(Whitespace\"\\n\"))))(Secondary((id \
         3b767275-683b-409d-a24c-c869f8fe9f48)(content(Whitespace\"\\n\"))))(Tile((id \
         fe6ee9dd-8cf0-4f22-93ca-2742b9d428a5)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         2c315044-b2f8-46b4-b970-473d11646225)(content(Whitespace\"\\n\"))))(Tile((id \
         6d253da4-b017-4266-8516-23b70f71a00b)(label(clean_entry))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         d7702057-94a4-4a11-badd-315a32aebbc1)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         d9cd1d06-28b8-43c0-9604-5350c2a5ffed)(label(\"\\\"  \
         \\240\\159\\140\\149 Full Moon -- clear skies, planted moonbloom  \
         \\\"\"))(mold((out Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0))(children()))))))))(Secondary((id \
         df5e1297-6651-46a2-9c66-4f40b19a1092)(content(Whitespace\"\\n\"))))(Tile((id \
         931fb179-5a73-4709-90ec-058834670aab)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         8c11c8b3-5337-4543-aaee-8b089948d9d4)(content(Whitespace\" \
         \"))))(Tile((id \
         3dddcf23-92a8-431c-aa3a-d2f6e9097038)(label(\"\\\"Full Moon: clear \
         skies, planted moonbloom\\\"\"))(mold((out Exp)(in_())(nibs(((shape \
         Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         5e9a85e6-388e-426c-ae21-1528553777a5)(content(Whitespace\"\\n\")))))))))(Tile((id \
         5693b45e-299b-4c48-b3da-870ca5ca89d2)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         28a674cd-d72a-43f8-8303-c36afa202a95)(content(Whitespace\"\\n\"))))(Secondary((id \
         10581a69-1f62-4488-8170-7324dcae3754)(content(Whitespace\"\\n\"))))(Tile((id \
         ae5c5cd0-59d3-428d-99ab-6c33770c6823)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         7fd0fc6f-8901-4793-8b3c-a3426eea7757)(content(Whitespace\"\\n\"))))(Tile((id \
         ded29596-b7ce-4bf7-a5a3-8f02fc1426e8)(label(clean_entry))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         2e441f08-9d10-4296-b8bd-5004317135b5)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         5a8b3213-4904-4f8a-aa0f-bac2d3a18c08)(label(\"\\\"\\240\\159\\140\\145  \
         New Moon--cloudy,   harvested starfern\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         ff4727bc-02e2-46cf-a2f0-90ffe7983d96)(content(Whitespace\"\\n\"))))(Tile((id \
         c54a1e44-a760-404d-bf72-e7b7c68e7392)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         5d973854-a991-49a7-a2cf-7e60d4055884)(content(Whitespace\" \
         \"))))(Tile((id 91196587-46ec-4146-b994-be1cb2452a88)(label(\"\\\"New \
         Moon: cloudy, harvested starfern\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         81e0f833-3f57-4fdb-b3a4-2cbccbdd56cb)(content(Whitespace\"\\n\")))))))))(Tile((id \
         0f3eb288-b53a-4f8a-835e-825785e83ea7)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         3577d150-321c-471d-ac64-fd52685a5fb0)(content(Whitespace\"\\n\"))))(Secondary((id \
         3cbc20cc-b9ed-42f2-8990-4333157117b6)(content(Whitespace\"\\n\"))))(Tile((id \
         2547d2c2-9045-4730-9e87-1c9ae81043a1)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         fee68021-6daa-4bcc-9614-9c4495c2132a)(content(Whitespace\"\\n\"))))(Tile((id \
         d6d56507-3a7b-447a-81bc-439736842bcb)(label(clean_entry))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         08c70359-c1c5-4155-9719-8f8906dc1df2)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         7d5cb3ec-7f65-4236-aa7d-42c891ecdc46)(label(\"\\\"  \
         \\240\\159\\140\\147 Half Moon -- light rain, pruned duskrose  \
         \\\"\"))(mold((out Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0))(children()))))))))(Secondary((id \
         995aa8ba-7db1-455f-9da9-bd8e1562b9ea)(content(Whitespace\"\\n\"))))(Tile((id \
         b2fdc593-0f15-4bf2-8553-9120fa84a0da)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         ddd81b1c-d26e-4c50-81fe-983753fa2423)(content(Whitespace\" \
         \"))))(Tile((id \
         90af3107-cb99-4a66-aaf3-0d6d728f7342)(label(\"\\\"Half Moon: light \
         rain, pruned duskrose\\\"\"))(mold((out Exp)(in_())(nibs(((shape \
         Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         3f21b737-5827-4f81-9ae0-4f6b87ad7b0f)(content(Whitespace\"\\n\")))))))))(Tile((id \
         d868fcac-23f3-47b4-a93f-53efd1cde7c9)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         63b98398-12c8-410c-b8f6-9c0f6ba1a2af)(content(Whitespace\"\\n\"))))(Secondary((id \
         e03680a6-14a6-44a2-9eb7-ce2ee0a9c03b)(content(Whitespace\"\\n\"))))(Tile((id \
         ae065f5f-7ef2-47ad-9b5b-a4175c6e3118)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         ad386d75-05d8-4121-a888-804958d6636f)(content(Whitespace\"\\n\"))))(Tile((id \
         41069ca3-2453-4147-94e2-0ddd0228901c)(label(clean_entry))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         d5dfb59d-1baf-445a-96e0-91a2d4abb4bf)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         a107ebfc-bed3-4260-904c-13bb30910375)(label(\"\\\"\\240\\159\\140\\151 \
         Crescent--foggy,  checked   moth traps\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         be656e56-ebc3-48db-989b-4e960b3fb91b)(content(Whitespace\"\\n\"))))(Tile((id \
         e164db6d-8d01-413c-8e92-a6ae2a10a1e1)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         84a1438d-ce48-46e9-8bcf-a7e02f8716d6)(content(Whitespace\" \
         \"))))(Tile((id \
         e0a5b236-d5a8-4823-81cd-744208bb5949)(label(\"\\\"Crescent: foggy, \
         checked moth traps\\\"\"))(mold((out Exp)(in_())(nibs(((shape \
         Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         757102b7-6761-4d5f-9b4a-3b147bf7eefa)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         5d61d4a4-283f-42bd-bf40-c66a892ba28d)(content(Whitespace\"\\n\")))))";
      backup_text =
        "# Moonphase Log Cleaner                            #\n\
         #                                                  #\n\
         # Garden keepers record observations in a messy    #\n\
         # log with emoji markers, inconsistent dashes,     #\n\
         # and extra whitespace. Implement clean_entry to   #\n\
         # standardize each log entry.                      #\n\
         #                                                  #\n\
         # Raw entries look like:                           #\n\
         #   \"  \240\159\140\149 Full Moon -- clear skies, planted moonbloom  \
         \"  #\n\
         #   \"\240\159\140\145  New Moon--cloudy,   harvested starfern\"  #\n\
         #                                                  #\n\
         # Cleaned entries should look like:                #\n\
         #   \"Full Moon: clear skies, planted moonbloom\"     #\n\
         #   \"New Moon: cloudy, harvested starfern\"         #\n\
         #                                                  #\n\
         # Steps:                                           #\n\
         #   1. Trim leading/trailing whitespace            #\n\
         #   2. Remove moon emoji symbols                   #\n\
         #   3. Normalize \" -- \" or \"--\" into \": \"          #\n\
         #   4. Collapse multiple spaces into one           #\n\
         #   5. Final trim for any leftover edge spaces     #\n\
         #                                                  #\n\
         # Available functions:                             #\n\
         #   string_trim(str) -> String                     #\n\
         #   string_replace(pattern, str, replacement)      #\n\
         #     -> String (replaces ALL matches)             #\n\
         #   string_match(pattern, str) -> Bool             #\n\
         #                                                  #\n\
         # Patterns are regex:                              #\n\
         #   \" +\" matches one or more spaces                #\n\
         #   \" *-- *\" matches -- with optional spaces       #\n\
         #   \"[abc]\" matches any character in the set       #\n\
         #                                                  #\n\
         # Tip: Build one step at a time! After each line,  #\n\
         # check the probe to see what your pattern did.    #\n\n\
         let clean_entry: String -> String = fun entry ->\n\
         ?\n\n\n\n\
         in\n\n\
         test\n\
         clean_entry(\"  \240\159\140\149 Full Moon -- clear skies, planted \
         moonbloom  \")\n\
         == \"Full Moon: clear skies, planted moonbloom\"\n\
         end;\n\n\
         test\n\
         clean_entry(\"\240\159\140\145  New Moon--cloudy,   harvested \
         starfern\")\n\
         == \"New Moon: cloudy, harvested starfern\"\n\
         end;\n\n\
         test\n\
         clean_entry(\"  \240\159\140\147 Half Moon -- light rain, pruned \
         duskrose  \")\n\
         == \"Half Moon: light rain, pruned duskrose\"\n\
         end;\n\n\
         test\n\
         clean_entry(\"\240\159\140\151 Crescent--foggy,  checked   moth \
         traps\")\n\
         == \"Crescent: foggy, checked moth traps\"\n\
         end\n";
      refractors = "()";
    } )
