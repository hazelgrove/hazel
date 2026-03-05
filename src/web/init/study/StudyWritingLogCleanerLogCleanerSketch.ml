let out : string * Haz3lcore.PersistentSegment.t =
  ( "Study / writing / log-cleaner / log-cleaner-sketch",
    {
      segment =
        "((Secondary((id \
         a038660e-be62-4b42-bdf1-0d5db1db911f)(content(Comment\"# Moonphase \
         Log Cleaner                            #\"))))(Secondary((id \
         ff527a15-d457-4770-895e-e4e36758acd5)(content(Whitespace\"\\n\"))))(Secondary((id \
         597011dd-0bfc-4455-9b05-f5ba900ac903)(content(Comment\"#                                                  \
         #\"))))(Secondary((id \
         d11c7562-814e-40e9-8ba5-50124bb29efd)(content(Whitespace\"\\n\"))))(Secondary((id \
         508e3a3f-3af2-4bfa-afb5-8024aa0d8435)(content(Comment\"# Garden \
         keepers record observations in a messy    #\"))))(Secondary((id \
         07ad69d7-e122-4c8f-b9af-7d5d4d6d8c1c)(content(Whitespace\"\\n\"))))(Secondary((id \
         7ea44f6b-4f0f-47e2-83d1-5e50ccb57c4f)(content(Comment\"# log with \
         emoji markers, inconsistent dashes,     #\"))))(Secondary((id \
         8c54a008-ad3c-4586-8257-9559d40b11e0)(content(Whitespace\"\\n\"))))(Secondary((id \
         511d6214-dd21-4221-be45-7b12b2cfb133)(content(Comment\"# and extra \
         whitespace. Implement clean_entry to   #\"))))(Secondary((id \
         b63cfc98-f8d8-42dc-b59a-63960e048b7b)(content(Whitespace\"\\n\"))))(Secondary((id \
         9a410f8c-1b3f-481c-abb1-17d553d1c873)(content(Comment\"# standardize \
         each log entry.                      #\"))))(Secondary((id \
         e5eae0f3-ecad-4565-bbb1-2847604ba80a)(content(Whitespace\"\\n\"))))(Secondary((id \
         06772d30-705d-4c61-9c02-21b566cbd716)(content(Comment\"#                                                  \
         #\"))))(Secondary((id \
         59ae8b1d-392c-4f55-b4e5-24b2c6ea47f1)(content(Whitespace\"\\n\"))))(Secondary((id \
         ad2060cf-95c3-4ff5-a136-f577379554e4)(content(Comment\"# Raw entries \
         look like:                           #\"))))(Secondary((id \
         77a57e6c-bc9d-4c6b-92c4-5e7753fac0a1)(content(Whitespace\"\\n\"))))(Secondary((id \
         489e04cf-43d0-4101-84db-45b15f645ac3)(content(Comment\"#   \\\"  \
         \\240\\159\\140\\149 Full Moon -- clear skies, planted moonbloom  \
         \\\"  #\"))))(Secondary((id \
         c28051eb-a2f5-414b-8cfb-1bf2973c029f)(content(Whitespace\"\\n\"))))(Secondary((id \
         07f5c128-2464-4c72-a8de-ecdc54a65255)(content(Comment\"#   \
         \\\"\\240\\159\\140\\145  New Moon--cloudy,   harvested starfern\\\"  \
         #\"))))(Secondary((id \
         c069cd42-d18c-4038-9ce1-5324f05acae0)(content(Whitespace\"\\n\"))))(Secondary((id \
         2a80df47-9fcd-48db-b0c9-98fcc81f50bd)(content(Comment\"#                                                  \
         #\"))))(Secondary((id \
         5ae553a6-c58c-4e05-9c20-26123581a79a)(content(Whitespace\"\\n\"))))(Secondary((id \
         0bb2304e-8952-4251-a893-3a57c0ceb4d2)(content(Comment\"# Cleaned \
         entries should look like:                #\"))))(Secondary((id \
         baddbc3b-5153-4622-be23-ff5edf8dfa63)(content(Whitespace\"\\n\"))))(Secondary((id \
         4410e63e-b1ef-4c62-bc86-9464bd25e1b5)(content(Comment\"#   \\\"Full \
         Moon: clear skies, planted moonbloom\\\"     #\"))))(Secondary((id \
         82d3d72f-5e28-4409-9a15-f24adc9f3e7a)(content(Whitespace\"\\n\"))))(Secondary((id \
         3a23fced-801d-4bb2-84b4-fc74c159a8f1)(content(Comment\"#   \\\"New \
         Moon: cloudy, harvested starfern\\\"         #\"))))(Secondary((id \
         11c00bd6-6b6e-4bf0-b472-3e6303380248)(content(Whitespace\"\\n\"))))(Secondary((id \
         5b3b44d3-a074-4b60-9c09-5f62104a94f4)(content(Comment\"#                                                  \
         #\"))))(Secondary((id \
         aea9664e-2ca0-4fa8-b7c6-e79ab623d5f7)(content(Whitespace\"\\n\"))))(Secondary((id \
         b796a95a-7b1f-4797-bfff-d6934ef6561a)(content(Comment\"# \
         Steps:                                           \
         #\"))))(Secondary((id \
         c84a7e4d-620c-4abc-9528-c9f88ff1c6ca)(content(Whitespace\"\\n\"))))(Secondary((id \
         4e052ac6-0768-4284-95e0-403ec0537661)(content(Comment\"#   1. Trim \
         leading/trailing whitespace            #\"))))(Secondary((id \
         06631bf4-ea87-412c-8fa0-c89a830795ef)(content(Whitespace\"\\n\"))))(Secondary((id \
         38f645a0-c365-4095-9747-9674e6d4acf5)(content(Comment\"#   2. Remove \
         moon emoji symbols                   #\"))))(Secondary((id \
         05f48390-783e-4b9a-9a07-b73a16b1eb5e)(content(Whitespace\"\\n\"))))(Secondary((id \
         19c24d40-c8a1-41de-a0cd-7efffcd09321)(content(Comment\"#   3. \
         Normalize \\\" -- \\\" or \\\"--\\\" into \\\": \\\"          \
         #\"))))(Secondary((id \
         798fb779-38e4-4018-9d8b-aa60fd9d3173)(content(Whitespace\"\\n\"))))(Secondary((id \
         8fb466d1-9200-46ea-a1a4-d6a7a5f2766d)(content(Comment\"#   4. \
         Collapse multiple spaces into one           #\"))))(Secondary((id \
         4b873aee-1dd8-43bc-ab76-ee50a1d91dfc)(content(Whitespace\"\\n\"))))(Secondary((id \
         b4073942-8507-489f-8a05-3edcc349a1a7)(content(Comment\"#   5. Final \
         trim for any leftover edge spaces     #\"))))(Secondary((id \
         dcb0fcc1-7918-4377-af0e-13ef1856e923)(content(Whitespace\"\\n\"))))(Secondary((id \
         094d15dc-119a-48ef-a71b-1ab7960ceb71)(content(Comment\"#                                                  \
         #\"))))(Secondary((id \
         6d1f035f-8d27-4fe5-b723-9ab4e94ec4b7)(content(Whitespace\"\\n\"))))(Secondary((id \
         3d2017f6-b959-4637-9bcd-0c8a6588afff)(content(Comment\"# Available \
         functions:                             #\"))))(Secondary((id \
         1828c345-746d-4f90-9cd5-24e0a8284533)(content(Whitespace\"\\n\"))))(Secondary((id \
         0d7de61d-7fd8-4caf-b419-1e7a848c2ffb)(content(Comment\"#   \
         string_trim(str) -> String                     #\"))))(Secondary((id \
         464fb2b2-e4ea-4f30-86a8-638628133f43)(content(Whitespace\"\\n\"))))(Secondary((id \
         75d1be4b-bcd8-4080-99d5-21e6b080075e)(content(Comment\"#   \
         string_replace(pattern, str, replacement)      #\"))))(Secondary((id \
         a9878e89-ff31-45da-aff8-eb95bbce04a0)(content(Whitespace\"\\n\"))))(Secondary((id \
         9d16af4a-52cb-4dda-98c0-7b1fcc7f7605)(content(Comment\"#     -> \
         String (replaces ALL matches)             #\"))))(Secondary((id \
         a042cf0b-ac97-43f8-b510-33c9133ddec5)(content(Whitespace\"\\n\"))))(Secondary((id \
         dec9c6f1-3c14-4e6a-aae6-6823243669ae)(content(Comment\"#   \
         string_match(pattern, str) -> Bool             #\"))))(Secondary((id \
         a95e46f4-f574-41b8-9f1d-eef2d1b2c24c)(content(Whitespace\"\\n\"))))(Secondary((id \
         e3b4ae05-c35b-4f79-862c-54014435aed8)(content(Comment\"#                                                  \
         #\"))))(Secondary((id \
         997e4f60-3797-4eb2-800d-c47380180cb2)(content(Whitespace\"\\n\"))))(Secondary((id \
         cf002b01-8925-4659-a96f-bfd8d2fce429)(content(Comment\"# Patterns are \
         regex:                              #\"))))(Secondary((id \
         add3ef94-77bc-468e-9b37-0245e05c96d2)(content(Whitespace\"\\n\"))))(Secondary((id \
         de135465-6c35-4fc4-bc04-31eedde6bb00)(content(Comment\"#   \\\" +\\\" \
         matches one or more spaces                #\"))))(Secondary((id \
         9f672ba8-c416-4cb4-8aad-e016f51eabd5)(content(Whitespace\"\\n\"))))(Secondary((id \
         93025bc2-cfcd-4424-b2e0-c85dac5d34bc)(content(Comment\"#   \\\" *-- \
         *\\\" matches -- with optional spaces       #\"))))(Secondary((id \
         ef83b32f-2fd4-4902-af3f-bb536b242a3d)(content(Whitespace\"\\n\"))))(Secondary((id \
         b5795129-c017-487e-a37e-7e9035638383)(content(Comment\"#   \
         \\\"[abc]\\\" matches any character in the set       \
         #\"))))(Secondary((id \
         ec7ae2a7-478e-45cd-b469-bde0053456e7)(content(Whitespace\"\\n\"))))(Secondary((id \
         43b745bf-ad7e-4ee2-bb63-5cd85a55233e)(content(Comment\"#                                                  \
         #\"))))(Secondary((id \
         31962232-1b5c-4506-aa66-c4d7f1f5ee6b)(content(Whitespace\"\\n\"))))(Secondary((id \
         4123d2a6-19e8-4772-9899-3483b3f52d44)(content(Comment\"# Tip: Build \
         one step at a time! After each line,  #\"))))(Secondary((id \
         c03c8057-6ceb-4eb6-874b-95829ec60a27)(content(Whitespace\"\\n\"))))(Secondary((id \
         906a0cfd-c8b3-4bfc-a873-77cf3dbcd579)(content(Comment\"# check the \
         probe to see what your pattern did.    #\"))))(Secondary((id \
         aa873129-e1aa-4e77-864a-15567f8240ab)(content(Whitespace\"\\n\"))))(Secondary((id \
         ce5b8beb-8eef-431e-a312-6c74d22a2a02)(content(Whitespace\"\\n\"))))(Tile((id \
         15bfc31e-598f-4d41-a983-1baa947447de)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         f0e607c3-ee9f-470d-9ea1-be71d69ae0f2)(content(Whitespace\" \
         \"))))(Tile((id \
         afcc2abf-1e84-4b47-914b-ad8d71eefc33)(label(clean_entry))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         62cf581f-6f74-4f69-8ffa-fa1e8cb10a99)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 24))(sort Pat))((shape(Concave \
         24))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         d485975f-6c0f-4be4-9516-1916cc1bb46f)(content(Whitespace\" \
         \"))))(Tile((id \
         b677a0a6-fd6f-436e-bfcb-ec77cd54cdcc)(label(String))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         85683c2d-34bb-4f22-bb74-8fd41e940696)(content(Whitespace\" \
         \"))))(Tile((id \
         c9ca5d4c-612a-448d-a724-568e535b360d)(label(->))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 13))(sort Typ))((shape(Concave \
         13))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         658a1074-a924-404e-947b-4b2ef3a7f719)(content(Whitespace\" \
         \"))))(Tile((id \
         d7b898f5-568d-4e12-b3b3-542f71ff3010)(label(String))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         34861e54-b066-4e39-b074-5bffcc5156ee)(content(Whitespace\" \
         \")))))((Secondary((id \
         2bf2664c-659e-49b2-b14b-e6ad66895a3d)(content(Whitespace\" \
         \"))))(Tile((id 52861ed3-e451-4251-90cb-462d7790ffa8)(label(fun \
         ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 37))(sort Exp))))))(shards(0 \
         1))(children(((Secondary((id \
         dc2f0538-2c7d-45b2-baff-22cfe508b80a)(content(Whitespace\" \
         \"))))(Tile((id \
         4b1b192a-8e06-4f27-be1b-d9d7fb533f3f)(label(entry))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         97898103-a14a-44c9-a30e-16098a7fcda2)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         842722bc-f39e-41ed-b2ad-e383dc33443b)(content(Whitespace\"\\n\"))))(Tile((id \
         ed6ba497-accf-4f38-a271-cb9018fe4b39)(label(?))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         cfdd5e24-bfb1-45f9-92b6-f2b479ae68c1)(content(Whitespace\"\\n\"))))(Secondary((id \
         b60fa0ba-7cdd-44bd-91aa-872a7b70fe7e)(content(Whitespace\"\\n\"))))(Secondary((id \
         695c1448-0261-4a11-afa9-d00daeae4d9e)(content(Whitespace\"\\n\"))))(Secondary((id \
         afd79f06-bc88-47c3-94a3-e7a0402c5271)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         24522513-dc5e-42a9-ab66-2066b9119992)(content(Whitespace\"\\n\"))))(Secondary((id \
         82de7138-5ed9-4164-9166-35c209603959)(content(Whitespace\"\\n\"))))(Tile((id \
         0358a02a-9ad4-41c3-823e-37ef7564edc2)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         c7e9bcc4-9033-4614-a973-481ba9d65794)(content(Whitespace\"\\n\"))))(Tile((id \
         e40125bb-64ef-4d83-8640-35e5956dfdc8)(label(clean_entry))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         234962f3-4e0a-45ce-9f87-07ab5f2bfe45)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         efcc53b2-7760-4759-abd7-852a27d1d9b4)(label(\"\\\"  \
         \\240\\159\\140\\149 Full Moon -- clear skies, planted moonbloom  \
         \\\"\"))(mold((out Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0))(children()))))))))(Secondary((id \
         3749d3f2-e7fd-4191-be85-6755e972484f)(content(Whitespace\"\\n\"))))(Tile((id \
         f65730e5-b822-47ff-8153-0fb563f62921)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         9d7f8740-6dff-4a93-90eb-02c2b4e73b8f)(content(Whitespace\" \
         \"))))(Tile((id \
         9731f688-20b3-41d0-98b0-eb75a42717fb)(label(\"\\\"Full Moon: clear \
         skies, planted moonbloom\\\"\"))(mold((out Exp)(in_())(nibs(((shape \
         Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         3caa039c-ad39-434f-9ddd-722edcda7365)(content(Whitespace\"\\n\")))))))))(Tile((id \
         ed862573-f150-429b-a574-1b8353c58edd)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         88877d62-290f-4f75-8467-8b9c550ffb33)(content(Whitespace\"\\n\"))))(Secondary((id \
         a3b21315-0616-49ac-8954-098e61edbdee)(content(Whitespace\"\\n\"))))(Tile((id \
         387fd956-ab87-492c-8c8a-1945e2489ffc)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         e6c619d2-f184-4bc4-8afb-9d75dbde2a9d)(content(Whitespace\"\\n\"))))(Tile((id \
         7a700bf5-1314-4d0e-9774-682b43793579)(label(clean_entry))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         06a05c95-abf0-483f-91d8-f23014ff69b2)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         72ad8f74-5850-4784-ae4e-a44c05e7c95f)(label(\"\\\"\\240\\159\\140\\145  \
         New Moon--cloudy,   harvested starfern\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         e9e0d5b8-a4a2-4fb4-a881-4c5cf72b9664)(content(Whitespace\"\\n\"))))(Tile((id \
         ebe18a40-efad-4316-a2e9-a924122f649e)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         6eb497a6-86c7-414d-8720-97136d89a4c3)(content(Whitespace\" \
         \"))))(Tile((id f44702b9-7084-4926-b712-33dcc619dded)(label(\"\\\"New \
         Moon: cloudy, harvested starfern\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         a5fa06c5-ac08-48b4-bc06-c88864c79a73)(content(Whitespace\"\\n\")))))))))(Tile((id \
         19e9e266-f37d-4a2b-a526-89fca41c93df)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         2f5c4b1a-9282-401e-9f3c-02b5f5e84b80)(content(Whitespace\"\\n\"))))(Secondary((id \
         f2708046-90c7-4dc3-acec-811852dccbba)(content(Whitespace\"\\n\"))))(Tile((id \
         5fc578b1-30c4-49a5-be68-97a3e3e8faf0)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         9929a09e-a82a-4b67-8e32-a23a8e432f7d)(content(Whitespace\"\\n\"))))(Tile((id \
         66550dbd-9015-41e2-8814-d0c5ac3fa202)(label(clean_entry))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         1aa9cc25-c21c-4dc3-bc2a-89383e8df6bc)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         6b3847cd-38dc-48b2-ad76-a0333b194003)(label(\"\\\"  \
         \\240\\159\\140\\147 Half Moon -- light rain, pruned duskrose  \
         \\\"\"))(mold((out Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0))(children()))))))))(Secondary((id \
         c536614a-7a00-49ad-a7b0-f0e74aaf8ea7)(content(Whitespace\"\\n\"))))(Tile((id \
         d7faa99e-98b7-4c19-acb7-d0027403e841)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         9e4116d0-9fda-4106-aabf-5a5670452c74)(content(Whitespace\" \
         \"))))(Tile((id \
         24d4e4ef-764a-44e3-b460-81ee7b5ab4e1)(label(\"\\\"Half Moon: light \
         rain, pruned duskrose\\\"\"))(mold((out Exp)(in_())(nibs(((shape \
         Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         0e181c98-66ff-4b47-882e-9d350bd677e9)(content(Whitespace\"\\n\")))))))))(Tile((id \
         cab1f512-d196-48ac-8096-0bdcd7e85499)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         d3ec7a56-922e-4a84-93f9-5c107094e220)(content(Whitespace\"\\n\"))))(Secondary((id \
         c123a059-4b3d-484b-8c5f-9708ea00d823)(content(Whitespace\"\\n\"))))(Tile((id \
         ac368ba5-f55e-4c9a-b721-486a2eccabee)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         f3e9028a-7420-4ee6-a126-f071a8cc9ff9)(content(Whitespace\"\\n\"))))(Tile((id \
         4dec3b86-8140-46dd-8299-946cc218a7c9)(label(clean_entry))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         4b39c682-5ca4-45fb-bb4c-b364ac0595eb)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         3b44f658-2fef-4bc8-adb0-1cf2cbd7ec06)(label(\"\\\"\\240\\159\\140\\151 \
         Crescent--foggy,  checked   moth traps\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         eb898192-c3cc-4ceb-bdd2-3ed4b3c1b269)(content(Whitespace\"\\n\"))))(Tile((id \
         155c3b6c-1a3d-4af8-b0a3-52ee45bb64a0)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         b0b2454e-61bc-4000-9426-d70db5d4d6de)(content(Whitespace\" \
         \"))))(Tile((id \
         5ec3efc2-9e48-48cb-8616-75d4d64fbb1b)(label(\"\\\"Crescent: foggy, \
         checked moth traps\\\"\"))(mold((out Exp)(in_())(nibs(((shape \
         Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         931dcd9d-0ccb-4dd2-85ac-73fc95ed31f8)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         de4454f7-ac91-4002-8d1f-676cb5d0d165)(content(Whitespace\"\\n\")))))";
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
