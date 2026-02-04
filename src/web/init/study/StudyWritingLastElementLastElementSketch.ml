let out : string * Haz3lcore.PersistentSegment.t =
  ( "Study / writing / last-element / last-element-sketch",
    {
      segment =
        "((Secondary((id \
         d6bf7b9d-a9dc-4403-bfbe-8d440d275c1b)(content(Comment\"# LAST ELEMENT \
         TASK                            #\"))))(Secondary((id \
         9597091c-493a-479d-b9d8-9ba191ea8297)(content(Whitespace\"\\n\"))))(Secondary((id \
         ccba79cd-aff1-4f4f-996d-39b94d2ca63e)(content(Comment\"#                                              \
         #\"))))(Secondary((id \
         7bd9ed5a-c625-49dc-8a69-09858e6b1b50)(content(Whitespace\"\\n\"))))(Secondary((id \
         9a9717e9-c810-47dc-9d85-a8183b5b938b)(content(Comment\"# Implement \
         last: get the last element of a    #\"))))(Secondary((id \
         638ae8e8-701c-4a58-9ceb-248e84e3d5a7)(content(Whitespace\"\\n\"))))(Secondary((id \
         79ed93e8-5269-434c-98cc-84d6ef5e1016)(content(Comment\"# list, or \
         return a default if empty.          #\"))))(Secondary((id \
         fc541e77-96f7-4bd3-8584-f2cb440cd6e3)(content(Whitespace\"\\n\"))))(Secondary((id \
         7bb094b9-ed0c-4095-84d6-160ea1b4ef16)(content(Comment\"#                                              \
         #\"))))(Secondary((id \
         c5eb6785-e607-4b79-b6bf-fbc75766dbe9)(content(Whitespace\"\\n\"))))(Secondary((id \
         16a680e3-0b43-4ad3-8c62-ad8ecdf514e8)(content(Comment\"# \
         Examples:                                    #\"))))(Secondary((id \
         3f765f90-ad4d-4332-ac71-450a1c77ab2e)(content(Whitespace\"\\n\"))))(Secondary((id \
         11fb5ea5-6dca-4eb2-8134-74805881390c)(content(Comment\"#   last([1, \
         2, 3], 0) == 3                    #\"))))(Secondary((id \
         1d77e1b8-7b31-4453-993f-0d6152bc23e9)(content(Whitespace\"\\n\"))))(Secondary((id \
         76e26514-b2af-4a77-9946-00ac2470a3c2)(content(Comment\"#   last([42], \
         0) == 42                        #\"))))(Secondary((id \
         03219803-5afe-48f8-aea5-590755a8ff77)(content(Whitespace\"\\n\"))))(Secondary((id \
         09d38239-952b-4921-ac95-5a37adf9998f)(content(Comment\"#   last([], \
         99) == 99                         #\"))))(Secondary((id \
         281d346d-42cc-4933-9ace-bb56ef6e5f65)(content(Whitespace\"\\n\"))))(Secondary((id \
         3fa006b5-9821-4f53-9e21-c5bad6d90100)(content(Comment\"#                                              \
         #\"))))(Secondary((id \
         2d2693ce-47d1-44a3-8dca-4cd5372d1f19)(content(Whitespace\"\\n\"))))(Secondary((id \
         d33b0c85-ab58-4aa9-a93b-415852e35c26)(content(Comment\"# Available \
         functions:                         #\"))))(Secondary((id \
         3becd26b-e836-487d-bfa8-479fefab241b)(content(Whitespace\"\\n\"))))(Secondary((id \
         53c8c908-5d3a-4f25-ad82-25e865383d31)(content(Comment\"#   \
         fold_left(list, fn, init) -> result        #\"))))(Secondary((id \
         5632f657-fd5d-4c58-826a-ac8127b78c9d)(content(Whitespace\"\\n\"))))(Secondary((id \
         a98c6ae1-1c63-4e1d-9fe6-d7bdda832536)(content(Comment\"#     fn takes \
         (accumulator, element)          #\"))))(Secondary((id \
         10a05bc6-c994-4489-9c4d-60f0aa4b4992)(content(Whitespace\"\\n\"))))(Secondary((id \
         abf0db4d-9523-4217-9008-26acfe536c8d)(content(Comment\"#   \
         fold_right(list, fn, init) -> result       #\"))))(Secondary((id \
         65b57a01-5724-4bfa-82ac-bd99295e7fbc)(content(Whitespace\"\\n\"))))(Secondary((id \
         f160e272-fdcd-4342-9587-a2bd16bb72f0)(content(Comment\"#     fn takes \
         (element, accumulator)          #\"))))(Secondary((id \
         a42c6eee-364d-4104-981f-6d9286b89cc7)(content(Whitespace\"\\n\"))))(Secondary((id \
         fffd89c9-4a83-437d-9a24-0216c0b7ebc8)(content(Comment\"#   rev(list) \
         -> list                          #\"))))(Secondary((id \
         f99e0c50-e0a6-4a89-afd8-a563623e57f5)(content(Whitespace\"\\n\"))))(Secondary((id \
         88dee708-6a82-4d32-956c-3d1414c23ec8)(content(Comment\"#   \
         length(list) -> Int                        #\"))))(Secondary((id \
         52eb71bf-9bec-4394-831e-dd7a09f4470f)(content(Whitespace\"\\n\"))))(Secondary((id \
         70da6395-7e36-4809-957d-ece75fa39c80)(content(Comment\"#                                              \
         #\"))))(Secondary((id \
         be8fd696-6507-4ead-a3eb-72c4e86f464e)(content(Whitespace\"\\n\"))))(Secondary((id \
         be12ef0b-e1d3-4221-aca5-7ab3b1079288)(content(Comment\"# Tip: Think \
         about what the fold should        #\"))))(Secondary((id \
         5d2f16ef-4ef9-4f93-a3ee-b726293ea95a)(content(Whitespace\"\\n\"))))(Secondary((id \
         b64c2a16-c274-48f1-9d19-c14898f21867)(content(Comment\"# \
         \\\"remember\\\" as it processes each element.     \
         #\"))))(Secondary((id \
         9a991105-b9ab-4b43-9df7-3cc2718904de)(content(Whitespace\"\\n\"))))(Secondary((id \
         93e81c0e-b209-44dc-9593-9955ab41aeea)(content(Whitespace\"\\n\"))))(Tile((id \
         3111f3a0-0a01-491b-821c-b52cd2065645)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         076c5e5e-ba7d-4e70-af18-2f4a7e94795f)(content(Whitespace\" \
         \"))))(Tile((id \
         eabe544d-c066-465f-8212-8abcdeb3edc0)(label(last))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         665f0644-36bd-4a20-95b4-6b319d638c73)(content(Whitespace\" \
         \")))))((Secondary((id \
         c86b6d95-14f2-41f7-970f-0f41b3c6e024)(content(Whitespace\" \
         \"))))(Tile((id 8dd08b2f-073a-4931-b98d-3bb6fe107a86)(label(fun \
         ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 37))(sort Exp))))))(shards(0 \
         1))(children(((Secondary((id \
         35e18acc-2d3b-4297-82b4-15f6b12b5aea)(content(Whitespace\" \
         \"))))(Tile((id \
         8581ed9e-49a8-492f-aba8-664b07bec78f)(label(\"(\"\")\"))(mold((out \
         Pat)(in_(Pat))(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0 1))(children(((Tile((id \
         74202098-09f9-4eab-a643-f8a9a9b60c17)(label(xs))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         a8a4e86e-3466-4bcd-a679-3ef666061170)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 44))(sort Pat))((shape(Concave \
         44))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         9aa3b98e-ad32-4e97-9911-8cae3270b940)(content(Whitespace\" \
         \"))))(Tile((id \
         c27af26c-40ed-45d2-ba79-b91f583b2db0)(label(default))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children()))))))))(Secondary((id \
         eae706ee-2127-4cb2-b4ce-e128f487055d)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         b67a2455-0cf0-4747-b4c6-286f8a787def)(content(Whitespace\"\\n\"))))(Tile((id \
         14347ce7-dd6a-43d6-ba62-2426d953c625)(label(?))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         a3b2f4b4-e231-4981-a36f-c61640323238)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         68465524-918f-4ec8-84b7-66f8426c1c97)(content(Whitespace\"\\n\"))))(Secondary((id \
         b1a9c06f-4a0d-468c-bd50-57e5fc728fa1)(content(Whitespace\"\\n\"))))(Tile((id \
         3ee05db9-037f-4a4b-bcdc-0a5574091381)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         f303f6a5-8fc3-4f73-ba09-dae17e8362ef)(content(Whitespace\"\\n\"))))(Tile((id \
         d330a0c6-fa7e-4ca6-baf1-87452cca1f39)(label(last))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         18092147-9877-4bf7-86ab-5a1a261585c4)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         f5f79860-6876-4b47-aadb-6f23e0c95d87)(label([ ]))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         927739b7-3d7f-471e-990d-c27c0052dcb5)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         afe09c98-2a5a-413c-a32e-594c896acdf0)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         19d6991c-9705-441d-ad95-c9ddb4545180)(content(Whitespace\" \
         \"))))(Tile((id \
         020f2d2e-1f9d-4914-81d9-c98e9b410dd8)(label(2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         c5936081-28bc-4efe-84a9-f06a769cffe4)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         2037e9c7-cfac-4cdf-b8b7-737cb7b0f8c1)(content(Whitespace\" \
         \"))))(Tile((id \
         019a5a43-ad6d-45bd-825f-2fe5b17dd5c6)(label(3))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         8e753076-0db6-4bc1-9660-0af3d273cd9b)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         39d2112b-9ccc-4a0d-88af-708c9f1d85b5)(content(Whitespace\" \
         \"))))(Tile((id \
         6e93e6e8-631f-43ec-b407-f2267b8dec72)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         62125fb8-c91f-48ab-ab4e-c4489de27599)(content(Whitespace\"\\n\"))))(Tile((id \
         3e144aff-5141-41ac-b356-01b6b3149662)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         1a9f5a10-0a53-41c0-b660-6cc065fd0a99)(content(Whitespace\" \
         \"))))(Tile((id \
         2de96922-bc86-4459-9cfb-fd3b1f96c65a)(label(3))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         c18ba287-37d5-4608-8b2a-db34de9548ee)(content(Whitespace\"\\n\")))))))))(Tile((id \
         c7fd914a-71d1-493e-9deb-e51cc2f5318d)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         e50b0f1a-0bd4-4dd1-b38f-6d96eed2785d)(content(Whitespace\"\\n\"))))(Secondary((id \
         d5dc04cc-a233-48cc-85f5-7068fca7b636)(content(Whitespace\"\\n\"))))(Tile((id \
         2417954c-87bd-429c-b2ae-c867f1cfd8e2)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         e2b06900-fdc4-4a8f-88cc-6842222124b5)(content(Whitespace\"\\n\"))))(Tile((id \
         610ca52a-2845-40f2-bc3f-7dc023d8bca2)(label(last))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         507bfaba-8554-4b8f-839a-1a8e60b5ffb4)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         b9ca004b-1503-4742-bf98-feba628fa43b)(label([ ]))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         55cd0921-37fd-444d-ba73-bd4a44998d41)(label(42))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         a863bec9-9b5b-4c26-bead-990259d60250)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         e54fb0e1-0e86-40c5-9378-98ec2e03ac9f)(content(Whitespace\" \
         \"))))(Tile((id \
         5e314602-684e-4fe3-b66f-05682b3d6554)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         f1f387fd-4898-48a7-b6cc-72cd00edbf22)(content(Whitespace\"\\n\"))))(Tile((id \
         4a8cf0c4-83c8-4032-983f-618347ffbaed)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         d505775f-dcb0-4ff8-a10e-81ca45eab91b)(content(Whitespace\" \
         \"))))(Tile((id \
         602b953d-7d05-4874-ac71-c6d9943b4ac3)(label(42))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         aeb7fd89-9d89-4fd5-a074-a6ce90d81de1)(content(Whitespace\"\\n\")))))))))(Tile((id \
         9aa0d087-b3e3-4868-8a7c-22fa8bc29b0a)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         6b0f73ac-acbc-49f2-933a-3c27f3c81414)(content(Whitespace\"\\n\"))))(Secondary((id \
         5dcb422f-7b4f-4083-8cdc-cd2c53561e56)(content(Whitespace\"\\n\"))))(Tile((id \
         eef6fd73-f7a7-4015-b392-02370efbf744)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         0da565eb-fef7-4b83-b5f8-bbd061d4ed44)(content(Whitespace\"\\n\"))))(Tile((id \
         0aacba88-09aa-4a3a-816a-9f3280823a30)(label(last))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         7774f982-f143-4aa9-95d5-4e85fa143037)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         141fc606-4d61-4b7e-bb96-4ef677741e34)(label([ ]))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         09b7a6b3-f25e-4f1f-a976-cfa314671271)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         e19b8cc0-eae1-4314-be90-97405c9e47f1)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         e923782d-2c69-4f44-8fb6-05cc3f824339)(content(Whitespace\" \
         \"))))(Tile((id \
         671d5ffc-c719-4c42-9ade-8fbff03fd9ca)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         cfed6ff5-4f10-4348-9ccc-9da4db89cf9d)(content(Whitespace\"\\n\"))))(Tile((id \
         ff4f18e2-3a94-4dc4-88b3-6b629dfdceb9)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         8551c2a6-d5ec-4b49-946d-a3867fa58180)(content(Whitespace\" \
         \"))))(Tile((id \
         0025a19a-4b31-41a9-a42f-ea880f0672f2)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         77eef466-bb74-4d99-adc1-a2401bbbdc48)(content(Whitespace\"\\n\")))))))))(Tile((id \
         39c79fd4-6b49-49c3-a168-1f00b7db1673)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         b90d36df-6efb-4f6f-abf5-5cbce2636601)(content(Whitespace\"\\n\"))))(Secondary((id \
         c65078c8-354a-464d-8e01-b96d9915d1cb)(content(Whitespace\"\\n\"))))(Tile((id \
         5e0784fd-3fdc-4c8f-9609-2cd85f614476)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         e84224c2-099c-4f49-882d-f7be88343d9b)(content(Whitespace\"\\n\"))))(Tile((id \
         164525ef-dc4f-4fec-a9ad-f8f3fe1c481c)(label(last))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         9410bdf3-abb6-48b8-815e-c08a260bfda1)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         bcdf9a7a-92d4-46ff-aaf6-994cc9fcdfc0)(label([]))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         2f93bf06-2e10-41a3-a98e-b0d64b8c72d3)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         ec8df2a5-f804-4767-a62c-b750f6483f3d)(content(Whitespace\" \
         \"))))(Tile((id \
         d2137c9b-de67-4a09-8996-7274e852b006)(label(99))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         b7337264-cf01-446c-82b7-ad77bbed1097)(content(Whitespace\"\\n\"))))(Tile((id \
         1a7f3f3a-fa71-46f9-bb84-7766ee247c7d)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         a5a8f0ab-5143-48f4-a809-2de47cac9a5a)(content(Whitespace\" \
         \"))))(Tile((id \
         3d609cab-f3c5-4048-9b43-b5c8f1661a62)(label(99))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         0fece8ba-cb33-44ca-a08a-21169c4664de)(content(Whitespace\"\\n\")))))))))(Tile((id \
         a9d4015b-3074-4abf-bbf8-9f4fff957846)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         1fef9fc1-871b-4930-9c19-a1bcc0801980)(content(Whitespace\"\\n\"))))(Secondary((id \
         4819d6fe-66b6-4d38-ad29-5c7ca079f34f)(content(Whitespace\"\\n\"))))(Tile((id \
         9c001cd9-1c8b-4ce3-9215-c5682136c47d)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         cc2760a0-aa4b-4b80-b9f1-f6aa58d107d2)(content(Whitespace\"\\n\"))))(Tile((id \
         bb21ddf5-74db-46e4-81aa-0099eae5c4ab)(label(last))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         32804932-7a85-43f0-a7c0-2b1997e1c2bb)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         ace64d21-bf80-4ad2-97c1-aa1dc00b6604)(label([]))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         3211a6df-6ee4-4b3c-af01-e37922ffb40c)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         1f84cff8-a6bd-4d2e-b667-fd5d7dc721c3)(content(Whitespace\" \
         \"))))(Tile((id \
         dec0b1a3-febe-45c8-9cc6-7325ca80422f)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         bca2db9a-06dc-498b-8161-16811e704ba4)(content(Whitespace\"\\n\"))))(Tile((id \
         8a99d8a7-37f2-4df2-bead-401dcfc36555)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         2995f0cb-c2b8-4434-833f-ddd50bf8e3da)(content(Whitespace\" \
         \"))))(Tile((id \
         dba06ce1-f24b-4ae5-bf33-c6ee81c54694)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         ecbd76e3-436a-49c8-ae58-78e32f38aa4b)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         9cb2f3d1-8c4e-4a9d-9e74-3c2df931ce06)(content(Whitespace\"\\n\")))))";
      backup_text =
        "# LAST ELEMENT TASK                            #\n\
         #                                              #\n\
         # Implement last: get the last element of a    #\n\
         # list, or return a default if empty.          #\n\
         #                                              #\n\
         # Examples:                                    #\n\
         #   last([1, 2, 3], 0) == 3                    #\n\
         #   last([42], 0) == 42                        #\n\
         #   last([], 99) == 99                         #\n\
         #                                              #\n\
         # Available functions:                         #\n\
         #   fold_left(list, fn, init) -> result        #\n\
         #     fn takes (accumulator, element)          #\n\
         #   fold_right(list, fn, init) -> result       #\n\
         #     fn takes (element, accumulator)          #\n\
         #   rev(list) -> list                          #\n\
         #   length(list) -> Int                        #\n\
         #                                              #\n\
         # Tip: Think about what the fold should        #\n\
         # \"remember\" as it processes each element.     #\n\n\
         let last = fun (xs, default) ->\n\
         ?\n\
         in\n\n\
         test\n\
         last([1, 2, 3], 0)\n\
         == 3\n\
         end;\n\n\
         test\n\
         last([42], 0)\n\
         == 42\n\
         end;\n\n\
         test\n\
         last([1], 0)\n\
         == 1\n\
         end;\n\n\
         test\n\
         last([], 99)\n\
         == 99\n\
         end;\n\n\
         test\n\
         last([], 0)\n\
         == 0\n\
         end\n";
      refractors = "()";
    } )
