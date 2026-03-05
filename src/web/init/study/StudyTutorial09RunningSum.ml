let out : string * Haz3lcore.PersistentSegment.t =
  ( "Study / tutorial / 09-running-sum",
    {
      segment =
        "((Secondary((id \
         e2e8eea6-ab0f-4a2e-bb4a-0ce34a098d8c)(content(Comment\"# PROBES \
         TUTORIAL - PART 9: WRITING EXERCISE (FOLD)                \
         #\"))))(Secondary((id \
         9a641a9e-13a1-4df4-9625-267f1a62c64a)(content(Whitespace\"\\n\"))))(Secondary((id \
         4bb1eed9-0b4f-41a1-a4b9-11e5bb7ee2d5)(content(Whitespace\"\\n\"))))(Secondary((id \
         413a5bb1-be40-4b56-884d-a026d9b217cd)(content(Comment\"# Implement \
         `running_sum`: compute a list where each element       \
         #\"))))(Secondary((id \
         22b94340-d759-4337-87bb-65d58e967d35)(content(Whitespace\"\\n\"))))(Secondary((id \
         9af708ee-3d93-45f1-a514-1c485b6b8bf7)(content(Comment\"# is the sum \
         of all elements up to that position.                   \
         #\"))))(Secondary((id \
         71465d3c-8a7a-45d3-8d7c-911d13c1d3fc)(content(Whitespace\"\\n\"))))(Secondary((id \
         4d6084a4-349f-4b04-a769-4cca622fbc8f)(content(Comment\"#                                                                   \
         #\"))))(Secondary((id \
         cde483c3-0736-4088-930d-426f4f116e5f)(content(Whitespace\"\\n\"))))(Secondary((id \
         b0634744-9a91-4c0b-8a92-d6137975a756)(content(Comment\"#   \
         running_sum([1, 2, 3]) == [1, 3, 6]                            \
         #\"))))(Secondary((id \
         342553f8-71df-40c5-be4a-b1493c1b817e)(content(Whitespace\"\\n\"))))(Secondary((id \
         6315b7dc-0565-4729-9d36-797ab4dc45db)(content(Comment\"#   \
         running_sum([5]) == [5]                                         \
         #\"))))(Secondary((id \
         9493cda2-8ed8-4510-b23b-5ef06da76349)(content(Whitespace\"\\n\"))))(Secondary((id \
         985fc0da-c7ec-42b8-abe1-ddbef6e3cfd6)(content(Comment\"#   \
         running_sum([]) == []                                           \
         #\"))))(Secondary((id \
         69d46c10-ce27-4f7b-95d8-28ef38867653)(content(Whitespace\"\\n\"))))(Secondary((id \
         db53acb3-7e58-4afd-84fa-c0a78070417f)(content(Comment\"#                                                                   \
         #\"))))(Secondary((id \
         c13a4a32-312d-4d29-b4dd-c0f05c76babc)(content(Whitespace\"\\n\"))))(Secondary((id \
         b74d01df-51ce-44ea-b5f7-2eabdabd1ea7)(content(Comment\"# Use \
         fold_left to walk through the list:                           \
         #\"))))(Secondary((id \
         b73bd0f2-89aa-41bf-9c18-ff8d2ba72977)(content(Whitespace\"\\n\"))))(Secondary((id \
         67529619-d66b-4237-89d4-f438a701089c)(content(Comment\"#   \
         fold_left(list, fn, init) -> result                             \
         #\"))))(Secondary((id \
         af78ee89-dd5a-47b3-83e1-e65a24bd75bc)(content(Whitespace\"\\n\"))))(Secondary((id \
         ba3c8c24-9623-40a9-97a6-423faeebc86a)(content(Comment\"#   fn takes \
         (accumulator, element) and returns new accumulator     \
         #\"))))(Secondary((id \
         8cd797e0-ded0-4bcf-8109-6e3ff9983b22)(content(Whitespace\"\\n\"))))(Secondary((id \
         2fc12f51-d655-4332-80d1-12ca30ec443b)(content(Comment\"#                                                                   \
         #\"))))(Secondary((id \
         551dd66f-4a55-4e9e-bfb7-7f3e533ce389)(content(Whitespace\"\\n\"))))(Secondary((id \
         e6347ee6-441e-490d-beed-8f74e086b662)(content(Comment\"# Tip: You'll \
         need to track both a running total and the result    \
         #\"))))(Secondary((id \
         d0335e1b-86ec-4082-87fd-ec7c523cc1fa)(content(Whitespace\"\\n\"))))(Secondary((id \
         71723fab-d071-491c-8352-ec442032926e)(content(Comment\"# list. Use a \
         tuple (total, result_list) as your accumulator.      \
         #\"))))(Secondary((id \
         c3209000-a367-4b96-869e-c0cf08e0651e)(content(Whitespace\"\\n\"))))(Secondary((id \
         85e845bc-bae4-46dc-938a-4869d4dba856)(content(Comment\"#                                                                   \
         #\"))))(Secondary((id \
         ca476fdb-7064-4fab-ac48-0d21dc911477)(content(Whitespace\"\\n\"))))(Secondary((id \
         9e481ef4-fb61-48b1-8eea-50d99097ecdd)(content(Comment\"# Other useful \
         functions:                                           \
         #\"))))(Secondary((id \
         4c20d116-539c-4d68-ac44-bb2a5636589d)(content(Whitespace\"\\n\"))))(Secondary((id \
         b2f349e8-6ead-4a72-9828-eda5ae515f2e)(content(Comment\"#   \
         append(list1, list2) -> list                                    \
         #\"))))(Secondary((id \
         132bf22c-68b7-4a79-8ea3-209169a750cc)(content(Whitespace\"\\n\"))))(Secondary((id \
         7237ac7c-a471-4014-9f13-dea22ca94dac)(content(Comment\"#   Tuple \
         access via pattern: let (x, y) = t                       \
         #\"))))(Secondary((id \
         cbedf8ea-dccd-4730-8a22-5ad95305e13d)(content(Whitespace\"\\n\"))))(Secondary((id \
         6a12b23a-0cca-43e5-b4b1-0ed744e795f2)(content(Comment\"#                                                                   \
         #\"))))(Secondary((id \
         fa6a26c4-c33e-443c-8df0-5a7848d10b36)(content(Whitespace\"\\n\"))))(Secondary((id \
         59d386a7-aafb-406f-b5ab-8e4779a502ff)(content(Comment\"# Turn on \
         auto-probe and click inside your fold callback.           \
         #\"))))(Secondary((id \
         9d7ebfec-1754-4f8d-b9fe-a8e66684cbd9)(content(Whitespace\"\\n\"))))(Secondary((id \
         872a5a6d-088d-4a2b-8339-04b545b4a0a7)(content(Comment\"# In Many mode \
         you can see the accumulator at each step.            \
         #\"))))(Secondary((id \
         9efd2311-9e68-4949-8bbf-3594d4a77526)(content(Whitespace\"\\n\"))))(Secondary((id \
         a53c174e-e3d8-4271-a4d1-6740c6c8cfbb)(content(Comment\"# \
         =============================================================== \
         #\"))))(Secondary((id \
         7746b703-8eb1-4fc4-9eca-b2e7910bd9fa)(content(Whitespace\"\\n\"))))(Secondary((id \
         32ad92cc-9846-4d39-8535-78e66af2c47d)(content(Whitespace\"\\n\"))))(Tile((id \
         3658a8ef-b6da-4e53-b944-59fa5768eae2)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         cf74bdcf-397c-4e14-9b9b-0b0cd5d40207)(content(Whitespace\" \
         \"))))(Tile((id \
         b5e5f9f8-f066-447d-896b-37af3acec01e)(label(running_sum))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         af1a956b-223a-4c6e-9e8d-648dc3324ecc)(content(Whitespace\" \
         \")))))((Secondary((id \
         5dcbccb2-b507-4f6f-b318-b00d78f592ac)(content(Whitespace\" \
         \"))))(Tile((id d2953ee9-6e47-4d27-a88e-6f41a240fa42)(label(fun \
         ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 37))(sort Exp))))))(shards(0 \
         1))(children(((Secondary((id \
         f8ddf805-c63f-4bdc-a2a3-64d042b4307b)(content(Whitespace\" \
         \"))))(Tile((id \
         7eb4dde0-bda5-40a0-88b7-da32fbaa77f9)(label(nums))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         d212e65f-b28d-48a6-a671-f251f5e5cc9c)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         978d5b55-d463-4ea8-8770-bb26d15af83d)(content(Whitespace\"\\n\"))))(Tile((id \
         d40dd223-6e8c-42e9-a724-5fd671cf8e66)(label(?))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         c9f69c6a-9ad8-460e-9a4b-427c30447dbc)(content(Whitespace\"\\n\"))))(Secondary((id \
         2c10fb22-e9d3-446a-853f-89fa99f21957)(content(Whitespace\"\\n\"))))(Secondary((id \
         5e98069d-1c73-4b8c-ab80-d944e55f46a4)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         a23763d6-b765-414b-aaa5-a9247b08cefe)(content(Whitespace\"\\n\"))))(Secondary((id \
         3a24c851-372c-4bb9-8d54-67a2490c6f48)(content(Whitespace\"\\n\"))))(Tile((id \
         a524a75c-644f-47d3-863f-3629b41fd005)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         7d822f9a-ba65-4f1b-9f6c-639187029168)(content(Whitespace\"\\n\"))))(Tile((id \
         11a786eb-3551-43f3-8ec3-e72d8f4e0749)(label(running_sum))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         bf167791-4604-457f-baaf-bb0ad55c5084)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         e780f1cf-f8b4-4c55-a6af-c1625916e99a)(label([ ]))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         0e3ad7ec-1978-4e40-9bb9-eb548d84a33d)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         15ec2b56-bbf2-4811-8e11-bc0adb9c52fb)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         086fb344-0868-43fa-9616-671fc6d0cfea)(content(Whitespace\" \
         \"))))(Tile((id \
         5cf2c3b6-3bcb-4028-b582-ecf1975f1bc7)(label(2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         9bf9d88e-8867-4068-8717-177122f93f8b)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         c90f86d4-b546-4101-b950-6cfbcc52fd15)(content(Whitespace\" \
         \"))))(Tile((id \
         60309965-516e-4f48-9dab-2f67f86024f0)(label(3))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))))))))))))(Secondary((id \
         5c9e448b-2243-4bd5-ae3b-1f55ea8ce81c)(content(Whitespace\"\\n\"))))(Tile((id \
         4f16fe73-6037-4107-8757-284d61b65f85)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         8b20677f-9aab-49b8-8176-193170fb9c70)(content(Whitespace\" \
         \"))))(Tile((id c22e12d3-9656-49a2-8dcc-84f99c5cba9e)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         84587a7a-adaa-4207-8375-b4ca438ffccb)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         547e761e-1d92-40d0-a9c0-fbd257fcdbb5)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         71590424-d164-4942-9fd0-0679bbd392dc)(content(Whitespace\" \
         \"))))(Tile((id \
         7d573c19-97cd-49c6-b3d9-36b0e8217d82)(label(3))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         b80408e3-fe36-4437-b288-ec4f04bc9601)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         7139e21d-3ae1-441a-8fb9-e4d45d7a5508)(content(Whitespace\" \
         \"))))(Tile((id \
         81583002-12bd-486b-aeb8-f8472dfd3cde)(label(6))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         f79cf399-0949-444e-8918-532a95f239ab)(content(Whitespace\"\\n\")))))))))(Tile((id \
         28fbc0b4-bef0-421a-85d5-4d60eba04fb7)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         ebe06cce-ed2c-4caa-a0a4-1c581dafafa0)(content(Whitespace\"\\n\"))))(Secondary((id \
         e700639d-8194-4421-92d6-f745faf54789)(content(Whitespace\"\\n\"))))(Tile((id \
         01388c3d-9449-414e-ae9d-a9d1c43acbd9)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         f0905921-0632-4ef6-8c65-10298c2edf26)(content(Whitespace\"\\n\"))))(Tile((id \
         43750c2a-7d6c-45c1-ad27-424c7ae5b74e)(label(running_sum))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         6bc2ee41-88f2-458d-928d-9e0b5c4ab5aa)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         f91f41c1-9142-42a4-9a46-46926b42f595)(label([ ]))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         fa65bc2f-10e9-45ed-8673-d829457474df)(label(5))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))))))))))))(Secondary((id \
         dbd04f6c-46e0-4f50-8c28-74dfc4888eb1)(content(Whitespace\"\\n\"))))(Tile((id \
         a1fbdf68-648f-4022-91e9-5f1b21cfbd24)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         3bf58ca9-5352-43ce-87f8-7e9e3db602c5)(content(Whitespace\" \
         \"))))(Tile((id c96d2716-4a98-451e-b0e5-0d8d4d8c5fff)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         8d30e25a-e5fe-48b9-9529-b70067f2cbeb)(label(5))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         70c173d8-4ebf-4fe1-aa5e-1616738d0e92)(content(Whitespace\"\\n\")))))))))(Tile((id \
         e1dcafc7-a40f-4e0d-9c19-07d7a8b467fa)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         ce3fa9c7-d182-4d32-876a-f0a151d000af)(content(Whitespace\"\\n\"))))(Secondary((id \
         9ad27917-f4e6-477f-a00c-63a1b46cf1a7)(content(Whitespace\"\\n\"))))(Tile((id \
         28004b29-9ba5-4831-bcb5-6c91e7e86347)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         f79ed83c-875d-4c8a-abc3-8e1326f18293)(content(Whitespace\"\\n\"))))(Tile((id \
         2305f59b-a91b-4690-918f-182fe631eecb)(label(running_sum))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         adc9740d-8da8-48e9-835a-b999a0df5eae)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         3040b23b-fe49-46a0-94e1-af71e4464778)(label([]))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         e09f86c4-d4db-490f-a797-541b2a1f3f19)(content(Whitespace\"\\n\"))))(Tile((id \
         9324e779-0a8f-476c-8feb-bec65cab6e38)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         71644882-c9ea-4be7-b24d-b9d405a38a8c)(content(Whitespace\" \
         \"))))(Tile((id \
         1b8147b5-cff7-4d08-b60f-d11d88e66979)(label([]))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         fd8f2920-2189-4910-a236-a137ab1d63d8)(content(Whitespace\"\\n\")))))))))(Tile((id \
         be8e65fb-8295-42ca-a10b-218aa3bb0bbd)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         acf40d10-0787-4f8a-b424-0e7528d20bec)(content(Whitespace\"\\n\"))))(Secondary((id \
         19780667-f064-4d98-80fa-cf8524d7605c)(content(Whitespace\"\\n\"))))(Tile((id \
         81cd2c5b-a519-47e8-a3c3-05f8e4da067a)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         0d04709f-096f-439f-8c34-c757f273f49c)(content(Whitespace\"\\n\"))))(Tile((id \
         db56fa28-c6b6-4a25-89bf-2725f9fd00de)(label(running_sum))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         e795559a-66ae-4c73-950c-7c6891afb87a)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         e3c9d9ed-f97c-4bcf-acc4-da9cc2568875)(label([ ]))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         2f2493fe-32f7-4cea-95a9-59a313d577fc)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         c170e9f8-5e97-4ac8-911c-5b095e12e10e)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         399d40ea-606d-4871-bff4-0f4a44bcd769)(content(Whitespace\" \
         \"))))(Tile((id \
         d437ebd1-17c2-4983-b123-ded4718da90a)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         7c761ef8-ab25-40eb-bb29-8ff5c2aa0262)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         d5574ba5-e332-4cdb-a0aa-1d4a5cd450e3)(content(Whitespace\" \
         \"))))(Tile((id \
         72b18b85-00e7-4af7-83a7-9a2a3f299798)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         20e4ad25-67b1-406e-beb1-3cdb2041a14f)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         354d53ec-ee60-4dcc-b477-8b5ff602b216)(content(Whitespace\" \
         \"))))(Tile((id \
         583fe8ef-4782-478f-ab44-4318e378e4e8)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))))))))))))(Secondary((id \
         1b874a14-ba6d-4192-bc18-461d765df0be)(content(Whitespace\"\\n\"))))(Tile((id \
         974b6923-1d3e-420b-90c2-badf73ffe42c)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         2ff8736c-38e8-4a78-a66f-72efec16d988)(content(Whitespace\" \
         \"))))(Tile((id 5e2b27b0-0653-4bb0-b8ca-3cb9a773cdb8)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         e51d5fb2-809c-4863-878b-e75b4fea1ffd)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         cbec9077-6dfb-4315-aacd-72981a329700)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         36bf142c-380c-446c-894a-5ba5d4fc3720)(content(Whitespace\" \
         \"))))(Tile((id \
         13beda1b-1954-4457-8875-e762fad10f09)(label(2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         5cbbb860-64b8-4ed1-91ed-946b3582664d)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         46ff3144-389a-42f0-befc-d81cd4f59934)(content(Whitespace\" \
         \"))))(Tile((id \
         81bef1a5-d571-4102-be39-7db4399cd2f9)(label(3))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         6eaf4fc7-0001-4952-84a2-26c3e39172cf)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         ffee316c-b262-48d8-9ef6-71a71913cac7)(content(Whitespace\" \
         \"))))(Tile((id \
         194b567a-de71-4237-a822-bca38d0b3f8b)(label(4))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         7c05ecf3-7c3f-4962-b3f3-5ade93e18f31)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         86b87530-8ba9-457f-bac3-33c11e413c57)(content(Whitespace\"\\n\"))))(Secondary((id \
         a9a9ab96-23c7-4a45-8be0-79f98d1d42e4)(content(Whitespace\"\\n\"))))(Secondary((id \
         00fc4739-64d5-49e8-b8e2-659696a1f500)(content(Comment\"# END OF PART \
         9 - Select the next slide from the top menu       \
         #\"))))(Secondary((id \
         9ffeefd8-7689-4eff-b5a1-1241c27fc17e)(content(Whitespace\"\\n\")))))";
      backup_text =
        "# PROBES TUTORIAL - PART 9: WRITING EXERCISE (FOLD)                #\n\n\
         # Implement `running_sum`: compute a list where each element       #\n\
         # is the sum of all elements up to that position.                   #\n\
         #                                                                   #\n\
         #   running_sum([1, 2, 3]) == [1, 3, 6]                            #\n\
         #   running_sum([5]) == [5]                                         #\n\
         #   running_sum([]) == []                                           #\n\
         #                                                                   #\n\
         # Use fold_left to walk through the list:                           #\n\
         #   fold_left(list, fn, init) -> result                             #\n\
         #   fn takes (accumulator, element) and returns new accumulator     #\n\
         #                                                                   #\n\
         # Tip: You'll need to track both a running total and the result    #\n\
         # list. Use a tuple (total, result_list) as your accumulator.      #\n\
         #                                                                   #\n\
         # Other useful functions:                                           #\n\
         #   append(list1, list2) -> list                                    #\n\
         #   Tuple access via pattern: let (x, y) = t                       #\n\
         #                                                                   #\n\
         # Turn on auto-probe and click inside your fold callback.           #\n\
         # In Many mode you can see the accumulator at each step.            #\n\
         # =============================================================== #\n\n\
         let running_sum = fun nums ->\n\
         ?\n\n\n\
         in\n\n\
         test\n\
         running_sum([1, 2, 3])\n\
         == [1, 3, 6]\n\
         end;\n\n\
         test\n\
         running_sum([5])\n\
         == [5]\n\
         end;\n\n\
         test\n\
         running_sum([])\n\
         == []\n\
         end;\n\n\
         test\n\
         running_sum([1, 1, 1, 1])\n\
         == [1, 2, 3, 4]\n\
         end\n\n\
         # END OF PART 9 - Select the next slide from the top menu       #\n";
      refractors = "()";
    } )
