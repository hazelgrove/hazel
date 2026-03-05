let out : string * Haz3lcore.PersistentSegment.t =
  ( "Study / writing / crop-tally / crop-tally-sketch",
    {
      segment =
        "((Secondary((id \
         dcdcc0c5-6a99-4af9-8f14-64848dbbc918)(content(Comment\"# Crop \
         Tally                                           #\"))))(Secondary((id \
         b374df41-5edb-4741-ba32-a830fa8abe61)(content(Whitespace\"\\n\"))))(Secondary((id \
         c0add89d-4d63-4145-9aeb-94f3c9f4f83e)(content(Comment\"#                                                      \
         #\"))))(Secondary((id \
         986b2111-5c8d-4669-80eb-4ea455662287)(content(Whitespace\"\\n\"))))(Secondary((id \
         c4cfaac4-22b3-465b-881f-2883418d011a)(content(Comment\"# Garden rows \
         are recorded as space-separated          #\"))))(Secondary((id \
         576e3430-7196-46d0-8ba8-618bbbc9f5c3)(content(Whitespace\"\\n\"))))(Secondary((id \
         86da749d-f3d1-4cb7-86ca-64423e786ff9)(content(Comment\"# strings of \
         plant names:                              #\"))))(Secondary((id \
         d2bbd35a-6bf4-4a3a-bed7-7f5ac3b2395c)(content(Whitespace\"\\n\"))))(Secondary((id \
         069560ab-d610-40aa-8eb2-789f85b4efdf)(content(Comment\"#   \\\"fern \
         orchid fern cactus\\\"                          #\"))))(Secondary((id \
         69201b1a-2ea0-40ca-9419-dd114dced59b)(content(Whitespace\"\\n\"))))(Secondary((id \
         f87489e2-a592-4dee-9e30-4ec4b0298d2c)(content(Comment\"#   \\\"orchid \
         starfern fern orchid\\\"                      #\"))))(Secondary((id \
         7038b95f-2dfa-49d5-b19c-121abe26ebb4)(content(Whitespace\"\\n\"))))(Secondary((id \
         87e2fff8-35f7-47e1-b20a-a4b1032313d1)(content(Comment\"#                                                      \
         #\"))))(Secondary((id \
         e990194f-4a86-4bd5-9239-c8db31bb4c9b)(content(Whitespace\"\\n\"))))(Secondary((id \
         f2a2a701-cb23-4508-91a8-f8d9e219b8d8)(content(Comment\"# Count how \
         many times a given plant appears           #\"))))(Secondary((id \
         2e46beb7-41bc-4c29-92f6-4d21b08508fb)(content(Whitespace\"\\n\"))))(Secondary((id \
         cd6ea663-38c5-403c-a3e8-b4c874b75bd7)(content(Comment\"# across all \
         rows of the garden.                       #\"))))(Secondary((id \
         dd9dd0f9-e11f-41d2-9b39-ab4cbe0103e1)(content(Whitespace\"\\n\"))))(Secondary((id \
         2dfe207c-a4f6-4c73-9d4c-8cca0bf6206c)(content(Comment\"#                                                      \
         #\"))))(Secondary((id \
         b81fcfc5-9654-4c24-a5a2-34d0a262e7ba)(content(Whitespace\"\\n\"))))(Secondary((id \
         fa8061dc-7b52-4315-92b2-3da634713cdd)(content(Comment\"# \
         Steps:                                               \
         #\"))))(Secondary((id \
         18aae471-f263-452c-aa6a-102f006a9b6f)(content(Whitespace\"\\n\"))))(Secondary((id \
         fcb04b10-f2f5-433a-8659-be3016a1af65)(content(Comment\"#   1. \
         count_in_row: count a plant in one row string   #\"))))(Secondary((id \
         58af19b6-0b66-49db-ac22-89f9667073c2)(content(Whitespace\"\\n\"))))(Secondary((id \
         5d729cbe-9e91-4182-a7e0-aeab2caa6776)(content(Comment\"#   2. \
         count_in_garden: total a plant across all rows  #\"))))(Secondary((id \
         8125a70b-ed75-44d1-bd99-3d44a97774c5)(content(Whitespace\"\\n\"))))(Secondary((id \
         472466b6-f5e6-42c3-b6e8-a1eda5f4d5f4)(content(Comment\"#                                                      \
         #\"))))(Secondary((id \
         ef432b63-5bcc-41e1-954f-028c4ca0af63)(content(Whitespace\"\\n\"))))(Secondary((id \
         6e786f79-8207-421b-9395-9332b4add95c)(content(Comment\"# Available \
         functions:                                 #\"))))(Secondary((id \
         2762e53f-c3c6-4690-a4fd-78b29a7de35e)(content(Whitespace\"\\n\"))))(Secondary((id \
         85acfb6f-602e-47ff-8c79-d265817e0383)(content(Comment\"#   \
         string_split: (String, String) -> [String]         \
         #\"))))(Secondary((id \
         996cfb65-b9c5-47f5-8d58-735e9400773e)(content(Whitespace\"\\n\"))))(Secondary((id \
         686f4881-1e1a-4108-9434-cd19161d2eaf)(content(Comment\"#   filter: \
         ([?], ? -> Bool) -> [?]                    #\"))))(Secondary((id \
         df70bcc3-7bad-4ba9-844d-abc9fc89eb01)(content(Whitespace\"\\n\"))))(Secondary((id \
         5dfa8c8a-c8d7-432f-a385-b99ab0580cd1)(content(Comment\"#   length: \
         [?] -> Int                                 #\"))))(Secondary((id \
         9c2e64ef-2d25-4831-810e-06fe312a9532)(content(Whitespace\"\\n\"))))(Secondary((id \
         52257c12-7f6f-45f0-8441-f3e0d7432da6)(content(Comment\"#   map: ([?], \
         ? -> ?) -> [?]                          #\"))))(Secondary((id \
         9b729e8d-c876-4ee1-814f-ee35402cbda1)(content(Whitespace\"\\n\"))))(Secondary((id \
         1a7e830a-7560-4add-9f3c-282e6c22195c)(content(Comment\"#   fold_left: \
         ([?], (?, ?) -> ?, ?) -> ?              #\"))))(Secondary((id \
         56d182e4-8d55-439d-bb83-846021d37333)(content(Whitespace\"\\n\"))))(Secondary((id \
         5a41d621-8054-4e4f-a9cf-686492dc1926)(content(Comment\"#                                                      \
         #\"))))(Secondary((id \
         bd2f922c-66ae-4109-a695-daa50c914990)(content(Whitespace\"\\n\"))))(Secondary((id \
         3944284e-7017-4c84-954e-7b3834a82dd9)(content(Comment\"# fold_left \
         combines list elements into one value      #\"))))(Secondary((id \
         b88ab173-a473-4833-b83f-beca2eb9297a)(content(Whitespace\"\\n\"))))(Secondary((id \
         e584a13e-7427-4b4b-819e-fd4eb66d03a7)(content(Comment\"# using a \
         function and a starting value.               #\"))))(Secondary((id \
         9c695993-c54d-4661-970e-7ff6cb4aaad5)(content(Whitespace\"\\n\"))))(Secondary((id \
         d836ef65-80b1-40c2-87d4-436122aa0eb2)(content(Comment\"#                                                      \
         #\"))))(Secondary((id \
         4a629bd7-60c0-4d2f-b971-e13e8ff7ea4e)(content(Whitespace\"\\n\"))))(Secondary((id \
         d2e7f499-499e-4fc7-8863-f8520bab4c30)(content(Comment\"# Tip: Try \
         each function on a simple example first     #\"))))(Secondary((id \
         2a8ac65a-47af-4ef1-8a0c-8bfac0bdc5f2)(content(Whitespace\"\\n\"))))(Secondary((id \
         4b41f03a-ccc6-4372-a180-a429305e3f0f)(content(Comment\"# and probe \
         the result. The argument order for these   #\"))))(Secondary((id \
         e8589300-33b2-450d-bc6f-b466976491d8)(content(Whitespace\"\\n\"))))(Secondary((id \
         628719e9-a281-409e-a5b8-a0d2439bb837)(content(Comment\"# functions \
         may not be what you expect!                #\"))))(Secondary((id \
         5279d892-eb2f-4f96-acca-8ebabcb40370)(content(Whitespace\"\\n\"))))(Secondary((id \
         a9167c31-49c1-4a2b-b0da-b84e3b4198cc)(content(Whitespace\"\\n\"))))(Tile((id \
         3cbf041d-b2e5-401d-a083-b5ce1240a18b)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         b6d63820-b1b6-434f-a84d-4dedde751073)(content(Whitespace\" \
         \"))))(Tile((id \
         d70e874a-8db5-420f-8110-d37ba852ce8d)(label(garden))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         8c18b8f7-a6d0-49d6-a870-5289f6b1163e)(content(Whitespace\" \
         \")))))((Secondary((id \
         d8f0a808-c3cf-4817-b757-e35276ed8962)(content(Whitespace\" \
         \"))))(Tile((id 4baded80-8f65-4d8d-88e4-e132f5df7328)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         33be2cd7-7382-478f-a002-1cd6392afc2d)(content(Whitespace\"\\n\"))))(Tile((id \
         86949be4-5b05-46e1-845c-4070c4d513f8)(label(\"\\\"fern orchid fern \
         cactus\\\"\"))(mold((out Exp)(in_())(nibs(((shape Convex)(sort \
         Exp))((shape Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
         726029da-5bb0-4ba8-bbe2-48b6e2a5384a)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         d22e1b4f-411e-4943-86df-262d0510069a)(content(Whitespace\"\\n\"))))(Tile((id \
         c04b8cd2-bebe-4406-8d1e-380a3a9cba2c)(label(\"\\\"orchid starfern \
         fern orchid\\\"\"))(mold((out Exp)(in_())(nibs(((shape Convex)(sort \
         Exp))((shape Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
         916efac1-7b33-467c-9186-d5b290f1589d)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         60651b7b-44d6-4f90-b84e-3351220b27ae)(content(Whitespace\"\\n\"))))(Tile((id \
         56788687-0d95-4d7b-872c-9f7bd5823857)(label(\"\\\"cactus fern orchid \
         orchid\\\"\"))(mold((out Exp)(in_())(nibs(((shape Convex)(sort \
         Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         d4908682-1ceb-447a-89c0-240b24d67c7d)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         d4a604e8-ff13-4480-a8e0-80a7aa87ba6a)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         dba53152-3756-496d-bce1-631ea7d5db70)(content(Whitespace\"\\n\"))))(Secondary((id \
         17ebc9d4-2b26-42bd-9c3c-bf05f4d262d1)(content(Whitespace\"\\n\"))))(Secondary((id \
         236915e1-ff20-47e6-a8e2-d7692ce27253)(content(Comment\"# Count how \
         many times plant appears in a row string #\"))))(Secondary((id \
         a6bceb8a-9e80-474f-a91e-c4c69e7dfe0f)(content(Whitespace\"\\n\"))))(Tile((id \
         c75ce340-2b50-4480-95b3-2b0044c0bc59)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         fd2815ed-e66f-4653-84ad-f14547f90351)(content(Whitespace\" \
         \"))))(Tile((id \
         4fd4ca2a-082e-4112-984a-96283f442071)(label(count_in_row))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         d028c804-6ed7-4412-983f-ca572f529911)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 24))(sort Pat))((shape(Concave \
         24))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         96b96bf2-cb1f-47d6-9a84-d554be73aeaa)(content(Whitespace\" \
         \"))))(Tile((id \
         611ae74e-8e66-4a01-a2e4-a8b9b6329906)(label(\"(\"\")\"))(mold((out \
         Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0 1))(children(((Tile((id \
         38feecc9-2e06-43a5-a2b7-36564b708c63)(label(String))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         88dc8af9-903c-43ae-863c-d543b7df67a7)(label(,))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 44))(sort Typ))((shape(Concave \
         44))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         e195e4d3-8cbc-4a8f-b9ab-69cef622c068)(content(Whitespace\" \
         \"))))(Tile((id \
         0d2329e0-deef-47f9-84f1-393fcc352fe6)(label(String))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children()))))))))(Secondary((id \
         6845d922-034b-4911-b63c-ff412cef3032)(content(Whitespace\" \
         \"))))(Tile((id \
         d8b9c4b2-2ed0-44b8-8137-374ef5121416)(label(->))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 13))(sort Typ))((shape(Concave \
         13))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         fdc1284e-81b2-457e-99f9-f0c13680af53)(content(Whitespace\" \
         \"))))(Tile((id \
         b3b999a8-27c3-4678-8fd5-ab1c9e967cb7)(label(Int))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         3da8a473-8ad8-4893-a17a-3586b8865f75)(content(Whitespace\" \
         \")))))((Secondary((id \
         30b4eedd-a767-47fb-b6f2-d50831e9d85a)(content(Whitespace\"\\n\"))))(Tile((id \
         4fc1d160-08ed-46c9-912a-de8cdff621aa)(label(fun ->))(mold((out \
         Exp)(in_(Pat))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         37))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         b381b6c5-f0f3-432a-82ed-087d71785e09)(content(Whitespace\" \
         \"))))(Tile((id \
         d1a6a762-c3a9-45d6-99ca-e72246c0bec4)(label(\"(\"\")\"))(mold((out \
         Pat)(in_(Pat))(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0 1))(children(((Tile((id \
         ec79e177-c369-49c4-ae98-a4ce8e77fe41)(label(row))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         01f6c5ac-a091-45ba-95b7-9416d9b68ab4)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 44))(sort Pat))((shape(Concave \
         44))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         93afc358-ac78-44d3-9941-6e3128676e27)(content(Whitespace\" \
         \"))))(Tile((id \
         f1fa0067-2b29-4caf-8055-13531451aeb3)(label(plant))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children()))))))))(Secondary((id \
         e3519961-90bd-47a0-ab0c-70167f1a93ba)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         f607213f-163c-488e-9c7c-844a35f273c8)(content(Whitespace\"\\n\"))))(Tile((id \
         b1bcaace-2b00-4603-8748-fdd9948b6dbd)(label(?))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         b9808d11-8bd6-4616-b58e-9d746b2a9115)(content(Whitespace\"\\n\"))))(Secondary((id \
         021bfa88-777e-4db8-a881-41939eea3eaa)(content(Whitespace\"\\n\"))))(Secondary((id \
         f01aae22-34ab-40dc-b56e-3860228cc76e)(content(Whitespace\"\\n\"))))(Secondary((id \
         cf478413-6061-4b51-95b6-c193691b713a)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         70772909-e062-435f-af55-3276b95b8f64)(content(Whitespace\"\\n\"))))(Secondary((id \
         fbc6240c-5398-40e0-b9c2-d1dc5b9e0e1b)(content(Whitespace\"\\n\"))))(Secondary((id \
         3b274c1c-265b-4d9e-be3b-d9a438a3572e)(content(Comment\"# Count a \
         plant across all garden rows #\"))))(Secondary((id \
         6053845c-fae7-4caa-8d94-b204c7b9eb45)(content(Whitespace\"\\n\"))))(Tile((id \
         80dd1c59-68e1-44b9-9b86-41dc1b78eb0e)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         acfd1ceb-4314-46b5-8ea4-691eb5994834)(content(Whitespace\" \
         \"))))(Tile((id \
         4d2285f2-484e-4131-a5fc-4d0bdfdd0f1f)(label(count_in_garden))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         e68f0b41-5d2e-4cd0-ac55-dd360c60487a)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 24))(sort Pat))((shape(Concave \
         24))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         5ee571aa-443a-40e0-b81f-1b641416c169)(content(Whitespace\" \
         \"))))(Tile((id \
         d57921d5-56bc-4234-8de9-3160219bc8ba)(label(\"(\"\")\"))(mold((out \
         Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0 1))(children(((Tile((id \
         e8a50a58-6598-4ce7-8b33-8e6b32700695)(label([ ]))(mold((out \
         Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0 1))(children(((Tile((id \
         23399e1d-7a85-4a0b-b72b-448c2b960be4)(label(String))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children()))))))))(Tile((id \
         f0d27b27-36c8-4181-9b57-a5593b1073c0)(label(,))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 44))(sort Typ))((shape(Concave \
         44))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         18e2a1f2-7d4c-4be0-8658-7146b3552ee0)(content(Whitespace\" \
         \"))))(Tile((id \
         42c441ec-ea7d-4b11-9484-570818260565)(label(String))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children()))))))))(Secondary((id \
         23405a86-2815-41c7-ab0f-bb92c59866ad)(content(Whitespace\" \
         \"))))(Tile((id \
         05d5f4f5-e99f-4c61-b9d3-914598629139)(label(->))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 13))(sort Typ))((shape(Concave \
         13))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         ec274a1e-64e2-4ded-b0e2-e6861c518158)(content(Whitespace\" \
         \"))))(Tile((id \
         106afc5e-88f5-46a7-9af9-372b1ff6cc21)(label(Int))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         2614507d-691d-4145-8733-c7ae74db48f5)(content(Whitespace\" \
         \")))))((Secondary((id \
         b70cf507-38a7-4e92-bfc0-1f903bc84ab7)(content(Whitespace\"\\n\"))))(Tile((id \
         a512a766-58a6-4489-8b72-5c78a8e38592)(label(fun ->))(mold((out \
         Exp)(in_(Pat))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         37))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         cb88bda6-8254-4305-a86f-2b0eba1afa58)(content(Whitespace\" \
         \"))))(Tile((id \
         83fe1de0-1ea0-4a0f-990a-5be1b3626a86)(label(\"(\"\")\"))(mold((out \
         Pat)(in_(Pat))(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0 1))(children(((Tile((id \
         668339c4-64c8-41e7-89db-ec10560df2e6)(label(garden))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         f3e0251c-3fa9-439e-97fa-8c83e4568b19)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 44))(sort Pat))((shape(Concave \
         44))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         a02b1584-424b-4552-a59f-4bc32935a616)(content(Whitespace\" \
         \"))))(Tile((id \
         d070e7ee-5558-4db5-8d9a-83f050943174)(label(plant))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children()))))))))(Secondary((id \
         e71f6a4e-7c74-449a-8f96-39ab91fe6200)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         77118bb4-3524-46f0-aa95-9d7e8c2c2663)(content(Whitespace\"\\n\"))))(Tile((id \
         c121e2c9-73f0-4d7e-b063-a2f50a958d01)(label(?))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         09c8ad17-c1f8-430b-b74f-4258fb6b1112)(content(Whitespace\"\\n\"))))(Secondary((id \
         0ac492b2-db8a-4cb2-be1a-9598b7d43c24)(content(Whitespace\"\\n\"))))(Secondary((id \
         ac699f4a-431f-44f0-8d4e-13104c8a5b09)(content(Whitespace\"\\n\"))))(Secondary((id \
         1b07aad8-e989-4f14-9e42-d78f90d21ac5)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         9901fd41-c35a-481a-8ca7-dce8f941760d)(content(Whitespace\"\\n\"))))(Secondary((id \
         0f8d23bf-bc36-477b-8cc5-76df1df1ca90)(content(Whitespace\"\\n\"))))(Tile((id \
         b4cf29e3-3a59-45ae-b7f2-73bd20fc0784)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         771151a9-ade4-4757-ba9b-1ca429895244)(content(Whitespace\" \
         \"))))(Tile((id \
         b47745a2-330e-485b-a027-f77294e07d90)(label(count_in_row))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         ecddf60b-fd69-4264-aceb-35e6b33e034c)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         5ae06b7f-0059-4a94-af01-f55e3e66e96b)(label(\"\\\"fern orchid fern \
         cactus\\\"\"))(mold((out Exp)(in_())(nibs(((shape Convex)(sort \
         Exp))((shape Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
         f73d13f3-f35e-44af-a54a-2794cce5f0c4)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         058da444-4127-4af5-bb0b-ca32ccceb57d)(content(Whitespace\" \
         \"))))(Tile((id \
         c6e92288-6d4f-4244-8825-4fd77e44011f)(label(\"\\\"fern\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         6de58406-becc-4281-9068-08d3f7e33393)(content(Whitespace\" \
         \"))))(Tile((id \
         6a6cbf70-78d0-41cc-9c89-4de858dabdb5)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         c07449bb-5529-488f-a23f-1baa526a0cbc)(content(Whitespace\" \
         \"))))(Tile((id \
         366c2862-987c-405e-882e-d4f54fa9da8f)(label(2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         404bcb0b-806b-4980-b9e5-f858313784fe)(content(Whitespace\" \
         \")))))))))(Tile((id \
         4253b7de-98b5-4c9d-961c-975b9750a864)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         52d8f0e9-dd5a-4cb0-a591-3a19ccc3eb48)(content(Whitespace\"\\n\"))))(Secondary((id \
         aab3974e-85a3-495f-a42b-c1d03f6da649)(content(Whitespace\"\\n\"))))(Tile((id \
         1af0f333-385d-4066-bad6-b49ffdb0bfbe)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         4bca0e14-273b-4e76-bbdf-542157935b5c)(content(Whitespace\" \
         \"))))(Tile((id \
         c275841f-4008-4b17-be2c-a2f5439fbd56)(label(count_in_row))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         a26dda0a-7734-45e8-81a7-025741f41a2d)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         193ae434-08d7-42a6-ac07-c2839ac9e35d)(label(\"\\\"orchid starfern \
         fern orchid\\\"\"))(mold((out Exp)(in_())(nibs(((shape Convex)(sort \
         Exp))((shape Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
         a32b143b-8795-451e-b0ba-fc6d0cd034de)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         d5da38e8-2e49-4a08-97bc-0556bfec8aaf)(content(Whitespace\" \
         \"))))(Tile((id \
         377e96e3-7583-4a35-ac97-35710a5ae013)(label(\"\\\"fern\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         31946a17-6625-415d-ae0b-61eaf0a62de1)(content(Whitespace\" \
         \"))))(Tile((id \
         cef498d5-6038-4ce5-bc06-520c5e56e6e4)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         1f405dbc-3124-48cc-9608-871f192050f5)(content(Whitespace\" \
         \"))))(Tile((id \
         df11ecd4-3d88-4986-8b4f-0748b90546b9)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         10851fbc-cd21-4912-b88a-e61fc4fe1320)(content(Whitespace\" \
         \")))))))))(Tile((id \
         38f9cda4-df34-4cee-a730-24f2436d76f3)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         f1050146-c48f-4eab-b2f1-85dbe4434d35)(content(Whitespace\"\\n\"))))(Secondary((id \
         97f59a22-efad-4552-909e-2497544a6a70)(content(Whitespace\"\\n\"))))(Tile((id \
         0a388e72-0bb0-4422-9992-158100afe071)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         f7059b5c-cb56-4fbd-a879-61b1925f45aa)(content(Whitespace\" \
         \"))))(Tile((id \
         91d02db3-bc42-4c5f-b753-b2ebb270f13e)(label(count_in_garden))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         6b8ac1fb-1a96-4658-a79e-8fec77603d20)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         5fd3f354-25a1-4ce4-a85c-c2117751ebda)(label(garden))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         b22e1ca3-5b7a-4a57-8bc6-bfcb3bd26e56)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         f75cf103-1740-427d-811e-67a2f96b4b20)(content(Whitespace\" \
         \"))))(Tile((id \
         92b9fc82-b05e-4a46-a0c6-6725dfe9b637)(label(\"\\\"fern\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         2b893c50-8195-4caf-b7a2-c8e74b5e9772)(content(Whitespace\" \
         \"))))(Tile((id \
         cc747a58-9ab6-4940-8650-1cf32deec0ef)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         4d7fd87b-3e76-45fd-864f-4c85884ffac1)(content(Whitespace\" \
         \"))))(Tile((id \
         bd9ab730-5637-4915-a0c2-c0b5668b8142)(label(4))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         41af5a08-cf1e-474b-9e64-cae0d08250fb)(content(Whitespace\" \
         \")))))))))(Tile((id \
         73dc0f32-ec58-447f-9766-70486d7b0ef6)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         261b2839-3615-438b-8dea-513b1fcc3012)(content(Whitespace\"\\n\"))))(Secondary((id \
         a1a66153-0cdb-4385-af26-8a9dd8243df2)(content(Whitespace\"\\n\"))))(Tile((id \
         e659595e-aec5-4085-ae77-e3ff032dbce4)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         931e2ac2-8d4a-4dd8-b0a0-15fc5f4a3179)(content(Whitespace\" \
         \"))))(Tile((id \
         e71d2000-9275-429a-a249-9db204620712)(label(count_in_garden))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         f8483a62-f66d-4140-b3b3-9ac50bb02278)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         f1e52a62-8226-450a-81cc-4401c063ac08)(label(garden))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         b143526b-c91e-479e-b42f-befbb99218cf)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         48bd3c32-d094-4d71-8c07-476ae575722b)(content(Whitespace\" \
         \"))))(Tile((id \
         901dc7a8-a4bd-4067-96b6-3319bee3f69d)(label(\"\\\"orchid\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         5a19ac85-8f82-448b-8fbd-9d4b0c9a9ec5)(content(Whitespace\" \
         \"))))(Tile((id \
         fd661ae5-86ef-4141-a3bf-fd5c79d2c08d)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         33c82da9-0fc8-4e7f-834b-455597a74b45)(content(Whitespace\" \
         \"))))(Tile((id \
         77bbe4ee-10f1-4312-acd4-cf345266088f)(label(5))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         2038a2fe-c889-4af1-a4a4-45ef52e36193)(content(Whitespace\" \
         \")))))))))(Tile((id \
         1722c8d0-9913-48c8-b4e1-7c98d7074ce0)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         926b24cd-0c4b-47e2-b0e7-e12da473be92)(content(Whitespace\"\\n\"))))(Secondary((id \
         cd392510-1b9d-4e12-ac3d-b179b210ab20)(content(Whitespace\"\\n\"))))(Tile((id \
         c3b78f38-e8d5-4bfe-9812-738e67631a3a)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         4e89b5ff-dda0-4f25-95bf-997f1ff7baf6)(content(Whitespace\" \
         \"))))(Tile((id \
         a4ea263f-7c09-4ee5-abf2-188d3c202a4b)(label(count_in_garden))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         1ff33e3b-c54b-4174-952b-c099c9a62688)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         481b19f0-23af-4b5f-a087-0856fefcf596)(label(garden))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         2ee6f956-db1a-48ad-9547-81dd2f94bfd3)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         0b6b3e56-57c3-401e-98f0-c890e88ba9af)(content(Whitespace\" \
         \"))))(Tile((id \
         0e694c5c-9e9b-4806-acff-88827fece691)(label(\"\\\"cactus\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         5ef7b856-9ad2-44f4-94e9-782519f1b566)(content(Whitespace\" \
         \"))))(Tile((id \
         46761128-72d1-429d-9309-bd98448eb54e)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         1da8ceca-c03f-4e72-a71b-62b00dd1999c)(content(Whitespace\" \
         \"))))(Tile((id \
         a1d940df-2609-4ed3-be56-cbd9b2ec4d75)(label(2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         3f31b035-065e-4de4-96a1-f35f90c56ebb)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         9d522548-9281-4d3d-81e0-067fb2521903)(content(Whitespace\"\\n\")))))";
      backup_text =
        "# Crop Tally                                           #\n\
         #                                                      #\n\
         # Garden rows are recorded as space-separated          #\n\
         # strings of plant names:                              #\n\
         #   \"fern orchid fern cactus\"                          #\n\
         #   \"orchid starfern fern orchid\"                      #\n\
         #                                                      #\n\
         # Count how many times a given plant appears           #\n\
         # across all rows of the garden.                       #\n\
         #                                                      #\n\
         # Steps:                                               #\n\
         #   1. count_in_row: count a plant in one row string   #\n\
         #   2. count_in_garden: total a plant across all rows  #\n\
         #                                                      #\n\
         # Available functions:                                 #\n\
         #   string_split: (String, String) -> [String]         #\n\
         #   filter: ([?], ? -> Bool) -> [?]                    #\n\
         #   length: [?] -> Int                                 #\n\
         #   map: ([?], ? -> ?) -> [?]                          #\n\
         #   fold_left: ([?], (?, ?) -> ?, ?) -> ?              #\n\
         #                                                      #\n\
         # fold_left combines list elements into one value      #\n\
         # using a function and a starting value.               #\n\
         #                                                      #\n\
         # Tip: Try each function on a simple example first     #\n\
         # and probe the result. The argument order for these   #\n\
         # functions may not be what you expect!                #\n\n\
         let garden = [\n\
         \"fern orchid fern cactus\",\n\
         \"orchid starfern fern orchid\",\n\
         \"cactus fern orchid orchid\"\n\
         ] in\n\n\
         # Count how many times plant appears in a row string #\n\
         let count_in_row: (String, String) -> Int =\n\
         fun (row, plant) ->\n\
         ?\n\n\n\n\
         in\n\n\
         # Count a plant across all garden rows #\n\
         let count_in_garden: ([String], String) -> Int =\n\
         fun (garden, plant) ->\n\
         ?\n\n\n\n\
         in\n\n\
         test count_in_row(\"fern orchid fern cactus\", \"fern\") == 2 end;\n\n\
         test count_in_row(\"orchid starfern fern orchid\", \"fern\") == 1 \
         end;\n\n\
         test count_in_garden(garden, \"fern\") == 4 end;\n\n\
         test count_in_garden(garden, \"orchid\") == 5 end;\n\n\
         test count_in_garden(garden, \"cactus\") == 2 end\n";
      refractors = "()";
    } )
