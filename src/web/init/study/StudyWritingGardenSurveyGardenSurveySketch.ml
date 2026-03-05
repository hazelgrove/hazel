let out : string * Haz3lcore.PersistentSegment.t =
  ( "Study / writing / garden-survey / garden-survey-sketch",
    {
      segment =
        "((Secondary((id \
         7b961e39-d7ca-419a-a26a-889b54228ec7)(content(Comment\"# Garden \
         Survey Notes                                  #\"))))(Secondary((id \
         00459c59-dffd-4fd7-80c6-83f601c7f003)(content(Whitespace\"\\n\"))))(Secondary((id \
         1bd6286c-db6d-4db5-b893-83192214471f)(content(Comment\"#                                                      \
         #\"))))(Secondary((id \
         544ade61-cee5-4f12-8634-90be116fd651)(content(Whitespace\"\\n\"))))(Secondary((id \
         683c6f5e-97e6-46b2-8e56-f930e3540185)(content(Comment\"# After each \
         moonlit garden walk, visitors fill out    #\"))))(Secondary((id \
         0c533744-bfaa-4a67-923b-d0a424135847)(content(Whitespace\"\\n\"))))(Secondary((id \
         0b4082ab-5339-4f29-9867-b88ee70b3bc1)(content(Comment\"# a short \
         survey. Entries look like:                   #\"))))(Secondary((id \
         c1832223-9147-4d49-b242-b116f6f060b2)(content(Whitespace\"\\n\"))))(Secondary((id \
         293f4c43-5470-413b-b5d5-b12b211a86e7)(content(Comment\"#   \\\"Q1: \
         yes -- the moonbloom beds were stunning\\\"      \
         #\"))))(Secondary((id \
         cd3a2c42-7fb2-44cf-b7a9-833519dcf762)(content(Whitespace\"\\n\"))))(Secondary((id \
         3c568984-7856-4708-9ebb-5f4e2717e657)(content(Comment\"#   \\\"Q2: no \
         -- too many weeds near the starfern\\\"       #\"))))(Secondary((id \
         3db9bfad-513d-4549-85e0-1fdf2846a9f0)(content(Whitespace\"\\n\"))))(Secondary((id \
         1a84ae1f-8079-4f9c-9f14-165cdaafc1f4)(content(Comment\"#                                                      \
         #\"))))(Secondary((id \
         84cbaff6-8213-48a9-afcb-7eee5334088f)(content(Whitespace\"\\n\"))))(Secondary((id \
         1aad5233-2741-4dd8-bb7d-8bd16d908042)(content(Comment\"# Extract just \
         the notes from positive responses.      #\"))))(Secondary((id \
         4da1f43f-4a33-42cf-9770-b351956e0c1a)(content(Whitespace\"\\n\"))))(Secondary((id \
         ee424bbd-758a-4d6f-baba-632cf171e4de)(content(Comment\"# For the data \
         below, the result should be:            #\"))))(Secondary((id \
         60c01aed-ca7d-4c13-bd1c-cffaf70ed305)(content(Whitespace\"\\n\"))))(Secondary((id \
         fb5a818e-1783-419a-8048-adb916aa66ab)(content(Comment\"#   [\\\"the \
         moonbloom beds were stunning\\\",               #\"))))(Secondary((id \
         760cafcd-8fa7-4009-9275-0c77fe937598)(content(Whitespace\"\\n\"))))(Secondary((id \
         9062b216-2f5e-4e7f-9981-02b0b19d302d)(content(Comment\"#    \\\"loved \
         the new duskrose pathway\\\",                 #\"))))(Secondary((id \
         dc2cbc8d-0931-4ab0-905c-9197592af364)(content(Whitespace\"\\n\"))))(Secondary((id \
         d4424b6a-8c02-4729-aecf-22b1ec44b0b9)(content(Comment\"#    \\\"the \
         nightshade corner was magical\\\"]              #\"))))(Secondary((id \
         bc2f2e52-c78d-4a18-babe-0a566eebcb4a)(content(Whitespace\"\\n\"))))(Secondary((id \
         26380bf4-148b-4cac-8f3c-cdb7542dd9ec)(content(Comment\"#                                                      \
         #\"))))(Secondary((id \
         d70e3d93-23f8-4b38-822d-296c4ee85646)(content(Whitespace\"\\n\"))))(Secondary((id \
         09523939-b5d8-4ed2-ac22-ae5fc7a1c978)(content(Comment\"# \
         Steps:                                               \
         #\"))))(Secondary((id \
         753d3819-1de9-4154-b7ea-9a6cf5b4441b)(content(Whitespace\"\\n\"))))(Secondary((id \
         1b41432d-5f8f-4ddc-81db-0029f1136b15)(content(Comment\"#   1. \
         is_positive: check if a response is \\\"yes\\\"       \
         #\"))))(Secondary((id \
         5e133c3d-3876-470a-a63a-37c860770a21)(content(Whitespace\"\\n\"))))(Secondary((id \
         c0ccd967-5ca9-4b44-9ad3-4add0794f6e2)(content(Comment\"#   2. \
         extract_note: get the text after the --         #\"))))(Secondary((id \
         23b102a9-6364-4719-89b2-2b6f1045d468)(content(Whitespace\"\\n\"))))(Secondary((id \
         3587eea0-b623-4079-8844-7fc26274d56c)(content(Comment\"#   3. \
         positive_notes: filter then extract             #\"))))(Secondary((id \
         e76f30be-058a-49fe-8584-d5e4d9bbdc36)(content(Whitespace\"\\n\"))))(Secondary((id \
         7089e7fb-f1e9-4816-a23f-b8290de710f4)(content(Comment\"#                                                      \
         #\"))))(Secondary((id \
         460b9609-6db4-4bcf-84df-5fad58c329da)(content(Whitespace\"\\n\"))))(Secondary((id \
         370082fb-e430-47b4-81fc-21f48f6a0126)(content(Comment\"# Available \
         functions:                                 #\"))))(Secondary((id \
         94455007-c7f4-4ab2-81c2-b94e81247dc5)(content(Whitespace\"\\n\"))))(Secondary((id \
         e81d8a46-9d1c-4959-bb33-167764971b53)(content(Comment\"#   \
         string_match: (String, String) -> Bool             \
         #\"))))(Secondary((id \
         8ec7e162-1bc3-4e9b-8b37-e76fb011ce89)(content(Whitespace\"\\n\"))))(Secondary((id \
         00b917f3-02c6-423e-a291-fff67d7df7d9)(content(Comment\"#   \
         string_split: (String, String) -> [String]         \
         #\"))))(Secondary((id \
         b8f263ed-482f-4556-ba9e-e5d0b30aa33d)(content(Whitespace\"\\n\"))))(Secondary((id \
         6d8289b4-4efd-4b91-8e9a-de4ce5fc4092)(content(Comment\"#   \
         string_trim: String -> String                      \
         #\"))))(Secondary((id \
         89a1f86c-c9e0-48c7-aa9f-672c4cd60bf1)(content(Whitespace\"\\n\"))))(Secondary((id \
         27a0722d-0467-46fa-bb05-e4e0d96a326b)(content(Comment\"#   nth: ([?], \
         Int) -> ?                               #\"))))(Secondary((id \
         02a4ca18-cd86-4960-a3b3-190335395ae4)(content(Whitespace\"\\n\"))))(Secondary((id \
         3147934e-e2ae-4d73-a5e0-792097feee42)(content(Comment\"#   filter: \
         ([?], ? -> Bool) -> [?]                    #\"))))(Secondary((id \
         25198be8-6fd4-424c-bc2d-e277df4006da)(content(Whitespace\"\\n\"))))(Secondary((id \
         d8b71bb9-250e-4dfb-b3ae-0da3a17c6a4d)(content(Comment\"#   map: ([?], \
         ? -> ?) -> [?]                          #\"))))(Secondary((id \
         4a6030b7-f5d3-4325-bd6e-53230a4f716e)(content(Whitespace\"\\n\"))))(Secondary((id \
         f5871679-3d01-4244-86ed-29473860e374)(content(Comment\"#                                                      \
         #\"))))(Secondary((id \
         45ebd7b8-62ed-4623-b37d-f8b6d9f6eeb2)(content(Whitespace\"\\n\"))))(Secondary((id \
         da852a06-5248-45f4-98d3-fc6fee692902)(content(Comment\"# string_match \
         checks whether a regex pattern          #\"))))(Secondary((id \
         29a0c3b1-312c-4c6d-8f1b-4008e08493ed)(content(Whitespace\"\\n\"))))(Secondary((id \
         f1584437-bc5c-4028-ab34-30ee8f847aa3)(content(Comment\"# appears \
         anywhere in the string. Anywhere!            #\"))))(Secondary((id \
         a85d9c36-382d-406f-9afc-08f0f4c3ecf7)(content(Whitespace\"\\n\"))))(Secondary((id \
         0c40bcd7-e6ab-4d3a-bb21-5aa77188a953)(content(Comment\"#                                                      \
         #\"))))(Secondary((id \
         4a4014a4-4ce9-4d25-adb3-28287897c290)(content(Whitespace\"\\n\"))))(Secondary((id \
         3f2fc0c7-9fc3-4699-8132-14000c79fa61)(content(Comment\"# Tip: Probe \
         each function with the test data          #\"))))(Secondary((id \
         6f8ddee2-927d-438b-ad51-a7bb4aac1f51)(content(Whitespace\"\\n\"))))(Secondary((id \
         c34911f5-8c26-4d97-ba3c-4603f06f365f)(content(Comment\"# before \
         combining them. Regex substring matching      #\"))))(Secondary((id \
         364a0f84-7a05-4536-b72e-ecffbbc82a20)(content(Whitespace\"\\n\"))))(Secondary((id \
         21ac2826-8a2c-404d-a0f2-1b546f6c58e4)(content(Comment\"# can be \
         surprising -- check carefully!                #\"))))(Secondary((id \
         8066382a-0258-478d-b1e8-8001d084d36b)(content(Whitespace\"\\n\"))))(Secondary((id \
         10c29208-bf59-4e40-a911-75c95d392694)(content(Whitespace\"\\n\"))))(Tile((id \
         1ea621c2-fc5b-45e0-8f10-a7fcde8a133f)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         6c074db5-4538-45b0-9112-e38602e9d322)(content(Whitespace\" \
         \"))))(Tile((id \
         085fdf86-087a-4b81-a53a-87f41d1bb5ac)(label(entries))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         dc46ff5a-04b0-4bc8-9b35-fe074bdb933c)(content(Whitespace\" \
         \")))))((Secondary((id \
         e84a6756-07ad-4825-8b9b-53be87d51ea7)(content(Whitespace\" \
         \"))))(Tile((id 5f6bbe33-0693-41ef-80af-d6b4f744b9d6)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         766b186e-884d-4a81-9fa2-ee7fcd3120dd)(content(Whitespace\"\\n\"))))(Tile((id \
         4c08b69a-6873-41e8-9d40-ea169a1b7ad7)(label(\"\\\"Q1: yes -- the \
         moonbloom beds were stunning\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         5d525981-8af6-4749-97ba-4459ea9b4c44)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         802ca778-bb0c-4c9d-a25c-5e464112f895)(content(Whitespace\"\\n\"))))(Tile((id \
         9b3ab405-de25-4729-a2e0-34fdbe9ad644)(label(\"\\\"Q2: no -- too many \
         weeds near the starfern\\\"\"))(mold((out Exp)(in_())(nibs(((shape \
         Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         580cf912-d0a6-4e5e-bdce-35b625582727)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         e0727520-8dc3-4343-950d-a6b39c555b8a)(content(Whitespace\"\\n\"))))(Tile((id \
         069067dc-8a7f-49cf-b8c7-3840dd00a6be)(label(\"\\\"Q3: yes -- loved \
         the new duskrose pathway\\\"\"))(mold((out Exp)(in_())(nibs(((shape \
         Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         81a2f558-bed3-435a-b09d-6d27df35e813)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         c6b15a10-4aed-4760-84b3-afbbffdddb84)(content(Whitespace\"\\n\"))))(Tile((id \
         78f52293-f508-4154-81fc-b15bbdd8ccb9)(label(\"\\\"Q4: no -- \
         yesterday's rain left puddles\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         9a18fc16-33b7-4181-b732-db7574207c5d)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         510ba155-9afc-4276-a87b-a3ef0253e9e4)(content(Whitespace\"\\n\"))))(Tile((id \
         2cfde39c-0949-4792-9203-41bb1ed7c9f5)(label(\"\\\"Q5: yes -- the \
         nightshade corner was magical\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         7a8bcd72-5e05-4833-aa70-57da2980b7ef)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         dc52dd38-ceb2-4209-9e90-4a19236e4f2a)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         2a90712b-4d61-4e4c-8dea-c86c5a0a33c8)(content(Whitespace\"\\n\"))))(Secondary((id \
         be1b64f1-20c8-42ea-9cd2-090a9f964d04)(content(Whitespace\"\\n\"))))(Secondary((id \
         7146f310-7ad8-498d-bf06-6eade438eb59)(content(Comment\"# Does this \
         survey entry have a positive response? #\"))))(Secondary((id \
         9dca4b19-aa65-4514-8cec-0653743e2c62)(content(Whitespace\"\\n\"))))(Tile((id \
         f1c6321f-f0ed-4cf1-bb42-d9bb35f891b8)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         29088381-d3df-41cc-b73d-603837aad283)(content(Whitespace\" \
         \"))))(Tile((id \
         da316f92-f31b-4687-b36c-55afbecf2bb4)(label(is_positive))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         6a2d5074-e487-45b6-98aa-b743ac0ed861)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 24))(sort Pat))((shape(Concave \
         24))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         8ea5950e-c018-4bcb-a67d-efd60cce3ad0)(content(Whitespace\" \
         \"))))(Tile((id \
         7d72aa1b-1f21-41b5-97e3-0617546375d6)(label(String))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         d7f60d8b-0ac4-42b7-80c7-40f62d4cd304)(content(Whitespace\" \
         \"))))(Tile((id \
         754a937e-52d0-4ff9-88e7-ff07b896fbcd)(label(->))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 13))(sort Typ))((shape(Concave \
         13))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         2ce2e3de-b94e-4a90-903f-6fe7b467ede9)(content(Whitespace\" \
         \"))))(Tile((id \
         8e5ce887-96ad-46d6-8c9f-230c2cf647f3)(label(Bool))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         4807ddc4-ae88-4813-9481-98f00559d83a)(content(Whitespace\" \
         \")))))((Secondary((id \
         6bbe21bd-7346-4893-920c-1b383e88e2cf)(content(Whitespace\" \
         \"))))(Tile((id 764a2825-3666-423e-b6a3-493c664c6238)(label(fun \
         ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 37))(sort Exp))))))(shards(0 \
         1))(children(((Secondary((id \
         52df808b-6b8c-45b1-921a-32008f732dd6)(content(Whitespace\" \
         \"))))(Tile((id \
         1502b43e-a5c1-4664-b58d-85b93f955fd5)(label(entry))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         83e12c18-0292-4623-a6d7-80e390bf613e)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         25cc3fab-b8b3-44c9-9c4f-c7350e37ac75)(content(Whitespace\"\\n\"))))(Tile((id \
         dd043b07-617c-44c4-95ca-4555bcc6d9d9)(label(?))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         1c0dc784-e980-4db3-926c-6430aa7e5f2c)(content(Whitespace\"\\n\"))))(Secondary((id \
         09e67e12-301e-40e6-be52-48bcdc17e0d2)(content(Whitespace\"\\n\"))))(Secondary((id \
         33444724-e80d-4bce-9669-59e5c2812442)(content(Whitespace\"\\n\"))))(Secondary((id \
         a03127a3-fa79-4e76-ba2c-2f0e6e7902dd)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         73da6fb0-a625-4592-ba1d-581520f200c8)(content(Whitespace\"\\n\"))))(Secondary((id \
         692b83e1-2538-454f-9a66-c769a231e1f0)(content(Whitespace\"\\n\"))))(Secondary((id \
         ef2dad6d-cb40-4013-a0b8-57fa07234df8)(content(Comment\"# Extract the \
         note text from a survey entry #\"))))(Secondary((id \
         7af6da60-92a6-43fc-b0c8-c14b598d5422)(content(Whitespace\"\\n\"))))(Tile((id \
         5806e6bf-4492-4dcb-8b2d-dc8d28b165ea)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         ce8c15fe-3da6-418f-b715-47ff51381aca)(content(Whitespace\" \
         \"))))(Tile((id \
         7bbe472a-979c-4cf4-a9b1-bcd153598da2)(label(extract_note))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         a1a4be94-4388-4abf-86d0-fb90400b5d3a)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 24))(sort Pat))((shape(Concave \
         24))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         145832f0-757a-4618-9683-ccae191aa749)(content(Whitespace\" \
         \"))))(Tile((id \
         58e11f4d-ecee-4097-8ce3-b1bee887eed5)(label(String))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         e562e5d5-b37f-46ce-a43f-6fdf9663a6ef)(content(Whitespace\" \
         \"))))(Tile((id \
         cc53781b-3452-4ced-8135-19020e42aae9)(label(->))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 13))(sort Typ))((shape(Concave \
         13))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         9c14a6b0-f832-45d7-bafc-c614b6d23b2c)(content(Whitespace\" \
         \"))))(Tile((id \
         ff0a7878-5643-45ed-85bf-f2c95e43dc19)(label(String))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         6d6fcb43-39d3-48d0-bca1-38dd546cb732)(content(Whitespace\" \
         \")))))((Secondary((id \
         3742b6da-0ede-4843-a6fd-b841a6e270fb)(content(Whitespace\" \
         \"))))(Tile((id f2fc3c29-2300-4c43-8b6a-2d3cc5898e64)(label(fun \
         ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 37))(sort Exp))))))(shards(0 \
         1))(children(((Secondary((id \
         66ddc272-03f1-4eb1-b86c-5c2f989feb75)(content(Whitespace\" \
         \"))))(Tile((id \
         e75739e2-2af6-4f6a-bf5f-2c6185353ee0)(label(entry))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         76eb53a4-6443-4329-b9cc-5ff282cdca55)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         109e333d-584b-4141-a4b4-15291e9c3a7c)(content(Whitespace\"\\n\"))))(Tile((id \
         198c90b2-a314-4faa-8414-c91d0345349c)(label(?))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         63a8bcf9-eaba-412f-b9e2-6be94375fcfb)(content(Whitespace\"\\n\"))))(Secondary((id \
         325f8bca-2ad9-4592-a8e1-00ceca538803)(content(Whitespace\"\\n\"))))(Secondary((id \
         1b434492-fd87-4673-bb5d-afc5c1ba528f)(content(Whitespace\"\\n\"))))(Secondary((id \
         a6c05a12-e447-4586-86d5-50642ab61920)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         26ac0392-95a3-406e-b9be-de1b17d44082)(content(Whitespace\"\\n\"))))(Secondary((id \
         688b625a-73e0-4071-88b2-c7ea346b3368)(content(Whitespace\"\\n\"))))(Secondary((id \
         851033ac-b022-442f-bc45-131b349cef61)(content(Comment\"# Get only the \
         notes from positive responses #\"))))(Secondary((id \
         aef3a503-3f02-4c5d-843f-461d0fa41695)(content(Whitespace\"\\n\"))))(Tile((id \
         a62f1a7b-90ac-427f-9ef8-45b6fff703b4)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         f1c1a0b4-be99-48c2-a8dd-a50f1a1904e4)(content(Whitespace\" \
         \"))))(Tile((id \
         9cbc4cde-0471-4385-9d0a-ffc59a7a699c)(label(positive_notes))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         d5dc5e9f-0b2a-4707-b9f7-383ace98f9e8)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 24))(sort Pat))((shape(Concave \
         24))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         6dadc022-69b0-4c17-95d7-a409901c0e3d)(content(Whitespace\" \
         \"))))(Tile((id 9497ae45-14fd-4300-a433-f5f182bcc8de)(label([ \
         ]))(mold((out Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape \
         Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
         61ac3c70-d6e7-4320-9776-57b73c08bff1)(label(String))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children()))))))))(Secondary((id \
         4fa0aef6-98e3-40a6-87d1-f0083b183392)(content(Whitespace\" \
         \"))))(Tile((id \
         e092b377-09f8-4800-863b-af2c56f1f7be)(label(->))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 13))(sort Typ))((shape(Concave \
         13))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         c6cea65f-8b47-49f3-98e9-ebb5b2c9362d)(content(Whitespace\" \
         \"))))(Tile((id 29bba817-e923-43bb-b8bd-3ed0241410b1)(label([ \
         ]))(mold((out Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape \
         Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
         b1e6d363-a905-4b1f-834c-f78ba1c8a73f)(label(String))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children()))))))))(Secondary((id \
         13cdfbc0-bc87-48f4-bcd0-4c8d836dbb25)(content(Whitespace\" \
         \")))))((Secondary((id \
         3734eea0-f41e-4994-9adf-e4980e09146d)(content(Whitespace\" \
         \"))))(Tile((id 701c12d1-54a5-4cb5-aef9-8d908bb8266d)(label(fun \
         ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 37))(sort Exp))))))(shards(0 \
         1))(children(((Secondary((id \
         1d515f0f-703d-4a0c-b9dd-b5ac7c3f77bd)(content(Whitespace\" \
         \"))))(Tile((id \
         2601ba3e-ca65-4a80-9b49-2ca9d0939166)(label(entries))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         ab8864f9-3c78-4e7a-aa42-4d9c0a5b24da)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         2d73cd25-caf7-4f06-a9e6-6d7c5cadba0a)(content(Whitespace\"\\n\"))))(Tile((id \
         789c464f-7f01-4484-a8c1-e8a96479fc88)(label(?))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         d62c56b9-4491-4b73-ba12-368b38dea91b)(content(Whitespace\"\\n\"))))(Secondary((id \
         6c43eb82-f82a-4a63-bdf2-59275f951a09)(content(Whitespace\"\\n\"))))(Secondary((id \
         ab9e5424-d7d9-4fa7-84f2-a0d0b81f5bdb)(content(Whitespace\"\\n\"))))(Secondary((id \
         ffd45bfb-dca6-4424-90cd-b375e0c8f54a)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         c6d79a5f-6a9e-4828-b11e-39f4550f4be2)(content(Whitespace\"\\n\"))))(Secondary((id \
         1629169b-7a47-4804-ae0c-40a3f6d6cb6f)(content(Whitespace\"\\n\"))))(Tile((id \
         f14028f8-67aa-4bd7-b723-884bc41ffda7)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         2d5b341f-831f-4207-8f92-3ee48eaa979c)(content(Whitespace\" \
         \"))))(Tile((id \
         d8d1e501-ef79-4e2f-ad90-5435e6bc9ba1)(label(positive_notes))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         3131f80f-a4db-4185-9e75-8979807095e1)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         e5f0d35c-7915-4a56-84ec-fa088de29c96)(label(entries))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         7e7e517c-09c2-4e4a-b840-eec738877b7d)(content(Whitespace\" \
         \"))))(Tile((id \
         98f96563-4b7a-4db9-8316-f770cc2a9e5e)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         1fed062a-b251-4299-b4b8-f4833f202e23)(content(Whitespace\" \
         \"))))(Tile((id 533c39e2-7d7b-4d11-bb26-c9a5cdf95790)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         815cf8cf-445e-48e3-8497-b841773e6b64)(content(Whitespace\"\\n\"))))(Tile((id \
         f958e394-aa1a-47fd-a806-3ed79213ab6b)(label(\"\\\"the moonbloom beds \
         were stunning\\\"\"))(mold((out Exp)(in_())(nibs(((shape Convex)(sort \
         Exp))((shape Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
         af42c9b1-7ae7-44af-ab56-a07115a6a04d)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         557054f4-9220-4169-8487-f46edf29b1a8)(content(Whitespace\"\\n\"))))(Tile((id \
         4d76daff-9ce6-4346-98ed-0430d67d132d)(label(\"\\\"loved the new \
         duskrose pathway\\\"\"))(mold((out Exp)(in_())(nibs(((shape \
         Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         74dd0134-2760-47a2-8bfe-8c413032491e)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         dbfd52db-1daa-4089-bc25-80041f4246e5)(content(Whitespace\"\\n\"))))(Tile((id \
         32fffa97-3152-403c-9429-9989e46f0ab3)(label(\"\\\"the nightshade \
         corner was magical\\\"\"))(mold((out Exp)(in_())(nibs(((shape \
         Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         fc7918e6-fcce-4559-83c0-4ddc152fbb97)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         0997c3d9-1814-4615-aa85-9ae18113b029)(content(Whitespace\" \
         \")))))))))(Tile((id \
         41773053-afd7-44ce-8cf7-f78c9ab08896)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         6bb6c06e-1dce-49e2-a1da-9ec87f7b1c8b)(content(Whitespace\"\\n\"))))(Secondary((id \
         5c83af53-e3c4-4078-998e-eef6b90388b7)(content(Whitespace\"\\n\"))))(Tile((id \
         943b3118-43a3-4ce1-b4d7-6b61f5cb2fba)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         b7808d38-f795-4313-9d00-0e926f01d232)(content(Whitespace\" \
         \"))))(Tile((id \
         9c8aaa11-ecfa-4e27-99de-860d1cda3a1e)(label(is_positive))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         2a868f75-b037-4433-a4fa-c7a770eadf81)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         7e9ce331-4ee5-4027-99b7-32d897376e72)(label(\"\\\"Q1: yes -- \
         moonblooms lovely\\\"\"))(mold((out Exp)(in_())(nibs(((shape \
         Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         2881110f-7c12-46bd-9028-1cf591feefcc)(content(Whitespace\" \
         \"))))(Tile((id \
         77a11e18-12fb-4366-bae6-a67c0fa37ad5)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         2e1577e1-b570-40a0-b09f-d4db58f95f21)(content(Whitespace\" \
         \"))))(Tile((id \
         93e8867a-513d-44fc-afb0-2ce5d79fc1e0)(label(true))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         21bf0443-1a18-4cae-8b87-c9edb386487a)(content(Whitespace\" \
         \")))))))))(Tile((id \
         0f7358e8-e109-40b9-a6ee-8f430da09f0e)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         367549d2-7182-4983-8ab4-eaa652338e33)(content(Whitespace\"\\n\"))))(Secondary((id \
         4b738ed6-aad2-4ae9-a32b-163654a2f928)(content(Whitespace\"\\n\"))))(Tile((id \
         e973eaa3-700d-4a0f-b816-3a599dd0944d)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         96d58e75-5239-489b-995a-a625a1a74ab1)(content(Whitespace\" \
         \"))))(Tile((id \
         e53e196a-1a33-40b2-ad3a-1292066bf2e1)(label(is_positive))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         92153da1-676b-4341-bba5-138a849ceecb)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         64ebbecc-191b-4a51-b501-47ff68cf5a4b)(label(\"\\\"Q2: no -- needs \
         more starfern\\\"\"))(mold((out Exp)(in_())(nibs(((shape Convex)(sort \
         Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         c0c2643b-4569-4dc3-ab36-b28701613afe)(content(Whitespace\" \
         \"))))(Tile((id \
         475af94b-4b72-4695-a289-2bc6cec593a1)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         c7db5ddc-0ba9-4628-9e92-1ab988401a98)(content(Whitespace\" \
         \"))))(Tile((id \
         ac5749e0-6928-45bb-96cc-d8154537caa2)(label(false))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         6683cdb0-b4c5-4703-bfb6-ccfb8cf564ef)(content(Whitespace\" \
         \")))))))))(Tile((id \
         141954fe-7634-42f4-95c7-ade69936cb22)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         9da5555b-1a22-4957-95a0-17edaaf8df48)(content(Whitespace\"\\n\"))))(Secondary((id \
         428a8277-87b7-47f4-a148-001dac74dce3)(content(Whitespace\"\\n\"))))(Tile((id \
         3818dec6-6ce0-43e7-bbf4-0e52d8d80d64)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         e8f4f33a-d263-4de4-92cb-6cc26decf934)(content(Whitespace\" \
         \"))))(Tile((id \
         a4c136ec-ad1f-4e80-ac1d-5793ce7d5d3f)(label(is_positive))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         987f5c88-1275-42a5-837b-f01650ae6fe0)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         70e6bf70-4179-423d-93bd-dd07a0c21a96)(label(\"\\\"Q4: no -- yesterday \
         was rainy\\\"\"))(mold((out Exp)(in_())(nibs(((shape Convex)(sort \
         Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         265d4e53-8ccf-4912-9ca1-54dbe3ce6121)(content(Whitespace\" \
         \"))))(Tile((id \
         e37ab50f-17ee-454b-8f8a-5afbf564c74f)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         6e2027d0-22ac-42a2-81aa-6b62580cbfb9)(content(Whitespace\" \
         \"))))(Tile((id \
         c8e04dc7-bf83-4111-b63f-eefca8fd1b77)(label(false))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         d1d1a2d7-9b21-4b51-aee6-4ee3fe792238)(content(Whitespace\" \
         \")))))))))(Tile((id \
         498354c5-8293-4852-a5a5-cd9de8a02507)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         b28b0dd7-ace5-48a2-9cf2-d2b9207ab8dd)(content(Whitespace\"\\n\"))))(Secondary((id \
         b7b91724-4f90-4117-856f-c41b6743eec8)(content(Whitespace\"\\n\"))))(Tile((id \
         94e7ef5f-3bc0-4c4f-a5b5-0cc8d52818ac)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         2f0a1524-f7ee-4956-aa30-38993b38fa2a)(content(Whitespace\" \
         \"))))(Tile((id \
         99e1359e-be6d-4a56-916c-d5b4990b74f9)(label(extract_note))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         79aca90e-87d6-4116-8afe-436d14b05690)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         08a36304-2cf3-4b31-af71-e0d249671173)(label(\"\\\"Q3: yes -- duskrose \
         pathway\\\"\"))(mold((out Exp)(in_())(nibs(((shape Convex)(sort \
         Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         d42a15f6-0234-41b5-be18-2cffc5ce58f4)(content(Whitespace\"\\n\"))))(Tile((id \
         30939473-e7b3-4d5d-b297-d7a8a677683e)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         ace52afc-d23b-46da-b31d-84614a7cf7c4)(content(Whitespace\" \
         \"))))(Tile((id \
         87a1cdbd-b4ea-490d-847a-25299b61541b)(label(\"\\\"duskrose \
         pathway\\\"\"))(mold((out Exp)(in_())(nibs(((shape Convex)(sort \
         Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         f927bcfe-428c-4b88-aee8-fae90ae2a8ef)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         d352e29b-16e6-48af-9c29-4b528bc50cf5)(content(Whitespace\"\\n\")))))";
      backup_text =
        "# Garden Survey Notes                                  #\n\
         #                                                      #\n\
         # After each moonlit garden walk, visitors fill out    #\n\
         # a short survey. Entries look like:                   #\n\
         #   \"Q1: yes -- the moonbloom beds were stunning\"      #\n\
         #   \"Q2: no -- too many weeds near the starfern\"       #\n\
         #                                                      #\n\
         # Extract just the notes from positive responses.      #\n\
         # For the data below, the result should be:            #\n\
         #   [\"the moonbloom beds were stunning\",               #\n\
         #    \"loved the new duskrose pathway\",                 #\n\
         #    \"the nightshade corner was magical\"]              #\n\
         #                                                      #\n\
         # Steps:                                               #\n\
         #   1. is_positive: check if a response is \"yes\"       #\n\
         #   2. extract_note: get the text after the --         #\n\
         #   3. positive_notes: filter then extract             #\n\
         #                                                      #\n\
         # Available functions:                                 #\n\
         #   string_match: (String, String) -> Bool             #\n\
         #   string_split: (String, String) -> [String]         #\n\
         #   string_trim: String -> String                      #\n\
         #   nth: ([?], Int) -> ?                               #\n\
         #   filter: ([?], ? -> Bool) -> [?]                    #\n\
         #   map: ([?], ? -> ?) -> [?]                          #\n\
         #                                                      #\n\
         # string_match checks whether a regex pattern          #\n\
         # appears anywhere in the string. Anywhere!            #\n\
         #                                                      #\n\
         # Tip: Probe each function with the test data          #\n\
         # before combining them. Regex substring matching      #\n\
         # can be surprising -- check carefully!                #\n\n\
         let entries = [\n\
         \"Q1: yes -- the moonbloom beds were stunning\",\n\
         \"Q2: no -- too many weeds near the starfern\",\n\
         \"Q3: yes -- loved the new duskrose pathway\",\n\
         \"Q4: no -- yesterday's rain left puddles\",\n\
         \"Q5: yes -- the nightshade corner was magical\"\n\
         ] in\n\n\
         # Does this survey entry have a positive response? #\n\
         let is_positive: String -> Bool = fun entry ->\n\
         ?\n\n\n\n\
         in\n\n\
         # Extract the note text from a survey entry #\n\
         let extract_note: String -> String = fun entry ->\n\
         ?\n\n\n\n\
         in\n\n\
         # Get only the notes from positive responses #\n\
         let positive_notes: [String] -> [String] = fun entries ->\n\
         ?\n\n\n\n\
         in\n\n\
         test positive_notes(entries) == [\n\
         \"the moonbloom beds were stunning\",\n\
         \"loved the new duskrose pathway\",\n\
         \"the nightshade corner was magical\"\n\
         ] end;\n\n\
         test is_positive(\"Q1: yes -- moonblooms lovely\") == true end;\n\n\
         test is_positive(\"Q2: no -- needs more starfern\") == false end;\n\n\
         test is_positive(\"Q4: no -- yesterday was rainy\") == false end;\n\n\
         test extract_note(\"Q3: yes -- duskrose pathway\")\n\
         == \"duskrose pathway\" end\n";
      refractors = "()";
    } )
