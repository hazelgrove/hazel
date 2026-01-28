let out : string * Haz3lcore.PersistentSegment.t =
  ( "Examples / study-write / safe-head / safe-head-sketch",
    {
      segment =
        "((Secondary((id \
         42403360-9a73-47b7-8faf-b08c087350b4)(content(Comment\"# SAFE HEAD \
         TASK                               #\"))))(Secondary((id \
         50817a20-a4cc-479f-929c-1ece24c90e53)(content(Whitespace\"\\n\"))))(Secondary((id \
         fb9ed0eb-2c75-45e4-b774-95a4d2a88013)(content(Comment\"#                                              \
         #\"))))(Secondary((id \
         6e559717-7096-43d9-bf91-a70f9932045b)(content(Whitespace\"\\n\"))))(Secondary((id \
         14f70de8-ba07-4c56-a6df-0ac61cca0f4c)(content(Comment\"# Implement \
         safe_head: get the first element   #\"))))(Secondary((id \
         1b3e85fe-416b-43de-b7c9-26c4fb856ac2)(content(Whitespace\"\\n\"))))(Secondary((id \
         c721f9a1-0566-4bb7-a764-e6c76915939e)(content(Comment\"# of a list, \
         or return a default if empty.     #\"))))(Secondary((id \
         55f51042-86c4-42bb-901d-da20bbbe49e2)(content(Whitespace\"\\n\"))))(Secondary((id \
         305ec79f-d451-4276-aa01-213477ae3620)(content(Comment\"#                                              \
         #\"))))(Secondary((id \
         e1b7be85-cc4a-4cfa-a3e5-d31bbc706e4f)(content(Whitespace\"\\n\"))))(Secondary((id \
         ca9f1e87-6fcb-4c51-bb26-074d8ab38686)(content(Comment\"# \
         Examples:                                    #\"))))(Secondary((id \
         1981b68e-a67f-459a-97dd-b4d076c13762)(content(Whitespace\"\\n\"))))(Secondary((id \
         0124e1ed-cd05-4784-a12e-5c74631dd4af)(content(Comment\"#   \
         safe_head([1, 2, 3], 0) == 1               #\"))))(Secondary((id \
         171d13c9-48cb-421f-8116-8282dd788c08)(content(Whitespace\"\\n\"))))(Secondary((id \
         55e651e9-559f-403e-b9d4-9b3150fa8446)(content(Comment\"#   \
         safe_head([], 99) == 99                    #\"))))(Secondary((id \
         79fc1d7d-b928-43e6-8253-23d2ebd40d1f)(content(Whitespace\"\\n\"))))(Secondary((id \
         b43238e7-7f19-4935-8460-8a60522337f8)(content(Comment\"#                                              \
         #\"))))(Secondary((id \
         b6b874f8-fed6-4059-9687-bcc480d0fa45)(content(Whitespace\"\\n\"))))(Secondary((id \
         0e4a50b4-c982-4817-98e6-cc23ab3f1f04)(content(Comment\"# Available \
         syntax:                            #\"))))(Secondary((id \
         243eef47-32be-47d0-9f63-728f7f8c7170)(content(Whitespace\"\\n\"))))(Secondary((id \
         a0d33115-1ca9-4f67-9369-9a8009145a28)(content(Comment\"#   case \
         expr                                  #\"))))(Secondary((id \
         18dd9700-2377-453e-8249-a1c42cc49855)(content(Whitespace\"\\n\"))))(Secondary((id \
         3cf4ebe8-c90d-4905-9869-349a1aa53d0b)(content(Comment\"#   | pattern1 \
         => result1                      #\"))))(Secondary((id \
         36615385-e345-4f02-b2f1-ae06fad9e97d)(content(Whitespace\"\\n\"))))(Secondary((id \
         f7001c05-509e-447c-a353-323e42f0eb6b)(content(Comment\"#   | pattern2 \
         => result2                      #\"))))(Secondary((id \
         de5d967e-9d7a-4e76-b590-899acc71ea47)(content(Whitespace\"\\n\"))))(Secondary((id \
         5f88363a-1423-4ca9-8e43-28e820eb65f0)(content(Comment\"#   \
         end                                        #\"))))(Secondary((id \
         6e93c8a7-30bc-43e0-933f-7ccc35ced1b7)(content(Whitespace\"\\n\"))))(Secondary((id \
         640510e4-6128-4873-a0c0-3b84bce0f272)(content(Comment\"#                                              \
         #\"))))(Secondary((id \
         36342429-deaa-48b8-9f64-f58b1eb25e2b)(content(Whitespace\"\\n\"))))(Secondary((id \
         5c6857c0-ed08-4fab-a0be-129353a7109b)(content(Comment\"#   List \
         patterns: [], x::xs, [a, b, c]        #\"))))(Secondary((id \
         0fecc530-cda2-4d3b-9a04-a4c497ef1ea9)(content(Whitespace\"\\n\"))))(Secondary((id \
         eaf40845-c712-4abe-8db5-0d5ef6a3c4d1)(content(Comment\"#                                              \
         #\"))))(Secondary((id \
         a3c73429-238f-4ba7-a989-1eee6cd57d8e)(content(Whitespace\"\\n\"))))(Secondary((id \
         90acb9cf-4192-4378-8c25-80ddcffbcc31)(content(Comment\"# Tip: Turn on \
         auto-probe to see which branch  #\"))))(Secondary((id \
         21de6a50-5602-45ad-8eec-c8a29607317e)(content(Whitespace\"\\n\"))))(Secondary((id \
         423d250f-9b0f-4e61-8ff9-734587df2709)(content(Comment\"# is taken for \
         each test case.                 #\"))))(Secondary((id \
         eb0c6b0b-410c-43fe-aa5d-23154f4074ac)(content(Whitespace\"\\n\"))))(Secondary((id \
         4a70ae81-c7b2-4f3c-a2c2-a5b0accce528)(content(Whitespace\"\\n\"))))(Tile((id \
         77cba3a2-8e82-4fff-8d82-4e372f79e768)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         40))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         32a0931b-2cde-4dda-97cc-725a221598cc)(content(Whitespace\" \
         \"))))(Tile((id \
         5df2ed1d-3f61-4066-8384-6322d6a7bb37)(label(safe_head))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         eee26434-b3c0-428d-99f1-055702f9798b)(content(Whitespace\" \
         \")))))((Secondary((id \
         60886a1b-ec02-4c7a-8f6c-016efcaf01b6)(content(Whitespace\" \
         \"))))(Tile((id 3a30aa13-8aaa-4650-9b5f-bd746d610653)(label(fun \
         ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 36))(sort Exp))))))(shards(0 \
         1))(children(((Secondary((id \
         9df2e4ed-bb61-4808-add8-c31f23462c55)(content(Whitespace\" \
         \"))))(Tile((id \
         fcb1eb0f-166e-4d44-a866-b2f394c9fc44)(label(\"(\"\")\"))(mold((out \
         Pat)(in_(Pat))(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0 1))(children(((Tile((id \
         488f8db9-d5f0-4584-9ca0-decd4250c4dd)(label(xs))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         cb2255ce-717c-47cc-a7c6-260cd2ad8f9b)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 47))(sort Pat))((shape(Concave \
         47))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         8d8bb94e-d83d-43bc-bb20-7c48aa24bde0)(content(Whitespace\" \
         \"))))(Tile((id \
         557d23b2-3026-4106-bf61-2ba4c8df239f)(label(default))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children()))))))))(Secondary((id \
         14882cdb-6c5d-4374-9d18-435ffa0eed93)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         acc92edd-62d8-42fd-bedb-473023e7b8a4)(content(Whitespace\"\\n\"))))(Tile((id \
         0f63dfd8-abcc-4cb2-ae18-4e9164d5975f)(label(?))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         1ca5b736-5fc6-49fd-9b77-32d82ceffaeb)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         85a162e4-562b-44c8-a6f8-32164c48f1eb)(content(Whitespace\"\\n\"))))(Secondary((id \
         7f79b4a9-4bf7-4310-937d-d8720af1b30d)(content(Whitespace\"\\n\"))))(Tile((id \
         9ae45e07-7215-4646-8b13-a404719654ff)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         66599e53-ad07-4963-b8e0-e7ffe1d9ce63)(content(Whitespace\"\\n\"))))(Tile((id \
         15a4888f-b70f-4b9b-9f5b-59d5769180ad)(label(safe_head))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         4572e9c6-5726-43a9-a769-115b130022de)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         fa4cdc11-5468-4fd3-bea0-93902dfab990)(label([ ]))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         d6adcdb9-ae7d-4fb8-88d9-741e43dc2bf6)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         6d6e0d53-2da4-4eca-9f52-72a1cca0ffd2)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         c845018e-6275-42d5-bf4c-2eedb9871a8f)(content(Whitespace\" \
         \"))))(Tile((id \
         20888c73-563d-41e9-8327-ea9a603bfcd2)(label(2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         c3cea329-6f30-42cd-966a-17a0238a7c42)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         538e8d56-a5d1-4177-a785-9fe154c16bf0)(content(Whitespace\" \
         \"))))(Tile((id \
         4ee962ae-e287-492c-b0c3-0c85372614f7)(label(3))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         fe35d65d-d2f7-4e97-9d21-40102e015380)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         7b280b3c-3154-4141-bf4d-276e25cbbdbb)(content(Whitespace\" \
         \"))))(Tile((id \
         cb33a538-8bca-4005-a96f-fb07992045bb)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         bb8cc36d-beae-4f74-b23b-385405f926c7)(content(Whitespace\"\\n\"))))(Tile((id \
         40ca32fe-d433-416e-928d-fd7ebaf36172)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         8bd7f19a-80d5-43b1-8c7d-e87b1858ba75)(content(Whitespace\" \
         \"))))(Tile((id \
         d2d81a98-25bd-4ab0-89de-a80a03bc99b2)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         df4a8a69-656e-4fdf-b4ee-1c8c8a1b6c0e)(content(Whitespace\"\\n\")))))))))(Tile((id \
         a044dc5d-655f-4f00-ac28-1f23afd9bd41)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 38))(sort Exp))((shape(Concave \
         38))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         fe9f0519-9262-4713-b7aa-950f0435355b)(content(Whitespace\"\\n\"))))(Secondary((id \
         f141e6fd-d9df-4aac-ae4e-33f668272c55)(content(Whitespace\"\\n\"))))(Tile((id \
         7e3aac77-2d4a-468c-a449-3bd2aac2b63e)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         4d3e8969-87d8-40c5-b0bb-1a82772972ae)(content(Whitespace\"\\n\"))))(Tile((id \
         a582a814-762d-4fc1-bb41-4879f9ea4230)(label(safe_head))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         bfad7164-f970-4ea6-b6cd-6e91745a9c18)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         5c394178-a90f-48a7-8894-dedc99105b7f)(label([ ]))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         cd9188cb-f3d3-4d44-be9f-df3372f43431)(label(42))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         d1cbac58-1224-4ff1-bc51-4098f1553e6f)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         cc06469a-34a7-4e52-885d-71e22e56ca0f)(content(Whitespace\" \
         \"))))(Tile((id \
         69609700-25f4-4c3b-b411-7e3102b82b27)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         ac594323-874e-4534-a5f7-8bc7cb610b5d)(content(Whitespace\"\\n\"))))(Tile((id \
         948d668d-2999-402d-80d2-60f07a82cf10)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         9108cbbb-9ea9-46aa-aa9d-6b5f921b79dd)(content(Whitespace\" \
         \"))))(Tile((id \
         615c95a5-69e3-4298-abf0-4acc43bea2c8)(label(42))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         399e2c16-bf49-4c65-896e-c09ea32ff14e)(content(Whitespace\"\\n\")))))))))(Tile((id \
         2df7b4ad-873a-4540-ae4e-254395e56991)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 38))(sort Exp))((shape(Concave \
         38))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         976313cd-edcd-4737-ac95-37ad7e03bbdf)(content(Whitespace\"\\n\"))))(Secondary((id \
         b9046ed0-e56e-46c4-acfe-ebcac68e28f4)(content(Whitespace\"\\n\"))))(Tile((id \
         4d7e3f2c-c840-4634-8a6a-687abbe42835)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         07e737b4-22e2-4e7b-a741-bda747715e52)(content(Whitespace\"\\n\"))))(Tile((id \
         44c3ebf3-6a0a-42ed-9c98-0097cf876b3b)(label(safe_head))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         34d95793-b3d2-422b-af8d-0a8d6796bbbe)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         d92be47e-af1e-48c1-afed-00e1b186b1cf)(label([]))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         2f1d0fd9-48d7-4840-99e4-4a6c33a77103)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         3ba7c154-a174-4bb2-8bbe-2de40ceb08f9)(content(Whitespace\" \
         \"))))(Tile((id \
         b6f55adb-05c6-4dd2-af53-62fe311aef84)(label(99))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         711cd1b4-cb66-4e8c-96da-6cde1339f4b5)(content(Whitespace\"\\n\"))))(Tile((id \
         912d71e0-05e8-4a4c-8932-09bbb60a9a6f)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         ede65358-1d87-4010-9638-da54386dd3b3)(content(Whitespace\" \
         \"))))(Tile((id \
         1a6c1916-e7ee-461c-bd32-a069dd9b441c)(label(99))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         93fe7007-741a-42a0-bf1b-4f42288c9237)(content(Whitespace\"\\n\")))))))))(Tile((id \
         6af0e5ab-a1af-4cbc-b9a7-6c1af244a497)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 38))(sort Exp))((shape(Concave \
         38))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         ffe29b6c-b9fb-4ca8-8d3f-186cc2b4b742)(content(Whitespace\"\\n\"))))(Secondary((id \
         73aed51a-0289-4425-b7bb-8774d2688b2a)(content(Whitespace\"\\n\"))))(Tile((id \
         10682802-a037-4efe-9645-ecfb41c9b0aa)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         bf0d814e-441e-4eb1-bd4a-54712147802b)(content(Whitespace\"\\n\"))))(Tile((id \
         d522fe05-f146-4ab9-ae59-399a1f2626f1)(label(safe_head))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         ff7f203e-9509-4546-8b5d-09a39702345c)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         3444f75a-a347-43e6-b929-6dcb5579cfab)(label([]))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         34945123-8fe4-4dfc-b62e-b0824fa84f6e)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         4ad2b0d7-b703-4c55-abe2-b4fa6b000fe6)(content(Whitespace\" \
         \"))))(Tile((id \
         bbe32f38-3875-4da4-8a42-bdd166fd80ea)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         2aab9c17-b676-45b9-b2ff-fe4512d41fe2)(content(Whitespace\"\\n\"))))(Tile((id \
         f15ceb00-15d6-4755-9dc3-39df892f2743)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         d50bbf04-7215-4166-9851-78e20ce67f0c)(content(Whitespace\" \
         \"))))(Tile((id \
         83ed971a-131b-43fc-854e-ac8a0707ea22)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         5adb89b0-06a1-413f-81b9-1295256a52a7)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         ee29de54-13b0-4ecf-a463-645820f03e16)(content(Whitespace\"\\n\")))))";
      backup_text =
        "# SAFE HEAD TASK                               #\n\
         #                                              #\n\
         # Implement safe_head: get the first element   #\n\
         # of a list, or return a default if empty.     #\n\
         #                                              #\n\
         # Examples:                                    #\n\
         #   safe_head([1, 2, 3], 0) == 1               #\n\
         #   safe_head([], 99) == 99                    #\n\
         #                                              #\n\
         # Available syntax:                            #\n\
         #   case expr                                  #\n\
         #   | pattern1 => result1                      #\n\
         #   | pattern2 => result2                      #\n\
         #   end                                        #\n\
         #                                              #\n\
         #   List patterns: [], x::xs, [a, b, c]        #\n\
         #                                              #\n\
         # Tip: Turn on auto-probe to see which branch  #\n\
         # is taken for each test case.                 #\n\n\
         let safe_head = fun (xs, default) ->\n\
         ?\n\
         in\n\n\
         test\n\
         safe_head([1, 2, 3], 0)\n\
         == 1\n\
         end;\n\n\
         test\n\
         safe_head([42], 0)\n\
         == 42\n\
         end;\n\n\
         test\n\
         safe_head([], 99)\n\
         == 99\n\
         end;\n\n\
         test\n\
         safe_head([], 0)\n\
         == 0\n\
         end\n";
      refractors = "()";
    } )
