let out : string * Haz3lcore.PersistentSegment.t =
  ( "Study / writing / crop-plotter-extend / crop-plotter-extend-sketch",
    {
      segment =
        "((Secondary((id \
         ecad264d-1539-49e7-a9d2-93491b4d0c11)(content(Comment\"# CROP PLOTTER \
         EXTENSION TASK                     #\"))))(Secondary((id \
         98f140e0-610e-4e68-926f-e5085dc0684c)(content(Whitespace\"\\n\"))))(Secondary((id \
         4238e833-ac38-4835-89f8-1517f6c212bc)(content(Comment\"#                                                 \
         #\"))))(Secondary((id \
         5440175f-abec-44b3-b27a-f0ebb42a6a5e)(content(Whitespace\"\\n\"))))(Secondary((id \
         6c87e857-a775-4a97-90d8-73dfd8ed0fb6)(content(Comment\"# The crop \
         plotter app lets you plant seeds on    #\"))))(Secondary((id \
         2ec624ae-be14-44ad-b4e7-23c60394c812)(content(Whitespace\"\\n\"))))(Secondary((id \
         d8e7d396-dc7d-4160-a1ff-8a87195bc3d7)(content(Comment\"# a grid. It \
         already supports planting rows.      #\"))))(Secondary((id \
         f8a5c22f-1660-46d2-834f-95b786d50291)(content(Whitespace\"\\n\"))))(Secondary((id \
         13607853-e826-4fcc-9c56-6d6acd8fad4c)(content(Comment\"#                                                 \
         #\"))))(Secondary((id \
         8cc5ffcc-458e-40ba-a2a4-2938d080b1b5)(content(Whitespace\"\\n\"))))(Secondary((id \
         e3197176-fb68-48da-aade-a41d4d1a4c80)(content(Comment\"# YOUR TASK: \
         Add a PlantCol action that fills     #\"))))(Secondary((id \
         3744af7b-bd88-4a1e-82bb-22dcf99f21cb)(content(Whitespace\"\\n\"))))(Secondary((id \
         286f9361-4029-45bf-8249-6bc5fe00ffd6)(content(Comment\"# an entire \
         column with the current seed.         #\"))))(Secondary((id \
         7c249d02-56d2-406b-97b8-0b8e70413bac)(content(Whitespace\"\\n\"))))(Secondary((id \
         c99d493c-ceea-4e8f-a744-a1cbb51bd7be)(content(Comment\"#                                                 \
         #\"))))(Secondary((id \
         527225aa-bd78-474c-82fe-68974eeaa4c8)(content(Whitespace\"\\n\"))))(Secondary((id \
         6527bcbf-84d7-46e9-9afd-3b92906049c4)(content(Comment\"# You need \
         to:                                    #\"))))(Secondary((id \
         c6ea96e2-166f-4613-a7cf-3121f7ea3cae)(content(Whitespace\"\\n\"))))(Secondary((id \
         694dedd2-c3f8-4a0d-94a0-9d42ec3e8c02)(content(Comment\"#   1. Add \
         PlantCol(Col) to the Action type       #\"))))(Secondary((id \
         e0c6cd36-dcec-4698-9622-2b7b10287b55)(content(Whitespace\"\\n\"))))(Secondary((id \
         091c953f-124d-410f-9f7d-c7ffada8b8d8)(content(Comment\"#   2. Add a \
         setCol helper function               #\"))))(Secondary((id \
         a8dba541-54a9-4d94-8405-0c1695bb6d53)(content(Whitespace\"\\n\"))))(Secondary((id \
         a02d8b80-603f-4382-a47c-29a98d24ee43)(content(Comment\"#   3. Handle \
         PlantCol in the update function     #\"))))(Secondary((id \
         bf2256db-3906-472b-a824-e610d79df274)(content(Whitespace\"\\n\"))))(Secondary((id \
         01d3b0a3-02f4-4d17-a9f2-b9dbefd96189)(content(Comment\"#                                                 \
         #\"))))(Secondary((id \
         ced2641e-a581-4966-a024-dcaf5e204abd)(content(Whitespace\"\\n\"))))(Secondary((id \
         2cec05a8-bf67-44f7-b8af-f85e0ca20f8b)(content(Comment\"# Look at how \
         PlantRow is implemented for         #\"))))(Secondary((id \
         b00c6cfe-91ce-487c-a883-cb40a2e11bc5)(content(Whitespace\"\\n\"))))(Secondary((id \
         34e66e71-296c-451c-acf5-abafde838445)(content(Comment\"# guidance - \
         PlantCol is similar but vertical.    #\"))))(Secondary((id \
         742647e9-323e-479f-9e32-34b2888d12a0)(content(Whitespace\"\\n\"))))(Secondary((id \
         d80a941d-fe64-4628-a8f0-a5522241e4df)(content(Comment\"#                                                 \
         #\"))))(Secondary((id \
         972c89f9-e820-42fe-bbd4-3e10bb4ef733)(content(Whitespace\"\\n\"))))(Secondary((id \
         1ae74a8b-69d9-415c-9f25-9bc1d9ed0705)(content(Comment\"# Tip: Use \
         auto-probe to see how the grove        #\"))))(Secondary((id \
         e0aa9ddf-8334-43f0-9ca1-c240105c48a9)(content(Whitespace\"\\n\"))))(Secondary((id \
         c4c39a3e-87a9-44f8-94a8-e88fed95133a)(content(Comment\"# changes \
         after each action.                      #\"))))(Secondary((id \
         776aea3b-927b-4351-beac-c328443a0ac0)(content(Whitespace\"\\n\"))))(Secondary((id \
         0a82aaee-15c6-4bdf-b395-8fb8b455f858)(content(Whitespace\"\\n\"))))(Tile((id \
         b015a647-04fa-4549-bc12-a38222febaa7)(label(type = in))(mold((out \
         Exp)(in_(TPat Typ))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         672cf395-e5e8-4e65-97c0-c87ce9a495d2)(content(Whitespace\" \
         \"))))(Tile((id \
         d9646ad6-a3d8-4d90-a68a-adbbfc185492)(label(Plant))(mold((out \
         TPat)(in_())(nibs(((shape Convex)(sort TPat))((shape Convex)(sort \
         TPat))))))(shards(0))(children())))(Secondary((id \
         8fc1f1cf-d62b-4fcd-b355-e76135f20ba2)(content(Whitespace\" \
         \")))))((Secondary((id \
         4312a829-54fe-4b5d-9a3b-0594a515e054)(content(Whitespace\" \
         \"))))(Tile((id \
         e56a38f6-f343-4db2-b956-c9a62878a3ef)(label(String))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         315134ac-4737-4acd-8ced-cd67d1fcf48f)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         ee9b704f-f086-40fa-96dd-44af342f0f7d)(content(Whitespace\"\\n\"))))(Tile((id \
         bdf3da11-b7ce-42a8-bc8f-5f1eb5f72ce8)(label(type = in))(mold((out \
         Exp)(in_(TPat Typ))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         c2f29378-426e-4dbc-9fc2-23eedb6018f3)(content(Whitespace\" \
         \"))))(Tile((id \
         18eaf3a5-79da-41e8-b8d8-931d6daec055)(label(Grove))(mold((out \
         TPat)(in_())(nibs(((shape Convex)(sort TPat))((shape Convex)(sort \
         TPat))))))(shards(0))(children())))(Secondary((id \
         d389cf09-3eaa-446a-b39a-fa6ddb0c4849)(content(Whitespace\" \
         \")))))((Secondary((id \
         0d919e5d-2368-410f-a14c-f318e9a51180)(content(Whitespace\" \
         \"))))(Tile((id f557b6bb-bdc6-4be0-90ed-06349cd71378)(label([ \
         ]))(mold((out Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape \
         Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
         1ad9573c-3b79-45bb-bb90-a9a3c441c322)(label([ ]))(mold((out \
         Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0 1))(children(((Tile((id \
         f3e5f754-c4c1-4adc-94d3-90ca90a323cd)(label(Plant))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))))))))))))(Secondary((id \
         b766f816-0240-4b8c-a083-8fcc19157ca0)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         f7b4cae1-539d-4740-8e1e-310a43fb38c2)(content(Whitespace\"\\n\"))))(Tile((id \
         8e0119f2-de82-41fc-b61b-39e3dc6ff580)(label(type = in))(mold((out \
         Exp)(in_(TPat Typ))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         01da1927-9ef6-4e30-b6b7-9eb0ead8a298)(content(Whitespace\" \
         \"))))(Tile((id \
         bb2058f7-60cc-4226-af9a-9321572e9b47)(label(Row))(mold((out \
         TPat)(in_())(nibs(((shape Convex)(sort TPat))((shape Convex)(sort \
         TPat))))))(shards(0))(children())))(Secondary((id \
         a3a77987-624d-4092-b8c9-9446b61f7c5d)(content(Whitespace\" \
         \")))))((Secondary((id \
         de770a4b-66d8-428a-996b-144897778e4e)(content(Whitespace\" \
         \"))))(Tile((id \
         e62fabd4-37d2-4f53-9e64-2a62e3cd8fa6)(label(Int))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         548df21d-02f6-4355-86ec-ad313c0490fd)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         065767a7-15e5-4401-b3c1-492563d27057)(content(Whitespace\"\\n\"))))(Tile((id \
         a56ef027-f4f9-498d-b36e-5a93816e7f0c)(label(type = in))(mold((out \
         Exp)(in_(TPat Typ))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         fb487790-f8e6-415d-be7b-f5c884a4314a)(content(Whitespace\" \
         \"))))(Tile((id \
         26079434-9544-4a6c-9520-7d483df20690)(label(Col))(mold((out \
         TPat)(in_())(nibs(((shape Convex)(sort TPat))((shape Convex)(sort \
         TPat))))))(shards(0))(children())))(Secondary((id \
         be78bead-94da-4447-8b51-c239b015e5fe)(content(Whitespace\" \
         \")))))((Secondary((id \
         b7439490-0c36-4be2-8cfd-29555df3e582)(content(Whitespace\" \
         \"))))(Tile((id \
         bfe52d0b-a7a1-4c8f-a5e9-2105c3de042e)(label(Int))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         a212bc3f-5fb4-4ed3-a6e7-455863034b09)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         904f0e83-bbf4-4620-b513-4dc24aa2ba4f)(content(Whitespace\"\\n\"))))(Secondary((id \
         cb358c74-d096-43a9-b3fa-bd1af9f15636)(content(Whitespace\"\\n\"))))(Tile((id \
         4f649ed0-1aad-49cc-96de-c421c90ec9ad)(label(type = in))(mold((out \
         Exp)(in_(TPat Typ))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         8aa0fba3-7dd1-443b-aad8-36e1b42d86db)(content(Whitespace\" \
         \"))))(Tile((id \
         9aa92bdf-ad4d-4fbe-b263-3b94008d0e0b)(label(Model))(mold((out \
         TPat)(in_())(nibs(((shape Convex)(sort TPat))((shape Convex)(sort \
         TPat))))))(shards(0))(children())))(Secondary((id \
         ace99582-d5e8-4dfc-82e9-9625e1f854de)(content(Whitespace\" \
         \")))))((Secondary((id \
         a85effc8-1976-4f90-96d3-82fc2446bfaf)(content(Whitespace\" \
         \"))))(Tile((id \
         36e0ab6e-88b6-4e88-bc71-43aef7235c2b)(label(\"(\"\")\"))(mold((out \
         Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0 1))(children(((Secondary((id \
         fb925f45-0519-4ce0-b91a-bdfa97c03140)(content(Whitespace\"\\n\"))))(Tile((id \
         ea04d7c1-4a55-48f8-895c-ce556d4d3691)(label(grove))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         e19e53b7-0b61-473b-9b7f-26be42f31462)(content(Whitespace\" \
         \"))))(Tile((id \
         de2bb7c3-372e-4b92-b97d-b61a9b570266)(label(=))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 39))(sort Typ))((shape(Concave \
         39))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         08fd3412-4f35-439e-a2f7-ec2d5b845a54)(content(Whitespace\" \
         \"))))(Tile((id \
         03a16a8a-f40c-42d5-9a27-34c6a46d06a9)(label(Grove))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         5a530206-4f2d-4e5a-9f20-f716a05df36d)(label(,))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 44))(sort Typ))((shape(Concave \
         44))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         382d10b2-3b95-43c6-8547-bdbeacf97112)(content(Whitespace\"\\n\"))))(Tile((id \
         789b3c47-d272-4373-a65a-99654d3e1244)(label(currentSeed))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         00880b36-443f-4791-81fb-8a903d71815b)(content(Whitespace\" \
         \"))))(Tile((id \
         177c9dd3-ad78-43cb-b588-8d31d592c0a9)(label(=))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 39))(sort Typ))((shape(Concave \
         39))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         a4b59263-8a7f-4508-bc95-c09b2087ab74)(content(Whitespace\" \
         \"))))(Tile((id \
         b05ecea2-e2e4-4e42-8f6f-c35dd7fe6e67)(label(Plant))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         765f99d1-1062-4fee-8ce4-c1137726b6a4)(label(,))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 44))(sort Typ))((shape(Concave \
         44))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         1a5e876b-f2cc-4e66-bf7c-860203c61cda)(content(Whitespace\"\\n\"))))(Tile((id \
         365ab502-3b0b-4264-81c2-ed90d7de9d20)(label(seedInventory))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         aa456ab9-fe64-4311-8765-a7e128e1476c)(content(Whitespace\" \
         \"))))(Tile((id \
         7dfb2246-d0eb-4322-8652-b2f98989d41a)(label(=))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 39))(sort Typ))((shape(Concave \
         39))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         4a28b7b2-8260-4498-b202-bdf428d10b5d)(content(Whitespace\" \
         \"))))(Tile((id 99fc0a5d-45a4-4573-aa9e-daebf7eab361)(label([ \
         ]))(mold((out Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape \
         Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
         fee60782-c8a9-4e35-9a77-a6bee1877442)(label(Plant))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children()))))))))(Secondary((id \
         de38da0b-fc41-4bb7-bd22-4c7a52f83b63)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         f53835fc-ed5b-480d-ab1a-0f6c507a174a)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         7653b243-2ac0-4f9b-a232-3b79071049c4)(content(Whitespace\"\\n\"))))(Secondary((id \
         8822307c-3e22-4519-84c6-1dc1b5506ec5)(content(Whitespace\"\\n\"))))(Tile((id \
         f4793d05-d1c5-4752-8738-97978c29655c)(label(type = in))(mold((out \
         Exp)(in_(TPat Typ))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         ae1e7d26-0e63-40d3-b5ca-ede4f363069d)(content(Whitespace\" \
         \"))))(Tile((id \
         48728924-6f9c-4944-8306-d7b9b3683e61)(label(Action))(mold((out \
         TPat)(in_())(nibs(((shape Convex)(sort TPat))((shape Convex)(sort \
         TPat))))))(shards(0))(children())))(Secondary((id \
         b608380c-9633-42a4-b4dd-c68b4b049d4a)(content(Whitespace\" \
         \")))))((Secondary((id \
         61d5293a-0446-466a-a8f3-032eff02e9e7)(content(Whitespace\"\\n\"))))(Tile((id \
         95a0ae0a-c2de-426a-b7ab-122c52309773)(label(+))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape(Concave 33))(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         a6865c60-4b49-43a5-9a6d-dada61dbb5ac)(content(Whitespace\" \
         \"))))(Tile((id \
         1504380c-496d-4c22-a356-ce38d97c3e60)(label(SelectSeed))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         16eb19c0-fb2f-4d8e-8b89-32be1d2f7ddf)(label(\"(\"\")\"))(mold((out \
         Typ)(in_(Typ))(nibs(((shape(Concave 11))(sort Typ))((shape \
         Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
         2d62ec0a-47f5-40fd-99e6-2e274eab93d0)(label(Int))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children()))))))))(Secondary((id \
         1106481a-d9b1-46a4-b798-1fbb5f5c54a9)(content(Whitespace\"\\n\"))))(Tile((id \
         46f2ce0f-ae79-42a2-803b-f5fdd244fa9a)(label(+))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 12))(sort Typ))((shape(Concave \
         12))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         d7daa0e8-19b4-4641-bfd1-79711b75c8c5)(content(Whitespace\" \
         \"))))(Tile((id \
         4cda8e82-1933-4fa1-9b64-1fabd9ff28e4)(label(PlantSeed))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         3bc8bb3a-edec-452b-b962-885b2394e84b)(label(\"(\"\")\"))(mold((out \
         Typ)(in_(Typ))(nibs(((shape(Concave 11))(sort Typ))((shape \
         Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
         01e673eb-a438-49c4-b1d2-b0386d932f2b)(label(Row))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         fb426e35-0875-4a8b-9922-2e196941abc9)(label(,))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 44))(sort Typ))((shape(Concave \
         44))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         7c4338be-da0f-4286-bb46-4b23c095a2c5)(content(Whitespace\" \
         \"))))(Tile((id \
         7f7f021a-82bf-4f38-a0bb-bb470574ded2)(label(Col))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children()))))))))(Secondary((id \
         f8ecdce0-4f4f-49db-abed-bcd72e49c2a6)(content(Whitespace\"\\n\"))))(Tile((id \
         171a897b-6e17-46f5-a384-7f349eba1b60)(label(+))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 12))(sort Typ))((shape(Concave \
         12))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         7fc9e50e-c800-4804-94e6-38dada87d415)(content(Whitespace\" \
         \"))))(Tile((id \
         8e705c52-61d9-407b-8db0-66948f11f36a)(label(Uproot))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         ca0c1700-2ca3-4a7d-ae31-40e022744ed7)(label(\"(\"\")\"))(mold((out \
         Typ)(in_(Typ))(nibs(((shape(Concave 11))(sort Typ))((shape \
         Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
         54f8e40f-e18c-45eb-9aad-de41ef29fe56)(label(Row))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         077cc5d6-5891-44d1-9176-a09d214974a1)(label(,))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 44))(sort Typ))((shape(Concave \
         44))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         75f3671d-3777-4a04-8031-105b88afd478)(content(Whitespace\" \
         \"))))(Tile((id \
         d172e5e7-9ea2-41af-ae6b-133068a82e4e)(label(Col))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children()))))))))(Secondary((id \
         02015d9d-04a7-471c-adcd-bcd1695c131e)(content(Whitespace\"\\n\"))))(Tile((id \
         16121647-1307-4398-a5ac-f673681ab058)(label(+))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 12))(sort Typ))((shape(Concave \
         12))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         5ed1b13c-eb45-4b94-a970-f0f7f691bfab)(content(Whitespace\" \
         \"))))(Tile((id \
         e706e17b-3c7a-4dcd-81a9-5f119e23f66b)(label(ClearGrove))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         d5602de8-824e-4bbd-bc30-8871bd763ff2)(content(Whitespace\"\\n\"))))(Tile((id \
         d5592898-8b8d-4405-998a-f1cc1fd515be)(label(+))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 12))(sort Typ))((shape(Concave \
         12))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         ef2b2a2f-03cf-4e77-a60a-041e0ecd1a17)(content(Whitespace\" \
         \"))))(Tile((id \
         5a15ca78-2cb0-4a56-835b-c2c11035eff8)(label(PlantRow))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         5de22b1b-2048-4100-a382-bb8d2f5e37b7)(label(\"(\"\")\"))(mold((out \
         Typ)(in_(Typ))(nibs(((shape(Concave 11))(sort Typ))((shape \
         Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
         3f6c1954-7c14-4677-9a98-62178b388278)(label(Row))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children()))))))))(Secondary((id \
         e31870aa-bc49-4643-a416-7b0fb1b833dc)(content(Whitespace\"\\n\"))))(Secondary((id \
         24d90ac7-5968-438f-9b1e-2ca31c4da6f3)(content(Comment\"# TODO: Add \
         PlantCol(Col) here #\"))))(Secondary((id \
         0b7d5d6a-98bf-43d0-9b5e-c0dacd70f9c4)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         fa1ba114-fa9d-40c0-ae4c-25a896ac1042)(content(Whitespace\"\\n\"))))(Secondary((id \
         02803cc4-612b-4fa3-842d-0089a0682f80)(content(Whitespace\"\\n\"))))(Tile((id \
         6a16fefb-d8ce-437f-938c-8d54e3b7c6e5)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         a1a01e47-6651-4d5e-a69a-6cfd48b70786)(content(Whitespace\" \
         \"))))(Tile((id \
         a8c83efd-7f1a-48af-b9d4-ad8f5b8ec1ae)(label(init))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         15aa322b-6460-4179-931e-25a391916098)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 24))(sort Pat))((shape(Concave \
         24))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         ed14a98b-e1d9-44fa-a6a4-163356a9150f)(content(Whitespace\" \
         \"))))(Tile((id \
         26ab3964-70e2-4482-9d54-45fae93d860a)(label(Model))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         93279060-c7f6-45f7-a561-92f0ca8c81e0)(content(Whitespace\" \
         \")))))((Secondary((id \
         8830f108-d92a-46c5-9853-1837c3654180)(content(Whitespace\" \
         \"))))(Tile((id \
         533ab434-3647-4488-88ef-77213432ef1d)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         c1944d64-87c0-4e14-bbe0-d964b40db3c0)(content(Whitespace\"\\n\"))))(Tile((id \
         edc8ad4d-5754-4441-9fad-b8052e52b8a8)(label(grove))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         073029dc-7063-4842-ac4b-2eb84443424c)(content(Whitespace\" \
         \"))))(Tile((id \
         55ffffb4-677e-4e3a-b1a6-d59a59e938a4)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         4326ef72-b92e-4598-8252-6a3194e58f03)(content(Whitespace\" \
         \"))))(Tile((id 13d6a32d-a394-487c-98c2-aa669a38e472)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         9b25eab9-7059-4331-a9b3-d2f50cd5a697)(content(Whitespace\"\\n\"))))(Tile((id \
         bb66433a-c79d-40e7-a849-1fee82cb4002)(label([ ]))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         1fc9e71a-3ece-4793-86c0-e53f6dcffb98)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         3f2f620d-8e44-4de3-b3ea-cc3d933ed0b5)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         aea49905-e3c1-4c90-99c6-3942d407377d)(content(Whitespace\" \
         \"))))(Tile((id \
         d6ce9318-6b9e-4837-91a5-2d803ef26f69)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         70d6dcb7-95b1-4730-90e9-906e6789bb23)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         562fb785-5533-41b6-b741-c94bfc391236)(content(Whitespace\" \
         \"))))(Tile((id \
         fe71cab1-e021-483a-a22c-864d74d73389)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         86edbfea-e9f3-4a28-8039-63d2e3d9ee10)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         102b3b68-e507-4ceb-8386-de422f5c13c2)(content(Whitespace\"\\n\"))))(Tile((id \
         b85b2ab7-978a-438d-b3f9-9634909e5661)(label([ ]))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         6e325043-27f7-446f-940b-8ad429d03331)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         9d5b7b97-53d4-4eb3-8bff-17bcea3911c7)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         e5c00952-a1a7-4123-b180-540ab77aee01)(content(Whitespace\" \
         \"))))(Tile((id \
         991e1745-7cd7-480e-a888-dbb53394f3eb)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         6755c39c-b1db-4df1-b942-4effcbc95830)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         b72e40d0-b157-442f-b15a-df57d4194402)(content(Whitespace\" \
         \"))))(Tile((id \
         729c1f7f-4520-45fb-9dd1-1f25cd76d239)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         d92d1c3a-edc7-4c27-8fb0-40e69dcf8c54)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         5bdb4076-1351-473d-842f-e099ee4ef8bf)(content(Whitespace\"\\n\"))))(Tile((id \
         1d61cb9a-e79b-4e42-a09a-454b1f1cfc2a)(label([ ]))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         f40180ce-d3f4-43e6-b4f0-b3ee24075066)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         1986cb4f-a05f-470a-beec-89c2ba62f12f)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         3d278b85-1809-4b11-9d8e-97d11d290e16)(content(Whitespace\" \
         \"))))(Tile((id \
         a676ea62-86c2-4719-b3c2-f6f332c348d6)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         ea06600a-74c5-4f43-bef6-527ed4759d9f)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         0b65b426-8445-4119-bcab-41ab076a155a)(content(Whitespace\" \
         \"))))(Tile((id \
         b532ec60-1413-43e0-bf21-ee47c97ce42b)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         354c3d48-33da-45c5-9ef9-662c00e04022)(content(Whitespace\"\\n\")))))))))(Tile((id \
         018bba0e-68f9-4558-b762-302f0a147a28)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         e8bca076-ecf8-4cc9-9281-aff0753bec18)(content(Whitespace\"\\n\"))))(Tile((id \
         3e1412cb-8c1a-4889-9114-03a31cab54aa)(label(currentSeed))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         4718cbf7-7025-4538-8466-9bbe2913a42c)(content(Whitespace\" \
         \"))))(Tile((id \
         a58839c6-abc6-4828-bf26-56458d16b522)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         08c5b4a9-2c16-4678-b20d-207eb0037aa3)(content(Whitespace\" \
         \"))))(Tile((id \
         4057efd9-c8e0-4b5f-9ac6-2163e7a9d41c)(label(\"\\\"\\240\\159\\140\\177\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         692d9b1b-832a-4abb-b8c1-e58799720042)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         c20fd02a-4584-4e87-8679-721833a8ca6c)(content(Whitespace\"\\n\"))))(Tile((id \
         e03b96a4-f6e9-4ec4-ac2c-1bedd7389038)(label(seedInventory))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         afed5374-0ae3-4344-9787-bde6e5bd3e32)(content(Whitespace\" \
         \"))))(Tile((id \
         82fd11c0-f496-4518-a133-4f4cc41ef137)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         9cfa9297-1e0b-418e-b036-8e61f9735d07)(content(Whitespace\" \
         \"))))(Tile((id 30211e80-5781-4abd-bf81-0a87119a0e2b)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         4cd348f7-38db-460c-a0a8-8ddf6e16705e)(label(\"\\\"\\240\\159\\140\\177\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         b0f53932-1010-4b02-aa48-18cb810b49a9)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         232d26f2-68b3-418c-9109-954f6ff01594)(content(Whitespace\" \
         \"))))(Tile((id \
         eb81477a-bed9-452d-9188-829fe8369237)(label(\"\\\"\\240\\159\\140\\191\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         873eb4fd-3e55-42e2-b81b-a51b432995ed)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         f5a30a3e-66f7-4f29-bce6-6fea42ff9b1e)(content(Whitespace\" \
         \"))))(Tile((id \
         655402f7-2214-47a4-98ca-8fc105438480)(label(\"\\\"\\240\\159\\141\\132\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         51f3a80a-bfdb-4cd5-902e-e79ae2b75cdf)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         788a5f97-98dd-4c63-ac08-92d8b10242c9)(content(Whitespace\" \
         \"))))(Tile((id \
         d78f7bf8-7dc7-4620-af4a-a4feccedeab8)(label(\"\\\"\\226\\152\\152\\239\\184\\143\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         e4fb2d0e-2a47-4a3c-9b58-c774a7c6e67b)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         88ed0136-6eae-4bf0-9583-79823a5d3394)(content(Whitespace\" \
         \"))))(Tile((id \
         a58c75da-ff9c-42fa-90eb-264510cdff70)(label(\"\\\"\\240\\159\\140\\184\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         4f9cacbc-cab3-475b-8725-91f422a8ba28)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         933a7f41-0f16-402e-8706-aeb899a98710)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         d45a95a7-207e-437e-8f48-fec07d097574)(content(Whitespace\"\\n\"))))(Secondary((id \
         a5dd43e2-becb-44ae-9577-e9ce7e67c6bf)(content(Whitespace\"\\n\"))))(Tile((id \
         66e33c01-8505-4d13-bc2a-d33aeea1b8a3)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         818c6fb2-1b43-40ab-85dc-cfa46b2e0e53)(content(Whitespace\" \
         \"))))(Tile((id \
         03c19407-511e-49ac-8c8e-172392e6abd9)(label(setCell))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         f222030c-710e-468d-a9d4-c330e5ee0f11)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 24))(sort Pat))((shape(Concave \
         24))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         62b4f47c-e2a3-4912-b39b-86bc884dfa77)(content(Whitespace\" \
         \"))))(Tile((id \
         0b6e123d-2ec5-42e4-8c96-e0bda35d8247)(label(\"(\"\")\"))(mold((out \
         Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0 1))(children(((Tile((id \
         924ce93d-37c7-41ff-b197-fbcd31b36950)(label(Grove))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         3f283330-98e4-4c45-ac7c-950c580698e8)(label(,))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 44))(sort Typ))((shape(Concave \
         44))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         d8b8638e-c145-4639-b47a-292ca416862e)(content(Whitespace\" \
         \"))))(Tile((id \
         81c4196e-2c06-47ce-8e61-f27da87f4e13)(label(Row))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         82b1453a-c732-471a-905b-5c002150a629)(label(,))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 44))(sort Typ))((shape(Concave \
         44))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         35908bf9-5e9b-436d-9e50-33afc396099b)(content(Whitespace\" \
         \"))))(Tile((id \
         2ff124bc-3eda-418a-9b6c-0ffcc03d257e)(label(Col))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         0ea093e6-94fb-4fd7-a8cc-84234ce9e722)(label(,))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 44))(sort Typ))((shape(Concave \
         44))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         7b0ef0f6-07a4-46c8-9422-e147d7eefea1)(content(Whitespace\" \
         \"))))(Tile((id \
         442666a4-aedf-427e-a9d5-c6186289479f)(label(Plant))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children()))))))))(Secondary((id \
         8aa0f631-5f93-4114-8342-215f1c793dde)(content(Whitespace\" \
         \"))))(Tile((id \
         a2ec9933-1361-41eb-8515-1a66ae7791e1)(label(->))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 13))(sort Typ))((shape(Concave \
         13))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         5143f36a-4699-4ee9-9a68-e9039c4bada6)(content(Whitespace\" \
         \"))))(Tile((id \
         3737b285-960e-4500-a457-dee2fe3c68c6)(label(Grove))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         64a20872-b928-4503-a611-f293573baabd)(content(Whitespace\" \
         \")))))((Secondary((id \
         4c50ba02-a0b0-4bc5-8dcc-db3292f9afb6)(content(Whitespace\"\\n\"))))(Tile((id \
         7199c101-d0b1-4527-ace8-f66ae7a6229e)(label(fun ->))(mold((out \
         Exp)(in_(Pat))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         37))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         f4d05b1e-e923-46fe-b93a-3e0ae405e556)(content(Whitespace\" \
         \"))))(Tile((id \
         02cdd4be-e51c-452c-90d8-e5f727268031)(label(grove))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         918410d0-2a20-4200-bd5d-aa21f68f8785)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 44))(sort Pat))((shape(Concave \
         44))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         c41e6660-6122-47de-a5d0-ade622bd77a5)(content(Whitespace\" \
         \"))))(Tile((id \
         241a5584-ccb0-436d-9257-e9553ea32ab7)(label(row))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         72ec6bc7-ce94-4f4c-8c32-d42ada4d8feb)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 44))(sort Pat))((shape(Concave \
         44))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         7effc614-90fd-47f4-ac1e-ab0df98564fd)(content(Whitespace\" \
         \"))))(Tile((id \
         07099992-aa55-45cf-87b1-a15a821c4af4)(label(col))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         13f3aa2e-dad2-43b1-940c-492eec757ac6)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 44))(sort Pat))((shape(Concave \
         44))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         3abed655-9893-4574-909b-a0f7a33ebe01)(content(Whitespace\" \
         \"))))(Tile((id \
         db0be52c-3ffa-47ee-a8c3-a53d12ccf689)(label(plant))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         60fb5bec-0379-4055-aa70-d42be85cf0b1)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         2e12dd37-4a92-4b46-b323-cbdf560e48b2)(content(Whitespace\"\\n\"))))(Tile((id \
         36273b7d-8d0e-4158-9600-a75f9ebab382)(label(mapi))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         b9543131-3f23-4204-90c2-7130bd571515)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         7791e8ab-9c0f-49af-8e26-b21218699570)(label(grove))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         08a9a69b-3103-455e-b2f8-4cc3046f49f5)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         ce676124-c6c9-450b-97b4-e2335a45c76e)(content(Whitespace\" \
         \"))))(Tile((id 72f733c6-c245-441a-8347-2a6aa8aa83e8)(label(fun \
         ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 37))(sort Exp))))))(shards(0 \
         1))(children(((Secondary((id \
         ae9c7a2f-7935-47c5-9ec3-b5d7d7283748)(content(Whitespace\" \
         \"))))(Tile((id \
         d23134ec-5bbc-4d0b-b75d-50439b2e8b49)(label(\"(\"\")\"))(mold((out \
         Pat)(in_(Pat))(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0 1))(children(((Tile((id \
         64580c62-dbc1-4e8f-844f-1206b5ea6bea)(label(i))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         132105ae-8e1c-4f71-845c-7bf3e9e580dd)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 44))(sort Pat))((shape(Concave \
         44))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         d537cf60-42ab-4025-9c07-664306e338ad)(content(Whitespace\" \
         \"))))(Tile((id \
         5fed2d91-8252-486c-a96a-7697f3fdbc19)(label(r))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children()))))))))(Secondary((id \
         d96405e9-d02b-4a45-b558-fc0558cb105e)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         8e7ff064-fc6f-4a08-95ee-1bbcbf9607b4)(content(Whitespace\"\\n\"))))(Tile((id \
         4f37a466-0f7d-46e3-833d-b62a078c632f)(label(if then else))(mold((out \
         Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         36))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         d3473ca7-6eeb-42a6-a64c-7cd50d6afdbd)(content(Whitespace\" \
         \"))))(Tile((id \
         af0db780-b014-423c-9839-538a87e31a76)(label(i))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         7b04d23f-33c1-432b-9957-93bcdf86d861)(content(Whitespace\" \
         \"))))(Tile((id \
         e89886f4-3900-4b25-8f2a-605557ee83c5)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         66cddaa3-2e8f-484c-ad8c-91f8e1c56ba9)(content(Whitespace\" \
         \"))))(Tile((id \
         f1449c09-3ec1-4c6c-9867-a7f726f896bd)(label(row))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         394198c9-0027-4df0-aa1e-9962d30df244)(content(Whitespace\"\\n\")))))((Secondary((id \
         5e79d265-058e-4f3c-8208-4ffb0fff6d4e)(content(Whitespace\" \
         \"))))(Tile((id \
         be57d5a7-92bc-4d9c-b98a-5298af056679)(label(mapi))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         25bd67e0-aeb2-450d-87cb-6f18e9ba799e)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         72388409-db44-447c-95bb-7f4c36a68bd1)(label(r))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         de6d2cad-ea1e-44e6-bd69-0858a20ea731)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         212bdcee-39a8-4034-a95e-7161d654c33a)(content(Whitespace\" \
         \"))))(Tile((id 7edf1b61-498f-4fc0-a91c-c19a4f942193)(label(fun \
         ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 37))(sort Exp))))))(shards(0 \
         1))(children(((Secondary((id \
         66d9421c-0129-4276-af56-3463bbfe05d3)(content(Whitespace\" \
         \"))))(Tile((id \
         bf91962d-9342-4fc3-b373-0f44582c8d33)(label(\"(\"\")\"))(mold((out \
         Pat)(in_(Pat))(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0 1))(children(((Tile((id \
         5dbee29a-98a6-4b79-9c40-0c53a5fc0a0f)(label(j))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         beae6567-14cc-45a1-a5ff-2163d6c9c842)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 44))(sort Pat))((shape(Concave \
         44))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         a8d1bdb0-817a-45c3-96c2-25d0b435b877)(content(Whitespace\" \
         \"))))(Tile((id \
         5f256dde-22de-4c66-991a-2acf271f2e92)(label(c))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children()))))))))(Secondary((id \
         18f3c936-96f4-4f42-b3cd-d3d41201c956)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         04fe849e-8d1a-4d9a-8d88-6e70de194c7d)(content(Whitespace\" \
         \"))))(Tile((id 83ae2ca6-cc49-4310-8b95-a86d439ee864)(label(if then \
         else))(mold((out Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 36))(sort Exp))))))(shards(0 1 \
         2))(children(((Secondary((id \
         acc2acd4-4061-4104-808f-6e5f769af812)(content(Whitespace\" \
         \"))))(Tile((id \
         f24f398e-8427-45c3-b7b0-570981478f32)(label(j))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         b1be0818-f745-4d80-9430-358a1386d968)(content(Whitespace\" \
         \"))))(Tile((id \
         dceee943-c015-4bf0-b08a-aa38f570ab27)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         a1e3a7d8-eeda-44d7-9b5e-dbd40e48741a)(content(Whitespace\" \
         \"))))(Tile((id \
         139dc705-3e7e-4d5e-9682-f5b78cb82ed7)(label(col))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         95d83010-2ef3-46e8-a6f3-a7798b453df1)(content(Whitespace\" \
         \")))))((Secondary((id \
         e13d2e9c-0b63-4626-a292-f11afdb205da)(content(Whitespace\" \
         \"))))(Tile((id \
         a3bf193a-8cfe-4fbc-891e-3e2ab75b56f3)(label(plant))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         a1e89878-86e6-4274-819e-5990e4c3ad77)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         9e7a359d-cf69-4c28-84ce-1988cf10fbde)(content(Whitespace\" \
         \"))))(Tile((id \
         bf444e79-8e02-4fea-b6b2-42d65fbabb66)(label(c))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         a242aa01-1d6e-4aef-b435-92b490f5bf86)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         2179ee28-594b-4904-8bff-5442911cdf2a)(content(Whitespace\" \
         \"))))(Tile((id \
         64bfdd7e-e424-4d40-82c8-ad41fc73e6cf)(label(r))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         b0fed489-14fd-4184-8e44-d1e0ad54ba01)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         201fc3af-646c-45f8-bf92-125aa93b5d31)(content(Whitespace\"\\n\"))))(Secondary((id \
         965ed4ed-40be-4564-bb3c-d1e91f5868c5)(content(Whitespace\"\\n\"))))(Tile((id \
         f95a5e6e-8d5c-4c97-9f89-aafcbc670a25)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         6fc2a1ee-2ccd-4fb0-8655-70ac57330590)(content(Whitespace\" \
         \"))))(Tile((id \
         f10fcbc7-15b1-4ef5-8502-1e3e334e136d)(label(setRow))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         05419c1e-0b10-42dd-918f-6d4958000193)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 24))(sort Pat))((shape(Concave \
         24))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         c1229e23-e21d-4a3f-b2c5-5928e1d79504)(content(Whitespace\" \
         \"))))(Tile((id \
         546979da-6b8d-4851-8e0d-58cbdcceaa54)(label(\"(\"\")\"))(mold((out \
         Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0 1))(children(((Tile((id \
         0cf197d8-3d32-4715-9b86-ee4c3e3c15ff)(label(Grove))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         e884ed29-cb2a-4bae-aaa0-be58102afb48)(label(,))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 44))(sort Typ))((shape(Concave \
         44))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         a2ab1ffb-cf21-4df7-a8ac-1355a798fe08)(content(Whitespace\" \
         \"))))(Tile((id \
         36e7a799-de85-491f-b64e-25f100dbe112)(label(Row))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         1b413372-e606-41f7-8976-c72419cb3acd)(label(,))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 44))(sort Typ))((shape(Concave \
         44))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         fb5e6703-073a-48c1-b270-a4302a5564b4)(content(Whitespace\" \
         \"))))(Tile((id \
         405bdc2c-4800-480a-8a3f-804db5e5e4fd)(label(Plant))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children()))))))))(Secondary((id \
         e453cd14-6cf6-4b4f-be67-0b8e41beb94d)(content(Whitespace\" \
         \"))))(Tile((id \
         df2c56d7-51f9-4274-bb7e-1ba5471993ed)(label(->))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 13))(sort Typ))((shape(Concave \
         13))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         19881de2-775a-41d2-99c2-6311e23059c6)(content(Whitespace\" \
         \"))))(Tile((id \
         a322f453-cc90-4403-8eb8-78d14072fc0d)(label(Grove))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         058cb45b-cce9-48f7-b3bc-4cb003051dad)(content(Whitespace\" \
         \")))))((Secondary((id \
         dc5f042b-ee4a-41c6-82d7-c24d4e78dc82)(content(Whitespace\"\\n\"))))(Tile((id \
         069b9d2c-e0a2-4b96-9309-ee5f0de511f3)(label(fun ->))(mold((out \
         Exp)(in_(Pat))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         37))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         65557092-fe52-4fac-9454-e7354c0fa344)(content(Whitespace\" \
         \"))))(Tile((id \
         abc952d0-a9c8-40c3-b278-36cd10820624)(label(grove))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         c0d4f1ea-612b-4ece-83fe-86720ce04e26)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 44))(sort Pat))((shape(Concave \
         44))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         b2d85614-c7b6-4c14-a594-8c67020a7d86)(content(Whitespace\" \
         \"))))(Tile((id \
         19403a94-eb67-415d-ac96-8a0e8d594ad5)(label(targetRow))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         7e1428f0-ac0d-44ac-ba3d-427b472e69c5)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 44))(sort Pat))((shape(Concave \
         44))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         8d3a20ff-d184-4dc0-85c8-3b7bdb7f5e8a)(content(Whitespace\" \
         \"))))(Tile((id \
         4fba918d-da0c-4f9c-b854-282b90990480)(label(plant))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         31b0cc00-6f4f-4fb4-9c91-f25f6e0e3631)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         505837d7-7181-41ed-a39c-b1aed59dd77a)(content(Whitespace\"\\n\"))))(Tile((id \
         e2257c68-3999-4081-9bd3-bef7dcf3b824)(label(mapi))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         1c04c700-f92b-460a-9a39-96e327e93338)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         65143792-6a6e-403f-b33b-900718c533bd)(label(grove))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         7ca8454b-647c-46c8-bc90-c21998268bf9)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         a58b925f-40e1-4696-bfbc-b7b87c7fe279)(content(Whitespace\" \
         \"))))(Tile((id c5b6577f-2244-4078-9232-591f25a389a0)(label(fun \
         ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 37))(sort Exp))))))(shards(0 \
         1))(children(((Secondary((id \
         d5deca88-3394-48b3-9d87-1dc47569782c)(content(Whitespace\" \
         \"))))(Tile((id \
         f085956c-8625-4057-9b75-4c8def2aa3b8)(label(\"(\"\")\"))(mold((out \
         Pat)(in_(Pat))(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0 1))(children(((Tile((id \
         3bc385ab-cbb0-4813-8be5-c2321a65e7e2)(label(i))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         f98ea445-74f7-49d9-a9a9-cd7be912c01e)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 44))(sort Pat))((shape(Concave \
         44))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         49a2c3ca-a275-4940-8969-5e24d3a7caa0)(content(Whitespace\" \
         \"))))(Tile((id \
         09d49a86-d758-4166-b647-a3296a669263)(label(row))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children()))))))))(Secondary((id \
         a5bc5b6f-54c6-402e-bb04-af392e1e6608)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         4872b6fb-7d3a-4200-85eb-5198c47ea773)(content(Whitespace\"\\n\"))))(Tile((id \
         ef605b20-3a87-41e0-b844-c2abf8921ea0)(label(if then else))(mold((out \
         Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         36))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         9b5aaab4-37f6-4a49-9fa7-85dcf5f754b2)(content(Whitespace\" \
         \"))))(Tile((id \
         a265d59d-92cd-49a8-a621-49d6e2e0bbce)(label(i))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         cf4818cf-e989-4295-b2e0-d3256c72b794)(content(Whitespace\" \
         \"))))(Tile((id \
         a3d3df12-d34e-4f34-962c-c4285d9cdf90)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         e9cc0981-e930-4c09-867b-79eea9ad4911)(content(Whitespace\" \
         \"))))(Tile((id \
         c60088a5-e89b-4391-8cb7-f49331d3443c)(label(targetRow))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         aeb425d1-0d69-47c8-8f2f-88cbe6f606c7)(content(Whitespace\"\\n\")))))((Secondary((id \
         f94a2575-a325-4523-871b-5670f05f8a2f)(content(Whitespace\" \
         \"))))(Tile((id \
         0062e216-50f9-4280-91f0-32d12bcbe8fc)(label(map))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         ce2864ba-93f6-4419-8c1c-e1835423363f)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         0939de60-2bc5-4aa3-9720-a66cafbe4898)(label(row))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         1a5a9f0d-16dc-4cc6-80f0-5ca4fcee8a96)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         92d76bda-edea-448a-851d-6a0307390536)(content(Whitespace\" \
         \"))))(Tile((id 0e24021d-412f-4234-a6bd-5fe20786852d)(label(fun \
         ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 37))(sort Exp))))))(shards(0 \
         1))(children(((Secondary((id \
         5a10f581-74d1-41b6-ae1a-629552918908)(content(Whitespace\" \
         \"))))(Tile((id \
         903826c1-9baa-43ae-b8d2-9e650b67df96)(label(_))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         157b550f-c6fd-45b2-8cfb-613b594b3c7a)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         6a087785-669b-44d0-b61b-1fac2cb63d8e)(content(Whitespace\" \
         \"))))(Tile((id \
         85ee63d8-8cf9-4420-b239-961e4e834ba1)(label(plant))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         fafa3b7a-63cc-46cc-8895-42b53e44ff7b)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         039b4f2f-6d80-43b4-b0d3-04a420d7000c)(content(Whitespace\" \
         \"))))(Tile((id \
         430cb2fb-e38f-4495-9d83-b65b3e7d9247)(label(row))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         0b6c26b4-7678-4e2d-a026-1a797bae6c53)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         b3444e3a-0c5d-4b74-9fcc-dbff573bd498)(content(Whitespace\"\\n\"))))(Secondary((id \
         24a4ed2e-1bb2-40d5-a663-02a6009bd5e5)(content(Whitespace\"\\n\"))))(Secondary((id \
         8baf0f77-04bc-4cb8-a80a-1401814d113f)(content(Comment\"# TODO: Add \
         setCol helper here #\"))))(Secondary((id \
         3d51ef22-440f-48c9-9edf-4f71c747e359)(content(Whitespace\"\\n\"))))(Secondary((id \
         dd9bbbaf-bdab-4ba5-b771-1092a1722545)(content(Comment\"# Hint: You \
         need to modify each row, changing #\"))))(Secondary((id \
         1c4652d5-fdb8-4ddf-b2bd-c608d8ae67f8)(content(Whitespace\"\\n\"))))(Secondary((id \
         ba401fb4-5bbe-43d3-b707-53efa1940690)(content(Comment\"# only the \
         cell at the target column.         #\"))))(Secondary((id \
         17273638-35ac-4ead-a4a5-e552b04f8f6c)(content(Whitespace\"\\n\"))))(Secondary((id \
         610ed345-ffe0-4984-bcbb-a7dd7c89a381)(content(Whitespace\"\\n\"))))(Tile((id \
         1205712b-198b-4212-a076-bed26aa1ebfb)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         2268f8cb-78e8-4531-b59e-8e04f99ac31e)(content(Whitespace\" \
         \"))))(Tile((id \
         8bc3a817-3135-4bbf-9ad8-c69878066f7f)(label(setAll))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         a5152deb-e7be-48bb-a86f-ddecf6f187c9)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 24))(sort Pat))((shape(Concave \
         24))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         64e6e455-4593-4196-9b59-117155c11df9)(content(Whitespace\" \
         \"))))(Tile((id \
         69ee6897-d28c-4da8-9edd-0ff4c632a968)(label(\"(\"\")\"))(mold((out \
         Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0 1))(children(((Tile((id \
         3a4b7bdf-3108-42d0-ac91-9abce7c358fa)(label(Grove))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         973b4a34-f925-4846-8bf3-135b7733f6e9)(label(,))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 44))(sort Typ))((shape(Concave \
         44))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         c6935fae-597d-498f-baac-2772d88b9199)(content(Whitespace\" \
         \"))))(Tile((id \
         6df45ccb-8a95-482d-a3d5-8ab2a1ef2c96)(label(Plant))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children()))))))))(Secondary((id \
         9642fef4-81df-4b06-9d60-35a045d43f58)(content(Whitespace\" \
         \"))))(Tile((id \
         01f396d1-1064-40d6-9e91-a7f548bdfeed)(label(->))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 13))(sort Typ))((shape(Concave \
         13))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         a38a7a93-b155-41b3-b658-e7742ebec7ed)(content(Whitespace\" \
         \"))))(Tile((id \
         e15a4897-c1d5-4536-844f-8d1e846c6190)(label(Grove))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         bf30006f-b4a5-479e-ad0f-9783e783213a)(content(Whitespace\" \
         \")))))((Secondary((id \
         57c1be7b-cbb8-40a5-a980-efccc608e80c)(content(Whitespace\"\\n\"))))(Tile((id \
         b346da24-bada-415f-86a4-20fd78f68785)(label(fun ->))(mold((out \
         Exp)(in_(Pat))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         37))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         96107c37-8d05-478f-b24b-6042cc1e3306)(content(Whitespace\" \
         \"))))(Tile((id \
         e032cfe4-7ed8-4810-9f0c-cf2cf34cda7b)(label(\"(\"\")\"))(mold((out \
         Pat)(in_(Pat))(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0 1))(children(((Tile((id \
         7d475c13-2fef-43de-bb05-9f14314c29c7)(label(grove))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         321a65e3-de0c-4f8d-ac73-5fbf0805e205)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 44))(sort Pat))((shape(Concave \
         44))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         1a371f5e-fa7e-41af-af47-e7a06e1363dc)(content(Whitespace\" \
         \"))))(Tile((id \
         19e01eb0-f34f-4430-84a1-c173b204e66d)(label(plant))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children()))))))))(Secondary((id \
         e4c71079-593a-4aa4-ab8c-b92345560d92)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         ec3285fe-5313-4b52-afa8-1e647ab71917)(content(Whitespace\"\\n\"))))(Tile((id \
         5f0e737e-aa8b-43da-8a91-9c0d4f0417a9)(label(map))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         71721e57-3075-4f02-9fc2-095bc43c398d)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         c14b5b05-0439-4aa1-b453-93c395b3e12d)(label(grove))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         671fcf8f-cbfc-4144-bbfe-26852e4f8381)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         bad0e9ca-be99-4150-a728-87b0b538eb74)(content(Whitespace\" \
         \"))))(Tile((id 565cb4f1-5c8e-4b61-828e-56b96ef63500)(label(fun \
         ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 37))(sort Exp))))))(shards(0 \
         1))(children(((Secondary((id \
         e38951dc-bbd9-4bd1-a72e-873094ce85c0)(content(Whitespace\" \
         \"))))(Tile((id \
         728e5266-beae-40e5-9b2a-af1838dd2e9f)(label(row))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         8d1804ec-b58a-409b-8f82-ba73eaaf6484)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         04fd3d78-37b3-41a9-8168-c2941c81065d)(content(Whitespace\" \
         \"))))(Tile((id \
         a6c9ce11-b52b-443d-a18a-bc23fc20a33d)(label(map))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         96ed5529-3731-4e3b-a43c-2024819922b5)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         b6973663-7c1f-44cb-b363-12bb7b8e51ed)(label(row))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         d638b429-2451-49b5-8a4c-48171e418786)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         95ca3330-cb07-4aaa-a885-82e951cb3e10)(content(Whitespace\" \
         \"))))(Tile((id 18afd06f-3f8d-4d0f-8357-87eefdddffea)(label(fun \
         ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 37))(sort Exp))))))(shards(0 \
         1))(children(((Secondary((id \
         b86738e9-4edc-4380-9d9d-91c2c1ac8bb2)(content(Whitespace\" \
         \"))))(Tile((id \
         84fac6d4-2823-49b4-bfd7-dca1f63a42d4)(label(_))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         975c22e9-7f4d-4477-ad8c-7033a5c7b019)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         7d0859d4-5c80-47f7-8d86-4b253e90aca8)(content(Whitespace\" \
         \"))))(Tile((id \
         d274f1c5-b3ec-48dd-a3da-bdd852f2905b)(label(plant))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))))))))))))(Secondary((id \
         2e3418d2-8fdb-4e38-b38a-504d769be0b4)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         fd5c0ab4-a910-435a-9fca-814217a19fc2)(content(Whitespace\"\\n\"))))(Secondary((id \
         eed672f3-42e7-42c6-9352-d74fbb3ffc7c)(content(Whitespace\"\\n\"))))(Tile((id \
         4e1535d2-e1cd-4c37-8904-f72c6bc57832)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         90b0dcf0-0bb7-42be-9351-5a68475c676c)(content(Whitespace\" \
         \"))))(Tile((id \
         77e52f98-02e3-49b7-9688-b0ba05dc93ee)(label(updateGrove))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         66c0ed62-6377-4d61-98c6-e91d68d54e6f)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 24))(sort Pat))((shape(Concave \
         24))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         3b5dbbae-a3bd-48b6-b16e-b2aa244c7d6a)(content(Whitespace\" \
         \"))))(Tile((id \
         386f52e1-fac5-4538-8466-fd66c23f502a)(label(\"(\"\")\"))(mold((out \
         Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0 1))(children(((Tile((id \
         9411b3cb-7307-4286-a9c5-abce3957d876)(label(Model))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         8ecadea6-5e10-4469-b5fc-6ef7ae639d23)(label(,))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 44))(sort Typ))((shape(Concave \
         44))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         f24686ff-9c2e-4a9f-9214-077ab1da389d)(content(Whitespace\" \
         \"))))(Tile((id \
         fac5bd10-30d6-4848-85ea-79dfa6310dd8)(label(Grove))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         27a3aec6-d86f-4a39-a6a7-e8d226bb970f)(content(Whitespace\" \
         \"))))(Tile((id \
         06811735-edb5-4435-ab83-675b83e693c6)(label(->))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 13))(sort Typ))((shape(Concave \
         13))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         504c2305-6859-490d-8eba-0868762e678c)(content(Whitespace\" \
         \"))))(Tile((id \
         7f89a027-5388-44e4-baf8-d7b4ca85cbf2)(label(Grove))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children()))))))))(Secondary((id \
         f22d4135-4ba5-41eb-a0ee-e3af2c04887d)(content(Whitespace\" \
         \"))))(Tile((id \
         2307051c-60e0-4886-ae11-49efee262ef2)(label(->))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 13))(sort Typ))((shape(Concave \
         13))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         399ff5ab-0a1f-4118-a3c4-cb3d66cd3be9)(content(Whitespace\" \
         \"))))(Tile((id \
         c7ceab70-e876-44f0-939a-d795bc6cf593)(label(Model))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         a0ed36ae-eb82-4918-8915-5104cf035299)(content(Whitespace\" \
         \")))))((Secondary((id \
         88de48dd-0ad3-4e4d-87b9-b2ae670d1297)(content(Whitespace\"\\n\"))))(Tile((id \
         be0c54e4-f527-416a-a888-1536fb0180e0)(label(fun ->))(mold((out \
         Exp)(in_(Pat))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         37))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         2f5f6f77-a5e6-488b-8149-225669642b55)(content(Whitespace\" \
         \"))))(Tile((id \
         965f7eaa-5eb9-4ec0-b13b-678d63d0c087)(label(\"(\"\")\"))(mold((out \
         Pat)(in_(Pat))(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0 1))(children(((Tile((id \
         68a29159-31a2-43f2-aed5-524eb7605f06)(label(m))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         590075f2-9770-4c35-a244-1a74a153e7ce)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 44))(sort Pat))((shape(Concave \
         44))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         64f3a8d3-bf2e-46c8-b265-f231c34d6368)(content(Whitespace\" \
         \"))))(Tile((id \
         b6dd908c-944a-4016-a0dc-ef5d3dea3478)(label(f))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children()))))))))(Secondary((id \
         ace6b0bc-817d-4a02-b7c5-6215be8acfec)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         2e467506-0930-4ffa-bd09-f083a6061efd)(content(Whitespace\" \
         \"))))(Tile((id \
         659bd3dd-8e83-4236-bfc4-cffa8033da14)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         d221298c-b589-4e2b-9218-443265939af3)(label(f))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         a85cd7e0-56ad-4b16-b2ee-4ec707d633e8)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         0e53b05a-2f82-4cfc-8934-9f7b368c50d3)(label(m))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         016f858f-eb30-4d56-850c-ebc8133bf0cf)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         5a71ddec-f66a-4979-999a-97bbdf831438)(label(grove))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         e7a9ffc2-8160-4694-b315-c6b544a986f9)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         f5ded231-70b4-48cd-b98e-ad434ddc231e)(content(Whitespace\" \
         \"))))(Tile((id \
         39a41e47-af74-4f10-890d-aa66f4309290)(label(m))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         adb35b97-5403-4cea-9b01-4fdbb397b9a7)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         003e59ac-96ba-4b22-b79d-de38e2c1a528)(label(currentSeed))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         dd532f5e-1fb6-4bc0-927c-eb5d7fe5f1fe)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         a201c1c5-cef3-4444-83ce-026778641cd2)(content(Whitespace\" \
         \"))))(Tile((id \
         a6812854-68e3-4450-bb2f-d09f77dc8ce8)(label(m))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         9048a482-b30a-4ba2-9729-fa6eb733c1e6)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         b570ae54-b883-4d3c-8273-3ae0c41c0da1)(label(seedInventory))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         bb377ac5-02dd-4d4e-8259-2ba335fe9931)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         cce136be-f1b1-4000-8e0a-0a8e09b13686)(content(Whitespace\"\\n\"))))(Secondary((id \
         8df877ea-ee44-42e5-bb05-41bcad230243)(content(Whitespace\"\\n\"))))(Tile((id \
         b55360a8-07c2-4d8e-b9f3-8716a0e2427b)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         20ac83a4-ab69-4280-ab6c-15133733f645)(content(Whitespace\" \
         \"))))(Tile((id \
         7266bdb5-5a5f-42c6-a9e0-8ce32c9ba260)(label(update))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         1d33df8e-9fad-4080-a4d9-f3f67c4ea45a)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 24))(sort Pat))((shape(Concave \
         24))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         3e93cf2c-ef1f-4bbc-93ee-357700e41b1d)(content(Whitespace\" \
         \"))))(Tile((id \
         b7db4d62-0ea3-4c84-8da5-90a296bcc3c9)(label(\"(\"\")\"))(mold((out \
         Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0 1))(children(((Tile((id \
         7bd2c4e5-7ed1-46ef-a527-ee17bcfe96e3)(label(Model))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         55a42082-bb3f-405d-bca4-a6b8144b6f9a)(label(,))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 44))(sort Typ))((shape(Concave \
         44))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         a1c9ee01-dd2a-4958-bd87-7c27a2905774)(content(Whitespace\" \
         \"))))(Tile((id \
         030b48db-bc86-4849-8377-4ce5d3da8145)(label(Action))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children()))))))))(Secondary((id \
         be6b4a5a-7dab-49ac-8e64-f04bb05ea34a)(content(Whitespace\" \
         \"))))(Tile((id \
         3878529f-63fa-48c5-b21f-d94f0c2e4a52)(label(->))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 13))(sort Typ))((shape(Concave \
         13))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         a2cda0dc-b453-49ec-9edb-71a498bb0a4f)(content(Whitespace\" \
         \"))))(Tile((id \
         de1455f8-7838-4742-b771-efb315423ed4)(label(Model))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         5e6982a1-c88d-469a-8b12-102b08b113e8)(content(Whitespace\" \
         \")))))((Secondary((id \
         c1400c86-5fd4-44ca-a9cc-b979be23cedc)(content(Whitespace\"\\n\"))))(Tile((id \
         6f8b2160-32e3-44c3-92d4-827c623cb881)(label(fun ->))(mold((out \
         Exp)(in_(Pat))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         37))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         e8abcd15-a277-4f90-b057-3737f57292a7)(content(Whitespace\" \
         \"))))(Tile((id \
         b5f2a10b-487b-4c88-a333-1b065c24e259)(label(m))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         b1c3f37a-3a6e-43ec-9369-ac5b3bad5f0e)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 44))(sort Pat))((shape(Concave \
         44))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         165beaa2-3a37-43f1-ad89-4a0a13735f89)(content(Whitespace\" \
         \"))))(Tile((id \
         6a2d38ba-fd9e-4277-a478-b4f41f264580)(label(action))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         e7230ac7-0791-4f00-a964-50460eeb14b3)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         a7557ce9-c2a7-4252-8d5b-ca6e34495a30)(content(Whitespace\"\\n\"))))(Tile((id \
         9972b362-8f95-4a93-aa5f-ba201a92378d)(label(case end))(mold((out \
         Exp)(in_(Rul))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         21e32e78-2ec1-4451-b737-b4f8c7236eff)(content(Whitespace\" \
         \"))))(Tile((id \
         3574dc69-8690-4c9b-8a32-030fd79c73da)(label(action))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         d694b2eb-605b-4c97-94b4-08febeea7c8b)(content(Whitespace\"\\n\"))))(Tile((id \
         cf974726-e2cf-458f-a862-06e5465d87f3)(label(| =>))(mold((out \
         Rul)(in_(Pat))(nibs(((shape(Concave 46))(sort Exp))((shape(Concave \
         46))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         c82fb279-2a27-477d-b8c5-9f0c74beabfe)(content(Whitespace\" \
         \"))))(Tile((id \
         71c40dd9-0f2b-481a-a345-369844a7b82b)(label(SelectSeed))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         75f9b87f-3edc-4e5e-932f-d5d667de4721)(label(\"(\"\")\"))(mold((out \
         Pat)(in_(Pat))(nibs(((shape(Concave 23))(sort Pat))((shape \
         Convex)(sort Pat))))))(shards(0 1))(children(((Tile((id \
         88d7b6bc-d821-4ac6-b975-5512712425f5)(label(idx))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children()))))))))(Secondary((id \
         08429670-11ce-4eae-91eb-865e551d9e32)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         c9dfcbe2-8592-4801-8864-6f7ef73ad26f)(content(Whitespace\"\\n\"))))(Tile((id \
         86b83f24-4eec-4db3-b735-531d7c58df73)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         c6ad3895-dcda-4ff6-b3c2-9adab7e64d67)(label(m))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         a911412d-c222-4f2f-80f8-5d05b959b86d)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         8a039738-d51a-40d7-a546-0b3c9004678f)(label(grove))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         42d86ee9-3b4b-4b55-9ee9-77bd1a5f8093)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         d1d14cd7-c990-4ad6-8786-16b61f9e8d3d)(content(Whitespace\" \
         \"))))(Tile((id \
         0baede10-1145-49b4-852d-f2a47868c321)(label(nth))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         06c3b6da-7e4e-4533-835f-e8e1dae4bf97)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         0bfd9bbf-c024-4f6a-9467-7b5e49d75260)(label(m))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         dc1d3ebd-e85b-40b6-bbd9-33fded25a8a9)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         50df65d3-d52c-4a2c-ac45-228103df8a4d)(label(seedInventory))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         7eab785d-9e5e-441f-a334-f283f86a8fc6)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         9d124742-e2be-4098-84f5-49d6cded5ca0)(content(Whitespace\" \
         \"))))(Tile((id \
         0de2ec81-5a56-4baf-bfb5-81e761827175)(label(idx))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         598e056d-af88-424b-8db6-30525b148340)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         d8f2a40d-3d96-4800-b241-72dbbe9a31f1)(content(Whitespace\" \
         \"))))(Tile((id \
         263a847d-9803-42b5-8278-b049e264efbf)(label(m))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         2930b4ce-4548-44ec-8557-7fa025e96f8f)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         839e9de5-c4a6-46bf-8259-e7f873fa896d)(label(seedInventory))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         605d3b78-9038-4bf4-9616-f34ab220c479)(content(Whitespace\"\\n\"))))(Tile((id \
         688345bf-d0fa-4032-85d7-a9aa76a8b064)(label(| =>))(mold((out \
         Rul)(in_(Pat))(nibs(((shape(Concave 46))(sort Exp))((shape(Concave \
         46))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         481ab4b4-ed73-4640-9fb2-e36bab217abc)(content(Whitespace\" \
         \"))))(Tile((id \
         dea7eb60-77fa-467d-bc91-99762307f3cd)(label(PlantSeed))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         873be3e2-c171-42b4-a6ce-453e0dc165e9)(label(\"(\"\")\"))(mold((out \
         Pat)(in_(Pat))(nibs(((shape(Concave 23))(sort Pat))((shape \
         Convex)(sort Pat))))))(shards(0 1))(children(((Tile((id \
         e60a214e-b518-4d08-ae08-6f68fb146d2e)(label(row))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         b3a80570-bb69-4b86-9427-bedb3e238bd7)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 44))(sort Pat))((shape(Concave \
         44))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         3746d15b-862c-4df8-a7c8-2c7626becb5b)(content(Whitespace\" \
         \"))))(Tile((id \
         62adffb5-bfca-493d-ab6f-c3eb3fb7be5a)(label(col))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children()))))))))(Secondary((id \
         a5452825-501a-454d-b6e8-f4c34d640365)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         cd726c40-9e9c-4d1d-bc5e-890165c9fee2)(content(Whitespace\"\\n\"))))(Tile((id \
         c3b25ee1-0e48-4463-84ed-bba279fbe566)(label(updateGrove))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         d4a0ad77-c3b5-41d8-9118-90df88075e3c)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         46577cd3-cfca-47e9-aabf-966c414f4caa)(label(m))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         cc137798-af42-4ae4-aff1-1f111208ea1a)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         4d2c6b76-20fd-451b-87bd-456f80ef15f5)(content(Whitespace\" \
         \"))))(Tile((id 178a3164-4872-457f-87a9-af95ab65512a)(label(fun \
         ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 37))(sort Exp))))))(shards(0 \
         1))(children(((Secondary((id \
         76c3a4c9-ab38-4be5-8528-e2e1de44fa73)(content(Whitespace\" \
         \"))))(Tile((id \
         d3b04207-2f6e-4c5b-8e61-f7af87bab247)(label(g))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         28994bab-b94e-4998-a943-eb66ed65f6b6)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         877d544b-e466-492f-81d1-c9fdc1918c3c)(content(Whitespace\" \
         \"))))(Tile((id \
         fd9850df-6397-4247-a5ef-7c2abd077224)(label(setCell))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         2254171f-1f85-41a2-8b1e-220123ba3f24)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         6913f721-d0f4-4e1d-b20d-9e692ea4d818)(label(g))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         fd132b8b-432a-484f-8450-a2f2713ea6f0)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         af0ba96d-9013-4ca5-bf61-82184860d173)(content(Whitespace\" \
         \"))))(Tile((id \
         228f4f93-cd7d-4109-a98c-93c7d48ac7b6)(label(row))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         b3339237-0624-4b70-b5d9-c612390b1408)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         c1140b07-3025-4874-87a9-3231d7352974)(content(Whitespace\" \
         \"))))(Tile((id \
         8b26ac54-f89a-456e-9742-26f13c41d9c6)(label(col))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         40a3fe3b-d713-498d-b75f-c6bdfb954160)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         7fa38a27-6239-4751-b12f-6274f9a9ae49)(content(Whitespace\" \
         \"))))(Tile((id \
         0b9d9bed-cf8e-40bd-a0d0-ab8bba34697c)(label(m))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         b6e768f9-e5e4-4edf-bc94-a31767af1e49)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         7955a5a0-2ef0-4337-a5a4-4ba35f7296a5)(label(currentSeed))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))))))))))))(Secondary((id \
         6b4831c0-b7d7-4ff3-b4fd-c737d7c13f1e)(content(Whitespace\"\\n\"))))(Tile((id \
         4b0b2018-c64e-43ba-bafa-8f395f1aff9b)(label(| =>))(mold((out \
         Rul)(in_(Pat))(nibs(((shape(Concave 46))(sort Exp))((shape(Concave \
         46))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         56a4fe42-9d09-4ba8-b76d-d06fb81be4a7)(content(Whitespace\" \
         \"))))(Tile((id \
         86bd3734-fce3-4f80-a3e0-a805df6c6f52)(label(Uproot))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         5f0dd9ac-73b2-4dbf-91dc-a1bff06236dd)(label(\"(\"\")\"))(mold((out \
         Pat)(in_(Pat))(nibs(((shape(Concave 23))(sort Pat))((shape \
         Convex)(sort Pat))))))(shards(0 1))(children(((Tile((id \
         f55d9564-2261-41bd-9ec4-c5f90d5353e5)(label(row))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         138467ec-f4ac-4c4c-9d90-47659e9bdfc3)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 44))(sort Pat))((shape(Concave \
         44))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         afbc386b-ecef-4992-a5a4-7c10b3c1212a)(content(Whitespace\" \
         \"))))(Tile((id \
         6b64cabc-24a3-4d6d-8c76-1ef34c6692c3)(label(col))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children()))))))))(Secondary((id \
         fe0e3197-d54b-4a79-90ab-34ccb97a1ba0)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         ae3c6d2a-9b56-4975-92a3-759bafe20cc1)(content(Whitespace\"\\n\"))))(Tile((id \
         3535db25-9752-4f27-94a3-7e7d4c1f2914)(label(updateGrove))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         00fdbe23-36da-4f49-b53f-035164d78a2e)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         1c606447-b425-4d88-a517-b495d1f57d54)(label(m))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         362f6539-c4ee-4aa3-8468-caee052de146)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         89c096c8-1d8c-4457-9aca-f965e6178a34)(content(Whitespace\" \
         \"))))(Tile((id 8f8714fe-07c2-4f5e-88bb-fa611153b004)(label(fun \
         ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 37))(sort Exp))))))(shards(0 \
         1))(children(((Secondary((id \
         945a5de1-7a80-47fc-be65-8f92d3247265)(content(Whitespace\" \
         \"))))(Tile((id \
         884979f1-c2f6-434e-a776-41e2ba4870c6)(label(g))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         e9f91b15-c2d1-44dc-9420-94e1a1dcb349)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         9332f267-a50c-41a3-b558-7efcbbf65c76)(content(Whitespace\" \
         \"))))(Tile((id \
         df5bfb60-70ed-49fb-89e0-d6c13baa29a4)(label(setCell))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         58cc8a0e-5c85-44c3-b83c-782ae1f14e50)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         b6c32523-42d8-479f-a16e-623a8062deb7)(label(g))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         520717e2-71b2-44c7-b860-cf5e0e34cc7a)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         e010706e-9ffb-48cc-8ebf-dcc6c9c488cf)(content(Whitespace\" \
         \"))))(Tile((id \
         5a1c4fc4-ba65-47f0-bbcf-e9b3f90435ce)(label(row))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         f693b3af-3fcf-4125-b572-16b9758481cc)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         61641118-16f7-4e87-949b-cc404c833517)(content(Whitespace\" \
         \"))))(Tile((id \
         693b0535-4c75-43d8-b9af-e75eff9c43a3)(label(col))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         3a9f866f-c8dd-4d4b-9b23-9ff64227bcac)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         74964090-2ced-48c6-86f3-293b0a179102)(content(Whitespace\" \
         \"))))(Tile((id \
         47e3dd29-2e95-4415-91b6-cf5949bbfa86)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))))))))))))(Secondary((id \
         9e273db0-e5b0-4f32-a9ea-8c588c121348)(content(Whitespace\"\\n\"))))(Tile((id \
         369d64d2-a0f1-4991-b91a-df743cbdf359)(label(| =>))(mold((out \
         Rul)(in_(Pat))(nibs(((shape(Concave 46))(sort Exp))((shape(Concave \
         46))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         7a8ce670-d95f-48ba-8183-8688c91a52d7)(content(Whitespace\" \
         \"))))(Tile((id \
         50657272-f938-47db-979f-9d34dcbcf5c3)(label(ClearGrove))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         b7acf5dc-d686-4fb3-aabe-cce51ed9d9db)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         b34d4040-b68a-40ad-a17c-0ba0c7c83a66)(content(Whitespace\"\\n\"))))(Tile((id \
         11577273-9951-46db-8ba4-907772c17d0e)(label(updateGrove))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         e65edebd-a3a3-4b02-ab27-fdafa220c6d6)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         b72e6faa-2740-4b2b-a43e-c93448d87582)(label(m))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         76e9bf04-5b00-446c-bbc5-5c5afd4ab689)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         243c0620-98a1-4465-b633-b290ae21806f)(content(Whitespace\" \
         \"))))(Tile((id 8272051c-7beb-4633-94fd-7769797217da)(label(fun \
         ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 37))(sort Exp))))))(shards(0 \
         1))(children(((Secondary((id \
         c08af2fc-c1d7-4601-9003-db7efe9fd0c7)(content(Whitespace\" \
         \"))))(Tile((id \
         340438a3-83c5-46e9-875e-958243978d90)(label(g))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         70d45dbe-7725-4bd1-959c-ebbca21261db)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         e2b0e3d2-3382-4224-bad2-51359ab6c9a7)(content(Whitespace\" \
         \"))))(Tile((id \
         593abf12-f237-4aac-9a1a-02ec57f56677)(label(setAll))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         ee1cbbea-a739-4254-a51b-9785147da080)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         39774c73-c0b6-4e05-bc26-d7851cd0ddc0)(label(g))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         bdd9f20d-72ef-4af6-8234-5901efd4eb91)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         7c5660fe-b87b-43d0-a90f-e1a41591e9d4)(content(Whitespace\" \
         \"))))(Tile((id \
         ff33c18f-31f2-40dd-a693-6315722ca80d)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))))))))))))(Secondary((id \
         28095609-4ce1-41da-a2ee-fe66581c672f)(content(Whitespace\"\\n\"))))(Tile((id \
         fb860ec3-8b81-42a0-bfd1-79edee68f019)(label(| =>))(mold((out \
         Rul)(in_(Pat))(nibs(((shape(Concave 46))(sort Exp))((shape(Concave \
         46))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         770d6362-30ce-4b08-8984-9ec6204f5fa7)(content(Whitespace\" \
         \"))))(Tile((id \
         706f3fc7-ccad-42ca-85f5-bd56c7b081aa)(label(PlantRow))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         90b32209-f09e-4260-8035-690fde6307ab)(label(\"(\"\")\"))(mold((out \
         Pat)(in_(Pat))(nibs(((shape(Concave 23))(sort Pat))((shape \
         Convex)(sort Pat))))))(shards(0 1))(children(((Tile((id \
         f348d6bb-176d-42fc-b448-aea1c37ab808)(label(row))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children()))))))))(Secondary((id \
         3232d4ab-3eb4-44c3-915e-7e0f67efc32c)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         d1e9c75d-78df-4a03-bc66-6b77fb4957bf)(content(Whitespace\"\\n\"))))(Tile((id \
         e5b941d1-4833-4394-a87e-2fc07f7eac98)(label(updateGrove))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         14b1d0b6-5827-4aa9-9c88-8a38ffae9821)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         6f1d2207-bbe5-41cc-88a4-5c4bc3d99080)(label(m))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         9ac3bf90-7568-477b-857c-ed501ac7513b)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         8c786c8f-ab3f-4768-b761-9fc63093fc12)(content(Whitespace\" \
         \"))))(Tile((id 715308c8-f12f-49e1-90d2-a043ec1e7354)(label(fun \
         ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 37))(sort Exp))))))(shards(0 \
         1))(children(((Secondary((id \
         4553170b-3505-4696-bfa4-77665dd5235d)(content(Whitespace\" \
         \"))))(Tile((id \
         768b4d37-e979-4a55-8a19-0ea7677efcd5)(label(g))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         1f78c08a-e252-43bc-9e5a-9af1fcad5c45)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         f226339a-a324-4f0d-a381-7c74e8038090)(content(Whitespace\" \
         \"))))(Tile((id \
         60de4287-e21f-4afc-8380-85ffa975b4ca)(label(setRow))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         8a82c21d-79fa-4ed3-89b6-eb1fa2df3422)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         18fc5716-31cc-4a5f-af29-c9a236d70f00)(label(g))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         c190fee5-d51d-4e60-895e-ec2f094b309d)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         948fd92d-b104-44e5-98fa-efa05b041994)(content(Whitespace\" \
         \"))))(Tile((id \
         3e20cef4-ff0e-4192-9911-c3b757030bd3)(label(row))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         e5cc334b-abca-49c2-86b7-794d8114e95e)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         cee90d81-7f48-41d5-8190-5d8facfdf5f8)(content(Whitespace\" \
         \"))))(Tile((id \
         79d6dbaf-59ef-40f4-bc91-41e1ed324660)(label(m))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         e78ee15a-20e2-4ac3-b41e-f2bef4137ac1)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         aa11f11c-3b72-460a-a407-af42397fe097)(label(currentSeed))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))))))))))))(Secondary((id \
         dd88813f-b8a4-434e-8e08-46cf4a48d1eb)(content(Whitespace\"\\n\"))))(Secondary((id \
         fcc47e67-dfb1-403d-8de2-6b6f209f164e)(content(Comment\"# TODO: Add \
         PlantCol case here #\"))))(Secondary((id \
         30f87442-3bbe-4143-892f-45acb5bee630)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         78384144-7e53-419a-8ee2-e597874879c0)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         ace1b1e8-67ab-47d6-87c2-7671006a3704)(content(Whitespace\"\\n\"))))(Secondary((id \
         7221848a-cf15-4b31-bcd2-37a7fd078629)(content(Whitespace\"\\n\"))))(Tile((id \
         2b34962d-27d5-41cc-948c-8fdf35ba7385)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         dd794461-1ce2-42c5-9e7f-fb9c9d764cf2)(content(Whitespace\" \
         \"))))(Tile((id \
         ad25d22f-4a8d-4cf1-add6-a6b625ae3870)(label(do))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         fc92692b-5361-4edf-8042-117de2abf81d)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 24))(sort Pat))((shape(Concave \
         24))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         b429332b-390e-4e02-a3c8-5dbe52426cb5)(content(Whitespace\" \
         \"))))(Tile((id \
         8cd8c78d-a5f7-41f1-a934-33258c6bb502)(label(\"(\"\")\"))(mold((out \
         Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0 1))(children(((Tile((id \
         ea2a59ff-f307-477c-9b2f-facef5fcd22d)(label(Model))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         3a5f98e7-edaf-4cb7-a675-f6ce87d68a68)(label(,))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 44))(sort Typ))((shape(Concave \
         44))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         d0e64481-1b48-455c-8721-33ef7922b522)(content(Whitespace\" \
         \"))))(Tile((id f59a9593-0e74-46b6-b6f9-41c5e30411ab)(label([ \
         ]))(mold((out Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape \
         Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
         7e738265-b5fe-4b8e-813c-6b701535c3d6)(label(Action))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))))))))))))(Secondary((id \
         f20d9840-d58d-4d34-99ca-9a5010af0204)(content(Whitespace\" \
         \"))))(Tile((id \
         051b5ff6-7f33-4238-8e78-000a56fd0b80)(label(->))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 13))(sort Typ))((shape(Concave \
         13))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         968df276-ec26-42ab-b99f-e1b345010b34)(content(Whitespace\" \
         \"))))(Tile((id \
         e002477d-2a70-491e-9e33-f89fec55c25a)(label(Model))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         089a25dd-dea9-44b1-beee-f41ab0516f6c)(content(Whitespace\" \
         \")))))((Secondary((id \
         cee9b4bf-f4d3-4d8b-9c18-5a2718434d0f)(content(Whitespace\"\\n\"))))(Tile((id \
         d1a4a838-412b-4062-8af3-6c19799f38fa)(label(fun ->))(mold((out \
         Exp)(in_(Pat))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         37))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         a9c515ad-ef19-4104-bee4-3ea30550a9a6)(content(Whitespace\" \
         \"))))(Tile((id \
         99fa157a-ca5b-4c8c-a035-08ba467c5d7f)(label(\"(\"\")\"))(mold((out \
         Pat)(in_(Pat))(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0 1))(children(((Tile((id \
         24b33821-0cbf-426f-a57c-ff3b11322119)(label(init))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         091c1e85-e71d-479b-b470-d50294f0812b)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 24))(sort Pat))((shape(Concave \
         24))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         1553453b-27e5-4598-a33b-e3d996793dcf)(content(Whitespace\" \
         \"))))(Tile((id \
         f3c9e137-58e7-4f1d-954d-0999d4fb039f)(label(Model))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         df5b86db-1771-4066-abf9-4099f6e8a0b7)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 44))(sort Pat))((shape(Concave \
         44))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         23a6bb26-6505-455a-9b33-d37d642e1614)(content(Whitespace\" \
         \"))))(Tile((id \
         cab9c88e-672b-4c6b-b35f-ee369fc05211)(label(actions))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         42fe1308-98b8-4e01-aba8-5db2ebd349a8)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 24))(sort Pat))((shape(Concave \
         24))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         af0656b1-2b64-4faf-8be5-d36e70507eb3)(content(Whitespace\" \
         \"))))(Tile((id c96f4cf8-8307-4a92-af62-42fd19132405)(label([ \
         ]))(mold((out Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape \
         Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
         19638b07-0da7-48ea-a51c-0c1f18e0f8bc)(label(Action))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))))))))))))(Secondary((id \
         faf337ad-b8a4-418f-b751-53fe07baa3f8)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         d4338b0c-d888-44e6-85c3-f2af71a87550)(content(Whitespace\"\\n\"))))(Tile((id \
         a2b5a620-395e-49fe-8d04-08d7a1ae515a)(label(fold_left))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         2a66a221-1862-4e85-bcca-9e227ab4bbf2)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         a5de55eb-ae0a-40a7-94c4-b6e00bdaa425)(label(actions))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         172256d7-0866-436b-bb0f-cc2b5d5d58c6)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         901df143-39cd-40ee-8aa4-0491c57a45d8)(content(Whitespace\" \
         \"))))(Tile((id \
         6a6bcac4-e4c4-41ee-9675-2240e6ad5a62)(label(update))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         dc3f2eaf-c4ee-443c-9c43-571c3b6beadd)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         822bdbc0-f6e7-4f67-add4-7018729049a2)(content(Whitespace\" \
         \"))))(Tile((id \
         7172ad84-b7b3-4da6-8dc8-859e7e84d2ea)(label(init))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         8925d915-36eb-42f6-aef0-e571208e0d4a)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         e12e16a7-ba5c-479a-9f4a-3dda59f2510e)(content(Whitespace\"\\n\"))))(Secondary((id \
         dacd7658-e08b-4c33-9b0c-7d3a12be45fd)(content(Whitespace\"\\n\"))))(Secondary((id \
         16e87b0b-f204-44e2-afa0-2833ae2dd921)(content(Comment\"# Existing \
         tests #\"))))(Secondary((id \
         ebb1ca5a-9601-4759-88c3-22ce92e68c29)(content(Whitespace\"\\n\"))))(Tile((id \
         f461d3db-b0d2-43e1-9eb8-e467b616b649)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         c6c1aa11-d89b-4bc6-8704-37961ece46f1)(content(Whitespace\"\\n\"))))(Tile((id \
         2d0a9634-7e71-4979-b47a-dfe5f7da0ffd)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         b73daec9-9e55-4b95-9149-3e88bb4024f2)(content(Whitespace\" \
         \"))))(Tile((id \
         8cfcf8f6-cc7a-4b47-8d48-e58f879d7480)(label(m))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         e2ff282d-4e7b-49a8-972d-2dd42a6052f0)(content(Whitespace\" \
         \")))))((Secondary((id \
         2a17eb1c-8d5f-45f0-882c-5e480d1abb93)(content(Whitespace\" \
         \"))))(Tile((id \
         fe364d22-b841-4d5a-b35a-b957ba5cc4fa)(label(update))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         922dcd68-95b4-4b0f-9d26-2b4a36c66207)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         0f5ae3b6-f962-4c9d-a06e-d85b21103d5a)(label(init))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         15e4a1f8-7b9e-4707-b15a-e52a41f7efa0)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         ad0fdfcb-5901-4634-9871-5787f3bdf291)(content(Whitespace\" \
         \"))))(Tile((id \
         2fc2a12c-d487-44be-9af3-2fee8ef44a5e)(label(PlantRow))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         73a1d87d-7c44-467d-98e4-6ac3cc646e66)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         d830b6d2-e704-4bbb-9fef-9da5d15d3a2a)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))))))))))))(Secondary((id \
         88fb7ddc-3cae-4004-942b-27783bc2de4f)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         9a180bc6-69a9-4598-be7b-2f3fe2320e67)(content(Whitespace\"\\n\"))))(Tile((id \
         2d3f0920-49fc-4242-b2da-f2dab1cf7dea)(label(m))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         71ce2b91-6378-4973-8600-ed4015481233)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         383e52f4-5078-4bc9-bbf7-02c0ef726107)(label(grove))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         4c182f47-825c-4c9e-9a9d-2079420da13e)(content(Whitespace\" \
         \"))))(Tile((id \
         ab1eb843-48b3-4dbb-8c5b-7e06f74b6fc7)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         7c63da8d-e861-4bb6-b21b-bb02687f7530)(content(Whitespace\" \
         \"))))(Tile((id 87c70711-e3c2-4e40-b874-fe75503630a0)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         cc28dcfd-d4b8-4976-925a-46ae389ff6b9)(label([ ]))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         874b7a42-3301-485f-9096-c0a2919db240)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         b7533039-ef31-41c5-ad61-1734f34dde42)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         7a442778-d5bf-4de0-9a88-204943feca1c)(content(Whitespace\" \
         \"))))(Tile((id \
         608b2af8-2c67-476d-8827-5acab90e3f21)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         85e961d5-9129-4179-a4b2-be169ab54f11)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         62b5beed-2307-4fc4-b760-e431fd09d5b1)(content(Whitespace\" \
         \"))))(Tile((id \
         f39287a0-8678-455f-b920-93551b922ed5)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         09e9befc-c3c0-4a0d-aa8c-898e2c4bc6ed)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         66282f08-2aca-4491-a8c0-1ec0808a1245)(content(Whitespace\" \
         \"))))(Tile((id e2a1c1d7-6e7c-4861-a760-0ed9bb1adfcd)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         3c1384af-7214-43d3-94d2-94989f31c3cd)(label(\"\\\"\\240\\159\\140\\177\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         4c57fdda-0506-4148-aa6f-d1dbd967e36e)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         5ef065a0-7adf-4d9e-a915-1fffb15c8833)(content(Whitespace\" \
         \"))))(Tile((id \
         a15ee9a4-9b27-4716-b62a-ee535ae4ab14)(label(\"\\\"\\240\\159\\140\\177\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         a5e833d1-ef0d-4162-bb30-93e9b198553a)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         eac1f925-97c3-4c31-8046-ead372006d01)(content(Whitespace\" \
         \"))))(Tile((id \
         a829e7e8-d3ee-472d-a50a-31d3d1c9ad98)(label(\"\\\"\\240\\159\\140\\177\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         65ad3c99-1ea2-4284-ae40-0a4d9d661fd6)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         aef5218e-f63f-4a74-8d0e-68f161bd1d5f)(content(Whitespace\" \
         \"))))(Tile((id 733aa7ff-f03d-4a00-8c27-736988daabb2)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         c9f7f0ae-b296-4edc-a63f-be33a03ed75e)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         ca498ace-b5e7-4b05-a8d2-eb47085e961c)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         132545b6-a7b4-4520-aaad-465707f961e1)(content(Whitespace\" \
         \"))))(Tile((id \
         734cb38a-ef36-417d-8c6b-3fa7f7322f7c)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         c5d96830-e1e2-4d03-9b8d-25c124650149)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         ce4b8204-bc2a-4272-a8ff-ff29c864d4fc)(content(Whitespace\" \
         \"))))(Tile((id \
         234eca9e-ecf0-46b3-8616-b8925d9f5674)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))))))))))))(Secondary((id \
         234c9a0c-529e-4945-8f19-cc9d3b191077)(content(Whitespace\"\\n\")))))))))(Tile((id \
         ed0e3729-0d93-45c0-a2b7-9a5bdc724637)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         c4bafcfe-2542-4585-bd57-5da25bf1d342)(content(Whitespace\"\\n\"))))(Secondary((id \
         94836d47-c8e0-40e2-bccf-cc2be56b5282)(content(Whitespace\"\\n\"))))(Secondary((id \
         b65000e5-231e-415c-b18e-bf22878f9399)(content(Comment\"# New tests \
         for PlantCol #\"))))(Secondary((id \
         60a83403-1247-4f95-bdea-6847900f3bdc)(content(Whitespace\"\\n\"))))(Tile((id \
         1f4b89df-c3d0-4551-8dad-35d559e781fe)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         2d0e7d57-50f2-4eac-a54e-24fed97991fb)(content(Whitespace\"\\n\"))))(Tile((id \
         3f2282c4-5fca-45d7-b146-c4f277b0bc1f)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         c8ff83ca-2825-4e1c-9b1e-1491a9b7c873)(content(Whitespace\" \
         \"))))(Tile((id \
         5a0d02c3-fd8d-47d2-9a61-4891e77ab534)(label(m))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         8f3a5d19-18b7-4141-ae98-c6897f5ba084)(content(Whitespace\" \
         \")))))((Secondary((id \
         15db67f6-1fef-4b78-99e8-96e98304ee16)(content(Whitespace\" \
         \"))))(Tile((id \
         d0e37da4-4d96-4baa-b058-b0c79610a583)(label(update))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         66dbb311-c5b1-4565-97f0-663a765a4a29)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         e0987256-b305-4eda-a8be-50f82af1713c)(label(init))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         664c5b30-bebb-4701-b086-accbff810c39)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         fcccf603-7458-4b3b-a7ed-15c8e2472851)(content(Whitespace\" \
         \"))))(Tile((id \
         a6ed8cd3-cae2-4054-a7d1-7f3e33c84cd0)(label(PlantCol))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         0c07f9b5-013d-45f2-84dc-7645ce89301b)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         10fcd9f2-9a9a-4843-85c1-75824768551a)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))))))))))))(Secondary((id \
         6617717e-0b62-49c9-8595-b9c42bed83f2)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         5b77f672-50ac-4bcd-83f0-2fe045843f46)(content(Whitespace\"\\n\"))))(Tile((id \
         23a26168-7525-47ac-a99a-817ac068212b)(label(m))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         a3f47ec3-0be3-4827-a9f5-45a2c5d516a1)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         00318f8e-157a-4221-9e82-ef619338dd6c)(label(grove))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         fb2e04ef-1756-424c-a640-3036d13e6c5e)(content(Whitespace\" \
         \"))))(Tile((id \
         a5ef1ca6-b10c-49a8-934e-d2f56d175918)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         6d505318-972e-42ca-be64-2f933cb0c758)(content(Whitespace\" \
         \"))))(Tile((id 453ac200-85cd-4de0-8f74-3148a2f9d05e)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         590486c2-df10-49d4-b2d3-7d2030f97e9d)(label([ ]))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         dfc924f8-272f-4f82-b61c-8ed7bb328ef7)(label(\"\\\"\\240\\159\\140\\177\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         a4a25bf7-f6fd-49ba-b56f-fb3ba70b0aa0)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         33f05cbc-c2e4-413e-9046-36b8f9424b59)(content(Whitespace\" \
         \"))))(Tile((id \
         a36b1f87-d1eb-4cf7-b59e-850eb1abf246)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         6577cf5d-9027-4310-92f2-0fe68dd2397b)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         2897ca5b-c4a1-44a4-98b1-41816337cc1d)(content(Whitespace\" \
         \"))))(Tile((id \
         cf720767-9668-47df-aaec-a73dd62b6b65)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         835848f8-b7af-473d-a19a-46b5a08bb25b)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         d2cc43ae-c290-448e-91b1-32455c44e303)(content(Whitespace\" \
         \"))))(Tile((id ec59c797-f132-41a8-b0bc-762cd1c04b8d)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         2a307444-74e5-4b2d-94d4-c5e1a92efbc2)(label(\"\\\"\\240\\159\\140\\177\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         afc1815c-d679-4bb5-9dc9-53b4ce9e4e18)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         26338f05-c6d6-4695-a845-895045e4833a)(content(Whitespace\" \
         \"))))(Tile((id \
         67dea3ac-7972-49fc-bfb9-3959e073ddfb)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         d93cee45-716a-44fe-9e17-a897dc526416)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         e5cbddca-b255-4856-9a00-8a94dbf2fb65)(content(Whitespace\" \
         \"))))(Tile((id \
         30387450-6634-4c19-8723-a20c84a8a586)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         045001c6-98ed-45e7-b8cf-8f7e2a357dfe)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         a5b0adb2-a86c-4273-a632-277c3d4cb854)(content(Whitespace\" \
         \"))))(Tile((id 51a7a708-e382-4a80-84c4-0328501115ce)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         77186e49-2df2-4ddc-99b3-7d61baa6e5e3)(label(\"\\\"\\240\\159\\140\\177\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         017e7ef2-d31b-4165-ae5a-0b74d3a47028)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         605b97bf-7409-46b5-bba2-d9d1dd64f5c1)(content(Whitespace\" \
         \"))))(Tile((id \
         26e353ed-d88e-4c0d-9bc4-a1adcc605d15)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         bf4fac0d-9445-458d-91ae-8d1dfe940e25)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         3700784e-9cad-415d-bfc1-b5e087641dcb)(content(Whitespace\" \
         \"))))(Tile((id \
         b46791bf-a8b0-45b0-af11-fd84a892f6c7)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))))))))))))(Secondary((id \
         5a2cbca6-eb48-4cda-a2a6-a54b1fde6cd4)(content(Whitespace\"\\n\")))))))))(Tile((id \
         d6394629-11e6-414b-ad75-08703b50dc8d)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         1564e37c-6ee5-494d-b3b6-cf6d32cceaf8)(content(Whitespace\"\\n\"))))(Secondary((id \
         d2ae8c0e-2c3f-46a9-9f40-a40d04880b74)(content(Whitespace\"\\n\"))))(Tile((id \
         ec64c780-199f-4af4-bb7b-63ae09f201ed)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         196dc9e9-e115-4ef9-a3d3-9bc0eaee2874)(content(Whitespace\"\\n\"))))(Tile((id \
         ecb4a656-cb68-4aae-884f-9ff6c17ec3b1)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         bbef9c52-5fe4-46b9-8262-22b24461c3e1)(content(Whitespace\" \
         \"))))(Tile((id \
         82f815fc-10a1-4180-91b1-372f6eadb312)(label(m))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         2733e5de-16bc-4a9a-87aa-fad9b4cff619)(content(Whitespace\" \
         \")))))((Secondary((id \
         46a66a6b-0193-4876-b100-848e24d65673)(content(Whitespace\" \
         \"))))(Tile((id \
         91ce9b38-ad46-4318-af72-cd0ea2e2034f)(label(update))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         3b8777d3-4c48-4951-b612-f4da606dc2d7)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         ccd4e584-0ef9-41c8-9928-8f274b165dde)(label(init))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         eb36c69a-2c79-454b-9747-c561cf18dbc4)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         d2ce9d8e-d681-4778-8078-62556803da75)(content(Whitespace\" \
         \"))))(Tile((id \
         c640dab2-b389-4090-ac43-e6c15897d9fc)(label(PlantCol))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         c1a6a0c9-b2c5-4bab-bbc7-5d3a013396e6)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         193b0d34-98d8-4b1f-b23e-edd7a980a6d8)(label(2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))))))))))))(Secondary((id \
         a9395aac-3f8b-4fbc-a811-aa99737a7f44)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         5fb254b9-5fe9-4fd5-8ed7-a0399982a6b5)(content(Whitespace\"\\n\"))))(Tile((id \
         18916473-1e8b-4bff-8b43-2e8939f32528)(label(m))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         9f3eda0b-a484-4dda-8a76-6258d488013d)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         9d337857-e5e0-4f9c-b7ad-94cdf94a047e)(label(grove))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         8641a631-497e-4ef8-9db1-4321a784c5df)(content(Whitespace\" \
         \"))))(Tile((id \
         0cb58e59-5f12-4ace-9e6d-92314e3c918d)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         2fd88350-2b68-4b33-9081-3fc92d548e19)(content(Whitespace\" \
         \"))))(Tile((id 44dfb6a9-8d08-4496-9b80-5fc2c665633b)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         30a86d15-f865-448b-a2a1-eb8430ed5c45)(label([ ]))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         aa32ae6e-4730-4777-b5e5-ff6048bfed46)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         bd50d61e-953c-4393-b84e-a3cc4e292997)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         ddaafc12-75ed-45ec-8894-437e2dbcaaf7)(content(Whitespace\" \
         \"))))(Tile((id \
         e17ad470-8caf-4c21-aafe-3e3af65a11e3)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         cd8d4509-ba5e-43c5-b570-2accb2b02c8d)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         bc846196-8f7d-4f9e-b59a-78eb8b37b2da)(content(Whitespace\" \
         \"))))(Tile((id \
         c2430f0c-499f-478e-af24-677ae0c6eb98)(label(\"\\\"\\240\\159\\140\\177\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         b63b6db5-04a7-4474-8cd1-025b93d9ca32)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         66e83e89-8856-4022-a7b4-e0d93ff56eec)(content(Whitespace\" \
         \"))))(Tile((id ab373f94-1d98-4f3d-8529-57dd2d443963)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         6774cc98-ab24-465f-a800-f76a2abecc3e)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         7d6b088e-0899-42f2-bffd-c61e50aec1d3)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         dfa8ac85-89a9-4571-bfce-60079597a9da)(content(Whitespace\" \
         \"))))(Tile((id \
         c64f8d97-e0a3-4e26-addb-6efcb15691eb)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         36a1021c-919c-46c8-a725-eeed3233aab7)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         f36e934b-0236-4ebe-ace3-ad365321d926)(content(Whitespace\" \
         \"))))(Tile((id \
         e3554b6e-3f8d-4e76-ac59-0cfcdc9a0f36)(label(\"\\\"\\240\\159\\140\\177\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         1e121e4d-bf5a-4b0f-bf7a-6961ea4019fd)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         8ca5d77c-a593-438e-8282-939a4f3bfbe9)(content(Whitespace\" \
         \"))))(Tile((id c6423468-eef7-42b6-89e3-01ca5397c7da)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         7276a8b8-d85d-4aa4-8b64-348cee868592)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         8c86977f-71de-4b39-a2cf-0d3b17c38875)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         c0fa1059-1887-474e-a898-3c38688b4d7a)(content(Whitespace\" \
         \"))))(Tile((id \
         8200520e-08a5-436f-a246-4a128d798135)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         514ee98f-1cc0-4a18-933d-c9ceec3cd350)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         22a80280-47b1-47be-aff9-e80740169972)(content(Whitespace\" \
         \"))))(Tile((id \
         8ed42d47-0474-4fcf-9fa6-6d767046d8de)(label(\"\\\"\\240\\159\\140\\177\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))))))))))))(Secondary((id \
         fefeac54-01bf-4cd4-8f18-be84097a9e54)(content(Whitespace\"\\n\")))))))))(Tile((id \
         bb7367b5-8210-43b8-b354-0fe199e03e3f)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         8a402794-4456-4af7-a0db-33f4524e3077)(content(Whitespace\"\\n\"))))(Secondary((id \
         effdbd47-ae37-4f0c-aa86-f4371059e16a)(content(Whitespace\"\\n\"))))(Tile((id \
         757cb415-c3cf-462b-a834-29dfbb90774d)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         646e3ce8-f10c-4b36-baf0-bdfeb271454d)(content(Whitespace\"\\n\"))))(Tile((id \
         f079e6e5-a7f7-4006-9f48-c4bc94c21dc5)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         502f53af-96c2-4287-b084-19fd25da47d6)(content(Whitespace\" \
         \"))))(Tile((id \
         db713843-08d1-492a-9070-0658bc2b7f4e)(label(m))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         8edc35a4-869b-4b39-9fc5-25b9808ef072)(content(Whitespace\" \
         \")))))((Secondary((id \
         56c2c476-253a-4122-9b58-c6b1b418c72d)(content(Whitespace\" \
         \"))))(Tile((id \
         d60ea2a9-8ad4-46af-ab6a-37afffda214c)(label(do))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         cab37ed4-0c56-4d53-ad54-8dc2f6f9d615)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         4164ef0a-30fb-423b-9dbb-e16462dc3dba)(label(init))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         92ea6483-2ceb-4728-a146-fbd57902fac5)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         a3d0d18a-6951-4327-b102-b18efceff17d)(content(Whitespace\" \
         \"))))(Tile((id a7b822f4-4caf-49ec-a46b-9f56276ab0a2)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         2b96417a-6f6f-4cee-9eda-5b4b366e2c6d)(label(PlantRow))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         2e307ae7-3afb-4d85-b334-e74629c861dc)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         00339d04-857b-4261-a168-06376246b82d)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         059a33f4-310a-49ab-88b2-60cd25cabdd4)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         f67f8e12-cb9c-4eb7-ac15-ac966a838936)(content(Whitespace\" \
         \"))))(Tile((id \
         61a4cabb-da03-4002-a102-2d761a49b1b1)(label(PlantCol))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         9cb83f89-da68-4c28-b018-6b32ef1d42de)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         cb418cd0-8e1a-40fd-931a-4cb25840934a)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))))))))))))(Secondary((id \
         80fcedd2-2fa2-4dd3-82b6-b12940a8098d)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         151e415b-ea92-43e8-969e-b3743dcfd2f2)(content(Whitespace\"\\n\"))))(Tile((id \
         5bb32d2d-08ab-4d21-a708-51ce28ac9f80)(label(m))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         c85c07b9-e395-4e3f-ad38-f826272d660c)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         f32d756b-f574-4ab3-80a4-afbf1e917fdf)(label(grove))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         463045cb-4c5b-4166-8bcd-0acd3ce50145)(content(Whitespace\" \
         \"))))(Tile((id \
         ad34ef85-57e6-4b8b-a16a-b8f0e201d9f2)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         3d96edca-2cb3-43ed-afd2-af03cdfb35c0)(content(Whitespace\" \
         \"))))(Tile((id 625d4684-4a2c-4251-907e-0b0c34a0c5e7)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         7f28dcf9-4686-4dc2-a094-9f8fd9f0291a)(label([ ]))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         b93e5743-3814-425a-b29f-f31e323d18d7)(label(\"\\\"\\240\\159\\140\\177\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         be080474-5a91-40df-94fb-0a29cdfa1d2f)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         19894d19-cdec-4c30-bf06-a813d124b4a0)(content(Whitespace\" \
         \"))))(Tile((id \
         953ff90e-e672-4cba-b887-9ec573456496)(label(\"\\\"\\240\\159\\140\\177\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         9e209837-0ea8-4b99-9af9-0e8783ddcc87)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         a5883188-fd3f-4757-a0b2-42b7b0473cf9)(content(Whitespace\" \
         \"))))(Tile((id \
         84e0ac76-6693-4290-8ef3-41dd8edb97c0)(label(\"\\\"\\240\\159\\140\\177\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         4376731d-1172-4025-9636-ded7cf95b7d2)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         c5190802-1ede-4946-ab3b-9b84c50106c1)(content(Whitespace\" \
         \"))))(Tile((id a172d841-0480-44b8-ad6e-59b5e34616b8)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         2deafa0c-7b52-4eac-82c5-94656bf36571)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         2c44454c-1fc4-4920-ade4-210ca333767f)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         2c1a85e8-3db4-42ce-9a06-ab3a00516bc9)(content(Whitespace\" \
         \"))))(Tile((id \
         a4485392-834d-4d95-aab3-827a97ee9619)(label(\"\\\"\\240\\159\\140\\177\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         a421cbe1-2494-4927-9694-fc97ad46c2a6)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         c9062bd9-510a-4dcb-ad60-787d32a005e3)(content(Whitespace\" \
         \"))))(Tile((id \
         3f9d54fb-c4e1-4b1d-8a29-4dd1c32ba009)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         b30f5df6-79f2-4ebd-b481-0c753c6f43e1)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         da8d12a1-a96c-4317-b9f6-ba57386dc31d)(content(Whitespace\" \
         \"))))(Tile((id 4eab9e94-82da-4f9a-8f86-6cc3ca6c9215)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         4bf8e154-3a02-4c0c-8042-b6f7b071a078)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         1ab6218a-0159-4f87-91fc-cf4668092497)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         4c456b16-764d-405b-8f72-3b665e60db81)(content(Whitespace\" \
         \"))))(Tile((id \
         0578f0b1-4bc2-4365-94a5-8825c160b5da)(label(\"\\\"\\240\\159\\140\\177\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         ef7e10dd-0361-4812-bda9-32de3c07053f)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         31f16bfb-cd27-4022-ab36-df155ac0f080)(content(Whitespace\" \
         \"))))(Tile((id \
         57530d48-01c7-47f2-be7f-6896e8916b8f)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))))))))))))(Secondary((id \
         b567f637-4510-4c51-aaeb-7bad6ca9146c)(content(Whitespace\"\\n\")))))))))(Tile((id \
         f6684c85-8b3b-4b02-b553-6f87583e454e)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         8b5018e9-4e5c-4ddf-8af9-a94a8aec8b4d)(content(Whitespace\"\\n\"))))(Secondary((id \
         9f8aa16d-95c9-416e-b10a-f50e164d2bee)(content(Whitespace\"\\n\"))))(Tile((id \
         4f10553f-9b1c-45e9-be9a-0ca31570a244)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         0026eb00-2aad-40b7-838c-1b6cb2b52a59)(content(Whitespace\"\\n\"))))(Tile((id \
         b12b34eb-f4fc-43c6-8448-9568c76447a3)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         465378ea-e5e7-4c89-9ef7-dc743dd4a6cd)(content(Whitespace\" \
         \"))))(Tile((id \
         7f83cae2-d05b-4951-9280-6436d6bf8892)(label(m))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         23c8f1f8-074c-402e-83da-d9d889657304)(content(Whitespace\" \
         \")))))((Secondary((id \
         7d17bdc8-7834-4013-8631-ef4954214678)(content(Whitespace\" \
         \"))))(Tile((id \
         eef7164c-e202-458b-ba1b-61dcccecefa4)(label(do))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         dc03ae9f-f0b2-43ce-8223-4601f104d162)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         25528e4a-fd12-42ea-944e-27e3c16a6a50)(label(init))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         473ba80d-1664-4405-9b85-e987d0d8dcc7)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         5bbc3b01-f4e2-4d25-ab15-db0e5cdd6e72)(content(Whitespace\" \
         \"))))(Tile((id c948a7b0-9c1d-468f-913b-0b4e28ee2e9e)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         6bba3499-bf25-471d-89e1-a73298555a4d)(label(SelectSeed))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         e834c0a3-2eb2-4911-a861-0d97b237c8d5)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         7a286963-c920-46da-9a4c-d0b80d140347)(label(2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         a9f4a807-cc1d-42fa-bd52-04a2a4282807)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         fafd6aa6-69f1-468e-acd1-66ebdbb39659)(content(Whitespace\" \
         \"))))(Tile((id \
         a12f3f63-5c2f-4c83-be3a-4fe0db8fcbd8)(label(PlantCol))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         f06526f5-d7e3-4d36-92fa-0fcc8d49cc97)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         3a22920c-f879-4673-8838-aaadb43f44ab)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))))))))))))(Secondary((id \
         4d775010-3e37-4c52-9fd6-5c7848260ff3)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         22bad8da-72be-4cd7-a367-0d034daca583)(content(Whitespace\"\\n\"))))(Tile((id \
         6f6a6b6d-cbc5-4ca5-a366-22c4ba6fb0a3)(label(m))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         d618b653-4762-44e6-8b99-9710d5794efe)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         4e47406c-4265-4eb0-8c28-b5601bd51ac7)(label(grove))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         88668889-62a2-4807-a154-d032ffceee19)(content(Whitespace\" \
         \"))))(Tile((id \
         05e57d61-d122-4596-a810-abdd6c689dd1)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         3bf4d5f4-4d07-466e-92db-ce088ef6664d)(content(Whitespace\" \
         \"))))(Tile((id 85d8e6b0-7837-4e1b-9461-ab8d46aaeadf)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         7c20d71e-0c6c-4c18-9798-51443018627f)(label([ ]))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         7985433f-0fba-440c-9e76-a9baa12968b0)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         59dc28cc-6e49-4e54-aa27-daa30e738c1b)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         cea7e794-311a-420c-9e0d-19ca56e03606)(content(Whitespace\" \
         \"))))(Tile((id \
         244349dc-b2dc-43ca-882e-ab0031858696)(label(\"\\\"\\240\\159\\141\\132\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         275fb390-f140-4b32-857a-34ed2167f6a5)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         2896f22b-8b31-4998-aae8-d0d6ed96edb1)(content(Whitespace\" \
         \"))))(Tile((id \
         67c8a970-7f03-4980-9cbc-13e42c297924)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         62cb5ba1-7643-4efb-8fb9-256f91ffabda)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         c0b1e6e0-a904-4f26-96ef-e6c645ca45f9)(content(Whitespace\" \
         \"))))(Tile((id ba5f1753-5811-4f57-9f10-7f8bca0af0d9)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         9f9410e5-ebb5-4e4a-854d-e81bb524f6f4)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         d841fa6f-cd1f-4104-a25a-d2d7660a238e)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         7066c1c8-7ee5-437f-ad51-67be48725f5f)(content(Whitespace\" \
         \"))))(Tile((id \
         de30a34d-5523-46e6-b0fb-6c325e318240)(label(\"\\\"\\240\\159\\141\\132\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         91254eb5-833a-4eaf-b911-d2d592ae6417)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         852ce9d4-8efe-4b29-b8cb-b46eba0576ea)(content(Whitespace\" \
         \"))))(Tile((id \
         b8685df7-44a2-4dd1-a51c-df4cc6211791)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         2a7e695e-48dc-4b64-b0f8-608dd7e45ed0)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         7d2b2585-c8c2-4d6a-b869-d471b5f1a8e4)(content(Whitespace\" \
         \"))))(Tile((id d5c5bf73-d81b-4e05-b095-017d88a137a3)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         ceb1bfe6-3bb7-4d8a-bf86-e568ed39ba26)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         49bbedb0-9cb3-43db-92d5-d7285294aa64)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         6b55fe10-642e-4e81-bf91-80ab34580831)(content(Whitespace\" \
         \"))))(Tile((id \
         ea460564-1a8a-4446-bfe3-233ab81a9c87)(label(\"\\\"\\240\\159\\141\\132\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         79e3bab1-4054-4742-9bac-7dcaf84a82a6)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         38bc384f-e5c0-4d56-9d01-76338cd5adbd)(content(Whitespace\" \
         \"))))(Tile((id \
         9edadfaa-5de4-4435-9535-f3803f8fc959)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))))))))))))(Secondary((id \
         6154ae85-c14a-4811-81a1-a3cf94d4f3a8)(content(Whitespace\"\\n\"))))(Tile((id \
         20e7c255-9467-4f65-ad8a-a3dcbab1d6ac)(label(&&))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 32))(sort Exp))((shape(Concave \
         32))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         201c4d39-fea8-463b-a6b9-914e77297fa0)(content(Whitespace\" \
         \"))))(Tile((id \
         109e237b-49d4-47d3-8a7e-c37d727682e9)(label(m))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         78335fc6-8eab-4673-b65c-32e61ddd13b3)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         8521291f-bf18-4002-a405-9d16b096296a)(label(currentSeed))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         c476c740-2f4d-4105-acd5-e8ed1c566a49)(content(Whitespace\" \
         \"))))(Tile((id \
         040c7204-6469-46a9-adc3-bd8bef6014c3)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         4fdbf018-aca0-491b-bd0d-fb5ef961e125)(content(Whitespace\" \
         \"))))(Tile((id \
         b00289c3-f084-465d-b479-50b8ac109962)(label(\"\\\"\\240\\159\\141\\132\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         63a0c763-c0b9-42ac-b7ee-45188490b34d)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         ec535ec1-6ef1-43bf-8a63-112a99f96054)(content(Whitespace\"\\n\")))))";
      backup_text =
        "# CROP PLOTTER EXTENSION TASK                     #\n\
         #                                                 #\n\
         # The crop plotter app lets you plant seeds on    #\n\
         # a grid. It already supports planting rows.      #\n\
         #                                                 #\n\
         # YOUR TASK: Add a PlantCol action that fills     #\n\
         # an entire column with the current seed.         #\n\
         #                                                 #\n\
         # You need to:                                    #\n\
         #   1. Add PlantCol(Col) to the Action type       #\n\
         #   2. Add a setCol helper function               #\n\
         #   3. Handle PlantCol in the update function     #\n\
         #                                                 #\n\
         # Look at how PlantRow is implemented for         #\n\
         # guidance - PlantCol is similar but vertical.    #\n\
         #                                                 #\n\
         # Tip: Use auto-probe to see how the grove        #\n\
         # changes after each action.                      #\n\n\
         type Plant = String in\n\
         type Grove = [[Plant]] in\n\
         type Row = Int in\n\
         type Col = Int in\n\n\
         type Model = (\n\
         grove = Grove,\n\
         currentSeed = Plant,\n\
         seedInventory = [Plant]\n\
         ) in\n\n\
         type Action =\n\
         + SelectSeed(Int)\n\
         + PlantSeed(Row, Col)\n\
         + Uproot(Row, Col)\n\
         + ClearGrove\n\
         + PlantRow(Row)\n\
         # TODO: Add PlantCol(Col) here #\n\
         in\n\n\
         let init: Model = (\n\
         grove = [\n\
         [\"\", \"\", \"\"],\n\
         [\"\", \"\", \"\"],\n\
         [\"\", \"\", \"\"]\n\
         ],\n\
         currentSeed = \"\240\159\140\177\",\n\
         seedInventory = [\"\240\159\140\177\", \"\240\159\140\191\", \
         \"\240\159\141\132\", \"\226\152\152\239\184\143\", \
         \"\240\159\140\184\"]\n\
         ) in\n\n\
         let setCell: (Grove, Row, Col, Plant) -> Grove =\n\
         fun grove, row, col, plant ->\n\
         mapi(grove, fun (i, r) ->\n\
         if i == row\n\
         then mapi(r, fun (j, c) -> if j == col then plant else c)\n\
         else r)\n\
         in\n\n\
         let setRow: (Grove, Row, Plant) -> Grove =\n\
         fun grove, targetRow, plant ->\n\
         mapi(grove, fun (i, row) ->\n\
         if i == targetRow\n\
         then map(row, fun _ -> plant)\n\
         else row)\n\
         in\n\n\
         # TODO: Add setCol helper here #\n\
         # Hint: You need to modify each row, changing #\n\
         # only the cell at the target column.         #\n\n\
         let setAll: (Grove, Plant) -> Grove =\n\
         fun (grove, plant) ->\n\
         map(grove, fun row -> map(row, fun _ -> plant))\n\
         in\n\n\
         let updateGrove: (Model, Grove -> Grove) -> Model =\n\
         fun (m, f) -> (f(m.grove), m.currentSeed, m.seedInventory)\n\
         in\n\n\
         let update: (Model, Action) -> Model =\n\
         fun m, action ->\n\
         case action\n\
         | SelectSeed(idx) =>\n\
         (m.grove, nth(m.seedInventory, idx), m.seedInventory)\n\
         | PlantSeed(row, col) =>\n\
         updateGrove(m, fun g -> setCell(g, row, col, m.currentSeed))\n\
         | Uproot(row, col) =>\n\
         updateGrove(m, fun g -> setCell(g, row, col, \"\"))\n\
         | ClearGrove =>\n\
         updateGrove(m, fun g -> setAll(g, \"\"))\n\
         | PlantRow(row) =>\n\
         updateGrove(m, fun g -> setRow(g, row, m.currentSeed))\n\
         # TODO: Add PlantCol case here #\n\
         end\n\
         in\n\n\
         let do: (Model, [Action]) -> Model =\n\
         fun (init: Model, actions: [Action]) ->\n\
         fold_left(actions, update, init)\n\
         in\n\n\
         # Existing tests #\n\
         test\n\
         let m = update(init, PlantRow(1)) in\n\
         m.grove == [[\"\", \"\", \"\"], [\"\240\159\140\177\", \
         \"\240\159\140\177\", \"\240\159\140\177\"], [\"\", \"\", \"\"]]\n\
         end;\n\n\
         # New tests for PlantCol #\n\
         test\n\
         let m = update(init, PlantCol(0)) in\n\
         m.grove == [[\"\240\159\140\177\", \"\", \"\"], \
         [\"\240\159\140\177\", \"\", \"\"], [\"\240\159\140\177\", \"\", \
         \"\"]]\n\
         end;\n\n\
         test\n\
         let m = update(init, PlantCol(2)) in\n\
         m.grove == [[\"\", \"\", \"\240\159\140\177\"], [\"\", \"\", \
         \"\240\159\140\177\"], [\"\", \"\", \"\240\159\140\177\"]]\n\
         end;\n\n\
         test\n\
         let m = do(init, [PlantRow(0), PlantCol(1)]) in\n\
         m.grove == [[\"\240\159\140\177\", \"\240\159\140\177\", \
         \"\240\159\140\177\"], [\"\", \"\240\159\140\177\", \"\"], [\"\", \
         \"\240\159\140\177\", \"\"]]\n\
         end;\n\n\
         test\n\
         let m = do(init, [SelectSeed(2), PlantCol(1)]) in\n\
         m.grove == [[\"\", \"\240\159\141\132\", \"\"], [\"\", \
         \"\240\159\141\132\", \"\"], [\"\", \"\240\159\141\132\", \"\"]]\n\
         && m.currentSeed == \"\240\159\141\132\"\n\
         end\n";
      refractors = "()";
    } )
