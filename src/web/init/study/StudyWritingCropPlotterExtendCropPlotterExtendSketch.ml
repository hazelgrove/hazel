let out : string * Haz3lcore.PersistentSegment.t =
  ( "Study / writing / crop-plotter-extend / crop-plotter-extend-sketch",
    {
      segment =
        "((Secondary((id \
         fc6c660c-587b-4828-a829-8d22f6b38327)(content(Comment\"# CROP PLOTTER \
         EXTENSION TASK                     #\"))))(Secondary((id \
         be0d89ac-1ff5-4415-90c8-07ebbb10e44f)(content(Whitespace\"\\n\"))))(Secondary((id \
         af7dd4c4-945e-47b0-a3c1-4db551bec24f)(content(Comment\"#                                                 \
         #\"))))(Secondary((id \
         421de011-908c-4f51-98f3-fed1776829d0)(content(Whitespace\"\\n\"))))(Secondary((id \
         56af582c-6310-43db-ac7e-abcab34066c7)(content(Comment\"# The crop \
         plotter app lets you plant seeds on    #\"))))(Secondary((id \
         52b17903-caaa-4165-8a47-91770d18cb77)(content(Whitespace\"\\n\"))))(Secondary((id \
         864f305b-37ce-4650-87b9-0f259d26ce64)(content(Comment\"# a grid. It \
         already supports planting rows.      #\"))))(Secondary((id \
         542448dd-9f01-490d-83fc-14d5032ea6d8)(content(Whitespace\"\\n\"))))(Secondary((id \
         d65e17fa-9d59-419f-8205-64487728ddea)(content(Comment\"#                                                 \
         #\"))))(Secondary((id \
         7211814a-196b-4368-8bf9-68d7deb7b7f5)(content(Whitespace\"\\n\"))))(Secondary((id \
         69f0995e-0b01-4770-bc59-4460b1480660)(content(Comment\"# YOUR TASK: \
         Add a PlantCol action that fills     #\"))))(Secondary((id \
         141d8d76-27bb-4397-8baf-36154eea661f)(content(Whitespace\"\\n\"))))(Secondary((id \
         e320c60b-5fbd-4527-85ff-d44ea44a7d8d)(content(Comment\"# an entire \
         column with the current seed.         #\"))))(Secondary((id \
         87408938-27c5-46dc-a9b3-6705e947bf4e)(content(Whitespace\"\\n\"))))(Secondary((id \
         9e566b5d-b22c-43bc-a9a1-37b2fd7ac4b9)(content(Comment\"#                                                 \
         #\"))))(Secondary((id \
         c048d191-26d0-4f77-bf8d-17afb1a74cef)(content(Whitespace\"\\n\"))))(Secondary((id \
         d9f184e6-db8b-4dc9-ae2a-0d88c0ed7abe)(content(Comment\"# You need \
         to:                                    #\"))))(Secondary((id \
         a845c9ef-04fe-4775-92f7-c6315ea5db76)(content(Whitespace\"\\n\"))))(Secondary((id \
         5c04db9a-ba31-42b7-9b92-7947a2002671)(content(Comment\"#   1. Add \
         PlantCol(Col) to the Action type       #\"))))(Secondary((id \
         5b50f9f3-a582-47f3-9440-8d2699e3b3d4)(content(Whitespace\"\\n\"))))(Secondary((id \
         d32d9716-f5b1-497a-80ab-0530961c2570)(content(Comment\"#   2. Add a \
         setCol helper function               #\"))))(Secondary((id \
         9332e9b6-029a-44eb-b833-4f40da5630ee)(content(Whitespace\"\\n\"))))(Secondary((id \
         94ddcbe6-e00d-46ab-8dbe-17332b84bc80)(content(Comment\"#   3. Handle \
         PlantCol in the update function     #\"))))(Secondary((id \
         a368f3c0-a237-4897-9a1d-25059f288067)(content(Whitespace\"\\n\"))))(Secondary((id \
         b2a719c3-8b51-4447-aa4c-0a33c3628b2d)(content(Comment\"#                                                 \
         #\"))))(Secondary((id \
         29342185-0121-48c5-8166-75f20dd46fad)(content(Whitespace\"\\n\"))))(Secondary((id \
         46c5d2c0-3f57-4531-8849-f6b519eaf0f1)(content(Comment\"# Look at how \
         PlantRow is implemented for         #\"))))(Secondary((id \
         f83ca001-0744-43b6-b9cd-cdcbe7898fcd)(content(Whitespace\"\\n\"))))(Secondary((id \
         91df05d9-7ff1-431d-81ba-ae531dab1e71)(content(Comment\"# guidance - \
         PlantCol is similar but vertical.    #\"))))(Secondary((id \
         1aa3238e-4e24-4006-ba02-05842f2e3a83)(content(Whitespace\"\\n\"))))(Secondary((id \
         9bbed47c-49a5-4ba1-9609-09eeaa417f4b)(content(Comment\"#                                                 \
         #\"))))(Secondary((id \
         22cf69ae-7da1-4401-a72e-b1ce53aeb275)(content(Whitespace\"\\n\"))))(Secondary((id \
         e0fbc441-0e9a-4876-802f-d1aa4dbc092a)(content(Comment\"# Tip: Use \
         auto-probe to see how the grove        #\"))))(Secondary((id \
         806e06d1-d224-4920-bec5-503dc5aed3de)(content(Whitespace\"\\n\"))))(Secondary((id \
         8da20dd3-0bca-46a4-be14-fb7792394fb2)(content(Comment\"# changes \
         after each action.                      #\"))))(Secondary((id \
         fa24f680-4642-4b8f-b5af-9c6412d13bf6)(content(Whitespace\"\\n\"))))(Secondary((id \
         9b692314-37fa-498c-820e-8350b32c5893)(content(Whitespace\"\\n\"))))(Tile((id \
         4c52ad76-cc56-48e0-93ff-2cb3faf9419d)(label(type = in))(mold((out \
         Exp)(in_(TPat Typ))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         c53be3ad-4b9f-48e3-816a-ea2d1cf36262)(content(Whitespace\" \
         \"))))(Tile((id \
         b5b5442f-2c89-4fb2-b651-e7a1c3cd1f75)(label(Plant))(mold((out \
         TPat)(in_())(nibs(((shape Convex)(sort TPat))((shape Convex)(sort \
         TPat))))))(shards(0))(children())))(Secondary((id \
         13af23d7-c6b4-468a-8bd2-db1df1b4750c)(content(Whitespace\" \
         \")))))((Secondary((id \
         c4ec8dd6-7a88-4ad6-a30c-4eb9b087af50)(content(Whitespace\" \
         \"))))(Tile((id \
         24d97288-e6da-42c9-aa5a-af02567735a3)(label(String))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         cbe52dab-7d2d-4126-9731-ee77c8435049)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         dea4b386-5f3d-4111-bc79-1f3a46a1cc65)(content(Whitespace\"\\n\"))))(Tile((id \
         061dbeae-8c9a-4518-ab87-a99d9c5cfee4)(label(type = in))(mold((out \
         Exp)(in_(TPat Typ))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         9003a8e8-0b28-4c97-841c-33c0db1a5266)(content(Whitespace\" \
         \"))))(Tile((id \
         e1c89c54-f7a6-4275-b0c5-f6f0d56feb24)(label(Grove))(mold((out \
         TPat)(in_())(nibs(((shape Convex)(sort TPat))((shape Convex)(sort \
         TPat))))))(shards(0))(children())))(Secondary((id \
         1e73f2dc-ddea-43b4-a256-77f22b261d55)(content(Whitespace\" \
         \")))))((Secondary((id \
         2fe754a5-fc74-4915-892b-244a7fedc8b6)(content(Whitespace\" \
         \"))))(Tile((id 48c2cbeb-d506-41da-a93a-be54dc41e77a)(label([ \
         ]))(mold((out Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape \
         Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
         a228dab9-a7ef-4399-b8b3-26f00575eeb1)(label([ ]))(mold((out \
         Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0 1))(children(((Tile((id \
         95db4eb5-14aa-4047-93bf-b7daf2d99314)(label(Plant))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))))))))))))(Secondary((id \
         4bc6330b-b8e8-4483-943e-defaaa36e63f)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         e94e5bfa-639c-4f17-bfec-5878cbe6101e)(content(Whitespace\"\\n\"))))(Tile((id \
         f9ccb256-c301-43ab-bcca-6ca6522ae1a5)(label(type = in))(mold((out \
         Exp)(in_(TPat Typ))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         a170bb79-8248-4f1b-b18e-36ccff5c1b8f)(content(Whitespace\" \
         \"))))(Tile((id \
         e1685240-9bca-47bd-b5e7-031fe8a1c50e)(label(Row))(mold((out \
         TPat)(in_())(nibs(((shape Convex)(sort TPat))((shape Convex)(sort \
         TPat))))))(shards(0))(children())))(Secondary((id \
         0d107ab4-87e9-4cc6-8dd0-de0c7f7e6b6f)(content(Whitespace\" \
         \")))))((Secondary((id \
         31397454-6ec4-4ee1-955d-eb220761f952)(content(Whitespace\" \
         \"))))(Tile((id \
         0c88557a-f17b-473f-8f16-a83c5763b33b)(label(Int))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         b9024fc0-2207-413d-8212-70aa6ee1e53a)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         169fe697-2eae-4681-a018-962892726009)(content(Whitespace\"\\n\"))))(Tile((id \
         d3a844ef-1de9-49cb-aaee-919966842ab1)(label(type = in))(mold((out \
         Exp)(in_(TPat Typ))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         aeedffcc-d9f5-4482-b9fd-5bf1964ed19f)(content(Whitespace\" \
         \"))))(Tile((id \
         c55a010f-8bf8-4462-9913-10802818ea02)(label(Col))(mold((out \
         TPat)(in_())(nibs(((shape Convex)(sort TPat))((shape Convex)(sort \
         TPat))))))(shards(0))(children())))(Secondary((id \
         f457c35d-4426-4044-af17-e9fbd5156756)(content(Whitespace\" \
         \")))))((Secondary((id \
         deb4b5bb-0071-4ea4-b537-4219f4f8beef)(content(Whitespace\" \
         \"))))(Tile((id \
         5a39017c-7b06-4727-9b42-1423794d25c8)(label(Int))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         ec154652-1ebb-4944-980a-2c58ce47eee3)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         adc02760-7162-412a-bf3e-b8a657bf4610)(content(Whitespace\"\\n\"))))(Secondary((id \
         cb585399-475d-4b04-8a10-72eddbd7fce6)(content(Whitespace\"\\n\"))))(Tile((id \
         9568a1c3-3a99-49ea-9e34-993dad969daf)(label(type = in))(mold((out \
         Exp)(in_(TPat Typ))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         aa98e436-23e8-493a-8862-79283e0abf3a)(content(Whitespace\" \
         \"))))(Tile((id \
         ddf25545-8bf8-470d-84f5-8a29b3453395)(label(Model))(mold((out \
         TPat)(in_())(nibs(((shape Convex)(sort TPat))((shape Convex)(sort \
         TPat))))))(shards(0))(children())))(Secondary((id \
         e9678599-dcb6-41f7-9286-410c8c4c1d3b)(content(Whitespace\" \
         \")))))((Secondary((id \
         a21e604a-7d81-4a7e-9f34-80adae64e95b)(content(Whitespace\" \
         \"))))(Tile((id \
         91820fa6-5d38-4645-9657-3b5be766b905)(label(\"(\"\")\"))(mold((out \
         Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0 1))(children(((Secondary((id \
         3b088c76-349e-4de0-b67d-87d5018f428a)(content(Whitespace\"\\n\"))))(Tile((id \
         4326c28b-7f3d-4db4-bb0c-5ae7a1422419)(label(grove))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         e7dc2013-c65b-4b17-a3fb-731c1a1cf1ac)(content(Whitespace\" \
         \"))))(Tile((id \
         a69345a8-67d9-463f-b6cd-215c81556d71)(label(=))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 39))(sort Typ))((shape(Concave \
         39))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         18447f08-947c-4aa9-ad3d-c1ca5d98c8d2)(content(Whitespace\" \
         \"))))(Tile((id \
         af815909-bbcf-4c96-8103-c173a019837d)(label(Grove))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         c2123831-34a8-49d9-8d02-e92195e249ba)(label(,))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 44))(sort Typ))((shape(Concave \
         44))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         ac1d1cc1-6949-4057-aa82-4c1e95dd4f12)(content(Whitespace\"\\n\"))))(Tile((id \
         28d2aa47-768e-462d-93a1-591e015594ed)(label(currentSeed))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         10d5fc83-0f69-4757-a11c-e1aca3482a5d)(content(Whitespace\" \
         \"))))(Tile((id \
         35b70b0a-6019-4c89-bb29-4e92f82eb2e3)(label(=))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 39))(sort Typ))((shape(Concave \
         39))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         5554db17-e82b-4956-a686-4e0bf3b4f0cf)(content(Whitespace\" \
         \"))))(Tile((id \
         0747bafb-3797-4114-bcc5-90700eb24d0e)(label(Plant))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         fcb9c3d2-001b-4a94-9204-09ffbcca903b)(label(,))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 44))(sort Typ))((shape(Concave \
         44))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         a880ba8d-23a2-4fcf-b5f0-217e11b7348a)(content(Whitespace\"\\n\"))))(Tile((id \
         b346c0ea-0b15-41ff-a965-e45cb0a9f835)(label(seedInventory))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         32ac855e-0843-4e7c-9556-f698a9546688)(content(Whitespace\" \
         \"))))(Tile((id \
         1e0ead1a-856c-4850-8513-a2623e5d44d9)(label(=))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 39))(sort Typ))((shape(Concave \
         39))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         cba4d4b0-42c8-4a64-b901-66e0ff9cef8a)(content(Whitespace\" \
         \"))))(Tile((id 81afd31d-780a-4482-84bf-a03f2ea4070a)(label([ \
         ]))(mold((out Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape \
         Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
         ba296433-0436-4ab5-bcdd-5676d4e8f1ce)(label(Plant))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children()))))))))(Secondary((id \
         26a588d2-84cb-4bb7-b5b9-2bf88079c4da)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         9abc9ef7-fe1c-4778-9eb9-9cdd21c34a21)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         5dee6d57-471f-4d63-9f1c-53247abb6271)(content(Whitespace\"\\n\"))))(Secondary((id \
         e69c77e1-2efa-4f8b-a2c0-de93a91500e6)(content(Whitespace\"\\n\"))))(Tile((id \
         05bb73b6-9009-4c01-aa9c-78d828a189f8)(label(type = in))(mold((out \
         Exp)(in_(TPat Typ))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         798cb3de-3a9d-4901-ab0e-3e6f7bb3bc89)(content(Whitespace\" \
         \"))))(Tile((id \
         6747f5e8-56c6-468d-b95c-ca67bd321276)(label(Action))(mold((out \
         TPat)(in_())(nibs(((shape Convex)(sort TPat))((shape Convex)(sort \
         TPat))))))(shards(0))(children())))(Secondary((id \
         5f4582db-8090-45db-8300-0c0c7e01de9c)(content(Whitespace\" \
         \")))))((Secondary((id \
         334f1933-428b-4703-b05a-57f4f6e3103b)(content(Whitespace\"\\n\"))))(Tile((id \
         3295f97d-c786-428f-90b6-c2aa774952c7)(label(+))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape(Concave 33))(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         4ceebfb7-094b-41de-89a3-82ddcc617683)(content(Whitespace\" \
         \"))))(Tile((id \
         0bf8365e-3a62-4230-9b37-440b9b3c6855)(label(SelectSeed))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         de7094de-961d-466f-a9a8-e63a030bb0d4)(label(\"(\"\")\"))(mold((out \
         Typ)(in_(Typ))(nibs(((shape(Concave 11))(sort Typ))((shape \
         Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
         0927802c-daa7-4eb4-9f31-0ef9e9c2bbce)(label(Int))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children()))))))))(Secondary((id \
         0eabb45a-2c9d-4647-9143-7f17faa82ece)(content(Whitespace\"\\n\"))))(Tile((id \
         1259363e-bd8d-4484-93a2-9302b0b82db4)(label(+))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 12))(sort Typ))((shape(Concave \
         12))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         8b59747a-8a31-4964-b940-0f89dc37c8d2)(content(Whitespace\" \
         \"))))(Tile((id \
         4d9b2aa9-d505-4573-9083-91ec71e93ae4)(label(PlantSeed))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         32d2db65-a58f-4f66-b34d-bb54d4d6aabe)(label(\"(\"\")\"))(mold((out \
         Typ)(in_(Typ))(nibs(((shape(Concave 11))(sort Typ))((shape \
         Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
         df1f56f0-3977-48dd-9754-85847bf51bfa)(label(Row))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         2f3aeb53-6b86-406e-8b15-f414bc76a6b0)(label(,))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 44))(sort Typ))((shape(Concave \
         44))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         4c995464-b80d-49c9-92db-fd42eb3e277c)(content(Whitespace\" \
         \"))))(Tile((id \
         a4ed31d5-6d1c-4abb-a427-53a577a9169c)(label(Col))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children()))))))))(Secondary((id \
         3d604b53-a7dd-4fc3-947e-e70b4e386124)(content(Whitespace\"\\n\"))))(Tile((id \
         b2a66eb9-3cf5-4ea6-83df-28d83b91df7a)(label(+))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 12))(sort Typ))((shape(Concave \
         12))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         f21edad8-bb7e-44a5-8076-59fc7d5daab5)(content(Whitespace\" \
         \"))))(Tile((id \
         15667f8f-ca53-4397-bd4f-ecb9ba3cffc3)(label(Uproot))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         b5bec365-3da2-4b70-9e4c-5870eef86d9c)(label(\"(\"\")\"))(mold((out \
         Typ)(in_(Typ))(nibs(((shape(Concave 11))(sort Typ))((shape \
         Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
         9699a93a-8cc2-496f-86d4-9b433d1b3f3c)(label(Row))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         680300f8-1e3c-4c57-b734-d6e9a47b3a32)(label(,))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 44))(sort Typ))((shape(Concave \
         44))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         e1be19d8-3d0d-435e-a0f8-5b1e8c35cfc7)(content(Whitespace\" \
         \"))))(Tile((id \
         f4fa0025-477c-41f8-82ff-47ad0a8f289f)(label(Col))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children()))))))))(Secondary((id \
         747f2509-f3c2-4dac-bd33-ad44d6610974)(content(Whitespace\"\\n\"))))(Tile((id \
         6a5e0197-919b-4454-a444-eeb54f784d9c)(label(+))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 12))(sort Typ))((shape(Concave \
         12))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         e6a8c777-fb0f-4d99-a4ef-de7a67da62ec)(content(Whitespace\" \
         \"))))(Tile((id \
         c7c48390-1a10-4431-a5c6-d7ac66f5e6c1)(label(ClearGrove))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         41fa4e6d-42e1-4aba-9e84-8245cfd4b2b3)(content(Whitespace\"\\n\"))))(Tile((id \
         f6a77d71-8b35-44d2-90e2-ae7caa958ea3)(label(+))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 12))(sort Typ))((shape(Concave \
         12))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         ee7c5563-523f-4ca0-9b38-954e5e20e967)(content(Whitespace\" \
         \"))))(Tile((id \
         a4d9a350-ffad-468a-9f0d-c8feae6ab8fb)(label(PlantRow))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         07525856-c672-4d16-8d34-2f282082e145)(label(\"(\"\")\"))(mold((out \
         Typ)(in_(Typ))(nibs(((shape(Concave 11))(sort Typ))((shape \
         Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
         29236477-187e-4bdb-a4b8-ac9f4f2bad67)(label(Row))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children()))))))))(Secondary((id \
         60b33948-5af5-48f7-bf76-3ec17d3093f2)(content(Whitespace\"\\n\"))))(Secondary((id \
         9a3a93bd-1f81-4416-a77e-67a725a9e05c)(content(Comment\"# TODO: Add \
         PlantCol(Col) here #\"))))(Secondary((id \
         b25ea9f9-cd9b-485c-8bd6-2ff68def48a2)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         a77b469b-3815-4515-88be-a11d9bf9b44e)(content(Whitespace\"\\n\"))))(Secondary((id \
         15a2a230-537e-472f-bc42-5ddc62afa709)(content(Whitespace\"\\n\"))))(Tile((id \
         9b648a32-76e8-4c38-840f-77a295a0fee3)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         592a6e73-3b5f-4ab2-ba46-f268f84683d3)(content(Whitespace\" \
         \"))))(Tile((id \
         27bbdf63-90f9-4b17-98e1-84e59790d133)(label(init))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         87b4076a-3260-482f-9c86-77fb08f7c3d6)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 24))(sort Pat))((shape(Concave \
         24))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         06e91a02-776c-48b2-8f66-41a79f2619a4)(content(Whitespace\" \
         \"))))(Tile((id \
         f5607bac-32cb-4b65-b797-a739fb2fa2ae)(label(Model))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         580358c4-26f4-47a5-b2e9-248ce3d047ce)(content(Whitespace\" \
         \")))))((Secondary((id \
         7c6475a2-22ff-4aa9-b396-bd79a6e14b6e)(content(Whitespace\" \
         \"))))(Tile((id \
         b0707fb9-9564-480f-be9a-95298ce82456)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         204bf165-f67e-41a2-86ab-9ff6462845a4)(content(Whitespace\"\\n\"))))(Tile((id \
         faf0c129-3fa6-49fc-acfa-f20aaffde894)(label(grove))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         955beb19-cda9-4ed2-9786-ae41f1e137aa)(content(Whitespace\" \
         \"))))(Tile((id \
         a7d32113-e907-433e-80ca-18ab1c89237d)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         4331cada-6241-4941-9f81-2113a622d1be)(content(Whitespace\" \
         \"))))(Tile((id 14b9b28e-3b1a-43c4-89e7-ed0179681551)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         f307d745-32a7-462a-b159-5121f740d265)(content(Whitespace\"\\n\"))))(Tile((id \
         109290f5-6373-48a5-8191-7d6cccbaf9dc)(label([ ]))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         cd8767ba-b15f-499e-8b1d-bcb32f4e65dd)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         128c343b-e58e-4b69-b9b6-30046cc2e3a3)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         e28ef224-0e00-4274-bdee-ea0e9dc29e3e)(content(Whitespace\" \
         \"))))(Tile((id \
         de2e964d-f15b-4f0f-a19d-27eb3571e411)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         972932a9-6c0f-4f53-8fb5-73fc777ef772)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         c73ed813-4024-4d70-ac70-47b82892106f)(content(Whitespace\" \
         \"))))(Tile((id \
         b97a9fee-1d01-40d9-9848-9a753c0bf212)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         3c502a73-ea8d-432c-96ca-6135a7be18ab)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         860bef8b-3cce-49c2-b2b0-8fa9290beeb8)(content(Whitespace\"\\n\"))))(Tile((id \
         4683045d-5091-400b-9c54-b39c75410938)(label([ ]))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         7a45e6fe-6ecc-4436-810a-b46e7185bde5)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         46da0d7f-dc65-4fc4-bc2c-8a14f875f3c4)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         68646b6e-7251-44f2-8dc1-1014f5e42988)(content(Whitespace\" \
         \"))))(Tile((id \
         14d5623a-3edc-4375-a1d9-301969959919)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         6ca4d71d-4f0d-4592-a0e0-9c68ecaa1f77)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         c1c8d254-700a-4a5f-90bb-636c66844699)(content(Whitespace\" \
         \"))))(Tile((id \
         6aa90831-0511-41d0-a6d4-b6f66c40ef61)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         69cb0654-6a29-4386-94f9-af65750de158)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         9fd1ce8c-c6f2-4712-8551-9118c6362d74)(content(Whitespace\"\\n\"))))(Tile((id \
         2c79bc36-90d4-4895-af09-26b8c1b65da6)(label([ ]))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         a4a27470-4248-47f8-b989-c58dd9f1369b)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         6be87f6d-e0a1-41d2-ac46-c3fa73491745)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         322a3bc8-67d6-4bf3-83dd-4e175f1be6a8)(content(Whitespace\" \
         \"))))(Tile((id \
         8e5b50ee-bd93-4c1e-9bbf-8a786f3f5337)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         a78d385c-9a95-433a-bdee-890cf1d3a74a)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         c65d710c-ab57-4387-923e-0a4b70f31ebf)(content(Whitespace\" \
         \"))))(Tile((id \
         eae25341-6d24-4faf-bdb8-a6d0f65ed14d)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         0bc3957f-9a1a-4b63-8d66-3799c5209ffb)(content(Whitespace\"\\n\")))))))))(Tile((id \
         6d7130be-9098-41ba-a073-95b730512463)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         ee0e683e-4fa5-4c1e-93f1-734ecee65b8e)(content(Whitespace\"\\n\"))))(Tile((id \
         d9f4aa06-ca35-4d29-ba39-90e28e651510)(label(currentSeed))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         6fe45bc7-acb7-4cf9-a390-0aec6c514575)(content(Whitespace\" \
         \"))))(Tile((id \
         df65824c-3e06-40cc-821a-569ad4671ebb)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         13d30b15-8da9-4c42-ba44-9318e40a6cba)(content(Whitespace\" \
         \"))))(Tile((id \
         2a5831f7-e099-4763-9752-22376a208989)(label(\"\\\"\\240\\159\\140\\177\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         93e606aa-bc48-49dc-bdf9-1b44a60a0bd0)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         1d22332d-f31d-4030-9850-40e4216d87ce)(content(Whitespace\"\\n\"))))(Tile((id \
         22bda9e0-903d-403e-8e7a-a268d0685fa1)(label(seedInventory))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         f8d55726-bc6a-4089-90b0-22e8450789f7)(content(Whitespace\" \
         \"))))(Tile((id \
         ccf20a86-af0a-40ae-9128-57c8b4e32e42)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         5f5a0a6c-510e-4d7b-acee-55c34969b1f9)(content(Whitespace\" \
         \"))))(Tile((id 7f0616b4-2a26-40c4-b7d9-b883611e203f)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         458946e9-794b-4cc5-8ae2-d4a4927c5cf4)(label(\"\\\"\\240\\159\\140\\177\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         8236a20a-07b6-4960-831e-2123c6a3391c)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         4ca283e8-2f24-47f2-9568-1addcb880299)(content(Whitespace\" \
         \"))))(Tile((id \
         133183aa-51b6-41f2-b3be-853e733596c7)(label(\"\\\"\\240\\159\\140\\191\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         b9a3d406-ff37-4c00-96ce-efaf86917c70)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         1bd96ed3-7cb5-4ffd-a918-d92fa3d1d6db)(content(Whitespace\" \
         \"))))(Tile((id \
         52828da2-9297-4d41-82b8-b942232961b7)(label(\"\\\"\\240\\159\\141\\132\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         ecfa9b65-1dfb-434c-af9b-6ef37aaee29f)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         548e3114-00d8-4d00-981d-1ad0244de536)(content(Whitespace\" \
         \"))))(Tile((id \
         232b237a-0d4c-4216-a184-90732ec65746)(label(\"\\\"\\226\\152\\152\\239\\184\\143\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         46b45abf-8f8a-4f86-93c8-fd87800306e9)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         290bcfa7-2960-4c52-bc2a-74ca4b392764)(content(Whitespace\" \
         \"))))(Tile((id \
         0107ffee-bf3f-4cc1-8a22-3f80529b94ef)(label(\"\\\"\\240\\159\\140\\184\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         1cff4b38-4c80-492d-9e3b-057a1d3697d7)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         bf606517-e4c3-4cbc-8ff3-251be902d6f9)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         33ef68a4-8ed3-48a2-8b13-753997038845)(content(Whitespace\"\\n\"))))(Secondary((id \
         e69983ca-8e56-4c79-addd-1c4d3c7579db)(content(Whitespace\"\\n\"))))(Tile((id \
         b4edfaa3-d3c6-492b-bbea-5c9b19c857a7)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         d9ff6f28-89d3-40a9-a8f0-c9368ff2265b)(content(Whitespace\" \
         \"))))(Tile((id \
         227503f3-49d0-439e-b334-ea3ca8d0b36c)(label(setCell))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         60bfcbb5-fa2f-43e1-8a81-120954f0e735)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 24))(sort Pat))((shape(Concave \
         24))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         f3d46d8f-1433-46ce-b528-32b453f21917)(content(Whitespace\" \
         \"))))(Tile((id \
         a1d8f795-9066-43aa-8663-d13799c0443a)(label(\"(\"\")\"))(mold((out \
         Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0 1))(children(((Tile((id \
         079e5b5b-48c5-4710-80a6-aa4c4adbea62)(label(Grove))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         82a91d71-3501-4871-b316-797c1d84265b)(label(,))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 44))(sort Typ))((shape(Concave \
         44))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         9da4aa5d-f18f-4a26-af5f-145fe229c103)(content(Whitespace\" \
         \"))))(Tile((id \
         be752483-bb82-4536-aaca-5ef480b21fff)(label(Row))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         97c6b6d0-e8ea-4c8d-9b74-4d01cb9d2eea)(label(,))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 44))(sort Typ))((shape(Concave \
         44))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         60ea1a96-6c23-4aa6-a7a4-fb140e184bce)(content(Whitespace\" \
         \"))))(Tile((id \
         7e7123a5-63df-4c4d-8abd-3f4ee589015d)(label(Col))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         b1e10400-5974-40e3-ad04-990d10cdcacd)(label(,))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 44))(sort Typ))((shape(Concave \
         44))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         78a7f3a9-100b-42fb-ab29-4e6224ab3bc4)(content(Whitespace\" \
         \"))))(Tile((id \
         a05ec1a5-4d0d-466c-b9b7-3e43fe524f65)(label(Plant))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children()))))))))(Secondary((id \
         9cc88bb2-fcb0-4a5c-ad88-96b6470ea1b0)(content(Whitespace\" \
         \"))))(Tile((id \
         6a1d5a3d-327d-46d1-8dd7-b9a5e0f1bd15)(label(->))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 13))(sort Typ))((shape(Concave \
         13))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         1b5dc75e-e106-4a31-9761-d709e31bd84a)(content(Whitespace\" \
         \"))))(Tile((id \
         f5ea8351-8d73-4ee6-918e-8aa5e8a2fbd8)(label(Grove))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         7f93af2f-3482-4aa1-a5bb-7bf2685f447f)(content(Whitespace\" \
         \")))))((Secondary((id \
         0e6e390a-f308-48a3-ad69-f16bae48ef7f)(content(Whitespace\"\\n\"))))(Tile((id \
         50177666-8d1a-4af9-8357-f4986ac5785b)(label(fun ->))(mold((out \
         Exp)(in_(Pat))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         37))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         66e580a8-e875-4aaa-88bf-2ad93cce22b4)(content(Whitespace\" \
         \"))))(Tile((id \
         376a0ac4-967c-4bc1-a803-53c8b77157d4)(label(grove))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         e08642d8-df71-4294-909b-78298e39ffea)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 44))(sort Pat))((shape(Concave \
         44))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         dc8d7270-fb68-4ef9-be74-62ad8b2ef073)(content(Whitespace\" \
         \"))))(Tile((id \
         809877ad-d0e4-4e8c-88ee-974b201b075c)(label(row))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         d4806979-952c-4302-b638-64c3e874c921)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 44))(sort Pat))((shape(Concave \
         44))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         e282ae24-0d16-4298-8a3e-4e6470a7fc7e)(content(Whitespace\" \
         \"))))(Tile((id \
         36f162c7-4ec7-484e-8c53-0feae7b4e362)(label(col))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         70250251-20e6-4c7b-9623-4cc9297ccf47)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 44))(sort Pat))((shape(Concave \
         44))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         fd359957-1a1f-43cd-9837-27ede278b097)(content(Whitespace\" \
         \"))))(Tile((id \
         6d6084f0-8e06-4abd-bf3d-11c87084fde5)(label(plant))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         45be5bba-c416-473c-a7d8-be0c109608ef)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         1b0d5a98-b7c1-4113-8c38-060d81efc013)(content(Whitespace\"\\n\"))))(Tile((id \
         41682a4e-df60-454a-8f2e-2873ae043f54)(label(mapi))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         b367ec23-08c9-41e0-b9c4-9da67ffe567f)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         3b082e59-aad5-4d5e-9318-48fd7e8e173a)(label(grove))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         41b2aaec-2512-4cd7-ae66-e68cd455c965)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         c4678d12-51a3-4f6f-a298-3aadd071625c)(content(Whitespace\" \
         \"))))(Tile((id f9f40ccb-1cf7-4707-b20b-d23780561e21)(label(fun \
         ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 37))(sort Exp))))))(shards(0 \
         1))(children(((Secondary((id \
         405eaee0-44d9-407d-b4ba-6032526ed93d)(content(Whitespace\" \
         \"))))(Tile((id \
         47be05ac-b6db-4681-93db-616b66ff1bec)(label(\"(\"\")\"))(mold((out \
         Pat)(in_(Pat))(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0 1))(children(((Tile((id \
         8e4fbf97-a8ae-4147-a3a2-977fa1f8e7fd)(label(i))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         46e2eeaf-a1e3-437b-a8d0-6ccc1e176d5b)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 44))(sort Pat))((shape(Concave \
         44))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         22ecefea-2a34-4c80-ab56-db28ee747d70)(content(Whitespace\" \
         \"))))(Tile((id \
         c13b6ee2-1745-4cd3-9477-c2c1fb1ffd51)(label(r))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children()))))))))(Secondary((id \
         51a0a969-105c-42fc-8cf4-fd18c8c47bf4)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         5c885ca7-2513-4a49-b84e-16167c638fd9)(content(Whitespace\"\\n\"))))(Tile((id \
         cff15154-8bf0-403d-9875-948c5c6cc439)(label(if then else))(mold((out \
         Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         36))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         16a1ac67-a1d1-4c99-b37e-4eab31a712e3)(content(Whitespace\" \
         \"))))(Tile((id \
         301beb29-72ec-4b59-bfec-1f608d16cabc)(label(i))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         91b96a48-5903-4ae4-b86a-10df92f120ac)(content(Whitespace\" \
         \"))))(Tile((id \
         22f45f3c-6d8d-4efd-bb0f-e0e0e7bd3ada)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         e0c2f9e2-dedf-4e6d-ab88-ae99c026cee6)(content(Whitespace\" \
         \"))))(Tile((id \
         86adce87-afef-44f5-93b4-aa5d17d143e1)(label(row))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         42a7f34a-4184-4923-90e2-0bb73bec445f)(content(Whitespace\"\\n\")))))((Secondary((id \
         0af09ef3-b28c-44b8-a47e-f0e0fa561484)(content(Whitespace\" \
         \"))))(Tile((id \
         65019c99-5702-417f-8aec-11e8deff1115)(label(mapi))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         9b433647-0e06-4114-af71-8fa2c7e0b82d)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         1e493aa0-03a5-4c6d-a285-fc7b1fb57067)(label(r))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         c32ebbb9-7b02-45fd-926d-56bc38fb66d4)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         626f688d-2c2b-49e9-993c-b37ea00127aa)(content(Whitespace\" \
         \"))))(Tile((id 99f9809e-bc20-4542-9043-b5cb2ed0f0ed)(label(fun \
         ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 37))(sort Exp))))))(shards(0 \
         1))(children(((Secondary((id \
         f2519cf9-79f4-411c-b2f5-4c35bf9ed1c0)(content(Whitespace\" \
         \"))))(Tile((id \
         a437c635-95e6-4b46-bfde-7f6a49ca82ec)(label(\"(\"\")\"))(mold((out \
         Pat)(in_(Pat))(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0 1))(children(((Tile((id \
         b3bd90b6-7fa2-48fa-8082-61f5778ddeab)(label(j))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         cae01483-ccd9-483b-b38f-4b1e75aa64ad)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 44))(sort Pat))((shape(Concave \
         44))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         d46925eb-36d0-4c3f-bdef-b88836fa3400)(content(Whitespace\" \
         \"))))(Tile((id \
         b2aed14a-3278-4e48-93b2-e1f50c9a07d7)(label(c))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children()))))))))(Secondary((id \
         58411497-11bd-4299-a5e7-83af38b1983d)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         c3a8a8de-96fe-41b9-8c69-447862dd9cb1)(content(Whitespace\" \
         \"))))(Tile((id c996d08e-2707-4a48-a274-809024dc8df5)(label(if then \
         else))(mold((out Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 36))(sort Exp))))))(shards(0 1 \
         2))(children(((Secondary((id \
         14a409e6-8f3d-4fd4-98dd-cd3b11d1a4f8)(content(Whitespace\" \
         \"))))(Tile((id \
         61def4ce-c9ce-4917-a16d-13178dd70e28)(label(j))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         5d34ed72-a8c5-4dae-924f-21a801a5ab11)(content(Whitespace\" \
         \"))))(Tile((id \
         0b34ca03-e0d7-4f11-ba1d-1977460d578b)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         a04aa7e4-479d-49de-a0ae-961a6076ecc4)(content(Whitespace\" \
         \"))))(Tile((id \
         3e3be20e-08d0-4488-8ccb-2fbdb00a65a7)(label(col))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         92cabacb-3ce6-40f8-bfd4-5c2aaa87ed70)(content(Whitespace\" \
         \")))))((Secondary((id \
         61eed805-c83d-48f0-9060-d138d1d48f7d)(content(Whitespace\" \
         \"))))(Tile((id \
         ce49bb79-3a05-4638-8412-f6fbca80b359)(label(plant))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         907a7513-a10e-40e7-a25b-7ef740a8d367)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         9614f303-2358-468a-95f3-c12a3e49173d)(content(Whitespace\" \
         \"))))(Tile((id \
         b74d51a2-452d-4cf0-90ad-4dc2ae2df6a8)(label(c))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         0fed3e24-315e-4bd9-a63e-93cd4a4511da)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         be047139-12eb-4eff-a276-e623763880be)(content(Whitespace\" \
         \"))))(Tile((id \
         ff2f83d9-7799-4bc8-9f74-9fa2ace44bd5)(label(r))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         dd38c223-2888-4018-920f-aaa009090f70)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         4ffb0219-edcb-4986-b0e1-15bc47e01dd6)(content(Whitespace\"\\n\"))))(Secondary((id \
         4e81c524-b669-4b53-bf7f-c8fca5dc9d79)(content(Whitespace\"\\n\"))))(Tile((id \
         8c3cd96d-b5bb-4619-8232-515de3b0e345)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         d893e3ba-7c60-4606-b225-f022c56a7e10)(content(Whitespace\" \
         \"))))(Tile((id \
         c84e4f6c-a964-415b-aebe-7f48adc7268e)(label(setRow))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         6de55b42-aa94-4ad5-b088-b638e9272a08)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 24))(sort Pat))((shape(Concave \
         24))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         a0fbdbfb-edcd-48b1-9052-c7c2138eab87)(content(Whitespace\" \
         \"))))(Tile((id \
         3c923d7e-6d0c-48f6-a7d5-439cfa247529)(label(\"(\"\")\"))(mold((out \
         Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0 1))(children(((Tile((id \
         19e85f52-589d-4a46-8cd6-bb3a7fdbc9ad)(label(Grove))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         3bdd6e72-1a07-48f3-bfc7-c55d8ab71f6c)(label(,))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 44))(sort Typ))((shape(Concave \
         44))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         04ccebb7-665b-449f-bcea-45418a7a4066)(content(Whitespace\" \
         \"))))(Tile((id \
         e56db24f-f359-41c4-a875-e8d67475f155)(label(Row))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         935dceb8-e3e3-4752-af07-5b04e54e0d39)(label(,))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 44))(sort Typ))((shape(Concave \
         44))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         a334d8cf-6fec-4465-9542-0a03ed13beed)(content(Whitespace\" \
         \"))))(Tile((id \
         5a1f6b85-2202-4441-b17b-372ea566ee16)(label(Plant))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children()))))))))(Secondary((id \
         d5f538e5-de38-4533-ba6f-78f009515cdd)(content(Whitespace\" \
         \"))))(Tile((id \
         83cb1953-9617-4c7f-864b-a5577623d621)(label(->))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 13))(sort Typ))((shape(Concave \
         13))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         d7dc5dee-2689-4c68-90b1-c41a5e4c065d)(content(Whitespace\" \
         \"))))(Tile((id \
         e3cae152-9ad5-4bbc-bf70-d25cb0feedde)(label(Grove))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         6639f94a-847a-4baa-bd82-54f865519578)(content(Whitespace\" \
         \")))))((Secondary((id \
         2adaa55d-8a7b-498e-96b3-8b6934232282)(content(Whitespace\"\\n\"))))(Tile((id \
         f050a170-894c-497d-bb95-660b0d01de87)(label(fun ->))(mold((out \
         Exp)(in_(Pat))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         37))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         6f148365-7ae9-4ca5-8850-2763d56bea26)(content(Whitespace\" \
         \"))))(Tile((id \
         00523c58-4528-4fd6-a2f0-31033a89b002)(label(grove))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         fa67966f-dfe4-4ff9-9a9e-6ecf036bd3d4)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 44))(sort Pat))((shape(Concave \
         44))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         ac4b87d5-da67-4500-9a73-c94e81b86544)(content(Whitespace\" \
         \"))))(Tile((id \
         0735a8bb-3f8a-4fec-b71c-11f3e4b4ff75)(label(targetRow))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         f5192320-c706-4a91-8476-b0e02813fd4c)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 44))(sort Pat))((shape(Concave \
         44))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         b87e71cb-c9e0-49e9-8f8f-7aa43886367e)(content(Whitespace\" \
         \"))))(Tile((id \
         dc3caac2-ed5e-4069-9cb2-23e105fad11f)(label(plant))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         3e922fe9-ae82-490c-8f8b-4ba0ec0dfd3e)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         7a00e864-0070-4c5b-81dd-d5a6ee0bdccf)(content(Whitespace\"\\n\"))))(Tile((id \
         6a3bc6a3-8eaa-4cff-a280-db67fc0d84a2)(label(mapi))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         ee8d1e45-5c5a-4acf-b379-e5ca74cbfb79)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         e8801a8a-b4a4-4cff-8293-eb55b70ffb44)(label(grove))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         4b6dd21f-8aea-4c3d-9ad5-f1fbc53a87cf)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         ec8e79e9-bdb5-4349-9538-411ce7707bc2)(content(Whitespace\" \
         \"))))(Tile((id 8498a5c1-7ac7-4695-8559-5b1ac9fcaca0)(label(fun \
         ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 37))(sort Exp))))))(shards(0 \
         1))(children(((Secondary((id \
         24d740aa-8c85-45ef-a70b-c886eaad42c4)(content(Whitespace\" \
         \"))))(Tile((id \
         28a52f64-bb9f-40fc-80e2-ac19d5e8d3cc)(label(\"(\"\")\"))(mold((out \
         Pat)(in_(Pat))(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0 1))(children(((Tile((id \
         789a1874-2bf3-476b-b175-5f14570509ca)(label(i))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         5754bc3c-839f-4675-b32f-1dc9855521f6)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 44))(sort Pat))((shape(Concave \
         44))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         c910d927-b4cc-40cb-b331-eb393d2f6b17)(content(Whitespace\" \
         \"))))(Tile((id \
         ec007705-4807-44eb-82b2-71b52e01bcde)(label(row))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children()))))))))(Secondary((id \
         9b6d899d-bf2e-4e86-83f5-d02de3e7f0e7)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         ce6940ab-1c2c-4644-b9aa-ba03b1b49b4a)(content(Whitespace\"\\n\"))))(Tile((id \
         bedc74bb-6539-42b4-ad15-10c8339a73c9)(label(if then else))(mold((out \
         Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         36))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         48284dea-53fb-489b-86ae-0f19f204479b)(content(Whitespace\" \
         \"))))(Tile((id \
         db78dd72-679d-4400-af76-da6c1b3bc40f)(label(i))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         04f7e547-06f7-4293-a12c-f84e43ddbec8)(content(Whitespace\" \
         \"))))(Tile((id \
         0d34edf5-e05c-4f36-9240-3e2d01449fa3)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         03302db9-e19f-45d7-a8b6-d4b9dc5d8229)(content(Whitespace\" \
         \"))))(Tile((id \
         b2bb17a1-d4b7-4478-a6f3-5f772ed19dba)(label(targetRow))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         71210ece-9947-454f-8172-6b16dbacd6db)(content(Whitespace\"\\n\")))))((Secondary((id \
         df6b05ec-4d32-443f-b7b7-7ac25626b8d4)(content(Whitespace\" \
         \"))))(Tile((id \
         e0990317-14ff-4f2e-88a2-6080bf631889)(label(map))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         57fdc3c3-425e-489e-9839-c4819d32ca70)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         7d047b0e-8079-4be5-81d2-87f1aedce364)(label(row))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         2f0ce07b-4672-4be8-90a5-b6ad656bff6a)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         a2cb921b-7cdb-4d3d-8bfb-57097f761bf5)(content(Whitespace\" \
         \"))))(Tile((id 1c5bae2b-f37d-4de6-a5a2-47cdafd53796)(label(fun \
         ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 37))(sort Exp))))))(shards(0 \
         1))(children(((Secondary((id \
         42abaddf-97b2-440c-8b29-d86dd169f5b8)(content(Whitespace\" \
         \"))))(Tile((id \
         3fb28101-bf93-48bf-8e72-fcc3ad1f25e7)(label(_))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         adc79c62-e75e-434b-bd0f-2d573561a775)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         8700963b-4935-46bd-a60a-153895abc267)(content(Whitespace\" \
         \"))))(Tile((id \
         748b0bf6-d4c9-44da-bcb5-97b62f67db53)(label(plant))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         9c0a8cf9-86c5-47eb-a8cc-27fefd800ce9)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         5cc06f6f-d86b-475d-b31b-f3ba8a23c02e)(content(Whitespace\" \
         \"))))(Tile((id \
         b5669f7e-605b-4abe-b21a-99e17e295a05)(label(row))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         4672a937-f2e9-405d-90c9-f55d70adf7cb)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         3eecf731-e847-4be2-926b-e19a72feb99e)(content(Whitespace\"\\n\"))))(Secondary((id \
         852b1b26-cec2-49dd-bbbe-7880597fcc02)(content(Whitespace\"\\n\"))))(Secondary((id \
         f8239ae2-f770-4f1c-b7e8-097221cd33df)(content(Comment\"# TODO: Add \
         setCol helper here #\"))))(Secondary((id \
         9c5db032-f3e8-470b-a647-5dc1955991b4)(content(Whitespace\"\\n\"))))(Secondary((id \
         129b20a2-6422-4c93-8182-8a31f79110c2)(content(Comment\"# Hint: You \
         need to modify each row, changing #\"))))(Secondary((id \
         9c328cac-e3fa-4747-b13e-6f840152f202)(content(Whitespace\"\\n\"))))(Secondary((id \
         61b16eb0-f9d7-4038-9cf6-8cfd7786623b)(content(Comment\"# only the \
         cell at the target column.         #\"))))(Secondary((id \
         52435b97-6e92-44fa-9f66-78324f8ba9ec)(content(Whitespace\"\\n\"))))(Secondary((id \
         efc9ff77-5f88-4995-951e-d0a324292b1e)(content(Whitespace\"\\n\"))))(Tile((id \
         add2f01d-7e30-420e-9489-fe45b07b6abc)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         73ca1485-17ce-4b4f-b17b-d0f9a5d5babc)(content(Whitespace\" \
         \"))))(Tile((id \
         e17733e9-c777-4f7f-8ddf-63f15249fcd1)(label(setAll))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         c13af062-9ff0-45f5-8fbe-859c326d478d)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 24))(sort Pat))((shape(Concave \
         24))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         86ba8948-8b6e-4177-b477-cc42df033040)(content(Whitespace\" \
         \"))))(Tile((id \
         9c2ed5f9-7d2a-406c-8856-f3b0b2bf4319)(label(\"(\"\")\"))(mold((out \
         Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0 1))(children(((Tile((id \
         b65644d7-ee4f-4941-bfdc-b74fd216aad3)(label(Grove))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         4d01df49-37ea-47f5-b0b8-d757bcaee411)(label(,))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 44))(sort Typ))((shape(Concave \
         44))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         9f2f0e5f-b831-497b-9b76-f519cea69148)(content(Whitespace\" \
         \"))))(Tile((id \
         e41693b8-b92a-45b3-bfc8-fbee92eda967)(label(Plant))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children()))))))))(Secondary((id \
         848c7e71-824f-430f-b651-8a8efb889755)(content(Whitespace\" \
         \"))))(Tile((id \
         99a47b25-e65f-45fe-a45d-326561bca029)(label(->))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 13))(sort Typ))((shape(Concave \
         13))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         022ad93c-46e9-407c-9930-df5fdbe4e3d1)(content(Whitespace\" \
         \"))))(Tile((id \
         5bccb6c6-01f6-4278-8f88-16f164ee0b4b)(label(Grove))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         d4095910-6d71-465c-bf45-d179061b282e)(content(Whitespace\" \
         \")))))((Secondary((id \
         70f11459-3423-4e00-ae6e-fea03efbf5a5)(content(Whitespace\"\\n\"))))(Tile((id \
         ca9db6cb-cb26-4e59-a3db-0a4edfec355e)(label(fun ->))(mold((out \
         Exp)(in_(Pat))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         37))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         985a34a1-3d21-49e7-bc13-a20b750e1301)(content(Whitespace\" \
         \"))))(Tile((id \
         cc1afb5f-78ba-495b-b3d9-91ddadbcac05)(label(\"(\"\")\"))(mold((out \
         Pat)(in_(Pat))(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0 1))(children(((Tile((id \
         95455abc-f65d-449b-8f5b-7d53b589e4c0)(label(grove))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         fce21965-a977-41d1-af95-25ddbbf89e76)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 44))(sort Pat))((shape(Concave \
         44))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         d71b06d1-8aba-4677-a9e7-e34c57dc0478)(content(Whitespace\" \
         \"))))(Tile((id \
         c0f14067-5f8e-41c6-8a57-a45791155e28)(label(plant))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children()))))))))(Secondary((id \
         ee260b81-4e6b-429a-9abd-cc4a3409c85b)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         c52f4309-ae40-45c7-a6ea-3374203cec38)(content(Whitespace\"\\n\"))))(Tile((id \
         eef0bc3a-fb42-4b92-993a-221ce81ee5ef)(label(map))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         cae9b9da-c3e9-44bb-805f-3026a7beb323)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         1fc502a4-c277-4b13-aeee-c4a073b1b11d)(label(grove))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         d8acb6a3-8983-46ff-b97f-9764f56f9779)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         8ab9aec9-ef18-4482-9dad-58ed47b4d32e)(content(Whitespace\" \
         \"))))(Tile((id 761ec6ea-1421-4f07-a504-51ece918e20b)(label(fun \
         ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 37))(sort Exp))))))(shards(0 \
         1))(children(((Secondary((id \
         dbae9851-5db3-49c1-9a55-d215e4c123d7)(content(Whitespace\" \
         \"))))(Tile((id \
         f2ae636b-00ef-4510-b1a8-142e2d2c3bc5)(label(row))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         d0378f44-1788-4793-a500-c52c4e173544)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         0b573caf-cb07-4597-b3a1-64dcf0435a4b)(content(Whitespace\" \
         \"))))(Tile((id \
         68a76cf8-dd20-4f64-82d5-82fcb6a5d9ec)(label(map))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         2c492d8a-1509-4839-990f-7e40111ba4d3)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         fff26721-8cfc-4fb7-9b20-2b316c93f0cb)(label(row))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         897b0832-1f1f-4e5f-9308-bcfbbaeb8777)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         d23c151c-5ca9-400a-9159-faef276774c2)(content(Whitespace\" \
         \"))))(Tile((id 013b6513-8100-47e7-a3df-a529159baddb)(label(fun \
         ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 37))(sort Exp))))))(shards(0 \
         1))(children(((Secondary((id \
         8a36c6e3-f79b-499b-bf73-bc4f24e03181)(content(Whitespace\" \
         \"))))(Tile((id \
         58c79535-e344-4ec2-b099-e2a344d20b86)(label(_))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         0d4528c1-979c-4c03-82d5-94bea24bb2a0)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         753c55b4-65c9-44e4-a4f3-0a4bdee5ef15)(content(Whitespace\" \
         \"))))(Tile((id \
         65ec103d-6d23-4b99-8f17-7d7e394b6233)(label(plant))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))))))))))))(Secondary((id \
         dd7bf81c-9955-4279-929a-a66f62aa44ea)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         8bccca62-42dd-409e-aed5-5cb72c97b7fa)(content(Whitespace\"\\n\"))))(Secondary((id \
         8e8d5d18-e269-4f15-b501-ffe811e61801)(content(Whitespace\"\\n\"))))(Tile((id \
         a0f6f49a-7f13-4cc3-8c9b-ef66b14e408e)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         509b9e99-82cc-475f-b9b1-d1043fb4a7a4)(content(Whitespace\" \
         \"))))(Tile((id \
         87375df4-9e9e-4d40-8a4b-5c7ddc395e66)(label(updateGrove))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         2aea49d4-1deb-45d7-a319-fed3e2e35b41)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 24))(sort Pat))((shape(Concave \
         24))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         29b0b39d-3872-4fc7-835a-93d0f04e4aa6)(content(Whitespace\" \
         \"))))(Tile((id \
         3fb8f2a9-089b-4d3d-aef7-c48958aa5717)(label(\"(\"\")\"))(mold((out \
         Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0 1))(children(((Tile((id \
         453b5b90-b18d-43d4-8b23-b73f33725506)(label(Model))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         f009fa63-4619-44e3-9db7-29f3a800482d)(label(,))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 44))(sort Typ))((shape(Concave \
         44))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         c8f4fcf7-b1b0-467b-87ee-bd8a9de9fa0c)(content(Whitespace\" \
         \"))))(Tile((id \
         4d55dee7-e25b-43c3-9ab0-5d05deff59aa)(label(Grove))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         1f8e2f06-10f8-4d5a-aee8-1b3952e653ca)(content(Whitespace\" \
         \"))))(Tile((id \
         6c271536-6fcf-4f97-8756-c7cfce547b77)(label(->))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 13))(sort Typ))((shape(Concave \
         13))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         9bb5a788-8400-4f6e-826b-5482bcc9a199)(content(Whitespace\" \
         \"))))(Tile((id \
         e349d654-d18b-4934-9bfd-72f5ce7dae30)(label(Grove))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children()))))))))(Secondary((id \
         3201abe2-0fef-4b6c-89d3-0847b64f847e)(content(Whitespace\" \
         \"))))(Tile((id \
         a4a6895c-a368-4c1e-8b1c-768603de5f86)(label(->))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 13))(sort Typ))((shape(Concave \
         13))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         4a279346-07ac-40d5-912b-5dbc70b9e9f6)(content(Whitespace\" \
         \"))))(Tile((id \
         94a27e18-83a8-460f-98bf-df2614ae1986)(label(Model))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         d77e921c-2e0b-4019-98fa-32a7dba27cd3)(content(Whitespace\" \
         \")))))((Secondary((id \
         eb4d6f4c-af34-4975-a365-5b36aa632ff1)(content(Whitespace\"\\n\"))))(Tile((id \
         e40b358b-fd69-44a5-bae7-29db572220ca)(label(fun ->))(mold((out \
         Exp)(in_(Pat))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         37))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         5c87198f-ed80-4b44-bc63-1d0302942943)(content(Whitespace\" \
         \"))))(Tile((id \
         b2c04f28-7418-4d54-b57e-2320cf38ef78)(label(\"(\"\")\"))(mold((out \
         Pat)(in_(Pat))(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0 1))(children(((Tile((id \
         cfd49049-dcdd-49ba-b675-38cc8c4da39c)(label(m))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         7b053c52-8c5e-48f1-9f67-3aa038885123)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 44))(sort Pat))((shape(Concave \
         44))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         2f468560-82cd-41fe-a3f7-ab51226b58a8)(content(Whitespace\" \
         \"))))(Tile((id \
         e8ffcf85-53e8-4198-8621-390f6efd2d22)(label(f))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children()))))))))(Secondary((id \
         b64330be-5a7b-41fe-8fba-ba8fc5780312)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         e036dcd3-d7fa-4a88-bf25-3012467270c1)(content(Whitespace\" \
         \"))))(Tile((id \
         541a6694-b9d2-40b0-850f-c915dd626ce2)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         19050401-5771-4cd6-b9b8-281e2b3fde48)(label(f))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         4e1728dd-4abf-4fc5-a314-8c892dfa193c)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         eb523b6c-047f-4271-b98f-65269e1ce5cc)(label(m))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         d2cae2fe-e896-4fe7-adc4-4df9d6c12a79)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         f1345d49-3ac8-46ec-bace-6d83a7fc9553)(label(grove))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         fd31636d-d615-40e5-8e90-f4c3499a902c)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         ac22d074-6d38-41e6-8b29-e6ff8503d7cd)(content(Whitespace\" \
         \"))))(Tile((id \
         3c6fa178-26ea-4ff4-a1f6-2b9047a68d0c)(label(m))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         9419d89b-7ca9-43ae-a636-460bbca95941)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         b1a9bcb3-ecf7-4b7a-a9ab-a819d9597550)(label(currentSeed))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         5577731e-15c5-4d3c-9fee-90c243180001)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         50b4861d-10b6-4467-8dc7-05a214a813da)(content(Whitespace\" \
         \"))))(Tile((id \
         7738983e-4afc-4777-8d8d-910baedf8be3)(label(m))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         57cf5c9b-65d1-4079-9eaf-70756befef55)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         cd58b400-1d50-4ad9-b4cd-9a9771e31713)(label(seedInventory))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         7af57c29-3a29-46d0-90c0-95f0a2b93d3e)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         166220dc-96c8-4908-849b-1fca952244df)(content(Whitespace\"\\n\"))))(Secondary((id \
         edd16019-9254-42c1-8cf0-b144b656c159)(content(Whitespace\"\\n\"))))(Tile((id \
         9942c9b5-5829-496b-93c2-ce3d280f9e62)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         ddebbba0-b2c8-40cf-8fa0-0ee12304a7b5)(content(Whitespace\" \
         \"))))(Tile((id \
         a5155784-0f1d-459d-a779-938b72628c3b)(label(update))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         7b7f0c62-e06b-4a64-8a42-475780beb346)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 24))(sort Pat))((shape(Concave \
         24))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         ddb0f64e-610a-434d-8532-1792ef808055)(content(Whitespace\" \
         \"))))(Tile((id \
         08f12b34-78d5-4b93-bb58-a10264556a82)(label(\"(\"\")\"))(mold((out \
         Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0 1))(children(((Tile((id \
         2f29191a-a8d4-4455-a09e-8cd0b7ca676d)(label(Model))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         60611834-a0c7-4e87-acde-f95f7ad08423)(label(,))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 44))(sort Typ))((shape(Concave \
         44))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         a21aa810-a034-43b9-b782-d21d570c6803)(content(Whitespace\" \
         \"))))(Tile((id \
         01e92a25-07e0-40b8-bfe3-d46bfbf6d38e)(label(Action))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children()))))))))(Secondary((id \
         2a7fcdce-0247-4b39-8f4d-ba2616e18593)(content(Whitespace\" \
         \"))))(Tile((id \
         5e911c1a-25fc-429d-bad9-155cc4cef92b)(label(->))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 13))(sort Typ))((shape(Concave \
         13))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         813484e2-e541-4a41-bfc4-fb356ab32c24)(content(Whitespace\" \
         \"))))(Tile((id \
         7c476267-de43-4125-a739-ab0e065fcfa0)(label(Model))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         d884ac59-95ad-431d-b2eb-e7977413460b)(content(Whitespace\" \
         \")))))((Secondary((id \
         8b43575b-e421-4dac-9097-f3ecf2113dfd)(content(Whitespace\"\\n\"))))(Tile((id \
         3e1a1d77-2e3e-4156-939a-4019337b6665)(label(fun ->))(mold((out \
         Exp)(in_(Pat))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         37))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         d36121fc-99fe-4e7e-b7d3-6f9e3b2c7b44)(content(Whitespace\" \
         \"))))(Tile((id \
         8c89c12a-0477-4f6c-9d73-4658ea0f348d)(label(m))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         405b16f5-6ecb-4265-91c7-c31a3d1ddef5)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 44))(sort Pat))((shape(Concave \
         44))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         b9cc1304-bb75-47d2-b882-0ae8b6038199)(content(Whitespace\" \
         \"))))(Tile((id \
         4ab4d9d0-0d98-4556-8475-10ca937b75ae)(label(action))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         1a8a2ada-4cce-4a31-8eb6-5dbc33db336e)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         a8700893-f77e-40a2-b8cf-6a9000c103be)(content(Whitespace\"\\n\"))))(Tile((id \
         84fa6a82-8365-4ac8-95e9-1208d13129d3)(label(case end))(mold((out \
         Exp)(in_(Rul))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         4b3d825c-6f1e-4947-91ec-ad616088ac64)(content(Whitespace\" \
         \"))))(Tile((id \
         b7bed705-008b-469f-a40f-cda617d4157c)(label(action))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         07c15848-897c-448b-a6bd-ed9e64fa2623)(content(Whitespace\"\\n\"))))(Tile((id \
         7c4768f1-c80a-4520-8157-7a1772040984)(label(| =>))(mold((out \
         Rul)(in_(Pat))(nibs(((shape(Concave 46))(sort Exp))((shape(Concave \
         46))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         8b65e14c-682d-4747-8196-8bbeb81592f4)(content(Whitespace\" \
         \"))))(Tile((id \
         8a52e13c-f4fe-483c-abf1-26a3c351746f)(label(SelectSeed))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         76af312b-af05-4731-b28b-b94420671347)(label(\"(\"\")\"))(mold((out \
         Pat)(in_(Pat))(nibs(((shape(Concave 23))(sort Pat))((shape \
         Convex)(sort Pat))))))(shards(0 1))(children(((Tile((id \
         e89a2dec-e061-481b-8529-7dd83abce965)(label(idx))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children()))))))))(Secondary((id \
         263767f9-0ee8-4aa6-b503-72817f59f7b8)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         3799a74c-09ae-4094-a6fc-5ea3783490db)(content(Whitespace\"\\n\"))))(Tile((id \
         b5a9a682-7b44-49d5-a425-de652f1467c8)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         e47d116a-cdaa-4a7e-a064-54f068f1bf7d)(label(m))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         c72dfaf5-b7c1-4556-afa8-c4654d46d025)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         84f604c9-4750-4d78-9460-20b81f88a227)(label(grove))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         2a015074-0ece-4f8d-b5a4-115a1a85879f)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         b8076f7c-e0c7-49f0-94ff-eb6a98065efb)(content(Whitespace\" \
         \"))))(Tile((id \
         fcbb83ac-e014-48cc-94d4-5f85ed76f725)(label(nth))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         ed6789c2-4122-41d9-8211-5005cde890f0)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         a9788098-1637-4743-b323-2a71086b9cbe)(label(m))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         ee59ea5c-7dfe-404a-aff2-bc0c4a706fe1)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         c6bea0f6-7c7b-432e-877d-746dd28c8170)(label(seedInventory))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         9a6021e7-7657-45ee-9ef1-15516c479990)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         9940e0d5-eec5-429d-80bf-55ad218f0130)(content(Whitespace\" \
         \"))))(Tile((id \
         da1e034c-dee5-44f3-b315-6e8dfddc5b69)(label(idx))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         0f520741-c0ff-4b8a-aae2-b2f334046d60)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         f7685664-5531-470f-8678-4259c9c70914)(content(Whitespace\" \
         \"))))(Tile((id \
         e177f82a-3bb5-4b11-acad-2d2c360be395)(label(m))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         e9acc719-d25f-4367-b642-873ae7a5bd07)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         f5388973-a5f8-4e87-b65b-3d03cedbb0a2)(label(seedInventory))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         eb9b6a9f-cb2a-4044-a4d5-ce91ab06903f)(content(Whitespace\"\\n\"))))(Tile((id \
         90eacc18-327f-40a6-95e7-09b043874c01)(label(| =>))(mold((out \
         Rul)(in_(Pat))(nibs(((shape(Concave 46))(sort Exp))((shape(Concave \
         46))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         907004b0-1a7d-4c21-9b3d-b03c669b9bed)(content(Whitespace\" \
         \"))))(Tile((id \
         d4d7c270-06de-4f58-8c5f-3c4ff4527fbc)(label(PlantSeed))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         ebd49eb7-6eb0-44f5-bd44-eeb3c9e48a17)(label(\"(\"\")\"))(mold((out \
         Pat)(in_(Pat))(nibs(((shape(Concave 23))(sort Pat))((shape \
         Convex)(sort Pat))))))(shards(0 1))(children(((Tile((id \
         2ef0d54c-dfe3-4523-8ee0-c605d8e8f330)(label(row))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         c7782106-b345-44d7-98c2-2cb7341fa36d)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 44))(sort Pat))((shape(Concave \
         44))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         785e3dea-003f-4314-82ca-a2b65fee298e)(content(Whitespace\" \
         \"))))(Tile((id \
         e44fbecc-7a27-4fbc-a859-77ad5523efc0)(label(col))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children()))))))))(Secondary((id \
         47025118-6f5d-4b9f-8656-9147f3f1f4cc)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         7a42b7cd-529a-4da2-a259-ebaece32e055)(content(Whitespace\"\\n\"))))(Tile((id \
         f9083449-5425-4a10-b027-849b10820ab4)(label(updateGrove))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         6edc6a6d-d849-4455-b729-baae6bcbc041)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         3815f6ba-d6f1-4757-9567-71f33d696d62)(label(m))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         b9b9706a-288c-4869-8f1e-daf80ac05cc3)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         b1069131-f2ce-4a22-88a1-90a980c0edd0)(content(Whitespace\" \
         \"))))(Tile((id 661e3055-a12f-4554-a8c4-e922a182bd72)(label(fun \
         ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 37))(sort Exp))))))(shards(0 \
         1))(children(((Secondary((id \
         faf5930f-c0f8-49fe-8412-e1708e94b857)(content(Whitespace\" \
         \"))))(Tile((id \
         18ea446c-4e60-4383-a971-8cab9237cbf2)(label(g))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         2ee9876b-78a4-49b2-86ab-a67d63c3adc2)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         de4b8c7d-1ffc-4af7-b4f0-1cb881fd8e45)(content(Whitespace\" \
         \"))))(Tile((id \
         3d578975-257f-459e-8c70-303578416d85)(label(setCell))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         b7135b79-b980-4969-809e-d4aaee0ba8c0)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         0777dc5a-8b5e-4f4e-8216-40a6b504d157)(label(g))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         42c665ac-f0c1-47ff-8399-cc95a57763b1)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         4b482797-32ca-4873-bcb6-8373203a4c7c)(content(Whitespace\" \
         \"))))(Tile((id \
         a7fe8875-3e25-4b1b-ae30-30292e4b502e)(label(row))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         4a6b7b43-ea52-4ab2-adda-a1a71134f1c2)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         e7daa051-7aac-4eec-a5da-ad4c793317db)(content(Whitespace\" \
         \"))))(Tile((id \
         375110e0-e1df-4c3b-b95b-ed51e1c6d398)(label(col))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         8d749fea-1194-4ac8-8725-9ab98a267ecc)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         cfe4c0a4-da22-4ab2-8a20-c53b11cd7b41)(content(Whitespace\" \
         \"))))(Tile((id \
         eb6a6fda-6049-4c93-89d1-2d5aead3d2a9)(label(m))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         938b27cd-f9ff-4300-a3c1-f4e1a1c914db)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         22887c5e-ed0c-4ce7-b249-20a275b3cd92)(label(currentSeed))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))))))))))))(Secondary((id \
         3133cea1-acb2-4115-868e-1b5aa0428313)(content(Whitespace\"\\n\"))))(Tile((id \
         567e36ab-ab6a-41dd-8683-cfc83ff54f9b)(label(| =>))(mold((out \
         Rul)(in_(Pat))(nibs(((shape(Concave 46))(sort Exp))((shape(Concave \
         46))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         a2cc373b-963b-44fe-950f-a45d05749622)(content(Whitespace\" \
         \"))))(Tile((id \
         88ef9f1c-b743-4913-a1d3-172e216e76d2)(label(Uproot))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         ae0e398f-4628-415b-affc-c39cef4122ac)(label(\"(\"\")\"))(mold((out \
         Pat)(in_(Pat))(nibs(((shape(Concave 23))(sort Pat))((shape \
         Convex)(sort Pat))))))(shards(0 1))(children(((Tile((id \
         105b317e-240a-4663-be77-0b1dabed9985)(label(row))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         0f5eaac4-913b-4953-a20b-08a94d1704e4)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 44))(sort Pat))((shape(Concave \
         44))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         83453a68-be06-4ecc-a5bd-b6fb355fde6a)(content(Whitespace\" \
         \"))))(Tile((id \
         5c8acc8b-e80d-488c-8088-2cd9442a4520)(label(col))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children()))))))))(Secondary((id \
         77393472-9f35-4350-9739-08205d1e39fe)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         bce58880-1a7c-4923-a30b-3f8466ca76f5)(content(Whitespace\"\\n\"))))(Tile((id \
         2692c955-c5e5-44e2-babf-c94c5be8975b)(label(updateGrove))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         2bee92aa-a55f-411f-9baf-0085e6b293d1)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         7b6b48c1-8461-4e04-95fa-383138bbec6f)(label(m))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         307b5475-4010-4f82-baf3-98f86a5673df)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         165e3fad-ed5a-4426-994b-b76899c8773d)(content(Whitespace\" \
         \"))))(Tile((id b090ec7d-7371-40ba-bfa4-d3d6a9244857)(label(fun \
         ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 37))(sort Exp))))))(shards(0 \
         1))(children(((Secondary((id \
         5e59a794-61bf-46ce-a961-0078d23b9f15)(content(Whitespace\" \
         \"))))(Tile((id \
         3b0afff4-5bec-417f-b958-ea593a185b16)(label(g))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         4d4b2d01-b282-4618-98b4-a715beaf56b1)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         c4e2416f-b719-45ae-a9fc-1daf4d3848d1)(content(Whitespace\" \
         \"))))(Tile((id \
         61d566f3-d410-4b19-9f5f-54e264e37aee)(label(setCell))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         80100854-15e8-412a-a868-a4b970f882fd)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         3c2fbbbe-63e1-4801-b6a7-f7c6b5ad6d0e)(label(g))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         d76c5230-ec6c-4743-be81-31cceb0dafbb)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         4cde5598-e423-4088-a592-ca879802762f)(content(Whitespace\" \
         \"))))(Tile((id \
         15b57896-8c6a-475c-a381-a119d359d162)(label(row))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         96504d21-c074-459f-9268-4f72ecc821e4)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         98c796ec-36cf-4d43-b96f-3b359eaf9c32)(content(Whitespace\" \
         \"))))(Tile((id \
         c7aeb93b-02f4-4a2a-9244-270159e37144)(label(col))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         0f1f2141-6d94-4f7a-b608-8e2e7dcbe54c)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         eee6c30e-1b4a-4be8-8e29-44696b453be8)(content(Whitespace\" \
         \"))))(Tile((id \
         73d59a2b-182e-4b90-b5e5-b69323eabf85)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))))))))))))(Secondary((id \
         400ce7b6-434c-4407-ba7a-7776d942e820)(content(Whitespace\"\\n\"))))(Tile((id \
         fe54081a-153c-4198-89f4-f21039f197bc)(label(| =>))(mold((out \
         Rul)(in_(Pat))(nibs(((shape(Concave 46))(sort Exp))((shape(Concave \
         46))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         298219db-6708-4600-8f31-5354602ca30f)(content(Whitespace\" \
         \"))))(Tile((id \
         025a8227-fb1b-46f2-9de3-2d351b500a86)(label(ClearGrove))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         2c5b3700-3b74-46b4-92ba-637eede40222)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         c57a8f3b-9285-4d49-b6e6-b334eca9b0f4)(content(Whitespace\"\\n\"))))(Tile((id \
         60292438-914e-4701-9f72-5fa7f2debb2b)(label(updateGrove))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         4db5c179-4dbb-4c83-9fce-19a2ddc514e8)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         5c77aebb-0cf8-4c31-987e-8ee26f370da8)(label(m))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         bae113bd-bcb2-475c-97a0-e316baabef8d)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         624a1be2-9f2d-4d5b-b4ed-a76b8fbf288a)(content(Whitespace\" \
         \"))))(Tile((id dddea20f-3020-46dc-be16-b6ff03ef4b6b)(label(fun \
         ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 37))(sort Exp))))))(shards(0 \
         1))(children(((Secondary((id \
         277e2025-2ca7-4607-ab3a-ef4f5b1fed54)(content(Whitespace\" \
         \"))))(Tile((id \
         a4ce6dc9-b8d0-4d7e-8903-dd394924bb11)(label(g))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         a145fea9-fb91-4e76-b296-224d6c9da227)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         4603e89f-e4f8-4032-a86c-36fc2b40f05f)(content(Whitespace\" \
         \"))))(Tile((id \
         7f4262bd-9165-4585-902e-9412b32e7e12)(label(setAll))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         53db1b78-3d4e-492e-bd25-7a3b331399a9)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         b9d6529e-2c77-4ffa-acce-1611724c6091)(label(g))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         32f84cbf-f9f3-4b98-9a46-f2ceb0935bcd)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         34d885a0-a1a5-4a47-84fd-70be53c35115)(content(Whitespace\" \
         \"))))(Tile((id \
         b3c633fc-f16a-405d-bdcb-109f75c6befa)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))))))))))))(Secondary((id \
         d2b004a0-6b43-4a54-9c9f-8bef063a326f)(content(Whitespace\"\\n\"))))(Tile((id \
         81ffdf3c-d364-410f-ae32-11f7ae2a7ea0)(label(| =>))(mold((out \
         Rul)(in_(Pat))(nibs(((shape(Concave 46))(sort Exp))((shape(Concave \
         46))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         c051f729-235c-446f-8f24-6049ac423d4d)(content(Whitespace\" \
         \"))))(Tile((id \
         12e0609e-a181-4c9d-b20e-efecf43c926e)(label(PlantRow))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         f065103f-19db-4085-9fdf-0ecc68cee411)(label(\"(\"\")\"))(mold((out \
         Pat)(in_(Pat))(nibs(((shape(Concave 23))(sort Pat))((shape \
         Convex)(sort Pat))))))(shards(0 1))(children(((Tile((id \
         4075f0c6-1b5b-4686-b08e-7c7c40d24f71)(label(row))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children()))))))))(Secondary((id \
         a9737554-788b-484c-b905-2ad61a1e43cf)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         45e20864-898a-4c43-b873-d963666cef1c)(content(Whitespace\"\\n\"))))(Tile((id \
         b2737a53-289a-482b-a33e-4f7c33b35ae0)(label(updateGrove))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         a43f233d-7b8f-4bcf-8b04-5d61fdbc27d9)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         8d7a9527-717f-4846-9503-777f6786daf6)(label(m))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         7d437c01-0f95-46d3-9c46-d73eb096f54e)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         d53e09ae-48ca-4181-bec1-d69756f44c59)(content(Whitespace\" \
         \"))))(Tile((id 7ff72bb1-2c2c-4f34-bb3b-408da310aec1)(label(fun \
         ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 37))(sort Exp))))))(shards(0 \
         1))(children(((Secondary((id \
         614ee4a7-c3f0-4419-bcb6-15c9cdc59735)(content(Whitespace\" \
         \"))))(Tile((id \
         3b7ab2d1-41ea-47b6-b04f-f2b7448311ca)(label(g))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         fcd2e12c-3a6c-41fb-b693-bd0f8f04c61f)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         419d40a4-a151-4d2d-9fb7-c24fd9a1d48d)(content(Whitespace\" \
         \"))))(Tile((id \
         1ef51c37-cf7e-4b14-a7f8-e0f4b38c3c5b)(label(setRow))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         655e9d36-df52-49c9-b030-ec685a573832)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         b03a8067-8de2-461d-bed1-d1c874a339f6)(label(g))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         2d24a3c7-1ffc-473f-bc82-6b6e8b9f561d)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         d1a3194c-c8af-41f4-8df4-fa9c6e0ea346)(content(Whitespace\" \
         \"))))(Tile((id \
         1e1924b4-633b-4138-b237-ca0d7a8b2803)(label(row))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         1a20b240-d173-4a40-b6d4-050daa422804)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         db2c59ac-6ef8-4b78-bc98-51a717d46dd1)(content(Whitespace\" \
         \"))))(Tile((id \
         e53dfc7a-9e13-49b4-a0ef-cd3ce262f791)(label(m))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         148250e3-6982-4115-9884-7780380144aa)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         fff7b208-a53a-4303-8a79-091720457cba)(label(currentSeed))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))))))))))))(Secondary((id \
         8ddb9d70-abde-4076-8b55-e7bd032ee5c1)(content(Whitespace\"\\n\"))))(Secondary((id \
         9264781c-e171-4461-b572-577c0cc9f58f)(content(Comment\"# TODO: Add \
         PlantCol case here #\"))))(Secondary((id \
         12a7c1c9-4f30-4d34-b75f-d128620c5d34)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         63bbda4f-189c-40ec-9c1a-b04eaecb253d)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         6f41e8c7-7bf7-4c56-80e8-09e7c89e83df)(content(Whitespace\"\\n\"))))(Secondary((id \
         751830f6-3f4f-474e-ba5e-698910ae0fc2)(content(Whitespace\"\\n\"))))(Tile((id \
         2f2bfa17-8721-4fcb-bc10-51809b23a46f)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         2c9b1da4-04ff-40de-a0e4-1b00cf52b7bd)(content(Whitespace\" \
         \"))))(Tile((id \
         db8b0571-e397-4412-be55-3386ae21f047)(label(do))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         148aae98-2961-4d79-b381-e35abe33792c)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 24))(sort Pat))((shape(Concave \
         24))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         a9efeec2-abbd-4140-928d-88d46080222a)(content(Whitespace\" \
         \"))))(Tile((id \
         a8991d67-eac5-4044-92ab-85cfeb00e7ce)(label(\"(\"\")\"))(mold((out \
         Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0 1))(children(((Tile((id \
         627ccbed-8c92-42de-9ce2-1311e2f23e22)(label(Model))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         f7a4c314-d2d5-468a-8f1f-12de6d7a5397)(label(,))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 44))(sort Typ))((shape(Concave \
         44))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         36bbc199-227c-4d7a-a9cd-2d0beda58558)(content(Whitespace\" \
         \"))))(Tile((id fa021e8b-d478-4ba5-a13a-7059c3594fd4)(label([ \
         ]))(mold((out Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape \
         Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
         4c8e69b9-4db4-4bc5-a7a5-736f0cb60fd2)(label(Action))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))))))))))))(Secondary((id \
         34990b2c-c0b4-4f88-921a-4e6a1b709d46)(content(Whitespace\" \
         \"))))(Tile((id \
         dc45667f-aeb3-45f5-9d7e-ffab01c0d84d)(label(->))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 13))(sort Typ))((shape(Concave \
         13))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         f0e916c6-2509-4ff4-ae5b-30afafc93180)(content(Whitespace\" \
         \"))))(Tile((id \
         e47ba204-2e4b-4e6b-8554-0b9020ae464d)(label(Model))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         8d75a723-f0e9-4070-bb82-0afee8f20d21)(content(Whitespace\" \
         \")))))((Secondary((id \
         bd152acf-adf9-4f21-ae80-4ca5d32cf7d0)(content(Whitespace\"\\n\"))))(Tile((id \
         d56cb250-e397-4226-9619-908b2de8c890)(label(fun ->))(mold((out \
         Exp)(in_(Pat))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         37))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         4d2d2d3d-0d05-49a0-984b-96fc9e4506ad)(content(Whitespace\" \
         \"))))(Tile((id \
         6c59bf48-df80-4616-a6e1-3bd3c37d68f7)(label(\"(\"\")\"))(mold((out \
         Pat)(in_(Pat))(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0 1))(children(((Tile((id \
         8df683a3-37a4-4952-805c-a892bfbc2fb3)(label(init))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         b4d08534-28fe-47ff-9997-adb51948707d)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 24))(sort Pat))((shape(Concave \
         24))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         e5b781a8-4408-47cc-8391-7511e4996fa1)(content(Whitespace\" \
         \"))))(Tile((id \
         3981e321-6430-4a5d-b9c6-f5e249fc6d47)(label(Model))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         a648b7be-1b17-4506-8676-4dd07f9868af)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 44))(sort Pat))((shape(Concave \
         44))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         219af6b0-f9c6-4414-b673-58f6936969ec)(content(Whitespace\" \
         \"))))(Tile((id \
         d08bed65-53fe-41c7-a917-7a9f97b951c1)(label(actions))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         99a0e542-79db-4f21-85b0-14360ce0521d)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 24))(sort Pat))((shape(Concave \
         24))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         0a0686ed-c94a-4051-8e57-cc0aafd33abb)(content(Whitespace\" \
         \"))))(Tile((id 69530984-7a1c-45cb-9c92-678f1cbd4212)(label([ \
         ]))(mold((out Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape \
         Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
         5032d6cf-8d32-45f7-9540-c118321370ad)(label(Action))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))))))))))))(Secondary((id \
         8ff31af6-4295-4158-90a8-4c6664cccc55)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         fae075b6-bfc8-4db0-a38c-99421ce38fe3)(content(Whitespace\"\\n\"))))(Tile((id \
         42e26ea3-1db9-4e5a-a2ba-bc2d9fca3659)(label(fold_left))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         f75e1abe-965b-48ec-b830-daf6a30b6684)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         3f957ea7-5d10-4ccc-920b-a30ec5ff56d0)(label(actions))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         74d9b8b8-68ba-4c75-804a-18ab0991d148)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         d9542ff0-42e4-4e5d-b454-69ea2ba22e69)(content(Whitespace\" \
         \"))))(Tile((id \
         5dfbeab1-9735-4c1d-b47d-df218099a378)(label(update))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         e96c6136-b847-4b4a-ac3e-5e6c4fc621e2)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         85c62296-ce63-4213-b8f7-c4a3c89a73a4)(content(Whitespace\" \
         \"))))(Tile((id \
         2080c0b3-01fd-4b2a-b42c-221d7094708d)(label(init))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         df534735-0160-45fe-af50-6e971fc206a3)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         a69acb97-72da-4770-aae2-ebbbfd48c12d)(content(Whitespace\"\\n\"))))(Secondary((id \
         26a966e3-fcab-4234-963b-60a6cbf27bb7)(content(Whitespace\"\\n\"))))(Secondary((id \
         ead7da90-93ad-495d-90e8-cc16d84182a2)(content(Comment\"# Existing \
         tests #\"))))(Secondary((id \
         f6bb8e08-1ac4-4e4e-9274-8f1ee755c162)(content(Whitespace\"\\n\"))))(Tile((id \
         cf649bf8-76c2-41b8-9bd2-de1aaa5af729)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         42be04d1-951d-4bf9-8849-92c9892c32e8)(content(Whitespace\"\\n\"))))(Tile((id \
         4de0dcec-de83-4f5e-8467-16e2971138ae)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         047aed56-1226-42d7-960d-145dd070ad2b)(content(Whitespace\" \
         \"))))(Tile((id \
         bdde7285-d4e0-4193-9394-9a8f5dcb72c1)(label(m))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         cb69ba69-0446-4023-a4dc-bdf7fe6d1f77)(content(Whitespace\" \
         \")))))((Secondary((id \
         fa4be79e-028b-4927-9dbe-cca4fff83477)(content(Whitespace\" \
         \"))))(Tile((id \
         8bff7a7d-3525-4d35-b5ff-c1624ff647b8)(label(update))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         7f51c7ba-beae-4830-8d2e-507feb019e9c)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         a31cfdd6-d072-4101-9ec1-f6330035290d)(label(init))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         520a51e5-7771-4ea5-a589-a9c998f1263c)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         e39fe1fd-cff5-42f8-8af4-77966de957f6)(content(Whitespace\" \
         \"))))(Tile((id \
         093e3d82-c791-47b6-bcf0-671dc8d36678)(label(PlantRow))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         31da9285-4743-441c-9b4d-a3d9ceea26fe)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         b4010e26-7075-44c7-8b83-512914158cba)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))))))))))))(Secondary((id \
         089ba3fb-f5f7-4499-82bd-e9901d2d8f9f)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         a189909b-c930-4d9e-b57c-c9116cca5530)(content(Whitespace\"\\n\"))))(Tile((id \
         d2324669-d707-46ad-bc0a-34c7fd7709aa)(label(m))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         02e77722-d736-4599-bb1c-42877f9052ab)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         faeb5de7-2bf7-466b-809b-2e7e6873b163)(label(grove))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         71860aed-b512-4f65-868a-c06de62211a0)(content(Whitespace\" \
         \"))))(Tile((id \
         4b989655-ada4-41e9-b292-ff5256d4b6d2)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         9c8e2577-a5d2-41e7-9390-16057e72caa3)(content(Whitespace\" \
         \"))))(Tile((id 6a82dfca-ce45-4b9f-bc4f-f0011f5b48e9)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         dca085f1-5e68-42c5-ac69-c6fb54c604b0)(label([ ]))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         74c265db-dec4-49db-8545-5e61b594e913)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         e13f1b68-cb5f-476e-a222-972c095c9609)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         47532f08-88fc-4806-acdd-f0b87b283e02)(content(Whitespace\" \
         \"))))(Tile((id \
         84516d5d-6969-48de-90b0-f85ac1062618)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         edf82c74-c01c-48ce-838a-9f6c17e4fde4)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         1a7fb97e-5475-47d6-a669-1bbc9485ad57)(content(Whitespace\" \
         \"))))(Tile((id \
         4d1685b3-bbfb-4917-bedf-36b9021df656)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         df16019e-65b0-463e-8e60-17e1c3fa61bd)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         ff3365d2-b6cc-4b00-8a79-cc4082f513e5)(content(Whitespace\" \
         \"))))(Tile((id 09133a9e-7a1a-4f26-ac86-4476b336b8ae)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         5f63de5b-1d6f-45e6-b47b-9949a4539116)(label(\"\\\"\\240\\159\\140\\177\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         bb12473f-8f5e-47b9-b092-ad33aaba9a3c)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         3e1f59f4-ab84-4f90-a16c-ff256136ba52)(content(Whitespace\" \
         \"))))(Tile((id \
         872766e1-2b50-4d7f-b7b7-07e09d70b2e2)(label(\"\\\"\\240\\159\\140\\177\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         99010877-81ea-4f43-93b9-02bb87a0144e)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         4a89e24f-302a-4588-b280-dc48c2ec6179)(content(Whitespace\" \
         \"))))(Tile((id \
         a640560c-9c52-491c-a960-5899330c93ba)(label(\"\\\"\\240\\159\\140\\177\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         607fbba1-0c32-4218-9d28-a39831e3e567)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         ba4d60a0-1ffd-40bb-9885-a37c5da1c24e)(content(Whitespace\" \
         \"))))(Tile((id 7b6187ce-8e8f-4b7b-b158-c7452dba7e52)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         22af7287-13f0-4ee6-9473-7221402a9f65)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         55bd40ab-1a10-49be-bdce-833b0f0ccf2c)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         5f2e1991-128e-49c5-8f0f-941651f0ce18)(content(Whitespace\" \
         \"))))(Tile((id \
         1bc3007b-5839-4076-a69b-c66b2bb35142)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         96d5567f-e3b1-4627-a0cb-b12c9139665c)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         dff98a42-4297-4675-8827-8ce704f43f94)(content(Whitespace\" \
         \"))))(Tile((id \
         718deb2c-100e-415f-8a81-9ba4ccbd605b)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))))))))))))(Secondary((id \
         81b58572-a86f-44f1-b299-2b5c530f5504)(content(Whitespace\"\\n\")))))))))(Tile((id \
         8b3c3581-2d8a-44a3-a88d-3fb8c2de4e52)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         d5c86adf-b9da-48d6-9fe2-4da1d883fa10)(content(Whitespace\"\\n\"))))(Secondary((id \
         8befa881-8a6a-40d9-afa2-c6683e8815fa)(content(Whitespace\"\\n\"))))(Secondary((id \
         b9a1a582-d55c-4697-abd5-99e3e4e9e493)(content(Comment\"# New tests \
         for PlantCol #\"))))(Secondary((id \
         59d72752-515a-4436-90ea-df9a3cddade0)(content(Whitespace\"\\n\"))))(Tile((id \
         b6b0ca41-b156-40ea-af5b-05c7ae0beab7)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         f7f7fc2b-015c-4ae0-bbe2-d2a734d9fa70)(content(Whitespace\"\\n\"))))(Tile((id \
         1a4d2df8-e81c-4dc9-945a-1c96147583d8)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         95cec034-1b6b-4e7c-85a1-c248db1e5d04)(content(Whitespace\" \
         \"))))(Tile((id \
         7d263276-14f4-4e23-a6eb-8780afbac031)(label(m))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         6a93f1ae-0070-4333-aca8-a8abc0a0dd2d)(content(Whitespace\" \
         \")))))((Secondary((id \
         daeb3d46-aea1-45d8-9772-d89a95f37f8f)(content(Whitespace\" \
         \"))))(Tile((id \
         9f2d573e-5ae8-45fb-b18b-859ff06fefc2)(label(update))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         78a1c7ed-f90a-4881-8ad2-aed835430bf2)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         b5447bab-a287-43db-94bc-a1754b11ddf6)(label(init))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         764eebe0-d9d5-46a9-a07b-c8cb0bbb7d15)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         25fe1dbc-a6a9-4da9-8c2a-6b339a1a9b4e)(content(Whitespace\" \
         \"))))(Tile((id \
         d6350f0d-ff51-4f16-bb22-f1e2ba8c90ba)(label(PlantCol))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         c67d0fad-bc36-4a2a-8e17-d02ee48a8fa1)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         45ffc98b-cbe1-4fad-a307-f2b718672099)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))))))))))))(Secondary((id \
         d3e60c7e-7e98-4524-bbcc-3aaa1fd2116b)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         6ddc0846-bf53-407d-9364-29b9766c5187)(content(Whitespace\"\\n\"))))(Tile((id \
         fc53907a-8fce-45dd-bd57-34e253661f89)(label(m))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         5c28cf85-beef-493c-90b3-221b042a983c)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         493c7894-16d7-4e46-b0f2-76caf2026ada)(label(grove))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         c6365f52-52ff-4b4d-a816-de561c84fab1)(content(Whitespace\" \
         \"))))(Tile((id \
         f4aa7aec-edc7-40c6-aeef-3e0a305f8e6a)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         f45267a9-affc-4f65-8175-c581e06d0983)(content(Whitespace\" \
         \"))))(Tile((id 86838853-b797-4129-a47f-2764eec4e5bc)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         d912e877-00f0-44e3-b768-17a76cebb9fc)(label([ ]))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         75883e70-6377-4b29-b6d4-7bef8c2ba74c)(label(\"\\\"\\240\\159\\140\\177\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         fa074473-dbc7-43b8-b67a-9e5281d6381e)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         c92fe964-a4fe-4290-b1a6-a52b36ba89ee)(content(Whitespace\" \
         \"))))(Tile((id \
         d88e2305-7e07-417a-a380-26a33efc1575)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         2430bf52-1637-42aa-98b9-ea29c5f4aa90)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         25ebe246-6653-40ad-888c-fc5009c13060)(content(Whitespace\" \
         \"))))(Tile((id \
         2fef4dab-1cd9-43d2-bbf2-e05eec5d097f)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         dd468af2-030e-4e0f-931f-4695cb4cf14c)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         b4fbb4c2-b4a5-41fb-82f2-8ec10cdbb2cc)(content(Whitespace\" \
         \"))))(Tile((id a8a062eb-3894-49f7-b4bb-03bfc0bdd66d)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         c19caf79-83b3-41b4-9aa7-0d9ab1fe2b74)(label(\"\\\"\\240\\159\\140\\177\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         5f77484e-9507-4032-b0cc-2043321f8b8c)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         7448fddf-c159-4027-81cf-64e2a8f4e41e)(content(Whitespace\" \
         \"))))(Tile((id \
         8de69235-2b89-4136-b283-7c6f7a672818)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         3ae6b4e1-bf0f-4ca9-bbce-29041ecd2c28)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         fe4c1d91-35da-4426-9ff5-8e69b56e297d)(content(Whitespace\" \
         \"))))(Tile((id \
         648e5728-f1e5-46c1-a267-a9816e854c36)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         ef533f7a-2014-4d2c-a2d7-371550aff973)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         c1bffd9f-1f23-49fd-9866-bcc80fcb4040)(content(Whitespace\" \
         \"))))(Tile((id 6c5715ea-138e-4714-9031-04e5602fa21d)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         ac2a1018-b6a3-4c56-a487-a91fe971d899)(label(\"\\\"\\240\\159\\140\\177\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         98be459a-079c-4e3b-9d74-3d9af9075e17)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         27d77459-e9fb-4ac2-8289-c91e2d32e1e2)(content(Whitespace\" \
         \"))))(Tile((id \
         66d3129c-c5b7-4eea-baf9-303ca341a918)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         a72246ba-deb1-4799-999d-fb206fca190e)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         9b652250-32f5-417b-b23d-32e0623dba2a)(content(Whitespace\" \
         \"))))(Tile((id \
         f448bbc5-327a-4f1d-99ee-c538ab983a14)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))))))))))))(Secondary((id \
         3fb27883-24ea-4cf8-91b2-d1ba5a1d6102)(content(Whitespace\"\\n\")))))))))(Tile((id \
         f6f37e5d-5d14-4ddf-9955-85af1d99fe52)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         d0a3021e-c2d8-4522-8d9e-117fc81924a9)(content(Whitespace\"\\n\"))))(Secondary((id \
         6ceb08a5-c9a8-4954-822b-a6f1f4add6ac)(content(Whitespace\"\\n\"))))(Tile((id \
         5c4171cd-e6e6-473a-9664-828a8e2840e3)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         35654b22-811c-4c1f-a5f8-fd447f85d224)(content(Whitespace\"\\n\"))))(Tile((id \
         f3efc83c-6962-4a50-8b73-6ec15d86cfa1)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         d81b8fac-435c-4677-b855-6a753546518a)(content(Whitespace\" \
         \"))))(Tile((id \
         8e0df9f6-952b-444c-9c91-e1dcad390f23)(label(m))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         753626b6-2a5d-4283-b809-1f15267a43bd)(content(Whitespace\" \
         \")))))((Secondary((id \
         9d564c4b-865c-472c-a683-911a7546cc49)(content(Whitespace\" \
         \"))))(Tile((id \
         81043243-4395-4143-adeb-07abcc67a100)(label(update))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         7d800ac6-0eaf-43e8-a9e4-1550b12963b5)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         df40c7ec-3d85-415e-860e-05b51d940283)(label(init))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         71fcbfa5-3855-483d-9d0e-2329fc6fac40)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         4bb625c1-56f9-440c-ba4e-c6d3ba34da80)(content(Whitespace\" \
         \"))))(Tile((id \
         3e15f384-96c6-4aac-ad2d-9d97bee72c07)(label(PlantCol))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         610c559d-1df9-4e76-a7dd-bef3028b6e85)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         e431a256-e2be-4b08-95da-b7b388480493)(label(2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))))))))))))(Secondary((id \
         48f3f64c-5de0-4a69-b772-a4a4fb0eaaa9)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         8d88461e-4608-46f5-be74-0e1761da899d)(content(Whitespace\"\\n\"))))(Tile((id \
         d11e469a-574d-488f-b441-efd482dc5552)(label(m))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         408890da-a79d-4aeb-958c-d9d462509b4f)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         7f000847-0b63-4e66-ae06-8ebfe045720c)(label(grove))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         42baf92e-0277-40ad-b75c-deaf074dbe72)(content(Whitespace\" \
         \"))))(Tile((id \
         3ba7378b-61b0-411e-a6b1-a16cb1275f12)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         70055647-a201-4266-b0eb-2e9794af7815)(content(Whitespace\" \
         \"))))(Tile((id d0909066-372d-41b7-9104-2d690bd754c5)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         9443408d-b679-4de7-b38f-81b9f265a5d5)(label([ ]))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         22294318-d898-4cac-9588-81977a10fb25)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         5c9a3bfa-3cb3-47d8-a8a0-5e68a715bdb7)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         8467e509-b85c-456f-838b-09b49a739d4b)(content(Whitespace\" \
         \"))))(Tile((id \
         8c080362-ec4d-4522-94c4-ce1296b3d598)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         aa2ead0d-15c8-4fde-be80-183fc6c9a869)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         ad332353-d5f7-4a99-90bf-d1b69d6275fc)(content(Whitespace\" \
         \"))))(Tile((id \
         3b7aa143-cd55-4513-a4f1-8225dada0d1c)(label(\"\\\"\\240\\159\\140\\177\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         2c06e0bd-56ae-4949-9511-7a6ccbb4b34d)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         a0f98328-1767-45eb-a644-c35e433f9e98)(content(Whitespace\" \
         \"))))(Tile((id 92ce239b-2627-4480-9a2e-c0df3cfe750e)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         7f3b8435-c07d-436d-9b20-9c5f0ccffb82)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         4bc3a19b-398d-4217-b7e4-5b6d39b2fe15)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         4378b092-ea9e-4e72-84c0-c9488f4c75bb)(content(Whitespace\" \
         \"))))(Tile((id \
         ee91d5c3-6f81-4345-bd54-09b7855d6a7e)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         b6adff5d-eb99-445b-a055-bf47629882a5)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         e6c71abf-40b7-4922-9f18-eb5b6c4e6c9d)(content(Whitespace\" \
         \"))))(Tile((id \
         5372327d-b1c3-4f58-9911-6108e65102be)(label(\"\\\"\\240\\159\\140\\177\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         85603a84-f8d5-4bea-a836-f70d119ad763)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         eae530a3-bff2-41cc-9bb0-b5ebf908d0dd)(content(Whitespace\" \
         \"))))(Tile((id def8fe30-8f1d-4e4b-a808-52c57d93af65)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         954fe646-ddc4-4be9-b420-52ec54d4bc44)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         12a86a66-fdf3-4522-97c8-5f854eb6947d)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         d77f4b93-d65b-4a1e-9db1-8575d42d92ca)(content(Whitespace\" \
         \"))))(Tile((id \
         ec95bf1c-488c-4f6a-8615-adfeebda9604)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         d2b166b1-72dd-4487-a15f-16b4366ea540)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         b7bb71fe-5cfd-42c8-902e-4d8d0a0e65ee)(content(Whitespace\" \
         \"))))(Tile((id \
         6d4577bf-2741-4d20-a18b-0a736bc88c18)(label(\"\\\"\\240\\159\\140\\177\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))))))))))))(Secondary((id \
         1bb10935-bd55-4fbd-946e-ceb6e04888c7)(content(Whitespace\"\\n\")))))))))(Tile((id \
         c8cd1599-6c6d-46f5-9020-c809e3dc10fa)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         b4ae9c6f-05f8-46f5-a363-ed8b8de2b7bf)(content(Whitespace\"\\n\"))))(Secondary((id \
         81846a56-de7f-4482-930c-50a483298bca)(content(Whitespace\"\\n\"))))(Tile((id \
         761879a3-e3ae-407b-85fa-0bc9528041ac)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         54a0b1d9-7d62-4ede-ac51-7eec92a821fb)(content(Whitespace\"\\n\"))))(Tile((id \
         6c574b92-717b-4136-9d66-701a3e8f1413)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         5cf9e50f-1a16-453f-8fa2-2d9d16cceab8)(content(Whitespace\" \
         \"))))(Tile((id \
         46e9db85-86f2-4679-8a9b-1a0425e29322)(label(m))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         a6c28320-66e2-425d-b782-f63fcaea97e1)(content(Whitespace\" \
         \")))))((Secondary((id \
         8eb87425-8d2a-4271-b37a-0e4d58d54287)(content(Whitespace\" \
         \"))))(Tile((id \
         acd8fe3e-68d8-4b0e-8a6f-2b039db9a3f8)(label(do))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         91322a4e-c535-4701-8a17-571472e177c5)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         7b5a7c40-970e-4d26-a414-2e0929175258)(label(init))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         bfc1e678-1517-493d-8ce8-0568f37e5ce1)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         ec88150c-9af4-4e9e-8c4c-401b78a18f89)(content(Whitespace\" \
         \"))))(Tile((id 7806a997-7543-4c53-8d8d-f957072cf965)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         a68e8eea-9bd3-4313-844c-741005377238)(label(PlantRow))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         d885e1de-b4fd-49c4-b94b-f2a31b746448)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         3aa7d621-59cc-40b6-ba7f-3fde43c1edfc)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         ce1a6355-7b49-42b4-8008-403e14ccd1e5)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         ef108833-9f87-474a-957e-75f75d26c0ce)(content(Whitespace\" \
         \"))))(Tile((id \
         4bf4f92a-77be-4bb2-8004-f85636ec4af7)(label(PlantCol))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         dca03e4e-fe36-44af-abe0-a0f03abb7f7c)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         4411a766-9283-4e3c-b192-21d695791fda)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))))))))))))(Secondary((id \
         69901cf6-efd8-4cd7-9f91-78f24e3a146e)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         5638d163-bdd2-40b9-9338-de807fecb924)(content(Whitespace\"\\n\"))))(Tile((id \
         699b4f1a-1262-444c-b0ed-244a4ed08e12)(label(m))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         aea56abb-1d44-4ac5-b560-2ae247aa9a5c)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         3b24ca94-76a0-4544-bd5b-21893f2a3529)(label(grove))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         28904af6-17d8-4266-abda-578c888daad1)(content(Whitespace\" \
         \"))))(Tile((id \
         ca581d34-30db-4c45-af69-137cf75307e2)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         baa41bce-3391-49b6-8a53-90b31f39309d)(content(Whitespace\" \
         \"))))(Tile((id 912dcff0-f04f-4c74-b8f0-621e6bae4433)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         ea054d15-cb72-4188-aa46-d16218295e6a)(label([ ]))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         b08f66a3-8aa1-4e67-8a53-d949a8562d2e)(label(\"\\\"\\240\\159\\140\\177\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         9ac54bff-84a3-4779-a59d-2ad5a0f96212)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         588f1de3-afe9-4cc7-8be8-f55a4ae13859)(content(Whitespace\" \
         \"))))(Tile((id \
         89e76183-86fd-44b3-8cb8-948c2f5054f3)(label(\"\\\"\\240\\159\\140\\177\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         3b528e7e-5dae-40cb-8d13-e33f76a1c31f)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         e9b7829f-0c8a-4bdf-9dbb-30880a0e3e07)(content(Whitespace\" \
         \"))))(Tile((id \
         2dc614d8-f989-4689-9450-66804d77ee60)(label(\"\\\"\\240\\159\\140\\177\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         6342f7ad-8a29-4aad-a64f-bc6f622bac1a)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         f7707a06-0994-49b0-a226-a4ab566cd86c)(content(Whitespace\" \
         \"))))(Tile((id 59862870-3cc4-46e0-8ffc-a7a0dfc74954)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         1255d3f0-c672-4b64-a957-3047096857c8)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         02293be5-f540-4be0-9d0f-0c4ea6403c1b)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         003b9234-4a82-48f2-bc14-bd661efa8e22)(content(Whitespace\" \
         \"))))(Tile((id \
         06857205-8d0a-433d-a3f4-af92cd0e23fa)(label(\"\\\"\\240\\159\\140\\177\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         25811d4a-e89d-4033-8e01-ff5eadb1fcdb)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         f24e2b1e-85dc-4b09-a716-f677061ddbdd)(content(Whitespace\" \
         \"))))(Tile((id \
         52d57cc3-05a7-4ce8-8702-3d0e5d2035d5)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         3a87d162-b764-4066-af4f-606307ae23f2)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         d632e8f9-cfc0-488d-973e-5ab50f5942e2)(content(Whitespace\" \
         \"))))(Tile((id b8f9612c-6d2a-43dd-8204-c6a797e656a2)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         49da42d2-ffc5-4f00-94fc-f5f13cb3c0eb)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         71855059-7527-4d16-bded-a40a78bb2c18)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         fb0a8bc3-5bbc-4551-9271-9c0ea5a1f978)(content(Whitespace\" \
         \"))))(Tile((id \
         df50d2b9-cb52-4768-af00-0285380f837d)(label(\"\\\"\\240\\159\\140\\177\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         ef3799f7-876b-48ef-9006-d06143441269)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         beb7f27d-181d-44b5-b681-332cac053a21)(content(Whitespace\" \
         \"))))(Tile((id \
         a3468ad5-88b7-4ad7-8121-c0da7c344a3e)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))))))))))))(Secondary((id \
         7a6608d3-499e-4480-a11a-d8ab63ede6b6)(content(Whitespace\"\\n\")))))))))(Tile((id \
         f92158f1-a074-4086-a6e8-62b1a44267b0)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         944fe2f1-c900-4c0f-856a-23b8665a7b12)(content(Whitespace\"\\n\"))))(Secondary((id \
         3f0ee569-5c90-4250-b997-daca670f3f7a)(content(Whitespace\"\\n\"))))(Tile((id \
         1fec7487-8db1-4bca-b1f1-7c1e7b813983)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         b814561f-4752-4b6d-91ef-674a7cc39866)(content(Whitespace\"\\n\"))))(Tile((id \
         417bdad0-a0d4-4b52-af5e-b58c9d884245)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         22daf795-8984-4812-9168-1f2bb4313db3)(content(Whitespace\" \
         \"))))(Tile((id \
         2d537af6-1666-40c9-a9af-6c1506ad1292)(label(m))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         3a2ed81e-2673-44bf-94e3-0c1568799fa2)(content(Whitespace\" \
         \")))))((Secondary((id \
         58b4706e-6b2c-40f2-b49d-e1ab87510cc6)(content(Whitespace\" \
         \"))))(Tile((id \
         8e6c900c-1b84-483d-b5c9-663e455e24c7)(label(do))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         f1b011df-1809-463a-83f0-829bafd3c797)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         a42ac94c-0e7f-4232-89b8-dc375d6ef5f0)(label(init))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         5c6b919e-00a2-4aff-a6ca-4f0deb31ddae)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         82ea0b48-e8df-4553-8b1c-9948c3aa486d)(content(Whitespace\" \
         \"))))(Tile((id dec977eb-ec5b-44a3-87c1-b0fd471e7b0c)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         ef55d689-2fc7-44ba-a6bb-4ebbe3acb33b)(label(SelectSeed))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         56db9d18-35c1-4930-a273-03d624ff32f9)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         7d78eb18-71c0-4d20-8fd5-7763a1f52f8c)(label(2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         e7b9fd03-cccf-46c9-9cf0-c86c0470b047)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         9249984e-3dad-41c2-854c-5812548ee3d3)(content(Whitespace\" \
         \"))))(Tile((id \
         b6c79d6c-d97f-4f19-a8b9-44f0d980e7ea)(label(PlantCol))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         7bf37f19-7ba8-4341-a852-2d572f97b260)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         a6c32cd4-3648-4ac5-b513-0fdde467ba5a)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))))))))))))(Secondary((id \
         349e9e78-7dc6-456c-8819-63772c1c8acb)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         18f616d5-9fe5-47ec-98eb-199d4cfd24bb)(content(Whitespace\"\\n\"))))(Tile((id \
         d5385525-b55e-4aab-a1e2-fba244767612)(label(m))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         ccd10340-1ae1-4d0c-a259-e3c7c39eb725)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         35386044-d707-430a-b5f2-637436acf8e9)(label(grove))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         7524e004-d61f-4b1a-8ede-0d673d6d8500)(content(Whitespace\" \
         \"))))(Tile((id \
         6b03974b-89aa-4e86-8f74-e7232bca2d6d)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         3312820a-9d9e-41d9-ac3f-08e69963a47c)(content(Whitespace\" \
         \"))))(Tile((id fd4190de-b951-481b-a0af-1f657936236e)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         b70bbb29-bf84-4e31-a1da-7e42abf8b103)(label([ ]))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         3e3071f0-15d9-4999-853a-4b42d7532b4f)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         9078cc8e-016d-49a8-9812-0fd9346c3b0b)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         a830d1c6-1e68-43bf-b592-e45979c01d32)(content(Whitespace\" \
         \"))))(Tile((id \
         f4e4c850-f75e-456b-8608-40c15490ee17)(label(\"\\\"\\240\\159\\141\\132\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         c5bfbfd2-c959-482c-9e92-9eabdf6f0a68)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         c16f7016-a15b-461c-a7fb-b5dea1967c58)(content(Whitespace\" \
         \"))))(Tile((id \
         b4347a80-416d-4cec-806c-8e7737175290)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         32f1031a-b572-4bf7-9ab7-442f90493cc3)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         085215b4-c119-470a-9734-3a74e12ac72c)(content(Whitespace\" \
         \"))))(Tile((id 07da0662-5b62-42c6-b0c3-c213a8e6f635)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         5254a685-1a83-4e66-a8b1-6bf00be5acaf)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         17743652-cf62-4b4c-a7a6-f0da7dec7ab4)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         d51b47f5-aa48-4ca6-a77c-a5060ef081fa)(content(Whitespace\" \
         \"))))(Tile((id \
         64a33d92-c094-4a5b-a218-1fe7d288cf40)(label(\"\\\"\\240\\159\\141\\132\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         05d4f1f3-d210-4094-8144-74a9877ee48c)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         71282ec7-a41f-4a24-9c4d-a6eeea76cec9)(content(Whitespace\" \
         \"))))(Tile((id \
         badf87d1-12d6-4feb-a483-b1453c014e53)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         dbb938c9-5497-4349-876c-c2ffa04b0a43)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         ac3014c0-62e3-48cd-89cd-1daed7c0c4ed)(content(Whitespace\" \
         \"))))(Tile((id 3d6f4c4a-6750-40f3-84a5-43fd0a6aa4a3)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         2ebe646d-2f38-40f6-98b4-42d86a2f7dbb)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         3da2719d-67eb-47dd-8094-0351c511d24e)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         60d15c18-da00-4434-bc5e-258e5e61097c)(content(Whitespace\" \
         \"))))(Tile((id \
         6bacf37c-693a-4664-9824-fd322425d6ba)(label(\"\\\"\\240\\159\\141\\132\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         4272819b-f0f6-4130-a7b2-2c956b1f9df8)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         f0d7748a-8f11-4d74-b229-e6a3492fde65)(content(Whitespace\" \
         \"))))(Tile((id \
         897f1c97-64e5-4947-aaaa-518a5a23b865)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))))))))))))(Secondary((id \
         8f7d0427-07a9-41d8-8460-f02252acaab8)(content(Whitespace\"\\n\"))))(Tile((id \
         0c9e4125-56b0-467d-b77a-c7d516589db4)(label(&&))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 32))(sort Exp))((shape(Concave \
         32))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         01a81498-bca5-44d5-ba81-3d4bd5489205)(content(Whitespace\" \
         \"))))(Tile((id \
         3c33fb6d-763d-4767-95e9-f3557a321d44)(label(m))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         e41c83b2-90c8-4aac-9709-c67f6b1d19ab)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         5f60ba22-35f6-4f29-b58b-d722db87702e)(label(currentSeed))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         6900378b-7641-4320-9266-1a36720f785a)(content(Whitespace\" \
         \"))))(Tile((id \
         161f51a4-a87e-4cdc-af65-14d32846323c)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         36969443-2b86-49ed-a587-d9ea3d4183ef)(content(Whitespace\" \
         \"))))(Tile((id \
         b5632bc3-971c-4cdb-a78a-c458b841cbe9)(label(\"\\\"\\240\\159\\141\\132\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         9684c3be-dce6-4192-97b3-fc5c26a4f9f3)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         e6142658-6a04-4789-826d-35431a285fda)(content(Whitespace\"\\n\")))))";
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
