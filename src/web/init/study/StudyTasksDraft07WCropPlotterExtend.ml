let out : string * Haz3lcore.PersistentSegment.t =
  ( "Study / tasks-draft / 07W-crop-plotter-extend",
    {
      segment =
        "((Secondary((id \
         44abfb4b-7e65-44d4-bef5-aad3029a9d95)(content(Comment\"# CROP PLOTTER \
         EXTENSION TASK                     #\"))))(Secondary((id \
         fd688327-bab2-402f-93cd-f4c174cf49af)(content(Whitespace\"\\n\"))))(Secondary((id \
         30c2e24c-4364-4f8a-8c34-e12e2bbd5ced)(content(Comment\"#                                                 \
         #\"))))(Secondary((id \
         8c478af9-fac4-4fe1-9e9c-98d1cd83c9aa)(content(Whitespace\"\\n\"))))(Secondary((id \
         7f4e8ea4-fe86-4254-b02c-f3284c066ce3)(content(Comment\"# The crop \
         plotter app lets you plant seeds on    #\"))))(Secondary((id \
         eabd5692-afef-4219-bd3c-bec6a9433512)(content(Whitespace\"\\n\"))))(Secondary((id \
         a802cdef-17a2-4a7e-adad-026686a5dd70)(content(Comment\"# a grid. It \
         already supports planting rows.      #\"))))(Secondary((id \
         38d783fa-bbab-4545-9ea3-b4ad94a42d23)(content(Whitespace\"\\n\"))))(Secondary((id \
         6f7a4591-ea3d-4fd9-9223-bf13a025d7b8)(content(Comment\"#                                                 \
         #\"))))(Secondary((id \
         4b9fcf3d-953c-4a53-84e2-96b6741ae134)(content(Whitespace\"\\n\"))))(Secondary((id \
         4bf6451c-88d3-4adb-bc9d-00f8f59d67ee)(content(Comment\"# YOUR TASK: \
         Add a PlantCol action that fills     #\"))))(Secondary((id \
         d08d8941-8881-4093-ba2f-f3cb2bf05c91)(content(Whitespace\"\\n\"))))(Secondary((id \
         5038fc13-8cff-4a68-ba8d-bc4b3a8190b9)(content(Comment\"# an entire \
         column with the current seed.         #\"))))(Secondary((id \
         7c1971d3-2cfe-43b9-a32e-64b7024968c7)(content(Whitespace\"\\n\"))))(Secondary((id \
         52d4d1c6-a3f8-42b2-a279-fa3f6feb977b)(content(Comment\"#                                                 \
         #\"))))(Secondary((id \
         0cc306d5-6d6e-4ec9-b06a-509db94e717a)(content(Whitespace\"\\n\"))))(Secondary((id \
         5ff94694-dae6-4539-8e08-978a324749b6)(content(Comment\"# You need \
         to:                                    #\"))))(Secondary((id \
         0f58b116-3f74-4444-832c-98135c49189d)(content(Whitespace\"\\n\"))))(Secondary((id \
         7711bb3c-8297-46b0-8afb-7bb733d1c4af)(content(Comment\"#   1. Add \
         PlantCol(Col) to the Action type       #\"))))(Secondary((id \
         dfd32f91-fa0c-4e5e-81b7-8b0b52d2f254)(content(Whitespace\"\\n\"))))(Secondary((id \
         748904c2-b57a-43a6-b5a4-4334a8364ef3)(content(Comment\"#   2. Add a \
         setCol helper function               #\"))))(Secondary((id \
         e28e9528-278b-4617-9b23-d81429ef3cad)(content(Whitespace\"\\n\"))))(Secondary((id \
         a83558d8-ecb5-41df-bab5-4186c3a378c7)(content(Comment\"#   3. Handle \
         PlantCol in the update function     #\"))))(Secondary((id \
         281e295b-bad9-4e7d-9820-9efe751f3202)(content(Whitespace\"\\n\"))))(Secondary((id \
         66e912b1-5c31-4871-87a5-e244ea14fa5f)(content(Comment\"#                                                 \
         #\"))))(Secondary((id \
         86b6f1ee-651b-4437-ad37-eb54aee33f20)(content(Whitespace\"\\n\"))))(Secondary((id \
         3c5ce0f7-2d6b-47fd-ae34-b3ff319f8e2d)(content(Comment\"# Look at how \
         PlantRow is implemented for         #\"))))(Secondary((id \
         e4dbf073-d853-4d46-89bc-4cf28f11bdec)(content(Whitespace\"\\n\"))))(Secondary((id \
         684dd5c9-5d6c-4187-805e-cb1ef3d56a94)(content(Comment\"# guidance - \
         PlantCol is similar but vertical.    #\"))))(Secondary((id \
         83066129-9100-4dd7-a3e6-8ef386f7f7b0)(content(Whitespace\"\\n\"))))(Secondary((id \
         6b6701fd-409d-499b-bef1-500d63813b86)(content(Comment\"#                                                 \
         #\"))))(Secondary((id \
         28b6ba46-7e12-4833-975a-66d909049201)(content(Whitespace\"\\n\"))))(Secondary((id \
         312a991b-2099-4ff6-9341-b62ecf3d5775)(content(Comment\"# Tip: Use \
         auto-probe to see how the grove        #\"))))(Secondary((id \
         85e2465d-d09a-402d-9bdf-2414c077734f)(content(Whitespace\"\\n\"))))(Secondary((id \
         eaad14bb-d9aa-4b20-8805-fca97024a4f5)(content(Comment\"# changes \
         after each action.                      #\"))))(Secondary((id \
         ed649855-bb41-4a64-86d0-d102f4fd0bf2)(content(Whitespace\"\\n\"))))(Secondary((id \
         733d6011-49de-4587-a0de-637c03d2b65f)(content(Whitespace\"\\n\"))))(Tile((id \
         cf8b297b-a02b-448b-aced-ee06709e6aab)(label(type = in))(mold((out \
         Exp)(in_(TPat Typ))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         4aa40962-ac52-4b16-85f3-b7ca649e5b20)(content(Whitespace\" \
         \"))))(Tile((id \
         8c840b43-46ba-4f06-9fc8-85d5e782af5c)(label(Plant))(mold((out \
         TPat)(in_())(nibs(((shape Convex)(sort TPat))((shape Convex)(sort \
         TPat))))))(shards(0))(children())))(Secondary((id \
         076b240b-488a-4506-a9d3-799dd9de14d6)(content(Whitespace\" \
         \")))))((Secondary((id \
         8be55cc0-6adc-47fb-b25e-5a1565c5f895)(content(Whitespace\" \
         \"))))(Tile((id \
         389395b7-0dc8-49bb-ab94-270ae2e0f872)(label(String))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         94a0b2d8-d7a3-4133-a2ba-e331b4eb4cec)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         92bb385b-f3e6-45b9-bf99-210e0609c575)(content(Whitespace\"\\n\"))))(Tile((id \
         f2f0b664-8637-4fcc-969e-678a32ab521d)(label(type = in))(mold((out \
         Exp)(in_(TPat Typ))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         34ad41e5-4ccf-40e0-924a-a16f446c08c7)(content(Whitespace\" \
         \"))))(Tile((id \
         2d45a09c-9b6f-4acb-9fa3-af3db21cca08)(label(Grove))(mold((out \
         TPat)(in_())(nibs(((shape Convex)(sort TPat))((shape Convex)(sort \
         TPat))))))(shards(0))(children())))(Secondary((id \
         6e8c2432-49bd-4d3e-a8c0-a8c9b271f3ca)(content(Whitespace\" \
         \")))))((Secondary((id \
         c6222460-9ece-4623-9e53-fee66db2fdc0)(content(Whitespace\" \
         \"))))(Tile((id 076684f7-8fe3-4a5c-b7ff-f4f1cb2d1168)(label([ \
         ]))(mold((out Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape \
         Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
         eb0d690f-6fbf-424d-9f1c-d6a95f72c1bd)(label([ ]))(mold((out \
         Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0 1))(children(((Tile((id \
         67985f43-64b7-400e-8473-758fbe7a3090)(label(Plant))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))))))))))))(Secondary((id \
         40e25c48-021c-4d81-b0e1-54c956640828)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         a7b667b5-eed1-4ff9-81fd-d338f021dd55)(content(Whitespace\"\\n\"))))(Tile((id \
         0359c122-0772-4cad-959a-34e21018c439)(label(type = in))(mold((out \
         Exp)(in_(TPat Typ))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         0a8594d8-2442-4b88-a5cc-b6c753cfd26b)(content(Whitespace\" \
         \"))))(Tile((id \
         871d01fb-0f5c-4fed-9b02-c3b834fef84b)(label(Row))(mold((out \
         TPat)(in_())(nibs(((shape Convex)(sort TPat))((shape Convex)(sort \
         TPat))))))(shards(0))(children())))(Secondary((id \
         92215d1a-197b-4b60-bee0-0cfcd2596595)(content(Whitespace\" \
         \")))))((Secondary((id \
         03dbcff0-ad05-45f3-9032-1764924b7ca4)(content(Whitespace\" \
         \"))))(Tile((id \
         bcac37a5-664d-42d5-a223-7a81d9c8d2f7)(label(Int))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         1c634981-9588-46bd-81d9-cab5afb51979)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         11560bfd-76c9-4467-a74f-a8e71d59b8a1)(content(Whitespace\"\\n\"))))(Tile((id \
         21939e00-9f5e-4bf2-9366-5165e09e5c98)(label(type = in))(mold((out \
         Exp)(in_(TPat Typ))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         7eb7780d-03bc-4afa-9a19-f7a91f767b30)(content(Whitespace\" \
         \"))))(Tile((id \
         3c776d15-aebf-4a07-97d1-063bf3b21053)(label(Col))(mold((out \
         TPat)(in_())(nibs(((shape Convex)(sort TPat))((shape Convex)(sort \
         TPat))))))(shards(0))(children())))(Secondary((id \
         54e144f1-1e64-47d9-896d-faf44f8829e4)(content(Whitespace\" \
         \")))))((Secondary((id \
         6ae80698-35e8-44e6-9475-887ca638601e)(content(Whitespace\" \
         \"))))(Tile((id \
         79f814c7-3531-44a6-a437-3ca6a4512783)(label(Int))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         d23f514e-0f37-474f-b05c-4af18807c1d0)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         6198b75c-019b-4356-85dd-6af4054ab540)(content(Whitespace\"\\n\"))))(Secondary((id \
         370fda79-208f-4618-b4d3-a945f0dc5415)(content(Whitespace\"\\n\"))))(Tile((id \
         5f5ed455-98ca-49db-873d-899150324153)(label(type = in))(mold((out \
         Exp)(in_(TPat Typ))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         b2f6f5bd-f1cb-42c4-932e-6442db261f51)(content(Whitespace\" \
         \"))))(Tile((id \
         c890c25c-6eda-43d2-ae69-5d4abfa2569c)(label(Model))(mold((out \
         TPat)(in_())(nibs(((shape Convex)(sort TPat))((shape Convex)(sort \
         TPat))))))(shards(0))(children())))(Secondary((id \
         1226eb7a-4454-4693-a16d-6a750141306b)(content(Whitespace\" \
         \")))))((Secondary((id \
         ca5069f1-741f-45a9-a506-e3779d01dcc6)(content(Whitespace\" \
         \"))))(Tile((id \
         f3eee731-545d-416d-9ff9-a978b3ae4657)(label(\"(\"\")\"))(mold((out \
         Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0 1))(children(((Secondary((id \
         6afd0646-7166-4338-ba4b-ba558d97eda0)(content(Whitespace\"\\n\"))))(Tile((id \
         69d4d545-e2a0-470c-8a21-c7244983c476)(label(grove))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         5dd91f54-8d70-4eb5-b0ed-6cedafc95139)(content(Whitespace\" \
         \"))))(Tile((id \
         627a6031-3c32-4e33-b0b9-4538047cbee9)(label(=))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 39))(sort Typ))((shape(Concave \
         39))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         8a71c81c-0027-4375-bb27-82e55c3bc77e)(content(Whitespace\" \
         \"))))(Tile((id \
         998279a9-a7e9-4dd4-8a72-f19887aee957)(label(Grove))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         d1cf9ed9-9541-4875-8c5a-ec11fdf7c330)(label(,))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 44))(sort Typ))((shape(Concave \
         44))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         f5214120-3db8-4096-8cad-53873b94cfe3)(content(Whitespace\"\\n\"))))(Tile((id \
         0d0d2da3-6fe7-4417-8de6-5d1e73a01dff)(label(currentSeed))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         044a4fc9-6eda-4db7-92df-1e500e180909)(content(Whitespace\" \
         \"))))(Tile((id \
         e9ff7747-6d4b-4af5-a723-88e4ff5af063)(label(=))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 39))(sort Typ))((shape(Concave \
         39))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         2a7e8357-e6ca-4f13-96c5-dcc4c49c8a42)(content(Whitespace\" \
         \"))))(Tile((id \
         ebd03604-fbcf-420d-9540-0715c3049a74)(label(Plant))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         0f2f85b4-03bf-45fa-b25d-a3d4d1f5bdb7)(label(,))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 44))(sort Typ))((shape(Concave \
         44))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         e3e942d5-0d93-4f8d-8445-3eaf86fe2587)(content(Whitespace\"\\n\"))))(Tile((id \
         97098f73-06e2-4514-afb2-aa29a493ef01)(label(seedInventory))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         acb4675d-a608-44f3-bd0e-d8f4d785d91c)(content(Whitespace\" \
         \"))))(Tile((id \
         cb7fb2ac-b838-43b7-a013-f2ced30b8830)(label(=))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 39))(sort Typ))((shape(Concave \
         39))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         bad0bb73-19b8-4d57-9f83-b959db490d78)(content(Whitespace\" \
         \"))))(Tile((id e58cc757-cdb7-4b3c-9f85-3af7f665fab1)(label([ \
         ]))(mold((out Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape \
         Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
         530b77fa-a4e3-43a7-a9db-f1c565a6a6f9)(label(Plant))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children()))))))))(Secondary((id \
         01038518-7224-48f7-8511-0b09064c4f2b)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         01a5449d-8851-4d2b-9c6b-58d5f4b07f93)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         7bbe805f-058c-4c61-a6b2-cdfba631f84d)(content(Whitespace\"\\n\"))))(Secondary((id \
         0d8e92c2-781f-47d3-affa-4fd5e248a4df)(content(Whitespace\"\\n\"))))(Tile((id \
         0f2ca5ff-099c-4235-ad4e-e4bad0f5077c)(label(type = in))(mold((out \
         Exp)(in_(TPat Typ))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         f8a7e9d3-49ee-4169-af1c-4d8d9792a7a7)(content(Whitespace\" \
         \"))))(Tile((id \
         7b257ba7-32fa-4353-bf80-a2f8be1fd45e)(label(Action))(mold((out \
         TPat)(in_())(nibs(((shape Convex)(sort TPat))((shape Convex)(sort \
         TPat))))))(shards(0))(children())))(Secondary((id \
         c230ef5e-07a8-44ad-aab0-eee1e5783729)(content(Whitespace\" \
         \")))))((Secondary((id \
         15c11166-7ccc-480f-9132-5bd10f491553)(content(Whitespace\"\\n\"))))(Tile((id \
         4532558f-ae84-423e-9ab3-0a56940ebdd9)(label(+))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape(Concave 33))(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         fb16b8b0-84b9-4a11-89b0-5ddc4d403aab)(content(Whitespace\" \
         \"))))(Tile((id \
         a67ba0b6-2f7c-404e-88d2-34ae8516df75)(label(SelectSeed))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         a8da5c96-ccee-4654-9730-6b57fe83f67a)(label(\"(\"\")\"))(mold((out \
         Typ)(in_(Typ))(nibs(((shape(Concave 11))(sort Typ))((shape \
         Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
         58b0d396-6f54-4bbb-984a-583e9315a85b)(label(Int))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children()))))))))(Secondary((id \
         dd37c862-c49a-4070-9fba-448c4b758ead)(content(Whitespace\"\\n\"))))(Tile((id \
         e8d68675-548f-4ec3-aba2-05de44f782f5)(label(+))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 12))(sort Typ))((shape(Concave \
         12))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         7e09f8c9-4f16-4e2e-8bb0-c5d1fc690718)(content(Whitespace\" \
         \"))))(Tile((id \
         71020675-4878-4170-a56f-0c8fca14e16b)(label(PlantSeed))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         c2d89953-8db4-4a02-9068-dfd88317a070)(label(\"(\"\")\"))(mold((out \
         Typ)(in_(Typ))(nibs(((shape(Concave 11))(sort Typ))((shape \
         Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
         79483685-3c7f-4d8c-98e6-600137cf5805)(label(Row))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         844478e4-d424-4aa0-a7a9-1aef3806a8f2)(label(,))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 44))(sort Typ))((shape(Concave \
         44))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         2d10e0f6-c516-45b3-85ba-78f592bbaf74)(content(Whitespace\" \
         \"))))(Tile((id \
         44ea6bdf-aaca-487b-a876-49231b93edac)(label(Col))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children()))))))))(Secondary((id \
         ed626bd3-c853-4352-b2a4-1681fbf1aa7b)(content(Whitespace\"\\n\"))))(Tile((id \
         a4d9b429-f7ce-4260-afd6-3ffe615e29a3)(label(+))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 12))(sort Typ))((shape(Concave \
         12))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         822ae224-048c-4028-8f1f-b677ce61ee7a)(content(Whitespace\" \
         \"))))(Tile((id \
         6e4970c2-c4d7-4850-8c8e-8d227fe49ed8)(label(Uproot))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         57dfabcb-d088-4ddb-86d4-aa3b405f050a)(label(\"(\"\")\"))(mold((out \
         Typ)(in_(Typ))(nibs(((shape(Concave 11))(sort Typ))((shape \
         Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
         eea89dc4-7946-44b9-889d-78f5ee717ebb)(label(Row))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         0db50f1a-8f8e-4ed5-97cf-0580d0c85192)(label(,))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 44))(sort Typ))((shape(Concave \
         44))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         6e108951-e339-4d9a-9725-6bd0a6144816)(content(Whitespace\" \
         \"))))(Tile((id \
         bd0e3bd8-645b-41fc-b748-bdad1efab32f)(label(Col))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children()))))))))(Secondary((id \
         c671a3cb-c917-4e5c-bda7-ad00ec720a11)(content(Whitespace\"\\n\"))))(Tile((id \
         435fdade-6d3d-47c3-9d17-6dd35bd57fe6)(label(+))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 12))(sort Typ))((shape(Concave \
         12))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         e0c1e652-8b79-4ebd-9ad1-94f918d19493)(content(Whitespace\" \
         \"))))(Tile((id \
         46b0bc7d-2480-4e5f-8d4f-ff326269c443)(label(ClearGrove))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         b592c982-a0e1-4422-931d-e82ace66b93b)(content(Whitespace\"\\n\"))))(Tile((id \
         172c9ea6-33d8-46e0-81c7-f9cbd7ab2078)(label(+))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 12))(sort Typ))((shape(Concave \
         12))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         b558cdae-254d-47dc-9ef7-033cc5fc55e2)(content(Whitespace\" \
         \"))))(Tile((id \
         8cda76f9-5825-4518-aa43-e311a0a007e4)(label(PlantRow))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         c9dd8d24-8b93-4904-a7ee-c158b1c77c76)(label(\"(\"\")\"))(mold((out \
         Typ)(in_(Typ))(nibs(((shape(Concave 11))(sort Typ))((shape \
         Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
         c61def9b-d6ee-4d9c-bc22-9d45cd86862e)(label(Row))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children()))))))))(Secondary((id \
         4d1f9ece-1555-4236-a41b-94f2f8a137a2)(content(Whitespace\"\\n\"))))(Secondary((id \
         ff9011b6-5df9-4121-9f9a-3ba853a2ef3a)(content(Comment\"# TODO: Add \
         PlantCol(Col) here #\"))))(Secondary((id \
         516146c4-54c0-4a60-b6fb-d2b8d8e426d5)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         55fd852b-0f82-4155-a635-2c6690fd1070)(content(Whitespace\"\\n\"))))(Secondary((id \
         a03a17d1-a55a-4a75-b0af-ee7b1d6de99b)(content(Whitespace\"\\n\"))))(Tile((id \
         c5261f76-4101-43fb-af4f-63302b9435b7)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         f483e309-3686-42f8-b7da-2f4c15766543)(content(Whitespace\" \
         \"))))(Tile((id \
         6885d499-8f16-461b-874e-aada872daa71)(label(init))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         a01bd1f8-e8c4-4307-b7ac-6b0d3528606f)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 24))(sort Pat))((shape(Concave \
         24))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         bb62b121-9a23-4b92-bdab-a4434ed4c0d0)(content(Whitespace\" \
         \"))))(Tile((id \
         25fb11d7-c446-4a61-bf47-fb49a07d9c60)(label(Model))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         2a683a99-02df-4de1-b656-4277746ad4ee)(content(Whitespace\" \
         \")))))((Secondary((id \
         338f084f-b5a0-4e62-8699-094bc1627dfe)(content(Whitespace\" \
         \"))))(Tile((id \
         89ad4f60-f23b-4a44-bd70-c1bd45da68b4)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         b204411c-124a-48de-be12-ac4c3cb7bdbd)(content(Whitespace\"\\n\"))))(Tile((id \
         a28150d8-71ae-4863-a5eb-7e55553be94a)(label(grove))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         426c8ce5-0ea1-4cb3-96d1-a49e12f3057d)(content(Whitespace\" \
         \"))))(Tile((id \
         f94f87f3-6f04-4e9a-a08c-22f192f8acae)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         597fed5e-7114-4bc5-a895-6e6d58dc8a83)(content(Whitespace\" \
         \"))))(Tile((id f5781bd2-f1b4-4269-a377-cf5543dd3573)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         4a65cb1c-5b48-4569-9aa4-0c3c8b722937)(content(Whitespace\"\\n\"))))(Tile((id \
         b0a3793d-109f-441e-8584-cc5db7c87731)(label([ ]))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         e233789d-8c24-4e84-a8ee-320ce1440270)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         8bd02898-cf3f-4b3d-9c12-6d3847b0305e)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         df599fb1-c789-4961-aacf-0c120c6f3a7b)(content(Whitespace\" \
         \"))))(Tile((id \
         7b569eb2-931a-4918-bee1-297789071099)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         1cab328b-09f3-482e-94ad-7d26ab84e280)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         67ad824d-e8db-47b2-b466-487768362a31)(content(Whitespace\" \
         \"))))(Tile((id \
         912c82df-b9ea-4681-80d1-f337cb16c594)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         bbfe38e6-42d0-432e-af50-02bb0b05081c)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         58d6f3d5-1412-491f-9407-d72998dc66fc)(content(Whitespace\"\\n\"))))(Tile((id \
         b6992bbf-b4c6-4386-b0d3-e5e6e5e436a1)(label([ ]))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         024cc7aa-0826-44cb-8c0a-05a65d9ef419)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         9514bd4d-c811-49dd-9efe-23e3750da129)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         6c91eafc-78fc-4505-88e8-da68ebff4b65)(content(Whitespace\" \
         \"))))(Tile((id \
         833f44e2-130d-4c23-98a1-5343f2644e10)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         fe1679c0-86ba-4dde-b1bf-4559740a0d3f)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         8f4d88c5-815d-454b-b1a5-b7b4cbed3d0f)(content(Whitespace\" \
         \"))))(Tile((id \
         4155eba4-9a92-4f32-96c6-dfaa69bc66f7)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         df95f893-73c4-44d2-8d75-664dc4411d9e)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         54350b5c-74d8-4267-95a6-cd88bb97438b)(content(Whitespace\"\\n\"))))(Tile((id \
         b466b896-f4b2-482e-b0bc-663cd674ac9b)(label([ ]))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         a5dc4049-9e96-4e50-82c0-ae4bffca17f2)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         150d60d5-6caa-4d97-b11d-ca84fbf1158b)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         c48ee415-6d97-4e69-9d70-aaea849fe549)(content(Whitespace\" \
         \"))))(Tile((id \
         95be2f6d-a1c4-4bce-9696-f59a95d43869)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         852a3d59-feb7-43d3-8d23-d9ad67ac1bdc)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         899c4910-9f47-42a2-8c7f-3ce537c995f1)(content(Whitespace\" \
         \"))))(Tile((id \
         150f7798-01bc-4dac-8517-2738c91e1017)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         742bfdb9-2fd7-43dd-8d91-407d515f015d)(content(Whitespace\"\\n\")))))))))(Tile((id \
         5e5a8ead-de90-49ad-a701-33597c3c2955)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         ef80533b-207e-4d17-946a-fef2b35577ef)(content(Whitespace\"\\n\"))))(Tile((id \
         e8c5a940-719e-4a4c-8917-8967cc9c6e1e)(label(currentSeed))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         4eb3f9ed-3ba6-487a-8c15-744a41daa2cb)(content(Whitespace\" \
         \"))))(Tile((id \
         2ddec39c-2264-4d63-aced-d478df82e56c)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         3b305c3a-77d7-4ee1-b640-b6506aa8b3d3)(content(Whitespace\" \
         \"))))(Tile((id \
         dbe8d5d0-0665-4f62-aef7-ef8cf88d8551)(label(\"\\\"\\240\\159\\140\\177\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         a855d59d-ba7d-4dac-a1b0-8e9ce38282f9)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         932e3dbf-1b70-4371-b4a3-1af6de5a577a)(content(Whitespace\"\\n\"))))(Tile((id \
         c95dc9e8-2d8b-4b1a-b29a-30cbda1a8922)(label(seedInventory))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         c2f8bd4b-0cef-4525-8a87-2dd8d568d8bb)(content(Whitespace\" \
         \"))))(Tile((id \
         2b8c2467-f952-47cc-a503-30df46ea3421)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         3d67ef30-359a-4cb8-8cd7-f2cea9ef8b38)(content(Whitespace\" \
         \"))))(Tile((id 91110ea4-cbbe-4bab-816c-4297df5dfee8)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         0ee870b5-3631-4c9a-8f19-1ceb6249bf2f)(label(\"\\\"\\240\\159\\140\\177\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         e4260ae4-4b47-4792-8da2-dbe61dbaff46)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         26c8e64e-c27f-45ee-bb1d-e5628c688994)(content(Whitespace\" \
         \"))))(Tile((id \
         755bcb88-393f-499c-bdcc-652ca349fb06)(label(\"\\\"\\240\\159\\140\\191\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         cc991a8e-0ba7-49a7-a145-4613407196e7)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         208d0cce-9447-409e-8d02-116cd4698e30)(content(Whitespace\" \
         \"))))(Tile((id \
         c61c6f10-a1bf-4b60-b421-6e038dc6d9ec)(label(\"\\\"\\240\\159\\141\\132\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         bdfb946d-64ea-475f-98ae-fa9f2c7cfdbe)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         de9400e1-9dbb-4078-9724-9fa3500633f3)(content(Whitespace\" \
         \"))))(Tile((id \
         cb83b59f-da1b-42d8-9de4-65f2c7c5ea42)(label(\"\\\"\\226\\152\\152\\239\\184\\143\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         b8e454cb-1930-4b63-b81a-b28715c93021)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         306c0561-2aa9-4e2f-a5a3-e0188f768355)(content(Whitespace\" \
         \"))))(Tile((id \
         7395f482-f0fa-463b-9b5e-fa47d593b914)(label(\"\\\"\\240\\159\\140\\184\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         a3d4f974-8e88-46fe-aa81-c1835de5d450)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         fae39a83-1cc2-4e84-a2eb-2134bd99c352)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         203415b2-ec10-4395-a97e-2b796ac7837d)(content(Whitespace\"\\n\"))))(Secondary((id \
         57ba0ce3-be6c-44fd-9936-6c08429616a0)(content(Whitespace\"\\n\"))))(Tile((id \
         ce558a2c-70aa-4ea1-a245-4ae4a94947ed)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         55b448d8-26cf-4d17-b1c6-e6d8a909dfcb)(content(Whitespace\" \
         \"))))(Tile((id \
         ec55f230-08c8-4aa0-8fc1-dd339016b867)(label(setCell))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         9c227790-2165-465e-aef7-7c0760031f18)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 24))(sort Pat))((shape(Concave \
         24))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         5a917451-5884-495e-8d60-6d0ad8af2670)(content(Whitespace\" \
         \"))))(Tile((id \
         c0ea62a3-4d88-448b-9f54-8b7ba777f408)(label(\"(\"\")\"))(mold((out \
         Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0 1))(children(((Tile((id \
         51730e4b-d0b7-4825-8bb8-5e7b2e266929)(label(Grove))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         6bdbdb4b-2b83-44d3-b045-70d022c4b207)(label(,))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 44))(sort Typ))((shape(Concave \
         44))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         ebf2a111-c792-4f3e-8737-4e881f6adb93)(content(Whitespace\" \
         \"))))(Tile((id \
         dd3da24a-5887-4786-a52f-7970a4aa0bdf)(label(Row))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         ce649634-e7fc-4c5d-8525-920771b40ba5)(label(,))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 44))(sort Typ))((shape(Concave \
         44))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         9b5029e0-f98b-4fdc-bd11-5dd666425b00)(content(Whitespace\" \
         \"))))(Tile((id \
         dd3871d4-ae9b-47d5-81f7-e1657aeb4442)(label(Col))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         db71f55a-a766-42a8-ac95-6cea91f5aa6e)(label(,))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 44))(sort Typ))((shape(Concave \
         44))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         5260a60b-7533-4802-98f9-1d46cfe3b308)(content(Whitespace\" \
         \"))))(Tile((id \
         1f9fe714-3bf7-40f2-9655-ef3f269d772b)(label(Plant))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children()))))))))(Secondary((id \
         746e1c19-e6f3-4725-9274-ea81ee093aaf)(content(Whitespace\" \
         \"))))(Tile((id \
         1a0c2dcd-05b7-40b4-ac50-5af8f32a087e)(label(->))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 13))(sort Typ))((shape(Concave \
         13))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         2fb83c40-a787-4fa8-9495-845eef16fd70)(content(Whitespace\" \
         \"))))(Tile((id \
         a472cddf-2676-4e85-88e1-b220d9d15e47)(label(Grove))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         718e551a-4e1e-4aa8-b757-a79f093d6faf)(content(Whitespace\" \
         \")))))((Secondary((id \
         d3b043fd-e6eb-4557-b073-93bbb434d564)(content(Whitespace\"\\n\"))))(Tile((id \
         59096093-be08-4f7b-8536-31b60e9cb4f1)(label(fun ->))(mold((out \
         Exp)(in_(Pat))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         37))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         957fc579-13e2-4cd9-916a-b3f10411b027)(content(Whitespace\" \
         \"))))(Tile((id \
         9a37e87f-03e3-4539-a910-ad3170d5b3ec)(label(grove))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         c79e3623-125f-43f0-af40-4ccc9f319410)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 44))(sort Pat))((shape(Concave \
         44))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         6b9b32fc-5380-43c1-b9a1-ab76a5753447)(content(Whitespace\" \
         \"))))(Tile((id \
         cf482fd4-28b7-4bb9-b577-432ae12ad392)(label(row))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         75b78a07-a200-43d8-a54a-32a052e21005)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 44))(sort Pat))((shape(Concave \
         44))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         6ecca24a-e619-45dc-9dcc-ca14a95f8503)(content(Whitespace\" \
         \"))))(Tile((id \
         37806205-0cb7-4e6a-a6f4-cd3ef63ef562)(label(col))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         1e19ac93-c367-44e6-8cad-5c5b8ace8253)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 44))(sort Pat))((shape(Concave \
         44))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         5f5aeaee-1712-488e-9fa4-7b163fe08e87)(content(Whitespace\" \
         \"))))(Tile((id \
         f29d4367-be13-482a-a3fb-f7cd7bf6698e)(label(plant))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         d614a657-540f-4ccf-a3c7-dfd130ae1574)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         43ce5c5e-2bea-4a23-ae46-f1d0af0263ad)(content(Whitespace\"\\n\"))))(Tile((id \
         babe9b4a-a262-4b05-a3cf-2a7dc5fdf494)(label(mapi))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         94b59f8f-b4fa-4c01-9c97-c8f1b7b88058)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         88b4496b-2537-4f19-9e9e-0dedb421cde9)(label(grove))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         cfe7dd41-e6b3-48b8-94cd-65fb7f176bea)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         2fa808b0-d0e4-4fa3-a961-6c11c58ddd83)(content(Whitespace\" \
         \"))))(Tile((id d7c6e232-2264-4921-9567-34e042d15652)(label(fun \
         ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 37))(sort Exp))))))(shards(0 \
         1))(children(((Secondary((id \
         b98e231b-f7e2-41d8-a155-d07721c67635)(content(Whitespace\" \
         \"))))(Tile((id \
         ebfe2cc0-16d0-4283-af1e-015feac1a238)(label(\"(\"\")\"))(mold((out \
         Pat)(in_(Pat))(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0 1))(children(((Tile((id \
         4807c917-edf5-45c2-aac0-4b7d775aa463)(label(i))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         f1646baf-0257-4aa4-af05-ca577a3d2d9d)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 44))(sort Pat))((shape(Concave \
         44))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         884c8a77-d2bb-4406-b029-ecb8f8f55036)(content(Whitespace\" \
         \"))))(Tile((id \
         c95e2fbd-13b2-4c68-8f2c-e12086a4cf90)(label(r))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children()))))))))(Secondary((id \
         ddcd06a6-3a25-4ee9-9c94-5ea10ce2a7c6)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         8da72adc-8a45-425f-94c7-ca56afb7aa90)(content(Whitespace\"\\n\"))))(Tile((id \
         b1c3cc4e-2d8f-4233-b5d1-8422923c1803)(label(if then else))(mold((out \
         Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         36))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         1b255e59-1eca-4adc-95fc-001c1f1849cb)(content(Whitespace\" \
         \"))))(Tile((id \
         159afc6e-d714-4880-ae9c-c8f2827f96f7)(label(i))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         bdf34b1b-4d96-486d-885a-5b815d61c4e0)(content(Whitespace\" \
         \"))))(Tile((id \
         5ed17650-d144-42b9-b9c0-de82bfc5c074)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         f2c5dfa9-ec2e-4b2f-a258-b120fffe9950)(content(Whitespace\" \
         \"))))(Tile((id \
         8d8a292c-de5b-4bcf-b399-5c091ad889fb)(label(row))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         279af1f1-47a5-4979-803a-522f8204bd6d)(content(Whitespace\"\\n\")))))((Secondary((id \
         c5905b34-8e9e-47e7-b783-5c458e953c43)(content(Whitespace\" \
         \"))))(Tile((id \
         fd9444f7-b22a-4778-8de9-a737dc81813f)(label(mapi))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         1a2df0ab-bcb3-4e8c-a7c6-7cd218452a57)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         b1b0d621-cbb0-47b1-aa57-c02670d0029e)(label(r))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         4ef606fd-6365-4b47-80aa-28ea7580870e)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         f02ef798-6c2d-4d28-8992-6f3558cad201)(content(Whitespace\" \
         \"))))(Tile((id 70f69b08-60ad-4e62-8668-826675b2f1a7)(label(fun \
         ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 37))(sort Exp))))))(shards(0 \
         1))(children(((Secondary((id \
         6a4e882d-8bc8-4bf9-9f2e-7dcdfe40527e)(content(Whitespace\" \
         \"))))(Tile((id \
         186d3303-9f8d-4974-9122-385a372893d3)(label(\"(\"\")\"))(mold((out \
         Pat)(in_(Pat))(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0 1))(children(((Tile((id \
         4a47039f-0f62-468c-8fc2-dfbd94410a56)(label(j))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         e3d76e91-e991-4621-9076-058556c1250c)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 44))(sort Pat))((shape(Concave \
         44))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         9e71277a-a22d-47d1-849c-22e03b0e6633)(content(Whitespace\" \
         \"))))(Tile((id \
         ebc006af-0ab8-4651-8a19-bba5ec7de64f)(label(c))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children()))))))))(Secondary((id \
         ed350ad2-b176-4165-9790-af65a145142f)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         db9300ff-ce48-4bcf-b8d7-630891bdedc3)(content(Whitespace\" \
         \"))))(Tile((id 4d552b6d-f394-4a3e-a20e-0cb5ea951917)(label(if then \
         else))(mold((out Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 36))(sort Exp))))))(shards(0 1 \
         2))(children(((Secondary((id \
         f19db1ef-72e9-462c-ae9e-630f1d211817)(content(Whitespace\" \
         \"))))(Tile((id \
         764d895b-5192-4d95-8f0f-c6787a7e95c3)(label(j))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         3bc271a1-eeb9-4489-a2bd-5373a920c4bd)(content(Whitespace\" \
         \"))))(Tile((id \
         8a17a454-d55d-4f1e-b78a-a0c05f98cdc7)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         c74f9ff3-e3c0-4069-9131-9f05e8b581a3)(content(Whitespace\" \
         \"))))(Tile((id \
         f2449c4e-fae5-40a5-9bdf-70dde718f1ed)(label(col))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         436cc165-1095-4bf7-9ce5-b90a753c7a7d)(content(Whitespace\" \
         \")))))((Secondary((id \
         90d856ff-7a99-4c79-a9aa-dea8ddfbc7d0)(content(Whitespace\" \
         \"))))(Tile((id \
         46c1f8bb-ff62-4341-883c-78694baf0b5d)(label(plant))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         45b71144-be0a-4287-abc9-8c8a7770a512)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         d276eb01-27db-4dcc-ba70-d74f7c311a10)(content(Whitespace\" \
         \"))))(Tile((id \
         466856af-3816-4bf6-92b2-61f574d6719c)(label(c))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         56e1833d-9d40-4111-84f6-acbf3ccf97fe)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         e5f0da42-1d93-4aa9-9cff-226f7a8599b2)(content(Whitespace\" \
         \"))))(Tile((id \
         79183e75-eab0-4c5e-a880-bfa30973f21b)(label(r))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         513049b9-0815-4d93-a0c0-445f897534ca)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         5a14d697-5cdd-43e3-b3b1-e4b225bf7650)(content(Whitespace\"\\n\"))))(Secondary((id \
         586eceff-f191-40de-861e-1452372ac1f3)(content(Whitespace\"\\n\"))))(Tile((id \
         92b54b9c-3744-4818-8966-06654ab3d063)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         06af1cc1-e6c5-419b-afdc-5ba20e9e8016)(content(Whitespace\" \
         \"))))(Tile((id \
         94548e53-500c-4bb5-a6b2-a3e937d1dc9b)(label(setRow))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         ca998a34-aa3b-40aa-b585-e7941783621a)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 24))(sort Pat))((shape(Concave \
         24))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         113b57f8-6239-458b-84db-acf189a0d9d2)(content(Whitespace\" \
         \"))))(Tile((id \
         518a06c1-8d95-4e2c-8487-f689967705e6)(label(\"(\"\")\"))(mold((out \
         Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0 1))(children(((Tile((id \
         3edebfd1-8c52-42db-b792-fc9fd4b006ef)(label(Grove))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         6d7aebee-91b2-465c-9fe7-49ad3da3792c)(label(,))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 44))(sort Typ))((shape(Concave \
         44))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         016c58cc-36eb-4239-a1c1-c4e9782436f6)(content(Whitespace\" \
         \"))))(Tile((id \
         b62eb313-4e22-41f7-9ee0-f2244b759799)(label(Row))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         b1d20b42-8da1-41fb-89f5-114cd71968e6)(label(,))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 44))(sort Typ))((shape(Concave \
         44))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         f7f0c4b3-8346-4703-9b2b-8370aac26516)(content(Whitespace\" \
         \"))))(Tile((id \
         39ea66ec-379f-430e-b8f2-1613d93f7927)(label(Plant))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children()))))))))(Secondary((id \
         eda85bd1-bfcb-45ad-ad21-45c86776b7e4)(content(Whitespace\" \
         \"))))(Tile((id \
         59b5aaf7-488f-42db-8e49-d09456070ec7)(label(->))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 13))(sort Typ))((shape(Concave \
         13))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         2a4ec633-d86d-4392-b536-c8a437dbb682)(content(Whitespace\" \
         \"))))(Tile((id \
         47357f4b-f757-42c6-9029-13d4bb16714f)(label(Grove))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         0d6a9aac-1c8a-4801-92ea-37a62e1cb2de)(content(Whitespace\" \
         \")))))((Secondary((id \
         2805287e-97f5-46d0-8b01-dbca0082ed0d)(content(Whitespace\"\\n\"))))(Tile((id \
         3d710dd9-d7dc-426b-8107-05089ea129b9)(label(fun ->))(mold((out \
         Exp)(in_(Pat))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         37))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         d612daf1-f169-4e81-a511-84f0ec741ce3)(content(Whitespace\" \
         \"))))(Tile((id \
         abe96b86-3baf-482e-a4d5-0fe42fc5cd29)(label(grove))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         f14907ca-79e9-42e0-a074-9681878267b2)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 44))(sort Pat))((shape(Concave \
         44))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         d6390e6b-1f07-4773-a4de-6be43d6b46df)(content(Whitespace\" \
         \"))))(Tile((id \
         c27bfa59-9066-429c-8e9d-427a368df13c)(label(targetRow))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         9d9b022a-935e-4c09-9dc1-d4d699a95f36)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 44))(sort Pat))((shape(Concave \
         44))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         a07a36ec-da79-400a-97d5-295ca24e7047)(content(Whitespace\" \
         \"))))(Tile((id \
         83a83ccb-9fb3-4818-88cd-8ed8b5fd369a)(label(plant))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         2bba434c-49b1-47bb-aaf9-1f598cf3bd2f)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         03dc954e-6578-4fa3-9314-d38cd2eaa59d)(content(Whitespace\"\\n\"))))(Tile((id \
         79217a1e-ba8c-40a8-b69b-20141c14e753)(label(mapi))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         b9b48dd2-ea78-4936-bea8-1102cc61096a)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         0873d0c1-475b-4668-88ad-7df68361a9cd)(label(grove))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         e55ed1ce-7f70-41ba-9adb-a2359e20f361)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         5cb40499-20a7-4f28-9636-096a40fa7346)(content(Whitespace\" \
         \"))))(Tile((id da75adb4-5183-459f-b05f-3c566f35768d)(label(fun \
         ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 37))(sort Exp))))))(shards(0 \
         1))(children(((Secondary((id \
         d729eb06-874e-4787-ba74-18202f0f4c9f)(content(Whitespace\" \
         \"))))(Tile((id \
         328727eb-8811-43a0-a033-5658e56e4433)(label(\"(\"\")\"))(mold((out \
         Pat)(in_(Pat))(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0 1))(children(((Tile((id \
         c6addbdd-6a56-4ddf-9212-fbd36ec11bd0)(label(i))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         fa91ac1b-ab62-40f6-aff8-b45617b78fcc)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 44))(sort Pat))((shape(Concave \
         44))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         3d3abd63-4fb8-4a41-a442-a8f6af6c440d)(content(Whitespace\" \
         \"))))(Tile((id \
         3e604d15-73f8-4776-ae0a-07e7d9199457)(label(row))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children()))))))))(Secondary((id \
         591e02b7-3c03-4fd2-b8ba-6e0bb6fc33a1)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         2fa9f9a5-43c1-410b-ab47-3b876907bc57)(content(Whitespace\"\\n\"))))(Tile((id \
         e62ff137-0ec9-420e-b473-f2b0fb327a0d)(label(if then else))(mold((out \
         Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         36))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         e9f748ef-73eb-41e7-aa94-fefbb0827089)(content(Whitespace\" \
         \"))))(Tile((id \
         4a518b40-0905-4a77-9b5e-84523d6f7b97)(label(i))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         57c0041a-cfa9-4919-aed5-7d2ecf8bb35d)(content(Whitespace\" \
         \"))))(Tile((id \
         815b4b3a-f79f-4f0f-9ddb-9fb8b11aa993)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         73dd2e0e-8a18-432b-957b-9b4e297d5478)(content(Whitespace\" \
         \"))))(Tile((id \
         ee7fe5ae-0cd4-4c7c-80ae-8ed48135f46d)(label(targetRow))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         fc1aee44-bc80-43ae-a086-c5a8e684481a)(content(Whitespace\"\\n\")))))((Secondary((id \
         4fcaef3f-8d96-4a95-a0b2-749576dfe7dc)(content(Whitespace\" \
         \"))))(Tile((id \
         0af14e19-3821-4ce3-aa28-ad322de70cb8)(label(map))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         fcb2dfdd-0a4f-4051-8f79-372e7d3ce9f9)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         42ffc210-9dca-4632-af5b-baea6b945afc)(label(row))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         17420de5-a2cf-46a5-9e64-670733266cfc)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         ddd5728e-3fff-488b-98c1-339efc2a2d5b)(content(Whitespace\" \
         \"))))(Tile((id 765855a6-be8c-475a-b85e-dbdb68ef5126)(label(fun \
         ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 37))(sort Exp))))))(shards(0 \
         1))(children(((Secondary((id \
         1867f1aa-68dc-4803-b0e5-4932b6ce40f1)(content(Whitespace\" \
         \"))))(Tile((id \
         c3083887-a454-41f2-9a0d-56109ea38708)(label(_))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         14824fb8-d8b0-415d-932a-83e711f03a18)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         ef4ec8b7-9981-4858-b704-b68255b30525)(content(Whitespace\" \
         \"))))(Tile((id \
         a4e46564-802b-47b2-8fee-96820e467afb)(label(plant))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         d244d009-5de6-47f1-b009-70b48acb208f)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         0a2b58f5-99b0-43b6-afdb-01a9fba531dd)(content(Whitespace\" \
         \"))))(Tile((id \
         52ffe346-1dff-4837-8153-6b58c9ce6690)(label(row))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         4cc7668d-fb67-46ef-b2b8-716b96ec10e1)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         7a917e72-0cb9-4617-be8a-55b93dd017df)(content(Whitespace\"\\n\"))))(Secondary((id \
         1ce5bf25-202f-44f2-9704-cd6131b43855)(content(Whitespace\"\\n\"))))(Secondary((id \
         69f8bf9b-347b-4030-9007-8b0dafc96208)(content(Comment\"# TODO: Add \
         setCol helper here #\"))))(Secondary((id \
         18f5b80e-e84d-420b-be15-dbdf4123bf55)(content(Whitespace\"\\n\"))))(Secondary((id \
         ea916028-0396-4eaa-81da-a39765cd7bd8)(content(Comment\"# Hint: You \
         need to modify each row, changing #\"))))(Secondary((id \
         75455618-e676-4d15-b0ce-4e9470ffb966)(content(Whitespace\"\\n\"))))(Secondary((id \
         9d5190ec-c58c-4b27-9758-013227aeecf9)(content(Comment\"# only the \
         cell at the target column.         #\"))))(Secondary((id \
         ab6633da-9dc2-46bb-9843-74ce9a55f0f2)(content(Whitespace\"\\n\"))))(Secondary((id \
         29a453d6-30aa-4aab-8622-9bffcfcdb8fa)(content(Whitespace\"\\n\"))))(Tile((id \
         f9620305-b30c-4c6c-a88d-3137320eed9f)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         0fb01d75-474b-481f-b730-02e11a35378c)(content(Whitespace\" \
         \"))))(Tile((id \
         e5bc9260-e97c-41e8-9a07-64677ce12776)(label(setAll))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         5007aa18-59a8-40cc-875a-71e6257ad169)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 24))(sort Pat))((shape(Concave \
         24))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         72ba6433-26c0-483f-95a6-466dde6c40e1)(content(Whitespace\" \
         \"))))(Tile((id \
         60b42aee-10bb-4cab-aef4-9f5ac3479947)(label(\"(\"\")\"))(mold((out \
         Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0 1))(children(((Tile((id \
         07efdbce-6dfd-4f6c-9799-f29e15212585)(label(Grove))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         e1873e69-b0eb-4717-a204-42194dfe5865)(label(,))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 44))(sort Typ))((shape(Concave \
         44))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         d486170a-9959-47c3-a95b-555833c4c31c)(content(Whitespace\" \
         \"))))(Tile((id \
         634535f6-457c-47b1-9da1-670ba8506b19)(label(Plant))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children()))))))))(Secondary((id \
         3b2e3d98-a646-4a1b-84b5-7ee71d039591)(content(Whitespace\" \
         \"))))(Tile((id \
         55ffe742-ec3f-4b4d-bd52-446101b89553)(label(->))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 13))(sort Typ))((shape(Concave \
         13))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         a9612c2f-0b2a-4744-ac46-305d220f70b4)(content(Whitespace\" \
         \"))))(Tile((id \
         8fe3f945-6f96-4de6-b52c-34f1c77415f1)(label(Grove))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         5be01af0-ce6a-437e-b0ba-0d6c537aa0bd)(content(Whitespace\" \
         \")))))((Secondary((id \
         3a73ec25-b469-41d6-ae1a-bdb1cb51c2ff)(content(Whitespace\"\\n\"))))(Tile((id \
         d66e4833-c6e3-4719-b4ce-2afd79985079)(label(fun ->))(mold((out \
         Exp)(in_(Pat))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         37))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         35358765-3e4c-44c0-975d-5ec86254c65f)(content(Whitespace\" \
         \"))))(Tile((id \
         c78b6fc7-5360-4057-bba6-923004dbef72)(label(\"(\"\")\"))(mold((out \
         Pat)(in_(Pat))(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0 1))(children(((Tile((id \
         f95529ea-8327-4dc3-8670-7e3fd64501c2)(label(grove))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         4f2dbc3c-c069-4789-9185-340ba3c8fd50)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 44))(sort Pat))((shape(Concave \
         44))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         58220b13-868e-444f-a094-6e5616b4f02e)(content(Whitespace\" \
         \"))))(Tile((id \
         feff2a9b-2b39-473a-8ee0-32cb437f5949)(label(plant))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children()))))))))(Secondary((id \
         ea7495b1-f223-4688-9fc2-461d7664d1ca)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         6716de28-b2b7-407b-a757-294b470fdc57)(content(Whitespace\"\\n\"))))(Tile((id \
         40be905a-37fc-4911-96ba-ee8bb4b86e07)(label(map))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         c0dbbc1d-1534-4aab-8d4a-5f3e698cf5bd)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         e5a1d839-c189-4cd2-b9b9-720c67ced516)(label(grove))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         605b420e-a657-4922-90b4-b8edcdc8bc1c)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         1cb4e103-c4be-47e6-b649-1bc20dc172c8)(content(Whitespace\" \
         \"))))(Tile((id 04afb5e9-075a-4424-8847-322ebf3e303e)(label(fun \
         ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 37))(sort Exp))))))(shards(0 \
         1))(children(((Secondary((id \
         195f9fc0-312c-403a-bf9a-bf95194b58db)(content(Whitespace\" \
         \"))))(Tile((id \
         f0182a43-2ba4-47f9-9b37-bd4870a5a325)(label(row))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         d2b4b72a-efc9-4e06-ba20-53b46cf31988)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         c78b3cc1-54b8-422a-a8ff-9bc435aa1db3)(content(Whitespace\" \
         \"))))(Tile((id \
         b13e4976-a25a-4b37-b3ea-790502924cb6)(label(map))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         63c6d2a2-af53-48ae-b7d2-95d4c989f44f)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         cdb56c8c-70fa-4849-a64d-e27115b6dab7)(label(row))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         4620ee19-b42a-45a4-90e7-5d01b88cc71a)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         2c3c837f-932a-498b-9d90-c6a9e87a913e)(content(Whitespace\" \
         \"))))(Tile((id 96ae7ba3-87a3-4b32-a28e-bffdca52b00d)(label(fun \
         ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 37))(sort Exp))))))(shards(0 \
         1))(children(((Secondary((id \
         0eae7ee4-2042-4516-a070-1b9d25f60447)(content(Whitespace\" \
         \"))))(Tile((id \
         fc42ca9e-abeb-44e3-8404-75e736016cdb)(label(_))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         8913760d-1c00-4458-9dc0-731a75e82e5b)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         70aa0a30-4756-4534-af14-0d9cb52280f8)(content(Whitespace\" \
         \"))))(Tile((id \
         46453b4e-4e6e-4625-b8d7-fbdf849fad42)(label(plant))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))))))))))))(Secondary((id \
         be2127bc-6244-4eca-a50c-f6c1970e2bad)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         ec678e1b-f9e2-4522-9b4c-157534181f4b)(content(Whitespace\"\\n\"))))(Secondary((id \
         e3ddfdf5-47c4-4a87-99f5-db07433f1b77)(content(Whitespace\"\\n\"))))(Tile((id \
         481fd8f1-ec4f-4b44-86bf-ac8d4c392b94)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         18b77f25-bf52-4814-8b13-fe3dd8ea7dcc)(content(Whitespace\" \
         \"))))(Tile((id \
         d212fe0c-88a4-4090-a816-732c91c25795)(label(updateGrove))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         4a824e87-eaf3-4a10-a806-31f22c431825)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 24))(sort Pat))((shape(Concave \
         24))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         9e856ae4-9ac8-413f-9d49-6657c4c57328)(content(Whitespace\" \
         \"))))(Tile((id \
         904410c4-b420-4c12-8368-bf0d6a4e2344)(label(\"(\"\")\"))(mold((out \
         Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0 1))(children(((Tile((id \
         382f60cb-8e59-40eb-8af1-4874ed5d61b5)(label(Model))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         845feb20-4c49-44af-8b97-e3cd769848a7)(label(,))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 44))(sort Typ))((shape(Concave \
         44))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         d5b01069-5b42-4438-a201-9c4ae8df374b)(content(Whitespace\" \
         \"))))(Tile((id \
         e51ad79b-9efa-453e-bbda-e5378eab0340)(label(Grove))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         3a8495e8-d9c9-4a8c-a068-86cbbac3e0aa)(content(Whitespace\" \
         \"))))(Tile((id \
         9ec1b5de-093c-4f2e-a587-7f9e91a3f9aa)(label(->))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 13))(sort Typ))((shape(Concave \
         13))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         6830771d-d074-42be-b46e-c16891f53df1)(content(Whitespace\" \
         \"))))(Tile((id \
         bcc3731e-fa08-4803-b576-44e83c65f3e1)(label(Grove))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children()))))))))(Secondary((id \
         0aa52f92-0e05-4b3b-8f73-2d561db97698)(content(Whitespace\" \
         \"))))(Tile((id \
         69c3273d-dbd8-4dd4-85b1-22822720ebc6)(label(->))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 13))(sort Typ))((shape(Concave \
         13))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         1f9892ad-0f56-4706-a57f-7640b91f1316)(content(Whitespace\" \
         \"))))(Tile((id \
         1bd278d9-c3bd-4872-9812-de362c4cdb61)(label(Model))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         df92e25b-b175-41f0-82ad-92e4f1e151bc)(content(Whitespace\" \
         \")))))((Secondary((id \
         9d6ab1f8-01fe-44da-b84f-34dc16c4a6f2)(content(Whitespace\"\\n\"))))(Tile((id \
         0ff397b3-6beb-4cac-9188-6f2c053109c2)(label(fun ->))(mold((out \
         Exp)(in_(Pat))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         37))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         2bc0dc56-7781-41ad-82ff-bfd536553b38)(content(Whitespace\" \
         \"))))(Tile((id \
         8891b16c-cb4a-4420-8d5c-6a5206460cd3)(label(\"(\"\")\"))(mold((out \
         Pat)(in_(Pat))(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0 1))(children(((Tile((id \
         15bd4c68-6062-4d28-a212-11473f878d11)(label(m))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         0c7a80de-e6bf-4837-b331-468cd9bfe99c)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 44))(sort Pat))((shape(Concave \
         44))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         124e7533-9897-445f-9768-6611ba56e69d)(content(Whitespace\" \
         \"))))(Tile((id \
         4787774f-a718-4ecc-bca3-f4aade7cb3b0)(label(f))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children()))))))))(Secondary((id \
         ee8b9ab5-a6ce-494e-b602-ab58341075d0)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         fc08be53-bc89-4581-8ec7-4640492a3ff3)(content(Whitespace\" \
         \"))))(Tile((id \
         6527e2dc-48c3-4a23-8f52-51e026e40245)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         90ae2e36-f8a3-4207-8960-f6b79e50df27)(label(f))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         3dc48133-626a-40d4-9061-9ab7c7d10cb2)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         5be824f4-4869-4e5e-a0e4-594769321f86)(label(m))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         fb88c103-9b0a-49b3-a13a-427b86e3f0d8)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         c84499af-f336-4dac-b6b8-33d160591f63)(label(grove))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         3a2fe67b-4892-4b83-a1d5-0a67d71a06c7)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         454f08fc-1b42-4000-90ec-509be800f1f1)(content(Whitespace\" \
         \"))))(Tile((id \
         f96c3a3a-1e39-485d-96f3-97ead3da8607)(label(m))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         789145e1-0a52-4cbd-bf0f-c228854ddc10)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         a93a4bf1-06f1-4cff-b760-42950f8907e7)(label(currentSeed))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         fd27f7a8-077f-4316-b668-8fcf8716e518)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         5cc543e1-6e47-41fc-979a-e3499b126d26)(content(Whitespace\" \
         \"))))(Tile((id \
         60351718-b862-490b-b10d-1f07ff615ce9)(label(m))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         f32a8a50-68bf-47d3-9624-85bfd3e42c99)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         3da18220-ef29-4a57-8019-4cc41d9f00ad)(label(seedInventory))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         aada181e-8ed5-4f21-b79a-2afaaa06507c)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         7d3bf8b3-e59f-49c3-a5be-a6e4f70121ae)(content(Whitespace\"\\n\"))))(Secondary((id \
         855a0bd9-8ba3-41c2-9167-efb62b577077)(content(Whitespace\"\\n\"))))(Tile((id \
         566a9ef0-adcb-44c2-8be5-c4513ca64106)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         8c433e65-66a7-4a06-8f5c-d3a8a02cb38c)(content(Whitespace\" \
         \"))))(Tile((id \
         f2bef5cf-738a-4ced-b48e-45757c53ea12)(label(update))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         8a73e39a-3935-4ade-b0a4-9f3966fb952a)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 24))(sort Pat))((shape(Concave \
         24))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         c8ed65a5-cac1-46d1-9c87-4c691401c4c4)(content(Whitespace\" \
         \"))))(Tile((id \
         aa7cc33c-89bd-4798-9998-5e7284849de7)(label(\"(\"\")\"))(mold((out \
         Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0 1))(children(((Tile((id \
         46ca085b-7d36-4e17-bd69-5f3d820eed7b)(label(Model))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         acdb851c-cd6c-422a-afa5-1efc778fe568)(label(,))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 44))(sort Typ))((shape(Concave \
         44))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         48982142-589a-492b-89df-833c9a993b95)(content(Whitespace\" \
         \"))))(Tile((id \
         c31fbb04-7ab3-4b01-87c1-c81053402422)(label(Action))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children()))))))))(Secondary((id \
         caf533f8-de89-40d2-8a33-bb4770d0c02a)(content(Whitespace\" \
         \"))))(Tile((id \
         8508c920-30d9-41a0-8197-5d35f4e186ba)(label(->))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 13))(sort Typ))((shape(Concave \
         13))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         b3b8af9a-edb2-4fe9-bc58-41f6082fefbd)(content(Whitespace\" \
         \"))))(Tile((id \
         2f36bd9c-784b-4da5-a72c-27c398217e15)(label(Model))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         a6b8b6d0-74a7-4103-a0c1-0a37abcf3452)(content(Whitespace\" \
         \")))))((Secondary((id \
         ca2fb7d8-16e1-41c1-a3e1-1d2238d64f3b)(content(Whitespace\"\\n\"))))(Tile((id \
         692de86f-f57a-4395-964d-87a0b30d5d9d)(label(fun ->))(mold((out \
         Exp)(in_(Pat))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         37))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         a7f76c17-fa69-45e3-80ba-2958bd6b9795)(content(Whitespace\" \
         \"))))(Tile((id \
         f6a27482-f373-44fd-a75c-39abd7267be9)(label(m))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         ded7fad9-73e3-41c2-ac6e-0049347a507d)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 44))(sort Pat))((shape(Concave \
         44))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         022f52c1-e4b3-486b-b54a-fddc090690a6)(content(Whitespace\" \
         \"))))(Tile((id \
         4d5e691b-c044-4381-836c-e71cfa2c9c8a)(label(action))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         2514914a-fe18-48b9-b38c-3cb33aff96b4)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         aaaac513-2758-4f6d-86fb-56becb0394a7)(content(Whitespace\"\\n\"))))(Tile((id \
         91f7304f-ff48-4ef3-9ad7-7c84e14f9279)(label(case end))(mold((out \
         Exp)(in_(Rul))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         a06a05af-957b-4a89-846d-7b101eaa7621)(content(Whitespace\" \
         \"))))(Tile((id \
         2620a51d-c45f-48e6-bb08-25bce7f0c417)(label(action))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         f178cacc-b0c5-4882-9da2-50786e6a66a8)(content(Whitespace\"\\n\"))))(Tile((id \
         9eef63a3-734f-4bd7-b0b9-6e62cd960d38)(label(| =>))(mold((out \
         Rul)(in_(Pat))(nibs(((shape(Concave 46))(sort Exp))((shape(Concave \
         46))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         07673444-e8c5-438e-94d7-2e072e272f0b)(content(Whitespace\" \
         \"))))(Tile((id \
         90683247-a6da-42bc-a661-7f54545e4378)(label(SelectSeed))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         911de3c0-9c74-4e66-9c2a-8b242fd9c90d)(label(\"(\"\")\"))(mold((out \
         Pat)(in_(Pat))(nibs(((shape(Concave 23))(sort Pat))((shape \
         Convex)(sort Pat))))))(shards(0 1))(children(((Tile((id \
         adc54726-e88d-4e14-942f-9d88d9b884ba)(label(idx))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children()))))))))(Secondary((id \
         50f0b71a-8b7c-4a58-bca5-f1e81cd3cf5f)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         9bfe4c77-39f8-4d79-916f-89d9e2ef9c0b)(content(Whitespace\"\\n\"))))(Tile((id \
         9743aa63-d6f8-4c0a-9825-583c8b62f5d0)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         e67fdf01-dac5-4712-81bb-c8ce26354983)(label(m))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         c978973c-3ec1-4414-a24a-bc6019ca21a9)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         99680c3c-e527-43ed-8f23-fa3e2c181127)(label(grove))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         44c837a6-4294-4df0-b9f2-b54cd02b1292)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         538275b1-41fe-400a-af53-142b60983b0a)(content(Whitespace\" \
         \"))))(Tile((id \
         51daadc6-7f08-493d-b4fc-e0010ee4ce42)(label(nth))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         1a96aa47-e920-49fb-8538-6f119c1ee8dc)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         f950cb92-6b76-42ba-b27a-1dac2d9283f7)(label(m))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         6d426c17-2471-4862-a87b-cfa9a7456926)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         981aede4-1424-4694-9fef-6a88c05c0e2a)(label(seedInventory))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         d3de2a9b-7adf-4188-aa0d-00296ee37571)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         84ef0156-a0d9-49d9-bd92-bfab450499cf)(content(Whitespace\" \
         \"))))(Tile((id \
         c72d2bb3-d791-4aaa-b964-48a7f1bf700f)(label(idx))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         757ca5d5-5837-4cf4-99bb-c32546346c04)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         9801a765-e1b6-4597-a6e3-3b7bce45059b)(content(Whitespace\" \
         \"))))(Tile((id \
         a9145673-58d4-4239-9337-976804585fb0)(label(m))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         556a93de-1101-4e23-b1d6-aa649f472ab5)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         f2c98e65-4349-4b0a-a6e2-6b3650cf1289)(label(seedInventory))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         88b7f607-dcff-4de3-8b25-531d761f1e2a)(content(Whitespace\"\\n\"))))(Tile((id \
         6a438b07-01ba-480a-943f-4b200d1c853c)(label(| =>))(mold((out \
         Rul)(in_(Pat))(nibs(((shape(Concave 46))(sort Exp))((shape(Concave \
         46))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         e4afb599-f0df-407c-94a0-1f5cf42bf4c6)(content(Whitespace\" \
         \"))))(Tile((id \
         2d101c9e-51a2-4689-9493-dc66df9fa072)(label(PlantSeed))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         2a629d6a-0d82-4925-88ed-82ad44ba4750)(label(\"(\"\")\"))(mold((out \
         Pat)(in_(Pat))(nibs(((shape(Concave 23))(sort Pat))((shape \
         Convex)(sort Pat))))))(shards(0 1))(children(((Tile((id \
         b4dfb3cb-5a7c-4ed5-87e0-9c699ac27a36)(label(row))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         68d4c415-5ab7-4194-b105-e8761f6f054b)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 44))(sort Pat))((shape(Concave \
         44))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         2fa51ca3-cce2-4b6e-9225-12294ae23dfa)(content(Whitespace\" \
         \"))))(Tile((id \
         41afa65d-d9f3-4334-bab5-7b00d3da8786)(label(col))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children()))))))))(Secondary((id \
         6f5ebb42-7c91-4dbb-a327-05ac7632ed36)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         bcc6fcd2-a4f1-42b1-8555-25b245bd4e6d)(content(Whitespace\"\\n\"))))(Tile((id \
         90e83a14-302a-405a-b7ff-360eaf386905)(label(updateGrove))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         c6857167-9add-4655-b026-5fd8a2e08952)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         333ac1d8-aaaf-4c59-b2d1-2da4fbbb74fc)(label(m))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         3d9232f8-a7ed-4b8c-8cdb-d2c479be3da3)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         9bd4c96a-8b83-4cef-bce8-d35b38148258)(content(Whitespace\" \
         \"))))(Tile((id 1c322469-9e2b-4096-b567-5ebb03f6f56c)(label(fun \
         ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 37))(sort Exp))))))(shards(0 \
         1))(children(((Secondary((id \
         a8b8f054-8275-45d1-b8d0-be3d0319e558)(content(Whitespace\" \
         \"))))(Tile((id \
         48f8f1da-8b3d-49ec-9ecf-a04ddb2458bb)(label(g))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         ed70165a-2e33-4cec-9a04-e444bd5c025c)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         aae191e5-250a-4d16-9724-9fa9ad13dee4)(content(Whitespace\" \
         \"))))(Tile((id \
         46a4067c-055a-43ea-b0de-76a4aa7112f7)(label(setCell))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         f5763d13-f3d4-4b28-8755-5c47491e256f)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         615ea6bb-67a0-4330-8822-02fba36b5d90)(label(g))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         1d3730b0-989e-4021-a9dd-8453e0840dc6)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         6719c012-f5d3-46e1-9727-392795700253)(content(Whitespace\" \
         \"))))(Tile((id \
         16c5fe4b-da88-472e-953d-18b5ee34cdf5)(label(row))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         193129db-3f48-422b-afa0-935b16d17e1c)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         efef59bc-dad5-4233-a69b-fff80468c449)(content(Whitespace\" \
         \"))))(Tile((id \
         878a1f8b-85ab-41ab-ba24-46521559069d)(label(col))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         643822b3-5a82-4fda-8739-b320392b2cde)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         fcbc19da-33bd-4712-9725-930b6c60a567)(content(Whitespace\" \
         \"))))(Tile((id \
         936ad5b4-9af8-49d8-bef2-012127d11f95)(label(m))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         7331e2ad-3c21-4c39-a3cb-2b5caf638019)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         f710464e-2a39-4d46-9ea6-ac76e7ab0e69)(label(currentSeed))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))))))))))))(Secondary((id \
         9763cade-10be-4b18-96ef-6ea5c05fa2ff)(content(Whitespace\"\\n\"))))(Tile((id \
         48beafe0-000a-461d-9b46-0a37549014f7)(label(| =>))(mold((out \
         Rul)(in_(Pat))(nibs(((shape(Concave 46))(sort Exp))((shape(Concave \
         46))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         66785a20-e10b-47e8-8bdd-daa1c0bab157)(content(Whitespace\" \
         \"))))(Tile((id \
         5eb489b0-bf5e-4edd-8f0f-d6d1e692a74b)(label(Uproot))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         1eb3c5be-cc0d-4ecf-b922-2cdc4685fba9)(label(\"(\"\")\"))(mold((out \
         Pat)(in_(Pat))(nibs(((shape(Concave 23))(sort Pat))((shape \
         Convex)(sort Pat))))))(shards(0 1))(children(((Tile((id \
         956810c3-1a69-44b3-ad84-bb6ca71995e9)(label(row))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         2df55469-b5a2-4bd6-85e8-dc5f104e513b)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 44))(sort Pat))((shape(Concave \
         44))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         fd6e7ab7-5dec-4405-94f6-05f4f82196f0)(content(Whitespace\" \
         \"))))(Tile((id \
         700baabd-5959-48a1-b36f-629d87820453)(label(col))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children()))))))))(Secondary((id \
         b7d40f23-7b7c-4c64-9553-0db7e15c16c9)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         f5a88606-8817-4d9c-8ae1-2d420ceddedf)(content(Whitespace\"\\n\"))))(Tile((id \
         9c963958-726f-41a3-a94c-9548de2fa344)(label(updateGrove))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         33a7b6cb-e027-4b33-8b1d-ae778fe65ae0)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         e1416ee9-b989-440d-952e-6dac6958ce5c)(label(m))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         6673542d-872b-4940-81df-030edbb18068)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         09c0e153-e575-4b54-9a70-13e3b4cb15e1)(content(Whitespace\" \
         \"))))(Tile((id 410e1cff-2ea1-4e3d-907c-6f23fb610a60)(label(fun \
         ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 37))(sort Exp))))))(shards(0 \
         1))(children(((Secondary((id \
         1049f26c-1a27-48a8-8a9f-bd9fba2d6bf5)(content(Whitespace\" \
         \"))))(Tile((id \
         99344fcb-79d3-42ed-878e-c11e9d1aa767)(label(g))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         e243577f-5728-4554-8093-1e71d683028a)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         8b527029-791c-4ce8-8c83-a2b092f3d953)(content(Whitespace\" \
         \"))))(Tile((id \
         2ebd4808-97a1-4129-9efc-cad758944aec)(label(setCell))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         c5133226-bd5a-4448-8d62-e1c791f864b3)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         12f89ec7-9871-4b9d-8d4a-a1384eb6151a)(label(g))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         2b067cf2-e52c-4122-a2ff-a20b0858167c)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         65994e15-c9c4-4118-8651-53b9729363d1)(content(Whitespace\" \
         \"))))(Tile((id \
         edadbaba-bf36-493b-8527-a8b049a7a58f)(label(row))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         33f88833-ae90-4440-8655-c3334ebf8279)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         dcdbf2ac-e135-40ec-a175-f8f3491d378d)(content(Whitespace\" \
         \"))))(Tile((id \
         7c98e01b-43b6-45a7-a0f9-4378543dd36d)(label(col))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         f34afb43-8f81-4de8-b31a-4cc952527a5a)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         0fca7498-d224-41de-8f9e-5a612188c707)(content(Whitespace\" \
         \"))))(Tile((id \
         8d6e3bc0-ec53-4201-9044-1a79e28cd8d1)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))))))))))))(Secondary((id \
         20e9bac2-41f5-4a76-9dd3-a3ec9b3f9b9b)(content(Whitespace\"\\n\"))))(Tile((id \
         f22d0f7f-ada9-4a25-802c-3985963f20a2)(label(| =>))(mold((out \
         Rul)(in_(Pat))(nibs(((shape(Concave 46))(sort Exp))((shape(Concave \
         46))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         8aa59540-c915-4128-af80-3e437dcaa9b1)(content(Whitespace\" \
         \"))))(Tile((id \
         6954140c-85d5-42db-be4d-38abd855bb3e)(label(ClearGrove))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         30606242-e668-4a84-a3eb-f49087544fdf)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         19655bfb-904f-4949-ba29-ed889c069236)(content(Whitespace\"\\n\"))))(Tile((id \
         9db33167-c3d0-4bd4-bd4f-5c679da4c7b0)(label(updateGrove))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         509fc952-f11a-4e26-99aa-35d0689ca8ec)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         123453a5-a7b3-479a-9573-b3ea4f1166f6)(label(m))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         3789d87a-f915-40ab-a237-0033ff20dd85)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         c14676a8-fef6-4459-9c01-72d9895928cc)(content(Whitespace\" \
         \"))))(Tile((id 5bec5370-4b4e-4209-b810-2be617266fac)(label(fun \
         ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 37))(sort Exp))))))(shards(0 \
         1))(children(((Secondary((id \
         1c12f2a1-8c4e-4a09-8dca-8d7932bc5ea6)(content(Whitespace\" \
         \"))))(Tile((id \
         36f1064e-1c31-4686-adf9-16481bec76e7)(label(g))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         feedca3a-0542-481a-90ae-06abe51c2ba1)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         03b48556-028a-412a-8f33-79c2bb0e6021)(content(Whitespace\" \
         \"))))(Tile((id \
         15df5e25-9e0c-4b86-99bc-1ccedbce5500)(label(setAll))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         7bd06549-b77c-4959-9772-ec4f211d69be)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         a1797548-9d32-4478-b183-18317ae2a032)(label(g))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         4d61f38e-112f-4a37-ada2-0ec14c8ee7d9)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         b9340f15-6861-4519-9e91-10c0200933ec)(content(Whitespace\" \
         \"))))(Tile((id \
         932f0ca5-eef8-4162-b7ec-9fcdfacbe9f1)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))))))))))))(Secondary((id \
         bbbe235e-c728-4ab2-89ae-995604988796)(content(Whitespace\"\\n\"))))(Tile((id \
         8c66fd7d-0e06-4db1-ab2e-7bdc09eab239)(label(| =>))(mold((out \
         Rul)(in_(Pat))(nibs(((shape(Concave 46))(sort Exp))((shape(Concave \
         46))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         d147b101-3a80-490c-99c7-0a5e40a8b3c5)(content(Whitespace\" \
         \"))))(Tile((id \
         9fcb1af2-5c1d-4a47-b643-30758495f0d4)(label(PlantRow))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         dd6d275b-07d1-4454-8b47-a9840f693e35)(label(\"(\"\")\"))(mold((out \
         Pat)(in_(Pat))(nibs(((shape(Concave 23))(sort Pat))((shape \
         Convex)(sort Pat))))))(shards(0 1))(children(((Tile((id \
         a4213a07-f95d-4b34-be86-b9f11197d11e)(label(row))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children()))))))))(Secondary((id \
         4f5ac32e-b90d-46d2-b979-6d87d023f4a9)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         a6d81edb-4937-434a-8f9f-6bec77ce531d)(content(Whitespace\"\\n\"))))(Tile((id \
         47420ecc-120e-4f57-bc70-661c0da520c5)(label(updateGrove))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         410bcb02-2a97-49ae-b0a4-b4cf4c7517cd)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         5444c2f5-7b0b-4751-bb7d-f74c4a448425)(label(m))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         2a97dc23-249f-40b2-8c66-5a873129e7c0)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         2e864c36-cea5-4193-affb-dcb1bc353d34)(content(Whitespace\" \
         \"))))(Tile((id 864e2d30-14bf-4e1f-ab2f-decbbfae6774)(label(fun \
         ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 37))(sort Exp))))))(shards(0 \
         1))(children(((Secondary((id \
         038ef886-ffeb-4423-bf9d-4654260ca8b8)(content(Whitespace\" \
         \"))))(Tile((id \
         97b2ebe3-2fe9-4a40-b507-185a384272f3)(label(g))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         9f0706bb-ec31-4060-b027-5dc6554811ec)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         7f874cd0-d452-4bd9-8bd6-47c6edad4282)(content(Whitespace\" \
         \"))))(Tile((id \
         8aaadfdd-caa0-4de4-86a7-04e5d81ea926)(label(setRow))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         8a65c319-8800-4e44-ba40-2a48b7aa2456)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         2ae2d5c2-5583-455d-8064-7579eceaa530)(label(g))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         0737ee0e-2456-4303-80b5-031f42f3ad3a)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         f12cc933-0bba-4b9f-9bf0-91334083cd70)(content(Whitespace\" \
         \"))))(Tile((id \
         5225e27f-4299-4c3c-a6f5-e97be1fc9c04)(label(row))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         62041db7-5d45-46ac-901a-97b865842f33)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         795f3b6c-6661-49bb-8f74-253c1320dbc0)(content(Whitespace\" \
         \"))))(Tile((id \
         831f86c7-8bca-4aec-844a-9e5b75ee776b)(label(m))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         8a99eb3f-0518-44f4-ab5b-f884e9d89881)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         aa8be921-f296-4b8b-b451-9df31532a6ee)(label(currentSeed))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))))))))))))(Secondary((id \
         5ba04c27-9629-4842-bab9-c06460079565)(content(Whitespace\"\\n\"))))(Secondary((id \
         416fb16f-107a-4f89-9509-9ff49b96ca9c)(content(Comment\"# TODO: Add \
         PlantCol case here #\"))))(Secondary((id \
         d7867a3c-a775-424b-8b58-ecd127d8df21)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         90c020ea-db8e-4390-bbbf-f4cd1d80afb6)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         8a01ccf3-7762-4e3c-915b-dff0093a0b00)(content(Whitespace\"\\n\"))))(Secondary((id \
         0464d99f-5cfc-4db1-a310-6f2b7a668b00)(content(Whitespace\"\\n\"))))(Tile((id \
         dc77bb79-d9c6-49d9-99e8-2929943755af)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         cd4d9b30-641f-4a8b-ab97-5ed95a9d1959)(content(Whitespace\" \
         \"))))(Tile((id \
         784da240-ab18-4056-a017-a72102a14d68)(label(do))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         e5f113f4-5b52-4aa6-b186-a852ebce0e58)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 24))(sort Pat))((shape(Concave \
         24))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         5a12aa04-4666-4f4c-9c62-3104b88ea0ab)(content(Whitespace\" \
         \"))))(Tile((id \
         dc57d0fe-ae15-45b5-9be5-1853bea881cc)(label(\"(\"\")\"))(mold((out \
         Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0 1))(children(((Tile((id \
         15982556-41d4-4605-b137-5b79759cc5ac)(label(Model))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         90055b23-f9dc-40d8-b605-64416e21bf0f)(label(,))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 44))(sort Typ))((shape(Concave \
         44))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         d59af2ae-9abb-4d77-b69b-23d4904e6137)(content(Whitespace\" \
         \"))))(Tile((id 8218235a-297c-4d56-8951-012fd097b9e8)(label([ \
         ]))(mold((out Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape \
         Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
         f24b4647-b097-4929-b2d7-f9c64dab2cf4)(label(Action))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))))))))))))(Secondary((id \
         10d04fd9-8f7c-4b86-918d-af1713adce0c)(content(Whitespace\" \
         \"))))(Tile((id \
         8caf883a-5dd2-4797-ab49-7efb1ab7a9c4)(label(->))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 13))(sort Typ))((shape(Concave \
         13))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         d0eae316-5df6-40bd-944e-585572eb930d)(content(Whitespace\" \
         \"))))(Tile((id \
         8734f1b8-a23f-4261-bc90-2bd1683b8002)(label(Model))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         9831b670-cdd5-4971-8b5b-6bbe2e83c8c5)(content(Whitespace\" \
         \")))))((Secondary((id \
         d9986a83-d891-4d17-b8a8-11db875c0579)(content(Whitespace\"\\n\"))))(Tile((id \
         96ad46fc-7910-4877-b15f-88d90e277ba1)(label(fun ->))(mold((out \
         Exp)(in_(Pat))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         37))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         498d3022-a26a-4718-aa38-68904183fc85)(content(Whitespace\" \
         \"))))(Tile((id \
         ee8d1117-c185-4fde-b689-b2e6d26fa57f)(label(\"(\"\")\"))(mold((out \
         Pat)(in_(Pat))(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0 1))(children(((Tile((id \
         fd0694e1-ce03-448d-bfe8-6d01a444b4be)(label(init))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         83024287-afa9-458b-a1e4-d0193392a8fa)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 24))(sort Pat))((shape(Concave \
         24))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         c93283ee-7de8-46c6-8710-349f8b1fead4)(content(Whitespace\" \
         \"))))(Tile((id \
         42e52df4-3eb8-4529-bd73-10ebd6e58935)(label(Model))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         477e8182-9368-448d-ba76-ea6c8a429d76)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 44))(sort Pat))((shape(Concave \
         44))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         eaf70a18-5df6-4c7f-92f0-6c59adc96f39)(content(Whitespace\" \
         \"))))(Tile((id \
         56868930-6269-40c0-a255-268612616c95)(label(actions))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         cbb1a9c6-3720-42b2-8a48-e0340febb100)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 24))(sort Pat))((shape(Concave \
         24))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         d88e9cbf-0673-447f-a98b-72f6ddc9698b)(content(Whitespace\" \
         \"))))(Tile((id df5d4c82-e1bd-454e-9ceb-bb3f77114bda)(label([ \
         ]))(mold((out Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape \
         Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
         51809338-d2a5-40aa-ae58-a61a06b59a3a)(label(Action))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))))))))))))(Secondary((id \
         e9bceb1b-bb1d-4239-8adb-cefb53f7671e)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         da1c1677-04d2-4f87-a6db-acdb99376445)(content(Whitespace\"\\n\"))))(Tile((id \
         ee95faa3-94d1-4941-8979-a8a4d8841a7f)(label(fold_left))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         2f9461e6-6913-45a5-801c-b9196def1c11)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         8e7db8f0-0619-45c0-a560-a7e4a9980ad2)(label(actions))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         56e1349e-96ef-421c-a2b0-b91e612f8683)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         76395ae5-f024-4135-adf3-55b46fdcf7ba)(content(Whitespace\" \
         \"))))(Tile((id \
         27dd0d05-4dbf-41a6-983e-c39d0f3dbe69)(label(update))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         c4159d6a-ea48-4cef-b956-b4c62b31c1d3)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         95d343d1-aec9-471d-a77f-db00887b35e6)(content(Whitespace\" \
         \"))))(Tile((id \
         4b3ae11e-6ed0-482a-92ee-c978794bef5c)(label(init))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         e5ed37f9-55ed-4da9-8c15-39304597f215)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         c7912f29-7dbe-4b02-a45f-32f8a5b261b7)(content(Whitespace\"\\n\"))))(Secondary((id \
         a05789bc-8f59-4465-88ac-2fb96a08b9d6)(content(Whitespace\"\\n\"))))(Secondary((id \
         4863b824-cfa4-4c85-84a8-56532d814fc0)(content(Comment\"# Existing \
         tests #\"))))(Secondary((id \
         0e244cdb-e4ce-405a-b428-3293a896c6a6)(content(Whitespace\"\\n\"))))(Tile((id \
         103c7594-fcc5-45ee-8bfe-a86694176449)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         194f4db0-2904-4864-9358-6f1e6aaf72e7)(content(Whitespace\"\\n\"))))(Tile((id \
         7939c987-075d-4770-9914-ebcadede7655)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         374f1aa8-58f8-45a3-8a39-28af8fac3db5)(content(Whitespace\" \
         \"))))(Tile((id \
         5db0945b-38f3-465d-ac94-250679c6b7a8)(label(m))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         9a6f22fc-6a14-47e5-b521-930bf77f908d)(content(Whitespace\" \
         \")))))((Secondary((id \
         d576eecd-701a-4cb7-ba61-66dd398c5167)(content(Whitespace\" \
         \"))))(Tile((id \
         362b379e-99eb-4700-be46-4eb2c8acc90d)(label(update))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         2dee24f9-cdb7-4982-8ac3-0e7347d85e91)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         6793da85-8454-4ac0-a2a4-8a0c8d177284)(label(init))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         c73a9109-b8c7-4540-ab15-7fad0ac78908)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         989e979e-2e1f-449d-b935-2f6274fa1d44)(content(Whitespace\" \
         \"))))(Tile((id \
         d4c7d27c-77de-4470-80ca-3b6f9cff85b7)(label(PlantRow))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         d46b2140-9818-4873-a4a5-7b9960340fc2)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         fa66019e-a874-437f-bc55-0d26f838b2a8)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))))))))))))(Secondary((id \
         e775e38b-b46d-46f3-b036-edcce9fb5117)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         3f1a62ce-74ed-44a7-a2d8-60268b9e5670)(content(Whitespace\"\\n\"))))(Tile((id \
         a7f44896-8f57-40d9-938a-f36e9c04b264)(label(m))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         79c934f7-f120-4140-999a-01bcfbb198f2)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         eded758b-4989-4275-bfcb-192bc66b6c91)(label(grove))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         206fc7c7-8759-465f-b8c1-7272f2ff859f)(content(Whitespace\" \
         \"))))(Tile((id \
         cefb616a-58ab-413c-b0b1-bab662d34b7d)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         ada02f29-c054-448a-90dd-2d9ecf574289)(content(Whitespace\" \
         \"))))(Tile((id 0a1ea0f8-24c8-4381-8414-21958cf45f49)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         4ddbe961-a360-4a48-ad6e-87e32c73d4fd)(label([ ]))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         9fe976e8-41d1-49ab-8b93-3061eab53b91)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         53e511fd-f8b0-4bff-9ce9-cea35a3b0efd)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         fd7578c2-c92b-41b8-828f-4408270de15e)(content(Whitespace\" \
         \"))))(Tile((id \
         93eda18c-cb4e-48bf-86c9-cb3c8d39aae8)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         52f22f8d-332f-45d5-bb68-3ec079038bb9)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         ddbcd2c4-3782-428d-b66f-e24782f290d1)(content(Whitespace\" \
         \"))))(Tile((id \
         71e4f543-99cd-42b8-90f6-93f95f92bacd)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         dd9b3ba8-e224-45d5-86eb-7e9cf368ad66)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         f64b08e2-4526-49fe-9292-529560d4395b)(content(Whitespace\" \
         \"))))(Tile((id 1da4cbd0-273c-45cc-9cd2-d38aa1de57d9)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         43f8f830-6d54-4712-8a22-1c3a9fd8a066)(label(\"\\\"\\240\\159\\140\\177\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         3f70beb5-e6c1-4f8d-9c9b-73d0a1e940f4)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         ac9089b2-72c9-41e0-b550-cc789b162078)(content(Whitespace\" \
         \"))))(Tile((id \
         d0b31f5b-1c2e-4ba2-9792-34dd49038de5)(label(\"\\\"\\240\\159\\140\\177\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         3194ee4a-96b4-4041-af6e-60cf533cb49a)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         cad5ac76-5131-4d92-93d9-8bd3e3977298)(content(Whitespace\" \
         \"))))(Tile((id \
         ced6f289-37b1-4c95-a2f9-0e9248871481)(label(\"\\\"\\240\\159\\140\\177\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         e55c5ced-30c9-447c-ad6d-335fb764cc8c)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         14747757-b4b9-4263-bd00-19cdf4920372)(content(Whitespace\" \
         \"))))(Tile((id ca055326-05e9-4c88-b50d-2cbd9acb9ab5)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         1c9db6aa-e69d-4ecd-b842-62a03cfdc4d4)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         bb2685be-b1cc-4606-a006-c3d132997015)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         0556e25b-14d8-4339-bac2-4733ba1396f1)(content(Whitespace\" \
         \"))))(Tile((id \
         a6876843-4956-4f48-80b3-5a00ad146f0b)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         d0493e52-f98a-49eb-b23f-1c9b66dc5d46)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         59e02a64-6f63-4c89-9509-0dcd7885f338)(content(Whitespace\" \
         \"))))(Tile((id \
         d91e8d94-1621-4e48-b386-6949499e991c)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))))))))))))(Secondary((id \
         40539dc2-3c27-46c0-9d11-9606ff3cb35c)(content(Whitespace\"\\n\")))))))))(Tile((id \
         a3ed53e5-187d-4f9c-8158-67d4c50b4bc8)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         0b42f7b2-a579-431b-9c04-6a5bd2f53ca8)(content(Whitespace\"\\n\"))))(Secondary((id \
         907c9f0d-1a8f-402f-a122-a52e7a18950c)(content(Whitespace\"\\n\"))))(Secondary((id \
         d482ff50-c6a5-4b5d-80c2-c78392402733)(content(Comment\"# New tests \
         for PlantCol #\"))))(Secondary((id \
         9428c452-a81c-457e-9e38-39de064d070e)(content(Whitespace\"\\n\"))))(Tile((id \
         a22e118b-d3ac-48e9-b9a2-b9fa214024c3)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         0014ab86-ac4d-4d9b-a41d-3aa3a2a52482)(content(Whitespace\"\\n\"))))(Tile((id \
         80785f02-a1c0-4e65-a8b7-66648143a541)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         0aac76f9-6f05-464d-a840-89e6f5360899)(content(Whitespace\" \
         \"))))(Tile((id \
         39e4470c-c50f-4ef6-beee-b118187034cf)(label(m))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         505fd3a5-b4bb-44f6-a57f-ba1c1d093183)(content(Whitespace\" \
         \")))))((Secondary((id \
         d39f2aa1-3153-4753-b90a-cb4324d91f10)(content(Whitespace\" \
         \"))))(Tile((id \
         625757e0-3e3f-4022-bf04-4b684b486d51)(label(update))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         f412fd83-94cb-4c38-b2d3-9a0a7cfe0f29)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         07da89c6-0f6d-486f-95a7-b1cc7871f8f5)(label(init))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         928dc775-7539-475c-8829-80a614bd34d9)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         8a3a6aca-e531-40e3-82fc-19e7f49004e3)(content(Whitespace\" \
         \"))))(Tile((id \
         2635cfbf-ecc1-4339-8029-d9b378ff6779)(label(PlantCol))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         6c812d01-58d5-4bb3-99ef-6200d340b190)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         a543e8f2-7b26-4a6e-8254-c58afb7ea909)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))))))))))))(Secondary((id \
         55d50625-30f1-4c1e-84af-6ad98873951a)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         261c4cc1-c4cc-404a-a61e-8945a5c69aae)(content(Whitespace\"\\n\"))))(Tile((id \
         76a53326-fc0a-4cbd-97cb-f0dab3173e67)(label(m))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         51d01441-c106-49d7-8ec0-f6b370508833)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         0979d0a1-2061-407d-9546-477667fb658f)(label(grove))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         cdb4020a-1de2-4a28-b8fb-80795c337e14)(content(Whitespace\" \
         \"))))(Tile((id \
         ac32dddb-6ecf-4648-900c-d1f8cacebd92)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         da456441-073b-4a6d-bc78-6656bb7d59ad)(content(Whitespace\" \
         \"))))(Tile((id 39068fe8-3674-441f-9c12-88d93ca728a6)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         3795cf28-0e2e-4b1e-8a50-8f68bd4d7d71)(label([ ]))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         fc8d51f2-d971-4182-9192-39d25eec7a72)(label(\"\\\"\\240\\159\\140\\177\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         47d05dfe-ffcf-411e-b17c-b327992582bc)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         4ddc605b-a3d8-4a21-985c-bccbebfa8b7c)(content(Whitespace\" \
         \"))))(Tile((id \
         4fecfe10-0115-4ef3-a699-76ed4b1b9618)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         05478086-f9b1-4036-8880-6b7f07219726)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         e825d8fc-312a-4040-8e1d-ab2c6e3dbeb1)(content(Whitespace\" \
         \"))))(Tile((id \
         db6d945b-6c29-4c86-8dac-af86851e0b6a)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         ec432fd0-63bb-4c7a-b35a-b44689b9d739)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         172acd58-781c-4f51-956b-2868fd276fa0)(content(Whitespace\" \
         \"))))(Tile((id c4f5cea7-4697-472b-b0c0-602ed094ac14)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         658f55d0-1002-44d2-bf27-fb275931215a)(label(\"\\\"\\240\\159\\140\\177\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         d270d06d-7b3b-4f69-a9cd-8fa8a44f8ebb)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         18b50b03-cd74-46bf-9c16-80b8eef2cf4b)(content(Whitespace\" \
         \"))))(Tile((id \
         a62d40d1-bcd6-443c-8ce1-d6fc52bd7207)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         212ab91c-1fcc-4906-875d-ee891f51878c)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         f08d0c58-ac05-4ea6-9a54-41d8a404497d)(content(Whitespace\" \
         \"))))(Tile((id \
         dc8fc551-117f-455f-ada1-2bb79e0d6b24)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         4ef0065b-c137-4e09-86be-d2b72a7a4627)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         b7bb8900-675d-4219-ad6b-352b13257484)(content(Whitespace\" \
         \"))))(Tile((id 79cc0ee2-0d56-4687-bd7e-68bfdada78ce)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         f611d32f-07c1-4593-a8d3-bd4a2bb95a7c)(label(\"\\\"\\240\\159\\140\\177\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         ade1b7c1-4713-4755-9062-e4de8800a9ad)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         69c7abd2-a923-4914-ac87-2cc129cb23a6)(content(Whitespace\" \
         \"))))(Tile((id \
         b9080ce3-cd80-4e95-a0f6-c348f3400c79)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         114c6c1c-7ca6-4a4d-b7ea-ca16bc6ecb52)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         6cecd4c9-1129-4ef5-a043-8aadc5946579)(content(Whitespace\" \
         \"))))(Tile((id \
         f0432cf6-f444-4072-85af-212af5ddb856)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))))))))))))(Secondary((id \
         df33b0e0-5e1f-4576-89e6-04f4aa1f33b8)(content(Whitespace\"\\n\")))))))))(Tile((id \
         0413cfe5-d4e0-4cf5-a3c0-21325fca3440)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         83f5072e-8b33-4680-85b2-0f6a2d4a9869)(content(Whitespace\"\\n\"))))(Secondary((id \
         a81a8921-c1a5-44e6-a980-15315179db93)(content(Whitespace\"\\n\"))))(Tile((id \
         cfdf04c2-74ac-4156-9721-cf284cc682f7)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         7e7bcb02-4247-497a-a338-864f201458ac)(content(Whitespace\"\\n\"))))(Tile((id \
         b5f2ecac-8ee3-4bab-9e56-d66d0e4c6b08)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         888dace8-8e99-489c-b949-36c06047a4f9)(content(Whitespace\" \
         \"))))(Tile((id \
         806afaec-2e6b-4035-8b95-b91d862b0877)(label(m))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         2341d475-071b-4fbf-b967-298f42a167ac)(content(Whitespace\" \
         \")))))((Secondary((id \
         0523fbcb-eedf-4fb9-992b-ed7315d08379)(content(Whitespace\" \
         \"))))(Tile((id \
         0e8ca41f-25a5-4a88-a1b8-39166750744c)(label(update))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         3a9dfd14-f204-45be-a46f-564a853a68df)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         9255c1e8-82bf-4e64-a1b1-ad07c2d1af65)(label(init))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         4691d148-21c2-4aa9-bfaa-2ee4c676c7d5)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         2a714ae1-bc77-4bca-84b0-dd8f748ed4e1)(content(Whitespace\" \
         \"))))(Tile((id \
         eedc24e7-219c-4015-8e7e-4fcabd74beef)(label(PlantCol))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         cdc110b0-1cc2-4487-924a-b0e0f59a235f)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         da803576-a50b-4e32-a622-ece132f97a46)(label(2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))))))))))))(Secondary((id \
         8a0de176-da6f-4f12-b0cf-f10970a67761)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         d879d090-6f98-4347-84ea-af532701afd6)(content(Whitespace\"\\n\"))))(Tile((id \
         d0431044-3fde-4944-bc8a-787baa3c4417)(label(m))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         8fdfd1fa-795d-4c70-bb09-23ac0c98b27c)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         8ff07d54-5e67-4b38-aa57-e69d9486cd93)(label(grove))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         a9f2dcf5-221e-4dbb-a2e2-b454e4c71742)(content(Whitespace\" \
         \"))))(Tile((id \
         08947c7b-702f-4a26-94a9-e77d812684e7)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         c81bbf2e-b694-444e-920b-81f7290df636)(content(Whitespace\" \
         \"))))(Tile((id 9b38aeb4-e5a0-4c78-867e-859afc82e77a)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         2252ded1-7960-4b52-9ca6-1d92656aa535)(label([ ]))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         e94112df-6ad5-48fc-bcb5-dede6c19faee)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         df607d3e-4ff0-442c-8293-c18173d45121)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         71c8d3be-34ac-412c-a205-1f7c5fb906d2)(content(Whitespace\" \
         \"))))(Tile((id \
         312c4426-5c15-4119-b504-6888a72e5460)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         9c4859fc-f9d4-4bab-9e1a-809b803a07e4)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         53f41930-3a97-4c5f-a914-ec2ae719191f)(content(Whitespace\" \
         \"))))(Tile((id \
         d2fa5cb8-2506-4e8b-97c5-0b8659d6e227)(label(\"\\\"\\240\\159\\140\\177\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         b57d09dc-211c-4a8a-af00-079cd5394fee)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         78ed8950-eca3-4ab7-b3d7-d6368fd0b996)(content(Whitespace\" \
         \"))))(Tile((id 4b5b8a4d-78c9-4655-a7f6-308de218a997)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         a5be4417-d585-468b-867f-d3fb5a9088c3)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         9a50564c-4c95-4a5b-82d5-eeda15028b81)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         65051339-a373-433e-beb6-89c1f7f0e143)(content(Whitespace\" \
         \"))))(Tile((id \
         6da65f61-bb9f-4818-9fda-51f8008f0020)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         70056bf2-257f-4503-bad5-61a31a462751)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         eeb2c875-9a1c-482d-bdd1-d3ff2350c023)(content(Whitespace\" \
         \"))))(Tile((id \
         13473b49-e4cd-418b-b569-0cbf226063ad)(label(\"\\\"\\240\\159\\140\\177\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         6fab03b2-7157-4b76-9607-45bffd9f0cbf)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         0cfe840a-00fb-437f-ac1b-fb3ddde95836)(content(Whitespace\" \
         \"))))(Tile((id 9f7c54b2-f8aa-4af9-a2b1-ed361ca42679)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         b94f33b8-e953-4c49-9ee3-1361317ab06e)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         8db28068-6e3c-4222-988b-a4818c88fae1)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         5863e762-49ff-4202-ab97-8f4bb8147094)(content(Whitespace\" \
         \"))))(Tile((id \
         c905967c-284b-4683-bcc6-a19ed719c7f7)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         f5f9b416-31ee-49c5-b91e-09f23dbb22b9)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         4c6ea74b-6c38-4bce-bb66-6afd50c7962e)(content(Whitespace\" \
         \"))))(Tile((id \
         4182a084-50b3-4dd1-a054-8309c74cbdb5)(label(\"\\\"\\240\\159\\140\\177\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))))))))))))(Secondary((id \
         6be44dfa-7acf-44d3-8428-a9ce8985f207)(content(Whitespace\"\\n\")))))))))(Tile((id \
         54146aa7-9728-4cef-8649-7dbe6a717a22)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         ec49f405-4815-468d-954b-7d41ad14dd63)(content(Whitespace\"\\n\"))))(Secondary((id \
         0a61671e-4447-43fb-ae37-6d912ac0fc1d)(content(Whitespace\"\\n\"))))(Tile((id \
         7ad62228-c65f-45b0-ae0d-e93a769d1f3e)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         f14124c4-3344-494b-86bd-f4c0014a6f6b)(content(Whitespace\"\\n\"))))(Tile((id \
         cab6c7d4-d492-4e8d-bde5-6c4727dc872a)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         e84aadaf-3faf-45da-ad85-9d99118d4082)(content(Whitespace\" \
         \"))))(Tile((id \
         a2bcca60-7a89-4899-ba1a-9282563d4313)(label(m))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         e797e266-386f-4b4c-b487-a70841ce418f)(content(Whitespace\" \
         \")))))((Secondary((id \
         7e41151b-d3a1-4c73-a528-eb41c97ddae3)(content(Whitespace\" \
         \"))))(Tile((id \
         ed09aaf0-8780-402f-9750-2ceae1918215)(label(do))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         69e7ccfe-d842-4369-9474-1d7ba757babe)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         df794539-0deb-4d33-ac38-50c302229a4a)(label(init))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         4bd66bfb-77bb-4d71-9e08-33c36664a01c)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         ee53a4b8-16b8-4cff-b60b-fee379b4422f)(content(Whitespace\" \
         \"))))(Tile((id 5cf42208-6cde-4adc-985e-5f7b39fc5db7)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         3489f202-2c9b-4ab8-b64e-f43ad9879a36)(label(PlantRow))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         2ba2ba2e-3613-44c8-b564-bfdfa24a571e)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         7a2436e1-893c-4717-aaff-e4580e0e1a72)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         2898e244-3647-430d-a8e7-13edfe757f58)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         20cdd017-3d46-4c81-9bd5-acd23b40b9f2)(content(Whitespace\" \
         \"))))(Tile((id \
         7578257a-48f9-40ef-b4e7-c9745a4f54d3)(label(PlantCol))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         0bb08028-3e5c-4f60-800f-486186842e82)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         3970a5b8-b194-4600-89c0-ffe33d2f8f45)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))))))))))))(Secondary((id \
         3a0f3e37-c59f-4c52-82eb-995648bf68cc)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         0a1b19e4-fa50-479c-83b6-73b67051aa59)(content(Whitespace\"\\n\"))))(Tile((id \
         97ad9c0c-d8df-4c5d-8a1f-81c5f6157373)(label(m))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         ebf5e866-1022-482d-8e50-42421ce2859a)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         e4fb237b-e281-4a3a-94c3-6fb8e85e3c25)(label(grove))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         807df90c-c627-4385-8a26-f420024396d5)(content(Whitespace\" \
         \"))))(Tile((id \
         b4b34d45-cebd-413b-b961-3666df54a91e)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         d1fdb3ae-1358-4394-8a31-dd81d65127aa)(content(Whitespace\" \
         \"))))(Tile((id 9bc1a8a8-0f89-4a28-a708-e26feef0f455)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         bc41947f-3b47-4c66-896f-abc39198d447)(label([ ]))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         20719baf-48b1-4562-8e62-92483b9bcd0e)(label(\"\\\"\\240\\159\\140\\177\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         5174a618-24ed-4e29-8a4d-ff087f4db39f)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         47d07501-4c17-40e9-bcfe-cdb5adce2ddf)(content(Whitespace\" \
         \"))))(Tile((id \
         39ce7024-14a5-451d-b59c-990579107573)(label(\"\\\"\\240\\159\\140\\177\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         bb8df092-a061-4af1-a33c-47ac0b54e610)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         81257875-f6de-4fe6-bf1c-cb8bf43ace9c)(content(Whitespace\" \
         \"))))(Tile((id \
         7ba28acd-3f8a-4bb9-a938-f7c76107451f)(label(\"\\\"\\240\\159\\140\\177\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         06424731-b198-4b4d-8792-93d27a8f688d)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         d60eeceb-0065-4de3-b0de-848e90a3fd79)(content(Whitespace\" \
         \"))))(Tile((id dc0ec971-65be-4300-ab88-a1288b83a44d)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         f51c888e-0a4d-47c7-80b9-0fb340b7a16d)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         c628d192-62a2-4848-9668-5ee030816515)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         4df2ad83-4f5e-4804-9c24-1d428ad570a6)(content(Whitespace\" \
         \"))))(Tile((id \
         b3f0c83f-f024-4f98-b417-7de3623e2270)(label(\"\\\"\\240\\159\\140\\177\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         1f7e6d94-d0a6-4e86-b52f-bc6a6529912e)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         ab83f21d-74a6-451b-9295-cb79cd0085bd)(content(Whitespace\" \
         \"))))(Tile((id \
         df450808-e28f-45f2-92c1-3c1dafdcc576)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         4b32f4da-0c1c-46f5-a77a-a321b947477a)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         bd5d4826-4967-4dbc-8fed-063171f79930)(content(Whitespace\" \
         \"))))(Tile((id 84ee0ecc-3b87-4158-9494-a6b4cf8d7327)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         d4556a21-5e67-49e0-a1c5-ab7d0546bae7)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         f67e74c0-ae4f-44d4-8ee8-4ae607ecd3e8)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         36f1cd8b-ebf6-43d5-aaf4-c9791116eb3e)(content(Whitespace\" \
         \"))))(Tile((id \
         242a4941-1395-41ec-9b62-4fe84cadf53b)(label(\"\\\"\\240\\159\\140\\177\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         96109b71-a0aa-4f48-bcb1-f885f8b07f77)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         0f10a42d-2160-41f7-a2fe-5d5af5e20d18)(content(Whitespace\" \
         \"))))(Tile((id \
         4761a7e7-d981-4162-af15-074caf564a40)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))))))))))))(Secondary((id \
         65b6a99a-e1f5-48d1-9a3a-571c9de57a38)(content(Whitespace\"\\n\")))))))))(Tile((id \
         0b67711d-767d-4fa4-a23c-b40728cc13e6)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         c00c064e-e728-4209-a4b7-575bea841da5)(content(Whitespace\"\\n\"))))(Secondary((id \
         0c09a7a3-24bc-411a-a8bf-aa62b3b126be)(content(Whitespace\"\\n\"))))(Tile((id \
         2874593d-f459-4964-b033-7e06bd477bb3)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         d46f1bcf-1d34-496a-b867-87bb2de76060)(content(Whitespace\"\\n\"))))(Tile((id \
         a3088ec6-3b2b-4852-8ff0-93e6881b7380)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         f4bb125c-a8e7-4812-8b1a-1609d7e40dd0)(content(Whitespace\" \
         \"))))(Tile((id \
         6823eed4-e5a4-4cc4-a66c-4d380ac5ded8)(label(m))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         705b959a-3031-420b-9450-69247ad486da)(content(Whitespace\" \
         \")))))((Secondary((id \
         4bbcbad8-eb96-4119-959e-11c6728ce6c8)(content(Whitespace\" \
         \"))))(Tile((id \
         fa9d8599-7894-4569-aa09-72690a956725)(label(do))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         3dc6d62f-442d-446c-8856-99c5d23fd3d3)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         ac85facf-f2e8-4365-b570-81b9b89c41aa)(label(init))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         5efa046a-f69d-42d5-a234-4f991103ed42)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         50e309a9-a4d3-4115-979f-932ea7858009)(content(Whitespace\" \
         \"))))(Tile((id 232092c1-42d4-40d2-b62b-6133162d8b01)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         feeeded3-25d4-430e-87c3-fc11161023a0)(label(SelectSeed))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         2fec6a1f-9727-4bd8-80c4-699ff15fdac4)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         8d5c7ef2-b285-4de7-b368-5ac5711a4168)(label(2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         0687f154-7a45-4b41-9136-15bc8375043a)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         90a90ece-2f1b-4b5c-b0d9-764e3216ee1b)(content(Whitespace\" \
         \"))))(Tile((id \
         0571e707-85b1-4c9d-859a-0a60ae8bb9dd)(label(PlantCol))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         4675822a-1372-49ab-b1d6-ca1ca715d9bd)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         966cf4fd-b1da-4abb-8553-ab61d80c3dcc)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))))))))))))(Secondary((id \
         c5c7b4ad-ca0d-4572-95e1-24d3ad22a46a)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         65b75216-bc7e-4545-9537-63176ab1abc7)(content(Whitespace\"\\n\"))))(Tile((id \
         4608f836-6eb1-4cea-afd9-f50b2cbe7629)(label(m))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         740d9978-cfa7-4c62-bc80-5339c730ddf5)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         00be8621-0e57-4b91-89d2-c8e169549cf7)(label(grove))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         3ddde269-abcf-4423-9956-86ea0d7f1e0c)(content(Whitespace\" \
         \"))))(Tile((id \
         b76b012d-d981-484f-bd4f-8c528e2e108d)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         cd3ad19b-8cd0-4eaa-b800-a62568018363)(content(Whitespace\" \
         \"))))(Tile((id 65860762-5820-4adc-bf76-495ef96bfae2)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         334fdd4d-d718-4693-918c-a4a21c88f90d)(label([ ]))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         9ad4755a-1785-4c23-9718-c27cf78953b0)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         2eb7b395-a790-430b-8e65-109d647c8f33)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         b0405f74-b122-41be-87ac-af9b9db4e641)(content(Whitespace\" \
         \"))))(Tile((id \
         b07ae7e3-6e23-4693-bb9a-6040627dbdb2)(label(\"\\\"\\240\\159\\141\\132\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         d9b71dc5-eae5-4bf4-a251-15edbaa6b338)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         c9c5e35b-976b-4755-a255-0d4701e7167d)(content(Whitespace\" \
         \"))))(Tile((id \
         796f6ea6-6a4f-4b3a-bfdc-e210ebb09cb4)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         5bb04c62-586b-4f38-98ae-d728427dd34b)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         9553876f-c977-4904-ab94-7b20bae8c357)(content(Whitespace\" \
         \"))))(Tile((id 53a41de6-c0ea-4909-aa3a-71ddf186ef1f)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         8229f78e-8973-44f9-8137-c582a0b9ce92)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         2e70197b-3a3f-4d9b-8e70-bbd5da18db40)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         ccf4d572-1ffa-4fe2-a9b6-e189c9a2a031)(content(Whitespace\" \
         \"))))(Tile((id \
         b8825e7b-1e2f-488b-8089-47242b5015fc)(label(\"\\\"\\240\\159\\141\\132\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         9336c994-af07-4fb0-a987-9e9a47583045)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         79ee5df2-7f2d-4fdc-9e21-fa6e4864ed06)(content(Whitespace\" \
         \"))))(Tile((id \
         a9d3e187-9136-4103-bc23-b1b790b64ed8)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         37a2d328-e24d-40b9-9fa3-1a3edf488220)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         a40411bc-385a-4753-a6b2-57dafc6babfa)(content(Whitespace\" \
         \"))))(Tile((id 5decf4c8-a564-40ea-a552-b6db907ec7ad)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         77f66ff6-224d-4226-b47b-e84ca241ce4d)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         c08e0346-51cd-4a99-8bf2-3604b70d7b26)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         c8d57988-da7c-4774-b80e-775b4514fb6d)(content(Whitespace\" \
         \"))))(Tile((id \
         7d5f88ef-c36b-475d-b095-3b9facd5b1c1)(label(\"\\\"\\240\\159\\141\\132\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         d498d9c5-773a-43ef-b137-b84dcdbdf4f3)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         98628630-64c2-4233-ace7-fbb56d8cdb1f)(content(Whitespace\" \
         \"))))(Tile((id \
         d3874ff3-fce3-4553-b14e-fa07adf3566e)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))))))))))))(Secondary((id \
         18194c50-45da-4b08-a88e-8400b2e6d30c)(content(Whitespace\"\\n\"))))(Tile((id \
         a283e27b-f759-43ca-9b7c-5cfcb02624df)(label(&&))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 32))(sort Exp))((shape(Concave \
         32))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         8a6cba48-f628-42bb-94c5-4a4cf9f6ddc5)(content(Whitespace\" \
         \"))))(Tile((id \
         8748e5fd-0f61-4146-abc7-1a7547b23c0e)(label(m))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         f7ae7e72-f66c-4000-aca4-643449582b22)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         28ccf726-c42b-4f9c-a1a8-b0be83fbf231)(label(currentSeed))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         800bc522-a412-47b2-b6f7-764478fcf987)(content(Whitespace\" \
         \"))))(Tile((id \
         bd7741cd-dcf0-4a67-922e-dcf46a366f8f)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         889cf404-50f0-405c-8979-5e99e969f4d4)(content(Whitespace\" \
         \"))))(Tile((id \
         54a391ee-4a01-4d50-95ac-976d6727ec68)(label(\"\\\"\\240\\159\\141\\132\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         e4457827-ff96-4553-ad9c-8f102642f592)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         ba132f17-5e69-44eb-8eac-b1e1d89d5008)(content(Whitespace\"\\n\")))))";
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
