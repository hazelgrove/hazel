let out : string * Haz3lcore.PersistentSegment.t =
  ( "Study / tasks-draft / 07W-crop-plotter-extend",
    {
      segment =
        "((Secondary((id \
         9e2185a9-a25b-4a12-b24a-10d99f97fe38)(content(Comment\"# CROP PLOTTER \
         EXTENSION TASK                     #\"))))(Secondary((id \
         3edac7f1-b444-4c41-bd1c-7f968bcbb0ae)(content(Whitespace\"\\n\"))))(Secondary((id \
         1183691c-b3cb-4582-b1a3-a528749bf64e)(content(Comment\"#                                                 \
         #\"))))(Secondary((id \
         eb1512bc-276c-4154-9288-1483c1759854)(content(Whitespace\"\\n\"))))(Secondary((id \
         a3f472a1-3a65-4841-a8aa-2420a0cefd75)(content(Comment\"# The crop \
         plotter app lets you plant seeds on    #\"))))(Secondary((id \
         0f9229e2-c1e2-4de6-9ad9-d8362f2e38e6)(content(Whitespace\"\\n\"))))(Secondary((id \
         d37369fb-0b34-4fac-989a-5b35be16260b)(content(Comment\"# a grid. It \
         already supports planting rows.      #\"))))(Secondary((id \
         5ba1da15-9224-4112-8ae1-5617efdc27c3)(content(Whitespace\"\\n\"))))(Secondary((id \
         8c6ac46b-4edb-46cf-9cec-784c5805a34c)(content(Comment\"#                                                 \
         #\"))))(Secondary((id \
         412e101d-f46e-471d-bb58-9bfc87d63916)(content(Whitespace\"\\n\"))))(Secondary((id \
         b35ab542-9dac-48f4-8d6a-f0240794b99c)(content(Comment\"# YOUR TASK: \
         Add a PlantCol action that fills     #\"))))(Secondary((id \
         656f0871-6d5b-4a86-955e-ec1064f3fac0)(content(Whitespace\"\\n\"))))(Secondary((id \
         9988e04e-7835-4606-8672-e8db968bbb9f)(content(Comment\"# an entire \
         column with the current seed.         #\"))))(Secondary((id \
         2073e659-931d-4520-a05d-b48b697c0e11)(content(Whitespace\"\\n\"))))(Secondary((id \
         eed01141-2ea3-4d06-96ee-53574ba18892)(content(Comment\"#                                                 \
         #\"))))(Secondary((id \
         680c4ced-8402-4ca9-85e5-a9613aa6a27a)(content(Whitespace\"\\n\"))))(Secondary((id \
         d93a5035-d134-4c89-83e9-4b4c37a6aa6f)(content(Comment\"# You need \
         to:                                    #\"))))(Secondary((id \
         847dfb48-9d0e-41b8-bdd2-4474914af3df)(content(Whitespace\"\\n\"))))(Secondary((id \
         f2ccdad9-bac8-4def-b87c-c0c6be5570b9)(content(Comment\"#   1. Add \
         PlantCol(Col) to the Action type       #\"))))(Secondary((id \
         297e2c89-d772-4a51-b9a9-5f0a0402f522)(content(Whitespace\"\\n\"))))(Secondary((id \
         e1a5da39-bac9-43ff-a35a-d54d0f3f5fa0)(content(Comment\"#   2. Add a \
         setCol helper function               #\"))))(Secondary((id \
         29940825-097b-4f85-9487-b88d58909749)(content(Whitespace\"\\n\"))))(Secondary((id \
         a2f39c5a-5ec0-4688-a666-0e20721f43aa)(content(Comment\"#   3. Handle \
         PlantCol in the update function     #\"))))(Secondary((id \
         fb1d781a-1b85-43bf-a1dc-fce099c7b8de)(content(Whitespace\"\\n\"))))(Secondary((id \
         b8800624-19ac-4ccc-965c-e9334cee2243)(content(Comment\"#                                                 \
         #\"))))(Secondary((id \
         5863bcf1-b2b8-4029-9985-69d7ecf6a189)(content(Whitespace\"\\n\"))))(Secondary((id \
         696373f2-539a-45e9-868a-fc7524dc2e60)(content(Comment\"# Look at how \
         PlantRow is implemented for         #\"))))(Secondary((id \
         f9618383-a254-471e-868a-589802563e1d)(content(Whitespace\"\\n\"))))(Secondary((id \
         d48d9533-9077-4398-aaa1-3de2fbfd83d5)(content(Comment\"# guidance - \
         PlantCol is similar but vertical.    #\"))))(Secondary((id \
         7a5d1366-d798-4ee4-9114-9e70ba9c1875)(content(Whitespace\"\\n\"))))(Secondary((id \
         b2a76000-8c74-469f-921d-8f41fefaf6e1)(content(Comment\"#                                                 \
         #\"))))(Secondary((id \
         b07c2531-6171-4dd4-a9df-6fcf09dcd44d)(content(Whitespace\"\\n\"))))(Secondary((id \
         f70036d2-8e42-47ed-96db-83f5372c31dd)(content(Comment\"# Tip: Use \
         auto-probe to see how the grove        #\"))))(Secondary((id \
         da36b31d-cbdd-42df-ae65-e816bf11f2ca)(content(Whitespace\"\\n\"))))(Secondary((id \
         0158d743-22ec-4efe-b467-27e4dbe20e06)(content(Comment\"# changes \
         after each action.                      #\"))))(Secondary((id \
         5785afee-9936-43bd-9ee3-7a924c18de03)(content(Whitespace\"\\n\"))))(Secondary((id \
         1ad17314-bc1b-40e3-a61a-56bbc5a92dba)(content(Whitespace\"\\n\"))))(Tile((id \
         abae631d-89fc-493f-a6bb-836a1f4e6363)(label(type = in))(mold((out \
         Exp)(in_(TPat Typ))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         ed0f4b29-30df-4805-b5eb-41753257ef19)(content(Whitespace\" \
         \"))))(Tile((id \
         a181011c-db13-4fbc-b030-1aa362f38abd)(label(Plant))(mold((out \
         TPat)(in_())(nibs(((shape Convex)(sort TPat))((shape Convex)(sort \
         TPat))))))(shards(0))(children())))(Secondary((id \
         4d8fd7e6-ff63-4f12-8ad5-80c804822f99)(content(Whitespace\" \
         \")))))((Secondary((id \
         1d60117c-f112-460c-856c-636c3b747521)(content(Whitespace\" \
         \"))))(Tile((id \
         f12a88e2-f6a7-4f6b-b66d-7748bf0a3e6a)(label(String))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         1341ff9f-2e1d-45cd-a5e1-a8def6d7b4e2)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         72d3eec0-6c1d-4c0f-8526-86c0d872bd68)(content(Whitespace\"\\n\"))))(Tile((id \
         350d9690-7435-49e1-bd06-fe585c13fec9)(label(type = in))(mold((out \
         Exp)(in_(TPat Typ))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         91626588-6c58-459c-906b-9ea05f744bf5)(content(Whitespace\" \
         \"))))(Tile((id \
         cd1f9b77-6728-47cf-91a6-6abe45c83e3e)(label(Grove))(mold((out \
         TPat)(in_())(nibs(((shape Convex)(sort TPat))((shape Convex)(sort \
         TPat))))))(shards(0))(children())))(Secondary((id \
         ae4a9d9f-a335-42ba-99d1-8ebe67fc6afe)(content(Whitespace\" \
         \")))))((Secondary((id \
         ca7e0e08-1080-4481-b215-d11152f15934)(content(Whitespace\" \
         \"))))(Tile((id 15e6ad0c-00c2-4f54-bf23-3711072873c4)(label([ \
         ]))(mold((out Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape \
         Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
         6affaff3-8613-4887-b74c-253086c29d3c)(label([ ]))(mold((out \
         Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0 1))(children(((Tile((id \
         32dcad07-9a5d-4ef2-99d2-162ff6f5889c)(label(Plant))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))))))))))))(Secondary((id \
         2284750a-c70f-4eaf-acef-d23b3bef8d68)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         33cd3741-6450-40e0-9ca8-2c5e61dc3903)(content(Whitespace\"\\n\"))))(Tile((id \
         54f17873-20dc-4d55-9992-1353a0b0edc6)(label(type = in))(mold((out \
         Exp)(in_(TPat Typ))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         95fc3187-e73e-4df6-9761-ab62f859387b)(content(Whitespace\" \
         \"))))(Tile((id \
         dfabcf5e-9fea-4167-b3d8-1862079cef2f)(label(Row))(mold((out \
         TPat)(in_())(nibs(((shape Convex)(sort TPat))((shape Convex)(sort \
         TPat))))))(shards(0))(children())))(Secondary((id \
         5566c0ab-5b01-4145-ae0d-33a3dfae50aa)(content(Whitespace\" \
         \")))))((Secondary((id \
         81411d02-ada7-48d1-a2e4-097918fe6f00)(content(Whitespace\" \
         \"))))(Tile((id \
         9d19bc0e-2d4f-420a-94b8-e6f2874064b8)(label(Int))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         f016e75b-ee45-44c0-aa43-34b7de9a3acb)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         f98f38ab-796c-4535-b414-f4769971b70d)(content(Whitespace\"\\n\"))))(Tile((id \
         f20eeb50-74b9-4ac5-9489-f4b3f6665fd2)(label(type = in))(mold((out \
         Exp)(in_(TPat Typ))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         ce6d0e73-38df-4c5b-ae47-eecc9136bf7f)(content(Whitespace\" \
         \"))))(Tile((id \
         77abfc6e-5e17-44b9-b953-992c11d7a4b9)(label(Col))(mold((out \
         TPat)(in_())(nibs(((shape Convex)(sort TPat))((shape Convex)(sort \
         TPat))))))(shards(0))(children())))(Secondary((id \
         e7c12267-633d-4a1b-be88-06b43cd157f3)(content(Whitespace\" \
         \")))))((Secondary((id \
         d874c3b7-e3aa-4db5-ad7f-720885fe10c9)(content(Whitespace\" \
         \"))))(Tile((id \
         9b641573-463c-4d90-a7c8-a36ed25819a5)(label(Int))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         3640d394-d00e-4e8f-98c3-a16ac547f34c)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         af554df7-5a1b-4c74-9ec5-0bafc2b6b1e2)(content(Whitespace\"\\n\"))))(Secondary((id \
         e26baf35-8b6c-4b9e-9e67-2bf5b258bafb)(content(Whitespace\"\\n\"))))(Tile((id \
         607f05d0-5fec-45cd-b500-67d4218548e9)(label(type = in))(mold((out \
         Exp)(in_(TPat Typ))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         20d79c32-cc4a-46d4-a3df-91ad49b9b421)(content(Whitespace\" \
         \"))))(Tile((id \
         a3a3cb40-2ee6-44d6-9893-0bf0c5092550)(label(Model))(mold((out \
         TPat)(in_())(nibs(((shape Convex)(sort TPat))((shape Convex)(sort \
         TPat))))))(shards(0))(children())))(Secondary((id \
         33298135-7311-4e11-833f-f3f278aabf24)(content(Whitespace\" \
         \")))))((Secondary((id \
         cbc44b33-af2f-4bce-9ec9-c5af14084d8c)(content(Whitespace\" \
         \"))))(Tile((id \
         f22501b2-f5a2-4308-b893-7521a2cc8327)(label(\"(\"\")\"))(mold((out \
         Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0 1))(children(((Secondary((id \
         a2f14a9e-29c6-4099-96c1-8fc8d7cbf3d8)(content(Whitespace\"\\n\"))))(Tile((id \
         f1ceb3a9-554a-4c59-af42-0a01de81ddf0)(label(grove))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         6686de9f-ff6c-45a3-987d-272781bb83ab)(content(Whitespace\" \
         \"))))(Tile((id \
         9b3133e1-59fc-4096-a710-938c8eb0ebfd)(label(=))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 39))(sort Typ))((shape(Concave \
         39))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         ef22326b-8c8c-420e-8e4c-724dcad26dc9)(content(Whitespace\" \
         \"))))(Tile((id \
         d6e868f1-14bb-4b88-862a-b9ddc3903909)(label(Grove))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         845fc635-2f0e-4606-89e9-0a0b55d8cf63)(label(,))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 44))(sort Typ))((shape(Concave \
         44))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         75ad4142-8ae2-413b-aa19-b97b6033b812)(content(Whitespace\"\\n\"))))(Tile((id \
         1054f069-b10d-47f5-9dbc-07a92f849fbb)(label(currentSeed))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         5a470e44-75b2-403a-9e21-0b5593b03015)(content(Whitespace\" \
         \"))))(Tile((id \
         b199c7b3-8888-4c92-884d-e0a7935a3359)(label(=))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 39))(sort Typ))((shape(Concave \
         39))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         b8f4a460-a70f-4ef7-a3cb-8c7bc3359ae8)(content(Whitespace\" \
         \"))))(Tile((id \
         b2c76a65-abd9-41ed-a4f8-0120bd5bc95e)(label(Plant))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         85e5de02-38ed-4b39-9726-75294a4b220d)(label(,))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 44))(sort Typ))((shape(Concave \
         44))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         1fcb69c9-792e-43e4-b1df-b0a100e60342)(content(Whitespace\"\\n\"))))(Tile((id \
         bda4f38a-e9bf-4c87-9c69-c36c45ca0a91)(label(seedInventory))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         0a840a5e-80e0-4c13-990d-ed99585901b0)(content(Whitespace\" \
         \"))))(Tile((id \
         59695d10-de0b-4d2a-b970-140bfa9dc7eb)(label(=))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 39))(sort Typ))((shape(Concave \
         39))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         8caa8478-fc30-4364-b262-1c4fc3886444)(content(Whitespace\" \
         \"))))(Tile((id 3b01666b-bdb7-44a5-b1f1-4a8b1465be84)(label([ \
         ]))(mold((out Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape \
         Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
         caf094db-4068-4862-9375-e7972b9d364a)(label(Plant))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children()))))))))(Secondary((id \
         ca9efa77-2edf-4a0b-a0b8-fff1d9c0d867)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         fe3cbe77-7e27-4ee1-a279-18e319f4808f)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         d2ccf286-1832-4923-a101-f55b479e21f0)(content(Whitespace\"\\n\"))))(Secondary((id \
         f2399f6a-24d9-4c34-9c15-bfb4bb7f6f2f)(content(Whitespace\"\\n\"))))(Tile((id \
         77403437-f5f7-4824-9e12-8956e31dffb1)(label(type = in))(mold((out \
         Exp)(in_(TPat Typ))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         9a8606f3-057c-4d33-b9c2-66a51c99c305)(content(Whitespace\" \
         \"))))(Tile((id \
         571c5111-8a60-4df7-8db5-27c6d9301783)(label(Action))(mold((out \
         TPat)(in_())(nibs(((shape Convex)(sort TPat))((shape Convex)(sort \
         TPat))))))(shards(0))(children())))(Secondary((id \
         8cbc5776-fad5-4824-818e-80bf7b6f7eec)(content(Whitespace\" \
         \")))))((Secondary((id \
         87fef5f5-0126-4a67-a6b8-2fddcf63e878)(content(Whitespace\"\\n\"))))(Tile((id \
         a178a3e1-9552-46ce-8530-3121f47fb1c0)(label(+))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape(Concave 33))(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         13ee2ef0-0f1c-4d7b-88ab-5516ce16029b)(content(Whitespace\" \
         \"))))(Tile((id \
         ed40e3bd-f038-4948-9125-450200896a80)(label(SelectSeed))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         9b687d97-ae30-4ed0-9f0f-34a275e251a0)(label(\"(\"\")\"))(mold((out \
         Typ)(in_(Typ))(nibs(((shape(Concave 11))(sort Typ))((shape \
         Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
         4b234864-76c2-47c6-8338-edb6df41d570)(label(Int))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children()))))))))(Secondary((id \
         88bad31b-0055-4d94-b699-edd39c024afc)(content(Whitespace\"\\n\"))))(Tile((id \
         98d6956a-173f-4997-932e-6b646ea3a4fc)(label(+))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 12))(sort Typ))((shape(Concave \
         12))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         04a9b639-3b46-4317-a3cf-d58f1b86b798)(content(Whitespace\" \
         \"))))(Tile((id \
         235a6926-77a0-47a6-b5ac-5c33b4c3e3b9)(label(PlantSeed))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         0e8cbbbb-187e-45ec-a537-8f7fbf73c86e)(label(\"(\"\")\"))(mold((out \
         Typ)(in_(Typ))(nibs(((shape(Concave 11))(sort Typ))((shape \
         Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
         8ce8746e-8961-4c2a-a1eb-e923d626f7ec)(label(Row))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         fdac1f7d-726e-4be0-9c4d-9118b93b2860)(label(,))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 44))(sort Typ))((shape(Concave \
         44))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         52bd6bb4-6d4e-477f-9c4d-354af99f3090)(content(Whitespace\" \
         \"))))(Tile((id \
         7eb1a9a0-e24b-489e-bacb-428d55688975)(label(Col))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children()))))))))(Secondary((id \
         c5a2dab9-1deb-4f32-82e6-09ea20b5c348)(content(Whitespace\"\\n\"))))(Tile((id \
         06be82f8-3b30-4bd7-a41e-a4ccf63069b7)(label(+))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 12))(sort Typ))((shape(Concave \
         12))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         527c7d52-16ba-40ee-bccd-1b8695799e2f)(content(Whitespace\" \
         \"))))(Tile((id \
         2fd6c470-8f34-4ae6-abdd-10310398a530)(label(Uproot))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         7c289c1d-0111-41a5-ba4b-be2468ec244c)(label(\"(\"\")\"))(mold((out \
         Typ)(in_(Typ))(nibs(((shape(Concave 11))(sort Typ))((shape \
         Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
         6eba4026-b833-4e68-a5ed-3e5ef6596ef2)(label(Row))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         b7782144-99ae-4d6a-8125-f82f976c5739)(label(,))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 44))(sort Typ))((shape(Concave \
         44))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         f917fd5d-340f-4faf-839e-ab245531e78a)(content(Whitespace\" \
         \"))))(Tile((id \
         07cc31e8-ee53-4f3d-b1dc-a0ad98518c89)(label(Col))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children()))))))))(Secondary((id \
         2e5fb23f-f6ce-44a1-bd60-7a823fb8d0f8)(content(Whitespace\"\\n\"))))(Tile((id \
         ff8bd20d-5aa1-495e-8a38-bb4f9097c81e)(label(+))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 12))(sort Typ))((shape(Concave \
         12))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         f9252511-16b5-411b-a30e-ed8b551cf939)(content(Whitespace\" \
         \"))))(Tile((id \
         deee0714-3608-4698-9e46-7fa0562f7236)(label(ClearGrove))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         45436118-1e0f-4405-a855-1d15e2700465)(content(Whitespace\"\\n\"))))(Tile((id \
         6e45f47c-9e98-45c2-8d5a-829c5ec1b557)(label(+))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 12))(sort Typ))((shape(Concave \
         12))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         a28e8193-92f3-404c-84fb-e9e053e01589)(content(Whitespace\" \
         \"))))(Tile((id \
         d58ec132-e0ae-4663-84a4-84b6bb974b60)(label(PlantRow))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         6d6679f5-76d1-4233-8f77-29a70cc12330)(label(\"(\"\")\"))(mold((out \
         Typ)(in_(Typ))(nibs(((shape(Concave 11))(sort Typ))((shape \
         Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
         528fcc00-978e-4a06-847d-cb9e45b1bcdc)(label(Row))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children()))))))))(Secondary((id \
         5a2b1663-1e8b-46dd-9b24-0a6c80acc53a)(content(Whitespace\"\\n\"))))(Secondary((id \
         fbedb955-6428-4528-a9fd-bfbd1368f5c5)(content(Comment\"# TODO: Add \
         PlantCol(Col) here #\"))))(Secondary((id \
         80648eea-ee80-4e80-90b0-ce07baf14f36)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         28dee6fa-d760-4422-9236-3f952190318f)(content(Whitespace\"\\n\"))))(Secondary((id \
         2523e1ee-7cbe-4695-9250-454eba290ec0)(content(Whitespace\"\\n\"))))(Tile((id \
         8365ebbd-1a16-40dc-acf9-f07e55e0706f)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         1652c36d-6dbd-46ca-8ddb-5c8b1a05dd38)(content(Whitespace\" \
         \"))))(Tile((id \
         af92e5ed-98a4-4e74-b93d-bae3bede1fee)(label(init))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         e5ee7f7f-6f5a-454d-a780-8389c165b0ac)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 24))(sort Pat))((shape(Concave \
         24))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         df395183-63b1-430e-b0d0-c4ce94daf2f6)(content(Whitespace\" \
         \"))))(Tile((id \
         1fddbff9-e7b4-4739-905c-3f9eeb093cd0)(label(Model))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         8edd674b-9d51-4fbe-a42f-b2c90c49661c)(content(Whitespace\" \
         \")))))((Secondary((id \
         2d10e52c-5384-4f79-8458-68f9bf974641)(content(Whitespace\" \
         \"))))(Tile((id \
         0fcd42dc-91dd-44a0-9515-c51ba8b63d06)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         0fd63a93-d114-4195-b805-b229c5417722)(content(Whitespace\"\\n\"))))(Tile((id \
         21efe1ea-7ea1-41a6-aecb-3730a943bd98)(label(grove))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         f3fda18c-4fa1-42bc-a118-ee6946d75e11)(content(Whitespace\" \
         \"))))(Tile((id \
         56fe28a6-ad83-44bf-99af-da5dcd579243)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         156d8a57-3ad3-490a-8882-28e8d563d19e)(content(Whitespace\" \
         \"))))(Tile((id 19db3285-794e-4bbb-a393-1c4a7dde4886)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         b69178ee-180f-4e3e-a1dc-cd6dd4201958)(content(Whitespace\"\\n\"))))(Tile((id \
         d8326191-96f6-4e77-bc96-71395a5af234)(label([ ]))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         bb8dccb4-635d-4f95-ad8c-f760bf952633)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         4c8dfe54-2e90-4264-9fb3-b6a8437e24e8)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         4507a990-17c3-4f5a-9016-dd870b67d03a)(content(Whitespace\" \
         \"))))(Tile((id \
         08fbb593-5e43-49da-b2e8-ebdacf49114d)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         964ce0d3-3d21-4964-80e1-1673a6879014)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         c6db869d-8dc0-4be5-9fda-ebba071aa59d)(content(Whitespace\" \
         \"))))(Tile((id \
         595ca131-3669-4bb1-919e-93973b150274)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         2226834c-1409-417a-a7ba-a59203aaae67)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         b5495b67-5ad6-46c7-a306-fab2d40e2ee8)(content(Whitespace\"\\n\"))))(Tile((id \
         1a436e2d-a1c3-4b6d-a43b-8b820b069b05)(label([ ]))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         f9951b77-990b-40ff-987b-449bb91653f4)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         06eb3ebd-f98f-47b5-855f-7f7fd9b2e68d)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         9294ae13-52b8-4b88-a235-becd08f78d65)(content(Whitespace\" \
         \"))))(Tile((id \
         5fd95f7b-c55e-4620-bc3b-26777d104a6c)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         af6451d3-4a88-4cdb-98ad-f5c4f2ace1d1)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         ec945a7c-f858-4e39-8964-51153f13eb39)(content(Whitespace\" \
         \"))))(Tile((id \
         237ff5af-5dea-435b-b6b9-a7cf06bd7e68)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         130d92f8-305a-42eb-a77c-113601bb4cfa)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         34874919-ff43-4197-b0ee-e0b6c81264b6)(content(Whitespace\"\\n\"))))(Tile((id \
         caa91260-faf6-414b-9d59-c615f55d1b65)(label([ ]))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         e77341cd-5e85-49e8-af12-7824b3d8ab65)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         e00dd11d-aa18-4223-b215-6068b7cf6ad5)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         cffd8d10-3e59-452b-b75d-54b99add6c23)(content(Whitespace\" \
         \"))))(Tile((id \
         72b1e7f5-84c4-4076-8c2c-58d907c6806b)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         9985f2d1-8b10-46e6-94bf-7a6c71fbaead)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         a9b89b10-6bcf-4108-b827-a99a2a9dec51)(content(Whitespace\" \
         \"))))(Tile((id \
         7b6f8dd8-db0c-43c3-b66e-984bc0cfc83c)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         8d143bf2-aa7c-4493-8bfb-35760eb83c1f)(content(Whitespace\"\\n\")))))))))(Tile((id \
         308604d9-e27c-47b6-8640-cdaada5fe788)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         990c357d-e59e-4fcc-a25c-924a5935a24c)(content(Whitespace\"\\n\"))))(Tile((id \
         dfb1b22e-abfd-4773-a63a-ba19a694c772)(label(currentSeed))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         980db0d8-3d87-49f6-9166-b0a10aa6ab6d)(content(Whitespace\" \
         \"))))(Tile((id \
         a9679880-6212-4822-bfc4-2d2b5a2937ff)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         3b8896ab-9476-4985-aea1-fd815664ec0d)(content(Whitespace\" \
         \"))))(Tile((id \
         f17d16b2-dc88-4f58-bf53-9faa4daa7e64)(label(\"\\\"\\240\\159\\140\\177\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         5dacd601-752d-4494-96b6-33fc27a01548)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         573473ba-13a4-467d-a162-1aae84d1f0dc)(content(Whitespace\"\\n\"))))(Tile((id \
         4f9fc821-f9d9-48c8-a93a-600d917fc906)(label(seedInventory))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         a682d72f-d3ae-4f3e-a711-11e365fa43d1)(content(Whitespace\" \
         \"))))(Tile((id \
         76249673-aa4e-4983-bbd6-6e5bf58f1f06)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         736ddadd-3672-4c43-9b30-8085617ad138)(content(Whitespace\" \
         \"))))(Tile((id ca8ccf28-1915-40f7-8dda-08fbc453b001)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         8502351f-b36e-4474-b0f3-6998120d3e31)(label(\"\\\"\\240\\159\\140\\177\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         699e6d82-748f-44c1-8dc5-42adedcd7811)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         61e641cf-4f93-4880-a198-5eb10e9f7327)(content(Whitespace\" \
         \"))))(Tile((id \
         acce5a8a-c146-48d4-bd74-65724bd2a66a)(label(\"\\\"\\240\\159\\140\\191\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         252acb83-900d-4c5f-921e-1bd811729ce7)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         f31351ea-da6e-4faa-91fa-9eafd2c1ad34)(content(Whitespace\" \
         \"))))(Tile((id \
         16161063-1b3d-4f20-94fd-d9737f8a91cc)(label(\"\\\"\\240\\159\\141\\132\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         bbb5e838-ccd3-48d7-b8d7-4c56261afea7)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         1f1f614c-8319-43dc-9c31-1605fc4a707e)(content(Whitespace\" \
         \"))))(Tile((id \
         4088d5a1-4e4a-4b2e-9d55-8c404ed0a5b9)(label(\"\\\"\\226\\152\\152\\239\\184\\143\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         a83b053f-5869-4235-9fbb-078f92f018ea)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         0c8ff20d-e90b-42ca-8b4e-8a569e1ca134)(content(Whitespace\" \
         \"))))(Tile((id \
         d3743b84-251d-433b-9dfc-d537cf133457)(label(\"\\\"\\240\\159\\140\\184\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         67bb1d38-5c32-4b19-a84c-64896472390e)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         dc92cdc0-7a50-45fb-8f6b-6ae815b4d1a9)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         6f35d8bb-0aac-418b-b4df-21d5178159cf)(content(Whitespace\"\\n\"))))(Secondary((id \
         4afd61bc-8a6e-446d-8e88-8b0184e8ea04)(content(Whitespace\"\\n\"))))(Tile((id \
         20cf3bd1-f634-4c1f-8b96-165db43eba2d)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         3402697e-2015-4cf0-8679-2dba64f7e3a8)(content(Whitespace\" \
         \"))))(Tile((id \
         b36d25fe-a237-44d6-82c1-cc3027cd0534)(label(setCell))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         9c2adc83-d82e-4ed0-9022-21c8562bac33)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 24))(sort Pat))((shape(Concave \
         24))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         0c52d2e8-98c0-45c4-90dd-7481fc511d46)(content(Whitespace\" \
         \"))))(Tile((id \
         6889f5b5-b08e-425e-a2aa-eec9b8d8edec)(label(\"(\"\")\"))(mold((out \
         Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0 1))(children(((Tile((id \
         3d1ed63c-5d5e-4f23-8989-5a3be7f09e83)(label(Grove))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         40d2964d-ab31-459b-bcb4-566f03bfa79c)(label(,))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 44))(sort Typ))((shape(Concave \
         44))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         7b31d859-90b4-44ff-a92e-7b07f80725e8)(content(Whitespace\" \
         \"))))(Tile((id \
         d5b22f4c-e95c-4c63-923c-7fab4dbaaacc)(label(Row))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         b4453d84-d635-48c9-89b7-3b0fed394660)(label(,))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 44))(sort Typ))((shape(Concave \
         44))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         eac6f218-45d3-4453-b894-0a451b9fc747)(content(Whitespace\" \
         \"))))(Tile((id \
         da0471aa-0cc4-4638-8418-b410c8049a70)(label(Col))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         a7dc049b-4357-42ad-b205-25dc2911d71d)(label(,))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 44))(sort Typ))((shape(Concave \
         44))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         42f3c8df-7b63-4f3a-80a0-970fb33f8d70)(content(Whitespace\" \
         \"))))(Tile((id \
         61cf4ba7-8df4-4317-9244-23d11f95181f)(label(Plant))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children()))))))))(Secondary((id \
         eb4e2191-f19c-47b1-a952-586aff5b93df)(content(Whitespace\" \
         \"))))(Tile((id \
         1d0e3ba9-f01b-40e2-865d-a1273acd39dd)(label(->))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 13))(sort Typ))((shape(Concave \
         13))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         361f32e1-ed4e-4f25-8642-47e2484ecc8b)(content(Whitespace\" \
         \"))))(Tile((id \
         92a72d9f-12b8-44bf-83a1-4106d0773b4f)(label(Grove))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         d76e15d3-798b-4dc3-9aad-2fcceb3d7701)(content(Whitespace\" \
         \")))))((Secondary((id \
         e58691b4-6e4c-41b8-887c-28aaa42ea57d)(content(Whitespace\"\\n\"))))(Tile((id \
         cc048c0c-7de2-43d6-8707-13562535c8bd)(label(fun ->))(mold((out \
         Exp)(in_(Pat))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         37))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         cabf857d-3699-4ec2-b847-67f12e34d66f)(content(Whitespace\" \
         \"))))(Tile((id \
         aef071fe-ed42-425a-a28b-ad6962366349)(label(grove))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         fc105a00-ffd0-4d3a-aa60-eac0c5c14ab1)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 44))(sort Pat))((shape(Concave \
         44))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         336ed0d9-d790-44c5-98c6-1aaa042a243d)(content(Whitespace\" \
         \"))))(Tile((id \
         e27474d6-416e-4685-aab2-2680ac3c54de)(label(row))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         4164a4a9-636a-40b8-865f-f8a3fe94893e)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 44))(sort Pat))((shape(Concave \
         44))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         f65a7d65-b62c-421e-bed2-08973e32a25a)(content(Whitespace\" \
         \"))))(Tile((id \
         469be4f0-0426-4b9f-956b-ee78d41a4990)(label(col))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         b5c59a2a-907f-45d2-8248-e7000d0d5d39)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 44))(sort Pat))((shape(Concave \
         44))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         10a22425-0873-4bf3-a062-a31525927a08)(content(Whitespace\" \
         \"))))(Tile((id \
         6ffbae6b-e03e-49f6-b7fb-0244a3346850)(label(plant))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         e9ba150c-7243-4ec9-b806-31f98aada4ee)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         2e46c3ef-9f58-4dff-89fc-827c19d8aa23)(content(Whitespace\"\\n\"))))(Tile((id \
         95caeae4-06b0-4397-b994-ca5e5f6c1569)(label(mapi))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         01a263b9-29f4-494d-a995-08b120e47b30)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         d7777672-5eab-4216-8bff-22c8fbd554c3)(label(grove))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         82c70141-b873-402d-8922-e362b1e6d28b)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         eca829eb-3264-4d5a-a8ee-276396a2e58e)(content(Whitespace\" \
         \"))))(Tile((id 7f0cb63a-92eb-4acc-9217-9e7734fa037b)(label(fun \
         ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 37))(sort Exp))))))(shards(0 \
         1))(children(((Secondary((id \
         bec79708-8edc-4bb4-97ba-825d03f46cb4)(content(Whitespace\" \
         \"))))(Tile((id \
         d553f40b-eab6-4f32-bc39-7a8c3b2a497e)(label(\"(\"\")\"))(mold((out \
         Pat)(in_(Pat))(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0 1))(children(((Tile((id \
         3abacaab-cd09-40d5-acbd-a9fdfeca5f03)(label(i))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         b7836d86-babf-4fbe-a75c-1929d03db086)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 44))(sort Pat))((shape(Concave \
         44))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         0726b5b1-d7a9-429e-aee3-84ccc17ec40f)(content(Whitespace\" \
         \"))))(Tile((id \
         9f7995c1-6e8d-4065-beec-c1923dc3e026)(label(r))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children()))))))))(Secondary((id \
         b3ae9903-d51b-42a9-9411-450529054680)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         b063b6ad-1207-4db8-ac71-538a11c84563)(content(Whitespace\"\\n\"))))(Tile((id \
         7101d903-8621-4a77-9657-71a7513a6a52)(label(if then else))(mold((out \
         Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         36))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         ddab8e72-9f55-4fb2-8799-15f016a40f67)(content(Whitespace\" \
         \"))))(Tile((id \
         cd92357f-f6e0-44e1-93d4-4d291a37992f)(label(i))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         17e7bb3e-d53f-452c-94d8-c172725e64cd)(content(Whitespace\" \
         \"))))(Tile((id \
         994f7073-1f2c-43e0-a4d5-fd04cff0ef7e)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         e1645b24-522e-457a-8006-9084cc27e084)(content(Whitespace\" \
         \"))))(Tile((id \
         49126a0a-3e9a-4aea-971d-18c67d3f5891)(label(row))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         be7c0a9f-5c0f-4f27-9d5e-82069908bdff)(content(Whitespace\"\\n\")))))((Secondary((id \
         fa609071-c0a8-4d4f-988f-5f3efdd8979f)(content(Whitespace\" \
         \"))))(Tile((id \
         9c2211d7-3e29-4695-8da7-71ba3f1e34b2)(label(mapi))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         f7579687-87eb-437a-b5c9-1b89f87a1ec9)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         8956580a-c575-4120-a108-c18991f5b826)(label(r))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         6cd3d4b4-6067-4c4b-8b37-b3526d036da8)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         e5507aec-96b4-4e21-a795-ae0eef4efacc)(content(Whitespace\" \
         \"))))(Tile((id 18085aa0-ac44-4649-9bb0-da67b1a1cf70)(label(fun \
         ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 37))(sort Exp))))))(shards(0 \
         1))(children(((Secondary((id \
         4316b3c6-c830-4c56-a1c3-524be02ef949)(content(Whitespace\" \
         \"))))(Tile((id \
         7c3cc842-bed4-4a10-9946-c514e77abf77)(label(\"(\"\")\"))(mold((out \
         Pat)(in_(Pat))(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0 1))(children(((Tile((id \
         1bf70be2-70b6-4589-8921-898d7a486a16)(label(j))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         afa3df5e-de38-450e-b1db-7bc2a0b419ee)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 44))(sort Pat))((shape(Concave \
         44))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         8ab910eb-71d6-4b60-b309-d3843deb6589)(content(Whitespace\" \
         \"))))(Tile((id \
         c9554c64-6287-458c-ba69-4d4d692f4691)(label(c))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children()))))))))(Secondary((id \
         e6796967-09a8-47b0-9aa5-e6a5fdbbbef5)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         62234987-3fb9-4e6f-8005-268b8f00f0cd)(content(Whitespace\" \
         \"))))(Tile((id ff9220a8-decc-4cb2-bbc0-2a803f36a834)(label(if then \
         else))(mold((out Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 36))(sort Exp))))))(shards(0 1 \
         2))(children(((Secondary((id \
         d0f10a98-7e41-4074-a889-164671b8a3ba)(content(Whitespace\" \
         \"))))(Tile((id \
         3f7a44e0-041e-4b1b-a13f-7ee7d7ac2670)(label(j))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         467812a4-8cba-48f7-bb12-5a2816fa3998)(content(Whitespace\" \
         \"))))(Tile((id \
         57b7e053-abb4-4d27-bfcc-039c02012d04)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         9bfeb489-c79c-4ed1-8231-2d491527ac8a)(content(Whitespace\" \
         \"))))(Tile((id \
         8eeebc96-d0e1-405a-a708-4a57e5da24d4)(label(col))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         245c3ebd-9f84-43d5-ae63-c36451925d65)(content(Whitespace\" \
         \")))))((Secondary((id \
         b585a2a4-9865-4304-a622-6a621af6fd84)(content(Whitespace\" \
         \"))))(Tile((id \
         31ccf8c8-84f2-411f-b1cf-d25ed11051a9)(label(plant))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         29f92910-b73d-4fd1-8910-4ad3ca6d760c)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         0ee2c0ad-1720-40db-96e5-2cb4bc540aff)(content(Whitespace\" \
         \"))))(Tile((id \
         eea83138-3e48-4829-8d33-8f60d5c40b88)(label(c))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         f7ab0500-d53a-4b36-a41a-9f6f30a2fb06)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         010e48f3-8569-4854-9863-75b34b081b5e)(content(Whitespace\" \
         \"))))(Tile((id \
         7af79f86-f1c6-4fe6-9df1-56966ad9798b)(label(r))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         0469ce95-137b-4ceb-8055-fa3b7a1afd30)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         c492f228-802c-4fd4-9614-a94ecba7b33c)(content(Whitespace\"\\n\"))))(Secondary((id \
         56c33f72-f090-4240-865e-9e445116a583)(content(Whitespace\"\\n\"))))(Tile((id \
         53e01e75-9dfc-4330-9528-7218fc1e661b)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         6e7de5ec-bf5c-48e7-9aa8-4cd355b76c31)(content(Whitespace\" \
         \"))))(Tile((id \
         9b7e93dc-16bf-461c-b518-782bb7c5f019)(label(setRow))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         4d369e8e-db4d-4147-8997-8b7c8df59cbb)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 24))(sort Pat))((shape(Concave \
         24))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         b281c94c-1a0f-40af-b0d9-d39d63a3132e)(content(Whitespace\" \
         \"))))(Tile((id \
         83879737-ce54-439c-95b4-7184957a0070)(label(\"(\"\")\"))(mold((out \
         Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0 1))(children(((Tile((id \
         5b0b8e7d-e7c9-43d2-b93c-c60d8b7cbcef)(label(Grove))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         016a13ce-8145-44cc-af99-8712138d5697)(label(,))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 44))(sort Typ))((shape(Concave \
         44))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         9d7f8c71-1e4e-4595-98bf-4c4054d4feea)(content(Whitespace\" \
         \"))))(Tile((id \
         acb014f8-192e-45f0-97cd-891b4fa6973c)(label(Row))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         c5e5d495-8c6d-4dc3-a9e0-88ea76aae805)(label(,))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 44))(sort Typ))((shape(Concave \
         44))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         a1c8a4de-2be6-47c6-b2a0-4647b02b4db8)(content(Whitespace\" \
         \"))))(Tile((id \
         5a5f4fda-c4a5-49df-a0a2-38952a5287cc)(label(Plant))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children()))))))))(Secondary((id \
         c14f2bbe-e26a-439b-baaf-813b1ecb90d7)(content(Whitespace\" \
         \"))))(Tile((id \
         66fbeeb9-e3b6-47f7-a782-4538309ffc15)(label(->))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 13))(sort Typ))((shape(Concave \
         13))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         d1361713-c67d-4258-8f27-53cca1ef54f6)(content(Whitespace\" \
         \"))))(Tile((id \
         8cc9c88c-4e75-49df-97a0-5ac8ebe807b0)(label(Grove))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         2214eb77-b3c7-46b9-a79b-f739424dfde2)(content(Whitespace\" \
         \")))))((Secondary((id \
         205be9d0-cf15-4efa-b2b1-dc4fc1845dd7)(content(Whitespace\"\\n\"))))(Tile((id \
         df96a1de-a8a2-4eb9-b925-af7206c8f9e1)(label(fun ->))(mold((out \
         Exp)(in_(Pat))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         37))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         ea028cae-4cd3-4c8d-a147-93012e5307e8)(content(Whitespace\" \
         \"))))(Tile((id \
         fe20384d-4463-458b-ac50-71e537407081)(label(grove))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         91867581-eb08-4af3-990e-537e40f201a9)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 44))(sort Pat))((shape(Concave \
         44))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         040b5950-faca-4382-bd53-215bbc4f3886)(content(Whitespace\" \
         \"))))(Tile((id \
         49de482a-3c59-4403-b678-d74d261b33bf)(label(targetRow))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         2368af54-856e-4dc1-92e0-815344bbb036)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 44))(sort Pat))((shape(Concave \
         44))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         9a6c20d6-1ee8-4612-98c9-ee83c1c5d836)(content(Whitespace\" \
         \"))))(Tile((id \
         c0353aa2-6ea6-43ff-b656-deb62673702f)(label(plant))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         3217fd20-dd3f-46c1-bf98-d71eacdf5a3e)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         cb19c40a-b303-4778-a4e3-65fec91801c5)(content(Whitespace\"\\n\"))))(Tile((id \
         5a98c86e-708c-4c4c-a0ec-d095a424dc43)(label(mapi))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         31140c0b-9a94-4789-88e5-60cdc19042ac)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         cffb3d9a-92ec-4c76-9c1d-a82af15d41b6)(label(grove))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         390791a7-a5a2-411e-be9e-e076a49ee94d)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         ceb09e20-bbd5-4b85-9478-1fbafcecf1e1)(content(Whitespace\" \
         \"))))(Tile((id 7efbf438-e195-4a61-ae74-c099b3a4d6a2)(label(fun \
         ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 37))(sort Exp))))))(shards(0 \
         1))(children(((Secondary((id \
         6a5d198a-7972-412b-9b86-95f38a2e2473)(content(Whitespace\" \
         \"))))(Tile((id \
         5700263f-4bd8-4c53-b5a9-7ea5e0b335b8)(label(\"(\"\")\"))(mold((out \
         Pat)(in_(Pat))(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0 1))(children(((Tile((id \
         90e433bf-bc05-4fb2-a051-1e35f52b9845)(label(i))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         2a5357c5-14b9-4aec-9c83-ad46960334ab)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 44))(sort Pat))((shape(Concave \
         44))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         df369f92-7466-4ba6-b2a9-08d5a6b0d94b)(content(Whitespace\" \
         \"))))(Tile((id \
         9e109a03-847f-4167-a6b4-67ad98d6b4b1)(label(row))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children()))))))))(Secondary((id \
         18362737-2ba5-4b5c-8dda-01631808f808)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         0d1f8b60-bc01-41b7-9e1c-5dc63e47dd22)(content(Whitespace\"\\n\"))))(Tile((id \
         fca04598-037d-4e0c-abb3-d713f4175b8e)(label(if then else))(mold((out \
         Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         36))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         578cf485-bfcb-419b-99e9-88ce49d37a53)(content(Whitespace\" \
         \"))))(Tile((id \
         7eaba822-5d0e-492d-9f6b-0475b6bb14a0)(label(i))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         16bdeef6-42d2-465a-89d4-1049f79c4992)(content(Whitespace\" \
         \"))))(Tile((id \
         07e3bf4c-0cdb-4346-aa93-abd23160935b)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         61950fe7-add5-41c9-8b76-44f9d09a54af)(content(Whitespace\" \
         \"))))(Tile((id \
         222bb0c5-4622-4620-9913-51bfcfa3a2ec)(label(targetRow))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         4b13f710-5073-4ff5-8668-a10168eb300f)(content(Whitespace\"\\n\")))))((Secondary((id \
         b53889bf-aae6-4e34-9726-8b74bdd99879)(content(Whitespace\" \
         \"))))(Tile((id \
         70294bf7-76d8-4b0c-8e13-c26459d984f8)(label(map))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         c45e7d17-f375-4630-8409-2e745de9b954)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         760a6f80-16ca-45a1-ab09-a6688bd8aa45)(label(row))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         e99c7582-fa5f-4afb-87c1-9996b3e7ada6)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         8c41c95e-7690-4996-8a5a-838f8e261c36)(content(Whitespace\" \
         \"))))(Tile((id 583326c6-0c28-4f73-9a5c-41bc7a443101)(label(fun \
         ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 37))(sort Exp))))))(shards(0 \
         1))(children(((Secondary((id \
         2230cbd8-0150-46bd-a166-568474ef1c27)(content(Whitespace\" \
         \"))))(Tile((id \
         8dd3bb5f-f674-44a2-b79e-a924d49ae03e)(label(_))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         89ae6883-15d1-4676-ade0-a44903fb24f4)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         0d1fed16-64ce-4c61-a6fc-21552b465d75)(content(Whitespace\" \
         \"))))(Tile((id \
         9937ed9b-ca35-48c1-8b5d-84bc7ac03517)(label(plant))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         0238f5e9-b74d-47b5-97fc-11fc69ceb664)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         057fa44c-3294-4fb3-8dae-b3211a87399c)(content(Whitespace\" \
         \"))))(Tile((id \
         4a9827e1-d62b-4f0b-be15-f4f12ebacca3)(label(row))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         9d8a5fe7-e057-433c-9495-b4d7d7778215)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         18556991-33cc-46fe-92c9-b0a837f8f6d4)(content(Whitespace\"\\n\"))))(Secondary((id \
         43a5efa2-8a21-4179-8746-9474eeda9d86)(content(Whitespace\"\\n\"))))(Secondary((id \
         5d610c11-ded8-4556-9f8d-d8e2a39391c0)(content(Comment\"# TODO: Add \
         setCol helper here #\"))))(Secondary((id \
         d3568a25-a84c-4fad-a178-2cfc91ad231b)(content(Whitespace\"\\n\"))))(Secondary((id \
         02c2cddf-cccf-4cd6-a708-c717c6a8f2c0)(content(Comment\"# Hint: You \
         need to modify each row, changing #\"))))(Secondary((id \
         64648647-15f7-474f-8dcb-0ef673d5c78b)(content(Whitespace\"\\n\"))))(Secondary((id \
         a6cd76ae-3781-47de-8680-969b48ad085a)(content(Comment\"# only the \
         cell at the target column.         #\"))))(Secondary((id \
         dbaf13db-aa0e-48c3-86c3-235aca4c39a9)(content(Whitespace\"\\n\"))))(Secondary((id \
         a7a34e1c-4078-4617-84aa-eab8ebb92c44)(content(Whitespace\"\\n\"))))(Tile((id \
         cbd16827-3773-4272-861a-9395f3bc8162)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         a6fcab50-2eb2-433d-861f-a545e0699cd5)(content(Whitespace\" \
         \"))))(Tile((id \
         089c80a2-4571-40fa-9fe6-713a9b87c2c7)(label(setAll))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         017e4aa5-d42a-49ba-b1a5-394b076debdb)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 24))(sort Pat))((shape(Concave \
         24))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         63758c7b-ea66-4361-a4f8-942e30ec2aab)(content(Whitespace\" \
         \"))))(Tile((id \
         8ecb1f99-a1b6-4832-88a7-235d167df79d)(label(\"(\"\")\"))(mold((out \
         Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0 1))(children(((Tile((id \
         23681c67-04c2-4d6b-81a3-330cdc152327)(label(Grove))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         f4e2e4b8-3407-4626-ad5d-5f1fc998294e)(label(,))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 44))(sort Typ))((shape(Concave \
         44))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         c09f3303-6b66-4731-8016-f3f77b118e97)(content(Whitespace\" \
         \"))))(Tile((id \
         fced898a-c68b-4995-8c9d-ab6fa11e8c55)(label(Plant))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children()))))))))(Secondary((id \
         0d45266e-535b-4495-8c83-f545482f126d)(content(Whitespace\" \
         \"))))(Tile((id \
         b7a51459-ff1a-429f-a2ce-96a2fc9eb79b)(label(->))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 13))(sort Typ))((shape(Concave \
         13))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         417af21d-ed7e-4935-b445-be75b00b2dff)(content(Whitespace\" \
         \"))))(Tile((id \
         bb333caf-ec2c-46d5-90b8-7645d8723ac3)(label(Grove))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         108f7f46-6f79-45f7-857e-95196e83e392)(content(Whitespace\" \
         \")))))((Secondary((id \
         f7bc2cb2-254b-482f-ad4a-473c2f594a2d)(content(Whitespace\"\\n\"))))(Tile((id \
         f6a953a3-a6ba-4e1b-9aca-a3e0c1b85551)(label(fun ->))(mold((out \
         Exp)(in_(Pat))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         37))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         b6e94b23-9000-461f-b61e-27a5dd555647)(content(Whitespace\" \
         \"))))(Tile((id \
         fb961191-afab-4057-9781-ffd72de25147)(label(\"(\"\")\"))(mold((out \
         Pat)(in_(Pat))(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0 1))(children(((Tile((id \
         768a09e9-ee3c-4bcd-8b1d-8212cde90e5c)(label(grove))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         7499f72d-42ac-474e-9547-2f63fb68ab3f)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 44))(sort Pat))((shape(Concave \
         44))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         436fb1db-1250-4e0f-b727-5d52054bad84)(content(Whitespace\" \
         \"))))(Tile((id \
         bf71e5d1-e310-410d-aaa6-aebed17ab9c2)(label(plant))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children()))))))))(Secondary((id \
         1bb45345-900e-43e1-88d6-078b19ebacb9)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         b2b14e2e-162b-4203-a94a-bcb3ec3a7c98)(content(Whitespace\"\\n\"))))(Tile((id \
         e16af643-a18e-4264-b98b-a056e345a6d4)(label(map))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         db2cc21f-15cc-4a92-839f-22d140cc8585)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         cb7863f1-1b90-4c0f-aedf-2277d23294cf)(label(grove))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         866787c3-78f5-4b75-9bc4-95e491c5eb20)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         b296de66-b96a-46d2-9d9a-5da2d0bb6128)(content(Whitespace\" \
         \"))))(Tile((id 27d1241d-54b8-4f81-adbb-b3703ce7e93d)(label(fun \
         ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 37))(sort Exp))))))(shards(0 \
         1))(children(((Secondary((id \
         46241d31-db97-4644-ac5e-80b161ea5539)(content(Whitespace\" \
         \"))))(Tile((id \
         76e81dd7-bb93-4262-8a66-fe42d15016f8)(label(row))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         7cad0bf8-8aa2-48af-a69d-9ee3ce2da9a3)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         a06a1bea-9774-406b-864d-4f0e30f747f3)(content(Whitespace\" \
         \"))))(Tile((id \
         544d3db3-7f2a-4c36-bbc4-c6abbe697c2f)(label(map))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         17741492-76a1-4ea3-8bdc-7c66ca4dc64f)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         5b500103-c7af-4319-b423-e6503e378057)(label(row))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         37427c11-0432-4100-88fb-116488427d8e)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         62e0079a-121e-4b6c-960f-0c5257564110)(content(Whitespace\" \
         \"))))(Tile((id 800ebfab-2106-44e8-b44b-b55901cf5487)(label(fun \
         ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 37))(sort Exp))))))(shards(0 \
         1))(children(((Secondary((id \
         50aa6659-befc-4887-9a62-020fef4464b4)(content(Whitespace\" \
         \"))))(Tile((id \
         e1123d8c-35a5-4691-9eac-6e140bf2270a)(label(_))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         6ec37706-9f34-442f-8baa-f47f7a6a5e11)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         13bf2b81-7a78-45c1-ad00-f52aa673c9a9)(content(Whitespace\" \
         \"))))(Tile((id \
         8fa79050-87c0-49b8-951a-1eb093c01dfd)(label(plant))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))))))))))))(Secondary((id \
         35a06456-8700-4aaf-923f-43300a5f21b4)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         436112a9-c4f0-458d-8c39-4ec91a926831)(content(Whitespace\"\\n\"))))(Secondary((id \
         2af4eb59-e4b5-4c59-98e3-91867bd0dc89)(content(Whitespace\"\\n\"))))(Tile((id \
         84418ac5-7fc0-43a2-aa48-06a1e0a746f2)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         4011e2ad-6e53-446c-91ff-f749b73cf38b)(content(Whitespace\" \
         \"))))(Tile((id \
         a416a77b-a280-47d6-86fd-f196492bc86c)(label(updateGrove))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         cb9b91c4-f0fc-418c-81cd-b57c3e72b18f)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 24))(sort Pat))((shape(Concave \
         24))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         327ae571-d931-4995-bc87-262babf85d28)(content(Whitespace\" \
         \"))))(Tile((id \
         49e8f8bb-91ba-4a96-899b-8d52cf553eb8)(label(\"(\"\")\"))(mold((out \
         Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0 1))(children(((Tile((id \
         10955a3a-8cfd-4d63-9760-5b4731401f7a)(label(Model))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         7a6e5248-a10b-4315-835d-8a757de8eab7)(label(,))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 44))(sort Typ))((shape(Concave \
         44))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         25c81406-b93f-400a-b6fe-74f2ff78a479)(content(Whitespace\" \
         \"))))(Tile((id \
         e6137e7c-038e-4d9e-9e3d-3b29d9e6386e)(label(Grove))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         a8b21a38-01d3-4649-92be-9df025a2533a)(content(Whitespace\" \
         \"))))(Tile((id \
         f3194197-d732-49ef-b97d-da0a0c3d660b)(label(->))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 13))(sort Typ))((shape(Concave \
         13))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         de66a282-dad9-469d-8b67-79334764e056)(content(Whitespace\" \
         \"))))(Tile((id \
         4cbc1567-eb12-44e2-987f-f7991500762d)(label(Grove))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children()))))))))(Secondary((id \
         b88e3b59-28e3-4b6b-8cfe-464afa301211)(content(Whitespace\" \
         \"))))(Tile((id \
         2d1c8d79-ed2c-420d-a847-29e4518fdf3f)(label(->))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 13))(sort Typ))((shape(Concave \
         13))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         7884c90a-6296-45f7-ab91-b3888dd2f23e)(content(Whitespace\" \
         \"))))(Tile((id \
         f7134a97-bf2b-4cc7-a85d-a27d03e5ba42)(label(Model))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         6bbd419a-269b-419f-b40b-e14da2dbe8f8)(content(Whitespace\" \
         \")))))((Secondary((id \
         6beecca5-d228-4b66-abbc-cbe2714007a1)(content(Whitespace\"\\n\"))))(Tile((id \
         6fe44a58-e0a2-4f08-b591-5f0cedf3b4e5)(label(fun ->))(mold((out \
         Exp)(in_(Pat))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         37))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         d5397bff-d576-459b-bdd4-f84ae056fa26)(content(Whitespace\" \
         \"))))(Tile((id \
         2e83a63a-d016-43ba-b54c-70f055108571)(label(\"(\"\")\"))(mold((out \
         Pat)(in_(Pat))(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0 1))(children(((Tile((id \
         066af19c-f63d-4ebe-91ae-6170a68a816c)(label(m))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         a8cd87dd-c7c5-4d6e-91bb-c4387f1a53ff)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 44))(sort Pat))((shape(Concave \
         44))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         9a33f23e-a68e-4a0c-bba4-b3511c18fe6b)(content(Whitespace\" \
         \"))))(Tile((id \
         eb230b43-bcb3-40e9-bef1-59e3b4787db2)(label(f))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children()))))))))(Secondary((id \
         c10a2ec7-fcb9-49a9-ba75-b618def8c98d)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         6509dbe8-cc0d-4ab4-92c2-bdefbca52238)(content(Whitespace\" \
         \"))))(Tile((id \
         487aaa03-c6b4-443f-81b3-f14e6a6d276f)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         e5b90bad-5e99-424f-b557-90b6f03e2bcc)(label(f))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         1ca3b2f5-ba66-45cf-9e85-22c8496d6829)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         7a401ba3-e389-4876-b0f3-4f39bf1ba2ba)(label(m))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         468eaab0-1dc6-4b02-a674-4c7913ef4093)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         0f44ea0e-0476-4349-b674-f44f1ed26ec8)(label(grove))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         a97bbf3c-50ba-4aca-9d00-889b9a80987f)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         bfb8fc39-2019-4f77-ba58-eb3e125de017)(content(Whitespace\" \
         \"))))(Tile((id \
         a807c5a4-b9a0-4af2-8f67-459e39ada57d)(label(m))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         a029d2f4-ea85-44e1-99e5-545836f43afd)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         56379e00-0c6e-4d78-a577-b0f9c46e105c)(label(currentSeed))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         450c39e5-c3a3-4dcb-813d-025eb75a63a7)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         60d3970e-3b6b-481f-8be9-260795dbdd92)(content(Whitespace\" \
         \"))))(Tile((id \
         272ab73a-a2d1-4e34-a8c6-881d3ed398c3)(label(m))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         22ed4509-8a2d-43c9-abb6-fa5a87e19606)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         195f3ef0-28de-4b77-8b17-61c20c0c97a6)(label(seedInventory))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         a295cc9a-59d4-4873-abc4-31e30b33ef34)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         3357d92e-1548-4e30-992f-50c0d22367cb)(content(Whitespace\"\\n\"))))(Secondary((id \
         60ae406c-cd83-4449-8acf-2a3bb23bf617)(content(Whitespace\"\\n\"))))(Tile((id \
         9d3bb2e1-9c54-405a-89e2-38c68681b42e)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         4ef52611-7a97-4fb3-bba3-0ae6a1d75673)(content(Whitespace\" \
         \"))))(Tile((id \
         1c26363c-126f-401a-b319-31a1ceb67236)(label(update))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         9eb6a7e6-2e6f-4575-bf10-e9f0d0c54684)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 24))(sort Pat))((shape(Concave \
         24))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         6d5a2c89-2181-4baf-8d8c-d0d101b9d8c0)(content(Whitespace\" \
         \"))))(Tile((id \
         6fd68536-8b75-4d1a-850b-2e47ec740a38)(label(\"(\"\")\"))(mold((out \
         Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0 1))(children(((Tile((id \
         c018be05-44d1-4dc2-a81d-dbf83567ad4b)(label(Model))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         948b6e02-d72c-45e2-87e3-24cfcfa3f00f)(label(,))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 44))(sort Typ))((shape(Concave \
         44))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         c62199a6-4731-4610-adf1-45a60df8cbbe)(content(Whitespace\" \
         \"))))(Tile((id \
         8ee0f8a0-2c67-4d9f-9acc-51227596d544)(label(Action))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children()))))))))(Secondary((id \
         c1963861-677e-4779-9e3a-7985cc9be213)(content(Whitespace\" \
         \"))))(Tile((id \
         15547b22-a5a2-46b9-8de2-5aa3e8b581d3)(label(->))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 13))(sort Typ))((shape(Concave \
         13))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         32d75a99-e695-443e-8484-93656a16e753)(content(Whitespace\" \
         \"))))(Tile((id \
         595ad9f7-1b13-4750-8ec7-91205cb37a8e)(label(Model))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         c8beb486-315a-400b-aa94-b29e58121308)(content(Whitespace\" \
         \")))))((Secondary((id \
         be77de5d-984c-4aaa-8647-0e2cd9ef89c2)(content(Whitespace\"\\n\"))))(Tile((id \
         b6fdee1c-ab3d-495c-9fa0-1384869fed80)(label(fun ->))(mold((out \
         Exp)(in_(Pat))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         37))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         a47ca0f5-87d3-4d77-8f05-f9e584a5ec0f)(content(Whitespace\" \
         \"))))(Tile((id \
         9b5ff02b-3a13-4603-805e-24d8c29597cf)(label(m))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         d3c237c6-6bb7-41df-8c4e-c188791d140c)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 44))(sort Pat))((shape(Concave \
         44))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         c2f9c532-4a57-4414-8990-55384c364705)(content(Whitespace\" \
         \"))))(Tile((id \
         a09da835-5d45-44e3-8d5c-30b08f0cc4c4)(label(action))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         9b60884f-abc4-480d-ab0b-0d6dcb6a1424)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         6e339f33-cc40-45fd-9280-add4892ced51)(content(Whitespace\"\\n\"))))(Tile((id \
         e6de7b2c-5b20-4a23-9a93-a82187b2e0dc)(label(case end))(mold((out \
         Exp)(in_(Rul))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         7ba5af01-bd4a-4457-86ff-7c5c8848f987)(content(Whitespace\" \
         \"))))(Tile((id \
         ba5785b4-0c61-4e63-81cd-51a71d1001d6)(label(action))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         1f15f786-100a-4606-b776-061e0cc7833b)(content(Whitespace\"\\n\"))))(Tile((id \
         46ceb668-c69b-4503-a559-a34c8e472943)(label(| =>))(mold((out \
         Rul)(in_(Pat))(nibs(((shape(Concave 46))(sort Exp))((shape(Concave \
         46))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         b80d2a48-1fc6-46b9-a0a2-ae42f94f891e)(content(Whitespace\" \
         \"))))(Tile((id \
         0fee5e34-e725-4b2b-968b-6ebec4af0b48)(label(SelectSeed))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         3a724607-c943-4d7c-a291-c272e545e634)(label(\"(\"\")\"))(mold((out \
         Pat)(in_(Pat))(nibs(((shape(Concave 23))(sort Pat))((shape \
         Convex)(sort Pat))))))(shards(0 1))(children(((Tile((id \
         7bf0f8b5-ab71-4817-b257-2b88f9fdf52d)(label(idx))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children()))))))))(Secondary((id \
         847bee9b-352b-4108-ac4d-dfb6990e49b1)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         64cc4a5b-3929-4243-9f26-e287ef92a61f)(content(Whitespace\"\\n\"))))(Tile((id \
         9ef34860-a0fa-44f7-90c6-cb0385a51f85)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         f65f9f1f-7fe1-460c-9662-fb66e0549b02)(label(m))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         01793b1c-d2ed-4a88-aa0e-dfb437ff7d79)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         15620c9c-0f07-45e6-8ea1-a47fd35b6494)(label(grove))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         1fb902b7-9e71-425d-bdb8-02e907bd23d6)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         83bc77d0-563e-4bc2-a11f-53112a4e2ca6)(content(Whitespace\" \
         \"))))(Tile((id \
         89952caf-802a-4a91-8b8f-d67ab0469ec1)(label(nth))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         d061809b-31e8-41cd-ad16-4a51bad3ec9d)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         eb410cd3-40e1-4c33-a627-7669f4ef5405)(label(m))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         6396edda-9aba-4018-bd99-7b4850ece1af)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         a1ddb374-0f18-4322-aab0-5bd75d6fb164)(label(seedInventory))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         b42f71fe-9583-4ac2-bcef-5d78b1e5d011)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         1acde8c5-82e6-41c2-a227-39589c657453)(content(Whitespace\" \
         \"))))(Tile((id \
         3ff91e39-2287-45cc-aec8-29286f38fac1)(label(idx))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         3a6a804b-8adb-46eb-aa42-53f5029375fa)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         59894589-d9d8-4675-950f-2966d7adb162)(content(Whitespace\" \
         \"))))(Tile((id \
         4a9abce5-aad9-45d4-9095-e19a8c109bc6)(label(m))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         c27410fb-1f48-4eb0-990d-d9fa084595d8)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         33f6a578-9d65-4bc4-86bd-78b6beb64b24)(label(seedInventory))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         605f814e-68ef-4fa1-81d5-aa6b9320cd43)(content(Whitespace\"\\n\"))))(Tile((id \
         7005c8e7-21ba-4fcc-aaf2-cbffad7a457f)(label(| =>))(mold((out \
         Rul)(in_(Pat))(nibs(((shape(Concave 46))(sort Exp))((shape(Concave \
         46))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         55eaf134-a512-4147-adde-c5e52ea851f8)(content(Whitespace\" \
         \"))))(Tile((id \
         2626b1db-c65f-4f5b-8ece-1e1cf9e3eb88)(label(PlantSeed))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         5f8ddffa-3d69-450c-a9e4-bfd53f61bc1d)(label(\"(\"\")\"))(mold((out \
         Pat)(in_(Pat))(nibs(((shape(Concave 23))(sort Pat))((shape \
         Convex)(sort Pat))))))(shards(0 1))(children(((Tile((id \
         962cac66-63af-4b3b-bb73-22c26df17d6e)(label(row))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         fc5d7a5c-d8c3-4ada-853a-e3408eda0516)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 44))(sort Pat))((shape(Concave \
         44))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         1cc97c20-9653-46ee-83fa-b70c5ecdda41)(content(Whitespace\" \
         \"))))(Tile((id \
         d4141eb8-b00a-4dd8-9741-50487579e8c2)(label(col))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children()))))))))(Secondary((id \
         a4de11e7-fddd-4582-b722-e7ab68270329)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         8560fa8a-5292-458e-a030-6077abe376c1)(content(Whitespace\"\\n\"))))(Tile((id \
         34d01981-00c0-49d6-b660-15375bfc2557)(label(updateGrove))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         dae69b77-990e-44e1-9453-b4653b138241)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         0b51ab8a-7b5c-4fa3-8709-afb294c9c39d)(label(m))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         9f9a9d53-479a-4f5a-b992-09ca274e286b)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         1482b5de-2547-4096-9402-d1f13e0d83a9)(content(Whitespace\" \
         \"))))(Tile((id 4c5b8bd3-6b67-4ecd-83df-269c7fa9331d)(label(fun \
         ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 37))(sort Exp))))))(shards(0 \
         1))(children(((Secondary((id \
         fc516a8c-416c-4c58-8d24-b05555faf913)(content(Whitespace\" \
         \"))))(Tile((id \
         0411eaef-ee2b-4b26-bbd2-11777b96c0d6)(label(g))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         2852fe73-d153-4b99-a3fe-e655efe375db)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         41ca122b-8666-4168-8851-d369d2aa1ec9)(content(Whitespace\" \
         \"))))(Tile((id \
         e61886a9-201c-40e5-a67a-56bab2315801)(label(setCell))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         fdb06362-8e98-4ce4-8c75-615f92e50291)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         6aa1ab65-2cc6-4ce8-9049-4bebb77e0a69)(label(g))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         6523ddec-2c4a-4369-be52-096b17a31f31)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         7dd45591-d2bb-4d49-ba22-0866d0df79cc)(content(Whitespace\" \
         \"))))(Tile((id \
         629fb30a-2cca-4fb3-93b7-01fb8559e4c6)(label(row))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         d794db31-d43a-45a2-a442-d89aeb9671a0)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         d2e71287-d2a2-4918-859b-5f14cdc669dc)(content(Whitespace\" \
         \"))))(Tile((id \
         3b309168-9c26-4f4a-bb38-3762461dff29)(label(col))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         8a8fc4f9-242f-4620-9392-569f3d12a878)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         4d2a764e-53d1-42a1-b3a6-b2f6e9c58424)(content(Whitespace\" \
         \"))))(Tile((id \
         58327d1d-2b87-41e3-aba2-577692e2c073)(label(m))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         1a4e2d3c-f83c-4c7f-acad-1bb7ed8995c4)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         2bc9ce1e-bf33-44a7-8de9-91e4c325cd2b)(label(currentSeed))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))))))))))))(Secondary((id \
         cd79a8e8-00a5-4b10-a3c1-a8ab0163c081)(content(Whitespace\"\\n\"))))(Tile((id \
         afadb0b0-0f44-4870-a9a8-624324c3f44a)(label(| =>))(mold((out \
         Rul)(in_(Pat))(nibs(((shape(Concave 46))(sort Exp))((shape(Concave \
         46))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         90583ab9-e0e2-4386-a57a-74ca70e31e3c)(content(Whitespace\" \
         \"))))(Tile((id \
         8fe7b356-879d-4251-b48f-0777a182ed1f)(label(Uproot))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         516194c6-c79b-4203-a73a-8039de0c22b1)(label(\"(\"\")\"))(mold((out \
         Pat)(in_(Pat))(nibs(((shape(Concave 23))(sort Pat))((shape \
         Convex)(sort Pat))))))(shards(0 1))(children(((Tile((id \
         3e36f6f5-d6fc-4e23-9c84-b0d28943cb1a)(label(row))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         792ee653-8188-4c98-b6a5-b08a45cd84f3)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 44))(sort Pat))((shape(Concave \
         44))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         4ac27db1-45f7-40c4-baaf-e4e85e3b0c33)(content(Whitespace\" \
         \"))))(Tile((id \
         8be245b3-e108-4b9d-a6ab-1d0e9c9580ce)(label(col))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children()))))))))(Secondary((id \
         28f2370e-645b-4567-be00-422781cb191b)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         ebe17529-cf36-4a75-a675-b70be0362a68)(content(Whitespace\"\\n\"))))(Tile((id \
         183107e2-bcfd-4175-80bf-ec7d33416032)(label(updateGrove))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         00729f8c-d0b3-4324-81b2-b7750fe85e30)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         2ebe6706-48aa-46fa-8517-89a6c8020aae)(label(m))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         a89a4063-89f2-4f0e-bd6f-e4322b3da346)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         37673692-e880-4870-9d0f-42ec95bccb33)(content(Whitespace\" \
         \"))))(Tile((id 0a2c3125-a9b5-4d35-8cca-dced93b6f519)(label(fun \
         ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 37))(sort Exp))))))(shards(0 \
         1))(children(((Secondary((id \
         7fc07544-5f34-4a85-a642-6e06afae3546)(content(Whitespace\" \
         \"))))(Tile((id \
         9b0d38fc-9fcb-4537-9874-20eabf94c1e7)(label(g))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         b14568d9-1190-4203-9aec-5bf9cf803a77)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         12df05b6-8370-40ac-b11a-c8aece5baade)(content(Whitespace\" \
         \"))))(Tile((id \
         6eee51d1-c647-4764-906e-368c7d33b933)(label(setCell))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         854d94e8-ca7e-44c1-8563-3c545f1e0500)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         fa5d9327-f941-4e76-84e2-2cdca764a575)(label(g))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         34e7e8a6-c485-47d1-837d-5859dff1fc6f)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         37d5aca4-cd3e-4f63-a13c-2ada64f3efe8)(content(Whitespace\" \
         \"))))(Tile((id \
         3c1bb820-7433-419f-956c-9c96b88ac62f)(label(row))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         c1ffc507-ad81-46ac-966b-79f5914e04ac)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         0aab5e6c-414b-465d-9320-dcb5f4c3f381)(content(Whitespace\" \
         \"))))(Tile((id \
         00f2f7bd-28d8-4bc3-976f-4eeee3968960)(label(col))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         8856b88a-c017-483b-a7a3-56d7ebe47eb4)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         ac30af5b-5cdf-4fd9-9b1e-f358e4e8303c)(content(Whitespace\" \
         \"))))(Tile((id \
         51aa7952-31c8-4d0c-90d5-7a823b56b592)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))))))))))))(Secondary((id \
         86057a47-0992-4178-b1aa-d48ca11489b5)(content(Whitespace\"\\n\"))))(Tile((id \
         71b4977b-25c2-4e62-a1b0-a4f836aad8b8)(label(| =>))(mold((out \
         Rul)(in_(Pat))(nibs(((shape(Concave 46))(sort Exp))((shape(Concave \
         46))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         6c5e64d2-1b6a-47fe-ab35-f5b28941e91b)(content(Whitespace\" \
         \"))))(Tile((id \
         fe461a63-2254-47a6-8f97-1abafb092df6)(label(ClearGrove))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         afc78f65-76ef-4a05-9d8c-95acf45119bf)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         d4ce899b-0283-4509-bc77-d6ae21005bd3)(content(Whitespace\"\\n\"))))(Tile((id \
         ea491cec-f5c5-46e7-afee-22a81fb2bfb3)(label(updateGrove))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         936484be-4cd9-45d9-b827-7a1453209fd3)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         5fef5e6b-d094-428b-8d02-8e19e72fe09f)(label(m))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         a7b28069-88c3-4aa2-ac9a-6e8d48f3a11a)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         1669012a-4dc0-4bad-96e8-8aaeab00984b)(content(Whitespace\" \
         \"))))(Tile((id 8b65ba67-4902-4ca0-b972-8068b0006f24)(label(fun \
         ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 37))(sort Exp))))))(shards(0 \
         1))(children(((Secondary((id \
         86ec8894-60e3-476e-b5f9-4ab40c84595a)(content(Whitespace\" \
         \"))))(Tile((id \
         520571f6-819a-43ba-957c-10ff05846352)(label(g))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         54674ab4-96b1-4d57-ab7b-6671977b6c21)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         5b5bd582-42e4-456f-9ee7-1c32afe06db9)(content(Whitespace\" \
         \"))))(Tile((id \
         75b4ebd4-c6a3-44a9-bb41-a3677fc46e09)(label(setAll))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         e55301aa-49b6-41da-bcd6-942c0f218d55)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         4cfe7a27-f1cf-4ccb-be45-271da273688e)(label(g))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         e85b2ed2-c260-4e1f-8f60-7f0efb412ca6)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         b3616d29-2aa8-45cf-afb6-025913453f5b)(content(Whitespace\" \
         \"))))(Tile((id \
         8d980836-cc6d-4da5-afc3-aa9ff1a41412)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))))))))))))(Secondary((id \
         5ea8e9bf-873e-41a2-9396-3bab327b3bbd)(content(Whitespace\"\\n\"))))(Tile((id \
         d527fad5-85c4-47eb-a084-2ff60024e621)(label(| =>))(mold((out \
         Rul)(in_(Pat))(nibs(((shape(Concave 46))(sort Exp))((shape(Concave \
         46))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         45b96b25-19b7-435c-8fa9-0199fc9428da)(content(Whitespace\" \
         \"))))(Tile((id \
         d5adc4ad-4470-4078-80e5-66ec1cef30a4)(label(PlantRow))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         0e5cdc88-ae22-4b6f-8478-ec0618865b26)(label(\"(\"\")\"))(mold((out \
         Pat)(in_(Pat))(nibs(((shape(Concave 23))(sort Pat))((shape \
         Convex)(sort Pat))))))(shards(0 1))(children(((Tile((id \
         046685c5-194a-4411-96d0-516006c39e07)(label(row))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children()))))))))(Secondary((id \
         6d278371-05b4-4dec-a4e3-15daae1a795c)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         91466a59-2729-4d5e-ba07-2dd422ac47b1)(content(Whitespace\"\\n\"))))(Tile((id \
         0c880ea3-d422-4de5-9bb9-a9912c6919a3)(label(updateGrove))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         644529ba-26f5-4460-97e8-64c0304db6ab)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         d5fa23b6-0c89-4c12-b636-ace1ce0fa9de)(label(m))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         522a1d44-b866-4379-8345-608ac722eb9b)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         4dd01a2d-f775-4dea-a34d-22a135942771)(content(Whitespace\" \
         \"))))(Tile((id 443c79b5-442e-48a6-bf0f-b126fa548ee5)(label(fun \
         ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 37))(sort Exp))))))(shards(0 \
         1))(children(((Secondary((id \
         8b9e3102-e12d-4411-b40e-4e81596335a1)(content(Whitespace\" \
         \"))))(Tile((id \
         0821bf9a-73bc-4f20-9b18-ed5f35e0c0f2)(label(g))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         4c9201ee-53bd-4aed-9deb-b56dd9da9504)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         42337bde-f037-4c30-a07b-81fe5b11b4cf)(content(Whitespace\" \
         \"))))(Tile((id \
         7b2a5136-411a-4fcf-af38-366f24f0f672)(label(setRow))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         0d879150-ad1d-4d6f-abab-58187d41d341)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         2ba76894-14cd-4ae5-8b3e-7e7de86550a2)(label(g))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         df0ac0d1-04c5-451a-8d3a-b09678c17060)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         9101250c-42b7-4a84-8987-a986c88679b6)(content(Whitespace\" \
         \"))))(Tile((id \
         87b5e341-bdd1-4136-a63b-bc4ea72f3734)(label(row))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         bcf6a9fd-7832-4a22-91d4-f3b791b34158)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         0ba1c89d-f7a8-4f60-a45b-dbb5daa7b89d)(content(Whitespace\" \
         \"))))(Tile((id \
         2f105dbc-96bc-44cc-b15e-1c8e7d4eacf4)(label(m))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         18231ec2-619e-4577-a47b-2afd0ad42a84)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         24faedf1-05f0-4b1a-87f6-b88926ff0826)(label(currentSeed))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))))))))))))(Secondary((id \
         b5b3b717-92c9-47f6-b7cd-2da2917a9cc5)(content(Whitespace\"\\n\"))))(Secondary((id \
         2588b578-f685-48ed-971f-33b428eb128d)(content(Comment\"# TODO: Add \
         PlantCol case here #\"))))(Secondary((id \
         e856a3fe-37e0-41d8-8d9e-a239798ae610)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         d8249666-c6ff-4964-985b-ac39d9b65f43)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         35061a97-59c9-465b-b50d-4c0da972519b)(content(Whitespace\"\\n\"))))(Secondary((id \
         a3ec10dc-fe23-42f3-8947-87b3feef6378)(content(Whitespace\"\\n\"))))(Tile((id \
         cbf7eaa9-e137-49ca-8019-5a35b8ac2153)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         fb8117ca-ca75-4b10-90d1-50662d23a42a)(content(Whitespace\" \
         \"))))(Tile((id \
         6e731728-3367-4ba7-a7fb-85de26159321)(label(do))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         f029438b-815b-4a2b-85c1-efad17092ab8)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 24))(sort Pat))((shape(Concave \
         24))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         9c649801-d810-4549-b846-c0b7eaabf40f)(content(Whitespace\" \
         \"))))(Tile((id \
         32050230-de79-4a23-88eb-463eb4c86019)(label(\"(\"\")\"))(mold((out \
         Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0 1))(children(((Tile((id \
         2ff315f2-a219-45aa-8a68-16772cef76e5)(label(Model))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         fd9219ad-31b0-48e7-b4ea-b786290a81d1)(label(,))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 44))(sort Typ))((shape(Concave \
         44))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         493f57a8-b759-47ce-824c-95ad92a42137)(content(Whitespace\" \
         \"))))(Tile((id e336eacc-a732-475f-9329-9d59afb7c3ef)(label([ \
         ]))(mold((out Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape \
         Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
         911f11b5-bd13-4536-99b1-261b8cb2e17c)(label(Action))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))))))))))))(Secondary((id \
         015e97dc-7239-4742-985e-94f42fc97048)(content(Whitespace\" \
         \"))))(Tile((id \
         c2ac5c3e-92b7-4ff2-92c7-d7bfbc1f414a)(label(->))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 13))(sort Typ))((shape(Concave \
         13))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         9b7963e4-b231-472c-9841-24a0529d55db)(content(Whitespace\" \
         \"))))(Tile((id \
         fb739cf8-a106-428e-a5bc-aa2c6996d59b)(label(Model))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         2fca9c4e-da3e-4e86-bceb-ea8581d10ebc)(content(Whitespace\" \
         \")))))((Secondary((id \
         03cb6cac-b2e3-4723-970f-4f5ab79fdf3c)(content(Whitespace\"\\n\"))))(Tile((id \
         9adf9bb6-3dfe-4fed-9a44-4301a07b1b65)(label(fun ->))(mold((out \
         Exp)(in_(Pat))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         37))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         822d4a90-3cc8-47ff-8ba0-86195e67dc44)(content(Whitespace\" \
         \"))))(Tile((id \
         4c5f8599-e18f-4446-9f21-7e5d6cfecd02)(label(\"(\"\")\"))(mold((out \
         Pat)(in_(Pat))(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0 1))(children(((Tile((id \
         1988af51-ccfe-4da3-a6ff-657579d47493)(label(init))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         0ce996cd-86cf-4176-9325-c48056788c6d)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 24))(sort Pat))((shape(Concave \
         24))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         c73d3334-c624-4465-b70b-09b3a5423727)(content(Whitespace\" \
         \"))))(Tile((id \
         24a99543-a572-457b-a74d-5ec82b114268)(label(Model))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         9e8fc4f3-72e5-4868-a3b7-70df15c0717d)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 44))(sort Pat))((shape(Concave \
         44))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         7a630fa5-e77b-47e8-af73-8392dab41b90)(content(Whitespace\" \
         \"))))(Tile((id \
         f5351c5b-59db-4229-9bda-44035ef9b94e)(label(actions))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         723a92ba-8ec2-4a46-bacd-d652f4814c7d)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 24))(sort Pat))((shape(Concave \
         24))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         a0cd9017-824a-4aa0-8c8d-7e5e0648d2e8)(content(Whitespace\" \
         \"))))(Tile((id 5062404d-57b5-4c78-86a8-fa76ab9b6113)(label([ \
         ]))(mold((out Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape \
         Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
         c3e99916-adca-45a1-9cdf-fca1db4d4580)(label(Action))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))))))))))))(Secondary((id \
         87da211a-6013-48da-adb9-0e28960717e9)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         343d440f-987f-4b91-be07-25ef04d88208)(content(Whitespace\"\\n\"))))(Tile((id \
         a9b26af1-47c3-417e-b6a9-83d6a22270cc)(label(fold_left))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         eaf5039e-ad3e-414e-8599-bb3b95731399)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         2694401b-de23-42ab-b626-17a06a630742)(label(actions))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         bbfc50ae-7043-4d0b-a730-240fe205aa1f)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         3e32e262-529c-44d0-b080-d5b9d5d33100)(content(Whitespace\" \
         \"))))(Tile((id \
         36b2eb5d-a0a4-4de6-bcf8-c6046aa9f708)(label(update))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         a76a7a66-4222-4d12-8683-bec4f301b63f)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         df9ed5e1-a754-44ed-a493-9a2e917cd646)(content(Whitespace\" \
         \"))))(Tile((id \
         43b967f9-cb64-4c66-93c7-c1a0fa176fee)(label(init))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         47c7da6f-f702-4ea8-bfd9-9b0b6a9021dc)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         3e76ae99-a4bb-4bd0-9599-d30dfecb3d5f)(content(Whitespace\"\\n\"))))(Secondary((id \
         b3f60845-6e15-43b4-9381-bf5e4e1985f4)(content(Whitespace\"\\n\"))))(Secondary((id \
         10c03d1a-ab3b-4710-9aa0-10acecfae3c7)(content(Comment\"# Existing \
         tests #\"))))(Secondary((id \
         e2721423-82e4-4ff6-a6ac-ddf9d0e9c4f0)(content(Whitespace\"\\n\"))))(Tile((id \
         bf27b2a0-37b3-4972-b884-57f21f8625a7)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         bebacab6-9199-4de0-93fd-d4285887e5ee)(content(Whitespace\"\\n\"))))(Tile((id \
         87277476-0dff-458b-8dda-ca5959c047a1)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         4ab7d44e-22e8-4f50-b3f4-51a2dd809ab1)(content(Whitespace\" \
         \"))))(Tile((id \
         2c4fc029-821e-4b1d-bc79-6cb9aee5fc1b)(label(m))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         a14cbc95-58bd-4103-9850-79376c1d2be3)(content(Whitespace\" \
         \")))))((Secondary((id \
         a055a63b-88fd-4501-84e4-83d5617d858f)(content(Whitespace\" \
         \"))))(Tile((id \
         96930ff0-fe67-4030-972d-96e00f9807a8)(label(update))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         1c97a036-e01e-4744-88c6-b13dd8348ac8)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         9d46c538-bc7b-4154-a71f-d879fdc90793)(label(init))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         29f9983a-8757-4cd2-8af1-a393aeea88c2)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         9e643113-5fbb-4592-89cf-b75a8659c199)(content(Whitespace\" \
         \"))))(Tile((id \
         ba19fa9b-e359-4785-b34a-dcc87dff62cf)(label(PlantRow))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         add14779-29d1-47b8-ab0a-79c8031e905a)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         0d5b1777-af12-4397-bb4d-c3316d727200)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))))))))))))(Secondary((id \
         a026e417-8e44-4fec-929f-eaf231ece2bf)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         5dfd82e1-b954-4dec-9886-519c97323ab1)(content(Whitespace\"\\n\"))))(Tile((id \
         8fc54207-b6ee-4b5f-9602-6d07173f9664)(label(m))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         54c416a0-9722-46c5-842e-a7be594816f8)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         370c5c42-6b65-49f4-b631-8e0ab1532588)(label(grove))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         684ce229-7539-470f-8adc-e26f0e77d096)(content(Whitespace\" \
         \"))))(Tile((id \
         766fef73-af5e-4ebf-8531-dc97bf55df6c)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         adc172b7-b6f7-4912-bd7d-8e2eb02995fc)(content(Whitespace\" \
         \"))))(Tile((id 2dc28f20-e53e-4b7c-ad1c-0bdce7087ab1)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         a6db0e4b-3a72-478d-8359-3cf46bfd0e96)(label([ ]))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         19cce3ef-2431-4475-8823-b6a40c728fa6)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         dfd3bc83-4550-4e8f-b76f-44e85ad0db04)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         43e1428b-edab-4cae-a72f-1e3643ec8272)(content(Whitespace\" \
         \"))))(Tile((id \
         a69abd6a-2096-4a0b-b20d-e064d48944f3)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         86ecdc80-dc7c-43a3-8c02-1736a7527802)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         b7c292d7-ea69-4f04-93c3-303cf2d27e85)(content(Whitespace\" \
         \"))))(Tile((id \
         d2a318df-0754-4df3-bf3e-84f832f311d3)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         1817762c-d93c-4738-b07d-4b8fe541c1b0)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         3c1c5ef7-d0ab-4475-b8fd-6d89d322cbec)(content(Whitespace\" \
         \"))))(Tile((id cb487791-d101-4b88-8fa8-e452f6717829)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         1a1e6463-64ba-4c38-9e47-545c0ed6b338)(label(\"\\\"\\240\\159\\140\\177\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         d28718a2-0de5-4436-805b-8f03f77990f3)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         bf7222fa-6e06-4db7-8bd0-7d2e384298e5)(content(Whitespace\" \
         \"))))(Tile((id \
         a69b834e-5019-4ecb-83d3-4a3e6e2df904)(label(\"\\\"\\240\\159\\140\\177\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         0411e5cc-247f-4cd2-ae20-25db700dd07a)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         649137d5-762e-4198-9925-7692600556c3)(content(Whitespace\" \
         \"))))(Tile((id \
         56a9f215-9c0a-41fb-b4b4-5a1bdbe4dd19)(label(\"\\\"\\240\\159\\140\\177\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         1427e32f-e267-485f-9be9-f8ee44e75dac)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         07da8ef3-6828-4a75-9134-666373b6e422)(content(Whitespace\" \
         \"))))(Tile((id 661876aa-7d55-498c-9804-2bc126d262b4)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         9b241886-98b2-4614-bb0a-ee5d1a364f30)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         f3be30ed-c595-4d2d-8f22-57d99aeb978e)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         cb2de9fe-4cda-4106-b692-a4c325f89f84)(content(Whitespace\" \
         \"))))(Tile((id \
         51816410-d6ba-40d9-b151-2032407b2c93)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         487c47eb-bca1-4b2e-b63c-1a1dba075ec4)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         07353912-7130-482d-8d24-4e63d069ef9e)(content(Whitespace\" \
         \"))))(Tile((id \
         53738006-74ed-456d-8d19-044f9b93d5e1)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))))))))))))(Secondary((id \
         c4fb0c27-d2ba-4502-b6ec-f27d1dde1950)(content(Whitespace\"\\n\")))))))))(Tile((id \
         d90545ea-e413-4c52-ac47-b90fda8eb3fd)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         a0595931-1940-48b2-96aa-ce760ba0a49d)(content(Whitespace\"\\n\"))))(Secondary((id \
         52b14d39-4a2a-4466-ac15-8987e34455c9)(content(Whitespace\"\\n\"))))(Secondary((id \
         1602bac0-58bd-4f66-8ff9-57b2b9205014)(content(Comment\"# New tests \
         for PlantCol #\"))))(Secondary((id \
         e20871ff-f962-4d4b-bbbe-ae5941ae6734)(content(Whitespace\"\\n\"))))(Tile((id \
         69cbd8a0-9299-4ff4-9336-54048f85694f)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         c95b399c-f67a-4fad-ae5d-d2a6b39007a6)(content(Whitespace\"\\n\"))))(Tile((id \
         478b3d6b-4f96-4018-baf9-78f1851e9d58)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         59c16edb-6588-4791-af1c-bc940e063956)(content(Whitespace\" \
         \"))))(Tile((id \
         1d3dc13f-1573-4c66-9653-365146e8df6f)(label(m))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         00a4ee7d-e000-4580-b32e-1a2e374fb1f0)(content(Whitespace\" \
         \")))))((Secondary((id \
         2ca49856-2a85-4a89-b1c3-97b384a54274)(content(Whitespace\" \
         \"))))(Tile((id \
         ce954683-4e6c-4761-ab47-1edb8d1dd2f1)(label(update))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         ca3e3c80-74bf-4957-8042-e85492eeb42a)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         51784089-44e3-45df-8fb4-5bc51e88ac57)(label(init))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         3f1e8207-a778-4bf3-a180-164ec2e4dd95)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         4e79a2ea-72eb-47ce-9a1e-e5ef520ae07b)(content(Whitespace\" \
         \"))))(Tile((id \
         558a4e71-631e-484e-9ebb-c94c70c957df)(label(PlantCol))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         a12e2704-7fe7-460a-a673-0cf93ecf0258)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         18985fd1-f6a4-42c7-99db-25290c2d5621)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))))))))))))(Secondary((id \
         b36b39c3-3b41-4dfb-b51c-7a40c355113f)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         6d1cd634-a580-4ead-a5cd-8893148dc470)(content(Whitespace\"\\n\"))))(Tile((id \
         c555bff3-6ea3-441d-934d-d3a11d1bd0c8)(label(m))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         440a576f-fc59-45e0-aad1-6c1a9745be8c)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         53531856-8460-4266-a87e-1e4dbf7d9498)(label(grove))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         eb99c65a-6c28-4bea-b9cb-61d0af63e130)(content(Whitespace\" \
         \"))))(Tile((id \
         bb355db2-da06-4bce-87f1-31863e4ecdc4)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         61e3cfd6-e109-4284-beb4-5550a96000fa)(content(Whitespace\" \
         \"))))(Tile((id 7b701425-8ee9-476a-b365-58417524807a)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         63cda0e7-e1d9-415c-85d8-f19c0e09daa2)(label([ ]))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         f01ecae1-d128-468d-93de-acd7586471fd)(label(\"\\\"\\240\\159\\140\\177\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         d44c1e9a-2480-4662-b7e8-e7630cee29aa)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         06b07b2a-ce64-45bd-b26b-ada8d138b1cc)(content(Whitespace\" \
         \"))))(Tile((id \
         108ecb41-5187-4b07-93d1-c47af34a4873)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         9fa73abb-f671-4738-a683-aa2066d7d38d)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         6b169445-4fe7-4ce3-9039-a0aa1f72660d)(content(Whitespace\" \
         \"))))(Tile((id \
         0bbd6c82-8ca3-4a7d-b85f-7a07eea63d32)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         42b7a0ed-fb34-4f07-8edd-7c5d738db4b1)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         b17a2469-8a4a-4526-8d86-cd680ee741f3)(content(Whitespace\" \
         \"))))(Tile((id 7f2548bc-49c1-456b-b558-4657df8bc644)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         e3ddaf89-7aac-4fbb-9614-1dc40335b70f)(label(\"\\\"\\240\\159\\140\\177\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         76fea8ff-a440-4dc5-b500-7fa0a14d0750)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         49ad3854-48eb-42c7-962d-a42f39eb9d24)(content(Whitespace\" \
         \"))))(Tile((id \
         7ab519df-3e4b-438d-8f5a-41757b64a97e)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         6d9d0811-4101-4ade-b00c-78f04d6ee495)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         5d2e5360-dd80-4404-b43f-1179f2b39f70)(content(Whitespace\" \
         \"))))(Tile((id \
         845f3960-d587-42fe-80d3-132bcdadd6e8)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         deab93ea-733d-45f7-a265-a20de6f4e792)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         64e27408-cff9-48c6-b07b-2a1bebb621f0)(content(Whitespace\" \
         \"))))(Tile((id 5a522aab-1012-48cd-b171-e37628d81e46)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         57e4c7a0-c2b3-4840-ac5f-fb5343ac1082)(label(\"\\\"\\240\\159\\140\\177\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         d1f7d080-88b7-4e4e-a31b-03df75170a4e)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         8e5ea6e2-f54b-4848-950a-2e5bbfcf95ed)(content(Whitespace\" \
         \"))))(Tile((id \
         d841d554-08e6-4d16-94cb-9651553f6d29)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         f5286ce2-eb5e-4c4b-a3e8-9ecda0b53976)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         37bc7e20-c758-40c8-b6c5-086a2c889831)(content(Whitespace\" \
         \"))))(Tile((id \
         1ae488d8-baae-419b-9b2e-d76236d355bf)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))))))))))))(Secondary((id \
         4d73f472-38c8-4074-9bee-47afe047c955)(content(Whitespace\"\\n\")))))))))(Tile((id \
         95986598-0052-4abf-b483-a5dc8b87c8b7)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         254660f5-9aaa-414a-8fd5-28dcd1d4057f)(content(Whitespace\"\\n\"))))(Secondary((id \
         15ee83be-e70d-4bd1-ab70-ffa5c5f01a57)(content(Whitespace\"\\n\"))))(Tile((id \
         907d62ed-fe81-4f27-ae9e-9787574f2a84)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         d9be19d2-dc9a-4a64-b696-54303b860907)(content(Whitespace\"\\n\"))))(Tile((id \
         5f46031e-034a-4c82-8474-b85305283c97)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         50e3d1c4-9160-4c61-a8e5-36a68a3e774d)(content(Whitespace\" \
         \"))))(Tile((id \
         8c153e75-a084-4c50-a4fe-85098c04caae)(label(m))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         a1229feb-b053-41b3-87cf-146da4b7305d)(content(Whitespace\" \
         \")))))((Secondary((id \
         b86ef2b6-8643-40d4-96b9-f5fefdfbc25f)(content(Whitespace\" \
         \"))))(Tile((id \
         6d2ed5c4-d632-4e44-8fd2-f7c101a116eb)(label(update))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         f31594c6-c416-4a81-98f1-8291b1694d51)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         14eb97b5-202e-4de7-bf3d-efb52d29c79f)(label(init))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         c48246da-b945-4a0a-8d80-260552551258)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         14411118-d950-4880-be92-f4a4106f9035)(content(Whitespace\" \
         \"))))(Tile((id \
         2d072ad9-a770-44f2-88ae-1e05c525d4a3)(label(PlantCol))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         7a55d9bf-1c67-45cf-8127-f73e5c60efa0)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         4ab6f180-1271-46f0-88a1-a9916b8a8040)(label(2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))))))))))))(Secondary((id \
         ec82b21a-2dab-4826-8b83-719cf3d5dd56)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         8b1a89d6-5ed6-47f5-aa8b-6a61f40f3607)(content(Whitespace\"\\n\"))))(Tile((id \
         ab71f88b-6ea8-4412-9100-8e4339d279b0)(label(m))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         23fe38b1-f628-40f5-8fd0-c58923e1a137)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         74e0218a-dd57-4287-8841-23bfbfeeb3e8)(label(grove))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         83b66179-0db7-4fc8-b7c2-12033b452752)(content(Whitespace\" \
         \"))))(Tile((id \
         61f59302-c956-4ad0-a240-bd55457ad7bb)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         6dfa471e-d0d1-44ab-b5d4-e26391c3b5b8)(content(Whitespace\" \
         \"))))(Tile((id e4e992ac-4008-4761-9388-70bd3560189b)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         91eb33f2-a959-490d-b598-f59a366a5f0d)(label([ ]))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         d28bb1da-396e-4800-b615-8131c7acd254)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         19ce1252-aafe-49ef-8708-0851a4dd28c2)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         6f018441-1623-4c39-8a4c-97b65af17705)(content(Whitespace\" \
         \"))))(Tile((id \
         bd7836b1-aa52-4f0a-9233-a5ecbdf1bc13)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         7d4d8e63-24a0-4d25-b689-06047e6bdb57)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         573261a4-e182-4552-bc2a-015d45f507eb)(content(Whitespace\" \
         \"))))(Tile((id \
         4ad84d81-ffa9-4490-89cb-67497bd94a37)(label(\"\\\"\\240\\159\\140\\177\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         081e4313-a6dd-466b-b984-e5bcf9672861)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         f43d8862-997c-4cc5-9440-12b644053438)(content(Whitespace\" \
         \"))))(Tile((id 5f7269f0-939e-4b7f-90a4-aa6857393f03)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         ac4c65a8-eb22-4c98-ac81-4d4922372b1a)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         e232d8b6-ee93-45aa-984f-6ad07fada2ff)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         b39af84a-197d-4eed-81aa-768f5e1f787d)(content(Whitespace\" \
         \"))))(Tile((id \
         15597731-ef6e-4394-af36-4910682db440)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         e4650409-2e3f-46c1-bd8c-e3e4d7a6a02d)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         d754a3c3-0e7f-424e-8895-8e7ada49a245)(content(Whitespace\" \
         \"))))(Tile((id \
         bd66fd63-9b58-4fa3-b634-fef9f49661b6)(label(\"\\\"\\240\\159\\140\\177\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         6d8b9e2a-0d65-4c10-9bdc-4d93fa44dc52)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         df08ea0e-7a21-44b9-aab1-8a6025cdc5ea)(content(Whitespace\" \
         \"))))(Tile((id 42084b1e-b6d4-49e7-9b55-d0d3d3392d43)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         4c2ad337-c49f-435e-aeb5-972f3ccc75ca)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         e45b14e5-dc61-4156-b702-be62f5ee1bbd)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         de560fa7-a143-45c2-8a11-26976cd88e4b)(content(Whitespace\" \
         \"))))(Tile((id \
         02f5aea4-537d-41ce-b8ca-a3f822117142)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         7af2ca92-676f-4624-b0d2-eb2b2b70a38c)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         5167c929-59fb-4dc8-93a3-1121faaee35b)(content(Whitespace\" \
         \"))))(Tile((id \
         1b227adc-8f75-4663-abc9-ec461cc57456)(label(\"\\\"\\240\\159\\140\\177\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))))))))))))(Secondary((id \
         3caef8e8-5ae4-4487-8fc9-5eabbcdf84a1)(content(Whitespace\"\\n\")))))))))(Tile((id \
         66f1245c-fd7d-470e-b32f-314e9e2c4480)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         1a6fa6c9-bf5c-4823-8c22-a0d88419967a)(content(Whitespace\"\\n\"))))(Secondary((id \
         554c9bb1-2c96-43a2-9963-26ffec370370)(content(Whitespace\"\\n\"))))(Tile((id \
         984247e4-b133-4172-a34b-fd422e3a45d5)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         aeb33bac-e296-4ade-9a16-a8023ad24f7f)(content(Whitespace\"\\n\"))))(Tile((id \
         cba6be3e-a27d-4880-8f6c-4f53227d2918)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         6b6f20c2-2092-49bf-a4f7-d96a458d9899)(content(Whitespace\" \
         \"))))(Tile((id \
         e012c694-a304-4da9-8885-45084a9ef455)(label(m))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         1153353f-5aec-4709-b196-6e9c543392e4)(content(Whitespace\" \
         \")))))((Secondary((id \
         82b299a7-d0c9-43e3-a3aa-8b7916df83eb)(content(Whitespace\" \
         \"))))(Tile((id \
         bf7dd5c4-d0c3-46e4-b865-9d4e1e9e3981)(label(do))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         cf1c1d9d-376e-4256-a7f9-87610a5d4b22)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         dc4b5bc1-f112-4d8d-943b-2f5c0d50c999)(label(init))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         ea83e31a-3f19-4f67-a5e3-05600dcddfad)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         1e254d61-0ef8-493b-acff-3207d7b19566)(content(Whitespace\" \
         \"))))(Tile((id ea4ed48c-fbe4-433d-906c-905b63eca63e)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         f21ec364-5a82-4f8e-a30f-15df160dbe86)(label(PlantRow))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         19443976-061a-40c5-888b-362ff8419701)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         8fe15722-6ba0-429a-8c32-225cb43f94bb)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         fee0d4b8-71f0-4ca0-9e3f-5977df801108)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         fa86f1b5-12d2-43d9-86a0-369596722bc8)(content(Whitespace\" \
         \"))))(Tile((id \
         f27ad129-724b-411b-b7eb-7dd30ad2f0c5)(label(PlantCol))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         5ffa7761-42be-41de-bccb-4a833c249faa)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         df31ac98-b440-4d5b-b65f-53598a459c4a)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))))))))))))(Secondary((id \
         27aa4dce-3ad3-47f9-b475-baf0e2ad84bd)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         13f02753-b9cf-4e90-9f9c-f630c6ae9aba)(content(Whitespace\"\\n\"))))(Tile((id \
         0fd76ca1-cc6a-4e0f-8837-e891eba4edde)(label(m))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         48b2435c-5097-43bb-8ad2-d56dd3252c0d)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         00b85a37-253c-4d9e-b95f-ce7ab2a5596c)(label(grove))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         5ce45a4b-e9bc-4a36-8174-3ce3414bca65)(content(Whitespace\" \
         \"))))(Tile((id \
         dcbed316-05ce-4f6d-b93a-45b37db84483)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         73dfac8e-14cf-4981-b02a-fb7c75e077ad)(content(Whitespace\" \
         \"))))(Tile((id 6e720f2f-fd51-41be-b53b-d1443ac2128f)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         78c6a723-6b3a-4083-bc23-88f6810ff74c)(label([ ]))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         b41880d8-ecb6-4e5c-897e-19b830c359b8)(label(\"\\\"\\240\\159\\140\\177\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         36ba2afa-704f-45d5-9e91-7281a939a4f0)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         aee53782-bde5-43cc-9ac7-f762cc27508e)(content(Whitespace\" \
         \"))))(Tile((id \
         e7e725cc-920e-445b-bf9f-48ab6e712e26)(label(\"\\\"\\240\\159\\140\\177\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         2c05dcfe-a34a-40d6-8761-1e062483e0c6)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         84b56cf4-56bc-4910-971b-cbf02cdd4ff8)(content(Whitespace\" \
         \"))))(Tile((id \
         22478a62-fb51-4909-a0b4-3933ec21f7ad)(label(\"\\\"\\240\\159\\140\\177\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         7e09ae51-0a0c-490e-8948-d528cf30b2fa)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         de349324-22be-4769-a04f-1bccdd3346aa)(content(Whitespace\" \
         \"))))(Tile((id 326ea938-04bd-4336-b715-3acbd12cce7b)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         a8d1ca57-d2e1-44a3-ae53-438e92ec043b)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         d3189711-89a4-47ac-9172-ce15a9363b59)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         968996e9-b41a-4d2d-a2fd-1a57cabead88)(content(Whitespace\" \
         \"))))(Tile((id \
         78e70bba-69cb-4dd6-a850-8c7c716a48ee)(label(\"\\\"\\240\\159\\140\\177\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         b3480f8c-7d58-4f96-8fcc-204cf9c16837)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         45db2508-d05d-49f9-90d8-ba762cf73f24)(content(Whitespace\" \
         \"))))(Tile((id \
         851251cb-e857-42a1-9f59-4238e9bdc5ee)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         5adb050a-963e-4db2-b824-3be8a07daa93)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         b8514368-c55e-4afb-add7-ce71e3853e6e)(content(Whitespace\" \
         \"))))(Tile((id 7182cb11-3eb4-4aef-998e-eaa769f577b8)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         d9706a72-6550-4cac-ba90-f7cd1f208d68)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         050c92b5-325c-43ad-9787-fe8539ec2415)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         79f95ed9-eb00-41e0-aa4f-efa2676fd906)(content(Whitespace\" \
         \"))))(Tile((id \
         5b46c2de-41ff-4874-a693-b7ff5aad8997)(label(\"\\\"\\240\\159\\140\\177\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         7a804976-41fb-4994-bf13-a1dc64f7a192)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         2e7e6f40-2c3a-4764-a15b-9f66184e0f0d)(content(Whitespace\" \
         \"))))(Tile((id \
         388c0d42-f170-4f70-8762-24e5d6c52d09)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))))))))))))(Secondary((id \
         b9d2aaae-9bdd-4caf-9760-cefbc5a38d57)(content(Whitespace\"\\n\")))))))))(Tile((id \
         c7b0ea6a-4e1b-4538-bfe6-9ea350224fab)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         f2e5d355-3107-411e-a7fb-7a03de5644b8)(content(Whitespace\"\\n\"))))(Secondary((id \
         9fe1fe45-5851-4b12-9376-060d5d0365ef)(content(Whitespace\"\\n\"))))(Tile((id \
         91b18fbf-d83d-4f0d-b550-8ebca8b2e2a1)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         4270cba0-f7e4-42e7-bee7-52ea027f94dd)(content(Whitespace\"\\n\"))))(Tile((id \
         3bf7aa55-7187-4c00-a40b-56efabcdc8c0)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         bc1cca81-e0dd-4093-a861-623f8f384440)(content(Whitespace\" \
         \"))))(Tile((id \
         4ec6ca53-c4d0-4ae4-985e-dc3c917c44df)(label(m))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         d594df41-8e1a-4a6a-bcc4-1fc5c33b7fba)(content(Whitespace\" \
         \")))))((Secondary((id \
         1c172106-ed6f-47aa-975e-bb2330fe1495)(content(Whitespace\" \
         \"))))(Tile((id \
         47059f0a-79e7-438c-b97f-def239815f00)(label(do))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         4e044206-9637-4eba-bc12-3e0dca0c895f)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         98835f48-64d4-4755-ada4-0cc24d99b47a)(label(init))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         62bcd0cc-77ae-4f0a-b2e9-edf822171f9c)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         d629d30c-2ee8-47b7-863d-b1ace6052157)(content(Whitespace\" \
         \"))))(Tile((id 8d304fd6-4deb-444e-a1c3-946c9da59e96)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         a0b8e9ba-7f4d-48e0-b3ed-aa0dcf7e6cfb)(label(SelectSeed))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         d818867c-0842-4f4b-806c-52d9a36711c4)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         1fb86152-403d-43fd-b253-99c90cb0a0e0)(label(2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         1acd817d-97d6-42af-abc0-269ade7fab24)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         e142e298-fc6f-481d-8dab-441f7006be92)(content(Whitespace\" \
         \"))))(Tile((id \
         40fe5ed9-c84b-4337-80e8-2ce9736c17e6)(label(PlantCol))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         34f279c6-532f-4835-b6d9-e5cb19cd7af9)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         73f3d25e-bdd6-4a1d-8c3e-8ad921a39ad5)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))))))))))))(Secondary((id \
         afdca924-5982-41c5-959f-cda1f494622d)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         a1dc75c7-bc0d-41f0-828f-dbf42fade077)(content(Whitespace\"\\n\"))))(Tile((id \
         d4237b7f-9d54-4700-b606-f6630ee9d406)(label(m))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         67a11b5f-766d-4e6b-8eaa-a683987c20a2)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         7e639c4b-272f-46f2-ae8f-41e6f17adfb5)(label(grove))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         3421fe3c-cdc8-488b-967f-e411e3447391)(content(Whitespace\" \
         \"))))(Tile((id \
         329fdee0-2774-45ee-b4e6-9406b3185eb5)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         391978f1-71e2-4e65-8548-9bcd896f7912)(content(Whitespace\" \
         \"))))(Tile((id 293e4d86-f069-4aea-982d-9d35a2f25880)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         635f5940-e7fd-4a6c-bb30-d51332f0f87f)(label([ ]))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         14366952-ae13-4d01-ad90-b2892d08cefa)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         49f5d7f0-03aa-4fbe-b8e6-ad8f4cd2af0b)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         3f89c334-dd88-421b-b6ca-2b0858d036cb)(content(Whitespace\" \
         \"))))(Tile((id \
         9c9d8a1e-cf92-4bf6-9ad5-31e7233a7797)(label(\"\\\"\\240\\159\\141\\132\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         fcfdc037-e9ca-4f45-bd2c-9fa08777b8f6)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         bad258e9-248e-4365-9339-a23f9314d22e)(content(Whitespace\" \
         \"))))(Tile((id \
         cba7210f-14e5-4011-b027-1e739ff2a8fc)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         51f3cef3-d3c9-4538-a79d-62a807e17f68)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         5b393044-66dc-4c86-b58e-e508755e98ae)(content(Whitespace\" \
         \"))))(Tile((id 12a76a4d-a082-45a9-bb9d-558c7356bf84)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         9034f8b0-867a-43e9-b185-a82cbc2a6462)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         61c604bc-217b-4a56-a04f-d3c67fba807b)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         2fd692ec-800e-4406-8f04-ea56e2539655)(content(Whitespace\" \
         \"))))(Tile((id \
         5564d310-7498-4747-9e42-722fbc677f96)(label(\"\\\"\\240\\159\\141\\132\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         417afc0c-a9d1-4418-ad7f-35c4b94beda3)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         288a2a86-31bf-4759-8e71-7a9831448125)(content(Whitespace\" \
         \"))))(Tile((id \
         40808e4b-6e68-44ec-9006-3462e456e631)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         dcbdf39a-c45b-4e0c-b5b7-ff24c37cd67f)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         b2ada13b-5c8e-4e28-ba39-b8c0d1032e16)(content(Whitespace\" \
         \"))))(Tile((id 096db8af-447b-430e-9c01-68edc54c593e)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         7abf39f1-92c2-46b1-9012-b61dc83ea144)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         b51bcb94-0130-4f4a-badb-b4170e4cb4c8)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         1ba5a4c7-e32a-485e-88ea-96c6c9be620f)(content(Whitespace\" \
         \"))))(Tile((id \
         1934f4ed-e272-476e-8d91-0171dc8ac19d)(label(\"\\\"\\240\\159\\141\\132\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         89307340-b362-492b-8bde-37ae89ab7e8a)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         7147fb29-db18-4c88-b480-441d7a0ae75b)(content(Whitespace\" \
         \"))))(Tile((id \
         b87b2d6b-640f-41e7-8711-04275d884a82)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))))))))))))(Secondary((id \
         067e6dc8-ac31-4980-a219-375b98389f30)(content(Whitespace\"\\n\"))))(Tile((id \
         cb5195ee-e576-44c3-9f8c-74d64f9288f7)(label(&&))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 32))(sort Exp))((shape(Concave \
         32))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         2427b843-c441-4de1-9c72-091826cc50f9)(content(Whitespace\" \
         \"))))(Tile((id \
         f19a515b-3427-4c54-8814-e861ec7ddb6f)(label(m))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         78d721cb-19ee-4608-a2b5-4efbc0acbc39)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         4ae0ca0e-8cc2-49ac-8543-98a2d1d14711)(label(currentSeed))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         7814f3cc-d143-425f-9e98-502dc1365dd6)(content(Whitespace\" \
         \"))))(Tile((id \
         854da679-b489-4086-834b-d0b91739b674)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         86c220b1-2bc5-4d8a-bc9a-72f1e971fe93)(content(Whitespace\" \
         \"))))(Tile((id \
         fda1fd61-cdbb-4c12-ab05-225c8d70505b)(label(\"\\\"\\240\\159\\141\\132\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         9c547f6c-54ef-4df2-8d64-8eb257844b9d)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         34fbf5b4-c204-4683-a3db-132cbaa700a3)(content(Whitespace\"\\n\")))))";
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
