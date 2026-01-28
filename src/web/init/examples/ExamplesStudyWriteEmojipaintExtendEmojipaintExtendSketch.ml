let out : string * Haz3lcore.PersistentSegment.t =
  ( "Examples / study-write / emojipaint-extend / emojipaint-extend-sketch",
    {
      segment =
        "((Secondary((id \
         65822757-cb27-4b10-8460-903c434f257b)(content(Comment\"# EMOJIPAINT \
         EXTENSION TASK                     #\"))))(Secondary((id \
         bba744d5-b538-48f1-8b46-4953742f7451)(content(Whitespace\"\\n\"))))(Secondary((id \
         921b1779-38f6-43ee-a678-56b20e2b5aca)(content(Comment\"#                                               \
         #\"))))(Secondary((id \
         4c2cb940-07a3-46f3-af02-9686ad41919e)(content(Whitespace\"\\n\"))))(Secondary((id \
         8f5929f4-345e-46f0-ba9c-ad267ff2fa73)(content(Comment\"# The \
         emojipaint app lets you paint emojis on   #\"))))(Secondary((id \
         30bae551-6cd2-4cbc-9c83-27c112328257)(content(Whitespace\"\\n\"))))(Secondary((id \
         dda7928c-5154-4bd7-88be-e8aeeec67244)(content(Comment\"# a grid. It \
         already supports painting rows.    #\"))))(Secondary((id \
         c2b46034-998c-4f5b-8f47-9c8f737c8e4a)(content(Whitespace\"\\n\"))))(Secondary((id \
         d0805d78-8490-498a-9047-68e656500d19)(content(Comment\"#                                               \
         #\"))))(Secondary((id \
         b0b64868-6080-4b0b-8edc-ad9b2758fee3)(content(Whitespace\"\\n\"))))(Secondary((id \
         6882134a-fdab-4015-98de-f9038af33036)(content(Comment\"# YOUR TASK: \
         Add a PaintCol action that fills   #\"))))(Secondary((id \
         42214975-f24e-47fc-b5ab-50a0e5ce15f9)(content(Whitespace\"\\n\"))))(Secondary((id \
         196c193d-877d-487c-a6e8-db0b0d95515c)(content(Comment\"# an entire \
         column with the current brush.      #\"))))(Secondary((id \
         dae9f9ae-6d5a-4951-9266-7a66fd727d65)(content(Whitespace\"\\n\"))))(Secondary((id \
         98cb39ca-a97e-4f39-9b8a-df552319d809)(content(Comment\"#                                               \
         #\"))))(Secondary((id \
         cf10e5d0-4e9c-4f32-a432-0d166b52b8b6)(content(Whitespace\"\\n\"))))(Secondary((id \
         18a87dcc-5fd2-4790-b4da-e29825c5097f)(content(Comment\"# You need \
         to:                                  #\"))))(Secondary((id \
         b940479d-ed3c-4027-bb49-a19daee8152a)(content(Whitespace\"\\n\"))))(Secondary((id \
         cab2c4d4-3074-41fd-90c4-63db1225b83a)(content(Comment\"#   1. Add \
         PaintCol(Col) to the Action type     #\"))))(Secondary((id \
         2947179a-1372-4517-b6bf-f8e527b8d58a)(content(Whitespace\"\\n\"))))(Secondary((id \
         41c89548-2901-4571-84bc-7d6954f3beaa)(content(Comment\"#   2. Add a \
         setCol helper function             #\"))))(Secondary((id \
         694310ad-6683-40f9-aad5-2b5302b32c96)(content(Whitespace\"\\n\"))))(Secondary((id \
         0fc4d99f-bc29-410f-a479-46cd8d407077)(content(Comment\"#   3. Handle \
         PaintCol in the update function   #\"))))(Secondary((id \
         46763a9d-668b-4a01-a559-d6d4afdf27e9)(content(Whitespace\"\\n\"))))(Secondary((id \
         487792d4-95df-4bc3-8e79-fd7eb74cdd9c)(content(Comment\"#                                               \
         #\"))))(Secondary((id \
         19c2f779-84bb-4c82-9aee-de3c23fcaa1c)(content(Whitespace\"\\n\"))))(Secondary((id \
         0778771a-6ebe-4702-968c-a340bcba4cf5)(content(Comment\"# Look at how \
         PaintRow is implemented for       #\"))))(Secondary((id \
         7e1578c7-1765-4531-a7dd-b62873c93239)(content(Whitespace\"\\n\"))))(Secondary((id \
         d1a6ecab-9d57-4530-b966-68606ea00789)(content(Comment\"# guidance - \
         PaintCol is similar but vertical.  #\"))))(Secondary((id \
         509d3554-42b4-4fdc-91e6-0f0d78ad5e22)(content(Whitespace\"\\n\"))))(Secondary((id \
         e9ab7c0b-7c84-416a-b190-ae16f279b346)(content(Comment\"#                                               \
         #\"))))(Secondary((id \
         a7d8a100-448d-4a82-8e89-561ea8a59972)(content(Whitespace\"\\n\"))))(Secondary((id \
         82f9d82b-072d-49d4-b8ad-47b40fd12d28)(content(Comment\"# Tip: Use \
         auto-probe to see how the canvas     #\"))))(Secondary((id \
         202a1aa8-7f59-4a01-9047-b4b1430c3951)(content(Whitespace\"\\n\"))))(Secondary((id \
         f8b2420f-a342-4337-8a81-6b8c1bf5844b)(content(Comment\"# changes \
         after each action.                    #\"))))(Secondary((id \
         0ec9978b-27b4-49cc-a03b-b91a767ed2e1)(content(Whitespace\"\\n\"))))(Secondary((id \
         d1fe9392-c838-4fd3-820e-3df93cdf4f46)(content(Whitespace\"\\n\"))))(Tile((id \
         8469a682-f441-49a3-9d12-e6379aa02f77)(label(type = in))(mold((out \
         Exp)(in_(TPat Typ))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         40))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         a5379faf-43f2-4ade-a01c-7142e491259d)(content(Whitespace\" \
         \"))))(Tile((id \
         48c52427-e2e1-4fd3-9f5e-682a19783378)(label(Emoji))(mold((out \
         TPat)(in_())(nibs(((shape Convex)(sort TPat))((shape Convex)(sort \
         TPat))))))(shards(0))(children())))(Secondary((id \
         90eff96a-70d6-461d-95a2-43d35c517826)(content(Whitespace\" \
         \")))))((Secondary((id \
         d1527ec8-9189-4d90-b176-4aaad3189835)(content(Whitespace\" \
         \"))))(Tile((id \
         87711c5e-fd43-420d-a026-bf147d4fe2ba)(label(String))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         d11dc625-6c78-413a-aaf3-0218f6fb5170)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         4a59f848-b2a7-4a37-b63c-ecfe8cc9ae66)(content(Whitespace\"\\n\"))))(Tile((id \
         210ede75-e3f0-4905-b1bb-7c1f74a42155)(label(type = in))(mold((out \
         Exp)(in_(TPat Typ))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         40))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         bea8532e-fecd-40f7-a71e-d150e97384a3)(content(Whitespace\" \
         \"))))(Tile((id \
         43f2da24-ebd0-4a48-84ac-e5895e888f0f)(label(Canvas))(mold((out \
         TPat)(in_())(nibs(((shape Convex)(sort TPat))((shape Convex)(sort \
         TPat))))))(shards(0))(children())))(Secondary((id \
         5e6b1bd7-64c1-4954-9538-b4f304c932bb)(content(Whitespace\" \
         \")))))((Secondary((id \
         ac9e8f85-60cd-4eb3-8d1f-d6ce7bef2ee9)(content(Whitespace\" \
         \"))))(Tile((id 75ed05f0-4c5a-46fc-820e-a951129e81f2)(label([ \
         ]))(mold((out Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape \
         Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
         929e0d68-a5c7-419b-82af-a04b71a3d19a)(label([ ]))(mold((out \
         Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0 1))(children(((Tile((id \
         4a518e20-8fb2-4833-8236-1e415c48b6b5)(label(Emoji))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))))))))))))(Secondary((id \
         a17b5ebb-cf62-49bb-83e8-bf9492b0fe7f)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         01e2fcaf-7b63-45bf-ba3e-f8fd03a9968b)(content(Whitespace\"\\n\"))))(Tile((id \
         b43e6598-4395-4fa4-8985-3bfd07cd9190)(label(type = in))(mold((out \
         Exp)(in_(TPat Typ))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         40))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         4255e6db-45e4-4b1f-9354-ae0cae36e685)(content(Whitespace\" \
         \"))))(Tile((id \
         a8ff599d-06e1-42f8-b5f0-12f33232c00f)(label(Row))(mold((out \
         TPat)(in_())(nibs(((shape Convex)(sort TPat))((shape Convex)(sort \
         TPat))))))(shards(0))(children())))(Secondary((id \
         256b2f87-4c9a-48dd-988f-0fc49f27a57b)(content(Whitespace\" \
         \")))))((Secondary((id \
         13612e39-67d6-4284-9790-1ed360732291)(content(Whitespace\" \
         \"))))(Tile((id \
         12da6284-8ff0-4a6d-87fe-0787912714b1)(label(Int))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         1c0da65a-c8dc-44c0-b3bb-0f976a8187b1)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         bb501d20-ebf5-491a-98c9-da3f6f61c434)(content(Whitespace\"\\n\"))))(Tile((id \
         4b04d121-208f-455d-a2df-ee0512491108)(label(type = in))(mold((out \
         Exp)(in_(TPat Typ))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         40))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         fcac2070-e0d5-4622-a0c7-2e9f9d591a90)(content(Whitespace\" \
         \"))))(Tile((id \
         e87be3a5-10c6-481e-b1ea-87cea20fa047)(label(Col))(mold((out \
         TPat)(in_())(nibs(((shape Convex)(sort TPat))((shape Convex)(sort \
         TPat))))))(shards(0))(children())))(Secondary((id \
         57a1a54e-c001-4372-905a-1dbe31da2db5)(content(Whitespace\" \
         \")))))((Secondary((id \
         675aa77d-a840-43c1-88d3-df58a8255af5)(content(Whitespace\" \
         \"))))(Tile((id \
         552ea94b-2f2c-45a5-9535-d1ff91461282)(label(Int))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         302f68fe-081b-46f9-b60e-2aaa8a039e36)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         f0f322ad-04da-421a-b035-ab5e12c0a1eb)(content(Whitespace\"\\n\"))))(Secondary((id \
         017cd46e-537d-4d52-b5ba-8d6df3c4f61e)(content(Whitespace\"\\n\"))))(Tile((id \
         bc8c9918-6854-41cf-a092-a262dbedc19d)(label(type = in))(mold((out \
         Exp)(in_(TPat Typ))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         40))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         2b3fc742-063c-4783-8f7a-b51bb84c9adc)(content(Whitespace\" \
         \"))))(Tile((id \
         56a8a74d-43ea-4d92-9653-de0714f7956a)(label(Model))(mold((out \
         TPat)(in_())(nibs(((shape Convex)(sort TPat))((shape Convex)(sort \
         TPat))))))(shards(0))(children())))(Secondary((id \
         0d3d6709-e124-4d44-ae8e-5fc1ccf73b9a)(content(Whitespace\" \
         \")))))((Secondary((id \
         d9834c89-79be-4437-b58f-bfebf9ded1f7)(content(Whitespace\" \
         \"))))(Tile((id \
         80a0dc69-b90b-446a-9ea3-fc3377e2d934)(label(\"(\"\")\"))(mold((out \
         Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0 1))(children(((Secondary((id \
         52e201f7-2ad9-4436-ae38-5a90a7532d2f)(content(Whitespace\"\\n\"))))(Tile((id \
         236aa26d-9f05-4105-bb0e-26d305380aea)(label(canvas))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         f349426b-a7c1-4e84-b716-987edb4804da)(content(Whitespace\" \
         \"))))(Tile((id \
         43301422-f8eb-4675-a284-c496af1a2026)(label(=))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 39))(sort Typ))((shape(Concave \
         39))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         cbc5834c-1ce3-44d0-85a2-485a5565ebf7)(content(Whitespace\" \
         \"))))(Tile((id \
         ad35eb28-bedc-4e43-994a-ef53d8bd45d6)(label(Canvas))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         58e9d6db-8e82-4bf6-89c8-b2e4ad623e3c)(label(,))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 47))(sort Typ))((shape(Concave \
         47))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         9cb93af6-92c8-44e9-a81a-580853dba778)(content(Whitespace\"\\n\"))))(Tile((id \
         553c4e44-4dc4-410d-bc47-f87641883142)(label(brush))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         1e0bb2e4-336e-4d8d-98a8-db012b64cf94)(content(Whitespace\" \
         \"))))(Tile((id \
         de7b88ee-7aa8-4b6b-8ade-36a7369988b7)(label(=))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 39))(sort Typ))((shape(Concave \
         39))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         cff32faf-02da-47e9-bad5-a17a141dbe35)(content(Whitespace\" \
         \"))))(Tile((id \
         bc721fab-e5e4-4207-ad20-feb087eb6db4)(label(Emoji))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         c6b417cb-4a12-492d-b260-a053c880936f)(label(,))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 47))(sort Typ))((shape(Concave \
         47))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         13e91558-37d4-4a24-9fd6-42e83e83c6c5)(content(Whitespace\"\\n\"))))(Tile((id \
         f0a7fef3-ba75-4d52-9e13-bfd575e979bf)(label(palette))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         3ff19345-f758-4108-bbf2-f25950046ae6)(content(Whitespace\" \
         \"))))(Tile((id \
         70c3ef23-3ea0-41b1-aade-6e543ea3fc41)(label(=))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 39))(sort Typ))((shape(Concave \
         39))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         99eb596f-d70e-40ab-9a49-dfa9f0b213fe)(content(Whitespace\" \
         \"))))(Tile((id 95fa4d13-c1ed-4164-ad8a-5e7190592078)(label([ \
         ]))(mold((out Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape \
         Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
         571689a0-09fb-4905-b6c6-e0e80ea3de89)(label(Emoji))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children()))))))))(Secondary((id \
         9ffbe76e-59c1-4ddd-9459-d24ccd2622ba)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         d5190197-8e8d-43ca-9b9a-4e6ffba6cc56)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         4936910c-30a5-486f-b3f4-db1d59ad5fc2)(content(Whitespace\"\\n\"))))(Secondary((id \
         5e535c51-a592-482d-9505-7b6e88f65b97)(content(Whitespace\"\\n\"))))(Tile((id \
         34ce8171-fb86-482e-ae52-c6c0b14d1540)(label(type = in))(mold((out \
         Exp)(in_(TPat Typ))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         40))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         54e8cc9d-869d-4580-b2af-bde9c6f9d5ae)(content(Whitespace\" \
         \"))))(Tile((id \
         f4b64bb6-d9de-4b21-9cba-87996f065be3)(label(Action))(mold((out \
         TPat)(in_())(nibs(((shape Convex)(sort TPat))((shape Convex)(sort \
         TPat))))))(shards(0))(children())))(Secondary((id \
         39851a4c-1b72-4675-81be-b5d70993ef7a)(content(Whitespace\" \
         \")))))((Secondary((id \
         699de733-933d-4b14-a1ca-179ed7b8a744)(content(Whitespace\"\\n\"))))(Tile((id \
         265b5dcc-2577-4ca3-b22a-379eca8daa1d)(label(+))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape(Concave 33))(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         2ea9724a-88b5-4f32-953b-13cce6a7ba00)(content(Whitespace\" \
         \"))))(Tile((id \
         f96819aa-ea5f-4720-9830-bbe13e598512)(label(SetBrush))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         a0130475-0783-4763-9f2a-562a4ffc878c)(label(\"(\"\")\"))(mold((out \
         Typ)(in_(Typ))(nibs(((shape(Concave 11))(sort Typ))((shape \
         Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
         a391e78d-80fb-4b59-9071-8d9655832830)(label(Int))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children()))))))))(Secondary((id \
         7a9485fd-98b8-44d6-8893-6806d2926c89)(content(Whitespace\"\\n\"))))(Tile((id \
         444ae068-ca64-4625-a5e6-4e825644d9cc)(label(+))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 12))(sort Typ))((shape(Concave \
         12))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         fea1db6f-aa8b-48c2-8e18-6c5b70b71cdb)(content(Whitespace\" \
         \"))))(Tile((id \
         a47fc09f-0a2a-4f30-ad4a-584855c2d549)(label(PaintCell))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         bc6f7bd6-2a17-4685-b370-f3808b57b75c)(label(\"(\"\")\"))(mold((out \
         Typ)(in_(Typ))(nibs(((shape(Concave 11))(sort Typ))((shape \
         Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
         97401b9f-5914-49d3-b88d-72f4bdc46ed2)(label(Row))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         07416a85-c4fb-4bac-9a7f-af08fc00505e)(label(,))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 47))(sort Typ))((shape(Concave \
         47))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         dbd3aa92-2148-4a45-8ee2-e64fcf1e4fdd)(content(Whitespace\" \
         \"))))(Tile((id \
         9b1fafe4-acba-4d35-bf88-90a8dede3691)(label(Col))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children()))))))))(Secondary((id \
         5b3193f7-1516-461d-bb3e-f740f8ac4448)(content(Whitespace\"\\n\"))))(Tile((id \
         97b52447-1972-407e-b39c-c081123a2983)(label(+))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 12))(sort Typ))((shape(Concave \
         12))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         73df4501-3b89-462b-8e8e-4c3f0cf9eafb)(content(Whitespace\" \
         \"))))(Tile((id \
         c8ebe0a1-a4fe-436d-8be9-6d4311944903)(label(ClearCell))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         cff08437-da28-487c-940a-864076167aad)(label(\"(\"\")\"))(mold((out \
         Typ)(in_(Typ))(nibs(((shape(Concave 11))(sort Typ))((shape \
         Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
         a33030a0-ef54-4b24-a413-797e8ff28bf8)(label(Row))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         5ff57935-21a3-4316-9e80-56753b69c120)(label(,))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 47))(sort Typ))((shape(Concave \
         47))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         48ea4ede-a363-4b29-a43d-f0a6ac261e75)(content(Whitespace\" \
         \"))))(Tile((id \
         6e0721c8-2ebc-40c2-8275-750d5ab0d6bf)(label(Col))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children()))))))))(Secondary((id \
         28d43219-75d6-43b5-9e84-62c99a0ac834)(content(Whitespace\"\\n\"))))(Tile((id \
         934a520d-fadf-4ae6-8e65-84da7fc055b1)(label(+))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 12))(sort Typ))((shape(Concave \
         12))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         6697edb6-e39b-47ef-bc40-38b2606cdfdd)(content(Whitespace\" \
         \"))))(Tile((id \
         1f632341-306b-4285-a364-f7dfee1958a3)(label(ClearGrid))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         1dcbc3a8-6740-4f81-9d69-9da57aeaf240)(content(Whitespace\"\\n\"))))(Tile((id \
         bd1a547d-7895-4bc8-820e-3d973f5afa1c)(label(+))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 12))(sort Typ))((shape(Concave \
         12))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         6843ef38-3cdc-438a-909e-ed28159f5c3e)(content(Whitespace\" \
         \"))))(Tile((id \
         03599964-88ca-4e19-b3b8-44d133169391)(label(PaintRow))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         bd08ebe6-67fc-4f34-ba23-5faea4ed4291)(label(\"(\"\")\"))(mold((out \
         Typ)(in_(Typ))(nibs(((shape(Concave 11))(sort Typ))((shape \
         Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
         581e3087-cb84-4254-a55b-1f9f06524070)(label(Row))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children()))))))))(Secondary((id \
         d7269ea8-f4fc-41be-a189-278553939b96)(content(Whitespace\"\\n\"))))(Secondary((id \
         aacf30b5-e182-454c-b05b-068eb498138e)(content(Comment\"# TODO: Add \
         PaintCol(Col) here #\"))))(Secondary((id \
         9e92ac3e-a6d8-4df7-8065-d815c9527067)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         ff6738f2-9949-4a6f-b82d-5fb7dd502c04)(content(Whitespace\"\\n\"))))(Secondary((id \
         a7874de5-276d-4473-8326-18b7fb08139b)(content(Whitespace\"\\n\"))))(Tile((id \
         a54d21f3-697d-4186-804d-f8a216261e3c)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         40))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         505e8de1-65dd-4929-9f75-5a2ac70f1efa)(content(Whitespace\" \
         \"))))(Tile((id \
         55bf1eee-eb4f-409e-ad9e-ab02b05e16ac)(label(init))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         98e1db6e-fbea-424d-b265-0aed7b5cdf15)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 24))(sort Pat))((shape(Concave \
         24))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         740ba6d7-24b4-4bae-8a41-11fc15d045f5)(content(Whitespace\" \
         \"))))(Tile((id \
         7bd018a2-45bf-48ab-aec7-9fecffe13595)(label(Model))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         b23557d4-081f-4b66-a2f7-fb056b583352)(content(Whitespace\" \
         \")))))((Secondary((id \
         425e9b74-c72f-4d9a-8e85-61651ad05385)(content(Whitespace\" \
         \"))))(Tile((id \
         9b290b04-68a4-48e9-b511-ba9a6404a9c6)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         981299a9-9b6d-4d43-b532-f586af0f15ba)(content(Whitespace\"\\n\"))))(Tile((id \
         07355888-68c0-4ffa-8861-d21b4a97b8fb)(label(canvas))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         0b40701c-5283-4c7d-9724-b2314b6d44fa)(content(Whitespace\" \
         \"))))(Tile((id \
         95851616-43d2-4cd7-afa2-f93a44750901)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         a50a1652-a4b0-46e4-a58d-6912ecb79c6d)(content(Whitespace\" \
         \"))))(Tile((id 02ba08a1-41da-4671-9784-5ed7b028cbca)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         581e679e-cc8f-40a2-814a-0705a218d67d)(content(Whitespace\"\\n\"))))(Tile((id \
         25fe29f9-388f-4311-9d61-17a0e80b37e9)(label([ ]))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         137fedec-9eeb-4a46-af64-cb397753083e)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         a4ff7b12-efb2-4eb9-b912-602046a98298)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         7ad03c74-4fcf-4221-a896-6534b7df7282)(content(Whitespace\" \
         \"))))(Tile((id \
         09f62559-7a27-4b99-9c92-0c07a91c6726)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         c75329a9-77c9-49ba-b4c5-7897e54c1d24)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         4ead3313-0fa1-479c-b86a-da00585d37c4)(content(Whitespace\" \
         \"))))(Tile((id \
         e2487eb7-9328-434e-be58-f0c19d13552e)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         7acb4a1e-babb-4191-bb18-7c7bdb88c6c8)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         8156a46d-decf-4c50-9f2a-e58ef7627bdc)(content(Whitespace\"\\n\"))))(Tile((id \
         0615a25a-c7a3-4397-909f-4f7210c9fb6c)(label([ ]))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         446b6609-4e4a-4f16-ade0-bef67b568507)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         d7036a85-8065-488e-b473-79eba027cf4c)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         edde091e-e0b8-4db9-9f73-ede8446b4f83)(content(Whitespace\" \
         \"))))(Tile((id \
         221aaeba-2567-42c3-8e49-6535f5c65591)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         03db230c-425d-4673-9190-78acc8ef33f9)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         3943926f-96ae-462e-af9a-679ddb5ff9a2)(content(Whitespace\" \
         \"))))(Tile((id \
         fdfc56c3-77ef-4ed4-a058-41d3fc3b115d)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         edacdd97-6d4a-4f31-8e1a-ae194b40f1bd)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         07fa8f9e-c2ef-49c3-af75-ce151e60ff0e)(content(Whitespace\"\\n\"))))(Tile((id \
         fa67a427-4219-48b9-8c7d-47b05485575f)(label([ ]))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         94feeffb-1147-4301-8b33-d97da6b2b4c0)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         15f6db45-7e00-4bb0-928b-4678be9fcce7)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         0e5a733c-3380-4a86-a2cc-225d655dac6b)(content(Whitespace\" \
         \"))))(Tile((id \
         92475571-f8ac-4602-8cb2-439a051748ef)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         58b52eb3-9885-4638-a574-9676b9b84067)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         3d070145-674b-40b3-9fb6-e520a3a05ecb)(content(Whitespace\" \
         \"))))(Tile((id \
         3a4676bf-d30b-4e59-9dd2-d4266e2fd4a8)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         7d8408a7-c037-4911-a7f9-d947c316183f)(content(Whitespace\"\\n\")))))))))(Tile((id \
         fec422f0-c296-4271-8f01-001be7aeb436)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         f1b4e498-299a-4278-a008-2e0f77c35895)(content(Whitespace\"\\n\"))))(Tile((id \
         e2ec26e6-bccb-414c-bc45-f4efc96da683)(label(brush))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         4b788a1f-14da-4ea3-b0a1-2f1836ff19e9)(content(Whitespace\" \
         \"))))(Tile((id \
         0d118bd8-b28f-431e-8003-c57a69419cad)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         1871656e-2e21-415b-af2d-9ccc8943b3d1)(content(Whitespace\" \
         \"))))(Tile((id \
         b71cff8d-51e9-4e65-aa74-d0c880622865)(label(\"\\\"\\240\\159\\142\\168\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         a1842fcb-822a-4c80-b22f-d0831db68ed4)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         f15cde6b-ca08-4961-aab6-c6c4e7610c75)(content(Whitespace\"\\n\"))))(Tile((id \
         0624b2b8-e26c-4501-ae51-3966da94e3f2)(label(palette))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         a55cf19a-c5db-4de6-bacd-3c536892a3fc)(content(Whitespace\" \
         \"))))(Tile((id \
         f02e23ef-7500-4c0a-8421-b9440a00cfad)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         b4e99cc0-97a2-4690-8f31-165eb1682beb)(content(Whitespace\" \
         \"))))(Tile((id 3c9dc216-438a-4e5a-b969-ac84fc928a69)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         1dc82f26-d70f-4b90-8bb1-6ffd5ba46a62)(label(\"\\\"\\240\\159\\142\\168\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         0fbb5698-c4c9-4092-b391-db71590de946)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         2a305954-e58a-4ab2-95ef-29b9ca9332a8)(content(Whitespace\" \
         \"))))(Tile((id \
         af212262-b200-49ae-82ff-a4a8a1d1f7de)(label(\"\\\"\\240\\159\\140\\159\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         b28a9d03-07df-4c6f-958b-527145ada1a3)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         b88a663c-1deb-48aa-b2e1-bc102bf02fde)(content(Whitespace\" \
         \"))))(Tile((id \
         4db28098-c7b3-45b7-9f49-950c17530a09)(label(\"\\\"\\240\\159\\146\\156\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         04588ea6-b362-4447-acaf-55d5c2d39db4)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         bcc8114f-1a88-45ae-97a7-03dad98a3a1b)(content(Whitespace\" \
         \"))))(Tile((id \
         1b9363e5-7247-4d5a-9fd8-62b53b8aaf93)(label(\"\\\"\\240\\159\\148\\165\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         5610f7c0-0461-4682-a8f6-a8340ec5f079)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         1629d0d8-5a00-47b6-b8c7-df69394eecc9)(content(Whitespace\" \
         \"))))(Tile((id \
         a98289f2-4389-4a74-903a-c1e29b54858d)(label(\"\\\"\\240\\159\\140\\138\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         d7a0399c-dc15-4638-a7de-9ae9e5ab6509)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         863a4573-661d-47e0-9b93-07cb100cd393)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         2361c63c-ebdb-4ee0-96c8-5762437479fd)(content(Whitespace\"\\n\"))))(Secondary((id \
         81ffdad3-c014-4103-8cfa-111da36b4178)(content(Whitespace\"\\n\"))))(Tile((id \
         6651dc4e-a217-4e7c-a95b-6ef9d531863b)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         40))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         d4187542-fc0d-4d38-a1f2-c5a44e077c1b)(content(Whitespace\" \
         \"))))(Tile((id \
         03f5c971-4d26-4a5b-88ac-0884ed103f74)(label(setCell))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         e6a35728-1841-4d91-b2e0-94e45983efdc)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 24))(sort Pat))((shape(Concave \
         24))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         4f702329-d1df-483d-b34a-8a52787ec56f)(content(Whitespace\" \
         \"))))(Tile((id \
         8e17c49a-3ff2-45bf-a471-ede6de35d661)(label(\"(\"\")\"))(mold((out \
         Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0 1))(children(((Tile((id \
         ca304ccb-3101-461c-bea2-d7e11edc9ade)(label(Canvas))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         be4c8eb8-372f-4119-aae4-291f6e0d79c8)(label(,))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 47))(sort Typ))((shape(Concave \
         47))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         f5aa1f98-72b7-4473-9ab4-528ea76a9cbd)(content(Whitespace\" \
         \"))))(Tile((id \
         61b53c03-43d2-45af-843b-58beacc258a2)(label(Row))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         c58d77a3-fb6e-4b6b-a702-8bfbf5872981)(label(,))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 47))(sort Typ))((shape(Concave \
         47))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         b8a5c131-7380-41ec-a96e-a6d66c9ba9ee)(content(Whitespace\" \
         \"))))(Tile((id \
         e716f160-228f-46ce-8ef9-1298b94cdbb9)(label(Col))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         2ff6cdb6-d1fa-4461-806d-d9c3bda75f59)(label(,))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 47))(sort Typ))((shape(Concave \
         47))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         02970506-e931-482c-a103-307675cd69b1)(content(Whitespace\" \
         \"))))(Tile((id \
         0268cda7-4c0a-4d57-897c-cec8374e416a)(label(Emoji))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children()))))))))(Secondary((id \
         35290297-f9d6-46b9-b5fe-3b7d1298c744)(content(Whitespace\" \
         \"))))(Tile((id \
         414a02c8-6f67-48cc-86d8-a556a29e7fcb)(label(->))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 13))(sort Typ))((shape(Concave \
         13))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         81098289-1af1-4ee5-ad14-99a44e9c0f44)(content(Whitespace\" \
         \"))))(Tile((id \
         edb3baee-6454-4846-b515-cf0c9cc1a59e)(label(Canvas))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         75a324ad-73a6-4558-8ba4-659cbf108f76)(content(Whitespace\" \
         \")))))((Secondary((id \
         e7d14eb4-0830-4492-805d-4ee3edc00881)(content(Whitespace\"\\n\"))))(Tile((id \
         c8d22b12-390d-4680-a9d7-d51d60ddceaa)(label(fun ->))(mold((out \
         Exp)(in_(Pat))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         36))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         d3193309-ca01-44b5-aa90-566d3c092ccc)(content(Whitespace\" \
         \"))))(Tile((id \
         d3ac488b-0e84-4413-b7e6-45b159b21820)(label(canvas))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         c3bc998b-9e0b-42bc-8074-57be4294ec48)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 47))(sort Pat))((shape(Concave \
         47))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         f1dc79ad-dc86-4aa9-be07-e1db5c5a1719)(content(Whitespace\" \
         \"))))(Tile((id \
         6e3786fe-8e70-42ab-b6d1-b4d383843144)(label(row))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         ef4c62c6-792c-4cfc-ad0d-d4ca0c50d02b)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 47))(sort Pat))((shape(Concave \
         47))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         d291e2a4-8c6e-4593-bead-262623887faa)(content(Whitespace\" \
         \"))))(Tile((id \
         b0c99e77-cf69-47d5-aef5-c537ddc7a1dc)(label(col))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         feeda10a-0cd2-467f-baba-53b7039233ad)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 47))(sort Pat))((shape(Concave \
         47))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         e6e82e16-54b4-46aa-ad1d-3f1b99a8691e)(content(Whitespace\" \
         \"))))(Tile((id \
         787c91c8-a2e0-4c92-bcc5-70b533f09b29)(label(emoji))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         255118fa-4e53-4df2-afe7-e380c117931d)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         e21d20f8-0519-471a-aeb4-9913b4d2ce4d)(content(Whitespace\"\\n\"))))(Tile((id \
         b589de59-9f9f-4f27-9dab-db57e1cdadaa)(label(mapi))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         7a8059c1-2d47-4a0f-ba7e-33934cc15559)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         990fa095-f7a4-4678-af47-3f353e8375a6)(label(canvas))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         d8ad660d-785e-476b-851f-fab765045ec6)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         567a91b8-93f5-4ff7-9ef1-7099b12dce41)(content(Whitespace\" \
         \"))))(Tile((id b2576875-59ea-4c6f-9ec1-61a017826d2c)(label(fun \
         ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 36))(sort Exp))))))(shards(0 \
         1))(children(((Secondary((id \
         140f5622-f669-434d-b10d-54bd7e4ed695)(content(Whitespace\" \
         \"))))(Tile((id \
         df716cd6-d4cb-484e-b315-1d2442bf2a61)(label(\"(\"\")\"))(mold((out \
         Pat)(in_(Pat))(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0 1))(children(((Tile((id \
         bfa2f5ac-6d97-4cc4-ae27-bbdc495203af)(label(i))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         e6db4b13-b5f8-4651-9cb3-2dbb722bbca4)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 47))(sort Pat))((shape(Concave \
         47))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         502e1422-1cf1-4d6c-880f-4a0ec395d841)(content(Whitespace\" \
         \"))))(Tile((id \
         6ea092e1-fdc5-4e34-946f-811ab794be2c)(label(r))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children()))))))))(Secondary((id \
         89d56cea-26cc-4adc-8ca4-92fe26d0c1b8)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         905b6b16-1921-470b-aee0-d95d1328500d)(content(Whitespace\"\\n\"))))(Tile((id \
         0b56de3d-8230-48af-bbe0-a9b9bfe83c0b)(label(if then else))(mold((out \
         Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         ecea40c1-90eb-4955-a0cb-a759d5c37b50)(content(Whitespace\" \
         \"))))(Tile((id \
         74df074a-bd3e-4760-af94-a2ac3a627023)(label(i))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         c3f4df0f-c56f-4a53-aaa2-ca6035d71a6f)(content(Whitespace\" \
         \"))))(Tile((id \
         fb7cb977-a109-4244-a409-d5efe5a9e1dc)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         8402bfd2-3be3-4b08-81cf-78d6e3057e8f)(content(Whitespace\" \
         \"))))(Tile((id \
         bd5dd8e6-3c0e-4ebd-b638-376bacef7c07)(label(row))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         35487f87-958a-46e0-b6f1-95e8bf057e3a)(content(Whitespace\"\\n\")))))((Secondary((id \
         c21291f4-da0e-4f57-96b2-9a314787a19e)(content(Whitespace\" \
         \"))))(Tile((id \
         0c1dcfd7-d9bb-4c10-b049-1e406c6afaec)(label(mapi))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         34e1417c-6545-4c21-8def-210761b16686)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         ce379013-3440-497f-93a3-2c832322da6f)(label(r))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         855114cf-5d1a-4477-95fc-4652e190c387)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         067ec375-5a8e-45f7-ad9f-b7911c61547c)(content(Whitespace\" \
         \"))))(Tile((id 4b54a2cd-e947-4ada-abf2-5634faa88965)(label(fun \
         ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 36))(sort Exp))))))(shards(0 \
         1))(children(((Secondary((id \
         883909d4-9a96-49b2-a33f-40cd90532742)(content(Whitespace\" \
         \"))))(Tile((id \
         b0edfdfd-1963-4cb9-bcc3-7f0f217dc570)(label(\"(\"\")\"))(mold((out \
         Pat)(in_(Pat))(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0 1))(children(((Tile((id \
         0e1b5a19-1d8c-4042-9ae2-f61516469864)(label(j))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         cbd51b22-d9a5-4aa7-b350-9935386d2ce0)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 47))(sort Pat))((shape(Concave \
         47))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         143c0c46-a328-4e36-bcde-323d58156c8f)(content(Whitespace\" \
         \"))))(Tile((id \
         a3fd7031-9c0d-4b20-bda9-0f4b540fc3be)(label(c))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children()))))))))(Secondary((id \
         c98ff1ac-d2c9-4961-912b-9e491918c1f9)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         7c4d249f-f58a-4f40-b11a-f55a84928443)(content(Whitespace\" \
         \"))))(Tile((id d8feb29b-c3b4-4a56-b4ab-6f3249bc82fc)(label(if then \
         else))(mold((out Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 35))(sort Exp))))))(shards(0 1 \
         2))(children(((Secondary((id \
         a74224fd-e0fc-4d6b-a896-f5ea6f9174eb)(content(Whitespace\" \
         \"))))(Tile((id \
         3bb23d94-6d09-4f99-9e30-615bf1a132b9)(label(j))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         f4236e3c-85e5-4891-9964-649d34241107)(content(Whitespace\" \
         \"))))(Tile((id \
         844551da-a64c-4800-afc4-03656340b7c1)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         050897a1-ae12-4448-b751-fb8c5eafb095)(content(Whitespace\" \
         \"))))(Tile((id \
         0e30be63-a68b-44b2-89a2-3bcd85b69dc4)(label(col))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         a3cccad6-97b0-4816-8ae2-1dc66c636b9b)(content(Whitespace\" \
         \")))))((Secondary((id \
         c3e350d0-8e43-4bd0-8466-7576bef30569)(content(Whitespace\" \
         \"))))(Tile((id \
         2a256488-1dba-4ade-9175-09e8eb9adc52)(label(emoji))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         a77fbdc4-aa82-4f8d-ad09-26c71d1ac7e3)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         8e22a105-a69a-410b-a019-7d12b88fd231)(content(Whitespace\" \
         \"))))(Tile((id \
         8d411128-23bd-45a9-a565-11061b402e43)(label(c))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         c0e18405-59c0-46fb-8a03-4719aaa083f3)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         ae08f0b8-1549-4614-b7c5-efaa9fd68f50)(content(Whitespace\" \
         \"))))(Tile((id \
         a218908d-4621-496a-96d5-e42b2bbc9aec)(label(r))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         1cbe6d1f-5330-4b87-b5b1-d56b8a6faf00)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         5565406d-e9c0-41d6-9cc2-93f2a28ae37d)(content(Whitespace\"\\n\"))))(Secondary((id \
         f78a23fa-b73b-4d12-b572-1eb93415171c)(content(Whitespace\"\\n\"))))(Tile((id \
         70faf3d5-f6ee-4959-8690-a327bdc60949)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         40))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         65c9d5ac-7990-4cef-96a3-260507efca7e)(content(Whitespace\" \
         \"))))(Tile((id \
         3bb639d2-985a-4f26-ad9c-55cb886d84c2)(label(setRow))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         f53dbb81-d600-4b51-b2eb-fe9d1a0ca89d)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 24))(sort Pat))((shape(Concave \
         24))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         3e5e7e31-10eb-4dea-ab12-e0df6db3148d)(content(Whitespace\" \
         \"))))(Tile((id \
         efcde562-225a-42c1-96bd-87cb4a4c6d55)(label(\"(\"\")\"))(mold((out \
         Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0 1))(children(((Tile((id \
         0f4c7025-2f1f-4f42-a5f4-8202d9349303)(label(Canvas))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         35eae896-308c-41f7-8d7e-f8a6666544b7)(label(,))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 47))(sort Typ))((shape(Concave \
         47))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         1db244cc-50eb-4d32-9bf7-831092641516)(content(Whitespace\" \
         \"))))(Tile((id \
         841bdaeb-0d66-4e7e-897e-8c98a894195a)(label(Row))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         e9277ab9-72f0-477f-8283-e4cf64b19424)(label(,))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 47))(sort Typ))((shape(Concave \
         47))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         676d9efd-bfe1-4c56-9aed-f291154c06c5)(content(Whitespace\" \
         \"))))(Tile((id \
         8399f067-edb6-4651-b6a2-d86339266392)(label(Emoji))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children()))))))))(Secondary((id \
         006d72ac-573e-4ff6-a7b6-6be4251c1333)(content(Whitespace\" \
         \"))))(Tile((id \
         17d2c47e-db1f-46ab-995a-7f853950b545)(label(->))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 13))(sort Typ))((shape(Concave \
         13))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         a527418f-bee1-4152-8e0d-fe087db321be)(content(Whitespace\" \
         \"))))(Tile((id \
         809eb2fe-0985-46bf-a5e4-98d2e1340641)(label(Canvas))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         ddaafcde-568e-42bf-af4c-ea0dcbce8a64)(content(Whitespace\" \
         \")))))((Secondary((id \
         f00c259c-24c2-4e74-91a4-96674de2f0f2)(content(Whitespace\"\\n\"))))(Tile((id \
         57135add-1f74-4585-8e9e-6cf8b0b4f73f)(label(fun ->))(mold((out \
         Exp)(in_(Pat))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         36))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         1970f7ab-79e6-46c7-a4af-d26d5991ce62)(content(Whitespace\" \
         \"))))(Tile((id \
         8ccb781b-b64a-4c74-a3f8-2309e5cc7232)(label(canvas))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         831dd027-f6d2-4a5b-b48a-e2718fde91ab)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 47))(sort Pat))((shape(Concave \
         47))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         02694004-0fdf-4504-af76-ee690359b59f)(content(Whitespace\" \
         \"))))(Tile((id \
         671df327-408b-408d-b6c7-b8b0376ed453)(label(targetRow))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         b701c1dd-73cf-4172-8cb0-8772e73877bd)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 47))(sort Pat))((shape(Concave \
         47))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         381cf7cc-4d97-41ca-9c3f-38051e036a48)(content(Whitespace\" \
         \"))))(Tile((id \
         9bc303d1-b77f-4155-abfe-872d5fd12e9f)(label(emoji))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         c93c2573-644e-40ba-8565-0e39c5f4750b)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         266eb1a8-921e-4433-8dad-9fd70ff433d6)(content(Whitespace\"\\n\"))))(Tile((id \
         767ee1e4-4306-4a79-81ec-2a790301144d)(label(mapi))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         730a0026-bc64-4c97-92da-bc4ff2090ed8)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         16507535-646d-4e08-a2ed-307ec12b474f)(label(canvas))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         1c78ff22-c273-433d-8e31-3d30224b6fd7)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         a09140b4-407a-4c9f-b03b-93f5eccbe9b6)(content(Whitespace\" \
         \"))))(Tile((id 4a10ff34-1706-4298-a88e-f71c059eb2fd)(label(fun \
         ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 36))(sort Exp))))))(shards(0 \
         1))(children(((Secondary((id \
         bc55b98a-7626-4ff3-9ac1-4d82ec0dcfae)(content(Whitespace\" \
         \"))))(Tile((id \
         ab785c4a-1dff-4f5f-874a-167159aa5288)(label(\"(\"\")\"))(mold((out \
         Pat)(in_(Pat))(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0 1))(children(((Tile((id \
         614afa1d-f789-487c-b730-6ca134160da3)(label(i))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         e794a87a-3f78-4857-8d9b-c9ada7e0f558)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 47))(sort Pat))((shape(Concave \
         47))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         ac862bd2-9c60-4a51-a74f-6694979279fa)(content(Whitespace\" \
         \"))))(Tile((id \
         88350b12-a145-4475-8822-cf96658f1042)(label(row))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children()))))))))(Secondary((id \
         5437458c-8d98-4117-b1fd-265e339bd325)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         2af7c999-07db-41b6-8047-6b53820261d7)(content(Whitespace\"\\n\"))))(Tile((id \
         751bfdd6-8657-4fb5-b392-ab6b808451c1)(label(if then else))(mold((out \
         Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         1e9baf84-0a79-41f5-9246-891d8110ed0d)(content(Whitespace\" \
         \"))))(Tile((id \
         bda77fe3-078f-4b6e-8b01-e1e8ae5587b9)(label(i))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         596c3b64-1cd3-4dd1-8d35-0396f640228a)(content(Whitespace\" \
         \"))))(Tile((id \
         9de03e18-bb77-42db-9397-c0e47b722a07)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         fc2f948f-a64c-4fe2-92d2-9bdefd978b95)(content(Whitespace\" \
         \"))))(Tile((id \
         4a55eb20-a719-43e4-8e17-1d0fd633be67)(label(targetRow))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         4599185b-511a-412a-afaf-4c2357398f5a)(content(Whitespace\"\\n\")))))((Secondary((id \
         b7be26b3-9383-4b2f-9ab1-83eb8d9fc6e6)(content(Whitespace\" \
         \"))))(Tile((id \
         18eb94d8-8807-4901-a1a4-d0195eca07ca)(label(map))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         bb63b187-6108-4ee2-a56c-7e6525d1dfc9)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         c4b6ae65-1125-4829-9593-169157d391ac)(label(row))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         f6afd4bb-edc2-4fe3-9612-81630659136d)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         e94180d9-172e-4fe4-b773-97e7c5648302)(content(Whitespace\" \
         \"))))(Tile((id 2856cbe2-ab94-430d-a2b9-ecb8033cfeec)(label(fun \
         ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 36))(sort Exp))))))(shards(0 \
         1))(children(((Secondary((id \
         05bc701c-9d2d-46ab-a621-6b4e1d2af7dd)(content(Whitespace\" \
         \"))))(Tile((id \
         77a405fa-34e4-4c96-aeb9-5221e5550079)(label(_))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         40ac893b-cb79-4569-9787-0de38805766f)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         e05d96c9-6f1a-4404-982c-354956468daf)(content(Whitespace\" \
         \"))))(Tile((id \
         f238371e-2663-4f0f-aa0e-e0a128295a91)(label(emoji))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         8d7f7aef-6215-49de-b810-46819b2f08e8)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         39ba1c8f-8042-4630-8576-b6cdd4cf2130)(content(Whitespace\" \
         \"))))(Tile((id \
         360a7b19-19e3-4bc5-ab1c-137e32a0fdc3)(label(row))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         bcbc8dcb-4f50-442b-9e8e-23f3e8d6da2b)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         38be4f6f-59bf-4a0a-b355-4df2a06c2260)(content(Whitespace\"\\n\"))))(Secondary((id \
         d049e693-6ba9-4abe-8df9-05da2293f1d8)(content(Whitespace\"\\n\"))))(Secondary((id \
         b603bf77-7a00-4795-ada7-81a380129589)(content(Comment\"# TODO: Add \
         setCol helper here #\"))))(Secondary((id \
         5d250b79-a0f1-4f18-9e7d-07cd20edeb90)(content(Whitespace\"\\n\"))))(Secondary((id \
         42f4c1a2-1644-4b92-b815-79248bf927aa)(content(Comment\"# Hint: You \
         need to modify each row, changing #\"))))(Secondary((id \
         9e2c1228-93fe-4011-b98e-fbec380390d0)(content(Whitespace\"\\n\"))))(Secondary((id \
         885aa649-7012-4d96-ad6b-147e30525cce)(content(Comment\"# only the \
         cell at the target column.         #\"))))(Secondary((id \
         a965f05e-ca82-4aed-b186-d729f5205102)(content(Whitespace\"\\n\"))))(Secondary((id \
         d37742c4-923d-413d-8114-c72037fab3b5)(content(Whitespace\"\\n\"))))(Tile((id \
         a77c877e-462f-4812-b1cc-798ed3e05368)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         40))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         df117715-20f8-4447-a15c-ef48703dd996)(content(Whitespace\" \
         \"))))(Tile((id \
         58dc677b-3238-49b8-b236-71c58040a5cb)(label(setAll))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         75c828d8-3952-42cd-81f6-60cc3ccbb9d5)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 24))(sort Pat))((shape(Concave \
         24))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         06aa22f7-6c5a-4184-adb2-94acd8b42dad)(content(Whitespace\" \
         \"))))(Tile((id \
         9310a82b-1d8b-4860-8dcd-9f520aa04914)(label(\"(\"\")\"))(mold((out \
         Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0 1))(children(((Tile((id \
         8e2eb001-defd-4822-8fc8-e1375d9c4d79)(label(Canvas))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         3881e437-a8d6-4056-8a16-e04e19d03179)(label(,))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 47))(sort Typ))((shape(Concave \
         47))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         865898bf-473a-444f-9cb7-759c10e15b37)(content(Whitespace\" \
         \"))))(Tile((id \
         02be5667-15eb-4259-9270-9b9bd75d1b10)(label(Emoji))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children()))))))))(Secondary((id \
         85167380-2352-47a7-b991-837de82a1d81)(content(Whitespace\" \
         \"))))(Tile((id \
         61661c9d-44c5-4bfc-a692-102484d53e93)(label(->))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 13))(sort Typ))((shape(Concave \
         13))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         72f907b0-9aff-4cdf-83f0-d14e7bf10b0c)(content(Whitespace\" \
         \"))))(Tile((id \
         352934b5-db06-415a-b062-460b9ab128bc)(label(Canvas))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         c058c667-4102-479f-a997-0c4e44f86135)(content(Whitespace\" \
         \")))))((Secondary((id \
         b706a237-c08e-4ff1-b2bf-46069eed4b4c)(content(Whitespace\"\\n\"))))(Tile((id \
         103b999a-a806-457d-bb65-718e66730bef)(label(fun ->))(mold((out \
         Exp)(in_(Pat))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         36))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         e227304c-0617-4513-90ca-88ab056235f8)(content(Whitespace\" \
         \"))))(Tile((id \
         4751531e-1354-4986-8f5c-fee7abf51b68)(label(\"(\"\")\"))(mold((out \
         Pat)(in_(Pat))(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0 1))(children(((Tile((id \
         1822c3a7-7038-4fb5-adf0-2b721e1424f0)(label(canvas))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         37000ae8-cae7-4384-82f8-c9c9b90c3ac6)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 47))(sort Pat))((shape(Concave \
         47))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         80b8f783-5434-4064-8d9b-e5c6461c0369)(content(Whitespace\" \
         \"))))(Tile((id \
         214cd20c-53b5-41e8-94ad-3f32aa8d078c)(label(emoji))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children()))))))))(Secondary((id \
         e0f5eb7a-09c1-4f66-a1f7-36ebb63353a1)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         d32f1dc2-5fbe-4d33-bd01-6476a41bb731)(content(Whitespace\"\\n\"))))(Tile((id \
         1e582d5f-cf6a-4563-bd76-896ab68e6d00)(label(map))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         e7c5a844-4d00-4e27-a4f9-25388cd84523)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         99993f89-edfe-445a-9da3-590a87c6a40d)(label(canvas))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         85b09de8-f294-45f1-be53-1cda255471ac)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         67fcd76a-c94f-4feb-a86a-3a27dc3b9d33)(content(Whitespace\" \
         \"))))(Tile((id da5c3b9d-fe3a-4a6b-986a-98d42665856e)(label(fun \
         ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 36))(sort Exp))))))(shards(0 \
         1))(children(((Secondary((id \
         e5e6b27d-999e-4e82-98f6-6e55517e13fa)(content(Whitespace\" \
         \"))))(Tile((id \
         1ed6bb83-b546-4cae-9d72-8994eb4447d5)(label(row))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         73efb50c-aa0f-414e-99ab-325fd75c0807)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         2cfaa67e-157c-45ac-bb69-3b709547c61d)(content(Whitespace\" \
         \"))))(Tile((id \
         e593107a-caae-451f-bac9-2cf6dfe5349e)(label(map))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         1b304f49-0fd6-402f-9e89-dc3a7c0eb23c)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         258f7a95-a0b4-4c71-bfca-64a745597058)(label(row))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         2deb65b3-920f-43b9-bc26-b7d32f528ef0)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         46b9818f-69e4-450e-9b02-75cbbcaf398f)(content(Whitespace\" \
         \"))))(Tile((id 1ccfd4c7-984a-40b5-977f-8618e1635325)(label(fun \
         ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 36))(sort Exp))))))(shards(0 \
         1))(children(((Secondary((id \
         82ddc980-390c-41bc-a4c5-b16e4da196a0)(content(Whitespace\" \
         \"))))(Tile((id \
         efd91550-3e09-4fa5-badb-6cb91b5cf1d1)(label(_))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         2f89f6f7-763a-448e-8877-5eb42e45cbb1)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         fbcd37eb-6e54-4d17-b096-e53f2c4ebac5)(content(Whitespace\" \
         \"))))(Tile((id \
         9f0d8415-970c-43d5-ad76-5d6efa9506a3)(label(emoji))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))))))))))))(Secondary((id \
         d2c3282f-5ef4-427d-bb49-3c918f4c0789)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         021ea77e-ac47-4059-81d6-9dc2c990971c)(content(Whitespace\"\\n\"))))(Secondary((id \
         aa5519de-283e-4d00-81c2-b0442f598b42)(content(Whitespace\"\\n\"))))(Tile((id \
         e00019de-b653-438d-bf9e-900bed1409a6)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         40))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         c092307a-77c5-4c62-84ff-b8d8d89e621f)(content(Whitespace\" \
         \"))))(Tile((id \
         13e385e6-2284-4ef6-8f2e-91410bf92715)(label(updateGrid))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         620cb3cf-c00f-490e-b810-f3baf321faaf)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 24))(sort Pat))((shape(Concave \
         24))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         848bca06-895f-4b90-8f90-ecdf9fce736e)(content(Whitespace\" \
         \"))))(Tile((id \
         80537173-5498-46aa-80e0-7d249b6c1b39)(label(\"(\"\")\"))(mold((out \
         Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0 1))(children(((Tile((id \
         a1a21964-34f2-47bd-a3ba-2e3edd152d8a)(label(Model))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         2bce319a-0cae-47f6-8f84-ad29bbc90e09)(label(,))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 47))(sort Typ))((shape(Concave \
         47))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         dbfdb66d-fd20-4358-bbc5-3af0870e2ee3)(content(Whitespace\" \
         \"))))(Tile((id \
         3fde885b-939b-40b1-9ad7-d5f510d83967)(label(Canvas))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         2e75475f-d5e8-44be-8bd2-dc388296f25b)(content(Whitespace\" \
         \"))))(Tile((id \
         9779da36-4305-4583-83ab-69623cd12f94)(label(->))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 13))(sort Typ))((shape(Concave \
         13))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         171c9ffd-c068-4c0f-85b8-2854a1bb4cc5)(content(Whitespace\" \
         \"))))(Tile((id \
         bfc56931-9356-4d1e-9f97-8cec9b0fa918)(label(Canvas))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children()))))))))(Secondary((id \
         69cc089a-af5e-4a94-bd26-1a5f35fad1b0)(content(Whitespace\" \
         \"))))(Tile((id \
         b5b551d5-7457-492f-b479-c9b83e55da4d)(label(->))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 13))(sort Typ))((shape(Concave \
         13))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         950869f8-2824-4632-bb54-4f718cc0ef71)(content(Whitespace\" \
         \"))))(Tile((id \
         2d45635b-afe6-40e8-bbf7-6c06a616f94e)(label(Model))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         a4d56014-2d42-4778-9b0b-376a20f07408)(content(Whitespace\" \
         \")))))((Secondary((id \
         b2854237-62d9-4f3c-a28f-98077782f8f1)(content(Whitespace\"\\n\"))))(Tile((id \
         0389a923-0ed5-4dbe-8f3f-da0497c070cc)(label(fun ->))(mold((out \
         Exp)(in_(Pat))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         36))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         d5aaceeb-b341-448c-a2c9-0c3db608ed39)(content(Whitespace\" \
         \"))))(Tile((id \
         ee7ee7d7-f479-4782-a866-5de3a793ec85)(label(\"(\"\")\"))(mold((out \
         Pat)(in_(Pat))(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0 1))(children(((Tile((id \
         e921b2a1-b243-4711-9327-8e42194f92a1)(label(m))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         b1b6728b-35a5-459b-b552-ec250c45d649)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 47))(sort Pat))((shape(Concave \
         47))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         7d399597-eccb-408b-9a2b-79c52846186a)(content(Whitespace\" \
         \"))))(Tile((id \
         287971d6-2cd4-436b-939b-e7af88983a63)(label(f))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children()))))))))(Secondary((id \
         3ba5b674-ef7a-4e2d-9655-6f5ab147cce5)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         36a6b1e6-26da-4b0c-a728-8121c5733551)(content(Whitespace\" \
         \"))))(Tile((id \
         734b75a1-b4c5-4a47-9bc6-b786b833cf2d)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         cb791bee-51be-41ee-ba0f-e7015d65db22)(label(f))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         67cfa872-04ad-4b3e-a607-d76a789a7fb1)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         112a54e1-fecc-4514-b81c-b70c040c419b)(label(m))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         9e5a3760-af27-4214-9fde-19de518c4b07)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         b6bbd6c2-a121-4f33-acb9-7c3f967b0b8c)(label(canvas))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         c174e197-9a16-491e-94a6-0fe3876a6707)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         c013f1d4-65e9-4e89-afee-3e00f860b82d)(content(Whitespace\" \
         \"))))(Tile((id \
         91756ddd-fc0a-48be-aa86-659bdfd121af)(label(m))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         7aa28051-ba4a-443a-8b4a-7689834f969f)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         636a0b79-50a0-4524-a7da-4a28ab8f0510)(label(brush))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         f526f39a-e578-40af-83a1-22658df02fbf)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         98f5c025-4451-4f62-ade8-8448bb78e2be)(content(Whitespace\" \
         \"))))(Tile((id \
         48706594-6129-458d-850f-b5208925fd5a)(label(m))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         48c94996-7ac5-4205-a702-ab8fe6054cff)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         b13ad5ac-b2e1-4e82-87f7-a30d9f9e045e)(label(palette))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         bc5439a6-5834-4f63-93f3-a77cab5499fd)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         97c9b6db-d269-4ebf-a0d7-92b9c6deab59)(content(Whitespace\"\\n\"))))(Secondary((id \
         11cd4641-e862-4901-a2bc-609beb320354)(content(Whitespace\"\\n\"))))(Tile((id \
         a2f41747-a5d9-497e-863b-a09c511bf6a9)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         40))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         d90f8e53-ba60-47c2-a107-5c581e5c3467)(content(Whitespace\" \
         \"))))(Tile((id \
         d5378c8a-60b6-4abc-abda-8a4ee598952a)(label(update))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         dd9179f6-d979-4a90-ad25-2119180f6681)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 24))(sort Pat))((shape(Concave \
         24))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         a4594bbe-7e3f-4866-83b2-6bcf4e0fc63a)(content(Whitespace\" \
         \"))))(Tile((id \
         66339ccb-3bd9-4071-81bd-a51cbb9276b9)(label(\"(\"\")\"))(mold((out \
         Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0 1))(children(((Tile((id \
         824cf221-d66b-4c70-9d97-19ea94853bcd)(label(Model))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         85c4a4c1-b34d-4f39-b8a3-03d69612f619)(label(,))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 47))(sort Typ))((shape(Concave \
         47))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         c41ebf5c-396b-41a3-922b-06df63ffff18)(content(Whitespace\" \
         \"))))(Tile((id \
         6bed0663-bab9-4ace-9c7a-92728ff9f2ef)(label(Action))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children()))))))))(Secondary((id \
         c0015cb1-dd78-4bed-b2a4-fd447213475f)(content(Whitespace\" \
         \"))))(Tile((id \
         a551f507-1744-4303-9488-bdf1201468bb)(label(->))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 13))(sort Typ))((shape(Concave \
         13))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         1da807c6-fdbd-4540-903b-27919cdf54d7)(content(Whitespace\" \
         \"))))(Tile((id \
         5944650b-73c4-4a79-b3e9-34d10b04ee93)(label(Model))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         35307932-951c-4fbe-93b2-af731e4e9587)(content(Whitespace\" \
         \")))))((Secondary((id \
         aefedd85-c4e2-46d3-af86-31ed24456e1b)(content(Whitespace\"\\n\"))))(Tile((id \
         abb063ec-84fe-4f06-853a-924b0cc98406)(label(fun ->))(mold((out \
         Exp)(in_(Pat))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         36))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         e49391e8-c2c0-494d-814e-d38e6a4abd6a)(content(Whitespace\" \
         \"))))(Tile((id \
         a04e9424-17d0-453a-92cc-b4218bea048e)(label(m))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         7e00e2fb-e106-405d-b062-d85a2bf06b36)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 47))(sort Pat))((shape(Concave \
         47))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         f87c6385-e991-4b27-83d7-6bc7c1fe99f3)(content(Whitespace\" \
         \"))))(Tile((id \
         5430d02e-1294-48da-a58d-6bcf44e35fef)(label(action))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         5462707c-ade6-4d3f-9d3d-432ecf831fcb)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         03a2d77b-35af-48c6-8a3e-16ed2ec8e877)(content(Whitespace\"\\n\"))))(Tile((id \
         5333be9c-9998-4b5b-b20a-bfe2d8f7a66b)(label(case end))(mold((out \
         Exp)(in_(Rul))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         fb8bc907-2071-4a61-b72a-da306f6b2cc8)(content(Whitespace\" \
         \"))))(Tile((id \
         32aa486b-9f60-44c7-a8d5-9f52f7603c80)(label(action))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         07a0deb6-49eb-4f9b-9222-d257c8bf8aed)(content(Whitespace\"\\n\"))))(Tile((id \
         6e9cfba0-01e6-4bb4-aff9-bf340e73e775)(label(| =>))(mold((out \
         Rul)(in_(Pat))(nibs(((shape(Concave 43))(sort Exp))((shape(Concave \
         43))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         efd9486d-377d-46a4-b2b6-a14acf3280f8)(content(Whitespace\" \
         \"))))(Tile((id \
         61ab304c-854d-48b6-b214-1101a77b2267)(label(SetBrush))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         701e54b4-337e-4fb9-a572-a186ab0c87e2)(label(\"(\"\")\"))(mold((out \
         Pat)(in_(Pat))(nibs(((shape(Concave 23))(sort Pat))((shape \
         Convex)(sort Pat))))))(shards(0 1))(children(((Tile((id \
         6cb661ff-5ba2-4edf-928c-b1460e92bed5)(label(idx))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children()))))))))(Secondary((id \
         0a0f227b-9a8a-403c-952a-255a4240b71f)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         69e9d7eb-34d4-4d8a-b79b-c4008869c6a4)(content(Whitespace\"\\n\"))))(Tile((id \
         1f2ce16f-521c-4e84-9df5-0a80160c55a9)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         b1ee3634-c884-4a18-8388-3d0c97e2ca02)(label(m))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         a2859b20-4776-4a71-a734-5996b8a7add3)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         a893fb72-631d-4c60-a3e0-7035033e25bf)(label(canvas))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         a1f83b26-4779-4ca2-ad63-41557bdaa5d6)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         ef8cbee2-2b68-47f3-9eb0-f389fa64b0fb)(content(Whitespace\" \
         \"))))(Tile((id \
         cbaaee89-8122-445f-aac7-90aa011c9814)(label(nth))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         3b513b1b-00c0-40b0-bdd2-aa2868f3aaf1)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         ccf1cc5b-c66d-45de-9cd2-cccff0306462)(label(m))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         c174d588-fa73-41d3-89cd-9a781e23a4e4)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         b6a3ee2f-c6ed-489d-94a3-f738189e5e13)(label(palette))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         0ede85cd-cad2-4eea-9416-2aee6f700ac8)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         bd34cc84-2983-45a3-a69a-5ec8150531b1)(content(Whitespace\" \
         \"))))(Tile((id \
         76eabb31-88a8-474f-9ced-2e11fcde913a)(label(idx))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         d1e6d80d-b389-478c-bd54-347901df0219)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         96381468-2ea3-4751-add3-92eb54942c6a)(content(Whitespace\" \
         \"))))(Tile((id \
         f05bfefa-43ca-48c3-a732-c426517ace3a)(label(m))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         5ae3eaab-c317-4ac7-9ae8-1b62b3e00e31)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         74447b05-dd6f-450e-a19a-5377e6cc6c28)(label(palette))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         391088bd-f1a6-42f6-9b06-de69134a8596)(content(Whitespace\"\\n\"))))(Tile((id \
         c3fcb314-56e7-4ff2-9b34-377dbd048bd6)(label(| =>))(mold((out \
         Rul)(in_(Pat))(nibs(((shape(Concave 43))(sort Exp))((shape(Concave \
         43))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         faf723f6-a2c9-4632-953b-7cf3f876e73c)(content(Whitespace\" \
         \"))))(Tile((id \
         a72c78b8-bd7d-4401-a28d-f52f9a2c77c4)(label(PaintCell))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         23a34036-d5ee-4cfe-8cc6-40bc48960403)(label(\"(\"\")\"))(mold((out \
         Pat)(in_(Pat))(nibs(((shape(Concave 23))(sort Pat))((shape \
         Convex)(sort Pat))))))(shards(0 1))(children(((Tile((id \
         4fe00973-97a8-4878-900c-24cdf1cc59e5)(label(row))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         649533f9-059a-497e-b78a-8f35a8f7f5b2)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 47))(sort Pat))((shape(Concave \
         47))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         b850f896-3214-4210-9f35-a7ecb8d14b0e)(content(Whitespace\" \
         \"))))(Tile((id \
         dd92e216-0233-4bb2-b874-54363fb0af8c)(label(col))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children()))))))))(Secondary((id \
         2f852123-7e61-4cc0-b5a2-6da9422c2519)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         96a50342-a02f-4279-8176-235a5619626d)(content(Whitespace\"\\n\"))))(Tile((id \
         59f27f36-c686-4802-8671-a3525d520da9)(label(updateGrid))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         3fe8a2fc-ed93-4f80-baaf-cc67bc866c56)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         75101e0c-45cf-4b1e-a974-843895431b82)(label(m))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         1c9db3d5-441f-4d3d-ad3a-e6c075ee6770)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         7fe5b990-6c20-46ed-b9e2-a601fa852213)(content(Whitespace\" \
         \"))))(Tile((id 9600acc6-c7d0-4860-885f-cca04cb2e481)(label(fun \
         ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 36))(sort Exp))))))(shards(0 \
         1))(children(((Secondary((id \
         3d48a417-15b6-42c0-adea-b381aa7c615f)(content(Whitespace\" \
         \"))))(Tile((id \
         7185e16b-3572-426a-9f2e-e8a979eb66e0)(label(c))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         ff2fc24f-9651-4490-b135-8f214546ead6)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         f852e9d7-653c-457b-aac4-2b518693ff9a)(content(Whitespace\" \
         \"))))(Tile((id \
         0a9bbdb7-8b82-4940-beba-c554ad84acf1)(label(setCell))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         70c5a02b-54ec-40b9-acc0-bb255097d160)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         e17efb2c-fd2a-4e69-976f-c4f8a8f43111)(label(c))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         6ebd29f4-51e2-45d2-8279-644f9c2fb01e)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         52aa291d-ab9e-40e8-ac22-689094a4acc2)(content(Whitespace\" \
         \"))))(Tile((id \
         8ea66c82-40d2-43ad-9217-fb090134b3e1)(label(row))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         84d4f45a-53ba-412f-a9e6-436923a2abb6)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         2a79fed7-509e-457c-bef9-ea0939e59d50)(content(Whitespace\" \
         \"))))(Tile((id \
         78ed0c9b-e644-4587-9245-37da4e3e003c)(label(col))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         74d2d83b-441a-45c7-a173-d54d08cb3df8)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         ae9303f8-f04e-4dad-93c9-e4c573eacd3b)(content(Whitespace\" \
         \"))))(Tile((id \
         922e3e31-084a-42a9-9d0c-22545146471a)(label(m))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         cb4066bc-703d-4ff4-9498-6cdf858d8d18)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         2c6f459f-d80d-4457-89d4-3fa3b02131ef)(label(brush))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))))))))))))(Secondary((id \
         6ca58697-e1b7-4625-8f14-70ccc77e66db)(content(Whitespace\"\\n\"))))(Tile((id \
         0c2ff6d4-4c88-4837-9f0c-5f2d4e84459c)(label(| =>))(mold((out \
         Rul)(in_(Pat))(nibs(((shape(Concave 43))(sort Exp))((shape(Concave \
         43))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         cbb2fa63-7915-47c9-8f74-0432ddcdb4ca)(content(Whitespace\" \
         \"))))(Tile((id \
         6595dd26-5667-451d-a64d-f3e52da086af)(label(ClearCell))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         ed590ece-052c-4784-a5a9-40bfe06599f0)(label(\"(\"\")\"))(mold((out \
         Pat)(in_(Pat))(nibs(((shape(Concave 23))(sort Pat))((shape \
         Convex)(sort Pat))))))(shards(0 1))(children(((Tile((id \
         b4115d06-cf52-4511-893d-400b5b90502d)(label(row))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         f4ca4985-ec3a-4187-9f3d-c22a979197da)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 47))(sort Pat))((shape(Concave \
         47))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         7381fe12-4df8-41d7-9180-ba58586b8615)(content(Whitespace\" \
         \"))))(Tile((id \
         b54370ac-57ab-4c94-8d63-d1bd992d9349)(label(col))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children()))))))))(Secondary((id \
         a654ed9d-9ab9-45dc-ba29-45131ea5f433)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         5e8a79a1-7407-4933-a8c2-8ebf01d2d958)(content(Whitespace\"\\n\"))))(Tile((id \
         49a22fe1-2b52-4fa6-ab97-c4a78aded3cf)(label(updateGrid))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         a180b963-dc30-4475-a374-5dd58697f557)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         bc317707-2264-44a7-a458-3c6d61578972)(label(m))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         9399b4f5-ed19-4fab-859f-225500804776)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         fe298ebc-9d21-4e7c-ade1-aaf4f4e5870a)(content(Whitespace\" \
         \"))))(Tile((id 8495174b-f57d-41c9-bbb9-a723d3eaca2c)(label(fun \
         ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 36))(sort Exp))))))(shards(0 \
         1))(children(((Secondary((id \
         43ffe474-2663-41fc-83fc-ebfa284506e8)(content(Whitespace\" \
         \"))))(Tile((id \
         d9a6b6d5-3c23-4fe4-b6eb-6c31ada0ae64)(label(c))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         3d2f10c4-b71c-490b-a1ca-a953a910e662)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         58038369-a25f-4657-8adb-6ebd4ac7cc34)(content(Whitespace\" \
         \"))))(Tile((id \
         63c001e0-2667-492f-b759-cb71fa6967bc)(label(setCell))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         79704c89-0094-4214-8a51-09fa4d67bfda)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         a34a79d0-3f8c-447d-aa4d-a4fdadfc3b7d)(label(c))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         bec123e4-3388-4794-99ff-13d76c6e44b7)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         9b80f9c2-780f-4040-a7d0-590ee43cbe31)(content(Whitespace\" \
         \"))))(Tile((id \
         5c06304f-13da-470c-8482-63b46c62c0be)(label(row))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         68f2e111-ae67-4d3b-afbd-b48bebeda5ed)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         332a0de4-58e0-4b7b-a76d-6c11f687ccb5)(content(Whitespace\" \
         \"))))(Tile((id \
         b158da87-ba06-43ac-99c7-d2e3132bd1ee)(label(col))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         a6f88e70-001d-471d-b943-96d4eb8ff4a0)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         18e90425-82a4-4c7c-ab24-9d169eb6898d)(content(Whitespace\" \
         \"))))(Tile((id \
         30f59add-768e-4fac-971a-77289c8bd610)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))))))))))))(Secondary((id \
         1194d139-01fc-40d8-8ee1-b6035236b819)(content(Whitespace\"\\n\"))))(Tile((id \
         030f9ca4-798e-48fc-a24d-4ca335e9e1c7)(label(| =>))(mold((out \
         Rul)(in_(Pat))(nibs(((shape(Concave 43))(sort Exp))((shape(Concave \
         43))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         e880d2a4-78eb-44f5-80e9-1fe3153180c1)(content(Whitespace\" \
         \"))))(Tile((id \
         0949cc21-bbf8-44af-a762-0cf92b15f0fd)(label(ClearGrid))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         418be0bb-7081-4b86-9a6d-1631888b3f8f)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         de8749d4-5c14-4d57-bc8d-91f03d46a06e)(content(Whitespace\"\\n\"))))(Tile((id \
         8624ab37-cc16-49f6-a3b5-5f15d8acba07)(label(updateGrid))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         d39c2c84-e4a7-4352-bf7d-084fd89345b8)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         f52430d8-5803-4c32-bc0c-a3f00f9ef2d0)(label(m))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         8c4a50f1-5fd1-4f92-b360-e2aae2493d3e)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         1d15b40e-38e1-4138-9a7e-69a077a8cad4)(content(Whitespace\" \
         \"))))(Tile((id a8c1f829-7e72-4063-ac0f-70c0512c4221)(label(fun \
         ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 36))(sort Exp))))))(shards(0 \
         1))(children(((Secondary((id \
         0cb436d2-7d23-4322-8000-2f47c5c6ffa7)(content(Whitespace\" \
         \"))))(Tile((id \
         9d0e83bb-4b28-4cf2-839b-d82ac55b5e41)(label(c))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         4a15292b-f677-4c82-b2d5-5680714baee1)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         edf880ec-9032-43de-a54b-f9633b4933fc)(content(Whitespace\" \
         \"))))(Tile((id \
         7f7cf9ef-a1e0-449c-96bb-b4c79554639b)(label(setAll))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         4e84f45a-0ff5-4c4a-8573-03f1c55970eb)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         70d0432d-9856-4df8-9c95-94b42474033a)(label(c))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         ce521750-a581-4290-a851-ba7b25d13dd8)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         f30a6ec1-ff74-49d5-811e-452d5723cbe4)(content(Whitespace\" \
         \"))))(Tile((id \
         dd235843-1a39-4294-9fbe-06cd4112ae1f)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))))))))))))(Secondary((id \
         c77c3115-d8c9-4058-abde-b4c7065191f4)(content(Whitespace\"\\n\"))))(Tile((id \
         6463b017-432c-46dd-a825-af19c47fa93c)(label(| =>))(mold((out \
         Rul)(in_(Pat))(nibs(((shape(Concave 43))(sort Exp))((shape(Concave \
         43))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         fdb29af4-5af6-4157-9226-0aa613eac256)(content(Whitespace\" \
         \"))))(Tile((id \
         951c4af7-cc9e-46a3-93a0-b10dc9661fd1)(label(PaintRow))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         0d60c28b-6c94-4027-8993-e4ba4e988882)(label(\"(\"\")\"))(mold((out \
         Pat)(in_(Pat))(nibs(((shape(Concave 23))(sort Pat))((shape \
         Convex)(sort Pat))))))(shards(0 1))(children(((Tile((id \
         544d845d-677c-4fe0-87f2-7457c237a9d0)(label(row))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children()))))))))(Secondary((id \
         3730083a-f108-4acb-9efc-c9876972a7d4)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         c7bf65e2-4352-4c96-8fa4-ce5f97b8d267)(content(Whitespace\"\\n\"))))(Tile((id \
         70a6a7f8-229f-4a53-8eae-38d10130e937)(label(updateGrid))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         41306350-5817-407a-8494-9d8a8d45cb61)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         525e6774-a3a2-4127-9c92-6a3790d49469)(label(m))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         8c10c39e-1537-409c-b345-76e2d18587e2)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         8dcfde7e-a7e5-427c-ad0b-3b0d5010b144)(content(Whitespace\" \
         \"))))(Tile((id da0f061d-9022-4025-8337-3b15a031d190)(label(fun \
         ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 36))(sort Exp))))))(shards(0 \
         1))(children(((Secondary((id \
         df322ab5-1d9d-4311-9989-0fbbbfde0c15)(content(Whitespace\" \
         \"))))(Tile((id \
         6965dc76-e386-49e4-97b8-bcd55de60512)(label(c))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         2801a250-b072-4eed-a439-9d0395a63964)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         7795c996-7589-4fc3-940f-fe4c47790e5d)(content(Whitespace\" \
         \"))))(Tile((id \
         de662ca3-b43f-47a1-88be-0418feab25b3)(label(setRow))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         ee5380cb-b51a-476e-bac8-19e637d73e10)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         e3a873a0-5dfb-4d77-a8cd-536af215864e)(label(c))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         779fad38-a56a-4e22-9816-353770020e42)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         c754f8c4-dd50-4ce0-b682-831db7ba6525)(content(Whitespace\" \
         \"))))(Tile((id \
         d2b9262e-df78-48f7-ad17-b1b753412b32)(label(row))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         dd34aabf-5ce9-4ad3-8975-0a0245c7ad2d)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         487b8326-11f1-4144-9215-fbac32b34789)(content(Whitespace\" \
         \"))))(Tile((id \
         9ad15808-c020-4a97-a3f9-49d2a984d17f)(label(m))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         835b56f6-0eca-48f2-adff-26b62e3babfe)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         f794063e-4cd0-4066-9b8a-91b142c639df)(label(brush))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))))))))))))(Secondary((id \
         ff7b52c4-700c-4020-a033-978b89098147)(content(Whitespace\"\\n\"))))(Secondary((id \
         5cd9ef8c-4f84-4d01-91c2-ec59f48853d0)(content(Comment\"# TODO: Add \
         PaintCol case here #\"))))(Secondary((id \
         723d9e4e-73ef-4d8f-a399-842161af0486)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         9b7fdedd-7d74-4a23-beba-16574030082c)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         5c6021f6-d2b2-4e6e-8feb-c314d2e20774)(content(Whitespace\"\\n\"))))(Secondary((id \
         cd2a2c69-83c5-427f-ac52-00a64290f69f)(content(Whitespace\"\\n\"))))(Tile((id \
         8b086474-934a-41fd-81e0-5043ed46d661)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         40))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         949dda8d-c686-4bf5-985d-af4650bcead4)(content(Whitespace\" \
         \"))))(Tile((id \
         ec149249-c6cc-4b89-bde5-9f670e267288)(label(do))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         96e47e23-33b4-47f0-a653-4c23e62ff0e7)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 24))(sort Pat))((shape(Concave \
         24))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         0749b287-04e0-48fd-bf26-11a9a54c5ab8)(content(Whitespace\" \
         \"))))(Tile((id \
         7a1b9de5-a06b-4eaf-aa15-0381730ba5b1)(label(\"(\"\")\"))(mold((out \
         Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0 1))(children(((Tile((id \
         f09bcacf-3c97-4608-b663-84449a294188)(label(Model))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         b0f647cc-2026-4bb4-9466-508ac1afa92c)(label(,))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 47))(sort Typ))((shape(Concave \
         47))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         8f88be56-c2e8-47ee-aa9a-6d7358ecc233)(content(Whitespace\" \
         \"))))(Tile((id 210e161e-9ac0-4d59-aa34-663c5a86ee6e)(label([ \
         ]))(mold((out Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape \
         Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
         8edc5937-b8b1-4a6b-a8b9-2e447d4410c2)(label(Action))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))))))))))))(Secondary((id \
         13e7cbe7-a77c-47b3-81cd-7deef7279591)(content(Whitespace\" \
         \"))))(Tile((id \
         27f527ca-3dfe-4b9c-96c3-dfb985c48360)(label(->))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 13))(sort Typ))((shape(Concave \
         13))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         ba43c961-19c2-4122-8937-39b79784f35f)(content(Whitespace\" \
         \"))))(Tile((id \
         fab598c8-0a6b-482e-b15a-8c096f508312)(label(Model))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         67a1bc09-92cd-4684-82d5-b9bc7fa0548f)(content(Whitespace\" \
         \")))))((Secondary((id \
         199e60fb-dbdf-4b70-8ec0-b08530e5c0c7)(content(Whitespace\"\\n\"))))(Tile((id \
         0f5e5161-5bf9-4b71-aa77-32936d07218a)(label(fun ->))(mold((out \
         Exp)(in_(Pat))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         36))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         b312065c-07f5-4746-a72d-8bc1283087cc)(content(Whitespace\" \
         \"))))(Tile((id \
         6345b137-d457-4845-972e-67bd81c3cbac)(label(\"(\"\")\"))(mold((out \
         Pat)(in_(Pat))(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0 1))(children(((Tile((id \
         036a6afe-3402-4550-854b-b63176e2025a)(label(init))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         3a22d6e1-f283-4a5c-ad1f-a80c3cd7b820)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 24))(sort Pat))((shape(Concave \
         24))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         b4ea2d40-44b7-401a-a9a7-709ca53d255a)(content(Whitespace\" \
         \"))))(Tile((id \
         afe244de-2e6e-427c-80a8-37f41de8b391)(label(Model))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         34444e25-a660-49ef-a895-30f2680d363f)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 47))(sort Pat))((shape(Concave \
         47))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         3cd26d62-386d-48f3-86b2-d8873417640e)(content(Whitespace\" \
         \"))))(Tile((id \
         616b4f97-a67a-44b0-a2a3-9342f9525edb)(label(actions))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         ad7bf443-c652-4415-bd09-06491ee067cb)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 24))(sort Pat))((shape(Concave \
         24))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         60bd4dee-4c9a-4cc0-baef-d4dc2cd7c24f)(content(Whitespace\" \
         \"))))(Tile((id c2c2b03c-a9da-4e8f-a8ed-25e29220bcba)(label([ \
         ]))(mold((out Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape \
         Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
         5caa5ae8-34e9-4b31-93b7-bd06800896bc)(label(Action))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))))))))))))(Secondary((id \
         41e18568-f346-453e-9061-11786ab59cf1)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         d18818df-45aa-42d9-a0c3-7a8562c793c4)(content(Whitespace\"\\n\"))))(Tile((id \
         b9119b56-3a50-4478-a9c1-7efa37f66036)(label(fold_left))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         dd771e99-c847-472f-ba9d-143284503271)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         0999a8d6-9965-4d5d-b47e-bcfdffc256a9)(label(actions))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         1a2e55ea-43c2-4b2c-8533-bfe8ece2f134)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         b507dda0-41e4-4979-8fb4-f5bd3fabee6e)(content(Whitespace\" \
         \"))))(Tile((id \
         b1bcc37f-27dd-4faa-b642-3182bb1d6add)(label(update))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         1f67a241-50c3-4d1c-a189-6628b02990f8)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         93e09075-5a78-4996-b358-e56f423f01f6)(content(Whitespace\" \
         \"))))(Tile((id \
         5a7d4b2f-d1b0-41fe-9543-2764509b1d29)(label(init))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         9197232f-50cf-4b28-9574-928b23f244ab)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         bbbc1e13-44b6-460b-872c-1a1541d457f1)(content(Whitespace\"\\n\"))))(Secondary((id \
         a908c7c6-2bc1-489f-828b-5480b06bfb79)(content(Whitespace\"\\n\"))))(Secondary((id \
         40702284-b284-4da4-ab3d-04ff7eaffe02)(content(Comment\"# Existing \
         tests #\"))))(Secondary((id \
         469490f4-92d2-43db-b6c0-357c3c373de5)(content(Whitespace\"\\n\"))))(Tile((id \
         2775950e-bad6-42c2-9c4d-35bf3b7f9999)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         676364a3-2209-4f4e-8ad9-0334c60eb9c9)(content(Whitespace\"\\n\"))))(Tile((id \
         6b76c22b-7dfc-45fc-ab53-9562570695c3)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         40))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         3830ea5e-01c9-486a-a466-98f9720e1327)(content(Whitespace\" \
         \"))))(Tile((id \
         c8d87f10-ff48-4043-806f-a2c9fd14b365)(label(m))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         537edf89-9148-457b-9e72-1c035e2d8b8e)(content(Whitespace\" \
         \")))))((Secondary((id \
         6ee7dbdb-14cc-4e8c-813c-a8333bcebb0b)(content(Whitespace\" \
         \"))))(Tile((id \
         de794678-32a8-4009-9824-c902fb5f818b)(label(update))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         b9badc62-7fca-4bca-9ef6-04a715e73be0)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         7fb5e1f0-1df2-48bb-b7c9-abca75141713)(label(init))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         56eb0611-7bb7-450a-9564-40aa9425948b)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         611e8c15-9419-4c76-b1e3-79140e0e8a6c)(content(Whitespace\" \
         \"))))(Tile((id \
         5dc5b078-ed6c-4de6-a756-b50edcf2dc53)(label(PaintRow))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         09233bdf-7206-40fc-a44f-b54f5b1c9f09)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         ced7d68b-979c-4919-b27f-9e4ef164070c)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))))))))))))(Secondary((id \
         1a9536d7-388e-4105-87b4-5e5961617b5c)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         4a185214-6e4c-4023-8943-34719e958417)(content(Whitespace\"\\n\"))))(Tile((id \
         ab400d08-f09e-41cc-92e7-b202d60499eb)(label(m))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         64dacd01-95d4-40e0-888c-956115d9ce1a)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         c7c609f3-b815-4aa8-820c-96fe87c525a1)(label(canvas))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         944e1956-ece6-4b31-b33c-cc5f752b6e25)(content(Whitespace\" \
         \"))))(Tile((id \
         3a88f598-db64-4c1c-9988-90d26c867566)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         bbae005b-b1ee-4786-aff8-3bc0308e60f9)(content(Whitespace\" \
         \"))))(Tile((id 7316d37e-2c7b-41f4-94d4-7752f7f5b33a)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         d8e11e09-732f-4515-ae49-25c2a4d4e18b)(label([ ]))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         a2adb1fb-1986-47e9-b520-437f08e16e72)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         675937a7-14c0-447a-8981-224899284d32)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         3af35f46-9d5e-4f56-8823-f609c16766de)(content(Whitespace\" \
         \"))))(Tile((id \
         cadc21bc-dc20-4a86-8f3d-1ffbe85eb5af)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         09854316-0674-4d6d-840f-d774221d186e)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         8a99f315-7bdd-4cfe-90e7-7236b3c2e597)(content(Whitespace\" \
         \"))))(Tile((id \
         76134eff-888e-4f70-8a31-85e525596d2f)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         5b9312f4-4a31-40b2-b3d6-203c8f4492e7)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         c21dd5b1-8dd2-498c-8ec6-904e4165e3cc)(content(Whitespace\" \
         \"))))(Tile((id 4298065d-7980-46e3-92fb-f022cd3a78be)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         da5f7b7b-6c45-4543-9d27-ba7fbb990a18)(label(\"\\\"\\240\\159\\142\\168\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         3c73bf09-b847-400b-bb1a-5da577c0fa7a)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         8aae33be-bbe6-4995-80a4-ba17ff93fab8)(content(Whitespace\" \
         \"))))(Tile((id \
         e9f0b2d6-b479-49b3-aaaf-c05e20abe987)(label(\"\\\"\\240\\159\\142\\168\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         58aaa9f8-12b6-470d-86c6-8c6e79813562)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         7a12d106-f9b1-4375-bbef-b942a765ce9b)(content(Whitespace\" \
         \"))))(Tile((id \
         302068ae-a001-4692-bd2d-ecd123235fb6)(label(\"\\\"\\240\\159\\142\\168\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         ff0cfc92-267a-49c3-86cd-431d871ec7fc)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         fbaef8c6-2c37-4fc5-9991-a5f067fcd3b0)(content(Whitespace\" \
         \"))))(Tile((id bdd08318-71e2-41b9-b576-493160f02aaf)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         354a3db4-0de0-4323-8510-44669b608db4)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         d0db1937-865c-412f-968c-e2476760bded)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         8a65c355-2819-4a9a-9b0d-7b44baf6e918)(content(Whitespace\" \
         \"))))(Tile((id \
         774b5846-051c-4fe7-a898-bc62a3deb7f6)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         ff0c5969-8e10-4140-880c-0aa0bfe0bf00)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         12c1cfae-6d7a-4088-b1e8-888624109afd)(content(Whitespace\" \
         \"))))(Tile((id \
         0ba1e95b-0c44-4aee-aecf-8b5e9b8fe7b1)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))))))))))))(Secondary((id \
         2078f15c-1653-46b7-89d9-35ee880089ae)(content(Whitespace\"\\n\")))))))))(Tile((id \
         13f7988a-adf2-4d6c-93e5-405ef9af9ef4)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 38))(sort Exp))((shape(Concave \
         38))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         6dde90e3-8a4c-43e4-85a6-f67f50ad02d0)(content(Whitespace\"\\n\"))))(Secondary((id \
         ee86c1bd-ac6f-4444-be76-5f13e4ac0c4f)(content(Whitespace\"\\n\"))))(Secondary((id \
         d62a0384-96d4-40c2-9e2a-954b4d04ad5d)(content(Comment\"# New tests \
         for PaintCol #\"))))(Secondary((id \
         fe835012-b9e5-4081-8cca-e7bf068af91e)(content(Whitespace\"\\n\"))))(Tile((id \
         7e02ecd2-7166-41ef-aa0a-eb9e80cbc899)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         5439b9a5-3a0f-44cc-8064-e2bee281e2ec)(content(Whitespace\"\\n\"))))(Tile((id \
         e2e46fb3-c971-4ad0-b7ad-54f94588541f)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         40))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         eaa0adc7-fc18-4fe8-a567-94e829fabdf2)(content(Whitespace\" \
         \"))))(Tile((id \
         afde7f3a-21d7-4d8a-a552-fa845b685295)(label(m))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         cfa6aeb0-88fd-42c0-935b-768d6cafa2e9)(content(Whitespace\" \
         \")))))((Secondary((id \
         6bfdf1c2-e0e8-476b-a04a-cbad022fb9cd)(content(Whitespace\" \
         \"))))(Tile((id \
         d3fa1bb2-d290-4e25-b0aa-bc8a4da90836)(label(update))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         5a59cc21-55d0-4667-b460-911bb95916e6)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         e4720080-55fa-4453-8cd4-2a11becf4450)(label(init))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         36b05135-e7fb-4a72-af1d-a14ec17634e7)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         3d08f9be-765d-42d6-8028-a17492e4a757)(content(Whitespace\" \
         \"))))(Tile((id \
         6ab27b39-8e8e-42c6-8b84-322ec0f95145)(label(PaintCol))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         33ea7b19-1044-44d6-8681-2d329d456cca)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         5bf6dba1-af3c-462f-bad4-5f7b4916bc3f)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))))))))))))(Secondary((id \
         cf432434-f1ad-4d91-8894-8839774f41fb)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         bdb7956e-9bd5-42b3-ab02-9903a25749a7)(content(Whitespace\"\\n\"))))(Tile((id \
         a3e89011-7b1a-4894-91eb-fe9800f39df8)(label(m))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         0bb756c4-90a4-4380-9d46-c7c321b2d8f2)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         3ff5bf5e-7e6c-4861-85d7-7831cc776795)(label(canvas))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         c78a7f76-fb58-407e-9ac6-5a4b132ae4fe)(content(Whitespace\" \
         \"))))(Tile((id \
         e4987724-349f-4160-9f91-7b5a8e563869)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         3357c0e4-212c-4cfc-bb51-343654a06ca1)(content(Whitespace\" \
         \"))))(Tile((id cf852997-4e10-4adb-931e-35644f031893)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         4f9f602d-ac99-47c2-8119-b365023bc627)(label([ ]))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         5ccad39f-893c-4c4e-9236-9e8fd2d8111c)(label(\"\\\"\\240\\159\\142\\168\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         5c86e26d-5c6a-4c05-8afb-12f3c009732a)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         46a8aeb7-1ba4-44db-8190-0b5b8dc19440)(content(Whitespace\" \
         \"))))(Tile((id \
         0895646d-6a79-4f74-b89f-4ccbdf76ff05)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         0b30415a-54ec-40f4-8d11-7f4e5c0546a8)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         1c1e3aff-811f-4373-a01f-0e640b7ef297)(content(Whitespace\" \
         \"))))(Tile((id \
         0af6e28a-3fe8-477b-bac4-c026744615ee)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         dd14a019-2bb6-4d86-8d21-8b13690c95d0)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         81acee36-498c-457a-a3d5-b615fba7eb15)(content(Whitespace\" \
         \"))))(Tile((id 5ae98df9-d2b9-4c58-b72f-6b77f22d3e98)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         f5c09e10-4d22-4fc2-9959-84ec0df5cd73)(label(\"\\\"\\240\\159\\142\\168\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         844cc09e-4674-4068-895d-40c07c80830d)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         59e6e727-52e9-4fa9-aeef-f0146e7d4c35)(content(Whitespace\" \
         \"))))(Tile((id \
         62a94783-e92f-47e4-b218-3b831b7da166)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         61777998-49a8-4781-bc92-7dd36afdf7de)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         ddd408ee-32ef-40cc-bfd2-2f85cda87de6)(content(Whitespace\" \
         \"))))(Tile((id \
         67fbdd0d-a994-4069-a418-cf18d1339bbf)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         3d262f46-f846-48f7-bc3f-690721820806)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         9d599b24-38ba-41b9-b2f7-b5470cb40f23)(content(Whitespace\" \
         \"))))(Tile((id d2bde5c2-26af-402f-93da-0b0b38d255e5)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         7a3dad7f-4a39-44f1-bde1-a40f5e10f2d3)(label(\"\\\"\\240\\159\\142\\168\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         3476215b-f08d-4421-8e33-5e457553f047)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         72a932f9-9d54-457c-9515-04137a3433ac)(content(Whitespace\" \
         \"))))(Tile((id \
         8f2a8a00-341d-4063-900e-56b78cb6bf21)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         2ac37a47-9ac2-4466-930c-499f59fa7539)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         299aa7a3-3513-4010-a1d5-614e2b1ae2ee)(content(Whitespace\" \
         \"))))(Tile((id \
         4caec912-b9b3-49a2-8d69-53f2a764689d)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))))))))))))(Secondary((id \
         a0cc4a57-1e7e-4e0a-8ed7-3cd458ff31ea)(content(Whitespace\"\\n\")))))))))(Tile((id \
         a14cf3d8-51f0-4b34-91ac-fcc829ff3258)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 38))(sort Exp))((shape(Concave \
         38))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         6389f3ec-93db-4611-9230-b55f9646cbb7)(content(Whitespace\"\\n\"))))(Secondary((id \
         0f4d73de-ba2d-4b82-be93-976f3537b1d3)(content(Whitespace\"\\n\"))))(Tile((id \
         4687b840-5456-40cc-82e4-b3c9709c2d2f)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         8026dd96-c8fe-4ea0-97c4-4a575a1d1887)(content(Whitespace\"\\n\"))))(Tile((id \
         f4853563-a9f2-46fa-b6e1-a326ed0542f7)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         40))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         83e8cfa5-99d8-4bfd-9b5e-309271efb937)(content(Whitespace\" \
         \"))))(Tile((id \
         7fbcdb45-0524-43ed-9cd8-a1a673806a5f)(label(m))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         f4bbfec1-508b-4b72-ab4a-e7bc39947e38)(content(Whitespace\" \
         \")))))((Secondary((id \
         6659338f-fc33-431f-8ee7-357b502c776e)(content(Whitespace\" \
         \"))))(Tile((id \
         9695cf92-a560-404a-afe3-fb7c8a2f4e6f)(label(update))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         3f864081-0c61-44a9-a5de-b156e22f2b4f)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         be4f5afe-e96b-4212-925a-33e0c65f094a)(label(init))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         898e79ea-971e-4726-8a2d-7d7892a8eb18)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         6c3171d3-ec56-47d5-86c3-a73e939d9053)(content(Whitespace\" \
         \"))))(Tile((id \
         2d82c8e1-e052-4ddf-a9b9-61e17df16c73)(label(PaintCol))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         dd9717d5-09c8-42ed-8782-77ed0c19b9bf)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         842677eb-f38c-41d4-aa36-f9a2fe1d33fe)(label(2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))))))))))))(Secondary((id \
         af624f1e-2142-443c-bebb-a480362fd341)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         748b73b8-4ebe-445f-883d-2d7f4cb3c939)(content(Whitespace\"\\n\"))))(Tile((id \
         7374f499-0d79-4efd-9743-29bb917d967e)(label(m))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         c7a04bdd-9ccf-436c-88a7-1f559464359f)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         867b39de-3ef1-4903-ac96-08997a59dfaf)(label(canvas))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         52cf7e12-cc87-4e71-8a1d-d7881a000756)(content(Whitespace\" \
         \"))))(Tile((id \
         086d4f33-b74b-4e66-b5fa-baa9ff2a9280)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         fcb8758d-aa91-49f9-9dae-f442a8d96f8f)(content(Whitespace\" \
         \"))))(Tile((id 3f40ed11-a9d2-48a4-b009-1db49500aa83)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         2eedfe6a-290b-4492-91f3-8937daeba6ef)(label([ ]))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         420b486a-0dfa-4bef-88d3-3acbc2ca44c0)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         9284568f-abff-4612-a5c0-8baa8292e3c7)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         cf9dd862-2002-47a7-a0af-8104ba058d76)(content(Whitespace\" \
         \"))))(Tile((id \
         043b9a55-3c14-46c3-a7c6-e8dd250515ee)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         3bae9579-1d5c-4af1-908d-f1c6a6868280)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         608e193d-28ea-463d-87c0-7af940892b84)(content(Whitespace\" \
         \"))))(Tile((id \
         17927acb-4315-4b13-a694-83fce1976322)(label(\"\\\"\\240\\159\\142\\168\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         314b13c4-164e-462c-846f-a9b7f8ab92e5)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         5e1dd4d3-2355-41ab-8e64-fe6b54e80005)(content(Whitespace\" \
         \"))))(Tile((id 89cce829-f1b6-48f8-93a9-89b84051d4a1)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         e460c6bd-99a9-4249-b333-d6a28733ef1d)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         8b2a861e-cdd6-42e7-ad42-070e8c850ecc)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         22b3fe04-5dd8-4645-b2e7-b302faea78fb)(content(Whitespace\" \
         \"))))(Tile((id \
         c54335e4-8645-406e-af89-50437c6b8320)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         4d2c938d-807b-49b0-a985-402a4dab99c4)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         802ca8fc-a5bb-4093-ae1e-95b23cee8b21)(content(Whitespace\" \
         \"))))(Tile((id \
         ffe7faaa-ed09-4529-9f0e-0cc8e8d4694d)(label(\"\\\"\\240\\159\\142\\168\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         3ef7a216-2508-4ca8-8528-99106abe1a3c)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         a58776e4-39d4-40c7-8a6e-f058f7239755)(content(Whitespace\" \
         \"))))(Tile((id 6d318e77-12c6-4f85-82b7-e7d3608923b2)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         eea977aa-1c37-4603-8dfd-f9c25dcc58b6)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         50b04a11-ca2d-4b6e-9594-d159672a289b)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         fa38d29c-13c6-4e18-8f44-e8bc610379fc)(content(Whitespace\" \
         \"))))(Tile((id \
         cd23f4c9-1880-4e8b-9038-55b8c36402fd)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         761681ec-792c-4845-97d9-4f06867b2163)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         be9c796f-ee6e-447d-ba98-3a6b5d13079c)(content(Whitespace\" \
         \"))))(Tile((id \
         896fe4f4-eef8-49a4-a949-d25206a1c46b)(label(\"\\\"\\240\\159\\142\\168\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))))))))))))(Secondary((id \
         808b0828-3d3f-4ec0-81e0-8b8c8c778e09)(content(Whitespace\"\\n\")))))))))(Tile((id \
         f38cb93a-0a55-4fad-b703-5e42599e9d3e)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 38))(sort Exp))((shape(Concave \
         38))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         e94053d2-936b-44df-a313-7b9bc482473d)(content(Whitespace\"\\n\"))))(Secondary((id \
         87c09031-e2cc-4c9d-90d5-c443eb643465)(content(Whitespace\"\\n\"))))(Tile((id \
         1ec87139-1f23-4526-aa68-9d70f990130d)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         469e0c57-9053-4e5d-99e1-967bfd832351)(content(Whitespace\"\\n\"))))(Tile((id \
         e6823230-5323-4969-bb04-62367a819a71)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         40))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         82bf2d7d-90c7-4c81-8d9c-c366537a70ab)(content(Whitespace\" \
         \"))))(Tile((id \
         a96ce710-dfbe-453a-8964-78964e38cd49)(label(m))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         fef5471b-58fa-46e2-9155-e48f4b973b57)(content(Whitespace\" \
         \")))))((Secondary((id \
         4a965f7c-6c5c-41ad-8002-42c28c5687d6)(content(Whitespace\" \
         \"))))(Tile((id \
         722c01fa-5142-4e69-8647-e7770c079eb6)(label(do))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         a8575234-7acd-4e86-87b5-40457cec7f22)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         88e1a408-8ed0-46aa-ac3a-d7b5c2c74718)(label(init))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         2142bf41-ad9f-4e85-af4a-c178c132875f)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         de823082-3992-4dbd-9b64-2c9ef49e00aa)(content(Whitespace\" \
         \"))))(Tile((id 9b26870a-38b8-4379-8cc5-5ba0566c838f)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         443f6ffa-46e6-48d7-8ceb-2fd6cc8e6607)(label(PaintRow))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         8bb78678-5573-47fb-9fb7-c95af398da13)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         f0d4001f-6def-4f38-8e88-a18585e3b357)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         48ce536c-2e8c-4f3a-a266-1f902a13e70f)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         dd247a60-6743-463f-9966-761c0df9b743)(content(Whitespace\" \
         \"))))(Tile((id \
         e6fad890-97bc-4e4f-906e-0b5dceb2b9ba)(label(PaintCol))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         011ce2d4-f705-4276-b806-57c0c2a7374a)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         491f8e6b-2697-4fd2-8be1-d1753af60dad)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))))))))))))(Secondary((id \
         ab7ba70d-af9c-4733-a88f-26dcf5b10569)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         f70fe95e-800b-4b63-beae-ebf27c246787)(content(Whitespace\"\\n\"))))(Tile((id \
         31b9a5c5-5748-4c10-9c81-585debb9d407)(label(m))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         cecb7141-d13e-48f0-b7d0-37f9d76c07b9)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         cd5c6675-cc84-405f-92c1-1286188ac00f)(label(canvas))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         047173b5-1180-44da-b79f-3c4f5af40f20)(content(Whitespace\" \
         \"))))(Tile((id \
         9b2f34c7-b551-451b-a2ed-22b972c590d4)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         4c2d8c69-45c8-4996-b2a6-6bf22fc709d7)(content(Whitespace\" \
         \"))))(Tile((id dd2a2777-7bde-4ac9-8f7d-bc2d821055d0)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         b91baed6-3a8d-4fdf-afa0-f859767188f5)(label([ ]))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         206646d7-aa9b-4fde-b0fa-a4556d90603d)(label(\"\\\"\\240\\159\\142\\168\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         f37278d6-4d39-43af-aadf-bacf2315b016)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         0f2d9f68-0671-4621-816f-8a0b3a5cb760)(content(Whitespace\" \
         \"))))(Tile((id \
         46ff8454-33fe-4772-abbf-465dcb3a417b)(label(\"\\\"\\240\\159\\142\\168\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         c84952de-160a-46d9-b519-f1c953067842)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         b09d72fa-5875-493f-bb15-8e524877f39d)(content(Whitespace\" \
         \"))))(Tile((id \
         75894aae-869d-4386-8a29-918b1b240ec2)(label(\"\\\"\\240\\159\\142\\168\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         c1878d11-2bfc-46d3-a23a-ade3207573d1)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         747f57f7-e33d-4ebc-a980-07d81a267c31)(content(Whitespace\" \
         \"))))(Tile((id ab8ac3d0-37db-4244-9b70-bb036ac2a981)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         f7f856a3-9e6d-4193-82e7-3c2dddbed916)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         401457d6-0752-4eda-ba82-9f8a1accc3be)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         2d47ca24-d80c-42b7-ae95-624f877fda2d)(content(Whitespace\" \
         \"))))(Tile((id \
         a666036e-ba2c-414a-8b9e-893aede64b00)(label(\"\\\"\\240\\159\\142\\168\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         376f39b9-a393-41ed-8577-86258823aa63)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         d30d026b-af68-47eb-b359-c63f5079f2b2)(content(Whitespace\" \
         \"))))(Tile((id \
         2f2f71a3-e3eb-456a-8aa2-89f7443b8a3e)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         9cb4b4c4-79e7-4762-b225-322b49b6297d)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         64b4f6fc-bcac-43a3-ade2-61d85004356c)(content(Whitespace\" \
         \"))))(Tile((id 06ceeb1b-36c6-4ca1-8282-a447fe97c04d)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         b9454bd9-89c8-439b-a3aa-5dd5a82df5f2)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         1563a669-ef26-4ec8-a741-24a72cd8af95)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         6f2eafb2-0fc2-4f55-94e5-b4dfb52939c8)(content(Whitespace\" \
         \"))))(Tile((id \
         0128dd31-6c46-4f23-991e-336b97259663)(label(\"\\\"\\240\\159\\142\\168\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         7fe4ff59-1efa-4cfe-a258-2a1f9b6f454d)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         ba10e404-c839-48df-a681-e6861569882e)(content(Whitespace\" \
         \"))))(Tile((id \
         4840e300-dfae-496f-a3b2-56c5a4e3347d)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))))))))))))(Secondary((id \
         1fd8679d-23f3-4b9a-9ddc-ef6e5ddc331f)(content(Whitespace\"\\n\")))))))))(Tile((id \
         b67ba320-2dd2-4ab3-9842-6e3304bfb161)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 38))(sort Exp))((shape(Concave \
         38))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         367cceb4-eba5-4d23-8385-3d2d48834f55)(content(Whitespace\"\\n\"))))(Secondary((id \
         622854cf-2947-4b12-9112-2340a0018acf)(content(Whitespace\"\\n\"))))(Tile((id \
         97b55070-e5f3-44c6-b2b5-b2201fd382f6)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         1e0a1c35-fc44-4c3a-8e7f-91b0dfde3a76)(content(Whitespace\"\\n\"))))(Tile((id \
         6babac57-e7a8-4745-b9cb-c3f911c72d2e)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         40))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         781f54ef-8e4b-4f26-8348-885f237d71e2)(content(Whitespace\" \
         \"))))(Tile((id \
         e85b7433-fc8d-47f4-8d1f-81d7f028891f)(label(m))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         65158c7c-5ee9-4140-ba06-b17854bc617b)(content(Whitespace\" \
         \")))))((Secondary((id \
         7fad7980-92aa-42a9-af71-4d28fdb36c42)(content(Whitespace\" \
         \"))))(Tile((id \
         6ca47105-57be-421b-aea1-8a183f3494d5)(label(do))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         65449632-2923-40ee-bf12-bdc487313517)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         138e1a9b-f1a7-451b-a0dd-b2361471cd38)(label(init))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         5180cba6-367f-468c-92bc-d44f79480e20)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         b0962a1f-b0f7-4c69-bd24-d463d0aa08f8)(content(Whitespace\" \
         \"))))(Tile((id d95e98fa-90a4-4f05-a6de-3d8d36d20b62)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         7bd9019f-c369-4ec3-a8f3-557180ff27b5)(label(SetBrush))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         f49c7472-b262-4106-9bb4-97ad842ba08d)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         8d874572-48dd-44d7-a215-d570e3a4006b)(label(2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         934ee23e-08f2-44da-90a1-50f91265d207)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         77149631-458e-4ab7-a61d-2dbf889ef447)(content(Whitespace\" \
         \"))))(Tile((id \
         14a75f9c-0eb0-437d-8f58-0bd17cf3518b)(label(PaintCol))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         850f26a4-bb79-4bf5-959a-c72def46e351)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         5a3c5f79-a78b-4c07-95eb-c717b2f16590)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))))))))))))(Secondary((id \
         8e94daf9-9b72-4448-ac32-381f4f059a8d)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         9cd3d4b2-1e5c-4099-808f-06132e8bdd4a)(content(Whitespace\"\\n\"))))(Tile((id \
         c9bd621d-6d99-494c-9ac8-dd955fc1ca94)(label(m))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         1760f1f1-9714-4f1a-91d2-6954d68085fe)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         59884ea1-105b-4c18-b39c-9f128c4fd1da)(label(canvas))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         0cbef8bd-0ba5-42dc-831b-bebc964bb394)(content(Whitespace\" \
         \"))))(Tile((id \
         26f71294-79ca-4550-a5cd-69a5a7714e99)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         77181933-981d-4ab5-9dfe-5518d08cc140)(content(Whitespace\" \
         \"))))(Tile((id d2f110e4-0a28-47c5-a4eb-d61593e9e131)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         d9c24045-e10d-4d53-91cf-5b7cb0b8df82)(label([ ]))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         e25233a3-64e6-41e4-9a15-e5f1cf8bf430)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         a5f3ea47-94eb-4c21-90ab-8cd588b9357e)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         0398c3d7-5766-4a0c-9728-42efec7af581)(content(Whitespace\" \
         \"))))(Tile((id \
         3c07848d-01db-4106-a1af-86949486f6f9)(label(\"\\\"\\240\\159\\146\\156\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         86e42c6e-5acb-47ed-a9e3-3a138c8c8947)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         17f0a927-21c0-4457-8a66-74a5016dac0e)(content(Whitespace\" \
         \"))))(Tile((id \
         c4cd2108-5e9e-4800-a6e5-097a5c36c295)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         ff09ca49-f9ab-45d5-8357-6ab643bd4fdd)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         91ed16ce-eb9d-4c78-b916-e0f19b4b3dba)(content(Whitespace\" \
         \"))))(Tile((id 00216d20-d7dc-4463-9977-b6485e0cb7c2)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         6e53a7b5-411c-4e4a-8057-7aef9e958b21)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         b25ba9b7-4927-4eb6-8045-51d562090c83)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         a874d981-4143-4fd8-9b35-07196603454b)(content(Whitespace\" \
         \"))))(Tile((id \
         1edd7e19-c549-4c95-98a0-a83be0b0e8e9)(label(\"\\\"\\240\\159\\146\\156\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         efa46982-47c2-4840-99c0-dcd5e4e64dde)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         ab2e0a97-ba83-4331-ae49-b4297840043f)(content(Whitespace\" \
         \"))))(Tile((id \
         e2f862ff-c21a-4f2a-b08c-6e4a61966634)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         56fbc212-a5be-49eb-95f1-75f74ea85b98)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         b1785d32-ed85-49a3-9b2c-68926f8eac18)(content(Whitespace\" \
         \"))))(Tile((id da11155b-87bf-464f-92bc-c1627d46db16)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         f5d61690-2a5c-4180-ad03-3aa3566ba321)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         2b9ece27-a4a6-4d9b-8e9c-a679c7457a9b)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         5450d23f-d0bd-44a3-abc1-88e52befc9b1)(content(Whitespace\" \
         \"))))(Tile((id \
         5b89a68e-154a-4062-bb88-e3e8ea7787d6)(label(\"\\\"\\240\\159\\146\\156\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         f92ee697-7fc0-4626-ab9f-766d9bf0a43a)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         b4312ea2-44d4-4d39-88c2-8ef0ca0243f9)(content(Whitespace\" \
         \"))))(Tile((id \
         9020f324-eb4d-425e-ad0e-afa12faaac93)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))))))))))))(Secondary((id \
         45fc5c40-9774-4316-a6a5-76ba95cd5c12)(content(Whitespace\"\\n\"))))(Tile((id \
         f004f78c-475e-4bf0-b74f-57aceb5cd6c8)(label(&&))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 32))(sort Exp))((shape(Concave \
         32))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         4fec15b0-d4fc-415c-8440-de6bd8b30a9f)(content(Whitespace\" \
         \"))))(Tile((id \
         f5da1520-4a63-4739-9226-38bbfa06a525)(label(m))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         bf1a341e-73b3-417e-8a1a-ca91eacc2107)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         0cf1d383-1d72-4488-9b3b-348187abde35)(label(brush))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         f1a1cd4d-b729-4aec-968b-da0f6dfb747c)(content(Whitespace\" \
         \"))))(Tile((id \
         d7795dba-e47e-4f06-887b-a9a4d2a5d83d)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         8ddc242f-df7f-45c7-a3a1-32088995e5df)(content(Whitespace\" \
         \"))))(Tile((id \
         bcbb8566-4e74-48af-a4df-aa77da5ff263)(label(\"\\\"\\240\\159\\146\\156\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         46d78325-9be7-45fb-9099-ea4dd5d69f54)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         326e5cad-a069-4ace-b621-9fd8623a494a)(content(Whitespace\"\\n\")))))";
      backup_text =
        "# EMOJIPAINT EXTENSION TASK                     #\n\
         #                                               #\n\
         # The emojipaint app lets you paint emojis on   #\n\
         # a grid. It already supports painting rows.    #\n\
         #                                               #\n\
         # YOUR TASK: Add a PaintCol action that fills   #\n\
         # an entire column with the current brush.      #\n\
         #                                               #\n\
         # You need to:                                  #\n\
         #   1. Add PaintCol(Col) to the Action type     #\n\
         #   2. Add a setCol helper function             #\n\
         #   3. Handle PaintCol in the update function   #\n\
         #                                               #\n\
         # Look at how PaintRow is implemented for       #\n\
         # guidance - PaintCol is similar but vertical.  #\n\
         #                                               #\n\
         # Tip: Use auto-probe to see how the canvas     #\n\
         # changes after each action.                    #\n\n\
         type Emoji = String in\n\
         type Canvas = [[Emoji]] in\n\
         type Row = Int in\n\
         type Col = Int in\n\n\
         type Model = (\n\
         canvas = Canvas,\n\
         brush = Emoji,\n\
         palette = [Emoji]\n\
         ) in\n\n\
         type Action =\n\
         + SetBrush(Int)\n\
         + PaintCell(Row, Col)\n\
         + ClearCell(Row, Col)\n\
         + ClearGrid\n\
         + PaintRow(Row)\n\
         # TODO: Add PaintCol(Col) here #\n\
         in\n\n\
         let init: Model = (\n\
         canvas = [\n\
         [\"\", \"\", \"\"],\n\
         [\"\", \"\", \"\"],\n\
         [\"\", \"\", \"\"]\n\
         ],\n\
         brush = \"\240\159\142\168\",\n\
         palette = [\"\240\159\142\168\", \"\240\159\140\159\", \
         \"\240\159\146\156\", \"\240\159\148\165\", \"\240\159\140\138\"]\n\
         ) in\n\n\
         let setCell: (Canvas, Row, Col, Emoji) -> Canvas =\n\
         fun canvas, row, col, emoji ->\n\
         mapi(canvas, fun (i, r) ->\n\
         if i == row\n\
         then mapi(r, fun (j, c) -> if j == col then emoji else c)\n\
         else r)\n\
         in\n\n\
         let setRow: (Canvas, Row, Emoji) -> Canvas =\n\
         fun canvas, targetRow, emoji ->\n\
         mapi(canvas, fun (i, row) ->\n\
         if i == targetRow\n\
         then map(row, fun _ -> emoji)\n\
         else row)\n\
         in\n\n\
         # TODO: Add setCol helper here #\n\
         # Hint: You need to modify each row, changing #\n\
         # only the cell at the target column.         #\n\n\
         let setAll: (Canvas, Emoji) -> Canvas =\n\
         fun (canvas, emoji) ->\n\
         map(canvas, fun row -> map(row, fun _ -> emoji))\n\
         in\n\n\
         let updateGrid: (Model, Canvas -> Canvas) -> Model =\n\
         fun (m, f) -> (f(m.canvas), m.brush, m.palette)\n\
         in\n\n\
         let update: (Model, Action) -> Model =\n\
         fun m, action ->\n\
         case action\n\
         | SetBrush(idx) =>\n\
         (m.canvas, nth(m.palette, idx), m.palette)\n\
         | PaintCell(row, col) =>\n\
         updateGrid(m, fun c -> setCell(c, row, col, m.brush))\n\
         | ClearCell(row, col) =>\n\
         updateGrid(m, fun c -> setCell(c, row, col, \"\"))\n\
         | ClearGrid =>\n\
         updateGrid(m, fun c -> setAll(c, \"\"))\n\
         | PaintRow(row) =>\n\
         updateGrid(m, fun c -> setRow(c, row, m.brush))\n\
         # TODO: Add PaintCol case here #\n\
         end\n\
         in\n\n\
         let do: (Model, [Action]) -> Model =\n\
         fun (init: Model, actions: [Action]) ->\n\
         fold_left(actions, update, init)\n\
         in\n\n\
         # Existing tests #\n\
         test\n\
         let m = update(init, PaintRow(1)) in\n\
         m.canvas == [[\"\", \"\", \"\"], [\"\240\159\142\168\", \
         \"\240\159\142\168\", \"\240\159\142\168\"], [\"\", \"\", \"\"]]\n\
         end;\n\n\
         # New tests for PaintCol #\n\
         test\n\
         let m = update(init, PaintCol(0)) in\n\
         m.canvas == [[\"\240\159\142\168\", \"\", \"\"], \
         [\"\240\159\142\168\", \"\", \"\"], [\"\240\159\142\168\", \"\", \
         \"\"]]\n\
         end;\n\n\
         test\n\
         let m = update(init, PaintCol(2)) in\n\
         m.canvas == [[\"\", \"\", \"\240\159\142\168\"], [\"\", \"\", \
         \"\240\159\142\168\"], [\"\", \"\", \"\240\159\142\168\"]]\n\
         end;\n\n\
         test\n\
         let m = do(init, [PaintRow(0), PaintCol(1)]) in\n\
         m.canvas == [[\"\240\159\142\168\", \"\240\159\142\168\", \
         \"\240\159\142\168\"], [\"\", \"\240\159\142\168\", \"\"], [\"\", \
         \"\240\159\142\168\", \"\"]]\n\
         end;\n\n\
         test\n\
         let m = do(init, [SetBrush(2), PaintCol(1)]) in\n\
         m.canvas == [[\"\", \"\240\159\146\156\", \"\"], [\"\", \
         \"\240\159\146\156\", \"\"], [\"\", \"\240\159\146\156\", \"\"]]\n\
         && m.brush == \"\240\159\146\156\"\n\
         end\n";
      refractors = "()";
    } )
