let out : string * Haz3lcore.PersistentSegment.t =
  ( "Study / writing / emojipaint-extend / emojipaint-extend-sketch",
    {
      segment =
        "((Secondary((id \
         719cd4b0-0316-403b-a968-b0d0d5ff0fea)(content(Comment\"# EMOJIPAINT \
         EXTENSION TASK                     #\"))))(Secondary((id \
         7cba943a-9a8a-4b69-9ef2-41becb215693)(content(Whitespace\"\\n\"))))(Secondary((id \
         af73d242-dd0a-4308-a4f3-4269c7da6abd)(content(Comment\"#                                               \
         #\"))))(Secondary((id \
         cd725d46-72e1-4e8a-b387-f1c3717044fa)(content(Whitespace\"\\n\"))))(Secondary((id \
         776778ac-50f4-483c-86e1-b638289bad19)(content(Comment\"# The \
         emojipaint app lets you paint emojis on   #\"))))(Secondary((id \
         48d71bf3-f2ee-4c60-8837-a04632e52dbe)(content(Whitespace\"\\n\"))))(Secondary((id \
         705e17f2-3a95-4975-b75f-5a6fea3a0f20)(content(Comment\"# a grid. It \
         already supports painting rows.    #\"))))(Secondary((id \
         85789fdd-77e5-4ab7-996d-a8e96457e8d5)(content(Whitespace\"\\n\"))))(Secondary((id \
         ff00ab32-bdce-4b15-801d-c4e197899ba9)(content(Comment\"#                                               \
         #\"))))(Secondary((id \
         bdc59349-422f-46c5-bb5e-087d2f7e970d)(content(Whitespace\"\\n\"))))(Secondary((id \
         51d0cca7-8e2d-4310-b74b-d301b10be6ed)(content(Comment\"# YOUR TASK: \
         Add a PaintCol action that fills   #\"))))(Secondary((id \
         dba4b135-abc4-4dad-a675-b1941633a9d9)(content(Whitespace\"\\n\"))))(Secondary((id \
         5ac479d6-0944-4a1b-b54b-0fa2d65d4ab9)(content(Comment\"# an entire \
         column with the current brush.      #\"))))(Secondary((id \
         9a327d15-5b35-42f2-8141-881841eb378d)(content(Whitespace\"\\n\"))))(Secondary((id \
         3c0df8e9-c1f0-4629-8f10-d3a98b9af49c)(content(Comment\"#                                               \
         #\"))))(Secondary((id \
         b934898e-5440-4275-a561-066a4581be89)(content(Whitespace\"\\n\"))))(Secondary((id \
         4113d0ec-22ec-4e3a-b5d0-1ee41d941058)(content(Comment\"# You need \
         to:                                  #\"))))(Secondary((id \
         7abad18d-6d65-4c91-939f-521baa582167)(content(Whitespace\"\\n\"))))(Secondary((id \
         69b770c8-b357-4e06-8a42-53ee62b411d7)(content(Comment\"#   1. Add \
         PaintCol(Col) to the Action type     #\"))))(Secondary((id \
         af048755-8f58-4dae-99fd-00f3a7580c93)(content(Whitespace\"\\n\"))))(Secondary((id \
         39d6f353-11e8-4fab-a38b-7958dcc1c33a)(content(Comment\"#   2. Add a \
         setCol helper function             #\"))))(Secondary((id \
         460da1b0-3252-4d32-b0ab-70089833274d)(content(Whitespace\"\\n\"))))(Secondary((id \
         85e65576-9884-4ddf-8e5d-6ce5559efc56)(content(Comment\"#   3. Handle \
         PaintCol in the update function   #\"))))(Secondary((id \
         fff0a597-8c87-453a-8937-fa096541aedb)(content(Whitespace\"\\n\"))))(Secondary((id \
         de591bed-e21f-4e5f-ae9f-4ee4ede652bf)(content(Comment\"#                                               \
         #\"))))(Secondary((id \
         dee80edc-2e3d-4298-92ab-7072356dc788)(content(Whitespace\"\\n\"))))(Secondary((id \
         b466ac49-e966-4a1f-9d29-216d1a4a8d92)(content(Comment\"# Look at how \
         PaintRow is implemented for       #\"))))(Secondary((id \
         d90dc7d0-6a66-413b-93c8-7ecbbe2cf6d8)(content(Whitespace\"\\n\"))))(Secondary((id \
         61c57043-f9e3-453c-a8e2-4e4bfa67eff4)(content(Comment\"# guidance - \
         PaintCol is similar but vertical.  #\"))))(Secondary((id \
         f0c586cf-2857-43ca-8d6b-b127d46ecca6)(content(Whitespace\"\\n\"))))(Secondary((id \
         c3d25301-4f7f-41c7-93df-4e542e7b835e)(content(Comment\"#                                               \
         #\"))))(Secondary((id \
         b60657bb-00c4-41ed-befd-9009e1c35777)(content(Whitespace\"\\n\"))))(Secondary((id \
         07d68c2a-4ebe-475b-95cc-0d6cb86b8800)(content(Comment\"# Tip: Use \
         auto-probe to see how the canvas     #\"))))(Secondary((id \
         66e810ed-128a-4db3-81a0-22621e5fbd82)(content(Whitespace\"\\n\"))))(Secondary((id \
         5dcffdd6-29c7-48d3-a8c1-4f03ea41665e)(content(Comment\"# changes \
         after each action.                    #\"))))(Secondary((id \
         bc606290-fadb-4c47-b442-53f556e4e663)(content(Whitespace\"\\n\"))))(Secondary((id \
         7f30c32c-e4e7-4c87-9a06-a320c32f624e)(content(Whitespace\"\\n\"))))(Tile((id \
         df2d2bed-53d5-4e59-9ce5-ed05a0f7fdef)(label(type = in))(mold((out \
         Exp)(in_(TPat Typ))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         f1448492-bcc9-4dcb-8d51-99213a77ecb3)(content(Whitespace\" \
         \"))))(Tile((id \
         ebf3074c-792f-406b-9eae-f35611db2ffe)(label(Emoji))(mold((out \
         TPat)(in_())(nibs(((shape Convex)(sort TPat))((shape Convex)(sort \
         TPat))))))(shards(0))(children())))(Secondary((id \
         923e1e1a-4b39-4b34-8610-5e5792f6c000)(content(Whitespace\" \
         \")))))((Secondary((id \
         a45c0a62-6091-44ba-8217-aa4a4f7cbbf3)(content(Whitespace\" \
         \"))))(Tile((id \
         5c0c5c00-e3b1-45b8-a7c0-dedef71390ee)(label(String))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         015e7b5d-463a-44d9-8987-1aca014a5497)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         b155488f-75ff-4ac3-9848-1cc4eef2f32b)(content(Whitespace\"\\n\"))))(Tile((id \
         37520ac2-54fd-41fa-8153-2bfe38d5d568)(label(type = in))(mold((out \
         Exp)(in_(TPat Typ))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         2ad75ce8-7718-4ff5-b169-d21d8b2a414d)(content(Whitespace\" \
         \"))))(Tile((id \
         d3c159b4-10dc-4eed-8a53-145ae90e4a63)(label(Canvas))(mold((out \
         TPat)(in_())(nibs(((shape Convex)(sort TPat))((shape Convex)(sort \
         TPat))))))(shards(0))(children())))(Secondary((id \
         f73e537f-5e15-45b7-9957-de121e17a6f5)(content(Whitespace\" \
         \")))))((Secondary((id \
         20ef81c1-5548-41cf-b5fd-c9c63192035b)(content(Whitespace\" \
         \"))))(Tile((id e653ac9e-ec68-4a11-a0ba-a1a766feca5c)(label([ \
         ]))(mold((out Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape \
         Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
         ac56742a-a656-4c1f-b9e6-96d54b804fa6)(label([ ]))(mold((out \
         Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0 1))(children(((Tile((id \
         8ae02e06-9721-449d-a588-f920860180a2)(label(Emoji))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))))))))))))(Secondary((id \
         45b08975-fb59-4fcc-a4dc-d24d0594ca8b)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         6af1bcc9-3cf1-4d8b-83fe-51d80d1ac065)(content(Whitespace\"\\n\"))))(Tile((id \
         d1694154-7319-4a8c-b45b-25b2faa4ccf5)(label(type = in))(mold((out \
         Exp)(in_(TPat Typ))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         d43c8571-5414-453e-b748-71bfdf0d2a57)(content(Whitespace\" \
         \"))))(Tile((id \
         924d2585-1a63-46fe-b9c3-3baf36261cf7)(label(Row))(mold((out \
         TPat)(in_())(nibs(((shape Convex)(sort TPat))((shape Convex)(sort \
         TPat))))))(shards(0))(children())))(Secondary((id \
         39eaf85d-2586-4d49-801f-86bbd49ff1f9)(content(Whitespace\" \
         \")))))((Secondary((id \
         d0c7b021-cbc7-47b1-b31b-c977483cb06a)(content(Whitespace\" \
         \"))))(Tile((id \
         8bb657c2-44cc-40cf-a7f1-6f07b33f4018)(label(Int))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         e069e10f-7d5f-465d-8671-4c6fb46c6edb)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         807f22b8-cc25-42b3-a7b2-0c89ea682437)(content(Whitespace\"\\n\"))))(Tile((id \
         71537495-7954-42da-9040-8fb9d6a838a7)(label(type = in))(mold((out \
         Exp)(in_(TPat Typ))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         76b9f2be-1760-428c-8155-663e96a73ff4)(content(Whitespace\" \
         \"))))(Tile((id \
         a74921e4-f7f9-4c2d-afba-d9b63cb2cfec)(label(Col))(mold((out \
         TPat)(in_())(nibs(((shape Convex)(sort TPat))((shape Convex)(sort \
         TPat))))))(shards(0))(children())))(Secondary((id \
         73810423-a0f5-4824-99bb-8990a7464936)(content(Whitespace\" \
         \")))))((Secondary((id \
         f1c6267b-bb76-4736-b3df-70c211425cb4)(content(Whitespace\" \
         \"))))(Tile((id \
         bf7937bc-962f-46e6-bbaf-590a1d12f007)(label(Int))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         1a19d50a-a0cf-4493-81de-a1f8ad595b1c)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         5df6116d-0d7a-4247-954d-24b62e888584)(content(Whitespace\"\\n\"))))(Secondary((id \
         a2802786-325e-4239-80dd-fb5be0f06f03)(content(Whitespace\"\\n\"))))(Tile((id \
         1ca02bdd-1550-49d4-94ce-de75b7e3086c)(label(type = in))(mold((out \
         Exp)(in_(TPat Typ))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         18f42c81-8d60-40a5-8050-79ef4ccda0db)(content(Whitespace\" \
         \"))))(Tile((id \
         1b5fa1fb-2006-4789-9381-e48bafef3783)(label(Model))(mold((out \
         TPat)(in_())(nibs(((shape Convex)(sort TPat))((shape Convex)(sort \
         TPat))))))(shards(0))(children())))(Secondary((id \
         02ae377b-f493-4ad0-a533-3aa36b09da44)(content(Whitespace\" \
         \")))))((Secondary((id \
         5672754c-43d7-4fbc-ac8b-8e66f0c88350)(content(Whitespace\" \
         \"))))(Tile((id \
         2861df35-df5c-4636-acd6-fd1435765e3e)(label(\"(\"\")\"))(mold((out \
         Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0 1))(children(((Secondary((id \
         2ee45b98-37cb-46f3-bce8-c839084ccb84)(content(Whitespace\"\\n\"))))(Tile((id \
         783cb180-5470-48a3-83cb-231632611752)(label(canvas))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         62199e80-9fa5-4a50-b019-cb16cc5732f8)(content(Whitespace\" \
         \"))))(Tile((id \
         c465ba48-8625-4d90-b1a3-26dfdc84838f)(label(=))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 39))(sort Typ))((shape(Concave \
         39))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         8ec4e160-6940-4373-a059-007fee749095)(content(Whitespace\" \
         \"))))(Tile((id \
         b7679d7f-30a7-4cfa-9727-2f48e11cbc25)(label(Canvas))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         9a5f13f5-b693-4a25-a363-ac4e17a95cdb)(label(,))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 44))(sort Typ))((shape(Concave \
         44))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         398bc750-bcbe-4241-a4cf-fd0d35bf8f39)(content(Whitespace\"\\n\"))))(Tile((id \
         0af1ccb0-9fec-41eb-996a-c559a4183f05)(label(brush))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         77ab9896-e19c-4223-bdcc-4736b8a2fd2e)(content(Whitespace\" \
         \"))))(Tile((id \
         02b37ffd-edc2-4252-bc72-9edefb7b0d02)(label(=))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 39))(sort Typ))((shape(Concave \
         39))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         4ad311c4-5aa4-4b3a-bc5b-058a8aa50f62)(content(Whitespace\" \
         \"))))(Tile((id \
         1ffd7704-4661-40af-aeb4-2a22ce01f668)(label(Emoji))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         4d842ee1-4688-4ba6-b26b-b8b946f1979c)(label(,))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 44))(sort Typ))((shape(Concave \
         44))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         9e6e0f58-29b3-4bb2-95da-1027e6240654)(content(Whitespace\"\\n\"))))(Tile((id \
         6e5f2554-c519-4cfc-acbb-ea60768508e8)(label(palette))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         9bfbf6c9-b3b9-4863-9935-1c3decc5a985)(content(Whitespace\" \
         \"))))(Tile((id \
         9bf6539f-79ad-4126-92cf-4ecd63bad273)(label(=))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 39))(sort Typ))((shape(Concave \
         39))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         407ea098-82b9-467e-8c62-aea2d5046db9)(content(Whitespace\" \
         \"))))(Tile((id bdeae288-fbb3-4cb7-93bc-7edd0df238b7)(label([ \
         ]))(mold((out Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape \
         Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
         bfc44421-10b8-4bd4-b606-d6b918cfbe28)(label(Emoji))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children()))))))))(Secondary((id \
         9f5d13e2-cb02-4623-a112-f46894c142b8)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         a295a015-05b7-46dd-a3a4-2c0c24462c85)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         6719f267-0e1c-4a47-abda-fe40ad923e02)(content(Whitespace\"\\n\"))))(Secondary((id \
         b11c9c22-edc6-4734-87e7-54d60fb72b72)(content(Whitespace\"\\n\"))))(Tile((id \
         47dd8cdd-9c97-4a6a-afca-e429f77edf55)(label(type = in))(mold((out \
         Exp)(in_(TPat Typ))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         1f20d3e3-6a70-4b3f-b527-284f7717966c)(content(Whitespace\" \
         \"))))(Tile((id \
         1fcb5309-4c32-4aab-8513-346d5c4a3a09)(label(Action))(mold((out \
         TPat)(in_())(nibs(((shape Convex)(sort TPat))((shape Convex)(sort \
         TPat))))))(shards(0))(children())))(Secondary((id \
         f3a17b12-8540-423b-ac9b-2112e52c25cb)(content(Whitespace\" \
         \")))))((Secondary((id \
         1ef848a6-0381-4fb7-8228-f190ab9f9258)(content(Whitespace\"\\n\"))))(Tile((id \
         d4fe41b2-8b7f-4a70-8af9-c116dabc3122)(label(+))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape(Concave 33))(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         68bac5fb-ed99-400b-97b8-3052fd62ac34)(content(Whitespace\" \
         \"))))(Tile((id \
         a8e7dcf2-1f56-496a-b624-0ad92c842953)(label(SetBrush))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         c83e6245-3ca1-4b29-ad31-c4e06152a213)(label(\"(\"\")\"))(mold((out \
         Typ)(in_(Typ))(nibs(((shape(Concave 11))(sort Typ))((shape \
         Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
         149f4db6-d26a-4484-bb46-76242d84c537)(label(Int))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children()))))))))(Secondary((id \
         6a3b1c5c-0435-44a8-bb61-400271b0588b)(content(Whitespace\"\\n\"))))(Tile((id \
         6ae64def-13f4-4766-b9f0-00370be4715c)(label(+))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 12))(sort Typ))((shape(Concave \
         12))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         77da269f-1668-4b02-bb2a-b9c84f45ac4c)(content(Whitespace\" \
         \"))))(Tile((id \
         34355b7a-5d67-4532-8b88-096dce34d07c)(label(PaintCell))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         801baa98-cb5d-468f-9a52-b0784ecf6a57)(label(\"(\"\")\"))(mold((out \
         Typ)(in_(Typ))(nibs(((shape(Concave 11))(sort Typ))((shape \
         Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
         b129da2e-df2a-4883-9f9e-290b29ad24eb)(label(Row))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         dfa59db6-f7e0-4dbe-b3ce-9f6f107acb03)(label(,))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 44))(sort Typ))((shape(Concave \
         44))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         05445ddd-f353-4fed-bb75-9a2068501f52)(content(Whitespace\" \
         \"))))(Tile((id \
         e9a2a4d3-61f2-4212-b9dc-c1e2f71dc63e)(label(Col))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children()))))))))(Secondary((id \
         b6c0328b-dbaa-45a8-9f4e-d01e3c50ebd2)(content(Whitespace\"\\n\"))))(Tile((id \
         a9b2d5b5-99b0-436c-896f-3bdad718948d)(label(+))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 12))(sort Typ))((shape(Concave \
         12))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         16ab4807-3cfb-4b05-bd37-64a5d1ced1c3)(content(Whitespace\" \
         \"))))(Tile((id \
         c3d50a1f-73fe-4d4b-857f-3bb8411642de)(label(ClearCell))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         ca51216e-eed8-451e-a2b3-8894fff09614)(label(\"(\"\")\"))(mold((out \
         Typ)(in_(Typ))(nibs(((shape(Concave 11))(sort Typ))((shape \
         Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
         c0404012-e973-45fe-93de-0418634f2fcb)(label(Row))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         dc655445-5641-4e18-848b-69b7e3e61e29)(label(,))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 44))(sort Typ))((shape(Concave \
         44))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         f23be50b-65d0-425b-96fd-2a274dcffd8f)(content(Whitespace\" \
         \"))))(Tile((id \
         e164ea9c-ff4e-430d-923e-ba65e59b8a94)(label(Col))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children()))))))))(Secondary((id \
         821cb968-4940-4dd1-96ec-b71acc4c4b18)(content(Whitespace\"\\n\"))))(Tile((id \
         10b09c7b-bbbf-4f5f-b932-2e2a751fe310)(label(+))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 12))(sort Typ))((shape(Concave \
         12))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         1aa2960c-6725-45f2-9609-1cdfcabd25ea)(content(Whitespace\" \
         \"))))(Tile((id \
         88a1aea6-29c1-4616-91f3-ee21f371ed21)(label(ClearGrid))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         cc19622f-5e04-4b24-a62c-0ca2a0d8c78b)(content(Whitespace\"\\n\"))))(Tile((id \
         0586dc9c-8e89-4412-8f85-04c1c482af1c)(label(+))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 12))(sort Typ))((shape(Concave \
         12))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         22f8eddc-d9aa-40d0-9e48-f11999ccc89e)(content(Whitespace\" \
         \"))))(Tile((id \
         023bfc6d-50ac-487b-a246-f39d072180ec)(label(PaintRow))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         c37bcb44-39d6-4d8a-a895-bfb48e10f061)(label(\"(\"\")\"))(mold((out \
         Typ)(in_(Typ))(nibs(((shape(Concave 11))(sort Typ))((shape \
         Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
         3fe6777f-e329-438b-91d6-55470e265efa)(label(Row))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children()))))))))(Secondary((id \
         0b93c973-c44e-4d0c-a131-e936d34ad365)(content(Whitespace\"\\n\"))))(Secondary((id \
         82a43100-4693-4c29-a881-4402cabe377d)(content(Comment\"# TODO: Add \
         PaintCol(Col) here #\"))))(Secondary((id \
         20f0422d-310a-4772-830d-fcdaf37fae8d)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         03f5b7e5-8e81-4997-a6a0-6f219b8607b5)(content(Whitespace\"\\n\"))))(Secondary((id \
         94b4360a-7420-46b7-83aa-b7fbdc53d55a)(content(Whitespace\"\\n\"))))(Tile((id \
         60c0e406-7fa8-499d-a90e-35c9092747ed)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         1f4734f1-d573-4b72-9003-6ff886aa635d)(content(Whitespace\" \
         \"))))(Tile((id \
         4b7a67f8-cec0-4c7a-8d10-6458b2f8c77a)(label(init))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         0afcfe9a-e198-4515-b9fb-660869e4af3c)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 24))(sort Pat))((shape(Concave \
         24))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         e2590a7f-d75b-40b7-a04f-ea786295756d)(content(Whitespace\" \
         \"))))(Tile((id \
         21555f35-ce2b-4a93-9a08-93a434d21c51)(label(Model))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         6f9103e1-3686-4841-946f-68c1c933e89f)(content(Whitespace\" \
         \")))))((Secondary((id \
         e2711743-e3c1-4eac-9100-37715618b74f)(content(Whitespace\" \
         \"))))(Tile((id \
         f53ef007-5e9e-4515-b577-c4040f8bd013)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         a2a8f03f-a4c6-40ca-8e0c-a090639b466f)(content(Whitespace\"\\n\"))))(Tile((id \
         a7bddd30-3691-4c46-9905-333d8b8a0f71)(label(canvas))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         1f88b432-4953-45e6-bce4-094436a94621)(content(Whitespace\" \
         \"))))(Tile((id \
         b3ab8bf5-52cf-4aa5-a00d-518d90b3e8f2)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         baee5769-6a20-4328-97f9-30bc498a7fdc)(content(Whitespace\" \
         \"))))(Tile((id fb2a510f-c1cd-415c-9922-3fd350d8ed8c)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         125fc1eb-c734-4881-8121-02308e5f0510)(content(Whitespace\"\\n\"))))(Tile((id \
         163e4e1a-ed88-4758-be8f-94f3f52794e7)(label([ ]))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         bed76697-63bb-4e77-87ed-448daa8449d6)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         d3da4e05-8c23-4df1-9b02-be35babc26cc)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         adc26422-edb6-45cf-9660-9c7048d27418)(content(Whitespace\" \
         \"))))(Tile((id \
         b3d9c793-6827-4256-b90d-c55993513d1f)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         69208802-14d3-439c-95a2-2b9ec64ffa80)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         4d03631a-766f-42e9-9ae2-71a2e56795f2)(content(Whitespace\" \
         \"))))(Tile((id \
         18ada37d-e259-4ff2-9660-5bbdfef42af7)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         cadb69d2-ecc0-4399-be40-82f1815ff07b)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         78badfdc-ff26-4197-955f-b9aae52c6c13)(content(Whitespace\"\\n\"))))(Tile((id \
         b2b30743-88be-455f-b9eb-5f49f15b6e32)(label([ ]))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         69416af4-5694-42fd-966a-2bd8aa502cda)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         8fcdea7f-5677-49c1-8143-6ca4fb89ef2f)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         c2818a61-e226-4b9c-885a-51a4f8f22f7e)(content(Whitespace\" \
         \"))))(Tile((id \
         ffb0417b-c92c-4463-acab-6a1247c04284)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         3bf83919-e25f-431a-bb25-57f7dbf9e5b8)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         c72db68c-20eb-49d1-8926-b1958475c8b7)(content(Whitespace\" \
         \"))))(Tile((id \
         fbcf2e38-47f3-444f-8a97-0125b81232e4)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         0e94af5b-f962-4488-9e77-ab4aef21794e)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         a312405f-3f43-49c6-94e6-9d904bfaaa68)(content(Whitespace\"\\n\"))))(Tile((id \
         3fa689b0-f352-43dd-ae82-bec878fd76c5)(label([ ]))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         2a48ee46-8b5a-448f-8582-848eab8b2afd)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         a283c807-2fcd-45ed-b631-7b23c6589a70)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         dfcd4984-b396-4ff2-a8c0-181f4308ccbc)(content(Whitespace\" \
         \"))))(Tile((id \
         89ea3d24-00e2-459b-8332-5c6a88912e1f)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         6c53c087-581a-481e-8024-fdefa074ad40)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         eeb70858-1f70-4c4f-beea-b5ad17e1083b)(content(Whitespace\" \
         \"))))(Tile((id \
         e2fab23e-5f7a-4008-aeea-f0c135346ee7)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         4ad6f81d-bbb8-4d2e-97c7-ac8f25613e37)(content(Whitespace\"\\n\")))))))))(Tile((id \
         af778342-0cb7-4f9d-ac21-7c6d86be675c)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         51cf9955-d641-46ae-8558-92763703a7c2)(content(Whitespace\"\\n\"))))(Tile((id \
         0d4d1aa5-b4f0-4818-8456-b1e4f13fe5f4)(label(brush))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         54a865fb-df6e-43a8-8632-2b0535056786)(content(Whitespace\" \
         \"))))(Tile((id \
         bfd23bde-4161-4948-98d3-731e0213e455)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         03dc24b2-4c90-4b51-b4c4-6de42c29d7d2)(content(Whitespace\" \
         \"))))(Tile((id \
         176fd15b-215a-4b0f-accc-a89212a853b9)(label(\"\\\"\\240\\159\\142\\168\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         4c502625-dc45-4306-a195-9f5555cf807c)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         4519b810-e096-4263-990e-01c62dfd2d2f)(content(Whitespace\"\\n\"))))(Tile((id \
         6ca1d105-12aa-49dc-bcb2-435b72d6e967)(label(palette))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         9b286d6a-22bf-41ed-be20-515bea508eff)(content(Whitespace\" \
         \"))))(Tile((id \
         60d6b84a-3015-4230-86f2-9890a01693c0)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         0304e89c-cfdb-4872-b322-182b2e61a72c)(content(Whitespace\" \
         \"))))(Tile((id ab9a5dfa-11d7-41d3-be2a-51043ecbbba7)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         6ae772e7-d162-4866-9336-94b467894c19)(label(\"\\\"\\240\\159\\142\\168\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         cd9e846c-41b0-432d-a131-79041fa2f5db)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         1143c12c-f00a-46ec-8788-45213eb24bac)(content(Whitespace\" \
         \"))))(Tile((id \
         10d2f2c3-03a2-43f6-8566-39680ca02b4b)(label(\"\\\"\\240\\159\\140\\159\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         0603e34d-d68e-4742-a4d2-749cf6980a11)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         6562bf32-74d6-4dca-801f-7a754d71fba9)(content(Whitespace\" \
         \"))))(Tile((id \
         4378ab57-8044-494b-8105-a146daae6659)(label(\"\\\"\\240\\159\\146\\156\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         45e8bf00-fba8-4f27-b8b6-b4b836663a10)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         6f48a58c-d7c5-4d65-a831-cfe4264d5c37)(content(Whitespace\" \
         \"))))(Tile((id \
         c629cf1a-4190-4794-a708-ce003f50c7f8)(label(\"\\\"\\240\\159\\148\\165\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         0e91d140-eafe-4a73-b16d-fcc9dc1fbd34)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         62c9e875-89a1-4c5c-8453-7c3baf19550e)(content(Whitespace\" \
         \"))))(Tile((id \
         a1b4e168-32cb-4875-b9e6-08d369025a07)(label(\"\\\"\\240\\159\\140\\138\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         d7cbddb1-4fb1-4f0d-bf1d-d3b3c3b21ae3)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         44899356-4ca0-4b4d-8477-881a820181a6)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         e0705820-9577-45e0-87f2-c1ba355aa4e2)(content(Whitespace\"\\n\"))))(Secondary((id \
         c22b77be-6664-42d0-9263-d62342dcf86f)(content(Whitespace\"\\n\"))))(Tile((id \
         e3209264-4f91-4206-b8a4-0b750cf65b0d)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         993dafbe-19ac-4c9f-bf39-b678a5ebfdc4)(content(Whitespace\" \
         \"))))(Tile((id \
         62d7767d-0f65-407a-9894-a5f619a19013)(label(setCell))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         7329d003-6814-4417-8a46-a5ed68b3bc5c)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 24))(sort Pat))((shape(Concave \
         24))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         01c0f865-6c51-4898-be72-df0f25f55867)(content(Whitespace\" \
         \"))))(Tile((id \
         3c486a6a-4641-4abd-9a1c-f35a9291ad23)(label(\"(\"\")\"))(mold((out \
         Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0 1))(children(((Tile((id \
         829657f9-7a24-447a-8cbe-4963e7a7f1ae)(label(Canvas))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         4a443f71-460b-4acb-9dcf-368938fe29c7)(label(,))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 44))(sort Typ))((shape(Concave \
         44))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         93ec8c20-e92d-43b7-b11a-aa18493c0df8)(content(Whitespace\" \
         \"))))(Tile((id \
         332c1338-c2a9-4eb7-bf9d-ae59442ca308)(label(Row))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         fca68009-9407-4fb1-802e-8c3a6e9f6b8c)(label(,))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 44))(sort Typ))((shape(Concave \
         44))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         9b7d9ee8-c166-4f03-86d4-3836d16aba10)(content(Whitespace\" \
         \"))))(Tile((id \
         1d1670dd-3b81-450a-bbbe-ac942f60c1e6)(label(Col))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         82462b4d-d572-4852-8ef3-1f1b91643112)(label(,))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 44))(sort Typ))((shape(Concave \
         44))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         c28e984d-98d9-45d1-af47-12cc62101c97)(content(Whitespace\" \
         \"))))(Tile((id \
         1720b9f4-3421-445c-90a1-347c4353ed9e)(label(Emoji))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children()))))))))(Secondary((id \
         1cb9ed02-a30e-4c68-861b-59fbf8360e22)(content(Whitespace\" \
         \"))))(Tile((id \
         9e232241-0a73-4f89-b0c3-81eb7f525a40)(label(->))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 13))(sort Typ))((shape(Concave \
         13))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         4ab254e4-1f7f-4162-8049-8a2430c85abc)(content(Whitespace\" \
         \"))))(Tile((id \
         758668fd-2ba8-4b40-a3d5-6ef767923a64)(label(Canvas))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         6d07bb0a-dbce-44dc-ab14-e47fda9bf1b2)(content(Whitespace\" \
         \")))))((Secondary((id \
         89436900-4f4e-4249-aa62-08c0ed84b647)(content(Whitespace\"\\n\"))))(Tile((id \
         3f026a20-d5b7-464c-ada2-af32f5899c85)(label(fun ->))(mold((out \
         Exp)(in_(Pat))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         37))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         c942c2d2-2720-438c-b36e-0009e916b051)(content(Whitespace\" \
         \"))))(Tile((id \
         e83f8b1e-fed1-4a2e-9db1-a00976f80380)(label(canvas))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         db3a3de0-67d3-4b4f-8327-eed1ed1edad2)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 44))(sort Pat))((shape(Concave \
         44))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         d1811333-e9de-425a-b8b1-9c6313a6faba)(content(Whitespace\" \
         \"))))(Tile((id \
         18237f56-2fe3-475f-a4fd-7ea13283e3b6)(label(row))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         d1bf63cc-4059-4888-a9fa-ba27b8e4e45a)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 44))(sort Pat))((shape(Concave \
         44))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         6983d9c0-c3c8-47ce-a755-0b8e20c04e3a)(content(Whitespace\" \
         \"))))(Tile((id \
         3054d834-bb88-40ea-969c-2d8cefff235f)(label(col))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         de9bf341-17b2-43b7-a662-fc2f1436bbaa)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 44))(sort Pat))((shape(Concave \
         44))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         5148b1df-a25b-4bb0-8e05-57c9ca1a50c1)(content(Whitespace\" \
         \"))))(Tile((id \
         cbd7a228-d88e-4872-b5e3-b62bd2c0e73d)(label(emoji))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         4b3bd675-7a72-4c8a-ab8a-357f250a8220)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         5cb1777b-1203-4794-81f7-76ea8f912150)(content(Whitespace\"\\n\"))))(Tile((id \
         882da448-cd25-4136-a980-704a78da637b)(label(mapi))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         bdb8fd7d-faab-4c44-8713-ab70ca1ae87e)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         a6d0ff69-9ccf-414f-b328-c0ab71496b6c)(label(canvas))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         fd08994f-949a-449b-a457-597b5473ac94)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         0cc258da-3b59-4ba1-909a-5dc0dbe02e1f)(content(Whitespace\" \
         \"))))(Tile((id 899e63b3-0666-4adf-b70b-870f6c2c08df)(label(fun \
         ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 37))(sort Exp))))))(shards(0 \
         1))(children(((Secondary((id \
         73e7ff35-64d9-4955-8faa-7af25d0cf2a1)(content(Whitespace\" \
         \"))))(Tile((id \
         959aae4e-424b-4911-98be-c1980a1657cd)(label(\"(\"\")\"))(mold((out \
         Pat)(in_(Pat))(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0 1))(children(((Tile((id \
         d728b0d3-2578-497a-b033-6d65e3951d24)(label(i))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         8a0084ad-14e9-4a9a-8f7c-eb021854d182)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 44))(sort Pat))((shape(Concave \
         44))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         2b8ab65e-c76c-49c9-ba2e-810ce2ac542b)(content(Whitespace\" \
         \"))))(Tile((id \
         20a5bc8a-467d-40e3-8186-10da735e5429)(label(r))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children()))))))))(Secondary((id \
         ef627624-75ac-4e8d-8218-0ce530f34ddd)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         5bae24f4-6c28-4b0a-a19e-11e34095bc91)(content(Whitespace\"\\n\"))))(Tile((id \
         063e7723-82b0-4819-bd38-2ac85867abc1)(label(if then else))(mold((out \
         Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         36))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         1270351c-1882-46aa-a260-81069692064d)(content(Whitespace\" \
         \"))))(Tile((id \
         fb7c6a26-4cf8-40fb-bc94-92efe2f0b81f)(label(i))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         53f06eac-25ee-403b-bcb2-35b19243cee3)(content(Whitespace\" \
         \"))))(Tile((id \
         3066a56a-673e-44a5-b589-df0000416a7e)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         d558afd8-a8aa-47b4-a55c-bca1950c5c5a)(content(Whitespace\" \
         \"))))(Tile((id \
         65729df6-25c9-4e4b-90e5-c82b15c6c55c)(label(row))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         5bd9ec8d-1127-453e-998b-bed4bf44d791)(content(Whitespace\"\\n\")))))((Secondary((id \
         6d83eb89-4ddb-424a-96dd-c54c871c6d9d)(content(Whitespace\" \
         \"))))(Tile((id \
         5f346389-5f67-4e70-b98b-7efb9f3477dc)(label(mapi))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         54dcd1da-5673-45a1-bdc5-d8b48293adf7)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         22fe728c-51db-47e6-9ec7-45332f57da16)(label(r))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         5a9fa2b4-e1f8-4036-9c61-83cf27f0fac0)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         a214315b-3c4c-49c7-ba7b-facfe24b9573)(content(Whitespace\" \
         \"))))(Tile((id 66eb5827-983c-45e1-afde-a4606b6036fb)(label(fun \
         ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 37))(sort Exp))))))(shards(0 \
         1))(children(((Secondary((id \
         c743a245-18bd-4665-a4cb-9c98e84b93e0)(content(Whitespace\" \
         \"))))(Tile((id \
         d56e6180-d9fc-452b-b687-7f57d3a2f36b)(label(\"(\"\")\"))(mold((out \
         Pat)(in_(Pat))(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0 1))(children(((Tile((id \
         3bcc839c-e924-4472-b239-5883c3d59fea)(label(j))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         fe1ac2b0-4226-428e-ba1e-1737eccde190)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 44))(sort Pat))((shape(Concave \
         44))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         74ef375b-f773-4115-848f-46eaa6872075)(content(Whitespace\" \
         \"))))(Tile((id \
         9dd6c0db-f65e-4c28-9855-81a58e3c173a)(label(c))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children()))))))))(Secondary((id \
         48cd43a5-c8c8-493b-97f2-181b834fa339)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         0026a24b-71fc-4601-997a-b9837da5e4b1)(content(Whitespace\" \
         \"))))(Tile((id 9d9566bc-538b-4945-a666-6b6a1c9a0316)(label(if then \
         else))(mold((out Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 36))(sort Exp))))))(shards(0 1 \
         2))(children(((Secondary((id \
         478b51d4-5921-41f6-b911-d96dcd052cc5)(content(Whitespace\" \
         \"))))(Tile((id \
         dd5dd6a1-68a7-4cb7-b9e4-8b39851cd7f0)(label(j))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         fd5b2d81-453f-4fac-9715-9ac5f8a7fd98)(content(Whitespace\" \
         \"))))(Tile((id \
         36868625-5bfe-4fe4-9dda-96fd8d546e2b)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         5246ff3e-7235-4c4d-a0de-0e251aa43d01)(content(Whitespace\" \
         \"))))(Tile((id \
         955eeb91-724d-4942-b3bf-b7870050f390)(label(col))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         14cb233b-66ca-428a-9b48-582e0e4acb3d)(content(Whitespace\" \
         \")))))((Secondary((id \
         e53c25f5-fbdd-4c6e-932a-3f514f58789a)(content(Whitespace\" \
         \"))))(Tile((id \
         5cb18b33-f218-4338-b571-8375d86ce8b7)(label(emoji))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         f97f2de9-ce2b-4a82-8778-c0f023cf8f1d)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         a5a578fc-11df-4a02-8878-3442646fa05d)(content(Whitespace\" \
         \"))))(Tile((id \
         ccbe8d72-a628-4424-87e1-261fc5ca70d3)(label(c))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         3127e263-e39d-4012-959c-a76ce88da2d4)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         3f023d3b-225d-4522-8976-8a9400350184)(content(Whitespace\" \
         \"))))(Tile((id \
         fe4bd01f-ff7e-464d-b3bd-e7127fc64736)(label(r))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         88ef2de7-48f6-424e-88bd-b8fb7965ae44)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         7694c5d3-a723-4c8d-91b4-d5c2a4b5439a)(content(Whitespace\"\\n\"))))(Secondary((id \
         1a67c3a6-566e-4006-9cb4-97d614fda19c)(content(Whitespace\"\\n\"))))(Tile((id \
         132d7440-d7a9-4cf2-98b5-348b453cdad6)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         3a428e57-f799-4ce9-b1fd-9665c2616059)(content(Whitespace\" \
         \"))))(Tile((id \
         b5f13262-6a0a-473f-ab28-83deddeb424d)(label(setRow))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         ff3476d5-c4e7-47f1-9d14-a793d1191f5a)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 24))(sort Pat))((shape(Concave \
         24))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         0ef4086a-41a2-4216-b362-0e6ebc048ec0)(content(Whitespace\" \
         \"))))(Tile((id \
         e70f9133-40ce-4833-b406-91e59340db73)(label(\"(\"\")\"))(mold((out \
         Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0 1))(children(((Tile((id \
         dbdbcf6b-3793-4720-a6b1-4c8ab0631bd6)(label(Canvas))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         d9363334-12bd-4cde-9e3c-2a9853591bca)(label(,))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 44))(sort Typ))((shape(Concave \
         44))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         2284521f-9437-4d8a-ae57-65c46e3f5623)(content(Whitespace\" \
         \"))))(Tile((id \
         f07214ff-18c6-4793-86e6-963dd7db5b9b)(label(Row))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         8c34df7d-3ad5-4581-9e64-7fe487144d62)(label(,))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 44))(sort Typ))((shape(Concave \
         44))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         3d724d9f-712c-40f1-b9e2-b296ea30a2fd)(content(Whitespace\" \
         \"))))(Tile((id \
         329e1b77-da81-4a89-8ed9-06e6455649ca)(label(Emoji))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children()))))))))(Secondary((id \
         1c98bbd7-c1be-4dd1-8004-414738820273)(content(Whitespace\" \
         \"))))(Tile((id \
         aa17e142-e67f-4ce8-8c7c-fc1523f2aeec)(label(->))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 13))(sort Typ))((shape(Concave \
         13))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         45d37676-d1e2-4330-8f6b-67f653e77c24)(content(Whitespace\" \
         \"))))(Tile((id \
         dbbe2eec-fa92-4ccb-b8a5-3234c9ef5282)(label(Canvas))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         500de475-c160-4231-b98c-f6730e2c3af4)(content(Whitespace\" \
         \")))))((Secondary((id \
         e5a66aa8-4f95-424f-8cdb-b714dd688dae)(content(Whitespace\"\\n\"))))(Tile((id \
         05ba5e98-29c3-4a77-83dd-4b8978d094f7)(label(fun ->))(mold((out \
         Exp)(in_(Pat))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         37))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         79511b25-a33a-45a3-968c-74e8c0dd3d31)(content(Whitespace\" \
         \"))))(Tile((id \
         e848a0d2-e8ea-4287-8dc5-3fb8a1184387)(label(canvas))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         2f9811e3-9807-401f-8053-e26fff970091)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 44))(sort Pat))((shape(Concave \
         44))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         5d3b6fa4-2e97-4f5b-9c65-53de406c1af9)(content(Whitespace\" \
         \"))))(Tile((id \
         412ccac3-ae73-4a07-8e41-215cd03745b7)(label(targetRow))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         48dde15e-852d-418b-ac68-ac5a647a080f)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 44))(sort Pat))((shape(Concave \
         44))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         750c8e50-96c5-4bb1-ba64-549576900d10)(content(Whitespace\" \
         \"))))(Tile((id \
         979372d5-7b02-4d0a-b386-d8816fc6d685)(label(emoji))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         7e01a21a-facf-4cd9-940b-c032e36a0e18)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         f0c41398-661c-4bc3-b971-a10f53522bb3)(content(Whitespace\"\\n\"))))(Tile((id \
         4bd6fa52-bce8-47ed-a458-8465d1709a1d)(label(mapi))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         1185a8f7-8687-487c-9a97-c58828a5343e)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         162234d0-64a2-4b73-8cf6-4bc2a306dc16)(label(canvas))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         49baf44f-7f64-4712-ab9a-03d36100c3a0)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         53a76cc7-ae26-47e1-a222-1a834864ed2b)(content(Whitespace\" \
         \"))))(Tile((id fb43cd91-ba86-4fed-8882-3dd268e6a353)(label(fun \
         ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 37))(sort Exp))))))(shards(0 \
         1))(children(((Secondary((id \
         b0dbcdb8-1f78-4971-b227-4df850e51b2d)(content(Whitespace\" \
         \"))))(Tile((id \
         2ce7558f-d418-4c6f-a58a-ea59b3ed03e1)(label(\"(\"\")\"))(mold((out \
         Pat)(in_(Pat))(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0 1))(children(((Tile((id \
         7ab29f4b-7f90-46f9-a71e-04d309a037dd)(label(i))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         f0458330-cf85-424c-90a9-136d5a1fedca)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 44))(sort Pat))((shape(Concave \
         44))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         0964c4b6-a0dd-46a7-bb9e-09d62874ea57)(content(Whitespace\" \
         \"))))(Tile((id \
         dff18cb3-9f61-4c08-89e1-edf86e61056f)(label(row))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children()))))))))(Secondary((id \
         dad98ed5-5739-44d3-b9c2-c2df12d4cf31)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         d0dbcd04-d5c3-4435-958a-9e05107c5d67)(content(Whitespace\"\\n\"))))(Tile((id \
         c4f740d3-943b-43a8-9c67-01a088ee4f4d)(label(if then else))(mold((out \
         Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         36))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         f5669f62-5612-4331-bce0-e0552c732e2a)(content(Whitespace\" \
         \"))))(Tile((id \
         c1220349-b9b5-4b7d-8d07-638b1626b7c5)(label(i))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         d6ab7483-7fe0-4761-8be8-dee67593cef2)(content(Whitespace\" \
         \"))))(Tile((id \
         60d4c1d9-c0cc-4669-be01-303c95463e25)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         f6a4341f-a955-43dd-9019-36faf560ab32)(content(Whitespace\" \
         \"))))(Tile((id \
         c23592c0-b832-4c3f-9e7e-160766e5ee4b)(label(targetRow))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         b0e5117f-b60e-4800-b76c-3f24f4344d7b)(content(Whitespace\"\\n\")))))((Secondary((id \
         38f28eb2-1cb0-4ef6-b5de-b66f52da4675)(content(Whitespace\" \
         \"))))(Tile((id \
         b094ffcd-67a8-4297-add0-1c832addd7be)(label(map))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         ccdc90ec-37fd-489f-9a94-38bed5ad6532)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         d916ec58-2936-4c94-a792-86c3f30c4f9a)(label(row))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         c7c2b1cc-3ae7-4436-9e41-07cb1eb99836)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         79608378-6743-471e-8647-38c57aaf5a9c)(content(Whitespace\" \
         \"))))(Tile((id 9a9b1bb9-76e2-4b24-80e9-ce607b6ee0e7)(label(fun \
         ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 37))(sort Exp))))))(shards(0 \
         1))(children(((Secondary((id \
         706a6c8b-2b2a-4f9b-b475-df9674d19253)(content(Whitespace\" \
         \"))))(Tile((id \
         7cd965dd-fb6c-4230-93a8-b19066f00c33)(label(_))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         43f7df47-76a1-4a1c-b8b7-c7cb8c37b8f1)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         98c785ae-353d-437e-87e2-188ea634d99f)(content(Whitespace\" \
         \"))))(Tile((id \
         98325ebb-f970-436c-98a3-faf06644021f)(label(emoji))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         f82dab43-59bd-466f-bf65-dd3108f04575)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         6d165638-8487-4349-b718-1f113af79e6b)(content(Whitespace\" \
         \"))))(Tile((id \
         3f465b64-f66a-426f-8627-a2b5fa29d478)(label(row))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         af40e408-518f-455f-ab28-89cd2cdfdb8e)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         ce419380-3031-4175-9bdd-2d5bb246563a)(content(Whitespace\"\\n\"))))(Secondary((id \
         421df4bd-7d0c-4cb2-a1c1-5db815feaa63)(content(Whitespace\"\\n\"))))(Secondary((id \
         3df69fef-4d36-44cf-a288-6076bf7a8a8b)(content(Comment\"# TODO: Add \
         setCol helper here #\"))))(Secondary((id \
         10c021cd-79db-4cbc-8203-700e4b5f6c2e)(content(Whitespace\"\\n\"))))(Secondary((id \
         7f4cb1b0-e2de-40d0-9067-f2782e221386)(content(Comment\"# Hint: You \
         need to modify each row, changing #\"))))(Secondary((id \
         471308b5-3ebe-4485-bac5-3c04666795d2)(content(Whitespace\"\\n\"))))(Secondary((id \
         5b8d29f5-9db3-46e9-adc2-3e8baae5dc30)(content(Comment\"# only the \
         cell at the target column.         #\"))))(Secondary((id \
         2eb3f96c-62a2-4a17-aeac-a99c0000272c)(content(Whitespace\"\\n\"))))(Secondary((id \
         ed1ba0a8-4d3c-4f4a-af6c-8fe2b23752ed)(content(Whitespace\"\\n\"))))(Tile((id \
         bbda38e0-1a31-42c5-b0d7-4ce00986a114)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         74f46633-4efb-4a02-aea1-9d064ab34a9c)(content(Whitespace\" \
         \"))))(Tile((id \
         a1dc9490-a119-40b5-aaa6-0624b8f2bfc8)(label(setAll))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         7fa28264-2156-492a-9bc0-bb854154efdc)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 24))(sort Pat))((shape(Concave \
         24))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         ccd3e7db-f10b-4a3f-b5a6-348dcf39f16a)(content(Whitespace\" \
         \"))))(Tile((id \
         ae2d71ea-920a-4b2f-a4b4-cb29c3ee972e)(label(\"(\"\")\"))(mold((out \
         Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0 1))(children(((Tile((id \
         20fe4c64-4760-4dc8-b2ec-5181693fd32f)(label(Canvas))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         77dddf23-f8ef-41f2-a1dd-4b2b2da2404b)(label(,))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 44))(sort Typ))((shape(Concave \
         44))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         03118374-ae34-4053-af3f-7649be279449)(content(Whitespace\" \
         \"))))(Tile((id \
         75fd39ad-a503-4e07-b053-bbdc9865d8a6)(label(Emoji))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children()))))))))(Secondary((id \
         ac2c4838-9860-4f39-bfc7-f2c6af4a2f1b)(content(Whitespace\" \
         \"))))(Tile((id \
         b1506c33-ba6d-48ab-b3ec-f97cc28dcd5b)(label(->))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 13))(sort Typ))((shape(Concave \
         13))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         dcc61ad6-062d-45f2-ba2f-cd53c18b4cbf)(content(Whitespace\" \
         \"))))(Tile((id \
         662f2d68-576e-404e-86bc-bde84559140c)(label(Canvas))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         5a8b3a9c-9dd7-4210-80c9-15912fe21490)(content(Whitespace\" \
         \")))))((Secondary((id \
         67cdcf97-07b5-402b-8945-5b1001a685e5)(content(Whitespace\"\\n\"))))(Tile((id \
         a2326b84-3208-4987-aac3-71f01cb782ef)(label(fun ->))(mold((out \
         Exp)(in_(Pat))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         37))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         937452fb-5001-40db-a897-e7f4332a7ed5)(content(Whitespace\" \
         \"))))(Tile((id \
         ee09ddc0-d684-4a80-b23a-b53839dbc859)(label(\"(\"\")\"))(mold((out \
         Pat)(in_(Pat))(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0 1))(children(((Tile((id \
         5d1f1a84-41c3-45c8-9f27-7386ccaa67f4)(label(canvas))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         5995e6ad-d3cf-4587-9d5d-a4511ab86b46)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 44))(sort Pat))((shape(Concave \
         44))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         899bd2ee-e849-40b4-be62-6d9861809d8c)(content(Whitespace\" \
         \"))))(Tile((id \
         1fa04208-3012-4c5d-8ee1-c09803820f38)(label(emoji))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children()))))))))(Secondary((id \
         153daf1c-e593-49fd-8b8d-5db32444bd4f)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         3c5a0859-7592-4cba-add4-71b0d3dd20b1)(content(Whitespace\"\\n\"))))(Tile((id \
         3bfd506d-1a71-4d9e-a1ad-4ed95822cb6f)(label(map))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         a6e46d45-21d0-47c3-ac09-d7d6378e4fbf)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         75068a70-f985-4f18-9cfa-725ab38deabf)(label(canvas))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         6867a608-5049-496d-a829-82ad0a20aa82)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         e60c47d7-c2b3-4a83-aecc-a0a0b2e80604)(content(Whitespace\" \
         \"))))(Tile((id d2022b32-0f5c-4309-9985-bea1d8181e84)(label(fun \
         ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 37))(sort Exp))))))(shards(0 \
         1))(children(((Secondary((id \
         3a803dfa-c542-4673-addf-cd3367d3e14b)(content(Whitespace\" \
         \"))))(Tile((id \
         25b5663a-80c0-4f74-8a59-9693e2241250)(label(row))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         28f2a3f0-fb9e-4473-9233-e021fe80244c)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         278e921f-cd3a-47be-8311-b39c79a8c197)(content(Whitespace\" \
         \"))))(Tile((id \
         22d74775-c2cb-4e04-b1c9-79199548d218)(label(map))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         275cc055-fa05-4c05-88fd-2e27cb3259c9)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         41a09eef-5bfa-4a31-ac0f-ce91b7cf834c)(label(row))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         c23153ef-c1d3-42d4-93d8-e99a3049b967)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         979218a0-d8af-435c-bd28-ad58c3504c5b)(content(Whitespace\" \
         \"))))(Tile((id 9332c67d-eb62-4fc8-99bb-4b31cf8f2bf4)(label(fun \
         ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 37))(sort Exp))))))(shards(0 \
         1))(children(((Secondary((id \
         515c160e-bfd2-490f-8cb2-d081789cd783)(content(Whitespace\" \
         \"))))(Tile((id \
         95142034-6e0c-4296-81e6-ee63d1c769c2)(label(_))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         ab90bed0-136f-46bb-97de-2652c4189128)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         950220b7-d06b-4701-b724-32692b4194fd)(content(Whitespace\" \
         \"))))(Tile((id \
         fd13e753-f6bf-4e59-ac93-7d39b21eaaec)(label(emoji))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))))))))))))(Secondary((id \
         d375e3cc-7fd7-446f-81c7-799d24a214c7)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         a6707668-b610-439b-87cf-4fb0146098fd)(content(Whitespace\"\\n\"))))(Secondary((id \
         04967d6d-8feb-4e62-af5c-126efc405808)(content(Whitespace\"\\n\"))))(Tile((id \
         f85a6613-5849-4c1d-9394-495125204153)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         9c21b5b1-ec82-43bf-85b6-3263ef5a4e6d)(content(Whitespace\" \
         \"))))(Tile((id \
         fdd2a7b2-b949-4525-a285-28c2224b6794)(label(updateGrid))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         b751bf36-b99e-407e-bfda-7f6912d1356c)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 24))(sort Pat))((shape(Concave \
         24))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         bd4fcfbb-3d68-476d-8713-40c0683dd80b)(content(Whitespace\" \
         \"))))(Tile((id \
         105ea3f7-6c4f-47e5-a657-bf1f182e2b2d)(label(\"(\"\")\"))(mold((out \
         Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0 1))(children(((Tile((id \
         6cff605d-7e0d-4179-8cc2-2f5d0512100d)(label(Model))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         bf9be7c5-eba8-4f12-ba3b-7594369f3766)(label(,))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 44))(sort Typ))((shape(Concave \
         44))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         63386a60-d29f-45aa-913f-6f2b319fbac8)(content(Whitespace\" \
         \"))))(Tile((id \
         77641fd6-8d1c-4cda-9872-0486945f426e)(label(Canvas))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         90b44aeb-69de-4cbe-80f2-9c6b798ebe8d)(content(Whitespace\" \
         \"))))(Tile((id \
         31a70bef-437a-4199-b70c-63b9f0bb2665)(label(->))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 13))(sort Typ))((shape(Concave \
         13))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         d476db3b-705f-421c-b833-411fc6610e54)(content(Whitespace\" \
         \"))))(Tile((id \
         b8a42e2c-cbc6-4811-8605-66444e203a18)(label(Canvas))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children()))))))))(Secondary((id \
         13da0aa5-b591-4238-ad3f-f7f0456ca36b)(content(Whitespace\" \
         \"))))(Tile((id \
         f6c4862f-34e3-4a19-8c38-aa1c436d6cab)(label(->))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 13))(sort Typ))((shape(Concave \
         13))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         265a0380-5ac2-4026-86b8-13baba1305cd)(content(Whitespace\" \
         \"))))(Tile((id \
         579e6647-0d18-42c9-bc6f-f2947af7407c)(label(Model))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         ca359594-3586-4c02-85c2-a7a3fdb0c3b9)(content(Whitespace\" \
         \")))))((Secondary((id \
         d5bcba27-c9ac-4ed8-a5a8-5dc360dc1f95)(content(Whitespace\"\\n\"))))(Tile((id \
         d7d9dc01-22bd-4ece-b3d2-ea3ca3e22d51)(label(fun ->))(mold((out \
         Exp)(in_(Pat))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         37))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         70b94fde-a482-4de9-b538-ca4999b6438d)(content(Whitespace\" \
         \"))))(Tile((id \
         3191e6b0-b57c-4bfb-945e-6cbc2c1d34ea)(label(\"(\"\")\"))(mold((out \
         Pat)(in_(Pat))(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0 1))(children(((Tile((id \
         b72c5bb3-af2c-489f-9274-5e5d534d529d)(label(m))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         b69ae362-8317-4182-81db-56f53885335d)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 44))(sort Pat))((shape(Concave \
         44))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         7e7fc724-11a0-41bf-9acd-ccc5147d42cf)(content(Whitespace\" \
         \"))))(Tile((id \
         08130298-f944-4254-91ea-bea62d40ee7c)(label(f))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children()))))))))(Secondary((id \
         7edadfdd-3f5f-40ec-9000-9f542242d47f)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         0b2efd39-d6ee-4e8f-b1de-b02c9598c6b6)(content(Whitespace\" \
         \"))))(Tile((id \
         55f65bb6-9b1b-4853-b4ab-9b239708e32f)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         87c16078-6f38-4bb6-b54a-6836616a2ec9)(label(f))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         783ea4e2-bb2e-4c0d-8bf5-8e7cb58faf2e)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         e1cc504c-fe66-4025-8896-88ece94e5285)(label(m))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         8c7f8d0d-4e86-463e-8351-5d0617d66d4c)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         1efbf1fb-1c01-47f1-bb0d-b5e0870e3612)(label(canvas))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         97ad6bff-e482-4662-ab0b-6c5863d60aa0)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         2ae637bd-25fc-43da-914c-b07870fb1ff6)(content(Whitespace\" \
         \"))))(Tile((id \
         4c618507-0dde-4c74-9d8d-0a6b461a5ab6)(label(m))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         c03d5f4f-fb62-459a-8196-eab264390a38)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         ac9440df-e874-49f5-8e62-83c75725d925)(label(brush))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         fbc187b6-be9d-4e2f-ac20-c9580715e33a)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         9c536532-f33a-4914-bc24-5ff15fbed3fb)(content(Whitespace\" \
         \"))))(Tile((id \
         c29a12b1-f0b2-4af4-ab4a-e273491f4259)(label(m))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         3745e34d-0689-432c-a850-5890bc331c89)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         650a484b-e5de-4b24-8fc6-0d5b8f507fa0)(label(palette))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         1aec1280-4ba2-4ae5-aa42-8d22e7ba2394)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         230b7e2d-944b-4950-8be2-5cab05124e3e)(content(Whitespace\"\\n\"))))(Secondary((id \
         6b606626-b202-4729-9fa5-d58dc9d6740b)(content(Whitespace\"\\n\"))))(Tile((id \
         f1b72921-af27-4c9f-961d-8224fb65a69b)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         8babdbb1-5845-4c5a-8843-5959f8b458eb)(content(Whitespace\" \
         \"))))(Tile((id \
         2b4aa3af-7c6a-4377-a848-a6d07f5cd642)(label(update))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         190d1ea9-01fd-4e09-b6a4-3106698c7738)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 24))(sort Pat))((shape(Concave \
         24))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         8f0670bb-b52c-47eb-b6ad-6618be48f5a1)(content(Whitespace\" \
         \"))))(Tile((id \
         18d138bf-04be-409a-8052-35d6f5c5994a)(label(\"(\"\")\"))(mold((out \
         Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0 1))(children(((Tile((id \
         3f4976e0-ce0b-4e7d-a717-cf4c1a1b5781)(label(Model))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         80312040-d187-4cb3-8828-c81719e257b1)(label(,))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 44))(sort Typ))((shape(Concave \
         44))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         28905691-192c-4fc7-b4e1-e171ad320a15)(content(Whitespace\" \
         \"))))(Tile((id \
         646b1b2b-dcdb-479c-bdbe-95701803d23e)(label(Action))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children()))))))))(Secondary((id \
         2048792f-ffc7-4432-98b4-dc290d9f14ff)(content(Whitespace\" \
         \"))))(Tile((id \
         327d36ea-7e20-4827-a617-491fc644d140)(label(->))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 13))(sort Typ))((shape(Concave \
         13))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         703b29e5-a4db-45b8-babb-5366ec0d507d)(content(Whitespace\" \
         \"))))(Tile((id \
         9029d40d-9e25-40c5-a349-75d518bf669f)(label(Model))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         014afa66-fa04-4f97-b412-22bbfa92f0be)(content(Whitespace\" \
         \")))))((Secondary((id \
         f50d57c3-fb5d-4a16-9532-f1b49d1c9b81)(content(Whitespace\"\\n\"))))(Tile((id \
         737cf061-645d-4891-934d-fcfcc4749f48)(label(fun ->))(mold((out \
         Exp)(in_(Pat))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         37))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         6c6253fc-01c8-44c2-92f9-02b8225ef4d6)(content(Whitespace\" \
         \"))))(Tile((id \
         834722ef-0bd9-45b9-8a41-6190f9daf391)(label(m))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         374cb3de-cef9-484c-ad05-b4f393e1bc8a)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 44))(sort Pat))((shape(Concave \
         44))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         2ff4b596-6635-46fb-8048-f2f2281da750)(content(Whitespace\" \
         \"))))(Tile((id \
         513bacab-38a7-4baf-8906-9766a65fc7e1)(label(action))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         f5d88174-87de-4b86-af40-266e3d5e053a)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         b5c6c896-e7b1-4ef3-9171-0444ba1a3fa6)(content(Whitespace\"\\n\"))))(Tile((id \
         505cd83f-ce30-4579-b5b3-e4261056e4cb)(label(case end))(mold((out \
         Exp)(in_(Rul))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         8adcb78d-19e0-4912-8e02-f43f7607eff1)(content(Whitespace\" \
         \"))))(Tile((id \
         14b2b7d4-d45b-4847-8fde-dd131feddc3f)(label(action))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         9d4b753b-b46e-4dc5-b0ce-09794c916daf)(content(Whitespace\"\\n\"))))(Tile((id \
         3887dca1-1ba9-4ca7-8084-8631199d6284)(label(| =>))(mold((out \
         Rul)(in_(Pat))(nibs(((shape(Concave 46))(sort Exp))((shape(Concave \
         46))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         47ea7dfb-e135-48f3-afbd-d023164c13fd)(content(Whitespace\" \
         \"))))(Tile((id \
         ee1792ec-df72-4828-95fc-978c9908d953)(label(SetBrush))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         067ada7e-dbeb-44cf-a1f9-909c45f98eb6)(label(\"(\"\")\"))(mold((out \
         Pat)(in_(Pat))(nibs(((shape(Concave 23))(sort Pat))((shape \
         Convex)(sort Pat))))))(shards(0 1))(children(((Tile((id \
         bfaf3cd5-ad10-43bd-a87a-803aaace53aa)(label(idx))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children()))))))))(Secondary((id \
         a2d92a98-973f-44c1-ac5a-ae92e945f538)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         2cb3ad61-dad5-47c6-9b18-d9695d45dedd)(content(Whitespace\"\\n\"))))(Tile((id \
         fd37963a-396f-459d-b579-6eeb89e60c99)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         b572b129-39cb-4e08-ad66-0bda2b2beadc)(label(m))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         c87f129e-2cbb-457a-9cc1-e5afccfa0fd5)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         28c2fa37-c5c8-4391-a295-296ca08f942b)(label(canvas))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         7d987031-9620-436f-8076-fbb59b91eb3e)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         568260e4-03cc-4d86-b17f-fd122f93dc01)(content(Whitespace\" \
         \"))))(Tile((id \
         499c89a2-fe3c-432c-9bc2-1adfaef94bc1)(label(nth))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         0f0f1d72-465e-4197-8885-2c24b95b3c67)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         d875823e-7bc4-4418-bf7b-ea0350b52739)(label(m))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         ba5c5332-6fce-4135-b286-14b0d0e4a52b)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         4f76c98b-3c9e-468b-98da-28c25654cf5f)(label(palette))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         478427dc-c100-45b9-8cfb-13fc84aedff7)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         7a53b500-aa62-49c5-a0fd-aaf40204da25)(content(Whitespace\" \
         \"))))(Tile((id \
         ae3d17b5-38f4-4232-8a8e-d204e24c4403)(label(idx))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         c558393b-1a43-4c3c-90e6-aece3edd04d3)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         b0f5d84c-e320-4ebd-9095-65a7f6428967)(content(Whitespace\" \
         \"))))(Tile((id \
         1f7fc004-d8e6-4ffa-adef-98c6bae31485)(label(m))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         918950c7-76e7-404a-93c6-136187b90fdb)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         59efd0ba-9c8c-4872-a357-a1dbd38c0851)(label(palette))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         b701218c-c1da-468c-9ab3-90ea24c14cef)(content(Whitespace\"\\n\"))))(Tile((id \
         571df153-4dd3-492a-9ed5-7344a16d8135)(label(| =>))(mold((out \
         Rul)(in_(Pat))(nibs(((shape(Concave 46))(sort Exp))((shape(Concave \
         46))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         be2c4405-a910-4dd1-a9e4-cfa136de5466)(content(Whitespace\" \
         \"))))(Tile((id \
         ec48f528-b29f-478d-b62a-ea85eeaf3807)(label(PaintCell))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         f65e00ac-27b1-409f-b9a2-9a39f7d2b827)(label(\"(\"\")\"))(mold((out \
         Pat)(in_(Pat))(nibs(((shape(Concave 23))(sort Pat))((shape \
         Convex)(sort Pat))))))(shards(0 1))(children(((Tile((id \
         49424505-2ac9-42d1-a403-836974844e5f)(label(row))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         0d70fc5a-cf14-4769-a23b-3111fb1f39dd)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 44))(sort Pat))((shape(Concave \
         44))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         efd4e3d3-aacb-43c3-8276-9a4e64d9730c)(content(Whitespace\" \
         \"))))(Tile((id \
         652f08a2-f2ca-4248-aebd-794914abf6f8)(label(col))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children()))))))))(Secondary((id \
         6220aea3-f9f5-47ab-ac52-3e93758789f0)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         62d3ed3f-aff5-4bf8-8ed0-cc4879e231dc)(content(Whitespace\"\\n\"))))(Tile((id \
         d163f4a6-7339-4f0d-8515-06b37440ebe2)(label(updateGrid))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         8f024fb1-a44f-4357-aefe-72a02d3534af)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         bfaff028-45a9-4c63-aefc-5da316938a35)(label(m))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         7b5cfecc-7ec0-4cbd-a2d9-5ac1f976c241)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         a7e6d5f3-5c74-4dae-b11d-8e5ef78795a9)(content(Whitespace\" \
         \"))))(Tile((id 125b177e-a0ea-418a-828f-5838a7c2c0b8)(label(fun \
         ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 37))(sort Exp))))))(shards(0 \
         1))(children(((Secondary((id \
         c4840c0a-1a6d-429b-b049-ff99ba95f3f1)(content(Whitespace\" \
         \"))))(Tile((id \
         66549932-3d7d-4842-a2e2-1bdb282b6b55)(label(c))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         6e72cc45-c695-4708-954e-0f1e0653756f)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         7eb12140-0837-4537-abf2-496b9b22467a)(content(Whitespace\" \
         \"))))(Tile((id \
         6b5f1f32-d308-4215-b141-f30532787872)(label(setCell))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         f6167880-c1cf-4005-9365-4ee88d1dcca3)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         b835afd7-655a-4a0b-933b-2e654e55d180)(label(c))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         f66ced03-8801-4630-9417-224e1c1d340c)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         32c8d6b3-dad0-4530-91fd-f4d935d11684)(content(Whitespace\" \
         \"))))(Tile((id \
         396c97f9-84e4-42f0-9525-eeab5612a421)(label(row))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         e3968e6c-060d-4db7-bb49-7ea6d1c1d7d7)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         5b167106-cef1-472a-9546-b31c55f21a4f)(content(Whitespace\" \
         \"))))(Tile((id \
         851afc14-26ba-46fa-bde7-77a6509f4fee)(label(col))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         0d69704f-cbcb-490b-8474-9e0d65ff1883)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         8a799661-92f3-4698-909a-96ea8a3e163d)(content(Whitespace\" \
         \"))))(Tile((id \
         2033d79f-4bc8-4c33-aa14-eada903c7c90)(label(m))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         e17fb90b-7bf8-4238-bcef-5d59cdda2b8a)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         0badbca8-55fb-49c0-baa7-b405a93d5037)(label(brush))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))))))))))))(Secondary((id \
         c2c5f4d9-f056-4d2d-b8dd-d07e1441b1a2)(content(Whitespace\"\\n\"))))(Tile((id \
         1693d89d-7f5c-49ba-b917-86121a1af21d)(label(| =>))(mold((out \
         Rul)(in_(Pat))(nibs(((shape(Concave 46))(sort Exp))((shape(Concave \
         46))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         4caa48a6-555d-4beb-abfa-b48929ca09ef)(content(Whitespace\" \
         \"))))(Tile((id \
         39a4fed8-6312-4751-9301-8bf0a74bb489)(label(ClearCell))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         c5f8a4ac-bd57-47a8-bf3d-04525773a713)(label(\"(\"\")\"))(mold((out \
         Pat)(in_(Pat))(nibs(((shape(Concave 23))(sort Pat))((shape \
         Convex)(sort Pat))))))(shards(0 1))(children(((Tile((id \
         7b3bcc21-2e8c-4927-a01d-fe1530743073)(label(row))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         a2fd5b4b-bc46-44a1-b845-35939e274a6d)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 44))(sort Pat))((shape(Concave \
         44))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         05d70c79-cecb-46cb-b506-d5ff41d0744c)(content(Whitespace\" \
         \"))))(Tile((id \
         71b3d7f8-13bc-40fa-bab2-31e7dfef04e7)(label(col))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children()))))))))(Secondary((id \
         c29e2463-359b-4415-b5f5-49a0aca20be1)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         74fba823-9ebe-4693-8ee7-b4bfda7500b5)(content(Whitespace\"\\n\"))))(Tile((id \
         5d78af00-52a4-47cb-931c-95704dfbbb88)(label(updateGrid))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         d65946a2-f86f-4013-8494-6abe7a3f6550)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         cccae9b9-7fd6-487c-874f-71bc0ce84a3c)(label(m))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         7de0c03f-3b62-47a8-a6fe-03361c931ffc)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         72ed070f-4818-4bda-9236-422df9649fac)(content(Whitespace\" \
         \"))))(Tile((id 6df3ea2d-e777-4ec0-a896-004c3d346952)(label(fun \
         ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 37))(sort Exp))))))(shards(0 \
         1))(children(((Secondary((id \
         549735e8-b926-4a0c-b7f1-3f2029975cf4)(content(Whitespace\" \
         \"))))(Tile((id \
         fb600bb5-2a8d-47cd-9a28-f1894692cecd)(label(c))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         dc349061-70a0-44fc-9db9-ceeee83e9474)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         75eca495-d508-4c8f-a340-45cec689fbcf)(content(Whitespace\" \
         \"))))(Tile((id \
         9a6d71a9-0a09-4895-84e5-0ac0f0346ece)(label(setCell))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         d55d6a3a-cbcd-4f29-bd92-c0afa3dd281e)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         ee22ac03-fd32-49db-bf4e-7c7f709b52f3)(label(c))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         f4407ee3-6719-4ad6-83fa-12a220bd3d41)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         02a1c43e-3518-4e65-9a66-5ed936272aa5)(content(Whitespace\" \
         \"))))(Tile((id \
         99264403-b4b8-460f-87a1-ac7e65ce165e)(label(row))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         4399ec83-7383-4c51-92c4-d49fe8d45a18)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         f3fff79c-3b76-4d28-8af0-6d54298e5271)(content(Whitespace\" \
         \"))))(Tile((id \
         77f32346-63c4-4d0e-a732-188012a9b3e1)(label(col))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         53aeb5e8-ca71-49a8-b28c-dee07a973d7d)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         2a4ef78c-dc72-4031-b03d-052fc9917f0b)(content(Whitespace\" \
         \"))))(Tile((id \
         308b3dae-5cad-4557-a0b4-212555e90517)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))))))))))))(Secondary((id \
         c1a250f4-44ae-45d7-b548-f202704b4eb7)(content(Whitespace\"\\n\"))))(Tile((id \
         e7e45293-d5e3-4960-b45d-961d1a41acb4)(label(| =>))(mold((out \
         Rul)(in_(Pat))(nibs(((shape(Concave 46))(sort Exp))((shape(Concave \
         46))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         8d219a2a-257e-4813-8a9c-e809c5157c5e)(content(Whitespace\" \
         \"))))(Tile((id \
         0cf72b69-daac-4411-9505-9c1ee8eaccf9)(label(ClearGrid))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         92015c5b-2ed0-4d6b-b59d-c4068ec2894f)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         c6346b1d-6dec-4636-b120-d5c956a5ac30)(content(Whitespace\"\\n\"))))(Tile((id \
         b575e88b-fdc4-4ccb-aa7b-5a78a91b094e)(label(updateGrid))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         c51b6db1-753b-4904-a91c-03875ae1ded0)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         f51a7e11-1d76-406e-99bc-8a40c3e05841)(label(m))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         1f213dee-8c6d-4d6b-9123-e0c81f04fc6c)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         db9644c4-ccc3-41f4-bec4-ed35b8443aa0)(content(Whitespace\" \
         \"))))(Tile((id 816cacb2-193c-445f-9992-075e1623403c)(label(fun \
         ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 37))(sort Exp))))))(shards(0 \
         1))(children(((Secondary((id \
         136203cb-797b-4b1b-b578-3636bfc247e5)(content(Whitespace\" \
         \"))))(Tile((id \
         f1a1ed4e-456d-43f2-93a2-c91838885146)(label(c))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         2048867f-41bf-485e-94d8-9d36190c1592)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         95f21dcb-d744-4b28-9d8f-eef544f02cc8)(content(Whitespace\" \
         \"))))(Tile((id \
         41f34e66-b90d-4db6-b1de-735b85815f2c)(label(setAll))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         47130b5e-eb93-42d0-9261-769de6e26100)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         02cff8f4-6f09-4a04-af7b-a83b11d91896)(label(c))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         585ec28d-dd43-497d-870e-346af429a559)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         82e06829-407b-4985-916e-e042ad7da1ed)(content(Whitespace\" \
         \"))))(Tile((id \
         5a5aeed3-c5e3-4479-b541-2fb6bab2fa56)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))))))))))))(Secondary((id \
         c34b1316-279c-43b3-b897-6e87f28f887d)(content(Whitespace\"\\n\"))))(Tile((id \
         9e3c7317-7f27-484a-b299-ecc39132126f)(label(| =>))(mold((out \
         Rul)(in_(Pat))(nibs(((shape(Concave 46))(sort Exp))((shape(Concave \
         46))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         5d785b7a-5812-40dc-8d53-c8e7975c6d0b)(content(Whitespace\" \
         \"))))(Tile((id \
         b8cfcf4c-c3c0-405e-85d8-bc72c9a87448)(label(PaintRow))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         97e85a07-c9d8-4b3a-803b-04c45af4ac5a)(label(\"(\"\")\"))(mold((out \
         Pat)(in_(Pat))(nibs(((shape(Concave 23))(sort Pat))((shape \
         Convex)(sort Pat))))))(shards(0 1))(children(((Tile((id \
         a77b7a74-4c2c-494a-a248-2378abec1ebc)(label(row))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children()))))))))(Secondary((id \
         ea6ada06-65d1-4907-a256-4257c523ea75)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         75280309-ca5d-4c30-bde6-badd97b6f09d)(content(Whitespace\"\\n\"))))(Tile((id \
         5fbf4d28-9fb8-4c0e-9742-f31650caa590)(label(updateGrid))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         edca8537-446a-4018-9268-9b3907fae0b2)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         13f0d594-29cd-45a5-8d7e-7be6fb6bdba3)(label(m))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         3a4bfa0a-9b86-42c0-adb2-b4fb85be5011)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         b74631c1-2566-4601-ba9c-412768128ee7)(content(Whitespace\" \
         \"))))(Tile((id 939f44b7-e059-4b95-8173-93220f1e815c)(label(fun \
         ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 37))(sort Exp))))))(shards(0 \
         1))(children(((Secondary((id \
         fc6017f7-a66d-4afa-b5fd-52c7e6906530)(content(Whitespace\" \
         \"))))(Tile((id \
         0dec6928-d89d-4741-a86e-6b58c419f183)(label(c))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         52375d21-b5ee-42bf-aa93-0ed2ebfd15de)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         ea0256e3-ea88-4730-9ce9-c0c353544c62)(content(Whitespace\" \
         \"))))(Tile((id \
         263426b4-5698-4fa9-84e1-1d874f31e32c)(label(setRow))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         af5121ab-1017-45af-9c0c-a333d1c00628)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         f7cdaaa3-120e-4c0b-8120-b10cfe305ee8)(label(c))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         d539113c-003f-494c-abb1-17062588ec2d)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         a62bf7eb-e962-4ccc-8382-5323266b57ba)(content(Whitespace\" \
         \"))))(Tile((id \
         039eb617-adee-448b-afe7-cc5778b4c1e7)(label(row))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         f33fd575-ae1b-42ae-9ded-37ad722b1f68)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         b16f14a8-fe38-4ec9-bd1c-035bf89a8d6f)(content(Whitespace\" \
         \"))))(Tile((id \
         9cf42138-d2ae-429e-b2b3-74f942a33424)(label(m))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         6f0763a2-fdb7-4279-a302-d3bd761e27ad)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         0d5831fb-9305-4822-887d-7db4d85c91f2)(label(brush))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))))))))))))(Secondary((id \
         0096cf05-d4fd-4557-874b-8a465da19576)(content(Whitespace\"\\n\"))))(Secondary((id \
         3ae04bb3-95e7-40f2-8245-97fbebf77e29)(content(Comment\"# TODO: Add \
         PaintCol case here #\"))))(Secondary((id \
         28633ba3-7dbc-453c-9dd8-9f8c498c4316)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         27b39fba-c44a-420c-a427-10ceae3dff02)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         a0c48346-74da-44c4-86e6-be3bc5e399ed)(content(Whitespace\"\\n\"))))(Secondary((id \
         d62adf10-f821-402e-ad6a-b901c58cdc15)(content(Whitespace\"\\n\"))))(Tile((id \
         ea1ac8e2-bdbf-466e-bb12-fa443008d5b2)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         1446c1d7-821f-42a3-a71d-8b97a904cf06)(content(Whitespace\" \
         \"))))(Tile((id \
         d36e67b3-7554-48aa-a56b-d5337eb0fc8e)(label(do))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         4caecfd3-775e-4c14-bc6d-fa3585ef4b9e)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 24))(sort Pat))((shape(Concave \
         24))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         7acccc15-b92a-4f45-878a-346de43e5dbf)(content(Whitespace\" \
         \"))))(Tile((id \
         006258d6-6f41-4cad-bd3c-c0f06a722298)(label(\"(\"\")\"))(mold((out \
         Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0 1))(children(((Tile((id \
         d18509b6-eb4a-4186-9fe4-e5fd5eaaf3e4)(label(Model))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         41adb4de-f4bb-4a2f-ba98-0b26bba5b003)(label(,))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 44))(sort Typ))((shape(Concave \
         44))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         f02ad2b0-0ad6-4cc9-9539-0ebd3a21ec5f)(content(Whitespace\" \
         \"))))(Tile((id f24bf459-7cc4-4e98-b428-49bd0df7eff8)(label([ \
         ]))(mold((out Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape \
         Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
         d5edc01f-5ee6-4e7e-9f0d-16572d7305fe)(label(Action))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))))))))))))(Secondary((id \
         3b39c6cb-062b-41fa-a74a-9d99309b5280)(content(Whitespace\" \
         \"))))(Tile((id \
         1703a2f7-66bd-47f7-a078-eebaaca3e368)(label(->))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 13))(sort Typ))((shape(Concave \
         13))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         d571dfac-fb47-441a-8d40-6e6585533a8d)(content(Whitespace\" \
         \"))))(Tile((id \
         92040c91-b04d-410b-a5bf-fb13318e4735)(label(Model))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         b0ac8aaf-46a7-429a-9b21-8eeccef9f05b)(content(Whitespace\" \
         \")))))((Secondary((id \
         a68afbfb-ead6-4ec5-88a7-6949adcc4911)(content(Whitespace\"\\n\"))))(Tile((id \
         330a0e41-7899-4c2b-bc63-6f56eccce3a8)(label(fun ->))(mold((out \
         Exp)(in_(Pat))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         37))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         bc8e90ba-ac2b-49ba-affc-79fefd8ac0b2)(content(Whitespace\" \
         \"))))(Tile((id \
         a7a1a878-8dca-4902-aecb-09d5f3c7d650)(label(\"(\"\")\"))(mold((out \
         Pat)(in_(Pat))(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0 1))(children(((Tile((id \
         830f236e-dbe7-4660-80d5-808ea88478eb)(label(init))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         a42fc3ef-82ba-4d32-bfa8-f3c18fe89d24)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 24))(sort Pat))((shape(Concave \
         24))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         10f76150-9a17-4c6c-a731-3d190dc67dd1)(content(Whitespace\" \
         \"))))(Tile((id \
         d5153655-2338-420c-a0b9-1dd7548b6260)(label(Model))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         c4a3981f-5d23-4402-92ef-12b3e7ea5fe5)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 44))(sort Pat))((shape(Concave \
         44))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         da9f1017-f0f2-4134-875e-a01fa940b5c3)(content(Whitespace\" \
         \"))))(Tile((id \
         ac2b702b-e568-43f6-b83f-d779b4a32926)(label(actions))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         f47a9c20-853c-412e-bead-c399b6d294e2)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 24))(sort Pat))((shape(Concave \
         24))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         893e76ea-6c79-46b6-8fca-e2c43be0d4f6)(content(Whitespace\" \
         \"))))(Tile((id 52f194ec-ddf7-423b-b2e0-8466202091d7)(label([ \
         ]))(mold((out Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape \
         Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
         5e186533-2f3a-4b87-91e7-c2b24360eacd)(label(Action))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))))))))))))(Secondary((id \
         72ece484-f0cf-4516-9785-9131f2e7200b)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         1970b569-4e62-4474-b130-885d18510eed)(content(Whitespace\"\\n\"))))(Tile((id \
         dfedf74d-47ab-49e9-a82f-580c25eae896)(label(fold_left))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         4d29b69e-9cc3-4816-b36b-b00b0cc4bb13)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         2a1e8b60-d1fd-4ee0-bfce-1eecd36d087b)(label(actions))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         b2f6c355-d8f2-4633-8257-3403a74fa018)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         73cbb9a6-17da-4f88-bd9b-44f5c66f1f33)(content(Whitespace\" \
         \"))))(Tile((id \
         29c15782-e31b-43e0-9ddd-0b09d5401115)(label(update))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         d37c0d88-0806-49e6-90bb-8f4aac2f6b56)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         df2fa983-077a-48dc-aee7-f2ada91a2cec)(content(Whitespace\" \
         \"))))(Tile((id \
         d8ac8c27-f64b-4b70-940d-f4cb5353add4)(label(init))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         cb36e757-a2c3-4e01-a4f6-e41e7ff5dab3)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         b340ebee-2695-4705-8e09-1082dd9524ec)(content(Whitespace\"\\n\"))))(Secondary((id \
         cd0234ee-743f-42b7-9efc-2d4ddcd36d31)(content(Whitespace\"\\n\"))))(Secondary((id \
         3ebfed62-0b40-4efa-abd4-9071b0fe6697)(content(Comment\"# Existing \
         tests #\"))))(Secondary((id \
         a837ed6f-839a-44b4-935c-a0eeefc2dc97)(content(Whitespace\"\\n\"))))(Tile((id \
         f46a2261-aede-46f6-a731-da9d07ab20e2)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         32555432-d799-41fd-89c3-38812132e12c)(content(Whitespace\"\\n\"))))(Tile((id \
         a19b6df7-db7a-4a83-b009-e1e29c526306)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         6f5017b2-7904-484b-8aec-87509eb3ad24)(content(Whitespace\" \
         \"))))(Tile((id \
         ebe9d743-811e-4e5d-9067-df29a9a8ac75)(label(m))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         e92906c9-3899-4881-a2eb-8d2c0400f59b)(content(Whitespace\" \
         \")))))((Secondary((id \
         2d493f10-95b4-44d6-87c5-a3375463a25d)(content(Whitespace\" \
         \"))))(Tile((id \
         13df83f7-7f97-4977-b3e0-4afcf8fb6183)(label(update))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         37b1c9d0-e0e6-4868-a420-b49186d819e4)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         e9baec99-3b39-499e-9ac7-ff462b8fafa2)(label(init))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         13cba5c2-803d-4a03-b80e-df774bf3c268)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         bdd96f97-5d82-4f67-ad95-b5241f66bfd0)(content(Whitespace\" \
         \"))))(Tile((id \
         0235f18a-92c0-4943-9b67-b9c8821ceea4)(label(PaintRow))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         e03994ff-255c-45a1-81d3-469b74822318)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         f0d5afe6-b353-49f0-bb24-beb16479fa52)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))))))))))))(Secondary((id \
         b0eb216c-3d59-48ee-bc97-051483964391)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         e8f3261b-ee33-44ec-b48d-8c5758b257ca)(content(Whitespace\"\\n\"))))(Tile((id \
         194c50ce-3290-4ff4-9342-d84096041e9f)(label(m))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         168265f2-f992-41a8-a60a-9d193adbdf6d)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         ab936c38-4b58-46e6-aafb-a4b484221ca6)(label(canvas))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         0adbd7a1-f9ff-414e-8cca-122d4495ffb4)(content(Whitespace\" \
         \"))))(Tile((id \
         3ab3e5d0-d43e-4be7-9ce5-49e0570434f8)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         2187d8f3-6f2c-4335-ad4f-6d2b40c88959)(content(Whitespace\" \
         \"))))(Tile((id 4e0f5c80-80e3-4ef8-9f9d-62b08e95e2fe)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         1093b982-80c7-4fba-8fe2-c9c91b6af13e)(label([ ]))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         069c9afe-7bb5-4da8-a53f-93be409fbeb8)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         9fb4961a-0f3b-4e18-85e8-a97c9e4feb12)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         77f0aa78-a2ff-4e29-a736-af60af0c7cd2)(content(Whitespace\" \
         \"))))(Tile((id \
         fae6b23c-2864-47b3-ad0f-75de25d30ecb)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         959eadf6-7903-4bf8-9c49-fae0aef6cd3f)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         1208e26c-65ee-409a-b668-9822b62f7151)(content(Whitespace\" \
         \"))))(Tile((id \
         5c5a404a-af94-4c83-83be-217eef006934)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         a5e5b756-7e4d-4d7b-b288-0e89ad9ccb05)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         96a84913-c59d-4a99-ae60-e2e7b0ba2754)(content(Whitespace\" \
         \"))))(Tile((id 0ff9365a-992c-43c9-bf01-cd060b7000e1)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         4b4daa55-2860-429b-a960-8a0a09b3d81e)(label(\"\\\"\\240\\159\\142\\168\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         59204a29-551d-4cf5-8836-b4ae9902fcbc)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         72e3f726-824d-45b4-b727-9ea991304c58)(content(Whitespace\" \
         \"))))(Tile((id \
         8f48d256-aba8-4e7c-a419-93323c71070f)(label(\"\\\"\\240\\159\\142\\168\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         55962dc4-4685-43ff-beec-9b0e69392010)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         0a4d1797-f859-4e56-bb57-5d8f9c99b7fe)(content(Whitespace\" \
         \"))))(Tile((id \
         0408552f-9b18-41e1-b677-e226512295ad)(label(\"\\\"\\240\\159\\142\\168\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         52258e79-a133-4e05-a989-5965a35a79ca)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         7ddd6e12-c315-43c3-a7e2-89e166a8aac7)(content(Whitespace\" \
         \"))))(Tile((id 1bab16ad-bcad-4034-a00f-190051bb2f48)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         5610a02d-cabf-494d-a52e-d1a883588757)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         6b3fb48b-51e5-4ca2-ad63-584f740ec4bf)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         334ddeb7-5827-4fa1-8515-9a0c236cd3f7)(content(Whitespace\" \
         \"))))(Tile((id \
         dae1b268-84f7-4c97-a92d-588aa5847057)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         f3ff5e1e-b50c-4754-938b-b9debfe7fc8f)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         a615c216-02b5-4fc5-bc43-d06f54aca29f)(content(Whitespace\" \
         \"))))(Tile((id \
         81e940d2-9797-4b81-9c30-6853c27474b5)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))))))))))))(Secondary((id \
         bff9a64a-45ac-4859-8fc2-bdce0bb6820a)(content(Whitespace\"\\n\")))))))))(Tile((id \
         e73d35e8-1771-451e-865e-34d6f1d73d2a)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         3ee8535c-9cf6-45bc-8094-472913ce2406)(content(Whitespace\"\\n\"))))(Secondary((id \
         54d7363f-75c3-4a92-a9ad-ed88a1cffdf7)(content(Whitespace\"\\n\"))))(Secondary((id \
         97da6aff-bfd5-4895-9cc2-058d31129349)(content(Comment\"# New tests \
         for PaintCol #\"))))(Secondary((id \
         354bd179-c769-4962-a8d6-67206e926d80)(content(Whitespace\"\\n\"))))(Tile((id \
         7e50d512-8bc5-41b9-9edc-cdaa17b607ef)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         c7381921-4ee1-4c79-b0b9-e4e962de3b25)(content(Whitespace\"\\n\"))))(Tile((id \
         4a6d001b-c73b-49de-aa21-30b064e28a75)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         f0c2e8b0-1edb-45a8-b359-47890d5cf68b)(content(Whitespace\" \
         \"))))(Tile((id \
         f651c09d-f3c8-46bf-91b3-e75f63fd35bc)(label(m))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         d9e23609-8d84-447c-888a-5147deb8fb7f)(content(Whitespace\" \
         \")))))((Secondary((id \
         be2619d9-22eb-43ce-9561-e1e6bdd4d95c)(content(Whitespace\" \
         \"))))(Tile((id \
         abe05dc5-f744-4ed5-b044-d79625811bd1)(label(update))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         21efc416-3098-40ae-9e90-3e6f77dd4941)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         5784bf95-8c0c-4fbf-b4e8-3488096f223c)(label(init))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         1d1cb0e3-8f20-4712-8fff-a6a9c5ab226d)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         7782ea83-904b-4a98-b0e9-5fac74430e06)(content(Whitespace\" \
         \"))))(Tile((id \
         4602d2a7-fc46-4551-9fe5-22c21b6a72bf)(label(PaintCol))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         5e733391-2413-4689-be9a-76a0826edd86)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         91947519-484f-470c-828f-441ae4f9283c)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))))))))))))(Secondary((id \
         1d766bba-f442-432c-9860-15b2881284b1)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         fa3ad6c0-d907-420c-8ca3-1bebd55baf3d)(content(Whitespace\"\\n\"))))(Tile((id \
         7fc3d437-b910-4e24-9b29-591184a7ed12)(label(m))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         fa3fabee-fc70-4332-a8b5-503c923b03f3)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         777aeffa-3fd4-42ff-981c-23b4bb677992)(label(canvas))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         3fc4bc33-f6c1-4612-ac96-d7feb4069fea)(content(Whitespace\" \
         \"))))(Tile((id \
         5974873a-3718-49db-b011-42666e06c46f)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         d57102d5-6d83-4887-8620-93a68fa8bf0f)(content(Whitespace\" \
         \"))))(Tile((id c845e6f7-7302-4d23-ac76-4fb7eb8f9189)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         4e9a9e21-48e9-4d10-b108-ca3c1c99f567)(label([ ]))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         48936664-c72a-4b01-b35a-251960a36b80)(label(\"\\\"\\240\\159\\142\\168\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         873fe409-d2de-4cbc-b7d5-1b422388622e)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         94d39cc9-68a9-44ba-8c23-f005dffdeeab)(content(Whitespace\" \
         \"))))(Tile((id \
         674bc0dd-6689-437c-9b9a-a920b8c66403)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         3fb94678-c299-4e2b-84d8-b69b706107c2)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         24881568-cef5-4e8a-87c6-c5bef7f1d3bf)(content(Whitespace\" \
         \"))))(Tile((id \
         e371ac3a-e644-4792-9b5f-037613816320)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         4f472075-f8cd-4a16-903b-a3c6f287af28)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         3307ef25-dfa4-47a7-8110-878e0d3cd8ee)(content(Whitespace\" \
         \"))))(Tile((id 210b8660-bd6a-4c9d-9725-cc29071d5334)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         179c35e3-2ab4-4f2e-a537-feb1b89d7f00)(label(\"\\\"\\240\\159\\142\\168\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         78692d17-d97e-473e-90b9-38c6097dfab7)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         d6b75f78-3434-4d95-9ac1-01ef07bb0283)(content(Whitespace\" \
         \"))))(Tile((id \
         bd2515e7-ea74-45ec-b3cf-c0bf50d3fcac)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         6a11e8bf-84a8-4948-aaf2-597ced9e06d1)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         72dcac33-0762-49a9-80f4-08a3a93ba24b)(content(Whitespace\" \
         \"))))(Tile((id \
         6cffb924-baa4-4bc7-a4e9-e0557473d354)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         99029838-aedd-4db6-a38e-6a20f5655601)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         1b2690fe-9472-4cc8-ae5d-6dc8f4a1c14e)(content(Whitespace\" \
         \"))))(Tile((id 0b9074b4-e7d9-450d-8e25-26a2e67de7f8)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         722468dc-7338-48f0-b479-416b0b7e3ad9)(label(\"\\\"\\240\\159\\142\\168\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         73101373-9fac-4100-b239-296b69b8a072)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         ceb3eda2-e3a9-4a3b-a85e-43316afebeb3)(content(Whitespace\" \
         \"))))(Tile((id \
         92359dfd-db2c-44a6-b602-711fe17b5e13)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         d89265a2-8a31-4b0f-93ca-1dff44969d59)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         6174e863-dcda-418d-872b-74857be6c599)(content(Whitespace\" \
         \"))))(Tile((id \
         78d9ff20-9560-4dad-80a2-c5605c7026f3)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))))))))))))(Secondary((id \
         4e28352e-c234-4c1b-9260-1029a592c3b2)(content(Whitespace\"\\n\")))))))))(Tile((id \
         14231617-51cf-41ef-9032-9fd1ead5cabc)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         9687c3b7-5b92-41b8-815e-f7820f5265cb)(content(Whitespace\"\\n\"))))(Secondary((id \
         135aa445-d193-4a5a-b37f-2abdfee2dbfe)(content(Whitespace\"\\n\"))))(Tile((id \
         7099944a-a778-48fb-a120-a612daa7d156)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         a6a60f9c-4c40-4cbe-ab96-fb3fbd7eb43b)(content(Whitespace\"\\n\"))))(Tile((id \
         809c2b84-61d7-4862-af32-5966e4c28f51)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         05ddca5d-d989-4c08-8f92-40c4c3d086b2)(content(Whitespace\" \
         \"))))(Tile((id \
         6066e5dc-b6f1-468a-93dd-c6dcbaa8ba34)(label(m))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         f987b241-81a5-4944-ac8d-9264587c5621)(content(Whitespace\" \
         \")))))((Secondary((id \
         3d4baf9f-d3d6-4629-bce1-6ebb8b09291f)(content(Whitespace\" \
         \"))))(Tile((id \
         aecb9d4b-6df4-40d7-aea9-e48af721bfc7)(label(update))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         824fd5b2-0262-4b6c-92d0-ba21b3586ba4)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         9a8c3f6d-2f71-4e68-9751-16dbde85bf2c)(label(init))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         92f02822-8cf9-4ff8-afc5-ea31180595cc)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         119c57e6-4037-4c79-8591-15d32666d240)(content(Whitespace\" \
         \"))))(Tile((id \
         9fee8a97-07be-40fd-85d2-692ebe83cd4c)(label(PaintCol))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         dcc17f54-5632-4f26-98af-6b4bbca73e6c)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         20f26d7e-b518-4eac-a71c-5b769f0e2ecb)(label(2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))))))))))))(Secondary((id \
         d82ecb6d-8235-44dd-9200-46fb07b393fc)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         13c55b73-0fe3-43d0-97cc-c809e6f85cec)(content(Whitespace\"\\n\"))))(Tile((id \
         25706806-eae0-461d-8340-06afe33ab228)(label(m))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         6ef9da2a-8304-4e86-9a5b-ce3fbacbca71)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         6c10da8c-e444-4996-a4d8-54f5c4b5fd3a)(label(canvas))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         3dba8ae1-dcd2-4e3c-ad5d-e1fca083902a)(content(Whitespace\" \
         \"))))(Tile((id \
         b5aa41b6-4e70-4fdc-a144-7ad06118668e)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         ccc1c0bf-2bbc-4247-af25-67381df0d503)(content(Whitespace\" \
         \"))))(Tile((id fd3fad1a-a00b-4218-87a1-e60de7ad34ff)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         58d56701-9d23-4958-90e3-f00e84ae6ef9)(label([ ]))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         31139a83-8506-4c6f-97ca-7534c84a5062)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         2e17f529-3d01-44b7-85e9-b1a6632546cc)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         3040cd1b-9687-4a27-9f9b-eb04bafc6389)(content(Whitespace\" \
         \"))))(Tile((id \
         39460ac2-57df-4811-85c0-f4ac5d03f9f2)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         8a10e6c6-624c-4ae8-9024-65486a6170c9)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         82e06d88-a4ab-4222-9ee7-5dc5c38ec866)(content(Whitespace\" \
         \"))))(Tile((id \
         596ea0ce-d58a-461a-9003-7aa3bf5af4d2)(label(\"\\\"\\240\\159\\142\\168\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         b99b3d9f-d7cb-41e7-8aef-5f111d90f100)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         b8a0df59-9e76-48af-9cdb-a0860fc42c60)(content(Whitespace\" \
         \"))))(Tile((id f0cff9ba-b6f4-422f-9268-74c5991d037e)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         73206b34-cd48-48e8-b63c-2658be453db1)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         f1a7abb1-ab28-4d47-bfc2-c1056421389f)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         0407b0f4-9066-4e4f-9977-4e0c36d3f7af)(content(Whitespace\" \
         \"))))(Tile((id \
         83af5be5-71cd-455d-8770-cdbb42b7b15d)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         f37d698b-8653-42c3-85b7-01ded80d235d)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         1db46d17-8337-4253-a49d-97300a6edf90)(content(Whitespace\" \
         \"))))(Tile((id \
         62157893-d736-441f-8741-05b3c244487f)(label(\"\\\"\\240\\159\\142\\168\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         56166438-3e12-4a6d-97d9-33de65e6be9c)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         fd263842-02d4-43bf-8132-49a136af0a44)(content(Whitespace\" \
         \"))))(Tile((id a925ee15-dc22-4218-ad37-09ddeb492f4a)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         8bb5c419-c68c-4e5c-8503-5f9b17c8d7e0)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         c5381564-571b-41e3-8641-275fe672ea7c)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         ca2e9fb2-f4ee-4943-8c49-99a6f70fb44a)(content(Whitespace\" \
         \"))))(Tile((id \
         4f826e78-d82e-48d3-a642-29356a37ac6a)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         29b66c14-51bb-409e-966e-361da0ae630c)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         18b75fdd-5eac-4f62-a5dc-69bbf2751ba1)(content(Whitespace\" \
         \"))))(Tile((id \
         98b29652-b346-4919-b8c0-aa2708a6b6aa)(label(\"\\\"\\240\\159\\142\\168\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))))))))))))(Secondary((id \
         c3f28d22-ccb1-481e-92d6-050e3bb62808)(content(Whitespace\"\\n\")))))))))(Tile((id \
         b3a31574-ab04-4a7e-9402-0ba3ada2696a)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         b7f97266-1ce7-4dd8-8b40-d8ac4e5e363e)(content(Whitespace\"\\n\"))))(Secondary((id \
         961a57e8-e485-40de-ac8e-a5d3079d37cc)(content(Whitespace\"\\n\"))))(Tile((id \
         b08cceee-c11c-4f99-823b-4d5c898a4970)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         c0a69121-84aa-45d9-9d2c-075a3b7b78a8)(content(Whitespace\"\\n\"))))(Tile((id \
         58a3bdec-99c6-464e-be07-733f1bf8944b)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         a37c0a01-4ea3-4b55-8e09-bb5587a8044a)(content(Whitespace\" \
         \"))))(Tile((id \
         c9c3f554-598f-41e3-aa93-f250ec48561a)(label(m))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         838afa50-bf2e-473c-9178-1157b2b85cd7)(content(Whitespace\" \
         \")))))((Secondary((id \
         70cabbbc-e6c8-49e0-a31f-cd8fdd7caf19)(content(Whitespace\" \
         \"))))(Tile((id \
         086d8fd8-8969-441d-8793-f02ea52a5e37)(label(do))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         cd62d741-8658-4f49-902c-9abed5fa19e3)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         ae236f29-f341-40ab-a639-085f5d65fe4a)(label(init))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         f81887bf-6ae6-42cb-ae40-04271fcb23a5)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         4ef068f0-9d84-405b-acf1-31ca31235ecc)(content(Whitespace\" \
         \"))))(Tile((id a22f001d-9b56-4f44-ba91-56593b7dd6ea)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         8063194f-9a9a-4fb5-9e4c-e35d0c5bc95e)(label(PaintRow))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         62c23b44-c742-4922-b9f0-91623ad03506)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         f9134b23-0795-4c9e-a69e-c55cb9666588)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         78410241-2f60-4bf2-b0f9-a9595975953e)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         f1369efe-f716-42a2-814b-cd6af2d0febb)(content(Whitespace\" \
         \"))))(Tile((id \
         43d56125-696b-4d06-91d3-407afdd22f97)(label(PaintCol))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         71b3e527-9d81-49c5-a9a6-d711b80045c9)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         c71572b2-455c-4d0a-a5ba-ea6c9918a3e9)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))))))))))))(Secondary((id \
         75262eec-65e1-451c-9859-0011f9abcf5d)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         a4e52dd3-a1a0-476a-8792-c6323f24c104)(content(Whitespace\"\\n\"))))(Tile((id \
         5a9d72fd-90ce-43ff-b5cd-6cf0c6c582dd)(label(m))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         9cebb525-9e8b-4820-8559-b0102568a89a)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         61fa3d75-0b6d-40c9-8649-f43277e3c71c)(label(canvas))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         e81e3b1b-58f1-4031-9aca-19ecb3d0bc44)(content(Whitespace\" \
         \"))))(Tile((id \
         1e831261-d3e6-4f86-be50-39af7c6d3f63)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         8b470d0d-a59c-4e36-a60d-59e29fcfe62d)(content(Whitespace\" \
         \"))))(Tile((id 1312027c-48e8-478b-af24-a0206fa76f62)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         9c08e125-3952-4f4c-afe3-57e206ff7a4e)(label([ ]))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         7086d242-d9db-477b-bc0f-e524bc98dfd0)(label(\"\\\"\\240\\159\\142\\168\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         55940256-aac8-4af2-a624-06d8ebf0ab92)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         9749e5ff-fc9b-4f92-ae1e-6c91b2022ba2)(content(Whitespace\" \
         \"))))(Tile((id \
         80c97ce5-d3bd-41b8-9d88-77c845fb8cde)(label(\"\\\"\\240\\159\\142\\168\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         65f3fdb2-800f-4bae-99f8-51ab496dab23)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         8174ba09-4025-49eb-bda3-533894338c79)(content(Whitespace\" \
         \"))))(Tile((id \
         2b07e802-4a90-4570-8d97-d56b3dfab3fc)(label(\"\\\"\\240\\159\\142\\168\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         32d5e56f-b67f-4da2-8897-aad08f42158d)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         e3de50bc-4585-4773-a84a-7f103d59d06f)(content(Whitespace\" \
         \"))))(Tile((id e776814d-9131-4c95-9f2c-fb62cc8fb742)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         8ea7ef3a-8065-4b96-9ba0-734e2cdda9b9)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         9c5cbe06-ff72-479c-88f9-0e6aa5682de7)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         994569b9-9f51-42bf-b91d-149fec6edb8c)(content(Whitespace\" \
         \"))))(Tile((id \
         80ffc51c-fe89-4d64-87ee-4bfe3e742c2c)(label(\"\\\"\\240\\159\\142\\168\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         bce5f9b7-9062-4080-821f-c2bea8752c6c)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         a2c2c9e5-a0af-4d74-8469-d6942c734e8f)(content(Whitespace\" \
         \"))))(Tile((id \
         f420821b-5d82-4bc1-82f0-93bc5f9e5e73)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         9d1bd4cc-6a48-4a32-bbcf-e58b8569950c)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         4396ba3f-364a-4c0b-9fb6-b12ea559e205)(content(Whitespace\" \
         \"))))(Tile((id f4e7729c-2b6c-4434-91a7-5ce0d5ef24c4)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         f3de8d9c-a507-4d46-acc9-a399ab0aa89f)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         05f888a5-c7b2-45af-a25d-916e786b54df)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         34f98821-6de8-42c2-9677-eebf9690ecd2)(content(Whitespace\" \
         \"))))(Tile((id \
         cd7bd1d3-e714-4a9e-b6dd-db7625f7be93)(label(\"\\\"\\240\\159\\142\\168\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         e44d8b3f-98b8-497d-a72f-e43efa270c81)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         a9df55e0-4586-4798-b908-2c58d32fc73f)(content(Whitespace\" \
         \"))))(Tile((id \
         3f0d1c7f-f8d9-4313-b5e0-f9b3d4c50658)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))))))))))))(Secondary((id \
         509911fc-d3e9-49bc-ae8c-55aac6931b12)(content(Whitespace\"\\n\")))))))))(Tile((id \
         33d83ec7-69db-4606-bd45-e9953ca85342)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         789ed2f6-2138-4b21-98e3-7c462ee29513)(content(Whitespace\"\\n\"))))(Secondary((id \
         811a4175-0834-4d62-9356-0d768c54c8c5)(content(Whitespace\"\\n\"))))(Tile((id \
         e9a04466-c29c-4fca-96b0-b3b314db229f)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         58d72c95-a6eb-492e-aa29-7da4c9b174f7)(content(Whitespace\"\\n\"))))(Tile((id \
         0b7c2bbd-0a77-45d9-9895-55ac2224f6fe)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         aa2b479c-51c4-4d7f-8b74-7a80444c536e)(content(Whitespace\" \
         \"))))(Tile((id \
         abdf464f-fb6c-4f8c-86ad-1cfcb38c378d)(label(m))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         c7ed0d85-d4fa-4a51-b303-5cfe1e50c65c)(content(Whitespace\" \
         \")))))((Secondary((id \
         85c11a29-e867-4d6b-a6f4-517a6f226396)(content(Whitespace\" \
         \"))))(Tile((id \
         a0cfa924-bacc-4ecb-ad4f-f691eb04d36b)(label(do))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         882d2f22-5059-4eff-9917-443ccd4bafef)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         56673c77-4eae-44e8-b080-3f77d46f55be)(label(init))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         b97af9bb-a771-4b6d-b3cb-9b25fe8b6223)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         6561446c-5466-4f65-bc94-bc042a4737fd)(content(Whitespace\" \
         \"))))(Tile((id 06cce53b-9ed0-485d-9ff9-68f9b494553c)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         a392e6b1-0857-48e3-ba15-b010fdeb6dd0)(label(SetBrush))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         c9a4bcad-9ca1-4a0e-80c2-b4e2da4afec9)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         a091d647-9984-453c-a803-2de955ef13d1)(label(2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         68c818de-0901-4d0e-a581-22f92f904791)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         6d0be055-32d1-4374-b5d1-71fe53d597eb)(content(Whitespace\" \
         \"))))(Tile((id \
         19c65b48-a106-41e4-8e3d-972e518686f4)(label(PaintCol))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         3e3f8290-14ce-4d77-946c-39025b7d707f)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         7f5f0562-a5f2-4d2b-8554-26164a7ac71e)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))))))))))))(Secondary((id \
         87fded7c-50a2-4336-9e91-ce6bfaf99547)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         5345d6bd-d2eb-4445-9501-6becfec4624c)(content(Whitespace\"\\n\"))))(Tile((id \
         fd5ddab6-696a-4082-a1e0-ca86b7843818)(label(m))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         905dc5df-e14a-46ec-8231-7044c091ceaf)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         217b9631-fca5-4d51-8a7b-e02e68b9367e)(label(canvas))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         c1123aac-aae6-4252-a9e9-b8ceb8b7af3b)(content(Whitespace\" \
         \"))))(Tile((id \
         d138370a-9e7a-4cd9-b467-4d4bdfe04d19)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         dd163165-7b06-45a0-a311-de28281aff39)(content(Whitespace\" \
         \"))))(Tile((id 347505f8-5558-4f03-a43e-4ef4e2541372)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         13a031b7-7a83-4717-9308-03d9b88305b5)(label([ ]))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         43e941f0-8f8b-4b9a-a9e0-3e4b3013ba29)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         cb571d5d-8b6c-4156-9714-e0ea8594baed)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         9684bc8b-ca2e-43ce-a525-6fbc07286fe0)(content(Whitespace\" \
         \"))))(Tile((id \
         a7c1498f-4588-447f-a1c1-b83cc17ecd7e)(label(\"\\\"\\240\\159\\146\\156\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         76ac7ecb-92ef-4be4-b30c-658bf1ecc70a)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         4601c6bc-20e9-4354-be55-bf2d4427b2b3)(content(Whitespace\" \
         \"))))(Tile((id \
         767cf584-3964-47f5-8958-1ee9adb9aa6f)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         de0ce7a3-3923-4606-9da1-c7e25a5f1259)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         3d39392b-b28a-48ab-9e5e-eea62195d4fc)(content(Whitespace\" \
         \"))))(Tile((id 7daa7b78-971c-4a77-8ae1-ca6c4fb32a68)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         e96cf11c-0187-402c-91b3-509006d2f38c)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         e72ede29-3439-4f20-8e1b-450741f3f942)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         f22c8d67-c2aa-45f2-a7e5-84447f5608d0)(content(Whitespace\" \
         \"))))(Tile((id \
         ee4ab332-7906-4a1c-bb89-7126082f478b)(label(\"\\\"\\240\\159\\146\\156\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         9608e9b5-d533-4b3d-a7c5-1a3438a9a023)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         c8297f3c-cadb-4905-ac80-a15ee4b719b3)(content(Whitespace\" \
         \"))))(Tile((id \
         864bf2a3-8327-4976-ae6c-24f995aa2866)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         37396ca1-09f0-43af-b22d-b9129f639308)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         9632e848-5224-4f6a-9be4-e74e4a0610e9)(content(Whitespace\" \
         \"))))(Tile((id 4bc0094e-e633-4d7f-84eb-2a64f9ca6eb8)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         8e928c87-be7e-4767-a1f8-9b7624178029)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         813f24cb-2c8d-458a-87a4-99a7fd653434)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         18413887-e7b9-4235-af19-965f4bab3a2b)(content(Whitespace\" \
         \"))))(Tile((id \
         cca47f7b-4bd8-4d92-9ea3-99bc98008bc8)(label(\"\\\"\\240\\159\\146\\156\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         8f155146-9d01-43c1-a413-ab89e5ec28db)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         81c21b7a-2f8b-4d2c-a3df-04dd436b2964)(content(Whitespace\" \
         \"))))(Tile((id \
         cc1449e8-6c72-4873-b1e7-582e6db10eb1)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))))))))))))(Secondary((id \
         047d575b-112d-422b-802b-04dd926b90b1)(content(Whitespace\"\\n\"))))(Tile((id \
         66ecc0b8-fe95-4618-ae39-0f4cd5525349)(label(&&))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 32))(sort Exp))((shape(Concave \
         32))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         56d41145-f75e-4646-9fa5-c83212525b19)(content(Whitespace\" \
         \"))))(Tile((id \
         057d0cfa-5280-4c95-b459-ed3cd03892d7)(label(m))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         5940236e-d962-4fb7-9033-ad776843bd86)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         5d03f35a-025a-4a7d-a811-8fefc35ba387)(label(brush))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         5115179e-3a1e-4adc-8460-50cf8999bc78)(content(Whitespace\" \
         \"))))(Tile((id \
         48d73783-dc49-4fe7-9d9b-16da19b468f2)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         e3243b8d-3f1e-4bde-88fe-02e3a2122e47)(content(Whitespace\" \
         \"))))(Tile((id \
         36df6384-710d-49c8-9d9b-d5a5eb179a65)(label(\"\\\"\\240\\159\\146\\156\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         63310805-6966-48a8-8d71-4fb0a817d43a)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         751e1293-9518-44e1-b994-68e0d9d6cc11)(content(Whitespace\"\\n\")))))";
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
