let out : string * Haz3lcore.PersistentSegment.t =
  ( "Study / writing / emojipaint-extend / emojipaint-extend-sketch",
    {
      segment =
        "((Secondary((id \
         29bc7b6c-611f-413f-8cfb-c1e64655c839)(content(Comment\"# EMOJIPAINT \
         EXTENSION TASK                     #\"))))(Secondary((id \
         a25a0855-d485-4543-94d8-29bb34656b6f)(content(Whitespace\"\\n\"))))(Secondary((id \
         655b9996-6c97-46b5-a49d-21ce2f9e6a70)(content(Comment\"#                                               \
         #\"))))(Secondary((id \
         efc1ef5d-a8c1-45e3-8eae-6c5710751c3e)(content(Whitespace\"\\n\"))))(Secondary((id \
         049e073a-098d-441e-b35b-70db3d917f0f)(content(Comment\"# The \
         emojipaint app lets you paint emojis on   #\"))))(Secondary((id \
         ecd41dea-8017-4212-8911-86fc2cab9e5f)(content(Whitespace\"\\n\"))))(Secondary((id \
         431fa356-6fd6-4cd2-9cd5-8ece99f1a5ce)(content(Comment\"# a grid. It \
         already supports painting rows.    #\"))))(Secondary((id \
         9b037ea5-07a8-4e1a-ad71-fde37f304f37)(content(Whitespace\"\\n\"))))(Secondary((id \
         7a8e002b-8943-42c5-a144-7f48d2835044)(content(Comment\"#                                               \
         #\"))))(Secondary((id \
         760a0d48-019e-491d-8fb6-1be82e428785)(content(Whitespace\"\\n\"))))(Secondary((id \
         a9eb313d-a95f-4cdf-a90c-73b70cbdc779)(content(Comment\"# YOUR TASK: \
         Add a PaintCol action that fills   #\"))))(Secondary((id \
         d78b2bbf-bd70-4828-83ff-835da7c8a5e7)(content(Whitespace\"\\n\"))))(Secondary((id \
         f4bf1db1-c398-4b17-adf4-8e076f7a8d81)(content(Comment\"# an entire \
         column with the current brush.      #\"))))(Secondary((id \
         22352a6f-3e4c-4ab8-83f3-e80cb6e4b4aa)(content(Whitespace\"\\n\"))))(Secondary((id \
         809d8666-ab52-46f3-be25-801b347b3bee)(content(Comment\"#                                               \
         #\"))))(Secondary((id \
         09fe464e-d0f3-40bd-9c5c-b78a48e59aa6)(content(Whitespace\"\\n\"))))(Secondary((id \
         86b04f37-cf85-41ec-9842-b34bbbb95ee0)(content(Comment\"# You need \
         to:                                  #\"))))(Secondary((id \
         a8548dd8-2db1-45e9-94fa-5231ffe3fc52)(content(Whitespace\"\\n\"))))(Secondary((id \
         01352bf1-d5fe-4b89-a2e9-81be7efc2425)(content(Comment\"#   1. Add \
         PaintCol(Col) to the Action type     #\"))))(Secondary((id \
         14ea4f52-e544-4c2d-adda-0c0beb2c6c42)(content(Whitespace\"\\n\"))))(Secondary((id \
         b30a58e4-5836-4f26-b48f-83533e67aae1)(content(Comment\"#   2. Add a \
         setCol helper function             #\"))))(Secondary((id \
         7b1ffa9e-749d-4779-b2bd-3b85f5bea509)(content(Whitespace\"\\n\"))))(Secondary((id \
         e9c81c64-d999-4fe4-8bbb-debb8f0c58b7)(content(Comment\"#   3. Handle \
         PaintCol in the update function   #\"))))(Secondary((id \
         68cae798-e3bb-4efb-a4f5-73c39613ff24)(content(Whitespace\"\\n\"))))(Secondary((id \
         02e5f4bf-4c8d-4dd8-8389-3ab66c031874)(content(Comment\"#                                               \
         #\"))))(Secondary((id \
         40663df1-8e81-42b6-bd81-15dcd626c791)(content(Whitespace\"\\n\"))))(Secondary((id \
         9dd71dac-7a50-4027-b2cb-c721a07e87bc)(content(Comment\"# Look at how \
         PaintRow is implemented for       #\"))))(Secondary((id \
         98a2b16c-5aba-48cd-a252-0fd68ac7b022)(content(Whitespace\"\\n\"))))(Secondary((id \
         f6ebde5e-36e1-4927-98d4-3957b99392a5)(content(Comment\"# guidance - \
         PaintCol is similar but vertical.  #\"))))(Secondary((id \
         334d7d57-ba11-4467-8f5f-cadc90c5a7fb)(content(Whitespace\"\\n\"))))(Secondary((id \
         822cadd3-6d71-41e2-bb02-829eb1fe0f61)(content(Comment\"#                                               \
         #\"))))(Secondary((id \
         20e0838c-b2cc-455b-946f-31fb3ee5448c)(content(Whitespace\"\\n\"))))(Secondary((id \
         c618c412-aa2a-489f-9fb3-3657ad922558)(content(Comment\"# Tip: Use \
         auto-probe to see how the canvas     #\"))))(Secondary((id \
         5b863107-a057-45b9-92b2-7bab99ad2571)(content(Whitespace\"\\n\"))))(Secondary((id \
         f0f81919-ac3e-42ef-8800-aee699736ad2)(content(Comment\"# changes \
         after each action.                    #\"))))(Secondary((id \
         249e4438-ed04-4487-bc2b-ee4377d2f1c7)(content(Whitespace\"\\n\"))))(Secondary((id \
         8b3bfebc-a7ee-458a-acd9-b252617998b7)(content(Whitespace\"\\n\"))))(Tile((id \
         801622c6-e6db-4b5a-b858-a17133b32842)(label(type = in))(mold((out \
         Exp)(in_(TPat Typ))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         44e04017-d0ce-4efb-9c14-cff49017f00e)(content(Whitespace\" \
         \"))))(Tile((id \
         a825532b-362a-40b9-8566-d8fec66d3adc)(label(Emoji))(mold((out \
         TPat)(in_())(nibs(((shape Convex)(sort TPat))((shape Convex)(sort \
         TPat))))))(shards(0))(children())))(Secondary((id \
         7c1541ed-4667-4960-b25f-82044be038cc)(content(Whitespace\" \
         \")))))((Secondary((id \
         99590c0d-6b74-44f2-a30b-1d73d09f1135)(content(Whitespace\" \
         \"))))(Tile((id \
         c6328d09-451b-4ab0-9e1e-32dcee9efbfd)(label(String))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         0a985d61-d6ac-4958-9454-dd3eb943402a)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         d58f6a9b-72bc-4947-b7cf-cb18b23b30db)(content(Whitespace\"\\n\"))))(Tile((id \
         ecb4cae3-d443-4f82-95aa-8bf4f6d6bb4f)(label(type = in))(mold((out \
         Exp)(in_(TPat Typ))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         47aa0fd4-d49d-43f8-8120-5c3071fbde2c)(content(Whitespace\" \
         \"))))(Tile((id \
         4d48f45e-f2d3-4e24-b988-94e03004edb5)(label(Canvas))(mold((out \
         TPat)(in_())(nibs(((shape Convex)(sort TPat))((shape Convex)(sort \
         TPat))))))(shards(0))(children())))(Secondary((id \
         28183c4f-5b7f-4818-a7ae-76fc123feff0)(content(Whitespace\" \
         \")))))((Secondary((id \
         fd6981c6-1d20-4711-baf0-7984b1ef358a)(content(Whitespace\" \
         \"))))(Tile((id df838e03-2c82-4a36-bff7-a0a94ed14dde)(label([ \
         ]))(mold((out Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape \
         Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
         95dd80d0-8554-4ae8-a7b2-bdc0f1b7d56a)(label([ ]))(mold((out \
         Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0 1))(children(((Tile((id \
         fd8cd3ec-99e7-4828-a7d1-5e51d155f0b9)(label(Emoji))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))))))))))))(Secondary((id \
         21f85b43-fa80-4d3f-b0e7-3be7d6048859)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         200b0214-c670-4015-ba7f-d8c66c2b18c2)(content(Whitespace\"\\n\"))))(Tile((id \
         b5ac3f66-6e99-47fb-935a-bd81ee747045)(label(type = in))(mold((out \
         Exp)(in_(TPat Typ))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         259674cf-9d96-4deb-812c-8b2913eb24f0)(content(Whitespace\" \
         \"))))(Tile((id \
         269a7476-e0ae-45dc-864a-99b56d723ccc)(label(Row))(mold((out \
         TPat)(in_())(nibs(((shape Convex)(sort TPat))((shape Convex)(sort \
         TPat))))))(shards(0))(children())))(Secondary((id \
         0c4411df-e14e-4b1d-a65c-3d150d1e5e3c)(content(Whitespace\" \
         \")))))((Secondary((id \
         fbe668b7-4906-498d-b5c2-195ecdae4b13)(content(Whitespace\" \
         \"))))(Tile((id \
         63bcd6ca-9c7e-43a4-8c17-c67c41b5ba76)(label(Int))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         3678499b-89a2-4085-aaea-627763693f53)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         8c51d049-9320-42f6-aac1-a7342a54b044)(content(Whitespace\"\\n\"))))(Tile((id \
         00770c43-bc10-453a-aa29-34bbb6b85e3f)(label(type = in))(mold((out \
         Exp)(in_(TPat Typ))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         bc6f312e-e78b-48ed-9f1e-6949701e80ae)(content(Whitespace\" \
         \"))))(Tile((id \
         a56d93e6-f588-43a8-9895-c749b2454264)(label(Col))(mold((out \
         TPat)(in_())(nibs(((shape Convex)(sort TPat))((shape Convex)(sort \
         TPat))))))(shards(0))(children())))(Secondary((id \
         e2b1d4cc-0a23-4098-8214-ec7d30467dd2)(content(Whitespace\" \
         \")))))((Secondary((id \
         0a0cf50e-b113-4661-9960-582fc6cbee52)(content(Whitespace\" \
         \"))))(Tile((id \
         745baabf-969c-4555-bb9e-32c7a6c8420d)(label(Int))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         e232fdd3-2f19-407a-968c-fefb693483a7)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         aeeafc38-2372-4857-8365-c85feb334855)(content(Whitespace\"\\n\"))))(Secondary((id \
         8abb3bd5-9716-4c62-9b77-6481409ef87a)(content(Whitespace\"\\n\"))))(Tile((id \
         4b0b652d-f77f-4f00-a0d9-67e6c03fb71d)(label(type = in))(mold((out \
         Exp)(in_(TPat Typ))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         55da20b5-6d12-4a80-999c-400021573a06)(content(Whitespace\" \
         \"))))(Tile((id \
         e182cfa7-a157-4d84-b8a9-00ad58a8a186)(label(Model))(mold((out \
         TPat)(in_())(nibs(((shape Convex)(sort TPat))((shape Convex)(sort \
         TPat))))))(shards(0))(children())))(Secondary((id \
         ab2a8291-0874-4e8a-8944-506bc6c2ae64)(content(Whitespace\" \
         \")))))((Secondary((id \
         4e4594f5-3be5-4e9a-9f6e-ab1606a6f7b5)(content(Whitespace\" \
         \"))))(Tile((id \
         5fe9c641-bc15-42fd-8df1-1536c7128574)(label(\"(\"\")\"))(mold((out \
         Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0 1))(children(((Secondary((id \
         9ae66e89-3728-45da-abc7-781a3f3e3990)(content(Whitespace\"\\n\"))))(Tile((id \
         1341e7f1-5d4e-48b0-888d-2e6c243d9bb5)(label(canvas))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         f884cbdf-e934-4899-ab4f-ed39f947f549)(content(Whitespace\" \
         \"))))(Tile((id \
         aeadd133-e492-475b-9491-fe49dc8e86fc)(label(=))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 39))(sort Typ))((shape(Concave \
         39))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         663b622b-6f6b-44f7-b3c6-2d03d2900f0a)(content(Whitespace\" \
         \"))))(Tile((id \
         1f1da407-6282-4d7d-a9db-c116924244c7)(label(Canvas))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         04c86da6-7639-4c70-b100-8ede3ad0e8af)(label(,))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 44))(sort Typ))((shape(Concave \
         44))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         2a716cfd-d881-40c0-82cf-8908d0aa03b0)(content(Whitespace\"\\n\"))))(Tile((id \
         29804aae-7cce-4136-aeb8-ffa48bb850d9)(label(brush))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         bbeae875-2eb6-484e-9eef-978635a31010)(content(Whitespace\" \
         \"))))(Tile((id \
         27b9c682-2780-48bc-96f8-0f0120a58ec7)(label(=))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 39))(sort Typ))((shape(Concave \
         39))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         2f1d8c21-8595-4afd-9efa-0a10f71ad44c)(content(Whitespace\" \
         \"))))(Tile((id \
         7b2f2323-cfd4-4419-95aa-4d11f2992cae)(label(Emoji))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         a0155571-c12a-4617-9057-58588cbd05a1)(label(,))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 44))(sort Typ))((shape(Concave \
         44))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         4e4bdca0-71df-4fcc-bff8-991d636c212a)(content(Whitespace\"\\n\"))))(Tile((id \
         db9ce395-2748-413c-a800-22a030671ded)(label(palette))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         b1b2dcbb-c207-4ed4-9420-90f497a5dad3)(content(Whitespace\" \
         \"))))(Tile((id \
         87909d37-d4b3-4f6c-9815-f872acd49752)(label(=))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 39))(sort Typ))((shape(Concave \
         39))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         8b46bac2-7853-484b-ac51-e5db0e130571)(content(Whitespace\" \
         \"))))(Tile((id 309bb3fe-a46e-41f0-b4bf-a218019e073b)(label([ \
         ]))(mold((out Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape \
         Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
         f06a58a5-eac3-466c-ab58-3f714b19d099)(label(Emoji))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children()))))))))(Secondary((id \
         979b9123-b02a-479f-bb9a-8a74b19af220)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         97fbe0e5-71b8-47fb-9b92-0bd40384cfff)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         c32d0240-88bd-478d-b256-74027156011c)(content(Whitespace\"\\n\"))))(Secondary((id \
         c4829209-e939-4176-bfb8-2835a5b23e48)(content(Whitespace\"\\n\"))))(Tile((id \
         d56e2d7c-2adf-4a53-a9bf-146dc0a6d77f)(label(type = in))(mold((out \
         Exp)(in_(TPat Typ))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         c048d4c0-28d6-40fc-a655-30d66b292205)(content(Whitespace\" \
         \"))))(Tile((id \
         f2b6b3d6-d5a8-4faf-afbd-b3c65fc6dbb8)(label(Action))(mold((out \
         TPat)(in_())(nibs(((shape Convex)(sort TPat))((shape Convex)(sort \
         TPat))))))(shards(0))(children())))(Secondary((id \
         0867b38e-9aaf-4216-96c9-5534dfc3922c)(content(Whitespace\" \
         \")))))((Secondary((id \
         00d04735-7c64-44ec-85e0-330818a17c00)(content(Whitespace\"\\n\"))))(Tile((id \
         afdb2225-b950-459a-8516-7eaf3bf6e7f4)(label(+))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape(Concave 33))(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         26fe0ef2-2842-4401-ad09-8024df9ee495)(content(Whitespace\" \
         \"))))(Tile((id \
         5dd5f55c-9723-4ee7-8d59-99e46eece4b5)(label(SetBrush))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         a3304d8d-7ba0-4374-9383-f55600d0e825)(label(\"(\"\")\"))(mold((out \
         Typ)(in_(Typ))(nibs(((shape(Concave 11))(sort Typ))((shape \
         Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
         2fedf1cb-bcaf-4f0c-b86f-1c241c1b6a89)(label(Int))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children()))))))))(Secondary((id \
         43f5b951-52e2-401c-8833-237c33d31ce9)(content(Whitespace\"\\n\"))))(Tile((id \
         1f46fa1e-6b1e-4bf7-827c-d37fdc23e971)(label(+))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 12))(sort Typ))((shape(Concave \
         12))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         76f7fcc6-435a-4919-b6f0-bba7b6558a52)(content(Whitespace\" \
         \"))))(Tile((id \
         2fb9350d-c367-487e-a57f-3b97230d0ea6)(label(PaintCell))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         075e3493-069f-4689-b340-5bed6af40cc8)(label(\"(\"\")\"))(mold((out \
         Typ)(in_(Typ))(nibs(((shape(Concave 11))(sort Typ))((shape \
         Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
         1192ae89-738c-4f65-a0d0-da54fe97e4dd)(label(Row))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         ca4e47f3-2e2d-4c62-ae9e-2227a5926868)(label(,))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 44))(sort Typ))((shape(Concave \
         44))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         b0300284-f491-4c18-9965-c58d2f4b899c)(content(Whitespace\" \
         \"))))(Tile((id \
         aba39191-1015-404d-a72c-ec81889a6c99)(label(Col))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children()))))))))(Secondary((id \
         0b0f29b9-e9da-4b38-90df-2b6bbf3fb4f8)(content(Whitespace\"\\n\"))))(Tile((id \
         94f65541-f255-46e7-a703-a6611afcfb08)(label(+))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 12))(sort Typ))((shape(Concave \
         12))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         4b0eaf4a-e1d0-4585-a8d8-f4132ac11ede)(content(Whitespace\" \
         \"))))(Tile((id \
         ca1c7ca6-e9f9-45ba-b109-6da8bff8f075)(label(ClearCell))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         c641d6e5-a19b-4a93-bd8e-7bd65987c2a1)(label(\"(\"\")\"))(mold((out \
         Typ)(in_(Typ))(nibs(((shape(Concave 11))(sort Typ))((shape \
         Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
         baf0edec-df26-4853-92e4-a95c060a7649)(label(Row))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         fb08c3f4-ac66-4734-a295-5b6822434ad5)(label(,))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 44))(sort Typ))((shape(Concave \
         44))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         0098cda7-c175-442d-8a9b-3f343a02cd8e)(content(Whitespace\" \
         \"))))(Tile((id \
         014aa610-1b32-46e2-9e37-6a38a6fad401)(label(Col))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children()))))))))(Secondary((id \
         e7f7d5fa-3644-4f60-8654-669c4f792744)(content(Whitespace\"\\n\"))))(Tile((id \
         70a3de9a-16a8-444f-bc62-7f95da9573d7)(label(+))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 12))(sort Typ))((shape(Concave \
         12))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         46371694-9255-4d14-9213-2e75247d4087)(content(Whitespace\" \
         \"))))(Tile((id \
         e05f729a-31f2-40a3-8861-738159e151e5)(label(ClearGrid))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         2ad9b354-c253-48a4-be0f-c1586bbf835c)(content(Whitespace\"\\n\"))))(Tile((id \
         11f8aea0-cb6f-42cf-a71c-a78e82f923a8)(label(+))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 12))(sort Typ))((shape(Concave \
         12))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         e74fa20f-19af-4a9a-84b6-36b0c6e57f69)(content(Whitespace\" \
         \"))))(Tile((id \
         022db1d7-1716-49d5-b0ed-eaf4aad978dc)(label(PaintRow))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         63786f95-1590-44c6-a743-793c9f804fac)(label(\"(\"\")\"))(mold((out \
         Typ)(in_(Typ))(nibs(((shape(Concave 11))(sort Typ))((shape \
         Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
         0379efd3-206f-4568-a0dd-8024ec518206)(label(Row))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children()))))))))(Secondary((id \
         5eb469ee-c873-4f6d-92a1-e82609cc7954)(content(Whitespace\"\\n\"))))(Secondary((id \
         8ddf9693-d900-47fd-b1b6-45ccac8c5571)(content(Comment\"# TODO: Add \
         PaintCol(Col) here #\"))))(Secondary((id \
         8b790380-743e-4e66-9d33-776b6c672f97)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         2860679c-74b5-4a95-b12c-daa0219ee805)(content(Whitespace\"\\n\"))))(Secondary((id \
         cba7189b-90b9-4341-9cdb-6e6a2fb65ced)(content(Whitespace\"\\n\"))))(Tile((id \
         3e0d90c5-42d9-4ae9-9c49-eb68884d1ac6)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         85b63e00-ff49-4fa4-bc09-9071dc5f69bd)(content(Whitespace\" \
         \"))))(Tile((id \
         b8e4683e-bffe-4385-bdce-ba9fefd522af)(label(init))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         08de6fdc-3cf5-4022-a3d4-08f0a9b3f3e0)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 24))(sort Pat))((shape(Concave \
         24))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         a07f615e-6ffe-422a-a919-ab6581f4bb71)(content(Whitespace\" \
         \"))))(Tile((id \
         a04b6d5a-1d6e-4171-8b47-b32dacd13558)(label(Model))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         d7354b69-d8e8-494e-b697-3218e20af158)(content(Whitespace\" \
         \")))))((Secondary((id \
         0dec10d6-5bd2-463c-94ba-a9e4575d4623)(content(Whitespace\" \
         \"))))(Tile((id \
         c8e3b06a-1cd4-432b-b9db-35c5e91ca1f2)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         c9488270-c133-4ff0-806f-c20022faac9c)(content(Whitespace\"\\n\"))))(Tile((id \
         639f4823-b1e9-4944-9549-86183053b5c4)(label(canvas))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         4ba6796e-8c63-414d-9c50-7be69d488f5d)(content(Whitespace\" \
         \"))))(Tile((id \
         34f01de5-06b9-45fe-893e-4c8dc5e35a53)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         25f6a891-0d72-4842-8cc7-aec41a8ab8df)(content(Whitespace\" \
         \"))))(Tile((id 2b23356e-48a6-40fb-868c-b9dae0b5f721)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         5759dc80-2caf-427e-ae38-c5a7fe3af1ee)(content(Whitespace\"\\n\"))))(Tile((id \
         630daf95-dd16-454a-8492-c98df1b8ceac)(label([ ]))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         28808734-ebc1-49da-a612-12057159e436)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         43f19610-c3aa-43c0-85cb-79f38e01eb60)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         39298132-2596-4427-9851-c7ccfed7d04a)(content(Whitespace\" \
         \"))))(Tile((id \
         2cda7d8d-8ed6-48e7-9ca6-21b3253aa0d1)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         0224d39b-fb11-4ab3-b8f3-1fc5514e8db6)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         e8dc5a5e-68f9-4c4d-baed-48d315346663)(content(Whitespace\" \
         \"))))(Tile((id \
         bac68c08-90d1-445b-b60f-8353f229d883)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         417aaeef-d085-4918-a6b8-fe0fe2ede339)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         c77bc1b5-c904-4cc8-ba1d-00a4cfdc9ca4)(content(Whitespace\"\\n\"))))(Tile((id \
         984875b4-4c2a-45b9-b50c-413512ae2f9f)(label([ ]))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         bc65f4f2-6c72-4a22-8e09-6a1f6ae98431)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         16996177-1065-4c76-8806-9fa4085786c2)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         2d81847e-c177-4faf-9d36-68a4441e23fe)(content(Whitespace\" \
         \"))))(Tile((id \
         fb4e89da-0b60-451f-a457-9e1d52290d46)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         a267112e-6dfe-45c9-a80e-e9a7638c2096)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         18130945-1d91-4cb8-82b6-19ce866fd146)(content(Whitespace\" \
         \"))))(Tile((id \
         aaa8b740-e19d-4fa7-b50d-af728f9be4ed)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         c60b065d-4530-4436-a27c-3c5a71b46935)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         d3b94126-f3a8-46b2-969e-f9aec54ce0e4)(content(Whitespace\"\\n\"))))(Tile((id \
         3ba61549-08c3-46c3-b267-09efffb812e3)(label([ ]))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         7f8ebd83-aac3-4ef7-b1a0-6712f6a7db10)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         55e6321e-cf0f-4f3c-a45f-a4efc544b064)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         d44b8372-7561-4ff3-a3b2-aeffafd7081e)(content(Whitespace\" \
         \"))))(Tile((id \
         f046a7e7-065b-4aee-8d18-3204596a1829)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         61955366-55c8-4683-b12d-a55f767e3025)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         fdb61ba1-62d8-4e00-a41f-8d4b31d55d65)(content(Whitespace\" \
         \"))))(Tile((id \
         f10cf51e-2911-423e-b0aa-3fdcc9c3990f)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         03edb9d7-7743-4666-aeb2-eb6a0ba7a0f2)(content(Whitespace\"\\n\")))))))))(Tile((id \
         b7ba048e-d194-4b56-b4f0-eef89c30e43c)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         50fe242c-b229-4253-b2dc-aa51e4e5c012)(content(Whitespace\"\\n\"))))(Tile((id \
         d93f9638-fe2e-4793-8460-53b78a2b01c3)(label(brush))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         1cc01f54-0324-4ca5-bc96-d81229e481b7)(content(Whitespace\" \
         \"))))(Tile((id \
         beaff024-3659-438b-9c9b-ff17fea196e5)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         e0e3ad60-fa77-4525-bf2e-07b4c40f6b79)(content(Whitespace\" \
         \"))))(Tile((id \
         d398122c-15e0-4868-bab9-facf2f1daa3e)(label(\"\\\"\\240\\159\\142\\168\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         073b8bb3-f63e-4f46-a429-2818af37f6cc)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         d4d47bff-1198-4a17-bbf7-5522e5e2e213)(content(Whitespace\"\\n\"))))(Tile((id \
         9d5acd1c-adb4-4714-91aa-d18e079b2c53)(label(palette))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         c8ed78f1-1019-4fa1-b13a-2d7954175d55)(content(Whitespace\" \
         \"))))(Tile((id \
         96a188b9-b76f-49b4-8705-0467b98df563)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         8cfe7cac-15e9-43b6-8386-fc97cc180936)(content(Whitespace\" \
         \"))))(Tile((id 05b4b582-f344-4280-b4f4-185cd2f099ea)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         ee0ee6ef-475f-4315-9adb-25db17299e9c)(label(\"\\\"\\240\\159\\142\\168\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         f4c5d0e0-3d5d-4ba1-a6e2-0c5fc92e7cf8)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         7b039712-86d0-434e-9f13-761f83e0e960)(content(Whitespace\" \
         \"))))(Tile((id \
         5c2319ec-4cbc-442d-a05a-85c9dd0aa2aa)(label(\"\\\"\\240\\159\\140\\159\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         7299bdf1-5f75-4e66-9026-a04152e9e1b7)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         7f4467f6-58dd-40fe-ab6a-2a3ba57ae43f)(content(Whitespace\" \
         \"))))(Tile((id \
         a24c62ea-cd93-411e-aaa5-e1c220788c90)(label(\"\\\"\\240\\159\\146\\156\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         6c1e1465-4e0b-4f89-a147-57aaa9faeb26)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         f79361a3-6870-4aae-ac7f-7087f0c97add)(content(Whitespace\" \
         \"))))(Tile((id \
         bf4c5d56-8302-4d16-b261-400e2204ed7e)(label(\"\\\"\\240\\159\\148\\165\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         9589adf3-fefc-4e3c-b009-6f914fc64084)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         9e306a98-20b2-4081-80e6-4bf965859f41)(content(Whitespace\" \
         \"))))(Tile((id \
         a2080e49-2e5e-49df-8f03-25b90f5b86ea)(label(\"\\\"\\240\\159\\140\\138\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         a5df3560-77c9-4b1e-b366-8f8b86e52274)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         d17e8dbe-9eb2-4c43-b0e6-959aeb4b60b3)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         a0f156e3-700c-46e7-bae3-42a34d464ef8)(content(Whitespace\"\\n\"))))(Secondary((id \
         ff71845c-924c-4181-bf16-87fbd10f42bf)(content(Whitespace\"\\n\"))))(Tile((id \
         f75933e2-aaba-440c-8fbc-631a3980a331)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         82120f41-6dc2-4b56-9e71-0f67a6233769)(content(Whitespace\" \
         \"))))(Tile((id \
         045dc3b7-46c1-4481-818b-796269cafb91)(label(setCell))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         7145771e-9b19-4c57-b716-5fbc7adbb331)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 24))(sort Pat))((shape(Concave \
         24))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         609ae2a9-c5e7-46a6-84b9-8795ac794b1e)(content(Whitespace\" \
         \"))))(Tile((id \
         a6bc7108-b1f7-4c1d-a7fb-b3dab330bdf7)(label(\"(\"\")\"))(mold((out \
         Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0 1))(children(((Tile((id \
         e4622608-6aaf-40cd-985a-7ba8fd702dd1)(label(Canvas))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         f3e1acc0-aaeb-4f50-a325-a57207b20666)(label(,))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 44))(sort Typ))((shape(Concave \
         44))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         d638c106-daf9-4b6a-86d9-d2dce7a735c5)(content(Whitespace\" \
         \"))))(Tile((id \
         50047cd5-8109-455d-b9d7-3e53b8271863)(label(Row))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         c33ade66-96b8-4436-b88c-7720f7efff79)(label(,))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 44))(sort Typ))((shape(Concave \
         44))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         ba648abb-7708-4f0f-a116-071778174531)(content(Whitespace\" \
         \"))))(Tile((id \
         68c71f66-bcb4-4758-9903-5f36e530fed7)(label(Col))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         f156d652-9433-47d2-aa4c-4680b856ba21)(label(,))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 44))(sort Typ))((shape(Concave \
         44))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         5d519175-9efc-42c6-87f8-9c1f23862da8)(content(Whitespace\" \
         \"))))(Tile((id \
         c3fd9f15-d65a-47f5-88da-3becbf763eca)(label(Emoji))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children()))))))))(Secondary((id \
         6d457458-cb35-4b76-9dc2-08b97df3f1de)(content(Whitespace\" \
         \"))))(Tile((id \
         8399ee9a-6b4b-492a-8298-c4096eb334a7)(label(->))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 13))(sort Typ))((shape(Concave \
         13))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         1605243b-7d62-45f7-bd86-fd713fe8807d)(content(Whitespace\" \
         \"))))(Tile((id \
         c227b32c-f082-437d-ab8a-294663682367)(label(Canvas))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         93430426-2bb8-4558-84d8-e8b9842d6710)(content(Whitespace\" \
         \")))))((Secondary((id \
         cb5aeea3-80f0-486c-b673-5cf5224e0416)(content(Whitespace\"\\n\"))))(Tile((id \
         5cea07bf-6529-406f-93ff-af7a17e3ea7c)(label(fun ->))(mold((out \
         Exp)(in_(Pat))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         37))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         3fd0dbbf-1775-454e-b429-c9a955153717)(content(Whitespace\" \
         \"))))(Tile((id \
         b6c47ef7-17b4-4709-9529-53a7c76caa54)(label(canvas))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         d0ecceff-96df-48cf-93b8-d6cb0a7aa989)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 44))(sort Pat))((shape(Concave \
         44))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         21fe789d-e240-4a78-8ae6-663ab87b0580)(content(Whitespace\" \
         \"))))(Tile((id \
         b4f7352c-1593-40b2-b8c6-140528138d48)(label(row))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         24286ef8-f261-427c-a763-e1bebb86ef70)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 44))(sort Pat))((shape(Concave \
         44))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         76360c37-b7c4-44f1-bce3-efa19558b72b)(content(Whitespace\" \
         \"))))(Tile((id \
         cc538837-baf0-415f-876c-a0a3f3338d40)(label(col))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         4d8ddf22-910f-452a-8815-95174953c25d)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 44))(sort Pat))((shape(Concave \
         44))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         658d50f1-0b15-4edc-91c8-27f562c6af44)(content(Whitespace\" \
         \"))))(Tile((id \
         3ee7bb00-1665-44f7-9ac7-6db347732fa6)(label(emoji))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         d8020d37-b455-4bf4-a172-f86129a9569c)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         adc9870c-4a55-4221-9e15-98ceb230bab0)(content(Whitespace\"\\n\"))))(Tile((id \
         271322a9-9d13-4a8e-9b4a-a0c3670b3821)(label(mapi))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         690e7d4c-c2ef-491b-877b-85decfaf8bb1)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         87d017b5-810d-4bb5-b577-40a455be55f8)(label(canvas))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         3e156bf7-dc65-480f-a465-3ee4d05c12f5)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         92019725-95ca-4e82-b424-3d5b815ad24d)(content(Whitespace\" \
         \"))))(Tile((id 97c2aa2d-57da-45fe-9b04-f2fbdb2c65e6)(label(fun \
         ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 37))(sort Exp))))))(shards(0 \
         1))(children(((Secondary((id \
         f6bfd6af-b52b-44c7-a78d-59180335d329)(content(Whitespace\" \
         \"))))(Tile((id \
         1ab80a27-b4ea-4446-a1b6-5b65c887e9c0)(label(\"(\"\")\"))(mold((out \
         Pat)(in_(Pat))(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0 1))(children(((Tile((id \
         2da75d11-1ba3-42bc-beef-2367d0dd7010)(label(i))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         614f02ed-578a-46f4-8ff8-e835a5cef807)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 44))(sort Pat))((shape(Concave \
         44))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         e05b4e67-79e6-432b-b2a8-c7079cd32a13)(content(Whitespace\" \
         \"))))(Tile((id \
         6367d7ac-4278-473c-9f37-2c24466d9ab5)(label(r))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children()))))))))(Secondary((id \
         adfb39af-7a50-4405-aa1d-767dcfc65f32)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         724332bb-65bc-43df-ad4b-073a0a76739f)(content(Whitespace\"\\n\"))))(Tile((id \
         7ce01ca4-311c-4667-8bcc-c85b309a3c05)(label(if then else))(mold((out \
         Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         36))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         1f3aa09d-4a9b-4f71-99aa-983dfd83878b)(content(Whitespace\" \
         \"))))(Tile((id \
         07387047-4594-43a3-b6c0-ab46515382dc)(label(i))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         265e33fc-b7d8-495d-85f0-1ff9ce59f03c)(content(Whitespace\" \
         \"))))(Tile((id \
         e6deb561-0fcf-409e-addc-0905e9d9e777)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         cb6408ff-4b22-4e12-a302-6a56184bbf09)(content(Whitespace\" \
         \"))))(Tile((id \
         c0a2fecc-b33e-4f46-8197-8a38868ac5b1)(label(row))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         32206004-2b7a-4372-a065-4be2ae83123b)(content(Whitespace\"\\n\")))))((Secondary((id \
         6c4ab9a4-e87b-417f-8aac-8b6ff6e9810e)(content(Whitespace\" \
         \"))))(Tile((id \
         c8eb967a-c6e1-4e79-b9be-fed9f3585223)(label(mapi))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         69fb44e6-beb6-4bf0-8c54-7aab1f158c73)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         485ec84d-0e1d-4ad9-98c5-046714c7b6e2)(label(r))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         099850ae-6182-4326-8247-709eac647c17)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         abbde7f0-7880-4a80-89a5-be3465a59cde)(content(Whitespace\" \
         \"))))(Tile((id 55c393ed-23c4-406b-af70-036be96e0034)(label(fun \
         ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 37))(sort Exp))))))(shards(0 \
         1))(children(((Secondary((id \
         e9910b01-6652-40eb-8426-42914731576d)(content(Whitespace\" \
         \"))))(Tile((id \
         52ae36c3-caed-4da6-a03a-74bbac206f1e)(label(\"(\"\")\"))(mold((out \
         Pat)(in_(Pat))(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0 1))(children(((Tile((id \
         ae1bde19-d12e-45f4-8dc3-88b7fe032edb)(label(j))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         408d1343-f965-4290-8b08-68ddb1c3ab86)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 44))(sort Pat))((shape(Concave \
         44))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         de5164cd-adb0-4f59-8d4b-9591a9f8dae8)(content(Whitespace\" \
         \"))))(Tile((id \
         ca270113-d6ff-45cf-ac05-9cb0b51353aa)(label(c))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children()))))))))(Secondary((id \
         a4adb832-4e3b-4bf3-bdad-78349b3e58e9)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         38a5ecc4-10ec-45d0-8d8c-39572c70e22e)(content(Whitespace\" \
         \"))))(Tile((id 6e6ecdcf-3801-4cf5-addf-69796fce6e5b)(label(if then \
         else))(mold((out Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 36))(sort Exp))))))(shards(0 1 \
         2))(children(((Secondary((id \
         3cec32d2-0e0b-4379-95d7-f240eba78c07)(content(Whitespace\" \
         \"))))(Tile((id \
         fa494bf6-1c15-4be5-a5e1-508c22557e6e)(label(j))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         a6505b47-77e2-4f94-9ae0-4ecd2d7ca8d5)(content(Whitespace\" \
         \"))))(Tile((id \
         4117cad0-64ba-4baa-b5db-ab32f0e324aa)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         395905c7-8f54-4afc-acc6-545234c58cfe)(content(Whitespace\" \
         \"))))(Tile((id \
         690f2dac-8b25-4eb2-92f2-104654d5144e)(label(col))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         d23949c2-93df-4224-89a3-46c0106f5ada)(content(Whitespace\" \
         \")))))((Secondary((id \
         50ac69d3-5958-4ab3-9d10-ad1177c70ca2)(content(Whitespace\" \
         \"))))(Tile((id \
         a6eee22a-7747-41a1-8946-8dace8903f8e)(label(emoji))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         a1898256-7846-44e4-9316-299df23a150f)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         df402b06-7280-4ed8-9db7-cec83405db82)(content(Whitespace\" \
         \"))))(Tile((id \
         6a4cc7f9-37f0-436b-8a2e-099a2de9f235)(label(c))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         a13e1a68-b8d2-4d9d-8791-59dac75d98a5)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         588c574c-811e-40cf-8020-939c67197013)(content(Whitespace\" \
         \"))))(Tile((id \
         349e268d-91d0-4e78-bcdc-a2e92ca2665c)(label(r))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         d037f159-c7fa-4ce5-bf3f-851bf5416e14)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         61bb866c-2383-41cf-9e7d-d31776529737)(content(Whitespace\"\\n\"))))(Secondary((id \
         dd2ae8b3-bf15-4eb0-b964-7db0f9dcb077)(content(Whitespace\"\\n\"))))(Tile((id \
         4c6c666b-4897-4e42-ae5f-8fd663c96c92)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         81adc79a-a739-4fde-b54d-13487ddf9aed)(content(Whitespace\" \
         \"))))(Tile((id \
         6e1faa1e-d115-4d66-817f-4bc6e0045eea)(label(setRow))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         78232cc1-d06e-4250-82c4-33d35d627a44)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 24))(sort Pat))((shape(Concave \
         24))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         6d01adbd-02f5-42bd-95e6-1e65cffe8b6e)(content(Whitespace\" \
         \"))))(Tile((id \
         e4cf8a38-c77f-47aa-88e2-169e740f9a65)(label(\"(\"\")\"))(mold((out \
         Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0 1))(children(((Tile((id \
         0c5f4753-9d97-4490-959f-4c0ce201cb81)(label(Canvas))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         fbbd8147-bab5-4ab2-a596-91ab4e0aec64)(label(,))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 44))(sort Typ))((shape(Concave \
         44))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         728ab982-3058-465f-9f35-e02f42f569bc)(content(Whitespace\" \
         \"))))(Tile((id \
         8196dc44-9171-4b40-80b0-1a8d422becac)(label(Row))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         d09015d8-1103-4001-8c15-4c756c9954f8)(label(,))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 44))(sort Typ))((shape(Concave \
         44))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         878b88a1-a0fb-4847-add5-4389b4558533)(content(Whitespace\" \
         \"))))(Tile((id \
         28aedcc5-762f-45e9-9e34-7e6024b4d981)(label(Emoji))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children()))))))))(Secondary((id \
         a747fb51-6d7a-40e4-a161-0bf98fd6c449)(content(Whitespace\" \
         \"))))(Tile((id \
         5911ebe5-97db-4cfd-818f-d2bda3269294)(label(->))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 13))(sort Typ))((shape(Concave \
         13))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         403c344b-ad4e-43af-bc20-349599a00bb9)(content(Whitespace\" \
         \"))))(Tile((id \
         3df1dd8c-2dd2-4598-a1f8-af4f9a67a1d6)(label(Canvas))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         af3e9e34-d26b-4305-b2f1-0fb6a69a6d69)(content(Whitespace\" \
         \")))))((Secondary((id \
         e589de81-c2d9-4b5e-8e9e-d9e89f9c8c1b)(content(Whitespace\"\\n\"))))(Tile((id \
         15227689-58bb-4d79-af22-4df0a5017056)(label(fun ->))(mold((out \
         Exp)(in_(Pat))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         37))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         57b8773c-6aad-43a8-ba86-5f4a5dc9c0a0)(content(Whitespace\" \
         \"))))(Tile((id \
         cabb7226-a901-4d93-955c-a59980a92203)(label(canvas))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         179fefa4-e35c-43a7-b2bb-80984cf3ceb8)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 44))(sort Pat))((shape(Concave \
         44))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         b576893c-dd27-4bd1-92c1-66da712f07f9)(content(Whitespace\" \
         \"))))(Tile((id \
         8c3b183b-6893-410f-acf6-24877259ccc8)(label(targetRow))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         26ebff16-1ed2-4776-89b3-182e3346d603)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 44))(sort Pat))((shape(Concave \
         44))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         dc9f7f70-466c-41c9-813e-62ac8cc4eca9)(content(Whitespace\" \
         \"))))(Tile((id \
         f324f5b6-563d-4aa0-8343-4baddbf95744)(label(emoji))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         6b023458-ecd0-4e64-b547-e158c040db06)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         a95ef859-f845-4db1-8d58-c2505311134c)(content(Whitespace\"\\n\"))))(Tile((id \
         e663ab48-8331-4d25-97f7-ed0e3749ad16)(label(mapi))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         4353db75-f73e-4fc6-9c1f-e52cacc312f7)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         2321f9a3-9bd3-4fac-9419-c0c8cf269ba7)(label(canvas))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         cba0e740-4405-4ed0-908f-cc9f11cb724e)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         b1e622d1-ab95-4406-9a5e-b0a65dc5ea8b)(content(Whitespace\" \
         \"))))(Tile((id 397741ac-d4f3-4f2f-b9a5-b51413b8154d)(label(fun \
         ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 37))(sort Exp))))))(shards(0 \
         1))(children(((Secondary((id \
         d815e181-a7be-4dfb-baf4-f1d457fdcf43)(content(Whitespace\" \
         \"))))(Tile((id \
         1d162513-a52f-469b-877d-df23fcb62574)(label(\"(\"\")\"))(mold((out \
         Pat)(in_(Pat))(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0 1))(children(((Tile((id \
         e36b482e-b9e9-48c7-b3da-1a5a4acc4275)(label(i))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         e0eb5f8b-3eac-43e0-b0db-acff98a8f70e)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 44))(sort Pat))((shape(Concave \
         44))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         da6adcbe-e161-4a4b-8c8d-a77178d35aa7)(content(Whitespace\" \
         \"))))(Tile((id \
         f2f443aa-1fcc-4dc6-8535-7ab2af4828aa)(label(row))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children()))))))))(Secondary((id \
         e7a348fb-cafd-4d68-82a7-2453c2347a9c)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         f44840e6-00b0-49c7-9083-4f05a337de5b)(content(Whitespace\"\\n\"))))(Tile((id \
         e5527245-ead8-419e-9bb1-74672a778546)(label(if then else))(mold((out \
         Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         36))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         9fa13abf-9e86-45c9-abc0-e35e4faa5f86)(content(Whitespace\" \
         \"))))(Tile((id \
         dd91e15a-bfc2-4349-8f1d-3001c7c36d6c)(label(i))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         91e19824-902c-42f3-a3f6-e3d181107d96)(content(Whitespace\" \
         \"))))(Tile((id \
         b77325c4-1fb9-4ce0-a8cf-41650db6a38b)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         c374683e-5864-4fd7-b0c0-7bd9b72adca1)(content(Whitespace\" \
         \"))))(Tile((id \
         ed67b84f-58e9-42aa-afda-14be4f2c5dcc)(label(targetRow))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         013bfe34-26b2-497d-a188-cfe181d95429)(content(Whitespace\"\\n\")))))((Secondary((id \
         76e7a87d-e052-4066-96c9-18ac78243d6d)(content(Whitespace\" \
         \"))))(Tile((id \
         36c71de4-36ab-44f4-87a2-5b70bfaded13)(label(map))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         dc41e5b8-a85d-4880-8f44-2b02620c6f21)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         3b24528c-a465-4f31-9e7c-932302f5f3d7)(label(row))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         c2599d6b-75e1-4d79-8f4c-0a313100e60b)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         dae09cda-2ad8-4d14-9a11-55d9f422e915)(content(Whitespace\" \
         \"))))(Tile((id 8b0f26e6-0892-43f9-8069-62df8bada6a0)(label(fun \
         ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 37))(sort Exp))))))(shards(0 \
         1))(children(((Secondary((id \
         19e41a13-0606-4186-9bdd-dc27187ceb48)(content(Whitespace\" \
         \"))))(Tile((id \
         ecac1f9f-c805-4f43-8617-1caed4c56c53)(label(_))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         aff4fd5b-41ad-4f3c-b36b-40359a5fc7cb)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         78af8956-bbc9-472a-8d4a-5747f25167bb)(content(Whitespace\" \
         \"))))(Tile((id \
         e859779b-ad83-4d69-89ba-3d7c4fe10d85)(label(emoji))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         86de4599-8459-4af3-a9da-4237f094e1a9)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         f946a246-da7e-425e-9489-9885ea83f7d3)(content(Whitespace\" \
         \"))))(Tile((id \
         da682926-e0d7-4014-aff2-419a75f48ba2)(label(row))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         447ea4b6-99f4-4ad3-a747-4e67219c120d)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         71969e85-2aff-4baa-964a-afff644384a1)(content(Whitespace\"\\n\"))))(Secondary((id \
         a21aefee-ca50-46b9-8598-63fe071727cd)(content(Whitespace\"\\n\"))))(Secondary((id \
         f38469ed-a6a0-4c18-ac93-cd460996e60d)(content(Comment\"# TODO: Add \
         setCol helper here #\"))))(Secondary((id \
         0a2418ae-facc-4beb-b76b-fe8b00a36032)(content(Whitespace\"\\n\"))))(Secondary((id \
         7f2ed526-45d3-41fd-875b-3f5345edb49a)(content(Comment\"# Hint: You \
         need to modify each row, changing #\"))))(Secondary((id \
         732399b3-9489-4170-a918-6e1762f37f36)(content(Whitespace\"\\n\"))))(Secondary((id \
         a66e0df1-9c9b-4308-becc-b377adfd46a6)(content(Comment\"# only the \
         cell at the target column.         #\"))))(Secondary((id \
         f301911f-dd7c-489e-b612-57a4870974c1)(content(Whitespace\"\\n\"))))(Secondary((id \
         2f27bcde-96f2-45b7-8700-94fe9c8f3a7f)(content(Whitespace\"\\n\"))))(Tile((id \
         f5cf7382-d1fb-48f3-a80d-9b538ed7a7b9)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         36979762-42d3-47d9-85a0-099d2106962d)(content(Whitespace\" \
         \"))))(Tile((id \
         ba98376f-ddda-4d19-a5e8-1feb779c7127)(label(setAll))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         d9a7393f-4c55-4024-9eed-49203bf48767)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 24))(sort Pat))((shape(Concave \
         24))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         bca2dd32-a7c2-4a72-9b4f-effaa082084f)(content(Whitespace\" \
         \"))))(Tile((id \
         1168f7c2-1d96-4f8a-bac8-2622f818ab04)(label(\"(\"\")\"))(mold((out \
         Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0 1))(children(((Tile((id \
         155dcabc-2e5e-45ce-87a8-97d502114292)(label(Canvas))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         5bd03a91-0bd8-476c-b0dd-a14d64eaaa90)(label(,))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 44))(sort Typ))((shape(Concave \
         44))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         e7dda87e-b3b2-4c34-a415-bb5282a4b8d6)(content(Whitespace\" \
         \"))))(Tile((id \
         1d1d8c8e-7195-43af-9e5c-9f926b8de3c3)(label(Emoji))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children()))))))))(Secondary((id \
         e2c6ba0c-653c-4ed0-bbd1-07615eb68836)(content(Whitespace\" \
         \"))))(Tile((id \
         b7631569-52a0-4d7a-a380-4086d2e58b45)(label(->))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 13))(sort Typ))((shape(Concave \
         13))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         d0b8db60-23e9-4ce6-8442-40907c86c9ba)(content(Whitespace\" \
         \"))))(Tile((id \
         d777cb2a-360f-43ba-a135-d435aa599e92)(label(Canvas))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         640f7630-65a4-4428-a756-cb008aed9c2a)(content(Whitespace\" \
         \")))))((Secondary((id \
         70eeada5-bb41-4652-ad27-817ebbb22df6)(content(Whitespace\"\\n\"))))(Tile((id \
         92c40e65-fea2-4d37-bafc-276d983047fc)(label(fun ->))(mold((out \
         Exp)(in_(Pat))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         37))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         775222cc-378b-47ce-a3b8-416352849298)(content(Whitespace\" \
         \"))))(Tile((id \
         1d98f183-2284-46da-a80b-19b47b84f399)(label(\"(\"\")\"))(mold((out \
         Pat)(in_(Pat))(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0 1))(children(((Tile((id \
         806533ca-1d7b-4a2c-bfee-21bdb13f6ec2)(label(canvas))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         58df96d7-95b2-47a6-b9f8-610d994a47c5)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 44))(sort Pat))((shape(Concave \
         44))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         60af6692-c98c-400b-8464-a6b20add37b6)(content(Whitespace\" \
         \"))))(Tile((id \
         4e703333-4a30-4ee3-be36-233ec978e500)(label(emoji))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children()))))))))(Secondary((id \
         7742e7e7-d82d-42d1-a18d-17e80e12b860)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         9371cb3b-e24b-4d67-b263-c180ced87f3b)(content(Whitespace\"\\n\"))))(Tile((id \
         61914952-8f03-493b-9a8d-29912cbd3055)(label(map))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         9cebcdda-cb69-448c-8a8a-ff59ee5a88c3)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         454458ed-554b-4e3b-bdbb-df765c9b8f65)(label(canvas))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         84a22b0c-8399-41d0-9ef1-78be4bb16172)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         5ca1e296-2aa9-4280-b1f5-5e4213da95cf)(content(Whitespace\" \
         \"))))(Tile((id 847b89cf-e3e3-4bc5-901f-ba4f970991ee)(label(fun \
         ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 37))(sort Exp))))))(shards(0 \
         1))(children(((Secondary((id \
         a4c70fc1-9995-4374-a7d2-6a8b78805222)(content(Whitespace\" \
         \"))))(Tile((id \
         852c8d32-ffc7-4f02-9f4e-9c5627ca2bf8)(label(row))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         7bf04fba-f4a7-4294-98ff-722df0780f7b)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         317e1eaf-0357-4580-a9c8-dc99c64f69cb)(content(Whitespace\" \
         \"))))(Tile((id \
         67a729e3-2dc7-4494-aca2-08ac22b85b50)(label(map))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         256a4fa6-cf23-4542-9e48-cc4550886a99)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         0f5ccb69-e7b6-42f0-a810-aaa40962e5f9)(label(row))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         e21cb3b3-f4f4-4445-a5ec-b53b42117914)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         b1333dec-b482-4d3d-8e16-ac4244a14b22)(content(Whitespace\" \
         \"))))(Tile((id 68ab7294-2e07-4e84-b43e-52ac0db3c89d)(label(fun \
         ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 37))(sort Exp))))))(shards(0 \
         1))(children(((Secondary((id \
         d5f15286-5dee-4514-9df7-1d0c3aa8778b)(content(Whitespace\" \
         \"))))(Tile((id \
         06cc1d10-d6bf-445c-b409-008a7f5d3a0f)(label(_))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         959120cb-8fe4-492e-a141-425cde89437f)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         1dbe19ed-2ae4-49b6-b47a-182246492c87)(content(Whitespace\" \
         \"))))(Tile((id \
         d46e4935-9f16-4e67-bb91-b03bd24f06a6)(label(emoji))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))))))))))))(Secondary((id \
         93ea28e6-fbfc-4a6d-bb34-71961f2bd36d)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         4df518f3-550a-44f2-8925-2114396cb4a8)(content(Whitespace\"\\n\"))))(Secondary((id \
         b99c5bf6-6fdf-4ba2-9dac-ccd85a37e479)(content(Whitespace\"\\n\"))))(Tile((id \
         caae1836-8e8d-4e47-88d5-186d7c8542ce)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         cc298e65-2f7f-42ce-b080-150e5fb380b7)(content(Whitespace\" \
         \"))))(Tile((id \
         c0a7d515-4eca-41ce-b502-b925842ef1d1)(label(updateGrid))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         e41597c2-8ed3-4893-88ba-9c1229217f6b)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 24))(sort Pat))((shape(Concave \
         24))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         d27a0b27-20eb-4096-a82c-f7dabc891b10)(content(Whitespace\" \
         \"))))(Tile((id \
         2c03cd39-8f8b-40a8-9402-3eff3fcf1935)(label(\"(\"\")\"))(mold((out \
         Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0 1))(children(((Tile((id \
         03b6c722-d93e-4cff-9bb0-54f1eba653b4)(label(Model))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         56896888-742b-4444-968c-3e5cb5cd2644)(label(,))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 44))(sort Typ))((shape(Concave \
         44))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         19c25ab2-954d-4062-9bcc-53f5dd2570db)(content(Whitespace\" \
         \"))))(Tile((id \
         bed6b5a3-9cf9-4673-870b-2b821d64559e)(label(Canvas))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         b98b9add-4b77-4b04-a7a9-becc4410b01d)(content(Whitespace\" \
         \"))))(Tile((id \
         f9c96685-fbf3-4df8-926f-41201380df5f)(label(->))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 13))(sort Typ))((shape(Concave \
         13))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         038e1b02-0568-4753-af8f-6a6085519b6b)(content(Whitespace\" \
         \"))))(Tile((id \
         429e50fe-0cb0-4dd1-bfc5-c45fbf421d82)(label(Canvas))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children()))))))))(Secondary((id \
         15475f36-285c-4af6-8944-b0867d2ad7e6)(content(Whitespace\" \
         \"))))(Tile((id \
         ecb7896f-ff92-491c-ab11-60c6c76cb99a)(label(->))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 13))(sort Typ))((shape(Concave \
         13))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         af99f837-03b6-470a-bae4-d0fee2b9aba1)(content(Whitespace\" \
         \"))))(Tile((id \
         0e5994ac-358b-4b10-ab26-bd72c3a98932)(label(Model))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         7ea19404-513b-4788-b0c6-49af7f390c04)(content(Whitespace\" \
         \")))))((Secondary((id \
         2126ab91-2a9c-4714-a112-9609d8401fde)(content(Whitespace\"\\n\"))))(Tile((id \
         edee5fea-6306-45ab-bbe6-79d714c36899)(label(fun ->))(mold((out \
         Exp)(in_(Pat))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         37))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         7ea2df3d-0d97-4442-98c5-d1918ca9ab0d)(content(Whitespace\" \
         \"))))(Tile((id \
         d47a3c51-9954-43f0-ab42-7f1c896b3c29)(label(\"(\"\")\"))(mold((out \
         Pat)(in_(Pat))(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0 1))(children(((Tile((id \
         db4bd5e2-708e-4da1-8693-12f8d9f94585)(label(m))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         10d46344-555c-43ff-a0ac-186604993c0a)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 44))(sort Pat))((shape(Concave \
         44))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         50f737ae-262b-4681-b796-b61e37da193f)(content(Whitespace\" \
         \"))))(Tile((id \
         b37e8957-c12b-4ed9-a78f-5a6e4c64d406)(label(f))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children()))))))))(Secondary((id \
         c0315991-6ec7-4478-ac63-f71eae94f236)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         dad7a304-5339-48f4-911d-41dffab2b647)(content(Whitespace\" \
         \"))))(Tile((id \
         6f7293a4-83e9-443f-a989-8d547c7d92e1)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         1e911e92-df8b-4612-a557-8e5e973e10ba)(label(f))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         30ca7e1f-43a5-46c3-898b-f46d89eece79)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         88f66c9b-6ff8-49cb-b40a-e88f25010cf0)(label(m))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         205fa494-6762-4fc1-af73-b2e581b2a96e)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         fddccd94-3306-4366-bfe0-da4cb4a5d95d)(label(canvas))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         05350056-15c5-4860-a7b7-7b8dc73bf729)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         7b64260d-e7e8-43be-a59a-ea487bc0cb31)(content(Whitespace\" \
         \"))))(Tile((id \
         8c590bcc-519f-4208-a198-0d1108f70622)(label(m))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         ce6f2d19-1a04-4c21-b26a-80f08fd77adb)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         cfacd159-015f-464a-8640-c9492e47432e)(label(brush))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         abb40c0a-6fad-4324-bb92-5e75696514db)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         f6155c38-0d31-4715-ae2b-4d68339efe08)(content(Whitespace\" \
         \"))))(Tile((id \
         37119940-3ce4-40a1-815c-674e46110746)(label(m))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         77893764-19a2-44b1-bf17-a3bd9f3b807e)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         7d752c1d-b401-4fc6-a36b-c155d4345008)(label(palette))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         a2005128-2fc9-48ea-9e80-b4a954196cdc)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         5adf0822-fdb0-4ad0-b9df-4fd5155e5070)(content(Whitespace\"\\n\"))))(Secondary((id \
         ab961a2c-b7fa-40b3-9ef3-a9390f57da6b)(content(Whitespace\"\\n\"))))(Tile((id \
         76a900a7-f63a-4d7f-ad30-f3840f70aaaf)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         625e4214-1c73-4e0d-bea7-578c295825e2)(content(Whitespace\" \
         \"))))(Tile((id \
         76925f24-4a71-48cb-8fb5-52b3653f0e19)(label(update))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         813c0657-75fa-4335-a6db-1e54b6268189)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 24))(sort Pat))((shape(Concave \
         24))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         86f9d7ae-fe7c-40bd-8fce-8df942b69969)(content(Whitespace\" \
         \"))))(Tile((id \
         7158a2b5-8c73-47d4-bd75-d39584438dcd)(label(\"(\"\")\"))(mold((out \
         Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0 1))(children(((Tile((id \
         36d9dbea-0e83-4689-bb3e-7aabb21d2975)(label(Model))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         5f3a787a-5024-4efd-b404-7e07a578109a)(label(,))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 44))(sort Typ))((shape(Concave \
         44))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         32bd19e8-7085-425d-bd12-903b75e33245)(content(Whitespace\" \
         \"))))(Tile((id \
         5b99d035-cc9d-45c4-97e6-85d646454de1)(label(Action))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children()))))))))(Secondary((id \
         6e6b51a0-2d16-48ae-b1aa-00ea48e11811)(content(Whitespace\" \
         \"))))(Tile((id \
         dbbe49f7-6260-4f67-a445-98db1ef0cdd3)(label(->))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 13))(sort Typ))((shape(Concave \
         13))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         53d2281c-bc02-4599-8705-b08c652241bf)(content(Whitespace\" \
         \"))))(Tile((id \
         75fae483-d787-48d8-b2d0-98611c2c29b3)(label(Model))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         b8baeb1c-289c-41d2-ab72-c5d45d92bb2c)(content(Whitespace\" \
         \")))))((Secondary((id \
         8672dfea-a1f1-49f8-af02-141e5ce7e2f2)(content(Whitespace\"\\n\"))))(Tile((id \
         fa473827-8d2f-49c6-b6ed-f0518fdd6020)(label(fun ->))(mold((out \
         Exp)(in_(Pat))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         37))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         70bb1816-5b87-4ee3-a660-43bd95408e16)(content(Whitespace\" \
         \"))))(Tile((id \
         93e610f2-c56e-4d53-8194-48bc8350cc97)(label(m))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         b03d8a01-bd09-4a61-832c-9e9c79713946)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 44))(sort Pat))((shape(Concave \
         44))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         316453ce-9650-43a3-957b-9a06c434bed4)(content(Whitespace\" \
         \"))))(Tile((id \
         46dc7506-4726-4f0a-ae6e-6ba75cf221a1)(label(action))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         60d1c8d8-67e3-4fed-81e5-1306cba63805)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         e5512c64-061d-4d46-84b4-67934a57f20e)(content(Whitespace\"\\n\"))))(Tile((id \
         473897a9-d94a-40f5-b19c-a33cbd30583d)(label(case end))(mold((out \
         Exp)(in_(Rul))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         027958dc-06d1-4c40-8fdc-d12c5b626824)(content(Whitespace\" \
         \"))))(Tile((id \
         f0b6b3f0-5dbf-46fa-9e95-fcc2c6b28e1d)(label(action))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         9747e4d5-027e-44fe-b2a4-99f4670d53d0)(content(Whitespace\"\\n\"))))(Tile((id \
         99e251bf-38b4-4698-ad14-cc19244ed961)(label(| =>))(mold((out \
         Rul)(in_(Pat))(nibs(((shape(Concave 46))(sort Exp))((shape(Concave \
         46))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         cd1f9ea9-0713-40fb-bd5e-e995bbe40920)(content(Whitespace\" \
         \"))))(Tile((id \
         a34cb292-50c0-4497-98cb-5148f7c544a6)(label(SetBrush))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         d40eb5c9-5724-460a-895b-edd788585c8c)(label(\"(\"\")\"))(mold((out \
         Pat)(in_(Pat))(nibs(((shape(Concave 23))(sort Pat))((shape \
         Convex)(sort Pat))))))(shards(0 1))(children(((Tile((id \
         31933189-abf7-4d72-8983-d99846ea76b7)(label(idx))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children()))))))))(Secondary((id \
         90bad065-2541-4e5b-ae5e-e8050f836a95)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         6aa9874f-d731-4aa6-91c9-388d6e4cb02c)(content(Whitespace\"\\n\"))))(Tile((id \
         75fcef00-1f96-4e7a-929a-0cffb7744ef9)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         d87d05f1-fafd-4e0a-99e2-cf878d23db9e)(label(m))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         58681d60-633f-4364-8dda-8cf00e1d2dfc)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         ee77a47d-af1a-4852-92f6-bafdca0e6f3f)(label(canvas))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         9fa9d284-aca8-4791-ab24-4034e7b6f2d4)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         82cd6de1-fc52-498c-ae2d-040bf02eb89f)(content(Whitespace\" \
         \"))))(Tile((id \
         bafe6295-7314-416b-8f49-a1cf6c9f580a)(label(nth))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         42e3e815-b7a3-40be-900e-7bebf7bfd58b)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         26ca406b-3df8-4386-97b9-a363f2a88866)(label(m))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         bae828fd-d1f9-4c38-809c-febae1fb738c)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         aa49321c-6db9-4c6a-bdd4-f029d40aa9e7)(label(palette))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         0e5cfb75-bc93-43ad-b207-d82144a13125)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         707cbdd9-bfd3-429e-bdb3-17195bb390aa)(content(Whitespace\" \
         \"))))(Tile((id \
         e66e59cf-f409-4a83-b990-74122db83f63)(label(idx))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         66d48dc7-b535-4bf0-a8cc-69750419fd7f)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         e67844d4-0a81-4d6a-8eb1-c1b49d5256f8)(content(Whitespace\" \
         \"))))(Tile((id \
         3ac40e0a-fec7-41a8-9ebf-b0df671c86c5)(label(m))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         3bdd14d4-98c7-45a8-9257-03dbeebdb87f)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         85a8be16-3ac7-4589-9128-fbbc4b49b09e)(label(palette))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         e8a60a09-60be-4020-8cb8-adbf7254935d)(content(Whitespace\"\\n\"))))(Tile((id \
         e4d4301f-39f2-442a-b551-9ad33b84dc5b)(label(| =>))(mold((out \
         Rul)(in_(Pat))(nibs(((shape(Concave 46))(sort Exp))((shape(Concave \
         46))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         115f634d-6f72-42f7-82fd-b244394961ce)(content(Whitespace\" \
         \"))))(Tile((id \
         e4cfc043-be64-472a-98d8-2bc5428002c2)(label(PaintCell))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         e167ef0d-6715-4191-95ed-7c2b43bfd1b6)(label(\"(\"\")\"))(mold((out \
         Pat)(in_(Pat))(nibs(((shape(Concave 23))(sort Pat))((shape \
         Convex)(sort Pat))))))(shards(0 1))(children(((Tile((id \
         4194b92f-5e51-4a87-95e2-07aa9c8027a5)(label(row))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         4f8dffd9-cb76-475a-9ea9-febd142cd7fc)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 44))(sort Pat))((shape(Concave \
         44))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         eee997f5-75f5-4ef4-be52-ba75fedf9592)(content(Whitespace\" \
         \"))))(Tile((id \
         b02cf57e-ebb2-4f68-9b9c-92d88149a5b6)(label(col))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children()))))))))(Secondary((id \
         b380d1a6-bc61-45f1-81e6-43addb4d2ed4)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         34ef2471-0feb-4edd-83c7-36e2dab5f6d4)(content(Whitespace\"\\n\"))))(Tile((id \
         a89ec247-5c6a-41a5-ad7c-6042fb09817e)(label(updateGrid))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         f12c4bbe-ff92-4160-89b1-f36b58f679a6)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         e4d92454-5025-41a8-93bb-4579e5d39aa7)(label(m))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         6e5a3c43-c382-455a-99e4-1b00c29045c7)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         89890ceb-04ce-4502-bade-da4ce0e93a6d)(content(Whitespace\" \
         \"))))(Tile((id 5f4df099-c189-42bc-843f-29e0b31e90b0)(label(fun \
         ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 37))(sort Exp))))))(shards(0 \
         1))(children(((Secondary((id \
         51aca955-b444-421e-9ca4-5fbcbcd7a39a)(content(Whitespace\" \
         \"))))(Tile((id \
         d60b15c4-3c85-4f7c-bc34-c4794138a36d)(label(c))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         5ddb8967-8bac-43b2-9a71-2c4fe1d04d19)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         c8acfbee-3c3b-4776-b614-3ef01462827d)(content(Whitespace\" \
         \"))))(Tile((id \
         ffe1d038-484a-4665-bd34-87a8a10a6f2b)(label(setCell))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         74b0b2cb-f1b8-4d09-b995-a4c2f0e3c401)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         0d0038d3-62e5-4ef9-9443-5e34fc46bc49)(label(c))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         5fff938c-5061-4ec4-8809-c70749c0d963)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         f18b2a47-24cf-42a8-9cee-310f6ae378e6)(content(Whitespace\" \
         \"))))(Tile((id \
         557f9b45-de54-4302-9ddf-a3d775b53e01)(label(row))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         29b11d21-4faf-468f-a1e7-b2a035447ac3)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         d5892fd5-a87b-4a63-82ad-f5d6bd3812eb)(content(Whitespace\" \
         \"))))(Tile((id \
         2ed7f9a9-9611-43df-aaa2-10cdd8082a7b)(label(col))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         21ff4cc1-d559-405a-9d3f-ca6b9d827f8c)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         2601f630-724b-456f-bd33-4ce3cac684e6)(content(Whitespace\" \
         \"))))(Tile((id \
         21655128-ff1b-4fbf-a6e7-cdd11bf4153c)(label(m))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         6b07d224-cdbf-40ba-8f56-fbe17b1b82fb)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         73a0ea2c-861e-43f4-996c-3c92a553a01d)(label(brush))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))))))))))))(Secondary((id \
         5e3cd801-c533-4e58-8161-64fb63168e5d)(content(Whitespace\"\\n\"))))(Tile((id \
         3db42f75-5d69-4762-9158-705045f2fab4)(label(| =>))(mold((out \
         Rul)(in_(Pat))(nibs(((shape(Concave 46))(sort Exp))((shape(Concave \
         46))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         81df759f-4adc-4b8a-82a3-ca49bc08eb40)(content(Whitespace\" \
         \"))))(Tile((id \
         e4598b8c-8197-4fb0-815e-d60c25fde3a3)(label(ClearCell))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         1de10add-cf49-4cab-b940-217dd628d788)(label(\"(\"\")\"))(mold((out \
         Pat)(in_(Pat))(nibs(((shape(Concave 23))(sort Pat))((shape \
         Convex)(sort Pat))))))(shards(0 1))(children(((Tile((id \
         ba6c937a-5968-4a11-9f7d-ee558b3e3817)(label(row))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         80ee67e0-95eb-4e58-920d-097b70ae42ed)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 44))(sort Pat))((shape(Concave \
         44))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         0763e9f3-6c91-436d-8d26-b4b3e6edf82f)(content(Whitespace\" \
         \"))))(Tile((id \
         5ef16f49-87d7-4b92-b6dc-e901efce2afb)(label(col))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children()))))))))(Secondary((id \
         805ae2a0-b947-4e8b-b250-52b2bb1ddd72)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         dfba4a0b-a69c-47ff-80c7-93b36a7d603c)(content(Whitespace\"\\n\"))))(Tile((id \
         bb56915e-9aa6-4146-87f8-5a9e148d755d)(label(updateGrid))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         0ee07b6b-f3ec-407b-af69-ae0384fb0c14)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         1658e012-1609-41c9-a81e-7ce4444b7c1f)(label(m))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         0c085f02-8db1-4169-873e-9f215443709d)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         955d10f7-5e9f-453e-b81b-84fe0a960473)(content(Whitespace\" \
         \"))))(Tile((id eb922626-186e-47c8-8af3-28ac29d63821)(label(fun \
         ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 37))(sort Exp))))))(shards(0 \
         1))(children(((Secondary((id \
         2a1b571d-ba77-4ca9-88eb-1caf7bc6015f)(content(Whitespace\" \
         \"))))(Tile((id \
         22c90606-88ed-4dda-9e5e-3c0f5da206f8)(label(c))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         124f35fd-9d30-4472-b518-c77ec8cbf8b0)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         43751fb4-61ae-42be-8ae2-f2da38dde119)(content(Whitespace\" \
         \"))))(Tile((id \
         6bb1ae6b-8e0b-423c-a427-8605d63bc27f)(label(setCell))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         1be71fda-fba0-40ee-83f5-b056250ac9b4)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         1d5b2f36-9505-4186-bf10-2e1245bdb91c)(label(c))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         c8bc4f45-b0c2-46ca-bd31-17fb2fb68b27)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         2744a5a4-8489-406a-bdae-97accc1619b2)(content(Whitespace\" \
         \"))))(Tile((id \
         276949e7-3a3c-4b8d-b8e1-167af3b117eb)(label(row))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         a68869b1-907a-46bf-a728-0055c5711d4a)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         40897cd0-0099-4358-8871-cfcdd3bf209c)(content(Whitespace\" \
         \"))))(Tile((id \
         173ce3ba-0e2a-4790-96f9-0e5a44d8ae1a)(label(col))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         f1c00621-3410-4a3f-80ef-22bde7c8b6ba)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         ff730d90-5241-49fc-b2f5-31a023f4dd99)(content(Whitespace\" \
         \"))))(Tile((id \
         ef77a3c1-6e58-458e-b1dd-51d0a2165543)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))))))))))))(Secondary((id \
         d7069678-9e12-448c-af9b-be38389001e2)(content(Whitespace\"\\n\"))))(Tile((id \
         f6c4d8d9-90e1-4394-a8da-c15377e1d731)(label(| =>))(mold((out \
         Rul)(in_(Pat))(nibs(((shape(Concave 46))(sort Exp))((shape(Concave \
         46))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         7c4641cc-51f5-48ba-bbb9-127341aeeac0)(content(Whitespace\" \
         \"))))(Tile((id \
         40a9f3ca-68ab-46c6-a938-3661e7d4a7cf)(label(ClearGrid))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         69bd1d44-54f2-4724-80b9-5dde34cc6f28)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         a6d68f3d-d009-4918-89a5-f4305351c52e)(content(Whitespace\"\\n\"))))(Tile((id \
         dc64950f-c07f-4380-96c5-e3dec539ec6d)(label(updateGrid))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         5d058cc7-00be-46a6-999a-8b15145062e7)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         f250e0ab-6d16-40c1-9d0a-080b09be6d9b)(label(m))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         3a63d10e-b6ce-4fd7-ad19-7d8aed5d6e70)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         a5914599-a318-4a7d-9556-f79b284ba641)(content(Whitespace\" \
         \"))))(Tile((id 9d19efb8-a8b1-4dbb-88a1-7f52e78751be)(label(fun \
         ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 37))(sort Exp))))))(shards(0 \
         1))(children(((Secondary((id \
         336955c6-2040-457d-afec-e844f404f3dd)(content(Whitespace\" \
         \"))))(Tile((id \
         03924f9f-9cb1-4f31-bb0c-0d42cd961d67)(label(c))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         d1fbd7c4-5a00-4f80-bca8-a0cbbfcc0336)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         aaabe82e-fcbb-4ab4-96d8-070ad7280c45)(content(Whitespace\" \
         \"))))(Tile((id \
         a2f7a0dd-dc56-444d-9360-b816e8ee6d3d)(label(setAll))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         81920c40-703a-46d8-b6e3-1e66893a2a47)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         8b6bff97-0049-4f14-8362-c9d4b642248c)(label(c))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         2c8ee272-7ade-45e2-9a16-0db73c89dec4)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         8dffdd01-4de0-4d67-b1b8-e3d74ec1cd0b)(content(Whitespace\" \
         \"))))(Tile((id \
         db41e39c-b932-41c9-84dd-266986d596a4)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))))))))))))(Secondary((id \
         fd1ba883-ff5d-4f9f-b514-086a39b78835)(content(Whitespace\"\\n\"))))(Tile((id \
         f0b849a5-0ca6-4fae-b2db-6a2e85ef05a4)(label(| =>))(mold((out \
         Rul)(in_(Pat))(nibs(((shape(Concave 46))(sort Exp))((shape(Concave \
         46))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         cf0114af-b78a-4654-b0ae-147923e77eff)(content(Whitespace\" \
         \"))))(Tile((id \
         48ce56dc-8611-4b07-9d7b-11cc1dd234de)(label(PaintRow))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         c904723b-aa07-4fe3-9231-69a1994f0ffe)(label(\"(\"\")\"))(mold((out \
         Pat)(in_(Pat))(nibs(((shape(Concave 23))(sort Pat))((shape \
         Convex)(sort Pat))))))(shards(0 1))(children(((Tile((id \
         79e824a9-a467-4b10-91d5-6ca5731c11dd)(label(row))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children()))))))))(Secondary((id \
         420e62a4-ba0b-412a-8fd9-0cd3c1ea8f2e)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         4a83e760-36c1-4626-ae15-f3833ddd08ce)(content(Whitespace\"\\n\"))))(Tile((id \
         b93db516-17f6-465e-a58a-737903d7eca4)(label(updateGrid))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         b02fd8c7-203a-41d6-b96a-2fbbcbddde1f)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         a3f3032f-1a2a-4edc-a0ee-de3dff51235b)(label(m))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         7e2148b1-be4f-4cb7-b445-944c0b7c2c7b)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         3061dfd1-007a-43bb-b479-5406fed7558a)(content(Whitespace\" \
         \"))))(Tile((id f58634ea-17db-4871-9162-62239e562cd3)(label(fun \
         ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 37))(sort Exp))))))(shards(0 \
         1))(children(((Secondary((id \
         bbe1ce7b-aa7e-4991-85e6-318d1ece521e)(content(Whitespace\" \
         \"))))(Tile((id \
         c5d0846b-12ef-4c8a-87da-3109e7ddbdde)(label(c))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         d5e58e15-1dd0-4178-b364-40e3f41b802e)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         3bb534c7-c6e6-4024-a66b-b910bb22a84a)(content(Whitespace\" \
         \"))))(Tile((id \
         18e9476c-2b48-44a7-bc5b-b8ecab608a37)(label(setRow))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         87f2f171-9928-45c5-a40d-ec8c1c23be14)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         ec63f768-e485-414f-80ca-7daaa4838c7d)(label(c))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         4cb179fa-1814-4e1c-bdc7-da6b0f7564d4)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         92483ed5-c248-4f1e-934d-178bfc18a546)(content(Whitespace\" \
         \"))))(Tile((id \
         72a23092-8b46-4d2c-8e61-7110f8bafbf5)(label(row))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         71423974-cbc8-4b9f-96c6-eac8a112aaea)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         e0a42fce-0363-44ab-86a5-e7ee5460b5c9)(content(Whitespace\" \
         \"))))(Tile((id \
         0ed95a33-66da-4b0d-9691-3488372f42e0)(label(m))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         56a0f1a9-be54-4f7a-bf71-4e6508ffbecb)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         d957e0c7-6355-4553-9925-f21db1fb58c8)(label(brush))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))))))))))))(Secondary((id \
         5ab04bf0-417c-47bf-9338-57310e839315)(content(Whitespace\"\\n\"))))(Secondary((id \
         81105bc3-adef-4723-88e5-2bc51cd9fe79)(content(Comment\"# TODO: Add \
         PaintCol case here #\"))))(Secondary((id \
         65484837-da06-49a6-a92e-8abd87b57a1f)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         c13b39f3-18b6-470b-8768-52d24df2bd1c)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         1b0d9375-4116-4a84-b92f-3c20f16a4061)(content(Whitespace\"\\n\"))))(Secondary((id \
         be771bbb-0837-4d19-a52e-98edddcf1ba8)(content(Whitespace\"\\n\"))))(Tile((id \
         159124fe-024d-4dde-ba0b-e0418e191087)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         8d93a54e-cb49-4fd8-bbd6-d618cada4da4)(content(Whitespace\" \
         \"))))(Tile((id \
         eb8effc6-9932-49a0-a4d8-138d5fe97d40)(label(do))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         1ab4267c-ca9d-41d2-a595-788caaa057c2)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 24))(sort Pat))((shape(Concave \
         24))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         a50a281f-7b60-4281-ad28-e881771920f7)(content(Whitespace\" \
         \"))))(Tile((id \
         469d4e54-b0fe-4a0c-9a8d-1b695b29d184)(label(\"(\"\")\"))(mold((out \
         Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0 1))(children(((Tile((id \
         81fa6f29-0f2e-48d7-9992-4ede35a336ea)(label(Model))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         555dd3e2-8a54-45a4-bee7-4b473e0909f2)(label(,))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 44))(sort Typ))((shape(Concave \
         44))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         d950d5f4-cd31-4edf-b51d-d1bd57e80b30)(content(Whitespace\" \
         \"))))(Tile((id 3bf13a9b-7f0d-471b-ae23-b12614654df2)(label([ \
         ]))(mold((out Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape \
         Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
         aa0a5d89-969e-46f7-8185-5fabb8ecdf4e)(label(Action))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))))))))))))(Secondary((id \
         92438494-9be8-437c-8e7c-686c28ba9c1c)(content(Whitespace\" \
         \"))))(Tile((id \
         6fdf5139-45b0-41f0-a211-58a647cc09d6)(label(->))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 13))(sort Typ))((shape(Concave \
         13))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         63a12e79-3556-4325-beeb-b70672441772)(content(Whitespace\" \
         \"))))(Tile((id \
         2e2db4da-246c-4d59-a266-92120f8c0e0b)(label(Model))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         83cae902-5b8d-4b75-b9ea-5d24f88e16b6)(content(Whitespace\" \
         \")))))((Secondary((id \
         777c5864-194d-4cb8-b1c1-584beadb668f)(content(Whitespace\"\\n\"))))(Tile((id \
         e4d1e79d-8d56-443a-b62c-9d25608a41d3)(label(fun ->))(mold((out \
         Exp)(in_(Pat))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         37))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         f822f5c4-27e7-459a-8fe4-da0e4c7d4ad9)(content(Whitespace\" \
         \"))))(Tile((id \
         daf3f8d0-3897-45f8-8145-5df10d600602)(label(\"(\"\")\"))(mold((out \
         Pat)(in_(Pat))(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0 1))(children(((Tile((id \
         482b31d1-8fe1-4988-9612-66d65674b1a4)(label(init))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         5752ff4d-53a4-4583-98f0-4ab3f84ae391)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 24))(sort Pat))((shape(Concave \
         24))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         6a36f145-244b-41cb-95fa-c12de32f4fb5)(content(Whitespace\" \
         \"))))(Tile((id \
         ec5ce94f-3dd1-4f24-834b-50cfbe787883)(label(Model))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         dc1f8b01-9c7c-4ce0-9616-02748e763ba2)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 44))(sort Pat))((shape(Concave \
         44))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         24f9163d-9901-49e1-a839-5e5d891c2ed1)(content(Whitespace\" \
         \"))))(Tile((id \
         ab81cb84-f387-4046-94a7-c95faf79a7c3)(label(actions))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         10c73f10-e7d7-46e5-81d1-675e1f0f6849)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 24))(sort Pat))((shape(Concave \
         24))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         cbe8a9b8-7d8e-4824-b818-357da3a02705)(content(Whitespace\" \
         \"))))(Tile((id af276c58-2192-44bc-9754-6bb40eec4662)(label([ \
         ]))(mold((out Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape \
         Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
         b6a8a8dc-69cc-4cbc-9455-261e655aaaa2)(label(Action))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))))))))))))(Secondary((id \
         f1ca345b-fad2-43e0-bd41-7bff59241c04)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         25e1c491-73d7-4727-9fb3-974ea54fb7ea)(content(Whitespace\"\\n\"))))(Tile((id \
         73caef3c-8c0c-4d8a-a134-02963c17011a)(label(fold_left))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         99851810-100b-419d-a615-b1d8a27d4dc9)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         2247eb4c-f2bf-45ee-82a5-e9df5c87c2b8)(label(actions))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         70342556-0511-44ad-a962-fcd77e3170c2)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         a327da28-0322-4494-a2a6-350ed1fdb72e)(content(Whitespace\" \
         \"))))(Tile((id \
         ed9107b8-7654-4b3f-a87f-9f872b38f1b2)(label(update))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         ca815df2-f54f-4008-a55f-dc4b577760ec)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         8272f61c-ddfa-405f-857f-cd271bbe0688)(content(Whitespace\" \
         \"))))(Tile((id \
         e2bfca01-0fb2-49bd-ab8e-88c89bcc5bd6)(label(init))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         bd4a3172-35cc-4e67-8455-5666ea162bd4)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         d4734220-3c0f-49da-9583-80da0c7e751f)(content(Whitespace\"\\n\"))))(Secondary((id \
         8f93891d-ecfa-49e4-9d86-eece0444d2e4)(content(Whitespace\"\\n\"))))(Secondary((id \
         20e2ed9a-3e83-4f62-8902-ceae65eb0db0)(content(Comment\"# Existing \
         tests #\"))))(Secondary((id \
         5c0ab948-57a4-406d-b50a-16f75f7c0494)(content(Whitespace\"\\n\"))))(Tile((id \
         1d5e607e-c013-41d5-843b-67423d3d5930)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         bec873f4-5aa7-4927-9c85-44c3d623e8ae)(content(Whitespace\"\\n\"))))(Tile((id \
         232d9e5d-4bb0-4d49-b9de-96753ea7c637)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         d798814d-ecce-4046-bb72-c2938442d223)(content(Whitespace\" \
         \"))))(Tile((id \
         bfaf725d-5ab2-4e3d-bdc1-6ca321cf8378)(label(m))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         fc613f9e-5455-40bf-b1b4-b0f76c074365)(content(Whitespace\" \
         \")))))((Secondary((id \
         7e2e8953-f740-4526-a10b-cba2c107f5fe)(content(Whitespace\" \
         \"))))(Tile((id \
         e1eb9270-c90b-4bdf-abf2-b4b0ffaf5bf5)(label(update))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         5c609a31-89bf-4721-8692-6379a5190ebb)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         6c7a6f96-d088-407d-861b-a841a71b6972)(label(init))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         a6bc4737-6e1d-4f22-ba2f-31a8ed6863a4)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         5bc3a9a1-bda9-41c3-8f47-f87e202b7af4)(content(Whitespace\" \
         \"))))(Tile((id \
         ae95017e-cd93-4634-aea9-488a85443263)(label(PaintRow))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         7061f2b2-d279-4223-b1f1-8b7323902059)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         8ea74d09-475c-4b0a-a431-3c3a4a5f80dc)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))))))))))))(Secondary((id \
         6794424c-4f7f-47e5-bc26-2bf9132ae71d)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         e2002a05-9648-42fd-91e8-8533f216ddc5)(content(Whitespace\"\\n\"))))(Tile((id \
         7d7bc87f-995d-4968-bbc2-0ba40af287b2)(label(m))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         e27e1a25-5488-4351-9507-23dd5e6f2356)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         b0b1249d-fb22-4b87-b120-eeb40f3aff5c)(label(canvas))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         a287ebef-aa65-47ab-a524-6b1a73348a90)(content(Whitespace\" \
         \"))))(Tile((id \
         550473fc-ed69-48a6-8a61-171aea72ca08)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         e061970e-ac77-4deb-a6c3-58caeaf4c267)(content(Whitespace\" \
         \"))))(Tile((id ebe9db07-80a5-47f4-8118-5b17d90cc68c)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         40ffcd7e-6780-49ac-8ad9-d74cef659be8)(label([ ]))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         48328085-0795-45e1-a8c3-4bde319549f8)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         59f50cf3-9b96-4978-a15b-d301920997fd)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         9e61ec37-6231-4878-b568-77fc28934cbb)(content(Whitespace\" \
         \"))))(Tile((id \
         3ddb698e-2cba-4a86-9a51-8270dc102043)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         307e84aa-1fc5-486c-87c5-4e8aed04adf1)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         c67d9b4b-d05d-4be3-8055-d8374e4a94a1)(content(Whitespace\" \
         \"))))(Tile((id \
         7df1c88f-761a-4a58-b854-45fc1593adf8)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         395283d9-5d4e-45e0-bced-59c799118670)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         c857168f-bd79-4e03-afb3-beb4a121444e)(content(Whitespace\" \
         \"))))(Tile((id a5987e93-8633-470d-b873-eb1c6b363f92)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         6a1fb247-a32c-4210-8113-9a38814cd9ff)(label(\"\\\"\\240\\159\\142\\168\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         54474125-ebcc-4e3d-9425-d01a52dadb7e)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         ee09a9e1-6f8e-4f63-9df2-1635f7507c54)(content(Whitespace\" \
         \"))))(Tile((id \
         210b66a7-58dc-4ea5-8d30-bb6a296b2a5e)(label(\"\\\"\\240\\159\\142\\168\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         051a54a5-c8b7-453d-bc0e-bd34bf4fa306)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         249d08b6-0b0c-4659-a3fc-7f4bd3a1880b)(content(Whitespace\" \
         \"))))(Tile((id \
         e032e85e-e491-490d-8821-c4bb6f7eb72b)(label(\"\\\"\\240\\159\\142\\168\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         10564b92-8044-42ce-bfbf-8b7abfb01db8)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         02b1d30d-ce7a-48a2-a6c9-3c874a91a564)(content(Whitespace\" \
         \"))))(Tile((id 9cc7e4cb-4022-499b-96c2-2acd1140fe11)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         72279323-f697-45f6-8105-525084e914b7)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         bc54e382-0ad2-48a5-adce-5e8c47f04e70)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         37aa7d3a-b77a-4fd3-84a6-6d464b721377)(content(Whitespace\" \
         \"))))(Tile((id \
         f70d9854-aae3-492a-9139-7f44e876a891)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         91e75ec8-dc44-4c5a-9b77-99e1520d91cd)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         c56f7ced-53a5-4cd7-986e-1943b120e1c1)(content(Whitespace\" \
         \"))))(Tile((id \
         aad9b2d0-8d0c-4b80-ae37-2eba36f9cf3e)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))))))))))))(Secondary((id \
         0806eb86-203d-49a3-a012-d9ef4367b9c2)(content(Whitespace\"\\n\")))))))))(Tile((id \
         6c353a24-9c7a-4e7f-af0e-9089bfc4e2f1)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         252ec1db-1d26-4913-b250-3a9db0b5215f)(content(Whitespace\"\\n\"))))(Secondary((id \
         aa14dbc8-f2dd-4495-93a3-dd6ee9f36391)(content(Whitespace\"\\n\"))))(Secondary((id \
         9ebd34fb-ac84-4554-958f-34294fddd118)(content(Comment\"# New tests \
         for PaintCol #\"))))(Secondary((id \
         a26f34a9-84e3-463b-a07e-a570e7df4dc0)(content(Whitespace\"\\n\"))))(Tile((id \
         3fe46a65-3693-4df8-a821-cea89a1f813b)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         66910853-3292-4b75-9933-00456877ebe8)(content(Whitespace\"\\n\"))))(Tile((id \
         8ea11de4-7c80-44d1-a716-730458720a61)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         8b6c9cae-2bd3-4d6c-9791-c263039b59b6)(content(Whitespace\" \
         \"))))(Tile((id \
         70f86093-f8bc-461d-ac26-39c9b6673ae1)(label(m))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         763ae060-835b-4cac-aecc-4cbc02e3af2d)(content(Whitespace\" \
         \")))))((Secondary((id \
         b1c8f49b-0c82-447a-a847-b338c516067f)(content(Whitespace\" \
         \"))))(Tile((id \
         e35266f7-57bb-4cc8-9146-9c31439993d4)(label(update))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         c66c558c-91f0-4c37-9c85-c7cfd9dbb59f)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         e5099499-42a1-427e-a03c-9247071f602b)(label(init))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         e3f019ef-6f96-41b0-9422-356cc353424e)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         3434b041-57aa-4639-9009-ff544ecb8bcd)(content(Whitespace\" \
         \"))))(Tile((id \
         db89a6e2-1f44-4552-880d-7a0666a9ef5a)(label(PaintCol))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         799d62df-7381-42bc-8633-86f986c0e081)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         efb95055-12e1-4f7f-a906-08c2b4fc1d0b)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))))))))))))(Secondary((id \
         09082cfd-bf79-495b-86cd-2fb28fe16623)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         a8ce9142-6b82-4d12-93f3-f3e7da57bb0b)(content(Whitespace\"\\n\"))))(Tile((id \
         6f13c5d5-9e97-457c-b56c-bb6e5614bf4f)(label(m))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         059b08d8-8c66-4dbb-b254-256095a4ee6e)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         a61871ba-877f-47c8-ba03-4b9dacd5289c)(label(canvas))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         fe51e2c0-320d-4cc3-be96-113ea0c4041d)(content(Whitespace\" \
         \"))))(Tile((id \
         606b6e67-df2e-41e5-bdaa-cadfe51a1e89)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         1e5b731e-61ee-4ebd-a16a-617904811492)(content(Whitespace\" \
         \"))))(Tile((id 2d890ca8-7cf1-4f51-995d-2234023a3abb)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         d82c8629-f8d7-4de0-9274-7ace02450281)(label([ ]))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         b2681d9d-44b9-4de4-a3da-233634ff11a3)(label(\"\\\"\\240\\159\\142\\168\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         a186082d-bac7-4595-9024-f8a5f286f664)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         df8e7f4f-c538-42bc-b333-f984da2be06f)(content(Whitespace\" \
         \"))))(Tile((id \
         6bf080b5-ce96-4a29-bf89-be43fb86d380)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         367d8d42-3c0e-45b0-9ea4-57951741751d)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         854f6b33-5c04-4ed8-9db0-14782dad19f5)(content(Whitespace\" \
         \"))))(Tile((id \
         c4feae1c-d1a8-4182-8f4d-5defc7d3d3d5)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         77ef9ce4-ae3b-4bfd-8407-894c5bfb1ab4)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         360c90cd-6fde-473d-b65a-e14590054fcb)(content(Whitespace\" \
         \"))))(Tile((id 198ae2d9-cc0e-49ed-8167-6daa0b4d7e25)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         86f23228-6fa6-4295-9792-0f76b312b912)(label(\"\\\"\\240\\159\\142\\168\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         f8070487-efc6-4dca-a290-6192168d9067)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         33c6ccb2-75b4-4f32-8363-d61d78a1cc81)(content(Whitespace\" \
         \"))))(Tile((id \
         fa65c2c2-9619-4b1e-97c4-4c52a2483805)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         f797515b-06ee-4e41-a2ca-ae86114130b1)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         01c5f289-9f6a-4206-89e5-de0443babc9b)(content(Whitespace\" \
         \"))))(Tile((id \
         423316eb-c8ed-4428-8d97-7a2bed3136f4)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         7a00bdd4-d920-440b-98e8-fbad72e744c3)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         e2721e08-c3d8-49ed-8507-abdd91e26f14)(content(Whitespace\" \
         \"))))(Tile((id ba76a518-b1e5-4fff-b095-bcd66617415f)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         62aaf064-0ad2-473a-9cae-ef5851469a23)(label(\"\\\"\\240\\159\\142\\168\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         f6344d11-5432-45b0-b0ba-3c4428298989)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         a98eafb7-b8e3-4a3f-a67c-4cb868f51344)(content(Whitespace\" \
         \"))))(Tile((id \
         df9b1225-9fdb-4fbd-bc5a-be3fb5b03dae)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         bcf0311b-0c43-4fd3-bf3f-f04869ece5e8)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         4e167f0c-2cb8-4651-ae39-2ad7602f6caf)(content(Whitespace\" \
         \"))))(Tile((id \
         f6dbed2a-05d5-4a7a-8bd5-c26c4eb3f63d)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))))))))))))(Secondary((id \
         f906e6e3-0c6a-45b1-8bb7-c852b01df4dc)(content(Whitespace\"\\n\")))))))))(Tile((id \
         2b25b31e-58c1-4c79-8205-10c7ea39ffad)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         833b8f98-25f5-4677-a325-2acf1ca5e259)(content(Whitespace\"\\n\"))))(Secondary((id \
         ceb19d52-8284-434a-870c-42f33b40e84d)(content(Whitespace\"\\n\"))))(Tile((id \
         3e8098ec-ec12-4b5f-9070-f65e971e43f7)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         96dcabe0-862b-4392-89e5-e6bd16e46e17)(content(Whitespace\"\\n\"))))(Tile((id \
         cccb0a2d-5aea-4ad4-a2dc-cd1430e8e70b)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         0903f7df-b167-4160-b1af-e0b8dc883e3c)(content(Whitespace\" \
         \"))))(Tile((id \
         001df03a-547d-4dbb-ace4-f928f6843e54)(label(m))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         7021b130-13ec-423b-9a53-5c4185376d12)(content(Whitespace\" \
         \")))))((Secondary((id \
         b8ffd6b9-23a6-44e0-8c26-285183c696d6)(content(Whitespace\" \
         \"))))(Tile((id \
         ff4c8716-6a18-4217-9795-d3a8b52fc1bf)(label(update))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         839ab873-cd6e-4ca8-873c-129ad2baa691)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         553a2c51-453f-4509-870d-43d8dc2645b0)(label(init))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         ba534b87-76f2-4f85-8464-915791eb9665)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         7b234d1e-c4e1-43d3-b834-cb21e488c0c2)(content(Whitespace\" \
         \"))))(Tile((id \
         f77bfb26-c69f-4cea-b9e6-c4fe12744322)(label(PaintCol))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         8c17c696-6fbf-4db9-8df6-74cd07195d3c)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         5e56cec0-cbd5-479e-86c6-28a0cbd53d6f)(label(2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))))))))))))(Secondary((id \
         c81cd55c-12a8-4a2c-9e4a-2e4fd8042fe6)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         fc230363-6ba6-429c-9b69-719a4842765d)(content(Whitespace\"\\n\"))))(Tile((id \
         9207724e-00ab-405f-83dd-e98bdf7cc673)(label(m))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         b3575580-7713-4e21-9815-abc940eb95ef)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         536f887d-e355-4e1c-bab9-3e7978361df4)(label(canvas))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         fbb6d2cd-55df-4010-96af-e802daa33d21)(content(Whitespace\" \
         \"))))(Tile((id \
         694eb066-6b5e-4219-a434-83fb29713e9b)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         962e052c-b020-46a7-ae3b-bf8fbd66f640)(content(Whitespace\" \
         \"))))(Tile((id 11473390-7d4b-4a7e-8ff6-dc844c8c80a6)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         9abfd8c6-c1d5-4182-933e-a4f8893a1f8e)(label([ ]))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         4d49c372-0540-4ccd-8afc-c6784dbe1a89)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         05a6348b-472b-42fd-acaf-6bcb3eb9dd28)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         1f9815aa-60b9-42b4-8eb9-9c5c9a458c5e)(content(Whitespace\" \
         \"))))(Tile((id \
         6320b07a-919b-4667-b186-471bbadd0fff)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         3330f679-4e77-483c-9090-1207d07ce7c6)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         a053d07b-05cf-4e21-a521-640c49193f2d)(content(Whitespace\" \
         \"))))(Tile((id \
         22bc8560-c276-4731-ba21-b97f36ef2ed4)(label(\"\\\"\\240\\159\\142\\168\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         5c3ad453-5bf0-4e9d-8093-763d18602293)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         ea3b657d-cf3e-453b-b3e7-125c196eb194)(content(Whitespace\" \
         \"))))(Tile((id 12327821-8bf6-415c-b167-50a57af104dc)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         14006e8c-79ca-49b6-b804-7c61a4886d17)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         b4e5f9c1-184b-4a82-983c-2cc2883dcbd8)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         69d40e9f-749c-4647-b1f7-957a62434335)(content(Whitespace\" \
         \"))))(Tile((id \
         de9d180b-8b06-4e3c-9a45-d363c6218613)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         0ed48adf-1110-4192-aa95-b3357324e25f)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         375e3486-1959-4a0a-93d1-1402fbb095ef)(content(Whitespace\" \
         \"))))(Tile((id \
         60f1458d-aaf3-42c0-a16c-2a8aa14537ba)(label(\"\\\"\\240\\159\\142\\168\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         7cb3afa0-9b81-4f48-a83f-dcd2f25c50bd)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         d457ce34-c44d-44b1-b4cd-b46b6331d6d6)(content(Whitespace\" \
         \"))))(Tile((id 775ad1f9-803e-4e88-a9b6-ed9f2767bd4b)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         33649c6e-981d-477d-9004-dba68c1d2f29)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         52e228a2-172c-4c0f-9597-0880c85a294e)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         2134efd5-2b2d-449d-9074-ddb67ecc2b43)(content(Whitespace\" \
         \"))))(Tile((id \
         20c21332-33dd-4c9c-b19f-657ac4edec2a)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         6c9627f8-e3df-4417-b9ad-11619a270722)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         f3511c62-8557-4600-81d6-5ad90517b5e1)(content(Whitespace\" \
         \"))))(Tile((id \
         a7b9d866-d74b-41f1-888f-a764d3f09b8a)(label(\"\\\"\\240\\159\\142\\168\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))))))))))))(Secondary((id \
         d558018e-f8fc-4e50-bc98-8ae1fb21bef5)(content(Whitespace\"\\n\")))))))))(Tile((id \
         829dce2e-2073-40e1-8e81-7f901f4cd28b)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         4da16f22-2c01-4fd5-9304-2a2e6919bb0b)(content(Whitespace\"\\n\"))))(Secondary((id \
         bab252bc-1212-4402-9acf-bde74542196f)(content(Whitespace\"\\n\"))))(Tile((id \
         f12babce-7c49-4a28-b34f-db355994751e)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         b23439a2-a389-412e-a668-d62bba251dda)(content(Whitespace\"\\n\"))))(Tile((id \
         a13d1e26-c865-415e-a8b6-ca3ba730f05d)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         1f227910-2b7e-40e6-9305-536b5045681c)(content(Whitespace\" \
         \"))))(Tile((id \
         1aa8949c-d1b8-42e2-a22a-21f515f1de4f)(label(m))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         8059d34f-18d0-4831-880b-b1be6fd85145)(content(Whitespace\" \
         \")))))((Secondary((id \
         50df5f17-0bd4-4536-afe6-0a7d3333f66b)(content(Whitespace\" \
         \"))))(Tile((id \
         f9a96a08-57f2-4d98-ae29-1feb13ecfe93)(label(do))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         36d08377-974e-4698-bb3a-ee2c3922e153)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         41b036e2-9671-4549-b741-3cb48b6c99cd)(label(init))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         c4fe4dae-67ca-4398-9b01-a15349acbefd)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         915efd21-f02f-4fe0-9597-4ff1259a6df4)(content(Whitespace\" \
         \"))))(Tile((id 1818347a-178f-4c34-a730-7a8279bfae39)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         035dcfeb-b699-4124-8e0a-cd0d38b76338)(label(PaintRow))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         7c4a1990-0a67-4e45-90e8-b7714804d3f5)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         a6b773ad-26d4-4453-95e4-6b0b465c54c9)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         2eae8b3f-39fa-47bb-8903-c7438345c973)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         7245c9d7-381f-46f9-b160-7f8b9589a294)(content(Whitespace\" \
         \"))))(Tile((id \
         9a3e505e-f252-4af6-8630-3848e826ca07)(label(PaintCol))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         d7b144be-0b07-4fb5-905a-5b41b4abe67f)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         95766c4e-e55e-4fc5-8bc0-d8e0a9af2211)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))))))))))))(Secondary((id \
         ab35f516-f226-4eb6-bd68-1d1966d7b076)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         eb36fcac-9b6d-425a-b1a2-5c4114d9bc4a)(content(Whitespace\"\\n\"))))(Tile((id \
         3c7c778f-59a5-4ec6-924b-83129bf218fd)(label(m))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         ed620e05-5a88-4055-94c8-8b9e74e55164)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         87d48e1f-d793-448a-830a-a42b9d9fb8d0)(label(canvas))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         e13bcc47-0bcb-4b70-b800-5f06e0cb71df)(content(Whitespace\" \
         \"))))(Tile((id \
         3f85818d-9a2a-459d-815a-889dea42caf4)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         d7d68946-0d40-4793-9ee2-dd80d3db08bb)(content(Whitespace\" \
         \"))))(Tile((id 349e9303-1564-4b9e-a1a8-36d899cd3472)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         1364a608-c754-428c-a5d3-a20942c04bb1)(label([ ]))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         053be038-625d-4469-92e9-a715004c25c4)(label(\"\\\"\\240\\159\\142\\168\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         28187d71-07cf-49e2-9e99-c0e590673ac5)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         b33fe753-1513-4ac2-9cc9-952f36890e5d)(content(Whitespace\" \
         \"))))(Tile((id \
         de790bab-ea40-4d1c-b6b0-e64d98ae093a)(label(\"\\\"\\240\\159\\142\\168\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         6b335d23-f4c0-4de7-83c1-9ce44876c302)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         95a3e378-1889-4559-9411-01f52451181f)(content(Whitespace\" \
         \"))))(Tile((id \
         e650a304-884a-44cc-a7dc-6274fa79d73b)(label(\"\\\"\\240\\159\\142\\168\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         c4cd7a88-e85b-4c69-aa03-f7dcdfb572d3)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         92bb672e-e5c5-446c-b6f4-9a48a3dc7e8f)(content(Whitespace\" \
         \"))))(Tile((id 121b01d3-1745-4647-a5e8-df2683af96b5)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         8cfb047e-3cfa-4c25-9633-1527783313b5)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         dfee0c61-aa09-407d-ba8e-3a2ac7755089)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         d7cb0792-c4da-4611-87c2-b5d22f8f8943)(content(Whitespace\" \
         \"))))(Tile((id \
         6c23a0bc-2247-460a-87c5-8dc8a5d776ca)(label(\"\\\"\\240\\159\\142\\168\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         6b9d1982-cef9-4895-a875-227de3c8e578)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         17dbe0fc-9817-4231-bdc0-e5a1514ff151)(content(Whitespace\" \
         \"))))(Tile((id \
         3f711af7-a7b3-4d49-bfd5-2c3fdf312f75)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         c206ffbc-fef5-4a9c-86f9-35cffbb3f91d)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         562b41f8-5e78-4301-8a3b-717dd750aeeb)(content(Whitespace\" \
         \"))))(Tile((id a5ebf654-ab3d-463f-87ec-9af408c0ed4b)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         78a0c88d-c83b-4dda-b1d3-2ed7d115e011)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         b57d9d2d-aa4d-4bac-82c6-918f9c0a9b19)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         13b159ba-b30b-4d04-b5e0-714e6edc4f3a)(content(Whitespace\" \
         \"))))(Tile((id \
         5785532a-fc0e-40c2-ada8-14c14d3e317d)(label(\"\\\"\\240\\159\\142\\168\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         e815e682-90f7-4dc3-ab0c-e546d2a4649e)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         ba486e0f-bcce-4330-9d34-f2188ff5ba69)(content(Whitespace\" \
         \"))))(Tile((id \
         c56d70a1-854b-4472-bb48-f4a5308d031b)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))))))))))))(Secondary((id \
         41a6f8c7-2aff-488c-b904-3033061762d8)(content(Whitespace\"\\n\")))))))))(Tile((id \
         fc5b3f09-913d-44ea-b623-910a461226ca)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         eb3e869f-7c23-41f1-bc05-9de8ba811afe)(content(Whitespace\"\\n\"))))(Secondary((id \
         06217735-eb1c-4e5e-a9fc-a6fce5bbd677)(content(Whitespace\"\\n\"))))(Tile((id \
         96c61835-0fa6-4046-bf4c-1db3df3c35a8)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         1f951308-8eb0-49e0-bdcc-c1e28c6b3553)(content(Whitespace\"\\n\"))))(Tile((id \
         273ce864-4a1a-48ba-a1c1-ca93237e269f)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         963cffb5-0635-4bd7-817e-479feee9c8fb)(content(Whitespace\" \
         \"))))(Tile((id \
         270900c3-302d-4d6b-8d16-0a8d9b71b0e8)(label(m))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         12a6a91d-5d91-44d5-a5fc-a5eca3aaa044)(content(Whitespace\" \
         \")))))((Secondary((id \
         5c75e0d4-59c6-4dea-b53a-c7d807f910a7)(content(Whitespace\" \
         \"))))(Tile((id \
         8269c3fe-83dc-4479-8215-295f669e43d7)(label(do))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         9697cbd3-40b9-408b-b6bf-15e6c2bb2eae)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         4134935d-a7cc-449b-afee-2ced2d3996d7)(label(init))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         08aef528-1810-448b-b819-36bbd0c9025b)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         9d4521b4-234e-4d8f-bb55-a826edbcd8d9)(content(Whitespace\" \
         \"))))(Tile((id a0ff3a74-0b82-4a41-bdbe-242031e0b30b)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         58f7ff90-7d2b-4646-b020-0ffa2d1ef8f9)(label(SetBrush))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         9f4de489-7841-41c1-aa14-dce7984466f6)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         4428f850-be2f-49d5-a19e-3a6012a5e320)(label(2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         a85e28e1-d961-4848-8326-bad5d5bb4b8b)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         62b2e1d9-8528-4da3-ad68-da4936513cd5)(content(Whitespace\" \
         \"))))(Tile((id \
         d1613fda-1866-410d-8379-01e5ab3722db)(label(PaintCol))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         3f311379-b526-499f-b3da-d9bc00719d2d)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         3e85735b-3cae-4db4-83b7-2462dd86b1f5)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))))))))))))(Secondary((id \
         844a78b0-1bbf-4f38-b8ca-d8fcce1fa7c6)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         e4662e55-9db3-47df-8fcf-a3d25300da74)(content(Whitespace\"\\n\"))))(Tile((id \
         ca0b2287-16af-49d2-86a8-6459e832d98e)(label(m))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         cf187d53-aa30-44af-bbf6-f0951793b42c)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         b8e6a013-a070-41fb-af35-921f8a6ede8c)(label(canvas))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         18656bd9-7936-4e11-a8f7-f884199c61ec)(content(Whitespace\" \
         \"))))(Tile((id \
         bf665e30-6a00-433f-ab1c-efb451aeaa60)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         0ef29bda-6971-49f4-8945-773e6130e037)(content(Whitespace\" \
         \"))))(Tile((id 35937424-fb14-4d37-aec3-b7a900181247)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         36222dca-311d-491b-856a-bb3871ab569a)(label([ ]))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         02cf7884-145e-4588-93ee-b3f5ef134c6d)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         c1668905-9224-49a3-944e-edfd4915f9c5)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         a1e53ded-7977-4006-a912-74bbf258dae0)(content(Whitespace\" \
         \"))))(Tile((id \
         94e2bcfa-268a-401b-9b58-4f6f82d42de8)(label(\"\\\"\\240\\159\\146\\156\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         f6df2141-8bcf-43ea-aa1a-6bff15786a6d)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         0abcb745-16e5-4088-a9df-2db8b9c8cb36)(content(Whitespace\" \
         \"))))(Tile((id \
         3e6094b5-4662-4f7a-930f-a0b14700c598)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         58ea5ffe-c0a9-4ace-b28d-bd4d83ccbd99)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         bf291df6-26f9-4492-97ec-3febad822894)(content(Whitespace\" \
         \"))))(Tile((id d486e03c-0005-425e-af96-7bc68ec1723b)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         59aa4c8e-ee26-4347-b055-1d0c35c45b74)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         e682a2f2-21d9-4167-bd68-43ca4795fbac)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         8ffdb04d-36b4-465b-9da0-2d25c0d9dc47)(content(Whitespace\" \
         \"))))(Tile((id \
         c4e7874c-3c85-4cf7-a747-15756ac8a0ce)(label(\"\\\"\\240\\159\\146\\156\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         42d6791c-e7d5-4465-87a4-a7f9d18d2a01)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         4a5f0f4e-cf61-4253-8ac7-598fcd528703)(content(Whitespace\" \
         \"))))(Tile((id \
         55bc53d6-a516-4d9c-b478-667aca347bc0)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         5a0cf0bf-97c3-4815-a7e8-ee1c89c4b8b4)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         1badac95-009f-450b-ba7e-925035cd7656)(content(Whitespace\" \
         \"))))(Tile((id 014829c1-7417-4c52-afcb-28e41e88df91)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         4d981a33-08be-41e4-b193-8de0862e4278)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         541ccc4b-b626-4326-a7e6-3367b46d2161)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         cc9f2f7b-ae20-468b-b5d4-1afaa945b9e7)(content(Whitespace\" \
         \"))))(Tile((id \
         7e6322e6-99dd-4700-9cbd-23353f69e7ac)(label(\"\\\"\\240\\159\\146\\156\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         b529051f-de96-41dd-8847-9f3383f78299)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         485d0425-8514-44ae-958d-b9e7a9a46c10)(content(Whitespace\" \
         \"))))(Tile((id \
         3efd77e6-77fd-4afe-be0b-26f60da85435)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))))))))))))(Secondary((id \
         80747e05-ca11-43e0-96ac-4ec310bbe9af)(content(Whitespace\"\\n\"))))(Tile((id \
         1f5dba89-441d-4a6b-9018-74a59dc37be9)(label(&&))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 32))(sort Exp))((shape(Concave \
         32))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         eee8d1ba-ea32-42b0-8b54-1219e97f62b2)(content(Whitespace\" \
         \"))))(Tile((id \
         a79911ba-4b99-4610-b3a6-b6d23fd50f65)(label(m))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         85fd2830-1bf2-46ff-b50a-251771619970)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         f0c18c1e-d22e-494a-92e9-13e1e1b054f2)(label(brush))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         45b2d74c-7d49-4dae-8c7f-55ce8b772c0c)(content(Whitespace\" \
         \"))))(Tile((id \
         644bf4ce-4394-479c-86e1-596fa8d525c3)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         20183da8-d1a7-4b19-8e99-154f2604c10d)(content(Whitespace\" \
         \"))))(Tile((id \
         395f3c10-1df7-4eaf-afb8-025fe25a0ba0)(label(\"\\\"\\240\\159\\146\\156\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         82c23e31-7384-4c78-a2f6-56111d481e48)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         8c686b8b-6623-492a-a88a-1281c53db367)(content(Whitespace\"\\n\")))))";
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
