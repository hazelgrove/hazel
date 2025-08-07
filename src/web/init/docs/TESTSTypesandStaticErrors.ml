let out : string * Haz3lcore.PersistentZipper.t =
  ( "[TESTS] Types and Static Errors",
    {
      zipper =
        "((selection((focus Left)(content())(mode \
         Normal)))(relatives((siblings(()((Secondary((id \
         429b4d2a-51ee-469c-b1c0-a43fc60b2cf7)(content(Comment\"# Internal \
         Regression Tests: Type errors #\"))))(Secondary((id \
         779d13dc-c053-4e0f-80df-4079220a4118)(content(Whitespace\"\\n\"))))(Secondary((id \
         2869d0ea-d30b-40ce-9e7b-7333ae08e43d)(content(Comment\"# Each line \
         should show errors or not as indicated #\"))))(Secondary((id \
         59b56fec-1737-4306-9fbd-d0ff7a649850)(content(Whitespace\"\\n\"))))(Secondary((id \
         db4745db-a4f2-40c4-88e4-0b297b0b1174)(content(Whitespace\"\\n\"))))(Tile((id \
         11beb2f9-cde0-463d-961a-df3484803d63)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         40))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         79fe4b8e-9bd3-4cee-a99a-fda82a05f268)(content(Whitespace\" \
         \"))))(Tile((id \
         a09bb7f2-9794-4494-b7a6-e9886e20e1e6)(label(_))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         7c6a18b9-1045-4661-9703-ba4f713f6f45)(content(Whitespace\" \
         \")))))((Secondary((id \
         0ada9a43-7f56-4a8f-8cf0-97f173ea9130)(content(Whitespace\" \
         \"))))(Tile((id \
         71dd0ad6-a2e2-4f46-8630-55a0c17737f0)(label(unbound))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         d0c9294f-cd96-4044-a3fa-a79fb57d1a9f)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         8a5d63fa-4b68-4878-b0c4-7a1b27dd6bff)(content(Whitespace\" \
         \"))))(Secondary((id \
         40ac0ab0-c5b1-4c7b-949f-6f7ce15cf4cb)(content(Comment \
         #err#))))(Secondary((id \
         704e11d3-fe54-4ca1-bc96-151679f985b1)(content(Whitespace\"\\n\"))))(Tile((id \
         a559497b-e731-468b-a4c9-5c278d987750)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         40))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         1b928765-17c6-4dda-ad1a-f584430d94f0)(content(Whitespace\" \
         \"))))(Tile((id \
         d316c285-9921-4cf1-a847-424c125c3257)(label(Undefined))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         b0fda6d9-3d24-4ce0-a15b-6e44bd95b479)(content(Whitespace\" \
         \")))))((Secondary((id \
         04256bfe-318e-4117-bdf1-67eb01c56eca)(content(Whitespace\" \
         \"))))(Tile((id \
         879d8a30-b714-4ee2-b262-a60f8175c116)(label(Undefined))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         e3b337a6-aeb7-4198-a67e-9b731bd20dd7)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         c305c98b-e8a2-4e41-9ddc-0bb73e4a4255)(content(Whitespace\" \
         \"))))(Secondary((id \
         a8eb9ac2-db51-4bbf-b30a-987ae3d7610f)(content(Comment\"# 2x \
         err#\"))))(Secondary((id \
         bf01440b-7f9d-4e6b-b609-e22b440caf24)(content(Whitespace\" \
         \"))))(Secondary((id \
         68ab04da-2aff-4b6d-95e8-b31830a36a16)(content(Whitespace\"\\n\"))))(Tile((id \
         2efcdfe6-ed6c-4c86-8c07-ad4df655a529)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         40))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         ae7b922e-4b18-4e8b-88ed-ad2d342236da)(content(Whitespace\" \
         \"))))(Tile((id \
         7562b86c-6669-48ab-a910-381c48705a9c)(label(true))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         1f2548f7-9246-412a-9c6d-8c4f7a52e625)(content(Whitespace\" \
         \")))))((Secondary((id \
         7144251b-5cf2-40ca-9811-7a18f2b4a805)(content(Whitespace\" \
         \"))))(Tile((id \
         681f7b54-f492-4b6f-aa8a-8ca05c108fa7)(label(2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         31e01710-10e9-4f8b-bcaa-20e5ccafda1b)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         1539111c-e4d4-47e4-a7ec-4702de34f414)(content(Whitespace\" \
         \"))))(Secondary((id \
         726e3a76-0419-4b0c-a2e4-65bf86a3a954)(content(Comment\"#2x \
         err#\"))))(Secondary((id \
         558d5a23-15b4-48ea-a59a-65d33fb25efb)(content(Whitespace\" \
         \"))))(Secondary((id \
         6e70e883-fc73-460b-89ab-9f0dcd9c5052)(content(Whitespace\"\\n\"))))(Secondary((id \
         f478e0dc-1ae6-4d3e-b5c4-3d55f1283a85)(content(Whitespace\"\\n\"))))(Tile((id \
         9058466d-46dd-48e5-8c30-9f3e1f01b4fa)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         40))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         c971916b-a1a9-4374-80fe-f89c0fee210d)(content(Whitespace\" \
         \"))))(Tile((id \
         909721cf-b3e6-4002-af58-4b52adc3a8db)(label(?))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         2d3066be-cfc1-444d-bfc0-b071318ebefe)(content(Whitespace\" \
         \")))))((Secondary((id \
         aaf9b128-1437-459b-8cf2-b9658014615f)(content(Whitespace\" \
         \"))))(Tile((id 9a68e858-d066-4f60-b65d-dc75b7b7499e)(label(if then \
         else))(mold((out Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 35))(sort Exp))))))(shards(0 1 \
         2))(children(((Secondary((id \
         4a272539-d14d-4531-af0e-8907875bbccf)(content(Whitespace\" \
         \"))))(Tile((id \
         5bf46976-fb49-4bf2-baa3-30670a52baa8)(label(true))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         522177e7-9eb0-4fb6-9f3b-12f0d3cd170c)(content(Whitespace\" \
         \")))))((Secondary((id \
         77b56501-fbb8-4aef-adc3-17c63af721ee)(content(Whitespace\" \
         \"))))(Tile((id \
         ef011715-937d-4981-9934-7b78f430f949)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         a49bbfcd-15bf-4463-9c4d-a24870803476)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         784adda0-cb32-4055-8ae0-481854081012)(content(Whitespace\" \
         \"))))(Tile((id \
         bc24b5da-b52f-4112-91a9-1b4eab0f72d3)(label(1.))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         bf7cb0b1-6854-4ce7-86d3-8a8a505fc9d0)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         32bc37be-bd71-446c-a0be-2f12e43c110c)(content(Whitespace\" \
         \"))))(Secondary((id \
         8043e742-6f8a-4564-a11d-4b0403a8aa5d)(content(Comment \
         #err#))))(Secondary((id \
         a010a487-78ce-415a-8be4-37a18d0b4b55)(content(Whitespace\" \
         \"))))(Secondary((id \
         0c304dee-c4da-4002-9ec0-55cd732be52e)(content(Whitespace\"\\n\"))))(Tile((id \
         7bdbd6b0-6ecc-425a-be76-a3f99dd188a2)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         40))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         5c085061-2187-431e-8592-c761ff11077d)(content(Whitespace\" \
         \"))))(Tile((id \
         b4b3803f-1811-4d5b-8397-ee3a12ee1fd4)(label(_))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         ed08c742-d46e-4ca9-9106-9681c45f7fd7)(content(Whitespace\" \
         \")))))((Secondary((id \
         7e0a00be-6f71-4583-8e78-df039c03f4c5)(content(Whitespace\" \
         \"))))(Tile((id 8d0c794f-1f38-47e2-9606-fa29b33809fe)(label(if then \
         else))(mold((out Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 35))(sort Exp))))))(shards(0 1 \
         2))(children(((Secondary((id \
         bab65d9f-4c2c-4447-bcfc-f0b82b8d575a)(content(Whitespace\" \
         \"))))(Tile((id \
         40fd0e01-f0ba-4952-9bef-ae013b105c31)(label(true))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         f54672c9-9d47-4523-a65c-c137111a592b)(content(Whitespace\" \
         \")))))((Secondary((id \
         2a4c6cbc-4a34-4c32-91ec-d25e988d5add)(content(Whitespace\" \
         \"))))(Tile((id \
         b95aa82b-b033-411c-b95b-638698a2c0b9)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         0babdf4f-40ff-4a97-894a-ec50e931fe9e)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         8ae52e04-b38b-4414-a9d3-6b8cd40622b7)(content(Whitespace\" \
         \"))))(Tile((id \
         17855d7a-51dd-4176-886f-589baec2f44a)(label(1.))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         94d6a12d-a20a-44ad-a307-b5387b71e9dd)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         d4aedae1-091a-4b5d-82c8-7b336aa1dfae)(content(Whitespace\" \
         \"))))(Secondary((id \
         0057a470-93cc-4bb9-9e3c-b5879f976603)(content(Comment \
         #err#))))(Secondary((id \
         047997e4-11b8-4df3-bc4b-5eedf3636b2e)(content(Whitespace\"\\n\"))))(Tile((id \
         c4dbbba7-d102-4a25-abc8-5c943b4a8cb2)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         40))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         33f9b8b0-bd65-4d3c-91b3-cabed924f0c9)(content(Whitespace\" \
         \"))))(Tile((id \
         962a560f-c352-4753-a051-6841bda62cb2)(label(_))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         f733b711-203a-4554-84bc-a32a96d90a7d)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 24))(sort Pat))((shape(Concave \
         24))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         f4618840-5396-4f6d-9aa3-19e85a2bb759)(content(Whitespace\" \
         \"))))(Tile((id \
         aa8f1f50-e9f4-4373-8e71-66b08016401f)(label(?))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         b47db4b7-1993-4524-a7d5-20a92f789dc5)(content(Whitespace\" \
         \")))))((Secondary((id \
         a5c46774-7484-4d29-91bc-8afd423c68f7)(content(Whitespace\" \
         \"))))(Tile((id 7e57ba62-f77b-40e9-a780-2e14008d1940)(label(if then \
         else))(mold((out Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 35))(sort Exp))))))(shards(0 1 \
         2))(children(((Secondary((id \
         440f1b0a-7acd-485e-b3ae-66e3375352ba)(content(Whitespace\" \
         \"))))(Tile((id \
         f84a8582-ee49-42cd-b9da-9e5e52cfb986)(label(true))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         a90c4e08-05d8-4ada-84d2-b1db20d55128)(content(Whitespace\" \
         \")))))((Secondary((id \
         d3875611-f2ad-4f3c-aba0-c61b0d9c88a2)(content(Whitespace\" \
         \"))))(Tile((id \
         e0bab29a-ea88-49ff-83c3-5d5ed9fdfa0b)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         eb067764-3293-4089-9eee-47c5742b5bb0)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         8b97c09a-d6d6-4696-8ebf-b1757c7712e7)(content(Whitespace\" \
         \"))))(Tile((id \
         ebc8cd6d-2665-43ce-9d4b-9220546c1bad)(label(1.))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         849670cd-e4ff-4725-b1fe-ced57783678c)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         cc02c4d8-d986-40ee-8a11-ac8b1f0e93c0)(content(Whitespace\"\\n\"))))(Tile((id \
         12c429fe-91d3-46f4-8183-ece8da86ca2e)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         40))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         9154198e-7e71-4b38-9db1-522e414bf933)(content(Whitespace\" \
         \"))))(Tile((id \
         559bc6a4-c9d7-4527-8095-229f9c4c5d15)(label(_))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         0a1ec056-f9c5-4119-866d-51b7857b9268)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 24))(sort Pat))((shape(Concave \
         24))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         87cd3191-672f-4b92-b78d-bc0d47503484)(content(Whitespace\" \
         \"))))(Tile((id \
         b3aa4f3a-9aa0-4bd9-8c1e-632e5ec001ea)(label(Int))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         429cfbd2-e169-4d28-8d12-752eae6c4e20)(content(Whitespace\" \
         \")))))((Secondary((id \
         a966f8c5-ea23-4b8a-bae3-8621fe5e2d6c)(content(Whitespace\" \
         \"))))(Tile((id b4084933-a239-4f7d-bee5-943e40ca3f68)(label(if then \
         else))(mold((out Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 35))(sort Exp))))))(shards(0 1 \
         2))(children(((Secondary((id \
         1bcb8e34-c416-4fe7-a0b3-a0529ae22ee3)(content(Whitespace\" \
         \"))))(Tile((id \
         fe3d2ba9-fb31-46e8-bde7-14677f9d9135)(label(true))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         c5d1294d-65a4-489a-be97-7047634d5cd7)(content(Whitespace\" \
         \")))))((Secondary((id \
         203789a7-5041-4a40-8082-d033e1f0da60)(content(Whitespace\" \
         \"))))(Tile((id \
         a8cce3e3-438b-4c19-8ffd-afe509422def)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         b1ece7f3-4996-49d3-b3cd-b83a353034f6)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         21775d84-ef74-4b0b-9a6c-fee02a195947)(content(Whitespace\" \
         \"))))(Tile((id \
         edc6a865-1893-49a1-aae4-08a73408d1d2)(label(1.))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         f76b9645-916e-4567-a427-b37018750798)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         c0046d85-65e8-4015-a728-78be06ebfa55)(content(Whitespace\" \
         \"))))(Secondary((id \
         9c869df1-22c2-4e45-8731-26d9d41a8765)(content(Comment \
         #err#))))(Secondary((id \
         a753239f-4538-4569-abd3-458ed506d723)(content(Whitespace\"\\n\"))))(Tile((id \
         76f7cb15-3ae5-4517-b124-363f77a9f1a0)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         40))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         5acd6256-e8b2-45a8-bac2-0cd64161868b)(content(Whitespace\" \
         \"))))(Tile((id \
         45762d3a-d4d6-4027-a78f-cd5245fff7fa)(label(_))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         e7127857-ae53-426c-af81-c06c55996b2a)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 24))(sort Pat))((shape(Concave \
         24))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         1eaef979-1ef8-4ece-90b2-b241c6d39b79)(content(Whitespace\" \
         \"))))(Tile((id \
         2aeee92e-e5df-4dd6-86ce-2d0e3357fc92)(label(Fake))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         1e4aad7b-e277-4a83-b331-cbc127fac06a)(content(Whitespace\" \
         \")))))((Secondary((id \
         9bf7c767-0cbf-4661-ae19-5e53c1d49ad3)(content(Whitespace\" \
         \"))))(Tile((id d27bb43b-9818-4c5d-a495-d3a997a4ff31)(label(if then \
         else))(mold((out Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 35))(sort Exp))))))(shards(0 1 \
         2))(children(((Secondary((id \
         3643f7e6-c003-444a-8ff9-7fe18d487749)(content(Whitespace\" \
         \"))))(Tile((id \
         322cf396-8061-4967-b372-0e60fd6ffa92)(label(true))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         4b82852a-f38f-4a3b-8d70-fd56a41edc96)(content(Whitespace\" \
         \")))))((Secondary((id \
         3ee3c54d-5111-475e-b552-d879000a06c7)(content(Whitespace\" \
         \"))))(Tile((id \
         ffdb9680-7bb5-4ae4-aa92-9295b80be26a)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         899ec603-ba36-4cab-b281-f885477f480c)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         9b78f6e5-3a70-455c-ac73-eb956ddee250)(content(Whitespace\" \
         \"))))(Tile((id \
         2128aa2d-edd5-410c-9950-aa339b5528de)(label(true))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         c3b4b00e-2562-4ce6-99ac-09e2343b87a2)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         5b64a3e2-ef16-4062-9048-14b7532fa2b6)(content(Whitespace\" \
         \"))))(Secondary((id \
         59532ddf-2fe3-4a8a-8fa4-ae6f2c5de37c)(content(Comment \
         #err#))))(Secondary((id \
         6451cc6c-8f24-40b0-8dae-6ef8ca6b2898)(content(Whitespace\"\\n\"))))(Tile((id \
         27ce18f3-f8bd-4bb2-9840-d141ee1fb60a)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         40))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         834fa734-2e82-4945-b792-4308974176fb)(content(Whitespace\" \
         \"))))(Tile((id \
         bd88af37-6bc3-4523-a806-d9e928f3e57d)(label(_))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         1c80dacc-eeb3-43eb-b656-f010c1ccd51b)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 47))(sort Pat))((shape(Concave \
         47))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         e761f55b-ca8a-4d4d-bba2-33e345e5b91e)(content(Whitespace\" \
         \"))))(Tile((id \
         68cbcbfa-8928-43c1-ac7e-a56f74055508)(label(_))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         40328d16-713f-4344-b716-14b29453d258)(content(Whitespace\" \
         \")))))((Secondary((id \
         de03c29d-12ed-4d4a-98a9-9d4b1ecbd01b)(content(Whitespace\" \
         \"))))(Tile((id 34d3ac88-58a5-40d4-b921-dbc853a538fd)(label(if then \
         else))(mold((out Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 35))(sort Exp))))))(shards(0 1 \
         2))(children(((Secondary((id \
         4d174470-e2b2-413c-8900-7866e9b78381)(content(Whitespace\" \
         \"))))(Tile((id \
         568738a7-3fe7-4860-a9e5-29eec641b0b5)(label(true))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         2eb1e2d8-693f-4c69-9d74-441ea25cb5f2)(content(Whitespace\" \
         \")))))((Secondary((id \
         c1d1c753-2175-40fb-bf5a-b2dd878fca0d)(content(Whitespace\" \
         \"))))(Tile((id \
         7d34f6cd-3a3b-4be0-acfe-d33bed1bdf28)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         9ddd79ab-6458-4df3-8e87-80342b8a2458)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         a8f08029-c7b9-4f66-97de-80a7b132ed70)(content(Whitespace\" \
         \"))))(Tile((id \
         b04581a2-6c84-44a9-a981-794a77699731)(label(1.))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         3226ec80-0eba-42fc-856e-763098104d1e)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         fcf7e5ca-9984-4ebb-81c6-d49f2cb8c6e3)(content(Whitespace\" \
         \"))))(Secondary((id \
         009735e0-6d27-4e1b-b1a8-d28c759a2153)(content(Comment\"#2x \
         err#\"))))(Secondary((id \
         79c9022c-0d53-42e3-aaea-198c89371c07)(content(Whitespace\"\\n\"))))(Tile((id \
         a10c2bb4-efed-4309-8575-2a981fe5a3e6)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         40))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         58c6520f-9c78-4391-849f-092e6df9bce8)(content(Whitespace\" \
         \"))))(Tile((id \
         8475fa9e-00f8-4db5-94cb-df1c14549d8e)(label(_))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         b6e2d7b5-3c5f-450c-92dd-faf84d34482d)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 47))(sort Pat))((shape(Concave \
         47))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         e9568afb-da2f-417b-b963-836f2a77b78c)(content(Whitespace\" \
         \"))))(Tile((id \
         498b4c9e-c83f-4903-8aa2-8f37723876a6)(label(_))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         5664e531-37cc-4cf6-bb0f-e50e80093c4e)(content(Whitespace\" \
         \")))))((Secondary((id \
         7294b417-f026-4aa6-8d7a-ea594e1d0543)(content(Whitespace\" \
         \"))))(Tile((id \
         a731490c-a07a-4103-ac8b-75cabdc49e27)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         f2ed4c90-ff4a-40f5-9f09-4526edaeb9a6)(label(if then else))(mold((out \
         Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         1a5d2e4b-576b-453c-a5c6-6719f52eaeb2)(content(Whitespace\" \
         \"))))(Tile((id \
         74674dee-ad61-4855-bc10-795a31e37072)(label(true))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         1757b3fa-ba6d-41ba-941f-f93147c7742a)(content(Whitespace\" \
         \")))))((Secondary((id \
         fb806572-f91a-47a7-9798-f3fdb6981726)(content(Whitespace\" \
         \"))))(Tile((id \
         a86eef5d-333e-44bd-8587-291e440a20c9)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         ee85798b-d39c-4b74-85e3-f98747240ba6)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         d34915c0-b5eb-4156-9453-f1eed014ed3e)(content(Whitespace\" \
         \"))))(Tile((id \
         245eddbe-8e5c-4634-a61e-f31ba9748ebf)(label(1.))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         54b67ddd-2801-42d4-9e6e-cc126d0e32d7)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Grout((id \
         991783d5-4c2b-4f41-bc9e-1814918edb39)(shape Convex)))(Secondary((id \
         c4b55248-7f79-4b1c-aee7-4edbab5ab141)(content(Whitespace\" \
         \"))))(Secondary((id \
         08915d5a-68e0-436f-90fe-a5e8ee8ef9fe)(content(Whitespace\" \
         \"))))(Secondary((id \
         f49908f3-3715-4c36-9eb3-60df4bd6c51b)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         7419cc32-db01-4910-b601-85266c29cfa1)(content(Whitespace\" \
         \"))))(Secondary((id \
         db12723e-0ceb-47b9-92ef-07a0e8fe1041)(content(Comment \
         #err#))))(Secondary((id \
         a3ec24c2-8b18-4352-b378-0d6ab863f845)(content(Whitespace\"\\n\"))))(Tile((id \
         9c296c1e-9333-44db-9b52-9e5dfad78439)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         40))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         b881c244-3b29-4ea5-9493-4595c571e951)(content(Whitespace\" \
         \"))))(Tile((id \
         1143d6ec-9a55-457d-8437-7b9128565256)(label(_))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         8ac78449-3962-4335-a0f3-5bf4d96d02fb)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 24))(sort Pat))((shape(Concave \
         24))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         5e850247-00d1-495c-bdb5-30e1e9c52164)(content(Whitespace\" \
         \"))))(Tile((id \
         25f7289c-efca-4ecf-b3af-3a337eadcde7)(label(?))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         b321045a-c0b5-410d-bd6b-e03ffa0ff689)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 47))(sort Pat))((shape(Concave \
         47))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         281db6ea-d28e-477d-84fe-9e2e8cd92df7)(content(Whitespace\" \
         \"))))(Tile((id \
         2b3747ed-1e55-4f5e-8ee1-53933c9956fd)(label(_))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         350f87af-91e8-44fd-bcb7-4d3b47fbdadf)(content(Whitespace\" \
         \")))))((Secondary((id \
         90cb28a8-8313-4431-8535-0bf87f8f4afe)(content(Whitespace\" \
         \"))))(Tile((id \
         81a8ec2c-5a01-4496-9ef5-44c147752423)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         aae738a2-7392-43a3-96ad-7dee0a656d93)(label(if then else))(mold((out \
         Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         e90e04bd-dac2-4f8b-b1d8-0059c8fd7b2e)(content(Whitespace\" \
         \"))))(Tile((id \
         405b8b50-cc74-4927-b837-f0a3a0f91843)(label(true))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         4e1e7fd5-35ea-48e7-b6c3-2996b7402e19)(content(Whitespace\" \
         \")))))((Secondary((id \
         6d7ad5f3-bbbf-4419-94b4-3d22c1553cf2)(content(Whitespace\" \
         \"))))(Tile((id \
         24a6fc8a-37f8-4cef-b017-a852ff58fcd1)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         8956d2c1-9507-4d3a-84a2-a0cdbe52cacb)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         e28ab3ba-f88d-4eca-8422-40e9739d80fd)(content(Whitespace\" \
         \"))))(Tile((id \
         73a1a607-4750-4bf0-b9d4-b30018818c56)(label(1.))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         552b5453-2860-417b-9156-3941c84377e2)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Grout((id \
         cb6e3239-7314-4920-aceb-81ae9116f6dd)(shape Convex)))(Secondary((id \
         29d53f48-ee95-4810-861b-32769c304f57)(content(Whitespace\" \
         \"))))(Secondary((id \
         09d26b3f-d5d3-4dd7-a4e1-0a77bfa2664e)(content(Whitespace\" \
         \"))))(Secondary((id \
         9a8e3f1d-5eb5-4d3d-80f7-ee7c76e693a5)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         6584c77b-c823-4f9c-b99d-932eb3b1dc29)(content(Whitespace\" \
         \"))))(Secondary((id \
         d4c4fd71-b555-4f89-98d1-24e032348336)(content(Whitespace\"\\n\"))))(Tile((id \
         901dc211-127a-4941-9a65-30879a2e6044)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         40))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         431b6a12-58f6-4561-9dd0-734af35e09ae)(content(Whitespace\" \
         \"))))(Tile((id 100c757a-bf86-4bd0-9667-d6b808636c72)(label([ \
         ]))(mold((out Pat)(in_(Pat))(nibs(((shape Convex)(sort Pat))((shape \
         Convex)(sort Pat))))))(shards(0 1))(children(((Tile((id \
         50514e98-24c5-49f3-bae9-2cc642a09769)(label(_))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children()))))))))(Secondary((id \
         b6d71d65-c31d-4954-a092-e0c4ba004644)(content(Whitespace\" \
         \")))))((Secondary((id \
         0786f86a-1daf-4755-b421-9a46ff78eece)(content(Whitespace\" \
         \"))))(Tile((id 71ed40d6-e600-47ca-a974-0c3e89dd0920)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         f8978cde-5cde-499b-8fc7-45b8e7202772)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         833765da-f967-4feb-9b48-3da0d99b3dd7)(label(if then else))(mold((out \
         Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         12bfab2d-0b66-43c0-835d-6af549af1415)(content(Whitespace\" \
         \"))))(Tile((id \
         c9c3b95c-3a6d-4f5a-9742-f9db3e51816f)(label(true))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         3a38bbfe-1f1c-47e5-bc76-d19b426c5f6c)(content(Whitespace\" \
         \")))))((Secondary((id \
         1580f4da-2657-448a-af57-98bc76537aad)(content(Whitespace\" \
         \"))))(Tile((id \
         ce592942-af31-43be-8be5-91b9a84ccc65)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         7eaae1e7-819c-4c5e-aef3-aaba9a02c5e9)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         e57f37f1-5b7b-4e02-b60c-94643590adbb)(content(Whitespace\" \
         \"))))(Tile((id \
         d3811035-2b01-4008-8d86-99c5edcc7ba8)(label(1.))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))))))))))))(Secondary((id \
         a8cba1ec-d6f4-4f96-95e1-708f5add08e9)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         61264b15-1465-4448-8cb7-70598bb3aac7)(content(Whitespace\" \
         \"))))(Secondary((id \
         fee7ca5b-cea2-4d08-b111-a11051f6f3a8)(content(Comment\"#2x \
         err#\"))))(Secondary((id \
         25969ab7-c6c2-4b6d-95e6-ab9e217d3259)(content(Whitespace\"\\n\"))))(Tile((id \
         7c440a3e-f7b4-4353-ba19-5d72799c12e9)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         40))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         f67f5a7a-1338-4647-af02-248a51289ffc)(content(Whitespace\" \
         \"))))(Tile((id 7569fd41-d48b-4295-86b0-ffb3b1c58168)(label([ \
         ]))(mold((out Pat)(in_(Pat))(nibs(((shape Convex)(sort Pat))((shape \
         Convex)(sort Pat))))))(shards(0 1))(children(((Tile((id \
         aea1e4b4-795c-48fe-9f74-26a075271ae4)(label(_))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children()))))))))(Secondary((id \
         daef4409-114f-42b1-af46-5a3aa99f5593)(content(Whitespace\" \
         \")))))((Secondary((id \
         ee3d0bfb-6911-4c37-aa22-b4dcab4e7214)(content(Whitespace\" \
         \"))))(Tile((id \
         0b452a24-6d4a-45a8-819c-8ab7bbda936c)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         29a89568-7c18-47aa-bda7-a0522a6eb0c7)(label(if then else))(mold((out \
         Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         9ea116f6-fa6e-4dda-9452-6a260ca3a2d2)(content(Whitespace\" \
         \"))))(Tile((id \
         d7868ba3-953c-4a0b-8737-247437344dea)(label(true))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         ec893e3a-24bd-4c17-9c52-d8f4c4b12593)(content(Whitespace\" \
         \")))))((Secondary((id \
         a944ac2f-8d39-4293-8207-5f7e8abe02e9)(content(Whitespace\" \
         \"))))(Tile((id \
         961eba76-9fff-47d4-b72c-c77df47e89b4)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         8884056e-9735-4812-b703-e546dbfbec53)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         32355763-09ca-4587-a833-49f9c880e302)(content(Whitespace\" \
         \"))))(Tile((id \
         a12035b6-357e-4f6a-bfc9-03e3803f591f)(label(1.))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         e911d058-8e69-46d2-b858-830e4f627efa)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         ed0b02f1-ce3d-49f6-a125-b63ee838ad8e)(content(Whitespace\" \
         \"))))(Secondary((id \
         2c2f8660-9a91-4ede-a070-87eeecad671d)(content(Comment\"#3x \
         err#\"))))(Secondary((id \
         7f7e800c-287c-4dcd-b142-7f71950820b2)(content(Whitespace\" \
         \"))))(Secondary((id \
         82451aa3-4d4b-4f76-96dd-d36a6c418aba)(content(Whitespace\"\\n\"))))(Secondary((id \
         6779aa02-b7d1-4a82-805c-ea8e9c393308)(content(Whitespace\"\\n\"))))(Tile((id \
         665aec53-b5a9-4bed-8d9e-9c72ad9e3e0f)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         894aa99b-a452-4ba9-9235-a93afefe4c8b)(label(?))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         a841d786-e6fe-4144-9aee-c578c6e56825)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         07601b0b-7b91-4e7d-998c-c2789a10fff7)(label(if then else))(mold((out \
         Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         abb752da-bc59-4b0c-af28-3dde59bbf0a4)(content(Whitespace\" \
         \"))))(Tile((id \
         69d3cd39-9b96-48b3-9a4f-00c687b76576)(label(true))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         b856432c-e543-4f9e-a780-c145e3b6e45e)(content(Whitespace\" \
         \")))))((Secondary((id \
         ccc84b67-e1ec-4dbb-a6f6-39415367e926)(content(Whitespace\" \
         \"))))(Tile((id \
         8fea77e5-68c5-48ff-bf2b-7ec114c5aa51)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         72177b79-9a1f-44d5-9c14-200127c77b5e)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         68081de4-37b7-4c3d-af38-866d4d4fccb2)(content(Whitespace\" \
         \"))))(Tile((id \
         011b764e-0e3c-4ae3-9170-31464a154398)(label(1.))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         91c99977-49a9-45ae-bf6e-8e3a1a89da6c)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 38))(sort Exp))((shape(Concave \
         38))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         2883e5c7-e551-4a26-96f6-a39b72c4c2da)(content(Whitespace\"\\n\"))))(Tile((id \
         525a97c8-afb4-4bab-a527-398ca499c192)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         7a43e34e-f57c-4a24-a234-06d1d6ba3917)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         d88dbf63-e36f-4ec7-be6f-9ac9e225f8dd)(label(if then else))(mold((out \
         Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         e3c646b1-4603-48eb-93ad-018c5116a633)(content(Whitespace\" \
         \"))))(Tile((id \
         c46e73d7-396f-4b64-a932-e181bbfba08f)(label(true))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         818cc8e1-eeda-493d-8049-a06df23d1ff3)(content(Whitespace\" \
         \")))))((Secondary((id \
         3dd07257-eab1-438b-920b-42ebc7ceb7fe)(content(Whitespace\" \
         \"))))(Tile((id \
         7c57e800-5314-40a9-8cb3-358fb6ea2d4d)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         ab7a7262-9170-4486-ad23-458c4087d5e6)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         5fba02d4-892a-46fd-a078-532fbb8e124b)(content(Whitespace\" \
         \"))))(Tile((id \
         6516c534-7685-4db2-a758-c578feb0d4eb)(label(1.))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         87e8783b-4131-4eca-9642-2c2fd6c202f5)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 38))(sort Exp))((shape(Concave \
         38))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         161df698-2c32-41ed-96fe-4177aba46064)(content(Whitespace\" \
         \"))))(Secondary((id \
         7ea191d5-649f-41e8-94ec-4ec7ffe0148a)(content(Comment \
         #err#))))(Secondary((id \
         75721c47-28d6-47a5-aab3-703353f0ef58)(content(Whitespace\"\\n\"))))(Tile((id \
         f295ea8e-0f70-486e-b03c-ff18fa32dcc5)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         a98faa01-706b-4807-a098-03fd5b6a3f22)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         08e0eaf2-7911-417c-ab51-e1be7643f7c0)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         e4470005-3db4-4938-a315-a25c2d5bd51c)(label(if then else))(mold((out \
         Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         d5008617-f54e-4b67-9576-663c9d04ad47)(content(Whitespace\" \
         \"))))(Tile((id \
         89d09597-53cb-4690-862d-9c965f4a4499)(label(true))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         3f7bee44-8cc5-4f2b-bcff-f828cf3e8f42)(content(Whitespace\" \
         \")))))((Secondary((id \
         7bb40119-6a06-46c5-8788-492bae2199b9)(content(Whitespace\" \
         \"))))(Tile((id \
         307f3a14-cdb3-4a4f-8716-c6b92a5384db)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         75a8a67f-fa20-4104-bae9-490cf308fec8)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         230757d2-385e-4b9e-bb4b-4e18272fd8dd)(content(Whitespace\" \
         \"))))(Tile((id \
         4e6898b2-8c91-490c-b6d1-6fd6cf0700c5)(label(1.))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         25480e68-a0c2-4ca4-a0c9-b9a5905a4799)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 38))(sort Exp))((shape(Concave \
         38))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         fae24115-93d0-4d13-8792-15ab53d436bc)(content(Whitespace\" \
         \"))))(Secondary((id \
         715d0357-7098-48b3-aa2e-5a345679eab7)(content(Comment \
         #err#))))(Secondary((id \
         f0090278-38d4-4a90-ace3-7135f282013a)(content(Whitespace\"\\n\"))))(Tile((id \
         84506fbd-3261-490c-92ee-c4d460a758d7)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         48da1513-4aa6-49c2-91a3-1ec2b268deab)(label(fun ->))(mold((out \
         Exp)(in_(Pat))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         36))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         c7e34f6f-b08e-45d7-b343-6cbb01082df5)(content(Whitespace\" \
         \"))))(Tile((id \
         3bd2878d-4cbf-440d-a99e-aee207fb6109)(label(?))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         02a5d42e-6d77-41af-95ec-9674fbae0e85)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         4fbd04d6-174f-4450-b1e1-ba08f079126f)(content(Whitespace\" \
         \"))))(Tile((id \
         d47ca5ee-b7af-455d-b56f-5e4c5f48c133)(label(?))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         5601da09-eab6-4c9a-ad05-3830afc8abc5)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         b4973733-0208-4340-9cd4-da71f0917bb4)(label(if then else))(mold((out \
         Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         4a78ca11-e2d8-4d99-8618-d3baa63ea1e7)(content(Whitespace\" \
         \"))))(Tile((id \
         9e820b91-9bb7-494a-84bc-51ce3d6534e2)(label(true))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         ecee921e-f33c-4995-a5bd-78f82c3ad121)(content(Whitespace\" \
         \")))))((Secondary((id \
         c4fe740f-a73e-4cdf-bbfd-2cf41af95289)(content(Whitespace\" \
         \"))))(Tile((id \
         0f6e917e-3ffa-4f68-9a54-77833755a1a3)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         2f423fd9-5467-4e41-9aeb-dcd0aad4b3cb)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         09806ca3-2da4-41e2-8941-45bc21dc34d3)(content(Whitespace\" \
         \"))))(Tile((id \
         f1bee554-70ed-4706-a37b-51eaf3925e2f)(label(1.))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         57c733cb-0e61-44eb-b739-2a6567443a82)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 38))(sort Exp))((shape(Concave \
         38))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         f46f64ff-1656-4474-9d70-2e3cc8a333ec)(content(Whitespace\"\\n\"))))(Tile((id \
         f313ea72-e9c5-4247-ada8-9ece1bfb2d60)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         b8b1d06c-57fb-4670-8f01-6e707d7dbc13)(label(fun ->))(mold((out \
         Exp)(in_(Pat))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         36))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         a6ce4f04-81c0-40bd-8fdc-582f25db83ff)(content(Whitespace\" \
         \"))))(Tile((id \
         cc9ad4e8-75d2-48a4-b6fe-313abb788474)(label(_))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         7e69bb8a-195c-450e-b219-3ffd40424f01)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         e2d8f190-0d80-43b8-8835-a8ca9d2375ad)(content(Whitespace\" \
         \"))))(Tile((id \
         6a21fdc3-d335-42cf-b8ef-1003617351e3)(label(?))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         f8f82b7f-f289-4d04-8cfb-99f6f4216517)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         d2aa680b-416e-43b8-a6da-3b563a3d0e8d)(label(if then else))(mold((out \
         Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         d9a15cda-8dc5-4b92-9bd3-22050cc80d39)(content(Whitespace\" \
         \"))))(Tile((id \
         6e5d4c7f-8239-46d7-8fac-43c983ed9b6c)(label(true))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         a03c45f5-bb4b-4ed1-82fa-cd52504dd727)(content(Whitespace\" \
         \")))))((Secondary((id \
         9c82ad9c-ca40-4342-a851-3e90c22fc342)(content(Whitespace\" \
         \"))))(Tile((id \
         eef07a04-00f7-4e3f-b231-91b944d6fc36)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         8be638b5-8bd7-461d-ab42-29cb8d426fa4)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         b1bdc8a1-e9e9-49b4-abf7-e84e0c4fb80c)(content(Whitespace\" \
         \"))))(Tile((id \
         17b90138-477e-42fd-87ce-ca87a247f1c2)(label(1.))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         778b356e-1a5c-4552-afb9-db2c9f7333db)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 38))(sort Exp))((shape(Concave \
         38))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         5d5cdf87-550e-40a3-924a-1f48ef8beacb)(content(Whitespace\"\\n\"))))(Tile((id \
         829659b3-ea9f-443b-868a-10d55ac509a9)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         f5ac7aed-1f9c-4121-9ee9-83829f5638ca)(label(fun ->))(mold((out \
         Exp)(in_(Pat))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         36))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         669b1bcd-75a5-414a-8cee-6ef14cea1ce3)(content(Whitespace\" \
         \"))))(Tile((id \
         611ec950-cddd-4125-bee8-d0bafeaae629)(label(_))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         e0acb1d3-42a0-471f-a5cb-e6172853d4dc)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 24))(sort Pat))((shape(Concave \
         24))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         c8879c03-69e6-439f-b8dc-a9eb9a420fe0)(content(Whitespace\" \
         \"))))(Tile((id \
         d7582ef9-825d-43a1-ba5d-4a8a664e0cd2)(label(?))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         bdc6fe7d-9218-41de-9720-44c50a63efb5)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         d705c0f5-f59e-4534-abf4-d8ee4a3fa907)(content(Whitespace\" \
         \"))))(Tile((id \
         bedcc997-623c-4d75-838f-bfd19f395474)(label(?))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         d385bbcd-8715-4f48-9e44-c681562a5a0a)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         78b12ac3-957d-4014-80fd-36379170da22)(label(if then else))(mold((out \
         Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         c2086a7b-56b1-4171-bb94-32d3c4d72ed3)(content(Whitespace\" \
         \"))))(Tile((id \
         aee04fd1-c469-468e-8d86-33085991e7df)(label(true))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         bb70a61e-7216-486e-8387-690051b5db5c)(content(Whitespace\" \
         \")))))((Secondary((id \
         054e4aa4-6662-443e-b9d2-6255bb91f763)(content(Whitespace\" \
         \"))))(Tile((id \
         dc6ad2e6-017a-4af3-94f2-e327e36ac2f5)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         efc5acbc-515a-4d4b-931c-0c1d72f3d5e3)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         5e7cfe09-76ce-4fbe-95e5-57597791f1cd)(content(Whitespace\" \
         \"))))(Tile((id \
         1ed28dbc-8d30-48d3-a3df-bfc03fb85b08)(label(1.))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         87930d01-eee9-4c59-aeff-9d2b913aa446)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 38))(sort Exp))((shape(Concave \
         38))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         5135cb57-fa43-4cd4-9d36-02d1965c3332)(content(Whitespace\"\\n\"))))(Tile((id \
         2e09bf12-f6b3-4d9c-9843-df87952a5593)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         4d57c0d2-d782-478b-b70f-60079207f4c2)(label(fun ->))(mold((out \
         Exp)(in_(Pat))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         36))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         d5bb7f97-142f-4823-af1a-a9d6da7c7115)(content(Whitespace\" \
         \"))))(Tile((id \
         fc5f68f3-d24f-451e-8330-950236a4a7cd)(label(_))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         bac2ff38-a238-4291-a00e-9d5a19764298)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 24))(sort Pat))((shape(Concave \
         24))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         d84b42fb-aed9-4888-a5f5-57b7fee545f5)(content(Whitespace\" \
         \"))))(Tile((id \
         aa83b4ed-3364-4716-af94-fd70d5201032)(label(Int))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         63adbf27-1102-4dd7-93ea-fe5cf5397681)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         7c21e2b8-1928-481b-835b-4722d3539731)(content(Whitespace\" \
         \"))))(Tile((id \
         38ce9dea-1f98-4194-ad57-6a27a7c38953)(label(?))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         74dc38f5-9e5f-4388-b709-3773bed9555a)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         a1bac526-61de-4226-971f-ad81abb64dfc)(label(if then else))(mold((out \
         Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         fe22e6d1-2773-4a9d-acee-2d70c17c9bde)(content(Whitespace\" \
         \"))))(Tile((id \
         ac2a43d6-2210-494a-a771-221a6d57b72c)(label(true))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         edc3b699-2754-4830-a916-ec141a1239d8)(content(Whitespace\" \
         \")))))((Secondary((id \
         1bae9171-0d9c-4ff3-9449-4da817e2e88a)(content(Whitespace\" \
         \"))))(Tile((id \
         a3ed5868-0973-442c-9922-93016fc2896c)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         558839d2-4fb1-415e-a520-f68afa5d671c)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         d299f148-87b1-418b-9670-0314af4e3431)(content(Whitespace\" \
         \"))))(Tile((id \
         d4135f9e-f143-4a45-9f8b-50aeb833bbcc)(label(1.))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         ceb8044b-2622-4847-9d5f-225c9d047899)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 38))(sort Exp))((shape(Concave \
         38))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         5c9da289-06e9-4204-a0a4-36b5a70a05bc)(content(Whitespace\" \
         \"))))(Secondary((id \
         c56db167-9c9d-4750-8a5f-7e21c4b174bd)(content(Comment \
         #err#))))(Secondary((id \
         fd522f77-8b83-4f6c-aa54-fb83fcaf910e)(content(Whitespace\"\\n\"))))(Secondary((id \
         32e1bcee-9acb-4eaf-bfa5-a486f7680407)(content(Whitespace\"\\n\"))))(Tile((id \
         53dfd921-3f75-40bc-a59d-17d044c9c995)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         40))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         780400bb-83a5-4a71-8820-51b9bc2ef6ed)(content(Whitespace\" \
         \"))))(Tile((id \
         ff799d06-6c7e-4d97-a72f-5c5d58aa250b)(label(_))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         bc7f21f5-add6-4323-b297-c839f896075d)(content(Whitespace\" \
         \")))))((Secondary((id \
         ccc8d48c-8730-4102-bc08-4143684e5aa8)(content(Whitespace\" \
         \"))))(Tile((id ce97ce49-79cd-4bd4-9bd3-2b7edc8560a6)(label(fun \
         ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 36))(sort Exp))))))(shards(0 \
         1))(children(((Secondary((id \
         cef5f7e3-2a7d-4027-88eb-9466de8dad4d)(content(Whitespace\" \
         \"))))(Tile((id \
         460847e4-a53b-486a-b388-62cad4bd78fa)(label(x))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         273eb771-4b6e-4ee7-8a9b-ecbe26f0b9be)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         60cddfbb-ed57-4fce-b8da-a2567ca99e44)(content(Whitespace\" \
         \"))))(Tile((id 2ac0a834-8403-45a6-b242-ec074a6c34e9)(label(if then \
         else))(mold((out Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 35))(sort Exp))))))(shards(0 1 \
         2))(children(((Secondary((id \
         8da801cb-2735-467a-b199-7ff204dd7f6a)(content(Whitespace\" \
         \"))))(Tile((id \
         80557bf2-556e-4323-883a-0a83efe4df16)(label(true))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         d819f655-3f20-4fac-b5c1-dc6c2506acf7)(content(Whitespace\" \
         \")))))((Secondary((id \
         d49143a1-890d-4dc3-91e9-8f5e8817fe18)(content(Whitespace\" \
         \"))))(Tile((id \
         ebacfa21-4d92-4d0b-a948-a992ddf73a36)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         1f553503-3ed6-4655-bb4c-8918c33b3407)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         f45dc549-a49e-48b0-ad3c-3f6470de3c59)(content(Whitespace\" \
         \"))))(Tile((id \
         dd4296e6-a51f-400d-a970-66edcff884b2)(label(1.))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         767662fa-80e0-494b-b5c0-10903affc855)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         9f7362fb-ed4d-46ff-a59d-026914f0f975)(content(Whitespace\" \
         \"))))(Secondary((id \
         14f3d66d-dd3a-44d5-a177-e83746b26a3a)(content(Comment \
         #err#))))(Secondary((id \
         6ebd689f-9ed9-4418-8457-fc50cb51de49)(content(Whitespace\"\\n\"))))(Tile((id \
         ced61029-15a2-4ece-949d-9ad02ca5893d)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         40))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         163d8ad4-c3ec-4b51-b412-faf86ae19e5c)(content(Whitespace\" \
         \"))))(Tile((id \
         3f98fcc6-9944-406a-8771-c8721083281a)(label(_))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         205359bb-8f5b-49e2-af58-b1ee0b85ff0a)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 24))(sort Pat))((shape(Concave \
         24))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         19f69b03-487f-4c40-a127-d20c2dbc3b8c)(content(Whitespace\" \
         \"))))(Tile((id \
         f671ed2d-cf0a-4c92-bebe-bf31b3589f50)(label(?))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         0e1c1be2-a0ec-4c7a-bb44-7309c8eadbc5)(content(Whitespace\" \
         \")))))((Secondary((id \
         702e92e7-5504-4a2b-99ae-5e705757b478)(content(Whitespace\" \
         \"))))(Tile((id bf90fa0f-6fca-4ed8-885b-8790f760dbc5)(label(fun \
         ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 36))(sort Exp))))))(shards(0 \
         1))(children(((Secondary((id \
         43e94a82-3c0f-43c0-af09-fbe5f631a4e2)(content(Whitespace\" \
         \"))))(Tile((id \
         155de169-46af-4e7b-ad1d-d8998e38f53c)(label(x))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         986d4827-578f-421f-9cca-f25b8a7a8260)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         cd1fdd8e-c2a1-45be-b8f5-0dbe3cbbb436)(content(Whitespace\" \
         \"))))(Tile((id fb61be28-1997-48fa-bdff-a2926426fd62)(label(if then \
         else))(mold((out Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 35))(sort Exp))))))(shards(0 1 \
         2))(children(((Secondary((id \
         f4447881-2ac0-47de-a6f8-ac3561acf8f1)(content(Whitespace\" \
         \"))))(Tile((id \
         449ec3f4-2714-4240-a767-c67c951835f1)(label(true))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         5d57d511-d290-45f7-bc42-788c1a20384c)(content(Whitespace\" \
         \")))))((Secondary((id \
         328f4059-e42a-4e81-bde3-1053785970ae)(content(Whitespace\" \
         \"))))(Tile((id \
         cee75fb4-9633-453c-9850-c867cbdcb05c)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         5bdf5fb7-8a56-4238-814e-159eeb56f0df)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         c4362581-b7a3-4933-bf56-369fe614bd34)(content(Whitespace\" \
         \"))))(Tile((id \
         8712d396-184b-4810-b8e1-fe3cec2af7b5)(label(1.))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         7b17649b-7dea-424e-b87d-08433448d900)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         80009d8e-ab3d-460b-ae26-ae2e0ab6c172)(content(Whitespace\"\\n\"))))(Tile((id \
         e13f9788-61b8-4181-86d7-fc790ab64b0a)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         40))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         5f7b55ef-6069-4771-985b-59dbad793267)(content(Whitespace\" \
         \"))))(Tile((id \
         730035eb-694f-4627-be8d-7d54cff3fd21)(label(_))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         3060a825-d051-4640-8b1f-0b2372b4d66e)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 24))(sort Pat))((shape(Concave \
         24))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         b7864190-3106-49dc-96b0-e06c7d81d658)(content(Whitespace\" \
         \"))))(Tile((id \
         65ea8250-a5ba-4318-8723-44e086576000)(label(?))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         5992a9e7-18e2-4b13-b098-8ae06ebfed55)(content(Whitespace\" \
         \"))))(Tile((id \
         16a58101-c705-4ed6-9f02-f11560bfad3a)(label(->))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 13))(sort Typ))((shape(Concave \
         13))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         6be3f4f6-efd9-4fd5-8ba4-01127ac04c00)(content(Whitespace\" \
         \"))))(Tile((id \
         d6886f74-3f9f-4a27-8d77-bbaebf2ca381)(label(?))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         9b63f0e0-6297-4493-9077-d52c76ce24e6)(content(Whitespace\" \
         \"))))(Secondary((id \
         64b555f2-e94f-4691-9e45-301583c3742a)(content(Whitespace\" \
         \")))))((Secondary((id \
         4972f571-a3fd-4816-b119-aee20c0b126a)(content(Whitespace\" \
         \"))))(Tile((id 4b774a8c-d821-4105-be4b-8e09b6f50655)(label(fun \
         ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 36))(sort Exp))))))(shards(0 \
         1))(children(((Secondary((id \
         8a8754c2-3555-4e44-8514-563e26737d19)(content(Whitespace\" \
         \"))))(Tile((id \
         8541161a-81dd-4839-9808-21738fd1db21)(label(x))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         ab8878dc-5a34-4556-ae00-c162a9db3343)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         ef28c41b-a269-4e3e-aeb7-76631b4cb9eb)(content(Whitespace\" \
         \"))))(Tile((id 735c560e-54c9-4c9a-95a8-c64725be2572)(label(if then \
         else))(mold((out Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 35))(sort Exp))))))(shards(0 1 \
         2))(children(((Secondary((id \
         34789ff0-61a0-42b7-9974-e2c813a4a00e)(content(Whitespace\" \
         \"))))(Tile((id \
         7e220260-df98-4063-93cc-9992424d6730)(label(true))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         e11fc827-cb74-4f14-b95b-998532bdb4fc)(content(Whitespace\" \
         \")))))((Secondary((id \
         5c5615f5-d2f9-4d42-8a1d-5c13a6018427)(content(Whitespace\" \
         \"))))(Tile((id \
         6dded3ca-c3b5-4ef4-9c3f-52ce1e5bf7d2)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         c6d6b75a-87f7-49fc-90e7-a0c80d280c9b)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         6b4e1f3f-0f44-4b76-a6ce-fdaedecd2453)(content(Whitespace\" \
         \"))))(Tile((id \
         ec7509d3-dce7-45c8-ac7f-a0d1967ff130)(label(1.))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         4d8c4716-b9b6-4f59-b38b-84a37b97167c)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         084f319e-eb95-4558-baad-e7bebaccb891)(content(Whitespace\"\\n\"))))(Tile((id \
         f1101328-d581-4256-90b2-cbf97644591b)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         40))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         a39d6ed3-f1b4-4618-8747-6b6a0664eeec)(content(Whitespace\" \
         \"))))(Tile((id \
         31ea0368-71db-48ef-8a78-0c9da424f58f)(label(_))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         66b0af99-3c28-4976-b6f9-83a90e56199e)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 24))(sort Pat))((shape(Concave \
         24))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         65ccdf4e-6285-4203-9c88-a640ff43eef7)(content(Whitespace\" \
         \"))))(Tile((id \
         90d9c157-6e49-4161-9293-b83729071a19)(label(?))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         db238760-295f-43f9-9271-2e41af5b8507)(content(Whitespace\" \
         \"))))(Tile((id \
         03621745-240b-48a4-8950-b139047f38e3)(label(->))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 13))(sort Typ))((shape(Concave \
         13))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         b08256bd-a45b-41ad-86dc-6f5d0e5397ba)(content(Whitespace\" \
         \"))))(Tile((id \
         48dfc15a-2fd1-4aed-886e-6348f35776a5)(label(Int))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         926a89fd-3c36-4c29-bd81-16016891d92d)(content(Whitespace\" \
         \")))))((Secondary((id \
         6afb16f5-f226-4cc8-b741-5139ea577a3b)(content(Whitespace\" \
         \"))))(Tile((id f64c80b8-571c-40d6-8e7e-6fc673646663)(label(fun \
         ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 36))(sort Exp))))))(shards(0 \
         1))(children(((Secondary((id \
         ca04f8a9-56be-4caf-bd79-03c8a4b56b15)(content(Whitespace\" \
         \"))))(Tile((id \
         d1e48d3d-2142-408b-8407-7fd7fe2f7b22)(label(x))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         32ee933b-2ea1-410c-8c48-de5331788032)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         709a6663-e1a3-4211-b860-daad10fce62d)(content(Whitespace\" \
         \"))))(Tile((id 333b589d-398b-48e4-a169-d718146e466f)(label(if then \
         else))(mold((out Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 35))(sort Exp))))))(shards(0 1 \
         2))(children(((Secondary((id \
         57ee22a6-982f-4226-88d3-09ea891d7579)(content(Whitespace\" \
         \"))))(Tile((id \
         7f445078-75d5-411a-8a29-bf648d3c607c)(label(true))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         43eaf1e2-1ad7-4af9-8289-0ebb6c77b43a)(content(Whitespace\" \
         \")))))((Secondary((id \
         8d01d4e1-7157-4229-8e96-5b3729fe9400)(content(Whitespace\" \
         \"))))(Tile((id \
         efa27392-2876-42ba-8017-66bc0918a991)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         29ec833f-2b21-43c5-bd81-0d9a0b2d1f82)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         1c25c0a8-1cd2-4ba2-9f53-60230ee62ace)(content(Whitespace\" \
         \"))))(Tile((id \
         5d48994c-8529-4d39-9013-6670f81f3010)(label(1.))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         c74640a8-c1ed-459c-85fb-eef0be3221e6)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         4bbc4388-013c-48ce-8062-4b21380475e3)(content(Whitespace\" \
         \"))))(Secondary((id \
         c4442daf-4211-4aed-8522-370b554e2a86)(content(Comment \
         #err#))))(Secondary((id \
         66de09f4-4876-40e5-932f-7ea4adfddf0b)(content(Whitespace\"\\n\"))))(Tile((id \
         3b064c00-4b26-4810-8df2-b480977a28dc)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         40))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         c32d4af9-e1e3-4d86-842f-0d37d1067424)(content(Whitespace\" \
         \"))))(Tile((id \
         edc93bc5-d57c-4395-9761-ea8ae00e0a24)(label(_))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         bfeb7045-38ac-41cb-97bb-57f5f3d6feb0)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 24))(sort Pat))((shape(Concave \
         24))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         668620fe-4ca5-4d6b-9b47-6a999bdad02d)(content(Whitespace\" \
         \"))))(Tile((id \
         7a4335bf-ad39-4e4c-b3e7-797282b9d308)(label(?))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         a9fb2f55-a691-490b-ac8b-b2fff240b547)(content(Whitespace\" \
         \"))))(Tile((id \
         a42675bd-9c61-4dfa-92ce-bd41f2d9c6e2)(label(->))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 13))(sort Typ))((shape(Concave \
         13))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         0dbc6a95-c026-4ea4-9604-8ce80d5983d0)(content(Whitespace\" \
         \"))))(Tile((id fb2a49b7-acf4-40a5-bbba-81ef11d61abe)(label([ \
         ]))(mold((out Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape \
         Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
         e839986b-3c3d-43e0-99fc-914a0041ec4b)(label(?))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children()))))))))(Secondary((id \
         4364b344-ff1d-413b-8b65-a0244c9b71b1)(content(Whitespace\" \
         \")))))((Secondary((id \
         0f3e9983-2fa6-455c-898a-92796b1c2391)(content(Whitespace\" \
         \"))))(Tile((id 21fa992b-596e-4a9b-99c3-65fc2a8a45f7)(label(fun \
         ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 36))(sort Exp))))))(shards(0 \
         1))(children(((Secondary((id \
         10235302-279e-444f-8957-b81f97a21e0b)(content(Whitespace\" \
         \"))))(Tile((id \
         501ae36b-5ac5-4e01-9994-db3341b8e101)(label(x))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         77dd2aff-d790-4ed8-94f4-e293878736e5)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         4d1f3e1d-7033-4606-947e-c193c3ffbe3e)(content(Whitespace\" \
         \"))))(Tile((id 8a3164e6-d5bc-4e21-aa58-dc3eb042d42e)(label(if then \
         else))(mold((out Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 35))(sort Exp))))))(shards(0 1 \
         2))(children(((Secondary((id \
         4502240f-b80a-4a8d-a190-7802ee1e7eac)(content(Whitespace\" \
         \"))))(Tile((id \
         56f197fa-eff9-4b59-b025-672d80ee2c00)(label(true))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         632ba7c9-752a-4716-aad0-156cf493cd35)(content(Whitespace\" \
         \")))))((Secondary((id \
         de016326-9907-435d-b41b-8fed4890c789)(content(Whitespace\" \
         \"))))(Tile((id \
         292e6e4c-32fe-41b2-a6bf-33fb8e60c6b5)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         80b9e948-91ca-4aae-80d6-477b6e31d299)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         c97b5be3-3aab-4eb8-9e26-2d6a109ec1a1)(content(Whitespace\" \
         \"))))(Tile((id \
         b846bc75-9b38-4f4f-8a04-954042fb3d07)(label(1.))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         fea6277b-407f-4276-b060-aeff584272b7)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         e943ae2c-2928-48f4-afa7-0a9c93467928)(content(Whitespace\" \
         \"))))(Secondary((id \
         d5f5df96-9f3e-49dc-8f8d-ee23bc2a5f5f)(content(Comment\"#2x \
         err#\"))))(Secondary((id \
         57c4c25d-a748-4a54-84a3-183cbb848612)(content(Whitespace\"\\n\"))))(Secondary((id \
         3e4a3b11-2c0b-4abd-ae36-b3dc83ed8473)(content(Whitespace\"\\n\"))))(Tile((id \
         ecf1d735-5555-4c1b-afa4-75ec220373ad)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         0399e4c4-eda4-420f-9021-52cb4081c604)(label(?))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         e8136411-d9c7-410f-9877-65f84ffcb0e3)(label(::))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 29))(sort Exp))((shape(Concave \
         29))(sort Exp))))))(shards(0))(children())))(Tile((id \
         10896b83-ebf8-4249-b183-917ae9285ef1)(label([ ]))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         c332fe64-aa86-4183-a268-68a05f45ee18)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         38fd922f-d9bc-4a47-8653-af6413efd213)(label(if then else))(mold((out \
         Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         a296eb68-0661-4a37-800b-f561ca98fd70)(content(Whitespace\" \
         \"))))(Tile((id \
         c5d45789-ed64-4a06-83df-cee577465987)(label(true))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         88586357-802c-4271-aed4-c838070099ce)(content(Whitespace\" \
         \")))))((Secondary((id \
         918d6947-eb49-4fc2-9117-76de87f282ad)(content(Whitespace\" \
         \"))))(Tile((id \
         79182e91-f684-406d-b77a-4c15ffc690fe)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         6e544cff-bcac-451a-b95a-1294ed2aaec8)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         7127cbd4-d6e9-4c76-aa0a-f562511f4fbd)(content(Whitespace\" \
         \"))))(Tile((id \
         2ddf8541-91a7-4155-b401-2cbb52361c04)(label(1.))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))))))))))))(Tile((id \
         5424fb06-91c6-46b0-8820-e55db178a38b)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 38))(sort Exp))((shape(Concave \
         38))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         1f4fe9a5-1de8-4bf3-a7d4-0356b33b444b)(content(Whitespace\"\\n\"))))(Tile((id \
         83ba3139-0712-4a87-9fe5-0c6833f7e3a5)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         55a5b1a1-f1c0-411d-a21a-b73b13e7c97f)(label(::))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 29))(sort Exp))((shape(Concave \
         29))(sort Exp))))))(shards(0))(children())))(Tile((id \
         8c3cfb1a-2e08-453f-9bac-851f786b3ba6)(label([ ]))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         e40e70be-51ed-42bd-b138-3abacf70cb29)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         ed80ec5f-fbe7-4d44-9ed2-1662964853f7)(label(if then else))(mold((out \
         Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         d513a380-1249-40c4-a744-0fd0b118e893)(content(Whitespace\" \
         \"))))(Tile((id \
         4c2826ed-a4e1-4054-9d9e-36f8ea72d5fa)(label(true))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         988b6f1e-d500-4c56-9e00-351fde340137)(content(Whitespace\" \
         \")))))((Secondary((id \
         b2768fbe-61f6-4263-847b-345e4cecb654)(content(Whitespace\" \
         \"))))(Tile((id \
         30ce80f8-4705-4d45-b180-48e75a859eaa)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         e2c423df-fa6e-4868-a6e1-875ea33d194a)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         8d47c5c8-c970-4615-a5d4-f13d328f3b63)(content(Whitespace\" \
         \"))))(Tile((id \
         a21e5c4e-0ab5-4295-996a-9a458813215c)(label(1.))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))))))))))))(Tile((id \
         d83b3429-a8f3-491b-acaa-ec8ea954d30f)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 38))(sort Exp))((shape(Concave \
         38))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         55f2fecc-b465-491f-9c25-c927c4931e9c)(content(Whitespace\" \
         \"))))(Secondary((id \
         a5768f95-09fb-460f-8da3-79cb333f93b3)(content(Comment \
         #err#))))(Secondary((id \
         9b8d41c7-ca5e-42bb-baa3-6b3280bb82b7)(content(Whitespace\"\\n\"))))(Tile((id \
         4b3ec348-0a43-4972-b9e1-681facfe7553)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         b0f2b83a-c4fe-4d1d-bf58-df9b383b1cc3)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         f705df81-90b0-4a87-9133-333abb060f93)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         07d5a3ac-7650-4b63-ab4b-c9e3cec5faad)(content(Whitespace\" \
         \"))))(Tile((id \
         75a10aa4-a314-4544-87bd-2a4d2eca2cc4)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         098f9429-f3db-4310-bd0b-9640d6b54814)(label(::))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 29))(sort Exp))((shape(Concave \
         29))(sort Exp))))))(shards(0))(children())))(Tile((id \
         885d2874-0e5f-44ad-8e95-b1979cc2bf9d)(label([ ]))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         10ffd20f-dc3a-4049-9e67-b2ca9e3da807)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         905b376b-ca1a-4b4a-8b85-246b730db796)(label(if then else))(mold((out \
         Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         30479109-d095-4462-b852-60e0f40b7471)(content(Whitespace\" \
         \"))))(Tile((id \
         18c2044b-9f27-4c4b-90d3-227a953e872f)(label(true))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         a3c475dc-1445-4c20-bf6d-0ced3e781697)(content(Whitespace\" \
         \")))))((Secondary((id \
         e9c13eb5-2d77-4f38-9790-ba425d590cc4)(content(Whitespace\" \
         \"))))(Tile((id \
         2abc978e-081e-443d-b04b-0d0faac6135a)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         443309a7-dd18-4122-bd45-6e2d855627b4)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         a9d7d206-aac6-43fb-90de-a3006283f629)(content(Whitespace\" \
         \"))))(Tile((id \
         8b200de7-f41b-485a-9be7-37b012d15951)(label(1.))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))))))))))))(Tile((id \
         a2ddcb34-ee69-4edf-8c62-9532fdc55871)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 38))(sort Exp))((shape(Concave \
         38))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         82ee32fa-f5cf-4b90-bfb6-29590612a4c1)(content(Whitespace\" \
         \"))))(Secondary((id \
         cb484040-cd8a-4455-9483-351882d88034)(content(Comment\"#2x \
         err#\"))))(Secondary((id \
         1502a871-c08f-42d8-9cb1-f97004f666fc)(content(Whitespace\"\\n\"))))(Secondary((id \
         b54c3ac3-837f-4a37-b3ae-62cdc3385892)(content(Whitespace\"\\n\"))))(Tile((id \
         0accfe54-c8e0-41d8-9652-b541263bc8f3)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         40))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         9c837bbb-59df-432b-ae31-e962ca7b5ecc)(content(Whitespace\" \
         \"))))(Tile((id \
         a2e64036-e8a2-468c-917f-ba6f59ec8f63)(label(?))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         3420df41-fed8-46dc-a6ed-0d80ce5762ec)(content(Whitespace\" \
         \")))))((Secondary((id \
         7f5115c0-9737-459a-8bf4-0c49418c1c90)(content(Whitespace\" \
         \"))))(Tile((id e9b81fad-e421-4f86-b5ee-5b8a84b9106e)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         2e0eb514-e003-4014-b899-beb6aaafd8dd)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         ba2e8e89-fb88-4c24-a092-e944c154b971)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         87928511-e98c-4807-8420-578cb800c1ef)(content(Whitespace\" \
         \"))))(Tile((id \
         c913f3c9-032f-469b-8da8-122ed68af0a2)(label(1.))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         6dd0e7b9-29fb-438f-bdb5-ecad182d09eb)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         7890db35-5ef3-4479-a319-c41938a89763)(content(Whitespace\" \
         \"))))(Tile((id \
         d9da2f54-dc35-4e9f-8208-3b311db936cf)(label(true))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         68fb56c4-53c7-4042-98f1-1f4fb59190c8)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         453f7c22-9bfc-4f91-8937-d55b90577a76)(content(Whitespace\" \
         \"))))(Secondary((id \
         4fdcf553-18ac-4c09-93ca-e7bc743cd3d8)(content(Comment\"#err: \
         inconsistent#\"))))(Secondary((id \
         efe94108-28af-4eed-a10d-aef0331c7b49)(content(Whitespace\"\\n\"))))(Tile((id \
         d1e2f00f-49a1-4e4f-b9b5-2ebe130e1ac4)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         40))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         33dcdc9b-21ba-40ca-bd89-0c15e8666a43)(content(Whitespace\" \
         \"))))(Tile((id \
         4a7061e2-050f-48d6-b66c-28dd8a98db4f)(label(_))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         cf0a0471-7b94-4a0b-b6f6-0eea1c1b7509)(content(Whitespace\" \
         \")))))((Secondary((id \
         303cff68-8027-430f-803a-e2c5b308aefb)(content(Whitespace\" \
         \"))))(Tile((id de32b5bc-6648-4cc6-bce5-28a20f93ab97)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         b5adb7da-2090-4646-9703-03ceb40fd9a0)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         c2d8fd4b-9f95-451f-83f5-4bfe924c6b16)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         f51aa555-cc83-40a2-a814-a296176cb9cc)(content(Whitespace\" \
         \"))))(Tile((id \
         33f55858-2f41-4543-9d45-b4827a00628c)(label(1.))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         b617d1a3-057b-4d50-a562-da9186feb7a3)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         81bdda93-8bf6-43b9-8155-f34d1351eb46)(content(Whitespace\" \
         \"))))(Tile((id \
         c0e74283-8380-4e8c-8738-17e849030a8c)(label(true))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         2ca06c00-ca7f-41bf-94da-7e60e82eb386)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         32def33b-b1cc-4b16-a16d-dd09cc879c31)(content(Whitespace\" \
         \"))))(Secondary((id \
         f59d3c7f-a4c5-4f1a-900d-86c638ae2062)(content(Comment\"#err: \
         inconsistent#\"))))(Secondary((id \
         a5765ee4-b66e-46d5-8d6d-7372a1afdd73)(content(Whitespace\"\\n\"))))(Tile((id \
         bfc6655e-bbad-428b-950b-32877ed33102)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         40))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         9a87ad7a-49d5-4c18-b19b-b1b76cabe07b)(content(Whitespace\" \
         \"))))(Tile((id \
         76e6daad-1c95-4bd0-9074-afc190cec23e)(label(_))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         4433b5b8-753c-48ff-8322-aea3f7922af2)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 24))(sort Pat))((shape(Concave \
         24))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         bcb7219d-2770-4cbf-9ad0-e1319bd53147)(content(Whitespace\" \
         \"))))(Tile((id \
         9cf6c186-a737-4cfc-b81f-1b43a7fdc04a)(label(?))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         6c559392-54b8-4f57-94d3-8bc59f636f66)(content(Whitespace\" \
         \")))))((Secondary((id \
         43e8dc40-bd6c-4a51-95b6-9e645f9caf14)(content(Whitespace\" \
         \"))))(Tile((id 0d8f921d-17b8-4c93-943d-d990eca5ecaa)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         db4ba04d-9fff-419d-b2bc-822e35b42db4)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         ecf55d5b-1aef-464b-b890-af64400c056c)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         faed2078-c5c1-4737-85f7-18f75326e4c1)(content(Whitespace\" \
         \"))))(Tile((id \
         0ceda5d9-df85-4bf2-8ff0-f19f6c8f5c78)(label(1.))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         ba88f004-270c-43f2-947c-b1d1e89f03eb)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         48dcae34-9422-49b2-b8fb-e8dcb984b082)(content(Whitespace\" \
         \"))))(Tile((id \
         979b5461-7a43-42eb-bf4c-42bbecb6382c)(label(true))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         dd0db821-c73d-40ba-9526-656580d5deb4)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         f7cc2fe3-f0cd-4c75-89a2-91d20c556ba2)(content(Whitespace\" \
         \"))))(Secondary((id \
         fadf9b6b-a23d-4920-a926-5b3deea8ef65)(content(Whitespace\"\\n\"))))(Tile((id \
         d395cd7b-0f47-4dcd-9dcd-bdc164f036dc)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         40))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         d38149b0-13b1-48cd-a6d7-fea215fc2779)(content(Whitespace\" \
         \"))))(Tile((id \
         099815b4-9210-43c7-873c-dba590503717)(label(_))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         4916a494-9fbc-4e24-bb3e-c7e11c7700a6)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 24))(sort Pat))((shape(Concave \
         24))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         8a8a2f7e-eaf5-46b2-8706-4ae7dc2dc744)(content(Whitespace\" \
         \"))))(Tile((id b495a9c1-851e-4557-8afc-bbb66a18fdbd)(label([ \
         ]))(mold((out Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape \
         Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
         90d02898-0dfb-4670-a4b4-22bf842fc9b8)(label(?))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children()))))))))(Secondary((id \
         daa4b4f1-01dd-4dc8-9918-699a455eb253)(content(Whitespace\" \
         \")))))((Secondary((id \
         0f2a0b7e-7ca4-4855-8fa5-691e626c1172)(content(Whitespace\" \
         \"))))(Tile((id f6ae5081-e5fb-4bdd-ad5e-a46a8d81148c)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         bc517dd1-93e3-4299-aedd-644e7e5ebabe)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         89d954a0-62bb-4c1f-bb74-d25ccbd9c282)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         e79ed1c3-ccd8-482c-bbd9-e40e15a97e2c)(content(Whitespace\" \
         \"))))(Tile((id \
         866dd9f1-4a5f-4060-a5ae-94e0649502c5)(label(1.))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         5141d0b5-8ecc-4b47-ac86-9a5962cefdcf)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         51717e79-b080-4c1c-8be8-0d2eb6e360e0)(content(Whitespace\" \
         \"))))(Tile((id \
         64dc1dc4-2693-4f89-b43e-61edf187c76a)(label(true))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         0053a3e3-25d8-4acd-ab85-3dff9b0bdd80)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         6b601656-692e-4cd1-9e8b-2e958269b38b)(content(Whitespace\"\\n\"))))(Tile((id \
         e3a862bf-2462-403d-b26f-0299281b9001)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         40))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         3593ce9e-6e5e-4b3d-8674-759a08f1835b)(content(Whitespace\" \
         \"))))(Tile((id \
         245bbe2d-6ef1-45e1-9909-fb66f7838b8d)(label(_))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         007acf0e-b2c2-4225-a183-2ebf46d29ddb)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 24))(sort Pat))((shape(Concave \
         24))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         f5adca03-78cf-4a7a-b77b-2ce55b3a3c7e)(content(Whitespace\" \
         \"))))(Tile((id 288d6cde-c7a9-4620-a0f3-320dc24885b8)(label([ \
         ]))(mold((out Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape \
         Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
         e7330219-7543-47a3-90f5-e89096e11f3e)(label(Int))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children()))))))))(Secondary((id \
         c3ca8470-6d6a-4194-8952-9dfd39f582cd)(content(Whitespace\" \
         \")))))((Secondary((id \
         189aaef1-71b3-471d-a353-89c3ef71ef75)(content(Whitespace\" \
         \"))))(Tile((id fcb11fcd-6f24-40aa-849c-c99304fb5f26)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         debdbce8-dd29-4245-bb2f-d8c12bd6a518)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         4b7d6aa8-90f5-4dad-a6ea-3af6e69e577b)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         ffa3ffc1-7efa-4a5d-aa98-2e8cdd9f5f08)(content(Whitespace\" \
         \"))))(Tile((id \
         52d11e19-919b-4801-a280-9d3de32e7f36)(label(1.))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         63b30a27-207e-4301-971f-8bcf418b758f)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         cd1f4be6-f5cf-430d-991b-26d9bda00bd3)(content(Whitespace\" \
         \"))))(Tile((id \
         016ff398-d294-47bf-aa01-e545a430c532)(label(true))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         c984fb84-eb69-48f2-bdc9-4a16c4a082a6)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         a37e9f1b-04ed-42fc-8a56-66f8d329795b)(content(Whitespace\" \
         \"))))(Secondary((id \
         16c2de07-3da7-4c33-bfe2-f873adf9532f)(content(Comment\"#2x \
         err#\"))))(Secondary((id \
         02b5639f-a5c5-4900-9299-f7947d2aae3b)(content(Whitespace\"\\n\"))))(Secondary((id \
         9f541dfd-d7bf-42ed-b0ad-63750c928c1f)(content(Whitespace\"\\n\"))))(Tile((id \
         580e19b1-1f48-4366-9bd0-c11f8e205079)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         40))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         4fd35541-c7b6-43b5-8226-519db90b11cd)(content(Whitespace\" \
         \"))))(Tile((id \
         f06d6a72-6d7a-4948-895a-9e78beff3497)(label(_))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         33176bc2-a18f-4c4c-b493-ec3bf568413a)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 24))(sort Pat))((shape(Concave \
         24))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         b77ad924-30f8-4da1-a66e-20a6954d53fe)(content(Whitespace\" \
         \"))))(Tile((id fede16d0-5828-4134-8e12-1e292761c48f)(label([ \
         ]))(mold((out Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape \
         Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
         733238e6-abb2-417b-a416-eb499db212da)(label(Int))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children()))))))))(Secondary((id \
         4b88281b-edcc-4cbc-9e12-fcc920a01e21)(content(Whitespace\" \
         \")))))((Secondary((id \
         85405088-6043-44d3-9e2e-8fc577e2232a)(content(Whitespace\" \
         \"))))(Tile((id \
         1ff4d728-8cfe-433e-bbd1-897f012a7fe8)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         21b93826-bc5c-46d2-ab23-4c14f8c6560a)(label(::))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 29))(sort Exp))((shape(Concave \
         29))(sort Exp))))))(shards(0))(children())))(Tile((id \
         f927bf84-9b59-4feb-b63e-423ba67f5412)(label([ ]))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         091eb942-6cdb-4936-8e91-94b0c60d6a9e)(label(2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         c912e8c2-e6c4-48f3-a154-ea66a4df8110)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         e8e74505-ca00-49c5-bd1f-e78f61de0c43)(content(Whitespace\"\\n\"))))(Tile((id \
         e1256329-6702-4e64-bf29-c85a63ca3921)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         40))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         d4493479-c751-4394-9386-2f2c808cb715)(content(Whitespace\" \
         \"))))(Tile((id \
         306a71b2-f382-45b8-9209-3bac2b94b417)(label(_))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         5256d653-682f-4508-8c3f-9562a221f4cc)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 24))(sort Pat))((shape(Concave \
         24))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         e394221c-e7e1-468e-b218-7dc9380241e8)(content(Whitespace\" \
         \"))))(Tile((id 82ea3b8b-ea61-4c68-8ff1-9b927c3cf0b0)(label([ \
         ]))(mold((out Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape \
         Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
         4c8d0f08-be2e-49f0-a6de-8cd54df5cdd4)(label(Int))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children()))))))))(Secondary((id \
         bf2843e6-3012-4be6-8137-7d7f0013ae67)(content(Whitespace\" \
         \")))))((Secondary((id \
         9a2ad2fa-e216-4cd0-af78-ab01927fbfd0)(content(Whitespace\" \
         \"))))(Tile((id \
         5954f8b3-a5a0-40a3-96e3-63970f01e6df)(label(1.0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         c5d4183d-50a0-42fe-8abc-64e6f9bc62d1)(label(::))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 29))(sort Exp))((shape(Concave \
         29))(sort Exp))))))(shards(0))(children())))(Tile((id \
         9777300b-608b-4c51-a204-97fc57a53367)(label([ ]))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         83a19aae-a628-41aa-992f-cb57273f1c68)(label(2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         f27ce9c5-3b32-494a-ab67-c3611c99fd81)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         19e8c9fe-08e1-4973-be18-7f5f629fcd59)(content(Whitespace\" \
         \"))))(Secondary((id \
         bbb459a7-44b5-4ecc-b1de-ae4c0a900c35)(content(Comment \
         #err#))))(Secondary((id \
         799e57be-f8a8-4114-b10c-7792c40934a8)(content(Whitespace\"\\n\"))))(Tile((id \
         149faa02-0d36-49a2-98bf-ce1581032260)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         40))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         2578fdae-f19b-44ae-867b-7188249d05d5)(content(Whitespace\" \
         \"))))(Tile((id \
         ccd31267-bd20-4f66-911e-f5a8df54d910)(label(_))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         8c041d18-6222-4737-82cb-23eac5b02468)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 24))(sort Pat))((shape(Concave \
         24))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         3d0ec1ea-1dac-4588-883c-e81e20c798df)(content(Whitespace\" \
         \"))))(Tile((id 23f8759f-f99f-4906-ab11-8ee65e4153a6)(label([ \
         ]))(mold((out Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape \
         Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
         a276ea40-e768-4e81-b1be-0bc5b65ff83e)(label(Int))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children()))))))))(Secondary((id \
         808b3334-7d6a-40e7-a8ef-f70d5f2944eb)(content(Whitespace\" \
         \")))))((Secondary((id \
         396ebc83-b0e7-43ea-815d-23980668639f)(content(Whitespace\" \
         \"))))(Tile((id \
         3a00e978-0cb1-4ea3-9d2e-f5ca0ab26601)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         3aa75033-23e0-4517-9799-9f63f4292772)(label(::))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 29))(sort Exp))((shape(Concave \
         29))(sort Exp))))))(shards(0))(children())))(Tile((id \
         10d40b42-f355-4d54-83fa-d9e5732f425c)(label([ ]))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         15a6d2db-3725-4192-bfec-21d40977307e)(label(2.0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         acfc9d6b-11b0-45b3-8a01-6e7dfed2f658)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         1fc618bd-7472-47d7-9d36-d4fd519c56e3)(content(Whitespace\" \
         \"))))(Secondary((id \
         8b0ada11-7b53-431e-82af-a79f2eeb17ad)(content(Comment \
         #err#))))(Secondary((id \
         671e35ab-a150-4028-8eea-f97030166e70)(content(Whitespace\"\\n\"))))(Tile((id \
         58c9af95-6893-45a8-b0bb-ff271a744940)(label(\"\\\"BYE\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))(ancestors())))(caret Outer))";
      backup_text =
        "# Internal Regression Tests: Type errors #\n\
         # Each line should show errors or not as indicated #\n\n\
         let _ = unbound in #err#\n\
         let Undefined = Undefined in # 2x err# \n\
         let true = 2 in #2x err# \n\n\
         let ? = if true then 1 else 1. in #err# \n\
         let _ = if true then 1 else 1. in #err#\n\
         let _: ? = if true then 1 else 1. in\n\
         let _: Int = if true then 1 else 1. in #err#\n\
         let _: Fake = if true then 1 else true in #err#\n\
         let _, _ = if true then 1 else 1. in #2x err#\n\
         let _, _ = (if true then 1 else 1.),   in #err#\n\
         let _: ?, _ = (if true then 1 else 1.),   in \n\
         let [_] = [(if true then 1 else 1.)] in #2x err#\n\
         let [_] = (if true then 1 else 1.) in #3x err# \n\n\
         (?)(if true then 1 else 1.);\n\
         1(if true then 1 else 1.); #err#\n\
         (1)(if true then 1 else 1.); #err#\n\
         (fun ? -> ?)(if true then 1 else 1.);\n\
         (fun _ -> ?)(if true then 1 else 1.);\n\
         (fun _: ? -> ?)(if true then 1 else 1.);\n\
         (fun _: Int -> ?)(if true then 1 else 1.); #err#\n\n\
         let _ = fun x -> if true then 1 else 1. in #err#\n\
         let _: ? = fun x -> if true then 1 else 1. in\n\
         let _: ? -> ?  = fun x -> if true then 1 else 1. in\n\
         let _: ? -> Int = fun x -> if true then 1 else 1. in #err#\n\
         let _: ? -> [?] = fun x -> if true then 1 else 1. in #2x err#\n\n\
         (?)::[(if true then 1 else 1.)];\n\
         1::[(if true then 1 else 1.)]; #err#\n\
         (1, 1)::[(if true then 1 else 1.)]; #2x err#\n\n\
         let ? = [1, 1., true] in #err: inconsistent#\n\
         let _ = [1, 1., true] in #err: inconsistent#\n\
         let _: ? = [1, 1., true] in \n\
         let _: [?] = [1, 1., true] in\n\
         let _: [Int] = [1, 1., true] in #2x err#\n\n\
         let _: [Int] = 1::[2] in\n\
         let _: [Int] = 1.0::[2] in #err#\n\
         let _: [Int] = 1::[2.0] in #err#\n\
         \"BYE\"";
    } )
