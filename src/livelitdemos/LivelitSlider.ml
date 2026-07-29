let out : string * Haz3lcore.PersistentSegment.t =
  ( "Define a Slider",
    {
      segment =
        "((Secondary((id \
         026c6bd0-e865-4a74-b294-21ed919708d0)(content(Comment\"# User-Defined \
         Livelits #\"))))(Secondary((id \
         419dc719-4c55-4cac-ac32-70e80ef4cfed)(content(Whitespace\"\\n\"))))(Secondary((id \
         bb22b5bf-b7c0-41d8-8606-6e63447f00a5)(content(Whitespace\"\\n\"))))(Secondary((id \
         e893b737-61ba-4429-9329-ab1b0dcc68b3)(content(Comment\"# A livelit is \
         a live GUI for a value in your program. Define one \
         #\"))))(Secondary((id \
         2ebe9b2b-c046-414d-9e7d-2e6bf30f40dc)(content(Whitespace\"\\n\"))))(Secondary((id \
         e8f2520d-3b0e-4a89-b4e4-f174fc2616f3)(content(Comment\"# with a \
         livelit name (^name) bound to a module: #\"))))(Secondary((id \
         0f98df77-bcc3-43fc-b740-3021f1519245)(content(Whitespace\"\\n\"))))(Secondary((id \
         96c70dbf-c668-4f17-84a4-e46a07a7fffa)(content(Whitespace\"\\n\"))))(Secondary((id \
         0df19f04-f00b-47e3-a018-e8571971f378)(content(Comment\"# init:   the \
         model a fresh use starts with #\"))))(Secondary((id \
         1ace4f52-9885-41d3-9b34-faaca5b499e4)(content(Whitespace\"\\n\"))))(Secondary((id \
         993d06cd-6f4d-4a57-9e77-9df9d16c6ab4)(content(Comment\"# update: \
         (Model, Action) -> Model, run when the GUI acts #\"))))(Secondary((id \
         a8e5604d-7df3-467b-86b1-81fb740879bd)(content(Whitespace\"\\n\"))))(Secondary((id \
         91964229-d516-4f16-874d-d7d6fb757b89)(content(Comment\"# view:   \
         Model -> HTML, whose handlers emit Actions #\"))))(Secondary((id \
         03342a2a-cf0c-4e9b-b044-b10fc1d72df3)(content(Whitespace\"\\n\"))))(Secondary((id \
         09b62bd5-70ba-43de-9a5c-709516c80cad)(content(Comment\"# expand: \
         Model -> value, what a use means to the program #\"))))(Secondary((id \
         e6364b12-f1c1-4f01-bbfe-de3cd9b24220)(content(Whitespace\"\\n\"))))(Secondary((id \
         e8a4a064-ba93-4d71-9016-dd21ad5dc62b)(content(Whitespace\"\\n\"))))(Tile((id \
         912b3645-d8df-46ee-a2ee-74754b372856)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         da251341-e050-4b12-953c-2df0ab68882e)(content(Whitespace\" \
         \"))))(Tile((id \
         db5a2c93-5693-4be7-b486-c56db4ac7149)(label(^pct))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         47a8c8bc-0a6d-4c4c-8b2f-6fc0dd95ef47)(content(Whitespace\" \
         \")))))((Secondary((id \
         f5cdacce-fedd-42f6-bab9-6f9f581fbcc1)(content(Whitespace\" \
         \"))))(Tile((id 414c6c5e-a720-49c0-8c41-f89924f1f70b)(label({ \
         }))(mold((out Exp)(in_(Mod))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         4b50a7c7-db53-4520-b82b-49c0eff1b8a9)(content(Whitespace\"\\n\"))))(Tile((id \
         9ba5917a-cee3-47fc-a59e-1527b3ee80b7)(label(type =))(mold((out \
         Mod)(in_(TPat))(nibs(((shape Convex)(sort Mod))((shape(Concave \
         45))(sort Typ))))))(shards(0 1))(children(((Secondary((id \
         fc9f77f5-1390-4bca-a446-c702a3304174)(content(Whitespace\" \
         \"))))(Tile((id \
         e19594df-6805-483a-bb19-929c128f5d05)(label(Model))(mold((out \
         TPat)(in_())(nibs(((shape Convex)(sort TPat))((shape Convex)(sort \
         TPat))))))(shards(0))(children())))(Secondary((id \
         9a420795-3995-404a-8c6f-febb48236ba1)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         db892f43-22eb-486d-90aa-764bf5393a79)(content(Whitespace\" \
         \"))))(Tile((id \
         96fc6e95-e650-4d27-9c89-4382e9ab7fd4)(label(Int))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         34265747-c147-4d2a-a74d-e5144f000d82)(label(\";\"))(mold((out \
         Mod)(in_())(nibs(((shape(Concave 47))(sort Mod))((shape(Concave \
         47))(sort Mod))))))(shards(0))(children())))(Secondary((id \
         fe20cdcc-1b97-498d-b367-5be233c56422)(content(Whitespace\"\\n\"))))(Tile((id \
         28edc523-f71e-49f9-933c-ba99d48897a0)(label(type =))(mold((out \
         Mod)(in_(TPat))(nibs(((shape Convex)(sort Mod))((shape(Concave \
         45))(sort Typ))))))(shards(0 1))(children(((Secondary((id \
         05da81f6-a379-4a9a-b8c9-844f5830b341)(content(Whitespace\" \
         \"))))(Tile((id \
         744cfeac-837c-44b4-aee6-6f18c262aac1)(label(Action))(mold((out \
         TPat)(in_())(nibs(((shape Convex)(sort TPat))((shape Convex)(sort \
         TPat))))))(shards(0))(children())))(Secondary((id \
         f23d0b7b-09f9-447b-844e-d156c487fe60)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         a0154b19-5802-477b-a945-75e0ac22e539)(content(Whitespace\" \
         \"))))(Tile((id \
         6c3da1f5-6aa1-48e2-9941-31a6761ebea1)(label(Int))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         9d352010-2478-44e4-ba4b-6b73e541f2dc)(label(\";\"))(mold((out \
         Mod)(in_())(nibs(((shape(Concave 47))(sort Mod))((shape(Concave \
         47))(sort Mod))))))(shards(0))(children())))(Secondary((id \
         cc68ff44-36fa-401e-8489-e7efb598fbbb)(content(Whitespace\"\\n\"))))(Secondary((id \
         c34357d8-6bdb-4885-8d42-2981aa71dce6)(content(Whitespace\"\\n\"))))(Tile((id \
         7f4dad1d-71b7-4ded-9111-0ba34a8d0e4a)(label(let =))(mold((out \
         Mod)(in_(Pat))(nibs(((shape Convex)(sort Mod))((shape(Concave \
         45))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         be464740-23be-4b4f-93da-6036566a6ba8)(content(Whitespace\" \
         \"))))(Tile((id \
         19633ce5-965d-4e23-a787-1496745e296f)(label(init))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         c9ad2b00-d031-4f07-9e56-2299e6d8438d)(content(Whitespace\" \
         \"))))(Tile((id \
         df7713b5-0473-422b-8d73-e5dcca94fc2f)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 24))(sort Pat))((shape(Concave \
         24))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         e809b78e-699d-42ef-9660-e606a14c3e83)(content(Whitespace\" \
         \"))))(Tile((id \
         5c35282b-814a-44b2-8aa3-de1bf13afefd)(label(Model))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         dea3f2d2-e153-45aa-a2a9-2ef447bb871c)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         6a790656-e5f2-42f8-8351-98536eab0772)(content(Whitespace\" \
         \"))))(Tile((id \
         b83e863f-6d37-4265-b8a0-d08c99634792)(label(50))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         865c4e51-17e5-46bc-b7e0-8462718124fd)(label(\";\"))(mold((out \
         Mod)(in_())(nibs(((shape(Concave 47))(sort Mod))((shape(Concave \
         47))(sort Mod))))))(shards(0))(children())))(Secondary((id \
         c8075215-cdf6-4cd5-ac3d-df70db8aea62)(content(Whitespace\"\\n\"))))(Secondary((id \
         59e4c3af-590d-48d4-8f2a-cc51dc9b5b74)(content(Whitespace\"\\n\"))))(Tile((id \
         6187813e-b36a-4d28-955b-f3cbd59dc657)(label(let =))(mold((out \
         Mod)(in_(Pat))(nibs(((shape Convex)(sort Mod))((shape(Concave \
         45))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         9471ea7a-5264-492c-9b2f-55cefc099500)(content(Whitespace\" \
         \"))))(Tile((id \
         aa928089-08fc-40cf-9841-0dbfd5fd79af)(label(update))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         4d9dc313-c7b7-40a0-bcb7-7606c769efa4)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         1ce7b7c7-c40c-431a-8951-7869e46be126)(content(Whitespace\" \
         \"))))(Tile((id 9bfa9df3-0550-45e0-bcd7-6d071bfada64)(label(fun \
         ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 37))(sort Exp))))))(shards(0 \
         1))(children(((Secondary((id \
         0228452b-204f-4588-998e-018edfbdd723)(content(Whitespace\" \
         \"))))(Tile((id \
         4ffb6088-c8d0-4e99-9aa8-1b3d9ddbaae7)(label(\"(\"\")\"))(mold((out \
         Pat)(in_(Pat))(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0 1))(children(((Tile((id \
         547777fa-2e93-4821-864e-8bd65c1297c0)(label(m))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         6d191e35-902e-4592-af6a-80527cc17682)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 44))(sort Pat))((shape(Concave \
         44))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         2b4cfc1e-fac1-457b-b254-da2e5c1859a0)(content(Whitespace\" \
         \"))))(Tile((id \
         f9bb0809-2f51-4394-b742-ba91f6da7b15)(label(a))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children()))))))))(Secondary((id \
         6ae64c1a-6096-47bb-8409-ad5934f9c585)(content(Whitespace\" \
         \"))))(Tile((id \
         cf91cd48-7aa4-408c-8324-db545400c73b)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 24))(sort Pat))((shape(Concave \
         24))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         25e81e4e-94e1-48f8-88c5-302b0ee7d131)(content(Whitespace\" \
         \"))))(Tile((id \
         80c0115e-675b-4aca-959f-c3227554bbee)(label(\"(\"\")\"))(mold((out \
         Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0 1))(children(((Tile((id \
         03c9031a-74e5-438c-a780-17134032dfe1)(label(Model))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         0829eecb-171c-4c87-b9c9-dcc247f5f245)(label(,))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 44))(sort Typ))((shape(Concave \
         44))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         0f44b49e-bf71-439d-bf37-c9b40d75d931)(content(Whitespace\" \
         \"))))(Tile((id \
         bfde1ae5-bc97-413e-b53f-2638bf2e0b6e)(label(Action))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children()))))))))(Secondary((id \
         dfb7f395-05c4-4fd2-bffb-e95ca7c9991e)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         1ff173bf-2439-4d0e-b8f3-58405c049958)(content(Whitespace\" \
         \"))))(Tile((id \
         dfb8b825-7da3-4617-b882-f4b962514b86)(label(a))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         d8d56d70-73dd-413c-9c36-9faa182919bf)(label(\";\"))(mold((out \
         Mod)(in_())(nibs(((shape(Concave 47))(sort Mod))((shape(Concave \
         47))(sort Mod))))))(shards(0))(children())))(Secondary((id \
         93a49353-1787-4d18-ba44-8fcf45772cc8)(content(Whitespace\"\\n\"))))(Secondary((id \
         ee20b3ce-51a9-4bb0-b390-ff238bf81d8c)(content(Whitespace\"\\n\"))))(Tile((id \
         78b37593-1aad-4442-b471-7bbe9f455891)(label(let =))(mold((out \
         Mod)(in_(Pat))(nibs(((shape Convex)(sort Mod))((shape(Concave \
         45))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         75d85c7a-2b10-4209-b870-366e368ba256)(content(Whitespace\" \
         \"))))(Tile((id \
         1a86a2cd-3f8f-4e45-8c41-0b0e9e2109b2)(label(view))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         ad528971-e968-4aba-b147-8726373b020a)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         43e56e16-5789-4bd4-9a18-d4bc1b633593)(content(Whitespace\" \
         \"))))(Tile((id 0e5ec5e7-f775-4768-8c3a-a3639b624873)(label(fun \
         ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 37))(sort Exp))))))(shards(0 \
         1))(children(((Secondary((id \
         4f62529d-134f-49fa-b771-f07b18235ec7)(content(Whitespace\" \
         \"))))(Tile((id \
         18a29555-7b2b-421b-91b2-78c58e353b30)(label(m))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         7a1f45b0-fc19-4ba1-ae0e-5889863ea44e)(content(Whitespace\" \
         \"))))(Tile((id \
         690eee49-a431-46fb-b592-a37a25a7ebc6)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 24))(sort Pat))((shape(Concave \
         24))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         fed73790-8dd7-4650-bbe1-bdd1ad111fb7)(content(Whitespace\" \
         \"))))(Tile((id \
         ebf8548a-5bc1-4f53-8084-b7b47c553d23)(label(Model))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         92ff9dc7-f1e9-42d0-9483-fb0404495ea6)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         89f5f4a6-c7e8-49a9-861b-065b710ef0db)(content(Whitespace\"\\n\"))))(Tile((id \
         a52b4e21-a50e-4a3a-82e3-c04b3d50a7e3)(label(Div))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         4761127f-602b-4a3d-b691-617899205c60)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         3e86fb66-a6f7-4e15-a316-ee2bdd369aa2)(content(Whitespace\"\\n\"))))(Tile((id \
         54f757aa-b18e-4d46-ae7a-70daece95471)(label([ ]))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         1a103b9f-9791-471e-8c1f-7cb36334f3cf)(label(Style))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         6373f7a9-e23f-4849-833c-ca2f065a8362)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         0b54d4da-ae78-423c-a5f1-4a9420dd5b80)(label([ ]))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         8df0e576-de7a-4106-9ed2-d54854217366)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         0e7253b1-9db3-4f13-92f5-3bfd481b8c22)(label(\"\\\"display\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         adeee252-d844-41ca-8f90-b9ef95ff3d78)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         3e3cc40f-53ef-4bc5-8b35-81d39b934eb2)(content(Whitespace\" \
         \"))))(Tile((id \
         8df59d11-1a40-4717-b9e6-3cd82322effd)(label(\"\\\"flex\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         c6ec2b18-be5a-448a-aef4-70a9ccdc5384)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         3452a747-5149-41d5-8943-f075d94788ad)(content(Whitespace\" \
         \"))))(Tile((id \
         fba32c15-76fe-4005-b585-658d272ba418)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         578a62dd-18eb-4cae-8576-e49997969e8b)(label(\"\\\"gap\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         31fbbcc7-b55a-4f91-bbaa-0a310e614ff2)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         533a4c66-b3df-4497-98e9-eaf06248a8e0)(content(Whitespace\" \
         \"))))(Tile((id \
         db0cb51e-d74c-4de5-aa55-4febd05ca7f9)(label(\"\\\"0.5em\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         48b5ed76-f20b-4b15-b33a-8e680f23970f)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         d9aa8974-db54-4881-b248-16e9908e5b68)(content(Whitespace\"\\n\"))))(Tile((id \
         46afa547-c561-4e14-9cfa-8e816845cf3f)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         fcffd7fe-7a78-4f86-900d-b619e850bd68)(label(\"\\\"align-items\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         39c5c097-4a69-4f7f-b905-31a601c39ae7)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         d1cbf251-3841-4482-8803-e3e5c09bed3c)(content(Whitespace\" \
         \"))))(Tile((id \
         ba8d5e2b-4840-4c4c-b3c4-57855062f867)(label(\"\\\"center\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         3f7eaef9-c4ef-4caa-a2d0-4513bf71ddd6)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         4565a67e-5785-4165-b454-0223fbb880e6)(content(Whitespace\"\\n\"))))(Tile((id \
         bd20bc97-3a42-4720-9cb8-a38cfcfa18c4)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         4ef4befd-633f-4be8-afac-9464816aac48)(label(\"\\\"font-family\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         93e8f884-44d3-4ac5-aa2f-5168a93a7174)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         e3785327-5f47-47eb-8e7a-929b9b4a99da)(content(Whitespace\" \
         \"))))(Tile((id \
         2d7642a4-b2fc-4359-8626-c39642b6cdd3)(label(\"\\\"var(--code-font)\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         549ffcc5-afd3-422a-8318-4833ab1a3a33)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         6825adc0-7843-45c3-939a-b778c207c1a6)(content(Whitespace\"\\n\"))))(Tile((id \
         275f71ed-5fe0-4c8f-8976-da63b6a5fa8e)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         e8d1714f-129a-4aeb-b63f-8da7ecf92895)(label(\"\\\"font-size\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         4e4d1fcb-4aa4-43f0-9532-52b95362164d)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         00186b7f-95a3-4965-8be5-51884d2e4b2c)(content(Whitespace\" \
         \"))))(Tile((id \
         91a3ef83-9f26-478f-97eb-e3dd8584559e)(label(\"\\\"0.8em\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))))))))))))))))))))))(Tile((id \
         5b05e569-cbd7-431f-8e9f-60150c780cb9)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         d4eb1738-b5e1-4708-9813-18c60fe35e97)(content(Whitespace\"\\n\"))))(Tile((id \
         00ed9b2d-fcf4-48ac-b0ab-ed36a0216abc)(label([ ]))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         dbc0b6c3-0a51-4bca-bddb-5e976812312b)(content(Whitespace\"\\n\"))))(Tile((id \
         0f5e57e7-e301-418f-a7a2-1532c7d4769e)(label(Input))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         250ca2fa-653e-48a1-8f0f-48a6e3952eda)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         61aef4f3-088d-4f99-b431-aa49bbe5dac0)(label([ ]))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         fc802724-7fb7-4848-9b6f-7166a8466498)(content(Whitespace\"\\n\"))))(Tile((id \
         ed540325-cd56-46b8-997f-b93309af84ea)(label(Type))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         324b1d36-8567-43ad-b2e6-35ec3b4ca756)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         0d0e66dd-9f1d-4fca-b285-e849a572a10b)(label(\"\\\"range\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         b98cb112-fbad-4b58-a741-a18279118c33)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         cd113ced-ec6b-494c-8cd5-ce79b046bb2e)(content(Whitespace\" \
         \"))))(Tile((id \
         242dd47c-dd4a-4c0d-a70c-6ac45f26172a)(label(Min))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         499304d9-38ec-4fde-bf61-d959b82fe7f0)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         2d3f6969-e6cf-4d67-84b5-52989c277d81)(label(\"\\\"0\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         7dd55338-c644-44f9-98df-d5564b0295b5)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         666046fd-cb30-47c9-8385-8331c90492a9)(content(Whitespace\" \
         \"))))(Tile((id \
         d52a0ab7-0af2-4bc9-b1c0-6242e1f26407)(label(Max))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         85c2c075-90c3-44b9-a43d-328bb453c212)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         4c87a83b-59c8-4eba-8b4e-2cf5d4702862)(label(\"\\\"100\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         36dd03dc-bd5e-4a71-9bde-770462b93808)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         64e86ef4-3291-4f46-a84c-74d87e48bc9b)(content(Whitespace\"\\n\"))))(Tile((id \
         7eb82f6d-e5f0-4db4-8988-ca360e437d03)(label(Value))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         57f05628-4ed6-4139-808c-93c54886c273)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         8638e9ea-9496-4eff-83c2-61120a578e5a)(label(string_of_int))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         ab5a3ebd-b0ae-4e13-9fc9-3c38031ce441)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         db1d2228-f10c-480e-87f8-8300555ca3b4)(label(m))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))))))))))))(Tile((id \
         ec69f21f-a01a-489b-89bd-83052722df22)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         03fa1a74-4696-4e1e-926f-e73751379ca5)(content(Whitespace\"\\n\"))))(Tile((id \
         8821adef-7cf9-4ec2-9904-8f707a03f4a4)(label(OnInput))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         01c94431-57e0-4eaa-a328-ba3098d83fe4)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         5c76aeff-7e68-4a81-8d26-e5b6f54a24bd)(label(fun ->))(mold((out \
         Exp)(in_(Pat))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         37))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         3b32d1be-9a74-4dd6-ba6d-b0369f3ccf2b)(content(Whitespace\" \
         \"))))(Tile((id \
         936ed296-dd6e-4465-bc3a-dac1607ba25a)(label(s))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         776d6e12-0208-4146-bd48-dde9a3d061c0)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         8bc20444-a525-4957-8ee9-208f3aca984a)(content(Whitespace\" \
         \"))))(Tile((id \
         8f8004f1-a649-49dd-ada6-979ad4338b4b)(label(int_of_string))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         92fb0d04-46d4-4e23-b823-881cf3ca984f)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         21d75536-c15a-40ec-954a-021640061702)(label(s))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))))))))))))(Secondary((id \
         fe9fa544-f6c3-42f6-9318-cc5f940e1cdb)(content(Whitespace\"\\n\"))))))))))))))(Tile((id \
         fbf1d111-c57b-478a-877e-19aa576d1acc)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         fa7a91ca-ba05-473f-a29c-90f6bc515f6a)(content(Whitespace\"\\n\"))))(Tile((id \
         fc811da9-ce7b-4039-b669-f43554b74ba0)(label(Text))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         3bee009c-8968-4dc3-92f5-5955dae98b11)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         041816e7-166c-4ef7-9a79-46e9ff38eb68)(label(string_of_int))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         6ef81360-2ba5-4a7a-91fc-23b5e0999752)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         4a251e7f-92df-4aed-a33e-2e8d77819b2c)(label(m))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         e756854f-ce9a-4654-b2c3-243d714702ff)(content(Whitespace\" \
         \"))))(Tile((id \
         a9fd32cf-c9c6-4423-bb21-bf240ad9956f)(label(++))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 30))(sort Exp))((shape(Concave \
         30))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         4c42d155-09d1-43f5-a4b6-4a4ea1e3dcb2)(content(Whitespace\" \
         \"))))(Tile((id \
         0848cfba-929b-42db-9621-63a86471291e)(label(\"\\\"%\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         49cc5361-c345-4c47-9350-502b9df5f544)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         b7b58475-342d-4392-ad5e-cbbc1e24d547)(content(Whitespace\"\\n\")))))))))(Tile((id \
         1a0b8b25-07ec-4231-bff5-72404dcde7dc)(label(\";\"))(mold((out \
         Mod)(in_())(nibs(((shape(Concave 47))(sort Mod))((shape(Concave \
         47))(sort Mod))))))(shards(0))(children())))(Secondary((id \
         1e6ab3d0-8489-4ee0-a010-6e8e218c0c17)(content(Whitespace\"\\n\"))))(Secondary((id \
         04253cd2-4606-48a4-8fa7-739ee771f0c5)(content(Whitespace\"\\n\"))))(Tile((id \
         789dde05-a9c2-4810-bc5b-e0eb4a72aced)(label(let =))(mold((out \
         Mod)(in_(Pat))(nibs(((shape Convex)(sort Mod))((shape(Concave \
         45))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         1c591199-9fb7-4345-860a-de4d56ed7d0d)(content(Whitespace\" \
         \"))))(Tile((id \
         a4c88b53-4a84-4387-93d1-3a6458f37421)(label(expand))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         86a05d74-424a-4a13-b3b6-5bb03296b7bc)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         f79dca8e-6685-4ae2-8d3e-c46202680ba0)(content(Whitespace\" \
         \"))))(Tile((id 15e477be-e233-40c9-b3ed-17bbd422475d)(label(fun \
         ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 37))(sort Exp))))))(shards(0 \
         1))(children(((Secondary((id \
         cf3af713-552e-489a-ac12-f0ab5c78e7e4)(content(Whitespace\" \
         \"))))(Tile((id \
         802fd165-d9b1-487a-b532-0c4c9ceafcb7)(label(m))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         c4bf5715-0a14-4b2a-b90c-ab3c12898182)(content(Whitespace\" \
         \"))))(Tile((id \
         21e14e27-230b-4265-89d0-33ea5cdf6b25)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 24))(sort Pat))((shape(Concave \
         24))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         455d7459-2f33-40e4-b4d8-43a5ff5c8a5f)(content(Whitespace\" \
         \"))))(Tile((id \
         a023ff8b-1f8b-46c6-a087-486e786c3e91)(label(Model))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         6f4fb838-8d7d-4baf-86d2-541c15d46d04)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         ca8a237b-76bb-4da1-b673-7f1d71d13c57)(content(Whitespace\" \
         \"))))(Tile((id \
         445d0653-15f6-400e-a495-7cb90b394c20)(label(m))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         1d10ae62-212d-4653-a433-58da16fb7376)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         b74ecd21-497c-4043-82cd-d4a39b2d5539)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         f6d27ff7-074a-4ce0-acf6-bfe5471b5125)(content(Whitespace\"\\n\"))))(Secondary((id \
         f7e1e489-3116-4a4d-8329-7342d8ddb01f)(content(Whitespace\"\\n\"))))(Secondary((id \
         26d6d237-7aeb-4676-aaf8-7fe56baa2ac5)(content(Comment\"# Each use \
         below carries its own model, stored right here in the \
         #\"))))(Secondary((id \
         fe70cee7-0f0b-4a6b-b68c-d683e4856b19)(content(Whitespace\"\\n\"))))(Secondary((id \
         5cbb5d9a-9eff-43b6-b83d-14e9aa1d043c)(content(Comment\"# text: drag a \
         slider and watch its argument change. #\"))))(Secondary((id \
         2b888d4f-0d02-4ad9-b4ad-e3e0cb05faf7)(content(Whitespace\"\\n\"))))(Secondary((id \
         4515f92c-3647-40ac-92e5-fa571a08d293)(content(Whitespace\"\\n\"))))(Projector((id \
         52bfd88d-56f7-4b4c-8584-1d346b236c24)(kind Livelit)(syntax(Tile((id \
         2f7af382-db07-4e66-b253-d363900dceef)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         333c62aa-2f26-4477-b3ab-5544f776d474)(label(^pct))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         8a509f0f-4b27-474b-9499-e5e867e6b6d2)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         16163076-f250-407e-85b3-8059c5149a88)(label(25))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))))))))(model\"()\")(placement \
         Inline)))(Secondary((id \
         b0304a1f-9a61-4539-a3df-ac3a03e2fba8)(content(Whitespace\" \
         \"))))(Tile((id \
         2e43f9f9-bcf0-4f6d-a643-1c1b7a29b470)(label(+))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 28))(sort Exp))((shape(Concave \
         28))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         c65bb687-b9dd-4ba4-bfc1-7a73392ceb29)(content(Whitespace\" \
         \"))))(Projector((id dc3dc7ee-74e4-4736-82cf-04cf89154eb0)(kind \
         Livelit)(syntax(Tile((id \
         d8b4b559-de0a-47fc-bc92-1ddffb8f12e4)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         7cdd8a07-161d-4e17-90f0-7284a6b97c4e)(label(^pct))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         82c233ad-f7d3-41f5-9b64-5df25adec1f3)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         91402baf-22d8-4ea9-8253-d51aae1dae57)(label(75))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))))))))(model\"()\")(placement \
         Inline)))(Secondary((id \
         fe1d8e85-ddb5-40e2-9202-77ad9f4b8faa)(content(Whitespace\"\\n\")))))";
      backup_text =
        "# User-Defined Livelits #\n\n\
         # A livelit is a live GUI for a value in your program. Define one #\n\
         # with a livelit name (^name) bound to a module: #\n\n\
         # init:   the model a fresh use starts with #\n\
         # update: (Model, Action) -> Model, run when the GUI acts #\n\
         # view:   Model -> HTML, whose handlers emit Actions #\n\
         # expand: Model -> value, what a use means to the program #\n\n\
         let ^pct = {\n\
         type Model = Int;\n\
         type Action = Int;\n\n\
         let init : Model = 50;\n\n\
         let update = fun (m, a) : (Model, Action) -> a;\n\n\
         let view = fun m : Model ->\n\
         Div(\n\
         [Style([(\"display\", \"flex\"), (\"gap\", \"0.5em\"),\n\
         (\"align-items\", \"center\"),\n\
         (\"font-family\", \"var(--code-font)\"),\n\
         (\"font-size\", \"0.8em\")])],\n\
         [\n\
         Input([\n\
         Type(\"range\"), Min(\"0\"), Max(\"100\"),\n\
         Value(string_of_int(m)),\n\
         OnInput(fun s -> int_of_string(s))\n\
         ]),\n\
         Text(string_of_int(m) ++ \"%\")\n\
         ]\n\
         );\n\n\
         let expand = fun m : Model -> m\n\
         } in\n\n\
         # Each use below carries its own model, stored right here in the #\n\
         # text: drag a slider and watch its argument change. #\n\n\
         ^^livelit(^pct(25)) + ^^livelit(^pct(75))\n";
      refractors = "()";
    } )
