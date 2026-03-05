let out : string * Haz3lcore.PersistentSegment.t =
  ( "Study / tutorial / 01-fundamentals",
    {
      segment =
        "((Secondary((id \
         9ac6d11f-18b3-47ab-bfb5-ca1180596279)(content(Comment\"# PROBES \
         TUTORIAL - PART 1: FUNDAMENTALS #\"))))(Secondary((id \
         80c3ef4e-1b7e-4255-939c-3bdbf0cb8d5a)(content(Whitespace\"\\n\"))))(Secondary((id \
         87d7e705-aed2-405f-8900-f9600df24922)(content(Comment\"# Probes show \
         you runtime values of expressions, inline in the editor. \
         #\"))))(Secondary((id \
         3db75104-cc78-4371-b02c-451cb2621387)(content(Whitespace\"\\n\"))))(Secondary((id \
         5f03567c-2abc-4a8c-a373-027c2b7148c1)(content(Whitespace\"\\n\"))))(Secondary((id \
         a9e87b88-5af7-42d7-8ed9-de9a819d6bae)(content(Comment\"# TRY THIS: \
         Put your cursor on the `*` in `250 * 7` below and \
         #\"))))(Secondary((id \
         03d1bfe9-afd4-4420-9bbd-7cdff2286c85)(content(Whitespace\"\\n\"))))(Secondary((id \
         40d45524-3ed5-40ed-bb34-94aaa88c275f)(content(Comment\"# right-click \
         to open the context menu and select \\\"Add probe\\\". \
         #\"))))(Secondary((id \
         7e625a86-7b24-4d11-81bf-81b5825ed00d)(content(Whitespace\"\\n\"))))(Secondary((id \
         ef764a04-8bb7-4940-8cd1-3a1d186536d2)(content(Comment\"# You should \
         see the sample value 1750 to the right. #\"))))(Secondary((id \
         2bc6090f-2905-4685-8347-bfc330f24afc)(content(Whitespace\"\\n\"))))(Secondary((id \
         04ff0ee8-4887-44ad-8aad-f727d232b452)(content(Whitespace\"\\n\"))))(Secondary((id \
         01d6b194-03b5-485f-95e8-67bf42485f6b)(content(Comment\"# Hazel uses \
         `let x = expr in body` to define values. #\"))))(Secondary((id \
         cc99d60b-a9c1-4bea-ae95-1172fb594fc6)(content(Whitespace\"\\n\"))))(Tile((id \
         ce8214b5-6db4-44bd-b8fd-8389364f0271)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         2581a8e2-e5b2-428e-8fcd-1e81bb6935f8)(content(Whitespace\" \
         \"))))(Tile((id \
         52a41ad2-8120-439c-8b02-7d60d3c099ba)(label(weekly_water))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         5a963f63-fab7-4c94-aa7a-50dd87c973ec)(content(Whitespace\" \
         \")))))((Secondary((id \
         9ce552ea-63da-4a1a-962c-71840ab43534)(content(Whitespace\" \
         \"))))(Tile((id \
         68f9b691-e539-4d68-87a0-b0e7c20fb730)(label(250))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         2b261a31-2fa7-4cd6-8800-8addbe6ad515)(content(Whitespace\" \
         \"))))(Tile((id \
         51fb1dec-864e-40ef-8a59-2769187b815e)(label(*))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 27))(sort Exp))((shape(Concave \
         27))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         613f498d-17e8-4424-af91-6bafe0ecc544)(content(Whitespace\" \
         \"))))(Tile((id \
         9bd6792b-ee00-4e11-9d97-aca5fbefd359)(label(7))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         bcfad242-d37e-4acc-865e-08cbd423c463)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         8f4795d2-4a03-4607-bbf3-72a4b4d421d0)(content(Whitespace\"\\n\"))))(Secondary((id \
         db06970b-20c0-4003-a7a6-78f260c4b9d8)(content(Whitespace\"\\n\"))))(Secondary((id \
         2a4fb662-b935-4fa3-8c26-5f98d8ada5ed)(content(Comment\"# TRY THIS: \
         You can also press Cmd/Ctrl+E to toggle that probe. \
         #\"))))(Secondary((id \
         7f96d1f6-7f42-40f7-9a3c-1b1ffcf5ff12)(content(Whitespace\"\\n\"))))(Secondary((id \
         7f11eb5d-0ca8-4cbb-bfe7-087e72bf10f3)(content(Whitespace\"\\n\"))))(Secondary((id \
         63773039-7df6-452f-944e-13d57cdd64b0)(content(Comment\"# You can also \
         probe variable names on the left side of a let binding. \
         #\"))))(Secondary((id \
         06d9ee72-50c0-4fa4-a463-1f067697a04d)(content(Whitespace\"\\n\"))))(Secondary((id \
         6eac406d-ad4b-4b9d-ade0-3abe6e3487b0)(content(Comment\"# Try adding a \
         probe to `daily_ml` on the left of the `=` sign below. \
         #\"))))(Secondary((id \
         e413ee47-ffec-4159-91c1-1a5529a500ec)(content(Whitespace\"\\n\"))))(Secondary((id \
         fe0b9d2d-793b-4e36-8ed1-5b97b2ec1635)(content(Whitespace\"\\n\"))))(Tile((id \
         21b7c610-6f2c-4853-8242-aff1239998fb)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         91589cee-c69b-440a-b7a0-b60f34877261)(content(Whitespace\" \
         \"))))(Tile((id \
         99b9abd3-e3ce-44f8-8a2d-763f47f7768b)(label(daily_ml))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         8a6a9f9f-e71e-414d-9134-bd630c9bbba2)(content(Whitespace\" \
         \")))))((Secondary((id \
         22fedd90-12f9-4669-bf01-92078b8659d2)(content(Whitespace\" \
         \"))))(Tile((id \
         702ff304-1e7e-459d-b898-cfaca5d19533)(label(250))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         0f384d5a-d8c4-473a-82c7-e604a80bb461)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         72ea4852-56e9-48a2-8d84-74b6df72b6e2)(content(Whitespace\"\\n\"))))(Tile((id \
         aee82b0e-c470-4d92-89fb-0010b266a6a0)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         80e48d8e-84ae-4e61-be47-1844e8fbf815)(content(Whitespace\" \
         \"))))(Tile((id \
         621a9eab-d125-4d79-ae03-451e50ff117f)(label(days))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         dbc755bc-c749-4e6d-952d-b83870ad3e5f)(content(Whitespace\" \
         \")))))((Secondary((id \
         3df6cefd-3216-4b3d-98e1-1e73b72f9145)(content(Whitespace\" \
         \"))))(Tile((id \
         796553a7-0c4d-487a-a9ee-ba6b9f8af8fd)(label(7))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         18e53863-fcf1-4305-b9d1-327eafdd5108)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         62b2d3bb-f7b8-4a64-ba6a-9c9c63d4ede4)(content(Whitespace\"\\n\"))))(Tile((id \
         405a3e35-d4a0-4f08-b68b-8d750db548bf)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         bc977cca-a24d-4911-8c45-5a961a00a9c5)(content(Whitespace\" \
         \"))))(Tile((id \
         679e1581-6a16-4541-8f51-11a553b66f07)(label(total))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         c01af8ed-0d0e-4f93-b354-cdbc1449b503)(content(Whitespace\" \
         \")))))((Secondary((id \
         3200c86e-9d73-4484-9a95-3de6d1fa4d3c)(content(Whitespace\" \
         \"))))(Tile((id \
         bbd59616-d9c2-48f2-8146-405e635cc854)(label(daily_ml))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         2254301c-3923-42b1-999f-128ca971ffc3)(content(Whitespace\" \
         \"))))(Tile((id \
         03bc661d-3900-41ef-9ebf-c50416b45147)(label(*))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 27))(sort Exp))((shape(Concave \
         27))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         41b16b71-ef95-435d-a5ca-ceffb8abda6b)(content(Whitespace\" \
         \"))))(Tile((id \
         d33a23c7-c2ed-49b1-91e0-a5857b1816d1)(label(days))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         1e7090e1-0504-4d98-953e-3b09d306969e)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         3189880f-25f7-4088-8146-4e5816da8775)(content(Whitespace\"\\n\"))))(Secondary((id \
         6561aa9c-d291-4cd7-9603-b6d6b38651b1)(content(Whitespace\"\\n\"))))(Secondary((id \
         94bfea35-c720-4704-b6d8-7fb8af8e007d)(content(Comment\"# Now click on \
         the sample value (1750). A dropdown shows the #\"))))(Secondary((id \
         26cd8cbf-27a7-4b7b-b143-add3eb756c72)(content(Whitespace\"\\n\"))))(Secondary((id \
         a4f85829-4f51-4c6c-90e2-50a4252b81ff)(content(Comment\"# ENVIRONMENT: \
         the variables in the expression and their values. \
         #\"))))(Secondary((id \
         3f70612b-94ab-4799-8f59-829cac4b3f33)(content(Whitespace\"\\n\"))))(Secondary((id \
         226d8fca-6fba-4112-84b0-83d60d36155d)(content(Whitespace\"\\n\"))))(Secondary((id \
         a60e4418-8a96-4f27-b5dc-95ac555e6de4)(content(Comment\"# `type` \
         defines type aliases. Sum types list options with `+`: \
         #\"))))(Secondary((id \
         ce3e0974-a0da-44f1-ba48-6404168c90cc)(content(Whitespace\"\\n\"))))(Tile((id \
         0a99860a-e046-48b2-a216-0e5624df24dc)(label(type = in))(mold((out \
         Exp)(in_(TPat Typ))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         94df61ed-a15f-4d5a-8e62-c3afc4dc180c)(content(Whitespace\" \
         \"))))(Tile((id \
         fae935c6-b6f3-4dd4-b4bb-6215611c8de4)(label(MoonPhase))(mold((out \
         TPat)(in_())(nibs(((shape Convex)(sort TPat))((shape Convex)(sort \
         TPat))))))(shards(0))(children())))(Secondary((id \
         4f607b0f-2edf-4d35-a482-6726653a795b)(content(Whitespace\" \
         \")))))((Secondary((id \
         7f259076-b0db-4417-9b7b-2f21c69f4e13)(content(Whitespace\" \
         \"))))(Tile((id \
         3f47a2e9-7af2-4146-b013-41b823ec00fd)(label(New))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         6119465a-8141-4ce0-aea5-70c567378a4e)(content(Whitespace\" \
         \"))))(Tile((id \
         0858105c-8e96-47bc-bbc6-ebf066f8b5ce)(label(+))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 12))(sort Typ))((shape(Concave \
         12))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         14cec724-0a68-427f-a80c-aaffba062ce4)(content(Whitespace\" \
         \"))))(Tile((id \
         61ff455b-57a3-4d8b-87ca-78e549902946)(label(Waxing))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         45ec8dd3-c70d-4342-9773-d0ecbc815e38)(content(Whitespace\" \
         \"))))(Tile((id \
         0864118c-0fcf-4e72-8e12-3d2a1c05c1ec)(label(+))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 12))(sort Typ))((shape(Concave \
         12))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         7fd319a5-d5c7-419f-b772-fa4dc5f614ed)(content(Whitespace\" \
         \"))))(Tile((id \
         468bf02a-8822-4698-9181-e75942930c41)(label(Full))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         44440d98-fbd4-4183-8db5-5b1170fd9ab3)(content(Whitespace\" \
         \"))))(Tile((id \
         0b0c512c-278f-48c4-a6cd-325bedc307a6)(label(+))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 12))(sort Typ))((shape(Concave \
         12))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         b4d21179-ee9e-492b-9b03-6bef7df4eaed)(content(Whitespace\" \
         \"))))(Tile((id \
         38c7fb29-82fd-4595-9af8-cbab5ed5a21b)(label(Waning))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         4c814adc-823b-419a-8ff0-a6dfdbea3726)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         f139b12f-fa3c-47d8-af91-e96a90b7b07f)(content(Whitespace\"\\n\"))))(Tile((id \
         738b48db-14ef-412e-9ec4-ded23abf1ba7)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         0e28d461-26df-4c4d-bc0c-377704ef3189)(content(Whitespace\" \
         \"))))(Tile((id \
         5c9a5556-74a8-4e2e-9b7f-3ddc7111563e)(label(base_water))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         c425496a-1502-4665-9c03-4b2515f403f9)(content(Whitespace\" \
         \")))))((Secondary((id \
         d36879e7-29c5-4e97-a083-8b9296f22a07)(content(Whitespace\" \
         \"))))(Tile((id \
         e6535dd2-2407-4743-ba4a-4ee2529a4601)(label(250))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         88a5199e-05a7-434a-95c2-c97c63ba2489)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         50c9c6f9-c1b4-4aa7-b69a-e4f86287f98a)(content(Whitespace\"\\n\"))))(Secondary((id \
         2404a4e4-d4a9-4d4c-a582-d120253a215c)(content(Whitespace\"\\n\"))))(Secondary((id \
         26bb2893-6279-4977-9c38-d4edc1170d37)(content(Comment\"# Note that \
         not every expression gets evaluated! #\"))))(Secondary((id \
         4af9df22-73de-434f-b9ca-cac63a0de53b)(content(Whitespace\"\\n\"))))(Secondary((id \
         36708e85-1414-49a2-9b08-428666f5dd61)(content(Comment\"# `case` \
         pattern-matches a value against branches: #\"))))(Secondary((id \
         10d0b599-2b34-455e-b652-1c8d17648252)(content(Whitespace\"\\n\"))))(Secondary((id \
         beb9cd1a-8df7-483e-ab28-689700df5784)(content(Comment\"# Add probes \
         to each branch of the case below. #\"))))(Secondary((id \
         2d8731ec-de21-4580-9e9d-013d12d67d99)(content(Whitespace\"\\n\"))))(Secondary((id \
         c497bd91-962c-4180-97e0-a57364234cb6)(content(Comment\"# (Add 3 \
         probes, on `+`, `-`, and `base_water`) #\"))))(Secondary((id \
         01f7fb55-bc5a-4712-aec9-36d6b88859c9)(content(Whitespace\"\\n\"))))(Secondary((id \
         078fa47b-635e-4ad0-9a02-3859b7570fae)(content(Whitespace\"\\n\"))))(Tile((id \
         92a35f78-3d8c-4d43-8194-a857740f3f12)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         39523007-c065-4aa5-a3e1-d9acb909d87f)(content(Whitespace\" \
         \"))))(Tile((id \
         ee9b9c4f-a3f8-4252-bc88-892e0f6093d4)(label(current_phase))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         4c46094e-510a-48ce-97d0-f7ae98e0991a)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 24))(sort Pat))((shape(Concave \
         24))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         d479da0c-5141-4a82-acb8-f4b0c32dd658)(content(Whitespace\" \
         \"))))(Tile((id \
         898852d3-46e1-44ea-8b56-6badab71616e)(label(MoonPhase))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         12957b83-4451-498d-bab6-b64378210fd9)(content(Whitespace\" \
         \")))))((Secondary((id \
         dfc43759-6e7d-4ff1-a35a-9f2521d2999f)(content(Whitespace\" \
         \"))))(Tile((id \
         9fe1e49c-3f8d-428d-9ee6-eef03aa8e964)(label(Full))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         534c022a-f454-4d6b-b09f-df177d96faf0)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         0ece94d5-6205-4402-a529-b277aa64b085)(content(Whitespace\"\\n\"))))(Secondary((id \
         cd3cd193-7660-4668-a705-fc56b9d43091)(content(Whitespace\"\\n\"))))(Tile((id \
         6efb5287-b119-43c7-9db2-4e6c2c8a944d)(label(case end))(mold((out \
         Exp)(in_(Rul))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         a2f3efd8-4ac2-4132-816d-0e2c4c5f07ce)(content(Whitespace\" \
         \"))))(Tile((id \
         144b4dce-1de9-445d-8c36-c4ff231dfca3)(label(current_phase))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         8e2760ee-69ae-4801-b033-f5d47892fb22)(content(Whitespace\"\\n\"))))(Tile((id \
         2f83935f-7e78-431d-be7b-15667360da71)(label(| =>))(mold((out \
         Rul)(in_(Pat))(nibs(((shape(Concave 46))(sort Exp))((shape(Concave \
         46))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         e2ce62d0-3e28-4cee-b659-af0554336c74)(content(Whitespace\" \
         \"))))(Tile((id \
         b1c6b070-8fea-427d-8779-d9baafdc1cae)(label(New))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         0fe815f7-cb08-42ef-9e51-1a1348b55fae)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         27a2329a-3b32-4ffd-9aeb-7b8ab8700d9a)(content(Whitespace\" \
         \"))))(Tile((id \
         59ecf4f1-e8ec-49ab-899d-8308a07d4a3b)(label(base_water))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         480317c7-7580-4dab-99d1-39ae1f05595d)(content(Whitespace\" \
         \"))))(Tile((id \
         caf74af0-f376-43ff-b75a-30149924fd66)(label(+))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 28))(sort Exp))((shape(Concave \
         28))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         c167cb5c-b8d7-48f9-9a86-dc1bb80bd4f0)(content(Whitespace\" \
         \"))))(Tile((id \
         bc88d44f-7934-4244-b0a5-ad2c5e544277)(label(50))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         17499538-584a-4d89-89ea-e61d8586ec8a)(content(Whitespace\"\\n\"))))(Tile((id \
         041ecbb5-101a-420d-838a-7be8722b388d)(label(| =>))(mold((out \
         Rul)(in_(Pat))(nibs(((shape(Concave 46))(sort Exp))((shape(Concave \
         46))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         baf22242-7902-41cb-b118-afb11890be1b)(content(Whitespace\" \
         \"))))(Tile((id \
         6b3a64b7-fcf6-4a84-9957-cdc1a5ae4a23)(label(Full))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         0a62881a-4b5d-4033-bc9b-1f9916d15384)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         acaec331-1acf-44b8-add1-6243840f8c66)(content(Whitespace\" \
         \"))))(Tile((id \
         5e6ce994-c730-4e9d-81ce-99ae7ef9d3dc)(label(base_water))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         ff2aec97-c728-4071-8f02-da71a31f1fb7)(content(Whitespace\" \
         \"))))(Tile((id \
         d23b6361-b905-4241-b8d2-54afc7452624)(label(-))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 28))(sort Exp))((shape(Concave \
         28))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         daad413d-df43-402b-9296-5b5945058107)(content(Whitespace\" \
         \"))))(Tile((id \
         67a6b7f8-286e-4e74-bfd3-9bf6b621a443)(label(30))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         6d39d37a-707b-4c55-a4e8-b79cd2fa2c8b)(content(Whitespace\"\\n\"))))(Tile((id \
         395d087b-1d9f-4d1c-8cd8-b1259b465271)(label(| =>))(mold((out \
         Rul)(in_(Pat))(nibs(((shape(Concave 46))(sort Exp))((shape(Concave \
         46))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         4cef81cb-fc10-4657-8c49-7b1d50fa4b96)(content(Whitespace\" \
         \"))))(Tile((id \
         4e666fa0-9bfd-42d4-8674-39892b8d2a3a)(label(_))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         6701b3df-7acf-46f7-8c12-671ecaf798dd)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         10967341-b992-499a-8cf8-50aecc83d04d)(content(Whitespace\" \
         \"))))(Tile((id \
         36fb9f19-8835-4350-8e89-e51de8dc1f9e)(label(base_water))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         e1cfede2-d1fa-4474-866e-79e090fc03e6)(content(Whitespace\" \
         \"))))(Secondary((id \
         3be68859-2a2b-4123-83dc-2349dd0cc7d0)(content(Whitespace\" \
         \"))))(Secondary((id \
         a59a116b-6ee2-497f-8ddf-d7ca6e843e1c)(content(Comment\"# _ is a \
         wildcard, matches anything #\"))))(Secondary((id \
         6b76635a-8842-4fd7-9c21-1a594a3dac0a)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         7cdb2d7f-c233-4cfc-ab6b-5ee703bd9cf6)(content(Whitespace\"\\n\"))))(Secondary((id \
         dd3012e2-169e-405a-9c98-95e0c16eaecb)(content(Whitespace\"\\n\"))))(Secondary((id \
         286ebe0b-5470-436d-9bbd-4db78b10f2d8)(content(Comment\"# Note that \
         only the matching branch shows a sample. #\"))))(Secondary((id \
         f8d388f3-b63a-4e95-92e6-d72ddde5e75a)(content(Whitespace\"\\n\"))))(Secondary((id \
         28258911-65e9-4305-891c-b8bb727a4c78)(content(Comment\"# The others \
         show a null-set icon, meaning there were #\"))))(Secondary((id \
         e89c5bdd-a31b-491e-8fa8-c87c1372b92f)(content(Whitespace\" \
         \"))))(Secondary((id \
         75832415-8fe6-4cc0-b614-987a4eb2da8f)(content(Whitespace\"\\n\"))))(Secondary((id \
         e81cf113-3950-486c-a58d-fea4e3359181)(content(Comment\"# no samples \
         collected since that code path was not taken! #\"))))(Secondary((id \
         6eb4fc2c-2600-4b6f-bb49-5a207ae3f05a)(content(Whitespace\"\\n\"))))(Secondary((id \
         8480ed0b-14cf-44c6-867f-7b069ca3d3b2)(content(Whitespace\"\\n\"))))(Secondary((id \
         fdc2e89c-03cb-4442-a459-7c36e3c6ddef)(content(Comment\"# Go back up \
         and try changing `Full` below to `New`. #\"))))(Secondary((id \
         40cb8447-031a-482b-b0ac-4a8028694769)(content(Whitespace\"\\n\"))))(Secondary((id \
         50d98832-9dcf-44f3-b320-f64363b385f7)(content(Comment\"# Which branch \
         will light up this time? #\"))))(Secondary((id \
         6ef9eb7d-221d-467a-abff-a01a8c87a293)(content(Whitespace\"\\n\"))))(Secondary((id \
         f2de0ca6-563e-4532-8472-4a8d786ef0d3)(content(Whitespace\"\\n\"))))(Secondary((id \
         4dc779fc-1ef3-4ae1-bb60-392edfd9ae9a)(content(Comment\"# END OF PART \
         1 - Select the next slide from the top menu #\")))))";
      backup_text =
        "# PROBES TUTORIAL - PART 1: FUNDAMENTALS #\n\
         # Probes show you runtime values of expressions, inline in the \
         editor. #\n\n\
         # TRY THIS: Put your cursor on the `*` in `250 * 7` below and #\n\
         # right-click to open the context menu and select \"Add probe\". #\n\
         # You should see the sample value 1750 to the right. #\n\n\
         # Hazel uses `let x = expr in body` to define values. #\n\
         let weekly_water = 250 * 7 in\n\n\
         # TRY THIS: You can also press Cmd/Ctrl+E to toggle that probe. #\n\n\
         # You can also probe variable names on the left side of a let \
         binding. #\n\
         # Try adding a probe to `daily_ml` on the left of the `=` sign below. \
         #\n\n\
         let daily_ml = 250 in\n\
         let days = 7 in\n\
         let total = daily_ml * days in\n\n\
         # Now click on the sample value (1750). A dropdown shows the #\n\
         # ENVIRONMENT: the variables in the expression and their values. #\n\n\
         # `type` defines type aliases. Sum types list options with `+`: #\n\
         type MoonPhase = New + Waxing + Full + Waning in\n\
         let base_water = 250 in\n\n\
         # Note that not every expression gets evaluated! #\n\
         # `case` pattern-matches a value against branches: #\n\
         # Add probes to each branch of the case below. #\n\
         # (Add 3 probes, on `+`, `-`, and `base_water`) #\n\n\
         let current_phase: MoonPhase = Full in\n\n\
         case current_phase\n\
         | New => base_water + 50\n\
         | Full => base_water - 30\n\
         | _ => base_water  # _ is a wildcard, matches anything #\n\
         end\n\n\
         # Note that only the matching branch shows a sample. #\n\
         # The others show a null-set icon, meaning there were # \n\
         # no samples collected since that code path was not taken! #\n\n\
         # Go back up and try changing `Full` below to `New`. #\n\
         # Which branch will light up this time? #\n\n\
         # END OF PART 1 - Select the next slide from the top menu #";
      refractors = "()";
    } )
