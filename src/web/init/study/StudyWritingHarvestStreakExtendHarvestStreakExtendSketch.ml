let out : string * Haz3lcore.PersistentSegment.t =
  ( "Study / writing / harvest-streak-extend / harvest-streak-extend-sketch",
    {
      segment =
        "((Secondary((id \
         70bcd878-119d-4fba-96e0-122d3428b343)(content(Comment\"# HARVEST \
         STREAK EXTENSION TASK                   #\"))))(Secondary((id \
         2e66ac96-8030-42c8-b6ff-26bfea6dfc13)(content(Whitespace\"\\n\"))))(Secondary((id \
         29e678f9-a78f-46b2-99e4-32bb2d68ba6e)(content(Comment\"#                                                  \
         #\"))))(Secondary((id \
         0714175b-2845-4d3d-9fcd-3797368fc9c0)(content(Whitespace\"\\n\"))))(Secondary((id \
         74c6c032-e11a-47b7-bb83-1d1e888739e4)(content(Comment\"# The harvest \
         ledger app tracks harvests and       #\"))))(Secondary((id \
         41e2b241-6e0c-4e50-9585-12a881c42cd9)(content(Whitespace\"\\n\"))))(Secondary((id \
         8b80e083-0385-4d75-862e-55c0060f2e6d)(content(Comment\"# builds \
         streak bonuses for consecutive same-      #\"))))(Secondary((id \
         cdd06913-f1ad-4b09-8071-a58667f460a8)(content(Whitespace\"\\n\"))))(Secondary((id \
         5719bf64-9f32-4d85-8650-54337dc820f7)(content(Comment\"# quality \
         harvests.                                #\"))))(Secondary((id \
         059944a5-5a96-47e9-991c-70cfc576a903)(content(Whitespace\"\\n\"))))(Secondary((id \
         0ee770da-a2cf-4d5a-8513-3c0460d89ea0)(content(Comment\"#                                                  \
         #\"))))(Secondary((id \
         9fe6acf1-eac2-4edb-85ef-6f4910b13328)(content(Whitespace\"\\n\"))))(Secondary((id \
         c02bd478-4465-49b5-b141-95da5e9f0abd)(content(Comment\"# YOUR TASK: \
         Add a PremiumSale action that lets    #\"))))(Secondary((id \
         96fae015-6080-4c56-8ef3-7b34867dff42)(content(Whitespace\"\\n\"))))(Secondary((id \
         3b59d7f7-9720-49df-82a1-3fa18d345379)(content(Comment\"# the farmer \
         claim their streak bonus with a       #\"))))(Secondary((id \
         75c32ac6-54ac-4e8b-ace9-85cfb753b479)(content(Whitespace\"\\n\"))))(Secondary((id \
         eb0a1e53-3833-4e41-ac40-17017e987693)(content(Comment\"# premium \
         multiplier when the streak is strong.    #\"))))(Secondary((id \
         9b6d465b-40aa-4837-b75d-69e741ae2303)(content(Whitespace\"\\n\"))))(Secondary((id \
         f277614a-994e-4ae4-a2e0-1792c107797b)(content(Comment\"#                                                  \
         #\"))))(Secondary((id \
         0fd3e946-aa3b-4244-8a2f-74262cc53a6a)(content(Whitespace\"\\n\"))))(Secondary((id \
         c3242c42-96cd-4ef1-b409-493235f4c4d6)(content(Comment\"# You need \
         to:                                     #\"))))(Secondary((id \
         b8bfa4fa-9840-41d6-a96b-47d989ee87e0)(content(Whitespace\"\\n\"))))(Secondary((id \
         0181f439-9673-4ef0-8928-ec02e21acb5a)(content(Comment\"#   1. Add \
         PremiumSale to the Action type          #\"))))(Secondary((id \
         7f10e6d4-cd47-4acc-8451-9198d2c2eca3)(content(Whitespace\"\\n\"))))(Secondary((id \
         0890a73d-1610-4450-99ad-1af316c34169)(content(Comment\"#   2. Write a \
         premiumMultiplier helper function   #\"))))(Secondary((id \
         8bfccd8a-345a-4406-b9e6-fcc2a0b25122)(content(Whitespace\"\\n\"))))(Secondary((id \
         35567d60-32a5-4f7a-bb39-0846f79c2057)(content(Comment\"#   3. Handle \
         PremiumSale in the update function   #\"))))(Secondary((id \
         469a6e3d-01a3-4998-91bb-0788869259df)(content(Whitespace\"\\n\"))))(Secondary((id \
         a234496e-2fd8-427e-a11a-80e147e077cd)(content(Comment\"#                                                  \
         #\"))))(Secondary((id \
         fef255ab-d7ba-4589-8be0-090acb045dce)(content(Whitespace\"\\n\"))))(Secondary((id \
         44cb574f-88c7-4ca4-9044-c3e6e485c32b)(content(Comment\"# Look at how \
         ClaimBonus is implemented for        #\"))))(Secondary((id \
         426b554b-a24c-4df5-ace5-da9b69e7da67)(content(Whitespace\"\\n\"))))(Secondary((id \
         ff388e1a-c66d-405b-a036-3bee4b28f881)(content(Comment\"# guidance - \
         PremiumSale is similar but applies    #\"))))(Secondary((id \
         872e0bd6-eaea-483a-8bd8-a725cb3bd395)(content(Whitespace\"\\n\"))))(Secondary((id \
         4b3fd7fc-8d31-4bc0-9501-04a15e6977ec)(content(Comment\"# a multiplier \
         to the payout.                      #\"))))(Secondary((id \
         3bbaef3d-4fe0-4487-8760-ecf7bcff2586)(content(Whitespace\"\\n\"))))(Secondary((id \
         6f6dd9ec-34df-48e9-8db4-ec24565e4374)(content(Comment\"#                                                  \
         #\"))))(Secondary((id \
         f3638238-341e-417c-a930-968f8c6d6c65)(content(Whitespace\"\\n\"))))(Secondary((id \
         7f316b10-df64-415e-99ab-b516c595623c)(content(Comment\"# Tip: Use \
         auto-probe on premiumMultiplier to see  #\"))))(Secondary((id \
         74c01589-d661-4826-b63b-8905a1b45722)(content(Whitespace\"\\n\"))))(Secondary((id \
         ee4167fd-0cc9-488e-9f5c-efbbe1bf1ccd)(content(Comment\"# when the \
         threshold fires.                        #\"))))(Secondary((id \
         26a62377-88ac-4331-a559-3820c60f2d5e)(content(Whitespace\"\\n\"))))(Secondary((id \
         6129a13c-ff73-41a7-8c55-3dfeb8b5e738)(content(Whitespace\"\\n\"))))(Secondary((id \
         b806b3cb-fec3-468e-bd2c-0b637ddfa362)(content(Comment\"# Quality \
         tiers from the moonlit fields #\"))))(Secondary((id \
         01d6b887-aba3-4de1-a786-25ffce59323e)(content(Whitespace\"\\n\"))))(Tile((id \
         cc3fd1cb-960c-47b0-8d92-29738aecaa82)(label(type = in))(mold((out \
         Exp)(in_(TPat Typ))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         4a8e58c7-3d8b-4cdf-ada7-0eaaae952e2c)(content(Whitespace\" \
         \"))))(Tile((id \
         0ec8d663-9a15-4c0f-8049-39d184c1e794)(label(Quality))(mold((out \
         TPat)(in_())(nibs(((shape Convex)(sort TPat))((shape Convex)(sort \
         TPat))))))(shards(0))(children())))(Secondary((id \
         55832110-b0db-48b2-8ddb-2aa05cbca0fe)(content(Whitespace\" \
         \")))))((Secondary((id \
         a13e5103-258f-4ead-a17a-0127d5f2806d)(content(Whitespace\"\\n\"))))(Tile((id \
         308219eb-e5af-435b-aafe-2ad57443c9a2)(label(+))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape(Concave 33))(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         bdfbb274-4822-41aa-9d36-24afc7dd7187)(content(Whitespace\" \
         \"))))(Tile((id \
         3c5c37c9-c26c-45cd-99ce-6beb37448221)(label(Bronze))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         1988c1bb-8405-4d42-b6f6-de5abe7e0b9b)(content(Whitespace\" \
         \"))))(Secondary((id \
         163623e4-2b92-4fa6-82f8-af022103476d)(content(Whitespace\" \
         \"))))(Secondary((id \
         f20c9684-d38e-49ce-9a2e-338556c0a0a2)(content(Whitespace\" \
         \"))))(Secondary((id \
         dc51f173-9738-47ad-bd02-dff5ab7040b7)(content(Whitespace\" \
         \"))))(Secondary((id \
         1d9fe61d-9fd5-48ef-b544-b073d07925a3)(content(Whitespace\" \
         \"))))(Secondary((id \
         a4f65f5c-70ca-4de8-b443-428af4b9d801)(content(Whitespace\" \
         \"))))(Secondary((id \
         2581a9f8-1801-4b96-a1b0-018fff8f33c2)(content(Comment\"# Common \
         harvest, basic value #\"))))(Secondary((id \
         a696ac4e-c3fe-4436-8dc0-b3e5eff1221b)(content(Whitespace\"\\n\"))))(Tile((id \
         7e2a1681-e0ab-44e1-8a73-4c6c11292ff6)(label(+))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 12))(sort Typ))((shape(Concave \
         12))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         2b4e8da3-69d0-4c0b-a585-9c9313c02981)(content(Whitespace\" \
         \"))))(Tile((id \
         1b1caf7b-73fc-4395-9ccd-07fb1adce994)(label(Silver))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         35cae8a3-ec57-4f24-9e94-fbb808890a4e)(content(Whitespace\" \
         \"))))(Secondary((id \
         0c7732aa-c289-41b7-a7a2-0362e5d8a9dd)(content(Whitespace\" \
         \"))))(Secondary((id \
         30382667-5dbe-4cb2-b7c7-55536180c2d1)(content(Whitespace\" \
         \"))))(Secondary((id \
         511f55e6-7d71-47a9-9485-b1039dd04648)(content(Whitespace\" \
         \"))))(Secondary((id \
         14c7a53b-a223-4297-a793-c7a646c9280c)(content(Whitespace\" \
         \"))))(Secondary((id \
         e2c6c646-1722-4a77-ac0e-b776f6a7e760)(content(Whitespace\" \
         \"))))(Secondary((id \
         4847a4ef-caf9-4a45-975e-1fedb5fe5264)(content(Comment\"# Good \
         quality, moderate bonus #\"))))(Secondary((id \
         6fd25ec2-e23e-41fe-9cd3-ee730560c363)(content(Whitespace\"\\n\"))))(Tile((id \
         2632970e-6d14-4e53-b3ea-5df410d46987)(label(+))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 12))(sort Typ))((shape(Concave \
         12))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         e1824540-5673-4670-9dad-3b1584f92cb7)(content(Whitespace\" \
         \"))))(Tile((id \
         f993936f-ae2f-40e4-9b9b-a49b89d6d48f)(label(Gold))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         8f2ea16a-4ef0-4523-adef-fc227f8c2351)(content(Whitespace\" \
         \"))))(Secondary((id \
         d6442c37-82b5-4352-b930-55ef8af575fe)(content(Whitespace\" \
         \"))))(Secondary((id \
         badca807-6492-4a25-b306-a59f23cd68a6)(content(Whitespace\" \
         \"))))(Secondary((id \
         ef84dc12-e163-40a5-88be-fb59ec201173)(content(Whitespace\" \
         \"))))(Secondary((id \
         b33835dd-28a9-4494-8804-b4cf24274986)(content(Whitespace\" \
         \"))))(Secondary((id \
         801795eb-b972-4476-9bc7-9ec87f782c46)(content(Whitespace\" \
         \"))))(Secondary((id \
         0d26a61d-9c37-44e4-9e80-cce657b46b4c)(content(Whitespace\" \
         \"))))(Secondary((id \
         cebc40a6-1992-4708-8c3c-b6267e2257bc)(content(Whitespace\" \
         \"))))(Secondary((id \
         2415142c-9f08-413b-a454-cf51e728b206)(content(Comment\"# Excellent \
         harvest, high value #\"))))(Secondary((id \
         a3a7826d-0303-46f7-a102-388541022314)(content(Whitespace\"\\n\"))))(Tile((id \
         5b901b49-796d-4cf0-a6be-4eb112730797)(label(+))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 12))(sort Typ))((shape(Concave \
         12))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         8aec1b5a-a3b5-417b-a0bc-f9fc235610d3)(content(Whitespace\" \
         \"))))(Tile((id \
         44b1befb-f16d-498e-bf96-3bd6657e58af)(label(Starlight))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         f27997bf-65ac-4f65-9f06-040ae7897434)(content(Whitespace\" \
         \"))))(Secondary((id \
         bef04c73-445c-4dec-805a-b5918be119b2)(content(Whitespace\" \
         \"))))(Secondary((id \
         4b980da7-828e-4ab3-8a9a-04e8322c50d6)(content(Whitespace\" \
         \"))))(Secondary((id \
         0eae3cb7-0a01-4226-91e7-85990e7c240c)(content(Comment\"# Legendary, \
         blessed by the moon #\"))))(Secondary((id \
         6ddca3bd-ff1f-4c5a-9eed-d26ae2bd47e1)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         69305a53-740b-4d7a-95ec-bfdf3b466788)(content(Whitespace\"\\n\"))))(Secondary((id \
         83f7ba16-73ff-457f-8740-78d082fb5888)(content(Whitespace\"\\n\"))))(Secondary((id \
         94efd262-f6b1-45aa-8012-e50edc97babf)(content(Comment\"# Crops that \
         grow under the night sky #\"))))(Secondary((id \
         04c77390-2f49-4f3d-bda8-bc17b001078c)(content(Whitespace\"\\n\"))))(Tile((id \
         d9da74bd-88f5-404b-b439-6a5ef12e49ec)(label(type = in))(mold((out \
         Exp)(in_(TPat Typ))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         e4ea94c6-6fd2-4eee-b8c5-08a2a3177f81)(content(Whitespace\" \
         \"))))(Tile((id \
         be9c2713-a271-48ca-a79e-dc8997bf4772)(label(Crop))(mold((out \
         TPat)(in_())(nibs(((shape Convex)(sort TPat))((shape Convex)(sort \
         TPat))))))(shards(0))(children())))(Secondary((id \
         cb8f0ca3-ff3d-48d6-9cce-5944b27259ad)(content(Whitespace\" \
         \")))))((Secondary((id \
         67ff55ac-c02f-4fe7-965b-47e77e0be488)(content(Whitespace\"\\n\"))))(Tile((id \
         a49c12cb-e22f-4067-88ad-eb3443757a7a)(label(+))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape(Concave 33))(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         b2f21e68-d406-4b5e-ab3b-8e5413b38941)(content(Whitespace\" \
         \"))))(Tile((id \
         4362fdbc-9013-4c69-b4f6-0eec3eae802b)(label(Moonmelon))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         875c17ae-7311-461b-b6df-706d8e65b491)(content(Whitespace\" \
         \"))))(Secondary((id \
         1f2b98f4-4715-4d06-b947-436476f76fe2)(content(Whitespace\" \
         \"))))(Secondary((id \
         cc075702-2360-40bb-b4b0-bb1abfc08d38)(content(Whitespace\" \
         \"))))(Secondary((id \
         bdf2d354-6dd2-4a91-acd2-dfac2c232610)(content(Comment\"# Glows \
         faintly, sweet taste #\"))))(Secondary((id \
         1abe8ab5-4978-4eed-afda-acf49caca82c)(content(Whitespace\"\\n\"))))(Tile((id \
         43b09755-53a9-463a-aa40-0ec11f145afe)(label(+))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 12))(sort Typ))((shape(Concave \
         12))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         bb950f09-7106-442d-baa5-48aea52ccb91)(content(Whitespace\" \
         \"))))(Tile((id \
         ba76d47c-0b94-4b8a-9489-de4cc434df78)(label(Starfruit))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         08d039b6-584a-494b-ad81-9652a47f9f5b)(content(Whitespace\" \
         \"))))(Secondary((id \
         745f5022-9011-4332-a27e-f487d5dd3aad)(content(Whitespace\" \
         \"))))(Secondary((id \
         9c4286b4-e44a-423e-aacd-d0b89d1c0917)(content(Whitespace\" \
         \"))))(Secondary((id \
         5941a917-7175-4f7b-be4a-b933c6d0d423)(content(Comment\"# Shaped like \
         stars, tangy #\"))))(Secondary((id \
         10b56d4c-640d-41ae-b0f5-423e84975c7e)(content(Whitespace\"\\n\"))))(Tile((id \
         133b4106-6b5b-4c29-a1ac-ddaa4f2406cc)(label(+))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 12))(sort Typ))((shape(Concave \
         12))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         952e52c4-29ae-47a9-a3cd-47c53c7edb69)(content(Whitespace\" \
         \"))))(Tile((id \
         748b76f1-1614-4f7c-a9a5-bb954b1e5857)(label(Nightshade))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         aec0db39-0829-440a-85e6-f54ccd258e5f)(content(Whitespace\" \
         \"))))(Secondary((id \
         e8ac571e-d1d2-4b04-b94e-c161813d47c9)(content(Whitespace\" \
         \"))))(Secondary((id \
         6483dc72-5be9-4c1a-be18-4123c43790e3)(content(Comment\"# Purple \
         bloom, magical properties #\"))))(Secondary((id \
         6753706c-e61f-4ab1-8b1e-6bb9b9a69456)(content(Whitespace\"\\n\"))))(Tile((id \
         4c9fc12d-af1c-4cea-8636-eed79f8cbc9b)(label(+))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 12))(sort Typ))((shape(Concave \
         12))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         66a337de-d7d3-4871-8bae-73f3194ba5be)(content(Whitespace\" \
         \"))))(Tile((id \
         85893f45-499f-4769-a50f-6053290c1eb2)(label(Duskwheat))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         c2216cb9-bd2d-4991-8a06-5861b7628a4e)(content(Whitespace\" \
         \"))))(Secondary((id \
         f6bcac6c-67c5-465d-9f88-a66d232eb178)(content(Whitespace\" \
         \"))))(Secondary((id \
         9eacb410-f303-44e2-9034-dc4ff20e62aa)(content(Whitespace\" \
         \"))))(Secondary((id \
         66ab7852-632c-43df-91ab-cc52528f9322)(content(Comment\"# Golden \
         stalks, hearty grain #\"))))(Secondary((id \
         19a3c366-464c-4dd3-a4df-77a2e7127bb9)(content(Whitespace\"\\n\"))))(Tile((id \
         e13f75a4-a41a-42cd-a903-424ce2a566fb)(label(+))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 12))(sort Typ))((shape(Concave \
         12))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         22ed1cbc-e443-4758-b41d-ae4bbe99b7ab)(content(Whitespace\" \
         \"))))(Tile((id \
         f03e5445-9c4e-4ccc-b735-de622c8ecf23)(label(Glowpumpkin))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         a4780887-47b3-4a17-99f1-30750764f8c1)(content(Whitespace\" \
         \"))))(Secondary((id \
         72269286-3c1d-423d-a4dd-317ffb61b98a)(content(Comment\"# Orange and \
         luminescent #\"))))(Secondary((id \
         eaab4f7a-4cd3-41a7-9f99-cc2f69687e47)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         ffd408b1-5bc7-4c01-a1d6-220750ff8602)(content(Whitespace\"\\n\"))))(Secondary((id \
         323e1c0a-f3ef-460d-924d-8f649adf5dfc)(content(Whitespace\"\\n\"))))(Secondary((id \
         40dfd907-d593-4323-880d-8e7cf5617419)(content(Comment\"# A single \
         harvest from the garden #\"))))(Secondary((id \
         dd3c18c7-7011-4f66-8a71-78645057e601)(content(Whitespace\"\\n\"))))(Tile((id \
         b35b5e41-a4f4-4e73-8e6a-499431f53923)(label(type = in))(mold((out \
         Exp)(in_(TPat Typ))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         4b8f9604-c7e2-4f6c-bd64-f6896cfb10fc)(content(Whitespace\" \
         \"))))(Tile((id \
         6729f323-fb0a-4dea-b5ec-4974b638bc38)(label(Harvest))(mold((out \
         TPat)(in_())(nibs(((shape Convex)(sort TPat))((shape Convex)(sort \
         TPat))))))(shards(0))(children())))(Secondary((id \
         1b229357-64ff-4352-bfcd-a53c355882ea)(content(Whitespace\" \
         \")))))((Secondary((id \
         cbe1c354-d5fd-4c71-bb58-dbd652c817b5)(content(Whitespace\" \
         \"))))(Tile((id \
         35d3c86f-b1bb-4361-8298-8cc767020496)(label(\"(\"\")\"))(mold((out \
         Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0 1))(children(((Secondary((id \
         d5ef13a8-f940-45a8-8134-9967dc54d6ab)(content(Whitespace\"\\n\"))))(Tile((id \
         3797358d-770b-426e-9b75-5f923608f869)(label(crop))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         1a810a42-1681-46dc-b111-1cb4aeb4b5a4)(content(Whitespace\" \
         \"))))(Tile((id \
         6f379769-d84c-43ca-b472-50586cb49154)(label(=))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 39))(sort Typ))((shape(Concave \
         39))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         2e57affd-7aa7-4903-8fcd-1134a1ed23d9)(content(Whitespace\" \
         \"))))(Tile((id \
         c03c112c-a798-4553-9655-09975f38b9dd)(label(Crop))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         ee12ae4f-a55c-4e8c-85e4-acc94e4c9887)(label(,))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 44))(sort Typ))((shape(Concave \
         44))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         c938f658-2c03-4348-926e-9f2f84157c1b)(content(Whitespace\"\\n\"))))(Tile((id \
         7e4126af-cd5b-425b-9c1b-576a67e729da)(label(quality))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         b201f1f5-5261-47a7-8ed7-6e0b10161f5a)(content(Whitespace\" \
         \"))))(Tile((id \
         953433db-48e3-4a60-aa06-8f5e2f5c4a79)(label(=))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 39))(sort Typ))((shape(Concave \
         39))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         b44b1fd9-46c1-4828-ae50-73bcbff7bcdd)(content(Whitespace\" \
         \"))))(Tile((id \
         7fc399e7-6256-407b-9ae8-d4db35c73c5d)(label(Quality))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         07ba447d-5153-4314-9b84-9eb1b128d57c)(label(,))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 44))(sort Typ))((shape(Concave \
         44))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         44941b33-e3aa-4c8d-85ad-5e01b58fc8aa)(content(Whitespace\"\\n\"))))(Tile((id \
         654f1e34-7aae-4b27-a7d2-1072bce2fcc4)(label(quantity))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         d755dfe2-716b-4346-baa9-0dcb72bec001)(content(Whitespace\" \
         \"))))(Tile((id \
         f6417a28-21d0-4ef9-9d35-e05f4a188d56)(label(=))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 39))(sort Typ))((shape(Concave \
         39))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         99a93c29-6f56-4bf9-8b41-e448687a1a1f)(content(Whitespace\" \
         \"))))(Tile((id \
         dc3d4dbe-c260-41eb-b255-02c98ca72a04)(label(Int))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         820dfe95-0d5a-42c4-9d97-0d582143fed6)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         3e8850bd-b483-475c-baac-48f6287ff60d)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         63438f6c-4aa7-4d36-91df-527edd8a2f55)(content(Whitespace\"\\n\"))))(Secondary((id \
         f0393a9f-53f6-4ddd-b50f-6fc1e37b313e)(content(Whitespace\"\\n\"))))(Secondary((id \
         2b983800-c74e-42ea-be17-9a8e807609ee)(content(Comment\"# The harvest \
         ledger tracks all harvests and bonuses #\"))))(Secondary((id \
         3c4c2fda-5a9e-4eae-a17e-d2361b01edac)(content(Whitespace\"\\n\"))))(Tile((id \
         f4646abf-8fa2-4d0c-b578-9f905e1a947f)(label(type = in))(mold((out \
         Exp)(in_(TPat Typ))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         fd7e3879-59c2-4953-913d-a27e471b1137)(content(Whitespace\" \
         \"))))(Tile((id \
         53013620-3abd-4c8b-8178-54279e7ebf85)(label(Model))(mold((out \
         TPat)(in_())(nibs(((shape Convex)(sort TPat))((shape Convex)(sort \
         TPat))))))(shards(0))(children())))(Secondary((id \
         752ce3f4-be62-403b-9ca3-220cb4afb137)(content(Whitespace\" \
         \")))))((Secondary((id \
         1ff424c8-2f39-432c-a9b2-762769a1a7d7)(content(Whitespace\" \
         \"))))(Tile((id \
         5bfb5ce4-c7b4-4045-a9e3-1e8dade2cfe0)(label(\"(\"\")\"))(mold((out \
         Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0 1))(children(((Secondary((id \
         19b79aaf-01ae-494c-a359-4797f645ad0d)(content(Whitespace\"\\n\"))))(Tile((id \
         9f89bae8-7284-4dac-b24d-22181dcc82a0)(label(harvests))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         9962f635-4e64-4b13-b2a0-f7fc54336bce)(content(Whitespace\" \
         \"))))(Tile((id \
         0cb27b82-8101-48ac-9e52-0f2a65d61035)(label(=))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 39))(sort Typ))((shape(Concave \
         39))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         77255ebe-a064-4ce8-9308-5824f6f94df7)(content(Whitespace\" \
         \"))))(Tile((id 76c814c0-b897-44fb-93c5-627a76faa008)(label([ \
         ]))(mold((out Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape \
         Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
         b0debc7c-1949-4657-a28a-b0ef0d78d356)(label(Harvest))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children()))))))))(Tile((id \
         98e433d9-6d2d-49d7-ae51-2239df48c7a4)(label(,))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 44))(sort Typ))((shape(Concave \
         44))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         24387002-71e9-4328-a974-e4f5b3a838ce)(content(Whitespace\"\\n\"))))(Tile((id \
         e06489f9-7881-4693-8b8d-cb7b9a5bc7d6)(label(totalValue))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         e58820f8-fe22-456f-8527-e0ac9943acf0)(content(Whitespace\" \
         \"))))(Tile((id \
         4e3c39fa-adff-4fc6-af75-1a996b8a3ada)(label(=))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 39))(sort Typ))((shape(Concave \
         39))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         e0ef482b-1b93-449b-844e-7bf7c950cb51)(content(Whitespace\" \
         \"))))(Tile((id \
         67851e23-a827-4459-92f5-bbc2441c906d)(label(Int))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         1842474c-b7d3-428d-8f87-8262fa3ab857)(label(,))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 44))(sort Typ))((shape(Concave \
         44))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         0c56e9a8-f875-4c6f-be10-9165af1d1c0f)(content(Whitespace\"\\n\"))))(Tile((id \
         284d0fd1-7e2c-4c72-903e-5eb7e077cf1f)(label(streakBonus))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         20fbca04-ab5c-4c61-8b7c-7c81b28b2123)(content(Whitespace\" \
         \"))))(Tile((id \
         901b617c-03cf-47ed-a980-68de85b52200)(label(=))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 39))(sort Typ))((shape(Concave \
         39))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         12bb8b65-1cd9-4099-adfd-87ca33bdc4e0)(content(Whitespace\" \
         \"))))(Tile((id \
         e90d177e-ea51-4f5c-a9b3-4c9f1ef21462)(label(Int))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         8c8124c0-b16f-43f6-9501-c6ca8f49a839)(label(,))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 44))(sort Typ))((shape(Concave \
         44))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         588af3ce-0edb-4cde-8ac9-85c1fd2f8b81)(content(Whitespace\"\\n\"))))(Tile((id \
         7b7b1d37-e35f-42bd-a6a4-52ac4ebbae1a)(label(lastQuality))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         90aa0f60-b066-4101-9627-bc6d786fec54)(content(Whitespace\" \
         \"))))(Tile((id \
         044ac733-c302-4446-9c6c-70865053654b)(label(=))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 39))(sort Typ))((shape(Concave \
         39))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         382777b7-718f-4cab-a1a0-336863b5b54d)(content(Whitespace\" \
         \"))))(Tile((id \
         f7918038-88f4-4581-9005-61bc910e0fb6)(label(Quality))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         4196018c-f383-41c1-acf1-9952af562e20)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         30353626-29da-4450-82ad-9522f5990cd1)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         3a23b4de-0610-4c33-aa26-56fa799d5112)(content(Whitespace\"\\n\"))))(Secondary((id \
         19dffbff-ba5f-45b0-bd64-f5fa9ffe780e)(content(Whitespace\"\\n\"))))(Secondary((id \
         4368a88b-0516-45aa-bdf9-cef968ab8f98)(content(Comment\"# Actions the \
         farmer can take #\"))))(Secondary((id \
         67ef1b49-d99a-4d0e-bb1a-b634b535f6fe)(content(Whitespace\"\\n\"))))(Tile((id \
         51550016-1992-42b7-a87c-88bd16da8ad4)(label(type = in))(mold((out \
         Exp)(in_(TPat Typ))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         34143443-67de-40b7-8e95-5a5481469751)(content(Whitespace\" \
         \"))))(Tile((id \
         ce88f83c-5827-45c6-8fc1-1ad11725aceb)(label(Action))(mold((out \
         TPat)(in_())(nibs(((shape Convex)(sort TPat))((shape Convex)(sort \
         TPat))))))(shards(0))(children())))(Secondary((id \
         cc4813d8-af01-411d-8ecb-2c773a101cea)(content(Whitespace\" \
         \")))))((Secondary((id \
         f59ee121-9680-4b14-b5a9-5b05c3cb27b1)(content(Whitespace\"\\n\"))))(Tile((id \
         1da0b629-f96c-4239-8b57-90567b9e4d7d)(label(+))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape(Concave 33))(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         1cb5675f-4bea-4e07-9aa4-a5e3dbd61818)(content(Whitespace\" \
         \"))))(Tile((id \
         e16bdecb-abec-400d-8073-2832b40eab2a)(label(RecordHarvest))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         a9a84696-1c9f-465c-8141-946b66ca22d9)(label(\"(\"\")\"))(mold((out \
         Typ)(in_(Typ))(nibs(((shape(Concave 11))(sort Typ))((shape \
         Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
         09dad564-8fb2-43b7-9125-b3db00ee57c8)(label(Harvest))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children()))))))))(Secondary((id \
         a2bf3b16-ac5e-4ace-9f00-92881e1b7644)(content(Whitespace\" \
         \"))))(Secondary((id \
         e22b0c92-3648-4f90-a62b-2021a4b485b7)(content(Whitespace\" \
         \"))))(Secondary((id \
         fe74966c-1dfc-4a8c-86a7-b8dacd850fc3)(content(Comment\"# Log a new \
         harvest #\"))))(Secondary((id \
         f8d497be-edcb-4381-9b6b-46884b024ce5)(content(Whitespace\"\\n\"))))(Tile((id \
         2c592318-d685-4889-9a09-88712569fc9b)(label(+))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 12))(sort Typ))((shape(Concave \
         12))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         d3f6936b-3a55-4f92-a10d-046b0ce69fd3)(content(Whitespace\" \
         \"))))(Tile((id \
         980f06be-cb51-450d-9935-d2f4d90778bd)(label(ClaimBonus))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         4d20971e-84dd-490b-9ab7-4e024f077dac)(content(Whitespace\" \
         \"))))(Secondary((id \
         841ad246-18aa-4e59-a64f-fa187e4080e2)(content(Whitespace\" \
         \"))))(Secondary((id \
         a9899912-ed2a-49cf-a1f7-24dbb8e2891a)(content(Whitespace\" \
         \"))))(Secondary((id \
         924df672-8b33-4f48-9833-c40243ee32bf)(content(Whitespace\" \
         \"))))(Secondary((id \
         5a8f49aa-1bec-44a1-b0f0-7f5c55e762c3)(content(Whitespace\" \
         \"))))(Secondary((id \
         41f2b307-3af7-4f62-9af2-4e1700bcecb2)(content(Whitespace\" \
         \"))))(Secondary((id \
         2419c6cd-f7c4-4ea6-8ac1-0202a5e84403)(content(Whitespace\" \
         \"))))(Secondary((id \
         a74dcff5-5835-444c-85da-5e63f53ca6e1)(content(Whitespace\" \
         \"))))(Secondary((id \
         3534bd1b-834e-4fb8-a747-da7da5289822)(content(Whitespace\" \
         \"))))(Secondary((id \
         83cd8ccd-6c64-425c-865e-ebd10a11c462)(content(Whitespace\" \
         \"))))(Secondary((id \
         02b3c397-60fc-43ef-a912-20f70ae54975)(content(Whitespace\" \
         \"))))(Secondary((id \
         61b8c91d-ab07-46f6-9ecd-b767e5062706)(content(Whitespace\" \
         \"))))(Secondary((id \
         8231f937-c32e-4f66-ab85-9c6ed9d7e39e)(content(Whitespace\" \
         \"))))(Secondary((id \
         56c8b241-6581-4d08-af18-c3b63133b086)(content(Whitespace\" \
         \"))))(Secondary((id \
         abc8eb5c-5fc5-4fcf-8b29-535dcc9069d9)(content(Comment\"# Collect \
         accumulated streak bonus #\"))))(Secondary((id \
         e34e03b1-4006-4585-a229-99f5ccd3866d)(content(Whitespace\"\\n\"))))(Tile((id \
         b0c6b120-6e5f-491b-8e3d-24e99f2bdcaa)(label(+))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 12))(sort Typ))((shape(Concave \
         12))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         2fe376da-74ac-4c69-afd0-6255f97532eb)(content(Whitespace\" \
         \"))))(Tile((id \
         fcbec374-9555-442b-b208-f9b22f2e579f)(label(CloseDay))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         393d3bfb-66b3-4dba-8cb3-06d7e8f389d0)(content(Whitespace\" \
         \"))))(Secondary((id \
         458e71ba-2b8d-43a9-b91f-9be268d29d06)(content(Whitespace\" \
         \"))))(Secondary((id \
         cc32289a-7464-4956-a1cc-4c48940243ed)(content(Whitespace\" \
         \"))))(Secondary((id \
         cba2892c-15ad-47f5-93d4-8bc30bd9b2e5)(content(Whitespace\" \
         \"))))(Secondary((id \
         acb7480c-842a-46e1-834a-588a28f5f643)(content(Whitespace\" \
         \"))))(Secondary((id \
         e1df2cab-7009-4a14-b7d7-f0c4d9a89415)(content(Whitespace\" \
         \"))))(Secondary((id \
         91602e91-d65d-40e6-9666-9720ec211bbe)(content(Whitespace\" \
         \"))))(Secondary((id \
         9a9bc986-3fef-4914-a41c-49e7ec9aed19)(content(Whitespace\" \
         \"))))(Secondary((id \
         6aa58778-d4c8-4762-aaef-87df50ddba20)(content(Whitespace\" \
         \"))))(Secondary((id \
         ae027b16-0b38-400a-b0e7-d9bbc63a1171)(content(Whitespace\" \
         \"))))(Secondary((id \
         0947bbba-06b2-4331-b88e-19e8b76cbc2e)(content(Whitespace\" \
         \"))))(Secondary((id \
         d6d61fb0-78d2-4656-88c9-f32b87fe0c3f)(content(Whitespace\" \
         \"))))(Secondary((id \
         4f24370f-799d-48ab-8762-d35a77252695)(content(Whitespace\" \
         \"))))(Secondary((id \
         4aec24af-3e57-47c2-9deb-2a6515d49fa9)(content(Whitespace\" \
         \"))))(Secondary((id \
         7e4486cf-83da-49cd-be46-3ca96ecbb1b8)(content(Whitespace\" \
         \"))))(Secondary((id \
         17412d04-16f6-4539-a743-4a17fffd4c69)(content(Whitespace\" \
         \"))))(Secondary((id \
         611f3d12-40b5-4818-8904-752a581ceed4)(content(Comment\"# End the \
         harvest day, reset streaks #\"))))(Secondary((id \
         2188610b-83c0-4f9a-aa67-775e1654594a)(content(Whitespace\"\\n\"))))(Secondary((id \
         32d161ce-f580-42a9-abf6-ba927c61149e)(content(Comment\"# TODO: Add \
         PremiumSale here #\"))))(Secondary((id \
         3ee2e8a4-a807-448a-96f1-ce0f4c82bb4c)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         12a619e1-fb43-495b-a359-193abbaa2d21)(content(Whitespace\"\\n\"))))(Secondary((id \
         5c3d1fb7-a0ea-4c08-8cdc-c3681c9eee3c)(content(Whitespace\"\\n\"))))(Secondary((id \
         83d38ac1-4955-42be-87a6-70bc278c7e6e)(content(Comment\"# Calculate \
         base value of a crop #\"))))(Secondary((id \
         7f7fc6b3-711f-4757-acc3-c74f8497bc9f)(content(Whitespace\"\\n\"))))(Tile((id \
         b6e75ef7-155f-42e9-910e-307e10ecd52e)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         aa17b0e3-d33c-42d2-97f9-22b5a2f622dc)(content(Whitespace\" \
         \"))))(Tile((id \
         b8980aa7-843a-4c18-b311-75564e1046aa)(label(cropValue))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         e199ce33-f2aa-4b5e-bdb7-c198204854ee)(content(Whitespace\" \
         \"))))(Tile((id \
         a472634b-87c0-437c-a12d-ce656d5ef2e7)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 24))(sort Pat))((shape(Concave \
         24))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         0d8a8c5b-0de2-4196-9f7f-74f9dc3e3bba)(content(Whitespace\" \
         \"))))(Tile((id \
         ffcd4847-e961-4e87-a7fe-f9ac0615d210)(label(Crop))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         048deefa-7f0c-4c84-9546-b9b981015e4f)(content(Whitespace\" \
         \"))))(Tile((id \
         04689137-41f4-4abb-b991-7270d8f295e9)(label(->))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 13))(sort Typ))((shape(Concave \
         13))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         53d6a173-67dc-4724-98c5-3dac59e67674)(content(Whitespace\" \
         \"))))(Tile((id \
         08d406a1-08b7-491a-9cfb-776f677152b9)(label(Int))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         ed31b1e0-ba74-4386-aea8-c21dd4cd2b17)(content(Whitespace\" \
         \")))))((Secondary((id \
         17371e27-7d17-418d-8ec6-1f738efd8c58)(content(Whitespace\"\\n\"))))(Tile((id \
         6770e3e8-0d14-4d21-af0b-54e0c3a243d7)(label(fun ->))(mold((out \
         Exp)(in_(Pat))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         37))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         cc4c45df-7e44-499a-8da4-1189e9a60424)(content(Whitespace\" \
         \"))))(Tile((id \
         07939868-851f-48d0-b4d1-de18d092b901)(label(c))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         7c6d5a36-86ff-4508-a771-2c2738f1066b)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         27a58658-c2bf-4db2-9399-aecd28a288ca)(content(Whitespace\"\\n\"))))(Tile((id \
         eb264588-70a0-4c77-ac5b-811c35c5b6ef)(label(case end))(mold((out \
         Exp)(in_(Rul))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         4b3322c5-ecc6-4c32-be99-92cf3262f604)(content(Whitespace\" \
         \"))))(Tile((id \
         4ae53ee8-b95e-453e-ba90-24277c1bfb41)(label(c))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         b25ac17a-59b8-444b-be16-6924e5f59191)(content(Whitespace\"\\n\"))))(Tile((id \
         2ede8449-7caf-4e1a-8c76-0ea2b8f2d339)(label(| =>))(mold((out \
         Rul)(in_(Pat))(nibs(((shape(Concave 46))(sort Exp))((shape(Concave \
         46))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         93e83735-3643-472f-99bb-540e89d2b096)(content(Whitespace\" \
         \"))))(Tile((id \
         9a296321-f36f-4810-af89-8c169446e01a)(label(Moonmelon))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         48b8be83-443e-417a-8a17-864232769539)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         deb64467-1f2c-4b21-954b-b71da2c07fb2)(content(Whitespace\" \
         \"))))(Tile((id \
         46e3d2b5-4abb-4351-b74d-089ff2f307e7)(label(15))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         48603956-9afe-4655-a37d-d71a7c4340d1)(content(Whitespace\"\\n\"))))(Tile((id \
         a97b56c4-1dd9-491a-94a1-631a0e91e6c4)(label(| =>))(mold((out \
         Rul)(in_(Pat))(nibs(((shape(Concave 46))(sort Exp))((shape(Concave \
         46))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         7dc6732a-4942-4f0c-8d24-36c5d1a36da3)(content(Whitespace\" \
         \"))))(Tile((id \
         c20b3805-0ef7-4c33-9b15-0ba7409146f5)(label(Starfruit))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         5c511aa3-3ad1-4c33-8851-cd90fc6a60fe)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         7f08decb-1f98-46e2-9847-ba7a8e10df21)(content(Whitespace\" \
         \"))))(Tile((id \
         56451092-4707-4398-90f6-ac95a8adac0c)(label(20))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         947a73c1-ef4f-41b8-8557-bc5371af825e)(content(Whitespace\"\\n\"))))(Tile((id \
         4b22031b-8e61-4c84-9cd4-936193cdc4f3)(label(| =>))(mold((out \
         Rul)(in_(Pat))(nibs(((shape(Concave 46))(sort Exp))((shape(Concave \
         46))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         8ad62676-1834-4070-9a1d-ce841819e7d4)(content(Whitespace\" \
         \"))))(Tile((id \
         411deeb1-d2a4-45cc-9e1f-1f018ba81079)(label(Nightshade))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         fbfadd08-dbd3-4c2c-a55f-2d02542b01d4)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         aa385281-7060-44e1-9ffd-3dc72c672e8c)(content(Whitespace\" \
         \"))))(Tile((id \
         b6c2b88c-3f1f-49f9-a6cb-92cbc408ebda)(label(20))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         ada0b310-a334-4760-9ced-b9770313cb9c)(content(Whitespace\"\\n\"))))(Tile((id \
         82ffa9ac-b4d6-42bf-a8de-851cece14228)(label(| =>))(mold((out \
         Rul)(in_(Pat))(nibs(((shape(Concave 46))(sort Exp))((shape(Concave \
         46))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         b865333c-b0dc-4786-bca3-3d2ad6980bae)(content(Whitespace\" \
         \"))))(Tile((id \
         45956f67-10b4-4086-8165-2db9b0bfb794)(label(Duskwheat))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         258f2f74-00ff-404e-95e0-4a7dde8f17fc)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         5a5a52a5-4016-42e9-aba5-2e6f6f8b035d)(content(Whitespace\" \
         \"))))(Tile((id \
         a72280eb-eb93-43d9-a218-8beab8026518)(label(10))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         d41c38bb-0f2f-42ee-847d-7cacca8ef988)(content(Whitespace\"\\n\"))))(Tile((id \
         985c8d01-ad74-4bf7-90eb-7e9a79d00ec3)(label(| =>))(mold((out \
         Rul)(in_(Pat))(nibs(((shape(Concave 46))(sort Exp))((shape(Concave \
         46))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         477b7e1f-f342-42c8-a26c-031942c9f814)(content(Whitespace\" \
         \"))))(Tile((id \
         d5ba0552-2105-455a-b7cc-112628e32017)(label(Glowpumpkin))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         bafa9618-3474-4f01-b93c-4c0c30b8a753)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         8646456d-f2b1-473f-8d77-23dc16415905)(content(Whitespace\" \
         \"))))(Tile((id \
         640a7be3-f4b0-43c6-9445-03d300436f19)(label(12))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         69e20235-8ab1-4cca-9294-240ad332582b)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         deadf32f-55d8-4951-ac4e-61452193d1e9)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         3661ca14-d336-4653-9f4f-54c369a68998)(content(Whitespace\"\\n\"))))(Secondary((id \
         3d9d071f-b2d0-466f-a3f0-7b7907c9a00d)(content(Whitespace\"\\n\"))))(Secondary((id \
         52a67303-99b4-4449-936a-2cc49e0f46d9)(content(Comment\"# Quality \
         multiplier for harvest value #\"))))(Secondary((id \
         435ff510-b381-4513-b33f-2e21c0c605f3)(content(Whitespace\"\\n\"))))(Tile((id \
         dea05a86-36b0-4964-a870-7b725f71f279)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         492403ec-3126-490b-ac46-5380c4d5c6ee)(content(Whitespace\" \
         \"))))(Tile((id \
         fba56fd4-4c3a-41f1-bdb0-d7a010206676)(label(qualityMultiplier))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         41229f1a-c0f2-4e19-9d39-0491c9f74d86)(content(Whitespace\" \
         \"))))(Tile((id \
         0637a275-dba4-463d-94f2-95f47a32bb25)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 24))(sort Pat))((shape(Concave \
         24))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         6d6f1e7d-e7e8-4501-aa49-618c811d0971)(content(Whitespace\" \
         \"))))(Tile((id \
         94b8001c-0b2a-4c8d-ba8c-af3eb2de2463)(label(Quality))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         26514bda-f09c-439c-8a46-2f671d1ade27)(content(Whitespace\" \
         \"))))(Tile((id \
         3c084992-951b-49e3-a79d-b1fdfb52aa48)(label(->))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 13))(sort Typ))((shape(Concave \
         13))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         734cd381-99ae-4991-bc6a-9c71e6772fdb)(content(Whitespace\" \
         \"))))(Tile((id \
         7ebb72b3-06cd-4845-9e6b-cfc392c2e4a2)(label(Int))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         6dcbcb1b-4eef-459d-8e90-9a65a1a8afda)(content(Whitespace\" \
         \")))))((Secondary((id \
         ffe832d8-2416-426d-b01f-db51b3bd0a05)(content(Whitespace\"\\n\"))))(Tile((id \
         1ad81dce-de9e-4b5d-9a75-65c2f3317b43)(label(fun ->))(mold((out \
         Exp)(in_(Pat))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         37))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         21a8d5f1-02db-40da-8fb6-e42292206da9)(content(Whitespace\" \
         \"))))(Tile((id \
         de6a1d2d-6313-48e0-9577-4a4865dfcbd7)(label(q))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         a5af6244-8595-4a51-8a1b-136cbad09466)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         d93bbdf1-3617-431e-b905-98f1b87ba0e0)(content(Whitespace\"\\n\"))))(Tile((id \
         9351ba58-12e4-4986-b3a6-8241f7a7f16b)(label(case end))(mold((out \
         Exp)(in_(Rul))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         720327a7-d93a-4a1e-b927-c3fc478545ee)(content(Whitespace\" \
         \"))))(Tile((id \
         699285a5-ff73-4f75-add2-f93795b10962)(label(q))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         dedd9d0f-8dfd-41be-951e-15040cf62c12)(content(Whitespace\"\\n\"))))(Tile((id \
         e49c0620-7776-467b-a4b4-d2ad2587458c)(label(| =>))(mold((out \
         Rul)(in_(Pat))(nibs(((shape(Concave 46))(sort Exp))((shape(Concave \
         46))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         a73da87d-a407-4647-88ae-4dd8f50931d5)(content(Whitespace\" \
         \"))))(Tile((id \
         3523d082-6f66-4647-96d3-d6f14de3069f)(label(Bronze))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         1b3b907b-73f2-4f21-97ca-edcc794cbff7)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         30f3a095-eb18-4237-924a-6a47ca105f07)(content(Whitespace\" \
         \"))))(Tile((id \
         902bcc45-c985-45db-b484-d95a87a65a9d)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         0dfa9b79-c23a-4c01-9ef4-ec3486525e06)(content(Whitespace\"\\n\"))))(Tile((id \
         ed94632c-76a2-4584-8607-cd2fcc0c8ba4)(label(| =>))(mold((out \
         Rul)(in_(Pat))(nibs(((shape(Concave 46))(sort Exp))((shape(Concave \
         46))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         e14ae0b9-0052-44a7-9bcd-ba0ca8ad8fda)(content(Whitespace\" \
         \"))))(Tile((id \
         a96f297c-cd94-4c7f-86cf-e97bd530fbfa)(label(Silver))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         951c5bad-f9cf-4200-bf10-8e67804534fc)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         2a2f45f8-7462-47bc-a2c1-cd682242bdfe)(content(Whitespace\" \
         \"))))(Tile((id \
         f3fa6b81-c6af-47a9-96b5-28087a3d4351)(label(2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         62e1ba05-3884-4512-933e-8a96b3c55afc)(content(Whitespace\"\\n\"))))(Tile((id \
         136451bb-5e82-4b1e-95c9-042a08add48a)(label(| =>))(mold((out \
         Rul)(in_(Pat))(nibs(((shape(Concave 46))(sort Exp))((shape(Concave \
         46))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         fa4a98ee-1268-419e-bae6-5d28742e267c)(content(Whitespace\" \
         \"))))(Tile((id \
         7d5ff854-1e83-44dd-bf68-3660246b0e22)(label(Gold))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         28a592d6-7525-4c90-98ed-89eb79a8fdd3)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         d6381011-d749-42a5-bdcb-c7b5194f2bd0)(content(Whitespace\" \
         \"))))(Tile((id \
         674344b2-02b9-46d8-a7e5-6fc4dfa29c3b)(label(3))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         fa4c5083-6ef6-45dc-8395-58d8ffadfa13)(content(Whitespace\"\\n\"))))(Tile((id \
         768fc616-8beb-4101-9a94-b0e7e4193727)(label(| =>))(mold((out \
         Rul)(in_(Pat))(nibs(((shape(Concave 46))(sort Exp))((shape(Concave \
         46))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         e2d9082d-ff01-4827-8f05-624df3a29340)(content(Whitespace\" \
         \"))))(Tile((id \
         15591a89-6862-41c6-9e74-20c88e138b34)(label(Starlight))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         03596c3d-e2b9-4209-8fd6-23348c517057)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         4c9cee48-5666-4049-8474-d8868caa8b4d)(content(Whitespace\" \
         \"))))(Tile((id \
         a8fd774b-0aa6-4e79-914f-422393132fef)(label(5))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         1166c734-dc4e-4430-a02c-7e4c62ddc865)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         0c26c6dd-7733-4557-91b4-78e71b288b81)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         a7a14d33-bf36-4805-be7a-29b40de29462)(content(Whitespace\"\\n\"))))(Secondary((id \
         9c495722-1c2e-44b9-bbe9-2671f2312c34)(content(Whitespace\"\\n\"))))(Secondary((id \
         fb11c483-f6f7-4ec3-bd6f-16ee40f7241d)(content(Comment\"# Calculate \
         the value of a single harvest #\"))))(Secondary((id \
         07493de9-c9f5-47a0-af81-43dfcc5ee432)(content(Whitespace\"\\n\"))))(Tile((id \
         d8cf5a4f-b9bb-405a-898b-f7bbfcda1c28)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         6a447f7b-187b-4080-99be-1fc01d2d5a12)(content(Whitespace\" \
         \"))))(Tile((id \
         29199db6-61c1-4600-b702-c3bcc02c9986)(label(harvestValue))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         5f4ef237-5ff6-4bf4-8912-a498243c202b)(content(Whitespace\" \
         \"))))(Tile((id \
         fb0240f6-10a1-49e9-8485-e682f0531fe3)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 24))(sort Pat))((shape(Concave \
         24))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         b2c0958c-4f8d-4627-9d16-22a8d5c7a073)(content(Whitespace\" \
         \"))))(Tile((id \
         a5b648bf-dafa-49ae-9995-c0372fddb3ae)(label(Harvest))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         24067846-1881-45e6-9022-bf141d360d7f)(content(Whitespace\" \
         \"))))(Tile((id \
         08fcc8f8-40dd-4911-a957-12351e9aae14)(label(->))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 13))(sort Typ))((shape(Concave \
         13))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         7699a661-cf20-4849-85ff-375f034c13e2)(content(Whitespace\" \
         \"))))(Tile((id \
         13da57f1-6e16-46c8-b90d-a4bed77f5fea)(label(Int))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         7cfb22ff-f420-40df-bc04-4f15447105f2)(content(Whitespace\" \
         \")))))((Secondary((id \
         051e1264-3cdd-4e04-972c-7498b7270715)(content(Whitespace\"\\n\"))))(Tile((id \
         48cc1a03-a26f-4c99-9f45-945d8807dbe7)(label(fun ->))(mold((out \
         Exp)(in_(Pat))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         37))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         10cb1e8b-ce4a-465b-92b7-ef99db6083c9)(content(Whitespace\" \
         \"))))(Tile((id \
         4db28486-696f-41fd-a934-85316890887f)(label(h))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         8e39515c-c3ca-4fa3-9289-37aa065528ac)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         8cd65c02-4904-44c2-80f3-45b1d81a7983)(content(Whitespace\"\\n\"))))(Tile((id \
         d9d69eb9-b847-4e04-9c04-4a9eb3ba35a0)(label(cropValue))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         4f582854-fb2d-4fff-bac3-55b2e8178a8f)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         bfe3dc81-7557-4080-8174-b854cb1cb6f5)(label(h))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         a9af343b-c7be-42d9-8c42-e46cfb1343ca)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         e4192bd5-f89a-448e-bf20-6d0690e0f59f)(label(crop))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         0b4b5b1b-b1e4-45ba-8d6b-bbffb276359b)(content(Whitespace\" \
         \"))))(Tile((id \
         d3a54afb-af5d-4ce4-99a7-8ad5ebaaeafd)(label(*))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 27))(sort Exp))((shape(Concave \
         27))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         ec8158d8-286d-41f1-81fc-bbdf35eb6b87)(content(Whitespace\" \
         \"))))(Tile((id \
         c32470b0-1369-4748-b11d-52f157d169b7)(label(qualityMultiplier))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         864d19f9-4df2-4426-9c43-1307f34767ac)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         2fd003c0-8d39-4c31-84f0-38e8085c8d5c)(label(h))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         79c29ce6-7af9-4296-8fbd-f53418f6d897)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         655948b3-becc-4594-9d1a-11c2e6a70b52)(label(quality))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         ab4668bb-db49-41e8-a4bb-7dfb2fd8a99e)(content(Whitespace\" \
         \"))))(Tile((id \
         bfa1df7b-b1eb-4644-96c9-7cd9b46bc872)(label(*))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 27))(sort Exp))((shape(Concave \
         27))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         827b97f7-f308-4196-a96a-0b76f1930b72)(content(Whitespace\" \
         \"))))(Tile((id \
         f0db4e9a-85e5-446e-8e79-1b98c53f12fe)(label(h))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         7f9b4ccb-7bc5-4d55-957a-0f029c6d029f)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         68cb2abe-57d6-4247-8deb-8777eadee4bf)(label(quantity))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         3b7289bc-ec45-4185-8705-bd3f4a381b3d)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         fc8bc2e3-9ef3-4d68-85b2-f91b669fd172)(content(Whitespace\"\\n\"))))(Secondary((id \
         bedc8dee-a35f-42c6-bbc1-84b37a82768d)(content(Whitespace\"\\n\"))))(Secondary((id \
         063ab054-5f22-4096-80e4-4823cbc32270)(content(Comment\"# Initial \
         empty ledger #\"))))(Secondary((id \
         4606d88e-7878-465b-8cd0-f013c35c3a91)(content(Whitespace\"\\n\"))))(Tile((id \
         c96c1605-450b-41a5-adb9-b97ef753168e)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         c782fdca-7814-4c22-bf4b-739732a1b5a7)(content(Whitespace\" \
         \"))))(Tile((id \
         7f1447b9-6153-41f7-9181-f0dfeb39f050)(label(initModel))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         8eb51b3f-1981-446f-a1c9-c4cf717a7d20)(content(Whitespace\" \
         \"))))(Tile((id \
         ae509000-3872-4454-981c-425da98c2ade)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 24))(sort Pat))((shape(Concave \
         24))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         798d9496-c9e1-4684-af5d-9ce20777f64a)(content(Whitespace\" \
         \"))))(Tile((id \
         a8dbb3e6-9f4f-4c42-a809-1a526264a954)(label(Model))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         35585bff-c90c-4c63-97d4-530c3716eae2)(content(Whitespace\" \
         \")))))((Secondary((id \
         f5760338-177b-44b4-94a5-e91af2eea4d0)(content(Whitespace\" \
         \"))))(Tile((id \
         e4940b81-f4bf-4c0a-9828-957721712f90)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         3a29bfc8-e90b-4e38-9e6a-f9716a0c5007)(content(Whitespace\"\\n\"))))(Tile((id \
         ec03bf53-e693-40f5-95da-830433152361)(label(harvests))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         082e44d2-09f7-4784-a1fb-d83df42c048e)(content(Whitespace\" \
         \"))))(Tile((id \
         65640f77-6158-4a3d-b108-e5180dbf749b)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         a726312d-4567-4ac3-a9b3-e006f33d1b5a)(content(Whitespace\" \
         \"))))(Tile((id \
         cd7615a0-7b0b-4117-868b-6341a1b01eca)(label([]))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         4860c732-ee9c-4fd0-b706-e25b82a11d2a)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         6c77a8ea-b6a1-4a04-bed1-1e70c7cefc4b)(content(Whitespace\"\\n\"))))(Tile((id \
         1aeeedc9-15ed-4a9a-8596-109b6c2b7ee6)(label(totalValue))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         6e80f253-7ee4-489c-be8f-4c6784611ddc)(content(Whitespace\" \
         \"))))(Tile((id \
         4c9a0119-f5a9-4682-85f6-5d179f958dd8)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         4495a3f5-65ea-4382-af20-3dc9bb8dae0c)(content(Whitespace\" \
         \"))))(Tile((id \
         2c3b1381-048c-4129-8ac2-0cf148389546)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         7232ad9e-1d8a-41b5-8891-bc3121a87b53)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         5f1dece3-ae2b-4495-baa7-7e431bc36d58)(content(Whitespace\"\\n\"))))(Tile((id \
         dadd9040-a138-4b99-9c1c-622068dc8a5e)(label(streakBonus))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         54cefcfd-2ba9-43da-8b21-91fa505cc025)(content(Whitespace\" \
         \"))))(Tile((id \
         dddb198a-b47b-41e7-a661-acf04c4a17db)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         b9c6fbb8-3a89-47dd-8b8e-b3b8ccd0671c)(content(Whitespace\" \
         \"))))(Tile((id \
         3a74a1bf-2892-44ad-9451-ae84e7fdf336)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         522d6fed-9846-4f51-a4fa-6fd3bb12d39a)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         8d9d941e-b041-4959-859c-5d5abf89aed0)(content(Whitespace\"\\n\"))))(Tile((id \
         6a025614-4952-4e30-a990-0e0802d17112)(label(lastQuality))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         be2ea979-decf-4471-9ce7-8f55144dd4c1)(content(Whitespace\" \
         \"))))(Tile((id \
         fb83a2bb-55df-4328-9b59-6eee8b7c0e7c)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         d7f268d0-1f3c-4b90-a14d-5b5d1a0fc34b)(content(Whitespace\" \
         \"))))(Tile((id \
         1705ac55-9083-4c37-9032-ffedc85c3c84)(label(Bronze))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         1972c54f-74dd-462f-86d1-d367340f50f4)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         458e2d6a-b1e0-4078-b114-1206769965a0)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         b1637bda-4b71-4e22-83bb-2a634a17ddc6)(content(Whitespace\"\\n\"))))(Secondary((id \
         e392d6f4-aeed-454c-a549-085768f2827e)(content(Whitespace\"\\n\"))))(Secondary((id \
         4cc01295-ef54-4738-ba4d-f42269fe27e5)(content(Comment\"# Process a \
         harvest action and update the ledger #\"))))(Secondary((id \
         4f66e19d-9c3c-4b23-ad23-f0f68fecaeb5)(content(Whitespace\"\\n\"))))(Tile((id \
         3d93677b-7083-436b-92b2-55a9d8e658ef)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         68b072fd-eded-4168-8dcc-d18147794923)(content(Whitespace\" \
         \"))))(Tile((id \
         1bf2a472-4d7f-46d4-8709-ec415d7c5f56)(label(processHarvest))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         e764b05c-f3ea-4ab9-b6ca-f5844025f4e3)(content(Whitespace\" \
         \"))))(Tile((id \
         63630194-a082-4f09-86be-01f3a325db51)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 24))(sort Pat))((shape(Concave \
         24))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         ce515325-0c8b-4905-9663-95b12d507bdd)(content(Whitespace\" \
         \"))))(Tile((id \
         d7ff0389-c957-473f-8253-feb612cca9e8)(label(\"(\"\")\"))(mold((out \
         Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0 1))(children(((Tile((id \
         fc40de1f-7d0f-46d4-b1bc-30781dec2e7b)(label(Model))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         5d7646c7-6f91-4e8f-8d93-934e02838b0a)(label(,))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 44))(sort Typ))((shape(Concave \
         44))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         6fb2a24d-b5dd-43a4-b8b5-719e3176ba61)(content(Whitespace\" \
         \"))))(Tile((id \
         94e8ba89-179a-4358-8522-82e02457acdd)(label(Harvest))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children()))))))))(Secondary((id \
         4fd73339-7e3f-4108-a3a3-7c262406e4fb)(content(Whitespace\" \
         \"))))(Tile((id \
         ce527494-87fb-4952-a74a-3190f6d74ce1)(label(->))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 13))(sort Typ))((shape(Concave \
         13))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         bc743a34-50c0-4df2-a1eb-52812d43b84a)(content(Whitespace\" \
         \"))))(Tile((id \
         394f329b-9ca3-46df-a75f-84c6aeef5e0c)(label(Model))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         757b1267-ff04-4299-8a8b-4d19663e7865)(content(Whitespace\" \
         \")))))((Secondary((id \
         30033897-00da-4349-8ae4-eaa2fcb32b29)(content(Whitespace\"\\n\"))))(Tile((id \
         70dc5604-7169-4085-8c17-9193509c3898)(label(fun ->))(mold((out \
         Exp)(in_(Pat))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         37))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         20b12f69-4065-4a5f-8402-4bb1c0a10915)(content(Whitespace\" \
         \"))))(Tile((id \
         66167861-d834-45c9-9497-70b53df7476b)(label(\"(\"\")\"))(mold((out \
         Pat)(in_(Pat))(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0 1))(children(((Tile((id \
         32251d75-95d1-427e-bc33-d2225e9e7e60)(label(ledger))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         5f258f09-0439-4d78-8d17-82ca9ac5d35a)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 44))(sort Pat))((shape(Concave \
         44))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         728a0aba-d382-4af0-a4a7-a6f64947754a)(content(Whitespace\" \
         \"))))(Tile((id \
         9803a29b-92ca-48c5-9fdf-f0e688d495b4)(label(h))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children()))))))))(Secondary((id \
         06af0439-0d99-4169-b001-660847b1c422)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         91c50518-61ab-4a45-bbca-049b673faef3)(content(Whitespace\"\\n\"))))(Tile((id \
         332fcace-00ba-46b8-aae3-598a362c0604)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         33dbb634-5fba-4037-bfd4-6d61a6ed8dda)(content(Whitespace\" \
         \"))))(Tile((id \
         c10c858d-9661-45fa-875e-b93f83aff261)(label(value))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         a6f2816b-679b-4bc6-ba28-43259537ae38)(content(Whitespace\" \
         \")))))((Secondary((id \
         e51524a4-042d-4381-a5a4-373a96434c7d)(content(Whitespace\" \
         \"))))(Tile((id \
         1f767533-01c2-41a0-b041-ed8c9401cbdc)(label(harvestValue))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         ba49c1e5-df63-4c42-b526-08646e541576)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         c022b7a3-88b2-41b3-af47-f0778c46fb93)(label(h))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         ee7215a6-23b2-4a4a-b1ed-4be564ba8416)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         406df6bc-eda6-4568-95a2-e5a0173388b5)(content(Whitespace\"\\n\"))))(Secondary((id \
         a878d104-fa93-4908-9153-b5d5fe7fe4ed)(content(Comment\"# Check if \
         this harvest continues the quality streak #\"))))(Secondary((id \
         8f24373f-8725-4397-b8c5-51e374a1475d)(content(Whitespace\"\\n\"))))(Secondary((id \
         c43d614b-4227-446a-8c2d-923f1991ad15)(content(Comment\"# First \
         harvest never continues a streak (no previous harvest) \
         #\"))))(Secondary((id \
         6a133f90-4fec-4849-9972-4671d0172520)(content(Whitespace\"\\n\"))))(Secondary((id \
         7aee190c-a8b1-4dc7-8cc7-ca7e411eba56)(content(Comment\"# Compare \
         current quality with the PREVIOUS lastQuality #\"))))(Secondary((id \
         66576bb7-14f5-4cf5-b311-1238f9678746)(content(Whitespace\"\\n\"))))(Tile((id \
         580671d1-ab45-4431-8465-6502b90c4711)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         923a9caa-ba6c-425a-99d1-94c216aaccf7)(content(Whitespace\" \
         \"))))(Tile((id \
         5d004ed4-2a9d-4c14-9999-afa54946218b)(label(isFirst))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         62a0b875-1ea2-4f3b-a9e6-a5b178a75e50)(content(Whitespace\" \
         \")))))((Secondary((id \
         b1f768c2-a70a-44e8-b8db-c6a903c4e58d)(content(Whitespace\" \
         \"))))(Tile((id \
         3a4b4af9-14d1-44f2-8826-717adee69524)(label(length))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         52ab97bf-f9c6-4012-8bb1-31b4eee98d1d)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         8a370e63-8b0e-4fbb-84c5-cb81b660a2bc)(label(ledger))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         086be2be-6492-47cc-b491-3d1327ecf635)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         c2d3dd34-9b15-43d9-a0c0-da00e87a5d0d)(label(harvests))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         b2e8bc99-014d-4e01-b5d8-6495c26a839b)(content(Whitespace\" \
         \"))))(Tile((id \
         7deee32f-ca0d-432f-9acb-fbba23e7c03e)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         9908a597-1ae8-4493-8c28-55aefee3f3a7)(content(Whitespace\" \
         \"))))(Tile((id \
         2e632ecd-643d-453b-be15-889d0bfc24a3)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         7c4ba925-335e-4638-bde6-f850a5695bf6)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         507614c7-27f9-45e4-95db-ca23261fc414)(content(Whitespace\"\\n\"))))(Tile((id \
         196e1004-6e53-49ff-a35e-7b22a6346cac)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         34f0f7c8-331c-4d76-9825-a87b10c00db1)(content(Whitespace\" \
         \"))))(Tile((id \
         f8ad138a-174e-41e4-894f-0894f967360c)(label(continues))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         fd86af01-60a2-4211-993d-19dc1497d1b6)(content(Whitespace\" \
         \")))))((Secondary((id \
         a980fe92-d056-4d2a-9a25-d0a1246aeb16)(content(Whitespace\" \
         \"))))(Tile((id \
         370d6e6f-a8c6-4f23-8e9c-620c87a3db83)(label(!))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape(Concave 27))(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         8b9c376c-bc85-4a59-bf45-d4109926e91a)(label(isFirst))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         e001f2e1-c253-4b85-8d9c-f038f8eebf7a)(content(Whitespace\" \
         \"))))(Tile((id \
         672c3f1b-be7a-4d79-ba5b-6aa53a8d35ec)(label(&&))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 32))(sort Exp))((shape(Concave \
         32))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         4dfb34f5-0fec-4119-a730-c709f2c18c72)(content(Whitespace\" \
         \"))))(Tile((id \
         4bfc5666-6ead-4eff-9a03-63d6de9f235a)(label(h))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         d40a3cbe-fcf3-4a26-8bf5-3e37ba7c3e41)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         3ca954b7-d8a2-48de-91d4-c8f4b5911c0e)(label(quality))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         8098ac57-9ff2-43a9-9777-6aae8dd85162)(content(Whitespace\" \
         \"))))(Tile((id \
         731a0fa6-307c-4a8b-91e8-ceb898b09d55)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         ad1eddbf-5f8c-423b-b561-efbbe505ecc4)(content(Whitespace\" \
         \"))))(Tile((id \
         568fea51-b678-4839-8c23-c5a8f338d303)(label(ledger))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         6ba35b3b-ece1-41a7-9251-3b33c7f7b441)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         cf517f1a-5f07-48d4-b763-523144473931)(label(lastQuality))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         89dbc24e-12fe-42b4-b3f9-a8e4193dcd41)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         d572ca9d-8c2c-4146-b878-b0abf78aea00)(content(Whitespace\"\\n\"))))(Secondary((id \
         c8cd9ec6-1efd-4bc5-8f8f-dc014776b440)(content(Comment\"# Now update \
         lastQuality to current harvest #\"))))(Secondary((id \
         0cb03d15-8861-4ca5-a03a-6801863e2300)(content(Whitespace\"\\n\"))))(Tile((id \
         62bbf98f-12a5-4e4d-9fab-1719ae954491)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         b0c39c20-4e5b-44f0-86ad-2b7b19f9651d)(content(Whitespace\" \
         \"))))(Tile((id \
         ed369c0f-7364-4064-bd8a-cfea3785dc7e)(label(newLast))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         08b0c796-e32e-4d98-b617-f230d7921cd8)(content(Whitespace\" \
         \")))))((Secondary((id \
         fe541515-d286-4608-a721-b1996d10470c)(content(Whitespace\" \
         \"))))(Tile((id \
         9390af20-c307-426b-990a-02c8a58be848)(label(h))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         1e709188-dc52-4ca5-9152-2e8d15970d93)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         22ecbb7e-4f61-4392-83a1-9f290cb7758c)(label(quality))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         f99ce21a-e11a-483e-a65f-a827577fd28c)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         8afd82f5-1aaa-4a60-9a29-61c3f54af42d)(content(Whitespace\"\\n\"))))(Tile((id \
         e5c27c6d-1fd6-4c3d-a73e-fce40dc98ccd)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         99124c22-de72-4d67-be3d-2b43c1fa73c9)(content(Whitespace\" \
         \"))))(Tile((id \
         59179d3f-d662-42e3-8218-cbc30b147777)(label(newStreak))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         4b2939d1-ca9b-41eb-9dc6-f088934e29c2)(content(Whitespace\" \
         \")))))((Secondary((id \
         a323c81c-7bba-4518-a2fc-15e8c5ecd2a9)(content(Whitespace\"\\n\"))))(Tile((id \
         ef40f6a7-d087-47d7-aebf-f7f7677b8181)(label(if then else))(mold((out \
         Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         36))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         f0781fa7-cfe4-453d-a2cc-7a0ed982ab07)(content(Whitespace\" \
         \"))))(Tile((id \
         59d4fabb-aa40-4572-9c80-d469432dd31b)(label(continues))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         433ff1c8-37b4-4ad0-9ade-c3f78689d18e)(content(Whitespace\"\\n\")))))((Secondary((id \
         145c7525-4075-4385-99cb-af8f2594bb4f)(content(Whitespace\" \
         \"))))(Tile((id \
         c79c337d-b598-4552-a74f-10da82d75c14)(label(ledger))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         07b20a82-6a1d-4d36-8cf0-f3d00dabdf0d)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         6e4c7403-a7b1-4065-b463-e6b398e2a33a)(label(streakBonus))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         aaa6c586-c50b-433a-b7aa-ee4ec0b746cf)(content(Whitespace\" \
         \"))))(Tile((id \
         d5d45e6b-a4bc-478c-ba75-522cac698e92)(label(+))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 28))(sort Exp))((shape(Concave \
         28))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         c4155a71-cbd5-4383-899b-dbdaff9f9967)(content(Whitespace\" \
         \"))))(Tile((id \
         d261c6ce-2f75-492b-87f2-f24c8b538aa1)(label(5))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         420d195c-a526-4c48-94da-7c6d22a69acb)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         66df7b32-c865-48f5-ad28-89f247e09525)(content(Whitespace\" \
         \"))))(Tile((id \
         e8c7b19c-737c-490c-b486-3e646e82ea5e)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         5bd2c56d-0993-4981-abf3-e4dd444f0659)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         384cda83-c6eb-46b7-8c83-929d2e51543b)(content(Whitespace\"\\n\"))))(Tile((id \
         9aafa54f-8c13-45a7-b799-f9bd88dc1396)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         9cbd70d1-d794-4691-901c-99456ce7ef2d)(content(Whitespace\"\\n\"))))(Tile((id \
         b4025efd-9af9-4927-845e-52f3efb7c30b)(label(harvests))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         19b07365-2eef-449f-a387-2ce70042cc1a)(content(Whitespace\" \
         \"))))(Tile((id \
         4d167c20-fa4e-45fe-9d47-9888a5b0aaef)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         9c5ee3c6-1c9f-4998-b833-8b8a5728aa17)(content(Whitespace\" \
         \"))))(Tile((id \
         a5895463-eb9d-4d13-b1f2-b2becde91c76)(label(h))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         98d0b72e-9f56-4130-a341-533a3b4661ad)(content(Whitespace\" \
         \"))))(Tile((id \
         004807b0-e369-4086-8ccb-408c8a00fe50)(label(::))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 29))(sort Exp))((shape(Concave \
         29))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         edd7a6ec-53fb-41bc-ab20-0f3d2946baab)(content(Whitespace\" \
         \"))))(Tile((id \
         5f57edec-d007-4ed0-a745-23a616cf5475)(label(ledger))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         7f5c8072-98f1-407c-b2e6-535519393b92)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         3995b3ba-9b95-4c16-ad78-3686bcb99ebe)(label(harvests))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         b69bc997-e763-4ffd-a166-83199e99e26b)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         aad121ab-2ad0-40e6-9ca4-766d57f6404f)(content(Whitespace\"\\n\"))))(Tile((id \
         8ce283f5-03e3-4fbe-94ab-a4a4ffb68942)(label(totalValue))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         8850af88-7fa8-4c1a-a3b4-10e9d825fe7f)(content(Whitespace\" \
         \"))))(Tile((id \
         93ee46b7-c88f-42e6-9c57-d7610bd874a5)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         641a7161-00c5-48a0-a040-ade72283b79c)(content(Whitespace\" \
         \"))))(Tile((id \
         24628219-6c22-41c3-a5dc-0065e0ccebe1)(label(ledger))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         856e7494-df90-4a95-9630-b4d1d389fadb)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         ce53c450-c9c3-42eb-bfd0-21620d87e3c9)(label(totalValue))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         9fa0aaf7-89a7-4fc3-aca0-e655756a2ad0)(content(Whitespace\" \
         \"))))(Tile((id \
         baa9c8ab-c1d5-4b48-8ffc-41099e7ad034)(label(+))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 28))(sort Exp))((shape(Concave \
         28))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         bbf5aaad-1076-41f4-8152-26b1af75e3a1)(content(Whitespace\" \
         \"))))(Tile((id \
         3536d125-0f6c-4da4-9a7f-0d03126cc31d)(label(value))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         b7c75fab-5744-4e1e-894c-797eed37a96e)(content(Whitespace\" \
         \"))))(Tile((id \
         1685250b-8ed3-4b38-be17-44d69570a324)(label(+))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 28))(sort Exp))((shape(Concave \
         28))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         7f6a61bc-c38c-4b28-b378-9abd1c2c82bc)(content(Whitespace\" \
         \"))))(Tile((id \
         816dbd3a-2439-4691-a2dd-772a802bff3a)(label(newStreak))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         d39e222b-3f8b-422f-9b3b-f1c8cdfd3e12)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         7cd368d5-62ef-48d4-b69a-0a3603143116)(content(Whitespace\"\\n\"))))(Tile((id \
         46babc82-c173-434a-92e3-9be397bf8f86)(label(streakBonus))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         dd3e9328-9f18-4d4e-b978-34c9d6ab49b4)(content(Whitespace\" \
         \"))))(Tile((id \
         040ebd2b-95f7-425c-8e56-ae6a12bb4200)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         bd678a0d-af01-4f2b-a5e1-79004c74f61c)(content(Whitespace\" \
         \"))))(Tile((id \
         acb7dbcc-d330-4a45-90ab-6cdd7e2cb613)(label(newStreak))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         807f125f-c156-4546-a132-8d1a243b7c9e)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         086bb8a3-a5be-4298-9060-4a50feae204e)(content(Whitespace\"\\n\"))))(Tile((id \
         c5d20c7e-94bb-4929-8292-cccef5f9ce5e)(label(lastQuality))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         85f88250-ac88-4344-9944-de9b37b8f0b4)(content(Whitespace\" \
         \"))))(Tile((id \
         f8a6e2cb-8e48-4ac2-b785-ddaecdece4f3)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         ca80d4e4-9e7f-48d0-b72c-4929df26b868)(content(Whitespace\" \
         \"))))(Tile((id \
         1528935f-8627-4b73-b1f3-f2e655c80dce)(label(newLast))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         3fd17811-fbaf-45ab-b67c-fe7b13df0a11)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         4c6a8ef1-f041-45a0-9431-64bb5c0a6801)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         2b39f013-9c97-459d-9424-6b20b15350e2)(content(Whitespace\"\\n\"))))(Secondary((id \
         52fd0157-13bc-40d4-8ac4-e2f87b82b3d7)(content(Whitespace\"\\n\"))))(Secondary((id \
         65c8caf9-e80a-461f-8ba1-6486841ae62e)(content(Comment\"# Claim the \
         streak bonus and reset it #\"))))(Secondary((id \
         19f4d79a-4589-4691-a9f5-452e46fce662)(content(Whitespace\"\\n\"))))(Tile((id \
         3f250966-5c44-4d68-824a-890033d60dfb)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         bc709c46-0c93-47c9-aa2e-7d2f21fac0e6)(content(Whitespace\" \
         \"))))(Tile((id \
         0abdfe2b-526b-4eb4-b68e-b96289b4f03e)(label(claimBonus))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         08643433-e9fe-4057-a9d8-4fad0c3c1fbb)(content(Whitespace\" \
         \"))))(Tile((id \
         36c9144d-e0ab-4b13-a784-8f59f6333a30)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 24))(sort Pat))((shape(Concave \
         24))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         4133fbde-09a2-40a6-b23e-ebbc11170af5)(content(Whitespace\" \
         \"))))(Tile((id \
         6e95dac9-cf6c-4108-91d6-9677c7b2387d)(label(Model))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         be5bc7a4-dc6a-4874-add2-1974be575d44)(content(Whitespace\" \
         \"))))(Tile((id \
         d61daa50-eb54-4059-bd7b-5d842d4422dd)(label(->))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 13))(sort Typ))((shape(Concave \
         13))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         15b20dc0-c889-4a39-b9bc-99464804f224)(content(Whitespace\" \
         \"))))(Tile((id \
         fdbbb3a6-5899-45a7-8bfb-7133104f3f07)(label(Model))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         89d308f1-42e6-4200-9b05-fa6efcec2edb)(content(Whitespace\" \
         \")))))((Secondary((id \
         41a67223-4661-4c3d-b5ee-ee5e8422989b)(content(Whitespace\"\\n\"))))(Tile((id \
         97c65364-64ec-4930-9f73-4a6a574173ad)(label(fun ->))(mold((out \
         Exp)(in_(Pat))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         37))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         ca4b6323-9c8f-42ae-9649-2e254f256086)(content(Whitespace\" \
         \"))))(Tile((id \
         2c6304bf-83ce-441d-9025-1080cd9c0d57)(label(ledger))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         c6fa3df0-c635-4240-83c0-97fc1f5f4b20)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         cdac7493-7200-4920-9994-560b5a334489)(content(Whitespace\"\\n\"))))(Tile((id \
         bf874378-2d5e-4d0b-ab37-e72d8fca6c05)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         e62f432d-c349-4dba-b77b-f231ce50f7c0)(content(Whitespace\"\\n\"))))(Tile((id \
         9c06f0fd-73c4-4768-a7b6-3372c51da2fc)(label(harvests))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         ba44f4b4-b32d-4be5-9ca5-d878fd9aa9b7)(content(Whitespace\" \
         \"))))(Tile((id \
         21efb2b8-5a0f-4625-b65a-d753a3febeb8)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         ff14c4f0-6e0a-4f7f-bf8c-040fea7e6c22)(content(Whitespace\" \
         \"))))(Tile((id \
         09c4727c-2a9e-4176-b45e-a563ab569057)(label(ledger))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         a9e1ee47-1e58-4288-86ff-f08f19671ec1)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         49e7da23-38ad-4539-ae7c-68269f60e23e)(label(harvests))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         2ad38a62-13ee-4a1c-a57c-939ecc7906f4)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         17d478c1-85e0-451f-ab76-a34ac11b13a0)(content(Whitespace\"\\n\"))))(Tile((id \
         749a872c-1593-4dfa-b8e5-e83346cf07ab)(label(totalValue))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         7eebce2e-5977-4631-8151-0912d688d6ac)(content(Whitespace\" \
         \"))))(Tile((id \
         80e1db6a-83e7-4022-a0ab-bc0c67c9da98)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         d516eb03-9e24-40aa-99a1-7c245eaca289)(content(Whitespace\" \
         \"))))(Tile((id \
         f33f337c-7f79-40ac-aa02-04850d844b4b)(label(ledger))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         49b67aab-b342-4251-ac87-dcb452bc1568)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         b9f880b2-a14b-4ca5-a830-0098268931f3)(label(totalValue))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         c4ddff07-7a3b-4baf-9548-de8797d89ce8)(content(Whitespace\" \
         \"))))(Tile((id \
         e4c4809b-02d3-4277-b9b0-74815a418164)(label(+))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 28))(sort Exp))((shape(Concave \
         28))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         dca00789-7215-42f8-8df1-606d59b4b0d0)(content(Whitespace\" \
         \"))))(Tile((id \
         73328a39-f5f3-4a1b-9749-15a00e390c8e)(label(ledger))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         995bf1b9-0324-46c3-a621-b2909b4d69f7)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         cf30e3d4-fdc8-434c-91ad-e6559db3f1b7)(label(streakBonus))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         0c990c6e-5943-4879-a400-914d73c857ed)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         047d6941-4ab9-4fa8-9661-41fc1cea46e3)(content(Whitespace\"\\n\"))))(Tile((id \
         659c5930-b306-4020-959f-436e9a0c740d)(label(streakBonus))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         f0e83b88-efe4-49e4-96f5-5928bfe34354)(content(Whitespace\" \
         \"))))(Tile((id \
         721a02c1-5519-43bc-bc35-a8de5e9468c2)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         19434794-cd69-48b3-a29d-2a4f747e5dfa)(content(Whitespace\" \
         \"))))(Tile((id \
         7846b255-9840-482b-a3cc-999062182f06)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         a21797db-3b4b-4464-95dc-4369ccfdbc65)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         1ee7a91d-7c7d-437e-ba7c-68f04cd3ee53)(content(Whitespace\"\\n\"))))(Tile((id \
         9a93a4e8-d2b0-4aa7-adc6-6f25241d96db)(label(lastQuality))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         895dd170-c73d-4c6b-8189-938ffd3f7670)(content(Whitespace\" \
         \"))))(Tile((id \
         99545a4c-060a-4207-b558-1bf6533ae58d)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         08beea51-9e66-4a3f-9330-dd4a00556712)(content(Whitespace\" \
         \"))))(Tile((id \
         c7cbb97f-fb27-44c6-b0dc-ce4b1570b1e4)(label(ledger))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         24adce39-861a-4064-8759-374e6d36e784)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         29bb5c09-fbd0-4325-bbf4-1ddf0ca83de9)(label(lastQuality))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         fa45b5e2-fd79-4005-97f9-f9d99aeb8996)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         53f29915-2b76-4a67-b6ea-b29f67445e22)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         4055e8e5-14fe-420f-af06-870b5b03c737)(content(Whitespace\"\\n\"))))(Secondary((id \
         9827a9e0-9d73-44ad-a43d-ff32a32e5a93)(content(Whitespace\"\\n\"))))(Secondary((id \
         64d4fcbb-d981-4828-846e-bc3529e9dcd9)(content(Comment\"# Close the \
         harvest day - reset streak tracking #\"))))(Secondary((id \
         c9e75aad-f137-4148-bc6a-dd75926a0582)(content(Whitespace\"\\n\"))))(Tile((id \
         6e3a3583-e56e-49b0-b6cc-936bd773ca26)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         eed132d4-d6bd-437a-8ab9-15006b9714a4)(content(Whitespace\" \
         \"))))(Tile((id \
         58b45fcc-bb04-4042-acb7-4729a32a73b5)(label(closeDay))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         3e6f9387-a94f-4e3c-842b-cd42b4ce6010)(content(Whitespace\" \
         \"))))(Tile((id \
         1c54dfa2-88f7-43dc-8cbc-d65812715017)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 24))(sort Pat))((shape(Concave \
         24))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         c5f449ad-12e0-4969-a6b9-7103891a28f6)(content(Whitespace\" \
         \"))))(Tile((id \
         7066810f-5c62-4371-9417-9d012a1a48ac)(label(Model))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         9fdd78c8-b510-4e99-bdb0-aa42f50f7e2f)(content(Whitespace\" \
         \"))))(Tile((id \
         2a2b8def-139a-48f4-95c6-30b158ab1e82)(label(->))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 13))(sort Typ))((shape(Concave \
         13))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         25535e9f-ebfb-44b0-9a6b-5c1466d300e8)(content(Whitespace\" \
         \"))))(Tile((id \
         25f869a8-d0b0-4b68-9a3a-f7265f80eb30)(label(Model))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         bf83b12d-87fe-4f1d-98e9-24d1e4f08bca)(content(Whitespace\" \
         \")))))((Secondary((id \
         fef1064d-c2a4-472c-804f-9e43512d4045)(content(Whitespace\"\\n\"))))(Tile((id \
         296e38d5-e2a7-4f8a-938b-74d2a765f92b)(label(fun ->))(mold((out \
         Exp)(in_(Pat))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         37))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         56f7df5b-6da5-470d-9e61-2c4a78e29edf)(content(Whitespace\" \
         \"))))(Tile((id \
         b044a4b1-695d-4a0f-a6ba-c9e2f57d39e6)(label(ledger))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         7978eef3-313d-4581-81c7-7caf926f426f)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         70ac7452-47fe-4504-8a90-4675f7ef4af5)(content(Whitespace\"\\n\"))))(Tile((id \
         2a343f98-fe25-4277-b191-1f7e478e2fb1)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         bf3fbfc3-3da8-4dc6-80e8-478ba161f28f)(content(Whitespace\"\\n\"))))(Tile((id \
         960536c8-6379-435d-a6b5-ed2636c1ae0a)(label(harvests))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         6bd55690-2fd5-4a47-8c4f-82d90f8f5aba)(content(Whitespace\" \
         \"))))(Tile((id \
         aaa1618c-1215-479d-bb78-7c676f10ea11)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         8a5cce00-8d47-4f51-b426-2a3f84534710)(content(Whitespace\" \
         \"))))(Tile((id \
         80ecaa69-9df6-4064-b68b-2ec030b06eb2)(label(ledger))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         132c11cf-93ce-4830-9013-a52e024748d3)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         11a24d85-3e0e-420a-accf-246712a63981)(label(harvests))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         ca85a9e7-5d3b-4f1c-a57c-255de4f94811)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         fd094a06-863d-4bb7-8bb4-d84897025e47)(content(Whitespace\"\\n\"))))(Tile((id \
         29ea1167-eab3-4f0c-b746-4f51cb0ae2be)(label(totalValue))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         f995a70b-5c75-4403-a3b1-9eb7449711b5)(content(Whitespace\" \
         \"))))(Tile((id \
         ffb06f78-d851-42ec-a5d3-0b0cf9853c0f)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         d1655030-742e-463f-8823-3004158c3343)(content(Whitespace\" \
         \"))))(Tile((id \
         252f7742-b893-40b8-a7a2-734cfb6f8038)(label(ledger))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         d2fd6596-9f4a-4995-86b2-d99f15fc56a9)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         813fade9-ad95-4a90-a41b-3c902f03423a)(label(totalValue))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         1a5aa0d6-f395-427e-a00b-153dfb9fd82a)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         c150ea1f-8649-4176-a25f-1d2ee180be81)(content(Whitespace\"\\n\"))))(Tile((id \
         2ad2c76a-0ffb-4bf8-b73a-fe90d2d2bc88)(label(streakBonus))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         2d81818e-0a88-406c-88ba-0f59198e4c04)(content(Whitespace\" \
         \"))))(Tile((id \
         ca44f24a-4f71-4658-9f19-f295028db1f4)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         cac8fec4-eede-42a9-a097-d21633ca9667)(content(Whitespace\" \
         \"))))(Tile((id \
         9d9fb4c5-ce2e-4ab7-a296-f5650c13a1b8)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         a8a7aa1a-0700-4ebe-ba0a-9985132cd248)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         f1fe0bf8-7deb-4366-8fee-923e38ac7108)(content(Whitespace\"\\n\"))))(Tile((id \
         2ed0e62f-2fec-4ba6-81ed-7147ff3b2792)(label(lastQuality))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         670a8b92-7b0f-4773-93bc-0df22e2dfdae)(content(Whitespace\" \
         \"))))(Tile((id \
         381d14d5-3c34-485a-9e88-7b71684035a6)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         adb66725-f304-4204-a58a-045110b92a47)(content(Whitespace\" \
         \"))))(Tile((id \
         c34658cf-a17c-4054-9a08-8bf2653b34ee)(label(Bronze))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         119ac191-cc59-406c-96a1-a06bac56d83d)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         3ad34131-d842-4d14-b2a3-5fc86d472383)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         4293fb81-93ec-41cd-9c50-5a44eb551965)(content(Whitespace\"\\n\"))))(Secondary((id \
         f8e03706-6f7b-4138-b502-26317df7debc)(content(Whitespace\"\\n\"))))(Secondary((id \
         8e6ac124-eaff-44da-9257-d1572f71f4ac)(content(Comment\"# TODO: Add \
         premiumMultiplier helper here                      \
         #\"))))(Secondary((id \
         f2b66f41-97d9-495d-a743-dd26f77b4f4f)(content(Whitespace\"\\n\"))))(Secondary((id \
         0f56c267-1ac7-4c27-b971-d0e485409d36)(content(Comment\"# It takes a \
         streakBonus (Int) and returns the multiplier:     \
         #\"))))(Secondary((id \
         c89a9573-77a6-4d53-98dc-187d1cdf3d51)(content(Whitespace\"\\n\"))))(Secondary((id \
         8ec9ea40-ea1c-4d7d-a4e1-96b9235463fc)(content(Comment\"#   - Return 2 \
         if the streak bonus is >= 10 (strong streak)    #\"))))(Secondary((id \
         283f4cbd-7342-4949-bd02-39182c3ee295)(content(Whitespace\"\\n\"))))(Secondary((id \
         5cc7a140-8f74-4711-8a03-3609f57c3636)(content(Comment\"#   - Return 1 \
         otherwise                                      #\"))))(Secondary((id \
         472bcabe-47d6-4aba-919b-958b9dd26fb5)(content(Whitespace\"\\n\"))))(Secondary((id \
         aab0ba90-2b2a-44cf-b4f7-2f2a9010b81a)(content(Comment\"# Hint: This \
         is a simple if/then/else on the streakBonus.     \
         #\"))))(Secondary((id \
         59938b2e-7ee8-4c75-872b-48765d812d40)(content(Whitespace\"\\n\"))))(Secondary((id \
         d335e541-0c01-4362-a909-b41f9e67dc4e)(content(Whitespace\"\\n\"))))(Tile((id \
         4435beb9-5971-45f8-9e01-f2ae36bf8c37)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         907622ec-5fcb-4d00-896f-15f587093714)(content(Whitespace\" \
         \"))))(Tile((id \
         eae986aa-d41b-4217-b6cb-d20111ddbf97)(label(premiumMultiplier))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         123339cc-e262-4495-8cc4-3fdeab4ee51c)(content(Whitespace\" \
         \"))))(Tile((id \
         23544d44-5d14-4191-8ad8-7c557240a35e)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 24))(sort Pat))((shape(Concave \
         24))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         9d1298a4-9c01-4326-b019-cff51bb5f19f)(content(Whitespace\" \
         \"))))(Tile((id \
         3c50f49f-acfb-43a1-aa98-d2738a8cc14e)(label(Int))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         89e7cde2-6cf8-4258-b39e-45f0737e6bb5)(content(Whitespace\" \
         \"))))(Tile((id \
         a90c9946-548b-4867-8ecc-bb59bc4bfb16)(label(->))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 13))(sort Typ))((shape(Concave \
         13))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         898263cc-18be-4995-aa69-abc76f87a420)(content(Whitespace\" \
         \"))))(Tile((id \
         1f9dd6fd-716b-4e57-9273-7f1ddb68367f)(label(Int))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         e7d94910-8cdb-479f-b520-3ce252912e86)(content(Whitespace\" \
         \")))))((Secondary((id \
         c2a68f38-e4ae-4927-bd2b-4a8788ed05a1)(content(Whitespace\"\\n\"))))(Tile((id \
         b1ae55c2-76ff-4f13-9eb2-05e768a8477d)(label(fun ->))(mold((out \
         Exp)(in_(Pat))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         37))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         dd9faa24-72a0-47f8-b9ab-12fea49aceb1)(content(Whitespace\" \
         \"))))(Tile((id \
         de3aaf81-0caa-4afe-8a59-7bd538d1332d)(label(streakBonus))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         79b4fd73-c9d8-4f1f-b16b-1b0ba4e1352b)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         b6e98695-3583-467d-828d-24ad09586978)(content(Whitespace\"\\n\"))))(Tile((id \
         7e4bef02-cd87-46cf-bc61-d061a134201b)(label(?))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         6deed089-b8a1-44a5-a48e-5cf44bd0383a)(content(Whitespace\"\\n\"))))(Secondary((id \
         56240521-3fdb-4159-98bf-3407d6a5e575)(content(Whitespace\"\\n\"))))(Secondary((id \
         4f92a74c-6026-42df-b796-e397bc8b1b77)(content(Whitespace\"\\n\"))))(Secondary((id \
         ecf3b73f-39ca-47c9-892e-530cdc0fdb3e)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         63f19d10-c46c-4c39-8aa7-71f7a14fcdda)(content(Whitespace\"\\n\"))))(Secondary((id \
         6e02935b-36b2-4bef-863e-9bc8d89fa10b)(content(Whitespace\"\\n\"))))(Secondary((id \
         784e6390-183e-4761-9f90-408a2caba8bc)(content(Comment\"# Main update \
         function - dispatch actions #\"))))(Secondary((id \
         94768c29-cab3-467b-8244-a761ff2f180d)(content(Whitespace\"\\n\"))))(Tile((id \
         7aa7117d-9ff5-4c08-b3af-0aa64df63de9)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         91b08c3b-441d-486c-bb19-2366c1c73363)(content(Whitespace\" \
         \"))))(Tile((id \
         de19adbf-62d0-4bfe-98d8-ac22f2164092)(label(update))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         d127e0e3-6624-4305-82be-df08481f85d2)(content(Whitespace\" \
         \"))))(Tile((id \
         7c576465-6260-442d-aefb-e019d318da3f)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 24))(sort Pat))((shape(Concave \
         24))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         51793e81-8e6b-4ea2-a382-fbe14780b12c)(content(Whitespace\" \
         \"))))(Tile((id \
         09455af6-4b7d-4102-b19a-f4539d265c98)(label(\"(\"\")\"))(mold((out \
         Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0 1))(children(((Tile((id \
         66204c7b-b8ca-4e5b-a7eb-278a3564e8be)(label(Model))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         f83d77af-5e81-4d48-be48-2c4f10aefd3c)(label(,))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 44))(sort Typ))((shape(Concave \
         44))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         9f00e733-81b1-4c19-8d10-b29e349488c3)(content(Whitespace\" \
         \"))))(Tile((id \
         e180ad70-0b75-4c90-98bd-fbabcec2952e)(label(Action))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children()))))))))(Secondary((id \
         3eef5e6e-bde8-486e-9009-fb305f6c6977)(content(Whitespace\" \
         \"))))(Tile((id \
         15036a79-f06b-4e79-b287-8ca81586ecbb)(label(->))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 13))(sort Typ))((shape(Concave \
         13))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         6e4d48da-a49e-4430-aee6-c6415ba7b944)(content(Whitespace\" \
         \"))))(Tile((id \
         0e4bc525-8efa-4f4b-9d0c-b89850bf9dcb)(label(Model))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         12f45036-4128-49ba-ae16-8897fae11c07)(content(Whitespace\" \
         \")))))((Secondary((id \
         8a03d840-b7d5-456d-8207-0d280de22c6e)(content(Whitespace\"\\n\"))))(Tile((id \
         f8acbb60-83a8-4f65-8cbd-9f4e7d145925)(label(fun ->))(mold((out \
         Exp)(in_(Pat))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         37))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         8ed789ed-5690-48ca-8944-a217ec289cbd)(content(Whitespace\" \
         \"))))(Tile((id \
         3d535408-fa9c-442b-a9e3-3e4953243d5a)(label(\"(\"\")\"))(mold((out \
         Pat)(in_(Pat))(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0 1))(children(((Tile((id \
         775cdb83-746a-4789-83b0-d9b44c611057)(label(ledger))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         7436a602-3dab-4e51-b666-4fd2961daa3c)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 44))(sort Pat))((shape(Concave \
         44))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         354c6636-ec44-4e7e-af1c-25b98b3d2b60)(content(Whitespace\" \
         \"))))(Tile((id \
         75a93071-e48f-4c8b-b495-80fe470a2c5a)(label(action))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children()))))))))(Secondary((id \
         d1da772a-4532-4d80-80d5-a0b12471b28f)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         48245554-6079-4a07-b3ae-f0103d03a02f)(content(Whitespace\"\\n\"))))(Tile((id \
         864e912c-cfc8-4cf1-9a00-76db64cc7922)(label(case end))(mold((out \
         Exp)(in_(Rul))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         fb5a0813-2d39-497d-a7f8-b11c8172ac51)(content(Whitespace\" \
         \"))))(Tile((id \
         fb4b82a2-eb59-4841-8bfd-6f0aa2fd1a28)(label(action))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         588e6460-1ee7-42ab-a344-2aa154aeed6b)(content(Whitespace\"\\n\"))))(Tile((id \
         b9452057-58a4-4cf6-9477-1ee0045f3cc5)(label(| =>))(mold((out \
         Rul)(in_(Pat))(nibs(((shape(Concave 46))(sort Exp))((shape(Concave \
         46))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         c256fb92-adad-47a5-b3b9-3a29ecab2443)(content(Whitespace\" \
         \"))))(Tile((id \
         1d0a75da-3886-45d2-b43c-7c41c93f27a6)(label(RecordHarvest))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         cf8497ca-2ce2-443d-945b-cda618c07c31)(label(\"(\"\")\"))(mold((out \
         Pat)(in_(Pat))(nibs(((shape(Concave 23))(sort Pat))((shape \
         Convex)(sort Pat))))))(shards(0 1))(children(((Tile((id \
         135cd2a2-b76d-46ae-9392-de4020ed331f)(label(h))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children()))))))))(Secondary((id \
         7028ec5d-4c85-4bea-990c-047b1e233c96)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         9a3b1470-d262-4ae1-ba21-058ea5d18abf)(content(Whitespace\" \
         \"))))(Tile((id \
         41e08ddf-2fd6-4643-91b8-110a092cc22e)(label(processHarvest))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         1ea42e0f-1595-4b4e-9b72-8be34609080f)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         1617e654-177e-4571-9e10-56df9b616c99)(label(ledger))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         a7bc19e4-111b-42ce-8ee8-3e2874a318ef)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         fcf2433b-dda2-43bf-be62-05fd9375e7b4)(content(Whitespace\" \
         \"))))(Tile((id \
         8acb924f-ec63-46e6-9eaf-54de5d514293)(label(h))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         c926bf72-d29e-4aca-ad9d-f9b832b94aa6)(content(Whitespace\"\\n\"))))(Tile((id \
         1fbbd1b6-1d43-4db4-8e6e-0d6cb33b94ae)(label(| =>))(mold((out \
         Rul)(in_(Pat))(nibs(((shape(Concave 46))(sort Exp))((shape(Concave \
         46))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         756bf320-93b1-446b-9e35-e9c0d1075a44)(content(Whitespace\" \
         \"))))(Tile((id \
         72af89e0-4c2a-4aea-950f-ccbef97ba4b3)(label(ClaimBonus))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         530bc604-cd23-4c9a-9143-855d1c852eaa)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         25d53267-0fea-43d6-a58e-a672381ad1bf)(content(Whitespace\" \
         \"))))(Tile((id \
         5c7e74da-d381-4113-9452-dfdb8f852dd1)(label(claimBonus))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         cbaba67c-94ba-4a9a-bfca-d1a99884133c)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         d9ac13d3-4926-48f4-89b9-477091b7e6f7)(label(ledger))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         30d74577-5baa-4c6d-a67f-42e5f26cfec5)(content(Whitespace\"\\n\"))))(Tile((id \
         09290557-9fb4-4240-a888-9f76b5f93a6c)(label(| =>))(mold((out \
         Rul)(in_(Pat))(nibs(((shape(Concave 46))(sort Exp))((shape(Concave \
         46))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         d8f71d9b-ab5b-4562-bb3c-c47572f30d74)(content(Whitespace\" \
         \"))))(Tile((id \
         32e56dd5-235e-457e-b528-7d921fa5cd90)(label(CloseDay))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         92e70a59-a090-4524-aa94-0c16ca83563e)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         949330d9-7fcd-4b0d-86a0-171e09486ac6)(content(Whitespace\" \
         \"))))(Tile((id \
         8333cdf5-cd5d-4f01-8506-9b99bd3960a3)(label(closeDay))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         84542548-7946-44dd-b59a-42a54cfb6fb4)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         673e59a4-a738-4eca-9852-3c7ba4b65882)(label(ledger))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         0fe50a4c-2223-443e-9700-2bc1f914cbf6)(content(Whitespace\"\\n\"))))(Secondary((id \
         97a305a6-8ea3-4116-bf4e-87709609f6c3)(content(Comment\"# TODO: Add \
         PremiumSale case here                          #\"))))(Secondary((id \
         4a6fa40a-a6ef-4d7d-b6db-fecadf76a712)(content(Whitespace\"\\n\"))))(Secondary((id \
         471f7f3b-0bec-42f6-be0c-d4392c41ea59)(content(Comment\"# Hint: \
         Compute payout = streakBonus * premiumMultiplier,  \
         #\"))))(Secondary((id \
         6a3eb3c8-577f-4b1f-a940-cc1bac2bc844)(content(Whitespace\"\\n\"))))(Secondary((id \
         13d2de6e-1728-41a8-bffd-700ed24e5bfa)(content(Comment\"# add payout \
         to totalValue, and reset streakBonus to 0.    #\"))))(Secondary((id \
         3ba4f1f4-02f3-458c-9838-e45278198eea)(content(Whitespace\"\\n\"))))(Secondary((id \
         d75fe786-7ae7-4b98-8c68-23e6f3e3812f)(content(Comment\"# Keep \
         harvests and lastQuality unchanged.                 \
         #\"))))(Secondary((id \
         8bb4788d-73f6-430e-9558-444947b315b3)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         da0f6aed-3834-408f-ad47-3e124c978fe3)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         4c1a833b-76b4-46fd-bb92-34dbdbd902a1)(content(Whitespace\"\\n\"))))(Secondary((id \
         3835cb21-f62e-4db6-bec2-4b8158e69e77)(content(Whitespace\"\\n\"))))(Secondary((id \
         994ab90c-b21c-4b4f-9377-98a26b532912)(content(Comment\"# Run multiple \
         actions in sequence #\"))))(Secondary((id \
         1b3d31f6-7567-4ae0-b1c4-07323be615a6)(content(Whitespace\"\\n\"))))(Tile((id \
         b87f9eb6-811c-4fb2-865c-fb4dc812dee9)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         3b749cbf-3a05-42ba-8d21-846283188af3)(content(Whitespace\" \
         \"))))(Tile((id \
         66c45568-b582-4209-8fa6-8b9e52b23590)(label(run))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         b3f01e0c-864b-4091-84be-371e3d3d695d)(content(Whitespace\" \
         \"))))(Tile((id \
         38947e9f-305c-4bf6-8cb9-f2d4155b8852)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 24))(sort Pat))((shape(Concave \
         24))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         4c0874ae-1488-49f4-9a6e-4c15449bbf17)(content(Whitespace\" \
         \"))))(Tile((id \
         eab61416-6f51-4a4d-91f7-433e51ea58b6)(label(\"(\"\")\"))(mold((out \
         Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0 1))(children(((Tile((id \
         5ce8f59b-dc97-403a-9c66-a62bd96b3039)(label(Model))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         3035c5e2-5996-4523-ae1f-2fce8c21a613)(label(,))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 44))(sort Typ))((shape(Concave \
         44))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         e41cc25d-2a6d-4581-b0fb-9e084033f800)(content(Whitespace\" \
         \"))))(Tile((id 9555849f-1fa4-4752-8028-8c5019338100)(label([ \
         ]))(mold((out Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape \
         Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
         7c855322-d9c6-4dbd-8217-4b8025ea8c00)(label(Action))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))))))))))))(Secondary((id \
         2d6f9a27-b57c-47e3-b93c-0c9c746dbffb)(content(Whitespace\" \
         \"))))(Tile((id \
         41f2d1c8-0196-4a53-ac93-d3b1b8519381)(label(->))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 13))(sort Typ))((shape(Concave \
         13))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         03fe97f9-464e-4f28-bca3-8098953bc5d2)(content(Whitespace\" \
         \"))))(Tile((id \
         dd991edd-9233-426b-ac1d-6be4c799d695)(label(Model))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         e3975f1f-ec6b-40f4-a941-cdafacf09380)(content(Whitespace\" \
         \")))))((Secondary((id \
         8dc06236-553f-404f-a814-6655f10d03af)(content(Whitespace\"\\n\"))))(Tile((id \
         397daad1-09d9-4e89-b47c-d22d4d59e5f3)(label(fun ->))(mold((out \
         Exp)(in_(Pat))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         37))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         ba3ea5c7-a827-409a-b62d-6236fed87e16)(content(Whitespace\" \
         \"))))(Tile((id \
         4ed796e6-74cf-4022-8369-4f1e4484cd26)(label(\"(\"\")\"))(mold((out \
         Pat)(in_(Pat))(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0 1))(children(((Tile((id \
         b758b4c7-0e69-4013-8781-255f174d568a)(label(ledger))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         99b13c11-2e14-44d7-82c0-395f06d241f6)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 44))(sort Pat))((shape(Concave \
         44))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         aa099ef5-270d-4e6c-a192-77d44aeef064)(content(Whitespace\" \
         \"))))(Tile((id \
         edaf38bd-aec7-43b1-8077-244a179f8f91)(label(actions))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children()))))))))(Secondary((id \
         750ef98e-8a7c-4101-96bb-4f2414013da6)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         61220126-94ad-4f35-98a1-f62052180a1b)(content(Whitespace\"\\n\"))))(Tile((id \
         c2cec262-12f0-4b69-b029-39695e6bf74f)(label(fold_left))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         fa7cf0c6-f911-4b91-941d-f3ba0cd419ee)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         ea25dd39-dc04-4472-98a0-34b9ce7f519f)(label(actions))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         c5be2b0a-5a9c-4c70-87e8-18dfc35eb613)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         4397abf9-b514-4187-9aeb-03afbbfb2d36)(content(Whitespace\" \
         \"))))(Tile((id \
         f035c065-3c7c-4205-a7b5-53c5f733bb74)(label(update))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         0ac557b6-aa2e-473f-987b-aeb0bd7d216b)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         770dd677-e967-4c5d-ba82-788ae2e4bd3d)(content(Whitespace\" \
         \"))))(Tile((id \
         16f1ddd5-3b75-45e9-9d70-df04c9e5a6bf)(label(ledger))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         45115d76-0974-47dd-bbbc-eba9385fe79d)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         3aaafec8-36c7-400f-b2f5-6616b0392372)(content(Whitespace\"\\n\"))))(Secondary((id \
         7dad75cb-0b44-4742-810d-b6ee9b668201)(content(Whitespace\"\\n\"))))(Secondary((id \
         0d871e01-d146-455f-a708-af47f0f7e31b)(content(Comment\"# ===== TESTS \
         ===== #\"))))(Secondary((id \
         6181c892-ee1a-411f-b219-6a0066751607)(content(Whitespace\"\\n\"))))(Secondary((id \
         1d853c5d-0a49-487a-a36f-831280028e55)(content(Whitespace\"\\n\"))))(Secondary((id \
         5ced11d1-2c50-4349-9dea-9dba5b802631)(content(Comment\"# Regression: \
         basic harvest recording still works #\"))))(Secondary((id \
         6fe7f9f0-a4fa-48ab-8f1b-dce8057adcfa)(content(Whitespace\"\\n\"))))(Tile((id \
         3ff31f0e-87ee-44ed-89f4-c5eff6c04ea7)(label(hint test end))(mold((out \
         Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         803f53f7-84c1-4dc4-b9a4-1badf575ba6d)(content(Whitespace\" \
         \"))))(Tile((id \
         8b0807b7-b88f-4662-996c-de2883b98aa4)(label(\"\\\"recording harvest \
         adds to total value\\\"\"))(mold((out Exp)(in_())(nibs(((shape \
         Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         c51ae99a-1253-437f-8fb7-85d4034a15d1)(content(Whitespace\"\\n\")))))((Secondary((id \
         7ad9e59d-94ab-46e6-aae5-228ffce5338e)(content(Whitespace\"\\n\"))))(Tile((id \
         6515756a-e71f-44d4-8cc7-98792737b02c)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         1e748ae7-dcf1-4b68-b30d-b49026105027)(content(Whitespace\" \
         \"))))(Tile((id \
         7a93eed5-4e44-44d0-a87e-701d4427993f)(label(h))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         265e74bd-db86-4fc7-8f6c-461c6f62a4c2)(content(Whitespace\" \
         \")))))((Secondary((id \
         bae39c32-7eaf-4d02-89ce-2942ba308821)(content(Whitespace\" \
         \"))))(Tile((id \
         b189fac5-2651-4509-af8b-94bd38c7647d)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         01777538-e8c0-4d79-b275-5844845e0234)(label(crop))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         4649b0fa-638b-4cda-9400-16c244581709)(content(Whitespace\" \
         \"))))(Tile((id \
         9c6f1c12-bcd6-4183-8f49-a07b2e97aa40)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         7f5b4fa0-7711-44f7-b1ab-6bceef4334c8)(content(Whitespace\" \
         \"))))(Tile((id \
         0d17c017-db6a-4929-bf8e-9374e15941d6)(label(Moonmelon))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         5114bb55-6ed3-4e9e-aa62-5a374d6ed16f)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         b412ee0d-be9e-442a-a05a-58046217d44c)(content(Whitespace\" \
         \"))))(Tile((id \
         ce35c361-7f22-4b19-a0f9-020c1c3723b7)(label(quality))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         105bfab6-c019-42cc-af98-3b25ad153706)(content(Whitespace\" \
         \"))))(Tile((id \
         18f1769d-5aa7-4f47-950e-e6d03f710311)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         1c2c46a8-8b73-4f90-841b-09f34dd849b4)(content(Whitespace\" \
         \"))))(Tile((id \
         27b2104b-81e4-4990-a083-e08f1a059537)(label(Bronze))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         f5010b14-99b0-47cd-a35b-c613b478a676)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         da36c2e4-a5ba-40b1-9564-76a763a5256b)(content(Whitespace\" \
         \"))))(Tile((id \
         cf0dce3e-406f-424e-b927-481bcf9401eb)(label(quantity))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         523b65c8-655d-4424-8c4f-1ecc8b22b116)(content(Whitespace\" \
         \"))))(Tile((id \
         f0a0b553-8002-4ab7-ac65-17046a6752fe)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         d89b33ca-71cd-44ce-a647-ac9556fec31b)(content(Whitespace\" \
         \"))))(Tile((id \
         2cfe30e4-9b3a-4be6-8327-4823703bec08)(label(2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         f158d8b5-ab7c-4d18-af4f-1fece82d02fc)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         7b18b1c3-c6a1-4ee0-89c2-61b4f29dfb08)(content(Whitespace\"\\n\"))))(Tile((id \
         3ca76957-767a-4948-b305-e7c5d8a46633)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         c39538f1-cedf-4e2b-942a-d3f625c1601e)(content(Whitespace\" \
         \"))))(Tile((id \
         932fdc52-0f7c-4142-a1a1-e6b53c8d63ef)(label(ledger))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         d7ab3d82-4ecd-41fb-bc2a-b367f8daaf4f)(content(Whitespace\" \
         \")))))((Secondary((id \
         300e01cd-9c46-49b1-87c8-19d1c1e0ac0b)(content(Whitespace\" \
         \"))))(Tile((id \
         820b5311-6534-46e0-8ec2-e49229431897)(label(update))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         f25f6ba2-86e2-497b-9130-b4095b51f943)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         a92672ed-98f0-44b2-a92f-06b76d5b2add)(label(initModel))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         c408ccd3-3698-4152-b31b-b679255d349a)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         4db72249-e0c6-4ed0-84dc-72c02c169229)(content(Whitespace\" \
         \"))))(Tile((id \
         5c088daa-3580-4211-8195-0d27d92b5563)(label(RecordHarvest))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         5f43e453-ee6a-4b58-b98f-3a2da94f2d17)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         b070f974-e41b-4eca-b603-1f64c6f91c99)(label(h))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))))))))))))(Secondary((id \
         c05654e2-ab44-414c-85a0-4c298742471d)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         38ffd998-9976-4ef3-9260-ef32dbb83b63)(content(Whitespace\"\\n\"))))(Secondary((id \
         6286c700-9dba-4a28-b7ee-25953e421bf2)(content(Comment\"# \
         Moonmelon(15) * Bronze(1) * 2 = 30, no streak bonus on first \
         #\"))))(Secondary((id \
         bff5adce-e5c8-4931-a6aa-0d7d52f31ed8)(content(Whitespace\"\\n\"))))(Tile((id \
         828624e0-917d-453b-b143-3d9be8ca8405)(label(ledger))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         7875ee7b-743b-4dbf-bbe9-ba0c7189f7e1)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         026bac4d-7bfa-495c-82ce-96c8ad22ec3e)(label(totalValue))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         e6645630-1654-4f39-901a-2588a12f6680)(content(Whitespace\" \
         \"))))(Tile((id \
         281acf72-85cb-4cbf-85af-59fa8aa4effc)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         1cde809e-2785-400f-b553-3387a35c6411)(content(Whitespace\" \
         \"))))(Tile((id \
         8b80cd7b-fbc3-44d5-b74f-ad0f28769f45)(label(30))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         c3700309-9efb-424e-9252-9149bad69177)(content(Whitespace\"\\n\")))))))))(Tile((id \
         6f946a08-b7ef-4303-9728-c55c3f21338a)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         88fa158c-ebbf-4650-b434-e23bf13352df)(content(Whitespace\"\\n\"))))(Secondary((id \
         32c01586-e0c4-44e5-83e9-49151e56a40c)(content(Whitespace\"\\n\"))))(Secondary((id \
         8a7762a6-933f-4bc0-9b04-412b8e72bf2a)(content(Comment\"# Regression: \
         streak bonus still works #\"))))(Secondary((id \
         98449dac-c8f6-4a2d-90f4-e34db58ade71)(content(Whitespace\"\\n\"))))(Tile((id \
         1aff07ff-b84d-49ae-92c9-65e9b9c1390c)(label(hint test end))(mold((out \
         Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         b9b20f5d-15eb-4678-8110-940c8319d7d4)(content(Whitespace\" \
         \"))))(Tile((id \
         295f16b4-1954-4014-87f6-4360aa4b7cd8)(label(\"\\\"same quality builds \
         streak\\\"\"))(mold((out Exp)(in_())(nibs(((shape Convex)(sort \
         Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         a784991e-e322-4c56-84d3-d4d83da36c58)(content(Whitespace\"\\n\")))))((Secondary((id \
         a410e04f-e0cc-4322-bc85-fb81c1b2fd24)(content(Whitespace\"\\n\"))))(Tile((id \
         e537ca68-a8bf-4570-b57c-2e387162e771)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         b9dca698-c118-4260-b852-88550184c260)(content(Whitespace\" \
         \"))))(Tile((id \
         97f0706c-1a9c-4fc7-a08c-5ec4c51eff46)(label(h1))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         45ddfb75-2065-493d-8873-0ea2c5d07c2d)(content(Whitespace\" \
         \")))))((Secondary((id \
         1c1203a6-4901-4ce5-963f-e615e41f94f8)(content(Whitespace\" \
         \"))))(Tile((id \
         372e03b1-b3d9-4512-b9ec-122056b8d5cc)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         1cf2fefe-3f25-4b47-a9a4-aa2487c2163e)(label(crop))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         1f4042fb-fc25-44b6-9f42-1160e9c4cba6)(content(Whitespace\" \
         \"))))(Tile((id \
         0c830026-65a1-4608-be08-76af83d81625)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         c02c7ad6-0fb8-4702-9127-d8da819fc4ba)(content(Whitespace\" \
         \"))))(Tile((id \
         25bcca35-629d-4e21-9610-3690065e53c7)(label(Moonmelon))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         7b71b333-7bb4-4b07-b0cf-812fe0746448)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         7c84cee6-109e-4adc-aca8-66d628161b5d)(content(Whitespace\" \
         \"))))(Tile((id \
         f1bf1b4b-5308-4af6-9490-a2d16447954c)(label(quality))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         5902a5a1-2d53-4384-9caf-0a0d9f57aa30)(content(Whitespace\" \
         \"))))(Tile((id \
         19c989e4-e9eb-46d2-9110-026342fcb6b1)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         f81e8b36-1b18-43da-9ea9-2f3993442fe6)(content(Whitespace\" \
         \"))))(Tile((id \
         ab7b684d-eb8b-4e7f-b7bb-653da7545f03)(label(Gold))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         833829f6-19b8-4823-afa4-97447efb2b3c)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         26a7373b-7512-4343-b85d-86db87694b95)(content(Whitespace\" \
         \"))))(Tile((id \
         b2e18911-8fc7-4674-9d2e-d9e8352bf04e)(label(quantity))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         dd4b3f4f-0f4c-4fc3-99ef-e58043597ceb)(content(Whitespace\" \
         \"))))(Tile((id \
         ce7c2c19-a0c9-410e-a34b-78497f201816)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         5a2b0590-fa2a-4581-a299-6a22a0c3528e)(content(Whitespace\" \
         \"))))(Tile((id \
         75d279a1-dd67-4f50-9b64-4cdb3b28cfb5)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         20d89538-d02f-4378-b6ee-e470048dfc71)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         7ffc591e-2a8d-48aa-9948-9d6efc3c9710)(content(Whitespace\"\\n\"))))(Tile((id \
         9f42f758-2f3f-4642-b3da-63a4fd86fb27)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         3f147173-0c44-4e2c-9192-19dc4fa41a06)(content(Whitespace\" \
         \"))))(Tile((id \
         fe513605-bb23-4ada-9cb5-9bdda9c8347b)(label(h2))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         129cdc7e-6d21-4ab1-9672-7343396e2257)(content(Whitespace\" \
         \")))))((Secondary((id \
         6e511c3f-0b9f-40d3-8ec1-4cd60959f577)(content(Whitespace\" \
         \"))))(Tile((id \
         ca680a3f-97ac-4ff4-8f19-9f457d0cede3)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         3c8788a1-16f3-4874-9fd7-957ca5c76636)(label(crop))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         1e202c17-9190-4267-a19c-1e6f8957e9e2)(content(Whitespace\" \
         \"))))(Tile((id \
         4e1072a6-0315-4199-8fd1-2d1cd4f1cab9)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         76518e60-9fa4-40d4-a592-4647f9e38cba)(content(Whitespace\" \
         \"))))(Tile((id \
         2d5da389-367d-48b3-9fe0-e66642e017ce)(label(Starfruit))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         bb293013-c56d-4259-8010-5ba2c8a41240)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         d755e571-3b4b-4499-8589-2b893add08f3)(content(Whitespace\" \
         \"))))(Tile((id \
         13851fd4-2df0-4977-b52d-a51b5a26cc00)(label(quality))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         bd93af72-4f3a-483f-ad09-0e64b61643e4)(content(Whitespace\" \
         \"))))(Tile((id \
         4754ecdc-ad88-424b-b3e0-0e4df41995ea)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         07e4a428-b452-4a45-9286-38bcb6fa2ed6)(content(Whitespace\" \
         \"))))(Tile((id \
         a7ee274d-98e0-448a-a71c-92797b482b80)(label(Gold))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         84054006-77bb-4dc8-9ffb-88306d6cae56)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         2443e89d-8eea-4133-b6ee-a7b674b4e9ad)(content(Whitespace\" \
         \"))))(Tile((id \
         b1650100-7da7-47e5-aaa3-2b4539634c6d)(label(quantity))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         c01f5811-1f17-4587-8fe1-8a84cf33eb93)(content(Whitespace\" \
         \"))))(Tile((id \
         096a65db-775f-4820-b665-21471d868957)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         41ac7658-28b5-457c-932a-4f1b0022e0a5)(content(Whitespace\" \
         \"))))(Tile((id \
         619b2763-6c44-46d9-ae5d-10f58c3b47ab)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         3fa7b0f6-f64c-4612-a98f-e83c6d160623)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         e63da7eb-1000-4435-867a-fd3d3695a785)(content(Whitespace\"\\n\"))))(Tile((id \
         e1b66172-3e0b-4f1e-a1b6-14171d891a0b)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         876cb54c-5001-46f0-8b93-0004614a4d80)(content(Whitespace\" \
         \"))))(Tile((id \
         4c59e13b-878c-42bf-a324-0d81ccc18391)(label(ledger))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         ced0c8c0-9af3-4712-b89b-413c9c8906d6)(content(Whitespace\" \
         \")))))((Secondary((id \
         13d573a0-3ca1-4c86-8ad6-9a772d6df5e8)(content(Whitespace\" \
         \"))))(Tile((id \
         85a90666-18c8-402f-ae34-cd8f5582f35e)(label(run))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         2dd7bab5-d946-4d72-a923-af44cc5fe5fa)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         2fc299b4-fa55-4cdb-85b2-91a4e0a8e3a2)(label(initModel))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         f42857a5-518d-45b1-abec-fe8928cdf70f)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         0e35448d-d829-4551-8007-962eddd10924)(content(Whitespace\" \
         \"))))(Tile((id a3fc4cf5-d268-43bf-8a40-30499ad0ffac)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         ac2e0223-91f8-446e-8dc9-a702be6fb5f2)(label(RecordHarvest))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         38206723-0401-4503-9b6e-aef2274aad7f)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         23494e16-1604-4769-8873-17f6c9edeef7)(label(h1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         c1626dcc-e02b-431a-a275-c5ddfd060022)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         495ad975-70b1-4af1-b0f3-ac5b4fec0f48)(content(Whitespace\" \
         \"))))(Tile((id \
         1151a49b-a8ec-4d20-abfe-aef66dc3d380)(label(RecordHarvest))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         c42bc6b4-0247-4134-972a-775ec1c6ab71)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         732dd2d7-9f2f-46a1-8138-2d155c30b255)(label(h2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))))))))))))(Secondary((id \
         510cbf0e-f638-4f65-8410-07067ed82d0c)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         82ed2702-fa65-4118-ba4a-dbcb56f50f9f)(content(Whitespace\"\\n\"))))(Tile((id \
         180e9cac-afb7-42a5-9199-7bbf3eea9eee)(label(ledger))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         01f108e4-c700-4224-822d-4d8496ba0bc9)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         f582a8a5-b841-4512-b768-9f633771e792)(label(streakBonus))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         4413eedd-9b60-4167-a24d-15ec2abb6e31)(content(Whitespace\" \
         \"))))(Tile((id \
         80f0cac9-e47f-4253-9ff1-c34e4018f503)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         b73169c3-0c60-4a2e-b68d-e56265675559)(content(Whitespace\" \
         \"))))(Tile((id \
         ce0c3ac3-e50e-4942-b2d4-667bb0f7a356)(label(5))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         9b4213f5-2581-4f07-8dea-77ee9439b53f)(content(Whitespace\"\\n\")))))))))(Tile((id \
         6a54b30e-9124-483b-b58b-a5b5f5189970)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         154add1c-75e1-48bb-8570-6aa22f1aa17a)(content(Whitespace\"\\n\"))))(Secondary((id \
         abcc393d-74fb-47b6-8f14-c82eb6c098d4)(content(Whitespace\"\\n\"))))(Secondary((id \
         cd2c5352-544b-4721-8e5d-4b8150b1b0ca)(content(Comment\"# Regression: \
         claim bonus still works #\"))))(Secondary((id \
         111ba5e2-cc7c-4919-a695-d82309178dbb)(content(Whitespace\"\\n\"))))(Tile((id \
         4957c40a-2280-49a0-8c82-5e8125f260cf)(label(hint test end))(mold((out \
         Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         799af9f5-1092-46f6-b8f4-754f68ab0faf)(content(Whitespace\" \
         \"))))(Tile((id \
         537da30e-6336-4fa3-a5f4-96f879f1a7c8)(label(\"\\\"claiming bonus adds \
         to total and resets streak\\\"\"))(mold((out Exp)(in_())(nibs(((shape \
         Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         e61a27d8-9611-4d80-bf90-b541d2a1f9fa)(content(Whitespace\"\\n\")))))((Secondary((id \
         4c063c89-b4cf-408f-856b-e9cdfdc26d35)(content(Whitespace\"\\n\"))))(Tile((id \
         b70751fa-a404-4ee9-ac3c-3d7cf235786c)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         216ad5d4-0ffa-44e5-ad58-7d9339a35f13)(content(Whitespace\" \
         \"))))(Tile((id \
         ad6584f8-68db-4d77-a375-e3e8d2ee4432)(label(h1))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         9dad0290-162b-4854-ac1b-fc4ce831184d)(content(Whitespace\" \
         \")))))((Secondary((id \
         3be5ebb8-de63-4d45-9f57-0376e84e3a0d)(content(Whitespace\" \
         \"))))(Tile((id \
         f4f5c44d-ab4f-45e0-9c91-36d3beb534ee)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         b788c077-5893-424c-b81c-befc88be6c3a)(label(crop))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         14b292c2-af22-4c7d-9727-baf753dca27f)(content(Whitespace\" \
         \"))))(Tile((id \
         6657024c-e027-4d45-a449-c9c9aaf823e8)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         3203c24e-664e-4a6c-b83f-79071649e6bf)(content(Whitespace\" \
         \"))))(Tile((id \
         3750cb7e-a748-46ee-9fb1-b998f43cb2b8)(label(Moonmelon))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         2b5ddab3-32bc-4ddf-b540-e40cdc0e8392)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         631a92a4-57d4-4176-bb44-9cf13fd57f19)(content(Whitespace\" \
         \"))))(Tile((id \
         1bb22cc8-13c4-4e45-9420-2543d808dd4e)(label(quality))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         9e28cedc-2662-4208-8368-e939ca1b9190)(content(Whitespace\" \
         \"))))(Tile((id \
         44e4028c-c4ee-48eb-b910-5bd0a0c8e8a4)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         4210eddd-979b-49de-94a8-d647f038a2ff)(content(Whitespace\" \
         \"))))(Tile((id \
         f283da1f-cb0a-4a77-9766-6cefdd6dacb2)(label(Gold))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         85257bb1-b6b4-44df-a065-20a8cf9eb31a)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         98f5ff05-033a-48d7-ae2d-e35d3546ea01)(content(Whitespace\" \
         \"))))(Tile((id \
         273cbe79-21cf-438e-9709-021e90ac79c3)(label(quantity))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         8a2eba83-66a6-4037-8d2c-e2c208a4f8c2)(content(Whitespace\" \
         \"))))(Tile((id \
         63f3fe3a-0583-49f1-880c-fdd4e9a78593)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         b6b0514a-ba7e-4944-8366-dadd297b666c)(content(Whitespace\" \
         \"))))(Tile((id \
         20d8003d-6cb4-4d63-91ba-c6183397ddf5)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         8e9ff32e-6c73-42cc-91f8-63923d73a228)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         d7df1153-6267-48f5-b9ca-924594308f63)(content(Whitespace\"\\n\"))))(Tile((id \
         aae56001-d0be-4fae-91ed-0906e7ab3ac9)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         a9a7cffe-8595-42a8-99b7-4a1d7ad84f1e)(content(Whitespace\" \
         \"))))(Tile((id \
         f73ad237-626f-4d64-9c6f-e0864ded3024)(label(h2))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         3e7f2375-35df-4e45-89bb-489842df85ff)(content(Whitespace\" \
         \")))))((Secondary((id \
         f7f50e86-89f0-45d0-b594-6e97ee33f022)(content(Whitespace\" \
         \"))))(Tile((id \
         bcb72793-1977-4421-8555-9e99b2626323)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         9c05251a-72e5-4cbb-982b-ab06d94621de)(label(crop))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         a60797e6-359d-4cc3-8104-b8cca1424c11)(content(Whitespace\" \
         \"))))(Tile((id \
         8e2f3362-88ab-4450-92c0-fa919ad870c5)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         606cb133-7850-41ad-b6e3-d3a4c2a9091a)(content(Whitespace\" \
         \"))))(Tile((id \
         c450ba95-add6-4b4e-a73a-5aca66d8f144)(label(Starfruit))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         6f28f0c5-776d-462e-8586-05db523bb768)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         06343717-ca77-4e82-bf13-332201ccfd59)(content(Whitespace\" \
         \"))))(Tile((id \
         99c09a2f-4931-43f8-a232-8851f8a5b518)(label(quality))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         3c57384f-a48c-49d6-9099-c2f7dfe384f7)(content(Whitespace\" \
         \"))))(Tile((id \
         ced66e74-aaa6-41fa-9c13-b13ccd7aa988)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         57789f2f-df2e-4c77-ae53-7f05a592f08d)(content(Whitespace\" \
         \"))))(Tile((id \
         e9455458-fff8-4894-99cb-797a35d32e24)(label(Gold))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         5a247b21-aca5-48ce-a5de-9fb678eb7c45)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         3604b3b4-06c0-40f7-ab90-d6d5961f61d9)(content(Whitespace\" \
         \"))))(Tile((id \
         3a3d2da6-66f1-4bce-be91-217dd1639fa9)(label(quantity))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         1e27d717-8299-45fb-896f-3e7c5bdac578)(content(Whitespace\" \
         \"))))(Tile((id \
         753617e2-a97b-4d3e-951d-68aa6d4fa8ad)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         7c6816ce-c088-4a7e-b723-358b15380d63)(content(Whitespace\" \
         \"))))(Tile((id \
         6b56d21d-4d70-4ce8-b591-d1be2edd1b30)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         ae18f57a-bc75-4dc4-98f5-9122b7680182)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         370206ef-d9dc-416d-aa51-4e096c12f87e)(content(Whitespace\"\\n\"))))(Tile((id \
         53188701-40db-4e80-b161-2294520ac1d4)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         dff9d939-da8a-4b6a-ab1f-7586316156cb)(content(Whitespace\" \
         \"))))(Tile((id \
         a949d665-098b-43d6-a02b-8d43e25b550a)(label(ledger))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         b94ec788-045a-458d-b077-429257741452)(content(Whitespace\" \
         \")))))((Secondary((id \
         9e6feda5-7be2-49e6-9765-c4c4dc46a25f)(content(Whitespace\" \
         \"))))(Tile((id \
         e739fb3e-b8a8-4951-930a-da10ff1b4bd7)(label(run))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         d72c5a1a-0aaa-4c08-bef1-ed93a38b6191)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         b4048be9-be8a-4672-9f10-2ee36ef181ad)(label(initModel))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         86a571f1-232a-45d1-af98-5fbb687c0510)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         0b129369-5143-4e44-97ed-16a9471a3ec9)(content(Whitespace\" \
         \"))))(Tile((id 110a5c35-c921-4e17-8445-744a5c1a7960)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         9c06c530-ddff-46b1-a222-2467a13ea6a8)(content(Whitespace\"\\n\"))))(Tile((id \
         2c08a341-d1b5-4dd5-9bda-b273f726a236)(label(RecordHarvest))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         c3922d77-45f3-44c1-8f54-c03733d24bdc)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         69b8b337-48f6-496a-b8dd-00771e8ca13d)(label(h1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         ca7bcfca-8958-47b9-b72e-99afede0cc84)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         e238da3a-5019-40d9-9cbb-b01fa06884b7)(content(Whitespace\"\\n\"))))(Tile((id \
         b519329e-238c-4838-b705-0c59960316cf)(label(RecordHarvest))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         3dc00716-c89b-48cc-a964-1cb2355b8329)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         1b52bd87-0cad-4d3e-b58f-1598b9c6a409)(label(h2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         813a35ce-b02b-464c-bfb0-ca295db90996)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         8c42a862-095b-4142-bba5-7e423c271d49)(content(Whitespace\"\\n\"))))(Tile((id \
         603c5b9e-b4ab-4693-a413-909cb95f1982)(label(ClaimBonus))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         dc246695-4716-4dd5-8204-d88f0fb0e63a)(content(Whitespace\"\\n\"))))))))))))))(Secondary((id \
         4fc14dbc-1153-40b6-a4f7-8e48323e6ff0)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         c0f639ea-f5db-49eb-b47f-563135697414)(content(Whitespace\"\\n\"))))(Tile((id \
         4c852c24-83e6-4b37-8a79-557cccfff468)(label(ledger))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         dce4d5f7-821c-49c7-8ba5-3352a01b69bc)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         493f6d9c-3e16-4f49-bfd3-a63e42e2cc49)(label(streakBonus))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         a8b2d54a-1bcb-4151-ab53-4813f4863118)(content(Whitespace\" \
         \"))))(Tile((id \
         eb983e7c-d10d-456e-920c-97ac2669ff76)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         b2386bb6-4aad-45f9-a18c-5fbc0d8787a9)(content(Whitespace\" \
         \"))))(Tile((id \
         6850fa0f-25c7-4dd7-93f3-6daa2b3d515e)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         8544fca2-f97e-4677-9c22-04b8d1fb99e8)(content(Whitespace\"\\n\")))))))))(Tile((id \
         cc9c2448-c83d-42be-bdbf-55949d88ac82)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         7ca7b670-79af-4411-b0f1-d3a383d9f753)(content(Whitespace\"\\n\"))))(Secondary((id \
         4ff8f6af-05ae-450c-bc6a-4f8fc2a45bef)(content(Whitespace\"\\n\"))))(Secondary((id \
         aa68708d-deee-4bca-8dd3-1659b4927e7c)(content(Comment\"# PremiumSale: \
         low streak gives 1x multiplier #\"))))(Secondary((id \
         6f1bf985-aaa1-4b3a-978c-1a04f5afed00)(content(Whitespace\"\\n\"))))(Tile((id \
         e11ccc30-b427-4489-b7e4-63048fd39b4e)(label(hint test end))(mold((out \
         Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         63488311-d3fb-4130-a08e-a4fa19d5c048)(content(Whitespace\" \
         \"))))(Tile((id \
         2b206f6a-96a9-4ace-8aaf-f7469a283557)(label(\"\\\"PremiumSale with \
         low streak uses 1x multiplier\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         0f3aee61-48bb-4c5d-9f3f-8cead48dae06)(content(Whitespace\"\\n\")))))((Secondary((id \
         94e0bedb-0df9-4311-b796-c415d46d5f8e)(content(Whitespace\"\\n\"))))(Secondary((id \
         67682295-17e5-4cdc-82c8-21d68e6fbfe6)(content(Comment\"# Two \
         same-quality harvests build streakBonus to 5 #\"))))(Secondary((id \
         cf513c4e-31c2-44fa-b3ab-07ca18469418)(content(Whitespace\"\\n\"))))(Tile((id \
         ede2a641-d7a8-4688-b4f6-5fd9bb159c29)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         0041ee47-0984-46f2-933b-2d054271be21)(content(Whitespace\" \
         \"))))(Tile((id \
         35e925f9-92f6-4352-ab5d-b2f1e8cc2c48)(label(h1))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         f93770e0-5df9-4472-ab67-966cdf49059a)(content(Whitespace\" \
         \")))))((Secondary((id \
         3e0ff2b4-9b32-4d22-b740-179414e74d95)(content(Whitespace\" \
         \"))))(Tile((id \
         0fbad86f-adbb-4979-b36b-75e944f512cb)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         e74822ac-d7ae-4f83-a6d6-ac088ea730b8)(label(crop))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         c50ff247-68e7-48be-b6e1-9ab654361da8)(content(Whitespace\" \
         \"))))(Tile((id \
         d168bd8e-7408-4fe0-9abc-e72a6f88c0a8)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         c9336986-1d68-4065-92bf-35ffbbfc6a8e)(content(Whitespace\" \
         \"))))(Tile((id \
         082e755c-5b91-4a97-8f65-25a67a3c1697)(label(Moonmelon))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         c211831b-353a-4508-b202-d7c1f35d6df8)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         3a970776-cb12-4fa7-9f37-a201aae60bb8)(content(Whitespace\" \
         \"))))(Tile((id \
         72837636-1721-41e9-885c-4bd413c26791)(label(quality))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         d7b08eed-b78d-4b36-af20-76fc81e51471)(content(Whitespace\" \
         \"))))(Tile((id \
         c9225943-3346-43cf-a37a-4fba481f1aa3)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         9041a67d-b827-4569-9a79-666dd1bc7b96)(content(Whitespace\" \
         \"))))(Tile((id \
         7d32f8fd-d47f-4886-a4d5-f885c4271f2e)(label(Silver))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         1ea950c3-7c63-4e32-a10e-9d7bf8d5ebca)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         86aef11a-d61e-4c4d-a003-97b0f9465c30)(content(Whitespace\" \
         \"))))(Tile((id \
         dde6a89b-8178-4e04-a6f5-36cdeafeae47)(label(quantity))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         a2f53f47-c324-4503-af2c-5216425d16c4)(content(Whitespace\" \
         \"))))(Tile((id \
         340b1ff5-db3d-440a-84c1-f40ce108640f)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         ac99808a-90bc-4f0d-895e-996ca17abcfc)(content(Whitespace\" \
         \"))))(Tile((id \
         1f4148ab-3e42-4de6-aa05-5163ef4e836c)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         028c85ad-055c-4e70-8a8f-0772f094967e)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         21291fa8-2398-4aa6-8e84-ab815d806703)(content(Whitespace\"\\n\"))))(Tile((id \
         9851a7ba-7b6d-47ff-af4e-eca2b347a726)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         0d446038-bd8c-4226-a958-c5cb29e5238d)(content(Whitespace\" \
         \"))))(Tile((id \
         49e0a7b8-3b42-46b4-9d28-e942e9335d52)(label(h2))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         0a694e73-925e-4f7a-86c6-7f0b1bc0493e)(content(Whitespace\" \
         \")))))((Secondary((id \
         1b51126a-49fb-4caa-8f1b-350f674c6f6e)(content(Whitespace\" \
         \"))))(Tile((id \
         4802a1f5-1d7b-49d5-92ed-385865debf43)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         999e3d77-82bb-4d56-89c8-0a545c1cc494)(label(crop))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         9e4f24ea-6b4c-418a-8d59-c5856cf05799)(content(Whitespace\" \
         \"))))(Tile((id \
         e4cd0e1f-1a0a-41df-b495-61a05d6a1295)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         bc691b8b-a344-4b57-976c-3b97499b5066)(content(Whitespace\" \
         \"))))(Tile((id \
         e6ab2470-e268-40d6-93eb-9b66d61e2c28)(label(Starfruit))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         af1c7ce3-ff67-48e9-ab2e-82f1b796bfce)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         a6ff5d3e-7b6b-4d5a-b368-523e60e86d75)(content(Whitespace\" \
         \"))))(Tile((id \
         0316ceea-7a88-4b13-8652-6475bda803d3)(label(quality))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         249c7125-0083-43dd-a4e8-e3476892465e)(content(Whitespace\" \
         \"))))(Tile((id \
         5778c74c-901a-411b-a2ad-8a3547b32993)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         62f99ad3-e1e6-471d-a451-5855eb0b33dd)(content(Whitespace\" \
         \"))))(Tile((id \
         aa90540f-de15-46ba-b604-d26132e3786d)(label(Silver))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         ccf7fcdb-c7c2-410f-8769-7ad2b82a9b38)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         86ef9133-932c-4b62-83b3-3e9b9ee9357c)(content(Whitespace\" \
         \"))))(Tile((id \
         1b1cd6f9-bf5d-4cb1-8fb9-bdc75d482195)(label(quantity))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         731d1ed5-e42a-4bfb-9817-8c134f17ed18)(content(Whitespace\" \
         \"))))(Tile((id \
         0a272aa6-acd4-4169-8ad8-dd9f51c956f3)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         6e131d61-0c34-4e18-a75c-6b7f1bb20729)(content(Whitespace\" \
         \"))))(Tile((id \
         cbd545ae-0102-4767-99ce-d70c5f662f03)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         c7e2c741-cde8-4c28-9087-cf392f882965)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         4a6eb91b-4e13-4566-885e-079bdbd7c0a9)(content(Whitespace\"\\n\"))))(Tile((id \
         227534f7-fa74-4074-8b6c-8e9b2011876f)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         091f474d-7bf5-4f01-9776-b5be579ba6de)(content(Whitespace\" \
         \"))))(Tile((id \
         07fa4f7b-5548-4388-936a-61cfc84dc512)(label(ledger))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         17ed9e80-0d0f-47ab-ac7f-ac60aeb7aa16)(content(Whitespace\" \
         \")))))((Secondary((id \
         c8f2b7aa-c3b4-4fdb-91be-48afc35ba5c1)(content(Whitespace\" \
         \"))))(Tile((id \
         d77ca5e3-04b2-46e2-adfa-995d71f6bbd4)(label(run))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         941000cd-f382-4723-b7f9-12b2c5c2246e)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         4e9b2a4c-dd5f-4976-9ea7-2fc4d1597d8f)(label(initModel))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         906d0dc4-8889-4509-acee-17b2d3110e2a)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         c78a0e48-387f-4739-aea1-9c17a1262b7d)(content(Whitespace\" \
         \"))))(Tile((id 56cd492f-b44c-4607-8db9-5653b903f317)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         80c0572e-975b-40ed-8920-e0b1af8ae328)(content(Whitespace\"\\n\"))))(Tile((id \
         5ea3b41d-2e47-4bc0-914a-05457f531320)(label(RecordHarvest))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         de0e4835-c715-494a-89d1-b342d003211e)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         3c4d0357-3b84-4f65-9b32-b1c17dda46a6)(label(h1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         22947983-8abe-4675-93ac-fd1276a12ba5)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         3204be58-ac10-4387-9f19-097c343b5e84)(content(Whitespace\"\\n\"))))(Tile((id \
         32f9bf4d-f5f2-42c2-9b90-01d836eab85c)(label(RecordHarvest))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         50691f4a-02f1-4528-a70c-8bcd327c328d)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         9bb8e665-4b44-4ccb-9481-bc6568095554)(label(h2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         c36b4325-6ba1-4f8a-b93d-60c668fbf0a6)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         f853f195-60a1-4bec-9440-a14ffcf70dca)(content(Whitespace\"\\n\"))))(Tile((id \
         16689cb7-f726-4f6f-b981-108def1b78ac)(label(PremiumSale))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         f9e78b40-5835-4a93-b3e0-e23fa8c8f078)(content(Whitespace\"\\n\"))))))))))))))(Secondary((id \
         5ca06a68-9a80-4b4a-af1a-7102167275ec)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         58e87fdb-f580-48fc-bf15-f8b72a9c5719)(content(Whitespace\"\\n\"))))(Secondary((id \
         cd95ce80-3c24-4ca2-9a9d-93bde4ad5a5a)(content(Comment\"# streakBonus \
         was 5, multiplier = 1, payout = 5 #\"))))(Secondary((id \
         686e3245-d700-404c-a3e4-69af99929891)(content(Whitespace\"\\n\"))))(Secondary((id \
         90e7178d-cbfc-4c5a-9a99-868ef89cb3bf)(content(Comment\"# h1: 15*2*1 = \
         30, h2: 20*2*1 + 5 = 45, PremiumSale: +5 #\"))))(Secondary((id \
         9a6ff8af-d5d8-4c7f-935d-ba40050571de)(content(Whitespace\"\\n\"))))(Tile((id \
         6ce3cefa-f804-4cee-936f-d7bae2eabc1b)(label(ledger))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         c67c5016-9a38-43f5-bd9e-07c885851441)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         a70be366-b888-4ca1-8afb-3f9e4afcedba)(label(totalValue))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         c89045c2-3bf5-4f41-8d68-c6cbbd680e25)(content(Whitespace\" \
         \"))))(Tile((id \
         f43b5ebd-c4e4-4bb6-a2cf-358f1df5fe2a)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         178b726a-16ba-469d-9a3e-b330727dfd9b)(content(Whitespace\" \
         \"))))(Tile((id \
         541a3d01-b41d-4ff5-bae2-11cd7c46b0b3)(label(30))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         b5d42bb7-fad1-4e51-95c1-99344e500d13)(content(Whitespace\" \
         \"))))(Tile((id \
         b6ec4152-b259-43ca-b77d-12ac75c26f70)(label(+))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 28))(sort Exp))((shape(Concave \
         28))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         d6fd26ea-d414-4a33-b26b-c317e3689019)(content(Whitespace\" \
         \"))))(Tile((id \
         4f338c01-810f-4b17-ba5e-6023367118a1)(label(45))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         8b0f357b-d49f-4c02-8e60-6211f750e171)(content(Whitespace\" \
         \"))))(Tile((id \
         5b7d17ea-829c-4de3-91d5-b87e2b65aa3f)(label(+))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 28))(sort Exp))((shape(Concave \
         28))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         41b87af6-4c26-435d-b720-2979d9193e70)(content(Whitespace\" \
         \"))))(Tile((id \
         a5ae227c-05d5-47fb-91df-cd3d4d2dd54d)(label(5))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         d94df353-3bfc-46fc-91c7-a83f6226ace2)(content(Whitespace\"\\n\")))))))))(Tile((id \
         bdee8d50-0bf9-415c-89b6-c539d17d74a2)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         1282b322-d626-49b1-a5c7-f978ab2328b0)(content(Whitespace\"\\n\"))))(Secondary((id \
         1383b74e-72e5-428e-8c90-329c4d513150)(content(Whitespace\"\\n\"))))(Secondary((id \
         d3787808-18ff-4438-b4be-bca87fddede1)(content(Comment\"# PremiumSale: \
         high streak gives 2x multiplier #\"))))(Secondary((id \
         0d4ae08e-3c38-4b3b-8b36-5fe10e7ea3b3)(content(Whitespace\"\\n\"))))(Tile((id \
         9ec06324-9e62-40a7-b937-0e6739c869c2)(label(hint test end))(mold((out \
         Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         7164a6d3-6af5-4720-bda2-5b14bd03a70c)(content(Whitespace\" \
         \"))))(Tile((id \
         6b7fdaa6-5eda-4247-810f-ccd0a28b37db)(label(\"\\\"PremiumSale with \
         high streak uses 2x multiplier\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         75854b51-e791-47af-8d27-2a5105199015)(content(Whitespace\"\\n\")))))((Secondary((id \
         936775f6-3411-4f7f-b22f-b92fea9a1245)(content(Whitespace\"\\n\"))))(Secondary((id \
         8624c8f7-63a3-426a-946d-7bcec8964d59)(content(Comment\"# Three \
         same-quality harvests build streakBonus to 10 #\"))))(Secondary((id \
         ff6a6ea1-4fd5-45d4-8de5-b368c9a2706f)(content(Whitespace\"\\n\"))))(Tile((id \
         0776d874-b28e-4cfb-8886-527f8c10d184)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         ff06f2fc-a985-4ad3-b89b-06f764577e33)(content(Whitespace\" \
         \"))))(Tile((id \
         8503b82e-9212-4cdf-b746-877f5b3e76e5)(label(h1))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         9f302ffe-8032-454c-be2a-f0baa11b664e)(content(Whitespace\" \
         \")))))((Secondary((id \
         272183a5-7fe1-4621-bc53-06ffb316a455)(content(Whitespace\" \
         \"))))(Tile((id \
         db2d5f4c-6125-4b2c-8525-5e126ce6f914)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         68860a32-8142-4768-88d0-c19b0ed87105)(label(crop))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         4f700fdf-bea0-46ab-b8ce-6d377ca3d549)(content(Whitespace\" \
         \"))))(Tile((id \
         23ff9148-d3ca-4530-bafe-1ace9e7dcd6d)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         bd40e9c7-0443-4b6f-aa8c-86881ec29894)(content(Whitespace\" \
         \"))))(Tile((id \
         ea4ada74-31a3-4110-88ae-102c00846f72)(label(Moonmelon))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         f2b7570c-a759-4836-af11-056ca762c753)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         e5158df1-7bf9-4dc3-a1f8-824d66fd7afc)(content(Whitespace\" \
         \"))))(Tile((id \
         34053c70-333c-47fd-a33d-1ae9437e7cd7)(label(quality))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         de0836a1-f051-480f-8d42-e4639feb1472)(content(Whitespace\" \
         \"))))(Tile((id \
         31107c4a-7126-40ad-9724-dcfd182747c6)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         8010c4a5-c1b6-4194-8c40-5ac15a5874e9)(content(Whitespace\" \
         \"))))(Tile((id \
         3625d5f5-2270-4cf6-88ba-b7496bb1f454)(label(Gold))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         474e329c-2f72-4552-80ab-2cb3e4041d77)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         3d38a786-05a8-4414-a925-4edccb788117)(content(Whitespace\" \
         \"))))(Tile((id \
         84e83b3c-33dc-4290-bc6d-de96fe99957e)(label(quantity))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         ee6d63e9-d38a-420b-bb8e-2ed665eebd28)(content(Whitespace\" \
         \"))))(Tile((id \
         3968f08b-2d63-4f67-b2ec-1bd54c1c0683)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         507c3ab8-426d-4fb8-965a-eea3239fd59d)(content(Whitespace\" \
         \"))))(Tile((id \
         0dc6a74a-b6bf-41e5-9f5b-da1e7677d2d3)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         5faea1c8-f8d1-4d0d-8972-4a59c48db322)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         af4ae9ba-ae83-4c35-8a95-6f91b2109d02)(content(Whitespace\"\\n\"))))(Tile((id \
         5a88414f-db43-41e5-b552-96bb65be2e22)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         a2eadb07-234e-42f7-92d1-921552eed51b)(content(Whitespace\" \
         \"))))(Tile((id \
         c1eea3fd-9bd7-49ef-89ef-cbf51977a19b)(label(h2))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         c949644a-a4d9-4ec4-967b-09c02a74ea30)(content(Whitespace\" \
         \")))))((Secondary((id \
         4ecb8243-2c07-4857-914b-48bc71fcac66)(content(Whitespace\" \
         \"))))(Tile((id \
         02309d13-99b9-477d-991c-0e9c4661bc53)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         dccebaab-264e-4ec7-89db-831c773b711a)(label(crop))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         62029544-a82d-449c-a9d6-e51bc489dc6c)(content(Whitespace\" \
         \"))))(Tile((id \
         d04c5c61-d9bc-4bb4-9eed-29409fb06f38)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         81204d97-9f8e-4c3e-a31f-287e13705d8a)(content(Whitespace\" \
         \"))))(Tile((id \
         d0df121a-3759-4542-a4d9-56aed397001c)(label(Starfruit))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         412e9e4a-bfa8-401e-a5af-57b2cc4ff0a5)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         513feed0-9cd8-4648-a2ec-9294e82050e3)(content(Whitespace\" \
         \"))))(Tile((id \
         1d7eb808-d862-45c2-83d1-34cb2c781adf)(label(quality))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         b64b6d78-d06b-45c3-9c19-75c40520dabd)(content(Whitespace\" \
         \"))))(Tile((id \
         4de9a62e-a515-4c65-b34e-e88c2becde6f)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         3734b4df-69fc-4edb-bbf8-b3b06699822c)(content(Whitespace\" \
         \"))))(Tile((id \
         a73c16df-3222-4ea3-b311-53b66c9615a1)(label(Gold))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         9495d23b-cbca-4bf9-9905-9bedc6801613)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         13bafb65-7925-442a-8a4e-fc963c4a0b44)(content(Whitespace\" \
         \"))))(Tile((id \
         157dbd3c-29f9-47bd-a278-df25716f3704)(label(quantity))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         48823b6c-c4d3-49f4-8657-e93f7da5c402)(content(Whitespace\" \
         \"))))(Tile((id \
         c39fad0e-70e7-4d85-9997-16bead1d967a)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         545882ca-aed0-4488-a873-49a5412a510d)(content(Whitespace\" \
         \"))))(Tile((id \
         e75ece85-52dd-46e9-abe1-77f4d5b885b8)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         08811533-aa0d-4f1d-a386-34629d4fd6ed)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         376cc4a8-8250-49d3-b7fc-63acd249c8e7)(content(Whitespace\"\\n\"))))(Tile((id \
         4fd3da1e-af49-46a4-a916-b56aaf04dac0)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         d18e1395-310e-4001-856a-077085002688)(content(Whitespace\" \
         \"))))(Tile((id \
         957d99e8-3f14-4fbc-ae80-f6ab112ae1d4)(label(h3))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         82fe4204-d570-412b-a761-e2b1701b2905)(content(Whitespace\" \
         \")))))((Secondary((id \
         3d510cae-5021-41a7-8c7d-7677dc4ab88f)(content(Whitespace\" \
         \"))))(Tile((id \
         602d3780-272e-47e6-8104-ec56df7dc715)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         95b11493-5d35-4040-981c-7f9e83d72918)(label(crop))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         fd967e6a-478a-43bd-8732-c708cfbfd164)(content(Whitespace\" \
         \"))))(Tile((id \
         b032a50a-f9c8-44e4-a671-22e57e1e3b36)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         3d9c1536-2947-47a7-957f-6e7f48a363b9)(content(Whitespace\" \
         \"))))(Tile((id \
         fd42a33d-dce4-4c88-875f-d9ee7c1bf363)(label(Nightshade))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         3fadc97a-e88e-4a21-b53b-6ede01d0f7b5)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         a427f774-2924-4360-894c-429315bbe6de)(content(Whitespace\" \
         \"))))(Tile((id \
         ff1d779c-fa17-4373-8b2f-c11b54eafde7)(label(quality))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         1f040d24-412b-457a-8ca9-1a93761db810)(content(Whitespace\" \
         \"))))(Tile((id \
         5e6e8999-bf92-4a2c-8c25-a8b573ba4653)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         c725f22e-e226-4173-bae8-0393068fab07)(content(Whitespace\" \
         \"))))(Tile((id \
         f4ad33c6-bb45-446e-96a4-f292091a65e4)(label(Gold))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         60315dac-30f9-4ce1-82f7-e79ed6e74f3c)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         b28f381c-41ee-4b1a-a7f8-0923c3d9597a)(content(Whitespace\" \
         \"))))(Tile((id \
         32357dac-5776-48bf-9632-11a566d4b8ed)(label(quantity))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         36f2a008-301f-4222-af86-f379ddefd43d)(content(Whitespace\" \
         \"))))(Tile((id \
         609ef024-6964-42f9-a4c6-fe17bc2e4782)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         3657072f-128e-45a6-857a-410ab459d866)(content(Whitespace\" \
         \"))))(Tile((id \
         f99e1332-ae04-4365-a93a-01a3dde25afc)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         6a7c95ec-2d7c-40f7-bea7-8d3187a8cc12)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         f2eea2e9-77e9-4266-a6a2-90e944648da2)(content(Whitespace\"\\n\"))))(Tile((id \
         f2172e3c-cd4a-4b1b-947c-784c3834c1ca)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         990b1597-cc76-49dd-8235-5667915d3aa9)(content(Whitespace\" \
         \"))))(Tile((id \
         19ec6c5d-68a4-417c-970e-fa840c65d318)(label(ledger))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         bf8c0af6-14d9-4021-a68c-f2d7014929ce)(content(Whitespace\" \
         \")))))((Secondary((id \
         f843a593-7ad4-4819-98ac-c12a7f76d47a)(content(Whitespace\" \
         \"))))(Tile((id \
         fedba7d8-db39-41d8-89d3-87dec3bf1c34)(label(run))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         c17f2349-8964-43bf-a5a9-a3cc580525c5)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         ad630dc9-1a55-48fd-af14-073729803da6)(label(initModel))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         c05591f1-3ddf-4c8c-95cf-389ea3eeb6fc)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         38c8fbfe-adc7-4efd-bd98-1a13f918263b)(content(Whitespace\" \
         \"))))(Tile((id 029ba1e9-75ec-45eb-a3c6-0ea2541f9d8e)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         087229b3-c6ad-4e6c-80e3-980dac9d695c)(content(Whitespace\"\\n\"))))(Tile((id \
         f26d5a6d-aba6-4844-b1e3-26e05a27822b)(label(RecordHarvest))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         32b2cfbf-f99a-4cf5-86c6-b37e3b8b570c)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         33c61458-8c07-4664-8429-654fcbd18b35)(label(h1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         d5dbb461-b98d-41c9-b2b3-c2d6d03233e9)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         1ba7cfc4-929b-492b-9008-5bcee0b927dc)(content(Whitespace\"\\n\"))))(Tile((id \
         e7055fda-3231-4eac-ab00-1cb21f652d35)(label(RecordHarvest))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         4a479647-d5e9-461b-816a-e54490c6e445)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         ca6dba9a-cac6-4060-a69b-afd3a8dcb638)(label(h2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         0079aaf2-475d-410f-98a9-9069e6f7a26d)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         d3e644cc-b6a5-4eef-95f3-9f2688d33e85)(content(Whitespace\"\\n\"))))(Tile((id \
         74d65efe-a27a-4230-9b36-f7e1e035fe41)(label(RecordHarvest))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         a70cf30c-94ff-4ec0-92aa-100839762665)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         21285ca9-d7ae-4934-bb8d-6b01778b9926)(label(h3))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         f9781702-f234-407c-b71e-e23a7774104f)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         26032318-98ca-4fd3-910b-a643cf6ba8c0)(content(Whitespace\"\\n\"))))(Tile((id \
         86d68df2-64a3-43f4-9403-b3d426054e6e)(label(PremiumSale))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         46862f17-76af-47df-987c-90e45a33e37c)(content(Whitespace\"\\n\"))))))))))))))(Secondary((id \
         97c35ac1-e24d-446a-a58c-db5b1900e092)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         d332c037-c0b9-480e-b08e-e103955cfe31)(content(Whitespace\"\\n\"))))(Secondary((id \
         8db7a79a-214e-4859-8eb4-996d98185578)(content(Comment\"# streakBonus \
         was 10, multiplier = 2, payout = 20 #\"))))(Secondary((id \
         2eda8aeb-5024-4855-b7fa-bbec0c987258)(content(Whitespace\"\\n\"))))(Secondary((id \
         c52638df-c5e4-4440-9d74-f0fa01487fea)(content(Comment\"# h1: 15*3=45, \
         h2: 20*3+5=65, h3: 20*3+10=70, PremiumSale: +20 #\"))))(Secondary((id \
         31843049-74df-407a-862f-78b03050a57f)(content(Whitespace\"\\n\"))))(Tile((id \
         fb79319a-e918-42f0-b2f6-9e01ece20990)(label(ledger))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         bcca4790-1e44-4254-9b4f-6d20997c651d)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         4d85bebc-c558-48fe-8cf3-a1763bc4b399)(label(totalValue))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         d5294434-93be-4550-a531-bcc612a5bdc4)(content(Whitespace\" \
         \"))))(Tile((id \
         344a118a-370f-4115-a08d-2046aafe0c58)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         5dedf6a5-1b10-4fb0-92de-5ec730e79744)(content(Whitespace\" \
         \"))))(Tile((id \
         40e11bd2-6459-432a-a039-d77b8cb0c86f)(label(45))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         3f356b0b-b159-4667-b4f2-ddbf0d308f0f)(content(Whitespace\" \
         \"))))(Tile((id \
         4a6cf2f1-e175-4036-bfb2-0dde5e1ff921)(label(+))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 28))(sort Exp))((shape(Concave \
         28))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         237ceba4-5512-4a74-bdac-c6b01338984a)(content(Whitespace\" \
         \"))))(Tile((id \
         96de202a-3743-4620-8d6b-10b654a4e1bd)(label(65))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         b9a737df-d195-4005-ab8e-705bb4118dec)(content(Whitespace\" \
         \"))))(Tile((id \
         6b0375d8-bb8f-41d2-8455-0ef1b0b33c0d)(label(+))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 28))(sort Exp))((shape(Concave \
         28))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         3d86470d-43c6-40db-9ed8-2d8d41cb78d1)(content(Whitespace\" \
         \"))))(Tile((id \
         92e3fa01-0289-48cf-a126-d16cf1e78543)(label(70))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         7b0c3e7f-735a-4952-bc10-5880018daca7)(content(Whitespace\" \
         \"))))(Tile((id \
         c2e5bc7a-4dea-4aa7-856c-7f21cc53d683)(label(+))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 28))(sort Exp))((shape(Concave \
         28))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         d0e25e12-e1f7-4e2c-8d87-dc63ca16b6d7)(content(Whitespace\" \
         \"))))(Tile((id \
         f74fbd4f-ffb7-47bb-943f-3dc7918a6e8c)(label(20))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         81c97aa4-4132-41c4-9a9f-f76db6f79b79)(content(Whitespace\"\\n\")))))))))(Tile((id \
         f4153157-6dce-4f17-be53-435274ca6555)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         cfbfdd8e-4b9e-43b3-8944-12795d3e0b07)(content(Whitespace\"\\n\"))))(Secondary((id \
         cf190e71-6202-459d-9900-2e20c069224a)(content(Whitespace\"\\n\"))))(Secondary((id \
         ab0d8e82-182a-4873-8e77-4bdf0a468d12)(content(Comment\"# PremiumSale \
         resets streak after claiming #\"))))(Secondary((id \
         dd92ce25-b569-42a1-be3c-ea5e774bfacf)(content(Whitespace\"\\n\"))))(Tile((id \
         54cb86df-c5bf-4f2a-9aea-082938f60f43)(label(hint test end))(mold((out \
         Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         e99641ba-ed0f-4271-bc19-174ea1822926)(content(Whitespace\" \
         \"))))(Tile((id \
         83c63b56-af1a-45e4-951a-8f7c4db6b5db)(label(\"\\\"PremiumSale resets \
         streak to zero\\\"\"))(mold((out Exp)(in_())(nibs(((shape \
         Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         1462b212-0d11-4937-8c8a-a5aab063f967)(content(Whitespace\"\\n\")))))((Secondary((id \
         90a968e0-6e51-432e-930c-d61fecd19471)(content(Whitespace\"\\n\"))))(Tile((id \
         fde97d56-40e3-4d52-98df-6060185bb99d)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         82f76c81-e567-4c68-86b6-1c42d29be896)(content(Whitespace\" \
         \"))))(Tile((id \
         7c5059fc-5067-4f08-a686-2d7bf229e5a5)(label(h1))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         9864834c-8019-429d-a21d-d4846020f721)(content(Whitespace\" \
         \")))))((Secondary((id \
         c544e728-1942-464f-ae23-60cb05b7c685)(content(Whitespace\" \
         \"))))(Tile((id \
         06fecab1-eefb-4529-a241-7a7ed39de214)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         b450c1c8-7344-4bdc-a759-87c35d138eee)(label(crop))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         d96373a7-6c8d-498d-b79b-6d933df8064c)(content(Whitespace\" \
         \"))))(Tile((id \
         0fe41abc-1c76-4eac-8fcd-2acfb4d6db6f)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         d1d59e79-307d-49a7-beb5-04a47bdaeb9d)(content(Whitespace\" \
         \"))))(Tile((id \
         5bfeff00-5e22-4ff9-a635-18a7b18a51ab)(label(Moonmelon))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         c0653606-a8db-4b73-bd16-375a6ec57a23)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         15e5c23b-8bb5-4afb-8a67-bbf084d609b0)(content(Whitespace\" \
         \"))))(Tile((id \
         f0b989a8-9e18-47d8-9284-aa21507ba1fb)(label(quality))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         62c5df32-b037-49b6-93cb-455161087010)(content(Whitespace\" \
         \"))))(Tile((id \
         734464af-bc1d-4987-857f-371b42595e86)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         497e9c72-30b8-4829-b608-1e322063404b)(content(Whitespace\" \
         \"))))(Tile((id \
         78a49f2e-3ae7-40b5-a502-f1aaa2d79585)(label(Gold))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         d4b44c83-026a-4df3-89ec-4dbe91d437a2)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         b51cb90f-f9ef-42ce-a8d0-78150c735131)(content(Whitespace\" \
         \"))))(Tile((id \
         6e7735f3-8898-4206-94a6-4f526d73913c)(label(quantity))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         1ad69509-ebab-423d-be0a-58b726f49eea)(content(Whitespace\" \
         \"))))(Tile((id \
         b5637abb-4ecc-4c05-8b4e-da8ca954b00e)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         d734ccf0-8741-47a4-bb08-d4931b036c0e)(content(Whitespace\" \
         \"))))(Tile((id \
         7776df05-b89d-4bb5-9df7-4ff4d4fc4548)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         e72901ef-6749-4a80-8a6a-f2f3def09635)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         b9bf5c26-5c1a-4542-8c36-3aa11cbfc84f)(content(Whitespace\"\\n\"))))(Tile((id \
         41b68187-efe5-450a-8a57-a71e8d570def)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         8cf21ea9-d183-42bb-a8a7-e8d720030a99)(content(Whitespace\" \
         \"))))(Tile((id \
         3cee670a-70ed-494c-8c02-c026161e6b05)(label(h2))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         3cd818be-5c63-46e2-a1f7-a85e14843a64)(content(Whitespace\" \
         \")))))((Secondary((id \
         7da96d6f-2ae8-4076-b56e-f4ff8e789bca)(content(Whitespace\" \
         \"))))(Tile((id \
         0b8f83f2-4bea-4976-beba-1953ba20a096)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         90cf8595-880d-4e68-a8d8-9ebe3cec413b)(label(crop))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         8193d2e2-0721-4def-a16c-d6e4c98eadeb)(content(Whitespace\" \
         \"))))(Tile((id \
         0640fa96-ee01-4119-a1e0-cc0a585032cc)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         9b326d6a-3c3c-45cf-b9a5-b605a0e30c6d)(content(Whitespace\" \
         \"))))(Tile((id \
         3152cec2-c39a-43f7-ab03-33d9e34fe4ce)(label(Starfruit))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         9131922c-73ac-40bf-9683-e6f2d5920716)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         54c2e527-466e-4336-ab20-abe0ba70a066)(content(Whitespace\" \
         \"))))(Tile((id \
         bc82ccf0-1895-4740-b91a-86ccb8d941e5)(label(quality))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         1752bc09-17bf-4867-bdab-17150ec65996)(content(Whitespace\" \
         \"))))(Tile((id \
         64968d60-edec-4adc-b9e7-259267b6a13e)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         d8cb9b3d-ed4b-4ce0-9623-507b927b1b91)(content(Whitespace\" \
         \"))))(Tile((id \
         cc49807b-ab82-49d3-b89c-5ded9058def7)(label(Gold))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         2a6028c6-fad2-4735-8c81-8f953f73367a)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         6c2f8c72-bbcd-4b40-a50e-f6dcee01c23d)(content(Whitespace\" \
         \"))))(Tile((id \
         ee7152be-a597-44cc-9fa0-ed9b9ede057b)(label(quantity))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         5e3ea410-31ce-4c78-9892-1a632b4942c7)(content(Whitespace\" \
         \"))))(Tile((id \
         4eb2c921-0763-47d0-aea4-8dbe9f2df3a7)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         4735b68a-7c18-470c-97a2-da86a631af95)(content(Whitespace\" \
         \"))))(Tile((id \
         ce2c62df-48a1-4173-b337-dd97e6041c5f)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         b5409917-8c3d-4cd0-b625-1e4b88dfdfc3)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         78f203d1-03ca-4b07-bda5-6bb739bb1b2c)(content(Whitespace\"\\n\"))))(Tile((id \
         92f98d4f-2265-4954-8a30-c33dd7aa320a)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         b4045ce0-705f-49f4-a45b-cba1c614ce73)(content(Whitespace\" \
         \"))))(Tile((id \
         5c606c93-704a-4fa1-82e8-45c5032f231b)(label(h3))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         20d430c9-c113-4242-9dac-0f389f1fd71e)(content(Whitespace\" \
         \")))))((Secondary((id \
         df575eb4-9e7c-4a7a-8475-78b99eee90bf)(content(Whitespace\" \
         \"))))(Tile((id \
         4ad43051-1b71-4f34-8c4e-1d5c0417bd16)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         373fe841-1259-43b3-8429-e22dbf37606d)(label(crop))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         33386478-5286-4009-9dd3-4d41fc163372)(content(Whitespace\" \
         \"))))(Tile((id \
         840eb377-91e7-4a70-b88b-4e3ef4f0d05b)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         5167d189-fa32-4359-bbdb-94fc5467e553)(content(Whitespace\" \
         \"))))(Tile((id \
         53dd2257-9dfd-4770-8c3c-d7b55be75f33)(label(Nightshade))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         c6452d51-548e-4753-a234-71769f884a73)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         972ac6cc-744b-4356-ab2d-5ae7bfefa9a7)(content(Whitespace\" \
         \"))))(Tile((id \
         82ba7471-4792-4d44-97fa-669700d476fc)(label(quality))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         52066afc-71d9-418a-bf62-3baeeea24aa9)(content(Whitespace\" \
         \"))))(Tile((id \
         7247890d-77a9-4f39-a623-30ed2b4afd72)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         21fdbad0-3203-4e1d-a930-55eeed7cd788)(content(Whitespace\" \
         \"))))(Tile((id \
         b2d4c86e-8e26-49f2-96c8-a5dc8d9af6ba)(label(Gold))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         66782c63-47e5-45cf-8402-34e9dd8c88ed)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         47c039f6-6998-4651-a72f-c9bb04677f13)(content(Whitespace\" \
         \"))))(Tile((id \
         a427ab46-88dd-4af3-9423-7244485eb263)(label(quantity))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         ab7e4738-348a-40c8-ae93-362a6e46b933)(content(Whitespace\" \
         \"))))(Tile((id \
         6cf903c0-ccb6-4076-8257-98c0ef7d9bef)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         71bb093a-7c8e-4309-8caa-833746e73688)(content(Whitespace\" \
         \"))))(Tile((id \
         a4b4beac-802d-4d78-9dc3-b1b65b4a5d19)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         704851da-bc18-47bf-af1e-35eedb2dd282)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         21e22481-6e21-41e8-a105-872521add952)(content(Whitespace\"\\n\"))))(Tile((id \
         42edce6b-82cf-4fcf-bfd0-119bf2c22944)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         45b119d8-b8e9-4c10-8b74-95953bbee650)(content(Whitespace\" \
         \"))))(Tile((id \
         4b5df97f-dcaf-4072-9df7-2e71f95fa4a8)(label(ledger))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         1bc6d868-947f-4198-b3fc-e907e16953b1)(content(Whitespace\" \
         \")))))((Secondary((id \
         a09ae1e3-8322-4124-997f-278fd3d752d7)(content(Whitespace\" \
         \"))))(Tile((id \
         268c3b2d-4868-4355-9782-1a5176d33f21)(label(run))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         f009ffe3-4a26-481e-8f0a-fac368c91863)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         1b50eb77-ff4c-4914-923d-f1c90f5233eb)(label(initModel))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         da1dddea-cc62-40ba-b4f7-a1a778d129fb)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         95f49067-8544-4359-8241-4c861f122738)(content(Whitespace\" \
         \"))))(Tile((id b006c928-045d-4ed5-8456-5d1ab534ee91)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         f77032aa-5de7-4342-9ccf-919d8e26df11)(content(Whitespace\"\\n\"))))(Tile((id \
         03a691ef-ea38-49c1-8001-391291238e29)(label(RecordHarvest))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         92ea71e8-07dd-4cb2-adb7-cc92546d4783)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         d9b6d7f8-d29d-4350-b5b3-1db7fcdf0ec4)(label(h1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         840b997b-bc13-490b-bfaa-4c165384f397)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         19a6582c-3a0d-4cb7-a2df-31f8bdbb14da)(content(Whitespace\"\\n\"))))(Tile((id \
         eeddc2ec-6e2e-49ff-83b8-42ad03018cb4)(label(RecordHarvest))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         92bc83d6-831a-471b-b0f5-979ab98b1483)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         692cd9e8-d1c2-45e6-9b17-167bf002f2ff)(label(h2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         65ffef24-44bd-41f3-aa8c-fcce62113159)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         fc963c7e-024d-42b2-9b76-e1a4ba1b4930)(content(Whitespace\"\\n\"))))(Tile((id \
         5a1839e0-5216-4b61-ba89-0652231e0a7f)(label(RecordHarvest))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         d0c62cf0-1130-456f-91ad-0aeaac4190e2)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         ff24f464-3d87-4102-ab06-482fa8bd6c82)(label(h3))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         a7cb9a30-69ae-4c40-a52e-161be674aec0)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         81b2f6c9-be23-4fcf-8b8f-5e27b056b735)(content(Whitespace\"\\n\"))))(Tile((id \
         003165c0-551d-4fe1-a19d-becfda9695ac)(label(PremiumSale))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         70d76f6b-48a1-400d-ad36-b3e33dd7dc70)(content(Whitespace\"\\n\"))))))))))))))(Secondary((id \
         a005dcd2-4435-44cb-bf01-fa4a6825477d)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         245f1479-8f37-42e9-a855-9738dfd5f392)(content(Whitespace\"\\n\"))))(Tile((id \
         474d2a10-7e05-4784-aa6d-1f1589451261)(label(ledger))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         e37e00b0-f831-456a-940a-9175efdbd165)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         53965d0d-2d54-41a2-b12d-81846d1d0c0a)(label(streakBonus))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         fa4a943f-eefc-46ff-9b75-53f32112e7eb)(content(Whitespace\" \
         \"))))(Tile((id \
         d3dfdac4-f145-4a2e-8dbe-1c66e2edad37)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         e2f74f74-a9fc-4c4e-a1cb-3e0f904a4029)(content(Whitespace\" \
         \"))))(Tile((id \
         d38ca772-7544-4fdc-90db-6a09122345c3)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         edbd3650-f60f-4962-8595-3995eed9ba26)(content(Whitespace\"\\n\")))))))))(Tile((id \
         9ad9c3d2-64ef-4c09-b72c-67269cafdaed)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         d478c000-83e9-4369-8057-e64a029ed53e)(content(Whitespace\"\\n\"))))(Secondary((id \
         e167e519-fe9b-4927-8ab1-3e85e2d6ba96)(content(Whitespace\"\\n\"))))(Secondary((id \
         8293a19d-7811-4e14-9579-a6b998bc15d2)(content(Comment\"# PremiumSale \
         with no streak gives zero payout #\"))))(Secondary((id \
         b43938c7-0aa3-49dc-9b23-45c60d72d546)(content(Whitespace\"\\n\"))))(Tile((id \
         fc42e81e-9366-460c-bce7-67b2195270fb)(label(hint test end))(mold((out \
         Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         e1634936-54e3-4f73-9154-d9dafcb52e37)(content(Whitespace\" \
         \"))))(Tile((id \
         ea0ed949-d28b-46ef-a281-0d0661888ae7)(label(\"\\\"PremiumSale with \
         zero streak adds nothing\\\"\"))(mold((out Exp)(in_())(nibs(((shape \
         Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         8dd698c9-d381-4f13-8095-1c5e8ea141d9)(content(Whitespace\"\\n\")))))((Secondary((id \
         8cc4c6f5-e119-442f-8e26-1a3b8359c75e)(content(Whitespace\"\\n\"))))(Tile((id \
         52903586-e2b9-42ba-aa90-e596cc18e5d6)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         22270964-ff66-41b8-b301-6c83c001a148)(content(Whitespace\" \
         \"))))(Tile((id \
         fdc33408-81ce-487a-8236-21abdb185dc7)(label(h))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         ad8c749e-b70a-43e6-9a76-a05beed57229)(content(Whitespace\" \
         \")))))((Secondary((id \
         77fba38b-bbe4-47a7-8542-e688c808407c)(content(Whitespace\" \
         \"))))(Tile((id \
         a12a2110-602d-457a-99dc-d3463bae8fac)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         0d7d9956-a8e2-4fc3-80e3-4981cfc1f791)(label(crop))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         94316251-1e5c-48c0-9c8e-3b90a34941b2)(content(Whitespace\" \
         \"))))(Tile((id \
         fc3c6c72-a0ec-4265-b716-6d7ee4570bc6)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         13698cff-e5e6-414e-96fd-0bdfc1dc8444)(content(Whitespace\" \
         \"))))(Tile((id \
         8fe19b33-de3a-4f73-a1f7-81d7668e98ea)(label(Moonmelon))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         cec4e912-8290-420b-a2d1-897bec0559f4)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         e96d9ff3-6ccb-4724-9d66-bf9408e58bdf)(content(Whitespace\" \
         \"))))(Tile((id \
         28656ab4-8d01-4b75-832a-bc5979920468)(label(quality))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         121242b5-7507-4f10-a51b-b6da2d565005)(content(Whitespace\" \
         \"))))(Tile((id \
         f1f9b399-6836-44aa-90da-55a12d36e0f0)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         acfaeec5-368b-409e-b856-962aaa84f7ab)(content(Whitespace\" \
         \"))))(Tile((id \
         251fabb6-08fa-4bd7-b768-275f608fc684)(label(Bronze))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         555970ed-4d93-4148-ae8a-bf20af112b40)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         bb707f8c-0662-4cfc-b53e-5abbc1f67225)(content(Whitespace\" \
         \"))))(Tile((id \
         1f3db8ed-7762-47fa-96b7-5fadd40259dc)(label(quantity))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         0a5beeab-e691-4511-8b96-f98138b2acd0)(content(Whitespace\" \
         \"))))(Tile((id \
         50e7485b-4ac2-4372-b9d3-7b430ef277fe)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         3da89a4e-8b76-4a59-9fd0-05fdebcb508c)(content(Whitespace\" \
         \"))))(Tile((id \
         9e4a8e07-9192-4e13-8f55-af77d1d093fa)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         6916646c-9daf-4f18-b033-883275588b41)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         2e582469-acf8-47b4-827d-1c06747a1268)(content(Whitespace\"\\n\"))))(Tile((id \
         0aacc64b-14aa-42fc-80f2-f61c6215f543)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         70e79e9a-e4de-4e1b-b408-1f6abd54fc53)(content(Whitespace\" \
         \"))))(Tile((id \
         4777068f-4609-4140-86c8-2760f549b315)(label(ledger))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         14e71d76-13c5-45ab-89d5-76d8b9071e28)(content(Whitespace\" \
         \")))))((Secondary((id \
         12b87f73-6914-4b9c-bd07-1652e5486f05)(content(Whitespace\" \
         \"))))(Tile((id \
         17efc68f-7ee0-49ca-a96a-3c7361ed35c0)(label(run))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         a97d175b-810d-4369-86c2-ddce1a25ebe8)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         d8da67c7-c91b-41e0-9f66-c959e91bf683)(label(initModel))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         287546dc-0582-47d3-a010-2a23b6a67819)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         115d5ac9-01a5-4918-bf5f-6ae9466ea26a)(content(Whitespace\" \
         \"))))(Tile((id 587ee757-52ba-44cc-82c0-6b224b7c6be9)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         2a12a971-1022-4c12-a7cc-2a96f493019e)(content(Whitespace\"\\n\"))))(Tile((id \
         f2a58510-aedc-4157-879b-81ac85a2a661)(label(RecordHarvest))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         11faa01b-c2cc-4e9b-a904-fc7e356dbf6a)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         b978e5fe-b1a0-4be8-a603-6a05344b80e9)(label(h))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         3bf1e312-262a-4468-a73f-800a28c40298)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         01909932-b66f-441b-aaa2-55c989ed84ae)(content(Whitespace\"\\n\"))))(Tile((id \
         eac47435-5f17-409f-abe3-1dc217721db1)(label(PremiumSale))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         950f1465-0aaf-438a-a302-270047718632)(content(Whitespace\"\\n\"))))))))))))))(Secondary((id \
         f840e14d-890f-4931-a0a7-a1f16b4b56cc)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         b1238fda-07a7-4055-aa8a-b29d76904f99)(content(Whitespace\"\\n\"))))(Secondary((id \
         5f8329e5-b10c-4077-9057-e3e17cde7aae)(content(Comment\"# streakBonus \
         was 0, payout = 0 * 1 = 0 #\"))))(Secondary((id \
         eefa9bc4-7bef-4819-a8d4-762a25538d75)(content(Whitespace\"\\n\"))))(Tile((id \
         1ef211fe-cfa7-45e5-a596-48069ce1763f)(label(ledger))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         018b29c1-fa30-466e-aa29-7f8fb587cc5b)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         e4919442-b334-439b-a6c9-61ab19005b5b)(label(totalValue))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         08264f70-4a28-499f-8ec4-50f136e4d057)(content(Whitespace\" \
         \"))))(Tile((id \
         25e5b1f0-8e56-4b8c-8386-d8aaaeb7cdf7)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         6a54a01b-69a3-4db9-9e98-37d9b4be7c4d)(content(Whitespace\" \
         \"))))(Tile((id \
         59611a3e-38ee-452c-8237-5882fd42257f)(label(15))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         a86686ac-7079-418d-8f08-ac66384ae6b2)(content(Whitespace\"\\n\")))))))))(Tile((id \
         39f66bcc-b8ef-4fbd-8256-20ba1b2ee9aa)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         d718939a-c20d-488b-bebb-7bfae64b0ecf)(content(Whitespace\"\\n\"))))(Secondary((id \
         fd1de8b4-c913-47de-a564-0ed78c4e472b)(content(Whitespace\"\\n\"))))(Secondary((id \
         c9d9c279-8b0c-42c3-8278-ecb819ebf3f5)(content(Comment\"# Demo: \
         Premium sale harvest day #\"))))(Secondary((id \
         950e8e7c-cb43-470e-a342-c3d19950fa3a)(content(Whitespace\"\\n\"))))(Tile((id \
         0e803c36-e870-4bca-bc8f-1424e84be6f6)(label(run))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         4d5e8d91-e3fa-4af6-8655-a1b828bed173)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         fe412046-21c6-4ac8-97f9-633a4a77f6b3)(label(initModel))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         380a2165-1dee-43c3-bfa0-d7d2fa6cc9c9)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         d5654af8-7bf3-4ffe-a2f2-330954abe9b0)(content(Whitespace\" \
         \"))))(Tile((id f5a65c63-e192-4789-968f-592952b809a2)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         03dedab1-80a4-47fe-85c0-0b90ffc5e11a)(content(Whitespace\"\\n\"))))(Tile((id \
         45b60204-7e1d-4596-860d-c58557fc5928)(label(RecordHarvest))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         b593ccb2-0f7e-413c-ba2d-4010b2472a1e)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         cb343b07-255d-46fd-8262-2ad0ee943dce)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         065614e2-cf86-428a-b620-9bc1c9c02c6e)(label(crop))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         995b2df7-f1c6-4b12-8b9b-48c3208975f5)(content(Whitespace\" \
         \"))))(Tile((id \
         10ea15b3-ced4-481a-8370-f0f83c9ff956)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         8c558acd-26dc-46c1-8729-43aa10207b55)(content(Whitespace\" \
         \"))))(Tile((id \
         b688fb20-e522-45ec-9b6f-da6a63abf0ed)(label(Nightshade))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         2d7ce90b-8987-4c0b-ac8d-f63580df545e)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         d15293bb-c7a6-4678-954f-656c9b8c2e29)(content(Whitespace\" \
         \"))))(Tile((id \
         6e0c4269-c5a6-442c-a89c-6ca7ce906d6a)(label(quality))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         9b21ace4-340c-498d-86ec-d8de14d05fc6)(content(Whitespace\" \
         \"))))(Tile((id \
         2da85e49-0110-4ff0-a17d-fb5d4decbf1c)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         eae36f2f-8888-41c6-90ae-627863abd4d9)(content(Whitespace\" \
         \"))))(Tile((id \
         ac2ccec6-f038-426e-b2cc-5cc3c2dddfab)(label(Gold))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         c663d483-2558-456c-ade6-e179555cdff3)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         65648cf1-8231-4931-b552-d7e5b2ab7490)(content(Whitespace\" \
         \"))))(Tile((id \
         90b33a8c-9d5e-4fbb-9805-eceaccbde923)(label(quantity))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         7ab7dd67-67a1-4930-afab-a2710cc9fd93)(content(Whitespace\" \
         \"))))(Tile((id \
         b5b694bd-c5b6-4968-8115-b87b07341913)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         6d1a7d3b-1f30-4f56-9fc7-04652a4d2a67)(content(Whitespace\" \
         \"))))(Tile((id \
         09a1edf3-faa3-421d-b423-873ab1ac0d13)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))))))))))))(Tile((id \
         2731544e-2fe7-4bca-93b7-589da5a95d93)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         afc53742-d0e9-4186-93bd-12bc0dabe690)(content(Whitespace\"\\n\"))))(Tile((id \
         75d373cb-9718-442c-9bce-2ab68e86b81e)(label(RecordHarvest))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         c632cb60-9688-4d48-bd35-3da3b1f90315)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         ed285ede-4638-405b-8130-78dbdd460864)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         8e336d93-36bd-4f48-9b99-88d2d1586bec)(label(crop))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         4b3640ed-46b6-4e6c-9457-a644d5da47d3)(content(Whitespace\" \
         \"))))(Tile((id \
         a55d198a-b2a5-4888-a42f-eb33afe3daba)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         174713f5-03e8-424e-b622-7ceefa58fb56)(content(Whitespace\" \
         \"))))(Tile((id \
         0ffb583e-aadc-4211-8cd3-241710d1883e)(label(Starfruit))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         3d81c569-e3ef-4344-8987-db8e84da40d1)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         4a340dc6-ace7-45f7-9e8d-37095e673647)(content(Whitespace\" \
         \"))))(Tile((id \
         37b80bbf-5009-464f-815a-e2191c03455e)(label(quality))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         f4b3a559-d7af-4c2f-92c0-ab357a733fe9)(content(Whitespace\" \
         \"))))(Tile((id \
         03af9f0c-ea12-4dcf-b7f0-1275dd6aaebd)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         6828d033-af08-41c0-b51d-27e5aa5d21f7)(content(Whitespace\" \
         \"))))(Tile((id \
         4755e157-8b04-453c-a05f-18c19e1419d8)(label(Gold))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         2cf6f4ef-b7fe-4861-8fc3-4d3628e9c00a)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         3a2b07fd-5806-4d54-8450-b9bdd48408b6)(content(Whitespace\" \
         \"))))(Tile((id \
         9e671ee5-b242-4600-ae4a-38cc00695f9c)(label(quantity))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         6e4b45cc-a42e-4ab5-9b97-f694488a6369)(content(Whitespace\" \
         \"))))(Tile((id \
         b47b7218-bffa-457e-9ab3-b668c1461e8c)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         5c797b36-0c6d-490b-af1e-6a49912c14e5)(content(Whitespace\" \
         \"))))(Tile((id \
         167c6e68-297a-48ea-93c9-a36ea031d5b3)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))))))))))))(Tile((id \
         2614d26d-517a-4af6-86dc-e02e2541ba27)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         381342d1-20f2-40bd-9585-b1aa8ae1384f)(content(Whitespace\"\\n\"))))(Tile((id \
         9bb5e2d1-2ef2-4782-a4df-dd022ae7b547)(label(RecordHarvest))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         c97e1edc-5b2c-4182-b302-d823eb5a0360)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         d27285af-197b-4d9e-a7a8-c09920b44731)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         59274867-784a-40f1-a116-f1339fc108d4)(label(crop))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         d811ffb8-1b98-4170-be15-ae0b6124b5ed)(content(Whitespace\" \
         \"))))(Tile((id \
         412b3859-a332-4184-98d2-e520ac8af845)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         be8a10ca-c04a-44cd-b133-1a7a718abe8a)(content(Whitespace\" \
         \"))))(Tile((id \
         995ca39f-cc14-4238-8b3b-4a3b2e31b7f6)(label(Moonmelon))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         0713a46a-4601-46bf-8cf2-aa73e5bc326f)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         c52681cd-1926-46bb-983b-243c73142076)(content(Whitespace\" \
         \"))))(Tile((id \
         0574b43c-a0c3-446f-9ac6-68f5c0fe6ce9)(label(quality))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         4ec13d8a-0331-4e5d-9faf-356d74895915)(content(Whitespace\" \
         \"))))(Tile((id \
         58ca6119-1bbc-4ab9-a5fc-75d1f04f4dae)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         2b5bdb2c-bdc2-4fa5-9a51-669060c39d1f)(content(Whitespace\" \
         \"))))(Tile((id \
         3d81ef8d-e964-471d-964e-36afd18c8dae)(label(Gold))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         2aacc507-35c4-4c0d-b71a-c7a207011b61)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         3f4c3a64-66ed-46f3-acf2-33c082cbe7ff)(content(Whitespace\" \
         \"))))(Tile((id \
         193a9c26-c03b-46f2-90d5-643fbd88a59c)(label(quantity))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         1a6a8691-f5f5-49db-9445-875504850337)(content(Whitespace\" \
         \"))))(Tile((id \
         94e4da3f-3014-4a69-b819-f91188e8cc8e)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         aa0c1ad8-9cce-4eaf-81b7-625cbcfdd84a)(content(Whitespace\" \
         \"))))(Tile((id \
         323b8c2c-98d3-4183-9963-0cf4ebbdbe63)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))))))))))))(Tile((id \
         e5332ac9-c774-4889-b2a2-cb37cd27e0a8)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         62880a52-9880-49cd-b7d6-c86468e756e7)(content(Whitespace\"\\n\"))))(Tile((id \
         76524eb8-3dea-4ee7-b637-88d98b3b3f78)(label(PremiumSale))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         f5a24060-d1af-4fd4-b128-02b2ef519e69)(content(Whitespace\"\\n\"))))))))))))))(Secondary((id \
         fbfbdcb8-d49d-489d-ae51-01a5ae4daf1e)(content(Whitespace\"\\n\")))))";
      backup_text =
        "# HARVEST STREAK EXTENSION TASK                   #\n\
         #                                                  #\n\
         # The harvest ledger app tracks harvests and       #\n\
         # builds streak bonuses for consecutive same-      #\n\
         # quality harvests.                                #\n\
         #                                                  #\n\
         # YOUR TASK: Add a PremiumSale action that lets    #\n\
         # the farmer claim their streak bonus with a       #\n\
         # premium multiplier when the streak is strong.    #\n\
         #                                                  #\n\
         # You need to:                                     #\n\
         #   1. Add PremiumSale to the Action type          #\n\
         #   2. Write a premiumMultiplier helper function   #\n\
         #   3. Handle PremiumSale in the update function   #\n\
         #                                                  #\n\
         # Look at how ClaimBonus is implemented for        #\n\
         # guidance - PremiumSale is similar but applies    #\n\
         # a multiplier to the payout.                      #\n\
         #                                                  #\n\
         # Tip: Use auto-probe on premiumMultiplier to see  #\n\
         # when the threshold fires.                        #\n\n\
         # Quality tiers from the moonlit fields #\n\
         type Quality =\n\
         + Bronze      # Common harvest, basic value #\n\
         + Silver      # Good quality, moderate bonus #\n\
         + Gold        # Excellent harvest, high value #\n\
         + Starlight   # Legendary, blessed by the moon #\n\
         in\n\n\
         # Crops that grow under the night sky #\n\
         type Crop =\n\
         + Moonmelon   # Glows faintly, sweet taste #\n\
         + Starfruit   # Shaped like stars, tangy #\n\
         + Nightshade  # Purple bloom, magical properties #\n\
         + Duskwheat   # Golden stalks, hearty grain #\n\
         + Glowpumpkin # Orange and luminescent #\n\
         in\n\n\
         # A single harvest from the garden #\n\
         type Harvest = (\n\
         crop = Crop,\n\
         quality = Quality,\n\
         quantity = Int\n\
         ) in\n\n\
         # The harvest ledger tracks all harvests and bonuses #\n\
         type Model = (\n\
         harvests = [Harvest],\n\
         totalValue = Int,\n\
         streakBonus = Int,\n\
         lastQuality = Quality\n\
         ) in\n\n\
         # Actions the farmer can take #\n\
         type Action =\n\
         + RecordHarvest(Harvest)  # Log a new harvest #\n\
         + ClaimBonus              # Collect accumulated streak bonus #\n\
         + CloseDay                # End the harvest day, reset streaks #\n\
         # TODO: Add PremiumSale here #\n\
         in\n\n\
         # Calculate base value of a crop #\n\
         let cropValue : Crop -> Int =\n\
         fun c ->\n\
         case c\n\
         | Moonmelon => 15\n\
         | Starfruit => 20\n\
         | Nightshade => 20\n\
         | Duskwheat => 10\n\
         | Glowpumpkin => 12\n\
         end\n\
         in\n\n\
         # Quality multiplier for harvest value #\n\
         let qualityMultiplier : Quality -> Int =\n\
         fun q ->\n\
         case q\n\
         | Bronze => 1\n\
         | Silver => 2\n\
         | Gold => 3\n\
         | Starlight => 5\n\
         end\n\
         in\n\n\
         # Calculate the value of a single harvest #\n\
         let harvestValue : Harvest -> Int =\n\
         fun h ->\n\
         cropValue(h.crop) * qualityMultiplier(h.quality) * h.quantity\n\
         in\n\n\
         # Initial empty ledger #\n\
         let initModel : Model = (\n\
         harvests = [],\n\
         totalValue = 0,\n\
         streakBonus = 0,\n\
         lastQuality = Bronze\n\
         ) in\n\n\
         # Process a harvest action and update the ledger #\n\
         let processHarvest : (Model, Harvest) -> Model =\n\
         fun (ledger, h) ->\n\
         let value = harvestValue(h) in\n\
         # Check if this harvest continues the quality streak #\n\
         # First harvest never continues a streak (no previous harvest) #\n\
         # Compare current quality with the PREVIOUS lastQuality #\n\
         let isFirst = length(ledger.harvests) == 0 in\n\
         let continues = !isFirst && h.quality == ledger.lastQuality in\n\
         # Now update lastQuality to current harvest #\n\
         let newLast = h.quality in\n\
         let newStreak =\n\
         if continues\n\
         then ledger.streakBonus + 5\n\
         else 0\n\
         in\n\
         (\n\
         harvests = h :: ledger.harvests,\n\
         totalValue = ledger.totalValue + value + newStreak,\n\
         streakBonus = newStreak,\n\
         lastQuality = newLast\n\
         )\n\
         in\n\n\
         # Claim the streak bonus and reset it #\n\
         let claimBonus : Model -> Model =\n\
         fun ledger ->\n\
         (\n\
         harvests = ledger.harvests,\n\
         totalValue = ledger.totalValue + ledger.streakBonus,\n\
         streakBonus = 0,\n\
         lastQuality = ledger.lastQuality\n\
         )\n\
         in\n\n\
         # Close the harvest day - reset streak tracking #\n\
         let closeDay : Model -> Model =\n\
         fun ledger ->\n\
         (\n\
         harvests = ledger.harvests,\n\
         totalValue = ledger.totalValue,\n\
         streakBonus = 0,\n\
         lastQuality = Bronze\n\
         )\n\
         in\n\n\
         # TODO: Add premiumMultiplier helper here                      #\n\
         # It takes a streakBonus (Int) and returns the multiplier:     #\n\
         #   - Return 2 if the streak bonus is >= 10 (strong streak)    #\n\
         #   - Return 1 otherwise                                      #\n\
         # Hint: This is a simple if/then/else on the streakBonus.     #\n\n\
         let premiumMultiplier : Int -> Int =\n\
         fun streakBonus ->\n\
         ?\n\n\n\n\
         in\n\n\
         # Main update function - dispatch actions #\n\
         let update : (Model, Action) -> Model =\n\
         fun (ledger, action) ->\n\
         case action\n\
         | RecordHarvest(h) => processHarvest(ledger, h)\n\
         | ClaimBonus => claimBonus(ledger)\n\
         | CloseDay => closeDay(ledger)\n\
         # TODO: Add PremiumSale case here                          #\n\
         # Hint: Compute payout = streakBonus * premiumMultiplier,  #\n\
         # add payout to totalValue, and reset streakBonus to 0.    #\n\
         # Keep harvests and lastQuality unchanged.                 #\n\
         end\n\
         in\n\n\
         # Run multiple actions in sequence #\n\
         let run : (Model, [Action]) -> Model =\n\
         fun (ledger, actions) ->\n\
         fold_left(actions, update, ledger)\n\
         in\n\n\
         # ===== TESTS ===== #\n\n\
         # Regression: basic harvest recording still works #\n\
         hint \"recording harvest adds to total value\"\n\
         test\n\
         let h = (crop = Moonmelon, quality = Bronze, quantity = 2) in\n\
         let ledger = update(initModel, RecordHarvest(h)) in\n\
         # Moonmelon(15) * Bronze(1) * 2 = 30, no streak bonus on first #\n\
         ledger.totalValue == 30\n\
         end;\n\n\
         # Regression: streak bonus still works #\n\
         hint \"same quality builds streak\"\n\
         test\n\
         let h1 = (crop = Moonmelon, quality = Gold, quantity = 1) in\n\
         let h2 = (crop = Starfruit, quality = Gold, quantity = 1) in\n\
         let ledger = run(initModel, [RecordHarvest(h1), RecordHarvest(h2)]) in\n\
         ledger.streakBonus == 5\n\
         end;\n\n\
         # Regression: claim bonus still works #\n\
         hint \"claiming bonus adds to total and resets streak\"\n\
         test\n\
         let h1 = (crop = Moonmelon, quality = Gold, quantity = 1) in\n\
         let h2 = (crop = Starfruit, quality = Gold, quantity = 1) in\n\
         let ledger = run(initModel, [\n\
         RecordHarvest(h1),\n\
         RecordHarvest(h2),\n\
         ClaimBonus\n\
         ]) in\n\
         ledger.streakBonus == 0\n\
         end;\n\n\
         # PremiumSale: low streak gives 1x multiplier #\n\
         hint \"PremiumSale with low streak uses 1x multiplier\"\n\
         test\n\
         # Two same-quality harvests build streakBonus to 5 #\n\
         let h1 = (crop = Moonmelon, quality = Silver, quantity = 1) in\n\
         let h2 = (crop = Starfruit, quality = Silver, quantity = 1) in\n\
         let ledger = run(initModel, [\n\
         RecordHarvest(h1),\n\
         RecordHarvest(h2),\n\
         PremiumSale\n\
         ]) in\n\
         # streakBonus was 5, multiplier = 1, payout = 5 #\n\
         # h1: 15*2*1 = 30, h2: 20*2*1 + 5 = 45, PremiumSale: +5 #\n\
         ledger.totalValue == 30 + 45 + 5\n\
         end;\n\n\
         # PremiumSale: high streak gives 2x multiplier #\n\
         hint \"PremiumSale with high streak uses 2x multiplier\"\n\
         test\n\
         # Three same-quality harvests build streakBonus to 10 #\n\
         let h1 = (crop = Moonmelon, quality = Gold, quantity = 1) in\n\
         let h2 = (crop = Starfruit, quality = Gold, quantity = 1) in\n\
         let h3 = (crop = Nightshade, quality = Gold, quantity = 1) in\n\
         let ledger = run(initModel, [\n\
         RecordHarvest(h1),\n\
         RecordHarvest(h2),\n\
         RecordHarvest(h3),\n\
         PremiumSale\n\
         ]) in\n\
         # streakBonus was 10, multiplier = 2, payout = 20 #\n\
         # h1: 15*3=45, h2: 20*3+5=65, h3: 20*3+10=70, PremiumSale: +20 #\n\
         ledger.totalValue == 45 + 65 + 70 + 20\n\
         end;\n\n\
         # PremiumSale resets streak after claiming #\n\
         hint \"PremiumSale resets streak to zero\"\n\
         test\n\
         let h1 = (crop = Moonmelon, quality = Gold, quantity = 1) in\n\
         let h2 = (crop = Starfruit, quality = Gold, quantity = 1) in\n\
         let h3 = (crop = Nightshade, quality = Gold, quantity = 1) in\n\
         let ledger = run(initModel, [\n\
         RecordHarvest(h1),\n\
         RecordHarvest(h2),\n\
         RecordHarvest(h3),\n\
         PremiumSale\n\
         ]) in\n\
         ledger.streakBonus == 0\n\
         end;\n\n\
         # PremiumSale with no streak gives zero payout #\n\
         hint \"PremiumSale with zero streak adds nothing\"\n\
         test\n\
         let h = (crop = Moonmelon, quality = Bronze, quantity = 1) in\n\
         let ledger = run(initModel, [\n\
         RecordHarvest(h),\n\
         PremiumSale\n\
         ]) in\n\
         # streakBonus was 0, payout = 0 * 1 = 0 #\n\
         ledger.totalValue == 15\n\
         end;\n\n\
         # Demo: Premium sale harvest day #\n\
         run(initModel, [\n\
         RecordHarvest((crop = Nightshade, quality = Gold, quantity = 1)),\n\
         RecordHarvest((crop = Starfruit, quality = Gold, quantity = 1)),\n\
         RecordHarvest((crop = Moonmelon, quality = Gold, quantity = 1)),\n\
         PremiumSale\n\
         ])\n";
      refractors = "()";
    } )
