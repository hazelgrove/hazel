let out : string * Haz3lcore.PersistentSegment.t =
  ( "Study / tutorial / 02-functions-and-cursor",
    {
      segment =
        "((Secondary((id \
         35da50ff-c1b5-42cd-8490-fb918ddda758)(content(Comment\"# PROBES \
         TUTORIAL - PART 2: FUNCTIONS AND THE DYNAMIC CURSOR \
         #\"))))(Secondary((id \
         6248c61e-30e3-4079-9d88-42893446e94b)(content(Whitespace\"\\n\"))))(Secondary((id \
         418bc588-870d-44e6-8f33-6cafda04b2aa)(content(Whitespace\"\\n\"))))(Secondary((id \
         5e1aa3b0-2c5c-4eb7-a83e-894bf7b2802f)(content(Comment\"# When a \
         function is called multiple times, each call #\"))))(Secondary((id \
         c9d5fadc-077b-47b3-b322-4b1ae24413ab)(content(Whitespace\"\\n\"))))(Secondary((id \
         b44a70b5-241a-487a-90c7-c62371701fab)(content(Comment\"# generates \
         its own sample. Let's see what that looks like! #\"))))(Secondary((id \
         697df1c4-aee9-41f2-b56f-124596c112dd)(content(Whitespace\"\\n\"))))(Secondary((id \
         bb7a5491-4929-4812-b2c5-e7f5e05c5eee)(content(Whitespace\"\\n\"))))(Tile((id \
         f7f4dd20-4c7a-4a1b-9df5-021269b606ae)(label(type = in))(mold((out \
         Exp)(in_(TPat Typ))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         a2471d6a-8ee2-4599-857d-0d36877b9524)(content(Whitespace\" \
         \"))))(Tile((id \
         98674f6d-7d83-4d51-ba0f-62bb1f41b1f9)(label(MoonPhase))(mold((out \
         TPat)(in_())(nibs(((shape Convex)(sort TPat))((shape Convex)(sort \
         TPat))))))(shards(0))(children())))(Secondary((id \
         91ca1cbf-c3a2-4782-8f7d-23d0bb73ea1c)(content(Whitespace\" \
         \")))))((Secondary((id \
         01c1068e-7046-4127-b3e2-eef8ba354fea)(content(Whitespace\" \
         \"))))(Tile((id \
         15aa550e-6e3b-4a06-99ef-5ed7468e927a)(label(New))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         d8919a2a-a943-4ac3-b7bc-4eff65ec65a9)(content(Whitespace\" \
         \"))))(Tile((id \
         c229daa0-09b5-45f8-84c7-607695371ac2)(label(+))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 12))(sort Typ))((shape(Concave \
         12))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         1b930093-bcb2-40bc-8f15-82edc9b2154e)(content(Whitespace\" \
         \"))))(Tile((id \
         b1ce3da4-cf1e-4fa9-b8df-244cda7c9fb2)(label(Waxing))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         93de81d5-93c3-481e-832f-d4c164855bd9)(content(Whitespace\" \
         \"))))(Tile((id \
         09d7ed1e-4b91-4732-ad36-b46cf46710a7)(label(+))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 12))(sort Typ))((shape(Concave \
         12))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         f64cf608-f831-4dcc-8c97-f085f63c93bd)(content(Whitespace\" \
         \"))))(Tile((id \
         4c80ebca-ebd7-46cb-a9ca-5d96a21df378)(label(Full))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         d11e81fd-b454-466e-8b0a-4b0f7e721644)(content(Whitespace\" \
         \"))))(Tile((id \
         cba7edd2-edbe-49be-8904-85d0acbb079c)(label(+))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 12))(sort Typ))((shape(Concave \
         12))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         79000fd3-7a3a-45c2-bb58-576bac1b10b2)(content(Whitespace\" \
         \"))))(Tile((id \
         f28f273e-d3a9-4b0d-a206-e251b0ebefe8)(label(Waning))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         f26401a4-a483-44ba-ad40-ed66023d63cd)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         f9aa8473-17b0-426e-ae83-3cd3424b69d9)(content(Whitespace\"\\n\"))))(Secondary((id \
         44b98179-55d9-4fb3-87ba-336c6ef01360)(content(Whitespace\"\\n\"))))(Secondary((id \
         8fa2e2e3-cd18-42c7-a3b3-049562dd6fac)(content(Comment\"# Hazel has no \
         special function definition syntax. #\"))))(Secondary((id \
         f36d79c2-c04f-4cb7-8ae4-bbb1c132055a)(content(Whitespace\"\\n\"))))(Secondary((id \
         467673b1-8d88-4f9f-8a9c-0704eac265bc)(content(Comment\"# We use \
         regular let definitions to define function literals, \
         #\"))))(Secondary((id \
         9dbf3487-2d94-434d-b590-7d31dcec7cd0)(content(Whitespace\"\\n\"))))(Secondary((id \
         00c30f90-9840-44f6-9307-8106b74eba50)(content(Comment\"# using the \
         syntax `fun <pattern> -> <body>`. #\"))))(Secondary((id \
         c09b0883-ecd1-436a-8cfb-fe12d6ad756b)(content(Whitespace\"\\n\"))))(Secondary((id \
         9318c9f6-5ec2-4a30-a70e-6d758f8c28a1)(content(Whitespace\"\\n\"))))(Secondary((id \
         a7b68841-57e9-4555-853b-23205f9d6e5d)(content(Comment\"# TRY THIS: \
         Add a probe to the `multiplier` variable inside #\"))))(Secondary((id \
         0ae9959b-cd00-4eb0-ac12-8bd7689bb859)(content(Whitespace\"\\n\"))))(Secondary((id \
         3988eb7a-00f3-410b-9d1f-4e48803d66eb)(content(Comment\"# the function \
         `watering_amount` below. When you click on the #\"))))(Secondary((id \
         2f7a19bf-e842-40a8-bd7b-58fa7d05857e)(content(Whitespace\"\\n\"))))(Secondary((id \
         075abffe-dcc8-46e1-8841-007c30240b2a)(content(Comment\"# sample, \
         notice the arrows that appear to the left. Click on \
         #\"))))(Secondary((id \
         f59ed341-931d-47ee-8e99-eae85a4038fc)(content(Whitespace\"\\n\"))))(Secondary((id \
         7784a7c3-9fb0-455f-a71c-a00f48ada0a6)(content(Comment\"# these \
         arrows, or use the left/right arrow keys, to navigate \
         #\"))))(Secondary((id \
         1fa91cfe-43f3-494c-92eb-4207792e9ae0)(content(Whitespace\"\\n\"))))(Secondary((id \
         d74c0287-2008-4b3a-98f4-46b3f2ecde74)(content(Comment\"# between the \
         three different samples collected. #\"))))(Secondary((id \
         dc6da689-1093-48fd-bf7c-a67185a6104b)(content(Whitespace\"\\n\"))))(Secondary((id \
         9285167c-c683-428f-8988-91e41dbfe32a)(content(Whitespace\"\\n\"))))(Tile((id \
         448beacd-d9a9-4bb9-8823-18dda8a97b15)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         99ee95fc-54fe-458b-b834-ee531672316f)(content(Whitespace\" \
         \"))))(Tile((id \
         a6f04508-cd3f-4998-a5a5-6e2ab51d5c30)(label(watering_amount))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         b771be58-619e-4faa-9198-8e76434b137c)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 24))(sort Pat))((shape(Concave \
         24))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         619fe4c8-f261-430c-b484-186f6613cc65)(content(Whitespace\" \
         \"))))(Tile((id \
         f4ec32e5-8413-4f20-8828-21997971c9be)(label(\"(\"\")\"))(mold((out \
         Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0 1))(children(((Tile((id \
         01866037-fbae-46ff-93a6-ee1b7fbbaff4)(label(Int))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         982b49ba-ecf1-4efa-8a7d-374416bcf1f1)(label(,))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 44))(sort Typ))((shape(Concave \
         44))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         29bf90d5-2d8c-4912-9082-aa8fd18e312d)(content(Whitespace\" \
         \"))))(Tile((id \
         5a071620-1b01-47d2-9173-e9d619a0e4a9)(label(MoonPhase))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children()))))))))(Secondary((id \
         aa3c1c5a-6040-4cf3-b961-2a0fb819b5be)(content(Whitespace\" \
         \"))))(Tile((id \
         09884659-0910-4f48-b357-6df1b29e2e9b)(label(->))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 13))(sort Typ))((shape(Concave \
         13))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         782d3d4f-c41d-4ac2-ae6e-ac98e739f6c7)(content(Whitespace\" \
         \"))))(Tile((id \
         efff8df3-4134-4ef1-848d-c70c8a1656fe)(label(Int))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         32d6a52c-6d0d-4cbb-8d8a-455cbfc1e60a)(content(Whitespace\" \
         \")))))((Secondary((id \
         52e2920c-9247-4a2c-b96a-92b62293e1b9)(content(Whitespace\"\\n\"))))(Tile((id \
         125f420f-7ed2-4795-a055-bc6cc8716708)(label(fun ->))(mold((out \
         Exp)(in_(Pat))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         37))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         2b8597a9-f8ea-4e24-a18f-209b207bb534)(content(Whitespace\" \
         \"))))(Tile((id \
         9f54b137-f46c-4ae0-b216-22520562468b)(label(\"(\"\")\"))(mold((out \
         Pat)(in_(Pat))(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0 1))(children(((Tile((id \
         7c0181bf-e9d0-4923-9ba5-a93705eb90a2)(label(base_ml))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         ed888365-e7f1-4f6e-bf18-1edd5d258136)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 44))(sort Pat))((shape(Concave \
         44))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         3963196d-9cd1-4185-9dd0-7b116453b6b4)(content(Whitespace\" \
         \"))))(Tile((id \
         fa1142a4-7607-4597-be21-649aa94827f6)(label(phase))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children()))))))))(Secondary((id \
         a6b149f6-e562-42a0-a1ad-9df03a91f116)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         1db474fa-8a01-4998-973a-206635706b1a)(content(Whitespace\"\\n\"))))(Tile((id \
         c0ab5a4d-5444-4b09-8f0d-1db2c41353bc)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         f2817fd9-7778-4e80-a36d-1abdfba1ddb7)(content(Whitespace\" \
         \"))))(Tile((id \
         4a31b90d-baac-4da1-9d53-18f8e8aee8a0)(label(multiplier))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         c6615bad-c7cd-41ee-8126-a5fd153b37e1)(content(Whitespace\" \
         \")))))((Secondary((id \
         c05b3dd2-5156-48ca-927c-be7d809f968b)(content(Whitespace\"\\n\"))))(Tile((id \
         8c1f48fc-e179-43c7-807d-5743777d4b55)(label(case end))(mold((out \
         Exp)(in_(Rul))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         023c5cc5-3787-4ab1-bb60-e9cb031206e9)(content(Whitespace\" \
         \"))))(Tile((id \
         844a27c5-663d-4068-9028-60e6c0cfc11e)(label(phase))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         dcfbf733-0353-4f7a-af0e-c620ae0a5cf5)(content(Whitespace\"\\n\"))))(Tile((id \
         dae78cfc-21b8-4de1-aa88-30f9e14792ff)(label(| =>))(mold((out \
         Rul)(in_(Pat))(nibs(((shape(Concave 46))(sort Exp))((shape(Concave \
         46))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         2ad4aeaa-182f-47fc-97a8-e7298d208af9)(content(Whitespace\" \
         \"))))(Tile((id \
         653a7434-8811-45dc-ad4d-566c5e0ae0fd)(label(New))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         4ef8cb82-7f25-4f87-93b7-c818fe188f94)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         1b76c6bc-b95d-454e-8358-b4bf5a7913b0)(content(Whitespace\" \
         \"))))(Tile((id \
         f85719ab-86a4-4381-ab65-2d9e4f7cae46)(label(1.2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         d139ccc2-c680-4de2-9c8c-f993ded2a787)(content(Whitespace\"\\n\"))))(Tile((id \
         862216ef-5fc2-4a7f-83ec-578bc24cd389)(label(| =>))(mold((out \
         Rul)(in_(Pat))(nibs(((shape(Concave 46))(sort Exp))((shape(Concave \
         46))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         3968fb98-3ad9-47d3-b92e-6ffb8a9029ac)(content(Whitespace\" \
         \"))))(Tile((id \
         05931007-047b-4143-94e7-eb68ba8dfe4f)(label(Full))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         f92b3d5e-fc68-45a5-b121-8d8270ea5263)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         a426137f-9587-4233-a9e9-98e1c1d877a5)(content(Whitespace\" \
         \"))))(Tile((id \
         6d19ad31-e525-4092-9007-cced7fc930d3)(label(0.88))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         6d1a4f21-caf7-4ee3-941f-55c9ba5f7878)(content(Whitespace\"\\n\"))))(Tile((id \
         a2bbf160-54d2-41af-ba73-51d244ee1678)(label(| =>))(mold((out \
         Rul)(in_(Pat))(nibs(((shape(Concave 46))(sort Exp))((shape(Concave \
         46))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         6c07971f-9a61-49be-9d67-d231247291ee)(content(Whitespace\" \
         \"))))(Tile((id \
         95098a65-b93b-4a67-98e2-448e0b09b6c5)(label(Waxing))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         dffd91f8-1dd9-4c71-93d0-7c4c50459d1f)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         d170de04-d4b6-486b-9de5-09c64a09bff2)(content(Whitespace\" \
         \"))))(Tile((id \
         51bc6c5f-43bb-4891-bf32-fbe7774fd26a)(label(1.1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         08f3f82e-20d1-4d8e-b8c9-eabf7eca4536)(content(Whitespace\"\\n\"))))(Tile((id \
         bfece538-dbe0-4411-b98f-f0025acc8a8e)(label(| =>))(mold((out \
         Rul)(in_(Pat))(nibs(((shape(Concave 46))(sort Exp))((shape(Concave \
         46))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         a531a7af-147e-46ff-b995-cf4d7d6947bc)(content(Whitespace\" \
         \"))))(Tile((id \
         526df02a-9009-465a-988f-16c3999bba54)(label(Waning))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         c6bb16bb-7d35-4509-921e-e95ea7e62c0e)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         68b675e1-07ec-4cf4-a8e6-8f3ceb4c816b)(content(Whitespace\" \
         \"))))(Tile((id \
         f101ec7b-cd8c-41b2-83de-40a348813fa7)(label(0.95))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         b02b3cfe-2871-4250-9ca0-d1f8164d75d3)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         36d971e3-384e-477e-914a-0cdfeb109851)(content(Whitespace\" \
         \"))))(Secondary((id \
         68fce0a5-fe74-4678-ba96-2ba9cc8b3258)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         ef80f067-3e00-44b0-8683-0743b0a007c3)(content(Whitespace\" \
         \"))))(Tile((id \
         09ecf328-6008-4fcf-b6a3-2203292d6be2)(label(int_of_float))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         17215b78-eaaa-404a-8f68-b23805db1fed)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         06458b11-c17b-4556-aded-45c413cef360)(label(float_of_int))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         d72015e9-a7c9-4da2-81a6-0be278fa69c4)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         ab7acc55-5247-4d34-8408-835f2a6031ce)(label(base_ml))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         d35a9686-1ebc-43f2-bbff-b67958a5c7d2)(content(Whitespace\" \
         \"))))(Tile((id \
         d111f3bb-b940-446e-808b-ce75c845b790)(label(*.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 27))(sort Exp))((shape(Concave \
         27))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         0b571980-ec3a-4391-b7a9-d8f5bd58376a)(content(Whitespace\" \
         \"))))(Tile((id \
         ac0bedfd-0842-424b-92cf-daad9effa30e)(label(multiplier))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         22561581-f5c5-41ff-8899-8eb52b4acb7a)(content(Whitespace\"\\n\"))))(Secondary((id \
         dd0164ad-3910-48e5-a0c3-b855309e5e01)(content(Comment\"# Above: Hazel \
         uses C-style Function application syntax #\"))))(Secondary((id \
         f7dcb2da-5b16-4278-87c7-258a38288b9d)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         b8aff2bf-14c8-4b2a-bbbc-fdcefca76546)(content(Whitespace\"\\n\"))))(Secondary((id \
         400f9813-6d1b-41b0-8d66-b13165f66eab)(content(Whitespace\"\\n\"))))(Secondary((id \
         0e72f6b7-54b8-4553-941b-1145951a5fde)(content(Comment\"# Now click \
         the samples for the 3 calls to `watering_amount` below. \
         #\"))))(Secondary((id \
         aa3a3204-4922-4861-b1ce-55557009fb46)(content(Whitespace\"\\n\"))))(Secondary((id \
         d8644d34-7753-4086-9d3b-198390a2f2e5)(content(Comment\"# Notice the \
         sample for 'multiplier' above changes to /align/ with \
         #\"))))(Secondary((id \
         a16600e8-a05b-4fd9-ae72-5fa93c02dc26)(content(Whitespace\"\\n\"))))(Secondary((id \
         a01b39d2-fb47-4027-a8a9-fc56974947a6)(content(Comment\"# the selected \
         call! We call this behavior the 'dynamic cursor', \
         #\"))))(Secondary((id \
         1b4fe4fa-fa12-4745-986b-7ffa45f0fbd6)(content(Whitespace\"\\n\"))))(Secondary((id \
         dd3b5596-5936-4563-81f5-814300d68b1d)(content(Comment\"# which aligns \
         probe samples to a particular step in an execution. \
         #\"))))(Secondary((id \
         743cd455-5416-4246-b301-72ba005722d8)(content(Whitespace\"\\n\"))))(Secondary((id \
         c1b49c10-023a-437a-a172-621fe5565e7d)(content(Whitespace\"\\n\"))))(Tile((id \
         87632d1a-eb5c-4411-9406-e136f8500b40)(label(watering_amount))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         1bd0ed0d-f3e2-432d-a615-523ba5bf72b2)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         45376b14-45c9-468b-a59e-2a64d1e3cf8b)(label(250))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         83d52abf-ff56-4947-8828-8e20edd4e8b7)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         7a8e2742-fc76-46fa-8aa4-ff9e4185af70)(content(Whitespace\" \
         \"))))(Tile((id \
         7f966cd6-3ef2-4d63-8a1b-7c5612ee2313)(label(Full))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         07e53fde-c196-4fed-bcab-8075e7fb350f)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         0409e43b-a399-4c6a-b762-09e1a9d7825b)(content(Whitespace\"\\n\"))))(Tile((id \
         ef363610-bb18-4f10-8a4e-474404dcaa17)(label(watering_amount))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         343be700-701f-4ff5-ab4c-70aa36f1766a)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         67a1d1b3-864e-4a8e-910f-147f26c6bb88)(label(50))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         15bcd860-4c8d-4016-b04a-4bd46c10cf56)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         f6f75671-a201-469f-9235-5c790fa507de)(content(Whitespace\" \
         \"))))(Tile((id \
         d99e0277-019a-4a19-ad8c-378144f4aa42)(label(New))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         1ead70f2-b99f-4412-a603-c4bcf3d3696f)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         45f22f1f-56d8-489f-9eb7-5621ba23c03f)(content(Whitespace\"\\n\"))))(Tile((id \
         34512259-af7d-446a-8bb6-c451c3fa90cf)(label(watering_amount))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         76731098-31c8-42ce-8c66-57aac7b3b9b3)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         d9c4e05c-b9b0-4752-b6d7-4aebad0b1342)(label(180))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         ec70f5b8-c27f-47ba-8152-48ddc954ab48)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         8c28d082-bb8a-4cc3-8120-a772ce85dde2)(content(Whitespace\" \
         \"))))(Tile((id \
         8f5fe915-17ab-4561-a509-6aeca88f3102)(label(Waning))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         21259708-6796-4c12-94f1-19c259dd6c2b)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         fb28a781-2e89-4b69-993d-92e110f3343a)(content(Whitespace\"\\n\"))))(Secondary((id \
         a2c7150f-648a-46be-9dfa-70b81eb6833a)(content(Whitespace\"\\n\"))))(Secondary((id \
         17fed030-94fd-4ca2-8874-98a13d8069d2)(content(Comment\"# Below is the \
         same function as above, this time with many probes. \
         #\"))))(Secondary((id \
         826c8d0c-e4c6-4591-8cbf-49de0f796936)(content(Whitespace\"\\n\"))))(Secondary((id \
         41dbcdd6-07bc-4e8b-9a42-e561f780d46e)(content(Comment\"# Select the \
         `multiplier` sample and use the arrow keys to move \
         #\"))))(Secondary((id \
         6c2f6f53-2e66-4e25-a13a-8e870365d9b8)(content(Whitespace\"\\n\"))))(Secondary((id \
         94a0dccd-fe58-433f-a7b5-d4006713ac89)(content(Comment\"# through the \
         different values. Notice how this time, there are two \
         #\"))))(Secondary((id \
         fb2fa306-7358-410d-bed2-a629e69ca8b0)(content(Whitespace\"\\n\"))))(Secondary((id \
         f5f558e4-eb7b-4a70-ab1b-57250f60c05c)(content(Comment\"# different \
         symbols next to the branches with no samples; \\226\\136\\133 from \
         #\"))))(Secondary((id \
         bd45cc5d-cc45-4e66-9def-39427b201576)(content(Whitespace\"\\n\"))))(Secondary((id \
         471cad6d-13a2-4d92-a770-5ac77d75158f)(content(Comment\"# before on \
         `Waxing`, which means never evaluated, and a new symbol \
         #\"))))(Secondary((id \
         5fbed940-39fd-4374-928e-b7c0cea71c26)(content(Whitespace\"\\n\"))))(Secondary((id \
         9da1c331-6606-4e3c-bcee-397bdaa078ce)(content(Comment\"# \
         \\226\\138\\150, which means there are samples, but they are not \
         aligned to the #\"))))(Secondary((id \
         5a1c8bf4-b098-4583-8367-6ad396a9666e)(content(Whitespace\"\\n\"))))(Secondary((id \
         d12baf22-baba-4600-b3a1-195a632cb3a7)(content(Comment\"# dynamic \
         cursor (because of the `multiplier` sample you selected). \
         #\"))))(Secondary((id \
         1c02a398-622c-4971-a406-118b3da6bf10)(content(Whitespace\"\\n\"))))(Secondary((id \
         8be7c64f-e85e-4713-942c-086ad59c0bc7)(content(Comment\"# Click on any \
         \\226\\138\\150 to align the dynamic cursor to that branch. \
         #\"))))(Secondary((id \
         53e6fc58-1c0f-4d84-badd-e0022b9bc752)(content(Whitespace\"\\n\"))))(Secondary((id \
         c60fbc8e-3b4f-4c71-9b62-a6fe382410e6)(content(Whitespace\"\\n\"))))(Tile((id \
         f049e731-babe-4c48-8549-44225db6ead2)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         29471ad1-f01b-493d-92c5-6c1f10828ae9)(content(Whitespace\" \
         \"))))(Tile((id \
         f6551378-40f8-4c4f-b130-0b10e98d00e5)(label(watering_amount))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         98c97ea4-6de9-48aa-8c9e-9fdc0ece6ca7)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 24))(sort Pat))((shape(Concave \
         24))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         abc9e3bd-f038-4686-8e83-54d1ab534d7e)(content(Whitespace\" \
         \"))))(Tile((id \
         2fe0bd18-0aa0-417e-9ca5-f0fea937c771)(label(\"(\"\")\"))(mold((out \
         Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0 1))(children(((Tile((id \
         af036410-bfe3-4418-a00f-8755685f7297)(label(Int))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         d1383a32-c196-4585-9b97-6a9fab4001d7)(label(,))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 44))(sort Typ))((shape(Concave \
         44))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         6327cc7b-ecd0-453b-865b-c92b714717d0)(content(Whitespace\" \
         \"))))(Tile((id \
         1cb501af-42b5-442f-a562-c32e13c376d8)(label(MoonPhase))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children()))))))))(Secondary((id \
         c218e302-5b87-4102-b131-af91ef77f585)(content(Whitespace\" \
         \"))))(Tile((id \
         ac255301-1a1d-48b1-92c6-c374ea3402c3)(label(->))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 13))(sort Typ))((shape(Concave \
         13))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         7d7c6989-6bad-4603-a121-58364cf397fe)(content(Whitespace\" \
         \"))))(Tile((id \
         140535c2-93ed-4752-99a5-a78978dde682)(label(Int))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         40a01169-ebf0-4ea5-9d47-2d45f6a0fc3e)(content(Whitespace\" \
         \")))))((Secondary((id \
         6550b4c8-b716-4a19-8a31-c8e074fa2971)(content(Whitespace\"\\n\"))))(Tile((id \
         e63e03a0-0f6b-4201-b987-0970611e77d3)(label(fun ->))(mold((out \
         Exp)(in_(Pat))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         37))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         722ddcd4-138c-4234-b8a8-88ab97b027d8)(content(Whitespace\" \
         \"))))(Tile((id \
         026e1409-e52a-459c-b652-e7f8f604b35b)(label(\"(\"\")\"))(mold((out \
         Pat)(in_(Pat))(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0 1))(children(((Tile((id \
         b19cfc64-1c9c-4c78-8f77-f09b883bb876)(label(base_ml))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         8aba16cc-a955-4275-833e-646dfb27eada)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 44))(sort Pat))((shape(Concave \
         44))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         9c151c17-9241-4479-a6fb-e5cb07eb974b)(content(Whitespace\" \
         \"))))(Tile((id \
         8bbe45c2-9dc1-4f43-88b5-a0c8483237b6)(label(phase))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children()))))))))(Secondary((id \
         c8885e52-97f1-4066-890f-5a9d589aac35)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         23c12137-71dd-44ff-80c0-308ea583539c)(content(Whitespace\"\\n\"))))(Tile((id \
         45e6e5fa-c010-4ab8-bf49-65f6f98e931b)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         4df297bc-ee2c-4da6-82f1-3dec7979783b)(content(Whitespace\" \
         \"))))(Tile((id \
         44a885cc-b5bb-4553-9085-23ada1d2e85a)(label(multiplier))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         f2b9576f-5574-4613-ba5b-2aaf2e7131d6)(content(Whitespace\" \
         \")))))((Secondary((id \
         6ed60e15-c4ce-4bd0-bed4-f092845be591)(content(Whitespace\"\\n\"))))(Tile((id \
         1f124ad1-b1d5-4349-a699-9d262a35d3cf)(label(case end))(mold((out \
         Exp)(in_(Rul))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         fea554a7-8a6e-4222-b6e9-1e35a1fcd9f0)(content(Whitespace\" \
         \"))))(Tile((id \
         d01fc059-b224-4715-90ac-ff5f12e8624b)(label(phase))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         ca0a5a8f-0580-4504-b881-3aa54ceaaf66)(content(Whitespace\"\\n\"))))(Tile((id \
         a98cdecd-6347-41f6-b896-0d446fffe4f1)(label(| =>))(mold((out \
         Rul)(in_(Pat))(nibs(((shape(Concave 46))(sort Exp))((shape(Concave \
         46))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         2c2e77ed-8ca5-43de-9d92-ff08ce0f44da)(content(Whitespace\" \
         \"))))(Tile((id \
         8682d2be-cf79-45b6-b6fe-e933fcb3132b)(label(New))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         d7a7065a-41e7-4a1b-bff2-49efc777791b)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         bfa1274a-62bc-4d33-9eee-46292be525a3)(content(Whitespace\" \
         \"))))(Tile((id \
         5151e4de-318d-497e-be48-b0e96793ac16)(label(1.2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         6deebd2d-51d2-48af-b978-f71e31eee8de)(content(Whitespace\"\\n\"))))(Tile((id \
         3e260b4b-b3a3-45fe-ad91-01d1dbf2d526)(label(| =>))(mold((out \
         Rul)(in_(Pat))(nibs(((shape(Concave 46))(sort Exp))((shape(Concave \
         46))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         d188eb87-3b7e-4b62-9323-944dfb6ef2b0)(content(Whitespace\" \
         \"))))(Tile((id \
         cb074b96-7d8d-4ff9-a68c-a1c91f56a0c3)(label(Full))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         5683d6cb-28b9-463d-8824-6264bbaa08d6)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         b7126a79-cc96-45e7-8a04-8f35099a806f)(content(Whitespace\" \
         \"))))(Tile((id \
         6cc3eaf2-5beb-4154-9b07-e1ff6d3eee26)(label(0.88))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         b2aac944-f0ad-4ce2-9b72-e9bc38eb5117)(content(Whitespace\"\\n\"))))(Tile((id \
         ae714a8a-7f13-472b-a5b5-253e95f299b0)(label(| =>))(mold((out \
         Rul)(in_(Pat))(nibs(((shape(Concave 46))(sort Exp))((shape(Concave \
         46))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         01539941-2f96-4226-9d72-a664238b53b3)(content(Whitespace\" \
         \"))))(Tile((id \
         c8271545-9aaa-4c48-a56e-03bf8ef3cd7d)(label(Waxing))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         233039e2-7f0d-4ac5-a12e-bbfd0581846b)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         4f5233dc-b016-4b8c-ac88-9a2e5a5cef40)(content(Whitespace\" \
         \"))))(Tile((id \
         79f78413-677c-4d2e-9881-080fe0e9fc43)(label(1.1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         9bbc0ad7-f2d3-4c98-aae5-4d4982e60ed3)(content(Whitespace\"\\n\"))))(Tile((id \
         8c837e7c-c7f1-406b-8217-fbf5152b9838)(label(| =>))(mold((out \
         Rul)(in_(Pat))(nibs(((shape(Concave 46))(sort Exp))((shape(Concave \
         46))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         33d12af4-1fbb-4efb-839d-4b66f3bdb7ab)(content(Whitespace\" \
         \"))))(Tile((id \
         0de23aa6-7291-4b47-94b0-9ac8c8127cd2)(label(Waning))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         0ff992fd-6993-4ea4-82ef-471e91bb1f27)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         a2c805fa-8121-4e22-8ad8-4e87a0cb2804)(content(Whitespace\" \
         \"))))(Tile((id \
         3ee9dd07-5d4e-43a2-8544-309cce4b309a)(label(0.95))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         1f1b0d09-bfd9-4c3e-a02d-d8da965cbe2c)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         aa0403e2-8f06-4943-9e3c-ed1bfa025027)(content(Whitespace\" \
         \"))))(Secondary((id \
         11653cdb-84b8-42d8-85ee-2b2dc67439aa)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         494ddaa7-0517-4acd-91e0-b414dc0e278e)(content(Whitespace\" \
         \"))))(Tile((id \
         3e248495-db4f-45d5-9045-c95512261e43)(label(int_of_float))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         a2a8c193-57ea-467a-af82-93692c92ff1d)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         8ee0dc22-6dec-43b3-a781-2d9aa7ed6c0e)(label(float_of_int))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         bf09ff1c-5118-4634-81fd-561b548a5aea)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         05089cb4-8ae5-4b76-8139-f4aec6ee3e71)(label(base_ml))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         016b4779-e1cd-4cdd-ae11-ce97b3ede154)(content(Whitespace\" \
         \"))))(Tile((id \
         f718b176-5c45-489f-9588-17558d6ca7f9)(label(*.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 27))(sort Exp))((shape(Concave \
         27))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         1ed2bd05-817a-4a42-b93b-aa3a70402ca1)(content(Whitespace\" \
         \"))))(Tile((id \
         16536a0e-a326-42f5-ab32-429e820219ae)(label(multiplier))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         1fcbc6eb-a91a-47c0-a00d-7f6cd2b909f0)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         ff667b49-ecc1-405c-882c-453500d9ef73)(content(Whitespace\"\\n\"))))(Secondary((id \
         46119f5e-a9c6-4ecf-96d2-a1f354af9b22)(content(Whitespace\"\\n\"))))(Secondary((id \
         c39c0268-71ab-4217-bebd-9cf0b90fbaf7)(content(Comment\"# TAKEAWAY: \
         The dynamic cursor is an internal mechanism which \
         #\"))))(Secondary((id \
         4cb42c83-9985-4606-ae17-563a7db8bba4)(content(Whitespace\"\\n\"))))(Secondary((id \
         7e2832f6-09c3-499c-b1e8-ffab8006e4a8)(content(Comment\"# tries to \
         keep the probe samples shown aligned to the same \
         #\"))))(Secondary((id \
         8900d2f5-eb1d-401d-86ab-6b77802b301c)(content(Whitespace\"\\n\"))))(Secondary((id \
         c5e0d57f-25ea-4cc8-b3ef-2270c01f9ec2)(content(Comment\"# execution, \
         in particular the same call to a function. #\"))))(Secondary((id \
         bb7d7ff9-517e-43c5-8dc0-e65477856dc7)(content(Whitespace\"\\n\"))))(Secondary((id \
         e5c414b7-d03d-4c33-85d9-b6c78e958f53)(content(Whitespace\"\\n\"))))(Tile((id \
         d9334d49-7977-4300-8a57-8bc831886a72)(label(watering_amount))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         4c5363b8-8790-4967-a75b-df03856efd93)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         9b7679ae-7d8e-4046-9f89-e41873caa71c)(label(250))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         0716cf82-3f19-418a-8650-c7cedc025f27)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         762a6642-0b36-44ef-8fff-c2bc51cfddff)(content(Whitespace\" \
         \"))))(Tile((id \
         40bec06d-e890-42af-8600-3bd55c5c616e)(label(Full))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         0d76f2a3-58e4-4488-a46a-451a07de231a)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         8799bf39-670f-4dda-8905-3d9287271eac)(content(Whitespace\"\\n\"))))(Tile((id \
         88383bd1-f1f1-4c72-ac81-9b2036fc460f)(label(watering_amount))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         bc1167e1-b260-4fad-bdc5-6a0763fab538)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         2c55cd0a-2b22-4f1d-9c2f-ba89043cf39c)(label(50))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         2d323d61-71e8-4173-8c1e-b35139abf2f6)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         8c2ad190-77f1-4922-9d68-4cc6f7fd2562)(content(Whitespace\" \
         \"))))(Tile((id \
         5394170b-6ef9-493f-85d1-918fffb11691)(label(New))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         479924c6-6c0f-405f-8abd-f4039c50842f)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         842fb319-a105-465c-92ee-16e72c980cd9)(content(Whitespace\"\\n\"))))(Tile((id \
         37eaeb3f-2ed4-418c-bde8-b733246be734)(label(watering_amount))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         2c0a87a6-92f5-429d-aa51-7583035d147f)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         c55c6cea-ea4b-459e-a2f8-d704bdff37f8)(label(180))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         634b8997-ca90-407d-bac3-cdeae18cb6cb)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         41da4675-b843-42d2-9145-cc8f9338e8e5)(content(Whitespace\" \
         \"))))(Tile((id \
         4bd47992-fa7b-40ee-8870-58b47ff73039)(label(Waning))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         8a9f2ae7-eb3d-457d-a061-b9eaa57839f3)(content(Whitespace\"\\n\"))))(Secondary((id \
         9b9fb497-3fdf-49d6-8119-9af18bbd1114)(content(Whitespace\"\\n\"))))(Secondary((id \
         cf198fd4-6f64-473e-8c98-ca78c89e86ea)(content(Comment\"# One last \
         thing: SINGLE MODE (default) vs MANY MODE #\"))))(Secondary((id \
         e423a27b-0ec8-464d-9ae1-a2b422d2d5c7)(content(Whitespace\"\\n\"))))(Secondary((id \
         29281c16-303e-47bf-82b3-891d1d2d74ab)(content(Comment\"# Double-click \
         any above sample, or press Space when a sample #\"))))(Secondary((id \
         609789ce-d94d-4d14-a4e1-9dfe250a1419)(content(Whitespace\"\\n\"))))(Secondary((id \
         88a0bd97-0067-4f41-854d-2b4ba1210179)(content(Comment\"# is selected \
         to toggle Many mode: all samples are shown at once! \
         #\"))))(Secondary((id \
         ff78e749-f902-46af-b55c-4f2b94136c5f)(content(Whitespace\"\\n\"))))(Secondary((id \
         8a4d2dca-f273-4a45-ba9b-29ed6a914349)(content(Comment\"# Similarly to \
         single mode, left/right arrow keys move samples. \
         #\"))))(Secondary((id \
         152036f6-7f95-4494-bf19-25014c82bf29)(content(Whitespace\"\\n\"))))(Secondary((id \
         e017dc2f-a4c5-40f7-bed3-8a3519aee076)(content(Comment\"# Double-click \
         again (or Space) to go back to Single mode. #\"))))(Secondary((id \
         b7b3e5cc-992c-4d05-9fa2-ae650d3112e5)(content(Whitespace\"\\n\"))))(Secondary((id \
         060fa12b-67f5-4d2e-8c68-2f20fcbe6e4c)(content(Whitespace\"\\n\"))))(Secondary((id \
         13e1cf20-a111-44eb-b204-c047f6ee83da)(content(Comment\"# END OF PART \
         2 - Select the next slide from the top menu #\"))))(Secondary((id \
         e9a08d1a-4010-4647-aa37-2dcf9919ac42)(content(Whitespace\"\\n\")))))";
      backup_text =
        "# PROBES TUTORIAL - PART 2: FUNCTIONS AND THE DYNAMIC CURSOR #\n\n\
         # When a function is called multiple times, each call #\n\
         # generates its own sample. Let's see what that looks like! #\n\n\
         type MoonPhase = New + Waxing + Full + Waning in\n\n\
         # Hazel has no special function definition syntax. #\n\
         # We use regular let definitions to define function literals, #\n\
         # using the syntax `fun <pattern> -> <body>`. #\n\n\
         # TRY THIS: Add a probe to the `multiplier` variable inside #\n\
         # the function `watering_amount` below. When you click on the #\n\
         # sample, notice the arrows that appear to the left. Click on #\n\
         # these arrows, or use the left/right arrow keys, to navigate #\n\
         # between the three different samples collected. #\n\n\
         let watering_amount: (Int, MoonPhase) -> Int =\n\
         fun (base_ml, phase) ->\n\
         let multiplier =\n\
         case phase\n\
         | New => 1.2\n\
         | Full => 0.88\n\
         | Waxing => 1.1\n\
         | Waning => 0.95\n\
         end \n\
         in int_of_float(float_of_int(base_ml) *. multiplier)\n\
         # Above: Hazel uses C-style Function application syntax #\n\
         in\n\n\
         # Now click the samples for the 3 calls to `watering_amount` below. #\n\
         # Notice the sample for 'multiplier' above changes to /align/ with #\n\
         # the selected call! We call this behavior the 'dynamic cursor', #\n\
         # which aligns probe samples to a particular step in an execution. #\n\n\
         ^^probe(watering_amount(250, Full));\n\
         ^^probe(watering_amount(50, New));\n\
         ^^probe(watering_amount(180, Waning));\n\n\
         # Below is the same function as above, this time with many probes. #\n\
         # Select the `multiplier` sample and use the arrow keys to move #\n\
         # through the different values. Notice how this time, there are two #\n\
         # different symbols next to the branches with no samples; \
         \226\136\133 from #\n\
         # before on `Waxing`, which means never evaluated, and a new symbol #\n\
         # \226\138\150, which means there are samples, but they are not \
         aligned to the #\n\
         # dynamic cursor (because of the `multiplier` sample you selected). #\n\
         # Click on any \226\138\150 to align the dynamic cursor to that \
         branch. #\n\n\
         let watering_amount: (Int, MoonPhase) -> Int =\n\
         fun (base_ml, phase) ->\n\
         let ^^probe(multiplier) =\n\
         case ^^probe(phase)\n\
         | New => ^^probe(1.2)\n\
         | Full => ^^probe(0.88)\n\
         | Waxing => ^^probe(1.1)\n\
         | Waning => ^^probe(0.95)\n\
         end \n\
         in ^^probe(int_of_float(float_of_int(base_ml) *. multiplier))\n\
         in\n\n\
         # TAKEAWAY: The dynamic cursor is an internal mechanism which #\n\
         # tries to keep the probe samples shown aligned to the same #\n\
         # execution, in particular the same call to a function. #\n\n\
         watering_amount(250, Full);\n\
         watering_amount(50, New);\n\
         watering_amount(180, Waning)\n\n\
         # One last thing: SINGLE MODE (default) vs MANY MODE #\n\
         # Double-click any above sample, or press Space when a sample #\n\
         # is selected to toggle Many mode: all samples are shown at once! #\n\
         # Similarly to single mode, left/right arrow keys move samples. #\n\
         # Double-click again (or Space) to go back to Single mode. #\n\n\
         # END OF PART 2 - Select the next slide from the top menu #\n";
      refractors =
        "((a2a8c193-57ea-467a-af82-93692c92ff1d((kind \
         Probe)(model\"()\")))(3ee9dd07-5d4e-43a2-8544-309cce4b309a((kind \
         Probe)(model\"()\")))(79f78413-677c-4d2e-9881-080fe0e9fc43((kind \
         Probe)(model\"()\")))(6cc3eaf2-5beb-4154-9b07-e1ff6d3eee26((kind \
         Probe)(model\"()\")))(5151e4de-318d-497e-be48-b0e96793ac16((kind \
         Probe)(model\"()\")))(d01fc059-b224-4715-90ac-ff5f12e8624b((kind \
         Probe)(model\"()\")))(44a885cc-b5bb-4553-9085-23ada1d2e85a((kind \
         Probe)(model\"()\")))(76731098-31c8-42ce-8c66-57aac7b3b9b3((kind \
         Probe)(model\"()\")))(343be700-701f-4ff5-ab4c-70aa36f1766a((kind \
         Probe)(model\"()\")))(1bd0ed0d-f3e2-432d-a615-523ba5bf72b2((kind \
         Probe)(model\"()\"))))";
    } )
