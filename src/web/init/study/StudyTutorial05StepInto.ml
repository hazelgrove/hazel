let out : string * Haz3lcore.PersistentSegment.t =
  ( "Study / tutorial / 05-step-into",
    {
      segment =
        "((Secondary((id \
         4a728433-e678-46f9-9b75-2ef05a5d78db)(content(Comment\"# PROBES \
         TUTORIAL - PART 5: STEP INTO AND THE DYNAMIC CURSOR BAR \
         #\"))))(Secondary((id \
         c3cecc40-ec74-48ee-a215-0ec5791ca7fb)(content(Whitespace\"\\n\"))))(Secondary((id \
         ccfd8288-bcf1-442f-98ab-3c5bd872216b)(content(Comment\"# You've \
         pinned a call and can see values inside a function. \
         #\"))))(Secondary((id \
         ea1dfc84-3e30-4fca-a933-9a65e8011386)(content(Whitespace\"\\n\"))))(Secondary((id \
         9d50d219-4576-4c7a-80dc-94e82df44295)(content(Comment\"# But what if \
         the bug is deeper, inside a function that your #\"))))(Secondary((id \
         916ebb21-708d-4f0f-b8ad-7a6ca162f342)(content(Whitespace\"\\n\"))))(Secondary((id \
         f2c6f1c3-ca2a-4c7b-a819-4315584385f0)(content(Comment\"# function \
         calls? Step Into follows the call stack down. #\"))))(Secondary((id \
         0da78b31-4a59-45ec-aa8f-3495d2915da5)(content(Whitespace\"\\n\"))))(Secondary((id \
         55d6a418-676a-4d97-b0c4-85ddf9c0e60d)(content(Whitespace\"\\n\"))))(Secondary((id \
         a7b4b468-a793-41ea-975d-3c362b6045d8)(content(Comment\"# TRY THIS: \
         #\"))))(Secondary((id \
         390fd470-ba9a-46c0-969a-007677cdc3e2)(content(Whitespace\"\\n\"))))(Secondary((id \
         50db7dc6-be2c-454d-9d04-d9ac9943891d)(content(Comment\"# 1. Turn on \
         auto-probe and click inside `daily_water` #\"))))(Secondary((id \
         205fd437-b716-493c-8136-2db260f0f2f3)(content(Whitespace\"\\n\"))))(Secondary((id \
         3ad6863c-e8ca-4e18-aa13-0e509fc07c43)(content(Comment\"# 2. Pin one \
         of the test calls (click a sample > Pin) #\"))))(Secondary((id \
         48af411a-9087-41d0-b3ce-34a6628dcb61)(content(Whitespace\"\\n\"))))(Secondary((id \
         082630aa-b310-4992-bcdf-4b67b16c9f31)(content(Comment\"# 3. Now add a \
         probe to the `phase_multiplier(phase)` call #\"))))(Secondary((id \
         c984a248-e682-4482-a786-2640b57dcdf1)(content(Whitespace\"\\n\"))))(Secondary((id \
         13151fd2-57e8-4c88-84b8-2b357466a01a)(content(Comment\"#    inside \
         `daily_water` (click on `phase_multiplier`) #\"))))(Secondary((id \
         60934a9d-e294-48b1-a6ef-61cf05004160)(content(Whitespace\"\\n\"))))(Secondary((id \
         339eb45c-57a9-4eaf-b2d1-e574909abc07)(content(Comment\"# 4. Click \
         that sample and choose \\\"Step Into\\\" from the \
         #\"))))(Secondary((id \
         ff5b5049-d471-4c2e-9390-feed17e1d058)(content(Whitespace\"\\n\"))))(Secondary((id \
         a615c5c2-9bf6-4c08-bd31-ad853b85cb57)(content(Comment\"#    dropdown \
         (or press Enter) #\"))))(Secondary((id \
         faf4098b-67d8-4249-abe9-99a930ad380f)(content(Whitespace\"\\n\"))))(Secondary((id \
         d4330c87-cb51-44e5-ab80-13c5e08756f8)(content(Comment\"# 5. Your \
         cursor jumps into `phase_multiplier`! The probes \
         #\"))))(Secondary((id \
         4e59e58f-6f24-433e-a0e9-2af165879fe1)(content(Whitespace\"\\n\"))))(Secondary((id \
         1acbbdf1-bf37-4651-9a16-21d80b4aed63)(content(Comment\"#    there \
         show only values from your pinned context. #\"))))(Secondary((id \
         ae9f8bb8-52cc-419b-95a9-0d42d69af063)(content(Whitespace\"\\n\"))))(Secondary((id \
         856aaa35-484e-4910-9386-bd8ad1ce64aa)(content(Whitespace\"\\n\"))))(Secondary((id \
         9b12f307-58f3-42b9-9c77-2f55df0f094e)(content(Comment\"# THE DYNAMIC \
         CURSOR BAR #\"))))(Secondary((id \
         7bed8417-ccd0-495f-a8c1-1de2f0238f2c)(content(Whitespace\"\\n\"))))(Secondary((id \
         c8fd743e-f013-4b32-a2e9-8b15fb578f28)(content(Comment\"# Look at the \
         bar at the top of the editor after stepping in. #\"))))(Secondary((id \
         b442bac9-8580-4bf8-824c-53e405b9e955)(content(Whitespace\"\\n\"))))(Secondary((id \
         a74543de-980e-4699-89b8-9842ec9d3d60)(content(Comment\"# It shows \
         your position in the call stack as breadcrumbs: #\"))))(Secondary((id \
         fbbdec56-2e78-4726-a8ed-422b148e7151)(content(Whitespace\"\\n\"))))(Secondary((id \
         be9565ce-ea69-4ba4-871a-27e390918115)(content(Comment\"#   top-level \
         > daily_water > phase_multiplier #\"))))(Secondary((id \
         2553e6a7-b637-4e03-ae9f-c52bfc64e779)(content(Whitespace\"\\n\"))))(Secondary((id \
         10940912-b850-4e32-a57d-c72b6a93e772)(content(Comment\"# Click a \
         function name to jump to its definition. #\"))))(Secondary((id \
         09991db3-3cb5-4315-a51e-924fc56f0404)(content(Whitespace\"\\n\"))))(Secondary((id \
         8983ec4e-b750-406d-8a43-d778b10fa8e2)(content(Comment\"# Click a \
         chevron (>) to jump to the call site. #\"))))(Secondary((id \
         39227a30-68e8-41a7-8f59-1f64cf452a75)(content(Whitespace\"\\n\"))))(Secondary((id \
         6b268ad8-7d02-4835-8f9e-3bcb631960f6)(content(Comment\"# This lets \
         you move up and down the call stack freely. #\"))))(Secondary((id \
         392957b6-4cd4-4ebc-a12b-5c3e4db75d0a)(content(Whitespace\"\\n\"))))(Secondary((id \
         0ec67064-e306-44a2-aab0-f2e18efe7e64)(content(Whitespace\"\\n\"))))(Tile((id \
         648963c2-4936-4410-b176-5d4533f72ba3)(label(type = in))(mold((out \
         Exp)(in_(TPat Typ))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         f558f41a-c86c-402e-a01c-49ad8d2328bd)(content(Whitespace\" \
         \"))))(Tile((id \
         6831a608-0f97-4644-8931-0fda1123481a)(label(MoonPhase))(mold((out \
         TPat)(in_())(nibs(((shape Convex)(sort TPat))((shape Convex)(sort \
         TPat))))))(shards(0))(children())))(Secondary((id \
         c3e9ee32-a3ec-426b-a989-de96248d5ff6)(content(Whitespace\" \
         \")))))((Secondary((id \
         cef73f5c-6863-44ae-be7d-2257f57bbdde)(content(Whitespace\" \
         \"))))(Tile((id \
         0e4479a7-734d-4b73-aee1-ab3f9c434f9b)(label(+))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape(Concave 33))(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         a9164cda-058b-40a6-80de-1adae598cacf)(content(Whitespace\" \
         \"))))(Tile((id \
         482a3c49-3ece-4f68-a578-fc86462f578b)(label(New))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         81d5749b-d8a6-414d-ab64-cecf0e931cd2)(content(Whitespace\" \
         \"))))(Tile((id \
         644df1c4-dad3-456e-9403-b0819168e873)(label(+))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 12))(sort Typ))((shape(Concave \
         12))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         bdf1c7be-dfab-4c68-acd6-c5836fbd4468)(content(Whitespace\" \
         \"))))(Tile((id \
         160e57a4-0793-440c-961d-c79e45a93feb)(label(Waxing))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         43432ee8-89d4-4a83-b574-13f15dcb1d5a)(content(Whitespace\" \
         \"))))(Tile((id \
         25dd467d-6d74-4705-b1a2-bcb3c808a584)(label(+))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 12))(sort Typ))((shape(Concave \
         12))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         f721a8e1-5876-43d0-ae9b-62616ad32c70)(content(Whitespace\" \
         \"))))(Tile((id \
         355609ff-f15d-4acd-901a-9b4d394fb243)(label(Full))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         a8d4cd4d-a933-4597-a773-efa77ab1e6b0)(content(Whitespace\" \
         \"))))(Tile((id \
         4ff214f4-4d08-4d14-9da6-38db72d2c085)(label(+))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 12))(sort Typ))((shape(Concave \
         12))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         49b1159d-8304-4292-858d-92d992b39528)(content(Whitespace\" \
         \"))))(Tile((id \
         bd6f09f6-c3f2-4c5c-b259-0eed68404afe)(label(Waning))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         88398d5f-4e5f-45c1-9d51-1cdab90c061e)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         e62b9a30-85a1-441d-bee7-9c42f3c95b11)(content(Whitespace\"\\n\"))))(Secondary((id \
         34fc13ed-2ed5-497b-b2ec-6bf5d074fe4f)(content(Whitespace\"\\n\"))))(Tile((id \
         45717f28-9aaf-4013-8be4-37b5dedc6f8c)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         169be539-9600-4a57-8957-a5e29305b218)(content(Whitespace\" \
         \"))))(Tile((id \
         b7e1640d-e3ce-442c-a8b0-05f279e40310)(label(phase_multiplier))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         eabfe17d-21de-4ea8-8d6c-fe4912be9fad)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 24))(sort Pat))((shape(Concave \
         24))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         1aaa2ed3-70bf-4dc8-a80d-cfb8290689ee)(content(Whitespace\" \
         \"))))(Tile((id \
         77bdd4de-4fa8-4367-9c42-ccda629dec6d)(label(MoonPhase))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         38a50c68-2dd8-442f-8734-ad571b7b3b8a)(content(Whitespace\" \
         \"))))(Tile((id \
         2a617e4c-9296-4338-9ce3-3ff04b99c5d3)(label(->))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 13))(sort Typ))((shape(Concave \
         13))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         17c21015-d26e-4b97-8b80-8647c7bcc4ee)(content(Whitespace\" \
         \"))))(Tile((id \
         0774bc26-32ed-4a12-aa9c-9d59260f277d)(label(Float))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         fd5d07e0-f422-4b90-a374-602c6a6df89d)(content(Whitespace\" \
         \")))))((Secondary((id \
         bbcc9bd6-ba87-4bbc-9971-4d1dbf3f1652)(content(Whitespace\"\\n\"))))(Tile((id \
         1ba1fdb7-6d36-4ebc-a530-c3f8ffba879b)(label(fun ->))(mold((out \
         Exp)(in_(Pat))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         37))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         e0d806b1-aa37-4e5d-98a5-5c95196f8199)(content(Whitespace\" \
         \"))))(Tile((id \
         608bfe24-7adf-4024-8e8f-83660256f5a7)(label(phase))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         b15ece4d-99b7-4607-9093-eb74189f1888)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         026710ed-c67d-4919-80ab-f414820d7a80)(content(Whitespace\" \
         \"))))(Tile((id 0d2a6f41-6ea8-4634-b6cf-cd36b5e3f0d7)(label(case \
         end))(mold((out Exp)(in_(Rul))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         b3b1dcf1-88cf-4db5-a27c-036fea3e9473)(content(Whitespace\" \
         \"))))(Tile((id \
         63c55da3-ed56-4011-a8c0-6512014257a0)(label(phase))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         fea4283b-c34f-407c-9b53-277ab6d095bd)(content(Whitespace\"\\n\"))))(Tile((id \
         1f46f9f9-f9d5-4503-bad2-9df793047eac)(label(| =>))(mold((out \
         Rul)(in_(Pat))(nibs(((shape(Concave 46))(sort Exp))((shape(Concave \
         46))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         cd66526b-6948-46ee-9d5e-c6ba17bda0ba)(content(Whitespace\" \
         \"))))(Tile((id \
         6d2cdf0e-740e-4a99-9e1c-cae4851db13d)(label(New))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         7d63385d-d711-4a73-916f-ebb3bf885e1a)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         fd512c63-5b96-45f7-a6cb-acf4d5824437)(content(Whitespace\" \
         \"))))(Tile((id \
         cb25fbfb-8a65-43a5-b552-e651aa17b10f)(label(1.2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         40ae5b6c-1a2e-4b1c-ad86-2fafcede7ee6)(content(Whitespace\"\\n\"))))(Tile((id \
         e15293f2-cae3-4ddb-87c6-6fdb77b7eebf)(label(| =>))(mold((out \
         Rul)(in_(Pat))(nibs(((shape(Concave 46))(sort Exp))((shape(Concave \
         46))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         df7066c2-a9c4-4187-9fb2-fd9694c390ea)(content(Whitespace\" \
         \"))))(Tile((id \
         fa7e7cf2-b879-4697-a014-ac361439e97c)(label(Waxing))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         e641fd28-d201-4eea-b83d-e97e4f770a55)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         9da88127-7257-43b7-ac74-2c5797d4e3d2)(content(Whitespace\" \
         \"))))(Tile((id \
         91476991-3524-440e-9df2-e5ebe82daaab)(label(1.1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         c13724fc-bc08-4260-b2a1-c0ea273cad17)(content(Whitespace\"\\n\"))))(Tile((id \
         5218feaa-c0fe-4155-82e2-775286937e33)(label(| =>))(mold((out \
         Rul)(in_(Pat))(nibs(((shape(Concave 46))(sort Exp))((shape(Concave \
         46))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         a7a8fdc3-fbf7-4fef-a54a-ff74ecbf5aa2)(content(Whitespace\" \
         \"))))(Tile((id \
         2a4f7370-df92-4c69-b74c-ead2211e7b89)(label(Full))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         bbcdafe5-e07b-4fba-bc14-abc083140826)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         b1007ecf-b1c4-4b92-ad1a-ce1afe97ab43)(content(Whitespace\" \
         \"))))(Tile((id \
         e4b350f0-d767-442b-a4b3-e85b1a090dc6)(label(0.88))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         f6321705-3554-45ea-8fba-bf73d17767ad)(content(Whitespace\"\\n\"))))(Tile((id \
         37cf32ea-46d3-436d-999d-03ebeb08c97b)(label(| =>))(mold((out \
         Rul)(in_(Pat))(nibs(((shape(Concave 46))(sort Exp))((shape(Concave \
         46))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         e661305e-eb0e-4bd9-92a4-29dcbabb5e21)(content(Whitespace\" \
         \"))))(Tile((id \
         618ad3c6-416f-425a-b1b2-dd168523930a)(label(Waning))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         2fd3a20d-4d2d-43d1-9b70-bb42f17cf65c)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         6941f19d-de78-47c0-976c-235da4addf29)(content(Whitespace\" \
         \"))))(Tile((id \
         fe5428ee-2bcb-4c2b-8d5a-e61a4608a7e7)(label(0.95))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         e4deb969-8409-4cf0-a95e-c426853d125c)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         21bda41e-4b37-4e49-899d-0e0406288a05)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         c28ac6f5-ace7-498e-a5df-f24b8d28ec75)(content(Whitespace\"\\n\"))))(Secondary((id \
         fdda6b3b-0137-4f1c-8e45-16a3f1419cf8)(content(Whitespace\"\\n\"))))(Tile((id \
         511d7af1-01d3-493c-b2fc-e2b9a547deda)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         f2f7c277-3b3a-48e1-826a-3287777c88f6)(content(Whitespace\" \
         \"))))(Tile((id \
         0f7ed36f-f450-4a8c-b0df-1dfd061649d3)(label(shade_multiplier))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         41e1ca91-2bf9-4602-b3ff-cfa40c220d3d)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 24))(sort Pat))((shape(Concave \
         24))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         c9210a1d-9663-4d95-b3b1-1f9a3c29bc3e)(content(Whitespace\" \
         \"))))(Tile((id \
         7dc8e838-9382-4193-bdc1-30f692eb5c47)(label(Int))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         c16d271e-003a-42d1-a7a9-2aa620fe4c1a)(content(Whitespace\" \
         \"))))(Tile((id \
         cf697907-1abe-4a08-9c9f-7d3f2986f9a6)(label(->))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 13))(sort Typ))((shape(Concave \
         13))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         8b3a24d2-5620-4e21-933f-d97b8ef808ce)(content(Whitespace\" \
         \"))))(Tile((id \
         a4a718a0-3f87-4566-b7f7-9076137ea750)(label(Float))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         16474007-c9e6-45be-ada2-8f7f47cd3cbb)(content(Whitespace\" \
         \")))))((Secondary((id \
         391817e8-a2b4-4cab-b5bb-4971ba207e17)(content(Whitespace\"\\n\"))))(Tile((id \
         d779aab8-7441-4029-830e-432d96ff784d)(label(fun ->))(mold((out \
         Exp)(in_(Pat))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         37))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         9f1c1ce0-31fb-4822-aa9f-3e8e9539fbac)(content(Whitespace\" \
         \"))))(Tile((id \
         9ef749d5-fb9a-411e-a69a-998c1e2717de)(label(shade))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         043b4744-2e8c-4f14-abc6-b61407a0d56c)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         7497a232-fc4d-4af6-9a9c-4bce0906d1f2)(content(Whitespace\" \
         \"))))(Tile((id 360acb6c-6cca-45c1-b0fd-e5d7c1862812)(label(case \
         end))(mold((out Exp)(in_(Rul))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         07511eb8-ae7c-467f-a097-6f11f4f93cf8)(content(Whitespace\" \
         \"))))(Tile((id \
         3dd3d8b6-7998-4239-92a4-5208995816c7)(label(shade))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         03868658-1565-4981-beeb-33e2aa409f41)(content(Whitespace\"\\n\"))))(Tile((id \
         5f065ee8-d323-495a-bc0e-5a705514da1b)(label(| =>))(mold((out \
         Rul)(in_(Pat))(nibs(((shape(Concave 46))(sort Exp))((shape(Concave \
         46))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         edc9344e-f6a9-4e62-adfa-786133c4475d)(content(Whitespace\" \
         \"))))(Tile((id \
         6f2a9bcd-0ec5-46ed-bea1-1fdeaa5b1bf6)(label(0))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         6e1d141f-c38f-4b49-a727-8a63757b2e0c)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         a2f731ed-c48a-4642-801c-168f5d68be85)(content(Whitespace\" \
         \"))))(Tile((id \
         9b713fc6-d79b-419e-9b28-7e96541291a4)(label(1.0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         e397e14a-ff24-40bf-9765-0d5f26200389)(content(Whitespace\"\\n\"))))(Tile((id \
         9715160a-0e08-4568-b7b9-a301a5df05aa)(label(| =>))(mold((out \
         Rul)(in_(Pat))(nibs(((shape(Concave 46))(sort Exp))((shape(Concave \
         46))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         077bd3c0-a1a9-4e3a-994c-f117e88a64e8)(content(Whitespace\" \
         \"))))(Tile((id \
         08133e98-4931-45b3-a2af-58d7ea17f6b5)(label(1))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         55e471d7-c604-46e6-92e3-16c403f97485)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         30e139cc-bcb9-4145-8900-ad616d8df71a)(content(Whitespace\" \
         \"))))(Tile((id \
         dcb274ea-4aba-4560-8d3b-6219fd2bf106)(label(0.9))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         312632e5-ec88-49d7-835e-387859d96b67)(content(Whitespace\"\\n\"))))(Tile((id \
         c139b04b-9dbf-4b66-8371-616ad0924193)(label(| =>))(mold((out \
         Rul)(in_(Pat))(nibs(((shape(Concave 46))(sort Exp))((shape(Concave \
         46))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         2286838a-3f1f-4d38-9578-a5dd6434e349)(content(Whitespace\" \
         \"))))(Tile((id \
         e018f047-d576-4a9e-808b-0a0dabaefbbc)(label(2))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         f6583303-449d-4ae1-a22c-85b4ea114993)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         01ff1dc6-9f22-42ea-927f-32ba6b7e6463)(content(Whitespace\" \
         \"))))(Tile((id \
         c4b9a141-5487-46fb-af1a-5a65b67346b2)(label(0.75))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         2c76b5d4-45b0-4a7f-9200-4dfa157ebcfe)(content(Whitespace\"\\n\"))))(Tile((id \
         326127da-5add-4fcd-85f5-6e8dda52a0ba)(label(| =>))(mold((out \
         Rul)(in_(Pat))(nibs(((shape(Concave 46))(sort Exp))((shape(Concave \
         46))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         4adcfe61-605f-4bf9-893f-a1c44250c799)(content(Whitespace\" \
         \"))))(Tile((id \
         f5f18138-409b-49f4-8458-166adb9c066f)(label(_))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         c7878b94-5695-48e6-acc1-a2e0b0eeef1d)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         9fea949d-fdbd-4343-999f-2a3f3651b512)(content(Whitespace\" \
         \"))))(Tile((id \
         396a5918-35ac-4c23-8785-82f42f6a58f3)(label(0.6))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         aa6aac29-ccb0-4f1d-88ed-9fbf8a981d39)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         ef82e5c7-944e-40de-aee3-b30e9c4a986b)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         5c965f34-34ea-4aa9-8962-86f05c2c221c)(content(Whitespace\"\\n\"))))(Secondary((id \
         abd1ad0f-ea6b-4e24-83c2-faed41102278)(content(Whitespace\"\\n\"))))(Tile((id \
         6efd0c68-66aa-4763-b61e-b12c0e193db5)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         b78b39a5-e829-4455-9f7e-e93b2e91ad98)(content(Whitespace\" \
         \"))))(Tile((id \
         af5f5ac1-a088-49a9-a388-070bece0f6d2)(label(daily_water))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         8944284c-cbea-4d8c-a0d4-821d41de62e0)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 24))(sort Pat))((shape(Concave \
         24))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         6af7a21b-b22b-4fd2-9c3f-fc7dab9c646e)(content(Whitespace\" \
         \"))))(Tile((id \
         c2e33d6c-2b33-488a-85a7-dddd8582fc13)(label(\"(\"\")\"))(mold((out \
         Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0 1))(children(((Tile((id \
         eed6a2ea-cd87-4c7e-bef1-f8c6ab057321)(label(Int))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         6b6a74e5-0e17-4a24-8110-f240f17023a2)(label(,))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 44))(sort Typ))((shape(Concave \
         44))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         ab65bcd1-e0aa-42fe-b639-05d5823145ee)(content(Whitespace\" \
         \"))))(Tile((id \
         5d8de801-8145-469c-80c8-0c53976b6ba4)(label(Int))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         470875e3-68f4-4b31-9989-1e4175dee64d)(label(,))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 44))(sort Typ))((shape(Concave \
         44))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         c4abef6a-c280-4a48-b847-caa080b18997)(content(Whitespace\" \
         \"))))(Tile((id \
         9fa73d39-8bbd-4944-b6bc-4ae7622da114)(label(MoonPhase))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children()))))))))(Secondary((id \
         228baa28-8577-4f71-a815-3f712926f527)(content(Whitespace\" \
         \"))))(Tile((id \
         1c062815-7e98-4595-bbfc-b2beae22f589)(label(->))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 13))(sort Typ))((shape(Concave \
         13))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         f4fba5da-42f2-4d0f-b1e0-4b4f983bc05e)(content(Whitespace\" \
         \"))))(Tile((id \
         37e5a1e4-41f0-4e38-9c0a-d1e37cfa2b87)(label(Int))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         08ede155-d66b-4451-805c-301856463510)(content(Whitespace\" \
         \")))))((Secondary((id \
         e00f9a1b-5c72-4c2d-aa65-c31683c2f8af)(content(Whitespace\"\\n\"))))(Tile((id \
         d685ff13-dfe8-4c5b-9470-0c8b265d52b1)(label(fun ->))(mold((out \
         Exp)(in_(Pat))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         37))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         a819da94-e587-446a-ad1c-d12d94e3ed79)(content(Whitespace\" \
         \"))))(Tile((id \
         56131e49-a355-4c44-a3d3-433707d37a24)(label(\"(\"\")\"))(mold((out \
         Pat)(in_(Pat))(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0 1))(children(((Tile((id \
         9297006a-53bd-4b74-9f0b-254afc32403a)(label(base))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         ca8ceb07-5f6a-4421-8885-d82bf7b58a41)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 44))(sort Pat))((shape(Concave \
         44))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         0ef1d90e-e6a4-42b4-ab8c-8f380e5429b8)(content(Whitespace\" \
         \"))))(Tile((id \
         435bda84-c5e1-4870-ae85-47be1a9e8d17)(label(shade))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         5f365a3f-6eb0-4a54-a070-1428ac957829)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 44))(sort Pat))((shape(Concave \
         44))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         4336e471-8821-42ce-83a8-53a366f74c62)(content(Whitespace\" \
         \"))))(Tile((id \
         34b84669-96c1-4041-9e13-41848fd353d9)(label(phase))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children()))))))))(Secondary((id \
         6ead9db6-093c-4cc2-b83f-0072d8e1de3a)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         411f448e-4d02-46d0-a8d3-c09e7d2f548a)(content(Whitespace\"\\n\"))))(Tile((id \
         7886fcb9-2323-43f7-b450-8119eb588860)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         a72c235d-7c72-49c5-9aa0-757fc47a62f9)(content(Whitespace\" \
         \"))))(Tile((id \
         0bc57907-df46-42cb-9cf5-54ef44235fb5)(label(base_f))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         8806a7a3-0a9e-49d7-bb9d-3692c348b37c)(content(Whitespace\" \
         \")))))((Secondary((id \
         14363ba8-2509-4f35-b6df-1a902049e8e2)(content(Whitespace\" \
         \"))))(Tile((id \
         087c39c7-cef1-41f6-980f-a12ee8e1a832)(label(float_of_int))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         be428bb8-9172-467e-8d93-7e6e60d8c18d)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         b3157288-6416-4b80-b2c5-40e9588f928d)(label(base))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         0e9fb111-e7c1-41f8-9acd-3d8ddb59471b)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         9984fb25-c679-47cd-b5c5-cacd46bf61f1)(content(Whitespace\"\\n\"))))(Tile((id \
         fbca8b0b-0085-4f62-af92-82f7c7e966f9)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         5a41215a-de05-47b2-ae37-b1e7716c7e15)(content(Whitespace\" \
         \"))))(Tile((id \
         9b65a2e5-6e38-464a-9cac-be15bf1efa2f)(label(phase_adj))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         daec37b8-164c-447c-8cd3-15702c16cf09)(content(Whitespace\" \
         \")))))((Secondary((id \
         bb65c0ed-08f8-411a-a383-6803ca94715e)(content(Whitespace\" \
         \"))))(Tile((id \
         4a02fab8-5f5b-4e22-9d21-f28639ffc26c)(label(base_f))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         9a6dfbcc-b7d2-47f7-9b55-1a9350313192)(content(Whitespace\" \
         \"))))(Tile((id \
         498b258f-fd13-4ec1-b26a-087f3dd12f7c)(label(*.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 27))(sort Exp))((shape(Concave \
         27))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         214e9539-9070-478f-92b3-3f7c081d5ecb)(content(Whitespace\" \
         \"))))(Tile((id \
         8e517eaa-d6e2-4054-a392-6034ccc1848d)(label(phase_multiplier))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         4dfaf07f-bd5a-4e69-aa66-d75a36321cab)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         6f48a312-8d8a-481a-b9b4-083e2982eda6)(label(phase))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         67cb84c8-b4ed-466a-a5db-a418b4f72784)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         d91ba9db-9388-4290-9875-e2311e635b95)(content(Whitespace\"\\n\"))))(Tile((id \
         92651ae7-e80a-4d5d-abe2-8b2967a7e5e9)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         41d7b1f2-e29d-469f-b3e9-73d79e8ba4a1)(content(Whitespace\" \
         \"))))(Tile((id \
         60dc17b6-78b6-4e22-9c35-17d23c879e29)(label(shade_adj))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         ea0bb73f-7e65-45b8-9757-3af2605b1a9d)(content(Whitespace\" \
         \")))))((Secondary((id \
         31cc8cc7-f8bf-4079-87cf-78a493a72956)(content(Whitespace\" \
         \"))))(Tile((id \
         e9b70f4f-e0e4-45e3-a405-6cd833a4d7e1)(label(phase_adj))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         b5ec7d4b-d85a-4a18-a6f5-070b440993bd)(content(Whitespace\" \
         \"))))(Tile((id \
         16ab7040-d4f7-4188-ab17-f2b911cd9550)(label(*.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 27))(sort Exp))((shape(Concave \
         27))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         c60dc7b3-c331-4f35-bf14-d98294bd3ab6)(content(Whitespace\" \
         \"))))(Tile((id \
         741a322c-00e1-4d47-afe3-fa662410bfe9)(label(shade_multiplier))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         00809970-c7b5-49a6-b9cf-8cfa87921116)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         d1ea0699-c70a-446c-80d5-84edc29bb097)(label(shade))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         2ba7cb8c-5e81-4dca-b02c-bd34fe532fdb)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         1fba0891-761e-4ff1-868d-2e3af36bd174)(content(Whitespace\"\\n\"))))(Tile((id \
         c146d7be-ef5b-4812-b571-97df409e899e)(label(int_of_float))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         02b436cb-8096-41e7-b506-15749de30887)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         d8e69cc8-816c-45c0-aa5b-bdbbefc77b64)(label(shade_adj))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         1bb93be7-775b-4451-ba03-17f02113061f)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         37d83d61-8931-4af0-8491-02a5f03be980)(content(Whitespace\"\\n\"))))(Secondary((id \
         2816da98-a971-46ef-9972-848871f07190)(content(Whitespace\"\\n\"))))(Tile((id \
         d3b3ae65-2fd2-4b19-b6f3-2de68001b473)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         f2ddfd02-38b7-45c1-a529-5425cb85a406)(content(Whitespace\" \
         \"))))(Tile((id \
         dfa7eb7a-dcc5-456d-b782-de94afbab956)(label(daily_water))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         45d0363d-954d-4a7b-821d-00ab2e531fff)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         240552b1-da92-47d2-bc3f-ca641fde084a)(label(250))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         a1fd916d-9782-487c-ac25-55404839aca6)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         e577ef8c-7677-4841-a350-515881b81826)(content(Whitespace\" \
         \"))))(Tile((id \
         9fc60bbd-6980-4ec8-957b-c10009002fe1)(label(2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         7f5745c9-9c05-4522-8f1e-c257e00a3037)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         06309273-ac08-4976-9a52-ca2360ce64fe)(content(Whitespace\" \
         \"))))(Tile((id \
         7b86324f-66c7-4c52-a658-58d9a771eaa3)(label(Full))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         0cde79aa-dc84-4c62-867e-fe60550b6bc2)(content(Whitespace\" \
         \"))))(Tile((id \
         68d5d146-5fa9-4c19-bc30-26ee1a54aaf9)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         ed0fd583-ca04-4391-8a97-bddd710f8dff)(content(Whitespace\" \
         \"))))(Tile((id \
         8ccd8e4a-e9d1-49d6-b8d2-c447f1218c56)(label(165))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         dae145ec-96b3-49fa-8bf5-ebf202a7df65)(content(Whitespace\" \
         \")))))))))(Tile((id \
         6486e05e-f31b-4ffb-a9fb-f2dc5ae134cd)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         435aa646-7b09-4752-8ace-57e4ccd63678)(content(Whitespace\"\\n\"))))(Tile((id \
         22c55e3d-bf55-479c-ad1d-ec4d550b2199)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         b8da5d13-24e1-4f61-9bb0-6af751e2dd78)(content(Whitespace\" \
         \"))))(Tile((id \
         fbec2348-10c6-4697-9f53-e00619876960)(label(daily_water))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         20cfd17f-7f44-4432-9402-d0b42426bc15)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         8f96bbda-7619-4126-82aa-bb9f0c39f755)(label(50))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         bdc5eaec-482d-4023-976e-86b2b49d3b0b)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         e1f43d4d-e888-417a-8877-3b5253332d30)(content(Whitespace\" \
         \"))))(Tile((id \
         84bd377c-a91f-48b7-abc7-42a92b7e2601)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         1316ecab-9272-4556-87eb-8b2ce142e9c1)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         3fe3d39d-7fcb-4084-acb0-de0330709039)(content(Whitespace\" \
         \"))))(Tile((id \
         0ff4d3f3-acc0-44b9-976a-25b6ab81be65)(label(New))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         654f8a04-1232-446d-a463-9365b205179c)(content(Whitespace\" \
         \"))))(Tile((id \
         fffd55bb-929e-4879-b2fd-62940fa9d6a5)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         4912bee6-1b95-48df-872a-529f388f5fa1)(content(Whitespace\" \
         \"))))(Tile((id \
         0f30af4e-9cfc-4d9d-8665-56d02bd65efe)(label(60))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         144baefc-0204-45b7-a302-09e3f8c181b6)(content(Whitespace\" \
         \")))))))))(Tile((id \
         b5fa01dd-7400-47e6-86de-effe89751d88)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         67b2d41c-46bc-4f8c-a0ef-e0c44b83353d)(content(Whitespace\"\\n\"))))(Tile((id \
         b19a321b-c25b-4071-91a9-16f731f00890)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         114aa6eb-8175-4f65-a325-9a4f96f0c3a8)(content(Whitespace\" \
         \"))))(Tile((id \
         abd68ae3-ba9e-4948-b553-f031f7175b58)(label(daily_water))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         dbd7f14b-5fd7-491e-a7a3-5b55e8b24239)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         1e5d0605-60e8-44b3-97ef-73151de421e9)(label(180))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         034b6bd3-9f01-4860-844b-0e10ff321a80)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         193715f0-4443-4b74-bab4-0ba6ddaa609d)(content(Whitespace\" \
         \"))))(Tile((id \
         d8465336-3368-4099-83f2-e1b356e67bfd)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         35fcddf8-25e1-400a-aed6-cc77ed4dfcfb)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         29e9cd3b-abbb-4907-b081-900db38ba714)(content(Whitespace\" \
         \"))))(Tile((id \
         1fc159a3-17fe-4688-b8d8-6d3944b339ab)(label(Waning))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         3efb5122-cac6-4142-ae7f-3afb3fd5f021)(content(Whitespace\" \
         \"))))(Tile((id \
         bf479eef-51fd-46ef-b4ff-161f4acb6cfd)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         dfb8f580-b292-4111-87a0-8efab513dd05)(content(Whitespace\" \
         \"))))(Tile((id \
         d8a232a7-5511-4121-90d0-0a077a7e70ea)(label(171))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         23c1d229-9cf8-4090-a177-956768df23b2)(content(Whitespace\" \
         \")))))))))(Tile((id \
         bb675efe-ae4e-48f5-966e-056b7a6cc6a8)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         11d08d46-eb45-4039-ac29-22170de8b7d0)(content(Whitespace\"\\n\"))))(Tile((id \
         9f65589c-90f7-446a-9877-6f6bd79b78aa)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         fc68cb7d-60ff-487e-918a-e8a470d79224)(content(Whitespace\" \
         \"))))(Tile((id \
         5fdedfc3-ef91-449b-8ea0-61f79ca84928)(label(daily_water))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         b05b87cc-4b76-426d-82bf-000ba27bf1f2)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         1420cae8-a2fd-40a7-8186-3d861fcda174)(label(200))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         a3fb63a7-9a60-45eb-bec2-972bcdc8a142)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         74b7df5d-3c44-4264-a279-9845c3889c07)(content(Whitespace\" \
         \"))))(Tile((id \
         78fd02d2-9f5f-48a7-b20e-cba5049d245d)(label(2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         81d8e215-3d77-411a-a58a-88c410d73be6)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         2c5aa460-00a3-4241-a76e-f92cc9950143)(content(Whitespace\" \
         \"))))(Tile((id \
         cf2e30a9-421b-428c-9529-827509a7b054)(label(Waxing))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         ce2fdc43-07c3-47ef-beed-24ea3fad5f9e)(content(Whitespace\" \
         \"))))(Tile((id \
         c5bd95ab-0735-43d7-9411-67fb4f8f51c2)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         a740453b-2734-433c-a3a4-00731cb86804)(content(Whitespace\" \
         \"))))(Tile((id \
         82c368d8-b72f-4089-b9e8-42783caf45b4)(label(165))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         21c5db9a-d871-4379-b3c8-9fded5f772f1)(content(Whitespace\" \
         \")))))))))(Tile((id \
         2078abe6-9aac-4543-b1a5-3d809fa68c5a)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         6bb65ea0-5259-42dc-aa3f-1979efa80bf7)(content(Whitespace\"\\n\"))))(Secondary((id \
         74fee978-77c6-49e6-8fed-5ba037aa6e22)(content(Whitespace\"\\n\"))))(Tile((id \
         3baa4052-ee47-4341-ad2b-b3a3bd1a92cb)(label(daily_water))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         8b9ca123-2d53-49eb-9983-d341fc7005fa)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         9793dba2-4737-4b52-ac08-c0b024bfeba0)(label(100))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         c5abf065-c0a4-4af8-a7fd-064996aa2472)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         6b487306-daab-47d9-8227-749b7cf9c191)(content(Whitespace\" \
         \"))))(Tile((id \
         7acff7a5-e924-4ef3-b946-7427b950d7d9)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         5dd573d5-d8b0-4ed8-bc01-8738346b7c9f)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         80379b4d-859b-4269-9e57-a4fbb915b5cc)(content(Whitespace\" \
         \"))))(Tile((id \
         858111e0-9915-4872-8a07-71b05ad3ab88)(label(Full))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         795f0c65-c2dd-4dc6-8e3f-595bcbe767a8)(content(Whitespace\"\\n\"))))(Secondary((id \
         413754c4-f565-4285-9813-835f4ace53f3)(content(Whitespace\"\\n\"))))(Secondary((id \
         9a6ae9eb-b3fe-4b16-9937-4710691cc1c9)(content(Comment\"# END OF PART \
         5 - Select the next slide from the top menu #\"))))(Secondary((id \
         c5a37539-da12-475a-8cb8-d45e5e044214)(content(Whitespace\"\\n\")))))";
      backup_text =
        "# PROBES TUTORIAL - PART 5: STEP INTO AND THE DYNAMIC CURSOR BAR #\n\
         # You've pinned a call and can see values inside a function. #\n\
         # But what if the bug is deeper, inside a function that your #\n\
         # function calls? Step Into follows the call stack down. #\n\n\
         # TRY THIS: #\n\
         # 1. Turn on auto-probe and click inside `daily_water` #\n\
         # 2. Pin one of the test calls (click a sample > Pin) #\n\
         # 3. Now add a probe to the `phase_multiplier(phase)` call #\n\
         #    inside `daily_water` (click on `phase_multiplier`) #\n\
         # 4. Click that sample and choose \"Step Into\" from the #\n\
         #    dropdown (or press Enter) #\n\
         # 5. Your cursor jumps into `phase_multiplier`! The probes #\n\
         #    there show only values from your pinned context. #\n\n\
         # THE DYNAMIC CURSOR BAR #\n\
         # Look at the bar at the top of the editor after stepping in. #\n\
         # It shows your position in the call stack as breadcrumbs: #\n\
         #   top-level > daily_water > phase_multiplier #\n\
         # Click a function name to jump to its definition. #\n\
         # Click a chevron (>) to jump to the call site. #\n\
         # This lets you move up and down the call stack freely. #\n\n\
         type MoonPhase = + New + Waxing + Full + Waning in\n\n\
         let phase_multiplier: MoonPhase -> Float =\n\
         fun phase -> case phase\n\
         | New => 1.2\n\
         | Waxing => 1.1\n\
         | Full => 0.88\n\
         | Waning => 0.95\n\
         end\n\
         in\n\n\
         let shade_multiplier: Int -> Float =\n\
         fun shade -> case shade\n\
         | 0 => 1.0\n\
         | 1 => 0.9\n\
         | 2 => 0.75\n\
         | _ => 0.6\n\
         end\n\
         in\n\n\
         let daily_water: (Int, Int, MoonPhase) -> Int =\n\
         fun (base, shade, phase) ->\n\
         let base_f = float_of_int(base) in\n\
         let phase_adj = base_f *. phase_multiplier(phase) in\n\
         let shade_adj = phase_adj *. shade_multiplier(shade) in\n\
         int_of_float(shade_adj)\n\
         in\n\n\
         test daily_water(250, 2, Full) == 165 end;\n\
         test daily_water(50, 0, New) == 60 end;\n\
         test daily_water(180, 0, Waning) == 171 end;\n\
         test daily_water(200, 2, Waxing) == 165 end;\n\n\
         daily_water(100, 1, Full)\n\n\
         # END OF PART 5 - Select the next slide from the top menu #\n";
      refractors = "()";
    } )
