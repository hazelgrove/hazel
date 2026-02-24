let out : string * Haz3lcore.PersistentSegment.t =
  ( "Study / tutorial / 05-step-into",
    {
      segment =
        "((Secondary((id \
         046612fa-08ad-4fa7-b4b3-d78f1b9ec107)(content(Comment\"# PROBES \
         TUTORIAL - PART 5: STEP INTO AND THE DYNAMIC CURSOR BAR \
         #\"))))(Secondary((id \
         ff0e28a9-359f-42c7-83b8-0ecc35612b64)(content(Whitespace\"\\n\"))))(Secondary((id \
         51efe111-9df1-4aee-9ca9-8a2039c382a8)(content(Comment\"# You've \
         pinned a call and can see values inside a function. \
         #\"))))(Secondary((id \
         dbdfb239-6f04-4bdd-b050-799ad7e43ea9)(content(Whitespace\"\\n\"))))(Secondary((id \
         08b7d44d-97a6-4a0e-b3cb-985619a9ee80)(content(Comment\"# But what if \
         the bug is deeper, inside a function that your #\"))))(Secondary((id \
         b708642e-497f-4e76-ba93-48862151ddf4)(content(Whitespace\"\\n\"))))(Secondary((id \
         39fa5502-b6f0-411b-93cf-ca730495a33c)(content(Comment\"# function \
         calls? Step Into follows the call stack down. #\"))))(Secondary((id \
         0655e0e1-19b1-4148-9797-9083ddaed8ec)(content(Whitespace\"\\n\"))))(Secondary((id \
         3704e8d0-7566-4b89-a1d9-c775ca650143)(content(Whitespace\"\\n\"))))(Secondary((id \
         0e109320-3d20-4ac1-a16e-6f8aebbe2d83)(content(Comment\"# TRY THIS: \
         #\"))))(Secondary((id \
         6117a9b8-8c1f-4471-998a-f628ee9b42e9)(content(Whitespace\"\\n\"))))(Secondary((id \
         ab605b91-dbfb-43a6-abd8-ee87ee6edd5e)(content(Comment\"# 1. Turn on \
         auto-probe and click inside `daily_water` #\"))))(Secondary((id \
         6ae80964-0640-4353-b63d-b7f2ee0f31ae)(content(Whitespace\"\\n\"))))(Secondary((id \
         aeff776e-58bc-4cea-8eb9-841169a97471)(content(Comment\"# 2. Pin one \
         of the test calls (click a sample > Pin) #\"))))(Secondary((id \
         85c6c939-022c-488d-88f4-faf287beb152)(content(Whitespace\"\\n\"))))(Secondary((id \
         b6687b53-02e4-4399-9ad5-ab94bf3e568f)(content(Comment\"# 3. Now add a \
         probe to the `phase_multiplier(phase)` call #\"))))(Secondary((id \
         f282fc33-3a06-4d5b-84fc-0d918699e6d0)(content(Whitespace\"\\n\"))))(Secondary((id \
         51328ed4-963f-429e-9410-d8ab876fd4bf)(content(Comment\"#    inside \
         `daily_water` (click on `phase_multiplier`) #\"))))(Secondary((id \
         af53f97d-29fd-4809-8b4a-8d76660d8b25)(content(Whitespace\"\\n\"))))(Secondary((id \
         e3d3541f-e99f-4c59-b7ad-36179be4be63)(content(Comment\"# 4. Click \
         that sample and choose \\\"Step Into\\\" from the \
         #\"))))(Secondary((id \
         05766aea-1628-4ee2-81b5-e2eeb218939d)(content(Whitespace\"\\n\"))))(Secondary((id \
         93594f8c-048a-4dda-9f7e-778109c178df)(content(Comment\"#    dropdown \
         (or press Enter) #\"))))(Secondary((id \
         e7d4c0a1-3ec1-4f13-9ed4-656905457594)(content(Whitespace\"\\n\"))))(Secondary((id \
         75639dec-6768-410d-b32c-72b4e707410c)(content(Comment\"# 5. Your \
         cursor jumps into `phase_multiplier`! The probes \
         #\"))))(Secondary((id \
         da6777b0-d719-4dd5-b714-1c41706d5004)(content(Whitespace\"\\n\"))))(Secondary((id \
         d72fdc6b-18f6-46a6-afa7-ea8711130c8f)(content(Comment\"#    there \
         show only values from your pinned context. #\"))))(Secondary((id \
         e5c6f16d-5808-4f9a-9ff2-05f70ae96683)(content(Whitespace\"\\n\"))))(Secondary((id \
         38036883-cfba-4de5-bd99-590135b3f25c)(content(Whitespace\"\\n\"))))(Secondary((id \
         873d086c-c63d-4743-824f-7a967996d20f)(content(Comment\"# THE DYNAMIC \
         CURSOR BAR #\"))))(Secondary((id \
         689d66fb-0838-4706-a133-30779b9da6f0)(content(Whitespace\"\\n\"))))(Secondary((id \
         2a7eba0f-e05c-4b74-9b72-235fa753562b)(content(Comment\"# Look at the \
         bar at the top of the editor after stepping in. #\"))))(Secondary((id \
         ab5828b6-a2ec-4ca2-a3dc-8889f2cb2fd3)(content(Whitespace\"\\n\"))))(Secondary((id \
         b8fdaf9a-182e-4022-80f4-7286bd94a7dc)(content(Comment\"# It shows \
         your position in the call stack as breadcrumbs: #\"))))(Secondary((id \
         b2302cf7-7e11-43ad-aa40-271823e9fa1b)(content(Whitespace\"\\n\"))))(Secondary((id \
         a0b4504a-f479-48ff-8ecd-7661dd7af5a0)(content(Comment\"#   top-level \
         > daily_water > phase_multiplier #\"))))(Secondary((id \
         83fe7bf0-1557-499c-827f-11d66dc77121)(content(Whitespace\"\\n\"))))(Secondary((id \
         8d0e55a2-e665-4071-b4c0-96863f37c33a)(content(Comment\"# Click a \
         function name to jump to its definition. #\"))))(Secondary((id \
         818a537b-e5b2-4b34-9888-3612f32417d0)(content(Whitespace\"\\n\"))))(Secondary((id \
         55d33b9d-41c2-4b68-9230-e091ed422a1b)(content(Comment\"# Click a \
         chevron (>) to jump to the call site. #\"))))(Secondary((id \
         388c0e94-a858-43e5-a936-ebb249c0f2f8)(content(Whitespace\"\\n\"))))(Secondary((id \
         01c048aa-01e4-422d-ae68-5084c37ff3e8)(content(Comment\"# This lets \
         you move up and down the call stack freely. #\"))))(Secondary((id \
         d5160d3a-c64b-44c6-ba51-fe5b93137c2f)(content(Whitespace\"\\n\"))))(Secondary((id \
         4131fe27-af7b-42a9-9335-8daccae2010d)(content(Whitespace\"\\n\"))))(Tile((id \
         42c3dbae-131c-47cf-8b40-875324cf8124)(label(type = in))(mold((out \
         Exp)(in_(TPat Typ))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         66adc2d1-44c8-462f-a26d-c49bb236dc68)(content(Whitespace\" \
         \"))))(Tile((id \
         0aa389cf-036a-4eb8-94c0-225c55566b34)(label(MoonPhase))(mold((out \
         TPat)(in_())(nibs(((shape Convex)(sort TPat))((shape Convex)(sort \
         TPat))))))(shards(0))(children())))(Secondary((id \
         835cf996-336d-4fe1-970e-7fbe37842227)(content(Whitespace\" \
         \")))))((Secondary((id \
         0c702540-9cda-48b6-9fad-b7e571ffa25c)(content(Whitespace\" \
         \"))))(Tile((id \
         961e47f2-a02e-4fb8-b085-6a54c3f8a177)(label(+))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape(Concave 33))(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         0f7edcc3-71fc-4f7d-be31-abf604356184)(content(Whitespace\" \
         \"))))(Tile((id \
         c11a866d-f2d6-4041-a479-3c138b3e1a4c)(label(New))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         313a9e47-ef49-42d4-b7f3-6458b4a419ed)(content(Whitespace\" \
         \"))))(Tile((id \
         0aae11a2-b661-42ff-895b-b87abe303e5e)(label(+))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 12))(sort Typ))((shape(Concave \
         12))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         c0184014-c686-40d9-b1d9-1fee86d6f2d2)(content(Whitespace\" \
         \"))))(Tile((id \
         bf9232ad-8501-481b-8d7b-d798fa25d50b)(label(Waxing))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         09c9d857-713e-4480-ad93-f1e8097cf9f4)(content(Whitespace\" \
         \"))))(Tile((id \
         ed128427-7aa2-4fee-aed7-ca8fe3dc67a0)(label(+))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 12))(sort Typ))((shape(Concave \
         12))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         ed25b528-71be-488c-8044-bfbef557bdd8)(content(Whitespace\" \
         \"))))(Tile((id \
         5cd8aa61-8f53-4370-acfe-c453f9fbeab1)(label(Full))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         f1f86e03-1832-47bf-802d-feb4a0c656b1)(content(Whitespace\" \
         \"))))(Tile((id \
         6915de16-dea3-4f70-b754-8d8e19672428)(label(+))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 12))(sort Typ))((shape(Concave \
         12))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         4a305590-6b23-4db4-bc66-b51462d9d641)(content(Whitespace\" \
         \"))))(Tile((id \
         1ea77f3d-6fd4-4b9b-9ff8-04407aaaa0f7)(label(Waning))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         8434baee-f3b3-4bcd-8eb2-b395731e602c)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         96fbd2d5-a1d1-4c07-a530-c1ab80ac0e74)(content(Whitespace\"\\n\"))))(Secondary((id \
         8edfed95-6ac5-4269-88f6-86d7450d6f33)(content(Whitespace\"\\n\"))))(Tile((id \
         1ca5febd-76ca-426f-8733-2a26ac979062)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         c6d124d0-1f38-4122-81ee-d766cd48a03e)(content(Whitespace\" \
         \"))))(Tile((id \
         1a9f01d9-4921-4d09-8cca-a1e557ae2dcb)(label(phase_multiplier))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         735ef83e-951f-4f68-a0c8-d609cd1ab007)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 24))(sort Pat))((shape(Concave \
         24))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         8630f473-32e1-4dc1-be96-cab812e8504a)(content(Whitespace\" \
         \"))))(Tile((id \
         f157927a-50a6-4bd8-a4c1-9ca232d6caeb)(label(MoonPhase))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         10082171-fd67-4567-b3b8-e006afb29ce5)(content(Whitespace\" \
         \"))))(Tile((id \
         fb7d8a1c-06f9-4a67-9ee9-bff9c20446c2)(label(->))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 13))(sort Typ))((shape(Concave \
         13))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         80d60cc5-174d-4d00-9235-f4db7c201e8b)(content(Whitespace\" \
         \"))))(Tile((id \
         7a2fe88b-759d-444a-b499-b2bce13ac791)(label(Float))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         9d751d26-d9bd-40f9-baba-aadfe2e2b6a2)(content(Whitespace\" \
         \")))))((Secondary((id \
         32aff303-a454-4bb6-80ff-d6c66465830c)(content(Whitespace\"\\n\"))))(Tile((id \
         cc7db8c5-3a9d-4321-acb9-ef2aecd24b04)(label(fun ->))(mold((out \
         Exp)(in_(Pat))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         37))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         4a1bcacc-509c-478d-8527-3040844ed824)(content(Whitespace\" \
         \"))))(Tile((id \
         5fb61b85-1063-41ee-96bc-e0752107507a)(label(phase))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         70a53b0a-cfc6-4dbb-89cc-25f29a1266cc)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         f5b632c8-1eed-4ab7-8413-f8067937f00b)(content(Whitespace\" \
         \"))))(Tile((id cce7b77c-667d-4ab7-b654-8d42e1c8f68f)(label(case \
         end))(mold((out Exp)(in_(Rul))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         27e0973a-7c0c-4210-9a89-7cc6e1207b83)(content(Whitespace\" \
         \"))))(Tile((id \
         47535fa5-e171-4ed6-b470-04fb27943c70)(label(phase))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         071cb2cb-2567-4adc-a2d0-eb128bf59206)(content(Whitespace\"\\n\"))))(Tile((id \
         2e504d39-efc5-4236-89ef-85c29a3ba9cf)(label(| =>))(mold((out \
         Rul)(in_(Pat))(nibs(((shape(Concave 46))(sort Exp))((shape(Concave \
         46))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         20e80982-73f9-48fe-b6af-2318048e313d)(content(Whitespace\" \
         \"))))(Tile((id \
         d36d9dd9-ab43-4399-9f75-8683923c5988)(label(New))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         fd002b4d-da0f-4d83-9a0e-ea8707f7bc8d)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         08e91b63-0a27-48a4-bb6e-fcf881a10299)(content(Whitespace\" \
         \"))))(Tile((id \
         9cfdbd43-4a33-4f79-81f0-5486b93e4ab5)(label(1.2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         d78ef324-846b-4655-9d25-659fe07e99c2)(content(Whitespace\"\\n\"))))(Tile((id \
         6469d48f-58da-4153-8319-736f6f847763)(label(| =>))(mold((out \
         Rul)(in_(Pat))(nibs(((shape(Concave 46))(sort Exp))((shape(Concave \
         46))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         b0902a10-c839-4948-9c1b-e47636e34380)(content(Whitespace\" \
         \"))))(Tile((id \
         ba167f22-ebc0-4f92-8455-b12e0196a820)(label(Waxing))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         25e136ea-d58f-4b13-a718-67d09fd671ce)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         f55eaf36-c5f1-4c92-b2b3-13d57ede09a8)(content(Whitespace\" \
         \"))))(Tile((id \
         ccc9050e-d65d-4482-b917-718d8e7cdf07)(label(1.1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         4a722981-26dc-4aa0-bc84-7309b9779444)(content(Whitespace\"\\n\"))))(Tile((id \
         f418156c-706f-424a-9d5f-ff5337de58ab)(label(| =>))(mold((out \
         Rul)(in_(Pat))(nibs(((shape(Concave 46))(sort Exp))((shape(Concave \
         46))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         a6fde862-685f-4419-988e-3b46cb942eb1)(content(Whitespace\" \
         \"))))(Tile((id \
         a9ec8243-f09c-401f-8c35-c13377014d55)(label(Full))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         6021eecf-28ba-485d-9027-e06f2aa8b49a)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         cf88120e-9075-4066-aedb-2ded4d25b33f)(content(Whitespace\" \
         \"))))(Tile((id \
         5d2f0597-43a3-4a0c-ad27-173069255286)(label(0.88))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         683cd2ff-337e-4075-a94e-55f604037f3f)(content(Whitespace\"\\n\"))))(Tile((id \
         f7c08ecc-fa7c-485c-8b33-19b38313d251)(label(| =>))(mold((out \
         Rul)(in_(Pat))(nibs(((shape(Concave 46))(sort Exp))((shape(Concave \
         46))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         46db0869-3c60-4820-bdda-8c2a838018c1)(content(Whitespace\" \
         \"))))(Tile((id \
         b626d622-d540-48bb-8c7d-fe61726abdb2)(label(Waning))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         ee4545c9-c436-40ea-bdbe-6562fe47e372)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         9d83d684-624d-4354-b34b-2d53b9ab0f54)(content(Whitespace\" \
         \"))))(Tile((id \
         e2037a61-c5cc-49e5-a317-89e189a33417)(label(0.95))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         ddffc7ca-c4c7-4dfc-a61f-24d9282540d2)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         7a7dfb8d-e385-48fa-8fdd-30ed7d09ac15)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         3063cce3-93ae-46a3-881b-c1524f6674a9)(content(Whitespace\"\\n\"))))(Secondary((id \
         11fcc175-be99-4298-8438-cdd8a85cfa61)(content(Whitespace\"\\n\"))))(Tile((id \
         10810447-10b0-4b74-994c-5c868e1cf9a9)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         0539b78e-004e-471f-82dc-582a5485edca)(content(Whitespace\" \
         \"))))(Tile((id \
         48d87744-34a6-4eb3-8970-a93db9e95b1f)(label(shade_multiplier))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         66e6e47e-57d6-44b1-91a1-0986276dc549)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 24))(sort Pat))((shape(Concave \
         24))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         61a11721-8f5e-4271-9acc-5969fa6514de)(content(Whitespace\" \
         \"))))(Tile((id \
         72cbadf0-bd36-46b4-85c2-649fe41ed79a)(label(Int))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         30db6e06-3c93-467a-94ea-fba8004f52d5)(content(Whitespace\" \
         \"))))(Tile((id \
         54f1e6c6-0d47-4d9e-ae29-c7ee418ce61c)(label(->))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 13))(sort Typ))((shape(Concave \
         13))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         0a67d48b-64d5-4fe1-a11b-3d9c36f85f26)(content(Whitespace\" \
         \"))))(Tile((id \
         51cae29d-c91c-479f-bda5-ec6dc42145a1)(label(Float))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         a6823152-6c67-42be-bb19-a7613d07823b)(content(Whitespace\" \
         \")))))((Secondary((id \
         eeec2325-3f62-4009-84b5-6748e577ac93)(content(Whitespace\"\\n\"))))(Tile((id \
         0f27bc04-c872-4450-920c-ba6bf51362b1)(label(fun ->))(mold((out \
         Exp)(in_(Pat))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         37))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         930779e7-648b-46df-afd2-dee95cd64153)(content(Whitespace\" \
         \"))))(Tile((id \
         f08d5d3e-17e1-4e7a-b34f-ee94d93c430b)(label(shade))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         256bc949-69f7-4cc7-b6fc-86c799b6a99c)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         1db129fd-6e53-40fd-bc7f-6afb3c3fd70a)(content(Whitespace\" \
         \"))))(Tile((id 457d25a5-bda5-44c4-88e9-734db51834f7)(label(case \
         end))(mold((out Exp)(in_(Rul))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         e3c462dc-84de-49bb-bc42-79e12c7155f6)(content(Whitespace\" \
         \"))))(Tile((id \
         a8448d99-f7c6-4297-a3aa-3a503bad8798)(label(shade))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         370d4aac-ed29-46fa-a47c-b3d04719a8b3)(content(Whitespace\"\\n\"))))(Tile((id \
         a3463e3f-57f6-485a-ad84-f305a6bdcafb)(label(| =>))(mold((out \
         Rul)(in_(Pat))(nibs(((shape(Concave 46))(sort Exp))((shape(Concave \
         46))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         04fded71-1053-460b-9e67-b610b1ff47dd)(content(Whitespace\" \
         \"))))(Tile((id \
         88c227c8-f048-49d4-8ade-0ddc79ac4d93)(label(0))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         db7a7591-fd54-4ff0-ace4-de4ad05267a7)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         3dc7adac-dbbb-4173-bd3c-e3f0b83c1f8d)(content(Whitespace\" \
         \"))))(Tile((id \
         5160bb15-1676-4739-8eaf-6fd3c93678b3)(label(1.0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         d3860380-cd7f-4f0d-ad3d-69c7cab455c0)(content(Whitespace\"\\n\"))))(Tile((id \
         d74f9db9-80f5-4b80-825f-78bf265c86c0)(label(| =>))(mold((out \
         Rul)(in_(Pat))(nibs(((shape(Concave 46))(sort Exp))((shape(Concave \
         46))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         4420f952-0613-4204-8b9f-31c404111e75)(content(Whitespace\" \
         \"))))(Tile((id \
         19ad7764-9303-4e64-a2ab-7ac9672b20d8)(label(1))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         3b6aa2db-76ea-4315-82ef-77dfe8a88c7d)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         d26402dd-72b9-45cb-a84d-e4b0c025696d)(content(Whitespace\" \
         \"))))(Tile((id \
         fb276ec2-e4c5-4423-a89f-2e9658399e45)(label(0.9))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         bce5484b-43ef-4e33-b117-c7188cd2f409)(content(Whitespace\"\\n\"))))(Tile((id \
         2cb0a816-0e07-4889-b447-44b75db82d27)(label(| =>))(mold((out \
         Rul)(in_(Pat))(nibs(((shape(Concave 46))(sort Exp))((shape(Concave \
         46))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         7ee858e3-02dc-439e-be1b-7aa5c45cd903)(content(Whitespace\" \
         \"))))(Tile((id \
         ead8cbac-7c77-4e66-bc0a-10e78ffd85f3)(label(2))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         5d10e355-78bf-4304-a6f5-83e312ae4f6d)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         a557b349-eaab-4c49-b630-0b6a45bfdb28)(content(Whitespace\" \
         \"))))(Tile((id \
         6180d416-8c7c-47db-a137-e07b9f08ae8c)(label(0.75))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         4c884df8-e893-4b2c-b0da-b584eaa5a4cd)(content(Whitespace\"\\n\"))))(Tile((id \
         77e3babf-7412-4fce-806d-962837e941ca)(label(| =>))(mold((out \
         Rul)(in_(Pat))(nibs(((shape(Concave 46))(sort Exp))((shape(Concave \
         46))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         9920af20-6cb6-4d5e-a0c6-bed94eafabd6)(content(Whitespace\" \
         \"))))(Tile((id \
         26cef57f-816d-4cad-b498-b277834cc0db)(label(_))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         2b4837df-ce04-4788-8227-6c3bee9e48be)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         f6792f2a-939b-441e-bd15-2e083076dff7)(content(Whitespace\" \
         \"))))(Tile((id \
         29b47ce0-9124-498e-84bf-1547d71f2e79)(label(0.6))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         d877480f-c552-482f-9aaa-714beee09ded)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         f4de20b2-d7ec-498a-a0e6-9d87dbfec18e)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         6b755d60-bfa1-4481-812f-b89eeb19d0e2)(content(Whitespace\"\\n\"))))(Secondary((id \
         80d11f4a-9c5f-423b-8542-68af86d08560)(content(Whitespace\"\\n\"))))(Tile((id \
         26238c0e-3830-47b3-b8ce-f9e13256f19e)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         21902a49-8abe-4197-abe0-2dfb00e52c3d)(content(Whitespace\" \
         \"))))(Tile((id \
         bcc902a5-2419-4b10-84a4-ace7cb98c7eb)(label(daily_water))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         40e3ef55-e610-4d81-994d-adae2180318f)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 24))(sort Pat))((shape(Concave \
         24))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         82cecc06-843c-4186-bf59-89d509ce5507)(content(Whitespace\" \
         \"))))(Tile((id \
         09eafa89-679e-40f2-afab-8b0c270426b9)(label(\"(\"\")\"))(mold((out \
         Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0 1))(children(((Tile((id \
         9963a8d2-b7f9-4d80-8dff-730cbb01df3f)(label(Int))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         6859d66f-f7dd-4a7b-8f42-069132cdc31e)(label(,))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 44))(sort Typ))((shape(Concave \
         44))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         8bdefc8e-8e63-46fd-ac01-e46e41d33881)(content(Whitespace\" \
         \"))))(Tile((id \
         daedf0f6-c3dc-4cf8-84fc-a86a3f709d32)(label(Int))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         330477d2-f88f-4052-aa38-dedbce56c771)(label(,))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 44))(sort Typ))((shape(Concave \
         44))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         450905c0-13cb-435a-b288-79302472d9ec)(content(Whitespace\" \
         \"))))(Tile((id \
         ea56b189-5d15-4aa6-8656-3d81bf00bbc3)(label(MoonPhase))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children()))))))))(Secondary((id \
         9ea7d424-cc97-46e3-a69d-ff42cbe57b72)(content(Whitespace\" \
         \"))))(Tile((id \
         be23b21d-5ccb-4e57-97f7-48bf745bfc6e)(label(->))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 13))(sort Typ))((shape(Concave \
         13))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         ebb7d458-4658-457f-a67a-fc14c3c78b77)(content(Whitespace\" \
         \"))))(Tile((id \
         202e6069-d966-4b99-86ef-462d2d874673)(label(Int))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         d51d12d8-6a88-4f90-8e91-53616c67a191)(content(Whitespace\" \
         \")))))((Secondary((id \
         b78cb430-b45c-43ce-bba0-0259aa4673eb)(content(Whitespace\"\\n\"))))(Tile((id \
         33a2d680-16e7-4022-83dd-f640b0ce3450)(label(fun ->))(mold((out \
         Exp)(in_(Pat))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         37))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         50830c97-a329-4cc3-a568-abbbec4bb6f6)(content(Whitespace\" \
         \"))))(Tile((id \
         cf171859-ab2c-4d16-a94f-af7589af2731)(label(\"(\"\")\"))(mold((out \
         Pat)(in_(Pat))(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0 1))(children(((Tile((id \
         265e4660-e61a-4205-a400-604692869b0d)(label(base))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         2d7c0913-3ebd-4536-9657-12d42200a940)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 44))(sort Pat))((shape(Concave \
         44))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         23ee7364-f877-4ea5-abe7-d0a02f4bde02)(content(Whitespace\" \
         \"))))(Tile((id \
         063e43b5-d9c0-4f02-a2f5-0a5e81208a45)(label(shade))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         cdc179a7-4679-4e0d-b3c5-2fe191b6b4e9)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 44))(sort Pat))((shape(Concave \
         44))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         ad424082-5d35-4c75-bd97-edc2928aa364)(content(Whitespace\" \
         \"))))(Tile((id \
         bdd609c2-5b60-40e9-b91b-91b2f9131de0)(label(phase))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children()))))))))(Secondary((id \
         770f74dd-06be-4f29-8635-9be39207b680)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         341a893d-d385-402e-8576-09e0675b837c)(content(Whitespace\"\\n\"))))(Tile((id \
         99f5da85-6a37-406c-950e-5bcae68ac4b9)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         f93ed35d-8700-49e7-a02d-650d7df13715)(content(Whitespace\" \
         \"))))(Tile((id \
         e4997f6f-ae11-4690-957a-fce7bea2bad4)(label(base_f))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         ebb9acd1-65ed-4e94-9a60-5fe9ba45865a)(content(Whitespace\" \
         \")))))((Secondary((id \
         44caa586-37c3-4856-84e1-06847dcd86ac)(content(Whitespace\" \
         \"))))(Tile((id \
         3092ff85-81f1-4ba2-ba63-85f8ff43c115)(label(float_of_int))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         1d06060e-aed0-40e5-90bd-dbcd13367f51)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         91f6d134-e16d-4759-81b8-29c964fdb366)(label(base))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         3519fc8e-c23d-4352-b1e6-95c9ebaac4fd)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         06e80a6b-16be-4b25-a4a5-81f1656d08e6)(content(Whitespace\"\\n\"))))(Tile((id \
         6b4e3b6d-f8c2-47f6-b3ab-a72f2a47e7be)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         f17860f7-e9f0-40e7-a084-5062b1b803ca)(content(Whitespace\" \
         \"))))(Tile((id \
         3542bfc1-c531-448c-81b5-497e596076cc)(label(phase_adj))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         6f3fdce9-832b-4cd6-93e8-ec6f6c11e53f)(content(Whitespace\" \
         \")))))((Secondary((id \
         39e89202-d317-4082-a2a7-04662f8f7edc)(content(Whitespace\" \
         \"))))(Tile((id \
         3a44d327-40b9-4b3b-854d-4e938dd74031)(label(base_f))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         9f3391a8-de1b-47bd-a31a-7b9d89a0e6f8)(content(Whitespace\" \
         \"))))(Tile((id \
         682807e5-a2cb-48ee-bcd9-1196d272561d)(label(*.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 27))(sort Exp))((shape(Concave \
         27))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         a36e563b-92ad-46f7-be9c-afdc07abd1bb)(content(Whitespace\" \
         \"))))(Tile((id \
         944bc24e-db42-4fc9-b4eb-93c8df436511)(label(phase_multiplier))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         6453f086-efe3-4bd7-84a9-4c51376460e6)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         3c084e32-dc77-44ca-9d8b-737dc05e0c8e)(label(phase))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         e690f8c5-d8fc-47f3-a17e-de553ee1bc48)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         7bef2344-87c6-4ad7-a39e-1e67848c6c74)(content(Whitespace\"\\n\"))))(Tile((id \
         7d0b2d2b-0676-40c2-9a90-c3ca7e744d78)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         ff9a78c6-e4d7-42f9-bfe6-b7d55b05b018)(content(Whitespace\" \
         \"))))(Tile((id \
         ff12086b-88ea-4855-b820-397b707fe7e5)(label(shade_adj))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         d65d16c5-f4c5-46ce-aa2d-e6864dd25517)(content(Whitespace\" \
         \")))))((Secondary((id \
         b4bb941f-6d17-4cc8-b19e-057c2d32fef4)(content(Whitespace\" \
         \"))))(Tile((id \
         8eb30284-591e-43b7-9f58-062c2891d1bd)(label(phase_adj))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         8fc9ead2-e29d-4db0-ae7a-b80a71cda79f)(content(Whitespace\" \
         \"))))(Tile((id \
         f08197da-6dbc-4911-b73a-605800d757bf)(label(*.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 27))(sort Exp))((shape(Concave \
         27))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         3095f9d0-c1a0-45a6-8e4e-b9605c3b2de1)(content(Whitespace\" \
         \"))))(Tile((id \
         442c9373-fb1c-49ca-84c4-da4e0fccaeca)(label(shade_multiplier))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         9f7e37ec-820b-4df1-9afc-7df914b131aa)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         58cd85df-c874-41bc-8448-4a6cc77353bb)(label(shade))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         89b4042f-a3ac-4eb9-ac42-2dc4ed548a20)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         e50c2905-caaa-463a-83a2-c46968140090)(content(Whitespace\"\\n\"))))(Tile((id \
         f0c9dfc9-1cb6-4c3c-bac1-52e07d23b4c8)(label(int_of_float))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         9b66d057-0769-42c3-b9a1-0d31f4228c29)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         4e413098-2f31-4873-9884-0eb0b5f5b235)(label(shade_adj))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         34023178-7037-4f07-8f8b-503e60d5d5fa)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         27c8f793-9998-439d-ae13-be08c057c64e)(content(Whitespace\"\\n\"))))(Secondary((id \
         7548e501-0457-4cfc-994a-6b890ee0c0aa)(content(Whitespace\"\\n\"))))(Tile((id \
         776130f7-5cd9-4dd5-8c47-587e86bda2e3)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         f99c7ac2-6728-4957-ad88-aa2899684b3e)(content(Whitespace\" \
         \"))))(Tile((id \
         3336db76-4b37-4b7a-9614-357e04234381)(label(daily_water))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         0f67631f-09d2-45c1-853c-dde2f67b81fb)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         60a4e707-db38-4a95-a103-e7de4f96f36e)(label(250))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         a85d40c1-8549-4359-930e-9f7efd546473)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         72de5b2e-d719-40b7-b5f5-cc3c6cf3d466)(content(Whitespace\" \
         \"))))(Tile((id \
         2a700039-8671-4d40-8fe9-894868941c2e)(label(2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         1958b823-6768-43e6-b8e5-09ce28427677)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         5ac0f23a-e149-46b7-a22b-8460135e0616)(content(Whitespace\" \
         \"))))(Tile((id \
         bab71bde-dff4-4eab-bd36-6069b2b573d1)(label(Full))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         aea44ea0-2657-47a8-82cb-f51efa6fbfa2)(content(Whitespace\" \
         \"))))(Tile((id \
         a93b1444-b826-4095-b364-de8a6459d3a4)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         a938eae0-1074-466b-be19-3f71cdc1b971)(content(Whitespace\" \
         \"))))(Tile((id \
         6e846c3c-70f9-419a-8553-35c31dcf1a32)(label(165))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         dd85df98-3b4c-4354-b4aa-0d3f7d6793cd)(content(Whitespace\" \
         \")))))))))(Tile((id \
         12356729-ed8a-4458-b212-6cf1665e2ba5)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         b3f1b182-895a-44ab-b053-bd2a36c313d3)(content(Whitespace\"\\n\"))))(Tile((id \
         20e6edcb-db29-48f1-953b-e37fc39d8557)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         e946bf34-de46-4ce1-8b4e-82b851298fd7)(content(Whitespace\" \
         \"))))(Tile((id \
         aec2078c-5f0e-460e-a57d-226eecbd2582)(label(daily_water))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         24fa7c52-65c0-44bc-bcd0-ff122f336d94)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         f68c058a-9288-4b61-a87b-f76d74139092)(label(50))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         7c8d7168-bb50-45ea-a579-448186f707aa)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         10052a39-e368-4303-b899-de61520e89fd)(content(Whitespace\" \
         \"))))(Tile((id \
         1a9258ff-4966-4abe-985c-3918ddd3a858)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         e67dbad8-b568-4b14-b898-e81d989f9464)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         231f74e9-16d8-4b72-bf85-5a2e0d0abf71)(content(Whitespace\" \
         \"))))(Tile((id \
         4fc1c315-fd19-4522-854c-4eff824c5706)(label(New))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         6076dbd7-65c3-48be-9f6e-5917ff48c6c8)(content(Whitespace\" \
         \"))))(Tile((id \
         e558e97e-244a-41f4-87e4-f33f61e0b6e6)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         5e586835-b1dd-44fa-bd47-12f4443ed5fd)(content(Whitespace\" \
         \"))))(Tile((id \
         d6f68024-aebc-4929-b257-0b9459c1bbc5)(label(60))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         02c6b61d-9b5d-4962-b710-5c41387015ef)(content(Whitespace\" \
         \")))))))))(Tile((id \
         a5e55552-4cba-4c38-8b29-1e28a0cd31cf)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         2340bd66-4bca-47b3-b796-15cf55892e58)(content(Whitespace\"\\n\"))))(Tile((id \
         7e70ccb1-3b23-4d83-b247-c015cf3cd5f3)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         3f96f9c9-f6bc-4881-8691-ac370aa2fa16)(content(Whitespace\" \
         \"))))(Tile((id \
         c69bcb7a-f73f-4925-bb53-c64f11ed7e84)(label(daily_water))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         3c9f3ee2-f06e-4275-a83d-d4b24fc79b15)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         3a44a52b-3d44-4fe9-96f5-b7bf52202994)(label(180))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         f7eb3bde-1d06-4767-a0c3-ffd7f0a27563)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         c743702b-bd4b-43bd-86a3-3c53393c0a21)(content(Whitespace\" \
         \"))))(Tile((id \
         914bc9fd-fb44-486b-9242-3b4950ff7bc5)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         0bb6b0d1-bced-4c68-b55e-60324310b0d2)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         4b0e1d0b-5263-4da1-a6ec-4b2e635ac355)(content(Whitespace\" \
         \"))))(Tile((id \
         ac316ddf-928d-49d2-aa0c-193b19ae3365)(label(Waning))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         df91e45b-f7a1-4453-b181-652983ffcdfe)(content(Whitespace\" \
         \"))))(Tile((id \
         8e1d703c-be27-4385-8b05-5120762fa444)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         292ec27b-a03f-43c5-a58d-e7381a14775b)(content(Whitespace\" \
         \"))))(Tile((id \
         91891928-9473-46f5-b6ab-820b214d0b91)(label(171))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         06b11152-3284-4ca6-a8df-1d5c3e106387)(content(Whitespace\" \
         \")))))))))(Tile((id \
         38ec3ff2-0d89-43c5-aa80-246d5fd4a150)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         25a84484-3f4b-45ab-b826-1c0aef717e77)(content(Whitespace\"\\n\"))))(Tile((id \
         427d2569-1f8e-4714-a5e3-deb62d47cbc0)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         12ecf102-002e-41a9-b0f9-9b33f15abf64)(content(Whitespace\" \
         \"))))(Tile((id \
         a1686f5f-035d-424e-a73e-7c288594a863)(label(daily_water))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         75340402-884b-4b95-ac3a-e3a5ad6e8b3b)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         5ac37fb6-5ff9-4234-a655-80114462d7c5)(label(200))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         5b34ba97-a9bc-46e5-899a-da43b17f1123)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         20d6868f-890e-4ca3-bdf4-822bda8c0fe4)(content(Whitespace\" \
         \"))))(Tile((id \
         a2a8235a-5608-4696-85da-825981f3da7b)(label(2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         5a3a8e1d-9785-4f22-9ef4-a096a2b6b282)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         82983db8-dc47-462b-bae7-9816d0b296e0)(content(Whitespace\" \
         \"))))(Tile((id \
         bc220d09-f7b3-430b-8a77-c0de0314923a)(label(Waxing))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         66782cfa-ca35-4c73-96a3-5dbe2a4b72e3)(content(Whitespace\" \
         \"))))(Tile((id \
         fb38b7b7-9ca3-4df1-908d-45eaeb3e8124)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         888ef97c-93dc-4f7b-bde6-36d6a784daf3)(content(Whitespace\" \
         \"))))(Tile((id \
         14d57b21-bca9-4079-8dbe-3dd8ba257fa7)(label(165))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         86e1b368-7fa5-46f4-ada0-31b1ec0ac103)(content(Whitespace\" \
         \")))))))))(Tile((id \
         bc5a2918-8ab9-4625-a472-66d733400beb)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         dd662ef9-c9d4-4b3f-ac4a-63223931356d)(content(Whitespace\"\\n\"))))(Secondary((id \
         a74ecdc6-5e28-454b-81a2-464a867dbea1)(content(Whitespace\"\\n\"))))(Tile((id \
         635524ce-2b4a-4e29-baee-6b74af294a7e)(label(daily_water))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         fbd744b0-8026-4076-9860-f133306c7ca4)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         29b1386c-acb9-484a-b074-26f6f2df4425)(label(100))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         749cf3a4-8b60-4a71-a298-032af88e50f1)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         53600c22-664a-45e2-b897-fb4c3deef012)(content(Whitespace\" \
         \"))))(Tile((id \
         b30ace89-1f8a-4f23-8b22-5db439807883)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         a7484295-1ba7-4e78-b926-fd53eb6ffb35)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         fe6b8227-6be3-4722-92de-a1e9f18c752a)(content(Whitespace\" \
         \"))))(Tile((id \
         bce8772b-e376-432b-80fb-a7dfeb3dd9b9)(label(Full))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         fd23610a-0608-4e38-ab1b-80158714443c)(content(Whitespace\"\\n\"))))(Secondary((id \
         9bcf5684-7903-4214-b4ca-c3d034f8998a)(content(Whitespace\"\\n\"))))(Secondary((id \
         ae978a8b-35e1-478e-9544-8e68faedde5d)(content(Comment\"# END OF PART \
         5 - Select the next slide from the top menu #\"))))(Secondary((id \
         b3fbfd53-d433-4d40-aebc-9e9bb2b05c83)(content(Whitespace\"\\n\")))))";
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
