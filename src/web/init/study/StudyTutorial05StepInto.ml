let out : string * Haz3lcore.PersistentSegment.t =
  ( "Study / tutorial / 05-step-into",
    {
      segment =
        "((Secondary((id \
         ebf697c2-0dd0-44cd-b34f-09ed9fc003ae)(content(Comment\"# PROBES \
         TUTORIAL - PART 5: STEP INTO AND THE DYNAMIC CURSOR BAR \
         #\"))))(Secondary((id \
         613e3a69-5903-43cb-b84b-7ab8ee88b300)(content(Whitespace\"\\n\"))))(Secondary((id \
         b999713a-089c-4cc0-9133-6781e6d8024c)(content(Comment\"# You've \
         pinned a call and can see values inside a function. \
         #\"))))(Secondary((id \
         b384f631-b5c6-49b5-83ea-d46ef3f43349)(content(Whitespace\"\\n\"))))(Secondary((id \
         73d2bc20-551d-4c00-a87d-cb6dab514cf6)(content(Comment\"# But what if \
         the bug is deeper, inside a function that your #\"))))(Secondary((id \
         868d8c0f-f5f2-4802-8633-151c48626239)(content(Whitespace\"\\n\"))))(Secondary((id \
         0ac2219d-42d9-44bb-9c0f-1a1ec7fc7c20)(content(Comment\"# function \
         calls? Step Into follows the call stack down. #\"))))(Secondary((id \
         5c5725f9-c2fb-4df8-bd07-c53bf23517fb)(content(Whitespace\"\\n\"))))(Secondary((id \
         f9b923c5-223c-48bd-a937-a2482ccbc544)(content(Whitespace\"\\n\"))))(Secondary((id \
         55607063-28d9-42ac-a7c7-3ea5d197a358)(content(Comment\"# TRY THIS: \
         #\"))))(Secondary((id \
         4e03d2a9-2e2e-4807-b6f4-66e16e956e5e)(content(Whitespace\"\\n\"))))(Secondary((id \
         bbeda515-5f47-423d-b7c2-b69b5c760d14)(content(Comment\"# 1. Turn on \
         auto-probe and click inside `daily_water` #\"))))(Secondary((id \
         8d7aa5e3-0142-4a8c-a07c-eec368c2bf80)(content(Whitespace\"\\n\"))))(Secondary((id \
         6dff11b7-06fb-4424-af12-4d1b5c24109b)(content(Comment\"# 2. Pin one \
         of the test calls (click a sample > Pin) #\"))))(Secondary((id \
         a8ec12ac-9707-49af-9d39-ebcc53cdc694)(content(Whitespace\"\\n\"))))(Secondary((id \
         c23797c4-4e1a-4561-a2e0-709cad4f8969)(content(Comment\"# 3. Now add a \
         probe to the `phase_multiplier(phase)` call #\"))))(Secondary((id \
         45cbf17f-3dad-48c9-8d00-b651f7ef1b5f)(content(Whitespace\"\\n\"))))(Secondary((id \
         2929a6bc-042f-4960-874a-fc232dff4176)(content(Comment\"#    inside \
         `daily_water` (click on `phase_multiplier`) #\"))))(Secondary((id \
         92310cb5-4a91-4e42-b065-298d89aee21c)(content(Whitespace\"\\n\"))))(Secondary((id \
         8c200ffa-d968-4da9-bf68-e74fbc26432f)(content(Comment\"# 4. Click \
         that sample and choose \\\"Step Into\\\" from the \
         #\"))))(Secondary((id \
         1bceb2bb-9092-4b5b-9259-4500c938c625)(content(Whitespace\"\\n\"))))(Secondary((id \
         c5e170c6-a2d4-4324-ac22-19cae0697fd3)(content(Comment\"#    dropdown \
         (or press Enter) #\"))))(Secondary((id \
         c60a282d-7a33-4d87-860f-485a439a8e3c)(content(Whitespace\"\\n\"))))(Secondary((id \
         27ae068d-12f8-44d6-8867-38a085a75079)(content(Comment\"# 5. Your \
         cursor jumps into `phase_multiplier`! The probes \
         #\"))))(Secondary((id \
         73dca5a3-c0a9-4fab-af05-b0b401f7fa36)(content(Whitespace\"\\n\"))))(Secondary((id \
         dd5933a2-5859-4058-9f85-c7d2844613f4)(content(Comment\"#    there \
         show only values from your pinned context. #\"))))(Secondary((id \
         a4e21a0e-b617-41f6-b0f9-1b5ac0c71005)(content(Whitespace\"\\n\"))))(Secondary((id \
         d17c6074-25bd-4d37-a3ec-7ca636747bc0)(content(Whitespace\"\\n\"))))(Secondary((id \
         4024c5ff-5d5d-4fff-8174-d4885d08b426)(content(Comment\"# THE DYNAMIC \
         CURSOR BAR #\"))))(Secondary((id \
         33a7c65c-fda6-4769-9dea-b4338a61b0ab)(content(Whitespace\"\\n\"))))(Secondary((id \
         3ef4f14a-b5b1-4db4-8113-34961c0561e2)(content(Comment\"# Look at the \
         bar at the top of the editor after stepping in. #\"))))(Secondary((id \
         e483aacc-7f13-4952-a919-b53539e49ca5)(content(Whitespace\"\\n\"))))(Secondary((id \
         7bbd26b6-3981-4a12-9fd6-24e8f81da9e6)(content(Comment\"# It shows \
         your position in the call stack as breadcrumbs: #\"))))(Secondary((id \
         9a65385d-f024-4b44-aa66-7b6b9b375d20)(content(Whitespace\"\\n\"))))(Secondary((id \
         976172b7-f5f2-487c-a8ba-1e2b5df6db74)(content(Comment\"#   top-level \
         > daily_water > phase_multiplier #\"))))(Secondary((id \
         3d93a5d1-4655-4077-9635-9cc841004fb5)(content(Whitespace\"\\n\"))))(Secondary((id \
         18c86e4a-a7fa-4982-b220-d1e91ffb887c)(content(Comment\"# Click a \
         function name to jump to its definition. #\"))))(Secondary((id \
         ed5e3595-e8cb-4e4a-bda9-c4e75ea1d29e)(content(Whitespace\"\\n\"))))(Secondary((id \
         f04ed49b-e025-4eed-8551-8f560a5d82ef)(content(Comment\"# Click a \
         chevron (>) to jump to the call site. #\"))))(Secondary((id \
         e65cbbd9-d903-4323-8f39-576c20837553)(content(Whitespace\"\\n\"))))(Secondary((id \
         d2dbf926-2f4e-442b-a4bc-8752836c701a)(content(Comment\"# This lets \
         you move up and down the call stack freely. #\"))))(Secondary((id \
         6c8ed2ce-451c-477b-84f5-0054bf35f260)(content(Whitespace\"\\n\"))))(Secondary((id \
         7e107a4b-b228-481b-96d2-1051307ea873)(content(Whitespace\"\\n\"))))(Tile((id \
         4dbbe683-0da0-4f46-8f0e-0732f6057aa6)(label(type = in))(mold((out \
         Exp)(in_(TPat Typ))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         5b6323a3-7f17-4090-bad0-4a04f77df0f3)(content(Whitespace\" \
         \"))))(Tile((id \
         79f9d9e0-4ad2-4dc5-9500-1dbd188024b2)(label(MoonPhase))(mold((out \
         TPat)(in_())(nibs(((shape Convex)(sort TPat))((shape Convex)(sort \
         TPat))))))(shards(0))(children())))(Secondary((id \
         d226a4f3-113d-463e-a7a7-d4e00ee52cb3)(content(Whitespace\" \
         \")))))((Secondary((id \
         7892f7d7-814d-4397-9300-a5d056418967)(content(Whitespace\" \
         \"))))(Tile((id \
         c2fa3e51-9639-4888-956d-d7f331822c6c)(label(+))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape(Concave 33))(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         4d36d232-a908-4414-9e09-b0485a4b5e9b)(content(Whitespace\" \
         \"))))(Tile((id \
         11acf986-45bc-48f0-ad08-4e67bf37f4b8)(label(New))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         003c4e19-cbbc-4be9-b20c-4a49f145d23f)(content(Whitespace\" \
         \"))))(Tile((id \
         45922c51-cd05-4b06-9802-f37778dc2c32)(label(+))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 12))(sort Typ))((shape(Concave \
         12))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         56f1ec13-2e93-4636-aa0a-fd66e46c721c)(content(Whitespace\" \
         \"))))(Tile((id \
         d17f739a-3cb1-48b8-b2e3-03f49623cefc)(label(Waxing))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         3522dae4-4350-4236-968c-d860de4422e4)(content(Whitespace\" \
         \"))))(Tile((id \
         651464bb-5708-43ed-b851-e39f3be89db8)(label(+))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 12))(sort Typ))((shape(Concave \
         12))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         d5691c07-64c0-47e7-802a-6fe4be735b2b)(content(Whitespace\" \
         \"))))(Tile((id \
         2795d835-7255-4f9f-b919-3003bae819d0)(label(Full))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         5737aa74-7c59-4348-b32e-340b686468ac)(content(Whitespace\" \
         \"))))(Tile((id \
         18653b72-98b9-48d0-ac2c-19823b875b4c)(label(+))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 12))(sort Typ))((shape(Concave \
         12))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         3e387242-f939-4cde-866c-993d4ce81f7a)(content(Whitespace\" \
         \"))))(Tile((id \
         4e2163f9-45cc-4c7c-b8c0-545a0d2d82f0)(label(Waning))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         03dfb5fb-ed8c-44e5-8558-27073066fb78)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         a1dfe182-33d8-41e9-a7c5-43f4066060ef)(content(Whitespace\"\\n\"))))(Secondary((id \
         4765aa47-b7e6-46d8-b54f-35ed37fbf99b)(content(Whitespace\"\\n\"))))(Tile((id \
         5a66b004-c628-4255-8fcb-bc57ee361dfa)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         bf6de8ef-99f6-475c-82c7-9b76ca19bc1e)(content(Whitespace\" \
         \"))))(Tile((id \
         e82176e3-bfde-4f7f-a5af-fa31476071a2)(label(phase_multiplier))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         987cbfa0-a7b3-4c08-aef2-5340414a557a)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 24))(sort Pat))((shape(Concave \
         24))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         b9bbbc8c-3c70-4a1d-9ca1-a786e4fa9f35)(content(Whitespace\" \
         \"))))(Tile((id \
         3f245f39-9301-44d7-9684-71ece44bb26c)(label(MoonPhase))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         a85f2ced-ee12-41c2-bde4-ad0ea8288779)(content(Whitespace\" \
         \"))))(Tile((id \
         7f20ced4-207d-490b-a707-cf222d0c4204)(label(->))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 13))(sort Typ))((shape(Concave \
         13))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         8d7b2efc-fb3a-41ba-8ca9-281daa0a2be6)(content(Whitespace\" \
         \"))))(Tile((id \
         2437e8b3-0f4b-4cf4-a199-c4d399ab5da1)(label(Float))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         7c58a18e-26d7-44c0-b72f-6aabc340e205)(content(Whitespace\" \
         \")))))((Secondary((id \
         4fc4ac55-5673-45a8-9ed2-352f4b5a78a7)(content(Whitespace\"\\n\"))))(Tile((id \
         a65b92a4-fc10-45d4-8cab-61f4e621c156)(label(fun ->))(mold((out \
         Exp)(in_(Pat))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         37))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         64c92a43-2e0a-4c88-b6d4-a9d5912e0767)(content(Whitespace\" \
         \"))))(Tile((id \
         ce22ac70-3e77-432d-bac7-e489d825db94)(label(phase))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         e8e128cb-5144-4a11-a1dc-77fe5faf0d15)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         e4dcde4a-29a9-4553-be07-57d1c7677ce1)(content(Whitespace\" \
         \"))))(Tile((id 4b3ab85a-1789-4852-9fc4-efc54c2f7588)(label(case \
         end))(mold((out Exp)(in_(Rul))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         5a93e75f-b769-472c-9206-bbcbbe2b8374)(content(Whitespace\" \
         \"))))(Tile((id \
         91a78b8d-3244-40dc-ac9a-ec0704882ad4)(label(phase))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         6ea5ee9a-2ff0-4303-9a17-631f36474f05)(content(Whitespace\"\\n\"))))(Tile((id \
         fa697ab1-7e3a-4f09-a7a0-259db503990c)(label(| =>))(mold((out \
         Rul)(in_(Pat))(nibs(((shape(Concave 46))(sort Exp))((shape(Concave \
         46))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         e7eb4df8-493a-4a62-9919-0aaacdc81507)(content(Whitespace\" \
         \"))))(Tile((id \
         1416dea4-de56-4ea0-8f3a-379e74f0bb99)(label(New))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         90d05a1f-c62b-4dd0-bc0c-5009f2949016)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         0006d8bd-66c2-4cec-81d9-8d6045c68e1e)(content(Whitespace\" \
         \"))))(Tile((id \
         ed383cfc-7d7c-4eca-86d8-ad62b5aeb999)(label(1.2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         bbab5da3-6e96-4302-a6e8-e4cd18efe984)(content(Whitespace\"\\n\"))))(Tile((id \
         10219547-37bc-45fb-8b83-73b582a5f340)(label(| =>))(mold((out \
         Rul)(in_(Pat))(nibs(((shape(Concave 46))(sort Exp))((shape(Concave \
         46))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         92aa2aec-004a-48e6-a468-cefa927af07c)(content(Whitespace\" \
         \"))))(Tile((id \
         ad2aaff4-37ce-404e-89a2-19186e80b205)(label(Waxing))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         5d15cff9-b1f6-4f92-934e-c06ea8c5c63a)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         eb2041ba-fcd3-429e-90a0-2a529b6f26d0)(content(Whitespace\" \
         \"))))(Tile((id \
         ca4a5dbb-db23-45aa-8c37-108b6e8a3dfc)(label(1.1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         28f53e56-e370-4c00-b0fe-6567f2dcd02c)(content(Whitespace\"\\n\"))))(Tile((id \
         279f736c-37b6-4a00-b22a-87153ff05527)(label(| =>))(mold((out \
         Rul)(in_(Pat))(nibs(((shape(Concave 46))(sort Exp))((shape(Concave \
         46))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         38748037-49cf-4760-b255-095950b116d3)(content(Whitespace\" \
         \"))))(Tile((id \
         8190f869-5707-401a-9ef7-3b0b02231a1d)(label(Full))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         4d98f8a3-2007-4960-a827-191af1aee644)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         518392a0-0902-4fbb-8970-ec8e8d3f4bbc)(content(Whitespace\" \
         \"))))(Tile((id \
         9275fbb0-ab02-4773-bbcf-79a2e65ba2df)(label(0.88))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         6dc3c47a-c798-4833-bce4-4933c1bad6a4)(content(Whitespace\"\\n\"))))(Tile((id \
         e3a84731-d6b9-46ee-9a48-91bc2aefe4fc)(label(| =>))(mold((out \
         Rul)(in_(Pat))(nibs(((shape(Concave 46))(sort Exp))((shape(Concave \
         46))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         5f08a36f-48e1-4ecd-8ebf-76a7e3541d0f)(content(Whitespace\" \
         \"))))(Tile((id \
         3b3a327c-b044-4f28-96ea-29da1fa6ff3d)(label(Waning))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         cc328b6d-1647-48da-a38f-1675a08a0249)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         4ddd092d-1d86-49f3-ad18-af60d9691cec)(content(Whitespace\" \
         \"))))(Tile((id \
         d72a66a4-f93b-45ba-9af8-7ec78ede8b20)(label(0.95))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         6b3becaf-3680-49d0-8a74-4d3fac8f592c)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         5566bddf-138c-4a46-bfc1-9ead2a449d21)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         b9d054aa-2d9e-4f31-aa74-3723b3f787b6)(content(Whitespace\"\\n\"))))(Secondary((id \
         ce6d0c17-aa1b-42ba-969b-d9a18a5ea9d9)(content(Whitespace\"\\n\"))))(Tile((id \
         c06f7e27-7bc1-4037-8062-83a547517d97)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         36c83db6-7ae8-4af5-997b-ba233fdff0ac)(content(Whitespace\" \
         \"))))(Tile((id \
         6d855f05-bdb4-43f4-bf3b-df5ad552b7c1)(label(shade_multiplier))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         cebcba4b-e75d-4898-80a4-a36aa29277ec)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 24))(sort Pat))((shape(Concave \
         24))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         bf0f108b-10bb-4179-ab5b-1bae9b052d67)(content(Whitespace\" \
         \"))))(Tile((id \
         f70dcd22-f0c6-4ac5-a92e-f8110549e14b)(label(Int))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         42f20d3e-c9e0-4dc5-bcbe-6ebda61afb26)(content(Whitespace\" \
         \"))))(Tile((id \
         44fbeb9a-11fc-4942-9325-22a65f05931c)(label(->))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 13))(sort Typ))((shape(Concave \
         13))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         245be86f-3125-4fc2-9616-45ce9c1e1088)(content(Whitespace\" \
         \"))))(Tile((id \
         5b34c139-f384-452f-b7f0-f7b4aa264f9f)(label(Float))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         6e22d82e-11fd-4db1-a5d1-b577ba349b7f)(content(Whitespace\" \
         \")))))((Secondary((id \
         9d40ea2e-0887-4e0f-a8ee-ac9513de22fe)(content(Whitespace\"\\n\"))))(Tile((id \
         1b46b125-9a99-4272-a49f-09b5f55f7db1)(label(fun ->))(mold((out \
         Exp)(in_(Pat))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         37))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         5763dd84-10f3-4f37-b411-c8df760dfd77)(content(Whitespace\" \
         \"))))(Tile((id \
         7264c3c8-2dda-471c-8b3c-8565a7b3fda4)(label(shade))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         2b92c181-a1c3-4c38-b2d4-1f47b0ab03aa)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         0f4e3ae3-0aab-46a5-a98f-c335377f5d37)(content(Whitespace\" \
         \"))))(Tile((id fd6186af-0cb4-474f-b7b1-672556e2d256)(label(case \
         end))(mold((out Exp)(in_(Rul))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         d812b3b8-9197-4e52-830d-f34c1f8c1f45)(content(Whitespace\" \
         \"))))(Tile((id \
         22b38e9b-1240-47cc-a711-262ac95520e6)(label(shade))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         cf09cfcb-0cc4-4c66-be9a-e090dabf8d5c)(content(Whitespace\"\\n\"))))(Tile((id \
         3473070d-7515-4d05-8140-593bfab38566)(label(| =>))(mold((out \
         Rul)(in_(Pat))(nibs(((shape(Concave 46))(sort Exp))((shape(Concave \
         46))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         b24cb27d-c533-4dac-9ab8-d7976cebc683)(content(Whitespace\" \
         \"))))(Tile((id \
         3de2a682-15a5-424d-8b8e-4206cd41589b)(label(0))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         63b3b989-2cad-4297-a901-1d59d8814b86)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         93730d51-f803-4676-a657-ea772784a5e8)(content(Whitespace\" \
         \"))))(Tile((id \
         fffe2937-ee04-4813-beae-768a8b964745)(label(1.0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         1c4061c7-320e-4a2d-ba29-88f9e0f3d64d)(content(Whitespace\"\\n\"))))(Tile((id \
         b4881e2f-8467-4527-92de-54ca32bf37b9)(label(| =>))(mold((out \
         Rul)(in_(Pat))(nibs(((shape(Concave 46))(sort Exp))((shape(Concave \
         46))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         b3f95fe0-47b7-4983-a238-add38ea1bdc6)(content(Whitespace\" \
         \"))))(Tile((id \
         ce6ffeb1-c4e5-438e-8d0c-3be12b6868b8)(label(1))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         1f4696b0-35eb-49c0-a6e6-5f87a297af47)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         5b12e780-b018-413b-bd1c-7f8771ca2a64)(content(Whitespace\" \
         \"))))(Tile((id \
         35336e43-69e6-48d5-b71a-3997694b9ad5)(label(0.9))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         db245c1b-4768-457b-b8b1-9af5abf85150)(content(Whitespace\"\\n\"))))(Tile((id \
         8be99f58-4d6c-44ca-9913-c140dd9ab86c)(label(| =>))(mold((out \
         Rul)(in_(Pat))(nibs(((shape(Concave 46))(sort Exp))((shape(Concave \
         46))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         8f4c655c-9131-4f73-8708-05e759dd18bb)(content(Whitespace\" \
         \"))))(Tile((id \
         92d558a0-6bb3-4d1c-9ab2-4384cacfac78)(label(2))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         7473fd34-998c-4ef0-82b8-b34511503a98)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         ba4cb64f-48f4-4ee8-830c-318d3f60c326)(content(Whitespace\" \
         \"))))(Tile((id \
         906769b1-9429-4e79-a792-2cebe8a4117b)(label(0.75))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         16f5d544-914f-482a-9bf4-e3174b6c9a1c)(content(Whitespace\"\\n\"))))(Tile((id \
         d7783365-1585-476e-8e8e-c58203badbb2)(label(| =>))(mold((out \
         Rul)(in_(Pat))(nibs(((shape(Concave 46))(sort Exp))((shape(Concave \
         46))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         10acabee-fe7e-4388-ac2f-a1e71db50820)(content(Whitespace\" \
         \"))))(Tile((id \
         2e362d84-08ac-4626-934f-80deb1ac8650)(label(_))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         1c7778c5-2393-485f-906f-a243718c3377)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         b58e885c-cbdc-40c0-aba9-aec40259c1ed)(content(Whitespace\" \
         \"))))(Tile((id \
         c3fb7797-ceff-44ed-aae2-654445131ab3)(label(0.6))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         bb1428b9-a44e-4a70-8ee9-578fb0a25feb)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         9750ad42-d812-441e-91c3-ffe2792bf5f7)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         d1408282-9cd2-4603-b705-40af9e6e6e1d)(content(Whitespace\"\\n\"))))(Secondary((id \
         4b0c505e-b321-4529-b624-0a8b56372f21)(content(Whitespace\"\\n\"))))(Tile((id \
         4aed5892-8879-4f03-8bbc-e49df564e723)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         4593b845-4644-4b3c-b78a-bc1de9a72091)(content(Whitespace\" \
         \"))))(Tile((id \
         9e26121e-0b66-41bd-b7c6-4e5ead69bbb0)(label(daily_water))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         0b41c38e-7f79-4efa-8bf3-1c7b1c9c9f95)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 24))(sort Pat))((shape(Concave \
         24))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         41c8c0b4-cee8-45c5-9a57-d72a4c3d17dc)(content(Whitespace\" \
         \"))))(Tile((id \
         14b674ce-4c20-41a4-bdc8-962972d541bb)(label(\"(\"\")\"))(mold((out \
         Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0 1))(children(((Tile((id \
         e1690ff5-b5ba-4461-87f1-b281429ab156)(label(Int))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         3656a2bf-2eb7-4b24-81d3-322454df1263)(label(,))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 44))(sort Typ))((shape(Concave \
         44))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         5e08f6bf-d5b1-4460-8aeb-24be3df74f9c)(content(Whitespace\" \
         \"))))(Tile((id \
         c7f6d327-1c79-4ddd-93aa-4635376f9e96)(label(Int))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         0edbd1ab-d0ff-4b05-bab4-363d482d9f30)(label(,))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 44))(sort Typ))((shape(Concave \
         44))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         bd805aed-82bb-4cef-853a-425b339241b5)(content(Whitespace\" \
         \"))))(Tile((id \
         e5dbc31b-e9ad-49c3-897c-8b2548f75104)(label(MoonPhase))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children()))))))))(Secondary((id \
         f9f561b1-8976-4f72-8d7e-61f6bbd559a1)(content(Whitespace\" \
         \"))))(Tile((id \
         f9aa12a2-d5c8-449f-98f6-6b536f605272)(label(->))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 13))(sort Typ))((shape(Concave \
         13))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         a06d2d51-f977-4c46-9f2d-ec4d5e017a44)(content(Whitespace\" \
         \"))))(Tile((id \
         01d6d6dd-e82a-4129-beab-644ea63d2a73)(label(Int))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         2096a5bb-95ec-4736-82ea-4502305c5bf3)(content(Whitespace\" \
         \")))))((Secondary((id \
         7bc4a512-d1f4-40ca-b07d-bb4ce8ff4b6d)(content(Whitespace\"\\n\"))))(Tile((id \
         3aea1ac6-6809-4b8f-9086-261b25809277)(label(fun ->))(mold((out \
         Exp)(in_(Pat))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         37))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         e7774506-129e-4fbb-8490-30b269c431c0)(content(Whitespace\" \
         \"))))(Tile((id \
         7d8b5b1c-aa16-450b-bb61-522d707182c2)(label(\"(\"\")\"))(mold((out \
         Pat)(in_(Pat))(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0 1))(children(((Tile((id \
         3e92b91b-1102-4a67-85c5-9e0c353cb54d)(label(base))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         056cc3af-b5e5-462a-8765-9185d5d5d7fc)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 44))(sort Pat))((shape(Concave \
         44))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         dc671ea8-243e-4ebf-b296-226bb1c96ca4)(content(Whitespace\" \
         \"))))(Tile((id \
         90cb8147-7d54-440e-8c50-586a1cc5ecaf)(label(shade))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         4fdaccfa-68ac-4a21-a3cd-3f8815cd20eb)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 44))(sort Pat))((shape(Concave \
         44))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         b6062493-2714-4e4b-8e52-5f2e8b84adfd)(content(Whitespace\" \
         \"))))(Tile((id \
         0fb25d0f-d3bb-42ac-9ae2-11c9dedd894e)(label(phase))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children()))))))))(Secondary((id \
         48721ef8-c82c-46fe-99f5-7e35aa7dba96)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         85347568-587e-4ba5-bad1-762cb6aa4ef4)(content(Whitespace\"\\n\"))))(Tile((id \
         90f98b34-8d0d-441d-991d-47ce5c445211)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         b70da366-4c1f-4de1-900f-ae883f5aa4fe)(content(Whitespace\" \
         \"))))(Tile((id \
         45ca49a5-1022-42f7-9610-a67998c6d049)(label(base_f))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         d8724483-899e-4fb0-b649-866d565ee812)(content(Whitespace\" \
         \")))))((Secondary((id \
         33d3f594-77b1-4597-8945-82e095b4cff1)(content(Whitespace\" \
         \"))))(Tile((id \
         941c6d9d-ce6c-4da6-a393-fa6cba2660c1)(label(float_of_int))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         f6d2fbee-20cd-48a5-bc0d-662dddf3fffa)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         2d8bc912-7016-4840-acb6-607be1de7950)(label(base))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         5a57670e-d6cf-4f49-8554-2b408a0bd755)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         2812f09a-1fe7-48e5-990f-e93109508be4)(content(Whitespace\"\\n\"))))(Tile((id \
         eab5bad0-8552-40c0-b716-16648f8d15ad)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         ba4267a8-13c0-4aa6-baed-5a2e588d5b0d)(content(Whitespace\" \
         \"))))(Tile((id \
         8732334e-8b48-4ce7-bb35-8f9055f46acd)(label(phase_adj))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         ab3fe0d7-4d91-4ab5-854c-b73b55d75f51)(content(Whitespace\" \
         \")))))((Secondary((id \
         af3de21d-7be4-40c9-9038-bf1b997eb04e)(content(Whitespace\" \
         \"))))(Tile((id \
         f3b82ac3-df83-4749-9448-00decaf00e1e)(label(base_f))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         dbc2a263-26f8-4036-b4d1-e555f2d8b0b7)(content(Whitespace\" \
         \"))))(Tile((id \
         0707b79a-8509-431b-a808-02d9dc7f6d7d)(label(*.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 27))(sort Exp))((shape(Concave \
         27))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         dcbd2466-153b-4adf-9b51-801e15526892)(content(Whitespace\" \
         \"))))(Tile((id \
         68078c15-3f50-437f-859a-130d287ea39e)(label(phase_multiplier))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         0e61f55a-dd57-4eea-bc8b-d1c235b98a27)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         6af6db14-456a-42bf-a581-9467e143ae77)(label(phase))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         60e87cc0-886c-4eb9-b88b-98208836e504)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         fdf9a2a3-e715-49fa-a6e7-2daeaf3732dd)(content(Whitespace\"\\n\"))))(Tile((id \
         0ca006f4-665f-4f08-9321-75d9f4983f4a)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         7ea03fa8-fe09-4327-9755-ad8a1259a209)(content(Whitespace\" \
         \"))))(Tile((id \
         c86f8dd5-a797-4e8b-adfb-26ecbd4508a3)(label(shade_adj))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         34c8bc91-5577-4963-a834-615574f5912f)(content(Whitespace\" \
         \")))))((Secondary((id \
         c561178c-0919-4aec-9c1e-f32658521437)(content(Whitespace\" \
         \"))))(Tile((id \
         5cab8bae-2d0c-4429-931f-4b57878d2575)(label(phase_adj))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         211af8b7-963a-4d61-a421-b513b09ee200)(content(Whitespace\" \
         \"))))(Tile((id \
         7062559e-cbdc-40ac-86c4-8e449b732d7a)(label(*.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 27))(sort Exp))((shape(Concave \
         27))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         29a2425c-2d0b-4cb7-a389-61e630f661a1)(content(Whitespace\" \
         \"))))(Tile((id \
         71364016-44fd-4e4e-a734-f5869b6028d6)(label(shade_multiplier))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         4bdabf9a-56d7-485d-936d-a194183d484e)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         489a6cfa-7df4-4c12-823f-67913a6f2249)(label(shade))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         64952f4e-11b0-46b1-9e3b-d0f253092171)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         9cc18382-4cea-4522-b5e6-5c104140335a)(content(Whitespace\"\\n\"))))(Tile((id \
         bad70733-4b29-4527-a18d-86bd24a14fef)(label(int_of_float))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         16adc11b-45c9-47ab-ac92-a808346b3b3c)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         e8186f8b-a2bc-4cec-b08f-1a69952e9471)(label(shade_adj))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         e5088e56-3956-447b-b6a3-f93aea5e3e4c)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         6d814676-af11-43ad-8cd2-319408446219)(content(Whitespace\"\\n\"))))(Secondary((id \
         fad37a89-3fd6-4c5c-b152-0ab7b04e11e5)(content(Whitespace\"\\n\"))))(Tile((id \
         f7b32290-3b79-46d4-89da-5012197a7a2a)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         789dc13c-cb1f-4bc1-bb25-7590b06b8e80)(content(Whitespace\" \
         \"))))(Tile((id \
         7784380e-c69f-49f2-a973-82931f705d07)(label(daily_water))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         3004e8b2-5747-434e-8a42-b51770bf1e08)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         c96c5912-1ea8-4e8c-900f-07f9adc5d3f0)(label(250))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         e62940cf-3268-4dac-850c-dd982ad8ce19)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         14f252c9-f7e5-4131-87e2-974006a8de85)(content(Whitespace\" \
         \"))))(Tile((id \
         6777ce14-6a23-4006-a8f8-e8424c7c7a7b)(label(2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         0584766d-a839-42a6-a2d4-75afe793b5ae)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         f661d493-918a-41bc-9005-8858b5b8bb4c)(content(Whitespace\" \
         \"))))(Tile((id \
         8d0f4770-1075-4319-bda5-f91857d65e07)(label(Full))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         e1c4746f-38c2-4b28-aa41-4b3524427d53)(content(Whitespace\" \
         \"))))(Tile((id \
         27c981d4-1c29-40b6-8051-9e39d2c9863f)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         27bf715b-ec91-4878-837c-2d1f7dd8033f)(content(Whitespace\" \
         \"))))(Tile((id \
         b62450b3-4015-4cad-9420-608ad23e2324)(label(165))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         897916f1-5e7d-452f-b5f2-f026b140c85b)(content(Whitespace\" \
         \")))))))))(Tile((id \
         327a6fc6-b174-494c-aacd-7e6e2c261051)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         92dbebe8-aa75-4e57-8392-6c31b19bdaac)(content(Whitespace\"\\n\"))))(Tile((id \
         55a99da9-6339-445a-8061-42e910febd4b)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         383737d2-caa9-4f96-94cf-e9755c182cf6)(content(Whitespace\" \
         \"))))(Tile((id \
         2e876f1e-19d0-4632-8388-e621fc601406)(label(daily_water))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         0ac26352-0e6c-4972-be2b-4db614c31a14)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         a182175f-2aa8-4005-bcd9-bdd6b58e3f1b)(label(50))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         1247c1f0-c3e0-4bb1-84d3-961fe931391e)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         96fc59bc-b1fc-41ff-a027-5d4b0369a536)(content(Whitespace\" \
         \"))))(Tile((id \
         b5166cf6-0763-483a-addd-429b23942d4c)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         1043afad-59d1-4de1-8822-c3fbfb81f1c1)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         e92b8a1c-77ed-4148-b191-d9c679be38fe)(content(Whitespace\" \
         \"))))(Tile((id \
         9172f122-2899-40cd-b62a-52342afb1317)(label(New))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         1f55d073-961d-4cb5-99cc-550015f98c29)(content(Whitespace\" \
         \"))))(Tile((id \
         67a739f2-a335-4a52-9f1b-cab63ff4b569)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         e1eec986-d8bc-40cd-b20e-ec29cdf1dc8b)(content(Whitespace\" \
         \"))))(Tile((id \
         59c0febb-f7eb-48b7-b1aa-f692b7375432)(label(60))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         9834bdd7-2b47-4479-84cb-ee47d8d7f87b)(content(Whitespace\" \
         \")))))))))(Tile((id \
         bf2836ef-cfe8-4d03-9546-4c95bb3046e5)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         07c6a813-a7d1-406a-83c9-0022a1f84d1d)(content(Whitespace\"\\n\"))))(Tile((id \
         28aa7f26-3262-4771-8773-449e1a49f316)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         0b887b7b-4dca-43e7-8036-e6b2f5e9e804)(content(Whitespace\" \
         \"))))(Tile((id \
         96390c19-c33a-48d2-9d5d-b6b19935e018)(label(daily_water))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         1af35325-dd7c-4aca-b8c4-9ba19dec54b7)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         bc118a54-3f96-4c2e-b188-5aa80e67471c)(label(180))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         2de5e56c-7189-46ab-b602-1517dfd9153e)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         7764eeb5-67aa-4184-b756-52f48a306787)(content(Whitespace\" \
         \"))))(Tile((id \
         257224e2-c8fa-4429-b56c-eef19d899062)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         59310176-c799-4ecf-9dca-5e62c6d4c7e0)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         def395de-a354-4465-8a2d-a260ccb1cdc4)(content(Whitespace\" \
         \"))))(Tile((id \
         91977480-25f3-434f-bb1a-21e3a57c002b)(label(Waning))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         bc59af9b-b30d-4233-8600-ecea1faa7bc9)(content(Whitespace\" \
         \"))))(Tile((id \
         9b629a73-3e2a-4ca2-8e2d-2b8ca83482ef)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         47506401-f542-4dce-a574-4e2945862a85)(content(Whitespace\" \
         \"))))(Tile((id \
         59d908ec-bcce-41f7-a671-8cf1bd42fc4b)(label(171))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         dc68c163-ba8c-4162-843d-747235372fd5)(content(Whitespace\" \
         \")))))))))(Tile((id \
         ecb6ddb6-67a2-444f-a187-cbf1944a01cb)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         32942be0-a41e-44ce-908d-062bb608e4e7)(content(Whitespace\"\\n\"))))(Tile((id \
         bd6ade52-9d78-4dbe-a663-3c5aa9e18562)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         1bc3c0a8-fce8-4059-8adf-cd4d53384542)(content(Whitespace\" \
         \"))))(Tile((id \
         d7b6c2b2-3505-4a56-a6fc-be12b7adedc4)(label(daily_water))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         14b37541-0aa3-42c9-a887-6a54f537288c)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         564f8c7e-5b44-4147-a5a7-ea0cc2c1c644)(label(200))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         57dfdb61-d614-4316-bce6-6dbc9fb780b6)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         c39fddd2-b320-4402-b9b7-b5eb9e2dea67)(content(Whitespace\" \
         \"))))(Tile((id \
         52cfd1f9-a981-4a74-80ca-c5b60e15280a)(label(2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         8603fc4b-dc60-4632-ade8-63bf78eb27ba)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         781907af-9f2d-422c-b0b8-b616639dbca9)(content(Whitespace\" \
         \"))))(Tile((id \
         2ebec57c-447c-4465-b1eb-35d46c249a9f)(label(Waxing))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         fe39afde-ad2d-485f-b48b-08c17fba42d0)(content(Whitespace\" \
         \"))))(Tile((id \
         50d47b39-9b72-47a8-80c8-9febf858961a)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         9da25f24-d04b-4003-9ca7-8b57ff7d289d)(content(Whitespace\" \
         \"))))(Tile((id \
         6284d115-d26e-42c2-90e1-f9ac80613752)(label(165))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         3166c185-2d3b-4ede-b56f-03e71d34bec1)(content(Whitespace\" \
         \")))))))))(Tile((id \
         d81f3074-98ab-4bda-81a4-5bf51aa903aa)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         873eb28c-392b-460f-b593-8a51b44b9e3e)(content(Whitespace\"\\n\"))))(Secondary((id \
         c2c2800d-2ff1-466e-8e2d-5f0e4c1a53fe)(content(Whitespace\"\\n\"))))(Tile((id \
         7d655dfd-cf8e-4dc3-b3e0-16d28ecadb0c)(label(daily_water))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         42b7880d-a527-4e37-a6fd-2fbfd4e82fa4)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         9aff2344-2e2d-45a6-8725-82d2cefe3228)(label(100))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         58ab33e3-1573-495e-a1ef-0d9298add47f)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         2005e17f-2a19-4d1a-a262-d46965d9d844)(content(Whitespace\" \
         \"))))(Tile((id \
         f897a969-28c0-44bc-b5ad-caf01c1e153f)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         fb42ae8d-19b3-4f5b-87a9-67e82d4d9bd7)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         d85eb76d-a43a-4f57-96e8-c6b03cd91d8c)(content(Whitespace\" \
         \"))))(Tile((id \
         5f986c8e-6e45-4c0e-ac5d-99bb28be6dca)(label(Full))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         4be51d47-9088-4847-bbf4-adb17443b511)(content(Whitespace\"\\n\"))))(Secondary((id \
         0a65dc44-a502-46c9-a597-fea32ed5de5a)(content(Whitespace\"\\n\"))))(Secondary((id \
         eb4a00cf-b525-4bd7-a8d2-c27b17caa148)(content(Comment\"# END OF PART \
         5 - Select the next slide from the top menu #\"))))(Secondary((id \
         bcefba3e-279a-4126-90a1-25414cc8b11b)(content(Whitespace\"\\n\")))))";
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
