let out : string * Haz3lcore.PersistentSegment.t =
  ( "Study / tutorial / 02-functions-and-cursor",
    {
      segment =
        "((Secondary((id \
         e3829af6-91ac-46e8-ac2b-c2e2ec0136e4)(content(Comment\"# PROBES \
         TUTORIAL - PART 2: FUNCTIONS AND THE DYNAMIC CURSOR \
         #\"))))(Secondary((id \
         e5973b03-5088-4429-aae9-50cc0d738bdb)(content(Whitespace\"\\n\"))))(Secondary((id \
         d08f505d-04ad-4007-bc01-0302e90d6e17)(content(Whitespace\"\\n\"))))(Secondary((id \
         a01e6bb7-f628-4fa1-939a-a8afe659e94d)(content(Comment\"# When a \
         function is called multiple times, each call #\"))))(Secondary((id \
         0b8a8ecc-7468-490c-9508-70fcbc2ce556)(content(Whitespace\"\\n\"))))(Secondary((id \
         fad85718-b5fd-4f5e-a2ff-b42d3449084f)(content(Comment\"# generates \
         its own sample. Let's see what that looks like! #\"))))(Secondary((id \
         33686492-2c10-46b0-a1d0-6e118ee376b5)(content(Whitespace\"\\n\"))))(Secondary((id \
         5cf78ff4-2501-49f7-ac38-eae4ece6bb56)(content(Whitespace\"\\n\"))))(Tile((id \
         9da6703b-4585-45d3-b16d-c07680e9105b)(label(type = in))(mold((out \
         Exp)(in_(TPat Typ))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         ac56b5d8-5cd7-419e-b0cb-a1c8876066b5)(content(Whitespace\" \
         \"))))(Tile((id \
         289e9462-9aa8-48f3-96fd-52184bc1911e)(label(MoonPhase))(mold((out \
         TPat)(in_())(nibs(((shape Convex)(sort TPat))((shape Convex)(sort \
         TPat))))))(shards(0))(children())))(Secondary((id \
         38459d59-757f-4d87-807d-ec13e4ff8548)(content(Whitespace\" \
         \")))))((Secondary((id \
         0f392263-8199-49c8-8659-9fea197f839c)(content(Whitespace\" \
         \"))))(Tile((id \
         64be1a80-7056-4dec-bd3c-99a7d3009b0c)(label(New))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         537ee7fc-5f95-47df-a072-394386b91f0c)(content(Whitespace\" \
         \"))))(Tile((id \
         ff970fc3-7482-4cff-a1bd-a53a9e9ccdb9)(label(+))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 12))(sort Typ))((shape(Concave \
         12))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         f3cd5248-1723-401c-9317-e5baa84b2688)(content(Whitespace\" \
         \"))))(Tile((id \
         d746169d-c976-465d-be13-978499131d2c)(label(Waxing))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         cf8ef36b-0daa-44b5-975c-ff876d25af97)(content(Whitespace\" \
         \"))))(Tile((id \
         c0eb88ba-b42d-465b-a2bd-55a7004d6af2)(label(+))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 12))(sort Typ))((shape(Concave \
         12))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         4ed2ccb4-d24a-4c92-9a52-865a673db632)(content(Whitespace\" \
         \"))))(Tile((id \
         4f9dab9e-a591-4225-87c2-d7cb3325a058)(label(Full))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         237dfbf3-9b4c-4b1f-9d9b-78662507e240)(content(Whitespace\" \
         \"))))(Tile((id \
         68e6dcd2-0740-4c25-9998-95eca92ef2b3)(label(+))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 12))(sort Typ))((shape(Concave \
         12))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         b2f0fc19-bee8-4e45-8b42-f5c17f54126e)(content(Whitespace\" \
         \"))))(Tile((id \
         e733afc9-776e-4ac1-a2f4-2c1764a5b2d1)(label(Waning))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         4a474235-abad-4dc0-a6e5-ad70d113f433)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         7595284f-a528-4e5a-9b7b-83156a194d26)(content(Whitespace\"\\n\"))))(Secondary((id \
         c23213fb-9903-40c0-bd7f-248c9fe0ce6f)(content(Whitespace\"\\n\"))))(Secondary((id \
         450d0bed-bb49-445d-a86d-867604bf0d62)(content(Comment\"# Hazel has no \
         special function definition syntax. #\"))))(Secondary((id \
         c98568e5-99d4-43f9-9565-ff234d826e53)(content(Whitespace\"\\n\"))))(Secondary((id \
         1b729f4c-b7c9-425e-a0b5-94eefc99da9b)(content(Comment\"# We use \
         regular let definitions to define function literals, \
         #\"))))(Secondary((id \
         958e2a23-70cc-4288-9176-969d5d206e8f)(content(Whitespace\"\\n\"))))(Secondary((id \
         c8b959e6-f8ce-45a3-b492-034eb307cda4)(content(Comment\"# using the \
         syntax `fun <pattern> -> <body>`. #\"))))(Secondary((id \
         a775f844-0812-4c10-b51c-e699312909a7)(content(Whitespace\"\\n\"))))(Secondary((id \
         cdfd843a-811f-4288-a5d4-3643fcd6eee4)(content(Whitespace\"\\n\"))))(Secondary((id \
         8e00aa55-7ee0-4e29-962f-37c7df054bf9)(content(Comment\"# TRY THIS: \
         Add a probe to the `multiplier` variable inside #\"))))(Secondary((id \
         1147ac1e-34c0-4670-8078-5589d753af1a)(content(Whitespace\"\\n\"))))(Secondary((id \
         4aa7646e-c66d-407b-843d-7a4ebe0c5c8c)(content(Comment\"# the function \
         `watering_amount` below. When you click on the #\"))))(Secondary((id \
         7b39b354-cdee-4beb-80be-160c12d349fe)(content(Whitespace\"\\n\"))))(Secondary((id \
         d43d9288-60b7-4ad7-afff-ef0100dacefc)(content(Comment\"# sample, \
         notice the arrows that appear to the left. Click on \
         #\"))))(Secondary((id \
         f23c3df7-3d95-4be4-b6a2-aaaf4971414b)(content(Whitespace\"\\n\"))))(Secondary((id \
         ffb7cb43-1cff-45a9-8e97-9b9216ee9b44)(content(Comment\"# these \
         arrows, or use the left/right arrow keys, to navigate \
         #\"))))(Secondary((id \
         ad2256bb-971a-4396-be66-1c5530533d6e)(content(Whitespace\"\\n\"))))(Secondary((id \
         4a2caedb-927b-4347-86a2-21cffa9695f0)(content(Comment\"# between the \
         three different samples collected. #\"))))(Secondary((id \
         ebb0e934-9f1e-477e-a9a0-b9f31a9e683c)(content(Whitespace\"\\n\"))))(Secondary((id \
         19a2c243-46f3-40e0-b610-8a072561c9f9)(content(Whitespace\"\\n\"))))(Tile((id \
         7e73837c-6fae-4444-8b28-70b7addb323e)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         2f7e7de4-150f-438d-9830-79c3fe5977d0)(content(Whitespace\" \
         \"))))(Tile((id \
         d8cfe513-8aab-49f5-9c9c-3d2d7c8483d5)(label(watering_amount))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         6a419f3c-882c-4ab1-8249-25bf3b365896)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 24))(sort Pat))((shape(Concave \
         24))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         de72fc5a-500a-4cb9-b03a-f8851fbd0210)(content(Whitespace\" \
         \"))))(Tile((id \
         df7fdac6-cf88-4353-90f7-4451fac3c812)(label(\"(\"\")\"))(mold((out \
         Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0 1))(children(((Tile((id \
         9a2ed2e5-b32a-481d-8c1c-b1ec29a9679d)(label(Int))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         28fda9ba-a107-443c-b770-599b4bdd4d10)(label(,))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 44))(sort Typ))((shape(Concave \
         44))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         fa52d12d-b0dc-4373-85ff-234240bd4d7b)(content(Whitespace\" \
         \"))))(Tile((id \
         f78f8b16-1316-4d2a-b1c4-de5d48b4e1b5)(label(MoonPhase))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children()))))))))(Secondary((id \
         cb9bb071-c4f2-460c-b9b1-5c461312dd60)(content(Whitespace\" \
         \"))))(Tile((id \
         c653db0e-65ae-47cf-b0f8-a280dda08f7d)(label(->))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 13))(sort Typ))((shape(Concave \
         13))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         949f360e-421b-4409-a3b7-1e1f70c993e7)(content(Whitespace\" \
         \"))))(Tile((id \
         42c91ba7-a012-4e4c-8496-95e7ea6d7ed6)(label(Int))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         90f7c4df-c494-438e-b7dc-563beb67edcb)(content(Whitespace\" \
         \")))))((Secondary((id \
         365b9dad-dd32-4330-969b-e48e0e07fddc)(content(Whitespace\"\\n\"))))(Tile((id \
         993fd237-44a2-4b3c-9cc4-2556b70f157a)(label(fun ->))(mold((out \
         Exp)(in_(Pat))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         37))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         81c38db3-8ac1-4b7e-972c-6a2480ddf560)(content(Whitespace\" \
         \"))))(Tile((id \
         87431bc8-0744-4909-b409-e1997762ee87)(label(\"(\"\")\"))(mold((out \
         Pat)(in_(Pat))(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0 1))(children(((Tile((id \
         56f613a8-9594-428d-9c6f-27761413c252)(label(base_ml))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         e29fd638-86dc-4800-92bc-e601aaaa83d7)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 44))(sort Pat))((shape(Concave \
         44))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         29b48fd2-93fe-4fdc-bb1a-0c0871c42062)(content(Whitespace\" \
         \"))))(Tile((id \
         21fe070e-7d37-418c-a88e-47bc600f1433)(label(phase))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children()))))))))(Secondary((id \
         3729062f-d6ec-45a6-b9de-3bd4e873865e)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         50109ed1-aec1-4b61-916c-ecad6ae6867e)(content(Whitespace\"\\n\"))))(Tile((id \
         858aad51-d497-43ea-a3a6-0a536d392ce0)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         7bc78297-8e8e-411b-bea3-2188e57503b6)(content(Whitespace\" \
         \"))))(Tile((id \
         8ca9b510-c1c8-4e57-9b82-1e44912d7b1e)(label(multiplier))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         f34ee951-a116-4b5b-9eba-fbd4c6d3a94c)(content(Whitespace\" \
         \")))))((Secondary((id \
         8e3d8bd8-2a2c-4781-bdba-879fcc7087b4)(content(Whitespace\"\\n\"))))(Tile((id \
         70c39d69-a651-4b93-8cf5-80f6933ef6d0)(label(case end))(mold((out \
         Exp)(in_(Rul))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         bab9d798-7c68-4972-b7f8-91ddd20503ad)(content(Whitespace\" \
         \"))))(Tile((id \
         86e9e42c-0531-472c-a83e-3d0e79cc9c09)(label(phase))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         8161ad7b-987e-4f09-bb45-5feaadf7c245)(content(Whitespace\"\\n\"))))(Tile((id \
         c0a087b2-9b8d-49cf-bd34-8aadf63b9c0c)(label(| =>))(mold((out \
         Rul)(in_(Pat))(nibs(((shape(Concave 46))(sort Exp))((shape(Concave \
         46))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         1749f8b9-37a0-42aa-8a88-3bc886453bb6)(content(Whitespace\" \
         \"))))(Tile((id \
         9a53b43a-9d0e-4d70-a23a-a59726225860)(label(New))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         919cd45d-bb35-4f2c-9e59-4896cc159f61)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         ac362b06-4c70-406f-9422-a64fb695a1d8)(content(Whitespace\" \
         \"))))(Tile((id \
         0cda3a37-63d0-485c-b4e5-3fa3fea87240)(label(1.2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         b07a43e6-13f9-4855-b39f-09bae2f3609b)(content(Whitespace\"\\n\"))))(Tile((id \
         912b52d9-fca4-48ae-8ccf-e291e88e832e)(label(| =>))(mold((out \
         Rul)(in_(Pat))(nibs(((shape(Concave 46))(sort Exp))((shape(Concave \
         46))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         7400a434-913b-465f-9db4-45e11073b596)(content(Whitespace\" \
         \"))))(Tile((id \
         921fdb70-7dc2-46ea-9d2a-7615857a5806)(label(Full))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         23bcbac4-d3c4-410e-ac43-5ce97d7c2dd3)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         64cb68cf-6230-4384-a755-e1cdecbd18a1)(content(Whitespace\" \
         \"))))(Tile((id \
         716b2fb7-7829-4d48-9b03-fc30f6b34973)(label(0.88))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         e8216d94-a8f4-4163-a696-0b9e291bf6b5)(content(Whitespace\"\\n\"))))(Tile((id \
         9709c097-19cd-48ce-be43-07172dbc8bfd)(label(| =>))(mold((out \
         Rul)(in_(Pat))(nibs(((shape(Concave 46))(sort Exp))((shape(Concave \
         46))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         106c2a78-f93b-4f4f-8285-a07ce2f25586)(content(Whitespace\" \
         \"))))(Tile((id \
         dd98f4eb-8d7b-42c8-bd39-e775002d6ee3)(label(Waxing))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         42bf926b-e578-4081-858e-41fba73ab844)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         289d52b2-4f18-4278-8aa0-bace4b3bc567)(content(Whitespace\" \
         \"))))(Tile((id \
         d94b0846-a9cd-483d-bf72-111523973861)(label(1.1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         b005ef61-1404-443c-b1d3-4b70d7d141bb)(content(Whitespace\"\\n\"))))(Tile((id \
         ff777ba6-69e1-4782-8527-1587f2ba1783)(label(| =>))(mold((out \
         Rul)(in_(Pat))(nibs(((shape(Concave 46))(sort Exp))((shape(Concave \
         46))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         23f705e4-9f44-42f7-bd02-f965eeffaf33)(content(Whitespace\" \
         \"))))(Tile((id \
         2c89944b-5501-4878-8171-96a8f021936d)(label(Waning))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         e417d792-1093-444a-9924-c0721e039beb)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         8f8811e3-df33-40a3-80b3-8cc36233f017)(content(Whitespace\" \
         \"))))(Tile((id \
         1817dbc9-4368-4f48-b9fb-b8b5c97b17c2)(label(0.95))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         fc16d0c0-740b-4cd4-8172-e26cde348d16)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         8f0bf0ec-6bd3-4fb9-906d-1438460f4c4a)(content(Whitespace\" \
         \"))))(Secondary((id \
         90c30518-ff4d-4ab2-8fd7-cb8c79e90b83)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         d63ef1a9-b02e-403c-9618-e9fabea28a39)(content(Whitespace\" \
         \"))))(Tile((id \
         0ebc5589-33fe-4e45-8fe5-5375383e51b3)(label(int_of_float))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         7de4dd2d-ef1a-47fc-9ec3-75602eed702b)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         ee5600f5-15fc-4929-bae0-363a047d0a75)(label(float_of_int))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         97d8c3f8-8c21-405c-ac06-88dd5b43159a)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         f9e74e2a-ce48-447f-b3ce-11ac98ab5bdb)(label(base_ml))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         7fd8c1b2-38bb-46c0-9086-433532809e46)(content(Whitespace\" \
         \"))))(Tile((id \
         ead6aa46-ae74-4c2c-b0a3-b539a45dad29)(label(*.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 27))(sort Exp))((shape(Concave \
         27))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         c5f706a7-1bb0-4b28-ab68-c2ba82395d2c)(content(Whitespace\" \
         \"))))(Tile((id \
         07dd1274-701b-435e-a203-84540711c546)(label(multiplier))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         f635bd24-1d14-4845-9ec1-8f297c38c994)(content(Whitespace\"\\n\"))))(Secondary((id \
         7446aaed-f6e0-4891-bf7b-14f1cd3f23b6)(content(Comment\"# Above: Hazel \
         uses C-style Function application syntax #\"))))(Secondary((id \
         37a4c2ba-db6a-4cf8-b84f-2a0263ccd041)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         5c02d0bf-983f-4286-9cb1-7df178a59f77)(content(Whitespace\"\\n\"))))(Secondary((id \
         46cd1a7b-7fdd-480d-91bf-270c7449b72e)(content(Whitespace\"\\n\"))))(Secondary((id \
         2b9cb572-30c8-4a42-bd31-ee4af6fbe1a7)(content(Comment\"# Now click \
         the samples for the 3 calls to `watering_amount` below. \
         #\"))))(Secondary((id \
         7d19f9c0-3233-49ae-84b8-306cefdc7abc)(content(Whitespace\"\\n\"))))(Secondary((id \
         0ddd5646-cff6-430e-9cf0-b820b70f5a08)(content(Comment\"# Notice the \
         sample for 'multiplier' above changes to /align/ with \
         #\"))))(Secondary((id \
         84ac5f43-872f-4ba1-99c9-fa3350cd3040)(content(Whitespace\"\\n\"))))(Secondary((id \
         6ac4e570-05d9-4ba9-9def-b4cf051b432f)(content(Comment\"# the selected \
         call! We call this behavior the 'dynamic cursor', \
         #\"))))(Secondary((id \
         93a57e43-3e11-4076-bde1-3003d4ab9ae4)(content(Whitespace\"\\n\"))))(Secondary((id \
         05313976-351b-4181-a865-8a9685c7759c)(content(Comment\"# which aligns \
         probe samples to a particular step in an execution. \
         #\"))))(Secondary((id \
         b23ceadd-8809-4816-93f1-a30e29a8bd44)(content(Whitespace\"\\n\"))))(Secondary((id \
         369949c6-861a-4562-9527-92e02cc2e579)(content(Whitespace\"\\n\"))))(Tile((id \
         81338367-01d2-4150-9c5f-2eaca4f08e35)(label(watering_amount))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         1f70203b-2d4d-48be-b765-71145287afdd)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         901b9e30-89da-4bcd-83ee-d7334242ab05)(label(250))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         10be1acf-a166-496e-b925-63f0e5187b92)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         0b0d7b79-98b5-44b1-a137-ce1bc1a1996d)(content(Whitespace\" \
         \"))))(Tile((id \
         2208e565-ddb0-47c1-a234-5f29d19443a3)(label(Full))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         e60e8c61-53f6-4825-b6bf-6b750a6877f5)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         fca39f2d-ea96-4bb3-bc09-e83941052723)(content(Whitespace\"\\n\"))))(Tile((id \
         e8865d8d-a221-499a-a241-bb5afc2d4e71)(label(watering_amount))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         eaabffb6-7117-47fe-b650-45b71c5084ea)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         4c711952-7954-4908-8d44-11fa19d9fb8c)(label(50))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         58850270-3fe0-4e8a-a00a-84cfb3605c16)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         796ddbd9-91c2-4fd1-a399-f2952f87207d)(content(Whitespace\" \
         \"))))(Tile((id \
         a14360f1-1d60-4fac-91ce-21f60f8a25a6)(label(New))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         b2116652-693e-4051-bd33-0b60fe010757)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         b33a11df-c673-4f71-8ff2-de2d2c8e8d59)(content(Whitespace\"\\n\"))))(Tile((id \
         ceaaec34-1eae-45e2-af57-45488eacebc5)(label(watering_amount))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         4eced469-ff9a-4463-989d-7778b17f8c38)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         fdf559ac-f7b4-4a23-b9af-3dc4dcab030f)(label(180))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         80ecfb60-e880-4ca6-b10e-211bc43f1081)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         9c316695-a57f-440e-a113-13f78bd64655)(content(Whitespace\" \
         \"))))(Tile((id \
         68b2aae1-a057-4d29-ab73-470eb5d4d5fb)(label(Waning))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         98948a17-9f6e-4ef9-8053-8eb1edb82aec)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         4bd85017-f2d0-4406-9c24-b9b0e712053f)(content(Whitespace\"\\n\"))))(Secondary((id \
         5c28bfcc-1847-47c5-9fe6-15d6d2ad5883)(content(Whitespace\"\\n\"))))(Secondary((id \
         1918432b-b2e6-4cd3-8df1-aaf38af5ab9c)(content(Comment\"# Below is the \
         same function as above, this time with many probes. \
         #\"))))(Secondary((id \
         a7690471-a60c-47c6-8d05-81b3ad6face2)(content(Whitespace\"\\n\"))))(Secondary((id \
         fbe039a9-8090-4883-b1cc-0f81e0068651)(content(Comment\"# Select the \
         `multiplier` sample and use the arrow keys to move \
         #\"))))(Secondary((id \
         12fc58fc-06e0-47c5-bed9-ee060c30a406)(content(Whitespace\"\\n\"))))(Secondary((id \
         c5b79fab-3200-4233-9228-c1dbce9db577)(content(Comment\"# through the \
         different values. Notice how this time, there are two \
         #\"))))(Secondary((id \
         7ce4f156-40db-430e-842f-22b1cf88392b)(content(Whitespace\"\\n\"))))(Secondary((id \
         543fc379-22aa-4708-8321-4cdf9ab17cec)(content(Comment\"# different \
         symbols next to the branches with no samples; \\226\\136\\133 from \
         #\"))))(Secondary((id \
         5924df11-6f08-41d7-a68b-d73f496a0120)(content(Whitespace\"\\n\"))))(Secondary((id \
         c8effa3c-7e93-40cb-892c-c1b0f4e2e01e)(content(Comment\"# before on \
         `Waxing`, which means never evaluated, and a new symbol \
         #\"))))(Secondary((id \
         c433b8a9-e105-48db-ba1f-dacaf2b86122)(content(Whitespace\"\\n\"))))(Secondary((id \
         b2a7c4e2-b694-4064-b88f-f0bc24f5265e)(content(Comment\"# \
         \\226\\138\\150, which means there are samples, but they are not \
         aligned to the #\"))))(Secondary((id \
         fd8a47ef-c125-40a7-b33b-84ffd81bdc2e)(content(Whitespace\"\\n\"))))(Secondary((id \
         47b3a4d5-8df9-47e0-b84f-9a3dcd2ea9ec)(content(Comment\"# dynamic \
         cursor (because of the `multiplier` sample you selected). \
         #\"))))(Secondary((id \
         4bc714b7-dcf4-48a9-b9c0-08ad840d4f5a)(content(Whitespace\"\\n\"))))(Secondary((id \
         208160cf-2551-4d9c-9fec-b133caf846b4)(content(Comment\"# Click on any \
         \\226\\138\\150 to align the dynamic cursor to that branch. \
         #\"))))(Secondary((id \
         e4998ef1-755a-4a99-bd7d-0ae82727c904)(content(Whitespace\"\\n\"))))(Secondary((id \
         5e7fb444-57a9-4bd9-b858-fab56beaf0bf)(content(Whitespace\"\\n\"))))(Tile((id \
         e9a3d720-ac05-4f22-b649-364f19e688b6)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         08dbe1d8-0518-49ae-ba8b-7926488dad67)(content(Whitespace\" \
         \"))))(Tile((id \
         21931b75-932d-4673-9c7f-f65cd1308931)(label(watering_amount))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         09586e16-1340-4bb4-8886-3c9094df98ef)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 24))(sort Pat))((shape(Concave \
         24))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         b741377f-c33a-4338-8a5f-4ee59aa26119)(content(Whitespace\" \
         \"))))(Tile((id \
         0ce9f5dc-0a82-47a9-89b7-5e1740c22d8e)(label(\"(\"\")\"))(mold((out \
         Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0 1))(children(((Tile((id \
         3a4d0fdc-85f9-4f7b-8814-4ef2ae53d8ad)(label(Int))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         f7daeefd-c7e0-45e3-bf4f-b94c3de33f9d)(label(,))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 44))(sort Typ))((shape(Concave \
         44))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         fd628d81-a6e4-4c10-a8c5-4533dce51c78)(content(Whitespace\" \
         \"))))(Tile((id \
         d9c3e369-1fbf-4337-a076-6228d7fe03fb)(label(MoonPhase))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children()))))))))(Secondary((id \
         4562b108-2fb7-4924-8341-f330b78a4035)(content(Whitespace\" \
         \"))))(Tile((id \
         eb2c9cc9-97ad-4517-addb-cf8ddf9e771b)(label(->))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 13))(sort Typ))((shape(Concave \
         13))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         594b6d46-c08d-4f62-8d1c-ac17adbe6d2e)(content(Whitespace\" \
         \"))))(Tile((id \
         a4ccf24a-46be-4c1b-bb29-473a5bad65d1)(label(Int))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         2f4856ee-7828-4ceb-8cba-60722ecde6d9)(content(Whitespace\" \
         \")))))((Secondary((id \
         d5329011-de49-4f13-a41c-d246a7403591)(content(Whitespace\"\\n\"))))(Tile((id \
         edcaa60f-48f5-4515-939b-4562a5529699)(label(fun ->))(mold((out \
         Exp)(in_(Pat))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         37))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         0ea95192-f41f-4067-8390-498edd62f4de)(content(Whitespace\" \
         \"))))(Tile((id \
         f70f8bd6-c77c-49f2-80d4-5cfcf5045b1e)(label(\"(\"\")\"))(mold((out \
         Pat)(in_(Pat))(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0 1))(children(((Tile((id \
         4d1711a5-fa25-4151-959d-b80efce9005e)(label(base_ml))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         7b3845e8-7a03-45ad-91d0-ef9b912697c4)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 44))(sort Pat))((shape(Concave \
         44))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         d40cf61d-83e4-4e6a-adce-c72676c673b6)(content(Whitespace\" \
         \"))))(Tile((id \
         363a09fc-23da-4540-b679-34b62ac69bfd)(label(phase))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children()))))))))(Secondary((id \
         d9bfa974-b5ff-4ccf-a049-7cae7180e357)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         c86781d9-718c-4d10-b2d4-485a72f84128)(content(Whitespace\"\\n\"))))(Tile((id \
         b13f69c3-677d-4852-98ed-83a1c341feb3)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         254dcacc-7956-48c3-88c0-0ad09c959639)(content(Whitespace\" \
         \"))))(Tile((id \
         7288e988-6e21-4a58-9ede-8b66b76360df)(label(multiplier))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         627a3089-6ef7-4967-811c-0dd1d122b245)(content(Whitespace\" \
         \")))))((Secondary((id \
         bc7a1a78-fdca-431d-8d39-39913049a85f)(content(Whitespace\"\\n\"))))(Tile((id \
         1b1997c4-702d-48a5-b531-6ad402c32016)(label(case end))(mold((out \
         Exp)(in_(Rul))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         8c0b0ea0-c8e2-4c59-94d6-620f1aa14135)(content(Whitespace\" \
         \"))))(Tile((id \
         9d2a25ef-9f44-4c4a-b827-f4dd0f650a09)(label(phase))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         d2806c73-90e9-4316-9440-a62dce6bf428)(content(Whitespace\"\\n\"))))(Tile((id \
         2c6922e8-9081-4b03-a2e0-0679e1c6025f)(label(| =>))(mold((out \
         Rul)(in_(Pat))(nibs(((shape(Concave 46))(sort Exp))((shape(Concave \
         46))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         6eeb0294-f87d-4fc8-9ece-bb7afb2b3712)(content(Whitespace\" \
         \"))))(Tile((id \
         5c0422eb-b1b4-4040-ba58-cb83b7fceaa7)(label(New))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         445fab02-ffee-42c2-bce0-1563a6a5623a)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         2d9f3c5d-526e-4df6-9dee-cd0d89447aec)(content(Whitespace\" \
         \"))))(Tile((id \
         dcf976ec-0701-44da-ab2d-e5ed5d961988)(label(1.2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         a7107f00-d3a1-42d5-bd88-aec08eb0ecee)(content(Whitespace\"\\n\"))))(Tile((id \
         2f95f749-d93d-4682-91e1-0a920148a6a6)(label(| =>))(mold((out \
         Rul)(in_(Pat))(nibs(((shape(Concave 46))(sort Exp))((shape(Concave \
         46))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         94fb6292-fdcb-477a-9ccc-684d860b875e)(content(Whitespace\" \
         \"))))(Tile((id \
         1e657f53-ac27-4124-8c71-f1c14b29aca0)(label(Full))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         bc3f11e3-b72f-4457-987f-7fab741ba390)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         f3ee71c7-04a9-46dc-9309-b32d4fa849d7)(content(Whitespace\" \
         \"))))(Tile((id \
         0b23a6de-a588-4a21-9ff8-c16560c0888b)(label(0.88))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         e49a63fe-7460-4a4e-8dfe-033c98dd5e1a)(content(Whitespace\"\\n\"))))(Tile((id \
         48df62ef-6edf-48c4-9d62-dcead32c5789)(label(| =>))(mold((out \
         Rul)(in_(Pat))(nibs(((shape(Concave 46))(sort Exp))((shape(Concave \
         46))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         cbffef38-ca51-4938-9630-9ca89f3b08cb)(content(Whitespace\" \
         \"))))(Tile((id \
         6804f12a-b6ba-42a3-8948-d160956fc4fc)(label(Waxing))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         5c7f101c-7520-42b7-bcf0-ff1d7bc7338a)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         6ccb6d61-295d-495e-8de6-3ae1ec39dd86)(content(Whitespace\" \
         \"))))(Tile((id \
         011b78b7-83ac-4300-bae4-a65adabeed81)(label(1.1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         6714238e-d5d4-403f-94dd-4d2df2ded7ce)(content(Whitespace\"\\n\"))))(Tile((id \
         e7ed8877-7b94-4c5c-8a73-9cb168e41802)(label(| =>))(mold((out \
         Rul)(in_(Pat))(nibs(((shape(Concave 46))(sort Exp))((shape(Concave \
         46))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         f52a611d-b0ef-41b5-9250-af0302a592aa)(content(Whitespace\" \
         \"))))(Tile((id \
         2ff2e8ce-01a0-4729-9db9-2de04a8bee62)(label(Waning))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         73279193-b650-4cb1-a43b-85492c2c59a7)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         4b0e2242-59e7-4781-9906-ae7a4e347e0e)(content(Whitespace\" \
         \"))))(Tile((id \
         7ac8e277-b19f-4a8e-b6f9-814a2ea17a97)(label(0.95))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         23b1b3fa-532a-4b36-be4e-356b2f71e516)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         345aec13-41a3-42f8-846e-650b26e44465)(content(Whitespace\" \
         \"))))(Secondary((id \
         2d4970f6-b61c-4e72-ba7d-def332fd40d0)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         d2a0f777-a507-4c03-ac02-710ca8d34135)(content(Whitespace\" \
         \"))))(Tile((id \
         0af3cd7f-6325-4380-afe1-7b787fb27766)(label(int_of_float))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         4b481700-385f-44c1-9520-cc87c54ab6af)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         e513ffa0-fb23-4a50-9fdc-6597f1e33e80)(label(float_of_int))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         b1bd2792-e167-4c1e-8ace-f17d2ab6acad)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         3aa79dad-685c-46e0-8f33-6502719e443e)(label(base_ml))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         d092884a-1d87-4036-9f8e-fd4367f3494d)(content(Whitespace\" \
         \"))))(Tile((id \
         f95ff478-cdfe-4a3f-9240-90c2c614d71d)(label(*.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 27))(sort Exp))((shape(Concave \
         27))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         9886fd23-1383-4d6e-aeb9-d23c5283b282)(content(Whitespace\" \
         \"))))(Tile((id \
         99d4de46-f4fb-4419-97e3-66defebe966a)(label(multiplier))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         218fcd24-f229-457d-ab4b-abff2a5c279e)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         6f7e0c09-e8d4-412a-a5c2-c9b204f0ec56)(content(Whitespace\"\\n\"))))(Secondary((id \
         9667b054-2491-4ec5-a745-9f80fd04ceb1)(content(Whitespace\"\\n\"))))(Secondary((id \
         c969984d-970d-4f04-9c30-d1291aa89e52)(content(Comment\"# TAKEAWAY: \
         The dynamic cursor is an internal mechanism which \
         #\"))))(Secondary((id \
         30d3f3e1-fb1c-449c-aca5-c0eab77c95f2)(content(Whitespace\"\\n\"))))(Secondary((id \
         98312cb0-7a4e-4591-ae56-7d5e3b784457)(content(Comment\"# tries to \
         keep the probe samples shown aligned to the same \
         #\"))))(Secondary((id \
         f580929f-5925-440a-b431-7551bcc6e87b)(content(Whitespace\"\\n\"))))(Secondary((id \
         6ce70d88-fabc-47ff-812f-01360e00cde2)(content(Comment\"# execution, \
         in particular the same call to a function. #\"))))(Secondary((id \
         ef8df275-c600-4957-9554-6ee7d9183d6d)(content(Whitespace\"\\n\"))))(Secondary((id \
         3a46d402-accb-4243-9c3f-8c3d64034ded)(content(Whitespace\"\\n\"))))(Tile((id \
         9d619bde-3b62-40fa-939b-08419405f2ed)(label(watering_amount))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         a8f30e1b-35bd-4150-b718-1100119fe5cc)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         d96af7b6-63c9-45fb-9ddf-d59873acd300)(label(250))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         ea969921-2cea-4a68-8819-382e109e1055)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         9b68d14d-d35a-4bcf-bb27-103e153d13d2)(content(Whitespace\" \
         \"))))(Tile((id \
         6f148d9e-47b5-4a5a-a089-523a1c1e0279)(label(Full))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         3062650b-4087-4fa7-af09-30f8bfe53702)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         aec7e338-c7a8-4f8d-a9e7-e7d1098059a2)(content(Whitespace\"\\n\"))))(Tile((id \
         81f4004a-6323-4a2c-a22d-988d086c9aea)(label(watering_amount))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         93d6fc5c-f5a0-4de6-b06a-60dea1771155)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         f3e5e998-9e9a-449f-bab6-db27f9230b63)(label(50))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         acf8e20f-542f-427f-92f1-36a622bc1bd0)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         32285ba0-31f3-4fa3-9eeb-ffc5f30bd336)(content(Whitespace\" \
         \"))))(Tile((id \
         b601b44c-7b48-4749-9610-236543a3f770)(label(New))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         46d6eda9-13be-45ce-a4a0-778babbb67b9)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         c220c2f5-b6b3-4293-bfb6-ff3b2195adc0)(content(Whitespace\"\\n\"))))(Tile((id \
         c607637a-460a-4028-9792-409df2b40a02)(label(watering_amount))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         63bd7e34-8a64-4780-9e28-921ec4202d19)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         c30985f0-c968-45c1-80a6-e189d16e426b)(label(180))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         d7464755-8703-40a5-a70e-2a8d1cfdd14a)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         5f449436-20e8-47a1-ae54-d2e9af9ddf2a)(content(Whitespace\" \
         \"))))(Tile((id \
         530e91a3-ac01-4e61-a1db-a3f41cce3162)(label(Waning))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         adfae657-6985-4cc9-ac90-b5bc75f1f9ff)(content(Whitespace\"\\n\"))))(Secondary((id \
         5b931eaa-53bb-470d-9186-eb64b41000c0)(content(Whitespace\"\\n\"))))(Secondary((id \
         ea709a02-5426-40b3-bb91-55ea766591b7)(content(Comment\"# One last \
         thing: SINGLE MODE (default) vs MANY MODE #\"))))(Secondary((id \
         bdeabb1b-6f02-4dc6-b415-de8339f3afd3)(content(Whitespace\"\\n\"))))(Secondary((id \
         31007997-ab25-44dc-9302-6f366036d701)(content(Comment\"# Double-click \
         any above sample, or press Space when a sample #\"))))(Secondary((id \
         147c0fcf-64b0-467b-8bb2-0b24fcc7f819)(content(Whitespace\"\\n\"))))(Secondary((id \
         955ad4a8-3616-45c1-94bd-428b16011ee1)(content(Comment\"# is selected \
         to toggle Many mode: all samples are shown at once! \
         #\"))))(Secondary((id \
         cee9123b-3345-48dd-8826-738d8f5a81bd)(content(Whitespace\"\\n\"))))(Secondary((id \
         bbeacb26-c3d4-403f-b6a1-72733520988d)(content(Comment\"# Similarly to \
         single mode, left/right arrow keys move samples. \
         #\"))))(Secondary((id \
         719c0ae2-17ff-4f8f-977f-c04bf7c8a61e)(content(Whitespace\"\\n\"))))(Secondary((id \
         0b7b16de-2157-44c0-87ae-b596d1c3f26a)(content(Comment\"# Double-click \
         again (or Space) to go back to Single mode. #\"))))(Secondary((id \
         73f1db05-7942-4750-bc43-daf51362071b)(content(Whitespace\"\\n\"))))(Secondary((id \
         c712ba6c-c0ee-4958-b17f-b3d94aeaa4ee)(content(Whitespace\"\\n\"))))(Secondary((id \
         81c8a86b-d792-4966-a8c3-e24d5748b4af)(content(Comment\"# END OF PART \
         2 - Select the next slide from the top menu #\"))))(Secondary((id \
         e510342f-7c95-48dd-bf92-9770d47e12d8)(content(Whitespace\"\\n\")))))";
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
        "((4b481700-385f-44c1-9520-cc87c54ab6af((kind \
         Probe)(model\"((active_renderer())(drawer_mode false)(dropdown_redraw \
         0))\")))(7ac8e277-b19f-4a8e-b6f9-814a2ea17a97((kind \
         Probe)(model\"((active_renderer())(drawer_mode false)(dropdown_redraw \
         0))\")))(011b78b7-83ac-4300-bae4-a65adabeed81((kind \
         Probe)(model\"((active_renderer())(drawer_mode false)(dropdown_redraw \
         0))\")))(0b23a6de-a588-4a21-9ff8-c16560c0888b((kind \
         Probe)(model\"((active_renderer())(drawer_mode false)(dropdown_redraw \
         0))\")))(dcf976ec-0701-44da-ab2d-e5ed5d961988((kind \
         Probe)(model\"((active_renderer())(drawer_mode false)(dropdown_redraw \
         0))\")))(9d2a25ef-9f44-4c4a-b827-f4dd0f650a09((kind \
         Probe)(model\"((active_renderer())(drawer_mode false)(dropdown_redraw \
         0))\")))(7288e988-6e21-4a58-9ede-8b66b76360df((kind \
         Probe)(model\"((active_renderer())(drawer_mode false)(dropdown_redraw \
         0))\")))(4eced469-ff9a-4463-989d-7778b17f8c38((kind \
         Probe)(model\"((active_renderer())(drawer_mode false)(dropdown_redraw \
         0))\")))(eaabffb6-7117-47fe-b650-45b71c5084ea((kind \
         Probe)(model\"((active_renderer())(drawer_mode false)(dropdown_redraw \
         0))\")))(1f70203b-2d4d-48be-b765-71145287afdd((kind \
         Probe)(model\"((active_renderer())(drawer_mode false)(dropdown_redraw \
         0))\"))))";
    } )
