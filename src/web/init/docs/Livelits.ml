let out : string * Haz3lcore.PersistentZipper.t =
  ( "Livelits",
    {
      zipper =
        "((selection((focus Left)(content())(mode \
         Normal)))(backpack())(relatives((siblings(((Secondary((id \
         adbb13f2-dda5-4827-856d-d683ed77d43d)(content(Whitespace\" \
         \")))))((Projector((id 091a3fca-9d05-46e9-bc8f-c8e895357a1b)(kind \
         Livelit)(syntax(Tile((id \
         b64b6142-8d63-4aa2-bc4f-b122ef7c7052)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         5b9b61da-8fb4-4b23-9405-7bc799db9a1e)(label(^emotion))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         a10289de-f247-48b8-876f-b82602102c17)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         5068e232-45b7-4e3f-acb6-8fef55cd96ad)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))))))))(model\"()\")))(Secondary((id \
         8c3b94ad-09b1-4730-9a60-42a3cd36473a)(content(Whitespace\" \
         \")))))))(ancestors((((id \
         36efe4ff-3e44-42a5-b1c7-19ad310f9f6c)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         40))(sort Exp))))))(shards((0 1)(2)))(children((((Secondary((id \
         b06aa76c-5b97-4849-ab5a-32708bb8e273)(content(Whitespace\" \
         \"))))(Tile((id \
         0851722c-3396-450d-b203-50a974ddc840)(label(current_mood))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         7177c286-45fb-4b37-9ebb-7f86618b32c8)(content(Whitespace\" \
         \"))))))())))(((Secondary((id \
         3005274d-401c-4f0e-86fd-0db8bdac94e0)(content(Comment\"# LIVELITS \
         #\"))))(Secondary((id \
         e770a6f0-1c31-48e6-8849-a8537fc1ae97)(content(Whitespace\"\\n\"))))(Secondary((id \
         4df2df05-603d-476a-a099-37759584d17d)(content(Comment\"# A livelit is \
         a live GUI widget which can be inserted into expressions. \
         #\"))))(Secondary((id \
         4736f0ee-93b3-4df7-9b75-1637c239320f)(content(Whitespace\"\\n\"))))(Secondary((id \
         cad8d72d-804a-4670-b815-91b42fb7fce0)(content(Comment\"# It \
         elaborates to a value of some given type. #\"))))(Secondary((id \
         8695d085-a636-4a35-8844-3a6718aec8aa)(content(Whitespace\"\\n\"))))(Secondary((id \
         f1c145d2-0af1-43dc-96a9-94601815f08c)(content(Whitespace\"\\n\"))))(Secondary((id \
         a8af02d6-4b66-4eb0-9e90-9b8491bae718)(content(Comment\"# Invocation: \
         #\"))))(Secondary((id \
         7711182c-ed76-4c1e-8913-01a99eeafe20)(content(Whitespace\"\\n\"))))(Secondary((id \
         5996be3c-7feb-4916-90ae-1f56801a9b5a)(content(Comment\"# To invoke a \
         livelit, insert the name of the livelit (always prefixed with ^) then \
         space. #\"))))(Secondary((id \
         6e264f2e-a137-4872-b381-e16d4a027152)(content(Whitespace\"\\n\"))))(Secondary((id \
         d128bd58-50fa-4632-9054-a2e002cd95c1)(content(Comment\"# The context \
         inspector shows which livelits are available and their elaboration \
         type. #\"))))(Secondary((id \
         6374782d-626e-47a7-85d2-f4016ed03a16)(content(Whitespace\"\\n\"))))(Secondary((id \
         f2341ef8-915f-4937-b780-a7d6e4fbecca)(content(Whitespace\"\\n\"))))(Secondary((id \
         7cb8c019-d66a-4b51-b242-47d6cf5ef830)(content(Comment\"# Each livelit \
         maintains an internal model, which is not generally something the \
         #\"))))(Secondary((id \
         5bd17a66-d8a9-4eaf-a825-60701b2f69c2)(content(Whitespace\"\\n\"))))(Secondary((id \
         dfcb8886-5ed8-4fea-a5d6-b6f3be1ffc73)(content(Comment\"# programmer \
         should need to look at. You can see the internal model by toggling \
         #\"))))(Secondary((id \
         e99eea8e-3292-4b64-bc5c-72d7c0ec3e11)(content(Whitespace\"\\n\"))))(Secondary((id \
         574caebf-075c-4309-8ca7-7c8158d85f35)(content(Comment\"# the livelit \
         projector off (bottom right corner of Hazel). #\"))))(Secondary((id \
         12b16056-9dfe-4f82-88ca-1d74c1ed6fec)(content(Whitespace\"\\n\"))))(Secondary((id \
         01ee69ba-2c85-433d-aa8b-98332e47ad2e)(content(Whitespace\"\\n\"))))(Secondary((id \
         534aeee3-8402-453e-b670-c619825176f3)(content(Comment\"# Built-in \
         Livelits: #\"))))(Secondary((id \
         47e71da3-f21f-454d-aae5-905dc7d9f392)(content(Whitespace\"\\n\"))))(Secondary((id \
         34d69f96-afe6-4afd-9cad-69ac9ce27e0e)(content(Comment\"# Currently, \
         all livelits are built in: #\"))))(Secondary((id \
         913effcc-43b0-46d2-9492-045f1020b3d8)(content(Whitespace\"\\n\"))))(Secondary((id \
         c1f048b3-2b1a-428c-b824-abc424ce31ab)(content(Whitespace\"\\n\"))))(Secondary((id \
         91eaea56-cc5a-4ae5-b872-75aac87b334b)(content(Comment\"# 1. ^slider \
         #\"))))(Secondary((id \
         da6d18e6-f6b2-4003-8c69-891ada0b8e2a)(content(Whitespace\"\\n\"))))(Secondary((id \
         1c5daf02-d135-400a-a42d-92016ed05981)(content(Comment\"# Elaboration \
         Type: Int                                                     \
         #\"))))(Secondary((id \
         49277937-2206-4663-bb5a-a2bc8551d148)(content(Whitespace\"\\n\"))))(Secondary((id \
         b9902d0f-51bd-40ea-bc51-24e610bf06de)(content(Comment\"# Elaboration: \
         The current value of the slider, which is between 0 and 100. \
         #\"))))(Secondary((id \
         2b531514-e166-4ffe-ae97-668aac8564b9)(content(Whitespace\"\\n\"))))(Secondary((id \
         303abd63-53a6-446c-ba52-936b135a0c8d)(content(Comment\"# Internal \
         Model: Int                                                       \
         #\"))))(Secondary((id \
         772e5ad9-c6e8-4293-a05f-99808210cd6d)(content(Whitespace\"\\n\"))))(Tile((id \
         19fbb9da-8d3e-4ea0-9ed2-0ff16da5344b)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         40))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         f8a9597e-c90e-422a-90a9-7cc30eabe2dd)(content(Whitespace\" \
         \"))))(Tile((id \
         3041aaa7-5e55-48d3-81cf-88258d902fc6)(label(slider_val))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         72e0a216-e9a2-4c14-a408-2e24a0c12997)(content(Whitespace\" \
         \")))))((Secondary((id \
         d086c1aa-bfb5-4be2-abca-ede5986df0dd)(content(Whitespace\" \
         \"))))(Projector((id 320d76ce-d278-4e4e-8e14-cbd5cdd65c4f)(kind \
         Livelit)(syntax(Tile((id \
         6f3e7585-5362-4e9c-b64f-b62e02f94024)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         20c49622-e75a-44c0-bd25-4bd02ac01f4e)(label(^slider))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         28586226-d528-4928-a9b2-cb00bba65e7b)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         fea5fd2c-b04a-44d7-b832-da614caa19fa)(label(85))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))))))))(model\"()\")))(Secondary((id \
         85e87edc-017d-47ae-ab37-acec51f901d4)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         993f1fd8-13b4-43f5-85ce-3e5042b427d6)(content(Whitespace\"\\n\"))))(Secondary((id \
         28288e79-fffa-40f7-b9ff-3e9fdc431643)(content(Whitespace\"\\n\"))))(Secondary((id \
         1443ea1f-f696-4e52-8560-67a3e7bde235)(content(Comment\"# 2. ^emotion \
         #\"))))(Secondary((id \
         03131227-9337-4716-a84a-50bc4ee6818e)(content(Whitespace\"\\n\"))))(Secondary((id \
         82ffe99a-4da3-4984-a6ea-b74a4e1352e4)(content(Comment\"# Elaboration \
         Type: String                                      \
         #\"))))(Secondary((id \
         5ae1b733-3c53-4026-bf65-fc4bad1be31e)(content(Whitespace\"\\n\"))))(Secondary((id \
         8366b589-850f-4213-8d54-fd8d591442d8)(content(Comment\"# Elaboration: \
         Returns a String representing an emotion,        \
         #\"))))(Secondary((id \
         34c98b30-bcb1-4893-9623-925763868481)(content(Whitespace\"\\n\"))))(Secondary((id \
         f2051a48-d5fa-465c-a2e6-5c2eddb9f3f9)(content(Comment\"#              \
         determined as follows based on the slider value: \
         #\"))))(Secondary((id \
         ec632e82-5805-487b-9329-1acb91e6238f)(content(Whitespace\"\\n\"))))(Secondary((id \
         dad52c0a-7a85-4b88-b5a1-b96b4bf376ad)(content(Comment\"#                \
         if value < 40 then \\\"sad\\\"                       \
         #\"))))(Secondary((id \
         76302cdc-81fa-4a16-88c5-24d24052b12d)(content(Whitespace\"\\n\"))))(Secondary((id \
         77110d1a-c672-471f-b816-75d20eedf7b1)(content(Comment\"#                \
         if value > 70 then \\\"happy\\\"                     \
         #\"))))(Secondary((id \
         34d5f8c6-ee84-4250-ae3d-dd0e4a7d970d)(content(Whitespace\"\\n\"))))(Secondary((id \
         5bb12d00-6eef-4486-97ff-20e3a2b6888f)(content(Comment\"#                \
         otherwise \\\"neutral\\\"                            \
         #\"))))(Secondary((id \
         58ac837d-dc43-47cf-b3d5-13f2da0d2507)(content(Whitespace\"\\n\"))))(Secondary((id \
         300a8ef7-2545-42e2-b13f-e24efe93ce66)(content(Comment\"# Internal \
         Model: Int                                           \
         #\"))))(Secondary((id \
         c5f889ea-3490-4136-9ede-9d182088645a)(content(Whitespace\"\\n\")))))((Grout((id \
         f11cf66a-5714-479c-99f4-d02782b1afdc)(shape Convex))))))))))(caret \
         Outer))";
      backup_text =
        "# LIVELITS #\n\
         # A livelit is a live GUI widget which can be inserted into \
         expressions. #\n\
         # It elaborates to a value of some given type. #\n\n\
         # Invocation: #\n\
         # To invoke a livelit, insert the name of the livelit (always \
         prefixed with ^) then space. #\n\
         # The context inspector shows which livelits are available and their \
         elaboration type. #\n\n\
         # Each livelit maintains an internal model, which is not generally \
         something the #\n\
         # programmer should need to look at. You can see the internal model \
         by toggling #\n\
         # the livelit projector off (bottom right corner of Hazel). #\n\n\
         # Built-in Livelits: #\n\
         # Currently, all livelits are built in: #\n\n\
         # 1. ^slider #\n\
         # Elaboration Type: \
         Int                                                     #\n\
         # Elaboration: The current value of the slider, which is between 0 \
         and 100. #\n\
         # Internal Model: \
         Int                                                       #\n\
         let slider_val = ^slider(85) in\n\n\
         # 2. ^emotion #\n\
         # Elaboration Type: String                                      #\n\
         # Elaboration: Returns a String representing an emotion,        #\n\
         #              determined as follows based on the slider value: #\n\
         #                if value < 40 then \"sad\"                       #\n\
         #                if value > 70 then \"happy\"                     #\n\
         #                otherwise \"neutral\"                            #\n\
         # Internal Model: Int                                           #\n\
         let current_mood = ^emotion(0) in ";
    } )
