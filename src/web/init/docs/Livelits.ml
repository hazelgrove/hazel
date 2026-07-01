let out : string * Haz3lcore.PersistentSegment.t =
  ( "Livelits",
    {
      segment =
        "((Secondary((id \
         cebfac6c-b687-41f4-a0a2-9db408761ac8)(content(Comment\"# LIVELITS \
         #\"))))(Secondary((id \
         b89569da-d157-4e94-8098-c7fc9e229f51)(content(Whitespace\"\\n\"))))(Secondary((id \
         c1ec4b61-4ef4-446a-b975-b669563d991e)(content(Comment\"# A livelit is \
         a live GUI widget which can be inserted into expressions. \
         #\"))))(Secondary((id \
         db3b24e1-3cf0-4bb0-b037-531385c1284c)(content(Whitespace\"\\n\"))))(Secondary((id \
         1491e462-9b7b-4c26-a519-df29c2c35216)(content(Comment\"# It \
         elaborates to a value of some given type. #\"))))(Secondary((id \
         aea180b5-e3ba-4318-a1b2-f13f6ee9fb42)(content(Whitespace\"\\n\"))))(Secondary((id \
         ad4010b8-8e26-4a24-99bc-60df8545b445)(content(Whitespace\"\\n\"))))(Secondary((id \
         ed4d571c-b68d-44e1-9cf4-da3b9aafae6d)(content(Comment\"# Invocation: \
         #\"))))(Secondary((id \
         247760ba-71da-4e63-b768-8bb9c4ab76ff)(content(Whitespace\"\\n\"))))(Secondary((id \
         1f473891-73a1-496c-8154-c5fab3923b3d)(content(Comment\"# To invoke a \
         livelit, insert the name of the livelit (always prefixed with ^) then \
         space. #\"))))(Secondary((id \
         41e0b353-7410-44c3-a27e-7dd4dc954237)(content(Whitespace\"\\n\"))))(Secondary((id \
         5e4a81a6-f8f4-49e7-be25-c4bf04c5e7c9)(content(Comment\"# The context \
         inspector shows which livelits are available and their elaboration \
         type. #\"))))(Secondary((id \
         68271038-1686-4bdb-a049-f9826f312c6f)(content(Whitespace\"\\n\"))))(Secondary((id \
         de5033cc-1778-4aea-bdbb-54e131090123)(content(Whitespace\"\\n\"))))(Secondary((id \
         54238b43-9714-4355-a789-d7399786a240)(content(Comment\"# Each livelit \
         maintains an internal model, which is not generally something the \
         #\"))))(Secondary((id \
         5ffdb892-d049-4a1b-a35b-92125ca54483)(content(Whitespace\"\\n\"))))(Secondary((id \
         d09b4987-7a28-46c4-a8f0-0b22ec749e71)(content(Comment\"# programmer \
         should need to look at. You can see the internal model by toggling \
         #\"))))(Secondary((id \
         a668e482-44e5-42a7-8c5b-f97c9c837671)(content(Whitespace\"\\n\"))))(Secondary((id \
         2f950a28-2208-49a3-8e00-420360b768cc)(content(Comment\"# the livelit \
         projector off (bottom right corner of Hazel). #\"))))(Secondary((id \
         816cbe18-f500-4059-a400-6c79011aeab4)(content(Whitespace\"\\n\"))))(Secondary((id \
         47863b00-9a11-4264-a46f-8a44f7b2fa0b)(content(Whitespace\"\\n\"))))(Secondary((id \
         a0d6764f-ea06-42da-9941-ec4232503a45)(content(Comment\"# Built-in \
         Livelits: #\"))))(Secondary((id \
         0c3c7815-dbb2-40a0-82c4-5fc269fb85f8)(content(Whitespace\"\\n\"))))(Secondary((id \
         30063aed-fb67-4949-a04f-22baf7c82bc7)(content(Comment\"# Currently, \
         all livelits are built in: #\"))))(Secondary((id \
         e7e6a32d-15e5-45da-8e59-1f5dfcc65318)(content(Whitespace\"\\n\"))))(Secondary((id \
         155843ee-37ce-401b-857d-f376fe68b767)(content(Whitespace\"\\n\"))))(Secondary((id \
         0088b5da-8c06-4275-9ad6-0ae0a871b82b)(content(Comment\"# 1. ^slider \
         #\"))))(Secondary((id \
         6db0ecc7-04aa-4849-b0d6-1491fd87dbcc)(content(Whitespace\"\\n\"))))(Secondary((id \
         6d202b95-d300-4f35-912a-183ec07290e9)(content(Comment\"# Elaboration \
         Type: Int                                                     \
         #\"))))(Secondary((id \
         8a4783a7-7f5d-4072-9446-0e066a777546)(content(Whitespace\"\\n\"))))(Secondary((id \
         942d9a37-b2f9-4b12-98d7-ce5adb843925)(content(Comment\"# Elaboration: \
         The current value of the slider, which is between 0 and 100. \
         #\"))))(Secondary((id \
         d1e43a02-f45d-48b5-b8db-c1d74a2c0981)(content(Whitespace\"\\n\"))))(Secondary((id \
         0f661e72-ece3-48c5-b2f9-69c5d9266fd3)(content(Comment\"# Internal \
         Model: Int                                                       \
         #\"))))(Secondary((id \
         5cc15482-8ca9-45dd-aaeb-486ca0f0a2c3)(content(Whitespace\"\\n\"))))(Tile((id \
         887ed128-0d40-4647-b304-cf5064c0faba)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         6e0acb3c-2dda-4971-8329-d567e76662cd)(content(Whitespace\" \
         \"))))(Tile((id \
         06a767e3-5bf2-4031-887f-f708201a6d2a)(label(slider_val))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         ee2932b9-f5b8-4dd1-b9dc-7c42e6f929d3)(content(Whitespace\" \
         \")))))((Secondary((id \
         3dbc90e0-cf73-4b58-8c03-d4c56227c327)(content(Whitespace\" \
         \"))))(Projector((id e5ffa981-7049-4676-a334-90c13f4852d2)(kind \
         Livelit)(syntax(Exp((term(Ap Forward((term(LivelitName \
         slider))(annotation((ids(bf3672af-049c-42f4-b50c-9b39cc0ba646))(secondary(()())))))((term(Atom(Int \
         85)))(annotation((ids(ffe716e6-0893-43c4-bbfb-224fc8724a74))(secondary(()())))))))(annotation((ids(cfe83640-a571-4bea-93b8-b3a344a83d66))(secondary(()())))))))(model\"()\")))(Secondary((id \
         84313cf5-10c4-4021-ac50-c6bd068a50a6)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         734cc9dc-e675-4096-a457-bd7a4c8a9b75)(content(Whitespace\"\\n\"))))(Secondary((id \
         d6122f31-f627-495b-9d7b-84ffed55328a)(content(Whitespace\"\\n\"))))(Secondary((id \
         0a72c1e1-7930-426b-9785-428bccd2be4f)(content(Comment\"# 2. ^emotion \
         #\"))))(Secondary((id \
         1dde6aa5-0c88-4af9-a980-4870b7c1f8b0)(content(Whitespace\"\\n\"))))(Secondary((id \
         f1b95711-6f5f-4953-a866-432b1cec57ec)(content(Comment\"# Elaboration \
         Type: String                                      \
         #\"))))(Secondary((id \
         ca505795-5c41-4445-a376-73811f21783f)(content(Whitespace\"\\n\"))))(Secondary((id \
         e6f03478-0ffa-409a-987b-b4e09e44866b)(content(Comment\"# Elaboration: \
         Returns a String representing an emotion,        \
         #\"))))(Secondary((id \
         6a9df222-fed7-4027-913b-1710c52f18a0)(content(Whitespace\"\\n\"))))(Secondary((id \
         192b7ae1-32af-4357-bdce-b198cc8c1e7b)(content(Comment\"#              \
         determined as follows based on the slider value: \
         #\"))))(Secondary((id \
         eb980b87-52fe-4409-8191-73c82091684b)(content(Whitespace\"\\n\"))))(Secondary((id \
         45911e1d-8d9e-4f89-b766-064bf63f2898)(content(Comment\"#                \
         if value < 40 then \\\"sad\\\"                       \
         #\"))))(Secondary((id \
         97716e51-7f74-4677-a450-4d126bd8a325)(content(Whitespace\"\\n\"))))(Secondary((id \
         f7e30b5f-4d4a-4c4b-a1b0-02c653deca10)(content(Comment\"#                \
         if value > 70 then \\\"happy\\\"                     \
         #\"))))(Secondary((id \
         8844b9ed-96d1-4a96-a8f7-aedfe90612e0)(content(Whitespace\"\\n\"))))(Secondary((id \
         88c11290-4172-4dd7-8104-d9b05b92118c)(content(Comment\"#                \
         otherwise \\\"neutral\\\"                            \
         #\"))))(Secondary((id \
         b7cd6715-f407-4305-98d9-2886b318ddf1)(content(Whitespace\"\\n\"))))(Secondary((id \
         c7c65534-a83c-4b9e-9101-5d40b5426c01)(content(Comment\"# Internal \
         Model: Int                                           \
         #\"))))(Secondary((id \
         e26c1f66-5c28-4e11-b9e8-e9cf2aa90b6c)(content(Whitespace\"\\n\"))))(Tile((id \
         9c650120-2be1-4bb7-97f0-4f5079c05d71)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         0700606e-66b6-4ed7-9edc-1c9cb29f1da9)(content(Whitespace\" \
         \"))))(Tile((id \
         9c0f9682-aab3-4bfc-ac1b-fd166b883aa8)(label(current_mood))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         75413e58-8622-4b72-8b60-382016947161)(content(Whitespace\" \
         \")))))((Secondary((id \
         852bee23-f72a-4a2b-8e34-7a8ad7a4dc56)(content(Whitespace\" \
         \"))))(Projector((id 77c39c78-6fe2-4c73-b5b4-48b9252a90d4)(kind \
         Livelit)(syntax(Exp((term(Ap Forward((term(LivelitName \
         emotion))(annotation((ids(771abfd7-0a2d-4dae-bfff-e14de2fa533b))(secondary(()())))))((term(Atom(Int \
         0)))(annotation((ids(e524ffa7-b02f-4924-a97f-1d12e4076b20))(secondary(()())))))))(annotation((ids(e1d2116a-61e8-4fe5-9a6b-ffea7712d700))(secondary(()())))))))(model\"()\")))(Secondary((id \
         9ab131db-1ed2-4280-be01-7798317c8d75)(content(Whitespace\" \
         \")))))))))(Grout((id 3bdf24d3-79d6-47d3-a4f5-a1d1308729b1)(shape \
         Convex))))";
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
         let slider_val = ^^livelit(^slider(85)) in\n\n\
         # 2. ^emotion #\n\
         # Elaboration Type: String                                      #\n\
         # Elaboration: Returns a String representing an emotion,        #\n\
         #              determined as follows based on the slider value: #\n\
         #                if value < 40 then \"sad\"                       #\n\
         #                if value > 70 then \"happy\"                     #\n\
         #                otherwise \"neutral\"                            #\n\
         # Internal Model: Int                                           #\n\
         let current_mood = ^^livelit(^emotion(0)) in";
      refractors = "()";
    } )
