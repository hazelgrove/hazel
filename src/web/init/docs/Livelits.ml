let out : string * Haz3lcore.PersistentSegment.t =
  ( "Livelits",
    {
      segment =
        "((Secondary((id \
         6544a561-7cfc-425a-8563-22c6f3274ef7)(content(Comment\"# LIVELITS \
         #\"))))(Secondary((id \
         110c250a-d146-4a14-a076-83845a751f01)(content(Whitespace\"\\n\"))))(Secondary((id \
         f26ed6f4-ddcd-4bf6-8f59-cb47b3ad612b)(content(Comment\"# A livelit is \
         a live GUI widget which can be inserted into expressions. \
         #\"))))(Secondary((id \
         66bfa5b9-9f48-4464-b116-ebe31a90031a)(content(Whitespace\"\\n\"))))(Secondary((id \
         e9280917-09bc-404c-a81f-166d46e6159f)(content(Comment\"# It \
         elaborates to a value of some given type. #\"))))(Secondary((id \
         4b55c78b-07f3-4527-aae4-007004a3dfb6)(content(Whitespace\"\\n\"))))(Secondary((id \
         103a0fbb-0457-492e-a176-d2931a567e03)(content(Whitespace\"\\n\"))))(Secondary((id \
         7dd28731-edd8-41d0-bbb1-c37db7e0779d)(content(Comment\"# Invocation: \
         #\"))))(Secondary((id \
         7f223f43-2327-49f2-a60b-9ef1ae80585d)(content(Whitespace\"\\n\"))))(Secondary((id \
         07d4de86-9142-4f00-8a71-03136c62e938)(content(Comment\"# To invoke a \
         livelit, insert the name of the livelit (always prefixed with ^) then \
         space. #\"))))(Secondary((id \
         2b13e88c-a767-465e-9c99-01011ad458bb)(content(Whitespace\"\\n\"))))(Secondary((id \
         d51c76d0-88a1-429f-8c4b-cf19c512a3bd)(content(Comment\"# The context \
         inspector shows which livelits are available and their elaboration \
         type. #\"))))(Secondary((id \
         4f956332-c019-4f9b-9943-f0e09b9d65e9)(content(Whitespace\"\\n\"))))(Secondary((id \
         e6818b18-7e26-4d07-8239-5108ea0671fb)(content(Whitespace\"\\n\"))))(Secondary((id \
         75326974-0bcf-49bc-a8b0-bbb27636d72c)(content(Comment\"# Each livelit \
         maintains an internal model, which is not generally something the \
         #\"))))(Secondary((id \
         e2c9ab09-dd90-46aa-811a-e278e08677d1)(content(Whitespace\"\\n\"))))(Secondary((id \
         6b1d911c-c8c7-4460-9946-569f095eac04)(content(Comment\"# programmer \
         should need to look at. You can see the internal model by toggling \
         #\"))))(Secondary((id \
         42f5023a-d68e-4e9c-8794-8026e3c9f9b2)(content(Whitespace\"\\n\"))))(Secondary((id \
         63373fe4-0963-4387-8cbf-05b815c9d184)(content(Comment\"# the livelit \
         projector off (bottom right corner of Hazel). #\"))))(Secondary((id \
         c034136c-2b94-4d13-8189-313bf8961cc1)(content(Whitespace\"\\n\"))))(Secondary((id \
         14798745-5f33-4ab7-9e72-db7461ac73d8)(content(Whitespace\"\\n\"))))(Secondary((id \
         8edd8d86-53a2-4e04-8a8a-ea90bb9ee9ab)(content(Comment\"# Built-in \
         Livelits: #\"))))(Secondary((id \
         0544296c-a833-42fa-b652-d47a1b7a536d)(content(Whitespace\"\\n\"))))(Secondary((id \
         f2d5f6d7-42de-4ccc-81af-701b8bd2bb0f)(content(Comment\"# Currently, \
         all livelits are built in: #\"))))(Secondary((id \
         6b12bb77-30e7-41a4-b6e6-190f645a7889)(content(Whitespace\"\\n\"))))(Secondary((id \
         b1bb74c6-9583-4fd6-89fd-f5a8001dfab8)(content(Whitespace\"\\n\"))))(Secondary((id \
         b5425822-bf52-4e78-bcec-8195e6a2e5a8)(content(Comment\"# 1. ^slider \
         #\"))))(Secondary((id \
         0ed0de54-6180-4ae2-8ee2-9df139e3bd10)(content(Whitespace\"\\n\"))))(Secondary((id \
         25e171c3-83c6-4eb8-b193-668458ed33d6)(content(Comment\"# Elaboration \
         Type: Int                                                     \
         #\"))))(Secondary((id \
         9f590566-e8a7-4f08-9965-c485a447151e)(content(Whitespace\"\\n\"))))(Secondary((id \
         e6e60d63-7f69-4e54-9773-ec2213cc9b36)(content(Comment\"# Elaboration: \
         The current value of the slider, which is between 0 and 100. \
         #\"))))(Secondary((id \
         2fa27185-d801-41df-b2e0-388839c17769)(content(Whitespace\"\\n\"))))(Secondary((id \
         10256c94-dc8f-4f60-a992-16aa01a2da54)(content(Comment\"# Internal \
         Model: Int                                                       \
         #\"))))(Secondary((id \
         67cad6f3-3182-4c51-936a-5713290b6715)(content(Whitespace\"\\n\"))))(Tile((id \
         bced7663-c364-461f-92be-556807c30e67)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         40))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         e356c518-34ab-417c-a3d0-e0d9029c0c9f)(content(Whitespace\" \
         \"))))(Tile((id \
         790b89d0-8245-4c1a-8bc0-33af849b5e52)(label(slider_val))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         1a5f3b6e-5830-4978-835d-278dd3a986ed)(content(Whitespace\" \
         \")))))((Secondary((id \
         590149d2-0ae2-433b-b659-7d73a79e4fb7)(content(Whitespace\" \
         \"))))(Projector((id f655fccf-8ab3-49bb-a073-c6637696129f)(kind \
         Livelit)(syntax(Tile((id \
         91ad023b-df20-4a59-8a6d-4b63826c6e44)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         2f301349-f2c1-4eb9-bd82-3c5d7b025962)(label(^slider))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         b9416645-a97a-4f76-a174-5163e3b73cb4)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         33b8cb2c-ac91-486f-83aa-b0651f004bd8)(label(85))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))))))))(model\"()\")))(Secondary((id \
         2ba5684e-fcb5-4c5e-b949-a15324e0e541)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         7df76a49-c053-48ef-ae8a-a5b176b17913)(content(Whitespace\"\\n\"))))(Secondary((id \
         78c92707-d5dc-4c02-b5c0-dd3a9d861e79)(content(Whitespace\"\\n\"))))(Secondary((id \
         e4333e9a-4cc7-4566-8938-631599724082)(content(Comment\"# 2. ^emotion \
         #\"))))(Secondary((id \
         5514a38a-f7b1-455b-ac86-5626b8c0b662)(content(Whitespace\"\\n\"))))(Secondary((id \
         70d7a21c-a3f1-4525-9b99-f0d78cc73725)(content(Comment\"# Elaboration \
         Type: String                                      \
         #\"))))(Secondary((id \
         3f052a1a-28f6-4bfe-9c1d-1091847ebe2c)(content(Whitespace\"\\n\"))))(Secondary((id \
         a53a11aa-78ca-46a1-be75-5debec050929)(content(Comment\"# Elaboration: \
         Returns a String representing an emotion,        \
         #\"))))(Secondary((id \
         7e5f09f6-f999-42e8-943c-2af2225c4f79)(content(Whitespace\"\\n\"))))(Secondary((id \
         788f357c-5929-4109-b20c-dd230cd00483)(content(Comment\"#              \
         determined as follows based on the slider value: \
         #\"))))(Secondary((id \
         d361b015-da8c-4767-853f-66506f97a540)(content(Whitespace\"\\n\"))))(Secondary((id \
         1257c15c-2045-45da-958d-75d348ff492c)(content(Comment\"#                \
         if value < 40 then \\\"sad\\\"                       \
         #\"))))(Secondary((id \
         138349af-2cc0-4f91-be70-56623d0c3083)(content(Whitespace\"\\n\"))))(Secondary((id \
         6ba0c5b3-28a7-4537-a2d8-b8acd36375ed)(content(Comment\"#                \
         if value > 70 then \\\"happy\\\"                     \
         #\"))))(Secondary((id \
         91c808fd-915f-485d-aed0-b556da9b9ae8)(content(Whitespace\"\\n\"))))(Secondary((id \
         27451202-ca9d-4f19-853c-143299eb26a8)(content(Comment\"#                \
         otherwise \\\"neutral\\\"                            \
         #\"))))(Secondary((id \
         f5a4c1c7-bb91-4bcf-b259-e6cf988dbd1b)(content(Whitespace\"\\n\"))))(Secondary((id \
         6655ede7-ee63-4805-86da-fabdee5d28bf)(content(Comment\"# Internal \
         Model: Int                                           \
         #\"))))(Secondary((id \
         527857a1-0e1f-43ea-98bb-f3029a418350)(content(Whitespace\"\\n\"))))(Tile((id \
         80ef36f0-8ee8-4731-ba68-6c908f95f274)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         40))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         a6b2ff5f-61a5-4bf4-a521-33daae25f285)(content(Whitespace\" \
         \"))))(Tile((id \
         6910ee05-ecc5-4dfb-9b1f-b3a11b75e515)(label(current_mood))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         df7f8e3b-6297-4cbf-92c8-cfe33ada4290)(content(Whitespace\" \
         \")))))((Secondary((id \
         62ece9e0-5091-4f6e-a745-dc375df3b5b2)(content(Whitespace\" \
         \"))))(Projector((id caf8fa3b-bda6-404c-a447-e8e05746b5ed)(kind \
         Livelit)(syntax(Tile((id \
         186b81bc-b170-4c3f-89e3-e1f782cedae3)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         379a5546-5bc4-4d32-a39b-1eb62dda9a5b)(label(^emotion))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         aaff3195-c5e2-49ae-bb52-32ffa9c46dfc)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         e4c64ce7-5885-43f0-8a4c-2c8bc4003982)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))))))))(model\"()\")))(Secondary((id \
         38361cff-c89f-4d36-a143-d4c9d88d4ae4)(content(Whitespace\" \
         \")))))))))(Grout((id 6ffd5f7f-c1bc-4636-b92f-1527d7ff5e24)(shape \
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
