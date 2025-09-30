let out : string * Haz3lcore.PersistentSegment.t =
  ( "Livelits",
    {
      refractors = Haz3lcore.Zipper.Refractor.Map.empty;
      segment =
        "((Secondary((id \
         66b121c3-cd30-462a-8927-e0ef55b5b5a2)(content(Comment\"# LIVELITS \
         #\"))))(Secondary((id \
         4748dd5b-63ef-47d1-aee4-4e6c2ef75c5c)(content(Whitespace\"\\n\"))))(Secondary((id \
         6a609d2f-edc3-4e8d-a450-e2de01443e20)(content(Comment\"# A livelit is \
         a live GUI widget which can be inserted into expressions. \
         #\"))))(Secondary((id \
         1aa3f4fd-88b0-46a6-8afa-7003e1ee21cf)(content(Whitespace\"\\n\"))))(Secondary((id \
         1f25590c-ae13-4e33-8009-ca44e924c34a)(content(Comment\"# It \
         elaborates to a value of some given type. #\"))))(Secondary((id \
         493738d0-2ef7-4b62-b206-909d5cb85810)(content(Whitespace\"\\n\"))))(Secondary((id \
         1350d611-f2c8-4340-a3e6-f6ca87ad309a)(content(Whitespace\"\\n\"))))(Secondary((id \
         fb966ae2-926f-409a-a6d8-53acb1feee4c)(content(Comment\"# Invocation: \
         #\"))))(Secondary((id \
         f871135b-e6b5-471b-a481-6309b38e4d01)(content(Whitespace\"\\n\"))))(Secondary((id \
         18d48f4a-1ed2-4935-b075-469d54c32414)(content(Comment\"# To invoke a \
         livelit, insert the name of the livelit (always prefixed with ^) then \
         space. #\"))))(Secondary((id \
         93885aea-e637-4376-9282-a5fc076fc32f)(content(Whitespace\"\\n\"))))(Secondary((id \
         3af673d8-42fb-474b-b0ac-69d3f468224c)(content(Comment\"# The context \
         inspector shows which livelits are available and their elaboration \
         type. #\"))))(Secondary((id \
         d4e7d01d-dbff-4562-9bac-d197272eaa5b)(content(Whitespace\"\\n\"))))(Secondary((id \
         ab7d355b-5b1d-4c4f-8a53-e39f04155acc)(content(Whitespace\"\\n\"))))(Secondary((id \
         48a5ed5d-9115-43c5-887d-1b0b04391056)(content(Comment\"# Each livelit \
         maintains an internal model, which is not generally something the \
         #\"))))(Secondary((id \
         1598c17f-9203-46c3-9ef7-da19ff424738)(content(Whitespace\"\\n\"))))(Secondary((id \
         c3fcfd58-bb4c-470f-b62e-ce32b3a4780a)(content(Comment\"# programmer \
         should need to look at. You can see the internal model by toggling \
         #\"))))(Secondary((id \
         5c519724-b364-4850-94f4-83b1f0ba47c6)(content(Whitespace\"\\n\"))))(Secondary((id \
         ccc09633-abfb-41a9-9ccc-5ef1fff9160d)(content(Comment\"# the livelit \
         projector off (bottom right corner of Hazel). #\"))))(Secondary((id \
         87c9b93f-8854-406b-95b2-687d6034897a)(content(Whitespace\"\\n\"))))(Secondary((id \
         6442c176-2075-4c2e-91e4-4d3729b38c89)(content(Whitespace\"\\n\"))))(Secondary((id \
         ee815156-94f3-4656-b5d2-e1ef878cce3a)(content(Comment\"# Built-in \
         Livelits: #\"))))(Secondary((id \
         51b21c76-49b1-43c0-b237-c2cbe8d300b3)(content(Whitespace\"\\n\"))))(Secondary((id \
         4f774e10-c1b3-4105-a8b4-089f55c86423)(content(Comment\"# Currently, \
         all livelits are built in: #\"))))(Secondary((id \
         56e58276-9938-43de-9f6d-3f85e075ce39)(content(Whitespace\"\\n\"))))(Secondary((id \
         2630aac8-46d9-4011-b701-c206ee5a0e27)(content(Whitespace\"\\n\"))))(Secondary((id \
         ff0d030e-3f1b-4b2e-8aaf-1c673cffdee5)(content(Comment\"# 1. ^slider \
         #\"))))(Secondary((id \
         54eed6ef-9640-4b57-9c92-9a926a93efdd)(content(Whitespace\"\\n\"))))(Secondary((id \
         5ee8c7dd-891e-477e-b794-18862b87220d)(content(Comment\"# Elaboration \
         Type: Int                                                     \
         #\"))))(Secondary((id \
         c1efb591-1355-4fcb-9ba4-2e4419bb4846)(content(Whitespace\"\\n\"))))(Secondary((id \
         c1211eda-fef8-45af-b8d5-9949fa6b528d)(content(Comment\"# Elaboration: \
         The current value of the slider, which is between 0 and 100. \
         #\"))))(Secondary((id \
         08ddc75a-257e-40c1-b8d7-ada79a701642)(content(Whitespace\"\\n\"))))(Secondary((id \
         7ce63081-12fa-4eae-8d2d-8863ff3af4e7)(content(Comment\"# Internal \
         Model: Int                                                       \
         #\"))))(Secondary((id \
         8ebb45c7-6e66-4d9e-8d69-81e0f736a12d)(content(Whitespace\"\\n\"))))(Tile((id \
         69c8eeef-46e0-434f-b1c9-51318758eb21)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         40))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         07cd6eb3-d83c-4a6d-a627-5b1ac1269868)(content(Whitespace\" \
         \"))))(Tile((id \
         0b3a5329-a629-47e3-9e70-cce48126d7df)(label(slider_val))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         d99c9b52-2a5b-4f9e-a394-5bd90f5cd2de)(content(Whitespace\" \
         \")))))((Secondary((id \
         db2c21d8-4d68-478b-911d-570fa1627650)(content(Whitespace\" \
         \"))))(Projector((id 71143fb3-242b-4182-8dbe-c17deb74c021)(kind \
         Livelit)(syntax(Tile((id \
         3e977b08-9847-4900-96b9-b4812ac40af2)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         75c72a24-1a1e-45ea-a86a-e3eb42855e3d)(label(^slider))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         fe7fc635-39f8-4481-b5f3-d0072a7800e7)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         7f81ba34-c5ca-4f1d-a685-95a47c782200)(label(85))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))))))))(model\"()\")))(Secondary((id \
         b7adad71-c1a6-4369-839b-6cc716859fd6)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         f5be5664-dcdb-4ca3-b026-84070d9ede2e)(content(Whitespace\"\\n\"))))(Secondary((id \
         e8a5b69f-060c-4516-b3bb-71fef8b6b0ef)(content(Whitespace\"\\n\"))))(Secondary((id \
         81503651-b0fb-4bdf-adcc-7e2c2bb6a89b)(content(Comment\"# 2. ^emotion \
         #\"))))(Secondary((id \
         052e2fca-57cd-40d1-acd7-af3a83b17200)(content(Whitespace\"\\n\"))))(Secondary((id \
         6809ac49-f459-488d-86db-2d82b0feeaa7)(content(Comment\"# Elaboration \
         Type: String                                      \
         #\"))))(Secondary((id \
         6d2a99b0-6a0a-4140-ba33-a34512f4e216)(content(Whitespace\"\\n\"))))(Secondary((id \
         34e156c8-513a-400e-8554-8ac1fcc5ece3)(content(Comment\"# Elaboration: \
         Returns a String representing an emotion,        \
         #\"))))(Secondary((id \
         46fcf75b-6e5f-441c-87fa-762694e04965)(content(Whitespace\"\\n\"))))(Secondary((id \
         b127eb40-9c52-450e-ba54-011d9c98b211)(content(Comment\"#              \
         determined as follows based on the slider value: \
         #\"))))(Secondary((id \
         70510191-bbf6-432a-ba31-d70b8ca60799)(content(Whitespace\"\\n\"))))(Secondary((id \
         7a1e44e0-f216-4dc2-b7b2-11e3906be474)(content(Comment\"#                \
         if value < 40 then \\\"sad\\\"                       \
         #\"))))(Secondary((id \
         90ec4f8e-be0e-4f06-974d-03ba19333337)(content(Whitespace\"\\n\"))))(Secondary((id \
         1ce4578a-c66e-4e12-9723-fa048d20fa20)(content(Comment\"#                \
         if value > 70 then \\\"happy\\\"                     \
         #\"))))(Secondary((id \
         5486f4b1-477a-4dab-a025-2db31ad83749)(content(Whitespace\"\\n\"))))(Secondary((id \
         923af167-49f1-4297-a3d6-28f48c14ab37)(content(Comment\"#                \
         otherwise \\\"neutral\\\"                            \
         #\"))))(Secondary((id \
         47960e47-57ce-4528-b034-a99479b99c96)(content(Whitespace\"\\n\"))))(Secondary((id \
         92427d3b-c08b-4530-b231-f22cfbbf0148)(content(Comment\"# Internal \
         Model: Int                                           \
         #\"))))(Secondary((id \
         6a33cf39-c667-44cf-a4ad-801c161283ba)(content(Whitespace\"\\n\"))))(Tile((id \
         9b54fd6a-7eed-4e6e-8444-193663808bff)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         40))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         2bda4674-cc71-4dd5-82ab-0cff778a183f)(content(Whitespace\" \
         \"))))(Tile((id \
         ecdc9b6c-07da-45cd-9437-daf95f84f7dc)(label(current_mood))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         00a9ab39-2bce-43fd-9327-fe7ddf6a9f49)(content(Whitespace\" \
         \")))))((Secondary((id \
         39bf857d-ef27-4ed4-b1df-2f7984b7346c)(content(Whitespace\" \
         \"))))(Projector((id a73f8254-0425-4968-80c1-71abf84fbc1c)(kind \
         Livelit)(syntax(Tile((id \
         fd7c3d96-2016-4143-ab74-536edfaba8fc)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         572809ed-59cb-4e1d-9824-a50d257cd1e7)(label(^emotion))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         e3c391a3-2c6c-4cd4-ae1b-dc741c9950fd)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         7e7a62e9-51a3-4bbb-9555-54e8b2f2c869)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))))))))(model\"()\")))(Secondary((id \
         44ddd530-82a8-4184-800a-21652348cfe7)(content(Whitespace\" \
         \")))))))))(Grout((id 3534f38b-9cab-4478-9cde-bd0d16f2fae0)(shape \
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
    } )
