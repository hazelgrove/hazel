let out : string * Haz3lcore.PersistentZipper.t =
  ( "Probes",
    {
      zipper =
        "((selection((focus Left)(content())(mode \
         Normal)))(relatives((siblings(((Secondary((id \
         b93b0fbe-48c3-4c44-b9e2-61e5316204b3)(content(Comment\"#  \
         _____           _                #\"))))(Secondary((id \
         61ef0b62-9568-4653-bf7c-eb706b449b5a)(content(Whitespace\"\\n\"))))(Secondary((id \
         ffc95f16-ddd8-4fdf-96a5-ff0083562404)(content(Comment\"# |  __ \
         \\\\         | |               #\"))))(Secondary((id \
         3806978b-4aef-46f6-99ae-3c752bfdd885)(content(Whitespace\"\\n\"))))(Secondary((id \
         678d1ffd-ce6c-44a9-84c9-1c8745f9fcb3)(content(Comment\"# | |__) | __ \
         ___ | |__   ___  ___  #\"))))(Secondary((id \
         6b6ad2c4-9ab9-4bb3-b4a2-41b46fb8f443)(content(Whitespace\"\\n\"))))(Secondary((id \
         387be71a-16af-4b31-8189-131939a90ac4)(content(Comment\"# |  ___/ '__/ \
         _ \\\\| '_ \\\\ / _ \\\\/ __| #\"))))(Secondary((id \
         e2c38b7c-7100-4b5e-b96e-92791db56a7b)(content(Whitespace\"\\n\"))))(Secondary((id \
         6079d97b-1f46-43a9-a11f-1c4a1a4eaae5)(content(Comment\"# | |   | | | \
         (_) | |_) |  __/\\\\__ \\\\ #\"))))(Secondary((id \
         02d5c752-afb5-491e-b641-072cce22a9a4)(content(Whitespace\"\\n\"))))(Secondary((id \
         54cf461d-bef1-4fc6-98d3-c9066a3bbaad)(content(Comment\"# |_|   |_|  \
         \\\\___/|_.__/ \\\\___||___/ #\"))))(Secondary((id \
         ad624d1b-73ab-4466-b746-4f5f81ee3a61)(content(Whitespace\"\\n\"))))(Secondary((id \
         4d6dc538-df24-4da8-b773-ed6f436df679)(content(Comment\"# INLINE EVAL \
         WITH PROBE PROJECTORS #\"))))(Secondary((id \
         a3bf63b9-6782-4862-a54d-d52cbf370ea6)(content(Whitespace\"\\n\")))))((Secondary((id \
         3c064f1d-1bce-4980-aad5-bc3938d29563)(content(Whitespace\"\\n\"))))(Secondary((id \
         a8c1a72c-8ef5-42c5-99e7-a89e894ee4f3)(content(Comment\"# INTRODUCTION \
         #\"))))(Secondary((id \
         7ff0b049-215e-4176-ae71-bbbe9b1a6b3a)(content(Whitespace\"\\n\"))))(Secondary((id \
         df19d628-2f63-4637-a12f-39281cb75541)(content(Whitespace\"\\n\"))))(Secondary((id \
         332e022d-afde-43a7-9d0a-3198d714417f)(content(Comment\"# Probe \
         projectors are a kind of inline evaluation, #\"))))(Secondary((id \
         27096517-8a08-49c6-851d-bd7a0ce335b2)(content(Whitespace\"\\n\"))))(Secondary((id \
         4e0c1f50-acd0-4a82-b683-3cfa84f4058e)(content(Comment\"# similar to \
         value hints in Emacs or IntelliJ. #\"))))(Secondary((id \
         38eb7626-f756-41ea-8048-21546c95d742)(content(Whitespace\"\\n\"))))(Secondary((id \
         1eaf1339-616b-4e9e-be1b-ee849c91d734)(content(Whitespace\"\\n\"))))(Secondary((id \
         1d375d58-5dce-4921-8216-dc77505fab0f)(content(Comment\"# You can put \
         these on any expression or variable binding to #\"))))(Secondary((id \
         9196cb7e-ad8f-4161-809d-bce84bdfe37d)(content(Whitespace\"\\n\"))))(Secondary((id \
         8add24f1-6382-4cba-a9c9-33d7f8bb438d)(content(Comment\"# see a list \
         of all values taken on by that expression/pattern. \
         #\"))))(Secondary((id \
         87add81c-5c4e-453e-b6e7-c00b619509bd)(content(Whitespace\"\\n\"))))(Secondary((id \
         58f5aa76-8085-4cda-aa80-88908ffe8e1b)(content(Comment\"# By default \
         values are sorted by left-to-right by most-recent. \
         #\"))))(Secondary((id \
         9fac3967-cdac-40ad-b69b-c6bd349ffc80)(content(Whitespace\"\\n\"))))(Secondary((id \
         9dc9f0a4-474c-4fca-98ec-645867e58322)(content(Whitespace\"\\n\"))))(Secondary((id \
         df21fefe-b41a-4e35-b206-46723373fbb9)(content(Comment\"# More \
         generally, each cell represents a stack state, #\"))))(Secondary((id \
         714457e3-ec10-4ef7-8c22-6052190c8608)(content(Whitespace\"\\n\"))))(Secondary((id \
         ce0c7084-5194-4e39-ac56-87ee80820c50)(content(Comment\"# including \
         the top stack frame / closure and hence the #\"))))(Secondary((id \
         1446a5c5-8004-4015-ad37-bdca0f481945)(content(Whitespace\"\\n\"))))(Secondary((id \
         f423d1dd-8edf-42c4-bbb4-6ded0bfdbea1)(content(Comment\"# expression's \
         value, the values of environment variables, #\"))))(Secondary((id \
         4d5f578e-a4f9-4111-8640-70ee8a05d1cd)(content(Whitespace\"\\n\"))))(Secondary((id \
         07e701a1-3c36-469b-87e4-b1f71efe5de3)(content(Comment\"# as well as \
         the surrounding call stack context. #\"))))(Secondary((id \
         b479d5ba-f593-47f0-ad9c-5a7ec380e55d)(content(Whitespace\"\\n\"))))(Secondary((id \
         08c34128-af19-4856-bbd6-871103a49d20)(content(Whitespace\"\\n\"))))(Secondary((id \
         96dbf841-dd3d-49d2-b2cd-3bad24c7e801)(content(Comment\"# When a cell \
         is selected, you can hover over it to see #\"))))(Secondary((id \
         9045eb50-3532-4dda-9919-967812ece12e)(content(Whitespace\"\\n\"))))(Secondary((id \
         7c751ab9-c66a-4ed0-9e9c-913e25e8dec3)(content(Comment\"# relevant \
         environment variables, and all /other/ cells #\"))))(Secondary((id \
         cca7b2db-971e-4cdc-ae94-1f5b4b270d13)(content(Whitespace\"\\n\"))))(Secondary((id \
         d8063084-2e49-4315-89c7-53507ebacbdc)(content(Comment\"# are \
         decorated according to their relative position in \
         #\"))))(Secondary((id \
         9ae56306-fc63-44fb-a20c-a4c70afe2705)(content(Whitespace\"\\n\"))))(Secondary((id \
         4420876a-25ea-4874-9184-62e728261bf7)(content(Comment\"# to the \
         selected cell. in the context #\"))))(Secondary((id \
         86edfdc1-0bb6-4726-b069-ef297474e9cc)(content(Whitespace\"\\n\"))))(Secondary((id \
         830fe5f6-314e-45a4-b5ba-d57a0d690f70)(content(Whitespace\"\\n\"))))(Secondary((id \
         2b59b919-4485-4eb5-8cf4-c1b889ca60da)(content(Comment\"# Probe are \
         intended mostly as a println replacement #\"))))(Secondary((id \
         bff2a625-adee-430a-a66c-ca2ded4916b3)(content(Whitespace\"\\n\"))))(Secondary((id \
         3f35ee85-e7d9-428c-95c9-eab18fdd4b64)(content(Comment\"# for exposing \
         intermediate values, with the above decorations #\"))))(Secondary((id \
         c3d074cc-a2d9-4ab6-883b-c719adced190)(content(Whitespace\"\\n\"))))(Secondary((id \
         7c902508-f394-4d2d-9f99-29b58e161de7)(content(Comment\"# as a \
         supporting feature to help maintain context when \
         #\"))))(Secondary((id \
         75369ac2-984a-4f35-bd75-0a2c07694b7e)(content(Whitespace\"\\n\"))))(Secondary((id \
         eb4f85ba-48bb-46a9-ae1c-84f9178159a5)(content(Comment\"# navigating \
         between multiple probed expressions, which #\"))))(Secondary((id \
         287dd5b7-de60-44cf-b713-d37105e10797)(content(Whitespace\"\\n\"))))(Secondary((id \
         b7ad3abe-65b5-4372-83e4-8d677bf84a70)(content(Comment\"# may take on \
         many values across nested or recursive functions. \
         #\"))))(Secondary((id \
         fba33a9d-ad30-45e5-bb3f-793c21681dea)(content(Whitespace\"\\n\"))))(Secondary((id \
         a84c6573-c814-46a9-b4d0-03059bdcaf6e)(content(Whitespace\"\\n\"))))(Secondary((id \
         61529aba-6611-4f9e-ae78-56a269bd2059)(content(Whitespace\"\\n\"))))(Secondary((id \
         577cee6a-0dd7-495d-8b61-540f142c7464)(content(Comment\"# TUTORIAL \
         #\"))))(Secondary((id \
         50207191-3a50-4de5-8186-197ebe1b194d)(content(Whitespace\"\\n\"))))(Secondary((id \
         3829dc22-9c7c-4125-8cd9-cb7b0fa1af5a)(content(Whitespace\"\\n\"))))(Secondary((id \
         017d0a58-fbfd-4685-ba5f-b6fe3c986730)(content(Comment\"# The \
         expression (10 * 10) below has a probe.  #\"))))(Secondary((id \
         91bbf605-8627-4521-8bd8-5775169ef465)(content(Whitespace\"\\n\"))))(Secondary((id \
         bdb51bb1-22c9-4f8f-a0a4-1ce16dfb950a)(content(Comment\"# Its value, \
         20, is shown in a cell to the right. #\"))))(Secondary((id \
         dd16003b-74d8-46a0-94d2-b9111d98d971)(content(Whitespace\"\\n\"))))(Tile((id \
         1320ccad-ea05-4c00-a72b-07e9fd03436e)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         40))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         821ba55c-af5c-4570-883b-9f25c78aaeb8)(content(Whitespace\" \
         \"))))(Tile((id \
         7e1ad1f0-fd3b-48c6-9431-171fdb06d688)(label(chips))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         d4025398-4fba-4312-aff1-ff85f09f59ae)(content(Whitespace\" \
         \")))))((Secondary((id \
         2a5872f3-d965-41f2-ace0-92b44a17e8e3)(content(Whitespace\" \
         \"))))(Projector((id d73c92cc-fb70-4ce6-b2a0-b5a84fe5feea)(kind \
         Probe)(syntax(Tile((id \
         94294c8f-e75f-4833-9880-cc3878b6318d)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         8d1bfef2-b25c-4892-aca8-3adece604b70)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         ae680881-0f86-4bef-85bd-a7a1af3bf7dc)(label(10))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         241ec43e-cff9-4ce9-8fe5-4e66d863ba69)(content(Whitespace\" \
         \"))))(Tile((id \
         b67a4429-64dd-492a-9e82-9b326c83e419)(label(+))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 27))(sort Exp))((shape(Concave \
         27))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         b1dca279-2e5f-49f3-b606-3d40b96e911c)(content(Whitespace\" \
         \"))))(Tile((id \
         dde0f302-8b03-42e7-917b-ce86afb6e028)(label(10))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))))))))(model\"((display_lengths())(max_closures \
         30)(index_offset 0))\")))(Secondary((id \
         cb117cb1-de0c-46f0-8a6c-63580ce29ff5)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         3edb4efc-cb0b-4cdb-9c25-4e8147138676)(content(Whitespace\"\\n\"))))(Secondary((id \
         0b09a401-ba77-4d0f-a786-d34cc37ef3b4)(content(Whitespace\"\\n\"))))(Secondary((id \
         6e6a4a81-4fc1-4d3e-afed-7c76e4910e6a)(content(Comment\"# To probe the \
         below expression, put your caret to #\"))))(Secondary((id \
         3af90004-6247-4d24-9e35-53db291f9b66)(content(Whitespace\"\\n\"))))(Secondary((id \
         eebc4d71-bddc-4c55-ad91-dca277f9b7e7)(content(Comment\"# left of the \
         `(` and press option/alt-v (for value), #\"))))(Secondary((id \
         5d732f46-26e8-47cf-af38-0340877d7f1f)(content(Whitespace\"\\n\"))))(Secondary((id \
         0ab33b9e-2889-4d5a-b1bb-303b6fedaa4b)(content(Comment\"# or select \
         `Probe` from the lower right corner menu: #\"))))(Secondary((id \
         82f646a3-a304-47fe-8d47-84386d2309a3)(content(Whitespace\"\\n\"))))(Tile((id \
         756bf144-4cde-4a34-9ca3-7aa134a7c82b)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         40))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         367e778d-85e6-4163-9ceb-dd133d962687)(content(Whitespace\" \
         \"))))(Tile((id \
         9f1a6fe2-6c59-43f1-a7cc-36e2eb99b7b5)(label(mult))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         cfaaca03-a239-483c-a1bb-6c49d2abde1e)(content(Whitespace\" \
         \")))))((Secondary((id \
         3a708ea6-cd6e-4314-8369-6bea8375256b)(content(Whitespace\" \
         \"))))(Tile((id \
         87ab1d67-e0e5-406f-8c04-c94806047f44)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         a7b7722b-fa96-4c89-b124-72f44bef4dae)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         ca99e66f-95a3-4467-8b8a-33877550ab99)(content(Whitespace\" \
         \"))))(Tile((id \
         d5bd0786-eca1-411b-9a57-73d77d7872df)(label(+))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 27))(sort Exp))((shape(Concave \
         27))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         22aadd16-febe-4362-bc00-1dd8de1c4135)(content(Whitespace\" \
         \"))))(Tile((id \
         d23a94fd-0ba5-458d-bf4f-26b624506dbf)(label(2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         b94f03bc-adbe-4047-bc7b-6b28c0b0e83d)(content(Whitespace\" \
         \"))))(Tile((id \
         3cc82d77-acd0-492b-a79f-aaf2df44ea9a)(label(*))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 26))(sort Exp))((shape(Concave \
         26))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         1e9234d6-5af0-4eee-9f82-cd6c100af01e)(content(Whitespace\" \
         \"))))(Tile((id \
         448780a5-3d5c-4a0f-817e-069a380faccc)(label(3))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         6ebd2fe4-3efb-422a-be23-3e206d7642ab)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         b01cddd0-9837-4736-879a-d6142cee4870)(content(Whitespace\"\\n\"))))(Secondary((id \
         71453117-1176-456c-8a2a-fc70edcb596d)(content(Comment\"# The \
         expression should be encased in a green block #\"))))(Secondary((id \
         5165eadb-f0a7-4062-a120-6a216ff1bb18)(content(Whitespace\"\\n\"))))(Secondary((id \
         2acfb5cf-6d12-49f5-bb46-6723cb3172a4)(content(Comment\"# and a cell \
         reading `7` should appear to the right. #\"))))(Secondary((id \
         2090bc60-44ee-4cf1-9ea6-27ec4a28e043)(content(Whitespace\"\\n\"))))(Secondary((id \
         3acf0caf-1b41-4d05-bcd7-9f88d8a4df62)(content(Comment\"# The same \
         shortcut or menu toggle removes it. #\"))))(Secondary((id \
         43fb482c-88e0-48c6-a1df-66df7f846ac1)(content(Whitespace\"\\n\"))))(Secondary((id \
         76f62431-e6bf-44d9-9eb2-99beb9d11a91)(content(Whitespace\"\\n\"))))(Secondary((id \
         7fe9cd19-e3ae-407a-a1e2-944701b8107a)(content(Comment\"# Click the \
         below cell (with value 21) to select it. #\"))))(Secondary((id \
         90688b1a-b6b1-475f-bfab-98079f38b032)(content(Whitespace\"\\n\"))))(Tile((id \
         d3579b84-e1c2-4271-9d82-5042f488acb6)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         40))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         900914df-fac7-4abb-b525-40d0afd9efc2)(content(Whitespace\" \
         \"))))(Tile((id \
         2d1a9c84-0e46-48e7-adb2-caf3f996f67d)(label(score))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         7d74258d-f27f-4e21-bb8c-b032d99f8b49)(content(Whitespace\" \
         \")))))((Secondary((id \
         1d534d01-0e27-430e-bad2-bfd1d066d8bc)(content(Whitespace\" \
         \"))))(Projector((id 3e792a58-a28e-4163-a146-a72f504f1fea)(kind \
         Probe)(syntax(Tile((id \
         708d268c-081f-40ed-9f1f-18942dd4628d)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         fbc2ce55-0f22-4a1e-bff9-bbba601a2e83)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         8e78df48-4ff0-4608-8c21-2ca2e431713c)(label(chips))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         8c804a16-e452-4b35-a390-9d94c89f8794)(content(Whitespace\" \
         \"))))(Tile((id \
         25f46221-3572-41e2-9f84-4eb86ddae57d)(label(*))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 26))(sort Exp))((shape(Concave \
         26))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         62755f1d-38ec-461b-a385-9821e530ae9a)(content(Whitespace\" \
         \"))))(Tile((id \
         a472cc8c-c979-4b6a-8948-fccf51a5ba02)(label(mult))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))))))))(model\"((display_lengths())(max_closures \
         1)(index_offset 0))\")))(Secondary((id \
         d93630c9-2d43-427e-afdb-21d8861ea1ea)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         a7ede28a-e46d-424e-97f9-8d6d5a6526aa)(content(Whitespace\"\\n\"))))(Secondary((id \
         6c3b96e0-d881-4c15-b8a5-e267d01b88e9)(content(Comment\"# Notice when \
         you hover over a selected cell, it #\"))))(Secondary((id \
         fcd16a75-9c54-4f02-992a-6f8e6b61bc8c)(content(Whitespace\"\\n\"))))(Secondary((id \
         127fef9f-c069-4bbf-adfb-a12d98ac257e)(content(Comment\"# shows the \
         values of any contained variables. #\"))))(Secondary((id \
         da8cc110-978c-4f06-8a66-5df117fa14c1)(content(Whitespace\"\\n\"))))(Secondary((id \
         7f5fef8a-f43e-4d4f-b1ef-a3004249547d)(content(Whitespace\"\\n\"))))(Secondary((id \
         25219e86-6b27-4555-848d-cb0f8058b397)(content(Comment\"# Probes only \
         have cells if the are evaluated. #\"))))(Secondary((id \
         8be6cf2e-ec55-417d-a668-eca29a4875e1)(content(Whitespace\"\\n\"))))(Secondary((id \
         2d66a897-47f7-4c64-8b64-f31b26dc5250)(content(Comment\"# Below, only \
         the first case branch is evaluated. #\"))))(Secondary((id \
         401a5ad5-1093-4d2d-82fb-3655074cf703)(content(Whitespace\"\\n\"))))(Tile((id \
         fc6f91b8-8a82-4761-b334-533aec2d48fe)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         40))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         6462acbd-e9b5-441a-a64a-fd0b752f5805)(content(Whitespace\" \
         \"))))(Tile((id \
         25139203-2a60-44ce-b8dc-9c6049cb3dfd)(label(check))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         c9d3e405-d2c7-4550-aeb8-cb5fa1694e47)(content(Whitespace\" \
         \")))))((Secondary((id \
         a00c0c8e-9e27-442d-9bac-ad9c2feac4d8)(content(Whitespace\" \
         \"))))(Tile((id 813caafd-30cb-4bba-97c1-dfb2272607a7)(label(case \
         end))(mold((out Exp)(in_(Rul))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         29279b05-60c7-4686-b0b3-fad8473a06bd)(content(Whitespace\" \
         \"))))(Projector((id 1685f762-82d5-4da2-b3fc-2d08db39fdc8)(kind \
         Checkbox)(syntax(Tile((id \
         da1e6ba2-56e9-4c62-af05-5a6c64ab0c0b)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         37c4b638-eccd-40b3-92a4-15148a85a5c1)(label(true))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))))))))(model\"()\")))(Secondary((id \
         0b3d5b2d-68a3-46ed-a1c7-7ee9397652c4)(content(Whitespace\"\\n\"))))(Tile((id \
         fcf93a68-68bb-4c5f-a7d5-da50976fc083)(label(| =>))(mold((out \
         Rul)(in_(Pat))(nibs(((shape(Concave 41))(sort Exp))((shape(Concave \
         41))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         be1c3205-7a3a-4336-922b-e85bef66e011)(content(Whitespace\" \
         \"))))(Tile((id \
         577818ad-7d9b-4d72-8c10-3ea4c92ff3c9)(label(false))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         8208a8a7-4f8b-4807-aa95-cd571dcb326d)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         35c57e3b-5d85-4550-8479-44319772febe)(content(Whitespace\" \
         \"))))(Projector((id b7be157f-3b59-45ce-aaf9-2fb957019718)(kind \
         Probe)(syntax(Tile((id \
         6bcb4ef8-ec5e-4f19-bcd5-a47543e5ad12)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         cf6ce2b7-c965-4a25-9273-676a9855fd5f)(label(\"\\\"checks \
         out\\\"\"))(mold((out Exp)(in_())(nibs(((shape Convex)(sort \
         Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))))))))(model\"((display_lengths())(max_closures \
         30)(index_offset 0))\")))(Secondary((id \
         66af5e8c-4f2d-4acf-9804-d7f6102d7d9c)(content(Whitespace\"\\n\"))))(Tile((id \
         77c09898-be45-49c6-bb51-2cc45dd5ae46)(label(| =>))(mold((out \
         Rul)(in_(Pat))(nibs(((shape(Concave 41))(sort Exp))((shape(Concave \
         41))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         2fb263e3-47ef-419d-b2be-a7907f3e30fd)(content(Whitespace\" \
         \"))))(Tile((id \
         c46b8825-323e-464d-8ee3-0d28b1aba270)(label(true))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         7b8eeb53-8866-4e39-a733-f8715ecf79e2)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         4d3590da-d568-470d-9927-d2fd84516f0f)(content(Whitespace\" \
         \"))))(Projector((id 724bcf7a-da9b-4b78-9a92-f3f52054e6ec)(kind \
         Probe)(syntax(Tile((id \
         3f7e7b64-7e74-40a1-81c5-fc9b94b64a1b)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         ba24da5d-9aa3-4aac-bdb4-f5d9ed2a7f5e)(label(\"\\\"you \
         cheated\\\"\"))(mold((out Exp)(in_())(nibs(((shape Convex)(sort \
         Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))))))))(model\"((display_lengths())(max_closures \
         30)(index_offset 0))\")))(Secondary((id \
         3e4cdcc4-bee6-4b9d-a02b-b648fd9579e5)(content(Whitespace\" \
         \"))))(Secondary((id \
         2028c5b0-4dab-4910-ae31-7130ffcb7cf9)(content(Whitespace\" \
         \"))))(Secondary((id \
         fbb4c4d7-fa7f-4541-aaaa-ae967d7cf07a)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         f5a5a38c-ee10-4bb5-b92d-a6877b20c1ed)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         d16c4e94-5300-4cc6-a205-d93e10ec72f9)(content(Whitespace\"\\n\"))))(Secondary((id \
         901a9f91-2bc9-477c-959f-d99f2c05c088)(content(Comment\"# Note the 2nd \
         branch probe has a zero to the right. #\"))))(Secondary((id \
         fc1f9036-71c8-467e-877f-4ef55cd017ee)(content(Whitespace\"\\n\"))))(Secondary((id \
         c53b4ce3-2af4-4fe4-a5c8-47d46354800d)(content(Comment\"# This is the \
         cell's collected closure count, i.e. #\"))))(Secondary((id \
         cf2896e5-1e2c-4e89-8235-f23cacab3d10)(content(Whitespace\"\\n\"))))(Secondary((id \
         efe380cc-3e63-4e01-9d79-e652d99f7e5b)(content(Comment\"# the number \
         of times the expression was evaluated #\"))))(Secondary((id \
         e2d549d8-27a9-4389-a8c4-83cf86501f97)(content(Whitespace\"\\n\"))))(Secondary((id \
         bad2cdd0-0ab5-4a9a-8cf0-f829f8f51c87)(content(Whitespace\"\\n\"))))(Secondary((id \
         574e41cb-bab4-4485-a522-4a5a1fd94a49)(content(Comment\"# Probes can \
         be placed on expressions: #\"))))(Secondary((id \
         16fe7e69-9e40-4c61-94cc-df3029946b08)(content(Whitespace\"\\n\"))))(Tile((id \
         9656698c-b447-4f5e-acd0-958e96f375c2)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         40))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         60eb4ecc-b782-4fc4-83e2-a5f20f8bef7f)(content(Whitespace\" \
         \"))))(Tile((id \
         4a627cad-c879-42b4-bb44-129d9cb571c2)(label(pow))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         e90a53d5-ef3f-464f-90ce-c17fd74ccb03)(content(Whitespace\" \
         \")))))((Secondary((id \
         d9c7e84f-cb45-4639-a3d5-947309371ae7)(content(Whitespace\" \
         \"))))(Projector((id c9bf7cdf-3516-481d-bd48-e7678353b9eb)(kind \
         Probe)(syntax(Tile((id \
         2ccf37fd-b02d-4eca-ac1b-900d70866149)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         fca955e9-cd0c-420f-91f1-d88efa7ade9a)(label(50))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         39a4bc48-d353-4a9d-9233-bed3b5ca5185)(content(Whitespace\" \
         \"))))(Tile((id \
         c529431a-849f-402d-8d82-af688ef592b9)(label(**))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 25))(sort Exp))((shape(Concave \
         25))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         f5471601-d385-4026-9bd8-b407801c309b)(content(Whitespace\" \
         \"))))(Tile((id \
         c6817053-0998-4395-9d48-90dda97688dd)(label(2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))))))))(model\"((display_lengths())(max_closures \
         30)(index_offset 0))\")))(Secondary((id \
         a92ca710-507d-49f3-906c-df8639c5231c)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         224986fd-d11f-452d-b561-d1693d2c109e)(content(Whitespace\"\\n\"))))(Secondary((id \
         0f56c009-8693-451b-85aa-cd9c2195abf7)(content(Comment\"# And also on \
         patterns (e.g. variables), shown in blue: #\"))))(Secondary((id \
         d75825b1-bc4f-4f68-9d57-bc2c63bfe71a)(content(Whitespace\"\\n\"))))(Tile((id \
         64de1d86-03d3-40a2-af32-f6129f1cf0ed)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         40))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         8fa680af-925e-4eca-b51b-9d0920724d70)(content(Whitespace\" \
         \"))))(Projector((id 366b586e-a375-441f-80f6-8eb0ba8aeca9)(kind \
         Probe)(syntax(Tile((id \
         24ca8ae8-489e-4608-b948-73874b06742f)(label(\"(\"\")\"))(mold((out \
         Pat)(in_(Pat))(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0 1))(children(((Tile((id \
         d4a54759-b2b6-4a53-8c63-46a466854901)(label(pow))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))))))))(model\"((display_lengths())(max_closures \
         30)(index_offset 0))\")))(Secondary((id \
         98c257d6-2749-470e-b92b-9ba4526d74c1)(content(Whitespace\" \
         \")))))((Secondary((id \
         36d8c225-ae1b-4765-93df-133c8ccf028c)(content(Whitespace\" \
         \"))))(Projector((id ead68cbe-0b18-45cc-8556-17adbe427e64)(kind \
         Slider)(syntax(Tile((id \
         091f67a3-ae54-4fad-bc62-f7e18feb2841)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         ed2ed929-d60e-469a-8ae4-389781050064)(label(54))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))))))))(model\"()\")))(Secondary((id \
         9f2e8d45-685c-45e8-b9a8-4ce45d7121e6)(content(Whitespace\" \
         \"))))(Tile((id \
         b23e30b6-7f65-477e-a302-bc37a730331a)(label(**))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 25))(sort Exp))((shape(Concave \
         25))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         d2edcc72-72b7-4b2f-9835-35df8038c2cf)(content(Whitespace\" \
         \"))))(Tile((id \
         aba584b2-d7cd-4a20-9a3c-66720f86d026)(label(2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         7d2e0896-df0b-4814-9ede-f51b39e468b3)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         d45cf076-cc02-4d5c-a8e2-87248a35c907)(content(Whitespace\"\\n\"))))(Secondary((id \
         f2f48222-34b2-4194-9d22-a456a2fe0886)(content(Comment\"# Expressions \
         currently CAN'T BE EDITED WHILE PROBED #\"))))(Secondary((id \
         e595fd43-805d-4e97-b0d7-180adf068543)(content(Whitespace\"\\n\"))))(Secondary((id \
         6975bc5a-677b-4447-8079-efd45dd90117)(content(Comment\"# So probing a \
         name instead makes iteration easier. #\"))))(Secondary((id \
         96522d91-0ced-4bbb-b2b8-e74b2d4b9a04)(content(Whitespace\"\\n\"))))(Secondary((id \
         a2bf1487-3149-4f45-9ef9-5e713e986d7f)(content(Whitespace\"\\n\"))))(Secondary((id \
         fa0d3e26-bc81-49e4-a4d7-071a98bcc2ba)(content(Whitespace\"\\n\"))))(Secondary((id \
         5a5bd86f-97e5-4085-a2ff-69ac2279cd09)(content(Comment\"# FUNCTIONS \
         #\"))))(Secondary((id \
         94b7c94c-bf2d-4b3f-a5e3-9a2ffc9b44b6)(content(Whitespace\"\\n\"))))(Tile((id \
         46f4b79a-39ab-41f2-9ae6-129d349eda5a)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         40))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         1df7de16-04da-4a44-9ae7-541950579bcf)(content(Whitespace\" \
         \"))))(Tile((id \
         022febdf-f520-4447-bdbb-20bee951b20a)(label(_))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         b75720e2-82ef-48cd-8d40-9d76d339401d)(content(Whitespace\" \
         \")))))((Secondary((id \
         018a8f64-bec7-44ed-8f34-6fc62ac702d0)(content(Whitespace\"\\n\"))))(Secondary((id \
         07af0cb0-ca39-4985-a82f-9a265a7b908b)(content(Comment\"# Because \
         functions can run multiple times, they can #\"))))(Secondary((id \
         a202afac-f226-4be7-90b2-ec62dacc6870)(content(Whitespace\"\\n\"))))(Secondary((id \
         5260e70f-f31e-4b0b-a857-f2627e638508)(content(Comment\"# have \
         multiple cells. Note the closure counts below #\"))))(Secondary((id \
         cf27e85e-390a-42fc-bd2c-2642597f4301)(content(Whitespace\"\\n\"))))(Secondary((id \
         b3deb81e-ad3e-4c8e-90da-c5d19aa58e09)(content(Comment\"# are all 2, \
         indicating each probe was evaluated twice. #\"))))(Secondary((id \
         7636a6d2-37f5-448d-b099-10eaf31a3d4e)(content(Whitespace\"\\n\"))))(Tile((id \
         be3b8cae-fddc-4aa4-9b7c-f4757ab6a4c8)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         38))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         f63ba14b-5926-4bd1-b0fe-5234a300c516)(content(Whitespace\" \
         \"))))(Tile((id \
         b688a68c-5ffd-4b07-b0b3-4f978e92869a)(label(celsius))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         10879894-2843-4d7a-bd41-8336ecdf123b)(content(Whitespace\" \
         \")))))((Secondary((id \
         6d46b68e-53e5-480e-88c8-da42be3b4757)(content(Whitespace\" \
         \"))))(Tile((id ec9d2d3d-0072-4166-85b7-a3f977c2310b)(label(fun \
         ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 35))(sort Exp))))))(shards(0 \
         1))(children(((Secondary((id \
         285854c2-b9cb-420c-a090-3b20b728b0d8)(content(Whitespace\" \
         \"))))(Projector((id 5e142e6c-9dc4-4e40-b374-8e0f9e09e12d)(kind \
         Probe)(syntax(Tile((id \
         e14c4c34-a3b0-41d4-b60f-a07fe4d2592d)(label(\"(\"\")\"))(mold((out \
         Pat)(in_(Pat))(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0 1))(children(((Tile((id \
         0442f817-d7a3-4e24-9e07-16b08595cc4e)(label(farenheit))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))))))))(model\"((display_lengths())(max_closures \
         30)(index_offset 0))\")))(Secondary((id \
         bc4736da-eee9-4b87-a419-b77749bd2f02)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         fecb6e0e-9116-4065-911f-d2d8918df8ce)(content(Whitespace\"\\n\"))))(Secondary((id \
         b90142c1-c95a-44fe-a775-cb1bda2d5cb7)(content(Comment\"# Click to \
         select the cell above reading 72.5 #\"))))(Secondary((id \
         e120513c-0827-48e9-a01f-5a80dbba05b8)(content(Whitespace\"\\n\"))))(Tile((id \
         741b73a5-f6cb-45c2-82c3-53ed83bd0b5b)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         38))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         955c96b5-77c1-4cd5-95eb-aad3e768758a)(content(Whitespace\" \
         \"))))(Tile((id \
         381d123c-aeb4-48b3-bda0-c932566212a3)(label(diff))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         7e6a68b9-934d-4380-af68-ff77bc37a0e3)(content(Whitespace\" \
         \")))))((Secondary((id \
         d8000ddc-5326-4f68-9542-e949e018eba4)(content(Whitespace\" \
         \"))))(Projector((id 8cea91c0-5237-4b8d-a880-22637326e7fd)(kind \
         Probe)(syntax(Tile((id \
         d7c81dc9-fb6f-4e56-a17c-9c906e64abcb)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         b10fc232-865e-4aa4-b1a8-a0cfc606dd8c)(label(farenheit))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         d854a0ba-4bc8-4ecd-8f56-f9a178aec05b)(content(Whitespace\" \
         \"))))(Tile((id \
         19f6c532-4833-4f06-9fed-d47ef67936b4)(label(-.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 27))(sort Exp))((shape(Concave \
         27))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         7913ef62-0f2f-48dd-9996-eccc2544ee6f)(content(Whitespace\" \
         \"))))(Tile((id \
         99d32236-e18e-458e-be43-6baf7e8920be)(label(32.))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))))))))(model\"((display_lengths())(max_closures \
         30)(index_offset 0))\")))(Secondary((id \
         e75d208f-b264-4972-ac07-2d64c49ed5a7)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         c80decb7-5393-4cb8-927c-0bfd852b3021)(content(Whitespace\"\\n\"))))(Secondary((id \
         277f4693-4ef4-45bb-b9d5-98c7ea40dbc7)(content(Comment\"# This \
         highlights cells below corresponding to the same \
         #\"))))(Secondary((id \
         eec20346-3c6d-4650-924b-9596b6b5ec1f)(content(Whitespace\"\\n\"))))(Secondary((id \
         47acc666-1cc6-4c44-9226-f11b4bacb48f)(content(Comment\"# function \
         call: the cells reading 40.5 and 22.5) #\"))))(Secondary((id \
         af92f9f8-e46d-49d5-a273-bf71e93cc20e)(content(Whitespace\"\\n\"))))(Projector((id \
         2b80f494-c2c5-4e83-acb0-b77d4bc09199)(kind Probe)(syntax(Tile((id \
         17d17d91-aace-4814-aead-ce80038b48d1)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         bbb9c2db-826e-4167-8793-1e8cc4908112)(label(5.))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         981f06b0-e429-4aac-a3fa-fda6ae3a51c2)(label(/.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 26))(sort Exp))((shape(Concave \
         26))(sort Exp))))))(shards(0))(children())))(Tile((id \
         3acdabbb-af17-4f04-a536-044d0997fb3a)(label(9.))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         743d3c11-27fb-4556-84c6-36cb0fc060b5)(content(Whitespace\" \
         \"))))(Tile((id \
         d1eb8c70-3df8-4f5b-82dd-e896de389079)(label(*.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 26))(sort Exp))((shape(Concave \
         26))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         a9d614cc-3464-4fd4-8039-c72d56c83b74)(content(Whitespace\" \
         \"))))(Tile((id \
         e96548b8-74eb-4924-9d50-70f5c0b39980)(label(diff))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))))))))(model\"((display_lengths())(max_closures \
         30)(index_offset 0))\")))(Secondary((id \
         109a5551-3e65-4546-96d8-452d502cd2ce)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         a1ff4c4d-0858-4b43-a0d8-1d1d84566b65)(content(Whitespace\"\\n\"))))(Tile((id \
         cee92f6b-e3d4-4e93-910e-240a113e1fd7)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         38))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         05cafacf-7103-4f66-b615-8434ea191844)(content(Whitespace\" \
         \"))))(Tile((id \
         9c4152b4-86cc-4de6-9829-c6f7ed94fb04)(label(\"(\"\")\"))(mold((out \
         Pat)(in_(Pat))(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0 1))(children(((Tile((id \
         f7a1669d-914c-4371-84e5-9c428616adfe)(label(t1))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         599dfc0b-ba5d-4b75-86bf-e8dc432649d0)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 45))(sort Pat))((shape(Concave \
         45))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         0995d86c-d171-4016-8855-4f4bb7d84f45)(content(Whitespace\" \
         \"))))(Tile((id \
         76c212f9-1f91-40fa-a6b4-5bbef78c0af8)(label(t2))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children()))))))))(Secondary((id \
         485b94bf-3949-4715-aeb0-005da606e820)(content(Whitespace\" \
         \")))))((Secondary((id \
         45c7fca6-2e2f-42e9-93a6-46c799365883)(content(Whitespace\" \
         \"))))(Tile((id \
         816a8682-180f-470a-acc0-e446bda32016)(label(72.5))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         31671464-29ab-4567-86f2-fd0a80f8f093)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 45))(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         3e69a469-8122-4ab1-8b2e-f0d86951d505)(content(Whitespace\" \
         \"))))(Tile((id \
         356304f5-9453-4e07-bb63-710bbc7746c5)(label(103.1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         d87162eb-cba8-487c-a3bd-400f173e5490)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         8e715679-cecd-4e19-8223-823ef8bfbb95)(content(Whitespace\" \
         \"))))(Tile((id \
         e37c2fc9-d316-40a9-93e6-723ae6462176)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         cc804264-49d9-4392-b838-5c6480c57944)(content(Whitespace\"\\n\"))))(Secondary((id \
         4880d643-3cfd-485a-ba56-8606bd5f037e)(content(Comment\"# It also \
         highlights in purple the cell #\"))))(Secondary((id \
         fe79d97a-dd39-4862-950e-cbef5a9bf40f)(content(Whitespace\"\\n\"))))(Secondary((id \
         a9092cd3-30f0-47a4-9134-c4043209cc1a)(content(Comment\"# of the \
         function's call site#\"))))(Secondary((id \
         83b35adb-c91f-45bd-a79a-d0bf418cedc7)(content(Whitespace\"\\n\"))))(Projector((id \
         41328039-9a10-4c68-b9bb-5e0eac78ab7d)(kind Probe)(syntax(Tile((id \
         255be14d-e115-44ee-968e-2fcd8004c492)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         a4065fb9-aafa-4d87-b8cf-96921ae61034)(label(celsius))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         80bd6abc-7bcb-4927-ab07-2d538e5bd989)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 22))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         a3b160bf-d155-4b47-b881-402a1dcfcf8e)(label(t1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))))))))(model\"((display_lengths())(max_closures \
         1)(index_offset 0))\")))(Tile((id \
         573e391b-af0c-4472-979b-a1de1ce15a6e)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 45))(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         c0b5755f-1b2e-40cf-abd8-2f84ee1ca5e7)(content(Whitespace\"\\n\"))))(Secondary((id \
         e5d5edc9-c7c7-403c-9567-8f1f07850964)(content(Comment\"# Now select \
         the cell above reading 22.5 #\"))))(Secondary((id \
         10a17a6a-c2dd-4a6a-ab44-df5cd5b018e9)(content(Whitespace\"\\n\"))))(Projector((id \
         128199b0-7c0b-48d5-a75d-0ce6c6548a40)(kind Probe)(syntax(Tile((id \
         3400eba9-37b2-4f24-9a9e-ddb4f555ad06)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         beb692d1-a157-491d-8964-4c64a9131d9d)(label(celsius))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         fea69b06-b9e8-4b4e-8de3-24d8e568db76)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 22))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         74f5d3a3-8cb2-423a-9a79-c403329873cb)(label(t2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))))))))(model\"((display_lengths())(max_closures \
         30)(index_offset 0))\")))(Secondary((id \
         8d37397c-1c6b-4355-85a3-c8aa903462a0)(content(Whitespace\"\\n\"))))(Secondary((id \
         1b1080c9-efcd-415f-b107-98c840940bbf)(content(Comment\"# Note the \
         72.5, 40.5, and 22.5 are no longer green-highlit \
         #\"))))(Secondary((id \
         ec0f5c50-4acd-445b-add5-a0dad659cf4e)(content(Whitespace\"\\n\"))))(Secondary((id \
         d2019b02-27a8-466f-9300-137fd727ecf4)(content(Comment\"# as they are \
         not part of the same call as /the expression/ #\"))))(Secondary((id \
         6590b7cd-39fe-4950-9bf5-9d46375054e6)(content(Whitespace\"\\n\"))))(Secondary((id \
         8452a89c-baf2-43c7-bd9c-f9d4d2ee2cd9)(content(Comment\"# \
         `celsius(t1)`. However, they now have purple hats, indicating \
         #\"))))(Secondary((id \
         79d88078-d8eb-44d2-852e-c22d8b806ec0)(content(Whitespace\"\\n\"))))(Secondary((id \
         48bd7065-46ee-4754-bd3f-d02cb99ee815)(content(Comment\"# they are \
         below that function call in the call stack #\"))))(Secondary((id \
         3ee58d27-ffd8-4ed2-8642-a68fb275cb64)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         ded5e120-7142-4c1f-bb75-fb4c0cdf87ba)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         de104f05-9547-4b57-a823-979085d3ab61)(content(Whitespace\"\\n\"))))(Secondary((id \
         91576e63-3b41-49b0-af3a-8aeef101889b)(content(Whitespace\"\\n\"))))(Secondary((id \
         571a6e6f-1caa-4eb6-913b-74238b9756a2)(content(Comment\"# BRANCHING IN \
         FUNCTIONS #\"))))(Secondary((id \
         ea374bcb-1c77-4a22-8f39-2a165af04770)(content(Whitespace\"\\n\"))))(Tile((id \
         d04fbef9-7f8a-408f-9bd6-407a072a8881)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         40))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         05a948aa-1ec7-4c38-b581-81e89c1c92aa)(content(Whitespace\" \
         \"))))(Tile((id \
         f905f735-722e-415a-be76-9e29c16e8ded)(label(cases))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         7e1e36dd-fb36-4273-81b6-e8b9e3efe637)(content(Whitespace\" \
         \")))))((Secondary((id \
         d4a0255e-6fb5-4556-a075-31b19fbc69a7)(content(Whitespace\"\\n\"))))(Secondary((id \
         5cd36fa3-0751-4a8a-83aa-9d8e162869c3)(content(Comment\"# Select `6` \
         then `5` then '4' below: #\"))))(Secondary((id \
         e4152dd3-4359-4b71-9519-629aa2f6f8ab)(content(Whitespace\"\\n\"))))(Tile((id \
         90932567-f60c-4bb4-9cd7-5f524894c08c)(label(fun ->))(mold((out \
         Exp)(in_(Pat))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         8ce5f496-2b7d-4f5b-a168-eabf020656e7)(content(Whitespace\" \
         \"))))(Projector((id cc982279-6beb-4bab-b215-551af87eca90)(kind \
         Probe)(syntax(Tile((id \
         cc982279-6beb-4bab-b215-551af87eca90)(label(\"(\"\")\"))(mold((out \
         Pat)(in_(Pat))(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0 1))(children(((Tile((id \
         e16865af-0ef0-4f7d-93e6-f94aedcbae68)(label(x))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))))))))(model\"((display_lengths())(max_closures \
         30)(index_offset 0))\")))(Secondary((id \
         35c98bde-6628-411d-abd8-ecb4dc1f29e9)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         1f444713-740a-40e7-a047-fe7657bf97e8)(content(Whitespace\" \
         \"))))(Tile((id b8464b07-5b53-45ba-ab42-7deb6914cd94)(label(case \
         end))(mold((out Exp)(in_(Rul))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         2b4cbcac-0f95-4309-a56e-09acdf57b563)(content(Whitespace\" \
         \"))))(Tile((id \
         f4385b69-64ba-4161-9bcf-03a78d5e0ff4)(label(x))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         6eac66be-4e09-4d38-be1e-d9a5de81368d)(content(Whitespace\" \
         \"))))(Secondary((id \
         152ca988-9e8f-48d9-a77b-8ac16b9a1d65)(content(Whitespace\"\\n\"))))(Secondary((id \
         1f6924e3-0d13-4e40-9b78-49dfbed4d0a6)(content(Comment\"# Note how \
         each activate exactly one branch below: #\"))))(Secondary((id \
         84948a93-a46a-4ba8-8be9-cd893755cbd6)(content(Whitespace\"\\n\"))))(Tile((id \
         12bc0362-9bfc-4f8a-9418-1e6dc15f3e01)(label(| =>))(mold((out \
         Rul)(in_(Pat))(nibs(((shape(Concave 41))(sort Exp))((shape(Concave \
         41))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         51c69013-dbef-4462-9242-84626ae37d6e)(content(Whitespace\" \
         \"))))(Tile((id \
         1a9ef844-38b2-4ad3-895b-2ccad47ac7fb)(label(4))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         21ea6a44-59d9-41bc-84ac-d23593e4b00a)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         32038cb8-1cf1-45b2-a999-74900c9b0424)(content(Whitespace\" \
         \"))))(Projector((id 2cdbd3ed-9b09-4792-afdd-6267dd849253)(kind \
         Probe)(syntax(Tile((id \
         2cdbd3ed-9b09-4792-afdd-6267dd849253)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         cdfbffcb-2e79-4960-b362-b0788feeddd4)(label(true))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))))))))(model\"((display_lengths())(max_closures \
         30)(index_offset 0))\")))(Secondary((id \
         45bc27f8-8ca7-4b75-9def-e5896f45d180)(content(Whitespace\"\\n\"))))(Secondary((id \
         28ec8945-9875-40c4-a9ce-36f40458998a)(content(Comment\"# Select the \
         `5` above and then the `false` below: #\"))))(Secondary((id \
         3f56b995-c320-4724-8852-383149d06f7b)(content(Whitespace\"\\n\"))))(Tile((id \
         af3d0927-cdd7-4340-8c25-531e2546b58c)(label(| =>))(mold((out \
         Rul)(in_(Pat))(nibs(((shape(Concave 41))(sort Exp))((shape(Concave \
         41))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         585ac864-f044-4e2d-9f77-b2efb42a3c0a)(content(Whitespace\" \
         \"))))(Tile((id \
         a08a67ce-e8fc-4b5d-af9c-ad812feda0aa)(label(5))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         c6f357e9-cc91-48c5-8c4b-3a6189e4c227)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         ba2340bd-a425-4b81-a6fd-ca4ca4c1433f)(content(Whitespace\" \
         \"))))(Projector((id c47218e0-3594-4351-9d77-58ec797f5f80)(kind \
         Probe)(syntax(Tile((id \
         c47218e0-3594-4351-9d77-58ec797f5f80)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         66e64b0d-22e0-4447-a8bf-75d447a113eb)(label(false))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))))))))(model\"((display_lengths())(max_closures \
         30)(index_offset 0))\")))(Secondary((id \
         db556050-d30e-443f-9a34-9ee252b2a780)(content(Whitespace\"\\n\"))))(Secondary((id \
         d987ed80-8af2-4ac9-8fba-0dec9383ae5c)(content(Comment\"# Note the \
         same things are highlit as both cells are #\"))))(Secondary((id \
         e0bc874c-115c-4790-a819-9448f975d80a)(content(Whitespace\"\\n\"))))(Secondary((id \
         55cf7952-2ebd-4666-a5cd-f0ba1dc5b443)(content(Comment\"# from the \
         same call to cases#\"))))(Secondary((id \
         a08fb9ca-81a2-4764-b53b-61ed9a6ce766)(content(Whitespace\"\\n\"))))(Tile((id \
         7dc0b796-c824-4508-bd87-93cdae235e40)(label(| =>))(mold((out \
         Rul)(in_(Pat))(nibs(((shape(Concave 41))(sort Exp))((shape(Concave \
         41))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         68d34468-7566-4286-a922-08f83b496045)(content(Whitespace\" \
         \"))))(Tile((id \
         a147f9f0-f9e5-4243-88e3-7ffe68cb9248)(label(_))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         342e3676-4b39-4812-81c1-b5ac1d4dfb19)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         bbb81763-4766-4fa4-a105-d9190c730c67)(content(Whitespace\" \
         \"))))(Projector((id 182ba112-a9f5-4ca6-809f-4df3eeb4a7da)(kind \
         Probe)(syntax(Tile((id \
         182ba112-a9f5-4ca6-809f-4df3eeb4a7da)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         e9338edc-f06e-48df-8ad6-21cb5fb1ae4b)(label(true))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))))))))(model\"((display_lengths())(max_closures \
         30)(index_offset 0))\")))(Secondary((id \
         da266654-2f45-40cb-a7ec-005118ffa239)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         37f3ead8-84f5-4a9a-932a-b869a5504106)(content(Whitespace\" \
         \"))))(Secondary((id \
         3c97dce3-49d8-49a4-84f4-ac67957b5d56)(content(Whitespace\" \
         \"))))(Secondary((id \
         fbf87b59-d9be-412f-a642-152ec75f97c3)(content(Whitespace\" \
         \"))))(Secondary((id \
         0815e4cf-67a1-4ce6-9269-9d1c40f583ea)(content(Whitespace\" \
         \"))))(Secondary((id \
         de4c6319-6cef-4802-8687-f5de4ff1ba67)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         db0b57f8-b577-4018-85f4-b4675c5d1a9e)(content(Whitespace\" \
         \"))))(Tile((id d06ce3a1-cd21-4810-ad41-eb655cd93c64)(label(let = \
         in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 40))(sort Exp))))))(shards(0 1 \
         2))(children(((Secondary((id \
         72b31316-a97d-4311-89d1-20813606faa7)(content(Whitespace\" \
         \"))))(Tile((id \
         bf3bb506-d2dd-4d5c-92dd-81b4617ee37b)(label(_))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         73e00f4e-22e1-448a-86e6-081f08dd8ee6)(content(Whitespace\" \
         \")))))((Secondary((id \
         1d080248-4467-46c5-9608-838edfe9f288)(content(Whitespace\" \
         \"))))(Tile((id \
         faacc8b6-104e-4d70-8d4b-5b681ef8f79f)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         62c7b308-25e8-46b6-be85-0dd1552e0a36)(content(Whitespace\"\\n\"))))(Secondary((id \
         25615a61-e624-4490-b5e0-50b20b900e3a)(content(Comment\"# Select \
         `true` below and then the `4` cell #\"))))(Secondary((id \
         86e0509d-a19c-492b-b245-f7cc0b6ddde5)(content(Whitespace\"\\n\"))))(Secondary((id \
         64fcad3d-992b-44d6-8d9e-03740bb081a1)(content(Comment\"# for the \
         argument x to `cases` above. #\"))))(Secondary((id \
         be5e95c6-5346-4295-b176-bb9da4ee48d6)(content(Whitespace\"\\n\"))))(Projector((id \
         7710ccdb-2805-4876-ba20-8a72c29bb1a1)(kind Probe)(syntax(Tile((id \
         37aa5b10-b2d3-4e7d-b491-8e699f789618)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         ec9850f5-a730-4ef7-839d-e7b952937e8e)(label(cases))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         fd2ab27f-db11-496c-8b88-09ffe839e293)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 22))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         850ab4f2-472a-4757-a09c-0d272ed04e7c)(label(4))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))))))))(model\"((display_lengths())(max_closures \
         30)(index_offset 0))\")))(Tile((id \
         62512420-91c6-443a-a1e3-8a1eff577580)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 45))(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         32bf9038-b61f-484b-835d-a44062d5c86a)(content(Whitespace\"\\n\"))))(Secondary((id \
         9aab4868-1f6e-4e0e-bd03-0a26dd75ac2c)(content(Comment\"# Note how the \
         same cells stay indicated, but the kind #\"))))(Secondary((id \
         69340416-aff4-4e69-82fa-dee53a21c618)(content(Whitespace\"\\n\"))))(Secondary((id \
         962248c8-19f1-455f-a421-8c954698d404)(content(Comment\"# of \
         indication changes. The `true` below the `4` above \
         #\"))))(Secondary((id \
         e47766c5-f286-42da-8919-00a81f75640e)(content(Whitespace\"\\n\"))))(Secondary((id \
         3298ff33-4846-4a5a-b85f-16384cb64f70)(content(Comment\"# goes from \
         purple outline (created by the cases(4) call) #\"))))(Secondary((id \
         82663a24-d843-467f-8437-ba55ba2006cd)(content(Whitespace\"\\n\"))))(Secondary((id \
         3414cf86-d48a-4dda-86e9-9fab0902e914)(content(Comment\"# to green \
         highlighting (part of the same call as `4`). #\"))))(Secondary((id \
         b8c09264-c86e-40fd-bf01-1d1520d134c9)(content(Whitespace\"\\n\"))))(Secondary((id \
         178bcd8e-f855-4dcb-b15c-065a0a065007)(content(Comment\"# The formerly \
         selected lower `true` is now highlit in #\"))))(Secondary((id \
         0f6643a3-50b4-4f41-ba2b-f0f14f5c682a)(content(Whitespace\"\\n\"))))(Secondary((id \
         5a564672-ea43-4301-98cc-60ee4aa695d0)(content(Comment\"# purple since \
         it indicates the call where `4` lives . #\"))))(Secondary((id \
         5e58057a-344d-43e6-a397-63fb71692915)(content(Whitespace\"\\n\"))))(Projector((id \
         d7a07d09-53a5-48ce-a013-3ff40e63d454)(kind Probe)(syntax(Tile((id \
         9262fc32-135c-4e63-bfd8-43d272f5aed2)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         6f03fd3a-a5e1-4adf-8d20-1e5c4b7cea99)(label(cases))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         4d346602-f800-4847-a635-e98b9b125d00)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 22))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         cf13df3e-413b-4b6b-bb79-b7bf60ee3864)(label(5))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))))))))(model\"((display_lengths())(max_closures \
         30)(index_offset 0))\")))(Tile((id \
         1d45d1f9-37d4-42c5-b21a-c688ec2afad9)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 45))(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         e2e1fa35-f049-4ea9-9f8b-ea105d436037)(content(Whitespace\"\\n\"))))(Projector((id \
         164f4ffd-a193-495d-9cb5-94562b1ed4bc)(kind Probe)(syntax(Tile((id \
         98236fcc-c680-4be9-a8c5-0ea8101348b9)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         3afcfd61-96a8-40c8-a40a-4c62daf833a8)(label(cases))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         f62fd59e-eb01-4ae2-a52d-ecd78a2edd73)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 22))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         f2c4822d-7e96-4bd7-b042-eee449a82087)(label(6))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))))))))(model\"((display_lengths())(max_closures \
         30)(index_offset 0))\")))(Secondary((id \
         4bfc63b3-74c8-44b9-a597-ed799adc38e4)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         79f48721-74c3-420f-b8cf-128cde905a80)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         c6f3f748-45dd-4b3b-a602-d42815d2c691)(content(Whitespace\"\\n\"))))(Secondary((id \
         d46111c8-a58a-4d4a-a771-85b3680f9717)(content(Whitespace\"\\n\"))))(Secondary((id \
         bcd94420-2ed5-405a-8986-8239d1522081)(content(Comment\"# FUNCTIONS \
         CALLING FUNCTIONS #\"))))(Secondary((id \
         e01404dc-4d93-4503-a9bf-a39fb16abf45)(content(Whitespace\"\\n\"))))(Tile((id \
         7e25aa64-ffc8-4698-8ac3-6a5c2b5e6466)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         40))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         f8be34e5-0f31-44cb-913d-930f30c3833f)(content(Whitespace\" \
         \"))))(Tile((id \
         5495cfdf-f7aa-4907-b8d1-0ee27abf4a11)(label(_))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         74bf69ac-345f-432f-884e-d9c2f3a64d11)(content(Whitespace\" \
         \")))))((Secondary((id \
         36010e39-0274-4af5-9562-6879ccc5d372)(content(Whitespace\"\\n\"))))(Secondary((id \
         fae230be-64f9-4fff-9f49-3e571813e438)(content(Whitespace\" \
         \"))))(Secondary((id \
         baeb1a92-0e19-4182-af52-fbfaccbd6fd5)(content(Whitespace\" \
         \"))))(Secondary((id \
         135c4690-3c44-48bc-97aa-abf61d38c781)(content(Comment\"# Select `9` \
         below. Note four cells below become purple #\"))))(Secondary((id \
         b5d6db27-1fa5-420f-8334-d4b2c0a992a8)(content(Whitespace\"\\n\"))))(Tile((id \
         d91ac691-d448-444a-8bf9-67e2977512ea)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         38))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         d5647464-9925-436e-9023-e49fd45e1fc7)(content(Whitespace\" \
         \"))))(Tile((id \
         50377d69-393d-4ccb-98d4-eb65972d4199)(label(fourth))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         7493d96f-3f03-4a3a-a587-823c13cbabb1)(content(Whitespace\" \
         \")))))((Secondary((id \
         24d674fd-66a8-4591-a547-579b88c62af6)(content(Whitespace\" \
         \"))))(Tile((id b981d568-7d51-47c0-9d2f-924784478940)(label(fun \
         ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 35))(sort Exp))))))(shards(0 \
         1))(children(((Secondary((id \
         7f88f422-e436-4cb8-8d12-814491a1fc53)(content(Whitespace\" \
         \"))))(Tile((id \
         a4a0fdfd-821f-47d3-b7e4-1a70f3d20d57)(label(f))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         17f11cde-b8ad-47d6-9b00-7f3b0c961114)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         59a13bb8-3f86-48e4-a36a-0a2a1988da95)(content(Whitespace\" \
         \"))))(Tile((id \
         7eddffc5-856d-41ec-a911-9cddbf637566)(label(4))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         b75213aa-5e2a-4bec-aff2-f3e04d6eb1c2)(content(Whitespace\" \
         \"))))(Tile((id \
         57602e14-b71d-467a-bd70-bd0229e5f88a)(label(*))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 26))(sort Exp))((shape(Concave \
         26))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         a781dd63-3432-4fe5-88b8-6e7f450144f5)(content(Whitespace\" \
         \"))))(Projector((id 4ba07c63-0f70-4f4f-b9bd-0fc7348965a0)(kind \
         Probe)(syntax(Tile((id \
         4ba07c63-0f70-4f4f-b9bd-0fc7348965a0)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         50753a87-7ba9-4528-958f-31a179d13cc5)(label(f))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))))))))(model\"((display_lengths())(max_closures \
         30)(index_offset 0))\")))(Secondary((id \
         0bc7c95d-cb36-4b4b-85ec-c1c07e0752b3)(content(Whitespace\" \
         \"))))(Tile((id \
         6bc29446-b697-4015-a7ab-9d666f07afc6)(label(-))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 27))(sort Exp))((shape(Concave \
         27))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         526a6a21-11b8-43f5-9f60-631a8f7f52f8)(content(Whitespace\" \
         \"))))(Tile((id \
         31827d1b-10a8-41b9-a211-46bf81b4abdc)(label(4))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         f06eef56-7449-4671-b2b3-7244a8d81f38)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         3ffe82f0-a80c-487b-8faa-8ea8d4c9feb1)(content(Whitespace\"\\n\"))))(Secondary((id \
         5147c81d-cf83-4a52-8cd4-a256f597fdeb)(content(Whitespace\" \
         \"))))(Secondary((id \
         f669f9ef-d9e5-49c8-9173-8aea6b68aafb)(content(Whitespace\" \
         \"))))(Secondary((id \
         d610e6c9-49ef-4b0f-9a16-a81abc9e6538)(content(Comment\"# This is \
         because they represent function calls #\"))))(Secondary((id \
         a98cecd2-0472-44d8-9e7d-37197ea1f684)(content(Whitespace\"\\n\"))))(Secondary((id \
         ce9c8e39-1134-4cad-a1f7-02ac54943771)(content(Whitespace\" \
         \"))))(Secondary((id \
         9c018fb1-659f-4469-99c1-30222fadadb8)(content(Whitespace\" \
         \"))))(Secondary((id \
         502f7709-6ed7-41e6-b074-659edd93570f)(content(Comment\"# above the \
         `9` cell in the function call stack. #\"))))(Secondary((id \
         839a9b02-520b-4164-8b71-ad25a1f436fe)(content(Whitespace\"\\n\"))))(Secondary((id \
         6b01f6ee-c842-496b-9fb1-b52e6991f194)(content(Whitespace\" \
         \"))))(Secondary((id \
         ebdfc9ad-24b9-40c8-8e9b-c0057e75fbe1)(content(Whitespace\" \
         \"))))(Secondary((id \
         0ffde64a-b61c-44c4-b01f-0eb36b8567a8)(content(Comment\"# For example \
         32 below represents the call producing `9`.  #\"))))(Secondary((id \
         fb8a4fbd-3383-49da-a41d-c7f9057774a6)(content(Whitespace\"\\n\"))))(Tile((id \
         23a7d68d-800a-41a0-be0e-03e541d868db)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         38))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         1b7f096a-6ecd-4d73-a759-54c2bae7d63e)(content(Whitespace\" \
         \"))))(Tile((id \
         4c45d6c4-9eec-4e25-9679-0e3de70bf4a4)(label(third))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         fb944e85-1b0a-432a-b6fa-05f5f8ec23ed)(content(Whitespace\" \
         \")))))((Secondary((id \
         98191633-92f0-4370-8f58-9f50733bd943)(content(Whitespace\" \
         \"))))(Tile((id b695122b-ddc9-4526-8fe8-10f3941192f1)(label(fun \
         ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 35))(sort Exp))))))(shards(0 \
         1))(children(((Secondary((id \
         997a35e2-1eef-44f8-9bf5-f847d2b67c15)(content(Whitespace\" \
         \"))))(Tile((id \
         c18b58a9-ac79-47eb-9060-ed0b5cf6dffb)(label(t))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         a3e78817-7ce0-4bfb-b4ee-e9cdbcc7763f)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         d207b961-ae4c-4008-ac5f-d96066d8d26c)(content(Whitespace\" \
         \"))))(Projector((id a65f6543-5dfb-4358-b6c7-2a06ccc77ae2)(kind \
         Probe)(syntax(Tile((id \
         a65f6543-5dfb-4358-b6c7-2a06ccc77ae2)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         75173c9f-38b6-450b-94e3-cc4f9289bee0)(label(fourth))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         bc0b4577-1e06-435e-932d-56573c481ac8)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 22))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         4faabeb1-eee3-4f33-bf0b-7862f091eda3)(label(t))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         e19b456f-0382-4a56-a34c-e91630d8bdd2)(content(Whitespace\" \
         \"))))(Tile((id \
         4f5a3f9a-abee-42ed-b6a0-97106167ca5f)(label(-))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 27))(sort Exp))((shape(Concave \
         27))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         622c40a7-e467-4b84-99d0-d01afc7c46c6)(content(Whitespace\" \
         \"))))(Tile((id \
         30e16e16-ed03-4f72-8515-db68e2585bb2)(label(3))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))))))))(model\"((display_lengths())(max_closures \
         30)(index_offset 0))\")))(Secondary((id \
         4caa88c0-2f72-4787-a29d-e93df21d40b0)(content(Whitespace\" \
         \"))))(Tile((id \
         6cec6cc0-f4f4-4f14-a046-31691f5a154a)(label(/))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 26))(sort Exp))((shape(Concave \
         26))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         42067285-f1a8-4508-9d0b-1ace72483010)(content(Whitespace\" \
         \"))))(Tile((id \
         cfe1f794-8d8f-419e-9ba3-27c4b7f7d491)(label(3))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         f046a882-b321-4740-885f-be552dd3fbb0)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         9bde4634-85e9-4597-84dc-a00d8a147978)(content(Whitespace\"\\n\"))))(Secondary((id \
         90957323-6499-487e-9783-0f7388739b50)(content(Whitespace\" \
         \"))))(Secondary((id \
         d08fd6c6-9822-46e4-9c27-63f59a81b3ba)(content(Whitespace\" \
         \"))))(Secondary((id \
         3621ad20-1b30-4313-95aa-07c1b4bbacc0)(content(Comment\"# Now, select \
         `32` above. Note the 9 now has a purple hat. #\"))))(Secondary((id \
         d249bbdb-1721-4d5e-ab16-a3e6f577c737)(content(Whitespace\"\\n\"))))(Secondary((id \
         22ad8e0c-0aa6-44d6-b77e-dba3c301acf6)(content(Whitespace\" \
         \"))))(Secondary((id \
         a3e77040-bafe-4cd5-94f0-6961df121adb)(content(Whitespace\" \
         \"))))(Secondary((id \
         3839409b-54ae-487d-99cf-3c824faf3afb)(content(Comment\"# This \
         represents that it is below the `32` call in the stack. \
         #\"))))(Secondary((id \
         0cc11ceb-b722-4708-ba73-86679b183a85)(content(Whitespace\"\\n\"))))(Secondary((id \
         902cba2d-4107-493f-8e73-a7a7158fd862)(content(Whitespace\" \
         \"))))(Secondary((id \
         49b5695f-e8e0-463d-a3b2-5004f79117c9)(content(Whitespace\" \
         \"))))(Secondary((id \
         42194102-7b50-45ca-93c1-1898dfe803da)(content(Comment\"# Now select \
         `10` below, which is a call to `third`: #\"))))(Secondary((id \
         3d40d39c-9273-4df9-9c6b-dc2d7881e0bf)(content(Whitespace\"\\n\"))))(Tile((id \
         05d0e32f-3905-44d4-8057-269fd337471f)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         38))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         514fb8f3-07d5-4faf-83b9-c3ee5319e42e)(content(Whitespace\" \
         \"))))(Tile((id \
         c2e2dacc-cc83-4155-b579-adb54c778da7)(label(second))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         395bd272-85f4-450f-b748-fb9b29f7e9bf)(content(Whitespace\" \
         \")))))((Secondary((id \
         6628b098-c094-41d4-b44b-1ba7bd7956a4)(content(Whitespace\" \
         \"))))(Tile((id 9ffbb82a-9642-461a-90b9-c715e563482d)(label(fun \
         ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 35))(sort Exp))))))(shards(0 \
         1))(children(((Secondary((id \
         cf2bfddc-9c82-4b09-963d-029c7ef4f0f2)(content(Whitespace\" \
         \"))))(Tile((id \
         22da03a5-fda1-42d3-9310-a6d0fa20f1c1)(label(s))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         0b476dc3-83fd-444b-a69d-3b2b603a726f)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         ad3461af-834c-4011-bc4b-715ca8c0ed30)(content(Whitespace\" \
         \"))))(Projector((id ada6dc19-1b3d-4f32-a76e-9c55cecaee79)(kind \
         Probe)(syntax(Tile((id \
         864e9772-ddab-42fe-a1fe-ba1b874ac617)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         e4db050e-9eac-49b8-993a-43c18d504d76)(label(third))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         619f64e3-7927-4d23-bd1f-d90cec4193c6)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 22))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         3eb4e1f7-ef21-452b-8170-00421bc8a58b)(label(2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         196b6770-fe7b-4fe6-bdf1-38dc374cbc96)(content(Whitespace\" \
         \"))))(Tile((id \
         4c71c327-eec1-4c85-9d22-29a5eb3634ad)(label(*))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 26))(sort Exp))((shape(Concave \
         26))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         77589628-fc1f-4e42-8f22-3543681d005b)(content(Whitespace\" \
         \"))))(Tile((id \
         49629947-5d0c-4bdb-a73f-f6d72d2449a4)(label(s))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))))))))(model\"((display_lengths())(max_closures \
         30)(index_offset 0))\")))(Secondary((id \
         9136a0ab-3e48-4bdf-8481-98b6413417ab)(content(Whitespace\" \
         \"))))(Tile((id \
         ccb0959b-6c18-432d-837d-5875dc527a87)(label(+))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 27))(sort Exp))((shape(Concave \
         27))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         60ec26c7-20c5-4b0c-8e13-73da078ce209)(content(Whitespace\" \
         \"))))(Tile((id \
         4a1a503c-77e9-49d1-aa59-135b7b563f78)(label(2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         7a580623-8a76-4a85-90d8-411532d86d32)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         85728700-0e7e-4b18-afaa-7466e5cfa700)(content(Whitespace\"\\n\"))))(Secondary((id \
         0fd1d823-3e18-44b9-80e0-420004e84c72)(content(Whitespace\" \
         \"))))(Secondary((id \
         17a81785-1281-40ba-92a8-3ac7b7412ad3)(content(Whitespace\" \
         \"))))(Secondary((id \
         99c76088-1180-4c25-9d85-1c29d6892f01)(content(Comment\"# Note that \
         `9` and `32` both have hats. `9` has a taller hat \
         #\"))))(Secondary((id \
         75c47089-0e1d-4ca2-b9b8-daafb2b2ed89)(content(Whitespace\"\\n\"))))(Secondary((id \
         c7287406-3f76-4dbf-b8a7-28d48d7ead00)(content(Whitespace\" \
         \"))))(Secondary((id \
         f2132d48-840a-44ee-887a-9f8fdc0079ab)(content(Whitespace\" \
         \"))))(Secondary((id \
         97af4b6f-4b05-4875-b371-991896d4d6df)(content(Comment\"# to show it's \
         lower in the call stack. `32` has purple text #\"))))(Secondary((id \
         e0ea4de8-ebe2-48bc-9d6f-f21dc3948725)(content(Whitespace\"\\n\"))))(Secondary((id \
         dd6de78e-c756-4da0-a45a-8c7aab80d6c1)(content(Whitespace\" \
         \"))))(Secondary((id \
         bb054980-c9c6-43fa-8105-0a819c844add)(content(Whitespace\" \
         \"))))(Secondary((id \
         54ac2dd0-56e3-4ddd-bf5d-6f7dcb126234)(content(Comment\"# to further \
         emphasize that is is directly below `10`. #\"))))(Secondary((id \
         6f7d697a-d51f-41de-8238-ac7bf7ed976f)(content(Whitespace\"\\n\"))))(Secondary((id \
         5fd99ff5-15d8-4a1b-af66-c6b0f5665e5d)(content(Whitespace\" \
         \"))))(Secondary((id \
         e95b4613-349b-42fa-9cf5-8255a0ee4d24)(content(Whitespace\" \
         \"))))(Secondary((id \
         94feed4a-dafa-4afb-848c-d4e1216ecc28)(content(Comment\"# Now select \
         12 below, representing a call to `second` #\"))))(Secondary((id \
         fb2a1b2b-237c-441c-9dee-876af6a0683f)(content(Whitespace\"\\n\"))))(Tile((id \
         df8df94e-a8dc-4c84-aa1f-1ccb429faf61)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         38))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         08ae7f95-9046-4470-823a-8d9ce4e04c34)(content(Whitespace\" \
         \"))))(Tile((id \
         18dac326-3e90-42c1-ba19-926e5f07e29a)(label(first))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         4282cc84-835e-42a0-a338-9a938b17cf9f)(content(Whitespace\" \
         \")))))((Secondary((id \
         311f869a-a48d-4de3-8fa4-e1e6170d981f)(content(Whitespace\" \
         \"))))(Tile((id 4867243e-5a54-4360-8b16-78c0279758c3)(label(fun \
         ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 35))(sort Exp))))))(shards(0 \
         1))(children(((Secondary((id \
         110d1b74-77fa-43e6-b1c2-6ec286d81fd7)(content(Whitespace\" \
         \"))))(Tile((id \
         3847cb07-1d9f-4eb9-b410-b8a32f481990)(label(f))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         9a71c718-5ef9-4e3b-9b5b-af7da7202c7f)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         122528cb-4722-43b1-b0c2-59f2108c10ed)(content(Whitespace\" \
         \"))))(Projector((id 7b94117e-0a29-421e-84a8-19bfba33b9d5)(kind \
         Probe)(syntax(Tile((id \
         5e68e2a3-d03a-40fd-b18b-8d6881090dac)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         a27d14a0-4f09-428d-85c6-134e9c413996)(label(second))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         56489aa7-4c3d-45f7-a707-31ef16b00a98)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 22))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         da3a6d8b-4b5d-48ff-9245-32329498191f)(label(f))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         318a9140-ffba-4a35-8228-b3107231dc9b)(content(Whitespace\" \
         \"))))(Tile((id \
         3e83c9c9-a00f-409f-a54e-2732625ec9e5)(label(+))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 27))(sort Exp))((shape(Concave \
         27))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         c690b3f7-b24e-41d2-8a8f-fb8b5e2c7577)(content(Whitespace\" \
         \"))))(Tile((id \
         5a0f7a8c-13b8-4aee-8766-947a5e5289ae)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))))))))(model\"((display_lengths())(max_closures \
         30)(index_offset 0))\")))(Secondary((id \
         33aa70f9-5e12-4450-bc27-92a245a453fc)(content(Whitespace\" \
         \"))))(Tile((id \
         f331f2c6-081e-4201-8a6e-53275d32907b)(label(*))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 26))(sort Exp))((shape(Concave \
         26))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         40057dfc-a234-42c1-b4fc-3d6ed88bbca2)(content(Whitespace\" \
         \"))))(Tile((id \
         a83bb3f0-50e3-4097-b762-009d181a24b1)(label(2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         6d676dc1-5f01-4324-a9ab-61fb7a7360db)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         723b88a1-d5fd-4183-96f0-135d5c74d612)(content(Whitespace\"\\n\"))))(Secondary((id \
         35189c31-4c67-4b7a-8665-fea962f1d88a)(content(Whitespace\" \
         \"))))(Secondary((id \
         cde2953d-a210-4d5c-8223-1dddf22ae3bd)(content(Whitespace\" \
         \"))))(Secondary((id \
         e3063637-c300-480a-bf79-ab2390848091)(content(Comment\"# Note how the \
         hats have changed. Finally, select `24` below, #\"))))(Secondary((id \
         120b7316-fe3b-47d6-bc4c-ec0f6405d9a8)(content(Whitespace\"\\n\"))))(Secondary((id \
         7ca7414d-37e5-4d70-8935-c2376afbdcfa)(content(Whitespace\" \
         \"))))(Secondary((id \
         d023eae8-85d5-41f9-bd09-21376d842f92)(content(Whitespace\" \
         \"))))(Secondary((id \
         aabf716d-bb7d-42eb-8787-88173c1f84bb)(content(Comment\"# and then \
         again select 12, 10, 32, and 9 in turn. Notice how \
         #\"))))(Secondary((id \
         13c7441e-e0a5-47aa-bdcf-8d4701eb8661)(content(Whitespace\"\\n\"))))(Secondary((id \
         80ab3b84-f400-40ce-a6f3-88bb6b7a47de)(content(Whitespace\" \
         \"))))(Secondary((id \
         a780eadb-ece8-4c3d-bcda-4066216d4e2b)(content(Whitespace\" \
         \"))))(Secondary((id \
         f68b74a0-7ef8-470e-9943-2c4fc04c9677)(content(Comment\"# the solid \
         purple call indicators have internal shadows #\"))))(Secondary((id \
         ebfcee09-479b-4d82-9d41-3d20462b791c)(content(Whitespace\"\\n\"))))(Secondary((id \
         ffa7d679-3462-4bb8-ab7f-07e6d128c384)(content(Whitespace\" \
         \"))))(Secondary((id \
         c134b5f2-3887-4e62-bff1-adcadb88a04f)(content(Whitespace\" \
         \"))))(Secondary((id \
         3a4e5873-d74b-4fd8-8e82-3e6d77625a6a)(content(Comment\"# representing \
         their depth relative to the selected cell #\"))))(Secondary((id \
         a64270f0-06cc-4c0d-957d-c94df50ff3ad)(content(Whitespace\"\\n\"))))(Projector((id \
         afd54498-a35f-445f-88c4-80136e283180)(kind Probe)(syntax(Tile((id \
         eca0949b-ca2c-4a27-8dc4-c34c21f1c45c)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         e4266d85-5caf-4145-b1b4-9c13fc335023)(label(first))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         8791603d-0fbc-4d7a-a5e3-cf393ea4c48e)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 22))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         9a7917e1-b042-43e9-b4bf-66256572f774)(label(5))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))))))))(model\"((display_lengths())(max_closures \
         30)(index_offset 0))\")))(Secondary((id \
         00fac901-0192-4243-8928-d168f8a97f04)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         00005e62-87c3-4939-9da0-0bd8ec9db715)(content(Whitespace\"\\n\"))))(Secondary((id \
         fe62b9be-85a3-46a9-ac2b-384f5e658904)(content(Whitespace\"\\n\"))))(Secondary((id \
         5fe37a94-c76a-4fbd-8d69-ed1bd6d09e8d)(content(Comment\"# RECURSION \
         #\"))))(Secondary((id \
         04cd267d-e8c8-41dd-8b2f-42d37dccea6f)(content(Whitespace\"\\n\"))))(Secondary((id \
         8bf6ca27-9c76-46eb-a65d-3a649fbd3bfc)(content(Comment\"# Note how \
         cells are lowered/raised to indicate their #\"))))(Secondary((id \
         9528eb1c-c58e-4b7f-a5c6-0f5cd28566c6)(content(Whitespace\"\\n\"))))(Secondary((id \
         39bacb07-a182-4aab-8d80-913b794e1a6f)(content(Comment\"# relative \
         call stack depth to the selected cell #\"))))(Secondary((id \
         ecbbe10a-049b-434b-af52-a28ba7987937)(content(Whitespace\"\\n\"))))(Tile((id \
         2505da9f-2555-4da3-b47c-7d591a391912)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         40))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         9b5f9804-e5b4-4d91-b245-985f08834c78)(content(Whitespace\" \
         \"))))(Tile((id \
         38df695c-fd05-4754-93eb-16858d345410)(label(fact))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         29168887-f957-4e32-a0d0-c2a0b9bd7ee6)(content(Whitespace\" \
         \")))))((Secondary((id \
         4c8ddfd2-4300-41e3-9f0e-39e34c148538)(content(Whitespace\" \
         \"))))(Tile((id 5400a2c1-4102-4353-b4b1-10f6cb306e46)(label(fun \
         ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 35))(sort Exp))))))(shards(0 \
         1))(children(((Secondary((id \
         bd0a37b0-3ff1-4ad9-a026-bef9b5ae54a3)(content(Whitespace\" \
         \"))))(Projector((id 2cc652d6-37f0-4d4c-a95d-cad439992a00)(kind \
         Probe)(syntax(Tile((id \
         8006ec29-e5fb-441e-8567-ecec8bee9b87)(label(\"(\"\")\"))(mold((out \
         Pat)(in_(Pat))(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0 1))(children(((Tile((id \
         9f25cec4-d9e8-4d06-b269-c64340cd73ee)(label(x))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))))))))(model\"((display_lengths())(max_closures \
         30)(index_offset 0))\")))(Secondary((id \
         ae2a3f6d-5148-4e4a-9938-278b4cc0c78b)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         55e7155e-0e33-4e44-a866-4cfdf74020d4)(content(Whitespace\"\\n\"))))(Tile((id \
         cd77c519-ca23-49ad-8d40-ad26add9e55a)(label(case end))(mold((out \
         Exp)(in_(Rul))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         8d53166d-8270-4aa0-97d3-1847b248e732)(content(Whitespace\" \
         \"))))(Projector((id a9cd28dc-e808-4b15-aa78-a5ed359dc306)(kind \
         Probe)(syntax(Tile((id \
         1a8ea0bc-af5b-489f-b3e4-fcd7e37dd846)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         dfb8ca0e-73ee-45a9-89b7-8edd6432e055)(label(x))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))))))))(model\"((display_lengths())(max_closures \
         30)(index_offset 0))\")))(Secondary((id \
         f1b6a4bb-f605-468c-a4ba-324e4e77dbeb)(content(Whitespace\"\\n\"))))(Tile((id \
         3d8fc8c4-628e-4ac5-8eb4-e16656705c7f)(label(| =>))(mold((out \
         Rul)(in_(Pat))(nibs(((shape(Concave 41))(sort Exp))((shape(Concave \
         41))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         21e7a125-ca64-49c5-80de-e2cb951dc4fe)(content(Whitespace\" \
         \"))))(Tile((id \
         ec71eda5-8514-4fc7-ae40-ec5d62f3fa48)(label(1))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         2c71eb29-91da-4f06-a624-03d30a070c56)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         3515a3e1-4b5f-4fa7-b43c-bfb0e9d6e940)(content(Whitespace\" \
         \"))))(Projector((id 721328e9-a135-4408-9a42-32bc50efc9ff)(kind \
         Probe)(syntax(Tile((id \
         2088cee2-1ecf-4252-b1ea-c1b64ce204a3)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         3183b8ba-4d9d-4306-adc1-86e2f3acc917)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))))))))(model\"((display_lengths())(max_closures \
         30)(index_offset 0))\")))(Secondary((id \
         ce1cb9d0-6a34-4eb8-8caf-6914861a942a)(content(Whitespace\"\\n\"))))(Tile((id \
         b930066f-6124-407b-b3a4-8e019f3a4e52)(label(| =>))(mold((out \
         Rul)(in_(Pat))(nibs(((shape(Concave 41))(sort Exp))((shape(Concave \
         41))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         0e77e21b-e4c6-410d-a34a-114bd89c28bb)(content(Whitespace\" \
         \"))))(Tile((id \
         d25310f2-5b12-40aa-8568-d869879b4876)(label(_))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         acd68fa2-4f8c-43f3-b530-09f509a3d75c)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         476fa274-af62-4a02-bd53-56d27840df15)(content(Whitespace\"\\n\"))))(Tile((id \
         af694c9e-b296-43c1-95e5-22b6ca4375ac)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         38))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         7dc1c199-f29c-4de8-9e6e-d6c75dda4b23)(content(Whitespace\" \
         \"))))(Tile((id \
         5c25a6dd-4596-46b6-8369-ee87b5133e2a)(label(r))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         5535e0b7-9e48-4bb0-b566-20730e573009)(content(Whitespace\" \
         \")))))((Secondary((id \
         eda1e3c5-ed75-4814-8e74-8cc60f27f2dc)(content(Whitespace\" \
         \"))))(Projector((id 1c05bb0a-7a5a-417b-9154-b637c8abbdc6)(kind \
         Probe)(syntax(Tile((id \
         4393ca39-a680-42a0-971b-666c77ef17b1)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         489edcc6-5fe2-4f14-b146-3dc99362db99)(label(fact))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         cc539a8c-6460-4c18-bf22-4e662addd664)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 22))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         b933424f-8efd-4fef-a9cd-2b0e07724723)(label(x))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         e064f726-879b-43e9-8e35-b8cedb225d83)(label(-))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 27))(sort Exp))((shape(Concave \
         27))(sort Exp))))))(shards(0))(children())))(Tile((id \
         b9fadc88-65f6-4b9d-8764-1711769157ad)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))))))))(model\"((display_lengths())(max_closures \
         30)(index_offset 0))\")))(Secondary((id \
         4b29738c-7e36-46e8-9f2c-8adc994f0f18)(content(Whitespace\" \
         \"))))(Secondary((id \
         f8f20c5c-9d8b-4186-aec9-a5913b0da5b2)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         1c89c788-d9ce-4e11-b1f1-e9e489241ab4)(content(Whitespace\" \
         \"))))(Projector((id 008054fa-cc03-4d4c-b846-57bf094e9a14)(kind \
         Probe)(syntax(Tile((id \
         89ea6974-f42c-4032-99d3-d2543d76d30a)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         cd056503-df38-4ffc-8b94-f9a2cc34560a)(label(x))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         24eaa639-416a-4d9f-81b1-829a697ab5a8)(label(*))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 26))(sort Exp))((shape(Concave \
         26))(sort Exp))))))(shards(0))(children())))(Tile((id \
         257e6249-dc1d-4fee-876b-e07e566e79ad)(label(r))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))))))))(model\"((display_lengths())(max_closures \
         30)(index_offset 0))\")))(Secondary((id \
         e38500e6-6c6d-4ec3-83ad-37ecec22403e)(content(Whitespace\" \
         \"))))(Secondary((id \
         994c2bab-f8c9-4eba-b9a3-f30288c46dc0)(content(Whitespace\" \
         \"))))(Secondary((id \
         c2fc9b29-2b05-468b-947f-f324a2806a65)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         c2d28f31-745f-42b0-a59a-314829a1f305)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         04959701-b644-461d-bdf9-44ba1b321995)(content(Whitespace\"\\n\"))))(Tile((id \
         dd566cf3-072d-44f7-b409-a98e14d243b2)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         0b2ce030-ec2a-4e50-9dc2-bb60707133da)(content(Whitespace\" \
         \"))))(Projector((id 8cd413b3-e1fe-4aa1-93e8-9884e3fb50fe)(kind \
         Probe)(syntax(Tile((id \
         e48333fc-96a3-4367-ae55-d77b2daf1b2a)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         4f0af5c2-7881-47c8-8910-86e4f305fbe0)(label(fact))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         67e11059-b987-46a5-a00c-977a82e2eb6c)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 22))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         c2788147-72b2-46f4-a7d3-8993902e0b6e)(label(5))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))))))))(model\"((display_lengths())(max_closures \
         30)(index_offset 0))\")))(Secondary((id \
         3f06aa7e-3903-416b-b853-8be87cb23fbc)(content(Whitespace\" \
         \"))))(Tile((id \
         955ebb2c-7f58-486e-98ee-764e77e35e22)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 30))(sort Exp))((shape(Concave \
         30))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         a398986a-2fb9-4784-82bb-cc18e9f8a3f4)(content(Whitespace\" \
         \"))))(Tile((id \
         0a16005c-df90-42a2-8020-13fd4d1923af)(label(120))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         be2a2c8f-c3c3-48a2-8961-a5f7c18f9e4c)(content(Whitespace\" \
         \")))))))))(Tile((id \
         f2d2eb53-4ea4-419b-b0d2-7966401b8054)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 38))(sort Exp))((shape(Concave \
         38))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         b086ee7c-544a-4466-a103-38531046cf99)(content(Whitespace\"\\n\"))))(Secondary((id \
         f54cdab2-b620-4351-864f-394cb0e2d713)(content(Whitespace\"\\n\"))))(Secondary((id \
         fe5a5e55-92f8-4acf-8de2-dfbad3e96aad)(content(Comment\"# TAIL \
         RECURSION #\"))))(Secondary((id \
         90aa4c36-8781-44af-a38a-a7769c9ad410)(content(Whitespace\"\\n\"))))(Tile((id \
         4d9a47c6-c29a-4735-b30b-80046d601ae1)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         40))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         6dff3c6c-022d-46cb-8012-74fcad4dc0f5)(content(Whitespace\" \
         \"))))(Tile((id \
         72665aa9-8bb3-4dff-82da-2e5db517012d)(label(fact1))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         aa193cee-0d6c-4674-a520-a70d1e9171b9)(content(Whitespace\" \
         \")))))((Secondary((id \
         3efcf050-5e64-4f6a-abf1-b0594f0fbf5b)(content(Whitespace\" \
         \"))))(Tile((id 6de8750c-a8ee-48e4-8946-8b32cca91aba)(label(let = \
         in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 38))(sort Exp))))))(shards(0 1 \
         2))(children(((Secondary((id \
         a3642b54-f116-4ba9-ac6c-168e0dc01772)(content(Whitespace\" \
         \"))))(Tile((id \
         aca0b22c-3a3d-46da-83b5-72cba06bb2d5)(label(go))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         8999edcf-51c2-402a-a415-8498ce416f76)(content(Whitespace\" \
         \")))))((Secondary((id \
         2d6157d4-f61f-4ecd-8f4a-b361176eb730)(content(Whitespace\"\\n\"))))(Tile((id \
         7c2b6e33-19d6-4e28-9933-86ec7b053d26)(label(fun ->))(mold((out \
         Exp)(in_(Pat))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         9c1c6cb0-1b91-4a82-a1f7-93076cd9cb27)(content(Whitespace\" \
         \"))))(Tile((id \
         023df549-b3c1-40f6-9ea9-925993441221)(label(\"(\"\")\"))(mold((out \
         Pat)(in_(Pat))(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0 1))(children(((Projector((id \
         5f59e5e6-965d-4a25-b71f-0461e78b9824)(kind Probe)(syntax(Tile((id \
         5328da8f-f7f0-4bdf-b4b0-318fb00dec8e)(label(\"(\"\")\"))(mold((out \
         Pat)(in_(Pat))(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0 1))(children(((Tile((id \
         ee231581-c736-45e8-a2ce-e57653deb72a)(label(x))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))))))))(model\"((display_lengths())(max_closures \
         30)(index_offset 0))\")))(Tile((id \
         df4eee26-b773-4e17-93bc-a50b6ee72ba3)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 45))(sort Pat))((shape(Concave \
         45))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         bfc013fa-0ce6-4e7e-99a9-449c868213d7)(content(Whitespace\"\\n\"))))(Secondary((id \
         424e73cc-45fe-4ad4-b958-54732169a280)(content(Whitespace\" \
         \"))))(Projector((id 9cf4c676-6889-41d5-8c10-1e64000ca6c0)(kind \
         Probe)(syntax(Tile((id \
         b78bb978-0200-48dd-8e2d-393e4e4b6732)(label(\"(\"\")\"))(mold((out \
         Pat)(in_(Pat))(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0 1))(children(((Tile((id \
         5c018ca5-5258-4f71-8ff6-1558e7e849cb)(label(acc))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))))))))(model\"((display_lengths())(max_closures \
         30)(index_offset 0))\"))))))))(Secondary((id \
         dcaa4ca5-781a-4707-b79e-0ffc18f6cac1)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         83fc8e5e-0f9c-4fef-9f95-451f315e1b30)(content(Whitespace\"\\n\"))))(Tile((id \
         1212ca9e-bd0a-4cf1-91a3-65c944b1e7be)(label(case end))(mold((out \
         Exp)(in_(Rul))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         b237ad6a-5298-4f62-bc9a-444ef8617308)(content(Whitespace\" \
         \"))))(Projector((id 229d206a-fcd2-42d5-8032-29742e4354aa)(kind \
         Probe)(syntax(Tile((id \
         c4d7c664-4f17-49ee-bfa3-85d74d33f2b6)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         85f8a6ff-4d95-481b-9eec-41dfc5800f03)(label(x))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))))))))(model\"((display_lengths())(max_closures \
         30)(index_offset 0))\")))(Secondary((id \
         868bc9e4-e311-4465-85c3-d83397d66bfa)(content(Whitespace\"\\n\"))))(Tile((id \
         64e2e80a-b2fa-4184-a916-2d1c44dcd8ac)(label(| =>))(mold((out \
         Rul)(in_(Pat))(nibs(((shape(Concave 41))(sort Exp))((shape(Concave \
         41))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         393713cb-152c-4563-8498-f866d5af7482)(content(Whitespace\" \
         \"))))(Tile((id \
         77deb701-fe6a-474a-bb48-bfacc2ad0a8a)(label(1))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         63f83f55-c54c-4612-8d21-a88658c00ce9)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         59165718-944e-46c1-9d4f-2cb131b59978)(content(Whitespace\" \
         \"))))(Projector((id adcc5c96-39ac-43a5-a9df-f5c736a0b71a)(kind \
         Probe)(syntax(Tile((id \
         847bc2ad-075b-49df-b670-4500cfe9c5df)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         dc9002eb-4238-4003-a36c-56c6eff1b308)(label(acc))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))))))))(model\"((display_lengths())(max_closures \
         30)(index_offset 0))\")))(Secondary((id \
         a367b9c8-7cf1-477c-a64b-fcb440c3e2cc)(content(Whitespace\"\\n\"))))(Tile((id \
         41860cc0-9d64-4947-9c7a-82f1b7b6c6a1)(label(| =>))(mold((out \
         Rul)(in_(Pat))(nibs(((shape(Concave 41))(sort Exp))((shape(Concave \
         41))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         731c4543-5ed1-4449-9099-ddae13363896)(content(Whitespace\" \
         \"))))(Tile((id \
         461f2ef9-6445-4378-a90e-aca6965a45e5)(label(_))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         8707f4d4-45d6-4f20-af4d-d40296a9783b)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         424a9dac-ee40-45ae-86d3-08b11e249180)(content(Whitespace\"\\n\"))))(Tile((id \
         05363266-c259-4049-b01d-b9669f586500)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         38))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         e32b8334-db8d-48f2-8fc3-2bdf387de6ce)(content(Whitespace\" \
         \"))))(Tile((id \
         808b8f37-97ea-4f27-90d2-c74f6a216284)(label(r))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         29837474-4f86-408d-a69b-049465c791b3)(content(Whitespace\" \
         \")))))((Secondary((id \
         3d6faf27-94b5-454f-8f14-327462bb5031)(content(Whitespace\" \
         \"))))(Projector((id 95e5f9d8-3a30-4162-85d0-6ca37d563d4d)(kind \
         Probe)(syntax(Tile((id \
         1d743fd8-4e1d-495e-83d8-a2663f8adfa7)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         59010602-89e8-4d3b-bae8-ac9799f38f40)(label(x))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         e5b247b8-2681-4a49-bd2f-4e4e02a594be)(label(*))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 26))(sort Exp))((shape(Concave \
         26))(sort Exp))))))(shards(0))(children())))(Tile((id \
         f15cba88-dba5-401f-9060-dc5946c7a3ab)(label(acc))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))))))))(model\"((display_lengths())(max_closures \
         30)(index_offset 0))\")))(Secondary((id \
         1394f4f1-915b-4af7-ac60-e359c7bbeddc)(content(Whitespace\" \
         \"))))(Secondary((id \
         bf14021f-505f-4c46-b2f1-eeed7450fc12)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         7d0c868d-f0dc-4b09-93ab-edf8ef3b01e4)(content(Whitespace\" \
         \"))))(Projector((id ee447f63-8ae5-41f2-96b5-7580ff2f3f45)(kind \
         Probe)(syntax(Tile((id \
         8545da15-6072-469b-85a0-e20f6bf18bbd)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         5b64ec43-a089-4c2a-9878-1dd6c0ecda78)(label(go))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         1a63dc73-2fc6-4b75-9050-40ee63c55d75)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 22))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         f75ebd55-6733-47c4-a0a8-2b836c472030)(label(x))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         05ee64fc-72e7-48e5-8226-629b1f6ce7dd)(label(-))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 27))(sort Exp))((shape(Concave \
         27))(sort Exp))))))(shards(0))(children())))(Tile((id \
         3db8f819-e8f4-40b3-b0d7-2f0c0e57ba48)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         80a1a203-4a4a-4c3c-aa6a-f16325d661a7)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 45))(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         27b79049-874d-4d14-a858-79f240aca5a3)(content(Whitespace\" \
         \"))))(Tile((id \
         0efb80c4-29b7-48fa-8bf5-4fd8bf4a9e79)(label(r))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))))))))(model\"((display_lengths())(max_closures \
         30)(index_offset 0))\")))(Secondary((id \
         dd4bad0a-7180-4541-8606-0431eb5554d9)(content(Whitespace\" \
         \"))))(Secondary((id \
         df825035-c083-4171-9719-ff8d97e22b68)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         21a2b633-843f-4db3-a502-cb5900bdd7ee)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         27ae0198-5ba9-4abc-81af-cb8831e6decd)(content(Whitespace\"\\n\"))))(Tile((id \
         5ce76874-5a03-4db6-be74-f52663e657b6)(label(fun ->))(mold((out \
         Exp)(in_(Pat))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         cc7d1623-0d4e-41b5-8a7c-0ad734e9c917)(content(Whitespace\" \
         \"))))(Tile((id \
         92a98e14-7060-4b0a-a62c-e29c45831d47)(label(x))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         e4a4c9ff-4777-479b-b9c0-1cd9bc2db454)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         4eccf997-02fd-44d2-aec0-5edb124c6811)(content(Whitespace\" \
         \"))))(Projector((id 84967f13-9ece-465c-9968-eeee993b950b)(kind \
         Probe)(syntax(Tile((id \
         787cf07e-2749-4d10-88cd-52f3918fb2ff)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         f53e08cc-e6db-407d-9c23-c5be9c75a21c)(label(go))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         85d0281b-8c85-49f7-99b9-8789ebb35431)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 22))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         bd7e6244-a67e-4c14-8200-d104d69a0460)(label(x))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         8b189401-f72d-402e-9bc9-03e443606af5)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 45))(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0))(children())))(Tile((id \
         668f66d5-76e2-4498-8a78-7ee96cf8a86f)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))))))))(model\"((display_lengths())(max_closures \
         30)(index_offset 0))\")))(Secondary((id \
         540669b2-ab73-4d69-8766-2656fb0c69b2)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         92730920-fb33-440d-9130-c4f58b20eea6)(content(Whitespace\"\\n\"))))(Tile((id \
         0b2b56c7-43ea-4ed9-bf46-82412d9b90f0)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         849ae3ac-2bc6-43c6-8bc8-aeb5bf48c697)(content(Whitespace\" \
         \"))))(Projector((id 7ed6efb2-f151-4077-943a-d06ed5850ba3)(kind \
         Probe)(syntax(Tile((id \
         9845f62f-944e-42d5-a3b3-871a5177c97f)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         3623dbad-7c19-4e1c-bd6a-659975587d91)(label(fact1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         b6f05753-7d62-4c0b-8a9a-8421dbdacb50)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 22))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         d8ce3c87-0b75-4eb9-9af7-832acdcb1590)(label(5))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))))))))(model\"((display_lengths())(max_closures \
         30)(index_offset 0))\")))(Secondary((id \
         3353f721-bb87-4751-aa66-2b924e03ebd9)(content(Whitespace\" \
         \"))))(Tile((id \
         2795af4a-5b46-45cd-a427-2483d92dfa7f)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 30))(sort Exp))((shape(Concave \
         30))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         65637f95-dca3-41f8-a436-19e0e2f4fde9)(content(Whitespace\" \
         \"))))(Tile((id \
         ff1ec884-714d-4082-9b90-31805d8469e2)(label(120))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         436b5f77-0f8d-40f5-95f9-8203def5a7c5)(content(Whitespace\" \
         \")))))))))(Tile((id \
         11387160-c431-4fff-bd3d-1f217a289e3a)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 38))(sort Exp))((shape(Concave \
         38))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         41446a60-32db-468f-90dc-c3835d7d0c6e)(content(Whitespace\"\\n\"))))(Secondary((id \
         f4ef9636-9fa1-4fc1-bb82-0be16b37525c)(content(Whitespace\"\\n\"))))(Secondary((id \
         c07a0420-d88d-4aae-829b-b46fec8139ab)(content(Comment\"# FUNCTIONS IN \
         FUNCTIONS #\"))))(Secondary((id \
         33beb186-a077-4357-90b3-447fef92fcbc)(content(Whitespace\"\\n\"))))(Secondary((id \
         e3ea07ad-1c16-4362-b02c-1a25ec2ec4ef)(content(Comment\"# The frunk \
         factory prethunks your frunk for later clunking #\"))))(Secondary((id \
         83d1f1ed-2c11-497a-91c8-ea3f81699fd8)(content(Whitespace\"\\n\"))))(Tile((id \
         df97dbcf-3d88-4e56-aa72-b609b84b418b)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         40))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         0066d48e-28df-4a51-81c7-d4a3c62ca708)(content(Whitespace\" \
         \"))))(Tile((id \
         7acf09f0-16b5-4773-a88e-e82430b8404d)(label(frunk_factory))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         dac42ced-8006-4715-bcd2-b56d66a71982)(content(Whitespace\" \
         \")))))((Secondary((id \
         3c8ec44c-07f9-4a1d-98f0-84d565a912a1)(content(Whitespace\" \
         \"))))(Tile((id 9fd69cb2-dfcf-48c9-8bc3-c27e59a55e76)(label(fun \
         ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 35))(sort Exp))))))(shards(0 \
         1))(children(((Secondary((id \
         6c6377b8-8498-4350-9358-b98d540b64e7)(content(Whitespace\" \
         \"))))(Projector((id e04821c8-7aaf-46ae-b7c1-6d98b8c4f2ea)(kind \
         Probe)(syntax(Tile((id \
         e04821c8-7aaf-46ae-b7c1-6d98b8c4f2ea)(label(\"(\"\")\"))(mold((out \
         Pat)(in_(Pat))(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0 1))(children(((Tile((id \
         7c87b018-164d-4d7b-8cbd-aa0c0438e934)(label(y))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))))))))(model\"((display_lengths())(max_closures \
         30)(index_offset 0))\")))(Secondary((id \
         60fac40b-15e2-444d-aa8b-99420b6bbbbb)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         20a4e3f1-fc3e-4bd5-b91c-fbb9df67f6be)(content(Whitespace\"\\n\"))))(Secondary((id \
         c49d7de4-825d-4ca7-8a03-7c85b8266424)(content(Comment\"# This is a \
         play area to explore nested function definitions \
         #\"))))(Secondary((id \
         1a4f54f9-9a6c-4a75-ae30-b950844943c3)(content(Whitespace\"\\n\"))))(Secondary((id \
         d0c95f5c-263e-4ffa-bce2-b965ff90808e)(content(Comment\"# and \
         functions returning functions #\"))))(Secondary((id \
         9eaa151a-60f5-453c-8b30-33e23a5d7a9c)(content(Whitespace\"\\n\"))))(Tile((id \
         e9876de5-7151-4832-ae49-9b7b256d52b5)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         38))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         4155ad27-9e53-4e0c-b53a-8e3d46334075)(content(Whitespace\" \
         \"))))(Tile((id \
         3300fd9d-823a-4b3c-9057-8268a1a0950c)(label(factor))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         a140d50b-1066-4c04-b144-84b3b42c7703)(content(Whitespace\" \
         \")))))((Secondary((id \
         dcff2db3-3498-430e-b1bf-764b13d9782c)(content(Whitespace\"\\n\"))))(Tile((id \
         69bacf45-e3af-4ce4-bf93-bece232aa4e1)(label(4))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         184c2ab0-a6b9-4384-89c0-808ca82dabb3)(content(Whitespace\" \
         \"))))(Tile((id \
         d2a7b02a-dbc1-418e-8b7e-3ed8d3681dd2)(label(+))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 27))(sort Exp))((shape(Concave \
         27))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         72f42493-f23c-492b-8673-e5282124cdf2)(content(Whitespace\" \
         \"))))(Projector((id 35392134-604a-4f47-bb72-391ddee07503)(kind \
         Probe)(syntax(Tile((id \
         35392134-604a-4f47-bb72-391ddee07503)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         4f1053b8-6f6c-4571-88f1-c374386220a7)(label(10))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         dfca029e-3c5a-4d15-9650-336a7f341d65)(content(Whitespace\" \
         \"))))(Tile((id \
         d4e23626-31c3-40a6-bfd8-f98fad5914d2)(label(*))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 26))(sort Exp))((shape(Concave \
         26))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         f47d4a61-5f9d-4b7c-a521-3d553d7d6892)(content(Whitespace\" \
         \"))))(Tile((id \
         a33f6c6b-9c1c-4bed-92f5-7d4ee57df77c)(label(y))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))))))))(model\"((display_lengths())(max_closures \
         30)(index_offset 0))\")))(Secondary((id \
         2571ba3d-e404-493f-b440-cdb4dbef2f99)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         8803d05a-ea0f-47c0-a88e-8cd23ca4cb2e)(content(Whitespace\"\\n\"))))(Tile((id \
         b850cd92-ec03-4d0f-86f8-3671087da592)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         38))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         ec71f771-597f-402f-b27e-c671d909e8a4)(content(Whitespace\" \
         \"))))(Tile((id \
         020454c5-3ec0-45b3-9036-b7eed87853ea)(label(refactor))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         ad364cc7-6975-4e91-8541-691a8e577a40)(content(Whitespace\" \
         \")))))((Secondary((id \
         8213c38d-5bf2-4125-9f87-0d675c97ee15)(content(Whitespace\" \
         \"))))(Tile((id 78a01d74-d826-47de-bb19-8322c35ad09c)(label(fun \
         ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 35))(sort Exp))))))(shards(0 \
         1))(children(((Secondary((id \
         5f16e4cb-af6f-4ba8-9f1c-a7f3f2c8edac)(content(Whitespace\" \
         \"))))(Projector((id bb4dee70-8f16-438a-b3ac-a1bdfd525b4f)(kind \
         Probe)(syntax(Tile((id \
         bb4dee70-8f16-438a-b3ac-a1bdfd525b4f)(label(\"(\"\")\"))(mold((out \
         Pat)(in_(Pat))(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0 1))(children(((Tile((id \
         b30d8c91-daae-476d-8cb6-78c25e8ab814)(label(x))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))))))))(model\"((display_lengths())(max_closures \
         30)(index_offset 0))\")))(Secondary((id \
         0a1aadc7-41c9-4a5d-a55c-de5bd5690631)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         3f7982b2-29ee-4e3d-aa64-d29afa8e6f42)(content(Whitespace\"\\n\"))))(Projector((id \
         773535ab-4668-4029-8beb-67c07e48a516)(kind Probe)(syntax(Tile((id \
         773535ab-4668-4029-8beb-67c07e48a516)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         0b92a411-6daa-4dc3-9ba0-90614c661b52)(label(x))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         88600920-58c2-4675-92bd-fb64a5099c1d)(content(Whitespace\" \
         \"))))(Tile((id \
         682da38e-c392-4c49-aac8-4e77f5d6df42)(label(+))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 27))(sort Exp))((shape(Concave \
         27))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         26535ab1-7ab2-4520-a2b1-4e3e5c2b516d)(content(Whitespace\" \
         \"))))(Tile((id \
         62e7eac7-739b-404f-89d6-6f3b21c2ad0b)(label(factor))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))))))))(model\"((display_lengths())(max_closures \
         30)(index_offset 0))\")))(Secondary((id \
         c2205064-0175-403f-95a6-32f6f0d6cc43)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         eed091b7-8a92-4e3b-afaf-94d9bc5dbb21)(content(Whitespace\"\\n\"))))(Tile((id \
         1cbded78-d36a-4e8b-9930-5a163bcc5110)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         38))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         265bbf50-bc67-4952-ab09-661343be6f3d)(content(Whitespace\" \
         \"))))(Tile((id \
         ac3c5077-e3dc-417a-9bf1-f3363a68b3ab)(label(factor))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         0fb202e1-bbdf-4983-a3f1-f9a8d77a5b01)(content(Whitespace\" \
         \")))))((Secondary((id \
         5d52a107-3b97-4abb-9b65-25bfda7389cf)(content(Whitespace\"\\n\"))))(Projector((id \
         c170ed7b-d043-4c3c-8d47-769369e85bbb)(kind Probe)(syntax(Tile((id \
         c170ed7b-d043-4c3c-8d47-769369e85bbb)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         988470e5-aa88-4d42-a080-9cbb10ea97cf)(label(refactor))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         4385e9e9-4d3f-433c-af1d-2b49b609828e)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 22))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         9b779ff7-5bb2-4907-a0ae-572c11158488)(label(factor))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))))))))(model\"((display_lengths())(max_closures \
         30)(index_offset 0))\")))(Secondary((id \
         41a24b74-e825-4cba-9c2b-3297d2754b3e)(content(Whitespace\"\\n\"))))(Tile((id \
         4ed8208c-5705-44cc-a3d1-6b234d593081)(label(-))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 27))(sort Exp))((shape(Concave \
         27))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         9554defe-676c-46aa-9abb-b2349c6f63ae)(content(Whitespace\" \
         \"))))(Projector((id cca8f8b6-3558-4301-9933-8f20be52384f)(kind \
         Probe)(syntax(Tile((id \
         cca8f8b6-3558-4301-9933-8f20be52384f)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         d505125b-12a8-48a7-a199-e455a9076ff3)(label(refactor))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         452ef41d-20f9-4e7c-90d3-b5390eb50745)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 22))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         eb747de7-52d1-450a-a304-2f609200c3d4)(label(y))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))))))))(model\"((display_lengths())(max_closures \
         30)(index_offset 0))\")))(Secondary((id \
         b66d2b9e-a5de-4f0b-80a6-25e46d1f4891)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         f12d36ee-1f93-41ea-9df4-69c1f81bc8ff)(content(Whitespace\" \
         \"))))(Secondary((id \
         d19cd614-bf22-4378-a2b6-8e2be4981c47)(content(Whitespace\"\\n\"))))(Tile((id \
         0c412625-238d-4cc2-b8b6-a1d51c646b8c)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         38))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         3abc29a6-3ca7-448b-966b-f40642b1c36a)(content(Whitespace\" \
         \"))))(Tile((id \
         70c0654d-d849-4595-9341-463f65fc7129)(label(perturb))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         ffe6ef1d-4e9b-413b-a733-87cc062095bc)(content(Whitespace\" \
         \")))))((Secondary((id \
         b7c40e2e-86a8-48e3-914a-d233977970c6)(content(Whitespace\" \
         \"))))(Tile((id 304fa848-aa91-43c4-a500-6c7c08b83851)(label(fun \
         ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 35))(sort Exp))))))(shards(0 \
         1))(children(((Secondary((id \
         617c41a3-bf68-436c-bdb9-cb6055261873)(content(Whitespace\" \
         \"))))(Projector((id 74120fc7-146e-4051-8301-5fa95b7caac5)(kind \
         Probe)(syntax(Tile((id \
         74120fc7-146e-4051-8301-5fa95b7caac5)(label(\"(\"\")\"))(mold((out \
         Pat)(in_(Pat))(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0 1))(children(((Tile((id \
         19dd0756-6ed5-44d1-8a45-4ef2f750f266)(label(s))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))))))))(model\"((display_lengths())(max_closures \
         30)(index_offset 0))\")))(Secondary((id \
         dc4cbfaa-884d-41ed-9157-cb329b82ac19)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         752daba9-bcd3-4c8b-a9fa-84f0043fb677)(content(Whitespace\"\\n\"))))(Tile((id \
         b83a25e0-3f10-4678-a202-d755bdbb07da)(label(factor))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         82e94bfe-b77c-42da-b8a7-0f9d1f9d66d9)(content(Whitespace\" \
         \"))))(Tile((id \
         ec303f6e-8890-47f4-a530-b499de1145a8)(label(+))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 27))(sort Exp))((shape(Concave \
         27))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         0d8fae96-8cfe-43c1-ac6e-ab2a6f1418e1)(content(Whitespace\" \
         \"))))(Projector((id 299340b3-2d74-4f20-ac07-15ce0f5d90c9)(kind \
         Probe)(syntax(Tile((id \
         299340b3-2d74-4f20-ac07-15ce0f5d90c9)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         a5084819-7607-48b2-a8b1-ca4bd9d349b9)(label(refactor))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         b2ab6ab6-3fe1-492e-a381-ccce27ec7a0b)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 22))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         0149b2e9-bd81-4ae3-a389-f0c2535a59fd)(label(s))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))))))))(model\"((display_lengths())(max_closures \
         30)(index_offset 0))\")))(Secondary((id \
         afa7c833-9a0f-4ce5-8515-6086919f464b)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         88a2d3ce-e1ad-40fa-8c6c-52f7a3fb5535)(content(Whitespace\"\\n\"))))(Tile((id \
         c01f7999-7509-4ddf-9ab6-9d4c0141bda2)(label(fun ->))(mold((out \
         Exp)(in_(Pat))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         9e3b320b-f7a1-4c6f-b16e-dc02f697dd13)(content(Whitespace\" \
         \"))))(Projector((id 85ba9e6f-967e-4bc0-b0c5-2a6fdff35f3f)(kind \
         Probe)(syntax(Tile((id \
         85ba9e6f-967e-4bc0-b0c5-2a6fdff35f3f)(label(\"(\"\")\"))(mold((out \
         Pat)(in_(Pat))(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0 1))(children(((Tile((id \
         f3959848-a593-4418-a1d4-f282f27dde4d)(label(z))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))))))))(model\"((display_lengths())(max_closures \
         30)(index_offset 0))\")))(Secondary((id \
         e631e6ee-09e3-42bf-8171-a7306b48ef0b)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         1691d277-0104-44c8-9257-4679a642ce02)(content(Whitespace\"\\n\"))))(Projector((id \
         f68263b7-0296-4765-baf1-e687baf1ba5b)(kind Probe)(syntax(Tile((id \
         f68263b7-0296-4765-baf1-e687baf1ba5b)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         680ee9d5-4bcc-48a8-bbd2-a4c42b4432d7)(label(perturb))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         3ad55655-e2fd-4c4f-becb-fbb3d5065fe2)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 22))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         604e0350-2d62-4c29-8b4c-a6b0e3b0c74f)(label(3))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         6d3bd2f3-395d-476e-96d1-34cbcb3c481f)(label(*))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 26))(sort Exp))((shape(Concave \
         26))(sort Exp))))))(shards(0))(children())))(Tile((id \
         63bc73df-7c90-4aed-91fe-7c48063ed9f2)(label(z))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))))))))(model\"((display_lengths())(max_closures \
         30)(index_offset 0))\")))(Secondary((id \
         a5cd56f2-9506-4e0e-848c-53f402831076)(content(Whitespace\"\\n\"))))(Tile((id \
         f62225d9-d449-4222-b6db-18022de8c011)(label(+))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 27))(sort Exp))((shape(Concave \
         27))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         48ec40aa-9f9f-4197-a8de-66bd72038d4e)(content(Whitespace\" \
         \"))))(Projector((id 87e1fe39-96b2-415e-9128-f7e439473a43)(kind \
         Probe)(syntax(Tile((id \
         87e1fe39-96b2-415e-9128-f7e439473a43)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         1051ba75-2517-459f-9fe8-30a545f7551a)(label(perturb))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         78ad2413-b654-4a46-8b5f-9814803ce898)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 22))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         c303f18c-5abd-498b-a3a9-4c0a802ac7f9)(label(5))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         15f14f7f-3475-4ce9-b7ea-b4c25b2ed735)(label(*))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 26))(sort Exp))((shape(Concave \
         26))(sort Exp))))))(shards(0))(children())))(Tile((id \
         4abf9109-2b98-4700-89fb-436caede8ba8)(label(z))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))))))))(model\"((display_lengths())(max_closures \
         30)(index_offset 0))\")))(Secondary((id \
         2a00ea48-b73a-465a-a563-7b9c56178a2d)(content(Whitespace\" \
         \"))))(Secondary((id \
         39cb887c-8d9a-4ce3-b20f-c219d36b14ea)(content(Whitespace\" \
         \"))))(Secondary((id \
         5e84868c-cbd8-4809-adb6-2dc23d0461ce)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         74b0b354-ce1b-4ad8-b780-2d1bd1e1c480)(content(Whitespace\" \
         \"))))(Tile((id daf91ae1-7997-4555-bfc0-f02222b2760d)(label(let = \
         in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 40))(sort Exp))))))(shards(0 1 \
         2))(children(((Secondary((id \
         a72d10ce-6718-4ce3-9e6c-7fdb58aa2c77)(content(Whitespace\" \
         \"))))(Tile((id \
         cf500943-b551-4095-bde8-b1a7259acd5a)(label(new_frunk))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         c8977099-47b2-47d6-b043-9bc8f37eee22)(content(Whitespace\" \
         \")))))((Secondary((id \
         2bc3c2dd-4124-412b-a86f-35278fe8f60f)(content(Whitespace\" \
         \"))))(Projector((id b2ed5a7f-70eb-4abc-a161-d845a94c2cf1)(kind \
         Probe)(syntax(Tile((id \
         b2ed5a7f-70eb-4abc-a161-d845a94c2cf1)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         94ccd5ba-af33-4494-a595-8d52e8cf0179)(label(frunk_factory))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         93038511-4f14-401e-aeb2-1158f9a85cb1)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 22))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         0f22b45f-5d7a-409a-9720-2fdfa0bf68e6)(label(7))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))))))))(model\"((display_lengths())(max_closures \
         30)(index_offset 0))\")))(Secondary((id \
         39b158b9-4096-4372-bc30-cd61b03aacf8)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         67f3cf4a-c60d-4d46-8d6a-f0a0d4f88534)(content(Whitespace\"\\n\"))))(Tile((id \
         bd13a88a-6db8-4add-92d2-dc383368c081)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         ae61e4a2-ecff-41d4-8741-fa2c5fc49bba)(content(Whitespace\" \
         \"))))(Projector((id 22119873-727b-4441-8926-60186c1f5d92)(kind \
         Probe)(syntax(Tile((id \
         22119873-727b-4441-8926-60186c1f5d92)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         19fcb858-544d-41f6-99e1-c00864afd785)(label(new_frunk))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         1d16d328-4844-42b8-bde7-1cca0b007efe)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 22))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         d8707851-00ce-4813-9c71-9649d0667252)(label(4))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))))))))(model\"((display_lengths())(max_closures \
         30)(index_offset 0))\")))(Secondary((id \
         ad51d67f-1f9b-4f26-b5e8-7b5a914b9aa1)(content(Whitespace\" \
         \"))))(Tile((id \
         95ab28d7-c9d3-4675-9c62-690afc98a64d)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 30))(sort Exp))((shape(Concave \
         30))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         9d468147-6337-4429-bf63-73482fc42a1c)(content(Whitespace\" \
         \"))))(Tile((id \
         813b420e-8973-45c7-b227-0eb37086bd2a)(label(314))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         f1996448-715e-4851-9e0e-f1d5e670e136)(content(Whitespace\" \
         \")))))))))(Tile((id \
         65076a03-35d1-471e-a4b5-ae001984d747)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 38))(sort Exp))((shape(Concave \
         38))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         96eb921f-3951-4f54-a78d-4b653c039923)(content(Whitespace\"\\n\"))))(Tile((id \
         a1a75396-8c19-4d31-ab75-cd6171a58f7e)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         e30d5ce0-bcb4-49fb-98ad-d1d377c91e32)(content(Whitespace\" \
         \"))))(Projector((id 8a36da61-45b4-41c1-aeae-d228064c976a)(kind \
         Probe)(syntax(Tile((id \
         8a36da61-45b4-41c1-aeae-d228064c976a)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         de084ca3-3dda-4583-900d-88e8e5e431bf)(label(new_frunk))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         7080cc6b-cd2d-4a32-8212-6aedf9262e57)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 22))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         52ca949f-ee4f-4793-b4c9-1d9e165d147d)(label(6))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))))))))(model\"((display_lengths())(max_closures \
         30)(index_offset 0))\")))(Secondary((id \
         cf9a08f8-1c42-4e7d-816a-d72ea1ee7b16)(content(Whitespace\" \
         \"))))(Tile((id \
         6fb8e11e-e4a2-426c-bdda-6728b40d5340)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 30))(sort Exp))((shape(Concave \
         30))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         a0b68d6b-9620-48ff-bc3e-e8aedb58b0dd)(content(Whitespace\" \
         \"))))(Tile((id \
         6297325c-203e-436e-a542-566a18764ef7)(label(330))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         0de741d7-2a91-4eea-b07a-3f62d178bd16)(content(Whitespace\" \
         \")))))))))(Tile((id \
         345a7428-7d3a-4fb3-b7eb-0d1d5c71caee)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 38))(sort Exp))((shape(Concave \
         38))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         41ddef95-c1b7-4887-97fb-cfa88e61e03d)(content(Whitespace\"\\n\"))))(Secondary((id \
         42d8b241-c133-4987-8a12-f66c74738a49)(content(Whitespace\"\\n\"))))(Secondary((id \
         6a01efcc-c114-4c2d-8ae3-442f4e7a572a)(content(Whitespace\"\\n\"))))(Tile((id \
         80424c33-ef77-4143-ba55-ed1f424d0835)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         40))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         0e5e3a3b-048a-444a-9d12-a3fcaec50b88)(content(Whitespace\" \
         \"))))(Tile((id \
         08f32a46-9a3a-4962-a69f-66b366e2cdb9)(label(_))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         d25b0333-ce4e-472f-9a25-f739f4d76f4d)(content(Whitespace\" \
         \")))))((Secondary((id \
         d7b64ba2-bfcc-4191-9683-b4769d216a00)(content(Whitespace\" \
         \"))))(Tile((id 3539b16d-ccd5-4b3f-8167-e5bc7a4b61a7)(label(let = \
         in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 38))(sort Exp))))))(shards(0 1 \
         2))(children(((Secondary((id \
         88273e1d-2be2-4f87-b387-3e4c59cf5fb8)(content(Whitespace\" \
         \"))))(Tile((id \
         8e063f2a-5b67-4640-9928-f7a09c2e5610)(label(fib))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         6c1e3ff3-1390-4fd9-929e-08ac6570eba1)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 23))(sort Pat))((shape(Concave \
         23))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         e768bd38-132f-4aee-a602-9b7044faa7fe)(content(Whitespace\" \
         \"))))(Tile((id \
         83864cf5-3389-478e-aa64-9a99b7dbd580)(label(Int))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         98c6af47-e0d3-411d-9f04-c933ddda607a)(content(Whitespace\" \
         \"))))(Tile((id \
         f9a9e724-d074-422d-9640-fea589622689)(label(->))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 13))(sort Typ))((shape(Concave \
         13))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         29c72309-2b3a-4fb4-8213-7d661722fbac)(content(Whitespace\" \
         \"))))(Tile((id \
         de18009c-0fd1-49eb-9a6c-f087ddb30c27)(label(Int))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         db352221-7488-479f-b2ac-999692d03af7)(content(Whitespace\" \
         \")))))((Secondary((id \
         b9fcd925-c2cd-4b71-994a-f2f2bcd091b8)(content(Whitespace\"\\n\"))))(Secondary((id \
         c9378ee0-bba3-44bd-b77a-59c396ece0ff)(content(Comment\"# Recursive \
         calls can complicate probe display due #\"))))(Secondary((id \
         b0ecda84-a1a5-4def-9e79-38a68989980f)(content(Whitespace\"\\n\"))))(Secondary((id \
         4ae76942-8ea0-4856-949d-83888d95ac27)(content(Comment\"# due to \
         overlapping information channels.#\"))))(Secondary((id \
         d75303d7-3b04-4607-a945-4b4eb612d038)(content(Whitespace\"\\n\"))))(Tile((id \
         789bee62-8c47-4b0a-ba2f-f87149475b5c)(label(fun ->))(mold((out \
         Exp)(in_(Pat))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         77b8816e-6875-48da-b18d-15acfb004460)(content(Whitespace\" \
         \"))))(Tile((id \
         a98381fa-f479-4ec3-8e4c-2b4c1c2c89d5)(label(x))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         4c23c6cb-9ede-41ae-8d52-9d098d48a50d)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         69b144c3-d054-497f-b595-be11dad3a7b3)(content(Whitespace\" \
         \"))))(Tile((id 9270024a-e155-4b40-854f-a3027b94f364)(label(case \
         end))(mold((out Exp)(in_(Rul))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         3ffc53c6-3c60-46ac-928d-c518ba4d3f02)(content(Whitespace\" \
         \"))))(Tile((id \
         7350c485-0c07-424b-9fb2-28761cd50685)(label(x))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         022bea1e-7e0f-4416-bc09-abc4432fb2fa)(content(Whitespace\"\\n\"))))(Tile((id \
         ee5e44e7-70e4-4e61-836b-6343630249a7)(label(| =>))(mold((out \
         Rul)(in_(Pat))(nibs(((shape(Concave 41))(sort Exp))((shape(Concave \
         41))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         9a36385c-91aa-45ac-8fdd-b28e3496bc0a)(content(Whitespace\" \
         \"))))(Tile((id \
         eca2e0dd-b929-49db-829b-d14107e081b6)(label(0))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         b355b1e9-840b-40be-9bc4-fe5e665920f2)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         c910daba-769e-4f32-b93c-9a598288470e)(content(Whitespace\" \
         \"))))(Tile((id \
         ee1a4bbd-0d3e-47fe-9540-76b82a1b4c40)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         168f5a94-b967-4aeb-aec3-8cc0ec88d6e6)(content(Whitespace\"\\n\"))))(Tile((id \
         ddc7ba06-36fd-490a-8c8b-5f42612df08e)(label(| =>))(mold((out \
         Rul)(in_(Pat))(nibs(((shape(Concave 41))(sort Exp))((shape(Concave \
         41))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         24fe2673-9bf1-4b37-8bc2-0af4928fddc0)(content(Whitespace\" \
         \"))))(Tile((id \
         c5c35a7e-0a90-4c18-9e42-da590595cd0d)(label(1))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         89bc1e3b-dc36-4c6a-9cd8-564c00374dfa)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         021881f0-56ea-4a09-bf3f-88d895a36262)(content(Whitespace\" \
         \"))))(Tile((id \
         fc62948b-47a6-419a-acfc-3ee0ae31be12)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         dd572011-926f-4672-b7ff-30fb151ae0cf)(content(Whitespace\"\\n\"))))(Tile((id \
         4e2b8185-8beb-4d70-8f09-1187999e79c2)(label(| =>))(mold((out \
         Rul)(in_(Pat))(nibs(((shape(Concave 41))(sort Exp))((shape(Concave \
         41))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         553514db-dc41-43b3-be9e-23af39dfaead)(content(Whitespace\" \
         \"))))(Tile((id \
         550df429-d46f-4713-a2d2-15c23b5d8d06)(label(n))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         8b3cba0e-97f0-4987-9521-dcbfca1646f6)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         e48cdfbb-2078-42ef-80ac-0f9d1e80ab1c)(content(Whitespace\" \
         \"))))(Secondary((id \
         c1063a2b-3739-4c51-81e5-bd926e23fb88)(content(Whitespace\" \
         \"))))(Secondary((id \
         416efbdf-1897-4499-8622-151ff5c26e91)(content(Whitespace\" \
         \"))))(Secondary((id \
         04c8d777-69df-4326-a2b4-d95b6317fa00)(content(Whitespace\"\\n\"))))(Secondary((id \
         882ce827-71d8-4046-9a2f-b42a378db373)(content(Comment\"# Select the \
         first `1` below: #\"))))(Secondary((id \
         45ce376a-5d22-42a0-a546-a3925a9363b3)(content(Whitespace\"\\n\"))))(Tile((id \
         0a4728f3-622a-4d28-a148-693690bb76b1)(label(fib))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         399c0815-0f4e-4b09-88d4-37db066b8587)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 22))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         92201852-705a-4c54-8200-944265ad9a84)(label(x))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         94656501-efdc-488e-9ad3-741293deeca6)(label(-))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 27))(sort Exp))((shape(Concave \
         27))(sort Exp))))))(shards(0))(children())))(Tile((id \
         d04ab3e9-25f0-4f66-af57-d48144d30395)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         4fa89f3c-8187-4896-90a5-37ae86be7314)(content(Whitespace\"\\n\"))))(Secondary((id \
         aa11f1e9-3329-4338-b574-f225f3b01623)(content(Comment\"# Note the \
         purple `2` below corresponding the call #\"))))(Secondary((id \
         b049a270-25dc-4bb6-99d0-c0c8f8552cac)(content(Whitespace\"\\n\"))))(Secondary((id \
         a6ed225f-492e-4106-8d44-7ef5dcb49889)(content(Comment\"# fib(4-2) \
         which contains the above `1`. The `1` below #\"))))(Secondary((id \
         4c93156f-bc68-4340-8bba-ccbf5ee7b680)(content(Whitespace\"\\n\"))))(Secondary((id \
         98707ae1-b4a7-4631-85c3-8c48793969e3)(content(Comment\"# OTOH is \
         highlit because when the above call was made, #\"))))(Secondary((id \
         52f9dd7a-f3de-4ef1-9d74-f3c57f3d4dac)(content(Whitespace\"\\n\"))))(Secondary((id \
         cdbe94f7-d280-43b3-a524-8bc14e947e6c)(content(Comment\"# the call \
         below had that value. The two `1s` outline in #\"))))(Secondary((id \
         4dd17727-578b-43f1-9a36-4cfeec8f20c9)(content(Whitespace\"\\n\"))))(Secondary((id \
         18647ae9-e8bd-46ec-a6fa-d36b8bb0aa59)(content(Comment\"# purple above \
         come /from/ the indicated call, whereas the #\"))))(Secondary((id \
         0db1c10a-e3b3-4961-b929-c90eb5248a81)(content(Whitespace\"\\n\"))))(Secondary((id \
         ff111caa-ca8b-4074-b36c-dd2edfdea517)(content(Comment\"# highlit `2`s \
         are from the /same/ call the indicated call #\"))))(Secondary((id \
         aadc4b31-0b6a-4099-93f8-29c33fcaf318)(content(Whitespace\"\\n\"))))(Secondary((id \
         7d7f7d80-1397-4011-9f36-3ef3c407df21)(content(Comment\"# was \
         evaluated in. #\"))))(Secondary((id \
         5a6cfe01-203a-4f16-80e8-56d7b6cc0aa8)(content(Whitespace\"\\n\"))))(Tile((id \
         0dc6155e-d673-4f5d-8d28-4458e0387869)(label(+))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 27))(sort Exp))((shape(Concave \
         27))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         fd08c07e-c938-4cec-a429-899c64c108c0)(content(Whitespace\" \
         \"))))(Tile((id \
         023b2f6a-b501-4594-8747-0672fad53734)(label(fib))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         38353929-22c0-42e6-b97d-0ba02e6387cc)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 22))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         f5abeb5b-1286-45ef-aee9-fa5b60acbb1f)(label(x))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         c0898400-ef14-416c-8b46-65c905b0e23a)(label(-))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 27))(sort Exp))((shape(Concave \
         27))(sort Exp))))))(shards(0))(children())))(Tile((id \
         81ee10e5-f0af-4427-ac40-feec7577c084)(label(2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         014a5115-111c-4958-994f-d948bda092e3)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         7dcb8b53-2160-4459-9c65-358415e574c0)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         04f9ec2f-c272-46ba-af00-852a80b0f2c5)(content(Whitespace\"\\n\"))))(Tile((id \
         957ce439-9a54-4aba-a502-bf26bdc38e35)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         a5fabda4-0d8c-4f06-a6d2-87c88610c3a3)(content(Whitespace\" \
         \"))))(Tile((id \
         a50df944-64d0-4064-968b-f4e179398306)(label(fib))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         21ca1979-7dbb-4600-a4e2-5ad0808c901a)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 22))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         2e9b51a5-070d-4599-86c8-15ae422f4c30)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         fa85aa05-13c3-4ecb-b493-fc9d7f8b4777)(content(Whitespace\" \
         \"))))(Tile((id \
         21b9e252-35db-4a46-9a54-b05ce9941ae1)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 30))(sort Exp))((shape(Concave \
         30))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         865ce85b-d6bb-40f6-9153-8ed19ae0d6d8)(content(Whitespace\" \
         \"))))(Tile((id \
         f5ab284e-43d6-444a-9757-e198c0790558)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         cde15ac5-8901-4da3-bf25-583c8560ed0e)(content(Whitespace\" \
         \")))))))))(Tile((id \
         0f4b1b00-0764-4a91-b9b8-a6f9e4e12b60)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 37))(sort Exp))((shape(Concave \
         37))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         c6ccc7e0-c222-47dd-9844-ea755e9f8625)(content(Whitespace\"\\n\"))))(Tile((id \
         5ab7bb27-a578-42fe-9787-316169daa451)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         3ed02d57-f9d2-4214-a883-8648b34a2f4d)(content(Whitespace\" \
         \"))))(Tile((id \
         95b58e91-5247-410d-aa77-716497481ee3)(label(fib))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         50283891-9ab5-49c1-8094-4e42675da80d)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 22))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         f0e363b9-476e-416f-90f4-2197872dbc84)(label(2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         8f5eedd2-61e0-4272-83d1-a4d230d949f8)(content(Whitespace\" \
         \"))))(Tile((id \
         ee7b9975-8915-4a96-b3e1-063cec5a7961)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 30))(sort Exp))((shape(Concave \
         30))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         d081b76d-1431-4112-ad97-4dfa421ac3e0)(content(Whitespace\" \
         \"))))(Tile((id \
         e6ee0414-9787-46e8-867e-7e6c68420df8)(label(2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         96fb33cf-fc53-4585-bbd9-bd322bb28171)(content(Whitespace\" \
         \")))))))))(Tile((id \
         c17bcb79-94d9-4bde-84c9-59ad0053ab85)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 37))(sort Exp))((shape(Concave \
         37))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         fe8eadab-328b-41b4-93d4-9a296a04b311)(content(Whitespace\"\\n\"))))(Tile((id \
         3010afb7-bfac-41e2-89b0-998cb5ecafcf)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         965235bb-9540-458c-a8e3-0ee9bd22a9b1)(content(Whitespace\" \
         \"))))(Tile((id \
         02b11ab4-afe1-4deb-8393-93dcf05dc504)(label(fib))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         97cfcc4b-9fa0-4271-8dfd-8e07b015a15d)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 22))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         b8f7ff8c-d6d7-49c1-adf3-370f5324d9b2)(label(3))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         059062b0-94b7-412a-a5e6-7c7796c5ff67)(content(Whitespace\" \
         \"))))(Tile((id \
         6abd291a-dd9a-4ec0-8da8-8aaf77ad749f)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 30))(sort Exp))((shape(Concave \
         30))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         9c67241d-fb05-4da7-9da5-ab234b77df56)(content(Whitespace\" \
         \"))))(Tile((id \
         559d71e8-559f-49af-91db-b52271af88ce)(label(3))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         ce571578-4865-4bc3-93b2-f91c67d51968)(content(Whitespace\" \
         \")))))))))(Tile((id \
         b1daf610-3166-49c8-a643-f62f3c3e44e2)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 37))(sort Exp))((shape(Concave \
         37))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         6cab4a76-cb54-4eff-9d3a-caf1d69bdbbc)(content(Whitespace\"\\n\"))))(Tile((id \
         7830dd87-8cda-4ca0-bcee-1acd1504a0e7)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         8d5377e5-35e4-428c-a0b4-dc0e9e3ad0f1)(content(Whitespace\" \
         \"))))(Tile((id \
         1d3a047e-9ee7-4d83-8beb-db8ac3f9563d)(label(fib))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         700160e3-263c-46ee-a3af-c50087abad3b)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 22))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         e02d361e-4b57-4866-96cb-3ac79b98185a)(label(4))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         e5178a71-18de-4d24-8781-a995cad89f15)(content(Whitespace\" \
         \"))))(Tile((id \
         579b091d-b325-41ac-87fe-47c86b640f0b)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 30))(sort Exp))((shape(Concave \
         30))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         d9bea473-bb10-4efc-8c3f-c5cc54c6ed59)(content(Whitespace\" \
         \"))))(Tile((id \
         a23c3eab-5e2e-4b18-ba7c-65a968629a93)(label(5))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         b79ad11d-1564-4974-9e4e-eee63a61c8c9)(content(Whitespace\" \
         \")))))))))(Tile((id \
         17554e39-c711-44ba-b502-0ac82d4a0609)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 37))(sort Exp))((shape(Concave \
         37))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         d45afbbd-b540-4c75-84fa-7a55050b83c9)(content(Whitespace\"\\n\"))))(Tile((id \
         146297c6-1c1a-4f5d-a7aa-3c59664df8c2)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         78e2ffbe-cf4a-40e1-9c38-0679d9dd1db9)(content(Whitespace\" \
         \"))))(Tile((id \
         6548c915-6b9b-48b6-9690-577575b18952)(label(fib))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         809f1b16-6408-4038-812c-d7bdec7d1875)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 22))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         2b557463-6616-47c3-9b83-00f64537fe26)(label(5))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         d1d09e51-e2dd-43e2-a1e6-eaaf3027e79a)(content(Whitespace\" \
         \"))))(Tile((id \
         8a9bd83d-e859-4e33-b69c-e03469a11bb2)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 30))(sort Exp))((shape(Concave \
         30))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         7439d02a-1071-4946-a16b-b8d3c8bf2fd3)(content(Whitespace\" \
         \"))))(Tile((id \
         f2ff54fe-2e7e-43f2-9e1b-a7e4b7d16dce)(label(8))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         51bdf316-9bc3-4949-9da9-d5aac7c66dd3)(content(Whitespace\" \
         \")))))))))(Tile((id \
         f7577712-71ae-4803-8646-7ed6c8d02c7c)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 37))(sort Exp))((shape(Concave \
         37))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         780d3c37-730f-404e-9ae7-655695c4274f)(content(Whitespace\"\\n\"))))(Tile((id \
         be20bc2f-2979-4056-b36f-b8c7f13dd82f)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         6e1520d2-73d3-47cc-adbc-92835d473f4d)(content(Whitespace\" \
         \"))))(Tile((id \
         55aa4d30-bc09-413d-85b9-2bf54445fb8d)(label(fib))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         40462147-ab28-43f9-925b-e2f42d0ac4ea)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 22))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         4c446f46-b8ac-47b9-bf9d-78645cdb2a10)(label(6))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         3c8c10fb-176d-4e3c-adb2-b65fd0b7cc18)(content(Whitespace\" \
         \"))))(Tile((id \
         55df95a6-528b-4428-8b08-475d34cfed77)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 30))(sort Exp))((shape(Concave \
         30))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         b0330a6d-270c-48a9-b63a-9ab94a5dc674)(content(Whitespace\" \
         \"))))(Tile((id \
         e5f02a34-5512-44ab-a8b1-2b22d0f38c51)(label(13))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         8e0635d7-38ae-41b0-9fb1-abc8d054694b)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         1bb852c7-2da0-4b94-a6c1-182457752b54)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         9d93ebc1-7656-4b82-8162-abda2bfefdff)(content(Whitespace\" \
         \"))))(Grout((id e7062cc6-3eba-46ef-b374-5deec0ecc8f1)(shape \
         Convex)))(Secondary((id \
         f6cd1883-2e76-4e2b-b671-af09377b5aca)(content(Whitespace\"\\n\"))))(Secondary((id \
         6f6b1aa2-a901-4702-b13c-fa344d5c9c08)(content(Whitespace\"\\n\"))))(Secondary((id \
         a0822f7c-4893-43fe-a6c8-6587bf52565c)(content(Comment\"# ADVANCED \
         FEATURES #\"))))(Secondary((id \
         171bff09-512e-4348-96a0-9b45864aeac0)(content(Whitespace\"\\n\"))))(Secondary((id \
         e29572f9-1542-449d-8f75-c263fb59aa6b)(content(Comment\"# - You can \
         resize a cell by holding shift and dragging horizontally \
         #\")))))))(ancestors())))(caret Outer))";
      backup_text =
        "#  _____           _                #\n\
         # |  __ \\         | |               #\n\
         # | |__) | __ ___ | |__   ___  ___  #\n\
         # |  ___/ '__/ _ \\| '_ \\ / _ \\/ __| #\n\
         # | |   | | | (_) | |_) |  __/\\__ \\ #\n\
         # |_|   |_|  \\___/|_.__/ \\___||___/ #\n\
         # INLINE EVAL WITH PROBE PROJECTORS #\n\n\
         # INTRODUCTION #\n\n\
         # Probe projectors are a kind of inline evaluation, #\n\
         # similar to value hints in Emacs or IntelliJ. #\n\n\
         # You can put these on any expression or variable binding to #\n\
         # see a list of all values taken on by that expression/pattern. #\n\
         # By default values are sorted by left-to-right by most-recent. #\n\n\
         # More generally, each cell represents a stack state, #\n\
         # including the top stack frame / closure and hence the #\n\
         # expression's value, the values of environment variables, #\n\
         # as well as the surrounding call stack context. #\n\n\
         # When a cell is selected, you can hover over it to see #\n\
         # relevant environment variables, and all /other/ cells #\n\
         # are decorated according to their relative position in #\n\
         # to the selected cell. in the context #\n\n\
         # Probe are intended mostly as a println replacement #\n\
         # for exposing intermediate values, with the above decorations #\n\
         # as a supporting feature to help maintain context when #\n\
         # navigating between multiple probed expressions, which #\n\
         # may take on many values across nested or recursive functions. #\n\n\n\
         # TUTORIAL #\n\n\
         # The expression (10 * 10) below has a probe.  #\n\
         # Its value, 20, is shown in a cell to the right. #\n\
         let chips = (10 + 10) in\n\n\
         # To probe the below expression, put your caret to #\n\
         # left of the `(` and press option/alt-v (for value), #\n\
         # or select `Probe` from the lower right corner menu: #\n\
         let mult = (1 + 2 * 3) in\n\
         # The expression should be encased in a green block #\n\
         # and a cell reading `7` should appear to the right. #\n\
         # The same shortcut or menu toggle removes it. #\n\n\
         # Click the below cell (with value 21) to select it. #\n\
         let score = (chips * mult) in\n\
         # Notice when you hover over a selected cell, it #\n\
         # shows the values of any contained variables. #\n\n\
         # Probes only have cells if the are evaluated. #\n\
         # Below, only the first case branch is evaluated. #\n\
         let check = case true\n\
         | false => \"checks out\"\n\
         | true => \"you cheated\"  \n\
         end in\n\
         # Note the 2nd branch probe has a zero to the right. #\n\
         # This is the cell's collected closure count, i.e. #\n\
         # the number of times the expression was evaluated #\n\n\
         # Probes can be placed on expressions: #\n\
         let pow = 50 ** 2 in\n\
         # And also on patterns (e.g. variables), shown in blue: #\n\
         let pow = 54 ** 2 in\n\
         # Expressions currently CAN'T BE EDITED WHILE PROBED #\n\
         # So probing a name instead makes iteration easier. #\n\n\n\
         # FUNCTIONS #\n\
         let _ =\n\
         # Because functions can run multiple times, they can #\n\
         # have multiple cells. Note the closure counts below #\n\
         # are all 2, indicating each probe was evaluated twice. #\n\
         let celsius = fun farenheit ->\n\
         # Click to select the cell above reading 72.5 #\n\
         let diff = farenheit -. 32. in\n\
         # This highlights cells below corresponding to the same #\n\
         # function call: the cells reading 40.5 and 22.5) #\n\
         5./.9. *. diff in\n\
         let (t1, t2) = 72.5, 103.1 in (\n\
         # It also highlights in purple the cell #\n\
         # of the function's call site#\n\
         celsius(t1),\n\
         # Now select the cell above reading 22.5 #\n\
         celsius(t2)\n\
         # Note the 72.5, 40.5, and 22.5 are no longer green-highlit #\n\
         # as they are not part of the same call as /the expression/ #\n\
         # `celsius(t1)`. However, they now have purple hats, indicating #\n\
         # they are below that function call in the call stack #\n\
         ) in\n\n\
         # BRANCHING IN FUNCTIONS #\n\
         let cases =\n\
         # Select `6` then `5` then '4' below: #\n\
         fun x -> case x \n\
         # Note how each activate exactly one branch below: #\n\
         | 4 => true\n\
         # Select the `5` above and then the `false` below: #\n\
         | 5 => false\n\
         # Note the same things are highlit as both cells are #\n\
         # from the same call to cases#\n\
         | _ => true end    \n\
         in let _ = (\n\
         # Select `true` below and then the `4` cell #\n\
         # for the argument x to `cases` above. #\n\
         cases(4),\n\
         # Note how the same cells stay indicated, but the kind #\n\
         # of indication changes. The `true` below the `4` above #\n\
         # goes from purple outline (created by the cases(4) call) #\n\
         # to green highlighting (part of the same call as `4`). #\n\
         # The formerly selected lower `true` is now highlit in #\n\
         # purple since it indicates the call where `4` lives . #\n\
         cases(5),\n\
         cases(6)\n\
         ) in\n\n\
         # FUNCTIONS CALLING FUNCTIONS #\n\
         let _ =\n\
        \  # Select `9` below. Note four cells below become purple #\n\
         let fourth = fun f -> 4 * f - 4 in\n\
        \  # This is because they represent function calls #\n\
        \  # above the `9` cell in the function call stack. #\n\
        \  # For example 32 below represents the call producing `9`.  #\n\
         let third = fun t -> fourth(t - 3) / 3 in\n\
        \  # Now, select `32` above. Note the 9 now has a purple hat. #\n\
        \  # This represents that it is below the `32` call in the stack. #\n\
        \  # Now select `10` below, which is a call to `third`: #\n\
         let second = fun s -> third(2 * s) + 2 in\n\
        \  # Note that `9` and `32` both have hats. `9` has a taller hat #\n\
        \  # to show it's lower in the call stack. `32` has purple text #\n\
        \  # to further emphasize that is is directly below `10`. #\n\
        \  # Now select 12 below, representing a call to `second` #\n\
         let first = fun f -> second(f + 1) * 2 in\n\
        \  # Note how the hats have changed. Finally, select `24` below, #\n\
        \  # and then again select 12, 10, 32, and 9 in turn. Notice how #\n\
        \  # the solid purple call indicators have internal shadows #\n\
        \  # representing their depth relative to the selected cell #\n\
         first(5) in\n\n\
         # RECURSION #\n\
         # Note how cells are lowered/raised to indicate their #\n\
         # relative call stack depth to the selected cell #\n\
         let fact = fun x ->\n\
         case x\n\
         | 1 => 1\n\
         | _ =>\n\
         let r = fact(x-1) \n\
         in x*r  \n\
         end in\n\
         test fact(5) == 120 end;\n\n\
         # TAIL RECURSION #\n\
         let fact1 = let go =\n\
         fun (x,\n\
        \ acc) ->\n\
         case x\n\
         | 1 => acc\n\
         | _ =>\n\
         let r = x*acc \n\
         in go(x-1, r) \n\
         end in\n\
         fun x -> go(x,1) in\n\
         test fact1(5) == 120 end;\n\n\
         # FUNCTIONS IN FUNCTIONS #\n\
         # The frunk factory prethunks your frunk for later clunking #\n\
         let frunk_factory = fun y ->\n\
         # This is a play area to explore nested function definitions #\n\
         # and functions returning functions #\n\
         let factor =\n\
         4 + 10 * y in\n\
         let refactor = fun x ->\n\
         x + factor in\n\
         let factor =\n\
         refactor(factor)\n\
         - refactor(y) in \n\
         let perturb = fun s ->\n\
         factor + refactor(s) in\n\
         fun z ->\n\
         perturb(3*z)\n\
         + perturb(5*z)  \n\
         in let new_frunk = frunk_factory(7) in\n\
         test new_frunk(4) == 314 end;\n\
         test new_frunk(6) == 330 end;\n\n\n\
         let _ = let fib: Int -> Int =\n\
         # Recursive calls can complicate probe display due #\n\
         # due to overlapping information channels.#\n\
         fun x -> case x\n\
         | 0 => 1\n\
         | 1 => 1\n\
         | n =>   \n\
         # Select the first `1` below: #\n\
         fib(x-1)\n\
         # Note the purple `2` below corresponding the call #\n\
         # fib(4-2) which contains the above `1`. The `1` below #\n\
         # OTOH is highlit because when the above call was made, #\n\
         # the call below had that value. The two `1s` outline in #\n\
         # purple above come /from/ the indicated call, whereas the #\n\
         # highlit `2`s are from the /same/ call the indicated call #\n\
         # was evaluated in. #\n\
         + fib(x-2) end\n\
         in\n\
         test fib(1) == 1 end;\n\
         test fib(2) == 2 end;\n\
         test fib(3) == 3 end;\n\
         test fib(4) == 5 end;\n\
         test fib(5) == 8 end;\n\
         test fib(6) == 13 end\n\
         in  \n\n\
         # ADVANCED FEATURES #\n\
         # - You can resize a cell by holding shift and dragging horizontally #";
    } )
