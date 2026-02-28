let out : string * Haz3lcore.PersistentSegment.t =
  ( "Study / tutorial / 07-writing",
    {
      segment =
        "((Secondary((id \
         df5c5196-af67-41cf-997c-ef3356f629fd)(content(Comment\"# WRITING WITH \
         LIVE VALUES #\"))))(Secondary((id \
         2753c194-737e-458f-9628-fb2edc3559c6)(content(Whitespace\"\\n\"))))(Secondary((id \
         ee740a5a-e931-4ce1-94f4-692b4e2e2680)(content(Whitespace\"\\n\"))))(Secondary((id \
         d9cabd62-47ef-4639-a6c6-3b3d47a9c93f)(content(Comment\"# In this \
         tutorial you'll write code step by step.           \
         #\"))))(Secondary((id \
         e8aa9e14-5290-46b7-b794-b2bf9748fb33)(content(Whitespace\"\\n\"))))(Secondary((id \
         7057f56e-2c42-49d7-a123-73028ecd789d)(content(Comment\"# Turn on Auto \
         Mode (microscope icon, top right).            #\"))))(Secondary((id \
         a7014187-ecde-4720-92ae-c71f030e6671)(content(Whitespace\"\\n\"))))(Secondary((id \
         ed6d5c0e-4942-4cda-b2f2-c3386471ae01)(content(Comment\"# Each stage \
         shows you what to type, then you type it        #\"))))(Secondary((id \
         1d5d1c1f-8031-4253-89a5-ceffe53323e9)(content(Whitespace\"\\n\"))))(Secondary((id \
         b204e527-f528-4798-9045-2a52374c02bf)(content(Comment\"# in the \
         designated area and see probe values appear.        \
         #\"))))(Secondary((id \
         74a7bdf6-16b4-480b-9e4e-9d6826f53e7d)(content(Whitespace\"\\n\"))))(Secondary((id \
         26e62bb1-9e41-478e-8522-a9b2fee7a76d)(content(Whitespace\"\\n\"))))(Secondary((id \
         6e3336cb-f673-448c-abdc-934aef6c4dd0)(content(Comment\"# \
         ============================================================ \
         #\"))))(Secondary((id \
         4558d37e-273d-47b9-bb2b-6eda4f03fd25)(content(Whitespace\"\\n\"))))(Secondary((id \
         fe0463aa-7571-4a6a-9e97-f0c9514e066b)(content(Comment\"# STAGE 1: LET \
         BINDINGS AND STRINGS                          #\"))))(Secondary((id \
         1c206224-6e9f-447e-95d4-c6dc7156009c)(content(Whitespace\"\\n\"))))(Secondary((id \
         53600489-753d-408e-ab12-43fa9610d988)(content(Comment\"# \
         ============================================================ \
         #\"))))(Secondary((id \
         9774dba9-5e22-45e0-9ede-b234446cc498)(content(Whitespace\"\\n\"))))(Secondary((id \
         3c76a09c-0f89-4ae7-ba4b-0eeaced2eb88)(content(Whitespace\"\\n\"))))(Secondary((id \
         cedb9fca-b5a9-4f95-b916-a2ca9b3986a5)(content(Comment\"# `let x = \
         expr in body` binds a value. `++` concatenates    \
         #\"))))(Secondary((id \
         282a91f1-07f3-438e-8685-ff33b45614b6)(content(Whitespace\"\\n\"))))(Secondary((id \
         f86583d8-75b4-4280-9841-f3de65e5f27a)(content(Comment\"# strings. \
         `string_of_int` converts a number to a string.    \
         #\"))))(Secondary((id \
         ebfd5562-d19b-4658-8066-f3b1ee77a97c)(content(Whitespace\"\\n\"))))(Secondary((id \
         4cf14c10-caa4-4528-b9e1-5f0a15024eae)(content(Comment\"#                                                             \
         #\"))))(Secondary((id \
         72e19524-3547-462f-981c-5ea145dd0b80)(content(Whitespace\"\\n\"))))(Secondary((id \
         d7a0f5f7-d3dc-454f-8aa1-f11a0f65cf60)(content(Comment\"# Replace the \
         ? below with:                                   #\"))))(Secondary((id \
         7942e7b8-55de-4da0-9f25-c6319c2ebc22)(content(Whitespace\"\\n\"))))(Secondary((id \
         97405ba5-1831-4997-b124-bfc41a53840c)(content(Comment\"#                                                             \
         #\"))))(Secondary((id \
         4b4def13-c0ba-4997-847e-d5ac403eed5b)(content(Whitespace\"\\n\"))))(Secondary((id \
         d483f317-8534-4d55-8686-bc77e3ae264d)(content(Comment\"#   let name = \
         \\\"Fern\\\" in                                      \
         #\"))))(Secondary((id \
         b27a8035-000a-4c79-92fd-d2698ab3a97f)(content(Whitespace\"\\n\"))))(Secondary((id \
         15090421-66df-4571-a50d-950732f396ac)(content(Comment\"#   let water \
         = 250 in                                        #\"))))(Secondary((id \
         e88af710-a2db-488c-b591-1be08997f6a1)(content(Whitespace\"\\n\"))))(Secondary((id \
         fcdfc729-5f93-4c3b-ad18-88d28412af8d)(content(Comment\"#   let label \
         =                                               #\"))))(Secondary((id \
         1649b681-9c30-4494-b662-e4db94e3fc11)(content(Whitespace\"\\n\"))))(Secondary((id \
         26fd684c-9035-4e3a-aba5-ad076b87dd44)(content(Comment\"#     name ++ \
         \\\": \\\" ++ string_of_int(water) ++ \\\"ml\\\"            \
         #\"))))(Secondary((id \
         296a5264-0aaa-4ef3-91c9-da768ae24ba4)(content(Whitespace\"\\n\"))))(Secondary((id \
         9846c223-088d-467e-a11c-20156d6f70bb)(content(Comment\"#   \
         in                                                        \
         #\"))))(Secondary((id \
         771a8b90-bc2f-498c-81e8-25c9b1c2fcf5)(content(Whitespace\"\\n\"))))(Secondary((id \
         e635eeb2-f463-4f04-9292-e755b77bf11e)(content(Comment\"#   \
         label                                                     \
         #\"))))(Secondary((id \
         acca6f4b-f346-4697-98b0-6ba8c67fa181)(content(Whitespace\"\\n\"))))(Secondary((id \
         aef05b30-5635-41e0-b54e-b19f2884203c)(content(Comment\"#                                                             \
         #\"))))(Secondary((id \
         67a81c06-c888-4d1c-aadc-50a9f1f4261e)(content(Whitespace\"\\n\"))))(Secondary((id \
         fe4636b2-ca09-449d-90a5-df80c12138b1)(content(Comment\"# Then click \
         inside your let bindings. Auto-probe should      \
         #\"))))(Secondary((id \
         a1e4ecd7-a82f-47fb-b372-7a79b3aea2c4)(content(Whitespace\"\\n\"))))(Secondary((id \
         0747940b-9388-4099-9109-966394a7ecfb)(content(Comment\"# show \
         \\\"Fern\\\", 250, and \\\"Fern: 250ml\\\". Try changing the      \
         #\"))))(Secondary((id \
         4c29e114-f420-422e-84d9-48318f82f0c8)(content(Whitespace\"\\n\"))))(Secondary((id \
         090bbd2e-777e-445e-b9c8-70879210ffb7)(content(Comment\"# name or \
         number and watch the label update instantly.        \
         #\"))))(Secondary((id \
         406f04cb-70c5-4932-8efd-45c29cc97f11)(content(Whitespace\"\\n\"))))(Secondary((id \
         8760832d-60be-4462-beeb-1eb499ee0425)(content(Whitespace\"\\n\"))))(Tile((id \
         a9734f0e-6fa2-4acb-977d-f1eef1ee84e9)(label(?))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         536bd02a-6d42-4b2d-a0c7-c1cddff75053)(content(Whitespace\"\\n\"))))(Secondary((id \
         a5c0ea22-f452-4dcd-a3d3-602cd3c618f8)(content(Whitespace\"\\n\"))))(Tile((id \
         a552e9a0-1a23-408d-9b43-d41d7fa40032)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         09edc4d8-e912-4625-9002-154d73c8c948)(content(Whitespace\"\\n\"))))(Secondary((id \
         b19d316b-ef09-47d6-9131-590baf3a02eb)(content(Whitespace\"\\n\"))))(Secondary((id \
         29fad181-329e-4aea-ad03-4aa4b609bcfb)(content(Comment\"# \
         ============================================================ \
         #\"))))(Secondary((id \
         d4e137a1-4879-4d45-adcc-d1d1a7cf9054)(content(Whitespace\"\\n\"))))(Secondary((id \
         4700a98d-5323-4790-8bc6-096ad011c92c)(content(Comment\"# STAGE 2: A \
         FUNCTION AND TESTS                              #\"))))(Secondary((id \
         fc70f941-bffc-47eb-bd7d-3a71ec8fa9c7)(content(Whitespace\"\\n\"))))(Secondary((id \
         476340b2-a6d2-42ed-962a-b4b8504b483b)(content(Comment\"# \
         ============================================================ \
         #\"))))(Secondary((id \
         a0e5e585-e0e5-4243-b196-a1db7f0589f7)(content(Whitespace\"\\n\"))))(Secondary((id \
         03fe47b0-9541-4c90-a8c2-82ad2a0127a9)(content(Whitespace\"\\n\"))))(Secondary((id \
         431f8e1b-6d6e-4898-9c4d-8ca5f50fc0f3)(content(Comment\"# Now let's \
         make the label formatting reusable.               \
         #\"))))(Secondary((id \
         8dab9ece-a223-4083-ae6d-5cbb4ae29aad)(content(Whitespace\"\\n\"))))(Secondary((id \
         403970ae-08ce-4fa8-b2dd-9abbd3cd8d07)(content(Comment\"# `fun (a, b) \
         -> body` defines a function taking a tuple.     #\"))))(Secondary((id \
         d1c54720-9171-4a95-865b-78160ec1c9db)(content(Whitespace\"\\n\"))))(Secondary((id \
         40cec566-a66c-40a3-8a5c-76ac1d75e39e)(content(Comment\"# `test X == Y \
         end` checks that X equals Y.                  #\"))))(Secondary((id \
         98e0e704-a18f-4987-919a-a4c8155a104f)(content(Whitespace\"\\n\"))))(Secondary((id \
         e83f2f70-2b1d-4055-ba16-806cb5030063)(content(Comment\"#                                                             \
         #\"))))(Secondary((id \
         8adf482f-f2e9-470c-8020-608df605a2e9)(content(Whitespace\"\\n\"))))(Secondary((id \
         4366ef27-81cf-49a9-887f-5b151d49f63b)(content(Comment\"# Replace the \
         ? below with:                                   #\"))))(Secondary((id \
         104fe664-4920-4973-8f12-1f172fceab50)(content(Whitespace\"\\n\"))))(Secondary((id \
         4c51c00e-02e1-4a65-bfd7-0bfc40c29e36)(content(Comment\"#                                                             \
         #\"))))(Secondary((id \
         d8a8e1a3-44b0-42fc-b200-a99526199637)(content(Whitespace\"\\n\"))))(Secondary((id \
         80573a1a-c191-4b84-8a53-2f4b6059da85)(content(Comment\"#   let \
         format_plant: (String, Int) -> String =               \
         #\"))))(Secondary((id \
         2c444eed-64a0-4cfc-bf86-c135206abedb)(content(Whitespace\"\\n\"))))(Secondary((id \
         69423b6f-de39-4f14-a244-cb59b8f6541e)(content(Comment\"#     fun \
         (name, water) ->                                    \
         #\"))))(Secondary((id \
         5f0f457a-37f1-4411-8801-e3a38d657ac8)(content(Whitespace\"\\n\"))))(Secondary((id \
         301e9d0f-5796-4e94-9e62-d5736ae2d6d0)(content(Comment\"#     name ++ \
         \\\": \\\" ++ string_of_int(water) ++ \\\"ml\\\"            \
         #\"))))(Secondary((id \
         dadc4df9-c4db-47c6-9d4a-998b4f271cc0)(content(Whitespace\"\\n\"))))(Secondary((id \
         4a2387dc-3cdc-4adb-bb6d-7253674335ae)(content(Comment\"#   \
         in                                                        \
         #\"))))(Secondary((id \
         453f1e15-9f21-41a3-946d-4eeb7f2e5e75)(content(Whitespace\"\\n\"))))(Secondary((id \
         4d70d27a-0e2a-45bc-b063-fab5300cdffb)(content(Comment\"#                                                             \
         #\"))))(Secondary((id \
         9633fe2e-db0c-43b2-9c49-51aba552096d)(content(Whitespace\"\\n\"))))(Secondary((id \
         7fe0ca6d-3b0b-40c1-8265-53c747eeca92)(content(Comment\"#   test \
         format_plant(\\\"Fern\\\", 250)                            \
         #\"))))(Secondary((id \
         289ad528-b671-4002-9c3f-1e0458a9aea0)(content(Whitespace\"\\n\"))))(Secondary((id \
         b0973291-e809-41a2-a220-e94855c259d6)(content(Comment\"#     == \
         \\\"Fern: 250ml\\\" end;                                   \
         #\"))))(Secondary((id \
         7f23e56c-0723-400a-9ff0-7f1553bc1741)(content(Whitespace\"\\n\"))))(Secondary((id \
         03820944-bbb4-4c8d-ab97-5bf30f1b6c0a)(content(Comment\"#   test \
         format_plant(\\\"Orchid\\\", 180)                          \
         #\"))))(Secondary((id \
         b3861773-9810-4628-8f11-aa5d6b810060)(content(Whitespace\"\\n\"))))(Secondary((id \
         f83f46a5-5be7-4cf0-9484-2cd30070e24e)(content(Comment\"#     == \
         \\\"Orchid: 180ml\\\" end;                                 \
         #\"))))(Secondary((id \
         bd0308b8-33a2-48e9-bd5e-52874badf1ed)(content(Whitespace\"\\n\"))))(Secondary((id \
         afb1fed8-40bc-4ddd-853a-968f2e57ca03)(content(Comment\"#   test \
         format_plant(\\\"Cactus\\\", 50)                           \
         #\"))))(Secondary((id \
         3c50e4cd-413a-446c-b805-9b0fad75afc3)(content(Whitespace\"\\n\"))))(Secondary((id \
         09432b28-e845-4854-873d-2c3fea17facc)(content(Comment\"#     == \
         \\\"Cactus: 50ml\\\" end                                   \
         #\"))))(Secondary((id \
         84dec32c-1070-4e71-bcf4-ea6d4185cf2f)(content(Whitespace\"\\n\"))))(Secondary((id \
         43ac5235-607b-4167-a4f3-5a02dcaf8f95)(content(Comment\"#                                                             \
         #\"))))(Secondary((id \
         be47eff5-9191-49d0-9715-80193cb7f748)(content(Whitespace\"\\n\"))))(Secondary((id \
         f3c1f0f3-25c6-4b6c-8de9-3dd708b8e1f5)(content(Comment\"# Click inside \
         format_plant to see auto-probe values for      #\"))))(Secondary((id \
         f1effdcb-4c56-451a-ab62-62b1cbcbcd1a)(content(Whitespace\"\\n\"))))(Secondary((id \
         e128318a-43d9-48cb-a3af-90c297c41a16)(content(Comment\"# each test \
         call. Toggle Many mode (Space) to see all         \
         #\"))))(Secondary((id \
         dcb69b18-f169-4593-ba5e-be565c1c57b3)(content(Whitespace\"\\n\"))))(Secondary((id \
         85470979-175e-4d6a-b28c-2624e7c2e04d)(content(Comment\"# three \
         results side by side.                                 \
         #\"))))(Secondary((id \
         9644e02f-754c-423c-803b-e331713048ca)(content(Whitespace\"\\n\"))))(Secondary((id \
         4a09291d-fc9d-4cef-9516-34e3e3b68148)(content(Whitespace\"\\n\"))))(Tile((id \
         c2002a40-2986-4034-9804-6dc2b6f96393)(label(?))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         0641a211-7f42-491e-b344-7af78148b20e)(content(Whitespace\"\\n\"))))(Secondary((id \
         cd157157-292a-458b-82f2-e511b4cb9e03)(content(Whitespace\"\\n\"))))(Tile((id \
         e377dfea-57b0-4013-8092-4f4cd1b76f88)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         b7385030-6a45-4f59-986e-4257408a1cc1)(content(Whitespace\"\\n\"))))(Secondary((id \
         2d771f25-fde9-4d79-9566-e1ea3a2d8390)(content(Whitespace\"\\n\"))))(Secondary((id \
         be248320-413e-43d8-82b3-839b95b36771)(content(Comment\"# \
         ============================================================ \
         #\"))))(Secondary((id \
         c2491931-ffe8-44ef-b032-0f6df915744d)(content(Whitespace\"\\n\"))))(Secondary((id \
         1d85c725-de15-4e21-94f1-de9069435f87)(content(Comment\"# STAGE 3: A \
         HELPER WITH IF/ELSE                             #\"))))(Secondary((id \
         1a4a5c6b-3b4c-4038-a113-f63a60e9fead)(content(Whitespace\"\\n\"))))(Secondary((id \
         515f5bad-7ab1-4bb8-8796-60b49ebb9f3d)(content(Comment\"# \
         ============================================================ \
         #\"))))(Secondary((id \
         0c88b60d-eb28-4c4f-8bdb-c91518d9b594)(content(Whitespace\"\\n\"))))(Secondary((id \
         3f2a74b8-6e9a-472c-937a-fc45f726caf1)(content(Whitespace\"\\n\"))))(Secondary((id \
         a57472b9-893f-49b8-a3ee-6e8ad49ccb57)(content(Comment\"# Plants with \
         high water needs should get a \
         \\240\\159\\146\\167\\240\\159\\146\\167 tag.        \
         #\"))))(Secondary((id \
         e7ddfd1c-33ef-4427-97e1-29f8742d8bb9)(content(Whitespace\"\\n\"))))(Secondary((id \
         64634aee-9e9f-4178-b788-35336474191d)(content(Comment\"# We'll write \
         a helper function using a conditional:          #\"))))(Secondary((id \
         b90c4da2-371f-4a8c-b981-d8a44f555e63)(content(Whitespace\"\\n\"))))(Secondary((id \
         6b2a56c8-d242-4c5a-92a5-94ae844aa739)(content(Comment\"#   `if \
         condition then expr1 else expr2`                      \
         #\"))))(Secondary((id \
         51f4e9c1-66e8-4c2b-aa31-4606b9e51506)(content(Whitespace\"\\n\"))))(Secondary((id \
         1b034b8a-7735-448f-95e6-c02e23a7fa78)(content(Comment\"#                                                             \
         #\"))))(Secondary((id \
         d7e969bc-d612-48ed-b963-3b47b8a771c6)(content(Whitespace\"\\n\"))))(Secondary((id \
         c6c1ea43-1b28-4333-a07d-cea45932db6a)(content(Comment\"# A note on \
         writing order: in practice, you'd first write    \
         #\"))))(Secondary((id \
         2b89d1c0-032d-4c74-977f-e971844eda60)(content(Whitespace\"\\n\"))))(Secondary((id \
         5dbcf020-6734-4c30-a4d6-22e8aea6a10f)(content(Comment\"# the CALL to \
         water_tag inside format_plant, see a hole       #\"))))(Secondary((id \
         49dfac55-594e-4edf-b3ff-8608c7d6baeb)(content(Whitespace\"\\n\"))))(Secondary((id \
         80a800c5-8948-46b0-a33e-c3d3e515c279)(content(Comment\"# value, then \
         go implement the helper above it. This          #\"))))(Secondary((id \
         cfcc9c8d-36af-48a2-b9d3-0f4367b2a83a)(content(Whitespace\"\\n\"))))(Secondary((id \
         7a9c79a8-7f79-4c89-815f-483bd8a9b03d)(content(Comment\"# outside-in \
         approach lets live values guide your writing.    \
         #\"))))(Secondary((id \
         1fd59b89-d48a-486c-b9d6-aedd12987b2b)(content(Whitespace\"\\n\"))))(Secondary((id \
         27c400fb-04b2-410d-922c-ed7837a3d130)(content(Comment\"#                                                             \
         #\"))))(Secondary((id \
         a17adc21-88f0-4e0a-9e39-93d676ca7ed1)(content(Whitespace\"\\n\"))))(Secondary((id \
         4609fc85-d344-425e-a3f7-9132e5b4219e)(content(Comment\"# Replace the \
         ? below with:                                   #\"))))(Secondary((id \
         6f1a4267-ebf3-4ac7-abc7-99347efc9080)(content(Whitespace\"\\n\"))))(Secondary((id \
         5f016d6c-8e05-4e2d-9935-298fccc4f327)(content(Comment\"#                                                             \
         #\"))))(Secondary((id \
         95d890bc-89c9-4504-ac31-b38442bc876a)(content(Whitespace\"\\n\"))))(Secondary((id \
         82e3c7f6-7ff4-4977-8ef9-58401f795e90)(content(Comment\"#   let \
         water_tag: Int -> String =                            \
         #\"))))(Secondary((id \
         dee1eb12-38c9-43ae-9386-6731880f233c)(content(Whitespace\"\\n\"))))(Secondary((id \
         051ff889-60ad-40ee-a51b-c09e2a29cbc7)(content(Comment\"#     fun \
         amount ->                                           \
         #\"))))(Secondary((id \
         65316232-a3c0-413a-88cc-abdc7dd277eb)(content(Whitespace\"\\n\"))))(Secondary((id \
         edfcd9d8-1768-45f4-991b-622e80c99989)(content(Comment\"#     if \
         amount > 200 then \
         \\\"\\240\\159\\146\\167\\240\\159\\146\\167\\\"                             \
         #\"))))(Secondary((id \
         8146c107-2109-4ec4-97ea-dbf0acd0c5db)(content(Whitespace\"\\n\"))))(Secondary((id \
         54e7f550-6e2c-4670-adc4-8f3938596fd5)(content(Comment\"#     else \
         \\\"\\240\\159\\146\\167\\\"                                               \
         #\"))))(Secondary((id \
         f75efbf2-4b67-4dfb-8edf-e90d7ff35c59)(content(Whitespace\"\\n\"))))(Secondary((id \
         54be62f4-c067-42f7-b5d0-8866b3cd955f)(content(Comment\"#   \
         in                                                        \
         #\"))))(Secondary((id \
         b0a891a7-2d14-4b6d-bf38-2fb657afb510)(content(Whitespace\"\\n\"))))(Secondary((id \
         42966fae-72e0-45e4-996a-cdfe83c092e7)(content(Comment\"#                                                             \
         #\"))))(Secondary((id \
         6b605f30-b271-486c-aca1-9909b2631f01)(content(Whitespace\"\\n\"))))(Secondary((id \
         63c5344b-2e2a-4ac6-9a3e-52ca0a1afa8d)(content(Comment\"#   let \
         format_plant: (String, Int) -> String =               \
         #\"))))(Secondary((id \
         ee4922ee-f3a0-474f-a7a5-50c68f2c70a0)(content(Whitespace\"\\n\"))))(Secondary((id \
         f3e6aa06-b30d-453c-890c-dd12ece98712)(content(Comment\"#     fun \
         (name, water) ->                                    \
         #\"))))(Secondary((id \
         406912f0-f07b-4ca7-b56e-4419e078ac12)(content(Whitespace\"\\n\"))))(Secondary((id \
         4013217a-c9d9-4796-998f-cbb3c57ec214)(content(Comment\"#     let tag \
         = water_tag(water) in                           #\"))))(Secondary((id \
         471fde2a-d80e-4061-ab80-2a509191eb1e)(content(Whitespace\"\\n\"))))(Secondary((id \
         a98c52ea-cfe6-4bc8-84e6-f5cf16d5bb25)(content(Comment\"#     tag ++ \
         \\\" \\\" ++ name ++ \\\": \\\"                              \
         #\"))))(Secondary((id \
         82aabfe6-d819-4f0e-8ae1-b6f93f9f5983)(content(Whitespace\"\\n\"))))(Secondary((id \
         6b0b3c54-8c7f-4d46-9378-07b229693325)(content(Comment\"#       ++ \
         string_of_int(water) ++ \\\"ml\\\"                       \
         #\"))))(Secondary((id \
         9d828329-51f2-45fa-b718-d584d932594c)(content(Whitespace\"\\n\"))))(Secondary((id \
         19c5ddf7-4d18-4003-9cf1-996449b3739a)(content(Comment\"#   \
         in                                                        \
         #\"))))(Secondary((id \
         791ff960-aff5-4a5d-b7a4-7c9cded73d71)(content(Whitespace\"\\n\"))))(Secondary((id \
         55268870-ca48-4b28-8ba8-f4d5d3263e15)(content(Comment\"#                                                             \
         #\"))))(Secondary((id \
         b86570d8-2838-446e-8a16-a68bc240981e)(content(Whitespace\"\\n\"))))(Secondary((id \
         afaa853e-2a9c-4d68-a063-dcfc79bda496)(content(Comment\"#   test \
         format_plant(\\\"Fern\\\", 250)                            \
         #\"))))(Secondary((id \
         b077b71b-3846-4f54-bc91-50f7bda220a9)(content(Whitespace\"\\n\"))))(Secondary((id \
         7aafad94-f1f6-4690-a0a3-dd93dcbe489b)(content(Comment\"#     == \
         \\\"\\240\\159\\146\\167\\240\\159\\146\\167 Fern: 250ml\\\" \
         end;                              #\"))))(Secondary((id \
         551e579b-df43-4df7-9100-925c6b3a13ff)(content(Whitespace\"\\n\"))))(Secondary((id \
         0480cd02-2737-4556-a581-585a4a9526e4)(content(Comment\"#   test \
         format_plant(\\\"Cactus\\\", 50)                           \
         #\"))))(Secondary((id \
         d2ca4b9e-29c1-4ed0-8b12-06ed18d3660b)(content(Whitespace\"\\n\"))))(Secondary((id \
         a0048b55-ffd9-4c08-a9f5-4d4eb99908ce)(content(Comment\"#     == \
         \\\"\\240\\159\\146\\167 Cactus: 50ml\\\" \
         end;                               #\"))))(Secondary((id \
         47313a1e-f937-406e-b92e-9ef789c33fbb)(content(Whitespace\"\\n\"))))(Secondary((id \
         3f21e177-c102-4f2c-b405-a2fffa1e8ddc)(content(Comment\"#   test \
         format_plant(\\\"Lily\\\", 200)                            \
         #\"))))(Secondary((id \
         b8a18025-6f92-49e8-bf96-c76ec224b2c7)(content(Whitespace\"\\n\"))))(Secondary((id \
         6828b5db-fd67-4eca-bfc9-d0c3c40c0ef1)(content(Comment\"#     == \
         \\\"\\240\\159\\146\\167\\240\\159\\146\\167 Lily: 200ml\\\" \
         end                               #\"))))(Secondary((id \
         3ecd1d14-6957-40ef-9810-40ca96d81f96)(content(Whitespace\"\\n\"))))(Secondary((id \
         63f86146-bb81-4d59-a5a7-efb160ed8570)(content(Comment\"#                                                             \
         #\"))))(Secondary((id \
         8a3f63b1-9665-463e-987f-c248bda3c33f)(content(Whitespace\"\\n\"))))(Secondary((id \
         6088f6cf-b9d1-47ab-ab0b-69af208231c9)(content(Comment\"# The first \
         two tests pass. The Lily test FAILS!              \
         #\"))))(Secondary((id \
         5dba2d44-3a9f-414b-af98-e67b7761074c)(content(Whitespace\"\\n\"))))(Secondary((id \
         986b0570-15f7-49f6-816f-01191f4b85f9)(content(Comment\"# Click inside \
         water_tag and look at the probe:               #\"))))(Secondary((id \
         90269837-2fa6-474f-bd73-860a4a6408b6)(content(Whitespace\"\\n\"))))(Secondary((id \
         933a5c09-de54-4bbc-9fc0-a3b7ed529c1b)(content(Comment\"# \
         water_tag(200) returns \\\"\\240\\159\\146\\167\\\" not \
         \\\"\\240\\159\\146\\167\\240\\159\\146\\167\\\".                    \
         #\"))))(Secondary((id \
         82e9a8d9-638e-4e03-8fa3-5cf119f1859c)(content(Whitespace\"\\n\"))))(Secondary((id \
         23cac459-5f8f-4a22-8252-fce45c68d865)(content(Comment\"# The > should \
         be >=. Don't fix it \\226\\128\\148 next stage improves      \
         #\"))))(Secondary((id \
         88071fc5-f58c-46bc-bf12-e6816ab4ac8a)(content(Whitespace\"\\n\"))))(Secondary((id \
         acf3cd94-460f-4e03-8b55-df2c52568c8d)(content(Comment\"# the whole \
         approach.                                         \
         #\"))))(Secondary((id \
         f94948f9-6f64-46d6-ac6a-2684935b1436)(content(Whitespace\"\\n\"))))(Secondary((id \
         b3373587-213c-4f63-aaf8-a505b2d410f3)(content(Whitespace\"\\n\"))))(Tile((id \
         4c094f05-8bdc-40ae-abf6-fd4dbe85a2d6)(label(?))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         34c0e4e8-f471-4bdf-8eb7-e702a0c678f0)(content(Whitespace\"\\n\"))))(Secondary((id \
         e63ad79a-4e62-4615-92aa-03dc5acba58a)(content(Whitespace\"\\n\"))))(Tile((id \
         3c392a8b-eee7-4add-ae6c-81f4de205228)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         fc7bb159-26dc-482f-9781-f995eda3793b)(content(Whitespace\"\\n\"))))(Secondary((id \
         388a86c0-ba38-4deb-88ac-a9324241786d)(content(Whitespace\"\\n\"))))(Secondary((id \
         71cb21dd-8ea0-4f2f-a073-37cb49361250)(content(Comment\"# \
         ============================================================ \
         #\"))))(Secondary((id \
         da31e39f-8125-4e4c-ba35-7071b6f462a1)(content(Whitespace\"\\n\"))))(Secondary((id \
         ef221d7b-fadb-45f7-89f8-b7866de19d21)(content(Comment\"# STAGE 4: A \
         TYPE AND CASE EXPRESSION                        #\"))))(Secondary((id \
         ef28296f-5ea9-4ac7-8bf6-e6d57f5a65ca)(content(Whitespace\"\\n\"))))(Secondary((id \
         55de2829-a7d7-4aee-bf2b-e7d43f829f33)(content(Comment\"# \
         ============================================================ \
         #\"))))(Secondary((id \
         d74604c3-203f-4eb3-a6c9-922cd0308eec)(content(Whitespace\"\\n\"))))(Secondary((id \
         99de39c5-b485-421e-9321-9733416b68d8)(content(Whitespace\"\\n\"))))(Secondary((id \
         b1c15a95-ce70-40fc-b01a-7017b2993931)(content(Comment\"# Two levels \
         isn't enough. Let's use three: Low, Medium,     #\"))))(Secondary((id \
         50358bee-a49a-4d35-abf3-4f004b7c2f13)(content(Whitespace\"\\n\"))))(Secondary((id \
         409f8fae-cffe-4191-88a3-4f0c35cd9700)(content(Comment\"# High. We \
         define a sum type and match with a case.          \
         #\"))))(Secondary((id \
         e24e5aff-4ed5-44d4-8fc5-450c6c0a5168)(content(Whitespace\"\\n\"))))(Secondary((id \
         b23c70d8-3347-4f8b-b370-27e6339436c3)(content(Comment\"#                                                             \
         #\"))))(Secondary((id \
         55a2af8c-b057-4c6c-8348-e899266674b0)(content(Whitespace\"\\n\"))))(Secondary((id \
         266ccf65-cc9d-4649-91e9-78da94c000a1)(content(Comment\"# `type Name = \
         + V1 + V2 + V3 in` defines a sum type.        #\"))))(Secondary((id \
         3d6d73bb-e653-4955-b20e-9eabd718a222)(content(Whitespace\"\\n\"))))(Secondary((id \
         cb39e52c-a7d9-460d-ae72-d5a337548e02)(content(Comment\"# `case expr | \
         V1 => e1 | V2 => e2 | ... end` matches.       #\"))))(Secondary((id \
         a1005361-8644-4243-b0f5-da37e46ccf31)(content(Whitespace\"\\n\"))))(Secondary((id \
         e2848b1f-1231-4f5b-b8b9-0f61cccdfb3e)(content(Comment\"#                                                             \
         #\"))))(Secondary((id \
         1cabadcc-cb03-4da3-a4d7-bf79c2ed3583)(content(Whitespace\"\\n\"))))(Secondary((id \
         4d1aa86b-23f4-4f6f-b802-45f9ef6d6e5e)(content(Comment\"# Replace the \
         ? below with:                                   #\"))))(Secondary((id \
         2d53654b-1d1f-4bd8-8db1-e672b88ecb8e)(content(Whitespace\"\\n\"))))(Secondary((id \
         8441dd22-d62a-4673-bad9-c30453dfa629)(content(Comment\"#                                                             \
         #\"))))(Secondary((id \
         034efa65-fa33-48d5-af6e-8f775b8dd9ee)(content(Whitespace\"\\n\"))))(Secondary((id \
         09cb8a65-9a9f-4017-9435-18a20e1c25d9)(content(Comment\"#   type \
         WaterLevel = + Low + Medium + High in                \
         #\"))))(Secondary((id \
         ff6fd935-9a61-4de5-b7d3-d7aa0a4d9ffd)(content(Whitespace\"\\n\"))))(Secondary((id \
         c914dffd-1bf0-451d-9500-287d66cc29f6)(content(Comment\"#                                                             \
         #\"))))(Secondary((id \
         689e5414-97f4-434f-a1f7-68331fad7557)(content(Whitespace\"\\n\"))))(Secondary((id \
         0b598062-2c14-4bdb-b453-1df62dd4d60a)(content(Comment\"#   let \
         classify: Int -> WaterLevel =                         \
         #\"))))(Secondary((id \
         1f96fa97-2794-47ae-896c-c5052985f241)(content(Whitespace\"\\n\"))))(Secondary((id \
         6243e999-9f71-4564-934b-eac71efa9901)(content(Comment\"#     fun \
         amount ->                                           \
         #\"))))(Secondary((id \
         402433f4-e211-4035-bdbb-c94574513895)(content(Whitespace\"\\n\"))))(Secondary((id \
         a3629a38-b188-4593-a6e6-726fa332a6c2)(content(Comment\"#     if \
         amount >= 200 then High                              \
         #\"))))(Secondary((id \
         5265f071-b862-4ff3-bf85-510852716bcb)(content(Whitespace\"\\n\"))))(Secondary((id \
         72c0881d-6910-49e6-a1e9-7842d235d16a)(content(Comment\"#     else if \
         amount >= 100 then Medium                       #\"))))(Secondary((id \
         36216470-72b0-4a5d-aa2b-3e4a943c9f46)(content(Whitespace\"\\n\"))))(Secondary((id \
         b8a838dc-143e-40c7-9f17-4a97c3f2b466)(content(Comment\"#     else \
         Low                                                \
         #\"))))(Secondary((id \
         f3ab3904-0902-4f10-bf43-ab56a9254c3a)(content(Whitespace\"\\n\"))))(Secondary((id \
         ee70857c-e797-4b32-b4dd-94692e126c3a)(content(Comment\"#   \
         in                                                        \
         #\"))))(Secondary((id \
         cac9f70f-fafc-467a-a312-a38a3152571f)(content(Whitespace\"\\n\"))))(Secondary((id \
         c88d808b-5f35-45a5-bad4-c683ca653aef)(content(Comment\"#                                                             \
         #\"))))(Secondary((id \
         970d5443-a978-4591-888d-2d98a717aa35)(content(Whitespace\"\\n\"))))(Secondary((id \
         f60d2424-853c-4dc4-9d88-2c402d7b7c64)(content(Comment\"#   let \
         format_plant: (String, Int) -> String =               \
         #\"))))(Secondary((id \
         6c602c5d-c98d-4d56-adbe-13167591b869)(content(Whitespace\"\\n\"))))(Secondary((id \
         84ba36de-009c-4129-b804-4a980d335e2d)(content(Comment\"#     fun \
         (name, water) ->                                    \
         #\"))))(Secondary((id \
         7a241bc1-2fa4-4676-9a43-d608e1af0fbd)(content(Whitespace\"\\n\"))))(Secondary((id \
         a7bf996a-2ad2-4846-89d1-086f64641c5a)(content(Comment\"#     let \
         emoji = case classify(water)                        \
         #\"))))(Secondary((id \
         383252ca-c1a6-417e-bdb7-26dfeebba9b7)(content(Whitespace\"\\n\"))))(Secondary((id \
         04f5e87b-c94c-48d2-a854-41f93c8d1914)(content(Comment\"#       | Low \
         => \
         \\\"\\240\\159\\140\\181\\\"                                         \
         #\"))))(Secondary((id \
         de52f61a-5088-4736-8262-935d481d1f8a)(content(Whitespace\"\\n\"))))(Secondary((id \
         96599a1b-f4c0-46fb-b972-a1a874b46fc5)(content(Comment\"#       | \
         Medium => \
         \\\"\\240\\159\\146\\167\\\"                                      \
         #\"))))(Secondary((id \
         a0feca02-2b91-45a6-b2ec-cda56ca2b3d7)(content(Whitespace\"\\n\"))))(Secondary((id \
         4f5898fb-93c3-47e1-a710-576e575ab1be)(content(Comment\"#       | High \
         => \
         \\\"\\240\\159\\146\\167\\240\\159\\146\\167\\\"                                      \
         #\"))))(Secondary((id \
         fca20874-12f1-4b33-a600-0cdd2c68e38e)(content(Whitespace\"\\n\"))))(Secondary((id \
         4ca6356b-dfc0-44eb-8ce7-678b071df2c3)(content(Comment\"#     end \
         in                                                  \
         #\"))))(Secondary((id \
         efdf04c5-274c-4ee6-950f-efe12171d6e3)(content(Whitespace\"\\n\"))))(Secondary((id \
         d814cf38-f4b2-4863-b843-2751f098ba3b)(content(Comment\"#     emoji ++ \
         \\\" \\\" ++ name ++ \\\": \\\"                            \
         #\"))))(Secondary((id \
         be3628e7-aa87-4a56-87ef-bd70b52b1545)(content(Whitespace\"\\n\"))))(Secondary((id \
         02687170-0acf-45b0-a379-825c78ccee6b)(content(Comment\"#       ++ \
         string_of_int(water) ++ \\\"ml\\\"                       \
         #\"))))(Secondary((id \
         bf05e6ef-45b7-49d4-a054-bc79b7c7caed)(content(Whitespace\"\\n\"))))(Secondary((id \
         d4d78c7e-061f-4ebd-b412-d839197f5077)(content(Comment\"#   \
         in                                                        \
         #\"))))(Secondary((id \
         60450a6c-4dcd-4984-8307-a6cfe2de4f75)(content(Whitespace\"\\n\"))))(Secondary((id \
         046417a1-1b77-4c0d-89d2-386d46881674)(content(Comment\"#                                                             \
         #\"))))(Secondary((id \
         7e2b6e23-281c-4a52-a8aa-603cd2153ced)(content(Whitespace\"\\n\"))))(Secondary((id \
         9f35423c-046d-499e-a842-58a5a5838e36)(content(Comment\"#   test \
         format_plant(\\\"Fern\\\", 250)                            \
         #\"))))(Secondary((id \
         63b4e303-4b13-49f1-8a67-83d6ffb6ccda)(content(Whitespace\"\\n\"))))(Secondary((id \
         b169dff3-9772-45fb-8fe8-f0029f71c4db)(content(Comment\"#     == \
         \\\"\\240\\159\\146\\167\\240\\159\\146\\167 Fern: 250ml\\\" \
         end;                              #\"))))(Secondary((id \
         653b9601-42b2-45a9-ad53-ec8cb583feca)(content(Whitespace\"\\n\"))))(Secondary((id \
         9737a021-131c-4294-8eb3-fe72b77153a3)(content(Comment\"#   test \
         format_plant(\\\"Orchid\\\", 180)                          \
         #\"))))(Secondary((id \
         3e09d535-0b7c-4870-ab92-be450fe1b2c5)(content(Whitespace\"\\n\"))))(Secondary((id \
         62158eb8-4072-4ebc-bc1d-37bc77b249da)(content(Comment\"#     == \
         \\\"\\240\\159\\146\\167 Orchid: 180ml\\\" \
         end;                              #\"))))(Secondary((id \
         4b6ab46c-f749-4b16-8765-a8571f720e89)(content(Whitespace\"\\n\"))))(Secondary((id \
         ecd03ceb-d450-463f-abc0-a445b5f4fe52)(content(Comment\"#   test \
         format_plant(\\\"Cactus\\\", 50)                           \
         #\"))))(Secondary((id \
         b85b2af8-d6c7-48ae-9dcd-11a54e58f7cc)(content(Whitespace\"\\n\"))))(Secondary((id \
         b54747c4-1978-43d3-b7af-222999807045)(content(Comment\"#     == \
         \\\"\\240\\159\\140\\181 Cactus: 50ml\\\" \
         end;                               #\"))))(Secondary((id \
         209d7d49-e9b2-4f56-802b-7fcd4a906206)(content(Whitespace\"\\n\"))))(Secondary((id \
         bb108ce0-15eb-49cf-899b-7821871fa4b8)(content(Comment\"#   test \
         format_plant(\\\"Lily\\\", 200)                            \
         #\"))))(Secondary((id \
         7093c2f2-d9db-4859-9a8a-549d813777b8)(content(Whitespace\"\\n\"))))(Secondary((id \
         424a45d8-700e-4318-97c2-56547199c25d)(content(Comment\"#     == \
         \\\"\\240\\159\\146\\167\\240\\159\\146\\167 Lily: 200ml\\\" \
         end                               #\"))))(Secondary((id \
         c1c186ae-9a97-4ee6-b7f4-7c99aedd77d6)(content(Whitespace\"\\n\"))))(Secondary((id \
         3ddddb49-b997-4e1f-88af-5a5a20ebf394)(content(Comment\"#                                                             \
         #\"))))(Secondary((id \
         5eb31d81-ebb3-46c1-a47c-0e677d199409)(content(Whitespace\"\\n\"))))(Secondary((id \
         486b15af-4125-4fb4-945b-a4fe212aa41a)(content(Comment\"# All four \
         pass \\226\\128\\148 including Lily! Click inside classify       \
         #\"))))(Secondary((id \
         f2a733fd-1435-43da-8229-0a6b9bc059d3)(content(Whitespace\"\\n\"))))(Secondary((id \
         afbcd07f-e5b4-4293-8394-fa786c45c51a)(content(Comment\"# in Many mode \
         to see: High, Medium, Low, High.              #\"))))(Secondary((id \
         a1bcf368-4427-42ce-b3dc-ccb5fa45d363)(content(Whitespace\"\\n\"))))(Secondary((id \
         c6df2fc7-dfe8-43d3-b82e-fe9a98c0de8d)(content(Comment\"# The >= 200 \
         threshold now handles the boundary correctly.    \
         #\"))))(Secondary((id \
         d7b84a2d-71a1-41c6-b338-50bed54ebe95)(content(Whitespace\"\\n\"))))(Secondary((id \
         b44ccf91-b7a8-4ec8-84f3-1879b8730a7a)(content(Whitespace\"\\n\"))))(Tile((id \
         e4714a1a-966b-4cc8-a6b4-d159564861fe)(label(?))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         c0cf7b45-8bf4-46df-bf67-d46217eb891e)(content(Whitespace\"\\n\"))))(Secondary((id \
         d28f413a-4f9e-4a54-a474-3bf52a4db92f)(content(Whitespace\"\\n\"))))(Secondary((id \
         befbb8d4-1aad-4ff4-80ba-97e421680b88)(content(Comment\"# \
         ============================================================ \
         #\"))))(Secondary((id \
         0c1b70ee-c7dd-4e78-8e1c-f8dd7de9b160)(content(Whitespace\"\\n\"))))(Secondary((id \
         e65d7777-522d-4f47-8fde-b8ae9ede2a1d)(content(Comment\"# \
         REVIEW                                                      \
         #\"))))(Secondary((id \
         d8ca277b-f525-4d26-8b03-0e2020121d46)(content(Whitespace\"\\n\"))))(Secondary((id \
         e8f24fe7-6c74-4650-b985-f8824a215ed1)(content(Comment\"#                                                             \
         #\"))))(Secondary((id \
         0a5c88db-b293-4342-a06b-3a195d4c0706)(content(Whitespace\"\\n\"))))(Secondary((id \
         b50d0389-8f92-4e28-8d67-3742b196dded)(content(Comment\"# You've \
         written:                                             \
         #\"))))(Secondary((id \
         fab56345-7f42-46d1-a604-2794accc1fe7)(content(Whitespace\"\\n\"))))(Secondary((id \
         9e15ea2c-8075-4c47-b228-e0e5d933dcf6)(content(Comment\"#   1. Let \
         bindings and string expressions                    \
         #\"))))(Secondary((id \
         d961c37a-e0bb-4a14-855e-da06b5c70816)(content(Whitespace\"\\n\"))))(Secondary((id \
         72a19cdc-a262-4195-bf43-5c81afeae142)(content(Comment\"#   2. A \
         function with tests                                  \
         #\"))))(Secondary((id \
         18f0ef68-1557-4630-9f9e-92da18adb34c)(content(Whitespace\"\\n\"))))(Secondary((id \
         4564aab0-e64b-4941-93f6-084368133861)(content(Comment\"#   3. A \
         helper with if/else (and caught a boundary bug)      \
         #\"))))(Secondary((id \
         4de3a837-560e-4048-bce1-6d072eafe355)(content(Whitespace\"\\n\"))))(Secondary((id \
         3129ff0e-8f5a-4aca-8da3-4b15682bc9af)(content(Comment\"#   4. A type \
         definition with case expression                 #\"))))(Secondary((id \
         463987ff-0323-489e-b962-c87e607e5247)(content(Whitespace\"\\n\"))))(Secondary((id \
         aa20eb08-9378-44dc-baa1-dda935a69cfd)(content(Comment\"#                                                             \
         #\"))))(Secondary((id \
         bd6e5f65-e13c-42a2-becf-a16be4f8b8ff)(content(Whitespace\"\\n\"))))(Secondary((id \
         151d664d-08ad-4203-b6d3-3798e97ec5ac)(content(Comment\"# At each \
         step, probes showed intermediate values inline,     \
         #\"))))(Secondary((id \
         d9171456-003f-43ab-ae6a-8039000b799b)(content(Whitespace\"\\n\"))))(Secondary((id \
         5ed71442-e083-4532-8c02-9d7a5a702528)(content(Comment\"# so you could \
         verify correctness as you went.                #\"))))(Secondary((id \
         daad8def-825d-4464-802d-b80c7f119e93)(content(Whitespace\"\\n\"))))(Secondary((id \
         8973930f-b902-4457-9fe4-a06bca1b0385)(content(Comment\"# \
         ============================================================ \
         #\"))))(Secondary((id \
         fe4cd96c-8881-4be4-8149-7e3f019b586a)(content(Whitespace\"\\n\"))))(Secondary((id \
         1720bbd9-aa7d-44b2-945f-4377e21f51e8)(content(Whitespace\"\\n\"))))(Secondary((id \
         0fc5fb8d-5e77-49c2-b4c0-3c1dc14584d4)(content(Comment\"# END \
         #\"))))(Secondary((id \
         bd6d2931-888f-4bfa-873a-f9d1e23e082e)(content(Whitespace\"\\n\")))))";
      backup_text =
        "# WRITING WITH LIVE VALUES #\n\n\
         # In this tutorial you'll write code step by step.           #\n\
         # Turn on Auto Mode (microscope icon, top right).            #\n\
         # Each stage shows you what to type, then you type it        #\n\
         # in the designated area and see probe values appear.        #\n\n\
         # ============================================================ #\n\
         # STAGE 1: LET BINDINGS AND STRINGS                          #\n\
         # ============================================================ #\n\n\
         # `let x = expr in body` binds a value. `++` concatenates    #\n\
         # strings. `string_of_int` converts a number to a string.    #\n\
         #                                                             #\n\
         # Replace the ? below with:                                   #\n\
         #                                                             #\n\
         #   let name = \"Fern\" in                                      #\n\
         #   let water = 250 in                                        #\n\
         #   let label =                                               #\n\
         #     name ++ \": \" ++ string_of_int(water) ++ \"ml\"            #\n\
         #   in                                                        #\n\
         #   label                                                     #\n\
         #                                                             #\n\
         # Then click inside your let bindings. Auto-probe should      #\n\
         # show \"Fern\", 250, and \"Fern: 250ml\". Try changing the      #\n\
         # name or number and watch the label update instantly.        #\n\n\
         ?\n\n\
         ;\n\n\
         # ============================================================ #\n\
         # STAGE 2: A FUNCTION AND TESTS                              #\n\
         # ============================================================ #\n\n\
         # Now let's make the label formatting reusable.               #\n\
         # `fun (a, b) -> body` defines a function taking a tuple.     #\n\
         # `test X == Y end` checks that X equals Y.                  #\n\
         #                                                             #\n\
         # Replace the ? below with:                                   #\n\
         #                                                             #\n\
         #   let format_plant: (String, Int) -> String =               #\n\
         #     fun (name, water) ->                                    #\n\
         #     name ++ \": \" ++ string_of_int(water) ++ \"ml\"            #\n\
         #   in                                                        #\n\
         #                                                             #\n\
         #   test format_plant(\"Fern\", 250)                            #\n\
         #     == \"Fern: 250ml\" end;                                   #\n\
         #   test format_plant(\"Orchid\", 180)                          #\n\
         #     == \"Orchid: 180ml\" end;                                 #\n\
         #   test format_plant(\"Cactus\", 50)                           #\n\
         #     == \"Cactus: 50ml\" end                                   #\n\
         #                                                             #\n\
         # Click inside format_plant to see auto-probe values for      #\n\
         # each test call. Toggle Many mode (Space) to see all         #\n\
         # three results side by side.                                 #\n\n\
         ?\n\n\
         ;\n\n\
         # ============================================================ #\n\
         # STAGE 3: A HELPER WITH IF/ELSE                             #\n\
         # ============================================================ #\n\n\
         # Plants with high water needs should get a \
         \240\159\146\167\240\159\146\167 tag.        #\n\
         # We'll write a helper function using a conditional:          #\n\
         #   `if condition then expr1 else expr2`                      #\n\
         #                                                             #\n\
         # A note on writing order: in practice, you'd first write    #\n\
         # the CALL to water_tag inside format_plant, see a hole       #\n\
         # value, then go implement the helper above it. This          #\n\
         # outside-in approach lets live values guide your writing.    #\n\
         #                                                             #\n\
         # Replace the ? below with:                                   #\n\
         #                                                             #\n\
         #   let water_tag: Int -> String =                            #\n\
         #     fun amount ->                                           #\n\
         #     if amount > 200 then \
         \"\240\159\146\167\240\159\146\167\"                             #\n\
         #     else \
         \"\240\159\146\167\"                                               #\n\
         #   in                                                        #\n\
         #                                                             #\n\
         #   let format_plant: (String, Int) -> String =               #\n\
         #     fun (name, water) ->                                    #\n\
         #     let tag = water_tag(water) in                           #\n\
         #     tag ++ \" \" ++ name ++ \": \"                              #\n\
         #       ++ string_of_int(water) ++ \"ml\"                       #\n\
         #   in                                                        #\n\
         #                                                             #\n\
         #   test format_plant(\"Fern\", 250)                            #\n\
         #     == \"\240\159\146\167\240\159\146\167 Fern: 250ml\" \
         end;                              #\n\
         #   test format_plant(\"Cactus\", 50)                           #\n\
         #     == \"\240\159\146\167 Cactus: 50ml\" \
         end;                               #\n\
         #   test format_plant(\"Lily\", 200)                            #\n\
         #     == \"\240\159\146\167\240\159\146\167 Lily: 200ml\" \
         end                               #\n\
         #                                                             #\n\
         # The first two tests pass. The Lily test FAILS!              #\n\
         # Click inside water_tag and look at the probe:               #\n\
         # water_tag(200) returns \"\240\159\146\167\" not \
         \"\240\159\146\167\240\159\146\167\".                    #\n\
         # The > should be >=. Don't fix it \226\128\148 next stage \
         improves      #\n\
         # the whole approach.                                         #\n\n\
         ?\n\n\
         ;\n\n\
         # ============================================================ #\n\
         # STAGE 4: A TYPE AND CASE EXPRESSION                        #\n\
         # ============================================================ #\n\n\
         # Two levels isn't enough. Let's use three: Low, Medium,     #\n\
         # High. We define a sum type and match with a case.          #\n\
         #                                                             #\n\
         # `type Name = + V1 + V2 + V3 in` defines a sum type.        #\n\
         # `case expr | V1 => e1 | V2 => e2 | ... end` matches.       #\n\
         #                                                             #\n\
         # Replace the ? below with:                                   #\n\
         #                                                             #\n\
         #   type WaterLevel = + Low + Medium + High in                #\n\
         #                                                             #\n\
         #   let classify: Int -> WaterLevel =                         #\n\
         #     fun amount ->                                           #\n\
         #     if amount >= 200 then High                              #\n\
         #     else if amount >= 100 then Medium                       #\n\
         #     else Low                                                #\n\
         #   in                                                        #\n\
         #                                                             #\n\
         #   let format_plant: (String, Int) -> String =               #\n\
         #     fun (name, water) ->                                    #\n\
         #     let emoji = case classify(water)                        #\n\
         #       | Low => \
         \"\240\159\140\181\"                                         #\n\
         #       | Medium => \
         \"\240\159\146\167\"                                      #\n\
         #       | High => \
         \"\240\159\146\167\240\159\146\167\"                                      \
         #\n\
         #     end in                                                  #\n\
         #     emoji ++ \" \" ++ name ++ \": \"                            #\n\
         #       ++ string_of_int(water) ++ \"ml\"                       #\n\
         #   in                                                        #\n\
         #                                                             #\n\
         #   test format_plant(\"Fern\", 250)                            #\n\
         #     == \"\240\159\146\167\240\159\146\167 Fern: 250ml\" \
         end;                              #\n\
         #   test format_plant(\"Orchid\", 180)                          #\n\
         #     == \"\240\159\146\167 Orchid: 180ml\" \
         end;                              #\n\
         #   test format_plant(\"Cactus\", 50)                           #\n\
         #     == \"\240\159\140\181 Cactus: 50ml\" \
         end;                               #\n\
         #   test format_plant(\"Lily\", 200)                            #\n\
         #     == \"\240\159\146\167\240\159\146\167 Lily: 200ml\" \
         end                               #\n\
         #                                                             #\n\
         # All four pass \226\128\148 including Lily! Click inside \
         classify       #\n\
         # in Many mode to see: High, Medium, Low, High.              #\n\
         # The >= 200 threshold now handles the boundary correctly.    #\n\n\
         ?\n\n\
         # ============================================================ #\n\
         # REVIEW                                                      #\n\
         #                                                             #\n\
         # You've written:                                             #\n\
         #   1. Let bindings and string expressions                    #\n\
         #   2. A function with tests                                  #\n\
         #   3. A helper with if/else (and caught a boundary bug)      #\n\
         #   4. A type definition with case expression                 #\n\
         #                                                             #\n\
         # At each step, probes showed intermediate values inline,     #\n\
         # so you could verify correctness as you went.                #\n\
         # ============================================================ #\n\n\
         # END #\n";
      refractors = "()";
    } )
