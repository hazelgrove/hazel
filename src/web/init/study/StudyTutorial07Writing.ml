let out : string * Haz3lcore.PersistentSegment.t =
  ( "Study / tutorial / 07-writing",
    {
      segment =
        "((Secondary((id \
         c457ac92-6397-4b4a-b33b-af475fb00f74)(content(Comment\"# WRITING WITH \
         LIVE VALUES #\"))))(Secondary((id \
         8e98700a-2324-4f62-8512-87f6d249cd16)(content(Whitespace\"\\n\"))))(Secondary((id \
         23344507-21e2-4c80-8c04-3b8d76a12610)(content(Whitespace\"\\n\"))))(Secondary((id \
         dc5dcf9c-e603-466c-932d-2a30b9a47211)(content(Comment\"# In this \
         tutorial you'll write code step by step.           \
         #\"))))(Secondary((id \
         4d191345-aaf0-4baa-b78f-19804b87e242)(content(Whitespace\"\\n\"))))(Secondary((id \
         ffdca48d-aa0c-419b-8859-8c98afcd6418)(content(Comment\"# Turn on Auto \
         Mode (microscope icon, top right).            #\"))))(Secondary((id \
         43eab599-d633-4a0b-b895-58154ff8c277)(content(Whitespace\"\\n\"))))(Secondary((id \
         e773b3a7-ad3c-4cec-808f-cae68997819e)(content(Comment\"# Each stage \
         shows you what to type, then you type it        #\"))))(Secondary((id \
         bb0f6da5-b67e-467f-8f5e-8b9a2f7f9589)(content(Whitespace\"\\n\"))))(Secondary((id \
         81588362-d497-4804-9d4d-9cda32d30a28)(content(Comment\"# in the \
         designated area and see probe values appear.        \
         #\"))))(Secondary((id \
         0a66c59e-1f93-4ec5-bf86-12a240603f1b)(content(Whitespace\"\\n\"))))(Secondary((id \
         7ca460e9-6f5a-45cf-a6c6-c8cb45f0eef1)(content(Whitespace\"\\n\"))))(Secondary((id \
         cbb2fd25-3141-4980-af6f-4acff1deaa8d)(content(Comment\"# \
         ============================================================ \
         #\"))))(Secondary((id \
         5610e71a-8120-4d07-bd98-51519af2957c)(content(Whitespace\"\\n\"))))(Secondary((id \
         5ab6d5b2-4858-4ba8-8536-3042ec25ca2a)(content(Comment\"# STAGE 1: LET \
         BINDINGS AND STRINGS                          #\"))))(Secondary((id \
         1d60e982-1da3-4e8c-83b9-f928ad25fed6)(content(Whitespace\"\\n\"))))(Secondary((id \
         0917f441-5540-42f8-8a35-38cb91ed129e)(content(Comment\"# \
         ============================================================ \
         #\"))))(Secondary((id \
         e142b0b7-d4fd-4531-9f5c-7da9aae5834e)(content(Whitespace\"\\n\"))))(Secondary((id \
         b5380fab-af51-4f45-b945-bb4e34f871c3)(content(Whitespace\"\\n\"))))(Secondary((id \
         820a777c-a912-42b2-94e7-ab9efdc28c14)(content(Comment\"# `let x = \
         expr in body` binds a value. `++` concatenates    \
         #\"))))(Secondary((id \
         5efb918b-8db4-4b4c-9739-2b9d8e01f726)(content(Whitespace\"\\n\"))))(Secondary((id \
         148b6046-f0f1-4bec-ba06-a22c1334c9a2)(content(Comment\"# strings. \
         `string_of_int` converts a number to a string.    \
         #\"))))(Secondary((id \
         bf38752f-cefa-4c41-87c0-c32c9821bc0d)(content(Whitespace\"\\n\"))))(Secondary((id \
         d471b053-a43b-4cec-a3f5-5a6ccdd58778)(content(Comment\"#                                                             \
         #\"))))(Secondary((id \
         9b3c7f30-d078-4c01-9200-224c3b65d04b)(content(Whitespace\"\\n\"))))(Secondary((id \
         f5853b0f-f39a-4f71-adc9-192c6b5b6b98)(content(Comment\"# Replace the \
         ? below with:                                   #\"))))(Secondary((id \
         a28e892e-3c97-4964-a132-b8793d802163)(content(Whitespace\"\\n\"))))(Secondary((id \
         9b4ea73a-85c8-4e4f-aae8-ecbc368b4a8a)(content(Comment\"#                                                             \
         #\"))))(Secondary((id \
         1f505cf6-1447-4328-80e8-1e61d1ae519f)(content(Whitespace\"\\n\"))))(Secondary((id \
         85cedb5c-b259-4869-bcef-1b98c73b4db9)(content(Comment\"#   let name = \
         \\\"Fern\\\" in                                      \
         #\"))))(Secondary((id \
         8f51e1ab-15c8-42b7-86bb-85fdbc2b7ba1)(content(Whitespace\"\\n\"))))(Secondary((id \
         1a4ddb28-04d8-47c1-ab37-85df2aa4dcda)(content(Comment\"#   let water \
         = 250 in                                        #\"))))(Secondary((id \
         4f1f2a92-1fce-4f31-af2b-a659b339ea0f)(content(Whitespace\"\\n\"))))(Secondary((id \
         71df4be6-7b27-4bcf-b978-d628f08a236a)(content(Comment\"#   let label \
         =                                               #\"))))(Secondary((id \
         eb046bed-c74b-4952-9fad-1f23d9e1003b)(content(Whitespace\"\\n\"))))(Secondary((id \
         71d3db32-15c4-4db7-b72c-ceb71ccef908)(content(Comment\"#     name ++ \
         \\\": \\\" ++ string_of_int(water) ++ \\\"ml\\\"            \
         #\"))))(Secondary((id \
         404e4a08-b5fe-4003-a23b-fe3c64fb191b)(content(Whitespace\"\\n\"))))(Secondary((id \
         83438957-effe-46e2-b42c-9a93f2ae8b84)(content(Comment\"#   \
         in                                                        \
         #\"))))(Secondary((id \
         4d01a213-a6eb-4928-b1ef-7fe45b9da8b2)(content(Whitespace\"\\n\"))))(Secondary((id \
         e4fef423-ae63-420f-b364-e569473145b7)(content(Comment\"#   \
         label                                                     \
         #\"))))(Secondary((id \
         9f20daf0-ca04-4019-8644-88dee9420d62)(content(Whitespace\"\\n\"))))(Secondary((id \
         0045de74-ea98-488c-9257-17788e12553c)(content(Comment\"#                                                             \
         #\"))))(Secondary((id \
         650ef93f-fbda-44eb-acab-b95299384717)(content(Whitespace\"\\n\"))))(Secondary((id \
         4eef1a2b-0ca8-4322-a02d-8dcfea2b6c62)(content(Comment\"# Then click \
         inside your let bindings. Auto-probe should      \
         #\"))))(Secondary((id \
         0e339d70-e3b1-470a-a8d3-d7a4ac148b0b)(content(Whitespace\"\\n\"))))(Secondary((id \
         0cb15c52-755a-415e-95ea-5defe31cd4a3)(content(Comment\"# show \
         \\\"Fern\\\", 250, and \\\"Fern: 250ml\\\". Try changing the      \
         #\"))))(Secondary((id \
         8cda47b8-8aef-42d4-9807-6a7e2274585a)(content(Whitespace\"\\n\"))))(Secondary((id \
         0668c339-dd12-4365-ad69-12d792178a8e)(content(Comment\"# name or \
         number and watch the label update instantly.        \
         #\"))))(Secondary((id \
         c3dfa1f4-6825-48ee-ab01-fa21adcbcc8a)(content(Whitespace\"\\n\"))))(Secondary((id \
         6c51190e-94dc-4f0a-be60-616fd71e9791)(content(Whitespace\"\\n\"))))(Tile((id \
         e9a96152-bba0-4677-9805-396ef1724005)(label(?))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         6dce6196-f610-4e8a-968f-c071438fb3ba)(content(Whitespace\"\\n\"))))(Secondary((id \
         62ade7ae-0af6-4f0d-ab23-cab172226967)(content(Whitespace\"\\n\"))))(Tile((id \
         fad1240e-e668-4233-ab19-8ac56deef280)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         9c9e7934-9671-4ec6-ad63-8a48d288cc41)(content(Whitespace\"\\n\"))))(Secondary((id \
         bbc5b317-1ba5-44fa-8889-6113846b27dd)(content(Whitespace\"\\n\"))))(Secondary((id \
         95a2778f-e568-449a-8885-4ec58147734d)(content(Comment\"# \
         ============================================================ \
         #\"))))(Secondary((id \
         d8487e98-3571-4a9b-8e7a-3cae8032703c)(content(Whitespace\"\\n\"))))(Secondary((id \
         29e90b1c-4775-440e-8934-9c7c91db85dd)(content(Comment\"# STAGE 2: A \
         FUNCTION AND TESTS                              #\"))))(Secondary((id \
         316d657a-7efd-4770-ad4b-9792fc7872d3)(content(Whitespace\"\\n\"))))(Secondary((id \
         b838a68e-3a89-401d-82a0-a810f46e462f)(content(Comment\"# \
         ============================================================ \
         #\"))))(Secondary((id \
         5a3e6031-e775-424a-a0cf-916b8d4c026a)(content(Whitespace\"\\n\"))))(Secondary((id \
         b6764cad-d4d0-4c71-9d33-cfc8bf20a0bc)(content(Whitespace\"\\n\"))))(Secondary((id \
         52874e6c-c098-4381-99c0-0f774d91d6ff)(content(Comment\"# Now let's \
         make the label formatting reusable.               \
         #\"))))(Secondary((id \
         13d6d0c9-37dc-4444-a0e9-116ac032067c)(content(Whitespace\"\\n\"))))(Secondary((id \
         1eca41a9-9ab3-478a-b9a0-ecd09223330c)(content(Comment\"# `fun (a, b) \
         -> body` defines a function taking a tuple.     #\"))))(Secondary((id \
         c639ac01-352b-42b8-b801-f74a1b1d7ca3)(content(Whitespace\"\\n\"))))(Secondary((id \
         f5124f1a-0b8c-45dd-9870-f0a7853b8d25)(content(Comment\"# `test X == Y \
         end` checks that X equals Y.                  #\"))))(Secondary((id \
         ef0c584b-0e7c-4134-9ea1-0ed65ea91f2a)(content(Whitespace\"\\n\"))))(Secondary((id \
         8afca428-e478-44fb-89f8-e4c88bb6ce52)(content(Comment\"#                                                             \
         #\"))))(Secondary((id \
         b141a17a-04e2-4391-a741-59121e8ca241)(content(Whitespace\"\\n\"))))(Secondary((id \
         3c1bc3fe-b635-418c-965d-03cd6802b32e)(content(Comment\"# Replace the \
         ? below with:                                   #\"))))(Secondary((id \
         593cfe0c-0200-4d18-b5c1-f6d9cc5a7ed3)(content(Whitespace\"\\n\"))))(Secondary((id \
         a4ff9ca3-c7e9-4b63-993b-c924d431defd)(content(Comment\"#                                                             \
         #\"))))(Secondary((id \
         d5b3c053-98ad-4fc8-907a-e94db1d87eab)(content(Whitespace\"\\n\"))))(Secondary((id \
         54bc1f29-b345-416f-9acc-b1c3eeb68787)(content(Comment\"#   let \
         format_plant: (String, Int) -> String =               \
         #\"))))(Secondary((id \
         40a798e6-f48a-474b-81e4-6e6f667c33e7)(content(Whitespace\"\\n\"))))(Secondary((id \
         b68ff3e4-ee3e-4f2f-8f99-0bf9d2f92f31)(content(Comment\"#     fun \
         (name, water) ->                                    \
         #\"))))(Secondary((id \
         146f69e6-27a1-4e43-a5a6-31e148460658)(content(Whitespace\"\\n\"))))(Secondary((id \
         f11fed0d-1b5c-4f16-a391-fedfddf6dc6c)(content(Comment\"#     name ++ \
         \\\": \\\" ++ string_of_int(water) ++ \\\"ml\\\"            \
         #\"))))(Secondary((id \
         8ae435ab-61ad-4d6e-b86d-dbc007c71c6e)(content(Whitespace\"\\n\"))))(Secondary((id \
         eceae982-fe9d-4851-9f9d-6cb280e6f7b5)(content(Comment\"#   \
         in                                                        \
         #\"))))(Secondary((id \
         3af17a71-11f2-4f6c-9437-6de47f4c38d3)(content(Whitespace\"\\n\"))))(Secondary((id \
         2ea4b40d-12cc-4f0c-a80b-2b9beb403f69)(content(Comment\"#                                                             \
         #\"))))(Secondary((id \
         818323d2-8e5d-4036-ab5a-3968f7bdcf68)(content(Whitespace\"\\n\"))))(Secondary((id \
         7f6be9a4-ffd0-428d-be61-d08b94c09c7c)(content(Comment\"#   test \
         format_plant(\\\"Fern\\\", 250)                            \
         #\"))))(Secondary((id \
         07614458-3b9b-48c0-a039-2e62bee05056)(content(Whitespace\"\\n\"))))(Secondary((id \
         629f8a83-4066-4493-9f31-0ab0e5680a26)(content(Comment\"#     == \
         \\\"Fern: 250ml\\\" end;                                   \
         #\"))))(Secondary((id \
         a3caa35c-b9b6-4c00-98a1-103d048a079e)(content(Whitespace\"\\n\"))))(Secondary((id \
         35223232-53b0-4544-a45c-cec8873531cf)(content(Comment\"#   test \
         format_plant(\\\"Orchid\\\", 180)                          \
         #\"))))(Secondary((id \
         93b0ecf1-6005-4302-aecd-e225479b549d)(content(Whitespace\"\\n\"))))(Secondary((id \
         32463943-8dba-47a3-a338-1ef7b1094b4c)(content(Comment\"#     == \
         \\\"Orchid: 180ml\\\" end;                                 \
         #\"))))(Secondary((id \
         16341804-00d4-4506-b43e-8dada645c8de)(content(Whitespace\"\\n\"))))(Secondary((id \
         ce7b4f12-498d-4353-9f7c-bf9882d59a10)(content(Comment\"#   test \
         format_plant(\\\"Cactus\\\", 50)                           \
         #\"))))(Secondary((id \
         de6c58f3-d8f3-48b7-ac4c-55f576cbe2ac)(content(Whitespace\"\\n\"))))(Secondary((id \
         07f1d646-2242-48bf-af42-cfaf804f85b1)(content(Comment\"#     == \
         \\\"Cactus: 50ml\\\" end                                   \
         #\"))))(Secondary((id \
         a2062e06-f51f-4630-b1b5-bab951b8eadd)(content(Whitespace\"\\n\"))))(Secondary((id \
         2f80018b-1ac9-4156-9d46-8707bc5a40ab)(content(Comment\"#                                                             \
         #\"))))(Secondary((id \
         827f7540-16f0-4129-bc9b-1a5016600cf7)(content(Whitespace\"\\n\"))))(Secondary((id \
         4e75928f-f474-40fe-8632-d92f1f84426f)(content(Comment\"# Click inside \
         format_plant to see auto-probe values for      #\"))))(Secondary((id \
         89984bce-4d03-46d1-935a-927e79be9ac3)(content(Whitespace\"\\n\"))))(Secondary((id \
         582ef4c4-7b9d-4667-9342-5e96402e7bc6)(content(Comment\"# each test \
         call. Toggle Many mode (Space) to see all         \
         #\"))))(Secondary((id \
         6ee6c0e2-4feb-4965-b477-d9199957a568)(content(Whitespace\"\\n\"))))(Secondary((id \
         6bf1d51e-aede-45cf-a552-73097178aa43)(content(Comment\"# three \
         results side by side.                                 \
         #\"))))(Secondary((id \
         0f22960d-fed3-4d3f-be9a-c276b016c24e)(content(Whitespace\"\\n\"))))(Secondary((id \
         62504dd8-01c3-4331-a4f7-034747a83653)(content(Whitespace\"\\n\"))))(Tile((id \
         c6f3b4b9-9a37-4c6f-9703-20a81a47055e)(label(?))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         2075ce31-3668-4af0-a5ee-a8fec14f6a13)(content(Whitespace\"\\n\"))))(Secondary((id \
         2a5e551c-aca5-47d0-a907-1802cd782da1)(content(Whitespace\"\\n\"))))(Tile((id \
         7822dcd6-a6c0-43ea-93ab-8c6d5e9181c1)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         9e4d147b-3340-4c3f-88f3-9287211641f9)(content(Whitespace\"\\n\"))))(Secondary((id \
         7f585798-af4f-41e7-b017-e0c6eddd7b19)(content(Whitespace\"\\n\"))))(Secondary((id \
         3170ae27-f2f1-477a-8697-24eab2ddefae)(content(Comment\"# \
         ============================================================ \
         #\"))))(Secondary((id \
         2625f8b5-64f2-4be2-ab5b-3828dff81d47)(content(Whitespace\"\\n\"))))(Secondary((id \
         31e0f724-e622-4cfb-b45a-176ccf8de833)(content(Comment\"# STAGE 3: A \
         HELPER WITH IF/ELSE                             #\"))))(Secondary((id \
         bb218826-11a1-43f0-9e19-30ce3b3a7324)(content(Whitespace\"\\n\"))))(Secondary((id \
         1adc0310-df1b-4a5d-bf95-6467995dbdf3)(content(Comment\"# \
         ============================================================ \
         #\"))))(Secondary((id \
         1b36f0ca-1928-4cfe-8546-a9855b5beae7)(content(Whitespace\"\\n\"))))(Secondary((id \
         b01b1cd6-dede-445c-bbc7-1e0c670f7e36)(content(Whitespace\"\\n\"))))(Secondary((id \
         a759e35c-3161-4e21-b274-215cc32ea176)(content(Comment\"# Plants with \
         high water needs should get a \
         \\240\\159\\146\\167\\240\\159\\146\\167 tag.        \
         #\"))))(Secondary((id \
         4e7ce6ee-50f1-42fc-b3e8-4c9ca4844082)(content(Whitespace\"\\n\"))))(Secondary((id \
         9616ad1c-ad01-4805-b215-d7cc2cac2fe1)(content(Comment\"# We'll write \
         a helper function using a conditional:          #\"))))(Secondary((id \
         f519d55d-89e9-4a5f-aa38-b2f4a5dbc0d9)(content(Whitespace\"\\n\"))))(Secondary((id \
         ece12cb5-fefc-45da-8fc9-e631016810df)(content(Comment\"#   `if \
         condition then expr1 else expr2`                      \
         #\"))))(Secondary((id \
         84149e28-2e97-4ff9-93d7-9d752e00324d)(content(Whitespace\"\\n\"))))(Secondary((id \
         8395d478-8a70-4dea-afe7-ab21df8680dc)(content(Comment\"#                                                             \
         #\"))))(Secondary((id \
         d7a31e26-c9fb-4649-83c0-708b36a47fbe)(content(Whitespace\"\\n\"))))(Secondary((id \
         a852cc56-4208-47ba-b574-aaa05660d319)(content(Comment\"# A note on \
         writing order: in practice, you'd first write    \
         #\"))))(Secondary((id \
         4e15c5ca-c644-4102-940c-0af8a02c8b1c)(content(Whitespace\"\\n\"))))(Secondary((id \
         ec2875c3-a3d1-4d5d-92d4-23fcfac2ddc0)(content(Comment\"# the CALL to \
         water_tag inside format_plant, see a hole       #\"))))(Secondary((id \
         e54b27e1-3859-4233-a87f-97ebee7029ae)(content(Whitespace\"\\n\"))))(Secondary((id \
         c55ad050-58a6-4c54-8c23-efbf8168155d)(content(Comment\"# value, then \
         go implement the helper above it. This          #\"))))(Secondary((id \
         582a660b-8918-47a3-be22-6fc4ffbae243)(content(Whitespace\"\\n\"))))(Secondary((id \
         bb0ab226-ff7f-4420-99b9-9003acc7c68a)(content(Comment\"# outside-in \
         approach lets live values guide your writing.    \
         #\"))))(Secondary((id \
         9ce38068-c367-4392-b635-fc8060d3e4a3)(content(Whitespace\"\\n\"))))(Secondary((id \
         5fbb28b0-4673-4d99-b5a9-581e321d3a69)(content(Comment\"#                                                             \
         #\"))))(Secondary((id \
         8a898c98-7b56-4681-a431-b9f73b2edc0a)(content(Whitespace\"\\n\"))))(Secondary((id \
         d0aca26f-ca0c-4249-af1c-15865e9f18cb)(content(Comment\"# Replace the \
         ? below with:                                   #\"))))(Secondary((id \
         45b9b37c-0ca1-475f-95fa-534dfdb2e99b)(content(Whitespace\"\\n\"))))(Secondary((id \
         1f8eaab9-8493-41df-967f-3fc98a0bb701)(content(Comment\"#                                                             \
         #\"))))(Secondary((id \
         e89290ac-dab8-4aba-8c0b-9a9e0a15c4ff)(content(Whitespace\"\\n\"))))(Secondary((id \
         a43ddfd2-1d3f-4eb0-9ee2-df50d3b36a6b)(content(Comment\"#   let \
         water_tag: Int -> String =                            \
         #\"))))(Secondary((id \
         593f9cf1-8504-43c7-8cc8-fd5b62d35fb1)(content(Whitespace\"\\n\"))))(Secondary((id \
         6c1c7819-480a-40cd-87ab-76f589068ce0)(content(Comment\"#     fun \
         amount ->                                           \
         #\"))))(Secondary((id \
         ca85b985-a985-4949-a14d-36cb160bd74e)(content(Whitespace\"\\n\"))))(Secondary((id \
         34f8b745-211e-47df-96d9-954aa0cff72a)(content(Comment\"#     if \
         amount > 200 then \
         \\\"\\240\\159\\146\\167\\240\\159\\146\\167\\\"                             \
         #\"))))(Secondary((id \
         0ddf62a3-fccb-4d18-bb78-e6e74080d5c9)(content(Whitespace\"\\n\"))))(Secondary((id \
         f6432e31-3cca-4991-8cbe-c00f81c0ee2d)(content(Comment\"#     else \
         \\\"\\240\\159\\146\\167\\\"                                               \
         #\"))))(Secondary((id \
         28ade30b-efe5-4d9e-9f9e-6f47651abe4f)(content(Whitespace\"\\n\"))))(Secondary((id \
         d772d3b6-e485-4d66-af03-9ab35b7c25ec)(content(Comment\"#   \
         in                                                        \
         #\"))))(Secondary((id \
         294026f4-92c9-4212-8eea-70a44859c687)(content(Whitespace\"\\n\"))))(Secondary((id \
         e5621c1c-fb4c-4f87-bca4-938db70d4cc9)(content(Comment\"#                                                             \
         #\"))))(Secondary((id \
         ab10904b-0906-4485-8822-e3ea18a67960)(content(Whitespace\"\\n\"))))(Secondary((id \
         19d697f1-97dd-4e0c-aa44-681e6709a955)(content(Comment\"#   let \
         format_plant: (String, Int) -> String =               \
         #\"))))(Secondary((id \
         21aee956-31f4-42b1-8517-a705f6ea709a)(content(Whitespace\"\\n\"))))(Secondary((id \
         c3456333-48dc-4a5a-9aa7-8abd60ae9f85)(content(Comment\"#     fun \
         (name, water) ->                                    \
         #\"))))(Secondary((id \
         5124fe82-69b3-46c0-9171-dba5e8b81702)(content(Whitespace\"\\n\"))))(Secondary((id \
         9c2cee24-40e3-4270-89ec-df922db81b77)(content(Comment\"#     let tag \
         = water_tag(water) in                           #\"))))(Secondary((id \
         ed8281af-391c-437c-ae7b-b271b3206c5c)(content(Whitespace\"\\n\"))))(Secondary((id \
         c3a87603-96b0-44c7-a836-099fc3db7a99)(content(Comment\"#     tag ++ \
         \\\" \\\" ++ name ++ \\\": \\\"                              \
         #\"))))(Secondary((id \
         d33531b9-277a-40b0-a2bc-e98ba53c07ba)(content(Whitespace\"\\n\"))))(Secondary((id \
         5e4d7f21-3e0e-47b9-ab60-a08201bbbdbf)(content(Comment\"#       ++ \
         string_of_int(water) ++ \\\"ml\\\"                       \
         #\"))))(Secondary((id \
         7d56fcc8-aaf8-4147-b9dc-360fd0c7664e)(content(Whitespace\"\\n\"))))(Secondary((id \
         bc998eee-4a17-4422-bc8a-11c526a14716)(content(Comment\"#   \
         in                                                        \
         #\"))))(Secondary((id \
         ce34289e-d656-4766-937c-37afa25d155c)(content(Whitespace\"\\n\"))))(Secondary((id \
         7f36b3a1-445f-41e3-8876-ff7cf7bf1f99)(content(Comment\"#                                                             \
         #\"))))(Secondary((id \
         f8227d8b-4103-4ac3-9441-ca5c231af1d5)(content(Whitespace\"\\n\"))))(Secondary((id \
         926a8240-c87c-49b2-ba0c-01ac387214da)(content(Comment\"#   test \
         format_plant(\\\"Fern\\\", 250)                            \
         #\"))))(Secondary((id \
         26dfa9bb-fae1-41bd-ac68-696755188c17)(content(Whitespace\"\\n\"))))(Secondary((id \
         defa812f-d89f-4925-8564-3bba4f7783c8)(content(Comment\"#     == \
         \\\"\\240\\159\\146\\167\\240\\159\\146\\167 Fern: 250ml\\\" \
         end;                              #\"))))(Secondary((id \
         2a25b994-bbdf-431d-9ef8-ee5232d12547)(content(Whitespace\"\\n\"))))(Secondary((id \
         730be2ab-bc61-4273-8a27-d6ec5f2f1abe)(content(Comment\"#   test \
         format_plant(\\\"Cactus\\\", 50)                           \
         #\"))))(Secondary((id \
         0c545255-70b9-453e-b94a-a2cc2a9b66e7)(content(Whitespace\"\\n\"))))(Secondary((id \
         40fdb23a-4145-464f-9b17-1842d0751e4e)(content(Comment\"#     == \
         \\\"\\240\\159\\146\\167 Cactus: 50ml\\\" \
         end;                               #\"))))(Secondary((id \
         efcb8aba-6b03-400f-9946-2ce9c8fc386d)(content(Whitespace\"\\n\"))))(Secondary((id \
         2ddb5bf5-29da-4c62-b2c0-1d40ffb45485)(content(Comment\"#   test \
         format_plant(\\\"Lily\\\", 200)                            \
         #\"))))(Secondary((id \
         6420b98f-2afd-4ab8-9233-5088c75319bb)(content(Whitespace\"\\n\"))))(Secondary((id \
         79ca7a0b-83dc-4de9-84d7-626f21026a1e)(content(Comment\"#     == \
         \\\"\\240\\159\\146\\167\\240\\159\\146\\167 Lily: 200ml\\\" \
         end                               #\"))))(Secondary((id \
         e8e280f2-ae9b-4cd4-984b-ca273e2d3189)(content(Whitespace\"\\n\"))))(Secondary((id \
         d71c01e9-3068-4ffe-84f3-b699b01a0ea5)(content(Comment\"#                                                             \
         #\"))))(Secondary((id \
         9234fccb-2770-4a59-aeaa-73803e4624ed)(content(Whitespace\"\\n\"))))(Secondary((id \
         3bfab271-5dc1-4745-9396-948c7158ed97)(content(Comment\"# The first \
         two tests pass. The Lily test FAILS!              \
         #\"))))(Secondary((id \
         a3bad21d-4188-4b44-9b36-d3cad622dc82)(content(Whitespace\"\\n\"))))(Secondary((id \
         34b447ab-7677-4ae3-ba16-98cdfef26421)(content(Comment\"# Click inside \
         water_tag and look at the probe:               #\"))))(Secondary((id \
         99a99a53-e624-4012-bc09-1f2b59cee09b)(content(Whitespace\"\\n\"))))(Secondary((id \
         c851e3ab-ce9d-42fe-8a41-d214fbea9e88)(content(Comment\"# \
         water_tag(200) returns \\\"\\240\\159\\146\\167\\\" not \
         \\\"\\240\\159\\146\\167\\240\\159\\146\\167\\\".                    \
         #\"))))(Secondary((id \
         c29689cb-bd73-4d30-aa59-b510552df3c4)(content(Whitespace\"\\n\"))))(Secondary((id \
         ccd5a6e9-3076-46ea-93d3-8c57a9e56842)(content(Comment\"# The > should \
         be >=. Don't fix it \\226\\128\\148 next stage improves      \
         #\"))))(Secondary((id \
         82e95a9b-0a5d-45cd-b422-c02e663bcab7)(content(Whitespace\"\\n\"))))(Secondary((id \
         e8a55387-185f-4360-a647-fdb9fb45bb51)(content(Comment\"# the whole \
         approach.                                         \
         #\"))))(Secondary((id \
         cce8d1d3-80c9-4fa5-bedc-9eb43c448716)(content(Whitespace\"\\n\"))))(Secondary((id \
         3e73ba58-744d-4bdd-a1f1-94f6842ea580)(content(Whitespace\"\\n\"))))(Tile((id \
         1c633f5c-b61b-439d-b8c1-df81446f071b)(label(?))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         1808c0c9-5dcc-46ed-a1d2-0d9c4199baf6)(content(Whitespace\"\\n\"))))(Secondary((id \
         a8931292-662d-4d3f-9a02-ef5e5cf24a4d)(content(Whitespace\"\\n\"))))(Tile((id \
         9365a334-b07d-42ff-a553-867944e0078f)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         9a4e349b-4b0d-4501-aaa8-fd5e298d3759)(content(Whitespace\"\\n\"))))(Secondary((id \
         f8268e92-1a0b-4a12-b85b-5a66e018fbf4)(content(Whitespace\"\\n\"))))(Secondary((id \
         59d7bc30-aabb-47b2-b217-f29e9e805d80)(content(Comment\"# \
         ============================================================ \
         #\"))))(Secondary((id \
         ecab87b5-a9f8-49d8-86d1-ad36d719f772)(content(Whitespace\"\\n\"))))(Secondary((id \
         1e20b23b-0f19-48d8-8b65-a0b13b0bd41f)(content(Comment\"# STAGE 4: A \
         TYPE AND CASE EXPRESSION                        #\"))))(Secondary((id \
         fb58069c-9708-4749-8b45-5349039af980)(content(Whitespace\"\\n\"))))(Secondary((id \
         e01249c7-c436-4f01-bdca-2c46b369e1a0)(content(Comment\"# \
         ============================================================ \
         #\"))))(Secondary((id \
         1a5cfd4f-38b4-468e-96b4-95ff531dc4c1)(content(Whitespace\"\\n\"))))(Secondary((id \
         04511820-375e-4b1f-b8be-aec5757a46a5)(content(Whitespace\"\\n\"))))(Secondary((id \
         6004f1ca-2539-4c24-ac58-a1bde66120dc)(content(Comment\"# Two levels \
         isn't enough. Let's use three: Low, Medium,     #\"))))(Secondary((id \
         12834dff-4d1f-49d0-b046-b37b0c3ca151)(content(Whitespace\"\\n\"))))(Secondary((id \
         0f874f9a-ebb8-4f42-abae-6149a0493ee4)(content(Comment\"# High. We \
         define a sum type and match with a case.          \
         #\"))))(Secondary((id \
         da71184b-e3a9-4df9-b8d7-28491e66afe5)(content(Whitespace\"\\n\"))))(Secondary((id \
         b0a9a645-c70c-4f8e-a4c1-fb580a143933)(content(Comment\"#                                                             \
         #\"))))(Secondary((id \
         1ad45db2-8cb8-4e83-9e13-c8cf837f58ea)(content(Whitespace\"\\n\"))))(Secondary((id \
         2b027ca2-afab-4ef8-91c3-ef128cd3cf65)(content(Comment\"# `type Name = \
         + V1 + V2 + V3 in` defines a sum type.        #\"))))(Secondary((id \
         66702354-54fa-49d8-ac6c-ee1854c06285)(content(Whitespace\"\\n\"))))(Secondary((id \
         77a8fdaa-136a-4a25-9733-18712f06b75f)(content(Comment\"# `case expr | \
         V1 => e1 | V2 => e2 | ... end` matches.       #\"))))(Secondary((id \
         c783dd04-033d-4985-9e87-3c7ecf4e6f25)(content(Whitespace\"\\n\"))))(Secondary((id \
         2c40771f-a16c-4ca8-9cd7-75d197ae1202)(content(Comment\"#                                                             \
         #\"))))(Secondary((id \
         96d57ec7-8860-40a6-8b13-7650897e166f)(content(Whitespace\"\\n\"))))(Secondary((id \
         1d1c8673-3486-4ba6-b284-9adb14679b98)(content(Comment\"# Replace the \
         ? below with:                                   #\"))))(Secondary((id \
         8298b7e2-61b1-4b6f-8544-8270ec39e1ed)(content(Whitespace\"\\n\"))))(Secondary((id \
         f38614b3-e960-42b8-871d-2c53525b7b30)(content(Comment\"#                                                             \
         #\"))))(Secondary((id \
         59339116-7ad1-4bfc-95fe-25a9e8ce2914)(content(Whitespace\"\\n\"))))(Secondary((id \
         ed47c4ad-15e4-4d1b-afa1-95ddf79d436c)(content(Comment\"#   type \
         WaterLevel = + Low + Medium + High in                \
         #\"))))(Secondary((id \
         459ab096-628a-4a88-93a9-e65c3e66dfb9)(content(Whitespace\"\\n\"))))(Secondary((id \
         dbabc77f-f9a6-40ba-88b7-906dfa1903c9)(content(Comment\"#                                                             \
         #\"))))(Secondary((id \
         e66f21da-6777-4f33-b5d6-7e2bffba1c4f)(content(Whitespace\"\\n\"))))(Secondary((id \
         3b7f3a07-a6e1-4f80-ac77-5b5c94b511ee)(content(Comment\"#   let \
         classify: Int -> WaterLevel =                         \
         #\"))))(Secondary((id \
         fde788c2-e681-4328-bb9c-6e5eb0b938ac)(content(Whitespace\"\\n\"))))(Secondary((id \
         6636db48-2f92-4b81-babf-ca7e32dd0798)(content(Comment\"#     fun \
         amount ->                                           \
         #\"))))(Secondary((id \
         01dcd1a0-1a4b-42cb-a3b0-2e58708f30e4)(content(Whitespace\"\\n\"))))(Secondary((id \
         42ab1f50-dc1a-461d-a886-b7908fee88de)(content(Comment\"#     if \
         amount >= 200 then High                              \
         #\"))))(Secondary((id \
         d187cafa-7c9c-48f7-912a-2308973fe24b)(content(Whitespace\"\\n\"))))(Secondary((id \
         bf2966f5-7c32-44f9-8003-62ffddfa096b)(content(Comment\"#     else if \
         amount >= 100 then Medium                       #\"))))(Secondary((id \
         d6474945-18f9-4819-9e50-96b709730823)(content(Whitespace\"\\n\"))))(Secondary((id \
         9dc0af59-0e1d-4041-8a04-2b1e001084ea)(content(Comment\"#     else \
         Low                                                \
         #\"))))(Secondary((id \
         626afac3-0913-4da0-bfd7-7d064145c579)(content(Whitespace\"\\n\"))))(Secondary((id \
         3072dfcf-ab93-4b1f-b662-5e02e4458f42)(content(Comment\"#   \
         in                                                        \
         #\"))))(Secondary((id \
         dd32f85b-d1ea-432b-a426-02d036925629)(content(Whitespace\"\\n\"))))(Secondary((id \
         bab2a324-5762-4e53-b552-322a2ed13234)(content(Comment\"#                                                             \
         #\"))))(Secondary((id \
         4909ae48-bb78-414c-b220-2bdd459f5bd0)(content(Whitespace\"\\n\"))))(Secondary((id \
         2aa18de2-208f-4cb5-88a1-d216ac2812ec)(content(Comment\"#   let \
         format_plant: (String, Int) -> String =               \
         #\"))))(Secondary((id \
         0de5e505-db56-4241-a951-c96b93c9f7af)(content(Whitespace\"\\n\"))))(Secondary((id \
         a5350f54-9042-405c-9fc1-be7cb83a7f16)(content(Comment\"#     fun \
         (name, water) ->                                    \
         #\"))))(Secondary((id \
         f62977d2-ce1d-4647-9f0d-d8de20d5ace6)(content(Whitespace\"\\n\"))))(Secondary((id \
         f243f8ff-dcd4-45d6-a6f3-2013f03e8410)(content(Comment\"#     let \
         emoji = case classify(water)                        \
         #\"))))(Secondary((id \
         e8cf5b60-b82a-4ecd-afee-ed391d75fa45)(content(Whitespace\"\\n\"))))(Secondary((id \
         866958d1-2dd0-453a-8165-d6ecd16256c5)(content(Comment\"#       | Low \
         => \
         \\\"\\240\\159\\140\\181\\\"                                         \
         #\"))))(Secondary((id \
         27ce8647-e734-4553-b028-48bc3c542c8a)(content(Whitespace\"\\n\"))))(Secondary((id \
         09baaa6a-78b2-4858-b293-27150257976d)(content(Comment\"#       | \
         Medium => \
         \\\"\\240\\159\\146\\167\\\"                                      \
         #\"))))(Secondary((id \
         6ba0ca03-a10c-4bc8-9c6c-597e1cf687e3)(content(Whitespace\"\\n\"))))(Secondary((id \
         4fce5283-5920-4880-8f86-1bf5366f55bb)(content(Comment\"#       | High \
         => \
         \\\"\\240\\159\\146\\167\\240\\159\\146\\167\\\"                                      \
         #\"))))(Secondary((id \
         e464c942-9074-4c70-8c7e-26dbadddc476)(content(Whitespace\"\\n\"))))(Secondary((id \
         d3a135ef-81a7-420c-8e40-050ff2391ce3)(content(Comment\"#     end \
         in                                                  \
         #\"))))(Secondary((id \
         97e26ec7-0e85-4143-9c7e-a8cc718927b4)(content(Whitespace\"\\n\"))))(Secondary((id \
         27138ab1-2e96-49de-918c-51b9293ac2cd)(content(Comment\"#     emoji ++ \
         \\\" \\\" ++ name ++ \\\": \\\"                            \
         #\"))))(Secondary((id \
         4bf63a52-8797-4da6-82de-944286cacb82)(content(Whitespace\"\\n\"))))(Secondary((id \
         da90a643-9338-4fae-87ca-1bde74e7bab2)(content(Comment\"#       ++ \
         string_of_int(water) ++ \\\"ml\\\"                       \
         #\"))))(Secondary((id \
         a4380439-e3f5-46ab-a11d-9bef568f96c5)(content(Whitespace\"\\n\"))))(Secondary((id \
         d6c053ff-8cce-43dd-904e-ec17263830be)(content(Comment\"#   \
         in                                                        \
         #\"))))(Secondary((id \
         224eec5f-beab-42ab-8a93-e3da6f05b9c5)(content(Whitespace\"\\n\"))))(Secondary((id \
         2e879ed6-19ae-4dc6-9c02-7b52d6ec099d)(content(Comment\"#                                                             \
         #\"))))(Secondary((id \
         3bea6ea9-76df-482f-8f1c-af4fd3987e26)(content(Whitespace\"\\n\"))))(Secondary((id \
         50bb676b-1106-4261-8d4f-894228325f49)(content(Comment\"#   test \
         format_plant(\\\"Fern\\\", 250)                            \
         #\"))))(Secondary((id \
         6375c2f8-b7d2-4d0b-81ab-634b17110270)(content(Whitespace\"\\n\"))))(Secondary((id \
         35e9bac5-6f4d-4103-990c-ae27be931ac3)(content(Comment\"#     == \
         \\\"\\240\\159\\146\\167\\240\\159\\146\\167 Fern: 250ml\\\" \
         end;                              #\"))))(Secondary((id \
         d3fb5704-ff4a-4338-98f8-4c79ade463f0)(content(Whitespace\"\\n\"))))(Secondary((id \
         4ff7acd5-15bd-4126-8788-27bb179ae337)(content(Comment\"#   test \
         format_plant(\\\"Orchid\\\", 180)                          \
         #\"))))(Secondary((id \
         0fff0dde-5982-463e-a96c-76651643cd23)(content(Whitespace\"\\n\"))))(Secondary((id \
         7efa3fba-5b90-49d7-b03b-6b49a8e1847c)(content(Comment\"#     == \
         \\\"\\240\\159\\146\\167 Orchid: 180ml\\\" \
         end;                              #\"))))(Secondary((id \
         a7b1539d-73cc-4454-9fc4-92dc8c03a9cf)(content(Whitespace\"\\n\"))))(Secondary((id \
         25e34537-8e55-4713-95b3-9cff43c010e4)(content(Comment\"#   test \
         format_plant(\\\"Cactus\\\", 50)                           \
         #\"))))(Secondary((id \
         8678a596-bdfe-4208-9fe4-f73ec48a144a)(content(Whitespace\"\\n\"))))(Secondary((id \
         84612f25-ddd3-4d38-b671-7dfe3524ed5e)(content(Comment\"#     == \
         \\\"\\240\\159\\140\\181 Cactus: 50ml\\\" \
         end;                               #\"))))(Secondary((id \
         ffb33fd9-c0f0-47ab-951b-21c5903d39c0)(content(Whitespace\"\\n\"))))(Secondary((id \
         4ea34d7c-9361-4db8-b316-7bfc51359fc5)(content(Comment\"#   test \
         format_plant(\\\"Lily\\\", 200)                            \
         #\"))))(Secondary((id \
         0e016fa0-f3dd-4ce1-81a6-489302691126)(content(Whitespace\"\\n\"))))(Secondary((id \
         0c9eead2-662a-4ff7-ae9a-9d411234a38a)(content(Comment\"#     == \
         \\\"\\240\\159\\146\\167\\240\\159\\146\\167 Lily: 200ml\\\" \
         end                               #\"))))(Secondary((id \
         670e01c5-6aa9-4196-82b5-ca07f98364fc)(content(Whitespace\"\\n\"))))(Secondary((id \
         c94796ff-1b5e-4efc-ba98-4af10da0defe)(content(Comment\"#                                                             \
         #\"))))(Secondary((id \
         ca2ea41c-8255-47ec-935f-b476c51de586)(content(Whitespace\"\\n\"))))(Secondary((id \
         45f47183-1faf-48ab-8b43-63e32293f356)(content(Comment\"# All four \
         pass \\226\\128\\148 including Lily! Click inside classify       \
         #\"))))(Secondary((id \
         20f3de56-45cb-4d51-93a7-3bfa4173bbd0)(content(Whitespace\"\\n\"))))(Secondary((id \
         31d212e7-7cbe-4fc1-affc-9ad67d148943)(content(Comment\"# in Many mode \
         to see: High, Medium, Low, High.              #\"))))(Secondary((id \
         e5dc86c5-41ae-491c-8403-a1ebe001a62f)(content(Whitespace\"\\n\"))))(Secondary((id \
         4fdbe52f-8b2e-4a63-a924-7f6986a8d9aa)(content(Comment\"# The >= 200 \
         threshold now handles the boundary correctly.    \
         #\"))))(Secondary((id \
         4abb55c4-0480-413b-bc33-48cf1e9165ee)(content(Whitespace\"\\n\"))))(Secondary((id \
         0ff995c8-df14-48e8-8ad4-e5f2d7848538)(content(Whitespace\"\\n\"))))(Tile((id \
         e2321c53-7471-49a0-a4f3-803a04ca1f2e)(label(?))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         8d34bef1-b335-4fdb-a031-3e98e90f0f1f)(content(Whitespace\"\\n\"))))(Secondary((id \
         317e210d-1b2c-4dac-a569-52ac06df84c3)(content(Whitespace\"\\n\"))))(Secondary((id \
         e9f79785-c5cc-427d-96df-af217c6f44a9)(content(Comment\"# \
         ============================================================ \
         #\"))))(Secondary((id \
         78bd84bd-1cde-4e07-b79c-98e4c61cef2f)(content(Whitespace\"\\n\"))))(Secondary((id \
         4d23e27a-0c21-4fee-9eb9-9544444246ce)(content(Comment\"# \
         REVIEW                                                      \
         #\"))))(Secondary((id \
         dfa7775d-06a8-412b-91e1-4ed9db4674e1)(content(Whitespace\"\\n\"))))(Secondary((id \
         4e2a18eb-38c0-4a4e-bf2d-b7959e40f25f)(content(Comment\"#                                                             \
         #\"))))(Secondary((id \
         cb75436b-2a8c-4515-b92c-92585e38e393)(content(Whitespace\"\\n\"))))(Secondary((id \
         a59d96b8-3b78-4697-9614-f0872cbdfedd)(content(Comment\"# You've \
         written:                                             \
         #\"))))(Secondary((id \
         cccf7662-c53a-45ec-b85e-3ebc44dca15a)(content(Whitespace\"\\n\"))))(Secondary((id \
         432bcc54-4494-48cd-9f4f-e4f9f8efd7d0)(content(Comment\"#   1. Let \
         bindings and string expressions                    \
         #\"))))(Secondary((id \
         be77cc1f-1035-479b-b888-198f0da1f12d)(content(Whitespace\"\\n\"))))(Secondary((id \
         aedc8a34-8200-4fbf-a341-aaa245fcb758)(content(Comment\"#   2. A \
         function with tests                                  \
         #\"))))(Secondary((id \
         d57995e6-582b-4585-b2bd-719228163903)(content(Whitespace\"\\n\"))))(Secondary((id \
         819608d8-d8d9-43fc-a896-de8624188f9b)(content(Comment\"#   3. A \
         helper with if/else (and caught a boundary bug)      \
         #\"))))(Secondary((id \
         82ebea35-897c-472d-9800-34a19c8acc78)(content(Whitespace\"\\n\"))))(Secondary((id \
         b837c85e-2fc4-4eda-bed3-8ae876fd77af)(content(Comment\"#   4. A type \
         definition with case expression                 #\"))))(Secondary((id \
         77005a89-3b3b-40f5-b218-aeded8627169)(content(Whitespace\"\\n\"))))(Secondary((id \
         1214f710-80b9-4805-aa84-2f01b22cea4e)(content(Comment\"#                                                             \
         #\"))))(Secondary((id \
         086d209b-9fac-4508-b15c-0e6f2065aca3)(content(Whitespace\"\\n\"))))(Secondary((id \
         5b75e4d8-9f18-4eeb-a4de-31743e31b1ce)(content(Comment\"# At each \
         step, probes showed intermediate values inline,     \
         #\"))))(Secondary((id \
         0e795518-4743-4261-b8f0-1c2beb143926)(content(Whitespace\"\\n\"))))(Secondary((id \
         2eb904fe-b545-4c9f-ab0d-9ee380aace87)(content(Comment\"# so you could \
         verify correctness as you went.                #\"))))(Secondary((id \
         f1d26ed9-c4e4-407a-b423-38cbfa33086d)(content(Whitespace\"\\n\"))))(Secondary((id \
         bf688d21-b807-4257-825b-b0329957fcce)(content(Comment\"# \
         ============================================================ \
         #\"))))(Secondary((id \
         e5c055f9-724a-4811-8683-73e0b40a80ab)(content(Whitespace\"\\n\"))))(Secondary((id \
         7c826bfa-9065-429e-a37c-f60b6a471007)(content(Whitespace\"\\n\"))))(Secondary((id \
         69ae7787-2f8e-4e0e-940b-6bf71d5a55f5)(content(Comment\"# END \
         #\"))))(Secondary((id \
         4e04567a-2fe1-46f4-ad46-a037eab35228)(content(Whitespace\"\\n\")))))";
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
