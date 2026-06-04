let out : string * Haz3lcore.PersistentSegment.t =
  ( "Study / tutorial / 07-writing",
    {
      segment =
        "((Secondary((id \
         dceaf4bc-d769-4bca-91da-58621f33f356)(content(Comment\"# WRITING WITH \
         LIVE VALUES #\"))))(Secondary((id \
         5e1c574c-ddff-4e48-8474-69dfdd0d29b7)(content(Whitespace\"\\n\"))))(Secondary((id \
         a9dcd888-afa8-43d5-b1c1-87c171a8308c)(content(Whitespace\"\\n\"))))(Secondary((id \
         02be82d9-cb54-4f69-b5c1-0fc194a506c5)(content(Comment\"# In this \
         tutorial you'll write code step by step.           \
         #\"))))(Secondary((id \
         c19bbb5a-fa5c-40a7-964f-7e6526560e46)(content(Whitespace\"\\n\"))))(Secondary((id \
         5e97bc66-ad84-49c2-9885-b7a227190505)(content(Comment\"# Turn on Auto \
         Mode (microscope icon, top right).            #\"))))(Secondary((id \
         e23ae199-97da-45b5-88d8-13e3fa42d2bc)(content(Whitespace\"\\n\"))))(Secondary((id \
         00520db9-4d79-430f-9163-6669d8d0463a)(content(Comment\"# Each stage \
         shows you what to type, then you type it        #\"))))(Secondary((id \
         0f643bf7-00a1-46a0-b47b-2e1670a2d03f)(content(Whitespace\"\\n\"))))(Secondary((id \
         e58ac1fb-4123-4997-b62a-91bee5afe1b1)(content(Comment\"# in the \
         designated area and see probe values appear.        \
         #\"))))(Secondary((id \
         d54ea871-1b32-40c4-a016-020a4670ac28)(content(Whitespace\"\\n\"))))(Secondary((id \
         de372ff9-caaa-46be-9c3c-aaae235e9439)(content(Whitespace\"\\n\"))))(Secondary((id \
         7e483e8a-5195-416d-97d8-812e8fe5453f)(content(Comment\"# \
         ============================================================ \
         #\"))))(Secondary((id \
         cd5bdab2-4d9c-4d26-beef-5df3f0cba8cb)(content(Whitespace\"\\n\"))))(Secondary((id \
         60208187-2e1f-4f70-9d59-3b9f145158ff)(content(Comment\"# STAGE 1: LET \
         BINDINGS AND STRINGS                          #\"))))(Secondary((id \
         bc55e4b3-9d40-432f-8773-1552d3da35bb)(content(Whitespace\"\\n\"))))(Secondary((id \
         232698df-b77c-48cd-a4c0-5486d10e7739)(content(Comment\"# \
         ============================================================ \
         #\"))))(Secondary((id \
         9bfa37f7-d5fd-411a-b0c0-41e26e644502)(content(Whitespace\"\\n\"))))(Secondary((id \
         e5b92444-ca4c-42e9-90e8-15412211e5ee)(content(Whitespace\"\\n\"))))(Secondary((id \
         c5dba687-58ce-4cd9-a0c1-73a9fa8c3d82)(content(Comment\"# `let x = \
         expr in body` binds a value. `++` concatenates    \
         #\"))))(Secondary((id \
         d3cc3bb0-a14f-4af1-9e66-80b3743e53ed)(content(Whitespace\"\\n\"))))(Secondary((id \
         6616d1b7-85ef-45cf-a491-055a4081cfe1)(content(Comment\"# strings. \
         `string_of_int` converts a number to a string.    \
         #\"))))(Secondary((id \
         95808c84-b7c4-491d-966b-da68443ec606)(content(Whitespace\"\\n\"))))(Secondary((id \
         19281443-030d-492f-8d3f-1d36fa882b75)(content(Comment\"#                                                             \
         #\"))))(Secondary((id \
         97ddee72-492a-4134-8b71-36fbb6bfd373)(content(Whitespace\"\\n\"))))(Secondary((id \
         1bb07c03-6322-4c26-9ff4-c009efdf2ec4)(content(Comment\"# Replace the \
         ? below with:                                   #\"))))(Secondary((id \
         41b99ac6-1dbe-40ae-8898-815c46358101)(content(Whitespace\"\\n\"))))(Secondary((id \
         9a507cab-43f4-4a8f-a454-e1cce1720515)(content(Comment\"#                                                             \
         #\"))))(Secondary((id \
         5a72a06c-20f3-44d0-ad5d-f63dd77d7be9)(content(Whitespace\"\\n\"))))(Secondary((id \
         cc06cdcb-b94e-406b-8f02-6b50e5a533b4)(content(Comment\"#   let name = \
         \\\"Fern\\\" in                                      \
         #\"))))(Secondary((id \
         06bc06e4-85ef-433c-983b-69c19abb72b1)(content(Whitespace\"\\n\"))))(Secondary((id \
         2ce12040-e38c-4925-bda1-9110f7877f7e)(content(Comment\"#   let water \
         = 250 in                                        #\"))))(Secondary((id \
         cc6b9acd-1898-4e4b-8cce-e0b1ff3b79b3)(content(Whitespace\"\\n\"))))(Secondary((id \
         eec12a3f-f425-4960-80ed-5c5ea1a7b8b2)(content(Comment\"#   let label \
         =                                               #\"))))(Secondary((id \
         dc75bce2-ebb5-4f29-967e-b3ebd01a27b7)(content(Whitespace\"\\n\"))))(Secondary((id \
         7f478f56-abae-4d01-8eb5-a505b781558e)(content(Comment\"#     name ++ \
         \\\": \\\" ++ string_of_int(water) ++ \\\"ml\\\"            \
         #\"))))(Secondary((id \
         3b527d66-9059-491e-b8b1-ad290953cd34)(content(Whitespace\"\\n\"))))(Secondary((id \
         e8e6b10a-52fc-4801-bf01-f732f0264055)(content(Comment\"#   \
         in                                                        \
         #\"))))(Secondary((id \
         71f72a46-b303-42b7-b8e7-ce7c50b90405)(content(Whitespace\"\\n\"))))(Secondary((id \
         b4b9578d-7443-4ab3-ac3f-14e52d24434a)(content(Comment\"#   \
         label                                                     \
         #\"))))(Secondary((id \
         fe785f45-44d7-4082-aaca-03e4a8444bcc)(content(Whitespace\"\\n\"))))(Secondary((id \
         a6989389-5fd5-46d6-83f2-e001339112a5)(content(Comment\"#                                                             \
         #\"))))(Secondary((id \
         458913f3-eecd-4bd5-a690-17bcaf2b69b4)(content(Whitespace\"\\n\"))))(Secondary((id \
         f0c9f97f-33a8-4c28-9bed-9597ae4698aa)(content(Comment\"# Then click \
         inside your let bindings. Auto-probe should      \
         #\"))))(Secondary((id \
         eb455f84-5b31-4d30-b62a-4bd823e844aa)(content(Whitespace\"\\n\"))))(Secondary((id \
         d0464137-4708-483b-8ffa-7c3c69ea590a)(content(Comment\"# show \
         \\\"Fern\\\", 250, and \\\"Fern: 250ml\\\". Try changing the      \
         #\"))))(Secondary((id \
         7b7c94da-0ae7-4721-846e-d9d05a20d7d5)(content(Whitespace\"\\n\"))))(Secondary((id \
         567cf227-cea8-4ad8-b2ef-e007072e6c3b)(content(Comment\"# name or \
         number and watch the label update instantly.        \
         #\"))))(Secondary((id \
         5f4b7fb8-ed51-4bf0-846f-eb0c22eb40fe)(content(Whitespace\"\\n\"))))(Secondary((id \
         100b54d9-3db5-4b51-982d-0850535f64ee)(content(Whitespace\"\\n\"))))(Tile((id \
         15abf534-a159-46e6-9f5d-ab1011e79f2e)(label(?))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         58534c44-e817-4869-9e6a-0b6241b89e5e)(content(Whitespace\"\\n\"))))(Secondary((id \
         68f18dff-e18f-4f89-84d9-30eccba8b58a)(content(Whitespace\"\\n\"))))(Tile((id \
         56f05aae-6540-4018-8533-1f3f145fe2a0)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         2e715645-0e84-4838-9bf5-960efad6c92f)(content(Whitespace\"\\n\"))))(Secondary((id \
         dd06181d-4ada-4641-bbce-751edda02bc5)(content(Whitespace\"\\n\"))))(Secondary((id \
         0de7b7a4-4662-4526-8845-2b9f9c56797c)(content(Comment\"# \
         ============================================================ \
         #\"))))(Secondary((id \
         10021490-6832-4a5a-b981-aaaea8202070)(content(Whitespace\"\\n\"))))(Secondary((id \
         b4401ba4-3008-49c5-bcaa-d95afd396ee2)(content(Comment\"# STAGE 2: A \
         FUNCTION AND TESTS                              #\"))))(Secondary((id \
         2f013388-eaf2-4bbb-9904-8b9230e85c52)(content(Whitespace\"\\n\"))))(Secondary((id \
         253bbf4d-f9e3-4284-9c16-23ce672d69a6)(content(Comment\"# \
         ============================================================ \
         #\"))))(Secondary((id \
         d2575fea-95e2-48e1-93f8-95fce58751b3)(content(Whitespace\"\\n\"))))(Secondary((id \
         c81f8900-442d-4583-909e-364ec915bec5)(content(Whitespace\"\\n\"))))(Secondary((id \
         aade08a0-ed82-4b7a-be35-0081fba77a48)(content(Comment\"# Now let's \
         make the label formatting reusable.               \
         #\"))))(Secondary((id \
         e9f53256-ad64-4ce7-aadb-34af6ea29c6c)(content(Whitespace\"\\n\"))))(Secondary((id \
         584c7692-fce6-4f67-b139-87c5ea80bab3)(content(Comment\"# `fun (a, b) \
         -> body` defines a function taking a tuple.     #\"))))(Secondary((id \
         93b2229f-d3c4-4ce2-a2dd-644fc45918a1)(content(Whitespace\"\\n\"))))(Secondary((id \
         906e01d7-94af-4ad6-8c55-c1eb8f871196)(content(Comment\"# `test X == Y \
         end` checks that X equals Y.                  #\"))))(Secondary((id \
         39e6df58-52ab-43a2-a9c4-f772b97cb975)(content(Whitespace\"\\n\"))))(Secondary((id \
         bb01c339-27ef-46ac-83f2-5f901d12b746)(content(Comment\"#                                                             \
         #\"))))(Secondary((id \
         4af078d1-b275-40f4-889f-af03c1c94e15)(content(Whitespace\"\\n\"))))(Secondary((id \
         44613bc2-2b2a-42be-9b8f-c45ae036a728)(content(Comment\"# Replace the \
         ? below with:                                   #\"))))(Secondary((id \
         a3da1cea-779f-4293-9aea-d778df4d8341)(content(Whitespace\"\\n\"))))(Secondary((id \
         48def068-a5b7-403c-87cc-3169bbf26e67)(content(Comment\"#                                                             \
         #\"))))(Secondary((id \
         a9efd154-bc3c-4ea9-8840-f6871cd54465)(content(Whitespace\"\\n\"))))(Secondary((id \
         a28ad61c-db22-4c4b-b565-7936433f5d6d)(content(Comment\"#   let \
         format_plant: (String, Int) -> String =               \
         #\"))))(Secondary((id \
         96b092dc-4be1-4c74-9211-74d949ff1dcc)(content(Whitespace\"\\n\"))))(Secondary((id \
         f25865bd-7ab8-481d-bf0c-942d40361840)(content(Comment\"#     fun \
         (name, water) ->                                    \
         #\"))))(Secondary((id \
         31153ba1-35b5-4c20-9dac-949eacb50615)(content(Whitespace\"\\n\"))))(Secondary((id \
         40d4c49d-9093-4607-8568-e744d5403ac1)(content(Comment\"#     name ++ \
         \\\": \\\" ++ string_of_int(water) ++ \\\"ml\\\"            \
         #\"))))(Secondary((id \
         b9afcc5e-7567-4137-aaea-6a6c27cd0f29)(content(Whitespace\"\\n\"))))(Secondary((id \
         e90c5de1-15e2-4d00-ab2e-0183bf7c26ba)(content(Comment\"#   \
         in                                                        \
         #\"))))(Secondary((id \
         80c3d36b-ecbd-48fb-8e01-1ccab0972cb0)(content(Whitespace\"\\n\"))))(Secondary((id \
         224afca2-926e-4174-b164-fbbafc4e739f)(content(Comment\"#                                                             \
         #\"))))(Secondary((id \
         488986dc-8f5d-4295-8f4d-f7b162cff7c4)(content(Whitespace\"\\n\"))))(Secondary((id \
         c0eb90e7-2406-4db3-b8ca-1e5ac022d7c9)(content(Comment\"#   test \
         format_plant(\\\"Fern\\\", 250)                            \
         #\"))))(Secondary((id \
         87f713d6-962f-4f95-866c-940edf9da16f)(content(Whitespace\"\\n\"))))(Secondary((id \
         15bf264c-3dd6-415b-a1b0-54249a15fb2c)(content(Comment\"#     == \
         \\\"Fern: 250ml\\\" end;                                   \
         #\"))))(Secondary((id \
         bdd234b3-e752-4262-ad08-fe2b9da2326e)(content(Whitespace\"\\n\"))))(Secondary((id \
         68e64871-a593-4874-bf74-575d51717ce3)(content(Comment\"#   test \
         format_plant(\\\"Orchid\\\", 180)                          \
         #\"))))(Secondary((id \
         9f6e21a6-edf4-419e-9949-669a252a3dce)(content(Whitespace\"\\n\"))))(Secondary((id \
         3f315973-5c31-4520-b86f-c7b2d7a4dbdb)(content(Comment\"#     == \
         \\\"Orchid: 180ml\\\" end;                                 \
         #\"))))(Secondary((id \
         3e42779a-8950-4efe-8cec-f5b83868d045)(content(Whitespace\"\\n\"))))(Secondary((id \
         a85c316a-1f4c-42e4-8fa4-bb36600b7990)(content(Comment\"#   test \
         format_plant(\\\"Cactus\\\", 50)                           \
         #\"))))(Secondary((id \
         9fc871f5-1260-44d5-aee2-73de052d3cac)(content(Whitespace\"\\n\"))))(Secondary((id \
         4b65b97b-8461-40e7-84f7-c49c95e0b5c4)(content(Comment\"#     == \
         \\\"Cactus: 50ml\\\" end                                   \
         #\"))))(Secondary((id \
         4ce79e71-dbb8-4a0f-9abb-e5562044275c)(content(Whitespace\"\\n\"))))(Secondary((id \
         88c932a1-da35-49db-a572-5c22609094ab)(content(Comment\"#                                                             \
         #\"))))(Secondary((id \
         f7c67c34-2f35-4202-b44a-c83d79763c21)(content(Whitespace\"\\n\"))))(Secondary((id \
         855af668-0f6d-4fad-a08f-c0d72200d6bb)(content(Comment\"# Click inside \
         format_plant to see auto-probe values for      #\"))))(Secondary((id \
         157c60bc-a7df-4825-bc43-e364778506d7)(content(Whitespace\"\\n\"))))(Secondary((id \
         b9656a34-d724-4e28-8c1a-b728c7958dd4)(content(Comment\"# each test \
         call. Toggle Many mode (Space) to see all         \
         #\"))))(Secondary((id \
         d5423da6-4590-4e08-93d2-874b8c0099ef)(content(Whitespace\"\\n\"))))(Secondary((id \
         83515d92-c69c-4c36-9fe1-e41b1fde7f5c)(content(Comment\"# three \
         results side by side.                                 \
         #\"))))(Secondary((id \
         d863f08c-68b3-4a0a-8a89-5e2a295272c9)(content(Whitespace\"\\n\"))))(Secondary((id \
         6baa4a50-c670-4976-9f83-218d07ec4e46)(content(Whitespace\"\\n\"))))(Tile((id \
         f6764a5a-ed94-46a9-b7e3-32fe8c0d7d54)(label(?))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         6f1d5f1d-fa8d-477e-83b6-8d7dfbc3a899)(content(Whitespace\"\\n\"))))(Secondary((id \
         324f6a6a-e88e-4af8-8121-55984e64d25e)(content(Whitespace\"\\n\"))))(Tile((id \
         24c41f06-de5d-42f8-bd0b-fe2dfa516c81)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         b2548e1c-3bd7-4ef3-8650-cba026da43f1)(content(Whitespace\"\\n\"))))(Secondary((id \
         ec5381f8-4083-41e3-a7d5-9cabd6149edf)(content(Whitespace\"\\n\"))))(Secondary((id \
         1b01a6a1-d5d0-4768-b95d-b4427a0d2383)(content(Comment\"# \
         ============================================================ \
         #\"))))(Secondary((id \
         34f5f3f8-9478-45bd-b926-ef094ad7a0a6)(content(Whitespace\"\\n\"))))(Secondary((id \
         656b0bec-9cd6-4e26-87d7-9318e605ce4a)(content(Comment\"# STAGE 3: A \
         HELPER WITH IF/ELSE                             #\"))))(Secondary((id \
         91f64df2-2512-4243-9992-ad5ada9f0707)(content(Whitespace\"\\n\"))))(Secondary((id \
         cd01908c-3904-4799-9cd2-bb17340cab04)(content(Comment\"# \
         ============================================================ \
         #\"))))(Secondary((id \
         7c63f3a7-58a5-40e2-91a2-f7646dd2226e)(content(Whitespace\"\\n\"))))(Secondary((id \
         6cb3707d-2d90-4afd-a034-128a83aa76cb)(content(Whitespace\"\\n\"))))(Secondary((id \
         072554cc-b9da-4e24-8fbc-8aec0ed0aab5)(content(Comment\"# Plants with \
         high water needs should get a \
         \\240\\159\\146\\167\\240\\159\\146\\167 tag.        \
         #\"))))(Secondary((id \
         96ef0bdb-32cf-4019-b439-b84d61271bf2)(content(Whitespace\"\\n\"))))(Secondary((id \
         5bd72895-200c-4dcf-b4c9-5d46b406e523)(content(Comment\"# We'll write \
         a helper function using a conditional:          #\"))))(Secondary((id \
         f1056084-9d14-4b04-a78e-98a1adfbbabd)(content(Whitespace\"\\n\"))))(Secondary((id \
         032ea49d-496b-48d5-a538-cebfebeac50b)(content(Comment\"#   `if \
         condition then expr1 else expr2`                      \
         #\"))))(Secondary((id \
         f4262886-f813-4321-898f-3fe09659ff69)(content(Whitespace\"\\n\"))))(Secondary((id \
         1f951ae9-e112-4d88-8bea-de2e41bea772)(content(Comment\"#                                                             \
         #\"))))(Secondary((id \
         5c3fe32d-93fc-4f82-a1f7-1316ebb1bd54)(content(Whitespace\"\\n\"))))(Secondary((id \
         a3b38978-c185-495f-91c7-eac5f3f05a76)(content(Comment\"# A note on \
         writing order: in practice, you'd first write    \
         #\"))))(Secondary((id \
         800a00f5-4e2c-4ddc-9461-83745d46a1c0)(content(Whitespace\"\\n\"))))(Secondary((id \
         95d41ddf-b9b6-4963-8486-24e6ddb85063)(content(Comment\"# the CALL to \
         water_tag inside format_plant, see a hole       #\"))))(Secondary((id \
         5630a0dc-e9e4-46f6-ae5f-7f472db46e06)(content(Whitespace\"\\n\"))))(Secondary((id \
         bcb8afc0-3246-43e8-aee7-335395aa4f29)(content(Comment\"# value, then \
         go implement the helper above it. This          #\"))))(Secondary((id \
         29236f6f-6718-4210-8007-ce9a10542d28)(content(Whitespace\"\\n\"))))(Secondary((id \
         c18742fe-2e10-4493-85a1-09c5167fa7cc)(content(Comment\"# outside-in \
         approach lets live values guide your writing.    \
         #\"))))(Secondary((id \
         ffb54e1b-6c1b-4948-a5fc-4feea415512a)(content(Whitespace\"\\n\"))))(Secondary((id \
         bba04859-7d05-4429-ac32-c5b78f21fd41)(content(Comment\"#                                                             \
         #\"))))(Secondary((id \
         8f141491-e60a-4656-b3cc-45c0db4a7906)(content(Whitespace\"\\n\"))))(Secondary((id \
         8abfb229-a573-4dec-83be-bc88ad33c841)(content(Comment\"# Replace the \
         ? below with:                                   #\"))))(Secondary((id \
         44257b71-f1b3-4e3e-b2d6-6540557c3e3e)(content(Whitespace\"\\n\"))))(Secondary((id \
         9c0676ec-d05c-43ea-b35e-5df7befa79ed)(content(Comment\"#                                                             \
         #\"))))(Secondary((id \
         7f363871-38eb-4a94-bc0d-6550e84472b6)(content(Whitespace\"\\n\"))))(Secondary((id \
         60bb7dc2-f20f-429e-9eb0-3602152c255b)(content(Comment\"#   let \
         water_tag: Int -> String =                            \
         #\"))))(Secondary((id \
         0915391a-6464-4274-a056-854663473ed3)(content(Whitespace\"\\n\"))))(Secondary((id \
         a343e911-38f5-4362-924f-65608579fb5f)(content(Comment\"#     fun \
         amount ->                                           \
         #\"))))(Secondary((id \
         55ebce86-30a1-41e0-801f-1d7e9f4a5cc6)(content(Whitespace\"\\n\"))))(Secondary((id \
         d7340d46-eb8a-41af-b980-3290aa9e7e29)(content(Comment\"#     if \
         amount > 200 then \
         \\\"\\240\\159\\146\\167\\240\\159\\146\\167\\\"                             \
         #\"))))(Secondary((id \
         3593afc3-f000-499e-905b-847da150a4f3)(content(Whitespace\"\\n\"))))(Secondary((id \
         06595fd8-c0ac-4c12-b1df-0c8bb76b4572)(content(Comment\"#     else \
         \\\"\\240\\159\\146\\167\\\"                                               \
         #\"))))(Secondary((id \
         205af200-38af-4001-95ad-04985b086bf9)(content(Whitespace\"\\n\"))))(Secondary((id \
         6c4292b7-226e-4420-bc1f-cc6571f6b719)(content(Comment\"#   \
         in                                                        \
         #\"))))(Secondary((id \
         8e0234c9-e6c9-4ab1-b46f-e9612ae7b413)(content(Whitespace\"\\n\"))))(Secondary((id \
         4175a564-f80b-4ed1-8806-0cea23da0038)(content(Comment\"#                                                             \
         #\"))))(Secondary((id \
         8967c87b-c5a8-430c-a8fa-9fee9dd21553)(content(Whitespace\"\\n\"))))(Secondary((id \
         b8e2cb81-198d-42ae-8927-d8e64141bc43)(content(Comment\"#   let \
         format_plant: (String, Int) -> String =               \
         #\"))))(Secondary((id \
         126ac75f-9913-4473-9969-7bc229284533)(content(Whitespace\"\\n\"))))(Secondary((id \
         86e8124c-90a9-439d-9b90-790e84104a0e)(content(Comment\"#     fun \
         (name, water) ->                                    \
         #\"))))(Secondary((id \
         f680c73e-7b3e-4ff1-b59c-0af44be95b86)(content(Whitespace\"\\n\"))))(Secondary((id \
         523e3340-676e-4d15-95f3-2a3600c1c165)(content(Comment\"#     let tag \
         = water_tag(water) in                           #\"))))(Secondary((id \
         825eb1bb-b68f-425c-b6e5-d343782e8362)(content(Whitespace\"\\n\"))))(Secondary((id \
         1b04c3f1-1950-4d37-9af0-6fce644eb761)(content(Comment\"#     tag ++ \
         \\\" \\\" ++ name ++ \\\": \\\"                              \
         #\"))))(Secondary((id \
         06fd63a8-f15d-4e14-8571-f32596e1db07)(content(Whitespace\"\\n\"))))(Secondary((id \
         5cba0f16-3023-4f29-a146-64cfc6f4e4f7)(content(Comment\"#       ++ \
         string_of_int(water) ++ \\\"ml\\\"                       \
         #\"))))(Secondary((id \
         987bc7f8-8141-4433-ae0d-1bca75802bdd)(content(Whitespace\"\\n\"))))(Secondary((id \
         4c3de59d-e8a4-4976-888e-d3c8fdd7cbc8)(content(Comment\"#   \
         in                                                        \
         #\"))))(Secondary((id \
         b6065a96-fbb0-4ed8-a8b8-644e53676fc6)(content(Whitespace\"\\n\"))))(Secondary((id \
         9ddf7ca2-48a7-4a02-a352-09a38c8a4110)(content(Comment\"#                                                             \
         #\"))))(Secondary((id \
         432c7bdf-cdad-4779-850e-b0f4ac117ed4)(content(Whitespace\"\\n\"))))(Secondary((id \
         e25fc4a8-bd6d-4b25-9c0b-dccb15108579)(content(Comment\"#   test \
         format_plant(\\\"Fern\\\", 250)                            \
         #\"))))(Secondary((id \
         3a0e4b02-ca97-40c1-ab8a-e087df184305)(content(Whitespace\"\\n\"))))(Secondary((id \
         644deb2e-3765-4b91-b1b8-50f5453476b1)(content(Comment\"#     == \
         \\\"\\240\\159\\146\\167\\240\\159\\146\\167 Fern: 250ml\\\" \
         end;                              #\"))))(Secondary((id \
         10037d43-118f-42b9-9d2f-a117d7acfe75)(content(Whitespace\"\\n\"))))(Secondary((id \
         5f3dd6da-a6ae-4699-a393-78ce38f83876)(content(Comment\"#   test \
         format_plant(\\\"Cactus\\\", 50)                           \
         #\"))))(Secondary((id \
         1420bb62-967f-40c6-b409-3b38654188f5)(content(Whitespace\"\\n\"))))(Secondary((id \
         9845b1f6-2e7e-4467-9878-97d6faea2d45)(content(Comment\"#     == \
         \\\"\\240\\159\\146\\167 Cactus: 50ml\\\" \
         end;                               #\"))))(Secondary((id \
         d1442d95-f819-4cb5-9e31-ee012ef9b16d)(content(Whitespace\"\\n\"))))(Secondary((id \
         03ed7f0b-6878-444d-b7f1-75a3fbbe67b5)(content(Comment\"#   test \
         format_plant(\\\"Lily\\\", 200)                            \
         #\"))))(Secondary((id \
         e212ae32-44a0-42dd-bcef-57b0f331fa41)(content(Whitespace\"\\n\"))))(Secondary((id \
         7d0be205-6817-42f3-86c3-62a50fd7be8b)(content(Comment\"#     == \
         \\\"\\240\\159\\146\\167\\240\\159\\146\\167 Lily: 200ml\\\" \
         end                               #\"))))(Secondary((id \
         2486675e-fe07-4e64-97d6-33fd81ba5c13)(content(Whitespace\"\\n\"))))(Secondary((id \
         3414f201-9bb4-4414-9c24-1448fe759d30)(content(Comment\"#                                                             \
         #\"))))(Secondary((id \
         6b502a53-23ce-4996-9bf3-897cf1b71bf3)(content(Whitespace\"\\n\"))))(Secondary((id \
         7ee1c8ad-6301-45aa-a9d2-ce522e5d6649)(content(Comment\"# The first \
         two tests pass. The Lily test FAILS!              \
         #\"))))(Secondary((id \
         2202ac59-8541-447c-a52b-f1f0f55fba72)(content(Whitespace\"\\n\"))))(Secondary((id \
         0d5321d0-1b52-47ef-a746-2368332a2bf4)(content(Comment\"# Click inside \
         water_tag and look at the probe:               #\"))))(Secondary((id \
         0f060590-72df-4e24-9143-151334b5db15)(content(Whitespace\"\\n\"))))(Secondary((id \
         1aec95f4-c7c0-4627-a242-4b4358f7c9a4)(content(Comment\"# \
         water_tag(200) returns \\\"\\240\\159\\146\\167\\\" not \
         \\\"\\240\\159\\146\\167\\240\\159\\146\\167\\\".                    \
         #\"))))(Secondary((id \
         200878b3-bef7-4908-a83c-458e97564e1b)(content(Whitespace\"\\n\"))))(Secondary((id \
         8722407f-622a-4375-9634-c62a35ad6ad8)(content(Comment\"# The > should \
         be >=. Don't fix it \\226\\128\\148 next stage improves      \
         #\"))))(Secondary((id \
         2dbd2609-266a-4731-b38e-ec5ae69059b9)(content(Whitespace\"\\n\"))))(Secondary((id \
         7e9b4f9d-0b74-4a2f-8797-56f754c8b391)(content(Comment\"# the whole \
         approach.                                         \
         #\"))))(Secondary((id \
         321f1240-5834-49a3-a77a-ec81e0712edb)(content(Whitespace\"\\n\"))))(Secondary((id \
         a6b1b533-421b-416e-8ccc-c04463d381b9)(content(Whitespace\"\\n\"))))(Tile((id \
         c08b5f16-8c7c-4952-8654-860fc23b7679)(label(?))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         c2d906bd-77f6-42c0-b3f5-88b5563b2953)(content(Whitespace\"\\n\"))))(Secondary((id \
         02b7dff7-70be-43f5-a82e-fab46a399496)(content(Whitespace\"\\n\"))))(Tile((id \
         7a0e75dc-ea6c-453d-a425-7748cb75a9c6)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         dae892cd-446c-4e30-92b6-26efa6cc5764)(content(Whitespace\"\\n\"))))(Secondary((id \
         da640356-913d-4dbd-8a60-4f8f33934050)(content(Whitespace\"\\n\"))))(Secondary((id \
         5106cdcd-4367-4621-8e3c-4b3834706d70)(content(Comment\"# \
         ============================================================ \
         #\"))))(Secondary((id \
         f88d1c3b-e73a-40ac-b0c9-095be7b930e4)(content(Whitespace\"\\n\"))))(Secondary((id \
         7789170e-b2d2-4ef2-8d27-a019687c58d9)(content(Comment\"# STAGE 4: A \
         TYPE AND CASE EXPRESSION                        #\"))))(Secondary((id \
         f37921c6-a0c3-4b43-b81e-a161f7129c8f)(content(Whitespace\"\\n\"))))(Secondary((id \
         46d200d3-3375-4fd4-a406-78f758f9f3c0)(content(Comment\"# \
         ============================================================ \
         #\"))))(Secondary((id \
         33045758-9fd8-4473-ae47-36407809c388)(content(Whitespace\"\\n\"))))(Secondary((id \
         89bb267a-1d15-443d-af66-a727cfd73d6b)(content(Whitespace\"\\n\"))))(Secondary((id \
         280ec5d9-7dda-4e22-a8c8-4899acce1fbe)(content(Comment\"# Two levels \
         isn't enough. Let's use three: Low, Medium,     #\"))))(Secondary((id \
         0f957558-1f8d-423b-b050-e76a7fafa78e)(content(Whitespace\"\\n\"))))(Secondary((id \
         d8265f4e-752d-4536-8600-6f854c120e60)(content(Comment\"# High. We \
         define a sum type and match with a case.          \
         #\"))))(Secondary((id \
         00e0eb96-c2a9-4b33-acc4-40571617fec2)(content(Whitespace\"\\n\"))))(Secondary((id \
         70bf4e98-9e56-4fe3-a14c-4bb059ed9bb4)(content(Comment\"#                                                             \
         #\"))))(Secondary((id \
         1e498ab7-7017-4ca5-a093-d9735b11d543)(content(Whitespace\"\\n\"))))(Secondary((id \
         fa3a17b8-6e71-46e9-a9da-3e4533a70ff1)(content(Comment\"# `type Name = \
         + V1 + V2 + V3 in` defines a sum type.        #\"))))(Secondary((id \
         4c5cc7e4-8396-4d1f-9476-8dceff43dc19)(content(Whitespace\"\\n\"))))(Secondary((id \
         f1bda93e-6119-440c-9f6b-5bad0cfab405)(content(Comment\"# `case expr | \
         V1 => e1 | V2 => e2 | ... end` matches.       #\"))))(Secondary((id \
         229928af-8177-45b3-b084-1bcb8c6295be)(content(Whitespace\"\\n\"))))(Secondary((id \
         97a5fd0d-8494-4ed6-b52f-2e4b6326b067)(content(Comment\"#                                                             \
         #\"))))(Secondary((id \
         b8408f01-abf0-491d-b3ec-8f9edcf84951)(content(Whitespace\"\\n\"))))(Secondary((id \
         ec22aed6-e793-40d1-a044-1c6664c40ec1)(content(Comment\"# Replace the \
         ? below with:                                   #\"))))(Secondary((id \
         89c1c0e0-f787-4f6f-86ca-b271405ae9bf)(content(Whitespace\"\\n\"))))(Secondary((id \
         f9b28015-a204-4760-915d-146685644265)(content(Comment\"#                                                             \
         #\"))))(Secondary((id \
         26ce70d5-2517-4a58-a336-0ab7b6e5128a)(content(Whitespace\"\\n\"))))(Secondary((id \
         cb6db198-7703-4735-8be4-eb25f2e9eb18)(content(Comment\"#   type \
         WaterLevel = + Low + Medium + High in                \
         #\"))))(Secondary((id \
         6394d269-6876-4de4-9b1e-63759a5efb3f)(content(Whitespace\"\\n\"))))(Secondary((id \
         8f5584d2-3ff2-4340-863f-c08597d4651f)(content(Comment\"#                                                             \
         #\"))))(Secondary((id \
         65cb3c47-a5fa-455a-ae11-fc28179f9288)(content(Whitespace\"\\n\"))))(Secondary((id \
         650d248b-c788-4ded-a16f-295c3e232c98)(content(Comment\"#   let \
         classify: Int -> WaterLevel =                         \
         #\"))))(Secondary((id \
         dd6b282b-9e58-4f31-91c1-6e3f93862ab8)(content(Whitespace\"\\n\"))))(Secondary((id \
         f1b9b2e6-bd40-4899-85d9-bc1788358ad6)(content(Comment\"#     fun \
         amount ->                                           \
         #\"))))(Secondary((id \
         a7ecce37-5df0-4ad5-877b-15876468c339)(content(Whitespace\"\\n\"))))(Secondary((id \
         d9f9c8dc-a14f-44b6-8f5f-73d0973c62e1)(content(Comment\"#     if \
         amount >= 200 then High                              \
         #\"))))(Secondary((id \
         94df8328-a12c-4c83-b94f-02f819193e24)(content(Whitespace\"\\n\"))))(Secondary((id \
         57dbc2a3-259b-4268-a49b-430ce00c1545)(content(Comment\"#     else if \
         amount >= 100 then Medium                       #\"))))(Secondary((id \
         5006e415-9ad6-4225-8149-1c93361afb47)(content(Whitespace\"\\n\"))))(Secondary((id \
         16b041d3-7a3b-44a7-bc61-8ba647371302)(content(Comment\"#     else \
         Low                                                \
         #\"))))(Secondary((id \
         e7e3677d-bc14-4076-9a72-ff97fdb29539)(content(Whitespace\"\\n\"))))(Secondary((id \
         6b912aca-d163-4e36-a78f-e0edb797cfa4)(content(Comment\"#   \
         in                                                        \
         #\"))))(Secondary((id \
         cb264bba-400d-4d69-9efa-08114a7f5c6d)(content(Whitespace\"\\n\"))))(Secondary((id \
         eb40ba99-34a6-42c9-b32c-f7e2e968bbd4)(content(Comment\"#                                                             \
         #\"))))(Secondary((id \
         e8885dcc-990f-4134-aa50-765281da7828)(content(Whitespace\"\\n\"))))(Secondary((id \
         4cef7eb4-f750-48e4-8b5e-b4944e956911)(content(Comment\"#   let \
         format_plant: (String, Int) -> String =               \
         #\"))))(Secondary((id \
         6a56b4bd-a76e-49f6-b267-10babe2dcdee)(content(Whitespace\"\\n\"))))(Secondary((id \
         afb3419f-edd9-4edf-a253-b97168c089cc)(content(Comment\"#     fun \
         (name, water) ->                                    \
         #\"))))(Secondary((id \
         3b462b10-fa96-49c5-9602-3648c8367a34)(content(Whitespace\"\\n\"))))(Secondary((id \
         58f25c37-1efa-4762-bdd0-183f66acebb0)(content(Comment\"#     let \
         emoji = case classify(water)                        \
         #\"))))(Secondary((id \
         3c1d7283-9fc0-4848-85c6-c40faa930e7f)(content(Whitespace\"\\n\"))))(Secondary((id \
         f8e3165f-a982-438a-bcce-a1d56e539754)(content(Comment\"#       | Low \
         => \
         \\\"\\240\\159\\140\\181\\\"                                         \
         #\"))))(Secondary((id \
         83b8b95e-3b30-48ea-aec3-114ebee3554a)(content(Whitespace\"\\n\"))))(Secondary((id \
         d969c917-d595-465f-97b4-a562489bef7e)(content(Comment\"#       | \
         Medium => \
         \\\"\\240\\159\\146\\167\\\"                                      \
         #\"))))(Secondary((id \
         6d93fa87-8301-4222-9716-0415b15a256f)(content(Whitespace\"\\n\"))))(Secondary((id \
         6e53e87e-1ffe-4767-a9ec-6d406fc15327)(content(Comment\"#       | High \
         => \
         \\\"\\240\\159\\146\\167\\240\\159\\146\\167\\\"                                      \
         #\"))))(Secondary((id \
         c1629de3-66f1-493e-adce-43f7b6242ea5)(content(Whitespace\"\\n\"))))(Secondary((id \
         62941eb8-b952-4ab0-9601-f8ff9dfef6c0)(content(Comment\"#     end \
         in                                                  \
         #\"))))(Secondary((id \
         74ca1146-d32f-4554-81b4-90e9dc8b11a8)(content(Whitespace\"\\n\"))))(Secondary((id \
         d68350d9-628b-440d-9a79-5525f6a94f5f)(content(Comment\"#     emoji ++ \
         \\\" \\\" ++ name ++ \\\": \\\"                            \
         #\"))))(Secondary((id \
         a2c927cc-be6b-42d0-9b1f-26c2e74e7eb6)(content(Whitespace\"\\n\"))))(Secondary((id \
         5fab77f9-d900-4fbd-a902-148eaa8f1b7a)(content(Comment\"#       ++ \
         string_of_int(water) ++ \\\"ml\\\"                       \
         #\"))))(Secondary((id \
         5049613c-cebe-4a77-bb4b-21892d71dff7)(content(Whitespace\"\\n\"))))(Secondary((id \
         6aed1b51-78c4-4f58-9f5e-37d25944654d)(content(Comment\"#   \
         in                                                        \
         #\"))))(Secondary((id \
         474d85ad-e9a4-49d1-a069-95d4d748e6e5)(content(Whitespace\"\\n\"))))(Secondary((id \
         e294fb5e-90d2-4206-9e4c-09bb4bb6c106)(content(Comment\"#                                                             \
         #\"))))(Secondary((id \
         bb76cecd-b9ad-4eb0-a445-c5a44d31423d)(content(Whitespace\"\\n\"))))(Secondary((id \
         7374934a-4fc7-4d4e-8512-58901043e175)(content(Comment\"#   test \
         format_plant(\\\"Fern\\\", 250)                            \
         #\"))))(Secondary((id \
         8020ef44-7f04-4a50-af9e-99afe523b1c4)(content(Whitespace\"\\n\"))))(Secondary((id \
         baa31241-db38-4f62-b4c1-5e7ae04b9728)(content(Comment\"#     == \
         \\\"\\240\\159\\146\\167\\240\\159\\146\\167 Fern: 250ml\\\" \
         end;                              #\"))))(Secondary((id \
         f3c3f4ae-004c-4920-89fe-5865ac340904)(content(Whitespace\"\\n\"))))(Secondary((id \
         3cf29353-4fe8-401e-9e11-1f64ccfb0589)(content(Comment\"#   test \
         format_plant(\\\"Orchid\\\", 180)                          \
         #\"))))(Secondary((id \
         83ade801-aa03-4157-a399-be750bd5ec7d)(content(Whitespace\"\\n\"))))(Secondary((id \
         8fc33002-1857-48d4-8ab7-f375f7170a23)(content(Comment\"#     == \
         \\\"\\240\\159\\146\\167 Orchid: 180ml\\\" \
         end;                              #\"))))(Secondary((id \
         b56fd7da-c351-4856-8340-a80c5371c5ed)(content(Whitespace\"\\n\"))))(Secondary((id \
         a69d940c-1b20-4a4f-a917-9a44cf277d09)(content(Comment\"#   test \
         format_plant(\\\"Cactus\\\", 50)                           \
         #\"))))(Secondary((id \
         e0a9afea-bdf3-43c5-97a2-4796314058a5)(content(Whitespace\"\\n\"))))(Secondary((id \
         c88fcb7c-d59c-431b-aae4-6768048d47b2)(content(Comment\"#     == \
         \\\"\\240\\159\\140\\181 Cactus: 50ml\\\" \
         end;                               #\"))))(Secondary((id \
         a38760e8-5bd8-46eb-91af-071edad405c8)(content(Whitespace\"\\n\"))))(Secondary((id \
         c8b69546-ec75-4564-9026-51f81101e970)(content(Comment\"#   test \
         format_plant(\\\"Lily\\\", 200)                            \
         #\"))))(Secondary((id \
         3caeeed6-7a08-4815-867f-e3ef6bb00391)(content(Whitespace\"\\n\"))))(Secondary((id \
         d79211f4-39ef-4d9a-a8f1-8c4a1f00d734)(content(Comment\"#     == \
         \\\"\\240\\159\\146\\167\\240\\159\\146\\167 Lily: 200ml\\\" \
         end                               #\"))))(Secondary((id \
         8fdc07d7-a2f2-4be8-90b8-fcf18f4a4a42)(content(Whitespace\"\\n\"))))(Secondary((id \
         f653e25e-154b-47c1-8d76-1eb7f07c469a)(content(Comment\"#                                                             \
         #\"))))(Secondary((id \
         84ed9047-4021-4f06-b543-5be83a6ed05d)(content(Whitespace\"\\n\"))))(Secondary((id \
         6228b2a2-8993-408b-b576-f1b3ac3b76ad)(content(Comment\"# All four \
         pass \\226\\128\\148 including Lily! Click inside classify       \
         #\"))))(Secondary((id \
         d1121caf-14cc-42af-ae2b-3ed7bbdae464)(content(Whitespace\"\\n\"))))(Secondary((id \
         d6c80a0d-e77e-41e2-962c-e80f737b3b89)(content(Comment\"# in Many mode \
         to see: High, Medium, Low, High.              #\"))))(Secondary((id \
         6cda2499-be36-4726-9137-5cef2d606ba5)(content(Whitespace\"\\n\"))))(Secondary((id \
         cfb7b866-b8a9-40a7-abac-c193faeef115)(content(Comment\"# The >= 200 \
         threshold now handles the boundary correctly.    \
         #\"))))(Secondary((id \
         c9cfa41f-d3bf-436a-94fc-d03404d3e001)(content(Whitespace\"\\n\"))))(Secondary((id \
         13631383-b0d9-48e8-9bba-a974b1e6b9cd)(content(Whitespace\"\\n\"))))(Tile((id \
         730e8eeb-c03c-443e-97c3-341aa12bc9c8)(label(?))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         aa33175d-5e27-4595-9070-d5928d7981d9)(content(Whitespace\"\\n\"))))(Secondary((id \
         d134cebb-1f73-42ec-8ad8-1a80c9d8eb0e)(content(Whitespace\"\\n\"))))(Secondary((id \
         041b9f64-0a1a-4fc5-9b7b-9035b91bc6a7)(content(Comment\"# \
         ============================================================ \
         #\"))))(Secondary((id \
         23025725-a3fc-49d4-ae38-5dd1e114752a)(content(Whitespace\"\\n\"))))(Secondary((id \
         33ce91cb-b832-478b-aed5-daa1e7870418)(content(Comment\"# \
         REVIEW                                                      \
         #\"))))(Secondary((id \
         e510ea53-73bb-4ca2-83b9-b783ae331264)(content(Whitespace\"\\n\"))))(Secondary((id \
         d88a44ad-e429-4810-8c2c-8afae171bbe4)(content(Comment\"#                                                             \
         #\"))))(Secondary((id \
         2a763c15-fba2-44af-8cb7-4e555777ec4c)(content(Whitespace\"\\n\"))))(Secondary((id \
         2da7da50-b933-4e9d-b4a9-7fc0569b3ce4)(content(Comment\"# You've \
         written:                                             \
         #\"))))(Secondary((id \
         381ac14c-0e09-4c54-9e5b-5542600c8bff)(content(Whitespace\"\\n\"))))(Secondary((id \
         3109cdea-c10d-42f3-9c7d-c6e4a0172c45)(content(Comment\"#   1. Let \
         bindings and string expressions                    \
         #\"))))(Secondary((id \
         48a31277-7830-4b2c-b3a3-07026b79082f)(content(Whitespace\"\\n\"))))(Secondary((id \
         817222d9-cfa7-49d3-938d-d12ded9c4494)(content(Comment\"#   2. A \
         function with tests                                  \
         #\"))))(Secondary((id \
         807c46c0-a96d-497c-9cec-ea337a6860e9)(content(Whitespace\"\\n\"))))(Secondary((id \
         300a2621-9c25-4e6b-a6f1-08a1bbe5e3d7)(content(Comment\"#   3. A \
         helper with if/else (and caught a boundary bug)      \
         #\"))))(Secondary((id \
         b5e70d40-bef0-4979-a2c0-cb9792c833b7)(content(Whitespace\"\\n\"))))(Secondary((id \
         bc386dc9-4833-4635-a001-8ab2bd89750b)(content(Comment\"#   4. A type \
         definition with case expression                 #\"))))(Secondary((id \
         2a84f7ce-f7ac-4d7d-a835-acb4685e07f1)(content(Whitespace\"\\n\"))))(Secondary((id \
         55b36243-0673-4906-9b39-d958232b5381)(content(Comment\"#                                                             \
         #\"))))(Secondary((id \
         39ba4d3d-109f-496a-8a19-2594267bd931)(content(Whitespace\"\\n\"))))(Secondary((id \
         d6909dfe-5973-4079-9205-03055fc18c47)(content(Comment\"# At each \
         step, probes showed intermediate values inline,     \
         #\"))))(Secondary((id \
         bdc8204e-d4be-49b4-8d20-d8568a1eeb4c)(content(Whitespace\"\\n\"))))(Secondary((id \
         7ed938ff-6150-4c16-9a2f-5bcb0b0223bd)(content(Comment\"# so you could \
         verify correctness as you went.                #\"))))(Secondary((id \
         afd936bf-e40f-4d66-8f03-8e4cea5cb2db)(content(Whitespace\"\\n\"))))(Secondary((id \
         0de802c0-adbc-4580-a6ee-c585aa97b574)(content(Comment\"# \
         ============================================================ \
         #\"))))(Secondary((id \
         ef727281-83ae-4f7c-8509-345b94881ee9)(content(Whitespace\"\\n\"))))(Secondary((id \
         6b239a35-fd2b-40f0-9ead-127977bf42b7)(content(Whitespace\"\\n\"))))(Secondary((id \
         1c7150b0-c205-4884-a0fc-a8b284660479)(content(Comment\"# END \
         #\"))))(Secondary((id \
         b44eeedb-7c76-40c8-86ae-f7ff55d4ff2d)(content(Whitespace\"\\n\")))))";
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
