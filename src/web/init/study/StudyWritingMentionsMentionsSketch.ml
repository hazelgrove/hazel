let out : string * Haz3lcore.PersistentSegment.t =
  ( "Study / writing / mentions / mentions-sketch",
    {
      segment =
        "((Secondary((id \
         62ff242e-505c-4cd4-b66d-28cb206b9436)(content(Comment\"# MENTION \
         EXTRACTOR TASK                        #\"))))(Secondary((id \
         68791453-797e-41ee-91c0-fd7b0c52b03b)(content(Whitespace\"\\n\"))))(Secondary((id \
         1fccae38-cd67-4797-9827-bc3b71c5bc0b)(content(Comment\"#                                               \
         #\"))))(Secondary((id \
         afe8517b-5b56-4f46-af56-4f1a0d0bd546)(content(Whitespace\"\\n\"))))(Secondary((id \
         ea40a458-0ab8-4607-a222-5a9035fd8094)(content(Comment\"# Extract \
         @mentions from a message string.      #\"))))(Secondary((id \
         231d1674-312f-4d33-88f9-771bed7549f8)(content(Whitespace\"\\n\"))))(Secondary((id \
         8280d355-23a5-4664-a5f8-36e6d656ed10)(content(Comment\"# Given \
         \\\"Hey @alice and @bob\\\", return           #\"))))(Secondary((id \
         e85073b4-5740-4a06-8a83-5e291eab2ffd)(content(Whitespace\"\\n\"))))(Secondary((id \
         12c77a98-f9a7-4928-b8c3-a10b481f6a3f)(content(Comment\"# \
         [\\\"alice\\\", \\\"bob\\\"].                             \
         #\"))))(Secondary((id \
         ef770c5c-1020-4921-af26-c1c7e8e7455e)(content(Whitespace\"\\n\"))))(Secondary((id \
         831aa9df-32c0-423b-aa78-bfdcb7296e24)(content(Comment\"#                                               \
         #\"))))(Secondary((id \
         2222232c-2123-433b-a57c-3750bce17dac)(content(Whitespace\"\\n\"))))(Secondary((id \
         90bd8ee3-5c60-4ca2-9cc6-7f292f8efb66)(content(Comment\"# \
         Steps:                                        #\"))))(Secondary((id \
         1f4c5afd-ead2-4bf7-aacf-d443261b56cf)(content(Whitespace\"\\n\"))))(Secondary((id \
         7b3ccc5c-c09d-434b-a62d-54b239538272)(content(Comment\"#   1. Split \
         message into words                 #\"))))(Secondary((id \
         ccf0352c-c7df-4047-b0bf-b3308c27e029)(content(Whitespace\"\\n\"))))(Secondary((id \
         bf876849-0854-4e3b-b03f-46e288e8e9e6)(content(Comment\"#   2. Keep \
         only words starting with @          #\"))))(Secondary((id \
         8df36db3-ae83-4633-8360-e115c194fa94)(content(Whitespace\"\\n\"))))(Secondary((id \
         dc613b99-6f0b-4db2-b89a-2fc86adfa74c)(content(Comment\"#   3. Remove \
         the @ from each                   #\"))))(Secondary((id \
         8e3b040d-0024-477c-abde-26380d55749e)(content(Whitespace\"\\n\"))))(Secondary((id \
         f28336c2-2886-4884-b609-0322b3aabe0e)(content(Comment\"#                                               \
         #\"))))(Secondary((id \
         beceac77-a8d8-4920-acec-bab0fb003cc5)(content(Whitespace\"\\n\"))))(Secondary((id \
         88f00e47-bbf7-4b87-adbf-b638b1bdacfb)(content(Comment\"# Available \
         functions:                          #\"))))(Secondary((id \
         4de91601-de4a-4ebe-b036-b1911a94db96)(content(Whitespace\"\\n\"))))(Secondary((id \
         0fbbf7ed-548b-4e51-861d-2f8c3f8a9570)(content(Comment\"#   \
         string_split(sep, str) -> [String]          #\"))))(Secondary((id \
         70babe86-c9bc-4750-8ad8-49d91a44d43a)(content(Whitespace\"\\n\"))))(Secondary((id \
         ce24ffb7-d484-4864-a2ba-110b70f59084)(content(Comment\"#   \
         string_sub(str, start, length) -> String    #\"))))(Secondary((id \
         45c7ca40-eb94-4698-a0dd-6ca9ef11594c)(content(Whitespace\"\\n\"))))(Secondary((id \
         55a19345-66ab-486b-8550-89f852764124)(content(Comment\"#   \
         string_length(str) -> Int                   #\"))))(Secondary((id \
         68bafe77-2647-4a77-a8eb-f57cc9de76f1)(content(Whitespace\"\\n\"))))(Secondary((id \
         afa85db3-299d-4eac-88dd-eae74960716c)(content(Comment\"#   \
         filter(list, predicate) -> list             #\"))))(Secondary((id \
         03869dd2-91fd-48ac-af94-92f3d03d5ae7)(content(Whitespace\"\\n\"))))(Secondary((id \
         d07666d5-7ab6-4930-bb90-0983308f7eb7)(content(Comment\"#   map(list, \
         fn) -> list                       #\"))))(Secondary((id \
         6a2e2f5c-a4f3-4ac9-92d0-dab8584d4180)(content(Whitespace\"\\n\"))))(Secondary((id \
         5c6b4598-d713-466c-be9d-b9b323723ca1)(content(Comment\"#                                               \
         #\"))))(Secondary((id \
         8f3a2971-2af1-424b-bf9f-0750a720fa76)(content(Whitespace\"\\n\"))))(Secondary((id \
         b26ed1cf-1126-4d8d-a3c7-1a2192b4bbc8)(content(Comment\"# Syntax \
         reminder:                              #\"))))(Secondary((id \
         4f2cde58-4648-4d39-ae52-120147e0c2cd)(content(Whitespace\"\\n\"))))(Secondary((id \
         f9357c6e-27d2-48e5-890b-47e38301713d)(content(Comment\"#   let name = \
         expr in body                     #\"))))(Secondary((id \
         833db3b3-eee5-4a84-86f6-9123a8574b2a)(content(Whitespace\"\\n\"))))(Secondary((id \
         ff9f7a34-29a2-4e52-92e3-8a6146327b3e)(content(Comment\"#   fun x -> \
         body                               #\"))))(Secondary((id \
         c442be4a-a972-40c3-9c41-221ca9cb5bed)(content(Whitespace\"\\n\"))))(Secondary((id \
         db47d187-0a59-45c1-bdce-877ff900343d)(content(Comment\"#                                               \
         #\"))))(Secondary((id \
         e3e68f79-c5d0-4325-83e6-610c8f4b9bcc)(content(Whitespace\"\\n\"))))(Secondary((id \
         6dffabe6-3d13-4f5d-a76d-9d922d2474dc)(content(Comment\"# Tip: Build \
         incrementally! Write one step,    #\"))))(Secondary((id \
         453cfe17-08c5-4bc9-ae8b-b77d7e57e0c0)(content(Whitespace\"\\n\"))))(Secondary((id \
         74b8b435-723f-42f6-a71d-cb12fb95839c)(content(Comment\"# check the \
         probe output, then add the next.   #\"))))(Secondary((id \
         097ff609-4f64-44a7-a146-10ddec11a4b4)(content(Whitespace\"\\n\"))))(Secondary((id \
         ccb41c5c-9a17-4493-b8f3-beab16c063df)(content(Whitespace\"\\n\"))))(Secondary((id \
         308b8138-2c86-4a28-81fa-edf2894787c2)(content(Comment\"# Helper: \
         check if a word starts with @ #\"))))(Secondary((id \
         07fc9604-befe-4186-9efc-d9eb0104dcb5)(content(Whitespace\"\\n\"))))(Tile((id \
         ca28f499-9715-4ca8-93f0-d66c6ce36de7)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         b26a23e9-ebf6-4cf4-8d2e-8ded23acec34)(content(Whitespace\" \
         \"))))(Tile((id \
         d042dde9-462a-4024-a8d0-3a9cf71bbd49)(label(starts_with_at))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         0c4b0559-a3e6-48b8-bb97-d63153f8e18e)(content(Whitespace\" \
         \")))))((Secondary((id \
         6a46510a-fd5c-467c-ba7b-ff00979efc40)(content(Whitespace\" \
         \"))))(Tile((id bf7a6d57-781c-4d11-b433-30e1cbbe13c4)(label(fun \
         ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 37))(sort Exp))))))(shards(0 \
         1))(children(((Secondary((id \
         4a94cc4e-25c3-494c-a30b-38bc752a79aa)(content(Whitespace\" \
         \"))))(Tile((id \
         e91b0b2b-e8cf-463a-9a01-aefafb5dc0bc)(label(word))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         b4295a69-c4eb-4584-baf2-403d60bac43f)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         9c175166-517a-4416-844f-fb182d7a4d7c)(content(Whitespace\"\\n\"))))(Tile((id \
         cff26e97-9780-403a-b3bd-5296ad73ba9a)(label(?))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         de56c89d-ee01-43c2-8158-398314b4b263)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         01c0dea9-830c-4c58-becb-ea22674e5ae6)(content(Whitespace\"\\n\"))))(Secondary((id \
         9d5858b2-0505-4df2-a9a4-8de4d70b7746)(content(Whitespace\"\\n\"))))(Secondary((id \
         09bf3dc4-199c-46d1-bb48-5998ef72076f)(content(Comment\"# Helper: \
         remove the @ prefix from a word #\"))))(Secondary((id \
         735a0a46-b4e0-4638-a833-99f42cb225ab)(content(Whitespace\"\\n\"))))(Tile((id \
         076e5e09-8a38-475d-b42e-004358635fc3)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         eb8e4046-0577-4d05-81c4-c791d13f9ba6)(content(Whitespace\" \
         \"))))(Tile((id \
         3660a65d-f7d2-47eb-bd65-f66cb7dc8a05)(label(strip_at))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         60d5b28e-fb90-49f3-817c-d6749dd128f0)(content(Whitespace\" \
         \")))))((Secondary((id \
         704dba15-fcc3-4e34-9c87-d96895890a4f)(content(Whitespace\" \
         \"))))(Tile((id 5f45f9fa-ba08-468f-bb77-fbc93a68fc12)(label(fun \
         ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 37))(sort Exp))))))(shards(0 \
         1))(children(((Secondary((id \
         96c82d7d-f3ee-45d4-9371-27fe6d88cc2e)(content(Whitespace\" \
         \"))))(Tile((id \
         fb40af1c-1dc0-4e99-a4f8-162f1164a0d2)(label(word))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         f196dc24-69e3-43f6-bb86-ce102b6d714a)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         34c90426-5494-4556-81d4-c772fce84c9b)(content(Whitespace\"\\n\"))))(Tile((id \
         e28e0750-2e4c-41ba-83d2-80124fb23654)(label(?))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         13af022a-0cd0-4c1b-b365-a10f2f73624d)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         c34f91e0-01d9-4122-a37a-1475a2c16778)(content(Whitespace\"\\n\"))))(Secondary((id \
         c00e9c09-8da7-42e3-8286-bf24d073aa20)(content(Whitespace\"\\n\"))))(Secondary((id \
         ebf51d25-fc45-4ca3-8a36-b94705a34209)(content(Comment\"# Main \
         function: extract usernames from message #\"))))(Secondary((id \
         da9efc80-d7ed-4036-8a3d-2211dfee4d7d)(content(Whitespace\"\\n\"))))(Tile((id \
         f9e62b58-8b3b-4ace-9491-cfa1927f339e)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         cd820b2d-63e8-4996-ac47-0bdcef02c8d2)(content(Whitespace\" \
         \"))))(Tile((id \
         e2742802-b444-4e76-b86b-cd92b28e7535)(label(extract_mentions))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         83810506-de13-44a2-be48-2286a6413900)(content(Whitespace\" \
         \")))))((Secondary((id \
         d46b53b7-e986-4136-8c16-e8483d077a55)(content(Whitespace\" \
         \"))))(Tile((id c65b438e-ddf1-4079-b4ff-16a5d51306a0)(label(fun \
         ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 37))(sort Exp))))))(shards(0 \
         1))(children(((Secondary((id \
         8f4e31b3-05b1-46a4-9bbc-a94eb405077b)(content(Whitespace\" \
         \"))))(Tile((id \
         2554649b-5539-4496-9240-40eb4aa840f2)(label(message))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         7b7bb06b-6826-4dfb-adc6-68b58ca893f4)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         1e750291-62b8-4c6a-a966-b5647aaad56c)(content(Whitespace\"\\n\"))))(Tile((id \
         32cc9592-6b16-4b0a-8203-29b2c8004533)(label(?))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         d48b6392-fdad-49a5-a717-278bc9f19abe)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         f1c64f2d-b09e-46f3-9a07-f75c4c750dde)(content(Whitespace\"\\n\"))))(Secondary((id \
         373b0909-fe4b-46aa-88f3-39a6c16f89b7)(content(Whitespace\"\\n\"))))(Tile((id \
         dcaa0acf-1454-4951-a407-673a79dde71d)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         4fef9c8c-a2c2-45dd-bde7-01f7b3af5054)(content(Whitespace\"\\n\"))))(Tile((id \
         4040ed12-7d00-4b88-9365-20e2671901e7)(label(extract_mentions))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         0a5b3548-cd4f-4de6-920e-465a88da1378)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         417e8cac-f1d8-4ce5-977a-f3d772de05e2)(label(\"\\\"Hey \
         @alice\\\"\"))(mold((out Exp)(in_())(nibs(((shape Convex)(sort \
         Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         d5ec7b88-14e6-47a5-91ed-bbde8ed4ddb0)(content(Whitespace\"\\n\"))))(Tile((id \
         94a44f58-322c-457d-a63e-42276a0213b9)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         420b7e7d-1024-4257-b873-6f360c7e4137)(content(Whitespace\" \
         \"))))(Tile((id d4a1f0e1-f92d-41bc-a02f-09b2ce597996)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         5c3272e1-4af7-4dfc-9881-356b51f17611)(label(\"\\\"alice\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         1dd3b3cc-6e7a-4b33-9bc2-a773a9901dbf)(content(Whitespace\"\\n\")))))))))(Tile((id \
         6c3e0bd7-41c7-44ee-8200-c5d175f94cb1)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         985fcc0f-37c5-4cf2-bee3-ff4cdeee9d98)(content(Whitespace\"\\n\"))))(Secondary((id \
         61576fd8-86c9-421e-a402-3e9d51ef2a8a)(content(Whitespace\"\\n\"))))(Tile((id \
         18567953-f3b8-4737-83ca-071b52f0a2be)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         781decab-56a8-4856-9849-cf5489d72b7f)(content(Whitespace\"\\n\"))))(Tile((id \
         7f59cbcc-0abb-4e5c-a4ec-63a858aa094a)(label(extract_mentions))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         d701a15d-a65b-4321-b420-a0007aa7a6ed)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         2177fcdd-1eb0-426a-a904-34f773ece58b)(label(\"\\\"@bob @carol \
         hello\\\"\"))(mold((out Exp)(in_())(nibs(((shape Convex)(sort \
         Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         a447f8a3-2585-4679-8b83-5bbb766c37c0)(content(Whitespace\"\\n\"))))(Tile((id \
         0eb0a36a-9e13-4bba-bbee-8f40cda675b6)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         104da5d8-8637-4be2-9245-b975c5665b1f)(content(Whitespace\" \
         \"))))(Tile((id b56b2177-f461-4685-bf94-01d090bbd0f6)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         18dd3e05-4871-4f7e-8e4e-d22939c48922)(label(\"\\\"bob\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         3ef27b55-cb5d-458e-8ab9-713150a76778)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         5a2036ec-bdd6-484d-abdb-ffec560ea4b3)(content(Whitespace\" \
         \"))))(Tile((id \
         e3d05bd5-0ef8-423b-b872-cdfc57012885)(label(\"\\\"carol\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         759457f6-92e1-48c8-80e1-cd1498416bf7)(content(Whitespace\"\\n\")))))))))(Tile((id \
         9ee3df80-31b0-4433-8d94-0ff7892c581b)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         b5892bfe-0f9f-46f6-8889-76827f390b8f)(content(Whitespace\"\\n\"))))(Secondary((id \
         d2d1a951-534e-419c-9dbf-ff939b9abca5)(content(Whitespace\"\\n\"))))(Tile((id \
         64cb1289-7e75-4ad6-8184-6bf769a296e2)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         a80f23d9-173b-4cdc-a5ce-87ae50679d2d)(content(Whitespace\"\\n\"))))(Tile((id \
         5aa8a0a5-4795-4299-8cc0-abbdc6f43e6c)(label(extract_mentions))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         ea19e07b-5713-4cce-aeb2-3e78ac5b0860)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         9ccf074a-b915-4d91-80b8-28f84d372e25)(label(\"\\\"no mentions \
         here\\\"\"))(mold((out Exp)(in_())(nibs(((shape Convex)(sort \
         Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         d870ba76-4498-4174-a0f2-eba0abe38bb4)(content(Whitespace\"\\n\"))))(Tile((id \
         8a16aeec-a20e-4ed9-9fa3-6ca362484adb)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         3e909c86-8b8e-42ef-98f9-7ff5ab6dcf0a)(content(Whitespace\" \
         \"))))(Tile((id \
         2069bf61-36fe-49ce-9aae-2d9293b2471a)(label([]))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         f5d165b4-6fa1-4d72-8c10-ed54d2e15c7b)(content(Whitespace\"\\n\")))))))))(Tile((id \
         3b2be1fe-13f4-4372-8e48-a34c4b0e9519)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         24b723b2-3103-4533-b780-1ba5b0857bf8)(content(Whitespace\"\\n\"))))(Secondary((id \
         8c7d322c-44a4-48eb-8181-3791f39a209f)(content(Whitespace\"\\n\"))))(Tile((id \
         f60e6d02-e883-4ddc-9904-94f382fa161d)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         5c9ae370-e186-4465-8dff-7ab7f27774d4)(content(Whitespace\"\\n\"))))(Tile((id \
         d9242070-8b64-4f2d-a531-a464ede48ce6)(label(extract_mentions))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         9e727c1c-2918-41ff-8da0-0117daec4847)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         33484cfd-89d5-46df-a53a-9e872ad3d870)(label(\"\\\"@dan\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         76f27f0f-0776-4ecc-8f28-1a6ac3f9c9c6)(content(Whitespace\"\\n\"))))(Tile((id \
         004046e3-d7df-4bc2-9216-cd80ec0faee5)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         2d1877ea-e699-4a21-8622-86bb98ee761b)(content(Whitespace\" \
         \"))))(Tile((id ad649e07-7166-451b-8dbf-df314e45630a)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         b5e4a881-ded8-443a-9ba1-7ec5491d6ad3)(label(\"\\\"dan\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         0905ccb6-2ccb-4e8b-95ad-004d097ce32d)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         fad60d2e-0cd7-4a17-9e44-6e3ab33e863a)(content(Whitespace\"\\n\")))))";
      backup_text =
        "# MENTION EXTRACTOR TASK                        #\n\
         #                                               #\n\
         # Extract @mentions from a message string.      #\n\
         # Given \"Hey @alice and @bob\", return           #\n\
         # [\"alice\", \"bob\"].                             #\n\
         #                                               #\n\
         # Steps:                                        #\n\
         #   1. Split message into words                 #\n\
         #   2. Keep only words starting with @          #\n\
         #   3. Remove the @ from each                   #\n\
         #                                               #\n\
         # Available functions:                          #\n\
         #   string_split(sep, str) -> [String]          #\n\
         #   string_sub(str, start, length) -> String    #\n\
         #   string_length(str) -> Int                   #\n\
         #   filter(list, predicate) -> list             #\n\
         #   map(list, fn) -> list                       #\n\
         #                                               #\n\
         # Syntax reminder:                              #\n\
         #   let name = expr in body                     #\n\
         #   fun x -> body                               #\n\
         #                                               #\n\
         # Tip: Build incrementally! Write one step,    #\n\
         # check the probe output, then add the next.   #\n\n\
         # Helper: check if a word starts with @ #\n\
         let starts_with_at = fun word ->\n\
         ?\n\
         in\n\n\
         # Helper: remove the @ prefix from a word #\n\
         let strip_at = fun word ->\n\
         ?\n\
         in\n\n\
         # Main function: extract usernames from message #\n\
         let extract_mentions = fun message ->\n\
         ?\n\
         in\n\n\
         test\n\
         extract_mentions(\"Hey @alice\")\n\
         == [\"alice\"]\n\
         end;\n\n\
         test\n\
         extract_mentions(\"@bob @carol hello\")\n\
         == [\"bob\", \"carol\"]\n\
         end;\n\n\
         test\n\
         extract_mentions(\"no mentions here\")\n\
         == []\n\
         end;\n\n\
         test\n\
         extract_mentions(\"@dan\")\n\
         == [\"dan\"]\n\
         end\n";
      refractors = "()";
    } )
